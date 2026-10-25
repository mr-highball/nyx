{ nyx
  Copyright (c) 2020 mr-highball

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
}
unit nyx.collections.inspection;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.collections;

type
  { An unavailable capability or refused transition leaves the displayed frame
    intact. This exception never grants permission to stop an application's feed. }
  ENyxCollectionInspection = class(ENyxCollection);

  { Following admits ordinary source publications. Paused retains the displayed
    immutable source while the authoritative store and peer views keep advancing.
    Following describes update delivery, not an implicit scroll-to-last command. }
  TNyxCollectionInspectionMode = (ncimFollowing, ncimPaused);

  { Viewport intent is separate from receiving source updates. LastVisible means
    the final row after this view's filters, sort and page, never an assumed
    timestamp. KeepPosition is the default; neither choice selects a record. }
  TNyxCollectionFollowMode = (ncfKeepPosition, ncfLastVisible);

  { Identifies a committed runtime observation independently of data-change logs.
    Pending source changes do not masquerade as a new displayed dataset. }
  TNyxCollectionInspectionCause = (ncicPolicyChanged, ncicPaused, ncicResumed,
    ncicSourceChanged, ncicViewChanged, ncicFollowChanged);

  { Runtime retention intent owns only scalars. Pause retains one existing
    immutable displayed source, not a queue of source snapshots or change logs.
    MaximumPausedBytes counts its logical DataBytes, excluding query/index and
    widget overhead. IdentityLimit bounds removed-ID tracking for safe resume;
    zero is allowed and requests explicit context reset after the first removal.
    DefaultFollow supplies initial viewport intent; runtime follow commands do
    not rewrite it. Default records are absent, preserving ordinary behavior. }
  TNyxCollectionInspectionPolicy = record
  private
    FDefined: Boolean;
    FMaximumPausedBytes: Integer;
    FIdentityLimit: Integer;
    FDefaultFollow: TNyxCollectionFollowMode;
  public
    function Follow(AValue: TNyxCollectionFollowMode): TNyxCollectionInspectionPolicy;
    function PausedBytes(AValue: Integer): TNyxCollectionInspectionPolicy;
    function RemovedIdentities(AValue: Integer): TNyxCollectionInspectionPolicy;
    function Copy: TNyxCollectionInspectionPolicy;
    procedure Validate;
    property Defined: Boolean read FDefined;
    property MaximumPausedBytes: Integer read FMaximumPausedBytes;
    property IdentityLimit: Integer read FIdentityLimit;
    property DefaultFollow: TNyxCollectionFollowMode read FDefaultFollow;
  end;

  { Copied, bounded UI status. Editing this value cannot command a live view.
    No snapshot, callback, view/store or subscription is retained. Pending counts
    saturate explicitly; removed-ID overflow requires conservative context reset
    on resume rather than restoring an old identity into a new incarnation.
    FollowRequestSerial distinguishes an explicit request even at the same source
    revision; FollowMode is runtime viewport intent, independent of Mode. }
  TNyxCollectionInspectionObservation = record
    Supported: Boolean;
    Configured: Boolean;
    Mode: TNyxCollectionInspectionMode;
    DisplayRevision: Integer;
    IncomingRevision: Integer;
    PendingPublications: Integer;
    PendingCountSaturated: Boolean;
    RetainedDataBytes: Integer;
    RetainedIdentities: Integer;
    IdentityTrackingOverflow: Boolean;
    MissingSelectedRows: Integer;
    FocusChangedOnResume: Boolean;
    ContextResetOnResume: Boolean;
    FollowMode: TNyxCollectionFollowMode;
    FollowRequestSerial: Integer;
  end;

  { A synchronous UI-owner notification carries detached status only. The
    receiver owns/disconnects its subscription before destruction. Exceptions
    cannot roll back a committed transition or suppress independent observers. }
  TNyxCollectionInspectionObserver = procedure(
    const AObservation: TNyxCollectionInspectionObservation;
    ACause: TNyxCollectionInspectionCause) of object;

{ Explicit opt-in defaults: at most 32MiB logical displayed source payload and
  4096 removed identities. Callers may tune either bound, including zero. }
function NyxInspectionPolicy: TNyxCollectionInspectionPolicy;

implementation

function NyxInspectionPolicy: TNyxCollectionInspectionPolicy;
begin
  Result := Default(TNyxCollectionInspectionPolicy);
  Result.FDefined := True;
  Result.FMaximumPausedBytes := 32 * 1024 * 1024;
  Result.FIdentityLimit := 4096;
end;

procedure TNyxCollectionInspectionPolicy.Validate;
begin

  if not FDefined or (FMaximumPausedBytes < 0) or (FIdentityLimit < 0) or
    (Ord(FDefaultFollow) < Ord(Low(TNyxCollectionFollowMode))) or
    (Ord(FDefaultFollow) > Ord(High(TNyxCollectionFollowMode))) then
  begin
    raise ENyxCollectionInspection.Create('Create a nonnegative inspection retention policy');
  end;
end;

function TNyxCollectionInspectionPolicy.Follow(AValue: TNyxCollectionFollowMode):
  TNyxCollectionInspectionPolicy;
begin
  Validate;
  Result := Self;
  Result.FDefaultFollow := AValue;
  Result.Validate;
end;

function TNyxCollectionInspectionPolicy.Copy: TNyxCollectionInspectionPolicy;
begin
  Result := Self;
end;

function TNyxCollectionInspectionPolicy.PausedBytes(AValue: Integer):
  TNyxCollectionInspectionPolicy;
begin
  Validate;
  Result := Self;
  Result.FMaximumPausedBytes := AValue;
  Result.Validate;
end;

function TNyxCollectionInspectionPolicy.RemovedIdentities(AValue: Integer):
  TNyxCollectionInspectionPolicy;
begin
  Validate;
  Result := Self;
  Result.FIdentityLimit := AValue;
  Result.Validate;
end;

end.
