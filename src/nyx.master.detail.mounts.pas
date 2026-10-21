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

unit nyx.master.detail.mounts;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.model, nyx.master.detail, nyx.collections.bindings, nyx.events,
  nyx.behavior, nyx.scheduler;

type
  { Parallel managed vectors avoid interfaces inside value records on pas2js.
    They retain prepared/lifetime owners, never a platform widget or renderer. }
  TNyxMasterDetailMounts = array of INyxPreparedMasterDetailBinding;
  { Exact realized identities, independent of captions, row positions and the
    source definition's unqualified component name. }
  TNyxMasterDetailMountIDs = array of TNyxText;

function NyxHasMasterDetailMounts(ARoot: TNyxNode): Boolean;
procedure PrepareNyxMasterDetailMounts(ARoot: TNyxNode;
  const AViews: INyxCollectionBindings; const AEventView: INyxPreparedEventView;
  const AScheduler: INyxScheduler; ARefresh: TNyxMasterDetailRefresh;
  out AMounts: TNyxMasterDetailMounts; out AIDs: TNyxMasterDetailMountIDs);
{ Outward view-readiness admission belongs to Validate, before old retirement.
  The later fence inspects the prepared owners, without host work. }
procedure ValidateNyxMasterDetailMounts(const AMounts: TNyxMasterDetailMounts);
procedure CheckNyxMasterDetailMounts(const AMounts: TNyxMasterDetailMounts);
procedure InstallNyxMasterDetailMounts(const AMounts: TNyxMasterDetailMounts);
procedure RevokeNyxMasterDetailMounts(const AMounts: TNyxMasterDetailMounts);
procedure DisconnectNyxMasterDetailMounts(var AMounts: TNyxMasterDetailMounts);
function FindNyxMasterDetailMount(const AMounts: TNyxMasterDetailMounts;
  const AIDs: TNyxMasterDetailMountIDs; const AID: TNyxText): INyxMasterDetailBinding;
{ Capture before router scheduling. The local managed vector keeps preparations
  alive across outward getters without borrowing the renderer after that call. }
function EnrichNyxMasterDetailEvent(const AMounts: TNyxMasterDetailMounts;
  const AEvent: TNyxEventInfo): TNyxEventInfo;

implementation

uses
  SysUtils, nyx.controls, nyx.publication;

function NyxHasMasterDetailMounts(ARoot: TNyxNode): Boolean;
var
  LIndex: Integer;
begin
  Result := False;

  if ARoot = nil then
  begin
    Exit;
  end;

  if HasNyxMasterDetailIntent(ARoot) then
  begin
    Exit(True);
  end;
  for LIndex := 0 to ARoot.Count - 1 do
  begin

    if NyxHasMasterDetailMounts(ARoot.Children[LIndex]) then
    begin
      Exit(True);
    end;
  end;
end;

procedure PrepareNyxMasterDetailMounts(ARoot: TNyxNode;
  const AViews: INyxCollectionBindings; const AEventView: INyxPreparedEventView;
  const AScheduler: INyxScheduler; ARefresh: TNyxMasterDetailRefresh;
  out AMounts: TNyxMasterDetailMounts; out AIDs: TNyxMasterDetailMountIDs);
var
  LMounts: TNyxMasterDetailMounts;
  LIDs: TNyxMasterDetailMountIDs;

  procedure Visit(ANode: TNyxNode);
  var
    LControl: INyxControl;
    LDetail: INyxMasterDetail;
    LIntent: TNyxMasterDetailIntent;
    LPlan: INyxPreparedMasterDetailBinding;
    LCount: Integer;
    LIndex: Integer;
  begin

    if HasNyxMasterDetailIntent(ANode) then
    begin
      LIntent := GetNyxMasterDetailIntent(ANode);
      LControl := RetainNyxControl(ANode);

      if not Supports(LControl, INyxMasterDetail, LDetail) then
      begin
        raise ENyxMasterDetail.Create('Detail projection requires a specialized MasterDetail');
      end;

      if LIntent.Mode = nmiDisabled then
      begin
        LPlan := PrepareNyxMasterDetailDisabled(LDetail, AEventView, AScheduler);
      end
      else
      begin
        LPlan := PrepareNyxMasterDetailBinding(LDetail, LIntent.Options, AViews,
          AEventView, AScheduler, ARefresh);
      end;
      LCount := Length(LMounts);
      SetLength(LMounts, LCount + 1);
      SetLength(LIDs, LCount + 1);
      LMounts[LCount] := LPlan;
      LIDs[LCount] := ANode.ID;
    end;
    for LIndex := 0 to ANode.Count - 1 do
    begin
      Visit(ANode.Children[LIndex]);
    end;
  end;

begin
  AMounts := nil;
  AIDs := nil;
  LMounts := nil;
  LIDs := nil;

  if ARoot = nil then
  begin
    Exit;
  end;
  try
    { Keep traversal state local. Besides atomic output publication, this
      avoids capturing Pascal out-argument cells in pas2js nested procedures. }
    Visit(ARoot);
    AMounts := LMounts;
    AIDs := LIDs;
  except
    DisconnectNyxMasterDetailMounts(LMounts);
    raise;
  end;
end;

procedure ValidateNyxMasterDetailMounts(const AMounts: TNyxMasterDetailMounts);
var
  LIndex: Integer;
begin
  for LIndex := 0 to High(AMounts) do
  begin
    AMounts[LIndex].Validate;
  end;
end;

procedure CheckNyxMasterDetailMounts(const AMounts: TNyxMasterDetailMounts);
var
  LIndex: Integer;
  LFence: INyxPublicationReadiness;
begin
  for LIndex := 0 to High(AMounts) do
  begin

    if not Supports(AMounts[LIndex], INyxPublicationReadiness, LFence) then
    begin
      raise ENyxMasterDetail.Create('Prepared MasterDetail owner has no final readiness fence');
    end;
    LFence.CheckReady;
  end;
end;

procedure InstallNyxMasterDetailMounts(const AMounts: TNyxMasterDetailMounts);
var
  LIndex: Integer;
begin
  for LIndex := 0 to High(AMounts) do
  begin
    AMounts[LIndex].Install;
  end;
end;

procedure RevokeNyxMasterDetailMounts(const AMounts: TNyxMasterDetailMounts);
var
  LIndex: Integer;
begin
  for LIndex := 0 to High(AMounts) do
  begin
    AMounts[LIndex].Revoke;
  end;
end;

procedure DisconnectNyxMasterDetailMounts(var AMounts: TNyxMasterDetailMounts);
var
  LIndex: Integer;
  LFailure: TNyxText;
begin
  LFailure := '';
  RevokeNyxMasterDetailMounts(AMounts);
  for LIndex := 0 to High(AMounts) do
  begin
    try
      AMounts[LIndex].Disconnect;
    except
      on E: Exception do
      begin

        if LFailure = '' then
        begin
          {$ifdef PAS2JS}
          LFailure := E.Message;
          {$else}
          LFailure := UTF8Encode(UnicodeString(E.Message));
          {$endif}
        end;
      end;
    end;
  end;
  AMounts := nil;

  if LFailure <> '' then
  begin
    raise ENyxMasterDetail.Create('MasterDetail retirement completed with cleanup failure: ' + LFailure);
  end;
end;

function FindNyxMasterDetailMount(const AMounts: TNyxMasterDetailMounts;
  const AIDs: TNyxMasterDetailMountIDs; const AID: TNyxText): INyxMasterDetailBinding;
var
  LIndex: Integer;
begin
  Result := nil;
  for LIndex := 0 to High(AMounts) do
  begin

    if AIDs[LIndex] = AID then
    begin
      Exit(AMounts[LIndex].Binding);
    end;
  end;
end;

function EnrichNyxMasterDetailEvent(const AMounts: TNyxMasterDetailMounts;
  const AEvent: TNyxEventInfo): TNyxEventInfo;
var
  LMounts: TNyxMasterDetailMounts;
  LBinding: INyxMasterDetailBinding;
  LIndex: Integer;
begin
  Result := AEvent.Copy;
  LMounts := Copy(AMounts, 0, Length(AMounts));
  for LIndex := 0 to High(LMounts) do
  begin
    LBinding := LMounts[LIndex].Binding;

    if (LBinding <> nil) and LBinding.Connected then
    begin
      Result := LBinding.EnrichEdit(Result);
    end;
  end;
end;

end.
