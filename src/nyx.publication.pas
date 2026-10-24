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

unit nyx.publication;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  SysUtils,
  nyx.text,
  nyx.state,
  nyx.publication.types;

type
  ENyxPublication = class(ENyxState);
  { The ordinary exception message explains committed publication; the retained
    receiver message is exact diagnostic data for resource/status consumers.
    Nested groups forward it without adding repeated explanatory prefixes. }
  ENyxPublicationNotification = class(ENyxPublication)
  private
    FReceiverMessage: TNyxText;
    FHasReceiverMessage: Boolean;
  public
    constructor CreateReceiverFailure(const AMessage: TNyxText);
    property ReceiverMessage: TNyxText read FReceiverMessage;
    property HasReceiverMessage: Boolean read FHasReceiverMessage;
  end;

  { A UI-thread-only, single-use prepared model publication. Construction may
    reserve its owner but invokes no external callbacks. Validate admits a
    detached candidate. After successful Validate, Install must only adopt
    preallocated, admitted data:
    no user callbacks, further validation, scheduling or throwing operations.
    Trusted private projection installers obey that same adoption-only contract.
    Notify runs AFTER every participant installs; failure reports committed
    data and cannot prevent independent participants being notified.
    Retire is idempotent and releases reservations on success/failure without rolling back an
    installed model. Destruction must retire an abandoned preparation. No
    document, control or receiver is owned by the coordinator. }
  INyxPreparedPublication = nyx.publication.types.INyxPreparedPublication;
  TNyxPreparedPublications = nyx.publication.types.TNyxPreparedPublications;
  INyxPublicationReadiness = nyx.publication.types.INyxPublicationReadiness;

{ Compose an owned, single-use preparation without executing its phases. The
  same 1..2048/distinct participant limits apply. Nested model owners use this
  to contribute their whole admitted scope to one application publication;
  child notifications still wait for all outer participants to install. }
function PrepareNyxGroup(const APrepared: array of INyxPreparedPublication): INyxPreparedPublication;

{ Admits all prepared participants, installs all, then notifies all. At most
  2048 participants; nil/empty groups refuse. Oversized groups are not adopted.
  A private interface vector retains the supplied group through borrowed
  callbacks, even when a receiver releases/replaces the caller's vector.
  Every adopted preparation retires even
  after validation or notification failure. A changed receiver may dispose its
  host; each preparation must retain its model and revoke borrowed receivers.
  Extension implementations must obey the nonthrowing Install/Retire contract.
  Reservations prevent reentrant commands until the complete group retires.
  Callers must not explicitly retire another participant from a borrowed
  receiver, or invoke the phase methods behind an active coordinator. }
procedure PublishNyxGroup(const APrepared: array of INyxPreparedPublication);

implementation

{$ifdef PAS2JS}
uses JS;
{$endif}

type
  { First notification failure is detached from the active catch before any
    sibling receiver runs. Pascal failures keep the established exact diagnostic
    text; raw browser values retain identity, including undefined and objects,
    without stringifying external values. Failed is independent of empty
    text or the platform value so neither can masquerade as success. }
  TNotificationFailure = record
  private
    FFailed: Boolean;
    FMessage: TNyxText;
    {$ifdef PAS2JS}
    FRaw: Boolean;
    FValue: JSValue;
    {$endif}
  public
    procedure Capture{$ifdef PAS2JS}(AValue: JSValue){$else}(AException: Exception){$endif};
    procedure RaiseFailure;
  end;

  { Single-use owner of participant/fence leases for a nested prepared group.
    Validate checks every participant before callback-free readiness and Install;
    retirement releases the complete group after refusal or notification failure.
    Borrowed model/receiver lifetimes remain each participant's responsibility.
    Entered notification retains this group and its original participant vector
    through receivers that withdraw the caller's group reference. }
  TPreparedGroup = class(TInterfacedObject, INyxPreparedPublication, INyxPublicationReadiness)
  private
    FItems: TNyxPreparedPublications;
    FFences: array of INyxPublicationReadiness;
    FValidated: Boolean;
    FInstalled: Boolean;
    FNotified: Boolean;
    FRetired: Boolean;
  public
    constructor Create(const APrepared: array of INyxPreparedPublication);
    destructor Destroy; override;
    procedure Validate;
    procedure Install;
    procedure Notify;
    procedure Retire;
    procedure CheckReady;
  end;

procedure TNotificationFailure.Capture{$ifdef PAS2JS}(AValue: JSValue){$else}(AException: Exception){$endif};
var
  LException: Exception;
begin

  if FFailed then
  begin
    Exit;
  end;
  FFailed := True;
  {$ifdef PAS2JS}

  if not isClassInstance(AValue) or not (TObject(AValue) is Exception) then
  begin
    FRaw := True;
    FValue := AValue;
    Exit;
  end;
  LException := Exception(AValue);
  {$else}
  LException := AException;
  {$endif}

  if (LException is ENyxPublicationNotification) and
    ENyxPublicationNotification(LException).HasReceiverMessage then
  begin
    FMessage := ENyxPublicationNotification(LException).ReceiverMessage;
  end
  else
  begin
    {$ifdef PAS2JS}
    FMessage := LException.Message;
    {$else}

    if LException is ENyxState then
    begin
      FMessage := RawByteString(LException.Message);
      SetCodePage(RawByteString(FMessage), CP_UTF8, False);
    end
    else
    begin
      FMessage := TNyxText(LException.Message);
    end;
    {$endif}
  end;
end;

procedure TNotificationFailure.RaiseFailure;
{$ifdef PAS2JS}
var
  LValue: JSValue;
{$endif}
begin

  if not FFailed then
  begin
    Exit;
  end;
  {$ifdef PAS2JS}

  if FRaw then
  begin
    LValue := FValue;
    asm
      throw LValue;
    end;
  end;
  {$endif}
  raise ENyxPublicationNotification.CreateReceiverFailure(FMessage);
end;

constructor ENyxPublicationNotification.CreateReceiverFailure(const AMessage: TNyxText);
begin
  inherited Create('Models published; receiver failed: ' + AMessage);
  FReceiverMessage := AMessage;
  FHasReceiverMessage := True;
end;

function PrepareNyxGroup(const APrepared: array of INyxPreparedPublication): INyxPreparedPublication;
begin
  Result := TPreparedGroup.Create(APrepared);
end;

constructor TPreparedGroup.Create(const APrepared: array of INyxPreparedPublication);
var
  LIndex: Integer;
  LPrevious: Integer;
  LFence: INyxPublicationReadiness;
begin
  inherited Create;

  if (Length(APrepared) = 0) or (Length(APrepared) > 2048) then
  begin
    raise ENyxPublication.Create('A prepared group requires 1..2048 participants');
  end;
  SetLength(FItems, Length(APrepared));
  SetLength(FFences, Length(APrepared));
  for LIndex := 0 to High(FItems) do
  begin
    FItems[LIndex] := APrepared[LIndex];

    if FItems[LIndex] = nil then
    begin
      raise ENyxPublication.Create('Prepared group participant is missing');
    end;
    LFence := nil;
    Supports(FItems[LIndex], INyxPublicationReadiness, LFence);
    FFences[LIndex] := LFence;
    for LPrevious := 0 to LIndex - 1 do
    begin

      if FItems[LPrevious] = FItems[LIndex] then
      begin
        raise ENyxPublication.Create('Prepared group participants must be distinct');
      end;
    end;
  end;
end;

destructor TPreparedGroup.Destroy;
begin
  Retire;
  inherited Destroy;
end;

procedure TPreparedGroup.Validate;
var
  LIndex: Integer;
begin

  if FRetired or FValidated then
  begin
    raise ENyxPublication.Create('Prepared group is single-use');
  end;
  for LIndex := 0 to High(FItems) do
  begin
    FItems[LIndex].Validate;
  end;
  FValidated := True;
  CheckReady;
end;

procedure TPreparedGroup.Install;
var
  LIndex: Integer;
begin
  { Phase order is owned by the outer coordinator; installation cannot throw. }

  if not FValidated or FInstalled or FRetired then
  begin
    Exit;
  end;
  for LIndex := 0 to High(FItems) do
  begin
    FItems[LIndex].Install;
  end;
  FInstalled := True;
end;

procedure TPreparedGroup.CheckReady;
var
  LIndex: Integer;
begin

  if not FValidated or FInstalled or FRetired then
  begin
    raise ENyxPublication.Create('Prepared group readiness requires validated candidates');
  end;
  for LIndex := 0 to High(FFences) do
  begin

    if FFences[LIndex] <> nil then
    begin
      FFences[LIndex].CheckReady;
    end;
  end;
end;

procedure TPreparedGroup.Notify;
var
  LIndex: Integer;
  LLease: INyxPreparedPublication;
  LItems: TNyxPreparedPublications;
  LFailure: TNotificationFailure;
begin
  LLease := Self;

  if not FInstalled or FRetired or FNotified then
  begin
    raise ENyxPublication.Create('Prepared group notification requires installed data');
  end;
  FNotified := True;
  LFailure := Default(TNotificationFailure);
  LItems := FItems;
  for LIndex := 0 to High(LItems) do
  begin
    try
      LItems[LIndex].Notify;
    except
      {$ifdef PAS2JS}
      LFailure.Capture(JSExceptValue);
      {$else}
      on LException: Exception do
      begin
        LFailure.Capture(LException);
      end;
      {$endif}
    end;
  end;
  LFailure.RaiseFailure;
end;

procedure TPreparedGroup.Retire;
var
  LIndex: Integer;
begin

  if FRetired then
  begin
    Exit;
  end;
  FRetired := True;
  for LIndex := 0 to High(FItems) do
  begin

    if FItems[LIndex] <> nil then
    begin
      FItems[LIndex].Retire;
    end;
  end;
  FItems := nil;
  FFences := nil;
end;

procedure PublishNyxGroup(const APrepared: array of INyxPreparedPublication);
var
  LIndex: Integer;
  LPrevious: Integer;
  LFailure: TNotificationFailure;
  LPrepared: array of INyxPreparedPublication;
  LFences: array of INyxPublicationReadiness;
  LFence: INyxPublicationReadiness;
begin
  LFailure := Default(TNotificationFailure);

  if (Length(APrepared) = 0) or (Length(APrepared) > 2048) then
  begin
    raise ENyxPublication.Create('A publication group requires 1..2048 participants');
  end;
  SetLength(LPrepared, Length(APrepared));
  SetLength(LFences, Length(APrepared));
  for LIndex := 0 to High(APrepared) do
  begin
    LPrepared[LIndex] := APrepared[LIndex];
  end;
  try
    for LIndex := 0 to High(LPrepared) do
    begin

      if LPrepared[LIndex] = nil then
      begin
        raise ENyxPublication.Create('Publication participant is missing');
      end;
      for LPrevious := 0 to LIndex - 1 do
      begin

        if LPrepared[LPrevious] = LPrepared[LIndex] then
        begin
          raise ENyxPublication.Create('Publication participants must be distinct');
        end;
      end;
    end;
    for LIndex := 0 to High(LPrepared) do
    begin
      LFence := nil;
      Supports(LPrepared[LIndex], INyxPublicationReadiness, LFence);
      LFences[LIndex] := LFence;
    end;
    for LIndex := 0 to High(LPrepared) do
    begin
      LPrepared[LIndex].Validate;
    end;
    for LIndex := 0 to High(LFences) do
    begin

      if LFences[LIndex] <> nil then
      begin
        LFences[LIndex].CheckReady;
      end;
    end;
    for LIndex := 0 to High(LPrepared) do
    begin
      LPrepared[LIndex].Install;
    end;
    for LIndex := 0 to High(LPrepared) do
    begin
      try
        LPrepared[LIndex].Notify;
      except
        {$ifdef PAS2JS}
        LFailure.Capture(JSExceptValue);
        {$else}
        on LException: Exception do
        begin
          LFailure.Capture(LException);
        end;
        {$endif}
      end;
    end;
  finally
    for LIndex := 0 to High(LPrepared) do
    begin

      if LPrepared[LIndex] <> nil then
      begin
        LPrepared[LIndex].Retire;
      end;
    end;
  end;

  LFailure.RaiseFailure;
end;

end.
