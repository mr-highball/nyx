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
  nyx.state;

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
  INyxPreparedPublication = interface
    ['{9367619F-A6CB-46D1-833C-C42473701750}']
    procedure Validate;
    procedure Install;
    procedure Notify;
    procedure Retire;
  end;

  TNyxPreparedPublications = array of INyxPreparedPublication;

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

type
  TPreparedGroup = class(TInterfacedObject, INyxPreparedPublication)
  private
    FItems: TNyxPreparedPublications;
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
begin
  inherited Create;

  if (Length(APrepared) = 0) or (Length(APrepared) > 2048) then
  begin
    raise ENyxPublication.Create('A prepared group requires 1..2048 participants');
  end;
  SetLength(FItems, Length(APrepared));
  for LIndex := 0 to High(FItems) do
  begin
    FItems[LIndex] := APrepared[LIndex];

    if FItems[LIndex] = nil then
    begin
      raise ENyxPublication.Create('Prepared group participant is missing');
    end;
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

procedure TPreparedGroup.Notify;
var
  LIndex: Integer;
  LError: TNyxText;
  LFailed: Boolean;
begin

  if not FInstalled or FRetired or FNotified then
  begin
    raise ENyxPublication.Create('Prepared group notification requires installed data');
  end;
  FNotified := True;
  LError := '';
  LFailed := False;
  for LIndex := 0 to High(FItems) do
  begin
    try
      FItems[LIndex].Notify;
    except
      on LException: Exception do
      begin

        if not LFailed then
        begin
          LFailed := True;

          if (LException is ENyxPublicationNotification) and
            ENyxPublicationNotification(LException).HasReceiverMessage then
          begin
            LError := ENyxPublicationNotification(LException).ReceiverMessage;
          end
          else
          begin
            {$IFDEF PAS2JS}
            LError := LException.Message;
            {$ELSE}

            if LException is ENyxState then
            begin
              LError := RawByteString(LException.Message);
              SetCodePage(RawByteString(LError), CP_UTF8, False);
            end
            else
            begin
              LError := TNyxText(LException.Message);
            end;
            {$ENDIF}
          end;
        end;
      end;
    end;
  end;

  if LFailed then
  begin
    raise ENyxPublicationNotification.CreateReceiverFailure(LError);
  end;
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
end;

procedure PublishNyxGroup(const APrepared: array of INyxPreparedPublication);
var
  LIndex: Integer;
  LPrevious: Integer;
  LError: TNyxText;
  LHasError: Boolean;
  LPrepared: array of INyxPreparedPublication;
begin
  LError := '';
  LHasError := False;

  if (Length(APrepared) = 0) or (Length(APrepared) > 2048) then
  begin
    raise ENyxPublication.Create('A publication group requires 1..2048 participants');
  end;
  SetLength(LPrepared, Length(APrepared));
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
      LPrepared[LIndex].Validate;
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
        on LException: Exception do
        begin

          if not LHasError then
          begin
            LHasError := True;

            if (LException is ENyxPublicationNotification) and
              ENyxPublicationNotification(LException).HasReceiverMessage then
            begin
              LError := ENyxPublicationNotification(LException).ReceiverMessage;
            end
            else
            begin
              {$IFDEF PAS2JS}
              LError := LException.Message;
              {$ELSE}

              if LException is ENyxState then
              begin
                LError := RawByteString(LException.Message);
                SetCodePage(RawByteString(LError), CP_UTF8, False);
              end
              else
              begin
                LError := TNyxText(LException.Message);
              end;
              {$ENDIF}
            end;
          end;
        end;
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

  if LHasError then
  begin
    raise ENyxPublicationNotification.CreateReceiverFailure(LError);
  end;
end;

end.
