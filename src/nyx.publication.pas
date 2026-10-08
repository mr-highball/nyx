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
  ENyxPublicationNotification = class(ENyxPublication);

  { A UI-thread-only, single-use prepared model publication. Construction may
    reserve its owner but invokes no external callbacks. Validate admits a
    detached candidate. After successful Validate, Install must only adopt
    preallocated, admitted data:
    no user callbacks, further validation, scheduling or throwing operations.
    Trusted private projection installers obey that same adoption-only contract.
    Notify runs AFTER every participant installs; failure reports committed
    data and cannot prevent independent participants being notified.
    Retire releases reservations on success/failure without rolling back an
    installed model. Destruction must retire an abandoned preparation. No
    document, control or receiver is owned by the coordinator. }
  INyxPreparedPublication = interface
    ['{9367619F-A6CB-46D1-833C-C42473701750}']
    procedure Validate;
    procedure Install;
    procedure Notify;
    procedure Retire;
  end;

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
    raise ENyxPublicationNotification.Create('Models published; receiver failed: ' + LError);
  end;
end;

end.
