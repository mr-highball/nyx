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
unit nyx.studio.recovery.status;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.data;

type
  { Small capability-free observation. Unsupported is a local compatibility
    outcome for older hosts, never a wire value. Status conveys no execution
    permission, saved source, registry owner or mutable session. }
  TNyxRuntimeRecoveryPhase = (nrpNotRequired, nrpAwaiting, nrpPublished,
    nrpCancelled, nrpUnsupported);
  TNyxRuntimeRecoveryStatus = record
    Phase: TNyxRuntimeRecoveryPhase;
    Pending: Boolean;
    Accepted: Integer;
    Units: Integer;
    Sessions: Integer;
  end;

{ Admit the bounded progress envelope returned by the recovery host. Reject
  unknown states, contradictory pending/publication and invalid counts before
  returning a value. This does not admit a project or certify execution. }
function DecodeNyxRuntimeRecoveryStatus(
  const AValue: TNyxDataValue): TNyxRuntimeRecoveryStatus;

implementation

uses nyx.model;

function DecodeNyxRuntimeRecoveryStatus(
  const AValue: TNyxDataValue): TNyxRuntimeRecoveryStatus;
var
  LState: TNyxText;
begin
  Result := Default(TNyxRuntimeRecoveryStatus);
  Result.Pending := AValue.Field('pending').AsBoolean;
  LState := AValue.Field('state').AsText;

  if LState = 'not-required' then
  begin

    if Result.Pending then
    begin
      raise ENyxModel.Create('A missing recovery checkpoint cannot be pending');
    end;
    Exit;
  end;

  if LState = 'awaiting-execution' then
  begin
    Result.Phase := nrpAwaiting;
  end
  else if LState = 'published' then
  begin
    Result.Phase := nrpPublished;
  end
  else if LState = 'cancelled' then
  begin
    Result.Phase := nrpCancelled;
  end
  else
  begin
    raise ENyxModel.Create('Recovery host returned an unknown phase');
  end;
  Result.Accepted := AValue.Field('accepted').AsInteger;
  Result.Units := AValue.Field('units').AsInteger;
  Result.Sessions := AValue.Field('sessions').AsInteger;

  if (Result.Sessions < 1) or (Result.Sessions > 9) or (Result.Units < 1) or
    (Result.Units > 459) or (Result.Accepted < 0) or (Result.Accepted > Result.Units) or
    (Result.Pending <> (Result.Phase <> nrpPublished)) or
    ((Result.Phase = nrpPublished) and (Result.Accepted <> Result.Units)) then
  begin
    raise ENyxModel.Create('Recovery progress differs from its bounded checkpoint');
  end;
end;

end.
