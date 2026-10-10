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
unit nyx.studio.recovery.probe.browser;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.studio.recovery.status, nyx.studio.sourcecompilation,
  nyx.studio.transport;

type
  { The probe owns a managed port until completion. View receivers borrow their
    controller and must revoke that callback before destruction; a port must
    never own the probe back. Only scsCompleted supplies an admitted status. }
  INyxRuntimeRecoveryProbePort = interface(IInterface)
    ['{6C080403-81B5-4E91-B222-101026100042}']
    procedure Inspected(AState: TNyxSourceCompilationState;
      const AStatus: TNyxRuntimeRecoveryStatus; const AMessage: TNyxText);
  end;

{ Read-only same-origin readiness. Reads no source, starts no compilation and
  modifies no editor/session/checkpoint. A 404 reports an older unsupported host;
  transport or malformed replies fail visibly instead of assuming readiness.
  Cancellation aborts only this observation; it cannot cancel server recovery.
  Returned operation owns its XHR and bounded deadline through completion. }
function InspectNyxBrowserRuntimeRecovery(const APort: INyxRuntimeRecoveryProbePort;
  const APolicy: INyxTransportPolicy = nil): INyxSourceCompilation;

implementation

uses SysUtils, Web, nyx.data, nyx.model;

type
  TRecoveryProbe = class(TInterfacedObject, INyxSourceCompilation)
  private
    FPort: INyxRuntimeRecoveryProbePort;
    FLease: INyxSourceCompilation;
    FState: TNyxSourceCompilationState;
    FRequest: TJSXMLHttpRequest;
    FTimer: NativeInt;
    procedure Finish(AState: TNyxSourceCompilationState;
      const AStatus: TNyxRuntimeRecoveryStatus; const AMessage: TNyxText);
    procedure Ready;
    procedure Expired;
  public
    procedure Cancel;
    function GetState: TNyxSourceCompilationState;
  end;

function TRecoveryProbe.GetState: TNyxSourceCompilationState;
begin
  Result := FState;
end;

procedure TRecoveryProbe.Finish(AState: TNyxSourceCompilationState;
  const AStatus: TNyxRuntimeRecoveryStatus; const AMessage: TNyxText);
var
  LLease: INyxSourceCompilation;
  LPort: INyxRuntimeRecoveryProbePort;
begin
  LLease := FLease;

  if (LLease = nil) or not (LLease.State in [scsPending, scsRunning]) then
  begin
    Exit;
  end;
  FState := AState;

  if FTimer <> 0 then
  begin
    window.clearTimeout(FTimer);
    FTimer := 0;
  end;

  if FRequest <> nil then
  begin
    FRequest.onreadystatechange := nil;
    FRequest.abort;
    FRequest := nil;
  end;
  LPort := FPort;
  FPort := nil;
  FLease := nil;

  if LPort <> nil then
  begin
    LPort.Inspected(AState, AStatus, AMessage);
  end;
end;

procedure TRecoveryProbe.Cancel;
begin
  Finish(scsCancelled, Default(TNyxRuntimeRecoveryStatus), 'Readiness check cancelled.');
end;

procedure TRecoveryProbe.Expired;
begin
  Finish(scsFailed, Default(TNyxRuntimeRecoveryStatus),
    'Recovery service did not respond before the readiness deadline. Local design remains available.');
end;

procedure TRecoveryProbe.Ready;
var
  LLease: INyxSourceCompilation;
  LStatus: TNyxRuntimeRecoveryStatus;
  LText: TNyxText;
  LHTTPStatus: Integer;
  LValue: TNyxDataValue;
begin
  LLease := FLease;

  if (LLease = nil) or (FRequest = nil) or (FRequest.readyState <> 4) then
  begin
    Exit;
  end;
  LHTTPStatus := FRequest.Status;
  LText := FRequest.responseText;
  try

    if LHTTPStatus = 404 then
    begin
      LStatus := Default(TNyxRuntimeRecoveryStatus);
      LStatus.Phase := nrpUnsupported;
      Finish(scsCompleted, LStatus, 'This Studio host uses immediate project recovery.');
      Exit;
    end;

    if (LHTTPStatus <> 200) or (Length(LText) > 8192) then
    begin
      raise ENyxModel.Create('Recovery readiness unavailable. Local design remains available.');
    end;
    LValue := TNyxDataValue.ParseJSON(LText);
    LStatus := DecodeNyxRuntimeRecoveryStatus(LValue.Field('recovery'));
    { The connect reply also contains a private capability. The probe discards
      it and never stores it in a view, exported project, history or diagnostic. }
    Finish(scsCompleted, LStatus, '');
  except
    on LException: Exception do
    begin
      Finish(scsFailed, Default(TNyxRuntimeRecoveryStatus), LException.Message);
    end;
  end;
end;

function InspectNyxBrowserRuntimeRecovery(const APort: INyxRuntimeRecoveryProbePort;
  const APolicy: INyxTransportPolicy): INyxSourceCompilation;
var
  LOwner: TRecoveryProbe;
  LPolicy: INyxTransportPolicy;
  LLimits: TNyxTransportLimits;
begin

  if APort = nil then
  begin
    raise ENyxModel.Create('Recovery inspection requires its managed delivery port');
  end;
  LPolicy := APolicy;

  if LPolicy = nil then
  begin
    LPolicy := NewNyxTransportPolicy;
  end;
  LLimits := LPolicy.Snapshot;
  ValidateNyxTransportLimits(LLimits);
  LOwner := TRecoveryProbe.Create;
  Result := LOwner;
  LOwner.FState := scsRunning;
  LOwner.FPort := APort;
  LOwner.FLease := Result;
  try
    LOwner.FRequest := TJSXMLHttpRequest.new;
    LOwner.FRequest.open('POST', 'api/recovery/connect', True);
    LOwner.FRequest.setRequestHeader('Content-Type', 'application/json');
    LOwner.FRequest.timeout := LLimits.DeadlineMS;
    LOwner.FRequest.onreadystatechange := @LOwner.Ready;
    LOwner.FTimer := window.setTimeout(@LOwner.Expired, LLimits.DeadlineMS);
    LOwner.FRequest.send('{}');
  except
    on LException: Exception do
    begin
      LOwner.Finish(scsFailed, Default(TNyxRuntimeRecoveryStatus), LException.Message);
    end;
  end;
end;

end.
