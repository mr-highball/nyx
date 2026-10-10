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

unit nyx.studio.recovery.browser;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.studio.transport, nyx.studio.sourcecompilation;

type
  { Closed presentation state for an optional owning browser recovery host.
    Counts refer to unique accepted units, not unfinished draft buffers. }
  TNyxBrowserRecoveryPhase = (brpConnecting, brpReadingUnit, brpCompiling,
    brpExecuting, brpPublishing, brpCancelling, brpCompleted, brpCancelled,
    brpFailed);
  { Resume never silently restarts cancelled work. The operator can explicitly
    request a fresh attempt once cancellation and compiler join have completed. }
  { CancelRetained reconnects to an interrupted/failed attempt and confirms
    compiler join without reading or executing another saved source unit. }
  TNyxBrowserRecoveryStart = (brsResume, brsRetryCancelled, brsCancelRetained);
  { The operation owns this managed delivery port until terminal notification.
    A view-owned implementation must revoke its borrowed UI callback on closure;
    it must not own the operation back and create a reference cycle. Failure
    retains server input. Cancellation is confirmed only after owned server
    jobs join; a lost acknowledgement can mean publication already happened. }
  INyxBrowserRecoveryPort = interface(IInterface)
    ['{6C080403-81B5-4E91-B222-101026100041}']
    procedure Progress(APhase: TNyxBrowserRecoveryPhase;
      AAcceptedUnits, ATotalUnits, ASessions: Integer);
    procedure Finished(AState: TNyxSourceCompilationState;
      const AMessage: TNyxText);
  end;

{ Explicit host execution, never an output/project flag or automatic enrollment.
  Connects to the same-origin recovery endpoint, reads one immutable source unit,
  delegates to its existing compiler queue, executes the exact compiled worker
  and submits that worker's original bounded producer message. No mutable editor
  or saved project is borrowed. Requests retry exact bytes after lost responses;
  each unit has a 150-second observation budget and each worker 30 seconds.
  Missing tools fail without replacing the checkpoint. Cancel owns worker/XHR
  retirement and observes server join before reporting confirmed cancellation. }
function StartNyxBrowserRuntimeRecovery(const APort: INyxBrowserRecoveryPort;
  const APolicy: INyxTransportPolicy = nil;
  AStart: TNyxBrowserRecoveryStart = brsResume): INyxSourceCompilation;

implementation

uses SysUtils, JS, Web, nyx.data, nyx.bytes, nyx.model, nyx.studio.builds,
  nyx.studio.sourceprojection, nyx.studio.sourcebuilds, nyx.studio.editorbuild,
  nyx.studio.recovery.status;

type
  TRecoveryWire = (rwConnect, rwRetry, rwUnit, rwRequest, rwJob, rwComplete, rwCancel,
    rwJoin);
  TBrowserRecovery = class(TInterfacedObject, INyxSourceCompilation)
  private
    FState: TNyxSourceCompilationState;
    FPort: INyxBrowserRecoveryPort;
    FLease: INyxSourceCompilation;
    FLimits: TNyxTransportLimits;
    FToken: TNyxText;
    FOperation: TNyxText;
    FSource: TNyxText;
    FJob: TNyxText;
    FProducer: TNyxText;
    FBody: TNyxText;
    FBuild: INyxSourceProjectionBuild;
    FAccepted: Integer;
    FTotal: Integer;
    FSessions: Integer;
    FMode: TRecoveryWire;
    FCancelled: Boolean;
    FStart: TNyxBrowserRecoveryStart;
    FRequest: TJSXMLHttpRequest;
    FTimer: NativeInt;
    FDeadline: NativeInt;
    FWorker: TJSWorker;
    FWorkerDeadline: NativeInt;
    FMessageHandler: TJSEventHandler;
    FErrorHandler: TJSEventHandler;
    procedure Report(APhase: TNyxBrowserRecoveryPhase);
    procedure RetireWorker;
    procedure Retire;
    procedure Finish(AState: TNyxSourceCompilationState; const AMessage: TNyxText);
    procedure RenewDeadline;
    procedure Expired;
    procedure WorkerExpired;
    procedure Advance(AMode: TRecoveryWire);
    procedure Send;
    procedure Ready;
    procedure ReadProgress(const AValue: TNyxDataValue; out APending: Boolean);
    procedure RunWorker;
    function Received(AEvent: TJSEvent): Boolean;
    function WorkerError(AEvent: TJSEvent): Boolean;
  public
    procedure Cancel;
    function GetState: TNyxSourceCompilationState;
  end;

procedure TBrowserRecovery.Report(APhase: TNyxBrowserRecoveryPhase);
begin

  if FPort <> nil then
  begin
    FPort.Progress(APhase, FAccepted, FTotal, FSessions);
  end;
end;

function TBrowserRecovery.GetState: TNyxSourceCompilationState;
begin
  Result := FState;
end;

procedure TBrowserRecovery.RetireWorker;
begin

  if FWorkerDeadline <> 0 then
  begin
    window.clearTimeout(FWorkerDeadline);
    FWorkerDeadline := 0;
  end;

  if FWorker <> nil then
  begin
    FWorker.removeEventListener('message', FMessageHandler);
    FWorker.removeEventListener('error', FErrorHandler);
    FWorker.terminate;
    FWorker := nil;
  end;
end;

procedure TBrowserRecovery.Retire;
begin
  RetireWorker;

  if FRequest <> nil then
  begin
    FRequest.onreadystatechange := nil;
    FRequest.abort;
    FRequest := nil;
  end;

  if FTimer <> 0 then
  begin
    window.clearTimeout(FTimer);
    FTimer := 0;
  end;

  if FDeadline <> 0 then
  begin
    window.clearTimeout(FDeadline);
    FDeadline := 0;
  end;
end;

procedure TBrowserRecovery.Finish(AState: TNyxSourceCompilationState;
  const AMessage: TNyxText);
var
  LLease: INyxSourceCompilation;
  LPort: INyxBrowserRecoveryPort;
begin
  LLease := FLease;

  if LLease = nil then
  begin
    Exit;
  end;

  if not (LLease.State in [scsPending, scsRunning]) then
  begin
    Exit;
  end;
  Retire;
  FState := AState;
  LPort := FPort;
  FPort := nil;
  FBuild := nil;
  FProducer := '';
  FSource := '';
  FBody := '';
  FLease := nil;

  if LPort <> nil then
  begin
    case AState of
      scsCompleted:
        begin
          LPort.Progress(brpCompleted, FAccepted, FTotal, FSessions);
        end;
      scsCancelled:
        begin
          LPort.Progress(brpCancelled, FAccepted, FTotal, FSessions);
        end;
      scsFailed:
        begin
          LPort.Progress(brpFailed, FAccepted, FTotal, FSessions);
        end;
    else
      begin
        raise ENyxModel.Create('Recovery may finish only with a terminal producer state');
      end;
    end;
    LPort.Finished(AState, AMessage);
  end;
end;

procedure TBrowserRecovery.RenewDeadline;
begin

  if FDeadline <> 0 then
  begin
    window.clearTimeout(FDeadline);
  end;
  FDeadline := window.setTimeout(@Expired, 150000);
end;

procedure TBrowserRecovery.Expired;
begin
  Finish(scsFailed, 'Recovery observation timed out. Reconnect to inspect retained input; an unacknowledged completion may already have published.');
end;

procedure TBrowserRecovery.WorkerExpired;
begin
  Finish(scsFailed, 'The saved Pascal constructor worker exceeded its execution deadline. Recovery input is retained.');
end;

procedure TBrowserRecovery.Cancel;
begin

  if not (FState in [scsPending, scsRunning]) then
  begin
    Exit;
  end;
  FCancelled := True;
  RetireWorker;
  { Keep an in-flight admission/completion request. Its exact acknowledgement
    determines which job to join, or whether publication already happened. }

  if (FRequest = nil) and (FTimer = 0) and (FToken <> '') then
  begin
    Advance(rwCancel);
  end;
end;

procedure TBrowserRecovery.Advance(AMode: TRecoveryWire);
var
  LValue: TNyxDataValue;
  LPhase: TNyxBrowserRecoveryPhase;
  LReport: Boolean;
begin
  FMode := AMode;
  LReport := True;
  LPhase := brpConnecting;
  case AMode of
    rwConnect:
      begin
        LValue := NyxObject([]);
      end;
    rwRetry:
      begin
        LReport := False;
        LValue := NyxObject([NyxField('mode', NyxData('retry'))]);
      end;
    rwUnit:
      begin
        RenewDeadline;
        LPhase := brpReadingUnit;
        LValue := NyxObject([NyxField('mode', NyxData('unit')),
          NyxField('unit', NyxData(FAccepted))]);
      end;
    rwRequest:
      begin
        LPhase := brpCompiling;
        LValue := NyxObject([NyxField('mode', NyxData('request')),
          NyxField('unit', NyxData(FAccepted)),
          NyxField('operationId', NyxData(FOperation + '-' + TNyxText(IntToStr(FAccepted))))]);
      end;
    rwJob, rwJoin:
      begin
        LReport := AMode = rwJob;
        LPhase := brpCompiling;
        LValue := NyxObject([NyxField('mode', NyxData('job')),
          NyxField('job', NyxData(FJob))]);
      end;
    rwComplete:
      begin
        LPhase := brpPublishing;
        LValue := NyxObject([NyxField('mode', NyxData('complete')),
          NyxField('unit', NyxData(FAccepted)), NyxField('job', NyxData(FJob)),
          NyxField('producer', NyxData(FProducer))]);
      end;
    rwCancel:
      begin
        LPhase := brpCancelling;
        LValue := NyxObject([NyxField('mode', NyxData('cancel'))]);
      end;
  end;
  FBody := LValue.ToJSON;
  Send;
  { Publish the observation after the request owns its callback. A port may
    request cancellation here; it must not dispatch a competing request while
    this admission acknowledgement is still in flight. }

  if LReport and (FState in [scsPending, scsRunning]) then
  begin
    Report(LPhase);
  end;
end;

procedure TBrowserRecovery.Send;
begin
  FTimer := 0;
  try
    FRequest := TJSXMLHttpRequest.new;
    FRequest.onreadystatechange := @Ready;

    if FMode = rwConnect then
    begin
      FRequest.open('POST', 'api/recovery/connect', True);
    end
    else
    begin
      FRequest.open('POST', 'api/recovery', True);
      FRequest.setRequestHeader('X-Nyx-Recovery', FToken);
    end;
    FRequest.timeout := FLimits.DeadlineMS;
    FRequest.setRequestHeader('Content-Type', 'application/json; charset=utf-8');
    FRequest.send(FBody);
  except
    on LException: Exception do
    begin
      Finish(scsFailed, LException.Message);
    end;
  end;
end;

procedure TBrowserRecovery.ReadProgress(const AValue: TNyxDataValue;
  out APending: Boolean);
var
  LStatus: TNyxRuntimeRecoveryStatus;
begin
  LStatus := DecodeNyxRuntimeRecoveryStatus(AValue);
  APending := LStatus.Pending;
  FTotal := LStatus.Units;
  FAccepted := LStatus.Accepted;
  FSessions := LStatus.Sessions;
end;

procedure TBrowserRecovery.Ready;
var
  LLease: INyxSourceCompilation;
  LStatus: Integer;
  LText: TNyxText;
  LValue: TNyxDataValue;
  LPending: Boolean;
  LPrevious: Integer;
  LJob: TNyxText;
  LState: TNyxBuildJobState;
begin

  if (FRequest = nil) or (FRequest.readyState <> 4) then
  begin
    Exit;
  end;
  LLease := FLease;

  if LLease = nil then
  begin
    Exit;
  end;

  if not (LLease.State in [scsPending, scsRunning]) then
  begin
    Exit;
  end;
  LStatus := FRequest.status;
  LText := FRequest.responseText;
  FRequest.onreadystatechange := nil;
  FRequest := nil;
  try

    if LStatus = 0 then
    begin
      { Never invent another operation after a lost reply. This retries the same
        admission/producer bytes, including a final whole-registry completion. }
      FTimer := window.setTimeout(@Send, 250);
      Exit;
    end;

    if NyxUTF8ByteCount(LText) > NyxProjectionMaximumResultBytes + 8192 then
    begin
      raise ENyxModel.Create('Recovery reply exceeds its observation budget');
    end;
    LValue := TNyxDataValue.ParseJSON(LText);

    if LStatus <> 200 then
    begin
      raise ENyxModel.Create('Recovery refused: ' + LValue.Field('error').AsText);
    end;
    case FMode of
      rwConnect, rwRetry:
        begin
          ReadProgress(LValue.Field('recovery'), LPending);

          if not LPending then
          begin
            Finish(scsCompleted, 'Shared runtime recovery is ready.');
            Exit;
          end;
          FToken := LValue.Field('token').AsText;

          if (Length(FToken) < 1) or (Length(FToken) > 128) or
            (Pos(#10, FToken) > 0) or (Pos(#13, FToken) > 0) or (Pos(#0, FToken) > 0) then
          begin
            raise ENyxModel.Create('Recovery connection has no admitted private capability');
          end;
          FJob := LValue.Field('recovery').Field('job').AsText;
          FState := scsRunning;

          if FCancelled then
          begin
            Advance(rwCancel);
          end
          else if LValue.Field('recovery').Field('state').AsText = 'cancelled' then
          begin

            if FStart <> brsRetryCancelled then
            begin
              raise ENyxModel.Create('Recovery was cancelled. Request an explicit retry to reopen retained input.');
            end;
            Advance(rwRetry);
          end
          else
          begin
            Advance(rwUnit);
          end;
        end;
      rwUnit:
        begin

          if LValue.Field('unit').AsInteger <> FAccepted then
          begin
            raise ENyxModel.Create('Recovery substituted another saved unit');
          end;
          FSource := LValue.Field('source').AsText;
          ValidateNyxProjectionSource(FSource);

          if FCancelled then
          begin
            Advance(rwCancel);
          end
          else if FJob <> '' then
          begin
            Advance(rwJob);
          end
          else
          begin
            Advance(rwRequest);
          end;
        end;
      rwRequest, rwJob, rwJoin:
        begin
          LJob := LValue.Field('job').AsText;
          NyxBuildJob(LJob);

          if (FJob <> '') and (FJob <> LJob) then
          begin
            raise ENyxModel.Create('Recovery substituted another compiler job');
          end;
          FJob := LJob;
          LState := ParseNyxBuildJobState(LValue.Field('state').AsText);

          if FMode = rwJoin then
          begin

            if NyxBuildJobTerminal(LState) then
            begin
              Finish(scsCancelled, 'Recovery cancelled after the compiler joined. Saved input is retained.');
            end
            else
            begin
              FTimer := window.setTimeout(@Send, 100);
            end;
          end
          else if FCancelled then
          begin
            Advance(rwCancel);
          end
          else if not NyxBuildJobTerminal(LState) then
          begin
            FMode := rwJob;
            FBody := NyxObject([NyxField('mode', NyxData('job')),
              NyxField('job', NyxData(FJob))]).ToJSON;
            FTimer := window.setTimeout(@Send, 100);
          end
          else
          begin
            FBuild := DecodeNyxBrowserSourceBuild(FSource, LValue.Field('receipt'));

            if (LState <> bjsSucceeded) or (FBuild.Projection.State <> spsCompiled) then
            begin
              raise ENyxModel.Create('Saved Pascal could not compile: ' + FBuild.Projection.Message);
            end;
            RunWorker;
          end;
        end;
      rwComplete:
        begin
          LPrevious := FAccepted;
          ReadProgress(LValue, LPending);

          if FAccepted <> LPrevious + 1 then
          begin
            raise ENyxModel.Create('Recovery acknowledgement differs from its owning unit');
          end;
          FProducer := '';
          FBuild := nil;
          FJob := '';

          if not LPending then
          begin
            Finish(scsCompleted, 'All saved projects and history have recovered.');
          end
          else if FCancelled then
          begin
            Advance(rwCancel);
          end
          else
          begin
            Advance(rwUnit);
          end;
        end;
      rwCancel:
        begin
          ReadProgress(LValue, LPending);

          if not LPending or (LValue.Field('state').AsText <> 'cancelled') then
          begin
            raise ENyxModel.Create('Recovery cancellation was not acknowledged');
          end;
          LJob := LValue.Field('job').AsText;

          if (FJob <> '') and (FJob <> LJob) then
          begin
            raise ENyxModel.Create('Recovery cancellation substituted its owning job');
          end;
          FJob := LJob;

          if FJob = '' then
          begin
            Finish(scsCancelled, 'Recovery cancelled. Saved input is retained.');
          end
          else
          begin
            Advance(rwJoin);
          end;
        end;
    end;
  except
    on LException: Exception do
    begin
      Finish(scsFailed, LException.Message);
    end;
  end;
end;

procedure TBrowserRecovery.RunWorker;
begin
  FMessageHandler := Received;
  FErrorHandler := WorkerError;
  FWorker := TJSWorker.new(FBuild.Artifact);
  FWorker.addEventListener('message', FMessageHandler);
  FWorker.addEventListener('error', FErrorHandler);
  FWorkerDeadline := window.setTimeout(@WorkerExpired, 30000);
  Report(brpExecuting);
end;

function TBrowserRecovery.Received(AEvent: TJSEvent): Boolean;
var
  LLease: INyxSourceCompilation;
  LProjection: INyxSourceProjection;
begin
  Result := False;
  LLease := FLease;

  if LLease = nil then
  begin
    Exit;
  end;

  if (FWorker = nil) or (AEvent.target <> FWorker) or FCancelled or
    (LLease.State <> scsRunning) then
  begin
    Exit;
  end;
  try

    if not isString(TJSMessageEvent(AEvent).data) then
    begin
      raise ENyxModel.Create('Recovery worker requires its bounded text result');
    end;
    FProducer := TNyxText(TJSMessageEvent(AEvent).data);
    LProjection := ReceiveNyxSourceProjection(FSource, FBuild.Reference,
      btBrowser, FProducer, FBuild.Projection.Report);

    if LProjection.State <> spsExecuted then
    begin
      raise ENyxModel.Create('Saved Pascal did not execute successfully: ' + LProjection.Message);
    end;
    RetireWorker;
    Advance(rwComplete);
  except
    on LException: Exception do
    begin
      Finish(scsFailed, LException.Message);
    end;
  end;
end;

function TBrowserRecovery.WorkerError(AEvent: TJSEvent): Boolean;
begin
  Result := False;

  if (FWorker = nil) or (AEvent.target <> FWorker) then
  begin
    Exit;
  end;
  Finish(scsFailed, 'The saved Pascal constructor worker failed to load or execute. Recovery input is retained.');
end;

function StartNyxBrowserRuntimeRecovery(const APort: INyxBrowserRecoveryPort;
  const APolicy: INyxTransportPolicy;
  AStart: TNyxBrowserRecoveryStart): INyxSourceCompilation;
var
  LOwner: TBrowserRecovery;
  LPolicy: INyxTransportPolicy;
  LLimits: TNyxTransportLimits;
  LIdentity: TGUID;
begin

  if APort = nil then
  begin
    raise ENyxModel.Create('Browser runtime recovery requires its managed delivery port');
  end;
  LPolicy := APolicy;

  if LPolicy = nil then
  begin
    LPolicy := NewNyxTransportPolicy;
  end;
  LLimits := LPolicy.Snapshot;
  ValidateNyxTransportLimits(LLimits);
  CreateGUID(LIdentity);
  LOwner := TBrowserRecovery.Create;
  Result := LOwner;
  LOwner.FState := scsPending;
  LOwner.FPort := APort;
  LOwner.FLimits := LLimits;
  LOwner.FStart := AStart;
  LOwner.FCancelled := AStart = brsCancelRetained;
  LOwner.FOperation := 'recovery-' + GUIDToString(LIdentity);
  LOwner.FLease := Result;
  LOwner.RenewDeadline;
  try
    LOwner.Advance(rwConnect);
  except
    on LException: Exception do
    begin
      LOwner.Finish(scsFailed, LException.Message);
    end;
  end;
end;

end.
