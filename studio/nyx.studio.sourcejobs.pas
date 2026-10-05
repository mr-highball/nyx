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
unit nyx.studio.sourcejobs;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.types, nyx.model, nyx.scheduler, nyx.schema,
  nyx.source.preparation, nyx.studio.session
  {$ifdef PAS2JS}, JS, Web{$endif};

type
  TNyxSourceCommandState = (nssIdle, nssPreparing, nssApplied, nssRejected,
    nssStale, nssCancelled, nssFailed);
  TNyxSourceCommandChanged = procedure(AState: TNyxSourceCommandState;
    const AMessage: TNyxText) of object;
  TNyxSourceCommands = class;

  { Retained couriers borrow only this revocable UI port. Detach revokes it before
    any controller/session is freed; neither a worker nor a queued result can
    dereference the editor directly. Its owner field is accessed on the UI only. }
  INyxSourceCommandPort = interface(IInterface)
    ['{8B6CF439-AE07-4C41-A6D8-79C501B85404}']
    procedure Deliver(ASequence: Integer; const ARequest: TNyxStudioSourceRequest;
      const APrepared: INyxPreparedSource; const AFailure: TNyxText);
    procedure Detach;
  end;

  { One controller belongs to one session, which must outlive it. Source inputs
    are immutable, native admission uses the public scheduler's real workers,
    and the browser uses a separately compiled Pascal worker program. Only one
    preparation runs at a time; repeated Apply replaces one queued request.
    Superseded results retire without publication. Ordinary compiler diagnostics
    and application builds remain separate operations.

    Detach is a UI operation and immediately revokes all editor callbacks. Native
    destruction drains retained workers while servicing their UI handoffs, after
    detachment. A host with several controllers must detach ALL of them before
    destroying any, since that drain may service host synchronization. Browser
    destruction terminates its owned worker. No accepted document runs on a worker. }
  TNyxSourceCommands = class
  private
    FSession: TNyxStudioSession;
    FScheduler: INyxScheduler;
    FPort: INyxSourceCommandPort;
    FChanged: TNyxSourceCommandChanged;
    FState: TNyxSourceCommandState;
    FMessage: TNyxText;
    FSequence: Integer;
    FActiveSequence: Integer;
    FRunning: Boolean;
    FQueued: Boolean;
    FDetached: Boolean;
    FRequest: TNyxStudioSourceRequest;
    FActiveRequest: TNyxStudioSourceRequest;
    FSchemas: INyxSchemaSnapshot;
    FActiveSchemas: INyxSchemaSnapshot;
    {$ifndef PAS2JS}
    FExecutions: array of INyxExecution;
    {$endif}
    {$ifdef PAS2JS}
    FWorker: TJSWorker;
    FWorkerURL: TNyxText;
    FTimeout: NativeInt;
    FReceiveHandler: TJSEventHandler;
    FErrorHandler: TJSEventHandler;
    function Receive(AEvent: TJSEvent): Boolean;
    function WorkerError(AEvent: TJSEvent): Boolean;
    procedure WorkerTimeout;
    procedure RetireWorker;
    {$endif}
    procedure Notify(AState: TNyxSourceCommandState; const AMessage: TNyxText);
    procedure StartQueued;
    procedure Finish(ASequence: Integer; const ARequest: TNyxStudioSourceRequest;
      const APrepared: INyxPreparedSource; const AFailure: TNyxText);
  public
    constructor Create(ASession: TNyxStudioSession;
      AChanged: TNyxSourceCommandChanged; const AWorkerURL: TNyxText = 'nyx_source_worker.js');
    destructor Destroy; override;
    { Capture/queue the current exact draft. A stale draft or exhausted command
      sequence raises before dispatch. Processor failures retain the current pair
      and appear through State/Message and the optional UI callback. }
    procedure Apply;
    { Supersede running work and discard the queued request. Running native work
      may finish, but its sequence can no longer publish. Does not erase a draft. }
    procedure Cancel;
    { Permanently revoke UI delivery. Safe before freeing other project contexts;
      use destruction afterward to retire this controller's retained work. }
    procedure Detach;
    { Consume ordinary Nyx Apply/Restore buttons. Typing stays in the session's
      existing source router, so editor focus and text selection remain mounted. }
    function Route(ANode: TNyxNode; ATrigger: TNyxTrigger): Boolean;
    property State: TNyxSourceCommandState read FState;
    property Message: TNyxText read FMessage;
    { UI-thread callback rebinding supports transferring a standalone editor into
      a project context. Callbacks may enqueue work, but must not free this
      controller inline while its delivery stack is active. }
    property OnChanged: TNyxSourceCommandChanged read FChanged write FChanged;
  end;

implementation

uses
  SysUtils, nyx.data
  {$ifndef PAS2JS}, Classes{$endif};

type
  TSourcePort = class(TInterfacedObject, INyxSourceCommandPort)
  public
    Owner: TNyxSourceCommands;
    procedure Deliver(ASequence: Integer; const ARequest: TNyxStudioSourceRequest;
      const APrepared: INyxPreparedSource; const AFailure: TNyxText);
    procedure Detach;
  end;

  TSourceDelivery = class(TInterfacedObject, INyxWork)
  public
    Port: INyxSourceCommandPort;
    Sequence: Integer;
    Request: TNyxStudioSourceRequest;
    Prepared: INyxPreparedSource;
    Failure: TNyxText;
    procedure Execute(const AExecution: INyxExecution);
  end;

  {$ifndef PAS2JS}
  TSourcePreparation = class(TInterfacedObject, INyxWork)
  public
    Port: INyxSourceCommandPort;
    Scheduler: INyxScheduler;
    Sequence: Integer;
    Request: TNyxStudioSourceRequest;
    Schemas: INyxSchemaSnapshot;
    procedure Execute(const AExecution: INyxExecution);
  end;
  {$endif}

procedure TSourcePort.Detach;
begin
  Owner := nil;
end;

procedure TSourcePort.Deliver(ASequence: Integer;
  const ARequest: TNyxStudioSourceRequest; const APrepared: INyxPreparedSource;
  const AFailure: TNyxText);
begin

  if Owner <> nil then
  begin
    Owner.Finish(ASequence, ARequest, APrepared, AFailure);
  end;
end;

procedure TSourceDelivery.Execute(const AExecution: INyxExecution);
begin

  if not AExecution.Cancelled then
  begin
    Port.Deliver(Sequence, Request, Prepared, Failure);
  end;
end;

{$ifndef PAS2JS}
procedure TSourcePreparation.Execute(const AExecution: INyxExecution);
var
  LDelivery: TSourceDelivery;
  LWork: INyxWork;
begin
  LDelivery := TSourceDelivery.Create;
  LWork := LDelivery;
  LDelivery.Port := Port;
  LDelivery.Sequence := Sequence;
  LDelivery.Request := Request;
  try
    LDelivery.Prepared := PrepareNyxSource(Request.Source, Schemas);
  except
    on LException: Exception do
    begin
      LDelivery.Failure := UTF8Encode(UnicodeString(LException.Message));
    end;
  end;

  if not AExecution.Cancelled then
  begin
    { No accepted state is accessed here. Retirement must reach the UI even
      when a newer request superseded this one; sequence guards decide admission. }
    Scheduler.PostUI(LWork);
  end;
end;
{$endif}

constructor TNyxSourceCommands.Create(ASession: TNyxStudioSession;
  AChanged: TNyxSourceCommandChanged; const AWorkerURL: TNyxText);
var
  LPort: TSourcePort;
begin
  inherited Create;

  if ASession = nil then
  begin
    raise ENyxModel.Create('Source scheduling requires an owned Studio session');
  end;
  FScheduler := NewNyxScheduler;
  FScheduler.RequireUI;
  FSession := ASession;
  FChanged := AChanged;
  LPort := TSourcePort.Create;
  FPort := LPort;
  LPort.Owner := Self;
  {$ifdef PAS2JS}
  FWorkerURL := AWorkerURL;
  FTimeout := -1;
  FReceiveHandler := Receive;
  FErrorHandler := WorkerError;
  {$endif}
end;

procedure TNyxSourceCommands.Notify(AState: TNyxSourceCommandState;
  const AMessage: TNyxText);
begin
  FState := AState;
  FMessage := AMessage;

  if not FDetached and Assigned(FChanged) then
  begin
    FChanged(AState, AMessage);
  end;
end;

procedure TNyxSourceCommands.Apply;
var
  LRequest: TNyxStudioSourceRequest;
  LSchemas: INyxSchemaSnapshot;
begin
  FScheduler.RequireUI;

  if FDetached then
  begin
    raise ENyxModel.Create('This source-command context has retired');
  end;
  LSchemas := CaptureNyxSchemas;
  LRequest := FSession.PrepareSourceRequest(LSchemas.Revision);

  if FSequence = High(Integer) then
  begin
    raise ENyxModel.Create('Source command sequence is exhausted');
  end;
  Inc(FSequence);
  FRequest := LRequest;
  FSchemas := LSchemas;
  FQueued := LRequest.Changed;

  if not LRequest.Changed then
  begin
    FSession.DiscardSourceDraft;
    Notify(nssApplied, 'Accepted Pascal is current');
    Exit;
  end;
  Notify(nssPreparing, 'Preparing Pascal / current design retained');

  if not FRunning then
  begin
    StartQueued;
  end;
end;

procedure TNyxSourceCommands.StartQueued;
{$ifndef PAS2JS}
var
  LWork: TSourcePreparation;
  LLease: INyxWork;
  LIndex: Integer;
  LCount: Integer;
{$endif}
begin

  if FDetached or not FQueued then
  begin
    Exit;
  end;
  FRunning := True;
  FQueued := False;
    FActiveSequence := FSequence;
    FActiveRequest := FRequest;
    FActiveSchemas := FSchemas;
  try
    {$ifdef PAS2JS}
    FWorker := TJSWorker.new(FWorkerURL);
    FWorker.addEventListener('message', FReceiveHandler);
    FWorker.addEventListener('error', FErrorHandler);
    FTimeout := window.setTimeout(@WorkerTimeout, 30000);
    FWorker.postMessage(NyxObject([NyxField('version', NyxData(1)),
      NyxField('source', NyxData(FRequest.Source)),
      NyxField('schemas', FSchemas.ToData)]).ToJSON);
    {$else}
    LCount := 0;
    for LIndex := 0 to High(FExecutions) do
    begin

      if (FExecutions[LIndex] <> nil) and
        (FExecutions[LIndex].Status in [nesPending, nesRunning]) then
      begin
        FExecutions[LCount] := FExecutions[LIndex];
        Inc(LCount);
      end;
    end;
    SetLength(FExecutions, LCount);
    LWork := TSourcePreparation.Create;
    LLease := LWork;
    LWork.Port := FPort;
    LWork.Scheduler := FScheduler;
    LWork.Sequence := FActiveSequence;
    LWork.Request := FRequest;
    LWork.Schemas := FSchemas;
    SetLength(FExecutions, LCount + 1);
    FExecutions[LCount] := FScheduler.Submit(LLease, neThreaded);
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      RetireWorker;
      {$endif}
      FRunning := False;
      Notify(nssFailed, 'Source processor unavailable: ' + LException.Message);
    end;
  end;
end;

procedure TNyxSourceCommands.Finish(ASequence: Integer;
  const ARequest: TNyxStudioSourceRequest; const APrepared: INyxPreparedSource;
  const AFailure: TNyxText);
var
  LCompletion: TNyxSourceCompletion;
  LState: TNyxSourceCommandState;
  LMessage: TNyxText;
begin
  FScheduler.RequireUI;

  if FDetached or (ASequence <> FActiveSequence) then
  begin
    Exit;
  end;
  FRunning := False;

  if ASequence = FSequence then
  begin
    try

      if AFailure <> '' then
      begin
        LState := nssFailed;
        LMessage := AFailure;
      end
      else
      begin
        LCompletion := FSession.CompleteSourceRequest(ARequest, APrepared);
        case LCompletion of
          nscUnchanged, nscApplied:
            begin
              LState := nssApplied;
              LMessage := 'Pascal applied / one Undo restores the pair';
            end;
          nscRejected:
            begin
              LState := nssRejected;
              LMessage := FSession.SourceDiagnostic.Message;
            end;
          nscStale:
            begin
              LState := nssStale;
              LMessage := 'Source result is stale / current design and draft retained';
            end;
        end;
      end;
    except
      on LException: Exception do
      begin
        LState := nssFailed;
        LMessage := LException.Message;
      end;
    end;
    { Presentation failures must not be misreported as failed admission after
      the pair already published. Notifications sit outside that error boundary. }
    Notify(LState, LMessage);
  end;

  if FQueued then
  begin
    StartQueued;
  end;
end;

procedure TNyxSourceCommands.Cancel;
begin
  FScheduler.RequireUI;

  if FSequence = High(Integer) then
  begin
    raise ENyxModel.Create('Source command sequence is exhausted');
  end;
  Inc(FSequence);
  FQueued := False;
  FSchemas := nil;
  {$ifdef PAS2JS}
  RetireWorker;
  FRunning := False;
  {$endif}
  Notify(nssCancelled, 'Source preparation cancelled / current pair retained');
end;

procedure TNyxSourceCommands.Detach;
begin
  FScheduler.RequireUI;
  FDetached := True;
  FChanged := nil;

  if FPort <> nil then
  begin
    FPort.Detach;
  end;
  FQueued := False;
end;

function TNyxSourceCommands.Route(ANode: TNyxNode; ATrigger: TNyxTrigger): Boolean;
begin
  Result := False;

  if (ANode = nil) or (ATrigger <> ntClick) then
  begin
    Exit;
  end;

  if ANode.ID = 'action-apply-source' then
  begin
    Apply;
    Exit(True);
  end;

  if ANode.ID = 'action-reset-source' then
  begin
    Cancel;
    FSession.DiscardSourceDraft;
    Notify(nssIdle, 'Accepted Pascal restored');
    Result := True;
  end;
end;

{$ifdef PAS2JS}
procedure TNyxSourceCommands.RetireWorker;
begin

  if FTimeout >= 0 then
  begin
    window.clearTimeout(FTimeout);
    FTimeout := -1;
  end;

  if FWorker <> nil then
  begin
    FWorker.removeEventListener('message', FReceiveHandler);
    FWorker.removeEventListener('error', FErrorHandler);
    FWorker.terminate;
    FWorker := nil;
  end;
end;

function TNyxSourceCommands.Receive(AEvent: TJSEvent): Boolean;
var
  LDelivery: TSourceDelivery;
  LWork: INyxWork;
begin
  Result := True;
  LDelivery := TSourceDelivery.Create;
  LWork := LDelivery;
  LDelivery.Port := FPort;
  LDelivery.Sequence := FActiveSequence;
  LDelivery.Request := FActiveRequest;
  try

    if not isString(TJSMessageEvent(AEvent).data) then
    begin
      raise ENyxModel.Create('Source worker returned non-text protocol data');
    end;
    LDelivery.Prepared := ReceiveNyxPreparedSource(
      TNyxDataValue.ParseJSON(TNyxText(TJSMessageEvent(AEvent).data)),
      FActiveRequest.Source, FActiveSchemas);
  except
    on LException: Exception do
    begin
      LDelivery.Failure := LException.Message;
    end;
  end;
  RetireWorker;
  FScheduler.PostUI(LWork);
end;

function TNyxSourceCommands.WorkerError(AEvent: TJSEvent): Boolean;
begin
  Result := False;
  AEvent.preventDefault;
  WorkerTimeout;
end;

procedure TNyxSourceCommands.WorkerTimeout;
begin
  RetireWorker;
  Finish(FActiveSequence, FActiveRequest, nil,
    'Source processor failed or timed out / current design and draft retained');
end;
{$endif}

destructor TNyxSourceCommands.Destroy;
{$ifndef PAS2JS}
var
  LIndex: Integer;
  LWaiting: Boolean;
{$endif}
begin

  if FScheduler <> nil then
  begin
    Detach;
    {$ifdef PAS2JS}
    RetireWorker;
    {$else}
    for LIndex := 0 to High(FExecutions) do
    begin

      if FExecutions[LIndex] <> nil then
      begin
        FExecutions[LIndex].Cancel;
      end;
    end;
    {$endif}
    FScheduler.Shutdown;
    {$ifndef PAS2JS}
    repeat
      CheckSynchronize;
      LWaiting := False;
      for LIndex := 0 to High(FExecutions) do
      begin

        if (FExecutions[LIndex] <> nil) and
          (FExecutions[LIndex].Status in [nesPending, nesRunning]) then
        begin
          LWaiting := True;
        end;
      end;

      if LWaiting then
      begin
        Sleep(1);
      end;
    until not LWaiting;
    CheckSynchronize;
    {$endif}
  end;
  FSession := nil;
  FPort := nil;
  FSchemas := nil;
  FScheduler := nil;
  inherited Destroy;
end;

end.
