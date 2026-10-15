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


unit nyx.flow.async;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.logic, nyx.flow, nyx.flow.types, nyx.scheduler,
  nyx.linq, nyx.linq.execution;

{ UI-owned continuation coordinator. Host steps may finish inline or later;
  successful frames always resume on a later scheduler turn. Pure steps use
  native workers under Asynchronous; Sequential/UIQueue use the UI queue.
  Threaded refuses graphs containing asynchronous UI host operations. A run
  retains its own pending lifetime until completion/cancellation; its revocable
  ports never retain an application, renderer or borrowed receiver. }
function SubmitNyxAsyncFlow(const AGraph: INyxResumableFlowGraph;
  const AInputs: array of TNyxFlowInput; const AScheduler: INyxScheduler;
  const APort: INyxQueryResultPort<INyxFlowResult>; APolicy: TNyxExecutionPolicy;
  const AGeneration: INyxCancellationScope): INyxQueryRun;

implementation

type
  TCoordinator = class;
  { A real delayed UI turn observes generation/scheduler retirement without
    requiring callers to poll. A skipped timer releases its pending run by
    cancellation; it cannot strand a self lease after scheduler Shutdown. }
  TWatch = class(TInterfacedObject, INyxWork)
  public
    Owner: TCoordinator;
    Lease: INyxQueryRun;
    Entered: Boolean;
    destructor Destroy; override;
    procedure Execute(const AExecution: INyxExecution);
  end;
  TScope = class(TInterfacedObject, INyxCancellationScope)
  public
    Local: INyxCancellationScope;
    Generation: INyxCancellationScope;
    procedure Cancel;
    function GetCancelled: Boolean;
  end;
  TAdvance = class(TInterfacedObject, INyxWork)
  public
    Scheduler: INyxScheduler;
    Owner: TCoordinator;
    procedure Execute(const AExecution: INyxExecution);
  end;
  TCompletion = class(TInterfacedObject, INyxLogicCompletion)
  public
    Scheduler: INyxScheduler;
    Owner: TCoordinator;
    procedure Succeed(const AOutputs: TNyxFlowValues);
    procedure Fail(const AReason: TNyxText);
  end;
  TDelivery = class(TInterfacedObject, INyxWork)
  public
    Completion: INyxLogicCompletion;
    Outputs: TNyxFlowValues;
    Failure: TNyxText;
    procedure Execute(const AExecution: INyxExecution);
  end;
  TCompute = class(TInterfacedObject, INyxWork)
  public
    Operation: INyxLogicOperation;
    Inputs: TNyxFlowValues;
    Scope: INyxCancellationScope;
    Scheduler: INyxScheduler;
    Completion: INyxLogicCompletion;
    procedure Execute(const AExecution: INyxExecution);
  end;
  TCoordinator = class(TInterfacedObject, INyxQueryRun)
  private
    FScheduler: INyxScheduler;
    FResultPort: INyxQueryResultPort<INyxFlowResult>;
    FScope: INyxCancellationScope;
    FCursor: INyxFlowContinuation;
    FLease: INyxQueryRun;
    FAdvance: TAdvance;
    FAdvancePort: INyxWork;
    FCompletion: TCompletion;
    FCompletionPort: INyxLogicCompletion;
    FInvocation: INyxLogicInvocation;
    FCompute: INyxExecution;
    FWatch: INyxExecution;
    FPhase: TNyxQueryPhase;
    FFailure: TNyxText;
    FStep: TNyxFlowStepRef;
    FStepLabel: TNyxText;
    FOutputs: TNyxFlowValues;
    FStepFailure: TNyxText;
    FReady: Boolean;
    FWaiting: Boolean;
    FQueued: Boolean;
    FPolicy: TNyxExecutionPolicy;
    function Terminal: Boolean;
    procedure Finish(APhase: TNyxQueryPhase; const AFailure: TNyxText);
    procedure Queue;
    procedure Watch;
    procedure Receive(const AOutputs: TNyxFlowValues; const AFailure: TNyxText);
  public
    constructor Create(const AGraph: INyxResumableFlowGraph;
      const AInputs: array of TNyxFlowInput; const AScheduler: INyxScheduler;
      const APort: INyxQueryResultPort<INyxFlowResult>; APolicy: TNyxExecutionPolicy;
      const AGeneration: INyxCancellationScope);
    destructor Destroy; override;
    procedure Start;
    procedure Pump;
    procedure Cancel;
    function GetPhase: TNyxQueryPhase;
    function GetFailure: TNyxText;
  end;

procedure TScope.Cancel;
begin
  Local.Cancel;
end;

destructor TWatch.Destroy;
begin

  if not Entered and (Lease <> nil) then
  begin
    Lease.Cancel;
  end;
  inherited Destroy;
end;

procedure TWatch.Execute(const AExecution: INyxExecution);
begin
  Entered := True;

  if Lease.Phase in [nqpPending, nqpComputing, nqpAwaitingUI] then
  begin
    try
      Owner.Watch;
    except
      Lease.Cancel;
      raise;
    end;
  end;
end;

function TScope.GetCancelled: Boolean;
begin
  Result := Local.Cancelled or ((Generation <> nil) and Generation.Cancelled);
end;

procedure TAdvance.Execute(const AExecution: INyxExecution);
var
  LLease: INyxQueryRun;
  LOwner: TCoordinator;
begin
  Scheduler.RequireUI;
  LOwner := Owner;

  if LOwner = nil then
  begin
    Exit;
  end;
  LLease := LOwner;
  LOwner.Pump;
end;

procedure TCompletion.Succeed(const AOutputs: TNyxFlowValues);
var
  LLease: INyxQueryRun;
  LOwner: TCoordinator;
begin
  Scheduler.RequireUI;
  LOwner := Owner;
  Owner := nil;

  if LOwner <> nil then
  begin
    LLease := LOwner;
    LOwner.Receive(AOutputs, '');
  end;
end;

procedure TCompletion.Fail(const AReason: TNyxText);
var
  LLease: INyxQueryRun;
  LOwner: TCoordinator;
  LReason: TNyxText;
begin
  Scheduler.RequireUI;
  LReason := AReason;

  if LReason = '' then
  begin
    LReason := 'Asynchronous Logic operation failed';
  end;
  LOwner := Owner;
  Owner := nil;

  if LOwner <> nil then
  begin
    LLease := LOwner;
    LOwner.Receive(NyxFlowValues, LReason);
  end;
end;

procedure TDelivery.Execute(const AExecution: INyxExecution);
begin

  if Failure <> '' then
  begin
    Completion.Fail(Failure);
  end
  else
  begin
    Completion.Succeed(Outputs);
  end;
end;

procedure TCompute.Execute(const AExecution: INyxExecution);
var
  LDelivery: TDelivery;
  LWork: INyxWork;
begin
  LDelivery := TDelivery.Create;
  LWork := LDelivery;
  LDelivery.Completion := Completion;
  try

    if not Scope.Cancelled then
    begin
      LDelivery.Outputs := Operation.Invoke(Inputs, Scope);
    end;
  except
    on E: ENyxFlowCancelled do
    begin
      Scope.Cancel;
    end;
    on E: Exception do
    begin
      LDelivery.Failure := E.Message;
    end;
  end;
  { The worker owns only immutable inputs and a weak completion capability.
    Publication/continuation always returns to UI, including cancellation. }
  Scheduler.PostUI(LWork);
end;

constructor TCoordinator.Create(const AGraph: INyxResumableFlowGraph;
  const AInputs: array of TNyxFlowInput; const AScheduler: INyxScheduler;
  const APort: INyxQueryResultPort<INyxFlowResult>; APolicy: TNyxExecutionPolicy;
  const AGeneration: INyxCancellationScope);
var
  LScope: TScope;
begin
  inherited Create;
  FScheduler := AScheduler;
  FResultPort := APort;
  FPolicy := APolicy;

  if (AGraph = nil) or (FScheduler = nil) or (FResultPort = nil) then
  begin
    raise ENyxFlow.Create('Asynchronous execution requires graph, scheduler and result authority');
  end;
  FScheduler.RequireUI;

  if FPolicy = neThreaded then
  begin
    raise ENyxFlow.Create('Asynchronous host operations require UI-owned graph coordination');
  end;
  LScope := TScope.Create;
  FScope := LScope;
  LScope.Local := NewNyxCancellationScope;
  LScope.Generation := AGeneration;
  FCursor := AGraph.BeginEvaluation(AInputs, FScope);
  FAdvance := TAdvance.Create;
  FAdvancePort := FAdvance;
  FAdvance.Scheduler := FScheduler;
  FAdvance.Owner := Self;
  FPhase := nqpPending;
end;

destructor TCoordinator.Destroy;
begin

  if FAdvance <> nil then
  begin
    FAdvance.Owner := nil;
  end;

  if FCompletion <> nil then
  begin
    FCompletion.Owner := nil;
  end;
  inherited Destroy;
end;

function TCoordinator.Terminal: Boolean;
begin
  Result := FPhase in [nqpSucceeded, nqpDiscarded, nqpCancelled, nqpFailed];
end;

procedure TCoordinator.Finish(APhase: TNyxQueryPhase; const AFailure: TNyxText);
var
  LInvocation: INyxLogicInvocation;
begin

  if Terminal then
  begin
    Exit;
  end;
  FPhase := APhase;
  FFailure := AFailure;
  FWaiting := False;
  FQueued := False;

  if FAdvance <> nil then
  begin
    FAdvance.Owner := nil;
  end;

  if FCompletion <> nil then
  begin
    FCompletion.Owner := nil;
  end;
  FCompletion := nil;
  FCompletionPort := nil;
  LInvocation := FInvocation;
  FInvocation := nil;
  try

    if FScope <> nil then
    begin
      FScope.Cancel;
    end;

    if FCompute <> nil then
    begin
      FCompute.Cancel;
      FCompute := nil;
    end;

    if FWatch <> nil then
    begin
      FWatch.Cancel;
      FWatch := nil;
    end;

    if LInvocation <> nil then
    begin
      LInvocation.Cancel;
    end;
  finally
    FCursor := nil;
    FResultPort := nil;
    FOutputs := NyxFlowValues;
    FLease := nil;
  end;
end;

procedure TCoordinator.Queue;
begin

  if not FQueued and not Terminal then
  begin
    FQueued := True;
    FScheduler.PostUI(FAdvancePort);
  end;
end;

procedure TCoordinator.Start;
begin
  FLease := Self;
  try
    Watch;
    Queue;
  except
    Finish(nqpCancelled, '');
    raise;
  end;
end;

procedure TCoordinator.Watch;
var
  LWatch: TWatch;
  LWork: INyxWork;
begin
  LWatch := TWatch.Create;
  LWork := LWatch;
  LWatch.Owner := Self;
  LWatch.Lease := Self;
  FWatch := NyxSchedulerDelays(FScheduler).PostAfter(LWork, 25);
end;

procedure TCoordinator.Receive(const AOutputs: TNyxFlowValues; const AFailure: TNyxText);
begin
  FScheduler.RequireUI;

  if Terminal or not FWaiting then
  begin
    Exit;
  end;
  FWaiting := False;
  FOutputs := AOutputs;
  FStepFailure := AFailure;
  FReady := True;
  Queue;
end;

procedure TCoordinator.Pump;
var
  LAsync: INyxAsyncLogicOperation;
  LOperation: INyxLogicOperation;
  LInputs: TNyxFlowValues;
  LInvocation: INyxLogicInvocation;
  LCompletion: INyxLogicCompletion;
  LResult: INyxFlowResult;
  LValues: TNyxSequenceArray<INyxFlowResult>;
  LCompute: TCompute;
  LWork: INyxWork;
  LPolicy: TNyxExecutionPolicy;
  LResultPort: INyxQueryResultPort<INyxFlowResult>;
begin
  FScheduler.RequireUI;
  FQueued := False;

  if Terminal then
  begin
    Exit;
  end;
  try

    if FScope.Cancelled then
    begin
      Finish(nqpCancelled, '');
      Exit;
    end;
    FPhase := nqpComputing;

    if FReady then
    begin
      FReady := False;

      if FStepFailure <> '' then
      begin
        raise ENyxFlow.Create(FStepFailure);
      end;
      FCursor.Accept(FOutputs);
      FOutputs := NyxFlowValues;
      FCompletion := nil;
      FCompletionPort := nil;
      FInvocation := nil;
      FCompute := nil;
    end;

    if FCursor.Completed then
    begin
      LResult := FCursor.Result;
      SetLength(LValues, 1);
      LValues[0] := LResult;
      FPhase := nqpAwaitingUI;
      LResultPort := FResultPort;

      if LResultPort.Publish(LValues) then
      begin
        Finish(nqpSucceeded, '');
      end
      else
      begin
        Finish(nqpDiscarded, '');
      end;
      Exit;
    end;
    FStep := FCursor.Step;
    FStepLabel := FStep.Name;
    LOperation := FCursor.Operation;
    LInputs := FCursor.Inputs;
    FCompletion := TCompletion.Create;
    FCompletionPort := FCompletion;
    FCompletion.Scheduler := FScheduler;
    FCompletion.Owner := Self;
    LCompletion := FCompletionPort;
    FWaiting := True;

    if Supports(LOperation, INyxAsyncLogicOperation, LAsync) then
    begin
      LInvocation := LAsync.BeginInvoke(LInputs, FScheduler, FScope, LCompletion);

      if not Terminal and FWaiting then
      begin

        if LInvocation = nil then
        begin
          raise ENyxFlow.Create('Pending asynchronous Logic requires a cancellation token');
        end;
        FInvocation := LInvocation;
      end
      else if LInvocation <> nil then
      begin
        { Inline completion or reentrant owner retirement wins over a token
          returned afterward. It must never resurrect the completed operation. }
        LInvocation.Cancel;
      end;
    end
    else
    begin
      LCompute := TCompute.Create;
      LWork := LCompute;
      LCompute.Operation := LOperation;
      LCompute.Inputs := LInputs;
      LCompute.Scope := FScope;
      LCompute.Scheduler := FScheduler;
      LCompute.Completion := LCompletion;
      LPolicy := neUIQueue;

      if (FPolicy = neAsynchronous) and (FCursor.Descriptor.Affinity = nlaPure) then
      begin
        LPolicy := neAsynchronous;
      end;
      FCompute := FScheduler.Submit(LWork, LPolicy);
    end;
  except
    on E: ENyxFlowCancelled do
    begin
      Finish(nqpCancelled, '');
    end;
    on E: Exception do
    begin
      Finish(nqpFailed, 'Logic step ' + FStepLabel + ': ' + TNyxText(E.Message));
    end;
  end;
end;

procedure TCoordinator.Cancel;
var
  LLease: INyxQueryRun;
begin
  FScheduler.RequireUI;
  LLease := Self;
  Finish(nqpCancelled, '');
end;

function TCoordinator.GetPhase: TNyxQueryPhase;
var
  LLease: INyxQueryRun;
begin
  FScheduler.RequireUI;
  LLease := Self;

  if not Terminal and FScope.Cancelled then
  begin
    Finish(nqpCancelled, '');
  end;

  if not Terminal and (FCompute <> nil) and (FCompute.Status = nesFailed) then
  begin
    Finish(nqpFailed, FCompute.Failure);
  end;

  if not Terminal and (FCompute <> nil) and (FCompute.Status = nesCancelled) then
  begin
    Finish(nqpCancelled, '');
  end;
  Result := FPhase;
end;

function TCoordinator.GetFailure: TNyxText;
begin
  GetPhase;
  Result := FFailure;
end;

function SubmitNyxAsyncFlow(const AGraph: INyxResumableFlowGraph;
  const AInputs: array of TNyxFlowInput; const AScheduler: INyxScheduler;
  const APort: INyxQueryResultPort<INyxFlowResult>; APolicy: TNyxExecutionPolicy;
  const AGeneration: INyxCancellationScope): INyxQueryRun;
var
  LRun: TCoordinator;
begin
  LRun := TCoordinator.Create(AGraph, AInputs, AScheduler, APort, APolicy, AGeneration);
  Result := LRun;
  LRun.Start;
end;

end.
