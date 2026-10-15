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


program nyx_async_flow_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.data, nyx.flow.types, nyx.logic, nyx.flow,
  nyx.flow.execution, nyx.flow.async, nyx.scheduler, nyx.linq, nyx.linq.execution
  {$ifdef PAS2JS}, JS, Web{$else}, Classes{$endif};

type
  TMode = (amInline, amDeferred, amDuplicate, amBadFrame, amFailure, amNil, amRaiseAfterReply);
  TInvocation = class(TInterfacedObject, INyxLogicInvocation, INyxWork)
  public
    Completion: INyxLogicCompletion;
    Scope: INyxCancellationScope;
    Mode: TMode;
    procedure Cancel;
    procedure Execute(const AExecution: INyxExecution);
  end;
  TOperation = class(TNyxLogicOperation, INyxAsyncLogicOperation)
  public
    Mode: TMode;
    Calls: Integer;
    LastCompletion: INyxLogicCompletion;
    constructor Create(AMode: TMode);
    destructor Destroy; override;
    function Execute(const AInputs: TNyxFlowValues;
      const ACancellation: INyxCancellationScope): TNyxFlowValues; override;
    function BeginInvoke(const AInputs: TNyxFlowValues; const AScheduler: INyxScheduler;
      const ACancellation: INyxCancellationScope;
      const ACompletion: INyxLogicCompletion): INyxLogicInvocation;
  end;
  TPort = class(TInterfacedObject, INyxQueryResultPort<INyxFlowResult>)
  public
    Calls: Integer;
    Reject: Boolean;
    Last: INyxFlowResult;
    function Publish(const AValues: TNyxSequenceArray<INyxFlowResult>): Boolean;
  end;

var
  GChecks: Integer;
  GCase: Integer;
  GWaiting: Boolean;
  GDone: Boolean;
  GScheduler: INyxScheduler;
  GRun: INyxQueryRun;
  GPort: TPort;
  GPortLease: INyxQueryResultPort<INyxFlowResult>;
  GOperation: TOperation;
  GOperationLease: INyxLogicOperation;
  GGraph: INyxFlowGraph;
  GScope: INyxCancellationScope;
  GShutdownScheduler: INyxScheduler;
  GDisposed: Integer;
  GDisposedExpected: Integer;
  {$ifndef PAS2JS}
  GMonitor: INyxSchedulerMonitor;
  GDrainUntil: QWord;
  {$endif}
  {$ifdef PAS2JS}GStarted: Double;{$else}GStarted: QWord;{$endif}

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create('Async graph: ' + AReason);
  end;
  Inc(GChecks);
end;

constructor TOperation.Create(AMode: TMode);
begin
  inherited Create;
  Mode := AMode;
  FConfiguration := NyxObject([]);
  FDescriptor := NyxLogicDescriptor(NyxLogicKind('async-fixture'), 'Async fixture',
    'Qualification-only completion protocol.', nlgFlow, nlaUI)
    .Output(NyxLogicPort(nlpText), nftText, 'Owned JSON text');
end;

destructor TOperation.Destroy;
begin
  Inc(GDisposed);
  inherited Destroy;
end;

function TOperation.Execute(const AInputs: TNyxFlowValues;
  const ACancellation: INyxCancellationScope): TNyxFlowValues;
begin
  Result := NyxFlowValues;
  raise ENyxFlow.Create('Async fixture refuses synchronous Invoke');
end;

procedure TInvocation.Cancel;
begin
  Completion := nil;
end;

procedure TInvocation.Execute(const AExecution: INyxExecution);
var
  LPort: INyxLogicCompletion;
begin
  LPort := Completion;
  Completion := nil;

  if LPort = nil then
  begin
    Exit;
  end;
  case Mode of
    amFailure: LPort.Fail('Expected host failure');
    amBadFrame: LPort.Succeed(NyxFlowValues);
    else
    begin
      LPort.Succeed(NyxFlowValues.WithValue(NyxLogicPort(nlpText),
        NyxFlowText('{"message":"Hosted result","exact":9007199254740993}')));

      if Mode = amDuplicate then
      begin
        LPort.Fail('Duplicate completion must be ignored');
      end;
    end;
  end;
end;

function TOperation.BeginInvoke(const AInputs: TNyxFlowValues;
  const AScheduler: INyxScheduler; const ACancellation: INyxCancellationScope;
  const ACompletion: INyxLogicCompletion): INyxLogicInvocation;
var
  LInvocation: TInvocation;
  LWork: INyxWork;
begin
  AScheduler.RequireUI;
  Inc(Calls);
  LastCompletion := ACompletion;
  LInvocation := TInvocation.Create;
  Result := LInvocation;
  LInvocation.Completion := ACompletion;
  LInvocation.Scope := ACancellation;
  LInvocation.Mode := Mode;

  if Mode = amNil then
  begin
    Result := nil;
    Exit;
  end;

  if Mode = amDeferred then
  begin
    LWork := LInvocation;
    AScheduler.PostUI(LWork);
  end
  else
  begin
    LInvocation.Execute(nil);

    if Mode = amRaiseAfterReply then
    begin
      raise ENyxFlow.Create('Host threw after inline completion');
    end;
  end;
end;

function TPort.Publish(const AValues: TNyxSequenceArray<INyxFlowResult>): Boolean;
begin
  Inc(Calls);
  Check(Length(AValues) = 1, 'one complete graph result reaches publication');
  Last := AValues[0];
  Result := not Reject;
end;

procedure Setup(AMode: TMode);
begin
  GOperation := TOperation.Create(AMode);
  GOperationLease := GOperation;
  GGraph := NewNyxFlowGraph.WithStep(NyxFlowStep('host'), GOperationLease)
    .WithStep(NyxFlowStep('parse'), NewNyxJSONParse)
    .Connect(NyxFlowEndpoint(NyxFlowStep('host'), NyxLogicPort(nlpText)),
      NyxFlowEndpoint(NyxFlowStep('parse'), NyxLogicPort(nlpText))).Build;
  GPort := TPort.Create;
  GPortLease := GPort;
  GScope := NewNyxCancellationScope;
end;

procedure Step;
var
  LFailed: Boolean;
  LCapability: INyxResumableFlowGraph;
  LCursor: INyxFlowContinuation;
  LResult: INyxFlowResult;
  LFrame: TNyxFlowValues;
begin

  if GCase = 11 then
  begin

    if GDisposed < GDisposedExpected then
    begin
      Exit;
    end;
    Check(GDisposed = GDisposedExpected, 'shutdown without polling/drop retires the pending graph owner');
    GShutdownScheduler := nil;
    GDone := True;
    Exit;
  end;

  if not GWaiting then
  begin
    case GCase of
      0:
      begin
        Setup(amInline);
        LFailed := False;
        try
          GGraph.Evaluate([]);
        except
          on E: ENyxFlow do
          begin
            LFailed := True;
          end;
        end;
        Check(LFailed and (GOperation.Calls = 0), 'direct evaluation refuses before host entry');
        Check(Supports(GGraph, INyxResumableFlowGraph, LCapability), 'optional continuation capability');
        LCursor := LCapability.BeginEvaluation([], nil);
        LFailed := False;
        try
          LResult := LCursor.Result;
        except
          on E: ENyxFlow do
          begin
            LFailed := True;
          end;
        end;
        Check(LFailed, 'incomplete continuation exposes no successful prefix');
        LFrame := LCursor.Inputs;
        Check(LFrame.Count = 0, 'host input frame is exact');
        LFailed := False;
        try
          LCursor.Accept(NyxFlowValues);
        except
          on E: ENyxFlow do
          begin
            LFailed := True;
          end;
        end;
        Check(LFailed and (LCursor.Step.Name = 'host'), 'bad frame leaves the pending step unchanged');
      end;
      1: Setup(amDeferred);
      2: Setup(amDuplicate);
      3: Setup(amBadFrame);
      4: Setup(amFailure);
      5: Setup(amNil);
      6: Setup(amRaiseAfterReply);
      7: Setup(amDeferred);
      8: Setup(amInline);
      9:
      begin
        Setup(amInline);
        GPort.Reject := True;
      end;
      10:
      begin
        Setup(amDeferred);
        LFailed := False;
        try
          TNyxFlowExecution.Submit(GGraph, [], GScheduler, GPortLease, neThreaded);
        except
          on E: Exception do
          begin
            LFailed := True;
          end;
        end;
        Check(LFailed and (GOperation.Calls = 0), 'threaded host graph refuses before entry');
        GDisposedExpected := GDisposed + 1;
        GShutdownScheduler := NewNyxScheduler;
        GRun := TNyxFlowExecution.Submit(GGraph, [], GShutdownScheduler, GPortLease, neUIQueue);
        GShutdownScheduler.Shutdown;
        GRun := nil;
        GGraph := nil;
        GOperationLease := nil;
        GPortLease := nil;
        GCase := 11;
        Exit;
      end;
    end;
    GRun := TNyxFlowExecution.Submit(GGraph, [], GScheduler, GPortLease, neAsynchronous, GScope);
    GWaiting := True;

    if GCase = 7 then
    begin
      GRun.Cancel;
    end;

    if GCase = 8 then
    begin
      GScope.Cancel;
    end;
    Exit;
  end;

  if GRun.Phase in [nqpPending, nqpComputing, nqpAwaitingUI] then
  begin
    Exit;
  end;
  case GCase of
    0, 1, 2:
    begin
      Check((GRun.Phase = nqpSucceeded) and (GPort.Calls = 1), 'successful async graph publishes once');
      Check(GPort.Last.Value(NyxFlowEndpoint(NyxFlowStep('parse'), NyxLogicPort(nlpJSON)))
        .AsJSON.Field('exact').ToJSON = '9007199254740993', 'resumed pure step retains exact JSON');
    end;
    3, 4, 5, 6:
    begin
      Check((GRun.Phase = nqpFailed) and (GPort.Calls = 0), 'invalid/failed host cannot publish a prefix');
      Check(Pos('host', GRun.Failure) > 0, 'failure identifies its exact step');
    end;
    7, 8:
    begin
      Check((GRun.Phase = nqpCancelled) and (GPort.Calls = 0), 'cancellation prevents host publication');

      if GOperation.LastCompletion <> nil then
      begin
        GOperation.LastCompletion.Succeed(NyxFlowValues);
        Check(GPort.Calls = 0, 'late host completion cannot resurrect a cancelled run');
      end;
    end;
    9: Check((GRun.Phase = nqpDiscarded) and (GPort.Calls = 1), 'revoked result authority discards the complete result');
  end;
  GRun := nil;
  GGraph := nil;
  GOperationLease := nil;
  GPortLease := nil;
  GWaiting := False;
  Inc(GCase);
end;

{$ifdef PAS2JS}
procedure Drive;
begin
  try
    Step;

    if GDone then
    begin
      GScheduler.Shutdown;
      document.body.setAttribute('data-async-flow', 'passed');
      document.body.setAttribute('data-async-flow-checks', IntToStr(GChecks));
      Exit;
    end;

    if TJSDate.now - GStarted > 30000 then
    begin
      raise Exception.Create('Async fixture deadline at case ' + IntToStr(GCase));
    end;
    window.setTimeout(@Drive, 5);
  except
    on E: Exception do
    begin
      document.body.setAttribute('data-async-flow', 'failed');
      document.body.setAttribute('data-async-flow-error', E.Message);
    end;
  end;
end;
{$endif}

begin
  try
    GScheduler := NewNyxScheduler;
    {$ifdef PAS2JS}
    GStarted := TJSDate.now;
    Drive;
    {$else}
    GStarted := GetTickCount64;
    while not GDone do
    begin
      CheckSynchronize;
      Step;

      if GetTickCount64 - GStarted > 30000 then
      begin
        raise Exception.Create('Async fixture deadline at case ' + IntToStr(GCase));
      end;
      Sleep(1);
    end;
    GScheduler.Shutdown;
    GMonitor := GScheduler as INyxSchedulerMonitor;
    GDrainUntil := GetTickCount64 + 100;
    { Shutdown never joins product work. The checked console fixture services
      cancelled UI couriers and waits for its own worker pool before RTL/heap
      finalization, which is not safe while free-on-terminate workers remain. }
    while (GetTickCount64 < GDrainUntil) or (GMonitor.WorkerLoad.ActiveWorkers > 0) do
    begin
      CheckSynchronize;
      Sleep(1);

      if GetTickCount64 > GDrainUntil + 5000 then
      begin
        raise Exception.Create('Owned async workers did not retire');
      end;
    end;
    Check(GMonitor.WorkerLoad.ActiveWorkers = 0, 'owned native workers retire before heap finalization');
    GMonitor := nil;
    GRun := nil;
    GGraph := nil;
    GOperationLease := nil;
    GPortLease := nil;
    GScope := nil;
    GScheduler := nil;
    WriteLn('PASS ', GChecks, ' asynchronous graph checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-async-flow', 'failed');
      document.body.setAttribute('data-async-flow-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
