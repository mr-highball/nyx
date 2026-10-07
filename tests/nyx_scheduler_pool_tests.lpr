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
program nyx_scheduler_pool_tests;

{$mode delphi}{$H+}
{$codepage utf8}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  SysUtils,
  Classes,
  SyncObjs,
  nyx.text,
  nyx.types,
  nyx.data,
  nyx.behavior,
  nyx.events,
  nyx.scheduler;

type
  { Work owns a lease to this synchronized probe, not a borrowed host object.
    Manual gates establish concurrency/capacity without interpreting elapsed
    throughput as proof of an execution policy. All shared observations lock. }
  IPoolProbe = interface(IInterface)
    ['{739BC309-7893-48E3-9600-001001000006}']
    procedure Run(AID: Integer; const AExecution: INyxExecution);
  end;

  TPoolProbe = class(TInterfacedObject, IPoolProbe)
  private
    FLock: TRTLCriticalSection;
    FGate: TEvent;
    FCalls: array[0..255] of Integer;
    FThreadIDs: array[0..255] of TThreadID;
    FOrder: array of Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Run(AID: Integer; const AExecution: INyxExecution);
    procedure Hold;
    procedure Release;
    function Calls(AID: Integer): Integer;
    function ThreadID(AID: Integer): TThreadID;
    function Order(AIndex: Integer): Integer;
  end;

  TPoolWork = class(TInterfacedObject, INyxWork)
  private
    FProbe: IPoolProbe;
    FID: Integer;
  public
    constructor Create(const AProbe: IPoolProbe; AID: Integer);
    procedure Execute(const AExecution: INyxExecution);
  end;

  { The ordinary event router supplies this callback with its owned event lease.
    Pool saturation must remain visible per registration without skipping an
    independent sequential sibling or invoking refused work on the UI. }
  TPoolCallback = class(TNyxEventCallback)
  private
    FProbe: IPoolProbe;
    FID: Integer;
  public
    constructor Create(const AProbe: IPoolProbe; AID: Integer);
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution); override;
  end;

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AMessage: String);
begin

  if not ACondition then
  begin
    raise Exception.Create('FAIL pool: ' + AMessage);
  end;
  Inc(GChecks);
end;

constructor TPoolProbe.Create;
begin
  inherited Create;
  InitCriticalSection(FLock);
  FGate := TEvent.Create(nil, True, False, '');
end;

destructor TPoolProbe.Destroy;
begin
  FGate.Free;
  DoneCriticalSection(FLock);
  inherited Destroy;
end;

procedure TPoolProbe.Run(AID: Integer; const AExecution: INyxExecution);
begin
  EnterCriticalSection(FLock);
  try
    Inc(FCalls[AID]);
    FThreadIDs[AID] := GetCurrentThreadID;
    SetLength(FOrder, Length(FOrder) + 1);
    FOrder[High(FOrder)] := AID;
  finally
    LeaveCriticalSection(FLock);
  end;

  if AID in [0, 1, 90, 91] then
  begin

    if FGate.WaitFor(10000) <> wrSignaled then
    begin
      raise Exception.Create('Fixture gate was never released');
    end;
  end;

  if AID = 40 then
  begin
    raise Exception.Create('Expected callback failure');
  end;
end;

procedure TPoolProbe.Hold;
begin
  FGate.ResetEvent;
end;

procedure TPoolProbe.Release;
begin
  FGate.SetEvent;
end;

function TPoolProbe.Calls(AID: Integer): Integer;
begin
  EnterCriticalSection(FLock);
  try
    Result := FCalls[AID];
  finally
    LeaveCriticalSection(FLock);
  end;
end;

function TPoolProbe.ThreadID(AID: Integer): TThreadID;
begin
  EnterCriticalSection(FLock);
  try
    Result := FThreadIDs[AID];
  finally
    LeaveCriticalSection(FLock);
  end;
end;

function TPoolProbe.Order(AIndex: Integer): Integer;
begin
  EnterCriticalSection(FLock);
  try
    Result := FOrder[AIndex];
  finally
    LeaveCriticalSection(FLock);
  end;
end;

constructor TPoolWork.Create(const AProbe: IPoolProbe; AID: Integer);
begin
  inherited Create;
  FProbe := AProbe;
  FID := AID;
end;

procedure TPoolWork.Execute(const AExecution: INyxExecution);
begin
  FProbe.Run(FID, AExecution);
end;

constructor TPoolCallback.Create(const AProbe: IPoolProbe; AID: Integer);
begin
  inherited Create;
  FProbe := AProbe;
  FID := AID;
end;

procedure TPoolCallback.Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution);
begin
  FProbe.Run(FID, AExecution);
end;

procedure AwaitEntry(AProbe: TPoolProbe; AID: Integer);
var
  LUntil: QWord;
begin
  LUntil := GetTickCount64 + 5000;
  while (AProbe.Calls(AID) = 0) and (GetTickCount64 < LUntil) do
  begin
    CheckSynchronize(0);
    Sleep(1);
  end;
  Check(AProbe.Calls(AID) = 1, 'worker entered held callback exactly once');
end;

procedure AwaitTerminal(const AExecution: INyxExecution);
var
  LUntil: QWord;
begin
  LUntil := GetTickCount64 + 5000;
  while (AExecution.Status in [nesPending, nesRunning]) and
    (GetTickCount64 < LUntil) do
  begin
    CheckSynchronize(0);
    Sleep(1);
  end;
  Check(not (AExecution.Status in [nesPending, nesRunning]),
    'execution reached a terminal state');
end;

procedure AwaitPoolIdle(const AMonitor: INyxSchedulerMonitor);
var
  LUntil: QWord;
  LLoad: TNyxWorkerPoolSnapshot;
begin
  LUntil := GetTickCount64 + 5000;
  repeat
    LLoad := AMonitor.WorkerLoad;

    if (LLoad.Running = 0) and (LLoad.Pending = 0) then
    begin
      Break;
    end;
    CheckSynchronize(0);
    Sleep(1);
  until GetTickCount64 >= LUntil;
  Check((LLoad.Running = 0) and (LLoad.Pending = 0), 'pool returned to idle');
end;

procedure Run;
var
  LOptions: TNyxSchedulerOptions;
  LConfigured: TNyxSchedulerOptions;
  LScheduler: INyxScheduler;
  LOther: INyxScheduler;
  LMonitor: INyxSchedulerMonitor;
  LOtherMonitor: INyxSchedulerMonitor;
  LProbe: TPoolProbe;
  LOtherProbe: TPoolProbe;
  LProbeLease: IPoolProbe;
  LOtherLease: IPoolProbe;
  LTickets: array[0..6] of INyxExecution;
  LOtherTickets: array[0..3] of INyxExecution;
  LFinal: array[0..2] of INyxExecution;
  LTicket: INyxExecution;
  LRejectedWork: INyxWork;
  LLoad: TNyxWorkerPoolSnapshot;
  LRouter: INyxEvents;
  LFirstRegistration: INyxEventSubscription;
  LSecondRegistration: INyxEventSubscription;
  LUIRegistration: INyxEventSubscription;
  LInvocations: TNyxExecutions;
  LEvent: TNyxEventInfo;
  LFirstThread: TThreadID;
  LSecondThread: TThreadID;
  LIndex: Integer;
  LRefused: Boolean;
  {$IFDEF MSWINDOWS}
  LThreads: array[0..2] of THandle;
  {$ENDIF}
begin
  LOptions := TNyxSchedulerOptions.Defaults;
  LConfigured := LOptions.Workers(2).PendingCapacity(3);
  Check((LOptions.WorkerLimit = 4) and (LOptions.PendingLimit = 1024),
    'fluent configuration preserves its source defaults');
  Check((LConfigured.WorkerLimit = 2) and (LConfigured.PendingLimit = 3),
    'typed configuration copied independently');
  for LIndex := 0 to 4 do
  begin
    LRefused := False;
    try
      case LIndex of
        0:
          begin
            LConfigured := LOptions.Workers(0);
          end;
        1:
          begin
            LConfigured := LOptions.Workers(65);
          end;
        2:
          begin
            LConfigured := LOptions.PendingCapacity(0);
          end;
        3:
          begin
            LConfigured := LOptions.PendingCapacity(65537);
          end;
        4:
          begin
            LScheduler := NewNyxScheduler(Default(TNyxSchedulerOptions));
          end;
      end;
    except
      on ENyxSchedule do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'invalid explicit configuration refuses');
  end;
  LTicket := NewNyxFailedExecution('Capacity / 🌙');
  Check((LTicket.Status = nesFailed) and (LTicket.Failure = 'Capacity / 🌙'),
    'admission failure owns its exact supplementary Unicode diagnostic');
  LTicket.Cancel;
  Check((LTicket.Status = nesFailed) and not LTicket.Cancelled,
    'terminal failure is not rewritten as cooperative cancellation');
  LRefused := False;
  try
    LTicket := NewNyxFailedExecution('');
  except
    on ENyxSchedule do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'empty admission failure diagnostic refuses');
  LConfigured := LOptions.Workers(2).PendingCapacity(3);
  LProbe := TPoolProbe.Create;
  LProbeLease := LProbe;
  LOtherProbe := TPoolProbe.Create;
  LOtherLease := LOtherProbe;
  {$IFDEF MSWINDOWS}
  FillChar(LThreads, SizeOf(LThreads), 0);
  {$ENDIF}
  try
    LScheduler := NewNyxScheduler(LConfigured);
    Check(Supports(LScheduler, INyxSchedulerMonitor, LMonitor), 'queryable native load');
    LLoad := LMonitor.WorkerLoad;
    Check((LLoad.ActiveWorkers = 0) and (LLoad.Pending = 0) and
      (LLoad.WorkerLimit = 2) and (LLoad.PendingLimit = 3) and not LLoad.Closed,
      'workers start lazily at the configured limits');
    LTickets[0] := LScheduler.Submit(TPoolWork.Create(LProbeLease, 0), neThreaded);
    AwaitEntry(LProbe, 0);
    LTickets[1] := LScheduler.Submit(TPoolWork.Create(LProbeLease, 1), neAsynchronous);
    AwaitEntry(LProbe, 1);
    LFirstThread := LProbe.ThreadID(0);
    LSecondThread := LProbe.ThreadID(1);
    Check((LFirstThread <> MainThreadID) and (LSecondThread <> MainThreadID) and
      (LFirstThread <> LSecondThread), 'two real independent workers, no UI fallback');
    LLoad := LMonitor.WorkerLoad;
    Check((LLoad.ActiveWorkers = 2) and (LLoad.Running = 2) and (LLoad.Pending = 0),
      'running work does not occupy pending capacity');
    {$IFDEF MSWINDOWS}
    LThreads[0] := OpenThread(SYNCHRONIZE, False, LFirstThread);
    LThreads[1] := OpenThread(SYNCHRONIZE, False, LSecondThread);
    Check((LThreads[0] <> 0) and (LThreads[1] <> 0), 'retain actual worker termination handles');
    {$ENDIF}
    for LIndex := 2 to 4 do
    begin
      LTickets[LIndex] := LScheduler.Submit(TPoolWork.Create(LProbeLease, LIndex), neThreaded);
    end;
    Check((LMonitor.WorkerLoad.Pending = 3) and (LProbe.Calls(2) = 0),
      'held workers establish exact queue saturation');
    LRejectedWork := TPoolWork.Create(LProbeLease, 5);
    LRefused := False;
    try
      LTicket := LScheduler.Submit(LRejectedWork, neAsynchronous);
    except
      on ENyxScheduleCapacity do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LMonitor.WorkerLoad.Pending = 3) and (LProbe.Calls(5) = 0),
      'saturation refuses without executing or adopting a hidden job');
    LRejectedWork := nil;
    LRouter := NewNyxEvents(LScheduler);
    LFirstRegistration := LRouter.OnAfterEnter(NyxControlEvents('input')).Policy(neThreaded)
      .Subscribe(TPoolCallback.Create(LProbeLease, 80));
    LSecondRegistration := LRouter.OnAfterEnter(NyxControlEvents('input'))
      .Subscribe(TPoolCallback.Create(LProbeLease, 81));
    LUIRegistration := LRouter.OnAfterEnter(NyxCompoundEvents('compound')).Policy(neSequential)
      .Subscribe(TPoolCallback.Create(LProbeLease, 82));
    LEvent := Default(TNyxEventInfo);
    LEvent.Trigger := ntAfterEnter;
    LEvent.Name := NyxEvent('entered');
    LEvent.Value := NyxData(TNyxText(''));
    LEvent.OriginID := 'input';
    LEvent.SourceID := 'compound';
    LInvocations := LRouter.Dispatch(LEvent, 'input', 'compound');
    Check((Length(LInvocations) = 3) and (LInvocations[0].Status = nesFailed) and
      (LInvocations[1].Status = nesFailed) and (LInvocations[2].Status = nesSucceeded),
      'event overload is per invocation and independent UI siblings still execute');
    Check((LFirstRegistration.LastExecution = LInvocations[0]) and
      (LSecondRegistration.LastExecution = LInvocations[1]) and
      (LUIRegistration.LastExecution = LInvocations[2]) and
      (Pos('capacity', LInvocations[0].Failure) > 0), 'registrations retain overload diagnostics');
    Check((LProbe.Calls(80) = 0) and (LProbe.Calls(81) = 0) and
      (LProbe.Calls(82) = 1) and (LProbe.ThreadID(82) = MainThreadID),
      'event saturation never silently executes worker callbacks inline');
    LRouter.Close;
    LRouter := nil;
    LTickets[3].Cancel;
    LTickets[6] := LScheduler.Submit(TPoolWork.Create(LProbeLease, 6), neThreaded);
    Check((LTickets[3].Status = nesCancelled) and (LMonitor.WorkerLoad.Pending = 3),
      'cancelled pending work releases capacity before the next admission');

    { A second pool runs while both first-pool workers remain held. Its single
      worker additionally proves FIFO dequeue without concurrency ambiguity. }
    LOther := NewNyxScheduler(LOptions.Workers(1).PendingCapacity(3));
    Check(Supports(LOther, INyxSchedulerMonitor, LOtherMonitor), 'independent pool monitor');
    LOtherTickets[0] := LOther.Submit(TPoolWork.Create(LOtherLease, 0), neThreaded);
    AwaitEntry(LOtherProbe, 0);
    {$IFDEF MSWINDOWS}
    LThreads[2] := OpenThread(SYNCHRONIZE, False, LOtherProbe.ThreadID(0));
    Check(LThreads[2] <> 0, 'retain independent worker termination handle');
    {$ENDIF}
    for LIndex := 1 to 3 do
    begin
      LOtherTickets[LIndex] := LOther.Submit(
        TPoolWork.Create(LOtherLease, LIndex + 1), neAsynchronous);
    end;
    Check((LOtherMonitor.WorkerLoad.Running = 1) and (LMonitor.WorkerLoad.Running = 2),
      'schedulers have independent workers and queues');
    LOtherProbe.Release;
    for LIndex := 0 to 3 do
    begin
      AwaitTerminal(LOtherTickets[LIndex]);
      Check(LOtherTickets[LIndex].Status = nesSucceeded, 'independent FIFO work succeeds');
    end;
    Check((LOtherProbe.Order(0) = 0) and (LOtherProbe.Order(1) = 2) and
      (LOtherProbe.Order(2) = 3) and (LOtherProbe.Order(3) = 4), 'single worker dequeues FIFO');
    LOther.Shutdown;

    LProbe.Release;
    for LIndex := 0 to 6 do
    begin

      if LTickets[LIndex] <> nil then
      begin
        AwaitTerminal(LTickets[LIndex]);

        if LIndex <> 3 then
        begin
          Check((LTickets[LIndex].Status = nesSucceeded) and (LProbe.Calls(LIndex) = 1),
            'admitted uncancelled work runs exactly once');
        end;
      end;
    end;
    Check((LProbe.Calls(3) = 0) and (LProbe.Calls(5) = 0),
      'cancelled and refused callbacks never start');
    AwaitPoolIdle(LMonitor);
    for LIndex := 100 to 123 do
    begin
      LTicket := LScheduler.Submit(TPoolWork.Create(LProbeLease, LIndex), neAsynchronous);
      AwaitTerminal(LTicket);
      Check((LTicket.Status = nesSucceeded) and
        ((LProbe.ThreadID(LIndex) = LFirstThread) or (LProbe.ThreadID(LIndex) = LSecondThread)),
        'later callbacks reuse the original live workers');
    end;
    Check(LMonitor.WorkerLoad.ActiveWorkers = 2, 'idle workers remain bounded and reusable');
    LTicket := LScheduler.Submit(TPoolWork.Create(LProbeLease, 40), neThreaded);
    AwaitTerminal(LTicket);
    Check((LTicket.Status = nesFailed) and (LTicket.Failure = 'Expected callback failure'),
      'callback failure stays in its execution diagnostic');
    LTicket := LScheduler.Submit(TPoolWork.Create(LProbeLease, 41), neThreaded);
    AwaitTerminal(LTicket);
    Check(LTicket.Status = nesSucceeded, 'a failed callback does not poison its worker');
    LTicket := LScheduler.Submit(TPoolWork.Create(LProbeLease, 70), neUIQueue);
    Check(LTicket.Status = nesPending, 'UI queue still defers');
    AwaitTerminal(LTicket);
    Check(LProbe.ThreadID(70) = MainThreadID, 'UI queue executes on the actual UI thread');
    LTicket := LScheduler.Submit(TPoolWork.Create(LProbeLease, 71), neSequential);
    Check((LTicket.Status = nesSucceeded) and (LProbe.ThreadID(71) = MainThreadID),
      'sequential execution remains inline on UI');
    AwaitPoolIdle(LMonitor);

    { Hold both reused workers, then close and release the scheduler itself.
      Returning while the gates stay closed proves shutdown did not join them.
      Work/queue leases, not the scheduler, carry them safely to completion. }
    LProbe.Hold;
    LFinal[0] := LScheduler.Submit(TPoolWork.Create(LProbeLease, 90), neThreaded);
    AwaitEntry(LProbe, 90);
    LFinal[1] := LScheduler.Submit(TPoolWork.Create(LProbeLease, 91), neThreaded);
    AwaitEntry(LProbe, 91);
    LFinal[2] := LScheduler.Submit(TPoolWork.Create(LProbeLease, 50), neThreaded);
    LScheduler.Shutdown;
    LLoad := LMonitor.WorkerLoad;
    Check(LLoad.Closed and (LLoad.Pending = 0) and (LLoad.Running = 2),
      'shutdown returns while workers remain held and releases pending work');
    Check(LFinal[0].Cancelled and LFinal[1].Cancelled and
      (LFinal[0].Status = nesRunning) and (LFinal[2].Status = nesCancelled),
      'running cancellation remains cooperative; queued work cannot enter');
    LRefused := False;
    { Keep an explicit caller lease across expected admission exceptions. Older
      FPC call-site temporaries are not a reliable exception lifetime boundary. }
    LRejectedWork := TPoolWork.Create(LProbeLease, 51);
    try
      LTicket := LScheduler.Submit(LRejectedWork, neThreaded);
    except
      on ENyxSchedule do
      begin
        LRefused := True;
      end;
    end;
    LRejectedWork := nil;
    Check(LRefused and (LProbe.Calls(51) = 0), 'closed scheduler refuses new work');
    LMonitor := nil;
    LScheduler := nil;
    LProbe.Release;
    AwaitTerminal(LFinal[0]);
    AwaitTerminal(LFinal[1]);
    Check((LFinal[0].Status = nesCancelled) and (LFinal[1].Status = nesCancelled) and
      (LProbe.Calls(50) = 0), 'independent worker leases survive scheduler release');
  finally
    LProbe.Release;
    LOtherProbe.Release;

    if LRouter <> nil then
    begin
      LRouter.Close;
    end;

    if LScheduler <> nil then
    begin
      LScheduler.Shutdown;
    end;

    if LOther <> nil then
    begin
      LOther.Shutdown;
    end;
    {$IFDEF MSWINDOWS}
    for LIndex := 0 to High(LThreads) do
    begin

      if LThreads[LIndex] <> 0 then
      begin
        Check(WaitForSingleObject(LThreads[LIndex], 5000) = WAIT_OBJECT_0,
          'actual pool worker terminated without a leaked host thread');
        CloseHandle(LThreads[LIndex]);
      end;
    end;
    {$ENDIF}
    LMonitor := nil;
    LOtherMonitor := nil;
    LScheduler := nil;
    LOther := nil;
    LProbeLease := nil;
    LOtherLease := nil;
  end;
end;

begin
  try
    Run;
    WriteLn('PASS ', GChecks, ' bounded native scheduler checks');
  except
    on LException: Exception do
    begin
      WriteLn(LException.Message);
      ExitCode := 1;
    end;
  end;
end.
