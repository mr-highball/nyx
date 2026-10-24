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
program nyx_scheduler_work_scope_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.scheduler
  {$ifdef PAS2JS}, JS, Web{$else}, Classes, SyncObjs{$endif};

type
  { A completed caller context carries only its independent scope. It lets the
    real scheduler prove admission even after the original parent is terminal. }
  TParent = class(TInterfacedObject, INyxExecution, INyxExecutionWorkScope)
  public
    Scope: INyxWorkScope;
    StopOnRead: INyxScheduler;
    StopOnCancelled: INyxScheduler;
    NestedScheduler: INyxScheduler;
    NestedWork: INyxWork;
    NestedTicket: INyxExecution;
    CancelledFlag: Boolean;
    procedure Cancel;
    function GetCancelled: Boolean;
    function GetStatus: TNyxExecutionStatus;
    function GetFailure: TNyxText;
    function GetWorkScope: INyxWorkScope;
  end;

  { Owned counters outlive individual queued work; borrowed object access is
    protected by the separate interface lease held by each work item. }
  TTrace = class(TInterfacedObject)
  public
    Entries: Integer;
    Finalizers: Integer;
    Propagated: Integer;
    BusyWhileRunning: Integer;
    BusyWhileFinalizing: Integer;
    Grandchild: INyxExecution;
  end;

  { Actual queued work optionally posts one grandchild using its own execution
    context. No scope owns this work or the scheduler. The finalizer inspects
    admission before releasing the owned scope to prove cleanup ordering. }
  TWork = class(TInterfacedObject, INyxWork)
  public
    Trace: TTrace;
    TraceLease: IInterface;
    Scope: INyxWorkScope;
    Scheduler: INyxScheduler;
    Spawn: Boolean;
    Fail: Boolean;
    destructor Destroy; override;
    procedure Execute(const AExecution: INyxExecution);
  end;

  {$ifndef PAS2JS}
  { Native worker holds one scope lease across an explicit barrier. Main-thread
    pause/close happens while that lease is live; no UI/thread sleep is needed. }
  TScopeWorker = class(TThread)
  public
    Scope: INyxWorkScope;
    Entered: TEvent;
    ReleaseWork: TEvent;
    Failed: Boolean;
    procedure Execute; override;
  end;
  {$endif}

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AMessage: TNyxText);
begin
  Inc(GChecks);

  if not ACondition then
  begin
    raise ENyxSchedule.Create(AMessage);
  end;
end;

procedure TParent.Cancel;
begin
  CancelledFlag := True;
end;

function TParent.GetCancelled: Boolean;
var
  LStop: INyxScheduler;
  LNested: INyxScheduler;
  LWork: INyxWork;
begin
  LStop := StopOnCancelled;
  StopOnCancelled := nil;
  LNested := NestedScheduler;
  NestedScheduler := nil;
  LWork := NestedWork;
  NestedWork := nil;

  if LStop <> nil then
  begin
    LStop.Shutdown;
  end;

  if LNested <> nil then
  begin
    NestedTicket := LNested.PostUI(LWork);
  end;
  Result := CancelledFlag;
end;

function TParent.GetStatus: TNyxExecutionStatus;
begin
  Result := nesSucceeded;
end;

function TParent.GetFailure: TNyxText;
begin
  Result := '';
end;

function TParent.GetWorkScope: INyxWorkScope;
var
  LStop: INyxScheduler;
begin
  LStop := StopOnRead;
  StopOnRead := nil;

  if LStop <> nil then
  begin
    LStop.Shutdown;
  end;
  Result := Scope;
end;

destructor TWork.Destroy;
var
  LPause: INyxWorkPause;
begin
  Inc(Trace.Finalizers);
  LPause := Scope.TryPause;

  if LPause = nil then
  begin
    Inc(Trace.BusyWhileFinalizing);
  end;
  inherited Destroy;
end;

procedure TWork.Execute(const AExecution: INyxExecution);
var
  LContext: INyxExecutionWorkScope;
  LPause: INyxWorkPause;
  LChild: TWork;
  LChildLease: INyxWork;
begin
  Inc(Trace.Entries);

  if Supports(AExecution, INyxExecutionWorkScope, LContext) and
    (LContext.WorkScope = Scope) then
  begin
    Inc(Trace.Propagated);
  end;
  LPause := Scope.TryPause;

  if LPause = nil then
  begin
    Inc(Trace.BusyWhileRunning);
  end;

  if Spawn then
  begin
    LChild := TWork.Create;
    LChildLease := LChild;
    LChild.Trace := Trace;
    LChild.TraceLease := TraceLease;
    LChild.Scope := Scope;
    Trace.Grandchild := Scheduler.PostUI(LChildLease, AExecution);
  end;

  if Fail then
  begin
    raise ENyxSchedule.Create('Expected child failure');
  end;
end;

{$ifndef PAS2JS}
procedure TScopeWorker.Execute;
var
  LLease: INyxWorkLease;
begin
  try
    LLease := Scope.Acquire;
    Entered.SetEvent;
    Failed := ReleaseWork.WaitFor(5000) <> wrSignaled;
    LLease.Retire;
    LLease.Retire;
  except
    Failed := True;
    Entered.SetEvent;
  end;
end;
{$endif}

function AcquireRefuses(const AScope: INyxWorkScope): Boolean;
var
  LLease: INyxWorkLease;
begin
  Result := False;
  try
    LLease := AScope.Acquire;
  except
    on E: ENyxSchedule do
    begin
      Result := True;
    end;
  end;
end;

procedure ScopeCases;
var
  LScope: INyxWorkScope;
  LOther: INyxWorkScope;
  LFirst: INyxWorkLease;
  LSecond: INyxWorkLease;
  LPause: INyxWorkPause;
  LOldPause: INyxWorkPause;
  LRefused: Boolean;
begin
  LScope := NewNyxWorkScope;
  LOther := NewNyxWorkScope;
  LFirst := LScope.Acquire;
  LSecond := LScope.Acquire;
  Check(LScope.TryPause = nil, 'two original admitted items prevent idle preview');
  LFirst.Retire;
  LFirst.Retire;
  Check(LScope.TryPause = nil, 'repeated retirement cannot release another item');
  LSecond := nil;
  LPause := LScope.TryPause;
  Check(LPause <> nil, 'last lease destruction admits exclusive idle preview');
  LPause.CheckReady;
  Check(AcquireRefuses(LScope) and (LScope.TryPause = nil),
    'preview refuses new work and a second preview without cancelling anything');
  LFirst := LOther.Acquire;
  Check(LFirst <> nil, 'another scope remains independently usable during preview');
  LRefused := False;
  try
    LFirst := LScope.Acquire;
  except
    on E: ENyxSchedule do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (LFirst <> nil) and (LOther.TryPause = nil),
    'failed hidden-result assignment preserves the caller previous work lease');
  LPause := LScope.TryPause;
  Check(LPause = nil, 'same-destination preview cannot release its guard before admission');
  LPause := LScope.TryPause;
  Check(LPause <> nil, 'caller replacement releases the refused old guard only after admission');
  LPause.Retire;
  LOldPause := LPause;
  LPause := LScope.TryPause;
  Check(LPause <> nil, 'retired preview permits a fresh exact guard');
  LOldPause.Retire;
  Check(AcquireRefuses(LScope), 'old guard cannot release its successor pause');
  LRefused := False;
  try
    LOldPause.CheckReady;
  except
    on E: ENyxSchedule do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'retained retired guard never regains readiness');
  LScope.Close;
  LRefused := False;
  try
    LPause.CheckReady;
  except
    on E: ENyxSchedule do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'closure invalidates an entered preview immediately');
  LPause := nil;
  Check(AcquireRefuses(LScope) and (LScope.TryPause = nil),
    'guard destruction cannot reopen a closed scope');
  LScope := NewNyxWorkScope;
  LFirst := LScope.Acquire;
  LScope.Close;
  LFirst.Retire;
  Check(AcquireRefuses(LScope), 'existing admitted work retires after terminal close');
end;

{$ifdef PAS2JS}
function Tick: TJSPromise;
begin
  Result := TJSPromise.new(procedure(AResolve, AReject: TJSPromiseResolver)
    begin
      window.setTimeout(procedure
        begin
          AResolve(True);
        end, 1);
    end);
end;
{$endif}

procedure SchedulerCases; {$ifdef PAS2JS}async;{$endif}
var
  LScope: INyxWorkScope;
  LScheduler: INyxScheduler;
  LCapability: INyxSchedulerWorkScopes;
  LParent: TParent;
  LParentLease: INyxExecution;
  LTrace: TTrace;
  LTraceLease: IInterface;
  LWork: TWork;
  LWorkLease: INyxWork;
  LChild: INyxExecution;
  LContext: INyxExecutionWorkScope;
  LPause: INyxWorkPause;
  LRefused: Boolean;
  LIndex: Integer;

  procedure Prepare(AFail, ASpawn: Boolean);
  begin
    LWork := TWork.Create;
    LWorkLease := LWork;
    LWork.Trace := LTrace;
    LWork.TraceLease := LTraceLease;
    LWork.Scope := LScope;
    LWork.Fail := AFail;
    LWork.Spawn := ASpawn;

    if ASpawn then
    begin
      LWork.Scheduler := LScheduler;
    end;
  end;

begin
  LScope := NewNyxWorkScope;
  LScheduler := NewNyxScheduler;
  LParent := TParent.Create;
  LParentLease := LParent;
  LParent.Scope := LScope;
  LTrace := TTrace.Create;
  LTraceLease := LTrace;
  try
    Check(Supports(LScheduler, INyxSchedulerWorkScopes, LCapability),
      'ordinary scheduler advertises explicit parent-scope admission');
    Prepare(False, True);
    LChild := LScheduler.PostUI(LWorkLease, LParentLease);
    LWorkLease := nil;
    Check((LParentLease.Status = nesSucceeded) and (LChild.Status = nesPending),
      'child work remains pending after its parent is terminal');
    Check((LScope.TryPause = nil) and (LTrace.Entries = 0),
      'pending child blocks preview without entering or cancelling the child');
    Check(Supports(LChild, INyxExecutionWorkScope, LContext) and
      (LContext.WorkScope = LScope), 'real child execution inherits its exact parent domain');
    for LIndex := 1 to 200 do
    begin
      {$ifdef PAS2JS}await(Tick);{$else}CheckSynchronize(10);{$endif}

      if LTrace.Finalizers = 2 then
      begin
        Break;
      end;
    end;
    Check((LTrace.Entries = 2) and (LTrace.Finalizers = 2) and
      (LTrace.Propagated = 2) and (LTrace.BusyWhileRunning = 2) and
      (LTrace.BusyWhileFinalizing = 2), 'child and grandchild stay counted through execution and finalizers');
    Check((LChild.Status = nesSucceeded) and (LTrace.Grandchild.Status = nesSucceeded),
      'independent child and grandchild tickets finish normally');
    LPause := LScope.TryPause;
    Check(LPause <> nil, 'retained terminal tickets do not keep the domain busy');
    Prepare(False, False);
    LRefused := False;
    try
      LChild := LScheduler.PostUI(LWorkLease, LParentLease);
    except
      on E: ENyxSchedule do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LTrace.Entries = 2), 'paused domain refuses before new work entry');
    LPause.Retire;
    LChild := LScheduler.PostUI(LWorkLease, LParentLease);
    LWorkLease := nil;
    LChild.Cancel;
    Check(LScope.TryPause = nil, 'cancelled queued courier still protects its owned work finalizer');
    for LIndex := 1 to 200 do
    begin
      {$ifdef PAS2JS}await(Tick);{$else}CheckSynchronize(10);{$endif}

      if LTrace.Finalizers = 3 then
      begin
        Break;
      end;
    end;
    Check((LTrace.Finalizers = 3) and (LTrace.Entries = 2) and
      (LChild.Status = nesCancelled), 'cancelled courier skips entry and releases owned work');
    LPause := LScope.TryPause;
    Check(LPause <> nil, 'cancelled courier retirement restores idle admission');
    LPause.Retire;
    Prepare(True, False);
    LChild := LScheduler.PostUI(LWorkLease, LParentLease);
    LWorkLease := nil;
    for LIndex := 1 to 200 do
    begin
      {$ifdef PAS2JS}await(Tick);{$else}CheckSynchronize(10);{$endif}

      if LTrace.Finalizers = 4 then
      begin
        Break;
      end;
    end;
    Check((LTrace.Finalizers = 4) and (LChild.Status = nesFailed) and
      (LChild.Failure = 'Expected child failure'), 'failed child preserves its diagnostic and retires admission');
    LPause := LScope.TryPause;
    Check(LPause <> nil, 'failed child leaves no hidden work reservation');
    LPause.Retire;
    Prepare(False, False);
    LParent.StopOnRead := LScheduler;
    LRefused := False;
    try
      LChild := LScheduler.PostUI(LWorkLease, LParentLease);
    except
      on E: ENyxSchedule do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LTrace.Entries = 3), 'foreign parent getter cannot submit into a scheduler it closed');
    LWorkLease := nil;
    LPause := LScope.TryPause;
    Check(LPause <> nil, 'failed admission releases its original domain lease');
  finally
    LScheduler.Shutdown;
  end;
end;

{ Prune reads actual scheduler ticket status, which queries its foreign parent
  cancellation. Closing or posting a NEW deferred item there must neither
  corrupt the original vector nor admit an outer item into a closed scheduler. }
procedure PruneReentryCases; {$ifdef PAS2JS}async;{$endif}
var
  LScope: INyxWorkScope;
  LScheduler: INyxScheduler;
  LParent: TParent;
  LParentLease: INyxExecution;
  LTrace: TTrace;
  LTraceLease: IInterface;
  LWork: TWork;
  LWorkLease: INyxWork;
  LOriginal: INyxExecution;
  LOuter: INyxExecution;
  LMode: Integer;
  LIndex: Integer;
  LRefused: Boolean;

  procedure Prepare;
  begin
    LWork := TWork.Create;
    LWorkLease := LWork;
    LWork.Trace := LTrace;
    LWork.TraceLease := LTraceLease;
    LWork.Scope := LScope;
  end;

begin
  for LMode := 0 to 1 do
  begin
    LScope := NewNyxWorkScope;
    LScheduler := NewNyxScheduler;
    LParent := TParent.Create;
    LParentLease := LParent;
    LParent.Scope := LScope;
    LTrace := TTrace.Create;
    LTraceLease := LTrace;
    try
      Prepare;
      LOriginal := LScheduler.PostUI(LWorkLease, LParentLease);
      LWorkLease := nil;

      if LMode = 0 then
      begin
        LParent.StopOnCancelled := LScheduler;
      end
      else
      begin
        Prepare;
        LParent.NestedScheduler := LScheduler;
        LParent.NestedWork := LWorkLease;
        LWorkLease := nil;
      end;
      Prepare;
      LRefused := False;
      try
        LOuter := LScheduler.Submit(LWorkLease, neSequential);
      except
        on E: ENyxSchedule do
        begin
          LRefused := True;
        end;
      end;
      LWorkLease := nil;

      if LMode = 0 then
      begin
        Check(LRefused and (LTrace.Entries = 0),
          'parent cancellation getter closing scheduler refuses outer work before adoption');
      end
      else
      begin
        Check(not LRefused and (LOuter.Status = nesSucceeded) and
          (LParent.NestedTicket.Status = nesPending),
          'nested deferred submission survives the original pruning pass');
      end;
      for LIndex := 1 to 200 do
      begin
        {$ifdef PAS2JS}await(Tick);{$else}CheckSynchronize(10);{$endif}

        if (LMode = 0) or (LTrace.Entries = 3) then
        begin
          Break;
        end;
      end;

      if LMode = 0 then
      begin
        Check((LTrace.Entries = 0) and (LOriginal.Status = nesCancelled),
          'shutdown from pruning leaves the original child cancelled without entry');
      end
      else
      begin
        Check((LTrace.Entries = 3) and (LOriginal.Status = nesSucceeded) and
          (LParent.NestedTicket.Status = nesSucceeded),
          'original, outer and nested work each complete exactly once');
        LScheduler.Shutdown;
      end;
    finally
      LScheduler.Shutdown;
      LParent.NestedWork := nil;
      LParent.NestedScheduler := nil;
      LParent.StopOnCancelled := nil;
    end;
  end;
end;

{$ifndef PAS2JS}
procedure NativeThreadCase;
var
  LScope: INyxWorkScope;
  LWorker: TScopeWorker;
  LFailed: Boolean;
begin
  LScope := NewNyxWorkScope;
  LWorker := TScopeWorker.Create(True);
  LWorker.Scope := LScope;
  LWorker.Entered := TEvent.Create(nil, True, False, '');
  LWorker.ReleaseWork := TEvent.Create(nil, True, False, '');
  try
    LWorker.Start;
    Check(LWorker.Entered.WaitFor(5000) = wrSignaled, 'native worker reaches its admitted barrier');
    Check(LScope.TryPause = nil, 'actual concurrent native lease prevents UI preview');
    LScope.Close;
    Check(AcquireRefuses(LScope), 'native close refuses new work while earlier worker remains admitted');
  finally
    LWorker.ReleaseWork.SetEvent;
    LWorker.WaitFor;
    LFailed := LWorker.Failed;
    LWorker.Entered.Free;
    LWorker.ReleaseWork.Free;
    LWorker.Free;
  end;
  Check(not LFailed, 'native admitted work retires after closure');
end;
{$endif}

procedure Run; {$ifdef PAS2JS}async;{$endif}
begin
  try
    ScopeCases;
    {$ifdef PAS2JS}
    await(SchedulerCases);
    await(PruneReentryCases);
    {$else}
    SchedulerCases;
    PruneReentryCases;
    NativeThreadCase;
    {$endif}
    {$ifdef PAS2JS}
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' scoped scheduler checks';
    document.body.setAttribute('data-work-scope', 'passed');
    document.body.setAttribute('data-work-scope-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' scoped scheduler checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.textContent := 'FAIL ' + E.Message;
      document.body.setAttribute('data-work-scope', 'failed');
      document.body.setAttribute('data-work-scope-error', E.Message);
      document.body.setAttribute('data-work-scope-checks', IntToStr(GChecks));
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end;

begin
  Run;
end.
