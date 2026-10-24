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
unit nyx.scheduler;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  SysUtils,
  nyx.text;

type
  { A cancellation generation owns no work, scheduler, model or platform handle.
    Cancel is irreversible and may be observed by native worker callbacks.
    A producer cancels its generation before replacing the source, including
    inside a sequential callback before Submit returns its execution token. }
  INyxCancellationScope = interface(IInterface)
    ['{739BC309-7893-48E3-9600-001001000006}']
    procedure Cancel;
    function GetCancelled: Boolean;
    property Cancelled: Boolean read GetCancelled;
  end;

  { Optional final admission for a retained cancellation generation. CheckReady
    reads synchronized cancellation facts only and raises when cancelled or when
    a dependency cannot supply this guarantee. It must not invoke application
    callbacks, query mutable UI/model objects or capture a newer generation.
    Consumers capture this facet before validation, then use it after all
    outward validators and immediately before noncalling publication installs.
    This interface grants neither scheduling nor UI access. }
  INyxCancellationReadiness = interface(IInterface)
    ['{0CFD558C-6244-42EC-BC52-A8309815B415}']
    procedure CheckReady;
  end;

  { One admitted work item retains its scope without owning a scheduler, work
    callback or UI. Retire is idempotent and releases its count; the scheduler
    calls it only after execution/skipping AND release of the owned work. }
  INyxWorkLease = interface(IInterface)
    ['{90AF406F-7633-4A34-ACF1-FDD8A23B52BB}']
    procedure Retire;
  end;

  { Exclusive reversible pause of an idle work scope. While retained, no new
    work may enter that scope. CheckReady reads synchronized flags only; Close
    invalidates readiness without revoking this guard's safe retirement. No UI,
    cancellation, callback or event-loop pumping occurs during these operations. }
  INyxWorkPause = interface(IInterface)
    ['{E2822967-E9D1-475F-88F9-E635FA5538AB}']
    procedure CheckReady;
    procedure Retire;
  end;

  { Independent admission domain for related scheduled work, including child
    work after its original callback completes. Acquire refuses paused/closed
    scopes; TryPause returns nil while work is retained, without cancelling it.
    Close is terminal and does not revoke existing leases or stop running work.
    Native methods are synchronized; browser methods use the host event loop.
    A scope owns only counters/flags, never executions, callbacks or controls. }
  INyxWorkScope = interface(IInterface)
    ['{497D2868-3C27-47F4-93AB-52F6817A4CFA}']
    function Acquire: INyxWorkLease;
    function TryPause: INyxWorkPause;
    procedure Close;
  end;

  ENyxSchedule = class(Exception);
  { A saturated native pending queue refuses the submission before adopting its
    work. Callers may defer/retry, coalesce their inputs or report overload; work
    never silently falls back to the UI thread. Running work is not queue usage. }
  ENyxScheduleCapacity = class(ENyxSchedule);
  { Sequential work completes before Submit returns. Asynchronous uses native
    workers or the browser event loop, as advertised by Capabilities. UIQueue
    always defers to the UI thread. Threaded explicitly requires a real worker;
    a browser never pretends that a timeout is a thread. }
  TNyxExecutionPolicy = (neSequential, neAsynchronous, neUIQueue, neThreaded);
  { A declaration may restrict execution without claiming target scheduler
    support. The event owner and scheduler admit these two concerns separately. }
  TNyxExecutionPolicies = set of TNyxExecutionPolicy;
  TNyxSchedulerCapability = (nscDeferredUI, nscWorkerThreads);
  TNyxSchedulerCapabilities = set of TNyxSchedulerCapability;
  TNyxExecutionStatus = (nesPending, nesRunning, nesSucceeded, nesFailed, nesCancelled);

  { Immutable fluent scheduler construction options, copied into the scheduler.
    Workers are native-only: browser asynchronous callbacks use the host event
    loop and Threaded still refuses. Start with Defaults; a zeroed record refuses
    construction instead of implicitly opting into unbounded work. Worker count
    is 1..64; pending capacity is 1..65536. Defaults are four workers/1024 slots. }
  TNyxSchedulerOptions = record
  private
    FWorkerLimit: Integer;
    FPendingLimit: Integer;
  public
    class function Defaults: TNyxSchedulerOptions; static;
    function Workers(ACount: Integer): TNyxSchedulerOptions;
    function PendingCapacity(ACount: Integer): TNyxSchedulerOptions;
    procedure Validate;
    property WorkerLimit: Integer read FWorkerLimit;
    property PendingLimit: Integer read FPendingLimit;
  end;

  { A copied instantaneous native pool observation, never an ownership handle.
    Pending excludes running/deferred-UI work; cancelled entries may remain until
    admission, dequeue or shutdown reclaims them. ActiveWorkers includes idle
    workers and may lag startup/retirement. Browser counts/limits are zero because
    it has no native pool. Closed reports scheduler admission, even before start. }
  TNyxWorkerPoolSnapshot = record
    WorkerLimit: Integer;
    PendingLimit: Integer;
    ActiveWorkers: Integer;
    Running: Integer;
    Pending: Integer;
    Closed: Boolean;
  end;

  { Optional inspection leaves alternative INyxScheduler implementations source
    compatible. The built-in scheduler implements this interface on both targets.
    Read on the UI thread; the native queue copies its counters under its lock. }
  INyxSchedulerMonitor = interface(IInterface)
    ['{739BC309-7893-48E3-9600-001001000004}']
    function GetWorkerLoad: TNyxWorkerPoolSnapshot;
    property WorkerLoad: TNyxWorkerPoolSnapshot read GetWorkerLoad;
  end;

  { A retained execution owns its diagnostic independently of its work/scheduler.
    Cancellation is cooperative once running; pending cancellation prevents entry.
    Native getters and transitions are synchronized. Status remains Running until
    running work returns. Cancelled exposes the request immediately. }
  INyxExecution = interface(IInterface)
    ['{739BC309-7893-48E3-9600-001001000001}']
    procedure Cancel;
    function GetCancelled: Boolean;
    function GetStatus: TNyxExecutionStatus;
    function GetFailure: TNyxText;
    property Cancelled: Boolean read GetCancelled;
    property Status: TNyxExecutionStatus read GetStatus;
    property Failure: TNyxText read GetFailure;
  end;

  { Optional immutable work ancestry, separate from terminal execution status
    and cancellation. PostUI captures this scope from its explicit Parent and
    passes it to the child, so grandchildren join the same admission domain.
    Nil denotes ordinary unscoped work. The facet owns no scheduler or UI. }
  INyxExecutionWorkScope = interface(IInterface)
    ['{B29386BF-15C6-4A64-8D51-030C116DA613}']
    function GetWorkScope: INyxWorkScope;
    property WorkScope: INyxWorkScope read GetWorkScope;
  end;

  { Optional scheduler guarantee: PostUI reserves an explicit parent's work
    scope before adopting/queueing its work, propagates the same scope to its
    execution, and releases admission after work retirement on every outcome.
    Alternative schedulers advertise this marker only if they meet that rule.
    It grants no worker, UI or whole-scheduler quiescence capability. }
  INyxSchedulerWorkScopes = interface(IInterface)
    ['{6E47F577-4627-49BE-8408-EAF494E9C72D}']
  end;

  { Work is retained until it returns or is skipped. Worker work must own its
    input and avoid UI/model mutations; use UIQueue for those mutations. Implement
    on any reference-counted base. A callback failure belongs to its execution,
    rather than escaping the browser timer or native thread. Native worker threads
    are reused: do not assume thread-local state starts fresh for each callback. }
  INyxWork = interface(IInterface)
    ['{739BC309-7893-48E3-9600-001001000002}']
    procedure Execute(const AExecution: INyxExecution);
  end;

  { Submit/Shutdown are UI-thread operations. Shutdown cancels all outstanding
    executions; it does not block the UI waiting for running workers. Their
    retained work can finish safely and observe cancellation. The native UI
    queue is serviced by LCL's loop or CheckSynchronize in a console host. }
  INyxScheduler = interface(IInterface)
    ['{739BC309-7893-48E3-9600-001001000003}']
    function GetCapabilities: TNyxSchedulerCapabilities;
    { Verify the caller can access UI-owned state. Native callers must be on
      the main UI thread; browser callbacks execute on their host event loop.
      This check is independent of shutdown so teardown can revoke weak ports. }
    procedure RequireUI;
    procedure Admit(APolicy: TNyxExecutionPolicy);
    { Native async/threaded submission can raise ENyxScheduleCapacity when the
      configured pending queue is full. Cancelled pending work is reclaimed before
      that decision; refusal performs no work and does not switch UI policy. }
    function Submit(const AWork: INyxWork; APolicy: TNyxExecutionPolicy): INyxExecution;
    { Safe worker-to-UI handoff. Native workers wait only for the UI to accept
      the submission; UI work itself runs through its deferred queue. Do not
      block the UI joining a worker that needs this handoff. Pass the callback's
      execution context as Parent to inherit its view/subscription cancellation;
      a completed parent can still invalidate a queued UI result on navigation. }
    function PostUI(const AWork: INyxWork;
      const AParent: INyxExecution = nil): INyxExecution;
    procedure Shutdown;
    property Capabilities: TNyxSchedulerCapabilities read GetCapabilities;
  end;

  { Optional cooperative UI turns preserve the original scheduler interface/GUID.
    PostTurn is UI-only and uses a short platform delay instead of appending an
    immediately eligible continuation to the same queue drain. Work still obeys
    ordinary cancellation/shutdown ownership. This is a scheduling opportunity,
    not a wall-clock latency or painted-frame guarantee. Older native RTLs and
    alternative adapters must report unsupported rather than claim real yielding. }
  INyxSchedulerTurns = interface(IInterface)
    ['{8794C0A7-22C9-4C67-8B76-5D4F23319CFA}']
    function GetCanYieldUI: Boolean;
    function PostTurn(const AWork: INyxWork): INyxExecution;
    property CanYieldUI: Boolean read GetCanYieldUI;
  end;
  { Optional delayed UI turns, preserving existing scheduler/turn interfaces.
    Delay is 1..60000 milliseconds and UI-only. This is an earliest scheduling
    opportunity, not a deadline. Pending cancellation skips Execute but the
    queued UI courier still releases its owned work on that UI thread. }
  INyxSchedulerDelays = interface(IInterface)
    ['{113AE5C0-7C20-4DAD-B30D-7783B7D7A2C7}']
    function PostAfter(const AWork: INyxWork; ADelayMilliseconds: Integer): INyxExecution;
  end;

{ Admit the optional capability on UI, without scheduling or adopting work.
  Nil/missing/unsupported adapters refuse with a scheduler diagnostic. }
function NyxSchedulerTurns(const AScheduler: INyxScheduler): INyxSchedulerTurns;
{ Admit delayed turns on UI; missing/unsupported adapters refuse explicitly. }
function NyxSchedulerDelays(const AScheduler: INyxScheduler): INyxSchedulerDelays;

{ Platform implementation, with no dependency on the document, DOM controls or
  LCL controls. Browser workers require an explicit separate transport/program;
  ordinary Pascal callbacks are deferred on its UI event loop. }
function NewNyxScheduler: INyxScheduler; overload;
function NewNyxScheduler(const AOptions: TNyxSchedulerOptions): INyxScheduler; overload;
{ Adapter admission failures can expose a terminal diagnostic without scheduling
  a callback. The token owns exact text; empty diagnostics refuse. This does not
  convert an unsupported policy into supported execution or perform any work. }
function NewNyxFailedExecution(const AFailure: TNyxText): INyxExecution;
{ Independent cancellation generation. Dropping a lease does not cancel it. }
function NewNyxCancellationScope: INyxCancellationScope;
{ A fresh independent work domain. A retained pause is reversible; Close is
  permanent. Destroying a lease/guard retires it without application callbacks. }
function NewNyxWorkScope: INyxWorkScope;

implementation

uses
  {$IFDEF PAS2JS}
  Web;
  {$ELSE}
  Classes,
  SyncObjs;
  {$ENDIF}

type
  { One atomic terminal flag, independent of scheduler/work ownership. Its final
    readiness facet has exactly the same synchronized cancellation meaning. }
  TNyxCancellationScope = class(TInterfacedObject, INyxCancellationScope,
    INyxCancellationReadiness)
  private
    FCancelled: LongInt;
  public
    procedure Cancel;
    function GetCancelled: Boolean;
    procedure CheckReady;
  end;

  { Synchronized ticket retains cancellation ancestry and an immutable optional
    work domain. The courier, not this terminal ticket, owns admission; retaining
    completed tickets therefore cannot keep an otherwise idle scope busy. }
  TNyxExecution = class(TInterfacedObject, INyxExecution, INyxExecutionWorkScope)
  private
    FCancelled: Boolean;
    FStatus: TNyxExecutionStatus;
    FFailure: TNyxText;
    FParent: INyxExecution;
    FWorkScope: INyxWorkScope;
    {$IFNDEF PAS2JS}
    FLock: TRTLCriticalSection;
    {$ENDIF}
    procedure Lock;
    procedure Unlock;
  public
    constructor Create(const AParent: INyxExecution;
      const AWorkScope: INyxWorkScope = nil);
    destructor Destroy; override;
    procedure Cancel;
    function GetCancelled: Boolean;
    function GetStatus: TNyxExecutionStatus;
    function GetFailure: TNyxText;
    function GetWorkScope: INyxWorkScope;
    function Start: Boolean;
    procedure Complete(const AFailure: TNyxText);
  end;

  { A courier owns every input. Immediate/worker delivery releases it after Run;
    a native deferred UI entry instead owns it until delivery or retirement.
    Browser timers retain the existing self-releasing delivery boundary. Its
    optional admission lease survives work finalizers and retires even when
    delivery fails or the native queue discards a cancelled pending entry. }
  TNyxCourier = class
  private
    FWork: INyxWork;
    FExecution: INyxExecution;
    FState: TNyxExecution;
    FWorkLease: INyxWorkLease;
  public
    constructor Create(const AWork: INyxWork; AState: TNyxExecution;
      const AWorkLease: INyxWorkLease);
    destructor Destroy; override;
    procedure Run;
    procedure Deliver;
  end;

  {$IFNDEF PAS2JS}
  TNyxNativeUIQueue = class;

  { Private managed platform target, never a public capability/GUID. Submission
    and the pending registry retain independent entry leases across Wake hooks;
    the raw RTL method pointer is removed before retirement releases user work. }
  INyxNativeUIEntry = interface(IInterface)
    procedure Enqueue(ADelay: Integer);
    procedure Detach;
    procedure Unqueue;
    procedure Retire;
  end;
  { An independent queue lifetime, with no reference back to its scheduler.
    Execution pruning/cancellation cannot orphan these pending ownership leases. }
  INyxNativeUIQueue = interface(IInterface)
    function Adopt(ACourier: TNyxCourier): INyxNativeUIEntry;
    procedure Close;
  end;
  { Each slot owns a managed entry; traversal order never selects another
    scheduler's work. The complete vector transfers to Close before callouts. }
  TNyxNativeUIEntries = array of INyxNativeUIEntry;
  { UI-thread-only registry. Entries borrow this owner only while registered;
    Close revokes every weak link and queued method before releasing any work. }
  TNyxNativeUIQueue = class(TInterfacedObject, INyxNativeUIQueue)
  private
    FClosed: Boolean;
    FEntries: TNyxNativeUIEntries;
    procedure Forget(const AEntry: INyxNativeUIEntry);
  public
    destructor Destroy; override;
    function Adopt(ACourier: TNyxCourier): INyxNativeUIEntry;
    procedure Close;
  end;
  { Owns one courier, not the scheduler. A submission lease outlives ForceQueue
    even when WakeMainThread delivers, closes the owner or raises after insertion.
    Deliver detaches before user work; Retire clears ownership before destruction. }
  TNyxNativeUIEntry = class(TInterfacedObject, INyxNativeUIEntry)
  private
    FOwner: TNyxNativeUIQueue;
    FCourier: TNyxCourier;
  public
    destructor Destroy; override;
    procedure Enqueue(ADelay: Integer);
    procedure Detach;
    procedure Unqueue;
    procedure Retire;
    procedure Deliver;
  end;

  { Workers retain only this independent queue, never their scheduler or UI.
    It owns pending couriers; Take transfers one to a worker. No thread object is
    retained by the queue, so retiring the scheduler cannot create a cycle. }
  INyxWorkerQueue = interface(IInterface)
    ['{739BC309-7893-48E3-9600-001001000005}']
    procedure StartWorkers;
    procedure Push(ACourier: TNyxCourier);
    function Take: TNyxCourier;
    procedure WorkerEntered;
    procedure WorkerExited;
    procedure Finished;
    procedure Close;
    function Snapshot: TNyxWorkerPoolSnapshot;
  end;

  TNyxWorkerQueue = class(TInterfacedObject, INyxWorkerQueue)
  private
    FLock: TRTLCriticalSection;
    FReady: TEvent;
    FOptions: TNyxSchedulerOptions;
    FItems: array of TNyxCourier;
    FHead: Integer;
    FCount: Integer;
    FWorkers: Integer;
    FRunning: Integer;
    FClosed: Boolean;
    FStarted: Boolean;
  public
    constructor Create(const AOptions: TNyxSchedulerOptions);
    destructor Destroy; override;
    procedure StartWorkers;
    procedure Push(ACourier: TNyxCourier);
    function Take: TNyxCourier;
    procedure WorkerEntered;
    procedure WorkerExited;
    procedure Finished;
    procedure Close;
    function Snapshot: TNyxWorkerPoolSnapshot;
  end;

  TNyxPoolWorker = class(TThread)
  private
    FQueue: INyxWorkerQueue;
  protected
    procedure Execute; override;
  public
    constructor Create(const AQueue: INyxWorkerQueue);
  end;

  { An empty one-shot worker is retained solely for older RTL UI-queue bootstrap.
    User asynchronous/threaded work always uses the owned bounded queue above. }
  TNyxWorker = class(TThread)
  private
    FCourier: TNyxCourier;
  protected
    procedure Execute; override;
  public
    constructor Create(ACourier: TNyxCourier);
    destructor Destroy; override;
  end;
  {$ENDIF}

  { UI-owned dispatcher retains couriers independently of worker/UI entries.
    Explicit parent work scopes are admitted per submission, without pausing
    unrelated work or adding a scheduler-to-work ownership cycle. }
  TNyxScheduler = class(TInterfacedObject, INyxScheduler, INyxSchedulerMonitor,
    INyxSchedulerTurns, INyxSchedulerDelays, INyxSchedulerWorkScopes)
  private
    FClosed: Boolean;
    { Status getters may reenter through a foreign parent execution. A nested
      submission skips pruning; its appended tickets must survive the outer pass. }
    FPruning: Boolean;
    {$IFNDEF PAS2JS}
    FOptions: TNyxSchedulerOptions;
    FPool: INyxWorkerQueue;
    FUIQueue: INyxNativeUIQueue;
    {$ENDIF}
    FExecutions: array of INyxExecution;
    procedure Prune;
    function SubmitAttached(const AWork: INyxWork; APolicy: TNyxExecutionPolicy;
      const AParent: INyxExecution; ADelay: Integer = 0): INyxExecution;
  public
    constructor Create(const AOptions: TNyxSchedulerOptions);
    destructor Destroy; override;
    function GetWorkerLoad: TNyxWorkerPoolSnapshot;
    function GetCanYieldUI: Boolean;
    function PostTurn(const AWork: INyxWork): INyxExecution;
    function PostAfter(const AWork: INyxWork; ADelayMilliseconds: Integer): INyxExecution;
    function GetCapabilities: TNyxSchedulerCapabilities;
    procedure RequireUI;
    procedure Admit(APolicy: TNyxExecutionPolicy);
    function Submit(const AWork: INyxWork; APolicy: TNyxExecutionPolicy): INyxExecution;
    function PostUI(const AWork: INyxWork;
      const AParent: INyxExecution = nil): INyxExecution;
    procedure Shutdown;
  end;

  {$IFNDEF PAS2JS}
  TNyxUISubmission = class
  public
    Scheduler: INyxScheduler;
    Work: INyxWork;
    Execution: INyxExecution;
    Parent: INyxExecution;
    procedure Submit;
  end;
  {$ENDIF}

{$I nyx.scheduler.work.inc}

class function TNyxSchedulerOptions.Defaults: TNyxSchedulerOptions;
begin
  Result.FWorkerLimit := 4;
  Result.FPendingLimit := 1024;
end;

function TNyxSchedulerOptions.Workers(ACount: Integer): TNyxSchedulerOptions;
begin

  if (ACount < 1) or (ACount > 64) then
  begin
    raise ENyxSchedule.Create('Worker count must be between 1 and 64');
  end;
  Result := Self;
  Result.FWorkerLimit := ACount;
end;

function TNyxSchedulerOptions.PendingCapacity(ACount: Integer): TNyxSchedulerOptions;
begin

  if (ACount < 1) or (ACount > 65536) then
  begin
    raise ENyxSchedule.Create('Pending capacity must be between 1 and 65536');
  end;
  Result := Self;
  Result.FPendingLimit := ACount;
end;

procedure TNyxSchedulerOptions.Validate;
begin

  if (FWorkerLimit < 1) or (FWorkerLimit > 64) or
    (FPendingLimit < 1) or (FPendingLimit > 65536) then
  begin
    raise ENyxSchedule.Create('Use valid explicit scheduler options or Defaults');
  end;
end;

procedure TNyxCancellationScope.Cancel;
begin
  {$IFDEF PAS2JS}
  FCancelled := 1;
  {$ELSE}
  InterlockedExchange(FCancelled, 1);
  {$ENDIF}
end;

function TNyxCancellationScope.GetCancelled: Boolean;
begin
  {$IFDEF PAS2JS}
  Result := FCancelled <> 0;
  {$ELSE}
  Result := InterlockedCompareExchange(FCancelled, 0, 0) <> 0;
  {$ENDIF}
end;

procedure TNyxCancellationScope.CheckReady;
begin

  if GetCancelled then
  begin
    raise ENyxSchedule.Create('Cancellation generation is no longer ready');
  end;
end;

function NewNyxCancellationScope: INyxCancellationScope;
begin
  Result := TNyxCancellationScope.Create;
end;

constructor TNyxExecution.Create(const AParent: INyxExecution;
  const AWorkScope: INyxWorkScope);
begin
  inherited Create;
  {$IFNDEF PAS2JS}
  InitCriticalSection(FLock);
  {$ENDIF}
  FStatus := nesPending;
  FParent := AParent;
  FWorkScope := AWorkScope;
end;

function TNyxExecution.GetWorkScope: INyxWorkScope;
begin
  Result := FWorkScope;
end;

destructor TNyxExecution.Destroy;
begin
  {$IFNDEF PAS2JS}
  DoneCriticalSection(FLock);
  {$ENDIF}
  inherited Destroy;
end;

procedure TNyxExecution.Lock;
begin
  {$IFNDEF PAS2JS}
  EnterCriticalSection(FLock);
  {$ENDIF}
end;

procedure TNyxExecution.Unlock;
begin
  {$IFNDEF PAS2JS}
  LeaveCriticalSection(FLock);
  {$ENDIF}
end;

procedure TNyxExecution.Cancel;
begin
  Lock;
  try

    if FStatus in [nesPending, nesRunning] then
    begin
      FCancelled := True;

      if FStatus = nesPending then
      begin
        FStatus := nesCancelled;
      end;
    end;
  finally
    Unlock;
  end;
end;

function TNyxExecution.GetCancelled: Boolean;
begin
  Lock;
  try
    Result := FCancelled;

    if FParent <> nil then
    begin
      Result := Result or FParent.Cancelled;
    end;
  finally
    Unlock;
  end;
end;

function TNyxExecution.GetStatus: TNyxExecutionStatus;
begin
  Lock;
  try
    Result := FStatus;

    if (Result = nesPending) and (FParent <> nil) and FParent.Cancelled then
    begin
      Result := nesCancelled;
    end;
  finally
    Unlock;
  end;
end;

function TNyxExecution.GetFailure: TNyxText;
begin
  Lock;
  try
    Result := FFailure;
  finally
    Unlock;
  end;
end;

function TNyxExecution.Start: Boolean;
begin
  Lock;
  try

    if (FParent <> nil) and FParent.Cancelled then
    begin
      FCancelled := True;
      FStatus := nesCancelled;
    end;
    Result := FStatus = nesPending;

    if Result then
    begin
      FStatus := nesRunning;
    end;
  finally
    Unlock;
  end;
end;

procedure TNyxExecution.Complete(const AFailure: TNyxText);
begin
  Lock;
  try
    FFailure := AFailure;

    if FCancelled or ((FParent <> nil) and FParent.Cancelled) then
    begin
      FStatus := nesCancelled;
    end
    else if AFailure <> '' then
    begin
      FStatus := nesFailed;
    end
    else
    begin
      FStatus := nesSucceeded;
    end;
  finally
    Unlock;
  end;
end;

constructor TNyxCourier.Create(const AWork: INyxWork; AState: TNyxExecution;
  const AWorkLease: INyxWorkLease);
begin
  inherited Create;
  FWork := AWork;
  FState := AState;
  FExecution := AState;
  FWorkLease := AWorkLease;
end;

destructor TNyxCourier.Destroy;
begin
  try
    { A work finalizer may submit another child. Keep this courier admitted
      through that callout, including the skipped/failed enqueue paths. }
    FWork := nil;
  finally
    FWorkLease := nil;
    inherited Destroy;
  end;
end;

procedure TNyxCourier.Run;
var
  LFailure: TNyxText;
begin

  if FState.Start then
  begin
    LFailure := '';
    try
      FWork.Execute(FExecution);
    except
      on LException: Exception do
      begin
        {$IFDEF PAS2JS}
        LFailure := LException.Message;
        {$ELSE}
        { Exception.Message is an ANSI diagnostic boundary in native FPC. }
        LFailure := UTF8Encode(UnicodeString(LException.Message));
        {$ENDIF}
      end;
    end;
    FState.Complete(LFailure);
  end;
  FWork := nil;
end;

procedure TNyxCourier.Deliver;
begin
  try
    Run;
  finally
    Free;
  end;
end;

{$IFNDEF PAS2JS}
constructor TNyxWorker.Create(ACourier: TNyxCourier);
begin
  inherited Create(True);
  FCourier := ACourier;
  FreeOnTerminate := True;
end;

destructor TNyxWorker.Destroy;
begin
  FCourier.Free;
  inherited Destroy;
end;

procedure TNyxWorker.Execute;
begin

  if FCourier <> nil then
  begin
    FCourier.Run;
  end;
end;

{$I nyx.scheduler.pool.inc}

destructor TNyxNativeUIEntry.Destroy;
var
  LCourier: TNyxCourier;
begin
  { Normal delivery/retirement already detached and cleared the courier. This
    final safeguard neither resurrects Self nor leaves a raw RTL method target. }
  FOwner := nil;
  Unqueue;
  LCourier := FCourier;
  FCourier := nil;
  LCourier.Free;
  inherited Destroy;
end;

procedure TNyxNativeUIEntry.Detach;
var
  LKeep: INyxNativeUIEntry;
  LOwner: TNyxNativeUIQueue;
begin
  LKeep := Self;
  LOwner := FOwner;
  FOwner := nil;

  if LOwner <> nil then
  begin
    LOwner.Forget(LKeep);
  end;
end;

procedure TNyxNativeUIEntry.Unqueue;
begin
  { This RTL operation compares both code and object data, removes only this
    exact target's records, and performs no user callback or courier destruction. }
  TThread.RemoveQueuedEvents(nil, Self.Deliver);
end;

procedure TNyxNativeUIEntry.Retire;
var
  LKeep: INyxNativeUIEntry;
  LCourier: TNyxCourier;
begin
  LKeep := Self;
  LKeep.Detach;
  LKeep.Unqueue;
  LCourier := FCourier;
  FCourier := nil;

  if LCourier <> nil then
  begin
    LCourier.FExecution.Cancel;
    LCourier.Free;
  end;
end;

procedure TNyxNativeUIEntry.Deliver;
var
  LKeep: INyxNativeUIEntry;
  LCourier: TNyxCourier;
begin
  LKeep := Self;
  LKeep.Detach;
  LCourier := FCourier;
  FCourier := nil;

  if LCourier <> nil then
  begin
    { Running work is no longer pending. Reentrant Shutdown can retire siblings
      without freeing this courier; its existing Deliver owns Run + final Free. }
    LCourier.Deliver;
  end;
end;

procedure TNyxNativeUIEntry.Enqueue(ADelay: Integer);
var
  LWorker: TNyxWorker;
begin
  { SubmitAttached pins this entry AND its independent owner before entering
    here. A bootstrap WaitFor can service callbacks just like a Wake hook can. }

  if (FCourier = nil) or (FOwner = nil) then
  begin
    Exit;
  end;
  { FPC 3.2.0 ForceQueue executes inline until the RTL has entered
    multithreaded mode. Complete one empty bootstrap thread before the
    first deferred submission. No user work runs in this bootstrap and
    no global RTL flag is manually changed. Queue from the UI thread:
    old RTL thread destruction removes even unowned queue entries that
    were submitted from the destroyed thread. }

  if not IsMultiThread then
  begin
    LWorker := TNyxWorker.Create(nil);
    LWorker.FreeOnTerminate := False;
    try
      LWorker.Start;
      LWorker.WaitFor;
    finally
      LWorker.Free;
    end;
  end;

  if (FCourier = nil) or (FOwner = nil) then
  begin
    Exit;
  end;
  {$if FPC_FULLVERSION >= 30301}
  TThread.ForceQueue(nil, Self.Deliver, ADelay);
  {$else}
  TThread.ForceQueue(nil, Self.Deliver);
  {$endif}
  { No raw courier or scheduler access follows WakeMainThread. It may already
    have delivered/retired this entry, or may raise after adding its RTL record. }
end;

destructor TNyxNativeUIQueue.Destroy;
begin
  Close;
  inherited Destroy;
end;

function TNyxNativeUIQueue.Adopt(ACourier: TNyxCourier): INyxNativeUIEntry;
var
  LEntry: TNyxNativeUIEntry;
  LLease: INyxNativeUIEntry;
  LCount: Integer;
begin

  if FClosed then
  begin
    raise ENyxSchedule.Create('Native UI queue is closed');
  end;
  LEntry := TNyxNativeUIEntry.Create;
  LLease := LEntry;
  LCount := Length(FEntries);
  SetLength(FEntries, LCount + 1);
  FEntries[LCount] := LLease;
  LEntry.FOwner := Self;
  { All allocation/registration precedes this no-callout ownership transfer.
    An earlier refusal leaves the caller's raw courier independently owned. }
  LEntry.FCourier := ACourier;
  Result := LLease;
end;

procedure TNyxNativeUIQueue.Forget(const AEntry: INyxNativeUIEntry);
var
  LIndex: Integer;
  LMove: Integer;
begin
  for LIndex := 0 to High(FEntries) do
  begin

    if FEntries[LIndex] = AEntry then
    begin
      for LMove := LIndex to High(FEntries) - 1 do
      begin
        FEntries[LMove] := FEntries[LMove + 1];
      end;
      { The caller pins this entry; all other leases remain in the vector.
        Removing membership cannot release user work or enter its destructor. }
      SetLength(FEntries, Length(FEntries) - 1);
      Exit;
    end;
  end;
end;

procedure TNyxNativeUIQueue.Close;
var
  LEntries: TNyxNativeUIEntries;
  LIndex: Integer;
  LFailure: TNyxText;
begin

  if FClosed then
  begin
    Exit;
  end;
  FClosed := True;
  LEntries := FEntries;
  FEntries := nil;
  for LIndex := 0 to High(LEntries) do
  begin
    LEntries[LIndex].Detach;
  end;
  for LIndex := 0 to High(LEntries) do
  begin
    LEntries[LIndex].Unqueue;
  end;
  { All weak links and RTL records are gone before the first destructor can
    pump, shut down again or release the scheduler. Only local leases follow. }
  LFailure := '';
  for LIndex := 0 to High(LEntries) do
  begin
    try
      LEntries[LIndex].Retire;
    except
      on LException: Exception do
      begin
        LFailure := LFailure + ' / ' + UTF8Encode(UnicodeString(LException.Message));
      end;
    end;
  end;

  if LFailure <> '' then
  begin
    raise ENyxSchedule.Create('Native UI retirement failed' + String(LFailure));
  end;
end;
{$ENDIF}

constructor TNyxScheduler.Create(const AOptions: TNyxSchedulerOptions);
begin
  inherited Create;
  AOptions.Validate;
  {$IFNDEF PAS2JS}
  FOptions := AOptions;
  {$ENDIF}
end;

destructor TNyxScheduler.Destroy;
begin
  Shutdown;
  {$IFNDEF PAS2JS}
  FPool := nil;
  {$ENDIF}
  inherited Destroy;
end;

function TNyxScheduler.GetWorkerLoad: TNyxWorkerPoolSnapshot;
begin
  RequireUI;
  Result := Default(TNyxWorkerPoolSnapshot);
  {$IFNDEF PAS2JS}
  Result.WorkerLimit := FOptions.WorkerLimit;
  Result.PendingLimit := FOptions.PendingLimit;

  if FPool <> nil then
  begin
    Result := FPool.Snapshot;
  end;
  {$ENDIF}
  Result.Closed := FClosed;
end;

function TNyxScheduler.GetCapabilities: TNyxSchedulerCapabilities;
begin
  Result := [nscDeferredUI];
  {$IFNDEF PAS2JS}
  Include(Result, nscWorkerThreads);
  {$ENDIF}
end;

procedure TNyxScheduler.RequireUI;
begin
  {$IFNDEF PAS2JS}

  if GetCurrentThreadID <> MainThreadID then
  begin
    raise ENyxSchedule.Create('UI work requires the main thread; use PostUI');
  end;
  {$ENDIF}
end;

procedure TNyxScheduler.Admit(APolicy: TNyxExecutionPolicy);
begin
  RequireUI;

  if FClosed then
  begin
    raise ENyxSchedule.Create('Scheduler is closed');
  end;

  if (APolicy = neThreaded) and not (nscWorkerThreads in GetCapabilities) then
  begin
    raise ENyxSchedule.Create('This scheduler does not support worker threads');
  end;
end;

procedure TNyxScheduler.Prune;
var
  LIndex: Integer;
  LCount: Integer;
  LOriginal: array of INyxExecution;
  LPending: array of INyxExecution;
begin

  if FPruning then
  begin
    Exit;
  end;
  FPruning := True;
  try
    LOriginal := FExecutions;
    SetLength(LPending, Length(LOriginal));
    LCount := 0;
    for LIndex := 0 to High(LOriginal) do
    begin

      if LOriginal[LIndex].Status in [nesPending, nesRunning] then
      begin
        LPending[LCount] := LOriginal[LIndex];
        Inc(LCount);
      end;
    end;
    { A nested submission only appends while FPruning is set; terminal Close
      can only clear the vector. Do not overwrite either change with this older
      candidate. Original local leases also prevent finalizers during adoption. }

    if not FClosed and (Length(FExecutions) = Length(LOriginal)) then
    begin
      SetLength(LPending, LCount);
      FExecutions := LPending;
    end;
  finally
    FPruning := False;
  end;
end;

function TNyxScheduler.Submit(const AWork: INyxWork;
  APolicy: TNyxExecutionPolicy): INyxExecution;
begin
  Result := SubmitAttached(AWork, APolicy, nil);
end;

function TNyxScheduler.SubmitAttached(const AWork: INyxWork;
  APolicy: TNyxExecutionPolicy; const AParent: INyxExecution; ADelay: Integer): INyxExecution;
var
  LState: TNyxExecution;
  LCourier: TNyxCourier;
  LImmediate: TNyxCourier;
  LSchedulerLease: INyxScheduler;
  LWork: INyxWork;
  LParent: INyxExecution;
  LParentScope: INyxExecutionWorkScope;
  LWorkScope: INyxWorkScope;
  LWorkLease: INyxWorkLease;
  {$IFNDEF PAS2JS}
  LUIQueue: INyxNativeUIQueue;
  LUIEntry: INyxNativeUIEntry;
  {$ENDIF}
begin
  LSchedulerLease := Self;
  LWork := AWork;
  LParent := AParent;
  Admit(APolicy);

  if LWork = nil then
  begin
    raise ENyxSchedule.Create('Scheduled work is required');
  end;
  { Scope discovery is an extension callout. Capture original inputs first;
    final scheduler admission follows discovery/acquisition, before adoption. }

  if Supports(LParent, INyxExecutionWorkScope, LParentScope) then
  begin
    LWorkScope := LParentScope.WorkScope;
  end;

  if LWorkScope <> nil then
  begin
    LWorkLease := LWorkScope.Acquire;
  end;
  Admit(APolicy);
  Prune;
  Admit(APolicy);
  LState := TNyxExecution.Create(LParent, LWorkScope);
  Result := LState;
  SetLength(FExecutions, Length(FExecutions) + 1);
  FExecutions[High(FExecutions)] := Result;
  LCourier := TNyxCourier.Create(LWork, LState, LWorkLease);
  LWorkLease := nil;
  LWork := nil;
  try

    if APolicy = neSequential then
    begin
      LImmediate := LCourier;
      LCourier := nil;
      LImmediate.Deliver;
    end
    else
    begin
      {$IFDEF PAS2JS}
      window.setTimeout(@LCourier.Deliver, ADelay);
      {$ELSE}

      if APolicy = neUIQueue then
      begin
        if FUIQueue = nil then
        begin
          FUIQueue := TNyxNativeUIQueue.Create;
        end;
        LUIQueue := FUIQueue;
        LUIEntry := LUIQueue.Adopt(LCourier);
        LCourier := nil;
        { Ownership is managed before bootstrap/enqueue can call outward. Wake
          can deliver, retire or raise without invalidating either local lease. }
        LUIEntry.Enqueue(ADelay);
      end
      else
      begin

        if FPool = nil then
        begin
          FPool := TNyxWorkerQueue.Create(FOptions);
          try
            FPool.StartWorkers;
          except
            FPool := nil;
            raise;
          end;
        end;
        { Adoption occurs only after capacity admission. Saturation preserves
          caller-owned work and the UI never runs a refused worker submission. }
        FPool.Push(LCourier);
      end;
      {$ENDIF}
    end;
    { Deferred ownership is now held by the platform entry/worker; synchronous
      delivery has already released the courier. }
    LCourier := nil;
  except
    {$IFNDEF PAS2JS}

    if LUIEntry <> nil then
    begin
      Result.Cancel;
      LUIEntry.Retire;
    end;
    {$ENDIF}
    LCourier.Free;
    Result.Cancel;
    raise;
  end;
end;

procedure TNyxScheduler.Shutdown;
var
  LIndex: Integer;
  {$IFNDEF PAS2JS}
  LUIQueue: INyxNativeUIQueue;
  LPool: INyxWorkerQueue;
  LExecutions: array of INyxExecution;
  {$ENDIF}
begin

  if FClosed then
  begin
    Exit;
  end;
  FClosed := True;
  {$IFNDEF PAS2JS}
  LUIQueue := FUIQueue;
  FUIQueue := nil;
  LPool := FPool;
  { Completed executions can still retain a caller-supplied parent context.
    Detach the WHOLE vector, retaining those leases through queue retirement;
    clearing the field must not invoke that parent's destructor mid-shutdown. }
  LExecutions := FExecutions;
  FExecutions := nil;
  for LIndex := 0 to High(LExecutions) do
  begin
    LExecutions[LIndex].Cancel;
  end;
  {$ELSE}
  for LIndex := 0 to Length(FExecutions) - 1 do
  begin
    FExecutions[LIndex].Cancel;
  end;
  SetLength(FExecutions, 0);
  {$ENDIF}
  {$IFNDEF PAS2JS}

  { Finalize scheduler-owned fields before user work can release the last
    scheduler reference. Independent local owners suffice afterward, including
    Destroy -> Shutdown; retaining Self there would resurrect a dying object. }
  try

    if LUIQueue <> nil then
    begin
      LUIQueue.Close;
    end;
  finally

    if LPool <> nil then
    begin
      LPool.Close;
    end;
  end;
  {$ENDIF}
end;

{$IFNDEF PAS2JS}
procedure TNyxUISubmission.Submit;
begin
  Execution := Scheduler.PostUI(Work, Parent);
end;
{$ENDIF}

function TNyxScheduler.PostUI(const AWork: INyxWork;
  const AParent: INyxExecution): INyxExecution;
{$IFNDEF PAS2JS}
var
  LSubmission: TNyxUISubmission;
{$ENDIF}
begin
  {$IFNDEF PAS2JS}

  if GetCurrentThreadID <> MainThreadID then
  begin
    LSubmission := TNyxUISubmission.Create;
    try
      LSubmission.Scheduler := Self;
      LSubmission.Work := AWork;
      LSubmission.Parent := AParent;
      { Accept on the UI thread so the RTL queue entry has UI identity and is
        not removed when the submitting worker is destroyed in older FPC. }
      TThread.Synchronize(nil, LSubmission.Submit);
      Exit(LSubmission.Execution);
    finally
      LSubmission.Free;
    end;
  end;
  {$ENDIF}
  Result := SubmitAttached(AWork, neUIQueue, AParent);
end;

function NewNyxScheduler: INyxScheduler;
begin
  Result := NewNyxScheduler(TNyxSchedulerOptions.Defaults);
end;

function TNyxScheduler.GetCanYieldUI: Boolean;
begin
  Result := {$ifdef PAS2JS}True{$else}
    {$if FPC_FULLVERSION >= 30301}True{$else}False{$endif}{$endif};
end;

function TNyxScheduler.PostTurn(const AWork: INyxWork): INyxExecution;
begin
  RequireUI;

  if not GetCanYieldUI then
  begin
    raise ENyxSchedule.Create('This native RTL does not support cooperative UI turns');
  end;
  Result := SubmitAttached(AWork, neUIQueue, nil, 1);
end;

function TNyxScheduler.PostAfter(const AWork: INyxWork;
  ADelayMilliseconds: Integer): INyxExecution;
begin
  RequireUI;

  if not GetCanYieldUI then
  begin
    raise ENyxSchedule.Create('This native RTL does not support delayed UI turns');
  end;

  if (ADelayMilliseconds < 1) or (ADelayMilliseconds > 60000) then
  begin
    raise ENyxSchedule.Create('UI delay must be between 1 and 60000 milliseconds');
  end;
  Result := SubmitAttached(AWork, neUIQueue, nil, ADelayMilliseconds);
end;

function NyxSchedulerDelays(const AScheduler: INyxScheduler): INyxSchedulerDelays;
var
  LTurns: INyxSchedulerTurns;
begin
  LTurns := NyxSchedulerTurns(AScheduler);

  if not Supports(AScheduler, INyxSchedulerDelays, Result) then
  begin
    raise ENyxSchedule.Create('Scheduler adapter does not support delayed UI turns');
  end;
end;

function NyxSchedulerTurns(const AScheduler: INyxScheduler): INyxSchedulerTurns;
begin

  if AScheduler = nil then
  begin
    raise ENyxSchedule.Create('Cooperative UI turns require a scheduler');
  end;
  AScheduler.RequireUI;

  if not Supports(AScheduler, INyxSchedulerTurns, Result) then
  begin
    raise ENyxSchedule.Create('Scheduler adapter does not provide cooperative UI turns');
  end;

  if not Result.CanYieldUI then
  begin
    raise ENyxSchedule.Create('Scheduler adapter does not support cooperative UI turns');
  end;
end;

function NewNyxScheduler(const AOptions: TNyxSchedulerOptions): INyxScheduler;
begin
  Result := TNyxScheduler.Create(AOptions);
end;

function NewNyxFailedExecution(const AFailure: TNyxText): INyxExecution;
var
  LState: TNyxExecution;
begin

  if AFailure = '' then
  begin
    raise ENyxSchedule.Create('A failed execution requires its diagnostic');
  end;
  LState := TNyxExecution.Create(nil);
  Result := LState;
  LState.Start;
  LState.Complete(AFailure);
end;

end.
