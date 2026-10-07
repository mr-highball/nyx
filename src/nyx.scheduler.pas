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

{ Platform implementation, with no dependency on the document, DOM controls or
  LCL controls. Browser workers require an explicit separate transport/program;
  ordinary Pascal callbacks are deferred on its UI event loop. }
function NewNyxScheduler: INyxScheduler; overload;
function NewNyxScheduler(const AOptions: TNyxSchedulerOptions): INyxScheduler; overload;
{ Adapter admission failures can expose a terminal diagnostic without scheduling
  a callback. The token owns exact text; empty diagnostics refuse. This does not
  convert an unsupported policy into supported execution or perform any work. }
function NewNyxFailedExecution(const AFailure: TNyxText): INyxExecution;

implementation

uses
  {$IFDEF PAS2JS}
  Web;
  {$ELSE}
  Classes,
  SyncObjs;
  {$ENDIF}

type
  TNyxExecution = class(TInterfacedObject, INyxExecution)
  private
    FCancelled: Boolean;
    FStatus: TNyxExecutionStatus;
    FFailure: TNyxText;
    FParent: INyxExecution;
    {$IFNDEF PAS2JS}
    FLock: TRTLCriticalSection;
    {$ENDIF}
    procedure Lock;
    procedure Unlock;
  public
    constructor Create(const AParent: INyxExecution);
    destructor Destroy; override;
    procedure Cancel;
    function GetCancelled: Boolean;
    function GetStatus: TNyxExecutionStatus;
    function GetFailure: TNyxText;
    function Start: Boolean;
    procedure Complete(const AFailure: TNyxText);
  end;

  { A courier owns every input. Its queued method is the sole raw self-reference;
    it releases itself only after the queue has invoked that method. }
  TNyxCourier = class
  private
    FWork: INyxWork;
    FExecution: INyxExecution;
    FState: TNyxExecution;
  public
    constructor Create(const AWork: INyxWork; AState: TNyxExecution);
    procedure Run;
    procedure Deliver;
  end;

  {$IFNDEF PAS2JS}
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

  TNyxScheduler = class(TInterfacedObject, INyxScheduler, INyxSchedulerMonitor)
  private
    FClosed: Boolean;
    {$IFNDEF PAS2JS}
    FOptions: TNyxSchedulerOptions;
    FPool: INyxWorkerQueue;
    {$ENDIF}
    FExecutions: array of INyxExecution;
    procedure Prune;
    function SubmitAttached(const AWork: INyxWork; APolicy: TNyxExecutionPolicy;
      const AParent: INyxExecution): INyxExecution;
  public
    constructor Create(const AOptions: TNyxSchedulerOptions);
    destructor Destroy; override;
    function GetWorkerLoad: TNyxWorkerPoolSnapshot;
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

constructor TNyxExecution.Create(const AParent: INyxExecution);
begin
  inherited Create;
  {$IFNDEF PAS2JS}
  InitCriticalSection(FLock);
  {$ENDIF}
  FStatus := nesPending;
  FParent := AParent;
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

constructor TNyxCourier.Create(const AWork: INyxWork; AState: TNyxExecution);
begin
  inherited Create;
  FWork := AWork;
  FState := AState;
  FExecution := AState;
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
begin
  LCount := 0;
  for LIndex := 0 to Length(FExecutions) - 1 do
  begin

    if FExecutions[LIndex].Status in [nesPending, nesRunning] then
    begin
      FExecutions[LCount] := FExecutions[LIndex];
      Inc(LCount);
    end;
  end;
  SetLength(FExecutions, LCount);
end;

function TNyxScheduler.Submit(const AWork: INyxWork;
  APolicy: TNyxExecutionPolicy): INyxExecution;
begin
  Result := SubmitAttached(AWork, APolicy, nil);
end;

function TNyxScheduler.SubmitAttached(const AWork: INyxWork;
  APolicy: TNyxExecutionPolicy; const AParent: INyxExecution): INyxExecution;
var
  LState: TNyxExecution;
  LCourier: TNyxCourier;
  LImmediate: TNyxCourier;
  {$IFNDEF PAS2JS}
  LWorker: TNyxWorker;
  {$ENDIF}
begin
  Admit(APolicy);

  if AWork = nil then
  begin
    raise ENyxSchedule.Create('Scheduled work is required');
  end;
  Prune;
  LState := TNyxExecution.Create(AParent);
  Result := LState;
  SetLength(FExecutions, Length(FExecutions) + 1);
  FExecutions[High(FExecutions)] := Result;
  LCourier := TNyxCourier.Create(AWork, LState);
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
      window.setTimeout(@LCourier.Deliver, 0);
      {$ELSE}

      if APolicy = neUIQueue then
      begin
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
        TThread.ForceQueue(nil, LCourier.Deliver);
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
    { Deferred ownership is now held by the platform queue/worker; synchronous
      delivery has already released the courier. }
    LCourier := nil;
  except
    LCourier.Free;
    Result.Cancel;
    raise;
  end;
end;

procedure TNyxScheduler.Shutdown;
var
  LIndex: Integer;
begin

  if FClosed then
  begin
    Exit;
  end;
  FClosed := True;
  for LIndex := 0 to Length(FExecutions) - 1 do
  begin
    FExecutions[LIndex].Cancel;
  end;
  SetLength(FExecutions, 0);
  {$IFNDEF PAS2JS}

  if FPool <> nil then
  begin
    FPool.Close;
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
