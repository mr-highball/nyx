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
unit nyx.test.scheduler;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  nyx.text;

{ Portable synchronous dispatch checks and a real target queue/worker journey.
  Async completion is signalled through an owned callback so the browser host
  can wait without claiming success merely from loading its script. }
function RunNyxSchedulerTests: Integer;
type
  TNyxScheduleTestCompletion = procedure(ACount: Integer; const AFailure: TNyxText) of object;
  TNyxScheduleJourney = class
  private
    FDone: TNyxScheduleTestCompletion;
    FChecks: Integer;
    FPolls: Integer;
    FState: TObject;
    procedure Poll;
    procedure Finish(const AFailure: TNyxText);
  public
    constructor Create(ADone: TNyxScheduleTestCompletion);
    destructor Destroy; override;
    procedure Start;
  end;

implementation

uses
  SysUtils,
  {$if defined(MSWINDOWS) and not defined(PAS2JS)}Windows,{$endif}
  nyx.types,
  nyx.data,
  nyx.behavior,
  nyx.state,
  nyx.model,
  nyx.events,
  nyx.scheduler,
  {$IFDEF PAS2JS}
  Web;
  {$ELSE}
  Classes;
  {$ENDIF}

type
  { Caller-owned facts outlive every tested receiver, including failed
    construction. The callback borrows them without creating an ownership cycle. }
  TConstructionLeaseFacts = class
  public
    Calls: Integer;
    Constructed: Integer;
    Destroyed: Integer;
  end;

  { A constructor deliberately exercises entered-call Self leases before any
    external interface owner exists. It omits inherited Create, proving the
    automatic construction boundary also protects existing descendant patterns.
    A second constructor publishes a caller owner before that boundary returns. }
  TConstructionLeaseCallback = class(TNyxEventCallback)
  private
    FFacts: TConstructionLeaseFacts;
    procedure TemporaryLease;
  public
    constructor Create(AFacts: TConstructionLeaseFacts; AFail: Boolean);
    constructor CreateRetained(AFacts: TConstructionLeaseFacts;
      out AOwner: INyxEventCallback);
    destructor Destroy; override;
    procedure Invoke(const AEvent: TNyxEventInfo;
      const AExecution: INyxExecution); override;
  end;

  { Independent fixture facts outlive the counted delivery scope and are never
    retained by a router, scheduler or callback after this isolated journey. }
  TScopeLeaseFacts = class
  public
    Destroyed: Integer;
  end;

  { A UI-only certified delivery source implements both owning scope facets.
    Destruction is counted without retaining its caller or inventing cancellation
    from reference count. Exact lifetime is tested through real guarded dispatch. }
  TScopeLeaseProbe = class(TInterfacedObject, INyxCancellationScope,
    INyxCancellationReadiness)
  private
    FFacts: TScopeLeaseFacts;
    FCancelled: Boolean;
  public
    constructor Create(AFacts: TScopeLeaseFacts);
    destructor Destroy; override;
    procedure Cancel;
    function GetCancelled: Boolean;
    procedure CheckReady;
  end;

  TProbe = class(TNyxEventCallback)
  public
    Calls: Integer;
    Last: TNyxEventInfo;
    Failure: Boolean;
    CancelOther: INyxEventSubscription;
    CancelSelf: INyxEventSubscription;
    AddTo: INyxEventStream;
    Addition: INyxEventCallback;
    Router: INyxEvents;
    Reenter: Boolean;
    Invalidate: Boolean;
    SawCancellation: Boolean;
    RetainedContext: INyxExecution;
    ConsumeInput: Boolean;
    SawCanConsume: Boolean;
    SawConsumed: Boolean;
    Nested: TNyxEventInfo;
    {$IFNDEF PAS2JS}
    ThreadID: TThreadID;
    UIWork: INyxWork;
    Scheduler: INyxScheduler;
    UITicket: INyxExecution;
    {$ENDIF}
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution); override;
  end;

  {$IFNDEF PAS2JS}
  { Real worker cancellation proof. Only atomic flags cross this boundary;
    no test waits on an unsynchronized mutable Pascal string or UI object. }
  TCooperativeCallback = class(TNyxEventCallback)
  public
    Started: LongInt;
    Stopped: LongInt;
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution); override;
  end;
  {$ENDIF}

  TJourneyState = class
  public
    Router: INyxEvents;
    Stream: INyxEventStream;
    First: INyxEventSubscription;
    Cancelled: INyxEventSubscription;
    Pending: TNyxExecutions;
    Probe: TProbe;
    ProbeLease: INyxEventCallback;
    SkipProbe: TProbe;
    SkipLease: INyxEventCallback;
    FaultProbe: TProbe;
    FaultLease: INyxEventCallback;
    FaultTicket: INyxExecution;
    AsyncProbe: TProbe;
    AsyncLease: INyxEventCallback;
    AsyncTicket: INyxExecution;
    QueuedKeyProbe: TProbe;
    QueuedKeyLease: INyxEventCallback;
    QueuedKeyTicket: INyxExecution;
    AsyncKeyProbe: TProbe;
    AsyncKeyLease: INyxEventCallback;
    AsyncKeyTicket: INyxExecution;
    {$IFNDEF PAS2JS}
    WorkerProbe: TProbe;
    WorkerLease: INyxEventCallback;
    WorkerTicket: INyxExecution;
    MainThreadID: TThreadID;
    UIProbe: TProbe;
    UILease: INyxEventCallback;
    Cooperative: TCooperativeCallback;
    CooperativeLease: INyxEventCallback;
    CooperativeRegistration: INyxEventSubscription;
    CooperativeTicket: INyxExecution;
    {$ENDIF}
    destructor Destroy; override;
  end;

  {$IFNDEF PAS2JS}
  TUIProbeWork = class(TInterfacedObject, INyxWork)
  private
    FProbe: INyxEventCallback;
    FEvent: TNyxEventInfo;
  public
    constructor Create(const AProbe: INyxEventCallback; const AEvent: TNyxEventInfo);
    procedure Execute(const AExecution: INyxExecution);
  end;

constructor TUIProbeWork.Create(const AProbe: INyxEventCallback; const AEvent: TNyxEventInfo);
begin
  inherited Create;
  FProbe := AProbe;
  FEvent := AEvent.Copy;
end;

procedure TUIProbeWork.Execute(const AExecution: INyxExecution);
begin
  FProbe.Invoke(FEvent, AExecution);
end;

procedure TCooperativeCallback.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
begin
  InterlockedExchange(Started, 1);
  while not AExecution.Cancelled do
  begin
    Sleep(1);
  end;
  InterlockedExchange(Stopped, 1);
end;
  {$ENDIF}

procedure Check(ACondition: Boolean; const AMessage: TNyxText; var ACount: Integer);
begin

  if not ACondition then
  begin
    raise ENyxModel.Create('FAIL scheduler: ' + AMessage);
  end;
  Inc(ACount);
end;

function EventInfo: TNyxEventInfo;
begin
  { Explicit initialization keeps unused new snapshot families absent. }
  Result := Default(TNyxEventInfo);
  Result.Trigger := ntAfterEnter;
  Result.Name := NyxEvent('after-enter');
  Result.OriginID := 'runtime/reply';
  Result.SourceID := 'runtime/thread';
  Result.TargetID := 'runtime/reply';
  Result.ValueID := 'runtime/reply';
  Result.Value := NyxData(TNyxText('Owned reply / 🌙'));
  Result.ValueKind := nskText;
  Result.HasValue := True;
  Result.Changed := False;
  Result.HasKeyboard := False;
  Result.Keyboard := NyxKeyStroke(nkUnknownKey);
end;

procedure TProbe.Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution);
var
  LOnce: Boolean;
begin
  Inc(Calls);
  RetainedContext := AExecution;
  Last := AEvent.Copy;

  if AEvent.HasKeyboard then
  begin
    SawCanConsume := NyxEventResponse(AExecution).CanConsume;
    SawConsumed := NyxEventResponse(AExecution).Consumed;
  end;
  {$IFNDEF PAS2JS}
  ThreadID := GetCurrentThreadID;

  if UIWork <> nil then
  begin
    UITicket := Scheduler.PostUI(UIWork, AExecution);
  end;
  {$ENDIF}

  if CancelOther <> nil then
  begin
    CancelOther.Cancel;
  end;

  if CancelSelf <> nil then
  begin
    CancelSelf.Cancel;
    SawCancellation := AExecution.Cancelled;
  end;

  if Addition <> nil then
  begin
    AddTo.Subscribe(Addition);
    Addition := nil;
  end;
  LOnce := Reenter;
  Reenter := False;

  if LOnce then
  begin
    Router.Dispatch(Nested, 'reply', 'thread');
  end;

  if Invalidate then
  begin
    Router.CancelPending;
    SawCancellation := AExecution.Cancelled;
  end;

  if ConsumeInput then
  begin
    NyxEventResponse(AExecution).Consume;
  end;

  if Failure then
  begin
    raise ENyxModel.Create('Intentional callback failure / 🌙');
  end;
end;

destructor TJourneyState.Destroy;
begin

  if Router <> nil then
  begin
    Router.Close;
  end;
  inherited Destroy;
end;

function RunNyxKeyboardResponseTests: Integer;
const
  CBrowserKeys: array[0..10] of TNyxText = ('a', 'Z', '4', 'F12', 'ArrowLeft',
    'Escape', '!', 'é', 'Process', 'F01', 'F25');
  CKeys: array[0..10] of TNyxKey = (nkAKey, nkZKey, nk4Key, nkF12Key,
    nkLeftKey, nkEscapeKey, nkUnknownKey, nkUnknownKey, nkUnknownKey,
    nkUnknownKey, nkUnknownKey);
var
  LRouter: INyxEvents;
  LFirst: TProbe;
  LSecond: TProbe;
  LFirstLease: INyxEventCallback;
  LSecondLease: INyxEventCallback;
  LFirstToken: INyxEventSubscription;
  LSecondToken: INyxEventSubscription;
  LInfo: TNyxEventInfo;
  LStroke: TNyxKeyStroke;
  LConsumed: Boolean;
  LRejected: Boolean;
  LIndex: Integer;
begin
  Result := 0;
  for LIndex := Low(CBrowserKeys) to High(CBrowserKeys) do
  begin
    Check(NyxKeyFromBrowser(CBrowserKeys[LIndex]) = CKeys[LIndex],
      'logical key admission: ' + CBrowserKeys[LIndex], Result);
  end;
  Check((NyxKeyFromVirtualCode($0D) = nkEnterKey) and
    (NyxKeyFromVirtualCode($7B) = nkF12Key) and
    (NyxKeyFromVirtualCode($FFFF) = nkUnknownKey),
    'native key boundary retains known and unknown identities', Result);
  LStroke := NyxKeyStroke(nkEnterKey, [nmControl]);
  Check(LStroke.Matches(nkEnterKey, [nmControl]) and
    not LStroke.Matches(nkEnterKey, []) and
    not LStroke.Matches(nkEnterKey, [nmControl, nmAltGraph]),
    'typed shortcuts require exact modifiers, including AltGraph', Result);
  LStroke := NyxKeyStroke(nkEnterKey, [nmControl], True);
  Check(not LStroke.Matches(nkEnterKey, [nmControl]) and
    LStroke.Matches(nkEnterKey, [nmControl], True),
    'shortcut repeats require explicit admission', Result);
  LRouter := NewNyxEvents;
  LFirst := TProbe.Create;
  LFirstLease := LFirst;
  LSecond := TProbe.Create;
  LSecondLease := LSecond;
  try
    LFirst.ConsumeInput := True;
    LFirstToken := LRouter.OnKeyDown(NyxControlEvents('reply')).Subscribe(LFirstLease);
    LSecondToken := LRouter.OnKeyDown(NyxControlEvents('reply')).Subscribe(LSecondLease);
    LInfo := EventInfo;
    LInfo.Trigger := ntKeyDown;
    LInfo.Name := NyxEvent(NyxTriggerName(ntKeyDown));
    LInfo.HasKeyboard := True;
    LInfo.Keyboard := NyxKeyStroke(nkEnterKey, [nmControl]);
    LRouter.DispatchInput(LInfo, 'reply', 'thread', LConsumed);
    Check(LConsumed and (LFirst.Calls = 1) and (LSecond.Calls = 1) and
      LFirst.SawCanConsume and LSecond.SawConsumed,
      'sequential consumption is visible to ordered siblings without suppressing them', Result);
    Check(LSecond.Last.HasKeyboard and
      LSecond.Last.Keyboard.Matches(nkEnterKey, [nmControl]),
      'callbacks retain an owned typed keyboard snapshot', Result);
    Check(not NyxEventResponse(LFirst.RetainedContext).CanConsume and
      NyxEventResponse(LFirst.RetainedContext).Consumed,
      'returned invocation seals its response while preserving the decision', Result);
    LRejected := False;
    try
      NyxEventResponse(LFirst.RetainedContext).Consume;
    except
      on ENyxSchedule do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'a retained response cannot consume a later platform event', Result);
    LFirst.Failure := True;
    LRouter.DispatchInput(LInfo, 'reply', 'thread', LConsumed);
    Check(LConsumed and (LFirstToken.LastExecution.Status = nesFailed) and
      (LSecond.Calls = 2) and (LSecondToken.LastExecution.Status = nesSucceeded),
      'failure after consumption retains the decision and runs siblings', Result);
    LFirst.Failure := False;
    LFirst.ConsumeInput := False;
    LRouter.Dispatch(LInfo, 'reply', 'thread');
    Check(not LFirst.SawCanConsume and not LSecond.SawConsumed,
      'ordinary signal dispatch supplies no physical consumption window', Result);
    LFirst.ConsumeInput := True;
    LFirst.CancelSelf := LFirstToken;
    LRouter.DispatchInput(LInfo, 'reply', 'thread', LConsumed);
    Check(not LConsumed and not LFirstToken.Active and (LSecond.Calls = 4),
      'self-cancelled callbacks cannot consume the input or suppress siblings', Result);
    LInfo.HasKeyboard := False;
    LRejected := False;
    try
      LRouter.DispatchInput(LInfo, 'reply', 'thread', LConsumed);
    except
      on ENyxSchedule do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LSecond.Calls = 4),
      'input admission rejects a missing keyboard payload before callbacks', Result);
  finally
    LFirst.CancelSelf := nil;
    LRouter.Close;
  end;
end;

{$ifndef PAS2JS}
type
  PNativeQueueScheduler = ^INyxScheduler;

  { Managed facts outlive any refused/undelivered work even if a check fails.
    The scheduler slot and sibling are borrowed only during this helper; its
    finally block revokes those pointers before the helper's stack unwinds. }
  TNativeQueueFacts = class(TInterfacedObject)
  public
    Calls, Destroyed, Reentries: Integer;
    ParentDestroyed: Integer;
    SawRunningCancellation, SawRunningLifetime: Boolean;
    ParentSawRetirement, ParentCallouts: Boolean;
    ShutdownOnRun, DropSchedulerOnDestroy, PumpOnDestroy: Boolean;
    Scheduler: INyxScheduler;
    SchedulerSlot: PNativeQueueScheduler;
    Sibling: TNativeQueueFacts;
    Ticket: INyxExecution;
    procedure Reset;
  end;

  TNativeQueueWork = class(TInterfacedObject, INyxWork)
  private
    FFacts: TNativeQueueFacts;
    FFactsLease: IInterface;
  public
    constructor Create(AFacts: TNativeQueueFacts);
    destructor Destroy; override;
    procedure Execute(const AExecution: INyxExecution);
  end;

  { An application-supplied parent forwards a real canonical execution. Its
    destructor is an ordinary outward lifetime boundary, independently of the
    private child execution that retains it after that child has completed. }
  TNativeQueueParent = class(TInterfacedObject, INyxExecution)
  private
    FInner: INyxExecution;
    FFacts: TNativeQueueFacts;
    FFactsLease: IInterface;
  public
    constructor Create(const AInner: INyxExecution; AFacts: TNativeQueueFacts);
    destructor Destroy; override;
    procedure Cancel;
    function GetCancelled: Boolean;
    function GetStatus: TNyxExecutionStatus;
    function GetFailure: TNyxText;
  end;

  TNativeQueueWakeMode = (nqwRaise, nqwShutdown, nqwDeliverRaise);
  { This is a scoped simulation of the RTL's ordinary WakeMainThread callout,
    not a scheduler probe or replacement queue. Only the UI-thread one-shot
    branch touches test state; other notifications retain the former receiver. }
  TNativeQueueWake = class
  public
    Previous: TNotifyEvent;
    Scheduler: INyxScheduler;
    Mode: TNativeQueueWakeMode;
    Armed: Boolean;
    Calls: Integer;
    procedure Notify(ASender: TObject);
  end;

const
  CNativeQueueWakeRefusal = 'Intentional native queue wake refusal';

procedure TNativeQueueFacts.Reset;
begin
  Scheduler := nil;
  Ticket := nil;
  Calls := 0;
  Destroyed := 0;
  Reentries := 0;
  ParentDestroyed := 0;
  SawRunningCancellation := False;
  SawRunningLifetime := False;
  ParentSawRetirement := False;
  ParentCallouts := False;
  ShutdownOnRun := False;
  DropSchedulerOnDestroy := False;
  PumpOnDestroy := False;
  SchedulerSlot := nil;
  Sibling := nil;
end;

constructor TNativeQueueWork.Create(AFacts: TNativeQueueFacts);
begin
  inherited Create;
  FFacts := AFacts;
  FFactsLease := AFacts;
end;

destructor TNativeQueueWork.Destroy;
var
  LScheduler: INyxScheduler;
begin
  Inc(FFacts.Destroyed);

  if FFacts.DropSchedulerOnDestroy and (FFacts.SchedulerSlot <> nil) then
  begin
    { Drop the caller's last public slot during the outer Shutdown cleanup.
      This local lease permits reentry, then releases before outer Shutdown
      resumes; its implementation may no longer read a destroyed scheduler. }
    LScheduler := FFacts.SchedulerSlot^;
    FFacts.SchedulerSlot^ := nil;

    if LScheduler <> nil then
    begin
      Inc(FFacts.Reentries);
      LScheduler.Shutdown;
    end;
  end;

  if FFacts.PumpOnDestroy then
  begin
    { An adversarial destructor may service the platform queue. Every pending
      sibling target must already be removed, not merely execution-cancelled. }
    CheckSynchronize(0);
  end;
  inherited Destroy;
end;

procedure TNativeQueueWork.Execute(const AExecution: INyxExecution);
begin
  Inc(FFacts.Calls);
  FFacts.Ticket := AExecution;

  if FFacts.ShutdownOnRun then
  begin
    FFacts.Scheduler.Shutdown;
    FFacts.SawRunningCancellation := AExecution.Cancelled;
    FFacts.SawRunningLifetime := (FFacts.Destroyed = 0) and
      (FFacts.Sibling <> nil) and (FFacts.Sibling.Destroyed = 1);
  end;
end;

constructor TNativeQueueParent.Create(const AInner: INyxExecution;
  AFacts: TNativeQueueFacts);
begin
  inherited Create;
  FInner := AInner;
  FFacts := AFacts;
  FFactsLease := AFacts;
end;

destructor TNativeQueueParent.Destroy;
begin
  Inc(FFacts.ParentDestroyed);
  FFacts.ParentSawRetirement := (FFacts.Sibling <> nil) and
    (FFacts.Sibling.Calls = 0) and (FFacts.Sibling.Destroyed = 1);

  if FFacts.ParentCallouts then
  begin

    if FFacts.Scheduler <> nil then
    begin
      FFacts.Scheduler.Shutdown;
    end;
    CheckSynchronize(0);
  end;
  inherited Destroy;
end;

procedure TNativeQueueParent.Cancel;
begin
  FInner.Cancel;
end;

function TNativeQueueParent.GetCancelled: Boolean;
begin
  Result := FInner.Cancelled;
end;

function TNativeQueueParent.GetStatus: TNyxExecutionStatus;
begin
  Result := FInner.Status;
end;

function TNativeQueueParent.GetFailure: TNyxText;
begin
  Result := FInner.Failure;
end;

procedure TNativeQueueWake.Notify(ASender: TObject);
begin

  if GetCurrentThreadID = MainThreadID then
  begin

    if Armed then
    begin
      Armed := False;
      Inc(Calls);
      case Mode of
        nqwRaise:
          raise ENyxSchedule.Create(CNativeQueueWakeRefusal);
        nqwShutdown:
          Scheduler.Shutdown;
        nqwDeliverRaise:
          begin
            CheckSynchronize(0);
            raise ENyxSchedule.Create(CNativeQueueWakeRefusal);
          end;
      end;
    end;
  end;

  if Assigned(Previous) then
  begin
    Previous(ASender);
  end;
end;

{ Immediate destruction is checked BEFORE a pump. Later CheckSynchronize calls
  test non-revival/double-release, never supply missing cleanup to a passing
  assertion. No original journey wait or RTL bootstrap flag is changed. }
function RunNativeQueueRetirementTests: Integer;
var
  LScheduler, LPeer: INyxScheduler;
  LWork, LOtherWork: INyxWork;
  LTicket, LOtherTicket: INyxExecution;
  LParent, LInner: INyxExecution;
  LTurns: INyxSchedulerTurns;
  LDelays: INyxSchedulerDelays;
  LFirst, LSecond: TNativeQueueFacts;
  LFirstLease, LSecondLease: IInterface;
  LHook: TNativeQueueWake;
  LPrevious: TNotifyEvent;
  LRejected: Boolean;

  procedure NextCase;
  begin
    LWork := nil;
    LOtherWork := nil;
    LParent := nil;
    LInner := nil;
    LTurns := nil;
    LDelays := nil;

    if LScheduler <> nil then
    begin
      LScheduler.Shutdown;
    end;
    LScheduler := nil;

    if LPeer <> nil then
    begin
      LPeer.Shutdown;
    end;
    LPeer := nil;
    LTicket := nil;
    LOtherTicket := nil;
    LFirst.Reset;
    LSecond.Reset;
    LScheduler := NewNyxScheduler;
  end;

  procedure SubmitWithWake(AMode: TNativeQueueWakeMode);
  begin
    LHook.Mode := AMode;
    LHook.Calls := 0;
    LHook.Scheduler := LScheduler;
    LHook.Armed := True;
    LRejected := False;
    WakeMainThread := LHook.Notify;
    try
      try
        LTicket := LScheduler.Submit(LWork, neUIQueue);
      except
        on LException: ENyxSchedule do
        begin

          if LException.Message <> CNativeQueueWakeRefusal then
          begin
            raise;
          end;
          LRejected := True;
        end;
      end;
    finally
      WakeMainThread := LPrevious;
      LHook.Armed := False;
      LHook.Scheduler := nil;
    end;
    LWork := nil;
  end;

begin
  Result := 0;
  LFirst := TNativeQueueFacts.Create;
  LFirstLease := LFirst;
  LSecond := TNativeQueueFacts.Create;
  LSecondLease := LSecond;
  LHook := TNativeQueueWake.Create;
  LPrevious := WakeMainThread;
  LHook.Previous := LPrevious;
  try
    NextCase;
    LWork := TNativeQueueWork.Create(LFirst);
    LTicket := LScheduler.Submit(LWork, neUIQueue);
    LWork := nil;
    Check((LFirst.Calls = 0) and (LFirst.Destroyed = 0),
      'pending native UI work is owned without entering inline', Result);
    LScheduler.Shutdown;
    Check((LFirst.Calls = 0) and (LFirst.Destroyed = 1) and
      (LTicket.Status = nesCancelled),
      'shutdown retires pending native work before any pump and preserves its cancelled ticket', Result);
    CheckSynchronize(0);
    Check((LFirst.Calls = 0) and (LFirst.Destroyed = 1),
      'later native queue service cannot revive shutdown work or release it twice', Result);

    NextCase;
    LWork := TNativeQueueWork.Create(LFirst);
    LTicket := LScheduler.Submit(LWork, neUIQueue);
    LWork := nil;
    LScheduler := nil;
    Check((LFirst.Calls = 0) and (LFirst.Destroyed = 1) and LTicket.Cancelled,
      'last scheduler release retires queued work without pumping', Result);
    CheckSynchronize(0);
    Check((LFirst.Calls = 0) and (LFirst.Destroyed = 1),
      'retained execution does not retain scheduler work after owner destruction', Result);

    NextCase;
    LWork := TNativeQueueWork.Create(LFirst);
    LTicket := LScheduler.Submit(LWork, neUIQueue);
    LWork := nil;
    LTicket.Cancel;
    Check((LFirst.Destroyed = 0) and (LTicket.Status = nesCancelled),
      'pending execution cancellation remains flags-only', Result);
    LOtherWork := TNativeQueueWork.Create(LSecond);
    LOtherTicket := LScheduler.Submit(LOtherWork, neUIQueue);
    LOtherWork := nil;
    LScheduler.Shutdown;
    Check((LFirst.Destroyed = 1) and (LSecond.Destroyed = 1) and
      (LFirst.Calls = 0) and (LSecond.Calls = 0) and LOtherTicket.Cancelled,
      'execution pruning cannot orphan an earlier cancelled native courier', Result);
    CheckSynchronize(0);
    Check((LFirst.Destroyed = 1) and (LSecond.Destroyed = 1),
      'pruned and pending native couriers both remain retired after queue service', Result);

    NextCase;
    LFirst.Scheduler := LScheduler;
    LFirst.Sibling := LSecond;
    LFirst.ShutdownOnRun := True;
    LWork := TNativeQueueWork.Create(LFirst);
    LTicket := LScheduler.Submit(LWork, neUIQueue);
    LWork := nil;
    LOtherWork := TNativeQueueWork.Create(LSecond);
    LOtherTicket := LScheduler.Submit(LOtherWork, neUIQueue);
    LOtherWork := nil;
    CheckSynchronize(0);
    Check((LFirst.Calls = 1) and LFirst.SawRunningCancellation and
      LFirst.SawRunningLifetime and (LFirst.Destroyed = 1),
      'running native work survives reentrant shutdown until its own return', Result);
    Check((LSecond.Calls = 0) and (LSecond.Destroyed = 1) and
      LOtherTicket.Cancelled and (LTicket.Status = nesCancelled),
      'reentrant shutdown retires the pending sibling and completes current cancellation', Result);

    NextCase;
    LFirst.DropSchedulerOnDestroy := True;
    LFirst.PumpOnDestroy := True;
    LFirst.SchedulerSlot := @LScheduler;
    LWork := TNativeQueueWork.Create(LFirst);
    LTicket := LScheduler.Submit(LWork, neUIQueue);
    LWork := nil;
    LOtherWork := TNativeQueueWork.Create(LSecond);
    LOtherTicket := LScheduler.Submit(LOtherWork, neUIQueue);
    LOtherWork := nil;
    LScheduler.Shutdown;
    Check((LScheduler = nil) and (LFirst.Reentries = 1) and
      (LFirst.Destroyed = 1) and (LFirst.Calls = 0) and LTicket.Cancelled,
      'work destruction can reenter shutdown and release the caller scheduler slot', Result);
    Check((LSecond.Calls = 0) and (LSecond.Destroyed = 1) and LOtherTicket.Cancelled,
      'all pending native targets are removed before a work destructor pumps', Result);
    CheckSynchronize(0);
    Check((LFirst.Destroyed = 1) and (LFirst.Calls = 0),
      'destructor-driven scheduler retirement leaves no queued target', Result);

    NextCase;
    LPeer := NewNyxScheduler;
    LOtherWork := TNativeQueueWork.Create(LSecond);
    LOtherTicket := LPeer.Submit(LOtherWork, neUIQueue);
    LOtherWork := nil;
    LWork := TNativeQueueWork.Create(LFirst);
    SubmitWithWake(nqwRaise);
    Check(LRejected and (LHook.Calls = 1) and (LFirst.Destroyed = 1) and
      (LFirst.Calls = 0),
      'wake refusal after insertion retires only the refused native entry without a pump', Result);
    Check((LOtherTicket.Status = nesPending) and (LSecond.Destroyed = 0) and
      (LSecond.Calls = 0),
      'exact-method enqueue cleanup preserves another scheduler pending entry', Result);
    CheckSynchronize(0);
    Check((LSecond.Calls = 1) and (LSecond.Destroyed = 1) and
      (LOtherTicket.Status = nesSucceeded) and (LFirst.Destroyed = 1),
      'peer scheduler still delivers once after another enqueue refusal', Result);

    NextCase;
    LWork := TNativeQueueWork.Create(LFirst);
    SubmitWithWake(nqwShutdown);
    Check(not LRejected and (LHook.Calls = 1) and (LFirst.Calls = 0) and
      (LFirst.Destroyed = 1) and (LTicket.Status = nesCancelled),
      'shutdown inside enqueue wake returns an inert retained execution', Result);
    CheckSynchronize(0);
    Check((LFirst.Calls = 0) and (LFirst.Destroyed = 1),
      'enqueue shutdown cannot post or revive a retired native entry', Result);

    NextCase;
    LWork := TNativeQueueWork.Create(LFirst);
    SubmitWithWake(nqwDeliverRaise);
    Check(LRejected and (LHook.Calls = 1) and (LFirst.Calls = 1) and
      (LFirst.Destroyed = 1) and (LFirst.Ticket.Status = nesSucceeded),
      'wake delivery followed by refusal does not double-release completed native work', Result);
    CheckSynchronize(0);
    Check((LFirst.Calls = 1) and (LFirst.Destroyed = 1),
      'delivery-before-enqueue-refusal has no second callback or destruction', Result);

    NextCase;
    LTurns := NyxSchedulerTurns(LScheduler);

    if LTurns.CanYieldUI then
    begin
      { The sibling remains future-dated while the immediate child completes.
        No delay is waited out and no bootstrap/global flag is manipulated. }
      LWork := TNativeQueueWork.Create(LFirst);
      LInner := LScheduler.Submit(LWork, neSequential);
      LWork := nil;
      LFirst.Reset;
      LFirst.Scheduler := LScheduler;
      LFirst.Sibling := LSecond;
      LFirst.ParentCallouts := True;
      LParent := TNativeQueueParent.Create(LInner, LFirst);
      LInner := nil;
      LDelays := NyxSchedulerDelays(LScheduler);
      LOtherWork := TNativeQueueWork.Create(LSecond);
      LOtherTicket := LDelays.PostAfter(LOtherWork, 60000);
      LOtherWork := nil;
      LWork := TNativeQueueWork.Create(LFirst);
      LTicket := LScheduler.PostUI(LWork, LParent);
      LWork := nil;
      LParent := nil;
      CheckSynchronize(0);
      Check((LFirst.Calls = 1) and (LTicket.Status = nesSucceeded) and
        (LOtherTicket.Status = nesPending) and (LSecond.Destroyed = 0) and
        (LFirst.ParentDestroyed = 0),
        'completed parented child and future sibling retain distinct native ownership', Result);
      { Only the scheduler's completed-execution vector now retains the child
        and its external parent. Do not submit again and prune that vector. }
      LTicket := nil;
      LFirst.Ticket := nil;
      LScheduler.Shutdown;
      Check((LFirst.ParentDestroyed = 1) and LFirst.ParentSawRetirement and
        (LSecond.Calls = 0) and (LSecond.Destroyed = 1),
        'external parent destructor runs after all pending native work has retired', Result);
      Check(LOtherTicket.Cancelled and (LFirst.Calls = 1) and
        (LFirst.Destroyed = 1) and (LSecond.Destroyed = 1),
        'parent destructor reentry and queue service preserve completed and cancelled outcomes', Result);
    end
    else
    begin
      WriteLn('SKIP native delayed parent retirement: RTL has no cooperative UI delay');
    end;
  finally
    WakeMainThread := LPrevious;
    LHook.Armed := False;
    LHook.Scheduler := nil;
    LHook.Free;
    { Revoke borrowed stack/test pointers even on a failed implementation.
      Managed fact leases keep any leaked work safe for the failure report. }
    LFirst.DropSchedulerOnDestroy := False;
    LFirst.PumpOnDestroy := False;
    LFirst.ShutdownOnRun := False;
    LFirst.ParentCallouts := False;
    LFirst.SchedulerSlot := nil;
    LFirst.Sibling := nil;
    LFirst.Scheduler := nil;
    LSecond.SchedulerSlot := nil;
    LWork := nil;
    LOtherWork := nil;
    LParent := nil;
    LInner := nil;
    LTurns := nil;
    LDelays := nil;

    if LScheduler <> nil then
    begin
      LScheduler.Shutdown;
    end;

    if LPeer <> nil then
    begin
      LPeer.Shutdown;
    end;
    LScheduler := nil;
    LPeer := nil;
    LTicket := nil;
    LOtherTicket := nil;
    LFirstLease := nil;
    LSecondLease := nil;
  end;
end;
{$endif}

{ Default event routers own their scheduler's terminal boundary. Direct queued
  descendants can retain that scheduler even after all event registrations are
  gone; Close must retire their native queue without requiring another pump.
  An explicitly shared scheduler belongs to its caller and remains usable. }
function RunRouterSchedulerOwnershipTests: Integer;
var
  LRouter: INyxEvents;
  LScheduler: INyxScheduler;
  LShared: INyxScheduler;
  LRejected: Boolean;
  {$ifndef PAS2JS}
  LFacts: TNativeQueueFacts;
  LFactsLease: IInterface;
  LWork: INyxWork;
  LTicket: INyxExecution;
  {$endif}
begin
  Result := 0;
  LRouter := nil;
  LScheduler := nil;
  LShared := nil;
  {$ifndef PAS2JS}
  LFacts := TNativeQueueFacts.Create;
  LFactsLease := LFacts;
  {$endif}
  try
    LRouter := NewNyxEvents;
    LScheduler := LRouter.Scheduler;
    {$ifndef PAS2JS}
    LFacts.Scheduler := LScheduler;
    LWork := TNativeQueueWork.Create(LFacts);
    LTicket := LScheduler.PostUI(LWork);
    LWork := nil;
    {$endif}
    LRouter.Close;
    {$ifndef PAS2JS}
    Check((LFacts.Calls = 0) and (LFacts.Destroyed = 1) and LTicket.Cancelled,
      'owned router Close retires queued scheduler-retaining work before any pump', Result);
    {$endif}
    LRejected := False;
    try
      LScheduler.Admit(neSequential);
    except
      on ENyxSchedule do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'retaining a default scheduler does not extend its closed router authority', Result);
    LRouter.Close;
    LRouter := nil;
    {$ifndef PAS2JS}
    LFacts.Reset;
    {$endif}
    LScheduler := nil;

    LRouter := NewNyxEvents;
    LScheduler := LRouter.Scheduler;
    {$ifndef PAS2JS}
    LFacts.Scheduler := LScheduler;
    LWork := TNativeQueueWork.Create(LFacts);
    LTicket := LScheduler.PostUI(LWork);
    LWork := nil;
    {$endif}
    LRouter := nil;
    {$ifndef PAS2JS}
    Check((LFacts.Calls = 0) and (LFacts.Destroyed = 1) and LTicket.Cancelled,
      'last router release also retires its default scheduler without a queue drain', Result);
    LFacts.Reset;
    {$endif}
    LScheduler := nil;

    LShared := NewNyxScheduler;
    LRouter := NewNyxEvents(LShared);
    {$ifndef PAS2JS}
    LFacts.Scheduler := LShared;
    LWork := TNativeQueueWork.Create(LFacts);
    LTicket := LShared.PostUI(LWork);
    LWork := nil;
    {$endif}
    LRouter.Close;
    LShared.Admit(neSequential);
    Check(True, 'closing a router leaves its caller-supplied scheduler open', Result);
    {$ifndef PAS2JS}
    Check(not LTicket.Cancelled and (LFacts.Calls = 0),
      'borrowed router closure preserves unrelated pending shared work', Result);
    CheckSynchronize(0);
    Check((LFacts.Calls = 1) and (LFacts.Destroyed = 1),
      'unrelated shared work still executes exactly once after router closure', Result);
    {$endif}
  finally

    if LRouter <> nil then
    begin
      LRouter.Close;
    end;

    if LScheduler <> nil then
    begin
      LScheduler.Shutdown;
    end;

    if LShared <> nil then
    begin
      LShared.Shutdown;
    end;
    {$ifndef PAS2JS}
    LFacts.Scheduler := nil;
    LWork := nil;
    LTicket := nil;
    LFactsLease := nil;
    {$endif}
  end;
end;

constructor TConstructionLeaseCallback.Create(AFacts: TConstructionLeaseFacts;
  AFail: Boolean);
begin
  { No inherited constructor is required to reach TObject.AfterConstruction. }
  FFacts := AFacts;
  TemporaryLease;

  if AFacts.Destroyed <> 0 then
  begin
    { Let the owning assertion report premature disposal without calling back
      through the receiver that the broken implementation already retired. }
    Exit;
  end;
  TemporaryLease;

  if AFail then
  begin
    raise ENyxSchedule.Create('Intentional callback constructor failure');
  end;
  Inc(AFacts.Constructed);
end;

constructor TConstructionLeaseCallback.CreateRetained(AFacts: TConstructionLeaseFacts;
  out AOwner: INyxEventCallback);
begin
  FFacts := AFacts;
  TemporaryLease;
  AOwner := Self;
  TemporaryLease;
  Inc(AFacts.Constructed);
end;

procedure TConstructionLeaseCallback.TemporaryLease;
var
  LLease: INyxEventCallback;
begin
  LLease := Self;
  LLease.Invoke(EventInfo, nil);
end;

procedure TConstructionLeaseCallback.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
begin
  Inc(FFacts.Calls);
end;

destructor TConstructionLeaseCallback.Destroy;
begin
  Inc(FFacts.Destroyed);
  inherited Destroy;
end;

function RunCallbackConstructionTests: Integer;
var
  LFacts: TConstructionLeaseFacts;
  LCallback: TConstructionLeaseCallback;
  LOwner: INyxEventCallback;
  LRefused: Boolean;
begin
  Result := 0;
  LFacts := TConstructionLeaseFacts.Create;
  LCallback := nil;
  try
    LCallback := TConstructionLeaseCallback.Create(LFacts, False);
    Check((LFacts.Destroyed = 0) and (LFacts.Constructed = 1) and (LFacts.Calls = 2),
      'temporary Self leases cannot destroy an unfinished callback', Result);
    Check(LCallback.RefCount = 0,
      'successful construction retires its internal reference without destruction', Result);
    LOwner := LCallback;
    Check(LCallback.RefCount = 1,
      'the first real interface owner receives the ordinary reference count', Result);
    LCallback := nil;
    LOwner.Invoke(EventInfo, nil);
    Check((LFacts.Calls = 3) and (LFacts.Destroyed = 0),
      'a constructor-leased receiver remains callable through its real owner', Result);
    LOwner := nil;
    Check(LFacts.Destroyed = 1,
      'the final real interface release destroys the callback exactly once', Result);
    LFacts.Calls := 0;
    LFacts.Constructed := 0;
    LFacts.Destroyed := 0;
    LCallback := TConstructionLeaseCallback.Create(LFacts, False);
    Check((LFacts.Destroyed = 0) and (LCallback.RefCount = 0),
      'a raw-class caller receives a live unowned callback without a leaked pin', Result);
    LCallback.Free;
    LCallback := nil;
    Check(LFacts.Destroyed = 1,
      'a raw-class caller can free its constructed receiver normally', Result);
    LFacts.Calls := 0;
    LFacts.Constructed := 0;
    LFacts.Destroyed := 0;
    LCallback := TConstructionLeaseCallback.CreateRetained(LFacts, LOwner);
    Check((LFacts.Destroyed = 0) and (LFacts.Constructed = 1) and
      (LFacts.Calls = 2) and (LCallback.RefCount = 1),
      'construction retires only its pin when a real caller already owns the receiver', Result);
    LCallback := nil;
    LOwner.Invoke(EventInfo, nil);
    Check((LFacts.Calls = 3) and (LFacts.Destroyed = 0),
      'a constructor-published owner remains valid after the construction boundary', Result);
    LOwner := nil;
    Check(LFacts.Destroyed = 1,
      'a constructor-published owner also destroys exactly once on final release', Result);
    LFacts.Calls := 0;
    LFacts.Constructed := 0;
    LFacts.Destroyed := 0;
    LRefused := False;
    try
      LCallback := TConstructionLeaseCallback.Create(LFacts, True);
    except
      on ENyxSchedule do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LFacts.Calls = 2) and (LFacts.Constructed = 0) and
      (LFacts.Destroyed = 1),
      'constructor failure unwinds temporary leases and destroys exactly once', Result);
  finally
    LOwner := nil;

    if (LCallback <> nil) and (LFacts.Destroyed = 0) then
    begin
      LCallback.Free;
    end;
    LFacts.Free;
  end;
end;

constructor TScopeLeaseProbe.Create(AFacts: TScopeLeaseFacts);
begin
  inherited Create;
  FFacts := AFacts;
end;

destructor TScopeLeaseProbe.Destroy;
begin
  Inc(FFacts.Destroyed);
  inherited Destroy;
end;

procedure TScopeLeaseProbe.Cancel;
begin
  FCancelled := True;
end;

function TScopeLeaseProbe.GetCancelled: Boolean;
begin
  Result := FCancelled;
end;

procedure TScopeLeaseProbe.CheckReady;
begin

  if FCancelled then
  begin
    raise ENyxSchedule.Create('Original counted delivery was cancelled');
  end;
end;

{ Real nested event scopes must retain both certified parent and delivery
  interfaces independently. Releasing a completed callback context must not
  destroy a caller's still-held delivery or the router's canonical generation. }
function RunEventScopeLeaseTests: Integer;
var
  LRouter: INyxEvents;
  LParent: INyxCancellationScope;
  LParentReady: INyxCancellationReadiness;
  LDelivery: INyxCancellationScope;
  LDeliveryReady: INyxCancellationReadiness;
  LProbe: TProbe;
  LProbeLease: INyxEventCallback;
  LToken: INyxEventSubscription;
  LTickets: TNyxExecutions;
  LEvent: TNyxEventInfo;
  LFacts: TScopeLeaseFacts;
  LRefused: Boolean;
begin
  Result := 0;
  LFacts := TScopeLeaseFacts.Create;
  LRouter := NewNyxEvents;
  try
    LParent := NyxEventGeneration(LRouter);
    Check(Supports(LParent, INyxCancellationReadiness, LParentReady),
      'actual router generation supplies its independent readiness lease', Result);
    LDelivery := TScopeLeaseProbe.Create(LFacts);
    Check(Supports(LDelivery, INyxCancellationReadiness, LDeliveryReady),
      'counted guarded delivery supplies a separate readiness lease', Result);
    LProbe := TProbe.Create;
    LProbeLease := LProbe;
    LToken := LRouter.OnAfterEnter(NyxControlEvents('reply')).Subscribe(LProbeLease);
    LEvent := EventInfo;
    LTickets := LRouter.DispatchGuarded(LEvent, 'reply', 'thread', LDelivery);
    Check((Length(LTickets) = 1) and (LTickets[0].Status = nesSucceeded) and
      (LProbe.Calls = 1) and not LProbe.RetainedContext.Cancelled,
      'guarded real callback retains an admitted parent and delivery context', Result);
    LDeliveryReady := nil;
    LTickets := nil;
    LProbe.RetainedContext := nil;
    Check(LFacts.Destroyed = 0,
      'releasing the actual child context preserves the caller-owned delivery', Result);
    LDelivery.Cancel;
    Check(LDelivery.Cancelled and (LFacts.Destroyed = 0),
      'retained delivery facade remains callable after child-scope retirement', Result);
    LDelivery := nil;
    Check(LFacts.Destroyed = 1,
      'delivery retires exactly once after its final real caller lease', Result);
    LParentReady.CheckReady;
    Check(not LParent.Cancelled,
      'child-scope retirement preserves both retained canonical parent facets', Result);
    LParent := nil;
    LParentReady := nil;
    LParent := NyxEventGeneration(LRouter);
    Check(Supports(LParent, INyxCancellationReadiness, LParentReady),
      'the router still owns its canonical generation after external leases retire', Result);
    LParentReady.CheckReady;
    LRouter.Close;
    LRouter := nil;
    LRefused := False;
    try
      LParentReady.CheckReady;
    except
      on ENyxSchedule do
      begin
        LRefused := True;
      end;
    end;
    Check(LParent.Cancelled and LRefused,
      'retained canonical parent safely reports terminal cancellation after router disposal', Result);
  finally

    if LRouter <> nil then
    begin
      LRouter.Close;
    end;
    LToken := nil;
    LProbeLease := nil;
    LTickets := nil;
    LDeliveryReady := nil;
    LDelivery := nil;
    LParentReady := nil;
    LParent := nil;
    LFacts.Free;
  end;
end;

function RunNyxSchedulerTests: Integer;
var
  LRouter: INyxEvents;
  LStream: INyxEventStream;
  LFirst: TProbe;
  LSecond: TProbe;
  LThird: TProbe;
  LFirstLease: INyxEventCallback;
  LSecondLease: INyxEventCallback;
  LThirdLease: INyxEventCallback;
  LFirstToken: INyxEventSubscription;
  LSecondToken: INyxEventSubscription;
  LThirdToken: INyxEventSubscription;
  LTickets: TNyxExecutions;
  LEvent: TNyxEventInfo;
  LRejected: Boolean;
begin
  Result := RunCallbackConstructionTests + RunNyxKeyboardResponseTests +
    RunEventScopeLeaseTests;
  LRouter := NewNyxEvents;
  try
    LFirst := TProbe.Create;
    LFirstLease := LFirst;
    LSecond := TProbe.Create;
    LSecondLease := LSecond;
    LThird := TProbe.Create;
    LThirdLease := LThird;
    LEvent := EventInfo;
    LStream := LRouter.OnAfterEnter(NyxControlEvents('reply'));
    LFirstToken := LStream.Subscribe(LFirstLease);
    LSecondToken := LStream.Subscribe(LSecondLease);
    Check((LStream.Count = 2) and (LFirstToken.ID <> LSecondToken.ID),
      'independent stable registrations are visible', Result);
    Check(LStream.Registrations[1].ID = LSecondToken.ID, 'visible registration order', Result);
    Check(Length(LRouter.Dispatch(LEvent, 'other', 'thread')) = 0,
      'exact design identity rejects another control', Result);
    LTickets := LRouter.Dispatch(LEvent, 'reply', 'thread');
    Check((Length(LTickets) = 2) and (LFirst.Calls = 1) and (LSecond.Calls = 1),
      'multiple sequential callbacks complete before dispatch returns', Result);
    Check((LTickets[0].Status = nesSucceeded) and
      (LSecond.Last.Value.AsText = TNyxText('Owned reply / 🌙')), 'owned typed snapshot', Result);
    LFirst.CancelOther := LSecondToken;
    LFirst.AddTo := LStream;
    LFirst.Addition := LThirdLease;
    LTickets := LRouter.Dispatch(LEvent, 'reply', 'thread');
    Check((LSecond.Calls = 1) and (LThird.Calls = 0) and
      (LTickets[1].Status = nesCancelled), 'dispatch mutation suppresses removed and defers added callback', Result);
    Check((LStream.Count = 2) and not LSecondToken.Active,
      'active list reflects removal and addition', Result);
    LTickets := LRouter.Dispatch(LEvent, 'reply', 'thread');
    Check(LThird.Calls = 1, 'new callback participates in the next dispatch', Result);
    LFirst.Failure := True;
    LTickets := LRouter.Dispatch(LEvent, 'reply', 'thread');
    Check((LTickets[0].Status = nesFailed) and
      (Pos(TNyxText('🌙'), LTickets[0].Failure) > 0) and (LThird.Calls = 2),
      'failed callback retains Unicode diagnostic and does not suppress siblings', Result);
    Check(LFirstToken.LastExecution.Status = nesFailed,
      'actual registration exposes its latest diagnostic', Result);
    LFirst.Failure := False;
    LFirst.Router := LRouter;
    LFirst.Nested := LEvent.Copy;
    LFirst.Reenter := True;
    LTickets := LRouter.Dispatch(LEvent, 'reply', 'thread');
    Check((LFirst.Calls = 6) and (LThird.Calls = 4), 'nested dispatch has independent membership', Result);
    LFirst.CancelSelf := LFirstToken;
    LTickets := LRouter.Dispatch(LEvent, 'reply', 'thread');
    Check(LFirst.SawCancellation and (LTickets[0].Status = nesCancelled),
      'self cancellation is observable before Submit returns', Result);
    LThird.Router := LRouter;
    LThird.Invalidate := True;
    LThirdToken := LStream.Registrations[0];
    LTickets := LRouter.Dispatch(LEvent, 'reply', 'thread');
    Check(LThird.SawCancellation and (LTickets[0].Status = nesCancelled),
      'view invalidation cancels the in-flight synchronous scope', Result);
    LThird.Invalidate := False;
    LThird.CancelSelf := LThirdToken;
    LRouter.Dispatch(LEvent, 'reply', 'thread');
    Check(LStream.Count = 0, 'all registrations can be removed independently', Result);
    LRejected := False;
    try
      LRouter.On(NyxControlEvents('reply'), ntDesignValue);
    except
      on LException: ENyxSchedule do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'design triggers are not application callbacks', Result);
    LRouter.Close;
    LRejected := False;
    try
      LStream.Policy(neUIQueue);
    except
      on LException: ENyxSchedule do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and not LFirstToken.Active, 'retained stream rejects authoring after router disposal', Result);
    Check(LFirst.RetainedContext.Cancelled and
      (LFirst.Last.Value.AsText = TNyxText('Owned reply / 🌙')),
      'retained execution context and payload survive router closure without a callback cycle', Result);
  finally
    LRouter.Close;
    LFirst.Router := nil;
    LFirst.AddTo := nil;
    LThird.Router := nil;
  end;
  {$ifndef PAS2JS}Inc(Result, RunNativeQueueRetirementTests);{$endif}
  Inc(Result, RunRouterSchedulerOwnershipTests);
end;

{$ifndef PAS2JS}
{ Execution status proves callback completion, not FreeOnTerminate destruction.
  The maintained Win32 pool qualification retains actual termination handles too.
  Capture this isolated fixture's workers while they remain idle/alive, then pump
  UI until shutdown terminates them. Zero-time OS probes never block UI handoff;
  no thread/process is forcibly terminated. Other widgetsets retain their existing
  execution-scope monitoring and need their own termination qualification. }
procedure RetireSchedulerFixture(const AScheduler: INyxScheduler);
var
  LStart: QWord;
  LMonitor: INyxSchedulerMonitor;
  LDone: Boolean;
  {$ifdef MSWINDOWS}
  LSnapshot: THandle;
  LEntry: THREADENTRY32;
  LHandles: array of THandle;
  LHandle: THandle;
  LIndex: Integer;
  LWait: DWORD;
  {$endif}
begin
  AScheduler.RequireUI;
  Supports(AScheduler, INyxSchedulerMonitor, LMonitor);
  {$ifdef MSWINDOWS}
  LHandles := nil;
  LSnapshot := CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD, 0);

  if LSnapshot = INVALID_HANDLE_VALUE then
  begin
    raise ENyxModel.Create('Cannot capture scheduler fixture termination handles');
  end;
  try
    FillChar(LEntry, SizeOf(LEntry), 0);
    LEntry.dwSize := SizeOf(LEntry);

    if Thread32First(LSnapshot, LEntry) then
    begin
      repeat

        if (LEntry.th32OwnerProcessID = GetCurrentProcessId) and
          (LEntry.th32ThreadID <> GetCurrentThreadId) then
        begin
          LHandle := OpenThread(SYNCHRONIZE, False, LEntry.th32ThreadID);

          if LHandle <> 0 then
          begin
            SetLength(LHandles, Length(LHandles) + 1);
            LHandles[High(LHandles)] := LHandle;
          end;
        end;
      until not Thread32Next(LSnapshot, LEntry);
    end;
  finally
    CloseHandle(LSnapshot);
  end;
  {$endif}
  try
    AScheduler.Shutdown;
    LStart := GetTickCount64;
    repeat
      LDone := (LMonitor = nil) or (LMonitor.WorkerLoad.ActiveWorkers = 0);
      {$ifdef MSWINDOWS}
      for LIndex := 0 to High(LHandles) do
      begin
        LWait := WaitForSingleObject(LHandles[LIndex], 0);

        if LWait = WAIT_FAILED then
        begin
          raise ENyxModel.Create('Scheduler fixture termination observation failed');
        end;
        LDone := LDone and (LWait = WAIT_OBJECT_0);
      end;
      {$endif}

      if not LDone then
      begin

        if GetTickCount64 - LStart > 5000 then
        begin
          raise ENyxModel.Create('Scheduler fixture workers did not terminate');
        end;
        CheckSynchronize(1);
        Sleep(1);
      end;
    until LDone;
  finally
    {$ifdef MSWINDOWS}
    for LIndex := 0 to High(LHandles) do
    begin
      CloseHandle(LHandles[LIndex]);
    end;
    {$endif}
  end;
end;
{$endif}

constructor TNyxScheduleJourney.Create(ADone: TNyxScheduleTestCompletion);
begin
  inherited Create;
  FDone := ADone;
end;

destructor TNyxScheduleJourney.Destroy;
begin
  FState.Free;
  inherited Destroy;
end;

procedure TNyxScheduleJourney.Start;
var
  LState: TJourneyState;
  LEvent: TNyxEventInfo;
  LRejected: Boolean;
  LConsumed: Boolean;
  {$IFNDEF PAS2JS}
  LStart: QWord;
  {$ENDIF}
begin
  try
    LState := TJourneyState.Create;
    FState := LState;
    LState.Router := NewNyxEvents;
    LState.Stream := LState.Router.OnAfterEnter(NyxControlEvents('reply'));
    LState.Probe := TProbe.Create;
    LState.ProbeLease := LState.Probe;
    LState.SkipProbe := TProbe.Create;
    LState.SkipLease := LState.SkipProbe;
    LState.First := LState.Stream.Policy(neUIQueue).Subscribe(LState.ProbeLease);
    LState.Cancelled := LState.Stream.Subscribe(LState.SkipLease);
    LEvent := EventInfo;
    LState.Pending := LState.Router.Dispatch(LEvent, 'reply', 'thread');
    LEvent.Value := NyxData('Changed after dispatch');
    LState.Cancelled.Cancel;
    Check((LState.Probe.Calls = 0) and (LState.SkipProbe.Calls = 0),
      'UIQueue never invokes inline on either target', FChecks);
    Check(LState.Pending[1].Status = nesCancelled,
      'cancelled pending ticket is visible immediately', FChecks);
    LState.FaultProbe := TProbe.Create;
    LState.FaultLease := LState.FaultProbe;
    LState.FaultProbe.Failure := True;
    LState.FaultTicket := LState.Router.OnAfterExit(NyxControlEvents('reply'))
      .Policy(neUIQueue).Subscribe(LState.FaultLease).LastExecution;
    LEvent := EventInfo;
    LEvent.Trigger := ntAfterExit;
    LState.Pending := LState.Router.Dispatch(LEvent, 'reply', 'thread');
    LState.FaultTicket := LState.Pending[0];
    LState.AsyncProbe := TProbe.Create;
    LState.AsyncLease := LState.AsyncProbe;
    LState.Router.On(NyxCompoundEvents('thread'), ntClick)
      .Policy(neAsynchronous).Subscribe(LState.AsyncLease);
    LEvent := EventInfo;
    LEvent.Trigger := ntClick;
    LState.Pending := LState.Router.Dispatch(LEvent, 'reply', 'thread');
    LState.AsyncTicket := LState.Pending[0];
    LState.QueuedKeyProbe := TProbe.Create;
    LState.QueuedKeyLease := LState.QueuedKeyProbe;
    LState.QueuedKeyProbe.ConsumeInput := True;
    LState.Router.OnKeyDown(NyxControlEvents('key-queued'))
      .Policy(neUIQueue).Subscribe(LState.QueuedKeyLease);
    LState.AsyncKeyProbe := TProbe.Create;
    LState.AsyncKeyLease := LState.AsyncKeyProbe;
    LState.AsyncKeyProbe.ConsumeInput := True;
    LState.Router.OnKeyUp(NyxControlEvents('key-async'))
      .Policy(neAsynchronous).Subscribe(LState.AsyncKeyLease);
    LEvent := EventInfo;
    LEvent.HasKeyboard := True;
    LEvent.Keyboard := NyxKeyStroke(nkEnterKey, [nmControl]);
    LEvent.Trigger := ntKeyDown;
    LState.Pending := LState.Router.DispatchInput(LEvent, 'key-queued', 'thread', LConsumed);
    LState.QueuedKeyTicket := LState.Pending[0];
    Check(not LConsumed, 'queued keyboard dispatch returns an unconsumed default', FChecks);
    LEvent.Trigger := ntKeyUp;
    LState.Pending := LState.Router.DispatchInput(LEvent, 'key-async', 'thread', LConsumed);
    LState.AsyncKeyTicket := LState.Pending[0];
    Check(not LConsumed, 'asynchronous keyboard dispatch cannot consume inline', FChecks);
    {$IFDEF PAS2JS}
    Check(not (nscWorkerThreads in LState.Router.Scheduler.Capabilities),
      'browser accurately advertises deferred UI rather than threads', FChecks);
    LRejected := False;
    try
      LState.Stream.Policy(neThreaded);
    except
      on LException: ENyxSchedule do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LState.Stream.ExecutionPolicy = neUIQueue),
      'unsupported threaded policy preserves accepted policy', FChecks);
    window.setTimeout(@Poll, 10);
    {$ELSE}
    Check(nscWorkerThreads in LState.Router.Scheduler.Capabilities,
      'native scheduler advertises actual threads', FChecks);
    LState.MainThreadID := GetCurrentThreadID;
    LState.WorkerProbe := TProbe.Create;
    LState.WorkerLease := LState.WorkerProbe;
    LState.UIProbe := TProbe.Create;
    LState.UILease := LState.UIProbe;
    LState.WorkerProbe.UIWork := TUIProbeWork.Create(LState.UILease, EventInfo);
    LState.WorkerProbe.Scheduler := LState.Router.Scheduler;
    LState.Stream := LState.Router.On(NyxControlEvents('worker'), ntAfterEnter);
    LState.Stream.Policy(neAsynchronous).Subscribe(LState.WorkerLease);
    LEvent := EventInfo;
    LEvent.Trigger := ntAfterEnter;
    LState.Pending := LState.Router.Dispatch(LEvent, 'worker', 'thread');
    LState.WorkerTicket := LState.Pending[0];
    LState.Cooperative := TCooperativeCallback.Create;
    LState.CooperativeLease := LState.Cooperative;
    LState.CooperativeRegistration := LState.Router.OnAfterEnter(NyxControlEvents('cooperative'))
      .Policy(neThreaded).Subscribe(LState.CooperativeLease);
    LState.Pending := LState.Router.Dispatch(LEvent, 'cooperative', 'thread');
    LState.CooperativeTicket := LState.Pending[0];
    LStart := GetTickCount64;
    while Assigned(FDone) and (GetTickCount64 - LStart < 5000) do
    begin
      CheckSynchronize(10);
      Poll;

      if Assigned(FDone) then
      begin
        Sleep(1);
      end;
    end;

    if Assigned(FDone) then
    begin
      Finish('Native scheduler journey timed out');
    end;
    RetireSchedulerFixture(LState.Router.Scheduler);
    {$ENDIF}
  except
    on LException: Exception do
    begin
      Finish(LException.Message);
    end;
  end;
end;

procedure TNyxScheduleJourney.Poll;
var
  LState: TJourneyState;
begin
  try
    Inc(FPolls);
    LState := TJourneyState(FState);
    {$IFNDEF PAS2JS}

    if InterlockedCompareExchange(LState.Cooperative.Started, 0, 0) <> 0 then
    begin
      LState.CooperativeRegistration.Cancel;
    end;
    {$ENDIF}

    if (LState.First.LastExecution.Status in [nesPending, nesRunning]) or
      (LState.FaultTicket.Status in [nesPending, nesRunning])
      or (LState.AsyncTicket.Status in [nesPending, nesRunning])
      or (LState.QueuedKeyTicket.Status in [nesPending, nesRunning])
      or (LState.AsyncKeyTicket.Status in [nesPending, nesRunning])
      {$IFNDEF PAS2JS}
      or (LState.WorkerTicket.Status in [nesPending, nesRunning])
      or ((LState.WorkerProbe.UITicket <> nil) and
      (LState.WorkerProbe.UITicket.Status in [nesPending, nesRunning]))
      or (LState.CooperativeTicket.Status in [nesPending, nesRunning])
      {$ENDIF} then
    begin

      if FPolls >= 200 then
      begin
        raise ENyxModel.Create('Scheduler queue did not complete');
      end;
      {$IFDEF PAS2JS}
      window.setTimeout(@Poll, 10);
      {$ENDIF}
      Exit;
    end;
    Check((LState.Probe.Calls = 1) and (LState.SkipProbe.Calls = 0),
      'deferred execution runs once and skips cancelled registration', FChecks);
    Check(LState.Probe.Last.Value.AsText = TNyxText('Owned reply / 🌙'),
      'deferred snapshot stays independent of later caller mutation', FChecks);
    Check((LState.FaultTicket.Status = nesFailed) and
      (Pos(TNyxText('🌙'), LState.FaultTicket.Failure) > 0),
      'queued callback failure retains exact diagnostic', FChecks);
    Check((LState.AsyncProbe.Calls = 1) and (LState.AsyncProbe.Last.Trigger = ntClick) and
      (LState.AsyncProbe.Last.SourceID = 'runtime/thread'),
      'asynchronous policy dispatches an independently owned semantic source event', FChecks);
    Check((LState.QueuedKeyTicket.Status = nesFailed) and
      not LState.QueuedKeyProbe.SawCanConsume and (LState.QueuedKeyProbe.Calls = 1),
      'real queued callbacks explicitly fail an attempted late Consume', FChecks);
    Check((LState.AsyncKeyTicket.Status = nesFailed) and
      not LState.AsyncKeyProbe.SawCanConsume and (LState.AsyncKeyProbe.Calls = 1),
      'real asynchronous callbacks explicitly fail an attempted Consume', FChecks);
    Check(LState.AsyncKeyProbe.Last.Keyboard.Matches(nkEnterKey, [nmControl]),
      'actual asynchronous execution retains the typed key/modifier snapshot', FChecks);
    {$IFNDEF PAS2JS}
    Check((LState.WorkerProbe.Calls = 1) and
      (LState.WorkerProbe.ThreadID <> LState.MainThreadID),
      'native asynchronous callback actually ran on another thread', FChecks);
    Check(LState.Probe.ThreadID = LState.MainThreadID,
      'native UIQueue actually returned to the submitting UI thread', FChecks);
    Check((LState.UIProbe.Calls = 1) and
      (LState.UIProbe.ThreadID = LState.MainThreadID),
      'worker handoff survives worker completion and runs on the UI thread', FChecks);
    Check((LState.CooperativeTicket.Status = nesCancelled) and
      (InterlockedCompareExchange(LState.Cooperative.Stopped, 0, 0) = 1),
      'actual threaded callback observes cancellation while running', FChecks);
    {$ENDIF}
    Finish('');
  except
    on LException: Exception do
    begin
      Finish(LException.Message);
    end;
  end;
end;

procedure TNyxScheduleJourney.Finish(const AFailure: TNyxText);
var
  LDone: TNyxScheduleTestCompletion;
begin
  LDone := FDone;
  FDone := nil;

  if Assigned(LDone) then
  begin
    LDone(FChecks, AFailure);
  end;
end;

end.
