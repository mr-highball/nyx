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
  Result := RunNyxKeyboardResponseTests;
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
end;

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
