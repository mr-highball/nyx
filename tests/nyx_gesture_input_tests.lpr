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
program nyx_gesture_input_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.types, nyx.data, nyx.model, nyx.behavior,
  nyx.gestures, nyx.events, nyx.scheduler
  {$ifdef PAS2JS}, JS, Web{$else}, Classes{$endif};

type
  { Scenario ownership stays outside the router. Probes borrow this owner only
    while its registrations are live; destruction closes them before retirement. }
  TScenario = class;

  { Mutations deliberately run inside the first callback, against the dispatch's
    already captured membership. These are physical-input tests, not UI gestures. }
  TProbeAction = (paObserve, paConsume, paFail, paNavigate, paClose, paSubscribe,
    paRetireNodes, paReenter);

  { Retain response interfaces, never the event router. Late authority checks
    must still fail after callback return, cancellation and queued delivery. }
  TProbe = class(TNyxEventCallback)
  public
    Owner: TScenario;
    Action: TProbeAction;
    Calls: Integer;
    CanConsume: Boolean;
    CanCapture: Boolean;
    SawConsumed: Boolean;
    ConsumeRefused: Boolean;
    CaptureRefused: Boolean;
    Reentered: Boolean;
    Input: INyxEventResponse;
    Gesture: INyxGestureResponse;
    Last: TNyxEventInfo;
    procedure Invoke(const AEvent: TNyxEventInfo;
      const AExecution: INyxExecution); override;
  end;

  { Before/after observers receive copied metadata and captured callback counts.
    A before observer adds a registration to test the same frozen membership. }
  TObservation = class(TNyxEventObserver)
  public
    Owner: TScenario;
    Calls: Integer;
    CallbackCount: Integer;
    AddCallback: Boolean;
    procedure Observe(const AEvent: TNyxEventInfo; ACallbackCount: Integer); override;
  end;

  { The fixture owns router/scheduler and three independent callback leases.
    Nodes are borrowed during the one lifetime test; its original owner flags
    distinguish explicit retirement from the helper's synchronous leases. }
  TScenario = class
  public
    Scheduler: INyxScheduler;
    Events: INyxEvents;
    Facet: INyxGestureInputEvents;
    First: TProbe;
    Second: TProbe;
    Added: TProbe;
    FirstLease: INyxEventCallback;
    SecondLease: INyxEventCallback;
    AddedLease: INyxEventCallback;
    FirstToken: INyxEventSubscription;
    SecondToken: INyxEventSubscription;
    AddedToken: INyxEventSubscription;
    Origin: TNyxNode;
    Source: TNyxNode;
    OwnsNodes: Boolean;
    constructor Create(ATrigger: TNyxTrigger = ntPointerDown);
    destructor Destroy; override;
  end;

  { Count actual node destruction independently of callback/response lifetime.
    The helper must keep both nodes alive when a callback drops their owners. }
  TTrackedNode = class(TNyxNode)
  public
    destructor Destroy; override;
  end;

var
  GChecks: Integer;
  GNodesRetired: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Gesture input: ' + AReason);
  end;
  Inc(GChecks);
end;

function PointerEvent(ATrigger: TNyxTrigger): TNyxEventInfo;
begin
  Result := Default(TNyxEventInfo);
  Result.Trigger := ATrigger;
  Result.Name := NyxEvent(NyxTriggerName(ATrigger));
  Result.OriginID := 'origin🌙';
  Result.SourceID := 'source🌙';
  Result.Value := NyxNull;
  Result.HasPointer := True;
  Result.Pointer.Kind := npiMouse;
  Result.Pointer.Button := npbPrimary;
  Result.Pointer.Buttons := [npbPrimary];
  Result.Pointer.Modifiers := [nmShift];
  Result.Pointer.HasPosition := True;
  Result.Pointer.X := 12.5;
  Result.Pointer.Y := 23.75;
  Result.Pointer.ID := 17;
  Result.Pointer.Primary := True;
end;

constructor TScenario.Create(ATrigger: TNyxTrigger);
begin
  inherited Create;
  Scheduler := NewNyxScheduler;
  Events := NewNyxEvents(Scheduler);
  Check(Supports(Events, INyxGestureInputEvents, Facet),
    'ordinary router advertises the optional physical pointer facet');
  First := TProbe.Create;
  FirstLease := First;
  First.Owner := Self;
  Second := TProbe.Create;
  SecondLease := Second;
  Second.Owner := Self;
  Added := TProbe.Create;
  AddedLease := Added;
  Added.Owner := Self;
  FirstToken := Events.On(NyxControlEvents('origin🌙'), ATrigger).Subscribe(FirstLease);
  SecondToken := Events.On(NyxCompoundEvents('source🌙'), ATrigger).Subscribe(SecondLease);
end;

destructor TScenario.Destroy;
begin

  if Events <> nil then
  begin
    Events.Close;
  end;

  if OwnsNodes then
  begin
    Source.ReleaseReference;
    Origin.ReleaseReference;
  end;
  First.Owner := nil;
  Second.Owner := nil;
  Added.Owner := nil;
  Scheduler.Shutdown;
  inherited Destroy;
end;

destructor TTrackedNode.Destroy;
begin
  Inc(GNodesRetired);
  inherited Destroy;
end;

procedure TProbe.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
var
  LInput: INyxEventResponse;
  LGesture: INyxGestureResponse;
  LDecision: INyxGestureDecision;
  LConsumed: Boolean;
begin
  Inc(Calls);
  Last := AEvent.Copy;
  Input := NyxEventResponse(AExecution);
  Gesture := NyxGestureResponse(AExecution);
  CanConsume := Input.CanConsume;
  CanCapture := Gesture.CanRequest(ngcCapturePointer);
  SawConsumed := Input.Consumed;
  case Action of
    paObserve:
      begin
        { Read the current decision without acquiring a default action. }
      end;
    paConsume, paFail:
      begin
        try
          Input.Consume;
        except
          on ENyxSchedule do
          begin
            ConsumeRefused := True;
          end;
        end;
        try
          Gesture.CapturePointer;
        except
          on ENyxSchedule do
          begin
            CaptureRefused := True;
          end;
        end;

        if Action = paFail then
        begin
          raise Exception.Create('Deliberate first callback failure');
        end;
      end;
    paNavigate:
      begin
        Owner.Events.CancelPending;
        Check(not Input.CanConsume and not Gesture.CanRequest(ngcCapturePointer),
          'navigation immediately revokes the current response');
      end;
    paClose:
      begin
        Owner.Events.Close;
      end;
    paSubscribe:
      begin

        if Owner.AddedToken = nil then
        begin
          Owner.AddedToken := Owner.Events.OnPointerDown(
            NyxControlEvents('origin🌙')).Subscribe(Owner.AddedLease);
        end;
      end;
    paRetireNodes:
      begin
        Owner.OwnsNodes := False;
        Owner.Source.ReleaseReference;
        Owner.Origin.ReleaseReference;
        Check(GNodesRetired = 0, 'both synchronous node leases survive caller retirement');
      end;
    paReenter:
      begin
        LInput := Input;
        LGesture := Gesture;

        if not Reentered then
        begin
          Reentered := True;
          LDecision := NewNyxGestureDecision([]);
          Owner.Facet.DispatchGestureInput(PointerEvent(ntPointerDown),
            'origin🌙', 'source🌙', LDecision, LConsumed);
          Check(LConsumed and LInput.CanConsume and LGesture.CanRequest(ngcCapturePointer),
            'nested dispatch returns without revoking its still-active outer response');
          LGesture.CapturePointer;
        end;
        LInput.Consume;
        Input := LInput;
        Gesture := LGesture;
      end;
  end;
end;

procedure TObservation.Observe(const AEvent: TNyxEventInfo; ACallbackCount: Integer);
begin
  Inc(Calls);
  CallbackCount := ACallbackCount;

  if AddCallback and (Owner.AddedToken = nil) then
  begin
    Owner.AddedToken := Owner.Events.OnPointerDown(
      NyxControlEvents('origin🌙')).Subscribe(Owner.AddedLease);
  end;
end;

procedure RetainedResponses(AProbe: TProbe);
var
  LRejected: Boolean;
begin
  Check(not AProbe.Input.CanConsume and
    not AProbe.Gesture.CanRequest(ngcCapturePointer),
    'returned response has no default or capture authority');
  LRejected := False;
  try
    AProbe.Input.Consume;
  except
    on ENyxSchedule do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'retained Consume explicitly refuses');
  LRejected := False;
  try
    AProbe.Gesture.CapturePointer;
  except
    on ENyxSchedule do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'retained capture explicitly refuses');
end;

procedure SynchronousCases;
const
  CTriggers: array[0..2] of TNyxTrigger = (ntPointerDown, ntPointerMove, ntPointerUp);
var
  LTrigger: TNyxTrigger;
  LScenario: TScenario;
  LDecision: INyxGestureDecision;
  LConsumed: Boolean;
  LExecutions: TNyxExecutions;
begin
  for LTrigger in CTriggers do
  begin
    LScenario := TScenario.Create(LTrigger);
    try
      LScenario.First.Action := paConsume;
      LDecision := NewNyxGestureDecision([ngcCapturePointer]);
      LExecutions := LScenario.Facet.DispatchGestureInput(PointerEvent(LTrigger),
        'origin🌙', 'source🌙', LDecision, LConsumed);
      Check(LConsumed and (Length(LExecutions) = 2) and
        (LScenario.First.Calls = 1) and (LScenario.Second.Calls = 1),
        'one snapshot invokes each origin/source registration exactly once');
      Check(LScenario.First.CanConsume and LScenario.First.CanCapture and
        not LScenario.First.ConsumeRefused and not LScenario.First.CaptureRefused,
        'one active sequential response supports both Consume and pointer capture');
      Check(LScenario.Second.SawConsumed and
        (LDecision.Seal.PointerRequest = nprCapture),
        'consumption is monotonic without suppressing later siblings or capture');
      Check((LScenario.Second.Last.Pointer.ID = 17) and
        (LScenario.Second.Last.Pointer.Modifiers = [nmShift]) and
        (LScenario.Second.Last.Pointer.X = 12.5),
        'canonical callback retains exact physical pointer metadata');
      RetainedResponses(LScenario.First);
      RetainedResponses(LScenario.Second);
    finally
      LScenario.Free;
    end;
  end;
end;

procedure MembershipAndFailureCases;
var
  LScenario: TScenario;
  LDecision: INyxGestureDecision;
  LConsumed: Boolean;
begin
  LScenario := TScenario.Create;
  try
    LScenario.First.Action := paFail;
    LDecision := NewNyxGestureDecision([ngcCapturePointer]);
    LScenario.Facet.DispatchGestureInput(PointerEvent(ntPointerDown),
      'origin🌙', 'source🌙', LDecision, LConsumed);
    Check(LConsumed and (LScenario.FirstToken.LastExecution.Status = nesFailed) and
      (LScenario.SecondToken.LastExecution.Status = nesSucceeded),
      'callback failure preserves its accepted decision and independent siblings');
    Check(not LDecision.CanRequest(ngcCapturePointer),
      'callback failure cannot leave the platform decision open');
    LScenario.First.Action := paSubscribe;
    LDecision := NewNyxGestureDecision([ngcCapturePointer]);
    LScenario.Facet.DispatchGestureInput(PointerEvent(ntPointerDown),
      'origin🌙', 'source🌙', LDecision, LConsumed);
    Check(not LConsumed and (LScenario.Added.Calls = 0),
      'registration added inside a callback is absent from the captured dispatch');
    LDecision := NewNyxGestureDecision([ngcCapturePointer]);
    LScenario.Facet.DispatchGestureInput(PointerEvent(ntPointerDown),
      'origin🌙', 'source🌙', LDecision, LConsumed);
    Check(LScenario.Added.Calls = 1, 'new membership starts with the next physical input');
    LScenario.First.Action := paNavigate;
    LDecision := NewNyxGestureDecision([ngcCapturePointer]);
    LScenario.Facet.DispatchGestureInput(PointerEvent(ntPointerDown),
      'origin🌙', 'source🌙', LDecision, LConsumed);
    Check((LScenario.Second.Calls = 3) and (LScenario.Added.Calls = 1) and
      not LConsumed and not LDecision.CanRequest(ngcCapturePointer),
      'navigation suppresses all not-yet-started captured callbacks and seals the decision');
    LScenario.First.Action := paClose;
    LDecision := NewNyxGestureDecision([ngcCapturePointer]);
    LScenario.Facet.DispatchGestureInput(PointerEvent(ntPointerDown),
      'origin🌙', 'source🌙', LDecision, LConsumed);
    Check((LScenario.First.Calls = 5) and (LScenario.Second.Calls = 3),
      'closing during dispatch is safe and suppresses later registrations');
  finally
    LScenario.Free;
  end;
end;

procedure ObservationAndReentryCases;
var
  LScenario: TScenario;
  LObservation: INyxEventObservation;
  LBefore: TObservation;
  LAfter: TObservation;
  LBeforeLease: INyxEventObserver;
  LAfterLease: INyxEventObserver;
  LDecision: INyxGestureDecision;
  LConsumed: Boolean;
begin
  LScenario := TScenario.Create;
  LBefore := TObservation.Create;
  LBeforeLease := LBefore;
  LBefore.Owner := LScenario;
  LBefore.AddCallback := True;
  LAfter := TObservation.Create;
  LAfterLease := LAfter;
  LAfter.Owner := LScenario;
  try
    Check(Supports(LScenario.Events, INyxEventObservation, LObservation),
      'same router retains its existing optional observation facet');
    LObservation.Observe(NyxControlEvents('origin🌙'), ntPointerDown,
      LBeforeLease, nopBeforeCallbacks, NyxEvent(''));
    LObservation.Observe(NyxControlEvents('origin🌙'), ntPointerDown,
      LAfterLease, nopAfterDispatch, NyxEvent(''));
    LDecision := NewNyxGestureDecision([ngcCapturePointer]);
    LScenario.Facet.DispatchGestureInput(PointerEvent(ntPointerDown),
      'origin🌙', 'source🌙', LDecision, LConsumed);
    Check(not LConsumed and (LBefore.Calls = 1) and (LAfter.Calls = 1) and
      (LBefore.CallbackCount = 2) and (LAfter.CallbackCount = 2),
      'before/main/after share one captured membership and callback count');
    Check((LScenario.First.Calls = 1) and (LScenario.Second.Calls = 1) and
      (LScenario.Added.Calls = 0),
      'before observer cannot insert a callback into the current physical input');
    LDecision := NewNyxGestureDecision([ngcCapturePointer]);
    LScenario.First.Action := paReenter;
    LScenario.Facet.DispatchGestureInput(PointerEvent(ntPointerDown),
      'origin🌙', 'source🌙', LDecision, LConsumed);
    Check(LConsumed and (LDecision.Seal.PointerRequest = nprCapture) and
      (LScenario.First.Calls = 3) and (LScenario.Second.Calls = 3) and
      (LScenario.Added.Calls = 2),
      'nested and outer inputs each invoke their independently captured callbacks once');
    Check((LBefore.Calls = 3) and (LAfter.Calls = 3) and
      (LBefore.CallbackCount = 3) and (LAfter.CallbackCount = 3),
      'nested observation retains ordinary ordering and admitted membership');
    RetainedResponses(LScenario.First);
  finally
    LScenario.Free;
    LBefore.Owner := nil;
    LAfter.Owner := nil;
  end;
end;

procedure AdmissionAndCompatibilityCases;
var
  LScenario: TScenario;
  LDecision: INyxGestureDecision;
  LEvent: TNyxEventInfo;
  LConsumed: Boolean;
  LRejected: Boolean;
  LCase: Integer;
  LDispatch: TNyxDispatch;
begin
  LScenario := TScenario.Create;
  try
    for LCase := 0 to 2 do
    begin
      LEvent := PointerEvent(ntPointerDown);
      case LCase of
        0:
          begin
            LEvent.HasPointer := False;
          end;
        1:
          begin
            LEvent.Pointer.HasPosition := False;
          end;
        2:
          begin
            LEvent.Trigger := ntClick;
          end;
      end;
      LDecision := NewNyxGestureDecision([ngcCapturePointer]);
      LConsumed := True;
      LRejected := False;
      try
        LScenario.Facet.DispatchGestureInput(LEvent, 'origin🌙', 'source🌙',
          LDecision, LConsumed);
      except
        on ENyxSchedule do
        begin
          LRejected := True;
        end;
      end;
      Check(LRejected and not LConsumed and (LScenario.First.Calls = 0) and
        not LDecision.CanRequest(ngcCapturePointer),
        'unsupported or incomplete physical input refuses before callbacks and seals');
    end;
    LEvent := PointerEvent(ntPointerDown);
    LScenario.First.Action := paConsume;
    LDecision := NewNyxGestureDecision([ngcCapturePointer]);
    LScenario.Events.DispatchGesture(LEvent, 'origin🌙', 'source🌙', LDecision);
    Check(not LScenario.First.CanConsume and LScenario.First.ConsumeRefused and
      LScenario.First.CanCapture and (LDecision.Seal.PointerRequest = nprCapture),
      'ordinary DispatchGesture retains its existing capture-only contract');
    LRejected := False;
    try
      LScenario.Events.DispatchInput(LEvent, 'origin🌙', 'source🌙', LConsumed);
    except
      on ENyxSchedule do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LScenario.First.Calls = 1),
      'ordinary DispatchInput does not silently expand its input family');
    LDispatch := Default(TNyxDispatch);
    LDispatch.Info := LEvent;
    LDecision := NewNyxGestureDecision([ngcCapturePointer]);
    LRejected := False;
    try
      DispatchNyxGestureInput(LScenario.Events, nil, LDispatch, LDecision);
    except
      on ENyxSchedule do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and not LDecision.CanRequest(ngcCapturePointer),
      'invalid helper origin/source refuses and retires even an unentered decision');
  finally
    LScenario.Free;
  end;
end;

procedure NodeLifetimeCase;
var
  LScenario: TScenario;
  LDispatch: TNyxDispatch;
  LDecision: INyxGestureDecision;
begin
  GNodesRetired := 0;
  LScenario := TScenario.Create;
  try
    LScenario.Origin := TTrackedNode.Create(nkButton, 'origin🌙');
    LScenario.Origin.AcquireReference;
    LScenario.Origin.ReleaseOwnership;
    LScenario.Source := TTrackedNode.Create(nkCard, 'source🌙');
    LScenario.Source.AcquireReference;
    LScenario.Source.ReleaseOwnership;
    LScenario.OwnsNodes := True;
    LScenario.First.Action := paRetireNodes;
    LDispatch := Default(TNyxDispatch);
    LDispatch.Source := LScenario.Source;
    LDispatch.Info := PointerEvent(ntPointerDown);
    LDecision := NewNyxGestureDecision([ngcCapturePointer]);
    Check(not DispatchNyxGestureInput(LScenario.Events, LScenario.Origin,
      LDispatch, LDecision), 'unconsumed helper returns the actual default decision');
    Check((GNodesRetired = 2) and (LScenario.Second.Calls = 1),
      'nodes retire only after all synchronous callbacks and helper leases finish');
  finally
    LScenario.Free;
  end;
end;

{$ifdef PAS2JS}
function NextTurn: TJSPromise;
begin
  Result := TJSPromise.new(procedure(AResolve, AReject: TJSPromiseResolver)
    begin
      window.setTimeout(procedure
        begin
          AResolve(True);
        end, 0);
    end);
end;
{$endif}

procedure DeferredCase; {$ifdef PAS2JS}async;{$endif}
var
  LScenario: TScenario;
  LDecision: INyxGestureDecision;
  LConsumed: Boolean;
  LTurns: Integer;
begin
  LScenario := TScenario.Create;
  try
    LScenario.Events.OnPointerDown(NyxControlEvents('origin🌙')).Policy(neUIQueue);
    LScenario.First.Action := paConsume;
    LDecision := NewNyxGestureDecision([ngcCapturePointer]);
    LScenario.Facet.DispatchGestureInput(PointerEvent(ntPointerDown),
      'origin🌙', 'source🌙', LDecision, LConsumed);
    Check(not LConsumed and (LScenario.First.Calls = 0) and
      not LDecision.CanRequest(ngcCapturePointer),
      'queued callback cannot hold the synchronous physical decision open');
    LTurns := 0;
    while (LScenario.FirstToken.LastExecution.Status = nesPending) and (LTurns < 100) do
    begin
      Inc(LTurns);
      {$ifdef PAS2JS}await(NextTurn);{$else}CheckSynchronize(10);{$endif}
    end;
    Check((LScenario.First.Calls = 1) and
      (LScenario.FirstToken.LastExecution.Status = nesSucceeded),
      'actual platform UI queue delivers its callback exactly once');
    Check(not LScenario.First.CanConsume and not LScenario.First.CanCapture and
      LScenario.First.ConsumeRefused and LScenario.First.CaptureRefused,
      'deferred Consume and capture both refuse explicitly');
    RetainedResponses(LScenario.First);
  finally
    LScenario.Free;
  end;
end;

procedure Run; {$ifdef PAS2JS}async;{$endif}
begin
  try
    SynchronousCases;
    MembershipAndFailureCases;
    ObservationAndReentryCases;
    AdmissionAndCompatibilityCases;
    NodeLifetimeCase;
    {$ifdef PAS2JS}await(DeferredCase);{$else}DeferredCase;{$endif}
    {$ifdef PAS2JS}
    document.body.setAttribute('data-gesture-input', 'passed');
    document.body.setAttribute('data-gesture-input-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' canonical gesture input checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-gesture-input', 'failed');
      document.body.setAttribute('data-gesture-input-error', LException.Message);
      {$else}
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end;

begin
  Run;
end.
