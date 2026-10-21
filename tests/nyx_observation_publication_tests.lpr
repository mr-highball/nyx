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

program nyx_observation_publication_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.types, nyx.data, nyx.events, nyx.behavior,
  nyx.scheduler, nyx.publication
  {$ifdef PAS2JS}, Web{$endif};

type
  { Retained ordinary observer records call order only. It owns no router,
    document or receiver and cannot grant model or execution authority. }
  TObserver = class(TNyxEventObserver)
  public
    Caption: TNyxText;
    Calls: Integer;
    CallbackCount: Integer;
    procedure Observe(const AEvent: TNyxEventInfo; ACallbackCount: Integer); override;
  end;

  { Ordinary callback can publish an already prepared fixture change. The
    dispatch snapshot owns this callback; its field never retains a tree. }
  TPublishCallback = class(TNyxEventCallback)
  public
    Pending: INyxPreparedPublication;
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution); override;
  end;

  { Later participant either refuses admission or explicitly closes the router
    during validation. Neither action is hidden in production code. }
  TRefusal = class(TInterfacedObject, INyxPreparedPublication)
  public
    Closing: INyxEvents;
    procedure Validate;
    procedure Install;
    procedure Notify;
    procedure Retire;
  end;

  { A plain callback records exact dispatch order without owning its router. }
  TTraceCallback = class(TNyxEventCallback)
  public
    Caption: TNyxText;
    Calls: Integer;
    procedure Invoke(const AEvent: TNyxEventInfo;
      const AExecution: INyxExecution); override;
  end;

  { Controlled pending tickets permit deterministic cancellation failures and
    same-length execution replacement, without native timers or worker threads. }
  TControlledExecution = class(TInterfacedObject, INyxExecution)
  public
    WasCancelled: Boolean;
    StatusValue: TNyxExecutionStatus;
    FailCancellation: Boolean;
    CancelCalls: Integer;
    procedure Cancel;
    function GetCancelled: Boolean;
    function GetStatus: TNyxExecutionStatus;
    function GetFailure: TNyxText;
  end;

  { One borrowed UI admission hook exercises public receiver/input leases. }
  TUIAdmission = procedure of object;

  { Deterministic scheduler retains only execution tickets. These tests never
    pump deferred work; policy is observed without substituting UI execution. }
  TControlledScheduler = class(TInterfacedObject, INyxScheduler)
  public
    Tickets: TNyxExecutions;
    States: array of TControlledExecution;
    OnUI: TUIAdmission;
    function GetCapabilities: TNyxSchedulerCapabilities;
    procedure RequireUI;
    procedure Admit(APolicy: TNyxExecutionPolicy);
    function Submit(const AWork: INyxWork;
      APolicy: TNyxExecutionPolicy): INyxExecution;
    function PostUI(const AWork: INyxWork;
      const AParent: INyxExecution = nil): INyxExecution;
    procedure Shutdown;
  end;

  { One later validator changes only the named existing router authority. Its
    ordinary dispatch is real work which the final readiness fence must detect. }
  TViewValidationAction = (vvaDispatch, vvaCancelPending, vvaClose,
    vvaCancelSlot, vvaCancelFuture, vvaReplaceExecution);

  { The publication group owns this explicit hostile participant through all
    phases. No mutation occurs in Install, Notify or Retire. }
  TViewValidator = class(TInterfacedObject, INyxPreparedPublication)
  public
    Events: INyxEvents;
    Token: INyxEventSubscription;
    Generation: INyxCancellationScope;
    Previous: TControlledExecution;
    Action: TViewValidationAction;
    procedure Validate;
    procedure Install;
    procedure Notify;
    procedure Retire;
  end;

  { The scheduler borrows this fixture callback while the public method keeps
    its own leases. It clears the last caller slots, not the operation's values. }
  TReleaseInputs = class
  public
    procedure Release;
    procedure Retire;
  end;

var
  GChecks: Integer;
  GTrace: TNyxText;
  GViewPlan: INyxPreparedEventView;
  GViewEvents: INyxEvents;
  GViewTarget: TNyxEventTarget;
  GViewCallback: INyxEventCallback;
  GViewName: TNyxEventRef;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Observation publication: ' + AReason);
  end;
  Inc(GChecks);
end;

procedure TObserver.Observe(const AEvent: TNyxEventInfo; ACallbackCount: Integer);
begin
  Inc(Calls);
  CallbackCount := ACallbackCount;
  GTrace := GTrace + Caption;
end;

procedure TPublishCallback.Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution);
var
  LPlan: INyxPreparedPublication;
begin
  GTrace := GTrace + 'C';
  LPlan := Pending;
  Pending := nil;

  if LPlan <> nil then
  begin
    PublishNyxGroup([LPlan]);
  end;
end;

procedure TRefusal.Validate;
begin

  if Closing <> nil then
  begin
    Closing.Close;
    Exit;
  end;
  raise ENyxSchedule.Create('Deliberate later-participant refusal');
end;

procedure TRefusal.Install;
begin
end;

procedure TRefusal.Notify;
begin
end;

procedure TRefusal.Retire;
begin
end;

procedure Click(const AEvents: INyxEvents);
var
  LEvent: TNyxEventInfo;
begin
  LEvent := Default(TNyxEventInfo);
  LEvent.Name := NyxEvent('click');
  LEvent.Trigger := ntClick;
  LEvent.OriginID := 'button';
  LEvent.SourceID := 'button';
  LEvent.TargetID := 'button';
  LEvent.Value := NyxNull;
  LEvent.Details := NyxNull;
  AEvents.Dispatch(LEvent, 'button', 'button');
end;

procedure TTraceCallback.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
begin
  Inc(Calls);
  GTrace := GTrace + Caption;
end;

procedure TControlledExecution.Cancel;
begin
  Inc(CancelCalls);

  if FailCancellation then
  begin
    raise ENyxSchedule.Create('Deliberate execution cancellation failure');
  end;
  WasCancelled := True;
  StatusValue := nesCancelled;
end;

function TControlledExecution.GetCancelled: Boolean;
begin
  Result := WasCancelled;
end;

function TControlledExecution.GetStatus: TNyxExecutionStatus;
begin
  Result := StatusValue;
end;

function TControlledExecution.GetFailure: TNyxText;
begin
  Result := '';
end;

function TControlledScheduler.GetCapabilities: TNyxSchedulerCapabilities;
begin
  Result := [nscDeferredUI];
end;

procedure TControlledScheduler.RequireUI;
var
  LHook: TUIAdmission;
begin
  LHook := OnUI;
  OnUI := nil;

  if Assigned(LHook) then
  begin
    LHook;
  end;
end;

procedure TControlledScheduler.Admit(APolicy: TNyxExecutionPolicy);
begin

  if APolicy = neThreaded then
  begin
    raise ENyxSchedule.Create('Controlled scheduler has no worker threads');
  end;
end;

function TControlledScheduler.Submit(const AWork: INyxWork;
  APolicy: TNyxExecutionPolicy): INyxExecution;
var
  LState: TControlledExecution;
  LCount: Integer;
begin
  Admit(APolicy);
  LState := TControlledExecution.Create;
  Result := LState;
  LState.StatusValue := nesPending;
  LCount := Length(Tickets);
  SetLength(Tickets, LCount + 1);
  SetLength(States, LCount + 1);
  Tickets[LCount] := Result;
  States[LCount] := LState;

  if APolicy = neSequential then
  begin
    LState.StatusValue := nesRunning;
    AWork.Execute(Result);

    if not LState.WasCancelled then
    begin
      LState.StatusValue := nesSucceeded;
    end;
  end;
end;

function TControlledScheduler.PostUI(const AWork: INyxWork;
  const AParent: INyxExecution): INyxExecution;
begin
  Result := Submit(AWork, neUIQueue);
end;

procedure TControlledScheduler.Shutdown;
var
  LIndex: Integer;
begin
  for LIndex := 0 to High(Tickets) do
  begin
    Tickets[LIndex].Cancel;
  end;
end;

procedure TViewValidator.Validate;
begin
  case Action of
    vvaDispatch:
      begin
        Click(Events);
      end;
    vvaCancelPending:
      begin
        Events.CancelPending;
      end;
    vvaClose:
      begin
        Events.Close;
      end;
    vvaCancelSlot:
      begin
        Token.Cancel;
      end;
    vvaCancelFuture:
      begin
        Generation.Cancel;
      end;
    vvaReplaceExecution:
      begin
        Previous.StatusValue := nesSucceeded;
        Click(Events);
      end;
  end;
end;

procedure TViewValidator.Install;
begin
end;

procedure TViewValidator.Notify;
begin
end;

procedure TViewValidator.Retire;
begin
end;

procedure TReleaseInputs.Release;
begin
  GViewPlan := nil;
  GViewEvents := nil;
  GViewCallback := nil;
  GViewTarget := NyxControlEvents('replaced');
  GViewName := NyxEvent('replaced');
end;

procedure TReleaseInputs.Retire;
begin
  GViewPlan.Retire;
  Release;
end;

procedure Run;
var
  LEvents: INyxEvents;
  LOther: INyxEvents;
  LEditing: INyxEventObservationEditing;
  LObservation: INyxEventObservation;
  LFirst: TObserver;
  LNext: TObserver;
  LFirstLease: INyxEventObserver;
  LNextLease: INyxEventObserver;
  LToken: INyxEventSubscription;
  LExternal: INyxEventSubscription;
  LCallbackToken: INyxEventSubscription;
  LCallback: TPublishCallback;
  LCallbackLease: INyxEventCallback;
  LPlan: INyxPreparedPublication;
  LReject: INyxPreparedPublication;
  LClosing: TRefusal;
  LFailed: Boolean;
begin
  LEvents := NewNyxEvents;
  LOther := NewNyxEvents(LEvents.Scheduler);
  try
    Check(Supports(LEvents, INyxEventObservationEditing, LEditing),
      'the standard event router exposes optional prepared observations');
    Supports(LEvents, INyxEventObservation, LObservation);
    LFirst := TObserver.Create;
    LFirstLease := LFirst;
    LFirst.Caption := 'A';
    LNext := TObserver.Create;
    LNextLease := LNext;
    LNext.Caption := 'B';
    LPlan := LEditing.PrepareObservation(NyxControlEvents('button'), ntClick,
      LFirstLease, nopAfterDispatch, NyxEvent(''), LToken);
    Check(not LToken.Active and not LEvents.HasSubscribers(ntClick),
      'reserved slots grant neither active token nor subscriber/default admission');
    Click(LEvents);
    Check(LFirst.Calls = 0, 'ordinary pruning/dispatch cannot execute a reserved observer');
    LExternal := LObservation.Observe(NyxControlEvents('button'), ntClick,
      LNextLease, nopAfterDispatch, NyxEvent(''));
    GTrace := '';
    Click(LEvents);
    Check(GTrace = 'B', 'a later independent observation stays active during preparation');
    PublishNyxGroup([LPlan]);
    LPlan := nil;
    GTrace := '';
    Click(LEvents);
    Check(LToken.Active and (GTrace = 'AB'),
      'installation activates the reserved ordered slot without replacing independent observers');

    LPlan := LEditing.PrepareObservationRemoval(LToken);
    LFailed := False;
    LReject := TRefusal.Create;
    try
      PublishNyxGroup([LPlan, LReject]);
    except
      on E: ENyxSchedule do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and LToken.Active, 'later refusal cannot disable an existing observer');
    LPlan := nil;
    LReject := nil;
    GTrace := '';
    Click(LEvents);
    Check(GTrace = 'AB', 'refused removal retains exact dispatch order');
    LPlan := LEditing.PrepareObservationRemoval(LToken);
    PublishNyxGroup([LPlan]);
    LPlan := nil;
    GTrace := '';
    Click(LEvents);
    Check(not LToken.Active and (GTrace = 'B'), 'committed removal revokes exactly its own observer');
    LExternal.Cancel;
    LExternal := nil;
    Check(not LEvents.HasSubscribers(ntClick), 'retired observations do not keep host defaults opted in');

    LPlan := LEditing.PrepareObservation(NyxControlEvents('button'), ntClick,
      LFirstLease, nopAfterDispatch, NyxEvent(''), LToken);
    LPlan := nil;
    GTrace := '';
    Click(LEvents);
    Check(not LToken.Active and (GTrace = ''), 'abandoned activation retires only its inert slot');

    LPlan := LEditing.PrepareObservation(NyxControlEvents('button'), ntClick,
      LFirstLease, nopAfterDispatch, NyxEvent(''), LToken);
    LToken.Cancel;
    LFailed := False;
    try
      PublishNyxGroup([LPlan]);
    except
      on E: ENyxSchedule do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and not LToken.Active, 'cancelled reservations cannot reactivate');
    LPlan := nil;

    LCallback := TPublishCallback.Create;
    LCallbackLease := LCallback;
    LCallbackToken := LEvents.On(NyxControlEvents('button'), ntClick).Subscribe(LCallbackLease);
    LCallback.Pending := LEditing.PrepareObservation(NyxControlEvents('button'), ntClick,
      LFirstLease, nopAfterDispatch, NyxEvent(''), LToken);
    GTrace := '';
    Click(LEvents);
    Check(LToken.Active and (GTrace = 'C'), 'mid-dispatch publication starts on the next event snapshot');
    GTrace := '';
    Click(LEvents);
    Check(GTrace = 'CA', 'the next ordinary event observes the newly installed route slot');
    LFailed := False;
    try
      LPlan := LEditing.PrepareObservationRemoval(LCallbackToken);
    except
      on E: ENyxSchedule do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and LCallbackToken.Active, 'observation editing cannot silently edit ordinary callback streams');
    LCallbackToken.Cancel;
    LCallbackToken := nil;
    LCallbackLease := nil;
    LCallback := nil;
    LToken.Cancel;

    Supports(LOther, INyxEventObservation, LObservation);
    LExternal := LObservation.Observe(NyxControlEvents('button'), ntClick,
      LNextLease, nopAfterDispatch, NyxEvent(''));
    LFailed := False;
    try
      LPlan := LEditing.PrepareObservationRemoval(LExternal);
    except
      on E: ENyxSchedule do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and LExternal.Active, 'another router token supplies no removal authority');
    LPlan := LEditing.PrepareObservation(NyxControlEvents('button'), ntClick,
      LFirstLease, nopAfterDispatch, NyxEvent(''), LToken);
    LClosing := TRefusal.Create;
    LReject := LClosing;
    LClosing.Closing := LEvents;
    LFailed := False;
    try
      PublishNyxGroup([LPlan, LReject]);
    except
      on E: ENyxSchedule do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and not LToken.Active and not LEvents.HasSubscribers(ntClick),
      'later owner retirement is caught by the final readiness fence');
  finally
    LPlan := nil;
    LReject := nil;
    LCallbackToken := nil;
    LCallbackLease := nil;
    LToken := nil;
    LExternal := nil;
    LFirstLease := nil;
    LNextLease := nil;
    LEditing := nil;
    LObservation := nil;
    LOther.Close;
    LEvents.Close;
    LOther := nil;
    LEvents := nil;
  end;
end;

procedure ViewStagingCases;
var
  LEvents: INyxEvents;
  LPreparation: INyxEventViewPreparation;
  LPlan: INyxPreparedEventView;
  LOtherPlan: INyxPreparedEventView;
  LPrevious: INyxCancellationScope;
  LFuture: INyxCancellationScope;
  LStream: INyxEventStream;
  LOldCallback: TTraceCallback;
  LNextCallback: TTraceCallback;
  LOldLease: INyxEventCallback;
  LNextLease: INyxEventCallback;
  LObserver: TObserver;
  LNextObserver: TObserver;
  LObserverLease: INyxEventObserver;
  LNextObserverLease: INyxEventObserver;
  LObservation: INyxEventObservation;
  LCallbackToken: INyxEventSubscription;
  LObserverToken: INyxEventSubscription;
  LToken: INyxEventSubscription;
  LStagedObserver: INyxEventSubscription;
  LFailed: Boolean;
  LRevision: Integer;
begin
  LEvents := NewNyxEvents;
  try
    Check(Supports(LEvents, INyxEventViewPreparation, LPreparation),
      'the ordinary router exposes optional successor-view preparation');
    LPrevious := NyxEventGeneration(LEvents);
    LRevision := LEvents.ViewRevision;
    LOldCallback := TTraceCallback.Create;
    LOldLease := LOldCallback;
    LOldCallback.Caption := 'A';
    LNextCallback := TTraceCallback.Create;
    LNextLease := LNextCallback;
    LNextCallback.Caption := 'B';
    LStream := LEvents.On(NyxControlEvents('button'), ntClick);
    LCallbackToken := LStream.Subscribe(LOldLease);
    LObserver := TObserver.Create;
    LObserverLease := LObserver;
    LObserver.Caption := 'O';
    LNextObserver := TObserver.Create;
    LNextObserverLease := LNextObserver;
    LNextObserver.Caption := 'N';
    Supports(LEvents, INyxEventObservation, LObservation);
    LObserverToken := LObservation.Observe(NyxControlEvents('button'), ntClick,
      LObserverLease, nopBeforeCallbacks, NyxEvent(''));
    LPlan := LPreparation.PrepareView;
    LFuture := LPlan.Generation;
    LToken := LPlan.PrepareCallback(NyxControlEvents('button'), ntClick,
      LNextLease, NyxEvent(''));
    LStagedObserver := LPlan.PrepareObservation(NyxControlEvents('button'), ntClick,
      LNextObserverLease, nopAfterDispatch, NyxEvent(''));
    Check((LPrevious <> LFuture) and not LPrevious.Cancelled and
      not LFuture.Cancelled and (NyxEventGeneration(LEvents) = LPrevious),
      'preparation owns an independent prospective generation without retargeting the view');
    Check(not LToken.Active and not LStagedObserver.Active and (LStream.Count = 1),
      'both staged registration families remain absent from ordinary stream discovery');
    GTrace := '';
    Click(LEvents);
    Check((GTrace = 'OA') and (LObserver.CallbackCount = 1),
      'dispatch snapshots and observer callback counts cannot see dormant callbacks');
    LFailed := False;
    try
      LOtherPlan := LPreparation.PrepareView;
    except
      on E: ENyxSchedule do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (LOtherPlan = nil) and not LPrevious.Cancelled,
      'a competing reservation refuses without replacing the mounted scope');
    PublishNyxGroup([LPlan]);
    Check(LPrevious.Cancelled and not LFuture.Cancelled and LToken.Active and
      LStagedObserver.Active and (NyxEventGeneration(LEvents) = LFuture) and
      (LEvents.ViewRevision = LRevision + 1),
      'installation revokes the old scope and publishes exactly the prepared successor');
    LPlan.Retire;
    LPlan := nil;
    GTrace := '';
    Click(LEvents);
    Check((GTrace = 'OABN') and (LObserver.CallbackCount = 2) and
      LToken.Active and LCallbackToken.Active and LObserverToken.Active,
      'retirement preserves installed slots, independent registrations and ordered phases');
    LFuture.Cancel;
    GTrace := '';
    Click(LEvents);
    Check((GTrace = '') and LFuture.Cancelled,
      'cancelling the installed successor prevents all old-view work from entering');
  finally
    LOtherPlan := nil;
    LPlan := nil;
    LPreparation := nil;
    LStream := nil;
    LObservation := nil;
    LEvents.Close;
    LEvents := nil;
  end;
end;

procedure ViewAbortCases;
var
  LEvents: INyxEvents;
  LPreparation: INyxEventViewPreparation;
  LPlan: INyxPreparedEventView;
  LPrevious: INyxCancellationScope;
  LFuture: INyxCancellationScope;
  LCallback: TTraceCallback;
  LCallbackLease: INyxEventCallback;
  LToken: INyxEventSubscription;
  LReject: INyxPreparedPublication;
  LFailed: Boolean;

  procedure Abandon;
  var
    LAbandoned: INyxPreparedEventView;
  begin
    LAbandoned := LPreparation.PrepareView;
    LFuture := LAbandoned.Generation;
    LToken := LAbandoned.PrepareCallback(NyxControlEvents('button'), ntClick,
      LCallbackLease, NyxEvent(''));
    Check(not LEvents.HasSubscribers(ntClick),
      'a staged-only callback does not opt a host into physical dispatch');
  end;

begin
  LEvents := NewNyxEvents;
  try
    Supports(LEvents, INyxEventViewPreparation, LPreparation);
    LPrevious := NyxEventGeneration(LEvents);
    LCallback := TTraceCallback.Create;
    LCallbackLease := LCallback;
    LCallback.Caption := 'B';
    { Helper scope ends FPC managed function-result temporaries before checking
      abandonment, independently of the destination interface's lifetime. }
    Abandon;
    Check(LFuture.Cancelled and not LPrevious.Cancelled and not LToken.Active and
      (NyxEventGeneration(LEvents) = LPrevious),
      'abandonment revokes only prospective scope and staged tokens');
    LPlan := LPreparation.PrepareView;
    LFuture := LPlan.Generation;
    LToken := LPlan.PrepareCallback(NyxControlEvents('button'), ntClick,
      LCallbackLease, NyxEvent(''));
    LReject := TRefusal.Create;
    LFailed := False;
    try
      PublishNyxGroup([LPlan, LReject]);
    except
      on E: ENyxSchedule do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and LFuture.Cancelled and not LPrevious.Cancelled and
      not LToken.Active and (NyxEventGeneration(LEvents) = LPrevious),
      'later ordinary admission refusal preserves the mounted view and retires staged work');
    LReject := nil;
    LPlan := nil;
    LPlan := LPreparation.PrepareView;
    LToken := LPlan.PrepareCallback(NyxControlEvents('button'), ntClick,
      LCallbackLease, NyxEvent(''));
    LPlan.Validate;
    LFailed := False;
    try
      LPlan.PrepareCallback(NyxControlEvents('button'), ntClick,
        LCallbackLease, NyxEvent(''));
    except
      on E: ENyxSchedule do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and not LToken.Active,
      'Validate seals callback admission without activating its existing reservation');
    LFailed := False;
    try
      LPlan.Validate;
    except
      on E: ENyxSchedule do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed, 'view validation is single use');
    LPlan.Retire;
    LPlan.Retire;
    LPlan := nil;
    Check(not LPrevious.Cancelled and not LToken.Active,
      'explicit repeated retirement cannot alter the still-mounted generation');
    LPlan := LPreparation.PrepareView;
    LFailed := False;
    try
      LPlan.PrepareCallback(NyxControlEvents('button'), ntClick,
        LCallbackLease, NyxEvent('not-a-physical-name'));
    except
      on E: ENyxSchedule do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and not LEvents.HasSubscribers(ntClick),
      'invalid physical-name payload refuses without creating a live default');
    LToken := LPlan.PrepareCallback(NyxControlEvents('button'), ntNamed,
      LCallbackLease, NyxNamedEvent('custom-card-move'));
    Check(not LToken.Active, 'a checked named callback can also be staged');
    LPlan.Retire;
    Check(not LToken.Active and not LPrevious.Cancelled,
      'retiring a named reservation preserves the mounted owner');
    LPlan := nil;
    LPlan := LPreparation.PrepareView;
    LFuture := LPlan.Generation;
    PublishNyxGroup([LPlan]);
    Check(LPrevious.Cancelled and not LFuture.Cancelled and
      (NyxEventGeneration(LEvents) = LFuture) and not LEvents.HasSubscribers(ntClick),
      'an empty successor still publishes one generation without inventing registrations');
  finally
    LReject := nil;
    LPlan := nil;
    LPreparation := nil;
    LEvents.Close;
    LEvents := nil;
  end;
end;

procedure ViewFenceCases;
var
  LEvents: INyxEvents;
  LPreparation: INyxEventViewPreparation;
  LPlan: INyxPreparedEventView;
  LPrevious: INyxCancellationScope;
  LFuture: INyxCancellationScope;
  LCallbackLease: INyxEventCallback;
  LOldToken: INyxEventSubscription;
  LToken: INyxEventSubscription;
  LValidator: TViewValidator;
  LValidatorLease: INyxPreparedPublication;
  LAction: TViewValidationAction;
  LFailed: Boolean;
begin
  for LAction := vvaDispatch to vvaCancelFuture do
  begin
    LEvents := NewNyxEvents;
    try
      Supports(LEvents, INyxEventViewPreparation, LPreparation);
      LCallbackLease := TTraceCallback.Create;
      LOldToken := LEvents.On(NyxControlEvents('button'), ntClick).Subscribe(LCallbackLease);
      LPrevious := NyxEventGeneration(LEvents);
      LPlan := LPreparation.PrepareView;
      LFuture := LPlan.Generation;
      LToken := LPlan.PrepareCallback(NyxControlEvents('button'), ntClick,
        LCallbackLease, NyxEvent(''));
      LValidator := TViewValidator.Create;
      LValidatorLease := LValidator;
      LValidator.Events := LEvents;
      LValidator.Token := LToken;
      LValidator.Generation := LFuture;
      LValidator.Action := LAction;
      LFailed := False;
      try
        PublishNyxGroup([LPlan, LValidatorLease]);
      except
        on E: ENyxSchedule do
        begin
          LFailed := True;
        end;
      end;
      Check(LFailed and not LToken.Active and LFuture.Cancelled,
        'later validator authority change refuses successor action ' + IntToStr(Ord(LAction)));

      if LAction in [vvaDispatch, vvaCancelSlot, vvaCancelFuture] then
      begin
        Check(not LPrevious.Cancelled and LOldToken.Active and
          (NyxEventGeneration(LEvents) = LPrevious),
          'non-navigation refusal preserves old owner for action ' + IntToStr(Ord(LAction)));
      end;
    finally
      LValidatorLease := nil;
      LValidator := nil;
      LPlan := nil;
      LPreparation := nil;
      LOldToken := nil;
      LToken := nil;
      LEvents.Close;
      LEvents := nil;
    end;
  end;
end;

procedure ViewPolicyAndExecutionCases;
var
  LScheduler: TControlledScheduler;
  LSchedulerLease: INyxScheduler;
  LEvents: INyxEvents;
  LPreparation: INyxEventViewPreparation;
  LPlan: INyxPreparedEventView;
  LPrevious: INyxCancellationScope;
  LFuture: INyxCancellationScope;
  LStream: INyxEventStream;
  LCallback: TTraceCallback;
  LCallbackLease: INyxEventCallback;
  LToken: INyxEventSubscription;
  LOldToken: INyxEventSubscription;
  LValidator: TViewValidator;
  LValidatorLease: INyxPreparedPublication;
  LFailed: Boolean;
begin
  LScheduler := TControlledScheduler.Create;
  LSchedulerLease := LScheduler;
  LEvents := NewNyxEvents(LSchedulerLease);
  try
    Supports(LEvents, INyxEventViewPreparation, LPreparation);
    LStream := LEvents.On(NyxControlEvents('button'), ntClick).Policy(neUIQueue);
    LCallback := TTraceCallback.Create;
    LCallbackLease := LCallback;
    LOldToken := LStream.Subscribe(LCallbackLease);
    Click(LEvents);
    Check((Length(LScheduler.Tickets) = 1) and (LCallback.Calls = 0),
      'the real router submits but does not execute controlled queued callbacks');
    LPrevious := NyxEventGeneration(LEvents);
    LPlan := LPreparation.PrepareView;
    LFuture := LPlan.Generation;
    LToken := LPlan.PrepareCallback(NyxControlEvents('button'), ntClick,
      LCallbackLease, NyxEvent(''));
    Check(LStream.ExecutionPolicy = neUIQueue,
      'preparing an ordinary callback leaves its caller-owned stream policy exact');
    LValidator := TViewValidator.Create;
    LValidatorLease := LValidator;
    LValidator.Events := LEvents;
    LValidator.Previous := LScheduler.States[0];
    LValidator.Action := vvaReplaceExecution;
    LFailed := False;
    try
      PublishNyxGroup([LPlan, LValidatorLease]);
    except
      on E: ENyxSchedule do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and not LPrevious.Cancelled and LFuture.Cancelled and
      not LToken.Active and (Length(LScheduler.Tickets) = 2),
      'same-length replacement of a completed execution is rejected by exact ticket identity');
    LPlan := nil;
    LValidatorLease := nil;
    LValidator := nil;
    LOldToken.Cancel;
    LOldToken := LStream.Subscribe(LCallbackLease);
    LToken := LStream.Subscribe(LCallbackLease);
    Click(LEvents);
    Check(Length(LScheduler.Tickets) = 4,
      'two current pending executions are available for the committed cancellation boundary');
    LScheduler.States[2].FailCancellation := True;
    LPlan := LPreparation.PrepareView;
    LFuture := LPlan.Generation;
    LToken := LPlan.PrepareCallback(NyxControlEvents('button'), ntClick,
      LCallbackLease, NyxEvent(''));
    LFailed := False;
    try
      PublishNyxGroup([LPlan]);
    except
      on E: ENyxPublicationNotification do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and LPrevious.Cancelled and not LFuture.Cancelled and
      (NyxEventGeneration(LEvents) = LFuture) and LToken.Active,
      'notification failure reports committed successor publication rather than rollback');
    Check((LScheduler.States[2].CancelCalls = 1) and
      (LScheduler.States[3].CancelCalls = 1) and LScheduler.Tickets[3].Cancelled,
      'all captured old executions receive cancellation despite an earlier failure');
    LPlan.Retire;
    LPlan := nil;
    Check(LToken.Active and not LFuture.Cancelled and (LCallback.Calls = 0),
      'retirement neither cancels installed slots nor fabricates queued execution');
    Check(LStream.ExecutionPolicy = neUIQueue,
      'publication and cancellation failure preserve the original policy');
  finally

    if Length(LScheduler.States) > 2 then
    begin
      LScheduler.States[2].FailCancellation := False;
    end;
    LPlan := nil;
    LValidatorLease := nil;
    LStream := nil;
    LPreparation := nil;
    LEvents.Close;
    LEvents := nil;
    LSchedulerLease := nil;
  end;
end;

procedure CaptureReleasedViewInputs(out AToken: INyxEventSubscription;
  out AGeneration: INyxCancellationScope);
var
  LScheduler: TControlledScheduler;
  LSchedulerLease: INyxScheduler;
  LPreparation: INyxEventViewPreparation;
  LRelease: TReleaseInputs;
begin
  LScheduler := TControlledScheduler.Create;
  LSchedulerLease := LScheduler;
  GViewEvents := NewNyxEvents(LSchedulerLease);
  Supports(GViewEvents, INyxEventViewPreparation, LPreparation);
  GViewPlan := LPreparation.PrepareView;
  LPreparation := nil;
  AGeneration := GViewPlan.Generation;
  GViewTarget := NyxControlEvents('button');
  GViewName := NyxEvent('');
  GViewCallback := TTraceCallback.Create;
  LRelease := TReleaseInputs.Create;
  try
    LScheduler.OnUI := LRelease.Release;
    AToken := GViewPlan.PrepareCallback(GViewTarget, ntClick, GViewCallback, GViewName);
  finally
    LScheduler.OnUI := nil;
    LRelease.Free;
    GViewPlan := nil;
    GViewEvents := nil;
    GViewCallback := nil;
  end;
end;

procedure ViewLifetimeAndSnapshotCases;
var
  LEvents: INyxEvents;
  LPreparation: INyxEventViewPreparation;
  LPlan: INyxPreparedEventView;
  LCallback: TPublishCallback;
  LCallbackLease: INyxEventCallback;
  LNextCallback: TTraceCallback;
  LNextLease: INyxEventCallback;
  LToken: INyxEventSubscription;
  LGeneration: INyxCancellationScope;
begin
  CaptureReleasedViewInputs(LToken, LGeneration);
  Check((GViewPlan = nil) and (GViewEvents = nil) and (GViewCallback = nil) and
    LGeneration.Cancelled and not LToken.Active,
    'last caller receiver/input release cannot outlive or reactivate the abandoned reservation');
  LEvents := NewNyxEvents;
  try
    Supports(LEvents, INyxEventViewPreparation, LPreparation);
    LCallback := TPublishCallback.Create;
    LCallbackLease := LCallback;
    LEvents.On(NyxControlEvents('button'), ntClick).Subscribe(LCallbackLease);
    LNextCallback := TTraceCallback.Create;
    LNextLease := LNextCallback;
    LNextCallback.Caption := 'B';
    LPlan := LPreparation.PrepareView;
    LToken := LPlan.PrepareCallback(NyxControlEvents('button'), ntClick,
      LNextLease, NyxEvent(''));
    LCallback.Pending := LPlan;
    LPlan := nil;
    GTrace := '';
    Click(LEvents);
    Check(LToken.Active and (GTrace = 'C') and (LNextCallback.Calls = 0),
      'a successor installed inside a callback cannot enter the already captured dispatch');
    GTrace := '';
    Click(LEvents);
    Check((GTrace = 'CB') and (LNextCallback.Calls = 1),
      'the next dispatch captures the accepted generation and new callback exactly once');
  finally
    LPlan := nil;
    LPreparation := nil;
    LEvents.Close;
    LEvents := nil;
  end;
end;

procedure ViewReentrantRetirementCase;
var
  LScheduler: TControlledScheduler;
  LSchedulerLease: INyxScheduler;
  LEvents: INyxEvents;
  LPreparation: INyxEventViewPreparation;
  LObserver: INyxEventObserver;
  LFuture: INyxCancellationScope;
  LPrevious: INyxCancellationScope;
  LRelease: TReleaseInputs;
  LFailed: Boolean;
begin
  LScheduler := TControlledScheduler.Create;
  LSchedulerLease := LScheduler;
  LEvents := NewNyxEvents(LSchedulerLease);
  LRelease := TReleaseInputs.Create;
  try
    Supports(LEvents, INyxEventViewPreparation, LPreparation);
    LPrevious := NyxEventGeneration(LEvents);
    GViewPlan := LPreparation.PrepareView;
    LFuture := GViewPlan.Generation;
    LObserver := TObserver.Create;
    LScheduler.OnUI := LRelease.Retire;
    LFailed := False;
    try
      GViewPlan.PrepareObservation(NyxControlEvents('button'), ntClick,
        LObserver, nopAfterDispatch, NyxEvent(''));
    except
      on E: ENyxSchedule do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (GViewPlan = nil) and LFuture.Cancelled and
      not LPrevious.Cancelled and not LEvents.HasSubscribers(ntClick),
      'retirement inside admission cannot leak the subsequently created staged observer');
    GViewPlan := LPreparation.PrepareView;
    Check(not GViewPlan.Generation.Cancelled,
      'reentrant retirement releases the reservation for an independent future plan');
    GViewPlan.Retire;
    GViewPlan := nil;
  finally
    GViewPlan := nil;
    LScheduler.OnUI := nil;
    LRelease.Free;
    LPreparation := nil;
    LEvents.Close;
    LEvents := nil;
    LSchedulerLease := nil;
  end;
end;

procedure ViewInstalledRetirementCase;
var
  LScheduler: TControlledScheduler;
  LSchedulerLease: INyxScheduler;
  LEvents: INyxEvents;
  LPreparation: INyxEventViewPreparation;
  LCallback: INyxEventCallback;
  LFuture: INyxCancellationScope;
  LToken: INyxEventSubscription;
  LStream: INyxEventStream;

  procedure InstallAndAbandon;
  var
    LPlan: INyxPreparedEventView;
    LFence: INyxPublicationReadiness;
  begin
    LPlan := LPreparation.PrepareView;
    LFuture := LPlan.Generation;
    LToken := LPlan.PrepareCallback(NyxControlEvents('button'), ntClick,
      LCallback, NyxEvent(''));
    LPlan.Validate;
    Supports(LPlan, INyxPublicationReadiness, LFence);
    LFence.CheckReady;
    LPlan.Install;
    { Deliberately omit Notify and explicit Retire. Both interface locals and
      managed function-result temporaries end at this helper's return. }
  end;

begin
  LScheduler := TControlledScheduler.Create;
  LSchedulerLease := LScheduler;
  LEvents := NewNyxEvents(LSchedulerLease);
  try
    Supports(LEvents, INyxEventViewPreparation, LPreparation);
    LCallback := TTraceCallback.Create;
    LStream := LEvents.On(NyxControlEvents('button'), ntClick).Policy(neUIQueue);
    LStream.Subscribe(LCallback);
    LStream.Subscribe(LCallback);
    Click(LEvents);
    Check(Length(LScheduler.States) = 2,
      'installed-abandonment fixture owns two genuine old dispatch tickets');
    LScheduler.States[0].FailCancellation := True;
    InstallAndAbandon;
    Check((LScheduler.States[0].CancelCalls = 1) and
      (LScheduler.States[1].CancelCalls = 1) and LScheduler.Tickets[1].Cancelled,
      'destruction without Notify attempts every old cancellation and contains its failure');
    Check(LToken.Active and not LFuture.Cancelled and
      (NyxEventGeneration(LEvents) = LFuture),
      'cleanup fallback never revokes installed successor slots or its generation');
  finally

    if Length(LScheduler.States) > 0 then
    begin
      LScheduler.States[0].FailCancellation := False;
    end;
    LPreparation := nil;
    LStream := nil;
    LEvents.Close;
    LEvents := nil;
    LSchedulerLease := nil;
  end;
end;

begin
  try
    Run;
    ViewStagingCases;
    ViewAbortCases;
    ViewFenceCases;
    ViewPolicyAndExecutionCases;
    ViewLifetimeAndSnapshotCases;
    ViewReentrantRetirementCase;
    ViewInstalledRetirementCase;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-observation-publication', 'passed');
    document.body.setAttribute('data-observation-publication-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' atomic observation publication checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-observation-publication', 'failed');
      document.body.setAttribute('data-observation-publication-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
