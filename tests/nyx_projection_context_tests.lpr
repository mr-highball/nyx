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
program nyx_projection_context_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.types, nyx.data, nyx.behavior, nyx.events, nyx.scheduler,
  nyx.publication, nyx.model, nyx.state, nyx.view.sections, nyx.projection.context
  {$ifdef PAS2JS}, JS, Web{$endif};

type
  { Deterministic UI ticket owns no callback or router. Its state stays terminal
    after execution; cancellation is observable independently of callback entry.
    An injected Pascal failure is owned until raised; raw platform values are
    retained only for explicit browser notification-failure qualification. }
  TTicket = class(TInterfacedObject, INyxExecution)
  public
    State: TNyxExecutionStatus;
    CancelCalls: Integer;
    Failure: Exception;
    {$ifdef PAS2JS}
    RaiseRaw: Boolean;
    RawFailure: JSValue;
    {$endif}
    destructor Destroy; override;
    procedure Cancel;
    function GetCancelled: Boolean;
    function GetStatus: TNyxExecutionStatus;
    function GetFailure: TNyxText;
  end;

  { The queue retains real router-produced INyxWork until an explicit drain.
    This fixture exercises queued delivery without timers, threads or UI hosts;
    sequential work still executes synchronously inside ordinary DispatchCore. }
  TQueue = class(TInterfacedObject, INyxScheduler)
  private
    FWork: array of INyxWork;
    FTickets: TNyxExecutions;
    FStates: array of TTicket;
  public
    PreviewOnUI: INyxEventViewPreview;
    RetireOnUI: INyxPreparedPublication;
    UIPreviewed: Boolean;
    function GetCapabilities: TNyxSchedulerCapabilities;
    procedure RequireUI;
    procedure Admit(APolicy: TNyxExecutionPolicy);
    function Submit(const AWork: INyxWork;
      APolicy: TNyxExecutionPolicy): INyxExecution;
    function PostUI(const AWork: INyxWork;
      const AParent: INyxExecution = nil): INyxExecution;
    procedure Shutdown;
    procedure Drain;
  end;

  { Ordinary callback records actual entry and can attempt preview from inside
    the synchronous dispatch. Preview is explicitly released to avoid retaining
    its router through this callback's subscription during teardown. }
  TCallback = class(TNyxEventCallback)
  public
    Calls: Integer;
    Preview: INyxEventViewPreview;
    EnteredRefusal: Boolean;
    procedure Invoke(const AEvent: TNyxEventInfo;
      const AExecution: INyxExecution); override;
  end;

  { Reversible target stands in for an adapter's owned old model/input snapshot.
    Its rollback deliberately enters the real router before and after restoring
    data. Failure is owned until Notify raises it; the catch then owns it.
    No physical-control or whole-editor performance claim comes from this port. }
  TTarget = class(TInterfacedObject, INyxPreparedPublication,
    INyxPublicationReadiness)
  private
    FBefore: TNyxText;
    FValidated: Boolean;
    FInstalled: Boolean;
    FRetired: Boolean;
  public
    Events: INyxEvents;
    Value: TNyxText;
    Ready: Boolean;
    ProbeRollback: Boolean;
    RollbackBlockedBefore: Boolean;
    RollbackBlockedAfter: Boolean;
    ClickOnNotify: Boolean;
    ClearCallerOnValidate: Boolean;
    ClearCallerOnNotify: Boolean;
    RetireOnValidate: INyxPreparedPublication;
    RetireOnNotify: INyxPreparedPublication;
    FailRetire: Boolean;
    Failure: Exception;
    {$ifdef PAS2JS}
    RawFailure: JSValue;
    RaiseRaw: Boolean;
    {$endif}
    Validations: Integer;
    Installs: Integer;
    Notifications: Integer;
    Retirements: Integer;
    destructor Destroy; override;
    procedure Validate;
    procedure CheckReady;
    procedure Install;
    procedure Notify;
    procedure Retire;
  end;

  { Canonical section handle exercises the real coordinator's nonempty group.
    It models reversible placement only, not native/DOM controls. The optional
    borrowed target is retained independently by the case; a later Preview can
    invalidate its fence, requiring rollback of every previewed section. }
  TSection = class(TInterfacedObject, INyxViewSection,
    INyxViewSectionPublication, INyxViewSectionCandidateRetirement)
  public
    Name: TNyxText;
    Revision: Integer;
    Target: TTarget;
    Previewed: Boolean;
    Publications: Integer;
    Rollbacks: Integer;
    Retirements: Integer;
    function GetReference: TNyxViewSectionRef;
    function GetRevision: Integer;
    function GetRoot: TNyxNode;
    function Prepare(ADocument: TNyxDocument; ARoot: TNyxNode;
      ADesignMode: Boolean = False; AState: TNyxState = nil): INyxViewSectionChange;
    procedure Close;
    function GetSection: INyxViewSection;
    function GetExpectedRevision: Integer;
    function Check: Boolean;
    procedure Preview;
    procedure Rollback;
    procedure Publish;
    procedure RetirePrevious;
    procedure RetireCandidate;
  end;

var
  GChecks: Integer;
  GCallerEffect: INyxPreparedPublication;
  GRollbackOrder: TNyxText;
  {$ifdef PAS2JS}
  GStringifications: Integer;
  {$endif}

procedure Check(ACondition: Boolean; const AMessage: TNyxText);
begin
  Inc(GChecks);

  if not ACondition then
  begin
    raise Exception.Create(AMessage);
  end;
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

function DispatchRefuses(const AEvents: INyxEvents): Boolean;
begin
  Result := False;
  try
    Click(AEvents);
  except
    on E: ENyxSchedule do
    begin
      Result := True;
    end;
  end;
end;

destructor TTicket.Destroy;
begin
  Failure.Free;
  inherited Destroy;
end;

procedure TTicket.Cancel;
var
  LFailure: Exception;
begin
  Inc(CancelCalls);
  State := nesCancelled;

  if Failure <> nil then
  begin
    LFailure := Failure;
    Failure := nil;
    raise LFailure;
  end;
  {$ifdef PAS2JS}

  if RaiseRaw then
  begin
    asm
      throw this.RawFailure;
    end;
  end;
  {$endif}
end;

function TTicket.GetCancelled: Boolean;
begin
  Result := State = nesCancelled;
end;

function TTicket.GetStatus: TNyxExecutionStatus;
begin
  Result := State;
end;

function TTicket.GetFailure: TNyxText;
begin
  Result := '';
end;

function TQueue.GetCapabilities: TNyxSchedulerCapabilities;
begin
  Result := [nscDeferredUI];
end;

procedure TQueue.RequireUI;
var
  LPreview: INyxEventViewPreview;
  LRetire: INyxPreparedPublication;
begin
  LPreview := PreviewOnUI;
  PreviewOnUI := nil;
  LRetire := RetireOnUI;
  RetireOnUI := nil;

  if LRetire <> nil then
  begin
    LRetire.Retire;
  end;

  if LPreview <> nil then
  begin
    UIPreviewed := LPreview.TryBeginPreview;
  end;
end;

procedure TQueue.Admit(APolicy: TNyxExecutionPolicy);
begin

  if APolicy = neThreaded then
  begin
    raise ENyxSchedule.Create('The fixture queue has no worker capability');
  end;
end;

function TQueue.Submit(const AWork: INyxWork;
  APolicy: TNyxExecutionPolicy): INyxExecution;
var
  LState: TTicket;
  LCount: Integer;
begin
  Admit(APolicy);
  LState := TTicket.Create;
  Result := LState;
  LState.State := nesPending;
  LCount := Length(FTickets);
  SetLength(FTickets, LCount + 1);
  SetLength(FStates, LCount + 1);
  SetLength(FWork, LCount + 1);
  FTickets[LCount] := Result;
  FStates[LCount] := LState;

  if APolicy = neSequential then
  begin
    LState.State := nesRunning;
    AWork.Execute(Result);

    if not Result.Cancelled then
    begin
      LState.State := nesSucceeded;
    end;
  end
  else
  begin
    FWork[LCount] := AWork;
  end;
end;

function TQueue.PostUI(const AWork: INyxWork;
  const AParent: INyxExecution): INyxExecution;
begin
  Result := Submit(AWork, neUIQueue);
end;

procedure TQueue.Shutdown;
var
  LIndex: Integer;
begin
  for LIndex := 0 to High(FTickets) do
  begin
    FTickets[LIndex].Cancel;
  end;
  FWork := nil;
  PreviewOnUI := nil;
  RetireOnUI := nil;
end;

procedure TQueue.Drain;
var
  LIndex: Integer;
  LWork: INyxWork;
begin
  for LIndex := 0 to High(FWork) do
  begin
    LWork := FWork[LIndex];
    FWork[LIndex] := nil;

    if (LWork <> nil) and not FTickets[LIndex].Cancelled then
    begin
      FStates[LIndex].State := nesRunning;
      LWork.Execute(FTickets[LIndex]);

      if not FTickets[LIndex].Cancelled then
      begin
        FStates[LIndex].State := nesSucceeded;
      end;
    end;
  end;
end;

procedure TCallback.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
begin
  Inc(Calls);

  if Preview <> nil then
  begin
    EnteredRefusal := not Preview.TryBeginPreview;
  end;
end;

destructor TTarget.Destroy;
begin
  Failure.Free;
  inherited Destroy;
end;

procedure TTarget.Validate;
var
  LRetire: INyxPreparedPublication;
begin
  Inc(Validations);
  FBefore := Value;
  FValidated := True;
  LRetire := RetireOnValidate;
  RetireOnValidate := nil;

  if LRetire <> nil then
  begin
    LRetire.Retire;
  end;

  if ClearCallerOnValidate then
  begin
    GCallerEffect := nil;
  end;
  CheckReady;
end;

procedure TTarget.CheckReady;
begin

  if not FValidated or not Ready or FRetired or FInstalled then
  begin
    raise ENyxPublication.Create('Deliberate target readiness refusal');
  end;
end;

procedure TTarget.Install;
begin
  FInstalled := True;
  Inc(Installs);
end;

procedure TTarget.Notify;
var
  LFailure: Exception;
  LRetire: INyxPreparedPublication;
begin
  Inc(Notifications);
  LRetire := RetireOnNotify;
  RetireOnNotify := nil;

  if LRetire <> nil then
  begin
    LRetire.Retire;
  end;

  if ClearCallerOnNotify then
  begin
    GCallerEffect := nil;
  end;

  if ClickOnNotify then
  begin
    Click(Events);
  end;

  if Failure <> nil then
  begin
    LFailure := Failure;
    Failure := nil;
    raise LFailure;
  end;
  {$ifdef PAS2JS}

  if RaiseRaw then
  begin
    asm
      throw this.RawFailure;
    end;
  end;
  {$endif}
end;

procedure TTarget.Retire;
begin

  if FRetired then
  begin
    Exit;
  end;
  FRetired := True;
  Inc(Retirements);
  RetireOnValidate := nil;
  RetireOnNotify := nil;

  if FValidated and not FInstalled then
  begin

    if ProbeRollback then
    begin
      RollbackBlockedBefore := DispatchRefuses(Events);
    end;
    Value := FBefore;

    if ProbeRollback then
    begin
      RollbackBlockedAfter := DispatchRefuses(Events);
    end;
  end;

  if FailRetire then
  begin
    raise ENyxPublication.Create('Deliberate secondary target retirement failure');
  end;
end;

function TSection.GetReference: TNyxViewSectionRef;
begin
  Result := TNyxViewSectionRef.Named(Name);
end;

function TSection.GetRevision: Integer;
begin
  Result := Revision;
end;

function TSection.GetRoot: TNyxNode;
begin
  Result := nil;
end;

function TSection.Prepare(ADocument: TNyxDocument; ARoot: TNyxNode;
  ADesignMode: Boolean; AState: TNyxState): INyxViewSectionChange;
begin
  Result := nil;
  raise ENyxModel.Create('This fixture supplies only its explicit publication port');
end;

procedure TSection.Close;
begin
end;

function TSection.GetSection: INyxViewSection;
begin
  Result := Self;
end;

function TSection.GetExpectedRevision: Integer;
begin
  Result := Revision;
end;

function TSection.Check: Boolean;
begin
  Result := True;
end;

procedure TSection.Preview;
begin
  Previewed := True;

  if Target <> nil then
  begin
    Target.Ready := False;
  end;
end;

procedure TSection.Rollback;
begin
  Inc(Rollbacks);
  Previewed := False;
  GRollbackOrder := GRollbackOrder + Name;
end;

procedure TSection.Publish;
begin
  Inc(Publications);
  Inc(Revision);
end;

procedure TSection.RetirePrevious;
begin
  Inc(Retirements);
end;

procedure TSection.RetireCandidate;
begin
  Inc(Retirements);
end;

procedure EventPreviewCases;
var
  LQueue: TQueue;
  LScheduler: INyxScheduler;
  LEvents: INyxEvents;
  LPreparation: INyxEventViewPreparation;
  LPlan: INyxPreparedEventView;
  LPreview: INyxEventViewPreview;
  LBefore: INyxCancellationScope;
  LCallback: TCallback;
  LCallbackLease: INyxEventCallback;
  LToken: INyxEventSubscription;
  LStream: INyxEventStream;
  LRevision: Integer;
begin
  LQueue := TQueue.Create;
  LScheduler := LQueue;
  LEvents := NewNyxEvents(LScheduler);
  try
    LCallback := TCallback.Create;
    LCallbackLease := LCallback;
    LStream := LEvents.On(NyxControlEvents('button'), ntClick).Policy(neUIQueue);
    LToken := LStream.Subscribe(LCallbackLease);
    LBefore := NyxEventGeneration(LEvents);
    LRevision := LEvents.ViewRevision;
    Click(LEvents);
    Check(LCallback.Calls = 0, 'a genuine router callback is queued before preview');
    Check(Supports(LEvents, INyxEventViewPreparation, LPreparation),
      'canonical router exposes successor preparation');
    LPlan := LPreparation.PrepareView;
    LPlan.Validate;
    Check(Supports(LPlan, INyxEventViewPreview, LPreview),
      'prepared successor exposes explicit preview admission');
    Check(not LPreview.TryBeginPreview and not LBefore.Cancelled and
      not LQueue.FTickets[0].Cancelled, 'pending work refuses without cancellation or consumption');
    LQueue.Drain;
    Check((LCallback.Calls = 1) and (LQueue.FTickets[0].Status = nesSucceeded),
      'the untouched old queued callback still executes on its original context');
    Check(LPreview.TryBeginPreview, 'terminal old work admits the same original preview');
    Check(LPreview.TryBeginPreview, 'repeated preview admission preserves the same reservation');
    Check(DispatchRefuses(LEvents) and (LCallback.Calls = 1),
      'new dispatch cannot run while retained preview is active');
    Check((LEvents.ViewRevision = LRevision) and not LBefore.Cancelled,
      'reversible preview has not published or revoked the mounted generation');
    LPlan.Retire;
    LPreview := nil;
    LPlan := nil;
    Click(LEvents);
    LQueue.Drain;
    Check((LCallback.Calls = 2) and not LBefore.Cancelled,
      'abandonment resumes ordinary dispatch without rotating the old generation');
    LStream.Policy(neSequential);
    LPlan := LPreparation.PrepareView;
    LPlan.Validate;
    Supports(LPlan, INyxEventViewPreview, LPreview);
    LCallback.Preview := LPreview;
    Click(LEvents);
    LCallback.Preview := nil;
    Check(LCallback.EnteredRefusal and (LCallback.Calls = 3),
      'an entered synchronous dispatch cannot preview before its own ticket is tracked');
    LPlan.Retire;
    LPreview := nil;
    LPlan := nil;
    LPlan := LPreparation.PrepareView;
    LPlan.Validate;
    Supports(LPlan, INyxEventViewPreview, LPreview);
    Check(LPreview.TryBeginPreview, 'completed synchronous work admits a later preview');
    LPlan.Install;
    LPlan.Notify;
    LPlan.Retire;
    Check(LBefore.Cancelled and (LEvents.ViewRevision = LRevision + 1),
      'successful publication rotates exactly one generation');
    Click(LEvents);
    Check((LCallback.Calls = 4) and LToken.Active,
      'installed retirement permits successor work and keeps ordinary registrations');
    LPreview := nil;
    LPlan := nil;
    LPlan := LPreparation.PrepareView;
    LPlan.Validate;
    Supports(LPlan, INyxEventViewPreview, LPreview);
    LQueue.PreviewOnUI := LPreview;
    Check(DispatchRefuses(LEvents) and LQueue.UIPreviewed and (LCallback.Calls = 4),
      'preview begun during UI admission refuses dispatch before any event capture');
    LPlan.Retire;
    Click(LEvents);
    Check(LCallback.Calls = 5, 'the admission-refused dispatch did not poison future entry');
  finally
    LCallback.Preview := nil;
    LPreview := nil;
    LPlan := nil;
    LPreparation := nil;
    LStream := nil;
    LEvents.Close;
    LEvents := nil;
    LQueue.Shutdown;
    LScheduler := nil;
  end;
end;

procedure CoupledCases;
var
  LEvents: INyxEvents;
  LBefore: INyxCancellationScope;
  LTarget: TTarget;
  LTargetLease: INyxPreparedPublication;
  LContext: INyxPreparedPublication;
  LCallback: TCallback;
  LCallbackLease: INyxEventCallback;
  LCommitted: Boolean;
  LFailed: Boolean;
begin
  LEvents := NewNyxEvents;
  try
    LCallback := TCallback.Create;
    LCallbackLease := LCallback;
    LEvents.On(NyxControlEvents('button'), ntClick).Subscribe(LCallbackLease);
    LBefore := NyxEventGeneration(LEvents);
    LTarget := TTarget.Create;
    LTargetLease := LTarget;
    LTarget.Events := LEvents;
    LTarget.Value := 'old';
    LTarget.Ready := True;
    LTarget.ProbeRollback := True;
    LContext := NewNyxProjectionContextPublication(LEvents, LTargetLease);
    LContext.Validate;
    LTarget.Value := 'preview';
    LContext.Retire;
    Check((LTarget.Value = 'old') and LTarget.RollbackBlockedBefore and
      LTarget.RollbackBlockedAfter, 'old dispatch stays fenced through the entire target rollback');
    Check(not LBefore.Cancelled and (LTarget.Installs = 0) and
      (LTarget.Retirements = 1), 'abandonment restores target without publishing event authority');
    Click(LEvents);
    Check(LCallback.Calls = 1, 'completed rollback releases the dispatch fence');
    LContext := nil;
    LTargetLease := nil;
    LTarget := TTarget.Create;
    LTargetLease := LTarget;
    LTarget.Events := LEvents;
    LTarget.Value := 'old';
    LTarget.Ready := True;
    LTarget.ClickOnNotify := True;
    LContext := NewNyxProjectionContextPublication(LEvents, LTargetLease);
    LContext.Validate;
    LTarget.Value := 'successor';
    Check(PublishNyxViewSections([], LContext, LCommitted) and LCommitted,
      'section coordinator publishes a previously validated retained context');
    Check((LTarget.Value = 'successor') and LBefore.Cancelled and
      (LTarget.Validations = 1) and (LTarget.Installs = 1) and
      (LTarget.Notifications = 1) and (LTarget.Retirements = 1),
      'repeated outer validation does not recapture or repeat any target phase');
    Check(LCallback.Calls = 2, 'notification dispatch sees the already installed successor');
    LContext := nil;
    LTargetLease := nil;
    LBefore := NyxEventGeneration(LEvents);
    LTarget := TTarget.Create;
    LTargetLease := LTarget;
    LTarget.Events := LEvents;
    LTarget.Value := 'accepted';
    LTarget.Ready := True;
    LTarget.ProbeRollback := True;
    LContext := NewNyxProjectionContextPublication(LEvents, LTargetLease);
    LContext.Validate;
    LTarget.Value := 'refused preview';
    LTarget.Ready := False;
    LFailed := False;
    try
      PublishNyxViewSections([], LContext, LCommitted);
    except
      on E: ENyxPublication do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and not LCommitted and not LBefore.Cancelled and
      (LTarget.Value = 'accepted') and (LTarget.Installs = 0),
      'outer final refusal rolls back preview and preserves the old event generation');
    Check(LTarget.RollbackBlockedBefore and LTarget.RollbackBlockedAfter,
      'failed outer publication also fences callbacks throughout target restoration');
    Click(LEvents);
    Check(LCallback.Calls = 3, 'refusal releases only its own temporary dispatch fence');
  finally
    LContext := nil;
    LTargetLease := nil;
    LEvents.Close;
    LEvents := nil;
  end;
end;

procedure PostPreviewFenceCase;
var
  LEvents: INyxEvents;
  LBefore: INyxCancellationScope;
  LTarget: TTarget;
  LTargetLease: INyxPreparedPublication;
  LContext: INyxPreparedPublication;
  LFirst: TSection;
  LSecond: TSection;
  LFirstLease: INyxViewSectionPublication;
  LSecondLease: INyxViewSectionPublication;
  LFirstChange: INyxViewSectionChange;
  LSecondChange: INyxViewSectionChange;
  LCommitted: Boolean;
  LFailed: Boolean;
begin
  LEvents := NewNyxEvents;
  try
    LBefore := NyxEventGeneration(LEvents);
    LTarget := TTarget.Create;
    LTargetLease := LTarget;
    LTarget.Events := LEvents;
    LTarget.Value := 'accepted';
    LTarget.Ready := True;
    LTarget.ProbeRollback := True;
    LContext := NewNyxProjectionContextPublication(LEvents, LTargetLease);
    LContext.Validate;
    LTarget.Value := 'retained preview';
    LFirst := TSection.Create;
    LFirstLease := LFirst;
    LFirst.Name := 'first';
    LSecond := TSection.Create;
    LSecondLease := LSecond;
    LSecond.Name := 'second';
    LSecond.Target := LTarget;
    LFirstChange := NewNyxViewSectionChange(LFirstLease);
    LSecondChange := NewNyxViewSectionChange(LSecondLease);
    GRollbackOrder := '';
    LFailed := False;
    try
      PublishNyxViewSections([LFirstChange, LSecondChange], LContext, LCommitted);
    except
      on E: ENyxPublication do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and not LCommitted and not LBefore.Cancelled,
      'later physical preview invalidates the original final fence before any install');
    Check((LFirst.Publications = 0) and (LSecond.Publications = 0) and
      (LTarget.Installs = 0), 'no section or retained target publishes after that refusal');
    Check((LFirst.Rollbacks = 1) and (LSecond.Rollbacks = 1) and
      not LFirst.Previewed and not LSecond.Previewed and
      (GRollbackOrder = 'secondfirst'), 'all previewed sections roll back in reverse order');
    Check((LFirstChange.State = vcsCanceled) and (LSecondChange.State = vcsCanceled) and
      (LFirst.Retirements = 1) and (LSecond.Retirements = 1),
      'both unpublished candidates retire exactly once after final-fence failure');
    Check((LTarget.Value = 'accepted') and LTarget.RollbackBlockedBefore and
      LTarget.RollbackBlockedAfter and (LTarget.Retirements = 1),
      'retained rollback remains fenced after every section has restored placement');
    Check(not DispatchRefuses(LEvents), 'complete group rollback releases the original router');
  finally
    LFirstChange := nil;
    LSecondChange := nil;
    LFirstLease := nil;
    LSecondLease := nil;
    LContext := nil;
    LTargetLease := nil;
    LEvents.Close;
    LEvents := nil;
  end;
end;

procedure ReentrantContextCases;
var
  LStage: Integer;
  LQueue: TQueue;
  LScheduler: INyxScheduler;
  LEvents: INyxEvents;
  LBefore: INyxCancellationScope;
  LTarget: TTarget;
  LTargetLease: INyxPreparedPublication;
  LContext: INyxPreparedPublication;
  LCommitted: Boolean;
  LFailed: Boolean;
begin
  for LStage := 0 to 2 do
  begin
    LQueue := TQueue.Create;
    LScheduler := LQueue;
    LEvents := NewNyxEvents(LScheduler);
    try
      LBefore := NyxEventGeneration(LEvents);
      LTarget := TTarget.Create;
      LTargetLease := LTarget;
      LTarget.Events := LEvents;
      LTarget.Value := 'accepted';
      LTarget.Ready := True;
      LContext := NewNyxProjectionContextPublication(LEvents, LTargetLease);

      if LStage = 0 then
      begin
        LTarget.RetireOnValidate := LContext;
      end
      else if LStage = 1 then
      begin
        LQueue.RetireOnUI := LContext;
      end;

      if LStage < 2 then
      begin
        LFailed := False;
        try
          LContext.Validate;
        except
          on E: ENyxPublication do
          begin
            LFailed := True;
          end;
        end;
        Check(LFailed and not LBefore.Cancelled and (LTarget.Installs = 0) and
          (LTarget.Retirements = 1),
          'reentrant retirement defers child detachment until validation returns');
        Check(not DispatchRefuses(LEvents),
          'validation retirement releases its original reservation without disposing an entered group');
      end
      else
      begin
        LContext.Validate;
        LTarget.Value := 'successor';
        LTarget.RetireOnNotify := LContext;
        Check(PublishNyxViewSections([], LContext, LCommitted) and LCommitted and
          LBefore.Cancelled and (LTarget.Value = 'successor') and
          (LTarget.Notifications = 1) and (LTarget.Retirements = 1),
          'notification retirement waits for its entered group and preserves committed data');
        Check(not DispatchRefuses(LEvents),
          'notification retirement keeps the installed successor generation usable');
      end;
    finally
      LContext := nil;
      LTargetLease := nil;
      LEvents.Close;
      LEvents := nil;
      LQueue.Shutdown;
      LScheduler := nil;
    end;
  end;
end;

procedure PublishFailureMode(AMode: Integer; const AFirst,
  ASecond: INyxPreparedPublication);
var
  LGroup: INyxPreparedPublication;
begin
  case AMode of
    0:
      begin
        PublishNyxGroup([AFirst, ASecond]);
      end;
    1:
      begin
        LGroup := PrepareNyxGroup([AFirst, ASecond]);
        PublishNyxGroup([LGroup]);
      end;
    2:
      begin
        { The first receiver withdraws this ONLY caller group reference during
          Notify. The group's entered lease must finish the second receiver
          and its own retirement before returning the original failure. }
        GCallerEffect := PrepareNyxGroup([AFirst, ASecond]);
        GCallerEffect.Validate;
        GCallerEffect.Install;
        GCallerEffect.Notify;
      end;
  end;
end;

{$ifdef PAS2JS}
function RefuseStringification: JSValue;
begin
  Result := Undefined;
  Inc(GStringifications);
  raise Exception.Create('Raw notification failures must not be stringified');
end;

function RawFailure(AKind: Integer): JSValue;
begin
  Result := Undefined;
  case AKind of
    1:
      begin
        Result := Null;
      end;
    2:
      begin
        Result := False;
      end;
    3:
      begin
        Result := 0;
      end;
    4:
      begin
        Result := String(TNyxText('raw / 🌙') + #0 + ' / exact');
      end;
    5:
      begin
        Result := TJSObject.new;
        TJSObject(Result)['toString'] := @RefuseStringification;
      end;
  end;
end;

function SameRaw(AFirst, ASecond: JSValue): Boolean;
begin
  asm
    return AFirst === ASecond;
  end;
end;
{$endif}

procedure GroupNotificationFailureCases;
var
  LMode: Integer;
  LKind: Integer;
  LFirst: TTarget;
  LSecond: TTarget;
  LFirstLease: INyxPreparedPublication;
  LSecondLease: INyxPreparedPublication;
  LMessage: TNyxText;
  LExact: Boolean;
  {$ifdef PAS2JS}
  LRaw: JSValue;
  {$endif}
begin
  for LMode := 0 to 2 do
  begin
    for LKind := 0 to 1 do
    begin
      LFirst := TTarget.Create;
      LFirstLease := LFirst;
      LSecond := TTarget.Create;
      LSecondLease := LSecond;
      LFirst.Ready := True;
      LSecond.Ready := True;
      LFirst.ClearCallerOnNotify := LMode = 2;
      LMessage := '';

      if LKind = 1 then
      begin
        LMessage := TNyxText('first / 🌙') + #0 + ' / exact';
      end;
      LFirst.Failure := ENyxPublicationNotification.CreateReceiverFailure(LMessage);
      LSecond.Failure := Exception.Create('Later failure must not replace the first');
      LExact := False;
      try
        PublishFailureMode(LMode, LFirstLease, LSecondLease);
      except
        on E: ENyxPublicationNotification do
        begin
          LExact := E.HasReceiverMessage and (E.ReceiverMessage = LMessage);
        end;
      end;
      Check(LExact and (LFirst.Notifications = 1) and (LSecond.Notifications = 1),
        'both direct and nested notifications preserve the exact first Pascal diagnostic');
      Check((LFirst.Installs = 1) and (LSecond.Installs = 1) and
        (LFirst.Retirements = 1) and (LSecond.Retirements = 1),
        'notification failure retains committed participants and retires each original owner');
      GCallerEffect := nil;
    end;
    {$ifdef PAS2JS}
    for LKind := 0 to 5 do
    begin
      LFirst := TTarget.Create;
      LFirstLease := LFirst;
      LSecond := TTarget.Create;
      LSecondLease := LSecond;
      LFirst.Ready := True;
      LSecond.Ready := True;
      LFirst.ClearCallerOnNotify := LMode = 2;
      LFirst.RaiseRaw := True;
      LRaw := RawFailure(LKind);
      LFirst.RawFailure := LRaw;
      LSecond.Failure := Exception.Create('Later Pascal failure cannot replace raw browser data');
      LExact := False;
      try
        PublishFailureMode(LMode, LFirstLease, LSecondLease);
      except
        LExact := SameRaw(JSExceptValue, LRaw);
      end;
      Check(LExact and (LFirst.Notifications = 1) and (LSecond.Notifications = 1),
        'an earlier raw browser failure cannot skip the later receiver or change first identity');
      Check((LFirst.Retirements = 1) and (LSecond.Retirements = 1) and
        (GStringifications = 0), 'raw failures retire every owner without external stringification');
      GCallerEffect := nil;
    end;
    {$endif}
  end;
end;

procedure EarlierTicketFailureCases;
var
  LKind: Integer;
  LQueue: TQueue;
  LScheduler: INyxScheduler;
  LEvents: INyxEvents;
  LBefore: INyxCancellationScope;
  LCallback: TCallback;
  LCallbackLease: INyxEventCallback;
  LTarget: TTarget;
  LTargetLease: INyxPreparedPublication;
  LContext: INyxPreparedPublication;
  LCommitted: Boolean;
  LExact: Boolean;
  {$ifdef PAS2JS}
  LRaw: JSValue;
  {$endif}
begin
  for LKind := 0 to {$ifdef PAS2JS}6{$else}0{$endif} do
  begin
    LQueue := TQueue.Create;
    LScheduler := LQueue;
    LEvents := NewNyxEvents(LScheduler);
    try
      LCallback := TCallback.Create;
      LCallbackLease := LCallback;
      LEvents.On(NyxControlEvents('button'), ntClick).Policy(neUIQueue).Subscribe(LCallbackLease);
      LEvents.On(NyxControlEvents('button'), ntClick).Subscribe(LCallbackLease);
      Click(LEvents);
      LQueue.Drain;
      Check((LCallback.Calls = 2) and (Length(LQueue.FTickets) = 2),
        'two genuine old event invocations finish before the retained preview');
      LBefore := NyxEventGeneration(LEvents);
      LTarget := TTarget.Create;
      LTargetLease := LTarget;
      LTarget.Events := LEvents;
      LTarget.Value := 'accepted';
      LTarget.Ready := True;
      LContext := NewNyxProjectionContextPublication(LEvents, LTargetLease);
      LContext.Validate;
      LTarget.Value := 'successor';

      if LKind = 0 then
      begin
        LQueue.FStates[0].Failure := ENyxSchedule.Create('First ticket cancellation failed');
      end;
      {$ifdef PAS2JS}

      if LKind > 0 then
      begin
        LRaw := RawFailure(LKind - 1);
        LQueue.FStates[0].RaiseRaw := True;
        LQueue.FStates[0].RawFailure := LRaw;
      end;
      {$endif}
      LQueue.FStates[1].Failure := ENyxSchedule.Create('Second cancellation failed');
      LExact := False;
      try
        PublishNyxViewSections([], LContext, LCommitted);
      except
        {$ifdef PAS2JS}

        if LKind > 0 then
        begin
          LExact := SameRaw(JSExceptValue, LRaw);
        end
        else if isClassInstance(JSExceptValue) and
          (TObject(JSExceptValue) is ENyxPublicationNotification) then
        begin
          LExact := Pos('First ticket cancellation failed',
            ENyxPublicationNotification(JSExceptValue).ReceiverMessage) > 0;
        end;
        {$else}
        on E: ENyxPublicationNotification do
        begin
          LExact := Pos('First ticket cancellation failed', E.ReceiverMessage) > 0;
        end;
        {$endif}
      end;
      Check(LExact and LCommitted and LBefore.Cancelled and (LTarget.Value = 'successor'),
        'the first original cancellation failure reports already committed successor data');
      Check((LQueue.FStates[0].CancelCalls = 1) and (LQueue.FStates[1].CancelCalls = 1) and
        (LTarget.Notifications = 1) and (LTarget.Retirements = 1),
        'every old ticket cancellation and the later target notification is attempted exactly once');
      LContext := nil;
      {$ifdef PAS2JS}
      LQueue.FStates[0].RaiseRaw := False;
      {$endif}
    finally
      LContext := nil;
      LTargetLease := nil;
      LEvents.Close;
      LEvents := nil;
      LQueue.Shutdown;
      LScheduler := nil;
    end;
  end;
end;

procedure EffectOwnershipAndFailureCases;
var
  LTarget: TTarget;
  LLease: INyxPreparedPublication;
  LCommitted: Boolean;
  LOriginal: Exception;
  LExact: Boolean;
  {$ifdef PAS2JS}
  LKind: Integer;
  LRaw: JSValue;
  LThrown: JSValue;
  {$endif}
begin
  LTarget := TTarget.Create;
  LLease := LTarget;
  LTarget.Ready := True;
  LTarget.ClearCallerOnValidate := True;
  GCallerEffect := LLease;
  Check(PublishNyxViewSections([], GCallerEffect, LCommitted) and LCommitted and
    (GCallerEffect = nil) and (LTarget.Validations = 1) and
    (LTarget.Installs = 1) and (LTarget.Notifications = 1) and
    (LTarget.Retirements = 1), 'caller withdrawal during validation preserves the exact effect phases');
  LTarget := TTarget.Create;
  LLease := LTarget;
  LTarget.Ready := True;
  LTarget.ClearCallerOnNotify := True;
  LTarget.FailRetire := True;
  LOriginal := Exception.Create('');
  LTarget.Failure := LOriginal;
  GCallerEffect := LLease;
  LExact := False;
  try
    PublishNyxViewSections([], GCallerEffect, LCommitted);
  except
    on E: Exception do
    begin
      LExact := (E = LOriginal) and (E.Message = '');
    end;
  end;
  LOriginal := nil;
  Check(LExact and LCommitted and (GCallerEffect = nil) and
    (LTarget.Installs = 1) and (LTarget.Notifications = 1) and
    (LTarget.Retirements = 1), 'empty-message primary exception survives caller withdrawal and failing cleanup');
  {$ifdef PAS2JS}
  for LKind := 0 to 1 do
  begin
    LTarget := TTarget.Create;
    LLease := LTarget;
    LTarget.Ready := True;
    LTarget.FailRetire := True;
    LTarget.RaiseRaw := True;
    LRaw := Undefined;

    if LKind = 1 then
    begin
      LRaw := TJSObject.new;
    end;
    LTarget.RawFailure := LRaw;
    LExact := False;
    try
      PublishNyxViewSections([], LLease, LCommitted);
    except
      LThrown := JSExceptValue;

      if LKind = 0 then
      begin
        LExact := isUndefined(LThrown);
      end
      else
      begin
        LExact := TJSObject(LThrown) = TJSObject(LRaw);
      end;
    end;
    Check(LExact and LCommitted and (LTarget.Retirements = 1),
      'raw JavaScript primary identity survives failing effect retirement');
  end;
  {$endif}
  GCallerEffect := nil;
  LLease := nil;
end;

begin
  try
    EventPreviewCases;
    CoupledCases;
    PostPreviewFenceCase;
    ReentrantContextCases;
    GroupNotificationFailureCases;
    EarlierTicketFailureCases;
    EffectOwnershipAndFailureCases;
    {$ifdef PAS2JS}
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' retained-context publication checks';
    document.body.setAttribute('data-projection-context', 'passed');
    document.body.setAttribute('data-projection-context-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' retained-context publication checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.textContent := 'FAIL ' + E.Message;
      document.body.setAttribute('data-projection-context', 'failed');
      document.body.setAttribute('data-projection-context-checks', IntToStr(GChecks));
      document.body.setAttribute('data-projection-context-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
