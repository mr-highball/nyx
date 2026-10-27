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

const
  CToolbarSearchPart = 'search';
  CToolbarSortPart = 'sort';
  CToolbarClearPart = 'clear';
  CToolbarSearchChannel = 'search';

type
  { A final no-op group participant observes the real installation boundary.
    It owns no model or callback. Its retained lease survives notification and
    retirement errors, including raw browser throws, without guessing commit
    from exception classes/messages or implementing another transaction engine. }
  TToolbarCommandPublication = class(TInterfacedObject, INyxPreparedPublication)
  public
    Installed: Boolean;
    procedure Validate;
    procedure Install;
    procedure Notify;
    procedure Retire;
  end;
  { Cleanup preserves the first failure, including arbitrary browser values
    and empty Pascal diagnostics, while attempting every independent duty. }
  TToolbarFailure = record
    Captured: Boolean;
    Message: TNyxText;
    {$ifdef PAS2JS}
    Raw: Boolean;
    Value: JSValue;
    {$endif}
  end;
  { Forward declaration for the router's revocable borrowed receiver. }
  TToolbarBinding = class;
  { Router slots retain this observer; the view subscription borrows its method.
    The binding keeps the inert observer alive until both subscription families
    have retired. Revocation clears the borrowed controller before any callout. }
  TToolbarObserver = class(TNyxEventObserver)
  public
    Owner: TToolbarBinding;
    procedure Observe(const AEvent: TNyxEventInfo; ACallbackCount: Integer); override;
    procedure Changed(const AView: INyxCollectionView;
      const AChanges: INyxCollectionChanges);
  end;
  { One common prepared coordinator owns the exact actor, event slots and weak
    input port. No widget, renderer, raw node or application is retained.
    Preparation and runtime share identity; installed Retire drops only the
    prospective event-plan lease. Explicit Disconnect attempts every cleanup.
    The last command receipt contains copied diagnostics, never execution rights. }
  TToolbarBinding = class(TInterfacedObject, INyxDataToolbarBinding,
    INyxCoordinatorBinding, INyxDataToolbarQueryBinding,
    INyxPreparedCoordinator, INyxPublicationReadiness, INyxDataToolbarCommandFeedback)
  private
    FOptions: TNyxDataToolbarOptions;
    FDisabled: Boolean;
    FProjection: INyxCoordinatorProjection;
    FLifetime: INyxCoordinatorProjectionLifetime;
    FInputs: INyxInputCommands;
    FSearch: TNyxControlRef;
    FSort: TNyxControlRef;
    FClear: TNyxControlRef;
    FQueryStatus: TNyxControlRef;
    FQueryDetails: TNyxControlRef;
    FTarget: TNyxControlRef;
    FRefresh: INyxCoordinatorRefresh;
    FQueryControls: INyxCollectionQueryWorkbench;
    FLastCommand: TNyxToolbarCommandFeedback;
    FViewToken: INyxCollectionViewSubscription;
    FViews: INyxCollectionBindings;
    FView: INyxCollectionView;
    FSession: INyxCollectionQuerySession;
    FActor: INyxCollectionQueryOwner;
    FEventView: INyxPreparedEventView;
    FGeneration: INyxCancellationScope;
    FScheduler: INyxScheduler;
    FObserver: TToolbarObserver;
    FObserverLease: INyxEventObserver;
    FTokens: array of INyxEventSubscription;
    FValidated: Boolean;
    FInstalled: Boolean;
    FNotified: Boolean;
    FRetired: Boolean;
    FRevoked: Boolean;
    FCleaned: Boolean;
    procedure CheckFlags(AInstalled: Boolean);
    procedure CheckActive;
    procedure Observe(const AEvent: TNyxEventInfo);
    function SearchEdits(const AText: TNyxText): TNyxQuerySessionEdits;
    function SortEdits(const AReference: TNyxToolbarSortRef): TNyxQuerySessionEdits;
    procedure PublishQuery(const AEdits: TNyxQuerySessionEdits);
    procedure UpdateQueryStatus;
    procedure RunCommand(ACommand: TNyxToolbarCommand; const AText: TNyxText;
      const ASort: TNyxToolbarSortRef; AAcceptedEvent: Boolean; out AInstalled: Boolean);
    procedure RecordCommandFailure(ACommand: TNyxToolbarCommand; AInstalled: Boolean;
      const AMessage: TNyxText);
  public
    constructor Create(const AProjection: INyxCoordinatorProjection;
      const AIntent: TNyxDataToolbarIntent; const AViews: INyxCollectionBindings;
      const AEventView: INyxPreparedEventView; const AScheduler: INyxScheduler;
      const ARefresh: INyxCoordinatorRefresh);
    destructor Destroy; override;
    function GetConnected: Boolean;
    function GetBinding: INyxCoordinatorBinding;
    procedure Search(const AText: TNyxText);
    procedure Sort(const AReference: TNyxToolbarSortRef);
    procedure Clear;
    function GetLastCommand: TNyxToolbarCommandFeedback;
    function GetObservation: TNyxQueryWorkbenchObservation;
    function GetQueryStatus: TNyxText;
    function GetQueryControls: INyxCollectionQueryWorkbench;
    procedure OpenQueryDetails;
    procedure Revoke;
    procedure Disconnect;
    procedure Validate;
    procedure CheckReady;
    procedure Install;
    procedure Notify;
    procedure Retire;
  end;
  { Applications register this trusted code; persisted metadata never loads it. }
  TToolbarFactory = class(TInterfacedObject, INyxCoordinatorFactory)
  public
    function Prepare(const AProjection: INyxCoordinatorProjection;
      const AIntent: TNyxDataValue; const AViews: INyxCollectionBindings;
      const AView: INyxPreparedEventView; const AScheduler: INyxScheduler;
      const ARefresh: INyxCoordinatorRefresh): INyxPreparedCoordinator;
  end;

procedure TToolbarCommandPublication.Validate;
begin
end;

procedure TToolbarCommandPublication.Install;
begin
  Installed := True;
end;

procedure TToolbarCommandPublication.Notify;
begin
end;

procedure TToolbarCommandPublication.Retire;
begin
end;

procedure CaptureToolbarFailure(var AFailure: TToolbarFailure;
  {$ifdef PAS2JS}AValue: JSValue{$else}const AMessage: TNyxText{$endif});
begin

  if AFailure.Captured then
  begin
    Exit;
  end;
  AFailure.Captured := True;
  {$ifdef PAS2JS}
  AFailure.Value := AValue;
  AFailure.Raw := not (isClassInstance(AValue) and (TObject(AValue) is Exception));

  if not AFailure.Raw then
  begin
    AFailure.Message := Exception(AValue).Message;
  end;
  {$else}
  AFailure.Message := AMessage;
  {$endif}
end;

procedure RaiseToolbarFailure(const AFailure: TToolbarFailure);
{$ifdef PAS2JS}
var
  LValue: JSValue;
{$endif}
begin

  if not AFailure.Captured then
  begin
    Exit;
  end;
  {$ifdef PAS2JS}

  if AFailure.Raw then
  begin
    LValue := AFailure.Value;
    asm
      throw LValue;
    end;
  end;
  {$endif}
  raise ENyxDataToolbar.Create(AFailure.Message);
end;

constructor TToolbarBinding.Create(const AProjection: INyxCoordinatorProjection;
  const AIntent: TNyxDataToolbarIntent; const AViews: INyxCollectionBindings;
  const AEventView: INyxPreparedEventView; const AScheduler: INyxScheduler;
  const ARefresh: INyxCoordinatorRefresh);
var
  LFacts: INyxCoordinatorRoleFacts;
  LBehavior: INyxCoordinatorBehaviorFacts;
  LInputs: INyxCoordinatorInputs;
  LTargets: INyxCoordinatorTargets;
  LRole: TNyxCoordinatorRoleInfo;
  LDomain: TNyxValueDomain;
  LDomainData: TNyxDataValue;
  LChoices: TNyxDataValue;
  LSpec: TNyxCollectionViewSpec;
  LIndex: Integer;
  LHasChoices: Boolean;

  procedure RequireRole(const AControl: TNyxControlRef; AKind: TNyxKind);
  var
    LInfo: TNyxCoordinatorRoleInfo;
  begin
    LInfo := LFacts.Describe(AControl);

    if (LInfo.Kind.Name <> NyxKindName(AKind)) or
      (LInfo.ProjectionKind.Name <> NyxKindName(AKind)) or LInfo.Compound or
      (LBehavior.Action(AControl) <> naNone) then
    begin
      raise ENyxDataToolbar.Create('Toolbar requires an exact scalar/action role');
    end;
  end;

begin
  inherited Create;
  FProjection := AProjection;
  FViews := AViews;
  FEventView := AEventView;
  FScheduler := AScheduler;
  FRefresh := ARefresh;

  if (FProjection = nil) or (FEventView = nil) or (FScheduler = nil) or
    not Supports(FProjection, INyxCoordinatorProjectionLifetime, FLifetime) then
  begin
    raise ENyxDataToolbar.Create('Toolbar requires the common prepared coordinator scope');
  end;
  FGeneration := FEventView.Generation;
  FDisabled := AIntent.Mode = ntiDisabled;
  CheckFlags(False);

  if FDisabled then
  begin
    Exit;
  end;
  FOptions := AIntent.Options;

  if not Supports(FProjection, INyxCoordinatorRoleFacts, LFacts) or
    not Supports(FProjection, INyxCoordinatorBehaviorFacts, LBehavior) or
    not Supports(FProjection, INyxCoordinatorInputs, LInputs) or
    not Supports(FProjection, INyxCoordinatorTargets, LTargets) or
    (FViews = nil) then
  begin
    raise ENyxDataToolbar.Create('Toolbar host lacks copied role/input/target capabilities');
  end;
  LRole := LFacts.Describe(FProjection.Root);

  if (LRole.Kind.Name <> NyxKindName(nkDataToolbar)) or not LRole.Compound then
  begin
    raise ENyxDataToolbar.Create('Toolbar intent requires its specialized compound');
  end;
  FSearch := FProjection.Part(NyxPart(CToolbarSearchPart));
  FSort := FProjection.Part(NyxPart(CToolbarSortPart));
  FClear := FProjection.Part(NyxPart(CToolbarClearPart));
  RequireRole(FSearch, nkInput);
  RequireRole(FSort, nkSelect);
  RequireRole(FClear, nkButton);

  if FOptions.QueryPresentation.Defined and
    (FOptions.QueryPresentation.DisplayMode <> ntqdHidden) then
  begin

    if FRefresh = nil then
    begin
      raise ENyxDataToolbar.Create('Query status requires the managed refresh port');
    end;
    FQueryStatus := FProjection.Part(NyxPart(NyxDataToolbarQueryStatusPart));
    RequireRole(FQueryStatus, nkLabel);
    { Reserve presentation before Build. Conflicting caller text bindings refuse
      the candidate; runtime updates never take ownership of such a binding. }
    FProjection.Text(FQueryStatus, FProjection.AuthoredText(FQueryStatus));

    if FOptions.QueryPresentation.DisplayMode = ntqdDetails then
    begin
      FQueryDetails := FProjection.Part(NyxPart(NyxDataToolbarQueryDetailsPart));
      RequireRole(FQueryDetails, nkButton);
    end;
  end;
  LDomain := LInputs.ValueDomain(FSearch);

  if not LDomain.Defined or (LDomain.Kind <> nskText) then
  begin
    raise ENyxDataToolbar.Create('Toolbar Search requires a text value domain');
  end;
  LDomain.Admit(NyxData(''));
  LDomain := LInputs.ValueDomain(FSort);

  if not LDomain.Defined or (LDomain.Kind <> nskText) then
  begin
    raise ENyxDataToolbar.Create('Toolbar Sort requires a text choice domain');
  end;
  LDomain.Admit(NyxData(FOptions.BaselineChoice.Name));
  for LIndex := 0 to FOptions.Count - 1 do
  begin
    LDomain.Admit(NyxData(FOptions.ChoiceAt(LIndex).Reference.Name));
  end;
  { A bound initial value is projected only after coordinator preparation.
    Require a closed declared choice domain here, so that ordinary binding
    admission refuses an unsupported value before any candidate is adopted. }
  LHasChoices := False;
  LDomainData := LDomain.ToData;
  for LIndex := 0 to LDomainData.Count - 1 do
  begin
    LHasChoices := LHasChoices or (LDomainData.Key(LIndex) = 'choices');
  end;

  if not LHasChoices then
  begin
    raise ENyxDataToolbar.Create('Toolbar Sort requires its closed declared choices');
  end;
  LChoices := LDomainData.Field('choices');

  if LChoices.Count <> FOptions.Count + 1 then
  begin
    raise ENyxDataToolbar.Create('Toolbar Sort domain differs from configured choices');
  end;
  LInputs.ReserveInput(FSearch);
  LInputs.ReserveInput(FSort);
  FInputs := LInputs.InputCommands;
  FTarget := LTargets.CollectionTarget(FOptions.Target);

  if not FProjection.TryCollectionSpec(FTarget, LSpec) then
  begin
    raise ENyxDataToolbar.Create('Toolbar target requires a collection binding');
  end;
  FView := FViews.ViewFor(FTarget.ID);

  if (FView = nil) or (FView.Spec.ToData.ToJSON <> LSpec.ToData.ToJSON) then
  begin
    raise ENyxDataToolbar.Create('Toolbar target view does not match its scoped binding');
  end;
  FOptions.ValidateSchema(FView.Store.Snapshot.Schema);
  FSession := NyxCollectionQuerySession(FView);
  FActor := FSession.AcquireOwner(NyxQueryOrigin(FProjection.Root.ID), 'Data toolbar');
  FObserver := TToolbarObserver.Create;
  FObserverLease := FObserver;
  FObserver.Owner := Self;
  SetLength(FTokens, 3);
  FTokens[0] := FEventView.PrepareObservation(NyxControlEvents(FSearch.ID, niRuntime),
    ntChange, FObserverLease, nopBeforeCallbacks, NyxEvent(''));
  FTokens[1] := FEventView.PrepareObservation(NyxControlEvents(FSort.ID, niRuntime),
    ntChange, FObserverLease, nopBeforeCallbacks, NyxEvent(''));
  FTokens[2] := FEventView.PrepareObservation(NyxControlEvents(FClear.ID, niRuntime),
    ntClick, FObserverLease, nopBeforeCallbacks, NyxEvent(''));

  if FQueryDetails.ID <> '' then
  begin
    SetLength(FTokens, 4);
    FTokens[3] := FEventView.PrepareObservation(NyxControlEvents(FQueryDetails.ID, niRuntime),
      ntClick, FObserverLease, nopBeforeCallbacks, NyxEvent(''));
  end;
end;

destructor TToolbarBinding.Destroy;
begin
  try
    Disconnect;
  except
    { Destruction revokes defensively. Explicit Disconnect reports failures. }
  end;
  inherited Destroy;
end;

procedure TToolbarBinding.CheckFlags(AInstalled: Boolean);
begin

  if FRevoked or FCleaned or (FGeneration = nil) or FGeneration.Cancelled or
    (AInstalled and not FInstalled) then
  begin
    raise ENyxDataToolbarCancelled.Create('Toolbar is dormant or retired');
  end;
end;

procedure TToolbarBinding.CheckActive;
begin
  CheckFlags(True);
  FScheduler.RequireUI;
  CheckFlags(True);

  if FDisabled or (FActor = nil) or not FActor.Connected or
    (FInputs = nil) or not FInputs.Connected or
    (FViews.ViewFor(FTarget.ID) <> FView) then
  begin
    raise ENyxDataToolbarCancelled.Create('Toolbar input or query generation was replaced');
  end;
  CheckFlags(True);
end;

function TToolbarBinding.GetConnected: Boolean;
begin
  Result := False;
  try
    CheckActive;
    Result := True;
  except
    on ENyxDataToolbarCancelled do
    begin
      Result := False;
    end;
  end;
end;

function TToolbarBinding.GetBinding: INyxCoordinatorBinding;
begin
  { Disabled still supplies the common mount's lifetime owner. Its Connected
    value is False and every specialized command refuses without side effects. }
  Result := Self;
end;

function TToolbarBinding.SearchEdits(const AText: TNyxText): TNyxQuerySessionEdits;
var
  LPredicate: INyxCollectionPredicate;
begin
  Result := NyxQuerySessionEdits;

  if AText = '' then
  begin
    Exit(Result.ClearFilter(NyxQueryChannel(CToolbarSearchChannel)));
  end;
  case FOptions.MatchMode of
    ntsmContains:
      begin
        LPredicate := NyxWhere(FOptions.SearchField).Contains(AText, FOptions.Comparison);
      end;
    ntsmStartsWith:
      begin
        LPredicate := NyxWhere(FOptions.SearchField).StartsWith(AText, FOptions.Comparison);
      end;
    ntsmEqual:
      begin
        LPredicate := NyxWhere(FOptions.SearchField).EqualTo(AText, FOptions.Comparison);
      end;
  end;
  Result := Result.Filter(NyxQueryChannel(CToolbarSearchChannel), LPredicate);
end;

function TToolbarBinding.SortEdits(const AReference: TNyxToolbarSortRef):
  TNyxQuerySessionEdits;
var
  LIndex: Integer;
begin
  LIndex := FOptions.SortIndex(AReference);
  Result := NyxQuerySessionEdits;

  if LIndex < 0 then
  begin
    Exit(Result.ResetSort);
  end;
  Result := Result.SortBy(FOptions.ChoiceAt(LIndex).Query);
end;

procedure TToolbarBinding.PublishQuery(const AEdits: TNyxQuerySessionEdits);
var
  LStage: INyxPreparedPublication;
begin
  CheckActive;
  LStage := FSession.PrepareSession(FSession.Capture, FActor, AEdits);
  CheckActive;
  PublishNyxGroup([LStage]);
end;

procedure TToolbarBinding.Search(const AText: TNyxText);
var
  LInstalled: Boolean;
begin
  RunCommand(ntcSearch, AText, FOptions.BaselineChoice, False, LInstalled);
end;

procedure TToolbarBinding.Sort(const AReference: TNyxToolbarSortRef);
var
  LInstalled: Boolean;
begin
  RunCommand(ntcSort, '', AReference, False, LInstalled);
end;

procedure TToolbarBinding.Clear;
var
  LInstalled: Boolean;
begin
  RunCommand(ntcClear, '', FOptions.BaselineChoice, False, LInstalled);
end;

function TToolbarBinding.GetLastCommand: TNyxToolbarCommandFeedback;
begin
  { Data-only observation also works after the scheduler/scope has retired.
    As with the binding itself, callers capture this value on the UI thread. }
  Result := FLastCommand;
end;

procedure TToolbarBinding.RecordCommandFailure(ACommand: TNyxToolbarCommand;
  AInstalled: Boolean; const AMessage: TNyxText);
begin
  FLastCommand.FCommand := ACommand;
  FLastCommand.FMessage := AMessage;

  if AInstalled then
  begin
    FLastCommand.FOutcome := ntcoCommittedWithError;
  end
  else
  begin
    FLastCommand.FOutcome := ntcoRefused;
  end;
  { Diagnostic presentation must not replace the original exception, including
    a raw browser value. The copied receipt remains available if refresh fails
    or retires this scope. Successful-command refresh errors are NOT swallowed:
    RunCommand catches them after its real installation marker instead. }
  try
    UpdateQueryStatus;
  except
  end;
end;

procedure TToolbarBinding.RunCommand(ACommand: TNyxToolbarCommand;
  const AText: TNyxText; const ASort: TNyxToolbarSortRef;
  AAcceptedEvent: Boolean; out AInstalled: Boolean);
var
  LLease: INyxDataToolbarBinding;
  LCapture: INyxInputCapture;
  LInputStage: INyxPreparedPublication;
  LQueryStage: INyxPreparedPublication;
  LProof: TToolbarCommandPublication;
  LProofLease: INyxPreparedPublication;
  LEdits: TNyxQuerySessionEdits;
  LFailure: TToolbarFailure;
begin
  LLease := Self;
  AInstalled := False;
  { Check revocation before dereferencing services released by Disconnect.
    Retained commands keep their typed cancellation and cannot replace the
    retired owner's last receipt or reach a successor. }
  CheckActive;
  FLastCommand := Default(TNyxToolbarCommandFeedback);
  FLastCommand.FCommand := ACommand;
  FLastCommand.FOutcome := ntcoRunning;
  LFailure := Default(TToolbarFailure);
  LProof := TToolbarCommandPublication.Create;
  LProofLease := LProof;
  try
    CheckActive;
    case ACommand of
      ntcSearch:
        begin
          LEdits := SearchEdits(AText);

          if not AAcceptedEvent then
          begin
            LCapture := FInputs.Capture([FSearch]);
            LInputStage := FInputs.Prepare(LCapture, [NyxInputValue(FSearch, AText)]);
          end;
        end;
      ntcSort:
        begin
          LEdits := SortEdits(ASort);

          if not AAcceptedEvent then
          begin
            LCapture := FInputs.Capture([FSort]);
            LInputStage := FInputs.Prepare(LCapture, [NyxInputValue(FSort, ASort.Name)]);
          end;
        end;
      ntcClear:
        begin
          LEdits := NyxQuerySessionEdits.ClearOwner;
          LCapture := FInputs.Capture([FSearch, FSort]);
          LInputStage := FInputs.Prepare(LCapture, [NyxInputValue(FSearch, TNyxText('')),
            NyxInputValue(FSort, FOptions.BaselineChoice.Name)]);
        end;
    else
      raise ENyxDataToolbar.Create('Toolbar command requires a concrete action');
    end;
    LQueryStage := FSession.PrepareSession(FSession.Capture, FActor, LEdits);
    CheckActive;

    if LInputStage = nil then
    begin
      { Accepted user input already owns its scalar publication. Never synthesize
        a second change callback or turn that input into a programmatic reset. }
      PublishNyxGroup([LQueryStage, LProofLease]);
    end
    else
    begin
      PublishNyxGroup([LInputStage, LQueryStage, LProofLease]);
    end;
    AInstalled := LProof.Installed;
    FLastCommand.FOutcome := ntcoSucceeded;
    UpdateQueryStatus;
  except
    {$ifdef PAS2JS}
    CaptureToolbarFailure(LFailure, JSExceptValue);
    AInstalled := LProof.Installed;

    if LFailure.Raw then
    begin
      RecordCommandFailure(ACommand, AInstalled, 'The command failed in platform code.');
    end
    else
    begin
      { Match native exact diagnostic selection. A Pascal wrapper's explanatory
        Message is not the original retained receiver text on either target. }

      if TObject(LFailure.Value) is ENyxInputAssignmentRefused then
      begin
        LFailure.Message := ENyxInputAssignmentRefused(TObject(LFailure.Value)).Diagnostic;
      end
      else if (TObject(LFailure.Value) is ENyxPublicationNotification) and
        ENyxPublicationNotification(TObject(LFailure.Value)).HasReceiverMessage then
      begin
        LFailure.Message := ENyxPublicationNotification(TObject(LFailure.Value)).ReceiverMessage;
      end;
      RecordCommandFailure(ACommand, AInstalled, LFailure.Message);
    end;
    raise;
    {$else}
    on LError: Exception do
    begin

      if LError is ENyxInputAssignmentRefused then
      begin
        LFailure.Message := ENyxInputAssignmentRefused(LError).Diagnostic;
      end
      else if (LError is ENyxPublicationNotification) and
        ENyxPublicationNotification(LError).HasReceiverMessage then
      begin
        LFailure.Message := ENyxPublicationNotification(LError).ReceiverMessage;
      end
      else
      begin
        CaptureToolbarFailure(LFailure, LError.Message);
      end;
      AInstalled := LProof.Installed;
      RecordCommandFailure(ACommand, AInstalled, LFailure.Message);
      raise;
    end;
    {$endif}
  end;
end;

procedure TToolbarObserver.Observe(const AEvent: TNyxEventInfo; ACallbackCount: Integer);
var
  LOwner: TToolbarBinding;
  LLease: INyxDataToolbarBinding;
begin
  LOwner := Owner;

  if LOwner = nil then
  begin
    Exit;
  end;
  LLease := LOwner;
  LOwner.Observe(AEvent);
end;

procedure TToolbarObserver.Changed(const AView: INyxCollectionView;
  const AChanges: INyxCollectionChanges);
var
  LOwner: TToolbarBinding;
  LLease: INyxDataToolbarBinding;
begin
  LOwner := Owner;

  if LOwner = nil then
  begin
    Exit;
  end;
  LLease := LOwner;
  LOwner.UpdateQueryStatus;
end;

function TToolbarBinding.GetObservation: TNyxQueryWorkbenchObservation;
var
  LLease: INyxDataToolbarBinding;
begin
  LLease := Self;
  CheckActive;
  Result := TNyxQueryWorkbenchObservation.Read(FView);
  CheckActive;
end;

function TToolbarBinding.GetQueryStatus: TNyxText;
var
  LLease: INyxDataToolbarBinding;
  LObservation: TNyxQueryWorkbenchObservation;
  LAction: TNyxText;
  LMessage: TNyxText;
  LPosition: Integer;
  LCount: Integer;
  LScalar: Integer;
begin
  LLease := Self;
  LObservation := GetObservation;
  Result := NyxDataToolbarQuerySummary(LObservation, FActor.ID, FOptions.QueryPresentation);

  if FLastCommand.Outcome in [ntcoRefused, ntcoCommittedWithError] then
  begin
    case FLastCommand.Command of
      ntcSearch: LAction := 'Search';
      ntcSort: LAction := 'Sort';
      ntcClear: LAction := 'Clear';
    else
      LAction := 'Command';
    end;
    { Bound only the visible preview at complete scalar boundaries. Keep the
      original receipt exact; arbitrary creator diagnostics need not fit a label. }
    LPosition := 1;
    LCount := 0;
    while (LPosition <= Length(FLastCommand.Message)) and (LCount < 240) do
    begin

      if not NyxNextScalar(FLastCommand.Message, LPosition, LScalar) then
      begin
        Break;
      end;
      Inc(LCount);
    end;
    LMessage := Copy(FLastCommand.Message, 1, LPosition - 1);

    if LPosition <= Length(FLastCommand.Message) then
    begin
      LMessage := LMessage + NyxScalarText($2026);
    end;

    if FLastCommand.Outcome = ntcoRefused then
    begin
      Result := Result + ' ' + LAction + ' was not applied. ' + LMessage;
    end
    else
    begin
      Result := Result + ' ' + LAction + ' applied; a follow-up failed. ' + LMessage;
    end;
  end;
end;

function TToolbarBinding.GetQueryControls: INyxCollectionQueryWorkbench;
begin
  CheckActive;
  Result := FQueryControls;
end;

procedure TToolbarBinding.OpenQueryDetails;
var
  LLease: INyxDataToolbarBinding;
  LHost: INyxCoordinatorQueryControls;
  LDetails: INyxCollectionQueryDetails;
begin
  LLease := Self;
  CheckActive;

  if FQueryControls = nil then
  begin

    if not Supports(FProjection, INyxCoordinatorQueryControls, LHost) then
    begin
      raise ENyxDataToolbar.Create('This coordinator host does not provide query details');
    end;
    FQueryControls := LHost.QueryDetails(FTarget);
    CheckActive;
  end;

  if not Supports(FQueryControls, INyxCollectionQueryDetails, LDetails) then
  begin
    raise ENyxDataToolbar.Create('The managed query surface lacks its inspection purpose');
  end;
  { The common scope owns cancellation and cleanup. This copied identity merely
    labels local contributions; no actor token is granted to the presenter. }
  LDetails.OpenDetailsFor(FActor.ID);
end;

procedure TToolbarBinding.UpdateQueryStatus;
var
  LLease: INyxDataToolbarBinding;
  LText: TNyxText;
begin
  LLease := Self;

  if not GetConnected or (FQueryStatus.ID = '') then
  begin
    Exit;
  end;
  LText := GetQueryStatus;
  FProjection.Text(FQueryStatus, LText);
  CheckActive;
  { Refresh is the final outward call and may dispose the whole view. }
  FRefresh.Request;
end;

procedure TToolbarBinding.Observe(const AEvent: TNyxEventInfo);
var
  LInstalled: Boolean;
begin
  { An already snapshotted router slot is inert after query/view retirement.
    Direct public commands still report a typed cancellation to their caller. }

  if not GetConnected then
  begin
    Exit;
  end;

  if (AEvent.Trigger = ntClick) and (AEvent.OriginID = FClear.ID) then
  begin
    try
      RunCommand(ntcClear, '', FOptions.BaselineChoice, False, LInstalled);
    except
      on LError: ENyxInputAssignmentRefused do
      begin
        { Only an unchanged, expected input refusal with opted-in query status
          becomes ordinary UI feedback. Direct methods, hidden/static recipes,
          programmer errors and any post-install failure retain their exception. }

        if LInstalled or (FQueryStatus.ID = '') then
        begin
          raise;
        end;
      end;
    end;
    Exit;
  end;

  if (AEvent.Trigger = ntClick) and (FQueryDetails.ID <> '') and
    (AEvent.OriginID = FQueryDetails.ID) then
  begin
    OpenQueryDetails;
    Exit;
  end;

  if (AEvent.Trigger <> ntChange) or not AEvent.HasValue then
  begin
    Exit;
  end;
  { Before-callback observation consumes this accepted event's copied scalar,
    never a later selection or a value read after queued application handlers. }

  if AEvent.OriginID = FSearch.ID then
  begin
    RunCommand(ntcSearch, AEvent.Value.AsText, FOptions.BaselineChoice, True, LInstalled);
  end
  else
  begin

    if AEvent.OriginID = FSort.ID then
    begin
      RunCommand(ntcSort, '', NyxToolbarSort(AEvent.Value.AsText), True, LInstalled);
    end;
  end;
end;

procedure TToolbarBinding.Revoke;
begin
  FRevoked := True;

  if FLifetime <> nil then
  begin
    FLifetime.Revoke;
  end;

  if FObserver <> nil then
  begin
    FObserver.Owner := nil;
  end;

  if FActor <> nil then
  begin
    FActor.Revoke;
  end;
end;

procedure TToolbarBinding.Disconnect;
var
  LTokens: array of INyxEventSubscription;
  LLifetime: INyxCoordinatorProjectionLifetime;
  LIndex: Integer;
  LFailure: TToolbarFailure;
  LViewToken: INyxCollectionViewSubscription;
begin

  if FCleaned then
  begin
    Exit;
  end;
  Revoke;
  FCleaned := True;
  LTokens := FTokens;
  FTokens := nil;
  LLifetime := FLifetime;
  FLifetime := nil;
  FObserver := nil;
  LViewToken := FViewToken;
  FViewToken := nil;
  FQueryControls := nil;
  FRefresh := nil;
  FInputs := nil;
  FActor := nil;
  FSession := nil;
  FView := nil;
  FViews := nil;
  FEventView := nil;
  FProjection := nil;
  FGeneration := nil;
  FScheduler := nil;
  LFailure := Default(TToolbarFailure);
  try

    if LViewToken <> nil then
    begin
      LViewToken.Disconnect;
    end;
  except
    {$ifdef PAS2JS}
    CaptureToolbarFailure(LFailure, JSExceptValue);
    {$else}
    on LException: Exception do
    begin
      CaptureToolbarFailure(LFailure, TNyxText(LException.Message));
    end;
    {$endif}
  end;
  for LIndex := 0 to High(LTokens) do
  begin
    try

      if LTokens[LIndex] <> nil then
      begin
        LTokens[LIndex].Cancel;
      end;
    except
      {$ifdef PAS2JS}
      CaptureToolbarFailure(LFailure, JSExceptValue);
      {$else}
      on LException: Exception do
      begin
        CaptureToolbarFailure(LFailure, TNyxText(LException.Message));
      end;
      {$endif}
    end;
  end;
  try

    if LLifetime <> nil then
    begin
      LLifetime.Disconnect;
    end;
  except
    {$ifdef PAS2JS}
    CaptureToolbarFailure(LFailure, JSExceptValue);
    {$else}
    on LException: Exception do
    begin
      CaptureToolbarFailure(LFailure, TNyxText(LException.Message));
    end;
    {$endif}
  end;
  { View callbacks borrow the observer, so release it only after all independent
    cancellation attempts. Reentrant Disconnect already sees FCleaned=True. }
  FObserverLease := nil;
  RaiseToolbarFailure(LFailure);
end;

procedure TToolbarBinding.Validate;
begin
  CheckFlags(False);

  if FValidated or FInstalled or FRetired then
  begin
    raise ENyxDataToolbar.Create('Toolbar preparation is single use');
  end;
  FScheduler.RequireUI;
  CheckFlags(False);

  if not FDisabled then
  begin

    if (FViews.ViewFor(FTarget.ID) <> FView) or not FActor.Connected or
      not NyxCollectionViewPublicationReady(FView) then
    begin
      raise ENyxDataToolbar.Create('Toolbar target is replaced or busy before adoption');
    end;
    FOptions.ValidateSchema(FView.Store.Snapshot.Schema);
  end;
  FValidated := True;
  CheckReady;
end;

procedure TToolbarBinding.CheckReady;
begin
  CheckFlags(False);

  if not FValidated or FInstalled or FRetired or
    (not FDisabled and not FActor.Connected) then
  begin
    raise ENyxDataToolbarCancelled.Create('Toolbar preparation is no longer ready');
  end;
end;

procedure TToolbarBinding.Install;
begin

  if FValidated and not FInstalled and not FRevoked and not FRetired then
  begin
    FInstalled := True;
  end;
end;

procedure TToolbarBinding.Notify;
var
  LLease: INyxDataToolbarBinding;
  LCapture: INyxInputCapture;
  LValue: TNyxText;
  LEdits: TNyxQuerySessionEdits;
  LSortIndex: Integer;
begin
  LLease := Self;

  if FNotified or FDisabled then
  begin
    Exit;
  end;
  FNotified := True;
  CheckActive;

  if FQueryStatus.ID <> '' then
  begin
    FViewToken := FView.Subscribe(FObserver.Changed);
    CheckActive;
  end;
  LCapture := FInputs.Capture([FSearch, FSort]);
  LEdits := NyxQuerySessionEdits;

  if LCapture.HasValueAt(0) then
  begin
    LValue := LCapture.ValueAt(0).TextValue;

    if LValue <> '' then
    begin
      LEdits := SearchEdits(LValue);
    end;
  end;

  if LCapture.HasValueAt(1) then
  begin
    LValue := LCapture.ValueAt(1).TextValue;

    if LValue <> FOptions.BaselineChoice.Name then
    begin
      LSortIndex := FOptions.SortIndex(NyxToolbarSort(LValue));
      LEdits := LEdits.SortBy(FOptions.ChoiceAt(LSortIndex).Query);
    end;
  end;

  if LEdits.Count > 0 then
  begin
    PublishQuery(LEdits);
  end;
  UpdateQueryStatus;
end;

procedure TToolbarBinding.Retire;
begin

  if FRetired then
  begin
    Exit;
  end;
  FRetired := True;
  FEventView := nil;

  if not FInstalled then
  begin
    Disconnect;
  end;
end;

function TToolbarFactory.Prepare(const AProjection: INyxCoordinatorProjection;
  const AIntent: TNyxDataValue; const AViews: INyxCollectionBindings;
  const AView: INyxPreparedEventView; const AScheduler: INyxScheduler;
  const ARefresh: INyxCoordinatorRefresh): INyxPreparedCoordinator;
begin
  Result := TToolbarBinding.Create(AProjection, TNyxDataToolbarIntent.FromData(AIntent),
    AViews, AView, AScheduler, ARefresh);
end;

function NewNyxDataToolbarCoordinatorFactory: INyxCoordinatorFactory;
begin
  Result := TToolbarFactory.Create;
end;
