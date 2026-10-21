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
program nyx_kanban_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.types, nyx.data, nyx.model, nyx.controls, nyx.kanban,
  nyx.kanban.intent,
  nyx.collections, nyx.collections.view, nyx.collections.view.types,
  nyx.collections.query, nyx.collections.bindings, nyx.collections.selection,
  nyx.composition, nyx.events, nyx.scheduler, nyx.behavior, nyx.gestures,
  nyx.codec, nyx.source, nyx.publication, nyx.coordinators
  {$ifdef PAS2JS}, Web{$endif};

type
  { Real ordinary authored/reusable boards and their renderer-free realized
    collection bindings. No fake view or test-only mutation path is supplied. }
  TBoardFixture = class
    Document: TNyxDocument;
    Runtime: TNyxNode;
    Options: TNyxKanbanOptions;
    Bindings: INyxCollectionBindings;
    Scheduler: INyxScheduler;
    Events: INyxEvents;
    FirstBoard: INyxKanbanBoard;
    SecondBoard: INyxKanbanBoard;
    First: INyxKanbanBinding;
    Second: INyxKanbanBinding;
    EventPlan: INyxPreparedEventView;
    Mounts: TNyxCoordinatorMounts;
    IDs: TNyxCoordinatorMountIDs;
    Registry: INyxCoordinatorRegistry;
    RefreshTarget: INyxCoordinatorRefreshTarget;
    constructor Create(const AScheduler: INyxScheduler = nil);
    destructor Destroy; override;
    function View(ASecond: Boolean; const ALane: TNyxKanbanLaneRef): INyxCollectionView;
    function Store: INyxCollection;
    procedure UseCommon;
    procedure PrepareCommon;
    procedure AcceptCommon;
  end;

  { Managed final-host-style receiver counts guarded refresh without retaining
    a renderer, document, fixture or coordinator binding back into its scope. }
  TCommonRefresh = class(TInterfacedObject, INyxCoordinatorRefreshTarget)
  public
    Calls: Integer;
    procedure Request;
  end;

  { Validators can revoke the current intent while all published state is still
    the baseline. The accepted notification path is deliberately separate. }
  TInterventionKind = (ikNone, ikCancelMove, ikDisconnect, ikGeneration,
    ikReleaseCaller);

  { Synchronous borrowed test receiver. Tokens are disconnected before freeing
    it. Stored references are explicit test leases, never renderer pointers. }
  TIntervention = class
    Fixture: TBoardFixture;
    Move: INyxKanbanMove;
    Kind: TInterventionKind;
    FailObserver: Boolean;
    ValidateCalls: Integer;
    ObserverCalls: Integer;
    AtomicViews: Boolean;
    procedure Validate(const ACandidate: INyxCollectionSnapshot;
      const AChanges: INyxCollectionChanges);
    procedure Observe(const AStore: INyxCollection;
      const AChanges: INyxCollectionChanges);
  end;

  { Private controlled UI turn. It is an execution lease, not event/gesture
    authority; the real router wraps it using the original captured policy. }
  IQueuedExecution = interface(INyxExecution)
    procedure Run(const AWork: INyxWork);
  end;

  { Deterministic queued state for the one policy-transition regression. }
  TQueuedExecution = class(TInterfacedObject, INyxExecution, IQueuedExecution)
  private
    FCancelled: Boolean;
    FStatus: TNyxExecutionStatus;
    FFailure: TNyxText;
    FParent: INyxExecution;
  public
    constructor Create(const AParent: INyxExecution);
    procedure Cancel;
    function GetCancelled: Boolean;
    function GetStatus: TNyxExecutionStatus;
    function GetFailure: TNyxText;
    procedure Run(const AWork: INyxWork);
  end;

  { Only queue timing is controlled. Real sequential scheduling/UI checks,
    events, schemas, stores, query projections and board parts remain ordinary. }
  TTurnScheduler = class(TInterfacedObject, INyxScheduler)
  private
    FInline: INyxScheduler;
    FWorks: array of INyxWork;
    FExecutions: array of IQueuedExecution;
    FClosed: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
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

  { Application Click receiver runs under its own queued policy, after the
    synchronous default. It borrows only this test's still-owned fixture. }
  TQueuedClick = class(TNyxEventCallback)
    Fixture: TBoardFixture;
    Calls: Integer;
    SawMoved: Boolean;
    procedure Invoke(const AEvent: TNyxEventInfo;
      const AExecution: INyxExecution); override;
  end;

  { Borrowed final presentation receiver counts refreshes and can release the
    last renderer-style plan lease during a real committed view notification. }
  TPreparedRefreshProbe = class
  public
    Calls: Integer;
    ReleasePlan: Boolean;
    procedure Refresh;
  end;

  { A later ordinary participant proves the pure final fence notices an actual
    selection publication after the binding's outward admission has returned. }
  TPreparedSelection = class(TInterfacedObject, INyxPreparedPublication)
  public
    View: INyxCollectionView;
    procedure Validate;
    procedure Install;
    procedure Notify;
    procedure Retire;
  end;

var
  GChecks: Integer;
  GReleasedMove: INyxKanbanMove;
  GPreparedPlan: INyxPreparedKanbanBinding;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Kanban: ' + AReason);
  end;
  Inc(GChecks);
end;

function ReadyLane: TNyxKanbanLaneRef;
begin
  Result := NyxKanbanLane('queued');
end;

function ActiveLane: TNyxKanbanLaneRef;
begin
  Result := NyxKanbanLane('in-progress');
end;

function DoneLane: TNyxKanbanLaneRef;
begin
  Result := NyxKanbanLane('completed');
end;

function Card: TNyxItemRef;
begin
  Result := NyxItem(NyxCollection('tasks'), 'card-one');
end;

constructor TBoardFixture.Create(const AScheduler: INyxScheduler);
var
  LDefinition: INyxKanbanBoard;
  LPage: INyxPage;
  LInstance: INyxComponent;
  LSchema: TNyxCollectionSchema;
  LControl: INyxControl;
begin
  inherited Create;
  Document := TNyxDocument.Create;
  Options := NyxKanbanOptions(NyxCollection('tasks'), NyxTextField('title'),
    NyxTextField('lane')).Lane(ReadyLane, 'Ready').Lane(ActiveLane, 'In progress').
    Lane(DoneLane, 'Done');
  LSchema := NyxCollectionSchema.Text(NyxTextField('title'), '').
    Text(NyxTextField('lane'), ReadyLane.Name).Integer(NyxIntegerField('score'), 0);
  Document.Collections.Define(NyxCollection('tasks'), LSchema, [
    NyxCollectionItem(Card).WithValue(NyxTextField('title'), 'Review a proposal').
      WithValue(NyxTextField('lane'), ReadyLane.Name).WithValue(NyxIntegerField('score'), 7),
    NyxCollectionItem(NyxItem(NyxCollection('tasks'), 'card-two')).
      WithValue(NyxTextField('title'), 'Ship a view').
      WithValue(NyxTextField('lane'), ReadyLane.Name),
    NyxCollectionItem(NyxItem(NyxCollection('tasks'), 'card-three')).
      WithValue(NyxTextField('title'), 'Describe the result').
      WithValue(NyxTextField('lane'), ActiveLane.Name)]);
  LDefinition := NewNyxKanbanBoard('work-board');
  ConfigureNyxKanbanBoard(LDefinition, Options);
  Document.AddComponent(LDefinition.Node);
  LPage := NewNyxPage('home');
  LInstance := NewNyxComponent('first-board');
  LInstance.Configure.Component(NyxComponent('work-board')).Done;
  LPage.Add(LInstance);
  LInstance := NewNyxComponent('second-board');
  LInstance.Configure.Component(NyxComponent('work-board')).Done;
  LPage.Add(LInstance);
  Document.AddPage(LPage.Node);
  Runtime := RealizeNyxView(Document, LPage.Node);
  Bindings := NewNyxCollectionBindings(Runtime,
    NewNyxCollectionContext(Document.Collections));
  Scheduler := AScheduler;

  if Scheduler = nil then
  begin
    Scheduler := NewNyxScheduler;
  end;
  Events := NewNyxEvents(Scheduler);
  LControl := RetainNyxControl(Runtime.Find(NyxQualifiedID('first-board', 'work-board')));
  FirstBoard := LControl as INyxKanbanBoard;
  LControl := RetainNyxControl(Runtime.Find(NyxQualifiedID('second-board', 'work-board')));
  SecondBoard := LControl as INyxKanbanBoard;
  First := NewNyxKanbanBinding(FirstBoard, Options, Bindings, Events, Scheduler, nil);
  Second := NewNyxKanbanBinding(SecondBoard, Options, Bindings, Events, Scheduler, nil);
end;

destructor TBoardFixture.Destroy;
begin
  DisconnectNyxCoordinatorMounts(Mounts);

  if First <> nil then
  begin
    First.Disconnect;
  end;

  if Second <> nil then
  begin
    Second.Disconnect;
  end;

  if Events <> nil then
  begin
    Events.Close;
  end;
  EventPlan := nil;
  Registry := nil;
  RefreshTarget := nil;
  First := nil;
  Second := nil;
  FirstBoard := nil;
  SecondBoard := nil;
  Bindings := nil;
  Events := nil;

  if Scheduler <> nil then
  begin
    Scheduler.Shutdown;
  end;
  Scheduler := nil;
  Runtime.Free;
  Document.Free;
  inherited Destroy;
end;

function TBoardFixture.View(ASecond: Boolean;
  const ALane: TNyxKanbanLaneRef): INyxCollectionView;
var
  LItems: INyxList;
begin

  if ASecond then
  begin
    LItems := Second.Items(ALane);
  end
  else
  begin
    LItems := First.Items(ALane);
  end;
  Result := Bindings.ViewFor(LItems.ID);
end;

function TBoardFixture.Store: INyxCollection;
begin
  Result := View(False, ReadyLane).Store;
end;

procedure TBoardFixture.PrepareCommon;
var
  LPreparation: INyxEventViewPreparation;
  LBinding: INyxCoordinatorBinding;
begin

  if Registry = nil then
  begin
    Registry := NewNyxCoordinatorRegistry;
    Registry.RegisterFactory(NyxCustomKind(NyxKindName(nkKanbanBoard)),
      NyxExtension(NyxKanbanIntentKey), NewNyxKanbanCoordinatorFactory);
  end;

  if RefreshTarget = nil then
  begin
    RefreshTarget := TCommonRefresh.Create;
  end;
  Supports(Events, INyxEventViewPreparation, LPreparation);
  EventPlan := LPreparation.PrepareView;
  PrepareNyxCoordinatorMounts(Runtime, Bindings, EventPlan, Scheduler,
    Registry.Snapshot, RefreshTarget, Mounts, IDs);
  LBinding := FindNyxCoordinatorMount(Mounts, IDs, FirstBoard.ID);
  Supports(LBinding, INyxKanbanBinding, First);
  LBinding := FindNyxCoordinatorMount(Mounts, IDs, SecondBoard.ID);
  Supports(LBinding, INyxKanbanBinding, Second);
end;

procedure TBoardFixture.UseCommon;
begin

  if First <> nil then
  begin
    First.Disconnect;
  end;

  if Second <> nil then
  begin
    Second.Disconnect;
  end;
  First := nil;
  Second := nil;
  SetNyxKanbanIntent(FirstBoard, TNyxKanbanIntent.Configured(Options));
  SetNyxKanbanIntent(SecondBoard, TNyxKanbanIntent.Configured(Options));
end;

procedure TBoardFixture.AcceptCommon;
var
  LParticipants: TNyxPreparedPublications;
  LIndex: Integer;
begin
  PrepareCommon;
  SetLength(LParticipants, Length(Mounts) + 1);
  LParticipants[0] := EventPlan;
  for LIndex := 0 to High(Mounts) do
  begin
    LParticipants[LIndex + 1] := Mounts[LIndex];
  end;
  PublishNyxGroup(LParticipants);
end;

procedure TCommonRefresh.Request;
begin
  Inc(Calls);
end;

constructor TQueuedExecution.Create(const AParent: INyxExecution);
begin
  inherited Create;
  FParent := AParent;
  FStatus := nesPending;
end;

procedure TQueuedExecution.Cancel;
begin
  FCancelled := True;
end;

function TQueuedExecution.GetCancelled: Boolean;
begin
  Result := FCancelled or ((FParent <> nil) and FParent.Cancelled);
end;

function TQueuedExecution.GetStatus: TNyxExecutionStatus;
begin
  Result := FStatus;
end;

function TQueuedExecution.GetFailure: TNyxText;
begin
  Result := FFailure;
end;

procedure TQueuedExecution.Run(const AWork: INyxWork);
var
  LKeepAlive: IQueuedExecution;
  LWork: INyxWork;
begin
  LKeepAlive := Self;
  LWork := AWork;

  if GetCancelled then
  begin
    FStatus := nesCancelled;
    Exit;
  end;
  FStatus := nesRunning;
  try
    LWork.Execute(Self);

    if GetCancelled then
    begin
      FStatus := nesCancelled;
    end
    else
    begin
      FStatus := nesSucceeded;
    end;
  except
    on LException: Exception do
    begin
      FFailure := LException.Message;
      FStatus := nesFailed;
    end;
  end;
  FParent := nil;
end;

constructor TTurnScheduler.Create;
begin
  inherited Create;
  FInline := NewNyxScheduler;
end;

destructor TTurnScheduler.Destroy;
begin
  Shutdown;
  inherited Destroy;
end;

function TTurnScheduler.GetCapabilities: TNyxSchedulerCapabilities;
begin
  Result := [nscDeferredUI];
end;

procedure TTurnScheduler.RequireUI;
begin
  FInline.RequireUI;
end;

procedure TTurnScheduler.Admit(APolicy: TNyxExecutionPolicy);
begin
  RequireUI;

  if FClosed or not (APolicy in [neSequential, neUIQueue]) then
  begin
    raise ENyxSchedule.Create('Controlled test scheduler rejects this policy/state');
  end;
end;

function TTurnScheduler.Submit(const AWork: INyxWork;
  APolicy: TNyxExecutionPolicy): INyxExecution;
begin
  Admit(APolicy);

  if APolicy = neSequential then
  begin
    Result := FInline.Submit(AWork, neSequential);
  end
  else
  begin
    Result := PostUI(AWork);
  end;
end;

function TTurnScheduler.PostUI(const AWork: INyxWork;
  const AParent: INyxExecution): INyxExecution;
var
  LExecution: IQueuedExecution;
begin
  Admit(neUIQueue);
  LExecution := TQueuedExecution.Create(AParent);
  SetLength(FWorks, Length(FWorks) + 1);
  SetLength(FExecutions, Length(FExecutions) + 1);
  FWorks[High(FWorks)] := AWork;
  FExecutions[High(FExecutions)] := LExecution;
  Result := LExecution;
end;

procedure TTurnScheduler.Shutdown;
var
  LIndex: Integer;
begin
  FClosed := True;
  for LIndex := 0 to High(FExecutions) do
  begin
    FExecutions[LIndex].Cancel;
  end;
  FWorks := nil;
  FExecutions := nil;
  FInline.Shutdown;
end;

procedure TTurnScheduler.Drain;
var
  LWork: INyxWork;
  LExecution: IQueuedExecution;
  LIndex: Integer;
begin
  RequireUI;
  while Length(FWorks) > 0 do
  begin
    LWork := FWorks[0];
    LExecution := FExecutions[0];
    for LIndex := 0 to High(FWorks) - 1 do
    begin
      FWorks[LIndex] := FWorks[LIndex + 1];
      FExecutions[LIndex] := FExecutions[LIndex + 1];
    end;
    SetLength(FWorks, Length(FWorks) - 1);
    SetLength(FExecutions, Length(FExecutions) - 1);
    LExecution.Run(LWork);
  end;
end;

procedure TQueuedClick.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
begin
  Inc(Calls);
  SawMoved := (AEvent.Trigger = ntClick) and not AExecution.Cancelled and
    (Fixture.Store.Snapshot.Item(Card).GetValue(NyxTextField('lane')) = ActiveLane.Name);
end;

procedure TIntervention.Validate(const ACandidate: INyxCollectionSnapshot;
  const AChanges: INyxCollectionChanges);
begin
  Inc(ValidateCalls);
  Check((ACandidate.Item(Card).GetValue(NyxTextField('lane')) = ActiveLane.Name) and
    (AChanges.Before.Item(Card).GetValue(NyxTextField('lane')) = ReadyLane.Name),
    'validator receives the complete moved candidate and exact original data');
  case Kind of
    ikNone:
      begin
      end;
    ikCancelMove:
      begin
        Move.Cancel;
      end;
    ikDisconnect:
      begin
        Fixture.First.Disconnect;
      end;
    ikGeneration:
      begin
        Fixture.Events.CancelPending;
      end;
    ikReleaseCaller:
      begin
        Fixture.First := nil;
        GReleasedMove := nil;
      end;
  end;
end;

procedure TIntervention.Observe(const AStore: INyxCollection;
  const AChanges: INyxCollectionChanges);
begin
  Inc(ObserverCalls);
  AtomicViews := not Fixture.View(False, ReadyLane).Snapshot.Has(Card) and
    Fixture.View(False, ActiveLane).Snapshot.Has(Card) and
    (AStore.Snapshot.Revision = AChanges.After.Revision);

  if FailObserver then
  begin
    raise Exception.Create('Deliberate accepted-notification failure');
  end;
end;

procedure OptionCases;
var
  LOptions: TNyxKanbanOptions;
  LOriginal: TNyxKanbanOptions;
  LBoard: INyxKanbanBoard;
  LRejected: Boolean;
begin
  LOptions := NyxKanbanOptions(NyxCollection('tasks'), NyxTextField('title'),
    NyxTextField('lane')).Lane(NyxKanbanLane('🌙'), 'Moon');
  LOriginal := LOptions;
  LOptions := LOptions.Lane(ActiveLane, 'Active').Lane(DoneLane, 'Done');
  Check((LOriginal.Count = 1) and (LOptions.Count = 3) and
    (LOriginal.LaneAt(0).Name = TNyxText('🌙')), 'lane builders own copied Unicode data');
  LRejected := False;
  try
    LOriginal := LOriginal.Lane(NyxKanbanLane('🌙'), 'Duplicate');
  except
    on ENyxKanban do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LOptions.Count = 3),
    'duplicate lane refuses without changing prior options');
  LBoard := NewNyxKanbanBoard('plain-board');
  Check((LBoard.Lane0Column.Count = 3) and (LBoard.Lane1Column.Count = 3) and
    (LBoard.Lane2Column.Count = 3), 'default specialized recipe remains unchanged');
  LRejected := False;
  try
    ConfigureNyxKanbanBoard(LBoard, NyxKanbanOptions(NyxCollection('tasks'),
      NyxTextField('title'), NyxTextField('lane')).Lane(ReadyLane, 'Ready'));
  except
    on ENyxKanban do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LBoard.Lane0Column.Count = 3),
    'incomplete lane options refuse before editing the first lane');
  ConfigureNyxKanbanBoard(LBoard, LOptions);
  Check((LBoard.Lane0Column.Count = 4) and LBoard.Lane0ItemsList.Node.HasCollectionView,
    'public authoring helper adds an ordinary visible Move and typed binding');
  LRejected := False;
  try
    ConfigureNyxKanbanBoard(LBoard, LOptions);
  except
    on ENyxKanban do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LBoard.Lane0Column.Count = 4),
    'reconfiguration cannot duplicate or overwrite authored Move controls');
end;

procedure IntentCases;
var
  LFixture: TBoardFixture;
  LDefinition: INyxControl;
  LFirst: INyxControl;
  LTitle: INyxHeading;
  LMoveButton: INyxButton;
  LIntent: TNyxKanbanIntent;
  LWorkspace: TNyxSourceWorkspace;
  LCandidate: TNyxDocument;
  LRuntime: TNyxNode;
  LBefore: TNyxText;
  LSource: TNyxText;
  LBadSource: TNyxText;
  LOptionsStart: Integer;
  LRejected: Boolean;
begin
  LFixture := TBoardFixture.Create;
  LWorkspace := TNyxSourceWorkspace.Create;
  LCandidate := nil;
  LRuntime := nil;
  try
    LDefinition := RetainNyxControl(LFixture.Document.Find('work-board'));
    LFirst := RetainNyxControl(LFixture.Document.Find('first-board'));
    Check(not HasNyxKanbanIntent(LDefinition.Node) and
      (GetNyxKanbanIntent(LDefinition.Node).Mode = nkiUndefined),
      'legacy manual boards have no implicit automatic movement intent');
    LIntent := TNyxKanbanIntent.Configured(LFixture.Options);
    SetNyxKanbanIntent(LDefinition, LIntent);
    Check(TNyxKanbanIntent.FromData(LIntent.ToData).ToData.ToJSON =
      LIntent.ToData.ToJSON, 'complete typed intent survives its strict data boundary');
    SetNyxKanbanIntent(LFirst, TNyxKanbanIntent.Disabled);
    LRuntime := RealizeNyxView(LFixture.Document, LFixture.Document.Find('home'));
    Check((GetNyxKanbanIntent(LRuntime.Find(
      NyxQualifiedID('first-board', 'work-board'))).Mode = nkiDisabled) and
      (GetNyxKanbanIntent(LRuntime.Find(
      NyxQualifiedID('second-board', 'work-board'))).Mode = nkiConfigured),
      'one reusable disabled mask replaces inherited intent without changing its sibling');
    ReleaseNyxNode(LRuntime);
    LRuntime := nil;
    LTitle := LDefinition.Part(NyxPart('lane-1/title')) as INyxHeading;
    LMoveButton := LDefinition.Part(NyxPart('lane-1/move')) as INyxButton;
    LTitle.Text := 'Building together';
    LMoveButton.Text := 'Bring selected card here';
    ValidateNyxKanbanIntent(LDefinition.Node);
    LBefore := TNyxCodec.Encode(LFixture.Document);
    LCandidate := TNyxCodec.Decode(LBefore);
    Check(TNyxCodec.Encode(LCandidate) = LBefore,
      'saved JSON retains configured intent, disabled mask and customized part values');
    LCandidate.Free;
    LCandidate := nil;
    LSource := LWorkspace.Render(LFixture.Document);
    Check((Pos('SetNyxKanbanIntent(', LSource) > 0) and
      (Pos('TNyxKanbanIntent.Configured(', LSource) > 0) and
      (Pos('TNyxKanbanIntent.Disabled', LSource) > 0) and
      (Pos('ConfigureNyxKanbanBoard(', LSource) = 0),
      'crafted source preserves typed intent without constructing recipe children twice');
    LCandidate := LWorkspace.Candidate(LFixture.Document, LSource);
    Check(TNyxCodec.Encode(LCandidate) = LBefore,
      'configured definition, disabled instance and customized parts reconstruct exactly');
    LCandidate.Free;
    LCandidate := nil;
    LOptionsStart := Pos('NyxKanbanOptions(', LSource);
    Check(LOptionsStart > 0, 'configured source includes the public typed options builder');
    LBadSource := Copy(LSource, 1, LOptionsStart - 1) +
      StringReplace(Copy(LSource, LOptionsStart, MaxInt), 'NyxTextField(''title'')',
      'NyxIntegerField(''score'')', []);
    LRejected := False;
    try
      LCandidate := LWorkspace.Candidate(LFixture.Document, LBadSource);
    except
      on ENyxSource do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (TNyxCodec.Encode(LFixture.Document) = LBefore),
      'source cannot substitute another field family for the typed title reference');
    LRejected := False;
    try
      SetNyxKanbanIntent(LDefinition, Default(TNyxKanbanIntent));
    except
      on ENyxKanban do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (TNyxCodec.Encode(LFixture.Document) = LBefore),
      'undefined intent refuses atomically instead of silently enabling or inheriting');
    LRejected := False;
    try
      TNyxKanbanIntent.FromData(NyxObject([NyxField('version', NyxData(1)),
        NyxField('mode', NyxData('disabled')), NyxField('lanes', NyxArray([]))]));
    except
      on ENyxKanban do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'disabled intent rejects hidden configuration members');
    InheritNyxKanbanIntent(LFirst);
    LRuntime := RealizeNyxView(LFixture.Document, LFixture.Document.Find('home'));
    Check(not HasNyxKanbanIntent(LFirst.Node) and
      (GetNyxKanbanIntent(LRuntime.Find(
      NyxQualifiedID('first-board', 'work-board'))).Mode = nkiConfigured),
      'inherit removes only the local mask and reveals the exact definition intent');
  finally
    ReleaseNyxNode(LRuntime);
    LCandidate.Free;
    LMoveButton := nil;
    LTitle := nil;
    LFirst := nil;
    LDefinition := nil;
    LWorkspace.Free;
    LFixture.Free;
  end;
end;

procedure ModelCases;
var
  LFixture: TBoardFixture;
  LMove: INyxKanbanMove;
  LRetained: INyxCollectionSnapshot;
  LBefore: TNyxText;
  LRevision: Integer;
  LRejected: Boolean;
  LDuplicate: INyxKanbanBinding;
  LWorkspace: TNyxSourceWorkspace;
  LCandidate: TNyxDocument;
  LSource: TNyxText;
begin
  LFixture := TBoardFixture.Create;
  LWorkspace := TNyxSourceWorkspace.Create;
  LCandidate := nil;
  try
    LBefore := TNyxCodec.Encode(LFixture.Document);
    LSource := LWorkspace.Render(LFixture.Document);
    LCandidate := LWorkspace.Candidate(LFixture.Document, LSource);
    Check(TNyxCodec.Encode(LCandidate) = LBefore,
      'ordinary generated Pascal reconstructs the configured bindings and Move controls');
    LCandidate.Free;
    LCandidate := nil;
    Check(LFixture.First.Connected and LFixture.Second.Connected and
      (LFixture.Store <> LFixture.View(True, ReadyLane).Store),
      'realized reusable boards resolve independent instance stores');
    Check((LFixture.View(False, ReadyLane).Snapshot.Count = 2) and
      (LFixture.View(False, ActiveLane).Snapshot.Count = 1),
      'actual query-bound lanes expose their admitted source cards');
    Check(LFixture.First.MoveButton(ActiveLane).Node.Prop(
      NyxAttributeName(atAccessibleName), '') = 'Move selected card to In progress',
      'Move input exposes its exact destination in an accessible name');
    LRejected := False;
    try
      LDuplicate := NewNyxKanbanBinding(LFixture.FirstBoard, LFixture.Options,
        LFixture.Bindings, LFixture.Events, LFixture.Scheduler, nil);
    except
      on ENyxKanban do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LDuplicate = nil) and LFixture.First.Connected,
      'a second live binding cannot register another move owner on the same board');
    LRetained := LFixture.Store.Snapshot;
    LRevision := LRetained.Revision;
    LFixture.View(False, ReadyLane).Select(Card);
    Check(LFixture.First.MoveButton(ActiveLane).Node.Prop(
      NyxAttributeName(atEnabled), 'true') = 'true', 'visible selection enables Move input');
    LMove := LFixture.First.CaptureMove(Card, ReadyLane, ActiveLane, LRevision);
    LMove.Commit;
    Check(LMove.Committed and (LFixture.Store.Snapshot.Revision = LRevision + 1),
      'one captured move commits exactly one store revision');
    Check(not LFixture.View(False, ReadyLane).Snapshot.Has(Card) and
      LFixture.View(False, ActiveLane).Snapshot.Has(Card),
      'same stable card leaves source and enters destination');
    Check((LFixture.Store.Snapshot.Item(Card).GetValue(NyxIntegerField('score')) = 7) and
      (LFixture.Store.Snapshot.Item(Card).GetValue(NyxTextField('title')) =
        'Review a proposal'), 'move preserves unrelated typed fields');
    Check(LFixture.View(False, ReadyLane).Selection.Contains(Card) and
      (LFixture.First.MoveButton(DoneLane).Node.Prop(
        NyxAttributeName(atEnabled), 'true') = 'false'),
      'hidden retained selection cannot silently become new move input');
    Check((LFixture.View(True, ReadyLane).Snapshot.Count = 2) and
      (LFixture.View(True, ReadyLane).Store.Snapshot.Revision = 0) and
      (LRetained.Item(Card).GetValue(NyxTextField('lane')) = ReadyLane.Name),
      'sibling store and retained immutable snapshot remain exact');
    LRejected := False;
    try
      LMove.Commit;
    except
      on ENyxKanbanCancelled do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and LMove.Committed, 'committed command cannot be retried');
    LRejected := False;
    try
      LMove := LFixture.First.CaptureMove(Card, ReadyLane, DoneLane,
        LFixture.Store.Snapshot.Revision);
    except
      on ENyxKanban do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'current revision cannot rescue a hidden old source membership');
    Check(TNyxCodec.Encode(LFixture.Document) = LBefore,
      'runtime data movement never rewrites authored source/defaults/definition');
  finally
    LCandidate.Free;
    LMove := nil;
    LDuplicate := nil;
    LFixture.Free;
    LWorkspace.Free;
  end;
end;

procedure RefusalCases;
var
  LFixture: TBoardFixture;
  LMove: INyxKanbanMove;
  LStore: INyxCollection;
  LRevision: Integer;
  LRejected: Boolean;
begin
  LFixture := TBoardFixture.Create;
  try
    LStore := LFixture.Store;
    LRevision := LStore.Snapshot.Revision;
    LMove := LFixture.First.CaptureMove(Card, ReadyLane, ActiveLane, LRevision);
    LStore.Update(LStore.Snapshot.Item(Card).WithValue(NyxTextField('title'), 'New title'));
    LRejected := False;
    try
      LMove.Commit;
    except
      on ENyxCollection do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and not LMove.Committed and
      (LStore.Snapshot.Item(Card).GetValue(NyxTextField('lane')) = ReadyLane.Name) and
      (LStore.Snapshot.Revision = LRevision + 1),
      'stale revision preserves later application data');
    LRejected := False;
    try
      LMove := LFixture.First.CaptureMove(NyxItem(NyxCollection('foreign'), 'card-one'),
        ReadyLane, ActiveLane, LStore.Snapshot.Revision);
    except
      on ENyxCollection do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'same-spelled foreign collection item refuses');
    LRejected := False;
    try
      LMove := LFixture.First.CaptureMove(Card, ReadyLane,
        NyxKanbanLane('outside'), LStore.Snapshot.Revision);
    except
      on ENyxKanban do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'unconfigured destination lane refuses');
    LMove := LFixture.First.CaptureMove(Card, ReadyLane, ActiveLane,
      LStore.Snapshot.Revision);
    LFixture.First.Items(ActiveLane).Configure.ReadOnly(True).Done;
    LRejected := False;
    try
      LMove.Commit;
    except
      on ENyxKanban do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and not LMove.Committed, 'later read-only target revokes a captured move');
    LFixture.First.Items(ActiveLane).Configure.ReadOnly(False).Done;
    LMove := LFixture.First.CaptureMove(Card, ReadyLane, ActiveLane,
      LStore.Snapshot.Revision);
    LMove.Cancel;
    LRejected := False;
    try
      LMove.Commit;
    except
      on ENyxKanbanCancelled do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and not LMove.Committed, 'explicit cancellation refuses before preparation');
    LMove := LFixture.First.CaptureMove(Card, ReadyLane, ActiveLane,
      LStore.Snapshot.Revision);
    LFixture.View(False, ActiveLane).ConfigureQuery(NyxCollectionQuery);
    LRejected := False;
    try
      LMove.Commit;
    except
      on ENyxKanban do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and not LMove.Committed, 'changed destination projection invalidates the move');
  finally
    LMove := nil;
    LStore := nil;
    LFixture.Free;
  end;
end;

procedure ValidatorCase(AKind: TInterventionKind);
var
  LFixture: TBoardFixture;
  LProbe: TIntervention;
  LToken: INyxCollectionSubscription;
  LMove: INyxKanbanMove;
  LStore: INyxCollection;
  LBefore: INyxCollectionSnapshot;
  LRejected: Boolean;
begin
  LFixture := TBoardFixture.Create;
  LProbe := TIntervention.Create;
  try
    LProbe.Fixture := LFixture;
    LProbe.Kind := AKind;
    LStore := LFixture.Store;
    LBefore := LStore.Snapshot;
    LToken := LStore.Subscribe(LProbe.Observe, LProbe.Validate);
    LMove := LFixture.First.CaptureMove(Card, ReadyLane, ActiveLane, LBefore.Revision);
    LProbe.Move := LMove;
    LRejected := False;
    try
      LMove.Commit;
    except
      on ENyxKanbanCancelled do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and not LMove.Committed and (LProbe.ValidateCalls = 1) and
      (LProbe.ObserverCalls = 0), 'validator revocation refuses before installation');
    Check((LStore.Snapshot = LBefore) and
      (LStore.Snapshot.Item(Card).GetValue(NyxTextField('lane')) = ReadyLane.Name),
      'cancelled prepared candidate preserves exact accepted snapshot/revision');
    Check(LFixture.Bindings.ViewFor(LFixture.FirstBoard.Lane0ItemsList.ID).
      Snapshot.Has(Card), 'cancelled candidate never installs the source projection');
    Check(not LFixture.Bindings.ViewFor(LFixture.FirstBoard.Lane1ItemsList.ID).
      Snapshot.Has(Card), 'cancelled candidate never installs the destination projection');
  finally

    if LToken <> nil then
    begin
      LToken.Disconnect;
    end;
    LToken := nil;
    LProbe.Move := nil;
    LMove := nil;
    LFixture.Free;
    LProbe.Free;
  end;
end;

procedure NotificationCase;
var
  LFixture: TBoardFixture;
  LProbe: TIntervention;
  LToken: INyxCollectionSubscription;
  LMove: INyxKanbanMove;
  LRevision: Integer;
  LRejected: Boolean;
begin
  LFixture := TBoardFixture.Create;
  LProbe := TIntervention.Create;
  try
    LProbe.Fixture := LFixture;
    LProbe.FailObserver := True;
    LRevision := LFixture.Store.Snapshot.Revision;
    LToken := LFixture.Store.Subscribe(LProbe.Observe, LProbe.Validate);
    LMove := LFixture.First.CaptureMove(Card, ReadyLane, ActiveLane, LRevision);
    LRejected := False;
    try
      LMove.Commit;
    except
      on ENyxCollectionNotification do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and LMove.Committed and (LProbe.ObserverCalls = 1) and
      LProbe.AtomicViews,
      'notification failure reports already committed, jointly installed lanes');
    Check((LFixture.Store.Snapshot.Revision = LRevision + 1) and
      (LFixture.Store.Snapshot.Item(Card).GetValue(NyxTextField('lane')) = ActiveLane.Name),
      'notification failure never rolls back or retries accepted data');
  finally

    if LToken <> nil then
    begin
      LToken.Disconnect;
    end;
    LToken := nil;
    LMove := nil;
    LFixture.Free;
    LProbe.Free;
  end;
end;

procedure LifetimeCases;
var
  LFixture: TBoardFixture;
  LMove: INyxKanbanMove;
  LStore: INyxCollection;
  LToken: INyxCollectionSubscription;
  LProbe: TIntervention;
  LRejected: Boolean;
begin
  LFixture := TBoardFixture.Create;
  LMove := LFixture.First.CaptureMove(Card, ReadyLane, ActiveLane,
    LFixture.Store.Snapshot.Revision);
  LStore := LFixture.Store;
  LFixture.Free;
  LRejected := False;
  try
    LMove.Commit;
  except
    on ENyxKanbanCancelled do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and not LMove.Committed and (LStore.Snapshot.Revision = 0),
    'retained command after complete page disposal owns no live attachment authority');
  LMove := nil;
  LStore := nil;
  LFixture := TBoardFixture.Create;
  LProbe := TIntervention.Create;
  try
    LProbe.Fixture := LFixture;
    LProbe.Kind := ikReleaseCaller;
    LStore := LFixture.Store;
    LToken := LStore.Subscribe(nil, LProbe.Validate);
    GReleasedMove := LFixture.First.CaptureMove(Card, ReadyLane, ActiveLane,
      LStore.Snapshot.Revision);
    GReleasedMove.Commit;
    Check((GReleasedMove = nil) and (LFixture.First = nil) and
      (LStore.Snapshot.Item(Card).GetValue(NyxTextField('lane')) = ActiveLane.Name),
      'validator may release caller binding/command slots without invalidating the active leases');
    Check(LFixture.FirstBoard.Lane0Column.Node.Part(NyxPart('move')).
      EnabledPolicyCount = 0, 'last active lease retires policies and weak event callbacks');
    LToken.Disconnect;
    LToken := nil;
    LStore.Update(LStore.Snapshot.Item(Card).WithValue(NyxTextField('title'), 'Still owned'));
    Check(LStore.Snapshot.Item(Card).GetValue(NyxTextField('title')) = 'Still owned',
      'independent store remains usable after attachment retirement');
  finally

    if LToken <> nil then
    begin
      LToken.Disconnect;
    end;
    GReleasedMove := nil;
    LToken := nil;
    LFixture.Free;
    LProbe.Free;
  end;
end;

function EventFor(ATrigger: TNyxTrigger; const AOrigin: INyxControl;
  const ABoard: INyxKanbanBoard): TNyxEventInfo;
begin
  Result := Default(TNyxEventInfo);
  Result.Value := NyxNull;
  Result.Trigger := ATrigger;
  Result.Name := NyxEvent(NyxTriggerName(ATrigger));
  Result.OriginID := AOrigin.ID;
  Result.TargetID := AOrigin.ID;
  Result.SourceID := ABoard.ID;
end;

procedure InputCases;
var
  LFixture: TBoardFixture;
  LEvent: TNyxEventInfo;
  LDecision: INyxGestureDecision;
  LResult: TNyxGestureResult;
  LTransfer: TNyxTransferSnapshot;
  LSource: INyxList;
  LTarget: INyxList;
  LMoveButton: INyxButton;
  LRevision: Integer;
begin
  LFixture := TBoardFixture.Create;
  try
    LFixture.View(False, ReadyLane).Select(Card);
    LMoveButton := LFixture.First.MoveButton(ActiveLane);
    LEvent := EventFor(ntClick, LMoveButton, LFixture.FirstBoard);
    LRevision := LFixture.Store.Snapshot.Revision;
    LFixture.Events.Dispatch(LEvent, LMoveButton.Node.DesignID,
      LFixture.FirstBoard.Node.DesignID);
    Check((LFixture.Store.Snapshot.Revision = LRevision + 1) and
      LFixture.View(False, ActiveLane).Snapshot.Has(Card),
      'normal routed Click applies its synchronous selected-card Move default');
    LFixture.View(False, ActiveLane).Select(Card);
    LSource := LFixture.First.Items(ActiveLane);
    LTarget := LFixture.First.Items(DoneLane);
    LEvent := EventFor(ntDragStart, LSource, LFixture.FirstBoard);
    LEvent.HasDrag := True;
    LEvent.Drag := NyxDragSnapshot(ndpStart, NyxTransferFromData(NyxArray([]), True),
      [ndoMove], ndoNone, LSource.ID, True);
    LDecision := NewNyxGestureDecision([ngcOfferDrag], [ndoMove]);
    LFixture.Events.DispatchGesture(LEvent, LSource.Node.DesignID,
      LFixture.FirstBoard.Node.DesignID, LDecision);
    LResult := LDecision.Seal;
    Check(LResult.Offered and (LResult.Allowed = [ndoMove]),
      'local selected-card drag offers only the admitted Move operation');
    LTransfer := LResult.Transfer;
    LRevision := LFixture.Store.Snapshot.Revision;
    LEvent := EventFor(ntDragOver, LTarget, LFixture.FirstBoard);
    LEvent.HasDrag := True;
    LEvent.Drag := NyxDragSnapshot(ndpOver, LTransfer.ProtectedCopy,
      [ndoMove], ndoNone, LSource.ID, True);
    LDecision := NewNyxGestureDecision([ngcAcceptDrop], [ndoMove]);
    LFixture.Events.DispatchGesture(LEvent, LTarget.Node.DesignID,
      LFixture.FirstBoard.Node.DesignID, LDecision);
    Check(LDecision.Seal.Accepted and (LFixture.Store.Snapshot.Revision = LRevision),
      'protected hover negotiates without reading data or mutating cards');
    LEvent.Trigger := ntDrop;
    LEvent.Name := NyxEvent(NyxTriggerName(ntDrop));
    LEvent.Drag := NyxDragSnapshot(ndpDrop, LTransfer, [ndoMove], ndoMove, LSource.ID, True);
    LDecision := NewNyxGestureDecision([ngcAcceptDrop], [ndoMove]);
    LFixture.Events.DispatchGesture(LEvent, LTarget.Node.DesignID,
      LFixture.FirstBoard.Node.DesignID, LDecision);
    Check(LDecision.Seal.Accepted and LFixture.View(False, DoneLane).Snapshot.Has(Card) and
      (LFixture.Store.Snapshot.Revision = LRevision + 1),
      'actual typed drop event shares the revision-checked selected-card command');
    LDecision := NewNyxGestureDecision([ngcAcceptDrop], [ndoMove]);
    LFixture.Events.DispatchGesture(LEvent, LTarget.Node.DesignID,
      LFixture.FirstBoard.Node.DesignID, LDecision);
    Check(not LDecision.Seal.Accepted and (LFixture.Store.Snapshot.Revision = LRevision + 1),
      'repeated/external transfer cannot replay consumed local drag authority');
    Check(LFixture.View(True, ReadyLane).Snapshot.Has(Card),
      'event and drop paths preserve another instance with the same card identity');
  finally
    LFixture.Free;
  end;
end;

procedure PolicyCases(ACommon: Boolean = False);
var
  LSchedulerOwner: TTurnScheduler;
  LScheduler: INyxScheduler;
  LFixture: TBoardFixture;
  LProbe: TQueuedClick;
  LProbeOwner: INyxEventCallback;
  LToken: INyxEventSubscription;
  LStream: INyxEventStream;
  LEvent: TNyxEventInfo;
  LDecision: INyxGestureDecision;
  LTransfer: TNyxTransferSnapshot;
  LExecutions: TNyxExecutions;
  LButton: INyxButton;
  LSource: INyxList;
  LTarget: INyxList;
  LRevision: Integer;
begin
  LSchedulerOwner := TTurnScheduler.Create;
  LScheduler := LSchedulerOwner;
  LFixture := TBoardFixture.Create(LScheduler);
  LProbe := TQueuedClick.Create;
  LProbeOwner := LProbe;
  try
    LButton := LFixture.First.MoveButton(ActiveLane);

    if ACommon then
    begin
      LFixture.UseCommon;
    end
    else
    begin
      LFixture.First.Disconnect;
      LFixture.First := nil;
    end;
    LStream := LFixture.Events.On(NyxControlEvents(LButton.ID, niRuntime), ntClick).
      Policy(neUIQueue);
    LProbe.Fixture := LFixture;
    LToken := LStream.Subscribe(LProbeOwner);

    if ACommon then
    begin
      LFixture.AcceptCommon;
    end
    else
    begin
      LFixture.First := NewNyxKanbanBinding(LFixture.FirstBoard, LFixture.Options,
        LFixture.Bindings, LFixture.Events, LScheduler, nil);
    end;
    Check(LStream.ExecutionPolicy = neUIQueue,
      'binding admission preserves the application Click execution policy');
    LFixture.View(False, ReadyLane).Select(Card);
    LRevision := LFixture.Store.Snapshot.Revision;
    LEvent := EventFor(ntClick, LButton, LFixture.FirstBoard);
    LExecutions := LFixture.Events.Dispatch(LEvent, LButton.Node.DesignID,
      LFixture.FirstBoard.Node.DesignID);
    Check((LFixture.Store.Snapshot.Revision = LRevision + 1) and
      (LProbe.Calls = 0) and (LToken.LastExecution.Status = nesPending),
      'synchronous Move default publishes once before queued application callbacks');
    LStream.Policy(neSequential);
    LSchedulerOwner.Drain;
    Check((LProbe.Calls = 1) and LProbe.SawMoved and
      (LToken.LastExecution.Status = nesSucceeded) and
      (LFixture.Store.Snapshot.Revision = LRevision + 1),
      'policy reset drains only the application callback without a delayed second Move');
    LFixture.View(False, ActiveLane).Select(Card);
    LSource := LFixture.First.Items(ActiveLane);
    LTarget := LFixture.First.Items(DoneLane);
    LStream := LFixture.Events.OnDragStart(NyxControlEvents(LSource.ID, niRuntime)).
      Policy(neUIQueue);
    LEvent := EventFor(ntDragStart, LSource, LFixture.FirstBoard);
    LEvent.HasDrag := True;
    LEvent.Drag := NyxDragSnapshot(ndpStart, NyxTransferFromData(NyxArray([]), True),
      [ndoMove], ndoNone, LSource.ID, True);
    LDecision := NewNyxGestureDecision([ngcOfferDrag], [ndoMove]);
    LExecutions := LFixture.Events.DispatchGesture(LEvent, LSource.Node.DesignID,
      LFixture.FirstBoard.Node.DesignID, LDecision);
    Check(not LDecision.Seal.Offered and (LExecutions[0].Status = nesPending),
      'queued drag callback receives no synchronous offer authority');
    LStream.Policy(neSequential);
    LDecision := NewNyxGestureDecision([ngcOfferDrag], [ndoMove]);
    LFixture.Events.DispatchGesture(LEvent, LSource.Node.DesignID,
      LFixture.FirstBoard.Node.DesignID, LDecision);
    Check(LDecision.Seal.Offered,
      'new sequential drag starts before the older queued callback drains');
    LTransfer := LDecision.Seal.Transfer;
    LSchedulerOwner.Drain;
    Check(LExecutions[0].Status = nesSucceeded,
      'late queued start is inert after its original response scope seals');
    LEvent := EventFor(ntDrop, LTarget, LFixture.FirstBoard);
    LEvent.HasDrag := True;
    LEvent.Drag := NyxDragSnapshot(ndpDrop, LTransfer, [ndoMove], ndoMove, LSource.ID, True);
    LDecision := NewNyxGestureDecision([ngcAcceptDrop], [ndoMove]);
    LFixture.Events.DispatchGesture(LEvent, LTarget.Node.DesignID,
      LFixture.FirstBoard.Node.DesignID, LDecision);
    Check(LDecision.Seal.Accepted and LFixture.View(False, DoneLane).Snapshot.Has(Card),
      'older queued start cannot clear the later live selected-card drag');
  finally

    if LToken <> nil then
    begin
      LToken.Cancel;
    end;
    LToken := nil;
    LProbeOwner := nil;
    LFixture.Free;
    LScheduler := nil;
  end;
end;

procedure TPreparedRefreshProbe.Refresh;
begin
  Inc(Calls);

  if ReleasePlan then
  begin
    GPreparedPlan := nil;
  end;
end;

procedure TPreparedSelection.Validate;
begin
  View.Select(Card);
end;

procedure TPreparedSelection.Install;
begin
end;

procedure TPreparedSelection.Notify;
begin
end;

procedure TPreparedSelection.Retire;
begin
end;

procedure DetachManual(AFixture: TBoardFixture);
begin
  AFixture.First.Disconnect;
  AFixture.Second.Disconnect;
  AFixture.First := nil;
  AFixture.Second := nil;
end;

procedure PreparedActivationCase;
var
  LSchedulerOwner: TTurnScheduler;
  LScheduler: INyxScheduler;
  LFixture: TBoardFixture;
  LPreparation: INyxEventViewPreparation;
  LEventPlan: INyxPreparedEventView;
  LPlan: INyxPreparedKanbanBinding;
  LPrevious: INyxCancellationScope;
  LFuture: INyxCancellationScope;
  LView: INyxCollectionView;
  LSibling: INyxCollection;
  LStore: INyxCollection;
  LButton: INyxButton;
  LHeading: INyxHeading;
  LSpec: TNyxCollectionViewSpec;
  LStream: INyxEventStream;
  LToken: INyxEventSubscription;
  LClick: TQueuedClick;
  LClickLease: INyxEventCallback;
  LEvent: TNyxEventInfo;
  LExecutions: TNyxExecutions;
  LProbe: TPreparedRefreshProbe;
  LMove: INyxKanbanMove;
  LBefore: TNyxText;
  LRejected: Boolean;
begin
  LSchedulerOwner := TTurnScheduler.Create;
  LScheduler := LSchedulerOwner;
  LFixture := TBoardFixture.Create(LScheduler);
  LProbe := TPreparedRefreshProbe.Create;
  try
    DetachManual(LFixture);
    LBefore := TNyxCodec.Encode(LFixture.Document);
    LButton := LFixture.FirstBoard.Part(NyxPart('lane-1/move')) as INyxButton;
    LHeading := LFixture.FirstBoard.Part(NyxPart('lane-1/title')) as INyxHeading;
    LButton.Text := 'Bring the selected card';
    LHeading.Text := 'Work together';
    LSpec := NyxCollectionView(LFixture.Options.Collection).
      Column(LFixture.Options.TitleField, 'Our cards').Scoped(csInstance).
      Selection(nsmSingle).Query(NyxCollectionQuery.Where(
        NyxWhere(LFixture.Options.LaneField).EqualTo(ReadyLane.Name, nqtExact)));
    LFixture.FirstBoard.Lane0ItemsList.Node.SetCollectionView(LSpec);
    LFixture.Bindings := LFixture.Bindings.Recompose(LFixture.Runtime);
    LView := LFixture.Bindings.ViewFor(LFixture.FirstBoard.Lane0ItemsList.ID);
    LStore := LView.Store;
    LSibling := LFixture.Bindings.ViewFor(LFixture.SecondBoard.Lane0ItemsList.ID).Store;
    LView.Select(Card);
    LStream := LFixture.Events.On(NyxControlEvents(LButton.ID, niRuntime), ntClick).
      Policy(neUIQueue);
    LClick := TQueuedClick.Create;
    LClickLease := LClick;
    LClick.Fixture := LFixture;
    LToken := LStream.Subscribe(LClickLease);
    Supports(LFixture.Events, INyxEventViewPreparation, LPreparation);
    LPrevious := NyxEventGeneration(LFixture.Events);
    LEventPlan := LPreparation.PrepareView;
    LFuture := LEventPlan.Generation;
    LPlan := PrepareNyxKanbanBinding(LFixture.FirstBoard, LFixture.Options,
      LFixture.Bindings, LEventPlan, LScheduler, LProbe.Refresh);
    LFixture.First := LPlan.Binding;
    Check((LFixture.First <> nil) and not LFixture.First.Connected and
      not LPrevious.Cancelled and not LFuture.Cancelled,
      'a prepared binding exposes no live movement before its prospective view installs');
    Check((LButton.Node.Prop(NyxAttributeName(atEnabled), 'true') = 'true') and
      (LProbe.Calls = 0),
      'cached selected-card availability is ready for first Sync without a host refresh');
    LEvent := EventFor(ntClick, LButton, LFixture.FirstBoard);
    LExecutions := LFixture.Events.Dispatch(LEvent, LButton.Node.DesignID,
      LFixture.FirstBoard.Node.DesignID);
    Check((LStore.Snapshot.Item(Card).GetValue(LFixture.Options.LaneField) =
      ReadyLane.Name) and (Length(LExecutions) = 1) and (LClick.Calls = 0),
      'dormant default slots cannot move data while independent application work stays queued');
    Check(LStream.ExecutionPolicy = neUIQueue,
      'prepared input preserves the ordinary caller-owned Click policy');
    PublishNyxGroup([LEventPlan, LPlan]);
    Check(LFixture.First.Connected and LPrevious.Cancelled and
      (NyxEventGeneration(LFixture.Events) = LFuture) and (LProbe.Calls = 0),
      'coordinated installation adopts the exact future generation without calling the host');
    Check((LButton.Text = 'Bring the selected card') and
      (LHeading.Text = 'Work together') and
      (LView.Spec.ColumnAt(0).Title = 'Our cards'),
      'automatic behavioral admission preserves button, heading and column cosmetics');
    LExecutions := LFixture.Events.Dispatch(LEvent, LButton.Node.DesignID,
      LFixture.FirstBoard.Node.DesignID);
    Check((LStore.Snapshot.Item(Card).GetValue(LFixture.Options.LaneField) =
      ActiveLane.Name) and (LProbe.Calls > 0) and (LClick.Calls = 0),
      'installed synchronous Move publishes through the real store before queued callbacks');
    LSchedulerOwner.Drain;
    Check((LClick.Calls = 1) and LClick.SawMoved and
      (LStream.ExecutionPolicy = neUIQueue),
      'only the accepted-generation application callback executes after the synchronous move');
    LMove := LFixture.First.CaptureMove(Card, ActiveLane, DoneLane,
      LStore.Snapshot.Revision);
    LPlan.Revoke;
    Check(not LFixture.First.Connected and
      (LButton.Node.Prop(NyxAttributeName(atEnabled), 'true') = 'false'),
      'flag-only revocation closes input and availability before old controls retire');
    LRejected := False;
    try
      LMove.Commit;
    except
      on ENyxKanbanCancelled do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LStore.Snapshot.Item(Card).
      GetValue(LFixture.Options.LaneField) = ActiveLane.Name),
      'a retained captured move cannot outlive its revoked automatic attachment');
    LPlan.Disconnect;
    LPlan.Disconnect;
    Check((LButton.Node.EnabledPolicyCount = 0) and
      (LButton.Node.StoredProp(NyxAttributeName(atEnabled), 'true') = 'true') and
      (LButton.Text = 'Bring the selected card'),
      'cleanup detaches only runtime availability and preserves authored presentation');
    Check((TNyxCodec.Encode(LFixture.Document) = LBefore) and
      (LSibling.Snapshot.Revision = 0) and
      (LSibling.Snapshot.Item(Card).GetValue(LFixture.Options.LaneField) = ReadyLane.Name),
      'automatic movement leaves the definition, saved document and sibling store exact');
  finally

    if LPlan <> nil then
    begin
      LPlan.Disconnect;
    end;

    if LToken <> nil then
    begin
      LToken.Cancel;
    end;
    LPlan := nil;
    LEventPlan := nil;
    LPreparation := nil;
    LProbe.Free;
    LFixture.Free;
    LScheduler := nil;
  end;
end;

procedure PreparedRefusalCases;
var
  LFixture: TBoardFixture;
  LPreparation: INyxEventViewPreparation;
  LEventPlan: INyxPreparedEventView;
  LPlan: INyxPreparedKanbanBinding;
  LView: INyxCollectionView;
  LStore: INyxCollection;
  LAtomic: INyxAtomicCollection;
  LReservation: INyxPreparedPublication;
  LPrevious: INyxCancellationScope;
  LMutation: TPreparedSelection;
  LMutationLease: INyxPreparedPublication;
  LIndex: Integer;
  LRejected: Boolean;
  LBefore: TNyxText;
begin
  for LIndex := 0 to 6 do
  begin
    LFixture := TBoardFixture.Create;
    try
      DetachManual(LFixture);
      LBefore := TNyxCodec.Encode(LFixture.Document);
      LView := LFixture.Bindings.ViewFor(LFixture.FirstBoard.Lane0ItemsList.ID);
      LStore := LView.Store;
      LPrevious := NyxEventGeneration(LFixture.Events);
      Supports(LFixture.Events, INyxEventViewPreparation, LPreparation);
      LEventPlan := LPreparation.PrepareView;
      LPlan := PrepareNyxKanbanBinding(LFixture.FirstBoard, LFixture.Options,
        LFixture.Bindings, LEventPlan, LFixture.Scheduler, nil);
      case LIndex of
        0:
          begin
            LView.Select(Card);
          end;
        1:
          begin
            LView.ConfigureQuery(NyxCollectionQuery);
          end;
        2:
          begin
            Supports(LStore, INyxAtomicCollection, LAtomic);
            LReservation := LAtomic.PrepareAssign(LStore.Snapshot,
              LStore.Snapshot.Revision);
          end;
        3:
          begin
            LFixture.FirstBoard.Lane0ItemsList.Node.RemoveCollectionView;
          end;
        4:
          begin
            LPlan.Revoke;
          end;
        5:
          begin
            LEventPlan.Generation.Cancel;
          end;
        6:
          begin
            LMutation := TPreparedSelection.Create;
            LMutationLease := LMutation;
            LMutation.View := LView;
          end;
      end;
      LRejected := False;
      try

        if LMutationLease <> nil then
        begin
          PublishNyxGroup([LEventPlan, LPlan, LMutationLease]);
        end
        else
        begin
          PublishNyxGroup([LEventPlan, LPlan]);
        end;
      except
        on ENyxKanban do
        begin
          LRejected := True;
        end;
        on ENyxSchedule do
        begin
          LRejected := True;
        end;
      end;
      Check(LRejected and not LPrevious.Cancelled and not LPlan.Binding.Connected,
        'prepared refusal preserves mounted generation for case ' + IntToStr(LIndex));
      Check((LStore.Snapshot.Revision = 0) and
        (TNyxCodec.Encode(LFixture.Document) = LBefore),
        'dirty/busy/retired preparation cannot publish data or source for case ' +
        IntToStr(LIndex));
    finally

      if LReservation <> nil then
      begin
        LReservation.Retire;
      end;
      LReservation := nil;
      LAtomic := nil;
      LMutationLease := nil;
      LMutation := nil;
      LPlan := nil;
      LEventPlan := nil;
      LPreparation := nil;
      LView := nil;
      LStore := nil;
      LFixture.Free;
    end;
  end;
end;

procedure PreparedDisabledCase;
var
  LFixture: TBoardFixture;
  LPreparation: INyxEventViewPreparation;
  LEventPlan: INyxPreparedEventView;
  LPlan: INyxPreparedKanbanBinding;
  LButton: INyxButton;
  LStore: INyxCollection;
  LEvent: TNyxEventInfo;
  LExecutions: TNyxExecutions;
begin
  LFixture := TBoardFixture.Create;
  try
    DetachManual(LFixture);
    LButton := LFixture.FirstBoard.Part(NyxPart('lane-1/move')) as INyxButton;
    LButton.Text := 'A customized move face';
    LStore := LFixture.Bindings.ViewFor(LFixture.FirstBoard.Lane0ItemsList.ID).Store;
    Supports(LFixture.Events, INyxEventViewPreparation, LPreparation);
    LEventPlan := LPreparation.PrepareView;
    LPlan := PrepareNyxKanbanDisabled(LFixture.FirstBoard, LEventPlan, LFixture.Scheduler);
    Check((LPlan.Binding = nil) and
      (LButton.Node.Prop(NyxAttributeName(atEnabled), 'true') = 'false') and
      (LButton.Node.StoredProp(NyxAttributeName(atEnabled), 'true') = 'true'),
      'disabled intent is a runtime mask without a fabricated data controller');
    PublishNyxGroup([LEventPlan, LPlan]);
    LEvent := EventFor(ntClick, LButton, LFixture.FirstBoard);
    LExecutions := LFixture.Events.Dispatch(LEvent, LButton.Node.DesignID,
      LFixture.FirstBoard.Node.DesignID);
    Check((Length(LExecutions) = 0) and (LStore.Snapshot.Revision = 0) and
      (LFixture.FirstBoard.Lane0ItemsList.Node.Prop(
        NyxAttributeName(atReadOnly), 'false') = 'false'),
      'disabled masks register no movement or unrelated list editing restriction');
    LPlan.Revoke;
    LPlan.Disconnect;
    Check((LButton.Node.Prop(NyxAttributeName(atEnabled), 'true') = 'true') and
      (LButton.Text = 'A customized move face'),
      'retiring a disabled mask restores the preserved customized face');
  finally

    if LPlan <> nil then
    begin
      LPlan.Disconnect;
    end;
    LPlan := nil;
    LEventPlan := nil;
    LPreparation := nil;
    LFixture.Free;
  end;
end;

procedure PreparedLifetimeCase;
var
  LFixture: TBoardFixture;
  LPreparation: INyxEventViewPreparation;
  LEventPlan: INyxPreparedEventView;
  LProbe: TPreparedRefreshProbe;
  LView: INyxCollectionView;
  LStore: INyxCollection;
  LBinding: INyxKanbanBinding;
  LMove: INyxKanbanMove;

  procedure Stage;
  begin
    GPreparedPlan := PrepareNyxKanbanBinding(LFixture.FirstBoard, LFixture.Options,
      LFixture.Bindings, LEventPlan, LFixture.Scheduler, LProbe.Refresh);
  end;

begin
  LFixture := TBoardFixture.Create;
  LProbe := TPreparedRefreshProbe.Create;
  try
    DetachManual(LFixture);
    LView := LFixture.Bindings.ViewFor(LFixture.FirstBoard.Lane0ItemsList.ID);
    LStore := LView.Store;
    Supports(LFixture.Events, INyxEventViewPreparation, LPreparation);
    LEventPlan := LPreparation.PrepareView;
    Stage;
    LBinding := GPreparedPlan.Binding;
    PublishNyxGroup([LEventPlan, GPreparedPlan]);
    LProbe.ReleasePlan := True;
    LMove := LBinding.CaptureMove(Card, ReadyLane, ActiveLane, LStore.Snapshot.Revision);
    LMove.Commit;
    Check(LMove.Committed and (GPreparedPlan = nil) and not LBinding.Connected and
      (LProbe.Calls = 1),
      'a committed notification may release the last attachment-plan lease safely');
    Check(LStore.Snapshot.Item(Card).GetValue(LFixture.Options.LaneField) = ActiveLane.Name,
      'callback disposal preserves the already installed card movement');
  finally
    GPreparedPlan := nil;
    LBinding := nil;
    LEventPlan := nil;
    LPreparation := nil;
    LProbe.Free;
    LFixture.Free;
  end;
end;

procedure PreparedSourceActivationCase;
var
  LFixture: TBoardFixture;
  LPreparation: INyxEventViewPreparation;
  LEventPlan: INyxPreparedEventView;
  LPlan: INyxPreparedKanbanBinding;
  LReady: INyxCollectionView;
  LActive: INyxCollectionView;
  LButton: INyxButton;
  LEvent: TNyxEventInfo;
begin
  LFixture := TBoardFixture.Create;
  try
    DetachManual(LFixture);
    LReady := LFixture.Bindings.ViewFor(LFixture.FirstBoard.Lane0ItemsList.ID);
    LActive := LFixture.Bindings.ViewFor(LFixture.FirstBoard.Lane1ItemsList.ID);
    LReady.Select(Card);
    LActive.Select(NyxItem(LFixture.Options.Collection, 'card-three'));
    Supports(LFixture.Events, INyxEventViewPreparation, LPreparation);
    LEventPlan := LPreparation.PrepareView;
    LPlan := PrepareNyxKanbanBinding(LFixture.FirstBoard, LFixture.Options,
      LFixture.Bindings, LEventPlan, LFixture.Scheduler, nil);
    LButton := LFixture.FirstBoard.Part(NyxPart('lane-2/move')) as INyxButton;
    Check(LButton.Node.Prop(NyxAttributeName(atEnabled), 'true') = 'false',
      'several retained lane selections never invent one initial active source');
    PublishNyxGroup([LEventPlan, LPlan]);
    LEvent := EventFor(ntClick, LFixture.FirstBoard.Lane0ItemsList, LFixture.FirstBoard);
    LFixture.Events.Dispatch(LEvent, LFixture.FirstBoard.Lane0ItemsList.Node.DesignID,
      LFixture.FirstBoard.Node.DesignID);
    Check((LButton.Node.Prop(NyxAttributeName(atEnabled), 'true') = 'true') and
      (LReady.Selection.Count = 1) and (LActive.Selection.Count = 1) and
      (LReady.Store.Snapshot.Revision = 0),
      'clicking an already-selected row establishes source intent without selection mutation');
    LEvent := EventFor(ntAfterEnter, LFixture.FirstBoard.Lane1ItemsList, LFixture.FirstBoard);
    LFixture.Events.Dispatch(LEvent, LFixture.FirstBoard.Lane1ItemsList.Node.DesignID,
      LFixture.FirstBoard.Node.DesignID);
    LEvent := EventFor(ntClick, LButton, LFixture.FirstBoard);
    LFixture.Events.Dispatch(LEvent, LButton.Node.DesignID,
      LFixture.FirstBoard.Node.DesignID);
    Check((LReady.Store.Snapshot.Item(Card).GetValue(LFixture.Options.LaneField) =
      ReadyLane.Name) and (LReady.Store.Snapshot.Item(
      NyxItem(LFixture.Options.Collection, 'card-three')).GetValue(
      LFixture.Options.LaneField) = DoneLane.Name),
      'real focus entry chooses the exact retained source lane for subsequent Move');
  finally

    if LPlan <> nil then
    begin
      LPlan.Disconnect;
    end;
    LPlan := nil;
    LEventPlan := nil;
    LPreparation := nil;
    LFixture.Free;
  end;
end;

{ Common staging uses the same real boards/views as every manual assertion.
  Retained facades are the actual canonical nodes, not ID-based stand-ins. }
procedure CommonCanonicalCase;
var
  LFixture: TBoardFixture;
  LRefresh: TCommonRefresh;
  LPrevious: INyxCancellationScope;
  LBase: INyxCoordinatorBinding;
  LTyped: INyxKanbanBinding;
  LItems: INyxList;
  LButton: INyxButton;
  LView: INyxCollectionView;
  LStore: INyxCollection;
  LMove: INyxKanbanMove;
  LEvent: TNyxEventInfo;
  LExecutions: TNyxExecutions;
  LBefore: TNyxText;
  LRejected: Boolean;
begin
  LFixture := TBoardFixture.Create;
  try
    LFixture.UseCommon;
    LBefore := TNyxCodec.Encode(LFixture.Document);
    LView := LFixture.Bindings.ViewFor(LFixture.FirstBoard.Lane0ItemsList.ID);
    LStore := LView.Store;
    LView.Select(Card);
    LRefresh := TCommonRefresh.Create;
    LFixture.RefreshTarget := LRefresh;
    LPrevious := NyxEventGeneration(LFixture.Events);
    LFixture.PrepareCommon;
    Check(Supports(LFixture.FirstBoard.Part(NyxPart('lane-1/move')),
      INyxButton, LButton), 'common Move face preserves the public button interface');
    Check(not LFixture.First.Connected and not LPrevious.Cancelled and
      (LButton.Node.Prop(NyxAttributeName(atEnabled), 'true') = 'true') and
      (LRefresh.Calls = 0),
      'common staging caches availability without activating commands or refreshing the host');
    LEvent := EventFor(ntClick, LButton, LFixture.FirstBoard);
    LExecutions := LFixture.Events.Dispatch(LEvent, LButton.Node.DesignID,
      LFixture.FirstBoard.Node.DesignID);
    Check((Length(LExecutions) = 0) and (LStore.Snapshot.Revision = 0),
      'common future observations remain invisible to the still-current router');
    PublishNyxGroup([LFixture.EventPlan, LFixture.Mounts[0], LFixture.Mounts[1]]);
    LBase := FindNyxCoordinatorMount(LFixture.Mounts, LFixture.IDs, LFixture.FirstBoard.ID);
    Check(LPrevious.Cancelled and LFixture.First.Connected and
      Supports(LBase, INyxKanbanBinding, LTyped) and (LTyped = LFixture.First) and
      (LRefresh.Calls = 0),
      'common installation preserves exact specialized identity and makes no host call');
    LItems := LTyped.Items(ReadyLane);
    LButton := LTyped.MoveButton(ActiveLane);
    Check((LItems.Node = LFixture.FirstBoard.Lane0ItemsList.Node) and
      (LButton.Node = LFixture.FirstBoard.Node.Part(NyxPart('lane-1/move'))) and
      (LTyped.Items(ReadyLane) = LItems) and (LTyped.MoveButton(ActiveLane) = LButton),
      'canonical lane and Move getters retain their real typed facades and descriptor identity');
    LItems.Configure.ReadOnly(True).Done;
    LRejected := False;
    try
      LMove := LTyped.CaptureMove(Card, ReadyLane, ActiveLane, LStore.Snapshot.Revision);
    except
      on ENyxKanban do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LMove = nil) and (LStore.Snapshot.Revision = 0),
      'configuring ReadOnly through the real returned facade still refuses card mutation');
    LItems.Configure.ReadOnly(False).Done;
    LMove := LTyped.CaptureMove(Card, ReadyLane, ActiveLane, LStore.Snapshot.Revision);
    LMove.Commit;
    Check(LMove.Committed and (LRefresh.Calls > 0) and
      LFixture.View(False, ActiveLane).Snapshot.Has(Card) and
      (LFixture.View(True, ReadyLane).Store.Snapshot.Revision = 0) and
      (TNyxCodec.Encode(LFixture.Document) = LBefore),
      'common movement shares the existing store engine and guarded refresh ' +
      'without source leakage');
  finally
    LTyped := nil;
    LBase := nil;
    LMove := nil;
    LFixture.Free;
  end;
end;

{ The final common fence must preserve the accepted router after dirty input,
  cancellation or a later validator publishes selection into the real view. }
procedure CommonRefusalCases;
var
  LFixture: TBoardFixture;
  LPrevious: INyxCancellationScope;
  LBinding: INyxKanbanBinding;
  LMutation: TPreparedSelection;
  LParticipant: INyxPreparedPublication;
  LView: INyxCollectionView;
  LIndex: Integer;
  LRejected: Boolean;
begin
  for LIndex := 0 to 2 do
  begin
    LFixture := TBoardFixture.Create;
    try
      LFixture.UseCommon;
      LView := LFixture.Bindings.ViewFor(LFixture.FirstBoard.Lane0ItemsList.ID);
      LPrevious := NyxEventGeneration(LFixture.Events);
      LFixture.PrepareCommon;
      LBinding := LFixture.First;
      case LIndex of
        0:
          begin
            LView.Select(Card);
          end;
        1:
          begin
            LFixture.EventPlan.Generation.Cancel;
          end;
        2:
          begin
            LMutation := TPreparedSelection.Create;
            LParticipant := LMutation;
            LMutation.View := LView;
          end;
      end;
      LRejected := False;
      try

        if LParticipant <> nil then
        begin
          PublishNyxGroup([LFixture.Mounts[0], LFixture.Mounts[1],
            LFixture.EventPlan, LParticipant]);
        end
        else
        begin
          PublishNyxGroup([LFixture.Mounts[0], LFixture.Mounts[1], LFixture.EventPlan]);
        end;
      except
        on ENyxKanban do
        begin
          LRejected := True;
        end;
        on ENyxCoordinator do
        begin
          LRejected := True;
        end;
        on ENyxSchedule do
        begin
          LRejected := True;
        end;
      end;
      Check(LRejected and not LPrevious.Cancelled and not LBinding.Connected,
        'common dirty/cancelled/later-mutated preparation never installs its controller');
      Check((LView.Store.Snapshot.Revision = 0) and
        (LFixture.FirstBoard.Node.Part(NyxPart('lane-1/move')).EnabledPolicyCount = 0),
        'refused common preparation releases candidate availability without changing cards');
    finally
      LParticipant := nil;
      LBinding := nil;
      LView := nil;
      LFixture.Free;
    end;
  end;
end;

{ Public typed Disconnect permits manual attachment on the very same nodes and
  live event generation; later retirement cannot detach that replacement. }
procedure CommonDisconnectCase;
var
  LFixture: TBoardFixture;
  LOld: INyxKanbanBinding;
  LMove: INyxKanbanMove;
  LGeneration: INyxCancellationScope;
  LButton: INyxButton;
  LStore: INyxCollection;
  LBefore: TNyxText;
  LRejected: Boolean;
begin
  LFixture := TBoardFixture.Create;
  try
    LFixture.UseCommon;
    LBefore := TNyxCodec.Encode(LFixture.Document);
    LFixture.AcceptCommon;
    LGeneration := NyxEventGeneration(LFixture.Events);
    LOld := LFixture.First;
    LButton := LOld.MoveButton(ActiveLane);
    LStore := LFixture.Store;
    LMove := LOld.CaptureMove(Card, ReadyLane, ActiveLane, LStore.Snapshot.Revision);
    LOld.Disconnect;
    LOld.Disconnect;
    Check(not LOld.Connected and not LGeneration.Cancelled and
      (LButton.Node.EnabledPolicyCount = 0) and LFixture.Second.Connected,
      'typed common Disconnect detaches its policies immediately ' +
      'without retiring sibling or router');
    LRejected := False;
    try
      LMove.Commit;
    except
      on ENyxKanbanCancelled do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and not LMove.Committed and (LStore.Snapshot.Revision = 0),
      'a captured command remains revoked after its typed owner explicitly disconnects');
    LFixture.First := NewNyxKanbanBinding(LFixture.FirstBoard, LFixture.Options,
      LFixture.Bindings, LFixture.Events, LFixture.Scheduler, nil);
    Check(LFixture.First.Connected and not LGeneration.Cancelled and
      (LFixture.First.MoveButton(ActiveLane).Node = LButton.Node),
      'unchanged manual API reacquires the same real board and event generation');
    DisconnectNyxCoordinatorMounts(LFixture.Mounts);
    LMove := LFixture.First.CaptureMove(Card, ReadyLane, ActiveLane, LStore.Snapshot.Revision);
    LMove.Commit;
    Check(LMove.Committed and LFixture.First.Connected and not LGeneration.Cancelled and
      (LButton.Node.EnabledPolicyCount = 1) and
      LFixture.View(False, ActiveLane).Snapshot.Has(Card),
      'later old common cleanup cannot detach the new manual policy or stable-card authority');
    Check(TNyxCodec.Encode(LFixture.Document) = LBefore,
      'same-board reacquisition never writes authored definition or collection defaults');
  finally
    LMove := nil;
    LOld := nil;
    LFixture.Free;
  end;
end;

{ A successor has the same logical IDs but independent canonical nodes. Retained
  old typed facades survive as model memory, never as authority over that successor. }
procedure CommonReplacementCase;
var
  LFixture: TBoardFixture;
  LRoot: TNyxNode;
  LOldRoot: TNyxNode;
  LBindings: INyxCollectionBindings;
  LOldMounts: TNyxCoordinatorMounts;
  LMounts: TNyxCoordinatorMounts;
  LIDs: TNyxCoordinatorMountIDs;
  LPreparation: INyxEventViewPreparation;
  LView: INyxPreparedEventView;
  LPrevious: INyxCancellationScope;
  LControl: INyxControl;
  LBoard: INyxKanbanBoard;
  LOld: INyxKanbanBinding;
  LBase: INyxCoordinatorBinding;
  LItems: INyxList;
  LRejected: Boolean;
begin
  LRoot := nil;
  LOldRoot := nil;
  LFixture := TBoardFixture.Create;
  try
    LFixture.UseCommon;
    LFixture.AcceptCommon;
    LOld := LFixture.First;
    LItems := LOld.Items(ReadyLane);
    LPrevious := NyxEventGeneration(LFixture.Events);
    LRoot := RealizeNyxView(LFixture.Document, LFixture.Document.Pages[0]);
    LBindings := LFixture.Bindings.Recompose(LRoot);
    LControl := RetainNyxControl(LRoot.Find(LFixture.FirstBoard.ID));
    Check(Supports(LControl, INyxKanbanBoard, LBoard),
      'replacement first board preserves its public specialized interface');
    SetNyxKanbanIntent(LBoard, TNyxKanbanIntent.Configured(LFixture.Options));
    LControl := RetainNyxControl(LRoot.Find(LFixture.SecondBoard.ID));
    SetNyxKanbanIntent(LControl, TNyxKanbanIntent.Configured(LFixture.Options));
    Supports(LFixture.Events, INyxEventViewPreparation, LPreparation);
    LView := LPreparation.PrepareView;
    PrepareNyxCoordinatorMounts(LRoot, LBindings, LView, LFixture.Scheduler,
      LFixture.Registry.Snapshot, LFixture.RefreshTarget, LMounts, LIDs);
    PublishNyxGroup([LView, LMounts[0], LMounts[1]]);
    LOldMounts := LFixture.Mounts;
    LFixture.Mounts := LMounts;
    LMounts := nil;
    LFixture.IDs := LIDs;
    LFixture.EventPlan := LView;
    LOldRoot := LFixture.Runtime;
    LFixture.Runtime := LRoot;
    LRoot := nil;
    LFixture.Bindings := LBindings;
    LFixture.FirstBoard := LBoard;
    Check(Supports(LControl, INyxKanbanBoard, LFixture.SecondBoard),
      'replacement second board preserves its public specialized interface');
    LBase := FindNyxCoordinatorMount(LFixture.Mounts, LFixture.IDs, LBoard.ID);
    Supports(LBase, INyxKanbanBinding, LFixture.First);
    LBase := FindNyxCoordinatorMount(LFixture.Mounts, LFixture.IDs, LControl.ID);
    Supports(LBase, INyxKanbanBinding, LFixture.Second);
    DisconnectNyxCoordinatorMounts(LOldMounts);
    LOldRoot.Free;
    LOldRoot := nil;
    Check(LPrevious.Cancelled and not LOld.Connected and LFixture.First.Connected and
      (LItems.ID = LFixture.First.Items(ReadyLane).ID) and
      (LItems.Node <> LFixture.First.Items(ReadyLane).Node),
      'same-ID replacement preserves canonical facade distinction and retires old commands');
    LRejected := False;
    try
      LOld.Items(ReadyLane);
    except
      on ENyxKanbanCancelled do
      begin
        LRejected := True;
      end;
    end;
    LItems.Configure.ReadOnly(True).Done;
    Check(LRejected and
      (LFixture.First.Items(ReadyLane).Node.Prop(
        NyxAttributeName(atReadOnly), 'false') = 'false') and
      (LItems.Node.Prop(NyxAttributeName(atReadOnly), 'false') = 'true'),
      'retained old facade configuration cannot mutate the accepted successor');
  finally
    DisconnectNyxCoordinatorMounts(LMounts);
    DisconnectNyxCoordinatorMounts(LOldMounts);
    LView := nil;
    LBase := nil;
    LOld := nil;
    LOldRoot.Free;
    LRoot.Free;
    LFixture.Free;
  end;
end;

procedure CommonDisabledCase;
var
  LFixture: TBoardFixture;
  LBase: INyxCoordinatorBinding;
  LTyped: INyxKanbanBinding;
begin
  LFixture := TBoardFixture.Create;
  try
    LFixture.UseCommon;
    SetNyxKanbanIntent(LFixture.FirstBoard, TNyxKanbanIntent.Disabled);
    LFixture.AcceptCommon;
    LBase := FindNyxCoordinatorMount(LFixture.Mounts, LFixture.IDs, LFixture.FirstBoard.ID);
    Check((LFixture.First = nil) and LBase.Connected and
      not Supports(LBase, INyxKanbanBinding, LTyped) and LFixture.Second.Connected and
      (LFixture.FirstBoard.Node.Part(NyxPart('lane-1/move')).Prop(
        NyxAttributeName(atEnabled), 'true') = 'false'),
      'common disabled masking exposes only a base owner and preserves other configured boards');
    LBase.Disconnect;
    Check(not LBase.Connected and
      (LFixture.FirstBoard.Node.Part(NyxPart('lane-1/move')).EnabledPolicyCount = 0),
      'disabled base Disconnect releases its exact availability policy too');
  finally
    LBase := nil;
    LFixture.Free;
  end;
  LFixture := TBoardFixture.Create;
  try
    LFixture.UseCommon;
    InheritNyxKanbanIntent(LFixture.FirstBoard);
    InheritNyxKanbanIntent(LFixture.SecondBoard);
    LFixture.AcceptCommon;
    Check((Length(LFixture.Mounts) = 0) and (LFixture.First = nil) and
      (LFixture.Second = nil),
      'absent effective intent retains the original static board behavior');
  finally
    LFixture.Free;
  end;
end;

begin
  try
    OptionCases;
    IntentCases;
    ModelCases;
    RefusalCases;
    ValidatorCase(ikCancelMove);
    ValidatorCase(ikDisconnect);
    ValidatorCase(ikGeneration);
    NotificationCase;
    LifetimeCases;
    InputCases;
    PolicyCases;
    PreparedActivationCase;
    PreparedRefusalCases;
    PreparedDisabledCase;
    PreparedLifetimeCase;
    PreparedSourceActivationCase;
    CommonCanonicalCase;
    CommonRefusalCases;
    CommonDisconnectCase;
    CommonReplacementCase;
    CommonDisabledCase;
    PolicyCases(True);
    {$ifdef PAS2JS}
    document.body.setAttribute('data-kanban', 'passed');
    document.body.setAttribute('data-kanban-checks', IntToStr(GChecks));
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' Kanban contract checks';
    {$else}
    WriteLn('PASS ', GChecks, ' Kanban contract checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-kanban', 'failed');
      document.body.setAttribute('data-kanban-error', LException.Message);
      document.body.textContent := 'FAIL ' + LException.Message;
      {$else}
      WriteLn('FAIL ', LException.Message, ' at check count ', GChecks);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
