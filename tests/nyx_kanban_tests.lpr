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
  nyx.collections, nyx.collections.view, nyx.collections.view.types,
  nyx.collections.query, nyx.collections.bindings, nyx.collections.selection,
  nyx.composition, nyx.events, nyx.scheduler, nyx.behavior, nyx.gestures,
  nyx.codec, nyx.source
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
    constructor Create(const AScheduler: INyxScheduler = nil);
    destructor Destroy; override;
    function View(ASecond: Boolean; const ALane: TNyxKanbanLaneRef): INyxCollectionView;
    function Store: INyxCollection;
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

var
  GChecks: Integer;
  GReleasedMove: INyxKanbanMove;

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

procedure PolicyCases;
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
    LFixture.First.Disconnect;
    LFixture.First := nil;
    LStream := LFixture.Events.On(NyxControlEvents(LButton.ID, niRuntime), ntClick).
      Policy(neUIQueue);
    LProbe.Fixture := LFixture;
    LToken := LStream.Subscribe(LProbeOwner);
    LFixture.First := NewNyxKanbanBinding(LFixture.FirstBoard, LFixture.Options,
      LFixture.Bindings, LFixture.Events, LScheduler, nil);
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

begin
  try
    OptionCases;
    ModelCases;
    RefusalCases;
    ValidatorCase(ikCancelMove);
    ValidatorCase(ikDisconnect);
    ValidatorCase(ikGeneration);
    NotificationCase;
    LifetimeCases;
    InputCases;
    PolicyCases;
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
