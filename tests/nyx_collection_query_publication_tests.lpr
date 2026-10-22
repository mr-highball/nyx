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
program nyx_collection_query_publication_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils,
  nyx.text,
  nyx.types,
  nyx.data,
  nyx.state,
  nyx.publication,
  nyx.collections,
  nyx.collections.query,
  nyx.collections.query.publication,
  nyx.collections.view,
  nyx.collections.view.types,
  nyx.collections.selection,
  nyx.collections.paging
  {$ifdef PAS2JS}, Web{$endif};

type
  { One public model fixture owns a real store, paged table view, hierarchical
    peer and scalar state. Observers borrow this object only until disconnect. }
  TFixture = class
  public
    Store: INyxCollection;
    Left: INyxCollectionView;
    Right: INyxCollectionView;
    LeftQueries: INyxCollectionQueryPublication;
    RightQueries: INyxCollectionQueryPublication;
    Pages: INyxCollectionPages;
    Tree: INyxTreeHierarchy;
    State: TNyxState;
    LeftToken: INyxCollectionViewSubscription;
    RightToken: INyxCollectionViewSubscription;
    StateToken: TNyxStateSubscription;
    LeftCalls: Integer;
    RightCalls: Integer;
    StateCalls: Integer;
    ExpectedLeft: TNyxText;
    ExpectedRight: TNyxText;
    ExpectedState: TNyxText;
    ExpectedLeftCount: Integer;
    ExpectedRightCount: Integer;
    Coherent: Boolean;
    ObserveCoherence: Boolean;
    FailLeft: Boolean;
    TestReentry: Boolean;
    ReentryRefused: Boolean;
    constructor Create;
    destructor Destroy; override;
    procedure AttachObservers;
    procedure Expect(const ALeft, ARight: TNyxCollectionQuery;
      const AState: TNyxText; ALeftCount, ARightCount: Integer);
    procedure Observe;
    procedure LeftChanged(const AView: INyxCollectionView;
      const AChanges: INyxCollectionChanges);
    procedure RightChanged(const AView: INyxCollectionView;
      const AChanges: INyxCollectionChanges);
    procedure StateChanged(AState: TNyxState; AChanges: TNyxStateChanges);
  end;

  { A later ordinary participant can refuse validation or revoke another owned
    final fence. It never invokes another participant's phase methods. }
  TGate = class(TInterfacedObject, INyxPreparedPublication,
    INyxPublicationReadiness)
  public
    Allowed: Boolean;
    RefuseValidation: Boolean;
    RevokeOnValidate: TGate;
    Validations: Integer;
    Installs: Integer;
    Notifications: Integer;
    Retirements: Integer;
    Retired: Boolean;
    constructor Create;
    procedure Validate;
    procedure CheckReady;
    procedure Install;
    procedure Notify;
    procedure Retire;
  end;

  { An unsupported public capture cannot forge a built-in view's private proof.
    The test reads this object only while its interface owner remains retained. }
  TForeignCapture = class(TInterfacedObject, INyxCollectionQueryCapture)
  public
    Reads: Integer;
    function GetQuery: TNyxCollectionQuery;
  end;

  { A borrowed lifetime counter outlives its source wrapper. It observes real
    interface release without assuming a store's private reference count. }
  TSourceLifetime = class
  public
    Destroyed: Integer;
  end;

  { An alternate source may observe a public Busy read. The receiver remains
    borrowed and the owning test disconnects it before releasing that receiver. }
  TBusyObservation = procedure of object;

  { A legacy source implements only the original public collection contract.
    All data and events delegate to an existing store, never another evaluator. }
  TLegacySource = class(TInterfacedObject, INyxCollection)
  protected
    FInner: INyxCollection;
    FLifetime: TSourceLifetime;
  public
    constructor Create(ALifetime: TSourceLifetime);
    destructor Destroy; override;
    function Snapshot: INyxCollectionSnapshot;
    function Append(const AItem: TNyxCollectionItem): INyxCollection;
    function Insert(AIndex: Integer; const AItem: TNyxCollectionItem): INyxCollection;
    function Remove(const ARef: TNyxItemRef): INyxCollection;
    function Move(const ARef: TNyxItemRef; AIndex: Integer): INyxCollection;
    function Update(const AItem: TNyxCollectionItem): INyxCollection;
    function Replace(const AItem: TNyxCollectionItem): INyxCollection;
    procedure Apply(const AEdits: array of TNyxCollectionEdit;
      AExpectedRevision: Integer = -1);
    procedure Assign(const ASource: INyxCollectionSnapshot;
      AExpectedRevision: Integer = -1);
    function Clone: INyxCollection;
    function Subscribe(AObserver: TNyxCollectionObserver;
      AValidator: TNyxCollectionValidator = nil): INyxCollectionSubscription;
  end;

  { The same transparent source opts into real prepared subscriptions. Its
    lifetime proves what captures and preparations retain on both dialects. }
  TAtomicSource = class(TLegacySource, INyxAtomicCollection)
  private
    FAtomic: INyxAtomicCollection;
  public
    BusyObservation: TBusyObservation;
    constructor Create(ALifetime: TSourceLifetime);
    function PrepareAssign(const ASource: INyxCollectionSnapshot;
      AExpectedRevision: Integer = -1): INyxPreparedPublication;
    function SubscribePrepared(AObserver: TNyxCollectionObserver;
      AValidator, AInstallation: TNyxCollectionValidator;
      ARetirement: TNyxCollectionRetirement): INyxCollectionSubscription;
    function GetBusy: Boolean;
  end;

  { This retained receiver changes a caller-owned query value or creates one
    nested public preparation during a one-shot alternate source Busy getter. }
  TBusyProbe = class
  public
    Commands: INyxCollectionQueryPublication;
    Capture: INyxCollectionQueryCapture;
    Proposal: TNyxCollectionQuery;
    Nested: INyxPreparedPublication;
    PrepareNested: Boolean;
    Calls: Integer;
    procedure ObserveBusy;
  end;

  { Public alternative predicates normalize through ToData while a query VALUE
    is built. The callback belongs to that earlier boundary, not PrepareQuery. }
  TPredicateProbe = class(TInterfacedObject, INyxCollectionPredicate)
  private
    FInner: INyxCollectionPredicate;
  public
    View: INyxCollectionView;
    ChangeQuery: Boolean;
    ReenterSelection: Boolean;
    Calls: Integer;
    constructor Create;
    function GetKind: TNyxQueryPredicateKind;
    function GetFieldName: TNyxText;
    function GetFieldKind: TNyxStateKind;
    function GetComparison: TNyxQueryComparison;
    function GetTextComparison: TNyxQueryTextComparison;
    function GetExpected: TNyxStateValue;
    function GetChildCount: Integer;
    function Child(AIndex: Integer): INyxCollectionPredicate;
    function GetNodeCount: Integer;
    function GetDepth: Integer;
    function AndAlso(const AOther: INyxCollectionPredicate): INyxCollectionPredicate;
    function OrElse(const AOther: INyxCollectionPredicate): INyxCollectionPredicate;
    function Negated: INyxCollectionPredicate;
    procedure Validate(const ASchema: TNyxCollectionSchema);
    function Matches(const AItem: TNyxCollectionItem): Boolean;
    function ToData: TNyxDataValue;
  end;

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Query publication: ' + AReason);
  end;
  Inc(GChecks);
end;

function Key: TNyxCollectionRef;
begin
  Result := NyxCollection('people');
end;

function Item(const AName: TNyxText): TNyxItemRef;
begin
  Result := NyxItem(Key, AName);
end;

function Schema: TNyxCollectionSchema;
begin
  Result := NyxCollectionSchema.Text(NyxTextField('name'), '')
    .Integer(NyxIntegerField('score'), 0)
    .Boolean(NyxBooleanField('active'), False)
    .Text(NyxTextField('parent'), '');
end;

function Person(const AID, AName: TNyxText; AScore: Integer;
  AActive: Boolean; const AParent: TNyxText): TNyxCollectionItem;
begin
  Result := NyxCollectionItem(Item(AID))
    .WithValue(NyxTextField('name'), AName)
    .WithValue(NyxIntegerField('score'), AScore)
    .WithValue(NyxBooleanField('active'), AActive)
    .WithValue(NyxTextField('parent'), AParent);
end;

function NewStore: INyxCollection;
begin
  Result := NewNyxCollection(Key, Schema, [
    Person('one', 'Ada', 4, True, ''),
    Person('two', 'Bea', 1, False, 'one'),
    Person('three', 'Cy', 3, True, 'one'),
    Person('four', 'Dee', 2, False, '')]);
end;

function TableSpec: TNyxCollectionViewSpec;
begin
  Result := NyxCollectionView(Key)
    .Column(NyxTextField('name'), 'Name', cmEditable)
    .Column(NyxIntegerField('score'), 'Score', cmEditable)
    .Selection(nsmMultiple)
    .Paged(NyxCollectionPaging.PageSize(2).InitialPage(2));
end;

function LeftPolicy: TNyxCollectionQuery;
begin
  Result := NyxCollectionQuery
    .Where(NyxWhere(NyxBooleanField('active')).EqualTo(True))
    .OrderBy(NyxIntegerField('score'), nsdDescending);
end;

function RightPolicy: TNyxCollectionQuery;
begin
  Result := NyxCollectionQuery
    .Where(NyxWhere(NyxIntegerField('score')).AtLeast(2))
    .OrderBy(NyxIntegerField('score'), nsdDescending);
end;

function SameQuery(const ALeft, ARight: TNyxCollectionQuery): Boolean;
begin
  Result := ALeft.ToData.ToJSON = ARight.ToData.ToJSON;
end;

procedure RefuseQuery(const ACommands: INyxCollectionQueryPublication;
  const ACapture: INyxCollectionQueryCapture;
  const AQuery: TNyxCollectionQuery; const AReason: TNyxText);
var
  LStage: INyxPreparedPublication;
  LRefused: Boolean;
begin
  LRefused := False;
  try
    try
      LStage := ACommands.PrepareQuery(ACapture, AQuery);
      LStage.Validate;
    except
      on ENyxCollection do
      begin
        LRefused := True;
      end;
    end;
  finally

    if LStage <> nil then
    begin
      LStage.Retire;
    end;
  end;
  Check(LRefused, AReason);
end;

constructor TFixture.Create;
begin
  inherited Create;
  Store := NewStore;
  Left := NewNyxCollectionView(Store, TableSpec, cpTable);
  Right := NewNyxCollectionView(Store, NyxCollectionView(Key)
    .Column(NyxTextField('name'), 'Name', cmEditable)
    .Parent(NyxTextField('parent')).Selection(nsmMultiple), cpTree);
  LeftQueries := NyxCollectionQueryPublication(Left);
  RightQueries := NyxCollectionQueryPublication(Right);
  Pages := NyxCollectionPages(Left);
  Tree := NyxTreeHierarchy(Right);
  Tree.SetExpanded(Item('one'), True);
  Left.SetSelection([Item('one'), Item('four')], Item('four'), Item('one'));
  Right.Select(Item('two'));
  State := TNyxState.Create;
  State.SetValue(NyxTextState('phase'), 'before');
  Coherent := True;
  ReentryRefused := True;
end;

destructor TFixture.Destroy;
begin

  if LeftToken <> nil then
  begin
    LeftToken.Disconnect;
  end;

  if RightToken <> nil then
  begin
    RightToken.Disconnect;
  end;
  LeftToken := nil;
  RightToken := nil;
  StateToken.Free;
  State.Free;
  LeftQueries := nil;
  RightQueries := nil;
  Pages := nil;
  Tree := nil;
  Left := nil;
  Right := nil;
  Store := nil;
  inherited Destroy;
end;

procedure TFixture.AttachObservers;
begin
  LeftToken := Left.Subscribe(LeftChanged);
  RightToken := Right.Subscribe(RightChanged);
  StateToken := State.Subscribe(StateChanged);
end;

procedure TFixture.Expect(const ALeft, ARight: TNyxCollectionQuery;
  const AState: TNyxText; ALeftCount, ARightCount: Integer);
begin
  ExpectedLeft := ALeft.ToData.ToJSON;
  ExpectedRight := ARight.ToData.ToJSON;
  ExpectedState := AState;
  ExpectedLeftCount := ALeftCount;
  ExpectedRightCount := ARightCount;
  ObserveCoherence := True;
end;

procedure TFixture.Observe;
var
  LRefused: Boolean;
begin

  if ObserveCoherence then
  begin
    Coherent := Coherent
      and (Left.QueryPolicy.ToData.ToJSON = ExpectedLeft)
      and (Right.QueryPolicy.ToData.ToJSON = ExpectedRight)
      and (State.GetValue(NyxTextState('phase')) = ExpectedState)
      and (Left.Snapshot.Count = ExpectedLeftCount)
      and (Right.Snapshot.Count = ExpectedRightCount);
  end;

  if TestReentry then
  begin
    LRefused := False;
    try
      Right.ClearSelection;
    except
      on ENyxCollection do
      begin
        LRefused := True;
      end;
    end;
    ReentryRefused := ReentryRefused and LRefused;
  end;
end;

procedure TFixture.LeftChanged(const AView: INyxCollectionView;
  const AChanges: INyxCollectionChanges);
begin
  Inc(LeftCalls);
  Coherent := Coherent and (AView = Left) and (AChanges = nil);
  Observe;

  if FailLeft then
  begin
    raise ENyxCollection.Create('Expected left observer failure');
  end;
end;

procedure TFixture.RightChanged(const AView: INyxCollectionView;
  const AChanges: INyxCollectionChanges);
begin
  Inc(RightCalls);
  Coherent := Coherent and (AView = Right) and (AChanges = nil);
  Observe;
end;

procedure TFixture.StateChanged(AState: TNyxState; AChanges: TNyxStateChanges);
begin
  Inc(StateCalls);
  Coherent := Coherent and (AState = State) and (AChanges.Count = 1);
  Observe;
end;

constructor TGate.Create;
begin
  inherited Create;
  Allowed := True;
end;

procedure TGate.Validate;
begin
  Inc(Validations);

  if RefuseValidation then
  begin
    raise ENyxCollection.Create('Expected later validator refusal');
  end;

  if RevokeOnValidate <> nil then
  begin
    RevokeOnValidate.Allowed := False;
  end;
end;

procedure TGate.CheckReady;
begin

  if not Allowed or Retired then
  begin
    raise ENyxCollection.Create('Expected later readiness revocation');
  end;
end;

procedure TGate.Install;
begin
  Inc(Installs);
end;

procedure TGate.Notify;
begin
  Inc(Notifications);
end;

procedure TGate.Retire;
begin

  if Retired then
  begin
    Exit;
  end;
  Retired := True;
  Inc(Retirements);
end;

function TForeignCapture.GetQuery: TNyxCollectionQuery;
begin
  Result := NyxCollectionQuery;
  Inc(Reads);
  raise ENyxCollection.Create('A foreign capture query must never be read');
end;

constructor TLegacySource.Create(ALifetime: TSourceLifetime);
begin
  inherited Create;
  FLifetime := ALifetime;
  FInner := NewStore;
end;

destructor TLegacySource.Destroy;
begin
  FInner := nil;

  if FLifetime <> nil then
  begin
    Inc(FLifetime.Destroyed);
  end;
  inherited Destroy;
end;

function TLegacySource.Snapshot: INyxCollectionSnapshot;
begin
  Result := FInner.Snapshot;
end;

function TLegacySource.Append(const AItem: TNyxCollectionItem): INyxCollection;
begin
  Result := FInner.Append(AItem);
end;

function TLegacySource.Insert(AIndex: Integer;
  const AItem: TNyxCollectionItem): INyxCollection;
begin
  Result := FInner.Insert(AIndex, AItem);
end;

function TLegacySource.Remove(const ARef: TNyxItemRef): INyxCollection;
begin
  Result := FInner.Remove(ARef);
end;

function TLegacySource.Move(const ARef: TNyxItemRef;
  AIndex: Integer): INyxCollection;
begin
  Result := FInner.Move(ARef, AIndex);
end;

function TLegacySource.Update(const AItem: TNyxCollectionItem): INyxCollection;
begin
  Result := FInner.Update(AItem);
end;

function TLegacySource.Replace(const AItem: TNyxCollectionItem): INyxCollection;
begin
  Result := FInner.Replace(AItem);
end;

procedure TLegacySource.Apply(const AEdits: array of TNyxCollectionEdit;
  AExpectedRevision: Integer);
begin
  FInner.Apply(AEdits, AExpectedRevision);
end;

procedure TLegacySource.Assign(const ASource: INyxCollectionSnapshot;
  AExpectedRevision: Integer);
begin
  FInner.Assign(ASource, AExpectedRevision);
end;

function TLegacySource.Clone: INyxCollection;
begin
  Result := FInner.Clone;
end;

function TLegacySource.Subscribe(AObserver: TNyxCollectionObserver;
  AValidator: TNyxCollectionValidator): INyxCollectionSubscription;
begin
  Result := FInner.Subscribe(AObserver, AValidator);
end;

constructor TAtomicSource.Create(ALifetime: TSourceLifetime);
var
  LAtomic: INyxAtomicCollection;
begin
  inherited Create(ALifetime);
  Check(Supports(FInner, INyxAtomicCollection, LAtomic),
    'tracked source delegates a real prepared store');
  FAtomic := LAtomic;
end;

function TAtomicSource.PrepareAssign(const ASource: INyxCollectionSnapshot;
  AExpectedRevision: Integer): INyxPreparedPublication;
begin
  Result := FAtomic.PrepareAssign(ASource, AExpectedRevision);
end;

function TAtomicSource.SubscribePrepared(AObserver: TNyxCollectionObserver;
  AValidator, AInstallation: TNyxCollectionValidator;
  ARetirement: TNyxCollectionRetirement): INyxCollectionSubscription;
begin
  Result := FAtomic.SubscribePrepared(AObserver, AValidator, AInstallation,
    ARetirement);
end;

function TAtomicSource.GetBusy: Boolean;
var
  LObservation: TBusyObservation;
begin
  { Clear the borrowed callback before invoking it: nested public view commands
    still read the actual delegated Busy state without recursive test callbacks. }
  LObservation := BusyObservation;
  BusyObservation := nil;

  if Assigned(LObservation) then
  begin
    LObservation;
  end;
  Result := FAtomic.Busy;
end;

procedure TBusyProbe.ObserveBusy;
begin
  Inc(Calls);

  if PrepareNested then
  begin
    Nested := Commands.PrepareQuery(Capture, RightPolicy);
  end
  else
  begin
    Proposal := RightPolicy;
  end;
end;

constructor TPredicateProbe.Create;
begin
  inherited Create;
  FInner := NyxWhere(NyxBooleanField('active')).EqualTo(True);
end;

function TPredicateProbe.GetKind: TNyxQueryPredicateKind;
begin
  Result := FInner.Kind;
end;

function TPredicateProbe.GetFieldName: TNyxText;
begin
  Result := FInner.FieldName;
end;

function TPredicateProbe.GetFieldKind: TNyxStateKind;
begin
  Result := FInner.FieldKind;
end;

function TPredicateProbe.GetComparison: TNyxQueryComparison;
begin
  Result := FInner.Comparison;
end;

function TPredicateProbe.GetTextComparison: TNyxQueryTextComparison;
begin
  Result := FInner.TextComparison;
end;

function TPredicateProbe.GetExpected: TNyxStateValue;
begin
  Result := FInner.Expected;
end;

function TPredicateProbe.GetChildCount: Integer;
begin
  Result := FInner.ChildCount;
end;

function TPredicateProbe.Child(AIndex: Integer): INyxCollectionPredicate;
begin
  Result := FInner.Child(AIndex);
end;

function TPredicateProbe.GetNodeCount: Integer;
begin
  Result := FInner.NodeCount;
end;

function TPredicateProbe.GetDepth: Integer;
begin
  Result := FInner.Depth;
end;

function TPredicateProbe.AndAlso(
  const AOther: INyxCollectionPredicate): INyxCollectionPredicate;
begin
  Result := FInner.AndAlso(AOther);
end;

function TPredicateProbe.OrElse(
  const AOther: INyxCollectionPredicate): INyxCollectionPredicate;
begin
  Result := FInner.OrElse(AOther);
end;

function TPredicateProbe.Negated: INyxCollectionPredicate;
begin
  Result := FInner.Negated;
end;

procedure TPredicateProbe.Validate(const ASchema: TNyxCollectionSchema);
begin
  FInner.Validate(ASchema);
end;

function TPredicateProbe.Matches(const AItem: TNyxCollectionItem): Boolean;
begin
  Result := FInner.Matches(AItem);
end;

function TPredicateProbe.ToData: TNyxDataValue;
begin
  Inc(Calls);

  if ChangeQuery then
  begin
    View.ConfigureQuery(RightPolicy);
  end;

  if ReenterSelection then
  begin
    View.ClearSelection;
  end;
  Result := FInner.ToData;
end;

{ All three observer families read the complete new model. The source remains
  unchanged, paging clamps after filtering and tree indexes are already ready. }
procedure GroupedModels;
var
  LFixture: TFixture;
  LLeft: INyxPreparedPublication;
  LRight: INyxPreparedPublication;
  LState: INyxPreparedPublication;
  LSource: INyxCollectionSnapshot;
  LOldLeft: INyxCollectionSnapshot;
  LOldRight: INyxCollectionSnapshot;
  LVisible: TNyxItemRefs;
begin
  LFixture := TFixture.Create;
  try
    LFixture.AttachObservers;
    LFixture.Expect(LeftPolicy, RightPolicy, 'after', 2, 3);
    LFixture.TestReentry := True;
    LSource := LFixture.Store.Snapshot;
    LOldLeft := LFixture.Left.Snapshot;
    LOldRight := LFixture.Right.Snapshot;
    LLeft := LFixture.LeftQueries.PrepareQuery(
      LFixture.LeftQueries.CaptureQuery, LeftPolicy);
    LRight := LFixture.RightQueries.PrepareQuery(
      LFixture.RightQueries.CaptureQuery, RightPolicy);
    LState := LFixture.State.PrepareApply([
      NyxStateValue(NyxTextState('phase'), 'after')]);
    Check((LFixture.Left.Snapshot = LOldLeft)
      and (LFixture.Right.Snapshot = LOldRight),
      'preparation leaves both accepted result snapshots exact');
    Check((LFixture.Pages.Window.Page = 2)
      and (LFixture.Pages.Window.Total = 4)
      and (LFixture.State.GetValue(NyxTextState('phase')) = 'before'),
      'preparation publishes neither page geometry nor scalar state');
    Check((LFixture.LeftCalls = 0) and (LFixture.RightCalls = 0)
      and (LFixture.StateCalls = 0), 'all prepared models remain silent');
    PublishNyxGroup([LState, LLeft, LRight]);
    Check(LFixture.Coherent, 'every observer sees both new queries and new state');
    Check((LFixture.LeftCalls = 1) and (LFixture.RightCalls = 1)
      and (LFixture.StateCalls = 1), 'each changed model notifies once');
    Check(LFixture.ReentryRefused,
      'all observers refuse cross-view mutation through group retirement');
    Check(SameQuery(LFixture.Left.QueryPolicy, LeftPolicy)
      and SameQuery(LFixture.Right.QueryPolicy, RightPolicy),
      'both effective typed queries commit');
    Check((LFixture.Left.Snapshot.ItemAt(0).Ref.ID = 'one')
      and (LFixture.Left.Snapshot.ItemAt(1).Ref.ID = 'three'),
      'real filtering and descending numeric ordering produce actual rows');
    Check((LFixture.Pages.Window.Page = 1)
      and (LFixture.Pages.Window.Pages = 1)
      and (LFixture.Pages.Window.Total = 2),
      'query publication installs clamped paging with the full result count');
    Check((LFixture.Left.Selection.Count = 2)
      and LFixture.Left.Selection.Contains(Item('four')),
      'a selected source identity remains owned while filtered out');
    Check(LFixture.Left.Snapshot.Has(LFixture.Left.Selection.Focus),
      'the accepted table cursor resolves to visible membership');
    LVisible := LFixture.Tree.VisibleItems;
    Check((Length(LVisible) = 3) and (LFixture.Right.ParentIndex(1) = 0)
      and LFixture.Tree.IsExpanded(Item('one')),
      'prepared tree hierarchy and disclosure are usable before notification');
    Check(LFixture.Right.Selection.Contains(Item('two')),
      'the independent tree also retains a filtered-out selected identity');
    Check((LFixture.Store.Snapshot = LSource)
      and (LFixture.Store.Snapshot.Revision = 0)
      and (LFixture.Store.Snapshot.Count = 4),
      'query and state publication never edits source rows or defaults');
    LLeft.Retire;
    LRight.Retire;
    LState.Retire;
    Check((LFixture.Left.Snapshot.Count = 2)
      and (LFixture.State.GetValue(NyxTextState('phase')) = 'after'),
      'idempotent retirement cannot roll committed models back');
  finally

    if LLeft <> nil then
    begin
      LLeft.Retire;
    end;

    if LRight <> nil then
    begin
      LRight.Retire;
    end;

    if LState <> nil then
    begin
      LState.Retire;
    end;
    LLeft := nil;
    LRight := nil;
    LState := nil;
    LFixture.Free;
  end;
end;

{ Refusal after earlier validations and a separately revoked final fence must
  leave all real models untouched. No participant calls another one's Retire. }
procedure GroupRefusals;
var
  LFixture: TFixture;
  LLeft: INyxPreparedPublication;
  LRight: INyxPreparedPublication;
  LState: INyxPreparedPublication;
  LGate: INyxPreparedPublication;
  LLater: INyxPreparedPublication;
  LGateObject: TGate;
  LLaterObject: TGate;
  LOldLeft: INyxCollectionSnapshot;
  LOldRight: INyxCollectionSnapshot;
  LOldSelection: INyxCollectionSelection;
  LRefused: Boolean;
begin
  LFixture := TFixture.Create;
  try
    LFixture.AttachObservers;
    LOldLeft := LFixture.Left.Snapshot;
    LOldRight := LFixture.Right.Snapshot;
    LOldSelection := LFixture.Left.Selection;
    LLeft := LFixture.LeftQueries.PrepareQuery(
      LFixture.LeftQueries.CaptureQuery, LeftPolicy);
    LRight := LFixture.RightQueries.PrepareQuery(
      LFixture.RightQueries.CaptureQuery, RightPolicy);
    LState := LFixture.State.PrepareApply([
      NyxStateValue(NyxTextState('phase'), 'after')]);
    LGateObject := TGate.Create;
    LGate := LGateObject;
    LGateObject.RefuseValidation := True;
    LRefused := False;
    try
      PublishNyxGroup([LLeft, LRight, LState, LGate]);
    except
      on E: ENyxCollection do
      begin
        LRefused := E.Message = 'Expected later validator refusal';
      end;
    end;
    Check(LRefused, 'a later validator refuses the complete model group');
    Check((LFixture.Left.Snapshot = LOldLeft)
      and (LFixture.Right.Snapshot = LOldRight)
      and (LFixture.Left.Selection = LOldSelection)
      and (LFixture.Pages.Window.Page = 2),
      'late refusal preserves exact results, selection and page');
    Check((LFixture.State.Revision = 1)
      and (LFixture.State.GetValue(NyxTextState('phase')) = 'before')
      and (LFixture.LeftCalls = 0) and (LFixture.RightCalls = 0)
      and (LFixture.StateCalls = 0),
      'late refusal changes no scalar revision and notifies nobody');
    Check((LGateObject.Validations = 1) and (LGateObject.Installs = 0)
      and (LGateObject.Notifications = 0) and (LGateObject.Retirements = 1),
      'a refused participant is retired without installation');
    LLeft := nil;
    LRight := nil;
    LState := nil;
    LGate := nil;

    LLeft := LFixture.LeftQueries.PrepareQuery(
      LFixture.LeftQueries.CaptureQuery, LeftPolicy);
    LRight := LFixture.RightQueries.PrepareQuery(
      LFixture.RightQueries.CaptureQuery, RightPolicy);
    LState := LFixture.State.PrepareApply([
      NyxStateValue(NyxTextState('phase'), 'after')]);
    LGateObject := TGate.Create;
    LGate := LGateObject;
    LLaterObject := TGate.Create;
    LLater := LLaterObject;
    LLaterObject.RevokeOnValidate := LGateObject;
    LRefused := False;
    try
      PublishNyxGroup([LLeft, LState, LGate, LRight, LLater]);
    except
      on E: ENyxCollection do
      begin
        LRefused := E.Message = 'Expected later readiness revocation';
      end;
    end;
    Check(LRefused, 'final readiness runs after every group validator');
    Check((LGateObject.Validations = 1) and (LLaterObject.Validations = 1)
      and (LGateObject.Installs = 0) and (LLaterObject.Installs = 0),
      'later flag revocation prevents every installation');
    Check((LFixture.Left.Snapshot = LOldLeft)
      and (LFixture.Right.Snapshot = LOldRight)
      and (LFixture.State.GetValue(NyxTextState('phase')) = 'before')
      and (LFixture.LeftCalls + LFixture.RightCalls + LFixture.StateCalls = 0),
      'readiness refusal preserves the full accepted baseline silently');
    LLeft := nil;
    LRight := nil;
    LState := nil;
    LGate := nil;
    LLater := nil;
    LFixture.Left.ConfigureQuery(LeftPolicy);
    Check(LFixture.Left.Snapshot.Count = 2,
      'all reservations release after group refusal');
  finally

    if LLeft <> nil then
    begin
      LLeft.Retire;
    end;

    if LRight <> nil then
    begin
      LRight.Retire;
    end;

    if LState <> nil then
    begin
      LState.Retire;
    end;
    LLater := nil;
    LGate := nil;
    LState := nil;
    LRight := nil;
    LLeft := nil;
    LFixture.Free;
  end;
end;

{ These are real public commands while a query owns the view reservation.
  Reads and another view's query remain usable; no mixed row/query group exists. }
procedure Reservations;
var
  LFixture: TFixture;
  LStage: INyxPreparedPublication;
  LRows: INyxPreparedPublication;
  LOther: INyxPreparedPublication;
  LAtomic: INyxAtomicCollection;
  LChanged: INyxCollection;
  LCapture: INyxCollectionQueryCapture;
  LToken: INyxCollectionViewSubscription;
  LBefore: INyxCollectionSnapshot;
  LRefused: Boolean;
begin
  LFixture := TFixture.Create;
  try
    LFixture.AttachObservers;
    LCapture := LFixture.LeftQueries.CaptureQuery;
    LBefore := LFixture.Store.Snapshot;
    LStage := LFixture.LeftQueries.PrepareQuery(LCapture, LeftPolicy);
    RefuseQuery(LFixture.LeftQueries, LCapture, RightPolicy,
      'a second preparation cannot reserve the same view');
    LRefused := False;
    try
      LFixture.Left.ConfigureQuery(RightPolicy);
    except
      on ENyxCollection do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'legacy ConfigureQuery cannot bypass reservation');
    LRefused := False;
    try
      LFixture.Left.ClearSelection;
    except
      on ENyxCollection do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'selection cannot mutate a reserved query view');
    LRefused := False;
    try
      LFixture.Pages.GoToPage(1);
    except
      on ENyxCollection do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'paging cannot mutate a reserved query view');
    LRefused := False;
    try
      LToken := LFixture.Left.Subscribe(LFixture.LeftChanged);
    except
      on ENyxCollection do
      begin
        LRefused := True;
      end;
    end;

    if LToken <> nil then
    begin
      LToken.Disconnect;
      LToken := nil;
    end;
    Check(LRefused, 'new subscriptions cannot bypass reservation');
    LRefused := False;
    try
      LFixture.Store.Update(Person('one', 'Changed', 4, True, ''));
    except
      on ENyxCollection do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'source-row mutation refuses before its store publishes');
    Check((LFixture.Store.Snapshot = LBefore)
      and (LFixture.Right.Snapshot.Revision = 0)
      and (LFixture.Store.Snapshot.Item(Item('one')).GetValue(
        NyxTextField('name')) = 'Ada')
      and (LFixture.LeftCalls = 0) and (LFixture.RightCalls = 0),
      'row refusal preserves source and the independent peer silently');
    LOther := LFixture.RightQueries.PrepareQuery(
      LFixture.RightQueries.CaptureQuery, RightPolicy);
    Check(LOther <> nil, 'a distinct view over the same source can prepare');
    LOther.Retire;
    LOther := nil;
    LStage.Retire;
    LStage.Retire;
    LStage := nil;
    Check(Supports(LFixture.Store, INyxAtomicCollection, LAtomic),
      'source exposes its real atomic publication capability');
    LChanged := LFixture.Store.Clone;
    LChanged.Update(Person('one', 'Changed', 4, True, ''));
    LRows := LAtomic.PrepareAssign(LChanged.Snapshot,
      LFixture.Store.Snapshot.Revision);
    RefuseQuery(LFixture.LeftQueries, LCapture, LeftPolicy,
      'row reservation before query preparation refuses reverse ordering');
    LRows.Retire;
    LRows := nil;
    Check((LFixture.Store.Snapshot = LBefore)
      and (LFixture.Pages.Window.Page = 2),
      'abandoned row/query preparations leave the accepted source and page');
    LToken := LFixture.Left.Subscribe(LFixture.LeftChanged);
    Check(LToken.Connected, 'subscription works after reservation retirement');
    LToken.Disconnect;
    LToken := nil;
    LFixture.Pages.GoToPage(1);
    Check(LFixture.Pages.Window.Page = 1,
      'paging works again after reservation retirement');
  finally

    if LToken <> nil then
    begin
      LToken.Disconnect;
    end;

    if LOther <> nil then
    begin
      LOther.Retire;
    end;

    if LRows <> nil then
    begin
      LRows.Retire;
    end;

    if LStage <> nil then
    begin
      LStage.Retire;
    end;
    LToken := nil;
    LOther := nil;
    LRows := nil;
    LStage := nil;
    LAtomic := nil;
    LChanged := nil;
    LFixture.Free;
  end;
end;

{ Captures prove exact query intent, not value equality across distinct views.
  Legacy equal assignments and equal preparations retain all no-op semantics. }
procedure CaptureCases;
var
  LFixture: TFixture;
  LCapture: INyxCollectionQueryCapture;
  LForeign: INyxCollectionQueryCapture;
  LForeignObject: TForeignCapture;
  LCopy: TNyxCollectionQuery;
  LStage: INyxPreparedPublication;
  LState: INyxPreparedPublication;
  LSnapshot: INyxCollectionSnapshot;
  LSelection: INyxCollectionSelection;
  LBad: TNyxCollectionQuery;
begin
  LFixture := TFixture.Create;
  try
    LCapture := LFixture.LeftQueries.CaptureQuery;
    Check(not LCapture.Query.Defined, 'capture reads the accepted empty policy');
    LCopy := LCapture.Query.OrderBy(NyxTextField('name'));
    Check(LCopy.Defined and not LCapture.Query.Defined
      and not LFixture.Left.QueryPolicy.Defined,
      'fluent capture copies cannot mutate the capture or live policy');
    RefuseQuery(LFixture.LeftQueries, LFixture.RightQueries.CaptureQuery,
      LeftPolicy, 'same-valued capture from another real view refuses');
    RefuseQuery(LFixture.LeftQueries, nil, LeftPolicy, 'nil capture refuses');
    LForeignObject := TForeignCapture.Create;
    LForeign := LForeignObject;
    RefuseQuery(LFixture.LeftQueries, LForeign, LeftPolicy,
      'unsupported capture refuses instead of trusting public query text');
    Check(LForeignObject.Reads = 0, 'foreign capture GetQuery is not invoked');
    LForeign := nil;
    LFixture.Left.ConfigureQuery(LeftPolicy);
    RefuseQuery(LFixture.LeftQueries, LCapture, RightPolicy,
      'an actual accepted query change stales the former capture');
    LFixture.Left.ConfigureQuery(NyxCollectionQuery);
    RefuseQuery(LFixture.LeftQueries, LCapture, LeftPolicy,
      'A-to-B-to-A cannot revive old query authority');

    LCapture := LFixture.LeftQueries.CaptureQuery;
    LSnapshot := LFixture.Left.Snapshot;
    LSelection := LFixture.Left.Selection;
    LFixture.AttachObservers;
    LFixture.Left.ConfigureQuery(NyxCollectionQuery);
    Check((LFixture.Left.Snapshot = LSnapshot)
      and (LFixture.Left.Selection = LSelection) and (LFixture.LeftCalls = 0),
      'equal legacy ConfigureQuery is a true silent snapshot no-op');
    LStage := LFixture.LeftQueries.PrepareQuery(LCapture, NyxCollectionQuery);
    LState := LFixture.State.PrepareApply([
      NyxStateValue(NyxTextState('phase'), 'after')]);
    LFixture.Expect(NyxCollectionQuery, NyxCollectionQuery, 'after', 2, 4);
    PublishNyxGroup([LStage, LState]);
    Check((LFixture.Left.Snapshot = LSnapshot)
      and (LFixture.Left.Selection = LSelection) and (LFixture.LeftCalls = 0),
      'equal prepared query also retains exact snapshot and selection');
    Check(LFixture.Coherent and (LFixture.StateCalls = 1)
      and (LFixture.State.GetValue(NyxTextState('phase')) = 'after'),
      'an equal query can guard a real scalar publication without query notify');
    LStage := nil;
    LState := nil;
    LStage := LFixture.LeftQueries.PrepareQuery(LCapture, LeftPolicy);
    Check(LStage <> nil, 'equal assignments do not invalidate captured intent');
    LStage.Retire;
    LStage := nil;

    LBad := NyxCollectionQuery.Where(
      NyxWhere(NyxTextField('missing')).EqualTo('not a schema field'));
    RefuseQuery(LFixture.LeftQueries, LCapture, LBad,
      'wrong schema field refuses the complete query candidate');
    Check((LFixture.Left.Snapshot = LSnapshot) and (LFixture.LeftCalls = 0),
      'failed query normalization/admission preserves accepted result');
    LStage := LFixture.LeftQueries.PrepareQuery(LCapture, LeftPolicy);
    Check(LStage <> nil, 'failed preparation releases its reservation');
    LStage.Retire;
    LStage := nil;
  finally

    if LStage <> nil then
    begin
      LStage.Retire;
    end;

    if LState <> nil then
    begin
      LState.Retire;
    end;
    LForeign := nil;
    LStage := nil;
    LState := nil;
    LFixture.Free;
  end;
end;

{ Row edits before PrepareQuery are legitimate: preparation uses current rows,
  rather than treating a query-intent capture as an old source snapshot hold. }
procedure SourceBeforePrepare;
var
  LFixture: TFixture;
  LCapture: INyxCollectionQueryCapture;
  LStage: INyxPreparedPublication;
  LSource: INyxCollectionSnapshot;
begin
  LFixture := TFixture.Create;
  try
    LCapture := LFixture.LeftQueries.CaptureQuery;
    LFixture.Store.Update(Person('two', 'Bea updated', 1, True, 'one'));
    LSource := LFixture.Store.Snapshot;
    Check(LSource.Revision = 1, 'real source update precedes preparation');
    LStage := LFixture.LeftQueries.PrepareQuery(LCapture, LeftPolicy);
    PublishNyxGroup([LStage]);
    Check((LFixture.Pages.Window.Total = 3)
      and (LFixture.Pages.Window.Page = 2)
      and (LFixture.Left.Snapshot.Count = 1)
      and (LFixture.Left.Snapshot.ItemAt(0).Ref.ID = 'two'),
      'preparation evaluates the updated source before selecting the page');
    Check((LFixture.Left.Snapshot.Item(Item('two')).GetValue(
      NyxTextField('name')) = 'Bea updated')
      and (LFixture.Store.Snapshot = LSource),
      'query installation retains current source data without editing it');
    Check(not LCapture.Query.Defined,
      'later source/query changes never mutate copied capture data');
  finally

    if LStage <> nil then
    begin
      LStage.Retire;
    end;
    LStage := nil;
    LFixture.Free;
  end;
end;

{ A predicate interface is observed while Where normalizes its descriptor.
  PrepareQuery later consumes only the already owned immutable query value. }
procedure NormalizationBoundary;
var
  LFixture: TFixture;
  LCapture: INyxCollectionQueryCapture;
  LStage: INyxPreparedPublication;
  LPredicate: INyxCollectionPredicate;
  LProbe: TPredicateProbe;
  LQuery: TNyxCollectionQuery;
  LRefused: Boolean;
begin
  LFixture := TFixture.Create;
  try
    LCapture := LFixture.LeftQueries.CaptureQuery;
    LProbe := TPredicateProbe.Create;
    LPredicate := LProbe;
    LProbe.View := LFixture.Left;
    LProbe.ChangeQuery := True;
    LQuery := NyxCollectionQuery.Where(LPredicate);
    Check((LProbe.Calls > 0) and SameQuery(LFixture.Left.QueryPolicy, RightPolicy),
      'alternative predicate callout occurs at the real Where normalization');
    RefuseQuery(LFixture.LeftQueries, LCapture, LQuery,
      'normalization changing query intent makes an older capture stale');
    LProbe.ChangeQuery := False;
    LProbe.View := nil;
    LPredicate := nil;

    LCapture := LFixture.LeftQueries.CaptureQuery;
    LStage := LFixture.LeftQueries.PrepareQuery(LCapture, LeftPolicy);
    LProbe := TPredicateProbe.Create;
    LPredicate := LProbe;
    LProbe.View := LFixture.Left;
    LProbe.ReenterSelection := True;
    LRefused := False;
    try
      LQuery := NyxCollectionQuery.Where(LPredicate);
    except
      on ENyxCollection do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LProbe.Calls > 0),
      'real custom normalization cannot reenter a reserved query view');
    Check(SameQuery(LFixture.Left.QueryPolicy, RightPolicy)
      and LFixture.Left.Selection.Contains(Item('four')),
      'normalization refusal retains accepted query and selection');
    LProbe.View := nil;
    LPredicate := nil;
    LStage.Retire;
    LStage := nil;
  finally

    if LStage <> nil then
    begin
      LStage.Retire;
    end;
    LPredicate := nil;
    LStage := nil;
    LFixture.Free;
  end;
end;

{ A capture retains no source. A preparation deliberately retains its real
  source/view while caller slots disappear, then releases them without cycles. }
procedure SourceAndViewLifetimes;
var
  LLifetime: TSourceLifetime;
  LStore: INyxCollection;
  LView: INyxCollectionView;
  LCommands: INyxCollectionQueryPublication;
  LCapture: INyxCollectionQueryCapture;
  LStage: INyxPreparedPublication;
begin
  LLifetime := TSourceLifetime.Create;
  try
    LStore := TAtomicSource.Create(LLifetime);
    LView := NewNyxCollectionView(LStore, NyxCollectionView(Key)
      .Column(NyxTextField('name'), 'Name'), cpList);
    LCommands := NyxCollectionQueryPublication(LView);
    LCapture := LCommands.CaptureQuery;
    LCommands := nil;
    LView := nil;
    LStore := nil;
    Check(LLifetime.Destroyed = 1,
      'an opaque capture does not retain its former source or view');
    Check(not LCapture.Query.Defined,
      'copied capture query remains readable after source retirement');
    LStore := TAtomicSource.Create(LLifetime);
    LView := NewNyxCollectionView(LStore, NyxCollectionView(Key)
      .Column(NyxTextField('name'), 'Name'), cpList);
    LCommands := NyxCollectionQueryPublication(LView);
    RefuseQuery(LCommands, LCapture, LeftPolicy,
      'retained capture cannot authorize a newly created same-valued view');
    LCapture := LCommands.CaptureQuery;
    LStage := LCommands.PrepareQuery(LCapture, LeftPolicy);
    LCommands := nil;
    LView := nil;
    LStore := nil;
    Check(LLifetime.Destroyed = 1,
      'a preparation retains the current source through caller slot release');
    LStage.Validate;
    LStage.Retire;
    LStage.Retire;
    LStage := nil;
    Check(LLifetime.Destroyed = 2,
      'abandoning a validated preparation releases source/view ownership');
    Check(not LCapture.Query.Defined,
      'abandonment never publishes the proposed query into its capture');

    LStore := TAtomicSource.Create(LLifetime);
    LView := NewNyxCollectionView(LStore, NyxCollectionView(Key)
      .Column(NyxTextField('name'), 'Name'), cpList);
    LCommands := NyxCollectionQueryPublication(LView);
    LStage := LCommands.PrepareQuery(LCommands.CaptureQuery, LeftPolicy);
    LCommands := nil;
    LView := nil;
    LStore := nil;
    LStage := nil;
    Check(LLifetime.Destroyed = 3,
      'destruction also retires an unvalidated abandoned preparation');
  finally

    if LStage <> nil then
    begin
      LStage.Retire;
    end;
    LStage := nil;
    LCapture := nil;
    LCommands := nil;
    LView := nil;
    LStore := nil;
    LLifetime.Free;
  end;
end;

{ A public alternate Busy getter is a real callout before reservation. The
  entry proposal is copied first, and a nested reservation cannot be replaced. }
procedure BusySourceBoundary;
var
  LLifetime: TSourceLifetime;
  LSourceObject: TAtomicSource;
  LStore: INyxCollection;
  LView: INyxCollectionView;
  LCommands: INyxCollectionQueryPublication;
  LProbe: TBusyProbe;
  LStage: INyxPreparedPublication;
  LBefore: INyxCollectionSnapshot;
  LRefused: Boolean;
begin
  LLifetime := TSourceLifetime.Create;
  LSourceObject := nil;
  LProbe := TBusyProbe.Create;
  try
    LSourceObject := TAtomicSource.Create(LLifetime);
    LStore := LSourceObject;
    LView := NewNyxCollectionView(LStore, NyxCollectionView(Key)
      .Column(NyxTextField('name'), 'Name'), cpList);
    LCommands := NyxCollectionQueryPublication(LView);
    LProbe.Commands := LCommands;
    LProbe.Capture := LCommands.CaptureQuery;
    LProbe.Proposal := LeftPolicy;
    LBefore := LView.Snapshot;
    LSourceObject.BusyObservation := LProbe.ObserveBusy;
    LStage := LCommands.PrepareQuery(LProbe.Capture, LProbe.Proposal);
    Check((LProbe.Calls = 1) and SameQuery(LProbe.Proposal, RightPolicy),
      'alternate Busy getter actually changes the caller proposal once');
    Check((LView.Snapshot = LBefore) and not LView.QueryPolicy.Defined,
      'Busy observation and preparation publish no accepted query result');
    PublishNyxGroup([LStage]);
    Check(SameQuery(LView.QueryPolicy, LeftPolicy)
      and (LView.Snapshot.Count = 2),
      'entry proposal survives the Busy callout changing its caller variable');
    LStage := nil;
    LView.ConfigureQuery(NyxCollectionQuery);
    LBefore := LView.Snapshot;
    LProbe.Capture := LCommands.CaptureQuery;
    LProbe.Proposal := LeftPolicy;
    LProbe.PrepareNested := True;
    LSourceObject.BusyObservation := LProbe.ObserveBusy;
    LRefused := False;
    try
      LStage := LCommands.PrepareQuery(LProbe.Capture, LProbe.Proposal);
    except
      on ENyxCollection do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LProbe.Calls = 2) and (LProbe.Nested <> nil),
      'outer preparation refuses after Busy retains a nested preparation');
    Check((LView.Snapshot = LBefore) and not LView.QueryPolicy.Defined,
      'nested preparation and outer refusal preserve the accepted baseline');
    RefuseQuery(LCommands, LProbe.Capture, LeftPolicy,
      'the nested exact-view reservation remains authoritative after refusal');
    PublishNyxGroup([LProbe.Nested]);
    Check(SameQuery(LView.QueryPolicy, RightPolicy)
      and (LView.Snapshot.Count = 3),
      'nested preparation still publishes its own actual result after refusal');
  finally

    if LSourceObject <> nil then
    begin
      LSourceObject.BusyObservation := nil;
    end;

    if LStage <> nil then
    begin
      LStage.Retire;
    end;

    if LProbe.Nested <> nil then
    begin
      LProbe.Nested.Retire;
    end;
    LStage := nil;
    LProbe.Nested := nil;
    LProbe.Capture := nil;
    LProbe.Commands := nil;
    LProbe.Free;
    LCommands := nil;
    LView := nil;
    LStore := nil;
    LLifetime.Free;
  end;
end;

{ Legacy non-atomic sources keep ConfigureQuery but cannot promise coordinated
  state/query admission, because their row writes have no prepared veto phase. }
procedure LegacySourceCompatibility;
var
  LLifetime: TSourceLifetime;
  LStore: INyxCollection;
  LView: INyxCollectionView;
  LCommands: INyxCollectionQueryPublication;
  LSnapshot: INyxCollectionSnapshot;
begin
  LLifetime := TSourceLifetime.Create;
  try
    LStore := TLegacySource.Create(LLifetime);
    LView := NewNyxCollectionView(LStore, NyxCollectionView(Key)
      .Column(NyxTextField('name'), 'Name'), cpList);
    LView.ConfigureQuery(LeftPolicy);
    Check((LView.Snapshot.Count = 2)
      and (LView.Snapshot.ItemAt(0).Ref.ID = 'one'),
      'legacy non-atomic source retains ordinary query behavior');
    LSnapshot := LView.Snapshot;
    LView.ConfigureQuery(LeftPolicy);
    Check(LView.Snapshot = LSnapshot,
      'legacy non-atomic equal policy keeps exact no-op result');
    LCommands := NyxCollectionQueryPublication(LView);
    RefuseQuery(LCommands, LCommands.CaptureQuery, RightPolicy,
      'prepared query explicitly refuses an unsupported non-atomic source');
    Check((LView.Snapshot = LSnapshot) and SameQuery(LView.QueryPolicy, LeftPolicy)
      and (LStore.Snapshot.Revision = 0),
      'atomic-capability refusal changes neither legacy query nor source');
    LView.ConfigureQuery(RightPolicy);
    Check(LView.Snapshot.Count = 3,
      'legacy queries remain usable after prepared-capability refusal');
  finally
    LCommands := nil;
    LView := nil;
    LStore := nil;
    LLifetime.Free;
  end;
end;

{ Notification failure cannot roll back an installed query/state group or skip
  the peer observers. Group retirement must still allow the next real command. }
procedure CommittedFailure;
var
  LFixture: TFixture;
  LLeft: INyxPreparedPublication;
  LRight: INyxPreparedPublication;
  LState: INyxPreparedPublication;
  LGate: INyxPreparedPublication;
  LGateObject: TGate;
  LCommittedFailure: Boolean;
begin
  LFixture := TFixture.Create;
  try
    LFixture.AttachObservers;
    LFixture.Expect(LeftPolicy, RightPolicy, 'after', 2, 3);
    LFixture.FailLeft := True;
    LFixture.TestReentry := True;
    LLeft := LFixture.LeftQueries.PrepareQuery(
      LFixture.LeftQueries.CaptureQuery, LeftPolicy);
    LRight := LFixture.RightQueries.PrepareQuery(
      LFixture.RightQueries.CaptureQuery, RightPolicy);
    LState := LFixture.State.PrepareApply([
      NyxStateValue(NyxTextState('phase'), 'after')]);
    LGateObject := TGate.Create;
    LGate := LGateObject;
    LCommittedFailure := False;
    try
      PublishNyxGroup([LLeft, LState, LGate, LRight]);
    except
      on E: ENyxPublicationNotification do
      begin
        LCommittedFailure := Pos('Expected left observer failure', E.Message) > 0;
      end;
    end;
    Check(LCommittedFailure, 'observer exception is reported as committed failure');
    Check(LFixture.Coherent and (LFixture.LeftCalls = 1)
      and (LFixture.RightCalls = 1) and (LFixture.StateCalls = 1),
      'independent real query/state observers all run after publication');
    Check(LFixture.ReentryRefused,
      'notification reentry cannot change another installed group member');
    Check((LGateObject.Installs = 1) and (LGateObject.Notifications = 1)
      and (LGateObject.Retirements = 1),
      'later notifications and retirement are attempted despite earlier failure');
    Check(SameQuery(LFixture.Left.QueryPolicy, LeftPolicy)
      and SameQuery(LFixture.Right.QueryPolicy, RightPolicy)
      and (LFixture.State.GetValue(NyxTextState('phase')) = 'after'),
      'all models remain committed after notification failure');
    LLeft := nil;
    LRight := nil;
    LState := nil;
    LGate := nil;
    LFixture.FailLeft := False;
    LFixture.TestReentry := False;
    LFixture.Expect(NyxCollectionQuery, RightPolicy, 'after', 2, 3);
    LLeft := LFixture.LeftQueries.PrepareQuery(
      LFixture.LeftQueries.CaptureQuery, NyxCollectionQuery);
    PublishNyxGroup([LLeft]);
    Check(LFixture.Coherent and not LFixture.Left.QueryPolicy.Defined
      and (LFixture.LeftCalls = 2),
      'new capture and publication work after committed-failure retirement');
    Check((LFixture.Store.Snapshot.Revision = 0)
      and (LFixture.Store.Snapshot.Count = 4),
      'all failure and recovery paths preserve authored source rows');
  finally

    if LLeft <> nil then
    begin
      LLeft.Retire;
    end;

    if LRight <> nil then
    begin
      LRight.Retire;
    end;

    if LState <> nil then
    begin
      LState.Retire;
    end;
    LGate := nil;
    LState := nil;
    LRight := nil;
    LLeft := nil;
    LFixture.Free;
  end;
end;

begin
  try
    GroupedModels;
    GroupRefusals;
    Reservations;
    CaptureCases;
    SourceBeforePrepare;
    NormalizationBoundary;
    SourceAndViewLifetimes;
    BusySourceBoundary;
    LegacySourceCompatibility;
    CommittedFailure;
    {$ifdef PAS2JS}
    document.body.textContent := 'PASS ' + IntToStr(GChecks)
      + ' prepared collection query checks';
    document.body.setAttribute('data-query-publication', 'passed');
    document.body.setAttribute('data-query-publication-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' prepared collection query checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.textContent := 'FAIL ' + E.Message;
      document.body.setAttribute('data-query-publication', 'failed');
      document.body.setAttribute('data-query-publication-checks', IntToStr(GChecks));
      document.body.setAttribute('data-query-publication-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message, ' at check count ', GChecks);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
