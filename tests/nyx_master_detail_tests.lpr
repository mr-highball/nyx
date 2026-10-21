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

program nyx_master_detail_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.types, nyx.data, nyx.state, nyx.model, nyx.controls,
  nyx.master.detail, nyx.collections, nyx.collections.view,
  nyx.collections.view.types, nyx.collections.bindings, nyx.collections.selection,
  nyx.collections.query, nyx.composition, nyx.codec, nyx.events,
  nyx.scheduler, nyx.behavior, nyx.publication, nyx.coordinators, nyx.responsive,
  nyx.interaction.policy
  {$ifdef PAS2JS}, JS, Web{$else}, Classes{$endif};

type
  { Real reusable definitions, independent instance stores and ordinary views.
    The focused consumer has no renderer, alternate store or fabricated view. }
  TDetailFixture = class
  public
    Document: TNyxDocument;
    Runtime: TNyxNode;
    Bindings: INyxCollectionBindings;
    Options: TNyxMasterDetailOptions;
    Scheduler: INyxScheduler;
    Events: INyxEvents;
    First: INyxMasterDetail;
    Second: INyxMasterDetail;
    EventPlan: INyxPreparedEventView;
    FirstPlan: INyxPreparedMasterDetailBinding;
    SecondPlan: INyxPreparedMasterDetailBinding;
    Mounts: TNyxCoordinatorMounts;
    IDs: TNyxCoordinatorMountIDs;
    Registry: INyxCoordinatorRegistry;
    RefreshTarget: INyxCoordinatorRefreshTarget;
    constructor Create;
    destructor Destroy; override;
    function View(ASecond: Boolean = False): INyxCollectionView;
    procedure Prepare(ARefresh: TNyxMasterDetailRefresh = nil);
    procedure Activate(ARefresh: TNyxMasterDetailRefresh = nil);
    procedure PrepareAutomatic;
    procedure AcceptAutomatic;
    function Automatic(ASecond: Boolean = False): INyxMasterDetailBinding;
  end;

  { Ordinary callbacks retain event snapshots while another callback deliberately
    selects another row. They do not read a mutable current selection afterward. }
  TEditReceiver = class(TNyxEventCallback)
  public
    View: INyxCollectionView;
    Redirect: Boolean;
    Calls: Integer;
    Last: TNyxMasterDetailRecord;
    Scalar: TNyxDataValue;
    HasScalar: Boolean;
    ScalarKind: TNyxStateKind;
    ValueID: TNyxText;
    procedure Invoke(const AEvent: TNyxEventInfo;
      const AExecution: INyxExecution); override;
  end;

  { Later validation publishes a real view selection; the earlier preparation's
    final fence must catch it without repeating any external readiness calls. }
  TSelectionValidator = class(TInterfacedObject, INyxPreparedPublication)
  public
    View: INyxCollectionView;
    procedure Validate;
    procedure Install;
    procedure Notify;
    procedure Retire;
  end;
  { Closed mutations target facts already copied by the automatic factory. }
  TRoleMutation = (rmProjectionKind, rmCompound);
  { A later public group validator changes real candidate metadata only after
    prior participants have completed their outward admission. }
  TRoleValidator = class(TInterfacedObject, INyxPreparedPublication)
  public
    Fixture: TDetailFixture;
    Mutation: TRoleMutation;
    procedure Validate;
    procedure Install;
    procedure Notify;
    procedure Retire;
  end;

  { Borrowed last-call receiver can release the last prepared owner while a
    view notification still has its own local controller/port leases. }
  TRefreshReceiver = class
  public
    Calls: Integer;
    ReleasePlan: Boolean;
    procedure Refresh;
  end;
  { A managed final-host-style port counts real requests without borrowing a
    widget. The controller's guarded refresh remains the last outward call. }
  TCoordinatorRefresh = class(TInterfacedObject, INyxCoordinatorRefreshTarget)
  public
    Calls: Integer;
    procedure Request;
  end;
  { Public ancestor creator reserves one real descendant text role. Its simple
    presentation proves ownership admission, not a private claims-table mock. }
  TTextClaim = class(TInterfacedObject, INyxPreparedCoordinator,
    INyxPreparedPublication, INyxPublicationReadiness, INyxCoordinatorBinding)
  private
    FProjection: INyxCoordinatorProjection;
    FGeneration: INyxCancellationScope;
    FConnected: Boolean;
    FRevoked: Boolean;
    FValidated: Boolean;
    FRetired: Boolean;
  public
    constructor Create(const AProjection: INyxCoordinatorProjection;
      const AView: INyxPreparedEventView; const APath: TNyxPartRef);
    function GetBinding: INyxCoordinatorBinding;
    function GetConnected: Boolean;
    procedure Revoke;
    procedure Disconnect;
    procedure Validate;
    procedure CheckReady;
    procedure Install;
    procedure Notify;
    procedure Retire;
  end;
  { Typed fixture registration deliberately competes with MasterDetail's title
    using its equal initial value. No raw node is supplied to the factory. }
  TTextClaimFactory = class(TInterfacedObject, INyxCoordinatorFactory)
  public
    Path: TNyxPartRef;
    function Prepare(const AProjection: INyxCoordinatorProjection;
      const AIntent: TNyxDataValue; const AViews: INyxCollectionBindings;
      const AView: INyxPreparedEventView; const AScheduler: INyxScheduler;
      const ARefresh: INyxCoordinatorRefresh): INyxPreparedCoordinator;
  end;

var
  GChecks: Integer;
  GReleasedPlan: INyxPreparedMasterDetailBinding;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('MasterDetail: ' + AReason);
  end;
  Inc(GChecks);
end;

function Person(ASecond: Boolean = False): TNyxItemRef;
begin
  Result := NyxItem(NyxCollection('people'), 'one');

  if ASecond then
  begin
    Result := NyxItem(NyxCollection('people'), 'two');
  end;
end;

constructor TDetailFixture.Create;
var
  LDefinition: INyxMasterDetail;
  LPage: INyxPage;
  LInstance: INyxComponent;
  LSchema: TNyxCollectionSchema;
  LControl: INyxControl;
begin
  inherited Create;
  Document := TNyxDocument.Create;
  Options := NyxMasterDetailOptions(NyxTextField('full-name'), NyxTextField('summary'));
  LSchema := NyxCollectionSchema.Text(NyxTextField('short-name'), '').
    Text(NyxTextField('full-name'), '').Text(NyxTextField('summary'), '').
    Integer(NyxIntegerField('score'), 0).Boolean(NyxBooleanField('active'), False).
    Number(NyxNumberField('weight'), 0);
  Document.Collections.Define(NyxCollection('people'), LSchema, [
    NyxCollectionItem(Person).WithValue(NyxTextField('short-name'), 'Ada').
      WithValue(NyxTextField('full-name'), 'Ada Lovelace').
      WithValue(NyxTextField('summary'), 'A carefully written first record.').
      WithValue(NyxIntegerField('score'), 7).WithValue(NyxBooleanField('active'), True).
      WithValue(NyxNumberField('weight'), 1.25),
    NyxCollectionItem(Person(True)).WithValue(NyxTextField('short-name'), 'Grace').
      WithValue(NyxTextField('full-name'), 'Grace Hopper').
      WithValue(NyxTextField('summary'), 'A different second record.')]);
  LDefinition := NewNyxMasterDetail('directory');
  LDefinition.MasterList.Binds.Collection(NyxCollectionView(NyxCollection('people')).
    Column(NyxTextField('short-name'), 'People').Scoped(csInstance)).Done;
  SetNyxMasterDetailIntent(LDefinition, TNyxMasterDetailIntent.Configured(Options));
  Document.AddComponent(LDefinition);
  LPage := NewNyxPage('home');
  LInstance := NewNyxComponent('first-directory');
  LInstance.Reference := NyxComponent('directory');
  LPage.Add(LInstance);
  LInstance := NewNyxComponent('second-directory');
  LInstance.Reference := NyxComponent('directory');
  LPage.Add(LInstance);
  Document.AddPage(LPage);
  Runtime := RealizeNyxView(Document, LPage.Node);
  Bindings := NewNyxCollectionBindings(Runtime,
    NewNyxCollectionContext(Document.Collections));
  Scheduler := NewNyxScheduler;
  Events := NewNyxEvents(Scheduler);
  LControl := RetainNyxControl(Runtime.Find(NyxQualifiedID('first-directory', 'directory')));
  First := LControl as INyxMasterDetail;
  LControl := RetainNyxControl(Runtime.Find(NyxQualifiedID('second-directory', 'directory')));
  Second := LControl as INyxMasterDetail;
end;

destructor TDetailFixture.Destroy;
begin
  DisconnectNyxCoordinatorMounts(Mounts);

  if FirstPlan <> nil then
  begin
    FirstPlan.Disconnect;
  end;

  if SecondPlan <> nil then
  begin
    SecondPlan.Disconnect;
  end;
  FirstPlan := nil;
  SecondPlan := nil;

  if EventPlan <> nil then
  begin
    EventPlan.Retire;
  end;
  EventPlan := nil;

  if Events <> nil then
  begin
    Events.Close;
  end;
  First := nil;
  Second := nil;
  Events := nil;
  Registry := nil;
  RefreshTarget := nil;
  Bindings := nil;
  Runtime.Free;
  Document.Free;

  if Scheduler <> nil then
  begin
    Scheduler.Shutdown;
  end;
  Scheduler := nil;
  inherited Destroy;
end;

function TDetailFixture.View(ASecond: Boolean): INyxCollectionView;
begin
  Result := Bindings.ViewFor(First.MasterList.ID);

  if ASecond then
  begin
    Result := Bindings.ViewFor(Second.MasterList.ID);
  end;
end;

procedure TDetailFixture.Prepare(ARefresh: TNyxMasterDetailRefresh);
var
  LPreparation: INyxEventViewPreparation;
begin
  Supports(Events, INyxEventViewPreparation, LPreparation);
  EventPlan := LPreparation.PrepareView;
  FirstPlan := PrepareNyxMasterDetailBinding(First, Options, Bindings,
    EventPlan, Scheduler, ARefresh);
  SecondPlan := PrepareNyxMasterDetailBinding(Second, Options, Bindings,
    EventPlan, Scheduler, nil);
end;

procedure TDetailFixture.Activate(ARefresh: TNyxMasterDetailRefresh);
begin
  Prepare(ARefresh);
  PublishNyxGroup([EventPlan, FirstPlan, SecondPlan]);
end;

procedure TDetailFixture.PrepareAutomatic;
var
  LPreparation: INyxEventViewPreparation;
begin

  if Registry = nil then
  begin
    Registry := NewNyxCoordinatorRegistry;
    Registry.RegisterFactory(NyxCustomKind(NyxKindName(nkMasterDetail)),
      NyxExtension(NyxMasterDetailIntentKey), NewNyxMasterDetailCoordinatorFactory);
  end;

  if RefreshTarget = nil then
  begin
    RefreshTarget := TCoordinatorRefresh.Create;
  end;
  Supports(Events, INyxEventViewPreparation, LPreparation);
  EventPlan := LPreparation.PrepareView;
  PrepareNyxCoordinatorMounts(Runtime, Bindings, EventPlan, Scheduler,
    Registry.Snapshot, RefreshTarget, Mounts, IDs);
end;

procedure TDetailFixture.AcceptAutomatic;
var
  LParticipants: TNyxPreparedPublications;
  LIndex: Integer;
begin
  PrepareAutomatic;
  SetLength(LParticipants, Length(Mounts) + 1);
  LParticipants[0] := EventPlan;
  for LIndex := 0 to High(Mounts) do
  begin
    LParticipants[LIndex + 1] := Mounts[LIndex];
  end;
  PublishNyxGroup(LParticipants);
end;

function TDetailFixture.Automatic(ASecond: Boolean): INyxMasterDetailBinding;
var
  LID: TNyxText;
  LBinding: INyxCoordinatorBinding;
begin
  LID := First.ID;

  if ASecond then
  begin
    LID := Second.ID;
  end;
  LBinding := FindNyxCoordinatorMount(Mounts, IDs, LID);
  Supports(LBinding, INyxMasterDetailBinding, Result);
end;

procedure TEditReceiver.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
begin
  Check(TryNyxMasterDetailEdit(AEvent, Last), 'ordinary Edit carries its typed copied record');
  Scalar := AEvent.Value.Copy;
  HasScalar := AEvent.HasValue;
  ScalarKind := AEvent.ValueKind;
  ValueID := AEvent.ValueID;
  Inc(Calls);

  if Redirect then
  begin
    View.Select(Person(True));
  end;
end;

procedure TSelectionValidator.Validate;
begin
  View.Select(Person);
end;

procedure TSelectionValidator.Install;
begin
end;

procedure TSelectionValidator.Notify;
begin
end;

procedure TSelectionValidator.Retire;
begin
end;

procedure TRoleValidator.Validate;
begin
  case Mutation of
    rmProjectionKind:
      begin
        Fixture.First.DetailTitleHeading.Node.SetProp('projection-kind',
          NyxKindName(nkLabel));
      end;
    rmCompound:
      begin
        Fixture.First.Node.SetProp(NyxAttributeName(atCompound), 'false');
      end;
  end;
end;

procedure TRoleValidator.Install;
begin
end;

procedure TRoleValidator.Notify;
begin
end;

procedure TRoleValidator.Retire;
begin
end;

procedure TRefreshReceiver.Refresh;
begin
  Inc(Calls);

  if ReleasePlan then
  begin
    GReleasedPlan := nil;
  end;
end;

procedure TCoordinatorRefresh.Request;
begin
  Inc(Calls);
end;

constructor TTextClaim.Create(const AProjection: INyxCoordinatorProjection;
  const AView: INyxPreparedEventView; const APath: TNyxPartRef);
var
  LView: INyxPreparedEventView;
  LPath: TNyxPartRef;
  LRole: TNyxControlRef;
  LText: TNyxText;
begin
  inherited Create;
  FProjection := AProjection;
  LView := AView;
  LPath := APath;
  FGeneration := LView.Generation;
  LRole := FProjection.Part(LPath);
  LText := FProjection.AuthoredText(LRole);
  FProjection.Text(LRole, LText);
end;

function TTextClaim.GetBinding: INyxCoordinatorBinding;
begin
  Result := Self;
end;

function TTextClaim.GetConnected: Boolean;
begin
  Result := FConnected and not FRevoked and not FGeneration.Cancelled;
end;

procedure TTextClaim.Revoke;
begin
  FRevoked := True;
  FConnected := False;
end;

procedure TTextClaim.Disconnect;
begin
  Revoke;
  FProjection := nil;
end;

procedure TTextClaim.Validate;
begin
  FValidated := True;
  CheckReady;
end;

procedure TTextClaim.CheckReady;
begin

  if not FValidated or FRevoked or FRetired or FGeneration.Cancelled then
  begin
    raise ENyxCoordinatorCancelled.Create('The ancestor title claim is no longer ready');
  end;
end;

procedure TTextClaim.Install;
begin
  FConnected := True;
end;

procedure TTextClaim.Notify;
begin
end;

procedure TTextClaim.Retire;
begin
  FRetired := True;

  if not FConnected then
  begin
    Disconnect;
  end;
end;

function TTextClaimFactory.Prepare(const AProjection: INyxCoordinatorProjection;
  const AIntent: TNyxDataValue; const AViews: INyxCollectionBindings;
  const AView: INyxPreparedEventView; const AScheduler: INyxScheduler;
  const ARefresh: INyxCoordinatorRefresh): INyxPreparedCoordinator;
begin
  Result := TTextClaim.Create(AProjection, AView, Path);
end;

procedure IntentCases;
const
  CBad: array[0..5] of TNyxText = (
    'null', '{"version":2,"title":"name","description":"summary"}',
    '{"version":1.0,"title":"name","description":"summary"}',
    '{"version":1,"title":"name","description":true}',
    '{"version":1,"title":"name","description":"summary","extra":0}',
    '{"version":1,"title":"","description":"summary"}');
var
  LFixture: TDetailFixture;
  LOptions: TNyxMasterDetailOptions;
  LIntent: TNyxMasterDetailIntent;
  LControl: INyxComponent;
  LWrong: INyxColumn;
  LRejected: Boolean;
  LIndex: Integer;
  LBefore: TNyxText;
begin
  LFixture := TDetailFixture.Create;
  try
    LBefore := TNyxCodec.Encode(LFixture.Document);
    LOptions := LFixture.Options.Copy;
    Check((LOptions.TitleField.Name = 'full-name') and
      (LOptions.DescriptionField.Name = 'summary'), 'options retain distinct typed field names');
    LOptions := TNyxMasterDetailOptions.FromData(LOptions.ToData);
    Check(LOptions.ToData.ToJSON = LFixture.Options.ToData.ToJSON,
      'options round trip without adding another collection or query authority');
    for LIndex := 0 to High(CBad) do
    begin
      LRejected := False;
      try
        LOptions := TNyxMasterDetailOptions.FromData(TNyxDataValue.ParseJSON(CBad[LIndex]));
      except
        on Exception do
        begin
          LRejected := True;
        end;
      end;
      Check(LRejected, 'malformed options refuse their entire closed value');
    end;
    LIntent := TNyxMasterDetailIntent.FromData(
      TNyxMasterDetailIntent.Configured(LFixture.Options).ToData);
    Check((LIntent.Mode = nmiConfigured) and
      (LIntent.Options.DescriptionField.Name = 'summary'), 'configured intent copies its options');
    LControl := RetainNyxControl(LFixture.Document.Find('first-directory')) as INyxComponent;
    SetNyxMasterDetailIntent(LControl, TNyxMasterDetailIntent.Disabled);
    ValidateNyxMasterDetailIntent(LControl.Node);
    Check(GetNyxMasterDetailIntent(LControl.Node).Mode = nmiDisabled,
      'an explicit disabled reusable mask survives metadata-only admission');
    InheritNyxMasterDetailIntent(LControl);
    Check(not HasNyxMasterDetailIntent(LControl.Node) and
      (TNyxCodec.Encode(LFixture.Document) = LBefore),
      'inherit removes only the local key and restores the exact authored document');
    ValidateNyxMasterDetailOptions(LFixture.First.Node, LFixture.Options);
    Check(LFixture.First.MasterList.Node.CollectionView.ColumnAt(0).FieldName = 'short-name',
      'detail full-name mapping does not require replacing the displayed list column');
    LWrong := NewNyxColumn('wrong');
    SetNyxMasterDetailIntent(LWrong, LIntent);
    LRejected := False;
    try
      ValidateNyxMasterDetailIntent(LWrong.Node);
    except
      on ENyxMasterDetail do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'known intent on another concrete kind refuses');
    LRejected := False;
    try
      ValidateNyxMasterDetailFields(LFixture.View.Store.Snapshot.Schema,
        NyxMasterDetailOptions(NyxTextField('score'), NyxTextField('summary')));
    except
      on ENyxCollection do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'mapped text fields cannot silently coerce integer data');
    LFixture.First.MasterList.Node.SetCollectionView(
      LFixture.First.MasterList.Node.CollectionView.Selection(nsmMultiple));
    LRejected := False;
    try
      ValidateNyxMasterDetailOptions(LFixture.First.Node, LFixture.Options);
    except
      on ENyxMasterDetail do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'multiple selection does not choose an arbitrary detail record');
    Check(TNyxCodec.Encode(LFixture.Document) = LBefore,
      'runtime refusals never modify reusable defaults or the sibling definition');
  finally
    LFixture.Free;
  end;
end;

procedure SelectionCases;
var
  LFixture: TDetailFixture;
  LBefore: TNyxText;
  LRecord: TNyxMasterDetailRecord;
  LRead: TNyxMasterDetailRecord;
  LChanged: TNyxCollectionItem;
  LRejected: Boolean;
begin
  LFixture := TDetailFixture.Create;
  try
    LBefore := TNyxCodec.Encode(LFixture.Document);
    LFixture.Activate;
    Check(not LFixture.FirstPlan.Binding.Current.Defined and
      (LFixture.First.DetailTitleHeading.Text = 'Record details') and
      (LFixture.First.DetailEditButton.Node.Prop(NyxAttributeName(atEnabled)) = 'false'),
      'empty selection retains authored placeholders and masks only runtime Edit availability');
    LFixture.View.Select(Person);
    LRecord := LFixture.FirstPlan.Binding.CaptureEdit;
    Check(LRecord.Defined and (LRecord.Ref.ID = 'one') and (LRecord.DataRevision = 0) and
      (LRecord.Item.GetValue(NyxIntegerField('score')) = 7),
      'a selected visible record captures exact scoped identity, revision and typed values');
    Check((LFixture.First.DetailTitleHeading.Text = 'Ada Lovelace') and
      (LFixture.First.DetailDescriptionLabel.Text = 'A carefully written first record.') and
      (LFixture.First.DetailEditButton.Node.Prop(NyxAttributeName(atEnabled), 'true') = 'true'),
      'live selected-record values project into the actual typed detail parts');
    Check(not LFixture.SecondPlan.Binding.Current.Defined and
      (LFixture.Second.DetailTitleHeading.Text = 'Record details') and
      (LFixture.View.Store <> LFixture.View(True).Store),
      'another reusable instance owns independent selection and runtime data');
    LRead := TNyxMasterDetailRecord.FromData(LRecord.ToData);
    LChanged := LRead.Item.WithValue(NyxTextField('full-name'), 'Changed copy');
    Check((LChanged.GetValue(NyxTextField('full-name')) = 'Changed copy') and
      (LRecord.Item.GetValue(NyxTextField('full-name')) = 'Ada Lovelace') and
      LRead.Item.SameItem(LRecord.Item), 'copied record builders cannot mutate prior snapshots');
    LFixture.View.ConfigureQuery(NyxCollectionQuery.Where(
      NyxWhere(NyxTextField('short-name')).EqualTo('Grace')));
    Check((LFixture.View.Selection.Count = 1) and
      not LFixture.FirstPlan.Binding.Current.Defined and
      (LFixture.First.DetailTitleHeading.Text = 'Record details') and
      (LFixture.First.DetailEditButton.Node.Prop(NyxAttributeName(atEnabled)) = 'false'),
      'filter-hidden membership stays selected but cannot expose or Edit a hidden record');
    LRejected := False;
    try
      LRead := LFixture.FirstPlan.Binding.CaptureEdit;
    except
      on ENyxMasterDetail do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'a hidden old selection does not grant a fresh Edit snapshot');
    LFixture.View.ConfigureQuery(NyxCollectionQuery);
    Check(LFixture.FirstPlan.Binding.Current.Ref.ID = 'one',
      'showing the retained selected identity restores its detail without changing membership');
    LFixture.View.Store.Apply([NyxUpdate(NyxCollectionItem(Person).
      WithValue(NyxTextField('summary'), 'A current accepted update.'))]);
    Check((LFixture.First.DetailDescriptionLabel.Text = 'A current accepted update.') and
      (LFixture.FirstPlan.Binding.Current.DataRevision = 1) and
      (LRecord.Item.GetValue(NyxTextField('summary')) = 'A carefully written first record.'),
      'store updates refresh live detail while earlier copied records remain exact');
    LFixture.View.Store.Apply([NyxRemove(Person)]);
    Check((LFixture.View.Selection.Count = 0) and
      not LFixture.FirstPlan.Binding.Current.Defined and
      (LFixture.First.DetailTitleHeading.Text = 'Record details'),
      'actual removal clears detail and never substitutes the nearby keyboard cursor');
    LRejected := False;
    try
      LFixture.View.Select(NyxItem(NyxCollection('elsewhere'), 'two'));
    except
      on ENyxCollection do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and not LFixture.FirstPlan.Binding.Current.Defined,
      'the actual view refuses a foreign identity before presentation changes');
    Check((LFixture.View(True).Store.Snapshot.Revision = 0) and
      LFixture.View(True).Store.Snapshot.Has(Person) and
      (TNyxCodec.Encode(LFixture.Document) = LBefore),
      'data/detail interaction preserves sibling store, definition and authored defaults');
  finally
    LFixture.Free;
  end;
  Check((LRecord.Item.GetValue(NyxTextField('full-name')) = 'Ada Lovelace') and
    (LRecord.Ref.Collection.Name = 'people'), 'record data outlives every original owner');
end;

procedure EventCases;
var
  LFixture: TDetailFixture;
  LDispatch: TNyxDispatch;
  LEvent: TNyxEventInfo;
  LCopy: TNyxEventInfo;
  LRecord: TNyxMasterDetailRecord;
  LRead: TNyxMasterDetailRecord;
  LFirst: TEditReceiver;
  LSecond: TEditReceiver;
  LFirstLease: INyxEventCallback;
  LSecondLease: INyxEventCallback;
  LFirstToken: INyxEventSubscription;
  LSecondToken: INyxEventSubscription;
  LStream: INyxEventStream;
  LRejected: Boolean;
  LIndex: Integer;
  LData: TNyxDataValue;
begin
  LFixture := TDetailFixture.Create;
  try
    LFixture.Activate;
    LFixture.View.Select(Person);
    LDispatch := DispatchNyxBehavior(LFixture.First.DetailEditButton.Node, ntClick);
    Check((LDispatch.Info.OriginID = LFixture.First.DetailEditButton.ID) and
      (LDispatch.Info.SourceID = LFixture.First.ID),
      'actual compound behavior supplies the exact Edit origin/source identities');
    LEvent := LFixture.FirstPlan.Binding.EnrichEdit(LDispatch.Info);
    LCopy := LEvent.Copy;
    Check(TryNyxMasterDetailEdit(LCopy, LRecord) and (LRecord.Ref.ID = 'one') and
      not LDispatch.Info.HasDetails, 'enrichment copies rather than mutates its event input');
    LStream := LFixture.Events.On(NyxControlEvents(
      LFixture.First.DetailEditButton.ID, niRuntime), ntClick);
    LFirst := TEditReceiver.Create;
    LFirstLease := LFirst;
    LFirst.View := LFixture.View;
    LFirst.Redirect := True;
    LSecond := TEditReceiver.Create;
    LSecondLease := LSecond;
    LFirstToken := LStream.Subscribe(LFirstLease);
    LSecondToken := LStream.Subscribe(LSecondLease);
    LFixture.Events.Dispatch(LCopy, LFixture.First.DetailEditButton.Node.DesignID,
      LFixture.First.Node.DesignID);
    Check((LFirst.Calls = 1) and (LSecond.Calls = 1) and
      (LFirst.Last.Ref.ID = 'one') and (LSecond.Last.Ref.ID = 'one') and
      (LFixture.FirstPlan.Binding.Current.Ref.ID = 'two'),
      'later callbacks retain the original record after an earlier callback changes selection');
    Check((LStream.ExecutionPolicy = neSequential) and (LRecord.Ref.ID = 'one'),
      'product enrichment preserves caller policy and detached prior event data');
    LEvent := LDispatch.Info.Copy;
    LEvent.OriginID := 'unrelated';
    LCopy := LFixture.FirstPlan.Binding.EnrichEdit(LEvent);
    Check(not LCopy.HasDetails, 'unrelated physical events remain structurally unchanged');
    LEvent.HasDetails := True;
    LEvent.Details := NyxObject([NyxField('kind', NyxData(7))]);
    Check(not TryNyxMasterDetailEdit(LEvent, LRead) and not LRead.Defined,
      'an unrelated numeric kind marker is not a malformed MasterDetail payload');
    LEvent := LDispatch.Info.Copy;
    LEvent.HasDetails := True;
    LEvent.Details := NyxObject([NyxField('application', NyxData('owned'))]);
    LRejected := False;
    try
      LCopy := LFixture.FirstPlan.Binding.EnrichEdit(LEvent);
    except
      on ENyxMasterDetail do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LEvent.Details.Field('application').AsText = 'owned'),
      'owned Edit cannot overwrite a preexisting application structured payload');
    for LIndex := 0 to 2 do
    begin
      LData := LRecord.ToData;
      case LIndex of
        0:
          begin
            LData := TNyxDataValue.ParseJSON(
              '{"version":1,"kind":"nyx.master-detail.edit","collection":"people",' +
              '"id":"one","revision":-1,"fields":[]}');
          end;
        1:
          begin
            LData := TNyxDataValue.ParseJSON(
              '{"version":1,"kind":"nyx.master-detail.edit","collection":"people",' +
              '"id":"one","revision":0,"fields":[{"name":"x","type":"integer",' +
              '"value":"wrong"}]}');
          end;
        2:
          begin
            LData := TNyxDataValue.ParseJSON(
              '{"version":1,"kind":"nyx.master-detail.edit","collection":"people",' +
              '"id":"one","revision":0,"fields":[{"name":"x","type":"text",' +
              '"value":"a"},{"name":"x","type":"text","value":"b"}]}');
          end;
      end;
      LRejected := False;
      try
        LRead := TNyxMasterDetailRecord.FromData(LData);
        LRead.ToData;
      except
        on Exception do
        begin
          LRejected := True;
        end;
      end;
      Check(LRejected and (LRecord.Ref.ID = 'one'),
        'malformed matching payload refuses while a separate prior record remains readable');
    end;
  finally

    if LFirstToken <> nil then
    begin
      LFirstToken.Cancel;
    end;

    if LSecondToken <> nil then
    begin
      LSecondToken.Cancel;
    end;
    LFirstToken := nil;
    LSecondToken := nil;
    LFirstLease := nil;
    LSecondLease := nil;
    LFixture.Free;
  end;
end;

procedure PreparedCases;
var
  LFixture: TDetailFixture;
  LProbe: TRefreshReceiver;
  LPrevious: INyxCancellationScope;
  LFuture: INyxCancellationScope;
  LBinding: INyxMasterDetailBinding;
  LSnapshot: TNyxMasterDetailRecord;
  LRejected: Boolean;
begin
  LFixture := TDetailFixture.Create;
  LProbe := TRefreshReceiver.Create;
  try
    LFixture.View.Select(Person);
    LPrevious := NyxEventGeneration(LFixture.Events);
    LFixture.Prepare(LProbe.Refresh);
    LFuture := LFixture.EventPlan.Generation;
    LBinding := LFixture.FirstPlan.Binding;
    Check(not LBinding.Connected and not LPrevious.Cancelled and not LFuture.Cancelled,
      'preparation never activates candidate input or retires the previous generation');
    Check((LFixture.First.DetailTitleHeading.Text = 'Ada Lovelace') and
      (LFixture.First.DetailEditButton.Node.Prop(NyxAttributeName(atEnabled), 'true') = 'true') and
      (LProbe.Calls = 0), 'dormant candidate text and enabled policy are ready before Build/Sync');
    LRejected := False;
    try
      LSnapshot := LBinding.CaptureEdit;
    except
      on ENyxMasterDetailCancelled do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'cached staging data grants no Edit authority before installation');
    PublishNyxGroup([LFixture.EventPlan, LFixture.FirstPlan, LFixture.SecondPlan]);
    Check(LBinding.Connected and LPrevious.Cancelled and
      (NyxEventGeneration(LFixture.Events) = LFuture) and (LProbe.Calls = 0),
      'installation adopts future generation and flags without a host callback');
    LFixture.View.Select(Person(True));
    Check((LProbe.Calls = 1) and (LBinding.Current.Ref.ID = 'two'),
      'installed view changes use the real selected snapshot before their last refresh call');
    LSnapshot := LBinding.CaptureEdit;
    LFixture.FirstPlan.Revoke;
    Check(not LBinding.Connected and
      (LFixture.First.DetailEditButton.Node.Prop(NyxAttributeName(atEnabled)) = 'false'),
      'revoke closes input and availability before old controls retire');
    LRejected := False;
    try
      LBinding.CaptureEdit;
    except
      on ENyxMasterDetailCancelled do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LSnapshot.Ref.ID = 'two'),
      'revocation blocks new captures but does not invalidate already copied record data');
    LFixture.FirstPlan.Disconnect;
    LFixture.FirstPlan.Disconnect;
    Check((LFixture.First.DetailEditButton.Node.EnabledPolicyCount = 0) and
      (LFixture.First.DetailEditButton.Node.StoredProp(NyxAttributeName(atEnabled), 'true') =
        'true'), 'disconnect removes only runtime availability, idempotently');
  finally
    LFixture.Free;
    LProbe.Free;
  end;
end;

procedure RefusalCases;
var
  LFixture: TDetailFixture;
  LPrevious: INyxCancellationScope;
  LAtomic: INyxAtomicCollection;
  LReservation: INyxPreparedPublication;
  LMutation: TSelectionValidator;
  LMutationLease: INyxPreparedPublication;
  LRejected: Boolean;
  LIndex: Integer;
begin
  for LIndex := 0 to 4 do
  begin
    LFixture := TDetailFixture.Create;
    try
      LPrevious := NyxEventGeneration(LFixture.Events);
      LFixture.Prepare;
      case LIndex of
        0:
          begin
            LFixture.View.Select(Person);
          end;
        1:
          begin
            Supports(LFixture.View.Store, INyxAtomicCollection, LAtomic);
            LReservation := LAtomic.PrepareAssign(LFixture.View.Store.Snapshot,
              LFixture.View.Store.Snapshot.Revision);
          end;
        2:
          begin
            LFixture.EventPlan.Generation.Cancel;
          end;
        3:
          begin
            LFixture.First.DetailTitleHeading.Node.Binds.Text(NyxTextState('foreign')).Done;
          end;
        4:
          begin
            LMutation := TSelectionValidator.Create;
            LMutation.View := LFixture.View;
            LMutationLease := LMutation;
          end;
      end;
      LRejected := False;
      try

        if LMutationLease <> nil then
        begin
          PublishNyxGroup([LFixture.EventPlan, LFixture.FirstPlan, LMutationLease]);
        end
        else
        begin
          PublishNyxGroup([LFixture.EventPlan, LFixture.FirstPlan]);
        end;
      except
        on Exception do
        begin
          LRejected := True;
        end;
      end;
      Check(LRejected and not LPrevious.Cancelled and
        not LFixture.FirstPlan.Binding.Connected,
        'dirty, busy, cancelled, conflicting or later-mutated candidate refuses before install');
    finally

      if LReservation <> nil then
      begin
        LReservation.Retire;
      end;
      LReservation := nil;
      LAtomic := nil;
      LMutationLease := nil;
      LFixture.Free;
    end;
  end;
end;

procedure DisabledAndLifetimeCases;
var
  LFixture: TDetailFixture;
  LPreparation: INyxEventViewPreparation;
  LPlan: INyxPreparedMasterDetailBinding;
  LProbe: TRefreshReceiver;
  LBinding: INyxMasterDetailBinding;
  LRecord: TNyxMasterDetailRecord;
begin
  LFixture := TDetailFixture.Create;
  try
    Supports(LFixture.Events, INyxEventViewPreparation, LPreparation);
    LFixture.EventPlan := LPreparation.PrepareView;
    LFixture.First.DetailEditButton.Text := 'Open this person';
    LPlan := PrepareNyxMasterDetailDisabled(LFixture.First,
      LFixture.EventPlan, LFixture.Scheduler);
    PublishNyxGroup([LFixture.EventPlan, LPlan]);
    Check((LPlan.Binding = nil) and
      (LFixture.First.DetailEditButton.Node.Prop(NyxAttributeName(atEnabled)) = 'false') and
      (LFixture.First.DetailEditButton.Text = 'Open this person'),
      'disabled intent masks the existing face without deleting or replacing authored content');
    LPlan.Disconnect;
    Check((LFixture.First.DetailEditButton.Node.EnabledPolicyCount = 0) and
      (LFixture.First.DetailEditButton.Text = 'Open this person'),
      'disabled cleanup preserves the independently customized action');
    LPlan := nil;
  finally
    LFixture.Free;
  end;
  LFixture := TDetailFixture.Create;
  LProbe := TRefreshReceiver.Create;
  try
    LFixture.Activate(LProbe.Refresh);
    LFixture.View.Select(Person);
    LRecord := LFixture.FirstPlan.Binding.CaptureEdit;
    LBinding := LFixture.FirstPlan.Binding;
    GReleasedPlan := LFixture.FirstPlan;
    LFixture.FirstPlan := nil;
    LProbe.ReleasePlan := True;
    LFixture.View.Select(Person(True));
    Check((GReleasedPlan = nil) and not LBinding.Connected and
      (LRecord.Ref.ID = 'one') and (LRecord.Item.GetValue(NyxTextField('full-name')) =
        'Ada Lovelace'), 'last-call refresh can drop the lifetime plan without dangling callbacks');
    LFixture.View.Select(Person);
    Check(LProbe.Calls = 2, 'retired weak ports do not call the borrowed renderer receiver again');
    LBinding := nil;
  finally
    GReleasedPlan := nil;
    LFixture.Free;
    LProbe.Free;
  end;
end;

procedure AbandonAndGenerationCases;
var
  LFixture: TDetailFixture;
  LPrevious: INyxCancellationScope;
  LFuture: INyxCancellationScope;
  LBinding: INyxMasterDetailBinding;
  LRecord: TNyxMasterDetailRecord;
  LRejected: Boolean;
begin
  LFixture := TDetailFixture.Create;
  try
    LPrevious := NyxEventGeneration(LFixture.Events);
    LFixture.Prepare;
    LFuture := LFixture.EventPlan.Generation;
    LFixture.FirstPlan.Retire;
    Check(not LPrevious.Cancelled and not LFixture.FirstPlan.Binding.Connected and
      (LFixture.First.DetailEditButton.Node.EnabledPolicyCount = 0),
      'abandoning an uninstalled preparation leaves old generation and controls available');
    LFixture.EventPlan.Retire;
    Check(LFuture.Cancelled and not LPrevious.Cancelled,
      'abandoning the event owner cancels only the unused prospective generation');
  finally
    LFixture.Free;
  end;
  LFixture := TDetailFixture.Create;
  try
    LFixture.Activate;
    LFixture.View.Select(Person);
    LBinding := LFixture.FirstPlan.Binding;
    LRecord := LBinding.CaptureEdit;
    LFixture.Events.CancelPending;
    LRejected := False;
    try
      LBinding.CaptureEdit;
    except
      on ENyxMasterDetailCancelled do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and not LBinding.Connected,
      'installed successor cancellation revokes future record-capture authority');
    LFixture.View.Select(Person(True));
    Check((LFixture.First.DetailTitleHeading.Text = 'Ada Lovelace') and
      (LRecord.Ref.ID = 'one'),
      'cancelled generation receives no later presentation work or record retargeting');
  finally
    LBinding := nil;
    LFixture.Free;
  end;
end;

procedure ReservationCases;
var
  LFixture: TDetailFixture;
  LAuthored: INyxLabel;
  LOriginal: TNyxNode;
  LClone: TNyxNode;
  LProjection: TNyxNode;
  LFirst: INyxRuntimeTextReservation;
  LNext: INyxRuntimeTextReservation;
  LCloneToken: INyxRuntimeTextReservation;
  LProjectionToken: INyxRuntimeTextReservation;
  LRejected: Boolean;
begin
  LAuthored := NewNyxLabel('authored-title');
  LRejected := False;
  try
    LFirst := LAuthored.Node.ReserveRuntimeText;
  except
    on ENyxModel do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LFirst = nil), 'authored descriptors cannot acquire runtime text authority');
  LFixture := TDetailFixture.Create;
  LClone := nil;
  LProjection := nil;
  try
    LOriginal := LFixture.First.DetailTitleHeading.Node;
    LFirst := LOriginal.ReserveRuntimeText;
    LClone := LOriginal.Clone;
    LProjection := LOriginal.CloneRuntimeProjection;
    LCloneToken := LClone.ReserveRuntimeText;
    LProjectionToken := LProjection.ReserveRuntimeText;
    Check(LFirst.Active and LCloneToken.Active and LProjectionToken.Active and
      (LFirst <> LCloneToken) and (LFirst <> LProjectionToken),
      'both clone families preserve model text without carrying exclusive mutation ownership');
    LFirst.Revoke;
    LNext := LOriginal.ReserveRuntimeText;
    LOriginal.ReleaseRuntimeText(LFirst);
    Check(LNext.Active and LCloneToken.Active and LProjectionToken.Active,
      'detaching an old revoked token cannot retire a newer reservation or either clone');
    LOriginal.ReleaseRuntimeText(LNext);
    Check(not LNext.Active and not LFirst.Active,
      'exact reservation retirement remains idempotent and revocable');
  finally
    LClone.Free;
    LProjection.Free;
    LFixture.Free;
  end;
end;

procedure ConfigureAncestor(AFixture: TDetailFixture; AWithDetail: Boolean);
var
  LFactory: TTextClaimFactory;
  LFactoryLease: INyxCoordinatorFactory;
begin
  AFixture.First.Configure.PartName(NyxPart('first')).Done;
  AFixture.Runtime.Extensions.SetValue(NyxExtension('example.detail-title'), NyxData(True));
  AFixture.Registry := NewNyxCoordinatorRegistry;
  LFactory := TTextClaimFactory.Create;
  LFactoryLease := LFactory;
  LFactory.Path := NyxPart('first/detail/title');
  AFixture.Registry.RegisterFactory(NyxCustomKind(NyxKindName(nkPage)),
    NyxExtension('example.detail-title'), LFactoryLease);

  if AWithDetail then
  begin
    AFixture.Registry.RegisterFactory(NyxCustomKind(NyxKindName(nkMasterDetail)),
      NyxExtension(NyxMasterDetailIntentKey), NewNyxMasterDetailCoordinatorFactory);
  end;
  AFixture.RefreshTarget := TCoordinatorRefresh.Create;
end;

procedure SharedOwnershipCases;
var
  LFixture: TDetailFixture;
  LPreparation: INyxEventViewPreparation;
  LRejected: Boolean;
  LBaseline: TNyxText;
begin
  LFixture := TDetailFixture.Create;
  try
    LBaseline := TNyxCodec.Encode(LFixture.Document);
    ConfigureAncestor(LFixture, False);
    LFixture.Prepare;
    LRejected := False;
    try
      PrepareNyxCoordinatorMounts(LFixture.Runtime, LFixture.Bindings,
        LFixture.EventPlan, LFixture.Scheduler, LFixture.Registry.Snapshot,
        LFixture.RefreshTarget, LFixture.Mounts, LFixture.IDs);
    except
      on ENyxCoordinator do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (Length(LFixture.Mounts) = 0) and
      not LFixture.EventPlan.Generation.Cancelled,
      'a generic ancestor cannot steal equal initial text from manual MasterDetail');
    PublishNyxGroup([LFixture.EventPlan, LFixture.FirstPlan, LFixture.SecondPlan]);
    LFixture.View.Select(Person);
    Check(LFixture.FirstPlan.Binding.Connected and
      (LFixture.First.DetailTitleHeading.Text = 'Ada Lovelace') and
      (TNyxCodec.Encode(LFixture.Document) = LBaseline),
      'generic refusal preserves the manual owner, actual selection and authored definition');
  finally
    LFixture.Free;
  end;
  LFixture := TDetailFixture.Create;
  try
    ConfigureAncestor(LFixture, False);
    LFixture.PrepareAutomatic;
    LRejected := False;
    try
      LFixture.FirstPlan := PrepareNyxMasterDetailBinding(LFixture.First,
        LFixture.Options, LFixture.Bindings, LFixture.EventPlan, LFixture.Scheduler, nil);
    except
      on ENyxMasterDetail do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LFixture.FirstPlan = nil) and
      (LFixture.First.DetailEditButton.Node.EnabledPolicyCount = 0),
      'manual attachment cannot steal an equal generic claim or strand its Edit policy');
    PublishNyxGroup([LFixture.EventPlan, LFixture.Mounts[0]]);
    Check(LFixture.Mounts[0].Binding.Connected,
      'manual refusal preserves the independent generic prepared owner');
    DisconnectNyxCoordinatorMounts(LFixture.Mounts);
    LFixture.Activate;
    LFixture.View.Select(Person);
    Check(LFixture.FirstPlan.Binding.Connected and
      (LFixture.First.DetailTitleHeading.Text = 'Ada Lovelace'),
      'retired generic claims release exact roles for a later manual attachment');
  finally
    LFixture.Free;
  end;
  LFixture := TDetailFixture.Create;
  try
    ConfigureAncestor(LFixture, True);
    LRejected := False;
    try
      LFixture.PrepareAutomatic;
    except
      on ENyxCoordinator do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (Length(LFixture.Mounts) = 0) and
      (LFixture.First.DetailEditButton.Node.EnabledPolicyCount = 0),
      'automatic MasterDetail and a generic ancestor share the same exclusive text admission');
    LFixture.EventPlan.Retire;
    LFixture.EventPlan := nil;
    LFixture.Registry := nil;
    LFixture.AcceptAutomatic;
    LFixture.View.Select(Person);
    Check((Length(LFixture.Mounts) = 2) and LFixture.Automatic.Connected and
      (LFixture.First.DetailTitleHeading.Text = 'Ada Lovelace'),
      'failed automatic candidates release claims and permit the same real tree to prepare again');
  finally
    LFixture.Free;
  end;
end;

{$ifdef PAS2JS}
function UITurn: TJSPromise;
begin
  Result := TJSPromise.new(procedure(AResolve, AReject: TJSPromiseResolver)
    begin
      window.setTimeout(procedure
        begin
          AResolve(JSValue(True));
        end, 0);
    end);
end;
{$endif}

procedure AwaitEdit(const AExecution: INyxExecution); {$ifdef PAS2JS}async;{$endif}
var
  LExecution: INyxExecution;
  {$ifdef PAS2JS}LStarted: Double;{$else}LStarted: QWord;{$endif}
begin
  LExecution := AExecution;
  {$ifdef PAS2JS}LStarted := TJSDate.now;{$else}LStarted := GetTickCount64;{$endif}
  while LExecution.Status in [nesPending, nesRunning] do
  begin
    {$ifdef PAS2JS}

    if TJSDate.now - LStarted > 10000 then
    {$else}

    if GetTickCount64 - LStarted > 10000 then
    {$endif}
    begin
      raise Exception.Create('Queued MasterDetail Edit did not complete');
    end;
    {$ifdef PAS2JS}await(UITurn);{$else}CheckSynchronize(1);{$endif}
  end;
  Check(LExecution.Status = nesSucceeded, 'queued Edit succeeds: ' + LExecution.Failure);
end;

procedure AutomaticCases; {$ifdef PAS2JS}async;{$endif}
var
  LFixture: TDetailFixture;
  LBinding: INyxMasterDetailBinding;
  LBase: INyxCoordinatorBinding;
  LDispatch: TNyxDispatch;
  LEvent: TNyxEventInfo;
  LCopy: TNyxEventInfo;
  LReceiver: TEditReceiver;
  LCallback: INyxEventCallback;
  LToken: INyxEventSubscription;
  LExecutions: TNyxExecutions;
  LRejected: Boolean;
  LBaseline: TNyxText;
begin
  LFixture := TDetailFixture.Create;
  try
    LBaseline := TNyxCodec.Encode(LFixture.Document);
    LFixture.AcceptAutomatic;
    LBinding := LFixture.Automatic;
    LBase := FindNyxCoordinatorMount(LFixture.Mounts, LFixture.IDs, LFixture.First.ID);
    Check((LBinding <> nil) and LBinding.Connected and
      Supports(LBinding, INyxCoordinatorBinding) and (LBase <> nil),
      'common lookup preserves the existing specialized MasterDetail command interface');
    LFixture.View.Select(Person);
    Check((LBinding.Current.Ref.ID = 'one') and
      not LFixture.Automatic(True).Current.Defined and
      (LFixture.First.DetailTitleHeading.Text = 'Ada Lovelace'),
      'common automatic mounting uses the real visible selection and independent instance source');
    LDispatch := DispatchNyxBehavior(LFixture.First.DetailEditButton.Node, ntClick);
    LEvent := LDispatch.Info.Copy;
    LEvent.HasValue := True;
    LEvent.ValueKind := nskText;
    LEvent.Value := NyxData('independent scalar');
    LEvent.ValueID := 'owned-scalar-source';
    LCopy := EnrichNyxCoordinatorEvent(LFixture.Mounts, LEvent);
    Check(LCopy.HasDetails and LCopy.HasValue and (LCopy.ValueKind = nskText) and
      (LCopy.Value.AsText = 'independent scalar') and
      (LCopy.ValueID = LEvent.ValueID) and not LEvent.HasDetails,
      'common prequeue enrichment copies record Details without replacing scalar Value or input');
    LReceiver := TEditReceiver.Create;
    LCallback := LReceiver;
    LToken := LFixture.Events.On(NyxControlEvents(
      LFixture.First.DetailEditButton.ID, niRuntime), ntClick).
      Policy(neUIQueue).Subscribe(LCallback);
    LExecutions := LFixture.Events.Dispatch(LCopy,
      LFixture.First.DetailEditButton.Node.DesignID, LFixture.First.Node.DesignID);
    Check(Length(LExecutions) = 1, 'only the actual ordinary queued Edit callback is scheduled');
    LFixture.View.Select(Person(True));
    {$ifdef PAS2JS}await(AwaitEdit(LExecutions[0]));{$else}AwaitEdit(LExecutions[0]);{$endif}
    Check((LReceiver.Calls = 1) and (LReceiver.Last.Ref.ID = 'one') and
      LReceiver.HasScalar and (LReceiver.ScalarKind = nskText) and
      (LReceiver.Scalar.AsText = 'independent scalar') and
      (LReceiver.ValueID = 'owned-scalar-source') and (LBinding.Current.Ref.ID = 'two'),
      'queued callback keeps original record and scalar after current selection changes');
    LEvent.SourceID := LFixture.Second.ID;
    LCopy := EnrichNyxCoordinatorEvent(LFixture.Mounts, LEvent);
    Check(not LCopy.HasDetails and LCopy.HasValue and
      (LCopy.Value.AsText = 'independent scalar'),
      'a foreign source with another instance Edit origin gains no structured record');
    LEvent := LDispatch.Info.Copy;
    LEvent.HasDetails := True;
    LEvent.Details := NyxObject([NyxField('application', NyxData('owned'))]);
    LRejected := False;
    try
      LCopy := EnrichNyxCoordinatorEvent(LFixture.Mounts, LEvent);
    except
      on ENyxMasterDetail do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LEvent.Details.Field('application').AsText = 'owned'),
      'common enrichment refuses to overwrite an existing owned structured payload');
    Check(TNyxCodec.Encode(LFixture.Document) = LBaseline,
      'automatic presentation, queued capture and refusal do not serialize runtime records');
  finally

    if LToken <> nil then
    begin
      LToken.Cancel;
    end;
    LToken := nil;
    LExecutions := nil;
    LCallback := nil;
    LBase := nil;
    LBinding := nil;
    LFixture.Free;
  end;
end;

procedure RoleFenceCases;
var
  LFixture: TDetailFixture;
  LBinding: INyxMasterDetailBinding;
  LValidator: TRoleValidator;
  LParticipant: INyxPreparedPublication;
  LPrevious: INyxCancellationScope;
  LMutation: TRoleMutation;
  LRejected: Boolean;
begin
  for LMutation := rmProjectionKind to rmCompound do
  begin
    LFixture := TDetailFixture.Create;
    try
      LPrevious := NyxEventGeneration(LFixture.Events);
      LFixture.PrepareAutomatic;
      LBinding := LFixture.Automatic;
      LValidator := TRoleValidator.Create;
      LParticipant := LValidator;
      LValidator.Fixture := LFixture;
      LValidator.Mutation := LMutation;
      LRejected := False;
      try
        PublishNyxGroup([LFixture.Mounts[0], LFixture.Mounts[1],
          LFixture.EventPlan, LParticipant]);
      except
        on ENyxCoordinator do
        begin
          LRejected := True;
        end;
      end;
      Check(LRejected and not LPrevious.Cancelled and
        LFixture.EventPlan.Generation.Cancelled,
        'post-Describe primitive/compound mutation refuses at the final common role fence');
      Check(not LBinding.Connected and
        (LFixture.First.DetailEditButton.Node.EnabledPolicyCount = 0),
        'late role-proof refusal activates no controller and retires candidate availability');
    finally
      LBinding := nil;
      LParticipant := nil;
      LFixture.Free;
    end;
  end;
end;

procedure AutomaticPolicyCases;
var
  LFixture: TDetailFixture;
  LBaseline: TNyxText;
begin
  LFixture := TDetailFixture.Create;
  try
    LFixture.First.DetailTitleHeading.Configure.
      WhenViewport(TNyxViewportWidth.Below(640)).Text('Compact record details').Done;
    LFixture.Runtime.ApplyViewport(390, 800, npfNativeLCL);
    LFixture.AcceptAutomatic;
    Check(LFixture.First.DetailTitleHeading.Node.StoredProp(NyxAttributeName(atText)) =
      'Compact record details',
      'automatic placeholder captures effective viewport text before its first projection');
    LFixture.View.Select(Person);
    LFixture.View.ClearSelection;
    Check(LFixture.First.DetailTitleHeading.Node.StoredProp(NyxAttributeName(atText)) =
      'Compact record details', 'empty selection restores the same effective placeholder');
  finally
    LFixture.Free;
  end;
  LFixture := TDetailFixture.Create;
  try
    LBaseline := TNyxCodec.Encode(LFixture.Document);
    SetNyxMasterDetailIntent(LFixture.First, TNyxMasterDetailIntent.Disabled);
    LFixture.AcceptAutomatic;
    Check((LFixture.Automatic = nil) and (LFixture.Automatic(True) <> nil) and
      (LFixture.First.DetailEditButton.Node.Prop(NyxAttributeName(atEnabled)) = 'false'),
      'disabled common owner masks Edit but exposes no specialized selected-record command');
    Check(TNyxCodec.Encode(LFixture.Document) = LBaseline,
      'runtime disabled masking leaves the original inherited authored intent intact');
  finally
    LFixture.Free;
  end;
  LFixture := TDetailFixture.Create;
  try
    InheritNyxMasterDetailIntent(LFixture.First);
    InheritNyxMasterDetailIntent(LFixture.Second);
    LFixture.AcceptAutomatic;
    Check((Length(LFixture.Mounts) = 0) and (LFixture.Automatic = nil) and
      (LFixture.First.DetailTitleHeading.Text = 'Record details'),
      'absent effective intent keeps the ordinary static recipe without loading a factory');
  finally
    LFixture.Free;
  end;
end;

procedure AutomaticDisconnectCases;
var
  LFixture: TDetailFixture;
  LAutomatic: INyxMasterDetailBinding;
  LManual: INyxMasterDetailBinding;
  LGeneration: INyxCancellationScope;
  LRecord: TNyxMasterDetailRecord;
  LBaseline: TNyxText;
  LRejected: Boolean;
begin
  LFixture := TDetailFixture.Create;
  try
    LBaseline := TNyxCodec.Encode(LFixture.Document);
    LFixture.AcceptAutomatic;
    LGeneration := NyxEventGeneration(LFixture.Events);
    LAutomatic := LFixture.Automatic;
    LFixture.View.Select(Person);
    LRecord := LAutomatic.CaptureEdit;
    LFixture.View.ClearSelection;
    Check(not LAutomatic.Current.Defined and
      (LFixture.First.DetailEditButton.Node.Prop(NyxAttributeName(atEnabled)) = 'false'),
      'empty automatic selection installs the real false Edit mask before explicit disconnect');

    { Call the exact public selected-record binding, not its enclosing prepared
      mount. The event plan and the other instance remain accepted throughout. }
    LAutomatic.Disconnect;
    LAutomatic.Disconnect;
    Check(not LAutomatic.Connected and not LGeneration.Cancelled and
      (NyxEventGeneration(LFixture.Events) = LGeneration) and
      LFixture.Automatic(True).Connected and
      (LFixture.First.DetailEditButton.Node.EnabledPolicyCount = 0) and
      (LFixture.First.DetailEditButton.Node.Prop(NyxAttributeName(atEnabled), 'true') = 'true'),
      'typed Disconnect immediately releases only its own scope without cancelling the live view');
    LRejected := False;
    try
      LAutomatic.CaptureEdit;
    except
      on ENyxMasterDetailCancelled do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LRecord.Ref.ID = 'one') and
      (LRecord.Item.GetValue(NyxTextField('full-name')) = 'Ada Lovelace'),
      'explicitly disconnected commands refuse while their earlier copied record remains valid');

    { Reuse the existing installed event plan, so successful reacquisition cannot
      be explained by cancelling the old generation or realizing a fresh board. }
    LFixture.FirstPlan := PrepareNyxMasterDetailBinding(LFixture.First,
      LFixture.Options, LFixture.Bindings, LFixture.EventPlan, LFixture.Scheduler, nil);
    LManual := LFixture.FirstPlan.Binding;
    PublishNyxGroup([LFixture.FirstPlan]);
    Check(LManual.Connected and not LGeneration.Cancelled and
      (NyxEventGeneration(LFixture.Events) = LGeneration) and
      (LFixture.First.DetailEditButton.Node.EnabledPolicyCount = 1),
      'the same real board reacquires both text roles and Edit through the unchanged manual API');
    LFixture.View.Select(Person(True));
    Check((LManual.CaptureEdit.Ref.ID = 'two') and
      (LFixture.First.DetailTitleHeading.Text = 'Grace Hopper') and
      (LFixture.First.DetailEditButton.Node.Prop(NyxAttributeName(atEnabled), 'true') = 'true'),
      'manual reacquisition observes actual selection and owns its new availability policy');

    { Old common scopes may still be retained by the host. Their later exact
      cleanup must never detach the manual owner's replacement tokens/policy. }
    DisconnectNyxCoordinatorMounts(LFixture.Mounts);
    LFixture.View.ClearSelection;
    LFixture.View.Select(Person);
    Check(LManual.Connected and not LGeneration.Cancelled and
      (LManual.CaptureEdit.Ref.ID = 'one') and
      (LFixture.First.DetailTitleHeading.Text = 'Ada Lovelace') and
      (LFixture.First.DetailEditButton.Node.EnabledPolicyCount = 1) and
      (LFixture.First.DetailEditButton.Node.Prop(NyxAttributeName(atEnabled), 'true') = 'true'),
      'later old-mount retirement leaves the replacement manual claims and selection usable');
    Check((LFixture.View(True).Selection.Count = 0) and
      (LFixture.View(True).Store.Snapshot.Revision = 0) and
      (TNyxCodec.Encode(LFixture.Document) = LBaseline),
      'disconnect and same-board reacquisition preserve sibling data and all authored defaults');
  finally
    LManual := nil;
    LAutomatic := nil;
    LFixture.Free;
  end;
end;

procedure Run; {$ifdef PAS2JS}async;{$endif}
begin
  try
    IntentCases;
    SelectionCases;
    EventCases;
    PreparedCases;
    RefusalCases;
    DisabledAndLifetimeCases;
    AbandonAndGenerationCases;
    ReservationCases;
    SharedOwnershipCases;
    {$ifdef PAS2JS}await(AutomaticCases);{$else}AutomaticCases;{$endif}
    RoleFenceCases;
    AutomaticPolicyCases;
    AutomaticDisconnectCases;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-master-detail', 'passed');
    document.body.setAttribute('data-master-detail-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' portable MasterDetail checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-master-detail', 'failed');
      document.body.setAttribute('data-master-detail-checks', IntToStr(GChecks));
      document.body.setAttribute('data-master-detail-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message, ' at check count ', GChecks);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end;

begin
  Run;
end.
