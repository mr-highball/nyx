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

program nyx_coordinators_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.types, nyx.data, nyx.model, nyx.controls, nyx.state,
  nyx.coordinators, nyx.collections, nyx.collections.view,
  nyx.collections.view.types, nyx.collections.bindings, nyx.collections.selection,
  nyx.collections.query, nyx.composition, nyx.codec, nyx.events, nyx.behavior,
  nyx.scheduler, nyx.publication
  {$ifdef PAS2JS}, JS, Web{$endif};

const
  CIntent = 'example.selection-summary';
  CSecondIntent = 'example.second-summary';

type
  { Controlled failures occur only after the creator revokes and disconnects its
    actual subscriptions. Platform values exercise the browser bridge boundary;
    every fixture destructor itself remains nonthrowing. }
  TCleanupFault = (cfNone, cfEmptyPascal{$ifdef PAS2JS}, cfPlatform{$endif});
  {$ifdef PAS2JS}
  { Browser bridges can reject without an Error object. These distinct payload
    domains must survive all-attempt retirement without truthiness conversion. }
  TPlatformFailureKind = (pfUndefined, pfNull, pfObject);
  {$endif}
  { A creator-owned specialized binding remains discoverable through the generic
    driver's Binding/Find contract. Count means retained selection membership. }
  ISummaryBinding = interface(INyxCoordinatorBinding)
    ['{CC193689-0DFE-437F-B56E-8F96B09EDEB6}']
    function Count: Integer;
    procedure Clear;
    function Notifications: Integer;
    function Disconnections: Integer;
  end;
  { Forward declaration for the example's weak event/view receiver. }
  TSummary = class;
  { Event/view subscriptions retain this weak port, never the creator owner. }
  ISummaryPort = interface(IInterface)
    ['{016CF22B-ECF4-48F3-9C39-08559AE684B1}']
    function Acquire: ISummaryBinding;
    procedure Revoke;
  end;
  { A normal physical Click observer clears selection synchronously without
    replacing the caller's ordinary callback execution policy. }
  TSummaryPort = class(TNyxEventObserver, ISummaryPort)
  private
    FOwner: TSummary;
  public
    constructor Create(AOwner: TSummary);
    function Acquire: ISummaryBinding;
    procedure Revoke;
    procedure Observe(const AEvent: TNyxEventInfo; ACallbackCount: Integer); override;
    procedure Changed(const AView: INyxCollectionView;
      const AChanges: INyxCollectionChanges);
  end;
  { Renderer-free creator example using actual collection/event owners and only
    guarded role projections. It owns no document, raw node or widget. }
  TSummary = class(TInterfacedObject, INyxPreparedCoordinator,
    INyxPreparedPublication, INyxPublicationReadiness, INyxCoordinatorBinding,
    ISummaryBinding)
  private
    FProjection: INyxCoordinatorProjection;
    FRefresh: INyxCoordinatorRefresh;
    FView: INyxCollectionView;
    FGeneration: INyxCancellationScope;
    FEventView: INyxPreparedEventView;
    FViewToken: INyxCollectionViewSubscription;
    FClickToken: INyxEventSubscription;
    FPort: ISummaryPort;
    FReceiver: TSummaryPort;
    FStatus: TNyxControlRef;
    FClear: TNyxControlRef;
    FConnected: Boolean;
    FValidated: Boolean;
    FRetired: Boolean;
    FRevoked: Boolean;
    FCleaned: Boolean;
    FNotifications: Integer;
    FDisconnections: Integer;
    procedure RequireLive;
    procedure Update(ARefresh: Boolean);
    procedure Cleanup;
  public
    FailValidate: Boolean;
    FailNotify: Boolean;
    FailDisconnect: Boolean;
    CleanupFault: TCleanupFault;
    {$ifdef PAS2JS}
    PlatformFailure: JSValue;
    {$endif}
    ReleaseOnNotify: Boolean;
    constructor Create(const AProjection: INyxCoordinatorProjection;
      const AViews: INyxCollectionBindings; const AView: INyxPreparedEventView;
      const ARefresh: INyxCoordinatorRefresh);
    destructor Destroy; override;
    function GetBinding: INyxCoordinatorBinding;
    function GetConnected: Boolean;
    function Count: Integer;
    procedure Clear;
    function Notifications: Integer;
    function Disconnections: Integer;
    procedure Revoke;
    procedure Disconnect;
    procedure Validate;
    procedure CheckReady;
    procedure Install;
    procedure Notify;
    procedure Retire;
  end;
  { The factory owns explicit test observation leases. RegisterDuringPrepare
    demonstrates that the candidate captured its registry before callbacks. }
  TSummaryFactory = class(TInterfacedObject, INyxCoordinatorFactory)
  public
    Calls: Integer;
    FailPrepare: Boolean;
    FailValidate: Boolean;
    FailNotify: Boolean;
    FailDisconnect: Boolean;
    CleanupFault: TCleanupFault;
    FirstCleanupFault: TCleanupFault;
    {$ifdef PAS2JS}
    PlatformFailure: JSValue;
    {$endif}
    ReleaseOnNotify: Boolean;
    RegisterDuringPrepare: INyxCoordinatorRegistry;
    Last: ISummaryBinding;
    Projection: INyxCoordinatorProjection;
    Refresh: INyxCoordinatorRefresh;
    function Prepare(const AProjection: INyxCoordinatorProjection;
      const AIntent: TNyxDataValue; const AViews: INyxCollectionBindings;
      const AView: INyxPreparedEventView; const AScheduler: INyxScheduler;
      const ARefresh: INyxCoordinatorRefresh): INyxPreparedCoordinator;
  end;
  { A public factory composes ordinary summary preparation with one ancestor
    text claim. It retains only copied role identity and managed capabilities. }
  TAncestorClaimFactory = class(TInterfacedObject, INyxCoordinatorFactory)
  private
    FInner: INyxCoordinatorFactory;
    FAncestor: TNyxControlRef;
  public
    ClaimAncestor: Boolean;
    Claimed: INyxCoordinatorProjection;
    constructor Create(const AInner: INyxCoordinatorFactory;
      const AAncestor: TNyxControlRef);
    function Prepare(const AProjection: INyxCoordinatorProjection;
      const AIntent: TNyxDataValue; const AViews: INyxCollectionBindings;
      const AView: INyxPreparedEventView; const AScheduler: INyxScheduler;
      const ARefresh: INyxCoordinatorRefresh): INyxPreparedCoordinator;
  end;
  { A real weak renderer-style target. Its caller can retire the full mount
    vector on Request; the guarded refresh caller cannot touch it afterward. }
  TRefreshTarget = class(TInterfacedObject, INyxCoordinatorRefreshTarget)
  public
    Calls: Integer;
    ReleaseMounts: Boolean;
    procedure Request;
  end;
  { One existing reusable definition realized twice with independent instance
    stores, ordinary list views and actual event-view publication. }
  TFixture = class
  public
    Document: TNyxDocument;
    Runtime: TNyxNode;
    Views: INyxCollectionBindings;
    Scheduler: INyxScheduler;
    Events: INyxEvents;
    EventPlan: INyxPreparedEventView;
    Registry: INyxCoordinatorRegistry;
    Factory: TSummaryFactory;
    FactoryLease: INyxCoordinatorFactory;
    Target: TRefreshTarget;
    TargetLease: INyxCoordinatorRefreshTarget;
    Mounts: TNyxCoordinatorMounts;
    IDs: TNyxCoordinatorMountIDs;
    Baseline: TNyxText;
    constructor Create;
    destructor Destroy; override;
    function Root(ASecond: Boolean = False): TNyxNode;
    function View(ASecond: Boolean = False): INyxCollectionView;
    function Summary(ASecond: Boolean = False): ISummaryBinding;
    procedure Prepare;
    procedure Accept;
    procedure Click(ASecond: Boolean = False; AExpectObserver: Boolean = True);
  end;
  { A later group validator changes actual authority after prior Validate. It
    neither fabricates a view nor bypasses the ordinary selection owner. }
  TInterventionKind = (ikSelect, ikCancel, ikRevoke, ikReenter);
  { Managed participant used to prove the final shared publication fence and
    pure installation boundary against reentrant/later validator work. }
  TIntervention = class(TInterfacedObject, INyxPreparedPublication)
  public
    Fixture: TFixture;
    Kind: TInterventionKind;
    ReentryRefused: Boolean;
    procedure Validate;
    procedure Install;
    procedure Notify;
    procedure Retire;
  end;
  { An ordinary view observer attempts preparation while the real view is busy.
    It catches refusal locally so the existing selection publication can finish. }
  TBusyReceiver = class
  public
    Fixture: TFixture;
    Busy: Boolean;
    Rejected: Boolean;
    procedure Changed(const AView: INyxCollectionView;
      const AChanges: INyxCollectionChanges);
  end;

var
  GChecks: Integer;
  GRelease: TNyxCoordinatorMounts;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Coordinators: ' + AReason);
  end;
  Inc(GChecks);
end;

function Item(ASecond: Boolean = False): TNyxItemRef;
begin
  Result := NyxItem(NyxCollection('people'), 'one');

  if ASecond then
  begin
    Result := NyxItem(NyxCollection('people'), 'two');
  end;
end;

constructor TSummaryPort.Create(AOwner: TSummary);
begin
  inherited Create;
  FOwner := AOwner;
end;

function TSummaryPort.Acquire: ISummaryBinding;
begin
  Result := nil;

  if FOwner <> nil then
  begin
    Result := FOwner;
  end;
end;

procedure TSummaryPort.Revoke;
begin
  FOwner := nil;
end;

procedure TSummaryPort.Observe(const AEvent: TNyxEventInfo; ACallbackCount: Integer);
var
  LOwner: ISummaryBinding;
begin
  LOwner := Acquire;

  if (LOwner <> nil) and LOwner.Connected then
  begin
    LOwner.Clear;
  end;
end;

procedure TSummaryPort.Changed(const AView: INyxCollectionView;
  const AChanges: INyxCollectionChanges);
var
  LLease: ISummaryBinding;
  LOwner: TSummary;
begin
  LLease := Acquire;
  LOwner := FOwner;

  if (LLease <> nil) and (LOwner <> nil) and LOwner.GetConnected then
  begin
    LOwner.Update(True);
  end;
end;

constructor TSummary.Create(const AProjection: INyxCoordinatorProjection;
  const AViews: INyxCollectionBindings; const AView: INyxPreparedEventView;
  const ARefresh: INyxCoordinatorRefresh);
var
  LItems: TNyxControlRef;
  LSpec: TNyxCollectionViewSpec;
begin
  inherited Create;
  FProjection := AProjection;
  FRefresh := ARefresh;
  FEventView := AView;
  FGeneration := FEventView.Generation;
  LItems := FProjection.Part(NyxPart('items'));
  FStatus := FProjection.Part(NyxPart('selection-status'));
  FClear := FProjection.Part(NyxPart('clear-selection'));

  if not FProjection.TryCollectionSpec(LItems, LSpec) or
    (LSpec.SelectionMode <> nsmMultiple) then
  begin
    raise ENyxCoordinator.Create('Selection Summary requires a declared multiple-selection list');
  end;
  Check((FProjection.Kind(LItems).Name = NyxKindName(nkList)) and
    (FProjection.AuthoredText(FStatus) = 'Retained selected count: 0'),
    'creator receives copied typed role discovery and the authored baseline');
  FView := AViews.ViewFor(LItems.ID);
  FReceiver := TSummaryPort.Create(Self);
  FPort := FReceiver;
  FViewToken := FView.Subscribe(FReceiver.Changed);
  FClickToken := FEventView.PrepareObservation(NyxControlEvents(FClear.ID, niRuntime),
    ntClick, FReceiver, nopBeforeCallbacks, NyxEvent(''));
end;

destructor TSummary.Destroy;
begin
  try
    Cleanup;
  except
    { Explicit cleanup reports the injected post-cleanup error. }
  end;
  inherited Destroy;
end;

procedure TSummary.RequireLive;
var
  LGeneration: INyxCancellationScope;
  LCancelled: Boolean;
begin
  LGeneration := FGeneration;

  if FRevoked or not FConnected or (LGeneration = nil) then
  begin
    raise ENyxCoordinatorCancelled.Create('Selection Summary is no longer active');
  end;
  LCancelled := LGeneration.Cancelled;

  if LCancelled or FRevoked or not FConnected then
  begin
    raise ENyxCoordinatorCancelled.Create('Selection Summary generation was cancelled');
  end;
end;

function TSummary.GetBinding: INyxCoordinatorBinding;
begin
  Result := Self;
end;

function TSummary.GetConnected: Boolean;
begin
  Result := FConnected and not FRevoked and (FGeneration <> nil);

  if Result then
  begin
    Result := not FGeneration.Cancelled and FConnected and not FRevoked;
  end;
end;

function TSummary.Count: Integer;
var
  LLease: ISummaryBinding;
begin
  LLease := Self;
  RequireLive;
  Result := FView.Selection.Count;
  RequireLive;
end;

procedure TSummary.Update(ARefresh: Boolean);
var
  LLease: ISummaryBinding;
  LCount: Integer;
  LRefresh: INyxCoordinatorRefresh;
begin
  LLease := Self;
  LCount := FView.Selection.Count;
  FProjection.Text(FStatus, 'Retained selected count: ' + IntToStr(LCount));
  FProjection.Enabled(FClear, LCount > 0);
  LRefresh := FRefresh;

  if ARefresh then
  begin
    { Last outward call. It can disconnect and release the entire owner vector. }
    LRefresh.Request;
  end;
end;

procedure TSummary.Clear;
var
  LLease: ISummaryBinding;
  LView: INyxCollectionView;
begin
  LLease := Self;
  RequireLive;
  LView := FView;
  LView.ClearSelection;
end;

function TSummary.Notifications: Integer;
begin
  Result := FNotifications;
end;

function TSummary.Disconnections: Integer;
begin
  Result := FDisconnections;
end;

procedure TSummary.Revoke;
begin
  FRevoked := True;
  FConnected := False;

  if FPort <> nil then
  begin
    FPort.Revoke;
  end;
end;

procedure TSummary.Cleanup;
{$ifdef PAS2JS}
var
  LPlatformFailure: JSValue;
{$endif}
begin
  Revoke;

  if FCleaned then
  begin
    Exit;
  end;
  FCleaned := True;
  Inc(FDisconnections);

  if FViewToken <> nil then
  begin
    FViewToken.Disconnect;
  end;

  if FClickToken <> nil then
  begin
    FClickToken.Cancel;
  end;
  FViewToken := nil;
  FClickToken := nil;
  FView := nil;
  FProjection := nil;
  FRefresh := nil;
  FEventView := nil;
  FGeneration := nil;
  FPort := nil;
  FReceiver := nil;

  if CleanupFault = cfEmptyPascal then
  begin
    raise ENyxCoordinator.Create('');
  end;
  {$ifdef PAS2JS}

  if CleanupFault = cfPlatform then
  begin
    { A Pascal class cast emits rtl.getObject and converts undefined to null.
      This minimal platform boundary must produce the actual arbitrary throw,
      so the test can detect that same lossy cast in a retirement implementation. }
    LPlatformFailure := PlatformFailure;
    asm
      throw LPlatformFailure;
    end;
  end;
  {$endif}

  if FailDisconnect then
  begin
    raise ENyxCoordinator.Create('Injected creator cleanup failure after retirement');
  end;
end;

procedure TSummary.Disconnect;
var
  LLease: ISummaryBinding;
begin
  LLease := Self;
  Cleanup;
end;

procedure TSummary.Validate;
begin

  if FValidated or FRetired or FRevoked or FGeneration.Cancelled then
  begin
    {$ifdef PAS2JS}
    { Copied diagnostics only at the existing refusal. Do not query a second
      generation getter or alter the original condition, exception or checks. }
    document.body.setAttribute('data-coordinators-refusal',
      'role=' + FStatus.ID + ';validated=' + BoolToStr(FValidated, True) +
      ';retired=' + BoolToStr(FRetired, True) +
      ';revoked=' + BoolToStr(FRevoked, True) +
      ';generation-present=' + BoolToStr(FGeneration <> nil, True));
    {$endif}
    raise ENyxCoordinatorCancelled.Create('Summary preparation is not current');
  end;

  if FailValidate then
  begin
    raise ENyxCoordinator.Create('Injected creator validation refusal');
  end;
  FValidated := True;
end;

procedure TSummary.CheckReady;
begin

  if not FValidated or FRetired or FRevoked or FGeneration.Cancelled then
  begin
    raise ENyxCoordinatorCancelled.Create('Summary final readiness was revoked');
  end;
end;

procedure TSummary.Install;
begin
  FConnected := True;
end;

procedure TSummary.Notify;
begin
  Inc(FNotifications);

  if ReleaseOnNotify then
  begin
    ReleaseOnNotify := False;
    DisconnectNyxCoordinatorMounts(GRelease);
  end;

  if FailNotify then
  begin
    raise ENyxCoordinator.Create('Injected creator notification failure after install');
  end;
end;

procedure TSummary.Retire;
begin

  if FRetired then
  begin
    Exit;
  end;
  FRetired := True;

  if not FConnected then
  begin
    Cleanup;
  end;
  FEventView := nil;
end;

function TSummaryFactory.Prepare(const AProjection: INyxCoordinatorProjection;
  const AIntent: TNyxDataValue; const AViews: INyxCollectionBindings;
  const AView: INyxPreparedEventView; const AScheduler: INyxScheduler;
  const ARefresh: INyxCoordinatorRefresh): INyxPreparedCoordinator;
var
  LOwner: TSummary;
  LPlan: INyxPreparedCoordinator;
  LOther: INyxCoordinatorFactory;
  LRegistry: INyxCoordinatorRegistry;
begin
  Inc(Calls);
  Projection := AProjection;
  Refresh := ARefresh;
  Check(AIntent.Field('version').AsInteger = 1,
    'factory receives the exact copied configured intent');

  if FailPrepare then
  begin
    raise ENyxCoordinator.Create('Injected factory preparation refusal');
  end;
  LRegistry := RegisterDuringPrepare;

  if LRegistry <> nil then
  begin
    RegisterDuringPrepare := nil;
    LOther := TSummaryFactory.Create;
    LRegistry.RegisterFactory(NyxCustomKind(NyxKindName(nkCard)),
      NyxExtension('example.later-registration'), LOther);
  end;
  LOwner := TSummary.Create(AProjection, AViews, AView, ARefresh);
  LPlan := LOwner;
  LOwner.FailValidate := FailValidate;
  LOwner.FailNotify := FailNotify;
  LOwner.FailDisconnect := FailDisconnect;
  LOwner.CleanupFault := CleanupFault;

  if (Calls = 1) and (FirstCleanupFault <> cfNone) then
  begin
    LOwner.CleanupFault := FirstCleanupFault;
  end;
  {$ifdef PAS2JS}
  LOwner.PlatformFailure := PlatformFailure;
  {$endif}
  LOwner.ReleaseOnNotify := ReleaseOnNotify;
  { Initial projection takes a temporary self-interface lease. The factory must
    own LPlan first: pas2js does not supply FPC's constructor reference guard.
    Keep the first projection before publication and before the observation
    lease, so a refused initialization releases the unadmitted child normally. }
  LOwner.Update(False);
  Last := LOwner;
  Result := LPlan;
end;

constructor TAncestorClaimFactory.Create(const AInner: INyxCoordinatorFactory;
  const AAncestor: TNyxControlRef);
begin
  inherited Create;
  FInner := AInner;
  FAncestor := AAncestor;
  ClaimAncestor := True;
end;

function TAncestorClaimFactory.Prepare(const AProjection: INyxCoordinatorProjection;
  const AIntent: TNyxDataValue; const AViews: INyxCollectionBindings;
  const AView: INyxPreparedEventView; const AScheduler: INyxScheduler;
  const ARefresh: INyxCoordinatorRefresh): INyxPreparedCoordinator;
var
  LRole: TNyxControlRef;
begin

  if ClaimAncestor and (AProjection.Root.ID = FAncestor.ID) then
  begin
    Claimed := AProjection;
    LRole := AProjection.Part(NyxPart('nested/selection-status'));
    AProjection.Text(LRole, 'Retained selected count: 0');
  end;
  Result := FInner.Prepare(AProjection, AIntent, AViews, AView, AScheduler, ARefresh);
end;

procedure TRefreshTarget.Request;
begin
  Inc(Calls);

  if ReleaseMounts then
  begin
    DisconnectNyxCoordinatorMounts(GRelease);
  end;
end;

constructor TFixture.Create;
var
  LCard: INyxCard;
  LList: INyxList;
  LStatus: INyxLabel;
  LClear: INyxButton;
  LPage: INyxPage;
  LInstance: INyxComponent;
begin
  inherited Create;
  Document := TNyxDocument.Create;
  Document.Collections.Define(NyxCollection('people'),
    NyxCollectionSchema.Text(NyxTextField('name'), ''), [
    NyxCollectionItem(Item).WithValue(NyxTextField('name'), 'Ada'),
    NyxCollectionItem(Item(True)).WithValue(NyxTextField('name'), 'Grace')]);
  LCard := NewNyxCard('summary', ncoDescriptor);
  LList := NewNyxList('items');
  LList.Configure.PartName(NyxPart('items')).Done;
  LList.Binds.Collection(NyxCollectionView(NyxCollection('people')).
    Column(NyxTextField('name'), 'People').Scoped(csInstance).
    Selection(nsmMultiple)).Done;
  LCard.Add(LList);
  LStatus := NewNyxLabel('selection-status');
  LStatus.Text := 'Retained selected count: 0';
  LStatus.Configure.PartName(NyxPart('selection-status')).Done;
  LCard.Add(LStatus);
  LClear := NewNyxButton('clear-selection');
  LClear.Text := 'Clear selection';
  LClear.Configure.PartName(NyxPart('clear-selection')).AccessibleName('Clear selection').Done;
  LCard.Add(LClear);
  LCard.Extensions.SetValue(NyxExtension(CIntent),
    NyxObject([NyxField('version', NyxData(1))]));
  Document.AddComponent(LCard);
  LPage := NewNyxPage('home');
  LInstance := NewNyxComponent('first-summary');
  LInstance.Reference := NyxComponent('summary');
  LPage.Add(LInstance);
  LInstance := NewNyxComponent('second-summary');
  LInstance.Reference := NyxComponent('summary');
  LPage.Add(LInstance);
  Document.AddPage(LPage);
  Runtime := RealizeNyxView(Document, LPage.Node);
  Views := NewNyxCollectionBindings(Runtime, NewNyxCollectionContext(Document.Collections));
  Scheduler := NewNyxScheduler;
  Events := NewNyxEvents(Scheduler);
  Registry := NewNyxCoordinatorRegistry;
  Factory := TSummaryFactory.Create;
  FactoryLease := Factory;
  Registry.RegisterFactory(NyxCustomKind(NyxKindName(nkCard)),
    NyxExtension(CIntent), FactoryLease);
  Target := TRefreshTarget.Create;
  TargetLease := Target;
  Baseline := TNyxCodec.Encode(Document);
end;

destructor TFixture.Destroy;
begin
  DisconnectNyxCoordinatorMounts(Mounts);

  if EventPlan <> nil then
  begin
    EventPlan.Retire;
  end;
  EventPlan := nil;

  if Events <> nil then
  begin
    Events.Close;
  end;
  Factory.Projection := nil;
  Factory.Refresh := nil;
  Factory.Last := nil;
  Registry := nil;
  Factory := nil;
  FactoryLease := nil;
  Target := nil;
  TargetLease := nil;
  Events := nil;
  Views := nil;
  Runtime.Free;
  Document.Free;

  if Scheduler <> nil then
  begin
    Scheduler.Shutdown;
  end;
  Scheduler := nil;
  inherited Destroy;
end;

function TFixture.Root(ASecond: Boolean): TNyxNode;
begin
  Result := Runtime.Find(NyxQualifiedID('first-summary', 'summary'));

  if ASecond then
  begin
    Result := Runtime.Find(NyxQualifiedID('second-summary', 'summary'));
  end;
end;

function TFixture.View(ASecond: Boolean): INyxCollectionView;
begin
  Result := Views.ViewFor(Root(ASecond).Part(NyxPart('items')).ID);
end;

function TFixture.Summary(ASecond: Boolean): ISummaryBinding;
var
  LBinding: INyxCoordinatorBinding;
begin
  LBinding := FindNyxCoordinatorMount(Mounts, IDs, Root(ASecond).ID);
  Check(Supports(LBinding, ISummaryBinding, Result),
    'generic lookup preserves the exact creator specialized binding interface');
end;

procedure TFixture.Prepare;
var
  LPreparation: INyxEventViewPreparation;
begin
  Supports(Events, INyxEventViewPreparation, LPreparation);
  EventPlan := LPreparation.PrepareView;
  PrepareNyxCoordinatorMounts(Runtime, Views, EventPlan, Scheduler,
    Registry.Snapshot, TargetLease, Mounts, IDs);
end;

procedure TFixture.Accept;
begin
  Prepare;
  PublishNyxGroup([Mounts[0], Mounts[1], EventPlan]);
end;

procedure TFixture.Click(ASecond: Boolean; AExpectObserver: Boolean);
var
  LInfo: TNyxEventInfo;
  LNode: TNyxNode;
  LExecutions: TNyxExecutions;
begin
  LNode := Root(ASecond).Part(NyxPart('clear-selection'));
  LInfo := Default(TNyxEventInfo);
  LInfo.Trigger := ntClick;
  LInfo.Name := NyxEvent(NyxTriggerName(ntClick));
  LInfo.Value := NyxNull;
  LInfo.Details := NyxNull;
  LInfo.OriginID := LNode.ID;
  LInfo.TargetID := LNode.ID;
  LInfo.SourceID := Root(ASecond).ID;
  Check(Events.On(NyxControlEvents(LNode.ID, niRuntime), ntClick).Count = 0,
    'default physical observer adds no ordinary callback registration');
  LExecutions := Events.Dispatch(LInfo, LNode.DesignID, Root(ASecond).DesignID);

  if AExpectObserver then
  begin
    Check(Length(LExecutions) = 1,
      'accepted physical Click dispatch returns exactly its observer execution ticket');
    Check(LExecutions[0].Status = nesSucceeded,
      'accepted physical observer succeeds: ' + LExecutions[0].Failure);
  end
  else
  begin
    Check(Length(LExecutions) = 0,
      'physical Click before installation cannot execute its dormant observer');
  end;
end;

procedure TIntervention.Validate;
var
  LPreparation: INyxEventViewPreparation;
  LOther: INyxPreparedEventView;
begin
  case Kind of
    ikSelect:
      begin
        Fixture.View.Select(Item);
      end;
    ikCancel:
      begin
        Fixture.EventPlan.Generation.Cancel;
      end;
    ikRevoke:
      begin
        RevokeNyxCoordinatorMounts(Fixture.Mounts);
      end;
    ikReenter:
      begin
        Supports(Fixture.Events, INyxEventViewPreparation, LPreparation);
        try
          LOther := LPreparation.PrepareView;
        except
          on Exception do
          begin
            ReentryRefused := True;
          end;
        end;
        Check(LOther = nil, 'a competing successor plan never acquires reservation authority');
      end;
  end;
end;

procedure TIntervention.Install;
begin
end;

procedure TIntervention.Notify;
begin
end;

procedure TIntervention.Retire;
begin
end;

procedure TBusyReceiver.Changed(const AView: INyxCollectionView;
  const AChanges: INyxCollectionChanges);
var
  LReadiness: INyxCollectionViewPublication;
begin
  Supports(AView, INyxCollectionViewPublication, LReadiness);
  Busy := not LReadiness.PublicationReady;
  try
    Fixture.Prepare;
  except
    on Exception do
    begin
      Rejected := True;
    end;
  end;
end;

procedure RegistryCases;
var
  LRegistry: INyxCoordinatorRegistry;
  LSnapshot: INyxCoordinatorRegistrySnapshot;
  LMutable: INyxCoordinatorRegistry;
  LFirst: INyxCoordinatorFactory;
  LSecond: INyxCoordinatorFactory;
  LRejected: Boolean;
begin
  LRegistry := NewNyxCoordinatorRegistry;
  LFirst := TSummaryFactory.Create;
  LSecond := TSummaryFactory.Create;
  Check(LRegistry.Snapshot.Count = 0, 'an empty caller registry leaves all static controls alone');
  LRegistry.RegisterFactory(NyxCustomKind(NyxKindName(nkCard)), NyxExtension(CIntent), LFirst);
  LSnapshot := LRegistry.Snapshot;
  LRejected := False;
  try
    LRegistry.RegisterFactory(NyxCustomKind(NyxKindName(nkCard)),
      NyxExtension(CIntent), LSecond);
  except
    on ENyxCoordinator do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LRegistry.Snapshot.Count = 1),
    'duplicate exact kind/key registration refuses without replacing its factory');
  LRegistry.RegisterFactory(NyxCustomKind(NyxKindName(nkCard)),
    NyxExtension(CSecondIntent), LSecond);
  Check((LSnapshot.Count = 1) and (LRegistry.Snapshot.Count = 2) and
    (LSnapshot.Factory(0) = LFirst),
    'captured membership and factory identity are independent of later registrations');
  Check((LSnapshot.Kind(0).Name = NyxKindName(nkCard)) and
    (LSnapshot.IntentKey(0).Name = CIntent),
    'snapshot discovery retains both distinct typed identities');
  Check(not Supports(LSnapshot, INyxCoordinatorRegistry, LMutable) and
    (LSnapshot.Count = 1),
    'the immutable snapshot does not expose the writable registry through Supports');
  LRejected := False;
  try
    LSnapshot.Factory(-1);
  except
    on ENyxCoordinator do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'snapshot indexing refuses a foreign registration position');
  LRejected := False;
  try
    LRegistry.RegisterFactory(NyxCustomKind(''), NyxExtension(CIntent), LFirst);
  except
    on ENyxCoordinator do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'an empty uninitialized kind cannot register behavioral authority');
  LRejected := False;
  try
    LRegistry.RegisterFactory(NyxCustomKind(NyxKindName(nkLabel)),
      NyxExtension(CIntent), nil);
  except
    on ENyxCoordinator do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'a registration without an executable managed factory refuses');
end;

procedure PreparedAndSelectionCases;
var
  LFixture: TFixture;
  LFirst: ISummaryBinding;
  LSecond: ISummaryBinding;
  LBase: INyxCoordinatorBinding;
  LExpected: INyxCoordinatorBinding;
  LRejected: Boolean;
  LPrevious: INyxCancellationScope;
  LClearID: TNyxText;
  LSpec: TNyxText;
begin
  LFixture := TFixture.Create;
  try
    LPrevious := NyxEventGeneration(LFixture.Events);
    LClearID := LFixture.Root.Part(NyxPart('clear-selection')).ID;
    LFixture.Events.On(NyxControlEvents(LClearID, niRuntime), ntClick).Policy(neUIQueue);
    LSpec := LFixture.View.Spec.ToData.ToJSON;
    LFixture.Prepare;
    Check((Length(LFixture.Mounts) = 2) and not LFixture.Events.HasSubscribers(ntClick),
      'two real reusable candidates prepare with no dormant slot visible to old input');
    Check((LFixture.Root.Part(NyxPart('clear-selection')).Prop(
      NyxAttributeName(atEnabled), 'true') = 'false') and
      (LFixture.Root.Part(NyxPart('selection-status')).Prop(
      NyxAttributeName(atText)) = 'Retained selected count: 0'),
      'first presentation and AND availability are ready before physical Build');
    LFixture.Click(False, False);
    Check((LFixture.Target.Calls = 0) and not LPrevious.Cancelled,
      'old dispatch before commit cannot activate staged default behavior or refresh');
    LRejected := False;
    try
      LFixture.Factory.Refresh.Request;
    except
      on ENyxCoordinatorCancelled do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'a dormant creator cannot refresh the accepted renderer');
    LRejected := False;
    try
      LFixture.Factory.Projection.Text(
        LFixture.Factory.Projection.Part(NyxPart('selection-status')), 'too late');
    except
      on ENyxCoordinatorCancelled do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'claims seal before Build and prevent dormant later projection writes');
    PublishNyxGroup([LFixture.Mounts[0], LFixture.Mounts[1], LFixture.EventPlan]);
    LFirst := LFixture.Summary;
    LSecond := LFixture.Summary(True);
    LBase := LFixture.Mounts[1].Binding;
    Check(Supports(LFixture.Factory.Last, INyxCoordinatorBinding, LExpected) and
      (LBase = LExpected) and
      (LFirst <> LSecond) and LFirst.Connected and LSecond.Connected,
      'accepted lookup returns the exact specialized binding and independent instances');
    Check(LPrevious.Cancelled and
      not LFixture.EventPlan.Generation.Cancelled,
      'one shared future event installation replaces only the old event generation');
    LRejected := False;
    try
      LFixture.Factory.Projection.Kind(NyxControl(LFixture.Root.ID));
    except
      on ENyxCoordinator do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'a sibling runtime identity grants no role authority in this projection');
    LFixture.View.SetSelection([Item, Item(True)], Item, Item);
    LFixture.View(True).Select(Item(True));
    Check((LFirst.Count = 2) and (LSecond.Count = 1) and
      (LFixture.Root.Part(NyxPart('selection-status')).Prop(
      NyxAttributeName(atText)) = 'Retained selected count: 2'),
      'actual multiple selection projects the retained count without choosing a row');
    LFixture.View.ConfigureQuery(Default(TNyxCollectionQuery).
      Where(NyxWhere(NyxTextField('name')).EqualTo('No matching rows')));
    Check((LFixture.View.Snapshot.Count = 0) and (LFirst.Count = 2) and
      (LSecond.Count = 1),
      'query-hidden membership remains counted by its existing selection owner');
    LFixture.Click;
    Check((LFirst.Count = 0) and (LSecond.Count = 1) and
      (LFixture.View.Spec.ToData.ToJSON = LSpec),
      'ordinary default Click clears only its own selection and preserves list specification');
    Check(LFixture.Events.On(NyxControlEvents(LClearID, niRuntime), ntClick).
      ExecutionPolicy = neUIQueue,
      'the default observer preserves the caller ordinary execution policy');
    Check((LFixture.View.Store.Snapshot.Count = 2) and
      (LFixture.Baseline = TNyxCodec.Encode(LFixture.Document)),
      'projection and Clear never rewrite collection rows or authored defaults');
    LFixture.Events.CancelPending;
    LRejected := False;
    try
      LFirst.Clear;
    except
      on ENyxCoordinatorCancelled do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and not LFirst.Connected,
      'a retained specialized command refuses a cancelled accepted generation');
    LRejected := False;
    try
      LFixture.Factory.Projection.Root;
    except
      on ENyxCoordinatorCancelled do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'generation cancellation also revokes retained projection discovery');
  finally
    LFirst := nil;
    LSecond := nil;
    LBase := nil;
    LExpected := nil;
    LFixture.Free;
  end;
end;

procedure SnapshotAndConflictCases;
var
  LFixture: TFixture;
  LSnapshot: INyxCoordinatorRegistrySnapshot;
  LOther: INyxCoordinatorFactory;
  LRejected: Boolean;
begin
  LFixture := TFixture.Create;
  try
    Check(NyxHasCoordinatorMounts(LFixture.Runtime, LFixture.Registry.Snapshot),
      'registered intent is discovered on the actual realized component instances');
    Check(not NyxHasCoordinatorMounts(LFixture.Runtime, NewNyxCoordinatorRegistry.Snapshot),
      'unknown opt-in intent alone cannot manufacture executable behavior');
    LSnapshot := LFixture.Registry.Snapshot;
    LFixture.Factory.RegisterDuringPrepare := LFixture.Registry;
    LFixture.Accept;
    Check((LFixture.Factory.Calls = 2) and (LSnapshot.Count = 1) and
      (LFixture.Registry.Snapshot.Count = 2),
      'factory reentrancy cannot alter the current candidate registration snapshot');
  finally
    LFixture.Factory.RegisterDuringPrepare := nil;
    LFixture.Free;
  end;
  LFixture := TFixture.Create;
  try
    LOther := TSummaryFactory.Create;
    LFixture.Registry.RegisterFactory(NyxCustomKind(NyxKindName(nkCard)),
      NyxExtension(CSecondIntent), LOther);
    LFixture.Root.Extensions.SetValue(NyxExtension(CSecondIntent), NyxNull);
    LRejected := False;
    try
      LFixture.Prepare;
    except
      on ENyxCoordinator do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LFixture.Factory.Calls = 0) and
      (Length(LFixture.Mounts) = 0) and (Length(LFixture.IDs) = 0),
      'competing matching intents refuse deterministically before any factory callback');
    Check(TNyxCodec.Encode(LFixture.Document) = LFixture.Baseline,
      'candidate conflict does not mutate the original reusable definition');
  finally
    LOther := nil;
    LFixture.Free;
  end;
  LFixture := TFixture.Create;
  try
    LFixture.Root.Part(NyxPart('selection-status')).Binds.
      Text(NyxTextState('external-title')).Done;
    LRejected := False;
    try
      LFixture.Prepare;
    except
      on ENyxCoordinator do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (Length(LFixture.Mounts) = 0) and
      (LFixture.Root.Part(NyxPart('selection-status')).Prop(
      NyxAttributeName(atText)) = 'Retained selected count: 0'),
      'a coordinator refuses a preexisting bound text role before projecting over its owner');
    Check(TNyxCodec.Encode(LFixture.Document) = LFixture.Baseline,
      'bound-role refusal preserves the original reusable and sibling defaults');
  finally
    LFixture.Free;
  end;
end;

procedure AncestorClaimCases;
var
  LFixture: TFixture;
  LNested: INyxCard;
  LList: INyxList;
  LStatus: INyxLabel;
  LClear: INyxButton;
  LSpec: TNyxCollectionViewSpec;
  LFactory: TAncestorClaimFactory;
  LFactoryLease: INyxCoordinatorFactory;
  LRejectedProjection: INyxCoordinatorProjection;
  LParticipants: TNyxPreparedPublications;
  LNestedView: INyxCollectionView;
  LNestedBinding: ISummaryBinding;
  LBinding: INyxCoordinatorBinding;
  LRejected: Boolean;
  LIndex: Integer;
  LNestedID: TNyxText;
begin
  LFixture := TFixture.Create;
  LFactory := nil;
  try
    { Extend the existing authored reusable with real roles and real views.
      The two rendered instances still use their existing collection owner. }
    LSpec := LFixture.View.Spec.Copy;
    LNested := NewNyxCard('nested-summary', ncoDescriptor);
    LNested.Configure.PartName(NyxPart('nested')).Done;
    LList := NewNyxList('nested-items');
    LList.Configure.PartName(NyxPart('items')).Done;
    LList.Binds.Collection(LSpec).Done;
    LNested.Add(LList);
    LStatus := NewNyxLabel('nested-selection-status');
    LStatus.Text := 'Retained selected count: 0';
    LStatus.Configure.PartName(NyxPart('selection-status')).Done;
    LNested.Add(LStatus);
    LClear := NewNyxButton('nested-clear-selection');
    LClear.Text := 'Clear selection';
    LClear.Configure.PartName(NyxPart('clear-selection')).
      AccessibleName('Clear selection').Done;
    LNested.Add(LClear);
    LNested.Extensions.SetValue(NyxExtension(CIntent),
      NyxObject([NyxField('version', NyxData(1))]));
    RetainNyxControl(LFixture.Document.Find('summary')).Add(LNested);
    LFixture.Views := nil;
    FreeAndNil(LFixture.Runtime);
    LFixture.Runtime := RealizeNyxView(LFixture.Document, LFixture.Document.Find('home'));
    LFixture.Views := NewNyxCollectionBindings(LFixture.Runtime,
      NewNyxCollectionContext(LFixture.Document.Collections));
    LFixture.Baseline := TNyxCodec.Encode(LFixture.Document);
    LNestedID := LFixture.Root.Part(NyxPart('nested')).ID;
    LFactory := TAncestorClaimFactory.Create(LFixture.FactoryLease,
      NyxControl(LFixture.Root.ID));
    LFactoryLease := LFactory;
    LFixture.Registry := NewNyxCoordinatorRegistry;
    LFixture.Registry.RegisterFactory(NyxCustomKind(NyxKindName(nkCard)),
      NyxExtension(CIntent), LFactoryLease);
    LRejected := False;
    try
      LFixture.Prepare;
    except
      on ENyxCoordinator do
      begin
        LRejected := True;
      end;
    end;
    LRejectedProjection := LFactory.Claimed;
    Check(LRejected and (LFixture.Factory.Calls = 2) and
      (Length(LFixture.Mounts) = 0) and (Length(LFixture.IDs) = 0),
      'ancestor and descendant cannot claim one real label even with equal initial text');
    Check((LFixture.Root.Part(NyxPart('selection-status')).Prop(
      NyxAttributeName(atText)) = 'Retained selected count: 0') and
      (LFixture.Root.Part(NyxPart('nested/selection-status')).Prop(
      NyxAttributeName(atText)) = 'Retained selected count: 0') and
      (LFixture.Root.Part(NyxPart('clear-selection')).EnabledPolicyCount = 0) and
      (LFixture.Root.Part(NyxPart('nested/clear-selection')).EnabledPolicyCount = 0),
      'refused nested ownership leaves equal text intact and detaches all candidate policies');
    LRejected := False;
    try
      LRejectedProjection.Text(NyxControl(
        LFixture.Root.Part(NyxPart('nested/selection-status')).ID), 'stale ancestor');
    except
      on ENyxCoordinatorCancelled do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (TNyxCodec.Encode(LFixture.Document) = LFixture.Baseline),
      'retained refused ancestor projection has no authority and authored defaults stay exact');
    LFixture.EventPlan.Retire;
    LFixture.EventPlan := nil;
    LFactory.ClaimAncestor := False;
    { A fresh group uses the same realized candidate after refusal. No private
      claim table or fabricated data-view implementation participates. }
    LFixture.Prepare;
    Check(Length(LFixture.Mounts) = 4,
      'released candidate claims allow both outer and both nested real factories to prepare');
    SetLength(LParticipants, Length(LFixture.Mounts) + 1);
    for LIndex := 0 to High(LFixture.Mounts) do
    begin
      LParticipants[LIndex] := LFixture.Mounts[LIndex];
    end;
    LParticipants[High(LParticipants)] := LFixture.EventPlan;
    PublishNyxGroup(LParticipants);
    for LIndex := 0 to High(LFixture.Mounts) do
    begin
      Check(LFixture.Mounts[LIndex].Binding.Connected,
        'each real nested/outer creator owns its admitted generation after conflict release');
    end;
    LBinding := FindNyxCoordinatorMount(LFixture.Mounts, LFixture.IDs, LNestedID);
    Check(Supports(LBinding, ISummaryBinding, LNestedBinding),
      'nested successful mount retains the exact specialized creator binding');
    LNestedView := LFixture.Views.ViewFor(LFixture.Root.Part(NyxPart('nested/items')).ID);
    LNestedView.Select(Item);
    Check((LNestedBinding.Count = 1) and
      (LFixture.Root.Part(NyxPart('nested/selection-status')).Prop(
      NyxAttributeName(atText)) = 'Retained selected count: 1') and
      (LFixture.Root.Part(NyxPart('selection-status')).Prop(
      NyxAttributeName(atText)) = 'Retained selected count: 0') and
      (TNyxCodec.Encode(LFixture.Document) = LFixture.Baseline),
      'nested selection projects only its owned label without rewriting outer or authored state');
  finally
    LBinding := nil;
    LNestedBinding := nil;
    LNestedView := nil;
    LParticipants := nil;
    LRejectedProjection := nil;

    if LFactory <> nil then
    begin
      LFactory.Claimed := nil;
    end;
    LFactoryLease := nil;
    LNested := nil;
    LList := nil;
    LStatus := nil;
    LClear := nil;
    LFixture.Free;
  end;
end;

procedure FenceAndRefusalCases;
var
  LFixture: TFixture;
  LIntervention: TIntervention;
  LParticipant: INyxPreparedPublication;
  LKind: TInterventionKind;
  LPrevious: INyxCancellationScope;
  LRejected: Boolean;
begin
  for LKind := ikSelect to ikReenter do
  begin
    LFixture := TFixture.Create;
    try
      LPrevious := NyxEventGeneration(LFixture.Events);
      LFixture.Prepare;
      LIntervention := TIntervention.Create;
      LParticipant := LIntervention;
      LIntervention.Fixture := LFixture;
      LIntervention.Kind := LKind;
      LRejected := False;
      try
        PublishNyxGroup([LFixture.Mounts[0], LFixture.Mounts[1],
          LFixture.EventPlan, LParticipant]);
      except
        on Exception do
        begin
          LRejected := True;
        end;
      end;

      if LKind = ikReenter then
      begin
        Check(not LRejected and LIntervention.ReentryRefused and LPrevious.Cancelled,
          'a refused reentrant event reservation preserves the admitted original candidate');
        Check(LFixture.Summary.Connected, 'reentrant refusal cannot replace the accepted binding');
      end
      else
      begin
        Check(LRejected and not LPrevious.Cancelled and
          LFixture.EventPlan.Generation.Cancelled,
          'later selection/cancellation/revocation is fenced after all group validators');
        Check(not LFixture.Factory.Last.Connected and (LFixture.Target.Calls = 0),
          'refused candidates retain no active callback or refresh authority');
      end;
      Check(TNyxCodec.Encode(LFixture.Document) = LFixture.Baseline,
        'final-fence refusal or reentrancy preserves exact authored source defaults');
    finally
      LParticipant := nil;
      LPrevious := nil;
      LFixture.Free;
    end;
  end;
  LFixture := TFixture.Create;
  try
    LFixture.Factory.FailPrepare := True;
    LRejected := False;
    try
      LFixture.Prepare;
    except
      on ENyxCoordinator do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (Length(LFixture.Mounts) = 0),
      'factory refusal publishes no partial output vector');
    LRejected := False;
    try
      LFixture.Factory.Projection.Root;
    except
      on ENyxCoordinatorCancelled do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'a factory-retained projection is revoked even if Prepare raises');
  finally
    LFixture.Free;
  end;
end;

procedure ReplacementCases;
var
  LFixture: TFixture;
  LCandidate: TNyxNode;
  LViews: INyxCollectionBindings;
  LPreparation: INyxEventViewPreparation;
  LEventPlan: INyxPreparedEventView;
  LMounts: TNyxCoordinatorMounts;
  LIDs: TNyxCoordinatorMountIDs;
  LOld: ISummaryBinding;
  LNext: ISummaryBinding;
  LPrevious: INyxCancellationScope;
  LBinding: INyxCoordinatorBinding;
  LRejected: Boolean;
begin
  LFixture := TFixture.Create;
  LCandidate := nil;
  try
    LFixture.Accept;
    LOld := LFixture.Summary;
    LFixture.View.Select(Item);
    LPrevious := NyxEventGeneration(LFixture.Events);
    LCandidate := RealizeNyxView(LFixture.Document, LFixture.Document.Find('home'));
    LViews := LFixture.Views.Recompose(LCandidate);
    Supports(LFixture.Events, INyxEventViewPreparation, LPreparation);
    LEventPlan := LPreparation.PrepareView;
    LFixture.Factory.FailValidate := True;
    PrepareNyxCoordinatorMounts(LCandidate, LViews, LEventPlan, LFixture.Scheduler,
      LFixture.Registry.Snapshot, LFixture.TargetLease, LMounts, LIDs);
    LRejected := False;
    try
      PublishNyxGroup([LMounts[0], LMounts[1], LEventPlan]);
    except
      on ENyxCoordinator do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and not LPrevious.Cancelled and LOld.Connected and (LOld.Count = 1),
      'a refused detached replacement preserves the actual old view, input and generation');
    LFixture.Click;
    Check(LOld.Count = 0, 'old physical dispatch remains useful after replacement refusal');
    DisconnectNyxCoordinatorMounts(LMounts);
    LEventPlan := nil;
    LViews := nil;
    LCandidate.Free;
    LCandidate := nil;
    LFixture.Factory.FailValidate := False;
    LCandidate := RealizeNyxView(LFixture.Document, LFixture.Document.Find('home'));
    LViews := LFixture.Views.Recompose(LCandidate);
    LEventPlan := LPreparation.PrepareView;
    PrepareNyxCoordinatorMounts(LCandidate, LViews, LEventPlan, LFixture.Scheduler,
      LFixture.Registry.Snapshot, LFixture.TargetLease, LMounts, LIDs);
    LBinding := FindNyxCoordinatorMount(LMounts, LIDs, LFixture.Root.ID);
    Check(Supports(LBinding, ISummaryBinding, LNext),
      'same-ID replacement still exposes the new exact specialized binding');
    PublishNyxGroup([LMounts[0], LMounts[1], LEventPlan]);
    Check(LPrevious.Cancelled and not LOld.Connected and LNext.Connected,
      'accepted same-ID replacement revokes the old command generation independently of identity');
    LRejected := False;
    try
      LOld.Clear;
    except
      on ENyxCoordinatorCancelled do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'retaining the old custom interface cannot target the replacement store');
    Check(TNyxCodec.Encode(LFixture.Document) = LFixture.Baseline,
      'replacement and refusal preserve the full authored reusable document');
  finally
    DisconnectNyxCoordinatorMounts(LMounts);

    if LEventPlan <> nil then
    begin
      LEventPlan.Retire;
    end;
    LEventPlan := nil;
    LBinding := nil;
    LOld := nil;
    LNext := nil;
    LViews := nil;
    LPreparation := nil;
    LCandidate.Free;
    LFixture.Free;
  end;
end;

procedure BusyAndAbandonCases;
var
  LFixture: TFixture;
  LReceiver: TBusyReceiver;
  LToken: INyxCollectionViewSubscription;
  LPrevious: INyxCancellationScope;
  LProjection: INyxCoordinatorProjection;
  LRejected: Boolean;
begin
  LFixture := TFixture.Create;
  LReceiver := TBusyReceiver.Create;
  try
    LReceiver.Fixture := LFixture;
    LPrevious := NyxEventGeneration(LFixture.Events);
    LToken := LFixture.View.Subscribe(LReceiver.Changed);
    LFixture.View.Select(Item);
    Check(LReceiver.Busy and LReceiver.Rejected and (Length(LFixture.Mounts) = 0),
      'real busy view notification refuses candidate subscription/admission without a fake view');
    Check(not LPrevious.Cancelled and (LFixture.View.Selection.Count = 1),
      'a refused busy candidate cannot cancel or roll back the actual old selection owner');
  finally

    if LToken <> nil then
    begin
      LToken.Disconnect;
    end;
    LToken := nil;
    LReceiver.Free;
    LFixture.Free;
  end;
  LFixture := TFixture.Create;
  try
    LPrevious := NyxEventGeneration(LFixture.Events);
    LFixture.Prepare;
    LProjection := LFixture.Factory.Projection;
    RetireNyxCoordinatorMounts(LFixture.Mounts);
    Check(not LPrevious.Cancelled and not LFixture.Events.HasSubscribers(ntClick),
      'abandoning the prepared mounts leaves old input and generation available');
    LRejected := False;
    try
      LProjection.Part(NyxPart('selection-status'));
    except
      on ENyxCoordinatorCancelled do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'explicit abandoned retirement revokes every retained projection capability');
    LFixture.EventPlan.Retire;
    Check(LFixture.EventPlan.Generation.Cancelled and not LPrevious.Cancelled,
      'abandoning the future event reservation cancels only the unused successor');
    Check(TNyxCodec.Encode(LFixture.Document) = LFixture.Baseline,
      'abandonment preserves original definitions, siblings and collection defaults');
  finally
    LProjection := nil;
    LPrevious := nil;
    LFixture.Free;
  end;
end;

procedure CommittedAndLifetimeCases;
var
  LFixture: TFixture;
  LFirst: ISummaryBinding;
  LSecond: ISummaryBinding;
  LProjection: INyxCoordinatorProjection;
  LRefresh: INyxCoordinatorRefresh;
  LRejected: Boolean;
  LCalls: Integer;
begin
  LFixture := TFixture.Create;
  try
    LFixture.Factory.FailNotify := True;
    LFixture.Prepare;
    LFirst := LFixture.Summary;
    LSecond := LFixture.Summary(True);
    LRejected := False;
    try
      PublishNyxGroup([LFixture.Mounts[0], LFixture.Mounts[1], LFixture.EventPlan]);
    except
      on ENyxPublicationNotification do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and LFirst.Connected and LSecond.Connected and
      (LFirst.Notifications = 1) and (LSecond.Notifications = 1),
      'postcommit failure attempts every creator Notify and preserves installed bindings');
    LFixture.View.Select(Item);
    Check((LFirst.Count = 1) and (LFixture.Target.Calls = 1),
      'committed-error controls still project live selection through the accepted scope');
    LProjection := LFixture.Factory.Projection;
    LRefresh := LFixture.Factory.Refresh;
    DisconnectNyxCoordinatorMounts(LFixture.Mounts);
    Check(not LFirst.Connected and not LSecond.Connected and
      (LFirst.Disconnections = 1) and (LSecond.Disconnections = 1),
      'explicit disconnection retires every specialized owner exactly once');
    Check(LFixture.Root.Part(NyxPart('clear-selection')).EnabledPolicyCount = 0,
      'owned AND policies detach without altering authored Enabled values');
  finally
    LFirst := nil;
    LSecond := nil;
    LFixture.Free;
  end;
  LRejected := False;
  try
    LProjection.Root;
  except
    on ENyxCoordinatorCancelled do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'retained projection is inert after renderer-style ownership and tree disposal');
  LRejected := False;
  try
    LRefresh.Request;
  except
    on ENyxCoordinatorCancelled do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'retained guarded refresh cannot reach a disposed weak host');
  LProjection := nil;
  LRefresh := nil;
  LFixture := TFixture.Create;
  try
    LFixture.Factory.FailDisconnect := True;
    LFixture.Accept;
    LFirst := LFixture.Summary;
    LSecond := LFixture.Summary(True);
    LRejected := False;
    try
      DisconnectNyxCoordinatorMounts(LFixture.Mounts);
    except
      on ENyxCoordinator do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (Length(LFixture.Mounts) = 0) and
      (LFirst.Disconnections = 1) and (LSecond.Disconnections = 1),
      'fallible cleanup attempts every mount and detaches the caller vector before reporting');
    Check((LFixture.Root.Part(NyxPart('clear-selection')).EnabledPolicyCount = 0) and
      (LFixture.Root(True).Part(NyxPart('clear-selection')).EnabledPolicyCount = 0),
      'creator cleanup errors cannot skip independent policy retirement');
  finally
    LFirst := nil;
    LSecond := nil;
    LFixture.Free;
  end;
  LFixture := TFixture.Create;
  try
    LFixture.Accept;
    LFirst := LFixture.Summary;
    GRelease := LFixture.Mounts;
    LFixture.Mounts := nil;
    LFixture.Target.ReleaseMounts := True;
    LFixture.View.Select(Item);
    LCalls := LFixture.Target.Calls;
    Check((Length(GRelease) = 0) and not LFirst.Connected and (LCalls = 1),
      'last-call weak refresh can disconnect/release all mounts during the real view callback');
    LFixture.View.ClearSelection;
    Check((LFixture.Target.Calls = LCalls) and
      (TNyxCodec.Encode(LFixture.Document) = LFixture.Baseline),
      'later source notifications cannot reenter the retired creator or mutate defaults');
  finally
    LFirst := nil;
    LFixture.Free;
  end;
  LFixture := TFixture.Create;
  try
    LFixture.Factory.ReleaseOnNotify := True;
    LFixture.Prepare;
    LFirst := LFixture.Summary;
    LSecond := LFixture.Summary(True);
    GRelease := LFixture.Mounts;
    LFixture.Mounts := nil;
    PublishNyxGroup([GRelease[0], GRelease[1], LFixture.EventPlan]);
    Check((Length(GRelease) = 0) and (LFirst.Notifications = 1) and
      (LSecond.Notifications = 1),
      'first creator Notify can release all mounts while every independent Notify is still attempted');
    Check(not LFirst.Connected and not LSecond.Connected and
      (LFirst.Disconnections = 1) and (LSecond.Disconnections = 1),
      'notification-triggered disposal preserves revocation and once-only cleanup');
    Check(TNyxCodec.Encode(LFixture.Document) = LFixture.Baseline,
      'all-attempt postcommit notification cleanup preserves authored collection defaults');
  finally
    DisconnectNyxCoordinatorMounts(GRelease);
    LFirst := nil;
    LSecond := nil;
    LFixture.Free;
  end;
end;

{ The first failure owns the diagnostic even when it has no text or no object.
  The same two installed reusable instances retain independent real scopes;
  subsequent failures cannot skip their disconnects or redirect the error. }
procedure EmptyDiagnosticCleanupCases;
const
  CExpectedDiagnostic: TNyxText = 'Coordinator vector retired; cleanup failed: ' +
    'Coordinator disconnected; cleanup failed: ';
var
  LFixture: TFixture;
  LFirst: ISummaryBinding;
  LSecond: ISummaryBinding;
  LRejected: Boolean;
begin
  LFixture := TFixture.Create;
  try
    LFixture.Factory.FirstCleanupFault := cfEmptyPascal;
    LFixture.Factory.FailDisconnect := True;
    LFixture.Accept;
    LFirst := LFixture.Summary;
    LSecond := LFixture.Summary(True);
    LRejected := False;
    try
      DisconnectNyxCoordinatorMounts(LFixture.Mounts);
    except
      on LException: ENyxCoordinator do
      begin
        { The mount and vector keep their established typed context prefixes.
          The first creator's empty text remains empty beneath BOTH layers;
          the second creator's nonempty diagnostic must not replace it. }
        LRejected := TNyxText(LException.Message) = CExpectedDiagnostic;
      end;
    end;
    Check(LRejected and (Length(LFixture.Mounts) = 0) and
      (LFirst.Disconnections = 1) and (LSecond.Disconnections = 1),
      'empty first Pascal diagnostic is reported after both installed owners disconnect');
    Check((LFixture.Root.Part(NyxPart('clear-selection')).EnabledPolicyCount = 0) and
      (LFixture.Root(True).Part(NyxPart('clear-selection')).EnabledPolicyCount = 0) and
      (TNyxCodec.Encode(LFixture.Document) = LFixture.Baseline),
      'later cleanup failure cannot replace an empty first diagnostic or retain sibling policies');
  finally
    LFirst := nil;
    LSecond := nil;
    LFixture.Free;
  end;
end;

{$ifdef PAS2JS}
procedure PlatformCleanupCases;
var
  LKind: TPlatformFailureKind;
  LFixture: TFixture;
  LFirst: ISummaryBinding;
  LSecond: ISummaryBinding;
  LPayload: JSValue;
  LObserved: JSValue;
  LObject: TJSObject;
  LCaught: Boolean;
begin
  for LKind := Low(TPlatformFailureKind) to High(TPlatformFailureKind) do
  begin
    LObject := nil;
    case LKind of
      pfUndefined: LPayload := Undefined;
      pfNull: LPayload := Null;
      pfObject:
      begin
        LObject := TJSObject.new;
        LObject['message'] := 'Original platform cleanup failure / 🌙';
        LPayload := LObject;
      end;
    end;
    LFixture := TFixture.Create;
    try
      LFixture.Factory.FirstCleanupFault := cfPlatform;
      LFixture.Factory.CleanupFault := cfEmptyPascal;
      LFixture.Factory.PlatformFailure := LPayload;
      LFixture.Accept;
      LFirst := LFixture.Summary;
      LSecond := LFixture.Summary(True);
      LCaught := False;
      LObserved := Undefined;
      try
        DisconnectNyxCoordinatorMounts(LFixture.Mounts);
      except
        LCaught := True;
        LObserved := JSExceptValue;
      end;
      case LKind of
        pfUndefined: Check(LCaught and isUndefined(LObserved),
          'undefined first platform throw survives subsequent Pascal cleanup failure');
        pfNull: Check(LCaught and isNull(LObserved),
          'null first platform throw remains distinct from no failure');
        pfObject: Check(LCaught and isObject(LObserved) and
          (TJSObject(LObserved) = LObject),
          'object platform throw retains exact identity after all cleanup');
      end;
      Check((Length(LFixture.Mounts) = 0) and not LFirst.Connected and
        not LSecond.Connected and (LFirst.Disconnections = 1) and
        (LSecond.Disconnections = 1),
        'raw platform failure still disconnects both independent installed owners exactly once');
      Check((LFixture.Root.Part(NyxPart('clear-selection')).EnabledPolicyCount = 0) and
        (LFixture.Root(True).Part(NyxPart('clear-selection')).EnabledPolicyCount = 0) and
        (TNyxCodec.Encode(LFixture.Document) = LFixture.Baseline),
        'platform cleanup failure retires both policy scopes and preserves authored defaults');
    finally
      LFirst := nil;
      LSecond := nil;
      LFixture.Free;
    end;
  end;
end;
{$endif}

begin
  try
    RegistryCases;
    PreparedAndSelectionCases;
    SnapshotAndConflictCases;
    AncestorClaimCases;
    FenceAndRefusalCases;
    ReplacementCases;
    BusyAndAbandonCases;
    CommittedAndLifetimeCases;
    EmptyDiagnosticCleanupCases;
    {$ifdef PAS2JS}PlatformCleanupCases;{$endif}
    {$ifdef PAS2JS}
    document.body.setAttribute('data-coordinators', 'passed');
    document.body.setAttribute('data-coordinators-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' portable coordinator checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-coordinators', 'failed');
      document.body.setAttribute('data-coordinators-checks', IntToStr(GChecks));
      document.body.setAttribute('data-coordinators-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message, ' at check count ', GChecks);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
