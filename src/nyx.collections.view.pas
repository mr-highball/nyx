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
unit nyx.collections.view;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  SysUtils,
  nyx.text,
  nyx.resources,
  nyx.resource.context,
  nyx.resources.rows,
  nyx.state,
  nyx.publication,
  nyx.data,
  nyx.contract,
  nyx.collections,
  nyx.collections.query,
  nyx.collections.query.view,
  nyx.collections.registry,
  nyx.collections.view.types,
  nyx.collections.selection;

const
  NyxMaximumCollectionViewInstances = 1024;

type
  INyxCollectionView = interface;
  { Optional explicit readiness for structural view retirement. Selection/query
    notifications can be busy even while their underlying store is idle.
    Unknown extension views do not imply readiness; they can implement this
    capability while preserving the existing collection-view interface ABI. }
  INyxCollectionViewPublication = interface(IInterface)
    ['{1B0B4B83-1B44-42C7-AEE4-9737A7BC331D}']
    function PublicationReady: Boolean;
  end;
  TNyxCollectionViewObserver = procedure(const AView: INyxCollectionView;
    const AChanges: INyxCollectionChanges) of object;

  { The owner retains this managed token and disconnects it before freeing the
    callback receiver. Tokens borrow a view, and views borrow token pointers.
    Releasing a token disconnects it; destroying a view detaches every token. }
  INyxCollectionViewSubscription = interface
    ['{E23AB366-740C-47F1-ACBB-5763C083DCBA}']
    function GetConnected: Boolean;
    procedure Disconnect;
    property Connected: Boolean read GetConnected;
  end;

  { Optional tree capability, obtained with NyxTreeHierarchy. It retains the
    runtime view, never a document or host widget. Closed branches are the default.
    Exact scoped identities retain disclosure through moves, reparenting and
    query displacement; actual removal retires it. Leaf disclosure is a no-op.
    Commands publish once through the view's existing ordered subscriptions;
    nil Changes also denotes disclosure. Reentrant mutation refuses, and observer
    failures report after publication. Calls remain confined to the UI thread.
    VisibleItems returns an independent preorder of currently disclosed items.
    Collapsing returns a hidden cursor to its visible ancestor, preserving both
    membership and anchor. Explicit view focus reveals its ancestors. }
  INyxTreeHierarchy = interface
    ['{B9D00F36-FD30-4F2D-93F8-E820A056DFE7}']
    { Monotonic accepted-command stamp, including silent no-ops and explicit
      focus. Adapters use it to refuse an older queued physical proposal after
      a newer application intent. It is not a store/document revision. }
    function GetCommandSerial: Integer;
    function HasChildren(const AItem: TNyxItemRef): Boolean;
    function IsExpanded(const AItem: TNyxItemRef): Boolean;
    function SetExpanded(const AItem: TNyxItemRef;
      AExpanded: Boolean): INyxTreeHierarchy;
    function ExpandAll: INyxTreeHierarchy;
    function CollapseAll: INyxTreeHierarchy;
    function VisibleItems: TNyxItemRefs;
    property CommandSerial: Integer read GetCommandSerial;
  end;

  { Live portable data view. Owns its store/snapshot/specification and one store
    subscription without retaining a renderer, document or application. Multiple
    observers are ordered; nil Changes denotes selection/query/disclosure.
    Selection owns scoped item identities independently of focus and anchor.
    Moves retain membership; removals prune missing identities and preserve a
    nearby keyboard cursor. Single is the compatible default, Multiple is an
    explicit view choice. All commands are confined to the UI thread.
    Reentrant mutation/subscription during view notification rejects. Observer
    failure follows publication and raises ENyxCollectionNotification.
    Native controls/DOM decode wire text only through EditWire; application code
    supplies typed values or typed store edits. Read-only columns reject edits. }
  INyxCollectionView = interface
    ['{2285D4A0-E723-40F5-B8C6-F1B75B0379EB}']
    function GetSpec: TNyxCollectionViewSpec;
    function GetSnapshot: INyxCollectionSnapshot;
    function GetStore: INyxCollection;
    function GetProjection: TNyxCollectionProjection;
    function GetHasSelection: Boolean;
    function GetSelected: TNyxItemRef;
    function GetSelection: INyxCollectionSelection;
    function GetQuery: TNyxCollectionQuery;
    { Admit one independent runtime policy before publishing. Invalid fields or
      families leave query, snapshot and selection exact and notify nobody.
      Equal policies are a no-op. Hidden selection membership is retained;
      keyboard focus stays on a visible row. Store/defaults remain unchanged. }
    function ConfigureQuery(const APolicy: TNyxCollectionQuery): INyxCollectionView;
    function ParentIndex(AIndex: Integer): Integer;
    { Read accepted text for any source identity, including a hidden selected
      member. Rendering/ranges use Snapshot visibility; missing/foreign source
      references and invalid columns still refuse. }
    function CellText(const AItem: TNyxItemRef; AColumn: Integer): TNyxText;
    procedure Select(const AItem: TNyxItemRef); overload;
    { Replace, toggle, range/additive range or focus-only. Dataset order defines
      programmatic ranges; target adapters provide their visible hierarchy below. }
    procedure Select(const AItem: TNyxItemRef; AAction: TNyxSelectionAction); overload;
    { Validate the whole candidate before publishing once. Unknown/foreign or
      duplicate items, invalid focus/anchor and multiple items in Single reject
      without changing state or notifying. Equal membership/focus/anchor is a no-op. }
    procedure SetSelection(const AItems: array of TNyxItemRef;
      const AFocus, AAnchor: TNyxItemRef);
    { Multiple only, replaces membership with the complete query result rather
      than only its viewport. Filtered-out source rows are not selected by it. }
    procedure SelectAll;
    { Range in the admitted visible ordering, excluding collapsed descendants.
      Add=True preserves discontiguous membership; an invisible anchor restarts
      the range at the target. Duplicate/foreign visible identities reject. }
    procedure SelectRange(const AItem: TNyxItemRef;
      const AVisibleOrder: array of TNyxItemRef; AAdd: Boolean);
    procedure ClearSelection;
    procedure Edit(const AItem: TNyxItemRef; AColumn: Integer;
      const AValue: TNyxStateValue);
    procedure EditWire(const AItem: TNyxItemRef; AColumn: Integer;
      const AValue: TNyxText);
    procedure Apply(const AEdits: array of TNyxCollectionEdit;
      AExpectedRevision: Integer = -1);
    function Subscribe(AObserver: TNyxCollectionViewObserver): INyxCollectionViewSubscription;
    property Spec: TNyxCollectionViewSpec read GetSpec;
    property Snapshot: INyxCollectionSnapshot read GetSnapshot;
    property Store: INyxCollection read GetStore;
    property Projection: TNyxCollectionProjection read GetProjection;
    property HasSelection: Boolean read GetHasSelection;
    property Selected: TNyxItemRef read GetSelected;
    property Selection: INyxCollectionSelection read GetSelection;
    property QueryPolicy: TNyxCollectionQuery read GetQuery;
  end;

  { Application lifetime scope resolver. Shared stores initialize once; each
    instance/key pair initializes independently from authored defaults, even if
    the application store has changed. Exact, separately stored scope/key names
    prevent concatenation collisions. Repeated resolution preserves local data
    through page/view navigation. Returned stores can outlive this context.
    AInstanceID is the admitted reusable owner's runtime identity, not a row ID;
    an empty owner rejects instance scope. At most 1024 local pairs are retained. }
  INyxCollectionContext = interface
    ['{B73BA7B9-384E-433D-87ED-6F2CDA23066A}']
    function Resolve(const ASpec: TNyxCollectionViewSpec;
      const AInstanceID: TNyxText): INyxCollection;
    function GetCollections: INyxCollections;
    property Collections: INyxCollections read GetCollections;
  end;

type
  { Source-aware detached admission, preserving the original interface/GUID. }
  INyxResourceCollectionContext = interface
    ['{312BB128-932D-46A5-815B-62A80DD392F5}']
    procedure ValidateResources(const AResources: INyxResources;
      const ALocale, AFallback: TNyxLocaleRef);
  end;

  { Optional coordinated reload capability. Changed mapped datasets update the
    application store and every already resolved instance/key pair together.
    Unchanged source rows preserve runtime edits. New instance seeds advance in
    the same install phase; new scope creation refuses until retirement. }
  INyxPreparedResourceCollectionContext = interface
    ['{67D1210C-0A8A-4B6B-9BFC-EA1CF0FA653D}']
    function PrepareResources(const AResources: INyxResources;
      const ALocale, AFallback: TNyxLocaleRef): INyxPreparedPublication;
  end;

function PrepareNyxCollectionContextResources(const AContext: INyxCollectionContext;
  const AResources: INyxResources; const ALocale, AFallback: TNyxLocaleRef): INyxPreparedPublication;

procedure ValidateNyxCollectionContextResources(const AContext: INyxCollectionContext;
  const AResources: INyxResources; const ALocale, AFallback: TNyxLocaleRef);

function NewNyxCollectionView(const AStore: INyxCollection;
  const ASpec: TNyxCollectionViewSpec;
  AProjection: TNyxCollectionProjection): INyxCollectionView;
{ Refuses nil/non-tree/alternative views without the optional capability. The
  existing collection interface remains unchanged for alternative implementations. }
function NyxTreeHierarchy(const AView: INyxCollectionView): INyxTreeHierarchy;
function NewNyxCollectionContext(
  const ADefaults: INyxCollectionDefaults): INyxCollectionContext; overload;
{ Resource-backed seeds resolve into independent application/instance stores at
  the explicit locale. This initialization does not attach a loading service. }
function NewNyxCollectionContext(const ADefaults: INyxCollectionDefaults;
  const AResources: INyxResources; const ALocale, AFallback: TNyxLocaleRef): INyxCollectionContext; overload;
{ Capture one accepted immutable frame, including explicit initial locale.
  Nil refuses; materialization retains no frame/document backreference. }
function NewNyxCollectionContext(const ADefaults: INyxCollectionDefaults;
  const AContext: INyxResourceContext): INyxCollectionContext; overload;
{ Admit a saved/default projection without constructing a mutable store or
  attaching a subscription. The same dataset validator serves live views. }
procedure ValidateNyxCollectionViewSnapshot(const ASnapshot: INyxCollectionSnapshot;
  const ASpec: TNyxCollectionViewSpec; AProjection: TNyxCollectionProjection);
{ False for nil/unknown views or active view/store notification. This is a
  synchronous readiness observation, never a reservation or admission cache. }
function NyxCollectionViewPublicationReady(const AView: INyxCollectionView): Boolean;

implementation

type
  TView = class;
  TViewSubscription = class(TInterfacedObject, INyxCollectionViewSubscription)
  public
    Owner: TView;
    Serial: Integer;
    Observer: TNyxCollectionViewObserver;
    destructor Destroy; override;
    function GetConnected: Boolean;
    procedure Disconnect;
  end;
  TParentIndexes = TNyxQueryIndexes;

  TView = class(TInterfacedObject, INyxCollectionView, INyxTreeHierarchy,
    INyxCollectionViewPublication)
  private
    FStore: INyxCollection;
    FAtomic: INyxAtomicCollection;
    FPrepared: TView; { detached, privately owned projection; never subscribed }
    FSource: INyxCollectionSnapshot;
    FSnapshot: INyxCollectionSnapshot;
    FQuery: TNyxCollectionQuery;
    FSpec: TNyxCollectionViewSpec;
    FProjection: TNyxCollectionProjection;
    FParents: TParentIndexes;
    { Disclosure indexes the complete source. Adjacency indexes the query result;
      neither array contains a widget, tree node, callback or managed back-link. }
    FExpanded: array of Boolean;
    FFirstChild: TParentIndexes;
    FNextSibling: TParentIndexes;
    FTreeCommandSerial: Integer;
    FSelection: INyxCollectionSelection;
    FStoreToken: INyxCollectionSubscription;
    FTokens: array of TViewSubscription;
    FNextSerial: Integer;
    FNotifying: Boolean;
    procedure Writable;
    procedure ValidateDataset(const AData: INyxCollectionSnapshot;
      out AParents: TParentIndexes);
    procedure ValidateCandidate(const AData: INyxCollectionSnapshot;
      const AChanges: INyxCollectionChanges);
    procedure PrepareCandidate(const AData: INyxCollectionSnapshot;
      const AChanges: INyxCollectionChanges);
    procedure InstallCandidate(const AData: INyxCollectionSnapshot;
      const AChanges: INyxCollectionChanges);
    procedure RetireCandidate;
    procedure ProjectChanges(const AChanges: INyxCollectionChanges);
    procedure StoreChanged(const AStore: INyxCollection;
      const AChanges: INyxCollectionChanges);
    procedure Notify(const AChanges: INyxCollectionChanges);
    procedure Disconnect(AToken: TViewSubscription);
    function ReconcileSelection(const ASource, AResult,
      APrevious: INyxCollectionSnapshot): INyxCollectionSelection;
    procedure BuildHierarchy;
    function TreeIndex(const AItem: TNyxItemRef): Integer;
    procedure ReconcileTreeFocus;
    function RevealAncestors(const AFocus: TNyxItemRef): Boolean;
    procedure AdvanceTreeCommand;
  public
    { TView is an implementation-only type. The temporary validator has no
      subscriptions and remains owned solely by its unit-local caller. }
    constructor CreateValidator(const ASpec: TNyxCollectionViewSpec;
      AProjection: TNyxCollectionProjection);
    constructor Create(const AStore: INyxCollection;
      const ASpec: TNyxCollectionViewSpec; AProjection: TNyxCollectionProjection);
    destructor Destroy; override;
    function GetSpec: TNyxCollectionViewSpec;
    function GetSnapshot: INyxCollectionSnapshot;
    function GetStore: INyxCollection;
    function GetProjection: TNyxCollectionProjection;
    function GetHasSelection: Boolean;
    function GetSelected: TNyxItemRef;
    function GetSelection: INyxCollectionSelection;
    function PublicationReady: Boolean;
    function GetQuery: TNyxCollectionQuery;
    function ConfigureQuery(const APolicy: TNyxCollectionQuery): INyxCollectionView;
    function ParentIndex(AIndex: Integer): Integer;
    function CellText(const AItem: TNyxItemRef; AColumn: Integer): TNyxText;
    procedure Select(const AItem: TNyxItemRef); overload;
    procedure Select(const AItem: TNyxItemRef; AAction: TNyxSelectionAction); overload;
    procedure SetSelection(const AItems: array of TNyxItemRef;
      const AFocus, AAnchor: TNyxItemRef);
    procedure SelectAll;
    procedure SelectRange(const AItem: TNyxItemRef;
      const AVisibleOrder: array of TNyxItemRef; AAdd: Boolean);
    procedure ClearSelection;
    procedure Edit(const AItem: TNyxItemRef; AColumn: Integer;
      const AValue: TNyxStateValue);
    procedure EditWire(const AItem: TNyxItemRef; AColumn: Integer;
      const AValue: TNyxText);
    procedure Apply(const AEdits: array of TNyxCollectionEdit;
      AExpectedRevision: Integer = -1);
    function Subscribe(AObserver: TNyxCollectionViewObserver): INyxCollectionViewSubscription;
    function HasChildren(const AItem: TNyxItemRef): Boolean;
    function GetCommandSerial: Integer;
    function IsExpanded(const AItem: TNyxItemRef): Boolean;
    function SetExpanded(const AItem: TNyxItemRef;
      AExpanded: Boolean): INyxTreeHierarchy;
    function ExpandAll: INyxTreeHierarchy;
    function CollapseAll: INyxTreeHierarchy;
    function VisibleItems: TNyxItemRefs;
  end;

  TContextPreparation = class;
  TContext = class(TInterfacedObject, INyxCollectionContext, INyxResourceCollectionContext,
    INyxPreparedResourceCollectionContext)
  private
    FDefaults: INyxCollectionDefaults;
    FCollections: INyxCollections;
    FOwners: array of TNyxText;
    FKeys: array of TNyxText;
    FStores: array of INyxCollection;
    FSourceKeys: array of TNyxCollectionRef;
    FSourceRows: array of TNyxResourceRows;
    FPreparation: TContextPreparation; { weak; stage retains this independent context }
  public
    constructor Create(const ADefaults: INyxCollectionDefaults;
      const AResources: INyxResources; const ALocale, AFallback: TNyxLocaleRef);
    function Resolve(const ASpec: TNyxCollectionViewSpec;
      const AInstanceID: TNyxText): INyxCollection;
    function GetCollections: INyxCollections;
    procedure ValidateResources(const AResources: INyxResources;
      const ALocale, AFallback: TNyxLocaleRef);
    function PrepareResources(const AResources: INyxResources;
      const ALocale, AFallback: TNyxLocaleRef): INyxPreparedPublication;
  end;

  TContextPreparation = class(TInterfacedObject, INyxPreparedPublication)
  private
    FOwner: TContext;
    FLease: INyxCollectionContext;
    FDefaults: INyxCollectionDefaults;
    FRows: INyxPreparedPublication;
    FValidated: Boolean;
    FInstalled: Boolean;
    FRetired: Boolean;
  public
    constructor Create(AOwner: TContext; const AResources: INyxResources;
      const ALocale, AFallback: TNyxLocaleRef);
    destructor Destroy; override;
    procedure Validate;
    procedure Install;
    procedure Notify;
    procedure Retire;
  end;

function PrepareNyxCollectionContextResources(const AContext: INyxCollectionContext;
  const AResources: INyxResources; const ALocale, AFallback: TNyxLocaleRef): INyxPreparedPublication;
var
  LPrepared: INyxPreparedResourceCollectionContext;
begin

  if (AContext = nil) or not Supports(AContext, INyxPreparedResourceCollectionContext, LPrepared) then
  begin
    raise ENyxCollection.Create('Collection context does not support coordinated resource publication');
  end;
  Result := LPrepared.PrepareResources(AResources, ALocale, AFallback);
end;

constructor TContextPreparation.Create(AOwner: TContext; const AResources: INyxResources;
  const ALocale, AFallback: TNyxLocaleRef);
var
  LIndex: Integer;
  LRow: Integer;
  LStore: Integer;
  LSeed, LCandidate: INyxCollectionSnapshot;
  LSame: Boolean;
  LItems: TNyxPreparedPublications;

  procedure PrepareStore(const AStore: INyxCollection);
  var
    LAtomic: INyxAtomicCollection;
    LNext: Integer;
  begin

    if not Supports(AStore, INyxAtomicCollection, LAtomic) then
    begin
      raise ENyxCollection.Create('Resource rows require a prepared collection store');
    end;
    LNext := Length(LItems);
    SetLength(LItems, LNext + 1);
    LItems[LNext] := LAtomic.PrepareAssign(LCandidate, AStore.Snapshot.Revision);
  end;

begin
  inherited Create;

  if AOwner.FPreparation <> nil then
  begin
    raise ENyxCollection.Create('Collection resource publication is already prepared');
  end;
  FLease := AOwner;
  FOwner := AOwner;
  FOwner.FPreparation := Self;
  try
    FDefaults := FOwner.FDefaults;
    for LIndex := 0 to High(FOwner.FSourceKeys) do
    begin
      LSeed := FOwner.FDefaults.Snapshot(FOwner.FSourceKeys[LIndex]);
      LCandidate := FOwner.FSourceRows[LIndex].Read(AResources,
        FOwner.FSourceKeys[LIndex], ALocale, AFallback);
      LSame := LSeed.Count = LCandidate.Count;
      for LRow := 0 to LSeed.Count - 1 do
      begin

        if not LSame then
        begin
          Break;
        end;
        LSame := LSeed.ItemAt(LRow).SameItem(LCandidate.ItemAt(LRow));
      end;

      if LSame then
      begin
        Continue;
      end;

      if FDefaults = FOwner.FDefaults then
      begin
        FDefaults := FOwner.FDefaults.Clone;
      end;
      FDefaults.Define(LCandidate);
      PrepareStore(FOwner.FCollections.Collection(FOwner.FSourceKeys[LIndex]));
      for LStore := 0 to High(FOwner.FStores) do
      begin

        if FOwner.FKeys[LStore] = FOwner.FSourceKeys[LIndex].Name then
        begin
          PrepareStore(FOwner.FStores[LStore]);
        end;
      end;
    end;

    if Length(LItems) > 0 then
    begin
      FRows := PrepareNyxGroup(LItems);
    end;
  finally
    { A later recipe can fail after earlier stores reserved. Retire that local
      prefix explicitly on both targets, before constructor cleanup releases it. }

    if FRows = nil then
    begin
      for LIndex := 0 to High(LItems) do
      begin

        if LItems[LIndex] <> nil then
        begin
          LItems[LIndex].Retire;
        end;
      end;
    end;
  end;
end;

destructor TContextPreparation.Destroy;
begin
  Retire;
  inherited Destroy;
end;

procedure TContextPreparation.Validate;
begin

  if FRetired or FValidated then
  begin
    raise ENyxCollection.Create('Collection resource preparation is single-use');
  end;

  if FRows <> nil then
  begin
    FRows.Validate;
  end;
  FValidated := True;
end;

procedure TContextPreparation.Install;
var
  LPrevious: INyxCollectionDefaults;
begin
  LPrevious := FOwner.FDefaults;
  FOwner.FDefaults := FDefaults;
  FDefaults := LPrevious;

  if FRows <> nil then
  begin
    FRows.Install;
  end;
  FInstalled := True;
end;

procedure TContextPreparation.Notify;
begin

  if not FInstalled or FRetired then
  begin
    raise ENyxCollection.Create('Collection resource notification requires installed data');
  end;

  if FRows <> nil then
  begin
    FRows.Notify;
  end;
end;

procedure TContextPreparation.Retire;
begin

  if FRetired then
  begin
    Exit;
  end;
  FRetired := True;

  if FRows <> nil then
  begin
    FRows.Retire;
    FRows := nil;
  end;

  if FOwner <> nil then
  begin
    FOwner.FPreparation := nil;
    FOwner := nil;
  end;
  FDefaults := nil;
  FLease := nil;
end;

function TContext.PrepareResources(const AResources: INyxResources;
  const ALocale, AFallback: TNyxLocaleRef): INyxPreparedPublication;
begin
  Result := TContextPreparation.Create(Self, AResources, ALocale, AFallback);
end;

constructor TContext.Create(const ADefaults: INyxCollectionDefaults;
  const AResources: INyxResources; const ALocale, AFallback: TNyxLocaleRef);
var
  LDefaults: INyxCollectionDefaults;
  LIndex: Integer;
  LSource: TNyxResourceRows;
  LNext: Integer;
begin
  inherited Create;

  if ADefaults = nil then
  begin
    raise ENyxCollection.Create('Collection context requires authored defaults');
  end;
  { Materialization re-admits alternative registries/snapshots. Capture our own
    immutable defaults so later authored edits cannot alter new instance seeds. }
  LDefaults := ADefaults;

  if NyxHasResourceCollections(ADefaults) then
  begin
    LDefaults := MaterializeNyxCollectionDefaults(ADefaults, AResources, ALocale, AFallback);
  end;
  FCollections := NewNyxCollections(LDefaults);
  for LIndex := 0 to FCollections.Count - 1 do
  begin

    if NyxCollectionResourceSource(ADefaults, FCollections.Key(LIndex), LSource) then
    begin
      LNext := Length(FSourceKeys);
      SetLength(FSourceKeys, LNext + 1);
      SetLength(FSourceRows, LNext + 1);
      FSourceKeys[LNext] := FCollections.Key(LIndex);
      FSourceRows[LNext] := LSource;
    end;
  end;
  FDefaults := NewNyxCollectionDefaults;
  while FDefaults.Count < FCollections.Count do
  begin
    FDefaults.Define(FCollections.Collection(
      FCollections.Key(FDefaults.Count)).Snapshot);
  end;
end;

procedure ValidateNyxCollectionContextResources(const AContext: INyxCollectionContext;
  const AResources: INyxResources; const ALocale, AFallback: TNyxLocaleRef);
var
  LContext: INyxResourceCollectionContext;
begin

  if (AContext <> nil) and Supports(AContext, INyxResourceCollectionContext, LContext) then
  begin
    LContext.ValidateResources(AResources, ALocale, AFallback);
  end;
end;

procedure TContext.ValidateResources(const AResources: INyxResources;
  const ALocale, AFallback: TNyxLocaleRef);
var
  LPrepared: INyxPreparedPublication;
begin
  LPrepared := PrepareResources(AResources, ALocale, AFallback);
  try
    LPrepared.Validate;
  finally
    LPrepared.Retire;
  end;
end;

function TContext.GetCollections: INyxCollections;
begin
  Result := FCollections;
end;

function TContext.Resolve(const ASpec: TNyxCollectionViewSpec;
  const AInstanceID: TNyxText): INyxCollection;
var
  LIndex: Integer;
  LStore: INyxCollection;
begin
  ASpec.Validate;

  if not ASpec.Defined then
  begin
    raise ENyxCollection.Create('Cannot resolve an absent collection view');
  end;

  if ASpec.Scope = csApplication then
  begin
    Exit(FCollections.Collection(ASpec.Key));
  end;

  if AInstanceID = '' then
  begin
    raise ENyxCollection.Create('Instance collection binding requires a reusable owner');
  end;
  for LIndex := 0 to Length(FStores) - 1 do
  begin

    if (FOwners[LIndex] = AInstanceID) and (FKeys[LIndex] = ASpec.Key.Name) then
    begin
      Exit(FStores[LIndex]);
    end;
  end;

  if FPreparation <> nil then
  begin
    raise ENyxCollection.Create('New instance scopes refuse during resource publication');
  end;

  if Length(FStores) >= NyxMaximumCollectionViewInstances then
  begin
    raise ENyxCollection.Create('Collection context exceeds 1024 instance stores');
  end;
  LStore := NewNyxCollection(FDefaults.Snapshot(ASpec.Key));
  LIndex := Length(FStores);
  SetLength(FOwners, LIndex + 1);
  SetLength(FKeys, LIndex + 1);
  SetLength(FStores, LIndex + 1);
  FOwners[LIndex] := AInstanceID;
  FKeys[LIndex] := ASpec.Key.Name;
  FStores[LIndex] := LStore;
  Result := LStore;
end;

function NewNyxCollectionContext(
  const ADefaults: INyxCollectionDefaults): INyxCollectionContext;
begin
  Result := TContext.Create(ADefaults, nil, NyxDefaultLocale, NyxDefaultLocale);
end;

function NewNyxCollectionContext(const ADefaults: INyxCollectionDefaults;
  const AResources: INyxResources; const ALocale, AFallback: TNyxLocaleRef): INyxCollectionContext;
begin
  Result := TContext.Create(ADefaults, AResources, ALocale, AFallback);
end;

function NewNyxCollectionContext(const ADefaults: INyxCollectionDefaults;
  const AContext: INyxResourceContext): INyxCollectionContext;
begin

  if AContext = nil then
  begin
    raise ENyxCollection.Create('Collection materialization requires a resource frame');
  end;
  Result := TContext.Create(ADefaults, AContext.Snapshot, AContext.Locale, AContext.Fallback);
end;

destructor TViewSubscription.Destroy;
begin
  Disconnect;
  inherited Destroy;
end;

function TViewSubscription.GetConnected: Boolean;
begin
  Result := Owner <> nil;
end;

procedure TViewSubscription.Disconnect;
begin

  if Owner <> nil then
  begin
    Owner.Disconnect(Self);
  end;
end;

constructor TView.Create(const AStore: INyxCollection;
  const ASpec: TNyxCollectionViewSpec; AProjection: TNyxCollectionProjection);
var
  LParents: TParentIndexes;
  LAtomic: INyxAtomicCollection;
begin
  inherited Create;
  ASpec.Validate;

  if (AStore = nil) or not ASpec.Defined then
  begin
    raise ENyxCollection.Create('Collection view requires a store and binding');
  end;

  if (Ord(AProjection) < Ord(Low(TNyxCollectionProjection))) or
    (Ord(AProjection) > Ord(High(TNyxCollectionProjection))) then
  begin
    raise ENyxCollection.Create('Unknown collection projection');
  end;

  if ASpec.HasTypeAhead and (AProjection = cpTable) then
  begin
    raise ENyxCollection.Create('Authored typeahead requires a list or tree projection');
  end;
  FStore := AStore;
  FSpec := ASpec.Copy;
  FProjection := AProjection;
  FSource := FStore.Snapshot;
  FQuery := ASpec.QueryPolicy;
  ValidateDataset(FSource, LParents);
  FSnapshot := NyxQuerySnapshot(FSource, FQuery, LParents, FParents);

  if FProjection = cpTree then
  begin
    SetLength(FExpanded, FSource.Count);
  end;
  BuildHierarchy;
  FSelection := NewNyxCollectionSelection(FSource, [],
    Default(TNyxItemRef), Default(TNyxItemRef));

  { Query into a local lease before retaining the optional capability. The
    matched pas2js compiler cleans up an interface passed to Supports on scope
    exit; passing the field itself leaves a released reference in the view.
    Explicit field assignment balances ownership on both target compilers. }

  if Supports(FStore, INyxAtomicCollection, LAtomic) then
  begin
    FAtomic := LAtomic;
    FStoreToken := FAtomic.SubscribePrepared(StoreChanged, PrepareCandidate,
      InstallCandidate, RetireCandidate);
  end
  else
  begin
    { An alternative store may retain the original single-store contract. It
      cannot join a coordinated publication without the optional capability. }
    FStoreToken := FStore.Subscribe(StoreChanged, ValidateCandidate);
  end;
end;

destructor TView.Destroy;
var
  LIndex: Integer;
begin
  RetireCandidate;

  if FStoreToken <> nil then
  begin
    FStoreToken.Disconnect;
    FStoreToken := nil;
  end;
  for LIndex := 0 to Length(FTokens) - 1 do
  begin
    FTokens[LIndex].Owner := nil;
  end;
  FTokens := nil;
  FSnapshot := nil;
  FSource := nil;
  FAtomic := nil;
  FStore := nil;
  inherited Destroy;
end;

function NewNyxCollectionView(const AStore: INyxCollection;
  const ASpec: TNyxCollectionViewSpec;
  AProjection: TNyxCollectionProjection): INyxCollectionView;
begin
  Result := TView.Create(AStore, ASpec, AProjection);
end;

constructor TView.CreateValidator(const ASpec: TNyxCollectionViewSpec;
  AProjection: TNyxCollectionProjection);
begin
  inherited Create;
  FSpec := ASpec.Copy;
  FProjection := AProjection;
end;

procedure ValidateNyxCollectionViewSnapshot(const ASnapshot: INyxCollectionSnapshot;
  const ASpec: TNyxCollectionViewSpec; AProjection: TNyxCollectionProjection);
var
  LValidator: TView;
  LParents: TParentIndexes;
begin
  ASpec.Validate;

  if not ASpec.Defined or
    (Ord(AProjection) < Ord(Low(TNyxCollectionProjection))) or
    (Ord(AProjection) > Ord(High(TNyxCollectionProjection))) then
  begin
    raise ENyxCollection.Create('Saved collection view requires a binding and projection');
  end;

  if ASpec.HasTypeAhead and (AProjection = cpTable) then
  begin
    { Shared document admission runs before target mounts are created. Reject
      an unsupported authored policy here as well as in live view construction. }
    raise ENyxCollection.Create('Authored typeahead requires a list or tree projection');
  end;
  LValidator := TView.CreateValidator(ASpec, AProjection);
  try
    LValidator.ValidateDataset(ASnapshot, LParents);
  finally
    LValidator.Free;
  end;
end;

procedure TView.Writable;
begin

  if FNotifying or ((FAtomic <> nil) and FAtomic.Busy) then
  begin
    raise ENyxCollection.Create('Reentrant collection view mutation is not supported');
  end;
end;

procedure TView.ValidateDataset(const AData: INyxCollectionSnapshot;
  out AParents: TParentIndexes);
var
  LSchema: TNyxCollectionSchema;
  LColumn: TNyxCollectionColumn;
  LIndex: Integer;
  LField: Integer;
  LFound: Boolean;
  LParentName: TNyxText;
  LColors: array of Integer;
  LCursor: Integer;
begin

  if (AData = nil) or (AData.Key.Name <> FSpec.Key.Name) or
    (AData.Count < 0) or (AData.Count > NyxMaximumCollectionItems) then
  begin
    raise ENyxCollection.Create('Collection view snapshot scope/count is invalid');
  end;
  LSchema := AData.Schema;
  LSchema.Validate;
  FSpec.QueryPolicy.Validate(LSchema);
  FQuery.Validate(LSchema);
  for LIndex := 0 to FSpec.Count - 1 do
  begin
    LColumn := FSpec.ColumnAt(LIndex);
    LFound := False;
    for LField := 0 to LSchema.Count - 1 do
    begin

      if LSchema.FieldAt(LField).Name = LColumn.FieldName then
      begin

        if LSchema.FieldAt(LField).Kind <> LColumn.Kind then
        begin
          raise ENyxCollection.Create('Collection column type differs from its schema: ' +
            LColumn.FieldName);
        end;
        LFound := True;
      end;
    end;

    if not LFound then
    begin
      raise ENyxCollection.Create('Unknown collection view field: ' + LColumn.FieldName);
    end;
  end;
  SetLength(AParents, AData.Count);
  for LIndex := 0 to AData.Count - 1 do
  begin
    AParents[LIndex] := -1;
  end;

  if FSpec.ParentField = '' then
  begin
    Exit;
  end;

  if FProjection <> cpTree then
  begin
    raise ENyxCollection.Create('Parent fields require a tree projection');
  end;
  LFound := False;
  for LField := 0 to LSchema.Count - 1 do
  begin

    if (LSchema.FieldAt(LField).Name = FSpec.ParentField) and
      (LSchema.FieldAt(LField).Kind = nskText) then
    begin
      LFound := True;
    end;
  end;

  if not LFound then
  begin
    raise ENyxCollection.Create('Tree parent field must exist with text type');
  end;
  for LIndex := 0 to AData.Count - 1 do
  begin
    LParentName := AData.ItemAt(LIndex).GetValue(NyxTextField(FSpec.ParentField));

    if LParentName <> '' then
    begin
      AParents[LIndex] := AData.IndexOf(NyxItem(FSpec.Key, LParentName));

      if AParents[LIndex] < 0 then
      begin
        raise ENyxCollection.Create('Tree parent item is missing: ' + LParentName);
      end;
    end;
  end;
  { Iterative three-color walk admits parent-before-child or child-before-parent
    datasets in O(rows), rejects cycles, and avoids recursion on deep trees. }
  SetLength(LColors, AData.Count);
  for LIndex := 0 to AData.Count - 1 do
  begin

    if LColors[LIndex] = 0 then
    begin
      LCursor := LIndex;
      while (LCursor >= 0) and (LColors[LCursor] = 0) do
      begin
        LColors[LCursor] := 1;
        LCursor := AParents[LCursor];
      end;

      if (LCursor >= 0) and (LColors[LCursor] = 1) then
      begin
        raise ENyxCollection.Create('Tree parent links contain a cycle');
      end;
      LCursor := LIndex;
      while (LCursor >= 0) and (LColors[LCursor] = 1) do
      begin
        LColors[LCursor] := 2;
        LCursor := AParents[LCursor];
      end;
    end;
  end;
end;

procedure TView.ValidateCandidate(const AData: INyxCollectionSnapshot;
  const AChanges: INyxCollectionChanges);
var
  LParents: TParentIndexes;
begin
  ValidateDataset(AData, LParents);
end;

procedure TView.StoreChanged(const AStore: INyxCollection;
  const AChanges: INyxCollectionChanges);
begin

  if FAtomic = nil then
  begin
    ProjectChanges(AChanges);
  end;
  { Built-in stores already installed every prepared view before the first
    observer. Notifications perform target synchronization, never admission. }
  Notify(AChanges);
end;

procedure TView.PrepareCandidate(const AData: INyxCollectionSnapshot;
  const AChanges: INyxCollectionChanges);
var
  LCandidate: TView;
  LKeepAlive: INyxCollectionView;
begin
  { Query predicates may be alternative implementations. Retain this receiver
    through admission even when application code retires its rendered host. }
  LKeepAlive := Self as INyxCollectionView;
  RetireCandidate;
  LCandidate := TView.CreateValidator(FSpec, FProjection);
  try
    LCandidate.FQuery := FQuery;
    LCandidate.FSource := FSource;
    LCandidate.FSnapshot := FSnapshot;
    LCandidate.FSelection := FSelection;
    LCandidate.FExpanded := Copy(FExpanded, 0, Length(FExpanded));
    LCandidate.ProjectChanges(AChanges);
    FPrepared := LCandidate;
  except
    LCandidate.Free;
    raise;
  end;
end;

procedure TView.InstallCandidate(const AData: INyxCollectionSnapshot;
  const AChanges: INyxCollectionChanges);
begin
  { Private installation is a nonallocating adoption of immutable references
    and detached vectors. No user callback, query or selection command runs. }
  FSource := FPrepared.FSource;
  FSnapshot := FPrepared.FSnapshot;
  FParents := FPrepared.FParents;
  FSelection := FPrepared.FSelection;
  FExpanded := FPrepared.FExpanded;
  FFirstChild := FPrepared.FFirstChild;
  FNextSibling := FPrepared.FNextSibling;
  RetireCandidate;
end;

procedure TView.RetireCandidate;
begin
  FreeAndNil(FPrepared);
end;

procedure TView.ProjectChanges(const AChanges: INyxCollectionChanges);
var
  LParents: TParentIndexes;
  LResultParents: TParentIndexes;
  LResult: INyxCollectionSnapshot;
  LSelection: INyxCollectionSelection;
  LExpanded: array of Boolean;
  LIndex: Integer;
  LPrevious: Integer;
begin
  ValidateDataset(AChanges.After, LParents);
  LResult := NyxQuerySnapshot(AChanges.After, FQuery, LParents, LResultParents);
  LSelection := ReconcileSelection(AChanges.After, LResult, FSnapshot);
  LExpanded := nil;
  { Disclosure follows exact surviving source identity, including rows currently
    filtered out. A new item with a previously removed ID starts closed. }

  if FProjection = cpTree then
  begin
    SetLength(LExpanded, AChanges.After.Count);
    for LIndex := 0 to AChanges.After.Count - 1 do
    begin
      LPrevious := FSource.IndexOf(AChanges.After.ItemAt(LIndex).Ref);

      if LPrevious >= 0 then
      begin
        LExpanded[LIndex] := FExpanded[LPrevious];
      end;
    end;
    { A grouped remove/reinsert can keep the final ID while replacing its runtime
      instance. Honor the admitted operation log instead of resurrecting state. }
    for LIndex := 0 to AChanges.Count - 1 do
    begin

      if AChanges.Kind(LIndex) = nceRemove then
      begin
        LPrevious := AChanges.After.IndexOf(AChanges.ItemRef(LIndex));

        if LPrevious >= 0 then
        begin
          LExpanded[LPrevious] := False;
        end;
      end;
    end;
  end;
  FSource := AChanges.After;
  FSnapshot := LResult;
  FParents := LResultParents;
  FSelection := LSelection;
  FExpanded := LExpanded;
  BuildHierarchy;
  ReconcileTreeFocus;
end;

function TView.ReconcileSelection(const ASource, AResult,
  APrevious: INyxCollectionSnapshot): INyxCollectionSelection;
var
  LItems: array of TNyxItemRef;
  LFocus: TNyxItemRef;
  LAnchor: TNyxItemRef;
  LIndex: Integer;
  LCount: Integer;
  LFocusIndex: Integer;
begin
  { Membership is validated against the complete source; only actual removals
    prune it. Visibility constrains the cursor, independently of hidden members. }
  SetLength(LItems, FSelection.Count);
  LCount := 0;
  for LIndex := 0 to ASource.Count - 1 do
  begin

    if FSelection.Contains(ASource.ItemAt(LIndex).Ref) then
    begin
      LItems[LCount] := ASource.ItemAt(LIndex).Ref;
      Inc(LCount);
    end;
  end;
  SetLength(LItems, LCount);
  LFocus := FSelection.Focus;
  LAnchor := FSelection.Anchor;

  if LFocus.Defined and not AResult.Has(LFocus) then
  begin
    LFocusIndex := APrevious.IndexOf(LFocus);
    LFocus := Default(TNyxItemRef);

    if AResult.Count > 0 then
    begin

      if LFocusIndex >= AResult.Count then
      begin
        LFocusIndex := AResult.Count - 1;
      end;

      if LFocusIndex < 0 then
      begin
        LFocusIndex := 0;
      end;
      LFocus := AResult.ItemAt(LFocusIndex).Ref;
    end;
  end;

  if LAnchor.Defined and not ASource.Has(LAnchor) then
  begin
    LAnchor := LFocus;
  end;
  Result := NewNyxCollectionSelection(ASource, LItems, LFocus, LAnchor);
end;

function TView.PublicationReady: Boolean;
begin
  Result := not FNotifying and (FPrepared = nil) and (FAtomic <> nil) and not FAtomic.Busy;
end;

function NyxCollectionViewPublicationReady(const AView: INyxCollectionView): Boolean;
var
  LReadiness: INyxCollectionViewPublication;
begin
  Result := (AView <> nil) and Supports(AView, INyxCollectionViewPublication, LReadiness)
    and LReadiness.PublicationReady;
end;

function TView.GetQuery: TNyxCollectionQuery;
begin
  Result := FQuery.Copy;
end;

function TView.ConfigureQuery(const APolicy: TNyxCollectionQuery): INyxCollectionView;
var
  LPolicy: TNyxCollectionQuery;
  LParents: TParentIndexes;
  LResultParents: TParentIndexes;
  LResult: INyxCollectionSnapshot;
  LSelection: INyxCollectionSelection;
begin
  Writable;
  Result := Self as INyxCollectionView;
  LPolicy := TNyxCollectionQuery.FromData(APolicy.ToData);

  if LPolicy.ToData.ToJSON = FQuery.ToData.ToJSON then
  begin
    Exit;
  end;
  ValidateDataset(FSource, LParents);
  LResult := NyxQuerySnapshot(FSource, LPolicy, LParents, LResultParents);
  LSelection := ReconcileSelection(FSource, LResult, FSnapshot);
  FQuery := LPolicy;
  FSnapshot := LResult;
  FParents := LResultParents;
  FSelection := LSelection;
  BuildHierarchy;
  ReconcileTreeFocus;
  Notify(nil);
end;

function TView.GetSpec: TNyxCollectionViewSpec;
begin
  Result := FSpec.Copy;
end;

function TView.GetSnapshot: INyxCollectionSnapshot;
begin
  Result := FSnapshot;
end;

function TView.GetStore: INyxCollection;
begin
  Result := FStore;
end;

function TView.GetProjection: TNyxCollectionProjection;
begin
  Result := FProjection;
end;

function TView.GetHasSelection: Boolean;
begin
  Result := FSelection.Count > 0;
end;

function TView.GetSelected: TNyxItemRef;
begin

  if FSelection.Count = 0 then
  begin
    raise ENyxCollection.Create('Collection view has no selected item');
  end;
  Result := FSelection.ItemAt(0);

  if FSelection.Focus.Defined and FSelection.Contains(FSelection.Focus) then
  begin
    Result := FSelection.Focus;
  end;
end;

function TView.GetSelection: INyxCollectionSelection;
begin
  Result := FSelection;
end;

function TView.ParentIndex(AIndex: Integer): Integer;
begin

  if (AIndex < 0) or (AIndex >= Length(FParents)) then
  begin
    raise ENyxCollection.Create('Collection view parent index is out of range');
  end;
  Result := FParents[AIndex];
end;

function TView.CellText(const AItem: TNyxItemRef; AColumn: Integer): TNyxText;
var
  LColumn: TNyxCollectionColumn;
  LItem: TNyxCollectionItem;
begin
  LColumn := FSpec.ColumnAt(AColumn);
  LItem := FSource.Item(AItem);
  case LColumn.Kind of
    nskText:
      begin
        Result := LItem.GetValue(NyxTextField(LColumn.FieldName));
      end;
    nskBoolean:
      begin
        Result := 'false';

        if LItem.GetValue(NyxBooleanField(LColumn.FieldName)) then
        begin
          Result := 'true';
        end;
      end;
    nskInteger:
      begin
        Result := IntToStr(LItem.GetValue(NyxIntegerField(LColumn.FieldName)));
      end;
    nskNumber:
      begin
        Result := TNyxStateValue.FromNumber(
          LItem.GetValue(NyxNumberField(LColumn.FieldName))).NumberText;
      end;
  end;
end;

procedure TView.Select(const AItem: TNyxItemRef);
begin
  Select(AItem, nsaReplace);
end;

procedure TView.SetSelection(const AItems: array of TNyxItemRef;
  const AFocus, AAnchor: TNyxItemRef);
var
  LSelection: INyxCollectionSelection;
  LItems: array of TNyxItemRef;
  LIndex: Integer;
  LCount: Integer;
  LRevealed: Boolean;
begin
  Writable;
  LSelection := NewNyxCollectionSelection(FSource, AItems, AFocus, AAnchor);

  if AFocus.Defined and not FSnapshot.Has(AFocus) then
  begin
    raise ENyxCollection.Create('Collection view focus must be visible');
  end;

  if (FSpec.SelectionMode = nsmSingle) and (LSelection.Count > 1) then
  begin
    raise ENyxCollection.Create('Single selection refuses multiple items');
  end;
  { Normalize public candidates in complete source order, never their caller's
    dynamic-array order. The temporary snapshot admits all values atomically. }
  SetLength(LItems, LSelection.Count);
  LCount := 0;
  for LIndex := 0 to FSource.Count - 1 do
  begin

    if LSelection.Contains(FSource.ItemAt(LIndex).Ref) then
    begin
      LItems[LCount] := FSource.ItemAt(LIndex).Ref;
      Inc(LCount);
    end;
  end;
  LSelection := NewNyxCollectionSelection(FSource, LItems, AFocus, AAnchor);
  AdvanceTreeCommand;
  { Admission above must finish before disclosure changes. Explicit application
    focus opens ancestors; a later structural/collapse publication never does. }
  LRevealed := RevealAncestors(AFocus);

  if FSelection.SameState(LSelection) and not LRevealed then
  begin
    Exit;
  end;
  FSelection := LSelection;
  Notify(nil);
end;

{$I nyx.collections.tree.inc}

procedure TView.Select(const AItem: TNyxItemRef; AAction: TNyxSelectionAction);
var
  LItems: array of TNyxItemRef;
  LAnchor: TNyxItemRef;
  LItem: TNyxItemRef;
  LIndex: Integer;
  LCount: Integer;
  LFirst: Integer;
  LLast: Integer;
  LVisibleIndex: Integer;
  LSelected: Boolean;
begin
  Writable;

  if not FSnapshot.Has(AItem) then
  begin
    raise ENyxCollection.Create('Selected collection item does not exist');
  end;

  if (Ord(AAction) < Ord(Low(TNyxSelectionAction))) or
    (Ord(AAction) > Ord(High(TNyxSelectionAction))) then
  begin
    raise ENyxCollection.Create('Unknown selection gesture');
  end;

  if FSpec.SelectionMode = nsmSingle then
  begin

    if AAction <> nsaFocus then
    begin
      AAction := nsaReplace;
    end;
  end;
  LAnchor := FSelection.Anchor;

  if (AAction in [nsaReplace, nsaToggle]) or not LAnchor.Defined then
  begin
    LAnchor := AItem;
  end;
  LFirst := FSnapshot.IndexOf(LAnchor);
  LLast := FSnapshot.IndexOf(AItem);

  if LFirst < 0 then
  begin
    LFirst := LLast;
  end;

  if LFirst > LLast then
  begin
    LIndex := LFirst;
    LFirst := LLast;
    LLast := LIndex;
  end;
  SetLength(LItems, FSource.Count);
  LCount := 0;
  for LIndex := 0 to FSource.Count - 1 do
  begin
    LItem := FSource.ItemAt(LIndex).Ref;
    LVisibleIndex := FSnapshot.IndexOf(LItem);
    LSelected := FSelection.Contains(LItem);
    case AAction of
      nsaFocus:
        begin
          { Focus changes the anchor below without changing selected members. }
        end;
      nsaReplace:
        begin
          LSelected := LItem.ID = AItem.ID;
        end;
      nsaToggle:
        begin

          if LItem.ID = AItem.ID then
          begin
            LSelected := not LSelected;
          end;
        end;
      nsaRange:
        begin
          LSelected := (LVisibleIndex >= LFirst) and (LVisibleIndex <= LLast);
        end;
      nsaAddRange:
        begin
          LSelected := LSelected or
            ((LVisibleIndex >= LFirst) and (LVisibleIndex <= LLast));
        end;
    end;

    if LSelected then
    begin
      LItems[LCount] := LItem;
      Inc(LCount);
    end;
  end;
  SetLength(LItems, LCount);
  SetSelection(LItems, AItem, LAnchor);
end;

procedure TView.SelectRange(const AItem: TNyxItemRef;
  const AVisibleOrder: array of TNyxItemRef; AAdd: Boolean);
var
  LOrder: INyxCollectionSelection;
  LRange: INyxCollectionSelection;
  LAnchor: TNyxItemRef;
  LItems: TNyxItemRefs;
  LFirst: Integer;
  LLast: Integer;
  LIndex: Integer;
  LCount: Integer;
begin
  Writable;
  LAnchor := FSelection.Anchor;
  LOrder := NewNyxCollectionSelection(FSnapshot, AVisibleOrder, AItem,
    Default(TNyxItemRef));

  if not LOrder.Contains(AItem) then
  begin
    raise ENyxCollection.Create('Range target is outside the visible order');
  end;

  if (FSpec.SelectionMode <> nsmMultiple) then
  begin
    Select(AItem);
    Exit;
  end;

  if not LAnchor.Defined or not LOrder.Contains(LAnchor) then
  begin
    LAnchor := AItem;
  end;
  LFirst := -1;
  LLast := -1;
  for LIndex := 0 to LOrder.Count - 1 do
  begin

    if LOrder.ItemAt(LIndex).ID = LAnchor.ID then
    begin
      LFirst := LIndex;
    end;

    if LOrder.ItemAt(LIndex).ID = AItem.ID then
    begin
      LLast := LIndex;
    end;
  end;

  if LFirst > LLast then
  begin
    LIndex := LFirst;
    LFirst := LLast;
    LLast := LIndex;
  end;
  SetLength(LItems, LLast - LFirst + 1);
  for LIndex := LFirst to LLast do
  begin
    LItems[LIndex - LFirst] := LOrder.ItemAt(LIndex);
  end;
  LRange := NewNyxCollectionSelection(FSnapshot, LItems, AItem, LAnchor);
  SetLength(LItems, FSource.Count);
  LCount := 0;
  for LIndex := 0 to FSource.Count - 1 do
  begin

    if LRange.Contains(FSource.ItemAt(LIndex).Ref) or
      (AAdd and FSelection.Contains(FSource.ItemAt(LIndex).Ref)) then
    begin
      LItems[LCount] := FSource.ItemAt(LIndex).Ref;
      Inc(LCount);
    end;
  end;
  SetLength(LItems, LCount);
  SetSelection(LItems, AItem, LAnchor);
end;

procedure TView.SelectAll;
var
  LItems: array of TNyxItemRef;
  LFocus: TNyxItemRef;
  LAnchor: TNyxItemRef;
  LIndex: Integer;
begin
  Writable;

  if FSpec.SelectionMode <> nsmMultiple then
  begin
    raise ENyxCollection.Create('SelectAll requires multiple selection');
  end;
  SetLength(LItems, FSnapshot.Count);
  for LIndex := 0 to FSnapshot.Count - 1 do
  begin
    LItems[LIndex] := FSnapshot.ItemAt(LIndex).Ref;
  end;
  LFocus := FSelection.Focus;
  LAnchor := FSelection.Anchor;

  if (FSnapshot.Count > 0) and not LFocus.Defined then
  begin
    LFocus := LItems[0];
    LAnchor := LFocus;
  end;
  SetSelection(LItems, LFocus, LAnchor);
end;

procedure TView.ClearSelection;
begin
  SetSelection([], Default(TNyxItemRef), Default(TNyxItemRef));
end;

procedure TView.Edit(const AItem: TNyxItemRef; AColumn: Integer;
  const AValue: TNyxStateValue);
var
  LColumn: TNyxCollectionColumn;
  LItem: TNyxCollectionItem;
begin
  Writable;
  LColumn := FSpec.ColumnAt(AColumn);

  if LColumn.Mode <> cmEditable then
  begin
    raise ENyxCollection.Create('Collection view column is read only');
  end;

  if AValue.Kind <> LColumn.Kind then
  begin
    raise ENyxCollection.Create('Collection view edit has the wrong scalar type');
  end;
  { Require an existing identity before creating a partial update. The store
    remains the atomic domain/schema/validator admission authority. }
  FSnapshot.Item(AItem);
  LItem := NyxCollectionItem(AItem);
  case AValue.Kind of
    nskText:
      begin
        LItem := LItem.WithValue(NyxTextField(LColumn.FieldName), AValue.TextValue);
      end;
    nskBoolean:
      begin
        LItem := LItem.WithValue(NyxBooleanField(LColumn.FieldName), AValue.BooleanValue);
      end;
    nskInteger:
      begin
        LItem := LItem.WithValue(NyxIntegerField(LColumn.FieldName), AValue.IntegerValue);
      end;
    nskNumber:
      begin
        LItem := LItem.WithValue(NyxNumberField(LColumn.FieldName), AValue.NumberValue);
      end;
  end;
  FStore.Update(LItem);
end;

procedure TView.EditWire(const AItem: TNyxItemRef; AColumn: Integer;
  const AValue: TNyxText);
var
  LValue: TNyxDataValue;
  LKind: TNyxStateKind;
begin
  LKind := FSpec.ColumnAt(AColumn).Kind;
  LValue := NyxScalarDomain(LKind).ReadWire(AValue);
  case LKind of
    nskText:
      begin
        Edit(AItem, AColumn, TNyxStateValue.FromText(LValue.AsText));
      end;
    nskBoolean:
      begin
        Edit(AItem, AColumn, TNyxStateValue.FromBoolean(LValue.AsBoolean));
      end;
    nskInteger:
      begin
        Edit(AItem, AColumn, TNyxStateValue.FromInteger(LValue.AsInteger));
      end;
    nskNumber:
      begin
        Edit(AItem, AColumn, TNyxStateValue.FromNumber(LValue.AsNumber));
      end;
  end;
end;

procedure TView.Apply(const AEdits: array of TNyxCollectionEdit;
  AExpectedRevision: Integer);
begin
  Writable;
  FStore.Apply(AEdits, AExpectedRevision);
end;

procedure TView.Disconnect(AToken: TViewSubscription);
var
  LIndex: Integer;
  LMove: Integer;
begin
  for LIndex := 0 to Length(FTokens) - 1 do
  begin

    if FTokens[LIndex] = AToken then
    begin
      AToken.Owner := nil;
      for LMove := LIndex to Length(FTokens) - 2 do
      begin
        FTokens[LMove] := FTokens[LMove + 1];
      end;
      SetLength(FTokens, Length(FTokens) - 1);
      Exit;
    end;
  end;
  AToken.Owner := nil;
end;

function TView.Subscribe(
  AObserver: TNyxCollectionViewObserver): INyxCollectionViewSubscription;
var
  LToken: TViewSubscription;
begin
  Writable;

  if not Assigned(AObserver) or
    (Length(FTokens) >= NyxMaximumCollectionSubscriptions) or
    (FNextSerial = High(Integer)) then
  begin
    raise ENyxCollection.Create('Collection view subscription is invalid or exceeds its budget');
  end;
  LToken := TViewSubscription.Create;
  Result := LToken;
  Inc(FNextSerial);
  LToken.Serial := FNextSerial;
  LToken.Observer := AObserver;
  SetLength(FTokens, Length(FTokens) + 1);
  FTokens[Length(FTokens) - 1] := LToken;
  LToken.Owner := Self;
end;

procedure TView.Notify(const AChanges: INyxCollectionChanges);
var
  LKeepAlive: INyxCollectionView;
  LSerials: array of Integer;
  LIndex: Integer;
  LCurrent: Integer;
  LObserver: TNyxCollectionViewObserver;
  LError: TNyxText;
  LHasError: Boolean;
begin
  { Snapshot serials rather than raw tokens: an earlier callback may disconnect
    and release a later receiver. The interface local protects our lifetime if
    a callback drops the last application's view reference. }
  LKeepAlive := Self as INyxCollectionView;
  LError := '';
  LHasError := False;
  SetLength(LSerials, Length(FTokens));
  for LIndex := 0 to Length(FTokens) - 1 do
  begin
    LSerials[LIndex] := FTokens[LIndex].Serial;
  end;
  FNotifying := True;
  try
    for LIndex := 0 to Length(LSerials) - 1 do
    begin
      for LCurrent := 0 to Length(FTokens) - 1 do
      begin

        if FTokens[LCurrent].Serial = LSerials[LIndex] then
        begin
          LObserver := FTokens[LCurrent].Observer;
          try
            LObserver(LKeepAlive, AChanges);
          except
            on LException: Exception do
            begin

              if not LHasError then
              begin
                LHasError := True;
                LError := TNyxText(LException.Message);
              end;
            end;
          end;
          Break;
        end;
      end;
    end;
  finally
    FNotifying := False;
  end;

  if LHasError then
  begin
    raise ENyxCollectionNotification.Create('Collection view observer failed after publication: ' +
      LError);
  end;
end;

end.
