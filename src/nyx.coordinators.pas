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

unit nyx.coordinators;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.types, nyx.data, nyx.model, nyx.collections.bindings,
  nyx.collections.view.types, nyx.events, nyx.behavior, nyx.scheduler,
  nyx.publication.types, nyx.binding.inputs, nyx.state, nyx.contract;

type
  { Coordinator admission is a UI-only operation, like the existing prepared
    collection and event owners. Factories are trusted Pascal application code. }
  ENyxCoordinator = class(Exception);
  { A revoked projection or refresh cannot regain authority through a retained
    interface, an old callback, or reuse of the same runtime identity. }
  ENyxCoordinatorCancelled = class(ENyxCoordinator);

  { A renderer implements this weak receiver. It must revoke its borrowed host
    before destruction. Only the private guarded refresh facade retains it. }
  INyxCoordinatorRefreshTarget = interface(IInterface)
    ['{B37C0817-53EE-4568-BFDB-17A35DA23003}']
    procedure Request;
  end;
  { Factories receive this revocable, installed-generation-only refresh port.
    Request is the last outward call: it may dispose the complete mounted view. }
  INyxCoordinatorRefresh = interface(IInterface)
    ['{3BEABCF0-ECC8-4A4A-A551-0E72653F1338}']
    procedure Request;
  end;
  { Copied discovery and guarded presentation, never Node/GetNode or a widget.
    Resolve roles and reserve Text/Enabled during Prepare. Claims seal before
    Build; later updates may use only those exact reserved roles. Text refuses
    active text bindings. Enabled adds an AND constraint, preserving authored
    values. Discovery does not grant collection mutation or parent ownership. }
  INyxCoordinatorProjection = interface(IInterface)
    ['{5E95D705-9894-40FB-931D-8B8379738613}']
    function Root: TNyxControlRef;
    function Part(const APath: TNyxPartRef): TNyxControlRef;
    function Kind(const ARole: TNyxControlRef): TNyxKindRef;
    function AuthoredText(const ARole: TNyxControlRef): TNyxText;
    function TryCollectionSpec(const ARole: TNyxControlRef;
      out ASpec: TNyxCollectionViewSpec): Boolean;
    procedure Text(const ARole: TNyxControlRef; const AValue: TNyxText);
    procedure Enabled(const ARole: TNyxControlRef; AAllowed: Boolean);
  end;
  { Copied role proof for specialized behavior. Semantic and primitive kinds
    are separate: changing a primitive projection must not impersonate a typed
    role. EffectiveText captures the current viewport/platform placeholder
    before any coordinator writes, while AuthoredText keeps its stored meaning. }
  TNyxCoordinatorRoleInfo = record
    Kind: TNyxKindRef;
    ProjectionKind: TNyxKindRef;
    Compound: Boolean;
    EffectiveText: TNyxText;
  end;
  { Optional discovery capability keeps the original projection GUID stable.
    Its copied facts join the same final candidate checks; no descriptor escapes. }
  INyxCoordinatorRoleFacts = interface(IInterface)
    ['{9CD362E4-56F0-4640-B5BA-EE5802F2FAD8}']
    function Describe(const ARole: TNyxControlRef): TNyxCoordinatorRoleInfo;
  end;
  { Optional closed behavior inspection leaves the original role-info record
    layout and interface intact. A compound can refuse a competing scalar
    action without borrowing its descriptor or rewriting caller callbacks. }
  INyxCoordinatorBehaviorFacts = interface(IInterface)
    ['{7D11B924-028F-4BA3-9E0E-018FCE5DA969}']
    function Action(const ARole: TNyxControlRef): TNyxAction;
  end;
  { Optional typed scalar capability; the original projection GUID stays
    unchanged. Reserve exact roles before Build seals discovery. StoredValue
    copies descriptor presence/value, before runtime binding projection; use
    Commands.Capture for current bound values after installation. Commands are
    weak, claimed-role-only and revoke with this scope even if its view lives. }
  INyxCoordinatorInputs = interface(IInterface)
    ['{DA86A3FA-FACF-470E-99DF-85A561960326}']
    procedure ReserveInput(const ARole: TNyxControlRef);
    function ValueDomain(const ARole: TNyxControlRef): TNyxValueDomain;
    function StoredValue(const ARole: TNyxControlRef; out AValue: TNyxStateValue): Boolean;
    function InputCommands: INyxInputCommands;
  end;
  { Explicit collection discovery under the coordinator's reusable expansion
    prefix. Returns a copied exact runtime identity; no arbitrary node escapes.
    Only List/Table targets are admitted. Outside roles remain collection-only,
    so discovering a sibling table grants no scalar/text/Enabled authority. }
  INyxCoordinatorTargets = interface(IInterface)
    ['{1BC73083-D580-4A33-B78C-47F989643657}']
    function CollectionTarget(const AReference: TNyxControlRef): TNyxControlRef;
  end;
  { Optional owner retirement for a behavior whose public Disconnect releases
    its presentation immediately. Revoke changes local flags only; Disconnect
    then attempts this scope's existing cleanup. Neither cancels the enclosing
    event generation nor calls the child/mount back. Retained copies stay inert. }
  INyxCoordinatorProjectionLifetime = interface(IInterface)
    ['{B463718B-3951-42AD-9246-D5D122B8DD01}']
    procedure Revoke;
    procedure Disconnect;
  end;
  { Optional synchronous capture before the router schedules callbacks. A
    component may append its exact copied record, never consult selection after
    queuing. It preserves independent scalar Value and unrelated event payload.
    The common wrapper fences its accepted generation around every callout. }
  INyxCoordinatorEventEnrichment = interface(IInterface)
    ['{5F8715C5-A721-40E8-9CC2-1E45F424EBD3}']
    function Enrich(const AEvent: TNyxEventInfo): TNyxEventInfo;
  end;
  { A creator's managed lifetime owner. Commands must check local revocation
    and the exact accepted generation before mutating their existing data owner.
    Releasing/disconnecting a mount retires subscriptions; it never owns a
    renderer or document. Its specialized interfaces/identity remain intact. }
  INyxCoordinatorBinding = interface(IInterface)
    ['{CDF10321-9215-48A5-B7D9-57079480D604}']
    function GetConnected: Boolean;
    procedure Disconnect;
    property Connected: Boolean read GetConnected;
  end;
  { Factories obey the existing prepared-publication/readiness contracts.
    Revoke is local flags only; Disconnect attempts fallible cleanup afterward.
    Installed Retire releases preparation leases without revoking the binding. }
  INyxPreparedCoordinator = interface(INyxPreparedPublication)
    ['{66FC6CCE-2C72-49ED-90E5-82D80537680F}']
    function GetBinding: INyxCoordinatorBinding;
    procedure Revoke;
    procedure Disconnect;
    property Binding: INyxCoordinatorBinding read GetBinding;
  end;
  { Prepare receives independent copied intent and exact candidate data/event
    owners. It must stage observations in AView, keep callbacks dormant until
    Install, and expose INyxPublicationReadiness. No executable factory is
    loaded from metadata, and saved intent grants no host/service authority. }
  INyxCoordinatorFactory = interface(IInterface)
    ['{E5AB017E-DBB3-496C-A4EF-458CED7C7090}']
    function Prepare(const AProjection: INyxCoordinatorProjection;
      const AIntent: TNyxDataValue; const AViews: INyxCollectionBindings;
      const AView: INyxPreparedEventView; const AScheduler: INyxScheduler;
      const ARefresh: INyxCoordinatorRefresh): INyxPreparedCoordinator;
  end;
  { Immutable registration membership and managed factory leases for one
    candidate. Later registrations affect only subsequently captured snapshots. }
  INyxCoordinatorRegistrySnapshot = interface(IInterface)
    ['{4AA6BA7C-184B-466B-9819-B1269F80BA70}']
    function GetCount: Integer;
    function Kind(AIndex: Integer): TNyxKindRef;
    function IntentKey(AIndex: Integer): TNyxExtensionRef;
    function Factory(AIndex: Integer): INyxCoordinatorFactory;
    property Count: Integer read GetCount;
  end;
  { Explicit caller-owned, UI-thread-confined registration. Duplicate kind/key
    pairs refuse; two matching keys on one node refuse rather than choosing an
    order-dependent winner. An empty registry changes no static control. }
  INyxCoordinatorRegistry = interface(IInterface)
    ['{B230D69E-4760-4D33-A173-C5A8869F50C2}']
    procedure RegisterFactory(const AKind: TNyxKindRef;
      const AIntentKey: TNyxExtensionRef; const AFactory: INyxCoordinatorFactory);
    function Snapshot: INyxCoordinatorRegistrySnapshot;
  end;
  { Managed prepared/lifetime owners avoid interface-containing records on
    pas2js. Renderers revoke the complete former vector before retiring nodes. }
  TNyxCoordinatorMounts = array of INyxPreparedCoordinator;
  { Parallel exact realized identities, independent of captions and row order. }
  TNyxCoordinatorMountIDs = array of TNyxText;

function NewNyxCoordinatorRegistry: INyxCoordinatorRegistry;
{ Both input memberships are copied before any factory can run. Duplicate
  kind/intent pairs refuse rather than overriding a built-in or caller by order. }
function ComposeNyxCoordinatorSnapshots(const ABuiltIns,
  ACaller: INyxCoordinatorRegistrySnapshot): INyxCoordinatorRegistrySnapshot;
function NyxHasCoordinatorMounts(ARoot: TNyxNode;
  const ASnapshot: INyxCoordinatorRegistrySnapshot): Boolean;
{ The host supplies paired candidate bindings/event plan and an already-realized
  detached candidate, never the currently accepted mutable projection.
  The driver copies the complete registry before invoking any factory.
  Projections privately retain canonical node facades, not their document/parent.
  ARefreshTarget must be a weak final-host port, never a renderer-owning closure. }
procedure PrepareNyxCoordinatorMounts(ARoot: TNyxNode;
  const AViews: INyxCollectionBindings; const AEventView: INyxPreparedEventView;
  const AScheduler: INyxScheduler; const ASnapshot: INyxCoordinatorRegistrySnapshot;
  const ARefreshTarget: INyxCoordinatorRefreshTarget;
  out AMounts: TNyxCoordinatorMounts; out AIDs: TNyxCoordinatorMountIDs);
procedure ValidateNyxCoordinatorMounts(const AMounts: TNyxCoordinatorMounts);
procedure CheckNyxCoordinatorMounts(const AMounts: TNyxCoordinatorMounts);
procedure InstallNyxCoordinatorMounts(const AMounts: TNyxCoordinatorMounts);
procedure NotifyNyxCoordinatorMounts(const AMounts: TNyxCoordinatorMounts);
procedure RetireNyxCoordinatorMounts(const AMounts: TNyxCoordinatorMounts);
procedure RevokeNyxCoordinatorMounts(const AMounts: TNyxCoordinatorMounts);
{ Detach the caller vector and revoke every scope before fallible disconnects.
  Every participant receives its cleanup attempt. The first Pascal failure,
  including an empty diagnostic, reports through ENyxCoordinator afterward;
  browser platform throws retain their exact original value and identity. }
procedure DisconnectNyxCoordinatorMounts(var AMounts: TNyxCoordinatorMounts);
function FindNyxCoordinatorMount(const AMounts: TNyxCoordinatorMounts;
  const AIDs: TNyxCoordinatorMountIDs; const AID: TNyxText): INyxCoordinatorBinding;
{ Captures managed vector and event copies before optional enrichment callouts.
  Renderers still check their own exact view revision before dispatch/legacy use. }
function EnrichNyxCoordinatorEvent(const AMounts: TNyxCoordinatorMounts;
  const AEvent: TNyxEventInfo): TNyxEventInfo;

implementation

uses
  nyx.controls, nyx.binding.types, nyx.collections.view, nyx.collections,
  nyx.interaction.policy, nyx.publication, nyx.coordinators.internal, nyx.schema
  {$ifdef PAS2JS}, JS{$endif};

type
  { One explicit cleanup result owns the first copied Pascal diagnostic or raw
    browser throw. Captured is independent of empty text/null/undefined, so a
    later duty cannot replace an earlier failure merely because it has no text. }
  TCoordinatorFailure = record
    Captured: Boolean;
    Message: TNyxText;
    {$ifdef PAS2JS}
    Value: JSValue;
    IsRaw: Boolean;
    {$endif}
  end;
  { Parallel arrays own values and interfaces without pas2js record limitations. }
  TCoordinatorRegistry = class(TInterfacedObject, INyxCoordinatorRegistry)
  private
    FKinds: array of TNyxKindRef;
    FKeys: array of TNyxExtensionRef;
    FFactories: array of INyxCoordinatorFactory;
    FFrozen: Boolean;
    procedure CheckIndex(AIndex: Integer);
  public
    procedure RegisterFactory(const AKind: TNyxKindRef;
      const AIntentKey: TNyxExtensionRef; const AFactory: INyxCoordinatorFactory);
    function Snapshot: INyxCoordinatorRegistrySnapshot;
    function GetCount: Integer;
    function Kind(AIndex: Integer): TNyxKindRef;
    function IntentKey(AIndex: Integer): TNyxExtensionRef;
    function Factory(AIndex: Integer): INyxCoordinatorFactory;
  end;
  { The snapshot exposes no writable registry interface, including through
    Supports. Its private copied table is frozen before this facade publishes. }
  TCoordinatorSnapshot = class(TInterfacedObject, INyxCoordinatorRegistrySnapshot)
  private
    FTable: TCoordinatorRegistry;
    FLifetime: INyxCoordinatorRegistry;
  public
    constructor Create(ATable: TCoordinatorRegistry;
      const ALifetime: INyxCoordinatorRegistry);
    function GetCount: Integer;
    function Kind(AIndex: Integer): TNyxKindRef;
    function IntentKey(AIndex: Integer): TNyxExtensionRef;
    function Factory(AIndex: Integer): INyxCoordinatorFactory;
  end;
  { Forward declaration for private weak policy/subscription receivers. }
  TCoordinatorScope = class;
  { Callbacks acquire a short managed scope lease before reading its raw owner.
    The port itself never keeps the scope or its retained nodes alive. }
  ICoordinatorPort = interface(IInterface)
    ['{A6679204-3400-40D8-9CAA-E75691ED9AA3}']
    function Acquire: INyxCoordinatorProjection;
    function Owner: TCoordinatorScope;
    procedure Revoke;
  end;
  { View changes mark a dormant candidate dirty without mutating selection.
    Scalar commands resolve only claimed roles through the exact adopted host;
    captures/stages retain this weak port instead of keeping the scope alive. }
  TCoordinatorPort = class(TInterfacedObject, ICoordinatorPort, INyxInputCommands)
  private
    FOwner: TCoordinatorScope;
  public
    constructor Create(AOwner: TCoordinatorScope);
    function Acquire: INyxCoordinatorProjection;
    function Owner: TCoordinatorScope;
    procedure Revoke;
    function GetConnected: Boolean;
    function Capture(const AControls: array of TNyxControlRef): INyxInputCapture;
    function Prepare(const ACapture: INyxInputCapture;
      const AValues: array of TNyxInputValue): INyxPreparedPublication;
    procedure Changed(const AView: INyxCollectionView;
      const AChanges: INyxCollectionChanges);
  end;
  { One exact role retains its canonical facade and copied behavioral baseline.
    Parent is identity evidence only; its lifetime is not granted to factories. }
  TCoordinatorRole = class
  public
    Control: INyxControl;
    Node: TNyxNode;
    Parent: TNyxNode;
    ID: TNyxText;
    Kind: TNyxText;
    ProjectionKind: TNyxText;
    Compound: Boolean;
    EffectiveText: TNyxText;
    Action: TNyxAction;
    Path: TNyxText;
    TextDefault: TNyxText;
    TextCurrent: TNyxText;
    EnabledDefault: TNyxText;
    Spec: TNyxText;
    TextClaimed: Boolean;
    TextReservation: INyxRuntimeTextReservation;
    EnabledClaimed: Boolean;
    InputClaimed: Boolean;
    ExternalCollection: Boolean;
    Allowed: Boolean;
    Policy: INyxRuntimeEnabledPolicy;
    View: INyxCollectionView;
    Readiness: INyxCollectionViewPublication;
    Token: INyxCollectionViewSubscription;
  end;
  { Restricts effective Enabled without modifying stored or bound values.
    Cached reads are callback-free and remain safe after scope revocation. }
  TCoordinatorEnabled = class(TInterfacedObject, INyxRuntimeEnabledPolicy)
  private
    FPort: ICoordinatorPort;
    FRole: Integer;
  public
    constructor Create(const APort: ICoordinatorPort; ARole: Integer);
    function GetAllowed: Boolean;
  end;
  { Canonical retained projection with sealed claims and a weak renderer port.
    Final readiness uses dirty flags, the retained generation and raw model
    baselines; alternate view readiness is called only during Validate. }
  TCoordinatorScope = class(TInterfacedObject, INyxCoordinatorProjection,
    INyxCoordinatorRoleFacts, INyxCoordinatorProjectionLifetime,
    INyxCoordinatorRefresh, INyxCoordinatorInputs, INyxCoordinatorTargets,
    INyxCoordinatorBehaviorFacts)
  private
    FTree: INyxControl;
    FTreeNode: TNyxNode;
    FRoot: INyxControl;
    FRootNode: TNyxNode;
    FIntentKey: TNyxExtensionRef;
    FIntent: TNyxText;
    FGeneration: INyxCancellationScope;
    FScheduler: INyxScheduler;
    FViews: INyxCollectionBindings;
    FTarget: INyxCoordinatorRefreshTarget;
    FInputHost: IInterface; { exact trusted weak adapter facet, never a renderer }
    FPort: ICoordinatorPort;
    FCallback: TCoordinatorPort;
    FRoles: array of TCoordinatorRole;
    FSealed: Boolean;
    FInstalled: Boolean;
    FRevoked: Boolean;
    FCleaned: Boolean;
    FDirty: Boolean;
    function AddRole(ANode: TNyxNode; const APath: TNyxText): Integer;
    function RoleIndex(const ARole: TNyxControlRef): Integer;
    procedure CheckLive(AInstalled: Boolean = False);
    procedure CheckNodes;
    function LiveInputs: INyxInputCommands;
    procedure CheckInput(const AControl: TNyxControlRef);
    procedure Seal;
    procedure Admit;
    procedure Cleanup;
  public
    constructor Create(ATree, ARoot: TNyxNode; const AKey: TNyxExtensionRef;
      const AViews: INyxCollectionBindings; const AView: INyxPreparedEventView;
      const AScheduler: INyxScheduler;
      const ATarget: INyxCoordinatorRefreshTarget);
    destructor Destroy; override;
    procedure Revoke;
    procedure Disconnect;
    function Root: TNyxControlRef;
    function Part(const APath: TNyxPartRef): TNyxControlRef;
    function Kind(const ARole: TNyxControlRef): TNyxKindRef;
    function AuthoredText(const ARole: TNyxControlRef): TNyxText;
    function Describe(const ARole: TNyxControlRef): TNyxCoordinatorRoleInfo;
    function Action(const ARole: TNyxControlRef): TNyxAction;
    procedure ReserveInput(const ARole: TNyxControlRef);
    function ValueDomain(const ARole: TNyxControlRef): TNyxValueDomain;
    function StoredValue(const ARole: TNyxControlRef; out AValue: TNyxStateValue): Boolean;
    function InputCommands: INyxInputCommands;
    function CollectionTarget(const AReference: TNyxControlRef): TNyxControlRef;
    function TryCollectionSpec(const ARole: TNyxControlRef;
      out ASpec: TNyxCollectionViewSpec): Boolean;
    procedure Text(const ARole: TNyxControlRef; const AValue: TNyxText);
    procedure Enabled(const ARole: TNyxControlRef; AAllowed: Boolean);
    procedure Request;
  end;
  { Private canonical proof prevents a capture from another coordinator scope
    from borrowing this scope's claims, even on the same mounted input owner. }
  IScopedInputProof = interface(IInterface)
    ['{FCD3EF83-8973-451F-BA8E-69ADE0F9A0BA}']
    function SnapshotObject: TObject;
  end;
  { Owned inner snapshot and weak scope identity. No retained projection/tree,
    widget or creator binding; copied values stay readable after revocation. }
  TScopedInputCapture = class(TInterfacedObject, INyxInputCapture, IScopedInputProof)
  public
    Port: ICoordinatorPort;
    Commands: INyxInputCommands;
    Inner: INyxInputCapture;
    function SnapshotObject: TObject;
    function GetCount: Integer;
    function ControlAt(AIndex: Integer): TNyxControlRef;
    function HasValueAt(AIndex: Integer): Boolean;
    function ValueAt(AIndex: Integer): TNyxStateValue;
  end;
  { Delegates all model phases to the existing input group. The added weak
    scope fence prevents a retired component from installing into a live peer.
    This wrapper owns no state/query model and implements no second engine. }
  TScopedInputStage = class(TInterfacedObject, INyxPreparedPublication,
    INyxPublicationReadiness)
  private
    FPort: ICoordinatorPort;
    FCommands: INyxInputCommands;
    FInner: INyxPreparedPublication;
    FFence: INyxPublicationReadiness;
    FRetired: Boolean;
    procedure CheckScope;
  public
    constructor Create(const APort: ICoordinatorPort;
      const ACommands: INyxInputCommands; const AInner: INyxPreparedPublication);
    destructor Destroy; override;
    procedure Validate;
    procedure CheckReady;
    procedure Install;
    procedure Notify;
    procedure Retire;
  end;
  { Wraps a trusted creator's preparation with the driver's private revocable
    scope. The exact creator binding remains public, preserving specialized
    Supports interfaces; its commands obey the same generation/revoke contract. }
  TCoordinatorMount = class(TInterfacedObject, INyxPreparedCoordinator,
    INyxPreparedPublication, INyxPublicationReadiness,
    INyxCoordinatorEventEnrichment)
  private
    FScope: TCoordinatorScope;
    FProjection: INyxCoordinatorProjection;
    FChild: INyxPreparedCoordinator;
    FBinding: INyxCoordinatorBinding;
    FFence: INyxPublicationReadiness;
    FValidated: Boolean;
    FInstalled: Boolean;
    FNotified: Boolean;
    FNotifying: Boolean;
    FRetired: Boolean;
    FCleaned: Boolean;
    procedure Cleanup;
  public
    constructor Create(AScope: TCoordinatorScope;
      const AProjection: INyxCoordinatorProjection;
      const AChild: INyxPreparedCoordinator);
    destructor Destroy; override;
    function GetBinding: INyxCoordinatorBinding;
    procedure Revoke;
    procedure Disconnect;
    procedure Validate;
    procedure CheckReady;
    procedure Install;
    procedure Notify;
    procedure Retire;
    function Enrich(const AEvent: TNyxEventInfo): TNyxEventInfo;
  end;

{ Call from the active catch before attempting the next independent duty.
  Pascal layers deliberately copy their established normalized diagnostic;
  browser bridges retain arbitrary original values without string conversion. }
procedure CaptureCoordinatorFailure(var AFailure: TCoordinatorFailure;
  {$ifdef PAS2JS}AValue: JSValue{$else}const AMessage: TNyxText{$endif});
begin

  if AFailure.Captured then
  begin
    Exit;
  end;
  AFailure.Captured := True;
  {$ifdef PAS2JS}
  AFailure.Value := AValue;
  AFailure.IsRaw := not (isClassInstance(AValue) and
    (TObject(AValue) is Exception));

  if not AFailure.IsRaw then
  begin
    AFailure.Message := TNyxText(Exception(AValue).Message);
  end;
  {$else}
  AFailure.Message := AMessage;
  {$endif}
end;

{ Preserve the existing per-layer Pascal prefix while raw browser failures keep
  their original identity. Every caller finishes all cleanup before reporting. }
procedure RaiseCoordinatorFailure(const AFailure: TCoordinatorFailure;
  const APrefix: TNyxText);
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

  if AFailure.IsRaw then
  begin
    { pas2js inserts rtl.getObject for an Exception cast, coercing primitive
      throws (notably undefined) to null. This boundary preserves the raw value. }
    LValue := AFailure.Value;
    asm
      throw LValue;
    end;
  end;
  {$endif}
  raise ENyxCoordinator.Create(APrefix + AFailure.Message);
end;

function NewNyxCoordinatorRegistry: INyxCoordinatorRegistry;
begin
  Result := TCoordinatorRegistry.Create;
end;

procedure TCoordinatorRegistry.CheckIndex(AIndex: Integer);
begin

  if (AIndex < 0) or (AIndex >= Length(FKinds)) then
  begin
    raise ENyxCoordinator.Create('Coordinator registration index is outside its snapshot');
  end;
end;

procedure TCoordinatorRegistry.RegisterFactory(const AKind: TNyxKindRef;
  const AIntentKey: TNyxExtensionRef; const AFactory: INyxCoordinatorFactory);
var
  LKind: TNyxKindRef;
  LKey: TNyxExtensionRef;
  LFactory: INyxCoordinatorFactory;
  LIndex: Integer;
  LCount: Integer;
begin

  if FFrozen then
  begin
    raise ENyxCoordinator.Create('A captured coordinator registry snapshot is immutable');
  end;
  LKind.Name := NyxData(AKind.Name).AsText;
  LKey := AIntentKey.Copy;
  LFactory := AFactory;

  if Trim(LKind.Name) = '' then
  begin
    raise ENyxCoordinator.Create('Coordinator registration requires a nonempty kind');
  end;

  if LFactory = nil then
  begin
    raise ENyxCoordinator.Create('A coordinator registration needs its managed factory');
  end;
  for LIndex := 0 to High(FKinds) do
  begin

    if (FKinds[LIndex].Name = LKind.Name) and (FKeys[LIndex].Name = LKey.Name) then
    begin
      raise ENyxCoordinator.Create('A coordinator kind/intent pair is already registered');
    end;
  end;
  LCount := Length(FKinds);
  SetLength(FKinds, LCount + 1);
  SetLength(FKeys, LCount + 1);
  SetLength(FFactories, LCount + 1);
  FKinds[LCount] := LKind;
  FKeys[LCount] := LKey;
  FFactories[LCount] := LFactory;
end;

function TCoordinatorRegistry.Snapshot: INyxCoordinatorRegistrySnapshot;
var
  LCopy: TCoordinatorRegistry;
  LLease: INyxCoordinatorRegistry;
  LIndex: Integer;
begin
  LCopy := TCoordinatorRegistry.Create;
  LLease := LCopy;
  for LIndex := 0 to High(FKinds) do
  begin
    LCopy.RegisterFactory(FKinds[LIndex], FKeys[LIndex], FFactories[LIndex]);
  end;
  LCopy.FFrozen := True;
  Result := TCoordinatorSnapshot.Create(LCopy, LLease);
end;

constructor TCoordinatorSnapshot.Create(ATable: TCoordinatorRegistry;
  const ALifetime: INyxCoordinatorRegistry);
begin
  inherited Create;
  FTable := ATable;
  FLifetime := ALifetime;
end;

function TCoordinatorSnapshot.GetCount: Integer;
begin
  Result := FTable.GetCount;
end;

function TCoordinatorSnapshot.Kind(AIndex: Integer): TNyxKindRef;
begin
  Result := FTable.Kind(AIndex);
end;

function TCoordinatorSnapshot.IntentKey(AIndex: Integer): TNyxExtensionRef;
begin
  Result := FTable.IntentKey(AIndex);
end;

function TCoordinatorSnapshot.Factory(AIndex: Integer): INyxCoordinatorFactory;
begin
  Result := FTable.Factory(AIndex);
end;

function TCoordinatorRegistry.GetCount: Integer;
begin
  Result := Length(FKinds);
end;

function TCoordinatorRegistry.Kind(AIndex: Integer): TNyxKindRef;
begin
  CheckIndex(AIndex);
  Result := FKinds[AIndex];
end;

function TCoordinatorRegistry.IntentKey(AIndex: Integer): TNyxExtensionRef;
begin
  CheckIndex(AIndex);
  Result := FKeys[AIndex].Copy;
end;

function TCoordinatorRegistry.Factory(AIndex: Integer): INyxCoordinatorFactory;
begin
  CheckIndex(AIndex);
  Result := FFactories[AIndex];
end;

function CopyRegistry(const ASnapshot: INyxCoordinatorRegistrySnapshot):
  INyxCoordinatorRegistrySnapshot;
var
  LInput: INyxCoordinatorRegistrySnapshot;
  LCopy: INyxCoordinatorRegistry;
  LKind: TNyxKindRef;
  LKey: TNyxExtensionRef;
  LFactory: INyxCoordinatorFactory;
  LCount: Integer;
  LIndex: Integer;
begin
  LInput := ASnapshot;
  LCopy := NewNyxCoordinatorRegistry;

  if LInput <> nil then
  begin
    LCount := LInput.Count;

    if LCount < 0 then
    begin
      raise ENyxCoordinator.Create('A registration snapshot cannot have a negative count');
    end;
    for LIndex := 0 to LCount - 1 do
    begin
      LKind := LInput.Kind(LIndex);
      LKey := LInput.IntentKey(LIndex);
      LFactory := LInput.Factory(LIndex);
      LCopy.RegisterFactory(LKind, LKey, LFactory);
    end;

    if LInput.Count <> LCount then
    begin
      raise ENyxCoordinator.Create('Registration membership changed while copying its snapshot');
    end;
  end;
  Result := LCopy.Snapshot;
end;

function ComposeNyxCoordinatorSnapshots(const ABuiltIns,
  ACaller: INyxCoordinatorRegistrySnapshot): INyxCoordinatorRegistrySnapshot;
var
  LBuiltIns: INyxCoordinatorRegistrySnapshot;
  LCaller: INyxCoordinatorRegistrySnapshot;
  LRegistry: INyxCoordinatorRegistry;

  procedure Append(const AInput: INyxCoordinatorRegistrySnapshot);
  var
    LIndex: Integer;
  begin
    for LIndex := 0 to AInput.Count - 1 do
    begin
      LRegistry.RegisterFactory(AInput.Kind(LIndex), AInput.IntentKey(LIndex),
        AInput.Factory(LIndex));
    end;
  end;

begin
  { Retain both arguments before an alternate snapshot getter can release an
    aliased caller slot. Only local immutable copies reach the append phase. }
  LBuiltIns := ABuiltIns;
  LCaller := ACaller;
  LBuiltIns := CopyRegistry(LBuiltIns);
  LCaller := CopyRegistry(LCaller);
  LRegistry := NewNyxCoordinatorRegistry;
  Append(LBuiltIns);
  Append(LCaller);
  Result := LRegistry.Snapshot;
end;

function Match(ANode: TNyxNode;
  const ASnapshot: INyxCoordinatorRegistrySnapshot): Integer;
var
  LIndex: Integer;
begin
  Result := -1;
  for LIndex := 0 to ASnapshot.Count - 1 do
  begin

    if (ANode.Kind = ASnapshot.Kind(LIndex).Name) and
      ANode.Extensions.Has(ASnapshot.IntentKey(LIndex)) then
    begin

      if Result >= 0 then
      begin
        raise ENyxCoordinator.Create('More than one coordinator intent matches ' + ANode.ID);
      end;
      Result := LIndex;
    end;
  end;
end;

constructor TCoordinatorPort.Create(AOwner: TCoordinatorScope);
begin
  inherited Create;
  FOwner := AOwner;
end;

function TCoordinatorPort.Acquire: INyxCoordinatorProjection;
begin
  Result := nil;

  if FOwner <> nil then
  begin
    Result := FOwner;
  end;
end;

function TCoordinatorPort.Owner: TCoordinatorScope;
begin
  Result := FOwner;
end;

procedure TCoordinatorPort.Revoke;
begin
  FOwner := nil;
end;

procedure TCoordinatorPort.Changed(const AView: INyxCollectionView;
  const AChanges: INyxCollectionChanges);
var
  LLease: INyxCoordinatorProjection;
  LOwner: TCoordinatorScope;
begin
  LLease := Acquire;
  LOwner := FOwner;

  if (LLease <> nil) and (LOwner <> nil) and not LOwner.FInstalled then
  begin
    LOwner.FDirty := True;
  end;
end;

constructor TCoordinatorEnabled.Create(const APort: ICoordinatorPort; ARole: Integer);
begin
  inherited Create;
  FPort := APort;
  FRole := ARole;
end;

function TCoordinatorEnabled.GetAllowed: Boolean;
var
  LLease: INyxCoordinatorProjection;
  LOwner: TCoordinatorScope;
  LGeneration: INyxCancellationScope;
  LCancelled: Boolean;
begin
  LLease := FPort.Acquire;
  LOwner := FPort.Owner;
  Result := (LLease <> nil) and (LOwner <> nil) and not LOwner.FRevoked;

  if Result then
  begin
    LGeneration := LOwner.FGeneration;
    Result := LGeneration <> nil;

    if Result then
    begin
      LCancelled := LGeneration.Cancelled;
      Result := not LCancelled and not LOwner.FRevoked and not LOwner.FCleaned;

      if Result then
      begin
        Result := LOwner.FRoles[FRole].Allowed;
      end;
    end;
  end;
end;

constructor TCoordinatorScope.Create(ATree, ARoot: TNyxNode;
  const AKey: TNyxExtensionRef;
  const AViews: INyxCollectionBindings; const AView: INyxPreparedEventView;
  const AScheduler: INyxScheduler; const ATarget: INyxCoordinatorRefreshTarget);
var
  LView: INyxPreparedEventView;
  LScheduler: INyxScheduler;
begin
  inherited Create;
  FTree := TNyxControl.CreateFromNode(ATree);
  FTreeNode := ATree;
  FRoot := TNyxControl.CreateFromNode(ARoot);
  FRootNode := ARoot;
  FIntentKey := AKey.Copy;
  FViews := AViews;
  LView := AView;
  FScheduler := AScheduler;
  LScheduler := FScheduler;
  FTarget := ATarget;

  if not FRootNode.IsRealized or (LView = nil) or (LScheduler = nil) then
  begin
    raise ENyxCoordinator.Create('A coordinator requires a realized target, event plan and scheduler');
  end;
  FIntent := FRootNode.Extensions.Value(FIntentKey).ToJSON;
  FGeneration := LView.Generation;
  CheckLive;
  LScheduler.RequireUI;
  CheckLive;
  FCallback := TCoordinatorPort.Create(Self);
  FPort := FCallback;
  AddRole(FRootNode, '');
end;

destructor TCoordinatorScope.Destroy;
begin
  try
    Cleanup;
  except
    { Explicit Disconnect reports cleanup failures. Destruction never revives it. }
  end;
  inherited Destroy;
end;

procedure TCoordinatorScope.CheckLive(AInstalled: Boolean);
var
  LGeneration: INyxCancellationScope;
  LCancelled: Boolean;
begin
  LGeneration := FGeneration;

  if FRevoked or FCleaned or (LGeneration = nil) or
    (not FInstalled and FDirty) or (AInstalled and not FInstalled) then
  begin
    raise ENyxCoordinatorCancelled.Create('Coordinator projection is changed, dormant or retired');
  end;
  LCancelled := LGeneration.Cancelled;

  if LCancelled or FRevoked or FCleaned or (FGeneration <> LGeneration) or
    (not FInstalled and FDirty) or (AInstalled and not FInstalled) then
  begin
    raise ENyxCoordinatorCancelled.Create('Coordinator event generation is no longer current');
  end;
end;

function TCoordinatorScope.AddRole(ANode: TNyxNode; const APath: TNyxText): Integer;
var
  LRole: TCoordinatorRole;
  LIndex: Integer;
begin
  for LIndex := 0 to High(FRoles) do
  begin

    if FRoles[LIndex].Node = ANode then
    begin
      Exit(LIndex);
    end;
  end;

  if FSealed then
  begin
    raise ENyxCoordinator.Create('Coordinator role discovery is sealed before physical Build');
  end;
  LRole := TCoordinatorRole.Create;
  try
    LRole.Control := TNyxControl.CreateFromNode(ANode);
    LRole.Node := ANode;
    LRole.Parent := ANode.Parent;
    LRole.ID := ANode.ID;
    LRole.Kind := ANode.Kind;
    LRole.ProjectionKind := ANode.ProjectionKind;
    LRole.Compound := ANode.Prop(NyxAttributeName(atCompound), 'false') = 'true';
    LRole.EffectiveText := ANode.Prop(NyxAttributeName(atText));

    if not TryNyxAction(ANode.Prop(NyxAttributeName(atAction),
      NyxActionName(naNone)), LRole.Action) then
    begin
      raise ENyxCoordinator.Create('Coordinator role has an invalid scalar action');
    end;
    LRole.Path := APath;
    LRole.TextDefault := ANode.StoredProp(NyxAttributeName(atText));
    LRole.TextCurrent := LRole.TextDefault;
    LRole.EnabledDefault := ANode.StoredProp(NyxAttributeName(atEnabled), 'true');
    LRole.Spec := ANode.CollectionView.ToData.ToJSON;
    LRole.Allowed := True;
    Result := Length(FRoles);
    SetLength(FRoles, Result + 1);
    FRoles[Result] := LRole;
  except
    LRole.Free;
    raise;
  end;
end;

function TCoordinatorScope.RoleIndex(const ARole: TNyxControlRef): Integer;
var
  LIndex: Integer;
begin
  for LIndex := 0 to High(FRoles) do
  begin

    if FRoles[LIndex].ID = ARole.ID then
    begin
      Exit(LIndex);
    end;
  end;
  raise ENyxCoordinator.Create('This identity was not discovered in the scoped candidate');
end;

function TCoordinatorScope.Root: TNyxControlRef;
var
  LLease: INyxCoordinatorProjection;
begin
  LLease := Self;
  CheckLive;
  Result := NyxControl(FRoles[0].ID);
end;

function TCoordinatorScope.Part(const APath: TNyxPartRef): TNyxControlRef;
var
  LLease: INyxCoordinatorProjection;
  LPath: TNyxText;
  LNode: TNyxNode;
  LIndex: Integer;
begin
  LLease := Self;
  LPath := APath.Name;
  CheckLive;
  LNode := FRootNode.Part(NyxPart(LPath));
  LIndex := AddRole(LNode, LPath);
  Result := NyxControl(FRoles[LIndex].ID);
end;

function TCoordinatorScope.Kind(const ARole: TNyxControlRef): TNyxKindRef;
var
  LLease: INyxCoordinatorProjection;
  LRole: TNyxControlRef;
begin
  LLease := Self;
  LRole := ARole;
  CheckLive;
  Result := NyxCustomKind(FRoles[RoleIndex(LRole)].Kind);
end;

function TCoordinatorScope.AuthoredText(const ARole: TNyxControlRef): TNyxText;
var
  LLease: INyxCoordinatorProjection;
  LRole: TNyxControlRef;
begin
  LLease := Self;
  LRole := ARole;
  CheckLive;
  Result := FRoles[RoleIndex(LRole)].TextDefault;
end;

function TCoordinatorScope.Describe(const ARole: TNyxControlRef): TNyxCoordinatorRoleInfo;
var
  LLease: INyxCoordinatorProjection;
  LRoleRef: TNyxControlRef;
  LRole: TCoordinatorRole;
begin
  LLease := Self;
  LRoleRef := ARole;
  CheckNodes;
  LRole := FRoles[RoleIndex(LRoleRef)];
  Result.Kind := NyxCustomKind(LRole.Kind);
  Result.ProjectionKind := NyxCustomKind(LRole.ProjectionKind);
  Result.Compound := LRole.Compound;
  Result.EffectiveText := LRole.EffectiveText;
end;

function TCoordinatorScope.Action(const ARole: TNyxControlRef): TNyxAction;
var
  LLease: INyxCoordinatorProjection;
  LRole: TCoordinatorRole;
begin
  LLease := Self;
  CheckNodes;
  LRole := FRoles[RoleIndex(ARole)];

  if LRole.ExternalCollection then
  begin
    raise ENyxCoordinator.Create('Collection discovery grants no scalar behavior inspection');
  end;
  Result := LRole.Action;
end;

function TCoordinatorScope.TryCollectionSpec(const ARole: TNyxControlRef;
  out ASpec: TNyxCollectionViewSpec): Boolean;
var
  LLease: INyxCoordinatorProjection;
  LRoleRef: TNyxControlRef;
  LRole: TCoordinatorRole;
  LView: INyxCollectionView;
  LReadiness: INyxCollectionViewPublication;
  LToken: INyxCollectionViewSubscription;
  LViews: INyxCollectionBindings;
begin
  LLease := Self;
  LRoleRef := ARole;
  ASpec := Default(TNyxCollectionViewSpec);
  CheckLive;
  LRole := FRoles[RoleIndex(LRoleRef)];
  ASpec := LRole.Node.CollectionView.Copy;
  Result := ASpec.Defined;

  if not Result or (LRole.View <> nil) then
  begin
    Exit;
  end;

  if FSealed then
  begin
    raise ENyxCoordinator.Create('A collection role must be admitted before claims seal');
  end;
  LViews := FViews;

  if LViews = nil then
  begin
    raise ENyxCoordinator.Create('A bound coordinator role requires candidate collection views');
  end;
  LView := LViews.ViewFor(LRole.ID);
  CheckLive;

  if (LView = nil) or not Supports(LView, INyxCollectionViewPublication, LReadiness) then
  begin
    raise ENyxCoordinator.Create('A coordinator collection role needs publication readiness');
  end;

  if LView.Spec.ToData.ToJSON <> LRole.Spec then
  begin
    raise ENyxCoordinator.Create('Candidate collection view differs from its realized role');
  end;
  CheckLive;
  LRole.View := LView;
  LRole.Readiness := LReadiness;
  LToken := LView.Subscribe(FCallback.Changed);
  CheckLive;
  LRole.Token := LToken;
end;

procedure TCoordinatorScope.Text(const ARole: TNyxControlRef; const AValue: TNyxText);
var
  LLease: INyxCoordinatorProjection;
  LRoleRef: TNyxControlRef;
  LValue: TNyxText;
  LRole: TCoordinatorRole;
  LBinding: TNyxBindingSpec;
begin
  LLease := Self;
  LRoleRef := ARole;
  LValue := NyxData(AValue).AsText;
  CheckLive;

  if FSealed and not FInstalled then
  begin
    raise ENyxCoordinatorCancelled.Create('A sealed dormant projection cannot accept text writes');
  end;
  LRole := FRoles[RoleIndex(LRoleRef)];

  if LRole.ExternalCollection then
  begin
    raise ENyxCoordinator.Create('A collection target does not grant text authority');
  end;

  if not LRole.TextClaimed then
  begin

    if FSealed then
    begin
      raise ENyxCoordinator.Create('Coordinator text role was not reserved before Build');
    end;

    if LRole.Node.FindBinding(bpText, LBinding) then
    begin
      raise ENyxCoordinator.Create('A coordinator cannot replace a bound text projection');
    end;
    try
      LRole.TextReservation := LRole.Node.ReserveRuntimeText;
    except
      on E: ENyxModel do
      begin
        raise ENyxCoordinator.Create('Coordinator text role is already reserved: ' + TNyxText(E.Message));
      end;
    end;
    LRole.TextClaimed := True;
  end;
  CheckNodes;
  LRole.Node.SetProp(NyxAttributeName(atText), LValue);
  LRole.TextCurrent := LValue;
end;

procedure TCoordinatorScope.Enabled(const ARole: TNyxControlRef; AAllowed: Boolean);
var
  LLease: INyxCoordinatorProjection;
  LRoleRef: TNyxControlRef;
  LRole: TCoordinatorRole;
begin
  LLease := Self;
  LRoleRef := ARole;
  CheckLive;

  if FSealed and not FInstalled then
  begin
    raise ENyxCoordinatorCancelled.Create('A sealed dormant projection cannot change availability');
  end;
  LRole := FRoles[RoleIndex(LRoleRef)];

  if LRole.ExternalCollection then
  begin
    raise ENyxCoordinator.Create('A collection target does not grant Enabled authority');
  end;

  if not LRole.EnabledClaimed then
  begin

    if FSealed then
    begin
      raise ENyxCoordinator.Create('Coordinator availability was not reserved before Build');
    end;
    LRole.Policy := TCoordinatorEnabled.Create(FPort, RoleIndex(LRoleRef));
    LRole.Node.AttachEnabledPolicy(LRole.Policy);
    LRole.EnabledClaimed := True;
  end;
  CheckNodes;
  LRole.Allowed := AAllowed;
end;

procedure TCoordinatorScope.CheckNodes;
var
  LRole: TCoordinatorRole;
  LIndex: Integer;
  LPolicy: Integer;
  LFound: Boolean;
  LBinding: TNyxBindingSpec;
begin
  CheckLive;

  if (FTreeNode.Find(FRoles[0].ID) <> FRootNode) or
    not FRootNode.Extensions.Has(FIntentKey) or
    (FRootNode.Extensions.Value(FIntentKey).ToJSON <> FIntent) then
  begin
    raise ENyxCoordinator.Create('Coordinator intent changed after candidate preparation');
  end;
  for LIndex := 0 to High(FRoles) do
  begin
    LRole := FRoles[LIndex];

    if not LRole.Node.IsRealized or (LRole.Node.ID <> LRole.ID) or
      (LRole.Node.Kind <> LRole.Kind) or (LRole.Node.Parent <> LRole.Parent) or
      (LRole.Node.ProjectionKind <> LRole.ProjectionKind) or
      (LRole.Node.Prop(NyxAttributeName(atAction), NyxActionName(naNone)) <>
        NyxActionName(LRole.Action)) or
      ((LRole.Node.Prop(NyxAttributeName(atCompound), 'false') = 'true') <> LRole.Compound) or
      ((not LRole.ExternalCollection) and (FRootNode.Find(LRole.ID) <> LRole.Node)) or
      (LRole.ExternalCollection and (FTreeNode.Find(LRole.ID) <> LRole.Node)) or
      (LRole.Node.CollectionView.ToData.ToJSON <> LRole.Spec) then
    begin
      raise ENyxCoordinator.Create('Coordinator role identity or binding changed');
    end;

    if (LRole.Path <> '') and (FRootNode.Part(NyxPart(LRole.Path)) <> LRole.Node) then
    begin
      raise ENyxCoordinator.Create('Coordinator named role changed after discovery');
    end;

    if LRole.TextClaimed and
      ((LRole.TextReservation = nil) or not LRole.TextReservation.Active or
      (LRole.Node.StoredProp(NyxAttributeName(atText)) <> LRole.TextCurrent) or
      LRole.Node.FindBinding(bpText, LBinding)) then
    begin
      raise ENyxCoordinator.Create('Coordinator text role conflicts with another projection');
    end;

    if LRole.EnabledClaimed then
    begin
      LFound := False;
      for LPolicy := 0 to LRole.Node.EnabledPolicyCount - 1 do
      begin
        LFound := LFound or (LRole.Node.EnabledPolicyAt(LPolicy) = LRole.Policy);
      end;

      if not LFound or (not FInstalled and
        (LRole.Node.StoredProp(NyxAttributeName(atEnabled), 'true') <> LRole.EnabledDefault)) then
      begin
        raise ENyxCoordinator.Create('Coordinator availability role changed');
      end;
    end;
  end;
  CheckLive;
end;

procedure TCoordinatorScope.Seal;
begin
  CheckNodes;
  FSealed := True;
end;

procedure TCoordinatorScope.Admit;
var
  LIndex: Integer;
  LReadiness: INyxCollectionViewPublication;
begin
  CheckNodes;
  for LIndex := 0 to High(FRoles) do
  begin
    LReadiness := FRoles[LIndex].Readiness;

    if LReadiness <> nil then
    begin

      if not LReadiness.PublicationReady then
      begin
        raise ENyxCoordinator.Create('Coordinator collection role is busy before publication');
      end;
      CheckNodes;
    end;
  end;
end;

procedure TCoordinatorScope.Request;
var
  LLease: INyxCoordinatorProjection;
  LScheduler: INyxScheduler;
  LTarget: INyxCoordinatorRefreshTarget;
begin
  LLease := Self;
  CheckLive(True);
  LScheduler := FScheduler;
  LTarget := FTarget;
  LScheduler.RequireUI;
  CheckLive(True);
  CheckNodes;

  if LTarget <> nil then
  begin
    { The weak receiver may dispose every mount. No field access follows. }
    LTarget.Request;
  end;
end;

procedure TCoordinatorScope.Revoke;
var
  LIndex: Integer;
begin
  FRevoked := True;
  FInstalled := False;
  for LIndex := 0 to High(FRoles) do
  begin

    if FRoles[LIndex].TextReservation <> nil then
    begin
      FRoles[LIndex].TextReservation.Revoke;
    end;
  end;

  if FPort <> nil then
  begin
    FPort.Revoke;
  end;
end;

procedure TCoordinatorScope.Cleanup;
var
  LIndex: Integer;
  LRole: TCoordinatorRole;
  LRoles: array of TCoordinatorRole;
  LFailure: TCoordinatorFailure;
begin

  if FCleaned then
  begin
    Exit;
  end;
  FCleaned := True;
  Revoke;
  { The port is already inert. Detach exact role ownership before a token can
    call application cleanup; reentrant retirement cannot revisit these roles. }
  LRoles := FRoles;
  FRoles := nil;
  LFailure := Default(TCoordinatorFailure);
  for LIndex := 0 to High(LRoles) do
  begin
    LRole := LRoles[LIndex];

    if LRole.TextReservation <> nil then
    begin
      try
        LRole.Node.ReleaseRuntimeText(LRole.TextReservation);
      except
        {$ifdef PAS2JS}
        CaptureCoordinatorFailure(LFailure, JSExceptValue);
        {$else}
        on E: Exception do
        begin
          CaptureCoordinatorFailure(LFailure, TNyxText(E.Message));
        end;
        {$endif}
      end;
    end;

    if LRole.Token <> nil then
    begin
      try
        LRole.Token.Disconnect;
      except
        {$ifdef PAS2JS}
        CaptureCoordinatorFailure(LFailure, JSExceptValue);
        {$else}
        on E: Exception do
        begin
          CaptureCoordinatorFailure(LFailure, TNyxText(E.Message));
        end;
        {$endif}
      end;
    end;

    if LRole.Policy <> nil then
    begin
      try
        LRole.Node.DetachEnabledPolicy(LRole.Policy);
      except
        {$ifdef PAS2JS}
        CaptureCoordinatorFailure(LFailure, JSExceptValue);
        {$else}
        on E: Exception do
        begin
          CaptureCoordinatorFailure(LFailure, TNyxText(E.Message));
        end;
        {$endif}
      end;
    end;
  end;
  for LIndex := 0 to High(LRoles) do
  begin
    LRoles[LIndex].Free;
  end;
  LRoles := nil;
  FRootNode := nil;
  FRoot := nil;
  FTreeNode := nil;
  FTree := nil;
  FViews := nil;
  FScheduler := nil;
  FGeneration := nil;
  FInputHost := nil;
  FTarget := nil;
  FCallback := nil;
  FPort := nil;
  RaiseCoordinatorFailure(LFailure, 'Coordinator scope retired; cleanup failed: ');
end;

procedure TCoordinatorScope.Disconnect;
var
  LLease: INyxCoordinatorProjection;
begin
  LLease := Self;
  Cleanup;
end;

constructor TCoordinatorMount.Create(AScope: TCoordinatorScope;
  const AProjection: INyxCoordinatorProjection; const AChild: INyxPreparedCoordinator);
var
  LFence: INyxPublicationReadiness;
begin
  inherited Create;
  FScope := AScope;
  FProjection := AProjection;
  FChild := AChild;

  if (FScope = nil) or (FProjection = nil) or (FChild = nil) then
  begin
    raise ENyxCoordinator.Create('A coordinator factory must return a prepared owner');
  end;
  FScope.CheckLive;

  if not Supports(FChild, INyxPublicationReadiness, LFence) then
  begin
    raise ENyxCoordinator.Create('A coordinator factory must supply final publication readiness');
  end;
  { Query into a local, then retain a distinct owning field lease. Finalizing
    the local must not release the field's reference on either compiler. }
  FFence := LFence;
  FBinding := FChild.Binding;
  FScope.CheckLive;

  if FBinding = nil then
  begin
    raise ENyxCoordinator.Create('A coordinator preparation must retain its lifetime binding');
  end;
  FScope.Seal;
end;

destructor TCoordinatorMount.Destroy;
begin
  try
    Cleanup;
  except
    { Every independent cleanup was attempted; explicit Disconnect reports it. }
  end;
  inherited Destroy;
end;

function TCoordinatorMount.GetBinding: INyxCoordinatorBinding;
begin
  Result := FBinding;
end;

procedure TCoordinatorMount.Revoke;
begin

  if FScope <> nil then
  begin
    FScope.Revoke;
  end;

  if FChild <> nil then
  begin
    FChild.Revoke;
  end;
end;

procedure TCoordinatorMount.Cleanup;
var
  LChild: INyxPreparedCoordinator;
  LProjection: INyxCoordinatorProjection;
  LScope: TCoordinatorScope;
  LFailure: TCoordinatorFailure;
begin

  if FCleaned then
  begin
    Exit;
  end;
  FCleaned := True;
  LChild := FChild;
  LProjection := FProjection;
  LScope := FScope;
  LFailure := Default(TCoordinatorFailure);

  if LScope <> nil then
  begin
    LScope.Revoke;
  end;

  if LChild <> nil then
  begin
    try
      LChild.Revoke;
    except
      {$ifdef PAS2JS}
      CaptureCoordinatorFailure(LFailure, JSExceptValue);
      {$else}
      on E: Exception do
      begin
        CaptureCoordinatorFailure(LFailure, TNyxText(E.Message));
      end;
      {$endif}
    end;
    try
      LChild.Disconnect;
    except
      {$ifdef PAS2JS}
      CaptureCoordinatorFailure(LFailure, JSExceptValue);
      {$else}
      on E: Exception do
      begin
        CaptureCoordinatorFailure(LFailure, TNyxText(E.Message));
      end;
      {$endif}
    end;

    if not FInstalled or (FNotified and not FNotifying) then
    begin
      try
        LChild.Retire;
      except
        {$ifdef PAS2JS}
        CaptureCoordinatorFailure(LFailure, JSExceptValue);
        {$else}
        on E: Exception do
        begin
          CaptureCoordinatorFailure(LFailure, TNyxText(E.Message));
        end;
        {$endif}
      end;
    end;
  end;

  if LScope <> nil then
  begin
    try
      LScope.Cleanup;
    except
      {$ifdef PAS2JS}
      CaptureCoordinatorFailure(LFailure, JSExceptValue);
      {$else}
      on E: Exception do
      begin
        CaptureCoordinatorFailure(LFailure, TNyxText(E.Message));
      end;
      {$endif}
    end;
  end;
  FScope := nil;
  FProjection := nil;
  FBinding := nil;
  FFence := nil;

  if not FInstalled or (FNotified and not FNotifying) then
  begin
    FChild := nil;
  end;

  RaiseCoordinatorFailure(LFailure, 'Coordinator disconnected; cleanup failed: ');
end;

procedure TCoordinatorMount.Disconnect;
var
  LLease: INyxPreparedCoordinator;
begin
  LLease := Self;
  Cleanup;
end;

procedure TCoordinatorMount.Validate;
var
  LLease: INyxPreparedCoordinator;
  LChild: INyxPreparedCoordinator;
  LProjection: INyxCoordinatorProjection;
  LScope: TCoordinatorScope;
  LScheduler: INyxScheduler;
begin
  LLease := Self;

  if FValidated or FInstalled or FRetired or FCleaned then
  begin
    raise ENyxCoordinator.Create('Coordinator preparation is single-use');
  end;
  LChild := FChild;
  LProjection := FProjection;
  LScope := FScope;
  LScope.CheckLive;
  LScheduler := LScope.FScheduler;
  LScheduler.RequireUI;
  LScope.CheckLive;
  LChild.Validate;
  LScope.CheckLive;
  LScope.Admit;
  FValidated := True;
  CheckReady;
end;

procedure TCoordinatorMount.CheckReady;
var
  LLease: INyxPreparedCoordinator;
  LProjection: INyxCoordinatorProjection;
  LScope: TCoordinatorScope;
  LFence: INyxPublicationReadiness;
begin
  LLease := Self;

  if not FValidated or FInstalled or FRetired or FCleaned then
  begin
    raise ENyxCoordinator.Create('Coordinator readiness requires an admitted preparation');
  end;
  LProjection := FProjection;
  LScope := FScope;
  LFence := FFence;
  LScope.CheckNodes;
  LFence.CheckReady;
  LScope.CheckNodes;
end;

procedure TCoordinatorMount.Install;
begin
  { Extension installations obey the existing nonthrowing, flag-only contract.
    Every child/scope lease was retained before this phase; none is released. }

  if FValidated and not FInstalled and not FRetired and not FCleaned then
  begin
    FChild.Install;
    FScope.FInstalled := True;
    FInstalled := True;
  end;
end;

procedure TCoordinatorMount.Notify;
var
  LLease: INyxPreparedCoordinator;
  LChild: INyxPreparedCoordinator;
begin
  LLease := Self;

  if not FInstalled or FNotified then
  begin
    Exit;
  end;
  FNotified := True;
  FNotifying := True;
  LChild := FChild;
  try

    if LChild <> nil then
    begin
      LChild.Notify;
    end;
  finally
    FNotifying := False;

    if FCleaned and (LChild <> nil) then
    begin
      try
        LChild.Retire;
      except
        { Retirement stays nonthrowing after the notification attempt. }
      end;
      FChild := nil;
    end;
  end;
end;

procedure TCoordinatorMount.Retire;
var
  LLease: INyxPreparedCoordinator;
  LChild: INyxPreparedCoordinator;
begin
  LLease := Self;

  if FRetired then
  begin
    Exit;
  end;
  FRetired := True;

  if FInstalled then
  begin
    LChild := FChild;

    if (LChild <> nil) and
      (not FCleaned or (FNotified and not FNotifying)) then
    begin
      try
        LChild.Retire;
      except
        { Retire never throws or revokes an accepted lifetime owner. }
      end;
    end;
  end
  else
  begin
    try
      Cleanup;
    except
      { Refused preparation has already revoked and attempted every resource. }
    end;
  end;
end;

function TCoordinatorMount.Enrich(const AEvent: TNyxEventInfo): TNyxEventInfo;
var
  LLease: INyxPreparedCoordinator;
  LProjection: INyxCoordinatorProjection;
  LScope: TCoordinatorScope;
  LGeneration: INyxCancellationScope;
  LScheduler: INyxScheduler;
  LBinding: INyxCoordinatorBinding;
  LEnrichment: INyxCoordinatorEventEnrichment;
  LInput: TNyxEventInfo;
  LCancelled: Boolean;

  function Current: Boolean;
  begin
    Result := (LScope <> nil) and (LGeneration <> nil) and not FCleaned and
      FInstalled and LScope.FInstalled and not LScope.FRevoked and not LScope.FCleaned;

    if Result then
    begin
      LCancelled := LGeneration.Cancelled;
      Result := not LCancelled and not FCleaned and FInstalled and
        not LScope.FRevoked and not LScope.FCleaned and LScope.FInstalled and
        (LScope.FGeneration = LGeneration);
    end;
  end;

begin
  LLease := Self;
  LInput := AEvent.Copy;
  Result := LInput.Copy;
  LProjection := FProjection;
  LScope := FScope;
  LBinding := FBinding;

  if (LScope = nil) or (LBinding = nil) then
  begin
    Exit;
  end;
  LGeneration := LScope.FGeneration;
  LScheduler := LScope.FScheduler;

  if not Current or not Supports(LBinding, INyxCoordinatorEventEnrichment, LEnrichment) then
  begin
    Exit;
  end;

  if not Current then
  begin
    Exit;
  end;
  LScheduler.RequireUI;

  if not Current then
  begin
    Exit;
  end;

  if not LBinding.Connected or not Current then
  begin
    Exit;
  end;
  LScope.CheckNodes;
  Result := LEnrichment.Enrich(LInput);

  if not Current then
  begin
    Result := LInput.Copy;
    Exit;
  end;
  LScope.CheckNodes;
end;

function NyxHasCoordinatorMounts(ARoot: TNyxNode;
  const ASnapshot: INyxCoordinatorRegistrySnapshot): Boolean;
var
  LRoot: INyxControl;
  LSnapshot: INyxCoordinatorRegistrySnapshot;

  function Visit(ANode: TNyxNode): Boolean;
  var
    LIndex: Integer;
  begin
    Result := Match(ANode, LSnapshot) >= 0;

    if Result then
    begin
      Exit;
    end;
    for LIndex := 0 to ANode.Count - 1 do
    begin

      if Visit(ANode.Children[LIndex]) then
      begin
        Exit(True);
      end;
    end;
  end;

begin
  Result := False;

  if ARoot = nil then
  begin
    Exit;
  end;
  LRoot := TNyxControl.CreateFromNode(ARoot);
  LSnapshot := CopyRegistry(ASnapshot);
  Result := Visit(LRoot.Node);
end;

procedure PrepareNyxCoordinatorMounts(ARoot: TNyxNode;
  const AViews: INyxCollectionBindings; const AEventView: INyxPreparedEventView;
  const AScheduler: INyxScheduler; const ASnapshot: INyxCoordinatorRegistrySnapshot;
  const ARefreshTarget: INyxCoordinatorRefreshTarget;
  out AMounts: TNyxCoordinatorMounts; out AIDs: TNyxCoordinatorMountIDs);
var
  LRoot: INyxControl;
  LViews: INyxCollectionBindings;
  LView: INyxPreparedEventView;
  LScheduler: INyxScheduler;
  LSnapshot: INyxCoordinatorRegistrySnapshot;
  LTarget: INyxCoordinatorRefreshTarget;
  LMounts: TNyxCoordinatorMounts;
  LIDs: TNyxCoordinatorMountIDs;
  LControls: array of INyxControl;
  LParents: array of TNyxNode;
  LMatches: array of Integer;
  LIntents: array of TNyxDataValue;
  LScope: TCoordinatorScope;
  LProjection: INyxCoordinatorProjection;
  LRefresh: INyxCoordinatorRefresh;
  LChild: INyxPreparedCoordinator;
  LFactory: INyxCoordinatorFactory;
  LCanonical: INyxControl;
  LCanonicalFactory: INyxCanonicalCoordinatorFactory;
  LHasCanonicalFactory: Boolean;
  LIndex: Integer;

  procedure Collect(ANode: TNyxNode);
  var
    LMatch: Integer;
    LSlot: Integer;
    LChildIndex: Integer;
  begin
    LMatch := Match(ANode, LSnapshot);

    if LMatch >= 0 then
    begin
      LSlot := Length(LControls);
      SetLength(LControls, LSlot + 1);
      SetLength(LParents, LSlot + 1);
      SetLength(LMatches, LSlot + 1);
      SetLength(LIntents, LSlot + 1);
      LControls[LSlot] := TNyxControl.CreateFromNode(ANode);
      LParents[LSlot] := ANode.Parent;
      LMatches[LSlot] := LMatch;
      LIntents[LSlot] := ANode.Extensions.Value(LSnapshot.IntentKey(LMatch)).Copy;
    end;
    for LChildIndex := 0 to ANode.Count - 1 do
    begin
      Collect(ANode.Children[LChildIndex]);
    end;
  end;

begin
  { Copy every managed caller input before the first snapshot getter/factory.
    Local output vectors avoid captured out-argument cells in pas2js. }
  LViews := AViews;
  LView := AEventView;
  LScheduler := AScheduler;
  LTarget := ARefreshTarget;
  AMounts := nil;
  AIDs := nil;

  if ARoot = nil then
  begin
    Exit;
  end;
  LRoot := TNyxControl.CreateFromNode(ARoot);
  LSnapshot := CopyRegistry(ASnapshot);

  if not LRoot.Node.IsRealized then
  begin
    raise ENyxCoordinator.Create('Coordinator traversal requires an already-realized root');
  end;
  Collect(LRoot.Node);
  SetLength(LMounts, Length(LControls));
  SetLength(LIDs, Length(LControls));
  try
    for LIndex := 0 to High(LControls) do
    begin

      if (LRoot.Node.Find(LControls[LIndex].ID) <> LControls[LIndex].Node) or
        (LControls[LIndex].Node.Parent <> LParents[LIndex]) or
        (LControls[LIndex].Node.Kind <> LSnapshot.Kind(LMatches[LIndex]).Name) then
      begin
        raise ENyxCoordinator.Create('A discovered coordinator target changed before preparation');
      end;
      LScope := TCoordinatorScope.Create(LRoot.Node, LControls[LIndex].Node,
        LSnapshot.IntentKey(LMatches[LIndex]), LViews, LView, LScheduler, LTarget);
      LProjection := LScope;
      LRefresh := LScope;
      try

        if LScope.FIntent <> LIntents[LIndex].ToJSON then
        begin
          raise ENyxCoordinator.Create('Coordinator intent changed before factory preparation');
        end;
        LFactory := LSnapshot.Factory(LMatches[LIndex]);
        LCanonical := LControls[LIndex];
        LCanonicalFactory := nil;
        { QueryInterface is an outward factory call. Keep all candidate leases
          local, then recheck the exact canonical roles before granting the
          internal built-in channel. Ordinary creator factories keep their
          existing Node-free projection and unchanged Prepare contract. }
        LHasCanonicalFactory := Supports(LFactory,
          INyxCanonicalCoordinatorFactory, LCanonicalFactory);
        LScope.CheckNodes;

        if LHasCanonicalFactory then
        begin
          LChild := LCanonicalFactory.PrepareCanonical(LCanonical, LProjection,
            LIntents[LIndex].Copy, LViews, LView, LScheduler, LRefresh);
        end
        else
        begin
          LChild := LFactory.Prepare(LProjection, LIntents[LIndex].Copy,
            LViews, LView, LScheduler, LRefresh);
        end;
        LScope.CheckLive;
        LMounts[LIndex] := TCoordinatorMount.Create(LScope, LProjection, LChild);
        LIDs[LIndex] := LScope.Root.ID;
      except
        LScope.Revoke;

        if LChild <> nil then
        begin
          try
            LChild.Disconnect;
          except
            { Preserve preparation refusal after attempting child cleanup. }
          end;
          try
            LChild.Retire;
          except
            { Preparation refusal remains the primary error. }
          end;
        end;
        try
          LScope.Cleanup;
        except
          { Preserve the factory/admission refusal after all scope cleanup. }
        end;
        raise;
      end;
      LChild := nil;
      LCanonicalFactory := nil;
      LCanonical := nil;
      LProjection := nil;
      LRefresh := nil;
      LScope := nil;
    end;
    AMounts := LMounts;
    AIDs := LIDs;
  except
    try
      DisconnectNyxCoordinatorMounts(LMounts);
    except
      { All candidate cleanup is attempted without concealing initial refusal. }
    end;
    raise;
  end;
end;

procedure ValidateNyxCoordinatorMounts(const AMounts: TNyxCoordinatorMounts);
var
  LMounts: TNyxCoordinatorMounts;
  LIndex: Integer;
begin
  LMounts := Copy(AMounts, 0, Length(AMounts));
  for LIndex := 0 to High(LMounts) do
  begin
    LMounts[LIndex].Validate;
  end;
end;

procedure CheckNyxCoordinatorMounts(const AMounts: TNyxCoordinatorMounts);
var
  LMounts: TNyxCoordinatorMounts;
  LIndex: Integer;
  LFence: INyxPublicationReadiness;
begin
  LMounts := Copy(AMounts, 0, Length(AMounts));
  for LIndex := 0 to High(LMounts) do
  begin

    if not Supports(LMounts[LIndex], INyxPublicationReadiness, LFence) then
    begin
      raise ENyxCoordinator.Create('Coordinator driver lost a final readiness capability');
    end;
    LFence.CheckReady;
  end;
end;

procedure InstallNyxCoordinatorMounts(const AMounts: TNyxCoordinatorMounts);
var
  LIndex: Integer;
begin
  { All vector and participant leases must already be held by the host. }
  for LIndex := 0 to High(AMounts) do
  begin
    AMounts[LIndex].Install;
  end;
end;

procedure RevokeNyxCoordinatorMounts(const AMounts: TNyxCoordinatorMounts);
var
  LIndex: Integer;
begin
  for LIndex := 0 to High(AMounts) do
  begin

    if AMounts[LIndex] <> nil then
    begin
      AMounts[LIndex].Revoke;
    end;
  end;
end;

procedure NotifyNyxCoordinatorMounts(const AMounts: TNyxCoordinatorMounts);
var
  LMounts: TNyxCoordinatorMounts;
  LIndex: Integer;
  LFailure: TNyxText;
begin
  LMounts := Copy(AMounts, 0, Length(AMounts));
  LFailure := '';
  for LIndex := 0 to High(LMounts) do
  begin
    try
      LMounts[LIndex].Notify;
    except
      on E: Exception do
      begin

        if LFailure = '' then
        begin
          LFailure := TNyxText(E.Message);
        end;
      end;
    end;
  end;

  if LFailure <> '' then
  begin
    raise ENyxPublicationNotification.CreateReceiverFailure(LFailure);
  end;
end;

procedure RetireNyxCoordinatorMounts(const AMounts: TNyxCoordinatorMounts);
var
  LMounts: TNyxCoordinatorMounts;
  LIndex: Integer;
begin
  LMounts := Copy(AMounts, 0, Length(AMounts));
  for LIndex := 0 to High(LMounts) do
  begin
    try

      if LMounts[LIndex] <> nil then
      begin
        LMounts[LIndex].Retire;
      end;
    except
      { Retire attempts all participants and never turns commit into rollback. }
    end;
  end;
end;

procedure DisconnectNyxCoordinatorMounts(var AMounts: TNyxCoordinatorMounts);
var
  LMounts: TNyxCoordinatorMounts;
  LIndex: Integer;
  LFailure: TCoordinatorFailure;
begin
  LMounts := AMounts;
  AMounts := nil;
  LFailure := Default(TCoordinatorFailure);
  { Revoke every scope before invoking the first arbitrary cleanup. }
  RevokeNyxCoordinatorMounts(LMounts);
  for LIndex := 0 to High(LMounts) do
  begin
    try

      if LMounts[LIndex] <> nil then
      begin
        LMounts[LIndex].Disconnect;
      end;
    except
      {$ifdef PAS2JS}
      CaptureCoordinatorFailure(LFailure, JSExceptValue);
      {$else}
      on E: Exception do
      begin
        CaptureCoordinatorFailure(LFailure, TNyxText(E.Message));
      end;
      {$endif}
    end;
  end;
  RetireNyxCoordinatorMounts(LMounts);
  RaiseCoordinatorFailure(LFailure, 'Coordinator vector retired; cleanup failed: ');
end;

function EnrichNyxCoordinatorEvent(const AMounts: TNyxCoordinatorMounts;
  const AEvent: TNyxEventInfo): TNyxEventInfo;
var
  LMounts: TNyxCoordinatorMounts;
  LEnrichment: INyxCoordinatorEventEnrichment;
  LIndex: Integer;
begin
  LMounts := Copy(AMounts, 0, Length(AMounts));
  Result := AEvent.Copy;
  for LIndex := 0 to High(LMounts) do
  begin

    if Supports(LMounts[LIndex], INyxCoordinatorEventEnrichment, LEnrichment) then
    begin
      Result := LEnrichment.Enrich(Result);
    end;
  end;
end;

function FindNyxCoordinatorMount(const AMounts: TNyxCoordinatorMounts;
  const AIDs: TNyxCoordinatorMountIDs; const AID: TNyxText): INyxCoordinatorBinding;
var
  LMounts: TNyxCoordinatorMounts;
  LIDs: TNyxCoordinatorMountIDs;
  LID: TNyxText;
  LIndex: Integer;
begin
  LMounts := Copy(AMounts, 0, Length(AMounts));
  LIDs := Copy(AIDs, 0, Length(AIDs));
  LID := AID;
  Result := nil;

  if Length(LMounts) <> Length(LIDs) then
  begin
    raise ENyxCoordinator.Create('Coordinator mount identities must match their owner vector');
  end;
  for LIndex := 0 to High(LMounts) do
  begin

    if LIDs[LIndex] = LID then
    begin
      Exit(LMounts[LIndex].Binding);
    end;
  end;
end;

{$I nyx.coordinators.inputs.inc}

end.
