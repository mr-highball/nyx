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

unit nyx.model;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  Classes,
  nyx.text,
  nyx.data,
  nyx.contract,
  nyx.types,
  nyx.state,
  nyx.collections.registry,
  nyx.collections.view.types,
  nyx.binding.types,
  SysUtils;

const
  NyxMaximumIDScalars = 128;
  NyxMaximumTreeDepth = 128;
  NyxMaximumNodes = 10000;

type
  { Automatic lookup prefers an exact runtime key, then an editable design ID.
    Explicit modes disambiguate an authored slash name from a qualified view key. }
  TNyxIdentityKind = (niAutomatic, niRuntime, niDesign);
  { Contract failures are distinct from platform and compiler errors. Callers
    may report these as actionable design diagnostics without exposing a DOM
    or LCL exception type through the portable API. }
  ENyxModel = class(Exception)
  public
    { Older Windows FPC's Exception constructor takes system-tagged String.
      Store UTF-8 diagnostic bytes without that implicit narrowing conversion;
      inherited Message remains usable by ordinary Exception handlers. }
    constructor Create(const AMessage: TNyxText); reintroduce;
  end;
  TNyxDocument = class;
  TNyxNode = class;
  TNyxNodeConfig = class;
  TNyxNodeBindings = class;

  { Minimal implementation-neutral bridge for managed authoring controls.
    Implementations must retain Node for their entire interface lifetime. The
    descriptor is borrowed: callers never Free it. Renderers/codec consume this
    portable descriptor, rather than requiring a particular control class. }
  INyxNode = interface(IInterface)
    ['{737A7921-4621-4C6F-8C01-010000000001}']
    function GetNode: TNyxNode;
    property Node: TNyxNode read GetNode;
  end;

  { The smallest renderer-independent building block.

    Kind selects a registered component recipe. A design node's ID is authored
    identity; a realized node's ID is its qualified runtime key. SourceID always
    retains the original template ID. DesignID identifies the editable owner
    (the selecting instance for inherited parts, the payload for overridden parts).
    Properties preserve insertion order and extension keys for deterministic
    serialization and generation. Configure exposes typed authoring; text wire
    values remain the codec/adapter boundary, with shared admission metadata.

    A node owns its descendants. Parent is a borrowed pointer. Once Add or
    Insert succeeds, callers must not free the child. Interface adoption also
    retains the actual implementation; its weak parent link creates no cycle.
    Disposing an owner releases its token, while retained interfaces keep their
    descriptors alive. Raw Extract returns ownership to the caller; managed
    extraction transfers a retained interface. Clone makes an independent tree
    with the same IDs, portable meaning and no shared implementation objects.
    Trees with cloned IDs may live in different documents, but Validate rejects
    duplicate identities inside one document. }
  TNyxNode = class
  private
    FKind: TNyxText;
    FID: TNyxText;
    FRuntimeID: TNyxText;
    FDesignID: TNyxText;
    FParent: TNyxNode;
    FOwner: TNyxDocument;
    FProps: TNyxStrings;
    FExtensions: TNyxExtensions;
    FChildren: array of TNyxNode;
    FChildReferences: array of INyxNode;
    FConfigure: TNyxNodeConfig;
    FPlatformConfigure: array[npfBrowser..npfNativeLCL] of TNyxNodeConfig;
    FContract: TNyxContract;
    FBindingConfig: TNyxNodeBindings;
    FStateBindings: array of TNyxBindingSpec;
    FHasCollectionView: Boolean;
    FCollectionView: TNyxCollectionViewSpec;
    FInstanceScopeID: TNyxText;
    FReferences: Integer;
    FRawOwnership: Boolean;
    function GetConfigure: TNyxNodeConfig;
    function GetContract: TNyxContract;
    function GetBindingConfig: TNyxNodeBindings;
    function GetBindingCount: Integer;
    function GetStateBinding(AIndex: Integer): TNyxBindingSpec;
    function GetCollectionView: TNyxCollectionViewSpec;
    function GetCount: Integer;
    function GetChild(AIndex: Integer): TNyxNode;
    function GetProjectionKind: TNyxText;
    function GetID: TNyxText;
    function GetDesignID: TNyxText;
    function GetIsRealized: Boolean;
    procedure Admit(ANode: TNyxNode);
  public
    { Empty IDs receive a process-local convenience identity. Serialized designs
      and generated source always preserve explicit IDs rather than relying on
      that counter. Failed admission leaves ownership with the caller. }
    constructor Create(const AKind: TNyxText; const AID: TNyxText = ''); overload;
    constructor Create(AKind: TNyxKind; const AID: TNyxText = ''); overload;
    constructor Create(const AKind: TNyxKindRef; const AID: TNyxText = ''); overload;
    { Composition boundary: construct an independently owned runtime node while
      retaining its authored source and editable owner identities. Runtime keys
      use NyxQualifiedID escaping and expansion budgets, not the authored limit.
      Realized identities are immutable and cannot be admitted into a design. }
    class function CreateRealized(const AKind, ASourceID, ARuntimeID,
      ADesignID: TNyxText): TNyxNode; static;
    destructor Destroy; override;
    { Managed authoring boundary. Each interface implementation acquires one
      descriptor reference and releases it at destruction. A raw constructor
      starts with caller ownership; ReleaseOwnership explicitly transfers that
      token to managed references. Tree/document owners release their token when
      disposing a node, allowing retained controls to survive independently.
      Parent/document links are weak. Never call Free on a borrowed descriptor. }
    procedure AcquireReference;
    procedure ReleaseReference;
    procedure ReleaseOwnership;
    { Retain the actual implementation admitted through an interface, when an
      owner has one. Raw descriptor adoption has no implementation reference.
      The node never retains a reference to its own implementation. }
    function ComponentReference: INyxNode;
    procedure BeforeDestruction; override;
    { Fluent mutations return this node, allowing composition with Add calls.
      Named admits 1..128 Unicode scalar values, refusing controls, whitespace-only
      IDs and malformed encoding. Exact text is retained without normalization.
      Failed rename preserves identity; document uniqueness is checked separately. }
    function Named(const AID: TNyxText): TNyxNode;
    { Low-level codec/adapter/legacy boundary; application authoring uses Configure.
      SetProp preserves property position and accepts an empty value. Prop
      distinguishes an absent key from a present empty value. }
    function SetProp(const AKey, AValue: TNyxText): TNyxNode;
    function Prop(const AKey: TNyxText; const ADefault: TNyxText = ''): TNyxText;
    function Add(ANode: TNyxNode): TNyxNode; overload;
    function Add(const ANode: INyxNode): TNyxNode; overload;
    { Insert accepts 0..Count. Ownership/cycle checks run before mutation. }
    procedure Insert(AIndex: Integer; ANode: TNyxNode); overload;
    procedure Insert(AIndex: Integer; const ANode: INyxNode); overload;
    { Extract detaches without freeing; Remove detaches and frees. }
    function Extract(AIndex: Integer): TNyxNode;
    procedure Remove(ANode: TNyxNode);
    function Find(const AID: TNyxText): TNyxNode;
    { Borrow a named compound part. Slash-separated paths describe nested slots,
      e.g. Part('actions/primary'); '.' borrows the root itself. A missing part raises a contract diagnostic
      instead of returning nil and failing later in a fluent customization chain. }
    function Part(const APath: TNyxText): TNyxNode; overload;
    { Typed authoring path. Other reference families cannot borrow a part. }
    function Part(const APath: TNyxPartRef): TNyxNode; overload;
    { A reusable reference cannot directly borrow definition parts for mutation.
      OverridePart returns an owned descriptor on this instance instead. Path
      addresses a named part in its realized definition. Properties customize
      that part; append/prepend add payload children, replace substitutes exactly
      one part, and remove deletes it. Definitions and other instances stay intact.
      Repeated calls for the same path return the existing descriptor. Omitted
      mode keeps an existing operation; a new descriptor starts with properties. }
    function OverridePart(const APath: TNyxText;
      const AMode: TNyxText = ''): TNyxNode; overload;
    { Omission preserves an existing operation; the enum overload deliberately
      selects a closed operation. Returned descriptors remain instance-owned. }
    function OverridePart(const APath: TNyxPartRef): TNyxNode; overload;
    function OverridePart(const APath: TNyxPartRef;
      AMode: TNyxOverrideMode): TNyxNode; overload;
    function Clone: TNyxNode;
    { Explicit descriptor/codec boundary. Copies immutable data and replaces a
      target in place; failed validation preserves metadata and its ordering. }
    procedure SetBinding(const ASpec: TNyxBindingSpec);
    { Remove only a local descriptor, restoring inherited behavior on the next
      realization. Clear instead records a deliberate inherited unbinding. }
    procedure RemoveBinding(ATarget: TNyxBindingProperty);
    function FindBinding(ATarget: TNyxBindingProperty; out ASpec: TNyxBindingSpec): Boolean;
    { A defined spec binds data; an absent spec records explicit inherited
      unbinding. RemoveCollectionView restores inheritance instead. Values are
      copied, never shared mutable configuration or runtime stores. }
    procedure SetCollectionView(const ASpec: TNyxCollectionViewSpec);
    procedure RemoveCollectionView;
    property HasCollectionView: Boolean read FHasCollectionView;
    property CollectionView: TNyxCollectionViewSpec read GetCollectionView;
    { Realization-only nearest reusable/compound owner identity. This is runtime
      metadata, excluded from persistence and authored source. It is immutable
      design meaning's scope bridge; no owner object is retained. }
    procedure SetInstanceScope(const AID: TNyxText);
    property InstanceScopeID: TNyxText read FInstanceScopeID;
    property Kind: TNyxText read FKind;
    { Borrow this node's lazily allocated typed fluent configuration object. }
    property Configure: TNyxNodeConfig read GetConfigure;
    { Borrow this node's owned scalar/field/event specification facade. Wire data
      lives in the namespaced immutable extension; clones never share a facade. }
    property Contract: TNyxContract read GetContract;
    { Node-owned typed fluent binding object; never free it separately. }
    property Binds: TNyxNodeBindings read GetBindingConfig;
    property BindingCount: Integer read GetBindingCount;
    property Bindings[AIndex: Integer]: TNyxBindingSpec read GetStateBinding;
    { Semantic kind remains the extension's identity. ProjectionKind preserves
      a recipe's base primitive via a persisted property, so derived controls
      retain layout, input and event semantics without a runtime catalog. }
    property ProjectionKind: TNyxText read GetProjectionKind;
    property ID: TNyxText read GetID;
    property SourceID: TNyxText read FID;
    property DesignID: TNyxText read GetDesignID;
    property IsRealized: Boolean read GetIsRealized;
    property Parent: TNyxNode read FParent;
    property Props: TNyxStrings read FProps;
    { Borrowed opaque structured data, owned by this node. Recognized node fields
      are protected. Clone/realization preserve immutable independent snapshots. }
    property Extensions: TNyxExtensions read FExtensions;
    property Count: Integer read GetCount;
    property Children[AIndex: Integer]: TNyxNode read GetChild;
  end;

  { Node-owned fluent configuration. Every method returns this borrowed facade;
    Done returns its node for composition. Never free or retain the facade after
    its node. Clones lazily create their own facade, with no references to the
    original. Values are typed here; strings are serialized only at SetProp.
    Individual numeric bounds fail before mutation. Cross-property/reference
    constraints are admitted by the document/Studio command boundary. }
  TNyxNodeConfig = class
  private
    FNode: TNyxNode;
    FPlatform: TNyxPlatform;
    constructor Create(ANode: TNyxNode);
    function Put(AKey: TNyxAttribute; const AValue: TNyxText): TNyxNodeConfig;
    function PutInteger(AKey: TNyxAttribute; AValue: Integer): TNyxNodeConfig;
    function PutBoolean(AKey: TNyxAttribute; AValue: Boolean): TNyxNodeConfig;
    function PutValue(const AValue: TNyxStateValue): TNyxNodeConfig;
  public
    { Returns an independent node-owned scope. Defaults remain unchanged when
      a scoped facade is retained. npfAny returns the ordinary configuration.
      Managed controls expose the same fluent contract with retained ownership. }
    function ForPlatform(APlatform: TNyxPlatform): TNyxNodeConfig;
    function SplitOrientation(AValue: TNyxSplitOrientation): TNyxNodeConfig;
    function SplitPosition(APercent: Integer): TNyxNodeConfig;
    function SplitMinimum(APercent: Integer): TNyxNodeConfig;
    function SplitMaximum(APercent: Integer): TNyxNodeConfig;
    function SplitResizable(AValue: Boolean): TNyxNodeConfig;
    { Source/target opt-in does not supply a payload or silently accept a drop.
      Active sequential drag callbacks offer data and negotiate operations. }
    function DragSource(AValue: Boolean): TNyxNodeConfig;
    function DropTarget(AValue: Boolean): TNyxNodeConfig;
    function TouchBehavior(AValue: TNyxTouchBehavior): TNyxNodeConfig;
    { Captions/user data stay text. Value overloads distinguish editable text,
      numeric values and checked state at the call site. }
    function Text(const AValue: TNyxText): TNyxNodeConfig;
    function Placeholder(const AValue: TNyxText): TNyxNodeConfig;
    function Items(const AValue: TNyxText): TNyxNodeConfig;
    function Hint(const AValue: TNyxText): TNyxNodeConfig;
    function AccessibleName(const AValue: TNyxText): TNyxNodeConfig;
    function LinkTo(const AValue: TNyxText): TNyxNodeConfig;
    function Source(const AValue: TNyxText): TNyxNodeConfig;
    function AlternativeText(const AValue: TNyxText): TNyxNodeConfig;
    function Option(const AValue: TNyxText): TNyxNodeConfig; overload;
    function Option(AValue: Integer): TNyxNodeConfig; overload;
    function Option(AValue: Boolean): TNyxNodeConfig; overload;
    function Option(AValue: Double): TNyxNodeConfig; overload;
    function Value(const AValue: TNyxText): TNyxNodeConfig; overload;
    function Value(AValue: Integer): TNyxNodeConfig; overload;
    function Value(AValue: Boolean): TNyxNodeConfig; overload;
    function Value(AValue: Double): TNyxNodeConfig; overload;
    { Pixel/layout integers and portable numeric limits. }
    function Padding(AValue: Integer): TNyxNodeConfig;
    function Gap(AValue: Integer): TNyxNodeConfig;
    function Columns(AValue: Integer): TNyxNodeConfig;
    function Width(AValue: Integer): TNyxNodeConfig;
    function Height(AValue: Integer): TNyxNodeConfig;
    function Left(AValue: Integer): TNyxNodeConfig;
    function Top(AValue: Integer): TNyxNodeConfig;
    function Flex(AValue: Integer): TNyxNodeConfig;
    function Minimum(AValue: Integer): TNyxNodeConfig;
    function Maximum(AValue: Integer): TNyxNodeConfig;
    { Boolean options never accept text such as 'true'. }
    function Enabled(AValue: Boolean): TNyxNodeConfig;
    function Visible(AValue: Boolean): TNyxNodeConfig;
    function ReadOnly(AValue: Boolean): TNyxNodeConfig;
    function Surface(AValue: Boolean): TNyxNodeConfig;
    function Compound(AValue: Boolean): TNyxNodeConfig;
    function Pressed(AValue: Boolean): TNyxNodeConfig;
    { Closed behavioral vocabularies use enums, including built-in projections. }
    function Layout(AValue: TNyxLayoutMode): TNyxNodeConfig;
    function Variant(AValue: TNyxVariant): TNyxNodeConfig;
    function Action(AValue: TNyxAction): TNyxNodeConfig;
    function ProjectAs(AValue: TNyxKind): TNyxNodeConfig;
    function OverrideMode(AValue: TNyxOverrideMode): TNyxNodeConfig;
    function InputType(AValue: TNyxInputType): TNyxNodeConfig;
    { Open references retain distinct types. Application event names are data;
      dispatch actions are the closed TNyxAction vocabulary above. }
    function PartName(const AValue: TNyxPartRef): TNyxNodeConfig;
    function Target(const AValue: TNyxPartRef): TNyxNodeConfig;
    function OverridePath(const AValue: TNyxPartRef): TNyxNodeConfig;
    function Component(const AValue: TNyxComponentRef): TNyxNodeConfig;
    function OnClick(const AValue: TNyxEventRef): TNyxNodeConfig;
    function OnChange(const AValue: TNyxEventRef): TNyxNodeConfig;
    { Clear preserves a present empty property. Metadata is an explicit codec /
      compatibility boundary for noncanonical legacy values. Extension refuses
      built-in keys; it cannot silently bypass typed configuration methods. }
    function Clear(AKey: TNyxAttribute): TNyxNodeConfig;
    function Metadata(AKey: TNyxAttribute; const AValue: TNyxText): TNyxNodeConfig;
    function CustomVariant(const AStyle: TNyxStyleRef): TNyxNodeConfig;
    function CustomProjection(const AKind: TNyxKindRef): TNyxNodeConfig;
    function Extension(const AKey, AValue: TNyxText): TNyxNodeConfig;
    function Done: TNyxNode;
  end;

  { Lazy node-owned binding facade. References keep their scalar type; getters
    and live admission refuse a different store kind. Text explicitly projects
    scalar captions; other targets retain Boolean/integer/text property types.
    Done returns the node for composition. Clone lazily owns a new facade. }
  TNyxNodeBindings = class
  private
    FNode: TNyxNode;
    constructor Create(ANode: TNyxNode);
    function Put(ATarget: TNyxBindingProperty; const AKey: TNyxText;
      AKind: TNyxStateKind; ADirection: TNyxBindingDirection): TNyxNodeBindings;
  public
    function Text(const AState: TNyxTextStateRef): TNyxNodeBindings; overload;
    function Text(const AState: TNyxBooleanStateRef): TNyxNodeBindings; overload;
    function Text(const AState: TNyxIntegerStateRef): TNyxNodeBindings; overload;
    function Text(const AState: TNyxNumberStateRef): TNyxNodeBindings; overload;
    function Value(const AState: TNyxTextStateRef;
      ADirection: TNyxBindingDirection = bdTwoWay): TNyxNodeBindings; overload;
    function Value(const AState: TNyxBooleanStateRef;
      ADirection: TNyxBindingDirection = bdTwoWay): TNyxNodeBindings; overload;
    function Value(const AState: TNyxIntegerStateRef;
      ADirection: TNyxBindingDirection = bdTwoWay): TNyxNodeBindings; overload;
    function Value(const AState: TNyxNumberStateRef;
      ADirection: TNyxBindingDirection = bdTwoWay): TNyxNodeBindings; overload;
    function Enabled(const AState: TNyxBooleanStateRef): TNyxNodeBindings;
    function Visible(const AState: TNyxBooleanStateRef): TNyxNodeBindings;
    function ReadOnly(const AState: TNyxBooleanStateRef): TNyxNodeBindings;
    function Pressed(const AState: TNyxBooleanStateRef): TNyxNodeBindings;
    function Placeholder(const AState: TNyxTextStateRef): TNyxNodeBindings;
    function Hint(const AState: TNyxTextStateRef): TNyxNodeBindings;
    function AccessibleName(const AState: TNyxTextStateRef): TNyxNodeBindings;
    function Width(const AState: TNyxIntegerStateRef): TNyxNodeBindings;
    function Height(const AState: TNyxIntegerStateRef): TNyxNodeBindings;
    function Left(const AState: TNyxIntegerStateRef): TNyxNodeBindings;
    function Top(const AState: TNyxIntegerStateRef): TNyxNodeBindings;
    function Padding(const AState: TNyxIntegerStateRef): TNyxNodeBindings;
    function Gap(const AState: TNyxIntegerStateRef): TNyxNodeBindings;
    function Columns(const AState: TNyxIntegerStateRef): TNyxNodeBindings;
    function Flex(const AState: TNyxIntegerStateRef): TNyxNodeBindings;
    function Minimum(const AState: TNyxIntegerStateRef): TNyxNodeBindings;
    function Maximum(const AState: TNyxIntegerStateRef): TNyxNodeBindings;
    { Clear retains an explicit inherited-unbinding operation for composition. }
    function Clear(ATarget: TNyxBindingProperty): TNyxNodeBindings;
    function Inherit(ATarget: TNyxBindingProperty): TNyxNodeBindings;
    { Typed data projection belongs to the same node-owned binding facade.
      Clear suppresses inherited data; Inherit removes only this local override. }
    function Collection(const ASpec: TNyxCollectionViewSpec): TNyxNodeBindings;
    function ClearCollection: TNyxNodeBindings;
    function InheritCollection: TNyxNodeBindings;
    function Done: TNyxNode;
  end;

  { A design/application owns page roots and reusable component definitions.
    A component instance is a node of kind 'component' whose 'component' property
    names a definition's ID. Definitions stay shared design data; adapters expand
    them into target controls without transferring or duplicating model ownership.

    Destroying a document releases every admitted root. Validate is the explicit
    acceptance boundary used before serialization, generation and rendering. }
  TNyxDocument = class
  private
    FTitle: TNyxText;
    FState: TNyxState;
    FCollections: INyxCollectionDefaults;
    FExtensions: TNyxExtensions;
    FPages: array of TNyxNode;
    FComponents: array of TNyxNode;
    FPageReferences: array of INyxNode;
    FComponentReferences: array of INyxNode;
    function GetCount: Integer;
    function GetPage(AIndex: Integer): TNyxNode;
    function GetComponentCount: Integer;
    function GetComponent(AIndex: Integer): TNyxNode;
    function GetHasCollectionViews: Boolean;
    procedure AdmitRoot(ANode: TNyxNode);
  public
    constructor Create;
    destructor Destroy; override;
    function AddPage(ANode: TNyxNode): TNyxDocument; overload;
    function AddPage(const ANode: INyxNode): TNyxDocument; overload;
    function AddComponent(ANode: TNyxNode): TNyxDocument; overload;
    function AddComponent(const ANode: INyxNode): TNyxDocument; overload;
    function Find(const AID: TNyxText): TNyxNode;
    function FindComponent(const AID: TNyxText): TNyxNode;
    function Clone: TNyxDocument;
    { Borrowed membership check through owned ancestry; does not search by text. }
    function Contains(ANode: TNyxNode): Boolean;
    { Checks identity, unresolved reusable definitions, cycles and finite budgets.
      This validates structural meaning; renderer/property admission is a
      separate catalog concern. An empty document is valid while being designed. }
    procedure Validate;
    property Title: TNyxText read FTitle write FTitle;
    { Borrowed authored defaults, owned by this document. Clone copies values
      without subscribers. Runtime applications must use an independent copy;
      ordinary control edits must not alter the saved application defaults. }
    property State: TNyxState read FState;
    { Managed authored collection defaults. Define admits immutable snapshots;
      applications materialize independent mutable stores. Retained registry or
      snapshot interfaces can safely outlive this document without backreferences. }
    property Collections: INyxCollectionDefaults read FCollections;
    { Includes deliberate clear descriptors; selects version-3 node semantics. }
    property HasCollectionViews: Boolean read GetHasCollectionViews;
    { Borrowed project-owned structured data. Unknown version-1 root fields are
      retained here; save/history/view builds and generation preserve them. }
    property Extensions: TNyxExtensions read FExtensions;
    property Count: Integer read GetCount;
    property Pages[AIndex: Integer]: TNyxNode read GetPage;
    property ComponentCount: Integer read GetComponentCount;
    property Components[AIndex: Integer]: TNyxNode read GetComponent;
  end;

{ Append an authored ID to a runtime scope. '~' becomes '~0', '/' becomes '~1'.
  Empty prefix produces a single escaped segment. Preserve complete Unicode text
  runs and ordinary ASCII keys; the source document's IDs are never rewritten. }
function NyxQualifiedID(const APrefix, ASourceID: TNyxText): TNyxText;
{ Release a caller-owned root token and clear the owner's pointer first.
  Retained component interfaces survive; nil is harmless. This is the owning
  adapter boundary for unmount/admission failure, rather than direct Free. }
procedure ReleaseNyxNode(var ANode: TNyxNode);

implementation

uses
  nyx.schema,
  nyx.collections.view;

procedure ReleaseNyxNode(var ANode: TNyxNode);
var
  LOwned: TNyxNode;
begin
  LOwned := ANode;
  ANode := nil;

  if LOwned <> nil then
  begin
    LOwned.ReleaseOwnership;
  end;
end;

function TNyxNode.GetCollectionView: TNyxCollectionViewSpec;
begin
  Result := FCollectionView.Copy;
end;

procedure TNyxNode.SetCollectionView(const ASpec: TNyxCollectionViewSpec);
begin
  ASpec.Validate;
  FCollectionView := ASpec.Copy;
  FHasCollectionView := True;
end;

procedure TNyxNode.RemoveCollectionView;
begin
  FHasCollectionView := False;
  FCollectionView := Default(TNyxCollectionViewSpec);
end;

procedure TNyxNode.SetInstanceScope(const AID: TNyxText);
begin

  if not IsRealized then
  begin
    raise ENyxModel.Create('Instance scope belongs to realized nodes only');
  end;
  FInstanceScopeID := AID;
end;

function TNyxNode.GetBindingConfig: TNyxNodeBindings;
begin

  if FBindingConfig = nil then
  begin
    FBindingConfig := TNyxNodeBindings.Create(Self);
  end;
  Result := FBindingConfig;
end;

function TNyxNode.GetBindingCount: Integer;
begin
  Result := Length(FStateBindings);
end;

function TNyxNode.GetStateBinding(AIndex: Integer): TNyxBindingSpec;
begin

  if (AIndex < 0) or (AIndex >= BindingCount) then
  begin
    raise ENyxModel.Create('State binding index out of range');
  end;
  Result := FStateBindings[AIndex].Copy;
end;

procedure TNyxNode.SetBinding(const ASpec: TNyxBindingSpec);
var
  LIndex: Integer;
begin
  ASpec.Validate;
  for LIndex := 0 to BindingCount - 1 do
  begin

    if FStateBindings[LIndex].Target = ASpec.Target then
    begin
      FStateBindings[LIndex] := ASpec.Copy;
      Exit;
    end;
  end;
  LIndex := BindingCount;
  SetLength(FStateBindings, LIndex + 1);
  FStateBindings[LIndex] := ASpec.Copy;
end;

procedure TNyxNode.RemoveBinding(ATarget: TNyxBindingProperty);
var
  LIndex: Integer;
  LNext: Integer;
begin
  TNyxBindingSpec.Clear(ATarget).Validate;
  for LIndex := 0 to BindingCount - 1 do
  begin

    if FStateBindings[LIndex].Target = ATarget then
    begin
      { Explicit record copies also preserve independent pas2js descriptors. }
      for LNext := LIndex to BindingCount - 2 do
      begin
        FStateBindings[LNext] := FStateBindings[LNext + 1].Copy;
      end;
      SetLength(FStateBindings, BindingCount - 1);
      Exit;
    end;
  end;
end;

function TNyxNode.FindBinding(ATarget: TNyxBindingProperty;
  out ASpec: TNyxBindingSpec): Boolean;
var
  LIndex: Integer;
begin
  ASpec := TNyxBindingSpec.Clear(ATarget);
  for LIndex := 0 to BindingCount - 1 do
  begin

    if FStateBindings[LIndex].Target = ATarget then
    begin
      ASpec := FStateBindings[LIndex].Copy;
      Exit(not ASpec.Cleared);
    end;
  end;
  Result := False;
end;

constructor TNyxNodeBindings.Create(ANode: TNyxNode);
begin
  inherited Create;
  FNode := ANode;
end;

function TNyxNodeBindings.Put(ATarget: TNyxBindingProperty; const AKey: TNyxText;
  AKind: TNyxStateKind; ADirection: TNyxBindingDirection): TNyxNodeBindings;
begin
  FNode.SetBinding(TNyxBindingSpec.Bound(ATarget, AKey, AKind, ADirection));
  Result := Self;
end;


function TNyxNodeBindings.Text(const AState: TNyxTextStateRef): TNyxNodeBindings;
begin
  Result := Put(bpText, AState.Name, nskText, bdFromState);
end;

function TNyxNodeBindings.Text(const AState: TNyxBooleanStateRef): TNyxNodeBindings;
begin
  Result := Put(bpText, AState.Name, nskBoolean, bdFromState);
end;

function TNyxNodeBindings.Text(const AState: TNyxIntegerStateRef): TNyxNodeBindings;
begin
  Result := Put(bpText, AState.Name, nskInteger, bdFromState);
end;

function TNyxNodeBindings.Text(const AState: TNyxNumberStateRef): TNyxNodeBindings;
begin
  Result := Put(bpText, AState.Name, nskNumber, bdFromState);
end;

function TNyxNodeBindings.Value(const AState: TNyxTextStateRef;
  ADirection: TNyxBindingDirection): TNyxNodeBindings;
begin
  Result := Put(bpValue, AState.Name, nskText, ADirection);
end;

function TNyxNodeBindings.Value(const AState: TNyxBooleanStateRef;
  ADirection: TNyxBindingDirection): TNyxNodeBindings;
begin
  Result := Put(bpValue, AState.Name, nskBoolean, ADirection);
end;

function TNyxNodeBindings.Value(const AState: TNyxIntegerStateRef;
  ADirection: TNyxBindingDirection): TNyxNodeBindings;
begin
  Result := Put(bpValue, AState.Name, nskInteger, ADirection);
end;

function TNyxNodeBindings.Value(const AState: TNyxNumberStateRef;
  ADirection: TNyxBindingDirection): TNyxNodeBindings;
begin
  Result := Put(bpValue, AState.Name, nskNumber, ADirection);
end;

function TNyxNodeBindings.Enabled(const AState: TNyxBooleanStateRef): TNyxNodeBindings;
begin
  Result := Put(bpEnabled, AState.Name, nskBoolean, bdFromState);
end;

function TNyxNodeBindings.Visible(const AState: TNyxBooleanStateRef): TNyxNodeBindings;
begin
  Result := Put(bpVisible, AState.Name, nskBoolean, bdFromState);
end;

function TNyxNodeBindings.ReadOnly(const AState: TNyxBooleanStateRef): TNyxNodeBindings;
begin
  Result := Put(bpReadOnly, AState.Name, nskBoolean, bdFromState);
end;

function TNyxNodeBindings.Pressed(const AState: TNyxBooleanStateRef): TNyxNodeBindings;
begin
  Result := Put(bpPressed, AState.Name, nskBoolean, bdFromState);
end;

function TNyxNodeBindings.Placeholder(const AState: TNyxTextStateRef): TNyxNodeBindings;
begin
  Result := Put(bpPlaceholder, AState.Name, nskText, bdFromState);
end;

function TNyxNodeBindings.Hint(const AState: TNyxTextStateRef): TNyxNodeBindings;
begin
  Result := Put(bpHint, AState.Name, nskText, bdFromState);
end;

function TNyxNodeBindings.AccessibleName(const AState: TNyxTextStateRef): TNyxNodeBindings;
begin
  Result := Put(bpAccessibleName, AState.Name, nskText, bdFromState);
end;

function TNyxNodeBindings.Width(const AState: TNyxIntegerStateRef): TNyxNodeBindings;
begin
  Result := Put(bpWidth, AState.Name, nskInteger, bdFromState);
end;

function TNyxNodeBindings.Height(const AState: TNyxIntegerStateRef): TNyxNodeBindings;
begin
  Result := Put(bpHeight, AState.Name, nskInteger, bdFromState);
end;

function TNyxNodeBindings.Left(const AState: TNyxIntegerStateRef): TNyxNodeBindings;
begin
  Result := Put(bpLeft, AState.Name, nskInteger, bdFromState);
end;

function TNyxNodeBindings.Top(const AState: TNyxIntegerStateRef): TNyxNodeBindings;
begin
  Result := Put(bpTop, AState.Name, nskInteger, bdFromState);
end;

function TNyxNodeBindings.Padding(const AState: TNyxIntegerStateRef): TNyxNodeBindings;
begin
  Result := Put(bpPadding, AState.Name, nskInteger, bdFromState);
end;

function TNyxNodeBindings.Gap(const AState: TNyxIntegerStateRef): TNyxNodeBindings;
begin
  Result := Put(bpGap, AState.Name, nskInteger, bdFromState);
end;

function TNyxNodeBindings.Columns(const AState: TNyxIntegerStateRef): TNyxNodeBindings;
begin
  Result := Put(bpColumns, AState.Name, nskInteger, bdFromState);
end;

function TNyxNodeBindings.Flex(const AState: TNyxIntegerStateRef): TNyxNodeBindings;
begin
  Result := Put(bpFlex, AState.Name, nskInteger, bdFromState);
end;

function TNyxNodeBindings.Minimum(const AState: TNyxIntegerStateRef): TNyxNodeBindings;
begin
  Result := Put(bpMinimum, AState.Name, nskInteger, bdFromState);
end;

function TNyxNodeBindings.Maximum(const AState: TNyxIntegerStateRef): TNyxNodeBindings;
begin
  Result := Put(bpMaximum, AState.Name, nskInteger, bdFromState);
end;

function TNyxNodeBindings.Clear(ATarget: TNyxBindingProperty): TNyxNodeBindings;
begin
  FNode.SetBinding(TNyxBindingSpec.Clear(ATarget));
  Result := Self;
end;

function TNyxNodeBindings.Inherit(ATarget: TNyxBindingProperty): TNyxNodeBindings;
begin
  FNode.RemoveBinding(ATarget);
  Result := Self;
end;

function TNyxNodeBindings.Collection(const ASpec: TNyxCollectionViewSpec): TNyxNodeBindings;
begin

  if not ASpec.Defined then
  begin
    raise ENyxModel.Create('Collection requires a defined view; use ClearCollection to unbind');
  end;
  FNode.SetCollectionView(ASpec);
  Result := Self;
end;

function TNyxNodeBindings.ClearCollection: TNyxNodeBindings;
begin
  FNode.SetCollectionView(Default(TNyxCollectionViewSpec));
  Result := Self;
end;

function TNyxNodeBindings.InheritCollection: TNyxNodeBindings;
begin
  FNode.RemoveCollectionView;
  Result := Self;
end;

function TNyxNodeBindings.Done: TNyxNode;
begin
  Result := FNode;
end;

var
  GNextID: Integer = 0;

procedure CheckDesignID(const AID: TNyxText);
var
  LIndex: Integer;
  LScalar: Integer;
  LCount: Integer;
  LContent: Boolean;
begin
  LIndex := 1;
  LCount := 0;
  LContent := False;
  while LIndex <= Length(AID) do
  begin

    if not NyxNextScalar(AID, LIndex, LScalar) then
    begin
      raise ENyxModel.Create('Node ID contains malformed Unicode');
    end;

    if (LScalar < 32) or ((LScalar >= $7f) and (LScalar <= $9f)) or
      (LScalar = $2028) or (LScalar = $2029) then
    begin
      raise ENyxModel.Create('Node ID contains a control or line separator');
    end;
    Inc(LCount);
    LContent := LContent or not NyxScalarWhitespace(LScalar);

    if LCount > NyxMaximumIDScalars then
    begin
      raise ENyxModel.Create('Node ID must contain 1..128 Unicode scalar values');
    end;
  end;

  if not LContent then
  begin
    raise ENyxModel.Create('Node ID requires non-whitespace content');
  end;
end;

function NyxQualifiedID(const APrefix, ASourceID: TNyxText): TNyxText;
var
  LIndex: Integer;
  LStart: Integer;
begin
  CheckDesignID(ASourceID);
  Result := '';
  LStart := 1;
  for LIndex := 1 to Length(ASourceID) do
  begin

    if (ASourceID[LIndex] = '~') or (ASourceID[LIndex] = '/') then
    begin
      { Do not concatenate individual native non-ASCII Char bytes: they are not
        standalone ANSI characters. Copy complete UTF-8 / UTF-16 runs instead. }
      Result := Result + Copy(ASourceID, LStart, LIndex - LStart);

      if ASourceID[LIndex] = '~' then
      begin
        Result := Result + '~0';
      end
      else
      begin
        Result := Result + '~1';
      end;
      LStart := LIndex + 1;
    end;
  end;
  Result := Result + Copy(ASourceID, LStart, MaxInt);

  if APrefix <> '' then
  begin
    Result := APrefix + '/' + Result;
  end;
end;

constructor ENyxModel.Create(const AMessage: TNyxText);
begin
  inherited Create('');
  {$IFDEF PAS2JS}
  Message := AMessage;
  {$ELSE}
  { RawByteString assignment preserves the UTF-8 codepage tag and bytes. It
    changes this exception only, never the process-wide default codepage. }
  Message := RawByteString(AMessage);
  {$ENDIF}
end;

constructor TNyxNode.Create(const AKind: TNyxText; const AID: TNyxText);
begin
  inherited Create;
  FRawOwnership := True;

  if Trim(AKind) = '' then
    raise ENyxModel.Create('Node kind is required');
  FKind := AKind;
  FProps := TNyxStrings.Create;
  FExtensions := TNyxExtensions.Create(nesNode);

  if AID = '' then
  begin
    Inc(GNextID);
    FID := 'node-' + IntToStr(GNextID);
  end
  else
  begin
    Named(AID);
  end;
end;

class function TNyxNode.CreateRealized(const AKind, ASourceID, ARuntimeID,
  ADesignID: TNyxText): TNyxNode;
var
  LIndex: Integer;
  LScalar: Integer;
  LCount: Integer;
begin
  CheckDesignID(ASourceID);
  CheckDesignID(ADesignID);
  LIndex := 1;
  LCount := 0;

  if ARuntimeID = '' then
  begin
    raise ENyxModel.Create('Realized identity is required');
  end;
  { Qualification is bounded by expansion depth and escaped segment size. It
    intentionally does not reuse the 128-scalar authored-ID admission boundary. }
  while LIndex <= Length(ARuntimeID) do
  begin

    if not NyxNextScalar(ARuntimeID, LIndex, LScalar) then
    begin
      raise ENyxModel.Create('Realized identity contains malformed Unicode');
    end;
    Inc(LCount);

    if (LScalar < 32) or ((LScalar >= $7f) and (LScalar <= $9f)) or
      (LCount > (NyxMaximumTreeDepth + 1) * (NyxMaximumIDScalars * 2 + 1)) then
    begin
      raise ENyxModel.Create('Realized identity exceeds its encoding/expansion budget');
    end;
  end;
  Result := TNyxNode.Create(AKind, ASourceID);
  Result.FRuntimeID := ARuntimeID;
  Result.FDesignID := ADesignID;
end;

function TNyxNode.GetID: TNyxText;
begin
  Result := FID;

  if FRuntimeID <> '' then
  begin
    Result := FRuntimeID;
  end;
end;

function TNyxNode.GetDesignID: TNyxText;
begin
  Result := FID;

  if IsRealized then
  begin
    Result := FDesignID;
  end;
end;

function TNyxNode.GetIsRealized: Boolean;
begin
  Result := FRuntimeID <> '';
end;

destructor TNyxNode.Destroy;
var
  LIndex: Integer;
begin
  for LIndex := 0 to Count - 1 do
  begin
    FChildren[LIndex].FParent := nil;
    FChildren[LIndex].ReleaseOwnership;
    FChildReferences[LIndex] := nil;
  end;
  FConfigure.Free;
  FPlatformConfigure[npfBrowser].Free;
  FPlatformConfigure[npfNativeLCL].Free;
  FContract.Free;
  FBindingConfig.Free;
  FProps.Free;
  FExtensions.Free;
  inherited Destroy;
end;

procedure TNyxNode.BeforeDestruction;
begin

  if FReferences <> 0 then
  begin
    raise ENyxModel.Create('A retained component descriptor cannot be freed directly');
  end;
  inherited BeforeDestruction;
end;

procedure TNyxNode.AcquireReference;
begin
  Inc(FReferences);
end;

procedure TNyxNode.ReleaseReference;
begin

  if FReferences <= 0 then
  begin
    raise ENyxModel.Create('Component descriptor reference underflow');
  end;
  Dec(FReferences);

  if (FReferences = 0) and not FRawOwnership and
    (FParent = nil) and (FOwner = nil) then
  begin
    Free;
  end;
end;

procedure TNyxNode.ReleaseOwnership;
begin
  FRawOwnership := False;

  if (FReferences = 0) and (FParent = nil) and (FOwner = nil) then
  begin
    Free;
  end;
end;

function TNyxNode.ComponentReference: INyxNode;
var
  LIndex: Integer;
begin
  Result := nil;

  if FParent <> nil then
  begin
    for LIndex := 0 to FParent.Count - 1 do
    begin

      if FParent.FChildren[LIndex] = Self then
      begin
        Exit(FParent.FChildReferences[LIndex]);
      end;
    end;
  end;

  if FOwner <> nil then
  begin
    for LIndex := 0 to FOwner.Count - 1 do
    begin

      if FOwner.FPages[LIndex] = Self then
      begin
        Exit(FOwner.FPageReferences[LIndex]);
      end;
    end;
    for LIndex := 0 to FOwner.ComponentCount - 1 do
    begin

      if FOwner.FComponents[LIndex] = Self then
      begin
        Exit(FOwner.FComponentReferences[LIndex]);
      end;
    end;
  end;
end;

constructor TNyxNode.Create(AKind: TNyxKind; const AID: TNyxText);
begin
  Create(NyxKindName(AKind), AID);
end;

function TNyxNode.GetConfigure: TNyxNodeConfig;
begin

  if FConfigure = nil then
  begin
    FConfigure := TNyxNodeConfig.Create(Self);
  end;
  Result := FConfigure;
end;

function TNyxNode.GetContract: TNyxContract;
begin

  if FContract = nil then
  begin
    FContract := TNyxContract.Create(FExtensions);
  end;
  Result := FContract;
end;

constructor TNyxNode.Create(const AKind: TNyxKindRef; const AID: TNyxText);
begin
  Create(AKind.Name, AID);
end;

constructor TNyxNodeConfig.Create(ANode: TNyxNode);
begin
  inherited Create;
  FNode := ANode;
  FPlatform := npfAny;
end;

function TNyxNodeConfig.ForPlatform(APlatform: TNyxPlatform): TNyxNodeConfig;
begin

  if APlatform = npfAny then
  begin
    Exit(FNode.Configure);
  end;

  if FNode.FPlatformConfigure[APlatform] = nil then
  begin
    FNode.FPlatformConfigure[APlatform] := TNyxNodeConfig.Create(FNode);
    FNode.FPlatformConfigure[APlatform].FPlatform := APlatform;
  end;
  Result := FNode.FPlatformConfigure[APlatform];
end;

function TNyxNodeConfig.SplitOrientation(AValue: TNyxSplitOrientation): TNyxNodeConfig;
begin
  Result := Put(atSplitOrientation, NyxSplitOrientationName(AValue));
end;

function TNyxNodeConfig.SplitPosition(APercent: Integer): TNyxNodeConfig;
begin
  Result := PutInteger(atSplitPosition, APercent);
end;

function TNyxNodeConfig.SplitMinimum(APercent: Integer): TNyxNodeConfig;
begin
  Result := PutInteger(atSplitMinimum, APercent);
end;

function TNyxNodeConfig.SplitMaximum(APercent: Integer): TNyxNodeConfig;
begin
  Result := PutInteger(atSplitMaximum, APercent);
end;

function TNyxNodeConfig.SplitResizable(AValue: Boolean): TNyxNodeConfig;
begin
  Result := PutBoolean(atSplitResizable, AValue);
end;

function TNyxNodeConfig.DragSource(AValue: Boolean): TNyxNodeConfig;
begin
  Result := PutBoolean(atDragSource, AValue);
end;

function TNyxNodeConfig.DropTarget(AValue: Boolean): TNyxNodeConfig;
begin
  Result := PutBoolean(atDropTarget, AValue);
end;

function TNyxNodeConfig.TouchBehavior(AValue: TNyxTouchBehavior): TNyxNodeConfig;
begin
  Result := Put(atTouchBehavior, NyxTouchBehaviorName(AValue));
end;

function TNyxNodeConfig.Put(AKey: TNyxAttribute;
  const AValue: TNyxText): TNyxNodeConfig;
begin
  if (FPlatform <> npfAny) and not NyxPlatformAttribute(AKey) then
  begin
    raise ENyxModel.Create('This attribute must retain portable meaning: ' + NyxAttributeName(AKey));
  end;
  FNode.SetProp(NyxPlatformKey(FPlatform, AKey), AValue);
  Result := Self;
end;

function TNyxNodeConfig.PutBoolean(AKey: TNyxAttribute;
  AValue: Boolean): TNyxNodeConfig;
begin

  if AValue then
  begin
    Result := Put(AKey, 'true');
  end
  else
  begin
    Result := Put(AKey, 'false');
  end;
end;

function TNyxNodeConfig.PutInteger(AKey: TNyxAttribute;
  AValue: Integer): TNyxNodeConfig;
var
  LMinimum: Integer;
  LMaximum: Integer;
begin
  LMinimum := 0;
  LMaximum := 100000;

  if AKey in [atSplitPosition, atSplitMinimum, atSplitMaximum] then
  begin
    LMaximum := 100;
  end;

  if AKey in [atValue, atMinimum, atMaximum] then
  begin
    LMinimum := -1000000;
    LMaximum := 1000000;
  end;

  if AKey = atColumns then
  begin
    LMinimum := 1;
    LMaximum := 64;
  end;

  if (AValue < LMinimum) or (AValue > LMaximum) then
  begin
    raise ENyxModel.Create('Invalid ' + NyxAttributeName(AKey) + ': outside typed bounds');
  end;
  Result := Put(AKey, IntToStr(AValue));
end;

function TNyxNodeConfig.Text(const AValue: TNyxText): TNyxNodeConfig;
begin
  Result := Put(atText, AValue);
end;

function TNyxNodeConfig.Placeholder(const AValue: TNyxText): TNyxNodeConfig;
begin
  Result := Put(atPlaceholder, AValue);
end;

function TNyxNodeConfig.Items(const AValue: TNyxText): TNyxNodeConfig;
begin
  Result := Put(atItems, AValue);
end;

function TNyxNodeConfig.Hint(const AValue: TNyxText): TNyxNodeConfig;
begin
  Result := Put(atHint, AValue);
end;

function TNyxNodeConfig.AccessibleName(const AValue: TNyxText): TNyxNodeConfig;
begin
  Result := Put(atAccessibleName, AValue);
end;

function TNyxNodeConfig.LinkTo(const AValue: TNyxText): TNyxNodeConfig;
begin
  Result := Put(atHref, AValue);
end;

function TNyxNodeConfig.Source(const AValue: TNyxText): TNyxNodeConfig;
begin
  Result := Put(atSource, AValue);
end;

function TNyxNodeConfig.AlternativeText(const AValue: TNyxText): TNyxNodeConfig;
begin
  Result := Put(atAlt, AValue);
end;

function TNyxNodeConfig.Option(const AValue: TNyxText): TNyxNodeConfig;
begin
  Result := Put(atOption, AValue);
end;

function TNyxNodeConfig.Option(AValue: Integer): TNyxNodeConfig;
begin
  Result := Put(atOption, IntToStr(AValue));
end;

function TNyxNodeConfig.Option(AValue: Boolean): TNyxNodeConfig;
begin
  Result := PutBoolean(atOption, AValue);
end;

function TNyxNodeConfig.Option(AValue: Double): TNyxNodeConfig;
begin
  Result := Put(atOption, TNyxStateValue.FromNumber(AValue).NumberText);
end;

function TNyxNodeConfig.PutValue(const AValue: TNyxStateValue): TNyxNodeConfig;
var
  LDomain: TNyxValueDomain;
  LData: TNyxDataValue;
  LText: TNyxText;
begin
  if FPlatform <> npfAny then
  begin
    raise ENyxModel.Create('Scalar values must retain portable meaning');
  end;
  LDomain := NyxNodeValueDomain(FNode);

  if LDomain.Defined and (AValue.Kind <> LDomain.Kind) and
    not ((AValue.Kind = nskInteger) and (LDomain.Kind = nskNumber)) then
  begin
    raise ENyxContract.Create('Value argument differs from its declared scalar domain on ' + FNode.ID);
  end;
  case AValue.Kind of
    nskText:
      begin
        LText := AValue.TextValue;
        LData := NyxData(LText);
      end;
    nskBoolean:
      begin
        LText := 'false';

        if AValue.BooleanValue then
        begin
          LText := 'true';
        end;
        LData := NyxData(AValue.BooleanValue);
      end;
    nskInteger:
      begin
        LText := IntToStr(AValue.IntegerValue);
        LData := NyxData(AValue.IntegerValue);
      end;
    nskNumber:
      begin
        LText := AValue.NumberText;
        LData := NyxData(AValue.NumberValue);
      end;
  end;

  if LDomain.Defined then
  begin
    LDomain.Admit(LData);
  end;

  if (AValue.Kind = nskInteger) and
    ((FNode.ProjectionKind = 'spin') or (FNode.ProjectionKind = 'slider') or
    (FNode.ProjectionKind = 'progress')) then
  begin
    Exit(PutInteger(atValue, AValue.IntegerValue));
  end;
  Result := Put(atValue, LText);
end;

function TNyxNodeConfig.Value(const AValue: TNyxText): TNyxNodeConfig;
begin
  Result := PutValue(TNyxStateValue.FromText(AValue));
end;

function TNyxNodeConfig.Value(AValue: Integer): TNyxNodeConfig;
begin
  Result := PutValue(TNyxStateValue.FromInteger(AValue));
end;

function TNyxNodeConfig.Value(AValue: Boolean): TNyxNodeConfig;
begin
  Result := PutValue(TNyxStateValue.FromBoolean(AValue));
end;

function TNyxNodeConfig.Value(AValue: Double): TNyxNodeConfig;
begin
  Result := PutValue(TNyxStateValue.FromNumber(AValue));
end;

function TNyxNodeConfig.Padding(AValue: Integer): TNyxNodeConfig;
begin
  Result := PutInteger(atPadding, AValue);
end;

function TNyxNodeConfig.Gap(AValue: Integer): TNyxNodeConfig;
begin
  Result := PutInteger(atGap, AValue);
end;

function TNyxNodeConfig.Columns(AValue: Integer): TNyxNodeConfig;
begin
  Result := PutInteger(atColumns, AValue);
end;

function TNyxNodeConfig.Width(AValue: Integer): TNyxNodeConfig;
begin
  Result := PutInteger(atWidth, AValue);
end;

function TNyxNodeConfig.Height(AValue: Integer): TNyxNodeConfig;
begin
  Result := PutInteger(atHeight, AValue);
end;

function TNyxNodeConfig.Left(AValue: Integer): TNyxNodeConfig;
begin
  Result := PutInteger(atLeft, AValue);
end;

function TNyxNodeConfig.Top(AValue: Integer): TNyxNodeConfig;
begin
  Result := PutInteger(atTop, AValue);
end;

function TNyxNodeConfig.Flex(AValue: Integer): TNyxNodeConfig;
begin
  Result := PutInteger(atFlex, AValue);
end;

function TNyxNodeConfig.Minimum(AValue: Integer): TNyxNodeConfig;
begin
  Result := PutInteger(atMinimum, AValue);
end;

function TNyxNodeConfig.Maximum(AValue: Integer): TNyxNodeConfig;
begin
  Result := PutInteger(atMaximum, AValue);
end;

function TNyxNodeConfig.Enabled(AValue: Boolean): TNyxNodeConfig;
begin
  Result := PutBoolean(atEnabled, AValue);
end;

function TNyxNodeConfig.Visible(AValue: Boolean): TNyxNodeConfig;
begin
  Result := PutBoolean(atVisible, AValue);
end;

function TNyxNodeConfig.ReadOnly(AValue: Boolean): TNyxNodeConfig;
begin
  Result := PutBoolean(atReadOnly, AValue);
end;

function TNyxNodeConfig.Surface(AValue: Boolean): TNyxNodeConfig;
begin
  Result := PutBoolean(atSurface, AValue);
end;

function TNyxNodeConfig.Compound(AValue: Boolean): TNyxNodeConfig;
begin
  Result := PutBoolean(atCompound, AValue);
end;

function TNyxNodeConfig.Pressed(AValue: Boolean): TNyxNodeConfig;
begin
  Result := PutBoolean(atPressed, AValue);
end;

function TNyxNodeConfig.Layout(AValue: TNyxLayoutMode): TNyxNodeConfig;
begin
  Result := Put(atLayout, NyxLayoutName(AValue));
end;

function TNyxNodeConfig.Variant(AValue: TNyxVariant): TNyxNodeConfig;
begin
  Result := Put(atVariant, NyxVariantName(AValue));
end;

function TNyxNodeConfig.Action(AValue: TNyxAction): TNyxNodeConfig;
begin
  Result := Put(atAction, NyxActionName(AValue));
end;

function TNyxNodeConfig.ProjectAs(AValue: TNyxKind): TNyxNodeConfig;
begin
  Result := Put(atProjection, NyxKindName(AValue));
end;

function TNyxNodeConfig.OverrideMode(AValue: TNyxOverrideMode): TNyxNodeConfig;
begin
  Result := Put(atOverrideMode, NyxOverrideName(AValue));
end;

function TNyxNodeConfig.InputType(AValue: TNyxInputType): TNyxNodeConfig;
begin
  Result := Put(atInputType, NyxInputTypeName(AValue));
end;

function TNyxNodeConfig.PartName(const AValue: TNyxPartRef): TNyxNodeConfig;
begin
  Result := Put(atPart, AValue.Name);
end;

function TNyxNodeConfig.Target(const AValue: TNyxPartRef): TNyxNodeConfig;
begin
  Result := Put(atTarget, AValue.Name);
end;

function TNyxNodeConfig.OverridePath(const AValue: TNyxPartRef): TNyxNodeConfig;
begin
  Result := Put(atPath, AValue.Name);
end;

function TNyxNodeConfig.Component(const AValue: TNyxComponentRef): TNyxNodeConfig;
begin
  Result := Put(atComponent, AValue.Name);
end;

function TNyxNodeConfig.OnClick(const AValue: TNyxEventRef): TNyxNodeConfig;
begin
  Result := Put(atEmit, AValue.Name);
end;

function TNyxNodeConfig.OnChange(const AValue: TNyxEventRef): TNyxNodeConfig;
begin
  Result := Put(atEmitChange, AValue.Name);
end;

function TNyxNodeConfig.Clear(AKey: TNyxAttribute): TNyxNodeConfig;
begin
  Result := Put(AKey, '');
end;

function TNyxNodeConfig.Metadata(AKey: TNyxAttribute; const AValue: TNyxText): TNyxNodeConfig;
begin
  Result := Put(AKey, AValue);
end;

function TNyxNodeConfig.CustomVariant(const AStyle: TNyxStyleRef): TNyxNodeConfig;
begin
  Result := Put(atVariant, AStyle.Name);
end;

function TNyxNodeConfig.CustomProjection(const AKind: TNyxKindRef): TNyxNodeConfig;
begin
  Result := Put(atProjection, AKind.Name);
end;

function TNyxNodeConfig.Extension(const AKey, AValue: TNyxText): TNyxNodeConfig;
var
  LAttribute: TNyxAttribute;
begin

  if TryNyxAttribute(AKey, LAttribute) or (Copy(AKey, 1, 5) = '@nyx.') or
    (FPlatform <> npfAny) then
  begin
    raise ENyxModel.Create('Built-in property requires typed configuration: ' + AKey);
  end;
  FNode.SetProp(AKey, AValue);
  Result := Self;
end;

function TNyxNodeConfig.Done: TNyxNode;
begin
  Result := FNode;
end;

function TNyxNode.GetProjectionKind: TNyxText;
begin
  Result := Prop('projection-kind', FKind);

  if Result = '' then
  begin
    Result := FKind;
  end;
end;

function TNyxNode.GetCount: Integer;
begin
  Result := Length(FChildren);
end;

function TNyxNode.GetChild(AIndex: Integer): TNyxNode;
begin

  if (AIndex < 0) or (AIndex >= Count) then
    raise ENyxModel.Create('Child index out of range');
  Result := FChildren[AIndex];
end;

function TNyxNode.Named(const AID: TNyxText): TNyxNode;
begin

  if IsRealized then
  begin
    raise ENyxModel.Create('Realized identity is immutable; edit the source design');
  end;
  CheckDesignID(AID);
  FID := AID;
  Result := Self;
end;

function TNyxNode.SetProp(const AKey, AValue: TNyxText): TNyxNode;
var
  LIndex: Integer;
begin

  if (AKey = '') or (Pos('=', AKey) > 0) or (Pos(#10, AKey) > 0) or
    (Pos(#13, AKey) > 0) then
    raise ENyxModel.Create('Invalid property name');
  LIndex := FProps.IndexOfName(AKey);

  if LIndex < 0 then
  begin
    FProps.Add(AKey + '=' + AValue);
  end
  else
  begin
    FProps[LIndex] := AKey + '=' + AValue;
  end;
  Result := Self;
end;

function TNyxNode.Prop(const AKey, ADefault: TNyxText): TNyxText;
var
  LIndex: Integer;
begin
  LIndex := FProps.IndexOfName(AKey);

  if LIndex < 0 then
    Exit(ADefault);
  Result := Copy(FProps[LIndex], Length(AKey) + 2, MaxInt);
end;

procedure TNyxNode.Admit(ANode: TNyxNode);
var
  LAncestor: TNyxNode;
begin
  { Check every ownership precondition before resizing the child array. Walking
    ancestors catches the less obvious case of adding a root beneath its own
    descendant, even though that root has no parent of its own. }

  if ANode = nil then
    raise ENyxModel.Create('Cannot add a nil node');

  if IsRealized <> ANode.IsRealized then
  begin
    raise ENyxModel.Create('Design and realized nodes require separate owned trees');
  end;

  if (ANode.FParent <> nil) or (ANode.FOwner <> nil) then
    raise ENyxModel.Create('Node already has an owner; extract or clone it first');
  LAncestor := Self;
  while LAncestor <> nil do
  begin

    if LAncestor = ANode then
      raise ENyxModel.Create('Cannot create a node cycle');
    LAncestor := LAncestor.Parent;
  end;
end;

function TNyxNode.Add(ANode: TNyxNode): TNyxNode;
begin
  Insert(Count, ANode);
  Result := Self;
end;

function TNyxNode.Add(const ANode: INyxNode): TNyxNode;
begin

  if ANode = nil then
  begin
    raise ENyxModel.Create('Child interface is required');
  end;
  Insert(Count, ANode);
  Result := Self;
end;

procedure TNyxNode.Insert(AIndex: Integer; const ANode: INyxNode);
begin

  if ANode = nil then
  begin
    raise ENyxModel.Create('Child interface is required');
  end;
  Insert(AIndex, ANode.Node);
  FChildReferences[AIndex] := ANode;
end;

procedure TNyxNode.Insert(AIndex: Integer; ANode: TNyxNode);
var
  LIndex: Integer;
begin

  if (AIndex < 0) or (AIndex > Count) then
    raise ENyxModel.Create('Insert index out of range');
  Admit(ANode);
  SetLength(FChildren, Count + 1);
  SetLength(FChildReferences, Count);
  for LIndex := Count - 1 downto AIndex + 1 do
  begin
    FChildren[LIndex] := FChildren[LIndex - 1];
    FChildReferences[LIndex] := FChildReferences[LIndex - 1];
  end;
  FChildren[AIndex] := ANode;
  FChildReferences[AIndex] := nil;
  ANode.FParent := Self;
  ANode.FRawOwnership := False;
end;

function TNyxNode.Extract(AIndex: Integer): TNyxNode;
var
  LIndex: Integer;
begin
  Result := GetChild(AIndex);
  { Detach and acquire the raw token before releasing an implementation anchor.
    Its destructor may release the last managed reference to this descriptor. }
  Result.FParent := nil;
  Result.FRawOwnership := True;
  for LIndex := AIndex to Count - 2 do
  begin
    FChildren[LIndex] := FChildren[LIndex + 1];
    FChildReferences[LIndex] := FChildReferences[LIndex + 1];
  end;
  SetLength(FChildReferences, Count - 1);
  SetLength(FChildren, Count - 1);
  Result.FParent := nil;
  { Raw extraction explicitly hands a caller ownership token back. A managed
    extraction retains an interface before releasing this token. }
  Result.FRawOwnership := True;
end;

procedure TNyxNode.Remove(ANode: TNyxNode);
var
  LIndex: Integer;
  LRemoved: TNyxNode;
begin
  for LIndex := 0 to Count - 1 do
  begin

    if FChildren[LIndex] = ANode then
    begin
      LRemoved := Extract(LIndex);
      LRemoved.ReleaseOwnership;
      Exit;
    end;
  end;
  raise ENyxModel.Create('Node is not a child');
end;

function TNyxNode.Find(const AID: TNyxText): TNyxNode;
var
  LIndex: Integer;
begin

  if ID = AID then
    Exit(Self);
  for LIndex := 0 to Count - 1 do
  begin
    Result := FChildren[LIndex].Find(AID);

    if Result <> nil then
      Exit;
  end;
  Result := nil;
end;

function TNyxNode.Clone: TNyxNode;
var
  LIndex: Integer;
begin
  { TNyxStrings.Assign copies stored strings; recursively constructing children
    avoids sharing dynamic arrays or borrowed parent pointers with the baseline. }

  if IsRealized then
  begin
    Result := TNyxNode.CreateRealized(Kind, SourceID, ID, DesignID);
  end
  else
  begin
    Result := TNyxNode.Create(Kind, ID);
  end;
  try
    Result.Props.Assign(FProps);
    Result.Extensions.Assign(FExtensions);
    Result.FInstanceScopeID := FInstanceScopeID;

    if FHasCollectionView then
    begin
      Result.SetCollectionView(FCollectionView);
    end;
    for LIndex := 0 to BindingCount - 1 do
    begin
      Result.SetBinding(FStateBindings[LIndex]);
    end;
    for LIndex := 0 to Count - 1 do
    begin
      Result.Add(FChildren[LIndex].Clone);
    end;
  except
    Result.Free;
    raise;
  end;
end;

function TNyxNode.Part(const APath: TNyxPartRef): TNyxNode;
begin
  Result := Part(APath.Name);
end;

function TNyxNode.Part(const APath: TNyxText): TNyxNode;
var
  LSlash: Integer;
  LName: TNyxText;
  LRemainder: TNyxText;
  LIndex: Integer;
begin

  if APath = '.' then
  begin
    Exit(Self);
  end;
  LSlash := Pos('/', APath);
  LName := APath;
  LRemainder := '';

  if LSlash > 0 then
  begin
    LName := Copy(APath, 1, LSlash - 1);
    LRemainder := Copy(APath, LSlash + 1, MaxInt);
  end;
  for LIndex := 0 to Count - 1 do
  begin

    if Children[LIndex].Prop('part') = LName then
    begin
      Result := Children[LIndex];

      if LRemainder <> '' then
      begin
        Result := Result.Part(LRemainder);
      end;
      Exit;
    end;
  end;
  raise ENyxModel.Create('Compound part not found: ' + APath);
end;

function TNyxNode.OverridePart(const APath: TNyxPartRef): TNyxNode;
begin
  Result := OverridePart(APath.Name);
end;

function TNyxNode.OverridePart(const APath: TNyxPartRef;
  AMode: TNyxOverrideMode): TNyxNode;
begin
  Result := OverridePart(APath.Name, NyxOverrideName(AMode));
end;

function TNyxNode.OverridePart(const APath, AMode: TNyxText): TNyxNode;
var
  LIndex: Integer;
  LMode: TNyxText;
begin

  if (Kind <> 'component') and (ProjectionKind <> 'component') then
  begin
    raise ENyxModel.Create('Part overrides belong to reusable references: ' + ID);
  end;

  if (APath = '') or (APath[1] = '/') or (APath[Length(APath)] = '/') or
    (Pos('//', APath) > 0) then
  begin
    raise ENyxModel.Create('Part override requires a named path');
  end;

  if (AMode <> '') and (AMode <> 'properties') and (AMode <> 'append') and (AMode <> 'prepend') and
    (AMode <> 'replace') and (AMode <> 'remove') then
  begin
    raise ENyxModel.Create('Unknown part override mode: ' + AMode);
  end;
  for LIndex := 0 to Count - 1 do
  begin

    if (Children[LIndex].Kind = 'slot-override') and
      (Children[LIndex].Prop('path') = APath) then
    begin
      Result := Children[LIndex];

      if AMode <> '' then
      begin
        Result.SetProp('mode', AMode);
      end;
      Exit;
    end;
  end;
  { Descriptors are ordinary owned design nodes, including stable serialized
    identity. Their payload is never shared with the definition or another view. }
  Result := TNyxNode.Create('slot-override');
  try
    LMode := AMode;

    if LMode = '' then
    begin
      LMode := 'properties';
    end;
    Result.SetProp('path', APath).SetProp('mode', LMode);
    Add(Result);
  except
    Result.Free;
    raise;
  end;
end;

constructor TNyxDocument.Create;
begin
  inherited Create;
  FState := TNyxState.Create;
  FCollections := NewNyxCollectionDefaults;
  FExtensions := TNyxExtensions.Create(nesDocument);
  FTitle := 'Untitled Nyx application';
end;

destructor TNyxDocument.Destroy;
var
  LIndex: Integer;
begin
  for LIndex := 0 to Count - 1 do
  begin
    FPages[LIndex].FOwner := nil;
    FPages[LIndex].ReleaseOwnership;
    FPageReferences[LIndex] := nil;
  end;
  for LIndex := 0 to ComponentCount - 1 do
  begin
    FComponents[LIndex].FOwner := nil;
    FComponents[LIndex].ReleaseOwnership;
    FComponentReferences[LIndex] := nil;
  end;
  FState.Free;
  FCollections := nil;
  FExtensions.Free;
  inherited Destroy;
end;

function TNyxDocument.GetCount: Integer;
begin
  Result := Length(FPages);
end;

function TNyxDocument.GetPage(AIndex: Integer): TNyxNode;
begin

  if (AIndex < 0) or (AIndex >= Count) then
    raise ENyxModel.Create('Page index out of range');
  Result := FPages[AIndex];
end;

function TNyxDocument.GetComponentCount: Integer;
begin
  Result := Length(FComponents);
end;

function TNyxDocument.GetComponent(AIndex: Integer): TNyxNode;
begin

  if (AIndex < 0) or (AIndex >= ComponentCount) then
    raise ENyxModel.Create('Component index out of range');
  Result := FComponents[AIndex];
end;

procedure TNyxDocument.AdmitRoot(ANode: TNyxNode);
begin

  if ANode = nil then
    raise ENyxModel.Create('Root is required');

  if ANode.IsRealized then
  begin
    raise ENyxModel.Create('Realized views cannot become design roots');
  end;

  if (ANode.FParent <> nil) or (ANode.FOwner <> nil) then
    raise ENyxModel.Create('Root already belongs to a tree or document');
end;

function TNyxDocument.AddPage(ANode: TNyxNode): TNyxDocument;
begin
  AdmitRoot(ANode);
  SetLength(FPages, Count + 1);
  SetLength(FPageReferences, Count);
  FPages[Count - 1] := ANode;
  ANode.FOwner := Self;
  ANode.FRawOwnership := False;
  Result := Self;
end;

function TNyxDocument.AddComponent(ANode: TNyxNode): TNyxDocument;
begin
  AdmitRoot(ANode);
  SetLength(FComponents, ComponentCount + 1);
  SetLength(FComponentReferences, ComponentCount);
  FComponents[ComponentCount - 1] := ANode;
  ANode.FOwner := Self;
  ANode.FRawOwnership := False;
  Result := Self;
end;

function TNyxDocument.AddPage(const ANode: INyxNode): TNyxDocument;
begin

  if ANode = nil then
  begin
    raise ENyxModel.Create('Page interface is required');
  end;
  Result := AddPage(ANode.Node);
  FPageReferences[Count - 1] := ANode;
end;

function TNyxDocument.AddComponent(const ANode: INyxNode): TNyxDocument;
begin

  if ANode = nil then
  begin
    raise ENyxModel.Create('Component interface is required');
  end;
  Result := AddComponent(ANode.Node);
  FComponentReferences[ComponentCount - 1] := ANode;
end;

function TNyxDocument.Find(const AID: TNyxText): TNyxNode;
var
  LIndex: Integer;
begin
  for LIndex := 0 to Count - 1 do
  begin
    Result := FPages[LIndex].Find(AID);

    if Result <> nil then
      Exit;
  end;
  for LIndex := 0 to ComponentCount - 1 do
  begin
    Result := FComponents[LIndex].Find(AID);

    if Result <> nil then
      Exit;
  end;
  Result := nil;
end;

function TNyxDocument.FindComponent(const AID: TNyxText): TNyxNode;
var
  LIndex: Integer;
begin
  for LIndex := 0 to ComponentCount - 1 do
  begin

    if FComponents[LIndex].ID = AID then
      Exit(FComponents[LIndex]);
  end;
  Result := nil;
end;

function TNyxDocument.Clone: TNyxDocument;
var
  LIndex: Integer;
begin
  Result := TNyxDocument.Create;
  try
    Result.Title := Title;
    Result.Extensions.Assign(FExtensions);
    Result.FState.Free;
    Result.FState := nil;
    Result.FState := FState.Clone;
    Result.FCollections := FCollections.Clone;
    for LIndex := 0 to Count - 1 do
    begin
      Result.AddPage(FPages[LIndex].Clone);
    end;
    for LIndex := 0 to ComponentCount - 1 do
    begin
      Result.AddComponent(FComponents[LIndex].Clone);
    end;
  except
    Result.Free;
    raise;
  end;
end;

function TNyxDocument.Contains(ANode: TNyxNode): Boolean;
begin
  Result := False;

  if ANode = nil then
  begin
    Exit;
  end;
  while ANode.Parent <> nil do
  begin
    ANode := ANode.Parent;
  end;
  Result := ANode.FOwner = Self;
end;

function TNyxDocument.GetHasCollectionViews: Boolean;
var
  LIndex: Integer;

  function HasView(ANode: TNyxNode): Boolean;
  var
    LChild: Integer;
  begin
    Result := ANode.HasCollectionView;
    for LChild := 0 to ANode.Count - 1 do
    begin

      if Result then
      begin
        Exit;
      end;
      Result := HasView(ANode.Children[LChild]);
    end;
  end;

begin
  Result := False;
  for LIndex := 0 to Count - 1 do
  begin

    if HasView(FPages[LIndex]) then
    begin
      Exit(True);
    end;
  end;
  for LIndex := 0 to ComponentCount - 1 do
  begin

    if HasView(FComponents[LIndex]) then
    begin
      Exit(True);
    end;
  end;
end;

procedure TNyxDocument.Validate;
var
  LIDs: TNyxStrings;
  LStack: TNyxStrings;
  LChecked: TNyxStrings;
  LTotal: Integer;
  LIndex: Integer;
  LHasCollectionViews: Boolean;

  procedure Visit(ANode: TNyxNode; ADepth: Integer);
  var
    LChildIndex: Integer;
    LBindingIndex: Integer;
    LBinding: TNyxBindingSpec;
    LPath: TNyxText;
    LMode: TNyxText;
    LPaths: TNyxStrings;
    LViewSpec: TNyxCollectionViewSpec;
    LProjection: TNyxCollectionProjection;
  begin
    Inc(LTotal);

    if (ADepth > NyxMaximumTreeDepth) or (LTotal > NyxMaximumNodes) then
      raise ENyxModel.Create('Document exceeds depth or node budget');

    if LIDs.IndexOf(ANode.ID) >= 0 then
      raise ENyxModel.Create('Duplicate node ID: ' + ANode.ID);
    LIDs.Add(ANode.ID);
    ANode.Extensions.Validate;
    ANode.Contract.Validate;

    if LHasCollectionViews and ANode.Extensions.Has(NyxExtension(NyxCollectionViewWireField)) then
    begin
      raise ENyxModel.Create('Typed collection views conflict with retained collectionView data on ' + ANode.ID);
    end;

    if ANode.HasCollectionView then
    begin
      LViewSpec := ANode.CollectionView;
      LViewSpec.Validate;

      if LViewSpec.Defined then
      begin
        LProjection := cpList;

        if ANode.ProjectionKind = 'table' then
        begin
          LProjection := cpTable;
        end
        else if (ANode.ProjectionKind = 'tree') or
          ((ANode.ProjectionKind = 'component') or (ANode.Kind = 'slot-override')) then
        begin
          { Reference/part projection is resolved by composition. Validate its
            field/default meaning now; the realized primitive validates shape. }

          if LViewSpec.ParentField <> '' then
          begin
            LProjection := cpTree;
          end;
        end
        else if ANode.ProjectionKind <> 'list' then
        begin
          raise ENyxModel.Create('Collection binding requires a list, table or tree on ' + ANode.ID);
        end;
        ValidateNyxCollectionViewSnapshot(FCollections.Snapshot(LViewSpec.Key), LViewSpec, LProjection);
      end;
    end;
    for LBindingIndex := 0 to ANode.BindingCount - 1 do
    begin
      LBinding := ANode.Bindings[LBindingIndex];
      LBinding.Validate;

      if not LBinding.Cleared then
      begin

        if not FState.Has(LBinding.StateName) then
        begin
          raise ENyxModel.Create('Binding default is missing on ' + ANode.ID +
            ': ' + LBinding.StateName);
        end;

        if FState.Value(LBinding.StateName).Kind <> LBinding.ValueKind then
        begin
          raise ENyxModel.Create('Binding default has a different kind on ' + ANode.ID);
        end;
      end;
    end;

    if ((ANode.Kind = 'component') or (ANode.ProjectionKind = 'component')) and
      (FindComponent(ANode.Prop('component')) = nil) then
      raise ENyxModel.Create('Unknown reusable component: ' + ANode.Prop('component'));

    if ((ANode.Kind = 'component') or (ANode.ProjectionKind = 'component')) and
      (ANode.Count > 0) then
    begin
      LPaths := TNyxStrings.Create;
      try
        for LChildIndex := 0 to ANode.Count - 1 do
        begin

          if ANode.Children[LChildIndex].Kind <> 'slot-override' then
          begin
            raise ENyxModel.Create('Reusable instance children require explicit slot overrides: ' + ANode.ID);
          end;
          LPath := ANode.Children[LChildIndex].Prop('path');

          if LPaths.IndexOf(LPath) >= 0 then
          begin
            raise ENyxModel.Create('Duplicate part override path on ' + ANode.ID + ': ' + LPath);
          end;
          LPaths.Add(LPath);
        end;
      finally
        LPaths.Free;
      end;
    end;

    if ANode.Kind = 'slot-override' then
    begin

      if (ANode.Parent = nil) or (ANode.Parent.ProjectionKind <> 'component') then
      begin
        raise ENyxModel.Create('Part overrides belong to reusable references: ' + ANode.ID);
      end;
      LPath := ANode.Prop('path');
      LMode := ANode.Prop('mode');

      if (LPath = '') or (LPath[1] = '/') or (LPath[Length(LPath)] = '/') or
        (Pos('//', LPath) > 0) then
      begin
        raise ENyxModel.Create('Invalid part override path on ' + ANode.ID);
      end;

      if (LMode <> 'properties') and (LMode <> 'append') and (LMode <> 'prepend') and
        (LMode <> 'replace') and (LMode <> 'remove') then
      begin
        raise ENyxModel.Create('Unknown part override mode on ' + ANode.ID);
      end;

      if (((LMode = 'properties') or (LMode = 'remove')) and (ANode.Count <> 0)) or
        ((LMode = 'replace') and (ANode.Count <> 1)) or
        (((LMode = 'append') or (LMode = 'prepend')) and (ANode.Count = 0)) then
      begin
        raise ENyxModel.Create('Invalid part override payload on ' + ANode.ID);
      end;

      if (ANode.Prop('path') = '.') and ((LMode = 'replace') or (LMode = 'remove')) then
      begin
        raise ENyxModel.Create('Reusable root supports property/content overrides: ' + ANode.ID);
      end;
      for LChildIndex := 0 to ANode.Props.Count - 1 do
      begin
        LPath := ANode.Props.Names[LChildIndex];

        if (LPath = 'component') or (LPath = 'projection-kind') or
          (LPath = 'design-id') or (LPath = 'part') then
        begin
          raise ENyxModel.Create('Part override cannot change identity metadata: ' + LPath);
        end;

        if (LMode = 'remove') and (LPath <> 'path') and (LPath <> 'mode') then
        begin
          raise ENyxModel.Create('Removed part cannot also have property overrides: ' + ANode.ID);
        end;
      end;
    end;
    for LChildIndex := 0 to ANode.Count - 1 do
    begin
      Visit(ANode.Children[LChildIndex], ADepth + 1);
    end;
  end;

  procedure CheckReferences(ANode: TNyxNode; ADepth: Integer);
  var
    LDefinition: TNyxNode;
    LChildIndex: Integer;
  begin
    { A tree can be acyclic while its component references are recursive.
      Track only the active expansion path: using the same definition twice in
      separate branches is valid and must not be mistaken for recursion. }

    if ADepth > 128 then
      raise ENyxModel.Create('Component expansion exceeds depth budget');

    if (ANode.Kind = 'component') or (ANode.ProjectionKind = 'component') then
    begin
      LDefinition := FindComponent(ANode.Prop('component'));

      if LStack.IndexOf(LDefinition.ID) >= 0 then
        raise ENyxModel.Create('Reusable component cycle: ' + LDefinition.ID);

      if LChecked.IndexOf(LDefinition.ID) < 0 then
      begin
        LStack.Add(LDefinition.ID);
        CheckReferences(LDefinition, ADepth + 1);
        LStack.Delete(LStack.Count - 1);
        LChecked.Add(LDefinition.ID);
      end;
    end;
    for LChildIndex := 0 to ANode.Count - 1 do
    begin
      CheckReferences(ANode.Children[LChildIndex], ADepth + 1);
    end;
  end;

begin
  FState.Validate;
  FCollections.Validate;
  LHasCollectionViews := HasCollectionViews;
  { Version-1 applications could already own an opaque collections wire field.
    New typed defaults require version 2; reject this explicit collision rather
    than overwrite retained application data during encoding or history. }

  if ((FCollections.Count > 0) or LHasCollectionViews) and
    FExtensions.Has(NyxExtension(NyxCollectionsWireField)) then
  begin
    raise ENyxModel.Create('Typed collections conflict with the retained version-1 collections extension');
  end;
  FExtensions.Validate;
  LIDs := TNyxStrings.Create;
  LStack := TNyxStrings.Create;
  LChecked := TNyxStrings.Create;
  try
    LTotal := 0;
    for LIndex := 0 to Count - 1 do
    begin
      Visit(FPages[LIndex], 0);
    end;
    for LIndex := 0 to ComponentCount - 1 do
    begin
      Visit(FComponents[LIndex], 0);
    end;
    for LIndex := 0 to Count - 1 do
    begin
      CheckReferences(FPages[LIndex], 0);
    end;
    for LIndex := 0 to ComponentCount - 1 do
    begin
      CheckReferences(FComponents[LIndex], 0);
    end;
  finally
    LChecked.Free;
    LStack.Free;
    LIDs.Free;
  end;
end;

end.
