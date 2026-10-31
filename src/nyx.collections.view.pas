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
  nyx.collections.query.publication,
  nyx.collections.query.session,
  nyx.collections.paging,
  nyx.collections.query.view,
  nyx.collections.registry,
  nyx.collections.view.types,
  nyx.collections.arrangement,
  nyx.collections.presentation,
  nyx.collections.alignment,
  nyx.collections.inspection,
  nyx.collections.position,
  nyx.scheduler,
  nyx.collections.selection;

const
  NyxMaximumCollectionViewInstances = 1024;

type
  INyxCollectionView = interface;
  { One immutable, bounded observation of a table's authored descriptors and
    host-owned preferences. It retains neither the live view nor any widget.
    Serial is diagnostic; private identity proof also fences prepared commands,
    so copying a serial cannot retarget a command to another table or rebind. }
  INyxCollectionColumnsCapture = interface(IInterface)
    ['{8D2BD642-934C-40E0-A9B4-C3760193CB60}']
    function GetSpec: TNyxCollectionViewSpec;
    function GetPreferences: TNyxColumnPreferences;
    function GetSerial: Integer;
    property Spec: TNyxCollectionViewSpec read GetSpec;
    property Preferences: TNyxColumnPreferences read GetPreferences;
    property Serial: Integer read GetSerial;
  end;

  { One detached presentation observation shares the table's canonical column
    serial and identity. It owns exact authored descriptors and copied user
    text/density overrides only; neither view nor store/widget is retained. }
  INyxCollectionPresentationCapture = interface(IInterface)
    ['{CF4EC4CA-7744-44CB-A36D-22B59FEB166D}']
    function GetSpec: TNyxCollectionViewSpec;
    function GetPresentationPreferences: TNyxTablePresentationPreferences;
    function GetSerial: Integer;
    property Spec: TNyxCollectionViewSpec read GetSpec;
    property Preferences: TNyxTablePresentationPreferences read GetPresentationPreferences;
    property Serial: Integer read GetSerial;
  end;

  { Immutable actual table output membership captured in one UI operation.
    Filtered is the complete accepted query result, before paging; Page is the
    actual accepted page. Neither depends on mutable selection. Columns retain
    exact authored typed descriptors/preferences. These snapshots and private
    identity witnesses retain no live view, store, document or widget. }
  INyxCollectionExportCapture = interface(IInterface)
    ['{AD3AE49B-F454-46AC-927B-E10B13F8F22D}']
    function GetSource: INyxCollectionSnapshot;
    function GetFiltered: INyxCollectionSnapshot;
    function GetPage: INyxCollectionSnapshot;
    function GetColumns: INyxCollectionColumnsCapture;
    property Source: INyxCollectionSnapshot read GetSource;
    property Filtered: INyxCollectionSnapshot read GetFiltered;
    property Page: INyxCollectionSnapshot read GetPage;
    property Columns: INyxCollectionColumnsCapture read GetColumns;
  end;

  { Detached original row-copy membership. Source, actual page, selection and
    typed columns belong to one accepted table cohort. Readiness reads inert
    synchronized retirement facts only; no view/store/mount/tree is retained.
    Accepted selection, source, query, page or column changes retire this proof
    before publication callbacks, including a later ABA to identical values. }
  INyxCollectionSelectedRowsCapture = interface(IInterface)
    ['{389DD80F-9F6D-4AD5-92DA-5CD741E06DCC}']
    function GetSource: INyxCollectionSnapshot;
    function GetPage: INyxCollectionSnapshot;
    function GetSelection: TNyxCollectionSelectionSnapshot;
    function GetColumns: INyxCollectionColumnsCapture;
    function GetReadiness: INyxCancellationReadiness;
    property Source: INyxCollectionSnapshot read GetSource;
    property Page: INyxCollectionSnapshot read GetPage;
    property Selection: TNyxCollectionSelectionSnapshot read GetSelection;
    property Columns: INyxCollectionColumnsCapture read GetColumns;
    property Readiness: INyxCancellationReadiness read GetReadiness;
  end;

  { Optional flags/snapshots-only UI-owner capture leaves the original view ABI
    unchanged. It invokes no application/atomic Busy getter, so input reserves
    this cohort before outward factories. Nil means locally busy/unsupported;
    later command admission still checks actual publication readiness. }
  INyxCollectionSelectedRowsView = interface(IInterface)
    ['{79319356-2F27-483A-866A-95C121894889}']
    function CaptureSelectedRows: INyxCollectionSelectedRowsCapture;
  end;

  { Optional output capture keeps all existing view GUIDs unchanged. Capture is
    UI-only and refuses a busy/non-table target. CheckExportCurrent rejects a
    foreign/stale capture, including query/page/layout ABA, before external save
    entry. Callers separately fence original mounted input and event lifetime.
    It does not grant file or clipboard authority. }
  INyxCollectionExportView = interface(IInterface)
    ['{0764B8D6-9A0B-4EA8-92A6-95CB87ED3A25}']
    function CaptureExport: INyxCollectionExportCapture;
    procedure CheckExportCurrent(const ACapture: INyxCollectionExportCapture);
  end;

  { Optional per-table column arrangement; all older view GUIDs stay unchanged.
    Commands reserve this same view and publish through its existing observers.
    No row, selection, query, document or authored default is changed. Resets
    remove overrides even if user commands have since been disabled. Host font
    measurement is separate: bounded fit-to-content publishes through AutoSize,
    which checks its own capability instead of assuming resize is also enabled. }
  INyxCollectionColumns = interface(IInterface)
    ['{FA1D2EC7-EAB4-408B-AC16-EFAE6331C1E2}']
    function CaptureColumns: INyxCollectionColumnsCapture;
    function PrepareColumns(const ACapture: INyxCollectionColumnsCapture;
      const APreferences: TNyxColumnPreferences): INyxPreparedPublication;
    function PrepareAutoSize(const ACapture: INyxCollectionColumnsCapture;
      const AField: TNyxColumnIdentity; AWidth: Integer): INyxPreparedPublication;
    function ConfigureColumns(const APreferences: TNyxColumnPreferences): INyxCollectionColumns;
  end;

  { Optional UI-thread presentation commands preserve every existing view ABI.
    Preparation reserves the existing column publication slot; density/text and
    width plans cannot overwrite one another's original captures. Explicit user
    overrides can activate presentation independently of saved defaults; an
    absent authored policy stays absent. Clearing overrides always works.
    Notifications use the view's existing observer owner and never change data,
    query, selection, column geometry or authored defaults. }
  INyxCollectionTablePresentation = interface(IInterface)
    ['{5B3BE59F-76D3-4812-BDC2-D2EC52ECC4D0}']
    function CapturePresentation: INyxCollectionPresentationCapture;
    function PreparePresentation(const ACapture: INyxCollectionPresentationCapture;
      const APreferences: TNyxTablePresentationPreferences): INyxPreparedPublication;
    function ConfigurePresentation(const APreferences: TNyxTablePresentationPreferences):
      INyxCollectionTablePresentation;
  end;
  { Bounded immutable text from the CURRENT table page, independent of host font
    measurement. Count is at most the authored AutoSizeRows limit; an empty page
    still exposes its column title. Returned descriptors/identities are values.
    A sample retains immutable data and inert identity proofs, never a live view,
    store, renderer or widget. Keeping it does not keep application UI alive. }
  INyxCollectionColumnSample = interface(IInterface)
    ['{1100E223-4C3B-408A-A264-06535915CF24}']
    function GetColumn: TNyxCollectionColumn;
    function GetCount: Integer;
    function ItemAt(AIndex: Integer): TNyxItemRef;
    function TextAt(AIndex: Integer): TNyxText;
    property Column: TNyxCollectionColumn read GetColumn;
    property Count: Integer read GetCount;
  end;
  { Optional measured-fit path. Original Columns/PrepareAutoSize remains the
    caller-supplied-width contract with its existing ABI. This distinct facet
    additionally proves that the sampled page/data/query/layout remained current
    through preparation and the final publication fence. A host still owns font,
    DPI, input/IME, event generation and actual form-open lifetime guards. }
  INyxCollectionColumnSampling = interface(IInterface)
    ['{2C8D0282-DB7C-47B3-8A64-B5314081EE8B}']
    function CaptureColumnSample(const ACapture: INyxCollectionColumnsCapture;
      const AField: TNyxColumnIdentity): INyxCollectionColumnSample;
    function PrepareSampleAutoSize(const ASample: INyxCollectionColumnSample;
      AWidth: Integer): INyxPreparedPublication;
  end;
  { Optional managed runtime paging, preserving the existing view ABI. The view
    owns filtered/ordered membership and one source subscription; this capability
    introduces no renderer or reference cycle. Commands notify once through the
    normal ordered view observers. Invalid/reentrant changes refuse atomically.
    Trees require a hierarchy-aware page contract and explicitly refuse here. }
  INyxCollectionPages = interface(IInterface)
    ['{2543D64A-A030-4F4F-A63B-2AA617655EC0}']
    function GetPaging: TNyxCollectionPaging;
    function GetPageWindow: TNyxCollectionPageWindow;
    function ConfigurePaging(const APolicy: TNyxCollectionPaging): INyxCollectionPages;
    function GoToPage(APage: Integer): INyxCollectionPages;
    function NextPage: INyxCollectionPages;
    function PreviousPage: INyxCollectionPages;
    function FirstPage: INyxCollectionPages;
    function LastPage: INyxCollectionPages;
    property Paging: TNyxCollectionPaging read GetPaging;
    property Window: TNyxCollectionPageWindow read GetPageWindow;
  end;
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

  { Optional UI-owner inspection preserves the original view interface. Pause
    retains its immutable displayed frame while the same atomic store and peer
    views keep publishing. Incoming revisions are coalesced, not queued as extra
    snapshots. Resume reprojects the latest store and reports lost context.
    Editing a paused view refuses; selection/query inspection remains local to
    its frozen source. Status notifications share the existing weak token owner
    but never pretend that a pending update changed the displayed dataset.
    Callers disconnect their status token before freeing its receiver. }
  INyxCollectionInspection = interface(IInterface)
    ['{A4B59572-6705-4D80-9937-7EF2939FB2C4}']
    function GetInspectionPolicy: TNyxCollectionInspectionPolicy;
    procedure ConfigureInspection(const APolicy: TNyxCollectionInspectionPolicy);
    function CaptureInspection: TNyxCollectionInspectionObservation;
    function GetDisplayedSource: INyxCollectionSnapshot;
    procedure PauseInspection;
    procedure ResumeInspection;
    function SubscribeInspection(AObserver: TNyxCollectionInspectionObserver):
      INyxCollectionViewSubscription;
  end;

  { Optional flat List/Table viewport commands use the existing view owner and
    inspection notifications. FollowLatest explicitly requests the current last
    visible row, then follows changed tail identities on source publications.
    KeepReading preserves the current place. Neither changes query, page, row
    selection or authored defaults. Resume is required before FollowLatest;
    Tree physical following is unsupported and refuses explicitly. }
  INyxCollectionFollowing = interface(IInterface)
    ['{C23746F1-9DD8-449B-BE42-09373D90D489}']
    function FollowLatest: INyxCollectionFollowing;
    function KeepReading: INyxCollectionFollowing;
  end;

  { A detached receipt proves identity continuity for the last displayed source
    transition. It owns no source/view and remains safe after this view retires.
    Deferred targets which skipped a different transition must recover explicitly. }
  INyxCollectionPositionView = interface(IInterface)
    ['{2B522861-19F3-40AA-AFA2-A9826F5C6520}']
    function CaptureDisplayContinuity: INyxCollectionDisplayContinuity;
  end;

  { Explicit navigation may reveal a row while leaving keyboard focus in the
    caller, or deliberately transfer it to the Tree. Ordinary updates do neither. }
  TNyxTreeRevealFocus = (ntrfPreserve, ntrfTake);

  { Copied one-shot viewport intent. Serial is local to its exact runtime view;
    zero/undefined Item means no current request. No host or command authority is
    retained. Source/query/disclosure/later focus changes invalidate old intent. }
  TNyxTreeRevealRequest = record
    Serial: Integer;
    Item: TNyxItemRef;
    Focus: TNyxTreeRevealFocus;
  end;

  { Optional explicit Tree navigation, separate from selection and disclosure.
    Reveal admits focus/ancestor opening and a fresh viewport request together,
    even at the same focused row. Exact selection membership/anchor are retained.
    Mounted targets consume only requests admitted during their active input
    incarnation; a newly attached target does not replay historical commands.
    CaptureTreeReveal reads owned facts only, with no application callbacks.
    UI-only; notification errors report an already committed request. }
  INyxTreeReveal = interface(IInterface)
    ['{5BDDB331-87C3-4401-A9B8-54FBF9B1440A}']
    procedure RevealTreeItem(const AItem: TNyxItemRef;
      AFocus: TNyxTreeRevealFocus = ntrfPreserve);
    function CaptureTreeReveal: TNyxTreeRevealRequest;
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
{ Retains the optional runtime page capability. Nil/alternative views without
  that contract refuse instead of implying toolkit-specific paging behavior. }
function NyxCollectionPages(const AView: INyxCollectionView): INyxCollectionPages;
{ Unsupported alternatives/non-table views refuse explicitly. The optional
  facet never manufactures a toolkit-specific column engine or data copy. }
function NyxCollectionColumns(const AView: INyxCollectionView): INyxCollectionColumns;
{ Query the optional runtime owner without changing an existing view or source.
  Unsupported alternatives return nil; a captured source is immutable data only. }
function NyxCollectionInspection(const AView: INyxCollectionView): INyxCollectionInspection;
{ Optional runtime viewport commands; retains the ordinary view, never a mount
  or renderer. A configured inspection policy is required before following. }
function NyxCollectionFollowing(const AView: INyxCollectionView): INyxCollectionFollowing;
{ Retains an optional UI-thread text/density command owner. Unsupported or
  non-table alternatives refuse; no host-specific control API is exposed. }
function NyxCollectionTablePresentation(const AView: INyxCollectionView):
  INyxCollectionTablePresentation;
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
{ Pure target-shape admission, also used after selecting a content recipe or
  viewport presentation. It reads no dataset/resource and creates no live view;
  authored data/schema admission remains independently required. }
procedure ValidateNyxCollectionViewProjection(const ASpec: TNyxCollectionViewSpec;
  AProjection: TNyxCollectionProjection);
{ Validate authored field/query/data meaning while a reusable component or part
  descriptor has no resolved primitive yet. Parent data is still checked for a
  valid hierarchy. The caller must separately admit the realized target through
  ValidateNyxCollectionViewSnapshot; this does not grant a mountable projection. }
procedure ValidateNyxCollectionDescriptorSnapshot(const ASnapshot: INyxCollectionSnapshot;
  const ASpec: TNyxCollectionViewSpec);
{ False for nil/unknown views or active view/store notification. This is a
  synchronous readiness observation, never a reservation or admission cache. }
function NyxCollectionViewPublicationReady(const AView: INyxCollectionView): Boolean;
{ Obtain the optional query capability without changing the original view ABI.
  Unsupported alternate views refuse explicitly; no sequential fallback can
  promise a coordinated query/state publication. }
function NyxCollectionQueryPublication(const AView: INyxCollectionView): INyxCollectionQueryPublication;
{ Shared runtime contribution metadata and scoped commands on the same view.
  Unsupported alternatives refuse explicitly; no parallel query engine exists. }
function NyxCollectionQuerySession(const AView: INyxCollectionView): INyxCollectionQuerySession;
{ Copy only a compatible captured session during detached view recomposition.
  Nil means an alternative lacks the optional session facet/atomic source; the
  caller keeps its existing query-copy fallback. No authority is transferred. }
function PrepareNyxCollectionQueryTransfer(const AFormer,
  ANext: INyxCollectionView): INyxPreparedPublication;
{ Recomposition calls this only after matching the exact runtime identity,
  store and projection. Matching typed fields retain valid preferences; removed
  or changed-family fields are pruned and new fields use current defaults.
  Query/page/selection transfer retains its separate, stricter Spec equality. }
function PrepareNyxCollectionColumnTransfer(const AFormer,
  ANext: INyxCollectionView): INyxPreparedPublication;

implementation

uses
  Math, nyx.text.index, nyx.objects;

type
  { Private forward declarations join source projection and query preparation
    without exposing either implementation type in the portable public API. }
  TView = class;
  TQueryPreparation = class;
  TQuerySessionState = class;
  TQueryScope = class;
  TQueryOwnerLife = class;
  TQueryOwnerLease = class;
  TColumnPreparation = class;
  { Trusted detached recomposition preserves existing preferences when new
    authored capabilities lock further USER changes. The public mutation facet
    never exposes this admission path, and all typed-field/bound checks remain. }
  IColumnTransferTarget = interface(IInterface)
    ['{AD8B34E9-9EF9-43AA-A6C6-73FB37F67838}']
    function PrepareColumnImport(const ACapture: INyxCollectionColumnsCapture;
      const APreferences: TNyxColumnPreferences): INyxPreparedPublication;
  end;
  { Trusted native/browser replacement imports width/order and text/density as
    one participant in the existing column slot. Old public interfaces stay
    intact; external implementations never gain an undocumented bypass. }
  IColumnPresentationTransferTarget = interface(IInterface)
    ['{E29F716F-8507-477B-A8AD-9344E0A7C719}']
    function PrepareColumnPresentationImport(const ACapture: INyxCollectionColumnsCapture;
      const AColumns: TNyxColumnPreferences;
      const APresentation: TNyxTablePresentationPreferences): INyxPreparedPublication;
  end;
  { A caller-owned token borrows its view and exactly one receiver kind. The
    view borrows token pointers; detachment clears both callbacks before either
    receiver can be freed. Inspection-only changes do not invoke data observers. }
  TViewSubscription = class(TInterfacedObject, INyxCollectionViewSubscription)
  public
    Owner: TView;
    Serial: Integer;
    Observer: TNyxCollectionViewObserver;
    InspectionObserver: TNyxCollectionInspectionObserver;
    destructor Destroy; override;
    function GetConnected: Boolean;
    procedure Disconnect;
  end;
  TParentIndexes = TNyxQueryIndexes;

  { One accepted row-copy generation has only an atomic active flag. The view
    retires it before changing accepted fields; captures can outlive that owner
    without querying a mutable object or keeping any subscription alive. }
  TRowCopyReadiness = class(TInterfacedObject, INyxCancellationReadiness)
  private
    FActive: LongInt;
  public
    constructor Create;
    procedure Retire;
    procedure CheckReady;
  end;

  { Immutable copied membership and a detached generation flag. Its getters
    never consult live query/selection state or acquire clipboard authority. }
  TRowCopyCapture = class(TInterfacedObject, INyxCollectionSelectedRowsCapture)
  public
    Source: INyxCollectionSnapshot;
    Page: INyxCollectionSnapshot;
    Selection: TNyxCollectionSelectionSnapshot;
    Columns: INyxCollectionColumnsCapture;
    Readiness: INyxCancellationReadiness;
    function GetSource: INyxCollectionSnapshot;
    function GetPage: INyxCollectionSnapshot;
    function GetSelection: TNyxCollectionSelectionSnapshot;
    function GetColumns: INyxCollectionColumnsCapture;
    function GetReadiness: INyxCancellationReadiness;
  end;

  { Live projection owns accepted query/page/selection state and subscriptions.
    The portable base protects temporary constructor Self leases while applying
    authored inspection defaults, before the factory obtains an interface owner.
    Optional detached row-copy witnesses are retired before state installation
    and final release, and never retain this owner through a captured menu.
    Optional inspection retains the displayed frame and bounded removed-ID facts,
    never a backlog or a second store. Its prepared state uses the SAME atomic
    reservation as normal projection, query and column publications. Explicit
    Tree reveal adds copied one-shot intent to ordinary selection admission;
    source revision, query intent and later Tree commands fence its lifetime. }
  TView = class(TNyxInterfacedObject, INyxCollectionView, INyxTreeHierarchy, INyxTreeReveal,
    INyxCollectionViewPublication, INyxCollectionPages, INyxCollectionQueryPublication,
    INyxCollectionQuerySession, INyxCollectionColumns, IColumnTransferTarget,
    INyxCollectionTablePresentation, IColumnPresentationTransferTarget,
    INyxCollectionColumnSampling, INyxCollectionExportView, INyxCollectionSelectedRowsView,
    INyxCollectionInspection, INyxCollectionPositionView, INyxCollectionFollowing)
  private
    FRowCopyReadiness: INyxCancellationReadiness;
    FRowCopyReadinessObject: TRowCopyReadiness;
    FStore: INyxCollection;
    FAtomic: INyxAtomicCollection;
    FPrepared: TView; { detached, privately owned projection; never subscribed }
    { Query plans have a separate reservation from source-row preparations.
      The plan retains this view; this weak slot cannot form an ownership cycle. }
    FQueryPreparation: TQueryPreparation;
    { A plan retains its view. This weak reservation and inert capture token
      cannot form a cycle; preferences contain only independently owned values. }
    FColumnPreparation: TColumnPreparation;
    FColumnIdentity: IInterface;
    FColumnSerial: Integer;
    { Invalidates measured page text on every accepted view notification,
      including paging-policy ABA returning to the same unpaged snapshot.
      Saturation disables sampling only; ordinary notifications never throw. }
    FColumnSampleSerial: Integer;
    FColumnPreferences: TNyxColumnPreferences;
    FPresentationPreferences: TNyxTablePresentationPreferences;
    { Identity has no owner backlink. Intent includes accepted session metadata
      changes; equal raw assignments remain complete no-ops. }
    FQueryIdentity: IInterface;
    FQueryIntent: Integer;
    { The raw immutable state is borrowed from the adjacent owning lease. It
      contains only query values and inert lifetime flags, never a source/view. }
    FQuerySession: TQuerySessionState;
    FQuerySessionLease: IInterface;
    FNextQueryOwner: Integer;
    FQueryOwnerLives: array of IInterface;
    FQueryOwnerFlags: array of TQueryOwnerLife;
    FSource: INyxCollectionSnapshot;
    FSnapshot: INyxCollectionSnapshot;
    FUnpaged: INyxCollectionSnapshot;
    FPaging: TNyxCollectionPaging;
    FPage: Integer;
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
    FTreeReveal: TNyxTreeRevealRequest;
    FTreeRevealCommand: Integer;
    FTreeRevealRevision: Integer;
    FTreeRevealQueryIntent: Integer;
    FSelection: INyxCollectionSelection;
    FStoreToken: INyxCollectionSubscription;
    FTokens: array of TViewSubscription;
    FNextSerial: Integer;
    FNotifying: Boolean;
    FInspectionPolicy: TNyxCollectionInspectionPolicy;
    FInspectionStatus: TNyxCollectionInspectionObservation;
    FInspectionRemoved: TNyxItemRefs;
    FInspectionIndex: TNyxTextIndex;
    FInspectionPendingOnly: Boolean;
    FInspectionTransition: Boolean;
    FDisplayContinuity: INyxCollectionDisplayContinuity;
    procedure RetireRowCopy;
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
    procedure ProjectSnapshot(const AData: INyxCollectionSnapshot;
      const AChanges: INyxCollectionChanges);
    procedure CopyProjectionTo(ATarget: TView);
    procedure PrepareInspectionUpdate(ATarget: TView;
      const AData: INyxCollectionSnapshot; const AChanges: INyxCollectionChanges);
    procedure PrepareDisplayContinuity(ATarget: TView;
      const AData: INyxCollectionSnapshot; const AChanges: INyxCollectionChanges);
    procedure StoreChanged(const AStore: INyxCollection;
      const AChanges: INyxCollectionChanges);
    procedure Notify(const AChanges: INyxCollectionChanges;
      AInspectionOnly: Boolean = False;
      ACause: TNyxCollectionInspectionCause = ncicViewChanged);
    procedure Disconnect(AToken: TViewSubscription);
    function ReconcileSelection(const ASource, AResult,
      APrevious: INyxCollectionSnapshot;
      ARemoved: TNyxTextIndex = nil): INyxCollectionSelection;
    procedure BuildHierarchy;
    function TreeIndex(const AItem: TNyxItemRef): Integer;
    procedure ReconcileTreeFocus;
    function RevealAncestors(const AFocus: TNyxItemRef): Boolean;
    procedure AdvanceTreeCommand;
    procedure SetSelectionCore(const AItems: array of TNyxItemRef;
      const AFocus, AAnchor: TNyxItemRef; AReveal: Boolean;
      ARevealFocus: TNyxTreeRevealFocus);
  public
    { TView is an implementation-only type. The temporary validator has no
      subscriptions and remains owned solely by its unit-local caller. }
    constructor CreateValidator(const ASpec: TNyxCollectionViewSpec;
      AProjection: TNyxCollectionProjection);
    constructor Create(const AStore: INyxCollection;
      const ASpec: TNyxCollectionViewSpec; AProjection: TNyxCollectionProjection);
    destructor Destroy; override;
    function GetInspectionPolicy: TNyxCollectionInspectionPolicy;
    function CaptureDisplayContinuity: INyxCollectionDisplayContinuity;
    function FollowLatest: INyxCollectionFollowing;
    function KeepReading: INyxCollectionFollowing;
    procedure ConfigureInspection(const APolicy: TNyxCollectionInspectionPolicy);
    function CaptureInspection: TNyxCollectionInspectionObservation;
    function GetDisplayedSource: INyxCollectionSnapshot;
    procedure PauseInspection;
    procedure ResumeInspection;
    function SubscribeInspection(AObserver: TNyxCollectionInspectionObserver):
      INyxCollectionViewSubscription;
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
    function CaptureColumns: INyxCollectionColumnsCapture;
    function CapturePresentation: INyxCollectionPresentationCapture;
    function PreparePresentation(const ACapture: INyxCollectionPresentationCapture;
      const APreferences: TNyxTablePresentationPreferences): INyxPreparedPublication;
    function ConfigurePresentation(const APreferences: TNyxTablePresentationPreferences):
      INyxCollectionTablePresentation;
    function CaptureExport: INyxCollectionExportCapture;
    function CaptureSelectedRows: INyxCollectionSelectedRowsCapture;
    procedure CheckExportCurrent(const ACapture: INyxCollectionExportCapture);
    function PrepareColumns(const ACapture: INyxCollectionColumnsCapture;
      const APreferences: TNyxColumnPreferences): INyxPreparedPublication;
    function PrepareAutoSize(const ACapture: INyxCollectionColumnsCapture;
      const AField: TNyxColumnIdentity; AWidth: Integer): INyxPreparedPublication;
    function ConfigureColumns(const APreferences: TNyxColumnPreferences): INyxCollectionColumns;
    function CaptureColumnSample(const ACapture: INyxCollectionColumnsCapture;
      const AField: TNyxColumnIdentity): INyxCollectionColumnSample;
    function PrepareSampleAutoSize(const ASample: INyxCollectionColumnSample;
      AWidth: Integer): INyxPreparedPublication;
    function PrepareColumnImport(const ACapture: INyxCollectionColumnsCapture;
      const APreferences: TNyxColumnPreferences): INyxPreparedPublication;
    function PrepareColumnPresentationImport(const ACapture: INyxCollectionColumnsCapture;
      const AColumns: TNyxColumnPreferences;
      const APresentation: TNyxTablePresentationPreferences): INyxPreparedPublication;
    function CaptureQuery: INyxCollectionQueryCapture;
    function PrepareQuery(const ACapture: INyxCollectionQueryCapture;
      const AQuery: TNyxCollectionQuery): INyxPreparedPublication;
    function Capture: INyxCollectionQuerySessionSnapshot;
    function AcquireOwner(const AOrigin: TNyxQueryOriginRef;
      const ALabel: TNyxText): INyxCollectionQueryOwner;
    function PrepareSession(const ACapture: INyxCollectionQuerySessionSnapshot;
      const AOwner: INyxCollectionQueryOwner;
      const AEdits: TNyxQuerySessionEdits): INyxPreparedPublication;
    function PrepareRebase(const ACapture: INyxCollectionQuerySessionSnapshot;
      const AOwner: INyxCollectionQueryOwner;
      const AAuthoredSpec: TNyxCollectionViewSpec): INyxPreparedPublication;
    function PrepareImport(const ATargetCapture,
      ASourceSnapshot: INyxCollectionQuerySessionSnapshot): INyxPreparedPublication;
    function GetPaging: TNyxCollectionPaging;
    function GetPageWindow: TNyxCollectionPageWindow;
    function ConfigurePaging(const APolicy: TNyxCollectionPaging): INyxCollectionPages;
    function GoToPage(APage: Integer): INyxCollectionPages;
    function NextPage: INyxCollectionPages;
    function PreviousPage: INyxCollectionPages;
    function FirstPage: INyxCollectionPages;
    function LastPage: INyxCollectionPages;
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
    procedure RevealTreeItem(const AItem: TNyxItemRef;
      AFocus: TNyxTreeRevealFocus = ntrfPreserve);
    function CaptureTreeReveal: TNyxTreeRevealRequest;
  end;

  { Private proof is independent of policy equality, source revision and caller
    names. Its inert identity token has no backreference to a view or dataset.
    Foreign public captures cannot acquire authority by returning the same query. }
  IQueryCaptureProof = interface(IInterface)
    ['{97FBA0A4-2294-45D2-A660-8FD3F068E369}']
    function QueryIdentity: IInterface;
    function QueryIntent: Integer;
  end;

  { An immutable, cheaply retained query-intent observation. Row updates may
    occur after capture; only preparation retains its then-current dataset. }
  TQueryCapture = class(TInterfacedObject, INyxCollectionQueryCapture, IQueryCaptureProof)
  private
    FIdentity: IInterface;
    FIntent: Integer;
    FQuery: TNyxCollectionQuery;
  public
    constructor Create(const AIdentity: IInterface; AIntent: Integer;
      const AQuery: TNyxCollectionQuery);
    function GetQuery: TNyxCollectionQuery;
    function QueryIdentity: IInterface;
    function QueryIntent: Integer;
  end;

  { One reserved, single-use query participant for the existing publication
    coordinator. The detached TView is only a projection calculator: it never
    subscribes to a store or owns a renderer. Former model references remain
    leased until retirement, including after a pure adoption into the live view. }
  TQueryPreparation = class(TInterfacedObject, INyxPreparedPublication,
    INyxPublicationReadiness)
  private
    FOwner: TView;
    FLease: INyxCollectionView;
    FSource: INyxCollectionSnapshot;
    FBeforeSnapshot: INyxCollectionSnapshot;
    FBeforeUnpaged: INyxCollectionSnapshot;
    FBeforeSelection: INyxCollectionSelection;
    FBeforeQuery: TNyxCollectionQuery;
    FQuery: TNyxCollectionQuery;
    FIntent: Integer;
    FBeforeSession: TQuerySessionState;
    FBeforeSessionLease: IInterface;
    FSession: TQuerySessionState;
    FSessionLease: IInterface;
    FSessionChanged: Boolean;
    FNextOwner: Integer;
    FHasCommandOwner: Boolean;
    FCommandInfo: TNyxQueryOwnerInfo;
    FCommandLife: TQueryOwnerLife;
    FCommandLifeLease: IInterface;
    FCandidate: TView;
    FChanged: Boolean;
    FValidationStarted: Boolean;
    FValidated: Boolean;
    FInstalled: Boolean;
    FNotified: Boolean;
    FRetired: Boolean;
    procedure RequireCurrent;
    procedure RequireSessionCapture(const ACapture: INyxCollectionQuerySessionSnapshot);
    procedure RequireSessionOwner(const AOwner: INyxCollectionQueryOwner);
    procedure SetSession(AState: TQuerySessionState; const ALease: IInterface);
  public
    { The factory acquires a managed interface before Initialize may call an
      alternate interface implementation. No constructor self-retention trap. }
    procedure Initialize(AOwner: TView; const ACapture: INyxCollectionQueryCapture;
      const AQuery: TNyxCollectionQuery);
    destructor Destroy; override;
    procedure Validate;
    procedure CheckReady;
    procedure Install;
    procedure Notify;
    procedure Retire;
  end;

  { Private capture authority consists only of an inert target token and an
    accepted arrangement serial. It has no owner/store/widget backreference. }
  IColumnCaptureProof = interface(IInterface)
    ['{301CF878-175A-48D3-817C-FC3288D34731}']
    function ColumnIdentity: IInterface;
    function ColumnSerial: Integer;
  end;

  { Detached inspector/command context. Returning copies prevents a pas2js
    caller's record-array mutation from changing any later reader's snapshot. }
  TColumnCapture = class(TInterfacedObject, INyxCollectionColumnsCapture,
    INyxCollectionPresentationCapture,
    IColumnCaptureProof)
  private
    FIdentity: IInterface;
    FSerial: Integer;
    FSpec: TNyxCollectionViewSpec;
    FPreferences: TNyxColumnPreferences;
    FPresentation: TNyxTablePresentationPreferences;
  public
    constructor Create(const AIdentity: IInterface; ASerial: Integer;
      const ASpec: TNyxCollectionViewSpec; const APreferences: TNyxColumnPreferences;
      const APresentation: TNyxTablePresentationPreferences);
    function GetSpec: TNyxCollectionViewSpec;
    function GetPreferences: TNyxColumnPreferences;
    function GetPresentationPreferences: TNyxTablePresentationPreferences;
    function GetSerial: Integer;
    function ColumnIdentity: IInterface;
    function ColumnSerial: Integer;
  end;

  { Forward output object is borrowed only under its private proof interface. }
  TExportCapture = class;
  { A foreign implementation cannot forge an accepted table's identity/intent.
    This proof owns only the immutable capture, never its original live view. }
  IExportCaptureProof = interface(IInterface)
    ['{24088165-BA2D-4414-A8D8-AE0C3085AA97}']
    function ExportData: TExportCapture;
  end;
  { Complete output captures retain actual immutable membership and inert
    generation witnesses. No getter calls back into a view or a source owner. }
  TExportCapture = class(TInterfacedObject, INyxCollectionExportCapture,
    IExportCaptureProof)
  public
    Identity: IInterface;
    QueryIdentity: IInterface;
    QueryIntent: Integer;
    ColumnSerial: Integer;
    ObservationSerial: Integer;
    Source: INyxCollectionSnapshot;
    Filtered: INyxCollectionSnapshot;
    Page: INyxCollectionSnapshot;
    Columns: INyxCollectionColumnsCapture;
    function GetSource: INyxCollectionSnapshot;
    function GetFiltered: INyxCollectionSnapshot;
    function GetPage: INyxCollectionSnapshot;
    function GetColumns: INyxCollectionColumnsCapture;
    function ExportData: TExportCapture;
  end;

  TColumnSample = class;
  { Unit-private canonical payload proof. The public sample exposes bounded
    values only. Retain its interface while borrowing this exact private object;
    alternative public sample implementations cannot invent a canonical payload. }
  IColumnSampleProof = interface(IInterface)
    ['{ED3844A9-FCCC-4C81-A8C4-FF75871C9635}']
    function SampleData: TColumnSample;
  end;
  { A page sample owns immutable accepted snapshots for identity/lifetime and
    copies at most the declared bounded text sample. It has no view backlink.
    RequireCurrent reads only the supplied canonical owner's cached fields and
    this immutable payload; it is safe in the final prepared readiness phase. }
  TColumnSample = class(TInterfacedObject, INyxCollectionColumnSample,
    IColumnSampleProof)
  private
    FIdentity: IInterface;
    FOriginal: INyxCollectionColumnsCapture;
    FSource, FSnapshot, FUnpaged: INyxCollectionSnapshot;
    FPreferences: TNyxColumnPreferences;
    FColumn: TNyxCollectionColumn;
    FColumnSerial, FSampleSerial, FQueryIntent: Integer;
    FItems: TNyxItemRefs;
    FTexts: array of TNyxText;
    procedure RequireCurrent(AOwner: TView);
  public
    function GetColumn: TNyxCollectionColumn;
    function GetCount: Integer;
    function ItemAt(AIndex: Integer): TNyxItemRef;
    function TextAt(AIndex: Integer): TNyxText;
    function SampleData: TColumnSample;
  end;

  { A single-use participant in the existing atomic publication coordinator.
    Both former and candidate vectors remain leased through retirement, leaving
    Install with nonthrowing adoption only. No detached second data view or
    second row/selection/query engine is needed for a presentation change. }
  TColumnPreparation = class(TInterfacedObject, INyxPreparedPublication,
    INyxPublicationReadiness)
  private
    FOwner: TView;
    FLease: INyxCollectionView;
    FSource, FSnapshot: INyxCollectionSnapshot;
    FSelection: INyxCollectionSelection;
    FBefore, FNext: TNyxColumnPreferences;
    { Both presentation vectors stay retained through flags-only Install. One
      shared serial fences concurrent column plans and same-byte ABA changes. }
    FBeforePresentation: TNyxTablePresentationPreferences;
    FNextPresentation: TNyxTablePresentationPreferences;
    FApplyPresentation: Boolean;
    FOnlyPresentation: Boolean;
    FSerial: Integer;
    FChanged, FValidated, FValidationStarted: Boolean;
    FInstalled, FNotified, FRetired: Boolean;
    FImported: Boolean;
    FAutoSize: TNyxColumnIdentity;
    FSample: TColumnSample;
    FSampleLease: INyxCollectionColumnSample;
    procedure RequireCurrent;
  public
    procedure Initialize(AOwner: TView; const ACapture: INyxCollectionColumnsCapture;
      const APreferences: TNyxColumnPreferences;
      const AAutoSize: TNyxColumnIdentity; AImported: Boolean = False);
    procedure InitializeSample(AOwner: TView; ASample: TColumnSample;
      AWidth: Integer);
    procedure InitializePresentation(AOwner: TView;
      const ACapture: INyxCollectionColumnsCapture;
      const AColumns: TNyxColumnPreferences;
      const APresentation: TNyxTablePresentationPreferences;
      AOnlyPresentation: Boolean; AImported: Boolean = False);
    destructor Destroy; override;
    procedure Validate;
    procedure CheckReady;
    procedure Install;
    procedure Notify;
    procedure Retire;
  end;

  { A generation flag and owner flag are inert ports. A lease/snapshot may keep
    them alive after a view disappears, without keeping that view or store alive. }
  TQueryScope = class(TInterfacedObject)
  public
    Active: Boolean;
  end;
  TQueryOwnerLife = class(TInterfacedObject)
  public
    Active: Boolean;
    Scope: TQueryScope;
    ScopeLease: IInterface;
  end;

  { Immutable canonical values, with parallel inert owner-life leases. Only
    captured Connected presentation is dynamic; values never expose these ports.
    Composition uses the existing typed query tree and its existing budgets. }
  TQuerySessionState = class(TInterfacedObject)
  public
    Scope: TQueryScope;
    ScopeLease: IInterface;
    Baseline: TNyxCollectionQuery;
    Effective: TNyxCollectionQuery;
    HasUserSort: Boolean;
    UserSort: TNyxCollectionQuery;
    SortOwner: TNyxQueryOwnerInfo;
    SortLife: TQueryOwnerLife;
    SortLifeLease: IInterface;
    Contributions: array of TNyxQueryContribution;
    OwnerFlags: array of TQueryOwnerLife;
    OwnerLives: array of IInterface;
    procedure Initialize(const ABaseline: TNyxCollectionQuery);
    procedure Assign(AOther: TQuerySessionState);
    procedure RemoveAt(AIndex: Integer);
    procedure Compose;
    procedure Validate(const ASchema: TNyxCollectionSchema);
    function SameValues(AOther: TQuerySessionState): Boolean;
    procedure Apply(const AOwner: TNyxQueryOwnerInfo; ALife: TQueryOwnerLife;
      const ALifeLease: IInterface; const AEdits: TNyxQuerySessionEdits);
    function Import(const ASource: INyxCollectionQuerySessionSnapshot): Integer;
  end;

  { Private proof types are not numeric IDs or public snapshot getters. Final
    fences retain their exact canonical objects and read fields only. }
  IQueryOwnerProof = interface(IInterface)
    ['{A05EBCA5-4828-47FC-8895-6855B964094B}']
    function OwnerLease: TQueryOwnerLease;
  end;
  IQuerySessionProof = interface(IInterface)
    ['{AA1CD2E4-C8D3-4C1A-9BC4-DA8201A7E2AB}']
    function Identity: IInterface;
    function Intent: Integer;
    function Session: TQuerySessionState;
  end;
  TQueryOwnerLease = class(TInterfacedObject, INyxCollectionQueryOwner, IQueryOwnerProof)
  public
    Info: TNyxQueryOwnerInfo;
    Life: TQueryOwnerLife;
    LifeLease: IInterface;
    destructor Destroy; override;
    function GetID: TNyxQueryOwnerID;
    function GetOrigin: TNyxQueryOriginRef;
    function GetCaption: TNyxText;
    function GetConnected: Boolean;
    procedure Revoke;
    function OwnerLease: TQueryOwnerLease;
  end;

  { Public observations freeze presentation booleans at capture time. The
    private retained state/identity is inert and grants no command lease. }
  TQuerySessionSnapshot = class(TInterfacedObject,
    INyxCollectionQuerySessionSnapshot, IQuerySessionProof)
  private
    FState: TQuerySessionState;
    FStateLease: IInterface;
    FIdentity: IInterface;
    FIntent: Integer;
    FContributions: array of TNyxQueryContribution;
    FSortOwner: TNyxQueryOwnerInfo;
  public
    constructor Create(AState: TQuerySessionState; const AStateLease,
      AIdentity: IInterface; AIntent: Integer);
    function GetBaseline: TNyxCollectionQuery;
    function GetEffective: TNyxCollectionQuery;
    function GetHasUserSort: Boolean;
    function GetUserSort: TNyxCollectionQuery;
    function GetSortOwner: TNyxQueryOwnerInfo;
    function GetCount: Integer;
    function ContributionAt(AIndex: Integer): TNyxQueryContribution;
    function Identity: IInterface;
    function Intent: Integer;
    function Session: TQuerySessionState;
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

constructor TQueryCapture.Create(const AIdentity: IInterface; AIntent: Integer;
  const AQuery: TNyxCollectionQuery);
begin
  inherited Create;
  FIdentity := AIdentity;
  FIntent := AIntent;
  FQuery := AQuery.Copy;
end;

function TQueryCapture.GetQuery: TNyxCollectionQuery;
begin
  Result := FQuery.Copy;
end;

function TQueryCapture.QueryIdentity: IInterface;
begin
  Result := FIdentity;
end;

function TQueryCapture.QueryIntent: Integer;
begin
  Result := FIntent;
end;

function NyxCollectionQueryPublication(const AView: INyxCollectionView): INyxCollectionQueryPublication;
var
  LPublication: INyxCollectionQueryPublication;
begin
  LPublication := nil;

  if (AView = nil) or not Supports(AView, INyxCollectionQueryPublication, LPublication) then
  begin
    raise ENyxCollection.Create('Collection view does not support prepared queries');
  end;
  { A local owning QI result is required by both compilers. Never hand a field
    directly to Supports and leave pas2js with an already released field value. }
  Result := LPublication;
end;

procedure TQueryPreparation.Initialize(AOwner: TView;
  const ACapture: INyxCollectionQueryCapture; const AQuery: TNyxCollectionQuery);
var
  LProof: IQueryCaptureProof;
  LCaptureLease: INyxCollectionQueryCapture;
  LSession: TQuerySessionState;
  LSessionLease: IInterface;
begin
  FLease := AOwner as INyxCollectionView;
  FOwner := AOwner;
  LCaptureLease := ACapture;
  { Preserve the entry value before even the atomic source's Busy getter can
    call application code. Predicates are already closed descriptors at Where. }
  FQuery := TNyxCollectionQuery.FromData(AQuery.ToData);
  FOwner.Writable;

  { Writable may call an alternate source. Recheck only canonical fields after
    it returns: a nested preparation must never lose its live reservation. }

  if FOwner.FNotifying or (FOwner.FQueryPreparation <> nil) or
    (FOwner.FAtomic = nil) or (FOwner.FPrepared <> nil) then
  begin
    raise ENyxCollection.Create('Prepared query requires an available atomic collection source');
  end;
  FSource := FOwner.FSource;
  FBeforeSnapshot := FOwner.FSnapshot;
  FBeforeUnpaged := FOwner.FUnpaged;
  FBeforeSelection := FOwner.FSelection;
  FBeforeQuery := FOwner.FQuery.Copy;
  FBeforeSession := FOwner.FQuerySession;
  FBeforeSessionLease := FOwner.FQuerySessionLease;
  FNextOwner := FOwner.FNextQueryOwner;
  FIntent := FOwner.FQueryIntent;
  FOwner.FQueryPreparation := Self;
  FChanged := FQuery.ToData.ToJSON <> FBeforeQuery.ToData.ToJSON;
  LProof := nil;

  if (LCaptureLease = nil) or not Supports(LCaptureLease, IQueryCaptureProof, LProof) then
  begin
    raise ENyxCollection.Create('Prepared query requires its exact view capture');
  end;

  if (LProof.QueryIdentity <> FOwner.FQueryIdentity) or
    (LProof.QueryIntent <> FIntent) then
  begin
    raise ENyxCollection.Create('Collection query capture belongs to another or changed view intent');
  end;
  { The private proof never calls or trusts the foreign public GetQuery method. }

  if FChanged and (FIntent = High(Integer)) then
  begin
    raise ENyxCollection.Create('Collection query intent is exhausted');
  end;

  if FChanged then
  begin
    { Raw effective-policy replacement starts a fresh baseline/generation.
      The former state and generation remain leased through group retirement. }
    LSession := TQuerySessionState.Create;
    LSessionLease := LSession;
    LSession.Initialize(FQuery);
    FSession := LSession;
    FSessionLease := LSessionLease;
    FSessionChanged := True;
    FNextOwner := 0;
  end;
  LProof := nil;
  LCaptureLease := nil;
  RequireCurrent;
end;

destructor TQueryPreparation.Destroy;
begin
  Retire;
  inherited Destroy;
end;

procedure TQueryPreparation.RequireCurrent;
begin

  if FRetired or (FOwner = nil) then
  begin
    raise ENyxCollection.Create('Prepared query has retired');
  end;

  if (FOwner.FQueryPreparation <> Self) or FOwner.FNotifying or
    (FOwner.FPrepared <> nil) or (FOwner.FQueryIntent <> FIntent) or
    (FOwner.FQuerySession <> FBeforeSession) or not FBeforeSession.Scope.Active or
    (FOwner.FSource <> FSource) or (FOwner.FSnapshot <> FBeforeSnapshot) or
    (FOwner.FUnpaged <> FBeforeUnpaged) or (FOwner.FSelection <> FBeforeSelection) then
  begin
    raise ENyxCollection.Create('Prepared query view/source/intent has changed');
  end;

  if FHasCommandOwner and ((FCommandLife = nil) or not FCommandLife.Active or
    (FCommandLife.Scope <> FBeforeSession.Scope) or
    not FCommandLife.Scope.Active) then
  begin
    raise ENyxCollection.Create('Query command owner has revoked or belongs to another session');
  end;
end;

procedure TQueryPreparation.Validate;
var
  LReadyLease: INyxPublicationReadiness;
  LParents: TParentIndexes;
  LWindow: TNyxCollectionPageWindow;
begin
  { Keep the plan and hence the view/source alive through outward immutable
    snapshot reads. The same owned lease performs the final readiness check. }
  LReadyLease := Self;
  RequireCurrent;

  if FValidated then
  begin
    Exit;
  end;

  if FValidationStarted then
  begin
    raise ENyxCollection.Create('A refused query preparation cannot be validated again');
  end;
  FValidationStarted := True;

  if FSession <> nil then
  begin
    FSession.Validate(FSource.Schema);
    RequireCurrent;
  end;

  if FChanged then
  begin
    FCandidate := TView.CreateValidator(FOwner.FSpec, FOwner.FProjection);
    FCandidate.FSource := FSource;
    FCandidate.FQuery := FQuery;
    FCandidate.FSnapshot := FBeforeSnapshot;
    FCandidate.FUnpaged := FBeforeUnpaged;
    FCandidate.FPaging := FOwner.FPaging;
    FCandidate.FPage := FOwner.FPage;
    FCandidate.FSelection := FBeforeSelection;
    FCandidate.FExpanded := Copy(FOwner.FExpanded, 0, Length(FOwner.FExpanded));
    FCandidate.ValidateDataset(FSource, LParents);
    FCandidate.FSnapshot := NyxQuerySnapshot(FSource, FQuery, LParents, FCandidate.FParents);
    FCandidate.FUnpaged := FCandidate.FSnapshot;

    if FCandidate.FPaging.Defined then
    begin
      LWindow := NyxCollectionPageWindow(FCandidate.FUnpaged.Count,
        FCandidate.FPaging.ItemsPerPage, FCandidate.FPage);
      FCandidate.FSnapshot := NyxPageSnapshot(FCandidate.FUnpaged, LWindow);
      FCandidate.FPage := LWindow.Page;
    end;
    FCandidate.FSelection := FCandidate.ReconcileSelection(FSource,
      FCandidate.FSnapshot, FBeforeSnapshot);
    FCandidate.BuildHierarchy;
    FCandidate.ReconcileTreeFocus;
  end;
  RequireCurrent;
  FValidated := True;
  LReadyLease.CheckReady;
end;

procedure TQueryPreparation.CheckReady;
begin
  { Pure canonical fields only. In particular do not call a store/scheduler/
    capture getter after the complete group's outward validators have run. }
  RequireCurrent;

  if not FValidated or FInstalled then
  begin
    raise ENyxCollection.Create('Prepared query readiness requires a validated candidate');
  end;
end;

procedure TQueryPreparation.Install;
begin
  { All allocation, source reads and final fences precede this adoption. Former
    interfaces remain leased on the plan, so assignment cannot finalize them. }

  if FValidated and not FInstalled and not FRetired and (FOwner <> nil) then
  begin

    if FChanged then
    begin
      FOwner.RetireRowCopy;
      FOwner.FQuery := FQuery;
      FOwner.FSnapshot := FCandidate.FSnapshot;
      FOwner.FUnpaged := FCandidate.FUnpaged;
      FOwner.FPage := FCandidate.FPage;
      FOwner.FParents := FCandidate.FParents;
      FOwner.FSelection := FCandidate.FSelection;
      FOwner.FExpanded := FCandidate.FExpanded;
      FOwner.FFirstChild := FCandidate.FFirstChild;
      FOwner.FNextSibling := FCandidate.FNextSibling;
    end;

    if FSessionChanged then
    begin

      if FSession.Scope <> FBeforeSession.Scope then
      begin
        FBeforeSession.Scope.Active := False;
      end;
      FOwner.FQuerySession := FSession;
      FOwner.FQuerySessionLease := FSessionLease;
      FOwner.FNextQueryOwner := FNextOwner;
    end;

    if FChanged or FSessionChanged then
    begin
      FOwner.FQueryIntent := FIntent + 1;
    end;
    FInstalled := True;
  end;
end;

procedure TQueryPreparation.Notify;
begin

  if not FInstalled or FRetired or FNotified then
  begin
    Exit;
  end;
  FNotified := True;

  if FChanged or FSessionChanged then
  begin
    { Notify leases the view itself and attempts every independent observer.
      No plan/owner access follows a callback that may release caller handles. }
    FOwner.Notify(nil);
  end;
end;

procedure TQueryPreparation.Retire;
var
  LCandidate: TView;
begin

  if FRetired then
  begin
    Exit;
  end;
  FRetired := True;

  if (FOwner <> nil) and (FOwner.FQueryPreparation = Self) then
  begin
    FOwner.FQueryPreparation := nil;
  end;
  FOwner := nil;
  LCandidate := FCandidate;
  FCandidate := nil;
  LCandidate.Free;
  FSource := nil;
  FBeforeSnapshot := nil;
  FBeforeUnpaged := nil;
  FBeforeSelection := nil;
  FBeforeQuery := Default(TNyxCollectionQuery);
  FQuery := Default(TNyxCollectionQuery);
  FCommandLife := nil;
  FCommandLifeLease := nil;
  FSession := nil;
  FSessionLease := nil;
  FBeforeSession := nil;
  FBeforeSessionLease := nil;
  { Last release may dispose the independent model. It cannot roll back an
    installed query and no borrowed owner is accessed afterward. }
  FLease := nil;
end;

{$include nyx.collections.query.session.implementation.inc}

{$include nyx.collections.arrangement.implementation.inc}

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
  Observer := nil;
  InspectionObserver := nil;

  if Owner <> nil then
  begin
    Owner.Disconnect(Self);
  end;
end;

constructor TRowCopyReadiness.Create;
begin
  inherited Create;
  FActive := 1;
end;

procedure TRowCopyReadiness.Retire;
begin
  {$ifdef PAS2JS}
  FActive := 0;
  {$else}
  InterlockedExchange(FActive, 0);
  {$endif}
end;

procedure TRowCopyReadiness.CheckReady;
var
  LActive: Boolean;
begin
  {$ifdef PAS2JS}
  LActive := FActive <> 0;
  {$else}
  LActive := InterlockedCompareExchange(FActive, 0, 0) <> 0;
  {$endif}

  if not LActive then
  begin
    raise ENyxCollection.Create('Original selected-row cohort is no longer current');
  end;
end;

function TRowCopyCapture.GetSource: INyxCollectionSnapshot;
begin
  Result := Source;
end;

function TRowCopyCapture.GetPage: INyxCollectionSnapshot;
begin
  Result := Page;
end;

function TRowCopyCapture.GetSelection: TNyxCollectionSelectionSnapshot;
begin
  Result := Selection;
end;

function TRowCopyCapture.GetColumns: INyxCollectionColumnsCapture;
begin
  Result := Columns;
end;

function TRowCopyCapture.GetReadiness: INyxCancellationReadiness;
begin
  Result := Readiness;
end;

procedure TView.RetireRowCopy;
begin

  if FRowCopyReadinessObject <> nil then
  begin
    FRowCopyReadinessObject.Retire;
  end;
  FRowCopyReadinessObject := nil;
  FRowCopyReadiness := nil;
end;

constructor TView.Create(const AStore: INyxCollection;
  const ASpec: TNyxCollectionViewSpec; AProjection: TNyxCollectionProjection);
var
  LParents: TParentIndexes;
  LAtomic: INyxAtomicCollection;
begin
  inherited Create;
  { Captures retain only this inert token, never the live view or its source. }
  FQueryIdentity := TInterfacedObject.Create;
  FColumnIdentity := TInterfacedObject.Create;
  FColumnPreferences := NyxColumnPreferences;
  FPresentationPreferences := NyxTablePresentationPreferences;
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

  if ASpec.InspectionPolicy.Defined and (AProjection = cpTree) and
    (ASpec.InspectionPolicy.DefaultFollow = ncfLastVisible) then
  begin
    raise ENyxCollection.Create('Tree physical following is not supported');
  end;

  if ASpec.HasTableWorkbenchPolicy and (AProjection <> cpTable) then
  begin
    raise ENyxCollection.Create('Authored workbench actions require a table projection');
  end;

  if ASpec.HasPresentationPolicy and (AProjection <> cpTable) then
  begin
    raise ENyxCollection.Create('Table text and density policies require a table projection');
  end;

  if ASpec.HasLayoutControls and (AProjection <> cpTable) then
  begin
    raise ENyxCollection.Create('Column arrangement requires a table projection');
  end;

  if ASpec.HasColumnFormat and (AProjection <> cpTable) then
  begin
    raise ENyxCollection.Create('Column display policies require a table projection');
  end;

  if ASpec.PagingPolicy.Defined and (AProjection = cpTree) then
  begin
    raise ENyxCollection.Create('Tree paging requires a hierarchy-aware policy');
  end;
  FStore := AStore;
  FSpec := ASpec.Copy;
  FProjection := AProjection;
  FSource := FStore.Snapshot;
  FInspectionStatus.DisplayRevision := FSource.Revision;
  FInspectionStatus.IncomingRevision := FInspectionStatus.DisplayRevision;
  FQuery := ASpec.QueryPolicy;
  FQuerySession := TQuerySessionState.Create;
  FQuerySessionLease := FQuerySession;
  FQuerySession.Initialize(FQuery);
  ValidateDataset(FSource, LParents);
  FSnapshot := NyxQuerySnapshot(FSource, FQuery, LParents, FParents);
  FUnpaged := FSnapshot;
  FPage := 1;

  if ASpec.PagingPolicy.Defined then
  begin
    FPaging := ASpec.PagingPolicy;
    FPage := NyxCollectionPageWindow(FUnpaged.Count, FPaging.ItemsPerPage,
      FPaging.FirstPage).Page;
    FSnapshot := NyxPageSnapshot(FUnpaged, NyxCollectionPageWindow(FUnpaged.Count,
      FPaging.ItemsPerPage, FPage));
  end;

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
    FInspectionStatus.Supported := True;
    FStoreToken := FAtomic.SubscribePrepared(StoreChanged, PrepareCandidate,
      InstallCandidate, RetireCandidate);
  end
  else
  begin
    { An alternative store may retain the original single-store contract. It
      cannot join a coordinated publication without the optional capability. }
    FStoreToken := FStore.Subscribe(StoreChanged, ValidateCandidate);
  end;

  if ASpec.InspectionPolicy.Defined then
  begin
    { No view observers exist during construction. Runtime defaults are copied
      through the same validated policy path used by application overrides. }
    ConfigureInspection(ASpec.InspectionPolicy);
  end;
end;

destructor TView.Destroy;
var
  LIndex: Integer;
begin
  { Lifetime ports retire before source disconnection or any model finalizer.
    Retained snapshots/leases can observe retirement without retaining Self. }
  RetireRowCopy;

  if (FQuerySession <> nil) and (FQuerySession.Scope <> nil) then
  begin
    FQuerySession.Scope.Active := False;
  end;
  RetireCandidate;

  if FStoreToken <> nil then
  begin
    FStoreToken.Disconnect;
    FStoreToken := nil;
  end;
  for LIndex := 0 to Length(FTokens) - 1 do
  begin
    FTokens[LIndex].Owner := nil;
    FTokens[LIndex].Observer := nil;
    FTokens[LIndex].InspectionObserver := nil;
  end;
  FTokens := nil;
  FSnapshot := nil;
  FSource := nil;
  FAtomic := nil;
  FStore := nil;
  FreeAndNil(FInspectionIndex);
  FInspectionRemoved := nil;
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

procedure ValidateNyxCollectionDescriptorSnapshot(const ASnapshot: INyxCollectionSnapshot;
  const ASpec: TNyxCollectionViewSpec);
var
  LValidator: TView;
  LParents: TParentIndexes;
  LDataProjection: TNyxCollectionProjection;
begin
  ASpec.Validate;

  if not ASpec.Defined then
  begin
    raise ENyxCollection.Create('Saved collection descriptor requires a binding');
  end;
  { This private validator chooses only the appropriate data traversal. It
    asserts no actual control kind and creates no store or subscription. }
  LDataProjection := cpList;

  if ASpec.ParentField <> '' then
  begin
    LDataProjection := cpTree;
  end;
  LValidator := TView.CreateValidator(ASpec, LDataProjection);
  try
    LValidator.ValidateDataset(ASnapshot, LParents);
  finally
    LValidator.Free;
  end;
end;

procedure ValidateNyxCollectionViewProjection(const ASpec: TNyxCollectionViewSpec;
  AProjection: TNyxCollectionProjection);
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

  if ASpec.InspectionPolicy.Defined and (AProjection = cpTree) and
    (ASpec.InspectionPolicy.DefaultFollow = ncfLastVisible) then
  begin
    raise ENyxCollection.Create('Tree physical following is not supported');
  end;

  if ASpec.HasTableWorkbenchPolicy and (AProjection <> cpTable) then
  begin
    raise ENyxCollection.Create('Authored workbench actions require a table projection');
  end;

  if ASpec.HasPresentationPolicy and (AProjection <> cpTable) then
  begin
    raise ENyxCollection.Create('Table text and density policies require a table projection');
  end;

  if ASpec.HasLayoutControls and (AProjection <> cpTable) then
  begin
    raise ENyxCollection.Create('Column arrangement requires a table projection');
  end;



  if ASpec.HasColumnFormat and (AProjection <> cpTable) then
  begin
    raise ENyxCollection.Create('Column display policies require a table projection');
  end;

  if ASpec.PagingPolicy.Defined and (AProjection = cpTree) then
  begin
    { A flat interval could detach a child from its ancestor. Reject before
      document admission as well as before constructing either target view. }
    raise ENyxCollection.Create('Tree paging requires a hierarchy-aware policy');
  end;

  if (ASpec.ParentField <> '') and (AProjection <> cpTree) then
  begin
    raise ENyxCollection.Create('Parent fields require a tree projection');
  end;
end;

procedure ValidateNyxCollectionViewSnapshot(const ASnapshot: INyxCollectionSnapshot;
  const ASpec: TNyxCollectionViewSpec; AProjection: TNyxCollectionProjection);
begin
  ValidateNyxCollectionViewProjection(ASpec, AProjection);
  ValidateNyxCollectionDescriptorSnapshot(ASnapshot, ASpec);
end;

procedure TView.Writable;
var
  LBusy: Boolean;
begin

  if FInspectionTransition or FNotifying or (FPrepared <> nil) or (FQueryPreparation <> nil) or
    (FColumnPreparation <> nil) then
  begin
    raise ENyxCollection.Create('Reentrant collection view mutation is not supported');
  end;
  LBusy := False;

  if FAtomic <> nil then
  begin
    LBusy := FAtomic.Busy;
  end;
  { Busy may be implemented by another store and call trusted application code.
    Recheck every canonical reservation AFTER it returns, so such a callback
    cannot acquire a column plan then let an older query/selection command pass. }

  if FInspectionTransition or LBusy or FNotifying or (FPrepared <> nil) or
    (FQueryPreparation <> nil) or (FColumnPreparation <> nil) then
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
        LColumn.ValidateDisplay(LSchema);
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

  if FColumnPreparation <> nil then
  begin
    raise ENyxCollection.Create('Source row publication conflicts with prepared columns');
  end;
  ValidateDataset(AData, LParents);
end;

procedure TView.StoreChanged(const AStore: INyxCollection;
  const AChanges: INyxCollectionChanges);
begin

  if FAtomic = nil then
  begin
    ProjectChanges(AChanges);
    FInspectionStatus.DisplayRevision := AChanges.After.Revision;
    FInspectionStatus.IncomingRevision := FInspectionStatus.DisplayRevision;
  end;
  { Built-in stores already installed every prepared view before the first
    observer. Notifications perform target synchronization, never admission. }
  Notify(AChanges, FInspectionStatus.Mode = ncimPaused, ncicSourceChanged);
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

  if FInspectionTransition or (FQueryPreparation <> nil) or (FColumnPreparation <> nil) then
  begin
    raise ENyxCollection.Create('Source row publication conflicts with a prepared view command');
  end;
  RetireCandidate;
  LCandidate := TView.CreateValidator(FSpec, FProjection);
  try
    CopyProjectionTo(LCandidate);

    if FInspectionStatus.Mode = ncimPaused then
    begin
      PrepareInspectionUpdate(LCandidate, AData, AChanges);
    end
    else
    begin
      LCandidate.ProjectChanges(AChanges);
      PrepareDisplayContinuity(LCandidate, AData, AChanges);
      LCandidate.FInspectionStatus.DisplayRevision := AData.Revision;
      LCandidate.FInspectionStatus.IncomingRevision := AData.Revision;
    end;
    FPrepared := LCandidate;
  except
    LCandidate.Free;
    raise;
  end;
end;

procedure TView.InstallCandidate(const AData: INyxCollectionSnapshot;
  const AChanges: INyxCollectionChanges);
var
  LPreviousIndex: TNyxTextIndex;
begin
  { Private installation is a nonallocating adoption of immutable references
    and detached vectors. No user callback, query or selection command runs. }
  FInspectionStatus := FPrepared.FInspectionStatus;
  FDisplayContinuity := FPrepared.FDisplayContinuity;
  FInspectionRemoved := FPrepared.FInspectionRemoved;
  LPreviousIndex := FInspectionIndex;
  FInspectionIndex := FPrepared.FInspectionIndex;
  FPrepared.FInspectionIndex := LPreviousIndex;

  if FPrepared.FInspectionPendingOnly then
  begin
    RetireCandidate;
    Exit;
  end;
  RetireRowCopy;
  FSource := FPrepared.FSource;
  FSnapshot := FPrepared.FSnapshot;
  FUnpaged := FPrepared.FUnpaged;
  FPaging := FPrepared.FPaging;
  FPage := FPrepared.FPage;
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
begin
  ProjectSnapshot(AChanges.After, AChanges);
end;

procedure TView.ProjectSnapshot(const AData: INyxCollectionSnapshot;
  const AChanges: INyxCollectionChanges);
var
  LParents: TParentIndexes;
  LResultParents: TParentIndexes;
  LResult: INyxCollectionSnapshot;
  LSelection: INyxCollectionSelection;
  LExpanded: array of Boolean;
  LIndex: Integer;
  LPrevious: Integer;
  LUnpaged: INyxCollectionSnapshot;
  LWindow: TNyxCollectionPageWindow;
  LRemoved: TNyxTextIndex;
begin
  ValidateDataset(AData, LParents);
  LResult := NyxQuerySnapshot(AData, FQuery, LParents, LResultParents);
  LUnpaged := LResult;
  LWindow := Default(TNyxCollectionPageWindow);

  if FPaging.Defined then
  begin
    LWindow := NyxCollectionPageWindow(LResult.Count, FPaging.ItemsPerPage, FPage);
    LResult := NyxPageSnapshot(LResult, LWindow);
  end;
  { Final IDs alone cannot distinguish a surviving row from a replacement.
    Build a short-lived removal index during admission, never during install.
    Query/page-only projections keep the ordinary no-removal reconciliation. }
  LRemoved := nil;
  try

    if AChanges <> nil then
    begin
      for LIndex := 0 to AChanges.Count - 1 do
      begin

        if AChanges.Kind(LIndex) = nceRemove then
        begin

          if LRemoved = nil then
          begin
            LRemoved := TNyxTextIndex.Create;
          end;
          LRemoved.AddFirst(AChanges.ItemRef(LIndex).ID, LIndex);
        end;
      end;
    end;
    LSelection := ReconcileSelection(AData, LResult, FSnapshot, LRemoved);
  finally
    LRemoved.Free;
  end;
  LExpanded := nil;
  { Disclosure follows exact surviving source identity, including rows currently
    filtered out. A new item with a previously removed ID starts closed. }

  if FProjection = cpTree then
  begin
    SetLength(LExpanded, AData.Count);
    for LIndex := 0 to AData.Count - 1 do
    begin
      LPrevious := FSource.IndexOf(AData.ItemAt(LIndex).Ref);

      if LPrevious >= 0 then
      begin
        LExpanded[LIndex] := FExpanded[LPrevious];
      end;
    end;
    { A grouped remove/reinsert can keep the final ID while replacing its runtime
      instance. Honor the admitted operation log instead of resurrecting state. }
    if AChanges <> nil then
    begin
      for LIndex := 0 to AChanges.Count - 1 do
      begin

        if AChanges.Kind(LIndex) = nceRemove then
        begin
          LPrevious := AData.IndexOf(AChanges.ItemRef(LIndex));

          if LPrevious >= 0 then
          begin
            LExpanded[LPrevious] := False;
          end;
        end;
      end;
    end;
  end;
  RetireRowCopy;
  FSource := AData;
  FSnapshot := LResult;
  FUnpaged := LUnpaged;

  if FPaging.Defined then
  begin
    FPage := LWindow.Page;
  end;
  FParents := LResultParents;
  FSelection := LSelection;
  FExpanded := LExpanded;
  BuildHierarchy;
  ReconcileTreeFocus;
end;

function TView.ReconcileSelection(const ASource, AResult,
  APrevious: INyxCollectionSnapshot;
  ARemoved: TNyxTextIndex): INyxCollectionSelection;
type
  { Detached source position keeps retained selected identities canonically
    ordered without scanning every source row during scalar publications. }
  TSelectedRef = record
    Position: Integer;
    Item: TNyxItemRef;
  end;
  { Merge-sort scratch belongs only to this candidate selection admission. }
  TSelectedRefs = array of TSelectedRef;
var
  LItems: array of TNyxItemRef;
  LSelected: TSelectedRefs;
  LBuffer: TSelectedRefs;
  LSwap: TSelectedRefs;
  LItem: TNyxItemRef;
  LFocus: TNyxItemRef;
  LAnchor: TNyxItemRef;
  LIndex: Integer;
  LCount: Integer;
  LFocusIndex: Integer;
  LPosition: Integer;
  LWidth: Integer;
  LFirst: Integer;
  LMiddle: Integer;
  LAfter: Integer;
  LLeft: Integer;
  LRight: Integer;
  LOutput: Integer;
  LOrdered: Boolean;

  function Retired(const AItem: TNyxItemRef): Boolean;
  begin
    Result := AItem.Defined and (ARemoved <> nil) and
      (ARemoved.IndexOf(AItem.ID) >= 0);
  end;

begin
  { Membership is validated against the complete source; only actual removals
    prune it. Look up the selected identities instead of reading every source
    item, especially when an application has no selected members. Source moves
    can change their order: sort admitted positions in detached local vectors.
    This bounds reconciliation by membership, not the complete source size.
    Visibility still constrains the cursor independently of hidden members. }
  SetLength(LSelected, FSelection.Count);
  LCount := 0;
  LOrdered := True;
  for LIndex := 0 to FSelection.Count - 1 do
  begin
    LItem := FSelection.ItemAt(LIndex);
    LPosition := ASource.IndexOf(LItem);

    if (LPosition >= 0) and not Retired(LItem) then
    begin

      if (LCount > 0) and (LPosition < LSelected[LCount - 1].Position) then
      begin
        LOrdered := False;
      end;
      LSelected[LCount].Position := LPosition;
      LSelected[LCount].Item := LItem;
      Inc(LCount);
    end;
  end;
  SetLength(LSelected, LCount);
  { Scalar publications normally leave canonical membership ordered. Dense
    selection therefore stays linear; only actual source reordering needs merge. }

  if not LOrdered then
  begin
    SetLength(LBuffer, LCount);
  end;
  LWidth := 1;
  while not LOrdered and (LWidth < LCount) do
  begin
    LFirst := 0;
    while LFirst < LCount do
    begin
      LMiddle := LCount;

      if LWidth < LCount - LFirst then
      begin
        LMiddle := LFirst + LWidth;
      end;
      LAfter := LCount;

      if LWidth < LCount - LMiddle then
      begin
        LAfter := LMiddle + LWidth;
      end;
      LLeft := LFirst;
      LRight := LMiddle;
      for LOutput := LFirst to LAfter - 1 do
      begin

        if (LRight >= LAfter) or ((LLeft < LMiddle) and
          (LSelected[LLeft].Position <= LSelected[LRight].Position)) then
        begin
          LBuffer[LOutput] := LSelected[LLeft];
          Inc(LLeft);
        end
        else
        begin
          LBuffer[LOutput] := LSelected[LRight];
          Inc(LRight);
        end;
      end;
      LFirst := LAfter;
    end;
    { Swap exclusively owned scratch vectors. Neither aliases the immutable
      prior selection; explicit record copies also preserve pas2js ownership. }
    LSwap := LSelected;
    LSelected := LBuffer;
    LBuffer := LSwap;

    if LWidth > LCount div 2 then
    begin
      Break;
    end;
    LWidth := LWidth * 2;
  end;
  SetLength(LItems, LCount);
  for LIndex := 0 to LCount - 1 do
  begin
    LItems[LIndex] := LSelected[LIndex].Item;
  end;
  LFocus := FSelection.Focus;
  LAnchor := FSelection.Anchor;
  { A missing row retains the existing nearby-cursor fallback below. A reused
    ID instead clears its old focus/anchor, matching paused-view retirement;
    the replacement must acquire fresh explicit navigation authority. }

  if Retired(LFocus) and ASource.Has(LFocus) then
  begin
    LFocus := Default(TNyxItemRef);
  end;

  if Retired(LAnchor) and ASource.Has(LAnchor) then
  begin
    LAnchor := Default(TNyxItemRef);
  end;

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
var
  LLease: INyxCollectionView;
  LBusy: Boolean;
begin
  LLease := Self;
  Result := not FInspectionTransition and not FNotifying and (FPrepared = nil) and (FQueryPreparation = nil) and
    (FColumnPreparation = nil) and
    (FAtomic <> nil);

  if Result then
  begin
    LBusy := FAtomic.Busy;
    { Readiness is not a reservation. An alternate getter may acquire one;
      report the canonical state after it returns and retain Self throughout. }
    Result := not FInspectionTransition and not LBusy and not FNotifying and (FPrepared = nil) and
      (FQueryPreparation = nil) and (FColumnPreparation = nil);
  end;
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

function TView.CaptureQuery: INyxCollectionQueryCapture;
begin
  Result := TQueryCapture.Create(FQueryIdentity, FQueryIntent, FQuery);
end;

function TView.PrepareQuery(const ACapture: INyxCollectionQueryCapture;
  const AQuery: TNyxCollectionQuery): INyxPreparedPublication;
var
  LPreparation: TQueryPreparation;
begin
  { Initialize may inspect an alternate capture/QI implementation. Establish
    interface ownership first, including on pas2js and every refusal path. }
  LPreparation := TQueryPreparation.Create;
  Result := LPreparation;
  LPreparation.Initialize(Self, ACapture, AQuery);
end;

function TView.ConfigureQuery(const APolicy: TNyxCollectionQuery): INyxCollectionView;
var
  LPolicy: TNyxCollectionQuery;
  LParents: TParentIndexes;
  LResultParents: TParentIndexes;
  LResult: INyxCollectionSnapshot;
  LSelection: INyxCollectionSelection;
  LUnpaged: INyxCollectionSnapshot;
  LWindow: TNyxCollectionPageWindow;
  LPrepared: INyxPreparedPublication;
  LReadiness: INyxPublicationReadiness;
  LSession: TQuerySessionState;
  LSessionLease: IInterface;
begin
  Writable;
  Result := Self as INyxCollectionView;
  LPolicy := TNyxCollectionQuery.FromData(APolicy.ToData);

  if LPolicy.ToData.ToJSON = FQuery.ToData.ToJSON then
  begin
    Exit;
  end;

  if FQueryIntent = High(Integer) then
  begin
    raise ENyxCollection.Create('Collection query intent is exhausted');
  end;

  if FAtomic <> nil then
  begin
    LPrepared := PrepareQuery(CaptureQuery, LPolicy);
    try
      LPrepared.Validate;
      LReadiness := nil;

      if not Supports(LPrepared, INyxPublicationReadiness, LReadiness) then
      begin
        raise ENyxCollection.Create('Prepared query readiness is unavailable');
      end;
      LReadiness.CheckReady;
      LPrepared.Install;
      { Preserve ConfigureQuery's existing ENyxCollectionNotification behavior.
        Multi-model callers choose PublishNyxGroup and its committed exception. }
      LPrepared.Notify;
    finally
      LPrepared.Retire;
    end;
    Exit;
  end;
  { Legacy non-atomic stores retain their ordinary immediate query API. They
    deliberately cannot promise the optional coordinated reservation contract. }
  LSession := TQuerySessionState.Create;
  LSessionLease := LSession;
  LSession.Initialize(LPolicy);
  ValidateDataset(FSource, LParents);
  LResult := NyxQuerySnapshot(FSource, LPolicy, LParents, LResultParents);
  LUnpaged := LResult;
  LWindow := Default(TNyxCollectionPageWindow);

  if FPaging.Defined then
  begin
    LWindow := NyxCollectionPageWindow(LResult.Count, FPaging.ItemsPerPage, FPage);
    LResult := NyxPageSnapshot(LResult, LWindow);
  end;
  LSelection := ReconcileSelection(FSource, LResult, FSnapshot);
  RetireRowCopy;
  FQuery := LPolicy;
  FSnapshot := LResult;
  FUnpaged := LUnpaged;

  if FPaging.Defined then
  begin
    FPage := LWindow.Page;
  end;
  FParents := LResultParents;
  FSelection := LSelection;
  Inc(FQueryIntent);
  BuildHierarchy;
  ReconcileTreeFocus;
  FQuerySession.Scope.Active := False;
  FQuerySession := LSession;
  FQuerySessionLease := LSessionLease;
  FNextQueryOwner := 0;
  Notify(nil);
end;

function NyxCollectionPages(const AView: INyxCollectionView): INyxCollectionPages;
begin

  if (AView = nil) or not Supports(AView, INyxCollectionPages, Result) then
  begin
    raise ENyxCollection.Create('Collection view does not support typed paging');
  end;
end;

function TView.GetPaging: TNyxCollectionPaging;
begin
  Result := FPaging;
end;

function TView.GetPageWindow: TNyxCollectionPageWindow;
var
  LSize: Integer;
begin
  LSize := FUnpaged.Count;

  if LSize = 0 then
  begin
    LSize := 1;
  end;

  if FPaging.Defined then
  begin
    LSize := FPaging.ItemsPerPage;
  end;
  Result := NyxCollectionPageWindow(FUnpaged.Count, LSize, FPage);
end;

function TView.ConfigurePaging(const APolicy: TNyxCollectionPaging): INyxCollectionPages;
var
  LPolicy: TNyxCollectionPaging;
  LWindow: TNyxCollectionPageWindow;
  LResult: INyxCollectionSnapshot;
  LSelection: INyxCollectionSelection;
begin
  Writable;
  Result := Self as INyxCollectionPages;
  LPolicy := TNyxCollectionPaging.FromData(APolicy.ToData);

  if LPolicy.Defined and (FProjection = cpTree) then
  begin
    raise ENyxCollection.Create('Tree paging requires a hierarchy-aware policy');
  end;

  if LPolicy.ToData.ToJSON = FPaging.ToData.ToJSON then
  begin
    Exit;
  end;
  LResult := FUnpaged;
  LWindow := NyxCollectionPageWindow(FUnpaged.Count, Max(1, FUnpaged.Count), 1);

  if LPolicy.Defined then
  begin
    LWindow := NyxCollectionPageWindow(FUnpaged.Count, LPolicy.ItemsPerPage, LPolicy.FirstPage);
    LResult := NyxPageSnapshot(FUnpaged, LWindow);
  end;
  LSelection := ReconcileSelection(FSource, LResult, FSnapshot);
  RetireRowCopy;
  FPaging := LPolicy;
  FPage := LWindow.Page;
  FSnapshot := LResult;
  FSelection := LSelection;
  Notify(nil);
end;

function TView.GoToPage(APage: Integer): INyxCollectionPages;
var
  LWindow: TNyxCollectionPageWindow;
  LResult: INyxCollectionSnapshot;
  LSelection: INyxCollectionSelection;
begin
  Writable;
  Result := Self as INyxCollectionPages;

  if APage < 1 then
  begin
    raise ENyxCollection.Create('Pages are one-based');
  end;

  if not FPaging.Defined then
  begin

    if APage <> 1 then
    begin
      raise ENyxCollection.Create('Configure paging before choosing another page');
    end;
    Exit;
  end;
  LWindow := NyxCollectionPageWindow(FUnpaged.Count, FPaging.ItemsPerPage, APage);

  if LWindow.Page = FPage then
  begin
    Exit;
  end;
  LResult := NyxPageSnapshot(FUnpaged, LWindow);
  LSelection := ReconcileSelection(FSource, LResult, FSnapshot);
  RetireRowCopy;
  FPage := LWindow.Page;
  FSnapshot := LResult;
  FSelection := LSelection;
  Notify(nil);
end;

function TView.NextPage: INyxCollectionPages;
var
  LWindow: TNyxCollectionPageWindow;
begin
  Writable;
  LWindow := GetPageWindow;
  Result := Self as INyxCollectionPages;

  if LWindow.CanNext then
  begin
    Result := GoToPage(LWindow.Page + 1);
  end;
end;

function TView.PreviousPage: INyxCollectionPages;
var
  LWindow: TNyxCollectionPageWindow;
begin
  Writable;
  LWindow := GetPageWindow;
  Result := Self as INyxCollectionPages;

  if LWindow.CanPrevious then
  begin
    Result := GoToPage(LWindow.Page - 1);
  end;
end;

function TView.FirstPage: INyxCollectionPages;
begin
  Result := GoToPage(1);
end;

function TView.LastPage: INyxCollectionPages;
var
  LWindow: TNyxCollectionPageWindow;
begin
  LWindow := GetPageWindow;
  Result := GoToPage(Max(1, LWindow.Pages));
end;

function TExportCapture.GetSource: INyxCollectionSnapshot;
begin
  Result := Source;
end;

function TExportCapture.GetFiltered: INyxCollectionSnapshot;
begin
  Result := Filtered;
end;

function TExportCapture.GetPage: INyxCollectionSnapshot;
begin
  Result := Page;
end;

function TExportCapture.GetColumns: INyxCollectionColumnsCapture;
begin
  Result := Columns;
end;

function TExportCapture.ExportData: TExportCapture;
begin
  Result := Self;
end;

function TView.CaptureSelectedRows: INyxCollectionSelectedRowsCapture;
var
  LLease: INyxCollectionView;
  LCapture: TRowCopyCapture;
  LCaptureLease: INyxCollectionSelectedRowsCapture;
  LReady: INyxCancellationReadiness;
begin
  LLease := Self;
  Result := nil;

  if (FProjection <> cpTable) or FNotifying or (FPrepared <> nil) or
    (FQueryPreparation <> nil) or (FColumnPreparation <> nil) or (FAtomic = nil) then
  begin
    Exit;
  end;

  if FRowCopyReadinessObject = nil then
  begin
    FRowCopyReadinessObject := TRowCopyReadiness.Create;
    FRowCopyReadiness := FRowCopyReadinessObject;
  end;
  LReady := FRowCopyReadiness;
  LCapture := TRowCopyCapture.Create;
  LCaptureLease := LCapture;
  LCapture.Source := FSource;
  LCapture.Page := FSnapshot;
  LCapture.Selection := FSelection.Snapshot;
  LCapture.Columns := TColumnCapture.Create(FColumnIdentity, FColumnSerial,
    FSpec, FColumnPreferences, FPresentationPreferences);
  LCapture.Readiness := LReady;
  Result := LCaptureLease;
end;

function TView.CaptureExport: INyxCollectionExportCapture;
var
  LLease: INyxCollectionView;
  LCapture: TExportCapture;
begin
  { The nested final check enters an alternative source Busy getter. Its own
    lease ends on return, so this public operation owns its complete entry too. }
  LLease := Self;
  LCapture := TExportCapture.Create;
  Result := LCapture;
  LCapture.Identity := FColumnIdentity;
  LCapture.QueryIdentity := FQueryIdentity;
  LCapture.QueryIntent := FQueryIntent;
  LCapture.ColumnSerial := FColumnSerial;
  LCapture.ObservationSerial := FColumnSampleSerial;
  LCapture.Source := FSource;
  LCapture.Filtered := FUnpaged;
  LCapture.Page := FSnapshot;
  LCapture.Columns := TColumnCapture.Create(FColumnIdentity, FColumnSerial,
    FSpec, FColumnPreferences, FPresentationPreferences);
  { The atomic store Busy getter is outward. Keep original fields first, then
    require that exact cohort after readiness rather than recapture its result. }
  CheckExportCurrent(Result);
end;

procedure TView.CheckExportCurrent(const ACapture: INyxCollectionExportCapture);
var
  LLease: INyxCollectionView;
  LProof: IExportCaptureProof;
  LCapture: TExportCapture;
begin
  { PublicationReady protects only its own body. Retain this outer receiver
    through all original-field comparisons after an outward Busy getter. }
  LLease := Self;

  if (ACapture = nil) or not Supports(ACapture, IExportCaptureProof, LProof) then
  begin
    raise ENyxCollection.Create('Export capture does not belong to this table');
  end;
  LCapture := LProof.ExportData;

  if (FProjection <> cpTable) or not PublicationReady or (LCapture = nil) or
    (LCapture.Identity <> FColumnIdentity) or
    (LCapture.QueryIdentity <> FQueryIdentity) or
    (LCapture.QueryIntent <> FQueryIntent) or
    (LCapture.ColumnSerial <> FColumnSerial) or
    (LCapture.ObservationSerial <> FColumnSampleSerial) or
    (FColumnSerial >= High(Integer)) or (FQueryIntent >= High(Integer)) or
    (FColumnSampleSerial >= High(Integer)) or
    (LCapture.Source <> FSource) or (LCapture.Filtered <> FUnpaged) or
    (LCapture.Page <> FSnapshot) then
  begin
    raise ENyxCollection.Create('Original export source/query/page/columns changed');
  end;
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
  Result := ColumnItemText(LItem, LColumn);
end;

procedure TView.Select(const AItem: TNyxItemRef);
begin
  Select(AItem, nsaReplace);
end;

procedure TView.SetSelection(const AItems: array of TNyxItemRef;
  const AFocus, AAnchor: TNyxItemRef);
begin
  SetSelectionCore(AItems, AFocus, AAnchor, False, ntrfPreserve);
end;

procedure TView.SetSelectionCore(const AItems: array of TNyxItemRef;
  const AFocus, AAnchor: TNyxItemRef; AReveal: Boolean;
  ARevealFocus: TNyxTreeRevealFocus);
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

  if AReveal and (FTreeReveal.Serial = High(Integer)) then
  begin
    raise ENyxCollection.Create('Tree reveal request serial is exhausted');
  end;
  AdvanceTreeCommand;
  { Admission above must finish before disclosure changes. Explicit application
    focus opens ancestors; a later structural/collapse publication never does. }
  LRevealed := RevealAncestors(AFocus);

  if FSelection.SameState(LSelection) and not LRevealed and not AReveal then
  begin
    Exit;
  end;
  RetireRowCopy;
  FSelection := LSelection;

  if AReveal then
  begin
    Inc(FTreeReveal.Serial);
    FTreeReveal.Item := AFocus;
    FTreeReveal.Focus := ARevealFocus;
    FTreeRevealCommand := FTreeCommandSerial;
    { Selection is our canonical owned snapshot. Its copied revision avoids a
      provider getter after focus/disclosure/request installation has begun. }
    FTreeRevealRevision := LSelection.DataRevision;
    FTreeRevealQueryIntent := FQueryIntent;
  end;
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
  { Paging is a read projection, not a new selection universe. SelectAll keeps
    the documented complete filtered result while focus stays on this page. }
  SetLength(LItems, FUnpaged.Count);
  for LIndex := 0 to FUnpaged.Count - 1 do
  begin
    LItems[LIndex] := FUnpaged.ItemAt(LIndex).Ref;
  end;
  LFocus := FSelection.Focus;
  LAnchor := FSelection.Anchor;

  if (FSnapshot.Count > 0) and not LFocus.Defined then
  begin
    LFocus := FSnapshot.ItemAt(0).Ref;
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

  if FInspectionStatus.Mode = ncimPaused then
  begin
    raise ENyxCollectionInspection.Create('Resume this inspection before editing live data');
  end;
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

  if FInspectionStatus.Mode = ncimPaused then
  begin
    raise ENyxCollectionInspection.Create('Resume this inspection before editing live data');
  end;
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

procedure TView.Notify(const AChanges: INyxCollectionChanges;
  AInspectionOnly: Boolean; ACause: TNyxCollectionInspectionCause);
var
  LKeepAlive: INyxCollectionView;
  LSerials: array of Integer;
  LIndex: Integer;
  LCurrent: Integer;
  LObserver: TNyxCollectionViewObserver;
  LInspectionObserver: TNyxCollectionInspectionObserver;
  LInspection: TNyxCollectionInspectionObservation;
  LError: TNyxText;
  LHasError: Boolean;
begin
  { Snapshot serials rather than raw tokens: an earlier callback may disconnect
    and release a later receiver. The interface local protects our lifetime if
    a callback drops the last application's view reference. }
  LKeepAlive := Self as INyxCollectionView;

  if not AInspectionOnly then
  begin
    RetireRowCopy;

    if FColumnSampleSerial < High(Integer) then
    begin
      Inc(FColumnSampleSerial);
    end;
  end;
  LInspection := FInspectionStatus;
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
          LInspectionObserver := FTokens[LCurrent].InspectionObserver;
          try

            if Assigned(LInspectionObserver) then
            begin
              LInspectionObserver(LInspection, ACause);
            end
            else if not AInspectionOnly and Assigned(LObserver) then
            begin
              LObserver(LKeepAlive, AChanges);
            end;
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

{$i nyx.collections.inspection.implementation.inc}

end.
