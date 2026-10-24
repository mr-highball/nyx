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
unit nyx.collections.mount;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  SysUtils,
  nyx.text,
  nyx.state,
  nyx.editing,
  nyx.collections,
  nyx.collections.arrangement,
  nyx.collections.view,
  nyx.collections.selection,
  nyx.collections.refresh,
  nyx.typeahead,
  nyx.events,
  nyx.publication,
  nyx.collections.query.controls,
  nyx.collections.arrangement.controls,
  nyx.collections.arrangement.presenter,
  nyx.collections.clipboard.actions,
  nyx.collections.cell.actions,
  nyx.collections.clipboard,
  nyx.clipboard,
  nyx.scheduler,
  nyx.binding.types;

type
  { Construction intent, independent of authored Enabled/ReadOnly. Ordinary
    mounts start ready; an opted-in replacement table is staged before Activate
    and keeps physical defaults dormant through reversible renderer preview. }
  TNyxCollectionInputPhase = (ncipReady, ncipStaged);

  { One renderer generation's flags-only input gate. Its immutable ancestry and
    atomic native flags permit cancellation reads from workers; control methods
    remain UI-thread-only. An embedded renderer can
    accept its own controls before its outer candidate commits; its immutable
    parent gate keeps those controls dormant until every enclosing owner accepts.
    Implementations must perform no callbacks/widget/model work in these methods.
    Parent gates must come from NewNyxCollectionInputGate; immutable canonical
    ancestry is traversed iteratively without calling arbitrary parent getters.
    Retire is terminal; Install never revives a retired generation. }
  INyxCollectionInputGate = interface(IInterface)
    ['{BBE590B7-5571-4934-9EF2-5FCB683DD7F4}']
    function GetReady: Boolean;
    function GetCurrent: Boolean;
    function GetDepth: Integer;
    procedure Install;
    procedure Retire;
    property Ready: Boolean read GetReady;
    property Current: Boolean read GetCurrent;
    property Depth: Integer read GetDepth;
  end;

  { Independent revocable input facts, with no mount/model/widget backreference.
    Native hooks retain this readonly port rather than their owning mount, so
    ordinary interface release still disconnects the mount without a cycle.
    Current permits controlled staging import AFTER Activate. Ready additionally
    requires installation, no edit suspension and ready enclosing generations.
    Retired distinguishes terminal ownership from safe pre-Activate preparation;
    checking it alone never admits physical input or controlled model commands.
    Epoch changes before explicit edit suspension or an enabled-to-disabled
    transition. A captured query cannot revive when Ready becomes true again.
    Whole-refresh shielding preserves Epoch. Saturation requires a fresh query
    input owner; no generation may be captured at High(Integer). }
  INyxCollectionInputLifetime = interface(IInterface)
    ['{D9BA1214-152B-4B5F-B72E-D185284EA937}']
    function GetCurrent: Boolean;
    function GetReady: Boolean;
    function GetRetired: Boolean;
    function GetEpoch: Integer;
    property Current: Boolean read GetCurrent;
    property Ready: Boolean read GetReady;
    property Retired: Boolean read GetRetired;
    property Epoch: Integer read GetEpoch;
  end;

  { Optional accepted-input ownership preserves the original mount interface.
    The renderer retains this port BEFORE retiring its old view. InstallInput
    is a nonthrowing flags-only adoption step: no widget, getter, notification,
    query or presenter call follows it. A disconnected mount cannot reactivate.
    Direct Store/View APIs remain independent application-model authorities. }
  INyxCollectionInputAdmission = interface(IInterface)
    ['{51B2C90E-4085-4F21-B785-0323D5EEA283}']
    function GetInputLifetime: INyxCollectionInputLifetime;
    function GetInputReady: Boolean;
    procedure InstallInput;
    property InputReady: Boolean read GetInputReady;
  end;

  { An owned physical cell draft, independent of widgets and authored defaults.
    Item and typed field identify its exact accepted scalar. Text is the host's
    wire draft (including an escaped NUL representation), not a promised scalar
    conversion. Selection uses Nyx's Unicode scalar offsets on both hosts.
    A nonediting dirty cell can survive browser row-window eviction; only one
    cell per snapshot may own editing/focus. An untouched record is absent. }
  TNyxCollectionCellEdit = record
  private
    FDefined, FEditing, FFocused: Boolean;
    FItem: TNyxItemRef;
    FField: TNyxColumnIdentity;
    FAccepted: TNyxStateValue;
    FText: TNyxText;
    FSelection: TNyxTextSelection;
    function GetAccepted: TNyxStateValue;
  public
    function Copy: TNyxCollectionCellEdit;
    procedure Validate;
    property Defined: Boolean read FDefined;
    property Item: TNyxItemRef read FItem;
    property Field: TNyxColumnIdentity read FField;
    property Accepted: TNyxStateValue read GetAccepted;
    property Text: TNyxText read FText;
    property Selection: TNyxTextSelection read FSelection;
    property Editing: Boolean read FEditing;
    property Focused: Boolean read FFocused;
  end;
  TNyxCollectionCellEdits = array of TNyxCollectionCellEdit;

  { Immutable copied drafts from one exact runtime store. No document, mount,
    widget or callback is retained. Store is retained for identity comparison;
    it does not grant a replacement the former view's query/selection state.
    CellAt copies records on FPC and pas2js; no mutable array escapes. }
  INyxCollectionEditsSnapshot = interface(IInterface)
    ['{9B598CD4-5B38-49D5-9499-2611B0A85158}']
    function GetStore: INyxCollection;
    function GetCount: Integer;
    function CellAt(AIndex: Integer): TNyxCollectionCellEdit;
    property Store: INyxCollection read GetStore;
    property Count: Integer read GetCount;
  end;

  { Optional physical edit handoff keeps the original mount ABI intact. Suspend
    copies EVERY retained draft before hiding/blurring, then suppresses physical
    edit/focus defaults until Resume or Disconnect. Nested Suspend refuses.
    Resume requires the exact returned snapshot as its single-use suspension
    lease; it cannot release a newer suspension. Restore copies compatible data
    into a replacement without EditCell, Select or query changes, checking exact
    store, current visible item, typed field, edit policy and accepted scalar.
    AFocus is permission from the renderer's physical-preview/rollback boundary,
    not an instruction to restore focus from unrelated asynchronous work. }
  INyxCollectionEditMount = interface(IInterface)
    ['{32EB1557-930E-4E0C-8BE3-0BDE38A2AF91}']
    function SuspendCellEdits: INyxCollectionEditsSnapshot;
    procedure ResumeCellEdits(const ASnapshot: INyxCollectionEditsSnapshot;
      AFocus: Boolean);
    { True only when the exact imported cell still owns actual focus AFTER all
      target callbacks. Renderers may remember that precise fallback for rollback;
      another field in the same view never grants cell-focus authority. }
    function RestoreCellEdits(const ASnapshot: INyxCollectionEditsSnapshot;
      AFocus: Boolean): Boolean;
    { A fresh, host-local proof for rollback AFTER later callbacks. Physical
      editor identity alone is insufficient: a native editor may now represent
      another cell. No store/view/snapshot getter or mutation is called here.
      A model notification or rebuilt map invalidates the cached proof. }
    function CellEditFocusMatches(const AStore: INyxCollection;
      const ACell: TNyxCollectionCellEdit): Boolean;
  end;

  { A renderer borrows the method receiver only until Disconnect. Snapshots own
    identities; callers may retain them after navigation or view destruction. }
  TNyxCollectionSelectionObserver = procedure(
    const ABefore, AAfter: INyxCollectionSelection) of object;
  { Managed renderer attachment. The renderer owns and disconnects it before
    its controls are freed; retained interfaces then report Connected=False.
    View is available while connected. Errors distinguish rejected commands from
    already-published observer failures. RefreshCount counts complete adapter
    synchronizations, not painted frames or a virtualization guarantee. }
  INyxCollectionMount = interface
    ['{C6197F04-2D12-414B-A404-067E2C626386}']
    procedure Disconnect;
    procedure Refresh;
    { Controls enforce effective ancestor policy as well as column editability.
      Read-only allows selection. Disabled controls reject selection and edits. }
    procedure SetInteraction(AEnabled, AReadOnly: Boolean);
    { Transient list/tree keyboard policy. Replaces the control's independent
      search after validation; no document, store or history is changed. Tables
      retain their cell-editor defaults. Disconnected controls refuse. }
    procedure ConfigureTypeAhead(const AOptions: TNyxTypeAheadOptions);
    procedure ObserveSelection(AObserver: TNyxCollectionSelectionObserver);
    function Select(const AItem: TNyxItemRef): Boolean; overload;
    function Select(const AItem: TNyxItemRef; AAction: TNyxSelectionAction): Boolean; overload;
    function SetSelection(const AItems: array of TNyxItemRef;
      const AFocus, AAnchor: TNyxItemRef): Boolean;
    function SelectAll: Boolean;
    function SelectRange(const AItem: TNyxItemRef;
      const AVisibleOrder: array of TNyxItemRef; AAdd: Boolean): Boolean;
    function EditCell(const AItem: TNyxItemRef; AColumn: Integer;
      const AWireValue: TNyxText): Boolean;
    function GetConnected: Boolean;
    function GetView: INyxCollectionView;
    function GetError: TNyxText;
    function GetFailure: TNyxBindingFailure;
    function GetRefreshCount: Integer;
    property Connected: Boolean read GetConnected;
    property View: INyxCollectionView read GetView;
    property ErrorText: TNyxText read GetError;
    property Failure: TNyxBindingFailure read GetFailure;
    property RefreshCount: Integer read GetRefreshCount;
  end;

  { Optional table affordances leave the original mount GUID/vtable intact.
    Hosts supply their exact accepted event router and a weak target presenter.
    Literal Items controls have no bound mount and never acquire this surface. }
  INyxCollectionQueryMount = interface(IInterface)
    ['{96760DF7-7CB0-46B7-9259-32C886E6CC03}']
    procedure ConfigureQueries(const APresenters: INyxQueryFormPresenterFactory;
      const AEvents: INyxEvents; const ATarget: TNyxEventTarget;
      const ADesignID: TNyxText = '');
    function GetQueryWorkbench: INyxCollectionQueryWorkbench;
    property QueryWorkbench: INyxCollectionQueryWorkbench read GetQueryWorkbench;
  end;

  { Optional exact-input context for an arranged table's existing form factory.
    Configure on the UI thread before constructing a form. Retain only these
    owner-free readonly facts, never the mount, view or physical table. Every
    resulting form decorates its actual host router with this input and the
    actual successful-open admission. Direct/custom hosts must implement this
    capability for arrangement-enabled queries; ordinary query factories keep
    their existing interface, identity and nonarranged behavior. }
  INyxCollectionQueryInputFactory = interface(IInterface)
    ['{765E3254-669C-4F11-AFB3-D7B7C7B93E50}']
    procedure ConfigureInput(const AInput: INyxCollectionInputLifetime);
  end;

  { Optional per-table column management. The mount owns and revokes the exact
    controller/factory before target teardown; no second preference store is
    introduced. Configuration is once per connected mount and uses its original
    input lifetime. A retained manager cannot act after that lifetime retires. }
  INyxCollectionColumnArrangementMount = interface(IInterface)
    ['{BE6DB0EB-E6B0-4B98-83F6-506C278313B5}']
    procedure ConfigureColumnArrangement(
      const APresenters: INyxColumnArrangementPresenterFactory;
      const AEvents: INyxEvents);
    function GetColumnArrangement: INyxColumnArrangementManager;
    property ColumnArrangement: INyxColumnArrangementManager read GetColumnArrangement;
  end;

  { Adapter base only. Derived classes borrow target controls, implement in-place
    synchronization and detach their target callbacks. No target types enter the
    portable interfaces. Activate runs after concrete handles are initialized
    and the caller retains an interface owner; target factories do this before
    activating. Active commands retain their own call frames through callbacks.
    UI-thread confinement is inherited from the view/store contract. }
  TNyxCollectionMountBase = class(TInterfacedObject, INyxCollectionMount,
    INyxCollectionQueryMount, INyxCollectionInputAdmission,
    INyxCollectionColumnArrangementMount, INyxCollectionClipboardMount,
    INyxCollectionCellActionsMount)
  private
    FToken: INyxCollectionViewSubscription;
    FConnected: Boolean;
    FDisconnecting: Boolean;
    FQueryFactory: INyxQueryFormPresenterFactory;
    { Construction admission is independent of the eventual workbench slot.
      Alternative getters/factories cannot recursively install another owner. }
    FConfiguringQueries: Boolean;
    { Configuration cannot recursively adopt an alternative manager from a
      custom factory/getter before the original construction finishes. }
    FConfiguringColumns: Boolean;
    { A host/scheduler callback cannot install another clipboard controller
      while the exact original runtime authority is being configured. }
    FConfiguringClipboard: Boolean;
    { A candidate context presenter cannot recursively replace the exact
      factory/controller being configured across scheduler admission. }
    FConfiguringCellActions: Boolean;
    FCellFactory: INyxCollectionCellPresenterFactory;
    FColumnFactory: INyxColumnArrangementPresenterFactory;
    FError: TNyxText;
    FFailure: TNyxBindingFailure;
    FRefreshCount: Integer;
    FSelection: INyxCollectionSelection;
    FSelectionObserver: TNyxCollectionSelectionObserver;
    FTypeAhead: INyxTypeAhead;
    FTypeAheadSnapshot: INyxCollectionSnapshot;
    FSearchOrder: TNyxItemRefs;
    FLastSnapshot: INyxCollectionSnapshot;
    FChanges: INyxCollectionChanges;
    { The interface owns the known-private state. Its borrowed TObject address
      is used only for flags-only control; no QI/callback occurs at adoption. }
    FInputLifetime: INyxCollectionInputLifetime;
    FInputStateObject: TObject;
    function SearchLabel(AIndex: Integer): TNyxText;
    procedure DisconnectInternal;
    procedure Changed(const AView: INyxCollectionView;
      const AChanges: INyxCollectionChanges);
  protected
    FView: INyxCollectionView;
    FUpdating: Boolean;
    FInputGate: INyxCollectionInputGate;
    { A full-render edit handoff pauses this adapter's physical refresh and
      input defaults without holding FUpdating across callbacks. Resume runs a
      fresh comparison against the last ACTUALLY rendered source snapshot. }
    FEditsSuspended: Boolean;
    { Flags-only invalidation precedes all outward model/host callbacks. Keep
      the copied proof inert until disconnect so invalidation cannot release
      a caller-owned store from inside a notification or focus predicate. }
    FCellFocusValid: Boolean;
    FCellFocusStore: INyxCollection;
    FCellFocusDraft: TNyxCollectionCellEdit;
    { Notifications advance even when a physical refresh is suppressed. A
      saturating serial never wraps back to an older import's identity; reaching
      its limit refuses further structural edit handoffs until a fresh mount. }
    FCellMutationSerial: Integer;
    FEnabled: Boolean;
    FReadOnly: Boolean;
    FQueries: INyxCollectionQueryWorkbench;
    FColumnManager: INyxColumnArrangementManager;
    { Controller owns model/runtime authority only. Per-command synchronous
      fences may retain this mount; asynchronous host scopes never do. }
    FClipboardCommands: INyxCollectionClipboardCommands;
    FCellActions: INyxCollectionCellActions;
    FClipboardFields: TNyxCollectionClipboardFields;
    { Cached canonical presentation, independently of row/query snapshots.
      Legacy descriptors keep their old geometry; only explicit layout choices
      acquire the optional view facet. Targets never query it while painting. }
    FHasColumnLayout: Boolean;
    FColumnLayout: TNyxColumnLayoutObservation;
    { Cached during synchronization, never query a source/controller from the
      native paint reader. Header zero retains its existing data-row ABI. }
    FQueryHeaders: array of TNyxText;
    FQuerySummary: TNyxText;
    { Valid only during RenderDataset. Dataset notifications carry their log;
      explicit policy/selection refreshes still synchronize target state. }
    FRefreshPlan: TNyxCollectionRefreshPlan;
    { A target can discover composition after an outward source getter. This
      per-call marker prevents an early target return being recorded as a
      completed physical refresh. Deferred work compares the old real snapshot. }
    FTargetRefreshDeferred: Boolean;
    { A completed target edit restores only its own item/column. Selection and
      unrelated publications preserve other live drafts. No-op/rejected edits
      still normalize their admitted display without a data revision. These
      borrowed command values are cleared before leaving the retained frame. }
    FNormalizeValues: Boolean;
    FNormalizeItem: TNyxItemRef;
    FNormalizeColumn: Integer;
    function PrepareClipboardInput: INyxCancellationReadiness;
    function ClipboardField(ACanonical: Integer): TNyxColumnIdentity;
    { Cached target flags only: no source, router, widget, DOM or application
      callback may run from a final clipboard readiness check. }
    function ClipboardInputAvailable: Boolean; virtual;
    { Synchronous target-only focus shielding. Its returned scope may retain
      this mount only through one menu/popover transition, never its session. }
    function BeginCellPresentation: INyxCollectionCellPresentation; virtual;
    { Current permits staging Sync/import while rejecting an ancestor retired
      during an outward callback. Ready additionally admits physical defaults. }
    function InputLifetimeCurrent: Boolean;
    procedure SetCellEditsSuspended(AValue: Boolean);
    function MatchesCellFocusProof(const AStore: INyxCollection;
      const ACell: TNyxCollectionCellEdit): Boolean;
    function DisplayColumnCount: Integer;
    function DisplayColumn(AIndex: Integer): Integer;
    function ColumnDisplayIndex(ACanonical: Integer): Integer;
    function ColumnVisible(ACanonical: Integer): Boolean;
    { Internal management seam, separate from the public mount ABI. Supply the
      ORIGINAL launch capture and full typed field. A target measures accepted
      sample text and returns one owned model+input preparation without publishing
      or changing any editor. The caller validates external generation/form
      guards BEFORE this group, so target metric validation is the last known
      outward validator. Later arbitrary direct style changes are not an atomic
      typography-at-install guarantee. ReadOnly data need not prevent layout. }
    function PrepareColumnFit(const ALaunch: INyxCollectionColumnsCapture;
      const AField: TNyxColumnIdentity;
      var AMeasurement: INyxColumnFitMeasurement): INyxPreparedPublication; virtual;
    procedure UpdateQueryHeaders;
    function QueryHeader(AColumn: Integer): TNyxText;
    { Target wire decoders report rejection through the same diagnostic phase
      contract as commands, before restoring their accepted widget display. }
    procedure Failed(AException: Exception);
    { Borrow visible identities only during the pure search; the reader cannot
      publish notifications. Dataset changes reset prefix, selection alone does
      not. Target handlers retain their mount before selection publication. }
    function FindTypeAhead(const ACharacter: TNyxText; ATimeMS: Double;
      const AOrder: TNyxItemRefs; AFocus: Integer): Integer;
    procedure ResetTypeAhead;
    { Target readiness precedes source/layout/header reads and is rechecked
      before physical synchronization. An override retains any outward-read
      owner and checks its known local flags LAST. Deferral marks the per-call
      flag and arranges one later platform wake. Refresh already owns FUpdating
      so an outward readiness getter cannot recursively rebuild this target. }
    function DeferTargetRefresh: Boolean; virtual;
    { Cached flags ONLY, before the refresh input shield. An already-composing
      editor must keep receiving platform END and committed characters. Targets
      mark deferred work here without consulting any router/source/widget. }
    function DeferComposingRefresh: Boolean; virtual;
    { Ordinary target wake only, LAST after Refresh has cleared its update/input
      shields. A native target may arm its existing idle handler here, including
      an early composition-deferred return. This is not a final-ready fence. }
    procedure RefreshTargetIdle; virtual;
    { Flags/weak-receiver retirement only, before query/observer cleanup can
      enter external code. Actual target API/hook cleanup remains DetachTarget. }
    procedure RetireTargetInput; virtual;
    procedure RenderDataset; virtual; abstract;
    procedure DetachTarget; virtual; abstract;
  public
    constructor Create(const AView: INyxCollectionView;
      AInputPhase: TNyxCollectionInputPhase = ncipReady;
      const AInputGate: INyxCollectionInputGate = nil);
    destructor Destroy; override;
    procedure Activate;
    function GetInputLifetime: INyxCollectionInputLifetime;
    function GetInputReady: Boolean;
    procedure InstallInput;
    procedure Disconnect;
    procedure Refresh;
    procedure SetInteraction(AEnabled, AReadOnly: Boolean);
    procedure ConfigureTypeAhead(const AOptions: TNyxTypeAheadOptions);
    procedure ConfigureQueries(const APresenters: INyxQueryFormPresenterFactory;
      const AEvents: INyxEvents; const ATarget: TNyxEventTarget;
      const ADesignID: TNyxText = '');
    function GetQueryWorkbench: INyxCollectionQueryWorkbench;
    procedure ConfigureColumnArrangement(
      const APresenters: INyxColumnArrangementPresenterFactory;
      const AEvents: INyxEvents);
    function GetColumnArrangement: INyxColumnArrangementManager;
    procedure ConfigureClipboard(const AHost: INyxClipboardWriteHost;
      const AEvents: INyxEvents; const AScheduler: INyxScheduler;
      const AOptions: TNyxCollectionClipboardOptions);
    function GetClipboardCommands: INyxCollectionClipboardCommands;
    procedure ConfigureCellActions(const AFactory: INyxCollectionCellPresenterFactory;
      const AScheduler: INyxScheduler);
    function GetCellActions: INyxCollectionCellActions;
    procedure ObserveSelection(AObserver: TNyxCollectionSelectionObserver);
    function Select(const AItem: TNyxItemRef): Boolean; overload;
    function Select(const AItem: TNyxItemRef; AAction: TNyxSelectionAction): Boolean; overload;
    function SetSelection(const AItems: array of TNyxItemRef;
      const AFocus, AAnchor: TNyxItemRef): Boolean;
    function SelectAll: Boolean;
    function SelectRange(const AItem: TNyxItemRef;
      const AVisibleOrder: array of TNyxItemRef; AAdd: Boolean): Boolean;
    function EditCell(const AItem: TNyxItemRef; AColumn: Integer;
      const AWireValue: TNyxText): Boolean;
    function GetConnected: Boolean;
    function GetView: INyxCollectionView;
    function GetError: TNyxText;
    function GetFailure: TNyxBindingFailure;
    function GetRefreshCount: Integer;
  end;

{ Checked value constructors. Snapshot copies the complete supplied vector,
  validates collection key/duplicates and the single editing/focus owner, and
  never truncates unfinished user work. Unsupported caret capture is undefined
  rather than inventing a UTF-8/UTF-16 offset conversion. }
function NyxCollectionCellEdit(const AItem: TNyxItemRef;
  const AField: TNyxColumnIdentity; const AAccepted: TNyxStateValue;
  const AText: TNyxText; const ASelection: TNyxTextSelection;
  AEditing, AFocused: Boolean): TNyxCollectionCellEdit;
function NewNyxCollectionEditsSnapshot(const AStore: INyxCollection;
  const ACells: array of TNyxCollectionCellEdit): INyxCollectionEditsSnapshot;
function NewNyxCollectionInputGate(
  const AParent: INyxCollectionInputGate = nil): INyxCollectionInputGate;
{ A callback-free final fence for an actual prepared UI command. Entry checks
  alone cannot stop a validator retiring its source gate before installation. }
function PrepareNyxCollectionInput(
  const AInput: INyxCollectionInputAdmission): INyxPreparedPublication;

implementation

uses
  nyx.text.index,
  nyx.collections.view.types, nyx.collections.query,
  nyx.collections.query.session,
  nyx.collections.query.workbench,
  nyx.collections.arrangement.workbench, nyx.collections.arrangement.manager,
  nyx.collections.clipboard.commands
  {$ifdef PAS2JS}, JS{$endif};

type
  { Disconnect records a failure separately from its possibly empty message.
    Browser cleanup may throw any JS value; retain its exact identity while
    every independent target/controller/subscription duty is still attempted. }
  TMountCleanupFailure = record
    Captured: Boolean;
    Message: TNyxText;
    {$ifdef PAS2JS}
    Value: JSValue;
    IsRaw: Boolean;
    {$endif}
  end;

  { Only synchronous preparation/dispatch owns this mount-retaining fence.
    The independent asynchronous scope retains canonical input/parent flags,
    never this object. Mutation is monotonic even across equal-value ABA. }
  TClipboardInputFence = class(TInterfacedObject, INyxCancellationReadiness)
  private
    FOwner: TNyxCollectionMountBase;
    FOwnerLease: INyxCollectionMount;
    FView: INyxCollectionView;
    FInput: INyxCollectionInputLifetime;
    FEpoch: Integer;
    FMutation: Integer;
  public
    constructor Create(AOwner: TNyxCollectionMountBase);
    procedure CheckReady;
  end;

  { Private control authority remains with the mount. Readers own only this
    object's readonly interface, which stays revoked after its mount is gone. }
  TCollectionInputState = class(TInterfacedObject, INyxCollectionInputLifetime)
  private
    FGate: INyxCollectionInputGate;
    FConnected, FInstalled, FSuspended, FRefreshing, FEnabled, FRetired: LongInt;
    FEpoch: LongInt;
    procedure AdvanceEpoch;
  public
    constructor Create(AInstalled: Boolean; const AGate: INyxCollectionInputGate);
    procedure Connect;
    procedure Install;
    procedure Suspend(AValue: Boolean);
    procedure Refreshing(AValue: Boolean);
    procedure InteractionEnabled(AValue: Boolean);
    procedure Retire;
    function GetCurrent: Boolean;
    function GetReady: Boolean;
    function GetRetired: Boolean;
    function GetEpoch: Integer;
  end;

  TCollectionInputGate = class;
  { Private canonical link is resolved once during construction, never during
    input/readiness/adoption. The retained parent interface owns this borrowed
    node. External parent implementations cannot inject callbacks into reads. }
  ICanonicalInputGate = interface(IInterface)
    ['{702091F2-7AC0-427F-B6F4-9839E52F107A}']
    function GetNode: TCollectionInputGate;
  end;

  { No model install/notification: this participant joins the same publication
    group solely to recheck admitted input AFTER every outward validator. }
  TCollectionInputFence = class(TInterfacedObject, INyxPreparedPublication,
    INyxPublicationReadiness)
  private
    FInput: INyxCollectionInputAdmission;
    FValidated, FRetired: Boolean;
  public
    constructor Create(const AInput: INyxCollectionInputAdmission);
    procedure Validate;
    procedure CheckReady;
    procedure Install;
    procedure Notify;
    procedure Retire;
  end;

  { No backreference to a renderer, mount or widget. A parent is retained only
    for readiness; children cannot install or retire that enclosing generation. }
  TCollectionInputGate = class(TInterfacedObject, INyxCollectionInputGate,
    ICanonicalInputGate)
  private
    FParent: INyxCollectionInputGate;
    FParentNode: TCollectionInputGate;
    FInstalled, FRetired: LongInt;
    FDepth: Integer;
  public
    constructor Create(const AParent: INyxCollectionInputGate);
    function GetReady: Boolean;
    function GetCurrent: Boolean;
    function GetDepth: Integer;
    function GetNode: TCollectionInputGate;
    procedure Install;
    procedure Retire;
  end;

  { Value ownership only. An exact-text index is temporary constructor work,
    so duplicate checking remains linear for many retained browser drafts. }
  TCollectionEditsSnapshot = class(TInterfacedObject, INyxCollectionEditsSnapshot)
  private
    FStore: INyxCollection;
    FCells: TNyxCollectionCellEdits;
  public
    constructor Create(const AStore: INyxCollection;
      const ACells: array of TNyxCollectionCellEdit);
    function GetStore: INyxCollection;
    function GetCount: Integer;
    function CellAt(AIndex: Integer): TNyxCollectionCellEdit;
  end;

procedure CaptureMountCleanup(var AFailure: TMountCleanupFailure;
  {$ifdef PAS2JS}AValue: JSValue{$else}const AMessage: TNyxText{$endif});
begin

  if AFailure.Captured then
  begin
    Exit;
  end;
  AFailure.Captured := True;
  {$ifdef PAS2JS}
  AFailure.Value := AValue;
  AFailure.IsRaw := not (isClassInstance(AValue) and (TObject(AValue) is Exception));

  if not AFailure.IsRaw then
  begin
    AFailure.Message := TNyxText(Exception(AValue).Message);
  end;
  {$else}
  AFailure.Message := AMessage;
  {$endif}
end;

procedure RaiseMountCleanup(const AFailure: TMountCleanupFailure);
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
    { Exception(value) emits rtl.getObject and coerces primitive throws. Keep
      this tiny adapter bridge for exact undefined/null/platform failures. }
    LValue := AFailure.Value;
    asm
      throw LValue;
    end;
  end;
  {$endif}
  raise ENyxCollection.Create(AFailure.Message);
end;

constructor TClipboardInputFence.Create(AOwner: TNyxCollectionMountBase);
begin
  inherited Create;
  FOwner := AOwner;
  FOwnerLease := AOwner as INyxCollectionMount;
  FView := AOwner.FView;
  FInput := AOwner.FInputLifetime;
  FEpoch := FInput.Epoch;
  FMutation := AOwner.FCellMutationSerial;
end;

procedure TClipboardInputFence.CheckReady;
begin

  if not FOwner.FConnected or FOwner.FDisconnecting or
    not FOwner.FEnabled or FOwner.FUpdating or FOwner.FEditsSuspended or
    (FOwner.FView <> FView) or (FOwner.FInputLifetime <> FInput) or
    (FMutation = High(Integer)) or (FOwner.FCellMutationSerial <> FMutation) or
    not FOwner.ClipboardInputAvailable then
  begin
    raise ENyxCollectionClipboard.Create('Original mounted collection copy is no longer ready');
  end;
  { The target contributes cached facts only. Canonical flags are LAST. }

  if (FEpoch <= 0) or (FEpoch >= High(Integer)) or
    not FInput.Ready or (FInput.Epoch <> FEpoch) then
  begin
    raise ENyxCollectionClipboard.Create('Original collection copy input epoch changed');
  end;
end;

function ReadInputFlag(var AValue: LongInt): Boolean;
begin
  {$ifdef PAS2JS}
  Result := AValue <> 0;
  {$else}
  Result := InterlockedCompareExchange(AValue, 0, 0) <> 0;
  {$endif}
end;

procedure WriteInputFlag(var AValue: LongInt; AEnabled: Boolean);
begin
  {$ifdef PAS2JS}
  AValue := Ord(AEnabled);
  {$else}
  InterlockedExchange(AValue, Ord(AEnabled));
  {$endif}
end;

constructor TCollectionInputState.Create(AInstalled: Boolean;
  const AGate: INyxCollectionInputGate);
begin
  inherited Create;
  FGate := AGate;
  FEpoch := 1;
  WriteInputFlag(FEnabled, True);
  WriteInputFlag(FInstalled, AInstalled);
end;

procedure TCollectionInputState.Connect;
begin

  if not ReadInputFlag(FRetired) then
  begin
    WriteInputFlag(FConnected, True);
  end;
end;

procedure TCollectionInputState.Install;
begin

  if ReadInputFlag(FConnected) and not ReadInputFlag(FRetired) then
  begin
    WriteInputFlag(FInstalled, True);
  end;
end;

procedure TCollectionInputState.Suspend(AValue: Boolean);
begin

  if AValue and not ReadInputFlag(FSuspended) then
  begin
    AdvanceEpoch;
  end;
  WriteInputFlag(FSuspended, AValue);
end;

procedure TCollectionInputState.AdvanceEpoch;
var
  LFormer: Integer;
begin
  { The mount's UI owner is the only writer. Native cancellation readers use
    atomic loads; increment before changing availability or calling outward. }
  LFormer := GetEpoch;

  if LFormer < High(Integer) then
  begin
    {$ifdef PAS2JS}
    FEpoch := LFormer + 1;
    {$else}
    InterlockedExchange(FEpoch, LFormer + 1);
    {$endif}
  end;
end;

procedure TCollectionInputState.InteractionEnabled(AValue: Boolean);
begin

  if not AValue and ReadInputFlag(FEnabled) then
  begin
    AdvanceEpoch;
  end;
  WriteInputFlag(FEnabled, AValue);
end;

procedure TCollectionInputState.Refreshing(AValue: Boolean);
begin
  WriteInputFlag(FRefreshing, AValue);
end;

procedure TCollectionInputState.Retire;
begin
  WriteInputFlag(FRetired, True);
end;

function TCollectionInputState.GetRetired: Boolean;
begin
  Result := ReadInputFlag(FRetired) or ((FGate <> nil) and not FGate.Current);
end;

function TCollectionInputState.GetCurrent: Boolean;
begin
  Result := ReadInputFlag(FConnected) and not GetRetired;
end;

function TCollectionInputState.GetEpoch: Integer;
begin
  {$ifdef PAS2JS}
  Result := FEpoch;
  {$else}
  Result := InterlockedCompareExchange(FEpoch, 0, 0);
  {$endif}
end;

function TCollectionInputState.GetReady: Boolean;
begin
  Result := GetCurrent and ReadInputFlag(FInstalled) and
    ReadInputFlag(FEnabled) and not ReadInputFlag(FSuspended) and
    not ReadInputFlag(FRefreshing);

  if Result and (FGate <> nil) then
  begin
    Result := FGate.Ready;
  end;
end;

constructor TCollectionInputFence.Create(const AInput: INyxCollectionInputAdmission);
begin
  inherited Create;
  FInput := AInput;

  if FInput = nil then
  begin
    raise ENyxCollection.Create('Input readiness requires the exact mounted owner');
  end;
end;

procedure TCollectionInputFence.Validate;
begin

  if FValidated or FRetired then
  begin
    raise ENyxCollection.Create('Input readiness preparation is no longer fresh');
  end;
  FValidated := True;
  CheckReady;
end;

procedure TCollectionInputFence.CheckReady;
begin

  if FRetired or not FValidated or not FInput.InputReady then
  begin
    raise ENyxCollection.Create('Collection input retired before its command was installed');
  end;
end;

procedure TCollectionInputFence.Install;
begin
  { The owning model installs its own preallocated values. }
end;

procedure TCollectionInputFence.Notify;
begin
  { The owning model notifies its own observers. }
end;

procedure TCollectionInputFence.Retire;
begin
  FRetired := True;
end;

function PrepareNyxCollectionInput(
  const AInput: INyxCollectionInputAdmission): INyxPreparedPublication;
begin
  Result := TCollectionInputFence.Create(AInput);
end;

constructor TCollectionInputGate.Create(const AParent: INyxCollectionInputGate);
var
  LCanonical: ICanonicalInputGate;
  LIdentity: INyxCollectionInputGate;
begin
  inherited Create;
  FParent := AParent;
  FDepth := 1;

  if FParent <> nil then
  begin

    if not Supports(FParent, ICanonicalInputGate, LCanonical) then
    begin
      raise ENyxCollection.Create('Collection input ancestry requires factory-created parent gates');
    end;
    FParentNode := LCanonical.GetNode;

    if (FParentNode = nil) or (FParentNode.FDepth < 1) or
      (FParentNode.FDepth = High(Integer)) then
    begin
      raise ENyxCollection.Create('Collection input gate ancestry is invalid or exhausted');
    end;
    LIdentity := FParentNode as INyxCollectionInputGate;

    if LIdentity <> FParent then
    begin
      raise ENyxCollection.Create('Collection input gate parent is not its canonical owner');
    end;
    FDepth := FParentNode.FDepth;
    Inc(FDepth);
  end;
end;

function TCollectionInputGate.GetReady: Boolean;
var
  LNode: TCollectionInputGate;
begin
  Result := False;
  LNode := Self;

  while LNode <> nil do
  begin

    if not ReadInputFlag(LNode.FInstalled) or ReadInputFlag(LNode.FRetired) then
    begin
      Exit;
    end;
    LNode := LNode.FParentNode;
  end;
  Result := True;
end;

function TCollectionInputGate.GetCurrent: Boolean;
var
  LNode: TCollectionInputGate;
begin
  Result := False;
  LNode := Self;

  while LNode <> nil do
  begin

    if ReadInputFlag(LNode.FRetired) then
    begin
      Exit;
    end;
    LNode := LNode.FParentNode;
  end;
  Result := True;
end;

function TCollectionInputGate.GetDepth: Integer;
begin
  Result := FDepth;
end;

function TCollectionInputGate.GetNode: TCollectionInputGate;
begin
  Result := Self;
end;

procedure TCollectionInputGate.Install;
begin

  if not ReadInputFlag(FRetired) then
  begin
    WriteInputFlag(FInstalled, True);
  end;
end;

procedure TCollectionInputGate.Retire;
begin
  WriteInputFlag(FRetired, True);
end;

function NewNyxCollectionInputGate(
  const AParent: INyxCollectionInputGate): INyxCollectionInputGate;
begin
  Result := TCollectionInputGate.Create(AParent);
end;

function TNyxCollectionCellEdit.GetAccepted: TNyxStateValue;
begin
  Result := FAccepted.Copy;
end;

function TNyxCollectionCellEdit.Copy: TNyxCollectionCellEdit;
begin
  Result := Self;
  Result.FAccepted := FAccepted.Copy;
end;

procedure TNyxCollectionCellEdit.Validate;
var
  LSelection: TNyxTextSelection;
begin

  if not FDefined or not FItem.Defined then
  begin
    raise ENyxCollection.Create('Cell edit requires a defined item and draft');
  end;
  FField.Validate;
  FAccepted.Validate;

  if (FAccepted.Kind <> FField.Kind) or (FFocused and not FEditing) then
  begin
    raise ENyxCollection.Create('Cell edit field, value and focus ownership must agree');
  end;
  NyxTextScalarCount(FText);

  if FSelection.Defined then
  begin
    LSelection := NyxTextSelection(FText, FSelection.Start, FSelection.Finish,
      FSelection.Direction);

    if not LSelection.SameRange(FSelection) then
    begin
      raise ENyxCollection.Create('Cell edit selection does not describe its exact draft');
    end;
  end;
end;

function NyxCollectionCellEdit(const AItem: TNyxItemRef;
  const AField: TNyxColumnIdentity; const AAccepted: TNyxStateValue;
  const AText: TNyxText; const ASelection: TNyxTextSelection;
  AEditing, AFocused: Boolean): TNyxCollectionCellEdit;
begin
  Result := Default(TNyxCollectionCellEdit);
  Result.FDefined := True;
  Result.FItem := AItem;
  Result.FField := AField;
  Result.FAccepted := AAccepted.Copy;
  Result.FText := AText;
  Result.FSelection := ASelection;
  Result.FEditing := AEditing;
  Result.FFocused := AFocused;
  Result.Validate;
end;

constructor TCollectionEditsSnapshot.Create(const AStore: INyxCollection;
  const ACells: array of TNyxCollectionCellEdit);
var
  LIndex, LEditing, LFocused: Integer;
  LKey, LIdentity: TNyxText;
  LSeen: TNyxTextIndex;
  LSource: INyxCollectionSnapshot;
begin
  inherited Create;
  FStore := AStore;

  if FStore = nil then
  begin
    raise ENyxCollection.Create('Cell edits require their exact runtime store');
  end;
  { Own the vector before a third-party store getter can call application code. }
  SetLength(FCells, Length(ACells));
  for LIndex := 0 to High(ACells) do
  begin
    FCells[LIndex] := ACells[LIndex].Copy;
    FCells[LIndex].Validate;
  end;
  LSource := FStore.Snapshot;
  LKey := LSource.Key.Name;
  LSeen := TNyxTextIndex.Create;
  try
    LEditing := 0;
    LFocused := 0;
    for LIndex := 0 to High(FCells) do
    begin

      if FCells[LIndex].Item.Collection.Name <> LKey then
      begin
        raise ENyxCollection.Create('Cell edit belongs to a different collection');
      end;
      { Portable identities prohibit control characters, so this temporary key
        is unambiguous. It controls neither behavior nor serialized metadata. }
      LIdentity := FCells[LIndex].Item.ID + #0 + FCells[LIndex].Field.Name;

      if LSeen.IndexOf(LIdentity) >= 0 then
      begin
        raise ENyxCollection.Create('Cell edit snapshot repeats an item field');
      end;
      LSeen.AddFirst(LIdentity, LIndex);

      if FCells[LIndex].Editing then
      begin
        Inc(LEditing);
      end;

      if FCells[LIndex].Focused then
      begin
        Inc(LFocused);
      end;
    end;

    if (LEditing > 1) or (LFocused > 1) then
    begin
      raise ENyxCollection.Create('Cell edits require one editing and focus owner');
    end;
  finally
    LSeen.Free;
  end;
end;

function TCollectionEditsSnapshot.GetStore: INyxCollection;
begin
  Result := FStore;
end;

function TCollectionEditsSnapshot.GetCount: Integer;
begin
  Result := Length(FCells);
end;

function TCollectionEditsSnapshot.CellAt(AIndex: Integer): TNyxCollectionCellEdit;
begin

  if (AIndex < 0) or (AIndex >= Length(FCells)) then
  begin
    raise ENyxCollection.Create('Cell edit index is outside its snapshot');
  end;
  Result := FCells[AIndex].Copy;
end;

function NewNyxCollectionEditsSnapshot(const AStore: INyxCollection;
  const ACells: array of TNyxCollectionCellEdit): INyxCollectionEditsSnapshot;
begin
  Result := TCollectionEditsSnapshot.Create(AStore, ACells);
end;

function TNyxCollectionMountBase.DisplayColumnCount: Integer;
begin
  Result := FView.Spec.Count;

  if FHasColumnLayout then
  begin
    Result := FColumnLayout.VisibleCount;
  end;
end;

function TNyxCollectionMountBase.DisplayColumn(AIndex: Integer): Integer;
begin
  Result := AIndex;

  if FHasColumnLayout then
  begin
    Result := FColumnLayout.VisibleAt(AIndex);
  end;
end;

function TNyxCollectionMountBase.ColumnDisplayIndex(ACanonical: Integer): Integer;
var
  LIndex: Integer;
begin
  Result := -1;
  for LIndex := 0 to DisplayColumnCount - 1 do
  begin

    if DisplayColumn(LIndex) = ACanonical then
    begin
      Exit(LIndex);
    end;
  end;
end;

function TNyxCollectionMountBase.ColumnVisible(ACanonical: Integer): Boolean;
begin
  Result := (ACanonical >= 0) and (ACanonical < FView.Spec.Count);

  if Result and FHasColumnLayout then
  begin
    Result := FColumnLayout.ColumnAt(ACanonical).IsVisible;
  end;
end;

function TNyxCollectionMountBase.GetQueryWorkbench: INyxCollectionQueryWorkbench;
begin
  Result := nil;

  if GetInputReady then
  begin
    Result := FQueries;
  end;
end;

function TNyxCollectionMountBase.GetColumnArrangement: INyxColumnArrangementManager;
begin
  Result := nil;

  if GetInputReady then
  begin
    Result := FColumnManager;
  end;
end;

function TNyxCollectionMountBase.ClipboardInputAvailable: Boolean;
begin
  Result := True;
end;

function TNyxCollectionMountBase.PrepareClipboardInput: INyxCancellationReadiness;
begin
  Result := nil;

  if FConnected and FEnabled and not FUpdating and not FEditsSuspended and
    (FInputLifetime <> nil) and FInputLifetime.Ready and ClipboardInputAvailable then
  begin
    Result := TClipboardInputFence.Create(Self);
  end;
end;

function TNyxCollectionMountBase.ClipboardField(ACanonical: Integer): TNyxColumnIdentity;
begin
  Result := Default(TNyxColumnIdentity);

  if (ACanonical >= 0) and (ACanonical < Length(FClipboardFields)) then
  begin
    Result := FClipboardFields[ACanonical];
  end;
end;

function TNyxCollectionMountBase.GetClipboardCommands: INyxCollectionClipboardCommands;
begin
  Result := FClipboardCommands;
end;

function TNyxCollectionMountBase.GetCellActions: INyxCollectionCellActions;
begin
  Result := FCellActions;
end;

function TNyxCollectionMountBase.BeginCellPresentation: INyxCollectionCellPresentation;
begin
  Result := nil;
end;

procedure TNyxCollectionMountBase.ConfigureCellActions(
  const AFactory: INyxCollectionCellPresenterFactory;
  const AScheduler: INyxScheduler);
var
  LLease: INyxCollectionMount;
  LView: INyxCollectionView;
  LInput: INyxCollectionInputLifetime;
  LFactory: INyxCollectionCellPresenterFactory;
  LWriter: INyxCollectionClipboardCapturedWrite;
  LScheduler: INyxScheduler;
  LCandidate: INyxCollectionCellActions;

  procedure Current;
  begin

    if not FConnected or FDisconnecting or (FView <> LView) or
      (FInputLifetime <> LInput) or not LInput.Current or (FCellActions <> nil) then
    begin
      raise ENyxCollectionClipboard.Create('Table changed while configuring cell actions');
    end;
  end;
begin

  if FConfiguringCellActions or not FConnected or FDisconnecting or
    (FClipboardCommands = nil) or (FCellActions <> nil) or
    (AFactory = nil) or (AScheduler = nil) then
  begin
    raise ENyxCollectionClipboard.Create('Cell actions require one configured table-copy incarnation');
  end;
  FConfiguringCellActions := True;
  try
    LLease := Self as INyxCollectionMount;
    LView := FView;
    LInput := FInputLifetime;
    LFactory := AFactory;
    LScheduler := AScheduler;
    LScheduler.RequireUI;
    Current;
    Supports(FClipboardCommands, INyxCollectionClipboardCapturedWrite, LWriter);
    Current;
    try
      LCandidate := NewNyxCollectionCellActions(LFactory, LWriter, LScheduler, BeginCellPresentation);
      Current;
      FCellFactory := LFactory;
      FCellActions := LCandidate;
    except
      try

        if LCandidate <> nil then
        begin
          LCandidate.Disconnect;
        end
        else
        begin
          LFactory.Revoke;
        end;
      except
        { Preserve the original construction/admission failure. }
      end;
      raise;
    end;
  finally
    FConfiguringCellActions := False;
  end;
end;

procedure TNyxCollectionMountBase.ConfigureClipboard(const AHost: INyxClipboardWriteHost;
  const AEvents: INyxEvents; const AScheduler: INyxScheduler;
  const AOptions: TNyxCollectionClipboardOptions);
var
  LLease: INyxCollectionMount;
  LView: INyxCollectionView;
  LInput: INyxCollectionInputLifetime;
  LHost: INyxClipboardWriteHost;
  LEvents: INyxEvents;
  LScheduler: INyxScheduler;
  LOptions: TNyxCollectionClipboardOptions;
  LSpec: TNyxCollectionViewSpec;
  LFields: TNyxCollectionClipboardFields;
  LCommands: INyxCollectionClipboardCommands;
  LIndex: Integer;

  procedure Current;
  begin

    if not FConnected or FDisconnecting or (FView <> LView) or
      (FInputLifetime <> LInput) or not LInput.Current or
      (FClipboardCommands <> nil) then
    begin
      raise ENyxCollectionClipboard.Create('Table changed while configuring clipboard authority');
    end;
  end;
begin

  if FConfiguringClipboard or not FConnected or FDisconnecting or
    (FClipboardCommands <> nil) or (AHost = nil) or
    (AEvents = nil) or (AScheduler = nil) then
  begin
    raise ENyxCollectionClipboard.Create('Clipboard authority requires one connected table incarnation');
  end;
  FConfiguringClipboard := True;
  try
    LLease := Self as INyxCollectionMount;
    LView := FView;
    LInput := FInputLifetime;
    LHost := AHost;
    LEvents := AEvents;
    LScheduler := AScheduler;
    LOptions := AOptions;
    LOptions.Validate;

    if LView.Projection <> cpTable then
    begin
      raise ENyxCollectionClipboard.Create('Clipboard cell/row commands require a table projection');
    end;
    Current;
    LSpec := LView.Spec;
    Current;
    SetLength(LFields, LSpec.Count);
    for LIndex := 0 to Length(LFields) - 1 do
    begin
      LFields[LIndex] := LSpec.ColumnAt(LIndex).Identity;
    end;
    { A previously disabled legacy table has not needed the optional epoch
      shield yet. Synchronize it BEFORE the controller can capture input. }
    TCollectionInputState(FInputStateObject).InteractionEnabled(FEnabled);
    try
      LCommands := NewNyxCollectionClipboardCommands(LView, LInput, LEvents,
        LHost, LScheduler, LOptions, PrepareClipboardInput);
      Current;
      FClipboardFields := LFields;
      FClipboardCommands := LCommands;
    except

      if (LCommands <> nil) and (FClipboardCommands = LCommands) then
      begin
        FClipboardCommands := nil;
        FClipboardFields := nil;
      end;
      try

        if LCommands <> nil then
        begin
          LCommands.Revoke;
        end;
      except
        { Construction failure owns the original exception. }
      end;
      raise;
    end;
  finally
    FConfiguringClipboard := False;
  end;
end;

procedure TNyxCollectionMountBase.ConfigureColumnArrangement(
  const APresenters: INyxColumnArrangementPresenterFactory;
  const AEvents: INyxEvents);
var
  LKeepAlive: INyxCollectionMount;
  LView: INyxCollectionView;
  LInput: INyxCollectionInputLifetime;
  LFactory: INyxColumnArrangementPresenterFactory;
  LManager: INyxColumnArrangementManager;
  LEvents: INyxEvents;

  procedure Current;
  begin

    if not FConnected or (FView <> LView) or (FInputLifetime <> LInput) or
      (FColumnManager <> nil) then
    begin
      raise ENyxCollection.Create('Table changed while configuring column management');
    end;
  end;
begin

  if FConfiguringColumns or not FConnected or not FHasColumnLayout or
    (APresenters = nil) or (AEvents = nil) or (FColumnManager <> nil) then
  begin
    raise ENyxCollection.Create('Column management requires one connected arranged table');
  end;
  FConfiguringColumns := True;
  try
    LKeepAlive := Self as INyxCollectionMount;
    LView := FView;
    LInput := FInputLifetime;
    LFactory := APresenters;
    try
      LEvents := NewNyxArrangementQueryEvents(AEvents, LInput);
      Current;
      LManager := NewNyxColumnArrangementManager(LView, LEvents, LFactory,
        PrepareColumnFit);
      Current;
      LManager.SetEnabled(FEnabled);
      Current;
      FColumnFactory := LFactory;
      FColumnManager := LManager;
      Refresh;
    except

      if (LManager <> nil) and (FColumnManager = LManager) then
      begin
        FColumnManager := nil;
        FColumnFactory := nil;
      end;
      try

        if LManager <> nil then
        begin
          LManager.Disconnect;
        end;
      except
        { Continue independent revocation without replacing admission's error. }
      end;
      try
        LFactory.Revoke;
      except
        { A failed constructor cannot leave borrowed target access alive. }
      end;
      raise;
    end;
  finally
    FConfiguringColumns := False;
  end;
end;

procedure TNyxCollectionMountBase.ConfigureQueries(
  const APresenters: INyxQueryFormPresenterFactory; const AEvents: INyxEvents;
  const ATarget: TNyxEventTarget; const ADesignID: TNyxText);
var
  LKeepAlive: INyxCollectionMount;
  LQueries: INyxCollectionQueryWorkbench;
  LSession: INyxCollectionQuerySession;
  LFactory: INyxQueryFormPresenterFactory;
  LQueryEvents: INyxEvents;
  LInput: INyxCollectionInputLifetime;
  LInputFactory: INyxCollectionQueryInputFactory;
  LView: INyxCollectionView;
  LSpec: TNyxCollectionViewSpec;

  procedure Current;
  begin

    if not FConnected or (FView <> LView) or (FInputLifetime <> LInput) or
      (FQueries <> nil) then
    begin
      raise ENyxCollection.Create('Table changed while configuring query controls');
    end;
  end;
begin

  if FConfiguringQueries or not FConnected or (FView = nil) or
    (APresenters = nil) or (AEvents = nil) or (FQueries <> nil) then
  begin
    raise ENyxCollection.Create('Query controls require one connected table and exact target presenter');
  end;
  FConfiguringQueries := True;
  try
    LKeepAlive := Self as INyxCollectionMount;
    LFactory := APresenters;
    LQueryEvents := AEvents;
    LInput := FInputLifetime;
    LView := FView;
    try

      if LView.Projection <> cpTable then
      begin
        raise ENyxCollection.Create('Query controls require a table projection');
      end;
      Current;
      LSpec := LView.Spec;
      Current;

      if not LSpec.TableQueryPolicy.IsEnabled then
      begin
        LFactory.Revoke;
        Exit;
      end;

      if not Supports(LView, INyxCollectionQuerySession, LSession) then
      begin
        Current;
        LFactory.Revoke;

        if LSpec.HasQueryControls then
        begin
          raise ENyxCollection.Create('This view implementation does not expose query sessions for saved table controls');
        end;
        { Preserve ordinary custom view implementations of the original
          interface without inventing a second mutable query owner. }
        Exit;
      end;
      Current;

      if FHasColumnLayout then
      begin

        if not Supports(LFactory, INyxCollectionQueryInputFactory, LInputFactory) then
        begin
          raise ENyxCollection.Create('Arranged queries require an input-aware form factory');
        end;
        Current;
        LInputFactory.ConfigureInput(LInput);
        Current;
        LQueryEvents := NewNyxArrangementQueryEvents(LQueryEvents, LInput);
        Current;
      end;
      LQueries := NewNyxCollectionQueryWorkbench(LView, LQueryEvents, ATarget,
        LFactory, ADesignID);
      Current;
      FQueryFactory := LFactory;
      FQueries := LQueries;
      FQueries.SetEnabled(FEnabled);
      Refresh;
    except

      if (LQueries <> nil) and (FQueries = LQueries) then
      begin
        FQueries := nil;
        FQueryFactory := nil;
      end;
      try

        if LQueries <> nil then
        begin
          LQueries.Disconnect;
        end;
      except
        { Attempt independent factory revocation while preserving admission's
          original failure; no partial controller may survive this attempt. }
      end;
      try
        LFactory.Revoke;
      except
        { Cleanup cannot replace the original constructor/refusal failure. }
      end;
      raise;
    end;
  finally
    FConfiguringQueries := False;
  end;
end;

procedure TNyxCollectionMountBase.UpdateQueryHeaders;
var
  LObservation: TNyxQueryWorkbenchObservation;
  LColumn: Integer;
begin
  FQueryHeaders := nil;
  FQuerySummary := '';

  if (FQueries = nil) or not FQueries.Connected then
  begin
    Exit;
  end;
  LObservation := FQueries.Observation;

  if not FConnected or (FView = nil) then
  begin
    Exit;
  end;
  FQuerySummary := IntToStr(LObservation.MatchedRows) + TNyxText(' matching / ') +
    IntToStr(LObservation.SourceRows) + TNyxText(' source rows');
  SetLength(FQueryHeaders, FView.Spec.Count);
  for LColumn := 0 to High(FQueryHeaders) do
  begin
    FQueryHeaders[LColumn] := NyxQueryWorkbenchHeader(LObservation, FView.Spec, LColumn);
  end;
end;

function TNyxCollectionMountBase.QueryHeader(AColumn: Integer): TNyxText;
begin

  if (AColumn >= 0) and (AColumn < Length(FQueryHeaders)) then
  begin
    Exit(FQueryHeaders[AColumn]);
  end;
  Result := FView.Spec.ColumnAt(AColumn).Title;
end;

constructor TNyxCollectionMountBase.Create(const AView: INyxCollectionView;
  AInputPhase: TNyxCollectionInputPhase;
  const AInputGate: INyxCollectionInputGate);
var
  LCanonical: ICanonicalInputGate;
  LNode: TCollectionInputGate;
  LState: TCollectionInputState;
  LInstalled: Boolean;
  LTable: Boolean;
begin
  inherited Create;

  if not (AInputPhase in [ncipReady, ncipStaged]) then
  begin
    raise ENyxCollection.Create('Unknown collection input construction phase');
  end;
  LInstalled := AInputPhase = ncipReady;

  if AView = nil then
  begin
    raise ENyxCollection.Create('Collection mount requires a live view');
  end;
  FView := AView;
  FEnabled := True;
  LTable := AView.Projection = cpTable;
  FHasColumnLayout := LTable and AView.Spec.HasLayoutControls;

  if not LTable then
  begin
    { Explicit table staging also owns later opt-in clipboard commands. Default
      direct mounts remain installed; other projections retain their old path. }
    LInstalled := True;
  end;

  if LTable then
  begin
    FInputGate := AInputGate;

    if FInputGate <> nil then
    begin

      if not Supports(FInputGate, ICanonicalInputGate, LCanonical) then
      begin
        raise ENyxCollection.Create('Collection input requires a factory-created lifetime gate');
      end;
      LNode := LCanonical.GetNode;

      if LNode = nil then
      begin
        raise ENyxCollection.Create('Collection input lifetime gate has no canonical owner');
      end;

      if (LNode as INyxCollectionInputGate) <> FInputGate then
      begin
        raise ENyxCollection.Create('Collection input lifetime gate differs from its canonical owner');
      end;
    end;
  end;
  LState := TCollectionInputState.Create(LInstalled, FInputGate);
  FInputLifetime := LState;
  FInputStateObject := LState;

  if FHasColumnLayout then
  begin
    FColumnLayout := TNyxColumnLayoutObservation.Read(
      NyxCollectionColumns(AView).CaptureColumns);
  end;
  { The view owns only immutable authored options. Each browser/native mount
    starts a separate engine before activation; prefix/time/focus and later
    ConfigureTypeAhead overrides never write back into the saved binding. }
  FTypeAhead := NewNyxTypeAhead(AView.Spec.TypeAheadPolicy);
  FTypeAheadSnapshot := nil;

  FLastSnapshot := nil;
  FChanges := nil;
  FRefreshPlan := Default(TNyxCollectionRefreshPlan);
end;

destructor TNyxCollectionMountBase.Destroy;
begin
  try
    DisconnectInternal;
  except
    { Explicit Disconnect is the fallible cleanup boundary. Final release must
      finish freeing the owner even if a host/controller cleanup raises. }
  end;
  inherited Destroy;
end;

function TNyxCollectionMountBase.GetInputLifetime: INyxCollectionInputLifetime;
begin
  Result := FInputLifetime;
end;

function TNyxCollectionMountBase.PrepareColumnFit(
  const ALaunch: INyxCollectionColumnsCapture; const AField: TNyxColumnIdentity;
  var AMeasurement: INyxColumnFitMeasurement): INyxPreparedPublication;
var
  LKeepAlive: INyxCollectionMount;
begin
  LKeepAlive := Self as INyxCollectionMount;
  AMeasurement := nil;
  Result := nil;
  raise ENyxCollection.Create('This collection target does not support measured column fit');
end;

function TNyxCollectionMountBase.GetInputReady: Boolean;
begin
  Result := (FInputLifetime <> nil) and FInputLifetime.Ready;
end;

function TNyxCollectionMountBase.InputLifetimeCurrent: Boolean;
begin
  Result := (FInputLifetime <> nil) and FInputLifetime.Current;
end;

procedure TNyxCollectionMountBase.SetCellEditsSuspended(AValue: Boolean);
var
  LClipboard: INyxCollectionClipboardCommands;
  LCells: INyxCollectionCellActions;
  LFailure: TMountCleanupFailure;
begin
  LClipboard := FClipboardCommands;
  LCells := FCellActions;
  FEditsSuspended := AValue;

  if FInputStateObject <> nil then
  begin
    TCollectionInputState(FInputStateObject).Suspend(AValue);
  end;

  if AValue then
  begin
    { Epoch withdrawal precedes host cancellation. Callers retain the mount and
      recheck their exact suspension before touching any borrowed target. }
    LFailure := Default(TMountCleanupFailure);
    try

      if LCells <> nil then
      begin
        LCells.Close;
      end;
    except
      {$ifdef PAS2JS}CaptureMountCleanup(LFailure, JSExceptValue);
      {$else}on E: Exception do CaptureMountCleanup(LFailure, TNyxText(E.Message));{$endif}
    end;
    try

      if LClipboard <> nil then
      begin
        LClipboard.CancelPending;
      end;
    except
      {$ifdef PAS2JS}CaptureMountCleanup(LFailure, JSExceptValue);
      {$else}on E: Exception do CaptureMountCleanup(LFailure, TNyxText(E.Message));{$endif}
    end;
    RaiseMountCleanup(LFailure);
  end;
end;

procedure TNyxCollectionMountBase.InstallInput;
begin
  { Pure adoption only. In particular, do not resume editors or touch native
    hooks here: the former accepted tree has already retired at this boundary. }

  if FInputStateObject <> nil then
  begin
    TCollectionInputState(FInputStateObject).Install;
  end;
end;

procedure TNyxCollectionMountBase.Activate;
begin

  if FConnected then
  begin
    raise ENyxCollection.Create('Collection mount is already active');
  end;

  if FView = nil then
  begin
    raise ENyxCollection.Create('Disconnected collection mount cannot reactivate');
  end;
  FToken := FView.Subscribe(Changed);
  FSelection := FView.Selection;
  FConnected := True;
  TCollectionInputState(FInputStateObject).Connect;
  try
    Refresh;
  except
    Disconnect;
    raise;
  end;
end;

procedure TNyxCollectionMountBase.Disconnect;
var
  LLease: INyxCollectionMount;
begin

  if FDisconnecting or ((FView = nil) and not FConnected) then
  begin
    Exit;
  end;
  LLease := Self as INyxCollectionMount;
  DisconnectInternal;
end;

procedure TNyxCollectionMountBase.DisconnectInternal;
var
  LQueries: INyxCollectionQueryWorkbench;
  LQueryFactory: INyxQueryFormPresenterFactory;
  LManager: INyxColumnArrangementManager;
  LFactory: INyxColumnArrangementPresenterFactory;
  LToken: INyxCollectionViewSubscription;
  LFailure: TMountCleanupFailure;
  LClipboard: INyxCollectionClipboardCommands;
  LCells: INyxCollectionCellActions;
  LCellFactory: INyxCollectionCellPresenterFactory;
begin

  if FDisconnecting or ((FView = nil) and not FConnected) then
  begin
    Exit;
  end;
  FDisconnecting := True;
  FConnected := False;
  FCellFocusValid := False;

  if FInputStateObject <> nil then
  begin
    TCollectionInputState(FInputStateObject).Retire;
  end;
  { Detach every public receiver before the first alternative-factory callback.
    Input already refuses defaults. Each independent cleanup is attempted even
    when an implementation throws; retain the first failure for the caller
    and always release the disconnect guard and borrowed target at the end. }
  LClipboard := FClipboardCommands;
  LCells := FCellActions;
  LCellFactory := FCellFactory;
  LManager := FColumnManager;
  LFactory := FColumnFactory;
  LQueries := FQueries;
  LQueryFactory := FQueryFactory;
  LToken := FToken;
  FClipboardCommands := nil;
  FCellActions := nil;
  FCellFactory := nil;
  FClipboardFields := nil;
  FColumnManager := nil;
  FColumnFactory := nil;
  FQueries := nil;
  FQueryFactory := nil;
  FToken := nil;
  LFailure := Default(TMountCleanupFailure);
  try
    try

      if LCells <> nil then
      begin
        LCells.Revoke;
      end;
    except
      {$ifdef PAS2JS}CaptureMountCleanup(LFailure, JSExceptValue);
      {$else}on E: Exception do CaptureMountCleanup(LFailure, TNyxText(E.Message));{$endif}
    end;
    try

      if LCellFactory <> nil then
      begin
        LCellFactory.Revoke;
      end;
    except
      {$ifdef PAS2JS}CaptureMountCleanup(LFailure, JSExceptValue);
      {$else}on E: Exception do CaptureMountCleanup(LFailure, TNyxText(E.Message));{$endif}
    end;
    try

      if LClipboard <> nil then
      begin
        LClipboard.Revoke;
      end;
    except
      {$ifdef PAS2JS}
      CaptureMountCleanup(LFailure, JSExceptValue);
      {$else}
      on E: Exception do
      begin
        CaptureMountCleanup(LFailure, TNyxText(E.Message));
      end;
      {$endif}
    end;
    try

      if LManager <> nil then
      begin
        LManager.Revoke;
      end;
    except
      {$ifdef PAS2JS}
      CaptureMountCleanup(LFailure, JSExceptValue);
      {$else}
      on E: Exception do
      begin
        CaptureMountCleanup(LFailure, TNyxText(E.Message));
      end;
      {$endif}
    end;
    try

      if LFactory <> nil then
      begin
        LFactory.Revoke;
      end;
    except
      {$ifdef PAS2JS}
      CaptureMountCleanup(LFailure, JSExceptValue);
      {$else}
      on E: Exception do
      begin
        CaptureMountCleanup(LFailure, TNyxText(E.Message));
      end;
      {$endif}
    end;
    try

      if LQueries <> nil then
      begin
        LQueries.Revoke;
      end;
    except
      {$ifdef PAS2JS}
      CaptureMountCleanup(LFailure, JSExceptValue);
      {$else}
      on E: Exception do
      begin
        CaptureMountCleanup(LFailure, TNyxText(E.Message));
      end;
      {$endif}
    end;
    try

      if LQueryFactory <> nil then
      begin
        LQueryFactory.Revoke;
      end;
    except
      {$ifdef PAS2JS}
      CaptureMountCleanup(LFailure, JSExceptValue);
      {$else}
      on E: Exception do
      begin
        CaptureMountCleanup(LFailure, TNyxText(E.Message));
      end;
      {$endif}
    end;
    try
      RetireTargetInput;
    except
      {$ifdef PAS2JS}
      CaptureMountCleanup(LFailure, JSExceptValue);
      {$else}
      on E: Exception do
      begin
        CaptureMountCleanup(LFailure, TNyxText(E.Message));
      end;
      {$endif}
    end;
    try

      if LCells <> nil then
      begin
        LCells.Disconnect;
      end;
    except
      {$ifdef PAS2JS}CaptureMountCleanup(LFailure, JSExceptValue);
      {$else}on E: Exception do CaptureMountCleanup(LFailure, TNyxText(E.Message));{$endif}
    end;
    try

      if LManager <> nil then
      begin
        LManager.Disconnect;
      end;
    except
      {$ifdef PAS2JS}
      CaptureMountCleanup(LFailure, JSExceptValue);
      {$else}
      on E: Exception do
      begin
        CaptureMountCleanup(LFailure, TNyxText(E.Message));
      end;
      {$endif}
    end;
    try

      if LQueries <> nil then
      begin
        LQueries.Disconnect;
      end;
    except
      {$ifdef PAS2JS}
      CaptureMountCleanup(LFailure, JSExceptValue);
      {$else}
      on E: Exception do
      begin
        CaptureMountCleanup(LFailure, TNyxText(E.Message));
      end;
      {$endif}
    end;
    try
      ResetTypeAhead;
    except
      {$ifdef PAS2JS}
      CaptureMountCleanup(LFailure, JSExceptValue);
      {$else}
      on E: Exception do
      begin
        CaptureMountCleanup(LFailure, TNyxText(E.Message));
      end;
      {$endif}
    end;
    try

      if LToken <> nil then
      begin
        LToken.Disconnect;
      end;
    except
      {$ifdef PAS2JS}
      CaptureMountCleanup(LFailure, JSExceptValue);
      {$else}
      on E: Exception do
      begin
        CaptureMountCleanup(LFailure, TNyxText(E.Message));
      end;
      {$endif}
    end;
    try

      if FView <> nil then
      begin
        DetachTarget;
      end;
    except
      {$ifdef PAS2JS}
      CaptureMountCleanup(LFailure, JSExceptValue);
      {$else}
      on E: Exception do
      begin
        CaptureMountCleanup(LFailure, TNyxText(E.Message));
      end;
      {$endif}
    end;
  finally
    FSearchOrder := nil;
    FSelectionObserver := nil;
    FSelection := nil;
    FTypeAhead := nil;
    FTypeAheadSnapshot := nil;
    FLastSnapshot := nil;
    FChanges := nil;
    FRefreshPlan := Default(TNyxCollectionRefreshPlan);
    FQueryHeaders := nil;
    FQuerySummary := '';
    FColumnLayout := Default(TNyxColumnLayoutObservation);
    FView := nil;
    FInputGate := nil;
    FInputStateObject := nil;
    FInputLifetime := nil;
    FCellFocusStore := nil;
    FCellFocusDraft := Default(TNyxCollectionCellEdit);
    FDisconnecting := False;
  end;

  RaiseMountCleanup(LFailure);
end;
procedure TNyxCollectionMountBase.Changed(const AView: INyxCollectionView;
  const AChanges: INyxCollectionChanges);
var
  LKeepAlive: INyxCollectionMount;
  LBefore: INyxCollectionSelection;
  LAfter: INyxCollectionSelection;
  LObserver: TNyxCollectionSelectionObserver;
begin
  LKeepAlive := Self as INyxCollectionMount;
  FCellFocusValid := False;

  if FCellMutationSerial < High(Integer) then
  begin
    Inc(FCellMutationSerial);
  end;
  LBefore := FSelection;
  LAfter := AView.Selection;
  FSelection := LAfter;
  LObserver := FSelectionObserver;
  FChanges := AChanges;
  try
    Refresh;
  finally
    { Do not retain before/after datasets beyond the notification frame. }
    FChanges := nil;
  end;

  if GetInputReady and FEnabled and Assigned(LObserver) and
    not LAfter.SameState(LBefore) then
  begin
    { Last borrowed call. It may dispose the renderer and disconnect this mount. }
    LObserver(LBefore, LAfter);
  end;
end;

procedure TNyxCollectionMountBase.ObserveSelection(AObserver: TNyxCollectionSelectionObserver);
begin

  if not FConnected then
  begin
    raise ENyxCollection.Create('Cannot observe a disconnected collection control');
  end;
  FSelectionObserver := AObserver;
  FSelection := FView.Selection;
end;

function TNyxCollectionMountBase.DeferTargetRefresh: Boolean;
begin
  Result := False;
end;

function TNyxCollectionMountBase.DeferComposingRefresh: Boolean;
begin
  Result := False;
end;

procedure TNyxCollectionMountBase.RefreshTargetIdle;
begin
  { Ordinary targets have no deferred native-tail drain. }
end;

procedure TNyxCollectionMountBase.RetireTargetInput;
begin
  { Ordinary targets have no additional weak input receiver. }
end;

procedure TNyxCollectionMountBase.Refresh;
var
  LKeepAlive: INyxCollectionMount;
  LSnapshot: INyxCollectionSnapshot;
  LView: INyxCollectionView;
  LSpec: TNyxCollectionViewSpec;
  LFormerLayout, LNextLayout: TNyxColumnLayoutObservation;
  LFormerHeaders: array of TNyxText;
  LFormerSummary: TNyxText;
  LInputLease: INyxCollectionInputLifetime;
  LInputState: TCollectionInputState;
begin
  FCellFocusValid := False;

  if not InputLifetimeCurrent or FUpdating or FEditsSuspended then
  begin
    Exit;
  end;
  { Target callbacks may release the renderer's last attachment reference.
    Retain the current call frame until its cleanup has finished. }
  LKeepAlive := Self as INyxCollectionMount;
  LView := FView;
  FTargetRefreshDeferred := False;

  LFormerLayout := FColumnLayout;
  LNextLayout := FColumnLayout;
  LFormerHeaders := FQueryHeaders;
  LFormerSummary := FQuerySummary;
  LInputLease := nil;
  LInputState := nil;
  FUpdating := True;
  try

    if DeferComposingRefresh then
    begin
      Exit;
    end;

    if FHasColumnLayout or (FClipboardCommands <> nil) then
    begin
      { The independent state survives a reentrant Disconnect that clears the
        mount field. Ready blocks NEW OS/DOM input throughout all outward reads
        and physical synchronization; Current still permits internal imports. }
      LInputLease := FInputLifetime;
      LInputState := TCollectionInputState(FInputStateObject);
      LInputState.Refreshing(True);
    end;

    if DeferTargetRefresh then
    begin
      Exit;
    end;

    if FHasColumnLayout then
    begin
      LNextLayout := TNyxColumnLayoutObservation.Read(
        NyxCollectionColumns(LView).CaptureColumns);
    end;

    if not InputLifetimeCurrent or (FView <> LView) then
    begin
      Exit;
    end;
    UpdateQueryHeaders;

    if not InputLifetimeCurrent or (FView <> LView) then
    begin
      Exit;
    end;
    LSnapshot := LView.Snapshot;
    LSpec := LView.Spec;

    if not InputLifetimeCurrent or (FView <> LView) then
    begin
      Exit;
    end;
    FRefreshPlan := NyxCollectionRefreshPlan(FLastSnapshot, LSnapshot, LSpec, FChanges);

    if not InputLifetimeCurrent or (FView <> LView) or DeferTargetRefresh then
    begin
      Exit;
    end;
    FColumnLayout := LNextLayout;
    RenderDataset;

    if FTargetRefreshDeferred then
    begin
      Exit;
    end;

    if InputLifetimeCurrent then
    begin
      FLastSnapshot := LSnapshot;
    end;
    Inc(FRefreshCount);
  finally
    { A source getter may start composition after the initial admission check.
      Preserve the caches belonging to the still-visible physical column map. }

    if FTargetRefreshDeferred and InputLifetimeCurrent and (FView = LView) then
    begin
      FColumnLayout := LFormerLayout;
      FQueryHeaders := LFormerHeaders;
      FQuerySummary := LFormerSummary;
    end;
    FRefreshPlan := Default(TNyxCollectionRefreshPlan);
    FUpdating := False;

    if (LInputLease <> nil) and (LInputState <> nil) then
    begin
      { Clearing this temporary flag cannot revive a terminal lifetime. Never
        borrow FInputStateObject again after an outward reader or target call. }
      LInputState.Refreshing(False);
    end;
    { Last target call, with this call frame still retained. No borrowed target
      or Self read follows a wake that could retire the renderer. }
    RefreshTargetIdle;
  end;
end;

function TNyxCollectionMountBase.MatchesCellFocusProof(
  const AStore: INyxCollection; const ACell: TNyxCollectionCellEdit): Boolean;
begin
  { Record comparisons only. In particular, never ask a retained external store
    for its snapshot while deciding whether rollback may move physical focus. }
  Result := InputLifetimeCurrent and FHasColumnLayout and FCellFocusValid and
    FEnabled and not FReadOnly and (AStore <> nil) and
    (AStore = FCellFocusStore) and ACell.Defined and ACell.Editing and
    ACell.Focused and FCellFocusDraft.Defined and
    (ACell.Item.Collection.Name = FCellFocusDraft.Item.Collection.Name) and
    (ACell.Item.ID = FCellFocusDraft.Item.ID) and
    ACell.Field.Same(FCellFocusDraft.Field) and
    ACell.Accepted.SameValue(FCellFocusDraft.Accepted) and
    (ACell.Text = FCellFocusDraft.Text) and
    (ACell.Selection.Defined = FCellFocusDraft.Selection.Defined);

  if Result and ACell.Selection.Defined then
  begin
    Result := ACell.Selection.SameRange(FCellFocusDraft.Selection);
  end;
end;

procedure TNyxCollectionMountBase.Failed(AException: Exception);
begin
  FError := TNyxText(AException.Message);
  FFailure := nbfRejected;

  if (AException is ENyxCollectionNotification) or
    (AException is ENyxPublicationNotification) then
  begin
    FFailure := nbfNotificationFailed;
  end;
end;

procedure TNyxCollectionMountBase.SetInteraction(AEnabled, AReadOnly: Boolean);
var
  LLease: INyxCollectionMount;
  LView: INyxCollectionView;
  LInput: INyxCollectionInputLifetime;
  LClipboard: INyxCollectionClipboardCommands;
  LCells: INyxCollectionCellActions;
  LQueries: INyxCollectionQueryWorkbench;
  LManager: INyxColumnArrangementManager;
  LMutation: Integer;
  LBodyFailed: Boolean;
  LFailure: TMountCleanupFailure;

  function Current: Boolean;
  begin
    Result := FConnected and (FView = LView) and (FInputLifetime = LInput) and
      (FEnabled = AEnabled) and (FReadOnly = AReadOnly) and
      (FCellMutationSerial = LMutation) and LInput.Current;
  end;
begin

  if (FEnabled = AEnabled) and (FReadOnly = AReadOnly) then
  begin
    Exit;
  end;
  LLease := Self as INyxCollectionMount;
  LView := FView;
  LInput := FInputLifetime;
  LClipboard := FClipboardCommands;
  LCells := FCellActions;
  FEnabled := AEnabled;
  FReadOnly := AReadOnly;
  FCellFocusValid := False;

  if (FHasColumnLayout or (LClipboard <> nil)) and (FInputStateObject <> nil) then
  begin
    { Revoke captured query epochs BEFORE SetEnabled can call the scheduler or
      retire a form. ReadOnly keeps query/selection authority independent. }
    TCollectionInputState(FInputStateObject).InteractionEnabled(AEnabled);
  end;

  if FCellMutationSerial < High(Integer) then
  begin
    Inc(FCellMutationSerial);
  end;
  LMutation := FCellMutationSerial;
  LBodyFailed := True;
  try
    LQueries := FQueries;

    if LQueries <> nil then
    begin
      LQueries.SetEnabled(AEnabled);
    end;

    if not Current then
    begin
      Exit;
    end;
    LManager := FColumnManager;

    if LManager <> nil then
    begin
      LManager.SetEnabled(AEnabled);
    end;

    if not Current then
    begin
      Exit;
    end;
    ResetTypeAhead;
    Refresh;
    LBodyFailed := False;
  finally

    if not AEnabled then
    begin
      { Input epoch already denies an in-flight receipt. Cancellation is LAST
        from retained locals, so host cleanup cannot invalidate a later target
        access. Preserve an earlier controller/refresh exception if both fail. }

      LFailure := Default(TMountCleanupFailure);
      try

        if LCells <> nil then
        begin
          LCells.Close;
        end;
      except
        {$ifdef PAS2JS}CaptureMountCleanup(LFailure, JSExceptValue);
        {$else}on E: Exception do CaptureMountCleanup(LFailure, TNyxText(E.Message));{$endif}
      end;
      try

        if LClipboard <> nil then
        begin
          LClipboard.CancelPending;
        end;
      except
        {$ifdef PAS2JS}CaptureMountCleanup(LFailure, JSExceptValue);
        {$else}on E: Exception do CaptureMountCleanup(LFailure, TNyxText(E.Message));{$endif}
      end;

      if not LBodyFailed then
      begin
        RaiseMountCleanup(LFailure);
      end;
    end;
  end;
end;

procedure TNyxCollectionMountBase.ConfigureTypeAhead(const AOptions: TNyxTypeAheadOptions);
var
  LCandidate: INyxTypeAhead;
begin

  if not FConnected then
  begin
    raise ENyxCollection.Create('Collection control is disconnected');
  end;
  LCandidate := NewNyxTypeAhead(AOptions);
  FTypeAhead := LCandidate;
  FTypeAheadSnapshot := nil;
end;

procedure TNyxCollectionMountBase.ResetTypeAhead;
begin

  if FTypeAhead <> nil then
  begin
    FTypeAhead.Reset;
  end;
end;

function TNyxCollectionMountBase.SearchLabel(AIndex: Integer): TNyxText;
begin
  Result := FView.CellText(FSearchOrder[AIndex], 0);
end;

function TNyxCollectionMountBase.FindTypeAhead(const ACharacter: TNyxText;
  ATimeMS: Double; const AOrder: TNyxItemRefs; AFocus: Integer): Integer;
begin

  if FTypeAheadSnapshot <> FView.Snapshot then
  begin
    ResetTypeAhead;
    FTypeAheadSnapshot := FView.Snapshot;
  end;
  FSearchOrder := AOrder;
  try
    Result := FTypeAhead.Find(ACharacter, ATimeMS, Length(AOrder), AFocus, SearchLabel);
  finally
    FSearchOrder := nil;
  end;
end;

function TNyxCollectionMountBase.Select(const AItem: TNyxItemRef): Boolean;
begin
  Result := Select(AItem, nsaReplace);
end;

function TNyxCollectionMountBase.Select(const AItem: TNyxItemRef;
  AAction: TNyxSelectionAction): Boolean;
var
  LKeepAlive: INyxCollectionMount;
  LView: INyxCollectionView;
begin

  if not FConnected then
  begin
    raise ENyxCollection.Create('Collection control is disconnected');
  end;

  if not GetInputReady then
  begin
    FError := 'Collection input is awaiting accepted view adoption or edit resume';
    FFailure := nbfRejected;
    Exit(False);
  end;
  LKeepAlive := Self as INyxCollectionMount;
  LView := FView;
  FError := '';
  FFailure := nbfNone;
  try

    if not FEnabled then
    begin
      raise ENyxCollection.Create('Collection control is disabled');
    end;
    LView.Select(AItem, AAction);
    Result := True;
  except
    on LException: Exception do
    begin
      Failed(LException);
      Result := FFailure = nbfNotificationFailed;
      Refresh;
    end;
  end;
end;

function TNyxCollectionMountBase.SetSelection(const AItems: array of TNyxItemRef;
  const AFocus, AAnchor: TNyxItemRef): Boolean;
var
  LKeepAlive: INyxCollectionMount;
  LView: INyxCollectionView;
begin

  if not FConnected then
  begin
    raise ENyxCollection.Create('Collection control is disconnected');
  end;

  if not GetInputReady then
  begin
    FError := 'Collection input is awaiting accepted view adoption or edit resume';
    FFailure := nbfRejected;
    Exit(False);
  end;
  LKeepAlive := Self as INyxCollectionMount;
  LView := FView;
  FError := '';
  FFailure := nbfNone;
  try

    if not FEnabled then
    begin
      raise ENyxCollection.Create('Collection control is disabled');
    end;
    LView.SetSelection(AItems, AFocus, AAnchor);
    Result := True;
  except
    on LException: Exception do
    begin
      Failed(LException);
      Result := FFailure = nbfNotificationFailed;
      Refresh;
    end;
  end;
end;

function TNyxCollectionMountBase.SelectAll: Boolean;
var
  LKeepAlive: INyxCollectionMount;
  LView: INyxCollectionView;
begin

  if not FConnected then
  begin
    raise ENyxCollection.Create('Collection control is disconnected');
  end;

  if not GetInputReady then
  begin
    FError := 'Collection input is awaiting accepted view adoption or edit resume';
    FFailure := nbfRejected;
    Exit(False);
  end;
  LKeepAlive := Self as INyxCollectionMount;
  LView := FView;
  FError := '';
  FFailure := nbfNone;
  try

    if not FEnabled then
    begin
      raise ENyxCollection.Create('Collection control is disabled');
    end;
    LView.SelectAll;
    Result := True;
  except
    on LException: Exception do
    begin
      Failed(LException);
      Result := FFailure = nbfNotificationFailed;
      Refresh;
    end;
  end;
end;

function TNyxCollectionMountBase.SelectRange(const AItem: TNyxItemRef;
  const AVisibleOrder: array of TNyxItemRef; AAdd: Boolean): Boolean;
var
  LKeepAlive: INyxCollectionMount;
  LView: INyxCollectionView;
begin

  if not FConnected then
  begin
    raise ENyxCollection.Create('Collection control is disconnected');
  end;

  if not GetInputReady then
  begin
    FError := 'Collection input is awaiting accepted view adoption or edit resume';
    FFailure := nbfRejected;
    Exit(False);
  end;
  LKeepAlive := Self as INyxCollectionMount;
  LView := FView;
  FError := '';
  FFailure := nbfNone;
  try

    if not FEnabled then
    begin
      raise ENyxCollection.Create('Collection control is disabled');
    end;
    LView.SelectRange(AItem, AVisibleOrder, AAdd);
    Result := True;
  except
    on LException: Exception do
    begin
      Failed(LException);
      Result := FFailure = nbfNotificationFailed;
      Refresh;
    end;
  end;
end;

function TNyxCollectionMountBase.EditCell(const AItem: TNyxItemRef;
  AColumn: Integer; const AWireValue: TNyxText): Boolean;
var
  LBefore: Integer;
  LKeepAlive: INyxCollectionMount;
  LView: INyxCollectionView;
begin

  if not FConnected then
  begin
    raise ENyxCollection.Create('Collection control is disconnected');
  end;

  if not GetInputReady then
  begin
    FError := 'Collection input is awaiting accepted view adoption or edit resume';
    FFailure := nbfRejected;
    Exit(False);
  end;
  LKeepAlive := Self as INyxCollectionMount;
  LView := FView;
  FError := '';
  FFailure := nbfNone;
  LBefore := FRefreshCount;
  try

    if not FEnabled or FReadOnly then
    begin
      raise ENyxCollection.Create('Collection control does not allow editing');
    end;
    LView.EditWire(AItem, AColumn, AWireValue);
    Result := True;
  except
    on LException: Exception do
    begin
      Failed(LException);
      Result := FFailure = nbfNotificationFailed;
    end;
  end;
  { A no-op value or rejected draft still needs normalization/restoration.
    Accepted notifications already synchronized the controls exactly once. }

  if FRefreshCount = LBefore then
  begin
    FNormalizeItem := AItem;
    FNormalizeColumn := AColumn;
    FNormalizeValues := False;

    if AItem.Defined then
    begin
      FNormalizeValues := AItem.Collection.Name = LView.Snapshot.Key.Name;
    end;
    try
      Refresh;
    finally
      FNormalizeValues := False;
      FNormalizeItem := Default(TNyxItemRef);
      FNormalizeColumn := -1;
    end;
  end;
end;

function TNyxCollectionMountBase.GetConnected: Boolean;
begin
  Result := FConnected;
end;

function TNyxCollectionMountBase.GetView: INyxCollectionView;
begin

  if not FConnected then
  begin
    raise ENyxCollection.Create('Collection control is disconnected');
  end;
  Result := FView;
end;

function TNyxCollectionMountBase.GetError: TNyxText;
begin
  Result := FError;
end;

function TNyxCollectionMountBase.GetFailure: TNyxBindingFailure;
begin
  Result := FFailure;
end;

function TNyxCollectionMountBase.GetRefreshCount: Integer;
begin
  Result := FRefreshCount;
end;

end.
