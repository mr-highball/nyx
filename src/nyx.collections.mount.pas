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
  nyx.editing, nyx.collections.edits,
  nyx.collections,
  nyx.collections.arrangement,
  nyx.collections.view, nyx.collections.inspection,
  nyx.collections.position,
  nyx.collections.recovery,
  nyx.collections.recovery.policy,
  nyx.collections.selection,
  nyx.collections.refresh,
  nyx.typeahead,
  nyx.events,
  nyx.publication,
  nyx.collections.query.controls,
  nyx.collections.arrangement.controls,
  nyx.collections.presentation,
  nyx.collections.presentation.controls,
  nyx.collections.arrangement.presenter,
  nyx.collections.clipboard.actions,
  nyx.collections.range,
  nyx.collections.range.actions,
  nyx.collections.range.commands,
  nyx.collections.range.navigation,
  nyx.collections.range.pointer,
  nyx.collections.range.stroke,
  nyx.collections.export.actions,
  nyx.collections.export,
  nyx.collections.cell.actions,
  nyx.collections.clipboard,
  nyx.clipboard,
  nyx.files.save,
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
    Whole-refresh shielding preserves Epoch. A failed physical refresh keeps
    Ready false until a successful retry, protecting an older visible row map.
    Saturation requires a fresh query
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

  { Optional UI-owner preparation for a newly configured command surface. This
    flags-only step synchronizes the mount's already accepted Enabled value and
    enables epoch tracking BEFORE callers reserve input or invoke foreign code.
    It performs no scheduler, model, widget or application callout. Later enabled
    transitions remain observable even on legacy tables without layout controls.
    Disconnected/retired owners refuse; preparation never installs staged input. }
  INyxCollectionInputPreparation = interface(IInterface)
    ['{C8B2A7F1-662E-42AE-B902-A73B64A148E1}']
    procedure PrepareInputTracking;
  end;

  { Source-compatible name for the portable editor value, independent of mounts. }
  TNyxCollectionCellEdit = nyx.collections.edits.TNyxCollectionCellEdit;
  { Source-compatible owned vector; the portable unit defines its value model. }
  TNyxCollectionCellEdits = nyx.collections.edits.TNyxCollectionCellEdits;

  { Optional runtime recovery access. Returned data has no mount/renderer/router
    backreference. Closing a physical target retires its writer before cleanup;
    already retained user work stays readable through independent data owners. }
  INyxCollectionDraftRecoveryMount = interface(IInterface)
    ['{7832DCFA-F357-47AC-A959-0479981480F8}']
    function GetDraftRecovery: INyxCollectionDraftRecovery;
  end;

  { Optional live mount status, separate from the journal's historical refusal
    receipt. Read/copy on UI; the interface retains its mount, while the copied
    Boolean grants no command or widget authority. Retired targets report false. }
  INyxCollectionDraftRecoveryStatus = interface(IInterface)
    ['{695EC5BD-92E1-4F73-A89F-5E5CFE70F6F6}']
    function GetDraftRecoveryHeld: Boolean;
    property DraftRecoveryHeld: Boolean read GetDraftRecoveryHeld;
  end;

  { UI-owner commands for the original refused draft, independently of journal
    capacity. Capture returns inert review data without hiding/focusing editors.
    Discard requires the same frame and exact text/caret/accepted value; a stale
    confirmation returns false. It never writes source or evicts saved entries.
    Successful discard does not claim Refresh succeeded. Retired mounts refuse. }
  INyxCollectionPendingDraftMount = interface(IInterface)
    ['{0D01B9F9-C392-472A-9010-0CE372DE0632}']
    function CapturePendingDraft: INyxPendingCollectionDraft;
    function DiscardPendingDraft(const AExpected: INyxPendingCollectionDraft): Boolean;
  end;
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
    Optional cell-range selection owns independent immutable generation facts;
    it never changes the view's ordinary row-selection owner. Explicit Tree
    viewport intent uses this same input incarnation and success-only refresh
    acknowledgement, without keeping physical callbacks in its request. UI-thread
    confinement is inherited from the view/store contract. Position recovery is
    a detached observation only; it cannot select rows or revive target input.
    Recovery review borrows its managed actions only during UI entry and hands
    queued presentation independent input flags rather than a mount callback.
    Automatic presenters are replaceable defaults; the first explicit assignment
    retains its original one-time meaning and never replaces journal ownership.
    Clipboard feedback is copied presentation state with a separately borrowed
    receiver, never the application's completion-observer slot. }
  TNyxCollectionMountBase = class(TInterfacedObject, INyxCollectionMount,
    INyxCollectionQueryMount, INyxCollectionInputAdmission,
    INyxCollectionInputPreparation, INyxCollectionClipboardFeedbackMount,
    INyxCollectionColumnArrangementMount, INyxCollectionClipboardMount,
    INyxCollectionCellActionsMount, INyxCollectionCellActionOptionsMount,
    INyxCollectionCellActionDefaultsMount,
    INyxCollectionExportMount, INyxCollectionCellRangeMount,
    INyxCollectionCellRangeKeyInput, INyxCollectionCellRangePointerInput,
    INyxCollectionCellRangeStrokeInput, INyxCollectionPositionObservation,
    INyxCollectionDraftRecoveryMount, INyxCollectionDraftRecoveryStatus,
    INyxCollectionPendingDraftMount, INyxCollectionRecoveryReviewMount)
  private
    FToken: INyxCollectionViewSubscription;
    FPositionState: INyxCollectionPositionState;
    FPositionStateObject: TObject; { Borrowed while FPositionState retains it. }
    FRecoveryWriter: INyxCollectionDraftRecoveryWriter;
    FRecoverySubscription: INyxCollectionSubscription;
    { Captured authored defaults apply only when this mount creates its own
      journal. Later runtime overrides are never reset by source refreshes. }
    FRecoveryPolicy: TNyxCollectionDraftRecoveryPolicy;
    { Inert recovery scope persists across failed target passes and is released
      after a successful install. Journal copies retain no reference to us. }
    FRecoveryFrame: TNyxDraftRecoveryFrame;
    { Coordinates only: rejected text remains solely in the physical target.
      A review request copies that current value through the target adapter. }
    FRecoveryPendingItem: TNyxItemRef;
    FRecoveryPendingField: TNyxColumnIdentity;
    FRecoveryPendingFrame: TNyxDraftRecoveryFrame;
    { Evidence for the still-physical frame across deferred/held refreshes.
      Only bounded removed IDs and revision scalars survive notifications; no
      change log, producer snapshot, callback or second collection is retained.
      Successful refresh/retirement releases it. Gaps/overflow stay unknown. }
    FRecoveryContinuity: INyxCollectionDisplayContinuity;
    FRecoveryRemoved: TNyxItemRefs;
    FRecoveryIdentityLimit: Integer;
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
    FClipboardFeedback: TNyxCollectionClipboardFeedback;
    FClipboardFeedbackEnabled: Boolean;
    procedure ClipboardFeedbackReceived(const AFeedback: TNyxCollectionClipboardFeedback);
  private
    { Range enablement may read a foreign view; recursive configuration cannot
      replace a newer mode while the original entry is still inspecting it. }
    FConfiguringCellRange: Boolean;
    { Export authority is separately injected; reentrant configuration cannot
      replace the original candidate while its scheduler/host is inspected. }
    FConfiguringExport: Boolean;
    { A candidate context presenter cannot recursively replace the exact
      factory/controller being configured across scheduler admission. }
    FConfiguringCellActions: Boolean;
    FCellFactory: INyxCollectionCellPresenterFactory;
    { Default controllers can be rebuilt with newly admitted services without
      revoking their shared target factory. Every controller gets a private,
      revocable factory lease; this mount alone owns the underlying factory. }
    FCellActionsDefault: Boolean;
    FCellDefaultScheduler: INyxScheduler;
    function NewCellActionsCandidate(
      const AFactory: INyxCollectionCellPresenterFactory;
      const AScheduler: INyxScheduler;
      const AOptions: TNyxCollectionCellActionOptions;
      const AClipboard: INyxCollectionClipboardCommands;
      const AExports: INyxCollectionExportCommands): INyxCollectionCellActions;
    procedure ConfigureCellActionsCore(
      const AFactory: INyxCollectionCellPresenterFactory;
      const AScheduler: INyxScheduler;
      const AOptions: TNyxCollectionCellActionOptions; ADefault: Boolean);
    { Runs after flags-only adoption. Cleanup failures are mount diagnostics,
      never a false admission failure or permission to roll back a new owner. }
    procedure RetireAdoptedCellActions(const APrevious: INyxCollectionCellActions;
      const AFactory: INyxCollectionCellPresenterFactory = nil);
  private
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
    { Explicit command preparation keeps legacy mounts' later interaction
      epochs and whole-refresh shields active before any command is installed. }
    FInputTrackingPrepared: Boolean;
    function SearchLabel(AIndex: Integer): TNyxText;
    procedure DisconnectInternal;
    procedure CaptureRecoveryContinuity(const AChanges: INyxCollectionChanges);
    procedure Changed(const AView: INyxCollectionView;
      const AChanges: INyxCollectionChanges);
  protected
    FView: INyxCollectionView;
    FPositionRecovery: TNyxCollectionPositionRecovery;
    FFollowRequestSerial: Integer;
    { Exact view facet only; it owns no physical target. Admission is sampled
      before the refresh shield. Success alone acknowledges a physical request. }
    FTreeReveal: INyxTreeReveal;
    FTreeRevealSerial: Integer;
    FPendingTreeRevealSerial: Integer;
    FTreeRevealEpoch: Integer;
    FTreeRevealAllowed: Boolean;
    FPendingFollowRequestSerial: Integer;
    FUpdating: Boolean;
    { Capacity refusal deliberately holds the old physical frame. Deferred
      target queues must not busy-retry it; source updates/explicit Refresh may
      still attempt admission. Clear only after a successful installation. }
    FDraftRecoveryHeld: Boolean;
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
    { One mount entry reservation spans source/page/column getters before the
      detached selection owner can reserve its own complete candidate. }
    FCellRangeRequest: Integer;
    FEnabled: Boolean;
    FReadOnly: Boolean;
    FQueries: INyxCollectionQueryWorkbench;
    FColumnManager: INyxColumnArrangementManager;
    { Controller owns model/runtime authority only. Per-command synchronous
      fences may retain this mount; asynchronous host scopes never do. }
    FClipboardCommands: INyxCollectionClipboardCommands;
    { Range mode is off by default. Only detached range intent and canonical
      cancel flags belong here; the state retains no mount/view callback. }
    FCellRangeEnabled: Boolean;
    FCellRanges: INyxCollectionCellRangeSelection;
    { Pending gesture ownership is detached data plus flags only. No message
      entry, target callback or widget is retained between physical events. }
    FCellStroke: INyxCollectionRangeStroke;
    FCellStrokeScope: INyxCancellationScope;
    FCellStrokePointer: TNyxCollectionRangeStrokePointer;
    FCellStrokePreview: INyxCollectionCellRangeCapture;
    FClipboardOptions: TNyxCollectionClipboardOptions;
    FExportCommands: INyxCollectionExportCommands;
    FCellActions: INyxCollectionCellActions;
    FClipboardFields: TNyxCollectionClipboardFields;
    { Cached canonical presentation, independently of row/query snapshots.
      Legacy descriptors keep their old geometry; only explicit layout choices
      acquire the optional view facet. Targets never query it while painting. }
    FHasColumnLayout: Boolean;
    FColumnLayout: TNyxColumnLayoutObservation;
    { A separate copied cache keeps text/density opt-in independent of width,
      pinning and arrangement controls. Runtime work limits never enter design. }
    FHasTablePresentation: Boolean;
    { A supporting table can observe explicit runtime presentation before any
      authored or runtime choice is active. This is capability, not opt-in;
      older alternative views keep their unchanged ordinary path. }
    FObservesTablePresentation: Boolean;
    FTablePresentation: TNyxTablePresentationObservation;
    FTextLimits: TNyxTableTextWorkLimits;
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
    { Presentation only, using cached immutable range membership. Target
      overrides must not refresh/validate editors or query the live dataset. }
    procedure RenderCellRange; virtual;
    function CellRangeAvailable: Boolean; virtual;
    function CellRangeContains(const AItem: TNyxItemRef;
      const AField: TNyxColumnIdentity): Boolean;
    { Reserve before an adapter reads its current row. The returned command is
      synchronous and owns this mount only through canonical key completion. }
    function BeginCellRangeKey(AKey: TNyxCollectionRangeKey): INyxCollectionCellRangeKeyCommand;
    function BeginCellRangePointer(AAction: TNyxCollectionRangePointerAction):
      INyxCollectionCellRangePointerEntry;
    procedure CancelCellRangeStroke;
    { Authority and cached preview have already been withdrawn. A target may
      release THIS subject's physical capture and restore cached accepted paint;
      it must not adopt a newer gesture or query a live dataset. Destruction
      also calls this hook with retired input, so never resurrect that mount. }
    procedure RetireCellRangeStroke(const AStroke: INyxCollectionRangeStroke); virtual;
    { Move only physical focus using the already admitted row/canonical field.
      Implementations suppress ordinary row-selection/editor side effects and
      return after any focus callbacks; the command then repeats its final fence. }
    function FocusCellRange(ARow, AColumn: Integer): Boolean; virtual;
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
    { Bounded membership notification updates presentation only. A target may
      defer while its existing physical input frame is busy; never Refresh the
      view or change source/selection merely to update a journal caption. }
    procedure RecoveryMembershipChanged(const AObservation: TNyxDraftRecoveryObservation); virtual;
    { Presentation only. Prepare stable target space at service admission;
      later outcomes change text without row refresh, focus or input epochs.
      Exceptions become mount diagnostics and cannot swallow caller completion. }
    procedure ClipboardFeedbackChanged; virtual;
    function HasClipboardFeedback: Boolean;
    { Admit target observation before the first authored/runtime presentation
      pass. A target may refuse unsupported direct mounts; failure does not
      claim activation or make an old physical surface authoritative. }
    procedure PrepareTablePresentation; virtual;
    { Recovery-only physical access bypasses stale source input admission, never
      writes source, and must recheck exact data before the final target change.
      Alternatives without editors retain these refusing defaults. }
    function ReadRecoveryDraft(const AItem: TNyxItemRef;
      const AField: TNyxColumnIdentity; out ACell: TNyxCollectionCellEdit): Boolean; virtual;
    function DiscardRecoveryDraft(const ACell: TNyxCollectionCellEdit): Boolean; virtual;
    { Flags/weak-receiver retirement only, before query/observer cleanup can
      enter external code. Actual target API/hook cleanup remains DetachTarget. }
    procedure RetireTargetInput; virtual;
    { Call BEFORE overwriting/removing captured physical text. AHasDraft=False
      withdraws incompatible context without journaling an untouched value.
      True means old automatic compatibility is withdrawn. }
    function RecoverCell(const ACell: TNyxCollectionCellEdit;
      const ABefore, AAfter: INyxCollectionSnapshot;
      out ALoss: TNyxCollectionDraftLoss; AHasDraft: Boolean = True): Boolean;
    { One target-specific incompatibility may be known only after a host refuses
      restoration. Reuse the SAME journal/frame/pressure admission; callers keep
      the original draft until this method returns and recheck their lifetime. }
    procedure PreserveCell(const ACell: TNyxCollectionCellEdit;
      AReason: TNyxCollectionDraftLoss; ABeforeRevision, AAfterRevision: Integer);
    function ResolvePosition(const ABefore, AAfter: INyxCollectionSnapshot;
      AIndex: Integer): TNyxCollectionPositionRecovery;
    { Capture current intent with retained view/lifetime checks. Pending excludes
      consumed requests; Current also qualifies browser measurement corrections.
      No target callback, selection or query mutation occurs here. }
    function CurrentTreeReveal: TNyxTreeRevealRequest;
    function PendingTreeReveal: TNyxTreeRevealRequest;
    procedure TreeRevealApplied(ASerial: Integer);
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
    procedure PrepareInputTracking;
    procedure InstallInput;
    procedure Disconnect;
    procedure Refresh;
    function GetPositionRecovery: TNyxCollectionPositionRecovery;
    function GetDraftRecovery: INyxCollectionDraftRecovery;
    function GetDraftRecoveryHeld: Boolean;
    function CapturePendingDraft: INyxPendingCollectionDraft;
    function DiscardPendingDraft(const AExpected: INyxPendingCollectionDraft): Boolean;
    function CapturePositionState: INyxCollectionPositionState;
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
    function GetClipboardFeedback: TNyxCollectionClipboardFeedback;
    function GetClipboardFeedbackEnabled: Boolean;
    procedure SetClipboardFeedbackEnabled(AEnabled: Boolean);
    procedure ConfigureCellRange(AEnabled: Boolean);
    function SelectCells(const AAnchor, AFocus: TNyxCollectionCellIdentity): Boolean;
    procedure ClearCellRange;
    function GetCellRange: INyxCollectionCellRangeIntent;
    function BeginCellRangeStroke(APhase: TNyxCollectionRangeStrokePhase;
      const APointer: TNyxCollectionRangeStrokePointer;
      AAction: TNyxCollectionRangePointerAction;
      const AStroke: INyxCollectionRangeStroke = nil): INyxCollectionCellRangeStrokeEntry;
    procedure AbandonCellRangeStroke(const APointer: TNyxCollectionRangeStrokePointer;
      const AStroke: INyxCollectionRangeStroke);
    procedure ConfigureExport(const AHost: INyxFileSaveHost;
      const AEvents: INyxEvents; const AScheduler: INyxScheduler;
      const AOptions: TNyxCollectionExportOptions);
    function GetExportCommands: INyxCollectionExportCommands;
    procedure ConfigureCellActions(const AFactory: INyxCollectionCellPresenterFactory;
      const AScheduler: INyxScheduler);
    procedure ConfigureCellActionsWithOptions(
      const AFactory: INyxCollectionCellPresenterFactory;
      const AScheduler: INyxScheduler;
      const AOptions: TNyxCollectionCellActionOptions);
    procedure ConfigureDefaultCellActions(
      const AFactory: INyxCollectionCellPresenterFactory;
      const AScheduler: INyxScheduler);
    function GetCellActions: INyxCollectionCellActions;
    function GetRecoveredEditCount: Integer;
    function OpenRecoveredEdits: Boolean;
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
{ Capture only the independent data journal, releasing its observing mount
  before return. Unsupported alternatives return nil. UI-owner operation. }
function NyxCollectionDraftRecovery(const AOwner: IInterface): INyxCollectionDraftRecovery;
{ False for retired targets or alternative mounts without this optional facet.
  Unlike LastRefusal, this reports a physical frame still held right now. }
function NyxCollectionDraftRecoveryHeld(const AOwner: IInterface): Boolean;
function NewNyxCollectionInputGate(
  const AParent: INyxCollectionInputGate = nil): INyxCollectionInputGate;
{ A callback-free final fence for an actual prepared UI command. Entry checks
  alone cannot stop a validator retiring its source gate before installation. }
function PrepareNyxCollectionInput(
  const AInput: INyxCollectionInputAdmission): INyxPreparedPublication;

implementation

uses
  nyx.text.index, nyx.model, nyx.root.types, nyx.menu, nyx.popover,
  nyx.collections.view.types, nyx.collections.query,
  nyx.collections.query.session,
  nyx.collections.query.workbench,
  nyx.collections.arrangement.workbench, nyx.collections.arrangement.manager,
  nyx.collections.clipboard.commands, nyx.collections.export.commands
  {$ifdef PAS2JS}, JS{$endif};

type
  { One controller's revocable view of a mount-owned target factory. Revoking
    this lease only withdraws its own future calls; it never revokes the shared
    DOM/LCL anchor. The mount retires the underlying factory after its last
    admitted controller. Entered calls retain their original factory locally. }
  TCellFactoryLease = class(TInterfacedObject, INyxCollectionCellPresenterFactory)
  private
    FFactory: INyxCollectionCellPresenterFactory;
  public
    constructor Create(const AFactory: INyxCollectionCellPresenterFactory);
    function CreateMenu(ADocument: TNyxDocument; const ARoot: TNyxRootRef;
      const AItems: TNyxMenuItems): INyxMenu;
    function CreateInspector(ADocument: TNyxDocument;
      const ARoot: TNyxRootRef): INyxPopover;
    procedure Revoke;
  end;

  { One data-only port can outlive the physical target. Its writer belongs
    exclusively to the mount; readonly clients cannot obtain an owner callback. }
  TPositionState = class(TInterfacedObject, INyxCollectionPositionState)
  public
    Active: Boolean;
    Recovery: TNyxCollectionPositionRecovery;
    function GetActive: Boolean;
    function GetRecovery: TNyxCollectionPositionRecovery;
  end;

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

  { Original mounted range input. Keys and pointer endpoints share reservation,
    original input fences, target focus and the existing selection owner.
    Page geometry is a copied scalar resolved once after that reservation;
    missing/malformed geometry cannot fall back to single-row selection.
    Prepared selection and copied snapshot leases
    cannot outlive this synchronous envelope; the accepted range owns none of
    these live mount/view references. Flags-only checks follow every callout. }
  TCellRangeKeyCommand = class(TInterfacedObject, INyxCollectionCellRangeKeyCommand,
    INyxCollectionCellRangePointerEntry, INyxCollectionCellRangePointerReceipt,
    INyxCollectionCellRangePageKeyCommand)
  private
    FOwner: TNyxCollectionMountBase;
    FLease: INyxCollectionMount;
    FView: INyxCollectionView;
    FInput: INyxCollectionInputLifetime;
    FSelection: INyxCollectionCellRangeSelection;
    FPrevious: INyxCollectionCellRangeIntent;
    FKey: TNyxCollectionRangeKey;
    FPageRows: Integer;
    FPageRowsResolved: Boolean;
    FOrigin: TNyxCollectionCellIdentity;
    FTarget: TNyxCollectionCellIdentity;
    FPointer: Boolean;
    FPointerAction: TNyxCollectionRangePointerAction;
    FPointerPlan: INyxCollectionCellRangePointerCommand;
    FOptions: TNyxCollectionClipboardOptions;
    FPlan: INyxPreparedPublication;
    FReady: INyxPublicationReadiness;
    FEpoch: Integer;
    FMutation: Integer;
    FRequest: Integer;
    FRow: Integer;
    FColumn: Integer;
    FAdmitted: Boolean;
    FResolved: Boolean;
    FPrepared: Boolean;
    FCompleted: Boolean;
    FInstalled: Boolean;
    FRetired: Boolean;
    procedure CaptureOwner(AOwner: TNyxCollectionMountBase; AKeepStroke: Boolean = False);
    function PreparedCurrent: Boolean;
  public
    constructor Create(AOwner: TNyxCollectionMountBase; AKey: TNyxCollectionRangeKey);
    constructor CreatePointer(AOwner: TNyxCollectionMountBase;
      AAction: TNyxCollectionRangePointerAction);
    constructor CreateStrokeGuard(AOwner: TNyxCollectionMountBase; AKeepStroke: Boolean);
    destructor Destroy; override;
    procedure ResolveOrigin(const AOrigin: TNyxCollectionCellIdentity);
    procedure ResolvePageRows(APageRows: Integer);
    procedure ResolveCells(const AOrigin, ATarget: TNyxCollectionCellIdentity);
    procedure Prepare;
    function GetCurrent: Boolean;
    function GetDispatchCurrent: Boolean;
    function GetInstalled: Boolean;
    function Complete(AAllowDefault: Boolean): Boolean;
    procedure Retire;
  end;

  { Readiness retains canonical input flags and a cancellation flag, never its
    mount. This is safe for the detached pending stroke after message retirement;
    epoch ABA and ancestor retirement cannot revive its original admission. }
  TCellStrokeReadiness = class(TInterfacedObject, INyxCancellationReadiness)
  public
    Input: INyxCollectionInputLifetime;
    Scope: INyxCancellationScope;
    Epoch: Integer;
    procedure CheckReady;
  end;

  { Recovery may remain readable after source membership changes, but its UI
    owner must still be this exact mounted input incarnation. This proof owns
    canonical flags and epoch only, never a view, mount or physical control. }
  TRecoveryReviewReadiness = class(TInterfacedObject, INyxCancellationReadiness)
  public
    Input: INyxCollectionInputLifetime;
    Epoch: Integer;
    procedure CheckReady;
  end;

  { One message delegates original mounted reservation to the existing guard.
    Its owned snapshots and guard cannot escape into the detached stroke. Focus
    runs only at begin/release, and every outward read repeats that SAME guard.
    Claimed completion stays entered through cleanup: explicit callback-driven
    retirement abandons it without resuming a borrowed receiver tail. }
  TCellStrokeEntry = class(TInterfacedObject, INyxCollectionCellRangePointerEntry,
    INyxCollectionCellRangeStrokeEntry)
  private
    FGuard: TCellRangeKeyCommand;
    FGuardLease: INyxCollectionCellRangePointerEntry;
    FPhase: TNyxCollectionRangeStrokePhase;
    FPointer: TNyxCollectionRangeStrokePointer;
    FAction: TNyxCollectionRangePointerAction;
    FStroke: INyxCollectionRangeStroke;
    FScope: INyxCancellationScope;
    FSource: INyxCollectionSnapshot;
    FPage: INyxCollectionSnapshot;
    FFields: TNyxCollectionClipboardFields;
    FSerial: Integer;
    FReady: Boolean;
    FCompleted: Boolean;
    FCompleting: Boolean;
    FRetired: Boolean;
    function Facts(const AFocus: TNyxCollectionCellIdentity): TNyxCollectionRangeStrokeInput;
  public
    constructor Create(AOwner: TNyxCollectionMountBase;
      APhase: TNyxCollectionRangeStrokePhase;
      const APointer: TNyxCollectionRangeStrokePointer;
      AAction: TNyxCollectionRangePointerAction;
      const AStroke: INyxCollectionRangeStroke);
    destructor Destroy; override;
    procedure ResolveCells(const AOrigin, ATarget: TNyxCollectionCellIdentity);
    procedure Prepare;
    function GetCurrent: Boolean;
    function GetDispatchCurrent: Boolean;
    function Complete(AAllowDefault: Boolean): Boolean;
    function GetPendingStroke: INyxCollectionRangeStroke;
    procedure Retire;
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

constructor TCellFactoryLease.Create(const AFactory: INyxCollectionCellPresenterFactory);
begin
  inherited Create;
  FFactory := AFactory;
end;

function TCellFactoryLease.CreateMenu(ADocument: TNyxDocument;
  const ARoot: TNyxRootRef; const AItems: TNyxMenuItems): INyxMenu;
var
  LFactory: INyxCollectionCellPresenterFactory;
begin
  LFactory := FFactory;

  if LFactory = nil then
  begin
    raise ENyxCollectionClipboard.Create('The original cell presenter lease is retired');
  end;
  Result := LFactory.CreateMenu(ADocument, ARoot, AItems);
end;

function TCellFactoryLease.CreateInspector(ADocument: TNyxDocument;
  const ARoot: TNyxRootRef): INyxPopover;
var
  LFactory: INyxCollectionCellPresenterFactory;
begin
  LFactory := FFactory;

  if LFactory = nil then
  begin
    raise ENyxCollectionClipboard.Create('The original cell presenter lease is retired');
  end;
  Result := LFactory.CreateInspector(ADocument, ARoot);
end;

procedure TCellFactoryLease.Revoke;
begin
  FFactory := nil;
end;

procedure TCellStrokeReadiness.CheckReady;
begin

  if (Scope = nil) or Scope.Cancelled or (Input = nil) or not Input.Ready or
    (Input.Epoch <> Epoch) then
  begin
    raise ENyxSchedule.Create('The original mounted range stroke is no longer ready');
  end;
end;

constructor TCellStrokeEntry.Create(AOwner: TNyxCollectionMountBase;
  APhase: TNyxCollectionRangeStrokePhase;
  const APointer: TNyxCollectionRangeStrokePointer;
  AAction: TNyxCollectionRangePointerAction;
  const AStroke: INyxCollectionRangeStroke);
begin
  inherited Create;
  FPhase := APhase;
  FPointer := APointer;
  FAction := AAction;
  FStroke := AStroke;
  FPointer.Validate;

  if (Ord(FPhase) < Ord(Low(TNyxCollectionRangeStrokePhase))) or
    (Ord(FPhase) > Ord(High(TNyxCollectionRangeStrokePhase))) or
    (Ord(FAction) < Ord(Low(TNyxCollectionRangePointerAction))) or
    (Ord(FAction) > Ord(High(TNyxCollectionRangePointerAction))) then
  begin
    raise ENyxCollectionRange.Create('Mounted stroke phase or action is invalid');
  end;
  FGuard := TCellRangeKeyCommand.CreateStrokeGuard(AOwner, FPhase <> ncrspBegin);
  FGuardLease := FGuard;

  if FPhase = ncrspBegin then
  begin
    FStroke := nil;

    if FGuard.GetCurrent then
    begin
      FScope := NewNyxCancellationScope;
      AOwner.FCellStrokeScope := FScope;
      AOwner.FCellStrokePointer := FPointer;
    end;
  end
  else
  begin
    FScope := AOwner.FCellStrokeScope;
  end;
end;

destructor TCellStrokeEntry.Destroy;
begin
  try
    Retire;
  except
    { Explicit retirement reports failure; final release must still unwind. }
  end;
  inherited Destroy;
end;

function TCellStrokeEntry.GetDispatchCurrent: Boolean;
begin
  Result := not FRetired and (FGuard <> nil) and FGuard.GetDispatchCurrent;
end;

function TCellStrokeEntry.GetCurrent: Boolean;
begin
  Result := not FRetired and (FGuard <> nil) and FGuard.GetCurrent and
    (FScope <> nil) and not FScope.Cancelled and
    (FGuard.FOwner.FCellStrokeScope = FScope) and
    FGuard.FOwner.FCellStrokePointer.Same(FPointer) and
    ((FPhase = ncrspBegin) or ((FStroke <> nil) and
      (FGuard.FOwner.FCellStroke = FStroke)));
end;

procedure TCellStrokeEntry.ResolveCells(const AOrigin,
  ATarget: TNyxCollectionCellIdentity);
begin

  if GetCurrent then
  begin
    FGuard.ResolveCells(AOrigin, ATarget);
  end;
end;

procedure TCellStrokeEntry.Prepare;
var
  LLease: INyxCollectionCellRangeStrokeEntry;
  LStore: INyxCollection;
  LInspection: INyxCollectionInspection;
  LSource: INyxCollectionSnapshot;
  LPage: INyxCollectionSnapshot;
  LColumns: INyxCollectionColumns;
  LCapture: INyxCollectionColumnsCapture;
  LObservation: TNyxColumnLayoutObservation;
  LFields: TNyxCollectionClipboardFields;
  LFocus: TNyxCollectionCellIdentity;
  LIndex: Integer;
begin
  LLease := Self;

  if not GetCurrent or FGuard.FPrepared or not FGuard.FResolved then
  begin
    Exit;
  end;
  FGuard.FPrepared := True;
  LStore := FGuard.FView.Store;

  if not GetCurrent or (LStore = nil) then
  begin
    Exit;
  end;
  LInspection := NyxCollectionInspection(FGuard.FView);

  if not GetCurrent then
  begin
    Exit;
  end;

  if LInspection <> nil then
  begin

    if LInspection.CaptureInspection.Mode <> ncimPaused then
    begin
      LInspection := nil;
    end;
  end;

  if not GetCurrent then
  begin
    Exit;
  end;

  if LInspection <> nil then
  begin
    LSource := LInspection.GetDisplayedSource;
  end
  else
  begin
    LSource := LStore.Snapshot;
  end;

  if not GetCurrent or (LSource = nil) then
  begin
    Exit;
  end;
  LPage := FGuard.FView.Snapshot;

  if not GetCurrent or (LPage = nil) then
  begin
    Exit;
  end;
  LColumns := NyxCollectionColumns(FGuard.FView);

  if not GetCurrent then
  begin
    Exit;
  end;
  LCapture := LColumns.CaptureColumns;

  if not GetCurrent then
  begin
    Exit;
  end;
  LObservation := TNyxColumnLayoutObservation.Read(LCapture);
  LFields := NyxCollectionClipboardFields(LObservation, nccfVisiblePresentation);

  if not GetCurrent then
  begin
    Exit;
  end;
  LFocus := FGuard.FOrigin;

  if FPhase = ncrspRelease then
  begin
    LFocus := FGuard.FTarget;
  end;
  FGuard.FRow := LPage.IndexOf(LFocus.Item);

  if not GetCurrent then
  begin
    Exit;
  end;
  for LIndex := 0 to High(FGuard.FOwner.FClipboardFields) do
  begin

    if FGuard.FOwner.FClipboardFields[LIndex].Same(LFocus.Field) then
    begin
      FGuard.FColumn := LIndex;
      Break;
    end;
  end;

  if (FGuard.FRow < 0) or (FGuard.FColumn < 0) then
  begin
    Exit;
  end;
  FSource := LSource;
  FPage := LPage;
  FFields := LFields;
  FSerial := LObservation.Serial;
  FReady := GetCurrent;
end;

function TCellStrokeEntry.Facts(const AFocus: TNyxCollectionCellIdentity):
  TNyxCollectionRangeStrokeInput;
begin
  Result := NyxCollectionRangeStrokeInput(GetCurrent, FGuard.FEpoch,
    FGuard.FMutation, FSerial, AFocus);
end;

function TCellStrokeEntry.GetPendingStroke: INyxCollectionRangeStroke;
begin
  Result := nil;

  if not FRetired then
  begin
    Result := FStroke;
  end;
end;

function TCellStrokeEntry.Complete(AAllowDefault: Boolean): Boolean;
var
  LLease: INyxCollectionCellRangeStrokeEntry;
  LReadinessObject: TCellStrokeReadiness;
  LReadiness: INyxCancellationReadiness;
  LRelease: INyxCollectionRangeStrokeRelease;
  LNewStroke: INyxCollectionRangeStroke;
  LOwner: TNyxCollectionMountBase;
  LBodyFailed: Boolean;
begin
  LLease := Self;
  Result := True;

  if FCompleted or FRetired then
  begin
    Exit;
  end;
  FCompleted := True;

  if not GetCurrent then
  begin
    Exit;
  end;
  LOwner := FGuard.FOwner;
  LBodyFailed := False;
  FCompleting := True;
  try
    try

      if AAllowDefault and FReady then
      begin

        if FPhase = ncrspBegin then
        begin

          if not LOwner.FocusCellRange(FGuard.FRow, FGuard.FColumn) or not GetCurrent then
          begin
            Exit;
          end;
          LReadinessObject := TCellStrokeReadiness.Create;
          LReadiness := LReadinessObject;
          LReadinessObject.Input := FGuard.FInput;
          LReadinessObject.Scope := FScope;
          LReadinessObject.Epoch := FGuard.FEpoch;
          LNewStroke := NewNyxCollectionRangeStroke(FGuard.FSelection, LReadiness,
            FPointer, FSource, FPage, FFields, FGuard.FOrigin, FGuard.FTarget,
            FAction, Facts(FGuard.FOrigin), FGuard.FOptions.IncludeHeaders,
            FGuard.FOptions.Lines, FGuard.FOptions.WorkLimits);

          if not GetCurrent or not LNewStroke.Current then
          begin
            LNewStroke.Retire;
            Exit;
          end;
          FStroke := LNewStroke;
          LOwner.FCellStroke := FStroke;
        end
        else
        begin

          if not FStroke.Stage(FPointer, FSource, FPage, Facts(FGuard.FOrigin),
            FGuard.FTarget) or not GetCurrent then
          begin
            Exit;
          end;

          if FPhase = ncrspRelease then
          begin
            LRelease := FStroke.PrepareRelease(FPointer, FSource, FPage,
              Facts(FGuard.FOrigin));
            try

              if (LRelease <> nil) and GetCurrent and
                LOwner.FocusCellRange(FGuard.FRow, FGuard.FColumn) and GetCurrent then
              begin
                LRelease.Complete(True, FSource, FPage, Facts(FGuard.FTarget));
              end;
            finally

              if LRelease <> nil then
              begin
                LRelease.Retire;
              end;
            end;
            Exit;
          end;
        end;
        LOwner.FCellStrokePreview := FStroke.Preview;
      end;
    except
      LBodyFailed := True;
      raise;
    end;
  finally
    try
      try
        { An abandoned/consumed message withdraws only its own current pending
          owner. A newer nested message's request and preview remain untouched. }
        if (FGuard <> nil) and FGuard.GetCurrent and (LOwner.FCellStrokeScope = FScope) then
        begin

          if not AAllowDefault or not FReady or (FStroke = nil) or
            not FStroke.Current or (FPhase = ncrspRelease) then
          begin
            LOwner.CancelCellRangeStroke;
          end;

          if (FGuard <> nil) and FGuard.GetCurrent then
          begin
            LOwner.RenderCellRange;
          end;
        end;
      except
        { Cleanup still withdraws pending authority. Preserve a prior Pascal or
          raw browser focus/canonical failure if painting also refuses. }
        if not LBodyFailed then
        begin
          raise;
        end;
      end;
    finally
      FCompleting := False;
    end;
  end;
end;

procedure TCellStrokeEntry.Retire;
var
  LGuard: INyxCollectionCellRangePointerEntry;
  LOwner: TNyxCollectionMountBase;
  LCancel: Boolean;
  LStroke: INyxCollectionRangeStroke;
  LScope: INyxCancellationScope;
  LSource: INyxCollectionSnapshot;
  LPage: INyxCollectionSnapshot;
begin

  if FRetired then
  begin
    Exit;
  end;

  LCancel := (not FCompleted or FCompleting) and GetCurrent;
  LOwner := nil;

  if FGuard <> nil then
  begin
    LOwner := FGuard.FOwner;
  end;
  FRetired := True;
  LGuard := FGuardLease;
  LStroke := FStroke;
  LScope := FScope;
  LSource := FSource;
  LPage := FPage;
  FGuardLease := nil;
  FGuard := nil;
  FStroke := nil;
  FScope := nil;
  FSource := nil;
  FPage := nil;
  FFields := nil;
  { No Self access follows the first outward retirement. This also works from
    Destroy without resurrecting a zero-reference interface receiver. }

  if LCancel then
  begin
    LOwner.CancelCellRangeStroke;

    if LGuard.Current then
    begin
      LOwner.RenderCellRange;
    end;
  end;

  if LGuard <> nil then
  begin
    LGuard.Retire;
  end;
end;

constructor TCellRangeKeyCommand.Create(AOwner: TNyxCollectionMountBase;
  AKey: TNyxCollectionRangeKey);
begin
  inherited Create;
  FKey := AKey;
  CaptureOwner(AOwner);
end;

constructor TCellRangeKeyCommand.CreatePointer(AOwner: TNyxCollectionMountBase;
  AAction: TNyxCollectionRangePointerAction);
begin
  inherited Create;
  FPointer := True;
  FPointerAction := AAction;
  CaptureOwner(AOwner);
end;

constructor TCellRangeKeyCommand.CreateStrokeGuard(AOwner: TNyxCollectionMountBase;
  AKeepStroke: Boolean);
begin
  inherited Create;
  FPointer := True;
  CaptureOwner(AOwner, AKeepStroke);
end;

procedure TCellRangeKeyCommand.CaptureOwner(AOwner: TNyxCollectionMountBase;
  AKeepStroke: Boolean);
var
  LHadPreview: Boolean;
begin
  FOwner := AOwner;
  FLease := AOwner as INyxCollectionMount;
  FView := AOwner.FView;
  FInput := AOwner.FInputLifetime;
  FSelection := AOwner.FCellRanges;
  FMutation := AOwner.FCellMutationSerial;
  FOptions := AOwner.FClipboardOptions;
  FRow := -1;
  FColumn := -1;

  if FInput <> nil then
  begin
    FEpoch := FInput.Epoch;
  end;
  { Even an inert reservation remains claimed. A later default must not borrow
    a new epoch after a getter/callback disables and re-enables this mount. }

  if AOwner.FCellRangeRequest < High(Integer) then
  begin
    Inc(AOwner.FCellRangeRequest);
    FRequest := AOwner.FCellRangeRequest;
  end;

  if not AKeepStroke then
  begin
    LHadPreview := AOwner.FCellStrokePreview <> nil;
    AOwner.CancelCellRangeStroke;

    if LHadPreview and AOwner.FConnected and not AOwner.FDisconnecting then
    begin
      { Even a consumed legacy key/endpoint must remove the abandoned preview.
        This port paints cached membership only; it reads no source or editor. }
      AOwner.RenderCellRange;
    end;
  end;
  FAdmitted := (FRequest > 0) and (FRequest < High(Integer)) and
    AOwner.FConnected and not AOwner.FDisconnecting and AOwner.FCellRangeEnabled and
    AOwner.FEnabled and not AOwner.FUpdating and not AOwner.FEditsSuspended and
    (FInput <> nil) and (FEpoch > 0) and (FEpoch < High(Integer)) and FInput.Ready and
    (FSelection <> nil) and AOwner.ClipboardInputAvailable;

  if FSelection <> nil then
  begin
    FPrevious := FSelection.Selection;
  end;
end;

destructor TCellRangeKeyCommand.Destroy;
begin
  try
    Retire;
  except
    { Explicit retirement reports failure. Final release still frees fields. }
  end;
  inherited Destroy;
end;

function TCellRangeKeyCommand.GetDispatchCurrent: Boolean;
begin
  Result := not FRetired and FAdmitted and (FOwner <> nil) and
    FOwner.FConnected and not FOwner.FDisconnecting and
    (FOwner.FView = FView) and (FOwner.FInputLifetime = FInput) and FInput.Current;
end;

function TCellRangeKeyCommand.GetInstalled: Boolean;
begin
  Result := FInstalled and not FRetired;
end;

function TCellRangeKeyCommand.GetCurrent: Boolean;
begin
  Result := GetDispatchCurrent and FOwner.FCellRangeEnabled and FOwner.FEnabled and
    not FOwner.FUpdating and not FOwner.FEditsSuspended and
    (FOwner.FCellRanges = FSelection) and (FMutation < High(Integer)) and
    (FOwner.FCellMutationSerial = FMutation) and
    (FOwner.FCellRangeRequest = FRequest) and FOwner.ClipboardInputAvailable and
    FInput.Ready and (FInput.Epoch = FEpoch);
end;

procedure TCellRangeKeyCommand.ResolveOrigin(const AOrigin: TNyxCollectionCellIdentity);
begin

  if FPointer or FResolved or FPrepared or not GetCurrent then
  begin
    Exit;
  end;
  FResolved := True;
  FOrigin := NyxCollectionCellIdentity(AOrigin.Item, AOrigin.Field);
end;

procedure TCellRangeKeyCommand.ResolvePageRows(APageRows: Integer);
begin

  if FPointer or FPageRowsResolved or FPrepared or not GetCurrent then
  begin
    Exit;
  end;
  FPageRowsResolved := True;

  if not (FKey in [ncrkExtendPageUp, ncrkExtendPageDown]) or (APageRows < 1) then
  begin
    raise ENyxCollectionRange.Create('Resolve a positive viewport step for a page-extension key');
  end;
  FPageRows := APageRows;
end;

procedure TCellRangeKeyCommand.ResolveCells(
  const AOrigin, ATarget: TNyxCollectionCellIdentity);
begin

  if not FPointer or FResolved or FPrepared or not GetCurrent then
  begin
    Exit;
  end;
  FResolved := True;
  FOrigin := NyxCollectionCellIdentity(AOrigin.Item, AOrigin.Field);
  FTarget := NyxCollectionCellIdentity(ATarget.Item, ATarget.Field);
end;

procedure TCellRangeKeyCommand.Prepare;
var
  LLease: INyxCollectionCellRangeKeyCommand;
  LStore: INyxCollection;
  LInspection: INyxCollectionInspection;
  LSource: INyxCollectionSnapshot;
  LPage: INyxCollectionSnapshot;
  LColumns: INyxCollectionColumns;
  LCapture: INyxCollectionColumnsCapture;
  LRangeCapture: INyxCollectionCellRangeCapture;
  LObservation: TNyxColumnLayoutObservation;
  LFields: TNyxCollectionClipboardFields;
  LNavigation: INyxCollectionRangeNavigation;
  LPreparation: INyxCollectionCellRangePreparation;
  LPointerPreparation: INyxCollectionCellRangePointerPreparation;
  LReady: INyxPublicationReadiness;
  LFocus: TNyxCollectionCellIdentity;
  LIndex: Integer;
begin
  LLease := Self;

  if FPrepared or not GetCurrent then
  begin
    Exit;
  end;
  FPrepared := True;

  if not FPointer and (FKey in [ncrkExtendPageUp, ncrkExtendPageDown]) and
    (FPageRows < 1) then
  begin
    Exit;
  end;

  if not FPointer and (FKey = ncrkClear) then
  begin

    if FPrevious = nil then
    begin
      Exit;
    end;
    LRangeCapture := FPrevious.Capture;

    if not GetCurrent or (LRangeCapture = nil) then
    begin
      Exit;
    end;
    LNavigation := NewNyxCollectionRangeNavigation(nil, nil, [], FOrigin,
      FPrevious, FKey, LRangeCapture.ColumnSerial, False, nccleLF,
      Default(TNyxCollectionClipboardLimits));
  end
  else
  begin

    if not FResolved then
    begin
      Exit;
    end;
    LStore := FView.Store;

    if not GetCurrent or (LStore = nil) then
    begin
      Exit;
    end;
    LInspection := NyxCollectionInspection(FView);

    if not GetCurrent then
    begin
      Exit;
    end;

    if LInspection <> nil then
    begin

      if LInspection.CaptureInspection.Mode <> ncimPaused then
      begin
        LInspection := nil;
      end;
    end;

    if not GetCurrent then
    begin
      Exit;
    end;

    if LInspection <> nil then
    begin
      LSource := LInspection.GetDisplayedSource;
    end
    else
    begin
      LSource := LStore.Snapshot;
    end;

    if not GetCurrent or (LSource = nil) then
    begin
      Exit;
    end;
    LPage := FView.Snapshot;

    if not GetCurrent or (LPage = nil) then
    begin
      Exit;
    end;
    LColumns := NyxCollectionColumns(FView);

    if not GetCurrent then
    begin
      Exit;
    end;
    LCapture := LColumns.CaptureColumns;

    if not GetCurrent then
    begin
      Exit;
    end;
    LObservation := TNyxColumnLayoutObservation.Read(LCapture);
    LFields := NyxCollectionClipboardFields(LObservation, nccfVisiblePresentation);

    if not GetCurrent then
    begin
      Exit;
    end;

    if FPointer then
    begin

      if not Supports(FSelection, INyxCollectionCellRangePointerPreparation,
        LPointerPreparation) then
      begin
        Exit;
      end;
      FPointerPlan := LPointerPreparation.PreparePointer(LSource, LPage,
        LFields, FOrigin, FTarget, FPrevious, FPointerAction, LObservation.Serial,
        FOptions.IncludeHeaders, FOptions.Lines, FOptions.WorkLimits);
      LFocus := FTarget;
    end
    else
    begin
      LNavigation := NewNyxCollectionRangeNavigation(LSource, LPage, LFields,
        FOrigin, FPrevious, FKey, LObservation.Serial, FOptions.IncludeHeaders,
        FOptions.Lines, FOptions.WorkLimits, FPageRows);
      LFocus := LNavigation.Focus;
    end;

    if not GetCurrent then
    begin
      Exit;
    end;
    FRow := LPage.IndexOf(LFocus.Item);

    if not GetCurrent then
    begin
      Exit;
    end;
    for LIndex := 0 to High(FOwner.FClipboardFields) do
    begin

      if FOwner.FClipboardFields[LIndex].Same(LFocus.Field) then
      begin
        FColumn := LIndex;
        Break;
      end;
    end;

    if (FRow < 0) or (FColumn < 0) then
    begin
      Exit;
    end;
  end;

  if FPointer then
  begin
    Exit;
  end;

  if not GetCurrent or not Supports(FSelection,
    INyxCollectionCellRangePreparation, LPreparation) then
  begin
    Exit;
  end;
  FPlan := LPreparation.PrepareNavigation(LNavigation);

  if not GetCurrent or (FPlan = nil) then
  begin
    Exit;
  end;
  FPlan.Validate;

  if Supports(FPlan, INyxPublicationReadiness, LReady) then
  begin
    FReady := LReady;
  end;
end;

function TCellRangeKeyCommand.PreparedCurrent: Boolean;
begin
  Result := False;

  if FPointer then
  begin
    Result := (FPointerPlan <> nil) and FPointerPlan.Current;
    Exit;
  end;

  if FReady = nil then
  begin
    Exit;
  end;
  try
    { Only this canonical flags-only fence supplies expected stale refusal.
      Application/canonical/focus exceptions never enter this catch boundary. }
    FReady.CheckReady;
    Result := True;
  except
    on E: ENyxCollectionRangeStale do
    begin
      { A newer selection/preparation has won; preserve it unchanged. }
    end;
  end;
end;

function TCellRangeKeyCommand.Complete(AAllowDefault: Boolean): Boolean;
var
  LLease: INyxCollectionCellRangeKeyCommand;
begin
  LLease := Self;
  Result := True;

  if FCompleted then
  begin
    Exit;
  end;
  { Claim completion before any allow/current/focus callout. Consumed input and
    nested focus callbacks must not retry this original prepared publication. }
  FCompleted := True;

  if not AAllowDefault or not GetCurrent or not PreparedCurrent then
  begin
    Exit;
  end;

  if (FPointer or (FKey <> ncrkClear)) and
    not FOwner.FocusCellRange(FRow, FColumn) then
  begin
    Exit;
  end;

  if not GetCurrent or not PreparedCurrent or not GetCurrent then
  begin
    Exit;
  end;
  { Both last fences are callbacks-free. Install retires only original range
    flags and adopts the fully admitted rectangle; it publishes no data/row edit. }
  if FPointer then
  begin

    if not FPointerPlan.Complete(True) then
    begin
      Exit;
    end;
    FInstalled := True;
  end
  else
  begin
    FPlan.Install;
    FPlan.Notify;
  end;
  FOwner.RenderCellRange;
end;

procedure TCellRangeKeyCommand.Retire;
var
  LPlan: INyxPreparedPublication;
  LPointerPlan: INyxCollectionCellRangePointerCommand;
  LLease: INyxCollectionMount;
begin

  if FRetired then
  begin
    Exit;
  end;
  FRetired := True;
  LLease := FLease;
  LPlan := FPlan;
  LPointerPlan := FPointerPlan;
  FPlan := nil;
  FPointerPlan := nil;
  FReady := nil;
  FPrevious := nil;
  FSelection := nil;
  FView := nil;
  FInput := nil;
  FOwner := nil;
  FLease := nil;

  if LPointerPlan <> nil then
  begin
    LPointerPlan.Retire;
  end;

  if LPlan <> nil then
  begin
    LPlan.Retire;
  end;
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

function NyxCollectionCellEdit(const AItem: TNyxItemRef;
  const AField: TNyxColumnIdentity; const AAccepted: TNyxStateValue;
  const AText: TNyxText; const ASelection: TNyxTextSelection;
  AEditing, AFocused: Boolean): TNyxCollectionCellEdit;
begin
  Result := nyx.collections.edits.NyxCollectionCellEdit(AItem, AField, AAccepted,
    AText, ASelection, AEditing, AFocused);
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

procedure TNyxCollectionMountBase.RenderCellRange;
begin
  { Target adapters repaint cached immutable membership only. }
end;

function TNyxCollectionMountBase.BeginCellRangeKey(AKey: TNyxCollectionRangeKey):
  INyxCollectionCellRangeKeyCommand;
begin
  Result := nil;

  if FCellRangeEnabled and (FCellRanges <> nil) then
  begin
    Result := TCellRangeKeyCommand.Create(Self, AKey);
  end;
end;

function TNyxCollectionMountBase.BeginCellRangePointer(
  AAction: TNyxCollectionRangePointerAction): INyxCollectionCellRangePointerEntry;
begin
  Result := nil;

  if FCellRangeEnabled and (FCellRanges <> nil) then
  begin
    Result := TCellRangeKeyCommand.CreatePointer(Self, AAction);
  end;
end;

procedure TNyxCollectionMountBase.CancelCellRangeStroke;
var
  LStroke: INyxCollectionRangeStroke;
  LScope: INyxCancellationScope;
  LPreview: INyxCollectionCellRangeCapture;
begin
  { Withdraw authority and cached paint before releasing caller-owned immutable
    snapshots. A finalizer may reenter, but cannot clear a newer pending stroke. }
  LStroke := FCellStroke;
  LScope := FCellStrokeScope;
  LPreview := FCellStrokePreview;
  FCellStroke := nil;
  FCellStrokeScope := nil;
  FCellStrokePreview := nil;
  FCellStrokePointer := Default(TNyxCollectionRangeStrokePointer);

  if LScope <> nil then
  begin
    LScope.Cancel;
  end;

  if LStroke <> nil then
  begin
    LStroke.Retire;
    RetireCellRangeStroke(LStroke);
  end;
end;

procedure TNyxCollectionMountBase.RetireCellRangeStroke(
  const AStroke: INyxCollectionRangeStroke);
begin
  { Nonphysical adapters own no capture or independent paint cache. }
end;

function TNyxCollectionMountBase.BeginCellRangeStroke(
  APhase: TNyxCollectionRangeStrokePhase;
  const APointer: TNyxCollectionRangeStrokePointer;
  AAction: TNyxCollectionRangePointerAction;
  const AStroke: INyxCollectionRangeStroke): INyxCollectionCellRangeStrokeEntry;
begin
  Result := nil;

  if FCellRangeEnabled and (FCellRanges <> nil) then
  begin
    Result := TCellStrokeEntry.Create(Self, APhase, APointer, AAction, AStroke);
  end;
end;

procedure TNyxCollectionMountBase.AbandonCellRangeStroke(
  const APointer: TNyxCollectionRangeStrokePointer;
  const AStroke: INyxCollectionRangeStroke);
var
  LLease: INyxCollectionMount;
  LStroke: INyxCollectionRangeStroke;
  LRequest: Integer;
begin
  LLease := Self as INyxCollectionMount;
  LStroke := AStroke;

  if (LStroke = nil) or (FCellStroke <> LStroke) or
    not FCellStrokePointer.Same(APointer) then
  begin
    Exit;
  end;

  if FCellRangeRequest < High(Integer) then
  begin
    Inc(FCellRangeRequest);
  end;
  LRequest := FCellRangeRequest;
  CancelCellRangeStroke;

  if (FCellRangeRequest = LRequest) and GetInputReady then
  begin
    RenderCellRange;
  end;
end;

function TNyxCollectionMountBase.FocusCellRange(ARow, AColumn: Integer): Boolean;
begin
  { A direct nonvisual mount cannot manufacture physical navigation. }
  Result := False;
end;

function TNyxCollectionMountBase.CellRangeAvailable: Boolean;
begin
  Result := True;
end;

function TNyxCollectionMountBase.CellRangeContains(const AItem: TNyxItemRef;
  const AField: TNyxColumnIdentity): Boolean;
var
  LRange: INyxCollectionCellRangeIntent;
begin
  Result := False;

  if not FCellRangeEnabled or (FCellRanges = nil) or not InputLifetimeCurrent then
  begin
    Exit;
  end;

  if FCellStrokePreview <> nil then
  begin
    Exit(FCellStrokePreview.Contains(AItem, AField));
  end;
  LRange := FCellRanges.Selection;
  Result := (LRange <> nil) and LRange.Current and LRange.Capture.Contains(AItem, AField);
end;

function TNyxCollectionMountBase.GetCellRange: INyxCollectionCellRangeIntent;
begin
  Result := nil;

  if FCellRangeEnabled and (FCellRanges <> nil) and GetInputReady then
  begin
    Result := FCellRanges.Selection;
  end;
end;

procedure TNyxCollectionMountBase.ConfigureCellRange(AEnabled: Boolean);
var
  LLease: INyxCollectionMount;
  LView: INyxCollectionView;
  LInput: INyxCollectionInputLifetime;
  LSpec: TNyxCollectionViewSpec;
  LFields: TNyxCollectionClipboardFields;
  LPrevious: INyxCollectionCellRangeIntent;
  LIndex: Integer;
begin
  LLease := Self as INyxCollectionMount;

  if FConfiguringCellRange then
  begin
    raise ENyxCollectionRange.Create('Cell range mode configuration is already entered');
  end;
  FConfiguringCellRange := True;
  try

    if FCellRangeRequest < High(Integer) then
    begin
      Inc(FCellRangeRequest);
    end;
    CancelCellRangeStroke;
    LView := FView;
    LInput := FInputLifetime;

    if not FConnected or FDisconnecting or (LView = nil) or
      (LInput = nil) or not LInput.Current then
    begin
      raise ENyxCollectionRange.Create('Cell range mode requires a connected table incarnation');
    end;

    if AEnabled then
    begin

      if not CellRangeAvailable then
      begin
        raise ENyxCollectionRange.Create('This target has no managed cell range presentation');
      end;

      if LView.Projection <> cpTable then
      begin
        raise ENyxCollectionRange.Create('Cell range mode requires a table projection');
      end;
      LSpec := LView.Spec;

      if not FConnected or FDisconnecting or (FView <> LView) or
        (FInputLifetime <> LInput) or not LInput.Current then
      begin
        raise ENyxCollectionRangeStale.Create('Table changed while enabling cell range mode');
      end;
      SetLength(LFields, LSpec.Count);
      for LIndex := 0 to High(LFields) do
      begin
        LFields[LIndex] := LSpec.ColumnAt(LIndex).Identity;
      end;

      if FCellRanges = nil then
      begin
        FCellRanges := NewNyxCollectionCellRangeSelection;
      end;
      FClipboardFields := LFields;
    end;
    FCellRangeEnabled := AEnabled;

    if not AEnabled and (FCellRanges <> nil) then
    begin
      LPrevious := FCellRanges.Selection;
      FCellRanges.Clear;
    end;
    { Match optional clipboard admission even on an ordinary no-layout table. }
    TCollectionInputState(FInputStateObject).InteractionEnabled(FEnabled);
    RenderCellRange;
  finally
    FConfiguringCellRange := False;
  end;
end;

procedure TNyxCollectionMountBase.ClearCellRange;
var
  LLease: INyxCollectionMount;
  LPrevious: INyxCollectionCellRangeIntent;
begin
  LLease := Self as INyxCollectionMount;

  if FCellRangeRequest < High(Integer) then
  begin
    Inc(FCellRangeRequest);
  end;
  CancelCellRangeStroke;

  if FCellRanges = nil then
  begin
    Exit;
  end;
  LPrevious := FCellRanges.Selection;
  FCellRanges.Clear;
  RenderCellRange;
end;

function TNyxCollectionMountBase.SelectCells(const AAnchor,
  AFocus: TNyxCollectionCellIdentity): Boolean;
var
  LLease: INyxCollectionMount;
  LSelection: INyxCollectionCellRangeSelection;
  LCandidate: INyxCollectionCellRangeIntent;
  LView: INyxCollectionView;
  LInput: INyxCollectionInputLifetime;
  LStore: INyxCollection;
  LInspection: INyxCollectionInspection;
  LSource: INyxCollectionSnapshot;
  LPage: INyxCollectionSnapshot;
  LColumns: INyxCollectionColumns;
  LCapture: INyxCollectionColumnsCapture;
  LObservation: TNyxColumnLayoutObservation;
  LFields: TNyxCollectionClipboardFields;
  LAnchor: TNyxCollectionCellIdentity;
  LFocus: TNyxCollectionCellIdentity;
  LOptions: TNyxCollectionClipboardOptions;
  LEpoch: Integer;
  LMutation: Integer;
  LRequest: Integer;

  function Current: Boolean;
  begin
    Result := FConnected and not FDisconnecting and FCellRangeEnabled and
      not FUpdating and not FEditsSuspended and FEnabled and
      (FView = LView) and (FInputLifetime = LInput) and
      (FCellRanges = LSelection) and (FCellMutationSerial = LMutation) and
      (LRequest < High(Integer)) and (FCellRangeRequest = LRequest) and
      LInput.Ready and (LInput.Epoch = LEpoch);
  end;

begin
  Result := False;
  LLease := Self as INyxCollectionMount;
  LSelection := FCellRanges;
  LView := FView;
  LInput := FInputLifetime;
  LMutation := FCellMutationSerial;
  LOptions := FClipboardOptions;

  if not FCellRangeEnabled or (LSelection = nil) or (LInput = nil) or
    not LInput.Ready or not FEnabled or FUpdating or FEditsSuspended or
    not ClipboardInputAvailable then
  begin
    Exit;
  end;
  { Reserve before any view/store/page getter. A nested newer selection or
    Clear remains authoritative even if all data revisions stay unchanged. }

  if FCellRangeRequest = High(Integer) then
  begin
    Exit;
  end;
  Inc(FCellRangeRequest);
  LRequest := FCellRangeRequest;
  CancelCellRangeStroke;
  LEpoch := LInput.Epoch;
  LAnchor := NyxCollectionCellIdentity(AAnchor.Item, AAnchor.Field);
  LFocus := NyxCollectionCellIdentity(AFocus.Item, AFocus.Field);
  LStore := LView.Store;

  if not Current or (LStore = nil) then
  begin
    Exit;
  end;
  LInspection := NyxCollectionInspection(LView);

  if not Current then
  begin
    Exit;
  end;

  if LInspection <> nil then
  begin

    if LInspection.CaptureInspection.Mode <> ncimPaused then
    begin
      LInspection := nil;
    end;
  end;

  if not Current then
  begin
    Exit;
  end;

  if LInspection <> nil then
  begin
    LSource := LInspection.GetDisplayedSource;
  end
  else
  begin
    LSource := LStore.Snapshot;
  end;

  if not Current or (LSource = nil) then
  begin
    Exit;
  end;
  LPage := LView.Snapshot;

  if not Current or (LPage = nil) then
  begin
    Exit;
  end;
  LColumns := NyxCollectionColumns(LView);

  if not Current then
  begin
    Exit;
  end;
  LCapture := LColumns.CaptureColumns;

  if not Current then
  begin
    Exit;
  end;
  LObservation := TNyxColumnLayoutObservation.Read(LCapture);
  LFields := NyxCollectionClipboardFields(LObservation, nccfVisiblePresentation);

  if not Current then
  begin
    Exit;
  end;
  try
    LCandidate := LSelection.Select(LSource, LPage, LFields, LAnchor, LFocus,
      LCapture.Serial, LOptions.IncludeHeaders, LOptions.Lines, LOptions.WorkLimits);
  except
    on E: ENyxCollectionRangeStale do
    begin
      Exit;
    end;
  end;

  if not Current then
  begin

    if LSelection.Selection = LCandidate then
    begin
      LSelection.Clear;
    end;
    Exit;
  end;
  Result := True;
  RenderCellRange;
end;

function TNyxCollectionMountBase.GetClipboardCommands: INyxCollectionClipboardCommands;
begin
  Result := FClipboardCommands;
end;

function TNyxCollectionMountBase.GetClipboardFeedback: TNyxCollectionClipboardFeedback;
begin
  Result := FClipboardFeedback;
end;

function TNyxCollectionMountBase.GetClipboardFeedbackEnabled: Boolean;
begin
  Result := FClipboardFeedbackEnabled;
end;

function TNyxCollectionMountBase.HasClipboardFeedback: Boolean;
begin
  Result := (FClipboardCommands <> nil) and (FClipboardOptions.Actions <> []);
end;

procedure TNyxCollectionMountBase.SetClipboardFeedbackEnabled(AEnabled: Boolean);
var
  LLease: INyxCollectionMount;
begin
  LLease := Self as INyxCollectionMount;

  if FClipboardFeedbackEnabled = AEnabled then
  begin
    Exit;
  end;
  FClipboardFeedbackEnabled := AEnabled;
  ClipboardFeedbackReceived(FClipboardFeedback);
end;

procedure TNyxCollectionMountBase.ClipboardFeedbackChanged;
begin
  { Alternative targets retain copied state without claiming a visual surface. }
end;

procedure TNyxCollectionMountBase.ClipboardFeedbackReceived(
  const AFeedback: TNyxCollectionClipboardFeedback);
var
  LLease: INyxCollectionMount;
  LValue: TNyxCollectionClipboardFeedback;
  LFailure: TMountCleanupFailure;
begin
  LLease := Self as INyxCollectionMount;
  LValue := AFeedback;

  if not FConnected or FDisconnecting or not InputLifetimeCurrent or
    (LValue.Serial < FClipboardFeedback.Serial) then
  begin
    Exit;
  end;
  FClipboardFeedback := LValue;
  LFailure := Default(TMountCleanupFailure);
  try
    ClipboardFeedbackChanged;
  except
    {$ifdef PAS2JS}CaptureMountCleanup(LFailure, JSExceptValue);
    {$else}on E: Exception do CaptureMountCleanup(LFailure, TNyxText(E.Message));{$endif}
  end;

  if LFailure.Captured then
  begin
    FFailure := nbfNotificationFailed;
    FError := LFailure.Message;
  end;
end;

function TNyxCollectionMountBase.GetExportCommands: INyxCollectionExportCommands;
begin
  Result := FExportCommands;
end;

procedure TNyxCollectionMountBase.ConfigureExport(const AHost: INyxFileSaveHost;
  const AEvents: INyxEvents; const AScheduler: INyxScheduler;
  const AOptions: TNyxCollectionExportOptions);
var
  LLease: INyxCollectionMount;
  LView: INyxCollectionView;
  LInput: INyxCollectionInputLifetime;
  LHost: INyxFileSaveHost;
  LEvents: INyxEvents;
  LScheduler: INyxScheduler;
  LOptions: TNyxCollectionExportOptions;
  LCommands: INyxCollectionExportCommands;
  LPrevious: INyxCollectionCellActions;
  LCandidate: INyxCollectionCellActions;

  procedure Current;
  begin

    if not FConnected or FDisconnecting or (FView <> LView) or
      (FInputLifetime <> LInput) or not LInput.Current or
      (FExportCommands <> nil) or (FCellActions <> LPrevious) then
    begin
      raise ENyxCollectionExport.Create('Table changed while configuring export authority');
    end;
  end;
begin

  if FConfiguringExport or FConfiguringClipboard or FConfiguringCellActions or
    not FConnected or FDisconnecting or (FExportCommands <> nil) or
    ((FCellActions <> nil) and not FCellActionsDefault) or (AHost = nil) or
    (AEvents = nil) or (AScheduler = nil) then
  begin
    raise ENyxCollectionExport.Create('Export configures once before contextual table actions');
  end;
  FConfiguringExport := True;
  try
    LLease := Self as INyxCollectionMount;
    LView := FView;
    LInput := FInputLifetime;
    LPrevious := FCellActions;
    LHost := AHost;
    LEvents := AEvents;
    LScheduler := AScheduler;
    LOptions := AOptions.Copy;
    LScheduler.RequireUI;
    Current;

    if LView.Projection <> cpTable then
    begin
      raise ENyxCollectionExport.Create('Export requires an actual table projection');
    end;
    Current;
    TCollectionInputState(FInputStateObject).InteractionEnabled(FEnabled);
    try
      LCommands := NewNyxCollectionExportCommands(LView, LInput, LEvents,
        LHost, LScheduler, LOptions, PrepareClipboardInput);
      Current;

      if FCellActionsDefault then
      begin
        LCandidate := NewCellActionsCandidate(FCellFactory, FCellDefaultScheduler,
          TNyxCollectionCellActionOptions.Defaults, FClipboardCommands, LCommands);
        Current;
        LPrevious.Revoke;
      end;
      FExportCommands := LCommands;

      if LCandidate <> nil then
      begin
        FCellActions := LCandidate;
        RetireAdoptedCellActions(LPrevious);
      end;
    except
      try

        if LCommands <> nil then
        begin
          LCommands.Revoke;
        end;
      except
        { The original construction/admission failure owns this tail. }
      end;
      raise;
    end;
  finally
    FConfiguringExport := False;
  end;
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
begin
  ConfigureCellActionsWithOptions(AFactory, AScheduler,
    TNyxCollectionCellActionOptions.Defaults);
end;

procedure TNyxCollectionMountBase.ConfigureCellActionsWithOptions(
  const AFactory: INyxCollectionCellPresenterFactory;
  const AScheduler: INyxScheduler;
  const AOptions: TNyxCollectionCellActionOptions);
begin
  ConfigureCellActionsCore(AFactory, AScheduler, AOptions, False);
end;

procedure TNyxCollectionMountBase.ConfigureDefaultCellActions(
  const AFactory: INyxCollectionCellPresenterFactory;
  const AScheduler: INyxScheduler);
begin
  ConfigureCellActionsCore(AFactory, AScheduler,
    TNyxCollectionCellActionOptions.Defaults, True);
end;

function TNyxCollectionMountBase.NewCellActionsCandidate(
  const AFactory: INyxCollectionCellPresenterFactory;
  const AScheduler: INyxScheduler;
  const AOptions: TNyxCollectionCellActionOptions;
  const AClipboard: INyxCollectionClipboardCommands;
  const AExports: INyxCollectionExportCommands): INyxCollectionCellActions;
var
  LView: INyxCollectionView;
  LInput: INyxCollectionInputLifetime;
  LPrevious: INyxCollectionCellActions;
  LClipboard: INyxCollectionClipboardCommands;
  LExports: INyxCollectionExportCommands;
  LFactoryLease: INyxCollectionCellPresenterFactory;
  LWriter: INyxCollectionClipboardCapturedWrite;
  LCandidate: INyxCollectionCellActions;
  LExporter: INyxCollectionExportCapturedSave;
  LRecovery: INyxCollectionCellRecoveryActions;
  LEpoch: Integer;

  procedure Current;
  begin

    if not FConnected or FDisconnecting or (FView <> LView) or
      (FInputLifetime <> LInput) or not LInput.Current or (LInput.Epoch <> LEpoch) or
      (FCellActions <> LPrevious) or (FClipboardCommands <> LClipboard) or
      (FExportCommands <> LExports) then
    begin
      raise ENyxCollectionClipboard.Create('Table changed while configuring cell actions');
    end;
  end;
begin
  LView := FView;
  LInput := FInputLifetime;
  LEpoch := LInput.Epoch;
  LPrevious := FCellActions;
  LClipboard := FClipboardCommands;
  LExports := FExportCommands;
  LFactoryLease := TCellFactoryLease.Create(AFactory);
  try
    Supports(AClipboard, INyxCollectionClipboardCapturedWrite, LWriter);
    Current;

    if AExports <> nil then
    begin
      Supports(AExports, INyxCollectionExportCapturedSave, LExporter);
      Current;
    end;
    LCandidate := NewNyxCollectionCellActions(LFactoryLease, LWriter, AScheduler,
      AOptions, BeginCellPresentation, LExporter);
    Current;

    if Supports(LCandidate, INyxCollectionCellRecoveryActions, LRecovery) then
    begin
      LRecovery.AttachRecovery(GetDraftRecovery);
      Current;
    end;
    Result := LCandidate;
  except
    try

      if LCandidate <> nil then
      begin
        LCandidate.Disconnect;
      end;
    except
      { Preserve the original admission failure. Only the private lease retires;
        the caller/default's underlying target factory is still authoritative. }
    end;
    raise;
  end;
end;

procedure TNyxCollectionMountBase.RetireAdoptedCellActions(
  const APrevious: INyxCollectionCellActions;
  const AFactory: INyxCollectionCellPresenterFactory);
var
  LFailure: TMountCleanupFailure;
begin
  LFailure := Default(TMountCleanupFailure);
  try

    if APrevious <> nil then
    begin
      APrevious.Disconnect;
    end;
  except
    {$ifdef PAS2JS}CaptureMountCleanup(LFailure, JSExceptValue);
    {$else}on E: Exception do CaptureMountCleanup(LFailure, TNyxText(E.Message));{$endif}
  end;
  try

    if AFactory <> nil then
    begin
      AFactory.Revoke;
    end;
  except
    {$ifdef PAS2JS}CaptureMountCleanup(LFailure, JSExceptValue);
    {$else}on E: Exception do CaptureMountCleanup(LFailure, TNyxText(E.Message));{$endif}
  end;

  if LFailure.Captured then
  begin
    FFailure := nbfNotificationFailed;
    FError := LFailure.Message;
  end;
end;

procedure TNyxCollectionMountBase.ConfigureCellActionsCore(
  const AFactory: INyxCollectionCellPresenterFactory;
  const AScheduler: INyxScheduler;
  const AOptions: TNyxCollectionCellActionOptions; ADefault: Boolean);
var
  LLease: INyxCollectionMount;
  LView: INyxCollectionView;
  LInput: INyxCollectionInputLifetime;
  LFactory: INyxCollectionCellPresenterFactory;
  LPreviousFactory: INyxCollectionCellPresenterFactory;
  LPrevious: INyxCollectionCellActions;
  LCandidate: INyxCollectionCellActions;
  LScheduler: INyxScheduler;
begin

  if FConfiguringCellActions or FConfiguringClipboard or FConfiguringExport or
    not FConnected or FDisconnecting or
    ((FCellActions <> nil) and (ADefault or not FCellActionsDefault)) or
    (AFactory = nil) or (AScheduler = nil) then
  begin
    raise ENyxCollectionClipboard.Create('Cell presentation requires one live table incarnation');
  end;
  FConfiguringCellActions := True;
  try
    LLease := Self as INyxCollectionMount;
    LView := FView;
    LInput := FInputLifetime;
    LPrevious := FCellActions;
    LPreviousFactory := FCellFactory;
    LFactory := AFactory;
    LScheduler := AScheduler;
    LScheduler.RequireUI;

    if not FConnected or FDisconnecting or (FView <> LView) or
      (FInputLifetime <> LInput) or not LInput.Current or
      (FCellActions <> LPrevious) or (FCellFactory <> LPreviousFactory) then
    begin
      raise ENyxCollectionClipboard.Create('Table changed while configuring cell actions');
    end;
    LCandidate := NewCellActionsCandidate(LFactory, LScheduler, AOptions,
      FClipboardCommands, FExportCommands);
    { All outward admission has finished. The old controller is our own managed
      implementation: Revoke clears its port/intent and local cancellation flags
      before publishing the candidate. No cleanup callback can revive old UI. }

    if LPrevious <> nil then
    begin
      LPrevious.Revoke;
    end;
    FCellFactory := LFactory;
    FCellActions := LCandidate;
    FCellActionsDefault := ADefault;
    FCellDefaultScheduler := nil;

    if ADefault then
    begin
      FCellDefaultScheduler := LScheduler;
    end;

    if LPreviousFactory = LFactory then
    begin
      LPreviousFactory := nil;
    end;
    RetireAdoptedCellActions(LPrevious, LPreviousFactory);
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
  LPrevious: INyxCollectionCellActions;
  LCandidate: INyxCollectionCellActions;
  LIndex: Integer;

  procedure Current;
  begin

    if not FConnected or FDisconnecting or (FView <> LView) or
      (FInputLifetime <> LInput) or not LInput.Current or
      (FClipboardCommands <> nil) or (FCellActions <> LPrevious) then
    begin
      raise ENyxCollectionClipboard.Create('Table changed while configuring clipboard authority');
    end;
  end;
begin

  if FConfiguringClipboard or FConfiguringExport or FConfiguringCellActions or
    not FConnected or FDisconnecting or
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
    LPrevious := FCellActions;
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
        LHost, LScheduler, LOptions, PrepareClipboardInput,
        {$ifdef PAS2JS}@{$endif}ClipboardFeedbackReceived);
      Current;

      if FCellActionsDefault then
      begin
        LCandidate := NewCellActionsCandidate(FCellFactory, FCellDefaultScheduler,
          TNyxCollectionCellActionOptions.Defaults, LCommands, FExportCommands);
        Current;
        LPrevious.Revoke;
      end;
      FClipboardFields := LFields;
      FClipboardOptions := LOptions;
      FClipboardCommands := LCommands;

      if LCandidate <> nil then
      begin
        FCellActions := LCandidate;
        RetireAdoptedCellActions(LPrevious);
      end;
      { Presentation may reenter or retire the mount. Every managed owner is
        already installed; no later assignment may resurrect a retired default. }
      ClipboardFeedbackReceived(FClipboardFeedback);
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
  LCapture: INyxCollectionColumnsCapture;
  LColumns: INyxCollectionColumns;
  LPresentation: INyxCollectionTablePresentation;
  LInspectionPolicy: TNyxCollectionInspectionPolicy;
begin
  inherited Create;
  FRecoveryFrame := Default(TNyxDraftRecoveryFrame);
  FRecoveryPendingFrame := Default(TNyxDraftRecoveryFrame);
  FTextLimits := TNyxTableTextWorkLimits.Defaults;
  FClipboardOptions := TNyxCollectionClipboardOptions.Defaults;

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
  FRecoveryPolicy := AView.Spec.DraftRecoveryPolicy;
  LInspectionPolicy := AView.Spec.InspectionPolicy;

  if not LInspectionPolicy.Defined then
  begin
    LInspectionPolicy := NyxInspectionPolicy;
  end;
  FRecoveryIdentityLimit := LInspectionPolicy.IdentityLimit;
  FEnabled := True;
  FClipboardFeedbackEnabled := True;
  LTable := AView.Projection = cpTable;
  FHasColumnLayout := LTable and AView.Spec.HasLayoutControls;
  FHasTablePresentation := LTable and AView.Spec.HasPresentationPolicy;
  FObservesTablePresentation := LTable and
    Supports(AView, INyxCollectionColumns, LColumns) and
    Supports(AView, INyxCollectionTablePresentation, LPresentation);

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

  if FHasColumnLayout or FHasTablePresentation or FObservesTablePresentation then
  begin
    LCapture := NyxCollectionColumns(AView).CaptureColumns;

    { Presentation-only tables also need the exact detached field/serial
      metadata for editor handoff. Reading it does not enable arrangement or
      apply runtime widths/pins; those remain guarded by FHasColumnLayout. }
    FColumnLayout := TNyxColumnLayoutObservation.Read(LCapture);

    if FHasTablePresentation or FObservesTablePresentation then
    begin
      FTablePresentation := TNyxTablePresentationObservation.Read(LCapture);
      FHasTablePresentation := FHasTablePresentation or
        FTablePresentation.HasRuntimeOverrides;
    end;
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

procedure TNyxCollectionMountBase.PrepareInputTracking;
var
  LState: TCollectionInputState;
begin

  if not FConnected or (FInputStateObject = nil) then
  begin
    raise ENyxCollection.Create('Input preparation requires a connected collection mount');
  end;
  LState := TCollectionInputState(FInputStateObject);

  if not LState.GetCurrent then
  begin
    raise ENyxCollection.Create('Input preparation requires the original current mount');
  end;
  { The borrowed state is privately owned here; neither this synchronization nor
    any subsequent tracking flag write invokes an alternative implementation. }
  FInputTrackingPrepared := True;
  LState.InteractionEnabled(FEnabled);
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
  LExport: INyxCollectionExportCommands;
  LCells: INyxCollectionCellActions;
  LFailure: TMountCleanupFailure;
  LRangeIntent: INyxCollectionCellRangeIntent;
begin
  LClipboard := FClipboardCommands;
  LExport := FExportCommands;
  LCells := FCellActions;
  FEditsSuspended := AValue;

  if FInputStateObject <> nil then
  begin
    TCollectionInputState(FInputStateObject).Suspend(AValue);
  end;

  if AValue then
  begin
    CancelCellRangeStroke;

    if FCellRanges <> nil then
    begin
      LRangeIntent := FCellRanges.Selection;
      FCellRanges.Clear;
    end;
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
    try

      if LExport <> nil then
      begin
        LExport.CancelPending;
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
var
  LReveal: INyxTreeReveal;
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
  LReveal := nil;

  if Supports(FView, INyxTreeReveal, LReveal) then
  begin
    FTreeReveal := LReveal;
    { A new physical generation never replays a former target's command. }
    FTreeRevealSerial := LReveal.CaptureTreeReveal.Serial;
  end;
  FConnected := True;

  if FPositionStateObject <> nil then
  begin
    TPositionState(FPositionStateObject).Active := True;
  end;
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
  LExport: INyxCollectionExportCommands;
  LCells: INyxCollectionCellActions;
  LCellFactory: INyxCollectionCellPresenterFactory;
  LRanges: INyxCollectionCellRangeSelection;
  LRangeIntent: INyxCollectionCellRangeIntent;
begin

  if FDisconnecting or ((FView = nil) and not FConnected) then
  begin
    Exit;
  end;
  FDisconnecting := True;
  FConnected := False;
  FCellFocusValid := False;

  if FRecoverySubscription <> nil then
  begin
    FRecoverySubscription.Disconnect;
    FRecoverySubscription := nil;
  end;

  if FRecoveryWriter <> nil then
  begin
    FRecoveryWriter.Retire;
  end;
  FRecoveryFrame := Default(TNyxDraftRecoveryFrame);
  FRecoveryPendingFrame := Default(TNyxDraftRecoveryFrame);
  FRecoveryPendingItem := Default(TNyxItemRef);
  FRecoveryContinuity := nil;
  FRecoveryRemoved := nil;

  if FPositionStateObject <> nil then
  begin
    TPositionState(FPositionStateObject).Active := False;
  end;

  if FInputStateObject <> nil then
  begin
    TCollectionInputState(FInputStateObject).Retire;
  end;
  { Detach every public receiver before the first alternative-factory callback.
    Input already refuses defaults. Each independent cleanup is attempted even
    when an implementation throws; retain the first failure for the caller
    and always release the disconnect guard and borrowed target at the end. }
  LClipboard := FClipboardCommands;
  LRanges := FCellRanges;

  if LRanges <> nil then
  begin
    LRangeIntent := LRanges.Selection;
  end;
  LExport := FExportCommands;
  LCells := FCellActions;
  LCellFactory := FCellFactory;
  LManager := FColumnManager;
  LFactory := FColumnFactory;
  LQueries := FQueries;
  LQueryFactory := FQueryFactory;
  LToken := FToken;
  FClipboardCommands := nil;
  FCellRanges := nil;
  FCellRangeEnabled := False;
  FExportCommands := nil;
  FCellActions := nil;
  FCellFactory := nil;
  FCellActionsDefault := False;
  FCellDefaultScheduler := nil;
  FClipboardFields := nil;
  FColumnManager := nil;
  FColumnFactory := nil;
  FQueries := nil;
  FQueryFactory := nil;
  FToken := nil;
  LFailure := Default(TMountCleanupFailure);

  if LRanges <> nil then
  begin
    LRanges.Revoke;
  end;
  try
    try
      CancelCellRangeStroke;
    except
      {$ifdef PAS2JS}CaptureMountCleanup(LFailure, JSExceptValue);
      {$else}on E: Exception do CaptureMountCleanup(LFailure, TNyxText(E.Message));{$endif}
    end;
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

      if LExport <> nil then
      begin
        LExport.Revoke;
      end;
    except
      {$ifdef PAS2JS}CaptureMountCleanup(LFailure, JSExceptValue);
      {$else}on E: Exception do CaptureMountCleanup(LFailure, TNyxText(E.Message));{$endif}
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
    FTablePresentation := Default(TNyxTablePresentationObservation);
    FView := nil;
    FTreeReveal := nil;
    FTreeRevealAllowed := False;
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
  LRangeIntent: INyxCollectionCellRangeIntent;
begin
  LKeepAlive := Self as INyxCollectionMount;
  { Sample before Refresh temporarily shields otherwise accepted input. A
    request issued while disabled/suspended cannot be revived by enable. }
  FTreeRevealAllowed := GetInputReady and FEnabled;

  if FInputLifetime <> nil then
  begin
    FTreeRevealEpoch := FInputLifetime.Epoch;
  end;
  FCellFocusValid := False;
  CancelCellRangeStroke;

  if FCellRanges <> nil then
  begin
    { Any view publication withdraws this physical range. Keep the old immutable
      snapshots alive through the entered notification, before foreign getters. }
    LRangeIntent := FCellRanges.Selection;
    FCellRanges.Clear;
  end;

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
    try
      CaptureRecoveryContinuity(AChanges);
    except
      { An alternative change getter/allocation can fail before Refresh enters
        its own shield. A committed source must never leave the older physical
        map writable after that failure. Only a successful retry clears it. }

      if (FInputStateObject <> nil) and InputLifetimeCurrent then
      begin
        TCollectionInputState(FInputStateObject).Refreshing(True);
      end;
      raise;
    end;
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

procedure TNyxCollectionMountBase.PrepareTablePresentation;
begin
  { Browser observation is attached before activation. Native targets override
    this boundary to admit their real frame/editor bridge and paint reader. }
end;

procedure TNyxCollectionMountBase.RefreshTargetIdle;
begin
  { Ordinary targets have no deferred native-tail drain. }
end;

procedure TNyxCollectionMountBase.RecoveryMembershipChanged(
  const AObservation: TNyxDraftRecoveryObservation);
begin
  { Data-only mounts expose their current count without physical presentation. }
end;

procedure TNyxCollectionMountBase.RetireTargetInput;
begin
  { Ordinary targets have no additional weak input receiver. }
end;

function TPositionState.GetActive: Boolean;
begin
  Result := Active;
end;

function TPositionState.GetRecovery: TNyxCollectionPositionRecovery;
begin
  Result := Recovery;
end;

function TNyxCollectionMountBase.CapturePositionState: INyxCollectionPositionState;
var
  LState: TPositionState;
begin

  if FPositionState = nil then
  begin
    LState := TPositionState.Create;
    FPositionState := LState;
    FPositionStateObject := LState;
    LState.Active := FConnected and not FDisconnecting;
    LState.Recovery := GetPositionRecovery;
  end;
  Result := FPositionState;
end;

function TNyxCollectionMountBase.GetPositionRecovery: TNyxCollectionPositionRecovery;
begin
  Result := FPositionRecovery;

  if Result.Kind = ncpUnavailable then
  begin
    Result.Index := -1;
  end;
end;

function TNyxCollectionMountBase.ResolvePosition(const ABefore,
  AAfter: INyxCollectionSnapshot; AIndex: Integer): TNyxCollectionPositionRecovery;
var
  LView: INyxCollectionView;
  LInspection: INyxCollectionInspection;
  LPosition: INyxCollectionPositionView;
  LContinuity: INyxCollectionDisplayContinuity;
  LPolicy: TNyxCollectionInspectionPolicy;
  LStatus: TNyxCollectionInspectionObservation;
  LMutation: Integer;

  function Current: Boolean;
  begin
    Result := InputLifetimeCurrent and (FView = LView);

    if Result and (FCellMutationSerial <> LMutation) then
    begin
      raise ENyxCollection.Create('Collection changed during viewport recovery');
    end;
  end;

begin
  Result := Default(TNyxCollectionPositionRecovery);
  Result.Index := -1;
  LView := FView;
  LMutation := FCellMutationSerial;

  if not Current or (LMutation = High(Integer)) or (AAfter = nil) then
  begin
    Exit;
  end;
  LInspection := NyxCollectionInspection(LView);

  if not Current or (LInspection = nil) then
  begin
    Exit;
  end;
  LPolicy := LInspection.GetInspectionPolicy;

  if not Current or not LPolicy.Defined then
  begin
    Exit;
  end;
  LStatus := LInspection.CaptureInspection;

  if not Current then
  begin
    Exit;
  end;

  if (ABefore <> nil) and (ABefore <> AAfter) and
    Supports(LView, INyxCollectionPositionView, LPosition) then
  begin

    if not Current then
    begin
      Exit;
    end;
    LContinuity := LPosition.CaptureDisplayContinuity;
  end;

  if not Current then
  begin
    Exit;
  end;

  if (LStatus.Mode = ncimFollowing) and (LStatus.FollowMode = ncfLastVisible) then
  begin
    Result := NyxCollectionLatestPosition(ABefore, AAfter, AIndex,
      LStatus.FollowRequestSerial <> FFollowRequestSerial, LContinuity);
  end;

  if (Result.Kind = ncpUnavailable) and (ABefore <> nil) and (ABefore <> AAfter) then
  begin
    Result := NyxCollectionPositionRecovery(ABefore, AAfter, AIndex, LContinuity);
  end;

  if not Current then
  begin
    Result := Default(TNyxCollectionPositionRecovery);
    Result.Index := -1;
    Exit;
  end;

  if LStatus.Mode = ncimFollowing then
  begin
    { A physical refresh must finish before acknowledging an explicit request.
      Failed/deferred target work leaves it available to a later retry. }
    FPendingFollowRequestSerial := LStatus.FollowRequestSerial;
  end;

  if Result.Kind = ncpUnavailable then
  begin
    Exit;
  end;
  Result.ObservationSerial := LMutation;
  FPositionRecovery := Result;

  if FPositionStateObject <> nil then
  begin
    TPositionState(FPositionStateObject).Recovery := Result;
  end;
end;

function TNyxCollectionMountBase.CurrentTreeReveal: TNyxTreeRevealRequest;
var
  LReveal: INyxTreeReveal;
  LView: INyxCollectionView;
  LMutation: Integer;
begin
  Result := Default(TNyxTreeRevealRequest);
  LReveal := FTreeReveal;
  LView := FView;
  LMutation := FCellMutationSerial;

  if not InputLifetimeCurrent or (LReveal = nil) then
  begin
    Exit;
  end;
  Result := LReveal.CaptureTreeReveal;

  if InputLifetimeCurrent and (FView = LView) and (FCellMutationSerial <> LMutation) then
  begin
    raise ENyxCollection.Create('Collection changed during Tree viewport capture');
  end;

  if not InputLifetimeCurrent or (FView <> LView) or (FTreeReveal <> LReveal) or
    (FCellMutationSerial <> LMutation) or not FTreeRevealAllowed or not FEnabled or
    (FInputLifetime.Epoch <> FTreeRevealEpoch) then
  begin
    Result := Default(TNyxTreeRevealRequest);
  end;
end;

function TNyxCollectionMountBase.PendingTreeReveal: TNyxTreeRevealRequest;
begin
  Result := CurrentTreeReveal;

  if Result.Serial <= FTreeRevealSerial then
  begin
    Result := Default(TNyxTreeRevealRequest);
  end;
end;

procedure TNyxCollectionMountBase.TreeRevealApplied(ASerial: Integer);
begin
  { Local proposed acknowledgement only. Refresh adopts it after target success;
    a thrown/deferred render leaves the same request available to a retry. }
  FPendingTreeRevealSerial := ASerial;
end;

procedure TNyxCollectionMountBase.Refresh;
var
  LKeepAlive: INyxCollectionMount;
  LSnapshot: INyxCollectionSnapshot;
  LView: INyxCollectionView;
  LSpec: TNyxCollectionViewSpec;
  LFormerLayout, LNextLayout: TNyxColumnLayoutObservation;
  LFormerPresentation: TNyxTablePresentationObservation;
  LNextPresentation: TNyxTablePresentationObservation;
  LCapture: INyxCollectionColumnsCapture;
  LFormerHeaders: array of TNyxText;
  LFormerSummary: TNyxText;
  LInputLease: INyxCollectionInputLifetime;
  LInputState: TCollectionInputState;
  LCompleted: Boolean;
  LRecoveryHeld: Boolean;
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
  LFormerPresentation := FTablePresentation;
  LNextPresentation := FTablePresentation;
  LFormerHeaders := FQueryHeaders;
  LFormerSummary := FQuerySummary;
  LInputLease := nil;
  LInputState := nil;
  LCompleted := False;
  LRecoveryHeld := False;
  FPendingFollowRequestSerial := FFollowRequestSerial;
  FPendingTreeRevealSerial := FTreeRevealSerial;
  FUpdating := True;
  try
    { A complete target refresh withdraws an unfinished physical gesture. Its
      cached highlight cannot outlive changed geometry or a deferred import.
      The existing updating shield refuses finalizer-created physical input. }
    CancelCellRangeStroke;

    if DeferComposingRefresh then
    begin
      Exit;
    end;

    if FInputStateObject <> nil then
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

    if FHasColumnLayout or FHasTablePresentation or FObservesTablePresentation then
    begin
      LCapture := NyxCollectionColumns(LView).CaptureColumns;

      LNextLayout := TNyxColumnLayoutObservation.Read(LCapture);

      if FHasTablePresentation or FObservesTablePresentation then
      begin
        LNextPresentation := TNyxTablePresentationObservation.Read(LCapture);
      end;
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
    FTablePresentation := LNextPresentation;

    if FHasTablePresentation or LNextPresentation.HasRuntimeOverrides then
    begin
      PrepareTablePresentation;
      { Once activated, retain the observer for this incarnation. Reset needs
        one final admitted pass to clear old styles/row geometry and drafts
        must keep their composition continuity even after all choices clear. }
      FHasTablePresentation := True;
    end;
    try
      RenderDataset;
    except
      on LPressure: ENyxDraftRecoveryPressure do
      begin
        { The journal refused BEFORE the target released its dirty value.
          Keep the former physical map and its existing input shield, while
          letting committed source/peer notifications complete normally.
          Applications review capacity through the data-only budget facet and
          retry this same mount explicitly after resolving the pressure. }
        LRecoveryHeld := True;
        FDraftRecoveryHeld := True;
        Exit;
      end;
    end;

    if FTargetRefreshDeferred then
    begin
      Exit;
    end;

    if InputLifetimeCurrent then
    begin
      FLastSnapshot := LSnapshot;
      FFollowRequestSerial := FPendingFollowRequestSerial;
      FTreeRevealSerial := FPendingTreeRevealSerial;
      FDraftRecoveryHeld := False;
      FRecoveryFrame := Default(TNyxDraftRecoveryFrame);
      FRecoveryPendingFrame := Default(TNyxDraftRecoveryFrame);
      FRecoveryPendingItem := Default(TNyxItemRef);
      FRecoveryContinuity := nil;
      FRecoveryRemoved := nil;
    end;
    Inc(FRefreshCount);
    LCompleted := True;
  finally
    { A source getter may start composition after the initial admission check.
      Preserve the caches belonging to the still-visible physical column map. }

    if (FTargetRefreshDeferred or LRecoveryHeld) and InputLifetimeCurrent and (FView = LView) then
    begin
      FColumnLayout := LFormerLayout;
      FTablePresentation := LFormerPresentation;
      FQueryHeaders := LFormerHeaders;
      FQuerySummary := LFormerSummary;
    end;
    FRefreshPlan := Default(TNyxCollectionRefreshPlan);
    FUpdating := False;

    if (LInputLease <> nil) and (LInputState <> nil) then
    begin
      { Clearing this temporary flag cannot revive a terminal lifetime. Never
        borrow FInputStateObject again after an outward reader or target call. }
      { Deferred composition retains its existing input rules. An exception
        before completion must not let an old physical map write newer rows;
        a later successful Refresh clears this same temporary shield. }
      LInputState.Refreshing(not LCompleted and not FTargetRefreshDeferred and
        InputLifetimeCurrent);
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
  Result := InputLifetimeCurrent and (FHasColumnLayout or FHasTablePresentation) and FCellFocusValid and
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
  LExport: INyxCollectionExportCommands;
  LCells: INyxCollectionCellActions;
  LQueries: INyxCollectionQueryWorkbench;
  LManager: INyxColumnArrangementManager;
  LMutation: Integer;
  LBodyFailed: Boolean;
  LFailure: TMountCleanupFailure;
  LRangeIntent: INyxCollectionCellRangeIntent;

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
  LExport := FExportCommands;
  LCells := FCellActions;
  FEnabled := AEnabled;
  FReadOnly := AReadOnly;
  FCellFocusValid := False;
  CancelCellRangeStroke;

  if not AEnabled and (FCellRanges <> nil) then
  begin
    LRangeIntent := FCellRanges.Selection;
    FCellRanges.Clear;
  end;

  if (FInputTrackingPrepared or FHasColumnLayout or FHasTablePresentation or (LClipboard <> nil) or
    (LExport <> nil) or FCellRangeEnabled) and
    (FInputStateObject <> nil) then
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

      try

        if LExport <> nil then
        begin
          LExport.CancelPending;
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

{$i nyx.collections.recovery.mount.inc}

end.
