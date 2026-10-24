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

unit nyx.render.lcl;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  nyx.types,
  nyx.text,
  nyx.publication,
  nyx.view.publication,
  nyx.coordinators,
  nyx.application.context, nyx.flow, nyx.tool.canvas, nyx.canvas.adoption,
  nyx.dates,
  nyx.dates.lcl,
  nyx.times,
  nyx.times.lcl,
  nyx.colors,
  nyx.colors.lcl,
  nyx.images, nyx.images.lcl,
  nyx.button.presentation, nyx.button.view, nyx.button.glyphs.lcl,
  nyx.image.lifecycle, nyx.image.events,
  nyx.sliders,
  nyx.sliders.lcl,
  nyx.tabs.lcl,
  nyx.tabs,
  nyx.disclosure,
  nyx.canvas.workspace,
  nyx.drawing, nyx.drawing.lcl, nyx.drawing.view,
  Classes,
  LCLType,
  SysUtils,
  Math,
  Types,
  Forms,
  Controls,
  StdCtrls,
  ExtCtrls,
  ComCtrls,
  Grids,
  Spin,
  Graphics,
  nyx.widgets.lcl,
  nyx.model,
  nyx.presentations,
  nyx.containers,
  nyx.layout.flow,
  nyx.layout.constraints,
  nyx.layout.viewport,
  nyx.interaction,
  nyx.editing,
  nyx.editing.commands,
  nyx.editing.lcl,
  nyx.accessibility.lcl,
  nyx.gestures,
  nyx.designer.input,
  nyx.designer.placement,
  nyx.designer.resize,
  nyx.designer.move,
  nyx.designer.guides,
  nyx.gestures.lcl,
  nyx.platform,
  nyx.split,
  nyx.split.lcl,
  nyx.schema,
  nyx.contract,
  nyx.theme,
  nyx.progress.lcl,
  nyx.design.tokens,
  nyx.behavior,
  nyx.events,
  nyx.kanban, nyx.kanban.intent,
  nyx.master.detail,
  nyx.data,
  nyx.viewport,
  nyx.viewport.lcl,
  nyx.observation,
  nyx.viewport.surface.lcl,
  nyx.event.emitter,
  nyx.state,
  nyx.resources,
  nyx.resource.context,
  nyx.binding,
  nyx.binding.lifetime,
  nyx.binding.types,
  nyx.collections.view,
  nyx.collections.selection,
  nyx.collections.bindings,
  nyx.controls,
  nyx.collections.pager,
  nyx.collections.view.types,
  nyx.collections.mount,
  nyx.collections.workbench.runtime,
  nyx.collections.presentation,
  nyx.collections.clipboard.actions,
  nyx.collections.lcl,
  nyx.collections.lcl.grid,
  nyx.literal.items,
  nyx.composition,
  nyx.content.mount,
  nyx.scheduler,
  nyx.projection.refresh;

type
  TNyxLCLRenderer = class;
  { Private canonical generation lease. Current is a flags-only weak-receiver
    check; it never queries a router, widget, store or replacement renderer. }
  INyxLCLBindingGeneration = interface(IInterface)
    function Current: Boolean;
    function Coordinator: TNyxLiveBindings;
    function PhysicalFrame: INyxNativeGestureFrame;
  end;
  { Saved widget slots preserve a custom factory's handlers. Two snapshots are
    needed for the outer face and a separate framed input; neither owns controls. }
  TNyxLCLPointerHooks = record
    DoubleClick: TNotifyEvent;
    Down: TMouseEvent;
    Up: TMouseEvent;
    Move: TMouseMoveEvent;
    Enter: TNotifyEvent;
    Leave: TNotifyEvent;
    ContextMenu: TContextPopupEvent;
    Wheel: TMouseWheelEvent;
    WheelHorz: TMouseWheelEvent;
    StartDrag: TStartDragEvent;
    EndDrag: TEndDragEvent;
    DragOver: TDragOverEvent;
    DragDrop: TDragDropEvent;
  end;
  TNyxLCLEvent = TNyxEventHandler;
  TNyxLCLBindingError = procedure(ANode: TNyxNode; const AReason: TNyxText;
    AFailure: TNyxBindingFailure) of object;
  { Create one fresh, unparented control owned by the supplied candidate host.
    The node is borrowed during construction; the renderer connects managed
    activation before parenting and owns later admission/retirement. A factory
    must not retain an owning renderer/document reference through its control. }
  TNyxLCLFactory = function(ANode: TNyxNode; AOwner: TComponent): TControl;
  { The initially dormant emitter connects only after complete view admission.
    It carries owned payloads and may outlive the borrowed node/control safely.
    Return a fresh unparented control with the same host ownership as above. }
  TNyxLCLEventFactory = function(ANode: TNyxNode; AOwner: TComponent;
    const AEmitter: INyxEventEmitter): TControl;
  { Borrowed custom-control refresh, after shared value/property admission.
    Keep control/child identity and do not write state inside this callback.
    Bound custom factories require an updater just as browser factories do. }
  TNyxLCLUpdater = procedure(ANode: TNyxNode; AControl: TControl);

  { Native bindings borrow model/control handles. LCL ownership remains rooted in
    the renderer's host panel; model ownership remains in its realized view. }
  TNyxLCLBinding = class
  private
    FRenderer: TNyxLCLRenderer;
    FGeneration: TObject; { weak canonical owner; entered methods lease it }
    FNode: TNyxNode;
    FControl: TControl;
    FInput: TControl;
    { Borrowed native date host owns the real editable input and its popup. }
    FDateField: TNyxLCLDateField;
    { Borrowed clock host owns its exact grouped editor and picker window. }
    FTimeField: TNyxLCLTimeField;
    FColorField: TNyxLCLColorField;
    FCaption: TLabel;
    { Value-owned layout and projection. The parent binding is borrowed from the
      same preorder renderer array, never a reference-counted tree back edge. }
    FLogicalParent: TNyxLCLBinding;
    FLogicalBox: TNyxViewportBox;
    FContainerAllocated: Boolean;
    FScreenBox: TNyxViewportBox;
    FPlacement: TNyxViewportPlacement;
    FLastValue: TNyxText;
    FHasValueBaseline: Boolean;
    { Separate baselines protect literal selection and local picture identity
      during unrelated state/layout publications. Managed collections and
      creator updaters retain exclusive ownership of their own contents. }
    FLastItems: TNyxText;
    FHasItemsBaseline: Boolean;
    FLastSource: TNyxText;
    FHasSourceBaseline: Boolean;
    FImageLifecycle: INyxImageLifecycle;
    { Binding-owned immutable fallback admission, without retaining this node or
      its provider. Retired with the binding before borrowed model teardown. }
    FDrawingSource: INyxDrawingSource;
    FCustom: Boolean;
    FUpdater: TNyxLCLUpdater;
    FDeferredValue: Boolean;
    FCommitting: Boolean;
    { Borrowed mounted nodes, retained only between admission and presentation. }
    FTabDispatch: TNyxDispatch;
    FHasTabDispatch: Boolean;
    FComposing: Boolean;
    FEditingContext: TNyxEditingSnapshot;
    FPreviousClick: TNotifyEvent;
    FPreviousInputClick: TNotifyEvent;
    FPreviousEnter: TNotifyEvent;
    FPreviousExit: TNotifyEvent;
    { Borrowed exact keyboard surface. Logical Enter/Exit and native focus
      messages share one transition baseline so ordinary focus never duplicates
      subscriptions, while popup returns remain observable. }
    FFocusControl: TWinControl;
    FHasFocus: Boolean;
    FPreviousKeyDown: TKeyEvent;
    FPreviousKeyUp: TKeyEvent;
    FCollectionMount: INyxCollectionMount;
    { Readonly facts avoid retaining a mount through native capture hooks.
      Embedded local acceptance still observes its enclosing pending gate. }
    FCollectionInputLifetime: INyxCollectionInputLifetime;
    FCanvasView: INyxCanvasWorkspaceView;
    FToolCanvasView: INyxToolCanvasView;
    FCollectionPager: INyxCollectionPager;
    { LCL's virtual key callback has no repeat flag. Track admitted key slots and
      clear on focus loss, so a missing outside-view key-up cannot stick a key. }
    FPressedKeys: array[0..255] of Boolean;
    FPointerHooks: array[0..1] of TNyxLCLPointerHooks;
    { Borrowed LCL-owned drag object. Clear before callbacks or view retirement;
      it never owns this binding or its realized node. }
    FActiveDrag: TDragObject;
    { Presentation/source/color baseline avoids decoding or resampling a toolbar
      atlas during unrelated input, layout settling or selection refresh. }
    FLastButtonFace: TNyxText;
    FLastButtonGlyphSource: TNyxText;
    function GenerationLease: INyxLCLBindingGeneration;
    procedure SyncLiteralItems;
    procedure SetupCanvas(ARenderer: TNyxLCLRenderer);
    procedure CanvasEvent(ANode: TNyxNode; const AEvent: TNyxEventInfo);
    procedure RefreshPager;
    procedure SyncPicture;
    procedure SyncButtonFace;
    procedure AttachPointer(AControl: TControl; ASlot: Integer);
    { Revoke all Nyx-installed physical slots before deferred destruction. The
      LCL control may finish its current message, but cannot enter this binding. }
    procedure DisconnectControl(AControl: TControl);
    function PointerSlot(ASender: TObject): Integer;
    function PointerEvent(ASender: TObject; ATrigger: TNyxTrigger;
      const APosition: TPoint; AHasPosition: Boolean; AButton: TNyxPointerButton;
      AShift: TShiftState): Boolean;
    procedure DoubleClick(ASender: TObject);
    procedure PointerDown(ASender: TObject; AButton: TMouseButton;
      AShift: TShiftState; AX, AY: Integer);
    procedure PointerUp(ASender: TObject; AButton: TMouseButton;
      AShift: TShiftState; AX, AY: Integer);
    procedure PointerMove(ASender: TObject; AShift: TShiftState; AX, AY: Integer);
    procedure PointerEnter(ASender: TObject);
    procedure PointerExit(ASender: TObject);
    procedure StartDrag(ASender: TObject; var ADragObject: TDragObject);
    procedure EndDrag(ASender, ATarget: TObject; AX, AY: Integer);
    procedure DragOver(ASender, ASource: TObject; AX, AY: Integer;
      AState: TDragState; var AAccept: Boolean);
    procedure DragDrop(ASender, ASource: TObject; AX, AY: Integer);
    function DragEvent(ASender: TObject; ADragObject: TDragObject;
      ATrigger: TNyxTrigger; APhase: TNyxDragPhase; AX, AY: Integer): TNyxGestureResult;
    procedure ContextMenu(ASender: TObject; APosition: TPoint; var AHandled: Boolean);
    procedure PreparedCellContext(ASender: TObject; APosition: TPoint;
      var AHandled: Boolean; const ACommand: INyxCollectionClipboardCommand;
      APrevious: TContextPopupEvent);
    procedure RunCellContext(ASender: TObject; APosition: TPoint;
      var AHandled: Boolean; const ACommand: INyxCollectionClipboardCommand;
      APrevious: TContextPopupEvent; APrepared: Boolean);
    procedure Wheel(ASender: TObject; AShift: TShiftState; ADelta: Integer;
      APosition: TPoint; var AHandled: Boolean);
    procedure WheelHorz(ASender: TObject; AShift: TShiftState; ADelta: Integer;
      APosition: TPoint; var AHandled: Boolean);
    procedure WheelEvent(ASender: TObject; AShift: TShiftState; ADelta: Integer;
      AHorizontal: Boolean; var AHandled: Boolean);
    procedure CollectionSelectionChanged(const ABefore, AAfter: INyxCollectionSelection);
    procedure SplitLayout(ASender: TObject);
    procedure SplitChanged(ASender: TObject);
    procedure TabsChanged(const AChange: TNyxTabTransition);
    function AdmitTabs(const AChange: TNyxTabTransition): Boolean;
    procedure Click(ASender: TObject);
    { Same real command, with an explicit outcome for a nested column button.
      Notification failure stays committed but denies that later Fit default. }
    function PerformClick(ASender: TObject): Boolean;
    procedure Change(ASender: TObject);
    procedure CommitValue(ASender: TObject);
    procedure Focus(ASender: TObject);
    procedure Enter(ASender: TObject);
    procedure Leave(ASender: TObject);
    { Native message observation skips creator Enter/Exit forwarding: those
      slots remain LCL-owned. Only Nyx's typed focus transition is recovered. }
    procedure ObserveFocus(ATrigger: TNyxTrigger);
    procedure KeyDown(ASender: TObject; var AKey: Word; AShift: TShiftState);
    procedure KeyUp(ASender: TObject; var AKey: Word; AShift: TShiftState);
    procedure Keyboard(ASender: TObject; var AKey: Word; AShift: TShiftState;
      ATrigger: TNyxTrigger);
  end;

  { Copied vector of borrowed binding handles. The exact generation assumes
    final wrapper ownership at retirement; its entered leases retain the vector. }
  TNyxLCLBindings = array of TNyxLCLBinding;

  { Adapter-private intrinsic result for one synchronous layout pass. Font owns
    a complete typography copy; text and size are values. No live label, parent,
    document or drawing context is retained. Every entry is freed on pass exit. }
  TNyxLCLLabelMeasurement = record
    Text: TNyxText;
    Font: TFont;
    AvailableWidth: Integer;
    Wrap: Boolean;
    ShowAccelChar: Boolean;
    BiDiMode: TBiDiMode;
    Width: Integer;
    Height: Integer;
  end;

  { A native projection using Lazarus controls rather than a browser embedded in
    a window. Custom kinds supply native factories through the same string keys
    used by the browser registry. The renderer never owns the caller's host.
    A supplied theme is borrowed; the default theme is renderer-owned.
    Factories/updaters borrow staged nodes and must not mutate their source or
    runtime store during preparation. Replacements preview fallible target work
    before retirement; extension destructors must release without raising. }
  TNyxLCLRenderer = class
  private type
    { Immutable managed replay intent. Its exact runtime identity and revocable
      generation own no widget/router/view. Irreversible mount cancellation
      prevents a same-ID replacement from inheriting an earlier replay policy. }
    IFocusReplayTarget = interface(IInterface)
      function Preserves(const AID: TNyxText): Boolean;
    end;
    { Independently managed dynamic scope. A focus callback may dispose the
      renderer, so the caller's finally restores only its local interface lease.
      Exact-target suppression lets callbacks reveal a different chosen control. }
    IFocusReplay = interface(IInterface)
      function GetTarget: IFocusReplayTarget;
      procedure SetTarget(const ATarget: IFocusReplayTarget);
      function Preserves(const AID: TNyxText): Boolean;
      property Target: IFocusReplayTarget read GetTarget write SetTarget;
    end;
  private
    FFocusReplay: IFocusReplay;
    FRenderAttempt: IInterface; { exact structural attempt, never revived }
    FGeneration: INyxLCLBindingGeneration;
    FGenerationObject: TObject; { exact private implementation, borrowed }
    FOwnedState: INyxBindingOwnedState;
    FThemeOwner, FBaseThemeOwner, FGlyphCacheOwner: IInterface;
    FRetirementShell, FDestroying: Boolean;
    FDeferredNativeControls: Boolean;
    FTheme: TNyxTheme;
    FOwnTheme: Boolean;
    { Startup palette owns independent values; per-document overrides can be
      removed without leaving stale colors or changing an external theme. }
    FBaseTheme: TNyxTheme;
    { A caller-supplied palette remains borrowed and may change between renders.
      Effective document overlays never mutate it or accumulate into its base. }
    FBorrowedTheme: TNyxTheme;
    FRoot: TNyxNode;
    FResourceContext: INyxResourceContext;
    FResourceUpdates: INyxResourceUpdateQueue;
    { Runtime services survive remounts as independent configuration leases;
      their host authority never enters a design or an authored policy. }
    FTableWorkbenchServices: INyxTableWorkbenchServices;
    { Runtime-only bounded text work; copied to each independently staged view. }
    FTableTextLimits: TNyxTableTextWorkLimits;
    FPresentationSelection: TNyxPresentationSelection;
    FPresentationView: INyxPresentationViewOwner;
    { Independently owned recipes outlive the source document. Queued work
      retains a revocable port, never the renderer or its native controls. }
    FContentBlueprint: TNyxContentBlueprint;
    { Runtime preparation source survives implicit content remounts. Its host
      receivers must revoke before disposal; a source never owns widgets. }
    FViewPublicationSource: INyxViewPublicationSource;
    FContentGuard: INyxContentGuard;
    FContentLifetime: INyxContentLifetime;
    FContentWork: INyxContentWork;
    FContentExecution: INyxExecution;
    FContentRequestedSelection: TNyxPresentationSelection;
    FContentFrame: TNyxViewFrame;
    FContentFrameKnown: Boolean;
    FContentMeasurementKey: TNyxText;
    FPublishingContent: Boolean;
    FImageDeliveryQueue: INyxImageDeliveryQueue;
    FButtonGlyphCache: TNyxLCLButtonGlyphCache;
    { Hidden candidates cannot publish lifecycle observations. }
    FImageViewPublished: Boolean;
    { One hidden candidate consumes a frozen allocation snapshot for both
      structure and scalar projection. Mounted layout observes actual boxes. }
    FStagingContent: Boolean;
    FStagingMeasurements: INyxContainerSnapshot;
    FLastContentError: TNyxText;
    procedure QueueContent;
    procedure FlushImages;
    procedure RefreshContent;
    function CaptureContentFaces: TNyxContentFaceStates;
    procedure RestoreContentFaces(const AStates: TNyxContentFaceStates; AFocus: Boolean);
    { Separate result storage preserves an outstanding caller preview during
      entry admission rather than retiring it through an aliased hidden Result. }
    function PrepareFormRefreshCore(ADocument: TNyxDocument; ARoot: TNyxNode;
      const AForms: TNyxProjectionFormContexts; ADesignMode: Boolean): INyxPreparedPublication;
    procedure RenderFrame(ADocument: TNyxDocument; ARoot: TNyxNode;
      AHost: TWinControl; ADesignMode: Boolean; AState: TNyxState;
      const ACollections: INyxCollectionBindings; const AFrame: TNyxViewFrame;
      const AMeasurements: INyxContainerSnapshot; AKeepPresentation: Boolean;
      const AStates: TNyxContentFaceStates;
      AResourceMode: TNyxResourceRenderMode = nrmConfigured;
      AUseContext: Boolean = False; const AContext: INyxApplicationContext = nil;
      const AResolver: INyxLogicResolver = nil;
      const APublicationSource: INyxViewPublicationSource = nil);
  private
    FPanel: TNyxLogicalScrollBox;
    { One detached LCL label measures intrinsic text without changing a live
      face's wrap policy or parent child list. Owned directly until Clear; no
      native window, document node or borrowed parent is retained. }
    FMeasurementLabel: TLabel;
    { Owned only during one synchronous layout pass, lazily acquired on its first
      label measurement. LCL canvas state is restored between labels; no native
      context, parent/control or document reference survives the pass. }
    FMeasurementContext: HDC;
    { Bounded pass-local reuse avoids repeated widgetset measurement while
      recursive preflight/allocation visits the same typography and width.
      Changed captions/fonts/wrap/DPI/width miss; no cache crosses Resize. }
    FLabelMeasurements: array of TNyxLCLLabelMeasurement;
    FVirtualLayout: Boolean;
    FLayouting: Boolean;
    {$ifdef NYX_STUDIO_PROFILE}
    { Per-candidate construction totals locate widget/schema/parenting costs.
      Recursive children contribute once to these fixed stages; no authored
      identity, content or borrowed control escapes through the profiler.
      Validation is nested inside creation; do not sum those two totals. }
    FBuildNodes: QWord;
    FBuildValidationMilliseconds: QWord;
    FBuildCreationMilliseconds: QWord;
    FBuildParentMilliseconds: QWord;
    FBuildBindingMilliseconds: QWord;
    FBuildCompletionMilliseconds: QWord;
    {$endif}
    {$ifdef NYX_LCL_LAYOUT_PROFILE}
    { Optional checked-workload counters; normal builds have no measurement
      logging or clocks. Counts describe traversals, never authored user text. }
    FMeasureCalls: QWord;
    FNaturalCalls: QWord;
    FRowPlanCalls: QWord;
    FLabelMeasurementsTaken: QWord;
    FLabelMeasurementsReused: QWord;
    {$endif}
    FProjecting: Boolean;
    FBindings: TNyxLCLBindings;
    FFactoryKinds: array of TNyxText;
    FDrawingNames: array of TNyxText;
    FDrawingProviders: array of INyxDrawingProvider;
    function DrawingProvider(ANode: TNyxNode): INyxDrawingProvider;
  private
    FFactories: array of TNyxLCLFactory;
    FEventFactories: array of TNyxLCLEventFactory;
    FEmitterScope: INyxEventEmitterScope;
    FViewportObserver: INyxViewportObserver;
    FEditingObserver: INyxLCLEditingObserver;
    FPhysicalFrame: INyxNativeGestureFrame;
    FCaptureObserver: INyxLCLCaptureObserver;
    FOnGestureFailure: TNyxGestureFailure;
    FLastGestureError: TNyxText;
    FUpdaters: array of TNyxLCLUpdater;
    FOnEvent: TNyxLCLEvent;
    FDesignerInput: TNyxDesignerInput;
    FOnDesignerGesture: TNyxDesignerGesture;
    FEvents: INyxEvents;
    { Candidate tables use the accepted parent's router. Their own temporary
      renderer router closes after adoption; mounted input captures the new
      accepted generation only when a real user command starts. }
    FCollectionQueryEvents: INyxEvents;
    FEmbeddedContext: INyxApplicationContext;
    FEmbeddedResolver: INyxLogicResolver;
    FCanvasAdoptionHost: INyxCanvasRouteAdoptionHost;
    FState: TNyxState;
    FOwnState: Boolean;
    FLiveBindings: TNyxLiveBindings;
    FCollectionBindings: INyxCollectionBindings;
    { The accepted own gate and immutable inherited gate have distinct owners.
      Embedded local admission never releases an enclosing staged generation. }
    FCollectionInputGate: INyxCollectionInputGate;
    FInheritedCollectionInputGate: INyxCollectionInputGate;
    FCollectionInputPhase: TNyxCollectionInputPhase;
    { Caller-owned executable registrations affect future candidates only.
      Built-ins compose through that same immutable membership and lifetime;
      accepted ports revoke before widgets, with cleanup deferred past adoption. }
    FCoordinatorRegistry: INyxCoordinatorRegistry;
    { Copied caller membership inherited from the exact parent candidate. It
      retains factories, never a parent renderer, and survives our Unmount as
      configuration. An explicit nonnil registry, even empty, replaces this input. }
    FInheritedCoordinatorSnapshot: INyxCoordinatorRegistrySnapshot;
    { Caller-only membership for this staging/accepted view. Descendant mounts
      use this fixed lease, never a mutable registry or composed built-ins. }
    FCallerCoordinatorSnapshot: INyxCoordinatorRegistrySnapshot;
    FCoordinatorMounts: TNyxCoordinatorMounts;
    FCoordinatorIDs: TNyxCoordinatorMountIDs;
    { The managed target owns no renderer. Revoked before destructor callouts;
      each mounted scope separately guards accepted generation and role lifetime. }
    FCoordinatorRefreshObject: TObject;
    FCoordinatorRefreshTarget: INyxCoordinatorRefreshTarget;
    FUpdating: Boolean;
    FDesignMode: Boolean;
    { Immutable fresh document context, not a retained document or a source
      admission cache. Creator changes force a full candidate mount. }
    FProjectionContext: TNyxText;
    FProjectionSchemaRevision: Integer;
    { Independent last authored projection, never live input or a document
      reference. It distinguishes new defaults from retained runtime edits. }
    FProjectionBaseline: TNyxNode;
    FSelectedDesignID: TNyxText;
    FResizePreview: TNyxResizePreview;
    FDropPreview: TNyxDropPreview;
    FResizeEdges: array[0..7] of TPanel;
    FCanvasResizeGrips: INyxCanvasResizeGrips;
    FCanvasMoveGrip: INyxCanvasMoveGrip;
    FCanvasMoveView: TNyxLCLRenderer;
    FCanvasMoveHost: TPanel;
    FCanvasGripViews: array[TNyxResizeAxis] of TNyxLCLRenderer;
    FCanvasGripHosts: array[TNyxResizeAxis] of TPanel;
    FSelectionEdges: array[0..3] of TShape;
    FForceValues: Boolean;
    { Scoped borrowed scalar rejection target; nil retains full forced sync. }
    FForceValueNode: TNyxNode;
    FOnBindingError: TNyxLCLBindingError;
    FLastBindingError: TNyxText;
    FLastBindingFailure: TNyxBindingFailure;
    procedure UpdateResizePreview;
    procedure ClearCanvasResizeGrips;
    procedure UpdateCanvasResizeGrips;
    procedure ClearCanvasMoveGrip;
    procedure UpdateCanvasMoveGrip;
    function CanvasMovePoint(const APointer: TNyxPointerSnapshot): TNyxResizePoint;
    function CanvasResizePoint(AAxis: TNyxResizeAxis;
      const APointer: TNyxPointerSnapshot): TNyxResizePoint;
    procedure Emit(AOrigin: TNyxNode; const ADispatch: TNyxDispatch);
    procedure BindCollectionPagers;
    procedure ViewportChanged(const AOriginID: TNyxText;
      const AViewport: TNyxViewportSnapshot);
    procedure EditingChanged(const AOriginID: TNyxText;
      const AEditing: TNyxEditingSnapshot);
    procedure CaptureChanged(const AOriginID: TNyxText; ATrigger: TNyxTrigger);
    procedure GestureFailed(const AOriginID, AReason: TNyxText);
    procedure SetDesignerInput(const AValue: TNyxDesignerInput);
    function EmitNamed(const AOriginID: TNyxText; const AName: TNyxEventRef;
      const APayload: TNyxDataValue; AHasPayload: Boolean): Boolean;
    procedure BindingFailed(ANode: TNyxNode; const AReason: TNyxText;
      AFailure: TNyxBindingFailure = nbfRejected; ARestoreInput: Boolean = True);
    procedure ClearRetired(AKeepPresentation: Boolean;
      const AEventView: INyxPreparedEventView);
    procedure Clear(AKeepPresentation: Boolean = False;
      const AEventView: INyxPreparedEventView = nil);
    function FactoryIndex(ANode: TNyxNode): Integer;
    function CreateControl(ANode: TNyxNode; out AInput: TControl;
      out ACaption: TLabel): TControl;
    function Build(ANode: TNyxNode; AParent: TWinControl): TControl;
    function Measure(ANode: TNyxNode; AWidth: Integer;
      AAllocatedWidth: Boolean = False): Integer;
    { Bound all intrinsic exits uniformly; recursive measurements use Measure
      so descendant constraints are admitted before measuring their parent. }
    function MeasureContent(ANode: TNyxNode; AWidth: Integer;
      AAllocatedWidth: Boolean): Integer;
    { Visible flow entries only; hidden controls remain owned and mounted. }
    function VisibleChildren(ANode: TNyxNode): Integer;
    { Platform typography supplies intrinsic widths. Portable allocation owns
      line membership/weights/spacing; no equal-cell fallback changes the policy. }
    function NaturalWidth(ANode: TNyxNode; AAvailable: Integer): Integer;
    function NaturalContentWidth(ANode: TNyxNode; AAvailable: Integer): Integer;
    { Read native group decorations without deriving them from clamped cached
      client sizes. Insets remain measurable while a retained group is parked
      at zero area; they are local adapter geometry, not authored padding. }
    { Native decoration consumes content space, independently of authored outer
      bounds. Physical scrollbars apply only to ordinary native layout; a
      logical projection must never subtract its temporary clipped face. }
    function ContentInsets(ANode: TNyxNode): TRect;
    { Measure copied typography on an owned detached helper. This never changes
      a live label's wrapping or invalidates its ancestors during allocation. }
    procedure LabelSize(ALabel: TLabel; AWidth: Integer; AWrap: Boolean;
      out ATextWidth, ATextHeight: Integer);
    procedure ClearLabelMeasurements;
    function EffectiveWidth(ANode: TNyxNode; AAvailable: Integer;
      AAllocated: Boolean = False): Integer;
    function ColumnChildWidth(AParent, AChild: TNyxNode; AAvailable: Integer): Integer;
    function RowPlan(ANode: TNyxNode; AWidth: Integer; out AWidths, ALefts,
      ATops, AHeights: TNyxFlowSizes): Integer;
    function LayoutColumns(ANode: TNyxNode; AWidth: Integer): Integer;
    procedure Layout(ANode: TNyxNode; AX, AY, AWidth: Integer; AHeight: Integer = -1;
      AAllocatedWidth: Boolean = False);
    function Binding(ANode: TNyxNode): TNyxLCLBinding;
    function IdentityBinding(const AID: TNyxText;
      AIdentity: TNyxIdentityKind): TNyxLCLBinding;
    { Conditions use the borrowed host's stable client rectangle. Internal
      content scrollbars/layout must not create a feedback loop in its size. }
    function UpdateViewport: Boolean;
    function MeasureContainers: INyxContainerSnapshot;
    procedure SetPresentationSelection(const AValue: TNyxPresentationSelection);
    function ReadPresentationSelection: TNyxPresentationSelection;
    function GetPresentationView: INyxPresentationView;
    procedure SetResourceContext(const AContext: INyxResourceContext);
    function GetResourceContext: INyxResourceContext;
    procedure SetResourceUpdates(const AQueue: INyxResourceUpdateQueue);
    procedure SetTableWorkbenchServices(const AServices: INyxTableWorkbenchServices);
    procedure SetTableTextLimits(const ALimits: TNyxTableTextWorkLimits);
    procedure WakeResources;
    procedure Resize(ASender: TObject);
    procedure MountCanvases(ADocument: TNyxDocument);
    { Retry only never-presented nested canvases after this renderer settles
      layout. Initial zero allocation is deferred, not an empty accepted scene;
      canvas WM_SIZE must not construct factories inside candidate alignment. }
    procedure InitializeCanvases;
    procedure MountToolCanvases(ADocument: TNyxDocument);
    procedure ProjectViewport(ASender: TObject);
    procedure ArrangeInput(ABinding: TNyxLCLBinding; AWidth, AHeight: Integer);
    { Adorners share the selected face's immediate paint parent. A graphic at
      the scroll host would be occluded by its full-page native child window. }
    procedure UpdateSelection;
    { LCL removes an unchecked radio's TabStop. Restore one entry per native
      peer parent after all value/policy setters, preferring an enabled checked
      item or the first enabled visible item. Creator widgets retain ownership. }
    procedure SyncRadioFocus;
    { Consumes the coordinator's fully admitted value-only synchronization plan.
      Unsupported/custom/content-sized fields retain ordinary complete Sync. }
    function SyncEditedValue(ANode: TNyxNode): Boolean;
    { Reparent admitted ordinary hosts without replacing controls/bindings.
      Preserve every supported editing range and suppress incidental events. }
    procedure ArrangeControls(AFormer: TNyxNode);
    { One native presentation boundary shared by visibility and focus recovery.
      Exact entered controls stay leased by their original physical frame. }
    procedure ChangePresentationPlane(APlane: TWinControl; AVisible: Boolean;
      AFocus: TWinControl; const ASelection: TNyxTextSelection;
      ASynchronize: Boolean);
  public
    { Internal cleanup shell, never a mounted renderer. Public visibility keeps
      FPC constructor semantics explicit; callers use Create for usable views. }
    constructor CreateRetirementShell;
    constructor Create(ATheme: TNyxTheme = nil);
    destructor Destroy; override;
    { UI-thread registration before mounting. Named implementations are managed,
      copied into independent candidates/embedded canvas renderers, and must
      never retain a renderer/document/view cycle. Unknown names refuse. }
    procedure RegisterDrawingProvider(const AName: TNyxDrawingProviderRef;
      const AProvider: INyxDrawingProvider);
    procedure RegisterFactory(const AKind: TNyxText; AFactory: TNyxLCLFactory;
      AUpdater: TNyxLCLUpdater = nil);
    { Replace the exact kind's projection with a declared named-event producer.
      Existing native factory/updater ownership contracts still apply. }
    procedure RegisterEventFactory(const AKind: TNyxKindRef;
      AFactory: TNyxLCLEventFactory; AUpdater: TNyxLCLUpdater = nil);
    { Actual target capability, including explicitly registered custom factories
      and text fallbacks. Does not imply full production acceptance. }
    function Capability(ANode: TNyxNode): TNyxCapability;
    { Supplied state is borrowed and must outlive the mounted view. Otherwise a
      fresh owned copy of authored defaults is used. Rejected physical edits are
      restored without rebuilding controls or showing a platform exception box. }
    procedure Render(ADocument: TNyxDocument; ARoot: TNyxNode; AHost: TWinControl;
      AState: TNyxState = nil; const ACollections: INyxCollectionBindings = nil); overload;
    { Designer purpose matches the browser overload: selection/value proposals
      reach OnEvent as ntDesignSelect/ntDesignValue, while application actions,
      callbacks, collection edits and drag negotiation stay inactive. Editable
      native text faces keep their ordinary widgetset input/IME behavior. The
      realized tree is independent; an editor admits proposals into its own
      document through an undoable command. Runtime rendering remains default. }
    procedure Render(ADocument: TNyxDocument; ARoot: TNyxNode; AHost: TWinControl;
      ADesignMode: Boolean; AState: TNyxState = nil;
      const ACollections: INyxCollectionBindings = nil;
      AResourceMode: TNyxResourceRenderMode = nrmConfigured); overload;
    { Stage/validate an independent realized view, then retain native controls
      when only shared supported scalar presentation differs. False changes
      nothing and asks the caller to Render normally. Structure, metadata,
      scalar bindings, defaults, collections, theme or custom factories refuse
      reuse. No accepted document/node is borrowed after return. Ordinary Sync
      updates values/layout; widget failures restore the previous properties.
      Explicit typed restores reset only their exact fields to current authored
      defaults; other runtime drafts remain untouched. Invalid restores refuse
      before mutation. Call on the owning UI thread with the mounted host alive. }
    function TryRefresh(ADocument: TNyxDocument; ARoot: TNyxNode;
      ADesignMode: Boolean; const ARestores: TNyxProjectionValueRestores = nil): Boolean;
    { Explicit form-owner changes over the same native controls/windows. Only
      named metadata keys in the exact supplied forms may differ; ordinary
      scalar refresh remains strict. Nil refuses before mutation. A nonnil
      publication owns its reversible preview, copied drafts and event fence
      until grouped installation or retirement. Changed forms reset accepted
      faces even when defaults are unchanged; unrelated drafts remain intact.
      No caller document is retained. The host stays borrowed through retirement.
      Call on the UI thread; a retired generation never writes a successor. }
    function TryPrepareFormRefresh(ADocument: TNyxDocument; ARoot: TNyxNode;
      const AForms: TNyxProjectionFormContexts;
      ADesignMode: Boolean = False): INyxPreparedPublication;
    { Copied physical continuity, independent of accepted model values. Capture
      and restore require an idle mounted view; False changes nothing. Records
      retain no renderer, control, model or store. Restore matches exact logical
      identity/domain/bindings and accepted value before reusing a text draft.
      Replay the model/source baseline first. This never edits that model or
      history, admits a value, dispatches input or reserves a publication turn.
      Compatible views may reuse these copies; changed contracts skip drafts.
      Ambiguous identities raise before touching faces; target errors propagate.
      Extension-owned editors require their own adapter continuity contract. }
    function CaptureInteraction(out AStates: TNyxContentFaceStates): Boolean;
    function RestoreInteraction(const AStates: TNyxContentFaceStates;
      AFocus: Boolean = True): Boolean;
    { Move the same mounted view to a different borrowed host, retaining control
      objects, state/subscriptions, focused text range and containing scroll.
      Both hosts must outlive their respective parentage; after successful move
      the old host can be freed. Nil, an unmounted view, a descendant or occupied
      host refuses before changing parenting. A same-host request is a no-op.
      No document/history command occurs. }
    procedure MoveHost(AHost: TWinControl);
    { Visibility-only adapter transaction over this exact borrowed presentation
      plane. Its parent never changes and its caller owns its lifetime. Native
      focus/validation/layout callbacks are muted; all grid editors retain their
      drafts and handles. A later ordinary Sync owns final visible allocation. }
    procedure SetPresentationPlaneVisible(APlane: TWinControl; AVisible: Boolean);
    { Rollback to this exact borrowed plane, run ordinary updater/layout Sync,
      then recover its borrowed focused descendant and scalar selection. The
      entered frame and grid validation guards span the entire operation;
      authored focus callbacks stay muted during native visibility/focus writes. }
    procedure RestorePresentationPlane(APlane: TWinControl; AFocus: TWinControl;
      const ASelection: TNyxTextSelection);
    { Highlight the outermost realized face of this authored design identity.
      Empty/absent identities clear the outline without taking keyboard focus.
      Selection is editor presentation and never mutates the owned document. }
    procedure Select(const ADesignID: TNyxText);
    { Presentation only: preview the selected authored face at a proposed size.
      Default clears it. Runtime views or another selection refuse before the
      current outline changes. Inert paint strips above the scroll surface
      retain input/focus and clip to the viewport without changing scroll extent. }
    procedure PreviewResize(const APreview: TNyxResizePreview);
    { Retain the independent public Nyx adornment until its nested view is gone. }
    procedure AttachMoveGrip(const AGrip: INyxCanvasMoveGrip);
    { Borrow the currently mounted public button; missing adornment refuses. }
    function CanvasMoveControl: TControl;
    { Retain public specialized Nyx button adornments for the selected authored
      face. Nil detaches. Separate runtime scopes do not enable application
      callbacks in the edited tree. Same attachment preserves target identity,
      focus/capture. Disconnect borrowed editor receivers before destroying them. }
    procedure AttachResizeGrips(const AGrips: INyxCanvasResizeGrips);
    { Borrowed mounted button for actual target input qualification. An absent
      adornment refuses. Never free this renderer-owned control. }
    function CanvasResizeControl(AAxis: TNyxResizeAxis): TControl;
    { Compose the actual native proposal windows into a caller-owned capture.
      AOrigin is the screen coordinate represented by canvas pixel (0,0).
      Win32's whole-form WM_PRINT can omit sibling child-window overlays;
      individual LCL PaintTo preserves their real widget paint. No synthesized
      rectangles, controls, focus or document mutation; active visible strips
      and mounted canvas grips are printed. This is offscreen capture. }
    procedure PaintResizePreview(ACanvas: TCanvas; const AOrigin: TPoint);
    { Reveal an exact mounted face through nested scroll scopes and the containing
      viewport. Presentation only: retain controls, focus, text/caret and authored
      document/history. Missing IDs refuse before movement; ordinary native views
      use LCL ScrollInView from the innermost ancestor outward, bounded by this
      renderer's own panel. Detached/parked ordinary faces are harmless no-ops;
      borrowed application hosts outside that panel are never scrolled here. }
    procedure Reveal(const AID: TNyxText;
      AIdentity: TNyxIdentityKind = niAutomatic);
    { Actual containing view offsets/extents in logical pixels, independent of
      whether the selected node declares its own internal scrolling. }
    function ViewViewport: TNyxViewportSnapshot;
    { Set containing-view logical pixel offsets, clamped by its actual extent.
      A mounted view is required. This changes presentation only. }
    procedure ScrollView(AX, AY: Integer);
    { Borrow a projected control/host through the same stable identity contract
      as the browser adapter. Caller must not free this renderer-owned control. }
    function ControlFor(const AID: TNyxText;
      AIdentity: TNyxIdentityKind = niAutomatic): TControl;
    { Own a reference to an ordinarily mounted workspace. Its host stays
      renderer-owned; Clear/replacement disconnects any retained interface. }
    function CanvasView(const AID: TNyxText;
      AIdentity: TNyxIdentityKind = niAutomatic): INyxCanvasWorkspaceView;
    { Borrow the actual input through its node identity, independently of the
      adapter's caption/frame hierarchy. Returns nil for mounted non-inputs;
      unmounted identities raise the same error as ControlFor. Never free it. }
    function InputFor(const AID: TNyxText;
      AIdentity: TNyxIdentityKind = niAutomatic): TControl;
    { Exact runtime identity for a currently borrowed native input. Empty for
      an unrelated/non-input control. One binding pass; nothing is retained. }
    function InputIdentity(AInput: TControl): TNyxText;
    { Copy the runtime reference for an exact projected control or its separate
      input. Nil/foreign controls return undefined. One binding pass retains
      nothing, dereferences no supplied widget and performs no ancestor guess. }
    function ControlReference(AControl: TControl): TNyxControlRef;
    { Borrow the actual keyboard/focus face, including a split separator's grip.
      A component without a declared face returns nil; a missing identity raises.
      Collection attachments manage entry within their returned host. The caller
      must not free this control or retain it beyond the mounted view. }
    function FocusFor(const AID: TNyxText;
      AIdentity: TNyxIdentityKind = niAutomatic): TWinControl;
    { Restore one mounted keyboard face without revealing it. Missing identity
      follows FocusFor; absent/nonfocusable faces are harmless. Focus callbacks
      retain their ordinary behavior and may redirect focus or destroy this view.
      Suppression applies only to this resolved runtime target/current generation,
      including nested replay calls; no widget access follows SetFocus. }
    procedure FocusWithoutScroll(const AID: TNyxText;
      AIdentity: TNyxIdentityKind = niAutomatic);
    { Runtime text selection uses Unicode scalar offsets against physical text,
      retaining native line endings. Neither method forces focus or scrolling. }
    function TextSelectionFor(const AID: TNyxText): TNyxTextSelection;
    function EditingFor(const AID: TNyxText): TNyxEditingSnapshot;
    procedure SetTextSelection(const AID: TNyxText; const ASelection: TNyxTextSelection);
    { Install optional managed history ownership for this physical text mount.
      Rebind after replacement; ordinary reparent/publication preserves the port. }
    procedure BindTextHistory(const AID: TNyxText;
      const ACommands: INyxTextHistoryCommands);
    { Read actual offsets without changing document state or selection.
      Units belong to each axis; native widget ranges are never labeled pixels. }
    function ViewportFor(const AID: TNyxText): TNyxViewportSnapshot;
    { Copied allocated outer face, not the input client area or clipped physical
      window. Exact identity admission matches ControlFor; zero is meaningful. }
    function SizeFor(const AID: TNyxText;
      AIdentity: TNyxIdentityKind = niAutomatic): TNyxResizeSize;
    { Copied visible immediate siblings from admitted logical allocation,
      independent of child-window clipping. Roots have no guide context;
      missing identity raises. No control, binding or model is retained. }
    function AlignmentFor(const AID: TNyxText;
      AIdentity: TNyxIdentityKind = niAutomatic): TNyxAlignmentContext;
    { Copied screen-plane mapping for reusable gesture grips. Native logical
      allocations currently use LCL pixels; no widget is retained by the value. }
    function ScreenPointFor(const AID: TNyxText;
      const APointer: TNyxPointerSnapshot): TNyxResizePoint;
    { Copied exact runtime primitive allocation and screen face. No authored
      owner fallback; virtual offsets remain in the logical face's geometry. }
    function DropFrameFor(const AID: TNyxText): TNyxDropFrame;
    { Inert standard-panel paint. Default clears; selection/unmount/resize
      retires this proposal before borrowed target controls are released. }
    procedure PreviewDrop(const APreview: TNyxDropPreview);
    function LogicalPointFor(const AID: TNyxText;
      const AScreen: TNyxResizePoint; AIdentity: TNyxIdentityKind = niAutomatic): TNyxResizePoint;
    { Shared one-based source navigation for the public code-editor component. }
    { One-based Unicode-scalar column, translated to the widgetset's caret units.
      Win32 memo columns use UTF-16 units, including both units of a surrogate.
      A hidden/inactive host retains the caret without requesting unavailable
      focus; visible focusable editors take ordinary keyboard focus. }
    procedure NavigateCodeLine(const AID: TNyxText; ALine: Integer; AColumn: Integer = 1);
    { Release a view before freeing a containing native host. LCL parenting
      destroys child controls, so the host must outlive the mounted renderer. }
    procedure Unmount;
    { Managed typed data attachment to an existing Nyx list/table/tree control.
      Unmount/remount disconnects it before freeing borrowed native handles.
      A control accepts one live attachment; retained disconnected interfaces
      remain safe and never retain its model or widget tree. }
    function BindCollection(const AID: TNyxText;
      const AView: INyxCollectionView): INyxCollectionMount;
    { Retain the automatically mounted typed view by exact runtime identity. }
    function CollectionView(const AID: TNyxText): INyxCollectionView;
    { Retain the existing target attachment for typed runtime keyboard policy.
      Does not rebind or replace the control. Retained handles disconnect on
      Unmount and never retain this renderer. Exact realized identity required. }
    function CollectionMount(const AID: TNyxText): INyxCollectionMount;
    { Retained portable pager attachment; it disconnects before this view retires. }
    function CollectionPager(const AID: TNyxText): INyxCollectionPager;
    { Exact runtime identity; a static/disabled/unmounted board returns nil. }
    function KanbanBinding(const AID: TNyxText): INyxKanbanBinding;
    function MasterDetailBinding(const AID: TNyxText): INyxMasterDetailBinding;
    function CoordinatorBinding(const AID: TNyxText): INyxCoordinatorBinding;
    { Register trusted Pascal factories before the next candidate. Nil inherits
      an embedded parent's fixed caller snapshot; nonnil, including empty,
      overrides callers only. Built-ins compose independently. Changes never
      rewrite a mounted snapshot or load code from project data. }
    property CoordinatorRegistry: INyxCoordinatorRegistry
      read FCoordinatorRegistry write FCoordinatorRegistry;
    procedure Sync;
    property OnEvent: TNyxLCLEvent read FOnEvent write FOnEvent;
    { Configure before mounting. Opted-in designer drops use a synchronous
      value-only receiver and keep application/custom native callbacks bypassed.
      Clear this borrowed receiver before releasing its object. }
    property DesignerInput: TNyxDesignerInput read FDesignerInput write SetDesignerInput;
    property OnDesignerGesture: TNyxDesignerGesture read FOnDesignerGesture write FOnDesignerGesture;
    { Multiple registrations share the portable event/scheduler contract.
      View replacement cancels queued work; destruction closes registrations. }
    property Events: INyxEvents read FEvents;
    { Optional public compound inspector. Missing/retyped identities refuse;
      its target renderer is borrowed only through the target view capability. }
    function ToolCanvasView(const AID: TNyxText;
      AIdentity: TNyxIdentityKind = niAutomatic): INyxToolCanvasView;
    { Explicit weak runtime authority for nested canvas/control views. Configure
      while unmounted; copied candidates retain no application/renderer cycle.
      Ordinary root callbacks/routes remain owned by their application host. }
    procedure ConfigureEmbeddedBehavior(const AContext: INyxApplicationContext;
      const AResolver: INyxLogicResolver = nil);
    { Explicit project host for subsequently mounted Tool Canvas faces. Retained
      faces keep their original issuing host; design-mode faces stay runtime-only. }
    property CanvasAdoptionHost: INyxCanvasRouteAdoptionHost
      read FCanvasAdoptionHost write FCanvasAdoptionHost;
    { Atomically stage an explicit nested-view runtime context with this frame.
      Existing controls keep their former context until admission succeeds.
      The host owns State/context lifetime and root callback/route attachment;
      nested canvas fragments use the context through their existing adapters.
      An optional managed source joins root behavior/runtime ownership to this
      accepted view boundary; notification failure leaves adoption committed. }
    procedure RenderContext(ADocument: TNyxDocument; ARoot: TNyxNode;
      AHost: TWinControl; ADesignMode: Boolean; AState: TNyxState;
      const ACollections: INyxCollectionBindings; const AContext: INyxApplicationContext;
      const AResolver: INyxLogicResolver = nil;
      AResourceMode: TNyxResourceRenderMode = nrmConfigured;
      const APublicationSource: INyxViewPublicationSource = nil);
    property Root: TNyxNode read FRoot;
    { UI-thread, view-local exclusive choice. A mounted view is required;
      unknown/automatic names refuse before mutation. Scalar-only views refresh
      immediately. Content recipes coalesce requests through an idle UI job;
      reads expose the last accepted choice until that job publishes. Compatible
      explicit named parts carry input state; changed contracts are new fields. }
    property PresentationSelection: TNyxPresentationSelection read FPresentationSelection write SetPresentationSelection;
    { Portable managed capability. Borrowed receiver links retire on Unmount;
      caller-retained interfaces stay safe and never retain the renderer. }
    property Presentations: INyxPresentationView read GetPresentationView;
    property DesignMode: Boolean read FDesignMode;
    property State: TNyxState read FState;
    { Reload copied resource values/locale in this mounted view. No saved
      defaults or sibling runtime is changed; invalid data preserves controls. }
    { Explicit immutable application context persists across Render/navigation
      and responsive rebuilds. Configure while unmounted; mounted updates use
      ReloadResources, whose accepted publication also advances this context. }
    property ResourceContext: INyxResourceContext read GetResourceContext write SetResourceContext;
    { Install while unmounted. The port owns no renderer; the application must
      stop it before destroying receivers. Deferred Wake cannot publish here. }
    property ResourceUpdates: INyxResourceUpdateQueue read FResourceUpdates write SetResourceUpdates;
    { Configure before Render or after explicit Unmount, with no active render
      attempt. Hidden candidates and embedded children copy this lease; existing
      mounts keep their original clipboard host. Nil reports Unavailable. }
    property TableWorkbenchServices: INyxTableWorkbenchServices
      read FTableWorkbenchServices write SetTableWorkbenchServices;
    { Set before mounting or after Unmount. No runtime work limit is saved in
      a document, and already entered mounts keep their original copied limits. }
    property TableTextLimits: TNyxTableTextWorkLimits
      read FTableTextLimits write SetTableTextLimits;
    function ResourceReady: Boolean;
    { View-section publication requires an idle input/model boundary. False
      during synchronous event/editing/gesture dispatch, scalar/collection
      notifications or target synchronization. Queue replacement to a later
      UI turn; this does not reserve the renderer or validate a candidate. }
    function SectionPublicationReady: Boolean;
    { False means busy; invalid selected values raise before publication.
      The detached check includes the currently mounted target projection. }
    { Readiness and mounted scalar preflight. Source rows/hidden scopes admit in
      the context's prepared phase; True is not evidence of full publication. }
    function CanReloadResources(const AContext: INyxResourceContext): Boolean;
    { Scalar/context participant for an application that separately prepares its
      shared row context. Standalone callers use ReloadResources, which prepares
      both. No target synchronization occurs before the group installs. }
    function PrepareResourcePublication(const AContext: INyxResourceContext): INyxPreparedPublication;
    { Standalone embedded scopes own their source-backed rows as well as scalar
      bindings. Prepare both for one outer publication without synchronizing
      targets early. Application hosts with shared row ownership keep using
      PrepareResourcePublication. Retire an abandoned result before teardown. }
    function PrepareResourceReload(const AContext: INyxResourceContext): INyxPreparedPublication;
    procedure ReloadResources(const AResources: INyxResources;
      const ALocale, AFallback: TNyxLocaleRef);
    property OnBindingError: TNyxLCLBindingError read FOnBindingError write FOnBindingError;
    property LastBindingError: TNyxText read FLastBindingError;
    property LastBindingFailure: TNyxBindingFailure read FLastBindingFailure;
    { Deferred structural failure retains the accepted view; a new geometry or
      explicit manual request retries without an identical-frame retry storm. }
    property LastContentError: TNyxText read FLastContentError;
    { Owned host refusal. The optional sink may navigate or dispose this view. }
    property OnGestureFailure: TNyxGestureFailure read FOnGestureFailure write FOnGestureFailure;
    property LastGestureError: TNyxText read FLastGestureError;
  end;

implementation

uses
  InterfaceBase,
  LCLIntf, nyx.canvas.lcl, nyx.canvas.scene, nyx.tool.canvas.lcl,
  nyx.collections, nyx.collections.query.lcl,
  nyx.collections.arrangement.lcl,
  nyx.collections.cell.lcl,
  nyx.application.resources, nyx.projection.context;

type

  { Internal control-flow unwind after the exact native receiver retires.
    Event boundaries consume it; it is never a successor binding diagnostic. }
  ENyxNativeGenerationRetired = class(ENyxModel);

  { One ordinary raw-object owner for exact built-in theme/cache arguments.
    Entered generations retain these leases through native helper callouts. }
  TNativeRenderObject = class(TInterfacedObject)
  private
    FValue: TObject;
  public
    constructor Create(AValue: TObject);
    destructor Destroy; override;
  end;

  { An accepted generation is never retargeted. The fresh detached candidate
    adopts its one final receiver at commit. Revoke detaches authority at once;
    raw coordinator/bindings/root/state remain leased through entered calls.
    No native panel is owned here: the SAME existing physical frame owns its
    deferred widget retirement independently of these portable model leases. }
  TNativeBindingGeneration = class(TInterfacedObject, INyxLCLBindingGeneration)
  public
    Owner: TNyxLCLRenderer;
    Live: TNyxLiveBindings;
    Root: TNyxNode;
    OwnedState: INyxBindingOwnedState;
    Source: INyxViewPublicationSource;
    ThemeOwner: IInterface;
    BaseThemeOwner: IInterface;
    GlyphCacheOwner: IInterface;
    Frame: INyxNativeGestureFrame;
    RetiredBindings: TNyxLCLBindings;
    constructor Create(AOwner: TNyxLCLRenderer);
    destructor Destroy; override;
    procedure Revoke;
    function Current: Boolean;
    function Coordinator: TNyxLiveBindings;
    function PhysicalFrame: INyxNativeGestureFrame;
  end;

  { The FIRST component owned by a physical panel is destroyed after its later
    controls. It keeps the exact retired generation/tree/assets alive through
    their destructors, including the existing deferred native frame release.
    It owns no panel or renderer; the frame detaches its queued panel batch
    before freeing it, so asynchronous retirement breaks the temporary lease. }
  TNativePanelGenerationLease = class(TComponent)
  private
    FGeneration: INyxLCLBindingGeneration;
  public
    constructor CreateFor(AOwner: TComponent;
      const AGeneration: INyxLCLBindingGeneration);
  end;

  { Semantic phase readers retain the entered coordinator and reject a retired
    generation after each real router read. The dispatcher never borrows a
    method pointer from a coordinator that cleanup can free synchronously. }
  TNativeSignalSnapshot = class(TInterfacedObject)
  private
    FGeneration: INyxLCLBindingGeneration;
    FEvents: INyxEvents;
    FRevision: Integer;
  public
    constructor Create(const AGeneration: INyxLCLBindingGeneration;
      const AEvents: INyxEvents; ARevision: Integer);
    function Read(ANode: TNyxNode; ATrigger: TNyxTrigger): TNyxEventInfo;
  end;

  { Final presentation receiver is weak. A scope retains this managed port,
    never a renderer/controller; Sync is the last outward action in Request. }
  TNyxLCLCoordinatorRefresh = class(TInterfacedObject, INyxCoordinatorRefreshTarget)
  public
    Owner: TNyxLCLRenderer;
    constructor Create(AOwner: TNyxLCLRenderer);
    procedure Request;
  end;
  { One immutable target lease. The existing cancellation token remains safe
    after its mount retires, with no native pointer or renderer back reference. }
  TNyxLCLFocusReplayTarget = class(TInterfacedObject, TNyxLCLRenderer.IFocusReplayTarget)
  private
    FID: TNyxText;
    FGeneration: INyxCancellationScope;
  public
    constructor Create(const AID: TNyxText; const AGeneration: INyxCancellationScope);
    function Preserves(const AID: TNyxText): Boolean;
  end;
  { Value-only replay policy, independently retained across native callbacks.
    There is no renderer, control, event-router or document back reference. }
  TNyxLCLFocusReplay = class(TInterfacedObject, TNyxLCLRenderer.IFocusReplay)
  private
    FTarget: TNyxLCLRenderer.IFocusReplayTarget;
  public
    function GetTarget: TNyxLCLRenderer.IFocusReplayTarget;
    procedure SetTarget(const ATarget: TNyxLCLRenderer.IFocusReplayTarget);
    function Preserves(const AID: TNyxText): Boolean;
  end;

constructor TNyxLCLFocusReplayTarget.Create(const AID: TNyxText;
  const AGeneration: INyxCancellationScope);
begin
  inherited Create;
  FID := AID;
  FGeneration := AGeneration;
end;

function TNyxLCLFocusReplayTarget.Preserves(const AID: TNyxText): Boolean;
begin
  Result := (FID <> '') and (FID = AID) and
    (FGeneration <> nil) and not FGeneration.Cancelled;
end;

function TNyxLCLFocusReplay.GetTarget: TNyxLCLRenderer.IFocusReplayTarget;
begin
  Result := FTarget;
end;

procedure TNyxLCLFocusReplay.SetTarget(const ATarget: TNyxLCLRenderer.IFocusReplayTarget);
begin
  FTarget := ATarget;
end;

function TNyxLCLFocusReplay.Preserves(const AID: TNyxText): Boolean;
begin
  Result := (FTarget <> nil) and FTarget.Preserves(AID);
end;

procedure TNyxLCLRenderer.NavigateCodeLine(const AID: TNyxText; ALine: Integer; AColumn: Integer);
var
  LInput: TControl;
  LLine: Integer;
  LText: TNyxText;
  LIndex, LScalar, LColumn, LUnits: Integer;
begin
  LInput := InputFor(AID);

  if not (LInput is TMemo) or (ALine < 1) or (AColumn < 1) then
  begin
    raise ENyxModel.Create('Source navigation requires a code editor and source line');
  end;
  LLine := ALine - 1;

  if LLine >= TMemo(LInput).Lines.Count then
  begin
    LLine := TMemo(LInput).Lines.Count - 1;
  end;

  if LLine < 0 then
  begin
    LLine := 0;
  end;
  LText := '';

  if TMemo(LInput).Lines.Count > 0 then
  begin
    LText := TMemo(LInput).Lines[LLine];
  end;
  LIndex := 1;
  LColumn := 1;
  LUnits := 0;
  while (LIndex <= Length(LText)) and (LColumn < AColumn) do
  begin

    if not NyxNextScalar(LText, LIndex, LScalar) then
    begin
      raise ENyxModel.Create('Source editor contains malformed Unicode');
    end;
    Inc(LColumn);
    Inc(LUnits);
    {$ifdef WINDOWS}

    if LScalar > $ffff then
    begin
      Inc(LUnits);
    end;
    {$endif}
  end;
  TMemo(LInput).CaretPos := Point(LUnits, LLine);
  { Embedded or inactive project hosts can be hidden. The requested caret still
    belongs to their owned editor; focus is available only after its host can
    receive it. Hidden navigation must not turn an admitted pair into a native
    form exception. LCL CanFocus excludes the form; CanSetFocus also qualifies
    that containing host. Visible Studio navigation takes normal focus. }

  if TMemo(LInput).CanSetFocus then
  begin
    TMemo(LInput).SetFocus;
  end;
  { Win32's memo CaretPos setter changes selection without scrolling it into
    view. Use the public LCL selection setter to reveal that same exact caret
    after explicit navigation. Ordinary synchronization continues to preserve
    the user's viewport through its separate non-scrolling selection bridge. }
  TMemo(LInput).SelStart := TMemo(LInput).SelStart;
end;

type
  { Measure with the widgetset's label implementation without changing a live
    label or creating a native parent. The renderer owns this detached helper.
    A temporary screen context supplies its canvas only during measurement. }
  TNyxIntrinsicLabel = class(TLabel)
  public
    procedure TextSize(AAvailableWidth: Integer; out AWidth, AHeight: Integer;
      AContext: HDC);
  end;
  { Access shared focus events without imposing a specific native input class. }
  TNyxWinControlAccess = class(TWinControl);
  TNyxControlAccess = class(TControl);
  { LCL owns this object from OnStartDrag until EndDrag finishes. Its payload and
    identities are owned values; the router revision revokes an obsolete view.
    Hover acceptance and committed drop results are deliberately separate. }
  TNyxNativeDragObject = class(TDragControlObjectEx)
  public
    Transfer: TNyxTransferSnapshot;
    Allowed: TNyxDropOperations;
    SourceID: TNyxText;
    Events: INyxEvents;
    Revision: Integer;
    Offered: Boolean;
    HoverID: TNyxText;
    HoverOperation: TNyxDropOperation;
    Operation: TNyxDropOperation;
    function Live: Boolean;
  end;

procedure TNyxIntrinsicLabel.TextSize(AAvailableWidth: Integer;
  out AWidth, AHeight: Integer; AContext: HDC);
var
  LContext: HDC;
  LOwnContext: Boolean;
begin
  LContext := AContext;
  LOwnContext := LContext = 0;

  if LOwnContext then
  begin
    LContext := LCLIntf.GetDC(0);
  end;

  if LContext = 0 then
  begin
    raise ENyxModel.Create('The native label measurement context is unavailable');
  end;
  try
    { CalculateSize preserves the canvas handle and applies LCL's font,
      Unicode, accelerator, multiline and bidi rules. Supply a handle first:
      a detached TControlCanvas cannot acquire one through a borrowed parent.
      The caller supplies either the allocated wrapping width or LCL's wide
      intrinsic rectangle. Neither operation touches an attached control. }
    Canvas.Handle := LContext;
    { LCL's canvas state contract works across widgetsets. Each label receives
      an isolated handle state even while its drawing context is shared. }

    if not LOwnContext then
    begin
      Canvas.SaveHandleState;
    end;
    try
      CalculateSize(AAvailableWidth, AWidth, AHeight);
    finally

      if not LOwnContext then
      begin
        Canvas.RestoreHandleState;
      end;
    end;
  finally
    Canvas.Handle := 0;

    if LOwnContext then
    begin
      LCLIntf.ReleaseDC(0, LContext);
    end;
  end;
end;

function TNyxNativeDragObject.Live: Boolean;
begin
  Result := Offered and (Events <> nil) and (Events.ViewRevision = Revision);
end;

function ThemeColor(const AValue: TNyxText): TColor;
var
  LRGB: Integer;
begin
  LRGB := NyxThemeRGB(AValue);
  Result := RGBToColor((LRGB shr 16) and $ff, (LRGB shr 8) and $ff, LRGB and $ff);
end;

function Metric(ANode: TNyxNode; const AKey: TNyxText; ADefault: Integer): Integer;
begin
  Result := StrToIntDef(ANode.Prop(AKey), ADefault);

  if Result < 0 then
  begin
    Result := 0;
  end;

  if Result > 100000 then
  begin
    Result := 100000;
  end;
end;

constructor TNyxLCLCoordinatorRefresh.Create(AOwner: TNyxLCLRenderer);
begin
  inherited Create;
  Owner := AOwner;
end;

procedure TNyxLCLCoordinatorRefresh.Request;
var
  LLease: INyxCoordinatorRefreshTarget;
  LOwner: TNyxLCLRenderer;
begin
  LLease := Self;
  LOwner := Owner;

  if LOwner <> nil then
  begin
    LOwner.Sync;
  end;
end;

function NativeGenerationCurrent(const AGeneration: INyxLCLBindingGeneration): Boolean;
begin
  Result := (AGeneration = nil) or AGeneration.Current;
end;

procedure RequireNativeGeneration(const AGeneration: INyxLCLBindingGeneration);
begin

  if not NativeGenerationCurrent(AGeneration) then
  begin
    raise ENyxNativeGenerationRetired.Create('Native binding generation retired during callback');
  end;
end;

constructor TNativePanelGenerationLease.CreateFor(AOwner: TComponent;
  const AGeneration: INyxLCLBindingGeneration);
begin
  inherited Create(AOwner);
  FGeneration := AGeneration;
end;

constructor TNativeSignalSnapshot.Create(
  const AGeneration: INyxLCLBindingGeneration; const AEvents: INyxEvents;
  ARevision: Integer);
begin
  inherited Create;
  FGeneration := AGeneration;
  FEvents := AEvents;
  FRevision := ARevision;
end;

function TNativeSignalSnapshot.Read(ANode: TNyxNode;
  ATrigger: TNyxTrigger): TNyxEventInfo;
var
  LRevision: Integer;
begin
  LRevision := FEvents.ViewRevision;
  RequireNativeGeneration(FGeneration);

  if LRevision <> FRevision then
  begin
    raise ENyxNativeGenerationRetired.Create('Native event view replaced during callback');
  end;
  Result := FGeneration.Coordinator.SignalSnapshot(ANode, ATrigger);
  RequireNativeGeneration(FGeneration);
end;

constructor TNativeRenderObject.Create(AValue: TObject);
begin
  inherited Create;
  FValue := AValue;
end;

destructor TNativeRenderObject.Destroy;
var
  LValue: TObject;
begin
  LValue := FValue;
  FValue := nil;
  try
    LValue.Free;
  finally
    inherited Destroy;
  end;
end;

constructor TNativeBindingGeneration.Create(AOwner: TNyxLCLRenderer);
var
  LRoot: TNyxNode;
begin
  inherited Create;
  { Acquire the extra tree reference before adopting any raw cleanup duty. A
    failed acquisition leaves the candidate's original ownership untouched. }
  LRoot := AOwner.FRoot;
  LRoot.AcquireReference;
  Root := LRoot;
  OwnedState := AOwner.FOwnedState;
  Source := AOwner.FViewPublicationSource;
  ThemeOwner := AOwner.FThemeOwner;
  BaseThemeOwner := AOwner.FBaseThemeOwner;
  GlyphCacheOwner := AOwner.FGlyphCacheOwner;
  Frame := AOwner.FPhysicalFrame;
  Live := AOwner.FLiveBindings;
  Owner := AOwner;
end;

procedure TNativeBindingGeneration.Revoke;
begin
  Owner := nil;

  if Live <> nil then
  begin
    Live.Retire;
  end;
end;

destructor TNativeBindingGeneration.Destroy;
var
  I: Integer;
  LLive: TNyxLiveBindings;
  LRoot: TNyxNode;
  LBindings: TNyxLCLBindings;
begin
  { Final cleanup detaches its exact raw duties before releasing anything.
    Subscription authority was normally retired at Clear entry; this also
    covers a candidate abandoned before activation. }
  Owner := nil;
  LLive := Live;
  Live := nil;
  LRoot := Root;
  Root := nil;
  LBindings := RetiredBindings;
  RetiredBindings := nil;
  try
    try

      if LLive <> nil then
      begin
        try
          LLive.Retire;
        finally
          LLive.Free;
        end;
      end;
    finally
      for I := 0 to High(LBindings) do
      begin
        LBindings[I].Free;
      end;
    end;
  finally
    try

      if LRoot <> nil then
      begin
        LRoot.ReleaseReference;
      end;
    finally
      Source := nil;
      OwnedState := nil;
      inherited Destroy;
    end;
  end;
end;

function TNativeBindingGeneration.Current: Boolean;
begin
  Result := Owner <> nil;
end;

function TNativeBindingGeneration.Coordinator: TNyxLiveBindings;
begin
  Result := Live;
end;

function TNativeBindingGeneration.PhysicalFrame: INyxNativeGestureFrame;
begin
  Result := Frame;
end;

function TNyxLCLBinding.GenerationLease: INyxLCLBindingGeneration;
begin
  Result := nil;

  if FGeneration <> nil then
  begin
    Result := TNativeBindingGeneration(FGeneration);
  end;
end;

{$I nyx.projection.context.adapter.inc}

constructor TNyxLCLRenderer.CreateRetirementShell;
begin
  inherited Create;
  FRetirementShell := True;
end;

constructor TNyxLCLRenderer.Create(ATheme: TNyxTheme);
begin
  inherited Create;
  FTableTextLimits := TNyxTableTextWorkLimits.Defaults;
  FFocusReplay := TNyxLCLFocusReplay.Create;
  FButtonGlyphCache := TNyxLCLButtonGlyphCache.Create;
  FGlyphCacheOwner := TNativeRenderObject.Create(FButtonGlyphCache);
  FTheme := ATheme;
  FBorrowedTheme := ATheme;
  FOwnTheme := ATheme = nil;

  if FOwnTheme then
  begin
    FTheme := TNyxTheme.Create;
    FThemeOwner := TNativeRenderObject.Create(FTheme);
  end;
  FTheme.Validate;
  FBaseTheme := NewNyxDocumentTheme(nil, FTheme);
  FBaseThemeOwner := TNativeRenderObject.Create(FBaseTheme);
  FEvents := NewNyxEvents;
  FCollectionQueryEvents := FEvents;
  FCoordinatorRefreshObject := TNyxLCLCoordinatorRefresh.Create(Self);
  FCoordinatorRefreshTarget := TNyxLCLCoordinatorRefresh(FCoordinatorRefreshObject);
  FContentGuard := NewNyxContentGuard(QueueContent);
  Supports(FContentGuard, INyxContentLifetime, FContentLifetime);
  FPhysicalFrame := NewNyxNativeGestureFrame;
end;

destructor TNyxLCLRenderer.Destroy;
var
  LRetiringCoordinators: TNyxCoordinatorMounts;
  LEvents: INyxEvents;
  LThemeOwner: IInterface;
  LBaseOwner: IInterface;
  LCacheOwner: IInterface;
begin

  if FRetirementShell then
  begin
    inherited Destroy;
    Exit;
  end;
  FDestroying := True;
  LEvents := FEvents;
  LThemeOwner := FThemeOwner;
  FThemeOwner := nil;
  LBaseOwner := FBaseThemeOwner;
  FBaseThemeOwner := nil;
  LCacheOwner := FGlyphCacheOwner;
  FGlyphCacheOwner := nil;

  if FGenerationObject <> nil then
  begin
    TNativeBindingGeneration(FGenerationObject).Owner := nil;
  end;
  { Flag-only weak receiver revocation precedes event/canvas/control cleanup. }

  if FCoordinatorRefreshObject <> nil then
  begin
    TNyxLCLCoordinatorRefresh(FCoordinatorRefreshObject).Owner := nil;
  end;
  LRetiringCoordinators := FCoordinatorMounts;
  RevokeNyxCoordinatorMounts(LRetiringCoordinators);
  FCoordinatorMounts := nil;
  FCoordinatorIDs := nil;

  if FContentGuard <> nil then
  begin
    FContentGuard.Retire;
  end;

  { Clear detaches original fields before callbacks; keep the exact router and
    assets locally until all old receiver cleanup has returned. }
  { All final instance writes precede Clear's first outward cleanup. }
  FTheme := nil;
  FBaseTheme := nil;
  FButtonGlyphCache := nil;
  try
    try
      Clear;
    except
      { Explicit Unmount reports fallible disconnects. Destruction must finish
        its FPC FreeInstance epilogue, so final release only attempts cleanup. }
    end;
    try

      if LEvents <> nil then
      begin
        LEvents.Close;
      end;
    except
      { A router cleanup failure cannot skip independent coordinator release. }
    end;
    try
      DisconnectNyxCoordinatorMounts(LRetiringCoordinators);
    except
      { Disposal has already retired every renderer field. Explicit Unmount and
        postcommit retirement still report cleanup failures to their caller. }
    end;
  finally
    inherited Destroy;
  end;
end;

procedure TNyxLCLRenderer.Clear(AKeepPresentation: Boolean;
  const AEventView: INyxPreparedEventView);
var
  LOld: TNyxLCLRenderer;
  LIndex: Integer;
  LAxis: TNyxResizeAxis;
  LGeneration: INyxLCLBindingGeneration;
  LFailure: Exception;

  procedure RememberFailure;
  begin

    if LFailure = nil then
    begin
      LFailure := Exception(AcquireExceptionObject);
    end;
  end;

begin
  LFailure := nil;
  { An isolated retirement receiver owns ORIGINAL fields only. Nested cleanup
    may mount a successor or destroy this renderer; no later tail reads Self. }
  LOld := TNyxLCLRenderer.CreateRetirementShell;
  try
    LOld.FEvents := FEvents;
    LOld.FPhysicalFrame := FPhysicalFrame;
    LGeneration := FGeneration;

    if FGenerationObject <> nil then
    begin
      TNativeBindingGeneration(FGenerationObject).Owner := nil;
      TNativeBindingGeneration(FGenerationObject).RetiredBindings := FBindings;
    end;
    LOld.FCollectionInputGate := FCollectionInputGate;
    FCollectionInputGate := nil;
    LOld.FViewPublicationSource := FViewPublicationSource;
    FViewPublicationSource := nil;
    LOld.FCallerCoordinatorSnapshot := FCallerCoordinatorSnapshot;
    FCallerCoordinatorSnapshot := nil;
    LOld.FCoordinatorMounts := FCoordinatorMounts;
    FCoordinatorMounts := nil;
    LOld.FCoordinatorIDs := FCoordinatorIDs;
    FCoordinatorIDs := nil;
    LOld.FContentBlueprint := FContentBlueprint;
    FContentBlueprint := nil;
    LOld.FCanvasResizeGrips := FCanvasResizeGrips;
    FCanvasResizeGrips := nil;
    LOld.FCanvasMoveGrip := FCanvasMoveGrip;
    FCanvasMoveGrip := nil;
    LOld.FCanvasMoveView := FCanvasMoveView;
    FCanvasMoveView := nil;
    LOld.FCanvasMoveHost := FCanvasMoveHost;
    FCanvasMoveHost := nil;
    LOld.FMeasurementLabel := FMeasurementLabel;
    FMeasurementLabel := nil;
    LOld.FPanel := FPanel;
    FPanel := nil;
    LOld.FProjectionBaseline := FProjectionBaseline;
    FProjectionBaseline := nil;
    LOld.FBindings := FBindings;
    FBindings := nil;
    LOld.FCollectionBindings := FCollectionBindings;
    FCollectionBindings := nil;
    LOld.FCaptureObserver := FCaptureObserver;
    FCaptureObserver := nil;
    LOld.FEditingObserver := FEditingObserver;
    FEditingObserver := nil;
    LOld.FViewportObserver := FViewportObserver;
    FViewportObserver := nil;
    LOld.FEmitterScope := FEmitterScope;
    FEmitterScope := nil;
    LOld.FLiveBindings := FLiveBindings;
    FLiveBindings := nil;
    LOld.FRoot := FRoot;
    FRoot := nil;
    LOld.FState := FState;
    FState := nil;
    LOld.FOwnedState := FOwnedState;
    FOwnedState := nil;
    LOld.FGeneration := FGeneration;
    FGeneration := nil;
    LOld.FGenerationObject := FGenerationObject;
    FGenerationObject := nil;
    LOld.FOwnState := FOwnState;
    FOwnState := False;
    LOld.FMeasurementContext := FMeasurementContext;
    FMeasurementContext := 0;
    LOld.FLabelMeasurements := FLabelMeasurements;
    FLabelMeasurements := nil;
    for LIndex := Low(FResizeEdges) to High(FResizeEdges) do
    begin
      LOld.FResizeEdges[LIndex] := FResizeEdges[LIndex];
      FResizeEdges[LIndex] := nil;
    end;
    for LIndex := Low(FSelectionEdges) to High(FSelectionEdges) do
    begin
      LOld.FSelectionEdges[LIndex] := FSelectionEdges[LIndex];
      FSelectionEdges[LIndex] := nil;
    end;
    for LAxis := Low(TNyxResizeAxis) to High(TNyxResizeAxis) do
    begin
      LOld.FCanvasGripViews[LAxis] := FCanvasGripViews[LAxis];
      FCanvasGripViews[LAxis] := nil;
      LOld.FCanvasGripHosts[LAxis] := FCanvasGripHosts[LAxis];
      FCanvasGripHosts[LAxis] := nil;
    end;

    if not AKeepPresentation then
    begin
      LOld.FPresentationView := FPresentationView;
      FPresentationView := nil;
      LOld.FContentWork := FContentWork;
      FContentWork := nil;
      LOld.FContentExecution := FContentExecution;
      FContentExecution := nil;
      FContentRequestedSelection := TNyxPresentationSelection.None;
      FLastContentError := '';
    end;
    FImageViewPublished := False;
    FContentFrameKnown := False;
    FContentMeasurementKey := '';
    FVirtualLayout := False;
    FDesignMode := False;
    FProjectionContext := '';
    FProjectionSchemaRevision := 0;
    FSelectedDesignID := '';
    FResizePreview := Default(TNyxResizePreview);
    FDropPreview := Default(TNyxDropPreview);
    FPresentationSelection := TNyxPresentationSelection.None;
    FUpdating := False;
    FForceValues := False;
    FForceValueNode := nil;
    FLayouting := False;
    FProjecting := False;
    { Everything above is detached before these native/event cleanup callouts. }

    LOld.FDeferredNativeControls :=
      ((LOld.FEditingObserver <> nil) and LOld.FEditingObserver.Dispatching) or
      ((LOld.FPhysicalFrame <> nil) and LOld.FPhysicalFrame.Dispatching);
    { Revoke every original global observer before a cancellation/extension
      callback can mount a same-ID successor on the original renderer. These
      disconnections begin with terminal local flags; each gets an attempt. }
    try

      if LOld.FCaptureObserver <> nil then
      begin
        LOld.FCaptureObserver.Disconnect;
      end;
    except
      on Exception do RememberFailure;
    end;
    try

      if LOld.FEditingObserver <> nil then
      begin
        LOld.FEditingObserver.Disconnect;
      end;
    except
      on Exception do RememberFailure;
    end;
    try

      if LOld.FViewportObserver <> nil then
      begin
        LOld.FViewportObserver.Disconnect;
      end;
    except
      on Exception do RememberFailure;
    end;
    try

      if LOld.FEmitterScope <> nil then
      begin
        LOld.FEmitterScope.Disconnect;
      end;
    except
      on Exception do RememberFailure;
    end;
    try

      if LOld.FLiveBindings <> nil then
      begin
        LOld.FLiveBindings.Retire;
      end;
    except
      on Exception do RememberFailure;
    end;

    if LOld.FPanel <> nil then
    begin
      LOld.FPanel.OnResize := nil;
      LOld.FPanel.Detach;
    end;

    if LOld.FCollectionInputGate <> nil then
    begin
      LOld.FCollectionInputGate.Retire;
    end;
    RevokeNyxCoordinatorMounts(LOld.FCoordinatorMounts);

    if LOld.FContentWork <> nil then
    begin
      LOld.FContentWork.Retire;
    end;
    try

      if AEventView <> nil then
      begin
        AEventView.Install;
      end
      else if LOld.FEvents <> nil then
      begin
        LOld.FEvents.CancelPending;
      end;
    except
      on Exception do RememberFailure;
    end;
    try
      LOld.ClearRetired(AKeepPresentation, AEventView);
    except
      on Exception do RememberFailure;
    end;

    if LFailure <> nil then
    begin
      raise LFailure;
    end;
  finally
    LOld.Free;
  end;
end;

procedure TNyxLCLRenderer.ClearRetired(AKeepPresentation: Boolean;
  const AEventView: INyxPreparedEventView);
var
  LIndex: Integer;
  LDeferControls: Boolean;
  LRetiringCoordinators: TNyxCoordinatorMounts;
  LRetiringSource: INyxViewPublicationSource;
  LRetiringCallerSnapshot: INyxCoordinatorRegistrySnapshot;
  LFailure: Exception;

  procedure RememberFailure;
  begin

    if LFailure = nil then
    begin
      LFailure := Exception(AcquireExceptionObject);
    end;
  end;

begin
  LFailure := nil;
  LDeferControls := True;


  try

    if FCollectionInputGate <> nil then
    begin
      FCollectionInputGate.Retire;
    end;
  except
    on Exception do RememberFailure;
  end;

  try
    LRetiringSource := FViewPublicationSource;
  except
    on Exception do RememberFailure;
  end;

  try
    LRetiringCallerSnapshot := FCallerCoordinatorSnapshot;
  except
    on Exception do RememberFailure;
  end;

  try
    FCallerCoordinatorSnapshot := nil;
  except
    on Exception do RememberFailure;
  end;

  try
    FUpdating := True;
  except
    on Exception do RememberFailure;
  end;

  try
    RevokeNyxCoordinatorMounts(FCoordinatorMounts);
  except
    on Exception do RememberFailure;
  end;

  try
    LRetiringCoordinators := nil;
  except
    on Exception do RememberFailure;
  end;

  try

    if AEventView = nil then
    begin
      LRetiringCoordinators := FCoordinatorMounts;
    end;
  except
    on Exception do RememberFailure;
  end;

  try
    FCoordinatorMounts := nil;
  except
    on Exception do RememberFailure;
  end;

  try
    FCoordinatorIDs := nil;
  except
    on Exception do RememberFailure;
  end;

  try
    FImageViewPublished := False;
  except
    on Exception do RememberFailure;
  end;

  try

    if not AKeepPresentation and (FPresentationView <> nil) then
    begin
      FPresentationView.Retire;
      FPresentationView := nil;
    end;
  except
    on Exception do RememberFailure;
  end;

  try

    if not AKeepPresentation then
    begin

      if FContentWork <> nil then
      begin
        FContentWork.Retire;
        FContentWork := nil;
      end;

      if FContentExecution <> nil then
      begin
        FContentExecution.Cancel;
        FContentExecution := nil;
      end;
      FContentRequestedSelection := TNyxPresentationSelection.None;
      FLastContentError := '';
    end;
  except
    on Exception do RememberFailure;
  end;

  try
    FreeAndNil(FContentBlueprint);
  except
    on Exception do RememberFailure;
  end;

  try
    FContentFrameKnown := False;
  except
    on Exception do RememberFailure;
  end;

  try
    FContentMeasurementKey := '';
  except
    on Exception do RememberFailure;
  end;

  try
    ClearCanvasResizeGrips;
  except
    on Exception do RememberFailure;
  end;

  try
    { Revoke borrowed sinks before destroying any part of the mounted view. }
    ClearCanvasMoveGrip;
  except
    on Exception do RememberFailure;
  end;

  try
    FUpdating := True;
  except
    on Exception do RememberFailure;
  end;

  try
    FVirtualLayout := False;
  except
    on Exception do RememberFailure;
  end;

  try
    ClearLabelMeasurements;
  except
    on Exception do RememberFailure;
  end;

  try
    FreeAndNil(FMeasurementLabel);
  except
    on Exception do RememberFailure;
  end;

  try

    if FMeasurementContext <> 0 then
    begin
      LCLIntf.ReleaseDC(0, FMeasurementContext);
      FMeasurementContext := 0;
    end;
  except
    on Exception do RememberFailure;
  end;

  try

    if FPanel <> nil then
    begin
      { Retirement must not enter layout through native resize messages while
        bindings and controls are being disconnected or destroyed. }
      FPanel.OnResize := nil;
      FPanel.Detach;
    end;
  except
    on Exception do RememberFailure;
  end;

  try
    FDesignMode := False;
  except
    on Exception do RememberFailure;
  end;

  try
    FProjectionContext := '';
  except
    on Exception do RememberFailure;
  end;

  try
    FProjectionSchemaRevision := 0;
  except
    on Exception do RememberFailure;
  end;

  try
    ReleaseNyxNode(FProjectionBaseline);
  except
    on Exception do RememberFailure;
  end;

  try
    FSelectedDesignID := '';
  except
    on Exception do RememberFailure;
  end;

  try
    FResizePreview := Default(TNyxResizePreview);
  except
    on Exception do RememberFailure;
  end;

  try
    FDropPreview := Default(TNyxDropPreview);
  except
    on Exception do RememberFailure;
  end;

  try
    for LIndex := Low(FResizeEdges) to High(FResizeEdges) do
    begin
      FreeAndNil(FResizeEdges[LIndex]);
    end;
  except
    on Exception do RememberFailure;
  end;

  try
    for LIndex := Low(FSelectionEdges) to High(FSelectionEdges) do
    begin
      { The scroll host owns the strips and releases them with its controls. }
      FSelectionEdges[LIndex] := nil;
    end;
  except
    on Exception do RememberFailure;
  end;

  try
    LDeferControls := FDeferredNativeControls or
      ((FEditingObserver <> nil) and FEditingObserver.Dispatching) or
      ((FPhysicalFrame <> nil) and FPhysicalFrame.Dispatching);
  except
    on Exception do RememberFailure;
  end;

  try

    if FCaptureObserver <> nil then
    begin
      FCaptureObserver.Disconnect;
      FCaptureObserver := nil;
    end;
  except
    on Exception do RememberFailure;
  end;

  try

    if FEditingObserver <> nil then
    begin
      FEditingObserver.Disconnect;
      FEditingObserver := nil;
    end;
  except
    on Exception do RememberFailure;
  end;

  try

    if FViewportObserver <> nil then
    begin
      FViewportObserver.Disconnect;
      FViewportObserver := nil;
    end;
  except
    on Exception do RememberFailure;
  end;

  try

    if FEmitterScope <> nil then
    begin
      FEmitterScope.Disconnect;
      FEmitterScope := nil;
    end;
  except
    on Exception do RememberFailure;
  end;

  try

    if FGeneration = nil then
    begin
      FreeAndNil(FLiveBindings);
    end
    else
    begin
      FLiveBindings := nil;
    end;
  except
    on Exception do RememberFailure;
  end;

  try
    for LIndex := 0 to Length(FBindings) - 1 do
    begin

    try

        if FBindings[LIndex].FControl is TNyxLogicalScrollBox then
        begin
          TNyxLogicalScrollBox(FBindings[LIndex].FControl).Detach;
        end;
    except
      on Exception do RememberFailure;
    end;

    try
        FBindings[LIndex].FActiveDrag := nil;
    except
      on Exception do RememberFailure;
    end;

    try
        FBindings[LIndex].FDrawingSource := nil;
    except
      on Exception do RememberFailure;
    end;

    try

        if FBindings[LIndex].FDateField <> nil then
        begin
          FBindings[LIndex].FDateField.Disconnect;
        end;
    except
      on Exception do RememberFailure;
    end;

    try

        if FBindings[LIndex].FTimeField <> nil then
        begin
          FBindings[LIndex].FTimeField.Disconnect;
        end;
    except
      on Exception do RememberFailure;
    end;

    try

        if FBindings[LIndex].FColorField <> nil then
        begin
          FBindings[LIndex].FColorField.Disconnect;
        end;
    except
      on Exception do RememberFailure;
    end;

    try
        FBindings[LIndex].DisconnectControl(FBindings[LIndex].FControl);
    except
      on Exception do RememberFailure;
    end;

    try

        if FBindings[LIndex].FImageLifecycle <> nil then
        begin
          FBindings[LIndex].FImageLifecycle.Close;
          FBindings[LIndex].FImageLifecycle := nil;
        end;
    except
      on Exception do RememberFailure;
    end;

    try

        if FBindings[LIndex].FInput <> FBindings[LIndex].FControl then
        begin
          FBindings[LIndex].DisconnectControl(FBindings[LIndex].FInput);
        end;
    except
      on Exception do RememberFailure;
    end;

    try

        if FBindings[LIndex].FControl is TNyxLCLSplitView then
        begin
          FBindings[LIndex].DisconnectControl(
            TNyxLCLSplitView(FBindings[LIndex].FControl).Grip);
        end;
    except
      on Exception do RememberFailure;
    end;

    try

        if FBindings[LIndex].FCollectionMount <> nil then
        begin
          FBindings[LIndex].FCollectionMount.Disconnect;
          FBindings[LIndex].FCollectionMount := nil;
        end;
    except
      on Exception do RememberFailure;
    end;

    try

        if FBindings[LIndex].FToolCanvasView <> nil then
        begin
          FBindings[LIndex].FToolCanvasView.Disconnect;
          FBindings[LIndex].FToolCanvasView := nil;
        end;
    except
      on Exception do RememberFailure;
    end;

    try

        if FBindings[LIndex].FCanvasView <> nil then
        begin
          FBindings[LIndex].FCanvasView.Disconnect;
          FBindings[LIndex].FCanvasView := nil;
        end;
    except
      on Exception do RememberFailure;
    end;

    try

        if FBindings[LIndex].FCollectionPager <> nil then
        begin
          FBindings[LIndex].FCollectionPager.Disconnect;
          FBindings[LIndex].FCollectionPager := nil;
        end;
    except
      on Exception do RememberFailure;
    end;

    end;
  except
    on Exception do RememberFailure;
  end;

  try
    { Free target controls before their borrowed nodes. Events cannot refer to a
      destroyed realized tree during native control destruction. }

    if LDeferControls and (FPanel <> nil) then
    begin
      { Native widget code and drag construction must finish before destruction.
        The physical frame owns the revoked panel through the next safe idle. }
      FPanel.OnResize := nil;
      FPanel.Visible := False;
      FPanel.Enabled := False;
      FPhysicalFrame.Retire(FPanel);
      FPanel := nil;
    end
    else
    begin
      for LIndex := 0 to High(FBindings) do
      begin

        if FBindings[LIndex].FControl.Dragging then
        begin
          FBindings[LIndex].FControl.EndDrag(False);
        end;

        if (FBindings[LIndex].FInput <> nil) and
          (FBindings[LIndex].FInput <> FBindings[LIndex].FControl) and
          FBindings[LIndex].FInput.Dragging then
        begin
          FBindings[LIndex].FInput.EndDrag(False);
        end;
        ReleaseNyxLCLPointer(FBindings[LIndex].FControl);
        ReleaseNyxLCLPointer(FBindings[LIndex].FInput);
      end;
      FreeAndNil(FPanel);
    end;
  except
    on Exception do RememberFailure;
  end;

  try

    if FGeneration = nil then
    begin
      for LIndex := 0 to Length(FBindings) - 1 do
      begin
        FBindings[LIndex].Free;
      end;
    end;
  except
    on Exception do RememberFailure;
  end;

  try
    SetLength(FBindings, 0);
  except
    on Exception do RememberFailure;
  end;

  try
    FCollectionBindings := nil;
  except
    on Exception do RememberFailure;
  end;

  try
    ReleaseNyxNode(FRoot);
  except
    on Exception do RememberFailure;
  end;

  try
    FPresentationSelection := TNyxPresentationSelection.None;
  except
    on Exception do RememberFailure;
  end;

  try
    { The explicit holder may also belong to an old entered generation. }
    FOwnedState := nil;
  except
    on Exception do RememberFailure;
  end;

  try
    FState := nil;
  except
    on Exception do RememberFailure;
  end;

  try
    FOwnState := False;
  except
    on Exception do RememberFailure;
  end;

  try
    FUpdating := False;
  except
    on Exception do RememberFailure;
  end;

  try
    { Runtime owners outlive every borrowed widget/store link above. RenderFrame
      retains the displaced source locally until successor adoption is complete. }
    FViewPublicationSource := nil;
  except
    on Exception do RememberFailure;
  end;

  try
    { All borrowed renderer state is retired before subscription cleanup can
      enter an alternate receiver or report a failure. Keep only local leases. }
    try
      DisconnectNyxCoordinatorMounts(LRetiringCoordinators);
    finally
      try
        { Keep factories leased until every old child/control link is retired. }
        LRetiringCallerSnapshot := nil;
      finally
        LRetiringSource := nil;
      end;
    end;
  except
    on Exception do RememberFailure;
  end;


  if LFailure <> nil then
  begin
    raise LFailure;
  end;
end;

procedure TNyxLCLRenderer.RegisterDrawingProvider(const AName: TNyxDrawingProviderRef;
  const AProvider: INyxDrawingProvider);
var
  LIndex: Integer;
begin

  if not AName.Defined or (AProvider = nil) or (FRoot <> nil) then
  begin
    raise ENyxDrawing.Create('Register a defined drawing provider before mounting');
  end;
  for LIndex := 0 to High(FDrawingNames) do
  begin

    if FDrawingNames[LIndex] = AName.Name then
    begin
      FDrawingProviders[LIndex] := AProvider;
      Exit;
    end;
  end;

  if Length(FDrawingNames) >= 256 then
  begin
    raise ENyxDrawing.Create('Drawing provider registry capacity exceeded');
  end;
  SetLength(FDrawingNames, Length(FDrawingNames) + 1);
  SetLength(FDrawingProviders, Length(FDrawingNames));
  FDrawingNames[High(FDrawingNames)] := AName.Name;
  FDrawingProviders[High(FDrawingProviders)] := AProvider;
end;

function TNyxLCLRenderer.DrawingProvider(ANode: TNyxNode): INyxDrawingProvider;
var
  LName: TNyxDrawingProviderRef;
  LIndex: Integer;
begin
  Result := nil;
  LName := NyxNodeDrawingProvider(ANode);

  if not LName.Defined then
  begin
    Exit;
  end;
  for LIndex := 0 to High(FDrawingNames) do
  begin

    if FDrawingNames[LIndex] = LName.Name then
    begin
      Exit(FDrawingProviders[LIndex]);
    end;
  end;
  raise ENyxDrawing.Create('Drawing provider is not registered: ' + LName.Name);
end;

procedure TNyxLCLRenderer.RegisterFactory(const AKind: TNyxText; AFactory: TNyxLCLFactory;
  AUpdater: TNyxLCLUpdater);
var
  LIndex: Integer;
begin

  if (AKind = '') or not Assigned(AFactory) then
  begin
    raise ENyxModel.Create('Custom native kind and factory are required');
  end;
  for LIndex := 0 to Length(FFactoryKinds) - 1 do
  begin

    if FFactoryKinds[LIndex] = AKind then
    begin
      FFactories[LIndex] := AFactory;
      FEventFactories[LIndex] := nil;
      FUpdaters[LIndex] := AUpdater;
      Exit;
    end;
  end;
  LIndex := Length(FFactoryKinds);
  SetLength(FFactoryKinds, LIndex + 1);
  SetLength(FFactories, LIndex + 1);
  SetLength(FEventFactories, LIndex + 1);
  SetLength(FUpdaters, LIndex + 1);
  FFactoryKinds[LIndex] := AKind;
  FFactories[LIndex] := AFactory;
  FUpdaters[LIndex] := AUpdater;
end;

procedure TNyxLCLRenderer.RegisterEventFactory(const AKind: TNyxKindRef;
  AFactory: TNyxLCLEventFactory; AUpdater: TNyxLCLUpdater);
var
  LIndex: Integer;
begin

  if (AKind.Name = '') or not Assigned(AFactory) then
  begin
    raise ENyxModel.Create('Custom native kind and event factory are required');
  end;
  LIndex := 0;
  while (LIndex < Length(FFactoryKinds)) and (FFactoryKinds[LIndex] <> AKind.Name) do
  begin
    Inc(LIndex);
  end;

  if LIndex = Length(FFactoryKinds) then
  begin
    SetLength(FFactoryKinds, LIndex + 1);
    SetLength(FFactories, LIndex + 1);
    SetLength(FEventFactories, LIndex + 1);
    SetLength(FUpdaters, LIndex + 1);
  end;
  FFactoryKinds[LIndex] := AKind.Name;
  FFactories[LIndex] := nil;
  FEventFactories[LIndex] := AFactory;
  FUpdaters[LIndex] := AUpdater;
end;

function TNyxLCLRenderer.FactoryIndex(ANode: TNyxNode): Integer;
var
  LIndex: Integer;
begin
  Result := -1;
  for LIndex := 0 to Length(FFactoryKinds) - 1 do
  begin

    if FFactoryKinds[LIndex] = ANode.Kind then
    begin
      Exit(LIndex);
    end;
  end;
  for LIndex := 0 to Length(FFactoryKinds) - 1 do
  begin

    if FFactoryKinds[LIndex] = ANode.ProjectionKind then
    begin
      Exit(LIndex);
    end;
  end;
end;

function TNyxLCLRenderer.Capability(ANode: TNyxNode): TNyxCapability;
var
  LInfo: TNyxPrimitiveInfo;
begin
  Result := ncMissing;

  if ANode = nil then
  begin
    Exit;
  end;

  if FactoryIndex(ANode) >= 0 then
  begin
    Exit(ncCustom);
  end;

  if FindNyxPrimitive(ANode.ProjectionKind, LInfo) then
  begin
    Result := LInfo.Native;
  end;
end;

function TNyxLCLRenderer.CreateControl(ANode: TNyxNode; out AInput: TControl;
  out ACaption: TLabel): TControl;
var
  LKind: TNyxText;
  LIndex: Integer;
  LLabel: TLabel;
  LPanel: TPanel;
  LInputSurface: TNyxLCLSurface;
  LDateField: TNyxLCLDateField;
  LTimeField: TNyxLCLTimeField;
  LColorField: TNyxLCLColorField;
  LInfo: TNyxPrimitiveInfo;
  LFactoryIndex: Integer;
  {$ifdef NYX_STUDIO_PROFILE}LValidationStarted: QWord;{$endif}
  procedure InheritTypeface(AFace: TControl; AMonospaced: Boolean = False);
  var
    LFont: TFont;
    LHeight: Integer;
  begin

    if AFace = nil then
    begin
      Exit;
    end;
    LFont := TNyxControlAccess(AFace).Font;
    LHeight := LFont.Height;

    if TNyxControlAccess(AFace).ParentFont then
    begin
      { Untouched native faces previously inherited their parent's size. Keep
        that behavior rather than freezing the constructor's default height. }
      LHeight := FPanel.Font.Height;
    end;
    { Size/color changes before parenting disable LCL ParentFont propagation.
      Adopt the accepted host family/DPI explicitly, preserving role metrics,
      weight and the code editors' intentionally monospaced family. }
    LFont.PixelsPerInch := FPanel.Font.PixelsPerInch;

    if not AMonospaced then
    begin
      LFont.Name := FPanel.Font.Name;
    end;
    LFont.CharSet := FPanel.Font.CharSet;
    LFont.Quality := FPanel.Font.Quality;
    LFont.Height := LHeight;
  end;
begin
  AInput := nil;
  ACaption := nil;
  LKind := ANode.ProjectionKind;
  {$ifdef NYX_STUDIO_PROFILE}LValidationStarted := GetTickCount64;{$endif}
  ValidateNyxProperties(ANode);
  {$ifdef NYX_STUDIO_PROFILE}
  Inc(FBuildValidationMilliseconds, GetTickCount64 - LValidationStarted);
  {$endif}
  LFactoryIndex := FactoryIndex(ANode);

  if LFactoryIndex >= 0 then
  begin
    for LIndex := 0 to ANode.BindingCount - 1 do
    begin

      if not ANode.Bindings[LIndex].Cleared and not Assigned(FUpdaters[LFactoryIndex]) then
      begin
        raise ENyxModel.Create('Bound custom native factory requires an updater: ' + ANode.Kind);
      end;
    end;

    if Assigned(FEventFactories[LFactoryIndex]) then
    begin
      Result := FEventFactories[LFactoryIndex](ANode, FPanel,
        FEmitterScope.ForControl(ANode.ID));
    end
    else
    begin
      Result := FFactories[LFactoryIndex](ANode, FPanel);
    end;

    if (Result = nil) or (Result.Owner <> FPanel) or (Result.Parent <> nil) then
    begin
      raise ENyxModel.Create('Native factory must return a new control owned by its supplied owner: ' + ANode.Kind);
    end;

    if (Result is TCustomEdit) or (Result is TComboBox) or (Result is TCheckBox) or
      (Result is TRadioButton) or (Result is TTrackBar) then
    begin
      AInput := Result;
    end;
    Exit;
  end;

  if not FindNyxPrimitive(LKind, LInfo) then
  begin
    raise ENyxModel.Create('No native projection for ' + ANode.Kind + ' (base ' + LKind +
      '). Derive a primitive recipe or register a native factory.');
  end;

  if not LInfo.Container and (ANode.Count > 0) then
  begin
    raise ENyxModel.Create('Compose children inside a layout host, not ' + ANode.ID);
  end;

  if LKind = 'split-view' then
  begin
    Result := TNyxLCLSplitView.Create(FPanel);
    TNyxLCLSplitView(Result).Initialize(ANode);
  end
  else if LKind = 'code-editor' then
  begin
    Result := TMemo.Create(FPanel);
    AInput := Result;
    TMemo(Result).Text := ANode.Prop('value');
    TMemo(Result).ReadOnly := ANode.Prop('readonly') = 'true';
    TMemo(Result).ScrollBars := ssBoth;
    TMemo(Result).WordWrap := False;
    TMemo(Result).Font.Name := 'Consolas';
    TMemo(Result).Color := ThemeColor(FTheme.Surface);
    TMemo(Result).Font.Color := ThemeColor(FTheme.Text);
  end
  else if (LKind = 'input') or (LKind = 'memo') or (LKind = 'select') or
    (LKind = 'spin') or (LKind = 'date') or (LKind = 'time') or (LKind = 'color') then
  begin
    LPanel := TPanel.Create(FPanel);
    LPanel.BevelOuter := bvNone;
    Result := LPanel;
    LLabel := TLabel.Create(FPanel);
    LLabel.Parent := LPanel;
    { Nyx owns the caption's viewport allocation. Automatic label sizing would
      restore its preferred size after an offscreen frame is parked at zero. }
    LLabel.AutoSize := False;
    ACaption := LLabel;
    LLabel.Caption := ANode.Prop('text');
    LLabel.SetBounds(0, 0, 300, 20);
    LLabel.Font.Color := ThemeColor(FTheme.Muted);
    LLabel.Font.Height := -Max(12, FTheme.FontSize - 1);
    LLabel.Font.Style := [fsBold];
    { Keep the editable LCL widget inside a themed frame. This retains native
      selection, clipboard, IME and keyboard behavior while the portable palette
      owns the frame's face, border and radius. The caption names the real input. }
    LInputSurface := TNyxLCLSurface.Create(FPanel);
    LInputSurface.ApplyTheme(FTheme, True);
    LInputSurface.Parent := LPanel;
    LInputSurface.SetBounds(0, 24, 300, 40);

    if LKind = 'memo' then
    begin
      AInput := TMemo.Create(FPanel);
      TMemo(AInput).Text := ANode.Prop('value');
      TMemo(AInput).ScrollBars := ssAutoVertical;
      TMemo(AInput).ReadOnly := ANode.Prop('readonly') = 'true';
    end
    else if LKind = 'select' then
    begin
      AInput := TComboBox.Create(FPanel);
      TComboBox(AInput).Style := csDropDownList;
      { SyncLiteralItems supplies the admitted initial choices once. }
    end
    else if LKind = 'date' then
    begin
      LDateField := TNyxLCLDateField.Create(FPanel);
      LDateField.Parent := LInputSurface;
      LDateField.AutoSize := False;
      LDateField.BorderStyle := bsNone;
      LDateField.SetBounds(12, 5, 276, 30);
      LDateField.Text := ANode.Prop('value');
      LDateField.Font.Height := -FTheme.FontSize;
      LDateField.Font.Color := ThemeColor(FTheme.Text);
      AInput := LDateField.Editor;
    end
    else if LKind = 'time' then
    begin
      LTimeField := TNyxLCLTimeField.Create(FPanel);
      LTimeField.Parent := LInputSurface;
      LTimeField.AutoSize := False;
      LTimeField.BorderStyle := bsNone;
      LTimeField.SetBounds(12, 5, 276, 30);
      LTimeField.Text := ANode.Prop('value');
      LTimeField.Font.Height := -FTheme.FontSize;
      LTimeField.Font.Color := ThemeColor(FTheme.Text);
      AInput := LTimeField.Editor;
    end
    else if LKind = 'color' then
    begin
      LColorField := TNyxLCLColorField.Create(FPanel);
      LColorField.Parent := LInputSurface;
      LColorField.AutoSize := False;
      LColorField.BorderStyle := bsNone;
      LColorField.SetBounds(12, 5, 276, 30);
      LColorField.Text := ANode.Prop('value');
      LColorField.Font.Height := -FTheme.FontSize;
      LColorField.Font.Color := ThemeColor(FTheme.Text);
      AInput := LColorField.Editor;
    end
    else if LKind = 'spin' then
    begin
      AInput := TSpinEdit.Create(FPanel);
      TSpinEdit(AInput).MinValue := StrToIntDef(ANode.Prop('min'), 0);
      TSpinEdit(AInput).MaxValue := StrToIntDef(ANode.Prop('max'), 100);
      TSpinEdit(AInput).Value := StrToIntDef(ANode.Prop('value'), 0);
    end
    else
    begin
      AInput := TEdit.Create(FPanel);
      TEdit(AInput).Text := ANode.Prop('value');
      TEdit(AInput).TextHint := ANode.Prop('placeholder');
      TEdit(AInput).ReadOnly := ANode.Prop('readonly') = 'true';

      if ANode.Prop('input-type') = 'password' then
      begin
        TEdit(AInput).PasswordChar := '*';
      end;
    end;

    if not (AInput.Parent is TNyxLCLDateField) and
      not (AInput.Parent is TNyxLCLTimeField) and
      not (AInput.Parent is TNyxLCLColorField) then
    begin
      { Nyx allocates this themed face and its editor together. A single-line
        edit's default AutoSize otherwise competes with ArrangeInput: LCL can
        restore its preferred font height during the parent's sizing pass and
        repeatedly reject our allocated height with ELayoutException. Native
        editing, selection and IME remain in the ordinary LCL widget. Grouped
        pickers keep their own internal editor geometry above. }

      if AInput is TCustomEdit then
      begin
        TEdit(AInput).AutoSize := False;
      end;
      AInput.Parent := LInputSurface;
      AInput.SetBounds(12, 10, 276, 20);
      AInput.Anchors := [akLeft, akTop, akRight];
    end;
    AInput.Font.Height := -FTheme.FontSize;
    LLabel.FocusControl := TWinControl(AInput);

    if AInput is TMemo then
    begin
      AInput.Anchors := AInput.Anchors + [akBottom];
    end;
    { Explicit surfaces keep native edit text readable in both theme variants.
      Widgetset defaults often remain white after the parent switches to dark. }

    if AInput is TCustomEdit then
    begin
      TEdit(AInput).Color := ThemeColor(FTheme.Surface);
      TEdit(AInput).Font.Color := ThemeColor(FTheme.Text);
      TEdit(AInput).BorderStyle := bsNone;
    end
    else if AInput is TComboBox then
    begin
      TComboBox(AInput).Color := ThemeColor(FTheme.Surface);
      TComboBox(AInput).Font.Color := ThemeColor(FTheme.Text);
    end;
  end
  else if (LKind = 'button') or (LKind = 'link') then
  begin
    { A native link must be keyboard reachable. Reuse the existing focusable
      LCL button projection rather than a non-focusable painted label; its
      click registration retains the same portable link identity. }
    Result := TNyxLCLButton.Create(FPanel);
    TNyxLCLButton(Result).Caption := ANode.Prop('text');
    TNyxLCLButton(Result).ApplyTheme(FTheme, ANode.Prop('variant'));
  end
  else if (LKind = 'checkbox') or (LKind = 'switch') then
  begin
    Result := TCheckBox.Create(FPanel);
    { Retain the widget's preferred metrics for measurement, but let Nyx alone
      assign its physical bounds, including a parked zero-area allocation. }
    TCheckBox(Result).AutoSize := False;
    TCheckBox(Result).Caption := ANode.Prop('text');
    TCheckBox(Result).Checked := ANode.Prop('value') = 'true';
    AInput := Result;
  end
  else if LKind = 'radio' then
  begin
    Result := TRadioButton.Create(FPanel);
    TRadioButton(Result).AutoSize := False;
    TRadioButton(Result).Caption := ANode.Prop('text');
    TRadioButton(Result).Checked := ANode.Prop('value') = 'true';
    AInput := Result;
  end
  else if LKind = 'slider' then
  begin
    Result := TNyxLCLSlider.Create(FPanel);
    TNyxLCLSlider(Result).Accept(TNyxSliderScale.Create(NyxNodeValueDomain(ANode),
      StrToIntDef(ANode.Prop('min'), 0), StrToIntDef(ANode.Prop('max'), 100),
      StrToIntDef(ANode.Prop('slider-intervals'), 1000)), ANode.Prop('value', '0'));
    AInput := Result;
  end
  else if LKind = 'progress' then
  begin
    Result := TNyxLCLProgress.Create(FPanel);
    TNyxLCLProgress(Result).ApplyTheme(FTheme);
    TProgressBar(Result).Min := StrToIntDef(ANode.Prop('min'), 0);
    TProgressBar(Result).Max := StrToIntDef(ANode.Prop('max'), 100);
    TProgressBar(Result).Position := StrToIntDef(ANode.Prop('value'), 0);

    if ANode.Prop(NyxAttributeName(atIndeterminate), 'false') = 'true' then
    begin
      TNyxLCLProgress(Result).Indeterminate := True;
    end;
  end
  else if LKind = 'list' then
  begin
    Result := TListBox.Create(FPanel);
  end
  else if LKind = 'tree' then
  begin
    Result := TTreeView.Create(FPanel);
  end
  else if LKind = 'table' then
  begin
    Result := TNyxCollectionStringGrid.Create(FPanel);
  end
  else if LKind = 'drawing' then
  begin
    Result := TNyxLCLDrawing.Create(FPanel);
  end
  else if LKind = 'image' then
  begin
    Result := TNyxLCLImage.Create(FPanel);
    TImage(Result).Proportional := True;
    TImage(Result).Center := True;

    { SyncPicture admits a detached picture before updating this face. }
  end
  else if LKind = 'tabs' then
  begin
    Result := TNyxLCLTabHost.CreateForTabs(FPanel, ANode);
  end
  else if LKind = 'group' then
  begin
    { Reuse LCL's real labeled group rather than painting an unlabeled panel.
      Its caption and accessibility stay part of the native widget contract. }
    Result := TGroupBox.Create(FPanel);
    TGroupBox(Result).Caption := ANode.Prop('text');
  end
  else if (LKind = 'heading') or (LKind = 'label') or (LKind = 'badge') or
    (LKind = 'alert') or (LKind = 'avatar') then
  begin
    LLabel := TLabel.Create(FPanel);
    Result := LLabel;
    LLabel.AutoSize := False;
    LLabel.WordWrap := True;
    LLabel.Caption := ANode.Prop('text');
    LLabel.Font.Color := ThemeColor(FTheme.Text);
    LLabel.Font.Height := -FTheme.FontSize;

    if LKind = 'heading' then
    begin
      { Theme metrics are logical pixels on both targets. Assigning points here
        made headings scale differently from the surrounding UI on high DPI. }
      LLabel.Font.Height := -24;
      LLabel.Font.Style := [fsBold];
    end;

    if LKind = 'label' then
    begin
      LLabel.Font.Color := ThemeColor(FTheme.Muted);
    end;

    if (LKind = 'badge') or (LKind = 'link') then
    begin
      LLabel.Font.Color := ThemeColor(FTheme.Accent);
    end;

    if LKind = 'badge' then
    begin
      LLabel.Font.Height := -11;
      LLabel.Font.Style := [fsBold];
    end;
  end
  else if LKind = 'scroll' then
  begin
    Result := TNyxLogicalScrollBox.Create(FPanel);
    TScrollBox(Result).BorderStyle := bsNone;

    if ANode.Prop('surface') = 'true' then
    begin
      TScrollBox(Result).ParentColor := False;
      TScrollBox(Result).Color := ThemeColor(FTheme.Surface);
    end;
  end
  else if LKind = 'code' then
  begin
    Result := TMemo.Create(FPanel);
    TMemo(Result).Text := ANode.Prop('text');
    TMemo(Result).ReadOnly := True;
    TMemo(Result).ScrollBars := ssBoth;
    TMemo(Result).WordWrap := False;
    TMemo(Result).Font.Name := 'Consolas';
  end
  else
  begin

    if (LKind = 'card') or (LKind = 'panel') or (LKind = 'group') or
      (ANode.Prop('surface') = 'true') then
    begin
      LPanel := TNyxLCLSurface.Create(FPanel);
      TNyxLCLSurface(LPanel).ApplyTheme(FTheme);
    end
    else
    begin
      LPanel := TPanel.Create(FPanel);
      LPanel.BevelOuter := bvNone;
      LPanel.Caption := '';
    end;
    Result := LPanel;
  end;
  { Native list/tree/grid/memo faces may retain a white widgetset background even
    when they inherit a dark parent font. Set both sides of that text contrast
    explicitly. Native selection glyphs, scrollbars and specialized widget chrome
    continue to use the widgetset's interaction/accessibility implementation. }

  if (LKind = 'list') or (LKind = 'tree') or (LKind = 'table') or
    (LKind = 'code') or (LKind = 'code-editor') then
  begin
    TNyxWinControlAccess(Result).Color := ThemeColor(FTheme.Surface);
    TNyxWinControlAccess(Result).Font.Color := ThemeColor(FTheme.Text);
    TNyxWinControlAccess(Result).Font.Height := -FTheme.FontSize;

    if Result is TStringGrid then
    begin
      TStringGrid(Result).FixedColor := ThemeColor(FTheme.Surface);
      TStringGrid(Result).GridLineColor := ThemeColor(FTheme.Border);
    end;
  end;
  InheritTypeface(Result, (LKind = 'code') or (LKind = 'code-editor'));
  InheritTypeface(AInput, (LKind = 'code') or (LKind = 'code-editor'));
  InheritTypeface(ACaption);
end;

function TNyxLCLRenderer.Build(ANode: TNyxNode; AParent: TWinControl): TControl;
var
  LInput: TControl;
  LFocus: TWinControl;
  LCaption: TLabel;
  LBinding: TNyxLCLBinding;
  LIndex: Integer;
  LValueDomain: TNyxValueDomain;
  {$ifdef NYX_STUDIO_PROFILE}LBuildStarted: QWord;{$endif}
begin
  {$ifdef NYX_STUDIO_PROFILE}
  Inc(FBuildNodes);
  LBuildStarted := GetTickCount64;
  {$endif}
  Result := CreateControl(ANode, LInput, LCaption);
  { Bind before parenting/native handle creation. Unadmitted candidate controls
    expose no application activation; the same physical frame owns retirement. }

  if Result is TNyxLCLButton then
  begin
    TNyxLCLButton(Result).ConnectAccessibility(FEmitterScope.ForControl(ANode.ID), FPhysicalFrame);
  end;
  {$ifdef NYX_STUDIO_PROFILE}
  Inc(FBuildCreationMilliseconds, GetTickCount64 - LBuildStarted);
  LBuildStarted := GetTickCount64;
  {$endif}
  { Match the admitted ancestor/disclosure policy before parenting can create
    native windows. A collection's initial HandleNeeded may realize its parent
    and visible siblings before the first Sync; closed bodies must already be
    hidden during that native initialization. Keep their control objects and
    bindings so opening later uses the same fields and event identities. }
  Result.Visible := NyxInteractionVisible(ANode);
  Result.Parent := AParent;
  {$ifdef NYX_STUDIO_PROFILE}
  Inc(FBuildParentMilliseconds, GetTickCount64 - LBuildStarted);
  LBuildStarted := GetTickCount64;
  {$endif}
  Result.Hint := ANode.Prop('hint');
  Result.ShowHint := Result.Hint <> '';
  Result.Enabled := ANode.Prop('enabled', 'true') <> 'false';

  if LInput <> nil then
  begin
    LInput.Enabled := Result.Enabled;
  end;
  LBinding := TNyxLCLBinding.Create;
  LBinding.FRenderer := Self;
  LBinding.FGeneration := FGenerationObject;
  LBinding.FNode := ANode;
  LBinding.FControl := Result;
  LBinding.FInput := LInput;
  LBinding.FCaption := LCaption;

  if (LInput <> nil) and (LInput.Parent is TNyxLCLDateField) then
  begin
    LBinding.FDateField := TNyxLCLDateField(LInput.Parent);
  end;

  if Result is TNyxLCLTabHost then
  begin
    TNyxLCLTabHost(Result).OnChange := LBinding.TabsChanged;
    TNyxLCLTabHost(Result).OnAdmit := LBinding.AdmitTabs;
  end;

  if (LInput <> nil) and (LInput.Parent is TNyxLCLTimeField) then
  begin
    LBinding.FTimeField := TNyxLCLTimeField(LInput.Parent);
  end;

  if (LInput <> nil) and (LInput.Parent is TNyxLCLColorField) then
  begin
    LBinding.FColorField := TNyxLCLColorField(LInput.Parent);
  end;

  if ANode.Parent <> nil then
  begin
    LBinding.FLogicalParent := Binding(ANode.Parent);
  end;
  LValueDomain := NyxNodeValueDomain(ANode);

  if LBinding.FDateField <> nil then
  begin
    LBinding.FDateField.SetDomain(LValueDomain);
    LBinding.FDateField.SetAcceptedValue(TNyxCalendarDate.FromText(ANode.Prop('value')));
  end;

  if LBinding.FTimeField <> nil then
  begin
    LBinding.FTimeField.SetDomain(LValueDomain);
    LBinding.FTimeField.SetAcceptedValue(TNyxClockTime.FromText(ANode.Prop('value')));
  end;

  if LBinding.FColorField <> nil then
  begin
    LBinding.FColorField.SetDomain(LValueDomain);
    LBinding.FColorField.SetAcceptedValue(TNyxRGBColor.FromText(ANode.Prop('value')));
  end;
  { Numeric and native clock drafts need an editing-complete boundary even
    without a state binding. Subsecond clock steps must not reject an unfinished
    fractional draft before its final digit can be entered. }
  LBinding.FDeferredValue := (LInput is TCustomEdit) and not (LInput is TSpinEdit) and
    LValueDomain.Defined;

  if LBinding.FDeferredValue then
  begin
    LBinding.FDeferredValue := (LValueDomain.Kind in [nskInteger, nskNumber]) or
      (LBinding.FTimeField <> nil) or (LBinding.FColorField <> nil);
  end;
  LBinding.FCustom := FactoryIndex(ANode) >= 0;

  if LBinding.FCustom then
  begin
    LBinding.FUpdater := FUpdaters[FactoryIndex(ANode)];
  end;
  SetLength(FBindings, Length(FBindings) + 1);
  FBindings[Length(FBindings) - 1] := LBinding;

  { Every native TControl exposes the same click slot, including labels and
    layout hosts. Preserve a factory's own callback just as for focus hooks;
    a portable callback is not limited to the button projection. }
  LBinding.FPreviousClick := TNyxControlAccess(Result).OnClick;
  TNyxControlAccess(Result).OnClick := LBinding.Click;
  LBinding.AttachPointer(Result, 0);
  Result.AccessibleName := ANode.Prop('aria-label', ANode.Prop('text'));
  SyncNyxLCLAccessibility(Result);

  if LInput <> nil then
  begin
    LInput.AccessibleName := Result.AccessibleName;
    SyncNyxLCLAccessibility(LInput);

    if LInput <> Result then
    begin
      { Framed fields have a separate editable child. LCL clicks do not bubble
        from that child to its frame, unlike DOM clicks on a wrapped input. }
      LBinding.FPreviousInputClick := TNyxControlAccess(LInput).OnClick;
      TNyxControlAccess(LInput).OnClick := LBinding.Click;
      LBinding.AttachPointer(LInput, 1);
    end;

  end;
  { Frames and split hosts are not their inner focus surface. Attach once on
    that surface and preserve the creator's own hooks before installing ours. }
  LFocus := nil;

  if LInput is TWinControl then
  begin
    LFocus := TWinControl(LInput);
  end
  else if Result is TNyxLCLSplitView then
  begin
    LFocus := TNyxLCLSplitView(Result).Grip;
  end
  else if (Result is TWinControl) and
    (NyxSupportsKeyboard(ANode) or
    (LBinding.FCustom and TWinControl(Result).TabStop)) then
  begin
    LFocus := TWinControl(Result);
  end;

  if LFocus <> nil then
  begin
    LBinding.FFocusControl := LFocus;
    LBinding.FPreviousEnter := TNyxWinControlAccess(LFocus).OnEnter;
    LBinding.FPreviousExit := TNyxWinControlAccess(LFocus).OnExit;
    TNyxWinControlAccess(LFocus).OnEnter := LBinding.Enter;
    TNyxWinControlAccess(LFocus).OnExit := LBinding.Leave;
    LBinding.FPreviousKeyDown := TNyxWinControlAccess(LFocus).OnKeyDown;
    LBinding.FPreviousKeyUp := TNyxWinControlAccess(LFocus).OnKeyUp;
    TNyxWinControlAccess(LFocus).OnKeyDown := LBinding.KeyDown;
    TNyxWinControlAccess(LFocus).OnKeyUp := LBinding.KeyUp;
  end;

  if LBinding.FDateField <> nil then
  begin
    { Preserve the grouped edit's forwarding slots. Keyboard, focus, pointer,
      selection and editing bridges stay attached to its actual inner editor. }
    LBinding.FDateField.OnChange := LBinding.Change;
    LBinding.FDateField.OnEditingDone := LBinding.CommitValue;
  end
  else if LBinding.FTimeField <> nil then
  begin
    { The real grouped input keeps its forwarding/keyboard/focus/IME hooks. }
    LBinding.FTimeField.OnChange := LBinding.Change;
    LBinding.FTimeField.OnEditingDone := LBinding.CommitValue;
  end
  else if LBinding.FColorField <> nil then
  begin
    LBinding.FColorField.OnChange := LBinding.Change;
    LBinding.FColorField.OnEditingDone := LBinding.CommitValue;
  end
  else if LInput is TCustomEdit then
  begin
    TEdit(LInput).OnChange := LBinding.Change;

    if LBinding.FDeferredValue then
    begin
      TEdit(LInput).OnEditingDone := LBinding.CommitValue;
    end;
  end
  else if LInput is TComboBox then
  begin
    TComboBox(LInput).OnChange := LBinding.Change;
  end
  else if LInput is TCheckBox then
  begin
    TCheckBox(LInput).OnChange := LBinding.Change;
  end
  else if LInput is TRadioButton then
  begin
    TRadioButton(LInput).OnChange := LBinding.Change;
  end
  else if LInput is TTrackBar then
  begin
    TTrackBar(LInput).OnChange := LBinding.Change;
  end;

  if (ANode.Count > 0) and not (Result is TWinControl) then
  begin
    raise ENyxModel.Create('Native leaf control cannot contain child controls');
  end;
  {$ifdef NYX_STUDIO_PROFILE}
  Inc(FBuildBindingMilliseconds, GetTickCount64 - LBuildStarted);
  {$endif}
  for LIndex := 0 to ANode.Count - 1 do
  begin

    if Result is TNyxLCLTabHost then
    begin
      Build(ANode.Children[LIndex], TNyxLCLTabHost(Result).PageHost(LIndex));
    end
    else if Result is TNyxLCLSplitView then
    begin
      Build(ANode.Children[LIndex], TNyxLCLSplitView(Result).Panes[LIndex]);
    end
    else
    begin
      Build(ANode.Children[LIndex], TWinControl(Result));
    end;
  end;

  {$ifdef NYX_STUDIO_PROFILE}LBuildStarted := GetTickCount64;{$endif}

  if Result is TNyxLCLSplitView then
  begin
    TNyxLCLSplitView(Result).OnLayout := LBinding.SplitLayout;
    TNyxLCLSplitView(Result).OnChanged := LBinding.SplitChanged;
    TNyxLCLSplitView(Result).Ready;
  end;
  Result.Tag := Length(FBindings);
  {$ifdef NYX_STUDIO_PROFILE}
  Inc(FBuildCompletionMilliseconds, GetTickCount64 - LBuildStarted);
  {$endif}
end;

function TNyxLCLRenderer.Binding(ANode: TNyxNode): TNyxLCLBinding;
var
  LIndex: Integer;
begin
  for LIndex := 0 to Length(FBindings) - 1 do
  begin

    if FBindings[LIndex].FNode = ANode then
    begin
      Exit(FBindings[LIndex]);
    end;
  end;
  raise ENyxModel.Create('Native projection is missing a node');
end;

function TNyxLCLRenderer.VisibleChildren(ANode: TNyxNode): Integer;
var
  LIndex: Integer;
begin
  { Geometry follows the current visible/disclosure path only. Enabled and
    read-only command constraints remain owned by Sync and actual input; do
    not re-evaluate them for every descendant intrinsic-size traversal. }
  Result := 0;
  for LIndex := 0 to ANode.Count - 1 do
  begin

    if NyxInteractionVisible(ANode.Children[LIndex]) then
    begin
      Inc(Result);
    end;
  end;
end;

function TNyxLCLRenderer.NaturalWidth(ANode: TNyxNode; AAvailable: Integer): Integer;
var
  LGeneration: INyxLCLBindingGeneration;
begin
  LGeneration := FGeneration;
  RequireNativeGeneration(LGeneration);

  {$ifdef NYX_LCL_LAYOUT_PROFILE}Inc(FNaturalCalls);
                                 {$endif}
  Result := NyxNodeSizeConstraints(ANode).WidthRange.Clamp(
    NaturalContentWidth(ANode, AAvailable));
    RequireNativeGeneration(LGeneration);
end;

procedure TNyxLCLRenderer.ClearLabelMeasurements;
var
  LIndex: Integer;
begin
  for LIndex := 0 to High(FLabelMeasurements) do
  begin
    FLabelMeasurements[LIndex].Font.Free;
  end;
  FLabelMeasurements := nil;
end;

procedure TNyxLCLRenderer.LabelSize(ALabel: TLabel; AWidth: Integer;
  AWrap: Boolean; out ATextWidth, ATextHeight: Integer);
var
  LGeneration: INyxLCLBindingGeneration;
  LIndex: Integer;
  LCount: Integer;
  LText: TNyxText;
  LFont: TFont;
begin
  LGeneration := FGeneration;
  RequireNativeGeneration(LGeneration);

  AWidth := Max(1, AWidth);
  RequireNativeGeneration(LGeneration);
  LText := TNyxText(ALabel.Caption);
  RequireNativeGeneration(LGeneration);

  if FLayouting then
  begin
    for LIndex := 0 to High(FLabelMeasurements) do
    begin

      if (FLabelMeasurements[LIndex].Text = LText) and
        (FLabelMeasurements[LIndex].AvailableWidth = AWidth) and
        (FLabelMeasurements[LIndex].Wrap = AWrap) and
        (FLabelMeasurements[LIndex].ShowAccelChar = ALabel.ShowAccelChar) and
        (FLabelMeasurements[LIndex].BiDiMode = ALabel.BiDiMode) and
        (FLabelMeasurements[LIndex].Font.PixelsPerInch = ALabel.Font.PixelsPerInch) and
        FLabelMeasurements[LIndex].Font.IsEqual(ALabel.Font) then
      begin
        RequireNativeGeneration(LGeneration);
        ATextWidth := FLabelMeasurements[LIndex].Width;
        RequireNativeGeneration(LGeneration);
        ATextHeight := FLabelMeasurements[LIndex].Height;
        RequireNativeGeneration(LGeneration);
        {$ifdef NYX_LCL_LAYOUT_PROFILE}Inc(FLabelMeasurementsReused);
                                       {$endif}
        Exit;
      end;
      RequireNativeGeneration(LGeneration);
    end;
    RequireNativeGeneration(LGeneration);
  end;

  if FMeasurementLabel = nil then
  begin
    FMeasurementLabel := TNyxIntrinsicLabel.Create(nil);
    RequireNativeGeneration(LGeneration);
    FMeasurementLabel.Visible := False;
    FMeasurementLabel.AutoSize := False;
  end;
  { LCL Assign preserves its destination DPI and otherwise converts through
    integer point size. Set DPI first so explicit pixel height remains exact. }
  FMeasurementLabel.Font.PixelsPerInch := ALabel.Font.PixelsPerInch;
  RequireNativeGeneration(LGeneration);
  FMeasurementLabel.Font.Assign(ALabel.Font);
  RequireNativeGeneration(LGeneration);
  FMeasurementLabel.Caption := ALabel.Caption;
  RequireNativeGeneration(LGeneration);
  FMeasurementLabel.ShowAccelChar := ALabel.ShowAccelChar;
  RequireNativeGeneration(LGeneration);
  FMeasurementLabel.BiDiMode := ALabel.BiDiMode;
  RequireNativeGeneration(LGeneration);
  FMeasurementLabel.WordWrap := AWrap;

  if FLayouting and (FMeasurementContext = 0) then
  begin
    FMeasurementContext := LCLIntf.GetDC(0);
    RequireNativeGeneration(LGeneration);

    if FMeasurementContext = 0 then
    begin
      raise ENyxModel.Create('The native label measurement context is unavailable');
    end;
  end;
  RequireNativeGeneration(LGeneration);
  TNyxIntrinsicLabel(FMeasurementLabel).TextSize(Max(1, AWidth),
    ATextWidth, ATextHeight, FMeasurementContext);
    RequireNativeGeneration(LGeneration);
  {$ifdef NYX_LCL_LAYOUT_PROFILE}Inc(FLabelMeasurementsTaken);
                                 {$endif}

  if FLayouting and (Length(FLabelMeasurements) < 1024) then
  begin
    { Never evict/reuse an owned font while another measurement is in progress.
      At capacity, ordinary measurement continues with identical semantics. }
    LFont := TFont.Create;
    RequireNativeGeneration(LGeneration);
    try
      LFont.PixelsPerInch := ALabel.Font.PixelsPerInch;
      RequireNativeGeneration(LGeneration);
      LFont.Assign(ALabel.Font);
      RequireNativeGeneration(LGeneration);
      LCount := Length(FLabelMeasurements);
      RequireNativeGeneration(LGeneration);
      SetLength(FLabelMeasurements, LCount + 1);
      RequireNativeGeneration(LGeneration);
      FLabelMeasurements[LCount].Text := LText;
      FLabelMeasurements[LCount].AvailableWidth := AWidth;
      FLabelMeasurements[LCount].Wrap := AWrap;
      FLabelMeasurements[LCount].ShowAccelChar := ALabel.ShowAccelChar;
      RequireNativeGeneration(LGeneration);
      FLabelMeasurements[LCount].BiDiMode := ALabel.BiDiMode;
      RequireNativeGeneration(LGeneration);
      FLabelMeasurements[LCount].Width := ATextWidth;
      FLabelMeasurements[LCount].Height := ATextHeight;
      FLabelMeasurements[LCount].Font := LFont;
      LFont := nil;
      RequireNativeGeneration(LGeneration);
    finally
      LFont.Free;
    end;
    RequireNativeGeneration(LGeneration);
  end;
  RequireNativeGeneration(LGeneration);
end;

function TNyxLCLRenderer.ContentInsets(ANode: TNyxNode): TRect;
var
  LGeneration: INyxLCLBindingGeneration;
  LControl: TControl;
  LGroup: TWinControl;
  LWindow: TRect;
  LClient: TRect;
begin
  LGeneration := FGeneration;
  RequireNativeGeneration(LGeneration);

  Result := Rect(0, 0, 0, 0);
  RequireNativeGeneration(LGeneration);

  if (ANode.ProjectionKind = 'scroll') and not FVirtualLayout then
  begin
    RequireNativeGeneration(LGeneration);
    LControl := Binding(ANode).FControl;
    RequireNativeGeneration(LGeneration);

    if LControl is TScrollBox then
    begin
      { LCL reports the real client extent after native bars consume space.
        Keep the authored outer box, but allocate descendants inside that
        viewport. No platform-specific scrollbar metric is guessed here. }
      Result.Right := Max(0, LControl.Width - LControl.ClientWidth);
      RequireNativeGeneration(LGeneration);
      Result.Bottom := Max(0, LControl.Height - LControl.ClientHeight);
      RequireNativeGeneration(LGeneration);
    end;
    Exit;
  end;
  RequireNativeGeneration(LGeneration);

  if (ANode.ProjectionKind <> 'group') and (FactoryIndex(ANode) < 0) then
  begin
    RequireNativeGeneration(LGeneration);
    { The common undecorated built-ins need no binding lookup/native query. }
    Exit;
  end;
  RequireNativeGeneration(LGeneration);
  LControl := Binding(ANode).FControl;
  RequireNativeGeneration(LGeneration);

  if not (LControl is TCustomGroupBox) then
  begin
    Exit;
  end;
  RequireNativeGeneration(LGeneration);
  LGroup := TWinControl(LControl);
  RequireNativeGeneration(LGeneration);
  LGroup.HandleNeeded;
  RequireNativeGeneration(LGeneration);
  { The widgetset's unclamped client bounds include its caption/frame offsets.
    TWinControl.ClientWidth/Height clamp a parked face to zero and would erase
    that information. Query the actual owned handle, with no temporary window,
    authored changes or hard-coded Win32 caption/font metrics. }

  if (LCLIntf.GetWindowRect(LGroup.Handle, LWindow) = 0) or
    not WidgetSet.GetClientBounds(LGroup.Handle, LClient) then
  begin
    RequireNativeGeneration(LGeneration);
    raise ENyxModel.Create('Unable to measure native group content: ' + ANode.ID);
  end;
  RequireNativeGeneration(LGeneration);
  Result.Left := Max(0, LClient.Left);
  RequireNativeGeneration(LGeneration);
  Result.Top := Max(0, LClient.Top);
  RequireNativeGeneration(LGeneration);
  Result.Right := Max(0, LWindow.Right - LWindow.Left - LClient.Right);
  RequireNativeGeneration(LGeneration);
  Result.Bottom := Max(0, LWindow.Bottom - LWindow.Top - LClient.Bottom);
  RequireNativeGeneration(LGeneration);
end;

function TNyxLCLRenderer.NaturalContentWidth(ANode: TNyxNode; AAvailable: Integer): Integer;
var
  LGeneration: INyxLCLBindingGeneration;
  LControl: TControl;
  LLabel: TLabel;
  LIndex: Integer;
  LWidth: Integer;
  LHeight: Integer;
  LVisible: Integer;
  LSum: Double;
  LFrame: TRect;
  LContentAvailable: Integer;
begin
  LGeneration := FGeneration;
  RequireNativeGeneration(LGeneration);

  AAvailable := Max(0, AAvailable);
  RequireNativeGeneration(LGeneration);

  if ANode.Prop('width-sizing') = 'fill' then
  begin
    RequireNativeGeneration(LGeneration);
    Exit(AAvailable);
  end;
  RequireNativeGeneration(LGeneration);

  if ((ANode.Prop('width-sizing') = '') or (ANode.Prop('width-sizing') = 'auto')) and
    (ANode.Prop('width') <> '') then
  begin
    RequireNativeGeneration(LGeneration);
    Exit(Min(AAvailable, Metric(ANode, 'width', AAvailable)));
  end;
  RequireNativeGeneration(LGeneration);

  if ANode.QueryContainer.Defined then
  begin
    RequireNativeGeneration(LGeneration);
    { Intrinsic width containment is an empty content contribution, rather
      than a guessed fixed width. Fill/flex allocation, explicit width and
      typed bounds remain owned by the ordinary external allocator. }
    Exit(Min(AAvailable, 2 * Metric(ANode, 'padding', 0)));
  end;
  RequireNativeGeneration(LGeneration);
  Result := 0;

  if (ANode.Count > 0) and (ANode.ProjectionKind <> 'split-view') then
  begin
    RequireNativeGeneration(LGeneration);
    LFrame := ContentInsets(ANode);
    RequireNativeGeneration(LGeneration);
    LContentAvailable := Max(0, AAvailable - LFrame.Left - LFrame.Right);
    RequireNativeGeneration(LGeneration);
    LSum := 0;
    LVisible := 0;
    RequireNativeGeneration(LGeneration);
    for LIndex := 0 to ANode.Count - 1 do
    begin
      RequireNativeGeneration(LGeneration);

      if not NyxInteractionVisible(ANode.Children[LIndex]) then
      begin
        RequireNativeGeneration(LGeneration);
        Continue;
      end;
      RequireNativeGeneration(LGeneration);
      Inc(LVisible);
      LWidth := NaturalWidth(ANode.Children[LIndex], LContentAvailable);
      RequireNativeGeneration(LGeneration);
      Result := Max(Result, LWidth);
      RequireNativeGeneration(LGeneration);
      LSum := LSum + LWidth;
    end;
    RequireNativeGeneration(LGeneration);

    if NyxLayout(ANode) = 'row' then
    begin
      Result := Trunc(Min(Double(AAvailable), LSum +
        Double(Max(0, LVisible - 1)) * Metric(ANode, 'gap', 12)));
        RequireNativeGeneration(LGeneration);
    end
    else if NyxLayout(ANode) = 'grid' then
    begin
      RequireNativeGeneration(LGeneration);
      { Large admitted column counts must be capped before Integer conversion. }
      Result := Trunc(Min(Double(AAvailable), Double(Result) *
        Metric(ANode, 'columns', 2)));
        RequireNativeGeneration(LGeneration);
    end;
    RequireNativeGeneration(LGeneration);
    Exit(Min(AAvailable, Result + 2 * Metric(ANode, 'padding', 0) +
      LFrame.Left + LFrame.Right));
  end;
  RequireNativeGeneration(LGeneration);
  LControl := Binding(ANode).FControl;
  RequireNativeGeneration(LGeneration);

  LWidth := 0;
  RequireNativeGeneration(LGeneration);
  LHeight := 0;
  RequireNativeGeneration(LGeneration);

  if LControl is TLabel then
  begin
    LLabel := TLabel(LControl);
    RequireNativeGeneration(LGeneration);
    { Changing a visible label's WordWrap invalidates all ancestor preferred
      sizes. LCL may be querying those very sizes through a split resize, making
      that mutation recursive and raising ELayoutException. Detached measurement
      has the same widgetset/font/text semantics and no ancestor invalidation. }

    LabelSize(LLabel, 10000, False, LWidth, LHeight);
    RequireNativeGeneration(LGeneration);
  end
  else
  begin

    if LControl.Parent <> nil then
    begin
      RequireNativeGeneration(LGeneration);
      LControl.Parent.HandleNeeded;
      RequireNativeGeneration(LGeneration);
    end;
    RequireNativeGeneration(LGeneration);
    LControl.GetPreferredSize(LWidth, LHeight, True, False);
    RequireNativeGeneration(LGeneration);
  end;

  if (Binding(ANode).FInput <> nil) and (Binding(ANode).FInput <> LControl) then
  begin
    LWidth := Max(LWidth, 200);
    RequireNativeGeneration(LGeneration);
  end;
  RequireNativeGeneration(LGeneration);

  if ANode.ProjectionKind = 'spacer' then
  begin
    LWidth := 0;
    RequireNativeGeneration(LGeneration);
  end;
  Result := Min(AAvailable, Max(0, LWidth));
  RequireNativeGeneration(LGeneration);
end;

function TNyxLCLRenderer.EffectiveWidth(ANode: TNyxNode; AAvailable: Integer;
  AAllocated: Boolean): Integer;
begin
  Result := Max(0, AAvailable);

  if AAllocated then
  begin
    Exit(NyxNodeSizeConstraints(ANode).WidthRange.Clamp(Result));
  end;

  if ANode.Prop('width-sizing') = 'content' then
  begin
    Exit(NaturalWidth(ANode, Result));
  end;

  if ANode.Prop('width-sizing') <> 'fill' then
  begin
    Result := Min(Result, Metric(ANode, 'width', Result));
  end;
  Result := NyxNodeSizeConstraints(ANode).WidthRange.Clamp(Result);
end;

function TNyxLCLRenderer.ColumnChildWidth(AParent, AChild: TNyxNode;
  AAvailable: Integer): Integer;
var
  LAlignment: TNyxText;
begin
  Result := EffectiveWidth(AChild, AAvailable);
  LAlignment := AParent.Prop('cross-alignment', 'auto');

  if (AChild.Prop('width-sizing') = 'fill') or (AChild.Prop('width') <> '') then
  begin
    Exit;
  end;

  if ((LAlignment <> '') and (LAlignment <> 'auto') and (LAlignment <> 'stretch')) or
    (((LAlignment = '') or (LAlignment = 'auto')) and
    ((AChild.ProjectionKind = 'button') or (AChild.ProjectionKind = 'badge') or
    (AChild.ProjectionKind = 'link'))) then
  begin
    Result := NaturalWidth(AChild, AAvailable);
  end;
end;

function TNyxLCLRenderer.RowPlan(ANode: TNyxNode; AWidth: Integer;
  out AWidths, ALefts, ATops, AHeights: TNyxFlowSizes): Integer;
var
  LItems: TNyxFlowItems;
  LSlice: TNyxFlowItems;
  LRanges, LSliceRanges: TNyxSizeRanges;
  LLines: TNyxFlowLines;
  LSizes: TNyxFlowSizes;
  LPositions: TNyxFlowSizes;
  LIndex: Integer;
  LLine: Integer;
  LLocal: Integer;
  LHeight: Integer;
  LInner: Integer;
  LGap: Integer;
  LWrap: Boolean;
  LJustification: TNyxJustification;
  LFrame: TRect;
begin
  {$ifdef NYX_LCL_LAYOUT_PROFILE}Inc(FRowPlanCalls);{$endif}
  LFrame := ContentInsets(ANode);
  LInner := Max(0, AWidth - LFrame.Left - LFrame.Right -
    2 * Metric(ANode, 'padding', 0));
  LGap := Metric(ANode, 'gap', 12);
  SetLength(LItems, ANode.Count);
  SetLength(LRanges, ANode.Count);
  SetLength(AWidths, ANode.Count);
  SetLength(ALefts, ANode.Count);
  SetLength(ATops, ANode.Count);
  SetLength(AHeights, ANode.Count);
  for LIndex := 0 to ANode.Count - 1 do
  begin
    LItems[LIndex].Visible := NyxInteractionVisible(ANode.Children[LIndex]);
    LItems[LIndex].Weight := NyxFlexWeight(ANode.Children[LIndex]);
    LItems[LIndex].NaturalSize := 0;
    LRanges[LIndex] := NyxNodeSizeConstraints(ANode.Children[LIndex]).WidthRange;

    if LItems[LIndex].Visible and (LItems[LIndex].Weight = 0) then
    begin
      { Hidden entries and zero-basis weights need no intrinsic traversal.
        Avoid measuring a whole nested recipe whose result cannot be consumed. }
      LItems[LIndex].NaturalSize := NaturalWidth(ANode.Children[LIndex], LInner);
    end;
  end;
  LWrap := ANode.Prop('flow-wrap') = 'wrap';

  if (ANode.Prop('flow-wrap') = '') or (ANode.Prop('flow-wrap') = 'auto') then
  begin
    LWrap := FPanel.ClientWidth <= 600;
  end;
  LJustification := njStart;
  for LJustification := Low(TNyxJustification) to High(TNyxJustification) do
  begin

    if NyxJustificationName(LJustification) = ANode.Prop('justification', 'start') then
    begin
      Break;
    end;
  end;
  LLines := NyxFlowLines(LInner, LGap, LItems, LRanges, LWrap);
  Result := 0;
  for LLine := 0 to High(LLines) do
  begin
    SetLength(LSlice, LLines[LLine].Last - LLines[LLine].First + 1);
    SetLength(LSliceRanges, Length(LSlice));
    for LLocal := 0 to High(LSlice) do
    begin
      LSlice[LLocal] := LItems[LLines[LLine].First + LLocal];
      LSliceRanges[LLocal] := LRanges[LLines[LLine].First + LLocal];
    end;
    LSizes := NyxFlowSizes(LInner, LGap, LSlice, LSliceRanges);
    LPositions := NyxFlowPositions(LInner, LGap, LSlice, LSizes, LJustification);
    LHeight := 0;
    for LLocal := 0 to High(LSlice) do
    begin
      LIndex := LLines[LLine].First + LLocal;
      AWidths[LIndex] := LSizes[LLocal];
      ALefts[LIndex] := LPositions[LLocal];
      ATops[LIndex] := Result;

      if LSlice[LLocal].Visible then
      begin
        LHeight := Max(LHeight, Measure(ANode.Children[LIndex], LSizes[LLocal], True));
      end;
    end;
    for LLocal := 0 to High(LSlice) do
    begin
      AHeights[LLines[LLine].First + LLocal] := LHeight;
    end;
    Inc(Result, LHeight);

    if LLine < High(LLines) then
    begin
      Inc(Result, LGap);
    end;
  end;
end;

function TNyxLCLRenderer.LayoutColumns(ANode: TNyxNode; AWidth: Integer): Integer;
begin
  Result := 1;

  if NyxLayout(ANode) = 'grid' then
  begin
    Result := Max(1, Metric(ANode, 'columns', 2));
  end
  else if NyxLayout(ANode) = 'row' then
  begin
    Result := Max(1, VisibleChildren(ANode));
  end;
end;

function TNyxLCLRenderer.Measure(ANode: TNyxNode; AWidth: Integer;
  AAllocatedWidth: Boolean): Integer;
var
  LGeneration: INyxLCLBindingGeneration;
begin
  LGeneration := FGeneration;
  RequireNativeGeneration(LGeneration);

  {$ifdef NYX_LCL_LAYOUT_PROFILE}Inc(FMeasureCalls);
                                 {$endif}

  if not NyxInteractionVisible(ANode) then
  begin
    RequireNativeGeneration(LGeneration);
    Exit(0);
  end;
  RequireNativeGeneration(LGeneration);
  Result := NyxNodeSizeConstraints(ANode).HeightRange.Clamp(
    MeasureContent(ANode, AWidth, AAllocatedWidth));
    RequireNativeGeneration(LGeneration);
end;

function TNyxLCLRenderer.MeasureContent(ANode: TNyxNode; AWidth: Integer;
  AAllocatedWidth: Boolean): Integer;
var
  LGeneration: INyxLCLBindingGeneration;
  LControl: TControl;
  LIndex: Integer;
  LHeight: Integer;
  LPadding: Integer;
  LGap: Integer;
  LColumns: Integer;
  LCellWidth: Integer;
  LRowHeight: Integer;
  LTextWidth: Integer;
  LTextHeight: Integer;
  LLabel: TLabel;
  LVisible: Integer;
  LPosition: Integer;
  LWidths: TNyxFlowSizes;
  LLefts: TNyxFlowSizes;
  LTops: TNyxFlowSizes;
  LHeights: TNyxFlowSizes;
  LFrame: TRect;
  LContentWidth: Integer;
  LButtonFace: TNyxButtonPresentation;
begin
  LGeneration := FGeneration;
  RequireNativeGeneration(LGeneration);

  if not NyxInteractionVisible(ANode) then
  begin
    RequireNativeGeneration(LGeneration);
    Exit(0);
  end;
  RequireNativeGeneration(LGeneration);

  AWidth := EffectiveWidth(ANode, AWidth, AAllocatedWidth);
  RequireNativeGeneration(LGeneration);

  if (ANode.Prop('height') <> '') and
    ((ANode.Prop('height-sizing') = '') or (ANode.Prop('height-sizing') = 'auto')) then
  begin
    RequireNativeGeneration(LGeneration);
    Exit(Metric(ANode, 'height', 32));
  end;
  RequireNativeGeneration(LGeneration);

  LFrame := ContentInsets(ANode);
  RequireNativeGeneration(LGeneration);
  LContentWidth := Max(0, AWidth - LFrame.Left - LFrame.Right);
  RequireNativeGeneration(LGeneration);

  if ANode.QueryContainer.Defined and (ANode.ContainerContainment = nccSize) then
  begin
    RequireNativeGeneration(LGeneration);
    Exit(2 * Metric(ANode, 'padding', 0) + LFrame.Top + LFrame.Bottom);
  end;
  RequireNativeGeneration(LGeneration);
  Result := 28;

  if (ANode.ProjectionKind = 'input') or (ANode.ProjectionKind = 'select') or
    (ANode.ProjectionKind = 'spin') or (ANode.ProjectionKind = 'date') or
    (ANode.ProjectionKind = 'time') or (ANode.ProjectionKind = 'color') then
  begin
    RequireNativeGeneration(LGeneration);
    { Match the shared 1.5 line height plus frame padding and caption space.
      Explicit heights still win; auto sizes must remain usable with large fonts. }
    Result := 24 + (FTheme.FontSize * 3 div 2) + 22;
    RequireNativeGeneration(LGeneration);
  end
  else if ANode.ProjectionKind = 'memo' then
  begin
    RequireNativeGeneration(LGeneration);
    Result := 120;

    if 24 + (FTheme.FontSize * 3 div 2) + 22 > Result then
    begin
      RequireNativeGeneration(LGeneration);
      Result := 24 + (FTheme.FontSize * 3 div 2) + 22;
      RequireNativeGeneration(LGeneration);
    end;
    RequireNativeGeneration(LGeneration);
  end
  else if ANode.ProjectionKind = 'code-editor' then
  begin
    RequireNativeGeneration(LGeneration);
    Result := 240;
  end
  else if ANode.ProjectionKind = 'split-view' then
  begin
    RequireNativeGeneration(LGeneration);
    Result := 320;
    Exit;
  end
  else if (ANode.ProjectionKind = 'table') or (ANode.ProjectionKind = 'tree') or
    (ANode.ProjectionKind = 'list') or (ANode.ProjectionKind = 'code') or
    (ANode.ProjectionKind = 'image') then
  begin
    RequireNativeGeneration(LGeneration);
    Result := 160;
  end
  else if ANode.ProjectionKind = 'heading' then
  begin
    RequireNativeGeneration(LGeneration);
    Result := 38;
  end
  else if ANode.ProjectionKind = 'button' then
  begin
    RequireNativeGeneration(LGeneration);
    Result := (FTheme.FontSize * 3 div 2) + 22;
    RequireNativeGeneration(LGeneration);
    LButtonFace := NyxButtonPresentationOf(ANode);
    RequireNativeGeneration(LGeneration);
    case LButtonFace.ControlDensity of
      nbdNormal: ;
       { Retain ordinary metrics. }
      nbdCompact: Result := Max(32, FTheme.FontSize + 16);
      nbdTouch: Result := Max(44, FTheme.FontSize + 24);
    end;

    if LButtonFace.HasGlyph and (LButtonFace.ContentMode <> nbcCaption) then
    begin
      RequireNativeGeneration(LGeneration);
      Result := Max(Result, LButtonFace.GlyphBoxSize + 8);
      RequireNativeGeneration(LGeneration);
    end;
    RequireNativeGeneration(LGeneration);
  end
  else if ANode.ProjectionKind = 'separator' then
  begin
    RequireNativeGeneration(LGeneration);
    Result := 2;
  end
  else if ANode.ProjectionKind = 'spacer' then
  begin
    RequireNativeGeneration(LGeneration);
    Result := 16;
  end;
  RequireNativeGeneration(LGeneration);

  if ANode.Count = 0 then
  begin
    RequireNativeGeneration(LGeneration);
    { A leaf has one owned target face throughout this synchronous measurement.
      Resolve its binding once; repeated scans do not add admission or identity
      checks and become quadratic across a large column of captions. No binding
      or measured size is retained beyond this invocation. }
    LControl := Binding(ANode).FControl;
    RequireNativeGeneration(LGeneration);

    if LControl is TCustomGroupBox then
    begin
      Exit(2 * Metric(ANode, 'padding', 0) + LFrame.Top + LFrame.Bottom);
    end;
    { Measure real LCL typography at the allocated width. Fixed one-line label
      heights clip headings/captions in a phone-width view or enlarged fonts. }

    if LControl is TLabel then
    begin
      LLabel := TLabel(LControl);
      RequireNativeGeneration(LGeneration);
      LTextWidth := AWidth;
      RequireNativeGeneration(LGeneration);

      if LTextWidth < 1 then
      begin
        LTextWidth := 1;
        RequireNativeGeneration(LGeneration);
      end;
      LTextHeight := 0;
      RequireNativeGeneration(LGeneration);
      LabelSize(LLabel, LTextWidth, LLabel.WordWrap, LTextWidth, LTextHeight);
      RequireNativeGeneration(LGeneration);

      if LTextHeight + 4 > Result then
      begin
        Result := LTextHeight + 4;
      end;
    end;
    Exit;
  end;
  RequireNativeGeneration(LGeneration);
  LPadding := Metric(ANode, 'padding', 0);
  RequireNativeGeneration(LGeneration);
  LGap := Metric(ANode, 'gap', 12);
  RequireNativeGeneration(LGeneration);
  LVisible := VisibleChildren(ANode);
  RequireNativeGeneration(LGeneration);
  Result := 0;

  if NyxLayout(ANode) = 'row' then
  begin
    Result := RowPlan(ANode, AWidth, LWidths, LLefts, LTops, LHeights);
    RequireNativeGeneration(LGeneration);
  end
  else if NyxLayout(ANode) = 'grid' then
  begin
    RequireNativeGeneration(LGeneration);
    LColumns := LayoutColumns(ANode, AWidth);
    RequireNativeGeneration(LGeneration);
    LCellWidth := Max(0, LContentWidth - 2 * LPadding - LGap * (LColumns - 1)) div LColumns;
    RequireNativeGeneration(LGeneration);
    LRowHeight := 0;
    RequireNativeGeneration(LGeneration);
    LPosition := 0;
    RequireNativeGeneration(LGeneration);
    for LIndex := 0 to ANode.Count - 1 do
    begin
      RequireNativeGeneration(LGeneration);

      if not NyxInteractionVisible(ANode.Children[LIndex]) then
      begin
        RequireNativeGeneration(LGeneration);
        Continue;
      end;
      RequireNativeGeneration(LGeneration);

      LHeight := Measure(ANode.Children[LIndex], LCellWidth);
      RequireNativeGeneration(LGeneration);
      Inc(LPosition);

      if LHeight > LRowHeight then
      begin
        LRowHeight := LHeight;
        RequireNativeGeneration(LGeneration);
      end;

      if (LPosition mod LColumns = 0) or (LPosition = LVisible) then
      begin
        Inc(Result, LRowHeight);
        LRowHeight := 0;
        RequireNativeGeneration(LGeneration);

        if LPosition < LVisible then
        begin
          Inc(Result, LGap);
        end;
      end;
      RequireNativeGeneration(LGeneration);
    end;
    RequireNativeGeneration(LGeneration);
  end
  else
  begin
    RequireNativeGeneration(LGeneration);
    for LIndex := 0 to ANode.Count - 1 do
    begin
      RequireNativeGeneration(LGeneration);

      if NyxInteractionVisible(ANode.Children[LIndex]) then
      begin
        RequireNativeGeneration(LGeneration);
        Inc(Result, Measure(ANode.Children[LIndex],
          ColumnChildWidth(ANode, ANode.Children[LIndex],
            Max(0, LContentWidth - 2 * LPadding)), True));
            RequireNativeGeneration(LGeneration);
      end;
      RequireNativeGeneration(LGeneration);
    end;
    RequireNativeGeneration(LGeneration);
    Inc(Result, LGap * Max(0, LVisible - 1));
  end;
  RequireNativeGeneration(LGeneration);
  Inc(Result, 2 * LPadding + LFrame.Top + LFrame.Bottom);
end;

procedure TNyxLCLRenderer.Layout(ANode: TNyxNode; AX, AY, AWidth: Integer;
  AHeight: Integer; AAllocatedWidth: Boolean);
var
  LGeneration: INyxLCLBindingGeneration;
  LBinding: TNyxLCLBinding;
  LIndex: Integer;
  LPadding: Integer;
  LGap: Integer;
  LWidth: Integer;
  LHeight: Integer;
  LColumns: Integer;
  LCellWidth: Integer;
  LCellHeight: Integer;
  LChild: TNyxNode;
  LInputHeight: Integer;
  LFitHeight: Integer;
  LFitWidth: Integer;
  LChildY: Integer;
  LVisible: Integer;
  LPosition: Integer;
  LDefiniteColumn: Boolean;
  LItems: TNyxFlowItems;
  LRanges: TNyxSizeRanges;
  LSizes: TNyxFlowSizes;
  LLefts: TNyxFlowSizes;
  LTops: TNyxFlowSizes;
  LHeights: TNyxFlowSizes;
  LPositions: TNyxFlowSizes;
  LAlignment: TNyxText;
  LJustification: TNyxJustification;
  LChildX: Integer;
  LFrame: TRect;
begin
  LGeneration := FGeneration;
  RequireNativeGeneration(LGeneration);

  LBinding := Binding(ANode);
  RequireNativeGeneration(LGeneration);
  { The browser theme caps every node at its containing block's available
    width. Apply that same cap before measuring native wrapping/descendants. }
  LWidth := EffectiveWidth(ANode, AWidth, AAllocatedWidth);
  RequireNativeGeneration(LGeneration);

  if AHeight >= 0 then
  begin
    { The parent has already admitted this allocation through its measured flow
      plan, or a bounded host supplies the definite extent. Measuring the entire
      child subtree here only to discard that height duplicates typography and
      descendant work. Intrinsic allocations still use Measure below; final
      constraints and every descendant's own layout remain applied as usual. }
    LHeight := AHeight;
    RequireNativeGeneration(LGeneration);
  end
  else
  begin
    LHeight := Measure(ANode, LWidth, True);
    RequireNativeGeneration(LGeneration);
  end;
  LHeight := NyxNodeSizeConstraints(ANode).HeightRange.Clamp(LHeight);
  RequireNativeGeneration(LGeneration);
  LBinding.FLogicalBox := NyxViewportBox(AX, AY, LWidth, LHeight);
  RequireNativeGeneration(LGeneration);
  LBinding.FContainerAllocated := True;

  if not FVirtualLayout then
  begin
    LBinding.FControl.SetBounds(AX, AY, LWidth, LHeight);
    RequireNativeGeneration(LGeneration);
    ArrangeInput(LBinding, LWidth, LHeight);
    RequireNativeGeneration(LGeneration);
  end;

  if LBinding.FControl is TNyxLCLSplitView then
  begin

    if FVirtualLayout then
    begin
      { The split owns ordinary bounded pane hosts. Its existing layout callback
        records child logical boxes against those pane dimensions. Pane/grip
        origins use signed native message coordinates even when the containing
        window's unsigned size would be legal. Refuse that unsupported geometry
        before Arrange can move a pane outside its physical coordinate domain. }

      if (LWidth > High(SmallInt)) or (LHeight > High(SmallInt)) then
      begin
        raise ENyxModel.Create('An oversized native split requires logical pane projection');
      end;
      RequireNativeGeneration(LGeneration);
      LBinding.FControl.SetBounds(0, 0, LWidth, LHeight);
      RequireNativeGeneration(LGeneration);
    end;
    TNyxLCLSplitView(LBinding.FControl).Arrange;
    RequireNativeGeneration(LGeneration);
    Exit;
  end;

  if LBinding.FControl is TNyxLCLTabHost then
  begin
    TNyxLCLTabHost(LBinding.FControl).Arrange;
    RequireNativeGeneration(LGeneration);
    for LIndex := 0 to ANode.Count - 1 do
    begin
      RequireNativeGeneration(LGeneration);
      Layout(ANode.Children[LIndex], 0, 0,
        TNyxLCLTabHost(LBinding.FControl).PageHost(LIndex).ClientWidth,
        TNyxLCLTabHost(LBinding.FControl).PageHost(LIndex).ClientHeight);
        RequireNativeGeneration(LGeneration);
    end;
    RequireNativeGeneration(LGeneration);
    Exit;
  end;

  LPadding := Metric(ANode, 'padding', 0);
  RequireNativeGeneration(LGeneration);
  { Logical outer boxes remain the authored faces. Their children are allocated
    within native usable content, independently of the current physical clip. }
  LFrame := ContentInsets(ANode);
  RequireNativeGeneration(LGeneration);
  LWidth := Max(0, LWidth - LFrame.Left - LFrame.Right);
  RequireNativeGeneration(LGeneration);
  LHeight := Max(0, LHeight - LFrame.Top - LFrame.Bottom);
  RequireNativeGeneration(LGeneration);
  LGap := Metric(ANode, 'gap', 12);
  RequireNativeGeneration(LGeneration);
  LAlignment := ANode.Prop('cross-alignment', 'auto');
  RequireNativeGeneration(LGeneration);
  { Rows use one measured line plan for both wrapping and arrangement. Retain
    every control; changing a policy only changes geometry, never input state. }

  if NyxLayout(ANode) = 'row' then
  begin
    RowPlan(ANode, LBinding.FLogicalBox.Width, LSizes, LLefts, LTops, LHeights);
    RequireNativeGeneration(LGeneration);
    for LIndex := 0 to ANode.Count - 1 do
    begin
      RequireNativeGeneration(LGeneration);
      LChild := ANode.Children[LIndex];
      RequireNativeGeneration(LGeneration);

      if not NyxInteractionVisible(LChild) then
      begin
        RequireNativeGeneration(LGeneration);
        Continue;
      end;
      RequireNativeGeneration(LGeneration);
      LCellHeight := LHeights[LIndex];
      RequireNativeGeneration(LGeneration);
      { A single flex line fills a definite cross axis. Its natural height can
        greatly exceed the allocated workspace when a child is a scroll view.
        Stretch/center/end must use the admitted host extent, as browser flex
        does, rather than growing that viewport to all of its scroll content. }

      if (ANode.Prop('flow-wrap') = 'nowrap') or
        (((ANode.Prop('flow-wrap') = '') or (ANode.Prop('flow-wrap') = 'auto')) and
        (FPanel.ClientWidth > 600)) then
      begin
        RequireNativeGeneration(LGeneration);

        if (AHeight >= 0) or ((ANode.Prop('height') <> '') and
          ((ANode.Prop('height-sizing') = '') or (ANode.Prop('height-sizing') = 'auto'))) then
        begin
          RequireNativeGeneration(LGeneration);
          LCellHeight := Max(0, LHeight - 2 * LPadding);
          RequireNativeGeneration(LGeneration);
        end
        else
        begin
          RequireNativeGeneration(LGeneration);
          LCellHeight := Max(LCellHeight, LHeight - 2 * LPadding);
          RequireNativeGeneration(LGeneration);
        end;
        RequireNativeGeneration(LGeneration);
      end;
      RequireNativeGeneration(LGeneration);
      LFitHeight := Measure(LChild, LSizes[LIndex], True);
      RequireNativeGeneration(LGeneration);

      if (LChild.Prop('height-sizing') = 'fill') or ((LAlignment = 'stretch') and
        ((LChild.Prop('height-sizing') = '') or (LChild.Prop('height-sizing') = 'auto')) and
        (LChild.Prop('height') = '')) then
      begin
        RequireNativeGeneration(LGeneration);
        LFitHeight := LCellHeight;
        RequireNativeGeneration(LGeneration);
      end;
      RequireNativeGeneration(LGeneration);
      { Clamp before alignment, so a capped stretched control is positioned
        against its actual height rather than its discarded line allocation. }
      LFitHeight := NyxNodeSizeConstraints(LChild).HeightRange.Clamp(LFitHeight);
      RequireNativeGeneration(LGeneration);
      LChildY := LPadding + LTops[LIndex];
      RequireNativeGeneration(LGeneration);

      if (LAlignment = '') or (LAlignment = 'auto') or (LAlignment = 'center') then
      begin
        Inc(LChildY, Max(0, LCellHeight - LFitHeight) div 2);
      end
      else if LAlignment = 'end' then
      begin
        RequireNativeGeneration(LGeneration);
        Inc(LChildY, Max(0, LCellHeight - LFitHeight));
      end;
      RequireNativeGeneration(LGeneration);
      Layout(LChild, LPadding + LLefts[LIndex], LChildY, LSizes[LIndex], LFitHeight, True);
      RequireNativeGeneration(LGeneration);
    end;
    RequireNativeGeneration(LGeneration);
    Exit;
  end;
  RequireNativeGeneration(LGeneration);
  AY := LPadding;
  AX := LPadding;
  LColumns := LayoutColumns(ANode, LWidth);
  RequireNativeGeneration(LGeneration);
  LVisible := VisibleChildren(ANode);
  RequireNativeGeneration(LGeneration);
  LPosition := 0;
  RequireNativeGeneration(LGeneration);
  LDefiniteColumn := (NyxLayout(ANode) = 'column') and
    ((AHeight >= 0) or ((ANode.Prop('height') <> '') and
    ((ANode.Prop('height-sizing') = '') or (ANode.Prop('height-sizing') = 'auto'))));
    RequireNativeGeneration(LGeneration);

  if NyxLayout(ANode) = 'column' then
  begin
    SetLength(LItems, ANode.Count);
    RequireNativeGeneration(LGeneration);
    SetLength(LRanges, ANode.Count);
    RequireNativeGeneration(LGeneration);
    for LIndex := 0 to ANode.Count - 1 do
    begin
      RequireNativeGeneration(LGeneration);
      LItems[LIndex].Visible := NyxInteractionVisible(ANode.Children[LIndex]);
      RequireNativeGeneration(LGeneration);
      LRanges[LIndex] := NyxNodeSizeConstraints(ANode.Children[LIndex]).HeightRange;
      RequireNativeGeneration(LGeneration);
      LItems[LIndex].Weight := 0;

      if LDefiniteColumn then
      begin
        LItems[LIndex].Weight := NyxFlexWeight(ANode.Children[LIndex]);
        RequireNativeGeneration(LGeneration);
      end;
      LItems[LIndex].NaturalSize := Measure(ANode.Children[LIndex],
        ColumnChildWidth(ANode, ANode.Children[LIndex], Max(0, LWidth - 2 * LPadding)), True);
        RequireNativeGeneration(LGeneration);

      if LDefiniteColumn and (ANode.Children[LIndex].Prop('height-sizing') = 'fill') then
      begin
        RequireNativeGeneration(LGeneration);
        LItems[LIndex].NaturalSize := Max(0, LHeight - 2 * LPadding);
        RequireNativeGeneration(LGeneration);
      end;
      RequireNativeGeneration(LGeneration);
    end;
    RequireNativeGeneration(LGeneration);
    LSizes := NyxFlowSizes(Max(0, LHeight - 2 * LPadding), LGap, LItems, LRanges);
    RequireNativeGeneration(LGeneration);
    LJustification := njStart;
    for LJustification := Low(TNyxJustification) to High(TNyxJustification) do
    begin

      if NyxJustificationName(LJustification) = ANode.Prop('justification', 'start') then
      begin
        RequireNativeGeneration(LGeneration);
        Break;
      end;
      RequireNativeGeneration(LGeneration);
    end;
    RequireNativeGeneration(LGeneration);
    LPositions := NyxFlowPositions(Max(0, LHeight - 2 * LPadding), LGap,
      LItems, LSizes, LJustification);
      RequireNativeGeneration(LGeneration);
  end;
  RequireNativeGeneration(LGeneration);
  LCellWidth := Max(0, LWidth - 2 * LPadding - LGap * (LColumns - 1)) div LColumns;
  RequireNativeGeneration(LGeneration);
  LCellHeight := 0;
  RequireNativeGeneration(LGeneration);
  for LIndex := 0 to ANode.Count - 1 do
  begin
    RequireNativeGeneration(LGeneration);
    LChild := ANode.Children[LIndex];
    RequireNativeGeneration(LGeneration);

    if not NyxInteractionVisible(LChild) then
    begin
      RequireNativeGeneration(LGeneration);
      { Keep the mounted control and draft, but never reserve a flow slot/gap.
        Its next visible layout recomputes descendants at the current size. }
      Continue;
    end;
    RequireNativeGeneration(LGeneration);
    Inc(LPosition);

    if ANode.Prop('layout') = 'absolute' then
    begin
      RequireNativeGeneration(LGeneration);
      Layout(LChild, Metric(LChild, 'left', 0), Metric(LChild, 'top', 0), LCellWidth);
      RequireNativeGeneration(LGeneration);
    end
    else
    begin
      RequireNativeGeneration(LGeneration);
      LFitHeight := -1;
      RequireNativeGeneration(LGeneration);
      LFitWidth := LCellWidth;
      RequireNativeGeneration(LGeneration);
      LChildY := AY;
      RequireNativeGeneration(LGeneration);

      if NyxLayout(ANode) = 'column' then
      begin
        LFitHeight := LSizes[LIndex];
        RequireNativeGeneration(LGeneration);
        LFitWidth := ColumnChildWidth(ANode, LChild, Max(0, LWidth - 2 * LPadding));
        RequireNativeGeneration(LGeneration);
        LChildY := LPadding + LPositions[LIndex];
        RequireNativeGeneration(LGeneration);
        LChildX := LPadding;
        RequireNativeGeneration(LGeneration);

        if LAlignment = 'center' then
        begin
          Inc(LChildX, Max(0, LWidth - 2 * LPadding - LFitWidth) div 2);
        end
        else if LAlignment = 'end' then
        begin
          Inc(LChildX, Max(0, LWidth - 2 * LPadding - LFitWidth));
        end;
        Layout(LChild, LChildX, LChildY, LFitWidth, LFitHeight, True);
        RequireNativeGeneration(LGeneration);
        Continue;
      end;
      RequireNativeGeneration(LGeneration);

      Layout(LChild, AX, LChildY, LFitWidth, LFitHeight);
      RequireNativeGeneration(LGeneration);
      LInputHeight := Measure(LChild, LFitWidth);
      RequireNativeGeneration(LGeneration);

      if LFitHeight >= 0 then
      begin
        LInputHeight := LFitHeight;
        RequireNativeGeneration(LGeneration);
      end;

      if LInputHeight > LCellHeight then
      begin
        LCellHeight := LInputHeight;
        RequireNativeGeneration(LGeneration);
      end;
      { Grid tracks keep their cell extent even when a child has a smaller
        authored width. Row/column flows returned through their own plans above. }
      Inc(AX, LCellWidth + LGap);

      if (LPosition mod LColumns) = 0 then
      begin
        AX := LPadding;
        Inc(AY, LCellHeight + LGap);
        LCellHeight := 0;
        RequireNativeGeneration(LGeneration);
      end;
      RequireNativeGeneration(LGeneration);
    end;
    RequireNativeGeneration(LGeneration);
  end;
  RequireNativeGeneration(LGeneration);
end;

procedure TNyxLCLRenderer.Select(const ADesignID: TNyxText);
begin
  FEvents.Scheduler.RequireUI;
  FSelectedDesignID := ADesignID;

  if (FCanvasResizeGrips <> nil) and (FCanvasResizeGrips.Owner.ID <> ADesignID) then
  begin
    ClearCanvasResizeGrips;
  end;

  if (FCanvasMoveGrip <> nil) and (FCanvasMoveGrip.Owner.ID <> ADesignID) then
  begin
    ClearCanvasMoveGrip;
  end;
  FResizePreview := Default(TNyxResizePreview);
  { Painting selection is deliberately scroll-neutral. Navigation uses Reveal,
    so an observing refresh cannot undo the user's independent scroll position. }
  FDropPreview := Default(TNyxDropPreview);
  UpdateSelection;
end;

procedure TNyxLCLRenderer.PreviewDrop(const APreview: TNyxDropPreview);
begin
  FEvents.Scheduler.RequireUI;

  if APreview.Active and (not FDesignMode or
    not APreview.Frame.SameFrame(DropFrameFor(APreview.Origin.ID))) then
  begin
    raise ENyxModel.Create('Drop presentation requires current exact design geometry');
  end;
  FResizePreview := Default(TNyxResizePreview);
  FDropPreview := APreview;
  UpdateResizePreview;
end;

procedure TNyxLCLRenderer.PreviewResize(const APreview: TNyxResizePreview);
begin
  FEvents.Scheduler.RequireUI;

  if APreview.Active and (not FDesignMode or
    (APreview.Control.ID <> FSelectedDesignID)) then
  begin
    raise ENyxModel.Create('Resize presentation requires the selected authored face');
  end;

  if APreview.Active then
  begin
    { Resolve before replacing presentation, matching browser refusal semantics.
      A missing mounted identity cannot publish an unresolvable proposal. }
    IdentityBinding(APreview.Control.ID, niDesign);
  end;
  FResizePreview := APreview;
  FDropPreview := Default(TNyxDropPreview);
  UpdateSelection;
end;

procedure TNyxLCLRenderer.ClearCanvasResizeGrips;
var
  LAxis: TNyxResizeAxis;
  LGrips: INyxCanvasResizeGrips;
begin
  LGrips := FCanvasResizeGrips;
  FCanvasResizeGrips := nil;

  if LGrips <> nil then
  begin
    LGrips.Unbind;
  end;
  for LAxis := Low(TNyxResizeAxis) to High(TNyxResizeAxis) do
  begin
    FreeAndNil(FCanvasGripViews[LAxis]);
    FreeAndNil(FCanvasGripHosts[LAxis]);
  end;
  { The retained local interface keeps its borrowed document alive until every
    target view is gone, even when the renderer was its last external owner. }
  LGrips := nil;
end;

procedure TNyxLCLRenderer.AttachResizeGrips(const AGrips: INyxCanvasResizeGrips);
var
  LAxis: TNyxResizeAxis;
begin
  FEvents.Scheduler.RequireUI;

  if AGrips = nil then
  begin
    ClearCanvasResizeGrips;
    Exit;
  end;

  if not FDesignMode or (AGrips.Owner.ID <> FSelectedDesignID) then
  begin
    raise ENyxModel.Create('Canvas grips require the selected authored design face');
  end;
  IdentityBinding(AGrips.Owner.ID, niDesign);

  if FCanvasResizeGrips = AGrips then
  begin
    UpdateCanvasResizeGrips;
    Exit;
  end;
  ClearCanvasResizeGrips;
  FCanvasResizeGrips := AGrips;
  try
    for LAxis := Low(TNyxResizeAxis) to High(TNyxResizeAxis) do
    begin
      FCanvasGripHosts[LAxis] := TPanel.Create(nil);
      FCanvasGripHosts[LAxis].Name := 'NyxCanvasResizeHost' + IntToStr(Ord(LAxis));
      FCanvasGripHosts[LAxis].Caption := '';
      FCanvasGripHosts[LAxis].BevelOuter := bvNone;
      FCanvasGripHosts[LAxis].BorderWidth := 0;
      FCanvasGripHosts[LAxis].TabStop := False;
      FCanvasGripHosts[LAxis].SetBounds(0, 0, 44, 44);
      FCanvasGripHosts[LAxis].Parent := FPanel.Parent;
      FCanvasGripViews[LAxis] := TNyxLCLRenderer.Create(FTheme);
      FCanvasGripViews[LAxis].Render(AGrips.Document, AGrips.Root(LAxis), FCanvasGripHosts[LAxis]);
      AGrips.Bind(LAxis, FCanvasGripViews[LAxis].Events, CanvasResizePoint);
    end;
    UpdateCanvasResizeGrips;
  except
    ClearCanvasResizeGrips;
    raise;
  end;
end;

function TNyxLCLRenderer.CanvasResizeControl(AAxis: TNyxResizeAxis): TControl;
begin

  if (FCanvasResizeGrips = nil) or (FCanvasGripViews[AAxis] = nil) then
  begin
    raise ENyxModel.Create('Canvas resize adornment is not mounted');
  end;
  Result := FCanvasGripViews[AAxis].ControlFor(NyxCanvasResizeGripID(AAxis));
end;

function TNyxLCLRenderer.CanvasResizePoint(AAxis: TNyxResizeAxis;
  const APointer: TNyxPointerSnapshot): TNyxResizePoint;
var
  LOrigin: TPoint;
begin
  LOrigin := CanvasResizeControl(AAxis).ClientToScreen(Point(0, 0));
  Result := NyxResizePoint(LOrigin.X + APointer.X, LOrigin.Y + APointer.Y);
end;

procedure TNyxLCLRenderer.UpdateCanvasResizeGrips;
var
  LAxis: TNyxResizeAxis;
  LBinding: TNyxLCLBinding;
  LHost: TWinControl;
  LOrigin: TPoint;
  LSize: TNyxResizeSize;
  LX: Integer;
  LY: Integer;
  LVisible: Boolean;
begin
  UpdateCanvasMoveGrip;

  if (FCanvasResizeGrips = nil) or (FPanel = nil) or (FPanel.Parent = nil) then
  begin
    Exit;
  end;
  LHost := FPanel.Parent;
  LBinding := IdentityBinding(FCanvasResizeGrips.Owner.ID, niDesign);
  LOrigin := LHost.ScreenToClient(LBinding.FControl.Parent.ClientToScreen(
    Point(LBinding.FControl.Left, LBinding.FControl.Top)));

  if FVirtualLayout then
  begin
    Dec(LOrigin.X, LBinding.FPlacement.ContentOffsetX);
    Dec(LOrigin.Y, LBinding.FPlacement.ContentOffsetY);
  end;
  LSize := SizeFor(FCanvasResizeGrips.Owner.ID, niDesign);

  if FResizePreview.Active then
  begin
    LSize := FResizePreview.Size;
  end;
  for LAxis := Low(TNyxResizeAxis) to High(TNyxResizeAxis) do
  begin
    LX := LOrigin.X + LSize.Width;
    LY := LOrigin.Y + LSize.Height;

    if LAxis = nraWidth then
    begin
      LY := LOrigin.Y + LSize.Height div 2;
    end;

    if LAxis = nraHeight then
    begin
      LX := LOrigin.X + LSize.Width div 2;
    end;
    LVisible := LBinding.FControl.Visible and (LHost.ClientWidth >= 44) and
      (LHost.ClientHeight >= 44) and (LX >= 0) and (LX <= LHost.ClientWidth) and
      (LY >= 0) and (LY <= LHost.ClientHeight);

    if not FCanvasResizeGrips.Dragging(LAxis) then
    begin
      LVisible := LVisible and ((LAxis <> nraWidth) or (LSize.Height >= 88)) and
        ((LAxis <> nraHeight) or (LSize.Width >= 88));
    end;
    FCanvasGripHosts[LAxis].Visible := LVisible;

    if LVisible then
    begin
      FCanvasGripHosts[LAxis].SetBounds(Max(0, Min(LX - 22, LHost.ClientWidth - 44)),
        Max(0, Min(LY - 22, LHost.ClientHeight - 44)), 44, 44);
      FCanvasGripHosts[LAxis].BringToFront;
    end;
  end;
end;

procedure TNyxLCLRenderer.ClearCanvasMoveGrip;
var
  LGrip: INyxCanvasMoveGrip;
begin
  LGrip := FCanvasMoveGrip;
  FCanvasMoveGrip := nil;

  if LGrip <> nil then
  begin
    LGrip.Unbind;
  end;
  FreeAndNil(FCanvasMoveView);
  FreeAndNil(FCanvasMoveHost);
  LGrip := nil;
end;

procedure TNyxLCLRenderer.AttachMoveGrip(const AGrip: INyxCanvasMoveGrip);
begin
  FEvents.Scheduler.RequireUI;

  if AGrip = nil then
  begin
    ClearCanvasMoveGrip;
    Exit;
  end;

  if not FDesignMode or (AGrip.Owner.ID <> FSelectedDesignID) then
  begin
    raise ENyxModel.Create('Canvas move grip requires the selected authored design face');
  end;
  IdentityBinding(AGrip.Owner.ID, niDesign);

  if FCanvasMoveGrip = AGrip then
  begin
    UpdateCanvasMoveGrip;
    Exit;
  end;
  ClearCanvasMoveGrip;
  FCanvasMoveGrip := AGrip;
  try
    FCanvasMoveHost := TPanel.Create(nil);
    FCanvasMoveHost.Name := 'NyxCanvasMoveHost';
    FCanvasMoveHost.Caption := '';
    FCanvasMoveHost.BevelOuter := bvNone;
    FCanvasMoveHost.BorderWidth := 0;
    FCanvasMoveHost.TabStop := False;
    FCanvasMoveHost.SetBounds(0, 0, 44, 44);
    FCanvasMoveHost.Parent := FPanel.Parent;
    FCanvasMoveView := TNyxLCLRenderer.Create(FTheme);
    FCanvasMoveView.Render(AGrip.Document, AGrip.Root, FCanvasMoveHost);
    AGrip.Bind(FCanvasMoveView.Events, CanvasMovePoint);
    UpdateCanvasMoveGrip;
  except
    ClearCanvasMoveGrip;
    raise;
  end;
end;

function TNyxLCLRenderer.CanvasMovePoint(const APointer: TNyxPointerSnapshot): TNyxResizePoint;
begin

  if (FCanvasMoveGrip = nil) or (FCanvasMoveView = nil) then
  begin
    raise ENyxModel.Create('Canvas move adornment is not mounted');
  end;
  Result := LogicalPointFor(FCanvasMoveGrip.Owner.ID,
    FCanvasMoveView.ScreenPointFor(NyxCanvasMoveGripID, APointer), niDesign);
end;

function TNyxLCLRenderer.CanvasMoveControl: TControl;
begin

  if (FCanvasMoveGrip = nil) or (FCanvasMoveView = nil) then
  begin
    raise ENyxModel.Create('Canvas move adornment is not mounted');
  end;
  Result := FCanvasMoveView.ControlFor(NyxCanvasMoveGripID);
end;

procedure TNyxLCLRenderer.UpdateCanvasMoveGrip;
var
  LBinding: TNyxLCLBinding;
  LHost: TWinControl;
  LOrigin: TPoint;
begin

  if (FCanvasMoveGrip = nil) or (FCanvasMoveHost = nil) or
    (FPanel = nil) or (FPanel.Parent = nil) then
  begin
    Exit;
  end;
  LHost := FPanel.Parent;
  LBinding := IdentityBinding(FCanvasMoveGrip.Owner.ID, niDesign);
  LOrigin := LHost.ScreenToClient(LBinding.FControl.Parent.ClientToScreen(
    Point(LBinding.FControl.Left, LBinding.FControl.Top)));

  if FVirtualLayout then
  begin
    Dec(LOrigin.X, LBinding.FPlacement.ContentOffsetX);
    Dec(LOrigin.Y, LBinding.FPlacement.ContentOffsetY);
  end;

  if FResizePreview.Active and (FResizePreview.Control.ID = FCanvasMoveGrip.Owner.ID) then
  begin
    Inc(LOrigin.X, Round(FResizePreview.OffsetX));
    Inc(LOrigin.Y, Round(FResizePreview.OffsetY));
  end;
  FCanvasMoveHost.Visible := LBinding.FControl.Visible and (LHost.ClientWidth >= 44) and
    (LHost.ClientHeight >= 44) and (LOrigin.X >= 0) and (LOrigin.X <= LHost.ClientWidth) and
    (LOrigin.Y >= 0) and (LOrigin.Y <= LHost.ClientHeight);

  if FCanvasMoveHost.Visible then
  begin
    FCanvasMoveHost.SetBounds(Max(0, Min(LOrigin.X - 22, LHost.ClientWidth - 44)),
      Max(0, Min(LOrigin.Y - 22, LHost.ClientHeight - 44)), 44, 44);
    FCanvasMoveHost.BringToFront;
  end;
end;

procedure TNyxLCLRenderer.UpdateResizePreview;
var
  LBinding: TNyxLCLBinding;
  LHost: TWinControl;
  LOrigin: TPoint;
  LIndex: Integer;
  LBox: TNyxGuideBox;
  LLeft: Double;
  LTop: Double;
  LRight: Double;
  LBottom: Double;
  LDropFrame: TNyxDropFrame;

  procedure Edge(AIndex, ALeft, ATop, AWidth, AHeight: Integer);
  var
    LRight: Integer;
    LBottom: Integer;
  begin
    { Clip before assigning physical native geometry. Very large logical
      proposals must never request an equally large widget/window allocation. }
    LRight := Min(ALeft + AWidth, LHost.ClientWidth);
    LBottom := Min(ATop + AHeight, LHost.ClientHeight);
    ALeft := Max(0, ALeft);
    ATop := Max(0, ATop);
    FResizeEdges[AIndex].Visible := False;

    if (LRight > ALeft) and (LBottom > ATop) then
    begin
      FResizeEdges[AIndex].SetBounds(ALeft, ATop, LRight - ALeft, LBottom - ATop);
      FResizeEdges[AIndex].Visible := True;
      FResizeEdges[AIndex].BringToFront;
    end;
  end;

  procedure Guide(AIndex: Integer; const AGuide: TNyxAlignmentGuide; ASegment: Integer);
  var
    LBox: TNyxGuideBox;
    LLeft, LTop, LRight, LBottom: Double;
  begin
    FResizeEdges[AIndex].Visible := False;
    LBox := AGuide.Segment(ASegment, FResizePreview.Size.Width, FResizePreview.Size.Height);

    if LBox.Defined then
    begin
      { Logical peers may be far outside native message coordinates. Intersect
        in the wide copied domain before any integer/window conversion. }
      LLeft := LOrigin.X + LBox.Left - AGuide.OwnerBox.Left;
      LTop := LOrigin.Y + LBox.Top - AGuide.OwnerBox.Top;
      LRight := Min(LLeft + LBox.Width, LHost.ClientWidth);
      LBottom := Min(LTop + LBox.Height, LHost.ClientHeight);
      LLeft := Max(0.0, LLeft);
      LTop := Max(0.0, LTop);

      if (LRight > LLeft) and (LBottom > LTop) then
      begin
        Edge(AIndex, Round(LLeft), Round(LTop), Round(LRight) - Round(LLeft),
          Round(LBottom) - Round(LTop));
      end;
    end;
  end;

begin

  if (not FResizePreview.Active and not FDropPreview.Active) or
    (FPanel = nil) or (FPanel.Parent = nil) or
    (not FDropPreview.Active and
      ((FSelectionEdges[0] = nil) or not FSelectionEdges[0].Visible)) then
  begin
    for LIndex := Low(FResizeEdges) to High(FResizeEdges) do
    begin

      if FResizeEdges[LIndex] <> nil then
      begin
        FResizeEdges[LIndex].Visible := False;
      end;
    end;
    Exit;
  end;
  LHost := FPanel.Parent;

  if FDropPreview.Active then
  begin
    LBinding := IdentityBinding(FDropPreview.Origin.ID, niRuntime);
    LOrigin := LHost.ScreenToClient(Point(Round(FDropPreview.Frame.Face.Left),
      Round(FDropPreview.Frame.Face.Top)));
  end
  else
  begin
    LBinding := IdentityBinding(FResizePreview.Control.ID, niDesign);
    { Reuse the freshly laid-out selected outer face, including logical clipping.
      Undo its two-pixel external highlight before converting to sibling paint
      space. Root client edges already start at the original face's origin. }
    LOrigin := Point(FSelectionEdges[0].Left, FSelectionEdges[0].Top);

    if (LBinding.FControl.Parent <> FPanel) or not (LBinding.FControl is TWinControl) then
    begin
      Inc(LOrigin.X, 2);
      Inc(LOrigin.Y, 2);
    end;
    LOrigin := LHost.ScreenToClient(FSelectionEdges[0].Parent.ClientToScreen(LOrigin));
    { Copied logical translation changes only the proposal's bounded paint. }
    Inc(LOrigin.X, Round(FResizePreview.OffsetX));
    Inc(LOrigin.Y, Round(FResizePreview.OffsetY));
  end;
  for LIndex := Low(FResizeEdges) to High(FResizeEdges) do
  begin

    if FResizeEdges[LIndex] = nil then
    begin
      { Reuse standard LCL panels as bounded child-window paint faces above
        descendant windows. Graphic-only ancestor shapes would paint behind
        those windows or clip to the unchanged row. No custom widget is needed. }
      FResizeEdges[LIndex] := TPanel.Create(nil);
      FResizeEdges[LIndex].Name := 'NyxResizeEdge' + IntToStr(LIndex);
      { LCL can copy Name into Caption. Paint only the accent, never that text. }
      FResizeEdges[LIndex].Caption := '';
      FResizeEdges[LIndex].BevelOuter := bvNone;
      FResizeEdges[LIndex].BevelInner := bvNone;
      FResizeEdges[LIndex].BorderWidth := 0;
      FResizeEdges[LIndex].ParentBackground := False;
      FResizeEdges[LIndex].ParentColor := False;
      FResizeEdges[LIndex].Enabled := False;
      FResizeEdges[LIndex].TabStop := False;
    end;
    FResizeEdges[LIndex].Parent := LHost;
    FResizeEdges[LIndex].Color := ThemeColor(FTheme.Accent);
  end;

  if FDropPreview.Active then
  begin
    LDropFrame := DropFrameFor(FDropPreview.Origin.ID);
    for LIndex := Low(FResizeEdges) to High(FResizeEdges) do
    begin
      FResizeEdges[LIndex].Visible := False;
      LBox := FDropPreview.Segment(LIndex);

      if LBox.Defined and FDropPreview.Frame.SameFrame(LDropFrame) then
      begin
        { Clip in the wide copied domain before converting to native pixels. }
        LLeft := Max(0.0, LOrigin.X + LBox.Left);
        LTop := Max(0.0, LOrigin.Y + LBox.Top);
        LRight := Min(Double(LHost.ClientWidth), LOrigin.X + LBox.Right);
        LBottom := Min(Double(LHost.ClientHeight), LOrigin.Y + LBox.Bottom);

        if (LRight > LLeft) and (LBottom > LTop) then
        begin
          Edge(LIndex, Round(LLeft), Round(LTop), Round(LRight) - Round(LLeft),
            Round(LBottom) - Round(LTop));
        end;
      end;
    end;
    Exit;
  end;
  Edge(0, LOrigin.X - 2, LOrigin.Y - 2, FResizePreview.Size.Width + 4, 2);
  Edge(1, LOrigin.X - 2, LOrigin.Y + FResizePreview.Size.Height,
    FResizePreview.Size.Width + 4, 2);
  Edge(2, LOrigin.X - 2, LOrigin.Y, 2, FResizePreview.Size.Height);
  Edge(3, LOrigin.X + FResizePreview.Size.Width, LOrigin.Y, 2, FResizePreview.Size.Height);
  Guide(4, FResizePreview.Size.WidthGuide, 0);
  Guide(5, FResizePreview.Size.WidthGuide, 1);
  Guide(6, FResizePreview.Size.HeightGuide, 0);
  Guide(7, FResizePreview.Size.HeightGuide, 1);
end;

procedure TNyxLCLRenderer.PaintResizePreview(ACanvas: TCanvas; const AOrigin: TPoint);
var
  LIndex: Integer;
  LPoint: TPoint;
  LAxis: TNyxResizeAxis;
begin
  FEvents.Scheduler.RequireUI;

  if ACanvas = nil then
  begin
    raise ENyxModel.Create('Native proposal capture requires a caller-owned canvas');
  end;

  for LIndex := Low(FResizeEdges) to High(FResizeEdges) do
  begin

    if (FResizeEdges[LIndex] <> nil) and FResizeEdges[LIndex].Visible then
    begin
      LPoint := FResizeEdges[LIndex].ClientToScreen(Point(0, 0));
      FResizeEdges[LIndex].PaintTo(ACanvas, LPoint.X - AOrigin.X, LPoint.Y - AOrigin.Y);
    end;
  end;
  for LAxis := Low(TNyxResizeAxis) to High(TNyxResizeAxis) do
  begin

    if (FCanvasGripHosts[LAxis] <> nil) and FCanvasGripHosts[LAxis].Visible then
    begin
      LPoint := FCanvasGripHosts[LAxis].ClientToScreen(Point(0, 0));
      FCanvasGripHosts[LAxis].PaintTo(ACanvas, LPoint.X - AOrigin.X, LPoint.Y - AOrigin.Y);
    end;
  end;
end;

procedure TNyxLCLRenderer.UpdateSelection;
var
  LGeneration: INyxLCLBindingGeneration;
  LControl: TControl;
  LIndex: Integer;
  LHost: TWinControl;
  LBounds: TRect;
  LSelected: TNyxLCLBinding;
begin
  LGeneration := FGeneration;

  if not NativeGenerationCurrent(LGeneration) then
  begin
    Exit;
  end;

  if FPanel = nil then
  begin
    Exit;
  end;
  LControl := nil;
  RequireNativeGeneration(LGeneration);
  LSelected := nil;
  RequireNativeGeneration(LGeneration);

  if FDesignMode and (FSelectedDesignID <> '') then
  begin
    for LIndex := 0 to High(FBindings) do
    begin

      if (FBindings[LIndex].FNode.DesignID = FSelectedDesignID) and
        FBindings[LIndex].FControl.Visible and
        NyxInteractionVisible(FBindings[LIndex].FNode) then
      begin
        RequireNativeGeneration(LGeneration);
        { Realization visits parents first. A reusable instance's descendants
          share its authored identity; outline that instance's outer face once. }
        LSelected := FBindings[LIndex];

        if not FVirtualLayout or LSelected.FPlacement.HasArea then
        begin
          LControl := LSelected.FControl;
          RequireNativeGeneration(LGeneration);
        end;
        Break;
      end;
      RequireNativeGeneration(LGeneration);
    end;
    RequireNativeGeneration(LGeneration);
  end;
  RequireNativeGeneration(LGeneration);
  LHost := FPanel;
  LBounds := Rect(0, 0, 0, 0);
  RequireNativeGeneration(LGeneration);

  if LControl <> nil then
  begin

    if (LControl.Parent = FPanel) and (LControl is TWinControl) then
    begin
      RequireNativeGeneration(LGeneration);
      { Root edges stay inside its client rectangle. They neither extend the
        scroll range nor sit behind the full-page child window. }
      LHost := TWinControl(LControl);
      RequireNativeGeneration(LGeneration);
      LBounds := LHost.ClientRect;
      RequireNativeGeneration(LGeneration);

      if FVirtualLayout then
      begin
        LBounds := Rect(-LSelected.FPlacement.ContentOffsetX,
          -LSelected.FPlacement.ContentOffsetY,
          LSelected.FLogicalBox.Width - LSelected.FPlacement.ContentOffsetX,
          LSelected.FLogicalBox.Height - LSelected.FPlacement.ContentOffsetY);
          RequireNativeGeneration(LGeneration);
      end;
    end
    else
    begin
      RequireNativeGeneration(LGeneration);
      LHost := LControl.Parent;
      RequireNativeGeneration(LGeneration);
      LBounds := LControl.BoundsRect;
      RequireNativeGeneration(LGeneration);

      if FVirtualLayout then
      begin
        OffsetRect(LBounds, -LSelected.FPlacement.ContentOffsetX,
          -LSelected.FPlacement.ContentOffsetY);
          RequireNativeGeneration(LGeneration);
        LBounds.Right := LBounds.Left + LSelected.FLogicalBox.Width;
        RequireNativeGeneration(LGeneration);
        LBounds.Bottom := LBounds.Top + LSelected.FLogicalBox.Height;
        RequireNativeGeneration(LGeneration);
      end;
      InflateRect(LBounds, 2, 2);
      RequireNativeGeneration(LGeneration);
    end;
    RequireNativeGeneration(LGeneration);
  end;
  for LIndex := Low(FSelectionEdges) to High(FSelectionEdges) do
  begin

    if (LControl <> nil) and (FSelectionEdges[LIndex] = nil) then
    begin
      FSelectionEdges[LIndex] := TShape.Create(FPanel);
      RequireNativeGeneration(LGeneration);
      FSelectionEdges[LIndex].Shape := stRectangle;
      FSelectionEdges[LIndex].Pen.Style := psSolid;
    end;
    RequireNativeGeneration(LGeneration);

    if FSelectionEdges[LIndex] <> nil then
    begin
      FSelectionEdges[LIndex].Parent := LHost;
      { TShape's rectangle excludes its final fill row/column. A matching solid
        pen covers that perimeter too, preserving the complete two-pixel strip. }
      FSelectionEdges[LIndex].Pen.Color := ThemeColor(FTheme.Accent);
      RequireNativeGeneration(LGeneration);
      FSelectionEdges[LIndex].Brush.Color := ThemeColor(FTheme.Accent);
      RequireNativeGeneration(LGeneration);
      FSelectionEdges[LIndex].Visible := LControl <> nil;
    end;
  end;
  RequireNativeGeneration(LGeneration);

  if LControl = nil then
  begin
    UpdateResizePreview;
    RequireNativeGeneration(LGeneration);
    UpdateCanvasResizeGrips;
    RequireNativeGeneration(LGeneration);
    Exit;
  end;
  { The scroll panel owns all four graphics even when their paint parent is a
    nested container. Outside edges leave inputs unobstructed; root edges use
    the page's padded inner perimeter. Neither kind creates a focus entry. }
  FSelectionEdges[0].SetBounds(LBounds.Left, LBounds.Top, LBounds.Right - LBounds.Left, 2);
  RequireNativeGeneration(LGeneration);
  FSelectionEdges[1].SetBounds(LBounds.Left, LBounds.Bottom - 2, LBounds.Right - LBounds.Left, 2);
  RequireNativeGeneration(LGeneration);
  FSelectionEdges[2].SetBounds(LBounds.Left, LBounds.Top + 2, 2,
    Max(0, LBounds.Bottom - LBounds.Top - 4));
    RequireNativeGeneration(LGeneration);
  FSelectionEdges[3].SetBounds(LBounds.Right - 2, LBounds.Top + 2, 2,
    Max(0, LBounds.Bottom - LBounds.Top - 4));
    RequireNativeGeneration(LGeneration);
  for LIndex := Low(FSelectionEdges) to High(FSelectionEdges) do
  begin
    FSelectionEdges[LIndex].BringToFront;
    RequireNativeGeneration(LGeneration);
  end;
  RequireNativeGeneration(LGeneration);
  UpdateResizePreview;
  RequireNativeGeneration(LGeneration);
  UpdateCanvasResizeGrips;
  RequireNativeGeneration(LGeneration);
end;


procedure TNyxLCLRenderer.SetPresentationPlaneVisible(APlane: TWinControl;
  AVisible: Boolean);
begin
  ChangePresentationPlane(APlane, AVisible, nil,
    Default(TNyxTextSelection), False);
end;

procedure TNyxLCLRenderer.RestorePresentationPlane(APlane: TWinControl;
  AFocus: TWinControl;
  const ASelection: TNyxTextSelection);
begin
  ChangePresentationPlane(APlane, True, AFocus, ASelection, True);
end;

procedure TNyxLCLRenderer.ChangePresentationPlane(APlane: TWinControl;
  AVisible: Boolean; AFocus: TWinControl;
  const ASelection: TNyxTextSelection; ASynchronize: Boolean);
var
  LGeneration: INyxLCLBindingGeneration;
  LFrame: INyxNativeGestureFrame;
  LGrids: array of TNyxCollectionStringGrid;
  LGrid: TNyxCollectionStringGrid;
  LIndex: Integer;
  LChild: Integer;
  LEntered: Integer;
  LUpdating: Boolean;
  LLayouting: Boolean;
  LParent: TWinControl;
  LCanFocus: Boolean;

  procedure IncludeGrid(AGrid: TNyxCollectionStringGrid);
  var
    LPrevious: Integer;
  begin
    for LPrevious := 0 to High(LGrids) do
    begin

      if LGrids[LPrevious] = AGrid then
      begin
        Exit;
      end;
    end;
    SetLength(LGrids, Length(LGrids) + 1);
    LGrids[High(LGrids)] := AGrid;
  end;
begin
  LGeneration := FGeneration;
  RequireNativeGeneration(LGeneration);
  FEvents.Scheduler.RequireUI;
  RequireNativeGeneration(LGeneration);

  if (FPanel = nil) or (FRoot = nil) or
    (APlane = nil) or (FPanel.Parent <> APlane) then
  begin
    raise ENyxModel.Create('Native visibility requires this view''s exact presentation plane');
  end;
  LParent := AFocus;
  while (LParent <> nil) and (LParent <> FPanel) do
  begin
    LParent := LParent.Parent;
  end;

  if (AFocus <> nil) and (LParent <> FPanel) then
  begin
    raise ENyxModel.Create('Native presentation focus must belong to this exact view');
  end;
  LFrame := FPhysicalFrame;
  LFrame.Enter;
  try
    RequireNativeGeneration(LGeneration);
    LGrids := nil;
    for LIndex := 0 to High(FBindings) do
    begin

      if FBindings[LIndex].FControl is TNyxCollectionStringGrid then
      begin
        LGrid := TNyxCollectionStringGrid(FBindings[LIndex].FControl);
        IncludeGrid(LGrid);
        for LChild := 0 to LGrid.ControlCount - 1 do
        begin

          if LGrid.Controls[LChild] is TNyxCollectionStringGrid then
          begin
            IncludeGrid(TNyxCollectionStringGrid(LGrid.Controls[LChild]));
          end;
        end;
      end
      else if (FBindings[LIndex].FCollectionMount <> nil) and
        (FBindings[LIndex].FControl is TStringGrid) then
      begin
        raise ENyxModel.Create('Native presentation requires a guarded collection grid');
      end;
    end;
    LUpdating := FUpdating;
    LLayouting := FLayouting;
    LEntered := 0;
    FUpdating := True;
    FLayouting := True;
    try
      for LIndex := 0 to High(LGrids) do
      begin
        LGrids[LIndex].BeginPresentationChange;
        Inc(LEntered);
      end;
      APlane.Visible := AVisible;
      RequireNativeGeneration(LGeneration);

      if ASynchronize then
      begin
        { Sync must perform its ordinary updater and allocation checks rather
          than exit through an already-updating shield. Its own renderer guard
          applies during every callout; the captured grid guards/frame remain
          entered throughout. No native call occurs between the flag changes. }
        FUpdating := LUpdating;
        FLayouting := LLayouting;
        Sync;
        RequireNativeGeneration(LGeneration);
        FUpdating := True;
        FLayouting := True;
      end;

      LCanFocus := False;

      if AFocus <> nil then
      begin
        LCanFocus := AFocus.CanFocus;
        RequireNativeGeneration(LGeneration);
      end;

      if LCanFocus then
      begin
        AFocus.SetFocus;
        RequireNativeGeneration(LGeneration);

        if ASelection.Defined then
        begin
          SelectNyxLCLText(AFocus, ASelection);
          RequireNativeGeneration(LGeneration);
        end;
      end;
      RequireNativeGeneration(LGeneration);
    finally
      { The entered original frame retains its native controls through callback
        retirement. Restore only current receiver flags; unwind borrowed grids
        from the captured vector, then leave that frame LAST without Self reads. }
      while LEntered <> 0 do
      begin
        Dec(LEntered);
        LGrids[LEntered].EndPresentationChange;
      end;

      if NativeGenerationCurrent(LGeneration) then
      begin
        FUpdating := LUpdating;
        FLayouting := LLayouting;
      end;
    end;
  finally
    LFrame.Leave;
  end;
end;

procedure TNyxLCLRenderer.MoveHost(AHost: TWinControl);
var
  LParent: TControl;
  LFocus: TWinControl;
  LSelection: TNyxTextSelection;
  LInside: Boolean;
  LHorizontal: Integer;
  LVertical: Integer;
  LUpdating: Boolean;
  LLayouting: Boolean;
  LIndex: Integer;
begin
  FEvents.Scheduler.RequireUI;

  if (AHost = nil) or (FPanel = nil) or (FRoot = nil) then
  begin
    raise ENyxModel.Create('Moving a native view requires a mounted view and host');
  end;
  LParent := AHost;
  while LParent <> nil do
  begin

    if LParent = FPanel then
    begin
      raise ENyxModel.Create('A native view cannot move into its own descendant');
    end;
    LParent := LParent.Parent;
  end;

  if FPanel.Parent = AHost then
  begin
    Exit;
  end;

  if AHost.ControlCount <> 0 then
  begin
    raise ENyxModel.Create('Replacement native host must be empty');
  end;
  LFocus := Screen.ActiveControl;
  LParent := LFocus;
  LInside := False;
  while LParent <> nil do
  begin

    if LParent = FPanel then
    begin
      LInside := True;
      Break;
    end;
    LParent := LParent.Parent;
  end;
  LSelection := Default(TNyxTextSelection);

  if LInside then
  begin
    LSelection := CaptureNyxLCLSelection(LFocus);
  end;
  LHorizontal := FPanel.HorzScrollBar.Position;
  LVertical := FPanel.VertScrollBar.Position;
  LUpdating := FUpdating;
  FUpdating := True;
  try
    { Reparenting can synchronously invoke OnResize before all native siblings
      have reached their final host. FUpdating guards authored input, while
      FLayouting guards geometry. Retain its prior value even when a host refuses
      insertion; the explicit Resize below still owns the completed move. }
    LLayouting := FLayouting;
    FLayouting := True;
    try
      { Reparent the scroll host, never recreate children or event routers. }
      FPanel.Parent := AHost;
      { Inert proposal windows are siblings of the scroll host. Move even hidden
        strips before the caller disposes the previous host; their borrowed
        parent must never free windows still owned by this renderer. }
      for LIndex := Low(FResizeEdges) to High(FResizeEdges) do
      begin

        if FResizeEdges[LIndex] <> nil then
        begin
          FResizeEdges[LIndex].Parent := AHost;
        end;
      end;
      for LIndex := Ord(Low(TNyxResizeAxis)) to Ord(High(TNyxResizeAxis)) do
      begin

        if FCanvasGripHosts[TNyxResizeAxis(LIndex)] <> nil then
        begin
          FCanvasGripHosts[TNyxResizeAxis(LIndex)].Parent := AHost;
        end;
      end;
    finally
      FLayouting := LLayouting;
    end;
    Resize(FPanel);

    if LInside and LFocus.CanFocus then
    begin
      LFocus.SetFocus;

      if LSelection.Defined then
      begin
        SelectNyxLCLText(LFocus, LSelection);
      end;
    end;
    FPanel.HorzScrollBar.Position := LHorizontal;
    FPanel.VertScrollBar.Position := LVertical;
    UpdateSelection;
  finally
    FUpdating := LUpdating;
  end;
end;

{$include nyx.render.lcl.viewport.inc}

function TNyxLCLRenderer.MeasureContainers: INyxContainerSnapshot;
var
  LGeneration: INyxLCLBindingGeneration;
  LMeasurements: TNyxContainerMeasurements;
  LIndex, LCount, LPadding: Integer;
  LAncestor: TNyxNode;
  LVisible: Boolean;
  LBinding: TNyxLCLBinding;
begin
  LGeneration := FGeneration;
  RequireNativeGeneration(LGeneration);

  LMeasurements := nil;
  RequireNativeGeneration(LGeneration);
  for LIndex := 0 to High(FBindings) do
  begin
    LBinding := FBindings[LIndex];
    RequireNativeGeneration(LGeneration);

    if not LBinding.FNode.QueryContainer.Defined or not LBinding.FContainerAllocated then
    begin
      RequireNativeGeneration(LGeneration);
      Continue;
    end;
    RequireNativeGeneration(LGeneration);
    LAncestor := LBinding.FNode;
    LVisible := True;
    RequireNativeGeneration(LGeneration);
    while LAncestor <> nil do
    begin
      LVisible := LVisible and (LAncestor.Prop('visible', 'true') <> 'false');
      RequireNativeGeneration(LGeneration);
      LAncestor := LAncestor.Parent;
      RequireNativeGeneration(LGeneration);
    end;

    if not LVisible then
    begin
      Continue;
    end;
    LCount := Length(LMeasurements);
    RequireNativeGeneration(LGeneration);
    SetLength(LMeasurements, LCount + 1);
    RequireNativeGeneration(LGeneration);
    LPadding := 2 * Metric(LBinding.FNode, 'padding', 0);
    RequireNativeGeneration(LGeneration);
    LMeasurements[LCount].RuntimeID := LBinding.FNode.ID;
    RequireNativeGeneration(LGeneration);
    { Logical allocation survives native virtualization. Only non-client
      thickness comes from the control; never publish its clipped screen size. }
    LMeasurements[LCount].Width := Max(0, LBinding.FLogicalBox.Width - LPadding -
      (LBinding.FControl.Width - LBinding.FControl.ClientWidth));
      RequireNativeGeneration(LGeneration);
    LMeasurements[LCount].Height := Max(0, LBinding.FLogicalBox.Height - LPadding -
      (LBinding.FControl.Height - LBinding.FControl.ClientHeight));
      RequireNativeGeneration(LGeneration);
  end;
  RequireNativeGeneration(LGeneration);
  Result := NewNyxContainerSnapshot(LMeasurements);
  RequireNativeGeneration(LGeneration);
end;

function TNyxLCLRenderer.UpdateViewport: Boolean;
var
  LGeneration: INyxLCLBindingGeneration;
  LMeasurements: INyxContainerSnapshot;
begin
  LGeneration := FGeneration;
  RequireNativeGeneration(LGeneration);

  Result := False;

  if (FRoot = nil) or (FPanel = nil) or (FPanel.Parent = nil) then
  begin
    RequireNativeGeneration(LGeneration);
    Exit;
  end;
  RequireNativeGeneration(LGeneration);
  LMeasurements := FStagingMeasurements;
  RequireNativeGeneration(LGeneration);

  if not FStagingContent then
  begin
    LMeasurements := MeasureContainers;
    RequireNativeGeneration(LGeneration);
  end;
  Result := FRoot.ApplyViewport(Max(0, FPanel.Parent.ClientWidth),
    Max(0, FPanel.Parent.ClientHeight), npfNativeLCL, FPresentationSelection, LMeasurements);
    RequireNativeGeneration(LGeneration);
end;

procedure TNyxLCLRenderer.Resize(ASender: TObject);
var
  LGeneration: INyxLCLBindingGeneration;
  LPhysicalFrame: INyxNativeGestureFrame;
  LHeight: Integer;
  LPanel: TNyxLogicalScrollBox;
  LIndex: Integer;
  LPass: Integer;
  LWidth: Integer;
  LClientHeight: Integer;
  LChild: Integer;
  LContentHeight: Double;
  {$ifdef NYX_LCL_LAYOUT_PROFILE}LMeasureStarted: QWord;{$endif}
begin
  LGeneration := FGeneration;

  if (LGeneration = nil) or not NativeGenerationCurrent(LGeneration) then
  begin
    Exit;
  end;
  LPhysicalFrame := FPhysicalFrame;
  LPhysicalFrame.Enter;
  try
    try

      if FLayouting or FProjecting then
      begin
        Exit;
      end;

      if (FRoot <> nil) and (FPanel <> nil) then
      begin

        if UpdateViewport and not FUpdating then
        begin
          { Sync applies visibility/interaction as well as geometry, under its
            existing event guard. Its nested Resize sees the already copied overlay. }
          Sync;
          RequireNativeGeneration(LGeneration);
          Exit;
        end;
        { LCL's automatic anchor/layout pass must see the complete new geometry.
          Otherwise showing a previously unplaced label can reapply its stale base
          position in the middle of SetBounds. Keep widgetset layout atomic while
          retaining the same controls and their focus/editing state. }
        ClearLabelMeasurements;
        RequireNativeGeneration(LGeneration);
        LPanel := FPanel;
        LPanel.DisableAutoSizing;
        RequireNativeGeneration(LGeneration);
        FLayouting := True;
        {$ifdef NYX_LCL_LAYOUT_PROFILE}
        FMeasureCalls := 0;
        FNaturalCalls := 0;
        FRowPlanCalls := 0;
        FLabelMeasurementsTaken := 0;
        FLabelMeasurementsReused := 0;
        LMeasureStarted := GetTickCount64;
        {$endif}
        try
          { Design canvases always retain their logical frame. Runtime views switch
            when any face/position could exceed the native message coordinate domain,
            including a large child inside an explicitly bounded scroll container. }
          FVirtualLayout := FDesignMode;
          for LIndex := 0 to High(FBindings) do
          begin
            FBindings[LIndex].FLogicalBox := Default(TNyxViewportBox);
            RequireNativeGeneration(LGeneration);

            if not FVirtualLayout then
            begin
              FVirtualLayout := (Measure(FBindings[LIndex].FNode, FPanel.ClientWidth, True) >
                High(SmallInt)) or
                (Abs(Double(Metric(FBindings[LIndex].FNode, 'top', 0))) > High(SmallInt)) or
                (Abs(Double(Metric(FBindings[LIndex].FNode, 'left', 0))) > High(SmallInt));
                RequireNativeGeneration(LGeneration);

              if not FVirtualLayout and (NyxLayout(FBindings[LIndex].FNode) = 'column') and
                (FBindings[LIndex].FNode.Prop('height') <> '') then
              begin
                RequireNativeGeneration(LGeneration);
                { An explicit bounded scroll/column height describes its viewport,
                  not its overflowing child extent. Detect that content before any
                  native window receives a distant child position. Small ordinary
                  scroll views keep their existing automatic LCL implementation.
                  Automatic columns were already measured completely above; do not
                  traverse those descendants a second time for each binding. }
                LContentHeight := 2.0 * Metric(FBindings[LIndex].FNode, 'padding', 0);
                RequireNativeGeneration(LGeneration);
                for LChild := 0 to FBindings[LIndex].FNode.Count - 1 do
                begin
                  RequireNativeGeneration(LGeneration);

                  if NyxInteractionVisible(FBindings[LIndex].FNode.Children[LChild]) then
                  begin
                    RequireNativeGeneration(LGeneration);
                    LContentHeight := LContentHeight + Measure(
                      FBindings[LIndex].FNode.Children[LChild], FPanel.ClientWidth, True) +
                      Metric(FBindings[LIndex].FNode, 'gap', 12);
                      RequireNativeGeneration(LGeneration);

                    if LContentHeight > High(SmallInt) then
                    begin
                      FVirtualLayout := True;
                      Break;
                    end;
                  end;
                  RequireNativeGeneration(LGeneration);
                end;
                RequireNativeGeneration(LGeneration);
              end;
              RequireNativeGeneration(LGeneration);
            end;
          end;
          RequireNativeGeneration(LGeneration);

          if FVirtualLayout then
          begin
            { Showing either native scrollbar changes the available client area.
              Settle both axes before publishing physical boxes, without recursive
              layout or sizing the root from the previous viewport width. }
            for LPass := 0 to 2 do
            begin
              LWidth := FPanel.ViewportWidth;
              RequireNativeGeneration(LGeneration);
              LClientHeight := FPanel.ViewportHeight;
              RequireNativeGeneration(LGeneration);
              LHeight := -1;
              RequireNativeGeneration(LGeneration);

              if FRoot.Prop('height-sizing') = 'fill' then
              begin
                RequireNativeGeneration(LGeneration);
                LHeight := LClientHeight;
                RequireNativeGeneration(LGeneration);
              end;
              RequireNativeGeneration(LGeneration);
              Layout(FRoot, 0, 0, LWidth, LHeight);
              RequireNativeGeneration(LGeneration);
              FPanel.SetLogicalExtent(Binding(FRoot).FLogicalBox.Width,
                Binding(FRoot).FLogicalBox.Height, ProjectViewport);
                RequireNativeGeneration(LGeneration);

              if (LWidth = FPanel.ViewportWidth) and
                (LClientHeight = FPanel.ViewportHeight) then
              begin
                RequireNativeGeneration(LGeneration);
                Break;
              end;
              RequireNativeGeneration(LGeneration);
            end;
            ProjectViewport(FPanel);
            RequireNativeGeneration(LGeneration);
          end
          else
          begin
            FPanel.UseNativeScrolling;
            RequireNativeGeneration(LGeneration);
            LHeight := -1;
            RequireNativeGeneration(LGeneration);

            if FRoot.Prop('height-sizing') = 'fill' then
            begin
              RequireNativeGeneration(LGeneration);
              LHeight := Max(0, FPanel.ClientHeight);
              RequireNativeGeneration(LGeneration);
            end;
            RequireNativeGeneration(LGeneration);
            Layout(FRoot, 0, 0, Max(0, FPanel.ClientWidth), LHeight);
            RequireNativeGeneration(LGeneration);
            for LIndex := 0 to High(FBindings) do
            begin

              if FBindings[LIndex].FControl is TNyxLCLSurface then
              begin
                TNyxLCLSurface(FBindings[LIndex].FControl).ProjectFace(Default(TNyxViewportBox));
                RequireNativeGeneration(LGeneration);
              end;
            end;
            RequireNativeGeneration(LGeneration);
          end;
        finally
          { Retirement also runs after a label, custom measurement or allocation
            refusal. Release before permitting another native auto-sizing pass. }

          if NativeGenerationCurrent(LGeneration) and (FMeasurementContext <> 0) then
          begin
            LCLIntf.ReleaseDC(0, FMeasurementContext);
            FMeasurementContext := 0;
          end;

          if NativeGenerationCurrent(LGeneration) then
          begin
            ClearLabelMeasurements;
            FLayouting := False;
          end;
          {$ifdef NYX_LCL_LAYOUT_PROFILE}

          if NativeGenerationCurrent(LGeneration) then
          begin
            WriteLn('PROFILE native layout: nodes=', Length(FBindings),
            ' height=', FMeasureCalls, ' width=', FNaturalCalls,
            ' rows=', FRowPlanCalls, ' label-measurements=', FLabelMeasurementsTaken,
            ' label-reuse=', FLabelMeasurementsReused,
            ' milliseconds=', GetTickCount64 - LMeasureStarted);
          end;
          {$endif}
          LPanel.EnableAutoSizing;
        end;
        RequireNativeGeneration(LGeneration);
        UpdateSelection;
        RequireNativeGeneration(LGeneration);

        if not FUpdating and UpdateViewport then
        begin
          Sync;
          RequireNativeGeneration(LGeneration);
        end;
      end;
      RequireNativeGeneration(LGeneration);
      InitializeCanvases;
      RequireNativeGeneration(LGeneration);
      QueueContent;
      RequireNativeGeneration(LGeneration);
    except
      on ENyxNativeGenerationRetired do
      begin
        { Obsolete receiver: committed data stays intact; no successor tail runs. }
      end;
    end;
  finally
    { LAST: retain the exact original native frame across every callback tail. }
    LPhysicalFrame.Leave;
  end;
end;

procedure TNyxLCLRenderer.Render(ADocument: TNyxDocument; ARoot: TNyxNode;
  AHost: TWinControl; AState: TNyxState; const ACollections: INyxCollectionBindings);
begin
  Render(ADocument, ARoot, AHost, False, AState, ACollections);
end;

function TNyxLCLRenderer.CaptureContentFaces: TNyxContentFaceStates;
var
  LIndex: Integer;
  LBinding: TNyxLCLBinding;
begin
  Result := nil;
  SetLength(Result, Length(FBindings));
  for LIndex := 0 to High(FBindings) do
  begin
    LBinding := FBindings[LIndex];
    Result[LIndex] := CaptureNyxContentFace(LBinding.FNode);
    Result[LIndex].Focused := (Screen.ActiveControl = LBinding.FInput) or
      (Screen.ActiveControl = LBinding.FControl);

    if LBinding.FControl is TScrollBox then
    begin
      Result[LIndex].ScrollLeft := TScrollBox(LBinding.FControl).HorzScrollBar.Position;
      Result[LIndex].ScrollTop := TScrollBox(LBinding.FControl).VertScrollBar.Position;
    end;

    if LBinding.FInput is TWinControl then
    begin
      Result[LIndex].Selection := CaptureNyxLCLSelection(TWinControl(LBinding.FInput));
      Result[LIndex].HasText := LBinding.FInput is TCustomEdit;

      if Result[LIndex].HasText then
      begin
        Result[LIndex].Text := NyxLCLInputText(TWinControl(LBinding.FInput));
      end;
    end;
  end;
end;

procedure TNyxLCLRenderer.RestoreContentFaces(
  const AStates: TNyxContentFaceStates; AFocus: Boolean);
var
  LGeneration: INyxLCLBindingGeneration;
  LPhysicalFrame: INyxNativeGestureFrame;
  LIndex: Integer;
  LSaved: Integer;
  LBinding: TNyxLCLBinding;
  LFocus: TWinControl;
  LUpdating: Boolean;
begin
  LGeneration := FGeneration;

  if (LGeneration = nil) or not NativeGenerationCurrent(LGeneration) then
  begin
    Exit;
  end;
  LPhysicalFrame := FPhysicalFrame;
  LPhysicalFrame.Enter;
  try
    LUpdating := FUpdating;
    FUpdating := True;
    try
      for LIndex := 0 to High(FBindings) do
      begin
        RequireNativeGeneration(LGeneration);
        LBinding := FBindings[LIndex];
        LSaved := NyxContentFaceIndex(LBinding.FNode, AStates);
        RequireNativeGeneration(LGeneration);

        if LSaved < 0 then
        begin
          Continue;
        end;

        if AStates[LSaved].HasText and (LBinding.FInput is TCustomEdit) and
          (LBinding.FNode.Prop('value') = AStates[LSaved].Accepted) then
        begin
          RequireNativeGeneration(LGeneration);
          TCustomEdit(LBinding.FInput).Text := AStates[LSaved].Text;
          RequireNativeGeneration(LGeneration);
          LBinding.FLastValue := AStates[LSaved].Accepted;
          RequireNativeGeneration(LGeneration);
          LBinding.FHasValueBaseline := True;
        end;
        RequireNativeGeneration(LGeneration);
        LFocus := nil;
        RequireNativeGeneration(LGeneration);

        if LBinding.FInput is TWinControl then
        begin
          LFocus := TWinControl(LBinding.FInput);
          RequireNativeGeneration(LGeneration);
        end
        else if LBinding.FControl is TWinControl then
        begin
          LFocus := TWinControl(LBinding.FControl);
          RequireNativeGeneration(LGeneration);
        end;

        if LFocus <> nil then
        begin

          if AFocus and AStates[LSaved].Focused and LFocus.CanFocus then
          begin
            RequireNativeGeneration(LGeneration);
            LFocus.SetFocus;
            RequireNativeGeneration(LGeneration);
          end;
          RequireNativeGeneration(LGeneration);

          if AStates[LSaved].Selection.Defined and (LFocus is TCustomEdit) and
            (TNyxText(TCustomEdit(LFocus).Text) = AStates[LSaved].Text) then
          begin
            RequireNativeGeneration(LGeneration);
            LFocus.HandleNeeded;
            RequireNativeGeneration(LGeneration);
            SelectNyxLCLText(LFocus, AStates[LSaved].Selection);
            RequireNativeGeneration(LGeneration);
          end;
          RequireNativeGeneration(LGeneration);
        end;

        if LBinding.FControl is TScrollBox then
        begin
          TScrollBox(LBinding.FControl).HorzScrollBar.Position := Round(AStates[LSaved].ScrollLeft);
          RequireNativeGeneration(LGeneration);
          TScrollBox(LBinding.FControl).VertScrollBar.Position := Round(AStates[LSaved].ScrollTop);
          RequireNativeGeneration(LGeneration);
        end;
      end;
      RequireNativeGeneration(LGeneration);
    finally

      if NativeGenerationCurrent(LGeneration) then
      begin
        { Retained generation/frame leases may outlive this receiver. Restore the
          guard only while this exact view still owns its physical continuation. }
        FUpdating := LUpdating;
      end;
    end;
    RequireNativeGeneration(LGeneration);
  finally
    LPhysicalFrame.Leave;
  end;
end;


function TNyxLCLRenderer.CaptureInteraction(
  out AStates: TNyxContentFaceStates): Boolean;
begin
  AStates := nil;
  Result := SectionPublicationReady;

  if Result then
  begin
    AStates := CaptureContentFaces;
  end;
end;

function TNyxLCLRenderer.RestoreInteraction(
  const AStates: TNyxContentFaceStates; AFocus: Boolean): Boolean;
var
  LIndex: Integer;
begin
  Result := SectionPublicationReady;

  if not Result then
  begin
    Exit;
  end;
  { Refuse ambiguous copied identities before touching any physical face. }
  for LIndex := 0 to High(FBindings) do
  begin
    NyxContentFaceIndex(FBindings[LIndex].FNode, AStates);
  end;
  RestoreContentFaces(AStates, AFocus);
end;

procedure TNyxLCLRenderer.QueueContent;
var
  LGeneration: INyxLCLBindingGeneration;
  LEvents: INyxEvents;
  LScheduler: INyxScheduler;
  LExecution: INyxExecution;
  LWork: INyxContentWork;
  LStatus: TNyxExecutionStatus;
begin
  LGeneration := FGeneration;

  if (LGeneration = nil) or not LGeneration.Current then
  begin
    Exit;
  end;
  WakeResources;

  if not LGeneration.Current then
  begin
    Exit;
  end;

  if (FContentBlueprint = nil) or FPublishingContent then
  begin
    Exit;
  end;

  if FContentFrameKnown and (FPanel <> nil) and (FPanel.Parent <> nil) and
    (FContentFrame.Width = Max(0, FPanel.Parent.ClientWidth)) and
    (FContentFrame.Height = Max(0, FPanel.Parent.ClientHeight)) and
    FContentFrame.Selection.Same(FContentRequestedSelection) and
    (FContentMeasurementKey = NyxContentMeasurements(FRoot, MeasureContainers)) then
  begin
    Exit;
  end;

  if not LGeneration.Current then
  begin
    Exit;
  end;
  LExecution := FContentExecution;

  if LExecution <> nil then
  begin
    LStatus := LExecution.Status;

    if not LGeneration.Current or (LStatus in [nesPending, nesRunning]) then
    begin
      Exit;
    end;
  end;

  if FContentWork = nil then
  begin
    FContentWork := NewNyxContentWork(RefreshContent);
  end;
  LWork := FContentWork;
  LEvents := FEvents;
  LScheduler := LEvents.Scheduler;

  if not LGeneration.Current then
  begin
    Exit;
  end;
  LExecution := LScheduler.Submit(LWork, neUIQueue);

  if not LGeneration.Current then
  begin
    LExecution.Cancel;
    Exit;
  end;
  FContentExecution := LExecution;
end;

procedure TNyxLCLRenderer.RefreshContent;
var
  LGeneration: INyxLCLBindingGeneration;
  LEnteredFrame: INyxNativeGestureFrame;
  LFrame: TNyxViewFrame;
  LMeasurements: INyxContainerSnapshot;
  LKey: TNyxText;
  LCandidate: TNyxNode;
  LStates: TNyxContentFaceStates;
  LIndex: Integer;
  LPreviousSelection: TNyxPresentationSelection;
  LLifetime: INyxContentLifetime;
begin
  LGeneration := FGeneration;

  if (LGeneration = nil) or not NativeGenerationCurrent(LGeneration) then
  begin
    Exit;
  end;
  LEnteredFrame := FPhysicalFrame;
  LEnteredFrame.Enter;
  try
    try

      LLifetime := FContentLifetime;
      RequireNativeGeneration(LGeneration);
      FEvents.Scheduler.RequireUI;
      RequireNativeGeneration(LGeneration);

      if FPublishingContent or FContentGuard.Busy then
      begin
        RequireNativeGeneration(LGeneration);
        Exit;
      end;
      RequireNativeGeneration(LGeneration);

      if (FContentBlueprint = nil) or (FPanel = nil) or (FPanel.Parent = nil) or
        (FRoot = nil) then
      begin
        RequireNativeGeneration(LGeneration);
        Exit;
      end;
      RequireNativeGeneration(LGeneration);

      if FUpdating or FLayouting or FProjecting or
        ((FEditingObserver <> nil) and FEditingObserver.Dispatching) or
        ((FPhysicalFrame <> nil) and FPhysicalFrame.Dispatching) or
        ((FLiveBindings <> nil) and not FDesignMode and not FLiveBindings.RefreshReady) or
        ((FState <> nil) and FState.Busy) then
      begin
        RequireNativeGeneration(LGeneration);
        FLastContentError := 'Structural publication requires an idle view and runtime store';
        Exit;
      end;
      RequireNativeGeneration(LGeneration);
      { Host input owns its composing/captured face until its producer ends it.
        Completion queues a fresh observation rather than replacing that widget. }
      for LIndex := 0 to High(FBindings) do
      begin

        if FBindings[LIndex].FComposing or
          NyxLCLHasPointerCapture(FBindings[LIndex].FControl) or
          NyxLCLHasPointerCapture(FBindings[LIndex].FInput) then
        begin
          Exit;
        end;
        RequireNativeGeneration(LGeneration);
      end;
      RequireNativeGeneration(LGeneration);
      LFrame := TNyxViewFrame.At(Max(0, FPanel.Parent.ClientWidth),
        Max(0, FPanel.Parent.ClientHeight), npfNativeLCL);
        RequireNativeGeneration(LGeneration);

      if FContentRequestedSelection.Reference.Defined then
      begin
        RequireNativeGeneration(LGeneration);
        LFrame := LFrame.Selecting(FContentRequestedSelection.Reference);
        RequireNativeGeneration(LGeneration);
      end;
      RequireNativeGeneration(LGeneration);
      LMeasurements := MeasureContainers;
      RequireNativeGeneration(LGeneration);
      LKey := NyxContentMeasurements(FRoot, LMeasurements);
      RequireNativeGeneration(LGeneration);

      if FContentFrameKnown and (FContentFrame.Width = LFrame.Width) and
        (FContentFrame.Height = LFrame.Height) and
        FContentFrame.Selection.Same(LFrame.Selection) and (FContentMeasurementKey = LKey) then
      begin
        RequireNativeGeneration(LGeneration);
        Exit;
      end;
      RequireNativeGeneration(LGeneration);
      LCandidate := nil;
      RequireNativeGeneration(LGeneration);
      try
        try
          LCandidate := FContentBlueprint.Realize(LFrame, LMeasurements);
          RequireNativeGeneration(LGeneration);

          if FResourceContext <> nil then
          begin
            LCandidate.BindResources(FResourceContext.Snapshot,
              FResourceContext.Locale, FResourceContext.Fallback);
              RequireNativeGeneration(LGeneration);
          end;

          if not SameNyxContentStructure(FRoot, LCandidate) then
          begin
            LStates := CaptureContentFaces;
            FPublishingContent := True;
            try
              RenderFrame(FContentBlueprint.Document, FContentBlueprint.Root, FPanel.Parent,
                FDesignMode, FState, FCollectionBindings, LFrame, LMeasurements, True, LStates,
                nrmConfigured, False, nil, nil, FViewPublicationSource);
                RequireNativeGeneration(LGeneration);

              if not LLifetime.Active or not NativeGenerationCurrent(LGeneration) then
              begin
                RequireNativeGeneration(LGeneration);
                Exit;
              end;
              RequireNativeGeneration(LGeneration);
              LKey := FContentMeasurementKey;
            finally

              if LLifetime.Active and NativeGenerationCurrent(LGeneration) then
              begin
                FPublishingContent := False;
                WakeResources;
              end;
            end;
            RequireNativeGeneration(LGeneration);
          end
          else
          begin
            RequireNativeGeneration(LGeneration);
            LPreviousSelection := FPresentationSelection;
            FPresentationSelection := LFrame.Selection;
            RequireNativeGeneration(LGeneration);
            FPublishingContent := True;
            try
              try
                Sync;
                RequireNativeGeneration(LGeneration);
              except

                if LLifetime.Active and NativeGenerationCurrent(LGeneration) then
                begin
                  RequireNativeGeneration(LGeneration);
                  FPresentationSelection := LPreviousSelection;
                  Sync;
                  RequireNativeGeneration(LGeneration);
                end;
                RequireNativeGeneration(LGeneration);
                raise;
              end;
            finally

              if LLifetime.Active and NativeGenerationCurrent(LGeneration) then
              begin
                FPublishingContent := False;
                WakeResources;
              end;
            end;
            RequireNativeGeneration(LGeneration);
          end;
          RequireNativeGeneration(LGeneration);

          if not LLifetime.Active or not NativeGenerationCurrent(LGeneration) then
          begin
            RequireNativeGeneration(LGeneration);
            Exit;
          end;
          RequireNativeGeneration(LGeneration);
          FContentFrame := LFrame;
          FContentFrameKnown := True;
          FContentMeasurementKey := LKey;
          FLastContentError := '';
        except
          on ENyxNativeGenerationRetired do raise;
          on LException: Exception do
          begin

            if LLifetime.Active and NativeGenerationCurrent(LGeneration) then
            begin
              RequireNativeGeneration(LGeneration);
              FContentFrame := LFrame;
              FContentFrameKnown := True;
              FContentMeasurementKey := LKey;
              FLastContentError := TNyxText(LException.Message);
              RequireNativeGeneration(LGeneration);
            end;
            RequireNativeGeneration(LGeneration);
            raise;
          end;
        end;
      finally
        LCandidate.Free;
      end;
      RequireNativeGeneration(LGeneration);

      if LLifetime.Active and NativeGenerationCurrent(LGeneration) then
      begin
        RequireNativeGeneration(LGeneration);
        FlushImages;
        RequireNativeGeneration(LGeneration);
      end;
      RequireNativeGeneration(LGeneration);
    except
      on ENyxNativeGenerationRetired do
      begin
        { The original generation retired; only retained locals may unwind. }
      end;
    end;
  finally
    LEnteredFrame.Leave;
  end;
end;


procedure TNyxLCLRenderer.Render(ADocument: TNyxDocument; ARoot: TNyxNode;
  AHost: TWinControl; ADesignMode: Boolean; AState: TNyxState;
  const ACollections: INyxCollectionBindings; AResourceMode: TNyxResourceRenderMode);
var
  LLifetime: INyxContentLifetime;
begin
  LLifetime := FContentLifetime;

  if (FLiveBindings <> nil) and FLiveBindings.ResourcePublicationBusy then
  begin
    raise ENyxState.Create('View replacement refuses during resource publication');
  end;

  if AHost = nil then
  begin
    raise ENyxModel.Create('Native host is required');
  end;

  if FPublishingContent then
  begin
    raise ENyxModel.Create('Native structural publication is already staging');
  end;
  { A custom constructor may pump CheckSynchronize. Keep accepted controls
    alive until the complete hidden candidate is admitted or discarded. }
  FPublishingContent := True;
  try
    RenderFrame(ADocument, ARoot, AHost, ADesignMode, AState, ACollections,
      TNyxViewFrame.At(Max(0, AHost.ClientWidth), Max(0, AHost.ClientHeight), npfNativeLCL),
      nil, False, nil, AResourceMode);
  finally
    { Notifications may dispose this renderer after adoption. The existing
      weak content guard is retired before any borrowed widgets are released. }

    if LLifetime.Active then
    begin
      FPublishingContent := False;
    end;
  end;

  if not LLifetime.Active then
  begin
    Exit;
  end;
  QueueContent;

  if LLifetime.Active then
  begin
    FlushImages;
  end;
end;

procedure TNyxLCLRenderer.RenderContext(ADocument: TNyxDocument; ARoot: TNyxNode;
  AHost: TWinControl; ADesignMode: Boolean; AState: TNyxState;
  const ACollections: INyxCollectionBindings; const AContext: INyxApplicationContext;
  const AResolver: INyxLogicResolver; AResourceMode: TNyxResourceRenderMode;
  const APublicationSource: INyxViewPublicationSource);
var
  LLifetime: INyxContentLifetime;
begin
  LLifetime := FContentLifetime;
  FEvents.Scheduler.RequireUI;

  if AHost = nil then
  begin
    raise ENyxModel.Create('Runtime rendering requires its target host');
  end;

  if (AContext <> nil) and (not AContext.Active or (AState <> AContext.State)) then
  begin
    raise ENyxModel.Create('Runtime rendering requires the exact active application store');
  end;

  if FPublishingContent or
    ((FLiveBindings <> nil) and FLiveBindings.ResourcePublicationBusy) then
  begin
    raise ENyxModel.Create('Runtime rendering refuses during another view publication');
  end;
  FPublishingContent := True;
  try
    RenderFrame(ADocument, ARoot, AHost, ADesignMode, AState, ACollections,
      TNyxViewFrame.At(Max(0, AHost.ClientWidth), Max(0, AHost.ClientHeight), npfNativeLCL), nil, False, nil,
      AResourceMode, True, AContext, AResolver, APublicationSource);
  finally

    if LLifetime.Active then
    begin
      FPublishingContent := False;
    end;
  end;

  if not LLifetime.Active then
  begin
    Exit;
  end;
  QueueContent;

  if LLifetime.Active then
  begin
    FlushImages;
  end;
end;

procedure TNyxLCLRenderer.RenderFrame(ADocument: TNyxDocument; ARoot: TNyxNode;
  AHost: TWinControl; ADesignMode: Boolean; AState: TNyxState;
  const ACollections: INyxCollectionBindings; const AFrame: TNyxViewFrame;
  const AMeasurements: INyxContainerSnapshot; AKeepPresentation: Boolean;
  const AStates: TNyxContentFaceStates; AResourceMode: TNyxResourceRenderMode;
  AUseContext: Boolean; const AContext: INyxApplicationContext;
  const AResolver: INyxLogicResolver;
  const APublicationSource: INyxViewPublicationSource);
var
  LAttempt: IInterface;
  LAttemptLifetime: INyxContentLifetime;
  LCandidate: TNyxLCLRenderer;
  LRenderEvents: INyxEvents;
  LAcceptedRevision: Integer;
  LIndex: Integer;
  LTransferState: Boolean;
  LRuntimeState: TNyxState;
  LRuntimeOwner: INyxBindingOwnedState;
  LCandidateGeneration: TNativeBindingGeneration;
  LOwnRuntimeState: Boolean;
  LCollections: INyxCollectionBindings;
  LCollectionInputGate: INyxCollectionInputGate;
  LCollectionAdmissions: array of INyxCollectionInputAdmission;
  LCollectionAdmission: INyxCollectionInputAdmission;
  LCollectionObservations: array of INyxObservationPublication;
  LCollectionObservation: INyxLCLCollectionObservation;
  LCollectionEditingPublication: INyxObservationPublication;
  LCollectionMount: INyxCollectionMount;
  LMeasurements: INyxContainerSnapshot;
  { Incoming managed const arguments can alias caller slots. Retain their
    intended values before asking an alternate registry for its snapshot. }
  LFrame: TNyxViewFrame;
  LStates: TNyxContentFaceStates;
  LContext: INyxApplicationContext;
  LResolver: INyxLogicResolver;
  LAllocated: INyxContainerSnapshot;
  LMeasuredRoot: TNyxNode;
  LSettlement: TNyxContentSettlement;
  LBlueprint: TNyxContentBlueprint;
  LFirstCandidate: Boolean;
  LHostSizingLocked: Boolean;
  LUpdating: Boolean;
  LFormerFocus: TWinControl;
  LFormerSelection: TNyxTextSelection;
  { Local leases retain suspended adapters through preview and all-attempt
    rollback. The payload is copied content identity plus value-only cell data;
    no editor pointer crosses into the replacement renderer. }
  LCellEdits: array of record
    Identity: TNyxContentIdentity;
    Source: INyxCollectionEditMount;
    Snapshot: INyxCollectionEditsSnapshot;
    Store: INyxCollection;
    Focused: TNyxCollectionCellEdit;
  end;
  LFormerCellFocus, LCellFocusAllowed: Boolean;
  LCellFocusAnchor: TWinControl;
  LReplacementCellFocus: TWinControl;
  LReplacementCellSource: INyxCollectionEditMount;
  LReplacementCellStore: INyxCollection;
  LReplacementCellDraft: TNyxCollectionCellEdit;
  LCellResumeError: TNyxText;
  LViewportPublication: INyxObservationPublication;
  LEditingPublication: INyxObservationPublication;
  LCapturePublication: INyxObservationPublication;
  LParent: TWinControl;
  LMeasurementKey: TNyxText;
  LFormerHorizontal: Integer;
  LFormerVertical: Integer;
  LEventPreparation: INyxEventViewPreparation;
  LEventView: INyxPreparedEventView;
  LEventFence: INyxPublicationReadiness;
  LFormerCoordinators: TNyxCoordinatorMounts;
  LAcceptedCoordinators: TNyxCoordinatorMounts;
  LCoordinatorRegistry: INyxCoordinatorRegistry;
  LBuiltInCoordinators: INyxCoordinatorRegistry;
  LCoordinatorSnapshot: INyxCoordinatorRegistrySnapshot;
  LCallerCoordinatorSnapshot: INyxCoordinatorRegistrySnapshot;
  LFormerCallerCoordinatorSnapshot: INyxCoordinatorRegistrySnapshot;
  LCoordinatorTarget: INyxCoordinatorRefreshTarget;
  LCommitted: Boolean;
  LSource: INyxViewPublicationSource;
  LFormerSource: INyxViewPublicationSource;
  LRootPublication: INyxPreparedPublication;
  LRootFence: INyxPublicationReadiness;
  LCleanupFailed: Boolean;
  LCleanupError: TNyxText;
  {$ifdef NYX_STUDIO_PROFILE}
  LPhaseStarted: QWord;

  procedure RecordPhase(const AName: TNyxText);
  var
    LNow: QWord;
  begin
    { Main-thread-only opt-in measurement. Production has no instrumentation;
      output identifies only fixed stages, mode and elapsed milliseconds. }
    LNow := GetTickCount64;
    WriteLn('native-view,', AName, ',', ADesignMode, ',', LNow - LPhaseStarted);
    LPhaseStarted := LNow;
  end;
  {$endif}

  function AttemptCurrent: Boolean;
  begin
    Result := False;

    if not LAttemptLifetime.Active then
    begin
      Exit;
    end;
    Result := FRenderAttempt = LAttempt;
  end;

  procedure RequireAttempt;
  begin

    if not AttemptCurrent then
    begin
      raise ENyxNativeGenerationRetired.Create('Native structural attempt retired during callback');
    end;
  end;

  procedure SuspendCellDrafts;
  var
    LBindingIndex, LPrevious, LSlot, LCell: Integer;
    LMount: INyxCollectionMount;
    LEdits: INyxCollectionEditMount;
    LIdentity: TNyxContentIdentity;
  begin
    for LBindingIndex := 0 to High(FBindings) do
    begin
      LMount := FBindings[LBindingIndex].FCollectionMount;

      if (LMount = nil) or not LMount.Connected or
        not LMount.View.Spec.HasLayoutControls then
      begin
        Continue;
      end;
      LEdits := nil;

      if not Supports(LMount, INyxCollectionEditMount, LEdits) then
      begin
        raise ENyxModel.Create('Native table has no cell edit handoff');
      end;
      LIdentity := NyxContentIdentity(FBindings[LBindingIndex].FNode);
      { Identify a borrowed old cell editor before Suspend can hide/reacquire
        it or fail. Even an internally unwound capture must not make rollback
        dereference that former raw editor pointer. }

      if (LFormerFocus is TCustomEdit) and
        (FBindings[LBindingIndex].FControl is TNyxCollectionStringGrid) and
        TNyxCollectionStringGrid(FBindings[LBindingIndex].FControl)
          .OwnsColumnInput(LFormerFocus) then
      begin
        LFormerCellFocus := True;
      end;
      for LPrevious := 0 to High(LCellEdits) do
      begin

        if LCellEdits[LPrevious].Identity.Same(LIdentity) then
        begin
          raise ENyxModel.Create('Native cell edit handoff has an ambiguous content identity');
        end;
      end;
      LSlot := Length(LCellEdits);
      SetLength(LCellEdits, LSlot + 1);
      LCellEdits[LSlot].Identity := LIdentity;
      LCellEdits[LSlot].Source := LEdits;
      LCellEdits[LSlot].Snapshot := LEdits.SuspendCellEdits;
      RequireAttempt;
      LCellEdits[LSlot].Store := LCellEdits[LSlot].Snapshot.Store;
      for LCell := 0 to LCellEdits[LSlot].Snapshot.Count - 1 do
      begin
        LFormerCellFocus := LFormerCellFocus or
          LCellEdits[LSlot].Snapshot.CellAt(LCell).Focused;

        if LCellEdits[LSlot].Snapshot.CellAt(LCell).Focused then
        begin
          LCellEdits[LSlot].Focused := LCellEdits[LSlot].Snapshot.CellAt(LCell);
        end;
      end;
    end;
    RequireAttempt;
    LCellFocusAnchor := Screen.ActiveControl;
  end;

  procedure RestoreCandidateCellDrafts;
  var
    LSlot, LBindingIndex, LFound: Integer;
    LMount: INyxCollectionMount;
    LEdits: INyxCollectionEditMount;
  begin
    for LSlot := 0 to High(LCellEdits) do
    begin
      LFound := -1;
      for LBindingIndex := 0 to High(LCandidate.FBindings) do
      begin

        if not NyxContentIdentity(LCandidate.FBindings[LBindingIndex].FNode)
          .Same(LCellEdits[LSlot].Identity) then
        begin
          Continue;
        end;

        if LFound >= 0 then
        begin
          raise ENyxModel.Create('Native replacement repeats a cell edit identity');
        end;
        LFound := LBindingIndex;
      end;

      if LFound < 0 then
      begin
        Continue;
      end;
      LMount := LCandidate.FBindings[LFound].FCollectionMount;
      LEdits := nil;

      if (LMount <> nil) and LMount.Connected and
        LMount.View.Spec.HasLayoutControls and
        Supports(LMount, INyxCollectionEditMount, LEdits) then
      begin

        if LEdits.RestoreCellEdits(LCellEdits[LSlot].Snapshot, LCellFocusAllowed) then
        begin
          LReplacementCellFocus := Screen.ActiveControl;
          LReplacementCellSource := LEdits;
          LReplacementCellStore := LCellEdits[LSlot].Store;
          LReplacementCellDraft := LCellEdits[LSlot].Focused;
        end;
      end;
    end;
  end;

  function CellRollbackFocusAllowed: Boolean;
  var
    LFocus: TWinControl;
    LSlot: Integer;
  begin
    LFocus := Screen.ActiveControl;
    Result := LFocus = nil;

    if Result then
    begin
      Exit;
    end;

    if (LReplacementCellSource <> nil) and
      (LFocus = LReplacementCellFocus) and
      LReplacementCellSource.CellEditFocusMatches(LReplacementCellStore,
        LReplacementCellDraft) then
    begin
      Exit(True);
    end;

    if LFocus <> LCellFocusAnchor then
    begin
      Exit(False);
    end;
    for LSlot := 0 to High(LCellEdits) do
    begin

      if LCellEdits[LSlot].Focused.Defined and
        LCellEdits[LSlot].Source.CellEditFocusMatches(LCellEdits[LSlot].Store,
          LCellEdits[LSlot].Focused) then
      begin
        Exit(True);
      end;
    end;
    Result := False;
  end;

  procedure ResumeFormerCellDrafts;
  var
    LSlot: Integer;
  begin
    for LSlot := 0 to High(LCellEdits) do
    begin

      if LCellEdits[LSlot].Snapshot = nil then
      begin
        Continue;
      end;
      try
        LCellEdits[LSlot].Source.ResumeCellEdits(LCellEdits[LSlot].Snapshot,
          LCellFocusAllowed);
      except
        on E: Exception do
        begin

          if LCellResumeError = '' then
          begin
            LCellResumeError := TNyxText(E.Message);
          end;
        end;
      end;
    end;

    if LCellResumeError <> '' then
    begin
      { Preserve the original refusal exception while exposing secondary
        all-attempt draft-rollback failure through existing renderer diagnostics. }
      FLastBindingError := 'Cell draft rollback failed: ' + LCellResumeError;
      FLastBindingFailure := nbfRejected;
    end;
  end;

  procedure RememberCleanupFailure(AError: Exception);
  begin

    if LCleanupFailed then
    begin
      Exit;
    end;
    LCleanupFailed := True;

    if (AError is ENyxPublicationNotification) and
      ENyxPublicationNotification(AError).HasReceiverMessage then
    begin
      LCleanupError := ENyxPublicationNotification(AError).ReceiverMessage;
    end
    else
    begin

      if AError is ENyxState then
      begin
        { Nyx model/state exceptions already carry UTF-8 diagnostic bytes. }
        LCleanupError := RawByteString(AError.Message);
        SetCodePage(RawByteString(LCleanupError), CP_UTF8, False);
      end
      else
      begin
        LCleanupError := TNyxText(AError.Message);
      end;
    end;
  end;

  procedure RetirePreparation;
  begin
    { The root owns observations in this exact successor. Retire those first,
      even for discarded hidden passes; never strand the shared event plan if
      an extension violates its nonthrowing retirement contract. }
    try

      if LRootPublication <> nil then
      begin
        LRootPublication.Retire;
      end;
    finally
      try

        if LEventView <> nil then
        begin
          LEventView.Retire;
        end;
      finally
        LRootFence := nil;
        LRootPublication := nil;
        LEventFence := nil;
        LEventView := nil;
      end;
    end;
  end;

  procedure ReleaseCandidate;
  begin
    try
      ReleaseNyxNode(LMeasuredRoot);
    finally
      try
        LCandidate.Free;
      finally
        try
          LSettlement.Free;
        finally
          try
            LBlueprint.Free;
          finally

            if LOwnRuntimeState then
            begin
              LRuntimeState.Free;
            end;
          end;
        end;
      end;
    end;
  end;

  procedure FinishPublication;
  var
    LMount: Integer;
  begin
    { Managed locals survive notification-triggered navigation/disposal. Each
      generic participant gets both attempts even after another one raises. }
    for LMount := 0 to High(LAcceptedCoordinators) do
    begin
      try
        LAcceptedCoordinators[LMount].Notify;
      except
        on E: Exception do RememberCleanupFailure(E);
      end;
      try
        LAcceptedCoordinators[LMount].Retire;
      except
        on E: Exception do RememberCleanupFailure(E);
      end;
    end;
    try
      DisconnectNyxCoordinatorMounts(LFormerCoordinators);
    except
      on E: Exception do RememberCleanupFailure(E);
    end;
    try
      { Release displaced factory leases outside the pure adoption boundary. }
      LFormerCallerCoordinatorSnapshot := nil;
    except
      on E: Exception do RememberCleanupFailure(E);
    end;
    { Only local leases are used after commit. Every independent cleanup and
      notification gets an attempt, including after final native alignment has
      raised. The first diagnostic describes committed data, never rollback. }

    if LRootPublication <> nil then
    begin
      try
        LRootPublication.Notify;
      except
        on E: Exception do RememberCleanupFailure(E);
      end;
      try
        LRootPublication.Retire;
      except
        on E: Exception do RememberCleanupFailure(E);
      end;
    end;

    if LEventView <> nil then
    begin
      try
        LEventView.Notify;
      except
        on E: Exception do RememberCleanupFailure(E);
      end;
      try
        LEventView.Retire;
      except
        on E: Exception do RememberCleanupFailure(E);
      end;
    end;

    if LCleanupFailed then
    begin
      raise ENyxPublicationNotification.CreateReceiverFailure(LCleanupError);
    end;
  end;
begin
  { Exact receiver attempt authority is independent of the accepted view
    generation, which intentionally changes during this operation's commit. }
  LAttemptLifetime := FContentLifetime;
  LRenderEvents := FEvents;
  LAttempt := TNativeRenderObject.Create(nil);
  FRenderAttempt := LAttempt;
  RequireAttempt;

  if AHost = nil then
  begin
    raise ENyxModel.Create('Native host is required');
  end;
  LParent := AHost;
  while LParent <> nil do
  begin

    if LParent = FPanel then
    begin
      raise ENyxModel.Create('A native view cannot replace itself inside its own descendant');
    end;
    LParent := LParent.Parent;
    RequireAttempt;
  end;

  if FDestroying then
  begin
    raise ENyxModel.Create('A destroying native renderer cannot accept a view');
  end;
  LCandidate := nil;
  RequireAttempt;
  LMeasuredRoot := nil;
  RequireAttempt;
  LSettlement := nil;
  RequireAttempt;
  LBlueprint := nil;
  RequireAttempt;
  LEventView := nil;
  RequireAttempt;
  LEventFence := nil;
  RequireAttempt;
  { Incoming arguments may alias the retained field during implicit remount.
    Snapshot both owners before Clear or any external preparation can run. }
  LSource := APublicationSource;
  RequireAttempt;
  LFormerSource := FViewPublicationSource;
  RequireAttempt;
  LRootPublication := nil;
  RequireAttempt;
  LRootFence := nil;
  RequireAttempt;
  LCleanupFailed := False;
  LCleanupError := '';
  LCellEdits := nil;
  RequireAttempt;
  LCollectionAdmissions := nil;
  RequireAttempt;
  LCollectionObservations := nil;
  RequireAttempt;
  LCollectionInputGate := nil;
  RequireAttempt;
  LFormerCellFocus := False;
  LCellFocusAllowed := False;
  LCellFocusAnchor := nil;
  RequireAttempt;
  LReplacementCellFocus := nil;
  RequireAttempt;
  LReplacementCellSource := nil;
  RequireAttempt;
  LReplacementCellStore := nil;
  RequireAttempt;
  LReplacementCellDraft := Default(TNyxCollectionCellEdit);
  RequireAttempt;
  LCellResumeError := '';
  LFormerCoordinators := nil;
  RequireAttempt;
  LAcceptedCoordinators := nil;
  RequireAttempt;
  { Capture once before any factory callout, including hidden settling passes.
    A registration made by a creator belongs to a later Render operation. }
  LCoordinatorRegistry := FCoordinatorRegistry;
  RequireAttempt;
  LCallerCoordinatorSnapshot := FInheritedCoordinatorSnapshot;
  RequireAttempt;
  LFormerCallerCoordinatorSnapshot := FCallerCoordinatorSnapshot;
  RequireAttempt;
  LCoordinatorTarget := FCoordinatorRefreshTarget;
  RequireAttempt;
  LCoordinatorSnapshot := nil;
  RequireAttempt;

  LCommitted := False;
  LRuntimeState := AState;
  LOwnRuntimeState := AState = nil;
  LCollections := ACollections;
  RequireAttempt;
  LMeasurements := AMeasurements;
  RequireAttempt;
  LFrame := AFrame;
  RequireAttempt;
  LStates := Copy(AStates, 0, Length(AStates));
  RequireAttempt;
  LContext := AContext;
  RequireAttempt;
  LResolver := AResolver;
  RequireAttempt;
  LFirstCandidate := True;
  LRuntimeOwner := nil;
  RequireAttempt;

  if (AState <> nil) and (FOwnedState <> nil) and
    (AState = FOwnedState.State) then
  begin
    RequireAttempt;
    { Capture the actual owner before any staging callback. Raw equality alone
      never creates an owner or retargets an entered generation. }
    LRuntimeOwner := FOwnedState;
    RequireAttempt;
  end;
  RequireAttempt;
  LTransferState := LRuntimeOwner <> nil;

  if LCoordinatorRegistry <> nil then
  begin
    { Normalize alternate snapshots before keeping caller-only membership for
      later embedded mounts. No mutable registry survives through this field. }
    LCallerCoordinatorSnapshot := ComposeNyxCoordinatorSnapshots(nil,
      LCoordinatorRegistry.Snapshot);
      RequireAttempt;
  end;
  { Built-ins and caller behavior share one immutable membership and prepared
    lifetime. Authored applications keep their automatic registrations. }
  LBuiltInCoordinators := NewNyxCoordinatorRegistry;
  RequireAttempt;
  LBuiltInCoordinators.RegisterFactory(NyxCustomKind(NyxKindName(nkMasterDetail)),
    NyxExtension(NyxMasterDetailIntentKey), NewNyxMasterDetailCoordinatorFactory);
    RequireAttempt;
  LBuiltInCoordinators.RegisterFactory(NyxCustomKind(NyxKindName(nkKanbanBoard)),
    NyxExtension(NyxKanbanIntentKey), NewNyxKanbanCoordinatorFactory);
    RequireAttempt;
  LCoordinatorSnapshot := ComposeNyxCoordinatorSnapshots(
    LBuiltInCoordinators.Snapshot, LCallerCoordinatorSnapshot);
    RequireAttempt;
  {$ifdef NYX_STUDIO_PROFILE}LPhaseStarted := GetTickCount64;
                             {$endif}
  { LCL propagates this balanced sizing lock through the host's parents and
    descendants. Stage the whole candidate before one final native alignment;
    per-child parenting/font/bounds changes must not relayout the accepted shell
    and every partly constructed sibling. Explicit Nyx layout still runs while
    locked. The host remains borrowed and the failure path releases the lock. }
  AHost.DisableAutoSizing;
  RequireAttempt;
  LHostSizingLocked := True;
  try
  try

    if LOwnRuntimeState then
    begin
      LRuntimeState := ADocument.State.Clone;
      RequireAttempt;
      LRuntimeOwner := OwnNyxBindingState(LRuntimeState);
      RequireAttempt;
      LOwnRuntimeState := False;
    end;
    LBlueprint := NewNyxContentBlueprint(ADocument, ARoot);
    RequireAttempt;

    if LBlueprint <> nil then
    begin
      LSettlement := TNyxContentSettlement.Create;
      RequireAttempt;
    end;
    repeat
      LCandidate := TNyxLCLRenderer.Create(FTheme);
      RequireAttempt;
      LCollectionAdmissions := nil;
      RequireAttempt;
      LCollectionObservations := nil;
      RequireAttempt;
      LCollectionInputGate := NewNyxCollectionInputGate(FInheritedCollectionInputGate);
      RequireAttempt;
      LCandidate.FCollectionInputGate := LCollectionInputGate;
      RequireAttempt;
      LCandidate.FCollectionInputPhase := ncipStaged;
      LCandidate.FTableWorkbenchServices := FTableWorkbenchServices;
      LCandidate.FTableTextLimits := FTableTextLimits;
      { Build may mount saved canvases before this outer candidate commits. }
      LCandidate.FCallerCoordinatorSnapshot := LCallerCoordinatorSnapshot;
      RequireAttempt;

      if AResourceMode = nrmConfigured then
      begin
        LCandidate.FResourceContext := FResourceContext;
        RequireAttempt;
        { Retain the same update port during staging. Nested saved canvases
          subscribe before their parent adopts the candidate's target faces. }
        LCandidate.FResourceUpdates := FResourceUpdates;
      end;
      LCandidate.FStagingContent := LBlueprint <> nil;
      LCandidate.FStagingMeasurements := LMeasurements;

      if FBorrowedTheme <> nil then
      begin
        LCandidate.FTheme := NewNyxDocumentTheme(ADocument, FBorrowedTheme);
        RequireAttempt;
      end
      else
      begin
        LCandidate.FTheme := NewNyxDocumentTheme(ADocument, FBaseTheme);
        RequireAttempt;
      end;
      LCandidate.FOwnTheme := True;
      LCandidate.FThemeOwner := TNativeRenderObject.Create(LCandidate.FTheme);
      RequireAttempt;
      LCandidate.FFactoryKinds := Copy(FFactoryKinds, 0, Length(FFactoryKinds));
      RequireAttempt;
      LCandidate.FDrawingNames := Copy(FDrawingNames, 0, Length(FDrawingNames));
      RequireAttempt;
      LCandidate.FDrawingProviders := Copy(FDrawingProviders, 0, Length(FDrawingProviders));
      RequireAttempt;
      LCandidate.FEmbeddedContext := FEmbeddedContext;
      RequireAttempt;
      LCandidate.FEmbeddedResolver := FEmbeddedResolver;

      if AUseContext then
      begin
        LCandidate.FEmbeddedContext := LContext;
        RequireAttempt;
        LCandidate.FEmbeddedResolver := LResolver;
      end;
      LCandidate.FCanvasAdoptionHost := FCanvasAdoptionHost;
      LCandidate.FFactories := Copy(FFactories, 0, Length(FFactories));
      RequireAttempt;
      LCandidate.FEventFactories := Copy(FEventFactories, 0, Length(FEventFactories));
      RequireAttempt;
      { Candidate ports belong to the final owner's scheduler and view lifetime. }
      LCandidate.FEmitterScope := NewNyxEventEmitterScope(FEvents.Scheduler);
      RequireAttempt;
      LCandidate.FViewportObserver := NewNyxViewportObserver;
      RequireAttempt;
      LCandidate.FEditingObserver := NewNyxLCLEditingObserver;
      RequireAttempt;
      LCandidate.FUpdaters := Copy(FUpdaters, 0, Length(FUpdaters));
      RequireAttempt;
      LCandidate.FDesignMode := ADesignMode;
      LCandidate.FPublishingContent := True;
      LCandidate.FDesignerInput := FDesignerInput;
      LCandidate.FOnDesignerGesture := FOnDesignerGesture;

      if AKeepPresentation then
      begin
        LCandidate.FProjectionContext := FProjectionContext;
        RequireAttempt;
      end
      else
      begin
        LCandidate.FProjectionContext := NyxProjectionContext(ADocument);
        RequireAttempt;
      end;
      LCandidate.FProjectionSchemaRevision := NyxSchemaRevision;
      LCandidate.FPresentationSelection := LFrame.Selection;
      RequireAttempt;
      LCandidate.FRoot := RealizeNyxView(ADocument, ARoot, LFrame, LMeasurements);
      RequireAttempt;

      if (AResourceMode = nrmConfigured) and (FResourceContext <> nil) then
      begin
        LCandidate.FRoot.BindResources(FResourceContext.Snapshot,
          FResourceContext.Locale, FResourceContext.Fallback);
          RequireAttempt;
      end;
      RequireAttempt;
      ApplyNyxPlatform(LCandidate.FRoot, npfNativeLCL);
      RequireAttempt;
      LCandidate.FRoot.ApplyViewport(Max(0, AHost.ClientWidth), Max(0, AHost.ClientHeight),
        npfNativeLCL, LFrame.Selection, LMeasurements);
        RequireAttempt;
      LCandidate.FProjectionBaseline := LCandidate.FRoot.Clone;
      RequireAttempt;
      RestoreNyxContentValues(LCandidate.FRoot, LStates);
      RequireAttempt;
      LCandidate.FCollectionBindings := LCollections;

      if LCollections <> nil then
      begin

        if AKeepPresentation or not LFirstCandidate then
        begin
          LCandidate.FCollectionBindings := LCollections.Recompose(LCandidate.FRoot);
          RequireAttempt;
        end
        else
        begin
          LCollections.ValidateRoot(LCandidate.FRoot);
          RequireAttempt;
        end;
      end
      else
      begin
        LCandidate.FCollectionBindings := NewNyxCollectionBindings(LCandidate.FRoot,
          NewNyxCollectionContext(ADocument.Collections, LCandidate.FRoot.ResourceContext));
          RequireAttempt;
      end;

      if ((not ADesignMode) and
        NyxHasCoordinatorMounts(LCandidate.FRoot, LCoordinatorSnapshot)) or
        (Length(FCoordinatorMounts) <> 0) or (LSource <> nil) or
        (LFormerSource <> nil) then
      begin
        RequireAttempt;

        if not Supports(FEvents, INyxEventViewPreparation, LEventPreparation) then
        begin
          RequireAttempt;
          raise ENyxModel.Create('Native view publication requires prepared event views');
        end;
        RequireAttempt;
        LEventView := LEventPreparation.PrepareView;
        RequireAttempt;

        if not Supports(LEventView, INyxPublicationReadiness, LEventFence) then
        begin
          RequireAttempt;
          raise ENyxModel.Create('Native event view has no final readiness fence');
        end;
        RequireAttempt;

        if not ADesignMode then
        begin
          PrepareNyxCoordinatorMounts(LCandidate.FRoot, LCandidate.FCollectionBindings,
            LEventView, FEvents.Scheduler, LCoordinatorSnapshot, LCoordinatorTarget,
            LCandidate.FCoordinatorMounts, LCandidate.FCoordinatorIDs);
            RequireAttempt;
        end;
      end;
      RequireAttempt;

      if LSource <> nil then
      begin
        LRootPublication := LSource.Prepare(ADocument, LCandidate.FRoot, LEventView);
        RequireAttempt;

        if (LRootPublication = nil) or
          not Supports(LRootPublication, INyxPublicationReadiness, LRootFence) then
        begin
          RequireAttempt;
          raise ENyxModel.Create('Native root publication requires a participant and final readiness fence');
        end;
        RequireAttempt;
      end;
      { Hidden passes share one runtime store/context. Candidate teardown removes
        staged validators and controls without freeing borrowed accepted data. }
      LCandidate.FState := LRuntimeState;
      LCandidate.FOwnedState := LRuntimeOwner;
      LCandidate.FOwnState := LRuntimeOwner <> nil;
      LCandidate.FLiveBindings := TNyxLiveBindings.Create(LCandidate.FRoot, LCandidate.FState);
      RequireAttempt;
      LCandidateGeneration := TNativeBindingGeneration.Create(LCandidate);
      RequireAttempt;
      LCandidate.FGenerationObject := LCandidateGeneration;
      LCandidate.FGeneration := LCandidateGeneration;
      RequireAttempt;
      LCandidate.FLiveBindings.OnSync := LCandidate.Sync;
      RequireAttempt;
      LCandidate.FLiveBindings.OnValueSync := LCandidate.SyncEditedValue;
      RequireAttempt;
      LCandidate.FPanel := TNyxLogicalScrollBox.Create(nil);
      TNativePanelGenerationLease.CreateFor(LCandidate.FPanel, LCandidate.FGeneration);
      RequireAttempt;
      LCandidate.FPanel.Visible := False;
      LCandidate.FPanel.Parent := AHost;
      LCandidate.FPanel.Align := alClient;
      { Alignment is deliberately deferred. Supply the host's current client
        frame for provisional measurements; final LCL alignment and OnResize
        run during reversible physical preview, including existing siblings. }
      LCandidate.FPanel.SetBounds(AHost.ClientRect.Left, AHost.ClientRect.Top,
        AHost.ClientWidth, AHost.ClientHeight);
        RequireAttempt;
      LCandidate.FPanel.BorderStyle := bsNone;
      LCandidate.FPanel.Color := ThemeColor(LCandidate.FTheme.Background);
      RequireAttempt;
      LCandidate.FPanel.Font.Assign(AHost.Font);
      RequireAttempt;
      LCandidate.FPanel.Font.Height := -LCandidate.FTheme.FontSize;
      RequireAttempt;
      LCandidate.FPanel.Font.Color := ThemeColor(LCandidate.FTheme.Text);
      RequireAttempt;
      {$ifdef NYX_STUDIO_PROFILE}RecordPhase('realize');
                                 RequireAttempt;{$endif}
      LCandidate.Build(LCandidate.FRoot, LCandidate.FPanel);
      RequireAttempt;
      {$ifdef NYX_STUDIO_PROFILE}
      WriteLn('native-build,nodes,', LCandidate.FBuildNodes,
        ',validation,', LCandidate.FBuildValidationMilliseconds,
        ',creation,', LCandidate.FBuildCreationMilliseconds,
        ',parent,', LCandidate.FBuildParentMilliseconds,
        ',binding,', LCandidate.FBuildBindingMilliseconds,
        ',completion,', LCandidate.FBuildCompletionMilliseconds,
        ',total,', GetTickCount64 - LPhaseStarted,
        ',collections,', LCandidate.FCollectionBindings.Count);
                       RequireAttempt;
      {$endif}
      for LIndex := 0 to LCandidate.FCollectionBindings.Count - 1 do
      begin
        RequireAttempt;
        LCandidate.FCollectionQueryEvents := FEvents;
        LCollectionMount := LCandidate.BindCollection(LCandidate.FCollectionBindings.ID(LIndex),
          LCandidate.FCollectionBindings.View(LIndex));
          RequireAttempt;
        LCollectionAdmission := nil;
        RequireAttempt;

        if Supports(LCollectionMount, INyxCollectionInputAdmission, LCollectionAdmission) then
        begin
          RequireAttempt;
          SetLength(LCollectionAdmissions, Length(LCollectionAdmissions) + 1);
          RequireAttempt;
          LCollectionAdmissions[High(LCollectionAdmissions)] := LCollectionAdmission;
          RequireAttempt;
        end;
        RequireAttempt;
        LCollectionObservation := nil;
        RequireAttempt;

        if Supports(LCollectionMount, INyxLCLCollectionObservation,
          LCollectionObservation) then
        begin
          RequireAttempt;
          LCollectionEditingPublication := LCollectionObservation.GetEditingPublication;
          RequireAttempt;

          if LCollectionEditingPublication <> nil then
          begin
            SetLength(LCollectionObservations, Length(LCollectionObservations) + 1);
            RequireAttempt;
            LCollectionObservations[High(LCollectionObservations)] := LCollectionEditingPublication;
            RequireAttempt;
          end;
        end;
        RequireAttempt;
      end;
      RequireAttempt;
      LCandidate.BindCollectionPagers;
      RequireAttempt;
      {$ifdef NYX_STUDIO_PROFILE}RecordPhase('build');
                                 RequireAttempt;{$endif}
      LCandidate.Resize(LCandidate.FPanel);
      RequireAttempt;
      {$ifdef NYX_STUDIO_PROFILE}RecordPhase('first-layout');
                                 RequireAttempt;{$endif}
      LCandidate.Sync;
      RequireAttempt;
      LCandidate.MountCanvases(ADocument);
      RequireAttempt;
      LCandidate.MountToolCanvases(ADocument);
      RequireAttempt;
      LCandidate.RestoreContentFaces(LStates, False);
      RequireAttempt;
      {$ifdef NYX_STUDIO_PROFILE}RecordPhase('first-sync');
                                 RequireAttempt;{$endif}

      if LBlueprint = nil then
      begin
        Break;
      end;
      LAllocated := LCandidate.MeasureContainers;
      RequireAttempt;
      LMeasuredRoot := RealizeNyxView(ADocument, ARoot, LFrame, LAllocated);
      RequireAttempt;

      if (AResourceMode = nrmConfigured) and (FResourceContext <> nil) then
      begin
        LMeasuredRoot.BindResources(FResourceContext.Snapshot,
          FResourceContext.Locale, FResourceContext.Fallback);
          RequireAttempt;
      end;
      RequireAttempt;
      ApplyNyxPlatform(LMeasuredRoot, npfNativeLCL);
      RequireAttempt;
      LMeasuredRoot.ApplyViewport(LFrame.Width, LFrame.Height,
        npfNativeLCL, LFrame.Selection, LAllocated);
        RequireAttempt;

      if LSettlement.Accept(LCandidate.FProjectionBaseline, LMeasuredRoot) then
      begin
        RequireAttempt;
        ReleaseNyxNode(LMeasuredRoot);
        RequireAttempt;
        LMeasurements := LAllocated;
        RequireAttempt;
        Break;
      end;
      RequireAttempt;
      ReleaseNyxNode(LMeasuredRoot);
      RequireAttempt;
      LCollections := LCandidate.FCollectionBindings;
      RequireAttempt;
      LMeasurements := LAllocated;
      RequireAttempt;
      LFirstCandidate := False;

      RetirePreparation;
      RequireAttempt;
      FreeAndNil(LCandidate);
      RequireAttempt;
    until False;
    RequireAttempt;
    { One owned authored blueprint survives successful admission. Ordinary
      scalar-only container views keep their existing native settling loop. }
    LCandidate.FContentBlueprint := LBlueprint;
    LBlueprint := nil;
    RequireAttempt;

    if not ADesignMode then
    begin
      LCandidate.FLiveBindings.Activate;
      RequireAttempt;
    end;
    { Physical preview is reversible: the old model, controls, hooks, pending
      callbacks and capability remain owned here until all target work succeeds.
      Native notifications from both trees are muted throughout this phase. }
    LFormerFocus := Screen.ActiveControl;
    RequireAttempt;
    LFormerSelection := Default(TNyxTextSelection);
    RequireAttempt;
    LFormerHorizontal := 0;
    RequireAttempt;
    LFormerVertical := 0;
    RequireAttempt;

    if FPanel <> nil then
    begin
      LFormerHorizontal := FPanel.HorzScrollBar.Position;
      RequireAttempt;
      LFormerVertical := FPanel.VertScrollBar.Position;
      RequireAttempt;
    end;

    if LFormerFocus <> nil then
    begin
      LFormerSelection := CaptureNyxLCLSelection(LFormerFocus);
      RequireAttempt;
    end;
    LUpdating := FUpdating;
    FUpdating := True;
    try
      { Capture after all arbitrary candidate factories and hidden passes. Its
        own mount flag suppresses native blur validation throughout preview. }
      SuspendCellDrafts;
      RequireAttempt;
      LCandidate.FUpdating := True;
      LCandidate.FPanel.OnResize := LCandidate.Resize;
      RequireAttempt;
      LCandidate.FPanel.Visible := True;

      if FPanel <> nil then
      begin
        LCandidate.FPanel.SendToBack;
        RequireAttempt;
      end;
      { Native alignment and resize callbacks are target work, not retirement
        cleanup. Balance the host lock now, while the old tree is still alive. }
      LHostSizingLocked := False;
      AHost.EnableAutoSizing;
      RequireAttempt;
      LCandidate.FUpdating := False;
      LCandidate.Sync;
      RequireAttempt;
      for LIndex := 0 to High(LCandidate.FBindings) do
      begin

        if LCandidate.FBindings[LIndex].FCanvasView <> nil then
        begin
          LCandidate.FBindings[LIndex].FCanvasView.Refresh;
          RequireAttempt;
        end;
      end;
      RequireAttempt;
      LCandidate.FUpdating := True;
      LCandidate.FCaptureObserver := NewNyxLCLCaptureObserver(LCandidate.FPhysicalFrame);
      RequireAttempt;
      for LIndex := 0 to High(LCandidate.FBindings) do
      begin

        if NyxSupportsViewport(LCandidate.FBindings[LIndex].FNode) then
        begin

          if LCandidate.FBindings[LIndex].FInput is TWinControl then
          begin
            LCandidate.FViewportObserver.Add(LCandidate.FBindings[LIndex].FNode.ID,
              TWinControl(LCandidate.FBindings[LIndex].FInput));
              RequireAttempt;
          end
          else if LCandidate.FBindings[LIndex].FControl is TWinControl then
          begin
            LCandidate.FViewportObserver.Add(LCandidate.FBindings[LIndex].FNode.ID,
              TWinControl(LCandidate.FBindings[LIndex].FControl));
              RequireAttempt;
          end;
        end;
        RequireAttempt;
      end;
      RequireAttempt;
      LCandidate.FViewportObserver.Activate(FEvents, ViewportChanged);
      RequireAttempt;
      for LIndex := 0 to High(LCandidate.FBindings) do
      begin

        if (NyxSupportsTextInput(LCandidate.FBindings[LIndex].FNode) or
          (LCandidate.FBindings[LIndex].FNode.ProjectionKind = 'input')) and
          (LCandidate.FBindings[LIndex].FInput is TCustomEdit) then
        begin
          RequireAttempt;
          LCandidate.FEditingObserver.Add(LCandidate.FBindings[LIndex].FNode.ID,
            TWinControl(LCandidate.FBindings[LIndex].FInput));
            RequireAttempt;
        end;
        RequireAttempt;
      end;
      RequireAttempt;
      LCandidate.FEditingObserver.Activate(FEvents, EditingChanged);
      RequireAttempt;
      for LIndex := 0 to High(LCandidate.FBindings) do
      begin
        LCandidate.FCaptureObserver.Add(LCandidate.FBindings[LIndex].FNode.ID,
          LCandidate.FBindings[LIndex].FControl,
          LCandidate.FBindings[LIndex].FControl = LCandidate.FBindings[LIndex].FFocusControl);
          RequireAttempt;

        if (LCandidate.FBindings[LIndex].FInput <> nil) and
          (LCandidate.FBindings[LIndex].FInput <> LCandidate.FBindings[LIndex].FControl) then
        begin
          LCandidate.FCaptureObserver.Add(LCandidate.FBindings[LIndex].FNode.ID,
            LCandidate.FBindings[LIndex].FInput,
            LCandidate.FBindings[LIndex].FInput = LCandidate.FBindings[LIndex].FFocusControl);
            RequireAttempt;
        end;
        RequireAttempt;

        if (LCandidate.FBindings[LIndex].FFocusControl <> nil) and
          (LCandidate.FBindings[LIndex].FFocusControl <> LCandidate.FBindings[LIndex].FControl) and
          (LCandidate.FBindings[LIndex].FFocusControl <> LCandidate.FBindings[LIndex].FInput) then
        begin
          LCandidate.FCaptureObserver.Add(LCandidate.FBindings[LIndex].FNode.ID,
            LCandidate.FBindings[LIndex].FFocusControl, True);
            RequireAttempt;
        end;
        RequireAttempt;
      end;
      RequireAttempt;
      LCandidate.FCaptureObserver.Activate(FEvents, CaptureChanged);
      RequireAttempt;

      if not Supports(LCandidate.FViewportObserver, INyxObservationPublication,
        LViewportPublication) or not LViewportPublication.Ready or
        not Supports(LCandidate.FEditingObserver, INyxObservationPublication,
        LEditingPublication) or not LEditingPublication.Ready or
        not Supports(LCandidate.FCaptureObserver, INyxObservationPublication,
        LCapturePublication) or not LCapturePublication.Ready then
      begin
        RequireAttempt;
        raise ENyxModel.Create('Native candidate observers cannot publish their prepared revision');
      end;
      RequireAttempt;
      LCandidate.FPanel.BringToFront;
      RequireAttempt;
      LCandidate.RestoreContentFaces(LStates, True);
      RequireAttempt;
      { An intervening preview callback can deliberately focus another control.
        Preserve that decision; only the still-current suspension fallback may
        transfer the former cell focus to its newly acquired logical editor. }
      LCellFocusAllowed := LFormerCellFocus and CellRollbackFocusAllowed;
      RestoreCandidateCellDrafts;
      RequireAttempt;

      if AKeepPresentation then
      begin
        LCandidate.FPanel.HorzScrollBar.Position := LFormerHorizontal;
        LCandidate.FPanel.VertScrollBar.Position := LFormerVertical;
        LCandidate.Select(FSelectedDesignID);
        RequireAttempt;
      end;
      LCandidate.FEmitterScope.Activate(EmitNamed);
      RequireAttempt;
      LMeasurementKey := NyxContentMeasurements(LCandidate.FRoot, LMeasurements);
      RequireAttempt;

      if LEventView <> nil then
      begin
        ValidateNyxCoordinatorMounts(LCandidate.FCoordinatorMounts);
        RequireAttempt;

        if LRootPublication <> nil then
        begin
          LRootPublication.Validate;
          RequireAttempt;
        end;
        LEventView.Validate;
        RequireAttempt;
        CheckNyxCoordinatorMounts(LCandidate.FCoordinatorMounts);
        RequireAttempt;

        if LRootFence <> nil then
        begin
          LRootFence.CheckReady;
          RequireAttempt;
        end;
        LEventFence.CheckReady;
        RequireAttempt;
      end;

      for LIndex := 0 to High(LCollectionObservations) do
      begin

        if not LCollectionObservations[LIndex].Ready then
        begin
          RequireAttempt;
          raise ENyxModel.Create('Native cell observers cannot publish their prepared revision');
        end;
        RequireAttempt;
      end;
      RequireAttempt;

      if not LCollectionInputGate.Current then
      begin
        RequireAttempt;
        raise ENyxModel.Create('Native collection input generation retired during preview');
      end;
      RequireAttempt;
    except
      RequireAttempt;
      { Retain exact old controls, input ranges and pending router revision.
        The candidate owns every newly installed hook and releases it on failure. }
      try
        LCellFocusAllowed := CellRollbackFocusAllowed;
        LCandidate.FUpdating := True;
        LCandidate.FPanel.OnResize := nil;
        RequireAttempt;
        LCandidate.FPanel.Visible := False;

        if FPanel <> nil then
        begin
          FPanel.BringToFront;
          RequireAttempt;
        end;

        if not LFormerCellFocus and (LFormerFocus <> nil) and LFormerFocus.CanFocus then
        begin
          RequireAttempt;
          LFormerFocus.SetFocus;
          RequireAttempt;

          if LFormerSelection.Defined then
          begin
            SelectNyxLCLText(LFormerFocus, LFormerSelection);
            RequireAttempt;
          end;
        end;
        RequireAttempt;

        if FPanel <> nil then
        begin
          FPanel.HorzScrollBar.Position := LFormerHorizontal;
          FPanel.VertScrollBar.Position := LFormerVertical;
        end;
      finally
        try
          { Old physical faces are back before their exact single-use leases
            resume. No hidden former editor pointer is dereferenced here. }
          ResumeFormerCellDrafts;
        finally

          if AttemptCurrent then
          begin
            FUpdating := LUpdating;
          end;
        end;
      end;
      RequireAttempt;
      raise;
    end;
    { Preserve an explicitly reused owned runtime store through full remount.
      The candidate borrows it until admission; failed factories retain the old
      view/store ownership. Clear only disconnects the old coordinator here. }

    if LTransferState then
    begin
      FOwnState := False;
    end;
    LFormerCoordinators := FCoordinatorMounts;
    Clear(AKeepPresentation, LEventView);
    RequireAttempt;
    LAcceptedRevision := LRenderEvents.ViewRevision;
    RequireAttempt;
    {$ifdef NYX_STUDIO_PROFILE}RecordPhase('retire');
                               RequireAttempt;{$endif}
    { Publish effective tokens with the successfully staged native controls. }

    FThemeOwner := LCandidate.FThemeOwner;
    RequireAttempt;
    LCandidate.FThemeOwner := nil;
    RequireAttempt;
    FTheme := LCandidate.FTheme;
    FOwnTheme := True;
    LCandidate.FTheme := nil;
    RequireAttempt;
    LCandidate.FOwnTheme := False;
    FState := LCandidate.FState;
    FEmbeddedContext := LCandidate.FEmbeddedContext;
    RequireAttempt;
    FEmbeddedResolver := LCandidate.FEmbeddedResolver;
    RequireAttempt;
    FOwnedState := LCandidate.FOwnedState;
    RequireAttempt;
    LCandidate.FOwnedState := nil;
    RequireAttempt;
    FOwnState := FOwnedState <> nil;
    LOwnRuntimeState := False;
    LCandidate.FState := nil;
    RequireAttempt;
    LCandidate.FOwnState := False;
    FLiveBindings := LCandidate.FLiveBindings;
    LCandidate.FLiveBindings := nil;
    RequireAttempt;
    FGeneration := LCandidate.FGeneration;
    RequireAttempt;
    FGenerationObject := LCandidate.FGenerationObject;
    LCandidate.FGeneration := nil;
    RequireAttempt;
    LCandidate.FGenerationObject := nil;
    RequireAttempt;
    { Only this fresh, never-accepted candidate changes receiver at adoption. }
    TNativeBindingGeneration(FGenerationObject).Owner := Self;
    RequireAttempt;
    TNativeBindingGeneration(FGenerationObject).Source := LSource;
    RequireAttempt;
    TNativeBindingGeneration(FGenerationObject).BaseThemeOwner := FBaseThemeOwner;
    RequireAttempt;
    FButtonGlyphCache := LCandidate.FButtonGlyphCache;
    FGlyphCacheOwner := LCandidate.FGlyphCacheOwner;
    RequireAttempt;
    LCandidate.FButtonGlyphCache := nil;
    RequireAttempt;
    LCandidate.FGlyphCacheOwner := nil;
    RequireAttempt;
    FLiveBindings.OnSync := Sync;
    FLiveBindings.OnValueSync := SyncEditedValue;
    FCollectionBindings := LCandidate.FCollectionBindings;
    RequireAttempt;
    LCandidate.FCollectionBindings := nil;
    RequireAttempt;
    FCollectionInputGate := LCollectionInputGate;
    RequireAttempt;
    LCandidate.FCollectionInputGate := nil;
    RequireAttempt;
    FCallerCoordinatorSnapshot := LCandidate.FCallerCoordinatorSnapshot;
    RequireAttempt;
    LCandidate.FCallerCoordinatorSnapshot := nil;
    RequireAttempt;
    FCoordinatorMounts := LCandidate.FCoordinatorMounts;
    FCoordinatorIDs := LCandidate.FCoordinatorIDs;
    LCandidate.FCoordinatorMounts := nil;
    RequireAttempt;
    LCandidate.FCoordinatorIDs := nil;
    RequireAttempt;
    FLastBindingError := '';
    FLastBindingFailure := nbfNone;
    FRoot := LCandidate.FRoot;
    FResourceContext := LCandidate.FResourceContext;
    RequireAttempt;
    LCandidate.FRoot := nil;
    RequireAttempt;
    FContentBlueprint := LCandidate.FContentBlueprint;
    LCandidate.FContentBlueprint := nil;
    RequireAttempt;
    FPresentationSelection := LFrame.Selection;
    RequireAttempt;
    FContentRequestedSelection := LFrame.Selection;
    RequireAttempt;
    FContentFrame := LFrame;
    FContentFrameKnown := True;
    FContentMeasurementKey := LMeasurementKey;
    FDesignMode := ADesignMode;
    FProjectionContext := LCandidate.FProjectionContext;
    RequireAttempt;
    FProjectionSchemaRevision := LCandidate.FProjectionSchemaRevision;
    RequireAttempt;
    FProjectionBaseline := LCandidate.FProjectionBaseline;
    LCandidate.FProjectionBaseline := nil;
    RequireAttempt;
    FVirtualLayout := LCandidate.FVirtualLayout;
    FPanel := LCandidate.FPanel;
    LCandidate.FPanel := nil;
    RequireAttempt;
    FSelectedDesignID := LCandidate.FSelectedDesignID;
    for LIndex := Low(FSelectionEdges) to High(FSelectionEdges) do
    begin
      FSelectionEdges[LIndex] := LCandidate.FSelectionEdges[LIndex];
      LCandidate.FSelectionEdges[LIndex] := nil;
      RequireAttempt;
    end;
    RequireAttempt;
    FBindings := LCandidate.FBindings;
    FEmitterScope := LCandidate.FEmitterScope;
    RequireAttempt;
    LCandidate.FEmitterScope := nil;
    RequireAttempt;
    FViewportObserver := LCandidate.FViewportObserver;
    RequireAttempt;
    LCandidate.FViewportObserver := nil;
    RequireAttempt;
    FEditingObserver := LCandidate.FEditingObserver;
    RequireAttempt;
    LCandidate.FEditingObserver := nil;
    RequireAttempt;
    FPhysicalFrame := LCandidate.FPhysicalFrame;
    RequireAttempt;
    FCaptureObserver := LCandidate.FCaptureObserver;
    RequireAttempt;
    LCandidate.FCaptureObserver := nil;
    RequireAttempt;
    LCandidate.FBindings := nil;
    RequireAttempt;
    { Logical scroll ports borrow their renderer receiver. The prepared frame
      was wired to LCandidate, which is released below. Rebase every port during
      ownership transfer, without rerunning native layout after old retirement. }
    FPanel.RebindProjection(ProjectViewport);
    RequireAttempt;
    for LIndex := 0 to Length(FBindings) - 1 do
    begin
      FBindings[LIndex].FRenderer := Self;

      if FBindings[LIndex].FControl is TNyxLogicalScrollBox then
      begin
        TNyxLogicalScrollBox(FBindings[LIndex].FControl).RebindProjection(ProjectViewport);
        RequireAttempt;
      end;
    end;
    RequireAttempt;
    FPanel.OnResize := Resize;
    { Commit updates installed revisions through pure managed publication ports.
      No showing, focus or hook installation follows old retirement. Target
      extensions must release their controls without raising in destruction. }
    LViewportPublication.PublishRevision(LAcceptedRevision);
    RequireAttempt;
    LEditingPublication.PublishRevision(LAcceptedRevision);
    RequireAttempt;
    LCapturePublication.PublishRevision(LAcceptedRevision);
    RequireAttempt;
    for LIndex := 0 to High(LCollectionObservations) do
    begin
      LCollectionObservations[LIndex].PublishRevision(LAcceptedRevision);
      RequireAttempt;
    end;
    RequireAttempt;
    InstallNyxCoordinatorMounts(FCoordinatorMounts);
    RequireAttempt;

    if not ADesignMode then
    begin
      FViewPublicationSource := LSource;
      RequireAttempt;
    end;

    if LRootPublication <> nil then
    begin
      LRootPublication.Install;
      RequireAttempt;
    end;
    LAcceptedCoordinators := FCoordinatorMounts;
    FImageViewPublished := True;
    { All facets were retained before retirement. Release only flags here;
      embedded children observe the SAME gate once the outer owner accepts. }
    for LIndex := 0 to High(LCollectionAdmissions) do
    begin
      LCollectionAdmissions[LIndex].InstallInput;
      RequireAttempt;
    end;
    RequireAttempt;
    LCollectionInputGate.Install;
    RequireAttempt;
    { A successor never inherits the entered old command synchronizing shield. }
    FUpdating := False;
    FForceValues := False;
    FForceValueNode := nil;
    RequireAttempt;
    FLayouting := False;
    FProjecting := False;
    LCommitted := True;
    {$ifdef NYX_STUDIO_PROFILE}RecordPhase('publish');
                               RequireAttempt;{$endif}
    {$ifdef NYX_STUDIO_PROFILE}RecordPhase('observe');
                               RequireAttempt;{$endif}
  finally
    try
      try

        if not LCommitted then
        begin
          RetirePreparation;
        end;
      finally
        ReleaseCandidate;
      end;
    finally

      if LHostSizingLocked then
      begin
        AHost.EnableAutoSizing;
      end;
    end;
  end;
  except
    on E: Exception do
    begin

      if not LCommitted then
      begin
        raise;
      end;
      { Only local notification state remains here, even after disposal. }
      RememberCleanupFailure(E);
    end;
  end;
  {$ifdef NYX_STUDIO_PROFILE}

  if AttemptCurrent then
  begin
    RecordPhase('settle');
  end;
  {$endif}
  { Cancellation may enter application code or dispose this renderer. Nothing
    following this boundary borrows Self or its newly accepted widgets. }
  FinishPublication;
end;


procedure TNyxLCLRenderer.FlushImages;
var
  LGeneration: INyxLCLBindingGeneration;
  LDeliveries: TNyxImageDeliveries;
  LIndex: Integer;
begin
  LGeneration := FGeneration;

  if not NativeGenerationCurrent(LGeneration) then
  begin
    Exit;
  end;

  if FUpdating or not FImageViewPublished or FPublishingContent or FDesignMode then
  begin
    Exit;
  end;
  LDeliveries := nil;
  RequireNativeGeneration(LGeneration);
  try
    for LIndex := 0 to Length(FBindings) - 1 do
    begin
      PrepareNyxImageDeliveries(FEvents, FBindings[LIndex].FNode,
        FBindings[LIndex].FImageLifecycle, LDeliveries);
        RequireNativeGeneration(LGeneration);
    end;
    RequireNativeGeneration(LGeneration);
  except
    for LIndex := 0 to Length(LDeliveries) - 1 do
    begin
      LDeliveries[LIndex].Free;
      RequireNativeGeneration(LGeneration);
    end;
    RequireNativeGeneration(LGeneration);
    raise;
  end;
  { The FIFO retains no borrowed binding or node across its UI turn. }

  if FImageDeliveryQueue = nil then
  begin
    FImageDeliveryQueue := NewNyxImageDeliveryQueue;
    RequireNativeGeneration(LGeneration);
  end;
  FImageDeliveryQueue.Enqueue(FEvents, LDeliveries);
  RequireNativeGeneration(LGeneration);
end;


procedure TNyxLCLRenderer.ArrangeControls(AFormer: TNyxNode);
var
  LGeneration: INyxLCLBindingGeneration;
  LPanel: TNyxLogicalScrollBox;
  LSelections: array of TNyxTextSelection;
  LFocus: TWinControl;
  LAncestor: TWinControl;
  LInside: Boolean;
  LUpdating: Boolean;
  LIndex: Integer;
  LHorizontal: Integer;
  LVertical: Integer;

  procedure Arrange(ANode: TNyxNode);
  var
    LBinding: TNyxLCLBinding;
    LChildBinding: TNyxLCLBinding;
    LChild: Integer;
    LTabOrder: Integer;
  begin
    LBinding := Binding(ANode);
    LTabOrder := 0;

    if NyxProjectionChildrenChanged(ANode, AFormer) then
    begin
      for LChild := 0 to ANode.Count - 1 do
      begin
        LChildBinding := Binding(ANode.Children[LChild]);

        if LChildBinding.FLogicalParent <> LBinding then
        begin
          LChildBinding.FControl.Parent := TWinControl(LBinding.FControl);
          RequireNativeGeneration(LGeneration);
          LChildBinding.FLogicalParent := LBinding;
        end;

        if (LChildBinding.FControl is TWinControl) and
          (LChildBinding.FControl.Parent = LBinding.FControl) then
        begin
          { Layout uses the portable tree; native keyboard traversal must follow
            that same order rather than the former TWinControl insertion order. }
          TWinControl(LChildBinding.FControl).TabOrder := LTabOrder;
          RequireNativeGeneration(LGeneration);
          Inc(LTabOrder);
        end;
      end;
    end;
    for LChild := 0 to ANode.Count - 1 do
    begin
      Arrange(ANode.Children[LChild]);
      RequireNativeGeneration(LGeneration);
    end;
  end;

begin
  LGeneration := FGeneration;

  if not NativeGenerationCurrent(LGeneration) then
  begin
    Exit;
  end;

  LFocus := Screen.ActiveControl;
  RequireNativeGeneration(LGeneration);
  LAncestor := LFocus;
  LInside := False;
  while LAncestor <> nil do
  begin

    if LAncestor = FPanel then
    begin
      LInside := True;
      Break;
    end;
    LAncestor := LAncestor.Parent;
    RequireNativeGeneration(LGeneration);
  end;
  SetLength(LSelections, Length(FBindings));
  RequireNativeGeneration(LGeneration);
  for LIndex := 0 to High(FBindings) do
  begin

    if FBindings[LIndex].FInput is TWinControl then
    begin
      LSelections[LIndex] := CaptureNyxLCLSelection(TWinControl(FBindings[LIndex].FInput));
      RequireNativeGeneration(LGeneration);
    end;
  end;
  RequireNativeGeneration(LGeneration);
  LHorizontal := FPanel.HorzScrollBar.Position;
  RequireNativeGeneration(LGeneration);
  LVertical := FPanel.VertScrollBar.Position;
  RequireNativeGeneration(LGeneration);
  LUpdating := FUpdating;
  FUpdating := True;
  LPanel := FPanel;
  LPanel.DisableAutoSizing;
  RequireNativeGeneration(LGeneration);
  try
    Arrange(FRoot);
    RequireNativeGeneration(LGeneration);
    { An emptied destination can still have a zero-height physical client box
      from its former layout. Allocate the new tree before restoring focus;
      focusing into the old empty box would be lost during the following Sync. }
    Resize(FPanel);
    RequireNativeGeneration(LGeneration);

    if LInside and LFocus.CanFocus then
    begin
      RequireNativeGeneration(LGeneration);
      LFocus.SetFocus;
      RequireNativeGeneration(LGeneration);
    end;
    RequireNativeGeneration(LGeneration);
    for LIndex := 0 to High(FBindings) do
    begin

      if LSelections[LIndex].Defined then
      begin
        { Win32 reparenting can retire a nonfocused child's window handle.
          Recreate that same control's handle before using its physical range
          bridge; the LCL object, Text and portable event binding stay retained. }
        TWinControl(FBindings[LIndex].FInput).HandleNeeded;
        RequireNativeGeneration(LGeneration);
        SelectNyxLCLText(TWinControl(FBindings[LIndex].FInput), LSelections[LIndex]);
        RequireNativeGeneration(LGeneration);
      end;
    end;
    RequireNativeGeneration(LGeneration);
    FPanel.HorzScrollBar.Position := LHorizontal;
    FPanel.VertScrollBar.Position := LVertical;
  finally

    if NativeGenerationCurrent(LGeneration) then
    begin
      FUpdating := LUpdating;
    end;
    LPanel.EnableAutoSizing;
  end;
  RequireNativeGeneration(LGeneration);
end;


function TNyxLCLRenderer.TryRefresh(ADocument: TNyxDocument; ARoot: TNyxNode;
  ADesignMode: Boolean; const ARestores: TNyxProjectionValueRestores): Boolean;
var
  LGeneration: INyxLCLBindingGeneration;
  LEnteredFrame: INyxNativeGestureFrame;
  LCandidate: TNyxNode;
  LPrevious: TNyxNode;
  LProjectedCandidate: TNyxNode;
  LProjectedBaseline: TNyxNode;
  LAccepted: TNyxNode;
  LBaseline: TNyxNode;
  LTheme: TNyxTheme;
  LIndex: Integer;
  LPreviousSelection: TNyxPresentationSelection;
  LArrangement: Boolean;
  LBound: Boolean;
  LContentBlueprint: TNyxContentBlueprint;
  LFrame: TNyxViewFrame;
  LMeasurements: INyxContainerSnapshot;
  LNextSelection: TNyxPresentationSelection;
  {$ifdef NYX_STUDIO_PROFILE}
  LPhaseStarted: QWord;

  procedure RefreshPhase(const APhase: TNyxText);
  var
    LNow: QWord;
  begin
    { Diagnostic builds expose static boundaries only. Keep admission,
      realization and physical Sync separate so a fast layout cannot conceal
      costly validation/repeated source work in the retained-view path. }
    LNow := GetTickCount64;
    WriteLn('native-refresh,', APhase, ',', Length(FBindings), ',',
      LNow - LPhaseStarted);
    LPhaseStarted := LNow;
  end;
  {$endif}

  function Compatible(AFrom, ATo: TNyxNode; AArrange: Boolean): Boolean;
  begin

    if LBound then
    begin

      if AArrange then
      begin
        Result := CanArrangeNyxBoundProjection(AFrom, ATo);
      end
      else
      begin
        Result := CanRefreshNyxBoundProjection(AFrom, ATo);
      end;
    end
    else if AArrange then
    begin
      Result := CanArrangeNyxProjection(AFrom, ATo);
    end
    else
    begin
      Result := CanRefreshNyxProjection(AFrom, ATo);
    end;
  end;

  function ApplyProjection(AFrom, ABefore: TNyxNode;
    const AFields: TNyxProjectionValueRestores): Boolean;
  begin

    if LBound then
    begin

      if LArrangement then
      begin
        Result := RefreshNyxBoundProjectionArrangement(FRoot, AFrom, ABefore, AFields);
      end
      else
      begin
        Result := RefreshNyxBoundProjectionProperties(FRoot, AFrom, ABefore, AFields);
      end;
    end
    else if LArrangement then
    begin
      Result := RefreshNyxProjectionArrangement(FRoot, AFrom, ABefore, AFields);
    end
    else
    begin
      Result := RefreshNyxProjectionProperties(FRoot, AFrom, ABefore, AFields);
    end;
  end;

  procedure RestoreFields;
  var
    LField: Integer;
    LBinding: Integer;
  begin
    { The complete restore group has already passed model admission. Clear only
      each requested face's accepted-value marker so Sync replaces an unfinished
      numeric draft even when its accepted value is unchanged. Other drafts keep
      their markers; a bound field restores the CURRENT store, never defaults. }
    for LField := 0 to High(ARestores) do
    begin
      for LBinding := 0 to High(FBindings) do
      begin

        if FBindings[LBinding].FNode.ID = ARestores[LField].RuntimeID then
        begin
          FBindings[LBinding].FHasValueBaseline := False;
          Break;
        end;
      end;
    end;
  end;
begin
  LGeneration := FGeneration;
  Result := False;

  if not NativeGenerationCurrent(LGeneration) then
  begin
    Exit;
  end;
  LEnteredFrame := FPhysicalFrame;
  LEnteredFrame.Enter;
  try
  {$ifdef NYX_STUDIO_PROFILE}LPhaseStarted := GetTickCount64;{$endif}

  FEvents.Scheduler.RequireUI;
  RequireNativeGeneration(LGeneration);
  Result := False;

  { Live collection edits call Sync directly. An authored replacement of a
    board must admit its exact new nodes/views and accepted event generation. }

  if (FRoot = nil) or (FPanel = nil) or FUpdating or
    (Length(FCoordinatorMounts) <> 0) or
    { Executable registration can change independently of scalar source. Keep
      such candidates on the complete prepared path, including first adoption. }
    (FCoordinatorRegistry <> nil) or
    (FInheritedCoordinatorSnapshot <> nil) or (FCallerCoordinatorSnapshot <> nil) or
    (FDesignMode <> ADesignMode) or
    ((FLiveBindings <> nil) and not FLiveBindings.RefreshReady and
      (not FDesignMode or FLiveBindings.HasBindings)) or
    ((FState <> nil) and FState.Busy) or
    (FProjectionSchemaRevision <> NyxSchemaRevision) then
  begin
    RequireNativeGeneration(LGeneration);
    Exit;
  end;
  RequireNativeGeneration(LGeneration);
  { An unbound design canvas deliberately has no live subscription. That idle
    absence must not force scalar edits to replace every control. Runtime and
    bound design views retain the coordinator's complete readiness guard; an
    active state command still refuses on either path. }
  for LIndex := 0 to High(FBindings) do
  begin

    if FBindings[LIndex].FCustom or (FactoryIndex(FBindings[LIndex].FNode) >= 0) then
    begin
      Exit;
    end;
    RequireNativeGeneration(LGeneration);
  end;
  RequireNativeGeneration(LGeneration);
  {$ifdef NYX_STUDIO_PROFILE}RefreshPhase('guards');{$endif}
  { Validate fresh public data before comparing the immutable context. Retaining
    controls never implies a stale document cache or skipped source admission. }
  ValidateNyxDocumentProperties(ADocument);
  RequireNativeGeneration(LGeneration);
  {$ifdef NYX_STUDIO_PROFILE}RefreshPhase('properties');{$endif}

  if NyxProjectionContext(ADocument) <> FProjectionContext then
  begin
    Exit;
  end;
  RequireNativeGeneration(LGeneration);
  {$ifdef NYX_STUDIO_PROFILE}RefreshPhase('context');{$endif}
  LCandidate := nil;
  RequireNativeGeneration(LGeneration);
  LPrevious := nil;
  RequireNativeGeneration(LGeneration);
  LProjectedCandidate := nil;
  RequireNativeGeneration(LGeneration);
  LProjectedBaseline := nil;
  RequireNativeGeneration(LGeneration);
  LTheme := nil;
  RequireNativeGeneration(LGeneration);
  LBound := (FLiveBindings <> nil) and FLiveBindings.HasBindings;
  RequireNativeGeneration(LGeneration);
  LContentBlueprint := nil;
  RequireNativeGeneration(LGeneration);
  try

    if FBorrowedTheme <> nil then
    begin
      LTheme := NewNyxDocumentTheme(ADocument, FBorrowedTheme);
      RequireNativeGeneration(LGeneration);
    end
    else
    begin
      LTheme := NewNyxDocumentTheme(ADocument, FBaseTheme);
      RequireNativeGeneration(LGeneration);
    end;

    if LTheme.CSS <> FTheme.CSS then
    begin
      RequireNativeGeneration(LGeneration);
      Exit;
    end;
    RequireNativeGeneration(LGeneration);
    {$ifdef NYX_STUDIO_PROFILE}RefreshPhase('theme');{$endif}
    LFrame := TNyxViewFrame.At(Max(0, FPanel.Parent.ClientWidth),
      Max(0, FPanel.Parent.ClientHeight), npfNativeLCL);
      RequireNativeGeneration(LGeneration);
    LNextSelection := FPresentationSelection.Reconciled(ADocument.Presentations.Snapshot);
    RequireNativeGeneration(LGeneration);

    if LNextSelection.Reference.Defined then
    begin
      RequireNativeGeneration(LGeneration);
      LFrame := LFrame.Selecting(LNextSelection.Reference);
      RequireNativeGeneration(LGeneration);
    end;
    RequireNativeGeneration(LGeneration);
    LMeasurements := MeasureContainers;
    RequireNativeGeneration(LGeneration);
    LCandidate := RealizeNyxView(ADocument, ARoot, LFrame, LMeasurements);
    RequireNativeGeneration(LGeneration);
    {$ifdef NYX_STUDIO_PROFILE}RefreshPhase('realize');{$endif}

    if FResourceContext <> nil then
    begin
      LCandidate.BindResources(FResourceContext.Snapshot,
        FResourceContext.Locale, FResourceContext.Fallback);
        RequireNativeGeneration(LGeneration);
    end;
    LContentBlueprint := NewNyxContentBlueprint(ADocument, ARoot);
    RequireNativeGeneration(LGeneration);
    ApplyNyxPlatform(LCandidate, npfNativeLCL);
    RequireNativeGeneration(LGeneration);
    {$ifdef NYX_STUDIO_PROFILE}RefreshPhase('content-platform');{$endif}

    LAccepted := LCandidate;
    LBaseline := FProjectionBaseline;

    if LBound then
    begin

      if not FLiveBindings.TryPrepareRefresh(LCandidate, FProjectionBaseline,
        LProjectedCandidate, LProjectedBaseline) then
      begin
        RequireNativeGeneration(LGeneration);
        Exit;
      end;
      RequireNativeGeneration(LGeneration);
      LAccepted := LProjectedCandidate;
      LBaseline := LProjectedBaseline;
    end;
    {$ifdef NYX_STUDIO_PROFILE}RefreshPhase('bound-prepare');{$endif}
    LArrangement := not Compatible(FRoot, LAccepted, False);
    RequireNativeGeneration(LGeneration);

    if ((not LArrangement) and not Compatible(LBaseline, LAccepted, False)) or
      (LArrangement and (not Compatible(FRoot, LAccepted, True) or
        not Compatible(LBaseline, LAccepted, True))) or
      (FProjectionSchemaRevision <> NyxSchemaRevision) then
    begin
      {$ifdef NYX_STUDIO_PROFILE}RefreshPhase('incompatible');{$endif}
      Exit;
    end;
    RequireNativeGeneration(LGeneration);
    {$ifdef NYX_STUDIO_PROFILE}RefreshPhase('compatible');{$endif}
    LPrevious := FRoot.Clone;
    RequireNativeGeneration(LGeneration);
    {$ifdef NYX_STUDIO_PROFILE}RefreshPhase('rollback-copy');{$endif}
    LPreviousSelection := FPresentationSelection;

    try

      if not ApplyProjection(LAccepted, LBaseline, ARestores) then
      begin
        Exit;
      end;
      RequireNativeGeneration(LGeneration);
      {$ifdef NYX_STUDIO_PROFILE}RefreshPhase('apply');{$endif}

      if LArrangement then
      begin
        ArrangeControls(LPrevious);
        RequireNativeGeneration(LGeneration);
      end;
      RestoreFields;
      RequireNativeGeneration(LGeneration);
      FPresentationSelection := FPresentationSelection.Reconciled(FRoot.PresentationSnapshot);
      RequireNativeGeneration(LGeneration);
      Sync;
      RequireNativeGeneration(LGeneration);
      {$ifdef NYX_STUDIO_PROFILE}RefreshPhase('sync');{$endif}
    except

      if not NativeGenerationCurrent(LGeneration) then
      begin
        raise;
      end;
      RequireNativeGeneration(LGeneration);
      { No custom updater entered this path. Restore properties AND admitted
        arrangement before normal synchronization; callbacks still refer to
        the same independently owned realized nodes and native controls. }

      if LArrangement then
      begin
        ApplyProjection(LPrevious, nil, nil);
        RequireNativeGeneration(LGeneration);
        ArrangeControls(LCandidate);
        RequireNativeGeneration(LGeneration);
      end
      else
      begin
        ApplyProjection(LPrevious, nil, nil);
        RequireNativeGeneration(LGeneration);
      end;
      FPresentationSelection := LPreviousSelection;
      Sync;
      RequireNativeGeneration(LGeneration);
      raise;
    end;
    ReleaseNyxNode(FProjectionBaseline);
    RequireNativeGeneration(LGeneration);
    FProjectionBaseline := LCandidate;
    LCandidate := nil;
    RequireNativeGeneration(LGeneration);
    FreeAndNil(FContentBlueprint);
    RequireNativeGeneration(LGeneration);
    FContentBlueprint := LContentBlueprint;
    LContentBlueprint := nil;
    RequireNativeGeneration(LGeneration);
    FContentFrameKnown := False;
    FContentRequestedSelection := FContentRequestedSelection.Reconciled(FRoot.PresentationSnapshot);
    RequireNativeGeneration(LGeneration);
    QueueContent;
    RequireNativeGeneration(LGeneration);
    Result := True;
    {$ifdef NYX_STUDIO_PROFILE}RefreshPhase('adopt');{$endif}
  finally
    LContentBlueprint.Free;
    LTheme.Free;
    ReleaseNyxNode(LProjectedBaseline);
    ReleaseNyxNode(LProjectedCandidate);
    ReleaseNyxNode(LPrevious);
    ReleaseNyxNode(LCandidate);
  end;
  RequireNativeGeneration(LGeneration);
  finally
    LEnteredFrame.Leave;
  end;
end;


procedure TNyxLCLRenderer.SetTableTextLimits(const ALimits: TNyxTableTextWorkLimits);
begin
  ALimits.Validate;

  if (FRoot <> nil) or (FRenderAttempt <> nil) or FPublishingContent then
  begin
    raise ENyxModel.Create('Configure table text limits before mounting or after Unmount');
  end;
  FTableTextLimits := ALimits;
end;

procedure TNyxLCLRenderer.SetTableWorkbenchServices(
  const AServices: INyxTableWorkbenchServices);
begin

  if (FRoot <> nil) or (FRenderAttempt <> nil) or FPublishingContent then
  begin
    raise ENyxModel.Create('Configure table services before mounting or after Unmount');
  end;
  FTableWorkbenchServices := AServices;
end;

procedure TNyxLCLRenderer.SetResourceContext(const AContext: INyxResourceContext);
var
  LContext: INyxResourceContext;
begin

  if FRoot <> nil then
  begin
    raise ENyxResource.Create('Configure a renderer resource context before mounting');
  end;
  LContext := nil;

  if AContext <> nil then
  begin
    LContext := NewNyxResourceContext(AContext.Snapshot,
      AContext.Locale, AContext.Fallback);
  end;
  FResourceContext := LContext;
end;

function TNyxLCLRenderer.GetResourceContext: INyxResourceContext;
begin
  Result := FResourceContext;

  if FRoot <> nil then
  begin
    Result := FRoot.ResourceContext;
  end;
end;

function TNyxLCLRenderer.PrepareResourcePublication(const AContext: INyxResourceContext): INyxPreparedPublication;
begin

  if (FLiveBindings = nil) or FPublishingContent or (AContext = nil) then
  begin
    raise ENyxState.Create('Resource publication requires an idle mounted view and context');
  end;
  Result := FLiveBindings.PrepareResourcePublication(AContext.Snapshot,
    AContext.Locale, AContext.Fallback);
end;

function TNyxLCLRenderer.PrepareResourceReload(
  const AContext: INyxResourceContext): INyxPreparedPublication;
var
  LRows: INyxPreparedPublication;
  LScalar: INyxPreparedPublication;
begin

  if AContext = nil then
  begin
    raise ENyxState.Create('Resource reload requires a context');
  end;
  LRows := PrepareNyxCollectionBindingResources(FCollectionBindings,
    AContext.Snapshot, AContext.Locale, AContext.Fallback);
  try
    LScalar := PrepareResourcePublication(AContext);

    if LRows <> nil then
    begin
      Result := PrepareNyxGroup([LRows, LScalar]);
    end
    else
    begin
      Result := LScalar;
    end;
  except

    if LRows <> nil then
    begin
      LRows.Retire;
    end;
    raise;
  end;
end;

function TNyxLCLRenderer.SectionPublicationReady: Boolean;
var
  LIndex: Integer;
begin
  FEvents.Scheduler.RequireUI;
  Result := (FRoot <> nil) and not FUpdating and not FPublishingContent and
    not FContentGuard.Busy and ((FState = nil) or not FState.Busy) and
    ((FEditingObserver = nil) or not FEditingObserver.Dispatching) and
    ((FPhysicalFrame = nil) or not FPhysicalFrame.Dispatching) and
    ((FLiveBindings = nil) or FLiveBindings.RefreshReady);

  if Result and (FCollectionBindings <> nil) then
  begin
    for LIndex := 0 to FCollectionBindings.Count - 1 do
    begin

      if not NyxCollectionViewPublicationReady(FCollectionBindings.View(LIndex)) then
      begin
        Exit(False);
      end;
    end;
  end;
end;

function TNyxLCLRenderer.ResourceReady: Boolean;
begin
  Result := (FLiveBindings <> nil) and not FPublishingContent;

  if Result then
  begin
    Result := FLiveBindings.RefreshReady;
  end;
end;

procedure TNyxLCLRenderer.SetResourceUpdates(const AQueue: INyxResourceUpdateQueue);
begin

  if FRoot <> nil then
  begin
    raise ENyxModel.Create('Install application resource updates before mounting');
  end;

  if AQueue <> nil then
  begin
    SetResourceContext(AQueue.Context);
  end;
  FResourceUpdates := AQueue;
end;

function TNyxLCLRenderer.CanReloadResources(const AContext: INyxResourceContext): Boolean;
begin
  Result := ResourceReady;

  if Result then
  begin
    FLiveBindings.ValidateResourceReload(AContext.Snapshot,
      AContext.Locale, AContext.Fallback);
  end;
end;

procedure TNyxLCLRenderer.WakeResources;
begin

  if (FResourceUpdates <> nil) and (not FPublishingContent) then
  begin
    FResourceUpdates.Wake;
  end;
end;

procedure TNyxLCLRenderer.ReloadResources(const AResources: INyxResources;
  const ALocale, AFallback: TNyxLocaleRef);
var
  LRows, LScalar: INyxPreparedPublication;
begin

  if FLiveBindings = nil then
  begin
    raise ENyxState.Create('Resource reload requires a mounted runtime view');
  end;
  LRows := PrepareNyxCollectionBindingResources(FCollectionBindings, AResources, ALocale, AFallback);
  try
    LScalar := FLiveBindings.PrepareResourcePublication(AResources, ALocale, AFallback);

    if LRows <> nil then
    begin
      PublishNyxGroup([LRows, LScalar]);
    end
    else
    begin
      PublishNyxGroup([LScalar]);
    end;
  finally
    { Retire also when scalar preparation refused before the group adopted rows. }

    if LRows <> nil then
    begin
      LRows.Retire;
    end;
    { Synchronization can fail AFTER ordinary binding publication. Preserve the
      actual accepted frame in that case too; never revert it to authored data.
      Existing view-only callers retain their original scope contract. }

    if (FResourceContext <> nil) and (FRoot <> nil) then
    begin
      FResourceContext := FRoot.ResourceContext;
    end;
  end;
end;

procedure TNyxLCLRenderer.Unmount;
begin
  FRenderAttempt := nil;
  Clear;
end;

procedure TNyxLCLRenderer.SetPresentationSelection(const AValue: TNyxPresentationSelection);
var
  LPrevious: TNyxPresentationSelection;
begin
  FEvents.Scheduler.RequireUI;

  if (FRoot = nil) or (FPanel = nil) then
  begin
    raise ENyxModel.Create('Select a presentation on an idle mounted view');
  end;
  AValue.Validate(FRoot.PresentationSnapshot);

  if FContentBlueprint <> nil then
  begin
    { Structural requests coalesce on the UI queue. Reading the capability
      reports accepted selection, never a request whose factories may fail. }
    FContentRequestedSelection := AValue;
    FContentFrameKnown := False;
    QueueContent;
    Exit;
  end;

  if FUpdating or FLayouting then
  begin
    raise ENyxModel.Create('Select a presentation on an idle mounted view');
  end;

  if AValue.Same(FPresentationSelection) then
  begin
    Exit;
  end;
  LPrevious := FPresentationSelection;
  FPresentationSelection := AValue;
  try
    Sync;
  except
    FPresentationSelection := LPrevious;
    Sync;
    raise;
  end;
end;

function TNyxLCLRenderer.ReadPresentationSelection: TNyxPresentationSelection;
begin
  FEvents.Scheduler.RequireUI;

  if FRoot = nil then
  begin
    raise ENyxPresentation.Create('This presentation view is not mounted');
  end;
  Result := FPresentationSelection;
end;

function TNyxLCLRenderer.GetPresentationView: INyxPresentationView;
begin
  ReadPresentationSelection;

  if FPresentationView = nil then
  begin
    FPresentationView := NewNyxPresentationView(ReadPresentationSelection, SetPresentationSelection);
  end;
  Result := FPresentationView;
end;

function TNyxLCLRenderer.CollectionView(const AID: TNyxText): INyxCollectionView;
var
  LIndex: Integer;
begin
  { Manual runtime attachments are public views too. Keep authored-default lookup
    as the fallback; observing a provider never writes a descriptor/default. }
  for LIndex := 0 to High(FBindings) do
  begin

    if (FBindings[LIndex].FNode.ID = AID) and
      (FBindings[LIndex].FCollectionMount <> nil) and
      FBindings[LIndex].FCollectionMount.Connected then
    begin
      Exit(FBindings[LIndex].FCollectionMount.View);
    end;
  end;

  if FCollectionBindings = nil then
  begin
    raise ENyxModel.Create('No authored collection views are mounted');
  end;
  Result := FCollectionBindings.ViewFor(AID);
end;

function TNyxLCLRenderer.CollectionMount(const AID: TNyxText): INyxCollectionMount;
begin
  Result := IdentityBinding(AID, niRuntime).FCollectionMount;

  if (Result = nil) or not Result.Connected then
  begin
    raise ENyxModel.Create('No live collection attachment: ' + AID);
  end;
end;

function TNyxLCLRenderer.BindCollection(const AID: TNyxText;
  const AView: INyxCollectionView): INyxCollectionMount;
var
  LBinding: TNyxLCLBinding;
  LKind: TNyxText;
  LQueryMount: INyxCollectionQueryMount;
  LColumnsMount: INyxCollectionColumnArrangementMount;
  LInputAdmission: INyxCollectionInputAdmission;
  LWorkbenchSpec: TNyxCollectionViewSpec;
begin

  if AView = nil then
  begin
    raise ENyxModel.Create('Collection binding requires a live view');
  end;
  LKind := 'list';

  if AView.Projection = cpTable then
  begin
    LKind := 'table';
  end
  else if AView.Projection = cpTree then
  begin
    LKind := 'tree';
  end;
  LBinding := IdentityBinding(AID, niAutomatic);

  if LBinding.FNode.ProjectionKind <> LKind then
  begin
    raise ENyxModel.Create('Collection view differs from the mounted control kind');
  end;

  if (LBinding.FCollectionMount <> nil) and LBinding.FCollectionMount.Connected then
  begin
    raise ENyxModel.Create('Control already has a live collection binding');
  end;
  Result := MountNyxLCLCollectionWithTextLimits(LBinding.FControl, AView, FTableTextLimits,
    FCollectionInputPhase, FCollectionInputGate, FPhysicalFrame,
    FCollectionQueryEvents, LBinding.FNode.ID, LBinding.PerformClick,
    LBinding.PreparedCellContext);
  LBinding.FCollectionMount := Result;

  if Supports(Result, INyxCollectionInputAdmission, LInputAdmission) then
  begin
    LBinding.FCollectionInputLifetime := LInputAdmission.GetInputLifetime;
  end;
  Result.ObserveSelection(LBinding.CollectionSelectionChanged);

  if not FDesignMode and (AView.Projection = cpTable) and
    Supports(Result, INyxCollectionQueryMount, LQueryMount) then
  begin
    LQueryMount.ConfigureQueries(NewNyxLCLQueryFormFactory(
      TWinControl(LBinding.FControl), FTheme), FCollectionQueryEvents,
      NyxControlEvents(LBinding.FNode.ID, niRuntime), LBinding.FNode.DesignID);
  end;

  if not FDesignMode and (AView.Projection = cpTable) and
    AView.Spec.HasLayoutControls and
    Supports(Result, INyxCollectionColumnArrangementMount, LColumnsMount) and
    Supports(Result, INyxCollectionInputAdmission, LInputAdmission) then
  begin
    LColumnsMount.ConfigureColumnArrangement(
      NewNyxLCLColumnArrangementFactory(TWinControl(LBinding.FControl), FTheme,
        LInputAdmission.GetInputLifetime), FCollectionQueryEvents);
  end;

  if not FDesignMode and (AView.Projection = cpTable) then
  begin
    LWorkbenchSpec := AView.Spec;

    if LWorkbenchSpec.HasTableWorkbenchPolicy then
    begin
      ConfigureNyxTableWorkbench(Result, LWorkbenchSpec.TableWorkbenchPolicy,
        FTableWorkbenchServices,
        NewNyxLCLCollectionCellPresenterFactory(TWinControl(LBinding.FControl)),
        FCollectionQueryEvents);
    end;
  end;
end;

function TNyxLCLRenderer.IdentityBinding(const AID: TNyxText;
  AIdentity: TNyxIdentityKind): TNyxLCLBinding;
var
  LIndex: Integer;
  LPass: Integer;
begin
  { Resolve the complete runtime namespace before falling back to editable owner
    identity. Explicit design/runtime modes share the browser adapter's contract. }
  for LPass := 0 to 1 do
  begin
    for LIndex := 0 to Length(FBindings) - 1 do
    begin

      if (LPass = 0) and (AIdentity <> niDesign) and
        (FBindings[LIndex].FNode.ID = AID) then
      begin
        Exit(FBindings[LIndex]);
      end;

      if (LPass = 1) and (AIdentity <> niRuntime) and
        (FBindings[LIndex].FNode.DesignID = AID) then
      begin
        Exit(FBindings[LIndex]);
      end;
    end;
  end;
  raise ENyxModel.Create('Native component is not mounted: ' + AID);
end;

function TNyxLCLRenderer.ControlFor(const AID: TNyxText;
  AIdentity: TNyxIdentityKind): TControl;
begin
  Result := IdentityBinding(AID, AIdentity).FControl;
end;

function TNyxLCLRenderer.InputIdentity(AInput: TControl): TNyxText;
var
  LIndex: Integer;
begin
  Result := '';

  if AInput = nil then
  begin
    Exit;
  end;
  for LIndex := 0 to High(FBindings) do
  begin

    if FBindings[LIndex].FInput = AInput then
    begin
      Exit(FBindings[LIndex].FNode.ID);
    end;
  end;
end;

function TNyxLCLRenderer.ControlReference(AControl: TControl): TNyxControlRef;
var
  LIndex: Integer;
begin
  Result := Default(TNyxControlRef);

  if AControl = nil then
  begin
    Exit;
  end;
  for LIndex := 0 to High(FBindings) do
  begin

    if (FBindings[LIndex].FInput = AControl) or
      (FBindings[LIndex].FControl = AControl) then
    begin
      Exit(NyxControl(FBindings[LIndex].FNode.ID));
    end;

    if (FBindings[LIndex].FInput is TNyxCollectionStringGrid) and
      TNyxCollectionStringGrid(FBindings[LIndex].FInput).HasColumnComposite and
      TNyxCollectionStringGrid(FBindings[LIndex].FInput).OwnsColumnInput(AControl) then
    begin
      Exit(NyxControl(FBindings[LIndex].FNode.ID));
    end;
  end;
end;

function TNyxLCLRenderer.InputFor(const AID: TNyxText;
  AIdentity: TNyxIdentityKind): TControl;
begin
  Result := IdentityBinding(AID, AIdentity).FInput;
end;

function TNyxLCLRenderer.FocusFor(const AID: TNyxText;
  AIdentity: TNyxIdentityKind): TWinControl;
var
  LBinding: TNyxLCLBinding;
begin
  LBinding := IdentityBinding(AID, AIdentity);
  Result := nil;

  if LBinding.FInput is TWinControl then
  begin
    Exit(TWinControl(LBinding.FInput));
  end;

  if LBinding.FControl is TNyxLCLSplitView then
  begin
    Exit(TNyxLCLSplitView(LBinding.FControl).Grip);
  end;

  if (LBinding.FControl is TWinControl) and
    (NyxSupportsKeyboard(LBinding.FNode) or
    (LBinding.FCustom and TWinControl(LBinding.FControl).TabStop)) then
  begin
    Result := TWinControl(LBinding.FControl);
  end;
end;

procedure TNyxLCLRenderer.FocusWithoutScroll(const AID: TNyxText;
  AIdentity: TNyxIdentityKind);
var
  LFocus: TWinControl;
  LReplay: IFocusReplay;
  LFormer: IFocusReplayTarget;
begin
  FEvents.Scheduler.RequireUI;
  LFocus := FocusFor(AID, AIdentity);

  if (LFocus = nil) or not LFocus.CanFocus then
  begin
    Exit;
  end;
  { Resolve automatic/design identity before callbacks. The gate owns values
    only; its local lease remains valid even if SetFocus disposes this renderer. }
  LReplay := FFocusReplay;
  LFormer := LReplay.Target;
  LReplay.Target := TNyxLCLFocusReplayTarget.Create(
    IdentityBinding(AID, AIdentity).FNode.ID, NyxEventGeneration(FEvents));
  try
    LFocus.SetFocus;
  finally
    LReplay.Target := LFormer;
  end;
end;

function TNyxLCLRenderer.ViewportFor(const AID: TNyxText): TNyxViewportSnapshot;
var
  LGeneration: INyxLCLBindingGeneration;
  LEnteredFrame: INyxNativeGestureFrame;
  LBinding: TNyxLCLBinding;
  LControl: TControl;
begin
  LGeneration := FGeneration;
  Result := Default(TNyxViewportSnapshot);

  if not NativeGenerationCurrent(LGeneration) then
  begin
    Exit;
  end;
  LEnteredFrame := FPhysicalFrame;
  LEnteredFrame.Enter;
  try
    try

      FEvents.Scheduler.RequireUI;
      RequireNativeGeneration(LGeneration);
      LBinding := IdentityBinding(AID, niAutomatic);
      RequireNativeGeneration(LGeneration);
      LControl := LBinding.FControl;
      RequireNativeGeneration(LGeneration);

      if LBinding.FInput <> nil then
      begin
        LControl := LBinding.FInput;
        RequireNativeGeneration(LGeneration);
      end;

      if not NyxSupportsViewport(LBinding.FNode) or not (LControl is TWinControl) then
      begin
        raise ENyxModel.Create('This native control has no declared viewport');
      end;
      RequireNativeGeneration(LGeneration);
      Result := CaptureNyxViewport(TWinControl(LControl));
      RequireNativeGeneration(LGeneration);
    except
      on ENyxNativeGenerationRetired do
      begin
        Result := Default(TNyxViewportSnapshot);
      end;
    end;
  finally
    LEnteredFrame.Leave;
  end;
end;


function TNyxLCLRenderer.SizeFor(const AID: TNyxText;
  AIdentity: TNyxIdentityKind): TNyxResizeSize;
var
  LBinding: TNyxLCLBinding;
begin
  FEvents.Scheduler.RequireUI;
  LBinding := IdentityBinding(AID, AIdentity);

  if not LBinding.FLogicalBox.Defined then
  begin
    raise ENyxModel.Create('Control size requires admitted logical layout');
  end;
  Result := NyxResizeSize(LBinding.FLogicalBox.Width, LBinding.FLogicalBox.Height);
end;

function TNyxLCLRenderer.DropFrameFor(const AID: TNyxText): TNyxDropFrame;
var
  LBinding: TNyxLCLBinding;
  LOrigin: TPoint;
  LParent: TNyxControlRef;
begin
  FEvents.Scheduler.RequireUI;
  Result := Default(TNyxDropFrame);
  LBinding := IdentityBinding(AID, niRuntime);

  if not LBinding.FLogicalBox.Defined or (LBinding.FLogicalBox.Width <= 0) or
    (LBinding.FLogicalBox.Height <= 0) then
  begin
    Exit;
  end;
  LOrigin := LBinding.FControl.Parent.ClientToScreen(
    Point(LBinding.FControl.Left, LBinding.FControl.Top));
  Dec(LOrigin.X, LBinding.FPlacement.ContentOffsetX);
  Dec(LOrigin.Y, LBinding.FPlacement.ContentOffsetY);
  LParent := Default(TNyxControlRef);

  if LBinding.FNode.Parent <> nil then
  begin
    LParent := NyxControl(LBinding.FNode.Parent.ID);
  end;
  Result := NyxDropFrame(NyxGuideBox(LOrigin.X, LOrigin.Y,
    LBinding.FLogicalBox.Width, LBinding.FLogicalBox.Height),
    LBinding.FLogicalBox.Width, LBinding.FLogicalBox.Height, LParent,
    NyxDesignerParentAxis(LBinding.FNode), NyxDesignerTarget(LBinding.FNode).Container);
end;

function TNyxLCLRenderer.ScreenPointFor(const AID: TNyxText;
  const APointer: TNyxPointerSnapshot): TNyxResizePoint;
var
  LOrigin: TPoint;
  LBinding: TNyxLCLBinding;
begin
  FEvents.Scheduler.RequireUI;

  if not APointer.HasPosition then
  begin
    raise ENyxModel.Create('Pointer mapping requires actual position');
  end;
  LBinding := IdentityBinding(AID, niAutomatic);
  LOrigin := LBinding.FControl.Parent.ClientToScreen(
    Point(LBinding.FControl.Left, LBinding.FControl.Top));
  Dec(LOrigin.X, LBinding.FPlacement.ContentOffsetX);
  Dec(LOrigin.Y, LBinding.FPlacement.ContentOffsetY);
  Result := NyxResizePoint(LOrigin.X + APointer.X, LOrigin.Y + APointer.Y);
end;

function TNyxLCLRenderer.LogicalPointFor(const AID: TNyxText;
  const AScreen: TNyxResizePoint; AIdentity: TNyxIdentityKind): TNyxResizePoint;
begin
  FEvents.Scheduler.RequireUI;

  if not AScreen.Defined or not IdentityBinding(AID, AIdentity).FLogicalBox.Defined then
  begin
    raise ENyxModel.Create('Logical pointer mapping requires mounted geometry');
  end;
  Result := AScreen;
end;

function TNyxLCLRenderer.AlignmentFor(const AID: TNyxText;
  AIdentity: TNyxIdentityKind): TNyxAlignmentContext;
var
  LBinding, LParent, LPeer: TNyxLCLBinding;
  LIndex: Integer;
  LWidth, LHeight: Integer;
  LFrame: TRect;

  function BoxFor(ABinding: TNyxLCLBinding): TNyxGuideBox;
  begin
    Result := NyxGuideBox(ABinding.FLogicalBox.X, ABinding.FLogicalBox.Y,
      ABinding.FLogicalBox.Width, ABinding.FLogicalBox.Height);
  end;

begin
  FEvents.Scheduler.RequireUI;
  Result := Default(TNyxAlignmentContext);
  LBinding := IdentityBinding(AID, AIdentity);
  LParent := LBinding.FLogicalParent;

  if (LParent = nil) or not LBinding.FLogicalBox.Defined or
    not LParent.FLogicalBox.Defined then
  begin
    Exit;
  end;
  LWidth := LParent.FLogicalBox.Width;
  LHeight := LParent.FLogicalBox.Height;

  if LParent.FControl is TCustomGroupBox then
  begin
    LFrame := ContentInsets(LParent.FNode);
    LWidth := Max(0, LWidth - LFrame.Left - LFrame.Right);
    LHeight := Max(0, LHeight - LFrame.Top - LFrame.Bottom);
  end
  else if LParent.FControl is TWinControl then
  begin
    { Native decorations consume client area even when the face is projected
      into a smaller physical window. Subtract the decoration, not clipping. }
    LWidth := Max(0, LWidth - (LParent.FControl.Width - TWinControl(LParent.FControl).ClientWidth));
    LHeight := Max(0, LHeight - (LParent.FControl.Height - TWinControl(LParent.FControl).ClientHeight));
  end;
  Result := NyxAlignmentContext(BoxFor(LBinding),
    NyxGuideBox(0, 0, LWidth, LHeight),
    NyxControl(LParent.FNode.ID)).Positions(LParent.FNode.Prop('layout') = 'absolute');
  for LIndex := 0 to High(FBindings) do
  begin
    LPeer := FBindings[LIndex];

    if (LPeer = LBinding) or (LPeer.FLogicalParent <> LParent) or
      not LPeer.FControl.Visible or not LPeer.FLogicalBox.Defined or
      not NyxInteractionPolicy(LPeer.FNode).Visible or
      (LPeer.FLogicalBox.Width = 0) or (LPeer.FLogicalBox.Height = 0) or
      (LPeer.FLogicalBox.Right <= 0) or (LPeer.FLogicalBox.X >= LWidth) or
      (LPeer.FLogicalBox.Bottom <= 0) or (LPeer.FLogicalBox.Y >= LHeight) or
      (FVirtualLayout and not LPeer.FPlacement.HasArea) then
    begin
      Continue;
    end;

    if Result.PeerCount = NyxMaximumGuidePeers then
    begin
      Break;
    end;
    Result := Result.Peer(NyxControl(LPeer.FNode.ID), BoxFor(LPeer));
  end;
end;

procedure TNyxLCLRenderer.ViewportChanged(const AOriginID: TNyxText;
  const AViewport: TNyxViewportSnapshot);
var
  LGeneration: INyxLCLBindingGeneration;
  LEnteredFrame: INyxNativeGestureFrame;
  LBinding: TNyxLCLBinding;
  LDispatch: TNyxDispatch;
begin
  LGeneration := FGeneration;

  if (LGeneration = nil) or not NativeGenerationCurrent(LGeneration) then
  begin
    Exit;
  end;
  LEnteredFrame := FPhysicalFrame;
  LEnteredFrame.Enter;
  try
    try

      if FUpdating or FDesignMode then
      begin
        Exit;
      end;
      LBinding := IdentityBinding(AOriginID, niRuntime);
      RequireNativeGeneration(LGeneration);
      LDispatch := FLiveBindings.Signal(LBinding.FNode, ntScroll);
      RequireNativeGeneration(LGeneration);
      LDispatch.Info.HasViewport := True;
      LDispatch.Info.Viewport := AViewport;
      Emit(LBinding.FNode, LDispatch);
      RequireNativeGeneration(LGeneration);
      { The borrowed binding/renderer may have ended inside Emit. }
    except
      on ENyxNativeGenerationRetired do
      begin
        { The original generation retired; only retained locals may unwind. }
      end;
    end;
  finally
    LEnteredFrame.Leave;
  end;
end;


procedure TNyxLCLRenderer.SetDesignerInput(const AValue: TNyxDesignerInput);
begin

  if FRoot <> nil then
  begin
    raise ENyxModel.Create('Configure designer input before mounting its view');
  end;
  FDesignerInput := AValue;
end;

procedure TNyxLCLRenderer.GestureFailed(const AOriginID, AReason: TNyxText);
var
  LHandler: TNyxGestureFailure;
begin
  FLastGestureError := AReason;
  LHandler := FOnGestureFailure;

  if Assigned(LHandler) then
  begin
    LHandler(AOriginID, AReason);
  end;
end;

procedure TNyxLCLRenderer.CaptureChanged(const AOriginID: TNyxText; ATrigger: TNyxTrigger);
var
  LGeneration: INyxLCLBindingGeneration;
  LEnteredFrame: INyxNativeGestureFrame;
  LBinding: TNyxLCLBinding;
  LDispatch: TNyxDispatch;
begin
  LGeneration := FGeneration;

  if (LGeneration = nil) or not NativeGenerationCurrent(LGeneration) then
  begin
    Exit;
  end;
  LEnteredFrame := FPhysicalFrame;
  LEnteredFrame.Enter;
  try
    try

      if ATrigger in [ntAfterEnter, ntAfterExit] then
      begin
        { Update the transition baseline even without subscribers. Do not project
          native focus as a pointer signal or invoke the creator's logical slot. }
        LBinding := IdentityBinding(AOriginID, niRuntime);
        RequireNativeGeneration(LGeneration);
        LBinding.ObserveFocus(ATrigger);
        RequireNativeGeneration(LGeneration);
        Exit;
      end;

      if ATrigger = ntPointerCaptureLost then
      begin
        QueueContent;
        RequireNativeGeneration(LGeneration);
      end;

      if FUpdating or FDesignMode or not FEvents.HasSubscribers(ATrigger) then
      begin
        RequireNativeGeneration(LGeneration);
        Exit;
      end;
      RequireNativeGeneration(LGeneration);
      LBinding := IdentityBinding(AOriginID, niRuntime);
      RequireNativeGeneration(LGeneration);

      if (LBinding.FCollectionInputLifetime <> nil) and
        not LBinding.FCollectionInputLifetime.Ready then
      begin
        RequireNativeGeneration(LGeneration);
        { Capture is outside the grid's LCL procedure. Actual Windows input is
          intercepted earlier at HWND level; this also refuses direct LCL capture
          injection while an enclosing embedded candidate remains unaccepted. }
        Exit;
      end;
      RequireNativeGeneration(LGeneration);

      if not NyxInteractionPolicy(LBinding.FNode).CanIssueCommand then
      begin
        RequireNativeGeneration(LGeneration);
        Exit;
      end;
      RequireNativeGeneration(LGeneration);
      LDispatch := FLiveBindings.Signal(LBinding.FNode, ATrigger);
      RequireNativeGeneration(LGeneration);
      LDispatch.Info.HasPointer := True;
      LDispatch.Info.Pointer.Kind := npiMouse;
      { Native capture belongs to the single LCL mouse stream (ID zero). This is an
        observation, without invented pressure, coordinates or response authority. }
      Emit(LBinding.FNode, LDispatch);
      RequireNativeGeneration(LGeneration);
    except
      on ENyxNativeGenerationRetired do
      begin
        { The original generation retired; only retained locals may unwind. }
      end;
    end;
  finally
    LEnteredFrame.Leave;
  end;
end;


procedure TNyxLCLBinding.StartDrag(ASender: TObject; var ADragObject: TDragObject);
var
  LGeneration: INyxLCLBindingGeneration;
  LEnteredFrame: INyxNativeGestureFrame;
  LPrevious: TStartDragEvent;
  LEvents: INyxEvents;
  LRevision: Integer;
  LFrame: INyxNativeGestureFrame;
  LRenderer: TNyxLCLRenderer;
  LDrag: TNyxNativeDragObject;
  LResult: TNyxGestureResult;
  LOriginID: TNyxText;
  LPolicy: TNyxInteractionPolicy;
  LSource: Boolean;
begin
  LGeneration := GenerationLease;

  if (LGeneration = nil) or not NativeGenerationCurrent(LGeneration) then
  begin
    Exit;
  end;
  LEnteredFrame := LGeneration.PhysicalFrame;
  LEnteredFrame.Enter;
  try
    try

      if FRenderer.FDesignMode then
      begin
        Exit;
      end;
      LPrevious := FPointerHooks[PointerSlot(ASender)].StartDrag;
      RequireNativeGeneration(LGeneration);
      LRenderer := FRenderer;
      LEvents := LRenderer.FEvents;
      RequireNativeGeneration(LGeneration);
      LRevision := LEvents.ViewRevision;
      RequireNativeGeneration(LGeneration);
      LFrame := LRenderer.FPhysicalFrame;
      RequireNativeGeneration(LGeneration);
      LOriginID := FNode.ID;
      RequireNativeGeneration(LGeneration);
      LPolicy := NyxInteractionPolicy(FNode);
      RequireNativeGeneration(LGeneration);
      LSource := FNode.Prop('drag-source') = 'true';
      RequireNativeGeneration(LGeneration);
      LFrame.Enter;
      RequireNativeGeneration(LGeneration);
      try

        if Assigned(LPrevious) then
        begin
          RequireNativeGeneration(LGeneration);
          LPrevious(ASender, ADragObject);
          RequireNativeGeneration(LGeneration);
        end;
        RequireNativeGeneration(LGeneration);

        if LEvents.ViewRevision <> LRevision then
        begin
          RequireNativeGeneration(LGeneration);
          LFrame.AbortDrag(TControl(ASender));
          RequireNativeGeneration(LGeneration);
          Exit;
        end;
        RequireNativeGeneration(LGeneration);

        if not LSource then
        begin
          Exit;
        end;

        if ADragObject <> nil then
        begin
          LFrame.AbortDrag(TControl(ASender));
          RequireNativeGeneration(LGeneration);
          LRenderer.GestureFailed(LOriginID,
            'A custom LCL drag object requires its own explicit Nyx transfer adapter');
                                                                                     RequireNativeGeneration(LGeneration);
          Exit;
        end;
        { Supply an object even when refused. Otherwise LCL silently substitutes an
          untyped default object. Cancellation waits until its constructor returns. }
        LDrag := TNyxNativeDragObject.Create(TControl(ASender));
        RequireNativeGeneration(LGeneration);
        ADragObject := LDrag;
        FActiveDrag := LDrag;
        LDrag.Events := LEvents;
        LDrag.Revision := LRevision;
        LDrag.SourceID := LOriginID;
        LDrag.Transfer := NyxTransferFromData(NyxArray([]), True);
        RequireNativeGeneration(LGeneration);
        LResult := DragEvent(ASender, LDrag, ntDragStart, ndpStart, 0, 0);
        RequireNativeGeneration(LGeneration);

        if LEvents.ViewRevision <> LRevision then
        begin
          RequireNativeGeneration(LGeneration);
          LFrame.AbortDrag(TControl(ASender));
          RequireNativeGeneration(LGeneration);
          Exit;
        end;
        RequireNativeGeneration(LGeneration);

        if not LResult.Offered or not LPolicy.CanIssueCommand or
          (LPolicy.ReadOnly and (ndoMove in LResult.Allowed)) then
        begin
          RequireNativeGeneration(LGeneration);
          LFrame.AbortDrag(TControl(ASender));
          RequireNativeGeneration(LGeneration);
          LRenderer.GestureFailed(LOriginID,
            'The native drag source did not offer an admitted sequential transfer');
                                                                                  RequireNativeGeneration(LGeneration);
          Exit;
        end;
        RequireNativeGeneration(LGeneration);
        LDrag.Transfer := LResult.Transfer;
        RequireNativeGeneration(LGeneration);
        LDrag.Allowed := LResult.Allowed;
        RequireNativeGeneration(LGeneration);
        LDrag.Offered := True;
        LRenderer.FLastGestureError := '';
      finally
        LFrame.Leave;
      end;
      RequireNativeGeneration(LGeneration);
    except
      on ENyxNativeGenerationRetired do
      begin
        { The original generation retired; only retained locals may unwind. }
      end;
    end;
  finally
    LEnteredFrame.Leave;
  end;
end;


procedure TNyxLCLBinding.EndDrag(ASender, ATarget: TObject; AX, AY: Integer);
var
  LGeneration: INyxLCLBindingGeneration;
  LEnteredFrame: INyxNativeGestureFrame;
  LPrevious: TEndDragEvent;
  LDrag: TDragObject;
  LEvents: INyxEvents;
  LRevision: Integer;
  LFrame: INyxNativeGestureFrame;
begin
  LGeneration := GenerationLease;

  if (LGeneration = nil) or not NativeGenerationCurrent(LGeneration) then
  begin
    Exit;
  end;
  LEnteredFrame := LGeneration.PhysicalFrame;
  LEnteredFrame.Enter;
  try
    try

      if FRenderer.FDesignMode then
      begin
        Exit;
      end;
      LPrevious := FPointerHooks[PointerSlot(ASender)].EndDrag;
      RequireNativeGeneration(LGeneration);
      LEvents := FRenderer.FEvents;
      RequireNativeGeneration(LGeneration);
      LRevision := LEvents.ViewRevision;
      RequireNativeGeneration(LGeneration);
      LFrame := FRenderer.FPhysicalFrame;
      RequireNativeGeneration(LGeneration);
      LDrag := FActiveDrag;
      FActiveDrag := nil;
      RequireNativeGeneration(LGeneration);
      LFrame.Enter;
      RequireNativeGeneration(LGeneration);
      try

        if Assigned(LPrevious) then
        begin
          RequireNativeGeneration(LGeneration);
          LPrevious(ASender, ATarget, AX, AY);
          RequireNativeGeneration(LGeneration);
        end;
        RequireNativeGeneration(LGeneration);

        if (LEvents.ViewRevision = LRevision) and
          (LDrag is TNyxNativeDragObject) and TNyxNativeDragObject(LDrag).Live then
        begin
          RequireNativeGeneration(LGeneration);
          DragEvent(ASender, LDrag, ntDragEnd, ndpEnd, AX, AY);
          RequireNativeGeneration(LGeneration);
        end;
        RequireNativeGeneration(LGeneration);
      finally
        LFrame.Leave;
      end;
      RequireNativeGeneration(LGeneration);
    except
      on ENyxNativeGenerationRetired do
      begin
        { The original generation retired; only retained locals may unwind. }
      end;
    end;
  finally
    LEnteredFrame.Leave;
  end;
end;


procedure TNyxLCLBinding.DragOver(ASender, ASource: TObject; AX, AY: Integer;
  AState: TDragState; var AAccept: Boolean);
var
  LGeneration: INyxLCLBindingGeneration;
  LEnteredFrame: INyxNativeGestureFrame;
  LPrevious: TDragOverEvent;
  LEvents: INyxEvents;
  LRevision: Integer;
  LFrame: INyxNativeGestureFrame;
  LDrag: TNyxNativeDragObject;
  LOriginID: TNyxText;
  LResult: TNyxGestureResult;
  LTrigger: TNyxTrigger;
  LPhase: TNyxDragPhase;
  LTarget: Boolean;
begin
  LGeneration := GenerationLease;

  if (LGeneration = nil) or not NativeGenerationCurrent(LGeneration) then
  begin
    AAccept := False;
    Exit;
  end;
  LEnteredFrame := LGeneration.PhysicalFrame;
  LEnteredFrame.Enter;
  try
    try

      if FRenderer.FDesignMode and not FRenderer.FDesignerInput.DropEnabled then
      begin
        RequireNativeGeneration(LGeneration);
        AAccept := False;
        Exit;
      end;
      RequireNativeGeneration(LGeneration);
      LPrevious := FPointerHooks[PointerSlot(ASender)].DragOver;
      RequireNativeGeneration(LGeneration);
      LEvents := FRenderer.FEvents;
      RequireNativeGeneration(LGeneration);
      LRevision := LEvents.ViewRevision;
      RequireNativeGeneration(LGeneration);
      LFrame := FRenderer.FPhysicalFrame;
      RequireNativeGeneration(LGeneration);
      LOriginID := FNode.ID;
      RequireNativeGeneration(LGeneration);
      LTarget := FRenderer.FDesignMode or (FNode.Prop('drop-target') = 'true');
      RequireNativeGeneration(LGeneration);
      LFrame.Enter;
      RequireNativeGeneration(LGeneration);
      try

        if not FRenderer.FDesignMode and Assigned(LPrevious) then
        begin
          RequireNativeGeneration(LGeneration);
          LPrevious(ASender, ASource, AX, AY, AState, AAccept);
          RequireNativeGeneration(LGeneration);
        end;
        RequireNativeGeneration(LGeneration);

        if LEvents.ViewRevision <> LRevision then
        begin
          RequireNativeGeneration(LGeneration);
          AAccept := False;
          Exit;
        end;
        RequireNativeGeneration(LGeneration);

        if not LTarget then
        begin
          Exit;
        end;
        AAccept := False;

        if not (ASource is TNyxNativeDragObject) then
        begin
          Exit;
        end;
        RequireNativeGeneration(LGeneration);
        LDrag := TNyxNativeDragObject(ASource);
        RequireNativeGeneration(LGeneration);

        if not LDrag.Live then
        begin
          RequireNativeGeneration(LGeneration);
          Exit;
        end;
        RequireNativeGeneration(LGeneration);
        case AState of
          dsDragEnter:
            begin
              LTrigger := ntDragEnter;
              LPhase := ndpEnter;
            end;
          dsDragLeave:
            begin
              LTrigger := ntDragExit;
              LPhase := ndpExit;
            end;
        else
          begin
            LTrigger := ntDragOver;
            LPhase := ndpOver;
          end;
        end;
        { LCL asks DragLeave for the final acceptance before delivering DragDrop.
          Preserve the last hover agreement, while exit has no response window. }
        AAccept := (AState = dsDragLeave) and (LDrag.HoverID = LOriginID) and
          (LDrag.HoverOperation <> ndoNone);
          RequireNativeGeneration(LGeneration);
        LResult := DragEvent(ASender, LDrag, LTrigger, LPhase, AX, AY);
        RequireNativeGeneration(LGeneration);

        if (LEvents.ViewRevision <> LRevision) or not LDrag.Live then
        begin
          RequireNativeGeneration(LGeneration);
          AAccept := False;
          Exit;
        end;
        RequireNativeGeneration(LGeneration);

        if AState <> dsDragLeave then
        begin
          LDrag.HoverID := LOriginID;
          LDrag.HoverOperation := ndoNone;

          if LResult.Accepted then
          begin
            RequireNativeGeneration(LGeneration);
            LDrag.HoverOperation := LResult.Operation;
            RequireNativeGeneration(LGeneration);
          end;
          RequireNativeGeneration(LGeneration);
          AAccept := LDrag.HoverOperation <> ndoNone;
          RequireNativeGeneration(LGeneration);
        end;
      finally
        LFrame.Leave;
      end;
      RequireNativeGeneration(LGeneration);
    except
      on ENyxNativeGenerationRetired do
      begin
        AAccept := False;
      end;
    end;
  finally
    LEnteredFrame.Leave;
  end;
end;


procedure TNyxLCLBinding.DragDrop(ASender, ASource: TObject; AX, AY: Integer);
var
  LGeneration: INyxLCLBindingGeneration;
  LEnteredFrame: INyxNativeGestureFrame;
  LPrevious: TDragDropEvent;
  LEvents: INyxEvents;
  LRevision: Integer;
  LFrame: INyxNativeGestureFrame;
  LDrag: TNyxNativeDragObject;
  LResult: TNyxGestureResult;
  LTarget: Boolean;
begin
  LGeneration := GenerationLease;

  if (LGeneration = nil) or not NativeGenerationCurrent(LGeneration) then
  begin
    Exit;
  end;
  LEnteredFrame := LGeneration.PhysicalFrame;
  LEnteredFrame.Enter;
  try
    try

      if FRenderer.FDesignMode and not FRenderer.FDesignerInput.DropEnabled then
      begin
        RequireNativeGeneration(LGeneration);
        Exit;
      end;
      RequireNativeGeneration(LGeneration);
      LPrevious := FPointerHooks[PointerSlot(ASender)].DragDrop;
      RequireNativeGeneration(LGeneration);
      LEvents := FRenderer.FEvents;
      RequireNativeGeneration(LGeneration);
      LRevision := LEvents.ViewRevision;
      RequireNativeGeneration(LGeneration);
      LFrame := FRenderer.FPhysicalFrame;
      RequireNativeGeneration(LGeneration);
      LTarget := FRenderer.FDesignMode or (FNode.Prop('drop-target') = 'true');
      RequireNativeGeneration(LGeneration);
      LFrame.Enter;
      RequireNativeGeneration(LGeneration);
      try

        if not LTarget then
        begin

          if Assigned(LPrevious) then
          begin
            RequireNativeGeneration(LGeneration);
            LPrevious(ASender, ASource, AX, AY);
            RequireNativeGeneration(LGeneration);
          end;
          RequireNativeGeneration(LGeneration);
          Exit;
        end;

        if not (ASource is TNyxNativeDragObject) then
        begin
          Exit;
        end;
        RequireNativeGeneration(LGeneration);
        LDrag := TNyxNativeDragObject(ASource);
        RequireNativeGeneration(LGeneration);

        if not LDrag.Live then
        begin
          RequireNativeGeneration(LGeneration);
          Exit;
        end;
        RequireNativeGeneration(LGeneration);
        LResult := DragEvent(ASender, LDrag, ntDrop, ndpDrop, AX, AY);
        RequireNativeGeneration(LGeneration);

        if (LEvents.ViewRevision = LRevision) and LDrag.Live and LResult.Accepted then
        begin
          RequireNativeGeneration(LGeneration);
          LDrag.Operation := LResult.Operation;
          RequireNativeGeneration(LGeneration);
        end;
        RequireNativeGeneration(LGeneration);
        { Accepted operations never mutate the source or target model implicitly.
          The application's typed callback owns its copy/move/link operation. }
      finally
        LFrame.Leave;
      end;
      RequireNativeGeneration(LGeneration);
    except
      on ENyxNativeGenerationRetired do
      begin
        { The original generation retired; only retained locals may unwind. }
      end;
    end;
  finally
    LEnteredFrame.Leave;
  end;
end;


function TNyxLCLBinding.DragEvent(ASender: TObject; ADragObject: TDragObject;
  ATrigger: TNyxTrigger; APhase: TNyxDragPhase; AX, AY: Integer): TNyxGestureResult;
var
  LGeneration: INyxLCLBindingGeneration;
  LEnteredFrame: INyxNativeGestureFrame;
  LDrag: TNyxNativeDragObject;
  LDispatch: TNyxDispatch;
  LCapabilities: TNyxGestureCapabilities;
  LDecision: INyxGestureDecision;
  LPosition: TPoint;
  LOperation: TNyxDropOperation;
  LDesignerTarget: TNyxDesignerTarget;
begin
  LGeneration := GenerationLease;
  Result := Default(TNyxGestureResult);

  if (LGeneration = nil) or not NativeGenerationCurrent(LGeneration) then
  begin
    Exit;
  end;
  LEnteredFrame := LGeneration.PhysicalFrame;
  LEnteredFrame.Enter;
  try
    try

      Result := Default(TNyxGestureResult);
      RequireNativeGeneration(LGeneration);

      if FRenderer.FUpdating or
        (FRenderer.FDesignMode and ((APhase in [ndpStart, ndpDrag, ndpEnd]) or
          not FRenderer.FDesignerInput.DropEnabled)) or
        (not FRenderer.FDesignMode and not NyxInteractionPolicy(FNode).CanIssueCommand) or
        not (ADragObject is TNyxNativeDragObject) then
      begin
        RequireNativeGeneration(LGeneration);
        Exit;
      end;
      RequireNativeGeneration(LGeneration);
      LDrag := TNyxNativeDragObject(ADragObject);
      RequireNativeGeneration(LGeneration);
      LCapabilities := [];

      if APhase = ndpStart then
      begin
        Include(LCapabilities, ngcOfferDrag);
      end
      else if (APhase in [ndpEnter, ndpOver, ndpDrop]) and
        (FRenderer.FDesignMode or NyxInteractionPolicy(FNode).CanEditValue) then
      begin
        Include(LCapabilities, ngcAcceptDrop);
      end;
      LOperation := LDrag.HoverOperation;
      RequireNativeGeneration(LGeneration);

      if APhase = ndpEnd then
      begin
        LOperation := LDrag.Operation;
        RequireNativeGeneration(LGeneration);
      end;

      if FRenderer.FDesignMode then
      begin
        LDispatch := Default(TNyxDispatch);
        RequireNativeGeneration(LGeneration);
        LDispatch.Info := NyxDesignerDragEvent(FNode, ATrigger);
        RequireNativeGeneration(LGeneration);
      end
      else
      begin
        LDispatch := FRenderer.FLiveBindings.Signal(FNode, ATrigger);
        RequireNativeGeneration(LGeneration);
      end;
      LDispatch.Info.HasDrag := True;
      LDispatch.Info.Drag := NyxDragSnapshot(APhase, LDrag.Transfer,
        LDrag.Allowed, LOperation, LDrag.SourceID, LCapabilities <> []);
        RequireNativeGeneration(LGeneration);
      LDispatch.Info.HasPointer := True;
      LDispatch.Info.Pointer.Kind := npiMouse;
      LDispatch.Info.Pointer.HasPosition := not (APhase in [ndpStart, ndpEnd]);
      RequireNativeGeneration(LGeneration);

      if LDispatch.Info.Pointer.HasPosition then
      begin
        RequireNativeGeneration(LGeneration);
        { Portable positions belong to the outer face, including native decorations.
          Use the parent's client coordinates for that outer origin; a groupbox's
          own ClientToScreen(0,0) begins below its caption and inside its frame. }
        LPosition := TControl(ASender).ClientToScreen(Point(AX, AY));
        RequireNativeGeneration(LGeneration);
        LPosition := FControl.Parent.ScreenToClient(LPosition);
        RequireNativeGeneration(LGeneration);
        Dec(LPosition.X, FControl.Left);
        Dec(LPosition.Y, FControl.Top);
        LDispatch.Info.Pointer.X := Double(LPosition.X) + FPlacement.ContentOffsetX;
        RequireNativeGeneration(LGeneration);
        LDispatch.Info.Pointer.Y := Double(LPosition.Y) + FPlacement.ContentOffsetY;
        RequireNativeGeneration(LGeneration);
      end;
      RequireNativeGeneration(LGeneration);
      LDecision := NewNyxGestureDecision(LCapabilities, LDrag.Allowed);
      RequireNativeGeneration(LGeneration);
      try

        if FRenderer.FDesignMode then
        begin

          if Assigned(FRenderer.FOnDesignerGesture) then
          begin
            RequireNativeGeneration(LGeneration);
            LDesignerTarget := NyxDesignerTarget(FNode).WithFrame(FRenderer.DropFrameFor(FNode.ID));
            RequireNativeGeneration(LGeneration);
            FRenderer.FOnDesignerGesture(LDesignerTarget, LDispatch.Info, LDecision);
            RequireNativeGeneration(LGeneration);
          end;
          RequireNativeGeneration(LGeneration);
        end
        else
        begin
          DispatchNyxGesture(FRenderer.FEvents, FNode, LDispatch, LDecision);
          RequireNativeGeneration(LGeneration);
        end;
      finally
        Result := LDecision.Seal;
      end;
      RequireNativeGeneration(LGeneration);
    except
      on ENyxNativeGenerationRetired do
      begin
        Result := Default(TNyxGestureResult);
      end;
    end;
  finally
    LEnteredFrame.Leave;
  end;
end;


procedure TNyxLCLBinding.DisconnectControl(AControl: TControl);
begin

  if AControl = nil then
  begin
    Exit;
  end;

  if AControl is TNyxLCLButton then
  begin
    TNyxLCLButton(AControl).DisconnectAccessibility;
  end;
  TNyxControlAccess(AControl).OnClick := nil;
  TNyxControlAccess(AControl).OnDblClick := nil;
  TNyxControlAccess(AControl).OnMouseDown := nil;
  TNyxControlAccess(AControl).OnMouseUp := nil;
  TNyxControlAccess(AControl).OnMouseMove := nil;
  TNyxControlAccess(AControl).OnMouseEnter := nil;
  TNyxControlAccess(AControl).OnMouseLeave := nil;
  TNyxControlAccess(AControl).OnContextPopup := nil;
  TNyxControlAccess(AControl).OnMouseWheel := nil;
  TNyxControlAccess(AControl).OnMouseWheelHorz := nil;
  TNyxControlAccess(AControl).OnStartDrag := nil;
  TNyxControlAccess(AControl).OnEndDrag := nil;
  TNyxControlAccess(AControl).OnDragOver := nil;
  TNyxControlAccess(AControl).OnDragDrop := nil;

  if AControl is TWinControl then
  begin
    TNyxWinControlAccess(AControl).OnEnter := nil;
    TNyxWinControlAccess(AControl).OnExit := nil;
    TNyxWinControlAccess(AControl).OnKeyDown := nil;
    TNyxWinControlAccess(AControl).OnKeyUp := nil;
  end;

  if AControl is TCustomEdit then
  begin
    TEdit(AControl).OnChange := nil;
    TEdit(AControl).OnEditingDone := nil;
  end
  else if AControl is TComboBox then
  begin
    TComboBox(AControl).OnChange := nil;
  end
  else if AControl is TCheckBox then
  begin
    TCheckBox(AControl).OnChange := nil;
  end
  else if AControl is TRadioButton then
  begin
    TRadioButton(AControl).OnChange := nil;
  end
  else if AControl is TTrackBar then
  begin
    TTrackBar(AControl).OnChange := nil;
  end;

  if AControl is TNyxLCLSplitView then
  begin
    TNyxLCLSplitView(AControl).OnLayout := nil;
    TNyxLCLSplitView(AControl).OnChanged := nil;
  end;

  if AControl is TNyxLCLTabHost then
  begin
    TNyxLCLTabHost(AControl).OnChange := nil;
    TNyxLCLTabHost(AControl).OnAdmit := nil;
  end;
end;

procedure TNyxLCLBinding.AttachPointer(AControl: TControl; ASlot: Integer);
begin
  FPointerHooks[ASlot].DoubleClick := TNyxControlAccess(AControl).OnDblClick;
  TNyxControlAccess(AControl).OnDblClick := DoubleClick;
  FPointerHooks[ASlot].Down := TNyxControlAccess(AControl).OnMouseDown;
  TNyxControlAccess(AControl).OnMouseDown := PointerDown;
  FPointerHooks[ASlot].Up := TNyxControlAccess(AControl).OnMouseUp;
  TNyxControlAccess(AControl).OnMouseUp := PointerUp;
  FPointerHooks[ASlot].Move := TNyxControlAccess(AControl).OnMouseMove;
  TNyxControlAccess(AControl).OnMouseMove := PointerMove;
  FPointerHooks[ASlot].Enter := TNyxControlAccess(AControl).OnMouseEnter;
  TNyxControlAccess(AControl).OnMouseEnter := PointerEnter;
  FPointerHooks[ASlot].Leave := TNyxControlAccess(AControl).OnMouseLeave;
  TNyxControlAccess(AControl).OnMouseLeave := PointerExit;
  FPointerHooks[ASlot].ContextMenu := TNyxControlAccess(AControl).OnContextPopup;
  TNyxControlAccess(AControl).OnContextPopup := ContextMenu;
  FPointerHooks[ASlot].Wheel := TNyxControlAccess(AControl).OnMouseWheel;
  TNyxControlAccess(AControl).OnMouseWheel := Wheel;
  FPointerHooks[ASlot].WheelHorz := TNyxControlAccess(AControl).OnMouseWheelHorz;
  TNyxControlAccess(AControl).OnMouseWheelHorz := WheelHorz;
  FPointerHooks[ASlot].StartDrag := TNyxControlAccess(AControl).OnStartDrag;
  TNyxControlAccess(AControl).OnStartDrag := StartDrag;
  FPointerHooks[ASlot].EndDrag := TNyxControlAccess(AControl).OnEndDrag;
  TNyxControlAccess(AControl).OnEndDrag := EndDrag;
  FPointerHooks[ASlot].DragOver := TNyxControlAccess(AControl).OnDragOver;
  TNyxControlAccess(AControl).OnDragOver := DragOver;
  FPointerHooks[ASlot].DragDrop := TNyxControlAccess(AControl).OnDragDrop;
  TNyxControlAccess(AControl).OnDragDrop := DragDrop;
end;

function TNyxLCLBinding.PointerSlot(ASender: TObject): Integer;
begin
  Result := 0;

  if (ASender = FInput) and (FInput <> FControl) then
  begin
    Result := 1;
  end;
end;

procedure TNyxLCLBinding.Wheel(ASender: TObject; AShift: TShiftState;
  ADelta: Integer; APosition: TPoint; var AHandled: Boolean);
var
  LGeneration: INyxLCLBindingGeneration;
  LEnteredFrame: INyxNativeGestureFrame;
  LPrevious: TMouseWheelEvent;
  LEvents: INyxEvents;
  LRevision: Integer;
begin
  LGeneration := GenerationLease;

  if (LGeneration = nil) or not NativeGenerationCurrent(LGeneration) then
  begin
    AHandled := True;
    Exit;
  end;
  LEnteredFrame := LGeneration.PhysicalFrame;
  LEnteredFrame.Enter;
  try
    try

      if FRenderer.FDesignMode then
      begin
        Exit;
      end;
      LPrevious := FPointerHooks[PointerSlot(ASender)].Wheel;
      RequireNativeGeneration(LGeneration);
      LEvents := FRenderer.FEvents;
      RequireNativeGeneration(LGeneration);
      LRevision := LEvents.ViewRevision;
      RequireNativeGeneration(LGeneration);
      WheelEvent(ASender, AShift, ADelta, False, AHandled);
      RequireNativeGeneration(LGeneration);

      if not AHandled and (LEvents.ViewRevision = LRevision) and Assigned(LPrevious) then
      begin
        RequireNativeGeneration(LGeneration);
        LPrevious(ASender, AShift, ADelta, APosition, AHandled);
        RequireNativeGeneration(LGeneration);
      end;
      RequireNativeGeneration(LGeneration);
    except
      on ENyxNativeGenerationRetired do
      begin
        AHandled := True;
      end;
    end;
  finally
    LEnteredFrame.Leave;
  end;
end;


procedure TNyxLCLBinding.WheelHorz(ASender: TObject; AShift: TShiftState;
  ADelta: Integer; APosition: TPoint; var AHandled: Boolean);
var
  LGeneration: INyxLCLBindingGeneration;
  LEnteredFrame: INyxNativeGestureFrame;
  LPrevious: TMouseWheelEvent;
  LEvents: INyxEvents;
  LRevision: Integer;
begin
  LGeneration := GenerationLease;

  if (LGeneration = nil) or not NativeGenerationCurrent(LGeneration) then
  begin
    AHandled := True;
    Exit;
  end;
  LEnteredFrame := LGeneration.PhysicalFrame;
  LEnteredFrame.Enter;
  try
    try

      if FRenderer.FDesignMode then
      begin
        Exit;
      end;
      LPrevious := FPointerHooks[PointerSlot(ASender)].WheelHorz;
      RequireNativeGeneration(LGeneration);
      LEvents := FRenderer.FEvents;
      RequireNativeGeneration(LGeneration);
      LRevision := LEvents.ViewRevision;
      RequireNativeGeneration(LGeneration);
      WheelEvent(ASender, AShift, ADelta, True, AHandled);
      RequireNativeGeneration(LGeneration);

      if not AHandled and (LEvents.ViewRevision = LRevision) and Assigned(LPrevious) then
      begin
        RequireNativeGeneration(LGeneration);
        LPrevious(ASender, AShift, ADelta, APosition, AHandled);
        RequireNativeGeneration(LGeneration);
      end;
      RequireNativeGeneration(LGeneration);
    except
      on ENyxNativeGenerationRetired do
      begin
        AHandled := True;
      end;
    end;
  finally
    LEnteredFrame.Leave;
  end;
end;


procedure TNyxLCLBinding.WheelEvent(ASender: TObject; AShift: TShiftState;
  ADelta: Integer; AHorizontal: Boolean; var AHandled: Boolean);
var
  LGeneration: INyxLCLBindingGeneration;
  LEnteredFrame: INyxNativeGestureFrame;
  LEvents: INyxEvents;
  LDispatch: TNyxDispatch;
  LModifiers: TNyxKeyModifiers;
  LX, LY: Double;
begin
  LGeneration := GenerationLease;

  if (LGeneration = nil) or not NativeGenerationCurrent(LGeneration) then
  begin
    AHandled := True;
    Exit;
  end;
  LEnteredFrame := LGeneration.PhysicalFrame;
  LEnteredFrame.Enter;
  try
    try

      LEvents := FRenderer.FEvents;
      RequireNativeGeneration(LGeneration);

      if AHandled or FRenderer.FUpdating or FRenderer.FDesignMode or
        (not LEvents.HasSubscribers(ntBeforeWheel) and not LEvents.HasSubscribers(ntWheel) and
          not LEvents.HasSubscribers(ntAfterWheel)) then
      begin
        RequireNativeGeneration(LGeneration);
        Exit;
      end;
      RequireNativeGeneration(LGeneration);
      LModifiers := [];

      if ssShift in AShift then
      begin
        Include(LModifiers, nmShift);
      end;

      if ssCtrl in AShift then
      begin
        Include(LModifiers, nmControl);
      end;

      if ssAlt in AShift then
      begin
        Include(LModifiers, nmAlt);
      end;
      LX := 0;
      RequireNativeGeneration(LGeneration);
      LY := -ADelta / 120;
      RequireNativeGeneration(LGeneration);

      if AHorizontal then
      begin
        { LCL horizontal positive deltas mean right; vertical positive means up. }
        LX := ADelta / 120;
        RequireNativeGeneration(LGeneration);
        LY := 0;
        RequireNativeGeneration(LGeneration);
      end;
      LDispatch := FRenderer.FLiveBindings.Signal(FNode, ntWheel);
      RequireNativeGeneration(LGeneration);
      LDispatch.Info.HasWheel := True;
      LDispatch.Info.Wheel := NyxWheel(LX, LY, 0, nwuDetents, LModifiers, True);
      RequireNativeGeneration(LGeneration);
      AHandled := DispatchNyxWheel(LEvents, FNode, LDispatch,
        FRenderer.FLiveBindings.SignalSnapshot);
        RequireNativeGeneration(LGeneration);
      { No binding/renderer access follows callbacks that may dispose this view. }
    except
      on ENyxNativeGenerationRetired do
      begin
        AHandled := True;
      end;
    end;
  finally
    LEnteredFrame.Leave;
  end;
end;


function NativePointerButton(AButton: TMouseButton): TNyxPointerButton;
begin
  case AButton of
    mbLeft:
      begin
        Result := npbPrimary;
      end;
    mbMiddle:
      begin
        Result := npbAuxiliary;
      end;
    mbRight:
      begin
        Result := npbSecondary;
      end;
  else
    Result := npbOther;
  end;
end;

function TNyxLCLBinding.PointerEvent(ASender: TObject; ATrigger: TNyxTrigger;
  const APosition: TPoint; AHasPosition: Boolean; AButton: TNyxPointerButton;
  AShift: TShiftState): Boolean;
var
  LGeneration: INyxLCLBindingGeneration;
  LEnteredFrame: INyxNativeGestureFrame;
  LDispatch: TNyxDispatch;
  LPosition: TPoint;
  LEvents: INyxEvents;
  LRevision: Integer;
  LFrame: INyxNativeGestureFrame;
  LObserver: INyxLCLCaptureObserver;
  LCapabilities: TNyxGestureCapabilities;
  LDecision: INyxGestureDecision;
  LResult: TNyxGestureResult;
  LControl: TControl;
  LInput: TControl;
  LRenderer: TNyxLCLRenderer;
  LOriginID: TNyxText;
begin
  LGeneration := GenerationLease;
  Result := False;

  if (LGeneration = nil) or not NativeGenerationCurrent(LGeneration) then
  begin
    Exit;
  end;
  LEnteredFrame := LGeneration.PhysicalFrame;
  LEnteredFrame.Enter;
  try
    try

      Result := False;
      LEvents := FRenderer.FEvents;
      RequireNativeGeneration(LGeneration);

      if FRenderer.FUpdating or FRenderer.FDesignMode or not LEvents.HasSubscribers(ATrigger) or
        not NyxInteractionPolicy(FNode).CanIssueCommand then
      begin
        RequireNativeGeneration(LGeneration);
        Exit;
      end;
      RequireNativeGeneration(LGeneration);
      LDispatch := FRenderer.FLiveBindings.Signal(FNode, ATrigger);
      RequireNativeGeneration(LGeneration);
      LDispatch.Info.HasPointer := True;
      LDispatch.Info.Pointer.Kind := npiMouse;
      LDispatch.Info.Pointer.Button := AButton;
      LDispatch.Info.Pointer.HasPosition := AHasPosition;

      if not AHasPosition then
      begin
        LDispatch.Info.Pointer.Kind := npiUnknown;
      end;

      if AHasPosition then
      begin
        LPosition := TControl(ASender).ClientToScreen(APosition);
        RequireNativeGeneration(LGeneration);
        LPosition := FControl.Parent.ScreenToClient(LPosition);
        RequireNativeGeneration(LGeneration);
        Dec(LPosition.X, FControl.Left);
        Dec(LPosition.Y, FControl.Top);
        LDispatch.Info.Pointer.X := Double(LPosition.X) + FPlacement.ContentOffsetX;
        RequireNativeGeneration(LGeneration);
        LDispatch.Info.Pointer.Y := Double(LPosition.Y) + FPlacement.ContentOffsetY;
        RequireNativeGeneration(LGeneration);
      end;

      if ssLeft in AShift then
      begin
        Include(LDispatch.Info.Pointer.Buttons, npbPrimary);
      end;

      if ssMiddle in AShift then
      begin
        Include(LDispatch.Info.Pointer.Buttons, npbAuxiliary);
      end;

      if ssRight in AShift then
      begin
        Include(LDispatch.Info.Pointer.Buttons, npbSecondary);
      end;

      if ssShift in AShift then
      begin
        Include(LDispatch.Info.Pointer.Modifiers, nmShift);
      end;

      if ssCtrl in AShift then
      begin
        Include(LDispatch.Info.Pointer.Modifiers, nmControl);
      end;

      if ssAlt in AShift then
      begin
        Include(LDispatch.Info.Pointer.Modifiers, nmAlt);
      end;

      if (ssMeta in AShift) or (ssSuper in AShift) then
      begin
        Include(LDispatch.Info.Pointer.Modifiers, nmMeta);
      end;
      RequireNativeGeneration(LGeneration);

      if ssAltGr in AShift then
      begin
        Include(LDispatch.Info.Pointer.Modifiers, nmAltGraph);
      end;

      if ATrigger = ntContextMenu then
      begin
        Result := DispatchNyxInput(LEvents, FNode, LDispatch);
        RequireNativeGeneration(LGeneration);
      end
      else if ATrigger in [ntPointerDown, ntPointerMove, ntPointerUp] then
      begin
        LRenderer := FRenderer;
        LFrame := LRenderer.FPhysicalFrame;
        RequireNativeGeneration(LGeneration);
        LObserver := LRenderer.FCaptureObserver;
        RequireNativeGeneration(LGeneration);
        LRevision := LEvents.ViewRevision;
        RequireNativeGeneration(LGeneration);
        LControl := FControl;
        RequireNativeGeneration(LGeneration);
        LInput := FInput;
        LOriginID := FNode.ID;
        RequireNativeGeneration(LGeneration);
        LCapabilities := [];

        if (ATrigger in [ntPointerDown, ntPointerMove]) and
          ((LDispatch.Info.Pointer.Buttons <> []) or (ATrigger = ntPointerDown)) then
        begin
          RequireNativeGeneration(LGeneration);
          Include(LCapabilities, ngcCapturePointer);
        end;
        RequireNativeGeneration(LGeneration);

        if NyxLCLHasPointerCapture(LControl) or NyxLCLHasPointerCapture(LInput) then
        begin
          Include(LCapabilities, ngcReleasePointer);
        end;
        RequireNativeGeneration(LGeneration);
        LDecision := NewNyxGestureDecision(LCapabilities);
        RequireNativeGeneration(LGeneration);
        LFrame.Enter;
        RequireNativeGeneration(LGeneration);
        try
          DispatchNyxGesture(LEvents, FNode, LDispatch, LDecision);
          RequireNativeGeneration(LGeneration);
          LResult := LDecision.Seal;
          RequireNativeGeneration(LGeneration);

          if LEvents.ViewRevision <> LRevision then
          begin
            RequireNativeGeneration(LGeneration);
            Exit;
          end;
          RequireNativeGeneration(LGeneration);
          try
            case LResult.PointerRequest of
              nprUnchanged:
                begin
                  { This dispatch requests no change to native pointer capture. }
                end;
              nprCapture:
                begin
                  CaptureNyxLCLPointer(LControl);
                  RequireNativeGeneration(LGeneration);
                end;
              nprRelease:
                begin
                  ReleaseNyxLCLPointer(LControl);
                  RequireNativeGeneration(LGeneration);
                  ReleaseNyxLCLPointer(LInput);
                  RequireNativeGeneration(LGeneration);
                end;
            end;
          except
            on ENyxNativeGenerationRetired do raise;
            on LException: Exception do
            begin
              LRenderer.GestureFailed(LOriginID, LException.Message);
              RequireNativeGeneration(LGeneration);
              Exit;
            end;
          end;

          if LResult.PointerRequest <> nprUnchanged then
          begin
            RequireNativeGeneration(LGeneration);
            LRenderer.FLastGestureError := '';
          end;
          RequireNativeGeneration(LGeneration);

          if LObserver <> nil then
          begin
            LObserver.Observe(LControl);
            RequireNativeGeneration(LGeneration);

            if (LEvents.ViewRevision = LRevision) and (LInput <> nil) and
              (LInput <> LControl) then
            begin
              RequireNativeGeneration(LGeneration);
              LObserver.Observe(LInput);
              RequireNativeGeneration(LGeneration);
            end;
            RequireNativeGeneration(LGeneration);
          end;
        finally
          LFrame.Leave;
        end;
        RequireNativeGeneration(LGeneration);
      end
      else
      begin
        FRenderer.Emit(FNode, LDispatch);
        RequireNativeGeneration(LGeneration);
      end;
    except
      on ENyxNativeGenerationRetired do
      begin
        Result := False;
      end;
    end;
  finally
    LEnteredFrame.Leave;
  end;
end;


procedure TNyxLCLBinding.DoubleClick(ASender: TObject);
var
  LGeneration: INyxLCLBindingGeneration;
  LEnteredFrame: INyxNativeGestureFrame;
  LEvents: INyxEvents;
  LRevision: Integer;
  LPrevious: TNotifyEvent;
  LPosition: TPoint;
begin
  LGeneration := GenerationLease;

  if (LGeneration = nil) or not NativeGenerationCurrent(LGeneration) then
  begin
    Exit;
  end;
  LEnteredFrame := LGeneration.PhysicalFrame;
  LEnteredFrame.Enter;
  try
    try

      if FRenderer.FDesignMode then
      begin
        Exit;
      end;
      LEvents := FRenderer.FEvents;
      RequireNativeGeneration(LGeneration);
      LRevision := LEvents.ViewRevision;
      RequireNativeGeneration(LGeneration);
      LPrevious := FPointerHooks[PointerSlot(ASender)].DoubleClick;
      RequireNativeGeneration(LGeneration);

      if Assigned(LPrevious) then
      begin
        RequireNativeGeneration(LGeneration);
        LPrevious(ASender);
        RequireNativeGeneration(LGeneration);
      end;
      RequireNativeGeneration(LGeneration);

      if LEvents.ViewRevision <> LRevision then
      begin
        RequireNativeGeneration(LGeneration);
        Exit;
      end;
      RequireNativeGeneration(LGeneration);
      LPosition := TControl(ASender).ScreenToClient(Mouse.CursorPos);
      RequireNativeGeneration(LGeneration);
      PointerEvent(ASender, ntDoubleClick, LPosition, True, npbNone, []);
      RequireNativeGeneration(LGeneration);
    except
      on ENyxNativeGenerationRetired do
      begin
        { The original generation retired; only retained locals may unwind. }
      end;
    end;
  finally
    LEnteredFrame.Leave;
  end;
end;


procedure TNyxLCLBinding.PointerEnter(ASender: TObject);
var
  LGeneration: INyxLCLBindingGeneration;
  LEnteredFrame: INyxNativeGestureFrame;
  LEvents: INyxEvents;
  LRevision: Integer;
  LPrevious: TNotifyEvent;
  LPosition: TPoint;
begin
  LGeneration := GenerationLease;

  if (LGeneration = nil) or not NativeGenerationCurrent(LGeneration) then
  begin
    Exit;
  end;
  LEnteredFrame := LGeneration.PhysicalFrame;
  LEnteredFrame.Enter;
  try
    try

      if FRenderer.FDesignMode then
      begin
        Exit;
      end;
      LEvents := FRenderer.FEvents;
      RequireNativeGeneration(LGeneration);
      LRevision := LEvents.ViewRevision;
      RequireNativeGeneration(LGeneration);
      LPrevious := FPointerHooks[PointerSlot(ASender)].Enter;
      RequireNativeGeneration(LGeneration);

      if Assigned(LPrevious) then
      begin
        RequireNativeGeneration(LGeneration);
        LPrevious(ASender);
        RequireNativeGeneration(LGeneration);
      end;
      RequireNativeGeneration(LGeneration);

      if LEvents.ViewRevision <> LRevision then
      begin
        RequireNativeGeneration(LGeneration);
        Exit;
      end;
      RequireNativeGeneration(LGeneration);
      LPosition := TControl(ASender).ScreenToClient(Mouse.CursorPos);
      RequireNativeGeneration(LGeneration);
      PointerEvent(ASender, ntPointerEnter, LPosition, True, npbNone, []);
      RequireNativeGeneration(LGeneration);
    except
      on ENyxNativeGenerationRetired do
      begin
        { The original generation retired; only retained locals may unwind. }
      end;
    end;
  finally
    LEnteredFrame.Leave;
  end;
end;


procedure TNyxLCLBinding.PointerExit(ASender: TObject);
var
  LGeneration: INyxLCLBindingGeneration;
  LEnteredFrame: INyxNativeGestureFrame;
  LEvents: INyxEvents;
  LRevision: Integer;
  LPrevious: TNotifyEvent;
  LPosition: TPoint;
begin
  LGeneration := GenerationLease;

  if (LGeneration = nil) or not NativeGenerationCurrent(LGeneration) then
  begin
    Exit;
  end;
  LEnteredFrame := LGeneration.PhysicalFrame;
  LEnteredFrame.Enter;
  try
    try

      if FRenderer.FDesignMode then
      begin
        Exit;
      end;
      LEvents := FRenderer.FEvents;
      RequireNativeGeneration(LGeneration);
      LRevision := LEvents.ViewRevision;
      RequireNativeGeneration(LGeneration);
      LPrevious := FPointerHooks[PointerSlot(ASender)].Leave;
      RequireNativeGeneration(LGeneration);

      if Assigned(LPrevious) then
      begin
        RequireNativeGeneration(LGeneration);
        LPrevious(ASender);
        RequireNativeGeneration(LGeneration);
      end;
      RequireNativeGeneration(LGeneration);

      if LEvents.ViewRevision <> LRevision then
      begin
        RequireNativeGeneration(LGeneration);
        Exit;
      end;
      RequireNativeGeneration(LGeneration);
      LPosition := TControl(ASender).ScreenToClient(Mouse.CursorPos);
      RequireNativeGeneration(LGeneration);
      PointerEvent(ASender, ntPointerExit, LPosition, True, npbNone, []);
      RequireNativeGeneration(LGeneration);
    except
      on ENyxNativeGenerationRetired do
      begin
        { The original generation retired; only retained locals may unwind. }
      end;
    end;
  finally
    LEnteredFrame.Leave;
  end;
end;


procedure TNyxLCLBinding.PointerDown(ASender: TObject; AButton: TMouseButton;
  AShift: TShiftState; AX, AY: Integer);
var
  LGeneration: INyxLCLBindingGeneration;
  LEnteredFrame: INyxNativeGestureFrame;
  LEvents: INyxEvents;
  LRevision: Integer;
  LPrevious: TMouseEvent;
begin
  LGeneration := GenerationLease;

  if (LGeneration = nil) or not NativeGenerationCurrent(LGeneration) then
  begin
    Exit;
  end;
  LEnteredFrame := LGeneration.PhysicalFrame;
  LEnteredFrame.Enter;
  try
    try

      if FRenderer.FDesignMode then
      begin
        Exit;
      end;
      LEvents := FRenderer.FEvents;
      RequireNativeGeneration(LGeneration);
      LRevision := LEvents.ViewRevision;
      RequireNativeGeneration(LGeneration);
      LPrevious := FPointerHooks[PointerSlot(ASender)].Down;
      RequireNativeGeneration(LGeneration);

      if Assigned(LPrevious) then
      begin
        RequireNativeGeneration(LGeneration);
        LPrevious(ASender, AButton, AShift, AX, AY);
        RequireNativeGeneration(LGeneration);
      end;
      RequireNativeGeneration(LGeneration);

      if LEvents.ViewRevision <> LRevision then
      begin
        RequireNativeGeneration(LGeneration);
        Exit;
      end;
      RequireNativeGeneration(LGeneration);
      PointerEvent(ASender, ntPointerDown, Point(AX, AY), True,
        NativePointerButton(AButton), AShift);
        RequireNativeGeneration(LGeneration);
    except
      on ENyxNativeGenerationRetired do
      begin
        { The original generation retired; only retained locals may unwind. }
      end;
    end;
  finally
    LEnteredFrame.Leave;
  end;
end;


procedure TNyxLCLBinding.PointerUp(ASender: TObject; AButton: TMouseButton;
  AShift: TShiftState; AX, AY: Integer);
var
  LGeneration: INyxLCLBindingGeneration;
  LEnteredFrame: INyxNativeGestureFrame;
  LEvents: INyxEvents;
  LRevision: Integer;
  LPrevious: TMouseEvent;
begin
  LGeneration := GenerationLease;

  if (LGeneration = nil) or not NativeGenerationCurrent(LGeneration) then
  begin
    Exit;
  end;
  LEnteredFrame := LGeneration.PhysicalFrame;
  LEnteredFrame.Enter;
  try
    try

      if FRenderer.FDesignMode then
      begin
        Exit;
      end;
      LEvents := FRenderer.FEvents;
      RequireNativeGeneration(LGeneration);
      LRevision := LEvents.ViewRevision;
      RequireNativeGeneration(LGeneration);
      LPrevious := FPointerHooks[PointerSlot(ASender)].Up;
      RequireNativeGeneration(LGeneration);

      if Assigned(LPrevious) then
      begin
        RequireNativeGeneration(LGeneration);
        LPrevious(ASender, AButton, AShift, AX, AY);
        RequireNativeGeneration(LGeneration);
      end;
      RequireNativeGeneration(LGeneration);

      if LEvents.ViewRevision <> LRevision then
      begin
        RequireNativeGeneration(LGeneration);
        Exit;
      end;
      RequireNativeGeneration(LGeneration);
      PointerEvent(ASender, ntPointerUp, Point(AX, AY), True,
        NativePointerButton(AButton), AShift);
        RequireNativeGeneration(LGeneration);
    except
      on ENyxNativeGenerationRetired do
      begin
        { The original generation retired; only retained locals may unwind. }
      end;
    end;
  finally
    LEnteredFrame.Leave;
  end;
end;


procedure TNyxLCLBinding.PointerMove(ASender: TObject; AShift: TShiftState; AX, AY: Integer);
var
  LGeneration: INyxLCLBindingGeneration;
  LEnteredFrame: INyxNativeGestureFrame;
  LEvents: INyxEvents;
  LRevision: Integer;
  LPrevious: TMouseMoveEvent;
begin
  LGeneration := GenerationLease;

  if (LGeneration = nil) or not NativeGenerationCurrent(LGeneration) then
  begin
    Exit;
  end;
  LEnteredFrame := LGeneration.PhysicalFrame;
  LEnteredFrame.Enter;
  try
    try

      if FRenderer.FDesignMode then
      begin
        Exit;
      end;
      LEvents := FRenderer.FEvents;
      RequireNativeGeneration(LGeneration);
      LRevision := LEvents.ViewRevision;
      RequireNativeGeneration(LGeneration);
      LPrevious := FPointerHooks[PointerSlot(ASender)].Move;
      RequireNativeGeneration(LGeneration);

      if Assigned(LPrevious) then
      begin
        RequireNativeGeneration(LGeneration);
        LPrevious(ASender, AShift, AX, AY);
        RequireNativeGeneration(LGeneration);
      end;
      RequireNativeGeneration(LGeneration);

      if LEvents.ViewRevision <> LRevision then
      begin
        RequireNativeGeneration(LGeneration);
        Exit;
      end;
      RequireNativeGeneration(LGeneration);
      PointerEvent(ASender, ntPointerMove, Point(AX, AY), True,
        npbNone, AShift);
        RequireNativeGeneration(LGeneration);
    except
      on ENyxNativeGenerationRetired do
      begin
        { The original generation retired; only retained locals may unwind. }
      end;
    end;
  finally
    LEnteredFrame.Leave;
  end;
end;


procedure TNyxLCLBinding.ContextMenu(ASender: TObject; APosition: TPoint;
  var AHandled: Boolean);
begin
  RunCellContext(ASender, APosition, AHandled, nil, nil, False);
end;

procedure TNyxLCLBinding.PreparedCellContext(ASender: TObject; APosition: TPoint;
  var AHandled: Boolean; const ACommand: INyxCollectionClipboardCommand;
  APrevious: TContextPopupEvent);
begin
  { The compound editor already reserved original intent and text before focus
    or its own previous callback. This entry never captures another command. }
  RunCellContext(ASender, APosition, AHandled, ACommand, APrevious, True);
end;

procedure TNyxLCLBinding.RunCellContext(ASender: TObject; APosition: TPoint;
  var AHandled: Boolean; const ACommand: INyxCollectionClipboardCommand;
  APrevious: TContextPopupEvent; APrepared: Boolean);
var
  LGeneration: INyxLCLBindingGeneration;
  LEnteredFrame: INyxNativeGestureFrame;
  LEvents: INyxEvents;
  LRevision: Integer;
  LPrevious: TContextPopupEvent;
  LMount: INyxCollectionMount;
  LInput: INyxLCLCollectionCellContextInput;
  LCommand: INyxCollectionClipboardCommand;
begin
  LCommand := ACommand;
  LGeneration := GenerationLease;

  if (LGeneration = nil) or not NativeGenerationCurrent(LGeneration) then
  begin
    AHandled := True;
    Exit;
  end;
  LEnteredFrame := LGeneration.PhysicalFrame;
  LEnteredFrame.Enter;
  try
    try

      if FRenderer.FDesignMode then
      begin
        Exit;
      end;
      { Capture exact cell/input authority before the first real router getter
        or previous context callback. The command owns its mount only until
        default handoff to the existing scheduler; the session never does. }
      LMount := FCollectionMount;

      if not AHandled then
      begin

        if not APrepared and Supports(LMount, INyxLCLCollectionCellContextInput, LInput) then
        begin
          LCommand := LInput.BeginCellContext(ASender, APosition);
          RequireNativeGeneration(LGeneration);
        end;

        if (LCommand <> nil) and LCommand.Current then
        begin
          LCommand.Prepare;
          RequireNativeGeneration(LGeneration);
        end;
      end;

      if (LCommand <> nil) and not LCommand.DispatchCurrent then
      begin
        AHandled := True;
        Exit;
      end;
      LEvents := FRenderer.FEvents;
      RequireNativeGeneration(LGeneration);
      LRevision := LEvents.ViewRevision;
      RequireNativeGeneration(LGeneration);

      if APrepared then
      begin
        LPrevious := APrevious;
      end
      else
      begin
        LPrevious := FPointerHooks[PointerSlot(ASender)].ContextMenu;
      end;
      RequireNativeGeneration(LGeneration);

      if Assigned(LPrevious) then
      begin
        RequireNativeGeneration(LGeneration);
        LPrevious(ASender, APosition, AHandled);
        RequireNativeGeneration(LGeneration);
      end;
      RequireNativeGeneration(LGeneration);

      if LEvents.ViewRevision <> LRevision then
      begin
        RequireNativeGeneration(LGeneration);
        AHandled := True;
        Exit;
      end;
      RequireNativeGeneration(LGeneration);

      if (LCommand <> nil) and not LCommand.DispatchCurrent then
      begin
        AHandled := True;
        Exit;
      end;

      if not AHandled then
      begin
        AHandled := PointerEvent(ASender, ntContextMenu, APosition,
          (APosition.X >= 0) and (APosition.Y >= 0), npbNone, []);
          RequireNativeGeneration(LGeneration);
      end;

      if LCommand <> nil then
      begin
        LCommand.Complete(not AHandled and LCommand.DispatchCurrent);
      end;
    except
      on ENyxNativeGenerationRetired do
      begin
        AHandled := True;
      end;
    end;
  finally
    try

      if LCommand <> nil then
      begin
        AHandled := True;
        LCommand.Retire;
      end;
    finally
      LEnteredFrame.Leave;
    end;
  end;
end;


procedure TNyxLCLBinding.Focus(ASender: TObject);
var
  LGeneration: INyxLCLBindingGeneration;
  LEnteredFrame: INyxNativeGestureFrame;
begin
  LGeneration := GenerationLease;

  if (LGeneration = nil) or not NativeGenerationCurrent(LGeneration) then
  begin
    Exit;
  end;
  LEnteredFrame := LGeneration.PhysicalFrame;
  LEnteredFrame.Enter;
  try
    try

      if FRenderer.FUpdating or FRenderer.FDesignMode then
      begin
        Exit;
      end;
      { LCL owns focus transitions. Repaint only the frame that visualizes that
        native state; there is no competing Nyx focus or keyboard state machine. }

      if (FInput <> nil) and (FInput.Parent is TNyxLCLSurface) then
      begin
        RequireNativeGeneration(LGeneration);
        FInput.Parent.Invalidate;
        RequireNativeGeneration(LGeneration);
      end;
      RequireNativeGeneration(LGeneration);
    except
      on ENyxNativeGenerationRetired do
      begin
        { The original generation retired; only retained locals may unwind. }
      end;
    end;
  finally
    LEnteredFrame.Leave;
  end;
end;


procedure TNyxLCLBinding.SplitLayout(ASender: TObject);
var
  LGeneration: INyxLCLBindingGeneration;
  LEnteredFrame: INyxNativeGestureFrame;
  LSplit: TNyxLCLSplitView;
  LIndex: Integer;
begin
  LGeneration := GenerationLease;

  if (LGeneration = nil) or not NativeGenerationCurrent(LGeneration) then
  begin
    Exit;
  end;
  LEnteredFrame := LGeneration.PhysicalFrame;
  LEnteredFrame.Enter;
  try
    try

      LSplit := TNyxLCLSplitView(ASender);
      RequireNativeGeneration(LGeneration);
      for LIndex := 0 to FNode.Count - 1 do
      begin
        RequireNativeGeneration(LGeneration);
        FRenderer.Layout(FNode.Children[LIndex], 0, 0,
          LSplit.Panes[LIndex].ClientWidth, LSplit.Panes[LIndex].ClientHeight);
          RequireNativeGeneration(LGeneration);
      end;
      RequireNativeGeneration(LGeneration);
    except
      on ENyxNativeGenerationRetired do
      begin
        { The original generation retired; only retained locals may unwind. }
      end;
    end;
  finally
    LEnteredFrame.Leave;
  end;
end;


procedure TNyxLCLBinding.SplitChanged(ASender: TObject);
var
  LGeneration: INyxLCLBindingGeneration;
  LEnteredFrame: INyxNativeGestureFrame;
begin
  LGeneration := GenerationLease;

  if (LGeneration = nil) or not NativeGenerationCurrent(LGeneration) then
  begin
    Exit;
  end;
  LEnteredFrame := LGeneration.PhysicalFrame;
  LEnteredFrame.Enter;
  try
    try

      FRenderer.Emit(FNode, NyxSplitChange(FNode, TNyxLCLSplitView(ASender).State.Position));
      RequireNativeGeneration(LGeneration);
    except
      on ENyxNativeGenerationRetired do
      begin
        { The original generation retired; only retained locals may unwind. }
      end;
    end;
  finally
    LEnteredFrame.Leave;
  end;
end;


function TNyxLCLBinding.AdmitTabs(const AChange: TNyxTabTransition): Boolean;
var
  LGeneration: INyxLCLBindingGeneration;
  LEnteredFrame: INyxNativeGestureFrame;
begin
  LGeneration := GenerationLease;
  Result := False;

  if (LGeneration = nil) or not NativeGenerationCurrent(LGeneration) then
  begin
    Exit;
  end;
  LEnteredFrame := LGeneration.PhysicalFrame;
  LEnteredFrame.Enter;
  try
    try

      Result := False;
      FHasTabDispatch := False;

      if FRenderer.FUpdating or (AChange.AfterSelection < 0) then
      begin
        RequireNativeGeneration(LGeneration);
        Exit;
      end;
      RequireNativeGeneration(LGeneration);

      if FRenderer.FDesignMode then
      begin
        Exit(True);
      end;
      try
        FTabDispatch := FRenderer.FLiveBindings.Edit(FNode, NyxTabValue(
          TNyxLCLTabHost(FControl).State.Entry(AChange.AfterSelection).SourceReference));
          RequireNativeGeneration(LGeneration);
        FHasTabDispatch := True;
        FRenderer.FLastBindingError := '';
        FRenderer.FLastBindingFailure := nbfNone;
        Result := True;
      except
        on ENyxNativeGenerationRetired do raise;
        on LException: ENyxStateNotification do
        begin
          { An old notification may have accepted a successor before raising.
            Its diagnostic belongs only to the original entered generation. }
          RequireNativeGeneration(LGeneration);
          FRenderer.BindingFailed(FNode, LException.Message, nbfNotificationFailed);
          RequireNativeGeneration(LGeneration);
        end;
        on LException: Exception do
        begin
          { An old notification may have accepted a successor before raising.
            Its diagnostic belongs only to the original entered generation. }
          RequireNativeGeneration(LGeneration);
          FRenderer.BindingFailed(FNode, LException.Message, nbfRejected, False);
          RequireNativeGeneration(LGeneration);
        end;
      end;
    except
      on ENyxNativeGenerationRetired do
      begin
        Result := False;
      end;
    end;
  finally
    LEnteredFrame.Leave;
  end;
end;


procedure TNyxLCLBinding.TabsChanged(const AChange: TNyxTabTransition);
var
  LGeneration: INyxLCLBindingGeneration;
  LEnteredFrame: INyxNativeGestureFrame;
  LDispatch: TNyxDispatch;
begin
  LGeneration := GenerationLease;

  if (LGeneration = nil) or not NativeGenerationCurrent(LGeneration) then
  begin
    Exit;
  end;
  LEnteredFrame := LGeneration.PhysicalFrame;
  LEnteredFrame.Enter;
  try
    try

      if FHasTabDispatch then
      begin
        LDispatch := FTabDispatch;
        FHasTabDispatch := False;
        FTabDispatch := Default(TNyxDispatch);
        RequireNativeGeneration(LGeneration);
        FRenderer.Emit(FNode, LDispatch);
        RequireNativeGeneration(LGeneration);
      end;
    except
      on ENyxNativeGenerationRetired do
      begin
        { The original generation retired; only retained locals may unwind. }
      end;
    end;
  finally
    LEnteredFrame.Leave;
  end;
end;


procedure TNyxLCLBinding.Click(ASender: TObject);
begin
  PerformClick(ASender);
end;

function TNyxLCLBinding.PerformClick(ASender: TObject): Boolean;
var
  LGeneration: INyxLCLBindingGeneration;
  LEnteredFrame: INyxNativeGestureFrame;
  LDispatch: TNyxDispatch;
  LEvents: INyxEvents;
  LRevision: Integer;
  LPrevious: TNotifyEvent;
begin
  Result := False;
  LGeneration := GenerationLease;

  if (LGeneration = nil) or not NativeGenerationCurrent(LGeneration) then
  begin
    Exit;
  end;
  LEnteredFrame := LGeneration.PhysicalFrame;
  LEnteredFrame.Enter;
  try
    try

      if FRenderer.FDesignMode then
      begin

        if not FRenderer.FUpdating and Assigned(FRenderer.FOnEvent) then
        begin
          RequireNativeGeneration(LGeneration);
          FRenderer.FOnEvent(FNode, NyxDesignEvent(FNode, ntDesignSelect));
          RequireNativeGeneration(LGeneration);
        end;
        RequireNativeGeneration(LGeneration);
        { A designer selection never calls a creator's application click hook or
          a live binding action. Native editable controls keep their own defaults. }
        Exit;
      end;
      { Native accessibility messages can arrive during a synchronizing layout.
        A creator's callback is application behavior too; refuse before invoking it. }

      if FRenderer.FUpdating then
      begin
        Exit;
      end;
      LEvents := FRenderer.FEvents;
      RequireNativeGeneration(LGeneration);
      LRevision := LEvents.ViewRevision;
      RequireNativeGeneration(LGeneration);
      LPrevious := FPreviousClick;

      if (ASender = FInput) and (FInput <> FControl) then
      begin
        LPrevious := FPreviousInputClick;
      end;
      RequireNativeGeneration(LGeneration);

      if Assigned(LPrevious) then
      begin
        RequireNativeGeneration(LGeneration);
        LPrevious(ASender);
        RequireNativeGeneration(LGeneration);
      end;
      RequireNativeGeneration(LGeneration);

      if LEvents.ViewRevision <> LRevision then
      begin
        RequireNativeGeneration(LGeneration);
        { A custom callback may replace/dispose this view and its binding. Retain
          only the router above and never read Self's borrowed fields afterward. }
        Exit;
      end;
      RequireNativeGeneration(LGeneration);

      if FRenderer.FUpdating then
      begin
        Exit;
      end;
      { General click subscriptions are opt-in. Older global callbacks retain their
        command/named-click behavior, and ordinary checkbox edits still emit once. }

      if not NyxHasLegacyClick(FNode) and not FRenderer.FEvents.HasSubscribers(ntClick) and
        (FNode.Prop(NyxAttributeName(atAction)) = '') then
      begin
        RequireNativeGeneration(LGeneration);
        Result := True;
        Exit;
      end;
      RequireNativeGeneration(LGeneration);
      try
        LDispatch := FRenderer.FLiveBindings.Dispatch(FNode, ntClick);
        RequireNativeGeneration(LGeneration);
        FRenderer.FLastBindingError := '';
        FRenderer.FLastBindingFailure := nbfNone;
      except
        on ENyxNativeGenerationRetired do raise;
        on LException: ENyxStateNotification do
        begin
          { An old notification may have accepted a successor before raising.
            Its diagnostic belongs only to the original entered generation. }
          RequireNativeGeneration(LGeneration);
          FRenderer.BindingFailed(FNode, LException.Message, nbfNotificationFailed);
          RequireNativeGeneration(LGeneration);
          Exit;
        end;
        on LException: Exception do
        begin
          { An old notification may have accepted a successor before raising.
            Its diagnostic belongs only to the original entered generation. }
          RequireNativeGeneration(LGeneration);
          FRenderer.BindingFailed(FNode, LException.Message, nbfRejected,
              not NyxIsDisclosureHeader(FNode));
              RequireNativeGeneration(LGeneration);
          Exit;
        end;
      end;

      FRenderer.Emit(FNode, LDispatch);
      RequireNativeGeneration(LGeneration);
      Result := True;
    except
      on ENyxNativeGenerationRetired do
      begin
        { The original generation retired; only retained locals may unwind. }
      end;
    end;
  finally
    LEnteredFrame.Leave;
  end;
end;


function TNyxLCLRenderer.TextSelectionFor(const AID: TNyxText): TNyxTextSelection;
var
  LInput: TControl;
begin
  FEvents.Scheduler.RequireUI;
  LInput := InputFor(AID);
  Result := Default(TNyxTextSelection);

  if LInput is TWinControl then
  begin
    Result := CaptureNyxLCLSelection(TWinControl(LInput));
  end;
end;

function TNyxLCLRenderer.EditingFor(const AID: TNyxText): TNyxEditingSnapshot;
var
  LInput: TControl;
  LIndex: Integer;
begin
  FEvents.Scheduler.RequireUI;
  LInput := InputFor(AID);
  for LIndex := 0 to High(FBindings) do
  begin

    if FBindings[LIndex].FInput = LInput then
    begin
      Exit(CaptureNyxLCLEditing(TWinControl(LInput), nepObservation,
        FBindings[LIndex].FComposing));
    end;
  end;
  raise ENyxModel.Create('The mounted component has no text editing context');
end;

procedure TNyxLCLRenderer.SetTextSelection(const AID: TNyxText;
  const ASelection: TNyxTextSelection);
var
  LInput: TControl;
begin
  FEvents.Scheduler.RequireUI;
  LInput := InputFor(AID);

  if not (LInput is TWinControl) then
  begin
    raise ENyxModel.Create('The mounted component has no text-selection input');
  end;
  SelectNyxLCLText(TWinControl(LInput), ASelection);
end;

procedure TNyxLCLRenderer.BindTextHistory(const AID: TNyxText;
  const ACommands: INyxTextHistoryCommands);
var
  LInput: TControl;
  LIndex: Integer;
  LHistory: INyxLCLTextHistoryObserver;
begin
  FEvents.Scheduler.RequireUI;
  LInput := InputFor(AID);
  for LIndex := 0 to High(FBindings) do
  begin

    if FBindings[LIndex].FInput = LInput then
    begin

      if not Supports(FEditingObserver, INyxLCLTextHistoryObserver, LHistory) then
      begin
        raise ENyxModel.Create('This editing observer has no text history capability');
      end;
      LHistory.BindHistory(FBindings[LIndex].FNode.ID, ACommands);
      Exit;
    end;
  end;
  raise ENyxModel.Create('Text history requires a current text input');
end;

procedure TNyxLCLRenderer.EditingChanged(const AOriginID: TNyxText;
  const AEditing: TNyxEditingSnapshot);
var
  LGeneration: INyxLCLBindingGeneration;
  LEnteredFrame: INyxNativeGestureFrame;
  LIndex: Integer;
  LBinding: TNyxLCLBinding;
  LEvents: INyxEvents;
  LRevision: Integer;
  LTrigger: TNyxTrigger;
  LDispatch: TNyxDispatch;
begin
  LGeneration := FGeneration;

  if (LGeneration = nil) or not NativeGenerationCurrent(LGeneration) then
  begin
    Exit;
  end;
  LEnteredFrame := FPhysicalFrame;
  LEnteredFrame.Enter;
  try
    try

      LEvents := FEvents;
      RequireNativeGeneration(LGeneration);
      LRevision := LEvents.ViewRevision;
      RequireNativeGeneration(LGeneration);

      if FUpdating or FDesignMode then
      begin
        Exit;
      end;
      LBinding := nil;
      RequireNativeGeneration(LGeneration);
      for LIndex := 0 to High(FBindings) do
      begin

        if FBindings[LIndex].FNode.ID = AOriginID then
        begin
          LBinding := FBindings[LIndex];
          RequireNativeGeneration(LGeneration);
          Break;
        end;
      end;
      RequireNativeGeneration(LGeneration);

      if (LBinding = nil) or not NyxSupportsTextInput(LBinding.FNode) or
        not NyxInteractionPolicy(LBinding.FNode).CanIssueCommand then
      begin
        RequireNativeGeneration(LGeneration);
        Exit;
      end;
      RequireNativeGeneration(LGeneration);
      case AEditing.Phase of
        nepCompositionStart:
          begin

            if not NyxInteractionPolicy(LBinding.FNode).CanEditValue then
            begin
              RequireNativeGeneration(LGeneration);
              Exit;
            end;
            RequireNativeGeneration(LGeneration);
            LBinding.FComposing := True;
            LTrigger := ntCompositionStart;
          end;
        nepCompositionUpdate:
          begin
            LTrigger := ntCompositionUpdate;
          end;
        nepCompositionEnd:
          begin
            LBinding.FComposing := False;
            LBinding.FEditingContext := NyxEditingSnapshot(nepInput,
              AEditing.Intent, AEditing.Text, AEditing.Data, AEditing.HasData,
              AEditing.Selection, False, False);
              RequireNativeGeneration(LGeneration);
            LBinding.Change(LBinding.FInput);
            RequireNativeGeneration(LGeneration);

            if LEvents.ViewRevision <> LRevision then
            begin
              RequireNativeGeneration(LGeneration);
              Exit;
            end;
            RequireNativeGeneration(LGeneration);
            LTrigger := ntCompositionEnd;
          end;
        nepSelectionChange:
          begin
            LTrigger := ntTextSelectionChange;
          end;
      else
        begin
          Exit;
        end;
      end;
      LDispatch := FLiveBindings.Signal(LBinding.FNode, LTrigger);
      RequireNativeGeneration(LGeneration);
      LDispatch.Info.HasEditing := True;
      LDispatch.Info.Editing := AEditing;
      QueueContent;
      RequireNativeGeneration(LGeneration);
      { Last borrowed renderer/control access. The observer keeps its own producer
        alive, while navigation revokes its handles and pending final admission. }
      Emit(LBinding.FNode, LDispatch);
      RequireNativeGeneration(LGeneration);
    except
      on ENyxNativeGenerationRetired do
      begin
        { The original generation retired; only retained locals may unwind. }
      end;
    end;
  finally
    LEnteredFrame.Leave;
  end;
end;


procedure TNyxLCLBinding.Change(ASender: TObject);
var
  LSnapshotObject: TNativeSignalSnapshot;
  LSnapshotLease: IInterface;
  LGeneration: INyxLCLBindingGeneration;
  LEnteredFrame: INyxNativeGestureFrame;
  LValue: TNyxText;
  LDispatch: TNyxDispatch;
  LProposal: TNyxDispatch;
  LEvents: INyxEvents;
  LRevision: Integer;
  LConsumed: Boolean;
  LEditing: TNyxEditingSnapshot;
begin
  LGeneration := GenerationLease;

  if (LGeneration = nil) or not NativeGenerationCurrent(LGeneration) then
  begin
    Exit;
  end;
  LEnteredFrame := LGeneration.PhysicalFrame;
  LEnteredFrame.Enter;
  try
    try

      if FRenderer.FUpdating then
      begin
        Exit;
      end;

      if FComposing then
      begin
        { The IME owns its draft until the genuine end notification is drained at
          UI idle. Do not admit or normalize individual committed WM_CHAR units. }
        Exit;
      end;
      LEditing := FEditingContext;
      FEditingContext := Default(TNyxEditingSnapshot);
      RequireNativeGeneration(LGeneration);

      if not LEditing.Defined and NyxSupportsTextInput(FNode) then
      begin
        RequireNativeGeneration(LGeneration);
        LEditing := CaptureNyxLCLEditing(TWinControl(FInput), nepInput, False);
        RequireNativeGeneration(LGeneration);
      end;
      RequireNativeGeneration(LGeneration);

      if not NyxInteractionPolicy(FNode).CanEditValue then
      begin
        RequireNativeGeneration(LGeneration);
        { Native selectors lacking ReadOnly may propose a changed physical value.
          Restore before any callback/admission; editable text uses the real LCL
          ReadOnly flag as well. Programmatic state refresh remains independent. }
        FRenderer.FForceValues := True;
        try
          FRenderer.Sync;
          RequireNativeGeneration(LGeneration);
        finally

          if NativeGenerationCurrent(LGeneration) then
          begin
            FRenderer.FForceValues := False;
          end;
        end;
        RequireNativeGeneration(LGeneration);
        Exit;
      end;
      RequireNativeGeneration(LGeneration);

      if FDeferredValue and not FCommitting then
      begin
        { A numeric/clock draft such as '-' or '08:30:00.1' is meaningful while typing.
          Lazarus's editing-complete event supplies the admission boundary, matching
          browser commit behavior instead of restoring a value on every keystroke. }
        Exit;
      end;
      LValue := '';

      if FInput is TMemo then
      begin
        LValue := StringReplace(TMemo(FInput).Text, #13#10, #10, [rfReplaceAll]);
        RequireNativeGeneration(LGeneration);
      end
      else if FInput is TCustomEdit then
      begin
        LValue := TEdit(FInput).Text;
        RequireNativeGeneration(LGeneration);
      end
      else if FInput is TComboBox then
      begin
        LValue := TComboBox(FInput).Text;
        RequireNativeGeneration(LGeneration);
      end
      else if FInput is TCheckBox then
      begin
        LValue := 'false';

        if TCheckBox(FInput).Checked then
        begin
          RequireNativeGeneration(LGeneration);
          LValue := 'true';
        end;
        RequireNativeGeneration(LGeneration);
      end
      else if FInput is TRadioButton then
      begin
        LValue := 'false';

        if TRadioButton(FInput).Checked then
        begin
          RequireNativeGeneration(LGeneration);
          LValue := 'true';
        end;
        RequireNativeGeneration(LGeneration);
      end
      else if FInput is TNyxLCLSlider then
      begin
        LValue := TNyxLCLSlider(FInput).NumericValue;
        RequireNativeGeneration(LGeneration);
      end
      else if FInput is TTrackBar then
      begin
        LValue := IntToStr(TTrackBar(FInput).Position);
        RequireNativeGeneration(LGeneration);
      end;

      if FRenderer.FDesignMode then
      begin
        { This is a realized proposal, not a store/application mutation. The host
          owns admission and can restore a refused value through ordinary Sync. }
        FNode.SetProp('value', LValue);
        RequireNativeGeneration(LGeneration);

        if Assigned(FRenderer.FOnEvent) then
        begin
          RequireNativeGeneration(LGeneration);
          FRenderer.FOnEvent(FNode, NyxDesignEvent(FNode, ntDesignValue));
          RequireNativeGeneration(LGeneration);
        end;
        RequireNativeGeneration(LGeneration);
        Exit;
      end;
      { Win32 may deliver a second change after a setter/restoration has returned.
        It already contains the accepted value. Ignore it so a rejected edit cannot
        emit a synthetic success or clear its diagnostic after the update guard. }

      if LValue = FNode.Prop('value') then
      begin
        RequireNativeGeneration(LGeneration);
        Exit;
      end;
      RequireNativeGeneration(LGeneration);
      LEvents := FRenderer.FEvents;
      RequireNativeGeneration(LGeneration);
      LRevision := LEvents.ViewRevision;
      RequireNativeGeneration(LGeneration);
      LSnapshotObject := TNativeSignalSnapshot.Create(LGeneration, LEvents, LRevision);
      LSnapshotLease := LSnapshotObject;
      RequireNativeGeneration(LGeneration);

      if NyxSupportsTextInput(FNode) and LEvents.HasSubscribers(ntBeforeTextInput) then
      begin
        RequireNativeGeneration(LGeneration);
        LProposal := FRenderer.FLiveBindings.ProposeText(FNode, LValue);
        RequireNativeGeneration(LGeneration);
        LProposal.Info.HasEditing := LEditing.Defined;
        RequireNativeGeneration(LGeneration);
        LProposal.Info.Editing := LEditing;
        LConsumed := DispatchNyxInput(LEvents, FNode, LProposal);
        RequireNativeGeneration(LGeneration);

        if LEvents.ViewRevision <> LRevision then
        begin
          RequireNativeGeneration(LGeneration);
          Exit;
        end;
        RequireNativeGeneration(LGeneration);

        if LConsumed then
        begin
          FRenderer.FForceValues := True;
          try
            FRenderer.Sync;
            RequireNativeGeneration(LGeneration);
          finally

            if NativeGenerationCurrent(LGeneration) then
          begin
            FRenderer.FForceValues := False;
          end;
          end;
          RequireNativeGeneration(LGeneration);
          LProposal.Info.DefaultPrevented := True;
          DispatchNyxTextResult(LEvents, FNode, LProposal,
            LSnapshotObject.Read);
            RequireNativeGeneration(LGeneration);
          Exit;
        end;
      end;
      RequireNativeGeneration(LGeneration);
      try
        LDispatch := FRenderer.FLiveBindings.Edit(FNode, LValue);
        RequireNativeGeneration(LGeneration);
        LDispatch.Info.HasEditing := LEditing.Defined;
        RequireNativeGeneration(LGeneration);
        LDispatch.Info.Editing := LEditing;
        FRenderer.FLastBindingError := '';
        FRenderer.FLastBindingFailure := nbfNone;
      except
        on ENyxNativeGenerationRetired do raise;
        on LException: ENyxStateNotification do
        begin
          { An old notification may have accepted a successor before raising.
            Its diagnostic belongs only to the original entered generation. }
          RequireNativeGeneration(LGeneration);
          FRenderer.BindingFailed(FNode, LException.Message, nbfNotificationFailed);
          RequireNativeGeneration(LGeneration);
          Exit;
        end;
        on LException: Exception do
        begin
          { An old notification may have accepted a successor before raising.
            Its diagnostic belongs only to the original entered generation. }
          RequireNativeGeneration(LGeneration);
          FRenderer.BindingFailed(FNode, LException.Message);
          RequireNativeGeneration(LGeneration);
          Exit;
        end;
      end;

      FRenderer.Emit(FNode, LDispatch);
      RequireNativeGeneration(LGeneration);
    except
      on ENyxNativeGenerationRetired do
      begin
        { The original generation retired; only retained locals may unwind. }
      end;
    end;
  finally
    LEnteredFrame.Leave;
  end;
end;


function TNyxLCLRenderer.EmitNamed(const AOriginID: TNyxText;
  const AName: TNyxEventRef; const APayload: TNyxDataValue;
  AHasPayload: Boolean): Boolean;
var
  LBinding: TNyxLCLBinding;
  LDispatch: TNyxDispatch;
begin
  Result := False;

  if FDesignMode or FUpdating or FPublishingContent then
  begin
    Exit;
  end;
  LBinding := IdentityBinding(AOriginID, niRuntime);
  LDispatch := DispatchNyxNamedEvent(LBinding.FNode, AName, APayload,
    AHasPayload, npfNativeLCL);
  Result := LDispatch.EventName <> '';

  if Result then
  begin
    Emit(LBinding.FNode, LDispatch);
  end;
  { Navigation/disposal may occur in Emit. No borrowed fields are read here. }
end;

procedure TNyxLCLBinding.CollectionSelectionChanged(
  const ABefore, AAfter: INyxCollectionSelection);
var
  LGeneration: INyxLCLBindingGeneration;
  LEnteredFrame: INyxNativeGestureFrame;
  LDispatch: TNyxDispatch;
begin
  LGeneration := GenerationLease;

  if (LGeneration = nil) or not NativeGenerationCurrent(LGeneration) then
  begin
    Exit;
  end;
  LEnteredFrame := LGeneration.PhysicalFrame;
  LEnteredFrame.Enter;
  try
    try

      if FRenderer.FUpdating or FRenderer.FDesignMode then
      begin
        Exit;
      end;
      LDispatch := FRenderer.FLiveBindings.Signal(FNode, ntSelectionChange);
      RequireNativeGeneration(LGeneration);
      LDispatch.Info.HasCollectionSelection := True;
      LDispatch.Info.SelectionBefore := ABefore.Snapshot;
      RequireNativeGeneration(LGeneration);
      LDispatch.Info.Selection := AAfter.Snapshot;
      RequireNativeGeneration(LGeneration);
      FRenderer.Emit(FNode, LDispatch);
      RequireNativeGeneration(LGeneration);
    except
      on ENyxNativeGenerationRetired do
      begin
        { The original generation retired; only retained locals may unwind. }
      end;
    end;
  finally
    LEnteredFrame.Leave;
  end;
end;


procedure TNyxLCLRenderer.Emit(AOrigin: TNyxNode; const ADispatch: TNyxDispatch);
var
  LSnapshotObject: TNativeSignalSnapshot;
  LSnapshotLease: IInterface;
  LGeneration: INyxLCLBindingGeneration;
  LEnteredFrame: INyxNativeGestureFrame;
  LEvents: INyxEvents;
  LInfo: TNyxEventInfo;
  LLegacy: TNyxLCLEvent;
  LRevision: Integer;
  LLegacyClick: Boolean;
  LContentGuard: INyxContentGuard;
  LIndex: Integer;
  LPager: INyxCollectionPager;
begin
  LGeneration := FGeneration;

  if (LGeneration = nil) or not NativeGenerationCurrent(LGeneration) then
  begin
    Exit;
  end;
  LEnteredFrame := FPhysicalFrame;
  LEnteredFrame.Enter;
  try
    try

      if FDesignMode or FUpdating or FPublishingContent or (ADispatch.EventName = '') then
      begin
        RequireNativeGeneration(LGeneration);
        Exit;
      end;
      RequireNativeGeneration(LGeneration);
      LEvents := FEvents;
      RequireNativeGeneration(LGeneration);
      LLegacy := FOnEvent;
      LLegacyClick := NyxHasLegacyClick(AOrigin);
      RequireNativeGeneration(LGeneration);
      LRevision := LEvents.ViewRevision;
      RequireNativeGeneration(LGeneration);
      LSnapshotObject := TNativeSignalSnapshot.Create(LGeneration, LEvents, LRevision);
      LSnapshotLease := LSnapshotObject;
      RequireNativeGeneration(LGeneration);
      LContentGuard := FContentGuard;
      RequireNativeGeneration(LGeneration);
      LContentGuard.Enter;
      RequireNativeGeneration(LGeneration);
      AOrigin.AcquireReference;
      RequireNativeGeneration(LGeneration);
      ADispatch.Source.AcquireReference;
      RequireNativeGeneration(LGeneration);
      try
        for LIndex := 0 to High(FBindings) do
        begin

          if FBindings[LIndex].FNode = ADispatch.Source then
          begin
            RequireNativeGeneration(LGeneration);
            LPager := FBindings[LIndex].FCollectionPager;
            RequireNativeGeneration(LGeneration);
            Break;
          end;
          RequireNativeGeneration(LGeneration);
        end;
        RequireNativeGeneration(LGeneration);

        if LPager <> nil then
        begin
          LPager.ApplyEvent(ADispatch.Info);
          RequireNativeGeneration(LGeneration);

          if LEvents.ViewRevision <> LRevision then
          begin
            RequireNativeGeneration(LGeneration);
            Exit;
          end;
          RequireNativeGeneration(LGeneration);
        end;
        { Freeze the selected record before the router captures queued callbacks. }
        LInfo := EnrichNyxCoordinatorEvent(FCoordinatorMounts, ADispatch.Info);
        RequireNativeGeneration(LGeneration);

        if LEvents.ViewRevision <> LRevision then
        begin
          RequireNativeGeneration(LGeneration);
          Exit;
        end;
        RequireNativeGeneration(LGeneration);
        LEvents.Dispatch(LInfo, AOrigin.DesignID, ADispatch.Source.DesignID);
        RequireNativeGeneration(LGeneration);

        if ADispatch.Info.HasTextEdit and (LEvents.ViewRevision = LRevision) then
        begin
          RequireNativeGeneration(LGeneration);
          DispatchNyxTextResult(LEvents, AOrigin, ADispatch, LSnapshotObject.Read);
          RequireNativeGeneration(LGeneration);
        end;
        RequireNativeGeneration(LGeneration);

        if Assigned(LLegacy) and (ADispatch.Info.Trigger in [ntClick, ntChange, ntNamed]) and
          ((ADispatch.Info.Trigger <> ntClick) or LLegacyClick) and
          (LEvents.ViewRevision = LRevision) then
        begin
          RequireNativeGeneration(LGeneration);
          LLegacy(ADispatch.Source, LInfo);
          RequireNativeGeneration(LGeneration);
        end;
        RequireNativeGeneration(LGeneration);
      finally
        ADispatch.Source.ReleaseReference;
        AOrigin.ReleaseReference;
        LContentGuard.Leave;
      end;
      RequireNativeGeneration(LGeneration);
    except
      on ENyxNativeGenerationRetired do
      begin
        { The original generation retired; only retained locals may unwind. }
      end;
    end;
  finally
    LEnteredFrame.Leave;
  end;
end;

{ Opt-in NYX_FOCUS_TRACE samples before dispatch, which can retire this binding.
  Ordinary builds contain no diagnostic output or additional focus operation. }


procedure TNyxLCLBinding.Enter(ASender: TObject);
var
  LGeneration: INyxLCLBindingGeneration;
  LEnteredFrame: INyxNativeGestureFrame;
  LEvents: INyxEvents;
  LRevision: Integer;
  LChanged: Boolean;
begin
  LGeneration := GenerationLease;

  if (LGeneration = nil) or not NativeGenerationCurrent(LGeneration) then
  begin
    Exit;
  end;
  LEnteredFrame := LGeneration.PhysicalFrame;
  LEnteredFrame.Enter;
  try
    try

      if FFocusControl is TNyxCollectionStringGrid then
      begin

        if TNyxCollectionStringGrid(FFocusControl).ColumnTransitionActive then
        begin
          RequireNativeGeneration(LGeneration);
          Exit;
        end;
        RequireNativeGeneration(LGeneration);

        if TNyxCollectionStringGrid(FFocusControl).HasColumnComposite then
        begin
          RequireNativeGeneration(LGeneration);
          ObserveFocus(ntAfterEnter);
          RequireNativeGeneration(LGeneration);
          Exit;
        end;
        RequireNativeGeneration(LGeneration);
      end;
      {$ifdef NYX_FOCUS_TRACE}
      WriteLn('FOCUS logical enter ', FNode.ID, ' before=', FHasFocus,
        ' native=', LCLIntf.GetFocus, ' updating=', FRenderer.FUpdating);
                  RequireNativeGeneration(LGeneration);
      {$endif}
      LChanged := not FHasFocus;
      FHasFocus := True;

      if not FRenderer.FUpdating and
        not FRenderer.FFocusReplay.Preserves(FNode.ID) then
      begin
        RequireNativeGeneration(LGeneration);
        { Native Tab assigns focus without scrolling nested ordinary containers.
          Reveal before user callbacks, so their later focus/scroll choice wins.
          Transactional replay and explicit viewport-preserving focus stay neutral. }
        FRenderer.Reveal(FNode.ID, niRuntime);
        RequireNativeGeneration(LGeneration);
      end;
      RequireNativeGeneration(LGeneration);

      if FRenderer.FDesignMode or FRenderer.FUpdating then
      begin
        Exit;
      end;
      LEvents := FRenderer.FEvents;
      RequireNativeGeneration(LGeneration);
      LRevision := LEvents.ViewRevision;
      RequireNativeGeneration(LGeneration);
      Focus(ASender);
      RequireNativeGeneration(LGeneration);

      if Assigned(FPreviousEnter) then
      begin
        RequireNativeGeneration(LGeneration);
        FPreviousEnter(ASender);
        RequireNativeGeneration(LGeneration);
      end;
      RequireNativeGeneration(LGeneration);

      if LEvents.ViewRevision <> LRevision then
      begin
        RequireNativeGeneration(LGeneration);
        Exit;
      end;
      RequireNativeGeneration(LGeneration);

      if LChanged and not FRenderer.FUpdating and LEvents.HasSubscribers(ntAfterEnter) then
      begin
        RequireNativeGeneration(LGeneration);
        FRenderer.Emit(FNode, FRenderer.FLiveBindings.Focus(FNode, ntAfterEnter));
        RequireNativeGeneration(LGeneration);
      end;
      RequireNativeGeneration(LGeneration);
    except
      on ENyxNativeGenerationRetired do
      begin
        { The original generation retired; only retained locals may unwind. }
      end;
    end;
  finally
    LEnteredFrame.Leave;
  end;
end;


procedure TNyxLCLBinding.Leave(ASender: TObject);
var
  LGeneration: INyxLCLBindingGeneration;
  LEnteredFrame: INyxNativeGestureFrame;
  LEvents: INyxEvents;
  LRevision: Integer;
  LIndex: Integer;
  LChanged: Boolean;
begin
  LGeneration := GenerationLease;

  if (LGeneration = nil) or not NativeGenerationCurrent(LGeneration) then
  begin
    Exit;
  end;
  LEnteredFrame := LGeneration.PhysicalFrame;
  LEnteredFrame.Enter;
  try
    try

      if FFocusControl is TNyxCollectionStringGrid then
      begin

        if TNyxCollectionStringGrid(FFocusControl).ColumnTransitionActive then
        begin
          RequireNativeGeneration(LGeneration);
          Exit;
        end;
        RequireNativeGeneration(LGeneration);

        if TNyxCollectionStringGrid(FFocusControl).HasColumnComposite then
        begin
          RequireNativeGeneration(LGeneration);
          ObserveFocus(ntAfterExit);
          RequireNativeGeneration(LGeneration);
          Exit;
        end;
        RequireNativeGeneration(LGeneration);
      end;
      {$ifdef NYX_FOCUS_TRACE}
      WriteLn('FOCUS logical exit ', FNode.ID, ' before=', FHasFocus,
        ' native=', LCLIntf.GetFocus, ' updating=', FRenderer.FUpdating);
                  RequireNativeGeneration(LGeneration);
      {$endif}
      LChanged := FHasFocus;
      FHasFocus := False;
      for LIndex := Low(FPressedKeys) to High(FPressedKeys) do
      begin
        FPressedKeys[LIndex] := False;
      end;
      RequireNativeGeneration(LGeneration);

      if FRenderer.FDesignMode or FRenderer.FUpdating then
      begin
        Exit;
      end;
      LEvents := FRenderer.FEvents;
      RequireNativeGeneration(LGeneration);
      LRevision := LEvents.ViewRevision;
      RequireNativeGeneration(LGeneration);
      Focus(ASender);
      RequireNativeGeneration(LGeneration);

      if Assigned(FPreviousExit) then
      begin
        RequireNativeGeneration(LGeneration);
        FPreviousExit(ASender);
        RequireNativeGeneration(LGeneration);
      end;
      RequireNativeGeneration(LGeneration);

      if LEvents.ViewRevision <> LRevision then
      begin
        RequireNativeGeneration(LGeneration);
        Exit;
      end;
      RequireNativeGeneration(LGeneration);

      if LChanged and not FRenderer.FUpdating and LEvents.HasSubscribers(ntAfterExit) then
      begin
        RequireNativeGeneration(LGeneration);
        FRenderer.Emit(FNode, FRenderer.FLiveBindings.Focus(FNode, ntAfterExit));
        RequireNativeGeneration(LGeneration);
      end;
      RequireNativeGeneration(LGeneration);
    except
      on ENyxNativeGenerationRetired do
      begin
        { The original generation retired; only retained locals may unwind. }
      end;
    end;
  finally
    LEnteredFrame.Leave;
  end;
end;


procedure TNyxLCLBinding.ObserveFocus(ATrigger: TNyxTrigger);
var
  LGeneration: INyxLCLBindingGeneration;
  LEnteredFrame: INyxNativeGestureFrame;
  LEntered: Boolean;
  LActualFocus: Boolean;
  LIndex: Integer;
  LComposite: Boolean;
  LEvents: INyxEvents;
  LRevision: Integer;
  LPrevious: TNotifyEvent;
begin
  LGeneration := GenerationLease;

  if (LGeneration = nil) or not NativeGenerationCurrent(LGeneration) then
  begin
    Exit;
  end;
  LEnteredFrame := LGeneration.PhysicalFrame;
  LEnteredFrame.Enter;
  try
    try

      LEntered := ATrigger = ntAfterEnter;
      {$ifdef NYX_FOCUS_TRACE}
      WriteLn('FOCUS native ', FNode.ID, ' enter=', LEntered, ' before=', FHasFocus,
        ' native=', LCLIntf.GetFocus, ' updating=', FRenderer.FUpdating);
                  RequireNativeGeneration(LGeneration);
      {$endif}

      if (FFocusControl is TNyxCollectionStringGrid) and
        TNyxCollectionStringGrid(FFocusControl).ColumnTransitionActive then
      begin
        RequireNativeGeneration(LGeneration);
        Exit;
      end;
      RequireNativeGeneration(LGeneration);
      LActualFocus := (FFocusControl <> nil) and FFocusControl.Focused;
      RequireNativeGeneration(LGeneration);
      LComposite := (FFocusControl is TNyxCollectionStringGrid) and
        TNyxCollectionStringGrid(FFocusControl).HasColumnComposite;
        RequireNativeGeneration(LGeneration);

      if LComposite then
      begin
        { Crossing between a middle editor and an owned pinned editor stays inside
          ONE logical Nyx control. Native child focus must not fabricate Exit/Enter. }
        LActualFocus := TNyxCollectionStringGrid(FFocusControl).HasOwnedColumnFocus;
        RequireNativeGeneration(LGeneration);
      end;

      if (FFocusControl = nil) or (LActualFocus <> LEntered) then
      begin
        { Native/default handling may have already redirected or restored focus.
          LCL also reaffirms the active editor with a synthetic kill/set pair whose
          kill message names that same editor as the next target. Only the admitted
          keyboard surface's actual post-handler focus may change our baseline;
          otherwise that stale exit would fabricate a second returning callback.
          Focused resolves widgetset-owned inner handles through LCL, preserving
          composite controls without assuming a Win32 HWND or disturbing focus. }
        Exit;
      end;
      RequireNativeGeneration(LGeneration);

      if FHasFocus = LEntered then
      begin
        Exit;
      end;
      FHasFocus := LEntered;

      if not LEntered then
      begin
        for LIndex := Low(FPressedKeys) to High(FPressedKeys) do
        begin
          FPressedKeys[LIndex] := False;
        end;
        RequireNativeGeneration(LGeneration);
      end;

      if FRenderer.FDesignMode or FRenderer.FUpdating then
      begin
        Exit;
      end;
      LEvents := FRenderer.FEvents;
      RequireNativeGeneration(LGeneration);
      LRevision := LEvents.ViewRevision;
      RequireNativeGeneration(LGeneration);

      if LComposite and LEntered and
        not FRenderer.FFocusReplay.Preserves(FNode.ID) then
      begin
        RequireNativeGeneration(LGeneration);
        FRenderer.Reveal(FNode.ID, niRuntime);
        RequireNativeGeneration(LGeneration);

        if LEvents.ViewRevision <> LRevision then
        begin
          RequireNativeGeneration(LGeneration);
          Exit;
        end;
        RequireNativeGeneration(LGeneration);
      end;
      RequireNativeGeneration(LGeneration);
      Focus(FFocusControl);
      RequireNativeGeneration(LGeneration);

      if LComposite then
      begin
        LPrevious := FPreviousExit;

        if LEntered then
        begin
          LPrevious := FPreviousEnter;
        end;

        if Assigned(LPrevious) then
        begin
          RequireNativeGeneration(LGeneration);
          LPrevious(FFocusControl);
          RequireNativeGeneration(LGeneration);

          if LEvents.ViewRevision <> LRevision then
          begin
            RequireNativeGeneration(LGeneration);
            Exit;
          end;
          RequireNativeGeneration(LGeneration);
        end;
        RequireNativeGeneration(LGeneration);
      end;

      if FRenderer.FEvents.HasSubscribers(ATrigger) then
      begin
        RequireNativeGeneration(LGeneration);
        { Emit can unmount this binding. No binding/widget access follows it. The
          caller's native physical frame owns safe deferred control retirement. }
        FRenderer.Emit(FNode, FRenderer.FLiveBindings.Focus(FNode, ATrigger));
        RequireNativeGeneration(LGeneration);
      end;
      RequireNativeGeneration(LGeneration);
    except
      on ENyxNativeGenerationRetired do
      begin
        { The original generation retired; only retained locals may unwind. }
      end;
    end;
  finally
    LEnteredFrame.Leave;
  end;
end;


procedure TNyxLCLBinding.KeyDown(ASender: TObject; var AKey: Word; AShift: TShiftState);
begin
  Keyboard(ASender, AKey, AShift, ntKeyDown);
end;

procedure TNyxLCLBinding.KeyUp(ASender: TObject; var AKey: Word; AShift: TShiftState);
begin
  Keyboard(ASender, AKey, AShift, ntKeyUp);
end;

procedure TNyxLCLBinding.Keyboard(ASender: TObject; var AKey: Word;
  AShift: TShiftState; ATrigger: TNyxTrigger);
var
  LSnapshotObject: TNativeSignalSnapshot;
  LSnapshotLease: IInterface;
  LGeneration: INyxLCLBindingGeneration;
  LEnteredFrame: INyxNativeGestureFrame;
  LEvents: INyxEvents;
  LRevision: Integer;
  LPrevious: TKeyEvent;
  LModifiers: TNyxKeyModifiers;
  LRepeating: Boolean;
  LDispatch: TNyxDispatch;
  LConsumed: Boolean;
begin
  LGeneration := GenerationLease;

  if (LGeneration = nil) or not NativeGenerationCurrent(LGeneration) then
  begin
    AKey := 0;
    Exit;
  end;
  LEnteredFrame := LGeneration.PhysicalFrame;
  LEnteredFrame.Enter;
  try
    try

      if FRenderer.FDesignMode or FRenderer.FUpdating then
      begin
        Exit;
      end;
      LEvents := FRenderer.FEvents;
      RequireNativeGeneration(LGeneration);
      LRevision := LEvents.ViewRevision;
      RequireNativeGeneration(LGeneration);
      LSnapshotObject := TNativeSignalSnapshot.Create(LGeneration, LEvents, LRevision);
      LSnapshotLease := LSnapshotObject;
      RequireNativeGeneration(LGeneration);
      LPrevious := FPreviousKeyDown;
      LRepeating := False;

      if ATrigger = ntKeyUp then
      begin
        LPrevious := FPreviousKeyUp;
      end;

      if AKey <= High(FPressedKeys) then
      begin
        LRepeating := (ATrigger = ntKeyDown) and FPressedKeys[AKey];
        RequireNativeGeneration(LGeneration);
        FPressedKeys[AKey] := ATrigger = ntKeyDown;
      end;

      if Assigned(LPrevious) then
      begin
        RequireNativeGeneration(LGeneration);
        LPrevious(ASender, AKey, AShift);
        RequireNativeGeneration(LGeneration);
      end;
      RequireNativeGeneration(LGeneration);

      if LEvents.ViewRevision <> LRevision then
      begin
        RequireNativeGeneration(LGeneration);
        { The custom hook disposed this binding; touch only the caller's key now. }
        AKey := 0;
        Exit;
      end;
      RequireNativeGeneration(LGeneration);
      { Preserve consumed custom keys and the widgetset's IME/process/packet input.
        Those are text-composition messages, not ordinary shortcut keys. }

      if FComposing or (AKey = 0) or (AKey = $E5) or (AKey = $E7) or FRenderer.FUpdating or
        not NyxHasKeyboardSubscribers(LEvents, ATrigger) then
      begin
        RequireNativeGeneration(LGeneration);
        Exit;
      end;
      RequireNativeGeneration(LGeneration);
      LModifiers := [];

      if ssShift in AShift then
      begin
        Include(LModifiers, nmShift);
      end;

      if ssCtrl in AShift then
      begin
        Include(LModifiers, nmControl);
      end;

      if ssAlt in AShift then
      begin
        Include(LModifiers, nmAlt);
      end;

      if (ssMeta in AShift) or (ssSuper in AShift) then
      begin
        Include(LModifiers, nmMeta);
      end;
      RequireNativeGeneration(LGeneration);

      if ssAltGr in AShift then
      begin
        Include(LModifiers, nmAltGraph);
      end;
      LDispatch := FRenderer.FLiveBindings.Keyboard(FNode, ATrigger,
        NyxKeyStroke(NyxKeyFromVirtualCode(AKey), LModifiers, LRepeating));
        RequireNativeGeneration(LGeneration);
      LConsumed := DispatchNyxKeyboard(LEvents, FNode, LDispatch,
        LSnapshotObject.Read);
        RequireNativeGeneration(LGeneration);

      if LConsumed or (LEvents.ViewRevision <> LRevision) then
      begin
        RequireNativeGeneration(LGeneration);
        AKey := 0;
      end;
      RequireNativeGeneration(LGeneration);
    except
      on ENyxNativeGenerationRetired do
      begin
        AKey := 0;
      end;
    end;
  finally
    LEnteredFrame.Leave;
  end;
end;


procedure TNyxLCLBinding.CommitValue(ASender: TObject);
var
  LGeneration: INyxLCLBindingGeneration;
  LEnteredFrame: INyxNativeGestureFrame;
begin
  LGeneration := GenerationLease;

  if (LGeneration = nil) or not NativeGenerationCurrent(LGeneration) then
  begin
    Exit;
  end;
  LEnteredFrame := LGeneration.PhysicalFrame;
  LEnteredFrame.Enter;
  try
    try

      FCommitting := True;
      try
        Change(ASender);
        RequireNativeGeneration(LGeneration);
      finally
        FCommitting := False;
      end;
      RequireNativeGeneration(LGeneration);
    except
      on ENyxNativeGenerationRetired do
      begin
        { The original generation retired; only retained locals may unwind. }
      end;
    end;
  finally
    LEnteredFrame.Leave;
  end;
end;


procedure TNyxLCLRenderer.BindingFailed(ANode: TNyxNode; const AReason: TNyxText;
  AFailure: TNyxBindingFailure; ARestoreInput: Boolean);
var
  LGeneration: INyxLCLBindingGeneration;
  LEnteredFrame: INyxNativeGestureFrame;
begin
  LGeneration := FGeneration;

  if (LGeneration = nil) or not NativeGenerationCurrent(LGeneration) then
  begin
    Exit;
  end;
  LEnteredFrame := FPhysicalFrame;
  LEnteredFrame.Enter;
  try
    try

      FLastBindingError := AReason;
      FLastBindingFailure := AFailure;

      if (AFailure = nbfRejected) and ARestoreInput then
      begin
        { A rejected date changes only its own physical draft. Other unchanged
          date fields may hold incomplete text that must survive this rejection. }

        if (ANode <> nil) and (ANode.ProjectionKind = NyxKindName(nkDate)) then
        begin
          RequireNativeGeneration(LGeneration);
          FForceValueNode := ANode;
        end;
        RequireNativeGeneration(LGeneration);
        FForceValues := True;
        try
          Sync;
          RequireNativeGeneration(LGeneration);
        finally

          if NativeGenerationCurrent(LGeneration) then
          begin
            FForceValues := False;
            FForceValueNode := nil;
          end;
        end;
        RequireNativeGeneration(LGeneration);
      end;
      RequireNativeGeneration(LGeneration);

      if Assigned(FOnBindingError) then
      begin
        RequireNativeGeneration(LGeneration);
        FOnBindingError(ANode, AReason, AFailure);
        RequireNativeGeneration(LGeneration);
      end;
      RequireNativeGeneration(LGeneration);
    except
      on ENyxNativeGenerationRetired do
      begin
        { The original generation retired; only retained locals may unwind. }
      end;
    end;
  finally
    LEnteredFrame.Leave;
  end;
end;


procedure TNyxLCLBinding.RefreshPager;
var
  LGeneration: INyxLCLBindingGeneration;
  LEnteredFrame: INyxNativeGestureFrame;
begin
  LGeneration := GenerationLease;

  if (LGeneration = nil) or not NativeGenerationCurrent(LGeneration) then
  begin
    Exit;
  end;
  LEnteredFrame := LGeneration.PhysicalFrame;
  LEnteredFrame.Enter;
  try
    try

      FRenderer.Sync;
      RequireNativeGeneration(LGeneration);
    except
      on ENyxNativeGenerationRetired do
      begin
        { The original generation retired; only retained locals may unwind. }
      end;
    end;
  finally
    LEnteredFrame.Leave;
  end;
end;


function TNyxLCLRenderer.CollectionPager(const AID: TNyxText): INyxCollectionPager;
var
  LIndex: Integer;
begin
  Result := nil;
  for LIndex := 0 to High(FBindings) do
  begin

    if FBindings[LIndex].FNode.ID = AID then
    begin
      Exit(FBindings[LIndex].FCollectionPager);
    end;
  end;
end;

function TNyxLCLRenderer.KanbanBinding(const AID: TNyxText): INyxKanbanBinding;
var
  LBinding: INyxCoordinatorBinding;
begin
  LBinding := CoordinatorBinding(AID);
  Result := nil;
  Supports(LBinding, INyxKanbanBinding, Result);
end;

function TNyxLCLRenderer.CoordinatorBinding(const AID: TNyxText): INyxCoordinatorBinding;
begin
  Result := FindNyxCoordinatorMount(FCoordinatorMounts, FCoordinatorIDs, AID);
end;

function TNyxLCLRenderer.MasterDetailBinding(const AID: TNyxText): INyxMasterDetailBinding;
var
  LBinding: INyxCoordinatorBinding;
begin
  LBinding := CoordinatorBinding(AID);
  Result := nil;
  Supports(LBinding, INyxMasterDetailBinding, Result);
end;

procedure TNyxLCLRenderer.BindCollectionPagers;
var
  LIndex: Integer;
  LBinding: TNyxLCLBinding;
  LControl: INyxControl;
  LPager: INyxPagination;
begin
  for LIndex := 0 to High(FBindings) do
  begin
    LBinding := FBindings[LIndex];

    if LBinding.FNode.Prop(NyxAttributeName(atPageView)) = '' then
    begin
      Continue;
    end;
    LControl := RetainNyxControl(LBinding.FNode);

    if not Supports(LControl, INyxPagination) then
    begin
      raise ENyxModel.Create('PageView configuration requires a specialized pagination control');
    end;
    LPager := LControl as INyxPagination;
    LBinding.FCollectionPager := NewNyxCollectionPager(LPager,
      NyxCollectionPagerView(FRoot, LBinding.FNode, FCollectionBindings),
      FEvents.Scheduler, LBinding.RefreshPager);
  end;
end;

procedure TNyxLCLBinding.SyncLiteralItems;
var
  LGeneration: INyxLCLBindingGeneration;
  LKind: TNyxKind;
  LText: TNyxText;
  LSelected: TNyxText;
  LRows: TNyxStrings;
  LCells: TNyxStrings;
  LList: TListBox;
  LChoice: TComboBox;
  LTree: TTreeView;
  LGrid: TStringGrid;
  LIndex: Integer;
  LColumn: Integer;
  LColumns: Integer;
  LOldRow: Integer;
  LOldColumn: Integer;
  LOldTop: Integer;
begin
  LGeneration := GenerationLease;
  RequireNativeGeneration(LGeneration);

  if FCustom or (FCollectionMount <> nil) or
    not TryNyxKind(FNode.ProjectionKind, LKind) or
    not (LKind in [nkSelect, nkList, nkTable, nkTree]) then
  begin
    RequireNativeGeneration(LGeneration);
    Exit;
  end;
  RequireNativeGeneration(LGeneration);
  LText := FNode.Prop('items');
  RequireNativeGeneration(LGeneration);

  if FHasItemsBaseline and (FLastItems = LText) then
  begin
    Exit;
  end;
  RequireNativeGeneration(LGeneration);
  LRows := TNyxStrings.Create;
  RequireNativeGeneration(LGeneration);
  try
    LRows.Text := LText;
    case LKind of
      nkSelect:
        begin
          LChoice := TComboBox(FInput);
          RequireNativeGeneration(LGeneration);
          LSelected := FNode.Prop('value');
          RequireNativeGeneration(LGeneration);

          if FHasValueBaseline and (FLastValue = LSelected) then
          begin
            LSelected := LChoice.Text;
            RequireNativeGeneration(LGeneration);
          end;
          RequireNativeGeneration(LGeneration);
          LChoice.Items.BeginUpdate;
          RequireNativeGeneration(LGeneration);
          try
            LChoice.Items.Clear;
            RequireNativeGeneration(LGeneration);
            for LIndex := 0 to LRows.Count - 1 do
            begin
              RequireNativeGeneration(LGeneration);
              LChoice.Items.Add(LRows[LIndex]);
              RequireNativeGeneration(LGeneration);
            end;
            RequireNativeGeneration(LGeneration);
            LChoice.ItemIndex := LChoice.Items.IndexOf(LSelected);
            RequireNativeGeneration(LGeneration);

            if (LChoice.ItemIndex < 0) and FHasValueBaseline and
              (FLastValue = FNode.Prop('value')) and
              (LChoice.Style <> csDropDownList) then
            begin
              RequireNativeGeneration(LGeneration);
              { The existing editable native choice can have an unfinished
                draft. A row publication does not replace that physical text. }
              LChoice.Text := LSelected;
            end;
            RequireNativeGeneration(LGeneration);
          finally
            LChoice.Items.EndUpdate;
          end;
          RequireNativeGeneration(LGeneration);
        end;
      nkList:
        begin
          LList := TListBox(FControl);
          RequireNativeGeneration(LGeneration);
          LSelected := '';
          LOldTop := LList.TopIndex;
          RequireNativeGeneration(LGeneration);

          if LList.ItemIndex >= 0 then
          begin
            RequireNativeGeneration(LGeneration);
            LSelected := LList.Items[LList.ItemIndex];
            RequireNativeGeneration(LGeneration);
          end;
          RequireNativeGeneration(LGeneration);
          LList.Items.BeginUpdate;
          RequireNativeGeneration(LGeneration);
          try
            LList.Items.Clear;
            RequireNativeGeneration(LGeneration);
            for LIndex := 0 to LRows.Count - 1 do
            begin
              RequireNativeGeneration(LGeneration);
              LList.Items.Add(LRows[LIndex]);
              RequireNativeGeneration(LGeneration);
            end;
            RequireNativeGeneration(LGeneration);
            LList.ItemIndex := LList.Items.IndexOf(LSelected);
            RequireNativeGeneration(LGeneration);

            if LRows.Count > 0 then
            begin
              RequireNativeGeneration(LGeneration);
              LList.TopIndex := Min(LOldTop, LRows.Count - 1);
              RequireNativeGeneration(LGeneration);
            end;
            RequireNativeGeneration(LGeneration);
          finally
            LList.Items.EndUpdate;
          end;
          RequireNativeGeneration(LGeneration);
        end;
      nkTree:
        begin
          LTree := TTreeView(FControl);
          RequireNativeGeneration(LGeneration);
          LSelected := '';

          if LTree.Selected <> nil then
          begin
            RequireNativeGeneration(LGeneration);
            LSelected := LTree.Selected.Text;
            RequireNativeGeneration(LGeneration);
          end;
          RequireNativeGeneration(LGeneration);
          LTree.Items.BeginUpdate;
          RequireNativeGeneration(LGeneration);
          try
            LTree.Items.Clear;
            RequireNativeGeneration(LGeneration);
            for LIndex := 0 to LRows.Count - 1 do
            begin
              RequireNativeGeneration(LGeneration);
              LTree.Items.Add(nil, LRows[LIndex]);
              RequireNativeGeneration(LGeneration);
            end;
            RequireNativeGeneration(LGeneration);

            if LSelected <> '' then
            begin
              LTree.Selected := LTree.Items.FindNodeWithText(LSelected);
              RequireNativeGeneration(LGeneration);
            end;
          finally
            LTree.Items.EndUpdate;
          end;
          RequireNativeGeneration(LGeneration);
        end;
      nkTable:
        begin
          LGrid := TStringGrid(FControl);
          RequireNativeGeneration(LGeneration);
          LOldRow := LGrid.Row;
          RequireNativeGeneration(LGeneration);
          LOldColumn := LGrid.Col;
          RequireNativeGeneration(LGeneration);
          LColumns := 1;
          RequireNativeGeneration(LGeneration);
          for LIndex := 0 to LRows.Count - 1 do
          begin
            RequireNativeGeneration(LGeneration);
            LCells := NyxLiteralCells(LRows[LIndex]);
            RequireNativeGeneration(LGeneration);
            try
              LColumns := Max(LColumns, LCells.Count);
              RequireNativeGeneration(LGeneration);
            finally
              LCells.Free;
            end;
            RequireNativeGeneration(LGeneration);
          end;
          RequireNativeGeneration(LGeneration);
          LGrid.BeginUpdate;
          RequireNativeGeneration(LGeneration);
          try
            LGrid.FixedRows := 0;
            LGrid.FixedCols := 0;
            LGrid.RowCount := Max(1, LRows.Count);
            RequireNativeGeneration(LGeneration);
            LGrid.ColCount := LColumns;
            { A widget needs one physical row even for empty Items. It contains
              no authored data. Clear retained cells when ragged rows shrink. }
            for LIndex := 0 to LGrid.RowCount - 1 do
            begin
              RequireNativeGeneration(LGeneration);
              for LColumn := 0 to LGrid.ColCount - 1 do
              begin
                RequireNativeGeneration(LGeneration);
                LGrid.Cells[LColumn, LIndex] := '';
                RequireNativeGeneration(LGeneration);
              end;
              RequireNativeGeneration(LGeneration);
            end;
            RequireNativeGeneration(LGeneration);
            for LIndex := 0 to LRows.Count - 1 do
            begin
              RequireNativeGeneration(LGeneration);
              LCells := NyxLiteralCells(LRows[LIndex]);
              RequireNativeGeneration(LGeneration);
              try
                for LColumn := 0 to LCells.Count - 1 do
                begin
                  RequireNativeGeneration(LGeneration);
                  LGrid.Cells[LColumn, LIndex] := LCells[LColumn];
                  RequireNativeGeneration(LGeneration);
                end;
                RequireNativeGeneration(LGeneration);
              finally
                LCells.Free;
              end;
              RequireNativeGeneration(LGeneration);
            end;
            RequireNativeGeneration(LGeneration);

            if LRows.Count > 1 then
            begin
              RequireNativeGeneration(LGeneration);
              LGrid.FixedRows := 1;
            end;
            RequireNativeGeneration(LGeneration);
            LGrid.Row := EnsureRange(LOldRow, LGrid.FixedRows, LGrid.RowCount - 1);
            RequireNativeGeneration(LGeneration);
            LGrid.Col := EnsureRange(LOldColumn, 0, LGrid.ColCount - 1);
            RequireNativeGeneration(LGeneration);
          finally
            LGrid.EndUpdate;
          end;
          RequireNativeGeneration(LGeneration);
        end;
      else
        begin
          { Non-collection faces have no literal rows to realize here. }
        end;
    end;
    FLastItems := LText;
    FHasItemsBaseline := True;
  finally
    LRows.Free;
  end;
  RequireNativeGeneration(LGeneration);
end;

procedure TNyxLCLBinding.SyncButtonFace;
var
  LGeneration: INyxLCLBindingGeneration;
  LPresentation: TNyxButtonPresentation;
  LSource: TNyxImageSource;
  LIdentity: TNyxText;
  LGlyph: TBitmap;
  LRequest: TNyxImageRequestID;
  LForeground: TColor;
  LSourceWidth: Integer;
  LSourceHeight: Integer;
  LSourceChanged: Boolean;
begin
  LGeneration := GenerationLease;
  RequireNativeGeneration(LGeneration);

  if FCustom or not (FControl is TNyxLCLButton) then
  begin
    Exit;
  end;
  RequireNativeGeneration(LGeneration);
  LPresentation := NyxButtonPresentationOf(FNode);
  RequireNativeGeneration(LGeneration);
  { Capture the admitted wire before resource resolution crosses an extension
    boundary. A callback changing the face must remain visible on the next Sync
    rather than marking its new wire as already painted with the old value. }
  LIdentity := FNode.Prop(NyxAttributeName(atButtonPresentation));
  RequireNativeGeneration(LGeneration);
  LSource := NyxButtonGlyphSource(FNode, LPresentation);
  RequireNativeGeneration(LGeneration);
  LForeground := TNyxControlAccess(FControl).Font.Color;
  RequireNativeGeneration(LGeneration);
  { NyxButtonPresentationOf has already admitted this exact effective wire.
    Comparing it avoids serializing/revalidating glyph bytes on every refresh.
    Keep source resolution above: unchanged selectors can acquire new bytes. }
  LIdentity := LIdentity + #10 + IntToStr(LForeground);
  RequireNativeGeneration(LGeneration);

  if (LIdentity = FLastButtonFace) and (LSource.ToWire = FLastButtonGlyphSource) then
  begin
    RequireNativeGeneration(LGeneration);
    Exit;
  end;
  RequireNativeGeneration(LGeneration);
  LGlyph := nil;
  RequireNativeGeneration(LGeneration);

  if not LPresentation.HasGlyph and (FImageLifecycle = nil) then
  begin
    RequireNativeGeneration(LGeneration);
    TNyxLCLButton(FControl).ApplyPresentation(LPresentation, nil);
    RequireNativeGeneration(LGeneration);
    FLastButtonFace := LIdentity;
    FLastButtonGlyphSource := LSource.ToWire;
    RequireNativeGeneration(LGeneration);
    Exit;
  end;
  RequireNativeGeneration(LGeneration);

  if FImageLifecycle = nil then
  begin
    FImageLifecycle := NewNyxImageLifecycle;
    RequireNativeGeneration(LGeneration);
  end;
  LSourceChanged := not FImageLifecycle.Current.Defined or
    (FImageLifecycle.Current.Source.ToWire <> LSource.ToWire);
    RequireNativeGeneration(LGeneration);
  { Loading belongs to source identity. A new ink, gap or caption policy must
    neither restart the request nor manufacture an error for a valid image. }

  if LSourceChanged then
  begin
    LRequest := FImageLifecycle.Start(LSource);
    RequireNativeGeneration(LGeneration);

    if LSource.Kind <> nisEmpty then
    begin
      RequireNativeGeneration(LGeneration);
      try
        FRenderer.FButtonGlyphCache.Dimensions(LSource, LSourceWidth, LSourceHeight);
        RequireNativeGeneration(LGeneration);

        if (LSourceWidth > 0) and (LSourceHeight > 0) then
        begin
          FImageLifecycle.Ready(LRequest, LSourceWidth, LSourceHeight);
          RequireNativeGeneration(LGeneration);
        end
        else
        begin
          RequireNativeGeneration(LGeneration);
          FImageLifecycle.Fail(LRequest, nifUnavailable, 'Button image is unavailable; caption retained');
          RequireNativeGeneration(LGeneration);
        end;
        RequireNativeGeneration(LGeneration);
      except
        on ENyxNativeGenerationRetired do raise;
        on E: Exception do
        begin
          FImageLifecycle.Fail(LRequest, nifDecode, 'Button image could not be decoded; caption retained');
          RequireNativeGeneration(LGeneration);
        end;
      end;
    end;
    RequireNativeGeneration(LGeneration);
  end;
  try
    try

      if (LPresentation.ContentMode <> nbcCaption) and
        (FImageLifecycle.Current.Phase = nipReady) then
      begin
        RequireNativeGeneration(LGeneration);
        LGlyph := FRenderer.FButtonGlyphCache.Prepare(LSource, LPresentation, LForeground);
        RequireNativeGeneration(LGeneration);
      end;
      RequireNativeGeneration(LGeneration);
      TNyxLCLButton(FControl).ApplyPresentation(LPresentation, LGlyph);
      RequireNativeGeneration(LGeneration);
    except
      on ENyxNativeGenerationRetired do raise;
      on E: Exception do
      begin
        { A crop can be invalid for this decoded image without invalidating the
          source's Ready observation. Keep the accessible command usable. }
        TNyxLCLButton(FControl).ApplyPresentation(LPresentation, nil);
        RequireNativeGeneration(LGeneration);
      end;
    end;
    FLastButtonFace := LIdentity;
    FLastButtonGlyphSource := LSource.ToWire;
    RequireNativeGeneration(LGeneration);
  finally
    LGlyph.Free;
  end;
  RequireNativeGeneration(LGeneration);
end;

procedure TNyxLCLBinding.SyncPicture;
var
  LGeneration: INyxLCLBindingGeneration;
  LSource: TNyxText;
  LPicture: TPicture;
  LRequest: TNyxImageRequestID;
  LPeer: INyxImageLifecycle;
begin
  LGeneration := GenerationLease;
  RequireNativeGeneration(LGeneration);

  if FCustom or not (FControl is TImage) then
  begin
    Exit;
  end;
  RequireNativeGeneration(LGeneration);
  FControl.AccessibleDescription := FNode.Prop('alt');
  RequireNativeGeneration(LGeneration);
  { Policy changes retain the source cache and physical LCL image. }

  if FControl is TNyxLCLImage then
  begin
    TNyxLCLImage(FControl).ConfigureImage(
      ReadNyxImageFit(FNode.Prop('image-fit', 'contain')),
      ReadNyxImageAnchor(FNode.Prop('image-position-x', 'center')),
      ReadNyxImageAnchor(FNode.Prop('image-position-y', 'center')));
      RequireNativeGeneration(LGeneration);
  end;
  LSource := FNode.Prop('src');
  RequireNativeGeneration(LGeneration);

  if FHasSourceBaseline and (FLastSource = LSource) and
    (FImageLifecycle <> nil) and
    (FImageLifecycle.Current.Source.ToWire = LSource) then
  begin
    RequireNativeGeneration(LGeneration);
    Exit;
  end;
  RequireNativeGeneration(LGeneration);

  if FImageLifecycle = nil then
  begin
    FImageLifecycle := NewNyxImageLifecycle;
    RequireNativeGeneration(LGeneration);
  end;
  LPeer := FImageLifecycle;
  RequireNativeGeneration(LGeneration);
  LRequest := LPeer.Start(TNyxImageSource.FromWire(LSource));
  RequireNativeGeneration(LGeneration);
  LPicture := nil;
  RequireNativeGeneration(LGeneration);
  try
    try

      if not FHasSourceBaseline or (FLastSource <> LSource) then
      begin
        LPicture := NewNyxLCLPicture(TNyxImageSource.FromWire(LSource));
        RequireNativeGeneration(LGeneration);
        { Decode on an independent picture before changing this accepted face. }
        TImage(FControl).Picture.Assign(LPicture);
        RequireNativeGeneration(LGeneration);
        FLastSource := LSource;
        RequireNativeGeneration(LGeneration);
        FHasSourceBaseline := True;
      end;
      RequireNativeGeneration(LGeneration);

      if LSource <> '' then
      begin

        if (TImage(FControl).Picture.Width > 0) and (TImage(FControl).Picture.Height > 0) then
        begin
          RequireNativeGeneration(LGeneration);
          LPeer.Ready(LRequest, TImage(FControl).Picture.Width, TImage(FControl).Picture.Height);
          RequireNativeGeneration(LGeneration);
        end
        else
        begin
          RequireNativeGeneration(LGeneration);
          { Missing/unsupported native locations keep their existing empty face
            semantics; report the observation without inventing network fetch. }
          LPeer.Fail(LRequest, nifUnavailable, 'Native image location is unavailable');
          RequireNativeGeneration(LGeneration);
        end;
        RequireNativeGeneration(LGeneration);
      end;
    except
      on ENyxNativeGenerationRetired do raise;
      on LException: Exception do
      begin
        LPeer.Fail(LRequest, nifDecode, 'Native decoder rejected the image request');
        RequireNativeGeneration(LGeneration);
        raise;
      end;
    end;
  finally
    LPicture.Free;
  end;
  RequireNativeGeneration(LGeneration);
end;

procedure TNyxLCLRenderer.SyncRadioFocus;
type
  { Borrowed only during synchronous projection; no scope survives Sync or
    retains a widget/model. Grouping follows LCL's actual peer-parent boundary. }
  TRadioScope = record
    Parent: TWinControl;
    Entry: TRadioButton;
    Checked: Boolean;
  end;
var
  LGeneration: INyxLCLBindingGeneration;
  LScopes: array of TRadioScope;
  LIndex: Integer;
  LScope: Integer;
  LRadio: TRadioButton;
  LPolicy: TNyxInteractionPolicy;
begin
  LGeneration := FGeneration;

  if not NativeGenerationCurrent(LGeneration) then
  begin
    Exit;
  end;

  SetLength(LScopes, 0);
  RequireNativeGeneration(LGeneration);
  for LIndex := 0 to High(FBindings) do
  begin

    if not FBindings[LIndex].FCustom and (FBindings[LIndex].FInput is TRadioButton) then
    begin
      LRadio := TRadioButton(FBindings[LIndex].FInput);
      RequireNativeGeneration(LGeneration);
      LRadio.TabStop := False;
      LPolicy := NyxInteractionPolicy(FBindings[LIndex].FNode);
      RequireNativeGeneration(LGeneration);

      if not LPolicy.Enabled or not LPolicy.Visible then
      begin
        RequireNativeGeneration(LGeneration);
        Continue;
      end;
      RequireNativeGeneration(LGeneration);
      LScope := 0;
      RequireNativeGeneration(LGeneration);
      while (LScope < Length(LScopes)) and (LScopes[LScope].Parent <> LRadio.Parent) do
      begin
        RequireNativeGeneration(LGeneration);
        Inc(LScope);
      end;
      RequireNativeGeneration(LGeneration);

      if LScope = Length(LScopes) then
      begin
        SetLength(LScopes, LScope + 1);
        RequireNativeGeneration(LGeneration);
        LScopes[LScope].Parent := LRadio.Parent;
        RequireNativeGeneration(LGeneration);
        LScopes[LScope].Entry := LRadio;
        LScopes[LScope].Checked := False;
      end;

      if LRadio.Checked and not LScopes[LScope].Checked then
      begin
        RequireNativeGeneration(LGeneration);
        LScopes[LScope].Entry := LRadio;
        LScopes[LScope].Checked := True;
      end;
      RequireNativeGeneration(LGeneration);
    end;
    RequireNativeGeneration(LGeneration);
  end;
  RequireNativeGeneration(LGeneration);
  for LScope := 0 to High(LScopes) do
  begin
    LScopes[LScope].Entry.TabStop := True;
  end;
  RequireNativeGeneration(LGeneration);
end;

function TNyxLCLRenderer.SyncEditedValue(ANode: TNyxNode): Boolean;
var
  LGeneration: INyxLCLBindingGeneration;
  LPhysicalFrame: INyxNativeGestureFrame;
  LIndex: Integer;
  LBinding: TNyxLCLBinding;
  LValue: TNyxText;
begin
  LGeneration := FGeneration;
  RequireNativeGeneration(LGeneration);
  LPhysicalFrame := FPhysicalFrame;
  LPhysicalFrame.Enter;
  try

  Result := False;

  if FUpdating or FDesignMode or FForceValues or (ANode = nil) or
    not ((ANode.ProjectionKind = 'input') or (ANode.ProjectionKind = 'memo')) or
    (ANode.Prop('width-sizing') = 'content') or (ANode.Prop('height-sizing') = 'content') then
  begin
    RequireNativeGeneration(LGeneration);
    Exit;
  end;
  RequireNativeGeneration(LGeneration);
  for LIndex := 0 to High(FBindings) do
  begin
    LBinding := FBindings[LIndex];
    RequireNativeGeneration(LGeneration);

    if LBinding.FNode <> ANode then
    begin
      Continue;
    end;

    if LBinding.FCustom or LBinding.FComposing or
      (FactoryIndex(ANode) >= 0) or not (LBinding.FInput is TCustomEdit) then
    begin
      Exit;
    end;
    RequireNativeGeneration(LGeneration);
    LValue := ANode.Prop('value');
    RequireNativeGeneration(LGeneration);
    FUpdating := True;
    try
      { Physical typing normally already has this exact admitted value. Keep
        its native selection/scroll untouched and only correct a differing face. }

      if StringReplace(TNyxText(TCustomEdit(LBinding.FInput).Text), #13#10, #10,
        [rfReplaceAll]) <> LValue then
      begin
        RequireNativeGeneration(LGeneration);
        TCustomEdit(LBinding.FInput).Text := LValue;
        RequireNativeGeneration(LGeneration);
      end;
      RequireNativeGeneration(LGeneration);
      LBinding.FHasValueBaseline := True;
      LBinding.FLastValue := LValue;
    finally

      if NativeGenerationCurrent(LGeneration) then
      begin
        FUpdating := False;
      end;
    end;
    RequireNativeGeneration(LGeneration);
    Result := True;
    { Structural recipes still receive the ordinary queued observation. }
    QueueContent;
    RequireNativeGeneration(LGeneration);
    Exit;
  end;
  RequireNativeGeneration(LGeneration);
  finally
    { LAST: retain the exact original native frame across every callback tail. }
    LPhysicalFrame.Leave;
  end;
end;

procedure TNyxLCLRenderer.Sync;
var
  LGeneration: INyxLCLBindingGeneration;
  LPhysicalFrame: INyxNativeGestureFrame;
  {$ifdef NYX_STUDIO_PROFILE}
  LSyncStarted: QWord;
  LLayoutStarted: QWord;
  {$endif}
  LIndex: Integer;
  LProjectionPass, LMaximumPasses: Integer;
  LBinding: TNyxLCLBinding;
  LDrawing: TNyxLCLDrawing;
  LDrawingProvider: INyxDrawingProvider;
  LDrawingScene: INyxDrawingPlan;
  LDrawingSource: INyxDrawingSource;
  LDrawingTheme: TNyxTheme;
  LDrawingLabel: TNyxText;
  LCanvasView: INyxCanvasWorkspaceView;
  LCollectionMount: INyxCollectionMount;
  LCanvasAccessibility: INyxCanvasAccessibilityView;
  LCanvasName: TNyxText;
  LCanvasDescription: TNyxText;
  LInput: TControl;
  LNode: TNyxNode;
  LPolicy: TNyxInteractionPolicy;
  LEnabled: Boolean;
  LReadOnly: Boolean;
  LValue: TNyxText;
  LMinimum: Integer;
  LMaximum: Integer;
  LWriteValue: Boolean;
  LValueDomain: TNyxValueDomain;
  LDisclosureRoot: TNyxNode;
  LDisclosureBody: TNyxNode;
  LDisclosureCaption: TNyxText;
  LDisclosureFocus: TWinControl;
  LActive: TControl;
  LFocusCursor: TControl;
  LFocusHeader: TNyxNode;
  LFocusEvents: INyxEvents;
  LFocusRevision: Integer;
  LFocusBinding: TNyxLCLBinding;
  LHeaderHadFocus: Boolean;
  LOriginBinding: TNyxLCLBinding;
  LOriginHadFocus: Boolean;
  LBeforeExit: TControl;
begin
  LGeneration := FGeneration;
  RequireNativeGeneration(LGeneration);
  LPhysicalFrame := FPhysicalFrame;
  LPhysicalFrame.Enter;
  try
    try

      if FRoot <> nil then
      begin
        FResourceContext := FRoot.ResourceContext;
        RequireNativeGeneration(LGeneration);
      end;
      { Native setters can fire change events. One guard covers every control kind,
        including Boolean/range widgets and layout side effects. Skip unchanged edit
        text so synchronization preserves selection, caret and focused drafts. }

      if FUpdating then
      begin
        Exit;
      end;
      { Native visibility changes may transfer focus before the remaining controls
        synchronize. Capture the owned origin and available disclosure header first;
        apply recovery only after the complete presentation has settled. }
      LDisclosureFocus := nil;
      RequireNativeGeneration(LGeneration);
      LFocusBinding := nil;
      RequireNativeGeneration(LGeneration);
      LActive := Screen.ActiveControl;
      RequireNativeGeneration(LGeneration);
      for LIndex := 0 to High(FBindings) do
      begin
        LFocusCursor := LActive;
        while (LFocusCursor <> nil) and
          (LFocusCursor <> FBindings[LIndex].FControl) do
        begin
          LFocusCursor := LFocusCursor.Parent;
          RequireNativeGeneration(LGeneration);
        end;
        RequireNativeGeneration(LGeneration);

        if not FDesignMode and (LFocusCursor <> nil) then
        begin
          LFocusHeader := NyxDisclosureClosingHeader(FBindings[LIndex].FNode);
          RequireNativeGeneration(LGeneration);

          if (LFocusHeader <> nil) and NyxInteractionPolicy(LFocusHeader).CanIssueCommand then
          begin
            RequireNativeGeneration(LGeneration);
            LDisclosureFocus := FocusFor(LFocusHeader.ID);
            RequireNativeGeneration(LGeneration);
            LFocusBinding := IdentityBinding(LFocusHeader.ID, niRuntime);
            RequireNativeGeneration(LGeneration);
            LHeaderHadFocus := LFocusBinding.FHasFocus;
            LOriginBinding := FBindings[LIndex];
            LOriginHadFocus := LOriginBinding.FHasFocus;
          end;
          RequireNativeGeneration(LGeneration);
        end;
        RequireNativeGeneration(LGeneration);
      end;
      RequireNativeGeneration(LGeneration);

      if LDisclosureFocus <> nil then
      begin
        LFocusEvents := FEvents;
        RequireNativeGeneration(LGeneration);
        LFocusRevision := LFocusEvents.ViewRevision;
        RequireNativeGeneration(LGeneration);
      end;
      FUpdating := True;
      try
        { Container containment removes child feedback on queried axes. Nested
          allocations propagate at most once per publisher, with two extra rounds
          for scrollbar/client-area settling. Never silently publish an oscillation. }
        LMaximumPasses := 2;
        for LIndex := 0 to High(FBindings) do
        begin

          if FBindings[LIndex].FNode.QueryContainer.Defined then
          begin
            RequireNativeGeneration(LGeneration);
            Inc(LMaximumPasses);
          end;
          RequireNativeGeneration(LGeneration);
        end;
        RequireNativeGeneration(LGeneration);
        for LProjectionPass := 0 to LMaximumPasses do
        begin
          {$ifdef NYX_STUDIO_PROFILE}LSyncStarted := GetTickCount64;
                                     {$endif}

          if (FRoot <> nil) and (FPanel <> nil) then
          begin
            UpdateViewport;
            RequireNativeGeneration(LGeneration);
          end;
          RequireNativeGeneration(LGeneration);
          for LIndex := 0 to Length(FBindings) - 1 do
          begin
            LBinding := FBindings[LIndex];
            RequireNativeGeneration(LGeneration);
            LNode := LBinding.FNode;
            LBinding.FControl.Visible := NyxInteractionPolicy(LNode).Visible;
            RequireNativeGeneration(LGeneration);
            LBinding.FControl.Hint := LNode.Prop('hint');
            RequireNativeGeneration(LGeneration);
            LBinding.FControl.ShowHint := LBinding.FControl.Hint <> '';
            RequireNativeGeneration(LGeneration);
            LBinding.FControl.AccessibleName := LNode.Prop('aria-label', LNode.Prop('text'));
            RequireNativeGeneration(LGeneration);
            SyncNyxLCLAccessibility(LBinding.FControl);
            RequireNativeGeneration(LGeneration);
            LPolicy := NyxInteractionPolicy(LNode);
            RequireNativeGeneration(LGeneration);
            LEnabled := LPolicy.Enabled;
            RequireNativeGeneration(LGeneration);
            LReadOnly := LPolicy.ReadOnly;
            RequireNativeGeneration(LGeneration);

            if LBinding.FCanvasView <> nil then
            begin
              { The outer layout panel is not the keyboard-focusable canvas. Apply
                its effective typed/bound name to that retained surface as well.
                Optional target capability preserves alternate view implementations. }
              LCanvasName := LNode.Prop('aria-label', NyxDefaultCanvasAccessibleName);
              RequireNativeGeneration(LGeneration);
              LCanvasDescription := LNode.Prop('hint', NyxCanvasKeyboardHelp);
              RequireNativeGeneration(LGeneration);

              if Supports(LBinding.FCanvasView, INyxCanvasAccessibilityView, LCanvasAccessibility) then
              begin
                RequireNativeGeneration(LGeneration);
                LCanvasAccessibility.SetAccessibleMetadata(LCanvasName, LCanvasDescription);
                RequireNativeGeneration(LGeneration);
              end;
              RequireNativeGeneration(LGeneration);
              LCanvasView := LBinding.FCanvasView;
              LCanvasView.Constrain(LEnabled, LReadOnly);
              RequireNativeGeneration(LGeneration);
            end;
            LBinding.FControl.Enabled := LEnabled;

            if LBinding.FDateField <> nil then
            begin
              LValueDomain := NyxNodeValueDomain(LNode);
              RequireNativeGeneration(LGeneration);
              LBinding.FDateField.SetDomain(LValueDomain);
              RequireNativeGeneration(LGeneration);
              LBinding.FDateField.SetAcceptedValue(
                TNyxCalendarDate.FromText(LNode.Prop('value')));
                RequireNativeGeneration(LGeneration);
              LBinding.FDateField.SetInteraction(LEnabled, LReadOnly,
                LBinding.FControl.IsVisible);
                RequireNativeGeneration(LGeneration);
            end;

            if LBinding.FTimeField <> nil then
            begin
              LValueDomain := NyxNodeValueDomain(LNode);
              RequireNativeGeneration(LGeneration);
              LBinding.FTimeField.SetDomain(LValueDomain);
              RequireNativeGeneration(LGeneration);
              LBinding.FTimeField.SetAcceptedValue(
                TNyxClockTime.FromText(LNode.Prop('value')));
                RequireNativeGeneration(LGeneration);
              LBinding.FTimeField.SetInteraction(LEnabled, LReadOnly,
                LBinding.FControl.IsVisible);
                RequireNativeGeneration(LGeneration);
            end;

            if LBinding.FColorField <> nil then
            begin
              LValueDomain := NyxNodeValueDomain(LNode);
              RequireNativeGeneration(LGeneration);
              LBinding.FColorField.SetDomain(LValueDomain);
              RequireNativeGeneration(LGeneration);
              LBinding.FColorField.SetAcceptedValue(TNyxRGBColor.FromText(LNode.Prop('value')));
              RequireNativeGeneration(LGeneration);
              LBinding.FColorField.SetInteraction(LEnabled, LReadOnly,
                LBinding.FControl.IsVisible);
                RequireNativeGeneration(LGeneration);
            end;

            if LNode.Props.IndexOfName('drag-source') >= 0 then
            begin
              RequireNativeGeneration(LGeneration);

              if not FDesignMode and (LNode.Prop('drag-source') = 'true') then
              begin
                RequireNativeGeneration(LGeneration);
                TNyxControlAccess(LBinding.FControl).DragMode := dmAutomatic;
                RequireNativeGeneration(LGeneration);
              end
              else
              begin
                RequireNativeGeneration(LGeneration);
                TNyxControlAccess(LBinding.FControl).DragMode := dmManual;
                RequireNativeGeneration(LGeneration);
              end;
              RequireNativeGeneration(LGeneration);

              if (LBinding.FInput <> nil) and (LBinding.FInput <> LBinding.FControl) then
              begin
                TNyxControlAccess(LBinding.FInput).DragMode :=
                  TNyxControlAccess(LBinding.FControl).DragMode;
                  RequireNativeGeneration(LGeneration);
              end;
              RequireNativeGeneration(LGeneration);
            end;
            RequireNativeGeneration(LGeneration);

            if LBinding.FControl is TNyxLCLSplitView then
            begin
              TNyxLCLSplitView(LBinding.FControl).SetInteraction(LEnabled and not FDesignMode,
                LReadOnly or FDesignMode);
                RequireNativeGeneration(LGeneration);
            end;

            if LBinding.FControl is TNyxLCLTabHost then
            begin

              TNyxLCLTabHost(LBinding.FControl).SynchronizeDescriptor(LNode);
              RequireNativeGeneration(LGeneration);
            end;

            if LBinding.FCollectionMount <> nil then
            begin
              LCollectionMount := LBinding.FCollectionMount;
              LCollectionMount.SetInteraction(LEnabled and not FDesignMode,
                LReadOnly or FDesignMode);
                RequireNativeGeneration(LGeneration);
            end;
            LBinding.SyncLiteralItems;
            RequireNativeGeneration(LGeneration);


            if not LBinding.FCustom and (LBinding.FControl is TNyxLCLDrawing) then
            begin

              if LBinding.FDrawingSource = nil then
              begin
                LBinding.FDrawingSource := NewNyxDrawingSource;
                RequireNativeGeneration(LGeneration);
              end;
              LDrawing := TNyxLCLDrawing(LBinding.FControl);
              LDrawingSource := LBinding.FDrawingSource;
              LDrawingTheme := FTheme;
              LDrawingLabel := LBinding.FNode.Prop('text');
              LDrawingScene := LDrawingSource.Read(LBinding.FNode);
              RequireNativeGeneration(LGeneration);
              LDrawingProvider := DrawingProvider(LBinding.FNode);
              RequireNativeGeneration(LGeneration);
              LDrawing.Configure(LDrawingScene, LDrawingProvider, LDrawingTheme, LDrawingLabel);
                RequireNativeGeneration(LGeneration);
            end;
            RequireNativeGeneration(LGeneration);
            LBinding.SyncPicture;
            RequireNativeGeneration(LGeneration);
            LBinding.FControl.AccessibleValue := LNode.Prop('pressed');
            RequireNativeGeneration(LGeneration);

            if LBinding.FCaption <> nil then
            begin
              LBinding.FCaption.Caption := LNode.Prop('text');
              RequireNativeGeneration(LGeneration);
            end
            else if not LBinding.FCustom then
            begin

              if (LBinding.FControl is TLabel) or (LBinding.FControl is TNyxLCLButton) or
                (LBinding.FControl is TButton) or (LBinding.FControl is TCheckBox) or
                (LBinding.FControl is TRadioButton) or (LBinding.FControl is TGroupBox) then
              begin
                TNyxControlAccess(LBinding.FControl).Caption := LNode.Prop('text');
                RequireNativeGeneration(LGeneration);

                if NyxDisclosureHeader(LNode, LDisclosureRoot, LDisclosureBody, LDisclosureCaption) then
                begin
                  TNyxControlAccess(LBinding.FControl).Caption := LDisclosureCaption;
                  RequireNativeGeneration(LGeneration);
                end;
                RequireNativeGeneration(LGeneration);
              end;
              RequireNativeGeneration(LGeneration);
            end;

            if not LBinding.FCustom and (LNode.ProjectionKind = 'code') and
              (StringReplace(TNyxText(TMemo(LBinding.FControl).Text), #13#10, #10,
              [rfReplaceAll]) <> LNode.Prop('text')) then
            begin
              RequireNativeGeneration(LGeneration);
              { A code block's content is Text, not the editable scalar Value. Skip
                unchanged text to preserve its inspection caret/selection and scroll. }
              TMemo(LBinding.FControl).Text := LNode.Prop('text');
              RequireNativeGeneration(LGeneration);
            end;
            RequireNativeGeneration(LGeneration);

            if LBinding.FControl is TNyxLCLButton then
            begin
              TNyxLCLButton(LBinding.FControl).ApplyTheme(FTheme, LNode.Prop('variant'));
              RequireNativeGeneration(LGeneration);
              LBinding.SyncButtonFace;
              RequireNativeGeneration(LGeneration);

              if NyxDisclosureHeader(LNode, LDisclosureRoot, LDisclosureBody, LDisclosureCaption) then
              begin

                if NyxInteractionPolicy(LDisclosureBody).Visible then
                begin
                  RequireNativeGeneration(LGeneration);
                  TNyxLCLButton(LBinding.FControl).Disclosure(ndcExpanded);
                  RequireNativeGeneration(LGeneration);
                end
                else
                begin
                  RequireNativeGeneration(LGeneration);
                  TNyxLCLButton(LBinding.FControl).Disclosure(ndcCollapsed);
                  RequireNativeGeneration(LGeneration);
                end;
                RequireNativeGeneration(LGeneration);
              end
              else
              begin
                RequireNativeGeneration(LGeneration);
                TNyxLCLButton(LBinding.FControl).Disclosure(ndcNone);
                RequireNativeGeneration(LGeneration);
              end;
              RequireNativeGeneration(LGeneration);
            end;
            LInput := LBinding.FInput;
            LValue := LNode.Prop('value');
            RequireNativeGeneration(LGeneration);
            { Composition owns its physical text until the OS end has drained. }

            if LBinding.FComposing then
            begin
              Continue;
            end;
            LWriteValue := (FForceValues and ((FForceValueNode = nil) or
              (FForceValueNode = LNode))) or not LBinding.FHasValueBaseline or
              (LBinding.FLastValue <> LValue);
              RequireNativeGeneration(LGeneration);
            LBinding.FHasValueBaseline := True;
            LBinding.FLastValue := LValue;

            if LInput <> nil then
            begin
              LInput.Enabled := LEnabled;
              LInput.Hint := LBinding.FControl.Hint;
              RequireNativeGeneration(LGeneration);
              LInput.ShowHint := LBinding.FControl.ShowHint;
              RequireNativeGeneration(LGeneration);
              LInput.AccessibleName := LBinding.FControl.AccessibleName;
              RequireNativeGeneration(LGeneration);
              SyncNyxLCLAccessibility(LInput);
              RequireNativeGeneration(LGeneration);
            end;

            if LInput is TSpinEdit then
            begin
              LMinimum := StrToIntDef(LNode.Prop('min'), 0);
              RequireNativeGeneration(LGeneration);
              LMaximum := StrToIntDef(LNode.Prop('max'), 100);
              RequireNativeGeneration(LGeneration);

              if LMinimum > TSpinEdit(LInput).MaxValue then
              begin
                RequireNativeGeneration(LGeneration);
                TSpinEdit(LInput).MaxValue := LMaximum;
                RequireNativeGeneration(LGeneration);
              end;
              RequireNativeGeneration(LGeneration);
              TSpinEdit(LInput).MinValue := LMinimum;
              RequireNativeGeneration(LGeneration);
              TSpinEdit(LInput).MaxValue := LMaximum;
              RequireNativeGeneration(LGeneration);
              TSpinEdit(LInput).ReadOnly := LReadOnly;
              RequireNativeGeneration(LGeneration);

              if LWriteValue and (TSpinEdit(LInput).Value <> StrToIntDef(LValue, 0)) then
              begin
                RequireNativeGeneration(LGeneration);
                TSpinEdit(LInput).Value := StrToIntDef(LValue, 0);
                RequireNativeGeneration(LGeneration);
              end;
              RequireNativeGeneration(LGeneration);
            end
            else if LInput is TCustomEdit then
            begin
              TEdit(LInput).ReadOnly := LReadOnly;
              RequireNativeGeneration(LGeneration);
              TEdit(LInput).TextHint := LNode.Prop('placeholder');
              RequireNativeGeneration(LGeneration);

              if not LBinding.FCustom and (LNode.ProjectionKind = 'input') then
              begin
                RequireNativeGeneration(LGeneration);
                { Formatting can change after mounting. Re-resolve its actual scalar
                  domain so a newly numeric face keeps unfinished drafts until commit,
                  and returning to Text restores ordinary per-change admission. An
                  explicit recipe domain remains authoritative over the format hint. }
                LValueDomain := NyxNodeValueDomain(LNode);
                RequireNativeGeneration(LGeneration);
                LBinding.FDeferredValue := LValueDomain.Defined and
                  (LValueDomain.Kind in [nskInteger, nskNumber]);
                  RequireNativeGeneration(LGeneration);

                if LBinding.FDeferredValue then
                begin
                  TEdit(LInput).OnEditingDone := LBinding.CommitValue;
                  RequireNativeGeneration(LGeneration);
                end
                else
                begin
                  TEdit(LInput).OnEditingDone := nil;
                  RequireNativeGeneration(LGeneration);
                end;

                if LNode.Prop('input-type') = 'password' then
                begin
                  RequireNativeGeneration(LGeneration);
                  TEdit(LInput).PasswordChar := '*';
                  RequireNativeGeneration(LGeneration);
                end
                else
                begin
                  RequireNativeGeneration(LGeneration);
                  TEdit(LInput).PasswordChar := #0;
                  RequireNativeGeneration(LGeneration);
                end;
                RequireNativeGeneration(LGeneration);
              end;
              RequireNativeGeneration(LGeneration);

              if LWriteValue and
                (StringReplace(TNyxText(TEdit(LInput).Text), #13#10, #10, [rfReplaceAll]) <> LValue) then
              begin
                RequireNativeGeneration(LGeneration);
                TEdit(LInput).Text := LValue;
                RequireNativeGeneration(LGeneration);
              end;
              RequireNativeGeneration(LGeneration);
            end
            else if LInput is TComboBox then
            begin

              if LWriteValue and (TNyxText(TComboBox(LInput).Text) <> LValue) then
              begin
                RequireNativeGeneration(LGeneration);
                TComboBox(LInput).ItemIndex := TComboBox(LInput).Items.IndexOf(LValue);
                RequireNativeGeneration(LGeneration);
              end;
              RequireNativeGeneration(LGeneration);
            end
            else if LInput is TCheckBox then
            begin

              if LWriteValue then
              begin
                TCheckBox(LInput).Checked := LValue = 'true';
                RequireNativeGeneration(LGeneration);
              end;
            end
            else if LInput is TRadioButton then
            begin

              if LWriteValue then
              begin
                TRadioButton(LInput).Checked := LValue = 'true';
                RequireNativeGeneration(LGeneration);
              end;
            end
            else if LInput is TNyxLCLSlider then
            begin
              TNyxLCLSlider(LInput).Accept(TNyxSliderScale.Create(NyxNodeValueDomain(LNode),
                StrToIntDef(LNode.Prop('min'), 0), StrToIntDef(LNode.Prop('max'), 100),
                StrToIntDef(LNode.Prop('slider-intervals'), 1000)), LValue);
                RequireNativeGeneration(LGeneration);
            end
            else if LInput is TTrackBar then
            begin
              LMinimum := StrToIntDef(LNode.Prop('min'), 0);
              RequireNativeGeneration(LGeneration);
              LMaximum := StrToIntDef(LNode.Prop('max'), 100);
              RequireNativeGeneration(LGeneration);

              if LMinimum > TTrackBar(LInput).Max then
              begin
                RequireNativeGeneration(LGeneration);
                TTrackBar(LInput).Max := LMaximum;
                RequireNativeGeneration(LGeneration);
              end;
              RequireNativeGeneration(LGeneration);
              TTrackBar(LInput).Min := LMinimum;
              RequireNativeGeneration(LGeneration);
              TTrackBar(LInput).Max := LMaximum;
              RequireNativeGeneration(LGeneration);

              if LWriteValue then
              begin
                TTrackBar(LInput).Position := StrToIntDef(LValue, 0);
                RequireNativeGeneration(LGeneration);
              end;
            end;

            if LBinding.FControl is TProgressBar then
            begin
              TProgressBar(LBinding.FControl).Min := StrToIntDef(LNode.Prop('min'), 0);
              RequireNativeGeneration(LGeneration);
              TProgressBar(LBinding.FControl).Max := StrToIntDef(LNode.Prop('max'), 100);
              RequireNativeGeneration(LGeneration);
              TProgressBar(LBinding.FControl).Position := StrToIntDef(LValue, 0);
              RequireNativeGeneration(LGeneration);

              if LBinding.FControl is TNyxLCLProgress then
              begin
                TNyxLCLProgress(LBinding.FControl).Indeterminate :=
                  LNode.Prop(NyxAttributeName(atIndeterminate), 'false') = 'true';
                  RequireNativeGeneration(LGeneration);
              end
              else if LNode.Prop(NyxAttributeName(atIndeterminate), 'false') = 'true' then
              begin
                TProgressBar(LBinding.FControl).Style := pbstMarquee;
                RequireNativeGeneration(LGeneration);
              end
              else
              begin
                TProgressBar(LBinding.FControl).Style := pbstNormal;
                RequireNativeGeneration(LGeneration);
              end;
            end;

            if Assigned(LBinding.FUpdater) then
            begin
              RequireNativeGeneration(LGeneration);
              LBinding.FUpdater(LNode, LBinding.FControl);
              RequireNativeGeneration(LGeneration);
            end;
            RequireNativeGeneration(LGeneration);
          end;
          RequireNativeGeneration(LGeneration);
          SyncRadioFocus;
          RequireNativeGeneration(LGeneration);
          {$ifdef NYX_STUDIO_PROFILE}
          WriteLn('native-sync,properties,', Length(FBindings), ',', GetTickCount64 - LSyncStarted);
          RequireNativeGeneration(LGeneration);
          LLayoutStarted := GetTickCount64;
          {$endif}
          Resize(FPanel);
          RequireNativeGeneration(LGeneration);
          {$ifdef NYX_STUDIO_PROFILE}
          WriteLn('native-sync,layout,', Length(FBindings), ',', GetTickCount64 - LLayoutStarted);
          RequireNativeGeneration(LGeneration);
          {$endif}

          if not UpdateViewport then
          begin
            Break;
          end;

          if LProjectionPass = LMaximumPasses then
          begin
            raise ENyxModel.Create('Container presentation allocation did not settle');
          end;
        end;
      finally

        if NativeGenerationCurrent(LGeneration) then
        begin
          FUpdating := False;
          FlushImages;
        end;
      end;
      RequireNativeGeneration(LGeneration);
      InitializeCanvases;
      RequireNativeGeneration(LGeneration);
      QueueContent;
      RequireNativeGeneration(LGeneration);

      if (LDisclosureFocus <> nil) and (LFocusEvents.ViewRevision = LFocusRevision) and
        LDisclosureFocus.CanFocus then
      begin
        RequireNativeGeneration(LGeneration);
        { Win32 can focus this header while the hidden body is synchronizing. Those
          guarded notifications update the physical baseline but do not dispatch.
          Recover the prior header baseline so the settled transition is delivered
          once even when SetFocus would now be a physical no-op. }
        LFocusBinding.FHasFocus := LHeaderHadFocus;

        if LOriginHadFocus then
        begin
          LOriginBinding.FHasFocus := True;
          LBeforeExit := Screen.ActiveControl;
          RequireNativeGeneration(LGeneration);
          LOriginBinding.ObserveFocus(ntAfterExit);
          RequireNativeGeneration(LGeneration);

          if (LFocusEvents.ViewRevision <> LFocusRevision) or
            (Screen.ActiveControl <> LBeforeExit) then
          begin
            RequireNativeGeneration(LGeneration);
            Exit;
          end;
          RequireNativeGeneration(LGeneration);
        end;

        if LDisclosureFocus.Focused then
        begin
          RequireNativeGeneration(LGeneration);
          LFocusBinding.ObserveFocus(ntAfterEnter);
          RequireNativeGeneration(LGeneration);
          Exit;
        end;
        RequireNativeGeneration(LGeneration);
        { Last borrowed call: focus callbacks may retire or dispose this renderer. }
        LDisclosureFocus.SetFocus;
        RequireNativeGeneration(LGeneration);
      end;
      RequireNativeGeneration(LGeneration);
    except
      on ENyxNativeGenerationRetired do
      begin
        { Obsolete receiver: committed data stays intact; no successor tail runs. }
      end;
    end;
  finally
    { LAST: retain the exact original native frame across every callback tail. }
    LPhysicalFrame.Leave;
  end;
end;

procedure TNyxLCLBinding.SetupCanvas(ARenderer: TNyxLCLRenderer);
begin
  { Children compose their own built-ins. A later parent registration cannot
    rewrite this accepted membership; an explicit child registry still wins. }
  ARenderer.FInheritedCoordinatorSnapshot := FRenderer.FCallerCoordinatorSnapshot;
  ARenderer.FInheritedCollectionInputGate := FRenderer.FCollectionInputGate;
  ARenderer.FFactoryKinds := Copy(FRenderer.FFactoryKinds, 0, Length(FRenderer.FFactoryKinds));
  ARenderer.FDrawingNames := Copy(FRenderer.FDrawingNames, 0, Length(FRenderer.FDrawingNames));
  ARenderer.FDrawingProviders := Copy(FRenderer.FDrawingProviders, 0, Length(FRenderer.FDrawingProviders));
  ARenderer.FFactories := Copy(FRenderer.FFactories, 0, Length(FRenderer.FFactories));
  ARenderer.FEventFactories := Copy(FRenderer.FEventFactories, 0, Length(FRenderer.FEventFactories));
  ARenderer.FUpdaters := Copy(FRenderer.FUpdaters, 0, Length(FRenderer.FUpdaters));
  ARenderer.FResourceContext := FRenderer.FResourceContext;
  ARenderer.FTableWorkbenchServices := FRenderer.FTableWorkbenchServices;
  ARenderer.FTableTextLimits := FRenderer.FTableTextLimits;
  ARenderer.ConfigureEmbeddedBehavior(FRenderer.FEmbeddedContext, FRenderer.FEmbeddedResolver);
  ARenderer.CanvasAdoptionHost := FRenderer.FCanvasAdoptionHost;
  ARenderer.OnEvent := CanvasEvent;
end;

procedure TNyxLCLBinding.CanvasEvent(ANode: TNyxNode; const AEvent: TNyxEventInfo);
var
  LGeneration: INyxLCLBindingGeneration;
  LEnteredFrame: INyxNativeGestureFrame;
  LEvents: INyxEvents;
  LCallback: TNyxLCLEvent;
  LRevision: Integer;
begin
  LGeneration := GenerationLease;

  if (LGeneration = nil) or not NativeGenerationCurrent(LGeneration) then
  begin
    Exit;
  end;
  LEnteredFrame := LGeneration.PhysicalFrame;
  LEnteredFrame.Enter;
  try
    try

      if FRenderer.FUpdating or FRenderer.FPublishingContent then
      begin
        Exit;
      end;
      LEvents := FRenderer.FEvents;
      RequireNativeGeneration(LGeneration);
      LCallback := FRenderer.FOnEvent;

      if FRenderer.FDesignMode then
      begin

        if Assigned(LCallback) then
        begin
          RequireNativeGeneration(LGeneration);
          LCallback(FNode, NyxDesignEvent(FNode, ntDesignSelect));
          RequireNativeGeneration(LGeneration);
        end;
        RequireNativeGeneration(LGeneration);
        Exit;
      end;
      LRevision := LEvents.ViewRevision;
      RequireNativeGeneration(LGeneration);
      { Dispatch may free this binding and its entire parent renderer. Keep only
        the managed stream and copied legacy callback afterward; retirement changes
        the captured revision and revokes the borrowed child node. }
      LEvents.Dispatch(AEvent, ANode.DesignID, ANode.DesignID);
      RequireNativeGeneration(LGeneration);

      if LEvents.ViewRevision <> LRevision then
      begin
        RequireNativeGeneration(LGeneration);
        Exit;
      end;
      RequireNativeGeneration(LGeneration);

      if Assigned(LCallback) then
      begin
        RequireNativeGeneration(LGeneration);
        LCallback(ANode, AEvent);
        RequireNativeGeneration(LGeneration);
      end;
      RequireNativeGeneration(LGeneration);
    except
      on ENyxNativeGenerationRetired do
      begin
        { The original generation retired; only retained locals may unwind. }
      end;
    end;
  finally
    LEnteredFrame.Leave;
  end;
end;


function TNyxLCLRenderer.ToolCanvasView(const AID: TNyxText;
  AIdentity: TNyxIdentityKind): INyxToolCanvasView;
var
  LIndex: Integer;
  LControl: TControl;
begin
  FEvents.Scheduler.RequireUI;
  LControl := ControlFor(AID, AIdentity);
  for LIndex := 0 to High(FBindings) do
  begin

    if (FBindings[LIndex].FControl = LControl) and
      (FBindings[LIndex].FToolCanvasView <> nil) then
    begin
      Exit(FBindings[LIndex].FToolCanvasView);
    end;
  end;
  raise ENyxModel.Create('Tool Canvas inspector identity is unavailable');
end;

procedure TNyxLCLRenderer.MountToolCanvases(ADocument: TNyxDocument);
var
  LIndex: Integer;
  LWorkspace: TNyxNode;
  LHost: TNyxNode;
  LBinding: TNyxLCLBinding;
  LAdoptionHost: INyxCanvasRouteAdoptionHost;
begin
  for LIndex := 0 to High(FBindings) do
  begin
    LBinding := FBindings[LIndex];

    if (LBinding.FNode.Kind <> NyxKindName(nkToolCanvas)) or
      (LBinding.FNode.Prop(NyxAttributeName(atCompound)) <> 'true') then
    begin
      Continue;
    end;
    LWorkspace := LBinding.FNode.Part(NyxPart('workspace'));
    LHost := LBinding.FNode.Part(NyxPart('tools/port-host'));
    LAdoptionHost := nil;

    if not FDesignMode then
    begin
      LAdoptionHost := FCanvasAdoptionHost;
    end;

    if not (ControlFor(LHost.ID, niRuntime) is TWinControl) then
    begin
      raise ENyxModel.Create('Tool Canvas port host factory must return a child-window container');
    end;
    LBinding.FToolCanvasView := MountNyxLCLToolCanvas(ADocument,
      CanvasView(LWorkspace.ID, niRuntime), TWinControl(ControlFor(LHost.ID, niRuntime)), FState, FTheme,
      NyxControl(LWorkspace.DesignID), LAdoptionHost);
  end;
end;

procedure TNyxLCLRenderer.ConfigureEmbeddedBehavior(const AContext: INyxApplicationContext;
  const AResolver: INyxLogicResolver);
begin
  FEvents.Scheduler.RequireUI;

  if FRoot <> nil then
  begin
    raise ENyxModel.Create('Configure embedded behavior authority before mounting');
  end;
  FEmbeddedContext := AContext;
  FEmbeddedResolver := AResolver;
end;

procedure TNyxLCLRenderer.InitializeCanvases;
var
  LGeneration: INyxLCLBindingGeneration;
  LIndex: Integer;
  LView: INyxCanvasWorkspaceView;
  LLayout: INyxCanvasLayoutView;
begin
  LGeneration := FGeneration;
  RequireNativeGeneration(LGeneration);

  if FUpdating or FLayouting or FProjecting then
  begin
    Exit;
  end;
  for LIndex := 0 to High(FBindings) do
  begin
    LView := FBindings[LIndex].FCanvasView;
    RequireNativeGeneration(LGeneration);

    if (LView <> nil) and LView.Connected and
      Supports(LView, INyxCanvasLayoutView, LLayout) then
    begin
      RequireNativeGeneration(LGeneration);

      if LLayout.PresentationRevision = 0 then
      begin
        RequireNativeGeneration(LGeneration);
        { Refresh owns factory failure on this caller's stack. A candidate's
          outer physical preview can still refuse before retiring its old tree. }
        LView.Refresh;
        RequireNativeGeneration(LGeneration);
      end;
      RequireNativeGeneration(LGeneration);
    end;
    RequireNativeGeneration(LGeneration);
  end;
  RequireNativeGeneration(LGeneration);
end;

procedure TNyxLCLRenderer.MountCanvases(ADocument: TNyxDocument);
var
  LIndex: Integer;
  LSerial: Integer;
  LID: TNyxText;
  LWorkspace: INyxCanvasWorkspace;
  LBinding: TNyxLCLBinding;
  LResources: INyxApplicationResources;
begin
  { The application port owns no renderer. Forward its optional prepared broker
    capability so ordinary saved canvases receive the same loaded publication
    as their surrounding application, without constructing another loader. }
  Supports(FResourceUpdates, INyxApplicationResources, LResources);
  for LIndex := 0 to High(FBindings) do
  begin
    LBinding := FBindings[LIndex];

    if LBinding.FNode.ProjectionKind <> NyxKindName(nkCanvas) then
    begin
      Continue;
    end;
    { Internal chrome belongs to a private copied context, never the authored
      canvas node. A bounded fresh identity avoids collisions with user roots. }
    LID := 'canvas-workspace';
    LSerial := 0;
    while ADocument.Find(LID) <> nil do
    begin
      Inc(LSerial);
      LID := 'canvas-workspace-' + IntToStr(LSerial);
    end;

    if LBinding.FNode.CanvasScene.Defined then
    begin
      LWorkspace := NewNyxCanvasWorkspace(LID, ADocument, LBinding.FNode.CanvasScene);
    end
    else
    begin
      LWorkspace := NewNyxCanvasWorkspace(LID, NewNyxCanvasScene);
    end;
    LBinding.FCanvasView := MountNyxLCLCanvasWorkspace(LWorkspace, ADocument,
      TWinControl(LBinding.FControl), FState, FTheme, LBinding.SetupCanvas, FDesignMode,
      FEmbeddedContext, FEmbeddedResolver, LResources);
    LBinding.FCanvasView.Constrain(NyxInteractionPolicy(LBinding.FNode).Enabled,
      NyxInteractionPolicy(LBinding.FNode).ReadOnly);
  end;
end;

function TNyxLCLRenderer.CanvasView(const AID: TNyxText;
  AIdentity: TNyxIdentityKind): INyxCanvasWorkspaceView;
var
  LControl: TControl;
  LIndex: Integer;
begin
  LControl := ControlFor(AID, AIdentity);
  for LIndex := 0 to High(FBindings) do
  begin

    if FBindings[LIndex].FControl = LControl then
    begin
      Result := FBindings[LIndex].FCanvasView;

      if Result = nil then
      begin
        raise ENyxModel.Create('The selected control has no mounted canvas workspace');
      end;
      Exit;
    end;
  end;
  raise ENyxModel.Create('Canvas workspace identity is unavailable');
end;

end.
