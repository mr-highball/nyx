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

unit nyx.render.browser;

{$mode delphi}{$H+}
{$codepage utf8}
{$modeswitch externalclass}

interface

uses
  nyx.types,
  nyx.text,
  nyx.viewport.context, nyx.viewport.binding,
  nyx.publication,
  nyx.view.publication,
  nyx.coordinators,
  nyx.application.context, nyx.flow, nyx.tool.canvas, nyx.canvas.adoption,
  nyx.images,
  nyx.button.presentation, nyx.button.view,
  nyx.images.browser,
  nyx.image.lifecycle,
  nyx.image.events,
  nyx.tabs.browser,
  nyx.tabs,
  nyx.disclosure,
  nyx.canvas.workspace,
  nyx.drawing, nyx.drawing.browser, nyx.drawing.view,
  nyx.mount.browser,
  Classes,
  SysUtils,
  Math,
  JS,
  Web,
  nyx.model,
  nyx.presentations,
  nyx.containers,
  nyx.interaction,
  nyx.editing,
  nyx.editing.commands,
  nyx.editing.browser,
  nyx.gestures,
  nyx.designer.input,
  nyx.designer.placement,
  nyx.designer.resize,
  nyx.designer.move,
  nyx.designer.guides,
  nyx.gestures.browser,
  nyx.platform,
  nyx.split,
  nyx.split.browser,
  nyx.schema,
  nyx.contract,
  nyx.sliders,
  nyx.colors,
  nyx.colors.browser,
  nyx.theme,
  nyx.design.tokens,
  nyx.behavior,
  nyx.events,
  nyx.kanban, nyx.kanban.intent,
  nyx.master.detail, nyx.data.toolbar,
  nyx.data,
  nyx.viewport,
  nyx.event.emitter,
  nyx.state,
  nyx.resources,
  nyx.resource.context,
  nyx.binding,
  nyx.binding.lifetime,
  nyx.binding.types,
  nyx.binding.inputs,
  nyx.collections.view,
  nyx.collections.range.stroke,
  nyx.collections.selection,
  nyx.collections.bindings,
  nyx.controls,
  nyx.collections.pager,
  nyx.collections.view.types,
  nyx.collections.mount,
  nyx.collections.workbench.runtime,
  nyx.collections.presentation,
  nyx.collections.browser,
  nyx.literal.items,
  nyx.composition,
  nyx.content.mount,
  nyx.scheduler,
  nyx.projection.refresh;

type
  TNyxBrowserRenderer = class;
  TNyxBrowserEvent = TNyxEventHandler;
  TNyxBrowserBindingError = procedure(ANode: TNyxNode; const AReason: TNyxText;
    AFailure: TNyxBindingFailure) of object;
  TNyxBrowserFactory = function(ANode: TNyxNode): TJSHTMLElement;
  { The emitter is initially dormant. It becomes connected only when the whole
    candidate view is accepted, and disconnects before its controls are freed.
    Creators may retain it without retaining the renderer or document. }
  TNyxBrowserEventFactory = function(ANode: TNyxNode;
    const AEmitter: INyxEventEmitter): TJSHTMLElement;
  { Refresh custom markup in place from already admitted runtime properties.
    The callback borrows both arguments, retains children/identity and does not
    write state. Bound custom factories require it; no textContent replacement
    may silently destroy extension-owned markup. }
  TNyxBrowserUpdater = procedure(ANode: TNyxNode; AElement: TJSHTMLElement);

  { A binding borrows a realized node and DOM handles. Its exact generation
    retains the binding/tree through entered callbacks; disconnect revokes all
    input before final memory release. Target events cross one portable boundary. }
  TNyxBrowserBinding = class
  private
    { Weak canonical generation. An entered method takes a managed local lease
      before its first callout; the generation owns this binding after detach. }
    FGenerationObject: TObject;
    FDisconnected: Boolean;
    procedure Disconnect;
    procedure RequireCurrent;
    function RetainView: IInterface;
    function CurrentRenderer: TNyxBrowserRenderer;
    FRenderer: TNyxBrowserRenderer;
    FNode: TNyxNode;
    FElement: TJSHTMLElement;
    FInput: TJSHTMLElement;
    FTabs: TNyxBrowserTabHost;
    { One accepted input dispatch borrows nodes only until this host presents
      its transition. Clear it before callbacks can replace the mounted view. }
    FTabDispatch: TNyxDispatch;
    FHasTabDispatch: Boolean;
    FCaption: TJSHTMLElement;
    { Copied ResizeObserver content-box data, never a transformed screen box.
      Absent until the observer reports this mounted face. }
    FContainerMeasured: Boolean;
    FContainerWidth: Double;
    FContainerHeight: Double;
    FLastValue: TNyxText;
    FHasValueBaseline: Boolean;
    { Mount-owned numeric wire baseline; ordinal slider coordinates never
      enter design proposals, stores or callback snapshots. }
    FSliderValue: TNyxSliderValue;
    { Literal rows have their own baseline. An unrelated state publication must
      not destroy option/row handles, selection or scroll. Bound collections and
      creator factories own their content independently of this fallback. }
    FLastItems: TNyxText;
    FHasItemsBaseline: Boolean;
    FCustom: Boolean;
    FUpdater: TNyxBrowserUpdater;
    FViewRevision: Integer;
    { Decorative glyph pixels share the normal image request/lifecycle. These
      DOM children stay inside the button and never create another input route. }
    FButtonGlyphImage: TJSHTMLImageElement;
    FButtonGlyphFace: TJSHTMLCanvasElement;
    FButtonPresentation: TNyxButtonPresentation;
    { Small paint identity, never a second source/loader. Retained pixels avoid
      resubmitting an entire embedded atlas as CSS on every ordinary Sync. }
    FButtonGlyphRasterKey: TNyxText;
    procedure SyncButtonFace;
    procedure PaintButtonFace;
    FImageLifecycle: INyxImageLifecycle;
    FImageRequest: INyxBrowserImageRequest;
    procedure SyncImage;
    procedure ImageSignal;
    FComposing: Boolean;
    { Optional input-owned timeline port. The binding retains no owner pointer;
      its managed receiver revokes borrowed application state before retirement. }
    FHistory: INyxTextHistoryCommands;
    FEditingSelection: TNyxTextSelection;
    { Capture IDs are host observations/requests for this exact mounted face.
      Teardown removes listeners before releasing any outstanding capture. }
    FCapturedPointers: array of Integer;
    procedure SyncLiteralItems;
    procedure RefreshPager;
    function BeforeEdit(AEvent: TEventListenerEvent): Boolean;
    function CompositionStart(AEvent: TEventListenerEvent): Boolean;
    function CompositionUpdate(AEvent: TEventListenerEvent): Boolean;
    function CompositionEnd(AEvent: TEventListenerEvent): Boolean;
    function TextSelectionChanged(AEvent: TEventListenerEvent): Boolean;
    function EditingEvent(AEvent: TEventListenerEvent; ATrigger: TNyxTrigger;
      APhase: TNyxEditingPhase): Boolean;
    FCollectionMount: INyxCollectionMount;
    { This face owns its nested view. Candidate transfer rebinds FRenderer;
      Clear disconnects the nested view before releasing the borrowed host. }
    FCanvasView: INyxCanvasWorkspaceView;
    FToolCanvasView: INyxToolCanvasView;
    FDrawingView: TNyxBrowserDrawing;
    { Binding-owned immutable fallback admission. It retains exact source/plan,
      never the borrowed node/DOM or a live provider result; Clear releases it. }
    FDrawingSource: INyxDrawingSource;
    procedure SetupCanvas(ARenderer: TNyxBrowserRenderer);
    procedure CanvasEvent(ANode: TNyxNode; const AEvent: TNyxEventInfo);
    FCollectionPager: INyxCollectionPager;
    FSplit: TNyxBrowserSplit;
    FColorField: TNyxBrowserColorField;
    procedure ColorChanged(const AValue: TNyxRGBColor);
    procedure SplitChanged(ASplit: TNyxBrowserSplit);
    procedure TabsChanged(const AChange: TNyxTabTransition);
    function AdmitTabs(const AChange: TNyxTabTransition): Boolean;
    function Click(AEvent: TJSMouseEvent): Boolean;
    { One captured Fit-button activation uses the ordinary Click dispatcher.
      Keyboard callers have already prepared this same command before their
      key callbacks; they never recapture a newly enabled/replaced column. }
    function ColumnClick(AEvent: TEventListenerEvent;
      const ACommand: INyxBrowserColumnCommand; APrepared: Boolean = False): Boolean;
    function Change(AEvent: TEventListenerEvent): Boolean;
    function Enter(AEvent: TEventListenerEvent): Boolean;
    function Leave(AEvent: TEventListenerEvent): Boolean;
    { The scalar input, separator grip or ordinary leaf; borrowed from this
      mount. Collection descendants are admitted separately by their owner. }
    function FocusElement: TJSHTMLElement;
    function IsFocusBoundary(AEvent: TEventListenerEvent): Boolean;
    { A checked but disabled/hidden HTML radio can suppress every peer's native
      Tab entry. A temporary label entry forwards focus to the same real input
      without scroll, value changes, synthetic activation or extra callbacks. }
    function ForwardRadioFocus(AEvent: TEventListenerEvent): Boolean;
    function KeyDown(AEvent: TJSKeyboardEvent): Boolean;
    function KeyUp(AEvent: TJSKeyboardEvent): Boolean;
    function Keyboard(AEvent: TJSKeyboardEvent; ATrigger: TNyxTrigger): Boolean;
    { Arranged-column defaults retain their entry proof across the SAME public
      callbacks. These claimed routes never let bubbling recapture a newer view. }
    function ColumnKeyboard(AEvent: TJSKeyboardEvent;
      const ACommand: INyxBrowserColumnCommand;
      ATrigger: TNyxTrigger = ntKeyDown; APrepared: Boolean = False): Boolean;
    function ColumnDoubleClick(AEvent: TJSMouseEvent;
      const ACommand: INyxBrowserColumnCommand): Boolean;
    function DoubleClick(AEvent: TJSMouseEvent): Boolean;
    function PointerDown(AEvent: TJSPointerEvent): Boolean;
    function RangePointerEvent(AEvent: TJSPointerEvent; ATrigger: TNyxTrigger;
      APhase: TNyxCollectionRangeStrokePhase): Boolean;
    function PointerUp(AEvent: TJSPointerEvent): Boolean;
    function PointerMove(AEvent: TJSPointerEvent): Boolean;
    function PointerEnter(AEvent: TJSPointerEvent): Boolean;
    function PointerExit(AEvent: TJSPointerEvent): Boolean;
    function PointerCancel(AEvent: TJSPointerEvent): Boolean;
    function PointerCapture(AEvent: TJSPointerEvent): Boolean;
    function PointerCaptureLost(AEvent: TJSPointerEvent): Boolean;
    procedure ObserveCapture(APointerID: Integer; ACaptured: Boolean);
    function DragStart(AEvent: TJSDragEvent): Boolean;
    function Drag(AEvent: TJSDragEvent): Boolean;
    function DragEnter(AEvent: TJSDragEvent): Boolean;
    function DragOver(AEvent: TJSDragEvent): Boolean;
    function DragExit(AEvent: TJSDragEvent): Boolean;
    function Drop(AEvent: TJSDragEvent): Boolean;
    function DragEnd(AEvent: TJSDragEvent): Boolean;
    function DragEvent(AEvent: TJSDragEvent; ATrigger: TNyxTrigger;
      APhase: TNyxDragPhase): Boolean;
    function ContextMenu(AEvent: TJSMouseEvent): Boolean;
    function PointerEvent(AEvent: TJSMouseEvent; ATrigger: TNyxTrigger;
      const AManagedInput: INyxBrowserColumnCommand = nil): Boolean;
    function Wheel(AEvent: TJSWheelEvent): Boolean;
    function Scroll(AEvent: TEventListenerEvent): Boolean;
    function ScrollEnd(AEvent: TEventListenerEvent): Boolean;
    function ViewportEvent(AEvent: TEventListenerEvent; ATrigger: TNyxTrigger): Boolean;
    procedure CollectionSelectionChanged(const ABefore, AAfter: INyxCollectionSelection);
  public
    destructor Destroy; override;
  end;

  { Projects the shared contract into real semantic browser controls.
    RegisterFactory admits an owned custom recipe without editing the default
    catalog or renderer. Full Render is the initial correctness path; target
    identity and event ownership remain explicit for later incremental updates.
    Renderer owns its default theme; a supplied theme is borrowed.
    Factories/updaters borrow staged nodes and must not mutate their source or
    runtime store during preparation. Replacements preview fallible target work
    before retirement; extension destructors must release without raising. }
  TNyxBrowserRenderer = class
  private
    { These leases own one immutable view generation and its actual palettes.
      The typed objects are private implementation views, never extra owners. }
    FGeneration: IInterface;
    FGenerationObject: TObject;
    FThemeOwner: IInterface;
    FBaseThemeOwner: IInterface;
    FStateOwner: INyxBindingOwnedState;
    FRetirementShell: Boolean;
    FDetachedRetirementHost: TJSHTMLElement;
    { Exact per-attempt authority, separate from accepted view identity. Nested
      rendering and explicit Unmount invalidate an older unfinished candidate. }
    FRenderAttempt: IInterface;
    constructor CreateRetirementShell;
    procedure DetachView(ATarget: TNyxBrowserRenderer; AKeepPresentation: Boolean);
    procedure RetireDetached(const AEventView: INyxPreparedEventView);
    procedure ReleaseDetached;
    FTheme: TNyxTheme;
    FOwnTheme: Boolean;
    { Independent startup palette. Document overrides never accumulate across
      renders or mutate a caller's supplied theme. }
    FBaseTheme: TNyxTheme;
    { A caller-supplied palette remains borrowed and may change between renders.
      Effective document overlays never mutate it or accumulate into its base. }
    FBorrowedTheme: TNyxTheme;
    { Renderer-local CSS identity is not document identity. An embedded preview
      must not change another renderer's palette, font or corner-radius tokens. }
    FThemeScope: TNyxText;
    FRoot: TNyxNode;
    FResourceContext: INyxResourceContext;
    FResourceUpdates: INyxResourceUpdateQueue;
    { Runtime authority is configuration, never part of the mounted document.
      Captured candidates/embedded children own independent interface leases. }
    FTableWorkbenchServices: INyxTableWorkbenchServices;
    { Runtime-only bounded text work; copied to each independently staged view. }
    FTableTextLimits: TNyxTableTextWorkLimits;
    FHost: TJSHTMLElement;
    { A compiled document owns a connected browser mount independently of its
      volatile editor placement. The exact artifact includes private launch
      context; it is compared locally and never exposed as a diagnostic. }
    FCompiledMount: TNyxBrowserPersistentMount;
    FCompiledArtifact: TNyxText;
    { Only views with authored viewport scopes observe available host width.
      The observer borrows this renderer and disconnects before mount teardown. }
    FViewportObserver: TJSHTMLResizeObserver;
    FViewportWidth: Double;
    FViewportHeight: Double;
    { An enclosing presentation viewport never becomes this host's CSS allocation.
      Export is explicit, so ordinary rule-free views need no extra observer. }
    FViewportBinding: INyxViewportBinding;
    FViewportExported: Boolean;
    { A hidden candidate consumes the captured final host frame. Its optional
      connected probe measures publishers without replacing that frame during
      Sync. Clear owns removal of this temporary host, never the caller's host. }
    FDetachedCandidate: Boolean;
    { Frozen input for one hidden admission pass. Ordinary mounted Sync uses
      observer snapshots; hidden Sync must not mix fresh allocation into only
      the scalar projection while its structure still uses the previous box. }
    FStagingMeasurements: INyxContainerSnapshot;
    FPresentationSelection: TNyxPresentationSelection;
    FPresentationView: INyxPresentationViewOwner;
    { The mounted snapshot owns recipes independently of the caller's document.
      Deferred work owns only a revocable receiver port, never this renderer. }
    FContentBlueprint: TNyxContentBlueprint;
    { Runtime preparation source survives implicit content remounts. Its host
      receivers must revoke before disposal; a source never owns widgets. }
    FViewPublicationSource: INyxViewPublicationSource;
    FContentGuard: INyxContentGuard;
    FContentLifetime: INyxContentLifetime;
    FContentWork: INyxContentWork;
    FContentExecution: INyxExecution;
    { Focus recovery may follow an accepted control command's synchronization.
      Its revocable work port borrows this renderer; copied IDs never own nodes. }
    FDisclosureFocusWork: INyxContentWork;
    FDisclosureFocusExecution: INyxExecution;
    FDisclosureFocusOrigin: TNyxText;
    FDisclosureFocusHeader: TNyxText;
    FDisclosureFocusActive: TJSNode;
    FDisclosureFocusExpected: TJSNode;
    FDisclosureFocusRevision: Integer;
    FContentRequestedSelection: TNyxPresentationSelection;
    FContentFrame: TNyxViewFrame;
    FContentFrameKnown: Boolean;
    FContentMeasurementKey: TNyxText;
    FPublishingContent: Boolean;
    FImageDeliveryQueue: INyxImageDeliveryQueue;
    FLastContentError: TNyxText;
    procedure QueueContent;
    procedure FlushImages;
    procedure RefreshContent;
    procedure CompleteDisclosureFocus;
    procedure RestoreDisclosureFocus(AHeader: TJSHTMLElement; AOrigin: TNyxNode;
      AActive: TJSNode; const AEvents: INyxEvents; ARevision: Integer);
    function CaptureContentFaces: TNyxContentFaceStates;
    procedure RestoreContentFaces(const AStates: TNyxContentFaceStates; AFocus: Boolean);
    { Public entry keeps the caller's old result alive until this core decides
      admission, including a same-destination outstanding preview handle. }
    function PrepareFormRefreshCore(ADocument: TNyxDocument; ARoot: TNyxNode;
      const AForms: TNyxProjectionFormContexts; ADesignMode: Boolean): INyxPreparedPublication;
    procedure RenderFrame(ADocument: TNyxDocument; ARoot: TNyxNode;
      AHost: TJSHTMLElement; ADesignMode: Boolean; AState: TNyxState;
      const ACollections: INyxCollectionBindings; const AFrame: TNyxViewFrame;
      const AMeasurements: INyxContainerSnapshot; AKeepPresentation: Boolean;
      const AStates: TNyxContentFaceStates;
      AResourceMode: TNyxResourceRenderMode = nrmConfigured;
      AUseContext: Boolean = False; const AContext: INyxApplicationContext = nil;
      const AResolver: INyxLogicResolver = nil;
      const APublicationSource: INyxViewPublicationSource = nil);
    function ReadPresentationSelection: TNyxPresentationSelection;
    function GetPresentationView: INyxPresentationView;
    procedure SetResourceContext(const AContext: INyxResourceContext);
    function GetResourceContext: INyxResourceContext;
    procedure SetResourceUpdates(const AQueue: INyxResourceUpdateQueue);
    procedure SetTableWorkbenchServices(const AServices: INyxTableWorkbenchServices);
    procedure SetTableTextLimits(const ALimits: TNyxTableTextWorkLimits);
    procedure WakeResources;
    procedure SetPresentationSelection(const AValue: TNyxPresentationSelection);
    { A prepared observer can target the final owner without publishing its
      candidate early. Its receiver rejects notifications until ownership moves. }
    procedure ObserveViewport(AReceiver: TNyxBrowserRenderer = nil);
    function MeasureContainers: INyxContainerSnapshot;
    function ReadViewportAllocation(out AWidth, AHeight: Double): Boolean;
    function ViewportReady: Boolean;
    function GetViewportContext: INyxViewportContext;
    function GetPresentationViewport: INyxViewportContext;
    procedure SetPresentationViewport(const AValue: INyxViewportContext);
    function GetPresentationViewportFailure: TNyxText;
    { Read actual untransformed CSS content boxes synchronously for a connected
      hidden candidate. Missing/disconnected boxes remain missing. }
    function MeasureAllocatedContainers: INyxContainerSnapshot;
    procedure ViewportChanged(AEntries: TJSHTMLResizeObserverEntryArray;
      AObserver: TJSHTMLResizeObserver);
    FDesignMode: Boolean;
    FProjectionContext: TNyxText;
    FProjectionSchemaRevision: Integer;
    FProjectionBaseline: TNyxNode;
    FBindings: array of TNyxBrowserBinding;
    FFactoryKinds: array of TNyxText;
    FDrawingNames: array of TNyxText;
    FDrawingProviders: array of INyxDrawingProvider;
    function DrawingProvider(ANode: TNyxNode): INyxDrawingProvider;
  private
    FFactories: array of TNyxBrowserFactory;
    FEventFactories: array of TNyxBrowserEventFactory;
    FEmitterScope: INyxEventEmitterScope;
    FUpdaters: array of TNyxBrowserUpdater;
    FOnEvent: TNyxBrowserEvent;
    FDesignerInput: TNyxDesignerInput;
    FOnDesignerGesture: TNyxDesignerGesture;
    FSelectedDesignID: TNyxText;
    FResizePreview: TNyxResizePreview;
    FDropPreview: TNyxDropPreview;
    FResizeEdges: array[0..7] of TJSHTMLElement;
    FResizePreviewHandler: TJSEventHandler;
    FResizeListening: Boolean;
    FCanvasResizeGrips: INyxCanvasResizeGrips;
    FCanvasMoveGrip: INyxCanvasMoveGrip;
    FCanvasMoveView: TNyxBrowserRenderer;
    FCanvasMoveHost: TJSHTMLElement;
    FCanvasGripViews: array[TNyxResizeAxis] of TNyxBrowserRenderer;
    FCanvasGripHosts: array[TNyxResizeAxis] of TJSHTMLElement;
    FEvents: INyxEvents;
    { Hidden candidates borrow the accepted parent's event router for table
      commands. It survives adoption; the candidate's own router does not. }
    FCollectionQueryEvents: INyxEvents;
    FEmbeddedContext: INyxApplicationContext;
    FEmbeddedResolver: INyxLogicResolver;
    FCanvasAdoptionHost: INyxCanvasRouteAdoptionHost;
    FState: TNyxState;
    FOwnState: Boolean;
    FLiveBindings: TNyxLiveBindings;
    FCollectionBindings: INyxCollectionBindings;
    { Own and inherited gates remain distinct through embedded publication. }
    FCollectionInputGate: INyxCollectionInputGate;
    FInheritedCollectionInputGate: INyxCollectionInputGate;
    FCollectionInputPhase: TNyxCollectionInputPhase;
    { Caller-owned executable registrations affect future candidates only.
      Built-ins compose through that same immutable membership and lifetime;
      accepted ports revoke before DOM, with cleanup deferred past adoption. }
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
    FForceValues: Boolean;
    { Scoped borrowed scalar rejection target; nil retains full forced sync. }
    FForceValueNode: TNyxNode;
    FOnBindingError: TNyxBrowserBindingError;
    FLastBindingError: TNyxText;
    FLastBindingFailure: TNyxBindingFailure;
    FLastGestureError: TNyxText;
    FOnGestureFailure: TNyxGestureFailure;
    procedure GestureFailed(const AOriginID, AReason: TNyxText);
    procedure SetDesignerInput(const AValue: TNyxDesignerInput);
    procedure Emit(AOrigin: TNyxNode; const ADispatch: TNyxDispatch);
    procedure BindCollectionPagers;
    function EmitNamed(const AOriginID: TNyxText; const AName: TNyxEventRef;
      const APayload: TNyxDataValue; AHasPayload: Boolean): Boolean;
    procedure BindingFailed(ANode: TNyxNode; const AReason: TNyxText;
      AFailure: TNyxBindingFailure = nbfRejected; ARestoreInput: Boolean = True);
    procedure UpdateResizePreview;
    function ResizePreviewViewport(AEvent: TJSEvent): Boolean;
    procedure ClearResizePreview;
    procedure UpdateResizeListening;
    procedure ClearCanvasResizeGrips;
    procedure UpdateCanvasResizeGrips;
    procedure ClearCanvasMoveGrip;
    procedure UpdateCanvasMoveGrip;
    function CanvasMovePoint(const APointer: TNyxPointerSnapshot): TNyxResizePoint;
    function CanvasResizePoint(AAxis: TNyxResizeAxis;
      const APointer: TNyxPointerSnapshot): TNyxResizePoint;
    procedure Clear(AKeepPresentation: Boolean = False;
      const AEventView: INyxPreparedEventView = nil);
    function FactoryIndex(ANode: TNyxNode): Integer;
    function CreateElement(ANode: TNyxNode; out AInput: TJSHTMLElement): TJSHTMLElement;
    procedure MountCanvases(ADocument: TNyxDocument);
    procedure MountToolCanvases(ADocument: TNyxDocument);
    function Build(ANode: TNyxNode): TJSHTMLElement;
    { Retain one eligible entry when a checked radio becomes disabled/hidden.
      Grouping follows the physical HTML name/form boundary, not display rows. }
    procedure SyncRadioFocus;
    { Exact field synchronization after full coordinator admission. Ordinary
      Sync remains authoritative for custom, composing or content-sized fields. }
    function SyncEditedValue(ANode: TNyxNode): Boolean;
    { Physical observations contain copied drafts and weak generation proof.
      Selective notification uses normal Sync, never a global forced restore. }
    function CaptureInputTargets(const AControls: array of TNyxControlRef): INyxPublicationReadiness;
    procedure SyncInputTargets(const AControls: array of TNyxControlRef);
    function InputStamp(const AID: TNyxText): TNyxText;
    function GetInputs: INyxInputCommands;
    { Reattach existing physical faces in admitted runtime-tree order. Guard
      host focus notifications and preserve editing ranges/scroll positions;
      no binding, callback, element or emitter is recreated. }
    procedure ArrangeControls(AFormer: TNyxNode);
  public
    constructor Create(ATheme: TNyxTheme = nil);
    destructor Destroy; override;
    { UI-thread registration before mounting. Named implementations are managed,
      copied into independent candidates/embedded canvas renderers, and must
      never retain a renderer/document/view cycle. Unknown names refuse. }
    procedure RegisterDrawingProvider(const AName: TNyxDrawingProviderRef;
      const AProvider: INyxDrawingProvider);
    procedure RegisterFactory(const AKind: TNyxText; AFactory: TNyxBrowserFactory;
      AUpdater: TNyxBrowserUpdater = nil);
    { Publish a custom producer using declared, typed named event schemas.
      Registration replaces an existing factory for the same exact kind. }
    procedure RegisterEventFactory(const AKind: TNyxKindRef;
      AFactory: TNyxBrowserEventFactory; AUpdater: TNyxBrowserUpdater = nil);
    { Reports this adapter's actual registration/base projection. A custom
      factory takes precedence over a derived primitive's default projection. }
    function Capability(ANode: TNyxNode): TNyxCapability;
    procedure Render(ADocument: TNyxDocument; ARoot: TNyxNode;
      AHost: TJSHTMLElement; ADesignMode: Boolean = False; AState: TNyxState = nil;
      const ACollections: INyxCollectionBindings = nil;
      AResourceMode: TNyxResourceRenderMode = nrmConfigured);
    { Same portable retained-projection guard as LCL. Stage independent current
      context/root/theme; only supported scalar presentation may reuse DOM,
      bindings and event scopes. False changes nothing and requests Render.
      Custom factories, scalar bindings, structure or context changes refuse
      reuse. No caller document/node is retained. Sync failures restore the
      previous owned properties before reporting the failure. Typed field restores
      reset only exact authored values, preserving independent other drafts;
      stale/mismatched restore groups refuse before any mutation. }
    function TryRefresh(ADocument: TNyxDocument; ARoot: TNyxNode;
      ADesignMode: Boolean; const ARestores: TNyxProjectionValueRestores = nil): Boolean;
    { Explicit form-owner changes over the same physical controls. Only named
      metadata keys inside the supplied exact forms may differ; ordinary scalar
      admission remains strict. Nil refuses before mutation. A nonnil result
      owns a reversible physical preview and fences old event work until grouped
      publication or retirement. Unchanged forms preserve independent drafts;
      changed forms reset accepted faces even when their defaults are unchanged.
      No caller document is retained. Keep the borrowed host alive through the
      result's retirement; target failures roll back before propagating. }
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
    { A supplied runtime store is borrowed and must outlive this mounted view.
      Otherwise the renderer owns a fresh copy of document defaults. Design mode
      projects that copy but does not subscribe or write application state.
      Failed control edits restore accepted values and report a diagnostic;
      LastBindingError is cleared by the next accepted control command. }
    { Borrow a projected host/control by runtime ID or original design ID. This
      lets applications compose nested Nyx views without global DOM queries or
      private component factories. Missing identities raise a model diagnostic. }
    function ElementFor(const AID: TNyxText;
      AIdentity: TNyxIdentityKind = niAutomatic): TJSHTMLElement;
    { Retain a declared workspace view by ordinary source/runtime identity.
      After Clear or replacement the retained interface is disconnected. }
    function CanvasView(const AID: TNyxText;
      AIdentity: TNyxIdentityKind = niAutomatic): INyxCanvasWorkspaceView;
    { Borrow the actual value input, including a labeled/framed editor's inner
      face. Mirrors the LCL adapter: missing identity raises; a component with
      no scalar input returns nil. Valid only until the mounted view ends. }
    function InputFor(const AID: TNyxText;
      AIdentity: TNyxIdentityKind = niAutomatic): TJSHTMLElement;
    { Borrow the declared focus face, including the split separator's divider.
      No declared face returns nil; missing identity raises. Bound collections
      return their logical host and manage their own descendant Tab entry.
      Never retain the element beyond this mounted view's lifetime. }
    function FocusFor(const AID: TNyxText;
      AIdentity: TNyxIdentityKind = niAutomatic): TJSHTMLElement;
    { Owned runtime text range, independent of document/design selection.
      SetTextSelection changes only the mounted input; no focus/scroll is forced. }
    function TextSelectionFor(const AID: TNyxText): TNyxTextSelection;
    function EditingFor(const AID: TNyxText): TNyxEditingSnapshot;
    procedure SetTextSelection(const AID: TNyxText; const ASelection: TNyxTextSelection);
    { Opt into one managed history authority for this exact physical mount.
      Browser history intents, including noncancelable input, share that owner. }
    procedure BindTextHistory(const AID: TNyxText;
      const ACommands: INyxTextHistoryCommands);
    { Read current local CSS-pixel offsets without changing state/selection.
      Signed offsets preserve the browser's RTL and overscroll conventions. }
    function ViewportFor(const AID: TNyxText): TNyxViewportSnapshot;
    { Integer logical outer-face allocation (offsetWidth/offsetHeight), excluding
      CSS transforms and including borders. Never returns a borrowed DOM handle. }
    function SizeFor(const AID: TNyxText;
      AIdentity: TNyxIdentityKind = niAutomatic): TNyxResizeSize;
    { Copied visible immediate siblings in the parent's logical CSS plane.
      Absolute layouts admit edge/center guides; flow layouts match sizes.
      Missing identity raises; roots have no guide context. No DOM is retained. }
    function AlignmentFor(const AID: TNyxText;
      AIdentity: TNyxIdentityKind = niAutomatic): TNyxAlignmentContext;
    { Map copied outer-face input to viewport coordinates, then into a chosen
      control's logical scale. Borrow mounted geometry synchronously. Mapping
      refuses missing/zero geometry and never retains DOM or event handles. }
    function ScreenPointFor(const AID: TNyxText;
      const APointer: TNyxPointerSnapshot): TNyxResizePoint;
    { Copied runtime primitive geometry; exact runtime identity is mandatory.
      No authored-owner fallback or reusable-definition guess is performed. }
    function DropFrameFor(const AID: TNyxText): TNyxDropFrame;
    { Inert placement paint, borrowing no tree. Default clears; unmount,
      selection or resize presentation retires this proposal. }
    procedure PreviewDrop(const APreview: TNyxDropPreview);
    function LogicalPointFor(const AID: TNyxText;
      const AScreen: TNyxResizePoint; AIdentity: TNyxIdentityKind = niAutomatic): TNyxResizePoint;
    { Read the containing mounted view's logical-pixel range/offset. Missing
      mount raises; this observation changes neither selection nor the model. }
    function ViewViewport: TNyxViewportSnapshot;
    { Set mounted containing-view logical pixel offsets. Browser bounds clamp
      against its actual scroll extent; no document/history command occurs. }
    procedure ScrollView(AX, AY: Integer);
    { Reveal the exact mounted runtime/design identity with nearest-edge
      scrolling. No control recreation, focus, text edit or history step. }
    procedure Reveal(const AID: TNyxText; AIdentity: TNyxIdentityKind = niAutomatic);
    { Navigate a mounted public code editor to a one-based source line. Focus
      stays inside the editor and never scrolls its containing designer canvas. }
    { One-based Unicode-scalar column; caret storage is translated for UTF-16.
      Focus is retained inside the editor without scrolling the whole document. }
    procedure NavigateCodeLine(const AID: TNyxText; ALine: Integer; AColumn: Integer = 1);
    { Mount a compiler-service browser artifact in an isolated view frame.
      URL is a relative service artifact. Renderer owns the mounted frame;
      a later normal Render replaces it through the same ownership boundary. }
    procedure RenderCompiled(const AArtifact: TNyxText; AHost: TJSHTMLElement);
    { True only for this exact live compiler artifact/launch. Authored projections
      have Root; compiled projections have independent mount ownership instead. }
    function IsCompiled(const AArtifact: TNyxText): Boolean;
    { Release the mounted view before disposing/replacing a containing Nyx host.
      The host remains caller-owned; no stale event bindings are retained. }
    procedure Unmount;
    { Attach a typed collection view to an already projected list/table/tree.
      Renderer retains and disconnects the managed attachment on unmount/remount;
      callers may retain its interface safely. A control accepts one live mount.
      This runtime bridge is also usable by custom application/component code. }
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
    { Move a mounted view without recreating controls or bindings. Caller owns
      both hosts; the new host must be empty and outside the old view. Retains
      a focused descendant's scalar text range and the host's scroll offsets.
      Refusal leaves the mounted view unchanged; same-host requests are no-ops.
      Compiled frames may move through the same boundary. }
    procedure MoveHost(AHost: TJSHTMLElement);
    { Outline the first visible realized face for an authored identity, retaining
      keyboard focus. Reusable parts share that identity; highlight their outer
      instance once rather than outlining every descendant independently. }
    procedure Select(const ADesignID: TNyxText);
    { Presentation only, for the selected authored face. Default clears it;
      runtime/foreign-selection proposals refuse before changing presentation.
      Pointer-transparent fixed paint strips follow captured scroll/resize,
      clip to the mounted host/viewport and never alter input or scroll extent. }
    procedure PreviewResize(const APreview: TNyxResizePreview);
    { Retain a public independent adornment; its document remains borrowed by
      the nested view. Nil retires subscriptions before removing its host. }
    procedure AttachMoveGrip(const AGrip: INyxCanvasMoveGrip);
    { Retain a public Nyx adornment for this selected authored face. Nil detaches.
      Three separately rendered specialized buttons keep application callbacks
      suppressed in the edited view. Caller disconnects editor receivers before
      destroying them; renderer owns only target views/hosts, not that receiver.
      Stable repeated attachment retains controls, focus and pointer capture. }
    procedure AttachResizeGrips(const AGrips: INyxCanvasResizeGrips);
    { Borrowed physical button for target input/capture validation. An absent
      adornment refuses; never remove or free this renderer-owned element. }
    function CanvasResizeElement(AAxis: TNyxResizeAxis): TJSHTMLElement;
    { Synchronize runtime properties while retaining DOM identity and focus.
      Compound actions use this path rather than rebuilding the entire view. }
    procedure Sync;
    property OnEvent: TNyxBrowserEvent read FOnEvent write FOnEvent;
    { Configure before mounting. Design drops use a synchronous value-only
      receiver, never the application's event registrations. Clear a borrowed
      receiver before its object ends. Defaults preserve selection-only hosts. }
    property DesignerInput: TNyxDesignerInput read FDesignerInput write SetDesignerInput;
    property OnDesignerGesture: TNyxDesignerGesture read FOnDesignerGesture write FOnDesignerGesture;
    { Managed multiple registrations are application/view scoped. Clear cancels
      queued invocations; renderer destruction closes every registration. }
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
      AHost: TJSHTMLElement; ADesignMode: Boolean; AState: TNyxState;
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
    { Optional enclosing presentation space; nil retains local logical host
      dimensions. Runtime Context survives ordinary replacement and retires on
      explicit Unmount/destruction; it never grants access to the host DOM. }
    property PresentationViewport: INyxViewportContext read GetPresentationViewport write SetPresentationViewport;
    property ViewportContext: INyxViewportContext read GetViewportContext;
    property PresentationViewportFailure: TNyxText read GetPresentationViewportFailure;
    { Portable managed capability. Borrowed receiver links retire on Unmount;
      caller-retained interfaces stay safe and never retain the renderer. }
    property Presentations: INyxPresentationView read GetPresentationView;
    { Copied projection purpose, matching the native adapter. Runtime views
      cannot mount designer input adornments or emit designer mutations. }
    property DesignMode: Boolean read FDesignMode;
    property State: TNyxState read FState;
    { Runtime-only weak commands over the existing binding owner, with physical
      draft/composition admission. Nil while unmounted or in design mode. }
    property Inputs: INyxInputCommands read GetInputs;
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
      attempt. Existing mounts never acquire replacement clipboard authority.
      Nil provides explicit Unavailable results for authored table actions. }
    property TableWorkbenchServices: INyxTableWorkbenchServices
      read FTableWorkbenchServices write SetTableWorkbenchServices;
    { Set before mounting or after Unmount. No runtime work limit is saved in
      a document, and already entered mounts keep their original copied limits. }
    property TableTextLimits: TNyxTableTextWorkLimits
      read FTableTextLimits write SetTableTextLimits;
    function ResourceReady: Boolean;
    { Same idle section-publication boundary as LCL: synchronous input dispatch,
      target synchronization and busy scalar/collection stores refuse. A caller
      queues replacement to another UI turn; True does not reserve or validate. }
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
    property OnBindingError: TNyxBrowserBindingError read FOnBindingError write FOnBindingError;
    property LastBindingError: TNyxText read FLastBindingError;
    property LastBindingFailure: TNyxBindingFailure read FLastBindingFailure;
    { A deferred recipe publication failure leaves its previous mounted tree.
      A new host/manual request retries; repeated identical failures do not loop. }
    property LastContentError: TNyxText read FLastContentError;
    { A host refusal is observable without changing the accepted model. The sink
      receives owned text and may dispose the view. A valid request clears it. }
    property LastGestureError: TNyxText read FLastGestureError;
    property OnGestureFailure: TNyxGestureFailure read FOnGestureFailure write FOnGestureFailure;
  end;

{ Restore focus without implicit scroll-to-control behavior. This typed bridge
  supplies the standard focus options absent from older pas2js Web declarations.
  The caller retains ownership of the borrowed control. }
procedure NyxFocusWithoutScroll(AControl: TJSHTMLElement);

implementation

uses
  nyx.canvas.browser, nyx.canvas.scene, nyx.tool.canvas.browser,
  nyx.collections, nyx.collections.query.browser, nyx.collections.query.controls,
  nyx.collections.query.workbench,
  nyx.collections.arrangement.browser,
  nyx.collections.cell.browser,
  nyx.application.resources, nyx.projection.context, nyx.coordinators.internal;

type
  { Copied axis-aligned DOM face transform. Browser layout owns the borrowed
    element; the value retains only screen origin and positive pixel ratios.
    Ancestor uniform zoom is included in getBoundingClientRect. This is not an
    arbitrary rotation/skew transform; empty axes retain the legacy unit ratio. }
  TBrowserFaceTransform = record
    X: Double;
    Y: Double;
    ScaleX: Double;
    ScaleY: Double;
    class function Capture(AElement: TJSHTMLElement): TBrowserFaceTransform; static;
    function LogicalPoint(AX, AY: Double): TNyxResizePoint;
    function ScreenPoint(AX, AY: Double): TNyxResizePoint;
  end;

  { One exact owned palette shared by the renderer and an entered generation.
    Replacing a palette releases only the replacing owner's lease. Supplied
    caller palettes remain borrowed and are never adopted by this helper. }
  TBrowserThemeOwner = class(TInterfacedObject)
  private
    FTheme: TNyxTheme;
  public
    constructor Create(ATheme: TNyxTheme);
    destructor Destroy; override;
  end;

  { A view has one canonical, terminal authority flag and one preallocated
    retirement shell. Clear moves the old fields there before any callout.
    Entered locals defer destruction of nodes, bindings, state and source;
    the shell never owns or closes the live renderer's event service. }
  TBrowserViewGeneration = class(TInterfacedObject)
  public
    Current: Boolean;
    RetiredView: TNyxBrowserRenderer;
    ThemeOwner: IInterface;
    BaseThemeOwner: IInterface;
    { Borrowed from this generation's active view or retirement shell. Input
      phase dispatch owns an outer generation lease while using this reader. }
    LiveBindings: TNyxLiveBindings;
    function ReadSignalSnapshot(ANode: TNyxNode;
      ATrigger: TNyxTrigger): TNyxEventInfo;
    constructor Create;
    destructor Destroy; override;
  end;

  { Private normal unwind after callback-driven view retirement. Only the
    synchronization boundary catches it; authored exceptions remain errors. }
  ENyxBrowserViewRetired = class(ENyxModel);

  { One remount's retained editing facts. The local managed lease array owns
    these objects until rollback/adoption finishes; a parallel typed array is
    only a borrowed view. Class storage gives pas2js the interface finalization
    that anonymous record members cannot provide. No renderer/widget is owned. }
  TBrowserCellEditTransfer = class(TInterfacedObject)
  public
    Identity: TNyxContentIdentity;
    Source: INyxCollectionEditMount;
    Snapshot: INyxCollectionEditsSnapshot;
    Store: INyxCollection;
    Focused: TNyxCollectionCellEdit;
  end;

  { Internal control-flow refusal. A router query may navigate while the shared
    keyboard dispatcher is asking for a fresh phase snapshot. Unwind that cycle
    before invoking its borrowed snapshot method; ordinary application errors
    still propagate to the listener's exception boundary. }
  ENyxColumnInputRetired = class(ENyxModel);

  { One synchronous keyboard cycle owns this adapter, the original command and
    real router. The callback itself still borrows the accepted live bindings;
    each Read checks the real revision and entry proof AFTER outward queries.
    Fresh per-phase payloads are preserved without retaining a renderer/tree or
    inventing a second keyboard dispatcher. }
  TColumnKeyboardSnapshot = class(TInterfacedObject)
  private
    FEvents: INyxEvents;
    FRevision: Integer;
    FCommand: INyxBrowserColumnCommand;
    FRead: TNyxSignalSnapshot;
  public
    constructor Create(const AEvents: INyxEvents; ARevision: Integer;
      const ACommand: INyxBrowserColumnCommand; ARead: TNyxSignalSnapshot);
    function Read(ANode: TNyxNode; ATrigger: TNyxTrigger): TNyxEventInfo;
  end;

  { Final presentation receiver is weak. A scope retains this managed port,
    never a renderer/controller; Sync is the last outward action in Request. }
  TNyxBrowserCoordinatorRefresh = class(TInterfacedObject, INyxCoordinatorRefreshTarget,
    INyxCoordinatorInputHost, INyxCoordinatorQueryHost)
  public
    Owner: TNyxBrowserRenderer;
    constructor Create(AOwner: TNyxBrowserRenderer);
    procedure Request;
    function InputsFor(ATree: TNyxNode): INyxInputCommands;
    function QueryDetailsFor(ATree: TNyxNode; const ATarget: TNyxControlRef;
      const AView: INyxCollectionView; const AEvents: TNyxEventTarget;
      const ADesignID: TNyxText): INyxCollectionQueryWorkbench;
  end;
  { Copied physical draft facts and the existing weak presentation receiver.
    The exact core port proves the mounted generation without retaining its
    widgets, renderer, document or retirement shell. Readiness invokes no user
    handlers and refuses newer physical input before any grouped installation. }
  TBrowserInputProof = class(TInterfacedObject, INyxPublicationReadiness)
  private
    FLease: INyxCoordinatorRefreshTarget;
    FReceiver: TNyxBrowserCoordinatorRefresh;
    FCommands: INyxInputCommands;
    FControls: array of TNyxControlRef;
    FDrafts: TNyxStrings;
  public
    constructor Create(AOwner: TNyxBrowserRenderer;
      const AControls: array of TNyxControlRef);
    destructor Destroy; override;
    procedure CheckReady;
  end;
  TNyxFocusOptions = class external name 'Object' (TJSObject)
    preventScroll: Boolean;
  end;
  TNyxFocusableElement = class external name 'HTMLElement' (TJSHTMLElement)
    procedure focus(AOptions: TNyxFocusOptions); reintroduce;
  end;
  TNyxWheelElement = class external name 'HTMLElement' (TJSHTMLElement)
    procedure Listen(const AName: String; AHandler: TJSMouseWheelEventHandler;
      AOptions: TJSObject); external name 'addEventListener';
  end;
  { Older Web declarations omit removeEventListener's capture argument. The
    standard DOM requires the same capture bit to revoke a key producer. }
  TNyxKeyboardElement = class external name 'HTMLElement' (TJSHTMLElement)
    procedure Unlisten(const AName: String; AHandler: TJSKeyEventHandler;
      ACapture: Boolean); external name 'removeEventListener';
  end;
  { This RTL exposes two-argument removal on Document. Capture listeners need
    the same capture flag on removal; keep that standard operation in this
    target-only declaration rather than editing the installed Web unit. }
  TNyxPreviewEventTarget = class external name 'EventTarget'(TJSEventTarget)
    procedure Unlisten(const AName: String; AHandler: TJSEventHandler;
      ACapture: Boolean); external name 'removeEventListener';
  end;

class function TBrowserFaceTransform.Capture(AElement: TJSHTMLElement): TBrowserFaceTransform;
var
  LFace: TJSDOMRect;
begin
  LFace := AElement.getBoundingClientRect;
  Result.X := LFace.left;
  Result.Y := LFace.top;
  Result.ScaleX := 1;
  Result.ScaleY := 1;

  if (AElement.offsetWidth > 0) and (LFace.width > 0) then
  begin
    Result.ScaleX := LFace.width / AElement.offsetWidth;
  end;

  if (AElement.offsetHeight > 0) and (LFace.height > 0) then
  begin
    Result.ScaleY := LFace.height / AElement.offsetHeight;
  end;
end;

function TBrowserFaceTransform.LogicalPoint(AX, AY: Double): TNyxResizePoint;
begin
  Result := NyxResizePoint((AX - X) / ScaleX, (AY - Y) / ScaleY);
end;

function TBrowserFaceTransform.ScreenPoint(AX, AY: Double): TNyxResizePoint;
begin
  Result := NyxResizePoint(X + AX * ScaleX, Y + AY * ScaleY);
end;

constructor TColumnKeyboardSnapshot.Create(const AEvents: INyxEvents;
  ARevision: Integer; const ACommand: INyxBrowserColumnCommand;
  ARead: TNyxSignalSnapshot);
begin
  inherited Create;
  FEvents := AEvents;
  FRevision := ARevision;
  FCommand := ACommand;
  FRead := ARead;
end;

function TColumnKeyboardSnapshot.Read(ANode: TNyxNode;
  ATrigger: TNyxTrigger): TNyxEventInfo;
var
  LRevision: Integer;
begin
  LRevision := FEvents.ViewRevision;

  if (LRevision <> FRevision) or not FCommand.DispatchCurrent then
  begin
    raise ENyxColumnInputRetired.Create('The original column key input retired');
  end;
  { This exact internal reader captures accepted tree facts without callouts.
    No borrowed live-binding access precedes the final original-command check. }
  Result := FRead(ANode, ATrigger);
end;

function CaptureBrowserViewport(AElement: TJSHTMLElement): TNyxViewportSnapshot;
begin
  Result := NyxViewport(
    NyxViewportAxis(AElement.scrollLeft, AElement.scrollWidth,
      AElement.clientWidth, nvuLogicalPixels),
    NyxViewportAxis(AElement.scrollTop, AElement.scrollHeight,
      AElement.clientHeight, nvuLogicalPixels),
    AElement.clientWidth, AElement.clientHeight);
end;

function TNyxBrowserRenderer.ViewViewport: TNyxViewportSnapshot;
begin
  FEvents.Scheduler.RequireUI;

  if (FHost = nil) or (FRoot = nil) then
  begin
    raise ENyxModel.Create('A containing viewport requires a mounted view');
  end;
  Result := CaptureBrowserViewport(FHost);
end;

procedure TNyxBrowserRenderer.Reveal(const AID: TNyxText; AIdentity: TNyxIdentityKind);
const
  CNearest = 'nearest';
var
  LOptions: TJSScrollIntoViewOptions;
begin
  FEvents.Scheduler.RequireUI;
  LOptions := TJSScrollIntoViewOptions.new;
  { DOM keywords stay at this adapter boundary. Public callers supply a typed
    identity, without embedding browser-specific scroll directives in designs. }
  LOptions.block := CNearest;
  LOptions.inline_ := CNearest;
  ElementFor(AID, AIdentity).scrollIntoView(LOptions);
end;

procedure TNyxBrowserRenderer.ScrollView(AX, AY: Integer);
begin
  FEvents.Scheduler.RequireUI;

  if (FHost = nil) or (FRoot = nil) then
  begin
    raise ENyxModel.Create('Scrolling a containing viewport requires a mounted view');
  end;
  FHost.scrollLeft := AX;
  FHost.scrollTop := AY;
end;

function TNyxBrowserRenderer.ViewportFor(const AID: TNyxText): TNyxViewportSnapshot;
var
  LElement: TJSHTMLElement;
  LIndex: Integer;
begin
  FEvents.Scheduler.RequireUI;
  LElement := ElementFor(AID);
  for LIndex := 0 to High(FBindings) do
  begin

    if (FBindings[LIndex].FElement = LElement) and (FBindings[LIndex].FInput <> nil) then
    begin
      LElement := FBindings[LIndex].FInput;
      Break;
    end;
  end;
  Result := CaptureBrowserViewport(LElement);
end;

function TNyxBrowserRenderer.SizeFor(const AID: TNyxText;
  AIdentity: TNyxIdentityKind): TNyxResizeSize;
var
  LElement: TJSHTMLElement;
begin
  FEvents.Scheduler.RequireUI;
  LElement := ElementFor(AID, AIdentity);
  Result := NyxResizeSize(Round(LElement.offsetWidth), Round(LElement.offsetHeight));
end;

function TNyxBrowserRenderer.DropFrameFor(const AID: TNyxText): TNyxDropFrame;
var
  LNode: TNyxNode;
  LElement: TJSHTMLElement;
  LFace: TJSDOMRect;
  LParent: TNyxControlRef;
begin
  FEvents.Scheduler.RequireUI;
  Result := Default(TNyxDropFrame);

  if FRoot = nil then
  begin
    raise ENyxModel.Create('Drop geometry requires a mounted view');
  end;
  LNode := FRoot.Find(AID);

  if LNode = nil then
  begin
    raise ENyxModel.Create('Drop geometry requires an exact mounted runtime primitive');
  end;
  LElement := ElementFor(AID, niRuntime);
  LFace := LElement.getBoundingClientRect;

  if (LFace.width <= 0) or (LFace.height <= 0) or
    (LElement.offsetWidth <= 0) or (LElement.offsetHeight <= 0) then
  begin
    Exit;
  end;
  LParent := Default(TNyxControlRef);

  if LNode.Parent <> nil then
  begin
    LParent := NyxControl(LNode.Parent.ID);
  end;
  Result := NyxDropFrame(NyxGuideBox(LFace.left, LFace.top, LFace.width, LFace.height),
    LElement.offsetWidth, LElement.offsetHeight, LParent,
    NyxDesignerParentAxis(LNode), NyxDesignerTarget(LNode).Container);
end;

function TNyxBrowserRenderer.ScreenPointFor(const AID: TNyxText;
  const APointer: TNyxPointerSnapshot): TNyxResizePoint;
var
  LTransform: TBrowserFaceTransform;
begin
  FEvents.Scheduler.RequireUI;

  if not APointer.HasPosition then
  begin
    raise ENyxModel.Create('Pointer mapping requires actual position');
  end;
  LTransform := TBrowserFaceTransform.Capture(ElementFor(AID));
  Result := LTransform.ScreenPoint(APointer.X, APointer.Y);
end;

function TNyxBrowserRenderer.LogicalPointFor(const AID: TNyxText;
  const AScreen: TNyxResizePoint; AIdentity: TNyxIdentityKind): TNyxResizePoint;
var
  LElement: TJSHTMLElement;
  LFace: TJSDOMRect;
begin
  FEvents.Scheduler.RequireUI;
  LElement := ElementFor(AID, AIdentity);
  LFace := LElement.getBoundingClientRect;

  if not AScreen.Defined or (LElement.offsetWidth <= 0) or (LElement.offsetHeight <= 0) or
    (LFace.width <= 0) or (LFace.height <= 0) then
  begin
    raise ENyxModel.Create('Logical pointer mapping requires positive mounted geometry');
  end;
  Result := NyxResizePoint(AScreen.X * LElement.offsetWidth / LFace.width,
    AScreen.Y * LElement.offsetHeight / LFace.height);
end;

function TNyxBrowserRenderer.AlignmentFor(const AID: TNyxText;
  AIdentity: TNyxIdentityKind): TNyxAlignmentContext;
var
  LElement, LParent, LPeer: TJSHTMLElement;
  LRect, LParentRect: TJSDOMRect;
  LNode: TNyxNode;
  LIndex: Integer;
  LScaleX, LScaleY: Double;

  function BoxFor(AElement: TJSHTMLElement): TNyxGuideBox;
  var
    LBox: TJSDOMRect;
  begin
    LBox := AElement.getBoundingClientRect;
    Result := NyxGuideBox((LBox.left - LParentRect.left) / LScaleX - LParent.clientLeft + LParent.scrollLeft,
      (LBox.top - LParentRect.top) / LScaleY - LParent.clientTop + LParent.scrollTop,
      AElement.offsetWidth, AElement.offsetHeight);
  end;

begin
  FEvents.Scheduler.RequireUI;
  Result := Default(TNyxAlignmentContext);
  LElement := ElementFor(AID, AIdentity);
  LNode := nil;
  for LIndex := 0 to High(FBindings) do
  begin

    if FBindings[LIndex].FElement = LElement then
    begin
      LNode := FBindings[LIndex].FNode;
      Break;
    end;
  end;

  if (LNode = nil) or (LNode.Parent = nil) then
  begin
    Exit;
  end;
  LParent := TJSHTMLElement(LElement.parentElement);
  LParentRect := LParent.getBoundingClientRect;
  LScaleX := 1;
  LScaleY := 1;

  if LParent.offsetWidth > 0 then
  begin
    LScaleX := LParentRect.width / LParent.offsetWidth;
  end;

  if LParent.offsetHeight > 0 then
  begin
    LScaleY := LParentRect.height / LParent.offsetHeight;
  end;

  if (LScaleX <= 0) or (LScaleY <= 0) then
  begin
    Exit;
  end;
  Result := NyxAlignmentContext(BoxFor(LElement),
    NyxGuideBox(0, 0, LParent.clientWidth, LParent.clientHeight),
    NyxControl(LNode.Parent.ID)).Positions(NyxLayout(LNode.Parent) = 'absolute');
  for LIndex := 0 to High(FBindings) do
  begin
    LPeer := FBindings[LIndex].FElement;

    if (FBindings[LIndex].FNode.Parent <> LNode.Parent) or (LPeer = LElement) or
      not NyxInteractionPolicy(FBindings[LIndex].FNode).Visible then
    begin
      Continue;
    end;
    LRect := LPeer.getBoundingClientRect;

    if (LRect.width <= 0) or (LRect.height <= 0) or
      (LRect.right <= LParentRect.left) or (LRect.left >= LParentRect.right) or
      (LRect.bottom <= LParentRect.top) or (LRect.top >= LParentRect.bottom) then
    begin
      Continue;
    end;

    if Result.PeerCount = NyxMaximumGuidePeers then
    begin
      Break;
    end;
    Result := Result.Peer(NyxControl(FBindings[LIndex].FNode.ID), BoxFor(LPeer));
  end;
end;

procedure NyxFocusWithoutScroll(AControl: TJSHTMLElement);
var
  LOptions: TNyxFocusOptions;
begin

  if AControl <> nil then
  begin
    LOptions := TNyxFocusOptions.new;
    LOptions.preventScroll := True;
    TNyxFocusableElement(AControl).focus(LOptions);
  end;
end;

var
  GThemeScope: Integer = 0;

procedure TNyxBrowserRenderer.NavigateCodeLine(const AID: TNyxText; ALine: Integer; AColumn: Integer);
var
  LEditor: TJSHTMLTextAreaElement;
  LElement: TJSHTMLElement;
  LIndex: Integer;
  LLineHeight: Double;
begin
  LElement := ElementFor(AID);

  if not (LElement is TJSHTMLTextAreaElement) then
  begin
    LElement := TJSHTMLElement(LElement.querySelector('textarea'));
  end;

  if (LElement = nil) or (ALine < 1) or (AColumn < 1) then
  begin
    raise ENyxModel.Create('Source navigation requires a code editor and source line');
  end;
  LEditor := TJSHTMLTextAreaElement(LElement);
  LIndex := NyxTextPosition(LEditor.value, ALine, AColumn) - 1;
  NyxFocusWithoutScroll(LEditor);
  LEditor.selectionStart := LIndex;
  LEditor.selectionEnd := LIndex;
  LLineHeight := parseFloat(window.getComputedStyle(LEditor).getPropertyValue('line-height'));

  if not (LLineHeight > 0) then
  begin
    LLineHeight := 20;
  end;
  LEditor.scrollTop := Round((ALine - 1) * LLineHeight);
end;

function Element(const ATag, AClass: TNyxText): TJSHTMLElement;
begin
  Result := TJSHTMLElement(document.createElement(ATag));
  Result.className := AClass;
end;

function SafeURL(const AValue: TNyxText; AImage: Boolean): Boolean;
var
  LValue: TNyxText;
begin
  LValue := LowerCase(Trim(AValue));
  Result := (LValue = '') or (Pos('https://', LValue) = 1) or
    (Pos('http://', LValue) = 1) or (Pos('./', LValue) = 1) or
    (Pos('../', LValue) = 1) or (Pos('/', LValue) = 1) or (Pos('#', LValue) = 1);

  if AImage then
  begin

    if Pos('data:', LValue) = 1 then
    begin
      { Use the portable admission boundary, including the exact authored policy
        parameter. A broader data: prefix would also admit unsupported/scriptable
        media. No literal-prefix duplicate may drop an explicit caller choice. }
      try
        Result := TNyxImageSource.FromWire(AValue).Kind = nisEmbedded;
      except
        on ENyxImage do
        begin
          Result := False;
        end;
      end;
    end;
  end
  else
  begin
    Result := Result or (Pos('mailto:', LValue) = 1);
  end;
end;

function TNyxBrowserCoordinatorRefresh.QueryDetailsFor(ATree: TNyxNode;
  const ATarget: TNyxControlRef; const AView: INyxCollectionView;
  const AEvents: TNyxEventTarget; const ADesignID: TNyxText): INyxCollectionQueryWorkbench;
var
  LLease: INyxCoordinatorRefreshTarget;
  LOwner: TNyxBrowserRenderer;
  LFactory: INyxQueryFormPresenterFactory;
begin
  LLease := Self;
  Result := nil;
  LOwner := Owner;

  if (LOwner = nil) or (LOwner.FRoot <> ATree) or (InputsFor(ATree) = nil) then
  begin
    Exit;
  end;

  if (Owner <> LOwner) or (LOwner.CollectionView(ATarget.ID) <> AView) then
  begin
    Exit;
  end;
  LFactory := NewNyxBrowserQueryFormFactory(LOwner.ElementFor(ATarget.ID), LOwner.FTheme);
  try

    if (Owner <> LOwner) or (LOwner.FRoot <> ATree) then
    begin
      LFactory.Revoke;
      Exit;
    end;
    Result := NewNyxCollectionQueryDetails(AView, LOwner.FEvents, AEvents, LFactory, ADesignID);

    if (Owner <> LOwner) or (LOwner.FRoot <> ATree) then
    begin
      Result.Disconnect;
      Result := nil;
    end;
  except
    LFactory.Revoke;
    raise;
  end;
end;
function TNyxBrowserCoordinatorRefresh.InputsFor(ATree: TNyxNode): INyxInputCommands;
var
  LInputs: INyxInputCommands;
begin
  Result := nil;

  if (Owner <> nil) and (Owner.FRoot = ATree) then
  begin
    LInputs := Owner.GetInputs;

    if (LInputs <> nil) and LInputs.Connected then
    begin
      Result := LInputs;
    end;
  end;
end;

constructor TNyxBrowserCoordinatorRefresh.Create(AOwner: TNyxBrowserRenderer);
begin
  inherited Create;
  Owner := AOwner;
end;

procedure TNyxBrowserCoordinatorRefresh.Request;
var
  LLease: INyxCoordinatorRefreshTarget;
  LOwner: TNyxBrowserRenderer;
begin
  LLease := Self;
  LOwner := Owner;

  if LOwner <> nil then
  begin
    LOwner.Sync;
  end;
end;

constructor TBrowserThemeOwner.Create(ATheme: TNyxTheme);
begin
  inherited Create;
  FTheme := ATheme;
end;

destructor TBrowserThemeOwner.Destroy;
var
  LTheme: TNyxTheme;
begin
  LTheme := FTheme;
  FTheme := nil;
  LTheme.Free;
  inherited Destroy;
end;

constructor TBrowserViewGeneration.Create;
begin
  inherited Create;
  RetiredView := TNyxBrowserRenderer.CreateRetirementShell;
  Current := True;
end;

destructor TBrowserViewGeneration.Destroy;
begin
  Current := False;
  FreeAndNil(RetiredView);
  inherited Destroy;
end;

function TBrowserViewGeneration.ReadSignalSnapshot(ANode: TNyxNode;
  ATrigger: TNyxTrigger): TNyxEventInfo;
begin

  if not Current then
  begin
    raise ENyxBrowserViewRetired.Create('The browser view was retired');
  end;
  Result := LiveBindings.SignalSnapshot(ANode, ATrigger);

  if not Current then
  begin
    raise ENyxBrowserViewRetired.Create('The browser view was retired');
  end;
end;

{$I nyx.projection.context.adapter.inc}

constructor TNyxBrowserRenderer.CreateRetirementShell;
begin
  inherited Create;
  FRetirementShell := True;
  FDetachedRetirementHost := TJSHTMLElement(document.createElement('div'));
end;

constructor TNyxBrowserRenderer.Create(ATheme: TNyxTheme);
begin
  inherited Create;
  FTableTextLimits := TNyxTableTextWorkLimits.Defaults;
  Inc(GThemeScope);
  FThemeScope := 'nyx-theme-' + IntToStr(GThemeScope);
  FTheme := ATheme;
  FBorrowedTheme := ATheme;
  FOwnTheme := ATheme = nil;

  if FOwnTheme then
  begin
    FTheme := TNyxTheme.Create;
    FThemeOwner := TBrowserThemeOwner.Create(FTheme);
    FOwnTheme := False;
  end;
  FTheme.Validate;
  FBaseTheme := NewNyxDocumentTheme(nil, FTheme);
  FBaseThemeOwner := TBrowserThemeOwner.Create(FBaseTheme);
  FGenerationObject := TBrowserViewGeneration.Create;
  FGeneration := TBrowserViewGeneration(FGenerationObject);
  TBrowserViewGeneration(FGenerationObject).ThemeOwner := FThemeOwner;
  TBrowserViewGeneration(FGenerationObject).BaseThemeOwner := FBaseThemeOwner;
  FEvents := NewNyxEvents;
  FCollectionQueryEvents := FEvents;
  FCoordinatorRefreshObject := TNyxBrowserCoordinatorRefresh.Create(Self);
  FCoordinatorRefreshTarget := TNyxBrowserCoordinatorRefresh(FCoordinatorRefreshObject);
  FContentGuard := NewNyxContentGuard(@QueueContent);
  Supports(FContentGuard, INyxContentLifetime, FContentLifetime);
  FResizePreviewHandler := @ResizePreviewViewport;
  FViewportBinding := NewNyxViewportBinding(FEvents.Scheduler, npfBrowser,
    @ReadViewportAllocation, @ViewportReady, @Sync);
end;

destructor TNyxBrowserRenderer.Destroy;
var
  LRetiringCoordinators: TNyxCoordinatorMounts;
  LEvents: INyxEvents;
  LTheme: TNyxTheme;
  LBaseTheme: TNyxTheme;
  LOwnTheme: Boolean;
  LThemeOwner: IInterface;
  LBaseThemeOwner: IInterface;
begin

  if FRetirementShell then
  begin
    try
      try
        ReleaseDetached;
      except
        { Entered generations already retired their outward producers. Final
          release cannot report a cleanup error through a destructor. }
      end;
    finally
      inherited Destroy;
    end;
    Exit;
  end;
  { No producer may regain authority while service-close callbacks execute. }

  if FViewportBinding <> nil then
  begin
    FViewportBinding.Retire;
    FViewportBinding := nil;
  end;

  if FGenerationObject <> nil then
  begin
    TBrowserViewGeneration(FGenerationObject).Current := False;
  end;
  LEvents := FEvents;
  { Flag-only weak receiver revocation precedes event/canvas/control cleanup. }

  if FCoordinatorRefreshObject <> nil then
  begin
    TNyxBrowserCoordinatorRefresh(FCoordinatorRefreshObject).Owner := nil;
  end;
  LRetiringCoordinators := FCoordinatorMounts;
  RevokeNyxCoordinatorMounts(LRetiringCoordinators);
  FCoordinatorMounts := nil;
  FCoordinatorIDs := nil;

  if FContentGuard <> nil then
  begin
    FContentGuard.Retire;
  end;
  { Revoke the exact asset duties before the first fallible cleanup. The local
    managed owners keep borrowed palettes alive through coordinator/event
    retirement; no later destructor tail can clear a reentrant successor's
    fields. Raw fallbacks cover only partially constructed owned palettes. }
  LTheme := FTheme;
  LBaseTheme := FBaseTheme;
  LOwnTheme := FOwnTheme;
  LThemeOwner := FThemeOwner;
  LBaseThemeOwner := FBaseThemeOwner;
  FTheme := nil;
  FBaseTheme := nil;
  FOwnTheme := False;
  FThemeOwner := nil;
  FBaseThemeOwner := nil;
  try
    try
      Clear;
    except
      { Explicit Unmount reports the same supported disconnect failure. Final
        destruction must still attempt every independent retirement below. }
    end;
    try
      DisconnectNyxCoordinatorMounts(LRetiringCoordinators);
    except
      { The ordinary coordinator owner attempts every registered participant. }
    end;
    try

      if LEvents <> nil then
      begin
        LEvents.Close;
      end;
    except
      { Event retirement cannot skip the exact remaining palette duties. }
    end;
    try

      if LBaseThemeOwner = nil then
      begin
        LBaseTheme.Free;
      end;
    except
      { Independent assets still receive their own final release attempt. }
    end;
    try

      if LOwnTheme then
      begin
        LTheme.Free;
      end;
    except
      { Extension destructors must not raise; explicit cleanup owns reporting. }
    end;
  finally
    inherited Destroy;
  end;
end;

procedure TNyxBrowserRenderer.Clear(AKeepPresentation: Boolean;
  const AEventView: INyxPreparedEventView);
var
  LGeneration: TBrowserViewGeneration;
  LLease: IInterface;
  LRetired: TNyxBrowserRenderer;
begin
  LGeneration := TBrowserViewGeneration(FGenerationObject);
  LLease := FGeneration;

  if LGeneration = nil then
  begin
    Exit;
  end;
  LGeneration.Current := False;
  LRetired := LGeneration.RetiredView;
  DetachView(LRetired, AKeepPresentation);
  FGenerationObject := nil;
  FGeneration := nil;
  { From here on cleanup reads only the displaced shell. A cleanup receiver may
    mount a successor, or free the original renderer, without old finally tails
    clearing its fields. The local lease also covers cleanup exceptions. }
  LRetired.RetireDetached(AEventView);
end;

procedure TNyxBrowserRenderer.DetachView(ATarget: TNyxBrowserRenderer;
  AKeepPresentation: Boolean);
var
  LIndex: Integer;
  LAxis: TNyxResizeAxis;
begin
  { This block performs assignments only. Every teardown-owned field leaves
    the live receiver BEFORE the first subscription/control/service callout. }
  ATarget.FEvents := FEvents;
  ATarget.FTheme := FTheme;
  ATarget.FThemeOwner := FThemeOwner;
  ATarget.FBaseTheme := FBaseTheme;
  ATarget.FBaseThemeOwner := FBaseThemeOwner;
  ATarget.FRoot := FRoot;
  FRoot := nil;
  ATarget.FState := FState;
  FState := nil;
  ATarget.FStateOwner := FStateOwner;
  FStateOwner := nil;
  ATarget.FOwnState := FOwnState;
  FOwnState := False;
  ATarget.FLiveBindings := FLiveBindings;
  FLiveBindings := nil;
  ATarget.FBindings := FBindings;
  FBindings := nil;
  ATarget.FHost := FHost;
  FHost := nil;
  ATarget.FDetachedCandidate := FDetachedCandidate;
  ATarget.FCompiledMount := FCompiledMount;
  FCompiledMount := nil;
  FCompiledArtifact := '';
  ATarget.FViewportObserver := FViewportObserver;
  FViewportObserver := nil;
  ATarget.FStagingMeasurements := FStagingMeasurements;
  FStagingMeasurements := nil;
  ATarget.FContentBlueprint := FContentBlueprint;
  FContentBlueprint := nil;
  FContentFrameKnown := False;
  FContentMeasurementKey := '';
  ATarget.FViewPublicationSource := FViewPublicationSource;
  FViewPublicationSource := nil;
  ATarget.FCallerCoordinatorSnapshot := FCallerCoordinatorSnapshot;
  FCallerCoordinatorSnapshot := nil;
  ATarget.FCoordinatorMounts := FCoordinatorMounts;
  FCoordinatorMounts := nil;
  ATarget.FCoordinatorIDs := FCoordinatorIDs;
  FCoordinatorIDs := nil;
  ATarget.FCollectionBindings := FCollectionBindings;
  FCollectionBindings := nil;
  ATarget.FCollectionInputGate := FCollectionInputGate;
  FCollectionInputGate := nil;
  ATarget.FEmitterScope := FEmitterScope;
  FEmitterScope := nil;
  ATarget.FProjectionBaseline := FProjectionBaseline;
  FProjectionBaseline := nil;
  FProjectionContext := '';
  FProjectionSchemaRevision := 0;
  ATarget.FDisclosureFocusWork := FDisclosureFocusWork;
  FDisclosureFocusWork := nil;
  ATarget.FDisclosureFocusExecution := FDisclosureFocusExecution;
  FDisclosureFocusExecution := nil;
  FDisclosureFocusOrigin := '';
  FDisclosureFocusHeader := '';
  FDisclosureFocusActive := nil;
  FDisclosureFocusExpected := nil;

  if not AKeepPresentation then
  begin
    ATarget.FPresentationView := FPresentationView;
    FPresentationView := nil;
    ATarget.FContentWork := FContentWork;
    FContentWork := nil;
    ATarget.FContentExecution := FContentExecution;
    FContentExecution := nil;
    FContentRequestedSelection := TNyxPresentationSelection.None;
    FLastContentError := '';
  end;
  FPresentationSelection := TNyxPresentationSelection.None;
  ATarget.FCanvasResizeGrips := FCanvasResizeGrips;
  FCanvasResizeGrips := nil;
  ATarget.FCanvasMoveGrip := FCanvasMoveGrip;
  FCanvasMoveGrip := nil;
  ATarget.FCanvasMoveView := FCanvasMoveView;
  FCanvasMoveView := nil;
  ATarget.FCanvasMoveHost := FCanvasMoveHost;
  FCanvasMoveHost := nil;
  for LAxis := Low(TNyxResizeAxis) to High(TNyxResizeAxis) do
  begin
    ATarget.FCanvasGripViews[LAxis] := FCanvasGripViews[LAxis];
    FCanvasGripViews[LAxis] := nil;
    ATarget.FCanvasGripHosts[LAxis] := FCanvasGripHosts[LAxis];
    FCanvasGripHosts[LAxis] := nil;
  end;
  for LIndex := Low(FResizeEdges) to High(FResizeEdges) do
  begin
    ATarget.FResizeEdges[LIndex] := FResizeEdges[LIndex];
    FResizeEdges[LIndex] := nil;
  end;
  ATarget.FResizePreview := FResizePreview;
  FResizePreview := Default(TNyxResizePreview);
  ATarget.FDropPreview := FDropPreview;
  FDropPreview := Default(TNyxDropPreview);
  ATarget.FResizePreviewHandler := FResizePreviewHandler;
  ATarget.FResizeListening := FResizeListening;
  FResizeListening := False;
  FSelectedDesignID := '';
  FUpdating := False;
  FForceValues := False;
  FForceValueNode := nil;
end;

procedure TNyxBrowserRenderer.RetireDetached(
  const AEventView: INyxPreparedEventView);
var
  LIndex: Integer;
  LHost: TJSHTMLElement;
  LParent: TJSNode;
  LChildren: array of TJSNode;
  LRetiringCoordinators: TNyxCoordinatorMounts;
  LFailure: JSValue;
  LHasFailure: Boolean;

  procedure RememberFailure(AFailure: JSValue);
  begin
    { pas2js exceptions are retained JavaScript values. Keep the original
      Pascal exception OR platform throw, rather than inventing a replacement
      diagnostic or relying on native-only AcquireExceptionObject. }

    if not LHasFailure then
    begin
      LFailure := AFailure;
      LHasFailure := True;
    end;
  end;

begin
  LHasFailure := False;
  LFailure := Undefined;
  LHost := FHost;
  LParent := nil;
  LChildren := nil;

  if LHost <> nil then
  begin
    LParent := LHost.parentNode;
    SetLength(LChildren, LHost.childNodes.length);
    for LIndex := 0 to High(LChildren) do
    begin
      LChildren[LIndex] := LHost.childNodes[LIndex];
    end;
  end;
  { The shell never clears the external host after a callout. Its preallocated
    holder receives only the captured original children still owned by that
    host; cleanup-triggered successor children are a different cohort. }
  FHost := FDetachedRetirementHost;
  FUpdating := True;
  LRetiringCoordinators := FCoordinatorMounts;
  FCoordinatorMounts := nil;
  FCoordinatorIDs := nil;
  try

    if FViewportObserver <> nil then
    begin
      FViewportObserver.disconnect;
      FViewportObserver := nil;
    end;
  except
    RememberFailure(JSExceptValue);
  end;
  try

    if FResizeListening then
    begin
      TNyxPreviewEventTarget(document).Unlisten('scroll', FResizePreviewHandler, True);
      window.removeEventListener('resize', FResizePreviewHandler);
      FResizeListening := False;
    end;
  except
    RememberFailure(JSExceptValue);
  end;
  { Canonical subscription/gate retirement is noncalling and must precede
    every cancellation or target callback that can observe the external store. }
  try

    if FLiveBindings <> nil then
    begin
      FLiveBindings.Retire;
    end;
  except
    RememberFailure(JSExceptValue);
  end;
  try

    if FCollectionInputGate <> nil then
    begin
      FCollectionInputGate.Retire;
    end;
  except
    RememberFailure(JSExceptValue);
  end;
  { Retire only the old event generation BEFORE any arbitrary disposal,
    cancellation or custom-element disconnect reaction can mount a successor. }
  try

    if AEventView <> nil then
    begin
      AEventView.Install;
    end
    else if FEvents <> nil then
    begin
      FEvents.CancelPending;
    end;
  except
    RememberFailure(JSExceptValue);
  end;
  try
    RevokeNyxCoordinatorMounts(LRetiringCoordinators);
  except
    RememberFailure(JSExceptValue);
  end;
  for LIndex := 0 to High(LChildren) do
  begin
    try

      if LChildren[LIndex].parentNode = LHost then
      begin
        FDetachedRetirementHost.appendChild(LChildren[LIndex]);
      end;
    except
      RememberFailure(JSExceptValue);
    end;
  end;
  try
    { A hidden candidate owns its empty probe. A callback that has reused that
      probe or moved it elsewhere takes precedence over this old cleanup. }

    if FDetachedCandidate and (LHost <> nil) and
      (LHost.parentNode = LParent) and (LHost.childNodes.length = 0) then
    begin
      LHost.remove;
    end;
  except
    RememberFailure(JSExceptValue);
  end;
  for LIndex := 0 to High(FBindings) do
  begin
    try
      FBindings[LIndex].Disconnect;
    except
      RememberFailure(JSExceptValue);
    end;
  end;
  try
    FreeAndNil(FCompiledMount);
  except
    RememberFailure(JSExceptValue);
  end;
  FCompiledArtifact := '';
  try

    if FDisclosureFocusWork <> nil then
    begin
      FDisclosureFocusWork.Retire;
    end;
  except
    RememberFailure(JSExceptValue);
  end;
  try

    if FDisclosureFocusExecution <> nil then
    begin
      FDisclosureFocusExecution.Cancel;
    end;
  except
    RememberFailure(JSExceptValue);
  end;
  try

    if FPresentationView <> nil then
    begin
      FPresentationView.Retire;
    end;
  except
    RememberFailure(JSExceptValue);
  end;
  try

    if FContentWork <> nil then
    begin
      FContentWork.Retire;
    end;
  except
    RememberFailure(JSExceptValue);
  end;
  try

    if FContentExecution <> nil then
    begin
      FContentExecution.Cancel;
    end;
  except
    RememberFailure(JSExceptValue);
  end;
  try
    ClearCanvasResizeGrips;
  except
    RememberFailure(JSExceptValue);
  end;
  try
    ClearCanvasMoveGrip;
  except
    RememberFailure(JSExceptValue);
  end;
  try
    ClearResizePreview;
  except
    RememberFailure(JSExceptValue);
  end;
  try

    if FEmitterScope <> nil then
    begin
      FEmitterScope.Disconnect;
    end;
  except
    RememberFailure(JSExceptValue);
  end;
  for LIndex := 0 to High(FBindings) do
  begin
    try

      if FBindings[LIndex].FCollectionMount <> nil then
      begin
        FBindings[LIndex].FCollectionMount.Disconnect;
      end;
    except
      RememberFailure(JSExceptValue);
    end;
    try

      if FBindings[LIndex].FToolCanvasView <> nil then
      begin
        FBindings[LIndex].FToolCanvasView.Disconnect;
      end;
    except
      RememberFailure(JSExceptValue);
    end;
    try

      if FBindings[LIndex].FCanvasView <> nil then
      begin
        FBindings[LIndex].FCanvasView.Disconnect;
      end;
    except
      RememberFailure(JSExceptValue);
    end;
    try

      if FBindings[LIndex].FDrawingView <> nil then
      begin
        FBindings[LIndex].FDrawingView.Disconnect;
      end;
    except
      RememberFailure(JSExceptValue);
    end;
    try

      if FBindings[LIndex].FCollectionPager <> nil then
      begin
        FBindings[LIndex].FCollectionPager.Disconnect;
      end;
    except
      RememberFailure(JSExceptValue);
    end;
  end;
  try

    if AEventView = nil then
    begin
      DisconnectNyxCoordinatorMounts(LRetiringCoordinators);
    end;
  except
    RememberFailure(JSExceptValue);
  end;
  { Exact source/factory/state/palette owners remain in this shell until every
    entered call returns. Failure does not skip another independent retirement. }

  if LHasFailure then
  begin
    { Casting an arbitrary JSValue to Exception emits rtl.getObject, which
      converts undefined and other primitive throws to null. Preserve the
      original platform value through this minimal pas2js exception bridge. }
    asm
      throw LFailure;
    end;
  end;
end;

procedure TNyxBrowserRenderer.ReleaseDetached;
var
  LIndex: Integer;
begin
  { All outward producers were disconnected in RetireDetached. This final
    release runs only after cleanup and every entered generation lease unwind. }
  FreeAndNil(FLiveBindings);
  for LIndex := 0 to High(FBindings) do
  begin
    FBindings[LIndex].Free;
  end;
  FBindings := nil;
  FreeAndNil(FContentBlueprint);
  ReleaseNyxNode(FProjectionBaseline);
  ReleaseNyxNode(FRoot);

  if FOwnState then
  begin
    FState.Free;
  end;
  FState := nil;
  FStateOwner := nil;
  FCollectionBindings := nil;
  FViewPublicationSource := nil;
  FTheme := nil;
  FBaseTheme := nil;
  FThemeOwner := nil;
  FBaseThemeOwner := nil;
end;

procedure TNyxBrowserRenderer.RegisterDrawingProvider(const AName: TNyxDrawingProviderRef;
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

function TNyxBrowserRenderer.DrawingProvider(ANode: TNyxNode): INyxDrawingProvider;
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

procedure TNyxBrowserRenderer.RegisterFactory(const AKind: TNyxText;
  AFactory: TNyxBrowserFactory; AUpdater: TNyxBrowserUpdater);
var
  LIndex: Integer;
begin

  if (AKind = '') or not Assigned(AFactory) then
  begin
    raise ENyxModel.Create('Custom browser kind and factory are required');
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

procedure TNyxBrowserRenderer.RegisterEventFactory(const AKind: TNyxKindRef;
  AFactory: TNyxBrowserEventFactory; AUpdater: TNyxBrowserUpdater);
var
  LIndex: Integer;
begin

  if (AKind.Name = '') or not Assigned(AFactory) then
  begin
    raise ENyxModel.Create('Custom browser kind and event factory are required');
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

function TNyxBrowserRenderer.FactoryIndex(ANode: TNyxNode): Integer;
var
  LIndex: Integer;
begin
  Result := -1;
  { Exact semantic overrides win. A base factory also customizes derivatives,
    without requiring one registration for every derived recipe name. }
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

function TNyxBrowserRenderer.Capability(ANode: TNyxNode): TNyxCapability;
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
    Result := LInfo.Browser;
  end;
end;

function TNyxBrowserRenderer.CreateElement(ANode: TNyxNode;
  out AInput: TJSHTMLElement): TJSHTMLElement;
var
  LKind: TNyxText;
  LInput: TJSHTMLInputElement;
  LCaption: TJSHTMLElement;
  LIndex: Integer;
  LURL: TNyxText;
  LInfo: TNyxPrimitiveInfo;
  LFactoryIndex: Integer;
begin
  AInput := nil;
  LKind := ANode.ProjectionKind;
  ValidateNyxProperties(ANode);
  LFactoryIndex := FactoryIndex(ANode);

  if LFactoryIndex >= 0 then
  begin
    for LIndex := 0 to ANode.BindingCount - 1 do
    begin

      if not ANode.Bindings[LIndex].Cleared and not Assigned(FUpdaters[LFactoryIndex]) then
      begin
        raise ENyxModel.Create('Bound custom browser factory requires an updater: ' + ANode.Kind);
      end;
    end;

    if Assigned(FEventFactories[LFactoryIndex]) then
    begin
      Result := FEventFactories[LFactoryIndex](ANode, FEmitterScope.ForControl(ANode.ID));
    end
    else
    begin
      Result := FFactories[LFactoryIndex](ANode);
    end;

    if (Result = nil) or (Result.parentNode <> nil) then
    begin
      raise ENyxModel.Create('Browser factory must return a new detached element: ' + ANode.Kind);
    end;

    if (Result is TJSHTMLInputElement) or (Result is TJSHTMLTextAreaElement) or
      (Result is TJSHTMLSelectElement) then
    begin
      AInput := Result;
    end;
    Exit;
  end;

  if not FindNyxPrimitive(LKind, LInfo) then
  begin
    raise ENyxModel.Create('No browser projection for ' + ANode.Kind + ' (base ' + LKind +
      '). Derive a primitive recipe or register a browser factory.');
  end;

  if not LInfo.Container and (ANode.Count > 0) then
  begin
    raise ENyxModel.Create('Compose children inside a layout host, not ' + ANode.ID);
  end;

  if LKind = 'code-editor' then
  begin
    Result := Element('textarea', 'nyx-code-editor');
    AInput := Result;
    TJSHTMLTextAreaElement(Result).value := ANode.Prop('value');
    TJSHTMLTextAreaElement(Result).spellcheck := False;
    Result.setAttribute('aria-label', ANode.Prop('text', 'Pascal source'));
  end
  else if (LKind = 'input') or (LKind = 'memo') or (LKind = 'select') or
    (LKind = 'spin') or (LKind = 'date') or (LKind = 'time') or (LKind = 'color') then
  begin
    Result := Element('label', 'nyx-field');

    if LKind = 'color' then
    begin
      { The group has two separately named inputs. A label cannot contain a
        second labelable control; explicit names retain both keyboard targets. }
      Result := Element('div', 'nyx-field');
    end;
    LCaption := Element('span', 'nyx-field-caption');
    LCaption.textContent := ANode.Prop('text');
    Result.appendChild(LCaption);

    if LKind = 'memo' then
    begin
      AInput := Element('textarea', 'nyx-memo-control');
      TJSHTMLTextAreaElement(AInput).value := ANode.Prop('value');
    end
    else if LKind = 'select' then
    begin
      AInput := Element('select', 'nyx-select-control');
      { SyncLiteralItems supplies the admitted initial rows once. }
    end
    else
    begin
      AInput := Element('input', 'nyx-input-control');
      LInput := TJSHTMLInputElement(AInput);
      LInput._type := 'text';

      if ANode.Prop('input-type') = 'password' then
      begin
        LInput._type := 'password';
      end;

      if ANode.Prop('input-type') = 'number' then
      begin
        LInput._type := 'number';
        LInput.setAttribute('step', 'any');
      end;

      if LKind = 'spin' then
      begin
        LInput._type := 'number';
      end;

      if (LKind = 'date') or (LKind = 'time') then
      begin
        LInput._type := LKind;
      end;
      LInput.value := ANode.Prop('value');
    end;
    AInput.setAttribute('placeholder', ANode.Prop('placeholder'));
    AInput.setAttribute('aria-label', ANode.Prop('text', ANode.ID));
    Result.appendChild(AInput);
  end
  else if (LKind = 'checkbox') or (LKind = 'switch') or (LKind = 'radio') then
  begin
    Result := Element('label', 'nyx-check');
    AInput := Element('input', '');
    LInput := TJSHTMLInputElement(AInput);
    LInput._type := 'checkbox';

    if LKind = 'radio' then
    begin
      LInput._type := 'radio';
      LInput.name := ANode.Prop('group', 'nyx-radio');
    end;

    if LKind = 'switch' then
    begin
      LInput.setAttribute('role', 'switch');
    end;
    LInput.checked := ANode.Prop('value') = 'true';
    Result.appendChild(AInput);
    LCaption := Element('span', '');
    LCaption.textContent := ANode.Prop('text');
    Result.appendChild(LCaption);
  end
  else if LKind = 'slider' then
  begin
    Result := Element('label', 'nyx-slider');
    AInput := Element('input', '');
    LInput := TJSHTMLInputElement(AInput);
    LInput._type := 'range';
    LInput.value := ANode.Prop('value', '0');
    LInput.setAttribute('aria-label', ANode.Prop('text', ANode.ID));
    Result.appendChild(AInput);
  end
  else if LKind = 'progress' then
  begin
    Result := Element('progress', '');
    TJSHTMLProgressElement(Result).max := StrToFloatDef(ANode.Prop('max'), 100);

    if ANode.Prop(NyxAttributeName(atIndeterminate), 'false') = 'false' then
    begin
      TJSHTMLProgressElement(Result).value := StrToFloatDef(ANode.Prop('value'), 0);
    end;
    Result.setAttribute('aria-label', ANode.Prop('text', 'Progress'));
  end
  else if LKind = 'drawing' then
  begin
    Result := Element('div', 'nyx-drawing');
  end
  else if LKind = 'image' then
  begin
    Result := Element('img', '');
    LURL := ANode.Prop('src');

    if not SafeURL(LURL, True) then
    begin
      raise ENyxModel.Create('Unsupported image URL');
    end;
    { Binding-owned request listeners are installed before Sync assigns src. }
    TJSHTMLImageElement(Result).alt := ANode.Prop('alt', ANode.Prop('text'));
    ApplyNyxBrowserImage(ANode, TJSHTMLImageElement(Result));
  end
  else if LKind = 'list' then
  begin
    Result := Element('ul', '');
  end
  else if LKind = 'table' then
  begin
    Result := Element('table', '');
  end
  else if LKind = 'tree' then
  begin
    { Literal Items are flat leaves; bound views supply actual hierarchy. }
    Result := Element('div', '');
  end
  else
  begin

    if LKind = 'button' then
    begin
      Result := Element('button', '');
      Result.setAttribute('type', 'button');
    end
    else if LKind = 'link' then
    begin
      Result := Element('a', '');
      LURL := ANode.Prop('href', '#');

      if not SafeURL(LURL, False) then
      begin
        raise ENyxModel.Create('Unsupported link URL');
      end;
      Result.setAttribute('href', LURL);
    end
    else if LKind = 'heading' then
    begin
      Result := Element('h2', '');
    end
    else if LKind = 'separator' then
    begin
      Result := Element('hr', '');
    end
    else if LKind = 'code' then
    begin
      Result := Element('pre', '');
    end
    else if LKind = 'group' then
    begin
      Result := Element('fieldset', '');
      LCaption := Element('legend', '');
      LCaption.textContent := ANode.Prop('text');
      Result.appendChild(LCaption);
    end
    else
    begin
      Result := Element('div', '');
    end;

    if (LKind = 'button') or (LKind = 'link') or (LKind = 'heading') or
      (LKind = 'label') or (LKind = 'badge') or (LKind = 'alert') or
      (LKind = 'avatar') or (LKind = 'code') then
    begin
      Result.textContent := ANode.Prop('text');
    end;
  end;
end;

function TNyxBrowserRenderer.Build(ANode: TNyxNode): TJSHTMLElement;
var
  LInput: TJSHTMLElement;
  LBinding: TNyxBrowserBinding;
  LIndex: Integer;
  LValue: TNyxText;
  LFirst: TJSHTMLElement;
  LSecond: TJSHTMLElement;
  LWheelOptions: TJSObject;
  LViewportElement: TJSHTMLElement;
begin
  Result := CreateElement(ANode, LInput);
  Result.className := Result.className + ' nyx-node nyx-' + ANode.Kind;

  if ANode.ProjectionKind <> ANode.Kind then
  begin
    Result.classList.add('nyx-' + ANode.ProjectionKind);
  end;

  if ANode.Prop('surface') = 'true' then
  begin
    Result.classList.add('nyx-card');
  end;
  Result.setAttribute('data-node', ANode.Prop('design-id', ANode.ID));
  Result.setAttribute('data-runtime-id', ANode.ID);
  Result.setAttribute('data-variant', ANode.Prop('variant'));

  if ANode.Prop('aria-label') <> '' then
  begin
    Result.setAttribute('aria-label', ANode.Prop('aria-label'));
  end;
  Result.title := ANode.Prop('hint');
  LValue := ANode.Prop('layout');

  if (LValue = 'row') or (LValue = 'column') then
  begin
    Result.style.setProperty('display', 'flex');
    Result.style.setProperty('flex-direction', LValue);
  end
  else if LValue = 'grid' then
  begin
    Result.style.setProperty('display', 'grid');
  end;

  if ANode.Prop('columns') <> '' then
  begin
    Result.style.setProperty('grid-template-columns', 'repeat(' +
      IntToStr(StrToIntDef(ANode.Prop('columns'), 2)) + ',minmax(0,1fr))');
  end;
  for LIndex := 0 to ANode.Props.Count - 1 do
  begin
    LValue := ANode.Props.Names[LIndex];

    if (LValue = 'width') or (LValue = 'height') or (LValue = 'gap') or
      (LValue = 'padding') or (LValue = 'left') or (LValue = 'top') then
    begin
      { Origins belong to the parent's absolute layout. Keep authored coordinates
        in the model for a later absolute transition, without turning them into
        relative CSS translations in a row, column or grid. }

      if ((LValue = 'left') or (LValue = 'top')) and
        ((ANode.Parent = nil) or (NyxLayout(ANode.Parent) <> 'absolute')) then
      begin
        Continue;
      end;

      if ANode.Prop(LValue) = '' then
      begin
        Continue;
      end;
      Result.style.setProperty(LValue, IntToStr(StrToIntDef(ANode.Prop(LValue), 0)) + 'px');
    end;
  end;

  if ANode.Prop('layout') = 'absolute' then
  begin
    Result.style.setProperty('display', 'block');
  end;

  if (ANode.Parent <> nil) and (NyxLayout(ANode.Parent) = 'absolute') then
  begin
    Result.style.setProperty('position', 'absolute');
  end;

  if ANode.Prop('flex') <> '' then
  begin

    if StrToIntDef(ANode.Prop('flex'), 0) > 0 then
    begin
      Result.style.setProperty('flex', ANode.Prop('flex'));
      Result.classList.add('nyx-flex');
    end
    else
    begin
      { CSS's bare zero also sets a zero basis. The Pascal zero-weight contract
        restores the authored/natural size instead of collapsing that item. }
      Result.style.setProperty('flex', '0 0 auto');
    end;
  end;

  if ANode.Prop('visible', 'true') = 'false' then
  begin
    Result.style.setProperty('display', 'none');
  end;

  if LInput <> nil then
  begin
    LInput.setAttribute('min', ANode.Prop('min', '0'));
    LInput.setAttribute('max', ANode.Prop('max', '100'));

    if ANode.Prop('enabled', 'true') = 'false' then
    begin
      LInput.setAttribute('disabled', '');
    end;

    if ANode.Prop('readonly') = 'true' then
    begin
      LInput.setAttribute('readonly', '');
    end;
  end
  else if (ANode.ProjectionKind = 'button') and (ANode.Prop('enabled', 'true') = 'false') then
  begin
    Result.setAttribute('disabled', '');
  end;
  LBinding := TNyxBrowserBinding.Create;
  LBinding.FGenerationObject := FGenerationObject;
  LBinding.FRenderer := Self;
  LBinding.FNode := ANode;
  LBinding.FElement := Result;
  LBinding.FInput := LInput;
  LBinding.FCustom := FactoryIndex(ANode) >= 0;

  if (ANode.ProjectionKind = 'color') and not LBinding.FCustom then
  begin
    LBinding.FColorField := TNyxBrowserColorField.Create(
      TJSHTMLInputElement(LInput), LBinding.ColorChanged);
  end;

  if LBinding.FCustom then
  begin
    LBinding.FUpdater := FUpdaters[FactoryIndex(ANode)];
  end;

  if not LBinding.FCustom then
  begin
    LBinding.FCaption := TJSHTMLElement(Result.querySelector('.nyx-field-caption'));

    if (ANode.ProjectionKind = 'checkbox') or (ANode.ProjectionKind = 'switch') or
      (ANode.ProjectionKind = 'radio') then
    begin
      LBinding.FCaption := TJSHTMLElement(Result.querySelector('span'));
    end;

    if ANode.ProjectionKind = 'group' then
    begin
      LBinding.FCaption := TJSHTMLElement(Result.querySelector('legend'));
    end;
  end;

  if not LBinding.FCustom and (ANode.ProjectionKind = 'radio') then
  begin
    Result.addEventListener('focus', @LBinding.ForwardRadioFocus);
  end;
  Result.onclick := @LBinding.Click;
  Result.addEventListener('dblclick', @LBinding.DoubleClick);
  Result.addEventListener('pointerdown', @LBinding.PointerDown);
  Result.addEventListener('pointerup', @LBinding.PointerUp);
  Result.addEventListener('pointermove', @LBinding.PointerMove);
  Result.addEventListener('pointerenter', @LBinding.PointerEnter);
  Result.addEventListener('pointerleave', @LBinding.PointerExit);
  Result.addEventListener('pointercancel', @LBinding.PointerCancel);
  Result.addEventListener('gotpointercapture', @LBinding.PointerCapture);
  Result.addEventListener('lostpointercapture', @LBinding.PointerCaptureLost);
  Result.addEventListener('dragstart', @LBinding.DragStart);
  Result.addEventListener('drag', @LBinding.Drag);
  Result.addEventListener('dragenter', @LBinding.DragEnter);
  Result.addEventListener('dragover', @LBinding.DragOver);
  Result.addEventListener('dragleave', @LBinding.DragExit);
  Result.addEventListener('drop', @LBinding.Drop);
  Result.addEventListener('dragend', @LBinding.DragEnd);
  Result.draggable := not FDesignMode and (ANode.Prop('drag-source') = 'true');
  Result.style.setProperty('touch-action', ANode.Prop('touch-behavior', 'auto'));
  Result.addEventListener('contextmenu', @LBinding.ContextMenu);
  { Wheel input must be non-passive when a sequential hook may cancel it.
    Only the outer face listens: input bubbling cannot duplicate a Nyx route. }
  LWheelOptions := TJSObject.new;
  LWheelOptions['passive'] := False;
  TNyxWheelElement(Result).Listen('wheel', @LBinding.Wheel, LWheelOptions);

  if NyxSupportsViewport(ANode) then
  begin
    LViewportElement := Result;

    if LInput <> nil then
    begin
      LViewportElement := LInput;
    end;
    LViewportElement.addEventListener('scroll', @LBinding.Scroll);
    LViewportElement.addEventListener('scrollend', @LBinding.ScrollEnd);

    if (LInput = nil) and (ANode.Prop('height') <> '') then
    begin
      Result.style.setProperty('overflow', 'auto');

      if ANode.ProjectionKind = 'table' then
      begin
        Result.style.setProperty('display', 'block');
      end;
    end;
  end;
  { Standard non-input focus policy is applied in Sync, including disabled
    transitions. Bound collection mounts retain their own roving Tab entry. }
  { Delegated focus observes collection rows/editors without adding a second
    Tab stop. Ordinary leaves still admit only their exact physical face.
    Capture keys before a collection's default row navigation so sequential
    Nyx hooks can consume it. Creator factories keep their existing ordering. }

  if (LInput = nil) and (LBinding.FCustom or
    (ANode.ProjectionKind <> 'split-view')) then
  begin
    Result.addEventListener('focusin', @LBinding.Enter);
    Result.addEventListener('focusout', @LBinding.Leave);
    Result.addEventListener('keydown', @LBinding.KeyDown, not LBinding.FCustom);
    Result.addEventListener('keyup', @LBinding.KeyUp, not LBinding.FCustom);
  end;

  if LInput <> nil then
  begin
    LInput.onchange := @LBinding.Change;
    { Text admission follows physical input on both adapters. This also covers
      paste, deletion, composition and virtual keyboards without keydown. Scalar
      number/choice controls retain their explicit editing-complete boundary. }

    if NyxSupportsTextInput(ANode) or (ANode.ProjectionKind = 'input') then
    begin
      { The input's scalar domain/format can change while it stays mounted.
        Install once; each producer checks the current typed domain before
        admission. Numeric drafts still wait for the physical change boundary. }
      LInput.addEventListener('input', @LBinding.Change);
      LInput.addEventListener('beforeinput', @LBinding.BeforeEdit);
      LInput.addEventListener('compositionstart', @LBinding.CompositionStart);
      LInput.addEventListener('compositionupdate', @LBinding.CompositionUpdate);
      LInput.addEventListener('compositionend', @LBinding.CompositionEnd);
      LInput.addEventListener('selectionchange', @LBinding.TextSelectionChanged);
      LInput.addEventListener('select', @LBinding.TextSelectionChanged);
      LBinding.FEditingSelection := CaptureNyxBrowserSelection(LInput);
    end;
    LInput.addEventListener('focus', @LBinding.Enter);
    LInput.addEventListener('blur', @LBinding.Leave);
    LInput.addEventListener('keydown', @LBinding.KeyDown);
    LInput.addEventListener('keyup', @LBinding.KeyUp);
  end;
  SetLength(FBindings, Length(FBindings) + 1);
  FBindings[Length(FBindings) - 1] := LBinding;

  if not LBinding.FCustom and (ANode.ProjectionKind = 'tabs') then
  begin
    LBinding.FTabs := TNyxBrowserTabHost.Create(ANode, Result);
    LBinding.FTabs.OnChange := @LBinding.TabsChanged;
    LBinding.FTabs.OnAdmit := @LBinding.AdmitTabs;
  end;
  for LIndex := 0 to ANode.Count - 1 do
  begin

    if not LBinding.FCustom and (ANode.ProjectionKind = 'tabs') then
    begin
      LBinding.FTabs.Attach(LIndex, Build(ANode.Children[LIndex]));
    end
    else
    begin
      Result.appendChild(Build(ANode.Children[LIndex]));
    end;
  end;

  if not LBinding.FCustom and (ANode.ProjectionKind = 'split-view') then
  begin
    LFirst := nil;
    LSecond := nil;

    if ANode.Count > 0 then
    begin
      LFirst := TJSHTMLElement(Result.children[0]);
    end;

    if ANode.Count > 1 then
    begin
      LSecond := TJSHTMLElement(Result.children[1]);
    end;
    LBinding.FSplit := TNyxBrowserSplit.Create(ANode, Result, LFirst, LSecond, FDesignMode);
    LBinding.FSplit.OnChanged := @LBinding.SplitChanged;
    LBinding.FSplit.Divider.addEventListener('focus', @LBinding.Enter);
    LBinding.FSplit.Divider.addEventListener('blur', @LBinding.Leave);
    LBinding.FSplit.Divider.addEventListener('keydown', @LBinding.KeyDown, True);
    LBinding.FSplit.Divider.addEventListener('keyup', @LBinding.KeyUp, True);
  end;
end;

function TNyxBrowserRenderer.CaptureContentFaces: TNyxContentFaceStates;
var
  LIndex: Integer;
  LBinding: TNyxBrowserBinding;
begin
  Result := nil;
  SetLength(Result, Length(FBindings));
  for LIndex := 0 to High(FBindings) do
  begin
    LBinding := FBindings[LIndex];
    Result[LIndex] := CaptureNyxContentFace(LBinding.FNode);
    Result[LIndex].Focused := document.activeElement = LBinding.FocusElement;
    Result[LIndex].ScrollLeft := LBinding.FElement.scrollLeft;
    Result[LIndex].ScrollTop := LBinding.FElement.scrollTop;

    if LBinding.FInput <> nil then
    begin
      Result[LIndex].Selection := CaptureNyxBrowserSelection(LBinding.FInput);
      Result[LIndex].InputScrollLeft := LBinding.FInput.scrollLeft;
      Result[LIndex].InputScrollTop := LBinding.FInput.scrollTop;
      Result[LIndex].HasText := (LBinding.FInput is TJSHTMLInputElement) or
        (LBinding.FInput is TJSHTMLTextAreaElement);

      if Result[LIndex].HasText then
      begin
        Result[LIndex].Text := NyxBrowserInputText(LBinding.FInput);
      end;
    end;
  end;
end;

procedure TNyxBrowserRenderer.RestoreContentFaces(
  const AStates: TNyxContentFaceStates; AFocus: Boolean);
var
  LGeneration: TBrowserViewGeneration;
  LGenerationLease: IInterface;
  LIndex: Integer;
  LSaved: Integer;
  LBinding: TNyxBrowserBinding;
  LUpdating: Boolean;

  procedure RequireCurrent;
  begin

    if not LGeneration.Current then
    begin
      raise ENyxBrowserViewRetired.Create('Browser input generation retired during restoration');
    end;
  end;

begin
  LGeneration := TBrowserViewGeneration(FGenerationObject);
  LGenerationLease := FGeneration;

  if (LGeneration = nil) or not LGeneration.Current then
  begin
    Exit;
  end;
  LUpdating := FUpdating;
  FUpdating := True;
  try
    for LIndex := 0 to High(FBindings) do
    begin
      RequireCurrent;
      LBinding := FBindings[LIndex];
      LSaved := NyxContentFaceIndex(LBinding.FNode, AStates);
      RequireCurrent;

      if LSaved < 0 then
      begin
        Continue;
      end;

      if AStates[LSaved].HasText and (LBinding.FInput <> nil) and
        (LBinding.FNode.Prop('value') = AStates[LSaved].Accepted) then
      begin

        if LBinding.FInput is TJSHTMLInputElement then
        begin
          TJSHTMLInputElement(LBinding.FInput).value := AStates[LSaved].Text;
          RequireCurrent;
        end
        else if LBinding.FInput is TJSHTMLTextAreaElement then
        begin
          TJSHTMLTextAreaElement(LBinding.FInput).value := AStates[LSaved].Text;
          RequireCurrent;
        end;
        LBinding.FLastValue := AStates[LSaved].Accepted;
        LBinding.FHasValueBaseline := True;
      end;

      if AFocus and AStates[LSaved].Focused and NyxInteractionPolicy(LBinding.FNode).Visible and
        NyxInteractionPolicy(LBinding.FNode).Enabled then
      begin
        NyxFocusWithoutScroll(LBinding.FocusElement);
        RequireCurrent;
      end;

      if (LBinding.FInput <> nil) and AStates[LSaved].Selection.Defined and
        CaptureNyxBrowserSelection(LBinding.FInput).Defined and
        (NyxBrowserInputText(LBinding.FInput) = AStates[LSaved].Text) then
      begin
        RequireCurrent;
        SelectNyxBrowserText(LBinding.FInput, AStates[LSaved].Selection);
        RequireCurrent;
      end;
      RequireCurrent;
      LBinding.FElement.scrollLeft := Round(AStates[LSaved].ScrollLeft);
      RequireCurrent;
      LBinding.FElement.scrollTop := Round(AStates[LSaved].ScrollTop);
      RequireCurrent;

      if LBinding.FInput <> nil then
      begin
        LBinding.FInput.scrollLeft := Round(AStates[LSaved].InputScrollLeft);
        RequireCurrent;
        LBinding.FInput.scrollTop := Round(AStates[LSaved].InputScrollTop);
        RequireCurrent;
      end;
    end;
  finally

    if LGeneration.Current then
    begin
      { A focus/setter callback may have cleared, remounted or destroyed this
        receiver. Its old generation lease protects data, never successor fields. }
      FUpdating := LUpdating;
    end;
  end;
  RequireCurrent;
end;

function TNyxBrowserRenderer.CaptureInteraction(
  out AStates: TNyxContentFaceStates): Boolean;
begin
  AStates := nil;
  Result := SectionPublicationReady;

  if Result then
  begin
    AStates := CaptureContentFaces;
  end;
end;

function TNyxBrowserRenderer.RestoreInteraction(
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

procedure TNyxBrowserRenderer.QueueContent;
var
  LGeneration: TBrowserViewGeneration;
  LGenerationLease: IInterface;
  LScheduler: INyxScheduler;
  LExecution: INyxExecution;
  LStatus: TNyxExecutionStatus;
  LFrame: TNyxViewFrame;
begin
  LGeneration := TBrowserViewGeneration(FGenerationObject);
  LGenerationLease := FGeneration;

  if (LGeneration = nil) or not LGeneration.Current then
  begin
    Exit;
  end;
  FViewportBinding.Observe;

  if not LGeneration.Current then
  begin
    Exit;
  end;
  FViewportBinding.Wake;

  if not LGeneration.Current then
  begin
    Exit;
  end;
  WakeResources;

  if not LGeneration.Current then
  begin
    Exit;
  end;

  if (FContentBlueprint = nil) or FDetachedCandidate or FPublishingContent then
  begin
    Exit;
  end;

  if FHost = nil then
  begin
    Exit;
  end;
  LFrame := FViewportBinding.Frame(FHost.clientWidth, FHost.clientHeight);

  if FContentRequestedSelection.Reference.Defined then
  begin
    LFrame := LFrame.Selecting(FContentRequestedSelection.Reference);
  end;

  if FContentFrameKnown and FContentFrame.Same(LFrame) and
    (FContentMeasurementKey = NyxContentMeasurements(FRoot, MeasureContainers)) then
  begin
    { A settled mount needs no redundant queued courier. A changed physical
      allocation or explicit request invalidates this exact observation. }
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
    FContentWork := NewNyxContentWork(@RefreshContent);
  end;
  LScheduler := FEvents.Scheduler;

  if not LGeneration.Current then
  begin
    Exit;
  end;
  LExecution := LScheduler.Submit(FContentWork, neUIQueue);

  if not LGeneration.Current then
  begin
    LExecution.Cancel;
    Exit;
  end;
  FContentExecution := LExecution;
end;

procedure TNyxBrowserRenderer.RefreshContent;
var
  LFrame: TNyxViewFrame;
  LMeasurements: INyxContainerSnapshot;
  LKey: TNyxText;
  LCandidate: TNyxNode;
  LStates: TNyxContentFaceStates;
  LIndex: Integer;
  LPreviousSelection: TNyxPresentationSelection;
  LLifetime: INyxContentLifetime;
begin
  LLifetime := FContentLifetime;
  FEvents.Scheduler.RequireUI;

  if FPublishingContent or FContentGuard.Busy then
  begin
    Exit;
  end;

  if (FContentBlueprint = nil) or (FHost = nil) or (FRoot = nil) then
  begin
    Exit;
  end;

  if FUpdating or ((FLiveBindings <> nil) and not FDesignMode and
    not FLiveBindings.RefreshReady) or ((FState <> nil) and FState.Busy) then
  begin
    FLastContentError := 'Structural publication requires an idle view and runtime store';
    Exit;
  end;
  { Do not retire host input while composition or pointer capture owns it.
    The producer's completion notification queues a fresh idle observation. }
  for LIndex := 0 to High(FBindings) do
  begin

    if FBindings[LIndex].FComposing or
      (Length(FBindings[LIndex].FCapturedPointers) > 0) then
    begin
      Exit;
    end;
  end;
  LFrame := FViewportBinding.Frame(FHost.clientWidth, FHost.clientHeight);

  if FContentRequestedSelection.Reference.Defined then
  begin
    LFrame := LFrame.Selecting(FContentRequestedSelection.Reference);
  end;
  LMeasurements := MeasureContainers;
  LKey := NyxContentMeasurements(FRoot, LMeasurements);

  if FContentFrameKnown and FContentFrame.Same(LFrame) and
    (FContentMeasurementKey = LKey) then
  begin
    Exit;
  end;
  LCandidate := nil;
  try
    try
      LCandidate := FContentBlueprint.Realize(LFrame, LMeasurements);

      if FResourceContext <> nil then
      begin
        LCandidate.BindResources(FResourceContext.Snapshot,
          FResourceContext.Locale, FResourceContext.Fallback);
      end;

      if not SameNyxContentStructure(FRoot, LCandidate) then
      begin
        LStates := CaptureContentFaces;
        FPublishingContent := True;
        try
          RenderFrame(FContentBlueprint.Document, FContentBlueprint.Root, FHost,
            FDesignMode, FState, FCollectionBindings, LFrame, LMeasurements, True, LStates,
            nrmConfigured, False, nil, nil, FViewPublicationSource);

          if not LLifetime.Active then
          begin
            Exit;
          end;
          LKey := FContentMeasurementKey;
        finally

          if LLifetime.Active then
          begin
            FPublishingContent := False;
            WakeResources;
          end;
        end;
      end
      else
      begin
        LPreviousSelection := FPresentationSelection;
        FPresentationSelection := LFrame.Selection;
        FPublishingContent := True;
        try
          try
            Sync;
          except

            if LLifetime.Active then
            begin
              FPresentationSelection := LPreviousSelection;
              Sync;
            end;
            raise;
          end;
        finally

          if LLifetime.Active then
          begin
            FPublishingContent := False;
            WakeResources;
          end;
        end;
      end;

      if not LLifetime.Active then
      begin
        Exit;
      end;
      FContentFrame := LFrame;
      FContentFrameKnown := True;
      { Changed structures retain the final hidden allocation signature. Scalar
        updates retain their consumed observation; later host changes reobserve. }
      FContentMeasurementKey := LKey;
      FLastContentError := '';
    except
      on LException: Exception do
      begin
        { Remember the exact failed observation to prevent an automatic retry
          storm. A new geometry or explicit manual request remains retryable. }

        if LLifetime.Active then
        begin
          FContentFrame := LFrame;
          FContentFrameKnown := True;
          FContentMeasurementKey := LKey;
          FLastContentError := TNyxText(LException.Message);
        end;
        raise;
      end;
    end;
  finally
    LCandidate.Free;
  end;

  if LLifetime.Active then
  begin
    FlushImages;
  end;
end;

procedure TNyxBrowserRenderer.Render(ADocument: TNyxDocument; ARoot: TNyxNode;
  AHost: TJSHTMLElement; ADesignMode: Boolean; AState: TNyxState;
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
    raise ENyxModel.Create('Browser host is required');
  end;

  if FPublishingContent then
  begin
    raise ENyxModel.Create('Browser structural publication is already staging');
  end;
  { Extension constructors can service the UI queue. Already queued work may
    observe, but cannot retire this accepted mount during candidate admission. }
  FPublishingContent := True;
  try
    RenderFrame(ADocument, ARoot, AHost, ADesignMode, AState, ACollections,
      FViewportBinding.Frame(AHost.clientWidth, AHost.clientHeight),
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

procedure TNyxBrowserRenderer.RenderContext(ADocument: TNyxDocument; ARoot: TNyxNode;
  AHost: TJSHTMLElement; ADesignMode: Boolean; AState: TNyxState;
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
      FViewportBinding.Frame(AHost.clientWidth, AHost.clientHeight), nil, False, nil,
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

procedure TNyxBrowserRenderer.RenderFrame(ADocument: TNyxDocument; ARoot: TNyxNode;
  AHost: TJSHTMLElement; ADesignMode: Boolean; AState: TNyxState;
  const ACollections: INyxCollectionBindings; const AFrame: TNyxViewFrame;
  const AMeasurements: INyxContainerSnapshot; AKeepPresentation: Boolean;
  const AStates: TNyxContentFaceStates; AResourceMode: TNyxResourceRenderMode;
  AUseContext: Boolean; const AContext: INyxApplicationContext;
  const AResolver: INyxLogicResolver;
  const APublicationSource: INyxViewPublicationSource);
var
  LCandidate: TNyxBrowserRenderer;
  LStyle: TJSHTMLElement;
  LIndex: Integer;
  LRuntimeState: TNyxState;
  LOwnRuntimeState: Boolean;
  LRuntimeStateOwner: INyxBindingOwnedState;
  LEnteredGeneration: IInterface;
  LRenderAttempt: IInterface;
  LReceiverLifetime: INyxContentLifetime;
  LCommittedRevision: Integer;
  LThemeOwner: IInterface;
  LBaseThemeOwner: IInterface;
  LCollections: INyxCollectionBindings;
  LCollectionInputGate: INyxCollectionInputGate;
  LCollectionAdmissions: array of INyxCollectionInputAdmission;
  LCollectionAdmission: INyxCollectionInputAdmission;
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
  LFormat: TFormatSettings;
  LHostStyle: TJSCSSStyleDeclaration;
  LProbeHost: TJSHTMLElement;
  LFormerChildren: TJSHTMLElement;
  LNewChildren: array of TJSNode;
  LFormerFocus: TJSHTMLElement;
  LFormerSelection: TNyxTextSelection;
  { Managed source facets remain leased until rollback or accepted Disconnect.
    Snapshots contain copied cell values and exact content/store identity only. }
  LCellEdits: array of TBrowserCellEditTransfer;
  LCellEditLeases: array of IInterface;
  LFormerCellFocus, LCellFocusAllowed: Boolean;
  LCellFocusAnchor: TJSElement;
  LReplacementCellFocus: TJSElement;
  LReplacementCellSource: INyxCollectionEditMount;
  LReplacementCellStore: INyxCollection;
  LReplacementCellDraft: TNyxCollectionCellEdit;
  LCellResumeError: TNyxText;
  LFormerClass: TNyxText;
  LFormerScope: TNyxText;
  LHadScope: Boolean;
  LUpdating: Boolean;
  LFormerTop: NativeInt;
  LFormerLeft: NativeInt;
  LInputTop: NativeInt;
  LInputLeft: NativeInt;
  LMeasurementKey: TNyxText;
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

  function AttemptCurrent: Boolean;
  begin
    { The retained lifetime is local and noncalling. Never inspect a destroyed
      receiver merely to discover that its attempt has already retired. }
    Result := False;

    if (LReceiverLifetime = nil) or not LReceiverLifetime.Active then
    begin
      Exit;
    end;
    Result := FRenderAttempt = LRenderAttempt;
  end;

  procedure CheckAttempt;
  begin

    if not AttemptCurrent then
    begin
      raise ENyxBrowserViewRetired.Create('The browser render attempt was retired');
    end;
  end;

  procedure SuspendCellDrafts;
  var
    LBindingIndex, LPrevious, LSlot, LCell: Integer;
    LMount: INyxCollectionMount;
    LEdits: INyxCollectionEditMount;
    LIdentity: TNyxContentIdentity;
    LTransfer: TBrowserCellEditTransfer;
    LTransferLease: IInterface;
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
        raise ENyxModel.Create('Browser table has no cell edit handoff');
      end;
      LIdentity := NyxContentIdentity(FBindings[LBindingIndex].FNode);
      for LPrevious := 0 to High(LCellEdits) do
      begin

        if LCellEdits[LPrevious].Identity.Same(LIdentity) then
        begin
          raise ENyxModel.Create('Browser cell edit handoff has an ambiguous content identity');
        end;
      end;
      LSlot := Length(LCellEdits);
      { Grow ownership storage first. Even allocation refusal cannot leave an
        unowned or nil transfer inside the rollback traversal's typed array. }
      SetLength(LCellEditLeases, LSlot + 1);
      LTransfer := TBrowserCellEditTransfer.Create;
      LTransferLease := LTransfer;
      SetLength(LCellEdits, LSlot + 1);
      LCellEdits[LSlot] := LTransfer;
      LCellEditLeases[LSlot] := LTransferLease;
      LCellEdits[LSlot].Identity := LIdentity;
      LCellEdits[LSlot].Source := LEdits;
      LCellEdits[LSlot].Snapshot := LEdits.SuspendCellEdits;
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
    LCellFocusAnchor := document.activeElement;
  end;

  procedure RestoreCandidateCellDrafts;
  var
    LSlot, LBindingIndex, LFound: Integer;
    LMount: INyxCollectionMount;
    LEdits: INyxCollectionEditMount;
    LIdentity: TNyxContentIdentity;
  begin
    for LSlot := 0 to High(LCellEdits) do
    begin
      LFound := -1;
      for LBindingIndex := 0 to High(LCandidate.FBindings) do
      begin
        LIdentity := NyxContentIdentity(LCandidate.FBindings[LBindingIndex].FNode);

        if not LIdentity.Same(LCellEdits[LSlot].Identity) then
        begin
          Continue;
        end;

        if LFound >= 0 then
        begin
          raise ENyxModel.Create('Browser replacement repeats a cell edit identity');
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
          LReplacementCellFocus := document.activeElement;
          LReplacementCellSource := LEdits;
          LReplacementCellStore := LCellEdits[LSlot].Store;
          LReplacementCellDraft := LCellEdits[LSlot].Focused;
        end;
      end;
    end;
  end;

  function CellRollbackFocusAllowed: Boolean;
  var
    LFocus: TJSElement;
    LSlot: Integer;
  begin
    LFocus := document.activeElement;
    Result := (LFocus = nil) or (LFocus = document.body);

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
            LCellResumeError := E.Message;
          end;
        end;
      end;
    end;

    if LCellResumeError <> '' then
    begin
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
      LCleanupError := AError.Message;
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
      notification gets an attempt. The first diagnostic describes committed
      data, never rollback; later failures cannot skip a required retirement. }

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
  { Build the entire candidate offscreen, then preview fallible physical work in
    the actual host. Model admission alone is insufficient. Transfer ownership
    only after target synchronization, observation and face restoration succeed. }
  LReceiverLifetime := FContentLifetime;
  LRenderAttempt := TInterfacedObject.Create;
  FRenderAttempt := LRenderAttempt;

  if AHost = nil then
  begin
    raise ENyxModel.Create('Browser host is required');
  end;

  if (FHost <> nil) and (FHost <> AHost) and FHost.contains(AHost) then
  begin
    raise ENyxModel.Create('A browser view cannot replace itself inside its own descendant');
  end;
  LCandidate := nil;
  LMeasuredRoot := nil;
  LSettlement := nil;
  LBlueprint := nil;
  LEventView := nil;
  LEventFence := nil;
  { Incoming arguments may alias the retained field during implicit remount.
    Snapshot both owners before Clear or any external preparation can run. }
  LSource := APublicationSource;
  LFormerSource := FViewPublicationSource;
  LEnteredGeneration := FGeneration;
  LThemeOwner := FThemeOwner;
  LBaseThemeOwner := FBaseThemeOwner;
  { Capture the actual owned holder before alternate registry/source callouts.
    A borrowed store with the same raw address cannot manufacture ownership. }
  LRuntimeStateOwner := FStateOwner;

  if (AState = nil) or (LRuntimeStateOwner = nil) or
    (LRuntimeStateOwner.State <> AState) then
  begin
    LRuntimeStateOwner := nil;
  end;
  LRootPublication := nil;
  LRootFence := nil;
  LCleanupFailed := False;
  LCleanupError := '';
  LCellEdits := nil;
  LCellEditLeases := nil;
  LCollectionAdmissions := nil;
  LCollectionInputGate := nil;
  LFormerCellFocus := False;
  LCellFocusAllowed := False;
  LCellFocusAnchor := nil;
  LReplacementCellFocus := nil;
  LReplacementCellSource := nil;
  LReplacementCellStore := nil;
  LReplacementCellDraft := Default(TNyxCollectionCellEdit);
  LCellResumeError := '';
  LFormerCoordinators := nil;
  LAcceptedCoordinators := nil;
  { Capture once before any factory callout, including hidden settling passes.
    A registration made by a creator belongs to a later Render operation. }
  LCoordinatorRegistry := FCoordinatorRegistry;
  LCallerCoordinatorSnapshot := FInheritedCoordinatorSnapshot;
  LFormerCallerCoordinatorSnapshot := FCallerCoordinatorSnapshot;
  LCoordinatorTarget := FCoordinatorRefreshTarget;
  LCoordinatorSnapshot := nil;

  LCommitted := False;
  LRuntimeState := AState;
  LOwnRuntimeState := AState = nil;
  LCollections := ACollections;
  LMeasurements := AMeasurements;
  LFrame := AFrame;
  LStates := Copy(AStates, 0, Length(AStates));
  LContext := AContext;
  LResolver := AResolver;
  LFirstCandidate := True;
  LFormat := FormatSettings;
  LFormat.DecimalSeparator := '.';

  if LCoordinatorRegistry <> nil then
  begin
    { Normalize alternate snapshots before keeping caller-only membership for
      later embedded mounts. No mutable registry survives through this field. }
    LCallerCoordinatorSnapshot := ComposeNyxCoordinatorSnapshots(nil,
      LCoordinatorRegistry.Snapshot);
    CheckAttempt;
  end;
  { Built-ins and caller behavior share one immutable membership and prepared
    lifetime. Authored applications keep their automatic registrations. }
  LBuiltInCoordinators := NewNyxCoordinatorRegistry;
  LBuiltInCoordinators.RegisterFactory(NyxCustomKind(NyxKindName(nkMasterDetail)),
    NyxExtension(NyxMasterDetailIntentKey), NewNyxMasterDetailCoordinatorFactory);
  LBuiltInCoordinators.RegisterFactory(NyxCustomKind(NyxKindName(nkKanbanBoard)),
    NyxExtension(NyxKanbanIntentKey), NewNyxKanbanCoordinatorFactory);
  LBuiltInCoordinators.RegisterFactory(NyxCustomKind(NyxKindName(nkDataToolbar)),
    NyxExtension(NyxDataToolbarIntentKey), NewNyxDataToolbarCoordinatorFactory);
  LCoordinatorSnapshot := ComposeNyxCoordinatorSnapshots(
    LBuiltInCoordinators.Snapshot, LCallerCoordinatorSnapshot);
  try
  try

    if LOwnRuntimeState then
    begin
      LRuntimeState := ADocument.State.Clone;
      LRuntimeStateOwner := OwnNyxBindingState(LRuntimeState);
      LOwnRuntimeState := False;
    end;
    LBlueprint := NewNyxContentBlueprint(ADocument, ARoot);

    if LBlueprint <> nil then
    begin
      LSettlement := TNyxContentSettlement.Create;
    end;
    repeat
      CheckAttempt;
      LCandidate := TNyxBrowserRenderer.Create(FTheme);
      LCandidate.PresentationViewport := PresentationViewport;
      LCandidate.FViewportExported := FViewportExported;
      LCollectionAdmissions := nil;
      LCollectionInputGate := NewNyxCollectionInputGate(FInheritedCollectionInputGate);
      LCandidate.FCollectionInputGate := LCollectionInputGate;
      LCandidate.FCollectionInputPhase := ncipStaged;
      LCandidate.FTableWorkbenchServices := FTableWorkbenchServices;
      LCandidate.FTableTextLimits := FTableTextLimits;
      { Build may mount saved canvases before this outer candidate commits. }
      LCandidate.FCallerCoordinatorSnapshot := LCallerCoordinatorSnapshot;

      if AResourceMode = nrmConfigured then
      begin
        LCandidate.FResourceContext := FResourceContext;
        { Retain the same update port during staging. Nested saved canvases
          subscribe before their parent adopts the candidate's target faces. }
        LCandidate.FResourceUpdates := FResourceUpdates;
      end;
      LCandidate.FPublishingContent := True;
      LCandidate.FDetachedCandidate := True;
      LCandidate.FStagingMeasurements := LMeasurements;

      if FBorrowedTheme <> nil then
      begin
        LCandidate.FTheme := NewNyxDocumentTheme(ADocument, FBorrowedTheme);
      end
      else
      begin
        LCandidate.FTheme := NewNyxDocumentTheme(ADocument, FBaseTheme);
      end;
      LCandidate.FOwnTheme := True;
      LCandidate.FThemeOwner := TBrowserThemeOwner.Create(LCandidate.FTheme);
      LCandidate.FOwnTheme := False;
      TBrowserViewGeneration(LCandidate.FGenerationObject).ThemeOwner := LCandidate.FThemeOwner;
      LCandidate.FFactoryKinds := Copy(FFactoryKinds, 0, Length(FFactoryKinds));
      LCandidate.FDrawingNames := Copy(FDrawingNames, 0, Length(FDrawingNames));
      LCandidate.FDrawingProviders := Copy(FDrawingProviders, 0, Length(FDrawingProviders));
      LCandidate.FEmbeddedContext := FEmbeddedContext;
      LCandidate.FEmbeddedResolver := FEmbeddedResolver;

      if AUseContext then
      begin
        LCandidate.FEmbeddedContext := LContext;
        LCandidate.FEmbeddedResolver := LResolver;
      end;
      LCandidate.FCanvasAdoptionHost := FCanvasAdoptionHost;
      LCandidate.FFactories := Copy(FFactories, 0, Length(FFactories));
      LCandidate.FEventFactories := Copy(FEventFactories, 0, Length(FEventFactories));
      { Use the final owner's scheduler, never the candidate's temporary router. }
      LCandidate.FEmitterScope := NewNyxEventEmitterScope(FEvents.Scheduler);
      LCandidate.FUpdaters := Copy(FUpdaters, 0, Length(FUpdaters));

      if AKeepPresentation then
      begin
        LCandidate.FProjectionContext := FProjectionContext;
      end
      else
      begin
        LCandidate.FProjectionContext := NyxProjectionContext(ADocument);
      end;
      LCandidate.FProjectionSchemaRevision := NyxSchemaRevision;
      LCandidate.FPresentationSelection := LFrame.Selection;
      LCandidate.FRoot := RealizeNyxView(ADocument, ARoot, LFrame, LMeasurements);

      if (AResourceMode = nrmConfigured) and (FResourceContext <> nil) then
      begin
        LCandidate.FRoot.BindResources(FResourceContext.Snapshot,
          FResourceContext.Locale, FResourceContext.Fallback);
      end;
      ApplyNyxPlatform(LCandidate.FRoot, npfBrowser);
      LCandidate.FViewportWidth := AHost.clientWidth;
      LCandidate.FViewportHeight := AHost.clientHeight;
      LCandidate.FRoot.ApplyViewport(LFrame.ViewportWidth, LFrame.ViewportHeight,
        npfBrowser, LFrame.Selection, LMeasurements);
      { Keep resolved viewport overlays for structural settlement while stored
        properties remain the independent authored refresh baseline. }
      LCandidate.FProjectionBaseline := LCandidate.FRoot.CloneRuntimeProjection;
      RestoreNyxContentValues(LCandidate.FRoot, LStates);
      LCandidate.FCollectionBindings := LCollections;

      if LCollections <> nil then
      begin

        if AKeepPresentation or not LFirstCandidate then
        begin
          LCandidate.FCollectionBindings := LCollections.Recompose(LCandidate.FRoot);
        end
        else
        begin
          LCollections.ValidateRoot(LCandidate.FRoot);
        end;
      end
      else
      begin
        LCandidate.FCollectionBindings := NewNyxCollectionBindings(LCandidate.FRoot,
          NewNyxCollectionContext(ADocument.Collections, LCandidate.FRoot.ResourceContext));
      end;

      if ((not ADesignMode) and
        NyxHasCoordinatorMounts(LCandidate.FRoot, LCoordinatorSnapshot)) or
        (Length(FCoordinatorMounts) <> 0) or (LSource <> nil) or
        (LFormerSource <> nil) then
      begin

        if not Supports(FEvents, INyxEventViewPreparation, LEventPreparation) then
        begin
          raise ENyxModel.Create('Browser view publication requires prepared event views');
        end;
        LEventView := LEventPreparation.PrepareView;

        if not Supports(LEventView, INyxPublicationReadiness, LEventFence) then
        begin
          raise ENyxModel.Create('Browser event view has no final readiness fence');
        end;

        if not ADesignMode then
        begin
          PrepareNyxCoordinatorMounts(LCandidate.FRoot, LCandidate.FCollectionBindings,
            LEventView, FEvents.Scheduler, LCoordinatorSnapshot, LCoordinatorTarget,
            LCandidate.FCoordinatorMounts, LCandidate.FCoordinatorIDs);
        end;
      end;

      if LSource <> nil then
      begin
        LRootPublication := LSource.Prepare(ADocument, LCandidate.FRoot, LEventView);
        CheckAttempt;

        if (LRootPublication = nil) or
          not Supports(LRootPublication, INyxPublicationReadiness, LRootFence) then
        begin
          raise ENyxModel.Create('Browser root publication requires a participant and final readiness fence');
        end;
      end;
      { One independent store/context serves every hidden pass. Failed passes
        release only their views/validators, never reimporting application defaults. }
      LCandidate.FState := LRuntimeState;
      LCandidate.FStateOwner := LRuntimeStateOwner;
      LCandidate.FOwnState := False;
      LCandidate.FLiveBindings := TNyxLiveBindings.Create(LCandidate.FRoot, LCandidate.FState);
      TBrowserViewGeneration(LCandidate.FGenerationObject).LiveBindings := LCandidate.FLiveBindings;
      LCandidate.FLiveBindings.OnSync := @LCandidate.Sync;
      LCandidate.FLiveBindings.OnValueSync := @LCandidate.SyncEditedValue;
      LCandidate.FLiveBindings.OnInputCapture := @LCandidate.CaptureInputTargets;
      LCandidate.FLiveBindings.OnInputSync := @LCandidate.SyncInputTargets;
      LCandidate.FHost := Element('div', '');
      LCandidate.FHost.className := AHost.className;
      LCandidate.FHost.style.cssText := AHost.style.cssText;
      LCandidate.FHost.classList.add('nyx-root');
      LCandidate.FHost.setAttribute('data-nyx-theme', LCandidate.FThemeScope);

      if ADesignMode then
      begin
        LCandidate.FHost.classList.add('nyx-design');
      end
      else
      begin
        LCandidate.FHost.classList.remove('nyx-design');
      end;
      LCandidate.FDesignMode := ADesignMode;
      LCandidate.FDesignerInput := FDesignerInput;
      LCandidate.FOnDesignerGesture := FOnDesignerGesture;
      LStyle := Element('style', '');
      LStyle.textContent := LCandidate.FTheme.CSS('.nyx-root[data-nyx-theme="' + LCandidate.FThemeScope + '"]');
      LCandidate.FHost.appendChild(LStyle);
      LCandidate.FHost.appendChild(LCandidate.Build(LCandidate.FRoot));
      for LIndex := 0 to LCandidate.FCollectionBindings.Count - 1 do
      begin
        LCandidate.FCollectionQueryEvents := FEvents;
        LCollectionMount := LCandidate.BindCollection(LCandidate.FCollectionBindings.ID(LIndex),
          LCandidate.FCollectionBindings.View(LIndex));
        LCollectionAdmission := nil;

        if Supports(LCollectionMount, INyxCollectionInputAdmission, LCollectionAdmission) then
        begin
          SetLength(LCollectionAdmissions, Length(LCollectionAdmissions) + 1);
          LCollectionAdmissions[High(LCollectionAdmissions)] := LCollectionAdmission;
        end;
      end;
      LCandidate.BindCollectionPagers;
      LCandidate.Sync;
      CheckAttempt;
      LCandidate.MountCanvases(ADocument);
      LCandidate.MountToolCanvases(ADocument);
      LCandidate.RestoreContentFaces(LStates, False);

      if LBlueprint = nil then
      begin
        Break;
      end;
      { An inert hidden sibling inherits the host's ancestry/classes and exact
        client frame without changing accepted children. Its unique theme scope
        prevents candidate tokens from recoloring the accepted view. Disconnected
        or display:none hosts yield missing boxes instead of invented allocation. }
      LCandidate.FHost.setAttribute('inert', '');
      LCandidate.FHost.setAttribute('aria-hidden', 'true');
      LCandidate.FHost.style.setProperty('position', 'fixed');
      LCandidate.FHost.style.setProperty('left', '-100000px');
      LCandidate.FHost.style.setProperty('top', '0');
      LCandidate.FHost.style.setProperty('visibility', 'hidden');
      LCandidate.FHost.style.setProperty('pointer-events', 'none');
      LCandidate.FHost.style.setProperty('box-sizing', 'border-box');
      { clientWidth/Height already exclude the host's border and scrollbar.
        Preserve its padding and inner layout, but use that exact client frame
        independently of external sizing constraints on the original host. }
      LHostStyle := window.getComputedStyle(AHost);
      LCandidate.FHost.style.setProperty('padding-top', LHostStyle.getPropertyValue('padding-top'));
      LCandidate.FHost.style.setProperty('padding-right', LHostStyle.getPropertyValue('padding-right'));
      LCandidate.FHost.style.setProperty('padding-bottom', LHostStyle.getPropertyValue('padding-bottom'));
      LCandidate.FHost.style.setProperty('padding-left', LHostStyle.getPropertyValue('padding-left'));
      LCandidate.FHost.style.setProperty('border', '0');
      LCandidate.FHost.style.setProperty('min-width', '0');
      LCandidate.FHost.style.setProperty('min-height', '0');
      LCandidate.FHost.style.setProperty('max-width', 'none');
      LCandidate.FHost.style.setProperty('max-height', 'none');
      LCandidate.FHost.style.setProperty('width', FloatToStr(LFrame.Width, LFormat) + 'px');
      LCandidate.FHost.style.setProperty('height', FloatToStr(LFrame.Height, LFormat) + 'px');

      if (AHost.parentNode <> nil) and document.body.contains(AHost) and
        (Length(AHost.getClientRects) > 0) then
      begin
        AHost.parentNode.insertBefore(LCandidate.FHost, AHost);
      end;
      LAllocated := LCandidate.MeasureAllocatedContainers;
      LMeasuredRoot := RealizeNyxView(ADocument, ARoot, LFrame, LAllocated);

      if (AResourceMode = nrmConfigured) and (FResourceContext <> nil) then
      begin
        LMeasuredRoot.BindResources(FResourceContext.Snapshot,
          FResourceContext.Locale, FResourceContext.Fallback);
      end;
      ApplyNyxPlatform(LMeasuredRoot, npfBrowser);
      LMeasuredRoot.ApplyViewport(LFrame.ViewportWidth, LFrame.ViewportHeight,
        npfBrowser, LFrame.Selection, LAllocated);

      if LSettlement.Accept(LCandidate.FProjectionBaseline, LMeasuredRoot) then
      begin
        ReleaseNyxNode(LMeasuredRoot);
        LMeasurements := LAllocated;
        for LIndex := 0 to High(LCandidate.FBindings) do
        begin

          if LCandidate.FBindings[LIndex].FNode.QueryContainer.Defined then
          begin
            LCandidate.FBindings[LIndex].FContainerMeasured := LAllocated.TrySize(
              LCandidate.FBindings[LIndex].FNode.ID,
              LCandidate.FBindings[LIndex].FContainerWidth,
              LCandidate.FBindings[LIndex].FContainerHeight);
          end;
        end;
        Break;
      end;
      ReleaseNyxNode(LMeasuredRoot);
      LCollections := LCandidate.FCollectionBindings;
      LMeasurements := LAllocated;
      LFirstCandidate := False;

      RetirePreparation;
      FreeAndNil(LCandidate);
    until False;
    { Retain one authored copy for the whole admission, not one per hidden pass. }
    LCandidate.FContentBlueprint := LBlueprint;
    LBlueprint := nil;

    if not ADesignMode then
    begin
      LCandidate.FLiveBindings.Activate;
    end;
    { Keep the accepted model, router revision and controls until the actual
      host accepts the candidate. Moving the exact old DOM objects to a private
      holder allows rollback without reconstructing controls or event handlers.
      A different target host retains its unrelated children on both paths. }
    LProbeHost := LCandidate.FHost;
    LFormerChildren := Element('div', '');
    LFormerClass := AHost.className;
    LHadScope := AHost.hasAttribute('data-nyx-theme');
    LFormerScope := AHost.getAttribute('data-nyx-theme');
    LFormerTop := AHost.scrollTop;
    LFormerLeft := AHost.scrollLeft;
    LFormerFocus := TJSHTMLElement(document.activeElement);
    LFormerSelection := Default(TNyxTextSelection);
    LInputTop := 0;
    LInputLeft := 0;

    if LFormerFocus <> nil then
    begin
      LFormerSelection := CaptureNyxBrowserSelection(LFormerFocus);
      LInputTop := LFormerFocus.scrollTop;
      LInputLeft := LFormerFocus.scrollLeft;
    end;
    SetLength(LNewChildren, LProbeHost.childNodes.length);
    for LIndex := 0 to High(LNewChildren) do
    begin
      LNewChildren[LIndex] := LProbeHost.childNodes[LIndex];
    end;
    LUpdating := FUpdating;
    FUpdating := True;
    LCandidate.FUpdating := True;
    try
      { Capture at the final reversible boundary, after all arbitrary Build
        callbacks. Mount suppression covers focusout/change caused by detaching
        the exact accepted DOM objects, including dormant offscreen drafts. }
      SuspendCellDrafts;
      CheckAttempt;

      if FHost = AHost then
      begin
        while AHost.firstChild <> nil do
        begin
          LFormerChildren.appendChild(AHost.firstChild);
        end;
      end;
      for LIndex := 0 to High(LNewChildren) do
      begin
        AHost.appendChild(LNewChildren[LIndex]);
      end;
      AHost.classList.add('nyx-root');
      AHost.setAttribute('data-nyx-theme', LCandidate.FThemeScope);

      if ADesignMode then
      begin
        AHost.classList.add('nyx-design');
      end
      else
      begin
        AHost.classList.remove('nyx-design');
      end;
      { Keep the staged projection frozen during physical synchronization. The
        next live allocation observation is queued only after admission. }
      LCandidate.FHost := AHost;
      LCandidate.FUpdating := False;
      LCandidate.Sync;
      CheckAttempt;
      { Nested workspaces first construct in an inert candidate. Refresh their
        actual final host allocations before accepting the parent publication;
        a lazy item's factory can still fail and must preserve the old view. }
      for LIndex := 0 to High(LCandidate.FBindings) do
      begin

        if LCandidate.FBindings[LIndex].FCanvasView <> nil then
        begin
          LCandidate.FBindings[LIndex].FCanvasView.Refresh;
          CheckAttempt;
        end;
      end;
      LCandidate.FUpdating := True;
      LCandidate.ObserveViewport(Self);
      LCandidate.RestoreContentFaces(LStates, True);
      CheckAttempt;
      LCellFocusAllowed := LFormerCellFocus and CellRollbackFocusAllowed;
      RestoreCandidateCellDrafts;
      CheckAttempt;

      if AKeepPresentation then
      begin
        AHost.scrollTop := LFormerTop;
        AHost.scrollLeft := LFormerLeft;
        LCandidate.Select(FSelectedDesignID);
      end;
      LCandidate.FEmitterScope.Activate(@EmitNamed);
      LMeasurementKey := NyxContentMeasurements(LCandidate.FRoot, LMeasurements);

      if LEventView <> nil then
      begin
        ValidateNyxCoordinatorMounts(LCandidate.FCoordinatorMounts);

        if LRootPublication <> nil then
        begin
          LRootPublication.Validate;
        end;
        LEventView.Validate;
        CheckNyxCoordinatorMounts(LCandidate.FCoordinatorMounts);

        if LRootFence <> nil then
        begin
          LRootFence.CheckReady;
        end;
        LEventFence.CheckReady;
      end;
      CheckAttempt;

      if not LCollectionInputGate.Current then
      begin
        raise ENyxModel.Create('Browser collection input generation retired during preview');
      end;
      FViewportBinding.CheckFrame(LFrame);
      CheckAttempt;
    except
      { Disconnect the new observer before restoring exact accepted DOM objects.
        Only candidate nodes are removed from a different borrowed host. }

      if LCandidate.FViewportObserver <> nil then
      begin
        LCandidate.FViewportObserver.disconnect;
        LCandidate.FViewportObserver := nil;
      end;
      LCandidate.FUpdating := True;
      LCandidate.FHost := LProbeHost;

      if not AttemptCurrent then
      begin
        raise;
      end;
      LCellFocusAllowed := CellRollbackFocusAllowed;

      if not AttemptCurrent then
      begin
        raise;
      end;
      try
        for LIndex := 0 to High(LNewChildren) do
        begin
          LProbeHost.appendChild(LNewChildren[LIndex]);
        end;
        while LFormerChildren.firstChild <> nil do
        begin
          AHost.appendChild(LFormerChildren.firstChild);
        end;
        AHost.className := LFormerClass;

        if LHadScope then
        begin
          AHost.setAttribute('data-nyx-theme', LFormerScope);
        end
        else
        begin
          AHost.removeAttribute('data-nyx-theme');
        end;

        if not LFormerCellFocus and (LFormerFocus <> nil) and
          document.body.contains(LFormerFocus) then
        begin
          NyxFocusWithoutScroll(LFormerFocus);

          if LFormerSelection.Defined then
          begin
            SelectNyxBrowserText(LFormerFocus, LFormerSelection);
          end;
          LFormerFocus.scrollTop := LInputTop;
          LFormerFocus.scrollLeft := LInputLeft;
        end;
        AHost.scrollTop := LFormerTop;
        AHost.scrollLeft := LFormerLeft;
      finally
        try
          ResumeFormerCellDrafts;
        finally

          if AttemptCurrent then
          begin
            FUpdating := LUpdating;
          end;
        end;
      end;
      raise;
    end;
    { The private renderer resumes ownership of its empty probe. Retirement of
      the old view clears the holder, never the newly attached accepted nodes. }
    LCandidate.FHost := LProbeHost;

    if FHost = AHost then
    begin
      FHost := LFormerChildren;
    end;
    { Passing this renderer's own State preserves that ownership across a full
      remount. Until admission the candidate only borrows it, so failure cannot
      release the still-mounted store. Clear must not free it during transfer. }

    LFormerCoordinators := FCoordinatorMounts;
    Clear(AKeepPresentation, LEventView);
    CheckAttempt;
    LCommittedRevision := FEvents.ViewRevision;
    CheckAttempt;
    { Transfer the independently admitted effective palette with its controls.
      Failure before this point preserves the mounted palette and tree. }

    FTheme := LCandidate.FTheme;
    FThemeOwner := LCandidate.FThemeOwner;
    LCandidate.FThemeOwner := nil;
    FOwnTheme := False;
    LCandidate.FTheme := nil;
    LCandidate.FOwnTheme := False;
    FState := LCandidate.FState;
    FStateOwner := LCandidate.FStateOwner;
    LCandidate.FStateOwner := nil;
    FEmbeddedContext := LCandidate.FEmbeddedContext;
    FEmbeddedResolver := LCandidate.FEmbeddedResolver;
    FOwnState := False;
    LOwnRuntimeState := False;
    LCandidate.FState := nil;
    LCandidate.FOwnState := False;
    FLiveBindings := LCandidate.FLiveBindings;
    LCandidate.FLiveBindings := nil;
    FLiveBindings.OnSync := @Sync;
    FLiveBindings.OnValueSync := @SyncEditedValue;
    FLiveBindings.OnInputCapture := @CaptureInputTargets;
    FLiveBindings.OnInputSync := @SyncInputTargets;
    FCollectionBindings := LCandidate.FCollectionBindings;
    LCandidate.FCollectionBindings := nil;
    FCollectionInputGate := LCollectionInputGate;
    LCandidate.FCollectionInputGate := nil;
    FCallerCoordinatorSnapshot := LCandidate.FCallerCoordinatorSnapshot;
    LCandidate.FCallerCoordinatorSnapshot := nil;
    FCoordinatorMounts := LCandidate.FCoordinatorMounts;
    FCoordinatorIDs := LCandidate.FCoordinatorIDs;
    LCandidate.FCoordinatorMounts := nil;
    LCandidate.FCoordinatorIDs := nil;
    FLastBindingError := '';
    FLastBindingFailure := nbfNone;
    FRoot := LCandidate.FRoot;
    FResourceContext := LCandidate.FResourceContext;
    LCandidate.FRoot := nil;
    FContentBlueprint := LCandidate.FContentBlueprint;
    LCandidate.FContentBlueprint := nil;
    FPresentationSelection := LFrame.Selection;
    FContentRequestedSelection := LFrame.Selection;
    FContentFrame := LFrame;
    FContentFrameKnown := True;
    FContentMeasurementKey := LMeasurementKey;
    FBindings := LCandidate.FBindings;
    FEmitterScope := LCandidate.FEmitterScope;
    LCandidate.FEmitterScope := nil;
    FViewportObserver := LCandidate.FViewportObserver;
    LCandidate.FViewportObserver := nil;
    { pas2js arrays share JavaScript identity after assignment. Detach the
      candidate reference; resizing it would also erase the accepted bindings. }
    LCandidate.FBindings := nil;
    FHost := AHost;
    FViewportWidth := LCandidate.FViewportWidth;
    FViewportHeight := LCandidate.FViewportHeight;
    FDesignMode := ADesignMode;
    FSelectedDesignID := LCandidate.FSelectedDesignID;
    FProjectionContext := LCandidate.FProjectionContext;
    FProjectionSchemaRevision := LCandidate.FProjectionSchemaRevision;
    FProjectionBaseline := LCandidate.FProjectionBaseline;
    LCandidate.FProjectionBaseline := nil;
    FThemeScope := LCandidate.FThemeScope;
    FGeneration := LCandidate.FGeneration;
    FGenerationObject := LCandidate.FGenerationObject;
    LCandidate.FGeneration := nil;
    LCandidate.FGenerationObject := nil;
    TBrowserViewGeneration(FGenerationObject).BaseThemeOwner := FBaseThemeOwner;
    for LIndex := 0 to Length(FBindings) - 1 do
    begin
      FBindings[LIndex].FRenderer := Self;
      FBindings[LIndex].FViewRevision := LCommittedRevision;
    end;
    { Prepared DOM handlers receive the committed epoch by assignment. Target
      extensions must not raise during destruction of the retired controls. }
    InstallNyxCoordinatorMounts(FCoordinatorMounts);

    if not ADesignMode then
    begin
      FViewPublicationSource := LSource;
    end;

    if LRootPublication <> nil then
    begin
      LRootPublication.Install;
    end;
    LAcceptedCoordinators := FCoordinatorMounts;
    for LIndex := 0 to High(LCollectionAdmissions) do
    begin
      LCollectionAdmissions[LIndex].InstallInput;
    end;
    LCollectionInputGate.Install;
    { The accepted generation owns its own shield. An old entered Sync may
      have been updating at launch; its finally no longer owns this successor. }
    FUpdating := False;
    LCommitted := True;
  finally
    try

      if not LCommitted then
      begin
        RetirePreparation;
      end;
    finally
      ReleaseCandidate;
    end;
  end;
  except
    on E: Exception do
    begin

      if not LCommitted then
      begin
        raise;
      end;
      RememberCleanupFailure(E);
    end;
  end;
  { Postcommit cleanup may navigate or dispose the renderer. Only local managed
    owners are used after this point; the accepted data is never rolled back. }
  FinishPublication;
end;

procedure TNyxBrowserBinding.ImageSignal;
var
  LGeneration: TBrowserViewGeneration;
  LGenerationLease: IInterface;
begin
  try
    LGeneration := TBrowserViewGeneration(FGenerationObject);
    LGenerationLease := LGeneration;

    if (LGeneration = nil) or not LGeneration.Current or FDisconnected then
    begin
      Exit;
    end;

    if FButtonGlyphImage <> nil then
    begin
      PaintButtonFace;
    end;
    CurrentRenderer.FlushImages;
  except
    on E: ENyxBrowserViewRetired do
    begin
      Exit;
    end;
  end;
end;

procedure TNyxBrowserBinding.PaintButtonFace;
var
  LGeneration: TBrowserViewGeneration;
  LGenerationLease: IInterface;
  LReady, LCaptionVisible: Boolean;
  LBounds: TNyxGlyphBounds;
  LWidth, LHeight, LScale, LRatio: Double;
  LInk, LKey: TNyxText;
  LContext: Web.TJSCanvasRenderingContext2D;
  LFormat: TFormatSettings;
  function Pixels(AValue: Double): String;
  begin
    Result := FloatToStr(AValue, LFormat) + 'px';
  end;
begin
  try
    LGeneration := TBrowserViewGeneration(FGenerationObject);
    LGenerationLease := LGeneration;

    if (LGeneration = nil) or not LGeneration.Current or FDisconnected then
    begin
      Exit;
    end;
    LFormat := FormatSettings;
    LFormat.DecimalSeparator := '.';
    LReady := (FImageLifecycle <> nil) and (FImageLifecycle.Current.Phase = nipReady) and
      (FButtonPresentation.ContentMode <> nbcCaption);

    if LReady then
    begin
      try
        LBounds := FButtonPresentation.GlyphCell.PixelBounds(
          FImageLifecycle.Current.Width, FImageLifecycle.Current.Height);
      except
        { A decoded image may be too small for the authored grid. Keep its command
          usable without presenting an arbitrary adjacent cell or broken image. }
        LReady := False;
      end;
    end;

    if LReady then
    begin
      try
        LScale := FButtonPresentation.GlyphBoxSize / Max(LBounds.Width, LBounds.Height);
        LWidth := LBounds.Width * LScale;
        LHeight := LBounds.Height * LScale;
        { Bound decorative raster work independently of browser zoom. CSS retains
          logical dimensions, while up to four device pixels per logical pixel
          keeps normal high-density displays crisp without unbounded tile memory. }
        LRatio := EnsureRange(window.devicePixelRatio, 1, 4);
        LInk := '';

        if FButtonPresentation.Tone = ngtForeground then
        begin
          LInk := window.getComputedStyle(FElement).getPropertyValue('color');

          if LInk = '' then
          begin
            LInk := CurrentRenderer.FTheme.Text;
          end;
        end;
        LKey := IntToStr(FImageLifecycle.Current.Request) + '/' +
          IntToStr(LBounds.Left) + '/' + IntToStr(LBounds.Top) + '/' +
          IntToStr(LBounds.Width) + '/' + IntToStr(LBounds.Height) + '/' +
          IntToStr(FButtonPresentation.GlyphBoxSize) + '/' +
          FloatToStr(LRatio, LFormat) + '/' + IntToStr(Ord(FButtonPresentation.Tone)) + '/' + LInk;

        if FButtonGlyphRasterKey <> LKey then
        begin
          FButtonGlyphFace.width := Max(1, Round(LWidth * LRatio));
          FButtonGlyphFace.height := Max(1, Round(LHeight * LRatio));
          LContext := FButtonGlyphFace.getContextAs2DContext('2d');

          if LContext = nil then
          begin
            raise ENyxModel.Create('Decorative canvas rendering is unavailable');
          end;
          { Draw from the existing decoded image request. There is no readback,
            data-URL conversion or new CORS requirement for hosted image pixels. }
          LContext.globalCompositeOperation := 'source-over';
          LContext.drawImage(FButtonGlyphImage, LBounds.Left, LBounds.Top,
            LBounds.Width, LBounds.Height, 0, 0, FButtonGlyphFace.width, FButtonGlyphFace.height);

          if FButtonPresentation.Tone = ngtForeground then
          begin
            LContext.globalCompositeOperation := 'source-in';
            LContext.fillStyleAsColor := LInk;
            LContext.fillRect(0, 0, FButtonGlyphFace.width, FButtonGlyphFace.height);
            LContext.globalCompositeOperation := 'source-over';
          end;
          FButtonGlyphRasterKey := LKey;
        end;
        FButtonGlyphFace.style.setProperty('width', Pixels(LWidth));
        FButtonGlyphFace.style.setProperty('height', Pixels(LHeight));
      except
        { The source may remain Ready even when a host cannot paint its crop.
          The ordinary caption still names and invokes the same command. }
        FButtonGlyphRasterKey := '';
        LReady := False;
      end;
    end;

    if LReady then
    begin
      FButtonGlyphFace.style.setProperty('display', 'inline-block');
    end
    else
    begin
      FButtonGlyphRasterKey := '';
      FButtonGlyphFace.style.setProperty('display', 'none');
    end;
    LCaptionVisible := not LReady or (FButtonPresentation.ContentMode <> nbcGlyphOnly);

    if FCaption.textContent = '' then
    begin
      FCaption.textContent := FNode.Prop('aria-label');
    end;
    FCaption.style.removeProperty('display');

    if not LCaptionVisible then
    begin
      FCaption.style.setProperty('display', 'none');
    end;
  except
    on E: ENyxBrowserViewRetired do
    begin
      Exit;
    end;
  end;
end;

procedure TNyxBrowserBinding.SyncButtonFace;
var
  LGeneration: TBrowserViewGeneration;
  LGenerationLease: IInterface;
  LSource: TNyxImageSource;
  LMinimumHeight: Integer;
  LDisclosureRoot, LDisclosureBody: TNyxNode;
  LCaption: TNyxText;
begin
  try
    LGeneration := TBrowserViewGeneration(FGenerationObject);
    LGenerationLease := LGeneration;

    if (LGeneration = nil) or not LGeneration.Current or FDisconnected then
    begin
      Exit;
    end;

    if FCustom or (FNode.ProjectionKind <> 'button') then
    begin
      Exit;
    end;

    if (FNode.Prop(NyxAttributeName(atButtonPresentation)) = '') and
      (FButtonGlyphImage = nil) then
    begin
      Exit;
    end;
    FButtonPresentation := NyxButtonPresentationOf(FNode);
    LSource := NyxButtonGlyphSource(FNode, FButtonPresentation);

    if not SafeURL(LSource.ToWire, True) then
    begin
      raise ENyxModel.Create('Unsupported button glyph URL');
    end;

    if FButtonGlyphImage = nil then
    begin
      FElement.textContent := '';
      FButtonGlyphFace := TJSHTMLCanvasElement(document.createElement('canvas'));
      FButtonGlyphFace.setAttribute('aria-hidden', 'true');
      FButtonGlyphFace.style.setProperty('flex-shrink', '0');
      FButtonGlyphFace.style.setProperty('pointer-events', 'none');
      FElement.appendChild(FButtonGlyphFace);
      FCaption := TJSHTMLElement(document.createElement('span'));
      FElement.appendChild(FCaption);
      FButtonGlyphImage := TJSHTMLImageElement(document.createElement('img'));
      FButtonGlyphImage.alt := '';
      FButtonGlyphImage.setAttribute('aria-hidden', 'true');
      FButtonGlyphImage.style.setProperty('display', 'none');
      FElement.appendChild(FButtonGlyphImage);
    end;
    LCaption := FNode.Prop('text');

    if NyxDisclosureHeader(FNode, LDisclosureRoot, LDisclosureBody, LCaption) then
    begin
      { The ordinary header inherits its compound root caption. Decorative
        presentation must not replace that effective name with empty local Text. }
      FCaption.textContent := LCaption;
    end
    else
    begin
      FCaption.textContent := FNode.Prop('text');
    end;

    if NyxInteractionPolicy(FNode).Visible then
    begin
      FElement.style.setProperty('display', 'inline-flex');
    end
    else
    begin
      { The face owns its decoration; portable interaction still owns visibility. }
      FElement.style.setProperty('display', 'none');
    end;
    FElement.style.setProperty('align-items', 'center');
    FElement.style.setProperty('gap', IntToStr(FButtonPresentation.GlyphSpacing) + 'px');
    FElement.style.setProperty('justify-content', 'center');

    if FButtonPresentation.ContentAlignment = nbaStart then
    begin
      FElement.style.setProperty('justify-content', 'flex-start');
    end;
    LMinimumHeight := 44;
    case FButtonPresentation.ControlDensity of
      nbdNormal: ; { Ordinary browser button metrics. }
      nbdCompact: LMinimumHeight := 36;
      nbdTouch: LMinimumHeight := 44;
    end;
    FElement.style.setProperty('min-height', IntToStr(LMinimumHeight) + 'px');
    FElement.style.setProperty('padding', '4px 12px');
    FElement.style.removeProperty('font-weight');
    case FButtonPresentation.Weight of
      nbwInherit: ; { The existing stylesheet supplies the inherited weight. }
      nbwRegular: FElement.style.setProperty('font-weight', '400');
      nbwSemibold: FElement.style.setProperty('font-weight', '600');
    end;

    if (FImageLifecycle = nil) or not FImageLifecycle.Current.Defined or
      (FImageLifecycle.Current.Source.ToWire <> LSource.ToWire) then
    begin

      if FImageRequest <> nil then
      begin
        FImageRequest.Retire;
        FImageRequest := nil;
      end;

      if FImageLifecycle = nil then
      begin
        FImageLifecycle := NewNyxImageLifecycle;
      end;
      FImageRequest := StartNyxBrowserImage(FButtonGlyphImage, LSource, FImageLifecycle, ImageSignal);
    end;
    PaintButtonFace;
  except
    on E: ENyxBrowserViewRetired do
    begin
      Exit;
    end;
  end;
end;

procedure TNyxBrowserBinding.SyncImage;
var
  LGeneration: TBrowserViewGeneration;
  LGenerationLease: IInterface;
  LSource: TNyxImageSource;
begin
  try
    LGeneration := TBrowserViewGeneration(FGenerationObject);
    LGenerationLease := LGeneration;

    if (LGeneration = nil) or not LGeneration.Current or FDisconnected then
    begin
      Exit;
    end;
    LSource := TNyxImageSource.FromWire(FNode.Prop('src'));

    if (FImageLifecycle <> nil) and FImageLifecycle.Current.Defined and
      (FImageLifecycle.Current.Source.ToWire = LSource.ToWire) then
    begin
      Exit;
    end;

    if FImageRequest <> nil then
    begin
      FImageRequest.Retire;
      FImageRequest := nil;
    end;

    if FImageLifecycle = nil then
    begin
      FImageLifecycle := NewNyxImageLifecycle;
    end;
    FImageRequest := StartNyxBrowserImage(TJSHTMLImageElement(FElement), LSource,
      FImageLifecycle, ImageSignal);
  except
    on E: ENyxBrowserViewRetired do
    begin
      Exit;
    end;
  end;
end;

procedure TNyxBrowserRenderer.FlushImages;
var
  LGeneration: TBrowserViewGeneration;
  LGenerationLease: IInterface;
  LDeliveries: TNyxImageDeliveries;
  LIndex: Integer;
  LDeliveryIndex: Integer;
begin
  LGeneration := TBrowserViewGeneration(FGenerationObject);
  LGenerationLease := FGeneration;

  if (LGeneration = nil) or not LGeneration.Current then
  begin
    Exit;
  end;

  if FUpdating or FDetachedCandidate or FPublishingContent or FDesignMode then
  begin
    Exit;
  end;
  LDeliveries := nil;
  try
    for LIndex := 0 to Length(FBindings) - 1 do
    begin
      PrepareNyxImageDeliveries(FEvents, FBindings[LIndex].FNode,
        FBindings[LIndex].FImageLifecycle, LDeliveries);

      if not LGeneration.Current then
      begin
        for LDeliveryIndex := 0 to High(LDeliveries) do
        begin
          LDeliveries[LDeliveryIndex].Free;
        end;
        Exit;
      end;
    end;
  except
    for LIndex := 0 to Length(LDeliveries) - 1 do
    begin
      LDeliveries[LIndex].Free;
    end;
    raise;
  end;
  { The FIFO owns copied payloads. Its later UI turn can retire this renderer
    without using a borrowed binding or node. }

  if FImageDeliveryQueue = nil then
  begin
    FImageDeliveryQueue := NewNyxImageDeliveryQueue;
  end;
  FImageDeliveryQueue.Enqueue(FEvents, LDeliveries);
end;

procedure TNyxBrowserRenderer.ArrangeControls(AFormer: TNyxNode);
type
  TEditingPosition = record
    Selection: TNyxTextSelection;
    Top: NativeInt;
    Left: NativeInt;
    InputTop: NativeInt;
    InputLeft: NativeInt;
  end;
var
  LPositions: array of TEditingPosition;
  LFocus: TJSHTMLElement;
  LInside: Boolean;
  LUpdating: Boolean;
  LIndex: Integer;
  LTop: NativeInt;
  LLeft: NativeInt;

  procedure Arrange(ANode: TNyxNode);
  var
    LParent: TJSHTMLElement;
    LChild: Integer;
    LElement: TJSHTMLElement;
    LNext: TJSNode;
  begin
    LParent := ElementFor(ANode.ID, niRuntime);
    { Candidate preorder moves an ancestor before its descendants. This also
      safely reverses a former parent/child relationship without a transient
      DOM cycle. Reverse insertion establishes exact authored sibling order,
      leaving each host's private caption/glyph nodes in place. }
    LNext := nil;

    if NyxProjectionChildrenChanged(ANode, AFormer) then
    begin
      for LChild := ANode.Count - 1 downto 0 do
      begin
        LElement := ElementFor(ANode.Children[LChild].ID, niRuntime);

        if (LElement.parentNode <> LParent) or (LElement.nextSibling <> LNext) then
        begin
          LParent.insertBefore(LElement, LNext);
        end;
        LNext := LElement;
      end;
    end;
    for LChild := 0 to ANode.Count - 1 do
    begin
      Arrange(ANode.Children[LChild]);
    end;
  end;

begin
  LFocus := TJSHTMLElement(document.activeElement);
  LInside := (LFocus <> nil) and FHost.contains(LFocus);
  LTop := FHost.scrollTop;
  LLeft := FHost.scrollLeft;
  SetLength(LPositions, Length(FBindings));
  for LIndex := 0 to High(FBindings) do
  begin
    LPositions[LIndex].Top := FBindings[LIndex].FElement.scrollTop;
    LPositions[LIndex].Left := FBindings[LIndex].FElement.scrollLeft;

    if FBindings[LIndex].FInput <> nil then
    begin
      LPositions[LIndex].Selection := CaptureNyxBrowserSelection(FBindings[LIndex].FInput);
      LPositions[LIndex].InputTop := FBindings[LIndex].FInput.scrollTop;
      LPositions[LIndex].InputLeft := FBindings[LIndex].FInput.scrollLeft;
    end;
  end;
  LUpdating := FUpdating;
  FUpdating := True;
  try
    Arrange(FRoot);

    if LInside then
    begin
      NyxFocusWithoutScroll(LFocus);
    end;
    for LIndex := 0 to High(FBindings) do
    begin

      if LPositions[LIndex].Selection.Defined then
      begin
        SelectNyxBrowserText(FBindings[LIndex].FInput, LPositions[LIndex].Selection);
      end;

      if FBindings[LIndex].FInput <> nil then
      begin
        FBindings[LIndex].FInput.scrollTop := LPositions[LIndex].InputTop;
        FBindings[LIndex].FInput.scrollLeft := LPositions[LIndex].InputLeft;
      end;
      FBindings[LIndex].FElement.scrollTop := LPositions[LIndex].Top;
      FBindings[LIndex].FElement.scrollLeft := LPositions[LIndex].Left;
    end;
    FHost.scrollTop := LTop;
    FHost.scrollLeft := LLeft;
  finally
    FUpdating := LUpdating;
  end;
end;

function TNyxBrowserRenderer.TryRefresh(ADocument: TNyxDocument; ARoot: TNyxNode;
  ADesignMode: Boolean; const ARestores: TNyxProjectionValueRestores): Boolean;
var
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
  FEvents.Scheduler.RequireUI;
  Result := False;

  { Data notification keeps the mounted board and calls Sync. Authored tree
    replacement takes full candidate admission for exact node/view authority. }

  if (FRoot = nil) or (FHost = nil) or FUpdating or
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
    Exit;
  end;
  { Design mode leaves live subscriptions inactive. Permit its unbound scalar
    refresh without treating an absent subscription as unfinished work. Bound
    design/runtime coordinators and active state commands still refuse. }
  for LIndex := 0 to High(FBindings) do
  begin

    if FBindings[LIndex].FCustom or (FactoryIndex(FBindings[LIndex].FNode) >= 0) then
    begin
      Exit;
    end;
  end;
  ValidateNyxDocumentProperties(ADocument);

  if NyxProjectionContext(ADocument) <> FProjectionContext then
  begin
    Exit;
  end;
  LCandidate := nil;
  LPrevious := nil;
  LProjectedCandidate := nil;
  LProjectedBaseline := nil;
  LTheme := nil;
  LBound := (FLiveBindings <> nil) and FLiveBindings.HasBindings;
  LContentBlueprint := nil;
  try

    if FBorrowedTheme <> nil then
    begin
      LTheme := NewNyxDocumentTheme(ADocument, FBorrowedTheme);
    end
    else
    begin
      LTheme := NewNyxDocumentTheme(ADocument, FBaseTheme);
    end;

    if LTheme.CSS <> FTheme.CSS then
    begin
      Exit;
    end;
    LFrame := FViewportBinding.Frame(FHost.clientWidth, FHost.clientHeight);
    LNextSelection := FPresentationSelection.Reconciled(ADocument.Presentations.Snapshot);

    if LNextSelection.Reference.Defined then
    begin
      LFrame := LFrame.Selecting(LNextSelection.Reference);
    end;
    LMeasurements := MeasureContainers;
    LCandidate := RealizeNyxView(ADocument, ARoot, LFrame, LMeasurements);

    if FResourceContext <> nil then
    begin
      LCandidate.BindResources(FResourceContext.Snapshot,
        FResourceContext.Locale, FResourceContext.Fallback);
    end;
    LContentBlueprint := NewNyxContentBlueprint(ADocument, ARoot);
    ApplyNyxPlatform(LCandidate, npfBrowser);

    LAccepted := LCandidate;
    LBaseline := FProjectionBaseline;

    if LBound then
    begin

      if not FLiveBindings.TryPrepareRefresh(LCandidate, FProjectionBaseline,
        LProjectedCandidate, LProjectedBaseline) then
      begin
        Exit;
      end;
      LAccepted := LProjectedCandidate;
      LBaseline := LProjectedBaseline;
    end;
    LArrangement := not Compatible(FRoot, LAccepted, False);

    if ((not LArrangement) and not Compatible(LBaseline, LAccepted, False)) or
      (LArrangement and (not Compatible(FRoot, LAccepted, True) or
        not Compatible(LBaseline, LAccepted, True))) or
      (FProjectionSchemaRevision <> NyxSchemaRevision) then
    begin
      Exit;
    end;
    LPrevious := FRoot.Clone;
    LPreviousSelection := FPresentationSelection;

    try

      if not ApplyProjection(LAccepted, LBaseline, ARestores) then
      begin
        Exit;
      end;

      if LArrangement then
      begin
        ArrangeControls(LPrevious);
      end;
      RestoreFields;
      FPresentationSelection := FPresentationSelection.Reconciled(FRoot.PresentationSnapshot);
      Sync;
      { First-rule admission starts observation; removing the last rule retires
        it. A retained projection must follow the same mount lifetime as Render. }
      ObserveViewport;
    except

      if LArrangement then
      begin
        ApplyProjection(LPrevious, nil, nil);
        ArrangeControls(LCandidate);
      end
      else
      begin
        ApplyProjection(LPrevious, nil, nil);
      end;
      FPresentationSelection := LPreviousSelection;
      Sync;
      ObserveViewport;
      raise;
    end;
    ReleaseNyxNode(FProjectionBaseline);
    FProjectionBaseline := LCandidate;
    LCandidate := nil;
    { Only admitted source replaces the independent mounted recipe book. A
      refused/failed retained refresh leaves future resize meaning unchanged. }
    FreeAndNil(FContentBlueprint);
    FContentBlueprint := LContentBlueprint;
    LContentBlueprint := nil;
    FContentFrameKnown := False;
    FContentRequestedSelection := FContentRequestedSelection.Reconciled(FRoot.PresentationSnapshot);
    ObserveViewport;
    QueueContent;
    Result := True;
  finally
    LContentBlueprint.Free;
    LTheme.Free;
    ReleaseNyxNode(LProjectedBaseline);
    ReleaseNyxNode(LProjectedCandidate);
    ReleaseNyxNode(LPrevious);
    ReleaseNyxNode(LCandidate);
  end;
end;

procedure TNyxBrowserRenderer.SetTableTextLimits(const ALimits: TNyxTableTextWorkLimits);
begin
  ALimits.Validate;

  if (FRoot <> nil) or (FRenderAttempt <> nil) or FPublishingContent then
  begin
    raise ENyxModel.Create('Configure table text limits before mounting or after Unmount');
  end;
  FTableTextLimits := ALimits;
end;

procedure TNyxBrowserRenderer.SetTableWorkbenchServices(
  const AServices: INyxTableWorkbenchServices);
begin

  if (FRoot <> nil) or (FRenderAttempt <> nil) or FPublishingContent then
  begin
    raise ENyxModel.Create('Configure table services before mounting or after Unmount');
  end;
  FTableWorkbenchServices := AServices;
end;

procedure TNyxBrowserRenderer.SetResourceContext(const AContext: INyxResourceContext);
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

function TNyxBrowserRenderer.GetResourceContext: INyxResourceContext;
begin
  Result := FResourceContext;

  if FRoot <> nil then
  begin
    Result := FRoot.ResourceContext;
  end;
end;

function TNyxBrowserRenderer.PrepareResourcePublication(const AContext: INyxResourceContext): INyxPreparedPublication;
begin

  if (FLiveBindings = nil) or FPublishingContent or (AContext = nil) then
  begin
    raise ENyxState.Create('Resource publication requires an idle mounted view and context');
  end;
  Result := FLiveBindings.PrepareResourcePublication(AContext.Snapshot,
    AContext.Locale, AContext.Fallback);
end;

function TNyxBrowserRenderer.PrepareResourceReload(
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

function TNyxBrowserRenderer.SectionPublicationReady: Boolean;
var
  LIndex: Integer;
begin
  FEvents.Scheduler.RequireUI;
  Result := (FRoot <> nil) and not FUpdating and not FPublishingContent and
    not FContentGuard.Busy and ((FState = nil) or not FState.Busy) and
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

function TNyxBrowserRenderer.ResourceReady: Boolean;
begin
  Result := (FLiveBindings <> nil) and not FPublishingContent;

  if Result then
  begin
    Result := FLiveBindings.RefreshReady;
  end;
end;

procedure TNyxBrowserRenderer.SetResourceUpdates(const AQueue: INyxResourceUpdateQueue);
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

function TNyxBrowserRenderer.CanReloadResources(const AContext: INyxResourceContext): Boolean;
begin
  Result := ResourceReady;

  if Result then
  begin
    FLiveBindings.ValidateResourceReload(AContext.Snapshot,
      AContext.Locale, AContext.Fallback);
  end;
end;

procedure TNyxBrowserRenderer.WakeResources;
begin

  if (FResourceUpdates <> nil) and (not FPublishingContent) then
  begin
    FResourceUpdates.Wake;
  end;
end;

procedure TNyxBrowserRenderer.ReloadResources(const AResources: INyxResources;
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

procedure TNyxBrowserRenderer.Unmount;
begin
  FRenderAttempt := nil;
  FViewportBinding.Suspend;
  FViewportExported := False;
  Clear;
end;

function TNyxBrowserRenderer.ReadViewportAllocation(out AWidth, AHeight: Double): Boolean;
begin
  Result := (FRoot <> nil) and (FHost <> nil);
  AWidth := 0;
  AHeight := 0;

  if Result then
  begin
    AWidth := FViewportWidth;
    AHeight := FViewportHeight;

    if not FDetachedCandidate then
    begin
      AWidth := FHost.clientWidth;
      AHeight := FHost.clientHeight;
    end;
  end;
end;

function TNyxBrowserRenderer.ViewportReady: Boolean;
begin
  Result := (FRoot <> nil) and not FDetachedCandidate and not FUpdating and
    not FPublishingContent and not FContentGuard.Busy;
end;

function TNyxBrowserRenderer.GetViewportContext: INyxViewportContext;
begin
  FViewportExported := True;
  Result := FViewportBinding.Context;
  ObserveViewport;
end;

function TNyxBrowserRenderer.GetPresentationViewport: INyxViewportContext;
begin
  Result := FViewportBinding.Source;
end;

procedure TNyxBrowserRenderer.SetPresentationViewport(const AValue: INyxViewportContext);
begin
  FEvents.Scheduler.RequireUI;

  if FPublishingContent or FUpdating then
  begin
    raise ENyxModel.Create('Enclosing viewport configuration requires an idle browser view');
  end;
  FViewportBinding.Source := AValue;
end;

function TNyxBrowserRenderer.GetPresentationViewportFailure: TNyxText;
begin
  Result := FViewportBinding.Failure;
end;

procedure TNyxBrowserRenderer.SetPresentationSelection(const AValue: TNyxPresentationSelection);
var
  LPrevious: TNyxPresentationSelection;
begin
  FEvents.Scheduler.RequireUI;

  if (FRoot = nil) or (FHost = nil) then
  begin
    raise ENyxModel.Create('Select a presentation on an idle mounted view');
  end;
  AValue.Validate(FRoot.PresentationSnapshot);

  if FContentBlueprint <> nil then
  begin
    { Structural selection is always queued, including a sequential callback.
      The capability continues reporting the accepted selection until publication. }
    FContentRequestedSelection := AValue;
    FContentFrameKnown := False;
    QueueContent;
    Exit;
  end;

  if FUpdating then
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

function TNyxBrowserRenderer.ReadPresentationSelection: TNyxPresentationSelection;
begin
  FEvents.Scheduler.RequireUI;

  if FRoot = nil then
  begin
    raise ENyxPresentation.Create('This presentation view is not mounted');
  end;
  Result := FPresentationSelection;
end;

function TNyxBrowserRenderer.GetPresentationView: INyxPresentationView;
begin
  ReadPresentationSelection;

  if FPresentationView = nil then
  begin
    FPresentationView := NewNyxPresentationView(@ReadPresentationSelection, @SetPresentationSelection);
  end;
  Result := FPresentationView;
end;

function TNyxBrowserRenderer.CollectionView(const AID: TNyxText): INyxCollectionView;
var
  LIndex: Integer;
begin
  { Query the actual connected runtime provider without persisting it into the
    document. Authored-default views retain their existing fallback contract. }
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

function TNyxBrowserRenderer.CollectionMount(const AID: TNyxText): INyxCollectionMount;
var
  LIndex: Integer;
begin
  for LIndex := 0 to Length(FBindings) - 1 do
  begin

    if FBindings[LIndex].FNode.ID = AID then
    begin
      Result := FBindings[LIndex].FCollectionMount;

      if (Result <> nil) and Result.Connected then
      begin
        Exit;
      end;
      Break;
    end;
  end;
  raise ENyxModel.Create('No live collection attachment: ' + AID);
end;

function TNyxBrowserRenderer.BindCollection(const AID: TNyxText;
  const AView: INyxCollectionView): INyxCollectionMount;
var
  LElement: TJSHTMLElement;
  LIndex: Integer;
  LKind: TNyxText;
  LQueryMount: INyxCollectionQueryMount;
  LColumnsMount: INyxCollectionColumnArrangementMount;
  LColumnsInput: INyxCollectionInputAdmission;
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
  LElement := ElementFor(AID);
  for LIndex := 0 to Length(FBindings) - 1 do
  begin

    if FBindings[LIndex].FElement = LElement then
    begin

      if FBindings[LIndex].FNode.ProjectionKind <> LKind then
      begin
        raise ENyxModel.Create('Collection view differs from the mounted control kind');
      end;

      if (FBindings[LIndex].FCollectionMount <> nil) and
        FBindings[LIndex].FCollectionMount.Connected then
      begin
        raise ENyxModel.Create('Control already has a live collection binding');
      end;
      Result := MountNyxBrowserCollectionWithTextLimits(LElement, AView, FTableTextLimits,
        FCollectionInputPhase, FCollectionInputGate);
      FBindings[LIndex].FCollectionMount := Result;
      Result.ObserveSelection(FBindings[LIndex].CollectionSelectionChanged);

      if not FDesignMode and (AView.Projection = cpTable) and
        Supports(Result, INyxCollectionQueryMount, LQueryMount) then
      begin
        LQueryMount.ConfigureQueries(NewNyxBrowserQueryFormFactory(
          LElement, FTheme), FCollectionQueryEvents,
          NyxControlEvents(FBindings[LIndex].FNode.ID, niRuntime), FBindings[LIndex].FNode.DesignID);
      end;

      if not FDesignMode and (AView.Projection = cpTable) and
        AView.Spec.HasLayoutControls and
        Supports(Result, INyxCollectionColumnArrangementMount, LColumnsMount) and
        Supports(Result, INyxCollectionInputAdmission, LColumnsInput) then
      begin
        LColumnsMount.ConfigureColumnArrangement(
          NewNyxBrowserColumnArrangementFactory(LElement, FTheme,
            LColumnsInput.GetInputLifetime), FCollectionQueryEvents);
      end;

      if not FDesignMode and (AView.Projection = cpTable) then
      begin
        LWorkbenchSpec := AView.Spec;

        if LWorkbenchSpec.HasTableWorkbenchPolicy then
        begin
          ConfigureNyxTableWorkbench(Result, LWorkbenchSpec.TableWorkbenchPolicy,
            FTableWorkbenchServices, NewNyxBrowserCollectionCellPresenterFactory(LElement),
            FCollectionQueryEvents);
        end;
      end;
      Exit;
    end;
  end;
  raise ENyxModel.Create('Collection control is not mounted: ' + AID);
end;

procedure TNyxBrowserRenderer.MoveHost(AHost: TJSHTMLElement);
var
  LFocus: TJSHTMLElement;
  LSelection: TNyxTextSelection;
  LInside: Boolean;
  LScrollTop: NativeInt;
  LScrollLeft: NativeInt;
  LUpdating: Boolean;
begin

  if (AHost = nil) or (FHost = nil) then
  begin
    raise ENyxModel.Create('Mounted view and replacement browser host are required');
  end;

  if FCompiledMount <> nil then
  begin
    FCompiledMount.MoveHost(AHost);
    FHost := AHost;
    Exit;
  end;

  if AHost = FHost then
  begin
    Exit;
  end;

  if FHost.contains(AHost) or (AHost.firstChild <> nil) then
  begin
    raise ENyxModel.Create('Replacement browser host must be empty and outside the view');
  end;
  LFocus := TJSHTMLElement(document.activeElement);
  LInside := (LFocus <> nil) and FHost.contains(LFocus);
  LSelection := Default(TNyxTextSelection);

  if LInside then
  begin
    LSelection := CaptureNyxBrowserSelection(LFocus);
  end;
  LScrollTop := FHost.scrollTop;
  LScrollLeft := FHost.scrollLeft;
  LUpdating := FUpdating;
  FUpdating := True;
  try
    AHost.classList.add('nyx-root');
    AHost.setAttribute('data-nyx-theme', FThemeScope);

    if FDesignMode then
    begin
      AHost.classList.add('nyx-design');
    end;
    while FHost.firstChild <> nil do
    begin
      AHost.appendChild(FHost.firstChild);
    end;
    FHost.classList.remove('nyx-root');
    FHost.classList.remove('nyx-design');
    FHost.removeAttribute('data-nyx-theme');
    FHost := AHost;

    if LInside then
    begin
      { Moving nodes can blur the real editor even though its object survived.
        Restore the range/focus under the update guard, with no application
        focus callback and no incidental scroll-to-top in its new chrome. }
      NyxFocusWithoutScroll(LFocus);

      if LSelection.Defined then
      begin
        SelectNyxBrowserText(LFocus, LSelection);
      end;
    end;
    FHost.scrollTop := LScrollTop;
    FHost.scrollLeft := LScrollLeft;
  finally
    FUpdating := LUpdating;
  end;
  ObserveViewport;
  FViewportWidth := FHost.clientWidth;
  FViewportHeight := FHost.clientHeight;
  Sync;
end;

procedure TNyxBrowserRenderer.ObserveViewport(AReceiver: TNyxBrowserRenderer);
var
  LIndex: Integer;
begin

  if AReceiver = nil then
  begin
    AReceiver := Self;
  end;

  if FViewportObserver <> nil then
  begin
    FViewportObserver.disconnect;
    FViewportObserver := nil;
  end;

  if (FRoot <> nil) and (FHost <> nil) and
    (FRoot.HasViewportRules or (FContentBlueprint <> nil) or FViewportExported) then
  begin
    FViewportObserver := TJSHTMLResizeObserver.new(@AReceiver.ViewportChanged);
    FViewportObserver.observe(FHost);
    for LIndex := 0 to High(FBindings) do
    begin

      if FBindings[LIndex].FNode.QueryContainer.Defined then
      begin
        FViewportObserver.observe(FBindings[LIndex].FElement);
      end;
    end;
  end;
end;

procedure TNyxBrowserRenderer.ViewportChanged(AEntries: TJSHTMLResizeObserverEntryArray;
  AObserver: TJSHTMLResizeObserver);
var
  LWidth: Double;
  LHeight: Double;
  LIndex: Integer;
  LContainerChanged: Boolean;
  LBindingIndex: Integer;
begin

  if (AObserver <> FViewportObserver) or (FHost = nil) or (FRoot = nil) then
  begin
    Exit;
  end;
  LWidth := FHost.clientWidth;
  LHeight := FHost.clientHeight;
  LContainerChanged := False;
  for LIndex := 0 to High(AEntries) do
  begin
    LContainerChanged := LContainerChanged or (AEntries[LIndex].target <> FHost);
    for LBindingIndex := 0 to High(FBindings) do
    begin

      if (AEntries[LIndex].target = FBindings[LBindingIndex].FElement) and
        FBindings[LBindingIndex].FNode.QueryContainer.Defined then
      begin
        FBindings[LBindingIndex].FContainerWidth := AEntries[LIndex].contentRect.width;
        FBindings[LBindingIndex].FContainerHeight := AEntries[LIndex].contentRect.height;
        FBindings[LBindingIndex].FContainerMeasured := True;
        Break;
      end;
    end;
  end;

  if LContainerChanged or (LWidth <> FViewportWidth) or (LHeight <> FViewportHeight) then
  begin
    FViewportWidth := LWidth;
    FViewportHeight := LHeight;
    Sync;
    UpdateResizePreview;
  end;
end;

function TNyxBrowserRenderer.MeasureContainers: INyxContainerSnapshot;
var
  LMeasurements: TNyxContainerMeasurements;
  LIndex, LCount: Integer;
  LElement: TJSHTMLElement;
begin

  if FDetachedCandidate then
  begin
    Exit(FStagingMeasurements);
  end;
  LMeasurements := nil;
  for LIndex := 0 to High(FBindings) do
  begin

    if not FBindings[LIndex].FNode.QueryContainer.Defined or
      not FBindings[LIndex].FContainerMeasured then
    begin
      Continue;
    end;
    LElement := FBindings[LIndex].FElement;

    if not document.body.contains(LElement) or (Length(LElement.getClientRects) = 0) then
    begin
      Continue;
    end;
    LCount := Length(LMeasurements);
    SetLength(LMeasurements, LCount + 1);
    LMeasurements[LCount].RuntimeID := FBindings[LIndex].FNode.ID;
    LMeasurements[LCount].Width := FBindings[LIndex].FContainerWidth;
    LMeasurements[LCount].Height := FBindings[LIndex].FContainerHeight;
  end;
  Result := NewNyxContainerSnapshot(LMeasurements);
end;

function TNyxBrowserRenderer.MeasureAllocatedContainers: INyxContainerSnapshot;
var
  LMeasurements: TNyxContainerMeasurements;
  LIndex: Integer;
  LCount: Integer;
  LElement: TJSHTMLElement;
  LComputed: TJSCSSStyleDeclaration;

  function Pixels(const AName: String): Double;
  begin
    Result := parseFloat(LComputed.getPropertyValue(AName));

    if IsNan(Result) then
    begin
      Result := 0;
    end;
  end;

begin
  LMeasurements := nil;
  for LIndex := 0 to High(FBindings) do
  begin

    if not FBindings[LIndex].FNode.QueryContainer.Defined then
    begin
      Continue;
    end;
    LElement := FBindings[LIndex].FElement;

    if not document.body.contains(LElement) or (Length(LElement.getClientRects) = 0) then
    begin
      Continue;
    end;
    LComputed := window.getComputedStyle(LElement);
    LCount := Length(LMeasurements);
    SetLength(LMeasurements, LCount + 1);
    LMeasurements[LCount].RuntimeID := FBindings[LIndex].FNode.ID;
    { Used CSS dimensions preserve fractional layout without transforms. Border
      boxes include padding/borders; content boxes already exclude them. Query
      publishers are Nyx block faces with an actual allocated width/height. }
    LMeasurements[LCount].Width := Pixels('width');
    LMeasurements[LCount].Height := Pixels('height');

    if LComputed.getPropertyValue('box-sizing') = 'border-box' then
    begin
      LMeasurements[LCount].Width := Max(0, LMeasurements[LCount].Width -
        Pixels('padding-left') - Pixels('padding-right') -
        Pixels('border-left-width') - Pixels('border-right-width'));
      LMeasurements[LCount].Height := Max(0, LMeasurements[LCount].Height -
        Pixels('padding-top') - Pixels('padding-bottom') -
        Pixels('border-top-width') - Pixels('border-bottom-width'));
    end;
  end;
  Result := NewNyxContainerSnapshot(LMeasurements);
end;

function TNyxBrowserRenderer.InputFor(const AID: TNyxText;
  AIdentity: TNyxIdentityKind): TJSHTMLElement;
var
  LElement: TJSHTMLElement;
  LIndex: Integer;
begin
  LElement := ElementFor(AID, AIdentity);
  Result := nil;
  for LIndex := 0 to High(FBindings) do
  begin

    if FBindings[LIndex].FElement = LElement then
    begin
      Exit(FBindings[LIndex].FInput);
    end;
  end;
end;

function TNyxBrowserRenderer.FocusFor(const AID: TNyxText;
  AIdentity: TNyxIdentityKind): TJSHTMLElement;
var
  LElement: TJSHTMLElement;
  LBinding: TNyxBrowserBinding;
  LIndex: Integer;
begin
  LElement := ElementFor(AID, AIdentity);
  Result := nil;
  for LIndex := 0 to High(FBindings) do
  begin
    LBinding := FBindings[LIndex];

    if (LBinding.FElement = LElement) and ((LBinding.FInput <> nil) or
      NyxSupportsKeyboard(LBinding.FNode) or
      (LBinding.FCustom and (LElement.tabIndex >= 0))) then
    begin
      Exit(LBinding.FocusElement);
    end;
  end;
end;

function TNyxBrowserRenderer.ElementFor(const AID: TNyxText;
  AIdentity: TNyxIdentityKind): TJSHTMLElement;
var
  LIndex: Integer;
  LPass: Integer;
begin
  { Automatic convenience has global precedence: a later exact runtime key must
    win over an earlier editable-owner match. Explicit modes avoid ambiguity when
    an authored slash name happens to equal another node's qualified runtime key. }
  for LPass := 0 to 1 do
  begin
    for LIndex := 0 to Length(FBindings) - 1 do
    begin

      if (LPass = 0) and (AIdentity <> niDesign) and
        (FBindings[LIndex].FNode.ID = AID) then
      begin
        Exit(FBindings[LIndex].FElement);
      end;

      if (LPass = 1) and (AIdentity <> niRuntime) and
        (FBindings[LIndex].FNode.DesignID = AID) then
      begin
        Exit(FBindings[LIndex].FElement);
      end;
    end;
  end;
  raise ENyxModel.Create('Browser component is not mounted: ' + AID);
end;

procedure TNyxBrowserRenderer.RenderCompiled(const AArtifact: TNyxText;
  AHost: TJSHTMLElement);
var
  LFrame: TJSHTMLElement;
begin

  if (AHost = nil) or (Pos('builds/job-', AArtifact) <> 1) or
    (Pos('..', AArtifact) > 0) or (Pos(':', AArtifact) > 0) or
    (Pos('\', AArtifact) > 0) then
  begin
    raise ENyxModel.Create('Compiled preview needs a local build artifact and host');
  end;
  LFrame := Element('iframe', 'nyx-compiled-preview');
  LFrame.setAttribute('src', AArtifact);
  LFrame.setAttribute('title', 'Compiled Nyx view');
  LFrame.style.setProperty('width', '100%');
  LFrame.style.setProperty('height', '620px');
  LFrame.style.setProperty('border', '0');
  Clear;
  FHost := AHost;
  FHost.textContent := '';
  FCompiledMount := TNyxBrowserPersistentMount.Create(LFrame, FHost, 620);
  FCompiledArtifact := AArtifact;
end;

function TNyxBrowserRenderer.IsCompiled(const AArtifact: TNyxText): Boolean;
begin
  Result := (FCompiledMount <> nil) and (AArtifact <> '') and
    (AArtifact = FCompiledArtifact);
end;

procedure TNyxBrowserRenderer.Select(const ADesignID: TNyxText);
var
  LIndex: Integer;
  LChosen: Boolean;
begin
  ClearResizePreview;
  FSelectedDesignID := ADesignID;

  if (FCanvasResizeGrips <> nil) and (FCanvasResizeGrips.Owner.ID <> ADesignID) then
  begin
    ClearCanvasResizeGrips;
  end;

  if (FCanvasMoveGrip <> nil) and (FCanvasMoveGrip.Owner.ID <> ADesignID) then
  begin
    ClearCanvasMoveGrip;
  end;
  LChosen := False;
  for LIndex := 0 to Length(FBindings) - 1 do
  begin

    if not LChosen and (FBindings[LIndex].FNode.DesignID = ADesignID) and
      NyxInteractionPolicy(FBindings[LIndex].FNode).Visible then
    begin
      FBindings[LIndex].FElement.classList.add('nyx-selected');
      LChosen := True;
    end
    else
    begin
      FBindings[LIndex].FElement.classList.remove('nyx-selected');
    end;
  end;
end;

procedure TNyxBrowserRenderer.ClearResizePreview;
var
  LIndex: Integer;
begin
  FResizePreview := Default(TNyxResizePreview);
  FDropPreview := Default(TNyxDropPreview);

  UpdateResizeListening;
  for LIndex := Low(FResizeEdges) to High(FResizeEdges) do
  begin

    if FResizeEdges[LIndex] <> nil then
    begin
      FResizeEdges[LIndex].remove;
      FResizeEdges[LIndex] := nil;
    end;
  end;
  UpdateCanvasResizeGrips;
end;

procedure TNyxBrowserRenderer.UpdateResizeListening;
begin

  if FResizePreview.Active or FDropPreview.Active or
    (FCanvasResizeGrips <> nil) or (FCanvasMoveGrip <> nil) then
  begin

    if not FResizeListening then
    begin
      document.addEventListener('scroll', FResizePreviewHandler, True);
      window.addEventListener('resize', FResizePreviewHandler);
      FResizeListening := True;
    end;
  end
  else if FResizeListening then
  begin
    TNyxPreviewEventTarget(document).Unlisten('scroll', FResizePreviewHandler, True);
    window.removeEventListener('resize', FResizePreviewHandler);
    FResizeListening := False;
  end;
end;

procedure TNyxBrowserRenderer.ClearCanvasResizeGrips;
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

    if FCanvasGripHosts[LAxis] <> nil then
    begin
      FCanvasGripHosts[LAxis].remove;
      FCanvasGripHosts[LAxis] := nil;
    end;
  end;
  LGrips := nil;
  UpdateResizeListening;
end;

procedure TNyxBrowserRenderer.AttachResizeGrips(const AGrips: INyxCanvasResizeGrips);
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
  ElementFor(AGrips.Owner.ID, niDesign);

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
      FCanvasGripHosts[LAxis] := Element('div', 'nyx-canvas-resize-host');
      FCanvasGripHosts[LAxis].style.cssText :=
        'position:fixed;width:44px;height:44px;z-index:21;overflow:visible;';
      document.body.appendChild(FCanvasGripHosts[LAxis]);
      FCanvasGripViews[LAxis] := TNyxBrowserRenderer.Create(FTheme);
      FCanvasGripViews[LAxis].Render(AGrips.Document, AGrips.Root(LAxis), FCanvasGripHosts[LAxis]);
      AGrips.Bind(LAxis, FCanvasGripViews[LAxis].Events, @CanvasResizePoint);
    end;
    UpdateResizeListening;
    UpdateCanvasResizeGrips;
  except
    ClearCanvasResizeGrips;
    raise;
  end;
end;

function TNyxBrowserRenderer.CanvasResizeElement(AAxis: TNyxResizeAxis): TJSHTMLElement;
begin

  if (FCanvasResizeGrips = nil) or (FCanvasGripViews[AAxis] = nil) then
  begin
    raise ENyxModel.Create('Canvas resize adornment is not mounted');
  end;
  Result := FCanvasGripViews[AAxis].ElementFor(NyxCanvasResizeGripID(AAxis));
end;

function TNyxBrowserRenderer.CanvasResizePoint(AAxis: TNyxResizeAxis;
  const APointer: TNyxPointerSnapshot): TNyxResizePoint;
begin
  Result := LogicalPointFor(FCanvasResizeGrips.Owner.ID,
    FCanvasGripViews[AAxis].ScreenPointFor(NyxCanvasResizeGripID(AAxis), APointer), niDesign);
end;

procedure TNyxBrowserRenderer.UpdateCanvasResizeGrips;
var
  LGeneration: TBrowserViewGeneration;
  LGenerationLease: IInterface;
  LGrips: INyxCanvasResizeGrips;
  LOwner: TNyxControlRef;
  LDragging: Boolean;
  LAxis: TNyxResizeAxis;
  LElement: TJSHTMLElement;
  LFace: TJSDOMRect;
  LClip: TJSDOMRect;
  LWidth: Double;
  LHeight: Double;
  LX: Double;
  LY: Double;
  LVisible: Boolean;
  LFormat: TFormatSettings;
begin
  LGeneration := TBrowserViewGeneration(FGenerationObject);
  LGenerationLease := FGeneration;

  if (LGeneration = nil) or not LGeneration.Current then
  begin
    Exit;
  end;
  UpdateCanvasMoveGrip;

  if not LGeneration.Current then
  begin
    Exit;
  end;

  if (FCanvasResizeGrips = nil) or (FHost = nil) then
  begin
    Exit;
  end;
  LGrips := FCanvasResizeGrips;
  LOwner := LGrips.Owner;

  if not LGeneration.Current then
  begin
    Exit;
  end;
  LElement := ElementFor(LOwner.ID, niDesign);
  LFace := LElement.getBoundingClientRect;
  LClip := FHost.getBoundingClientRect;
  LWidth := LFace.width;
  LHeight := LFace.height;

  if FResizePreview.Active then
  begin
    LWidth := FResizePreview.Size.Width;
    LHeight := FResizePreview.Size.Height;

    if LElement.offsetWidth > 0 then
    begin
      LWidth := LWidth * LFace.width / LElement.offsetWidth;
    end;

    if LElement.offsetHeight > 0 then
    begin
      LHeight := LHeight * LFace.height / LElement.offsetHeight;
    end;
  end;
  LFormat := FormatSettings;
  LFormat.DecimalSeparator := '.';
  for LAxis := Low(TNyxResizeAxis) to High(TNyxResizeAxis) do
  begin
    LX := LFace.left + LWidth;
    LY := LFace.top + LHeight;

    if LAxis = nraWidth then
    begin
      LY := LFace.top + LHeight / 2;
    end;

    if LAxis = nraHeight then
    begin
      LX := LFace.left + LWidth / 2;
    end;
    { Keep 44px targets within the visible host. Hide overlapping one-axis
      handles on small faces; their Inspector alternatives remain available. }
    LVisible := (LClip.width >= 44) and (LClip.height >= 44) and
      (LX >= Max(0, LClip.left)) and (LX <= Min(window.innerWidth, LClip.right)) and
      (LY >= Max(0, LClip.top)) and (LY <= Min(window.innerHeight, LClip.bottom));

    LDragging := LGrips.Dragging(LAxis);

    if not LGeneration.Current then
    begin
      Exit;
    end;

    if not LDragging then
    begin
      LVisible := LVisible and ((LAxis <> nraWidth) or (LHeight >= 88)) and
        ((LAxis <> nraHeight) or (LWidth >= 88));
    end;

    if LVisible then
    begin
      FCanvasGripHosts[LAxis].style.setProperty('display', 'block');
      FCanvasGripHosts[LAxis].style.setProperty('left',
        FloatToStr(Max(Max(0, LClip.left), Min(LX - 22, Min(window.innerWidth, LClip.right) - 44)), LFormat) + 'px');
      FCanvasGripHosts[LAxis].style.setProperty('top',
        FloatToStr(Max(Max(0, LClip.top), Min(LY - 22, Min(window.innerHeight, LClip.bottom) - 44)), LFormat) + 'px');

      if not document.body.contains(FCanvasGripHosts[LAxis]) then
      begin
        document.body.appendChild(FCanvasGripHosts[LAxis]);
      end;
    end
    else
    begin
      FCanvasGripHosts[LAxis].style.setProperty('display', 'none');
    end;
  end;
end;

procedure TNyxBrowserRenderer.ClearCanvasMoveGrip;
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

  if FCanvasMoveHost <> nil then
  begin
    FCanvasMoveHost.remove;
    FCanvasMoveHost := nil;
  end;
  LGrip := nil;
  UpdateResizeListening;
end;

procedure TNyxBrowserRenderer.AttachMoveGrip(const AGrip: INyxCanvasMoveGrip);
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
  ElementFor(AGrip.Owner.ID, niDesign);

  if FCanvasMoveGrip = AGrip then
  begin
    UpdateCanvasMoveGrip;
    Exit;
  end;
  ClearCanvasMoveGrip;
  FCanvasMoveGrip := AGrip;
  try
    FCanvasMoveHost := Element('div', 'nyx-canvas-move-host');
    FCanvasMoveHost.style.cssText := 'position:fixed;width:44px;height:44px;z-index:21;overflow:visible;';
    document.body.appendChild(FCanvasMoveHost);
    FCanvasMoveView := TNyxBrowserRenderer.Create(FTheme);
    FCanvasMoveView.Render(AGrip.Document, AGrip.Root, FCanvasMoveHost);
    AGrip.Bind(FCanvasMoveView.Events, @CanvasMovePoint);
    UpdateResizeListening;
    UpdateCanvasMoveGrip;
  except
    ClearCanvasMoveGrip;
    raise;
  end;
end;

function TNyxBrowserRenderer.CanvasMovePoint(const APointer: TNyxPointerSnapshot): TNyxResizePoint;
begin

  if (FCanvasMoveGrip = nil) or (FCanvasMoveView = nil) then
  begin
    raise ENyxModel.Create('Canvas move adornment is not mounted');
  end;
  Result := LogicalPointFor(FCanvasMoveGrip.Owner.ID,
    FCanvasMoveView.ScreenPointFor(NyxCanvasMoveGripID, APointer), niDesign);
end;

procedure TNyxBrowserRenderer.UpdateCanvasMoveGrip;
var
  LGeneration: TBrowserViewGeneration;
  LGenerationLease: IInterface;
  LGrip: INyxCanvasMoveGrip;
  LOwner: TNyxControlRef;
  LElement: TJSHTMLElement;
  LFace, LClip: TJSDOMRect;
  LX, LY: Double;
  LFormat: TFormatSettings;
begin
  LGeneration := TBrowserViewGeneration(FGenerationObject);
  LGenerationLease := FGeneration;

  if (LGeneration = nil) or not LGeneration.Current then
  begin
    Exit;
  end;

  if (FCanvasMoveGrip = nil) or (FCanvasMoveHost = nil) or (FHost = nil) then
  begin
    Exit;
  end;
  LGrip := FCanvasMoveGrip;
  LOwner := LGrip.Owner;

  if not LGeneration.Current then
  begin
    Exit;
  end;
  LElement := ElementFor(LOwner.ID, niDesign);
  LFace := LElement.getBoundingClientRect;
  LClip := FHost.getBoundingClientRect;
  LX := LFace.left;
  LY := LFace.top;

  if FResizePreview.Active and (FResizePreview.Control.ID = LOwner.ID) then
  begin
    LX := LX + FResizePreview.OffsetX * LFace.width / Max(1, LElement.offsetWidth);
    LY := LY + FResizePreview.OffsetY * LFace.height / Max(1, LElement.offsetHeight);
  end;
  FCanvasMoveHost.style.setProperty('display', 'none');

  if (LClip.width < 44) or (LClip.height < 44) or (LFace.width <= 0) or (LFace.height <= 0) or
    (LX < Max(0, LClip.left)) or (LX > Min(window.innerWidth, LClip.right)) or
    (LY < Max(0, LClip.top)) or (LY > Min(window.innerHeight, LClip.bottom)) then
  begin
    Exit;
  end;
  LFormat := FormatSettings;
  LFormat.DecimalSeparator := '.';
  FCanvasMoveHost.style.setProperty('display', 'block');
  FCanvasMoveHost.style.setProperty('left',
    FloatToStr(Max(Max(0, LClip.left), Min(LX - 22, Min(window.innerWidth, LClip.right) - 44)), LFormat) + 'px');
  FCanvasMoveHost.style.setProperty('top',
    FloatToStr(Max(Max(0, LClip.top), Min(LY - 22, Min(window.innerHeight, LClip.bottom) - 44)), LFormat) + 'px');

  if not document.body.contains(FCanvasMoveHost) then
  begin
    document.body.appendChild(FCanvasMoveHost);
  end;
end;

function TNyxBrowserRenderer.ResizePreviewViewport(AEvent: TJSEvent): Boolean;
begin
  UpdateResizePreview;
  UpdateCanvasResizeGrips;
  Result := True;
end;

procedure TNyxBrowserRenderer.PreviewDrop(const APreview: TNyxDropPreview);
begin
  FEvents.Scheduler.RequireUI;

  if APreview.Active and (not FDesignMode or
    not APreview.Frame.SameFrame(DropFrameFor(APreview.Origin.ID))) then
  begin
    raise ENyxModel.Create('Drop presentation requires current exact design geometry');
  end;
  ClearResizePreview;
  FDropPreview := APreview;
  UpdateResizeListening;
  UpdateResizePreview;
end;

procedure TNyxBrowserRenderer.PreviewResize(const APreview: TNyxResizePreview);
begin
  FEvents.Scheduler.RequireUI;

  if not APreview.Active then
  begin
    ClearResizePreview;
    Exit;
  end;

  if not FDesignMode or (APreview.Control.ID <> FSelectedDesignID) then
  begin
    raise ENyxModel.Create('Resize presentation requires the selected authored face');
  end;
  { Resolve before replacing a live proposal so missing identity cannot erase
    the previous presentation. Four outline strips and at most four guide
    strips remain inert paint, independent of accepted document ownership. }
  ElementFor(APreview.Control.ID, niDesign);
  FDropPreview := Default(TNyxDropPreview);
  FResizePreview := APreview;

  UpdateResizeListening;
  UpdateResizePreview;
  UpdateCanvasResizeGrips;
end;

procedure TNyxBrowserRenderer.UpdateResizePreview;
var
  LElement: TJSHTMLElement;
  LFace: TJSDOMRect;
  LClip: TJSDOMRect;
  LWidth: Double;
  LHeight: Double;
  LLeft, LTop: Double;
  LIndex: Integer;
  LBox: TNyxGuideBox;
  LDropFrame: TNyxDropFrame;

  procedure Edge(AIndex: Integer; ALeft, ATop, AWidth, AHeight: Double);
  var
    LRight: Double;
    LBottom: Double;
    LFormat: TFormatSettings;
  begin
    { Clip the strip itself: a proposed far edge outside the viewport disappears
      rather than inventing an edge at the viewport's boundary. No scroll range
      is introduced by these fixed, pointer-transparent, non-focusable faces. }
    LRight := Min(ALeft + AWidth, Min(LClip.right, window.innerWidth));
    LBottom := Min(ATop + AHeight, Min(LClip.bottom, window.innerHeight));
    ALeft := Max(ALeft, Max(LClip.left, 0));
    ATop := Max(ATop, Max(LClip.top, 0));
    FResizeEdges[AIndex].style.setProperty('display', 'none');

    if (LRight > ALeft) and (LBottom > ATop) then
    begin
      { CSS requires a decimal point even when an application localizes its
        numeric display. Use copied settings; never change the user's locale. }
      LFormat := FormatSettings;
      LFormat.DecimalSeparator := '.';
      FResizeEdges[AIndex].style.setProperty('display', 'block');
      FResizeEdges[AIndex].style.setProperty('left', FloatToStr(ALeft, LFormat) + 'px');
      FResizeEdges[AIndex].style.setProperty('top', FloatToStr(ATop, LFormat) + 'px');
      FResizeEdges[AIndex].style.setProperty('width', FloatToStr(LRight - ALeft, LFormat) + 'px');
      FResizeEdges[AIndex].style.setProperty('height', FloatToStr(LBottom - ATop, LFormat) + 'px');
    end;
  end;

  procedure Guide(AIndex: Integer; const AGuide: TNyxAlignmentGuide; ASegment: Integer);
  var
    LBox: TNyxGuideBox;
    LScaleX, LScaleY: Double;
  begin
    FResizeEdges[AIndex].style.setProperty('display', 'none');
    LBox := AGuide.Segment(ASegment, FResizePreview.Size.Width, FResizePreview.Size.Height);

    if not LBox.Defined then
    begin
      Exit;
    end;
    LScaleX := 1;
    LScaleY := 1;

    if LElement.offsetWidth > 0 then
    begin
      LScaleX := LFace.width / LElement.offsetWidth;
    end;

    if LElement.offsetHeight > 0 then
    begin
      LScaleY := LFace.height / LElement.offsetHeight;
    end;
    Edge(AIndex, LLeft + (LBox.Left - AGuide.OwnerBox.Left) * LScaleX,
      LTop + (LBox.Top - AGuide.OwnerBox.Top) * LScaleY,
      LBox.Width * LScaleX, LBox.Height * LScaleY);
  end;

begin

  if (not FResizePreview.Active and not FDropPreview.Active) or (FHost = nil) then
  begin
    Exit;
  end;

  if FDropPreview.Active then
  begin
    LElement := ElementFor(FDropPreview.Origin.ID, niRuntime);
  end
  else
  begin
    LElement := ElementFor(FResizePreview.Control.ID, niDesign);
  end;
  LFace := LElement.getBoundingClientRect;
  LClip := FHost.getBoundingClientRect;
  { Resize policy works in logical allocation pixels. Preserve a containing
    scale when translating the proposal to the browser's viewport coordinates. }
  LWidth := FResizePreview.Size.Width;
  LHeight := FResizePreview.Size.Height;

  if LElement.offsetWidth > 0 then
  begin
    LWidth := LWidth * LFace.width / LElement.offsetWidth;
  end;

  if LElement.offsetHeight > 0 then
  begin
    LHeight := LHeight * LFace.height / LElement.offsetHeight;
  end;
  for LIndex := Low(FResizeEdges) to High(FResizeEdges) do
  begin

    if FResizeEdges[LIndex] = nil then
    begin
      FResizeEdges[LIndex] := Element('div', 'nyx-resize-preview');
      FResizeEdges[LIndex].setAttribute('aria-hidden', 'true');

      if LIndex < 4 then
      begin
        FResizeEdges[LIndex].setAttribute('data-nyx-resize-edge', IntToStr(LIndex));
      end
      else
      begin
        FResizeEdges[LIndex].setAttribute('data-nyx-alignment-guide', IntToStr(LIndex - 4));
      end;
      FResizeEdges[LIndex].style.setProperty('position', 'fixed');
      FResizeEdges[LIndex].style.setProperty('pointer-events', 'none');
      FResizeEdges[LIndex].style.setProperty('z-index', '20');
      FResizeEdges[LIndex].style.setProperty('background', FTheme.Accent);
    end;
    { A surrounding Nyx shell may have moved/remounted the canvas host. Keep
      this exact owned face; no listener or input is recreated by reconnecting. }

    if not document.body.contains(FResizeEdges[LIndex]) then
    begin
      document.body.appendChild(FResizeEdges[LIndex]);
    end;
  end;

  if FDropPreview.Active then
  begin
    LDropFrame := DropFrameFor(FDropPreview.Origin.ID);
    for LIndex := Low(FResizeEdges) to High(FResizeEdges) do
    begin
      FResizeEdges[LIndex].style.setProperty('display', 'none');
      FResizeEdges[LIndex].setAttribute('data-nyx-drop-edge', IntToStr(LIndex));
      LBox := FDropPreview.Segment(LIndex);

      if LBox.Defined and FDropPreview.Frame.SameFrame(LDropFrame) then
      begin
        Edge(LIndex, LFace.left + LBox.Left * LFace.width / LElement.offsetWidth,
          LFace.top + LBox.Top * LFace.height / LElement.offsetHeight,
          LBox.Width * LFace.width / LElement.offsetWidth,
          LBox.Height * LFace.height / LElement.offsetHeight);
      end;
    end;
    Exit;
  end;
  for LIndex := Low(FResizeEdges) to High(FResizeEdges) do
  begin
    FResizeEdges[LIndex].removeAttribute('data-nyx-drop-edge');
  end;
  { A copied position proposal uses the same bounded, pointer-transparent paint.
    Translate the measured face without relocating the live editable control. }
  LLeft := LFace.left + FResizePreview.OffsetX * LFace.width / Max(1, LElement.offsetWidth);
  LTop := LFace.top + FResizePreview.OffsetY * LFace.height / Max(1, LElement.offsetHeight);
  Edge(0, LLeft - 2, LTop - 2, LWidth + 4, 2);
  Edge(1, LLeft - 2, LTop + LHeight, LWidth + 4, 2);
  Edge(2, LLeft - 2, LTop, 2, LHeight);
  Edge(3, LLeft + LWidth, LTop, 2, LHeight);
  Guide(4, FResizePreview.Size.WidthGuide, 0);
  Guide(5, FResizePreview.Size.WidthGuide, 1);
  Guide(6, FResizePreview.Size.HeightGuide, 0);
  Guide(7, FResizePreview.Size.HeightGuide, 1);
end;

procedure TNyxBrowserBinding.SplitChanged(ASplit: TNyxBrowserSplit);
var
  LGeneration: TBrowserViewGeneration;
  LGenerationLease: IInterface;
begin
  try
    LGeneration := TBrowserViewGeneration(FGenerationObject);
    LGenerationLease := LGeneration;

    if (LGeneration = nil) or not LGeneration.Current or FDisconnected then
    begin
      Exit;
    end;
    CurrentRenderer.Emit(FNode, NyxSplitChange(FNode, ASplit.State.Position));
  except
    on E: ENyxBrowserViewRetired do
    begin
      Exit;
    end;
  end;
end;

function TNyxBrowserBinding.AdmitTabs(const AChange: TNyxTabTransition): Boolean;
var
  LGeneration: TBrowserViewGeneration;
  LGenerationLease: IInterface;
begin
  try
    LGeneration := TBrowserViewGeneration(FGenerationObject);
    LGenerationLease := LGeneration;

    if (LGeneration = nil) or not LGeneration.Current or FDisconnected then
    begin
      Exit(False);
    end;
    Result := False;
    FHasTabDispatch := False;

    if CurrentRenderer.FUpdating or (AChange.AfterSelection < 0) then
    begin
      Exit;
    end;

    if CurrentRenderer.FDesignMode then
    begin
      { Designers can inspect retained pages locally without executing a bound
        application command or changing authored/runtime state. }
      Exit(True);
    end;
    try
      { Edit owns candidate validation/publication. Source identities remain local
        to this instance; no accepted host index changes before this call. }
      FTabDispatch := CurrentRenderer.FLiveBindings.Edit(FNode,
        NyxTabValue(FTabs.State.Entry(AChange.AfterSelection).SourceReference));
      FHasTabDispatch := True;
      CurrentRenderer.FLastBindingError := '';
      CurrentRenderer.FLastBindingFailure := nbfNone;
      Result := True;
    except
      on LException: ENyxStateNotification do
      begin

        if LGeneration.Current then
        begin
          CurrentRenderer.BindingFailed(FNode, LException.Message, nbfNotificationFailed);
        end;
      end;
      on LException: Exception do
      begin
        { The host has proposed values only. Restoring every text input would
          discard unrelated unfinished drafts although no physical edit occurred. }

        if LGeneration.Current then
        begin
          CurrentRenderer.BindingFailed(FNode, LException.Message, nbfRejected, False);
        end;
      end;
    end;
  except
    on E: ENyxBrowserViewRetired do
    begin
      Exit(False);
    end;
  end;
end;

procedure TNyxBrowserBinding.TabsChanged(const AChange: TNyxTabTransition);
var
  LGeneration: TBrowserViewGeneration;
  LGenerationLease: IInterface;
  LDispatch: TNyxDispatch;
begin
  try
    LGeneration := TBrowserViewGeneration(FGenerationObject);
    LGenerationLease := LGeneration;

    if (LGeneration = nil) or not LGeneration.Current or FDisconnected then
    begin
      Exit;
    end;

    if FHasTabDispatch then
    begin
      LDispatch := FTabDispatch;
      FHasTabDispatch := False;
      FTabDispatch := Default(TNyxDispatch);
      { Last operation: a receiver may replace the view and retire this binding. }
      CurrentRenderer.Emit(FNode, LDispatch);
    end;
  except
    on E: ENyxBrowserViewRetired do
    begin
      Exit;
    end;
  end;
end;

function TNyxBrowserBinding.Click(AEvent: TJSMouseEvent): Boolean;
var
  LGeneration: TBrowserViewGeneration;
  LGenerationLease: IInterface;
  LDispatch: TNyxDispatch;
  LEvents: INyxEvents;
  LLiveBindings: TNyxLiveBindings;
  LRevision: Integer;
  LSubscribers: Boolean;
  LEditable: Boolean;
  LColumnActivation: INyxBrowserCollectionColumnActivation;
  LColumnCommand: INyxBrowserColumnCommand;
begin
  try
    LGeneration := TBrowserViewGeneration(FGenerationObject);
    LGenerationLease := LGeneration;

    if (LGeneration = nil) or not LGeneration.Current or FDisconnected then
    begin
      Exit(True);
    end;
    Result := True;

    if not CurrentRenderer.FDesignMode and
      Supports(FCollectionMount, INyxBrowserCollectionColumnActivation, LColumnActivation) then
    begin
      LColumnCommand := LColumnActivation.BeginColumnClick(AEvent);

      if LColumnCommand <> nil then
      begin
        Exit(ColumnClick(AEvent, LColumnCommand));
      end;
    end;

    if CurrentRenderer.FUpdating then
    begin
      Exit;
    end;
    LEvents := CurrentRenderer.FEvents;
    LLiveBindings := CurrentRenderer.FLiveBindings;
    LRevision := FViewRevision;
    LEditable := FInput <> nil;

    if (LEvents.ViewRevision <> LRevision) or not LGeneration.Current then
    begin
      Exit;
    end;

    if CurrentRenderer.FDesignMode then
    begin
      AEvent.stopPropagation;
      { Selecting an editable control preserves native focus and interaction.
        Commands and links still select without executing application behavior. }

      if FInput = nil then
      begin
        AEvent.preventDefault;
      end;

      if Assigned(CurrentRenderer.OnEvent) then
      begin
        CurrentRenderer.OnEvent(FNode, NyxDesignEvent(FNode, ntDesignSelect));
      end;
      Exit(LEditable);
    end;

    { Runtime clicks route once from their nearest projected control, matching
      native non-bubbling slots. Wrapped inputs retain their native default action. }
    AEvent.stopPropagation;

    LSubscribers := LEvents.HasSubscribers(ntClick);

    if not LGeneration.Current then
    begin
      Exit;
    end;

    if NyxHasLegacyClick(FNode) or LSubscribers or
      (FNode.Prop(NyxAttributeName(atAction)) <> '') then
    begin
      try
        LDispatch := LLiveBindings.Dispatch(FNode, ntClick);

        if not LGeneration.Current then
        begin
          Exit;
        end;
        CurrentRenderer.FLastBindingError := '';
        CurrentRenderer.FLastBindingFailure := nbfNone;
      except
        on LException: ENyxStateNotification do
        begin

          if LGeneration.Current then
          begin
            CurrentRenderer.BindingFailed(FNode, LException.Message, nbfNotificationFailed);
          end;
          Exit(False);
        end;
        on LException: Exception do
        begin

          if LGeneration.Current then
          begin
            CurrentRenderer.BindingFailed(FNode, LException.Message, nbfRejected,
              not NyxIsDisclosureHeader(FNode));
          end;
          Exit(False);
        end;
      end;

      CurrentRenderer.Emit(FNode, LDispatch);
    end;
  except
    on E: ENyxBrowserViewRetired do
    begin
      Exit(True);
    end;
  end;
end;

function TNyxBrowserBinding.ColumnClick(AEvent: TEventListenerEvent;
  const ACommand: INyxBrowserColumnCommand; APrepared: Boolean): Boolean;
var
  LGeneration: TBrowserViewGeneration;
  LGenerationLease: IInterface;
  LOrigin: TNyxNode;
  LEvents: INyxEvents;
  LLiveBindings: TNyxLiveBindings;
  LRevision: Integer;
  LDispatch: TNyxDispatch;
  LBlocked: Boolean;

  function Current: Boolean;
  begin
    Result := LGeneration.Current and (LEvents.ViewRevision = LRevision) and
      ACommand.DispatchCurrent;
  end;

begin
  Result := False;
  LGeneration := TBrowserViewGeneration(FGenerationObject);
  LGenerationLease := LGeneration;
  try

    if (LGeneration = nil) or not LGeneration.Current or FDisconnected then
    begin
      Exit(True);
    end;
    LOrigin := FNode;
    LEvents := CurrentRenderer.FEvents;
    LLiveBindings := CurrentRenderer.FLiveBindings;
    LRevision := FViewRevision;
    LBlocked := CurrentRenderer.FUpdating or CurrentRenderer.FPublishingContent or
      CurrentRenderer.FDesignMode or AEvent.defaultPrevented or
      not NyxInteractionPolicy(LOrigin).CanIssueCommand;
    LOrigin.AcquireReference;
    try
      try

        if LBlocked or not Current then
        begin
          Exit;
        end;

        if not APrepared then
        begin

          if not ACommand.Current then
          begin
            Exit;
          end;
          ACommand.Prepare;
        end;

        if not Current then
        begin
          Exit;
        end;
        { Dispatch is intentional: Click may execute authored behavior and one
          atomic state admission. A notification failure remains committed;
          neither Signal nor replay can substitute for this command. The entered
          generation retains the original raw coordinator/tree/state throughout. }
        LDispatch := LLiveBindings.Dispatch(LOrigin, ntClick);

        if not Current then
        begin
          Exit;
        end;
        CurrentRenderer.FLastBindingError := '';
        CurrentRenderer.FLastBindingFailure := nbfNone;
        CurrentRenderer.Emit(LOrigin, LDispatch);

        if Current then
        begin
          ACommand.Complete(not AEvent.defaultPrevented);
        end;
      except
        on LException: ENyxStateNotification do
        begin

          if Current then
          begin
            CurrentRenderer.BindingFailed(LOrigin, LException.Message, nbfNotificationFailed);
          end;
        end;
        on LException: ENyxBrowserViewRetired do
        begin
          { Navigation is terminal for this input, without a successor error. }
        end;
        on LException: Exception do
        begin

          if Current then
          begin
            CurrentRenderer.BindingFailed(LOrigin, LException.Message, nbfRejected);
          end;
        end;
      end;
    finally
      LOrigin.ReleaseReference;
    end;
  finally
    { Same-host listeners still run after stopPropagation. Claim cancellation,
      refusal, completion and exceptions before they can recapture the input. }
    AEvent.preventDefault;
    AEvent.stopPropagation;
    ACommand.Retire;
  end;
end;

function TNyxBrowserRenderer.TextSelectionFor(const AID: TNyxText): TNyxTextSelection;
begin
  FEvents.Scheduler.RequireUI;
  Result := CaptureNyxBrowserSelection(InputFor(AID));
end;

function TNyxBrowserRenderer.EditingFor(const AID: TNyxText): TNyxEditingSnapshot;
var
  LInput: TJSHTMLElement;
  LIndex: Integer;
begin
  FEvents.Scheduler.RequireUI;
  LInput := InputFor(AID);
  for LIndex := 0 to High(FBindings) do
  begin

    if FBindings[LIndex].FInput = LInput then
    begin
      Exit(CaptureNyxBrowserEditing(LInput, nepObservation, nil,
        FBindings[LIndex].FComposing));
    end;
  end;
  raise ENyxModel.Create('The mounted component has no text editing context');
end;

procedure TNyxBrowserRenderer.SetTextSelection(const AID: TNyxText;
  const ASelection: TNyxTextSelection);
begin
  FEvents.Scheduler.RequireUI;
  SelectNyxBrowserText(InputFor(AID), ASelection);
end;

procedure TNyxBrowserRenderer.BindTextHistory(const AID: TNyxText;
  const ACommands: INyxTextHistoryCommands);
var
  LInput: TJSHTMLElement;
  LIndex: Integer;
begin
  FEvents.Scheduler.RequireUI;
  LInput := InputFor(AID);
  for LIndex := 0 to High(FBindings) do
  begin

    if (FBindings[LIndex].FInput = LInput) and
      NyxSupportsTextInput(FBindings[LIndex].FNode) then
    begin
      FBindings[LIndex].FHistory := ACommands;
      Exit;
    end;
  end;
  raise ENyxModel.Create('Text history requires a current text input');
end;

function TNyxBrowserBinding.EditingEvent(AEvent: TEventListenerEvent;
  ATrigger: TNyxTrigger; APhase: TNyxEditingPhase): Boolean;
var
  LGeneration: TBrowserViewGeneration;
  LGenerationLease: IInterface;
  LEvents: INyxEvents;
  LRevision: Integer;
  LDispatch: TNyxDispatch;
begin
  try
    LGeneration := TBrowserViewGeneration(FGenerationObject);
    LGenerationLease := LGeneration;

    if (LGeneration = nil) or not LGeneration.Current or FDisconnected then
    begin
      Exit(True);
    end;
    Result := True;
    LEvents := CurrentRenderer.FEvents;
    LRevision := FViewRevision;

    if ((LEvents.ViewRevision <> LRevision) or not LGeneration.Current) or CurrentRenderer.FUpdating or
      CurrentRenderer.FDesignMode or not NyxSupportsTextInput(FNode) or
      not NyxInteractionPolicy(FNode).CanIssueCommand then
    begin
      Exit;
    end;
    LDispatch := CurrentRenderer.FLiveBindings.Signal(FNode, ATrigger);
    LDispatch.Info.HasEditing := True;
    LDispatch.Info.Editing := CaptureNyxBrowserEditing(FInput, APhase, AEvent, FComposing);
    { The router lease is all that is read after Emit: a callback may unmount,
      navigate or dispose both renderer and this borrowed producer. }
    CurrentRenderer.Emit(FNode, LDispatch);
    Result := (LEvents.ViewRevision = LRevision) and LGeneration.Current;
  except
    on E: ENyxBrowserViewRetired do
    begin
      Exit(True);
    end;
  end;
end;

function TNyxBrowserBinding.BeforeEdit(AEvent: TEventListenerEvent): Boolean;
var
  LGeneration: TBrowserViewGeneration;
  LGenerationLease: IInterface;
  LEvents: INyxEvents;
  LRevision: Integer;
  LDispatch: TNyxDispatch;
  LConsumed: Boolean;
  LHistory: INyxTextHistoryCommands;
  LIntent: TNyxEditIntent;
begin
  try
    LGeneration := TBrowserViewGeneration(FGenerationObject);
    LGenerationLease := LGeneration;

    if (LGeneration = nil) or not LGeneration.Current or FDisconnected then
    begin
      Exit(True);
    end;
    Result := True;
    LEvents := CurrentRenderer.FEvents;
    LRevision := FViewRevision;

    if ((LEvents.ViewRevision <> LRevision) or not LGeneration.Current) or CurrentRenderer.FUpdating or
      CurrentRenderer.FDesignMode or not NyxSupportsTextInput(FNode) or
      not NyxInteractionPolicy(FNode).CanEditValue or
      AEvent.defaultPrevented then
    begin
      Exit;
    end;
    LDispatch := CurrentRenderer.FLiveBindings.Signal(FNode, ntBeforeEdit);
    LDispatch.Info.HasEditing := True;
    LDispatch.Info.Editing := CaptureNyxBrowserEditing(FInput, nepBeforeEdit, AEvent, FComposing);
    LIntent := LDispatch.Info.Editing.Intent;
    LHistory := FHistory;
    LConsumed := False;

    if LHistory <> nil then
    begin
      LHistory.BeforeEdit(LDispatch.Info.Editing);
    end;

    if ((LEvents.ViewRevision = LRevision) and LGeneration.Current) and LEvents.HasSubscribers(ntBeforeEdit) then
    begin
      LConsumed := DispatchNyxInput(LEvents, FNode, LDispatch);
    end;

    if not LConsumed and ((LEvents.ViewRevision = LRevision) and LGeneration.Current) and
      (LHistory <> nil) and LDispatch.Info.Editing.CanCancel and
      not LDispatch.Info.Editing.Composing and
      (LIntent in [neiHistoryUndo, neiHistoryRedo]) then
    begin
      { Cancel first: a receiver may retire its input while handling the command.
        Noncancelable history waits for the actual input notification below. }
      AEvent.preventDefault;
      LConsumed := True;

      if LIntent = neiHistoryUndo then
      begin
        LHistory.Execute(nhcUndo);
      end
      else
      begin
        LHistory.Execute(nhcRedo);
      end;
    end;

    if LConsumed and LDispatch.Info.Editing.CanCancel then
    begin
      AEvent.preventDefault;
      Result := False;
    end;
    { A navigation invalidates this producer even when no physical cancellation
      window exists. Never inspect the freed binding after synchronous dispatch. }
    Result := Result and ((LEvents.ViewRevision = LRevision) and LGeneration.Current);
  except
    on E: ENyxBrowserViewRetired do
    begin
      Exit(True);
    end;
  end;
end;

function TNyxBrowserBinding.CompositionStart(AEvent: TEventListenerEvent): Boolean;
var
  LGeneration: TBrowserViewGeneration;
  LGenerationLease: IInterface;
begin
  try
    LGeneration := TBrowserViewGeneration(FGenerationObject);
    LGenerationLease := LGeneration;

    if (LGeneration = nil) or not LGeneration.Current or FDisconnected then
    begin
      Exit(True);
    end;
    Result := True;

    if CurrentRenderer.FUpdating or CurrentRenderer.FDesignMode or
      not NyxSupportsTextInput(FNode) or not NyxInteractionPolicy(FNode).CanEditValue then
    begin
      Exit;
    end;
    FComposing := True;
    Result := EditingEvent(AEvent, ntCompositionStart, nepCompositionStart);
  except
    on E: ENyxBrowserViewRetired do
    begin
      Exit(True);
    end;
  end;
end;

function TNyxBrowserBinding.CompositionUpdate(AEvent: TEventListenerEvent): Boolean;
var
  LGeneration: TBrowserViewGeneration;
  LGenerationLease: IInterface;
begin
  try
    LGeneration := TBrowserViewGeneration(FGenerationObject);
    LGenerationLease := LGeneration;

    if (LGeneration = nil) or not LGeneration.Current or FDisconnected then
    begin
      Exit(True);
    end;
    Result := True;

    if not FComposing then
    begin
      Exit;
    end;
    Result := EditingEvent(AEvent, ntCompositionUpdate, nepCompositionUpdate);
  except
    on E: ENyxBrowserViewRetired do
    begin
      Exit(True);
    end;
  end;
end;

function TNyxBrowserBinding.CompositionEnd(AEvent: TEventListenerEvent): Boolean;
var
  LGeneration: TBrowserViewGeneration;
  LGenerationLease: IInterface;
  LEvents: INyxEvents;
  LRevision: Integer;
  LEditing: TNyxEditingSnapshot;
  LDispatch: TNyxDispatch;
begin
  try
    LGeneration := TBrowserViewGeneration(FGenerationObject);
    LGenerationLease := LGeneration;

    if (LGeneration = nil) or not LGeneration.Current or FDisconnected then
    begin
      Exit(True);
    end;
    Result := True;

    if not FComposing or CurrentRenderer.FUpdating or CurrentRenderer.FDesignMode then
    begin
      Exit;
    end;
    LEvents := CurrentRenderer.FEvents;
    LRevision := FViewRevision;
    LEditing := CaptureNyxBrowserEditing(FInput, nepCompositionEnd, AEvent, False);
    FComposing := False;
    { Capture the physical result first. Final model admission may deliberately
      reject it or a callback may update accepted state; neither changes the
      original owned IME result. Admission emits at most one accepted command. }
    Change(AEvent);

    if (LEvents.ViewRevision <> LRevision) or not LGeneration.Current then
    begin
      Exit(False);
    end;
    LDispatch := CurrentRenderer.FLiveBindings.Signal(FNode, ntCompositionEnd);
    LDispatch.Info.HasEditing := True;
    LDispatch.Info.Editing := LEditing;
    CurrentRenderer.QueueContent;
    CurrentRenderer.Emit(FNode, LDispatch);
    Result := (LEvents.ViewRevision = LRevision) and LGeneration.Current;
  except
    on E: ENyxBrowserViewRetired do
    begin
      Exit(True);
    end;
  end;
end;

function TNyxBrowserBinding.TextSelectionChanged(AEvent: TEventListenerEvent): Boolean;
var
  LGeneration: TBrowserViewGeneration;
  LGenerationLease: IInterface;
  LSelection: TNyxTextSelection;
begin
  try
    LGeneration := TBrowserViewGeneration(FGenerationObject);
    LGenerationLease := LGeneration;

    if (LGeneration = nil) or not LGeneration.Current or FDisconnected then
    begin
      Exit(True);
    end;
    Result := True;

    if CurrentRenderer.FUpdating or CurrentRenderer.FDesignMode or not NyxSupportsTextInput(FNode) then
    begin
      Exit;
    end;
    LSelection := CaptureNyxBrowserSelection(FInput);

    if not LSelection.Defined or FEditingSelection.SameRange(LSelection) then
    begin
      Exit;
    end;
    FEditingSelection := LSelection;
    Result := EditingEvent(AEvent, ntTextSelectionChange, nepSelectionChange);
  except
    on E: ENyxBrowserViewRetired do
    begin
      Exit(True);
    end;
  end;
end;

procedure TNyxBrowserBinding.ColorChanged(const AValue: TNyxRGBColor);
var
  LGeneration: TBrowserViewGeneration;
  LGenerationLease: IInterface;
begin
  try
    LGeneration := TBrowserViewGeneration(FGenerationObject);
    LGenerationLease := LGeneration;

    if (LGeneration = nil) or not LGeneration.Current or FDisconnected then
    begin
      Exit;
    end;
    TJSHTMLInputElement(FInput).value := AValue.ToText;
    Change(nil);
  except
    on E: ENyxBrowserViewRetired do
    begin
      Exit;
    end;
  end;
end;

function TNyxBrowserBinding.Change(AEvent: TEventListenerEvent): Boolean;
var
  LGeneration: TBrowserViewGeneration;
  LGenerationLease: IInterface;
  LValue: TNyxText;
  LDispatch: TNyxDispatch;
  LProposal: TNyxDispatch;
  LEvents: INyxEvents;
  LRevision: Integer;
  LConsumed: Boolean;
  LEditing: TNyxEditingSnapshot;
  LHistory: INyxTextHistoryCommands;
begin
  try
    LGeneration := TBrowserViewGeneration(FGenerationObject);
    LGenerationLease := LGeneration;

    if (LGeneration = nil) or not LGeneration.Current or FDisconnected then
    begin
      Exit(True);
    end;
    Result := True;

    if CurrentRenderer.FUpdating then
    begin
      Exit;
    end;

    if (FColorField <> nil) and (AEvent <> nil) and (AEvent._type = 'input') then
    begin
      { Hex text remains a draft until completion, including empty/pasted/IME
        input. No native color sanitizer substitutes black during typing. }
      Exit;
    end;

    if (AEvent <> nil) and (AEvent._type = 'input') and
      not NyxSupportsTextInput(FNode) then
    begin
      { A format/domain transition must not turn an existing input listener into
        per-keystroke numeric admission. Physical change retains commit semantics. }
      Exit;
    end;

    LEditing := Default(TNyxEditingSnapshot);

    if NyxSupportsTextInput(FNode) then
    begin
      LEditing := CaptureNyxBrowserEditing(FInput, nepInput, AEvent, FComposing);

      if LEditing.Composing then
      begin
        { IME owns its live draft. Model admission and normalization wait until
          compositionend; neither an input listener nor Sync rewrites that draft. }
        FComposing := True;
        Exit;
      end;
    end;

    if (FNode.ProjectionKind = 'slider') and not FCustom then
    begin
      LValue := FSliderValue.ReadPosition(StrToIntDef(TJSHTMLInputElement(FInput).value, -1));
    end
    else if FInput is TJSHTMLTextAreaElement then
    begin
      LValue := TJSHTMLTextAreaElement(FInput).value;
    end
    else if FInput is TJSHTMLSelectElement then
    begin
      LValue := TJSHTMLSelectElement(FInput).value;
    end
    else if (FNode.ProjectionKind = 'checkbox') or (FNode.ProjectionKind = 'switch') or
      (FNode.ProjectionKind = 'radio') then
    begin
      LValue := 'false';

      if TJSHTMLInputElement(FInput).checked then
      begin
        LValue := 'true';
      end;
    end
    else
    begin
      LValue := TJSHTMLInputElement(FInput).value;
    end;

    if not NyxInteractionPolicy(FNode).CanEditValue then
    begin
      { A platform without a read-only selector can still emit a physical draft.
        Restore it before proposal callbacks or model commands. Keep True so
        keyboard preflight can still deliver shortcuts against accepted text. }

      if LValue <> FNode.Prop('value') then
      begin
        CurrentRenderer.FForceValues := True;
        try
          CurrentRenderer.Sync;
        finally

          if LGeneration.Current then
          begin
            CurrentRenderer.FForceValues := False;
          end;
        end;
      end;
      Exit;
    end;

    if CurrentRenderer.FDesignMode then
    begin
      FNode.SetProp('value', LValue);
      { Send the actual field, rather than a compound's semantic event root. The
        host persists an authored value or independent reusable-part override. }

      if Assigned(CurrentRenderer.OnEvent) then
      begin
        CurrentRenderer.OnEvent(FNode, NyxDesignEvent(FNode, ntDesignValue));
      end;
      Exit;
    end;
    LHistory := FHistory;

    if (LHistory <> nil) and LEditing.Defined and
      (LEditing.Intent in [neiHistoryUndo, neiHistoryRedo]) then
    begin
      { Some browser/OS producers cannot cancel beforeinput. Never admit the
        platform stack's value into bindings/history: the owning timeline writes
        its exact result back through this retained input instead. }

      if LEditing.Intent = neiHistoryUndo then
      begin
        LHistory.Execute(nhcUndo);
      end
      else
      begin
        LHistory.Execute(nhcRedo);
      end;
      Exit(False);
    end;
    { A platform change event with the already accepted value carries no edit. }

    if LValue = FNode.Prop('value') then
    begin
      Exit;
    end;
    LEvents := CurrentRenderer.FEvents;
    LRevision := LEvents.ViewRevision;

    if NyxSupportsTextInput(FNode) and LEvents.HasSubscribers(ntBeforeTextInput) then
    begin
      LProposal := CurrentRenderer.FLiveBindings.ProposeText(FNode, LValue);
      LProposal.Info.HasEditing := LEditing.Defined;
      LProposal.Info.Editing := LEditing;
      LConsumed := DispatchNyxInput(LEvents, FNode, LProposal);

      if (LEvents.ViewRevision <> LRevision) or not LGeneration.Current then
      begin
        Exit(False);
      end;

      if LConsumed then
      begin
        { The physical edit is a proposal. Restore the latest accepted value in
          place, without a new history/state command or a spurious error dialog. }
        CurrentRenderer.FForceValues := True;
        try
          CurrentRenderer.Sync;
        finally

          if LGeneration.Current then
          begin
            CurrentRenderer.FForceValues := False;
          end;
        end;
        LProposal.Info.DefaultPrevented := True;
        DispatchNyxTextResult(LEvents, FNode, LProposal,
          @LGeneration.ReadSignalSnapshot);
        Exit(False);
      end;
    end;
    try
      LDispatch := CurrentRenderer.FLiveBindings.Edit(FNode, LValue);

      if not LGeneration.Current then
      begin
        Exit(True);
      end;
      LDispatch.Info.HasEditing := LEditing.Defined;
      LDispatch.Info.Editing := LEditing;
      CurrentRenderer.FLastBindingError := '';
      CurrentRenderer.FLastBindingFailure := nbfNone;
    except
      on LException: ENyxStateNotification do
      begin

        if LGeneration.Current then
        begin
          CurrentRenderer.BindingFailed(FNode, LException.Message, nbfNotificationFailed);
        end;
        Exit(False);
      end;
      on LException: Exception do
      begin

        if LGeneration.Current then
        begin
          CurrentRenderer.BindingFailed(FNode, LException.Message);
        end;
        Exit(False);
      end;
    end;

    CurrentRenderer.Emit(FNode, LDispatch);
  except
    on E: ENyxBrowserViewRetired do
    begin
      Exit(True);
    end;
  end;
end;

function TNyxBrowserRenderer.EmitNamed(const AOriginID: TNyxText;
  const AName: TNyxEventRef; const APayload: TNyxDataValue;
  AHasPayload: Boolean): Boolean;
var
  LIndex: Integer;
  LDispatch: TNyxDispatch;
begin
  Result := False;

  if FDesignMode or FUpdating or FPublishingContent then
  begin
    Exit;
  end;
  for LIndex := 0 to Length(FBindings) - 1 do
  begin

    if FBindings[LIndex].FNode.ID = AOriginID then
    begin
      LDispatch := DispatchNyxNamedEvent(FBindings[LIndex].FNode, AName,
        APayload, AHasPayload, npfBrowser);
      Result := LDispatch.EventName <> '';

      if Result then
      begin
        Emit(FBindings[LIndex].FNode, LDispatch);
      end;
      { A callback may dispose this renderer. Do not read fields after Emit. }
      Exit;
    end;
  end;
  raise ENyxModel.Create('Named event origin is not mounted: ' + AOriginID);
end;

procedure TNyxBrowserBinding.CollectionSelectionChanged(
  const ABefore, AAfter: INyxCollectionSelection);
var
  LGeneration: TBrowserViewGeneration;
  LGenerationLease: IInterface;
  LDispatch: TNyxDispatch;
  LEvents: INyxEvents;
begin
  try
    LGeneration := TBrowserViewGeneration(FGenerationObject);
    LGenerationLease := LGeneration;

    if (LGeneration = nil) or not LGeneration.Current or FDisconnected then
    begin
      Exit;
    end;

    if CurrentRenderer.FUpdating or CurrentRenderer.FDesignMode then
    begin
      Exit;
    end;
    LEvents := CurrentRenderer.FEvents;

    if LEvents.ViewRevision <> FViewRevision then
    begin
      Exit;
    end;
    LDispatch := CurrentRenderer.FLiveBindings.Signal(FNode, ntSelectionChange);
    LDispatch.Info.HasCollectionSelection := True;
    LDispatch.Info.SelectionBefore := ABefore.Snapshot;
    LDispatch.Info.Selection := AAfter.Snapshot;
    { Last borrowed call. Managed snapshots survive navigation/destruction. }
    CurrentRenderer.Emit(FNode, LDispatch);
  except
    on E: ENyxBrowserViewRetired do
    begin
      Exit;
    end;
  end;
end;

procedure TNyxBrowserRenderer.Emit(AOrigin: TNyxNode; const ADispatch: TNyxDispatch);
var
  LGeneration: TBrowserViewGeneration;
  LGenerationLease: IInterface;
  LCoordinators: TNyxCoordinatorMounts;
  LEvents: INyxEvents;
  LInfo: TNyxEventInfo;
  LLegacy: TNyxBrowserEvent;
  LRevision: Integer;
  LLegacyClick: Boolean;
  LContentGuard: INyxContentGuard;
  LIndex: Integer;
  LPager: INyxCollectionPager;
begin
  try
  LGeneration := TBrowserViewGeneration(FGenerationObject);
  LGenerationLease := FGeneration;

  if (LGeneration = nil) or not LGeneration.Current then
  begin
    Exit;
  end;

  if FUpdating or FPublishingContent or FDesignMode or (ADispatch.EventName = '') then
  begin
    Exit;
  end;
  LEvents := FEvents;
  LLegacy := FOnEvent;
  LLegacyClick := NyxHasLegacyClick(AOrigin);
  LCoordinators := FCoordinatorMounts;
  LContentGuard := FContentGuard;
  LContentGuard.Enter;
  AOrigin.AcquireReference;
  ADispatch.Source.AcquireReference;
  try
    { Capture dependencies before callbacks: navigation may dispose this view.
      Managed callbacks own only snapshots. The legacy method still borrows its
      receiver, which applications keep alive for the synchronous callback. }
    for LIndex := 0 to High(FBindings) do
    begin

      if FBindings[LIndex].FNode = ADispatch.Source then
      begin
        LPager := FBindings[LIndex].FCollectionPager;
        Break;
      end;
    end;
    LRevision := LEvents.ViewRevision;

    if not LGeneration.Current then
    begin
      Exit;
    end;

    if LPager <> nil then
    begin
      LPager.ApplyEvent(ADispatch.Info);

      if (LEvents.ViewRevision <> LRevision) or not LGeneration.Current then
      begin
        Exit;
      end;
    end;
    { Freeze the selected record before the router captures queued callbacks. }
    LInfo := EnrichNyxCoordinatorEvent(LCoordinators, ADispatch.Info);

    if (LEvents.ViewRevision <> LRevision) or not LGeneration.Current then
    begin
      Exit;
    end;
    LEvents.Dispatch(LInfo, AOrigin.DesignID, ADispatch.Source.DesignID);

    if ADispatch.Info.HasTextEdit and (LEvents.ViewRevision = LRevision) and
      LGeneration.Current then
    begin
      DispatchNyxTextResult(LEvents, AOrigin, ADispatch, @LGeneration.ReadSignalSnapshot);
    end;

    if Assigned(LLegacy) and (ADispatch.Info.Trigger in [ntClick, ntChange, ntNamed]) and
      ((ADispatch.Info.Trigger <> ntClick) or LLegacyClick) and
      (LEvents.ViewRevision = LRevision) and LGeneration.Current then
    begin
      LLegacy(ADispatch.Source, LInfo);
    end;
  finally
    ADispatch.Source.ReleaseReference;
    AOrigin.ReleaseReference;
    LContentGuard.Leave;
  end;
  except
    on E: ENyxBrowserViewRetired do
    begin
      { Later text/event phases lose authority with the original generation. }
    end;
  end;
end;

function TNyxBrowserBinding.ColumnDoubleClick(AEvent: TJSMouseEvent;
  const ACommand: INyxBrowserColumnCommand): Boolean;
var
  LGeneration: TBrowserViewGeneration;
  LGenerationLease: IInterface;
  LEvents: INyxEvents;
  LRevision, LObserved, LIndex: Integer;
  LOrigin, LSource: TNyxNode;
  LLiveBindings: TNyxLiveBindings;
  LRenderer: TNyxBrowserRenderer;
  LElement: TJSHTMLElement;
  LDispatch: TNyxDispatch;
  LInfo: TNyxEventInfo;
  LPointer: TNyxPointerSnapshot;
  LTransform: TBrowserFaceTransform;
  LPoint: TNyxResizePoint;
  LContentGuard: INyxContentGuard;
  LCoordinators: TNyxCoordinatorMounts;
  LPager: INyxCollectionPager;
  LBlocked, LSubscribers: Boolean;

  function Current: Boolean;
  begin
    { A real router getter may run application code. Only managed locals are
      read until the entry proof has the final say about borrowed-tree access. }
    LObserved := LEvents.ViewRevision;
    Result := (LObserved = LRevision) and ACommand.DispatchCurrent;
  end;

begin
  try
    LGeneration := TBrowserViewGeneration(FGenerationObject);
    LGenerationLease := LGeneration;

    if (LGeneration = nil) or not LGeneration.Current or FDisconnected then
    begin
      Exit(True);
    end;
    Result := False;
    LRenderer := CurrentRenderer;
    LEvents := LRenderer.FEvents;
    LRevision := FViewRevision;
    LOrigin := FNode;
    LSource := nil;
    LLiveBindings := LRenderer.FLiveBindings;
    LElement := FElement;
    LContentGuard := LRenderer.FContentGuard;
    LCoordinators := Copy(LRenderer.FCoordinatorMounts, 0,
      Length(LRenderer.FCoordinatorMounts));
    LBlocked := LRenderer.FUpdating or LRenderer.FPublishingContent or
      LRenderer.FDesignMode or AEvent.defaultPrevented or
      not NyxInteractionPolicy(LOrigin).CanIssueCommand;
    { Retain before Prepare or the first real router query can navigate. The
      command retains the mount, not this binding, renderer or realized tree. }
    LOrigin.AcquireReference;
    try
      ACommand.Prepare;

      if LBlocked or not Current then
      begin
        Exit;
      end;
      LSubscribers := LEvents.HasSubscribers(ntDoubleClick);

      if not Current then
      begin
        Exit;
      end;

      if LSubscribers then
      begin
        LDispatch := LLiveBindings.Signal(LOrigin, ntDoubleClick);
        LSource := LDispatch.Source;
        LSource.AcquireReference;
        { Preserve the ordinary pointer payload for the SAME canonical event.
          This bounded route is only the claimed header double-click, so no
          gesture/capture/text-edit/legacy callback default belongs to it. }
        LPointer := LDispatch.Info.Pointer;
        LPointer.Kind := npiMouse;

        if AEvent is TJSPointerEvent then
        begin
          LPointer.ID := TJSPointerEvent(AEvent).pointerId;
          LPointer.Primary := TJSPointerEvent(AEvent).isPrimary;
          LPointer.Pressure := TJSPointerEvent(AEvent).pressure;

          if TJSPointerEvent(AEvent).pointerType = 'touch' then
          begin
            LPointer.Kind := npiTouch;
          end
          else if TJSPointerEvent(AEvent).pointerType = 'pen' then
          begin
            LPointer.Kind := npiPen;
          end;
        end;
        LPointer.HasPosition := True;
        LTransform := TBrowserFaceTransform.Capture(LElement);
        LPoint := LTransform.LogicalPoint(AEvent.clientX, AEvent.clientY);
        LPointer.X := LPoint.X;
        LPointer.Y := LPoint.Y;
        LPointer.Button := npbNone;
        LPointer.Buttons := [];

        if (AEvent.buttons and 1) <> 0 then
        begin
          Include(LPointer.Buttons, npbPrimary);
        end;

        if (AEvent.buttons and 2) <> 0 then
        begin
          Include(LPointer.Buttons, npbSecondary);
        end;

        if (AEvent.buttons and 4) <> 0 then
        begin
          Include(LPointer.Buttons, npbAuxiliary);
        end;

        if (AEvent.buttons and not 7) <> 0 then
        begin
          Include(LPointer.Buttons, npbOther);
        end;
        LPointer.Modifiers := [];

        if AEvent.shiftKey then
        begin
          Include(LPointer.Modifiers, nmShift);
        end;

        if AEvent.ctrlKey then
        begin
          Include(LPointer.Modifiers, nmControl);
        end;

        if AEvent.altKey then
        begin
          Include(LPointer.Modifiers, nmAlt);
        end;

        if AEvent.metaKey then
        begin
          Include(LPointer.Modifiers, nmMeta);
        end;
        LDispatch.Info.HasPointer := True;
        LDispatch.Info.Pointer := LPointer;

        if not Current then
        begin
          Exit;
        end;
        { Copy the optional pager before any outward event work. Enrichment uses
          retained mounts; neither stage consults a renderer after callbacks. }
        for LIndex := 0 to High(LRenderer.FBindings) do
        begin

          if LRenderer.FBindings[LIndex].FNode = LSource then
          begin
            LPager := LRenderer.FBindings[LIndex].FCollectionPager;
            Break;
          end;
        end;
        LContentGuard.Enter;
        try

          if LPager <> nil then
          begin
            LPager.ApplyEvent(LDispatch.Info);

            if not Current then
            begin
              Exit;
            end;
          end;
          LInfo := EnrichNyxCoordinatorEvent(LCoordinators, LDispatch.Info);

          if not Current then
          begin
            Exit;
          end;
          LEvents.Dispatch(LInfo, LOrigin.DesignID, LSource.DesignID);
        finally
          LContentGuard.Leave;
        end;
      end;
      { No-subscriber inputs still perform the default once. Leave may have
        refreshed/navigated, so even that final default uses only the old proof. }

      if Current then
      begin
        ACommand.Complete(not AEvent.defaultPrevented);
      end;
    finally
      { Claim success, refusal AND exception before mount bubbling can recapture
        a newly enabled/replaced column. Only this token's unfinished work retires. }
      AEvent.preventDefault;
      AEvent.stopPropagation;
      ACommand.Retire;
      try

        if LSource <> nil then
        begin
          LSource.ReleaseReference;
        end;
      finally
        LOrigin.ReleaseReference;
      end;
    end;
  except
    on E: ENyxBrowserViewRetired do
    begin
      Exit(True);
    end;
  end;
end;

function TNyxBrowserBinding.DoubleClick(AEvent: TJSMouseEvent): Boolean;
var
  LGeneration: TBrowserViewGeneration;
  LGenerationLease: IInterface;
  LMount: INyxCollectionMount;
  LInput: INyxBrowserCollectionColumnInput;
  LCommand: INyxBrowserColumnCommand;
begin
  try
    LGeneration := TBrowserViewGeneration(FGenerationObject);
    LGenerationLease := LGeneration;

    if (LGeneration = nil) or not LGeneration.Current or FDisconnected then
    begin
      Exit(True);
    end;
    { Begin has no router/source callout: capture the exact entry identity before
      PointerEvent's first real revision/subscriber getter can replace this view. }
    LMount := FCollectionMount;

    if Supports(LMount, INyxBrowserCollectionColumnInput, LInput) then
    begin
      LCommand := LInput.BeginColumnDoubleClick(AEvent);

      if LCommand <> nil then
      begin
        Exit(ColumnDoubleClick(AEvent, LCommand));
      end;
    end;
    Result := PointerEvent(AEvent, ntDoubleClick);
  except
    on E: ENyxBrowserViewRetired do
    begin
      Exit(True);
    end;
  end;
end;

function TNyxBrowserBinding.PointerDown(AEvent: TJSPointerEvent): Boolean;
begin
  Result := RangePointerEvent(AEvent, ntPointerDown, ncrspBegin);
end;

function TNyxBrowserBinding.RangePointerEvent(AEvent: TJSPointerEvent;
  ATrigger: TNyxTrigger; APhase: TNyxCollectionRangeStrokePhase): Boolean;
var
  LGeneration: TBrowserViewGeneration;
  LGenerationLease: IInterface;
  LMount: INyxCollectionMount;
  LInput: INyxBrowserCollectionCellRangePointerInput;
  LStrokeInput: INyxBrowserCollectionCellRangeStrokeInput;
  LCommand: INyxBrowserColumnCommand;
  LEvents: INyxEvents;
  LRevision: Integer;
  LAllowed: Boolean;
  LEventScope: INyxCancellationScope;
begin
  try
    LGeneration := TBrowserViewGeneration(FGenerationObject);
    LGenerationLease := LGeneration;

    if (LGeneration = nil) or not LGeneration.Current or FDisconnected then
    begin
      Exit(True);
    end;
    LMount := FCollectionMount;
    LEvents := CurrentRenderer.FEvents;
    LRevision := FViewRevision;

    if not CurrentRenderer.FDesignMode then
    begin

      if Supports(LMount, INyxBrowserCollectionCellRangeStrokeInput, LStrokeInput) then
      begin
        LEventScope := NyxEventGeneration(LEvents);

        if not LGeneration.Current or LEventScope.Cancelled or
          (LEvents.ViewRevision <> LRevision) then
        begin
          Exit(True);
        end;
        LCommand := LStrokeInput.BeginRangeStroke(AEvent, APhase, LEventScope);
      end
      else if (APhase = ncrspBegin) and
        Supports(LMount, INyxBrowserCollectionCellRangePointerInput, LInput) then
      begin
        LCommand := LInput.BeginRangePointer(AEvent);
      end;

      if LCommand <> nil then
      begin
        try
          LCommand.Prepare;
          LAllowed := False;

          if LGeneration.Current and LCommand.DispatchCurrent and
            (LEvents.ViewRevision = LRevision) then
          begin
            LAllowed := PointerEvent(AEvent, ATrigger, LCommand);
          end;
          LCommand.Complete(LAllowed and LGeneration.Current and
            (LEvents.ViewRevision = LRevision) and not AEvent.defaultPrevented);
        finally
          LCommand.Retire;
          AEvent.preventDefault;
          AEvent.stopPropagation;
        end;
        Exit(False);
      end;
    end;
    Result := PointerEvent(AEvent, ATrigger);
  except
    on E: ENyxBrowserViewRetired do
    begin
      Exit(True);
    end;
  end;
end;

function TNyxBrowserBinding.PointerUp(AEvent: TJSPointerEvent): Boolean;
begin
  Result := RangePointerEvent(AEvent, ntPointerUp, ncrspRelease);
end;

function TNyxBrowserBinding.PointerMove(AEvent: TJSPointerEvent): Boolean;
begin
  Result := RangePointerEvent(AEvent, ntPointerMove, ncrspMove);
end;

function TNyxBrowserBinding.PointerEnter(AEvent: TJSPointerEvent): Boolean;
var
  LGeneration: TBrowserViewGeneration;
  LGenerationLease: IInterface;
begin
  try
    LGeneration := TBrowserViewGeneration(FGenerationObject);
    LGenerationLease := LGeneration;

    if (LGeneration = nil) or not LGeneration.Current or FDisconnected then
    begin
      Exit(True);
    end;
    Result := PointerEvent(AEvent, ntPointerEnter);
  except
    on E: ENyxBrowserViewRetired do
    begin
      Exit(True);
    end;
  end;
end;

function TNyxBrowserBinding.PointerExit(AEvent: TJSPointerEvent): Boolean;
var
  LGeneration: TBrowserViewGeneration;
  LGenerationLease: IInterface;
begin
  try
    LGeneration := TBrowserViewGeneration(FGenerationObject);
    LGenerationLease := LGeneration;

    if (LGeneration = nil) or not LGeneration.Current or FDisconnected then
    begin
      Exit(True);
    end;
    Result := PointerEvent(AEvent, ntPointerExit);
  except
    on E: ENyxBrowserViewRetired do
    begin
      Exit(True);
    end;
  end;
end;

function TNyxBrowserBinding.ContextMenu(AEvent: TJSMouseEvent): Boolean;
var
  LGeneration: TBrowserViewGeneration;
  LGenerationLease: IInterface;
  LMount: INyxCollectionMount;
  LInput: INyxBrowserCollectionCellContextInput;
  LCommand: INyxBrowserColumnCommand;
  LAllowed: Boolean;
begin
  try
    LGeneration := TBrowserViewGeneration(FGenerationObject);
    LGenerationLease := LGeneration;

    if (LGeneration = nil) or not LGeneration.Current or FDisconnected then
    begin
      Exit(True);
    end;
    LMount := FCollectionMount;

    if Supports(LMount, INyxBrowserCollectionCellContextInput, LInput) then
    begin
      LCommand := LInput.BeginCellContext(AEvent);
    end;

    if LCommand = nil then
    begin
      Exit(PointerEvent(AEvent, ntContextMenu));
    end;
    try

      if not AEvent.defaultPrevented and LCommand.Current then
      begin
        LCommand.Prepare;
      end;
      LAllowed := False;

      if LGeneration.Current and LCommand.DispatchCurrent then
      begin
        LAllowed := PointerEvent(AEvent, ntContextMenu);
      end;
      Result := LCommand.Complete(LAllowed and not AEvent.defaultPrevented and
        LCommand.DispatchCurrent);
    finally
      AEvent.preventDefault;
      AEvent.stopPropagation;
      LCommand.Retire;
    end;
  except
    on E: ENyxBrowserViewRetired do
    begin
      Exit(True);
    end;
  end;
end;

procedure TNyxBrowserBinding.ObserveCapture(APointerID: Integer; ACaptured: Boolean);
var
  LIndex: Integer;
  LOther: Integer;
begin
  for LIndex := 0 to High(FCapturedPointers) do
  begin

    if FCapturedPointers[LIndex] = APointerID then
    begin

      if not ACaptured then
      begin
        for LOther := LIndex to High(FCapturedPointers) - 1 do
        begin
          FCapturedPointers[LOther] := FCapturedPointers[LOther + 1];
        end;
        SetLength(FCapturedPointers, Length(FCapturedPointers) - 1);
        FRenderer.QueueContent;
      end;
      Exit;
    end;
  end;

  if ACaptured then
  begin
    SetLength(FCapturedPointers, Length(FCapturedPointers) + 1);
    FCapturedPointers[High(FCapturedPointers)] := APointerID;
  end;
end;

function TNyxBrowserBinding.PointerCancel(AEvent: TJSPointerEvent): Boolean;
var
  LGeneration: TBrowserViewGeneration;
  LGenerationLease: IInterface;
begin
  try
    LGeneration := TBrowserViewGeneration(FGenerationObject);
    LGenerationLease := LGeneration;

    if (LGeneration = nil) or not LGeneration.Current or FDisconnected then
    begin
      Exit(True);
    end;
    Result := PointerEvent(AEvent, ntPointerCancel);
  except
    on E: ENyxBrowserViewRetired do
    begin
      Exit(True);
    end;
  end;
end;

function TNyxBrowserBinding.PointerCapture(AEvent: TJSPointerEvent): Boolean;
var
  LGeneration: TBrowserViewGeneration;
  LGenerationLease: IInterface;
begin
  try
    LGeneration := TBrowserViewGeneration(FGenerationObject);
    LGenerationLease := LGeneration;

    if (LGeneration = nil) or not LGeneration.Current or FDisconnected then
    begin
      Exit(True);
    end;
    ObserveCapture(AEvent.pointerId, True);
    Result := PointerEvent(AEvent, ntPointerCapture);
  except
    on E: ENyxBrowserViewRetired do
    begin
      Exit(True);
    end;
  end;
end;

function TNyxBrowserBinding.PointerCaptureLost(AEvent: TJSPointerEvent): Boolean;
var
  LGeneration: TBrowserViewGeneration;
  LGenerationLease: IInterface;
begin
  try
    LGeneration := TBrowserViewGeneration(FGenerationObject);
    LGenerationLease := LGeneration;

    if (LGeneration = nil) or not LGeneration.Current or FDisconnected then
    begin
      Exit(True);
    end;
    ObserveCapture(AEvent.pointerId, False);
    Result := PointerEvent(AEvent, ntPointerCaptureLost);
  except
    on E: ENyxBrowserViewRetired do
    begin
      Exit(True);
    end;
  end;
end;

function TNyxBrowserBinding.PointerEvent(AEvent: TJSMouseEvent;
  ATrigger: TNyxTrigger; const AManagedInput: INyxBrowserColumnCommand): Boolean;
var
  LGeneration: TBrowserViewGeneration;
  LGenerationLease: IInterface;
  LEvents: INyxEvents;
  LRevision: Integer;
  LDispatch: TNyxDispatch;
  LTransform: TBrowserFaceTransform;
  LPoint: TNyxResizePoint;
  LPointer: TNyxPointerSnapshot;
  LConsumed: Boolean;
  LTarget: TJSHTMLElement;
  LGesture: INyxGestureDecision;
  LResponse: TNyxGestureResult;
  LCapabilities: TNyxGestureCapabilities;
  LElement: TNyxGestureElement;
  LInput: TNyxGestureElement;
  LOriginID: TNyxText;
  LRangeDecision: INyxBrowserRangeCaptureDecision;
begin
  try
    LGeneration := TBrowserViewGeneration(FGenerationObject);
    LGenerationLease := LGeneration;

    if (LGeneration = nil) or not LGeneration.Current or FDisconnected then
    begin
      Exit(True);
    end;
    Result := True;
    LEvents := CurrentRenderer.FEvents;
    LRevision := FViewRevision;

    if ((LEvents.ViewRevision <> LRevision) or not LGeneration.Current) or CurrentRenderer.FUpdating or
      CurrentRenderer.FDesignMode or AEvent.defaultPrevented or
      not LEvents.HasSubscribers(ATrigger) or
      not NyxInteractionPolicy(FNode).CanIssueCommand then
    begin
      Exit;
    end;
    { A framed input belongs to its outer Nyx face. A separately projected child
      belongs to that child's binding; bubbling must not duplicate its event. }
    LTarget := TJSHTMLElement(AEvent.target);

    if not (ATrigger in [ntPointerEnter, ntPointerExit]) and
      (LTarget.closest('[data-runtime-id]') <> FElement) then
    begin
      Exit;
    end;
    LDispatch := CurrentRenderer.FLiveBindings.Signal(FNode, ATrigger);
    LPointer := LDispatch.Info.Pointer;
    LPointer.Kind := npiMouse;

    if AEvent is TJSPointerEvent then
    begin
      LPointer.ID := TJSPointerEvent(AEvent).pointerId;
      LPointer.Primary := TJSPointerEvent(AEvent).isPrimary;
      LPointer.Pressure := TJSPointerEvent(AEvent).pressure;

      if TJSPointerEvent(AEvent).pointerType = 'touch' then
      begin
        LPointer.Kind := npiTouch;
      end
      else if TJSPointerEvent(AEvent).pointerType = 'pen' then
      begin
        LPointer.Kind := npiPen;
      end;
    end;
    LPointer.HasPosition := not ((ATrigger = ntContextMenu) and
      (AEvent.detail = 0) and (AEvent.clientX = 0) and (AEvent.clientY = 0));
    LTransform := TBrowserFaceTransform.Capture(FElement);
    LPoint := LTransform.LogicalPoint(AEvent.clientX, AEvent.clientY);
    LPointer.X := LPoint.X;
    LPointer.Y := LPoint.Y;

    if not LPointer.HasPosition then
    begin
      LPointer.Kind := npiUnknown;
      LPointer.X := 0;
      LPointer.Y := 0;
    end;
    LPointer.Button := npbNone;

    if ATrigger in [ntPointerDown, ntPointerUp] then
    begin
      case AEvent.button of
        0:
          begin
            LPointer.Button := npbPrimary;
          end;
        1:
          begin
            LPointer.Button := npbAuxiliary;
          end;
        2:
          begin
            LPointer.Button := npbSecondary;
          end;
      else
        LPointer.Button := npbOther;
      end;
    end;
    LPointer.Buttons := [];

    if (AEvent.buttons and 1) <> 0 then
    begin
      Include(LPointer.Buttons, npbPrimary);
    end;

    if (AEvent.buttons and 2) <> 0 then
    begin
      Include(LPointer.Buttons, npbSecondary);
    end;

    if (AEvent.buttons and 4) <> 0 then
    begin
      Include(LPointer.Buttons, npbAuxiliary);
    end;

    if (AEvent.buttons and not 7) <> 0 then
    begin
      Include(LPointer.Buttons, npbOther);
    end;
    LPointer.Modifiers := [];

    if AEvent.shiftKey then
    begin
      Include(LPointer.Modifiers, nmShift);
    end;

    if AEvent.ctrlKey then
    begin
      Include(LPointer.Modifiers, nmControl);
    end;

    if AEvent.altKey then
    begin
      Include(LPointer.Modifiers, nmAlt);
    end;

    if AEvent.metaKey then
    begin
      Include(LPointer.Modifiers, nmMeta);
    end;
    LDispatch.Info.HasPointer := True;
    LDispatch.Info.Pointer := LPointer;
    LConsumed := False;
    LElement := TNyxGestureElement(FElement);
    LInput := TNyxGestureElement(FInput);
    LOriginID := FNode.ID;

    if ATrigger = ntContextMenu then
    begin
      LConsumed := DispatchNyxInput(LEvents, FNode, LDispatch);
    end
    else if ATrigger in [ntPointerDown, ntPointerMove, ntPointerUp] then
    begin
      LCapabilities := [];

      if (AEvent is TJSPointerEvent) and (AEvent.buttons <> 0) and
        (ATrigger in [ntPointerDown, ntPointerMove]) then
      begin
        Include(LCapabilities, ngcCapturePointer);
      end;

      if (AEvent is TJSPointerEvent) and
        (LElement.hasPointerCapture(LPointer.ID) or
          ((LInput <> nil) and LInput.hasPointerCapture(LPointer.ID))) then
      begin
        Include(LCapabilities, ngcReleasePointer);
      end;
      LGesture := NewNyxGestureDecision(LCapabilities);

      if AManagedInput <> nil then
      begin
        LConsumed := DispatchNyxGestureInput(LEvents, FNode, LDispatch, LGesture);
      end
      else
      begin
        DispatchNyxGesture(LEvents, FNode, LDispatch, LGesture);
      end;
      LResponse := LGesture.Seal;

      if (AManagedInput <> nil) and AManagedInput.Current and
        LGeneration.Current and (LEvents.ViewRevision = LRevision) and
        Supports(AManagedInput, INyxBrowserRangeCaptureDecision, LRangeDecision) then
      begin
        LRangeDecision.ObservePointerRequest(LResponse.PointerRequest);
      end;

      { The mounted input can retire without replacing its view (for example
        disable/re-enable during the callback). Its sealed capture request has
        no more authority than the range default. Keep the same original token
        through this synchronous bridge, then recheck view flags after it. }
      if ((AManagedInput = nil) or AManagedInput.Current) and
        (LEvents.ViewRevision = LRevision) and LGeneration.Current and
        (LResponse.PointerRequest <> nprUnchanged) then
      begin
        try

          if LResponse.PointerRequest = nprCapture then
          begin
            LElement.setPointerCapture(LPointer.ID);
            ObserveCapture(LPointer.ID, True);
          end
          else
          begin

            if LElement.hasPointerCapture(LPointer.ID) then
            begin
              LElement.releasePointerCapture(LPointer.ID);
            end;

            if (LInput <> nil) and LInput.hasPointerCapture(LPointer.ID) then
            begin
              LInput.releasePointerCapture(LPointer.ID);
            end;
          end;
          RequireCurrent;
          CurrentRenderer.FLastGestureError := '';
        except

          if LGeneration.Current then
          begin
            CurrentRenderer.GestureFailed(LOriginID,
              'Browser refused capture/release for the current active pointer');
          end;
        end;
      end;
    end
    else
    begin
      CurrentRenderer.Emit(FNode, LDispatch);
    end;
    { A callback can unmount the binding. Only the owned router and physical
      event remain safe to inspect after dispatch. }

    if LConsumed or ((LEvents.ViewRevision <> LRevision) or not LGeneration.Current) then
    begin
      AEvent.preventDefault;
      Result := False;
    end;
    AEvent.stopPropagation;
  except
    on E: ENyxBrowserViewRetired do
    begin
      Exit(True);
    end;
  end;
end;

procedure TNyxBrowserRenderer.SetDesignerInput(const AValue: TNyxDesignerInput);
begin

  if FRoot <> nil then
  begin
    raise ENyxModel.Create('Configure designer input before mounting its view');
  end;
  FDesignerInput := AValue;
end;

procedure TNyxBrowserRenderer.GestureFailed(const AOriginID, AReason: TNyxText);
var
  LSink: TNyxGestureFailure;
begin
  FLastGestureError := AReason;
  LSink := FOnGestureFailure;

  if Assigned(LSink) then
  begin
    LSink(AOriginID, AReason);
  end;
end;

function TNyxBrowserBinding.DragStart(AEvent: TJSDragEvent): Boolean;
var
  LGeneration: TBrowserViewGeneration;
  LGenerationLease: IInterface;
begin
  try
    LGeneration := TBrowserViewGeneration(FGenerationObject);
    LGenerationLease := LGeneration;

    if (LGeneration = nil) or not LGeneration.Current or FDisconnected then
    begin
      Exit(True);
    end;
    Result := DragEvent(AEvent, ntDragStart, ndpStart);
  except
    on E: ENyxBrowserViewRetired do
    begin
      Exit(True);
    end;
  end;
end;

function TNyxBrowserBinding.Drag(AEvent: TJSDragEvent): Boolean;
var
  LGeneration: TBrowserViewGeneration;
  LGenerationLease: IInterface;
begin
  try
    LGeneration := TBrowserViewGeneration(FGenerationObject);
    LGenerationLease := LGeneration;

    if (LGeneration = nil) or not LGeneration.Current or FDisconnected then
    begin
      Exit(True);
    end;
    Result := DragEvent(AEvent, ntDrag, ndpDrag);
  except
    on E: ENyxBrowserViewRetired do
    begin
      Exit(True);
    end;
  end;
end;

function TNyxBrowserBinding.DragEnter(AEvent: TJSDragEvent): Boolean;
var
  LGeneration: TBrowserViewGeneration;
  LGenerationLease: IInterface;
begin
  try
    LGeneration := TBrowserViewGeneration(FGenerationObject);
    LGenerationLease := LGeneration;

    if (LGeneration = nil) or not LGeneration.Current or FDisconnected then
    begin
      Exit(True);
    end;
    Result := DragEvent(AEvent, ntDragEnter, ndpEnter);
  except
    on E: ENyxBrowserViewRetired do
    begin
      Exit(True);
    end;
  end;
end;

function TNyxBrowserBinding.DragOver(AEvent: TJSDragEvent): Boolean;
var
  LGeneration: TBrowserViewGeneration;
  LGenerationLease: IInterface;
begin
  try
    LGeneration := TBrowserViewGeneration(FGenerationObject);
    LGenerationLease := LGeneration;

    if (LGeneration = nil) or not LGeneration.Current or FDisconnected then
    begin
      Exit(True);
    end;
    Result := DragEvent(AEvent, ntDragOver, ndpOver);
  except
    on E: ENyxBrowserViewRetired do
    begin
      Exit(True);
    end;
  end;
end;

function TNyxBrowserBinding.DragExit(AEvent: TJSDragEvent): Boolean;
var
  LGeneration: TBrowserViewGeneration;
  LGenerationLease: IInterface;
begin
  try
    LGeneration := TBrowserViewGeneration(FGenerationObject);
    LGenerationLease := LGeneration;

    if (LGeneration = nil) or not LGeneration.Current or FDisconnected then
    begin
      Exit(True);
    end;
    Result := DragEvent(AEvent, ntDragExit, ndpExit);
  except
    on E: ENyxBrowserViewRetired do
    begin
      Exit(True);
    end;
  end;
end;

function TNyxBrowserBinding.Drop(AEvent: TJSDragEvent): Boolean;
var
  LGeneration: TBrowserViewGeneration;
  LGenerationLease: IInterface;
begin
  try
    LGeneration := TBrowserViewGeneration(FGenerationObject);
    LGenerationLease := LGeneration;

    if (LGeneration = nil) or not LGeneration.Current or FDisconnected then
    begin
      Exit(True);
    end;
    Result := DragEvent(AEvent, ntDrop, ndpDrop);
  except
    on E: ENyxBrowserViewRetired do
    begin
      Exit(True);
    end;
  end;
end;

function TNyxBrowserBinding.DragEnd(AEvent: TJSDragEvent): Boolean;
var
  LGeneration: TBrowserViewGeneration;
  LGenerationLease: IInterface;
begin
  try
    LGeneration := TBrowserViewGeneration(FGenerationObject);
    LGenerationLease := LGeneration;

    if (LGeneration = nil) or not LGeneration.Current or FDisconnected then
    begin
      Exit(True);
    end;
    Result := DragEvent(AEvent, ntDragEnd, ndpEnd);
  except
    on E: ENyxBrowserViewRetired do
    begin
      Exit(True);
    end;
  end;
end;

function TNyxBrowserBinding.DragEvent(AEvent: TJSDragEvent; ATrigger: TNyxTrigger;
  APhase: TNyxDragPhase): Boolean;
var
  LGeneration: TBrowserViewGeneration;
  LGenerationLease: IInterface;
  LEvents: INyxEvents;
  LRevision: Integer;
  LRenderer: TNyxBrowserRenderer;
  LOriginID: TNyxText;
  LPolicy: TNyxInteractionPolicy;
  LDispatch: TNyxDispatch;
  LTransfer: TNyxTransferSnapshot;
  LAllowed: TNyxDropOperations;
  LOperation: TNyxDropOperation;
  LCapabilities: TNyxGestureCapabilities;
  LDecision: INyxGestureDecision;
  LResponse: TNyxGestureResult;
  LTransform: TBrowserFaceTransform;
  LPoint: TNyxResizePoint;
  LTarget: TJSHTMLElement;
  LSource: Boolean;
  LDesignerTarget: TNyxDesignerTarget;
begin
  try
    LGeneration := TBrowserViewGeneration(FGenerationObject);
    LGenerationLease := LGeneration;

    if (LGeneration = nil) or not LGeneration.Current or FDisconnected then
    begin
      Exit(True);
    end;
    Result := True;
    LEvents := CurrentRenderer.FEvents;
    LRevision := FViewRevision;
    LSource := APhase in [ndpStart, ndpDrag, ndpEnd];

    if ((LEvents.ViewRevision <> LRevision) or not LGeneration.Current) or CurrentRenderer.FUpdating or
      (CurrentRenderer.FDesignMode and (LSource or not CurrentRenderer.FDesignerInput.DropEnabled)) or
      AEvent.defaultPrevented then
    begin
      Exit;
    end;
    LTarget := TJSHTMLElement(AEvent.target);

    if (LTarget.closest('[data-runtime-id]') <> FElement) or
      (LSource and (FNode.Prop('drag-source') <> 'true')) or
      (not LSource and not CurrentRenderer.FDesignMode and (FNode.Prop('drop-target') <> 'true')) then
    begin
      Exit;
    end;
    LRenderer := CurrentRenderer;
    LOriginID := FNode.ID;
    LPolicy := NyxInteractionPolicy(FNode);
    { Opted-in sources/targets negotiate explicitly. Default browser source
      behavior is canceled without an offer; a drop never inserts text, opens a
      file or follows a transferred URI through a browser default. }

    if APhase = ndpDrop then
    begin
      AEvent.preventDefault;
    end;
    AEvent.stopPropagation;

    if not CurrentRenderer.FDesignMode and not LPolicy.CanIssueCommand then
    begin

      if APhase in [ndpStart, ndpDrop] then
      begin
        AEvent.preventDefault;
        Result := False;
      end;
      Exit;
    end;
    try
      LTransfer := CaptureNyxBrowserTransfer(AEvent.dataTransfer,
        APhase in [ndpStart, ndpDrop]);
      TryNyxDropOperations(AEvent.dataTransfer.effectAllowed, LAllowed);
      TryNyxDropOperation(AEvent.dataTransfer.dropEffect, LOperation);
    except
      on LException: Exception do
      begin
        AEvent.preventDefault;
        LRenderer.GestureFailed(LOriginID, LException.Message);
        Exit(False);
      end;
    end;

    if CurrentRenderer.FDesignMode then
    begin
      LDispatch := Default(TNyxDispatch);
      LDispatch.Info := NyxDesignerDragEvent(FNode, ATrigger);
    end
    else
    begin
      LDispatch := CurrentRenderer.FLiveBindings.Signal(FNode, ATrigger);
    end;
    LCapabilities := [];

    if AEvent.cancelable then
    begin

      if APhase = ndpStart then
      begin
        Include(LCapabilities, ngcOfferDrag);
      end
      else if (APhase in [ndpEnter, ndpOver, ndpDrop]) and
        (CurrentRenderer.FDesignMode or LPolicy.CanEditValue) then
      begin
        Include(LCapabilities, ngcAcceptDrop);
      end;
    end;
    LDispatch.Info.HasDrag := True;
    LDispatch.Info.Drag := NyxDragSnapshot(APhase, LTransfer, LAllowed,
      LOperation, '', LCapabilities <> []);
    LDispatch.Info.HasPointer := True;
    LDispatch.Info.Pointer.Kind := npiMouse;
    LTransform := TBrowserFaceTransform.Capture(FElement);
    LPoint := LTransform.LogicalPoint(AEvent.clientX, AEvent.clientY);
    LDispatch.Info.Pointer.X := LPoint.X;
    LDispatch.Info.Pointer.Y := LPoint.Y;
    LDispatch.Info.Pointer.HasPosition := True;

    if AEvent.shiftKey then
    begin
      Include(LDispatch.Info.Pointer.Modifiers, nmShift);
    end;

    if AEvent.ctrlKey then
    begin
      Include(LDispatch.Info.Pointer.Modifiers, nmControl);
    end;

    if AEvent.altKey then
    begin
      Include(LDispatch.Info.Pointer.Modifiers, nmAlt);
    end;

    if AEvent.metaKey then
    begin
      Include(LDispatch.Info.Pointer.Modifiers, nmMeta);
    end;
    LDecision := NewNyxGestureDecision(LCapabilities, LAllowed);
    try

      if CurrentRenderer.FDesignMode then
      begin

        if Assigned(CurrentRenderer.FOnDesignerGesture) then
        begin
          LDesignerTarget := NyxDesignerTarget(FNode).WithFrame(CurrentRenderer.DropFrameFor(FNode.ID));
          { Drag and ordinary pointer callbacks now share the same logical face
            mapping. A second designer-only correction would apply zoom twice. }
          CurrentRenderer.FOnDesignerGesture(LDesignerTarget, LDispatch.Info, LDecision);
        end;
      end
      else
      begin
        DispatchNyxGesture(LEvents, FNode, LDispatch, LDecision);
      end;
    finally
      LResponse := LDecision.Seal;
    end;

    if (LEvents.ViewRevision <> LRevision) or not LGeneration.Current then
    begin
      AEvent.preventDefault;
      Exit(False);
    end;

    if APhase = ndpStart then
    begin

      if not LResponse.Offered or (LPolicy.ReadOnly and (ndoMove in LResponse.Allowed)) then
      begin
        AEvent.preventDefault;
        LRenderer.GestureFailed(LOriginID,
          'Drag requires a synchronous offer; a read-only source cannot offer Move');
        Exit(False);
      end;
      try
        OfferNyxBrowserTransfer(AEvent.dataTransfer, LResponse);
      except
        { Host DOM failures are not necessarily Pascal Exception instances. Keep
          the offer canceled and report an owned refusal through the public sink. }
        AEvent.preventDefault;
        LRenderer.GestureFailed(LOriginID, 'The browser refused the typed drag transfer');
        Exit(False);
      end;
      LRenderer.FLastGestureError := '';
    end
    else if APhase in [ndpEnter, ndpOver, ndpDrop] then
    begin
      LOperation := ndoNone;

      if LResponse.Accepted then
      begin
        LOperation := LResponse.Operation;
      end;
      try
        AEvent.dataTransfer.dropEffect := NyxDropOperationName(LOperation);
      except
        AEvent.preventDefault;
        LRenderer.GestureFailed(LOriginID, 'The browser refused the typed drop operation');
        Exit(False);
      end;

      if LOperation <> ndoNone then
      begin
        AEvent.preventDefault;
        LRenderer.FLastGestureError := '';
      end;
    end;
  except
    on E: ENyxBrowserViewRetired do
    begin
      Exit(True);
    end;
  end;
end;

procedure TNyxBrowserBinding.RequireCurrent;
begin

  if FDisconnected or (FGenerationObject = nil) or
    not TBrowserViewGeneration(FGenerationObject).Current then
  begin
    raise ENyxBrowserViewRetired.Create('The browser view was retired');
  end;
end;

function TNyxBrowserBinding.RetainView: IInterface;
begin
  Result := nil;

  if not FDisconnected and (FGenerationObject <> nil) and
    TBrowserViewGeneration(FGenerationObject).Current then
  begin
    Result := TBrowserViewGeneration(FGenerationObject);
  end;
end;

function TNyxBrowserBinding.CurrentRenderer: TNyxBrowserRenderer;
begin
  { The weak receiver is never retargeted to a successor. Checking its original
    canonical generation before EVERY borrowed receiver access also protects
    helper/error/finally paths after an outward getter retires the view. }
  RequireCurrent;
  Result := FRenderer;
end;

destructor TNyxBrowserBinding.Destroy;
begin
  Disconnect;
  FreeAndNil(FColorField);
  FreeAndNil(FSplit);
  FreeAndNil(FTabs);
  FreeAndNil(FDrawingView);
  inherited Destroy;
end;

procedure TNyxBrowserBinding.Disconnect;
var
  LViewportElement: TJSHTMLElement;
  LFocus: TJSHTMLElement;
  LIndex: Integer;
  LPointerID: Integer;
begin

  if FDisconnected then
  begin
    Exit;
  end;
  FDisconnected := True;

  if FElement <> nil then
  begin
    FElement.onclick := nil;
  end;

  if FInput <> nil then
  begin
    FInput.onchange := nil;
    FInput.removeEventListener('focus', @Enter);
    FInput.removeEventListener('blur', @Leave);
  end;
  { Detach the exact focus surface before disposing a split behavior. A caller
    may still hold its old DOM element after navigation; it must have no live
    Pascal event sink. Remove both capture modes used by default/custom faces. }
  LFocus := FocusElement;

  if LFocus <> nil then
  begin
    LFocus.removeEventListener('focus', @Enter);
    LFocus.removeEventListener('blur', @Leave);
    LFocus.removeEventListener('focusin', @Enter);
    LFocus.removeEventListener('focusout', @Leave);
    LFocus.removeEventListener('keydown', @KeyDown);
    LFocus.removeEventListener('keyup', @KeyUp);
    TNyxKeyboardElement(LFocus).Unlisten('keydown', @KeyDown, True);
    TNyxKeyboardElement(LFocus).Unlisten('keyup', @KeyUp, True);
  end;

  if FSplit <> nil then
  begin
    FSplit.Disconnect;
  end;

  if FTabs <> nil then
  begin
    FTabs.Disconnect;
  end;
  { Queued DOM scroll notifications can outlive removal from the document.
    Revoke these producers before releasing their borrowed renderer/node. }

  if FElement <> nil then
  begin
    FElement.removeEventListener('focus', @ForwardRadioFocus);
    FElement.removeEventListener('pointerdown', @PointerDown);
    FElement.removeEventListener('pointerup', @PointerUp);
    FElement.removeEventListener('pointermove', @PointerMove);
    FElement.removeEventListener('pointerenter', @PointerEnter);
    FElement.removeEventListener('pointerleave', @PointerExit);
    FElement.removeEventListener('pointercancel', @PointerCancel);
    FElement.removeEventListener('gotpointercapture', @PointerCapture);
    FElement.removeEventListener('lostpointercapture', @PointerCaptureLost);
    FElement.removeEventListener('dragstart', @DragStart);
    FElement.removeEventListener('drag', @Drag);
    FElement.removeEventListener('dragenter', @DragEnter);
    FElement.removeEventListener('dragover', @DragOver);
    FElement.removeEventListener('dragleave', @DragExit);
    FElement.removeEventListener('drop', @Drop);
    FElement.removeEventListener('dragend', @DragEnd);
    { Revoke sinks before asking the host to release capture; no loss callback
      can reenter a disposed binding, even if the old DOM face is retained. }
    for LIndex := 0 to High(FCapturedPointers) do
    begin
      LPointerID := FCapturedPointers[LIndex];
      try

        if TNyxGestureElement(FElement).hasPointerCapture(LPointerID) then
        begin
          FElement.releasePointerCapture(LPointerID);
        end;

        if (FInput <> nil) and TNyxGestureElement(FInput).hasPointerCapture(LPointerID) then
        begin
          FInput.releasePointerCapture(LPointerID);
        end;
      except
        { Host implicit release may already have ended the pointer. }
      end;
    end;
    FElement.removeEventListener('wheel', @Wheel);
    LViewportElement := FElement;

    if FInput <> nil then
    begin
      LViewportElement := FInput;
    end;
    LViewportElement.removeEventListener('scroll', @Scroll);
    LViewportElement.removeEventListener('scrollend', @ScrollEnd);
    LViewportElement.removeEventListener('beforeinput', @BeforeEdit);
    LViewportElement.removeEventListener('compositionstart', @CompositionStart);
    LViewportElement.removeEventListener('compositionupdate', @CompositionUpdate);
    LViewportElement.removeEventListener('compositionend', @CompositionEnd);
    LViewportElement.removeEventListener('selectionchange', @TextSelectionChanged);
    LViewportElement.removeEventListener('select', @TextSelectionChanged);
  end;

  if FImageRequest <> nil then
  begin
    FImageRequest.Retire;
    FImageRequest := nil;
  end;

  if FImageLifecycle <> nil then
  begin
    FImageLifecycle.Close;
    FImageLifecycle := nil;
  end;

  if FColorField <> nil then
  begin
    FColorField.Disconnect;
  end;
end;

function TNyxBrowserBinding.Wheel(AEvent: TJSWheelEvent): Boolean;
var
  LGeneration: TBrowserViewGeneration;
  LGenerationLease: IInterface;
  LEvents: INyxEvents;
  LRevision: Integer;
  LDispatch: TNyxDispatch;
  LUnits: TNyxWheelUnit;
  LModifiers: TNyxKeyModifiers;
  LConsumed: Boolean;
begin
  try
    LGeneration := TBrowserViewGeneration(FGenerationObject);
    LGenerationLease := LGeneration;

    if (LGeneration = nil) or not LGeneration.Current or FDisconnected then
    begin
      Exit(True);
    end;
    Result := True;
    LEvents := CurrentRenderer.FEvents;
    LRevision := FViewRevision;

    if ((LEvents.ViewRevision <> LRevision) or not LGeneration.Current) or CurrentRenderer.FUpdating or
      CurrentRenderer.FDesignMode or AEvent.defaultPrevented or
      (TJSHTMLElement(AEvent.target).closest('[data-runtime-id]') <> FElement) then
    begin
      Exit;
    end;

    if not LEvents.HasSubscribers(ntBeforeWheel) and not LEvents.HasSubscribers(ntWheel) and
      not LEvents.HasSubscribers(ntAfterWheel) then
    begin
      Exit;
    end;
    case AEvent.deltaMode of
      0:
        begin
          LUnits := nwuPixels;
        end;
      1:
        begin
          LUnits := nwuLines;
        end;
      2:
        begin
          LUnits := nwuPages;
        end;
    else
      { Unknown device units cannot safely be guessed. Leave the native action. }
      Exit;
    end;
    LModifiers := [];

    if AEvent.shiftKey then
    begin
      Include(LModifiers, nmShift);
    end;

    if AEvent.ctrlKey then
    begin
      Include(LModifiers, nmControl);
    end;

    if AEvent.altKey then
    begin
      Include(LModifiers, nmAlt);
    end;

    if AEvent.metaKey then
    begin
      Include(LModifiers, nmMeta);
    end;
    LDispatch := CurrentRenderer.FLiveBindings.Signal(FNode, ntWheel);
    LDispatch.Info.HasWheel := True;
    LDispatch.Info.Wheel := NyxWheel(AEvent.deltaX, AEvent.deltaY, AEvent.deltaZ,
      LUnits, LModifiers, AEvent.cancelable);
    LConsumed := DispatchNyxWheel(LEvents, FNode, LDispatch,
      @LGeneration.ReadSignalSnapshot);
    { A callback may destroy this binding; use only retained locals below. }

    if (LConsumed or ((LEvents.ViewRevision <> LRevision) or not LGeneration.Current)) and AEvent.cancelable then
    begin
      AEvent.preventDefault;
      Result := False;
    end;
    AEvent.stopPropagation;
  except
    on E: ENyxBrowserViewRetired do
    begin
      Exit(True);
    end;
  end;
end;

function TNyxBrowserBinding.Scroll(AEvent: TEventListenerEvent): Boolean;
var
  LGeneration: TBrowserViewGeneration;
  LGenerationLease: IInterface;
begin
  try
    LGeneration := TBrowserViewGeneration(FGenerationObject);
    LGenerationLease := LGeneration;

    if (LGeneration = nil) or not LGeneration.Current or FDisconnected then
    begin
      Exit(True);
    end;
    Result := ViewportEvent(AEvent, ntScroll);
  except
    on E: ENyxBrowserViewRetired do
    begin
      Exit(True);
    end;
  end;
end;

function TNyxBrowserBinding.ScrollEnd(AEvent: TEventListenerEvent): Boolean;
var
  LGeneration: TBrowserViewGeneration;
  LGenerationLease: IInterface;
begin
  try
    LGeneration := TBrowserViewGeneration(FGenerationObject);
    LGenerationLease := LGeneration;

    if (LGeneration = nil) or not LGeneration.Current or FDisconnected then
    begin
      Exit(True);
    end;
    Result := ViewportEvent(AEvent, ntScrollEnd);
  except
    on E: ENyxBrowserViewRetired do
    begin
      Exit(True);
    end;
  end;
end;

function TNyxBrowserBinding.ViewportEvent(AEvent: TEventListenerEvent;
  ATrigger: TNyxTrigger): Boolean;
var
  LGeneration: TBrowserViewGeneration;
  LGenerationLease: IInterface;
  LElement: TJSHTMLElement;
  LDispatch: TNyxDispatch;
begin
  try
    LGeneration := TBrowserViewGeneration(FGenerationObject);
    LGenerationLease := LGeneration;

    if (LGeneration = nil) or not LGeneration.Current or FDisconnected then
    begin
      Exit(True);
    end;
    Result := True;
    LElement := FElement;

    if FInput <> nil then
    begin
      LElement := FInput;
    end;

    if (CurrentRenderer.FEvents.ViewRevision <> FViewRevision) or CurrentRenderer.FUpdating or
      CurrentRenderer.FDesignMode or (AEvent.target <> LElement) or
      not CurrentRenderer.FEvents.HasSubscribers(ATrigger) then
    begin
      Exit;
    end;
    LDispatch := CurrentRenderer.FLiveBindings.Signal(FNode, ATrigger);
    LDispatch.Info.HasViewport := True;
    LDispatch.Info.Viewport := CaptureBrowserViewport(LElement);
    { Scroll is a post-movement observation; it never edits the document value,
      selection or undo history, and it cannot consume a platform default. }
    CurrentRenderer.Emit(FNode, LDispatch);
  except
    on E: ENyxBrowserViewRetired do
    begin
      Exit(True);
    end;
  end;
end;

function TNyxBrowserBinding.FocusElement: TJSHTMLElement;
begin
  Result := FInput;

  if Result = nil then
  begin
    Result := FElement;

    if FSplit <> nil then
    begin
      Result := FSplit.Divider;
    end;
  end;
end;

function TNyxBrowserBinding.ForwardRadioFocus(AEvent: TEventListenerEvent): Boolean;
var
  LGeneration: TBrowserViewGeneration;
  LGenerationLease: IInterface;
  LPolicy: TNyxInteractionPolicy;
begin
  try
    LGeneration := TBrowserViewGeneration(FGenerationObject);
    LGenerationLease := LGeneration;

    if (LGeneration = nil) or not LGeneration.Current or FDisconnected then
    begin
      Exit(True);
    end;
    Result := True;

    if (CurrentRenderer.FEvents.ViewRevision <> FViewRevision) or CurrentRenderer.FUpdating or
      CurrentRenderer.FDesignMode or (AEvent.target <> FElement) then
    begin
      Exit;
    end;
    LPolicy := NyxInteractionPolicy(FNode);

    if LPolicy.CanIssueCommand and (FInput <> nil) then
    begin
      { The input's existing focus listener owns the one Nyx notification. }
      NyxFocusWithoutScroll(FInput);
    end;
  except
    on E: ENyxBrowserViewRetired do
    begin
      Exit(True);
    end;
  end;
end;

function TNyxBrowserBinding.IsFocusBoundary(AEvent: TEventListenerEvent): Boolean;
var
  LRelated: TJSNode;
begin
  Result := False;

  if (FRenderer.FEvents.ViewRevision <> FViewRevision) or FRenderer.FUpdating or
    FRenderer.FDesignMode then
  begin
    Exit;
  end;

  if (FCollectionMount <> nil) and FCollectionMount.Connected then
  begin
    { Moving between a row and its editor stays in the same logical control.
      Only crossing the owning collection boundary emits enter/exit. }
    LRelated := TJSNode(TJSFocusEvent(AEvent).relatedTarget);
    Result := FElement.contains(TJSNode(AEvent.target)) and
      ((LRelated = nil) or not FElement.contains(LRelated));
  end
  else
  begin
    Result := AEvent.target = FocusElement;
  end;
end;

function TNyxBrowserBinding.Enter(AEvent: TEventListenerEvent): Boolean;
var
  LGeneration: TBrowserViewGeneration;
  LGenerationLease: IInterface;
begin
  try
    LGeneration := TBrowserViewGeneration(FGenerationObject);
    LGenerationLease := LGeneration;

    if (LGeneration = nil) or not LGeneration.Current or FDisconnected then
    begin
      Exit(True);
    end;
    Result := True;

    if IsFocusBoundary(AEvent) and
      CurrentRenderer.FEvents.HasSubscribers(ntAfterEnter) then
    begin
      CurrentRenderer.Emit(FNode, CurrentRenderer.FLiveBindings.Focus(FNode, ntAfterEnter));
    end;
  except
    on E: ENyxBrowserViewRetired do
    begin
      Exit(True);
    end;
  end;
end;

function TNyxBrowserBinding.Leave(AEvent: TEventListenerEvent): Boolean;
var
  LGeneration: TBrowserViewGeneration;
  LGenerationLease: IInterface;
begin
  try
    LGeneration := TBrowserViewGeneration(FGenerationObject);
    LGenerationLease := LGeneration;

    if (LGeneration = nil) or not LGeneration.Current or FDisconnected then
    begin
      Exit(True);
    end;
    Result := True;

    if IsFocusBoundary(AEvent) and
      CurrentRenderer.FEvents.HasSubscribers(ntAfterExit) then
    begin
      CurrentRenderer.Emit(FNode, CurrentRenderer.FLiveBindings.Focus(FNode, ntAfterExit));
    end;
  except
    on E: ENyxBrowserViewRetired do
    begin
      Exit(True);
    end;
  end;
end;

function TNyxBrowserBinding.KeyDown(AEvent: TJSKeyboardEvent): Boolean;
var
  LGeneration: TBrowserViewGeneration;
  LGenerationLease: IInterface;
begin
  try
    LGeneration := TBrowserViewGeneration(FGenerationObject);
    LGenerationLease := LGeneration;

    if (LGeneration = nil) or not LGeneration.Current or FDisconnected then
    begin
      Exit(True);
    end;
    Result := Keyboard(AEvent, ntKeyDown);
  except
    on E: ENyxBrowserViewRetired do
    begin
      Exit(True);
    end;
  end;
end;

function TNyxBrowserBinding.KeyUp(AEvent: TJSKeyboardEvent): Boolean;
var
  LGeneration: TBrowserViewGeneration;
  LGenerationLease: IInterface;
begin
  try
    LGeneration := TBrowserViewGeneration(FGenerationObject);
    LGenerationLease := LGeneration;

    if (LGeneration = nil) or not LGeneration.Current or FDisconnected then
    begin
      Exit(True);
    end;
    Result := Keyboard(AEvent, ntKeyUp);
  except
    on E: ENyxBrowserViewRetired do
    begin
      Exit(True);
    end;
  end;
end;

function TNyxBrowserBinding.ColumnKeyboard(AEvent: TJSKeyboardEvent;
  const ACommand: INyxBrowserColumnCommand; ATrigger: TNyxTrigger;
  APrepared: Boolean): Boolean;
var
  LGeneration: TBrowserViewGeneration;
  LGenerationLease: IInterface;
  LEvents: INyxEvents;
  LRevision, LObserved: Integer;
  LOrigin, LSource: TNyxNode;
  LLiveBindings: TNyxLiveBindings;
  LModifiers: TNyxKeyModifiers;
  LDispatch: TNyxDispatch;
  LSnapshot: TColumnKeyboardSnapshot;
  LSnapshotLease: IInterface;
  LBlocked, LSubscribers, LConsumed: Boolean;
  LButtonActivation: INyxBrowserColumnButtonActivation;
  LButtonRelease: INyxBrowserColumnButtonRelease;
  LInputAdmitted: Boolean;

  function Current: Boolean;
  begin
    LObserved := LEvents.ViewRevision;
    Result := (LObserved = LRevision) and ACommand.DispatchCurrent;
  end;

begin
  try
    LGeneration := TBrowserViewGeneration(FGenerationObject);
    LGenerationLease := LGeneration;

    if (LGeneration = nil) or not LGeneration.Current or FDisconnected then
    begin
      Exit(True);
    end;
    Result := False;
    { Every borrowed dependency is copied before the first outward query. Node
      references protect the public dispatcher's own early revision query; the
      command protects the borrowed live bindings between individual phases. }
    LEvents := CurrentRenderer.FEvents;
    LRevision := FViewRevision;
    LOrigin := FNode;
    LSource := nil;
    LLiveBindings := CurrentRenderer.FLiveBindings;
    LBlocked := CurrentRenderer.FUpdating or CurrentRenderer.FDesignMode or FComposing or
      AEvent.isComposing or AEvent.defaultPrevented;
    LOrigin.AcquireReference;
    try
      try
        { Initial admission is stronger than later callback lifetime. A staged
          or composing input never enters canonical key/action dispatch; once
          admitted, benign callback edits may still deliver remaining phases. }

        LInputAdmitted := ACommand.Current;

        if Supports(ACommand, INyxBrowserColumnButtonRelease, LButtonRelease) then
        begin
          LInputAdmitted := LButtonRelease.EntryAdmitted;
        end;

        if LBlocked or not LInputAdmitted or not Current then
        begin
          Exit;
        end;

        if not APrepared then
        begin
          ACommand.Prepare;
        end;

        if not Current then
        begin
          Exit;
        end;
        LSubscribers := NyxHasKeyboardSubscribers(LEvents, ATrigger);

        if not Current then
        begin
          Exit;
        end;

        if LSubscribers then
        begin
          LModifiers := [];

          if AEvent.shiftKey then
          begin
            Include(LModifiers, nmShift);
          end;

          if AEvent.ctrlKey then
          begin
            Include(LModifiers, nmControl);
          end;

          if AEvent.altKey then
          begin
            Include(LModifiers, nmAlt);
          end;

          if AEvent.metaKey then
          begin
            Include(LModifiers, nmMeta);
          end;

          if AEvent.getModifierState('AltGraph') then
          begin
            Include(LModifiers, nmAltGraph);
          end;
          LDispatch := LLiveBindings.Keyboard(LOrigin, ATrigger,
            NyxKeyStroke(NyxKeyFromBrowser(AEvent.key), LModifiers, AEvent._repeat));
          LSource := LDispatch.Source;
          LSource.AcquireReference;
          LSnapshot := TColumnKeyboardSnapshot.Create(LEvents, LRevision, ACommand,
            @LGeneration.ReadSignalSnapshot);
          LSnapshotLease := LSnapshot;
          { Preserve this key's before/main/after phases and KeyDown's related
            KeyPress phases, including fresh accepted payloads. }
          LConsumed := DispatchNyxKeyboard(LEvents, LOrigin, LDispatch, LSnapshot.Read);
        end
        else
        begin
          LConsumed := False;
        end;

        if Current then
        begin

          if not LConsumed and not AEvent.defaultPrevented and
            Supports(ACommand, INyxBrowserColumnButtonActivation, LButtonActivation) and
            LButtonActivation.ActivatesButton then
          begin

            if Supports(ACommand, INyxBrowserColumnButtonRelease, LButtonRelease) and
              LButtonRelease.AwaitsRelease then
            begin
              { Only owner-free prepared facts survive between physical keys.
                A repeated Space neither clicks nor replaces the first press. }
              LButtonRelease.HoldUntilRelease;
            end
            else
            begin
              { Enter and a matching Space release share ordinary Click actions.
                Shortcut Fit remains a key command. No fresh target is captured. }
              ColumnClick(AEvent, ACommand, True);
            end;
          end
          else
          begin
            ACommand.Complete(not LConsumed and not AEvent.defaultPrevented);
          end;
        end;
      except
        on ENyxColumnInputRetired do
        begin
          { Navigation inside a subscriber/revision query is an expected refusal.
            All application exceptions still pass through the outer finally. }
        end;
      end;
    finally
      AEvent.preventDefault;
      AEvent.stopPropagation;
      ACommand.Retire;
      try

        if LSource <> nil then
        begin
          LSource.ReleaseReference;
        end;
      finally
        LOrigin.ReleaseReference;
      end;
    end;
  except
    on E: ENyxBrowserViewRetired do
    begin
      Exit(True);
    end;
  end;
end;

function TNyxBrowserBinding.Keyboard(AEvent: TJSKeyboardEvent;
  ATrigger: TNyxTrigger): Boolean;
var
  LGeneration: TBrowserViewGeneration;
  LGenerationLease: IInterface;
  LModifiers: TNyxKeyModifiers;
  LInput: TJSHTMLElement;
  LDispatch: TNyxDispatch;
  LConsumed: Boolean;
  LEvents: INyxEvents;
  LRevision: Integer;
  LDomain: TNyxValueDomain;
  LMount: INyxCollectionMount;
  LColumnInput: INyxBrowserCollectionColumnInput;
  LColumnCommand: INyxBrowserColumnCommand;
  LColumnRelease: INyxBrowserCollectionColumnRelease;
  LClipboardInput: INyxBrowserCollectionClipboardInput;
  LRangeInput: INyxBrowserCollectionCellRangeInput;
begin
  try
    LGeneration := TBrowserViewGeneration(FGenerationObject);
    LGenerationLease := LGeneration;

    if (LGeneration = nil) or not LGeneration.Current or FDisconnected then
    begin
      Exit(True);
    end;
    Result := True;
    { Ordinary collection key capture runs before the mount's bubbling listener.
      Reserve original authority BEFORE even a real ViewRevision query; the
      claimed route alone brackets canonical dispatch and completes its default. }

    if ATrigger = ntKeyDown then
    begin
      LMount := FCollectionMount;

      if Supports(LMount, INyxBrowserCollectionCellRangeInput, LRangeInput) then
      begin
        LColumnCommand := LRangeInput.BeginRangeKey(AEvent);

        if LColumnCommand <> nil then
        begin
          Exit(ColumnKeyboard(AEvent, LColumnCommand));
        end;
      end;

      if Supports(LMount, INyxBrowserCollectionClipboardInput, LClipboardInput) then
      begin
        LColumnCommand := LClipboardInput.BeginClipboardKey(AEvent);

        if LColumnCommand <> nil then
        begin
          Exit(ColumnKeyboard(AEvent, LColumnCommand));
        end;
      end;

      if Supports(LMount, INyxBrowserCollectionColumnInput, LColumnInput) then
      begin
        LColumnCommand := LColumnInput.BeginColumnKey(AEvent);

        if LColumnCommand <> nil then
        begin
          Exit(ColumnKeyboard(AEvent, LColumnCommand));
        end;
      end;
    end;

    if ATrigger = ntKeyUp then
    begin
      LMount := FCollectionMount;

      if Supports(LMount, INyxBrowserCollectionColumnRelease, LColumnRelease) then
      begin
        LColumnCommand := LColumnRelease.BeginColumnRelease(AEvent);

        if LColumnCommand <> nil then
        begin
          Exit(ColumnKeyboard(AEvent, LColumnCommand, ntKeyUp, True));
        end;
      end;
    end;
    LEvents := CurrentRenderer.FEvents;
    LRevision := FViewRevision;
    { A custom listener may have navigated before this listener was entered.
      Check the saved mount generation before dereferencing its borrowed node. }

    if (LEvents.ViewRevision <> LRevision) or not LGeneration.Current then
    begin
      AEvent.preventDefault;
      Exit(False);
    end;

    if CurrentRenderer.FUpdating or CurrentRenderer.FDesignMode or
      FComposing or AEvent.isComposing or AEvent.defaultPrevented then
    begin
      Exit;
    end;
    LInput := FocusElement;
    { Keyboard events bubble. Ordinary controls route their actual input/leaf.
      A collection attachment owns its row/edit descendants, so their keys route
      through the owning data control before its default selection action.
      Compounds receive semantic source registrations through the router. }

    if ((AEvent.target <> LInput) and
      ((FCollectionMount = nil) or not FCollectionMount.Connected or
      not FElement.contains(TJSNode(AEvent.target)))) or
      not NyxHasKeyboardSubscribers(LEvents, ATrigger) then
    begin
      Exit;
    end;
    { Native text edits publish OnChange while typing; browsers normally publish
      change on blur. Admit a pending text edit before capturing a keyboard payload,
      so Ctrl+Enter sees the same accepted memo text without requiring a blur.
      Numeric/domain drafts retain their explicit editing-complete boundary. }

    if (FInput <> nil) and ((FNode.ProjectionKind = 'memo') or
      (FNode.ProjectionKind = 'input') or (FNode.ProjectionKind = 'code-editor')) then
    begin
      LDomain := NyxNodeValueDomain(FNode);

      if not LDomain.Defined or (LDomain.Kind = nskText) then
      begin

        if not Change(AEvent) then
        begin
          Exit;
        end;

        if (LEvents.ViewRevision <> LRevision) or not LGeneration.Current then
        begin
          AEvent.preventDefault;
          Exit(False);
        end;
      end;
    end;
    LModifiers := [];

    if AEvent.shiftKey then
    begin
      Include(LModifiers, nmShift);
    end;

    if AEvent.ctrlKey then
    begin
      Include(LModifiers, nmControl);
    end;

    if AEvent.altKey then
    begin
      Include(LModifiers, nmAlt);
    end;

    if AEvent.metaKey then
    begin
      Include(LModifiers, nmMeta);
    end;

    if AEvent.getModifierState('AltGraph') then
    begin
      Include(LModifiers, nmAltGraph);
    end;
    LDispatch := CurrentRenderer.FLiveBindings.Keyboard(FNode, ATrigger,
      NyxKeyStroke(NyxKeyFromBrowser(AEvent.key), LModifiers,
      (ATrigger = ntKeyDown) and AEvent._repeat));
    LConsumed := DispatchNyxKeyboard(LEvents, FNode, LDispatch,
      @LGeneration.ReadSignalSnapshot);
    { No borrowed binding/node/widget reads after dispatch: callbacks may navigate.
      The DOM event itself remains owned by the browser for this listener call. }

    if LConsumed or ((LEvents.ViewRevision <> LRevision) or not LGeneration.Current) then
    begin
      AEvent.preventDefault;
      AEvent.stopPropagation;
      Result := False;
    end;
  except
    on E: ENyxBrowserViewRetired do
    begin
      Exit(True);
    end;
  end;
end;

procedure TNyxBrowserRenderer.BindingFailed(ANode: TNyxNode; const AReason: TNyxText;
  AFailure: TNyxBindingFailure; ARestoreInput: Boolean);
var
  LGeneration: TBrowserViewGeneration;
  LGenerationLease: IInterface;
begin
  LGeneration := TBrowserViewGeneration(FGenerationObject);
  LGenerationLease := FGeneration;

  if (LGeneration = nil) or not LGeneration.Current then
  begin
    Exit;
  end;
  FLastBindingError := AReason;
  FLastBindingFailure := AFailure;
  { The accepted model was untouched by rejection. Restore the physical input
    through the same in-place path, including any Boolean or compound mirrors. }

  if (AFailure = nbfRejected) and ARestoreInput then
  begin
    { Restore the refused date without forcing unrelated physical drafts back
      to their accepted model values. Other forced-sync callers remain global. }

    if (ANode <> nil) and (ANode.ProjectionKind = NyxKindName(nkDate)) then
    begin
      FForceValueNode := ANode;
    end;
    FForceValues := True;
    try
      Sync;
    finally

      if LGeneration.Current then
      begin
        FForceValues := False;
        FForceValueNode := nil;
      end;
    end;
  end;

  if LGeneration.Current and Assigned(FOnBindingError) then
  begin
    FOnBindingError(ANode, AReason, AFailure);
  end;
end;

procedure TNyxBrowserBinding.RefreshPager;
var
  LGeneration: TBrowserViewGeneration;
  LGenerationLease: IInterface;
begin
  try
    LGeneration := TBrowserViewGeneration(FGenerationObject);
    LGenerationLease := LGeneration;

    if (LGeneration = nil) or not LGeneration.Current or FDisconnected then
    begin
      Exit;
    end;
    CurrentRenderer.Sync;
  except
    on E: ENyxBrowserViewRetired do
    begin
      Exit;
    end;
  end;
end;

function TNyxBrowserRenderer.CollectionPager(const AID: TNyxText): INyxCollectionPager;
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

function TNyxBrowserRenderer.KanbanBinding(const AID: TNyxText): INyxKanbanBinding;
var
  LBinding: INyxCoordinatorBinding;
begin
  LBinding := CoordinatorBinding(AID);
  Result := nil;
  Supports(LBinding, INyxKanbanBinding, Result);
end;

function TNyxBrowserRenderer.CoordinatorBinding(const AID: TNyxText): INyxCoordinatorBinding;
begin
  Result := FindNyxCoordinatorMount(FCoordinatorMounts, FCoordinatorIDs, AID);
end;

function TNyxBrowserRenderer.MasterDetailBinding(const AID: TNyxText): INyxMasterDetailBinding;
var
  LBinding: INyxCoordinatorBinding;
begin
  LBinding := CoordinatorBinding(AID);
  Result := nil;
  Supports(LBinding, INyxMasterDetailBinding, Result);
end;

procedure TNyxBrowserRenderer.BindCollectionPagers;
var
  LIndex: Integer;
  LBinding: TNyxBrowserBinding;
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
      FEvents.Scheduler, @LBinding.RefreshPager);
  end;
end;

procedure TNyxBrowserBinding.SyncLiteralItems;
var
  LGeneration: TBrowserViewGeneration;
  LGenerationLease: IInterface;
  LKind: TNyxKind;
  LText: TNyxText;
  LSelected: TNyxText;
  LRows: TNyxStrings;
  LCells: TNyxStrings;
  LHost: TJSHTMLElement;
  LRow: TJSHTMLElement;
  LCell: TJSHTMLElement;
  LIndex: Integer;
  LColumn: Integer;
  LScrollTop: NativeInt;
  LScrollLeft: NativeInt;
begin
  try
    LGeneration := TBrowserViewGeneration(FGenerationObject);
    LGenerationLease := LGeneration;

    if (LGeneration = nil) or not LGeneration.Current or FDisconnected then
    begin
      Exit;
    end;

    if FCustom or (FCollectionMount <> nil) or
      not TryNyxKind(FNode.ProjectionKind, LKind) or
      not (LKind in [nkSelect, nkList, nkTable, nkTree]) then
    begin
      Exit;
    end;
    LText := FNode.Prop('items');

    if FHasItemsBaseline and (FLastItems = LText) then
    begin
      Exit;
    end;
    LHost := FElement;
    LSelected := '';

    if LKind = nkSelect then
    begin
      LHost := FInput;
      LSelected := FNode.Prop('value');

      if FHasValueBaseline and (FLastValue = LSelected) then
      begin
        { A changed option set preserves the physical draft when its accepted
          value has not changed. Assigning a missing choice yields no selection;
          it never writes an invented first item back into the portable model. }
        LSelected := TJSHTMLSelectElement(LHost).value;
      end;
    end;
    LScrollTop := LHost.scrollTop;
    LScrollLeft := LHost.scrollLeft;
    LRows := TNyxStrings.Create;
    try
      LRows.Text := LText;
      { Only these fallback-owned children are replaced. The logical host, its
        focus, callbacks and viewport remain mounted; identical Items is a no-op. }
      LHost.textContent := '';
      for LIndex := 0 to LRows.Count - 1 do
      begin

        if LKind = nkTable then
        begin
          LRow := Element('tr', '');
          LCells := NyxLiteralCells(LRows[LIndex]);
          try
            for LColumn := 0 to LCells.Count - 1 do
            begin

              if LIndex = 0 then
              begin
                LCell := Element('th', '');
              end
              else
              begin
                LCell := Element('td', '');
              end;
              LCell.textContent := LCells[LColumn];
              LRow.appendChild(LCell);
            end;
          finally
            LCells.Free;
          end;
        end
        else
        begin
          case LKind of
            nkSelect: LRow := Element('option', '');
            nkList: LRow := Element('li', '');
          else
            LRow := Element('div', '');
          end;
          LRow.textContent := LRows[LIndex];

          if LKind = nkSelect then
          begin
            LRow.setAttribute('value', LRows[LIndex]);
          end;
        end;
        LHost.appendChild(LRow);
      end;

      if LKind = nkSelect then
      begin
        TJSHTMLSelectElement(LHost).value := LSelected;
      end;
      LHost.scrollTop := LScrollTop;
      LHost.scrollLeft := LScrollLeft;
      FLastItems := LText;
      FHasItemsBaseline := True;
    finally
      LRows.Free;
    end;
  except
    on E: ENyxBrowserViewRetired do
    begin
      Exit;
    end;
  end;
end;

procedure TNyxBrowserRenderer.SyncRadioFocus;
type
  TRadioScope = record
    Name: TNyxText;
    Form: TJSHTMLFormElement;
    Entry: TNyxBrowserBinding;
    Checked: Boolean;
    BlockedChecked: Boolean;
  end;
var
  LScopes: array of TRadioScope;
  LIndex: Integer;
  LScope: Integer;
  LRadio: TJSHTMLInputElement;
  LPolicy: TNyxInteractionPolicy;
begin
  SetLength(LScopes, 0);
  for LIndex := 0 to High(FBindings) do
  begin

    if not FBindings[LIndex].FCustom and
      (FBindings[LIndex].FNode.ProjectionKind = 'radio') then
    begin
      LRadio := TJSHTMLInputElement(FBindings[LIndex].FInput);
      LRadio.tabIndex := -1;
      FBindings[LIndex].FElement.removeAttribute('tabindex');
      LPolicy := NyxInteractionPolicy(FBindings[LIndex].FNode);
      LScope := 0;
      while (LScope < Length(LScopes)) and
        ((LRadio.name = '') or (LScopes[LScope].Name <> LRadio.name) or
        (LScopes[LScope].Form <> LRadio.form)) do
      begin
        Inc(LScope);
      end;

      if LScope = Length(LScopes) then
      begin
        SetLength(LScopes, LScope + 1);
        LScopes[LScope].Name := LRadio.name;
        LScopes[LScope].Form := LRadio.form;
        LScopes[LScope].Entry := nil;
        LScopes[LScope].Checked := False;
        LScopes[LScope].BlockedChecked := False;
      end;

      if not LPolicy.CanIssueCommand then
      begin
        LScopes[LScope].BlockedChecked := LScopes[LScope].BlockedChecked or LRadio.checked;
        Continue;
      end;

      if LScopes[LScope].Entry = nil then
      begin
        LScopes[LScope].Entry := FBindings[LIndex];
      end;

      if LRadio.checked and not LScopes[LScope].Checked then
      begin
        LScopes[LScope].Entry := FBindings[LIndex];
        LScopes[LScope].Checked := True;
      end;
    end;
  end;
  for LScope := 0 to High(LScopes) do
  begin

    if LScopes[LScope].Entry = nil then
    begin
      Continue;
    end;

    if LScopes[LScope].BlockedChecked then
    begin
      { Explicit input tabindex cannot override Chromium's checked-peer group
        exclusion. Keep that value intact and enter through its existing label;
        ForwardRadioFocus immediately delegates to the same enabled input. }
      LScopes[LScope].Entry.FElement.tabIndex := 0;
    end
    else
    begin
      LScopes[LScope].Entry.FInput.tabIndex := 0;
    end;
  end;
end;

function TNyxBrowserRenderer.SyncEditedValue(ANode: TNyxNode): Boolean;
var
  LGeneration: TBrowserViewGeneration;
  LGenerationLease: IInterface;
  LIndex: Integer;
  LBinding: TNyxBrowserBinding;
  LValue: TNyxText;
begin
  Result := False;
  LGeneration := TBrowserViewGeneration(FGenerationObject);
  LGenerationLease := FGeneration;

  if (LGeneration = nil) or not LGeneration.Current then
  begin
    Exit(True);
  end;

  if FUpdating or FDesignMode or FForceValues or (ANode = nil) or
    not ((ANode.ProjectionKind = 'input') or (ANode.ProjectionKind = 'memo')) or
    (ANode.Prop('width-sizing') = 'content') or (ANode.Prop('height-sizing') = 'content') then
  begin
    Exit;
  end;
  for LIndex := 0 to High(FBindings) do
  begin
    LBinding := FBindings[LIndex];

    if LBinding.FNode <> ANode then
    begin
      Continue;
    end;

    if LBinding.FCustom or LBinding.FComposing or (FactoryIndex(ANode) >= 0) or
      not ((LBinding.FInput is TJSHTMLInputElement) or
        (LBinding.FInput is TJSHTMLTextAreaElement)) then
    begin
      Exit;
    end;
    LValue := ANode.Prop('value');
    FUpdating := True;
    try

      if TJSHTMLInputElement(LBinding.FInput).value <> LValue then
      begin
        TJSHTMLInputElement(LBinding.FInput).value := LValue;
      end;
      LBinding.FHasValueBaseline := True;
      LBinding.FLastValue := LValue;
    finally
      FUpdating := False;
    end;
    Result := True;
    QueueContent;
    Exit;
  end;
end;

procedure TNyxBrowserRenderer.RestoreDisclosureFocus(AHeader: TJSHTMLElement;
  AOrigin: TNyxNode; AActive: TJSNode; const AEvents: INyxEvents; ARevision: Integer);
var
  LGeneration: TBrowserViewGeneration;
  LGenerationLease: IInterface;
  LBeforeExit: TJSNode;
  LExitDispatch: TNyxDispatch;
begin
  LGeneration := TBrowserViewGeneration(FGenerationObject);
  LGenerationLease := FGeneration;

  if (LGeneration = nil) or not LGeneration.Current then
  begin
    Exit;
  end;
  { A hidden input may already have lost DOM focus while callbacks were guarded.
    Dispatch its exit only after accepted command retirement. Callback redirects
    and mount retirement take precedence over returning focus to the header. }

  if document.activeElement = AActive then
  begin
    TJSHTMLElement(AActive).blur;

    if (AEvents.ViewRevision <> ARevision) or not LGeneration.Current or
      ((document.activeElement <> nil) and (document.activeElement <> document.body)) then
    begin
      Exit;
    end;
  end
  else
  begin
    LBeforeExit := document.activeElement;
    LExitDispatch := FLiveBindings.Focus(AOrigin, ntAfterExit);

    if not LGeneration.Current then
    begin
      Exit;
    end;
    Emit(AOrigin, LExitDispatch);

    if (AEvents.ViewRevision <> ARevision) or not LGeneration.Current or
      (document.activeElement <> LBeforeExit) then
    begin
      Exit;
    end;
  end;
  { Last borrowed call: enter handlers may dispose this renderer. }
  AHeader.focus;
end;

procedure TNyxBrowserRenderer.CompleteDisclosureFocus;
var
  LGeneration: TBrowserViewGeneration;
  LGenerationLease: IInterface;
  LOrigin: TNyxNode;
  LHeader: TNyxNode;
  LEvents: INyxEvents;
  LActive: TJSNode;
  LRevision: Integer;
begin
  LGeneration := TBrowserViewGeneration(FGenerationObject);
  LGenerationLease := FGeneration;

  if (LGeneration = nil) or not LGeneration.Current then
  begin
    Exit;
  end;
  LEvents := FEvents;
  LRevision := FDisclosureFocusRevision;

  if (LEvents.ViewRevision <> LRevision) or not LGeneration.Current then
  begin
    Exit;
  end;

  if ((document.activeElement <> FDisclosureFocusExpected) and
      (document.activeElement <> document.body)) or
    (FLiveBindings = nil) or not FLiveBindings.RefreshReady then
  begin
    Exit;
  end;

  if not LGeneration.Current then
  begin
    Exit;
  end;
  LOrigin := FRoot.Find(FDisclosureFocusOrigin);
  LHeader := NyxDisclosureClosingHeader(LOrigin);

  if (LHeader = nil) or (LHeader.ID <> FDisclosureFocusHeader) or
    not NyxInteractionPolicy(LHeader).CanIssueCommand then
  begin
    Exit;
  end;
  LActive := FDisclosureFocusActive;
  LRevision := FDisclosureFocusRevision;
  FDisclosureFocusActive := nil;
  FDisclosureFocusExpected := nil;
  FDisclosureFocusOrigin := '';
  FDisclosureFocusHeader := '';
  { Retain the revision guard independently if an exit/enter callback disposes
    the renderer. A borrowed interface argument alone is not this owner. }
  LEvents := FEvents;
  RestoreDisclosureFocus(ElementFor(LHeader.ID), LOrigin, LActive, LEvents, LRevision);
end;

procedure TNyxBrowserRenderer.Sync;
const
  CMetricKeys: array[0..5] of TNyxText = ('width', 'height', 'gap', 'padding', 'left', 'top');
var
  LGeneration: TBrowserViewGeneration;
  LGenerationLease: IInterface;
  LDrawingPlan: INyxDrawingPlan;
  LDrawingProvider: INyxDrawingProvider;
  LDrawingTheme: TNyxTheme;
  LViewportFrame: TNyxViewFrame;
  LIndex: Integer;
  LMetricIndex: Integer;
  LBinding: TNyxBrowserBinding;
  LCanvasAccessibility: INyxCanvasAccessibilityView;
  LCanvasName: TNyxText;
  LCanvasDescription: TNyxText;
  LNode: TNyxNode;
  LPolicy: TNyxInteractionPolicy;
  LControl: TJSHTMLElement;
  LEnabled: Boolean;
  LReadOnly: Boolean;
  LKeyboardKind: TNyxKind;
  LValue: TNyxText;
  LValueDomain: TNyxValueDomain;
  LDateDomain: TNyxDataValue;
  LDateField: Integer;
  LTimeDomain: TNyxDataValue;
  LTimeField: Integer;
  LTimeStep: Integer;
  LTimeFraction: TNyxText;
  LDisclosureRoot: TNyxNode;
  LDisclosureBody: TNyxNode;
  LDisclosureCaption: TNyxText;
  LDisclosureFocus: TJSHTMLElement;
  LActive: TJSNode;
  LFocusHeader: TNyxNode;
  LFocusEvents: INyxEvents;
  LFocusRevision: Integer;
  LFocusOrigin: TNyxNode;
  LSliderScale: TNyxSliderScale;
  LInfo: TNyxPrimitiveInfo;
  LLayout: TNyxText;
  LMinimum: Integer;
  LMaximum: Integer;
  LFocusScheduler: INyxScheduler;
  LFocusExecution: INyxExecution;

  procedure AttributeFlag(AElement: TJSHTMLElement; const AName: TNyxText; ASet: Boolean);
  begin

    if ASet then
    begin
      AElement.setAttribute(AName, '');
    end
    else
    begin
      AElement.removeAttribute(AName);
    end;

    if LBinding.FCustom and not LGeneration.Current then
    begin
      raise ENyxBrowserViewRetired.Create('The browser view was retired');
    end;
  end;

  procedure CheckCurrent;
  begin

    if not LGeneration.Current then
    begin
      raise ENyxBrowserViewRetired.Create('The browser view was retired');
    end;
  end;

begin
  LGeneration := TBrowserViewGeneration(FGenerationObject);
  LGenerationLease := FGeneration;

  if (LGeneration = nil) or not LGeneration.Current then
  begin
    Exit;
  end;
  try
    { Resource model adoption precedes row/scalar notifications. Capture its frame
      for later navigation/responsive preparation before updating target controls. }

    if FRoot <> nil then
    begin
      FResourceContext := FRoot.ResourceContext;
      CheckCurrent;
    end;

    if FUpdating then
    begin
      Exit;
    end;
    { Capture physical focus before hiding any body. A state-driven close has no
      header click to move focus for it; do not leave the retained editor focused
      inside invisible content. Unrelated focus is never claimed. }
    LDisclosureFocus := nil;
    LActive := document.activeElement;
    for LIndex := 0 to High(FBindings) do
    begin

      if not FDesignMode and (LActive <> nil) and
        FBindings[LIndex].FElement.contains(LActive) then
      begin
        LFocusHeader := NyxDisclosureClosingHeader(FBindings[LIndex].FNode);

        if (LFocusHeader <> nil) and NyxInteractionPolicy(LFocusHeader).CanIssueCommand then
        begin
          LDisclosureFocus := ElementFor(LFocusHeader.ID);
          LFocusOrigin := FBindings[LIndex].FNode;
        end;
      end;
    end;

    if LDisclosureFocus <> nil then
    begin
      LFocusEvents := FEvents;
      LFocusRevision := LFocusEvents.ViewRevision;
      CheckCurrent;
    end;
    FUpdating := True;
    try

      if FRoot <> nil then
      begin
        { A first rule can arrive after an unobserved ordinary host was resized.
          Read current space before projecting; observer callbacks are subsequent
          automatic notifications, not the authority for initial dimensions. }

        if not FDetachedCandidate then
        begin
          FViewportWidth := FHost.clientWidth;
          FViewportHeight := FHost.clientHeight;
        end;
        LViewportFrame := FViewportBinding.Frame(FViewportWidth, FViewportHeight);
        FViewportBinding.Observe;
        CheckCurrent;
        FRoot.ApplyViewport(LViewportFrame.ViewportWidth, LViewportFrame.ViewportHeight, npfBrowser,
          FPresentationSelection, MeasureContainers);
        CheckCurrent;
      end;
      for LIndex := 0 to Length(FBindings) - 1 do
      begin
        CheckCurrent;
        LBinding := FBindings[LIndex];
        LNode := LBinding.FNode;
        LControl := LBinding.FElement;
        LControl.setAttribute('data-variant', LNode.Prop('variant'));

        if LBinding.FCustom then
        begin
          CheckCurrent;
        end;
        LControl.title := LNode.Prop('hint');

        if LBinding.FCustom then
        begin
          CheckCurrent;
        end;
        LControl.style.removeProperty('container-type');

        if LBinding.FCustom then
        begin
          CheckCurrent;
        end;

        if LNode.QueryContainer.Defined then
        begin

          if LNode.ContainerContainment = nccSize then
          begin
            LControl.style.setProperty('container-type', 'size');

            if LBinding.FCustom then
            begin
              CheckCurrent;
            end;
          end
          else
          begin
            LControl.style.setProperty('container-type', 'inline-size');

            if LBinding.FCustom then
            begin
              CheckCurrent;
            end;
          end;
        end;
        LControl.draggable := not FDesignMode and (LNode.Prop('drag-source') = 'true');

        if LBinding.FCustom then
        begin
          CheckCurrent;
        end;
        LControl.style.setProperty('touch-action', LNode.Prop('touch-behavior', 'auto'));

        if LBinding.FCustom then
        begin
          CheckCurrent;
        end;

        if LNode.Prop('pressed') <> '' then
        begin
          LControl.setAttribute('aria-pressed', LowerCase(LNode.Prop('pressed')));

          if LBinding.FCustom then
          begin
            CheckCurrent;
          end;
        end
        else
        begin
          LControl.removeAttribute('aria-pressed');

          if LBinding.FCustom then
          begin
            CheckCurrent;
          end;
        end;
        LControl.style.removeProperty('display');

        if LBinding.FCustom then
        begin
          CheckCurrent;
        end;
        LControl.style.removeProperty('flex-direction');

        if LBinding.FCustom then
        begin
          CheckCurrent;
        end;
        LControl.style.removeProperty('position');

        if LBinding.FCustom then
        begin
          CheckCurrent;
        end;
        LControl.style.removeProperty('flex-wrap');

        if LBinding.FCustom then
        begin
          CheckCurrent;
        end;
        LControl.style.removeProperty('align-items');

        if LBinding.FCustom then
        begin
          CheckCurrent;
        end;
        LControl.style.removeProperty('justify-content');

        if LBinding.FCustom then
        begin
          CheckCurrent;
        end;
        LControl.style.removeProperty('align-content');

        if LBinding.FCustom then
        begin
          CheckCurrent;
        end;
        LControl.classList.remove('nyx-flow-row');

        if LBinding.FCustom then
        begin
          CheckCurrent;
        end;
        LControl.classList.remove('nyx-flow-column');

        if LBinding.FCustom then
        begin
          CheckCurrent;
        end;
        LControl.classList.remove('nyx-aligned');

        if LBinding.FCustom then
        begin
          CheckCurrent;
        end;
        LLayout := LNode.Prop('layout');

        if (LLayout = '') and not LBinding.FCustom and
          FindNyxPrimitive(LNode.ProjectionKind, LInfo) and LInfo.Container and
          (LNode.ProjectionKind <> 'split-view') then
        begin
          LLayout := NyxLayout(LNode);
        end;

        if LLayout = 'grid' then
        begin
          LControl.style.setProperty('display', 'grid');

          if LBinding.FCustom then
          begin
            CheckCurrent;
          end;
        end
        else if (LLayout = 'row') or (LLayout = 'column') then
        begin
          LControl.style.setProperty('display', 'flex');

          if LBinding.FCustom then
          begin
            CheckCurrent;
          end;
          LControl.style.setProperty('flex-direction', LLayout);

          if LBinding.FCustom then
          begin
            CheckCurrent;
          end;
          LControl.classList.add('nyx-flow-' + LLayout);

          if LBinding.FCustom then
          begin
            CheckCurrent;
          end;
          { Wrapped rows pack natural lines at the leading cross edge. A definite
            height does not stretch every line's height behind the author's back. }
          LControl.style.setProperty('align-content', 'flex-start');

          if LBinding.FCustom then
          begin
            CheckCurrent;
          end;
          LValue := LNode.Prop('flow-wrap', 'auto');

          if (LLayout = 'row') and (LValue <> '') and (LValue <> 'auto') then
          begin
            LControl.style.setProperty('flex-wrap', LValue);

            if LBinding.FCustom then
            begin
              CheckCurrent;
            end;
          end;
          LValue := LNode.Prop('cross-alignment', 'auto');

          if (LValue <> '') and (LValue <> 'auto') then
          begin
            LControl.classList.add('nyx-aligned');

            if LBinding.FCustom then
            begin
              CheckCurrent;
            end;

            if (LValue = 'start') or (LValue = 'end') then
            begin
              LValue := 'flex-' + LValue;
            end;

            if (LValue = 'center') or (LValue = 'flex-end') then
            begin
              LValue := 'safe ' + LValue;
            end;
            LControl.style.setProperty('align-items', LValue);

            if LBinding.FCustom then
            begin
              CheckCurrent;
            end;
          end;
          LValue := LNode.Prop('justification', 'start');

          if LValue <> '' then
          begin

            if (LValue = 'start') or (LValue = 'end') then
            begin
              LValue := 'flex-' + LValue;
            end;

            if (LValue = 'center') or (LValue = 'flex-end') then
            begin
              LValue := 'safe ' + LValue;
            end;
            LControl.style.setProperty('justify-content', LValue);

            if LBinding.FCustom then
            begin
              CheckCurrent;
            end;
          end;
        end
        else if LLayout = 'absolute' then
        begin
          LControl.style.setProperty('display', 'block');

          if LBinding.FCustom then
          begin
            CheckCurrent;
          end;
          { Coordinates belong to this host's content box, not the page body. }
          LControl.style.setProperty('position', 'relative');

          if LBinding.FCustom then
          begin
            CheckCurrent;
          end;
        end;

        if (LNode.Parent <> nil) and (NyxLayout(LNode.Parent) = 'absolute') then
        begin
          LControl.style.setProperty('position', 'absolute');

          if LBinding.FCustom then
          begin
            CheckCurrent;
          end;
        end;

        if not LBinding.FCustom and (LNode.ProjectionKind <> 'card') then
        begin

          if LNode.Prop('surface') = 'true' then
          begin
            LControl.classList.add('nyx-card');

            if LBinding.FCustom then
            begin
              CheckCurrent;
            end;
          end
          else
          begin
            LControl.classList.remove('nyx-card');

            if LBinding.FCustom then
            begin
              CheckCurrent;
            end;
          end;
        end;

        if NyxSupportsViewport(LNode) and (LBinding.FInput = nil) then
        begin
          { Sync must retain the same scroll face as initial projection. A table
            otherwise reverts to intrinsic height and silently loses its viewport. }
          LControl.style.removeProperty('overflow');

          if LBinding.FCustom then
          begin
            CheckCurrent;
          end;

          if (LNode.Prop('height') <> '') or (LNode.ProjectionKind = 'scroll') then
          begin
            LControl.style.setProperty('overflow', 'auto');

            if LBinding.FCustom then
            begin
              CheckCurrent;
            end;
          end;

          if (LNode.ProjectionKind = 'table') and (LNode.Prop('height') <> '') then
          begin
            LControl.style.setProperty('display', 'block');

            if LBinding.FCustom then
            begin
              CheckCurrent;
            end;
          end;
        end;

        if not NyxInteractionPolicy(LNode).Visible then
        begin
          LControl.style.setProperty('display', 'none');

          if LBinding.FCustom then
          begin
            CheckCurrent;
          end;
        end;
        for LMetricIndex := 0 to High(CMetricKeys) do
        begin
          LValue := LNode.Prop(CMetricKeys[LMetricIndex]);
          { Match native allocation and initial mounting: only an absolute parent
            consumes its child's origin. Removing stale inline offsets is necessary
            because ordinary Nyx faces remain relatively positioned CSS elements. }

          if ((CMetricKeys[LMetricIndex] = 'left') or
            (CMetricKeys[LMetricIndex] = 'top')) and
            ((LNode.Parent = nil) or (NyxLayout(LNode.Parent) <> 'absolute')) then
          begin
            LValue := '';
          end;

          if LValue = '' then
          begin
            LControl.style.removeProperty(CMetricKeys[LMetricIndex]);

            if LBinding.FCustom then
            begin
              CheckCurrent;
            end;
          end
          else
          begin
            LControl.style.setProperty(CMetricKeys[LMetricIndex], LValue + 'px');

            if LBinding.FCustom then
            begin
              CheckCurrent;
            end;
          end;
        end;
        { Sizing policies are translated only here. Retained pixel metrics become
          effective again after Automatic/Clear; Sync never changes descriptor data. }
        LValue := LNode.Prop('width-sizing');

        if LValue = 'content' then
        begin
          LControl.style.setProperty('width', 'fit-content');

          if LBinding.FCustom then
          begin
            CheckCurrent;
          end;
        end
        else if LValue = 'fill' then
        begin
          LControl.style.setProperty('width', '100%');

          if LBinding.FCustom then
          begin
            CheckCurrent;
          end;
        end;
        LValue := LNode.Prop('height-sizing');
        LControl.style.removeProperty('min-height');

        if LBinding.FCustom then
        begin
          CheckCurrent;
        end;

        if LValue = 'content' then
        begin
          LControl.style.setProperty('height', 'max-content');

          if LBinding.FCustom then
          begin
            CheckCurrent;
          end;
          LControl.style.setProperty('min-height', '0');

          if LBinding.FCustom then
          begin
            CheckCurrent;
          end;
        end
        else if LValue = 'fill' then
        begin
          LControl.style.setProperty('height', '100%');

          if LBinding.FCustom then
          begin
            CheckCurrent;
          end;
          LControl.style.setProperty('min-height', '0');

          if LBinding.FCustom then
          begin
            CheckCurrent;
          end;
        end;
        { Apply admitted pixel bounds after sizing defaults. Unsetting a bound
          removes stale inline CSS, restoring the theme's containing-width cap.
          An explicit minimum can still exceed that cap under CSS sizing rules.
          Flexbox itself redistributes capped weights; no parallel browser layout
          engine is maintained. These are adapter wire keys, not authoring API. }
        LValue := LNode.Prop('min-width');

        if LValue = '' then
        begin
          LControl.style.removeProperty('min-width');

          if LBinding.FCustom then
          begin
            CheckCurrent;
          end;
        end
        else
        begin
          LControl.style.setProperty('min-width', LValue + 'px');

          if LBinding.FCustom then
          begin
            CheckCurrent;
          end;
        end;
        LValue := LNode.Prop('max-width');

        if LValue = '' then
        begin
          LControl.style.removeProperty('max-width');

          if LBinding.FCustom then
          begin
            CheckCurrent;
          end;
        end
        else
        begin
          LControl.style.setProperty('max-width', 'min(100%, ' + LValue + 'px)');

          if LBinding.FCustom then
          begin
            CheckCurrent;
          end;
        end;
        LValue := LNode.Prop('min-height');

        if LValue <> '' then
        begin
          LControl.style.setProperty('min-height', LValue + 'px');

          if LBinding.FCustom then
          begin
            CheckCurrent;
          end;
        end;
        LValue := LNode.Prop('max-height');

        if LValue = '' then
        begin
          LControl.style.removeProperty('max-height');

          if LBinding.FCustom then
          begin
            CheckCurrent;
          end;
        end
        else
        begin
          LControl.style.setProperty('max-height', LValue + 'px');

          if LBinding.FCustom then
          begin
            CheckCurrent;
          end;
        end;
        LControl.style.removeProperty('flex');

        if LBinding.FCustom then
        begin
          CheckCurrent;
        end;
        LControl.classList.remove('nyx-flex');

        if LBinding.FCustom then
        begin
          CheckCurrent;
        end;
        LControl.style.removeProperty('grid-template-columns');

        if LBinding.FCustom then
        begin
          CheckCurrent;
        end;

        if (LNode.Prop('flex') <> '') or (NyxFlexWeight(LNode) > 0) then
        begin

          if NyxFlexWeight(LNode) > 0 then
          begin
            LControl.style.setProperty('flex', IntToStr(NyxFlexWeight(LNode)));

            if LBinding.FCustom then
            begin
              CheckCurrent;
            end;
            LControl.classList.add('nyx-flex');

            if LBinding.FCustom then
            begin
              CheckCurrent;
            end;
          end
          else
          begin
            LControl.style.setProperty('flex', '0 0 auto');

            if LBinding.FCustom then
            begin
              CheckCurrent;
            end;
          end;
        end;

        if LNode.Prop('columns') <> '' then
        begin
          LControl.style.setProperty('grid-template-columns',
            'repeat(' + LNode.Prop('columns') + ',minmax(0,1fr))');

          if LBinding.FCustom then
          begin
            CheckCurrent;
          end;
        end;
        LPolicy := NyxInteractionPolicy(LNode);
        { The header is an ordinary button; its linked retained body and desired
          state come from the portable compound contract. Authored visibility
          remains independent of that expansion gate. }

        if NyxDisclosureHeader(LNode, LDisclosureRoot, LDisclosureBody, LDisclosureCaption) then
        begin
          LBinding.FElement.setAttribute('aria-expanded',
            BoolToStr(NyxInteractionPolicy(LDisclosureBody).Visible, 'true', 'false'));

          if LBinding.FCustom then
          begin
            CheckCurrent;
          end;
          LBinding.FElement.setAttribute('aria-controls', LDisclosureBody.ID);

          if LBinding.FCustom then
          begin
            CheckCurrent;
          end;
          LBinding.FElement.classList.add('nyx-disclosure-header');

          if LBinding.FCustom then
          begin
            CheckCurrent;
          end;
          ElementFor(LDisclosureBody.ID).id := LDisclosureBody.ID;
        end;
        LEnabled := LPolicy.Enabled;
        LReadOnly := LPolicy.ReadOnly;

        if LBinding.FCanvasView <> nil then
        begin
          { Name the retained keyboard-focusable region, including effective
            state/resource/computed authoring, rather than only its outer panel. }
          LCanvasName := LNode.Prop('aria-label', NyxDefaultCanvasAccessibleName);
          LCanvasDescription := LNode.Prop('hint', NyxCanvasKeyboardHelp);

          if Supports(LBinding.FCanvasView, INyxCanvasAccessibilityView, LCanvasAccessibility) then
          begin
            CheckCurrent;
            LCanvasAccessibility.SetAccessibleMetadata(LCanvasName, LCanvasDescription);
            CheckCurrent;
          end;
          CheckCurrent;
          LBinding.FCanvasView.Constrain(LEnabled, LReadOnly);
          CheckCurrent;
        end;
        AttributeFlag(LControl, 'disabled', not LEnabled);
        LControl.setAttribute('aria-disabled', LowerCase(BoolToStr(not LEnabled, True)));

        if LBinding.FCustom then
        begin
          CheckCurrent;
        end;

        if (LNode.ProjectionKind = 'memo') and (LNode.Prop('height') <> '') then
        begin
          LControl.classList.add('nyx-sized-field');

          if LBinding.FCustom then
          begin
            CheckCurrent;
          end;
        end
        else
        begin
          LControl.classList.remove('nyx-sized-field');

          if LBinding.FCustom then
          begin
            CheckCurrent;
          end;
        end;

        if LBinding.FTabs <> nil then
        begin
          LBinding.FTabs.Enabled(LEnabled);

          LBinding.FTabs.SynchronizeDescriptor(LNode);
        end;

        if not LBinding.FCustom and (LBinding.FInput = nil) and
          NyxSupportsKeyboard(LNode) and TryNyxKind(LNode.ProjectionKind, LKeyboardKind) then
        begin
          case LKeyboardKind of
            nkCode, nkList, nkTable, nkTree:
              begin
                { These standard faces have no intrinsic HTML Tab entry. A
                  disabled static face loses tabindex altogether; a negative
                  tabindex would still allow programmatic/click focus. Read-only
                  retains inspection. The managed collection owns its own entry. }

                if LBinding.FCollectionMount = nil then
                begin

                  if LEnabled then
                  begin
                    LControl.setAttribute('tabindex', '0');

                    if LBinding.FCustom then
                    begin
                      CheckCurrent;
                    end;
                  end
                  else
                  begin
                    LControl.removeAttribute('tabindex');

                    if LBinding.FCustom then
                    begin
                      CheckCurrent;
                    end;
                  end;
                end;
              end;
            nkLink:
              begin
                { An anchor ignores HTML disabled. Retain its accessible link
                  identity while withdrawing navigation/focus, then restore the
                  authored URL on re-enable. Do not hide disabled content with
                  inert or manufacture a positive focus order. }
                LControl.setAttribute('role', 'link');

                if LBinding.FCustom then
                begin
                  CheckCurrent;
                end;

                if LEnabled then
                begin
                  LValue := LNode.Prop('href', '#');

                  if not SafeURL(LValue, False) then
                  begin
                    raise ENyxModel.Create('Unsupported link URL');
                  end;
                  LControl.setAttribute('href', LValue);

                  if LBinding.FCustom then
                  begin
                    CheckCurrent;
                  end;
                end
                else
                begin
                  LControl.removeAttribute('href');

                  if LBinding.FCustom then
                  begin
                    CheckCurrent;
                  end;
                end;
              end;
            else
              begin
                { Other standard faces already supply their intrinsic focus and
                  accessibility behavior; this branch makes no additional change. }
              end;
          end;
        end;

        if LBinding.FCollectionMount <> nil then
        begin
          LBinding.FCollectionMount.SetInteraction(LEnabled and not FDesignMode,
            LReadOnly or FDesignMode);
          CheckCurrent;
        end;
        LBinding.SyncLiteralItems;
        CheckCurrent;

        if not LBinding.FCustom and (LBinding.FNode.ProjectionKind = 'drawing') then
        begin

          if LBinding.FDrawingView = nil then
          begin
            LBinding.FDrawingView := TNyxBrowserDrawing.Create(LBinding.FElement,
              @LBinding.RetainView);
          end;

          if LBinding.FDrawingSource = nil then
          begin
            LBinding.FDrawingSource := NewNyxDrawingSource;
          end;
          LDrawingTheme := FTheme;
          LDrawingProvider := DrawingProvider(LBinding.FNode);
          LDrawingPlan := LBinding.FDrawingSource.Read(LBinding.FNode);
          CheckCurrent;
          LBinding.FDrawingView.Configure(LDrawingPlan, LDrawingProvider,
            LDrawingTheme, LBinding.FNode.Prop('text'));
          CheckCurrent;
        end;

        if LBinding.FCaption <> nil then
        begin
          LBinding.FCaption.textContent := LNode.Prop('text');

          if LBinding.FCustom then
          begin
            CheckCurrent;
          end;
        end
        else if not LBinding.FCustom and (LNode.Count = 0) and
          ((LNode.ProjectionKind = 'label') or (LNode.ProjectionKind = 'heading') or
          ((LNode.ProjectionKind = 'button') and (LBinding.FButtonGlyphImage = nil)) or (LNode.ProjectionKind = 'link') or
          (LNode.ProjectionKind = 'badge') or (LNode.ProjectionKind = 'alert') or
          (LNode.ProjectionKind = 'avatar') or (LNode.ProjectionKind = 'code')) then
        begin
          LControl.textContent := LNode.Prop('text');

          if LBinding.FCustom then
          begin
            CheckCurrent;
          end;
          { A default disclosure header inherits its root caption in place. }

          if NyxDisclosureHeader(LNode, LDisclosureRoot, LDisclosureBody, LDisclosureCaption) then
          begin
            LControl.textContent := LDisclosureCaption;

            if LBinding.FCustom then
            begin
              CheckCurrent;
            end;
          end;
        end;

        LBinding.SyncButtonFace;
        CheckCurrent;

        if LControl is TJSHTMLProgressElement then
        begin
          { HTML progress has an intrinsic minimum of zero. Project the declared
            interval while retaining its actual scalar bounds in accessibility. }
          LMinimum := StrToIntDef(LNode.Prop('min'), 0);
          LMaximum := StrToIntDef(LNode.Prop('max'), 100);
          TJSHTMLProgressElement(LControl).max := Max(1, LMaximum - LMinimum);

          if LBinding.FCustom then
          begin
            CheckCurrent;
          end;
          LControl.setAttribute('aria-valuemin', IntToStr(LMinimum));

          if LBinding.FCustom then
          begin
            CheckCurrent;
          end;
          LControl.setAttribute('aria-valuemax', IntToStr(LMaximum));

          if LBinding.FCustom then
          begin
            CheckCurrent;
          end;

          if LNode.Prop(NyxAttributeName(atIndeterminate), 'false') = 'true' then
          begin
            { Removing the native HTML value selects genuine indeterminate
              progress. Never announce a stale percentage during unknown work. }
            LControl.removeAttribute('value');

            if LBinding.FCustom then
            begin
              CheckCurrent;
            end;
            LControl.removeAttribute('aria-valuenow');

            if LBinding.FCustom then
            begin
              CheckCurrent;
            end;
          end
          else
          begin
            { Unrelated refresh must not briefly create/remove a value while
              activity is indeterminate; that can restart host animation. }
            TJSHTMLProgressElement(LControl).value :=
              StrToIntDef(LNode.Prop('value'), 0) - LMinimum;

            if LBinding.FCustom then
            begin
              CheckCurrent;
            end;

            if LMaximum = LMinimum then
            begin
              TJSHTMLProgressElement(LControl).value := 1;

              if LBinding.FCustom then
              begin
                CheckCurrent;
              end;
            end;
            LControl.setAttribute('aria-valuenow', LNode.Prop('value', '0'));

            if LBinding.FCustom then
            begin
              CheckCurrent;
            end;
          end;
        end;

        if not LBinding.FCustom and (LControl is TJSHTMLImageElement) then
        begin
          LValue := LNode.Prop('src');

          if not SafeURL(LValue, True) then
          begin
            raise ENyxModel.Create('Unsupported image URL');
          end;

          LBinding.SyncImage;
          CheckCurrent;
          TJSHTMLImageElement(LControl).alt := LNode.Prop('alt', LNode.Prop('text'));

          if LBinding.FCustom then
          begin
            CheckCurrent;
          end;
          ApplyNyxBrowserImage(LNode, TJSHTMLImageElement(LControl));
        end;

        if LNode.Prop('aria-label') <> '' then
        begin
          LControl.setAttribute('aria-label', LNode.Prop('aria-label'));

          if LBinding.FCustom then
          begin
            CheckCurrent;
          end;
        end
        else if (LBinding.FButtonGlyphImage <> nil) and
          (LBinding.FButtonPresentation.ContentMode = nbcGlyphOnly) then
        begin
          LControl.setAttribute('aria-label', LNode.Prop('text', LNode.ID));

          if LBinding.FCustom then
          begin
            CheckCurrent;
          end;
        end
        else if not LBinding.FCustom and (LBinding.FInput = nil) and
          (LBinding.FCollectionMount = nil) and
          TryNyxKind(LNode.ProjectionKind, LKeyboardKind) and
          (LKeyboardKind in [nkCode, nkList, nkTable, nkTree, nkDrawing]) then
        begin
          { The same authored caption names native static inspection faces. }
          LControl.setAttribute('aria-label', LNode.Prop('text', LNode.ID));

          if LBinding.FCustom then
          begin
            CheckCurrent;
          end;
        end
        else
        begin
          LControl.removeAttribute('aria-label');

          if LBinding.FCustom then
          begin
            CheckCurrent;
          end;
        end;

        if Assigned(LBinding.FUpdater) then
        begin
          LBinding.FUpdater(LNode, LBinding.FElement);
          CheckCurrent;
        end;

        if LBinding.FInput <> nil then
        begin
          LControl := LBinding.FInput;
          AttributeFlag(LControl, 'disabled', not LEnabled);
          AttributeFlag(LControl, 'readonly', LReadOnly);
          LControl.setAttribute('aria-readonly', LowerCase(BoolToStr(LReadOnly, True)));

          if LBinding.FCustom then
          begin
            CheckCurrent;
          end;
          LControl.title := LNode.Prop('hint');

          if LBinding.FCustom then
          begin
            CheckCurrent;
          end;
          LControl.setAttribute('placeholder', LNode.Prop('placeholder'));

          if LBinding.FCustom then
          begin
            CheckCurrent;
          end;
          LControl.setAttribute('aria-label', LNode.Prop('aria-label', LNode.Prop('text', LNode.ID)));

          if LBinding.FCustom then
          begin
            CheckCurrent;
          end;

          if LBinding.FColorField <> nil then
          begin
            LBinding.FColorField.Sync(TNyxRGBColor.FromText(LNode.Prop('value')),
              NyxNodeValueDomain(LNode), LEnabled, LReadOnly, LPolicy.Visible,
              LNode.Prop('aria-label', LNode.Prop('text', LNode.ID)));
            CheckCurrent;
          end;

          if not LBinding.FCustom and not LBinding.FComposing and
            (LNode.ProjectionKind = 'input') then
          begin
            LValue := LNode.Prop('input-type', 'text');

            if LValue = '' then
            begin
              LValue := 'text';
            end;

            if TJSHTMLInputElement(LControl)._type <> LValue then
            begin
              TJSHTMLInputElement(LControl)._type := LValue;

              if LBinding.FCustom then
              begin
                CheckCurrent;
              end;
            end;

            if LValue = 'number' then
            begin
              LControl.setAttribute('step', 'any');

              if LBinding.FCustom then
              begin
                CheckCurrent;
              end;
            end
            else
            begin
              LControl.removeAttribute('step');

              if LBinding.FCustom then
              begin
                CheckCurrent;
              end;
            end;
          end;

          if (LNode.ProjectionKind = 'slider') and not LBinding.FCustom then
          begin
            LSliderScale := TNyxSliderScale.Create(NyxNodeValueDomain(LNode),
              StrToIntDef(LNode.Prop('min'), 0), StrToIntDef(LNode.Prop('max'), 100),
              StrToIntDef(LNode.Prop('slider-intervals'), 1000));
            LBinding.FSliderValue.Accept(LSliderScale, LNode.Prop('value', '0'));
            LControl.setAttribute('min', '0');

            if LBinding.FCustom then
            begin
              CheckCurrent;
            end;
            LControl.setAttribute('max', IntToStr(LSliderScale.MaximumPosition));

            if LBinding.FCustom then
            begin
              CheckCurrent;
            end;
            LControl.setAttribute('step', '1');

            if LBinding.FCustom then
            begin
              CheckCurrent;
            end;
            { Accessibility exposes numeric meaning, never internal ordinal ticks. }
            LControl.setAttribute('aria-valuemin', TNyxStateValue.FromNumber(LSliderScale.Minimum).NumberText);

            if LBinding.FCustom then
            begin
              CheckCurrent;
            end;
            LControl.setAttribute('aria-valuemax', TNyxStateValue.FromNumber(LSliderScale.Maximum).NumberText);

            if LBinding.FCustom then
            begin
              CheckCurrent;
            end;
            LControl.setAttribute('aria-valuenow', LNode.Prop('value', '0'));

            if LBinding.FCustom then
            begin
              CheckCurrent;
            end;
            LControl.setAttribute('aria-valuetext', LNode.Prop('value', '0'));

            if LBinding.FCustom then
            begin
              CheckCurrent;
            end;
            TJSHTMLInputElement(LControl).value := IntToStr(LBinding.FSliderValue.Position);

            if LBinding.FCustom then
            begin
              CheckCurrent;
            end;
          end
          else if (LNode.ProjectionKind = 'spin') or (LNode.ProjectionKind = 'slider') then
          begin
            LControl.setAttribute('min', LNode.Prop('min', '0'));

            if LBinding.FCustom then
            begin
              CheckCurrent;
            end;
            LControl.setAttribute('max', LNode.Prop('max', '100'));

            if LBinding.FCustom then
            begin
              CheckCurrent;
            end;
          end
          else
          begin
            LControl.removeAttribute('min');

            if LBinding.FCustom then
            begin
              CheckCurrent;
            end;
            LControl.removeAttribute('max');

            if LBinding.FCustom then
            begin
              CheckCurrent;
            end;

            if LNode.Prop('min') <> '' then
            begin
              LControl.setAttribute('min', LNode.Prop('min'));

              if LBinding.FCustom then
              begin
                CheckCurrent;
              end;
            end;

            if LNode.Prop('max') <> '' then
            begin
              LControl.setAttribute('max', LNode.Prop('max'));

              if LBinding.FCustom then
              begin
                CheckCurrent;
              end;
            end;
          end;

          if LNode.ProjectionKind = 'date' then
          begin
            { Calendar bounds share the exact typed domain used by value admission;
              a numeric min/max property never supplies a date interpretation. }
            LValueDomain := NyxNodeValueDomain(LNode);
            LControl.removeAttribute('min');

            if LBinding.FCustom then
            begin
              CheckCurrent;
            end;
            LControl.removeAttribute('max');

            if LBinding.FCustom then
            begin
              CheckCurrent;
            end;

            if LValueDomain.CalendarDate then
            begin
              LDateDomain := LValueDomain.ToData;
              for LDateField := 0 to LDateDomain.Count - 1 do
              begin

                if LDateDomain.Key(LDateField) = 'min' then
                begin
                  LControl.setAttribute('min', LDateDomain.Field('min').AsText);

                  if LBinding.FCustom then
                  begin
                    CheckCurrent;
                  end;
                  LControl.setAttribute('max', LDateDomain.Field('max').AsText);

                  if LBinding.FCustom then
                  begin
                    CheckCurrent;
                  end;
                end;
              end;
            end;
          end;

          if LNode.ProjectionKind = 'time' then
          begin
            { HTML time uses seconds for step; the portable contract owns exact
              integer milliseconds and a midnight/minimum base. The default is
              any exact reading, so never inherit the browser's minute default.
              Raw numeric bounds do not supply clock semantics. }
            LValueDomain := NyxNodeValueDomain(LNode);
            LControl.removeAttribute('min');

            if LBinding.FCustom then
            begin
              CheckCurrent;
            end;
            LControl.removeAttribute('max');

            if LBinding.FCustom then
            begin
              CheckCurrent;
            end;
            LControl.setAttribute('step', 'any');

            if LBinding.FCustom then
            begin
              CheckCurrent;
            end;

            if LValueDomain.ClockTime then
            begin
              LTimeDomain := LValueDomain.ToData;
              for LTimeField := 0 to LTimeDomain.Count - 1 do
              begin

                if (LTimeDomain.Key(LTimeField) = 'min') or
                  (LTimeDomain.Key(LTimeField) = 'max') then
                begin
                  LControl.setAttribute(LTimeDomain.Key(LTimeField),
                    LTimeDomain.Field(LTimeDomain.Key(LTimeField)).AsText);

                  if LBinding.FCustom then
                  begin
                    CheckCurrent;
                  end;
                end;
              end;
              LTimeStep := LValueDomain.TimeStepMilliseconds;

              if LTimeStep > 0 then
              begin
                LValue := IntToStr(LTimeStep div 1000);

                if LTimeStep mod 1000 <> 0 then
                begin
                  LTimeFraction := Copy(TNyxText(IntToStr(LTimeStep mod 1000 + 1000)), 2, 3);
                  while LTimeFraction[Length(LTimeFraction)] = '0' do
                  begin
                    SetLength(LTimeFraction, Length(LTimeFraction) - 1);
                  end;
                  LValue := LValue + '.' + LTimeFraction;
                end;
                LControl.setAttribute('step', LValue);

                if LBinding.FCustom then
                begin
                  CheckCurrent;
                end;
              end;
            end;
          end;
          LValue := LNode.Prop('value');
          { The ordinal range already consumed the complete numeric publication.
            Writing numeric text again would narrow it through HTML sanitization. }

          if (LNode.ProjectionKind = 'slider') and not LBinding.FCustom then
          begin
            Continue;
          end;
          { Layout/state publications must not replace text owned by an IME. The
            accepted baseline remains pending until the real composition end. }

          if LBinding.FComposing then
          begin
            Continue;
          end;
          { A different key may update while this field holds an unfinished draft.
            Only an accepted change to its own value replaces that physical draft;
            failed commands explicitly force restoration above. }

          if not (FForceValues and ((FForceValueNode = nil) or
            (FForceValueNode = LNode))) and LBinding.FHasValueBaseline and
            (LBinding.FLastValue = LValue) then
          begin
            Continue;
          end;
          LBinding.FHasValueBaseline := True;
          LBinding.FLastValue := LValue;
          { Setting the same value can reset selection/caret on mobile browsers.
            Keep unchanged physical values, focus and unfinished native drafts. }

          if LControl is TJSHTMLTextAreaElement then
          begin

            if TJSHTMLTextAreaElement(LControl).value <> LValue then
            begin
              TJSHTMLTextAreaElement(LControl).value := LValue;

              if LBinding.FCustom then
              begin
                CheckCurrent;
              end;
            end;
          end
          else if LControl is TJSHTMLSelectElement then
          begin

            if TJSHTMLSelectElement(LControl).value <> LValue then
            begin
              TJSHTMLSelectElement(LControl).value := LValue;

              if LBinding.FCustom then
              begin
                CheckCurrent;
              end;
            end;
          end
          else if (LNode.ProjectionKind = 'checkbox') or (LNode.ProjectionKind = 'switch') or
            (LNode.ProjectionKind = 'radio') then
          begin
            TJSHTMLInputElement(LControl).checked := LValue = 'true';

            if LBinding.FCustom then
            begin
              CheckCurrent;
            end;
          end
          else
          begin

            if TJSHTMLInputElement(LControl).value <> LValue then
            begin
              TJSHTMLInputElement(LControl).value := LValue;

              if LBinding.FCustom then
              begin
                CheckCurrent;
              end;
            end;
          end;
        end;
      end;
      SyncRadioFocus;
      { Generic metrics never override an adapter-owned split layout. Do this
        after all child bindings so their ordinary root styles cannot undo it. }
      for LIndex := 0 to Length(FBindings) - 1 do
      begin

        if FBindings[LIndex].FSplit <> nil then
        begin
          FBindings[LIndex].FSplit.Update;
          CheckCurrent;
        end;
      end;
      UpdateResizePreview;
      CheckCurrent;
      UpdateCanvasResizeGrips;
      CheckCurrent;
    finally

      if LGeneration.Current then
      begin
        FUpdating := False;
        FlushImages;
      end;
    end;
    CheckCurrent;
    QueueContent;
    CheckCurrent;

    if (LDisclosureFocus <> nil) and ((LFocusEvents.ViewRevision = LFocusRevision) and LGeneration.Current) then
    begin
      { Synchronize runs before the admitted command returns. Keep the coordinator's
        no-reentry guard intact: UI work owns deferred focus notification instead. }

      if FLiveBindings.CommandActive then
      begin

        if FDisclosureFocusExecution <> nil then
        begin
          FDisclosureFocusExecution.Cancel;
          CheckCurrent;
        end;
        FDisclosureFocusOrigin := LFocusOrigin.ID;
        FDisclosureFocusHeader := NyxDisclosureClosingHeader(LFocusOrigin).ID;
        FDisclosureFocusActive := LActive;
        FDisclosureFocusExpected := document.activeElement;
        FDisclosureFocusRevision := LFocusRevision;

        if FDisclosureFocusWork = nil then
        begin
          FDisclosureFocusWork := NewNyxContentWork(@CompleteDisclosureFocus);
        end;
        LFocusScheduler := FEvents.Scheduler;
        CheckCurrent;
        LFocusExecution := LFocusScheduler.Submit(FDisclosureFocusWork, neUIQueue);

        if not LGeneration.Current then
        begin
          LFocusExecution.Cancel;
          Exit;
        end;
        FDisclosureFocusExecution := LFocusExecution;
        Exit;
      end;
      RestoreDisclosureFocus(LDisclosureFocus, LFocusOrigin, LActive, LFocusEvents, LFocusRevision);
    end;
  except
    on E: ENyxBrowserViewRetired do
    begin
      { Logical retirement is normal. It never becomes a binding error or a
        stale synchronization/focus tail against the accepted successor. }
    end;
  end;
end;


procedure TNyxBrowserBinding.SetupCanvas(ARenderer: TNyxBrowserRenderer);
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
  ARenderer.OnEvent := @CanvasEvent;
end;

procedure TNyxBrowserBinding.CanvasEvent(ANode: TNyxNode; const AEvent: TNyxEventInfo);
var
  LGeneration: TBrowserViewGeneration;
  LGenerationLease: IInterface;
  LEvents: INyxEvents;
  LCallback: TNyxBrowserEvent;
  LRevision: Integer;
begin
  try
    LGeneration := TBrowserViewGeneration(FGenerationObject);
    LGenerationLease := LGeneration;

    if (LGeneration = nil) or not LGeneration.Current or FDisconnected then
    begin
      Exit;
    end;

    if CurrentRenderer.FUpdating or CurrentRenderer.FPublishingContent then
    begin
      Exit;
    end;
    LEvents := CurrentRenderer.FEvents;
    LCallback := CurrentRenderer.FOnEvent;

    if CurrentRenderer.FDesignMode then
    begin

      if Assigned(LCallback) then
      begin
        LCallback(FNode, NyxDesignEvent(FNode, ntDesignSelect));
      end;
      Exit;
    end;
    LRevision := LEvents.ViewRevision;
    { Dispatch may free this binding and its entire parent renderer. Keep only
      the managed stream and copied legacy callback afterward; retirement changes
      the captured revision and revokes the borrowed child node. }
    LEvents.Dispatch(AEvent, ANode.DesignID, ANode.DesignID);

    if (LEvents.ViewRevision <> LRevision) or not LGeneration.Current then
    begin
      Exit;
    end;

    if Assigned(LCallback) then
    begin
      LCallback(ANode, AEvent);
    end;
  except
    on E: ENyxBrowserViewRetired do
    begin
      Exit;
    end;
  end;
end;

function TNyxBrowserRenderer.ToolCanvasView(const AID: TNyxText;
  AIdentity: TNyxIdentityKind): INyxToolCanvasView;
var
  LIndex: Integer;
  LElement: TJSHTMLElement;
begin
  FEvents.Scheduler.RequireUI;
  LElement := ElementFor(AID, AIdentity);
  for LIndex := 0 to High(FBindings) do
  begin

    if (FBindings[LIndex].FElement = LElement) and
      (FBindings[LIndex].FToolCanvasView <> nil) then
    begin
      Exit(FBindings[LIndex].FToolCanvasView);
    end;
  end;
  raise ENyxModel.Create('Tool Canvas inspector identity is unavailable');
end;

procedure TNyxBrowserRenderer.MountToolCanvases(ADocument: TNyxDocument);
var
  LIndex: Integer;
  LWorkspace: TNyxNode;
  LHost: TNyxNode;
  LBinding: TNyxBrowserBinding;
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
    LBinding.FToolCanvasView := MountNyxBrowserToolCanvas(ADocument,
      CanvasView(LWorkspace.ID, niRuntime), ElementFor(LHost.ID, niRuntime), FState, FTheme,
      NyxControl(LWorkspace.DesignID), LAdoptionHost);
  end;
end;

procedure TNyxBrowserRenderer.ConfigureEmbeddedBehavior(const AContext: INyxApplicationContext;
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

procedure TNyxBrowserRenderer.MountCanvases(ADocument: TNyxDocument);
var
  LIndex: Integer;
  LSerial: Integer;
  LID: TNyxText;
  LWorkspace: INyxCanvasWorkspace;
  LBinding: TNyxBrowserBinding;
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
    LBinding.FCanvasView := MountNyxBrowserCanvasWorkspace(LWorkspace, ADocument,
      LBinding.FElement, FState, FTheme, @LBinding.SetupCanvas, FDesignMode,
      FEmbeddedContext, FEmbeddedResolver, LResources);
    LBinding.FCanvasView.Constrain(NyxInteractionPolicy(LBinding.FNode).Enabled,
      NyxInteractionPolicy(LBinding.FNode).ReadOnly);
  end;
end;

function TNyxBrowserRenderer.CanvasView(const AID: TNyxText;
  AIdentity: TNyxIdentityKind): INyxCanvasWorkspaceView;
var
  LElement: TJSHTMLElement;
  LIndex: Integer;
begin
  LElement := ElementFor(AID, AIdentity);
  for LIndex := 0 to High(FBindings) do
  begin

    if FBindings[LIndex].FElement = LElement then
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

{$I nyx.render.browser.inputs.inc}

end.
