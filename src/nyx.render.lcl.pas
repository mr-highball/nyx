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
  Classes,
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
  nyx.layout.flow,
  nyx.layout.constraints,
  nyx.layout.viewport,
  nyx.interaction,
  nyx.editing,
  nyx.editing.lcl,
  nyx.gestures,
  nyx.designer.input,
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
  nyx.design.tokens,
  nyx.behavior,
  nyx.events,
  nyx.data,
  nyx.viewport,
  nyx.viewport.lcl,
  nyx.viewport.surface.lcl,
  nyx.event.emitter,
  nyx.state,
  nyx.binding,
  nyx.binding.types,
  nyx.collections.view,
  nyx.collections.selection,
  nyx.collections.bindings,
  nyx.collections.view.types,
  nyx.collections.mount,
  nyx.collections.lcl,
  nyx.literal.items,
  nyx.composition,
  nyx.projection.refresh;

type
  TNyxLCLRenderer = class;
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
  TNyxLCLFactory = function(ANode: TNyxNode; AOwner: TComponent): TControl;
  { The initially dormant emitter connects only after complete view admission.
    It carries owned payloads and may outlive the borrowed node/control safely. }
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
    FNode: TNyxNode;
    FControl: TControl;
    FInput: TControl;
    FCaption: TLabel;
    { Value-owned layout and projection. The parent binding is borrowed from the
      same preorder renderer array, never a reference-counted tree back edge. }
    FLogicalParent: TNyxLCLBinding;
    FLogicalBox: TNyxViewportBox;
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
    FCustom: Boolean;
    FUpdater: TNyxLCLUpdater;
    FDeferredValue: Boolean;
    FCommitting: Boolean;
    FComposing: Boolean;
    FEditingContext: TNyxEditingSnapshot;
    FPreviousClick: TNotifyEvent;
    FPreviousInputClick: TNotifyEvent;
    FPreviousEnter: TNotifyEvent;
    FPreviousExit: TNotifyEvent;
    FPreviousKeyDown: TKeyEvent;
    FPreviousKeyUp: TKeyEvent;
    FCollectionMount: INyxCollectionMount;
    { LCL's virtual key callback has no repeat flag. Track admitted key slots and
      clear on focus loss, so a missing outside-view key-up cannot stick a key. }
    FPressedKeys: array[0..255] of Boolean;
    FPointerHooks: array[0..1] of TNyxLCLPointerHooks;
    { Borrowed LCL-owned drag object. Clear before callbacks or view retirement;
      it never owns this binding or its realized node. }
    FActiveDrag: TDragObject;
    procedure SyncLiteralItems;
    procedure SyncPicture;
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
    procedure Wheel(ASender: TObject; AShift: TShiftState; ADelta: Integer;
      APosition: TPoint; var AHandled: Boolean);
    procedure WheelHorz(ASender: TObject; AShift: TShiftState; ADelta: Integer;
      APosition: TPoint; var AHandled: Boolean);
    procedure WheelEvent(ASender: TObject; AShift: TShiftState; ADelta: Integer;
      AHorizontal: Boolean; var AHandled: Boolean);
    procedure CollectionSelectionChanged(const ABefore, AAfter: INyxCollectionSelection);
    procedure SplitLayout(ASender: TObject);
    procedure SplitChanged(ASender: TObject);
    procedure Click(ASender: TObject);
    procedure Change(ASender: TObject);
    procedure CommitValue(ASender: TObject);
    procedure Focus(ASender: TObject);
    procedure Enter(ASender: TObject);
    procedure Leave(ASender: TObject);
    procedure KeyDown(ASender: TObject; var AKey: Word; AShift: TShiftState);
    procedure KeyUp(ASender: TObject; var AKey: Word; AShift: TShiftState);
    procedure Keyboard(ASender: TObject; var AKey: Word; AShift: TShiftState;
      ATrigger: TNyxTrigger);
  end;

  { A native projection using Lazarus controls rather than a browser embedded in
    a window. Custom kinds supply native factories through the same string keys
    used by the browser registry. The renderer never owns the caller's host.
    A supplied theme is borrowed; the default theme is renderer-owned. }
  TNyxLCLRenderer = class
  private
    FTheme: TNyxTheme;
    FOwnTheme: Boolean;
    { Startup palette owns independent values; per-document overrides can be
      removed without leaving stale colors or changing an external theme. }
    FBaseTheme: TNyxTheme;
    { A caller-supplied palette remains borrowed and may change between renders.
      Effective document overlays never mutate it or accumulate into its base. }
    FBorrowedTheme: TNyxTheme;
    FRoot: TNyxNode;
    FPresentationSelection: TNyxPresentationSelection;
    FPresentationView: INyxPresentationViewOwner;
    FPanel: TNyxLogicalScrollBox;
    FVirtualLayout: Boolean;
    FLayouting: Boolean;
    FProjecting: Boolean;
    FBindings: array of TNyxLCLBinding;
    FFactoryKinds: array of TNyxText;
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
    FState: TNyxState;
    FOwnState: Boolean;
    FLiveBindings: TNyxLiveBindings;
    FCollectionBindings: INyxCollectionBindings;
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
    FResizeEdges: array[0..7] of TPanel;
    FCanvasResizeGrips: INyxCanvasResizeGrips;
    FCanvasMoveGrip: INyxCanvasMoveGrip;
    FCanvasMoveView: TNyxLCLRenderer;
    FCanvasMoveHost: TPanel;
    FCanvasGripViews: array[TNyxResizeAxis] of TNyxLCLRenderer;
    FCanvasGripHosts: array[TNyxResizeAxis] of TPanel;
    FSelectionEdges: array[0..3] of TShape;
    FForceValues: Boolean;
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
      AFailure: TNyxBindingFailure = nbfRejected);
    procedure Clear;
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
    procedure SetPresentationSelection(const AValue: TNyxPresentationSelection);
    function ReadPresentationSelection: TNyxPresentationSelection;
    function GetPresentationView: INyxPresentationView;
    procedure Resize(ASender: TObject);
    procedure ProjectViewport(ASender: TObject);
    procedure ArrangeInput(ABinding: TNyxLCLBinding; AWidth, AHeight: Integer);
    { Adorners share the selected face's immediate paint parent. A graphic at
      the scroll host would be occluded by its full-page native child window. }
    procedure UpdateSelection;
    { LCL removes an unchecked radio's TabStop. Restore one entry per native
      peer parent after all value/policy setters, preferring an enabled checked
      item or the first enabled visible item. Creator widgets retain ownership. }
    procedure SyncRadioFocus;
  public
    constructor Create(ATheme: TNyxTheme = nil);
    destructor Destroy; override;
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
      const ACollections: INyxCollectionBindings = nil); overload;
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
    { Move the same mounted view to a different borrowed host, retaining control
      objects, state/subscriptions, focused text range and containing scroll.
      Both hosts must outlive their respective parentage; after successful move
      the old host can be freed. Nil, an unmounted view, a descendant or occupied
      host refuses before changing parenting. A same-host request is a no-op.
      No document/history command occurs. }
    procedure MoveHost(AHost: TWinControl);
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
    { Reveal exact logical bounds through the containing viewport. Presentation
      only: no document/history mutation or control reconstruction. Missing IDs
      refuse; normal/native scrolling retains LCL's own ScrollInView contract. }
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
    { Borrow the actual input through its node identity, independently of the
      adapter's caption/frame hierarchy. Returns nil for mounted non-inputs;
      unmounted identities raise the same error as ControlFor. Never free it. }
    function InputFor(const AID: TNyxText;
      AIdentity: TNyxIdentityKind = niAutomatic): TControl;
    { Exact runtime identity for a currently borrowed native input. Empty for
      an unrelated/non-input control. One binding pass; nothing is retained. }
    function InputIdentity(AInput: TControl): TNyxText;
    { Borrow the actual keyboard/focus face, including a split separator's grip.
      A component without a declared face returns nil; a missing identity raises.
      Collection attachments manage entry within their returned host. The caller
      must not free this control or retain it beyond the mounted view. }
    function FocusFor(const AID: TNyxText;
      AIdentity: TNyxIdentityKind = niAutomatic): TWinControl;
    { Runtime text selection uses Unicode scalar offsets against physical text,
      retaining native line endings. Neither method forces focus or scrolling. }
    function TextSelectionFor(const AID: TNyxText): TNyxTextSelection;
    function EditingFor(const AID: TNyxText): TNyxEditingSnapshot;
    procedure SetTextSelection(const AID: TNyxText; const ASelection: TNyxTextSelection);
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
    property Root: TNyxNode read FRoot;
    { UI-thread, view-local exclusive choice. A mounted view is required;
      unknown/automatic names refuse before mutation. Switching synchronizes
      the same controls and preserves authored source/history and live input. }
    property PresentationSelection: TNyxPresentationSelection read FPresentationSelection write SetPresentationSelection;
    { Portable managed capability. Borrowed receiver links retire on Unmount;
      caller-retained interfaces stay safe and never retain the renderer. }
    property Presentations: INyxPresentationView read GetPresentationView;
    property DesignMode: Boolean read FDesignMode;
    property State: TNyxState read FState;
    property OnBindingError: TNyxLCLBindingError read FOnBindingError write FOnBindingError;
    property LastBindingError: TNyxText read FLastBindingError;
    property LastBindingFailure: TNyxBindingFailure read FLastBindingFailure;
    { Owned host refusal. The optional sink may navigate or dispose this view. }
    property OnGestureFailure: TNyxGestureFailure read FOnGestureFailure write FOnGestureFailure;
    property LastGestureError: TNyxText read FLastGestureError;
  end;

implementation

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
end;

type
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

constructor TNyxLCLRenderer.Create(ATheme: TNyxTheme);
begin
  inherited Create;
  FTheme := ATheme;
  FBorrowedTheme := ATheme;
  FOwnTheme := ATheme = nil;

  if FOwnTheme then
  begin
    FTheme := TNyxTheme.Create;
  end;
  FTheme.Validate;
  FBaseTheme := NewNyxDocumentTheme(nil, FTheme);
  FEvents := NewNyxEvents;
  FPhysicalFrame := NewNyxNativeGestureFrame;
end;

destructor TNyxLCLRenderer.Destroy;
begin

  if FEvents <> nil then
  begin
    FEvents.Close;
  end;
  Clear;
  FBaseTheme.Free;

  if FOwnTheme then
  begin
    FTheme.Free;
  end;
  inherited Destroy;
end;

procedure TNyxLCLRenderer.Clear;
var
  LIndex: Integer;
  LDeferControls: Boolean;
begin
  if FPresentationView <> nil then
  begin
    FPresentationView.Retire;
    FPresentationView := nil;
  end;
  ClearCanvasResizeGrips;
  { Revoke borrowed sinks before destroying any part of the mounted view. }
  ClearCanvasMoveGrip;
  FUpdating := True;
  FVirtualLayout := False;

  if FPanel <> nil then
  begin
    { Retirement must not enter layout through native resize messages while
      bindings and controls are being disconnected or destroyed. }
    FPanel.OnResize := nil;
    FPanel.Detach;
  end;
  FDesignMode := False;
  FProjectionContext := '';
  FProjectionSchemaRevision := 0;
  ReleaseNyxNode(FProjectionBaseline);
  FSelectedDesignID := '';
  FResizePreview := Default(TNyxResizePreview);
  for LIndex := Low(FResizeEdges) to High(FResizeEdges) do
  begin
    FreeAndNil(FResizeEdges[LIndex]);
  end;
  for LIndex := Low(FSelectionEdges) to High(FSelectionEdges) do
  begin
    { The scroll host owns the strips and releases them with its controls. }
    FSelectionEdges[LIndex] := nil;
  end;
  LDeferControls := ((FEditingObserver <> nil) and FEditingObserver.Dispatching) or
    ((FPhysicalFrame <> nil) and FPhysicalFrame.Dispatching);

  if FCaptureObserver <> nil then
  begin
    FCaptureObserver.Disconnect;
    FCaptureObserver := nil;
  end;

  if FEditingObserver <> nil then
  begin
    FEditingObserver.Disconnect;
    FEditingObserver := nil;
  end;

  if FViewportObserver <> nil then
  begin
    FViewportObserver.Disconnect;
    FViewportObserver := nil;
  end;

  if FEmitterScope <> nil then
  begin
    FEmitterScope.Disconnect;
    FEmitterScope := nil;
  end;

  if FEvents <> nil then
  begin
    FEvents.CancelPending;
  end;
  FreeAndNil(FLiveBindings);
  for LIndex := 0 to Length(FBindings) - 1 do
  begin

    if FBindings[LIndex].FControl is TNyxLogicalScrollBox then
    begin
      TNyxLogicalScrollBox(FBindings[LIndex].FControl).Detach;
    end;
    FBindings[LIndex].FActiveDrag := nil;
    FBindings[LIndex].DisconnectControl(FBindings[LIndex].FControl);

    if FBindings[LIndex].FInput <> FBindings[LIndex].FControl then
    begin
      FBindings[LIndex].DisconnectControl(FBindings[LIndex].FInput);
    end;

    if FBindings[LIndex].FControl is TNyxLCLSplitView then
    begin
      FBindings[LIndex].DisconnectControl(
        TNyxLCLSplitView(FBindings[LIndex].FControl).Grip);
    end;

    if FBindings[LIndex].FCollectionMount <> nil then
    begin
      FBindings[LIndex].FCollectionMount.Disconnect;
      FBindings[LIndex].FCollectionMount := nil;
    end;
  end;
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
  for LIndex := 0 to Length(FBindings) - 1 do
  begin
    FBindings[LIndex].Free;
  end;
  SetLength(FBindings, 0);
  FCollectionBindings := nil;
  ReleaseNyxNode(FRoot);
  FPresentationSelection := TNyxPresentationSelection.None;

  if FOwnState then
  begin
    FState.Free;
  end;
  FState := nil;
  FOwnState := False;
  FUpdating := False;
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
  LInfo: TNyxPrimitiveInfo;
  LFactoryIndex: Integer;
begin
  AInput := nil;
  ACaption := nil;
  LKind := ANode.ProjectionKind;
  ValidateNyxProperties(ANode);
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
    ACaption := LLabel;
    LLabel.Caption := ANode.Prop('text');
    LLabel.SetBounds(0, 0, 300, 20);
    LLabel.Font.Color := ThemeColor(FTheme.Muted);
    LLabel.Font.Height := -12;
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
    AInput.Parent := LInputSurface;
    AInput.SetBounds(12, 10, 276, 20);
    AInput.Anchors := [akLeft, akTop, akRight];
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
    TCheckBox(Result).Caption := ANode.Prop('text');
    TCheckBox(Result).Checked := ANode.Prop('value') = 'true';
    AInput := Result;
  end
  else if LKind = 'radio' then
  begin
    Result := TRadioButton.Create(FPanel);
    TRadioButton(Result).Caption := ANode.Prop('text');
    TRadioButton(Result).Checked := ANode.Prop('value') = 'true';
    AInput := Result;
  end
  else if LKind = 'slider' then
  begin
    Result := TTrackBar.Create(FPanel);
    TTrackBar(Result).Min := StrToIntDef(ANode.Prop('min'), 0);
    TTrackBar(Result).Max := StrToIntDef(ANode.Prop('max'), 100);
    TTrackBar(Result).Position := StrToIntDef(ANode.Prop('value'), 0);
    AInput := Result;
  end
  else if LKind = 'progress' then
  begin
    Result := TProgressBar.Create(FPanel);
    TProgressBar(Result).Max := StrToIntDef(ANode.Prop('max'), 100);
    TProgressBar(Result).Position := StrToIntDef(ANode.Prop('value'), 0);
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
    Result := TStringGrid.Create(FPanel);
  end
  else if LKind = 'image' then
  begin
    Result := TImage.Create(FPanel);
    TImage(Result).Proportional := True;
    TImage(Result).Center := True;

    { SyncPicture admits a detached picture before updating this face. }
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
      LLabel.Font.Size := 18;
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
end;

function TNyxLCLRenderer.Build(ANode: TNyxNode; AParent: TWinControl): TControl;
var
  LInput: TControl;
  LFocus: TWinControl;
  LCaption: TLabel;
  LBinding: TNyxLCLBinding;
  LIndex: Integer;
  LValueDomain: TNyxValueDomain;
begin
  Result := CreateControl(ANode, LInput, LCaption);
  Result.Parent := AParent;
  Result.Hint := ANode.Prop('hint');
  Result.ShowHint := Result.Hint <> '';
  Result.Visible := ANode.Prop('visible', 'true') <> 'false';
  Result.Enabled := ANode.Prop('enabled', 'true') <> 'false';

  if LInput <> nil then
  begin
    LInput.Enabled := Result.Enabled;
  end;
  LBinding := TNyxLCLBinding.Create;
  LBinding.FRenderer := Self;
  LBinding.FNode := ANode;
  LBinding.FControl := Result;
  LBinding.FInput := LInput;
  LBinding.FCaption := LCaption;

  if ANode.Parent <> nil then
  begin
    LBinding.FLogicalParent := Binding(ANode.Parent);
  end;
  LValueDomain := NyxNodeValueDomain(ANode);
  { Numeric drafts need an editing-complete boundary even without a state
    binding. Their declared control/compound domain determines this behavior. }
  LBinding.FDeferredValue := (LInput is TCustomEdit) and not (LInput is TSpinEdit) and
    LValueDomain.Defined;

  if LBinding.FDeferredValue then
  begin
    LBinding.FDeferredValue := LValueDomain.Kind in [nskInteger, nskNumber];
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

  if LInput <> nil then
  begin
    LInput.AccessibleName := Result.AccessibleName;

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
    LBinding.FPreviousEnter := TNyxWinControlAccess(LFocus).OnEnter;
    LBinding.FPreviousExit := TNyxWinControlAccess(LFocus).OnExit;
    TNyxWinControlAccess(LFocus).OnEnter := LBinding.Enter;
    TNyxWinControlAccess(LFocus).OnExit := LBinding.Leave;
    LBinding.FPreviousKeyDown := TNyxWinControlAccess(LFocus).OnKeyDown;
    LBinding.FPreviousKeyUp := TNyxWinControlAccess(LFocus).OnKeyUp;
    TNyxWinControlAccess(LFocus).OnKeyDown := LBinding.KeyDown;
    TNyxWinControlAccess(LFocus).OnKeyUp := LBinding.KeyUp;
  end;

  if LInput is TCustomEdit then
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
  for LIndex := 0 to ANode.Count - 1 do
  begin

    if Result is TNyxLCLSplitView then
    begin
      Build(ANode.Children[LIndex], TNyxLCLSplitView(Result).Panes[LIndex]);
    end
    else
    begin
      Build(ANode.Children[LIndex], TWinControl(Result));
    end;
  end;

  if Result is TNyxLCLSplitView then
  begin
    TNyxLCLSplitView(Result).OnLayout := LBinding.SplitLayout;
    TNyxLCLSplitView(Result).OnChanged := LBinding.SplitChanged;
    TNyxLCLSplitView(Result).Ready;
  end;
  Result.Tag := Length(FBindings);
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
  Result := 0;
  for LIndex := 0 to ANode.Count - 1 do
  begin

    if ANode.Children[LIndex].Prop('visible', 'true') <> 'false' then
    begin
      Inc(Result);
    end;
  end;
end;

function TNyxLCLRenderer.NaturalWidth(ANode: TNyxNode; AAvailable: Integer): Integer;
begin
  Result := NyxNodeSizeConstraints(ANode).WidthRange.Clamp(
    NaturalContentWidth(ANode, AAvailable));
end;

function TNyxLCLRenderer.NaturalContentWidth(ANode: TNyxNode; AAvailable: Integer): Integer;
var
  LControl: TControl;
  LLabel: TLabel;
  LIndex: Integer;
  LWidth: Integer;
  LHeight: Integer;
  LWrap: Boolean;
  LWrapLength: Integer;
  LVisible: Integer;
  LSum: Double;
begin
  AAvailable := Max(0, AAvailable);

  if ANode.Prop('width-sizing') = 'fill' then
  begin
    Exit(AAvailable);
  end;

  if ((ANode.Prop('width-sizing') = '') or (ANode.Prop('width-sizing') = 'auto')) and
    (ANode.Prop('width') <> '') then
  begin
    Exit(Min(AAvailable, Metric(ANode, 'width', AAvailable)));
  end;
  Result := 0;

  if (ANode.Count > 0) and (ANode.ProjectionKind <> 'split-view') then
  begin
    LSum := 0;
    LVisible := 0;
    for LIndex := 0 to ANode.Count - 1 do
    begin

      if ANode.Children[LIndex].Prop('visible', 'true') = 'false' then
      begin
        Continue;
      end;
      Inc(LVisible);
      LWidth := NaturalWidth(ANode.Children[LIndex], AAvailable);
      Result := Max(Result, LWidth);
      LSum := LSum + LWidth;
    end;

    if NyxLayout(ANode) = 'row' then
    begin
      Result := Trunc(Min(Double(AAvailable), LSum +
        Double(Max(0, LVisible - 1)) * Metric(ANode, 'gap', 12)));
    end
    else if NyxLayout(ANode) = 'grid' then
    begin
      { Large admitted column counts must be capped before Integer conversion. }
      Result := Trunc(Min(Double(AAvailable), Double(Result) *
        Metric(ANode, 'columns', 2)));
    end;
    Exit(Min(AAvailable, Result + 2 * Metric(ANode, 'padding', 0)));
  end;
  LControl := Binding(ANode).FControl;

  if LControl.Parent <> nil then
  begin
    LControl.Parent.HandleNeeded;
  end;
  LWidth := 0;
  LHeight := 0;

  if LControl is TLabel then
  begin
    LLabel := TLabel(LControl);
    LWrap := LLabel.WordWrap;
    LWrapLength := LLabel.WordWrapLength;
    try
      LLabel.WordWrap := False;
      LLabel.WordWrapLength := 0;
      LLabel.GetPreferredSize(LWidth, LHeight, True, False);
    finally
      LLabel.WordWrap := LWrap;
      LLabel.WordWrapLength := LWrapLength;
    end;
  end
  else
  begin
    LControl.GetPreferredSize(LWidth, LHeight, True, False);
  end;

  if (Binding(ANode).FInput <> nil) and (Binding(ANode).FInput <> LControl) then
  begin
    LWidth := Max(LWidth, 200);
  end;

  if ANode.ProjectionKind = 'spacer' then
  begin
    LWidth := 0;
  end;
  Result := Min(AAvailable, Max(0, LWidth));
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
begin
  LInner := Max(0, AWidth - 2 * Metric(ANode, 'padding', 0));
  LGap := Metric(ANode, 'gap', 12);
  SetLength(LItems, ANode.Count);
  SetLength(LRanges, ANode.Count);
  SetLength(AWidths, ANode.Count);
  SetLength(ALefts, ANode.Count);
  SetLength(ATops, ANode.Count);
  SetLength(AHeights, ANode.Count);
  for LIndex := 0 to ANode.Count - 1 do
  begin
    LItems[LIndex].Visible := ANode.Children[LIndex].Prop('visible', 'true') <> 'false';
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
begin

  if ANode.Prop('visible', 'true') = 'false' then
  begin
    Exit(0);
  end;
  Result := NyxNodeSizeConstraints(ANode).HeightRange.Clamp(
    MeasureContent(ANode, AWidth, AAllocatedWidth));
end;

function TNyxLCLRenderer.MeasureContent(ANode: TNyxNode; AWidth: Integer;
  AAllocatedWidth: Boolean): Integer;
var
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
begin

  if ANode.Prop('visible', 'true') = 'false' then
  begin
    Exit(0);
  end;

  AWidth := EffectiveWidth(ANode, AWidth, AAllocatedWidth);

  if (ANode.Prop('height') <> '') and
    ((ANode.Prop('height-sizing') = '') or (ANode.Prop('height-sizing') = 'auto')) then
  begin
    Exit(Metric(ANode, 'height', 32));
  end;
  Result := 28;

  if (ANode.ProjectionKind = 'input') or (ANode.ProjectionKind = 'select') or
    (ANode.ProjectionKind = 'spin') or (ANode.ProjectionKind = 'date') or
    (ANode.ProjectionKind = 'time') or (ANode.ProjectionKind = 'color') then
  begin
    { Match the shared 1.5 line height plus frame padding and caption space.
      Explicit heights still win; auto sizes must remain usable with large fonts. }
    Result := 24 + (FTheme.FontSize * 3 div 2) + 22;
  end
  else if ANode.ProjectionKind = 'memo' then
  begin
    Result := 120;

    if 24 + (FTheme.FontSize * 3 div 2) + 22 > Result then
    begin
      Result := 24 + (FTheme.FontSize * 3 div 2) + 22;
    end;
  end
  else if ANode.ProjectionKind = 'code-editor' then
  begin
    Result := 240;
  end
  else if ANode.ProjectionKind = 'split-view' then
  begin
    Result := 320;
    Exit;
  end
  else if (ANode.ProjectionKind = 'table') or (ANode.ProjectionKind = 'tree') or
    (ANode.ProjectionKind = 'list') or (ANode.ProjectionKind = 'code') or
    (ANode.ProjectionKind = 'image') then
  begin
    Result := 160;
  end
  else if ANode.ProjectionKind = 'heading' then
  begin
    Result := 38;
  end
  else if ANode.ProjectionKind = 'button' then
  begin
    Result := (FTheme.FontSize * 3 div 2) + 22;
  end
  else if ANode.ProjectionKind = 'separator' then
  begin
    Result := 2;
  end
  else if ANode.ProjectionKind = 'spacer' then
  begin
    Result := 16;
  end;

  if ANode.Count = 0 then
  begin
    { Measure real LCL typography at the allocated width. Fixed one-line label
      heights clip headings/captions in a phone-width view or enlarged fonts. }

    if Binding(ANode).FControl is TLabel then
    begin
      LLabel := TLabel(Binding(ANode).FControl);
      LLabel.Parent.HandleNeeded;
      LTextWidth := AWidth;

      if LTextWidth < 1 then
      begin
        LTextWidth := 1;
      end;
      LLabel.WordWrapLength := LTextWidth;
      LTextHeight := 0;
      LLabel.GetPreferredSize(LTextWidth, LTextHeight, True, False);

      if LTextHeight + 4 > Result then
      begin
        Result := LTextHeight + 4;
      end;
    end;
    Exit;
  end;
  LPadding := Metric(ANode, 'padding', 0);
  LGap := Metric(ANode, 'gap', 12);
  LVisible := VisibleChildren(ANode);
  Result := 0;

  if NyxLayout(ANode) = 'row' then
  begin
    Result := RowPlan(ANode, AWidth, LWidths, LLefts, LTops, LHeights);
  end
  else if NyxLayout(ANode) = 'grid' then
  begin
    LColumns := LayoutColumns(ANode, AWidth);
    LCellWidth := Max(0, AWidth - 2 * LPadding - LGap * (LColumns - 1)) div LColumns;
    LRowHeight := 0;
    LPosition := 0;
    for LIndex := 0 to ANode.Count - 1 do
    begin

      if ANode.Children[LIndex].Prop('visible', 'true') = 'false' then
      begin
        Continue;
      end;

      LHeight := Measure(ANode.Children[LIndex], LCellWidth);
      Inc(LPosition);

      if LHeight > LRowHeight then
      begin
        LRowHeight := LHeight;
      end;

      if (LPosition mod LColumns = 0) or (LPosition = LVisible) then
      begin
        Inc(Result, LRowHeight);
        LRowHeight := 0;

        if LPosition < LVisible then
        begin
          Inc(Result, LGap);
        end;
      end;
    end;
  end
  else
  begin
    for LIndex := 0 to ANode.Count - 1 do
    begin

      if ANode.Children[LIndex].Prop('visible', 'true') <> 'false' then
      begin
        Inc(Result, Measure(ANode.Children[LIndex],
          ColumnChildWidth(ANode, ANode.Children[LIndex], Max(0, AWidth - 2 * LPadding)), True));
      end;
    end;
    Inc(Result, LGap * Max(0, LVisible - 1));
  end;
  Inc(Result, 2 * LPadding);
end;

procedure TNyxLCLRenderer.Layout(ANode: TNyxNode; AX, AY, AWidth: Integer;
  AHeight: Integer; AAllocatedWidth: Boolean);
var
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
begin
  LBinding := Binding(ANode);
  { The browser theme caps every node at its containing block's available
    width. Apply that same cap before measuring native wrapping/descendants. }
  LWidth := EffectiveWidth(ANode, AWidth, AAllocatedWidth);
  LHeight := Measure(ANode, LWidth, True);

  if AHeight >= 0 then
  begin
    LHeight := AHeight;
  end;
  LHeight := NyxNodeSizeConstraints(ANode).HeightRange.Clamp(LHeight);
  LBinding.FLogicalBox := NyxViewportBox(AX, AY, LWidth, LHeight);

  if not FVirtualLayout then
  begin
    LBinding.FControl.SetBounds(AX, AY, LWidth, LHeight);
    ArrangeInput(LBinding, LWidth, LHeight);
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
      LBinding.FControl.SetBounds(0, 0, LWidth, LHeight);
    end;
    TNyxLCLSplitView(LBinding.FControl).Arrange;
    Exit;
  end;

  LPadding := Metric(ANode, 'padding', 0);
  LGap := Metric(ANode, 'gap', 12);
  LAlignment := ANode.Prop('cross-alignment', 'auto');
  { Rows use one measured line plan for both wrapping and arrangement. Retain
    every control; changing a policy only changes geometry, never input state. }

  if NyxLayout(ANode) = 'row' then
  begin
    RowPlan(ANode, LWidth, LSizes, LLefts, LTops, LHeights);
    for LIndex := 0 to ANode.Count - 1 do
    begin
      LChild := ANode.Children[LIndex];

      if LChild.Prop('visible', 'true') = 'false' then
      begin
        Continue;
      end;
      LCellHeight := LHeights[LIndex];
      { A single flex line fills a definite cross axis. Its natural height can
        greatly exceed the allocated workspace when a child is a scroll view.
        Stretch/center/end must use the admitted host extent, as browser flex
        does, rather than growing that viewport to all of its scroll content. }

      if (ANode.Prop('flow-wrap') = 'nowrap') or
        (((ANode.Prop('flow-wrap') = '') or (ANode.Prop('flow-wrap') = 'auto')) and
        (FPanel.ClientWidth > 600)) then
      begin

        if (AHeight >= 0) or ((ANode.Prop('height') <> '') and
          ((ANode.Prop('height-sizing') = '') or (ANode.Prop('height-sizing') = 'auto'))) then
        begin
          LCellHeight := Max(0, LHeight - 2 * LPadding);
        end
        else
        begin
          LCellHeight := Max(LCellHeight, LHeight - 2 * LPadding);
        end;
      end;
      LFitHeight := Measure(LChild, LSizes[LIndex], True);

      if (LChild.Prop('height-sizing') = 'fill') or ((LAlignment = 'stretch') and
        ((LChild.Prop('height-sizing') = '') or (LChild.Prop('height-sizing') = 'auto')) and
        (LChild.Prop('height') = '')) then
      begin
        LFitHeight := LCellHeight;
      end;
      { Clamp before alignment, so a capped stretched control is positioned
        against its actual height rather than its discarded line allocation. }
      LFitHeight := NyxNodeSizeConstraints(LChild).HeightRange.Clamp(LFitHeight);
      LChildY := LPadding + LTops[LIndex];

      if (LAlignment = '') or (LAlignment = 'auto') or (LAlignment = 'center') then
      begin
        Inc(LChildY, Max(0, LCellHeight - LFitHeight) div 2);
      end
      else if LAlignment = 'end' then
      begin
        Inc(LChildY, Max(0, LCellHeight - LFitHeight));
      end;
      Layout(LChild, LPadding + LLefts[LIndex], LChildY, LSizes[LIndex], LFitHeight, True);
    end;
    Exit;
  end;
  AY := LPadding;
  AX := LPadding;
  LColumns := LayoutColumns(ANode, LWidth);
  LVisible := VisibleChildren(ANode);
  LPosition := 0;
  LDefiniteColumn := (NyxLayout(ANode) = 'column') and
    ((AHeight >= 0) or ((ANode.Prop('height') <> '') and
    ((ANode.Prop('height-sizing') = '') or (ANode.Prop('height-sizing') = 'auto'))));

  if NyxLayout(ANode) = 'column' then
  begin
    SetLength(LItems, ANode.Count);
    SetLength(LRanges, ANode.Count);
    for LIndex := 0 to ANode.Count - 1 do
    begin
      LItems[LIndex].Visible := ANode.Children[LIndex].Prop('visible', 'true') <> 'false';
      LRanges[LIndex] := NyxNodeSizeConstraints(ANode.Children[LIndex]).HeightRange;
      LItems[LIndex].Weight := 0;

      if LDefiniteColumn then
      begin
        LItems[LIndex].Weight := NyxFlexWeight(ANode.Children[LIndex]);
      end;
      LItems[LIndex].NaturalSize := Measure(ANode.Children[LIndex],
        ColumnChildWidth(ANode, ANode.Children[LIndex], Max(0, LWidth - 2 * LPadding)), True);

      if LDefiniteColumn and (ANode.Children[LIndex].Prop('height-sizing') = 'fill') then
      begin
        LItems[LIndex].NaturalSize := Max(0, LHeight - 2 * LPadding);
      end;
    end;
    LSizes := NyxFlowSizes(Max(0, LHeight - 2 * LPadding), LGap, LItems, LRanges);
    LJustification := njStart;
    for LJustification := Low(TNyxJustification) to High(TNyxJustification) do
    begin

      if NyxJustificationName(LJustification) = ANode.Prop('justification', 'start') then
      begin
        Break;
      end;
    end;
    LPositions := NyxFlowPositions(Max(0, LHeight - 2 * LPadding), LGap,
      LItems, LSizes, LJustification);
  end;
  LCellWidth := Max(0, LWidth - 2 * LPadding - LGap * (LColumns - 1)) div LColumns;
  LCellHeight := 0;
  for LIndex := 0 to ANode.Count - 1 do
  begin
    LChild := ANode.Children[LIndex];

    if LChild.Prop('visible', 'true') = 'false' then
    begin
      { Keep the mounted control and draft, but never reserve a flow slot/gap.
        Its next visible layout recomputes descendants at the current size. }
      Continue;
    end;
    Inc(LPosition);

    if ANode.Prop('layout') = 'absolute' then
    begin
      Layout(LChild, Metric(LChild, 'left', 0), Metric(LChild, 'top', 0), LCellWidth);
    end
    else
    begin
      LFitHeight := -1;
      LFitWidth := LCellWidth;
      LChildY := AY;

      if NyxLayout(ANode) = 'column' then
      begin
        LFitHeight := LSizes[LIndex];
        LFitWidth := ColumnChildWidth(ANode, LChild, Max(0, LWidth - 2 * LPadding));
        LChildY := LPadding + LPositions[LIndex];
        LChildX := LPadding;

        if LAlignment = 'center' then
        begin
          Inc(LChildX, Max(0, LWidth - 2 * LPadding - LFitWidth) div 2);
        end
        else if LAlignment = 'end' then
        begin
          Inc(LChildX, Max(0, LWidth - 2 * LPadding - LFitWidth));
        end;
        Layout(LChild, LChildX, LChildY, LFitWidth, LFitHeight, True);
        Continue;
      end;

      Layout(LChild, AX, LChildY, LFitWidth, LFitHeight);
      LInputHeight := Measure(LChild, LFitWidth);

      if LFitHeight >= 0 then
      begin
        LInputHeight := LFitHeight;
      end;

      if LInputHeight > LCellHeight then
      begin
        LCellHeight := LInputHeight;
      end;
      { Grid tracks keep their cell extent even when a child has a smaller
        authored width. Row/column flows returned through their own plans above. }
      Inc(AX, LCellWidth + LGap);

      if (LPosition mod LColumns) = 0 then
      begin
        AX := LPadding;
        Inc(AY, LCellHeight + LGap);
        LCellHeight := 0;
      end;
    end;
  end;
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
  UpdateSelection;
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

  if not FResizePreview.Active or (FPanel = nil) or (FPanel.Parent = nil) or
    (FSelectionEdges[0] = nil) or not FSelectionEdges[0].Visible then
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
  LControl: TControl;
  LIndex: Integer;
  LHost: TWinControl;
  LBounds: TRect;
  LSelected: TNyxLCLBinding;
begin

  if FPanel = nil then
  begin
    Exit;
  end;
  LControl := nil;
  LSelected := nil;

  if FDesignMode and (FSelectedDesignID <> '') then
  begin
    for LIndex := 0 to High(FBindings) do
    begin

      if (FBindings[LIndex].FNode.DesignID = FSelectedDesignID) and
        FBindings[LIndex].FControl.Visible and
        NyxInteractionPolicy(FBindings[LIndex].FNode).Visible then
      begin
        { Realization visits parents first. A reusable instance's descendants
          share its authored identity; outline that instance's outer face once. }
        LSelected := FBindings[LIndex];

        if not FVirtualLayout or LSelected.FPlacement.HasArea then
        begin
          LControl := LSelected.FControl;
        end;
        Break;
      end;
    end;
  end;
  LHost := FPanel;
  LBounds := Rect(0, 0, 0, 0);

  if LControl <> nil then
  begin

    if (LControl.Parent = FPanel) and (LControl is TWinControl) then
    begin
      { Root edges stay inside its client rectangle. They neither extend the
        scroll range nor sit behind the full-page child window. }
      LHost := TWinControl(LControl);
      LBounds := LHost.ClientRect;

      if FVirtualLayout then
      begin
        LBounds := Rect(-LSelected.FPlacement.ContentOffsetX,
          -LSelected.FPlacement.ContentOffsetY,
          LSelected.FLogicalBox.Width - LSelected.FPlacement.ContentOffsetX,
          LSelected.FLogicalBox.Height - LSelected.FPlacement.ContentOffsetY);
      end;
    end
    else
    begin
      LHost := LControl.Parent;
      LBounds := LControl.BoundsRect;

      if FVirtualLayout then
      begin
        OffsetRect(LBounds, -LSelected.FPlacement.ContentOffsetX,
          -LSelected.FPlacement.ContentOffsetY);
        LBounds.Right := LBounds.Left + LSelected.FLogicalBox.Width;
        LBounds.Bottom := LBounds.Top + LSelected.FLogicalBox.Height;
      end;
      InflateRect(LBounds, 2, 2);
    end;
  end;
  for LIndex := Low(FSelectionEdges) to High(FSelectionEdges) do
  begin

    if (LControl <> nil) and (FSelectionEdges[LIndex] = nil) then
    begin
      FSelectionEdges[LIndex] := TShape.Create(FPanel);
      FSelectionEdges[LIndex].Shape := stRectangle;
      FSelectionEdges[LIndex].Pen.Style := psSolid;
    end;

    if FSelectionEdges[LIndex] <> nil then
    begin
      FSelectionEdges[LIndex].Parent := LHost;
      { TShape's rectangle excludes its final fill row/column. A matching solid
        pen covers that perimeter too, preserving the complete two-pixel strip. }
      FSelectionEdges[LIndex].Pen.Color := ThemeColor(FTheme.Accent);
      FSelectionEdges[LIndex].Brush.Color := ThemeColor(FTheme.Accent);
      FSelectionEdges[LIndex].Visible := LControl <> nil;
    end;
  end;

  if LControl = nil then
  begin
    UpdateResizePreview;
    UpdateCanvasResizeGrips;
    Exit;
  end;
  { The scroll panel owns all four graphics even when their paint parent is a
    nested container. Outside edges leave inputs unobstructed; root edges use
    the page's padded inner perimeter. Neither kind creates a focus entry. }
  FSelectionEdges[0].SetBounds(LBounds.Left, LBounds.Top, LBounds.Right - LBounds.Left, 2);
  FSelectionEdges[1].SetBounds(LBounds.Left, LBounds.Bottom - 2, LBounds.Right - LBounds.Left, 2);
  FSelectionEdges[2].SetBounds(LBounds.Left, LBounds.Top + 2, 2,
    Max(0, LBounds.Bottom - LBounds.Top - 4));
  FSelectionEdges[3].SetBounds(LBounds.Right - 2, LBounds.Top + 2, 2,
    Max(0, LBounds.Bottom - LBounds.Top - 4));
  for LIndex := Low(FSelectionEdges) to High(FSelectionEdges) do
  begin
    FSelectionEdges[LIndex].BringToFront;
  end;
  UpdateResizePreview;
  UpdateCanvasResizeGrips;
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
    { Reparent the scroll host, never recreate its children or event routers.
      Guard native focus notifications caused by reparenting from authoring. }
    FPanel.Parent := AHost;
    { Inert proposal windows are siblings of the scroll host. Move even hidden
      strips before the caller disposes the previous host; their borrowed parent
      must never free windows still owned/referenced by this renderer. }
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

function TNyxLCLRenderer.UpdateViewport: Boolean;
begin
  Result := False;

  if (FRoot = nil) or (FPanel = nil) or (FPanel.Parent = nil) then
  begin
    Exit;
  end;
  Result := FRoot.ApplyViewport(Max(0, FPanel.Parent.ClientWidth),
    Max(0, FPanel.Parent.ClientHeight), npfNativeLCL, FPresentationSelection);
end;

procedure TNyxLCLRenderer.Resize(ASender: TObject);
var
  LHeight: Integer;
  LIndex: Integer;
  LPass: Integer;
  LWidth: Integer;
  LClientHeight: Integer;
  LChild: Integer;
  LContentHeight: Double;
begin

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
      Exit;
    end;
    { LCL's automatic anchor/layout pass must see the complete new geometry.
      Otherwise showing a previously unplaced label can reapply its stale base
      position in the middle of SetBounds. Keep widgetset layout atomic while
      retaining the same controls and their focus/editing state. }
    FPanel.DisableAutoSizing;
    FLayouting := True;
    try
      { Design canvases always retain their logical frame. Runtime views switch
        when any face/position could exceed the native message coordinate domain,
        including a large child inside an explicitly bounded scroll container. }
      FVirtualLayout := FDesignMode;
      for LIndex := 0 to High(FBindings) do
      begin
        FBindings[LIndex].FLogicalBox := Default(TNyxViewportBox);

        if not FVirtualLayout then
        begin
          FVirtualLayout := (Measure(FBindings[LIndex].FNode, FPanel.ClientWidth, True) >
            High(SmallInt)) or
            (Abs(Double(Metric(FBindings[LIndex].FNode, 'top', 0))) > High(SmallInt)) or
            (Abs(Double(Metric(FBindings[LIndex].FNode, 'left', 0))) > High(SmallInt));

          if not FVirtualLayout and (NyxLayout(FBindings[LIndex].FNode) = 'column') and
            (FBindings[LIndex].FNode.Prop('height') <> '') then
          begin
            { An explicit bounded scroll/column height describes its viewport,
              not its overflowing child extent. Detect that content before any
              native window receives a distant child position. Small ordinary
              scroll views keep their existing automatic LCL implementation.
              Automatic columns were already measured completely above; do not
              traverse those descendants a second time for each binding. }
            LContentHeight := 2.0 * Metric(FBindings[LIndex].FNode, 'padding', 0);
            for LChild := 0 to FBindings[LIndex].FNode.Count - 1 do
            begin

              if NyxInteractionPolicy(FBindings[LIndex].FNode.Children[LChild]).Visible then
              begin
                LContentHeight := LContentHeight + Measure(
                  FBindings[LIndex].FNode.Children[LChild], FPanel.ClientWidth, True) +
                  Metric(FBindings[LIndex].FNode, 'gap', 12);

                if LContentHeight > High(SmallInt) then
                begin
                  FVirtualLayout := True;
                  Break;
                end;
              end;
            end;
          end;
        end;
      end;

      if FVirtualLayout then
      begin
        { Showing either native scrollbar changes the available client area.
          Settle both axes before publishing physical boxes, without recursive
          layout or sizing the root from the previous viewport width. }
        for LPass := 0 to 2 do
        begin
          LWidth := FPanel.ViewportWidth;
          LClientHeight := FPanel.ViewportHeight;
          LHeight := -1;

          if FRoot.Prop('height-sizing') = 'fill' then
          begin
            LHeight := LClientHeight;
          end;
          Layout(FRoot, 0, 0, LWidth, LHeight);
          FPanel.SetLogicalExtent(Binding(FRoot).FLogicalBox.Width,
            Binding(FRoot).FLogicalBox.Height, ProjectViewport);

          if (LWidth = FPanel.ViewportWidth) and
            (LClientHeight = FPanel.ViewportHeight) then
          begin
            Break;
          end;
        end;
        ProjectViewport(FPanel);
      end
      else
      begin
        FPanel.UseNativeScrolling;
        LHeight := -1;

        if FRoot.Prop('height-sizing') = 'fill' then
        begin
          LHeight := Max(0, FPanel.ClientHeight);
        end;
        Layout(FRoot, 0, 0, Max(0, FPanel.ClientWidth), LHeight);
        for LIndex := 0 to High(FBindings) do
        begin

          if FBindings[LIndex].FControl is TNyxLCLSurface then
          begin
            TNyxLCLSurface(FBindings[LIndex].FControl).ProjectFace(Default(TNyxViewportBox));
          end;
        end;
      end;
    finally
      FLayouting := False;
      FPanel.EnableAutoSizing;
    end;
    UpdateSelection;
  end;
end;

procedure TNyxLCLRenderer.Render(ADocument: TNyxDocument; ARoot: TNyxNode;
  AHost: TWinControl; AState: TNyxState; const ACollections: INyxCollectionBindings);
begin
  Render(ADocument, ARoot, AHost, False, AState, ACollections);
end;

procedure TNyxLCLRenderer.Render(ADocument: TNyxDocument; ARoot: TNyxNode;
  AHost: TWinControl; ADesignMode: Boolean; AState: TNyxState;
  const ACollections: INyxCollectionBindings);
var
  LCandidate: TNyxLCLRenderer;
  LIndex: Integer;
  LTransferState: Boolean;
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
begin
  { Stage a hidden owned control tree beside the accepted view. A factory or
    layout exception destroys that candidate while retaining the live controls,
    their focus/state, and the accepted realized model. }

  if AHost = nil then
  begin
    raise ENyxModel.Create('Native host is required');
  end;
  LCandidate := TNyxLCLRenderer.Create(FTheme);
  LTransferState := (AState <> nil) and FOwnState and (AState = FState);
  {$ifdef NYX_STUDIO_PROFILE}LPhaseStarted := GetTickCount64;{$endif}
  { LCL propagates this balanced sizing lock through the host's parents and
    descendants. Stage the whole candidate before one final native alignment;
    per-child parenting/font/bounds changes must not relayout the accepted shell
    and every partly constructed sibling. Explicit Nyx layout still runs while
    locked. The host remains borrowed and the failure path releases the lock. }
  AHost.DisableAutoSizing;
  try

    if FBorrowedTheme <> nil then
    begin
      LCandidate.FTheme := NewNyxDocumentTheme(ADocument, FBorrowedTheme);
    end
    else
    begin
      LCandidate.FTheme := NewNyxDocumentTheme(ADocument, FBaseTheme);
    end;
    LCandidate.FOwnTheme := True;
    LCandidate.FFactoryKinds := Copy(FFactoryKinds, 0, Length(FFactoryKinds));
    LCandidate.FFactories := Copy(FFactories, 0, Length(FFactories));
    LCandidate.FEventFactories := Copy(FEventFactories, 0, Length(FEventFactories));
    { Candidate ports belong to the final owner's scheduler and view lifetime. }
    LCandidate.FEmitterScope := NewNyxEventEmitterScope(FEvents.Scheduler);
    LCandidate.FViewportObserver := NewNyxViewportObserver;
    LCandidate.FEditingObserver := NewNyxLCLEditingObserver;
    LCandidate.FUpdaters := Copy(FUpdaters, 0, Length(FUpdaters));
    LCandidate.FDesignMode := ADesignMode;
    LCandidate.FDesignerInput := FDesignerInput;
    LCandidate.FOnDesignerGesture := FOnDesignerGesture;
    LCandidate.FProjectionContext := NyxProjectionContext(ADocument);
    LCandidate.FProjectionSchemaRevision := NyxSchemaRevision;
    LCandidate.FRoot := RealizeNyxView(ADocument, ARoot);
    ApplyNyxPlatform(LCandidate.FRoot, npfNativeLCL);
    LCandidate.FRoot.ApplyViewport(Max(0, AHost.ClientWidth), Max(0, AHost.ClientHeight), npfNativeLCL);
    LCandidate.FProjectionBaseline := LCandidate.FRoot.Clone;
    LCandidate.FCollectionBindings := ACollections;

    if ACollections <> nil then
    begin
      ACollections.ValidateRoot(LCandidate.FRoot);
    end
    else
    begin
      LCandidate.FCollectionBindings := NewNyxCollectionBindings(LCandidate.FRoot,
        NewNyxCollectionContext(ADocument.Collections));
    end;
    LCandidate.FState := AState;
    LCandidate.FOwnState := AState = nil;

    if LCandidate.FOwnState then
    begin
      LCandidate.FState := ADocument.State.Clone;
    end;
    LCandidate.FLiveBindings := TNyxLiveBindings.Create(LCandidate.FRoot, LCandidate.FState);
    LCandidate.FLiveBindings.OnSync := LCandidate.Sync;
    LCandidate.FPanel := TNyxLogicalScrollBox.Create(nil);
    LCandidate.FPanel.Visible := False;
    LCandidate.FPanel.Parent := AHost;
    LCandidate.FPanel.Align := alClient;
    { Alignment is deliberately deferred. Supply the host's current client
      frame for provisional measurements; final LCL alignment and OnResize
      settle the actual frame after publication, including existing siblings. }
    LCandidate.FPanel.SetBounds(AHost.ClientRect.Left, AHost.ClientRect.Top,
      AHost.ClientWidth, AHost.ClientHeight);
    LCandidate.FPanel.BorderStyle := bsNone;
    LCandidate.FPanel.Color := ThemeColor(LCandidate.FTheme.Background);
    LCandidate.FPanel.Font.Assign(AHost.Font);
    LCandidate.FPanel.Font.Height := -LCandidate.FTheme.FontSize;
    LCandidate.FPanel.Font.Color := ThemeColor(LCandidate.FTheme.Text);
    {$ifdef NYX_STUDIO_PROFILE}RecordPhase('realize');{$endif}
    LCandidate.Build(LCandidate.FRoot, LCandidate.FPanel);
    for LIndex := 0 to LCandidate.FCollectionBindings.Count - 1 do
    begin
      LCandidate.BindCollection(LCandidate.FCollectionBindings.ID(LIndex),
        LCandidate.FCollectionBindings.View(LIndex));
    end;
    {$ifdef NYX_STUDIO_PROFILE}RecordPhase('build');{$endif}
    LCandidate.Resize(LCandidate.FPanel);
    {$ifdef NYX_STUDIO_PROFILE}RecordPhase('first-layout');{$endif}
    LCandidate.Sync;
    {$ifdef NYX_STUDIO_PROFILE}RecordPhase('first-sync');{$endif}

    if not ADesignMode then
    begin
      LCandidate.FLiveBindings.Activate;
    end;
    { Preserve an explicitly reused owned runtime store through full remount.
      The candidate borrows it until admission; failed factories retain the old
      view/store ownership. Clear only disconnects the old coordinator here. }

    if LTransferState then
    begin
      FOwnState := False;
    end;
    Clear;
    {$ifdef NYX_STUDIO_PROFILE}RecordPhase('retire');{$endif}
    { Publish effective tokens with the successfully staged native controls. }

    if FOwnTheme then
    begin
      FTheme.Free;
    end;
    FTheme := LCandidate.FTheme;
    FOwnTheme := True;
    LCandidate.FTheme := nil;
    LCandidate.FOwnTheme := False;
    FState := LCandidate.FState;
    FOwnState := LCandidate.FOwnState or LTransferState;
    LCandidate.FState := nil;
    LCandidate.FOwnState := False;
    FLiveBindings := LCandidate.FLiveBindings;
    LCandidate.FLiveBindings := nil;
    FLiveBindings.OnSync := Sync;
    FCollectionBindings := LCandidate.FCollectionBindings;
    LCandidate.FCollectionBindings := nil;
    FLastBindingError := '';
    FLastBindingFailure := nbfNone;
    FRoot := LCandidate.FRoot;
    LCandidate.FRoot := nil;
    FDesignMode := ADesignMode;
    FProjectionContext := LCandidate.FProjectionContext;
    FProjectionSchemaRevision := LCandidate.FProjectionSchemaRevision;
    FProjectionBaseline := LCandidate.FProjectionBaseline;
    LCandidate.FProjectionBaseline := nil;
    FVirtualLayout := LCandidate.FVirtualLayout;
    FPanel := LCandidate.FPanel;
    LCandidate.FPanel := nil;
    FBindings := LCandidate.FBindings;
    FEmitterScope := LCandidate.FEmitterScope;
    LCandidate.FEmitterScope := nil;
    FViewportObserver := LCandidate.FViewportObserver;
    LCandidate.FViewportObserver := nil;
    FEditingObserver := LCandidate.FEditingObserver;
    LCandidate.FEditingObserver := nil;
    FPhysicalFrame := LCandidate.FPhysicalFrame;
    FCaptureObserver := NewNyxLCLCaptureObserver(FPhysicalFrame);
    LCandidate.FBindings := nil;
    for LIndex := 0 to Length(FBindings) - 1 do
    begin
      FBindings[LIndex].FRenderer := Self;
    end;
    FPanel.OnResize := Resize;
    FEmitterScope.Activate(EmitNamed);
    FPanel.Visible := True;
    Resize(FPanel);
    {$ifdef NYX_STUDIO_PROFILE}RecordPhase('publish');{$endif}
    for LIndex := 0 to High(FBindings) do
    begin

      if NyxSupportsViewport(FBindings[LIndex].FNode) then
      begin

        if FBindings[LIndex].FInput is TWinControl then
        begin
          FViewportObserver.Add(FBindings[LIndex].FNode.ID,
            TWinControl(FBindings[LIndex].FInput));
        end
        else if FBindings[LIndex].FControl is TWinControl then
        begin
          FViewportObserver.Add(FBindings[LIndex].FNode.ID,
            TWinControl(FBindings[LIndex].FControl));
        end;
      end;
    end;
    FViewportObserver.Activate(FEvents, ViewportChanged);
    for LIndex := 0 to High(FBindings) do
    begin

      if (NyxSupportsTextInput(FBindings[LIndex].FNode) or
        (FBindings[LIndex].FNode.ProjectionKind = 'input')) and
        (FBindings[LIndex].FInput is TCustomEdit) then
      begin
        FEditingObserver.Add(FBindings[LIndex].FNode.ID,
          TWinControl(FBindings[LIndex].FInput));
      end;
    end;
    FEditingObserver.Activate(FEvents, EditingChanged);
    { Capture hooks are outermost in the native WindowProc chain. Revoke them
      before editing and viewport hooks when the mounted view ends. }
    for LIndex := 0 to High(FBindings) do
    begin
      FCaptureObserver.Add(FBindings[LIndex].FNode.ID, FBindings[LIndex].FControl);

      if (FBindings[LIndex].FInput <> nil) and
        (FBindings[LIndex].FInput <> FBindings[LIndex].FControl) then
      begin
        FCaptureObserver.Add(FBindings[LIndex].FNode.ID, FBindings[LIndex].FInput);
      end;
    end;
    FCaptureObserver.Activate(FEvents, CaptureChanged);
    {$ifdef NYX_STUDIO_PROFILE}RecordPhase('observe');{$endif}
  finally
    try
      LCandidate.Free;
    finally
      AHost.EnableAutoSizing;
    end;
  end;
  {$ifdef NYX_STUDIO_PROFILE}RecordPhase('settle');{$endif}
end;

function TNyxLCLRenderer.TryRefresh(ADocument: TNyxDocument; ARoot: TNyxNode;
  ADesignMode: Boolean; const ARestores: TNyxProjectionValueRestores): Boolean;
var
  LCandidate: TNyxNode;
  LPrevious: TNyxNode;
  LTheme: TNyxTheme;
  LIndex: Integer;
  LPreviousSelection: TNyxPresentationSelection;
begin
  FEvents.Scheduler.RequireUI;
  Result := False;

  if (FRoot = nil) or (FPanel = nil) or FUpdating or
    (FDesignMode <> ADesignMode) or
    (FProjectionSchemaRevision <> NyxSchemaRevision) then
  begin
    Exit;
  end;
  for LIndex := 0 to High(FBindings) do
  begin

    if FBindings[LIndex].FCustom or (FactoryIndex(FBindings[LIndex].FNode) >= 0) then
    begin
      Exit;
    end;
  end;
  { Validate fresh public data before comparing the immutable context. Retaining
    controls never implies a stale document cache or skipped source admission. }
  ValidateNyxDocumentProperties(ADocument);

  if NyxProjectionContext(ADocument) <> FProjectionContext then
  begin
    Exit;
  end;
  LCandidate := nil;
  LPrevious := nil;
  LTheme := nil;
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
    LCandidate := RealizeNyxView(ADocument, ARoot);
    ApplyNyxPlatform(LCandidate, npfNativeLCL);

    if not CanRefreshNyxProjection(FRoot, LCandidate) or
      not CanRefreshNyxProjection(FProjectionBaseline, LCandidate) or
      (FProjectionSchemaRevision <> NyxSchemaRevision) then
    begin
      Exit;
    end;
    LPrevious := FRoot.Clone;
    LPreviousSelection := FPresentationSelection;

    if not RefreshNyxProjectionProperties(FRoot, LCandidate, FProjectionBaseline, ARestores) then
    begin
      Exit;
    end;
    try
      FPresentationSelection := FPresentationSelection.Reconciled(FRoot.PresentationSnapshot);
      Sync;
    except
      { No custom updater or ownership change entered this path. Restore model
        properties before normal synchronization; retained callbacks still
        refer to the same independently owned realized nodes and controls. }
      RefreshNyxProjectionProperties(FRoot, LPrevious);
      FPresentationSelection := LPreviousSelection;
      Sync;
      raise;
    end;
    ReleaseNyxNode(FProjectionBaseline);
    FProjectionBaseline := LCandidate;
    LCandidate := nil;
    Result := True;
  finally
    LTheme.Free;
    ReleaseNyxNode(LPrevious);
    ReleaseNyxNode(LCandidate);
  end;
end;

procedure TNyxLCLRenderer.Unmount;
begin
  Clear;
end;

procedure TNyxLCLRenderer.SetPresentationSelection(const AValue: TNyxPresentationSelection);
var
  LPrevious: TNyxPresentationSelection;
begin
  FEvents.Scheduler.RequireUI;

  if (FRoot = nil) or (FPanel = nil) or FUpdating or FLayouting then
  begin
    raise ENyxModel.Create('Select a presentation on an idle mounted view');
  end;
  AValue.Validate(FRoot.PresentationSnapshot);

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
begin

  if FCollectionBindings = nil then
  begin
    raise ENyxModel.Create('No authored collection views are mounted');
  end;
  Result := FCollectionBindings.ViewFor(AID);
end;

function TNyxLCLRenderer.BindCollection(const AID: TNyxText;
  const AView: INyxCollectionView): INyxCollectionMount;
var
  LBinding: TNyxLCLBinding;
  LKind: TNyxText;
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
  Result := MountNyxLCLCollection(LBinding.FControl, AView);
  LBinding.FCollectionMount := Result;
  Result.ObserveSelection(LBinding.CollectionSelectionChanged);
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

function TNyxLCLRenderer.ViewportFor(const AID: TNyxText): TNyxViewportSnapshot;
var
  LBinding: TNyxLCLBinding;
  LControl: TControl;
begin
  FEvents.Scheduler.RequireUI;
  LBinding := IdentityBinding(AID, niAutomatic);
  LControl := LBinding.FControl;

  if LBinding.FInput <> nil then
  begin
    LControl := LBinding.FInput;
  end;

  if not NyxSupportsViewport(LBinding.FNode) or not (LControl is TWinControl) then
  begin
    raise ENyxModel.Create('This native control has no declared viewport');
  end;
  Result := CaptureNyxViewport(TWinControl(LControl));
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

function TNyxLCLRenderer.ScreenPointFor(const AID: TNyxText;
  const APointer: TNyxPointerSnapshot): TNyxResizePoint;
var
  LOrigin: TPoint;
begin
  FEvents.Scheduler.RequireUI;

  if not APointer.HasPosition then
  begin
    raise ENyxModel.Create('Pointer mapping requires actual position');
  end;
  LOrigin := IdentityBinding(AID, niAutomatic).FControl.ClientToScreen(Point(0, 0));
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

  if LParent.FControl is TWinControl then
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
  LBinding: TNyxLCLBinding;
  LDispatch: TNyxDispatch;
begin

  if FUpdating or FDesignMode then
  begin
    Exit;
  end;
  LBinding := IdentityBinding(AOriginID, niRuntime);
  LDispatch := FLiveBindings.Signal(LBinding.FNode, ntScroll);
  LDispatch.Info.HasViewport := True;
  LDispatch.Info.Viewport := AViewport;
  Emit(LBinding.FNode, LDispatch);
  { The borrowed binding/renderer may have ended inside Emit. }
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
  LBinding: TNyxLCLBinding;
  LDispatch: TNyxDispatch;
begin

  if FUpdating or FDesignMode or not FEvents.HasSubscribers(ATrigger) then
  begin
    Exit;
  end;
  LBinding := IdentityBinding(AOriginID, niRuntime);

  if not NyxInteractionPolicy(LBinding.FNode).CanIssueCommand then
  begin
    Exit;
  end;
  LDispatch := FLiveBindings.Signal(LBinding.FNode, ATrigger);
  LDispatch.Info.HasPointer := True;
  LDispatch.Info.Pointer.Kind := npiMouse;
  { Native capture belongs to the single LCL mouse stream (ID zero). This is an
    observation, without invented pressure, coordinates or response authority. }
  Emit(LBinding.FNode, LDispatch);
end;

procedure TNyxLCLBinding.StartDrag(ASender: TObject; var ADragObject: TDragObject);
var
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

  if FRenderer.FDesignMode then
  begin
    Exit;
  end;
  LPrevious := FPointerHooks[PointerSlot(ASender)].StartDrag;
  LRenderer := FRenderer;
  LEvents := LRenderer.FEvents;
  LRevision := LEvents.ViewRevision;
  LFrame := LRenderer.FPhysicalFrame;
  LOriginID := FNode.ID;
  LPolicy := NyxInteractionPolicy(FNode);
  LSource := FNode.Prop('drag-source') = 'true';
  LFrame.Enter;
  try

    if Assigned(LPrevious) then
    begin
      LPrevious(ASender, ADragObject);
    end;

    if LEvents.ViewRevision <> LRevision then
    begin
      LFrame.AbortDrag(TControl(ASender));
      Exit;
    end;

    if not LSource then
    begin
      Exit;
    end;

    if ADragObject <> nil then
    begin
      LFrame.AbortDrag(TControl(ASender));
      LRenderer.GestureFailed(LOriginID,
        'A custom LCL drag object requires its own explicit Nyx transfer adapter');
      Exit;
    end;
    { Supply an object even when refused. Otherwise LCL silently substitutes an
      untyped default object. Cancellation waits until its constructor returns. }
    LDrag := TNyxNativeDragObject.Create(TControl(ASender));
    ADragObject := LDrag;
    FActiveDrag := LDrag;
    LDrag.Events := LEvents;
    LDrag.Revision := LRevision;
    LDrag.SourceID := LOriginID;
    LDrag.Transfer := NyxTransferFromData(NyxArray([]), True);
    LResult := DragEvent(ASender, LDrag, ntDragStart, ndpStart, 0, 0);

    if LEvents.ViewRevision <> LRevision then
    begin
      LFrame.AbortDrag(TControl(ASender));
      Exit;
    end;

    if not LResult.Offered or not LPolicy.CanIssueCommand or
      (LPolicy.ReadOnly and (ndoMove in LResult.Allowed)) then
    begin
      LFrame.AbortDrag(TControl(ASender));
      LRenderer.GestureFailed(LOriginID,
        'The native drag source did not offer an admitted sequential transfer');
      Exit;
    end;
    LDrag.Transfer := LResult.Transfer;
    LDrag.Allowed := LResult.Allowed;
    LDrag.Offered := True;
    LRenderer.FLastGestureError := '';
  finally
    LFrame.Leave;
  end;
end;

procedure TNyxLCLBinding.EndDrag(ASender, ATarget: TObject; AX, AY: Integer);
var
  LPrevious: TEndDragEvent;
  LDrag: TDragObject;
  LEvents: INyxEvents;
  LRevision: Integer;
  LFrame: INyxNativeGestureFrame;
begin

  if FRenderer.FDesignMode then
  begin
    Exit;
  end;
  LPrevious := FPointerHooks[PointerSlot(ASender)].EndDrag;
  LEvents := FRenderer.FEvents;
  LRevision := LEvents.ViewRevision;
  LFrame := FRenderer.FPhysicalFrame;
  LDrag := FActiveDrag;
  FActiveDrag := nil;
  LFrame.Enter;
  try

    if Assigned(LPrevious) then
    begin
      LPrevious(ASender, ATarget, AX, AY);
    end;

    if (LEvents.ViewRevision = LRevision) and
      (LDrag is TNyxNativeDragObject) and TNyxNativeDragObject(LDrag).Live then
    begin
      DragEvent(ASender, LDrag, ntDragEnd, ndpEnd, AX, AY);
    end;
  finally
    LFrame.Leave;
  end;
end;

procedure TNyxLCLBinding.DragOver(ASender, ASource: TObject; AX, AY: Integer;
  AState: TDragState; var AAccept: Boolean);
var
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

  if FRenderer.FDesignMode and not FRenderer.FDesignerInput.DropEnabled then
  begin
    AAccept := False;
    Exit;
  end;
  LPrevious := FPointerHooks[PointerSlot(ASender)].DragOver;
  LEvents := FRenderer.FEvents;
  LRevision := LEvents.ViewRevision;
  LFrame := FRenderer.FPhysicalFrame;
  LOriginID := FNode.ID;
  LTarget := FRenderer.FDesignMode or (FNode.Prop('drop-target') = 'true');
  LFrame.Enter;
  try

    if not FRenderer.FDesignMode and Assigned(LPrevious) then
    begin
      LPrevious(ASender, ASource, AX, AY, AState, AAccept);
    end;

    if LEvents.ViewRevision <> LRevision then
    begin
      AAccept := False;
      Exit;
    end;

    if not LTarget then
    begin
      Exit;
    end;
    AAccept := False;

    if not (ASource is TNyxNativeDragObject) then
    begin
      Exit;
    end;
    LDrag := TNyxNativeDragObject(ASource);

    if not LDrag.Live then
    begin
      Exit;
    end;
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
    LResult := DragEvent(ASender, LDrag, LTrigger, LPhase, AX, AY);

    if (LEvents.ViewRevision <> LRevision) or not LDrag.Live then
    begin
      AAccept := False;
      Exit;
    end;

    if AState <> dsDragLeave then
    begin
      LDrag.HoverID := LOriginID;
      LDrag.HoverOperation := ndoNone;

      if LResult.Accepted then
      begin
        LDrag.HoverOperation := LResult.Operation;
      end;
      AAccept := LDrag.HoverOperation <> ndoNone;
    end;
  finally
    LFrame.Leave;
  end;
end;

procedure TNyxLCLBinding.DragDrop(ASender, ASource: TObject; AX, AY: Integer);
var
  LPrevious: TDragDropEvent;
  LEvents: INyxEvents;
  LRevision: Integer;
  LFrame: INyxNativeGestureFrame;
  LDrag: TNyxNativeDragObject;
  LResult: TNyxGestureResult;
  LTarget: Boolean;
begin

  if FRenderer.FDesignMode and not FRenderer.FDesignerInput.DropEnabled then
  begin
    Exit;
  end;
  LPrevious := FPointerHooks[PointerSlot(ASender)].DragDrop;
  LEvents := FRenderer.FEvents;
  LRevision := LEvents.ViewRevision;
  LFrame := FRenderer.FPhysicalFrame;
  LTarget := FRenderer.FDesignMode or (FNode.Prop('drop-target') = 'true');
  LFrame.Enter;
  try

    if not LTarget then
    begin

      if Assigned(LPrevious) then
      begin
        LPrevious(ASender, ASource, AX, AY);
      end;
      Exit;
    end;

    if not (ASource is TNyxNativeDragObject) then
    begin
      Exit;
    end;
    LDrag := TNyxNativeDragObject(ASource);

    if not LDrag.Live then
    begin
      Exit;
    end;
    LResult := DragEvent(ASender, LDrag, ntDrop, ndpDrop, AX, AY);

    if (LEvents.ViewRevision = LRevision) and LDrag.Live and LResult.Accepted then
    begin
      LDrag.Operation := LResult.Operation;
    end;
    { Accepted operations never mutate the source or target model implicitly.
      The application's typed callback owns its copy/move/link operation. }
  finally
    LFrame.Leave;
  end;
end;

function TNyxLCLBinding.DragEvent(ASender: TObject; ADragObject: TDragObject;
  ATrigger: TNyxTrigger; APhase: TNyxDragPhase; AX, AY: Integer): TNyxGestureResult;
var
  LDrag: TNyxNativeDragObject;
  LDispatch: TNyxDispatch;
  LCapabilities: TNyxGestureCapabilities;
  LDecision: INyxGestureDecision;
  LPosition: TPoint;
  LOperation: TNyxDropOperation;
begin
  Result := Default(TNyxGestureResult);

  if FRenderer.FUpdating or
    (FRenderer.FDesignMode and ((APhase in [ndpStart, ndpDrag, ndpEnd]) or
      not FRenderer.FDesignerInput.DropEnabled)) or
    (not FRenderer.FDesignMode and not NyxInteractionPolicy(FNode).CanIssueCommand) or
    not (ADragObject is TNyxNativeDragObject) then
  begin
    Exit;
  end;
  LDrag := TNyxNativeDragObject(ADragObject);
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

  if APhase = ndpEnd then
  begin
    LOperation := LDrag.Operation;
  end;

  if FRenderer.FDesignMode then
  begin
    LDispatch := Default(TNyxDispatch);
    LDispatch.Info := NyxDesignerDragEvent(FNode, ATrigger);
  end
  else
  begin
    LDispatch := FRenderer.FLiveBindings.Signal(FNode, ATrigger);
  end;
  LDispatch.Info.HasDrag := True;
  LDispatch.Info.Drag := NyxDragSnapshot(APhase, LDrag.Transfer,
    LDrag.Allowed, LOperation, LDrag.SourceID, LCapabilities <> []);
  LDispatch.Info.HasPointer := True;
  LDispatch.Info.Pointer.Kind := npiMouse;
  LDispatch.Info.Pointer.HasPosition := not (APhase in [ndpStart, ndpEnd]);

  if LDispatch.Info.Pointer.HasPosition then
  begin
    LPosition := FControl.ScreenToClient(TControl(ASender).ClientToScreen(Point(AX, AY)));
    LDispatch.Info.Pointer.X := Double(LPosition.X) + FPlacement.ContentOffsetX;
    LDispatch.Info.Pointer.Y := Double(LPosition.Y) + FPlacement.ContentOffsetY;
  end;
  LDecision := NewNyxGestureDecision(LCapabilities, LDrag.Allowed);
  try

    if FRenderer.FDesignMode then
    begin

      if Assigned(FRenderer.FOnDesignerGesture) then
      begin
        FRenderer.FOnDesignerGesture(NyxDesignerTarget(FNode), LDispatch.Info, LDecision);
      end;
    end
    else
    begin
      DispatchNyxGesture(FRenderer.FEvents, FNode, LDispatch, LDecision);
    end;
  finally
    Result := LDecision.Seal;
  end;
end;

procedure TNyxLCLBinding.DisconnectControl(AControl: TControl);
begin

  if AControl = nil then
  begin
    Exit;
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
  LPrevious: TMouseWheelEvent;
  LEvents: INyxEvents;
  LRevision: Integer;
begin

  if FRenderer.FDesignMode then
  begin
    Exit;
  end;
  LPrevious := FPointerHooks[PointerSlot(ASender)].Wheel;
  LEvents := FRenderer.FEvents;
  LRevision := LEvents.ViewRevision;
  WheelEvent(ASender, AShift, ADelta, False, AHandled);

  if not AHandled and (LEvents.ViewRevision = LRevision) and Assigned(LPrevious) then
  begin
    LPrevious(ASender, AShift, ADelta, APosition, AHandled);
  end;
end;

procedure TNyxLCLBinding.WheelHorz(ASender: TObject; AShift: TShiftState;
  ADelta: Integer; APosition: TPoint; var AHandled: Boolean);
var
  LPrevious: TMouseWheelEvent;
  LEvents: INyxEvents;
  LRevision: Integer;
begin

  if FRenderer.FDesignMode then
  begin
    Exit;
  end;
  LPrevious := FPointerHooks[PointerSlot(ASender)].WheelHorz;
  LEvents := FRenderer.FEvents;
  LRevision := LEvents.ViewRevision;
  WheelEvent(ASender, AShift, ADelta, True, AHandled);

  if not AHandled and (LEvents.ViewRevision = LRevision) and Assigned(LPrevious) then
  begin
    LPrevious(ASender, AShift, ADelta, APosition, AHandled);
  end;
end;

procedure TNyxLCLBinding.WheelEvent(ASender: TObject; AShift: TShiftState;
  ADelta: Integer; AHorizontal: Boolean; var AHandled: Boolean);
var
  LEvents: INyxEvents;
  LDispatch: TNyxDispatch;
  LModifiers: TNyxKeyModifiers;
  LX, LY: Double;
begin
  LEvents := FRenderer.FEvents;

  if AHandled or FRenderer.FUpdating or FRenderer.FDesignMode or
    (not LEvents.HasSubscribers(ntBeforeWheel) and not LEvents.HasSubscribers(ntWheel) and
      not LEvents.HasSubscribers(ntAfterWheel)) then
  begin
    Exit;
  end;
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
  LY := -ADelta / 120;

  if AHorizontal then
  begin
    { LCL horizontal positive deltas mean right; vertical positive means up. }
    LX := ADelta / 120;
    LY := 0;
  end;
  LDispatch := FRenderer.FLiveBindings.Signal(FNode, ntWheel);
  LDispatch.Info.HasWheel := True;
  LDispatch.Info.Wheel := NyxWheel(LX, LY, 0, nwuDetents, LModifiers, True);
  AHandled := DispatchNyxWheel(LEvents, FNode, LDispatch,
    FRenderer.FLiveBindings.SignalSnapshot);
  { No binding/renderer access follows callbacks that may dispose this view. }
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
  Result := False;
  LEvents := FRenderer.FEvents;

  if FRenderer.FUpdating or FRenderer.FDesignMode or not LEvents.HasSubscribers(ATrigger) or
    not NyxInteractionPolicy(FNode).CanIssueCommand then
  begin
    Exit;
  end;
  LDispatch := FRenderer.FLiveBindings.Signal(FNode, ATrigger);
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
    LPosition := FControl.ScreenToClient(TControl(ASender).ClientToScreen(APosition));
    LDispatch.Info.Pointer.X := Double(LPosition.X) + FPlacement.ContentOffsetX;
    LDispatch.Info.Pointer.Y := Double(LPosition.Y) + FPlacement.ContentOffsetY;
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

  if ssAltGr in AShift then
  begin
    Include(LDispatch.Info.Pointer.Modifiers, nmAltGraph);
  end;

  if ATrigger = ntContextMenu then
  begin
    Result := DispatchNyxInput(LEvents, FNode, LDispatch);
  end
  else if ATrigger in [ntPointerDown, ntPointerMove, ntPointerUp] then
  begin
    LRenderer := FRenderer;
    LFrame := LRenderer.FPhysicalFrame;
    LObserver := LRenderer.FCaptureObserver;
    LRevision := LEvents.ViewRevision;
    LControl := FControl;
    LInput := FInput;
    LOriginID := FNode.ID;
    LCapabilities := [];

    if (ATrigger in [ntPointerDown, ntPointerMove]) and
      ((LDispatch.Info.Pointer.Buttons <> []) or (ATrigger = ntPointerDown)) then
    begin
      Include(LCapabilities, ngcCapturePointer);
    end;

    if NyxLCLHasPointerCapture(LControl) or NyxLCLHasPointerCapture(LInput) then
    begin
      Include(LCapabilities, ngcReleasePointer);
    end;
    LDecision := NewNyxGestureDecision(LCapabilities);
    LFrame.Enter;
    try
      DispatchNyxGesture(LEvents, FNode, LDispatch, LDecision);
      LResult := LDecision.Seal;

      if LEvents.ViewRevision <> LRevision then
      begin
        Exit;
      end;
      try
        case LResult.PointerRequest of
          nprUnchanged:
            begin
              { This dispatch requests no change to native pointer capture. }
            end;
          nprCapture:
            begin
              CaptureNyxLCLPointer(LControl);
            end;
          nprRelease:
            begin
              ReleaseNyxLCLPointer(LControl);
              ReleaseNyxLCLPointer(LInput);
            end;
        end;
      except
        on LException: Exception do
        begin
          LRenderer.GestureFailed(LOriginID, LException.Message);
          Exit;
        end;
      end;

      if LResult.PointerRequest <> nprUnchanged then
      begin
        LRenderer.FLastGestureError := '';
      end;

      if LObserver <> nil then
      begin
        LObserver.Observe(LControl);

        if (LEvents.ViewRevision = LRevision) and (LInput <> nil) and
          (LInput <> LControl) then
        begin
          LObserver.Observe(LInput);
        end;
      end;
    finally
      LFrame.Leave;
    end;
  end
  else
  begin
    FRenderer.Emit(FNode, LDispatch);
  end;
end;

procedure TNyxLCLBinding.DoubleClick(ASender: TObject);
var
  LEvents: INyxEvents;
  LRevision: Integer;
  LPrevious: TNotifyEvent;
  LPosition: TPoint;
begin

  if FRenderer.FDesignMode then
  begin
    Exit;
  end;
  LEvents := FRenderer.FEvents;
  LRevision := LEvents.ViewRevision;
  LPrevious := FPointerHooks[PointerSlot(ASender)].DoubleClick;

  if Assigned(LPrevious) then
  begin
    LPrevious(ASender);
  end;

  if LEvents.ViewRevision <> LRevision then
  begin
    Exit;
  end;
  LPosition := TControl(ASender).ScreenToClient(Mouse.CursorPos);
  PointerEvent(ASender, ntDoubleClick, LPosition, True, npbNone, []);
end;

procedure TNyxLCLBinding.PointerEnter(ASender: TObject);
var
  LEvents: INyxEvents;
  LRevision: Integer;
  LPrevious: TNotifyEvent;
  LPosition: TPoint;
begin

  if FRenderer.FDesignMode then
  begin
    Exit;
  end;
  LEvents := FRenderer.FEvents;
  LRevision := LEvents.ViewRevision;
  LPrevious := FPointerHooks[PointerSlot(ASender)].Enter;

  if Assigned(LPrevious) then
  begin
    LPrevious(ASender);
  end;

  if LEvents.ViewRevision <> LRevision then
  begin
    Exit;
  end;
  LPosition := TControl(ASender).ScreenToClient(Mouse.CursorPos);
  PointerEvent(ASender, ntPointerEnter, LPosition, True, npbNone, []);
end;

procedure TNyxLCLBinding.PointerExit(ASender: TObject);
var
  LEvents: INyxEvents;
  LRevision: Integer;
  LPrevious: TNotifyEvent;
  LPosition: TPoint;
begin

  if FRenderer.FDesignMode then
  begin
    Exit;
  end;
  LEvents := FRenderer.FEvents;
  LRevision := LEvents.ViewRevision;
  LPrevious := FPointerHooks[PointerSlot(ASender)].Leave;

  if Assigned(LPrevious) then
  begin
    LPrevious(ASender);
  end;

  if LEvents.ViewRevision <> LRevision then
  begin
    Exit;
  end;
  LPosition := TControl(ASender).ScreenToClient(Mouse.CursorPos);
  PointerEvent(ASender, ntPointerExit, LPosition, True, npbNone, []);
end;

procedure TNyxLCLBinding.PointerDown(ASender: TObject; AButton: TMouseButton;
  AShift: TShiftState; AX, AY: Integer);
var
  LEvents: INyxEvents;
  LRevision: Integer;
  LPrevious: TMouseEvent;
begin

  if FRenderer.FDesignMode then
  begin
    Exit;
  end;
  LEvents := FRenderer.FEvents;
  LRevision := LEvents.ViewRevision;
  LPrevious := FPointerHooks[PointerSlot(ASender)].Down;

  if Assigned(LPrevious) then
  begin
    LPrevious(ASender, AButton, AShift, AX, AY);
  end;

  if LEvents.ViewRevision <> LRevision then
  begin
    Exit;
  end;
  PointerEvent(ASender, ntPointerDown, Point(AX, AY), True,
    NativePointerButton(AButton), AShift);
end;

procedure TNyxLCLBinding.PointerUp(ASender: TObject; AButton: TMouseButton;
  AShift: TShiftState; AX, AY: Integer);
var
  LEvents: INyxEvents;
  LRevision: Integer;
  LPrevious: TMouseEvent;
begin

  if FRenderer.FDesignMode then
  begin
    Exit;
  end;
  LEvents := FRenderer.FEvents;
  LRevision := LEvents.ViewRevision;
  LPrevious := FPointerHooks[PointerSlot(ASender)].Up;

  if Assigned(LPrevious) then
  begin
    LPrevious(ASender, AButton, AShift, AX, AY);
  end;

  if LEvents.ViewRevision <> LRevision then
  begin
    Exit;
  end;
  PointerEvent(ASender, ntPointerUp, Point(AX, AY), True,
    NativePointerButton(AButton), AShift);
end;

procedure TNyxLCLBinding.PointerMove(ASender: TObject; AShift: TShiftState; AX, AY: Integer);
var
  LEvents: INyxEvents;
  LRevision: Integer;
  LPrevious: TMouseMoveEvent;
begin

  if FRenderer.FDesignMode then
  begin
    Exit;
  end;
  LEvents := FRenderer.FEvents;
  LRevision := LEvents.ViewRevision;
  LPrevious := FPointerHooks[PointerSlot(ASender)].Move;

  if Assigned(LPrevious) then
  begin
    LPrevious(ASender, AShift, AX, AY);
  end;

  if LEvents.ViewRevision <> LRevision then
  begin
    Exit;
  end;
  PointerEvent(ASender, ntPointerMove, Point(AX, AY), True,
    npbNone, AShift);
end;

procedure TNyxLCLBinding.ContextMenu(ASender: TObject; APosition: TPoint;
  var AHandled: Boolean);
var
  LEvents: INyxEvents;
  LRevision: Integer;
  LPrevious: TContextPopupEvent;
begin

  if FRenderer.FDesignMode then
  begin
    Exit;
  end;
  LEvents := FRenderer.FEvents;
  LRevision := LEvents.ViewRevision;
  LPrevious := FPointerHooks[PointerSlot(ASender)].ContextMenu;

  if Assigned(LPrevious) then
  begin
    LPrevious(ASender, APosition, AHandled);
  end;

  if LEvents.ViewRevision <> LRevision then
  begin
    AHandled := True;
    Exit;
  end;

  if not AHandled then
  begin
    AHandled := PointerEvent(ASender, ntContextMenu, APosition,
      (APosition.X >= 0) and (APosition.Y >= 0), npbNone, []);
  end;
end;

procedure TNyxLCLBinding.Focus(ASender: TObject);
begin

  if FRenderer.FUpdating or FRenderer.FDesignMode then
  begin
    Exit;
  end;
  { LCL owns focus transitions. Repaint only the frame that visualizes that
    native state; there is no competing Nyx focus or keyboard state machine. }

  if (FInput <> nil) and (FInput.Parent is TNyxLCLSurface) then
  begin
    FInput.Parent.Invalidate;
  end;
end;

procedure TNyxLCLBinding.SplitLayout(ASender: TObject);
var
  LSplit: TNyxLCLSplitView;
  LIndex: Integer;
begin
  LSplit := TNyxLCLSplitView(ASender);
  for LIndex := 0 to FNode.Count - 1 do
  begin
    FRenderer.Layout(FNode.Children[LIndex], 0, 0,
      LSplit.Panes[LIndex].ClientWidth, LSplit.Panes[LIndex].ClientHeight);
  end;
end;

procedure TNyxLCLBinding.SplitChanged(ASender: TObject);
begin
  FRenderer.Emit(FNode, NyxSplitChange(FNode, TNyxLCLSplitView(ASender).State.Position));
end;

procedure TNyxLCLBinding.Click(ASender: TObject);
var
  LDispatch: TNyxDispatch;
  LEvents: INyxEvents;
  LRevision: Integer;
  LPrevious: TNotifyEvent;
begin

  if FRenderer.FDesignMode then
  begin

    if not FRenderer.FUpdating and Assigned(FRenderer.FOnEvent) then
    begin
      FRenderer.FOnEvent(FNode, NyxDesignEvent(FNode, ntDesignSelect));
    end;
    { A designer selection never calls a creator's application click hook or
      a live binding action. Native editable controls keep their own defaults. }
    Exit;
  end;
  LEvents := FRenderer.FEvents;
  LRevision := LEvents.ViewRevision;
  LPrevious := FPreviousClick;

  if (ASender = FInput) and (FInput <> FControl) then
  begin
    LPrevious := FPreviousInputClick;
  end;

  if Assigned(LPrevious) then
  begin
    LPrevious(ASender);
  end;

  if LEvents.ViewRevision <> LRevision then
  begin
    { A custom callback may replace/dispose this view and its binding. Retain
      only the router above and never read Self's borrowed fields afterward. }
    Exit;
  end;

  if FRenderer.FUpdating then
  begin
    Exit;
  end;
  { General click subscriptions are opt-in. Older global callbacks retain their
    command/named-click behavior, and ordinary checkbox edits still emit once. }

  if not NyxHasLegacyClick(FNode) and not FRenderer.FEvents.HasSubscribers(ntClick) and
    (FNode.Prop(NyxAttributeName(atAction)) = '') then
  begin
    Exit;
  end;
  try
    LDispatch := FRenderer.FLiveBindings.Dispatch(FNode, ntClick);
    FRenderer.FLastBindingError := '';
    FRenderer.FLastBindingFailure := nbfNone;
  except
    on LException: ENyxStateNotification do
    begin
      FRenderer.BindingFailed(FNode, LException.Message, nbfNotificationFailed);
      Exit;
    end;
    on LException: Exception do
    begin
      FRenderer.BindingFailed(FNode, LException.Message);
      Exit;
    end;
  end;

  FRenderer.Emit(FNode, LDispatch);
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

procedure TNyxLCLRenderer.EditingChanged(const AOriginID: TNyxText;
  const AEditing: TNyxEditingSnapshot);
var
  LIndex: Integer;
  LBinding: TNyxLCLBinding;
  LEvents: INyxEvents;
  LRevision: Integer;
  LTrigger: TNyxTrigger;
  LDispatch: TNyxDispatch;
begin
  LEvents := FEvents;
  LRevision := LEvents.ViewRevision;

  if FUpdating or FDesignMode then
  begin
    Exit;
  end;
  LBinding := nil;
  for LIndex := 0 to High(FBindings) do
  begin

    if FBindings[LIndex].FNode.ID = AOriginID then
    begin
      LBinding := FBindings[LIndex];
      Break;
    end;
  end;

  if (LBinding = nil) or not NyxSupportsTextInput(LBinding.FNode) or
    not NyxInteractionPolicy(LBinding.FNode).CanIssueCommand then
  begin
    Exit;
  end;
  case AEditing.Phase of
    nepCompositionStart:
      begin

        if not NyxInteractionPolicy(LBinding.FNode).CanEditValue then
        begin
          Exit;
        end;
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
        LBinding.Change(LBinding.FInput);

        if LEvents.ViewRevision <> LRevision then
        begin
          Exit;
        end;
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
  LDispatch.Info.HasEditing := True;
  LDispatch.Info.Editing := AEditing;
  { Last borrowed renderer/control access. The observer keeps its own producer
    alive, while navigation revokes its handles and pending final admission. }
  Emit(LBinding.FNode, LDispatch);
end;

procedure TNyxLCLBinding.Change(ASender: TObject);
var
  LValue: TNyxText;
  LDispatch: TNyxDispatch;
  LProposal: TNyxDispatch;
  LEvents: INyxEvents;
  LRevision: Integer;
  LConsumed: Boolean;
  LEditing: TNyxEditingSnapshot;
begin

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

  if not LEditing.Defined and NyxSupportsTextInput(FNode) then
  begin
    LEditing := CaptureNyxLCLEditing(TWinControl(FInput), nepInput, False);
  end;

  if not NyxInteractionPolicy(FNode).CanEditValue then
  begin
    { Native selectors lacking ReadOnly may propose a changed physical value.
      Restore before any callback/admission; editable text uses the real LCL
      ReadOnly flag as well. Programmatic state refresh remains independent. }
    FRenderer.FForceValues := True;
    try
      FRenderer.Sync;
    finally
      FRenderer.FForceValues := False;
    end;
    Exit;
  end;

  if FDeferredValue and not FCommitting then
  begin
    { A numeric draft such as '-' or '0.' is meaningful while typing. Lazarus's
      editing-complete event supplies the admission boundary, matching browser
      commit behavior instead of restoring the old number on every keystroke. }
    Exit;
  end;
  LValue := '';

  if FInput is TMemo then
  begin
    LValue := StringReplace(TMemo(FInput).Text, #13#10, #10, [rfReplaceAll]);
  end
  else if FInput is TCustomEdit then
  begin
    LValue := TEdit(FInput).Text;
  end
  else if FInput is TComboBox then
  begin
    LValue := TComboBox(FInput).Text;
  end
  else if FInput is TCheckBox then
  begin
    LValue := 'false';

    if TCheckBox(FInput).Checked then
    begin
      LValue := 'true';
    end;
  end
  else if FInput is TRadioButton then
  begin
    LValue := 'false';

    if TRadioButton(FInput).Checked then
    begin
      LValue := 'true';
    end;
  end
  else if FInput is TTrackBar then
  begin
    LValue := IntToStr(TTrackBar(FInput).Position);
  end;

  if FRenderer.FDesignMode then
  begin
    { This is a realized proposal, not a store/application mutation. The host
      owns admission and can restore a refused value through ordinary Sync. }
    FNode.SetProp('value', LValue);

    if Assigned(FRenderer.FOnEvent) then
    begin
      FRenderer.FOnEvent(FNode, NyxDesignEvent(FNode, ntDesignValue));
    end;
    Exit;
  end;
  { Win32 may deliver a second change after a setter/restoration has returned.
    It already contains the accepted value. Ignore it so a rejected edit cannot
    emit a synthetic success or clear its diagnostic after the update guard. }

  if LValue = FNode.Prop('value') then
  begin
    Exit;
  end;
  LEvents := FRenderer.FEvents;
  LRevision := LEvents.ViewRevision;

  if NyxSupportsTextInput(FNode) and LEvents.HasSubscribers(ntBeforeTextInput) then
  begin
    LProposal := FRenderer.FLiveBindings.ProposeText(FNode, LValue);
    LProposal.Info.HasEditing := LEditing.Defined;
    LProposal.Info.Editing := LEditing;
    LConsumed := DispatchNyxInput(LEvents, FNode, LProposal);

    if LEvents.ViewRevision <> LRevision then
    begin
      Exit;
    end;

    if LConsumed then
    begin
      FRenderer.FForceValues := True;
      try
        FRenderer.Sync;
      finally
        FRenderer.FForceValues := False;
      end;
      LProposal.Info.DefaultPrevented := True;
      DispatchNyxTextResult(LEvents, FNode, LProposal,
        FRenderer.FLiveBindings.SignalSnapshot);
      Exit;
    end;
  end;
  try
    LDispatch := FRenderer.FLiveBindings.Edit(FNode, LValue);
    LDispatch.Info.HasEditing := LEditing.Defined;
    LDispatch.Info.Editing := LEditing;
    FRenderer.FLastBindingError := '';
    FRenderer.FLastBindingFailure := nbfNone;
  except
    on LException: ENyxStateNotification do
    begin
      FRenderer.BindingFailed(FNode, LException.Message, nbfNotificationFailed);
      Exit;
    end;
    on LException: Exception do
    begin
      FRenderer.BindingFailed(FNode, LException.Message);
      Exit;
    end;
  end;

  FRenderer.Emit(FNode, LDispatch);
end;

function TNyxLCLRenderer.EmitNamed(const AOriginID: TNyxText;
  const AName: TNyxEventRef; const APayload: TNyxDataValue;
  AHasPayload: Boolean): Boolean;
var
  LBinding: TNyxLCLBinding;
  LDispatch: TNyxDispatch;
begin
  Result := False;

  if FDesignMode then
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
  LDispatch: TNyxDispatch;
begin

  if FRenderer.FUpdating or FRenderer.FDesignMode then
  begin
    Exit;
  end;
  LDispatch := FRenderer.FLiveBindings.Signal(FNode, ntSelectionChange);
  LDispatch.Info.HasCollectionSelection := True;
  LDispatch.Info.SelectionBefore := ABefore.Snapshot;
  LDispatch.Info.Selection := AAfter.Snapshot;
  FRenderer.Emit(FNode, LDispatch);
end;

procedure TNyxLCLRenderer.Emit(AOrigin: TNyxNode; const ADispatch: TNyxDispatch);
var
  LEvents: INyxEvents;
  LLegacy: TNyxLCLEvent;
  LRevision: Integer;
  LLegacyClick: Boolean;
begin

  if FDesignMode or (ADispatch.EventName = '') then
  begin
    Exit;
  end;
  LEvents := FEvents;
  LLegacy := FOnEvent;
  LLegacyClick := NyxHasLegacyClick(AOrigin);
  LRevision := LEvents.ViewRevision;
  AOrigin.AcquireReference;
  ADispatch.Source.AcquireReference;
  try
    LEvents.Dispatch(ADispatch.Info, AOrigin.DesignID, ADispatch.Source.DesignID);

    if ADispatch.Info.HasTextEdit and (LEvents.ViewRevision = LRevision) then
    begin
      DispatchNyxTextResult(LEvents, AOrigin, ADispatch, FLiveBindings.SignalSnapshot);
    end;

    if Assigned(LLegacy) and (ADispatch.Info.Trigger in [ntClick, ntChange, ntNamed]) and
      ((ADispatch.Info.Trigger <> ntClick) or LLegacyClick) and
      (LEvents.ViewRevision = LRevision) then
    begin
      LLegacy(ADispatch.Source, ADispatch.Info);
    end;
  finally
    ADispatch.Source.ReleaseReference;
    AOrigin.ReleaseReference;
  end;
end;

procedure TNyxLCLBinding.Enter(ASender: TObject);
var
  LEvents: INyxEvents;
  LRevision: Integer;
begin

  if FRenderer.FVirtualLayout and not FRenderer.FUpdating then
  begin
    FRenderer.Reveal(FNode.ID, niRuntime);
  end;

  if FRenderer.FDesignMode or FRenderer.FUpdating then
  begin
    Exit;
  end;
  LEvents := FRenderer.FEvents;
  LRevision := LEvents.ViewRevision;
  Focus(ASender);

  if Assigned(FPreviousEnter) then
  begin
    FPreviousEnter(ASender);
  end;

  if LEvents.ViewRevision <> LRevision then
  begin
    Exit;
  end;

  if not FRenderer.FUpdating and LEvents.HasSubscribers(ntAfterEnter) then
  begin
    FRenderer.Emit(FNode, FRenderer.FLiveBindings.Focus(FNode, ntAfterEnter));
  end;
end;

procedure TNyxLCLBinding.Leave(ASender: TObject);
var
  LEvents: INyxEvents;
  LRevision: Integer;
  LIndex: Integer;
begin
  for LIndex := Low(FPressedKeys) to High(FPressedKeys) do
  begin
    FPressedKeys[LIndex] := False;
  end;

  if FRenderer.FDesignMode or FRenderer.FUpdating then
  begin
    Exit;
  end;
  LEvents := FRenderer.FEvents;
  LRevision := LEvents.ViewRevision;
  Focus(ASender);

  if Assigned(FPreviousExit) then
  begin
    FPreviousExit(ASender);
  end;

  if LEvents.ViewRevision <> LRevision then
  begin
    Exit;
  end;

  if not FRenderer.FUpdating and LEvents.HasSubscribers(ntAfterExit) then
  begin
    FRenderer.Emit(FNode, FRenderer.FLiveBindings.Focus(FNode, ntAfterExit));
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
  LEvents: INyxEvents;
  LRevision: Integer;
  LPrevious: TKeyEvent;
  LModifiers: TNyxKeyModifiers;
  LRepeating: Boolean;
  LDispatch: TNyxDispatch;
  LConsumed: Boolean;
begin

  if FRenderer.FDesignMode or FRenderer.FUpdating then
  begin
    Exit;
  end;
  LEvents := FRenderer.FEvents;
  LRevision := LEvents.ViewRevision;
  LPrevious := FPreviousKeyDown;
  LRepeating := False;

  if ATrigger = ntKeyUp then
  begin
    LPrevious := FPreviousKeyUp;
  end;

  if AKey <= High(FPressedKeys) then
  begin
    LRepeating := (ATrigger = ntKeyDown) and FPressedKeys[AKey];
    FPressedKeys[AKey] := ATrigger = ntKeyDown;
  end;

  if Assigned(LPrevious) then
  begin
    LPrevious(ASender, AKey, AShift);
  end;

  if LEvents.ViewRevision <> LRevision then
  begin
    { The custom hook disposed this binding; touch only the caller's key now. }
    AKey := 0;
    Exit;
  end;
  { Preserve consumed custom keys and the widgetset's IME/process/packet input.
    Those are text-composition messages, not ordinary shortcut keys. }

  if FComposing or (AKey = 0) or (AKey = $E5) or (AKey = $E7) or FRenderer.FUpdating or
    not NyxHasKeyboardSubscribers(LEvents, ATrigger) then
  begin
    Exit;
  end;
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

  if ssAltGr in AShift then
  begin
    Include(LModifiers, nmAltGraph);
  end;
  LDispatch := FRenderer.FLiveBindings.Keyboard(FNode, ATrigger,
    NyxKeyStroke(NyxKeyFromVirtualCode(AKey), LModifiers, LRepeating));
  LConsumed := DispatchNyxKeyboard(LEvents, FNode, LDispatch,
    FRenderer.FLiveBindings.SignalSnapshot);

  if LConsumed or (LEvents.ViewRevision <> LRevision) then
  begin
    AKey := 0;
  end;
end;

procedure TNyxLCLBinding.CommitValue(ASender: TObject);
begin
  FCommitting := True;
  try
    Change(ASender);
  finally
    FCommitting := False;
  end;
end;

procedure TNyxLCLRenderer.BindingFailed(ANode: TNyxNode; const AReason: TNyxText;
  AFailure: TNyxBindingFailure);
begin
  FLastBindingError := AReason;
  FLastBindingFailure := AFailure;

  if AFailure = nbfRejected then
  begin
    FForceValues := True;
    try
      Sync;
    finally
      FForceValues := False;
    end;
  end;

  if Assigned(FOnBindingError) then
  begin
    FOnBindingError(ANode, AReason, AFailure);
  end;
end;

procedure TNyxLCLBinding.SyncLiteralItems;
var
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
  LRows := TNyxStrings.Create;
  try
    LRows.Text := LText;
    case LKind of
      nkSelect:
        begin
          LChoice := TComboBox(FInput);
          LSelected := FNode.Prop('value');

          if FHasValueBaseline and (FLastValue = LSelected) then
          begin
            LSelected := LChoice.Text;
          end;
          LChoice.Items.BeginUpdate;
          try
            LChoice.Items.Clear;
            for LIndex := 0 to LRows.Count - 1 do
            begin
              LChoice.Items.Add(LRows[LIndex]);
            end;
            LChoice.ItemIndex := LChoice.Items.IndexOf(LSelected);

            if (LChoice.ItemIndex < 0) and FHasValueBaseline and
              (FLastValue = FNode.Prop('value')) and
              (LChoice.Style <> csDropDownList) then
            begin
              { The existing editable native choice can have an unfinished
                draft. A row publication does not replace that physical text. }
              LChoice.Text := LSelected;
            end;
          finally
            LChoice.Items.EndUpdate;
          end;
        end;
      nkList:
        begin
          LList := TListBox(FControl);
          LSelected := '';
          LOldTop := LList.TopIndex;

          if LList.ItemIndex >= 0 then
          begin
            LSelected := LList.Items[LList.ItemIndex];
          end;
          LList.Items.BeginUpdate;
          try
            LList.Items.Clear;
            for LIndex := 0 to LRows.Count - 1 do
            begin
              LList.Items.Add(LRows[LIndex]);
            end;
            LList.ItemIndex := LList.Items.IndexOf(LSelected);

            if LRows.Count > 0 then
            begin
              LList.TopIndex := Min(LOldTop, LRows.Count - 1);
            end;
          finally
            LList.Items.EndUpdate;
          end;
        end;
      nkTree:
        begin
          LTree := TTreeView(FControl);
          LSelected := '';

          if LTree.Selected <> nil then
          begin
            LSelected := LTree.Selected.Text;
          end;
          LTree.Items.BeginUpdate;
          try
            LTree.Items.Clear;
            for LIndex := 0 to LRows.Count - 1 do
            begin
              LTree.Items.Add(nil, LRows[LIndex]);
            end;

            if LSelected <> '' then
            begin
              LTree.Selected := LTree.Items.FindNodeWithText(LSelected);
            end;
          finally
            LTree.Items.EndUpdate;
          end;
        end;
      nkTable:
        begin
          LGrid := TStringGrid(FControl);
          LOldRow := LGrid.Row;
          LOldColumn := LGrid.Col;
          LColumns := 1;
          for LIndex := 0 to LRows.Count - 1 do
          begin
            LCells := NyxLiteralCells(LRows[LIndex]);
            try
              LColumns := Max(LColumns, LCells.Count);
            finally
              LCells.Free;
            end;
          end;
          LGrid.BeginUpdate;
          try
            LGrid.FixedRows := 0;
            LGrid.FixedCols := 0;
            LGrid.RowCount := Max(1, LRows.Count);
            LGrid.ColCount := LColumns;
            { A widget needs one physical row even for empty Items. It contains
              no authored data. Clear retained cells when ragged rows shrink. }
            for LIndex := 0 to LGrid.RowCount - 1 do
            begin
              for LColumn := 0 to LGrid.ColCount - 1 do
              begin
                LGrid.Cells[LColumn, LIndex] := '';
              end;
            end;
            for LIndex := 0 to LRows.Count - 1 do
            begin
              LCells := NyxLiteralCells(LRows[LIndex]);
              try
                for LColumn := 0 to LCells.Count - 1 do
                begin
                  LGrid.Cells[LColumn, LIndex] := LCells[LColumn];
                end;
              finally
                LCells.Free;
              end;
            end;

            if LRows.Count > 1 then
            begin
              LGrid.FixedRows := 1;
            end;
            LGrid.Row := EnsureRange(LOldRow, LGrid.FixedRows, LGrid.RowCount - 1);
            LGrid.Col := EnsureRange(LOldColumn, 0, LGrid.ColCount - 1);
          finally
            LGrid.EndUpdate;
          end;
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
end;

procedure TNyxLCLBinding.SyncPicture;
var
  LSource: TNyxText;
  LPicture: TPicture;
begin

  if FCustom or not (FControl is TImage) then
  begin
    Exit;
  end;
  FControl.AccessibleDescription := FNode.Prop('alt');
  LSource := FNode.Prop('src');

  if FHasSourceBaseline and (FLastSource = LSource) then
  begin
    Exit;
  end;
  LPicture := TPicture.Create;
  try
    { Standard native images resolve local files. Network/portable asset
      providers remain a separate required adapter boundary. A missing file
      clears the old picture; a decoding failure preserves the admitted one. }

    if FileExists(LSource) then
    begin
      LPicture.LoadFromFile(LSource);
    end;
    TImage(FControl).Picture.Assign(LPicture);
    FLastSource := LSource;
    FHasSourceBaseline := True;
  finally
    LPicture.Free;
  end;
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
  LScopes: array of TRadioScope;
  LIndex: Integer;
  LScope: Integer;
  LRadio: TRadioButton;
  LPolicy: TNyxInteractionPolicy;
begin
  SetLength(LScopes, 0);
  for LIndex := 0 to High(FBindings) do
  begin

    if not FBindings[LIndex].FCustom and (FBindings[LIndex].FInput is TRadioButton) then
    begin
      LRadio := TRadioButton(FBindings[LIndex].FInput);
      LRadio.TabStop := False;
      LPolicy := NyxInteractionPolicy(FBindings[LIndex].FNode);

      if not LPolicy.Enabled or not LPolicy.Visible then
      begin
        Continue;
      end;
      LScope := 0;
      while (LScope < Length(LScopes)) and (LScopes[LScope].Parent <> LRadio.Parent) do
      begin
        Inc(LScope);
      end;

      if LScope = Length(LScopes) then
      begin
        SetLength(LScopes, LScope + 1);
        LScopes[LScope].Parent := LRadio.Parent;
        LScopes[LScope].Entry := LRadio;
        LScopes[LScope].Checked := False;
      end;

      if LRadio.Checked and not LScopes[LScope].Checked then
      begin
        LScopes[LScope].Entry := LRadio;
        LScopes[LScope].Checked := True;
      end;
    end;
  end;
  for LScope := 0 to High(LScopes) do
  begin
    LScopes[LScope].Entry.TabStop := True;
  end;
end;

procedure TNyxLCLRenderer.Sync;
var
  LIndex: Integer;
  LBinding: TNyxLCLBinding;
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
begin
  { Native setters can fire change events. One guard covers every control kind,
    including Boolean/range widgets and layout side effects. Skip unchanged edit
    text so synchronization preserves selection, caret and focused drafts. }

  if FUpdating then
  begin
    Exit;
  end;
  FUpdating := True;
  try

    if (FRoot <> nil) and (FPanel <> nil) then
    begin
      UpdateViewport;
    end;
    for LIndex := 0 to Length(FBindings) - 1 do
    begin
      LBinding := FBindings[LIndex];
      LNode := LBinding.FNode;
      LBinding.FControl.Visible := LNode.Prop('visible', 'true') <> 'false';
      LBinding.FControl.Hint := LNode.Prop('hint');
      LBinding.FControl.ShowHint := LBinding.FControl.Hint <> '';
      LBinding.FControl.AccessibleName := LNode.Prop('aria-label', LNode.Prop('text'));
      LPolicy := NyxInteractionPolicy(LNode);
      LEnabled := LPolicy.Enabled;
      LReadOnly := LPolicy.ReadOnly;
      LBinding.FControl.Enabled := LEnabled;

      if LNode.Props.IndexOfName('drag-source') >= 0 then
      begin

        if not FDesignMode and (LNode.Prop('drag-source') = 'true') then
        begin
          TNyxControlAccess(LBinding.FControl).DragMode := dmAutomatic;
        end
        else
        begin
          TNyxControlAccess(LBinding.FControl).DragMode := dmManual;
        end;

        if (LBinding.FInput <> nil) and (LBinding.FInput <> LBinding.FControl) then
        begin
          TNyxControlAccess(LBinding.FInput).DragMode :=
            TNyxControlAccess(LBinding.FControl).DragMode;
        end;
      end;

      if LBinding.FControl is TNyxLCLSplitView then
      begin
        TNyxLCLSplitView(LBinding.FControl).SetInteraction(LEnabled and not FDesignMode,
          LReadOnly or FDesignMode);
      end;

      if LBinding.FCollectionMount <> nil then
      begin
        LBinding.FCollectionMount.SetInteraction(LEnabled and not FDesignMode,
          LReadOnly or FDesignMode);
      end;
      LBinding.SyncLiteralItems;
      LBinding.SyncPicture;
      LBinding.FControl.AccessibleValue := LNode.Prop('pressed');

      if LBinding.FCaption <> nil then
      begin
        LBinding.FCaption.Caption := LNode.Prop('text');
      end
      else if not LBinding.FCustom then
      begin

        if (LBinding.FControl is TLabel) or (LBinding.FControl is TNyxLCLButton) or
          (LBinding.FControl is TButton) or (LBinding.FControl is TCheckBox) or
          (LBinding.FControl is TRadioButton) or (LBinding.FControl is TGroupBox) then
        begin
          TNyxControlAccess(LBinding.FControl).Caption := LNode.Prop('text');
        end;
      end;

      if not LBinding.FCustom and (LNode.ProjectionKind = 'code') and
        (StringReplace(TNyxText(TMemo(LBinding.FControl).Text), #13#10, #10,
        [rfReplaceAll]) <> LNode.Prop('text')) then
      begin
        { A code block's content is Text, not the editable scalar Value. Skip
          unchanged text to preserve its inspection caret/selection and scroll. }
        TMemo(LBinding.FControl).Text := LNode.Prop('text');
      end;

      if LBinding.FControl is TNyxLCLButton then
      begin
        TNyxLCLButton(LBinding.FControl).ApplyTheme(FTheme, LNode.Prop('variant'));
      end;
      LInput := LBinding.FInput;
      LValue := LNode.Prop('value');
      { Composition owns its physical text until the OS end has drained. }

      if LBinding.FComposing then
      begin
        Continue;
      end;
      LWriteValue := FForceValues or not LBinding.FHasValueBaseline or
        (LBinding.FLastValue <> LValue);
      LBinding.FHasValueBaseline := True;
      LBinding.FLastValue := LValue;

      if LInput <> nil then
      begin
        LInput.Enabled := LEnabled;
        LInput.Hint := LBinding.FControl.Hint;
        LInput.ShowHint := LBinding.FControl.ShowHint;
        LInput.AccessibleName := LBinding.FControl.AccessibleName;
      end;

      if LInput is TSpinEdit then
      begin
        LMinimum := StrToIntDef(LNode.Prop('min'), 0);
        LMaximum := StrToIntDef(LNode.Prop('max'), 100);

        if LMinimum > TSpinEdit(LInput).MaxValue then
        begin
          TSpinEdit(LInput).MaxValue := LMaximum;
        end;
        TSpinEdit(LInput).MinValue := LMinimum;
        TSpinEdit(LInput).MaxValue := LMaximum;
        TSpinEdit(LInput).ReadOnly := LReadOnly;

        if LWriteValue and (TSpinEdit(LInput).Value <> StrToIntDef(LValue, 0)) then
        begin
          TSpinEdit(LInput).Value := StrToIntDef(LValue, 0);
        end;
      end
      else if LInput is TCustomEdit then
      begin
        TEdit(LInput).ReadOnly := LReadOnly;
        TEdit(LInput).TextHint := LNode.Prop('placeholder');

        if not LBinding.FCustom and (LNode.ProjectionKind = 'input') then
        begin
          { Formatting can change after mounting. Re-resolve its actual scalar
            domain so a newly numeric face keeps unfinished drafts until commit,
            and returning to Text restores ordinary per-change admission. An
            explicit recipe domain remains authoritative over the format hint. }
          LValueDomain := NyxNodeValueDomain(LNode);
          LBinding.FDeferredValue := LValueDomain.Defined and
            (LValueDomain.Kind in [nskInteger, nskNumber]);

          if LBinding.FDeferredValue then
          begin
            TEdit(LInput).OnEditingDone := LBinding.CommitValue;
          end
          else
          begin
            TEdit(LInput).OnEditingDone := nil;
          end;

          if LNode.Prop('input-type') = 'password' then
          begin
            TEdit(LInput).PasswordChar := '*';
          end
          else
          begin
            TEdit(LInput).PasswordChar := #0;
          end;
        end;

        if LWriteValue and
          (StringReplace(TNyxText(TEdit(LInput).Text), #13#10, #10, [rfReplaceAll]) <> LValue) then
        begin
          TEdit(LInput).Text := LValue;
        end;
      end
      else if LInput is TComboBox then
      begin

        if LWriteValue and (TNyxText(TComboBox(LInput).Text) <> LValue) then
        begin
          TComboBox(LInput).ItemIndex := TComboBox(LInput).Items.IndexOf(LValue);
        end;
      end
      else if LInput is TCheckBox then
      begin

        if LWriteValue then
        begin
          TCheckBox(LInput).Checked := LValue = 'true';
        end;
      end
      else if LInput is TRadioButton then
      begin

        if LWriteValue then
        begin
          TRadioButton(LInput).Checked := LValue = 'true';
        end;
      end
      else if LInput is TTrackBar then
      begin
        LMinimum := StrToIntDef(LNode.Prop('min'), 0);
        LMaximum := StrToIntDef(LNode.Prop('max'), 100);

        if LMinimum > TTrackBar(LInput).Max then
        begin
          TTrackBar(LInput).Max := LMaximum;
        end;
        TTrackBar(LInput).Min := LMinimum;
        TTrackBar(LInput).Max := LMaximum;

        if LWriteValue then
        begin
          TTrackBar(LInput).Position := StrToIntDef(LValue, 0);
        end;
      end;

      if LBinding.FControl is TProgressBar then
      begin
        TProgressBar(LBinding.FControl).Min := StrToIntDef(LNode.Prop('min'), 0);
        TProgressBar(LBinding.FControl).Max := StrToIntDef(LNode.Prop('max'), 100);
        TProgressBar(LBinding.FControl).Position := StrToIntDef(LValue, 0);
      end;

      if Assigned(LBinding.FUpdater) then
      begin
        LBinding.FUpdater(LNode, LBinding.FControl);
      end;
    end;
    SyncRadioFocus;
    Resize(FPanel);
  finally
    FUpdating := False;
  end;
end;

end.
