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
unit nyx.canvas.browser;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, Math, JS, Web, nyx.text, nyx.types, nyx.model, nyx.state,
  nyx.theme, nyx.events, nyx.scheduler, nyx.render.browser,
  nyx.canvas.geometry, nyx.canvas.scene, nyx.canvas.workspace,
  nyx.application.context, nyx.view.behavior, nyx.flow, nyx.publication,
  nyx.flow.declarations, nyx.flow.editing, nyx.flow.routing, nyx.canvas.routes,
  nyx.flow.route.model, nyx.flow.routes, nyx.data, nyx.canvas.observers,
  nyx.programs.declarations, nyx.programs.editing, nyx.logic.programs,
  nyx.canvas.navigation, nyx.canvas.navigation.sources,
  nyx.canvas.precision, nyx.canvas.precision.interaction,
  nyx.resource.context, nyx.application.resources;

type
  { Configure each independently owned item renderer before it mounts DOM.
    The receiver is borrowed, UI-only and must outlive this workspace attachment. }
  TNyxCanvasBrowserRendererSetup = procedure(ARenderer: TNyxBrowserRenderer) of object;

  { Browser presentation capability for a connected workspace. Returned DOM and
    renderers are borrowed until Disconnect; callers never remove/reparent them.
    Off-screen items may have no mounted input. This supplies no file/host grants. }
  INyxBrowserCanvasWorkspaceView = interface(INyxCanvasWorkspaceView)
    ['{737A7921-4621-4C6F-8C01-0CA000000013}']
    function GetSurface: TJSHTMLElement;
    function RendererFor(const AItem: TNyxCanvasItemRef): TNyxBrowserRenderer;
    function InputFor(const AItem: TNyxCanvasItemRef;
      const AControl: TNyxControlRef): TJSHTMLElement;
    { Borrow DOM/renderer only until Disconnect. Surface is a keyboard-accessible
      region; ordinary embedded controls retain their own native browser input. }
    property Surface: TJSHTMLElement read GetSurface;
  end;

  { Optional borrowed ruler faces. X measures across the top, Y down the left.
    Hidden axes return nil. The adapter owns DOM/listeners until Disconnect;
    consumers must never remove, reparent or retain retired native elements. }
  INyxBrowserCanvasRulerPresentation = interface(IInterface)
    ['{2CAB13D6-C881-49DD-AC66-10160A100007}']
    function RulerFor(AAxis: TNyxCanvasAxis): TJSHTMLElement;
  end;

{ Same ownership and live-control scaling contract as the LCL mount. Document/
  theme are copied; host/store/setup receiver are borrowed. An explicit mount
  is required for this runtime composition until document/source integration.
  Optional AResources supplies an existing application broker, its current
  catalog and grouped retained-item updates; the caller owns Start/Stop. Nil
  preserves authored/default or explicit renderer-setup resource behavior.
  In design preview, optional managed AHostInput selects exact host-owned roots
  for control input without attaching application behavior. It is retained until
  Disconnect and queried for each newly mounted lifetime. }
function MountNyxBrowserCanvasWorkspace(const AWorkspace: INyxCanvasWorkspace;
  ADocument: TNyxDocument; AHost: TJSHTMLElement; AState: TNyxState = nil;
  ATheme: TNyxTheme = nil;
  ASetup: TNyxCanvasBrowserRendererSetup = nil;
  ADesignMode: Boolean = False;
  const AContext: INyxApplicationContext = nil;
  const AResolver: INyxLogicResolver = nil;
  const AResources: INyxApplicationResources = nil;
  const AHostInput: INyxCanvasHostInput = nil): INyxBrowserCanvasWorkspaceView;

implementation

uses
  nyx.design.tokens, nyx.interaction.policy, nyx.behavior, nyx.gestures.browser;

type
  { One owned browser item presentation. The scene content is retained, while
    this mount owns its copied context, renderer and behavior attachment. The
    borrowed host DOM is removed only after behavior authority is revoked.
    Uniform scaling adds an owned logical layout host inside the clipped face;
    the renderer and its live inputs are retained across camera changes. }
  TCanvasItemMount = class
  public
    Reference: TNyxCanvasItemRef;
    Content: INyxNode;
    Host: TJSHTMLElement;
    LayoutHost: TJSHTMLElement;
    Document: TNyxDocument;
    Renderer: TNyxBrowserRenderer;
    Behavior: INyxViewBehavior;
    destructor Destroy; override;
  end;

  { View-owned item vector; each class entry has exactly one disposing owner. }
  TCanvasMounts = array of TCanvasItemMount;
  { Detached geometry candidates contain no DOM, scene or behavior references. }
  TCanvasFrames = array of TNyxCanvasControlFrame;

  { Copied Pointer Events contact state in surface coordinates. The pointer ID
    identifies a borrowed browser interaction, never an owned DOM event object. }
  TCanvasContact = record
    ID: NativeInt;
    Start: TNyxCanvasPoint;
    Current: TNyxCanvasPoint;
  end;

  { UI-owned live presentation. It retains copied document/scene context and
    item mounts while borrowing its host/store; no scene control retains it.
    Prepared layout publication stages faces before the scene/camera commit.
    An optional resource port coordinates all retained renderers through the
    existing application broker, owning neither its application nor transport. }
  TCanvasView = class(TInterfacedObject,
    INyxCanvasWorkspaceView, INyxBrowserCanvasWorkspaceView, INyxCanvasAccessibilityView,
    INyxCanvasRuntimeView,
    INyxCanvasLayoutView, INyxCanvasCoordinatedLayoutView, INyxCanvasFlowEditing,
    INyxCanvasProgramEditing,
    INyxCanvasRouteEditing, INyxCanvasPresentationObservation, INyxCanvasControlGeometry,
    INyxCanvasNavigationContributions, INyxCanvasPrecisionView,
    INyxCanvasSourceLayoutView, INyxBrowserCanvasRulerPresentation, INyxCanvasScalingView)
  private
    FWorkspace: INyxCanvasWorkspace;
    FDocument: TNyxDocument;
    { Borrowed only while a source layout stages; its preparation owns it. }
    FPreparingDocument: TNyxDocument;
    FSourcePreparing: Boolean;
    FState: TNyxState;
    FOwnState: Boolean;
    FDesignMode: Boolean;
    FGate: INyxCanvasInteractionGate;
    FTheme: TNyxTheme;
    FSetup: TNyxCanvasBrowserRendererSetup;
    FHostInput: INyxCanvasHostInput;
    FParentContext: INyxApplicationContext;
    FResolver: INyxLogicResolver;
    { Retain the application's loader port, never its application. The caller
      owns Start/Stop; Disconnect revokes only this view's borrowed receiver. }
    FResources: INyxApplicationResources;
    FResourceSubscription: INyxResourceSubscription;
    FChrome: TNyxBrowserRenderer;
    FScheduler: INyxScheduler;
    FSurface: TJSHTMLElement;
    FBackground: TJSHTMLElement;
    { Hidden description belongs to Surface's DOM subtree. Its transient ID is
      unique in the active document; it retains no model/renderer or callback. }
    FDescription: TJSHTMLElement;
    FObserver: TJSHTMLResizeObserver;
    FCamera: TNyxCanvasCamera;
    FGrid: TNyxCanvasGrid;
    FGuideInput: INyxCanvasGuideInteraction;
    FTopRuler: TJSHTMLElement;
    FLeftRuler: TJSHTMLElement;
    FGuideSVG: TJSElement;
    FGuideCapture: TJSHTMLElement;
    FGuidePointer: NativeInt;
    FGuideCapturing: Boolean;
    FLastRulerCamera: TNyxCanvasCamera;
    FLastRulerData: TNyxText;
    FScaling: TNyxCanvasControlScaling;
    FSnapshot: INyxCanvasSnapshot;
    FMounts: TCanvasMounts;
    FConnected: Boolean;
    { Monotonic successful presentation identity, independent of scene revision. }
    FPresentationRevision: Integer;
    FPresentationObservers: TNyxCanvasObservers;
    FNavigationSources: TNyxCanvasNavigationSources;
    FNavigating: Boolean;
    FFlowRevision: Integer;
    FRouteOverrides: TNyxCanvasRouteOverrides;
    FConstructing: Boolean;
    { A pas2js factory owns Self only after constructor staging has finished. }
    FUpdating: Boolean;
    FDisconnectPending: Boolean;
    FContacts: array[0..1] of TCanvasContact;
    FContactCount: Integer;
    FStartCamera: TNyxCanvasCamera;
    FGestureCamera: TNyxCanvasCamera;
    function ResourcesReady(const AContext: INyxResourceContext): Boolean;
    function PrepareResources(const AContext: INyxResourceContext;
      out APrepared: INyxPreparedPublication): Boolean;
    procedure ApplyBehaviorChanges(const AFlows: array of TNyxFlowChange;
      const APrograms: array of TNyxProgramChange; AExpectedScene, AExpectedRevision: Integer);
    function DoFitNavigation(const AOptions: TNyxCanvasNavigationOptions;
      ALegacyFallback: Boolean): TNyxCanvasNavigationResult;
    function FindMount(const AItem: TNyxCanvasItemRef): Integer;
    function BuildRouteContext(ASource: TNyxDocument; const AItem: TNyxCanvasItemRef;
      AContent: TNyxNode; AOverrides: TNyxCanvasRouteOverrides;
      out AOwnedRoot: TNyxNode): TNyxDocument;
    function PrepareMount(const AItem: TNyxCanvasItemRef;
      const AContent: INyxNode; const AFrame: TNyxCanvasControlFrame): TCanvasItemMount;
    function Present(const ACamera: TNyxCanvasCamera; ARefresh: Boolean;
      const APlan: INyxCanvasLayoutPlan = nil; AExpectedPresentation: Integer = -1;
      const ACancellation: INyxCancellationScope = nil;
      const ACompanion: INyxPreparedPublication = nil): Boolean;
    procedure ChromeEvent(ANode: TNyxNode; const AEvent: TNyxEventInfo);
    procedure BoundsChanged(AEntries: TJSHTMLResizeObserverEntryArray;
      AObserver: TJSHTMLResizeObserver);
    function LocalPoint(AX, AY: Double): TNyxCanvasPoint;
    function Down(AEvent: TJSPointerEvent): Boolean;
    function Move(AEvent: TJSPointerEvent): Boolean;
    function Up(AEvent: TJSPointerEvent): Boolean;
    function Cancel(AEvent: TJSPointerEvent): Boolean;
    function Wheel(AEvent: TJSWheelEvent): Boolean;
    function Keyboard(AEvent: TJSKeyboardEvent): Boolean;
    procedure CancelPan;
    procedure RequireConnected;
    procedure PaintGrid;
    procedure PaintPrecision;
    procedure NotifyPrecision;
    procedure RequirePrecisionRevision;
    procedure CancelGuideGesture;
    procedure AppendPrecisionOccluders(const ACamera: TNyxCanvasCamera;
      var AFrame: TNyxCanvasNavigationFrame);
    function BuildRulerSVG(AAxis: TNyxCanvasAxis): TJSElement;
    function GuidePoint(AX, AY: Double): TNyxCanvasPoint;
    function RulerDown(AEvent: TJSPointerEvent): Boolean;
    function RulerKey(AEvent: TJSKeyboardEvent): Boolean;
    function GuideKey(AEvent: TJSKeyboardEvent): Boolean;
  public
    constructor Create(const AWorkspace: INyxCanvasWorkspace;
      ADocument: TNyxDocument; AHost: TJSHTMLElement; AState: TNyxState;
      ATheme: TNyxTheme; ASetup: TNyxCanvasBrowserRendererSetup; ADesignMode: Boolean;
      const AContext: INyxApplicationContext; const AResolver: INyxLogicResolver;
      const AResources: INyxApplicationResources; const AHostInput: INyxCanvasHostInput);
    destructor Destroy; override;
    function GetSurface: TJSHTMLElement;
    function RulerFor(AAxis: TNyxCanvasAxis): TJSHTMLElement;
    function GetPrecision: TNyxCanvasPrecisionOptions;
    procedure SetPrecision(const AOptions: TNyxCanvasPrecisionOptions);
    function GetSelectedGuide: TNyxCanvasGuideRef;
    procedure SelectGuide(const AReference: TNyxCanvasGuideRef);
    function SnapGuidePoint(const APoint: TNyxCanvasPoint;
      const AExclude: TNyxCanvasGuideRef): TNyxCanvasGuideSnap;
    function GetConnected: Boolean;
    function GetControlScaling: TNyxCanvasControlScaling;
    function GetCamera: TNyxCanvasCamera;
    function GetMountedCount: Integer;
    function GetPresentationRevision: Integer;
    function ObservePresentation(const AObserver: INyxCanvasPresentationObserver): INyxCancellationScope;
    function ObserveNavigation(const ASource: INyxCanvasNavigationSource): INyxCancellationScope;
    function FitNavigation(const AOptions: TNyxCanvasNavigationOptions): TNyxCanvasNavigationResult;
    function TryControlBounds(const AItem: TNyxCanvasItemRef;
      const AControl: TNyxControlRef; out ABounds: TNyxCanvasBounds): Boolean;
    function ApplyLayout(const APlan: INyxCanvasLayoutPlan;
      const ACamera: TNyxCanvasCamera; AExpectedPresentation: Integer;
      const ACancellation: INyxCancellationScope = nil): Boolean;
    procedure SetCamera(const ACamera: TNyxCanvasCamera);
    function ApplyCoordinatedLayout(const APlan: INyxCanvasLayoutPlan;
      const ACamera: TNyxCanvasCamera; AExpectedPresentation: Integer;
      const ACompanion: INyxPreparedPublication;
      const ACancellation: INyxCancellationScope = nil): Boolean;
    function ApplySourceLayout(ADocument: TNyxDocument;
      const APlan: INyxCanvasLayoutPlan; const ACamera: TNyxCanvasCamera;
      AExpectedPresentation: Integer; const ACompanion: INyxPreparedPublication = nil;
      const ACancellation: INyxCancellationScope = nil): Boolean;
    function RuntimeFlows: INyxFlowDeclarations;
    function GetFlowRevision: Integer;
    function RuntimePrograms: INyxProgramDeclarations;
    function GetProgramRevision: Integer;
    procedure ApplyProgramChanges(const AChanges: array of TNyxProgramChange;
      AExpectedScene, AExpectedPrograms: Integer);
    procedure ApplyFlowChanges(const AChanges: array of TNyxFlowChange;
      AExpectedScene, AExpectedFlows: Integer);
    function RouteContext(const AItem: TNyxCanvasItemRef;
      out AOwnedRoot: TNyxNode): TNyxDocument;
    function CaptureRoutes: TNyxCanvasRouteChanges;
    function GetRouteRevision: Integer;
    procedure ApplyRouteChanges(const AChanges: array of TNyxCanvasRouteChange;
      AExpectedScene, AExpectedRoutes: Integer);
    procedure PanBy(AX, AY: Double);
    procedure ZoomAt(AZoom: Double; const AAnchor: TNyxCanvasPoint);
    procedure Execute(ACommand: TNyxCanvasCommand);
    procedure Refresh;
    procedure Constrain(AEnabled, AReadOnly: Boolean);
    procedure SetAccessibleMetadata(const AName, ADescription: TNyxText);
    function EventsFor(const AItem: TNyxCanvasItemRef): INyxEvents;
    function BehaviorFor(const AItem: TNyxCanvasItemRef): INyxViewBehavior;
    function GetScene: INyxCanvasScene;
    function RendererFor(const AItem: TNyxCanvasItemRef): TNyxBrowserRenderer;
    function InputFor(const AItem: TNyxCanvasItemRef;
      const AControl: TNyxControlRef): TJSHTMLElement;
    procedure Disconnect;
  end;

{$I nyx.canvas.publication.inc}
{$I nyx.canvas.source.context.inc}
{$I nyx.canvas.flows.inc}
{$I nyx.canvas.routes.inc}

var
  { UI-realm sequence holds no project identity or host reference. Never reuse
    retired IDs, and skip collisions with caller-owned DOM before attachment. }
  GCanvasDescriptionSequence: Integer;

function NewCanvasDescriptionID: TNyxText;
const
  CPrefix: TNyxText = 'nyx-canvas-description-';
begin
  repeat

    if GCanvasDescriptionSequence = High(Integer) then
    begin
      raise ENyxCanvasScene.Create('Canvas description ID capacity exhausted');
    end;
    Inc(GCanvasDescriptionSequence);
    Result := CPrefix + TNyxText(IntToStr(GCanvasDescriptionSequence));
  until document.getElementById(Result) = nil;
end;

function Metric(AValue: Double): TNyxText;
var
  LFormat: TFormatSettings;
begin
  LFormat := FormatSettings;
  LFormat.DecimalSeparator := '.';
  Result := FloatToStr(AValue, LFormat) + 'px';
end;

procedure SetVisible(AHost: TJSHTMLElement; AVisible: Boolean);
begin

  if AVisible then
  begin
    AHost.style.setProperty('display', 'block');
  end
  else
  begin
    AHost.style.setProperty('display', 'none');
  end;
end;

procedure SetFrame(AHost: TJSHTMLElement; const AFrame: TNyxCanvasControlFrame);
begin
  AHost.style.setProperty('left', IntToStr(AFrame.Left) + 'px');
  AHost.style.setProperty('top', IntToStr(AFrame.Top) + 'px');
  AHost.style.setProperty('width', IntToStr(AFrame.Width) + 'px');
  AHost.style.setProperty('height', IntToStr(AFrame.Height) + 'px');
end;

procedure SetLayoutFrame(AMount: TCanvasItemMount;
  const AFrame: TNyxCanvasControlFrame);
var
  LFormat: TFormatSettings;
begin

  if AMount.LayoutHost = AMount.Host then
  begin
    Exit;
  end;
  LFormat := FormatSettings;
  LFormat.DecimalSeparator := '.';
  AMount.LayoutHost.style.setProperty('width', IntToStr(AFrame.LayoutWidth) + 'px');
  AMount.LayoutHost.style.setProperty('height', IntToStr(AFrame.LayoutHeight) + 'px');
  AMount.LayoutHost.style.setProperty('transform',
    'scale(' + FloatToStr(AFrame.ContentScale, LFormat) + ')');
end;

destructor TCanvasItemMount.Destroy;
begin

  if Behavior <> nil then
  begin
    Behavior.Close;
    Behavior := nil;
  end;
  Renderer.Free;
  Document.Free;

  if Host <> nil then
  begin
    Host.remove;
  end;
  Content := nil;
  inherited Destroy;
end;

constructor TCanvasView.Create(const AWorkspace: INyxCanvasWorkspace;
  ADocument: TNyxDocument; AHost: TJSHTMLElement; AState: TNyxState;
  ATheme: TNyxTheme; ASetup: TNyxCanvasBrowserRendererSetup; ADesignMode: Boolean;
  const AContext: INyxApplicationContext; const AResolver: INyxLogicResolver;
  const AResources: INyxApplicationResources; const AHostInput: INyxCanvasHostInput);
var
  LHost: TJSHTMLElement;
  LRoot: TNyxNode;
  LPrecision: INyxCanvasWorkspacePrecision;
  LOptions: TNyxCanvasPrecisionOptions;
begin
  inherited Create;
  FRouteOverrides := TNyxCanvasRouteOverrides.Create;
  FPresentationObservers := TNyxCanvasObservers.Create;
  FConstructing := True;

  if (AWorkspace = nil) or (ADocument = nil) or (AHost = nil) then
  begin
    raise ENyxCanvasScene.Create('Browser workspace mount requires workspace, document and host');
  end;
  FWorkspace := AWorkspace;
  FParentContext := AContext;
  FResolver := AResolver;
  FResources := AResources;
  FDocument := NewNyxCanvasRenderContext(ADocument, AWorkspace.Node, LRoot);
  FTheme := NewNyxDocumentTheme(FDocument, ATheme);
  FState := AState;
  FOwnState := (AState = nil) and (FParentContext = nil);

  if FParentContext <> nil then
  begin

    if not FParentContext.Active then
    begin
      raise ENyxApplicationContext.Create('Canvas behavior requires active application authority');
    end;

    if (FState <> nil) and (FState <> FParentContext.State) then
    begin
      raise ENyxApplicationContext.Create('Canvas controls and behavior must share the exact runtime store');
    end;
    FState := FParentContext.State;
  end;

  if FOwnState then
  begin
    FState := FDocument.State.Clone;
  end;
  FSetup := ASetup;
  FHostInput := AHostInput;
  FDesignMode := ADesignMode;
  FGate := NewNyxCanvasInteractionGate;
  FGrid := AWorkspace.Grid;
  LOptions := Default(TNyxCanvasPrecisionOptions);

  if Supports(AWorkspace, INyxCanvasWorkspacePrecision, LPrecision) then
  begin
    LOptions := LPrecision.Precision;
  end;
  FGuideInput := NewNyxCanvasGuideInteraction(LOptions);
  FScaling := AWorkspace.Scaling;
  FChrome := TNyxBrowserRenderer.Create(FTheme);
  FScheduler := FChrome.Events.Scheduler;
  FNavigationSources := TNyxCanvasNavigationSources.Create(FScheduler);
  FScheduler.RequireUI;
  FChrome.OnEvent := @ChromeEvent;
  FChrome.ResourceUpdates := FResources;
  FChrome.Render(FDocument, LRoot, AHost, False, FState);
  FChrome.Root.AttachEnabledPolicy(FGate.EnabledPolicy);
  LHost := FChrome.ElementFor(NyxCanvasWorkspacePart(AWorkspace.ID, ncwpSurface).ID);
  LHost.style.setProperty('position', 'relative');
  FSurface := TJSHTMLElement(document.createElement('div'));
  FSurface.style.setProperty('position', 'absolute');
  FSurface.style.setProperty('inset', '0');
  FSurface.style.setProperty('overflow', 'hidden');
  FSurface.style.setProperty('touch-action', 'auto');
  FSurface.style.setProperty('outline-offset', '-3px');
  FSurface.tabIndex := 0;
  FSurface.setAttribute('role', 'region');
  FSurface.setAttribute('aria-label', NyxDefaultCanvasAccessibleName);
  LHost.appendChild(FSurface);
  { touch-action intersects ancestor policies. Put none on the background
    sibling only; applying it to the common surface would suppress normal
    touch scrolling and selection inside embedded native browser controls. }
  FBackground := TJSHTMLElement(document.createElement('div'));
  FBackground.style.setProperty('position', 'absolute');
  FBackground.style.setProperty('inset', '0');
  FBackground.style.setProperty('touch-action', 'none');
  FBackground.setAttribute('aria-hidden', 'true');
  FSurface.appendChild(FBackground);
  { Referenced hidden text keeps help out of the concise name and preserves the
    caller's normal tooltip. Background remains the first child/input plane. }
  FDescription := TJSHTMLElement(document.createElement('span'));
  FDescription.id := NewCanvasDescriptionID;
  FDescription.setAttribute('hidden', '');
  FDescription.textContent := NyxCanvasKeyboardHelp;
  FSurface.appendChild(FDescription);
  FSurface.setAttribute('aria-describedby', FDescription.id);
  FTopRuler := TJSHTMLElement(document.createElement('div'));
  FLeftRuler := TJSHTMLElement(document.createElement('div'));
  FSurface.appendChild(FTopRuler);
  FSurface.appendChild(FLeftRuler);
  FTopRuler.addEventListener('pointerdown', @RulerDown);
  FLeftRuler.addEventListener('pointerdown', @RulerDown);
  FTopRuler.addEventListener('keydown', @RulerKey);
  FLeftRuler.addEventListener('keydown', @RulerKey);
  FSurface.addEventListener('pointerdown', @Down);
  FSurface.addEventListener('pointermove', @Move);
  FSurface.addEventListener('pointerup', @Up);
  FSurface.addEventListener('pointercancel', @Cancel);
  FSurface.addEventListener('lostpointercapture', @Cancel);
  FSurface.addEventListener('wheel', @Wheel);
  FSurface.addEventListener('keydown', @Keyboard);
  FCamera := AWorkspace.Camera;
  FConnected := True;
  Refresh;
  FObserver := TJSHTMLResizeObserver.new(@BoundsChanged);
  FObserver.observe(FSurface);
  FConstructing := False;

  if FResources <> nil then
  begin
    FResourceSubscription := NyxPreparedApplicationResources(FResources).
      SubscribePrepared(@ResourcesReady, @PrepareResources);
  end;
end;

destructor TCanvasView.Destroy;
begin
  Disconnect;
  FDocument.Free;
  FRouteOverrides.Free;
  FPresentationObservers.Free;
  FNavigationSources.Free;

  if FOwnState then
  begin
    FState.Free;
  end;
  FTheme.Free;
  FWorkspace := nil;
  inherited Destroy;
end;

function MountNyxBrowserCanvasWorkspace(const AWorkspace: INyxCanvasWorkspace;
  ADocument: TNyxDocument; AHost: TJSHTMLElement; AState: TNyxState;
  ATheme: TNyxTheme; ASetup: TNyxCanvasBrowserRendererSetup;
  ADesignMode: Boolean; const AContext: INyxApplicationContext;
  const AResolver: INyxLogicResolver;
  const AResources: INyxApplicationResources;
  const AHostInput: INyxCanvasHostInput): INyxBrowserCanvasWorkspaceView;
begin
  Result := TCanvasView.Create(AWorkspace, ADocument, AHost, AState, ATheme, ASetup,
    ADesignMode, AContext, AResolver, AResources, AHostInput);
end;

procedure TCanvasView.RequireConnected;
begin
  FScheduler.RequireUI;

  if not FConnected then
  begin
    raise ENyxCanvasScene.Create('Browser workspace view is disconnected');
  end;
end;

procedure TCanvasView.SetAccessibleMetadata(const AName, ADescription: TNyxText);
begin
  RequireConnected;

  if FSurface.getAttribute('aria-label') <> AName then
  begin
    FSurface.setAttribute('aria-label', AName);
  end;

  if FDescription.textContent <> ADescription then
  begin
    FDescription.textContent := ADescription;
  end;
end;

function TCanvasView.FindMount(const AItem: TNyxCanvasItemRef): Integer;
var
  LName: TNyxText;
begin
  LName := AItem.Name;
  for Result := 0 to High(FMounts) do
  begin

    if FMounts[Result].Reference.Name = LName then
    begin
      Exit;
    end;
  end;
  Result := -1;
end;

function TCanvasView.PrepareMount(const AItem: TNyxCanvasItemRef;
  const AContent: INyxNode; const AFrame: TNyxCanvasControlFrame): TCanvasItemMount;
var
  LRoot: TNyxNode;
  LSource: TNyxDocument;
  LDesignMode: Boolean;
begin
  Result := TCanvasItemMount.Create;
  try
    Result.Reference := AItem;
    Result.Content := AContent;
    Result.Host := TJSHTMLElement(document.createElement('div'));
    Result.Host.style.setProperty('position', 'absolute');
    Result.Host.style.setProperty('overflow', 'hidden');
    Result.Host.style.setProperty('box-sizing', 'border-box');
    { Hidden staging still needs an actual logical allocation. display:none
      would make the normal renderer measure a zero-width host. }
    Result.Host.style.setProperty('visibility', 'hidden');
    SetFrame(Result.Host, AFrame);
    Result.LayoutHost := Result.Host;

    if FScaling = nccsUniform then
    begin
      Result.LayoutHost := TJSHTMLElement(document.createElement('div'));
      Result.LayoutHost.style.setProperty('position', 'absolute');
      Result.LayoutHost.style.setProperty('left', '0px');
      Result.LayoutHost.style.setProperty('top', '0px');
      Result.LayoutHost.style.setProperty('transform-origin', '0 0');
      Result.Host.appendChild(Result.LayoutHost);
      SetLayoutFrame(Result, AFrame);
    end;
    FSurface.appendChild(Result.Host);
    Result.Renderer := TNyxBrowserRenderer.Create(FTheme);

    if Assigned(FSetup) then
    begin
      FSetup(Result.Renderer);
    end;

    if FResources <> nil then
    begin
      Result.Renderer.ResourceUpdates := FResources;
    end;
    LSource := FDocument;

    if FPreparingDocument <> nil then
    begin
      LSource := FPreparingDocument;
    end;
    Result.Document := BuildRouteContext(LSource, AItem, AContent.Node, FRouteOverrides, LRoot);
    { Design mode shows authored controls without executing application code.
      Each runtime item receives its own revocable subscriptions/generation. }

    if not FDesignMode and (FParentContext <> nil) then
    begin
      Result.Behavior := BindNyxViewBehavior(Result.Document, LRoot,
        Result.Renderer.Events, FParentContext, FResolver);
      Result.Renderer.ConfigureEmbeddedBehavior(Result.Behavior.Context, FResolver);
    end
    else if not FDesignMode and NyxViewRequiresBehavior(Result.Document, LRoot) then
    begin
      raise ENyxApplicationContext.Create('Embedded executable behavior requires explicit application context');
    end;
    LDesignMode := FDesignMode;

    if FDesignMode and (FHostInput <> nil) and FHostInput.AllowsInput(AItem, AContent) then
    begin
      LDesignMode := False;
    end;
    { This local input grant never changes the application-behavior gate above.
      Other source content retains its normal design-preview renderer policy. }
    Result.Renderer.Render(Result.Document, LRoot, Result.LayoutHost, LDesignMode, FState);
    Result.Renderer.Root.AttachEnabledPolicy(FGate.EnabledPolicy);
    Result.Renderer.Root.AttachReadOnlyPolicy(FGate.ReadOnlyPolicy);
    Result.Renderer.Sync;
  except
    Result.Free;
    raise;
  end;
end;

{$I nyx.canvas.resources.inc}

function TCanvasView.Present(const ACamera: TNyxCanvasCamera; ARefresh: Boolean;
  const APlan: INyxCanvasLayoutPlan; AExpectedPresentation: Integer;
  const ACancellation: INyxCancellationScope;
  const ACompanion: INyxPreparedPublication): Boolean;
var
  LKeepAlive: INyxCanvasWorkspaceView;
  LCamera: TNyxCanvasCamera;
  LSnapshot: INyxCanvasSnapshot;
  LFrames: TCanvasFrames;
  LMounts: TCanvasMounts;
  LAccepted: TCanvasMounts;
  LFormerMounts: TCanvasMounts;
  LCount: Integer;
  LNew: array of Boolean;
  LRetained: array of Boolean;
  LItem: TNyxCanvasItemInfo;
  LContent: INyxNode;
  LIndex: Integer;
  LFormer: Integer;
  LPublished: Boolean;
  LKeepFocus: Boolean;
  LScenePublication: INyxPreparedPublication;
  LSceneReady: INyxPublicationReadiness;
  LContentPlan: INyxCanvasContentPlan;
  LPresentation: TCanvasPresentation;
  LPresentationLease: INyxPreparedPublication;
  LNotificationFailed: Boolean;
  LReceiverMessage: TNyxText;
begin
  Result := False;
  RequireConnected;

  if (FGuideInput <> nil) and FGuideInput.Active then
  begin
    raise ENyxCanvasPrecision.Create('Finish or cancel the guide gesture before replacing its presentation');
  end;

  if FPresentationRevision = High(Integer) then
  begin
    raise ENyxCanvasScene.Create('Workspace presentation revision exhausted');
  end;

  if FUpdating then
  begin
    raise ENyxCanvasScene.Create('Browser workspace presentation is already updating');
  end;

  if (FSurface.clientWidth <= 0) or (FSurface.clientHeight <= 0) then
  begin
    Exit;
  end;

  if not FConstructing then
  begin
    LKeepAlive := Self;
  end;
  LCamera := ACamera.WithViewport(FSurface.clientWidth, FSurface.clientHeight);
  LSnapshot := FSnapshot;

  if APlan <> nil then
  begin

    if (AExpectedPresentation <> FPresentationRevision) or APlan.Committed or
      not APlan.BelongsTo(FWorkspace.Scene) then
    begin
      raise ENyxCanvasScene.Create('A layout requires its exact current scene and presentation');
    end;
    LSnapshot := APlan.Preview;

    if LSnapshot = nil then
    begin
      raise ENyxCanvasScene.Create('A layout requires a complete immutable preview');
    end;
  end
  else if ARefresh or (LSnapshot = nil) then
  begin
    LSnapshot := FWorkspace.Scene.Snapshot;
  end;
  SetLength(LFrames, LSnapshot.Count);
  SetLength(LMounts, LSnapshot.Count);
  SetLength(LAccepted, LSnapshot.Count);
  SetLength(LNew, LSnapshot.Count);
  SetLength(LRetained, Length(FMounts));
  LPublished := False;
  LNotificationFailed := False;
  LContentPlan := nil;

  if APlan <> nil then
  begin
    Supports(APlan, INyxCanvasContentPlan, LContentPlan);
  end;
  FUpdating := True;
  try
    { Admit every live allocation before changing the camera or any accepted
      face. Only newly needed/replaced mounts stage hidden physical controls. }
    for LIndex := 0 to LSnapshot.Count - 1 do
    begin
      LItem := LSnapshot.Items[LIndex];
      LFrames[LIndex] := NyxCanvasControlFrame(LCamera, LItem.Placement.Bounds,
        FScaling, LItem.Placement.IsVisible);
    end;
    for LIndex := 0 to LSnapshot.Count - 1 do
    begin
      LItem := LSnapshot.Items[LIndex];
      LFormer := FindMount(LItem.Item);

      if LContentPlan <> nil then
      begin
        LContent := LContentPlan.PreviewControl(LItem.Item);
      end
      else
      begin
        LContent := FWorkspace.Scene.Control(LItem.Item);
      end;

      if (LFormer >= 0) and (FMounts[LFormer].Content.Node = LContent.Node) then
      begin
        LMounts[LIndex] := FMounts[LFormer];
        LRetained[LFormer] := True;
      end
      else if LFrames[LIndex].Visible then
      begin
        LMounts[LIndex] := PrepareMount(LItem.Item, LContent, LFrames[LIndex]);
        LNew[LIndex] := True;
      end;
    end;
    { Stage the final mount vector before committing scene and camera. }
    LCount := 0;
    for LIndex := 0 to High(LMounts) do
    begin

      if LMounts[LIndex] <> nil then
      begin
        LAccepted[LCount] := LMounts[LIndex];
        Inc(LCount);
      end;
    end;
    SetLength(LAccepted, LCount);

    if (ACancellation <> nil) and ACancellation.Cancelled then
    begin
      Exit;
    end;

    if not FConnected then
    begin
      Exit;
    end;

    if APlan <> nil then
    begin

      if (AExpectedPresentation <> FPresentationRevision) or
        not APlan.BelongsTo(FWorkspace.Scene) then
      begin
        raise ENyxCanvasScene.Create('Workspace changed during layout staging');
      end;

      if not FConnected then
      begin
        Exit;
      end;

      if ACompanion = nil then
      begin
        APlan.Commit;
      end;
    end;
    LFormerMounts := FMounts;

    if ACompanion <> nil then
    begin

      if not Supports(APlan, INyxPreparedPublication, LScenePublication) or
        not Supports(APlan, INyxPublicationReadiness, LSceneReady) then
      begin
        raise ENyxCanvasScene.Create('Scene plan cannot participate in coordinated publication');
      end;
      LPresentation := TCanvasPresentation.Create;
      LPresentationLease := LPresentation;
      LPresentation.Owner := Self;
      LPresentation.Camera := LCamera;
      LPresentation.Snapshot := LSnapshot;
      LPresentation.FormerSnapshot := FSnapshot;
      LPresentation.Mounts := LAccepted;
      LPresentation.Scope := ACancellation;
      LPresentation.ExpectedRevision := AExpectedPresentation;
      try
        try
          PublishNyxGroup([ACompanion, LScenePublication, LPresentationLease]);
        except
          on E: ENyxPublicationNotification do
          begin
            { A validator may raise any exception type. Only actual installation
              grants post-publication reconciliation; the exception name cannot. }

            if not LPresentation.Installed then
            begin
              raise;
            end;
            LNotificationFailed := True;
            LReceiverMessage := E.ReceiverMessage;
          end;
        end;
      finally
        LPublished := LPresentation.Installed;
      end;
    end
    else
    begin
      FMounts := LAccepted;
      FSnapshot := LSnapshot;
      FCamera := LCamera;
      Inc(FPresentationRevision);
      LPublished := True;
    end;
    Result := True;
    try
      { Retire old faces after complete model publication. External callbacks or
        failures here have post-publication semantics, not fabricated rollback. }
      for LIndex := 0 to High(LFormerMounts) do
      begin

        if not LRetained[LIndex] then
        begin
          LFormerMounts[LIndex].Free;
        end;
      end;

      if not FConnected then
      begin
        Exit;
      end;
      for LIndex := 0 to High(LMounts) do
      begin

        if LMounts[LIndex] = nil then
        begin
          Continue;
        end;

        if LFrames[LIndex].Visible then
        begin
          SetFrame(LMounts[LIndex].Host, LFrames[LIndex]);
          SetLayoutFrame(LMounts[LIndex], LFrames[LIndex]);

          if not FConnected then
          begin
            Exit;
          end;
          LMounts[LIndex].Host.style.setProperty('z-index', IntToStr(LIndex));
          LMounts[LIndex].Host.style.setProperty('visibility', 'visible');
        end;
        LKeepFocus := LSnapshot.Items[LIndex].Placement.IsVisible and
          LMounts[LIndex].Host.contains(document.activeElement);

        if not LFrames[LIndex].Visible and LKeepFocus then
        begin
          LMounts[LIndex].Host.style.setProperty('left',
            IntToStr(-MaximumNyxCanvasControlExtent) + 'px');
        end;
        SetVisible(LMounts[LIndex].Host, LFrames[LIndex].Visible or LKeepFocus);

        if not FConnected then
        begin
          Exit;
        end;
      end;
      PaintGrid;
      FPresentationObservers.Notify(FPresentationRevision);
    finally

      if LNotificationFailed then
      begin
        raise ENyxPublicationNotification.CreateReceiverFailure(LReceiverMessage);
      end;
    end;
  finally

    if not LPublished then
    begin
      for LIndex := 0 to High(LMounts) do
      begin

        if LNew[LIndex] then
        begin
          LMounts[LIndex].Free;
        end;
      end;
    end;
    FUpdating := False;

    if FConnected and (FResources <> nil) then
    begin
      FResources.Wake;
    end;

    if FDisconnectPending then
    begin
      Disconnect;
    end;
  end;
end;


procedure TCanvasView.BoundsChanged(AEntries: TJSHTMLResizeObserverEntryArray;
  AObserver: TJSHTMLResizeObserver);
begin

  if FConnected and not FUpdating then
  begin
    CancelGuideGesture;
    Present(FCamera, False);
  end;
end;

procedure TCanvasView.PaintGrid;
var
  LSpacing: Double;
  LX: Double;
  LY: Double;
begin
  FSurface.style.setProperty('background-color', FTheme.Background);
  FSurface.style.setProperty('background-image', 'none');
  PaintPrecision;

  if FGrid.Style = ncgsNone then
  begin
    Exit;
  end;
  LSpacing := FGrid.VisibleSpacing(FCamera) * FCamera.Zoom;
  LX := FCamera.ViewportWidth / 2 +
    (FGrid.Origin.X - FCamera.WorldCenter.X) * FCamera.Zoom;
  LY := FCamera.ViewportHeight / 2 +
    (FGrid.Origin.Y - FCamera.WorldCenter.Y) * FCamera.Zoom;
  LX := LX - Floor(LX / LSpacing) * LSpacing;
  LY := LY - Floor(LY / LSpacing) * LSpacing;

  if FGrid.Style = ncgsLines then
  begin
    FSurface.style.setProperty('background-image',
      'linear-gradient(to right, ' + FTheme.Border + ' 1px, transparent 1px),' +
      'linear-gradient(to bottom, ' + FTheme.Border + ' 1px, transparent 1px)');
  end
  else
  begin
    FSurface.style.setProperty('background-image',
      'radial-gradient(circle at 1px 1px, ' + FTheme.Border + ' 1px, transparent 1.5px)');
  end;
  FSurface.style.setProperty('background-size', Metric(LSpacing) + ' ' + Metric(LSpacing));
  FSurface.style.setProperty('background-position', Metric(LX) + ' ' + Metric(LY));
end;

procedure TCanvasView.ChromeEvent(ANode: TNyxNode; const AEvent: TNyxEventInfo);
var
  LCommand: TNyxCanvasCommand;
begin

  if FConnected and (AEvent.Trigger = ntClick) and
    TryNyxCanvasCommand(FWorkspace.ID, ANode.SourceID, LCommand) then
  begin
    Execute(LCommand);
  end;
end;

function TCanvasView.LocalPoint(AX, AY: Double): TNyxCanvasPoint;
var
  LBounds: TJSDOMRect;
begin
  LBounds := FSurface.getBoundingClientRect;

  if (LBounds.width <= 0) or (LBounds.height <= 0) then
  begin
    raise ENyxCanvasScene.Create('Canvas pointer mapping requires a visible allocation');
  end;
  Result := NyxCanvasPoint((AX - LBounds.left) * FSurface.clientWidth / LBounds.width,
    (AY - LBounds.top) * FSurface.clientHeight / LBounds.height);
end;

function TCanvasView.Down(AEvent: TJSPointerEvent): Boolean;
var
  LKeepAlive: INyxCanvasWorkspaceView;
  LIndex: Integer;
  LGuide: TNyxCanvasGuideRef;
begin
  Result := True;

  if not FConnected or not FGate.EnabledPolicy.Allowed or
    ((AEvent.target <> FSurface) and (AEvent.target <> FBackground)) or
    not (AEvent.button in [0, 1]) or (FContactCount = 2) then
  begin
    Exit;
  end;
  LKeepAlive := Self;

  if FGuideCapturing then
  begin
    Exit;
  end;

  if not FDesignMode and (AEvent.button = 0) then
  begin
    LGuide := FGuideInput.Hit(GuidePoint(AEvent.clientX, AEvent.clientY), FCamera);

    if LGuide.Defined then
    begin
      RequirePrecisionRevision;
      FGuideInput.Select(LGuide);

      if not FGate.ReadOnlyPolicy.ReadOnly and not FGuideInput.Accepted.Named(LGuide).IsLocked then
      begin
        FGuideInput.BeginMove(LGuide);
        FGuideCapture := FSurface;
        FGuidePointer := AEvent.pointerId;
        FGuideCapturing := True;
      end;
      FSurface.focus;

      if not FConnected then
      begin
        Exit;
      end;

      if FGuideCapturing and AEvent.isTrusted then
      begin
        FGuideCapture.setPointerCapture(FGuidePointer);
      end;
      AEvent.preventDefault;
      NotifyPrecision;
      Exit;
    end;
  end;
  for LIndex := 0 to FContactCount - 1 do
  begin

    if FContacts[LIndex].ID = AEvent.pointerId then
    begin
      Exit;
    end;
  end;

  if FContactCount = 0 then
  begin
    FGestureCamera := FCamera;
  end;
  FContacts[FContactCount].ID := AEvent.pointerId;
  FContacts[FContactCount].Current := LocalPoint(AEvent.clientX, AEvent.clientY);
  Inc(FContactCount);
  FStartCamera := FCamera;
  for LIndex := 0 to FContactCount - 1 do
  begin
    FContacts[LIndex].Start := FContacts[LIndex].Current;
  end;
  FSurface.focus;

  if not FConnected then
  begin
    Exit;
  end;
  { Synthetic qualification events have no active hardware pointer to capture.
    Trusted active input uses the platform capture contract directly. }

  if AEvent.isTrusted then
  begin
    FSurface.setPointerCapture(AEvent.pointerId);
  end;
  AEvent.preventDefault;
end;

function TCanvasView.Move(AEvent: TJSPointerEvent): Boolean;
var
  LIndex: Integer;
begin
  Result := True;

  if not FConnected then
  begin
    Exit;
  end;

  if FGuideCapturing then
  begin

    if AEvent.pointerId = FGuidePointer then
    begin
      FGuideInput.Move(GuidePoint(AEvent.clientX, AEvent.clientY), FCamera, FGrid);
      PaintPrecision;
      AEvent.preventDefault;
    end;
    Exit;
  end;
  for LIndex := 0 to FContactCount - 1 do
  begin

    if FContacts[LIndex].ID = AEvent.pointerId then
    begin
      FContacts[LIndex].Current := LocalPoint(AEvent.clientX, AEvent.clientY);

      if FContactCount = 1 then
      begin
        SetCamera(FStartCamera.PanBy(
          FContacts[0].Current.X - FContacts[0].Start.X,
          FContacts[0].Current.Y - FContacts[0].Start.Y));
      end
      else if Sqr(FContacts[0].Current.X - FContacts[1].Current.X) +
        Sqr(FContacts[0].Current.Y - FContacts[1].Current.Y) > 0 then
      begin
        SetCamera(FStartCamera.Pinch(FContacts[0].Start, FContacts[1].Start,
          FContacts[0].Current, FContacts[1].Current));
      end;
      AEvent.preventDefault;
      Exit;
    end;
  end;
end;

function TCanvasView.Up(AEvent: TJSPointerEvent): Boolean;
var
  LIndex: Integer;
  LLease: INyxCanvasWorkspaceView;
  LCapture: TJSHTMLElement;
  LChanged: Boolean;
begin
  Result := True;

  if not FConnected then
  begin
    Exit;
  end;

  if FGuideCapturing then
  begin

    if AEvent.pointerId <> FGuidePointer then
    begin
      Exit;
    end;
    LLease := Self;
    try
      RequirePrecisionRevision;
      FGuideInput.Move(GuidePoint(AEvent.clientX, AEvent.clientY), FCamera, FGrid);
      LChanged := FGuideInput.Commit;
    except
      CancelGuideGesture;
      raise;
    end;
    LCapture := FGuideCapture;
    FGuideCapture := nil;
    FGuideCapturing := False;

    if (LCapture <> nil) and TNyxGestureElement(LCapture).hasPointerCapture(FGuidePointer) then
    begin
      LCapture.releasePointerCapture(FGuidePointer);
    end;
    AEvent.preventDefault;

    if LChanged then
    begin
      NotifyPrecision;
    end
    else
    begin
      PaintPrecision;
    end;
    Exit;
  end;
  for LIndex := 0 to FContactCount - 1 do
  begin

    if FContacts[LIndex].ID = AEvent.pointerId then
    begin
      Dec(FContactCount);

      if LIndex < FContactCount then
      begin
        FContacts[LIndex] := FContacts[FContactCount];
      end;

      if TNyxGestureElement(FSurface).hasPointerCapture(AEvent.pointerId) then
      begin
        FSurface.releasePointerCapture(AEvent.pointerId);
      end;
      FStartCamera := FCamera;

      if FContactCount > 0 then
      begin
        FContacts[0].Start := FContacts[0].Current;
      end;
      AEvent.preventDefault;
      Exit;
    end;
  end;
end;

function TCanvasView.Cancel(AEvent: TJSPointerEvent): Boolean;
var
  LIndex: Integer;
begin
  Result := True;

  if not FConnected then
  begin
    Exit;
  end;

  if FGuideCapturing and (AEvent.pointerId = FGuidePointer) then
  begin
    CancelGuideGesture;
    Exit;
  end;
  for LIndex := 0 to FContactCount - 1 do
  begin

    if FContacts[LIndex].ID = AEvent.pointerId then
    begin
      CancelPan;
      Exit;
    end;
  end;
end;

procedure TCanvasView.CancelPan;
var
  LIDs: array[0..1] of NativeInt;
  LCount: Integer;
  LIndex: Integer;
begin
  LCount := FContactCount;
  for LIndex := 0 to LCount - 1 do
  begin
    LIDs[LIndex] := FContacts[LIndex].ID;
  end;
  FContactCount := 0;
  for LIndex := 0 to LCount - 1 do
  begin

    if TNyxGestureElement(FSurface).hasPointerCapture(LIDs[LIndex]) then
    begin
      FSurface.releasePointerCapture(LIDs[LIndex]);
    end;
  end;

  if FConnected and (LCount > 0) then
  begin
    SetCamera(FGestureCamera);
  end;
end;

function TCanvasView.Wheel(AEvent: TJSWheelEvent): Boolean;
var
  LMultiplier: Double;
  LDelta: Double;
begin
  Result := True;

  if not FConnected or not FGate.EnabledPolicy.Allowed or ((AEvent.target <> FSurface) and
    (AEvent.target <> FBackground) and not AEvent.ctrlKey) then
  begin
    Exit;
  end;
  CancelGuideGesture;
  LMultiplier := 1;
  case AEvent.deltaMode of
    1: LMultiplier := 16;
    2: LMultiplier := FCamera.ViewportHeight;
  end;
  LDelta := AEvent.deltaY * LMultiplier;

  if AEvent.ctrlKey then
  begin
    ZoomAt(FCamera.Zoom * Power(1.1, Max(-10, Min(10, -LDelta / 100))),
      LocalPoint(AEvent.clientX, AEvent.clientY));
  end
  else
  begin
    PanBy(-AEvent.deltaX * LMultiplier, -LDelta);
  end;
  AEvent.preventDefault;
end;

function TCanvasView.Keyboard(AEvent: TJSKeyboardEvent): Boolean;
var
  LLease: INyxCanvasWorkspaceView;
begin
  Result := True;

  if not FConnected or not FGate.EnabledPolicy.Allowed or
    ((AEvent.target <> FSurface) and (AEvent.target <> FTopRuler) and
      (AEvent.target <> FLeftRuler)) then
  begin
    Exit;
  end;
  LLease := Self;

  if GuideKey(AEvent) then
  begin
    Exit;
  end;
  case AEvent.key of
    'ArrowLeft': PanBy(40, 0);
    'ArrowRight': PanBy(-40, 0);
    'ArrowUp': PanBy(0, 40);
    'ArrowDown': PanBy(0, -40);
    '+', '=': Execute(nccZoomIn);
    '-': Execute(nccZoomOut);
    'Home': Execute(nccHome);
    'Escape': CancelPan;
  else
    Exit;
  end;
  AEvent.preventDefault;
end;

function TCanvasView.GetConnected: Boolean;
begin
  Result := FConnected;
end;

function TCanvasView.GetControlScaling: TNyxCanvasControlScaling;
begin
  RequireConnected;
  Result := FScaling;
end;

function TCanvasView.GetSurface: TJSHTMLElement;
begin
  RequireConnected;
  Result := FSurface;
end;

function TCanvasView.GetCamera: TNyxCanvasCamera;
begin
  RequireConnected;
  Result := FCamera;
end;

function TCanvasView.GetMountedCount: Integer;
begin
  FScheduler.RequireUI;
  Result := Length(FMounts);
end;

function TCanvasView.GetPresentationRevision: Integer;
begin
  RequireConnected;
  Result := FPresentationRevision;
end;

function TCanvasView.ApplyLayout(const APlan: INyxCanvasLayoutPlan;
  const ACamera: TNyxCanvasCamera; AExpectedPresentation: Integer;
  const ACancellation: INyxCancellationScope): Boolean;
begin

  if APlan = nil then
  begin
    raise ENyxCanvasScene.Create('Workspace layout publication requires a prepared plan');
  end;
  FScheduler.RequireUI;

  if not FConnected then
  begin
    Exit(False);
  end;
  Result := Present(ACamera, True, APlan, AExpectedPresentation, ACancellation);
end;

procedure TCanvasView.SetCamera(const ACamera: TNyxCanvasCamera);
begin
  Present(ACamera, False);
end;

procedure TCanvasView.PanBy(AX, AY: Double);
begin
  SetCamera(GetCamera.PanBy(AX, AY));
end;

procedure TCanvasView.ZoomAt(AZoom: Double; const AAnchor: TNyxCanvasPoint);
begin
  SetCamera(GetCamera.ZoomAt(AZoom, AAnchor));
end;

procedure TCanvasView.Execute(ACommand: TNyxCanvasCommand);
var
  LCamera: TNyxCanvasCamera;
begin
  LCamera := GetCamera;
  case Ord(ACommand) of
    Ord(nccZoomOut): SetCamera(LCamera.ZoomAt(LCamera.Zoom / 1.25,
      NyxCanvasPoint(LCamera.ViewportWidth / 2, LCamera.ViewportHeight / 2)));
    Ord(nccZoomIn): SetCamera(LCamera.ZoomAt(LCamera.Zoom * 1.25,
      NyxCanvasPoint(LCamera.ViewportWidth / 2, LCamera.ViewportHeight / 2)));
    Ord(nccFit): DoFitNavigation(NyxCanvasNavigationOptions, True);
    Ord(nccHome): SetCamera(LCamera.AtCenter(NyxCanvasPoint(0, 0)));
  else
    raise ENyxCanvasScene.Create('Unknown canvas navigation command');
  end;
end;

procedure TCanvasView.Refresh;
begin
  Present(FCamera, True);
end;

function TCanvasView.GetScene: INyxCanvasScene;
begin
  RequireConnected;
  Result := FWorkspace.Scene;
end;

function TCanvasView.BehaviorFor(const AItem: TNyxCanvasItemRef): INyxViewBehavior;
var
  LIndex: Integer;
begin
  RequireConnected;
  LIndex := FindMount(AItem);

  if LIndex < 0 then
  begin
    raise ENyxCanvasScene.Create('Canvas item has no mounted behavior context');
  end;
  Result := FMounts[LIndex].Behavior;
end;

function TCanvasView.EventsFor(const AItem: TNyxCanvasItemRef): INyxEvents;
begin
  Result := RendererFor(AItem).Events;
end;

function TCanvasView.RendererFor(const AItem: TNyxCanvasItemRef): TNyxBrowserRenderer;
var
  LIndex: Integer;
begin
  RequireConnected;
  LIndex := FindMount(AItem);

  if LIndex < 0 then
  begin
    raise ENyxCanvasScene.Create('Canvas item has no mounted live controls');
  end;
  Result := FMounts[LIndex].Renderer;
end;

function TCanvasView.TryControlBounds(const AItem: TNyxCanvasItemRef;
  const AControl: TNyxControlRef; out ABounds: TNyxCanvasBounds): Boolean;
var
  LIndex: Integer;
  LElement: TJSHTMLElement;
  LOuter: TJSDOMRect;
  LSurface: TJSDOMRect;
  LBounds: TNyxCanvasBounds;
  LAncestor: TJSHTMLElement;
  LClip: TJSDOMRect;
  LLeft: Double;
  LTop: Double;
  LRight: Double;
  LBottom: Double;
  LScaleX: Double;
  LScaleY: Double;
  LOverflowX: TNyxText;
  LOverflowY: TNyxText;

  function Clips(const AOverflow: TNyxText): Boolean;
  begin
    { Closed CSS overflow names stay at this browser boundary. Visible overflow
      intentionally contributes no clip on that axis. }
    Result := (AOverflow = 'hidden') or (AOverflow = 'clip') or
      (AOverflow = 'auto') or (AOverflow = 'scroll');
  end;
begin
  ABounds := Default(TNyxCanvasBounds);
  RequireConnected;
  Result := False;
  LIndex := FindMount(AItem);

  if (LIndex < 0) or (FSurface.clientWidth <= 0) or (FSurface.clientHeight <= 0) then
  begin
    Exit;
  end;
  try
    LElement := FMounts[LIndex].Renderer.ElementFor(AControl.ID, niRuntime);
  except
    on E: ENyxModel do
    begin
      Exit;
    end;
  end;
  LOuter := LElement.getBoundingClientRect;
  LSurface := FSurface.getBoundingClientRect;

  if (LOuter.width <= 0) or (LOuter.height <= 0) or
    (LSurface.width <= 0) or (LSurface.height <= 0) then
  begin
    Exit;
  end;
  LLeft := Max(LOuter.left, LSurface.left);
  LTop := Max(LOuter.top, LSurface.top);
  LRight := Min(LOuter.right, LSurface.right);
  LBottom := Min(LOuter.bottom, LSurface.bottom);
  LAncestor := LElement;
  while LAncestor <> nil do
  begin

    if window.getComputedStyle(LAncestor).getPropertyValue('visibility') = 'hidden' then
    begin
      Exit;
    end;
    LOverflowX := window.getComputedStyle(LAncestor).getPropertyValue('overflow-x');
    LOverflowY := window.getComputedStyle(LAncestor).getPropertyValue('overflow-y');

    if (LAncestor <> LElement) and (Clips(LOverflowX) or Clips(LOverflowY)) then
    begin
      LClip := LAncestor.getBoundingClientRect;

      if (LAncestor.offsetWidth <= 0) or (LAncestor.offsetHeight <= 0) then
      begin
        Exit;
      end;
      LScaleX := LClip.width / LAncestor.offsetWidth;
      LScaleY := LClip.height / LAncestor.offsetHeight;

      if Clips(LOverflowX) then
      begin
        LLeft := Max(LLeft, LClip.left + LAncestor.clientLeft * LScaleX);
        LRight := Min(LRight, LClip.left +
          (LAncestor.clientLeft + LAncestor.clientWidth) * LScaleX);
      end;

      if Clips(LOverflowY) then
      begin
        LTop := Max(LTop, LClip.top + LAncestor.clientTop * LScaleY);
        LBottom := Min(LBottom, LClip.top +
          (LAncestor.clientTop + LAncestor.clientHeight) * LScaleY);
      end;
    end;

    if (LLeft >= LRight) or (LTop >= LBottom) then
    begin
      Exit;
    end;

    if LAncestor = FSurface then
    begin
      Break;
    end;
    LAncestor := TJSHTMLElement(LAncestor.parentElement);
  end;

  if LAncestor <> FSurface then
  begin
    Exit;
  end;
  LBounds := NyxCanvasBounds((LOuter.left - LSurface.left) * FSurface.clientWidth / LSurface.width,
    (LOuter.top - LSurface.top) * FSurface.clientHeight / LSurface.height,
    LOuter.width * FSurface.clientWidth / LSurface.width,
    LOuter.height * FSurface.clientHeight / LSurface.height);
  Result := True;

  if Result then
  begin
    ABounds := LBounds;
  end;
end;

function TCanvasView.InputFor(const AItem: TNyxCanvasItemRef;
  const AControl: TNyxControlRef): TJSHTMLElement;
begin
  Result := RendererFor(AItem).InputFor(AControl.ID);
end;

procedure TCanvasView.Disconnect;
var
  LIndex: Integer;
begin

  if FScheduler <> nil then
  begin
    FScheduler.RequireUI;
  end;
  FConnected := False;
  FHostInput := nil;

  if FResourceSubscription <> nil then
  begin
    FResourceSubscription.Disconnect;
    FResourceSubscription := nil;
  end;

  if FNavigationSources <> nil then
  begin
    FNavigationSources.Close;
  end;

  if FPresentationObservers <> nil then
  begin
    FPresentationObservers.Close;
  end;

  if FGate <> nil then
  begin
    FGate.Update(False, True);
  end;
  FContactCount := 0;

  if FUpdating then
  begin
    FDisconnectPending := True;
    Exit;
  end;
  FDisconnectPending := False;

  if FGuideInput <> nil then
  begin
    CancelGuideGesture;
  end;

  if FObserver <> nil then
  begin
    FObserver.disconnect;
    FObserver := nil;
  end;

  { Construction can refuse while reserving description IDs, before rulers
    exist. Retire each partially constructed native DOM hook independently. }

  if FTopRuler <> nil then
  begin
    FTopRuler.removeEventListener('pointerdown', @RulerDown);
    FTopRuler.removeEventListener('keydown', @RulerKey);
  end;

  if FLeftRuler <> nil then
  begin
    FLeftRuler.removeEventListener('pointerdown', @RulerDown);
    FLeftRuler.removeEventListener('keydown', @RulerKey);
  end;

  if FSurface <> nil then
  begin
    FSurface.removeEventListener('pointerdown', @Down);
    FSurface.removeEventListener('pointermove', @Move);
    FSurface.removeEventListener('pointerup', @Up);
    FSurface.removeEventListener('pointercancel', @Cancel);
    FSurface.removeEventListener('lostpointercapture', @Cancel);
    FSurface.removeEventListener('wheel', @Wheel);
    FSurface.removeEventListener('keydown', @Keyboard);
  end;
  for LIndex := 0 to High(FMounts) do
  begin
    FMounts[LIndex].Free;
  end;
  SetLength(FMounts, 0);

  if FSurface <> nil then
  begin
    FSurface.remove;
    FSurface := nil;
    FBackground := nil;
    FDescription := nil;
    FTopRuler := nil;
    FLeftRuler := nil;
    FGuideSVG := nil;
  end;
  FreeAndNil(FChrome);
  FResources := nil;
  FSnapshot := nil;
end;

procedure TCanvasView.Constrain(AEnabled, AReadOnly: Boolean);
var
  LIndex: Integer;
begin
  RequireConnected;

  if ((FGate.EnabledPolicy).Allowed = AEnabled) and
    ((FGate.ReadOnlyPolicy).ReadOnly = AReadOnly) then
  begin
    Exit;
  end;
  FGate.Update(AEnabled, AReadOnly);
  FChrome.Sync;

  if not AEnabled or AReadOnly then
  begin
    CancelGuideGesture;
  end;
  PaintPrecision;

  if not AEnabled then
  begin
    CancelPan;
  end;
  for LIndex := 0 to High(FMounts) do
  begin
    FMounts[LIndex].Renderer.Sync;
  end;
end;

{$I nyx.canvas.navigation.view.inc}
{$I nyx.canvas.precision.view.inc}
{$I nyx.canvas.precision.browser.inc}

end.
