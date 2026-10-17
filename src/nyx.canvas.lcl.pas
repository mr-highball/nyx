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
unit nyx.canvas.lcl;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  Classes, SysUtils, Math, Types, Controls, Forms, Graphics, ExtCtrls,
  nyx.text, nyx.types, nyx.model, nyx.state, nyx.theme, nyx.events,
  nyx.render.lcl, nyx.scheduler, nyx.canvas.geometry, nyx.canvas.scene, nyx.canvas.workspace,
  nyx.application.context, nyx.view.behavior, nyx.flow, nyx.publication,
  nyx.flow.declarations, nyx.flow.editing, nyx.flow.routing, nyx.canvas.routes,
  nyx.flow.route.model, nyx.flow.routes, nyx.data, nyx.canvas.observers,
  nyx.programs.declarations, nyx.programs.editing, nyx.logic.programs,
  nyx.canvas.navigation, nyx.canvas.navigation.sources,
  nyx.canvas.precision, nyx.canvas.precision.interaction,
  nyx.resource.context, nyx.application.resources;

type
  { Configure creator factories/events before each independent item renderer
    mounts. The receiver is borrowed and must outlive this view. }
  TNyxCanvasLCLRendererSetup = procedure(ARenderer: TNyxLCLRenderer) of object;

  { Native presentation capability for a connected workspace. Returned LCL
    controls/renderers are borrowed until Disconnect and must not be freed or
    reparented. Off-screen items may have no mounted input; widgetset behavior
    remains the adapter's responsibility rather than portable scene authority. }
  INyxLCLCanvasWorkspaceView = interface(INyxCanvasWorkspaceView)
    ['{737A7921-4621-4C6F-8C01-0CA000000012}']
    function GetSurface: TWinControl;
    function RendererFor(const AItem: TNyxCanvasItemRef): TNyxLCLRenderer;
    function InputFor(const AItem: TNyxCanvasItemRef;
      const AControl: TNyxControlRef): TControl;
    { Actual native handles are borrowed until Disconnect. Never Free/reparent
      them. Tests and application adapters can qualify live nested input. }
    property Surface: TWinControl read GetSurface;
  end;

  { Optional borrowed ruler faces for host integration and ordinary input
    qualification. X is the top measuring strip, Y the left. Hidden axes return
    nil; connected faces must never be freed/reparented by the caller. }
  INyxLCLCanvasRulerPresentation = interface(IInterface)
    ['{2CAB13D6-C881-49DD-AC66-10160A100006}']
    function RulerFor(AAxis: TNyxCanvasAxis): TWinControl;
  end;

{ Mount ordinary Nyx chrome and live item renderers. Host and a supplied store
  are borrowed; document/theme context is copied. Caller disconnects before host
  or borrowed store disposal. Distinct views own independent cameras and an
  independent default store when no application store is supplied. Optional
  AResources supplies the existing application broker's current catalog and
  prepared updates for chrome and every retained item, including rows. The caller
  owns its Start/Stop lifecycle; this view disconnects only its own subscription.
  Nil preserves authored/default or explicit renderer-setup resource behavior.
  In design preview, optional AHostInput grants input only to explicitly selected
  host-owned item roots; application behavior stays paused. This managed policy
  is retained until Disconnect and queried for each newly mounted lifetime. }
function MountNyxLCLCanvasWorkspace(const AWorkspace: INyxCanvasWorkspace;
  ADocument: TNyxDocument; AHost: TWinControl; AState: TNyxState = nil;
  ATheme: TNyxTheme = nil; ASetup: TNyxCanvasLCLRendererSetup = nil;
  ADesignMode: Boolean = False;
  const AContext: INyxApplicationContext = nil;
  const AResolver: INyxLogicResolver = nil;
  const AResources: INyxApplicationResources = nil;
  const AHostInput: INyxCanvasHostInput = nil): INyxLCLCanvasWorkspaceView;

implementation

uses
  nyx.design.tokens, nyx.interaction.policy, nyx.behavior, nyx.widgets.lcl,
  nyx.paint.buffer.lcl, LCLType, LMessages;
type
  { Forward identity for adapter-owned faces; neither face acquires a managed
    reference back into its owning presentation. }
  TCanvasView = class;

  { Borrow protected LCL capture on an existing native face. This access class
    allocates nothing and never changes control ownership; capture behavior
    remains the target widgetset's responsibility. }
  TCanvasInputAccess = class(TWinControl);

  { Native measured ruler face owned by its canvas surface. View is borrowed
    and revoked before retirement. Painting uses shared projected ticks; input
    normalizes LCL coordinates before entering the portable guide controller. }
  TCanvasRuler = class(TCustomControl)
  public
    View: TCanvasView;
    Axis: TNyxCanvasAxis;
    constructor CreateFor(AParent: TWinControl; AView: TCanvasView; AAxis: TNyxCanvasAxis);
  protected
    procedure Paint; override;
    procedure WndProc(var AMessage: TLMessage); override;
  end;

  { Native drawing/input surface owned by its LCL parent. FView is borrowed and
    revoked before the view retires; window messages never acquire scene ownership. }
  TCanvasSurface = class(TCustomControl)
  private
    FView: TCanvasView;
  protected
    procedure Paint; override;
    procedure Resize; override;
    procedure WndProc(var AMessage: TLMessage); override;
  public
    constructor CreateFor(AOwner: TComponent; AView: TCanvasView);
  end;

  { One owned native item presentation. This mount retains scene content and
    owns its copied context, renderer and behavior; its host panel belongs to
    the native surface and outlives the mounted renderer's disconnection. }
  TCanvasItemMount = class
  public
    Reference: TNyxCanvasItemRef;
    Content: INyxNode;
    Host: TPanel;
    Document: TNyxDocument;
    Renderer: TNyxLCLRenderer;
    Behavior: INyxViewBehavior;
    destructor Destroy; override;
  end;

  { View-owned item vector; each class entry has exactly one disposing owner. }
  TCanvasMounts = array of TCanvasItemMount;
  { Detached placement candidates contain no LCL controls or behavior authority. }
  TCanvasFrames = array of TNyxCanvasControlFrame;

  { UI-owned live presentation. It retains copied document/scene context and
    item mounts while borrowing its host/store; no scene control retains it.
    Prepared layout publication stages faces before the scene/camera commit.
    An optional resource port coordinates all retained renderers through the
    existing application broker, owning neither its application nor transport. }
  TCanvasView = class(TInterfacedObject, INyxCanvasWorkspaceView,
    INyxLCLCanvasWorkspaceView, INyxCanvasRuntimeView, INyxCanvasLayoutView,
    INyxCanvasCoordinatedLayoutView, INyxCanvasFlowEditing, INyxCanvasRouteEditing,
    INyxCanvasPresentationObservation, INyxCanvasControlGeometry, INyxCanvasProgramEditing,
    INyxCanvasNavigationContributions, INyxCanvasPrecisionView,
    INyxCanvasSourceLayoutView, INyxLCLCanvasRulerPresentation)
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
    FSetup: TNyxCanvasLCLRendererSetup;
    FHostInput: INyxCanvasHostInput;
    FParentContext: INyxApplicationContext;
    FResolver: INyxLogicResolver;
    { Retain the application's loader port, never its application. The caller
      owns Start/Stop; Disconnect revokes only this view's borrowed receiver. }
    FResources: INyxApplicationResources;
    FResourceSubscription: INyxResourceSubscription;
    FChrome: TNyxLCLRenderer;
    FScheduler: INyxScheduler;
    FSurface: TCanvasSurface;
    FCamera: TNyxCanvasCamera;
    FGrid: TNyxCanvasGrid;
    FGuideInput: INyxCanvasGuideInteraction;
    FTopRuler: TCanvasRuler;
    FLeftRuler: TCanvasRuler;
    FGuideCapture: TWinControl;
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
    { Keep constructor staging free of transient Self interface owners. }
    FUpdating: Boolean;
    FDisconnectPending: Boolean;
    FDragging: Boolean;
    FStartCamera: TNyxCanvasCamera;
    FStartPoint: TPoint;
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
    procedure Down(ASender: TObject; AButton: TMouseButton;
      AShift: TShiftState; AX, AY: Integer);
    procedure Move(ASender: TObject; AShift: TShiftState; AX, AY: Integer);
    procedure Up(ASender: TObject; AButton: TMouseButton;
      AShift: TShiftState; AX, AY: Integer);
    procedure Wheel(ASender: TObject; AShift: TShiftState; AWheelDelta: Integer;
      AMousePos: TPoint; var AHandled: Boolean);
    procedure Keyboard(ASender: TObject; var AKey: Word; AShift: TShiftState);
    procedure CancelPan;
    procedure RequireConnected;
    procedure Resized;
    procedure PaintGrid(ACanvas: TCanvas);
    procedure PaintPrecision;
    procedure PaintGuides(ACanvas: TCanvas);
    procedure PaintRuler(ARuler: TCanvasRuler);
    procedure NotifyPrecision;
    procedure RequirePrecisionRevision;
    procedure CancelGuideGesture;
    procedure AppendPrecisionOccluders(const ACamera: TNyxCanvasCamera;
      var AFrame: TNyxCanvasNavigationFrame);
    function GuidePoint(ASender: TObject; AX, AY: Integer): TNyxCanvasPoint;
    procedure RulerDown(ASender: TObject; AButton: TMouseButton;
      AShift: TShiftState; AX, AY: Integer);
    procedure RulerKey(ASender: TObject; var AKey: Word; AShift: TShiftState);
    function GuideKey(var AKey: Word; AShift: TShiftState): Boolean;
  public
    constructor Create(const AWorkspace: INyxCanvasWorkspace;
      ADocument: TNyxDocument; AHost: TWinControl; AState: TNyxState;
      ATheme: TNyxTheme; ASetup: TNyxCanvasLCLRendererSetup; ADesignMode: Boolean;
      const AContext: INyxApplicationContext; const AResolver: INyxLogicResolver;
      const AResources: INyxApplicationResources; const AHostInput: INyxCanvasHostInput);
    destructor Destroy; override;
    function GetSurface: TWinControl;
    function RulerFor(AAxis: TNyxCanvasAxis): TWinControl;
    function GetPrecision: TNyxCanvasPrecisionOptions;
    procedure SetPrecision(const AOptions: TNyxCanvasPrecisionOptions);
    function GetSelectedGuide: TNyxCanvasGuideRef;
    procedure SelectGuide(const AReference: TNyxCanvasGuideRef);
    function SnapGuidePoint(const APoint: TNyxCanvasPoint;
      const AExclude: TNyxCanvasGuideRef): TNyxCanvasGuideSnap;
    function GetConnected: Boolean;
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
    function EventsFor(const AItem: TNyxCanvasItemRef): INyxEvents;
    function BehaviorFor(const AItem: TNyxCanvasItemRef): INyxViewBehavior;
    function GetScene: INyxCanvasScene;
    function RendererFor(const AItem: TNyxCanvasItemRef): TNyxLCLRenderer;
    function InputFor(const AItem: TNyxCanvasItemRef;
      const AControl: TNyxControlRef): TControl;
    procedure Disconnect;
  end;

{$I nyx.canvas.publication.inc}
{$I nyx.canvas.source.context.inc}
{$I nyx.canvas.flows.inc}
{$I nyx.canvas.routes.inc}

constructor TCanvasSurface.CreateFor(AOwner: TComponent; AView: TCanvasView);
begin
  inherited Create(AOwner);
  FView := AView;
  TabStop := True;
  AccessibleName := 'Canvas workspace';
  AccessibleDescription := 'Arrow keys pan, plus and minus zoom, Home centers.';
  ControlStyle := ControlStyle + [csOpaque];
  DoubleBuffered := True;
end;

procedure TCanvasSurface.Paint;
begin

  if FView <> nil then
  begin
    FView.PaintGrid(Canvas);
  end;
end;

procedure TCanvasSurface.Resize;
begin
  inherited Resize;

  if FView <> nil then
  begin
    FView.Resized;
  end;
end;

procedure TCanvasSurface.WndProc(var AMessage: TLMessage);
begin

  if (AMessage.Msg = LM_CAPTURECHANGED) and (FView <> nil) then
  begin

    if FView.FDragging then
    begin
      FView.CancelPan;
    end;

    if (FView.FGuideInput <> nil) and FView.FGuideInput.Active then
    begin
      FView.CancelGuideGesture;
    end;
  end;
  inherited WndProc(AMessage);
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
  Host.Free;
  Content := nil;
  inherited Destroy;
end;

constructor TCanvasView.Create(const AWorkspace: INyxCanvasWorkspace;
  ADocument: TNyxDocument; AHost: TWinControl; AState: TNyxState;
  ATheme: TNyxTheme; ASetup: TNyxCanvasLCLRendererSetup; ADesignMode: Boolean;
  const AContext: INyxApplicationContext; const AResolver: INyxLogicResolver;
  const AResources: INyxApplicationResources; const AHostInput: INyxCanvasHostInput);
var
  LHost: TControl;
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
    raise ENyxCanvasScene.Create('Native workspace mount requires workspace, document and host');
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
  FChrome := TNyxLCLRenderer.Create(FTheme);
  FScheduler := FChrome.Events.Scheduler;
  FNavigationSources := TNyxCanvasNavigationSources.Create(FScheduler);
  FScheduler.RequireUI;
  FChrome.OnEvent := ChromeEvent;
  FChrome.ResourceUpdates := FResources;
  FChrome.Render(FDocument, LRoot, AHost, False, FState);
  FChrome.Root.AttachEnabledPolicy(FGate.EnabledPolicy);
  LHost := FChrome.ControlFor(NyxCanvasWorkspacePart(AWorkspace.ID, ncwpSurface).ID);

  if not (LHost is TWinControl) then
  begin
    raise ENyxCanvasScene.Create('Workspace surface must be a native Nyx layout host');
  end;
  FSurface := TCanvasSurface.CreateFor(nil, Self);
  FSurface.Parent := TWinControl(LHost);
  FSurface.Align := alClient;
  FSurface.OnMouseDown := Down;
  FSurface.OnMouseMove := Move;
  FSurface.OnMouseUp := Up;
  FSurface.OnMouseWheel := Wheel;
  FSurface.OnKeyDown := Keyboard;
  FTopRuler := TCanvasRuler.CreateFor(FSurface, Self, ncaxX);
  FLeftRuler := TCanvasRuler.CreateFor(FSurface, Self, ncaxY);
  FCamera := AWorkspace.Camera;
  FConnected := True;
  Refresh;
  FConstructing := False;

  if FResources <> nil then
  begin
    FResourceSubscription := NyxPreparedApplicationResources(FResources).
      SubscribePrepared(ResourcesReady, PrepareResources);
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

function MountNyxLCLCanvasWorkspace(const AWorkspace: INyxCanvasWorkspace;
  ADocument: TNyxDocument; AHost: TWinControl; AState: TNyxState;
  ATheme: TNyxTheme; ASetup: TNyxCanvasLCLRendererSetup;
  ADesignMode: Boolean; const AContext: INyxApplicationContext;
  const AResolver: INyxLogicResolver;
  const AResources: INyxApplicationResources;
  const AHostInput: INyxCanvasHostInput): INyxLCLCanvasWorkspaceView;
begin
  Result := TCanvasView.Create(AWorkspace, ADocument, AHost, AState, ATheme, ASetup,
    ADesignMode, AContext, AResolver, AResources, AHostInput);
end;

procedure TCanvasView.RequireConnected;
begin
  FScheduler.RequireUI;

  if not FConnected then
  begin
    raise ENyxCanvasScene.Create('Native workspace view is disconnected');
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
    Result.Host := TNyxLCLBufferedPanel.Create(nil);
    Result.Host.BevelOuter := bvNone;
    Result.Host.Visible := False;
    Result.Host.Parent := FSurface;
    Result.Host.SetBounds(AFrame.Left, AFrame.Top, AFrame.Width, AFrame.Height);
    Result.Renderer := TNyxLCLRenderer.Create(FTheme);

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
    { Host authoring input never changes the global behavior gate above. Source
      components remain preview-only unless their owner explicitly selects them. }
    Result.Renderer.Render(Result.Document, LRoot, Result.Host, LDesignMode, FState);
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
  LFocus: TWinControl;
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
    raise ENyxCanvasScene.Create('Native workspace presentation is already updating');
  end;

  if (FSurface.ClientWidth <= 0) or (FSurface.ClientHeight <= 0) then
  begin
    Exit;
  end;

  if not FConstructing then
  begin
    LKeepAlive := Self;
  end;
  LCamera := ACamera.WithViewport(FSurface.ClientWidth, FSurface.ClientHeight);
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
          LMounts[LIndex].Host.SetBounds(LFrames[LIndex].Left, LFrames[LIndex].Top,
            LFrames[LIndex].Width, LFrames[LIndex].Height);

          if not FConnected then
          begin
            Exit;
          end;
          LMounts[LIndex].Host.BringToFront;
        end;
        LKeepFocus := False;
        LFocus := Screen.ActiveControl;
        while LFocus <> nil do
        begin

          if LFocus = LMounts[LIndex].Host then
          begin
            LKeepFocus := LSnapshot.Items[LIndex].Placement.IsVisible;
            Break;
          end;
          LFocus := LFocus.Parent;
        end;

        if not LFrames[LIndex].Visible and LKeepFocus then
        begin
          { Keep a focused editor alive beyond the clipped viewport, with a safe
            negative allocation. Hiding its parent would transfer native focus. }
          LMounts[LIndex].Host.Left := -MaximumNyxCanvasControlExtent;
        end;
        LMounts[LIndex].Host.Visible := LFrames[LIndex].Visible or LKeepFocus;

        if not FConnected then
        begin
          Exit;
        end;
      end;
      PaintPrecision;
      FSurface.Invalidate;
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

procedure TCanvasView.Resized;
begin

  { A hidden ordinary-render candidate can receive synchronous WM_SIZE while
    its parent publishes final bounds. Before its first explicit presentation,
    keep construction on Refresh's caller stack: LCL otherwise catches a
    component-factory exception as a host error instead of refusing the candidate.
    Successful presentation publishes FSnapshot even for an empty scene. }
  if FConnected and not FUpdating and (FSnapshot <> nil) then
  begin
    CancelGuideGesture;
    Present(FCamera, False);
  end;
end;

procedure TCanvasView.PaintGrid(ACanvas: TCanvas);
var
  LSpacing: Double;
  LX: Double;
  LY: Double;
  LStartX: Double;
  LStartY: Double;
  LCamera: TNyxCanvasCamera;

  function ThemeColor(const AValue: TNyxText): TColor;
  var
    LRGB: Integer;
  begin
    LRGB := NyxThemeRGB(AValue);
    Result := RGBToColor((LRGB shr 16) and $FF, (LRGB shr 8) and $FF, LRGB and $FF);
  end;

begin
  ACanvas.Brush.Color := ThemeColor(FTheme.Background);
  ACanvas.FillRect(Rect(0, 0, FSurface.Width, FSurface.Height));

  if not FConnected then
  begin
    Exit;
  end;

  if FGrid.Style = ncgsNone then
  begin
    PaintGuides(ACanvas);
    Exit;
  end;
  LCamera := FCamera;
  LSpacing := FGrid.VisibleSpacing(LCamera) * LCamera.Zoom;
  LStartX := LCamera.ViewportWidth / 2 +
    (FGrid.Origin.X - LCamera.WorldCenter.X) * LCamera.Zoom;
  LStartY := LCamera.ViewportHeight / 2 +
    (FGrid.Origin.Y - LCamera.WorldCenter.Y) * LCamera.Zoom;
  LStartX := LStartX - Floor(LStartX / LSpacing) * LSpacing;
  LStartY := LStartY - Floor(LStartY / LSpacing) * LSpacing;
  ACanvas.Pen.Color := ThemeColor(FTheme.Border);

  if FGrid.Style = ncgsLines then
  begin
    LX := LStartX;
    while LX < FSurface.Width do
    begin
      ACanvas.Line(Floor(LX + 0.5), 0, Floor(LX + 0.5), FSurface.Height);
      LX := LX + LSpacing;
    end;
    LY := LStartY;
    while LY < FSurface.Height do
    begin
      ACanvas.Line(0, Floor(LY + 0.5), FSurface.Width, Floor(LY + 0.5));
      LY := LY + LSpacing;
    end;
  end
  else
  begin
    ACanvas.Brush.Color := ACanvas.Pen.Color;
    LX := LStartX;
    while LX < FSurface.Width do
    begin
      LY := LStartY;
      while LY < FSurface.Height do
      begin
        ACanvas.FillRect(Rect(Floor(LX), Floor(LY), Floor(LX) + 2, Floor(LY) + 2));
        LY := LY + LSpacing;
      end;
      LX := LX + LSpacing;
    end;
  end;
  PaintGuides(ACanvas);
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

procedure TCanvasView.Down(ASender: TObject; AButton: TMouseButton;
  AShift: TShiftState; AX, AY: Integer);
var
  LKeepAlive: INyxCanvasWorkspaceView;
  LGuide: TNyxCanvasGuideRef;
begin

  if not FConnected or not FGate.EnabledPolicy.Allowed or not (AButton in [mbLeft, mbMiddle]) then
  begin
    Exit;
  end;
  { One captured guide owns this gesture until release/cancel. Additional mouse
    buttons cannot start an overlapping pan or replace its preview. }

  if FGuideInput.Active then
  begin
    Exit;
  end;
  LKeepAlive := Self;

  if not FDesignMode and (AButton = mbLeft) then
  begin
    LGuide := FGuideInput.Hit(GuidePoint(ASender, AX, AY), FCamera);

    if LGuide.Defined then
    begin
      RequirePrecisionRevision;
      FGuideInput.Select(LGuide);

      if not FGate.ReadOnlyPolicy.ReadOnly and not FGuideInput.Accepted.Named(LGuide).IsLocked then
      begin
        FGuideInput.BeginMove(LGuide);
      end;
      FSurface.SetFocus;

      if not FConnected then
      begin
        Exit;
      end;

      if FGuideInput.Active then
      begin
        FGuideCapture := FSurface;
        FSurface.MouseCapture := True;
      end;
      NotifyPrecision;
      Exit;
    end;
  end;
  FStartCamera := FCamera;
  FStartPoint := Point(AX, AY);
  FDragging := True;
  FSurface.SetFocus;

  if not FConnected then
  begin
    Exit;
  end;
  FSurface.MouseCapture := True;
end;

procedure TCanvasView.Move(ASender: TObject; AShift: TShiftState; AX, AY: Integer);
begin

  if FConnected and FGuideInput.Active then
  begin

    if (FGuideCapture = nil) or not TCanvasInputAccess(FGuideCapture).MouseCapture then
    begin
      CancelGuideGesture;
      Exit;
    end;
    FGuideInput.Move(GuidePoint(ASender, AX, AY), FCamera, FGrid);
    PaintPrecision;
    FSurface.Invalidate;
    Exit;
  end;

  if FConnected and FDragging then
  begin

    if not FSurface.MouseCapture then
    begin
      CancelPan;
      Exit;
    end;
    SetCamera(FStartCamera.PanBy(AX - FStartPoint.X, AY - FStartPoint.Y));
  end;
end;

procedure TCanvasView.Up(ASender: TObject; AButton: TMouseButton;
  AShift: TShiftState; AX, AY: Integer);
var
  LLease: INyxCanvasWorkspaceView;
  LChanged: Boolean;
  LCapture: TWinControl;
begin

  if FConnected and FGuideInput.Active then
  begin

    if AButton <> mbLeft then
    begin
      Exit;
    end;
    LLease := Self;
    try
      RequirePrecisionRevision;
      FGuideInput.Move(GuidePoint(ASender, AX, AY), FCamera, FGrid);
      LChanged := FGuideInput.Commit;
    except
      CancelGuideGesture;
      raise;
    end;
    LCapture := FGuideCapture;
    FGuideCapture := nil;

    if LCapture <> nil then
    begin
      TCanvasInputAccess(LCapture).MouseCapture := False;
    end;

    if LChanged then
    begin
      NotifyPrecision;
    end
    else
    begin
      PaintPrecision;
      FSurface.Invalidate;
    end;
    Exit;
  end;
  FDragging := False;

  if FSurface <> nil then
  begin
    FSurface.MouseCapture := False;
  end;
end;

procedure TCanvasView.CancelPan;
var
  LCamera: TNyxCanvasCamera;
begin

  if not FDragging then
  begin
    Exit;
  end;
  LCamera := FStartCamera;
  FDragging := False;
  FSurface.MouseCapture := False;

  if FConnected then
  begin
    SetCamera(LCamera);
  end;
end;

procedure TCanvasView.Wheel(ASender: TObject; AShift: TShiftState;
  AWheelDelta: Integer; AMousePos: TPoint; var AHandled: Boolean);
var
  LPoint: TPoint;
begin

  if not FConnected or not FGate.EnabledPolicy.Allowed then
  begin
    Exit;
  end;
  LPoint := FSurface.ScreenToClient(AMousePos);

  if not (ssCtrl in AShift) and (FSurface.ControlAtPos(LPoint, True, True) <> nil) then
  begin
    { A nested editor keeps ordinary wheel/scroll defaults. Ctrl-wheel opts
      into workspace zoom, matching the browser descendant policy. }
    Exit;
  end;

  CancelGuideGesture;

  if ssCtrl in AShift then
  begin
    ZoomAt(FCamera.Zoom * Power(1.1, Max(-10, Min(10, AWheelDelta / 120))),
      NyxCanvasPoint(LPoint.X, LPoint.Y));
  end
  else
  begin
    PanBy(0, AWheelDelta / 3);
  end;
  AHandled := True;
end;

procedure TCanvasView.Keyboard(ASender: TObject; var AKey: Word; AShift: TShiftState);
var
  LLease: INyxCanvasWorkspaceView;
begin

  if not FConnected or not FGate.EnabledPolicy.Allowed then
  begin
    Exit;
  end;
  LLease := Self;

  if GuideKey(AKey, AShift) then
  begin
    Exit;
  end;
  case AKey of
    VK_LEFT: PanBy(40, 0);
    VK_RIGHT: PanBy(-40, 0);
    VK_UP: PanBy(0, 40);
    VK_DOWN: PanBy(0, -40);
    VK_HOME: Execute(nccHome);
    VK_ADD, VK_OEM_PLUS: Execute(nccZoomIn);
    VK_SUBTRACT, VK_OEM_MINUS: Execute(nccZoomOut);
    VK_ESCAPE: CancelPan;
  else
    Exit;
  end;
  AKey := 0;
end;

function TCanvasView.GetConnected: Boolean;
begin
  Result := FConnected;
end;

function TCanvasView.GetSurface: TWinControl;
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

function TCanvasView.RendererFor(const AItem: TNyxCanvasItemRef): TNyxLCLRenderer;
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
  LControl: TControl;
  LAncestor: TControl;
  LPosition: TPoint;
  LBounds: TNyxCanvasBounds;
begin
  ABounds := Default(TNyxCanvasBounds);
  RequireConnected;
  Result := False;
  LIndex := FindMount(AItem);

  if (LIndex < 0) or (FSurface.ClientWidth <= 0) or (FSurface.ClientHeight <= 0) then
  begin
    Exit;
  end;
  try
    LControl := FMounts[LIndex].Renderer.ControlFor(AControl.ID, niRuntime);
  except
    on E: ENyxModel do
    begin
      Exit;
    end;
  end;
  LAncestor := LControl;
  while (LAncestor <> nil) and (LAncestor <> FSurface) do
  begin

    if not LAncestor.Visible then
    begin
      Exit;
    end;
    LAncestor := LAncestor.Parent;
  end;

  if (LAncestor = nil) or (LControl.Parent = nil) or
    (LControl.Width <= 0) or (LControl.Height <= 0) then
  begin
    Exit;
  end;
  LPosition := FSurface.ScreenToClient(LControl.Parent.ClientToScreen(
    Types.Point(LControl.Left, LControl.Top)));
  LBounds := NyxCanvasBounds(LPosition.X, LPosition.Y, LControl.Width, LControl.Height);
  Result := LBounds.Intersects(NyxCanvasBounds(0, 0, FSurface.ClientWidth, FSurface.ClientHeight));

  if Result then
  begin
    ABounds := LBounds;
  end;
end;

function TCanvasView.InputFor(const AItem: TNyxCanvasItemRef;
  const AControl: TNyxControlRef): TControl;
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
  FDragging := False;

  if FUpdating then
  begin
    { A callback may disconnect while native sizing is still on its stack.
      Revoke publication now, then retire physical faces at that frame's end. }
    FDisconnectPending := True;
    Exit;
  end;
  FDisconnectPending := False;

  if FGuideInput <> nil then
  begin
    CancelGuideGesture;
  end;

  if FTopRuler <> nil then
  begin
    FTopRuler.View := nil;
    FLeftRuler.View := nil;
  end;

  if FSurface <> nil then
  begin
    FSurface.FView := nil;
    FSurface.OnMouseDown := nil;
    FSurface.OnMouseMove := nil;
    FSurface.OnMouseUp := nil;
    FSurface.OnMouseWheel := nil;
    FSurface.OnKeyDown := nil;
    FSurface.MouseCapture := False;
  end;
  for LIndex := 0 to High(FMounts) do
  begin
    FMounts[LIndex].Free;
  end;
  SetLength(FMounts, 0);
  FreeAndNil(FSurface);
  FTopRuler := nil;
  FLeftRuler := nil;
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
{$I nyx.canvas.precision.lcl.inc}

end.
