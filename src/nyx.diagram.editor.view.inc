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
{ Shared editor behavior compiled inside the two target adapters. Only host
  allocation, pointer mapping and mounting differ; diagram meaning, revision/
  draft guards, prepared publication and ordinary Nyx chrome remain shared. }

type
  { Forward owner for revocable borrowed callbacks. The mounted interface, never
    a component-tree backreference, provides the active lifetime lease. }
  TEditorView = class;

  { Drawing provider owns only an already admitted immutable display list.
    Publication replaces that value without invoking a platform or callback.
    Neither this provider nor the scene's Drawing retains its editor/view. }
  TDiagramProjection = class(TInterfacedObject, INyxDrawingProvider)
  public
    Plan: INyxDrawingPlan;
    function Build(const AFrame: TNyxDrawingFrame): INyxDrawingPlan;
  end;

  { Event router owns this revocable bridge; Owner is borrowed. Invoke acquires
    the mounted interface before work, so a later callback may disconnect safely.
    Disconnect clears Owner before cancelling subscriptions/retiring controls.
    Its independent scheduler refuses worker entry before reading that pointer. }
  TDiagramEditorCallback = class(TNyxEventCallback)
  public
    Owner: TEditorView;
    Scheduler: INyxScheduler;
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution); override;
  end;

  { Canvas notification borrows its editor through one revocable scope. It
    subscribes to a lazily mounted picture's existing event router only; it
    never changes camera/layout synchronously during presentation notification. }
  TDiagramEditorObserver = class(TInterfacedObject, INyxCanvasPresentationObserver)
  public
    Owner: TEditorView;
    procedure Presented(ARevision: Integer);
  end;

  { Single-use model participant in the existing canvas prepared group. The
    calling view lease outlives this borrowed Owner. Only normalized private
    diagram/drawing values install, with no creator callbacks or allocation.
    Scene geometry and view camera install before any participant notifies. }
  TDiagramEditorCommit = class(TInterfacedObject, INyxPreparedPublication,
    INyxPublicationReadiness)
  public
    Owner: TEditorView;
    Candidate: INyxDiagram;
    Projection: INyxDrawingPlan;
    Selection: TNyxDiagramVertexRef;
    Expected: Integer;
    Installed: Boolean;
    procedure Validate;
    procedure CheckReady;
    procedure Install;
    procedure Notify;
    procedure Retire;
  end;

  { UI-owned ordinary Nyx composition and existing canvas attachment. Each mount
    normalizes its own initial graph, store and runtime chrome; hosts/stores are
    borrowed when supplied. The private scene owns one Drawing, whose named
    provider owns no view. All retained callbacks are explicitly revoked first.
    Runtime exports grant no document/source/history or filesystem authority. }
  TEditorView = class(TInterfacedObject, INyxDiagramEditorView, IDiagramTargetView)
  private
    FEditorID: TNyxText;
    FOptions: TNyxDiagramEditorOptions;
    FRenderer: TDiagramRenderer;
    FScheduler: INyxScheduler;
    FDocument: TNyxDocument;
    FState: TNyxState;
    FOwnState: Boolean;
    FCanvas: IDiagramCanvasView;
    FWorkspace: INyxCanvasWorkspace;
    FScene: INyxCanvasScene;
    FPicture: INyxDrawing;
    FProvider: TDiagramProjection;
    FProviderLease: INyxDrawingProvider;
    FProviderName: TNyxDrawingProviderRef;
    FItem: TNyxCanvasItemRef;
    FLayer: TNyxCanvasLayerRef;
    FDiagram: INyxDiagram;
    FSelection: TNyxDiagramVertexRef;
    FRevision: Integer;
    FConnected: Boolean;
    FPublishing: Boolean;
    FToolsExpanded: Boolean;
    FFormBaseline: TNyxText;
    FLastError: TNyxText;
    FCallbacks: array of TDiagramEditorCallback;
    FCallbackLeases: array of INyxEventCallback;
    FSubscriptions: array of INyxEventSubscription;
    FObserver: TDiagramEditorObserver;
    FObserverLease: INyxCanvasPresentationObserver;
    FObservation: INyxCancellationScope;
    FPointerEvents: INyxEvents;
    procedure RequireConnected;
    procedure RequireWritable;
    function Part(APart: TNyxDiagramEditorPart): TNyxNode;
    function Field(APart: TNyxDiagramEditorPart): INyxTextInput;
    function PropertyBuffer: TNyxText;
    procedure RefreshProperties;
    procedure SyncPicture;
    procedure BindPictureEvents;
    procedure Feedback(const AMessage: TNyxText);
    procedure SetupRenderer(ARenderer: TDiagramRenderer);
    procedure Subscribe(const AEvents: INyxEvents; const AID: TNyxText;
      ATrigger: TNyxTrigger);
    procedure Event(const AEvent: TNyxEventInfo; const AExecution: INyxExecution);
    procedure AdvanceSelection(ADelta: Integer);
    procedure Publish(const ADiagram: INyxDiagram; AExpectedRevision: Integer);
    function PointerWorld(const AEvent: TNyxEventInfo): TNyxCanvasPoint;
  public
    constructor Create(const AEditor: INyxDiagramEditor; ADocument: TNyxDocument;
      AHost: TDiagramHost; AState: TNyxState; ATheme: TNyxTheme);
    destructor Destroy; override;
    function GetRenderer: TDiagramRenderer;
    function GetConnected: Boolean;
    function GetDiagram: INyxDiagram;
    function GetRevision: Integer;
    function GetSelection: TNyxDiagramVertexRef;
    function GetCanvas: INyxCanvasWorkspaceView;
    function GetHasPendingProperties: Boolean;
    function GetToolsExpanded: Boolean;
    function GetLastError: TNyxText;
    procedure SelectVertex(const AReference: TNyxDiagramVertexRef;
      ADiscardProperties: Boolean);
    procedure SelectAt(const AWorldPoint: TNyxCanvasPoint; ADiscardProperties: Boolean);
    procedure ReplaceDiagram(const ADiagram: INyxDiagram; AExpectedRevision: Integer;
      ADiscardProperties: Boolean);
    procedure SetGeometry(const AReference: TNyxDiagramVertexRef;
      const ABounds: TNyxCanvasBounds; AExpectedRevision: Integer);
    procedure NudgeSelection(AX, AY: Double; AExpectedRevision: Integer);
    procedure ApplyProperties;
    procedure RestoreProperties;
    procedure ShowTools(AExpanded: Boolean);
    procedure RevealSelection;
    procedure Disconnect;
  end;

function TDiagramProjection.Build(const AFrame: TNyxDrawingFrame): INyxDrawingPlan;
begin
  Result := Plan;
end;

procedure TDiagramEditorCallback.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
var
  LOwner: TEditorView;
  LLease: INyxDiagramEditorView;
begin
  Scheduler.RequireUI;
  LOwner := Owner;

  if LOwner = nil then
  begin
    Exit;
  end;
  LLease := LOwner;
  try
    LOwner.Event(AEvent, AExecution);
  except
    on E: Exception do
    begin

      if LOwner.GetConnected then
      begin
        LOwner.Feedback(E.Message);
      end;
      raise;
    end;
  end;
end;

procedure TDiagramEditorObserver.Presented(ARevision: Integer);
var
  LOwner: TEditorView;
  LLease: INyxDiagramEditorView;
begin
  LOwner := Owner;

  if LOwner <> nil then
  begin
    LLease := LOwner;
    LOwner.BindPictureEvents;
  end;
end;

procedure TDiagramEditorCommit.CheckReady;
begin

  if (Owner = nil) or not Owner.GetConnected or (Owner.FRevision <> Expected) or Installed then
  begin
    raise ENyxDiagram.Create('Diagram publication is stale or retired');
  end;
end;

procedure TDiagramEditorCommit.Validate;
begin
  CheckReady;
  Owner.RequireConnected;

  if (Candidate = nil) or (Projection = nil) or (Expected = High(Integer)) then
  begin
    raise ENyxDiagram.Create('Diagram publication lacks an admitted candidate or revision capacity');
  end;
end;

procedure TDiagramEditorCommit.Install;
begin
  Owner.FDiagram := Candidate;
  Owner.FProvider.Plan := Projection;
  Owner.FSelection := Selection;
  Inc(Owner.FRevision);
  Installed := True;
end;

procedure TDiagramEditorCommit.Notify;
begin
  { Physical reconciliation is the existing canvas owner's notification phase. }
end;

procedure TDiagramEditorCommit.Retire;
begin
  Owner := nil;
  Candidate := nil;
  Projection := nil;
end;

constructor TEditorView.Create(const AEditor: INyxDiagramEditor;
  ADocument: TNyxDocument; AHost: TDiagramHost; AState: TNyxState; ATheme: TNyxTheme);
var
  LRoot: TNyxNode;
  LHost: TDiagramHost;
  LPart: TNyxDiagramEditorPart;
  LObservation: INyxCanvasPresentationObservation;
  LToolbar: INyxControl;
  LSurface: INyxControl;
  LPolicy: TNyxInteractionPolicy;
begin
  inherited Create;

  if (AEditor = nil) or (ADocument = nil) or (AHost = nil) then
  begin
    raise ENyxDiagram.Create('Diagram mount requires its editor, document and borrowed host');
  end;
  FEditorID := AEditor.ID;
  FOptions := AEditor.Options;
  FDiagram := NyxDiagramFromData(AEditor.InitialDiagram.ToData);

  if FDiagram.VertexCount > 0 then
  begin
    FSelection := FDiagram.Vertex(0).Reference;
  end;
  FProvider := TDiagramProjection.Create;
  FProviderLease := FProvider;
  FProvider.Plan := DrawNyxDiagramSelection(FDiagram, FSelection);
  FProviderName := NyxDrawingProvider(AEditor.ID + '-diagram-provider');
  FItem := NyxCanvasItem('Diagram picture');
  FLayer := NyxCanvasLayer('Diagram');
  FPicture := NewNyxDrawing(AEditor.ID + '-diagram-picture')
    .WithText('Editable diagram').WithPlan(FProvider.Plan).WithProvider(FProviderName);
  FPicture.Configure.HeightSizing(nsFill).MinimumHeight(0).Done;
  FScene := NewNyxCanvasScene;
  FScene.AddLayer(FLayer).AddControl(FItem,
    NyxCanvasPlacement(FLayer, FProvider.Plan.ViewBox), FPicture);
  FWorkspace := NewNyxCanvasWorkspace(AEditor.ID + '-diagram-viewport', FScene)
    .WithCamera(FOptions.Camera).WithGrid(FOptions.Grid);
  FWorkspace.Configure.MinimumHeight(0).Done;
  LToolbar := RetainNyxControl(FWorkspace.Node.Find(
    NyxCanvasWorkspacePart(FWorkspace.ID, ncwpToolbar).ID));
  LToolbar.Configure.Visible(False).Done;
  LSurface := RetainNyxControl(FWorkspace.Node.Find(
    NyxCanvasWorkspacePart(FWorkspace.ID, ncwpSurface).ID));
  LSurface.Configure.MinimumHeight(64).Done;
  FDocument := NewNyxCanvasRenderContext(ADocument, AEditor.Node, LRoot);
  FState := AState;
  FOwnState := AState = nil;

  if FOwnState then
  begin
    FState := FDocument.State.Clone;
  end;
  FRenderer := TDiagramRenderer.Create(ATheme);
  FScheduler := FRenderer.Events.Scheduler;
  FScheduler.RequireUI;
  FRenderer.Render(FDocument, LRoot, AHost, False, FState);
  {$ifdef PAS2JS}
  LHost := FRenderer.ElementFor(NyxDiagramEditorPart(AEditor.ID, ndepCanvasHost).ID);
  FCanvas := MountNyxBrowserCanvasWorkspace(FWorkspace, FDocument, LHost, FState,
    ATheme, SetupRenderer);
  {$else}
  LHost := TWinControl(FRenderer.ControlFor(NyxDiagramEditorPart(AEditor.ID, ndepCanvasHost).ID));
  FCanvas := MountNyxLCLCanvasWorkspace(FWorkspace, FDocument, LHost, FState,
    ATheme, SetupRenderer);
  {$endif}
  FConnected := True;
  LPolicy := NyxInteractionPolicy(FRenderer.Root);
  FCanvas.Constrain(LPolicy.Enabled, LPolicy.ReadOnly);
  FObserver := TDiagramEditorObserver.Create;
  FObserverLease := FObserver;
  FObserver.Owner := Self;

  if not Supports(FCanvas, INyxCanvasPresentationObservation, LObservation) then
  begin
    raise ENyxDiagram.Create('Diagram canvas lacks presentation observation');
  end;
  FObservation := LObservation.ObservePresentation(FObserverLease);
  RestoreProperties;
  ShowTools(FOptions.InitiallyExpanded);

  if FSelection.Defined then
  begin
    RevealSelection;
  end;
  for LPart := ndepPrevious to ndepToggleTools do
  begin
    Subscribe(FRenderer.Events, NyxDiagramEditorPart(AEditor.ID, LPart).ID, ntClick);
  end;
  for LPart := ndepLeft to ndepDown do
  begin
    Subscribe(FRenderer.Events, NyxDiagramEditorPart(AEditor.ID, LPart).ID, ntClick);
  end;
  Subscribe(FRenderer.Events, NyxDiagramEditorPart(AEditor.ID, ndepApply).ID, ntClick);
  Subscribe(FRenderer.Events, NyxDiagramEditorPart(AEditor.ID, ndepRestore).ID, ntClick);
  Subscribe(FRenderer.Events, NyxDiagramEditorPart(AEditor.ID, ndepSelection).ID, ntClick);
  Subscribe(FRenderer.Events, NyxDiagramEditorPart(AEditor.ID, ndepSelection).ID, ntBeforeKeyDown);
  BindPictureEvents;
end;

destructor TEditorView.Destroy;
begin
  Disconnect;
  inherited Destroy;
end;

procedure TEditorView.SetupRenderer(ARenderer: TDiagramRenderer);
begin
  ARenderer.RegisterDrawingProvider(FProviderName, FProviderLease);
end;

procedure TEditorView.BindPictureEvents;
var
  LEvents: INyxEvents;
begin

  if not GetConnected or (FCanvas.MountedCount = 0) then
  begin
    Exit;
  end;
  LEvents := FCanvas.EventsFor(FItem);

  if LEvents <> FPointerEvents then
  begin
    Subscribe(LEvents, FPicture.ID, ntPointerDown);
    FPointerEvents := LEvents;
  end;
end;

procedure TEditorView.Feedback(const AMessage: TNyxText);
var
  LCaption: INyxCaptionControl;
begin
  FScheduler.RequireUI;
  FLastError := AMessage;
  LCaption := RetainNyxControl(Part(ndepStatus)) as INyxCaptionControl;
  LCaption.Text := AMessage;

  if AMessage = '' then
  begin
    LCaption.Text := 'Ready';
  end;
  FRenderer.Sync;
end;

procedure TEditorView.RequireConnected;
begin

  if FScheduler <> nil then
  begin
    FScheduler.RequireUI;
  end;

  if not GetConnected then
  begin
    raise ENyxDiagram.Create('Diagram editor is disconnected');
  end;
end;

procedure TEditorView.RequireWritable;
begin
  RequireConnected;

  if FPublishing then
  begin
    raise ENyxDiagram.Create('Diagram publication is already reserved');
  end;
end;

function TEditorView.GetConnected: Boolean;
begin

  if FScheduler <> nil then
  begin
    FScheduler.RequireUI;
  end;
  Result := FConnected and (FCanvas <> nil) and FCanvas.Connected;
end;

function TEditorView.GetRenderer: TDiagramRenderer;
begin
  RequireConnected;
  Result := FRenderer;
end;

function TEditorView.GetDiagram: INyxDiagram;
begin
  RequireConnected;
  Result := FDiagram;
end;

function TEditorView.GetRevision: Integer;
begin
  RequireConnected;
  Result := FRevision;
end;

function TEditorView.GetSelection: TNyxDiagramVertexRef;
begin
  RequireConnected;
  Result := FSelection;
end;

function TEditorView.GetCanvas: INyxCanvasWorkspaceView;
begin
  RequireConnected;
  Result := FCanvas;
end;

function TEditorView.Part(APart: TNyxDiagramEditorPart): TNyxNode;
begin
  Result := FRenderer.Root.Find(NyxDiagramEditorPart(FEditorID, APart).ID);

  if Result = nil then
  begin
    raise ENyxDiagram.Create('Diagram editor part is missing');
  end;
end;

function TEditorView.Field(APart: TNyxDiagramEditorPart): INyxTextInput;
var
  LControl: INyxControl;
begin
  LControl := RetainNyxControl(Part(APart));

  if not Supports(LControl, INyxTextInput, Result) then
  begin
    raise ENyxDiagram.Create('Diagram geometry field changed its contract');
  end;
end;

function TEditorView.PropertyBuffer: TNyxText;
begin
  Result := Field(ndepX).Value + #0 + Field(ndepY).Value + #0 +
    Field(ndepWidth).Value + #0 + Field(ndepHeight).Value + #0 + Field(ndepCaption).Value;
end;

function TEditorView.GetHasPendingProperties: Boolean;
begin
  RequireConnected;
  Result := PropertyBuffer <> FFormBaseline;
end;

function TEditorView.GetToolsExpanded: Boolean;
begin
  RequireConnected;
  Result := FToolsExpanded;
end;

function TEditorView.GetLastError: TNyxText;
begin
  RequireConnected;
  Result := FLastError;
end;

procedure TEditorView.RefreshProperties;
var
  LVertex: TNyxDiagramVertex;
  LCaption: INyxCaptionControl;
begin

  if FSelection.Defined then
  begin
    LVertex := FDiagram.FindVertex(FSelection);
    Field(ndepX).Value := NyxData(LVertex.Bounds.Left).ToJSON;
    Field(ndepY).Value := NyxData(LVertex.Bounds.Top).ToJSON;
    Field(ndepWidth).Value := NyxData(LVertex.Bounds.Width).ToJSON;
    Field(ndepHeight).Value := NyxData(LVertex.Bounds.Height).ToJSON;
    Field(ndepCaption).Value := LVertex.Caption;
    LCaption := RetainNyxControl(Part(ndepSelection)) as INyxCaptionControl;
    LCaption.Text := 'Selected / ' + FSelection.Name;
  end
  else
  begin
    Field(ndepX).Value := '';
    Field(ndepY).Value := '';
    Field(ndepWidth).Value := '';
    Field(ndepHeight).Value := '';
    Field(ndepCaption).Value := '';
    LCaption := RetainNyxControl(Part(ndepSelection)) as INyxCaptionControl;
    LCaption.Text := 'No shape selected';
  end;
  FFormBaseline := PropertyBuffer;
  FRenderer.Sync;
end;

procedure TEditorView.RestoreProperties;
begin
  RequireWritable;
  RefreshProperties;
end;

procedure TEditorView.SyncPicture;
begin

  if GetConnected and (FCanvas.MountedCount > 0) then
  begin
    FCanvas.RendererFor(FItem).Sync;
  end;
end;

procedure TEditorView.SelectVertex(const AReference: TNyxDiagramVertexRef;
  ADiscardProperties: Boolean);
var
  LProjection: INyxDrawingPlan;
begin
  RequireWritable;

  if (AReference.Name = FSelection.Name) then
  begin
    Exit;
  end;

  if GetHasPendingProperties and not ADiscardProperties then
  begin
    raise ENyxDiagram.Create('Apply or revert unfinished geometry before changing selection');
  end;

  if AReference.Defined then
  begin
    FDiagram.FindVertex(AReference);
  end;
  LProjection := DrawNyxDiagramSelection(FDiagram, AReference);
  FSelection := AReference;
  FProvider.Plan := LProjection;
  SyncPicture;
  RefreshProperties;
end;

procedure TEditorView.SelectAt(const AWorldPoint: TNyxCanvasPoint;
  ADiscardProperties: Boolean);
begin
  RequireWritable;
  SelectVertex(FDiagram.PickVertex(AWorldPoint), ADiscardProperties);
end;

procedure TEditorView.ShowTools(AExpanded: Boolean);
begin
  RequireWritable;
  RetainNyxControl(Part(ndepTools)).Configure.Visible(AExpanded).Done;
  FToolsExpanded := AExpanded;
  FRenderer.Sync;
end;

procedure TEditorView.RevealSelection;
begin
  RequireWritable;

  if FSelection.Defined then
  begin
    FCanvas.SetCamera(FCanvas.Camera.AtCenter(FDiagram.FindVertex(FSelection).Bounds.Center));
  end;
end;

procedure TEditorView.AdvanceSelection(ADelta: Integer);
var
  LIndex: Integer;
  LSelected: Integer;
begin
  RequireWritable;

  if FDiagram.VertexCount = 0 then
  begin
    Exit;
  end;
  LSelected := 0;
  for LIndex := 0 to FDiagram.VertexCount - 1 do
  begin

    if FDiagram.Vertex(LIndex).Reference.Name = FSelection.Name then
    begin
      LSelected := LIndex;
      Break;
    end;
  end;
  LSelected := (LSelected + ADelta + FDiagram.VertexCount) mod FDiagram.VertexCount;
  SelectVertex(FDiagram.Vertex(LSelected).Reference, False);
  RevealSelection;
end;

procedure TEditorView.Publish(const ADiagram: INyxDiagram; AExpectedRevision: Integer);
var
  LCandidate: INyxDiagram;
  LProjection: INyxDrawingPlan;
  LSelection: TNyxDiagramVertexRef;
  LIndex: Integer;
  LFound: Boolean;
  LLayouts: INyxCanvasSceneLayouts;
  LView: INyxCanvasCoordinatedLayoutView;
  LPresentation: INyxCanvasLayoutView;
  LPlan: INyxCanvasLayoutPlan;
  LCommit: TDiagramEditorCommit;
  LPublication: INyxPreparedPublication;
  LLease: INyxDiagramEditorView;
  LFrame: TNyxCanvasControlFrame;
  LPropertyBuffer: TNyxText;
begin
  RequireWritable;

  if (AExpectedRevision <> FRevision) or (FRevision = High(Integer)) then
  begin
    raise ENyxDiagram.Create('Diagram revision is stale or exhausted');
  end;

  if ADiagram = nil then
  begin
    raise ENyxDiagram.Create('Diagram candidate is absent');
  end;
  LLease := Self;
  LPropertyBuffer := PropertyBuffer;
  FPublishing := True;
  try
    LCandidate := NyxDiagramFromData(ADiagram.ToData);
    { ToData belongs to a caller implementation and may run arbitrary UI code.
      Revocation or freshly typed properties must win over the older proposal,
      including a proposal that explicitly discarded the fields seen on entry. }
    RequireConnected;

    if (FRevision <> AExpectedRevision) or (PropertyBuffer <> LPropertyBuffer) then
    begin
      raise ENyxDiagram.Create('Diagram admission changed while its snapshot was captured');
    end;
    LSelection := FSelection;
    LFound := False;
    for LIndex := 0 to LCandidate.VertexCount - 1 do
    begin
      LFound := LFound or (LCandidate.Vertex(LIndex).Reference.Name = LSelection.Name);
    end;

    if not LFound then
    begin
      LSelection := Default(TNyxDiagramVertexRef);

      if LCandidate.VertexCount > 0 then
      begin
        LSelection := LCandidate.Vertex(0).Reference;
      end;
    end;
    LProjection := DrawNyxDiagramSelection(LCandidate, LSelection);
    LFrame := NyxCanvasControlFrame(FCanvas.Camera, LProjection.ViewBox, nccsReflow);
    NyxDrawingMapping(LProjection, LFrame.Width, LFrame.Height);

    if not Supports(FScene, INyxCanvasSceneLayouts, LLayouts) or
      not Supports(FCanvas, INyxCanvasCoordinatedLayoutView, LView) or
      not Supports(FCanvas, INyxCanvasLayoutView, LPresentation) then
    begin
      raise ENyxDiagram.Create('Diagram host lacks prepared canvas publication');
    end;
    LPlan := LLayouts.PrepareLayout([NyxCanvasPlace(FItem,
      NyxCanvasPlacement(FLayer, LProjection.ViewBox))], FScene.Revision);
    LCommit := TDiagramEditorCommit.Create;
    LPublication := LCommit;
    LCommit.Owner := Self;
    LCommit.Candidate := LCandidate;
    LCommit.Projection := LProjection;
    LCommit.Selection := LSelection;
    LCommit.Expected := FRevision;
    try

      if not LView.ApplyCoordinatedLayout(LPlan, FCanvas.Camera,
        LPresentation.PresentationRevision, LPublication) then
      begin
        raise ENyxDiagram.Create('Diagram presentation was not available for publication');
      end;
    finally
      LPublication.Retire;

      if LCommit.Installed and GetConnected then
      begin
        SyncPicture;
        RefreshProperties;
      end;
    end;
  finally
    FPublishing := False;
  end;
end;

procedure TEditorView.ReplaceDiagram(const ADiagram: INyxDiagram; AExpectedRevision: Integer;
  ADiscardProperties: Boolean);
begin
  RequireWritable;

  if GetHasPendingProperties and not ADiscardProperties then
  begin
    raise ENyxDiagram.Create('Apply or revert unfinished geometry before replacing the diagram');
  end;
  Publish(ADiagram, AExpectedRevision);
end;

procedure TEditorView.SetGeometry(const AReference: TNyxDiagramVertexRef;
  const ABounds: TNyxCanvasBounds; AExpectedRevision: Integer);
begin
  RequireWritable;

  if GetHasPendingProperties then
  begin
    raise ENyxDiagram.Create('Apply or revert unfinished geometry before moving a shape');
  end;
  Publish(FDiagram.WithVertex(FDiagram.FindVertex(AReference).AtBounds(ABounds)), AExpectedRevision);
end;

procedure TEditorView.NudgeSelection(AX, AY: Double; AExpectedRevision: Integer);
var
  LVertex: TNyxDiagramVertex;
begin
  RequireWritable;

  if not FSelection.Defined then
  begin
    raise ENyxDiagram.Create('Select a shape before moving it');
  end;
  LVertex := FDiagram.FindVertex(FSelection);
  SetGeometry(FSelection, NyxCanvasBounds(LVertex.Bounds.Left + AX, LVertex.Bounds.Top + AY,
    LVertex.Bounds.Width, LVertex.Bounds.Height), AExpectedRevision);
end;

function GeometryNumber(const AText: TNyxText): Double;
begin

  if not TryNyxStateNumber(AText, Result) then
  begin
    raise ENyxDiagram.Create('Geometry fields require finite numbers');
  end;
end;

procedure TEditorView.ApplyProperties;
var
  LVertex: TNyxDiagramVertex;
  LCandidate: INyxDiagram;
begin
  RequireWritable;

  if not FSelection.Defined then
  begin
    raise ENyxDiagram.Create('Select a shape before applying properties');
  end;
  LVertex := FDiagram.FindVertex(FSelection)
    .AtBounds(NyxCanvasBounds(GeometryNumber(Field(ndepX).Value),
      GeometryNumber(Field(ndepY).Value), GeometryNumber(Field(ndepWidth).Value),
      GeometryNumber(Field(ndepHeight).Value)))
    .Captioned(Field(ndepCaption).Value);
  LCandidate := FDiagram.WithVertex(LVertex);
  Publish(LCandidate, FRevision);
end;

procedure TEditorView.Subscribe(const AEvents: INyxEvents; const AID: TNyxText;
  ATrigger: TNyxTrigger);
var
  LCallback: TDiagramEditorCallback;
  LIndex: Integer;
begin
  LCallback := TDiagramEditorCallback.Create;
  LCallback.Scheduler := FScheduler;
  LCallback.Owner := Self;
  LIndex := Length(FCallbacks);
  SetLength(FCallbacks, LIndex + 1);
  SetLength(FCallbackLeases, LIndex + 1);
  SetLength(FSubscriptions, LIndex + 1);
  FCallbackLeases[LIndex] := LCallback;
  FCallbacks[LIndex] := LCallback;
  FSubscriptions[LIndex] := AEvents.On(NyxControlEvents(AID), ATrigger).Subscribe(LCallback);
end;

function TEditorView.PointerWorld(const AEvent: TNyxEventInfo): TNyxCanvasPoint;
var
  LMapping: TNyxDrawingMapping;
  {$ifdef PAS2JS}
  LFace: TJSHTMLElement;
  {$else}
  LFace: TControl;
  {$endif}
begin
  {$ifdef PAS2JS}
  LFace := FCanvas.RendererFor(FItem).ElementFor(FPicture.ID);
  LMapping := NyxDrawingMapping(FProvider.Plan, LFace.clientWidth, LFace.clientHeight);
  {$else}
  LFace := FCanvas.RendererFor(FItem).ControlFor(FPicture.ID);
  LMapping := NyxDrawingMapping(FProvider.Plan, LFace.Width, LFace.Height);
  {$endif}

  if not AEvent.Pointer.HasPosition or (LMapping.Scale <= 0) then
  begin
    raise ENyxDiagram.Create('Diagram pointer has no allocated local position');
  end;
  Result := NyxCanvasPoint(LMapping.ViewBox.Left +
    (AEvent.Pointer.X - LMapping.Left) / LMapping.Scale,
    LMapping.ViewBox.Top + (AEvent.Pointer.Y - LMapping.Top) / LMapping.Scale);
end;

procedure TEditorView.Event(const AEvent: TNyxEventInfo; const AExecution: INyxExecution);
var
  LPart: TNyxDiagramEditorPart;
  LDistance: Double;
  LResponse: INyxEventResponse;
  LHandled: Boolean;
  LPolicy: TNyxInteractionPolicy;
begin
  RequireWritable;
  LPolicy := NyxInteractionPolicy(FRenderer.Root);

  if not LPolicy.CanIssueCommand then
  begin
    Exit;
  end;
  LDistance := FOptions.NudgeDistance;

  if AEvent.HasPointer and (AEvent.Trigger = ntPointerDown) and
    (AEvent.Pointer.Button = npbPrimary) then
  begin
    SelectAt(PointerWorld(AEvent), False);
    Feedback('');
    Exit;
  end;

  if AEvent.HasKeyboard and (AEvent.Trigger = ntBeforeKeyDown) then
  begin

    if (AEvent.Keyboard.Modifiers <> []) or LPolicy.ReadOnly then
    begin
      Exit;
    end;
    LHandled := AEvent.Keyboard.Key in [nkLeftKey, nkUpKey, nkRightKey, nkDownKey];

    if not LHandled then
    begin
      Exit;
    end;
    { Reserve the recognized key's default response even when draft/geometry
      admission subsequently refuses; a failed nudge must not scroll the page. }
    LResponse := NyxEventResponse(AExecution);
    LResponse.Consume;
    case AEvent.Keyboard.Key of
      nkLeftKey: NudgeSelection(-LDistance, 0, FRevision);
      nkUpKey: NudgeSelection(0, -LDistance, FRevision);
      nkRightKey: NudgeSelection(LDistance, 0, FRevision);
      nkDownKey: NudgeSelection(0, LDistance, FRevision);
    else
      Exit;
    end;
    Feedback('');
    Exit;
  end;
  for LPart := Low(TNyxDiagramEditorPart) to High(TNyxDiagramEditorPart) do
  begin

    if AEvent.OriginID <> NyxDiagramEditorPart(FEditorID, LPart).ID then
    begin
      Continue;
    end;

    if LPolicy.ReadOnly and (LPart in [ndepLeft, ndepUp, ndepRight,
      ndepDown, ndepApply, ndepRestore]) then
    begin
      Exit;
    end;
    case LPart of
      ndepPrevious: AdvanceSelection(-1);
      ndepNext: AdvanceSelection(1);
      ndepZoomOut: FCanvas.Execute(nccZoomOut);
      ndepZoomIn: FCanvas.Execute(nccZoomIn);
      ndepFit: FCanvas.Execute(nccFit);
      ndepActualSize:
        begin
          FCanvas.ZoomAt(1, NyxCanvasPoint(FCanvas.Camera.ViewportWidth / 2,
            FCanvas.Camera.ViewportHeight / 2));
          RevealSelection;
        end;
      ndepToggleTools: ShowTools(not FToolsExpanded);
      ndepLeft: NudgeSelection(-LDistance, 0, FRevision);
      ndepUp: NudgeSelection(0, -LDistance, FRevision);
      ndepRight: NudgeSelection(LDistance, 0, FRevision);
      ndepDown: NudgeSelection(0, LDistance, FRevision);
      ndepApply: ApplyProperties;
      ndepRestore: RestoreProperties;
      ndepSelection: RevealSelection;
    else
      Exit;
    end;
    Feedback('');
    Exit;
  end;
end;

procedure TEditorView.Disconnect;
var
  LIndex: Integer;
begin

  if (FRenderer = nil) and (FCanvas = nil) and (FDocument = nil) then
  begin
    Exit;
  end;

  if FScheduler <> nil then
  begin
    FScheduler.RequireUI;
  end;
  FConnected := False;

  if FObserver <> nil then
  begin
    FObserver.Owner := nil;
  end;

  if FObservation <> nil then
  begin
    FObservation.Cancel;
    FObservation := nil;
  end;
  FObserverLease := nil;
  FObserver := nil;
  for LIndex := 0 to High(FCallbacks) do
  begin

    if FCallbacks[LIndex] <> nil then
    begin
      FCallbacks[LIndex].Owner := nil;
    end;
  end;
  for LIndex := 0 to High(FSubscriptions) do
  begin

    if FSubscriptions[LIndex] <> nil then
    begin
      FSubscriptions[LIndex].Cancel;
    end;
  end;
  FCallbacks := nil;
  FSubscriptions := nil;
  FCallbackLeases := nil;
  FPointerEvents := nil;

  if FCanvas <> nil then
  begin
    FCanvas.Disconnect;
    FCanvas := nil;
  end;
  FreeAndNil(FRenderer);
  FWorkspace := nil;
  FScene := nil;
  FPicture := nil;
  FProvider := nil;
  FProviderLease := nil;
  FDiagram := nil;
  FreeAndNil(FDocument);

  if FOwnState then
  begin
    FreeAndNil(FState);
  end;
end;
