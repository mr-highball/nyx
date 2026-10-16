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

type
  { Forward reference permits a native parent-owned host to revoke its borrowed
    face before LCL destroys the host's children. The face owns its renderer. }
  TConnectorFace = class;

  { Managed presentation receiver borrows its Tool Canvas owner. Disconnect
    clears Owner before cancelling the scope; queued work uses a separate
    revocable content-work port and never retains this view through the canvas. }
  TConnectorPresentationObserver = class(TInterfacedObject, INyxCanvasPresentationObserver)
  public
    Owner: TToolView;
    procedure Presented(ARevision: Integer);
  end;

  { One physical port callback borrows its face/view only while the subscription
    is active. The face's retained subscription owns this callback; Invoke takes
    a managed view lease before entering any user-extensible publication. }
  TConnectorGestureCallback = class(TNyxEventCallback)
  public
    Owner: TToolView;
    Face: TConnectorFace;
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution); override;
  end;

  {$ifndef PAS2JS}
  { LCL owns this scroll host through the canvas surface. Parent destruction
    revokes/freezes the face's renderer while its child controls are still alive,
    then clears the face's borrowed host pointer. Ordinary face disposal performs
    the same order; no raw pointer survives a parent-driven native teardown. }
  TConnectorHost = class(TScrollBox)
  public
    Face: TConnectorFace;
    destructor Destroy; override;
  end;

  { Nonwindowed transparent decoration reuses Nyx's display-list painter. It
    owns copied plan/theme and borrows its Tool Canvas receiver solely to revoke
    that pointer during parent-driven destruction. Disabled hit testing leaves
    normal canvas and embedded-control input available beneath the curves. }
  TConnectorDrawing = class(TGraphicControl)
  private
    FPlan: INyxDrawingPlan;
    FTheme: TNyxTheme;
  protected
    procedure Paint; override;
  public
    OwnerView: TToolView;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Configure(const APlan: INyxDrawingPlan; ATheme: TNyxTheme);
  end;
  {$endif}

  { One independent ordinary Nyx face. Its source root belongs to the Tool
    Canvas's private display document; the renderer owns an independent realized
    copy. Gesture subscriptions keep their raw callback entries alive until
    disposal revokes them. NodeIndex -1 identifies control/combined faces; a
    nonnegative value belongs to a captured named graph step. Native parent
    ownership is explicitly coordinated. }
  TConnectorFace = class
  public
    Control: TNyxControlRef;
    NodeIndex: Integer;
    Bounds: TNyxCanvasBounds;
    Flow: TNyxFlowRef;
    Step: TNyxFlowStepRef;
    Root: INyxNode;
    Renderer: {$ifdef PAS2JS}TNyxBrowserRenderer{$else}TNyxLCLRenderer{$endif};
    Host: {$ifdef PAS2JS}TJSHTMLElement{$else}TConnectorHost{$endif};
    Callbacks: array of TNyxEventCallback;
    Subscriptions: array of INyxEventSubscription;
    destructor Destroy; override;
  end;

procedure TConnectorPresentationObserver.Presented(ARevision: Integer);
var
  LOwner: TToolView;
  LLease: INyxToolCanvasView;
begin
  LOwner := Owner;

  if LOwner = nil then
  begin
    Exit;
  end;
  LLease := LOwner;
  LOwner.QueueConnectors(False);
end;

procedure TConnectorGestureCallback.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
var
  LOwner: TToolView;
  LLease: INyxToolCanvasView;
begin
  LOwner := Owner;

  if (LOwner = nil) or (Face = nil) then
  begin
    Exit;
  end;
  LLease := LOwner;
  LOwner.ConnectorGesture(Face, AEvent, AExecution);
end;

{$ifndef PAS2JS}
destructor TConnectorHost.Destroy;
var
  LFace: TConnectorFace;
begin
  LFace := Face;
  Face := nil;

  if LFace <> nil then
  begin
    LFace.Host := nil;
    FreeAndNil(LFace.Renderer);
  end;
  inherited Destroy;
end;

constructor TConnectorDrawing.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csOpaque];
  Enabled := False;
end;

destructor TConnectorDrawing.Destroy;
begin

  if OwnerView <> nil then
  begin
    OwnerView.FConnectorDrawing := nil;
    OwnerView := nil;
  end;
  FPlan := nil;
  FTheme.Free;
  inherited Destroy;
end;

procedure TConnectorDrawing.Configure(const APlan: INyxDrawingPlan; ATheme: TNyxTheme);
var
  LTheme: TNyxTheme;
begin
  LTheme := NewNyxDocumentTheme(nil, ATheme);
  FTheme.Free;
  FTheme := LTheme;
  FPlan := APlan;
  Invalidate;
end;

procedure TConnectorDrawing.Paint;
begin

  if (FPlan <> nil) and (Width > 0) and (Height > 0) then
  begin
    PaintNyxLCLDrawing(Canvas, FPlan, NyxDrawingMapping(FPlan, Width, Height), FTheme);
  end;
end;
{$endif}

destructor TConnectorFace.Destroy;
var
  LIndex: Integer;
begin
  for LIndex := 0 to High(Callbacks) do
  begin

    if Callbacks[LIndex] <> nil then
    begin
      TConnectorGestureCallback(Callbacks[LIndex]).Owner := nil;
      TConnectorGestureCallback(Callbacks[LIndex]).Face := nil;
    end;
  end;
  for LIndex := 0 to High(Subscriptions) do
  begin

    if Subscriptions[LIndex] <> nil then
    begin
      Subscriptions[LIndex].Cancel;
    end;
  end;
  Subscriptions := nil;
  Callbacks := nil;
  FreeAndNil(Renderer);
  {$ifdef PAS2JS}

  if Host <> nil then
  begin
    Host.remove;
    Host := nil;
  end;
  {$else}
  FreeAndNil(Host);
  {$endif}
  Root := nil;
  inherited Destroy;
end;

procedure TToolView.InitializeConnectors(const ACanvas: INyxCanvasWorkspaceView;
  ADocument: TNyxDocument; ATheme: TNyxTheme);
var
  LCanvas: {$ifdef PAS2JS}INyxBrowserCanvasWorkspaceView{$else}INyxLCLCanvasWorkspaceView{$endif};
  LGeometry: INyxCanvasControlGeometry;
  LObservation: INyxCanvasPresentationObservation;
  LObserver: TConnectorPresentationObserver;
begin

  if not Supports(ACanvas,
    {$ifdef PAS2JS}INyxBrowserCanvasWorkspaceView{$else}INyxLCLCanvasWorkspaceView{$endif}, LCanvas) or
    not Supports(ACanvas, INyxCanvasControlGeometry, LGeometry) or
    not Supports(ACanvas, INyxCanvasPresentationObservation, LObservation) then
  begin
    Exit;
  end;
  FCanvas := LCanvas;
  { Retain immutable scheduler identity until final view release, so worker
    rejection never races disposal of a borrowed renderer/event router. }
  FConnectorScheduler := FRenderer.Events.Scheduler;
  FConnectorGeometry := LGeometry;
  FConnectorTheme := NewNyxDocumentTheme(ADocument, ATheme);
  LObserver := TConnectorPresentationObserver.Create;
  FConnectorObserverLease := LObserver;
  FConnectorObserver := LObserver;
  LObserver.Owner := Self;
  FConnectorScope := LObservation.ObservePresentation(LObserver);
end;

procedure TToolView.DisconnectConnectors;
begin

  if FConnectorObserver <> nil then
  begin
    TConnectorPresentationObserver(FConnectorObserver).Owner := nil;
  end;

  if FConnectorScope <> nil then
  begin
    FConnectorScope.Cancel;
    FConnectorScope := nil;
  end;
  FConnectorObserver := nil;
  FConnectorObserverLease := nil;

  if FConnectorWork <> nil then
  begin
    FConnectorWork.Retire;
    FConnectorWork := nil;
  end;

  if FConnectorExecution <> nil then
  begin
    FConnectorExecution.Cancel;
    FConnectorExecution := nil;
  end;
  ClearConnectorFaces;
  FConnectorNodePositions := nil;
  FConnectorNodeMode := False;
  {$ifdef PAS2JS}
  FreeAndNil(FConnectorDrawing);

  if FConnectorLineHost <> nil then
  begin
    FConnectorLineHost.remove;
    FConnectorLineHost := nil;
  end;
  {$else}
  FreeAndNil(FConnectorDrawing);
  {$endif}
  FreeAndNil(FConnectorTheme);
  FConnectorGeometry := nil;
  FCanvas := nil;
end;

procedure TToolView.ClearConnectorFaces;
var
  LIndex: Integer;
begin
  ReleaseConnectorPointer;
  for LIndex := 0 to High(FConnectorFaces) do
  begin
    FConnectorFaces[LIndex].Free;
  end;
  FConnectorFaces := nil;
  FreeAndNil(FConnectorDocument);
  FConnectorRects := nil;
  FConnectorPanelBounds := Default(TNyxCanvasBounds);
  FConnectorController := nil;
  FConnectorNodes := nil;
end;

procedure TToolView.QueueConnectors(ARebuild: Boolean);
begin

  if not FConnected or (FCanvas = nil) or not FCanvas.Connected then
  begin
    Exit;
  end;
  FConnectorRebuild := FConnectorRebuild or ARebuild;

  if FConnectorWork = nil then
  begin
    FConnectorWork := NewNyxContentWork({$ifdef PAS2JS}@{$endif}RefreshConnectors);
  end;

  if (FConnectorExecution = nil) or
    not (FConnectorExecution.Status in [nesPending, nesRunning]) then
  begin
    FConnectorExecution := FRenderer.Events.Scheduler.Submit(FConnectorWork, neUIQueue);
  end;
end;

procedure TToolView.RefreshConnectors;
var
  LLease: INyxToolCanvasView;
  LMode: INyxToolCanvasControlConnectors;
  LController: INyxToolControlWires;
  LNodes: INyxToolPipelineNodes;
  LIndex: Integer;

  { Restore only an explicitly remembered connector after its face is rebuilt.
    Source controls keep their own renderer and focus; no host control is retained
    across retirement of the old connector face. }
  procedure RestoreFocus;
  var
    LFace: TConnectorFace;
    LIndex: Integer;
    {$ifdef PAS2JS}
    LControl: TJSHTMLElement;
    {$else}
    LControl: TControl;
    {$endif}
  begin

    if FConnectorRestoreFocus = '' then
    begin
      Exit;
    end;
    try
      for LIndex := 0 to High(FConnectorFaces) do
      begin
        LFace := TConnectorFace(FConnectorFaces[LIndex]);

        if (LFace.Renderer = nil) or
          (LFace.Renderer.Root.Find(FConnectorRestoreFocus) = nil) then
        begin
          Continue;
        end;
        {$ifdef PAS2JS}
        LControl := LFace.Renderer.ElementFor(FConnectorRestoreFocus, niRuntime);
        LControl.focus;
        {$else}
        LControl := LFace.Renderer.ControlFor(FConnectorRestoreFocus, niRuntime);

        if (LControl is TWinControl) and TWinControl(LControl).CanFocus then
        begin
          TWinControl(LControl).SetFocus;
        end;
        {$endif}
        Break;
      end;
    finally
      FConnectorRestoreFocus := '';
    end;
  end;

  procedure Add(const ARoot: INyxNode; const AControl: TNyxControlRef; ANodeIndex: Integer = -1);
  const
    CTriggers: array[0..5] of TNyxTrigger =
      (ntPointerDown, ntPointerMove, ntPointerUp, ntPointerCancel, ntPointerCaptureLost, ntKeyDown);
  var
    LFace: TConnectorFace;
    LCallback: TConnectorGestureCallback;
    LCallbackLease: INyxEventCallback;
    LSubscription: INyxEventSubscription;
    LPort: Integer;
    LTrigger: Integer;
    LCount: Integer;
    LNode: TNyxNode;
    LMessage: INyxLabel;
    LWidth: Double;
    LHeight: Double;
  begin
    LFace := TConnectorFace.Create;
    LCount := Length(FConnectorFaces);
    SetLength(FConnectorFaces, LCount + 1);
    FConnectorFaces[LCount] := LFace;
    LFace.Control := AControl;
    LFace.NodeIndex := ANodeIndex;

    if ANodeIndex >= 0 then
    begin
      LFace.Flow := LNodes.NodeFlow;
      LFace.Step := LNodes.NodeStep(ANodeIndex);
    end
    else if (AControl.ID = '') and (FConnectorNodeMessage <> '') then
    begin
      LMessage := NewNyxLabel(ARoot.Node.ID + '-node-message').WithText(FConnectorNodeMessage);
      ARoot.Node.Add(LMessage.Node);
    end;
    LFace.Root := ARoot;
    FConnectorDocument.AddComponent(ARoot);
    ConnectorFaceSize(LFace, LWidth, LHeight);
    {$ifdef PAS2JS}
    LFace.Host := TJSHTMLElement(document.createElement('div'));
    LFace.Host.style.setProperty('position', 'absolute');
    LFace.Host.style.setProperty('overflow', 'auto');
    LFace.Host.style.setProperty('z-index', '10000');
    LFace.Host.setAttribute('data-nyx-connectors', ARoot.Node.ID);
    LFace.Host.style.setProperty('width', IntToStr(Round(LWidth)) + 'px');
    LFace.Host.style.setProperty('height', IntToStr(Round(LHeight)) + 'px');
    FCanvas.Surface.appendChild(LFace.Host);
    LFace.Renderer := TNyxBrowserRenderer.Create(FConnectorTheme);
    LFace.Renderer.OnEvent := @ConnectorEvent;
    LFace.Renderer.Render(FConnectorDocument, ARoot.Node, LFace.Host, False, FState);
    {$else}
    LFace.Host := TConnectorHost.Create(nil);
    LFace.Host.Face := LFace;
    LFace.Host.Parent := FCanvas.Surface;
    LFace.Host.BorderStyle := bsNone;
    LFace.Host.AutoScroll := AControl.ID = '';
    LFace.Host.HorzScrollBar.Visible := ANodeIndex >= 0;

    if AControl.ID <> '' then
    begin
      { A compact control strip is sized to its explicit wrapping row. A native
        scroll bar would consume a column, wrap the last port and then clip it
        outside the strip. Only the larger pipeline face owns scrolling. }
      LFace.Host.VertScrollBar.Visible := False;
    end;
    { Admit the final viewport before native rendering. Shrinking a provisional
      wider root can leave automatic scrollbars feeding a second wrapped row. }
    LFace.Host.SetBounds(0, 0, Round(LWidth), Round(LHeight));
    LFace.Renderer := TNyxLCLRenderer.Create(FConnectorTheme);
    LFace.Renderer.OnEvent := ConnectorEvent;
    LFace.Renderer.Render(FConnectorDocument, ARoot.Node, LFace.Host, False, FState);
    {$endif}
    for LPort := 0 to LController.PortCount do
    begin

      if LPort = LController.PortCount then
      begin
        LNode := nil;

        if AControl.ID = '' then
        begin
          LNode := LFace.Renderer.Root.Part(NyxPart('move'));
        end;
      end
      else
      begin
        LNode := LFace.Renderer.Root.Find(LController.PortControl(LPort).ID);
      end;

      if LNode = nil then
      begin
        Continue;
      end;
      for LTrigger := 0 to High(CTriggers) do
      begin
        LCallback := TConnectorGestureCallback.Create;
        LCallbackLease := LCallback;
        LCallback.Owner := Self;
        LCallback.Face := LFace;
        LSubscription := LFace.Renderer.Events.On(
          NyxControlEvents(LNode.ID, niRuntime), CTriggers[LTrigger]).Subscribe(LCallbackLease);
        try
          LCount := Length(LFace.Subscriptions);
          SetLength(LFace.Subscriptions, LCount + 1);
          SetLength(LFace.Callbacks, LCount + 1);
          LFace.Subscriptions[LCount] := LSubscription;
          LFace.Callbacks[LCount] := LCallback;
        except
          LCallback.Owner := nil;
          LCallback.Face := nil;
          LSubscription.Cancel;
          raise;
        end;
      end;
    end;
  end;

begin

  if not FConnected or FConnectorUpdating or (FCanvas = nil) or not FCanvas.Connected then
  begin
    Exit;
  end;
  LLease := Self;

  if not Supports(FInspector, INyxToolCanvasControlConnectors, LMode) or
    not LMode.ControlConnectorsVisible then
  begin
    ClearConnectorFaces;

    if FConnectorDrawing <> nil then
    begin
      {$ifdef PAS2JS}FConnectorLineHost.style.setProperty('display', 'none');{$else}FConnectorDrawing.Visible := False;{$endif}
    end;
    Exit;
  end;
  LController := LMode.ControlWires;
  LNodes := nil;

  if FConnectorNodeMode and Supports(LController, INyxToolPipelineNodes, LNodes) then
  begin
    try
      LNodes.ValidateFaces(FConnectorNodeMaximum, FConnectorNodePortMaximum);
    except
      on E: ENyxFlow do
      begin
        { A newly selected graph may exceed the chosen presentation budget.
          Preserve its complete metadata and display the combined panel, with
          an explicit diagnostic rather than truncating individual node ports. }
        FConnectorNodeMode := False;
        FConnectorNodeMessage := E.Message;
      end;
    end;
  end;
  FConnectorUpdating := True;
  try

    if FConnectorRebuild or (FConnectorController <> LController) or
      (Length(FConnectorFaces) = 0) then
    begin
      ClearConnectorFaces;
      FConnectorController := LController;
      FConnectorNodes := LNodes;
      FConnectorDocument := TNyxDocument.Create;
      try
        for LIndex := 0 to LController.ControlCount - 1 do
        begin
          Add(LController.BuildControl(LController.Control(LIndex)), LController.Control(LIndex));
        end;
        if FConnectorNodeMode and (LNodes <> nil) then
        begin
          Add(LNodes.BuildNodeToolbar, NyxControl(''));
          for LIndex := 0 to LNodes.NodeCount - 1 do
          begin
            Add(LNodes.BuildNode(LIndex), NyxControl(''), LIndex);
          end;
        end
        else
        begin
          Add(LController.BuildPipeline, NyxControl(''));
        end;
      except
        ClearConnectorFaces;
        raise;
      end;
      FConnectorRebuild := False;
    end;
    PlaceConnectorFaces;
    PaintConnectors;
    RestoreFocus;
  finally
    FConnectorUpdating := False;
  end;
end;

function TToolView.ControlConnectorRenderer(const AControl: TNyxControlRef):
  {$ifdef PAS2JS}TNyxBrowserRenderer{$else}TNyxLCLRenderer{$endif};
var
  LIndex: Integer;
  LFace: TConnectorFace;
begin
  Result := nil;
  for LIndex := 0 to High(FConnectorFaces) do
  begin
    LFace := TConnectorFace(FConnectorFaces[LIndex]);

    if (LFace.NodeIndex < 0) and (LFace.Control.ID = AControl.ID) then
    begin
      Exit(LFace.Renderer);
    end;
  end;
  raise ENyxModel.Create('The requested control connector face is not mounted');
end;

function TToolView.PipelineConnectorRenderer:
  {$ifdef PAS2JS}TNyxBrowserRenderer{$else}TNyxLCLRenderer{$endif};
begin
  Result := ControlConnectorRenderer(NyxControl(''));
end;

function TToolView.GetConnectorExecution: INyxExecution;
begin
  Result := FConnectorExecution;
end;

procedure TToolView.ConnectorEvent(ANode: TNyxNode; const AEvent: TNyxEventInfo);
var
  LLease: INyxToolCanvasView;
  LIndex: Integer;
  LFace: TConnectorFace;
  LFocused: Boolean;
  LID: TNyxText;
  LNodes: INyxToolPipelineNodes;
begin

  if not FConnected or FConnectorUpdating or (FConnectorController = nil) then
  begin
    Exit;
  end;
  LLease := Self;
  LID := ANode.ID;

  if Supports(FConnectorController, INyxToolPipelineNodes, LNodes) and
    (LID = LNodes.ModeControl.ID) and (AEvent.Trigger = ntClick) then
  begin
    try
      ShowPipelineNodes(not FConnectorNodeMode);
    except
      on E: ENyxFlow do
      begin
        FConnectorNodeMessage := E.Message;
        QueueConnectors(True);
      end;
    end;
    Exit;
  end;
  LFocused := False;
  for LIndex := 0 to High(FConnectorFaces) do
  begin
    LFace := TConnectorFace(FConnectorFaces[LIndex]);

    if (LFace.Renderer = nil) or (LFace.Renderer.Root.Find(LID) = nil) then
    begin
      Continue;
    end;
    {$ifdef PAS2JS}
    LFocused := LFace.Renderer.ElementFor(LID, niRuntime).contains(document.activeElement);
    {$else}
    LFocused := LFace.Renderer.ControlFor(LID, niRuntime) = Screen.ActiveControl;
    {$endif}
    Break;
  end;

  if FConnectorController.Handle(ANode, AEvent) and FConnected then
  begin

    if LFocused then
    begin
      FConnectorRestoreFocus := LID;
    end;
    QueueConnectors(True);
  end;
end;

{$I nyx.tool.canvas.connectors.geometry.inc}
{$I nyx.tool.canvas.nodes.inc}
