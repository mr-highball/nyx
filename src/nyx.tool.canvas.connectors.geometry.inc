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

{ Floating chrome uses the same planned rectangle during capture and target
  placement. Configure may precede a deferred rebuild, so its former physical
  bounds cannot describe the next navigation frame. No position is mutated. }
function ConnectorPanelRectangle(const APosition: TNyxCanvasPoint;
  AViewportWidth, AViewportHeight, AWidth, AHeight: Double): TNyxCanvasBounds;
var
  LLeft: Double;
  LTop: Double;
begin
  LLeft := AViewportWidth - AWidth - 8;
  LTop := AViewportHeight - AHeight - 8;

  if APosition.Defined then
  begin
    LLeft := APosition.X;
    LTop := APosition.Y;
  end;
  Result := NyxCanvasBounds(Max(0, Min(LLeft, AViewportWidth - AWidth)),
    Max(0, Min(LTop, AViewportHeight - AHeight)), AWidth, AHeight);
end;

{ Adapter-only geometry uses actual outer faces and the canvas's logical
  viewport. It clips hit regions against both the canvas and a scrolling face;
  hidden/off-screen buttons never become synthetic pointer targets. }
function ConnectorBounds(AOwner: TToolView; AFace: TConnectorFace;
  const AID: TNyxText; AClip: Boolean; out ABounds: TNyxCanvasBounds): Boolean;
var
  LBounds: TNyxCanvasBounds;
  LClip: TNyxCanvasBounds;
  LLeft: Double;
  LTop: Double;
  LRight: Double;
  LBottom: Double;
  {$ifdef PAS2JS}
  LElement: TJSHTMLElement;
  LSurface: TJSDOMRect;
  LRect: TJSDOMRect;
  LHost: TJSDOMRect;
  {$else}
  LControl: TControl;
  LAncestor: TControl;
  LPosition: TPoint;
  {$endif}
begin
  ABounds := Default(TNyxCanvasBounds);
  Result := False;

  if (AFace = nil) or (AFace.Renderer = nil) or (AFace.Host = nil) then
  begin
    Exit;
  end;
  {$ifdef PAS2JS}

  if AFace.Host.style.getPropertyValue('display') = 'none' then
  begin
    Exit;
  end;
  try
    LElement := AFace.Renderer.ElementFor(AID, niRuntime);
  except
    on E: ENyxModel do
    begin
      Exit;
    end;
  end;
  LRect := LElement.getBoundingClientRect;
  LSurface := AOwner.FCanvas.Surface.getBoundingClientRect;
  LHost := AFace.Host.getBoundingClientRect;

  if (LRect.width <= 0) or (LRect.height <= 0) or
    (LSurface.width <= 0) or (LSurface.height <= 0) then
  begin
    Exit;
  end;
  LBounds := NyxCanvasBounds(
    (LRect.left - LSurface.left) * AOwner.FCanvas.Surface.clientWidth / LSurface.width,
    (LRect.top - LSurface.top) * AOwner.FCanvas.Surface.clientHeight / LSurface.height,
    LRect.width * AOwner.FCanvas.Surface.clientWidth / LSurface.width,
    LRect.height * AOwner.FCanvas.Surface.clientHeight / LSurface.height);
  LClip := NyxCanvasBounds(
    Max(0, (LHost.left - LSurface.left) * AOwner.FCanvas.Surface.clientWidth / LSurface.width),
    Max(0, (LHost.top - LSurface.top) * AOwner.FCanvas.Surface.clientHeight / LSurface.height),
    Max(0, Min(AOwner.FCanvas.Surface.clientWidth,
      (LHost.right - LSurface.left) * AOwner.FCanvas.Surface.clientWidth / LSurface.width) -
      Max(0, (LHost.left - LSurface.left) * AOwner.FCanvas.Surface.clientWidth / LSurface.width)),
    Max(0, Min(AOwner.FCanvas.Surface.clientHeight,
      (LHost.bottom - LSurface.top) * AOwner.FCanvas.Surface.clientHeight / LSurface.height) -
      Max(0, (LHost.top - LSurface.top) * AOwner.FCanvas.Surface.clientHeight / LSurface.height)));
  {$else}

  if not AFace.Host.Visible then
  begin
    Exit;
  end;
  try
    LControl := AFace.Renderer.ControlFor(AID, niRuntime);
  except
    on E: ENyxModel do
    begin
      Exit;
    end;
  end;
  LAncestor := LControl;
  while (LAncestor <> nil) and (LAncestor <> AOwner.FCanvas.Surface) do
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
  LPosition := AOwner.FCanvas.Surface.ScreenToClient(
    LControl.Parent.ClientToScreen(Types.Point(LControl.Left, LControl.Top)));
  LBounds := NyxCanvasBounds(LPosition.X, LPosition.Y, LControl.Width, LControl.Height);
  LLeft := Max(0, AFace.Host.Left);
  LTop := Max(0, AFace.Host.Top);
  LClip := NyxCanvasBounds(LLeft, LTop,
    Max(0, Min(AOwner.FCanvas.Surface.ClientWidth, AFace.Host.Left + AFace.Host.ClientWidth) - LLeft),
    Max(0, Min(AOwner.FCanvas.Surface.ClientHeight, AFace.Host.Top + AFace.Host.ClientHeight) - LTop));
  {$endif}

  if not AClip then
  begin
    ABounds := LBounds;
    Exit(True);
  end;
  LLeft := Max(LBounds.Left, LClip.Left);
  LTop := Max(LBounds.Top, LClip.Top);
  LRight := Min(LBounds.Left + LBounds.Width, LClip.Left + LClip.Width);
  LBottom := Min(LBounds.Top + LBounds.Height, LClip.Top + LClip.Height);
  Result := (LRight > LLeft) and (LBottom > LTop);

  if Result then
  begin
    ABounds := NyxCanvasBounds(LLeft, LTop, LRight - LLeft, LBottom - LTop);
  end;
end;

procedure TToolView.ConnectorFaceSize(AFace: TObject; out AWidth, AHeight: Double);
var
  LFace: TConnectorFace;
  LWidth: Double;
  LHeight: Double;
begin
  LFace := TConnectorFace(AFace);
  LWidth := FCanvas.Surface.{$ifdef PAS2JS}clientWidth{$else}ClientWidth{$endif};
  LHeight := FCanvas.Surface.{$ifdef PAS2JS}clientHeight{$else}ClientHeight{$endif};

  if LFace.NodeIndex >= 0 then
  begin

    if FConnectorScaling = nccsUniform then
    begin
      { A stable world allocation prevents viewport/zoom from reflowing the
        card. Its independent scroll host retains access to longer port lists. }
      AWidth := 280;
      AHeight := 320;
      Exit;
    end;
    AWidth := Min(280, Max(64, LWidth - 16));
    AHeight := Min(320, Max(128, LHeight - 16));

    if FConnectorNodeMode and not GetToolsExpanded then
    begin
      { Compact cards retain readable control/font sizes in a shorter scroll
        host, leaving room for the corner tools instead of shrinking text. }
      AHeight := Min(AHeight, Max(128, LHeight - 80));
    end;
  end
  else if LFace.Control.ID <> '' then
  begin
    AWidth := Max(1, Min(LWidth - 8, Min(240, Max(48, LFace.Root.Node.Count * 48))));
    AHeight := Max(32, Ceil(LFace.Root.Node.Count * 48 / Max(48, AWidth)) * 36);
  end
  else
  begin

    if FConnectorNodeMode and not GetToolsExpanded then
    begin
      AWidth := Min(88, Max(64, LWidth - 16));
      AHeight := 56;
    end
    else
    begin
      AWidth := Min(360, Max(64, LWidth - 16));
      AHeight := Min(240, Max(96, LHeight * 0.5));

      if FConnectorNodeMode then
      begin
        { Expanded operation tools are a temporary usable sheet on a compact
          surface. Navigation remains above scrolling connection details. }
        AHeight := Min(240, Max(96, LHeight - 16));
      end;
    end;
  end;
end;

{ Renderer pointers are logical to their own control. Connector rectangles and
  wire hit testing use viewport pixels. Use the last actually applied factor;
  the camera may already have queued a later presentation. }
function ConnectorViewportPointer(AFace: TConnectorFace;
  const ABounds: TNyxCanvasBounds; const AEvent: TNyxEventInfo): TNyxCanvasPoint;
begin
  Result := NyxCanvasPoint(ABounds.Left + AEvent.Pointer.X * AFace.ContentScale,
    ABounds.Top + AEvent.Pointer.Y * AFace.ContentScale);
end;

procedure TToolView.PlaceConnectorFaces;
var
  LIndex: Integer;
  LPort: Integer;
  LFace: TConnectorFace;
  LControlBounds: TNyxCanvasBounds;
  LWidth: Double;
  LHeight: Double;
  LLeft: Double;
  LTop: Double;
  LViewportWidth: Double;
  LViewportHeight: Double;
  LVisible: Boolean;
  LPosition: TNyxCanvasPoint;
  LNodePosition: Integer;
  LScale: Double;
  LPhysicalWidth: Double;
  LPhysicalHeight: Double;
begin
  LViewportWidth := FCanvas.Surface.{$ifdef PAS2JS}clientWidth{$else}ClientWidth{$endif};
  LViewportHeight := FCanvas.Surface.{$ifdef PAS2JS}clientHeight{$else}ClientHeight{$endif};

  if (LViewportWidth <= 0) or (LViewportHeight <= 0) then
  begin
    Exit;
  end;
  for LIndex := 0 to High(FConnectorFaces) do
  begin
    LFace := TConnectorFace(FConnectorFaces[LIndex]);

    if LFace.Host = nil then
    begin
      Continue;
    end;
    LVisible := True;
    ConnectorFaceSize(LFace, LWidth, LHeight);
    LScale := 1;

    if (LFace.NodeIndex >= 0) and (FConnectorScaling = nccsUniform) then
    begin
      LScale := FCanvas.Camera.Zoom;
    end;
    LPhysicalWidth := LWidth * LScale;
    LPhysicalHeight := LHeight * LScale;
    {$ifndef PAS2JS}
    LPhysicalWidth := Max(1, Round(LPhysicalWidth));
    LPhysicalHeight := Max(1, Round(LPhysicalHeight));
    {$endif}

    LFace.Bounds := Default(TNyxCanvasBounds);

    if LFace.NodeIndex >= 0 then
    begin
      LNodePosition := NodePosition(LFace.NodeIndex);
      LPosition := FCanvas.Camera.WorldToViewport(FConnectorNodePositions[LNodePosition].Position);
      LLeft := LPosition.X;
      LTop := LPosition.Y;
      LVisible := (LLeft < LViewportWidth) and (LTop < LViewportHeight) and
        (LLeft + LPhysicalWidth > 0) and (LTop + LPhysicalHeight > 0);

      if not LVisible then
      begin
        { Never convert enormous off-screen world coordinates to native widget
          integers. The independently stored world position remains unchanged. }
        LLeft := 0;
        LTop := 0;
      end;
    end
    else if LFace.Control.ID <> '' then
    begin
      LVisible := FConnectorGeometry.TryControlBounds(FConnectorController.Item,
        LFace.Control, LControlBounds);
      LLeft := 4;
      LTop := 4;

      if LVisible then
      begin
        LLeft := LControlBounds.Left + LControlBounds.Width + 6;

        if LLeft + LWidth > LViewportWidth - 4 then
        begin
          LLeft := LControlBounds.Left + LControlBounds.Width - LWidth;
        end;
        LTop := LControlBounds.Top;
      end;
    end
    else
    begin
      LControlBounds := ConnectorPanelRectangle(FConnectorPanelPosition,
        LViewportWidth, LViewportHeight, LWidth, LHeight);
      LLeft := LControlBounds.Left;
      LTop := LControlBounds.Top;
    end;
    if LFace.NodeIndex < 0 then
    begin
      LLeft := Max(0, Min(LLeft, LViewportWidth - LWidth));
      LTop := Max(0, Min(LTop, LViewportHeight - LHeight));
    end;

    if (LFace.NodeIndex < 0) and (LFace.Control.ID = '') then
    begin
      FConnectorPanelBounds := NyxCanvasBounds(LLeft, LTop, LWidth, LHeight);

      if FConnectorPanelPosition.Defined then
      begin
        FConnectorPanelPosition := NyxCanvasPoint(LLeft, LTop);
      end;
    end;
    {$ifdef PAS2JS}
    LFace.Host.style.setProperty('left', IntToStr(Round(LLeft)) + 'px');
    LFace.Host.style.setProperty('top', IntToStr(Round(LTop)) + 'px');
    LFace.Host.style.setProperty('width', IntToStr(Round(LWidth)) + 'px');
    LFace.Host.style.setProperty('height', IntToStr(Round(LHeight)) + 'px');

    if (LFace.NodeIndex >= 0) and (FConnectorScaling = nccsUniform) then
    begin
      LFace.Host.style.setProperty('transform-origin', '0 0');
      LFace.Host.style.setProperty('transform', 'scale(' + NyxStateNumberText(LScale) + ')');
    end
    else
    begin
      LFace.Host.style.removeProperty('transform');
      LFace.Host.style.removeProperty('transform-origin');
    end;
    LFace.Host.style.setProperty('display', 'block');
    LFace.Host.style.setProperty('z-index', '10');

    if (LFace.NodeIndex >= 0) and (LFace.Step.Name = FNavigationRevealedStep.Name) then
    begin
      LFace.Host.style.setProperty('z-index', '20');
    end;

    if (LFace.NodeIndex < 0) and (LFace.Control.ID = '') then
    begin
      LFace.Host.style.setProperty('z-index', '30');
    end;

    if not LVisible then
    begin
      LFace.Host.style.setProperty('display', 'none');
    end;
    {$else}
    LFace.Host.SetBounds(Round(LLeft), Round(LTop), Round(LPhysicalWidth), Round(LPhysicalHeight));

    if (LFace.NodeIndex >= 0) and (FConnectorScaling = nccsUniform) then
    begin
      LFace.Renderer.SetContentScale(Round(LWidth), Round(LHeight), LScale);
    end;
    LFace.Host.Visible := LVisible;
    LFace.Host.BringToFront;
    {$endif}
    LFace.ContentScale := LScale;

    if (LFace.NodeIndex >= 0) and LVisible then
    begin
      { Bounds become queryable only after this face's actual projection.
        A target exception leaves it undefined instead of advertising a frame
        whose descendant controls have not accepted the requested factor. }
      LFace.Bounds := NyxCanvasBounds(LLeft, LTop, LPhysicalWidth, LPhysicalHeight);
    end;
  end;
  {$ifndef PAS2JS}
  { A revealed card stays reachable even when readable logical cards cannot all
    fit without overlap. Floating tools retain their own foreground role. }
  for LIndex := 0 to High(FConnectorFaces) do
  begin
    LFace := TConnectorFace(FConnectorFaces[LIndex]);

    if (LFace.Host <> nil) and (LFace.NodeIndex >= 0) and
      (LFace.Step.Name = FNavigationRevealedStep.Name) then
    begin
      LFace.Host.BringToFront;
    end;
  end;
  for LIndex := 0 to High(FConnectorFaces) do
  begin
    LFace := TConnectorFace(FConnectorFaces[LIndex]);

    if (LFace.Host <> nil) and (LFace.NodeIndex < 0) and (LFace.Control.ID = '') then
    begin
      LFace.Host.BringToFront;
    end;
  end;
  {$endif}
  SetLength(FConnectorRects, FConnectorController.PortCount);
  for LPort := 0 to High(FConnectorRects) do
  begin
    FConnectorRects[LPort] := Default(TNyxCanvasBounds);
    for LIndex := 0 to High(FConnectorFaces) do
    begin
      LFace := TConnectorFace(FConnectorFaces[LIndex]);

      if ConnectorBounds(Self, LFace, FConnectorController.PortControl(LPort).ID,
        True, LControlBounds) then
      begin
        FConnectorRects[LPort] := LControlBounds;
        Break;
      end;
    end;
  end;
end;

procedure TToolView.PaintConnectors;
var
  LBuilder: INyxDrawingBuilder;
  LPlan: INyxDrawingPlan;
  LWire: TNyxToolWire;
  LIndex: Integer;
  LSource: TNyxCanvasPoint;
  LTarget: TNyxCanvasPoint;
  LWidth: Double;
  LHeight: Double;
  LNodeWire: TNyxToolPipelineWire;
  LSourceBounds: TNyxCanvasBounds;
  LTargetBounds: TNyxCanvasBounds;
  {$ifndef PAS2JS}LDrawing: TConnectorDrawing;{$endif}

  function Point(AX, AY: Double): TNyxCanvasPoint;
  begin
    Result := NyxCanvasPoint(Max(0, Min(LWidth, AX)), Max(0, Min(LHeight, AY)));
  end;

  function PortPoint(AIndex: Integer): TNyxCanvasPoint;
  var
    LBounds: TNyxCanvasBounds;
    LPort: TNyxToolWirePort;
    LNodeBounds: TNyxCanvasBounds;
  begin
    LBounds := FConnectorRects[AIndex];
    LPort := FConnectorController.Port(AIndex);

    if FConnectorNodeMode and (LPort.Kind = ntwpkPipeline) and
      NodeEndpointBounds(LPort.Endpoint, LNodeBounds) then
    begin
      LBounds := LNodeBounds;
    end;

    if FConnectorController.Port(AIndex).Direction = nfdOutput then
    begin
      Result := Point(LBounds.Left + LBounds.Width, LBounds.Top + LBounds.Height * 0.5);
    end
    else
    begin
      Result := Point(LBounds.Left, LBounds.Top + LBounds.Height * 0.5);
    end;
  end;

  procedure Curve(const ASource, ATarget: TNyxCanvasPoint; AArrow: Boolean);
  var
    LDistance: Double;
    LStyle: TNyxDrawingStyle;
  begin
    LDistance := Min(160, Max(32, Abs(ATarget.X - ASource.X) * 0.5));
    LStyle := NyxDrawingStyle.Stroke(NyxThemePaint(ntcAccent), 2);
    LBuilder.Path(NyxDrawingPath(ASource).CurveTo(
      Point(ASource.X + LDistance, ASource.Y),
      Point(ATarget.X - LDistance, ATarget.Y), ATarget), LStyle);

    if AArrow then
    begin
      LBuilder.Path(NyxDrawingPath(Point(ATarget.X - 6, ATarget.Y - 4))
        .LineTo(ATarget).LineTo(Point(ATarget.X - 6, ATarget.Y + 4)), LStyle);
    end;
  end;

begin

  if (FConnectorController = nil) or (Length(FConnectorFaces) = 0) then
  begin
    Exit;
  end;
  LWidth := FCanvas.Surface.{$ifdef PAS2JS}clientWidth{$else}ClientWidth{$endif};
  LHeight := FCanvas.Surface.{$ifdef PAS2JS}clientHeight{$else}ClientHeight{$endif};

  if (LWidth <= 0) or (LHeight <= 0) then
  begin
    Exit;
  end;
  LBuilder := NewNyxDrawingPlan(NyxCanvasBounds(0, 0, LWidth, LHeight));

  if FConnectorNodeMode and (FConnectorNodes <> nil) then
  begin
    for LIndex := 0 to FConnectorNodes.NodeWireCount - 1 do
    begin
      LNodeWire := FConnectorNodes.NodeWire(LIndex);

      if NodeEndpointBounds(LNodeWire.Source, LSourceBounds) and
        NodeEndpointBounds(LNodeWire.Target, LTargetBounds) then
      begin
        Curve(Point(LSourceBounds.Left + LSourceBounds.Width,
          LSourceBounds.Top + LSourceBounds.Height * 0.5),
          Point(LTargetBounds.Left, LTargetBounds.Top + LTargetBounds.Height * 0.5), True);
      end;
    end;
  end;
  for LIndex := 0 to FConnectorController.WireCount - 1 do
  begin
    LWire := FConnectorController.Wire(LIndex);

    if FConnectorRects[LWire.Source].Defined and FConnectorRects[LWire.Target].Defined then
    begin
      LSource := PortPoint(LWire.Source);
      LTarget := PortPoint(LWire.Target);
      Curve(LSource, LTarget, True);
    end;
  end;

  if FConnectorPointerActive and (FConnectorPointerSource >= 0) and
    FConnectorPointerMoved and FConnectorRects[FConnectorPointerSource].Defined then
  begin
    Curve(PortPoint(FConnectorPointerSource),
      Point(FConnectorPointerPosition.X, FConnectorPointerPosition.Y), False);
  end;
  LPlan := LBuilder.Build;
  {$ifdef PAS2JS}

  if FConnectorDrawing = nil then
  begin
    FConnectorLineHost := TJSHTMLElement(document.createElement('div'));
    FConnectorLineHost.style.setProperty('position', 'absolute');
    FConnectorLineHost.style.setProperty('left', '0');
    FConnectorLineHost.style.setProperty('top', '0');
    FConnectorLineHost.style.setProperty('pointer-events', 'none');
    FConnectorLineHost.setAttribute('aria-hidden', 'true');
    FCanvas.Surface.insertBefore(FConnectorLineHost, FCanvas.Surface.firstChild);
    FConnectorDrawing := TNyxBrowserDrawing.Create(FConnectorLineHost);
  end;
  FConnectorLineHost.style.setProperty('display', 'block');
  FConnectorLineHost.style.setProperty('width', IntToStr(Round(LWidth)) + 'px');
  FConnectorLineHost.style.setProperty('height', IntToStr(Round(LHeight)) + 'px');
  FConnectorDrawing.Configure(LPlan, nil, FConnectorTheme, 'Control connections');
  {$else}

  if FConnectorDrawing = nil then
  begin
    LDrawing := TConnectorDrawing.Create(nil);
    FConnectorDrawing := LDrawing;
    LDrawing.OwnerView := Self;
    LDrawing.Parent := FCanvas.Surface;
  end;
  FConnectorDrawing.SetBounds(0, 0, Round(LWidth), Round(LHeight));
  FConnectorDrawing.Visible := True;
  FConnectorDrawing.SendToBack;
  TConnectorDrawing(FConnectorDrawing).Configure(LPlan, FConnectorTheme);
  {$endif}
end;

procedure TToolView.MovePipeline(const APosition: TNyxCanvasPoint);
var
  LWidth: Double;
  LHeight: Double;
  LViewportWidth: Double;
  LViewportHeight: Double;
begin

  if not FConnected or (FCanvas = nil) or not FCanvas.Connected or not APosition.Defined then
  begin
    raise ENyxModel.Create('Pipeline placement requires a live tool and viewport point');
  end;
  LViewportWidth := FCanvas.Surface.{$ifdef PAS2JS}clientWidth{$else}ClientWidth{$endif};
  LViewportHeight := FCanvas.Surface.{$ifdef PAS2JS}clientHeight{$else}ClientHeight{$endif};
  LWidth := Min(360, Max(64, LViewportWidth - 16));
  LHeight := Min(240, Max(96, LViewportHeight * 0.5));
  FConnectorPanelPosition := NyxCanvasPoint(
    Max(0, Min(APosition.X, LViewportWidth - LWidth)),
    Max(0, Min(APosition.Y, LViewportHeight - LHeight)));
  QueueConnectors(False);
end;

procedure TToolView.ResetPipelinePosition;
begin

  if not FConnected then
  begin
    raise ENyxModel.Create('Pipeline placement owner is retired');
  end;
  FConnectorPanelPosition := Default(TNyxCanvasPoint);
  QueueConnectors(False);
end;

function TToolView.GetPipelineBounds: TNyxCanvasBounds;
begin
  Result := FConnectorPanelBounds;
end;

procedure TToolView.ReleaseConnectorPointer;
var
  LIndex: Integer;
  LFace: TConnectorFace;
  LNode: TNyxNode;
  LID: TNyxText;
  {$ifdef PAS2JS}
  LElement: TJSHTMLElement;
  {$else}
  LControl: TControl;
  {$endif}
begin

  if not FConnectorPointerActive then
  begin
    Exit;
  end;
  FConnectorPointerActive := False;
  { Capture retirement must not call the controller: its application or an
    explicit creator call may already have disconnected that independent owner. }
  LID := FConnectorPointerOrigin;
  FConnectorPointerOrigin := '';
  for LIndex := 0 to High(FConnectorFaces) do
  begin
    LFace := TConnectorFace(FConnectorFaces[LIndex]);

    if LFace.Renderer = nil then
    begin
      Continue;
    end;
    LNode := LFace.Renderer.Root.Find(LID);

    if (FConnectorPointerSource = -2) and (LFace.NodeIndex < 0) and
      (LFace.Control.ID = '') and (LFace.Renderer.Root.ID <> NavigationID('compact')) then
    begin
      LNode := LFace.Renderer.Root.Part(NyxPart('move'));
    end;

    if LNode = nil then
    begin
      Continue;
    end;
    { Escape/teardown has no pointer response window. Resolve the still-owned
      source immediately; retain no host pointer across face retirement. }
    {$ifdef PAS2JS}
    LElement := LFace.Renderer.ElementFor(LNode.ID, niRuntime);

    if (LElement <> nil) and
      TNyxGestureElement(LElement).hasPointerCapture(FConnectorPointerID) then
    begin
      LElement.releasePointerCapture(FConnectorPointerID);
    end;
    {$else}
    LControl := LFace.Renderer.ControlFor(LNode.ID, niRuntime);
    ReleaseNyxLCLPointer(LControl);
    {$endif}
    Break;
  end;
end;

procedure TToolView.ConnectorGesture(AFace: TObject; const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
var
  LFace: TConnectorFace;
  LResponse: INyxGestureResponse;
  LEventResponse: INyxEventResponse;
  LNode: TNyxNode;
  LPort: Integer;
  LTarget: Integer;
  LIndex: Integer;
  LBounds: TNyxCanvasBounds;
  LPosition: TNyxCanvasPoint;
  LHandle: TNyxNode;
  LStep: Double;
  LMode: INyxToolCanvasControlConnectors;

  procedure Consume;
  begin

    if Supports(AExecution, INyxEventResponse, LEventResponse) and LEventResponse.CanConsume then
    begin
      LEventResponse.Consume;
    end;
  end;

begin

  if not FConnected or FConnectorUpdating or (FConnectorController = nil) then
  begin
    Exit;
  end;

  if not Supports(FInspector, INyxToolCanvasControlConnectors, LMode) or
    (LMode.ControlWires <> FConnectorController) then
  begin
    QueueConnectors(True);
    Exit;
  end;
  LFace := TConnectorFace(AFace);

  if LFace.Renderer = nil then
  begin
    Exit;
  end;
  LNode := LFace.Renderer.Root.Find(AEvent.OriginID);

  if LNode = nil then
  begin
    Exit;
  end;

  if (LFace.NodeIndex >= 0) and (LNode = LFace.Renderer.Root.Part(NyxPart('move'))) then
  begin
    NodeGesture(AFace, AEvent, AExecution);
    Exit;
  end;
  LPort := -1;
  for LIndex := 0 to FConnectorController.PortCount - 1 do
  begin

    if FConnectorController.PortControl(LIndex).ID = LNode.ID then
    begin
      LPort := LIndex;
      Break;
    end;
  end;
  LHandle := nil;

  if (LFace.NodeIndex < 0) and (LFace.Control.ID = '') and
    (LFace.Renderer.Root.ID <> NavigationID('compact')) then
  begin
    LHandle := LFace.Renderer.Root.Part(NyxPart('move'));

    if LHandle = LNode then
    begin
      LPort := -2;
    end;
  end;

  if (AEvent.Trigger = ntKeyDown) and AEvent.HasKeyboard then
  begin

    if AEvent.Keyboard.Matches(nkEscapeKey) and FConnectorPointerActive then
    begin
      ReleaseConnectorPointer;
      Consume;
      QueueConnectors(False);
      Exit;
    end;

    if LPort = -2 then
    begin

      if AEvent.Keyboard.Modifiers - [nmShift] <> [] then
      begin
        Exit;
      end;
      LStep := 8;

      if nmShift in AEvent.Keyboard.Modifiers then
      begin
        LStep := 32;
      end;
      LPosition := NyxCanvasPoint(FConnectorPanelBounds.Left, FConnectorPanelBounds.Top);

      if FConnectorPanelPosition.Defined then
      begin
        LPosition := FConnectorPanelPosition;
      end;
      case AEvent.Keyboard.Key of
        nkLeftKey:
          begin
            MovePipeline(LPosition.Offset(-LStep, 0));
          end;
        nkRightKey:
          begin
            MovePipeline(LPosition.Offset(LStep, 0));
          end;
        nkUpKey:
          begin
            MovePipeline(LPosition.Offset(0, -LStep));
          end;
        nkDownKey:
          begin
            MovePipeline(LPosition.Offset(0, LStep));
          end;
        nkHomeKey:
          begin
            ResetPipelinePosition;
          end;
        else
          begin
            Exit;
          end;
      end;
      Consume;
    end;
    Exit;
  end;

  if not Supports(AExecution, INyxGestureResponse, LResponse) or not AEvent.HasPointer then
  begin
    Exit;
  end;

  if (AEvent.Trigger = ntPointerDown) and not FConnectorPointerActive and
    (LPort <> -1) and (AEvent.Pointer.Button = npbPrimary) and
    ((LPort = -2) or (FConnectorController.Port(LPort).Direction = nfdOutput)) and
    LResponse.CanRequest(ngcCapturePointer) then
  begin

    if not ConnectorBounds(Self, LFace, LNode.ID, False, LBounds) then
    begin
      Exit;
    end;
    FConnectorPointerActive := True;
    FConnectorPointerMoved := False;
    FConnectorPointerSource := LPort;
    FConnectorPointerOrigin := LNode.ID;
    FConnectorPointerID := AEvent.Pointer.ID;
    FConnectorPointerPosition := ConnectorViewportPointer(LFace, LBounds, AEvent);
    FConnectorPointerStart := FConnectorPointerPosition;
    FConnectorPanelStart := NyxCanvasPoint(FConnectorPanelBounds.Left, FConnectorPanelBounds.Top);
    LResponse.CapturePointer;
    Consume;
    Exit;
  end;

  if not FConnectorPointerActive or (AEvent.Pointer.ID <> FConnectorPointerID) then
  begin
    Exit;
  end;

  if AEvent.Trigger in [ntPointerCancel, ntPointerCaptureLost] then
  begin
    FConnectorPointerActive := False;
    QueueConnectors(False);
    Exit;
  end;

  if ConnectorBounds(Self, LFace, LNode.ID, False, LBounds) then
  begin
    FConnectorPointerPosition := ConnectorViewportPointer(LFace, LBounds, AEvent);
  end;
  FConnectorPointerMoved := FConnectorPointerMoved or
    (Abs(FConnectorPointerPosition.X - FConnectorPointerStart.X) > 4) or
    (Abs(FConnectorPointerPosition.Y - FConnectorPointerStart.Y) > 4);

  if FConnectorPointerSource = -2 then
  begin
    MovePipeline(FConnectorPanelStart.Offset(
      FConnectorPointerPosition.X - FConnectorPointerStart.X,
      FConnectorPointerPosition.Y - FConnectorPointerStart.Y));
  end
  else
  begin
    QueueConnectors(False);
  end;

  if AEvent.Trigger = ntPointerUp then
  begin
    FConnectorPointerActive := False;

    if LResponse.CanRequest(ngcReleasePointer) then
    begin
      LResponse.ReleasePointer;
    end;
    Consume;

    if FConnectorPointerSource < 0 then
    begin
      Exit;
    end;
    { Host scrolling or a queued native child layout may have changed the
      clipped button rectangles since the last painted frame. A drop resolves
      current physical allocation, never the previous frame's hit snapshot. }
    PlaceConnectorFaces;
    LTarget := -1;
    for LIndex := High(FConnectorRects) downto 0 do
    begin

      if FConnectorPanelBounds.Defined and
        FConnectorPanelBounds.Contains(FConnectorPointerPosition) and
        (FConnectorNodeMode or (FConnectorController.Port(LIndex).Kind <> ntwpkPipeline)) then
      begin
        Continue;
      end;

      if FConnectorRects[LIndex].Defined and
        FConnectorRects[LIndex].Contains(FConnectorPointerPosition) then
      begin
        LTarget := LIndex;
        Break;
      end;
    end;
    try

      if FConnectorPointerMoved and (LTarget >= 0) and
        (LTarget <> FConnectorPointerSource) then
      begin
        FConnectorController.StageSelected(FConnectorPointerSource, LTarget);
      end
      else if not FConnectorPointerMoved then
      begin
        FConnectorController.SelectPort(FConnectorPointerSource);
      end;
    except
      on E: ENyxFlow do
      begin
        { StageSelected retains a visible diagnostic and its complete proposal. }
      end;
    end;
    QueueConnectors(True);
  end;
end;
