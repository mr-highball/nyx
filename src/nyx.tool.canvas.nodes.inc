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
{ Shared operation-node presentation. Cards remain ordinary Nyx compositions;
  only platform allocation, scoped pointer capture and renderer lookup diverge. }

procedure TToolView.RequirePipelineUI;
begin

  if FConnectorScheduler = nil then
  begin
    raise ENyxFlow.Create('Operation-node presentation is unavailable');
  end;
  FConnectorScheduler.RequireUI;
end;

procedure TToolView.ShowPipelineNodes(AVisible: Boolean; AMaximumNodes, AMaximumPorts: Integer);
var
  LMode: INyxToolCanvasControlConnectors;
  LNodes: INyxToolPipelineNodes;
begin
  RequirePipelineUI;

  if not FConnected or not Supports(FInspector, INyxToolCanvasControlConnectors, LMode) or
    not LMode.ControlConnectorsVisible or
    not Supports(LMode.ControlWires, INyxToolPipelineNodes, LNodes) then
  begin
    raise ENyxFlow.Create('Enable canvas connections before choosing their pipeline presentation');
  end;

  if AVisible then
  begin
    LNodes.ValidateFaces(AMaximumNodes, AMaximumPorts);
  end;
  FConnectorNodeMaximum := AMaximumNodes;
  FConnectorNodePortMaximum := AMaximumPorts;
  FConnectorNodeMode := AVisible;
  FConnectorNodeMessage := '';
  QueueConnectors(True);
end;

function TToolView.GetPipelineNodesVisible: Boolean;
begin
  RequirePipelineUI;
  Result := FConnected and FConnectorNodeMode and (FConnectorNodes <> nil);
end;

function TToolView.NodePosition(AIndex: Integer): Integer;
var
  LFlow: TNyxFlowRef;
  LStep: TNyxFlowStepRef;
  LIndex: Integer;
  LColumns: Integer;
  LHome: TNyxCanvasPoint;
  LSaved: TNyxCanvasPoint;
  LPlacement: INyxToolCanvasGraphPlacement;
begin

  if not FConnected or (FConnectorNodes = nil) then
  begin
    raise ENyxFlow.Create('Operation placement requires a current connected pipeline snapshot');
  end;
  LFlow := FConnectorNodes.NodeFlow;
  LStep := FConnectorNodes.NodeStep(AIndex);
  for LIndex := 0 to High(FConnectorNodePositions) do
  begin

    if (FConnectorNodePositions[LIndex].Flow.Name = LFlow.Name) and
      (FConnectorNodePositions[LIndex].Step.Name = LStep.Name) then
    begin
      Exit(LIndex);
    end;
  end;
  { Default positions materialize once in world space. Subsequent camera turns
    must not recompute them relative to the viewport or make nodes follow a pan.
    The shared registry and maximum face count bound the retained position vector. }

  if Length(FConnectorNodePositions) >= 64 * 2047 then
  begin
    raise ENyxFlow.Create('Retained operation-position capacity is exhausted');
  end;
  LColumns := Max(1, Min(4, Trunc(FCanvas.Camera.ViewportWidth / 320)));
  LHome := FCanvas.Camera.ViewportToWorld(
    NyxCanvasPoint(24 + (AIndex mod LColumns) * 320, 24 + (AIndex div LColumns) * 360));
  Result := Length(FConnectorNodePositions);
  SetLength(FConnectorNodePositions, Result + 1);
  FConnectorNodePositions[Result].Flow := LFlow;
  FConnectorNodePositions[Result].Step := LStep;
  FConnectorNodePositions[Result].Position := LHome;
  FConnectorNodePositions[Result].Home := LHome;

  if Supports(FInspector, INyxToolCanvasGraphPlacement, LPlacement) and
    LPlacement.GraphNodePosition(LFlow, LStep, LSaved) then
  begin
    FConnectorNodePositions[Result].Position := LSaved;
  end;
end;

procedure TToolView.MovePipelineNode(const AStep: TNyxFlowStepRef; const AWorld: TNyxCanvasPoint);
var
  LIndex: Integer;
  LPosition: Integer;
  LProjected: TNyxCanvasPoint;
  LPlacement: INyxToolCanvasGraphPlacement;
begin
  RequirePipelineUI;

  if not FConnected or not FConnectorNodeMode or (FConnectorNodes = nil) or not AWorld.Defined then
  begin
    raise ENyxFlow.Create('Moving an operation requires an active node presentation and world point');
  end;
  LProjected := FCanvas.Camera.WorldToViewport(AWorld);

  if not LProjected.Defined then
  begin
    raise ENyxFlow.Create('Operation placement requires an admitted camera projection');
  end;
  for LIndex := 0 to FConnectorNodes.NodeCount - 1 do
  begin

    if FConnectorNodes.NodeStep(LIndex).Name = AStep.Name then
    begin
      LPosition := NodePosition(LIndex);

      if Supports(FInspector, INyxToolCanvasGraphPlacement, LPlacement) then
      begin
        LPlacement.StageGraphNodePosition(FConnectorNodes.NodeFlow, AStep, AWorld);
      end;
      FConnectorNodePositions[LPosition].Position := AWorld;
      QueueConnectors(False);
      Exit;
    end;
  end;
  raise ENyxFlow.Create('Choose an existing displayed operation');
end;

procedure TToolView.ResetPipelineNode(const AStep: TNyxFlowStepRef);
var
  LIndex: Integer;
  LPosition: Integer;
begin
  RequirePipelineUI;

  if not FConnected or not FConnectorNodeMode or (FConnectorNodes = nil) then
  begin
    raise ENyxFlow.Create('Reset requires an active node presentation');
  end;
  for LIndex := 0 to FConnectorNodes.NodeCount - 1 do
  begin

    if FConnectorNodes.NodeStep(LIndex).Name = AStep.Name then
    begin
      LPosition := NodePosition(LIndex);
      MovePipelineNode(AStep, FConnectorNodePositions[LPosition].Home);
      Exit;
    end;
  end;
  raise ENyxFlow.Create('Choose an existing displayed operation');
end;

function TToolView.PipelineNodeBounds(const AStep: TNyxFlowStepRef): TNyxCanvasBounds;
var
  LIndex: Integer;
  LFace: TConnectorFace;
begin
  RequirePipelineUI;
  Result := Default(TNyxCanvasBounds);

  if not FConnected then
  begin
    raise ENyxFlow.Create('Operation presentation has retired');
  end;
  for LIndex := 0 to High(FConnectorFaces) do
  begin
    LFace := TConnectorFace(FConnectorFaces[LIndex]);

    if (LFace.NodeIndex >= 0) and (LFace.Step.Name = AStep.Name) then
    begin
      Exit(LFace.Bounds);
    end;
  end;
end;

function TToolView.ExportPipelineNodePositions: TNyxDataValue;
var
  LValues: array of TNyxDataValue;
  LIndex: Integer;
begin
  RequirePipelineUI;

  if not FConnected then
  begin
    raise ENyxFlow.Create('Operation presentation has retired');
  end;
  SetLength(LValues, Length(FConnectorNodePositions));
  for LIndex := 0 to High(LValues) do
  begin
    LValues[LIndex] := NyxObject([
      NyxField('graph', NyxData(FConnectorNodePositions[LIndex].Flow.Name)),
      NyxField('step', NyxData(FConnectorNodePositions[LIndex].Step.Name)),
      NyxField('x', NyxData(FConnectorNodePositions[LIndex].Position.X)),
      NyxField('y', NyxData(FConnectorNodePositions[LIndex].Position.Y))]);
  end;
  Result := NyxObject([NyxField('version', NyxData(1)), NyxField('nodes', NyxArray(LValues))]);
end;

function TToolView.PipelineNodeRenderer(const AStep: TNyxFlowStepRef):
  {$ifdef PAS2JS}TNyxBrowserRenderer{$else}TNyxLCLRenderer{$endif};
var
  LIndex: Integer;
  LFace: TConnectorFace;
begin
  RequirePipelineUI;
  Result := nil;

  if not FConnected then
  begin
    raise ENyxFlow.Create('Operation presentation has retired');
  end;
  for LIndex := 0 to High(FConnectorFaces) do
  begin
    LFace := TConnectorFace(FConnectorFaces[LIndex]);

    if (LFace.NodeIndex >= 0) and (LFace.Step.Name = AStep.Name) then
    begin
      Exit(LFace.Renderer);
    end;
  end;
  raise ENyxFlow.Create('The requested operation face is not mounted');
end;

function TToolView.NodeEndpointBounds(const AEndpoint: TNyxFlowEndpoint;
  out ABounds: TNyxCanvasBounds): Boolean;
var
  LIndex: Integer;
  LPortIndex: Integer;
  LFace: TConnectorFace;
  LPort: TNyxToolPipelinePort;
  LActual: TNyxCanvasBounds;
begin
  Result := False;
  ABounds := Default(TNyxCanvasBounds);

  if FConnectorNodes = nil then
  begin
    Exit;
  end;
  for LIndex := 0 to High(FConnectorFaces) do
  begin
    LFace := TConnectorFace(FConnectorFaces[LIndex]);

    if (LFace.NodeIndex < 0) or (LFace.Step.Name <> AEndpoint.Step.Name) then
    begin
      Continue;
    end;
    for LPortIndex := 0 to FConnectorNodes.NodeDescriptor(LFace.NodeIndex).PortCount - 1 do
    begin
      LPort := FConnectorNodes.NodePort(LFace.NodeIndex, LPortIndex);

      if LPort.Endpoint.Same(AEndpoint) then
      begin
        Result := ConnectorBounds(Self, LFace, LPort.DisplayControl.ID, True, LActual);

        if Result and LFace.Bounds.Defined then
        begin
          { Windows clip drawing behind child cards. Anchor on the outer edge,
            aligned to the real port row, rather than hiding the curve's ends
            inside an opaque card. Hit testing still uses the actual button. }
          if LPort.Metadata.Direction = nfdOutput then
          begin
            ABounds := NyxCanvasBounds(LFace.Bounds.Left + LFace.Bounds.Width,
              LActual.Top, 0, LActual.Height);
          end
          else
          begin
            ABounds := NyxCanvasBounds(LFace.Bounds.Left, LActual.Top, 0, LActual.Height);
          end;
        end;
        Exit;
      end;
    end;
  end;
end;

procedure TToolView.NodeGesture(AFace: TObject; const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
var
  LLease: INyxToolCanvasView;
  LFace: TConnectorFace;
  LResponse: INyxGestureResponse;
  LEventResponse: INyxEventResponse;
  LNode: TNyxNode;
  LBounds: TNyxCanvasBounds;
  LWorld: TNyxCanvasPoint;
  LCamera: TNyxCanvasCamera;
  LPosition: Integer;
  LAmount: Double;

  procedure Consume;
  begin

    if Supports(AExecution, INyxEventResponse, LEventResponse) and LEventResponse.CanConsume then
    begin
      LEventResponse.Consume;
    end;
  end;

begin
  LLease := Self;
  LFace := TConnectorFace(AFace);

  if not FConnected or not FConnectorNodeMode or (FConnectorNodes = nil) or
    (LFace.Renderer = nil) or (LFace.Flow.Name <> FConnectorNodes.NodeFlow.Name) then
  begin
    Exit;
  end;
  LNode := LFace.Renderer.Root.Part(NyxPart('move'));

  if (LNode = nil) or (LNode.ID <> AEvent.OriginID) then
  begin
    Exit;
  end;
  LPosition := NodePosition(LFace.NodeIndex);
  LWorld := FConnectorNodePositions[LPosition].Position;

  if (AEvent.Trigger = ntKeyDown) and AEvent.HasKeyboard then
  begin

    if AEvent.Keyboard.Matches(nkEscapeKey) and FConnectorPointerActive then
    begin
      ReleaseConnectorPointer;
      Consume;
      Exit;
    end;

    if FConnectorPointerActive or (AEvent.Keyboard.Modifiers - [nmShift] <> []) then
    begin
      Exit;
    end;
    LAmount := 8;

    if nmShift in AEvent.Keyboard.Modifiers then
    begin
      LAmount := 32;
    end;
    case AEvent.Keyboard.Key of
      nkLeftKey:
        begin
          MovePipelineNode(LFace.Step, LWorld.Offset(-LAmount, 0));
        end;
      nkRightKey:
        begin
          MovePipelineNode(LFace.Step, LWorld.Offset(LAmount, 0));
        end;
      nkUpKey:
        begin
          MovePipelineNode(LFace.Step, LWorld.Offset(0, -LAmount));
        end;
      nkDownKey:
        begin
          MovePipelineNode(LFace.Step, LWorld.Offset(0, LAmount));
        end;
      nkHomeKey:
        begin
          ResetPipelineNode(LFace.Step);
        end;
      else
        begin
          Exit;
        end;
    end;
    Consume;
    Exit;
  end;

  if not AEvent.HasPointer or not Supports(AExecution, INyxGestureResponse, LResponse) then
  begin
    Exit;
  end;

  if not FConnectorPointerActive and (AEvent.Trigger = ntPointerDown) and
    (AEvent.Pointer.Button = npbPrimary) and LResponse.CanRequest(ngcCapturePointer) and
    ConnectorBounds(Self, LFace, LNode.ID, False, LBounds) then
  begin
    FConnectorPointerActive := True;
    FConnectorPointerSource := -3;
    FConnectorPointerOrigin := LNode.ID;
    FConnectorPointerNode := LFace.Step;
    FConnectorPointerID := AEvent.Pointer.ID;
    FConnectorPointerStart := ConnectorViewportPointer(LFace, LBounds, AEvent);
    FConnectorNodeStart := LWorld;
    FConnectorNodeCamera := FCanvas.Camera;
    LResponse.CapturePointer;
    Consume;
    Exit;
  end;

  if not FConnectorPointerActive or (FConnectorPointerSource <> -3) or
    (AEvent.Pointer.ID <> FConnectorPointerID) then
  begin
    Exit;
  end;
  LCamera := FCanvas.Camera;

  if (AEvent.Trigger in [ntPointerCancel, ntPointerCaptureLost]) or
    (LCamera.Zoom <> FConnectorNodeCamera.Zoom) or
    (LCamera.WorldCenter.X <> FConnectorNodeCamera.WorldCenter.X) or
    (LCamera.WorldCenter.Y <> FConnectorNodeCamera.WorldCenter.Y) or
    (LCamera.ViewportWidth <> FConnectorNodeCamera.ViewportWidth) or
    (LCamera.ViewportHeight <> FConnectorNodeCamera.ViewportHeight) then
  begin
    ReleaseConnectorPointer;
    Exit;
  end;

  if ConnectorBounds(Self, LFace, LNode.ID, False, LBounds) then
  begin
    LWorld := ConnectorViewportPointer(LFace, LBounds, AEvent);
    LWorld := FConnectorNodeStart.Offset(
      (LWorld.X - FConnectorPointerStart.X) / LCamera.Zoom,
      (LWorld.Y - FConnectorPointerStart.Y) / LCamera.Zoom);
    MovePipelineNode(FConnectorPointerNode, LWorld);
    Consume;
  end;

  if AEvent.Trigger = ntPointerUp then
  begin
    ReleaseConnectorPointer;
  end;
end;
