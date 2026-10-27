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

{ Navigation is presentation only. Shared geometry and revocable receivers use
  the existing canvas; no source, program or graph publisher is invoked. }

function TToolView.NavigationID(const ASuffix: TNyxText): TNyxText;
begin
  Result := FInspector.Root.ID + '-navigation-' + ASuffix;
end;

function TToolView.GetToolsExpanded: Boolean;
begin
  RequirePipelineUI;

  if not FConnected or (FCanvas = nil) then
  begin
    raise ENyxFlow.Create('Tool navigation is retired');
  end;
  case FNavigationTools of
    ntctExpanded:
      begin
        Result := True;
      end;
    ntctCompact:
      begin
        Result := False;
      end;
  else
    Result := FNavigationShowTools or
      ((FCanvas.Surface.{$ifdef PAS2JS}clientWidth{$else}ClientWidth{$endif} >= 760) and
       (FCanvas.Surface.{$ifdef PAS2JS}clientHeight{$else}ClientHeight{$endif} >= 500));
  end;
end;

procedure TToolView.ConfigureNavigation(const AOptions: TNyxCanvasNavigationOptions;
  ATools: TNyxToolCanvasToolsMode);
begin
  RequirePipelineUI;
  AOptions.Validate;

  if not FConnected or not (ATools in [ntctAutomatic, ntctExpanded, ntctCompact]) then
  begin
    raise ENyxFlow.Create('Configure navigation on a connected view with typed tools policy');
  end;
  FNavigationOptions := AOptions;
  FNavigationTools := ATools;
  FNavigationShowTools := False;
  QueueConnectors(True);
end;

function TToolView.CaptureNavigation(
  const ACamera: TNyxCanvasCamera): TNyxCanvasNavigationFrame;
var
  LMode: INyxToolCanvasControlConnectors;
  LIndex: Integer;
  LFaceIndex: Integer;
  LPosition: Integer;
  LWidth: Double;
  LHeight: Double;
  LFace: TConnectorFace;
begin
  RequirePipelineUI;
  Result := Default(TNyxCanvasNavigationFrame);

  if not FConnected or not FConnectorNodeMode or (FConnectorNodes = nil) or
    not Supports(FInspector, INyxToolCanvasControlConnectors, LMode) or
    not LMode.ControlConnectorsVisible or (LMode.ControlWires <> FConnectorController) or
    FConnectorController.Stale then
  begin
    Exit;
  end;
  FConnectorNodes.ValidateFaces(FConnectorNodeMaximum, FConnectorNodePortMaximum);
  SetLength(Result.Items, FConnectorNodes.NodeCount);
  for LIndex := 0 to High(Result.Items) do
  begin
    LFace := nil;
    for LFaceIndex := 0 to High(FConnectorFaces) do
    begin

      if TConnectorFace(FConnectorFaces[LFaceIndex]).NodeIndex = LIndex then
      begin
        LFace := TConnectorFace(FConnectorFaces[LFaceIndex]);
        Break;
      end;
    end;

    if LFace = nil then
    begin
      raise ENyxFlow.Create('Wait for the operation presentation before navigating it');
    end;
    LPosition := NodePosition(LIndex);
    ConnectorFaceSize(LFace, LWidth, LHeight);
    Result.Items[LIndex] := NyxCanvasNavigationItem(NyxCanvasBounds(
      FConnectorNodePositions[LPosition].Position.X,
      FConnectorNodePositions[LPosition].Position.Y, LWidth, LHeight), FConnectorScaling, True);
  end;

  for LFaceIndex := 0 to High(FConnectorFaces) do
  begin
    LFace := TConnectorFace(FConnectorFaces[LFaceIndex]);

    if (LFace.NodeIndex < 0) and (LFace.Control.ID = '') then
    begin
      ConnectorFaceSize(LFace, LWidth, LHeight);
      SetLength(Result.Occluders, 1);
      Result.Occluders[0] := ConnectorPanelRectangle(FConnectorPanelPosition,
        ACamera.ViewportWidth, ACamera.ViewportHeight, LWidth, LHeight);
      Break;
    end;
  end;
end;

function TToolView.FitContent: TNyxCanvasNavigationResult;
var
  LNavigation: INyxCanvasNavigationContributions;
begin
  RequirePipelineUI;

  if not FConnected or not Supports(FCanvas, INyxCanvasNavigationContributions, LNavigation) then
  begin
    raise ENyxFlow.Create('This canvas has no live navigation contribution host');
  end;
  Result := LNavigation.FitNavigation(FNavigationOptions);
end;

function TToolView.RevealOperation(const AStep: TNyxFlowStepRef): TNyxCanvasNavigationResult;
var
  LFrame: TNyxCanvasNavigationFrame;
  LCamera: TNyxCanvasCamera;
  LIndex: Integer;
  LExpected: Integer;
  LLayout: INyxCanvasLayoutView;
begin
  RequirePipelineUI;

  if not FConnected or not FConnectorNodeMode or (FConnectorNodes = nil) then
  begin
    raise ENyxFlow.Create('Reveal requires an active operation-node presentation');
  end;
  LCamera := FCanvas.Camera;
  LExpected := -1;

  if Supports(FCanvas, INyxCanvasLayoutView, LLayout) then
  begin
    LExpected := LLayout.PresentationRevision;
  end;
  LFrame := CaptureNavigation(LCamera);

  if Length(LFrame.Items) <> FConnectorNodes.NodeCount then
  begin
    raise ENyxFlow.Create('The operation navigation frame is no longer current');
  end;
  for LIndex := 0 to FConnectorNodes.NodeCount - 1 do
  begin

    if FConnectorNodes.NodeStep(LIndex).Name = AStep.Name then
    begin
      Result := RevealNyxCanvasNavigation(LCamera, LFrame.Items[LIndex],
        LFrame.Occluders, FNavigationOptions);

      if (LLayout <> nil) and (LLayout.PresentationRevision <> LExpected) then
      begin
        raise ENyxFlow.Create('Canvas changed while the operation was being revealed');
      end;
      FCanvas.SetCamera(Result.Camera);
      FNavigationStep := AStep;
      FNavigationRevealedStep := AStep;
      QueueConnectors(False);
      Exit;
    end;
  end;
  raise ENyxFlow.Create('Choose a named operation in the active graph');
end;

function TToolView.BuildNavigationToolbar(const AToolbar: INyxCard): INyxCard;
var
  LRow: INyxRow;
  LReveal: INyxRow;
  LItems: TNyxText;
  LIndex: Integer;
  LFound: Boolean;
begin
  FNavigationWasExpanded := GetToolsExpanded;

  if not FNavigationWasExpanded then
  begin
    Result := NewNyxCard(NavigationID('compact'));
    Result.Configure.Padding(4).Gap(0).Done;
    Result.Add(NewNyxButton(NavigationID('tools')).Configure.Text('Tools')
      .Hint('Expand connection and graph navigation tools.').Done);
    Exit;
  end;
  Result := AToolbar;
  LItems := '';
  LFound := False;
  for LIndex := 0 to FConnectorNodes.NodeCount - 1 do
  begin
    LFound := LFound or (FConnectorNodes.NodeStep(LIndex).Name = FNavigationStep.Name);

    if LItems <> '' then
    begin
      LItems := LItems + #10;
    end;
    LItems := LItems + FConnectorNodes.NodeStep(LIndex).Name;
  end;

  if not LFound then
  begin
    FNavigationStep := Default(TNyxFlowStepRef);

    if FConnectorNodes.NodeCount > 0 then
    begin
      FNavigationStep := FConnectorNodes.NodeStep(0);
    end;
  end;
  LRow := NewNyxRow(NavigationID('actions'));
  LRow.Configure.Padding(0).Gap(6).Wrap(nfwWrap).Done;
  LRow.Add(NewNyxButton(NavigationID('fit')).WithText('Fit content'));
  LRow.Add(NewNyxButton(NavigationID('tools')).WithText('Compact tools'));
  LReveal := NewNyxRow(NavigationID('reveal-row'));
  LReveal.Configure.Padding(0).Gap(6).Wrap(nfwWrap).Done;
  LReveal.Add(NewNyxSelect(NavigationID('step')).Configure.Text('Reveal operation')
    .Flex(1).Items(LItems).Value(FNavigationStep.Name).Done);
  LReveal.Add(NewNyxButton(NavigationID('reveal')).Configure.Text('Reveal')
    .Enabled(FNavigationStep.Defined).Done);
  { Preserve the original drag header and named parts. Put navigation before
    potentially long wiring details, instead of requiring scrolling to find it. }
  Result.Node.Insert(1, LRow);
  Result.Node.Insert(2, LReveal);
  Result.Node.Insert(3, NewNyxLabel(NavigationID('feedback')).Configure
    .Text(FNavigationMessage).Visible(FNavigationMessage <> '').Done);
end;

function TToolView.NavigationEvent(ANode: TNyxNode; const AEvent: TNyxEventInfo): Boolean;
var
  LIndex: Integer;
  LValue: TNyxText;
begin
  Result := False;

  if not FConnectorNodeMode or (FConnectorNodes = nil) then
  begin
    Exit;
  end;

  if (ANode.ID = NavigationID('step')) and (AEvent.Trigger = ntChange) then
  begin
    LValue := ANode.Prop('value');
    for LIndex := 0 to FConnectorNodes.NodeCount - 1 do
    begin

      if FConnectorNodes.NodeStep(LIndex).Name = LValue then
      begin
        FNavigationStep := FConnectorNodes.NodeStep(LIndex);
        Exit(True);
      end;
    end;
    Exit(True);
  end;

  if AEvent.Trigger <> ntClick then
  begin
    Exit;
  end;

  if ANode.ID = NavigationID('tools') then
  begin

    if FNavigationTools = ntctAutomatic then
    begin

      if GetToolsExpanded then
      begin
        FNavigationShowTools := False;
        FNavigationTools := ntctCompact;
      end
      else
      begin
        FNavigationShowTools := True;
      end;
    end
    else if FNavigationTools = ntctExpanded then
    begin
      FNavigationTools := ntctCompact;
    end
    else
    begin
      FNavigationTools := ntctExpanded;
    end;
    QueueConnectors(True);
    Exit(True);
  end;

  if (ANode.ID = NavigationID('fit')) or (ANode.ID = NavigationID('reveal')) then
  begin
    FNavigationPending := True;
    FNavigationPendingStep := Default(TNyxFlowStepRef);

    if ANode.ID = NavigationID('reveal') then
    begin
      FNavigationPendingStep := FNavigationStep;
    end;

    if FNavigationTools = ntctAutomatic then
    begin
      FNavigationShowTools := False;
    end;
    QueueConnectors(True);
    Result := True;
  end;
end;

procedure TToolView.ExecuteNavigation;
var
  LStep: TNyxFlowStepRef;
  LResult: TNyxCanvasNavigationResult;
  LIndex: Integer;
  LFace: TConnectorFace;
  LFeedback: TNyxNode;
begin

  if not FNavigationPending or not FConnected then
  begin
    Exit;
  end;
  LStep := FNavigationPendingStep;
  FNavigationPending := False;
  try

    if LStep.Defined then
    begin
      LResult := RevealOperation(LStep);
    end
    else
    begin
      LResult := FitContent;
    end;
    FNavigationMessage := '';

    if LResult.Status = ncnsLimited then
    begin
      FNavigationMessage := 'Some content is clipped or overlaps at this size or zoom limit. Compact tools or reveal one operation.';
    end;
  except
    on E: Exception do
    begin
      FNavigationMessage := E.Message;
    end;
  end;
  { Navigation runs after the originating widget callback has retired. Reuse
    existing renderers; source controls, local fields and node positions survive. }
  for LIndex := 0 to High(FConnectorFaces) do
  begin
    LFace := TConnectorFace(FConnectorFaces[LIndex]);

    if LFace.Renderer <> nil then
    begin
      LFeedback := LFace.Renderer.Root.Find(NavigationID('feedback'));

      if LFeedback <> nil then
      begin
        LFeedback.Configure.Text(FNavigationMessage).Visible(FNavigationMessage <> '').Done;
        LFace.Renderer.Sync;
      end;
    end;
  end;
  PlaceConnectorFaces;
  PaintConnectors;
end;
