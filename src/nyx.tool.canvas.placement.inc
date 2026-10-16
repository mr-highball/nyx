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

{ Shared placement proposal owns copied intent only. Target cards and the source
  queue already retain their own lifetimes; this owner creates no reverse lease. }

procedure TInspector.ConfigureGraphPlacement(const AOwner: TNyxControlRef);
var
  LNode: TNyxNode;
  LContext: TNyxNode;
  LProjection: TNyxNode;
  LScene: TNyxCanvasRef;
begin

  if FFlowEditing = nil then
  begin
    Exit;
  end;
  FFlowEditing.GetFlowRevision;
  RequireConnected;

  if FGraphPlacementOwner.ID = AOwner.ID then
  begin
    Exit;
  end;

  if GetAdoptionPending or (Length(FGraphPlacements) > 0) then
  begin
    raise ENyxCanvasScene.Create('Save local node placement before changing its source canvas');
  end;
  LScene := Default(TNyxCanvasRef);
  LContext := nil;
  try

    if AOwner.ID <> '' then
    begin
      LNode := FSource.Find(AOwner.ID);

      if LNode = nil then
      begin
        raise ENyxCanvasScene.Create('Graph placement requires its exact source canvas owner');
      end;
      LContext := RealizeNyxContext(FSource, LNode, LProjection);

      if LProjection <> nil then
      begin
        LScene := LProjection.CanvasScene;
      end;
    end;
    FGraphPlacementOwner := AOwner;
    FGraphPlacementScene := LScene;
  finally
    LContext.Free;
  end;
end;

function TInspector.SavedGraphNodePosition(const AGraph: TNyxFlowRef;
  const AStep: TNyxFlowStepRef; out APosition: TNyxCanvasPoint): Boolean;
var
  LLayout: INyxCanvasDefinitionGraphLayout;
  LNode: TNyxCanvasGraphNode;
  LIndex: Integer;
begin
  Result := False;
  APosition := Default(TNyxCanvasPoint);

  if not FGraphPlacementScene.Defined or
    not FSource.Canvases.Contains(FGraphPlacementScene) then
  begin
    Exit;
  end;

  if Supports(FSource.Canvases.Definition(FGraphPlacementScene),
    INyxCanvasDefinitionGraphLayout, LLayout) then
  begin
    for LIndex := 0 to LLayout.GraphNodeCount - 1 do
    begin
      LNode := LLayout.GraphNode(LIndex);

      if (LNode.Graph.Name = AGraph.Name) and (LNode.Step.Name = AStep.Name) then
      begin
        APosition := LNode.Position;
        Exit(True);
      end;
    end;
  end;
end;

function TInspector.GraphNodePosition(const AGraph: TNyxFlowRef;
  const AStep: TNyxFlowStepRef; out APosition: TNyxCanvasPoint): Boolean;
var
  LIndex: Integer;
begin
  APosition := Default(TNyxCanvasPoint);

  if FFlowEditing = nil then
  begin
    Exit(False);
  end;
  FFlowEditing.GetFlowRevision;
  RequireConnected;
  for LIndex := 0 to High(FGraphPlacements) do
  begin

    if (FGraphPlacements[LIndex].Graph.Name = AGraph.Name) and
      (FGraphPlacements[LIndex].Step.Name = AStep.Name) then
    begin
      APosition := FGraphPlacements[LIndex].Position;
      Exit(True);
    end;
  end;
  Result := SavedGraphNodePosition(AGraph, AStep, APosition);
end;

procedure TInspector.StageGraphNodePosition(const AGraph: TNyxFlowRef;
  const AStep: TNyxFlowStepRef; const APosition: TNyxCanvasPoint);
var
  LNode: TNyxCanvasGraphNode;
  LSaved: TNyxCanvasPoint;
  LIndex: Integer;
  LPosition: Integer;
begin

  if FFlowEditing = nil then
  begin
    raise ENyxCanvasScene.Create('Node placement requires the runtime graph UI owner');
  end;
  FFlowEditing.GetFlowRevision;
  RequireConnected;
  LNode := NyxCanvasGraphNode(AGraph, AStep, APosition);
  LPosition := -1;
  for LIndex := 0 to High(FGraphPlacements) do
  begin

    if (FGraphPlacements[LIndex].Graph.Name = AGraph.Name) and
      (FGraphPlacements[LIndex].Step.Name = AStep.Name) then
    begin
      LPosition := LIndex;

      if (FGraphPlacements[LIndex].Position.X = APosition.X) and
        (FGraphPlacements[LIndex].Position.Y = APosition.Y) then
      begin
        Exit;
      end;
      Break;
    end;
  end;

  if FGraphPlacementRevision = High(Integer) then
  begin
    raise ENyxCanvasScene.Create('Node placement proposal version capacity exhausted');
  end;

  if SavedGraphNodePosition(AGraph, AStep, LSaved) and
    (LSaved.X = APosition.X) and (LSaved.Y = APosition.Y) then
  begin

    if LPosition < 0 then
    begin
      Exit;
    end;
    for LIndex := LPosition to High(FGraphPlacements) - 1 do
    begin
      FGraphPlacements[LIndex] := FGraphPlacements[LIndex + 1];
    end;
    SetLength(FGraphPlacements, Length(FGraphPlacements) - 1);
  end
  else
  begin

    if LPosition < 0 then
    begin

      if Length(FGraphPlacements) >= MaximumNyxCanvasGraphNodes then
      begin
        raise ENyxCanvasScene.Create('Local node placement exceeds the saved canvas node budget');
      end;
      LPosition := Length(FGraphPlacements);
      SetLength(FGraphPlacements, LPosition + 1);
    end;
    FGraphPlacements[LPosition] := LNode;
  end;
  Inc(FGraphPlacementRevision);
end;

procedure TInspector.RetireGraphPlacements(const ANodes: TNyxCanvasGraphNodes);
var
  LIndex: Integer;
  LCaptured: Integer;
  LNext: Integer;
  LMatched: Boolean;
begin
  { Acknowledgement never clears a newer move or an unpublished graph's local
    position. Exact captured keys/values alone are now represented by the source. }
  LIndex := 0;
  while LIndex < Length(FGraphPlacements) do
  begin
    LMatched := False;
    for LCaptured := 0 to High(ANodes) do
    begin
      LMatched := LMatched or ((FGraphPlacements[LIndex].Graph.Name = ANodes[LCaptured].Graph.Name) and
        (FGraphPlacements[LIndex].Step.Name = ANodes[LCaptured].Step.Name) and
        (FGraphPlacements[LIndex].Position.X = ANodes[LCaptured].Position.X) and
        (FGraphPlacements[LIndex].Position.Y = ANodes[LCaptured].Position.Y));
    end;

    if LMatched then
    begin
      for LNext := LIndex to High(FGraphPlacements) - 1 do
      begin
        FGraphPlacements[LNext] := FGraphPlacements[LNext + 1];
      end;
      SetLength(FGraphPlacements, Length(FGraphPlacements) - 1);
    end
    else
    begin
      Inc(LIndex);
    end;
  end;
end;
