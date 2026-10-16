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
{ Saved canvas presentation uses detached typed graph/step keys. Builders never
  acquire executable graph, document, scheduler or target adapter authority. }

function NyxCanvasGraphNode(const AGraph: TNyxFlowRef; const AStep: TNyxFlowStepRef;
  const APosition: TNyxCanvasPoint): TNyxCanvasGraphNode;
begin

  if not AGraph.Defined or not AStep.Defined or not APosition.Defined then
  begin
    raise ENyxCanvasScene.Create('A graph-node position requires defined graph, step and world point');
  end;
  Result.Graph := NyxFlowRef(AGraph.Name);
  Result.Step := NyxFlowStep(AStep.Name);
  Result.Position := NyxCanvasPoint(APosition.X, APosition.Y);
end;

function NyxCanvasGraphNodes(const ADefinition: INyxCanvasDefinition): TNyxCanvasGraphNodes;
var
  LDefinition: INyxCanvasDefinition;
  LLayout: INyxCanvasDefinitionGraphLayout;
  LNode: TNyxCanvasGraphNode;
  LIndex: Integer;
  LCount: Integer;
begin
  LDefinition := ADefinition;

  if LDefinition = nil then
  begin
    raise ENyxCanvasScene.Create('Layout inspection requires a saved canvas definition');
  end;
  Result := nil;

  if not Supports(LDefinition, INyxCanvasDefinitionGraphLayout, LLayout) then
  begin
    Exit;
  end;
  LCount := LLayout.GraphNodeCount;

  if (LCount < 0) or (LCount > MaximumNyxCanvasGraphNodes) then
  begin
    raise ENyxCanvasScene.Create('Saved graph-node count exceeds its declared capacity');
  end;
  SetLength(Result, LCount);
  for LIndex := 0 to LCount - 1 do
  begin
    LNode := LLayout.GraphNode(LIndex);
    Result[LIndex] := NyxCanvasGraphNode(LNode.Graph, LNode.Step, LNode.Position);
  end;
end;

function NyxCanvasGraphLayout(const ADefinition: INyxCanvasDefinition): INyxCanvasGraphLayout;
var
  LPlan: TCanvasDefinition;
begin
  LPlan := CopyAdmittedCanvasPlan(ADefinition);
  try
    Result := TCanvasGraphLayout.CreateOwned(LPlan);
    LPlan := nil;
  finally
    LPlan.Free;
  end;
end;

constructor TCanvasGraphLayout.CreateOwned(APlan: TCanvasDefinition);
begin
  inherited Create;

  if APlan = nil then
  begin
    raise ENyxCanvasScene.Create('A layout builder owns one independent saved plan');
  end;
  FPlan := APlan;
end;

destructor TCanvasGraphLayout.Destroy;
begin
  FPlan.Free;
  inherited Destroy;
end;

function TCanvasDefinition.GetGraphNodeCount: Integer;
begin
  Result := Length(FGraphNodes);
end;

function TCanvasDefinition.GraphNode(AIndex: Integer): TNyxCanvasGraphNode;
begin

  if (AIndex < 0) or (AIndex >= Length(FGraphNodes)) then
  begin
    raise ENyxCanvasScene.Create('Choose an existing saved graph-node position');
  end;
  Result := NyxCanvasGraphNode(FGraphNodes[AIndex].Graph,
    FGraphNodes[AIndex].Step, FGraphNodes[AIndex].Position);
end;

function TCanvasGraphLayout.Place(const AGraph: TNyxFlowRef;
  const AStep: TNyxFlowStepRef; const APosition: TNyxCanvasPoint): INyxCanvasGraphLayout;
begin
  Result := Nodes([NyxCanvasGraphNode(AGraph, AStep, APosition)]);
end;

function TCanvasGraphLayout.Nodes(const ANodes: array of TNyxCanvasGraphNode): INyxCanvasGraphLayout;
var
  LPlan: TCanvasDefinition;
  LNode: TNyxCanvasGraphNode;
  LIndex: Integer;
  LPrior: Integer;
  LPosition: Integer;
begin

  if (Length(ANodes) < 1) or (Length(ANodes) > MaximumNyxCanvasGraphNodes) then
  begin
    raise ENyxCanvasScene.Create('A layout group requires 1..4096 distinct graph-node positions');
  end;
  LPlan := FPlan.CopyPlan;
  try
    for LIndex := 0 to High(ANodes) do
    begin
      LNode := NyxCanvasGraphNode(ANodes[LIndex].Graph, ANodes[LIndex].Step, ANodes[LIndex].Position);
      for LPrior := 0 to LIndex - 1 do
      begin

        if (ANodes[LPrior].Graph.Name = LNode.Graph.Name) and
          (ANodes[LPrior].Step.Name = LNode.Step.Name) then
        begin
          raise ENyxCanvasScene.Create('Place each graph-node key once per layout group');
        end;
      end;
      LPosition := -1;
      for LPrior := 0 to High(LPlan.FGraphNodes) do
      begin

        if (LPlan.FGraphNodes[LPrior].Graph.Name = LNode.Graph.Name) and
          (LPlan.FGraphNodes[LPrior].Step.Name = LNode.Step.Name) then
        begin
          LPosition := LPrior;
          Break;
        end;
      end;

      if LPosition < 0 then
      begin

        if Length(LPlan.FGraphNodes) >= MaximumNyxCanvasGraphNodes then
        begin
          raise ENyxCanvasScene.Create('Saved graph nodes exceed their declared capacity');
        end;
        LPosition := Length(LPlan.FGraphNodes);
        SetLength(LPlan.FGraphNodes, LPosition + 1);
      end;
      LPlan.FGraphNodes[LPosition] := LNode;
    end;

    if NyxUTF8ByteCount(LPlan.ToData.ToJSON) > MaximumNyxCanvasDefinitionBytes then
    begin
      raise ENyxCanvasScene.Create('Saved graph layout exceeds the complete canvas byte budget');
    end;
    Result := TCanvasGraphLayout.CreateOwned(LPlan);
    LPlan := nil;
  finally
    LPlan.Free;
  end;
end;

function TCanvasGraphLayout.Automatic(const AGraph: TNyxFlowRef;
  const AStep: TNyxFlowStepRef): INyxCanvasGraphLayout;
var
  LPlan: TCanvasDefinition;
  LIndex: Integer;
  LPosition: Integer;
begin

  if not AGraph.Defined or not AStep.Defined then
  begin
    raise ENyxCanvasScene.Create('Automatic layout requires a defined graph and step');
  end;
  LPosition := -1;
  for LIndex := 0 to High(FPlan.FGraphNodes) do
  begin

    if (FPlan.FGraphNodes[LIndex].Graph.Name = AGraph.Name) and
      (FPlan.FGraphNodes[LIndex].Step.Name = AStep.Name) then
    begin
      LPosition := LIndex;
      Break;
    end;
  end;

  if LPosition < 0 then
  begin
    raise ENyxCanvasScene.Create('Choose an explicit node position before restoring automatic layout');
  end;
  LPlan := FPlan.CopyPlan;
  try
    for LIndex := LPosition to High(LPlan.FGraphNodes) - 1 do
    begin
      LPlan.FGraphNodes[LIndex] := LPlan.FGraphNodes[LIndex + 1];
    end;
    SetLength(LPlan.FGraphNodes, Length(LPlan.FGraphNodes) - 1);
    Result := TCanvasGraphLayout.CreateOwned(LPlan);
    LPlan := nil;
  finally
    LPlan.Free;
  end;
end;

function TCanvasGraphLayout.Done: INyxCanvasDefinition;
begin
  Result := FPlan.CopyPlan;
end;
