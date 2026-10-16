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
{ Included by both diagram adapters. All editing stays in the portable graph
  and the existing coordinated publisher; platform faces only deliver input. }

function TEditorView.GetSelectedEdge: TNyxDiagramEdgeRef;
begin
  RequireConnected;
  Result := FSelectedEdge;
end;

function TEditorView.GetConnectionStart: TNyxDiagramVertexRef;
begin
  RequireConnected;
  Result := FConnectionStart;
end;

function TEditorView.GetDrawToolsExpanded: Boolean;
begin
  RequireConnected;
  Result := FDrawToolsExpanded;
end;

procedure TEditorView.RefreshDraw;
var
  LCaption: INyxCaptionControl;
begin
  LCaption := RetainNyxControl(Part(ndepConnectionPrompt)) as INyxCaptionControl;
  RetainNyxControl(Part(ndepFinishPortConnection)).Configure.Enabled(
    FSelection.Defined and FPortSelection.Defined and HasConnectionIntent).Done;

  if FReconnectingEdge.Defined then
  begin

    if FReconnectEnd = ndceStart then
    begin
      LCaption.Text := 'Reconnect start of ' + FReconnectingEdge.Name + ': choose a destination shape';
    end
    else
    begin
      LCaption.Text := 'Reconnect end of ' + FReconnectingEdge.Name + ': choose a destination shape';
    end;
    (RetainNyxControl(Part(ndepToggleDraw)) as INyxCaptionControl).Text := 'Connect';
  end
  else if FConnectionStart.Defined then
  begin
    LCaption.Text := 'From ' + FConnectionStart.Name +
      ': choose a destination shape, or select one and use Connect to selected';
    (RetainNyxControl(Part(ndepToggleDraw)) as INyxCaptionControl).Text := 'Connect';
  end
  else
  begin
    LCaption.Text := 'Choose a shape, start a connection, then choose its destination.';
    (RetainNyxControl(Part(ndepToggleDraw)) as INyxCaptionControl).Text := 'Draw';
  end;
end;

procedure TEditorView.ShowDrawTools(AExpanded: Boolean);
begin
  RequireWritable;
  FDrawToolsExpanded := AExpanded;
  RetainNyxControl(Part(ndepDrawTools)).Configure.Visible(AExpanded).Done;

  if AExpanded then
  begin
    FToolsExpanded := False;
    RetainNyxControl(Part(ndepTools)).Configure.Visible(False).Done;
    FContentToolsExpanded := False;
    RetainNyxControl(Part(ndepContentTools)).Configure.Visible(False).Done;
    FPortToolsExpanded := False;
    RetainNyxControl(Part(ndepPortTools)).Configure.Visible(False).Done;
  end;
  RefreshDraw;
  RefreshFrame;
  FRenderer.Sync;
end;

procedure TEditorView.SelectEdge(const AReference: TNyxDiagramEdgeRef;
  ADiscardProperties: Boolean);
var
  LProjection: INyxDrawingPlan;
begin
  RequireWritable;

  if not AReference.Defined then
  begin
    SelectVertex(Default(TNyxDiagramVertexRef), ADiscardProperties);
    Exit;
  end;

  if GetHasPendingProperties and not ADiscardProperties then
  begin
    raise ENyxDiagram.Create('Apply or revert unfinished geometry before selecting a connection');
  end;

  if FSelectedEdge.Name = AReference.Name then
  begin
    Exit;
  end;
  FDiagram.FindEdge(AReference);
  LProjection := Projection(FDiagram, Default(TNyxDiagramVertexRef), AReference);
  FSelectedEdge := AReference;
  FSelection := Default(TNyxDiagramVertexRef);
  FProvider.Plan := LProjection;
  SyncPicture;
  RefreshProperties;
end;

procedure TEditorView.AdvanceEdgeSelection(ADelta: Integer);
var
  LIndex: Integer;
  LSelected: Integer;
begin
  RequireWritable;

  if FDiagram.EdgeCount = 0 then
  begin
    Feedback('There are no connections in this diagram');
    Exit;
  end;
  LSelected := -1;
  for LIndex := 0 to FDiagram.EdgeCount - 1 do
  begin

    if FDiagram.Edge(LIndex).Reference.Name = FSelectedEdge.Name then
    begin
      LSelected := LIndex;
      Break;
    end;
  end;

  if (LSelected < 0) and (ADelta < 0) then
  begin
    LSelected := 0;
  end;
  LSelected := (LSelected + ADelta + FDiagram.EdgeCount) mod FDiagram.EdgeCount;
  SelectEdge(FDiagram.Edge(LSelected).Reference, False);
  RevealSelection;
end;

function TEditorView.FreshVertexReference(AShape: TNyxDiagramShape): TNyxDiagramVertexRef;
const
  CNames: array[TNyxDiagramShape] of TNyxText = ('Rectangle', 'Ellipse', 'Decision');
var
  LNumber: Integer;
  LIndex: Integer;
  LFound: Boolean;
begin
  LNumber := 1;
  repeat
    Result := NyxDiagramVertexRef(CNames[AShape] + TNyxText(IntToStr(LNumber)));
    LFound := False;
    for LIndex := 0 to FDiagram.VertexCount - 1 do
    begin
      LFound := LFound or (FDiagram.Vertex(LIndex).Reference.Name = Result.Name);
    end;
    Inc(LNumber);
  until not LFound;
end;

function TEditorView.FreshEdgeReference: TNyxDiagramEdgeRef;
var
  LNumber: Integer;
  LIndex: Integer;
  LFound: Boolean;
begin
  LNumber := 1;
  repeat
    Result := NyxDiagramEdgeRef('Connection' + TNyxText(IntToStr(LNumber)));
    LFound := False;
    for LIndex := 0 to FDiagram.EdgeCount - 1 do
    begin
      LFound := LFound or (FDiagram.Edge(LIndex).Reference.Name = Result.Name);
    end;
    Inc(LNumber);
  until not LFound;
end;

procedure TEditorView.AddShapeAtCenter(AShape: TNyxDiagramShape);
const
  CTitles: array[TNyxDiagramShape] of TNyxText = ('New step', 'New state', 'Decision');
var
  LCenter: TNyxCanvasPoint;
begin
  RequireWritable;
  LCenter := FCanvas.Camera.WorldCenter;
  AddVertex(NyxDiagramVertex(FreshVertexReference(AShape), AShape,
    NyxCanvasBounds(LCenter.X - 80, LCenter.Y - 50, 160, 100))
    .Captioned(CTitles[AShape]), FRevision);
end;

procedure TEditorView.AddVertex(const AVertex: TNyxDiagramVertex; AExpectedRevision: Integer);
var
  LIndex: Integer;
begin
  RequireWritable;

  if GetHasPendingProperties then
  begin
    raise ENyxDiagram.Create('Apply or revert unfinished geometry before adding a shape');
  end;
  for LIndex := 0 to FDiagram.VertexCount - 1 do
  begin

    if FDiagram.Vertex(LIndex).Reference.Name = AVertex.Reference.Name then
    begin
      raise ENyxDiagram.Create('A shape with this exact name already exists');
    end;
  end;
  PublishSelection(FDiagram.WithVertex(AVertex), AExpectedRevision,
    AVertex.Reference, Default(TNyxDiagramEdgeRef));
end;

procedure TEditorView.SetEdge(const AEdge: TNyxDiagramEdge; AExpectedRevision: Integer);
begin
  RequireWritable;

  if GetHasPendingProperties then
  begin
    raise ENyxDiagram.Create('Apply or revert unfinished geometry before changing a connection');
  end;
  PublishSelection(FDiagram.WithEdge(AEdge), AExpectedRevision,
    Default(TNyxDiagramVertexRef), AEdge.Reference);
end;

procedure TEditorView.RemoveVertex(const AReference: TNyxDiagramVertexRef;
  ARemoval: TNyxDiagramRemoval; AExpectedRevision: Integer);
begin
  RequireWritable;
  ReplaceDiagram(FDiagram.WithoutVertex(AReference, ARemoval), AExpectedRevision, False);
end;

procedure TEditorView.RemoveEdge(const AReference: TNyxDiagramEdgeRef; AExpectedRevision: Integer);
begin
  RequireWritable;
  ReplaceDiagram(FDiagram.WithoutEdge(AReference), AExpectedRevision, False);
end;

procedure TEditorView.BeginConnection(const AStart: TNyxDiagramVertexRef;
  ARouting: TNyxDiagramRouting);
begin
  RequireWritable;

  if GetHasPendingProperties then
  begin
    raise ENyxDiagram.Create('Apply or revert unfinished geometry before starting a connection');
  end;
  FDiagram.FindVertex(AStart);

  if (Ord(ARouting) < Ord(Low(TNyxDiagramRouting))) or
    (Ord(ARouting) > Ord(High(TNyxDiagramRouting))) then
  begin
    raise ENyxDiagram.Create('Choose a known diagram routing policy');
  end;
  FConnectionStart := AStart;
  FConnectionStartPort := Default(TNyxDiagramPortRef);
  FReconnectingEdge := Default(TNyxDiagramEdgeRef);
  FConnectionRouting := ARouting;
  RefreshDraw;
  FRenderer.Sync;
end;

procedure TEditorView.CompleteConnection(const AFinish: TNyxDiagramVertexRef;
  AExpectedRevision: Integer);
var
  LEdge: TNyxDiagramEdge;
  LStart: TNyxDiagramEndpoint;
begin
  RequireWritable;

  if FReconnectingEdge.Defined then
  begin
    ReconnectEdge(FReconnectingEdge, FReconnectEnd, NyxDiagramAttached(AFinish), AExpectedRevision);
    Exit;
  end;

  if not FConnectionStart.Defined then
  begin
    raise ENyxDiagram.Create('Start a connection from a selected shape first');
  end;
  FDiagram.FindVertex(AFinish);

  if FConnectionStartPort.Defined then
  begin
    LStart := NyxDiagramPortEnd(FConnectionStart, FConnectionStartPort);
  end
  else
  begin
    LStart := NyxDiagramAttached(FConnectionStart);
  end;
  LEdge := NyxDiagramEdge(FreshEdgeReference, LStart,
    NyxDiagramAttached(AFinish)).Routed(FConnectionRouting);
  SetEdge(LEdge, AExpectedRevision);
end;

procedure TEditorView.CancelConnection;
begin
  RequireWritable;
  FConnectionStart := Default(TNyxDiagramVertexRef);
  FConnectionStartPort := Default(TNyxDiagramPortRef);
  FReconnectingEdge := Default(TNyxDiagramEdgeRef);
  RefreshDraw;
  FRenderer.Sync;
end;

function TEditorView.HasConnectionIntent: Boolean;
begin
  Result := FConnectionStart.Defined or FReconnectingEdge.Defined;
end;

function TEditorView.GetReconnectingEdge: TNyxDiagramEdgeRef;
begin
  RequireConnected;
  Result := FReconnectingEdge;
end;

procedure TEditorView.BeginReconnect(const AEdge: TNyxDiagramEdgeRef;
  AEnd: TNyxDiagramConnectionEnd);
begin
  RequireWritable;

  if GetHasPendingProperties then
  begin
    raise ENyxDiagram.Create('Apply or revert unfinished geometry before reconnecting a line');
  end;

  if (Ord(AEnd) < Ord(Low(TNyxDiagramConnectionEnd))) or
    (Ord(AEnd) > Ord(High(TNyxDiagramConnectionEnd))) then
  begin
    raise ENyxDiagram.Create('Choose a known connection endpoint');
  end;
  FDiagram.FindEdge(AEdge);
  FReconnectingEdge := AEdge;
  FReconnectEnd := AEnd;
  FConnectionStart := Default(TNyxDiagramVertexRef);
  FConnectionStartPort := Default(TNyxDiagramPortRef);
  RefreshDraw;
  FRenderer.Sync;
end;

procedure TEditorView.ReconnectEdge(const AEdge: TNyxDiagramEdgeRef;
  AEnd: TNyxDiagramConnectionEnd; const AEndpoint: TNyxDiagramEndpoint;
  AExpectedRevision: Integer);
begin
  RequireWritable;
  SetEdge(ReconnectNyxDiagramEdge(FDiagram.FindEdge(AEdge), AEnd, AEndpoint), AExpectedRevision);
end;
