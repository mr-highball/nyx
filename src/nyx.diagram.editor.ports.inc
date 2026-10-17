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
{ Both adapters consume this optional workspace through public Nyx controls.
  Field strings are a local UI boundary; accepted geometry/choices stay typed. }

function TEditorView.GetPortToolsExpanded: Boolean;
begin
  RequireConnected;
  Result := FPortToolsExpanded;
end;

function TEditorView.GetConnectionStartPort: TNyxDiagramPortRef;
begin
  RequireConnected;
  Result := FConnectionStartPort;
end;

procedure TEditorView.RefreshPortFields;
var
  LVertex: TNyxDiagramVertex;
  LPort: TNyxDiagramPort;
  LChoices: TNyxStrings;
  LIndex: Integer;
  LFound: Boolean;
  LPart: TNyxDiagramEditorPart;
begin
  LChoices := TNyxStrings.Create;
  try
    LChoices.Add(NyxDiagramPortChoiceCaption(Default(TNyxDiagramPortRef)));
    LPort := NyxDiagramPort(NyxDiagramPortRef('New port'), 1, 0.5);
    LFound := False;

    if FSelection.Defined then
    begin
      LVertex := FDiagram.FindVertex(FSelection);

      if FPortVertex.Name <> FSelection.Name then
      begin
        FPortSelection := Default(TNyxDiagramPortRef);

        if LVertex.PortCount > 0 then
        begin
          FPortSelection := LVertex.Port(0).Reference;
        end;
      end;
      for LIndex := 0 to LVertex.PortCount - 1 do
      begin
        LChoices.Add(NyxDiagramPortChoiceCaption(LVertex.Port(LIndex).Reference));

        if LVertex.Port(LIndex).Reference.Name = FPortSelection.Name then
        begin
          LPort := LVertex.Port(LIndex);
          LFound := True;
        end;
      end;
    end;

    if not LFound then
    begin
      FPortSelection := Default(TNyxDiagramPortRef);
      Field(ndepPortName).Value := '';
    end
    else
    begin
      Field(ndepPortName).Value := LPort.Reference.Name;
    end;
    FPortVertex := FSelection;
    (RetainNyxControl(Part(ndepPortChoice)) as INyxChoiceControl).Items := LChoices.Text;
    Field(ndepPortChoice).Value := NyxDiagramPortChoiceCaption(FPortSelection);
    Field(ndepPortX).Value := NyxData(LPort.Position.X).ToJSON;
    Field(ndepPortY).Value := NyxData(LPort.Position.Y).ToJSON;
    Field(ndepPortDirection).Value := NyxDiagramPortDirectionCaption(LPort.Direction);
    Field(ndepPortCaption).Value := LPort.Caption;
    for LPart := ndepPortChoice to ndepPortCaption do
    begin
      RetainNyxControl(Part(LPart)).Configure.Enabled(FVertices.Count = 1).Done;
    end;
    { Single-shape forms own their enabled state in both directions. Restoring
      one member must re-enable Apply after an earlier group selection. }
    RetainNyxControl(Part(ndepApplyPort)).Configure.Enabled(FVertices.Count = 1).Done;
    RetainNyxControl(Part(ndepRestorePort)).Configure.Enabled(FVertices.Count = 1).Done;
    RetainNyxControl(Part(ndepPortIncidentEdges)).Configure.Enabled(FVertices.Count = 1).Done;
    RetainNyxControl(Part(ndepRemovePort)).Configure.Enabled(FPortSelection.Defined and (FVertices.Count = 1)).Done;
    RetainNyxControl(Part(ndepStartPortConnection)).Configure.Enabled(FPortSelection.Defined and (FVertices.Count = 1)).Done;
    RetainNyxControl(Part(ndepFinishPortConnection)).Configure.Enabled(
      FPortSelection.Defined and (FVertices.Count = 1) and HasConnectionIntent).Done;
  finally
    LChoices.Free;
  end;
end;

procedure TEditorView.ChoosePort;
var
  LVertex: TNyxDiagramVertex;
  LChoice: TNyxText;
  LIndex: Integer;
  LFound: Boolean;
  LReference: TNyxDiagramPortRef;
begin
  RequireWritable;

  if GetHasPendingProperties then
  begin
    Field(ndepPortChoice).Value := NyxDiagramPortChoiceCaption(FPortSelection);
    FRenderer.Sync;
    raise ENyxDiagram.Create('Apply or revert unfinished fields before choosing another port');
  end;
  LChoice := Field(ndepPortChoice).Value;
  LReference := Default(TNyxDiagramPortRef);
  LFound := LChoice = NyxDiagramPortChoiceCaption(LReference);

  if FSelection.Defined then
  begin
    LVertex := FDiagram.FindVertex(FSelection);
    for LIndex := 0 to LVertex.PortCount - 1 do
    begin

      if LChoice = NyxDiagramPortChoiceCaption(LVertex.Port(LIndex).Reference) then
      begin
        LReference := LVertex.Port(LIndex).Reference;
        LFound := True;
        Break;
      end;
    end;
  end;

  if not LFound then
  begin
    raise ENyxDiagram.Create('Choose a current named port or create a new one');
  end;
  FPortSelection := LReference;
  RefreshPortFields;
  FPortFieldBaseline := PortFieldBuffer;
  FFormBaseline := PropertyBuffer;
  FRenderer.Sync;
end;

function TEditorView.SelectedPortDirection: TNyxDiagramAttachment;
var
  LDirection: TNyxDiagramAttachment;
begin
  for LDirection := Low(TNyxDiagramAttachment) to High(TNyxDiagramAttachment) do
  begin

    if Field(ndepPortDirection).Value = NyxDiagramPortDirectionCaption(LDirection) then
    begin
      Exit(LDirection);
    end;
  end;
  raise ENyxDiagram.Create('Choose a supported connector exit direction');
end;

function TEditorView.ReadPortFields: TNyxDiagramPort;
begin
  Result := NyxDiagramPort(NyxDiagramPortRef(Field(ndepPortName).Value),
    GeometryNumber(Field(ndepPortX).Value), GeometryNumber(Field(ndepPortY).Value))
    .Directed(SelectedPortDirection).Captioned(Field(ndepPortCaption).Value);
end;

procedure TEditorView.ShowPortTools(AExpanded: Boolean);
begin
  RequireWritable;
  RetainNyxControl(Part(ndepPortTools)).Configure.Visible(AExpanded).Done;
  FPortToolsExpanded := AExpanded;

  if AExpanded then
  begin
    RetainNyxControl(Part(ndepTools)).Configure.Visible(False).Done;
    RetainNyxControl(Part(ndepDrawTools)).Configure.Visible(False).Done;
    RetainNyxControl(Part(ndepContentTools)).Configure.Visible(False).Done;
    FToolsExpanded := False;
    FDrawToolsExpanded := False;
    FContentToolsExpanded := False;
    FArrangeToolsExpanded := False;
    RetainNyxControl(Part(ndepArrangeTools)).Configure.Visible(False).Done;
  end;
  RefreshFrame;
  FRenderer.Sync;
end;

procedure TEditorView.SetPort(const AVertex: TNyxDiagramVertexRef;
  const APort: TNyxDiagramPort; AExpectedRevision: Integer);
begin
  RequireWritable;

  if GetHasPendingProperties then
  begin
    raise ENyxDiagram.Create('Apply or revert unfinished fields before changing a port');
  end;
  Publish(FDiagram.WithVertex(FDiagram.FindVertex(AVertex).WithPort(APort)), AExpectedRevision);
end;

procedure TEditorView.RemovePort(const AVertex: TNyxDiagramVertexRef;
  const APort: TNyxDiagramPortRef; APolicy: TNyxDiagramRemoval; AExpectedRevision: Integer);
begin
  RequireWritable;

  if GetHasPendingProperties then
  begin
    raise ENyxDiagram.Create('Apply or revert unfinished fields before removing a port');
  end;
  Publish(RemoveNyxDiagramPort(FDiagram, AVertex, APort, APolicy), AExpectedRevision);
end;

procedure TEditorView.BeginPortConnection(const AVertex: TNyxDiagramVertexRef;
  const APort: TNyxDiagramPortRef; ARouting: TNyxDiagramRouting);
begin
  RequireWritable;
  FDiagram.FindVertex(AVertex).FindPort(APort);
  BeginConnection(AVertex, ARouting);
  FConnectionStartPort := APort;
  RefreshDraw;
  FRenderer.Sync;
end;

procedure TEditorView.CompletePortConnection(const AVertex: TNyxDiagramVertexRef;
  const APort: TNyxDiagramPortRef; AExpectedRevision: Integer);
var
  LStart: TNyxDiagramEndpoint;
begin
  RequireWritable;
  FDiagram.FindVertex(AVertex).FindPort(APort);

  if FReconnectingEdge.Defined then
  begin
    ReconnectEdge(FReconnectingEdge, FReconnectEnd,
      NyxDiagramPortEnd(AVertex, APort), AExpectedRevision);
    Exit;
  end;

  if not FConnectionStart.Defined then
  begin
    raise ENyxDiagram.Create('Start a connection before choosing a destination port');
  end;

  if FConnectionStartPort.Defined then
  begin
    LStart := NyxDiagramPortEnd(FConnectionStart, FConnectionStartPort);
  end
  else
  begin
    LStart := NyxDiagramAttached(FConnectionStart);
  end;
  SetEdge(NyxDiagramEdge(FreshEdgeReference, LStart, NyxDiagramPortEnd(AVertex, APort))
    .Routed(FConnectionRouting), AExpectedRevision);
end;
