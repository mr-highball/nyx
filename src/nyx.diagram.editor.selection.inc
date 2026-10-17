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
{ Shared shape selection and arrangement. No method owns an application/tree
  backreference; all geometry joins the existing graph/history publication. }

function TEditorView.GetSelectedVertices: INyxDiagramVertexSelection;
begin
  RequireConnected;
  Result := FVertices;
end;

function TEditorView.GetArrangeToolsExpanded: Boolean;
begin
  RequireConnected;
  Result := FArrangeToolsExpanded;
end;

procedure TEditorView.SelectVertices(const ASelection: INyxDiagramVertexSelection;
  ADiscardProperties: Boolean);
var
  LSelection: INyxDiagramVertexSelection;
  LPrimary: TNyxDiagramVertexRef;
  LProjection: INyxDrawingPlan;
  LRevision: Integer;
  LBuffer: TNyxText;
  LLease: INyxDiagramEditorView;
begin
  RequireWritable;

  if GetHasPendingProperties and not ADiscardProperties then
  begin
    raise ENyxDiagram.Create('Apply or revert unfinished properties before changing selection');
  end;
  LLease := Self;
  LRevision := FRevision;
  LBuffer := PropertyBuffer;
  FPublishing := True;
  try
    LSelection := CaptureNyxDiagramVertexSelection(ASelection);
    RequireConnected;

    if (FRevision <> LRevision) or (PropertyBuffer <> LBuffer) then
    begin
      raise ENyxDiagram.Create('Diagram changed while selection was captured');
    end;
    ValidateNyxDiagramVertexSelection(FDiagram, LSelection);
    LPrimary := Default(TNyxDiagramVertexRef);

    if LSelection.Count > 0 then
    begin
      LPrimary := LSelection.Item(LSelection.Count - 1);
    end;
    LProjection := Projection(FDiagram, LPrimary, Default(TNyxDiagramEdgeRef), LSelection);
    FVertices := LSelection;
    FSelection := LPrimary;
    FSelectedEdge := Default(TNyxDiagramEdgeRef);
    FProvider.Plan := LProjection;
    SyncPicture;

    if GetConnected then
    begin
      RefreshProperties;
    end;
  finally
    FPublishing := False;
  end;
end;

procedure TEditorView.SelectRegion(const ARegion: TNyxCanvasBounds;
  AMode: TNyxDiagramMarqueeMode; ADiscardProperties: Boolean);
begin
  RequireWritable;
  SelectVertices(SelectNyxDiagramVertices(FDiagram, ARegion, AMode), ADiscardProperties);
end;

procedure TEditorView.RequireArrangement(AExpectedRevision: Integer);
begin
  RequireWritable;

  if (AExpectedRevision <> FRevision) or (FRevision = High(Integer)) then
  begin
    raise ENyxDiagram.Create('Diagram revision is stale or exhausted');
  end;

  if GetHasPendingProperties then
  begin
    raise ENyxDiagram.Create('Apply or revert unfinished properties before arranging shapes');
  end;
end;

procedure TEditorView.MoveSelected(AX, AY: Double; AExpectedRevision: Integer);
var
  LCandidate: INyxDiagram;
begin
  RequireArrangement(AExpectedRevision);
  LCandidate := MoveNyxDiagramVertices(FDiagram, FVertices, AX, AY);

  if LCandidate <> FDiagram then
  begin
    Publish(LCandidate, AExpectedRevision);
  end;
end;

procedure TEditorView.AlignSelected(AAlignment: TNyxDiagramAlignment;
  AExpectedRevision: Integer);
var
  LCandidate: INyxDiagram;
begin
  RequireArrangement(AExpectedRevision);
  LCandidate := AlignNyxDiagramVertices(FDiagram, FVertices, AAlignment);

  if LCandidate <> FDiagram then
  begin
    Publish(LCandidate, AExpectedRevision);
  end;
end;

procedure TEditorView.DistributeSelected(ADistribution: TNyxDiagramDistribution;
  AExpectedRevision: Integer);
var
  LCandidate: INyxDiagram;
begin
  RequireArrangement(AExpectedRevision);
  LCandidate := DistributeNyxDiagramVertices(FDiagram, FVertices, ADistribution);

  if LCandidate <> FDiagram then
  begin
    Publish(LCandidate, AExpectedRevision);
  end;
end;

procedure TEditorView.SelectAllVertices;
var
  LSelection: INyxDiagramVertexSelection;
  LIndex: Integer;
begin
  RequireWritable;
  LSelection := NewNyxDiagramVertexSelection([]);
  for LIndex := 0 to FDiagram.VertexCount - 1 do
  begin
    LSelection := LSelection.Including(FDiagram.Vertex(LIndex).Reference);
  end;
  SelectVertices(LSelection);
end;

procedure TEditorView.ToggleMember;
var
  LReference: TNyxDiagramVertexRef;
begin
  RequireWritable;
  { Select's text value is an explicit UI input boundary. Resolve the exact
    open name to a typed known vertex before invoking any selection behavior. }
  LReference := NyxDiagramVertexRef(Field(ndepSelectionMembers).Value);
  FDiagram.FindVertex(LReference);
  SelectVertices(FVertices.Toggling(LReference));
end;

procedure TEditorView.RefreshArrangement;
var
  LItems: TNyxStrings;
  LPrevious: TNyxText;
  LSummary: TNyxText;
  LPart: TNyxDiagramEditorPart;
  LIndex: Integer;
  LEnabled: Boolean;
begin
  LItems := TNyxStrings.Create;
  try
    LPrevious := Field(ndepSelectionMembers).Value;
    for LIndex := 0 to FDiagram.VertexCount - 1 do
    begin
      LItems.Add(FDiagram.Vertex(LIndex).Reference.Name);
    end;
    (RetainNyxControl(Part(ndepSelectionMembers)) as INyxSelect).Items := LItems.Text;

    if LItems.IndexOf(LPrevious) >= 0 then
    begin
      Field(ndepSelectionMembers).Value := LPrevious;
    end
    else if FSelection.Defined then
    begin
      Field(ndepSelectionMembers).Value := FSelection.Name;
    end
    else if LItems.Count > 0 then
    begin
      Field(ndepSelectionMembers).Value := LItems[0];
    end
    else
    begin
      Field(ndepSelectionMembers).Value := '';
    end;
  finally
    LItems.Free;
  end;
  LSummary := TNyxText(IntToStr(FVertices.Count)) + ' shapes selected';

  if FSelection.Defined then
  begin
    LSummary := LSummary + ' / primary: ' + FSelection.Name;
  end;
  (RetainNyxControl(Part(ndepSelectionSummary)) as INyxCaptionControl).Text := LSummary;
  RetainNyxControl(Part(ndepToggleMember)).Configure.Enabled(FDiagram.VertexCount > 0).Done;
  RetainNyxControl(Part(ndepSelectAll)).Configure.Enabled(FDiagram.VertexCount > 0).Done;
  RetainNyxControl(Part(ndepClearSelection)).Configure.Enabled(FVertices.Count > 0).Done;
  for LPart := ndepAlignLeft to ndepDistributeVerticalGaps do
  begin
    LEnabled := FVertices.Count >= 2;

    if LPart >= ndepDistributeHorizontalCenters then
    begin
      LEnabled := FVertices.Count >= 3;
    end;
    RetainNyxControl(Part(LPart)).Configure.Enabled(LEnabled).Done;
  end;
end;

procedure TEditorView.ShowArrangeTools(AExpanded: Boolean);
begin
  RequireWritable;
  FArrangeToolsExpanded := AExpanded;
  RetainNyxControl(Part(ndepArrangeTools)).Configure.Visible(AExpanded).Done;

  if AExpanded then
  begin
    RetainNyxControl(Part(ndepTools)).Configure.Visible(False).Done;
    RetainNyxControl(Part(ndepDrawTools)).Configure.Visible(False).Done;
    RetainNyxControl(Part(ndepContentTools)).Configure.Visible(False).Done;
    RetainNyxControl(Part(ndepPortTools)).Configure.Visible(False).Done;
    FToolsExpanded := False;
    FDrawToolsExpanded := False;
    FContentToolsExpanded := False;
    FPortToolsExpanded := False;
  end;
  RefreshArrangement;
  RefreshFrame;
  FRenderer.Sync;
end;
