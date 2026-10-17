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
{ Actual mounted-editor history consumers. The host fixture owns every view,
  event and compiler boundary; this include starts no service or user project. }

const
  { Keep the draft oracle in the portable text type, including native UTF-8.
    A UnicodeString literal comparison would introduce an implicit conversion. }
  HistoryDraft: TNyxText = 'Unfinished draft — keep me';

procedure HistoryShortcut(AKey: TNyxKey; AShift: Boolean = False);
var
  {$ifdef PAS2JS}
  LOptions: TJSObject;
  LEvent: TJSEvent;
  {$else}
  LKey: Word;
  LShift: TShiftState;
  LFace: TWinControl;
  {$endif}
begin
  {$ifdef PAS2JS}
  LOptions := TJSObject.new;

  if AKey = nkZKey then
  begin
    LOptions['key'] := 'z';
  end
  else
  begin
    LOptions['key'] := 'y';
  end;
  LOptions['ctrlKey'] := True;
  LOptions['shiftKey'] := AShift;
  LOptions['bubbles'] := True;
  LOptions['cancelable'] := True;
  LEvent := TDiagramKeyEvent.new('keydown', LOptions);
  GTarget.Renderer.ElementFor(ID(ndepSelection)).dispatchEvent(LEvent);
  Check(LEvent.defaultPrevented, 'local diagram history reserves its browser shortcut');
  {$else}
  LFace := TWinControl(GTarget.Renderer.ControlFor(ID(ndepSelection)));
  LShift := [ssCtrl];

  if AShift then
  begin
    Include(LShift, ssShift);
  end;

  if AKey = nkZKey then
  begin
    LKey := Ord('Z');
  end
  else
  begin
    LKey := Ord('Y');
  end;
  TKeyAccess(LFace).OnKeyDown(LFace, LKey, LShift);
  Check(LKey = 0, 'local diagram history reserves its native shortcut');
  {$endif}
  Pump;
end;

procedure RunHistoryLimits; {$ifdef PAS2JS}async;{$endif}
var
  LHistory: INyxDiagramHistoryView;
  LVertex: TNyxDiagramVertex;
  LCandidate: INyxDiagram;
  LIndex: Integer;
  LBudget: Integer;
  LBytes: Integer;
  LRevision: Integer;
  LCamera: TNyxCanvasCamera;
  LOptions: TNyxDiagramEditorOptions;
  LFailed: Boolean;
begin
  Check(Supports(GOther, INyxDiagramHistoryView, LHistory), 'independent mount exposes optional history');
  LBudget := 3 * NyxUTF8ByteCount(GInitialBytes) + 512;
  LVertex := GOther.Diagram.Vertex(0);
  for LIndex := 1 to 3 do
  begin
    GOther.SetGeometry(LVertex.Reference, NyxCanvasBounds(LIndex * 11, 0, 160, 100), GOther.Revision);
  end;
  Check((LHistory.UndoCount = 2) and (LHistory.HistoryBytes <= LBudget),
    'configured capacity retires the oldest command while retaining the byte bound');
  LCamera := GOther.Canvas.Camera;
  LHistory.Undo(GOther.Revision);
  LHistory.Undo(GOther.Revision);
  LRevision := GOther.Revision;
  LHistory.Undo(LRevision);
  Check((GOther.Revision = LRevision) and (LHistory.RedoCount = 2) and
    (GOther.Canvas.Camera.WorldCenter.X = LCamera.WorldCenter.X) and
    (GOther.Canvas.Camera.Zoom = LCamera.Zoom),
    'bounded empty Undo is inert and replay leaves the current camera untouched');
  LHistory.Redo(GOther.Revision);
  LHistory.Redo(GOther.Revision);
  LVertex := GInitial.Vertex(0);
  for LIndex := 1 to 64 do
  begin
    LVertex := LVertex.WithPort(NyxDiagramPort(
      NyxDiagramPortRef('history-' + TNyxText(IntToStr(LIndex))), 0.5, 0.5));
    LCandidate := GInitial.WithVertex(LVertex);
    LBytes := NyxUTF8ByteCount(LCandidate.ToData.ToJSON);

    if LBytes > LBudget div 2 then
    begin
      Break;
    end;
  end;
  Check((LBytes > LBudget div 2) and (LBytes < LBudget),
    'the complete named-port graph exercises the budget independently of command capacity');
  GOther.ReplaceDiagram(LCandidate, GOther.Revision);
  GOther.SetGeometry(LVertex.Reference, NyxCanvasBounds(21, 1, 160, 100), GOther.Revision);
  Check((LHistory.UndoCount = 0) and (LHistory.RedoCount = 0) and
    (LHistory.HistoryBytes > 0) and (LHistory.HistoryBytes <= LBudget),
    'wire budget can retire an otherwise valid preceding graph without rejecting the new edit');
  GOther.ReplaceDiagram(GInitial, GOther.Revision);
  LHistory.ClearHistory;
  Check((LHistory.UndoCount = 0) and (LHistory.RedoCount = 0), 'explicit history clearing retains no command');
  LOptions := GEditor.Options;
  LFailed := False;
  try
    NyxDiagramEditorOptions.HistoryLimit(-1);
  except
    on E: ENyxDiagram do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed, 'negative command limits refuse before changing an editor');
  LFailed := False;
  try
    NyxDiagramEditorOptions.HistoryByteBudget(-1);
  except
    on E: ENyxDiagram do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed, 'negative byte budgets refuse explicitly');
  GOther.Disconnect;
  LHistory := nil;
  GEditor.WithOptions(LOptions.HistoryLimit(0));
  try
    {$ifdef PAS2JS}
    GOther := MountNyxBrowserDiagramEditor(GEditor, GDocument, GOtherHost);
    await(TJSPromise.resolve(TJSPromise.new(@DiagramTurn)));
    {$else}
    GOther := MountNyxLCLDiagramEditor(GEditor, GDocument, GOtherHost);
    {$endif}
  finally
    GEditor.WithOptions(LOptions);
  end;
  Check(Supports(GOther, INyxDiagramHistoryView, LHistory), 'history disablement preserves the optional inspection contract');
  GOther.SetGeometry(GOther.Selection, NyxCanvasBounds(11, 2, 160, 100), GOther.Revision);
  LRevision := GOther.Revision;
  LHistory.Undo(LRevision);
  Check((LHistory.UndoCount = 0) and (LHistory.HistoryBytes = 0) and
    (GOther.Revision = LRevision), 'disabled history neither retains snapshots nor blocks ordinary edits');
end;

procedure RunHistory;
var
  LHistory: INyxDiagramHistoryView;
  LInitial: TNyxText;
  LChanged: TNyxText;
  LRevision: Integer;
  LCount: Integer;
  LFailed: Boolean;
  LVertex: TNyxDiagramVertex;
  LSource: TNyxDocument;
  LSourceView: INyxDiagramSourceView;
begin
  Check(Supports(GView, INyxDiagramHistoryView, LHistory), 'ordinary editor exposes specialized local history');
  GView.ReplaceDiagram(GInitial, GView.Revision, True);
  GView.SelectVertex(GInitial.Vertex(0).Reference);
  LHistory.ClearHistory;
  LInitial := GView.Diagram.ToData.ToJSON;
  LVertex := GView.Diagram.Vertex(0);
  LRevision := GView.Revision;
  GView.SetGeometry(LVertex.Reference, NyxCanvasBounds(17, 9, 160, 100), LRevision);
  LChanged := GView.Diagram.ToData.ToJSON;
  Check((LHistory.UndoCount = 1) and (LHistory.RedoCount = 0), 'accepted geometry is one local command');
  Click(ndepUndo);
  Check((GView.Diagram.ToData.ToJSON = LInitial) and (GView.Selection.Name = LVertex.Reference.Name),
    'ordinary Undo restores complete graph and command selection');
  Click(ndepRedo);
  Check((GView.Diagram.ToData.ToJSON = LChanged) and (GView.Revision = LRevision + 3),
    'ordinary Redo restores exact graph with monotonically increasing revisions');
  HistoryShortcut(nkZKey);
  Check(GView.Diagram.ToData.ToJSON = LInitial, 'control-Z uses the same graph publication');
  HistoryShortcut(nkZKey, True);
  Check(GView.Diagram.ToData.ToJSON = LChanged, 'control-shift-Z redoes the local graph');
  HistoryShortcut(nkZKey);
  HistoryShortcut(nkYKey);
  Check(GView.Diagram.ToData.ToJSON = LChanged, 'control-Y supplies the alternate Redo shortcut');
  GView.SelectEdge(GView.Diagram.Edge(0).Reference);
  GView.RemoveEdge(GView.SelectedEdge, GView.Revision);
  LHistory.Undo(GView.Revision);
  Check((GView.SelectedEdge.Name = GView.Diagram.Edge(0).Reference.Name) and
    not GView.Selection.Defined and (GView.Diagram.ToData.ToJSON = LChanged),
    'Undo restores the complete selected connection instead of choosing a shape');
  GView.SelectVertex(LVertex.Reference);
  GView.RemoveVertex(LVertex.Reference, ndrmIncidentEdges, GView.Revision);
  Check(GView.Diagram.VertexCount = GInitial.VertexCount - 1, 'connected removal changes the accepted graph');
  LHistory.Undo(GView.Revision);
  Check(GView.Diagram.ToData.ToJSON = LChanged, 'Undo restores incident edges, styles and shape identity together');
  LHistory.Redo(GView.Revision);
  LHistory.Undo(GView.Revision);
  LCount := LHistory.RedoCount;
  GView.ReplaceDiagram(GView.Diagram, GView.Revision);
  Check(LHistory.RedoCount = LCount, 'an exact no-op replacement preserves the existing Redo branch');
  GView.SetGeometry(LVertex.Reference, NyxCanvasBounds(31, 9, 160, 100), GView.Revision);
  Check(LHistory.RedoCount = 0, 'a changed accepted branch releases Redo');
  GView.ShowTools(True);
  WriteInput(ndepCaption, HistoryDraft);
  Check(GView.HasPendingProperties, 'a visible property input owns an unfinished draft before Undo');
  LCount := LHistory.UndoCount;
  LRevision := GView.Revision;
  LChanged := GView.Diagram.ToData.ToJSON;
  LFailed := False;
  try
    LHistory.Undo(LRevision);
  except
    on E: ENyxDiagram do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and GView.HasPendingProperties and (GView.Revision = LRevision) and
    (LHistory.UndoCount = LCount) and (GView.Diagram.ToData.ToJSON = LChanged) and
    ((RetainNyxControl(GTarget.Renderer.Root.Find(ID(ndepCaption))) as INyxTextInput).Value =
      HistoryDraft),
    'unfinished properties refuse without consuming history or changing the graph');
  LFailed := False;
  try
    LHistory.Undo(LRevision - 1, True);
  except
    on E: ENyxDiagram do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and GView.HasPendingProperties and (LHistory.UndoCount = LCount),
    'stale explicit-discard requests preserve both draft and history');
  LHistory.Undo(LRevision, True);
  Check(not GView.HasPendingProperties and (LHistory.UndoCount = LCount - 1),
    'explicit discard joins only the successful graph/history publication');
  LSource := GDocument.Clone;
  try
    Check(Supports(GView, INyxDiagramSourceView, LSourceView),
      'source-context replacement uses its optional portable interface');
    LSourceView.ReplaceSource(LSource, GView.Diagram, GView.Revision);
  finally
    LSource.Free;
  end;
  Check((LHistory.UndoCount = 0) and (LHistory.RedoCount = 0),
    'successful source-context replacement starts an independent local history');
  GView.SelectVertex(Default(TNyxDiagramVertexRef));
  LChanged := GView.Diagram.ToData.ToJSON;
  GView.SetGeometry(LVertex.Reference, NyxCanvasBounds(47, 9, 160, 100), GView.Revision);
  LHistory.Undo(GView.Revision);
  Check(not GView.Selection.Defined and not GView.SelectedEdge.Defined and
    (GView.Diagram.ToData.ToJSON = LChanged),
    'Undo preserves an intentionally empty command selection');
  Check(GInitial.ToData.ToJSON = GInitialBytes, 'local history never authors its descriptor or original graph');
end;
