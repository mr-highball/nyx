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
{ Actual target selection/arrangement input shares the maintained editor host.
  Browser code is a consumer, never evidence of execution merely from compiling. }

type
  { Test-owned alternate selection changes ordinary text during its Count read.
    The accepted editor never retains this creator; complete post-capture guards
    must preserve that newer input and the previous membership/graph. }
  TReentrantSelection = class(TInterfacedObject, INyxDiagramVertexSelection)
  public
    Base: INyxDiagramVertexSelection;
    Calls: Integer;
    function GetCount: Integer;
    function Item(AIndex: Integer): TNyxDiagramVertexRef;
    function Contains(const AReference: TNyxDiagramVertexRef): Boolean;
    function Including(const AReference: TNyxDiagramVertexRef): INyxDiagramVertexSelection;
    function Excluding(const AReference: TNyxDiagramVertexRef): INyxDiagramVertexSelection;
    function Toggling(const AReference: TNyxDiagramVertexRef): INyxDiagramVertexSelection;
  end;

function TReentrantSelection.GetCount: Integer;
begin
  Inc(Calls);
  WriteInput(ndepCaption, 'Text entered while selection was captured');
  Result := Base.Count;
end;

function TReentrantSelection.Item(AIndex: Integer): TNyxDiagramVertexRef;
begin
  Result := Base.Item(AIndex);
end;

function TReentrantSelection.Contains(const AReference: TNyxDiagramVertexRef): Boolean;
begin
  Result := Base.Contains(AReference);
end;

function TReentrantSelection.Including(const AReference: TNyxDiagramVertexRef): INyxDiagramVertexSelection;
begin
  Result := Base.Including(AReference);
end;

function TReentrantSelection.Excluding(const AReference: TNyxDiagramVertexRef): INyxDiagramVertexSelection;
begin
  Result := Base.Excluding(AReference);
end;

function TReentrantSelection.Toggling(const AReference: TNyxDiagramVertexRef): INyxDiagramVertexSelection;
begin
  Result := Base.Toggling(AReference);
end;

procedure SelectAllShortcut;
var
  {$ifdef PAS2JS}
  LOptions: TJSObject;
  LEvent: TJSEvent;
  {$else}
  LKey: Word;
  LFace: TWinControl;
  {$endif}
begin
  {$ifdef PAS2JS}
  LOptions := TJSObject.new;
  LOptions['key'] := 'a';
  LOptions['ctrlKey'] := True;
  LOptions['bubbles'] := True;
  LOptions['cancelable'] := True;
  LEvent := TDiagramKeyEvent.new('keydown', LOptions);
  GTarget.Renderer.ElementFor(ID(ndepSelection)).dispatchEvent(LEvent);
  Check(LEvent.defaultPrevented, 'select-all reserves its browser default');
  {$else}
  LFace := GTarget.Renderer.FocusFor(ID(ndepSelection));
  LKey := Ord('A');
  TKeyAccess(LFace).OnKeyDown(LFace, LKey, [ssCtrl]);
  Check(LKey = 0, 'select-all reserves its native default');
  {$endif}
  Pump;
end;

procedure RunMultiSelection; {$ifdef PAS2JS}async;{$endif}
var
  LMulti: INyxDiagramMultiSelectionView;
  LHistory: INyxDiagramHistoryView;
  LSaved: INyxDiagram;
  LGraph: INyxDiagram;
  LAfter: INyxDiagram;
  LBefore: TNyxText;
  LRevision: Integer;
  LFirst: TNyxDiagramVertex;
  LSecond: TNyxDiagramVertex;
  LPoint: TNyxCanvasPoint;
  LFailed: Boolean;
  LCreator: TReentrantSelection;
  LCreatorLease: INyxDiagramVertexSelection;
begin
  Check(Supports(GView, INyxDiagramMultiSelectionView, LMulti) and
    Supports(GView, INyxDiagramHistoryView, LHistory), 'mounted editor exposes optional group and history contracts');
  LSaved := GView.Diagram;
  LGraph := WithNyxDiagramVertices(NewNyxDiagram, [
    NyxDiagramVertex(NyxDiagramVertexRef('One'), ndsRectangle, NyxCanvasBounds(0, 0, 80, 60)),
    NyxDiagramVertex(NyxDiagramVertexRef('Two'), ndsEllipse, NyxCanvasBounds(150, 80, 80, 60)),
    NyxDiagramVertex(NyxDiagramVertexRef('Three'), ndsDiamond, NyxCanvasBounds(400, 200, 80, 60))]);
  GView.ReplaceDiagram(LGraph, GView.Revision, True);
  GView.ShowTools(False);
  GView.SelectVertex(LGraph.Vertex(0).Reference);
  GView.Canvas.ZoomAt(1, NyxCanvasPoint(0, 0));
  GView.Canvas.SetCamera(GView.Canvas.Camera.AtCenter(LGraph.Vertex(1).Bounds.Center));
  Pump;
  {$ifdef PAS2JS}await(TJSPromise.resolve(TJSPromise.new(@DiagramTurn)));{$endif}
  LHistory.ClearHistory;
  LRevision := GView.Revision;
  DragSample(ddsDown, LGraph.Vertex(1).Bounds.Center, True, True);
  DragSample(ddsUp, LGraph.Vertex(1).Bounds.Center);
  Check((LMulti.SelectedVertices.Count = 2) and (GView.Selection.Name = 'Two') and
    not GView.Manipulating and (GView.Revision = LRevision) and (LHistory.UndoCount = 0),
    'actual modifier click toggles a member without capturing or authoring history: ' + GView.LastError);
  LFirst := LGraph.Vertex(0);
  LSecond := LGraph.Vertex(1);
  GView.Canvas.SetCamera(GView.Canvas.Camera.AtCenter(LFirst.Bounds.Center));
  Pump;
  {$ifdef PAS2JS}await(TJSPromise.resolve(TJSPromise.new(@DiagramTurn)));{$endif}
  LBefore := GView.Diagram.ToData.ToJSON;
  LPoint := LFirst.Bounds.Center;
  DragSample(ddsDown, LPoint);
  Check(GView.Manipulating and (LMulti.SelectedVertices.Count = 2), 'dragging a nonprimary member retains the whole set');
  DragSample(ddsMove, LPoint.Offset(20, 10));
  {$ifdef PAS2JS}await(TJSPromise.resolve(TJSPromise.new(@DiagramTurn)));{$endif}
  Check((GView.Revision = LRevision) and (GView.Diagram.ToData.ToJSON = LBefore) and
    (GView.DisplayDiagram.Vertex(0).Bounds.Left = 20) and
    (GView.DisplayDiagram.Vertex(1).Bounds.Left = 170), 'one group preview moves both members from accepted geometry');
  DragSample(ddsUp, LPoint.Offset(20, 10));
  Check((GView.Revision = LRevision + 1) and (LHistory.UndoCount = 1) and
    (GView.Diagram.Vertex(1).Bounds.Left = 170), 'release accepts one group graph and one local command');
  LAfter := GView.Diagram;
  GView.SelectVertex(LGraph.Vertex(2).Reference);
  LHistory.Undo(GView.Revision);
  Check((GView.Diagram.ToData.ToJSON = LBefore) and (LMulti.SelectedVertices.Count = 2) and
    (GView.Selection.Name = 'Two'), 'Undo restores the complete group and primary');
  LHistory.Redo(GView.Revision);
  Check((GView.Diagram.ToData.ToJSON = LAfter.ToData.ToJSON) and
    (LMulti.SelectedVertices.Count = 1) and (GView.Selection.Name = 'Three'),
    'Redo restores the actual selection at Undo time independently of graph history');
  LMulti.SelectVertices(NewNyxDiagramVertexSelection([LFirst.Reference, LSecond.Reference]));
  LRevision := GView.Revision;
  ArrowRight;
  Check((GView.Revision = LRevision + 1) and
    (GView.Diagram.Vertex(0).Bounds.Left = 30) and (GView.Diagram.Vertex(1).Bounds.Left = 180),
    'one actual arrow key nudges every member');
  LBefore := GView.Diagram.ToData.ToJSON;
  LRevision := GView.Revision;
  LPoint := GView.Diagram.Vertex(0).Bounds.Center;
  GView.Canvas.SetCamera(GView.Canvas.Camera.AtCenter(LPoint));
  Pump;
  {$ifdef PAS2JS}await(TJSPromise.resolve(TJSPromise.new(@DiagramTurn)));{$endif}
  DragSample(ddsDown, LPoint);
  Check(GView.Manipulating, 'the cancellation gesture captures the group: ' + GView.LastError);
  DragSample(ddsMove, LPoint.Offset(10, 10));
  Check(GView.Manipulating, 'the group cancellation preview retains capture: ' + GView.LastError);
  EscapeManipulation;
  Check((GView.Diagram.ToData.ToJSON = LBefore) and (GView.Revision = LRevision) and
    (LMulti.SelectedVertices.Count = 2), 'cancel retires a whole-group preview without a command');
  Click(ndepToggleArrange);
  Check(LMulti.ArrangeToolsExpanded and not GView.ToolsExpanded, 'ordinary Arrange opens its exclusive Nyx panel');
  Click(ndepAlignLeft);
  Check(GView.Diagram.Vertex(0).Bounds.Left = GView.Diagram.Vertex(1).Bounds.Left,
    'actual alignment button arranges the whole selection');
  LRevision := GView.Revision;
  Click(ndepAlignLeft);
  Check(GView.Revision = LRevision, 'already aligned input authors no extra command');
  Click(ndepSelectAll);
  Check(LMulti.SelectedVertices.Count = 3, 'touch-friendly select-all uses graph membership');
  Click(ndepDistributeVerticalGaps);
  Check(GView.Diagram.Vertex(1).Bounds.Top = 105, 'ordinary distribution uses the outer shapes and unequal prior gaps');
  Click(ndepClearSelection);
  Check((LMulti.SelectedVertices.Count = 0) and not GView.Selection.Defined,
    'ordinary clear removes only presentation selection');
  Click(ndepToggleMember);
  Check(LMulti.SelectedVertices.Count = 1, 'ordinary member picker can construct a touch selection');
  SelectAllShortcut;
  Check(LMulti.SelectedVertices.Count = 3, 'scoped keyboard select-all uses the same contract');
  LBefore := GView.Diagram.ToData.ToJSON;
  LRevision := GView.Revision;
  (RetainNyxControl(GTarget.Renderer.Root) as INyxControl).Configure.ReadOnly(True).Done;
  GTarget.Renderer.Sync;
  Click(ndepAlignBottom);
  Check((GView.Revision = LRevision) and (GView.Diagram.ToData.ToJSON = LBefore),
    'read-only host input cannot arrange the group');
  RetainNyxControl(GTarget.Renderer.Root).Configure.ReadOnly(False).Enabled(False).Done;
  GTarget.Renderer.Sync;
  Click(ndepAlignBottom);
  Check((GView.Revision = LRevision) and (GView.Diagram.ToData.ToJSON = LBefore),
    'disabled host input cannot arrange the group');
  RetainNyxControl(GTarget.Renderer.Root).Configure.Enabled(True).Done;
  GTarget.Renderer.Sync;
  LBefore := GView.Diagram.ToData.ToJSON;
  LRevision := GView.Revision;
  LFailed := False;
  try
    LMulti.MoveSelected(1, 1, LRevision - 1);
  except
    on E: ENyxDiagram do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and (GView.Diagram.ToData.ToJSON = LBefore), 'stale group mutation refuses atomically');
  GView.ShowTools(True);
  Check(not LMulti.ArrangeToolsExpanded, 'ordinary property panel retires Arrange visibility');
  GView.SelectVertex(LGraph.Vertex(0).Reference);
  WriteInput(ndepCaption, 'Unfinished group note');
  LFailed := False;
  try
    LMulti.SelectVertices(NewNyxDiagramVertexSelection([LFirst.Reference, LSecond.Reference]));
  except
    on E: ENyxDiagram do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and GView.HasPendingProperties and
    (ReadInput(ndepCaption) = 'Unfinished group note'), 'selection refuses without discarding unfinished properties');
  LMulti.ShowArrangeTools(True);
  LFailed := False;
  try
    LMulti.MoveSelected(1, 1, GView.Revision);
  except
    on E: ENyxDiagram do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and GView.HasPendingProperties and
    (ReadInput(ndepCaption) = 'Unfinished group note'), 'arrangement refuses without discarding hidden unfinished properties');
  LMulti.ShowArrangeTools(False);
  { Reentrant typing must target an actually visible ordinary input. Merely
    setting a hidden widget's text is not an admitted user-edit producer. }
  GView.ShowTools(True);
  GView.RestoreProperties;
  LCreator := TReentrantSelection.Create;
  LCreatorLease := LCreator;
  LCreator.Base := NewNyxDiagramVertexSelection([LFirst.Reference, LSecond.Reference]);
  LBefore := GView.Diagram.ToData.ToJSON;
  LRevision := GView.Revision;
  LFailed := False;
  try
    LMulti.SelectVertices(LCreatorLease);
  except
    on E: ENyxDiagram do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and (LCreator.Calls = 1) and (LMulti.SelectedVertices.Count = 1) and
    (GView.Diagram.ToData.ToJSON = LBefore) and (GView.Revision = LRevision) and
    (ReadInput(ndepCaption) = 'Text entered while selection was captured') and GView.HasPendingProperties,
    'alternate selection capture preserves newer ordinary input and the exact accepted frame / refused=' +
    TNyxText(BoolToStr(LFailed, True)) + ' / calls=' + TNyxText(IntToStr(LCreator.Calls)) +
    ' / selected=' + TNyxText(IntToStr(LMulti.SelectedVertices.Count)) + ' / caption=' + ReadInput(ndepCaption));
  GView.ReplaceDiagram(LSaved, GView.Revision, True);
end;
