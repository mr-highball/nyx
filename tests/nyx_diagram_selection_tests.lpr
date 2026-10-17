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
program nyx_diagram_selection_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, Math, nyx.text, nyx.diagram, nyx.diagram.selection,
  nyx.diagram.drawing, nyx.canvas.geometry, nyx.drawing
  {$ifdef PAS2JS}, Web{$endif};

type
  { Alternate immutable creator exposes only bounded Count/Item values to capture.
    Other methods deliberately refuse, proving readers use their detached copy.
    The fixture owns it independently of every returned selection. }
  TSelectionCreator = class(TInterfacedObject, INyxDiagramVertexSelection)
  public
    CountValue: Integer;
    CountReads: Integer;
    ItemReads: Integer;
    function GetCount: Integer;
    function Item(AIndex: Integer): TNyxDiagramVertexRef;
    function Contains(const AReference: TNyxDiagramVertexRef): Boolean;
    function Including(const AReference: TNyxDiagramVertexRef): INyxDiagramVertexSelection;
    function Excluding(const AReference: TNyxDiagramVertexRef): INyxDiagramVertexSelection;
    function Toggling(const AReference: TNyxDiagramVertexRef): INyxDiagramVertexSelection;
  end;

var
  GChecks: Integer;

function TSelectionCreator.GetCount: Integer;
begin
  Inc(CountReads);
  Result := CountValue;
end;

function TSelectionCreator.Item(AIndex: Integer): TNyxDiagramVertexRef;
begin
  Inc(ItemReads);
  Result := NyxDiagramVertexRef(TNyxText('Shape ') + TNyxText(IntToStr(AIndex)) + TNyxText(' 😀'));
end;

function TSelectionCreator.Contains(const AReference: TNyxDiagramVertexRef): Boolean;
begin
  Result := False;
  raise ENyxDiagram.Create('Capture must read values only');
end;

function TSelectionCreator.Including(const AReference: TNyxDiagramVertexRef): INyxDiagramVertexSelection;
begin
  Result := nil;
  raise ENyxDiagram.Create('Capture must read values only');
end;

function TSelectionCreator.Excluding(const AReference: TNyxDiagramVertexRef): INyxDiagramVertexSelection;
begin
  Result := nil;
  raise ENyxDiagram.Create('Capture must read values only');
end;

function TSelectionCreator.Toggling(const AReference: TNyxDiagramVertexRef): INyxDiagramVertexSelection;
begin
  Result := nil;
  raise ENyxDiagram.Create('Capture must read values only');
end;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

procedure Near(AActual, AExpected: Double; const AReason: TNyxText);
begin
  Check(Abs(AActual - AExpected) < 0.000001, AReason);
end;

procedure Run;
var
  LGraph: INyxDiagram;
  LNext: INyxDiagram;
  LSet: INyxDiagramVertexSelection;
  LCopy: INyxDiagramVertexSelection;
  LEmpty: INyxDiagramVertexSelection;
  LVertex: TNyxDiagramVertex;
  LBefore: TNyxText;
  LAlignment: TNyxDiagramAlignment;
  LDistribution: TNyxDiagramDistribution;
  LBounds: TNyxCanvasBounds;
  LPlan: INyxDrawingPlan;
  LFailed: Boolean;
  LIndex: Integer;
  LReferences: array of TNyxDiagramVertexRef;
  LCreator: TSelectionCreator;
  LCreatorLease: INyxDiagramVertexSelection;
begin
  LGraph := WithNyxDiagramVertices(NewNyxDiagram, [
    NyxDiagramVertex(NyxDiagramVertexRef('First'), ndsRectangle,
      NyxCanvasBounds(-20, -40, 20, 30)).Captioned('First — 😀')
      .WithPort(NyxDiagramPort(NyxDiagramPortRef('out'), 1, 0.5)),
    NyxDiagramVertex(NyxDiagramVertexRef('Middle'), ndsDiamond,
      NyxCanvasBounds(30, 20, 40, 50)),
    NyxDiagramVertex(NyxDiagramVertexRef('Last'), ndsEllipse,
      NyxCanvasBounds(180, 160, 60, 70)),
    NyxDiagramVertex(NyxDiagramVertexRef('first'), ndsRectangle,
      NyxCanvasBounds(500, 500, 10, 10))]);
  LGraph := LGraph.WithEdge(NyxDiagramEdge(NyxDiagramEdgeRef('Link'),
    NyxDiagramPortEnd(NyxDiagramVertexRef('First'), NyxDiagramPortRef('out')),
    NyxDiagramAttached(NyxDiagramVertexRef('Last'), ndaLeft)));
  LBefore := LGraph.ToData.ToJSON;
  LSet := NewNyxDiagramVertexSelection([NyxDiagramVertexRef('Last'),
    NyxDiagramVertexRef('First'), NyxDiagramVertexRef('Middle')]);
  LEmpty := NewNyxDiagramVertexSelection([]);
  Check((LSet.Count = 3) and (LSet.Item(2).Name = 'Middle'), 'explicit inclusion order supplies the primary');
  Check(not LSet.Contains(NyxDiagramVertexRef('first')), 'membership preserves exact case');
  Check(LSet.Including(LSet.Item(0)) = LSet, 'including an existing reference is inert');
  Check(LSet.Excluding(NyxDiagramVertexRef('missing')) = LSet, 'excluding an absent reference is inert');
  LCopy := LSet.Toggling(NyxDiagramVertexRef('First'));
  Check((LCopy.Count = 2) and (LSet.Count = 3), 'toggle owns independent membership');
  LCopy := LCopy.Toggling(NyxDiagramVertexRef('First'));
  Check(LCopy.Item(2).Name = 'First', 'reinclusion appends the primary');
  Check(CaptureNyxDiagramVertexSelection(LSet) = LSet, 'private immutable capture retains exact identity');
  LFailed := False;
  try
    NewNyxDiagramVertexSelection([LSet.Item(0), LSet.Item(0)]);
  except
    on E: ENyxDiagram do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed, 'duplicate input refuses');
  LFailed := False;
  try
    NewNyxDiagramVertexSelection([Default(TNyxDiagramVertexRef)]);
  except
    on E: ENyxDiagram do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed, 'undefined input refuses');
  LFailed := False;
  try
    LSet.Item(3);
  except
    on E: ENyxDiagram do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed, 'out-of-range membership refuses');
  LFailed := False;
  try
    MoveNyxDiagramVertices(LGraph, LSet.Including(NyxDiagramVertexRef('missing')), 1, 2);
  except
    on E: ENyxDiagram do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and (LGraph.ToData.ToJSON = LBefore), 'unknown group member refuses atomically');
  Check(MoveNyxDiagramVertices(LGraph, LSet, 0, 0) = LGraph, 'zero movement retains the graph');
  Check(MoveNyxDiagramVertices(LGraph, LEmpty, 1, 2) = LGraph, 'empty movement retains the graph');
  LNext := MoveNyxDiagramVertices(LGraph, LSet, 10.25, -5.5);
  for LIndex := 0 to 2 do
  begin
    Near(LNext.Vertex(LIndex).Bounds.Left, LGraph.Vertex(LIndex).Bounds.Left + 10.25, 'fractional grouped X');
    Near(LNext.Vertex(LIndex).Bounds.Top, LGraph.Vertex(LIndex).Bounds.Top - 5.5, 'fractional grouped Y');
    Check(LNext.Vertex(LIndex).AtBounds(LGraph.Vertex(LIndex).Bounds).ToData.ToJSON =
      LGraph.Vertex(LIndex).ToData.ToJSON, 'all nongeometry vertex values survive');
  end;
  Check(LNext.Vertex(3).ToData.ToJSON = LGraph.Vertex(3).ToData.ToJSON, 'nonmember stays exact');
  Check(LNext.Edge(0).ToData.ToJSON = LGraph.Edge(0).ToData.ToJSON, 'edge intent stays exact');
  Near(LNext.Route(NyxDiagramEdgeRef('Link')).Point(0).X,
    LGraph.Route(NyxDiagramEdgeRef('Link')).Point(0).X + 10.25, 'attached port route follows moved shape');
  for LAlignment := Low(TNyxDiagramAlignment) to High(TNyxDiagramAlignment) do
  begin
    LNext := AlignNyxDiagramVertices(LGraph, LSet, LAlignment);
    for LIndex := 0 to 2 do
    begin
      LBounds := LNext.Vertex(LIndex).Bounds;
      case LAlignment of
        ndalLeft: Near(LBounds.Left, -20, 'left alignment');
        ndalHorizontalCenter: Near(LBounds.Center.X, 110, 'horizontal center alignment');
        ndalRight: Near(LBounds.Left + LBounds.Width, 240, 'right alignment');
        ndalTop: Near(LBounds.Top, -40, 'top alignment');
        ndalVerticalCenter: Near(LBounds.Center.Y, 95, 'vertical center alignment');
        ndalBottom: Near(LBounds.Top + LBounds.Height, 230, 'bottom alignment');
      end;
      Check(LNext.Vertex(LIndex).AtBounds(LGraph.Vertex(LIndex).Bounds).ToData.ToJSON =
        LGraph.Vertex(LIndex).ToData.ToJSON, 'alignment preserves rich shape intent');
    end;
    Check(AlignNyxDiagramVertices(LNext, LSet, LAlignment) = LNext, 'repeated alignment is inert');
  end;
  for LDistribution := Low(TNyxDiagramDistribution) to High(TNyxDiagramDistribution) do
  begin
    LNext := DistributeNyxDiagramVertices(LGraph, LSet, LDistribution);
    Check((LNext.Vertex(0).ToData.ToJSON = LGraph.Vertex(0).ToData.ToJSON) and
      (LNext.Vertex(2).ToData.ToJSON = LGraph.Vertex(2).ToData.ToJSON), 'outer distribution anchors stay exact');
    LBounds := LNext.Vertex(1).Bounds;
    case LDistribution of
      nddiHorizontalCenters: Near(LBounds.Center.X, 100, 'equal horizontal centers');
      nddiVerticalCenters: Near(LBounds.Center.Y, 85, 'equal vertical centers');
      nddiHorizontalGaps: Near(LBounds.Left, 70, 'equal horizontal gaps with unequal sizes');
      nddiVerticalGaps: Near(LBounds.Top, 50, 'equal vertical gaps with unequal sizes');
    end;
  end;
  LCopy := SelectNyxDiagramVertices(LGraph, NyxCanvasBounds(-20, -40, 90, 110));
  Check((LCopy.Count = 2) and (LCopy.Item(0).Name = 'First') and
    (LCopy.Item(1).Name = 'Middle'), 'contained region includes exact boundary in paint order');
  LCopy := SelectNyxDiagramVertices(LGraph, NyxCanvasBounds(-10, -20, 2, 2), ndmmIntersecting);
  Check((LCopy.Count = 1) and (LCopy.Item(0).Name = 'First'), 'intersecting region includes partial bounds');
  LCopy := SelectNyxDiagramVertices(LGraph, NyxCanvasBounds(-10, -20, 2, 2));
  Check(LCopy.Count = 0, 'contained region excludes partial bounds');
  LNext := LGraph.WithoutVertex(NyxDiagramVertexRef('Middle'));
  LCopy := RetainNyxDiagramVertexSelection(LNext, LSet);
  Check((LCopy.Count = 2) and (LCopy.Item(1).Name = 'First'), 'removal retains surviving order and primary');
  LPlan := DrawNyxDiagramEditor(LGraph, LSet, LSet.Item(2), Default(TNyxDiagramEdgeRef),
    8, 10, Default(TNyxCanvasBounds));
  Check(LPlan.Count = DrawNyxDiagram(LGraph).Count, 'multi-selection highlights without resize/port overlay commands');
  LVertex := LGraph.Vertex(1);
  LFailed := False;
  try
    WithNyxDiagramVertices(LGraph, [LVertex, LVertex]);
  except
    on E: ENyxDiagram do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and (LGraph.ToData.ToJSON = LBefore), 'duplicate batch leaves complete source exact');
  SetLength(LReferences, MaximumNyxDiagramVertices + 1);
  LFailed := False;
  try
    NewNyxDiagramVertexSelection(LReferences);
  except
    on E: ENyxDiagram do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed, 'oversized selection refuses before reading absent members');
  LCreator := TSelectionCreator.Create;
  LCreatorLease := LCreator;
  LCreator.CountValue := 2;
  LCopy := CaptureNyxDiagramVertexSelection(LCreatorLease);
  Check((LCreator.CountReads = 1) and (LCreator.ItemReads = 2) and
    (LCopy.Item(1).Name = LCreator.Item(1).Name), 'alternate capture copies each exact supplementary reference once');
  LCreator.CountValue := 0;
  Check(LCopy.Count = 2, 'later alternate creator changes cannot rewrite the captured set');
  LCreator.CountValue := -1;
  LFailed := False;
  try
    CaptureNyxDiagramVertexSelection(LCreatorLease);
  except
    on E: ENyxDiagram do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed, 'negative alternate counts refuse before allocation');
  LVertex := NyxDiagramVertex(LGraph.Vertex(0).Reference, ndsRectangle, LGraph.Vertex(0).Bounds);
  LFailed := False;
  try
    WithNyxDiagramVertices(LGraph, [LGraph.Vertex(1).AtBounds(NyxCanvasBounds(40, 30, 40, 50)), LVertex]);
  except
    on E: ENyxDiagram do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and (LGraph.ToData.ToJSON = LBefore), 'a removed attached port refuses the complete batch atomically');
  LFailed := False;
  try
    MoveNyxDiagramVertices(LGraph, LSet, NaN, 0);
  except
    on E: ENyxDiagram do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed, 'nonfinite movement refuses before any member changes');
  LNext := WithNyxDiagramVertices(NewNyxDiagram, [
    NyxDiagramVertex(NyxDiagramVertexRef('First'), ndsRectangle, NyxCanvasBounds(0, 0, 60, 20)),
    NyxDiagramVertex(NyxDiagramVertexRef('Middle'), ndsRectangle, NyxCanvasBounds(1, 0, 60, 20)),
    NyxDiagramVertex(NyxDiagramVertexRef('Last'), ndsRectangle, NyxCanvasBounds(10, 0, 60, 20))]);
  LNext := DistributeNyxDiagramVertices(LNext, LSet, nddiHorizontalGaps);
  Near(LNext.Vertex(1).Bounds.Left, 5, 'negative gaps preserve intentional overlapping outer anchors');
  LNext := WithNyxDiagramVertices(NewNyxDiagram, [
    NyxDiagramVertex(NyxDiagramVertexRef('First'), ndsRectangle, NyxCanvasBounds(0, 0, 20, 20)),
    NyxDiagramVertex(NyxDiagramVertexRef('Middle'), ndsRectangle, NyxCanvasBounds(0, 40, 20, 20)),
    NyxDiagramVertex(NyxDiagramVertexRef('Last'), ndsRectangle, NyxCanvasBounds(100, 80, 20, 20))]);
  LNext := DistributeNyxDiagramVertices(LNext, LSet, nddiHorizontalCenters);
  Near(LNext.Vertex(0).Bounds.Left, 0, 'tied positions retain the first paint-order anchor');
  Near(LNext.Vertex(1).Bounds.Left, 50, 'tied interior order ignores selection inclusion order');
  Check(LGraph.ToData.ToJSON = LBefore, 'all successful and refused edits preserve the immutable source');
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-diagram-selection', 'passed');
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' diagram selection checks';
    {$else}
    WriteLn('PASS ', GChecks, ' diagram selection checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-diagram-selection', 'failed');
      document.body.setAttribute('data-diagram-selection-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
