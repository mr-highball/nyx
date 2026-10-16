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
program nyx_diagram_manipulation_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.diagram, nyx.diagram.manipulation, nyx.canvas.geometry,
  nyx.drawing, nyx.diagram.drawing, nyx.design.tokens.core
  {$ifdef PAS2JS}, Web{$endif};

var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create('Diagram manipulation: ' + AReason);
  end;
  Inc(GChecks);
end;

procedure Run;
var
  LVertex: TNyxDiagramVertex;
  LMove: TNyxDiagramManipulation;
  LCopy: TNyxDiagramManipulation;
  LGrip: TNyxDiagramGrip;
  LPoint: TNyxCanvasPoint;
  LBounds: TNyxCanvasBounds;
  LBefore: TNyxText;
  LWidth: Double;
  LHeight: Double;
  LLeft: Double;
  LTop: Double;
  LFailed: Boolean;
  LEdge: TNyxDiagramEdge;
  LChanged: TNyxDiagramEdge;
  LPlan: INyxDiagram;
  LCandidate: INyxDiagram;
  LDrawing: INyxDrawingPlan;
begin
  LVertex := NyxDiagramVertex(NyxDiagramVertexRef('Shape'), ndsRectangle,
    NyxCanvasBounds(-100.25, 20.5, 160.75, 80.25)).Captioned('Preserved caption');
  LBefore := LVertex.ToData.ToJSON;
  LMove := NyxDiagramMove(LVertex, NyxCanvasPoint(0.5, -0.25));
  LCopy := LMove.At(NyxCanvasPoint(21, -8.75));
  Check((LCopy.Bounds.Left = -79.75) and (LCopy.Bounds.Top = 12) and
    (LCopy.Bounds.Width = 160.75) and (LCopy.Bounds.Height = 80.25),
    'signed fractional movement keeps exact extents');
  Check(not LMove.Changed and LCopy.Changed and (LVertex.ToData.ToJSON = LBefore),
    'updating a copy preserves both retained proposal and original vertex');
  Check((LCopy.Vertex.Caption = LVertex.Caption) and
    (LCopy.Vertex.Style.ToData.ToJSON = LVertex.Style.ToData.ToJSON),
    'geometry retains caption/style');
  LCopy := LCopy.At(NyxCanvasPoint(0.5, -0.25));
  Check(not LCopy.Changed and (LCopy.Vertex.ToData.ToJSON = LBefore),
    'updates derive from the original capture instead of accumulating drift');
  LCopy := LMove.At(NyxCanvasPoint(7, -0.25), NyxCanvasGrid(10, ncgsLines, ncsmGrid));
  Check((LCopy.Bounds.Left = -90) and (LCopy.Bounds.Top = 20.5),
    'grid snaps only an axis with an actual delta');
  LCopy := LMove.At(NyxCanvasPoint(7, -0.25), NyxCanvasGrid(10, ncgsLines, ncsmGrid), True);
  Check(LCopy.Bounds.Left = -93.75, 'caller bypass retains exact unsnapped movement');

  for LGrip := ndgNorthWest to ndgWest do
  begin
    LPoint := NyxDiagramGripPoint(LVertex.Bounds, LGrip);
    Check(HitNyxDiagramGrip(LVertex.Bounds, LPoint, 0) = LGrip, 'exact boundary grip hit');
    LCopy := NyxDiagramResize(LVertex, LGrip, LPoint).At(LPoint.Offset(16.5, -8.25));
    LLeft := LVertex.Bounds.Left;
    LTop := LVertex.Bounds.Top;
    LWidth := LVertex.Bounds.Width;
    LHeight := LVertex.Bounds.Height;

    if LGrip in [ndgNorthWest, ndgWest, ndgSouthWest] then
    begin
      LLeft := LLeft + 16.5;
      LWidth := LWidth - 16.5;
    end
    else if LGrip in [ndgNorthEast, ndgEast, ndgSouthEast] then
    begin
      LWidth := LWidth + 16.5;
    end;

    if LGrip in [ndgNorthWest, ndgNorth, ndgNorthEast] then
    begin
      LTop := LTop - 8.25;
      LHeight := LHeight + 8.25;
    end
    else if LGrip in [ndgSouthWest, ndgSouth, ndgSouthEast] then
    begin
      LHeight := LHeight - 8.25;
    end;
    LBounds := LCopy.Bounds;
    Check((LBounds.Left = LLeft) and (LBounds.Top = LTop) and
      (LBounds.Width = LWidth) and (LBounds.Height = LHeight),
      'each grip changes only its intended sides with exact fractional deltas');
    Check(LMove.Vertex.ToData.ToJSON = LBefore, 'resize does not mutate an independent retained move');
  end;
  LPoint := NyxDiagramGripPoint(LVertex.Bounds, ndgNorthWest);
  LCopy := NyxDiagramResize(LVertex, ndgNorthWest, LPoint).At(LPoint.Offset(1000, 1000));
  Check((LCopy.Bounds.Width = 8) and (LCopy.Bounds.Height = 8) and
    (LCopy.Bounds.Left = 52.5) and (LCopy.Bounds.Top = 92.75),
    'minimum size pins the original opposite corner');
  LCopy := NyxDiagramResize(LVertex, ndgSouthEast, LPoint).At(LPoint.Offset(2000000, 2000000));
  Check((LCopy.Bounds.Width = 1000000) and (LCopy.Bounds.Height = 1000000) and
    (LCopy.Bounds.Left = LVertex.Bounds.Left), 'maximum extent is bounded without moving fixed origin');
  Check(HitNyxDiagramGrip(LVertex.Bounds, LVertex.Bounds.Center, 2) = ndgNone,
    'shape interior is not a resize grip');
  LFailed := False;
  try
    LCopy := Default(TNyxDiagramManipulation).At(LPoint);
  except
    on E: ENyxDiagram do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed, 'absent captures refuse updates');
  LFailed := False;
  try
    LCopy := NyxDiagramResize(LVertex, ndgNone, LPoint);
  except
    on E: ENyxDiagram do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed, 'absent grips refuse capture');
  LFailed := False;
  try
    LCopy := LMove.At(NyxCanvasPoint(1000000000, 0));
  except
    on E: Exception do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and not LMove.Changed and (LVertex.ToData.ToJSON = LBefore),
    'out-of-domain movement refuses without mutating baseline');

  LPlan := NewNyxDiagram.WithVertex(LVertex)
    .WithVertex(NyxDiagramVertex(NyxDiagramVertexRef('Target'), ndsEllipse,
      NyxCanvasBounds(200, 0, 100, 80)));
  LEdge := NyxDiagramEdge(NyxDiagramEdgeRef('Link'), NyxDiagramFreeEnd(NyxCanvasPoint(-200, 0)),
    NyxDiagramAttached(LVertex.Reference, ndaLeft)).Routed(ndrOrthogonal)
    .Arrows(ndaOpen, ndaNone).Styled(NyxDrawingStyle.Stroke(NyxThemePaint(ntcAccent), 3));
  LPlan := LPlan.WithEdge(LEdge);
  LBounds := DrawNyxDiagramEditor(LPlan, LVertex.Reference,
    Default(TNyxDiagramEdgeRef)).ViewBox;
  LDrawing := DrawNyxDiagramEditor(LPlan, NyxDiagramVertexRef('Target'),
    Default(TNyxDiagramEdgeRef));
  Check((LDrawing.ViewBox.Left = LBounds.Left) and (LDrawing.ViewBox.Top = LBounds.Top) and
    (LDrawing.ViewBox.Width = LBounds.Width) and (LDrawing.ViewBox.Height = LBounds.Height),
    'shape selection preserves the complete allocated drawing plane');
  LDrawing := DrawNyxDiagramEditor(LPlan, Default(TNyxDiagramVertexRef), LEdge.Reference);
  Check((LDrawing.ViewBox.Left = LBounds.Left) and (LDrawing.ViewBox.Top = LBounds.Top) and
    (LDrawing.ViewBox.Width = LBounds.Width) and (LDrawing.ViewBox.Height = LBounds.Height),
    'edge selection preserves the same plane after retiring shape grips');
  LChanged := ReconnectNyxDiagramEdge(LEdge, ndceStart,
    NyxDiagramAttached(NyxDiagramVertexRef('Target'), ndaBottom));
  Check((LChanged.Reference.Name = LEdge.Reference.Name) and
    (LChanged.ToData.Field('finish').ToJSON = LEdge.ToData.Field('finish').ToJSON),
    'reconnection retains exact opposite endpoint and identity');
  Check((LChanged.Style.ToData.ToJSON = LEdge.Style.ToData.ToJSON) and
    (LChanged.StartArrow = ndaOpen) and (LChanged.FinishArrow = ndaNone) and
    (LChanged.Routing = ndrOrthogonal), 'reconnection retains style, arrows and routing');
  LCandidate := LPlan.WithEdge(LChanged);
  Check((LCandidate.EdgeCount = 1) and (LCandidate.Route(LEdge.Reference).Count >= 2),
    'retargeted attached endpoint resolves in its complete graph');
  LChanged := ReconnectNyxDiagramEdge(LChanged, ndceFinish, NyxDiagramFreeEnd(NyxCanvasPoint(33.5, -17.25)));
  Check((LChanged.Finish.Kind = ndekFree) and (LChanged.Finish.Point.X = 33.5),
    'the same typed operation also supports exact free endpoints');
  LBefore := LPlan.ToData.ToJSON;
  LFailed := False;
  try
    LChanged := ReconnectNyxDiagramEdge(LEdge, ndceStart,
      NyxDiagramAttached(NyxDiagramVertexRef('Missing')));
    LCandidate := LPlan.WithEdge(LChanged);
  except
    on E: ENyxDiagram do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and (LPlan.ToData.ToJSON = LBefore), 'dangling targets refuse the independent graph');
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-diagram-manipulation', 'passed');
    document.body.setAttribute('data-diagram-manipulation-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' portable diagram manipulation checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-diagram-manipulation', 'failed');
      document.body.setAttribute('data-diagram-manipulation-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
