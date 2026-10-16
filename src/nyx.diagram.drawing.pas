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
unit nyx.diagram.drawing;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, Math, nyx.diagram, nyx.canvas.geometry, nyx.drawing, nyx.design.tokens;

{ Capture a complete visual graph into a detached ordinary Nyx display list.
  Edges paint first in declared order, then shapes/captions; arrows are world
  geometry. Negative coordinates are retained and a 64-unit frame includes stubs
  and the maximum admitted stroke around arrow decorations.
  An empty graph has a 160x100 view box. Caller owns the immutable result.
  Use INyxDrawing.WithPlan to embed it in any existing layout or canvas.
  This projection contains no editable graph metadata or executable callbacks:
  plain Drawing cloning/persistence retains the picture, not diagram intent.
  Captions use one line, 14 world units; automatic wrapping/ellipsis is separate. }
function DrawNyxDiagram(const ADiagram: INyxDiagram): INyxDrawingPlan;
{ Runtime selection presentation replaces only the selected shape's projected
  fill/stroke/text roles. The immutable graph and authored styles stay exact;
  no extra commands reduce the graph's admitted drawing capacity. }
function DrawNyxDiagramSelection(const ADiagram: INyxDiagram;
  const ASelection: TNyxDiagramVertexRef): INyxDrawingPlan;

implementation

function ShapePath(const AVertex: TNyxDiagramVertex): TNyxDrawingPath;
const
  { Standard four-cubic ellipse approximation; shared drawing adapters sample
    these same curves. Precise hit-testing uses the mathematical ellipse. }
  CKappa = 0.5522847498307936;
var
  LBox: TNyxCanvasBounds;
  LCenter: TNyxCanvasPoint;
  LRX: Double;
  LRY: Double;
begin
  LBox := AVertex.Bounds;
  LCenter := LBox.Center;
  LRX := LBox.Width / 2;
  LRY := LBox.Height / 2;
  case AVertex.Shape of
    ndsRectangle:
      Result := NyxDrawingRectangle(LBox);
    ndsDiamond:
      Result := NyxDrawingPath(NyxCanvasPoint(LCenter.X, LBox.Top))
        .LineTo(NyxCanvasPoint(LBox.Left + LBox.Width, LCenter.Y))
        .LineTo(NyxCanvasPoint(LCenter.X, LBox.Top + LBox.Height))
        .LineTo(NyxCanvasPoint(LBox.Left, LCenter.Y)).Close;
    ndsEllipse:
      Result := NyxDrawingPath(NyxCanvasPoint(LCenter.X + LRX, LCenter.Y))
        .CurveTo(NyxCanvasPoint(LCenter.X + LRX, LCenter.Y + CKappa * LRY),
          NyxCanvasPoint(LCenter.X + CKappa * LRX, LCenter.Y + LRY),
          NyxCanvasPoint(LCenter.X, LCenter.Y + LRY))
        .CurveTo(NyxCanvasPoint(LCenter.X - CKappa * LRX, LCenter.Y + LRY),
          NyxCanvasPoint(LCenter.X - LRX, LCenter.Y + CKappa * LRY),
          NyxCanvasPoint(LCenter.X - LRX, LCenter.Y))
        .CurveTo(NyxCanvasPoint(LCenter.X - LRX, LCenter.Y - CKappa * LRY),
          NyxCanvasPoint(LCenter.X - CKappa * LRX, LCenter.Y - LRY),
          NyxCanvasPoint(LCenter.X, LCenter.Y - LRY))
        .CurveTo(NyxCanvasPoint(LCenter.X + CKappa * LRX, LCenter.Y - LRY),
          NyxCanvasPoint(LCenter.X + LRX, LCenter.Y - CKappa * LRY),
          NyxCanvasPoint(LCenter.X + LRX, LCenter.Y)).Close;
  end;
end;

procedure IncludePoint(const APoint: TNyxCanvasPoint; var ADefined: Boolean;
  var ALeft, ATop, ARight, ABottom: Double);
begin

  if not ADefined then
  begin
    ADefined := True;
    ALeft := APoint.X;
    ATop := APoint.Y;
    ARight := APoint.X;
    ABottom := APoint.Y;
  end
  else
  begin
    ALeft := Min(ALeft, APoint.X);
    ATop := Min(ATop, APoint.Y);
    ARight := Max(ARight, APoint.X);
    ABottom := Max(ABottom, APoint.Y);
  end;
end;

procedure Arrow(const ABuilder: INyxDrawingBuilder; const ARoute: TNyxDiagramRoute;
  const AEdge: TNyxDiagramEdge; AStart: Boolean);
var
  LTip: TNyxCanvasPoint;
  LNeighbor: TNyxCanvasPoint;
  LDX: Double;
  LDY: Double;
  LLength: Double;
begin

  if ARoute.Count < 2 then
  begin
    Exit;
  end;

  if AStart then
  begin
    LTip := ARoute.Point(0);
    LNeighbor := ARoute.Point(1);
  end
  else
  begin
    LTip := ARoute.Point(ARoute.Count - 1);
    LNeighbor := ARoute.Point(ARoute.Count - 2);
  end;
  LDX := LNeighbor.X - LTip.X;
  LDY := LNeighbor.Y - LTip.Y;
  LLength := Sqrt(LDX * LDX + LDY * LDY);

  if LLength = 0 then
  begin
    Exit;
  end;
  LDX := LDX / LLength;
  LDY := LDY / LLength;
  ABuilder.Path(NyxDrawingPath(LTip.Offset(LDX * 8 - LDY * 4, LDY * 8 + LDX * 4))
    .LineTo(LTip)
    .LineTo(LTip.Offset(LDX * 8 + LDY * 4, LDY * 8 - LDX * 4)),
    AEdge.Style.Fill(Default(TNyxDrawingPaint)));
end;

function DrawNyxDiagramSelection(const ADiagram: INyxDiagram;
  const ASelection: TNyxDiagramVertexRef): INyxDrawingPlan;
var
  LDiagram: INyxDiagram;
  LBuilder: INyxDrawingBuilder;
  LDefined: Boolean;
  LLeft: Double;
  LTop: Double;
  LRight: Double;
  LBottom: Double;
  LIndex: Integer;
  LPoint: Integer;
  LVertex: TNyxDiagramVertex;
  LEdge: TNyxDiagramEdge;
  LRoute: TNyxDiagramRoute;
  LPath: TNyxDrawingPath;
  LStyle: TNyxDrawingStyle;
  LTextPaint: TNyxDrawingPaint;
begin

  if ADiagram = nil then
  begin
    raise ENyxDiagram.Create('Diagram projection requires a complete plan');
  end;
  { A creator implementation cannot change members between bounds and painting.
    Admission also enforces the same portable limits before allocating commands. }
  LDiagram := NyxDiagramFromData(ADiagram.ToData);
  LDefined := False;
  LLeft := 0;
  LTop := 0;
  LRight := 0;
  LBottom := 0;
  for LIndex := 0 to LDiagram.VertexCount - 1 do
  begin
    LVertex := LDiagram.Vertex(LIndex);
    IncludePoint(NyxCanvasPoint(LVertex.Bounds.Left, LVertex.Bounds.Top),
      LDefined, LLeft, LTop, LRight, LBottom);
    IncludePoint(NyxCanvasPoint(LVertex.Bounds.Left + LVertex.Bounds.Width,
      LVertex.Bounds.Top + LVertex.Bounds.Height), LDefined, LLeft, LTop, LRight, LBottom);
  end;
  for LIndex := 0 to LDiagram.EdgeCount - 1 do
  begin
    LRoute := LDiagram.Route(LDiagram.Edge(LIndex).Reference);
    for LPoint := 0 to LRoute.Count - 1 do
    begin
      IncludePoint(LRoute.Point(LPoint), LDefined, LLeft, LTop, LRight, LBottom);
    end;
  end;

  if LDefined then
  begin
    LBuilder := NewNyxDrawingPlan(NyxCanvasBounds(LLeft - 64, LTop - 64,
      LRight - LLeft + 128, LBottom - LTop + 128));
  end
  else
  begin
    LBuilder := NewNyxDrawingPlan(NyxCanvasBounds(0, 0, 160, 100));
  end;
  for LIndex := 0 to LDiagram.EdgeCount - 1 do
  begin
    LEdge := LDiagram.Edge(LIndex);
    LRoute := LDiagram.Route(LEdge.Reference);
    LPath := NyxDrawingPath(LRoute.Point(0));
    for LPoint := 1 to LRoute.Count - 1 do
    begin
      LPath := LPath.LineTo(LRoute.Point(LPoint));
    end;
    LBuilder.Path(LPath, LEdge.Style.Fill(Default(TNyxDrawingPaint)));

    if LEdge.StartArrow = ndaOpen then
    begin
      Arrow(LBuilder, LRoute, LEdge, True);
    end;

    if LEdge.FinishArrow = ndaOpen then
    begin
      Arrow(LBuilder, LRoute, LEdge, False);
    end;
  end;
  for LIndex := 0 to LDiagram.VertexCount - 1 do
  begin
    LVertex := LDiagram.Vertex(LIndex);
    LStyle := LVertex.Style;
    LTextPaint := NyxThemePaint(ntcText);

    if ASelection.Defined and (LVertex.Reference.Name = ASelection.Name) then
    begin
      LStyle := LStyle.Fill(NyxThemePaint(ntcAccent)).Stroke(NyxThemePaint(ntcAccent), 2);
      LTextPaint := NyxThemePaint(ntcAccentText);
    end;
    LBuilder.Path(ShapePath(LVertex), LStyle);

    if LVertex.Caption <> '' then
    begin
      LBuilder.Text(LVertex.Caption, LVertex.Bounds.Center.Offset(0, -7),
        LTextPaint, 14, ndtaCenter);
    end;
  end;
  Result := LBuilder.Build;
end;

function DrawNyxDiagram(const ADiagram: INyxDiagram): INyxDrawingPlan;
begin
  Result := DrawNyxDiagramSelection(ADiagram, Default(TNyxDiagramVertexRef));
end;

end.
