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
unit nyx.test.diagram;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.diagram, nyx.canvas.geometry;

{ English review fixture; no platform host, application or mutable globals.
  Each call builds an independent immutable diagram with three shape recipes,
  two ordinary connections, a self-loop and a detached annotation end. }
function DiagramFixture: INyxDiagram;

implementation

function DiagramFixture: INyxDiagram;
begin
  Result := NewNyxDiagram
    .WithVertex(NyxDiagramVertex(NyxDiagramVertexRef('start'), ndsRectangle,
      NyxCanvasBounds(0, 0, 120, 80)).Captioned('Start'))
    .WithVertex(NyxDiagramVertex(NyxDiagramVertexRef('decision'), ndsDiamond,
      NyxCanvasBounds(240, -10, 120, 100)).Captioned('Ready?'))
    .WithVertex(NyxDiagramVertex(NyxDiagramVertexRef('finish'), ndsEllipse,
      NyxCanvasBounds(480, 0, 120, 80)).Captioned('Publish'))
    .WithEdge(NyxDiagramEdge(NyxDiagramEdgeRef('review'),
      NyxDiagramAttached(NyxDiagramVertexRef('start'), ndaRight),
      NyxDiagramAttached(NyxDiagramVertexRef('decision'), ndaLeft)).Routed(ndrOrthogonal))
    .WithEdge(NyxDiagramEdge(NyxDiagramEdgeRef('publish'),
      NyxDiagramAttached(NyxDiagramVertexRef('decision'), ndaRight),
      NyxDiagramAttached(NyxDiagramVertexRef('finish'), ndaLeft)).Routed(ndrOrthogonal))
    .WithEdge(NyxDiagramEdge(NyxDiagramEdgeRef('retry'),
      NyxDiagramAttached(NyxDiagramVertexRef('finish')),
      NyxDiagramAttached(NyxDiagramVertexRef('finish'))).Routed(ndrOrthogonal))
    .WithEdge(NyxDiagramEdge(NyxDiagramEdgeRef('annotation'),
      NyxDiagramFreeEnd(NyxCanvasPoint(60, 160)),
      NyxDiagramAttached(NyxDiagramVertexRef('decision'), ndaBottom)));
end;

end.
