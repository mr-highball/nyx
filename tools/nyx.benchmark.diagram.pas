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
unit nyx.benchmark.diagram;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.diagram;

{ Stable English workload identities, shared by portable and mounted consumers.
  The returned reference owns text only, never a graph or target view. }
function NyxBenchmarkVertex(AIndex: Integer): TNyxDiagramVertexRef;

{ Complete independently owned graph with twice as many connections as vertices.
  Three outlines, named ports, orthogonal self-loops, parallel connections,
  captions and arrows retain the original portable benchmark's exact intent.
  Counts admit 1..MaximumNyxDiagramVertices; these are not application defaults. }
function NyxBenchmarkDiagram(AVertices: Integer): INyxDiagram;

implementation

uses SysUtils, nyx.text, nyx.canvas.geometry;

function NyxBenchmarkVertex(AIndex: Integer): TNyxDiagramVertexRef;
begin
  Result := NyxDiagramVertexRef('Step ' + TNyxText(IntToStr(AIndex)));
end;

function NyxBenchmarkDiagram(AVertices: Integer): INyxDiagram;
var
  LIndex: Integer;
  LVertex: TNyxDiagramVertex;
  LEdge: TNyxDiagramEdge;
  LNext: Integer;
begin

  if (AVertices < 1) or (AVertices > MaximumNyxDiagramVertices) then
  begin
    raise ENyxDiagram.Create('Benchmark graph count is outside its declared capacity');
  end;
  Result := NewNyxDiagram;
  for LIndex := 0 to AVertices - 1 do
  begin
    LVertex := NyxDiagramVertex(NyxBenchmarkVertex(LIndex), TNyxDiagramShape(LIndex mod 3),
      NyxCanvasBounds((LIndex mod 16) * 160 - 640, (LIndex div 16) * 120 - 240, 96, 64))
      .Captioned('Step ' + TNyxText(IntToStr(LIndex)))
      .WithPort(NyxDiagramPort(NyxDiagramPortRef('Input'), 0, 0.5).Directed(ndaLeft))
      .WithPort(NyxDiagramPort(NyxDiagramPortRef('Output'), 1, 0.5).Directed(ndaRight));
    Result := Result.WithVertex(LVertex);
  end;
  for LIndex := 0 to AVertices * 2 - 1 do
  begin
    LNext := (LIndex + 1) mod AVertices;

    if LIndex mod 7 = 0 then
    begin
      LNext := LIndex mod AVertices;
    end;
    LEdge := NyxDiagramEdge(NyxDiagramEdgeRef('Connection ' + TNyxText(IntToStr(LIndex))),
      NyxDiagramPortEnd(NyxBenchmarkVertex(LIndex mod AVertices), NyxDiagramPortRef('Output')),
      NyxDiagramPortEnd(NyxBenchmarkVertex(LNext), NyxDiagramPortRef('Input')))
      .Routed(ndrOrthogonal).Arrows(ndaNone, ndaOpen);
    Result := Result.WithEdge(LEdge);
  end;
end;

end.
