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
unit nyx.test.diagram.document;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.model;

{ Independent English starter plus dedicated Unicode/style qualification data.
  Each invocation owns its document and mutable definition membership; immutable
  graph values retain no caller, editor or runtime host. }
function DiagramDocumentFixture: TNyxDocument;

implementation

uses nyx.text, nyx.controls, nyx.diagram, nyx.diagram.declarations, nyx.test.diagram,
  nyx.drawing, nyx.canvas.geometry, nyx.colors, nyx.design.tokens;

function DiagramDocumentFixture: TNyxDocument;
var
  LPlan: INyxDiagram;
begin
  Result := TNyxDocument.Create;
  try
    Result.Title := 'Saved diagram review';
    Result.AddPage(NewNyxPage('home').WithText('Workflow'));
    LPlan := DiagramFixture;
    LPlan := LPlan.WithVertex(LPlan.Vertex(0).Captioned('Start / 🌙 / 漢字 / O''Brien')
      .AtBounds(NyxCanvasBounds(-120.125, 42.5, 120.25, 80.5))
      .Styled(NyxDrawingStyle.Fill(NyxDrawingPaint(NyxRGB(13, 27, 245)))
        .Stroke(NyxThemePaint(ntcAccentText), 2.5)));
    LPlan := LPlan.WithEdge(LPlan.Edge(0).Arrows(ndaOpen, ndaNone)
      .Styled(NyxDrawingStyle.Fill(NyxDrawingPaint(NyxNoColor))
        .Stroke(NyxDrawingPaint(NyxRGB(25, 50, 75)), 1.25)));
    Result.Diagrams.Define(NyxDiagramRef('Publishing workflow'), LPlan)
      .Define(NyxDiagramRef('Empty board'), NewNyxDiagram);
    Result.Validate;
  except
    Result.Free;
    raise;
  end;
end;

end.
