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

unit nyx.test.canvas.graph.layout;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.model;

{ English owned authoring fixture, with supplementary Unicode confined to exact
  identity qualification. The caller owns the document and all its descendants. }
function GraphLayoutDocument: TNyxDocument;
{ Distinct application identities stress scalar-preserving query/transaction
  boundaries without changing the English starter/demo text contract. }
function GraphLayoutFlowName: TNyxText;
function GraphLayoutStepName: TNyxText;

implementation

uses nyx.controls, nyx.types, nyx.root.types, nyx.canvas.types, nyx.canvas.geometry,
  nyx.canvas.declarations, nyx.flow, nyx.flow.types, nyx.flow.declarations,
  nyx.flow.routes, nyx.logic;

function GraphLayoutFlowName: TNyxText;
begin
  Result := 'Data ' + NyxScalarText($1F680);
end;

function GraphLayoutStepName: TNyxText;
begin
  Result := 'Read ' + NyxScalarText($1F9ED);
end;

function GraphLayoutDocument: TNyxDocument;
var
  LPage: INyxPage;
  LNotes: INyxColumn;
  LScene: INyxCanvasDefinition;
begin
  Result := TNyxDocument.Create;
  try
    Result.Title := 'An orderly workspace';
    Result.Flows.Define(NyxFlowRef(GraphLayoutFlowName), NewNyxFlowGraph
      .WithStep(NyxFlowStep(GraphLayoutStepName), NewNyxJSONParse)
      .WithStep(NyxFlowStep('Format'), NewNyxJSONFormat)
      .Connect(NyxFlowEndpoint(NyxFlowStep(GraphLayoutStepName), NyxLogicPort(nlpJSON)),
        NyxFlowEndpoint(NyxFlowStep('Format'), NyxLogicPort(nlpJSON))).Build);
    LNotes := NewNyxColumn('notes-form');
    LNotes.Add(NewNyxLabel('note-caption').WithText('Working notes'));
    LNotes.Add(NewNyxInput('note-input').Configure.Value('Keep this exact note').Done);
    Result.AddComponent(LNotes);
    LScene := NewNyxCanvasDefinition.WithLayer(NyxCanvasLayer('Content'))
      .WithCamera(NyxCanvasCamera(800, 600, NyxCanvasPoint(25, -50), 1.25))
      .WithScaling(nccsLogicalSize)
      .WithItem(NyxCanvasItem('Note'), NyxReusableRoot('notes-form'),
        NyxCanvasPlacement(NyxCanvasLayer('Content'), NyxCanvasBounds(-200, -100, 300, 200)));
    LScene := NyxCanvasConnections(LScene).ForControl(NyxCanvasItem('Note'),
      NyxControl('note-input'), NewNyxFlowRoutes).Done;
    LScene := NyxCanvasGraphLayout(LScene)
      .Place(NyxFlowRef(GraphLayoutFlowName), NyxFlowStep(GraphLayoutStepName),
        NyxCanvasPoint(64.25, -128.5))
      .Place(NyxFlowRef('Future graph'), NyxFlowStep('Arrange'), NyxCanvasPoint(0.75, 4)).Done;
    Result.Canvases.Define(NyxCanvasRef('Layout board'), LScene);
    Result.Canvases.Define(NyxCanvasRef('Other board'),
      NyxCanvasGraphLayout(LScene).Place(NyxFlowRef(GraphLayoutFlowName),
        NyxFlowStep(GraphLayoutStepName), NyxCanvasPoint(901, 902)).Done);
    LPage := NewNyxPage('home');
    LPage.Add(NewNyxToolCanvas('layout-tools').WithScene(NyxCanvasRef('Layout board')));
    Result.AddPage(LPage);
    Result.Validate;
  except
    Result.Free;
    raise;
  end;
end;

end.

