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

unit nyx.test.graph.only;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.model, nyx.flow.declarations;

const
  GraphOnlyName: TNyxText = 'Data workshop';
  GraphOnlyInput: TNyxText = '{"message":"Hello from Pascal","items":[3,1,2]}';

{ English application with an actually empty saved scene and unrelated reusable
  content. The latter must survive source adoption without entering Logic's
  metadata-only working context. Each caller owns an independent document. }
function GraphOnlyDocument: TNyxDocument;
{ Execute the declared built-in JSON pipeline explicitly, with no file/network
  host. The returned text is independent of the graph, application and UI. }
function RunGraphOnly(const AFlows: INyxFlowDeclarations): TNyxText;

implementation

uses SysUtils, nyx.types, nyx.state, nyx.controls, nyx.canvas.declarations,
  nyx.canvas.types, nyx.canvas.geometry, nyx.flow, nyx.flow.types, nyx.logic;

function GraphOnlyDocument: TNyxDocument;
var
  LPage: INyxPage;
  LTools: INyxToolCanvas;
  LStatus: INyxLabel;
  LUnused: INyxColumn;
  LIndex: Integer;
begin
  Result := TNyxDocument.Create;
  try
    Result.Title := 'A workspace for data';
    Result.State.SetValue(NyxTextState('result'), 'Not run');
    Result.Canvases.Define(NyxCanvasRef('Data canvas'), NewNyxCanvasDefinition
      .WithCamera(NyxCanvasCamera(1000, 640, NyxCanvasPoint(0, 0), 1))
      .WithLayer(NyxCanvasLayer('Workspace')));
    LPage := NewNyxPage('home');
    LPage.Add(NewNyxHeading('title').WithText('Build a data tool'));
    LPage.Add(NewNyxButton('run-graph').WithText('Run data tool'));
    LStatus := NewNyxLabel('result-label');
    LStatus.Binds.Text(NyxTextState('result')).Done;
    LPage.Add(LStatus);
    LTools := NewNyxToolCanvas('data-tools').WithScene(NyxCanvasRef('Data canvas'));
    LTools.WorkspaceCanvas.Configure.Height(640).MinimumHeight(400).Done;
    LTools.ToolsCard.Configure.Visible(True).Done;
    LPage.Add(LTools);
    Result.AddPage(LPage);
    LUnused := NewNyxColumn('unrelated-library');
    for LIndex := 1 to 256 do
    begin
      LUnused.Add(NewNyxLabel('unrelated-label-' + TNyxText(IntToStr(LIndex)))
        .WithText('Unrelated content ' + TNyxText(IntToStr(LIndex))));
    end;
    Result.AddComponent(LUnused);
    Result.Validate;
  except
    Result.Free;
    raise;
  end;
end;

function RunGraphOnly(const AFlows: INyxFlowDeclarations): TNyxText;
var
  LGraph: INyxFlowGraph;
  LResult: INyxFlowResult;
begin
  LGraph := AFlows.Definition(NyxFlowRef(GraphOnlyName)).Build;
  LResult := LGraph.Evaluate([NyxFlowInput(
    NyxFlowEndpoint(NyxFlowStep('parse'), NyxLogicPort(nlpText)), NyxFlowText(GraphOnlyInput))]);
  Result := LResult.Value(NyxFlowEndpoint(NyxFlowStep('format'), NyxLogicPort(nlpText))).AsText;
end;

end.
