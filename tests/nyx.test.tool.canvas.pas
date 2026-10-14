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


unit nyx.test.tool.canvas;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.model;

function ToolCanvasDocument: TNyxDocument;

implementation

uses
  nyx.text, nyx.types, nyx.controls, nyx.state, nyx.callbacks,
  nyx.flow.route.model, nyx.flow.routes, nyx.canvas.types, nyx.canvas.geometry,
  nyx.canvas.declarations, nyx.root.types, nyx.test.flow.routes;

function ToolCanvasDocument: TNyxDocument;
var
  LEditor: INyxCard;
  LOutput: INyxCard;
  LCanvas: INyxToolCanvas;
begin
  Result := RoutedLogicDocument;
  try
    Result.Title := 'Connected tools';
    LEditor := NewNyxCard('editor-template');
    LEditor.Add(ExtractNyxControl(Result, NyxControl('source-card')));
    LOutput := NewNyxCard('output-template');
    LOutput.Add(ExtractNyxControl(Result, NyxControl('pretty-code')));
    LOutput.Add(ExtractNyxControl(Result, NyxControl('compact-label')));
    Result.AddComponent(LEditor);
    Result.AddComponent(LOutput);
    SetNyxFlowRoutes(Result.Find('run-button'), NewNyxFlowRoutes.WithRoute(
      ProfileRoute(nfcmAfterDispatch)));
    NyxCallbacks(Result.Find('run-button')).OnClick
      .Add(NyxHandler('TToolRunObserver'), NyxCallbackID('ordinary-observer'));
    Result.Canvases.Define(NyxCanvasRef('tools'), NewNyxCanvasDefinition
      .WithCamera(NyxCanvasCamera(1000, 650, NyxCanvasPoint(0, 0), 1))
      .WithLayer(NyxCanvasLayer('Controls'))
      .WithItem(NyxCanvasItem('Editor'), NyxReusableRoot('editor-template'),
        NyxCanvasPlacement(NyxCanvasLayer('Controls'), NyxCanvasBounds(10, 10, 390, 340)))
      .WithItem(NyxCanvasItem('Output'), NyxReusableRoot('output-template'),
        NyxCanvasPlacement(NyxCanvasLayer('Controls'), NyxCanvasBounds(440, 10, 390, 340))));
    LCanvas := NewNyxToolCanvas('tool-canvas').WithScene(NyxCanvasRef('tools'));
    LCanvas.WorkspaceCanvas.Configure.Height(590).MinimumHeight(400).Done;
    Result.Pages[0].Add(LCanvas);
    Result.Validate;
  except
    Result.Free;
    raise;
  end;
end;

end.
