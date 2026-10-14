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
unit nyx.generated.view;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.types, nyx.model, nyx.controls, nyx.root.types,
  nyx.canvas.types, nyx.canvas.geometry, nyx.canvas.declarations;

function BuildNyxDocument: TNyxDocument;

implementation

{ These deliberately computed helpers remain outside the designer's continuation.
  A later scene edit must not replace unrelated values with serialized literals. }
function SceneSpacing: Double;
begin
  Result := 12.25 * 2;
end;

function EditorCaption: TNyxText;
begin
  Result := 'Handwritten ' + 'scene';
end;

function ScenePlan(const AContent: TNyxRootRef): INyxCanvasDefinition;
var
  LLayer: TNyxCanvasLayerRef;
begin
  LLayer := NyxCanvasLayer('Controls');
  Result := NewNyxCanvasDefinition
    .WithCamera(NyxCanvasCamera(400, 300, NyxCanvasPoint(0, 0)))
    .WithGrid(NyxCanvasGrid(SceneSpacing, ncgsDots, ncsmFree))
    .WithLayer(LLayer)
    .WithItem(NyxCanvasItem('Editor'), AContent,
      NyxCanvasPlacement(LLayer, NyxCanvasBounds(-150, -100, 300, 200)));
end;

function BuildNyxDocument: TNyxDocument;
begin
  Result := TNyxDocument.Create;
  try
    Result.Title := 'Handwritten canvas workshop';
    Result.AddPage(NewNyxPage('home'));
    Result.AddComponent(NewNyxMemo('first-content').Configure.Value(EditorCaption).Done);
    Result.AddComponent(NewNyxMemo('second-content').Configure.Value('Second scene').Done);
    Result.Canvases.Define(NyxCanvasRef('First'), ScenePlan(NyxReusableRoot('first-content')));
    Result.Canvases.Define(NyxCanvasRef('Second'), ScenePlan(NyxReusableRoot('second-content')));
    Result.AddComponent(NewNyxCanvas('canvas-template').WithScene(NyxCanvasRef('First'))
      .Configure.PartName(NyxPart('board')).Height(350).MinimumHeight(300).Done);
    Result.Pages[0].Add(NewNyxComponent('inherited')
      .Configure.Component(NyxComponent('canvas-template')).Done);
    Result.Pages[0].Add(NewNyxComponent('alternate')
      .Configure.Component(NyxComponent('canvas-template')).CanvasScene(NyxCanvasRef('Second')).Done);
    Result.Pages[0].Add(NewNyxComponent('empty')
      .Configure.Component(NyxComponent('canvas-template')).NoCanvasScene.Done);
    Result.AddComponent(NewNyxCard('wrapper-template')
      .Add(NewNyxCanvas('wrapped-board').WithScene(NyxCanvasRef('First'))
        .Configure.PartName(NyxPart('board')).Height(350).Done));
    Result.Pages[0].Add(NewNyxComponent('wrapped')
      .Configure.Component(NyxComponent('wrapper-template')).Done);
    Result.Find('wrapped').OverridePart(NyxPart('board')).Named('wrapped-scene')
      .Configure.CanvasScene(NyxCanvasRef('Second')).Done;
    Result.Validate;
  except
    Result.Free;
    raise;
  end;
end;

end.
