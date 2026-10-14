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
unit nyx.test.drawing.fixture;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.types, nyx.model, nyx.controls, nyx.drawing, nyx.canvas.geometry,
  nyx.canvas.types, nyx.canvas.declarations, nyx.root.types, nyx.resources,
  nyx.binding.types, nyx.images, nyx.image.fixtures, nyx.design.tokens, nyx.state;

function DrawingFixture: TNyxDocument;
function DrawingFixturePlan: INyxDrawingPlan;

implementation

function DrawingFixturePlan: INyxDrawingPlan;
begin
  Result := NewNyxDrawingPlan(NyxCanvasBounds(0, 0, 320, 180))
    .Path(NyxDrawingRectangle(NyxCanvasBounds(12, 12, 296, 156)),
      NyxDrawingStyle.Fill(NyxThemePaint(ntcSurface))
        .Stroke(NyxThemePaint(ntcAccent), 2))
    .Path(NyxDrawingPath(NyxCanvasPoint(36, 128))
      .CurveTo(NyxCanvasPoint(104, 18), NyxCanvasPoint(210, 164), NyxCanvasPoint(282, 54)),
      NyxDrawingStyle.Stroke(NyxThemePaint(ntcAccent), 4))
    .Text('Make connections', NyxCanvasPoint(32, 30), NyxThemePaint(ntcText),
      18, ndtaStart, True)
    .Text('Paths, controls and resources', NyxCanvasPoint(32, 60),
      NyxThemePaint(ntcMuted), 12)
    .Build;
end;

function DrawingFixture: TNyxDocument;
var
  LScene: INyxCanvasDefinition;
  LLayer: TNyxCanvasLayerRef;
  LStatic: INyxDrawing;
  LDynamic: INyxDrawing;
  LMemo: INyxMemo;
  LImage: INyxImage;
  LCanvas: INyxCanvas;
begin
  Result := TNyxDocument.Create;
  try
    Result.Title := 'Canvas workshop';
    Result.AddPage(NewNyxPage('home'));
    Result.Pages[0].Configure.HeightSizing(nsFill).WidthSizing(nsFill).Done;
    LStatic := NewNyxDrawing('connection-graphic').WithText('Connection graphic')
      .WithPlan(DrawingFixturePlan);
    LStatic.Configure.HeightSizing(nsFill).MinimumHeight(0).Done;
    Result.AddComponent(LStatic);
    LDynamic := NewNyxDrawing('provider-graphic').WithText('Custom provider')
      .WithPlan(DrawingFixturePlan).WithProvider(NyxDrawingProvider('workshop-provider'));
    LDynamic.Configure.HeightSizing(nsFill).MinimumHeight(0).Done;
    Result.AddComponent(LDynamic);
    LMemo := NewNyxMemo('notes-memo').WithText('Notes');
    LMemo.Configure.HeightSizing(nsFill).MinimumHeight(0).Done;
    Result.State.SetValue(NyxTextState('notes'), 'Connect your ideas');
    LMemo.Binds.Value(NyxTextState('notes')).Done;
    Result.AddComponent(LMemo);
    Result.Resources.Define(NyxResourceRef('workshop-image'),
      NyxImageResource(NyxEmbeddedImage(nimPNG, ImagePNG)));
    LImage := NewNyxImage('resource-image').WithText('Packed workshop resource');
    LImage.Configure.HeightSizing(nsFill).MinimumHeight(0).Done;
    LImage.Binds.Image(NyxResourceImage(NyxResourceRef('workshop-image'))).Done;
    Result.AddComponent(LImage);
    LLayer := NyxCanvasLayer('Content');
    LScene := NewNyxCanvasDefinition
      .WithCamera(NyxCanvasCamera(800, 500, NyxCanvasPoint(0, 0), 1))
      .WithGrid(NyxCanvasGrid(20, ncgsDots))
      .WithScaling(nccsReflow)
      .WithLayer(LLayer)
      .WithItem(NyxCanvasItem('graphic'), NyxReusableRoot('connection-graphic'),
        NyxCanvasPlacement(LLayer, NyxCanvasBounds(-350, -190, 320, 180)))
      .WithItem(NyxCanvasItem('provider'), NyxReusableRoot('provider-graphic'),
        NyxCanvasPlacement(LLayer, NyxCanvasBounds(30, -190, 320, 180)))
      .WithItem(NyxCanvasItem('memo'), NyxReusableRoot('notes-memo'),
        NyxCanvasPlacement(LLayer, NyxCanvasBounds(-350, 40, 320, 130)))
      .WithItem(NyxCanvasItem('image'), NyxReusableRoot('resource-image'),
        NyxCanvasPlacement(LLayer, NyxCanvasBounds(30, 40, 240, 120)));
    Result.Canvases.Define(NyxCanvasRef('workshop'), LScene);
    LCanvas := NewNyxCanvas('workspace').WithScene(NyxCanvasRef('workshop'));
    LCanvas.Configure.Height(550).Done;
    Result.Pages[0].Add(LCanvas);
    Result.Validate;
  except
    Result.Free;
    raise;
  end;
end;

end.
