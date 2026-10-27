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
program nyx_canvas_uniform_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, Math, nyx.text, nyx.data, nyx.model, nyx.controls, nyx.codec,
  nyx.codegen, nyx.source, nyx.root.types, nyx.canvas.geometry, nyx.canvas.types,
  nyx.canvas.declarations, nyx.canvas.workspace, nyx.canvas.navigation,
  nyx.canvas.precision;

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AMessage: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AMessage);
  end;
  Inc(GChecks);
end;

{ Explicit persistence-boundary edit, leaving the immutable input untouched. }
function ReplaceField(const AValue: TNyxDataValue; const AKey: TNyxText;
  const AReplacement: TNyxDataValue): TNyxDataValue;
var
  LFields: array of TNyxDataField;
  LIndex: Integer;
begin
  SetLength(LFields, AValue.Count);
  for LIndex := 0 to AValue.Count - 1 do
  begin
    LFields[LIndex] := NyxField(AValue.Key(LIndex), AValue.Field(AValue.Key(LIndex)));

    if AValue.Key(LIndex) = AKey then
    begin
      LFields[LIndex] := NyxField(AKey, AReplacement);
    end;
  end;
  Result := NyxObject(LFields);
end;

procedure Refuses(const AValue: TNyxDataValue; const AMessage: TNyxText);
var
  LRejected: Boolean;
begin
  LRejected := False;
  try
    NyxCanvasDefinitionFromData(AValue);
  except
    on Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, AMessage);
end;

procedure Run;
var
  LCamera: TNyxCanvasCamera;
  LFrame: TNyxCanvasControlFrame;
  LBounds: TNyxCanvasBounds;
  LDefinition: INyxCanvasDefinition;
  LCopy: INyxCanvasDefinition;
  LLegacy: INyxCanvasDefinition;
  LDocument: TNyxDocument;
  LParsed: TNyxDocument;
  LSourceOwner: TNyxSourceWorkspace;
  LSource: TNyxText;
  LWire: TNyxDataValue;
  LBefore: TNyxText;
  LBoundary: Integer;
  LPrecision: Integer;
  LRejected: Boolean;
  LNavigation: TNyxCanvasNavigationResult;
  LItem: TNyxCanvasNavigationItem;
begin
  Check((Ord(nccsReflow) = 0) and (Ord(nccsLogicalSize) = 1) and
    (Ord(nccsUniform) = 2), 'existing scaling ordinals remain stable');
  LCamera := NyxCanvasCamera(1280, 720, NyxCanvasPoint(0, 0), 0.5);
  LBounds := NyxCanvasBounds(0, 0, 450, 960);
  LFrame := NyxCanvasControlFrame(LCamera, LBounds, nccsUniform);
  Check(LFrame.Visible and (LFrame.Width = 225) and (LFrame.Height = 480),
    'uniform physical frame follows camera scale');
  Check((LFrame.LayoutWidth = 450) and (LFrame.LayoutHeight = 960) and
    SameValue(LFrame.ContentScale, 0.5), 'uniform content retains the world layout');
  LFrame := NyxCanvasControlFrame(LCamera, LBounds, nccsReflow);
  Check((LFrame.LayoutWidth = 225) and (LFrame.LayoutHeight = 480) and
    (LFrame.ContentScale = 1), 'legacy reflow retains unchanged typography');
  LFrame := NyxCanvasControlFrame(LCamera, LBounds, nccsLogicalSize);
  Check((LFrame.Width = 450) and (LFrame.Height = 960) and
    (LFrame.LayoutWidth = 450) and (LFrame.ContentScale = 1),
    'legacy fixed allocation remains independent of zoom');
  LFrame := NyxCanvasControlFrame(LCamera,
    NyxCanvasBounds(1.0e10, 0, 1.0e8, 1.0e8), nccsUniform);
  Check(not LFrame.Visible and (LFrame.LayoutWidth = 0),
    'offscreen culling precedes uniform logical allocation');
  LRejected := False;
  try
    LFrame := NyxCanvasControlFrame(
      NyxCanvasCamera(1280, 720, NyxCanvasPoint(0, 0), 0.001,
        NyxCanvasZoomLimits(0.001, 16)),
      NyxCanvasBounds(0, 0, 40000, 40000), nccsUniform);
  except
    on Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'small zoom cannot admit an oversized logical layout');
  LItem := NyxCanvasNavigationItem(LBounds, nccsUniform);
  LNavigation := FitNyxCanvasNavigation(LCamera, [LItem], [], NyxCanvasNavigationOptions);
  LFrame := NyxCanvasControlFrame(LNavigation.Camera, LBounds, nccsUniform);
  Check((LNavigation.Status = ncnsComplete) and (LFrame.Top >= 0) and
    (LFrame.Top + LFrame.Height <= LCamera.ViewportHeight + 1),
    'uniform navigation fits the same scaled world extent');

  for LBoundary := 0 to 1 do
  begin
    for LPrecision := 0 to 1 do
    begin
      LCamera := NyxCanvasCamera(1280, 720, NyxCanvasPoint(0, 0));

      if LBoundary = 1 then
      begin
        LCamera := LCamera.Within(NyxCanvasBounds(-2000, -2000, 4000, 4000));
      end;
      LLegacy := NewNyxCanvasDefinition.WithCamera(LCamera)
        .WithLayer(NyxCanvasLayer('Content'));

      if LPrecision = 1 then
      begin
        LLegacy := NyxCanvasPrecision(LLegacy).Options(NyxCanvasPrecisionOptions
          .WithRulers(NyxCanvasRulers.Axes([ncaxX, ncaxY]))).Done;
      end;
      LBefore := LLegacy.ToData.ToJSON;
      LDefinition := LLegacy.WithScaling(nccsUniform);
      LWire := LDefinition.ToData;
      Check(LWire.Field('version').AsInteger = 6, 'uniform opts into version six');
      LCopy := NyxCanvasDefinitionFromData(LWire);
      Check((LCopy.Scaling = nccsUniform) and (LCopy.ToData.ToJSON = LWire.ToJSON),
        'uniform shape round trips camera boundary and optional precision');
      Check(LDefinition.WithScaling(nccsReflow).ToData.ToJSON = LBefore,
        'clearing uniform restores the exact older descriptor bytes');
      Refuses(ReplaceField(LWire, 'version', NyxData(5)),
        'an older version cannot claim uniform scaling');
      Refuses(ReplaceField(LWire, 'scaling', NyxData('reflow')),
        'version six requires its explicit uniform intent');
    end;
  end;

  LDocument := TNyxDocument.Create;
  LParsed := nil;
  LSourceOwner := nil;
  try
    LDocument.Title := 'A live canvas 🌙';
    LDocument.AddPage(NewNyxPage('home'));
    LDocument.AddComponent(NewNyxCard('notebook'));
    LDefinition := NewNyxCanvasDefinition.WithScaling(nccsUniform)
      .WithLayer(NyxCanvasLayer('Content'))
      .WithItem(NyxCanvasItem('Notebook'), NyxReusableRoot('notebook'),
        NyxCanvasPlacement(NyxCanvasLayer('Content'), LBounds));
    LDocument.Canvases.Define(NyxCanvasRef('Workspace'), LDefinition);
    LDocument.Pages[0].Add(NewNyxCanvas('workspace').WithScene(NyxCanvasRef('Workspace')));
    LBefore := TNyxCodec.Encode(LDocument);
    LSource := TNyxCodegen.Generate(LDocument);
    Check(Pos('.WithScaling(nccsUniform)', LSource) > 0,
      'crafted source emits the typed new choice');
    LParsed := TNyxSourceWorkspace.PrepareDraft(LSource, LSourceOwner);
    Check(TNyxCodec.Encode(LParsed) = LBefore,
      'ordinary source admission preserves the exact document and new choice');
  finally
    LSourceOwner.Free;
    LParsed.Free;
    LDocument.Free;
  end;
end;

begin
  try
    Run;
    WriteLn('PASS ', GChecks, ' uniform canvas contract checks');
  except
    on LException: Exception do
    begin
      WriteLn('FAIL after ', GChecks, ' checks / ', LException.Message);
      ExitCode := 1;
    end;
  end;
end.
