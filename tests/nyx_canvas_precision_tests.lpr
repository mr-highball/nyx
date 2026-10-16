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
program nyx_canvas_precision_tests;
{$mode delphi}{$H+}{$codepage utf8}
uses SysUtils, Math, nyx.text, nyx.data, nyx.canvas.geometry,
  nyx.canvas.precision, nyx.canvas.declarations, nyx.flow.declarations,
  nyx.flow.types {$ifdef PAS2JS}, Web{$endif};

var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AReason: String);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

procedure Near(AActual, AExpected: Double; const AReason: String);
begin
  Check(Abs(AActual - AExpected) <= 1.0e-7, AReason);
end;

procedure Run;
var
  LBase: TNyxCanvasPrecisionOptions;
  LChanged: TNyxCanvasPrecisionOptions;
  LRead: TNyxCanvasPrecisionOptions;
  LRulers: TNyxCanvasRulers;
  LCamera: TNyxCanvasCamera;
  LTicks: TNyxCanvasRulerTicks;
  LTick: TNyxCanvasRulerTick;
  LSnap: TNyxCanvasGuideSnap;
  LExclude: TNyxCanvasGuideRef;
  LBefore: TNyxText;
  LIndex: Integer;
  LCase: Integer;
  LFailed: Boolean;
  LMajorCount: Integer;
  LLastX: Double;
  LLastY: Double;
  LDefinition: INyxCanvasDefinition;
  LConfigured: INyxCanvasDefinition;
  LRestored: INyxCanvasDefinition;
begin
  LExclude := Default(TNyxCanvasGuideRef);
  LBase := NyxCanvasPrecisionOptions
    .WithRulers(NyxCanvasRulers.Units(10, 'cm').Interval(0.1))
    .WithGuide(NyxCanvasGuide(NyxCanvasGuideRef('Left margin'), ncaxX, -25.5))
    .WithGuide(NyxCanvasGuide(NyxCanvasGuideRef('Top margin'), ncaxY, 30.25).Locked(True))
    .Snapping(ncgspVisible, 8);
  LBefore := LBase.ToData.ToJSON;
  LChanged := LBase.WithGuide(LBase.Guide(0).AtPosition(2)).WithoutGuide(LBase.Guide(1).Reference);
  Check((LChanged.Count = 1) and (LBase.Count = 2), 'removing a branch never resizes its baseline');
  Near(LBase.Guide(0).Position, -25.5, 'moving a branch preserves the earlier coordinate');
  Check(LBase.ToData.ToJSON = LBefore, 'branch edits retain every baseline field exactly');
  LChanged := LBase.WithGuide(LBase.Guide(0).Locked(True));
  Check((LChanged.Guide(0).Reference.Name = 'Left margin') and
    (LChanged.Guide(1).Reference.Name = 'Top margin'), 'updates retain declaration order');
  LRead := TNyxCanvasPrecisionOptions.FromData(LBase.ToData);
  Check(LRead.ToData.ToJSON = LBefore, 'precision options preserve exact round-trip bytes');
  LRead := LRead.WithGuide(NyxCanvasGuide(NyxCanvasGuideRef('Marge 🌙'), ncaxX, 0.125));
  LRead := TNyxCanvasPrecisionOptions.FromData(LRead.ToData);
  Check(LRead.Guide(2).Reference.Name = TNyxText('Marge 🌙'), 'dedicated Unicode guide retains supplementary scalars');
  Check(LBase.NextGuide.Name = 'Guide 1', 'default guide names are English and independent');
  LChanged := LBase.WithGuide(NyxCanvasGuide(LBase.NextGuide, ncaxX, 0));
  Check(LChanged.NextGuide.Name = 'Guide 2', 'automatic names never replace an existing guide');
  LCamera := NyxCanvasCamera(800, 600, NyxCanvasPoint(0, 0), 2);
  LSnap := SnapNyxCanvasGuides(LBase, NyxCanvasPoint(-22, 32), LCamera, LExclude);
  Near(LSnap.Point.X, -25.5, 'snap tolerance is in logical pixels');
  Near(LSnap.Point.Y, 30.25, 'locked guides still provide alignment references');
  Check(LSnap.XGuide.Defined and LSnap.YGuide.Defined, 'independent axes report their exact guide hits');
  LSnap := SnapNyxCanvasGuides(LBase, NyxCanvasPoint(-20, 32), LCamera, LExclude);
  Near(LSnap.Point.X, -20, 'out-of-tolerance coordinates stay untouched');
  LSnap := SnapNyxCanvasGuides(LBase, NyxCanvasPoint(-22, 32), LCamera, LBase.Guide(0).Reference);
  Check(not LSnap.XGuide.Defined and LSnap.YGuide.Defined, 'a moving guide can exclude its own old coordinate');
  LChanged := LBase.WithGuide(LBase.Guide(0).Visible(False));
  LSnap := SnapNyxCanvasGuides(LChanged, NyxCanvasPoint(-22, 32), LCamera, LExclude);
  Check(not LSnap.XGuide.Defined, 'visible-only snapping ignores hidden guides');
  LSnap := SnapNyxCanvasGuides(LChanged.Snapping(ncgspAll), NyxCanvasPoint(-22, 32), LCamera, LExclude);
  Check(LSnap.XGuide.Defined, 'caller can deliberately include hidden guides');
  LSnap := SnapNyxCanvasGuides(LBase.Snapping(ncgspOff), NyxCanvasPoint(-22, 32), LCamera, LExclude);
  Check(not LSnap.XGuide.Defined and not LSnap.YGuide.Defined, 'snapping can be disabled independently of rulers');

  LRulers := LBase.Rulers.FromOrigin(NyxCanvasPoint(15, -20));
  for LCase := 0 to 5 do
  begin
    case LCase of
      0: LCamera := NyxCanvasCamera(800, 600, NyxCanvasPoint(0, 0), 1);
      1: LCamera := NyxCanvasCamera(390, 720, NyxCanvasPoint(-23.25, 71.5), 2);
      2: LCamera := NyxCanvasCamera(800, 600, NyxCanvasPoint(0, 0), 0.05);
      3: LCamera := NyxCanvasCamera(32768, 32768, NyxCanvasPoint(0, 0), 16);
      4: LCamera := NyxCanvasCamera(800, 600, NyxCanvasPoint(9.99e11, -9.99e11), 16);
      5: LCamera := NyxCanvasCamera(1, 1, NyxCanvasPoint(0, 0), 1);
    end;
    LTicks := NyxCanvasRulerTicks(LRulers, LCamera);
    Check(Length(LTicks) <= MaximumNyxCanvasRulerTicks, 'projection remains bounded without off-screen enumeration');
    LMajorCount := 0;
    LLastX := -1;
    LLastY := -1;
    for LIndex := 0 to High(LTicks) do
    begin
      LTick := LTicks[LIndex];

      if LTick.Axis = ncaxX then
      begin
        Check((LTick.Pixel >= LLastX) and (LTick.Pixel <= LCamera.ViewportWidth), 'X ticks are ordered within the viewport');
        LLastX := LTick.Pixel;
        Near(LTick.Pixel, (LRulers.Origin.X + LTick.Value * LRulers.UnitSize -
          LCamera.WorldCenter.X) * LCamera.Zoom + LCamera.ViewportWidth / 2,
          'X tick position has an independent world-to-viewport oracle');
      end
      else
      begin
        Check((LTick.Pixel >= LLastY) and (LTick.Pixel <= LCamera.ViewportHeight), 'Y ticks are ordered within the viewport');
        LLastY := LTick.Pixel;
        Near(LTick.Pixel, (LRulers.Origin.Y + LTick.Value * LRulers.UnitSize -
          LCamera.WorldCenter.Y) * LCamera.Zoom + LCamera.ViewportHeight / 2,
          'Y tick position has an independent world-to-viewport oracle');
      end;

      if LTick.Major then
      begin
        Inc(LMajorCount);
        Check((LTick.Caption <> '') and (LTick.Caption <> '-0'), 'major labels are readable and normalize negative zero');
      end
      else
      begin
        Check(LTick.Caption = '', 'minor ticks do not crowd labels');
      end;
    end;

    if LCase < 4 then
    begin
      Check(LMajorCount >= 2, 'ordinary camera scales retain useful labels on both axes');
    end;
  end;
  LCamera := NyxCanvasCamera(800, 600, NyxCanvasPoint(0, 0), 1);
  LTicks := NyxCanvasRulerTicks(LRulers.Axes([ncaxX]), LCamera);
  for LIndex := 0 to High(LTicks) do
  begin
    Check(LTicks[LIndex].Axis = ncaxX, 'ruler axes can be independently hidden');
  end;
  LFailed := False;
  try
    LChanged := LBase.Snapping(ncgspVisible, -1);
  except
    on Exception do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and (LBase.ToData.ToJSON = LBefore), 'invalid policy refuses with exact baseline preservation');
  LChanged := NyxCanvasPrecisionOptions;
  for LIndex := 1 to MaximumNyxCanvasGuides do
  begin
    LChanged := LChanged.WithGuide(NyxCanvasGuide(LChanged.NextGuide, ncaxX, LIndex));
  end;
  LBefore := LChanged.ToData.ToJSON;
  LFailed := False;
  try
    LChanged.WithGuide(NyxCanvasGuide(LChanged.NextGuide, ncaxX, 0));
  except
    on Exception do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and (LChanged.ToData.ToJSON = LBefore), 'capacity refuses instead of truncating or replacing a guide');

  LDefinition := NewNyxCanvasDefinition.WithCamera(LCamera.Within(
    NyxCanvasBounds(-1000, -1000, 2000, 2000)));
  LBefore := LDefinition.ToData.ToJSON;
  LConfigured := NyxCanvasPrecision(LDefinition).Options(LBase).Done;
  Check(LDefinition.ToData.ToJSON = LBefore, 'saved precision builder preserves the exact prior branch');
  Check(LConfigured.ToData.Field('version').AsInteger = 5, 'explicit precision selects descriptor version five');
  LRestored := NyxCanvasDefinitionFromData(LConfigured.ToData);
  Check(LRestored.ToData.ToJSON = LConfigured.ToData.ToJSON, 'complete saved precision plan round trips exactly');
  Check(NyxCanvasPrecisionOptionsOf(LRestored).ToData.ToJSON = LBase.ToData.ToJSON,
    'saved definition exposes typed independent precision intent');
  LConfigured := NyxCanvasGraphLayout(LRestored).Place(NyxFlowRef('Dormant graph'),
    NyxFlowStep('Dormant step'), NyxCanvasPoint(1, 2)).Done;
  Check(NyxCanvasPrecisionOptionsOf(LConfigured).ToData.ToJSON = LBase.ToData.ToJSON,
    'existing graph layout authoring preserves precision intent');
  Check(NyxCanvasDefinitionFromData(LConfigured.ToData).ToData.ToJSON = LConfigured.ToData.ToJSON,
    'precision and graph layout share exact descriptor membership');
  LConfigured := NyxCanvasPrecision(LRestored).Options(Default(TNyxCanvasPrecisionOptions)).Done;
  Check(LConfigured.ToData.ToJSON = LBefore, 'explicit precision removal restores the entire legacy descriptor');
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-canvas-precision', 'passed');
    document.body.setAttribute('data-canvas-precision-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' precision geometry checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-canvas-precision', 'failed');
      document.body.setAttribute('data-canvas-precision-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
