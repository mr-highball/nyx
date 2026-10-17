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
program nyx_drawing_state_tests;

{$mode delphi}{$H+}{$codepage utf8}

{ Exercise the public borrowed-canvas contract in owned memory. All four path
  styles leave their specified final state, including invisible paths and failed
  theme resolution. No host, screenshot, file, timer or native-handle count is
  used as a substitute for the resulting pixels and public state. }
uses
  Interfaces, Forms, SysUtils, Types, Graphics,
  nyx.text, nyx.colors, nyx.theme, nyx.design.tokens, nyx.drawing,
  nyx.drawing.view, nyx.drawing.lcl, nyx.canvas.geometry;

type
  { Closed fill/stroke combinations exercise the painter's public state effects.
    Empty paint still clears both styles; it neither fills nor strokes a path. }
  TPathStyleCase = (pscEmpty, pscFill, pscStroke, pscFillStroke);

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AMessage: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Drawing state: ' + AMessage);
  end;
  Inc(GChecks);
end;

{ Install unrelated caller state after clearing the owned bitmap. The painter
  must preserve colors/width when that paint is absent, and never alter font,
  join or pen mode for a path-only command. }
procedure Prepare(ABitmap: TBitmap; AClipped: Boolean);
begin
  ABitmap.PixelFormat := pf24bit;
  ABitmap.SetSize(32, 32);
  ABitmap.Canvas.Clipping := False;
  ABitmap.Canvas.Brush.Style := bsSolid;
  ABitmap.Canvas.Brush.Color := clWhite;
  ABitmap.Canvas.FillRect(Types.Rect(0, 0, 32, 32));
  ABitmap.Canvas.Pen.Style := psDash;
  ABitmap.Canvas.Pen.Width := 7;
  ABitmap.Canvas.Pen.Color := clGreen;
  ABitmap.Canvas.Pen.JoinStyle := pjsRound;
  ABitmap.Canvas.Pen.Mode := pmCopy;
  ABitmap.Canvas.Brush.Style := bsDiagCross;
  ABitmap.Canvas.Brush.Color := clYellow;
  ABitmap.Canvas.Font.Name := 'Courier New';
  ABitmap.Canvas.Font.Height := -17;
  ABitmap.Canvas.Font.Color := clFuchsia;
  ABitmap.Canvas.Font.Style := [fsItalic, fsUnderline];

  if AClipped then
  begin
    ABitmap.Canvas.Clipping := True;
    ABitmap.Canvas.ClipRect := Types.Rect(0, 0, 0, 0);
  end;
end;

function Plan(const AStyle: TNyxDrawingStyle): INyxDrawingPlan;
begin
  Result := NewNyxDrawingPlan(NyxCanvasBounds(0, 0, 32, 32))
    .Path(NyxDrawingRectangle(NyxCanvasBounds(6, 6, 20, 20)), AStyle).Build;
end;

procedure CheckUnchangedPathState(ACanvas: TCanvas);
begin
  Check((ACanvas.Font.Name = 'Courier New') and (ACanvas.Font.Height = -17) and
    (ACanvas.Font.Color = clFuchsia) and (ACanvas.Font.Style = [fsItalic, fsUnderline]),
    'path-only painting preserves all installed font attributes');
  Check((ACanvas.Pen.JoinStyle = pjsRound) and (ACanvas.Pen.Mode = pmCopy),
    'caller join and transfer policies remain unchanged');
end;

{ Independent native primitive oracle: fill the rectangle with a null pen, then
  outline its closed boundary. It consumes no Drawing plan or renderer helpers. }
procedure ExpectedRectangle(ABitmap: TBitmap; AFill, AStroke, AClipped: Boolean);
var
  LPoints: array[0..4] of TPoint;
begin
  Prepare(ABitmap, AClipped);
  LPoints[0] := Types.Point(6, 6);
  LPoints[1] := Types.Point(26, 6);
  LPoints[2] := Types.Point(26, 26);
  LPoints[3] := Types.Point(6, 26);
  LPoints[4] := LPoints[0];

  if AFill then
  begin
    ABitmap.Canvas.Pen.Style := psClear;
    ABitmap.Canvas.Brush.Style := bsSolid;
    ABitmap.Canvas.Brush.Color := clRed;
    ABitmap.Canvas.Polygon(LPoints);
  end;

  if AStroke then
  begin
    ABitmap.Canvas.Brush.Style := bsClear;
    ABitmap.Canvas.Pen.Style := psSolid;
    ABitmap.Canvas.Pen.Width := 4;
    ABitmap.Canvas.Pen.Color := clBlue;
    ABitmap.Canvas.Polyline(LPoints);
  end;
  ABitmap.Canvas.Clipping := False;
end;

procedure ComparePixels(AActual, AExpected: TBitmap);
var
  LX: Integer;
  LY: Integer;
begin
  for LY := 0 to 31 do
  begin
    for LX := 0 to 31 do
    begin

      if ColorToRGB(AActual.Canvas.Pixels[LX, LY]) <>
        ColorToRGB(AExpected.Canvas.Pixels[LX, LY]) then
      begin
        raise Exception.Create('Drawing state pixels differ at ' + IntToStr(LX) + ',' + IntToStr(LY));
      end;
    end;
  end;
  Check(True, 'every pixel equals the independent native fill/outline oracle');
end;

procedure CheckStyles(ABitmap, AExpected: TBitmap);
var
  LCase: TPathStyleCase;
  LClipped: Boolean;
  LFill: Boolean;
  LStroke: Boolean;
  LStyle: TNyxDrawingStyle;
  LPlan: INyxDrawingPlan;
  LMapping: TNyxDrawingMapping;
  LRepeat: Integer;
  LExpectedCenter: TColor;
  LExpectedEdge: TColor;
begin
  for LCase := Low(TPathStyleCase) to High(TPathStyleCase) do
  begin
    LFill := LCase in [pscFill, pscFillStroke];
    LStroke := LCase in [pscStroke, pscFillStroke];
    LStyle := NyxDrawingStyle;

    if LFill then
    begin
      LStyle := LStyle.Fill(NyxDrawingPaint(NyxRGB(255, 0, 0)));
    end;

    if LStroke then
    begin
      LStyle := LStyle.Stroke(NyxDrawingPaint(NyxRGB(0, 0, 255)), 4);
    end;
    LPlan := Plan(LStyle);
    LMapping := NyxDrawingMapping(LPlan, 32, 32);
    for LClipped := False to True do
    begin
      Prepare(ABitmap, LClipped);
      ExpectedRectangle(AExpected, LFill, LStroke, LClipped);
      for LRepeat := 1 to 2 do
      begin
        { Literal RGB needs no theme. Repetition uses the same canvas/plan, so
          prior native resources cannot supply a stale or different final face. }
        PaintNyxLCLDrawing(ABitmap.Canvas, LPlan, LMapping, nil);
        CheckUnchangedPathState(ABitmap.Canvas);
        Check(((not LStroke) and (ABitmap.Canvas.Pen.Style = psClear) and
          (ABitmap.Canvas.Pen.Width = 7) and (ABitmap.Canvas.Pen.Color = clGreen)) or
          (LStroke and (ABitmap.Canvas.Pen.Style = psSolid) and
          (ABitmap.Canvas.Pen.Width = 4) and (ABitmap.Canvas.Pen.Color = clBlue)),
          'stroke presence controls final pen style/color/width');
        Check(((LFill and not LStroke) and (ABitmap.Canvas.Brush.Style = bsSolid)) or
          ((not LFill or LStroke) and (ABitmap.Canvas.Brush.Style = bsClear)),
          'only a fill without a final stroke leaves a solid brush');
        Check((LFill and (ABitmap.Canvas.Brush.Color = clRed)) or
          (not LFill and (ABitmap.Canvas.Brush.Color = clYellow)),
          'absent fill retains the caller brush color');
        { Pixel reads must retire the empty DC clip first. Reinstall it for the
          repeated call, without ever expanding the clip during painting. }
        ABitmap.Canvas.Clipping := False;
        ComparePixels(ABitmap, AExpected);
        LExpectedCenter := clWhite;
        LExpectedEdge := clWhite;

        if not LClipped then
        begin

          if LFill then
          begin
            LExpectedCenter := clRed;
            LExpectedEdge := clRed;
          end;

          if LStroke then
          begin
            LExpectedEdge := clBlue;
          end;
        end;
        Check(ColorToRGB(ABitmap.Canvas.Pixels[16, 16]) = ColorToRGB(LExpectedCenter),
          'interior pixels follow only the admitted fill and actual clip');
        Check(ColorToRGB(ABitmap.Canvas.Pixels[6, 16]) = ColorToRGB(LExpectedEdge),
          'edge pixels preserve fill/outline ordering and actual clip');

        if LClipped then
        begin
          ABitmap.Canvas.Clipping := True;
          ABitmap.Canvas.ClipRect := Types.Rect(0, 0, 0, 0);
        end;
      end;
    end;
  end;
end;

procedure CheckTheme(ABitmap: TBitmap; ATheme: TNyxTheme);
var
  LPlan: INyxDrawingPlan;
  LStyle: TNyxDrawingStyle;
  LMapping: TNyxDrawingMapping;
  LFill: Boolean;
  LHasTheme: Boolean;
  LFailed: Boolean;
  LPalette: TNyxTheme;
begin
  LPlan := Plan(NyxDrawingStyle.Stroke(NyxThemePaint(ntcAccent), 4));
  LMapping := NyxDrawingMapping(LPlan, 32, 32);
  Prepare(ABitmap, False);
  ATheme.Accent := '#ff0000';
  PaintNyxLCLDrawing(ABitmap.Canvas, LPlan, LMapping, ATheme);
  Check(ABitmap.Canvas.Pen.Color = clRed, 'first call resolves the current theme');
  ATheme.Accent := '#0000ff';
  PaintNyxLCLDrawing(ABitmap.Canvas, LPlan, LMapping, ATheme);
  Check((ABitmap.Canvas.Pen.Color = clBlue) and
    (ColorToRGB(ABitmap.Canvas.Pixels[6, 16]) = ColorToRGB(clBlue)),
    'the same immutable plan and native canvas resolve a changed theme afresh');
  for LFill := False to True do
  begin
    LStyle := NyxDrawingStyle;

    if LFill then
    begin
      LStyle := LStyle.Fill(NyxThemePaint(ntcAccent));
    end
    else
    begin
      LStyle := LStyle.Stroke(NyxThemePaint(ntcAccent), 4);
    end;
    LPlan := Plan(LStyle);
    LMapping := NyxDrawingMapping(LPlan, 32, 32);
    for LHasTheme := False to True do
    begin
      Prepare(ABitmap, True);
      ATheme.Accent := 'Invalid palette token';
      LPalette := nil;

      if LHasTheme then
      begin
        LPalette := ATheme;
      end;
      LFailed := False;
      try
        PaintNyxLCLDrawing(ABitmap.Canvas, LPlan, LMapping, LPalette);
      except
        on E: ENyxDrawing do
        begin
          LFailed := not LHasTheme;
        end;
        on E: ENyxColorValue do
        begin
          LFailed := LHasTheme;
        end;
      end;
      Check(LFailed, 'fully clipped paint still refuses absent/malformed palettes');
      Check((ABitmap.Canvas.Pen.Width = 7) and (ABitmap.Canvas.Pen.Color = clGreen) and
        (ABitmap.Canvas.Brush.Color = clYellow),
        'failed color resolution preserves caller colors and the pending stroke width');
      Check((LFill and (ABitmap.Canvas.Brush.Style = bsSolid) and
        (ABitmap.Canvas.Pen.Style = psClear)) or
        (not LFill and (ABitmap.Canvas.Brush.Style = bsClear) and
        (ABitmap.Canvas.Pen.Style = psSolid)),
        'failed paint retains the existing public partial-state contract');
      CheckUnchangedPathState(ABitmap.Canvas);
    end;
  end;
end;

var
  LBitmap: TBitmap;
  LExpected: TBitmap;
  LTheme: TNyxTheme;
begin
  try
    Application.Initialize;
    LBitmap := TBitmap.Create;
    LExpected := TBitmap.Create;
    LTheme := TNyxTheme.Create;
    try
      CheckStyles(LBitmap, LExpected);
      CheckTheme(LBitmap, LTheme);
    finally
      LTheme.Free;
      LExpected.Free;
      LBitmap.Free;
    end;
    WriteLn('PASS ', GChecks, ' native drawing state checks; no raster saved');
  except
    on E: Exception do
    begin
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
    end;
  end;
end.
