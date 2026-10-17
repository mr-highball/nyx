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
program nyx_drawing_clip_tests;

{$mode delphi}{$H+}{$codepage utf8}

{ Programmatic painter checks, without a shown host or image capture. A complete
  reference bitmap includes every path's control hull. Smaller direct/tiled
  paints must equal its translated crop at every pixel, including outside each
  clip. No raster, screenshot, browser fixture or visual-quality claim is saved. }

uses
  Interfaces, Forms, SysUtils, Math,
  {$ifdef MSWINDOWS}Windows,{$endif}Types, Graphics,
  nyx.text, nyx.data, nyx.theme, nyx.design.tokens, nyx.drawing,
  nyx.drawing.view, nyx.drawing.lcl, nyx.canvas.geometry;

const
  CropLeft = 256;
  CropTop = 192;
  CropWidth = 256;
  CropHeight = 192;

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AMessage: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Drawing clip: ' + AMessage);
  end;
  Inc(GChecks);
end;

function Fixture: INyxDrawingPlan;
begin
  Result := NewNyxDrawingPlan(NyxCanvasBounds(0, 0, 1024, 768))
    { An enclosing fill has no vertex inside some small clips. }
    .Path(NyxDrawingPath(NyxCanvasPoint(100, 60)).LineTo(NyxCanvasPoint(900, 60))
      .LineTo(NyxCanvasPoint(520, 700)).Close, NyxDrawingStyle.Fill(NyxThemePaint(ntcSurface)))
    .Path(NyxDrawingRectangle(NyxCanvasBounds(240, 180, 48, 48)),
      NyxDrawingStyle.Fill(NyxThemePaint(ntcAccent)).Stroke(NyxThemePaint(ntcText), 3))
    { Both endpoints are outside the crop; the curve enters it. }
    .Path(NyxDrawingPath(NyxCanvasPoint(240, 210))
      .CurveTo(NyxCanvasPoint(430, 210), NyxCanvasPoint(430, 350), NyxCanvasPoint(240, 350)),
      NyxDrawingStyle.Stroke(NyxThemePaint(ntcAccent), 5))
    { Stroke reaches the crop although the whole centerline lies to its left. }
    .Path(NyxDrawingPath(NyxCanvasPoint(248, 180)).LineTo(NyxCanvasPoint(248, 400)),
      NyxDrawingStyle.Stroke(NyxThemePaint(ntcText), 30))
    .Path(NyxDrawingPath(NyxCanvasPoint(248, 210)).LineTo(NyxCanvasPoint(240, 256))
      .LineTo(NyxCanvasPoint(250, 208)), NyxDrawingStyle.Stroke(NyxThemePaint(ntcAccent), 12))
    .Path(NyxDrawingRectangle(NyxCanvasBounds(420, 300, 40, 60)),
      NyxDrawingStyle.Fill(NyxThemePaint(ntcAccent)))
    .Text('Clip / café 𝄞', NyxCanvasPoint(250, 240), NyxThemePaint(ntcText), 18)
    .Text('Centered text', NyxCanvasPoint(380, 280), NyxThemePaint(ntcAccent), 14, ndtaCenter, True)
    .Text('Right edge', NyxCanvasPoint(530, 320), NyxThemePaint(ntcText), 16, ndtaEnd)
    { A distant last path also checks the observable final pen/brush state. }
    .Path(NyxDrawingPath(NyxCanvasPoint(960, 700)).LineTo(NyxCanvasPoint(980, 710)),
      NyxDrawingStyle.Fill(NyxThemePaint(ntcSurface)).Stroke(NyxThemePaint(ntcAccent), 7))
    .Build;
end;

procedure Prepare(ABitmap: TBitmap; AWidth, AHeight: Integer; AJoin: TPenJoinStyle);
{$ifdef MSWINDOWS}
var
  LLimit: Single;
{$endif}
begin
  ABitmap.PixelFormat := pf24bit;
  ABitmap.SetSize(AWidth, AHeight);
  ABitmap.Canvas.Clipping := False;
  ABitmap.Canvas.Brush.Style := bsSolid;
  ABitmap.Canvas.Brush.Color := clWhite;
  ABitmap.Canvas.FillRect(Types.Rect(0, 0, AWidth, AHeight));
  ABitmap.Canvas.Pen.JoinStyle := AJoin;
  {$ifdef MSWINDOWS}
  LLimit := 25;
  Check(Windows.SetMiterLimit(ABitmap.Canvas.Handle, LLimit, nil),
    'owned bitmap accepts its explicit nondefault miter limit');
  {$endif}
end;

function CanvasState(ACanvas: TCanvas): TNyxText;
begin
  Result := NyxArray([NyxData(Ord(ACanvas.Brush.Style)),
    NyxData(Double(ColorToRGB(ACanvas.Brush.Color))), NyxData(Ord(ACanvas.Pen.Style)),
    NyxData(Double(ColorToRGB(ACanvas.Pen.Color))), NyxData(ACanvas.Pen.Width),
    NyxData(TNyxText(ACanvas.Font.Name)), NyxData(ACanvas.Font.Height),
    NyxData(fsBold in ACanvas.Font.Style), NyxData(Double(ColorToRGB(ACanvas.Font.Color)))]).ToJSON;
end;

procedure ComparePixels(AExpected, AActual: TBitmap; const ACase: TNyxText);
var
  LX: Integer;
  LY: Integer;
begin
  for LY := 0 to CropHeight - 1 do
  begin
    for LX := 0 to CropWidth - 1 do
    begin

      if ColorToRGB(AExpected.Canvas.Pixels[LX, LY]) <>
        ColorToRGB(AActual.Canvas.Pixels[LX, LY]) then
      begin
        raise Exception.Create('Drawing clip pixels differ: ' + ACase + ' at ' +
          IntToStr(LX) + ',' + IntToStr(LY));
      end;
    end;
  end;
  Check(True, 'complete in-memory crop equals direct/tiled paint');
end;

procedure Run;
const
  Zooms: array[0..2] of Double = (0.75, 1, 1.25);
var
  LReference: TBitmap;
  LExpected: TBitmap;
  LActual: TBitmap;
  LTheme: TNyxTheme;
  LPlan: INyxDrawingPlan;
  LMapping: TNyxDrawingMapping;
  LTarget: TNyxDrawingMapping;
  LState: TNyxText;
  LClip: TRect;
  LCase: Integer;
  LZoom: Integer;
  LJoin: TPenJoinStyle;
  LX: Integer;
  LY: Integer;
  LFailed: Boolean;
begin
  LReference := TBitmap.Create;
  LExpected := TBitmap.Create;
  LActual := TBitmap.Create;
  LTheme := TNyxTheme.Create;
  try
    LPlan := Fixture;
    for LZoom := Low(Zooms) to High(Zooms) do
    begin
      for LJoin := Low(TPenJoinStyle) to High(TPenJoinStyle) do
      begin
        Prepare(LReference, Round(1024 * Zooms[LZoom]), Round(768 * Zooms[LZoom]), LJoin);
        LMapping := NyxDrawingMapping(LPlan, LReference.Width, LReference.Height);
        PaintNyxLCLDrawing(LReference.Canvas, LPlan, LMapping, LTheme);
        LState := CanvasState(LReference.Canvas);
        LTarget := LMapping;
        LTarget.Left := LTarget.Left - CropLeft;
        LTarget.Top := LTarget.Top - CropTop;
        for LCase := 0 to 5 do
        begin
          Prepare(LExpected, CropWidth, CropHeight, LJoin);
          Prepare(LActual, CropWidth, CropHeight, LJoin);
          case LCase of
            1: LClip := Types.Rect(60, 40, 140, 90);
            2: LClip := Types.Rect(17, 0, 18, CropHeight);
            3: LClip := Types.Rect(0, 97, CropWidth, 98);
            4: LClip := Types.Rect(8, 8, 8, 8);
          else
            LClip := Types.Rect(0, 0, CropWidth, CropHeight);
          end;

          if (LClip.Right > LClip.Left) and (LClip.Bottom > LClip.Top) then
          begin
            LExpected.Canvas.CopyRect(LClip, LReference.Canvas,
              Types.Rect(LClip.Left + CropLeft, LClip.Top + CropTop,
                LClip.Right + CropLeft, LClip.Bottom + CropTop));
          end;
          LActual.Canvas.Clipping := True;

          if LCase = 5 then
          begin
            for LY := 0 to 2 do
            begin
              for LX := 0 to 3 do
              begin
                LActual.Canvas.ClipRect := Types.Rect(LX * 64, LY * 64, (LX + 1) * 64, (LY + 1) * 64);
                PaintNyxLCLDrawing(LActual.Canvas, LPlan, LTarget, LTheme);
              end;
            end;
          end
          else
          begin
            LActual.Canvas.ClipRect := LClip;
            PaintNyxLCLDrawing(LActual.Canvas, LPlan, LTarget, LTheme);
          end;
          Check(CanvasState(LActual.Canvas) = LState, 'clip retains final public canvas paint state');
          { Win32 GetPixel refuses coordinates outside the current DC clip.
            Retire that output clip before reading the complete target bitmap. }
          LActual.Canvas.Clipping := False;
          ComparePixels(LExpected, LActual, TNyxText(IntToStr(LZoom) + '/' +
            IntToStr(Ord(LJoin)) + '/' + IntToStr(LCase)));
        end;
      end;
    end;
    LTarget.Scale := 1000000;
    LTarget.Left := -1000000000;
    LActual.Canvas.ClipRect := Types.Rect(0, 0, 1, 1);
    LFailed := False;
    try
      PaintNyxLCLDrawing(LActual.Canvas, LPlan, LTarget, LTheme);
    except
      on E: Exception do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed, 'offscreen work cannot bypass invalid mapping/metric refusal');
  finally
    LTheme.Free;
    LActual.Free;
    LExpected.Free;
    LReference.Free;
  end;
end;

begin
  try
    Application.Initialize;
    Run;
    WriteLn('PASS ', GChecks, ' programmatic native painter checks; no raster saved');
  except
    on E: Exception do
    begin
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
    end;
  end;
end.
