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
unit nyx.drawing.lcl;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  Classes, SysUtils, Math, Types, Controls, Graphics, nyx.text, nyx.data,
  nyx.colors, nyx.theme, nyx.design.tokens, nyx.drawing, nyx.drawing.view,
  nyx.canvas.geometry;

type
  { Owned native face for the portable display list. Configure admits a detached
    complete candidate before replacing the accepted face. Resize/provider errors
    retain that face and expose LastError, rather than escaping an LCL callback.
    The control copies the theme, retains managed provider/plan interfaces and
    never borrows a model, document or renderer. UI-thread use only. }
  TNyxLCLDrawing = class(TCustomControl)
  private
    FBase: INyxDrawingPlan;
    FPlan: INyxDrawingPlan;
    FProvider: INyxDrawingProvider;
    FTheme: TNyxTheme;
    FMapping: TNyxDrawingMapping;
    FLastError: TNyxText;
  protected
    procedure Paint; override;
    procedure Resize; override;
  public
    destructor Destroy; override;
    procedure Configure(const APlan: INyxDrawingPlan;
      const AProvider: INyxDrawingProvider; ATheme: TNyxTheme;
      const ACaption: TNyxText);
    property Plan: INyxDrawingPlan read FPlan;
    property LastError: TNyxText read FLastError;
  end;

{ Borrow a canvas/theme only for this call. It draws an admitted display list
  using the supplied logical mapping and changes the canvas's paint state.
  No background is filled, allowing ordinary surfaces and transparent native
  decorations to reuse the same path/text renderer. Callers own lifetime/clipping. }
procedure PaintNyxLCLDrawing(ACanvas: TCanvas; const APlan: INyxDrawingPlan;
  const AMapping: TNyxDrawingMapping; ATheme: TNyxTheme);

implementation

function FaceColor(const APaint: TNyxDrawingPaint; ATheme: TNyxTheme): TColor;
var
  LColor: TNyxRGBColor;
begin
  LColor := ResolveNyxDrawingPaint(APaint, ATheme);
  Result := RGBToColor(LColor.Red, LColor.Green, LColor.Blue);
end;

destructor TNyxLCLDrawing.Destroy;
begin
  FProvider := nil;
  FPlan := nil;
  FBase := nil;
  FTheme.Free;
  inherited Destroy;
end;

procedure TNyxLCLDrawing.Configure(const APlan: INyxDrawingPlan;
  const AProvider: INyxDrawingProvider; ATheme: TNyxTheme;
  const ACaption: TNyxText);
var
  LTheme: TNyxTheme;
  LBase: INyxDrawingPlan;
  LPlan: INyxDrawingPlan;
  LMapping: TNyxDrawingMapping;
begin

  if APlan = nil then
  begin
    raise ENyxDrawing.Create('Drawing plan required');
  end;
  LBase := CaptureNyxDrawingPlan(APlan);
  LTheme := NewNyxDocumentTheme(nil, ATheme);
  try
    LPlan := ResolveNyxDrawing(LBase, AProvider, ClientWidth, ClientHeight, LTheme);
    LMapping := NyxDrawingMapping(LPlan, ClientWidth, ClientHeight);
    FTheme.Free;
    FTheme := LTheme;
    LTheme := nil;
    FBase := LBase;
    FPlan := LPlan;
    FProvider := AProvider;
    FMapping := LMapping;
    FLastError := '';
    AccessibleName := ACaption;
    AccessibleRole := larImage;
    Invalidate;
  finally
    LTheme.Free;
  end;
end;

procedure TNyxLCLDrawing.Resize;
var
  LPlan: INyxDrawingPlan;
  LMapping: TNyxDrawingMapping;
begin
  inherited Resize;

  if FBase = nil then
  begin
    Exit;
  end;
  try
    LPlan := ResolveNyxDrawing(FBase, FProvider, ClientWidth, ClientHeight, FTheme);
    LMapping := NyxDrawingMapping(LPlan, ClientWidth, ClientHeight);
    FPlan := LPlan;
    FMapping := LMapping;
    FLastError := '';
    Invalidate;
  except
    on E: Exception do
    begin
      FLastError := E.Message;
    end;
  end;
end;

procedure PaintNyxLCLDrawing(ACanvas: TCanvas; const APlan: INyxDrawingPlan;
  const AMapping: TNyxDrawingMapping; ATheme: TNyxTheme);
var
  LIndex: Integer;
  LPointIndex: Integer;
  LCommand: TNyxDataValue;
  LPath: TNyxDrawingPath;
  LStyle: TNyxDrawingStyle;
  LPoints: TNyxDrawingPoints;
  LNative: array of TPoint;
  LPosition: TNyxCanvasPoint;
  LText: TNyxText;
  LX: Integer;
  LY: Integer;
begin
  if (APlan = nil) or (AMapping.Scale = 0) then
  begin
    Exit;
  end;
  for LIndex := 0 to APlan.Count - 1 do
  begin
    LCommand := APlan.Command(LIndex);

    if LCommand.Field('kind').AsInteger = 0 then
    begin
      LPath := TNyxDrawingPath.FromData(LCommand.Field('path'));
      LStyle := TNyxDrawingStyle.FromData(LCommand.Field('style'));
      LPoints := NyxDrawingPathPoints(LPath);
      SetLength(LNative, Length(LPoints));
      for LPointIndex := 0 to High(LPoints) do
      begin
        LPosition := AMapping.Point(LPoints[LPointIndex]);
        LNative[LPointIndex] := Types.Point(Round(LPosition.X), Round(LPosition.Y));
      end;
      ACanvas.Brush.Style := bsClear;
      ACanvas.Pen.Style := psClear;

      if LStyle.FillPaint.Kind <> ndpkNone then
      begin
        ACanvas.Brush.Style := bsSolid;
        ACanvas.Brush.Color := FaceColor(LStyle.FillPaint, ATheme);
        ACanvas.Polygon(LNative);
      end;

      if LStyle.StrokePaint.Kind <> ndpkNone then
      begin
        ACanvas.Brush.Style := bsClear;
        ACanvas.Pen.Style := psSolid;
        ACanvas.Pen.Color := FaceColor(LStyle.StrokePaint, ATheme);
        ACanvas.Pen.Width := Max(1, Round(AMapping.Metric(LStyle.StrokeWidth)));
        ACanvas.Polyline(LNative);
      end;
    end
    else if TNyxDrawingPaint.FromData(LCommand.Field('paint')).Kind <> ndpkNone then
    begin
      LPosition := AMapping.Point(NyxCanvasPoint(
        LCommand.Field('position').Item(0).AsNumber,
        LCommand.Field('position').Item(1).AsNumber));
      LText := LCommand.Field('text').AsText;
      ACanvas.Brush.Style := bsClear;
      ACanvas.Font.Name := 'Segoe UI';
      ACanvas.Font.Height := -Max(1, Round(AMapping.Metric(LCommand.Field('size').AsNumber)));
      ACanvas.Font.Style := [];

      if LCommand.Field('bold').AsBoolean then
      begin
        ACanvas.Font.Style := [fsBold];
      end;
      ACanvas.Font.Color := FaceColor(TNyxDrawingPaint.FromData(LCommand.Field('paint')), ATheme);
      LX := Round(LPosition.X);
      LY := Round(LPosition.Y);
      case TNyxDrawingTextAnchor(LCommand.Field('anchor').AsInteger) of
        ndtaStart:
          begin
            { Position already addresses the first glyph. }
          end;
        ndtaCenter:
          begin
            Dec(LX, ACanvas.TextWidth(LText) div 2);
          end;
        ndtaEnd:
          begin
            Dec(LX, ACanvas.TextWidth(LText));
          end;
      end;
      { Both adapters treat the text position as the top, not a font baseline.
        Native glyph metrics/rounding remain widgetset-specific. }
      ACanvas.TextOut(LX, LY, LText);
    end;
  end;
end;

procedure TNyxLCLDrawing.Paint;
begin
  inherited Paint;
  PaintNyxLCLDrawing(Canvas, FPlan, FMapping, FTheme);
end;

end.
