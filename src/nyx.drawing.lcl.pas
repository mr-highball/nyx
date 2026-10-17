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
  Classes, SysUtils, Math,
  {$ifdef MSWINDOWS}Windows,{$endif}Types, Controls, Graphics, LCLIntf, LCLType,
  nyx.text, nyx.data,
  nyx.colors, nyx.theme, nyx.design.tokens, nyx.drawing, nyx.drawing.view,
  nyx.canvas.geometry, nyx.paint.buffer.lcl;

type
  { Owned native face for the portable display list. Configure admits a detached
    complete candidate before replacing the accepted face. Resize/provider errors
    retain that face and expose LastError, rather than escaping an LCL callback.
    The control copies the theme, retains managed provider/plan interfaces and
    never borrows a model, document or renderer. An optional native paint scope
    buffers only the visible update rectangle and revokes before handle teardown.
    UI-thread use only. }
  TNyxLCLDrawing = class(TCustomControl)
  private
    FBase: INyxDrawingPlan;
    FPlan: INyxDrawingPlan;
    FProvider: INyxDrawingProvider;
    FTheme: TNyxTheme;
    FMapping: TNyxDrawingMapping;
    FLastError: TNyxText;
    FPaintBuffer: INyxLCLPaintBuffer;
  protected
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure Paint; override;
    procedure Resize; override;
  public
    destructor Destroy; override;
    procedure Configure(const APlan: INyxDrawingPlan;
      const AProvider: INyxDrawingProvider; ATheme: TNyxTheme;
      const ACaption: TNyxText);
    property Plan: INyxDrawingPlan read FPlan;
    property LastError: TNyxText read FLastError;
    { Copy platform-buffer diagnostics; absent/unsupported handles return zeros. }
    function PaintBufferStats: TNyxLCLPaintBufferStats;
  end;

{ Borrow a canvas/theme only for this call. It draws an admitted display list
  using the supplied logical mapping and changes the canvas's paint state.
  No background is filled, allowing ordinary surfaces and transparent native
  decorations to reuse the same path/text renderer. Callers own lifetime/clipping. }
procedure PaintNyxLCLDrawing(ACanvas: TCanvas; const APlan: INyxDrawingPlan;
  const AMapping: TNyxDrawingMapping; ATheme: TNyxTheme);

implementation

{ Include native endpoint/rounding slack and any actual miter extension. Win32's
  GetMiterLimit borrows the caller DC for this read only. Unknown/failed limits
  (including other widgetsets' miter policies) disable rejection, never drawing.
  No caller pen/join policy or device context state is changed by this query. }
function StrokeClipMargin(ACanvas: TCanvas; AWidth: Integer; out AMargin: Double): Boolean;
{$ifdef MSWINDOWS}
var
  LLimit: Single;
{$endif}
begin
  AMargin := AWidth + 2;
  Result := True;

  if (AWidth > 0) and (ACanvas.Pen.JoinStyle = pjsMiter) then
  begin
    {$ifdef MSWINDOWS}
    LLimit := 0;
    Result := Windows.GetMiterLimit(ACanvas.Handle, LLimit) and not IsNan(LLimit) and
      not IsInfinite(LLimit) and (LLimit >= 1);

    if Result then
    begin
      AMargin := Double(AWidth) * LLimit + 2;
    end;
    {$else}
    Result := False;
    {$endif}
  end;
end;

{ A cubic stays inside its control hull. Compare its mapped axis-aligned hull,
  enlarged for the native stroke, with the actual logical DC clip before costly
  sampling. Degenerate/nonfinite/out-of-native-range input falls through to the
  existing painter rather than concealing its refusal or overflow behavior. }
function PathOutsideClip(const APath: TNyxDrawingPath; const AMapping: TNyxDrawingMapping;
  const AClip: TRect; AMargin: Double): Boolean;
var
  LStep: TNyxDrawingStep;
  LPoint: TNyxCanvasPoint;
  LIndex: Integer;
  LPointIndex: Integer;
  LPointCount: Integer;
  LHasPoint: Boolean;
  LLeft: Double;
  LTop: Double;
  LRight: Double;
  LBottom: Double;
begin
  Result := False;
  LHasPoint := False;
  LLeft := 0;
  LTop := 0;
  LRight := 0;
  LBottom := 0;
  for LIndex := 0 to APath.StepCount - 1 do
  begin
    LStep := APath.Step(LIndex);
    LPointCount := 0;
    { Ord retains the conservative fallback without an unreachable enum branch
      on compilers that already know today's complete instruction vocabulary. }
    case Ord(LStep.Kind) of
      Ord(ndskMove), Ord(ndskLine): LPointCount := 1;
      Ord(ndskCurve): LPointCount := 3;
      Ord(ndskClose): LPointCount := 0;
    else
      { A future instruction must remain visible until its hull is understood. }
      Exit;
    end;
    for LPointIndex := 0 to LPointCount - 1 do
    begin
      LPoint := AMapping.Point(LStep.Point(LPointIndex));

      if IsNan(LPoint.X) or IsInfinite(LPoint.X) or IsNan(LPoint.Y) or IsInfinite(LPoint.Y) or
        (LPoint.X <= Low(Integer)) or (LPoint.X >= High(Integer)) or
        (LPoint.Y <= Low(Integer)) or (LPoint.Y >= High(Integer)) then
      begin
        Exit;
      end;

      if not LHasPoint then
      begin
        LLeft := LPoint.X;
        LRight := LPoint.X;
        LTop := LPoint.Y;
        LBottom := LPoint.Y;
        LHasPoint := True;
      end
      else
      begin
        LLeft := Math.Min(LLeft, LPoint.X);
        LRight := Math.Max(LRight, LPoint.X);
        LTop := Math.Min(LTop, LPoint.Y);
        LBottom := Math.Max(LBottom, LPoint.Y);
      end;
    end;
  end;

  if LHasPoint then
  begin
    Result := (AClip.Right <= AClip.Left) or (AClip.Bottom <= AClip.Top) or
      (LRight + AMargin < AClip.Left) or (LLeft - AMargin > AClip.Right) or
      (LBottom + AMargin < AClip.Top) or (LTop - AMargin > AClip.Bottom);
  end;
end;

function FaceColor(const APaint: TNyxDrawingPaint; ATheme: TNyxTheme): TColor;
var
  LColor: TNyxRGBColor;
begin
  LColor := ResolveNyxDrawingPaint(APaint, ATheme);
  Result := RGBToColor(LColor.Red, LColor.Green, LColor.Blue);
end;

destructor TNyxLCLDrawing.Destroy;
begin

  if FPaintBuffer <> nil then
  begin
    FPaintBuffer.Retire;
    FPaintBuffer := nil;
  end;
  FProvider := nil;
  FPlan := nil;
  FBase := nil;
  FTheme.Free;
  inherited Destroy;
end;

procedure TNyxLCLDrawing.CreateWnd;
begin
  inherited CreateWnd;
  { Match the viewport/panel lifecycle: inherited nested creation may already
    have installed this control's field-owned scope. Retire only that lease
    before replacement; another owner's duplicate attachment still refuses. }

  if FPaintBuffer <> nil then
  begin
    FPaintBuffer.Retire;
    FPaintBuffer := nil;
  end;
  FPaintBuffer := AttachNyxLCLPaintBuffer(Self);
end;

procedure TNyxLCLDrawing.DestroyWnd;
begin

  if FPaintBuffer <> nil then
  begin
    FPaintBuffer.Retire;
    FPaintBuffer := nil;
  end;
  inherited DestroyWnd;
end;

function TNyxLCLDrawing.PaintBufferStats: TNyxLCLPaintBufferStats;
begin
  Result := Default(TNyxLCLPaintBufferStats);

  if FPaintBuffer <> nil then
  begin
    Result := FPaintBuffer.Stats;
  end;
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
  LCommand: TNyxDrawingCommand;
  LCommands: INyxDrawingCommands;
  LPath: TNyxDrawingPath;
  LStyle: TNyxDrawingStyle;
  LPoints: TNyxDrawingPoints;
  LNative: array of TPoint;
  LPosition: TNyxCanvasPoint;
  LText: TNyxText;
  LX: Integer;
  LY: Integer;
  LClip: TRect;
  LClipRead: Boolean;
  LCanClip: Boolean;
  LVisible: Boolean;
  LStrokeWidth: Integer;
  LMargin: Double;
begin

  if (APlan = nil) or (AMapping.Scale = 0) then
  begin
    Exit;
  end;
  LCommands := NyxDrawingCommands(APlan);
  LClipRead := False;
  LCanClip := False;
  for LIndex := 0 to LCommands.Count - 1 do
  begin
    LCommand := LCommands.TypedCommand(LIndex);

    if LCommand.Kind = ndckPath then
    begin
      LPath := LCommand.Path;
      LStyle := LCommand.Style;
      LStrokeWidth := 0;

      if LStyle.StrokePaint.Kind <> ndpkNone then
      begin
        { Metrics must still refuse even for a fully clipped command. }
        LStrokeWidth := Max(1, Round(AMapping.Metric(LStyle.StrokeWidth)));
      end;

      if not LClipRead then
      begin
        LClipRead := True;
        { Unlike TCanvas.ClipRect's fallback rectangle, a failed native query
          must disable optimization. The caller's DC/clip is never replaced. }
        LCanClip := LCLIntf.GetClipBox(ACanvas.Handle, @LClip) <> LCLType.ERROR;
      end;
      LVisible := True;

      if LCanClip and StrokeClipMargin(ACanvas, LStrokeWidth, LMargin) then
      begin
        LVisible := not PathOutsideClip(LPath, AMapping, LClip, LMargin);
      end;

      if LVisible then
      begin
        LPoints := NyxDrawingPathPoints(LPath);
        SetLength(LNative, Length(LPoints));
        for LPointIndex := 0 to High(LPoints) do
        begin
          LPosition := AMapping.Point(LPoints[LPointIndex]);
          LNative[LPointIndex] := Types.Point(Round(LPosition.X), Round(LPosition.Y));
        end;
      end;
      { Keep paint/color validation and final public canvas state identical even
        when no sample allocation or native drawing call is needed. }
      ACanvas.Brush.Style := bsClear;
      ACanvas.Pen.Style := psClear;

      if LStyle.FillPaint.Kind <> ndpkNone then
      begin
        ACanvas.Brush.Style := bsSolid;
        ACanvas.Brush.Color := FaceColor(LStyle.FillPaint, ATheme);

        if LVisible then
        begin
          ACanvas.Polygon(LNative);
        end;
      end;

      if LStyle.StrokePaint.Kind <> ndpkNone then
      begin
        ACanvas.Brush.Style := bsClear;
        ACanvas.Pen.Style := psSolid;
        ACanvas.Pen.Color := FaceColor(LStyle.StrokePaint, ATheme);
        ACanvas.Pen.Width := LStrokeWidth;

        if LVisible then
        begin
          ACanvas.Polyline(LNative);
        end;
      end;
    end
    else if LCommand.Paint.Kind <> ndpkNone then
    begin
      LPosition := AMapping.Point(LCommand.Position);
      LText := LCommand.Text;
      ACanvas.Brush.Style := bsClear;
      ACanvas.Font.Name := 'Segoe UI';
      ACanvas.Font.Height := -Max(1, Round(AMapping.Metric(LCommand.Size)));
      ACanvas.Font.Style := [];

      if LCommand.Bold then
      begin
        ACanvas.Font.Style := [fsBold];
      end;
      ACanvas.Font.Color := FaceColor(LCommand.Paint, ATheme);
      LX := Round(LPosition.X);
      LY := Round(LPosition.Y);
      case LCommand.Anchor of
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
