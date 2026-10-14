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
unit nyx.drawing.view;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, Math, nyx.text, nyx.data, nyx.model, nyx.types, nyx.theme,
  nyx.drawing, nyx.canvas.geometry;

type
  { Copied projection mapping. Drawing allocations share the canvas adapter's
    bounded logical-pixel range; no device handles or mutable renderer escape. }
  TNyxDrawingMapping = record
    Scale: Double;
    Left: Double;
    Top: Double;
    ViewBox: TNyxCanvasBounds;
    function Point(const APoint: TNyxCanvasPoint): TNyxCanvasPoint;
    function Metric(AValue: Double): Double;
  end;

function NyxNodeDrawing(ANode: TNyxNode): INyxDrawingPlan;
function NyxNodeDrawingProvider(ANode: TNyxNode): TNyxDrawingProviderRef;
procedure ValidateNyxDrawingNode(ANode: TNyxNode);
function NyxDrawingMapping(const APlan: INyxDrawingPlan;
  AWidth, AHeight: Double): TNyxDrawingMapping;
{ Pure provider admission returns a detached normalized plan. No target is
  changed until the complete plan/mapping/metric boundary has passed. }
function ResolveNyxDrawing(const APlan: INyxDrawingPlan;
  const AProvider: INyxDrawingProvider; AWidth, AHeight: Double;
  ATheme: TNyxTheme): INyxDrawingPlan;

implementation

function NyxNodeDrawing(ANode: TNyxNode): INyxDrawingPlan;
begin

  if ANode = nil then
  begin
    raise ENyxDrawing.Create('Drawing descriptor required');
  end;

  if ANode.Extensions.Has(NyxExtension(NyxDrawingKey)) then
  begin
    Result := NyxDrawingPlanFromData(ANode.Extensions.Value(NyxExtension(NyxDrawingKey)));
  end
  else
  begin
    Result := NewNyxDrawingPlan(NyxCanvasBounds(0, 0, 100, 100)).Build;
  end;
end;

function NyxNodeDrawingProvider(ANode: TNyxNode): TNyxDrawingProviderRef;
begin
  Result := Default(TNyxDrawingProviderRef);

  if ANode.Extensions.Has(NyxExtension(NyxDrawingProviderKey)) then
  begin
    Result := NyxDrawingProvider(ANode.Extensions.Value(NyxExtension(NyxDrawingProviderKey)).AsText);
  end;
end;

procedure ValidateNyxDrawingNode(ANode: TNyxNode);
begin

  if ANode.Extensions.Has(NyxExtension(NyxDrawingKey)) then
  begin
    NyxNodeDrawing(ANode);
  end;

  if ANode.Extensions.Has(NyxExtension(NyxDrawingProviderKey)) then
  begin
    NyxNodeDrawingProvider(ANode);
  end;
end;

function TNyxDrawingMapping.Point(const APoint: TNyxCanvasPoint): TNyxCanvasPoint;
begin
  Result := NyxCanvasPoint(Left + (APoint.X - ViewBox.Left) * Scale,
    Top + (APoint.Y - ViewBox.Top) * Scale);
end;

function TNyxDrawingMapping.Metric(AValue: Double): Double;
begin
  Result := AValue * Scale;

  if IsNan(Result) or IsInfinite(Result) or (Result < 0) or (Result > 4096) then
  begin
    raise ENyxDrawing.Create('Drawing metric exceeds the shared 4096 logical-pixel boundary');
  end;
end;

function NyxDrawingMapping(const APlan: INyxDrawingPlan;
  AWidth, AHeight: Double): TNyxDrawingMapping;
var
  LIndex: Integer;
  LCommand: TNyxDataValue;
  LStyle: TNyxDrawingStyle;
begin

  if (APlan = nil) or IsNan(AWidth) or IsNan(AHeight) or IsInfinite(AWidth) or
    IsInfinite(AHeight) or (AWidth < 0) or (AHeight < 0) or
    (AWidth > 32768) or (AHeight > 32768) then
  begin
    raise ENyxDrawing.Create('Drawing allocation requires finite 0..32768 logical pixels');
  end;
  Result.ViewBox := APlan.ViewBox;

  if not Result.ViewBox.Defined or (Result.ViewBox.Width <= 0) or
    (Result.ViewBox.Height <= 0) or (APlan.Count < 0) or
    (APlan.Count > MaximumNyxDrawingCommands) then
  begin
    raise ENyxDrawing.Create('Drawing mapping requires an admitted immutable plan');
  end;
  Result.Scale := Min(AWidth / Result.ViewBox.Width, AHeight / Result.ViewBox.Height);
  Result.Left := (AWidth - Result.ViewBox.Width * Result.Scale) / 2;
  Result.Top := (AHeight - Result.ViewBox.Height * Result.Scale) / 2;
  for LIndex := 0 to APlan.Count - 1 do
  begin
    LCommand := APlan.Command(LIndex);

    if LCommand.Field('kind').AsInteger = 0 then
    begin
      LStyle := TNyxDrawingStyle.FromData(LCommand.Field('style'));
      Result.Metric(LStyle.StrokeWidth);
    end
    else
    begin
      Result.Metric(LCommand.Field('size').AsNumber);
    end;
  end;
end;

function ResolveNyxDrawing(const APlan: INyxDrawingPlan;
  const AProvider: INyxDrawingProvider; AWidth, AHeight: Double;
  ATheme: TNyxTheme): INyxDrawingPlan;
var
  LFrame: TNyxDrawingFrame;
begin
  { Reject hostile allocation before calling even a registered provider. }
  NyxDrawingMapping(APlan, AWidth, AHeight);
  Result := APlan;

  if AProvider <> nil then
  begin
    LFrame.Width := AWidth;
    LFrame.Height := AHeight;
    LFrame.Palette := NyxDrawingPalette(ATheme);
    Result := AProvider.Build(LFrame);

    if Result = nil then
    begin
      raise ENyxDrawing.Create('Drawing provider must return a detached immutable plan');
    end;
  end;
  Result := NyxDrawingPlanFromData(Result.ToData);
  NyxDrawingMapping(Result, AWidth, AHeight);
end;

end.
