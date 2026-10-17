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
  { One owner-confined descriptor reader. Read borrows its node only for that
    call and retains an admitted immutable fallback plus exact source bytes,
    never a node, provider, theme, renderer or application. Unchanged source and
    encoding policy reuse the fallback; changed or absent extensions re-admit.
    Failure preserves the previous cache. This prepares geometry, not a physical
    face or provider result; those retain their ordinary validation/publication. }
  INyxDrawingSource = interface
    ['{25B85FC9-D460-482C-B5C0-828A87D8FEA9}']
    { Nil or invalid descriptors refuse before retained state changes. Callers
      synchronize changes to fpjson's global formatter during each read. }
    function Read(ANode: TNyxNode): INyxDrawingPlan;
  end;

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
{ Create a private reference-counted reader for one binding/consumer lifetime.
  Releasing it retires its source/plan; independently retained plans stay valid. }
function NewNyxDrawingSource: INyxDrawingSource;
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

uses fpjson;

type
  { Private per-consumer source/plan pair. Text and plans are immutable owned
    values; source identity includes extension presence and slash-escape policy.
    A failed decode cannot install a new key or retain a borrowed node. }
  TDrawingSource = class(TInterfacedObject, INyxDrawingSource)
  private
    FPlan: INyxDrawingPlan;
    FSource: TNyxText;
    FHasDrawing: Boolean;
    FStrictEscaping: Boolean;
  public
    function Read(ANode: TNyxNode): INyxDrawingPlan;
  end;

function NewNyxDrawingSource: INyxDrawingSource;
begin
  Result := TDrawingSource.Create;
end;

function TDrawingSource.Read(ANode: TNyxNode): INyxDrawingPlan;
var
  LSource: TNyxText;
  LHasDrawing: Boolean;
  LStrictEscaping: Boolean;
  LPlan: INyxDrawingPlan;
begin

  if ANode = nil then
  begin
    raise ENyxDrawing.Create('Drawing descriptor required');
  end;
  LHasDrawing := ANode.Extensions.Has(NyxExtension(NyxDrawingKey));
  LStrictEscaping := TJSONString.StrictEscaping;
  LSource := '';

  if LHasDrawing then
  begin
    LSource := ANode.Extensions.Value(NyxExtension(NyxDrawingKey)).ToJSON;
  end;

  if (FPlan <> nil) and (FHasDrawing = LHasDrawing) and
    (FStrictEscaping = LStrictEscaping) and (FSource = LSource) then
  begin
    Exit(FPlan);
  end;
  { The existing strict boundary remains the only decoder. Preparing a new base
    does not accept a provider or alter the target; failed target admission will
    still call its provider again on the next synchronization. }
  LPlan := NyxNodeDrawing(ANode);
  FSource := LSource;
  FHasDrawing := LHasDrawing;
  FStrictEscaping := LStrictEscaping;
  FPlan := LPlan;
  Result := LPlan;
end;

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

{ Allocation preflight invokes no creator getter/provider or snapshot callback. }
procedure RequireDrawingAllocation(const APlan: INyxDrawingPlan; AWidth, AHeight: Double);
begin

  if (APlan = nil) or IsNan(AWidth) or IsNan(AHeight) or IsInfinite(AWidth) or
    IsInfinite(AHeight) or (AWidth < 0) or (AHeight < 0) or
    (AWidth > 32768) or (AHeight > 32768) then
  begin
    raise ENyxDrawing.Create('Drawing allocation requires finite 0..32768 logical pixels');
  end;
end;

function NyxDrawingMapping(const APlan: INyxDrawingPlan;
  AWidth, AHeight: Double): TNyxDrawingMapping;
var
  LIndex: Integer;
  LCommand: TNyxDrawingCommand;
  LStyle: TNyxDrawingStyle;
  LPlan: INyxDrawingPlan;
  LCommands: INyxDrawingCommands;
begin
  RequireDrawingAllocation(APlan, AWidth, AHeight);
  LPlan := CaptureNyxDrawingPlan(APlan);
  LCommands := NyxDrawingCommands(LPlan);
  Result.ViewBox := LPlan.ViewBox;

  if not Result.ViewBox.Defined or (Result.ViewBox.Width <= 0) or
    (Result.ViewBox.Height <= 0) or (LPlan.Count < 0) or
    (LPlan.Count > MaximumNyxDrawingCommands) then
  begin
    raise ENyxDrawing.Create('Drawing mapping requires an admitted immutable plan');
  end;
  Result.Scale := Min(AWidth / Result.ViewBox.Width, AHeight / Result.ViewBox.Height);
  Result.Left := (AWidth - Result.ViewBox.Width * Result.Scale) / 2;
  Result.Top := (AHeight - Result.ViewBox.Height * Result.Scale) / 2;
  for LIndex := 0 to LCommands.Count - 1 do
  begin
    LCommand := LCommands.TypedCommand(LIndex);

    if LCommand.Kind = ndckPath then
    begin
      LStyle := LCommand.Style;
      Result.Metric(LStyle.StrokeWidth);
    end
    else
    begin
      Result.Metric(LCommand.Size);
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
  RequireDrawingAllocation(APlan, AWidth, AHeight);
  Result := CaptureNyxDrawingPlan(APlan);
  NyxDrawingMapping(Result, AWidth, AHeight);

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
  Result := CaptureNyxDrawingPlan(Result);
  NyxDrawingMapping(Result, AWidth, AHeight);
end;

end.
