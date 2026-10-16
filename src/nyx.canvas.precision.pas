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
unit nyx.canvas.precision;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, Math, nyx.text, nyx.data, nyx.canvas.geometry, nyx.canvas.types;

const
  MaximumNyxCanvasGuides = 256;
  MaximumNyxCanvasRulerTicks = 512;
  MaximumNyxCanvasPrecisionBytes = 131072;

type
  { Precision contract failures occur before replacing any immutable proposal.
    They supply no host, file, document or execution authority. }
  ENyxCanvasPrecision = class(EArgumentException);

  { A measured world coordinate. X uses the top ruler and vertical guide lines;
    Y uses the left ruler and horizontal guide lines. This is not line direction. }
  TNyxCanvasAxis = (ncaxX, ncaxY);

  { Closed ruler visibility; either axis or both may be displayed independently. }
  TNyxCanvasAxes = set of TNyxCanvasAxis;

  { Exact open identity for one alignment reference, distinct from scene items,
    layers and control IDs. Names obey the shared 1..128-scalar canvas contract.
    The default record is absent and retains no object or authority. }
  TNyxCanvasGuideRef = record
  private
    FName: TNyxText;
    function GetDefined: Boolean;
  public
    property Name: TNyxText read FName;
    property Defined: Boolean read GetDefined;
  end;

  { Immutable world-coordinate reference. Locking prevents interactive movement
    or deletion, while visibility and snapping are independently configured.
    Fluent changes copy this value and never mutate a published options branch. }
  TNyxCanvasGuide = record
  private
    FReference: TNyxCanvasGuideRef;
    FAxis: TNyxCanvasAxis;
    FPosition: Double;
    FVisible: Boolean;
    FLocked: Boolean;
    procedure RequireDefined;
  public
    function AtPosition(AWorldPosition: Double): TNyxCanvasGuide;
    function Visible(AValue: Boolean): TNyxCanvasGuide;
    function Locked(AValue: Boolean): TNyxCanvasGuide;
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxCanvasGuide; static;
    property Reference: TNyxCanvasGuideRef read FReference;
    property Axis: TNyxCanvasAxis read FAxis;
    property Position: Double read FPosition;
    property IsVisible: Boolean read FVisible;
    property IsLocked: Boolean read FLocked;
  end;

  { Immutable ruler dimensions and units. Origin and world-units-per-label-unit
    define displayed values; minimum increment limits label precision. Adaptive
    major intervals are 1/2/5 decimal multiples of that increment, never denser
    than the logical-pixel spacing or the bounded tick budget. No theme/widget
    is retained. An absent ruler configuration displays no strips. }
  TNyxCanvasRulers = record
  private
    FDefined: Boolean;
    FAxes: TNyxCanvasAxes;
    FOrigin: TNyxCanvasPoint;
    FUnitSize: Double;
    FUnitCaption: TNyxText;
    FIncrement: Double;
    FMinimumPixels: Double;
    FThickness: Double;
    FDivisions: Integer;
    procedure RequireDefined;
  public
    function Axes(const AValue: TNyxCanvasAxes): TNyxCanvasRulers;
    function FromOrigin(const AValue: TNyxCanvasPoint): TNyxCanvasRulers;
    function Units(AWorldSize: Double; const ACaption: TNyxText = ''): TNyxCanvasRulers;
    function Interval(AMinimumIncrement: Double; AMinimumPixels: Double = 72;
      AMinorDivisions: Integer = 5): TNyxCanvasRulers;
    function Thickness(ALogicalPixels: Double): TNyxCanvasRulers;
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxCanvasRulers; static;
    property Defined: Boolean read FDefined;
    property VisibleAxes: TNyxCanvasAxes read FAxes;
    property Origin: TNyxCanvasPoint read FOrigin;
    property UnitSize: Double read FUnitSize;
    property UnitCaption: TNyxText read FUnitCaption;
    property Increment: Double read FIncrement;
    property MinimumPixels: Double read FMinimumPixels;
    property StripSize: Double read FThickness;
    property MinorDivisions: Integer read FDivisions;
  end;

  { Snapping policy is independent of ruler visibility. Locked guides remain
    valid references; All deliberately includes hidden guides. }
  TNyxCanvasGuideSnapping = (ncgspOff, ncgspVisible, ncgspAll);

  { Ordered immutable precision intent. Each guide update copies before writing;
    read access returns values, not caller-owned arrays. Missing removals are
    idempotent; duplicate names update in place and preserve order. Absent options
    preserve legacy documents and views. No document/view/receiver is retained. }
  TNyxCanvasPrecisionOptions = record
  private
    FDefined: Boolean;
    FRulers: TNyxCanvasRulers;
    FGuides: array of TNyxCanvasGuide;
    FSnapping: TNyxCanvasGuideSnapping;
    FTolerance: Double;
    function IndexOf(const AGuide: TNyxCanvasGuideRef): Integer;
    function GetCount: Integer;
    procedure RequireDefined;
  public
    function WithRulers(const AValue: TNyxCanvasRulers): TNyxCanvasPrecisionOptions;
    function WithGuide(const AValue: TNyxCanvasGuide): TNyxCanvasPrecisionOptions;
    function WithoutGuide(const AReference: TNyxCanvasGuideRef): TNyxCanvasPrecisionOptions;
    function Snapping(AMode: TNyxCanvasGuideSnapping;
      ATolerancePixels: Double = 8): TNyxCanvasPrecisionOptions;
    function Guide(AIndex: Integer): TNyxCanvasGuide;
    function Contains(const AReference: TNyxCanvasGuideRef): Boolean;
    function Named(const AReference: TNyxCanvasGuideRef): TNyxCanvasGuide;
    function NextGuide: TNyxCanvasGuideRef;
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxCanvasPrecisionOptions; static;
    property Defined: Boolean read FDefined;
    property Rulers: TNyxCanvasRulers read FRulers;
    property Count: Integer read GetCount;
    property SnapMode: TNyxCanvasGuideSnapping read FSnapping;
    property TolerancePixels: Double read FTolerance;
  end;

  { Detached projected tick. Pixel is local to its measured viewport axis;
    Value is in caller units. Only major ticks carry a label. Arrays remain
    independent values and contain no renderer, mutable options or callbacks. }
  TNyxCanvasRulerTick = record
    Axis: TNyxCanvasAxis;
    Pixel: Double;
    Value: Double;
    Major: Boolean;
    Caption: TNyxText;
  end;

  { An independently owned bounded tick window, shared by both target adapters. }
  TNyxCanvasRulerTicks = array of TNyxCanvasRulerTick;

  { Snapping result distinguishes an untouched axis from a guide hit. At most
    one closest guide wins per axis; insertion order breaks exact ties. }
  TNyxCanvasGuideSnap = record
    Point: TNyxCanvasPoint;
    XGuide: TNyxCanvasGuideRef;
    YGuide: TNyxCanvasGuideRef;
  end;

{ Factories validate closed choices, finite coordinates and meaningful extents.
  Failure leaves all caller values intact; no locale/global codepage is changed. }
function NyxCanvasGuideRef(const AName: TNyxText): TNyxCanvasGuideRef;
function NyxCanvasGuide(const AReference: TNyxCanvasGuideRef;
  AAxis: TNyxCanvasAxis; AWorldPosition: Double): TNyxCanvasGuide;
function NyxCanvasRulers: TNyxCanvasRulers;
function NyxCanvasPrecisionOptions: TNyxCanvasPrecisionOptions;
{ Projects only visible ticks, including negative values and a shifted origin.
  Empty/unallocated axes return no ticks; no off-screen list is materialized. }
function NyxCanvasRulerTicks(const ARulers: TNyxCanvasRulers;
  const ACamera: TNyxCanvasCamera): TNyxCanvasRulerTicks;
{ Guide tolerance is measured in logical pixels regardless of zoom. Exclusion
  prevents a moving guide from snapping back to its own previous coordinate. }
function SnapNyxCanvasGuides(const AOptions: TNyxCanvasPrecisionOptions;
  const APoint: TNyxCanvasPoint; const ACamera: TNyxCanvasCamera;
  const AExclude: TNyxCanvasGuideRef): TNyxCanvasGuideSnap;

implementation

uses nyx.bytes;

procedure ExactFields(const AValue: TNyxDataValue; const AKeys: array of TNyxText);
var
  LIndex: Integer;
begin

  if (AValue.Kind <> ndObject) or (AValue.Count <> Length(AKeys)) then
  begin
    raise ENyxCanvasPrecision.Create('Precision data requires its exact descriptor shape');
  end;
  for LIndex := 0 to High(AKeys) do
  begin
    AValue.Field(AKeys[LIndex]);
  end;
end;

procedure Positive(AValue, AMinimum, AMaximum: Double; const AName: String);
begin

  if IsNan(AValue) or IsInfinite(AValue) or (AValue < AMinimum) or (AValue > AMaximum) then
  begin
    raise ENyxCanvasPrecision.Create(AName + ' is outside its finite precision range');
  end;
end;

function TNyxCanvasGuideRef.GetDefined: Boolean;
begin
  Result := FName <> '';
end;

function NyxCanvasGuideRef(const AName: TNyxText): TNyxCanvasGuideRef;
begin
  Result.FName := NyxCanvasItem(AName).Name;
end;

procedure TNyxCanvasGuide.RequireDefined;
begin

  if not FReference.Defined then
  begin
    raise ENyxCanvasPrecision.Create('Guide identity is absent');
  end;
end;

function NyxCanvasGuide(const AReference: TNyxCanvasGuideRef;
  AAxis: TNyxCanvasAxis; AWorldPosition: Double): TNyxCanvasGuide;
var
  LPoint: TNyxCanvasPoint;
begin

  if not AReference.Defined or not (AAxis in [ncaxX, ncaxY]) then
  begin
    raise ENyxCanvasPrecision.Create('Guide requires an exact name and measured axis');
  end;
  LPoint := NyxCanvasPoint(AWorldPosition, 0);
  Result := Default(TNyxCanvasGuide);
  Result.FReference := AReference;
  Result.FAxis := AAxis;
  Result.FPosition := LPoint.X;
  Result.FVisible := True;
end;

function TNyxCanvasGuide.AtPosition(AWorldPosition: Double): TNyxCanvasGuide;
var
  LPoint: TNyxCanvasPoint;
begin
  RequireDefined;
  LPoint := NyxCanvasPoint(AWorldPosition, 0);
  Result := Self;
  Result.FPosition := LPoint.X;
end;

function TNyxCanvasGuide.Visible(AValue: Boolean): TNyxCanvasGuide;
begin
  RequireDefined;
  Result := Self;
  Result.FVisible := AValue;
end;

function TNyxCanvasGuide.Locked(AValue: Boolean): TNyxCanvasGuide;
begin
  RequireDefined;
  Result := Self;
  Result.FLocked := AValue;
end;

function TNyxCanvasGuide.ToData: TNyxDataValue;
begin
  RequireDefined;
  Result := NyxObject([NyxField('name', NyxData(FReference.Name)),
    NyxField('axis', NyxData(Ord(FAxis))), NyxField('position', NyxData(FPosition)),
    NyxField('visible', NyxData(FVisible)), NyxField('locked', NyxData(FLocked))]);
end;

class function TNyxCanvasGuide.FromData(const AData: TNyxDataValue): TNyxCanvasGuide;
var
  LAxis: Integer;
begin
  ExactFields(AData, ['name', 'axis', 'position', 'visible', 'locked']);
  LAxis := AData.Field('axis').AsInteger;

  if not (LAxis in [Ord(ncaxX), Ord(ncaxY)]) then
  begin
    raise ENyxCanvasPrecision.Create('Unknown guide axis');
  end;
  Result := NyxCanvasGuide(NyxCanvasGuideRef(AData.Field('name').AsText),
    TNyxCanvasAxis(LAxis), AData.Field('position').AsNumber)
    .Visible(AData.Field('visible').AsBoolean).Locked(AData.Field('locked').AsBoolean);
end;

procedure TNyxCanvasRulers.RequireDefined;
begin

  if not Defined then
  begin
    raise ENyxCanvasPrecision.Create('Ruler settings are absent');
  end;
end;

function NyxCanvasRulers: TNyxCanvasRulers;
begin
  Result := Default(TNyxCanvasRulers);
  Result.FDefined := True;
  Result.FAxes := [ncaxX, ncaxY];
  Result.FOrigin := NyxCanvasPoint(0, 0);
  Result.FUnitSize := 1;
  Result.FIncrement := 1;
  Result.FMinimumPixels := 72;
  Result.FThickness := 24;
  Result.FDivisions := 5;
end;

function TNyxCanvasRulers.Axes(const AValue: TNyxCanvasAxes): TNyxCanvasRulers;
begin
  RequireDefined;
  Result := Self;
  Result.FAxes := AValue;
end;

function TNyxCanvasRulers.FromOrigin(const AValue: TNyxCanvasPoint): TNyxCanvasRulers;
begin
  RequireDefined;

  if not AValue.Defined then
  begin
    raise ENyxCanvasPrecision.Create('Ruler origin is absent');
  end;
  Result := Self;
  Result.FOrigin := AValue;
end;

function TNyxCanvasRulers.Units(AWorldSize: Double; const ACaption: TNyxText): TNyxCanvasRulers;
var
  LIndex: Integer;
  LCount: Integer;
  LScalar: Integer;
begin
  RequireDefined;
  Positive(AWorldSize, 1.0e-12, 1.0e12, 'Ruler unit size');
  LIndex := 1;
  LCount := 0;
  while LIndex <= Length(ACaption) do
  begin
    Inc(LCount);

    if (LCount > 32) or not NyxNextScalar(ACaption, LIndex, LScalar) or
      (LScalar < 32) or ((LScalar >= 127) and (LScalar <= 159)) then
    begin
      raise ENyxCanvasPrecision.Create('Ruler unit caption requires at most 32 single-line scalars');
    end;
  end;
  Result := Self;
  Result.FUnitSize := AWorldSize;
  Result.FUnitCaption := ACaption;
end;

function TNyxCanvasRulers.Interval(AMinimumIncrement, AMinimumPixels: Double;
  AMinorDivisions: Integer): TNyxCanvasRulers;
begin
  RequireDefined;
  Positive(AMinimumIncrement, 1.0e-12, 1.0e12, 'Ruler increment');
  Positive(AMinimumPixels, 32, 512, 'Ruler label spacing');

  if (AMinorDivisions < 1) or (AMinorDivisions > 10) then
  begin
    raise ENyxCanvasPrecision.Create('Rulers require one to ten minor divisions');
  end;
  Result := Self;
  Result.FIncrement := AMinimumIncrement;
  Result.FMinimumPixels := AMinimumPixels;
  Result.FDivisions := AMinorDivisions;
end;

function TNyxCanvasRulers.Thickness(ALogicalPixels: Double): TNyxCanvasRulers;
begin
  RequireDefined;
  Positive(ALogicalPixels, 20, 64, 'Ruler strip size');
  Result := Self;
  Result.FThickness := ALogicalPixels;
end;

function TNyxCanvasRulers.ToData: TNyxDataValue;
begin
  Result := NyxNull;

  if not Defined then
  begin
    Exit;
  end;
  Result := NyxObject([NyxField('x', NyxData(ncaxX in FAxes)),
    NyxField('y', NyxData(ncaxY in FAxes)),
    NyxField('originX', NyxData(FOrigin.X)), NyxField('originY', NyxData(FOrigin.Y)),
    NyxField('unitSize', NyxData(FUnitSize)), NyxField('unitCaption', NyxData(FUnitCaption)),
    NyxField('increment', NyxData(FIncrement)), NyxField('minimumPixels', NyxData(FMinimumPixels)),
    NyxField('thickness', NyxData(FThickness)), NyxField('minorDivisions', NyxData(FDivisions))]);
end;

class function TNyxCanvasRulers.FromData(const AData: TNyxDataValue): TNyxCanvasRulers;
var
  LAxes: TNyxCanvasAxes;
begin
  Result := Default(TNyxCanvasRulers);

  if AData.Kind = ndNull then
  begin
    Exit;
  end;
  ExactFields(AData, ['x', 'y', 'originX', 'originY', 'unitSize', 'unitCaption',
    'increment', 'minimumPixels', 'thickness', 'minorDivisions']);
  LAxes := [];

  if AData.Field('x').AsBoolean then
  begin
    Include(LAxes, ncaxX);
  end;

  if AData.Field('y').AsBoolean then
  begin
    Include(LAxes, ncaxY);
  end;
  Result := NyxCanvasRulers.Axes(LAxes)
    .FromOrigin(NyxCanvasPoint(AData.Field('originX').AsNumber, AData.Field('originY').AsNumber))
    .Units(AData.Field('unitSize').AsNumber, AData.Field('unitCaption').AsText)
    .Interval(AData.Field('increment').AsNumber, AData.Field('minimumPixels').AsNumber,
      AData.Field('minorDivisions').AsInteger)
    .Thickness(AData.Field('thickness').AsNumber);
end;

procedure TNyxCanvasPrecisionOptions.RequireDefined;
begin

  if not Defined then
  begin
    raise ENyxCanvasPrecision.Create('Canvas precision options are absent');
  end;
end;

function NyxCanvasPrecisionOptions: TNyxCanvasPrecisionOptions;
begin
  Result := Default(TNyxCanvasPrecisionOptions);
  Result.FDefined := True;
  Result.FTolerance := 8;
end;

function TNyxCanvasPrecisionOptions.GetCount: Integer;
begin
  Result := Length(FGuides);
end;

function TNyxCanvasPrecisionOptions.IndexOf(const AGuide: TNyxCanvasGuideRef): Integer;
var
  LIndex: Integer;
begin

  if not AGuide.Defined then
  begin
    Exit(-1);
  end;
  for LIndex := 0 to High(FGuides) do
  begin

    if FGuides[LIndex].Reference.Name = AGuide.Name then
    begin
      Exit(LIndex);
    end;
  end;
  Result := -1;
end;

function TNyxCanvasPrecisionOptions.WithRulers(
  const AValue: TNyxCanvasRulers): TNyxCanvasPrecisionOptions;
begin
  RequireDefined;
  Result := Self;
  Result.FRulers := AValue;
end;

function TNyxCanvasPrecisionOptions.WithGuide(
  const AValue: TNyxCanvasGuide): TNyxCanvasPrecisionOptions;
var
  LIndex: Integer;
begin
  RequireDefined;
  AValue.RequireDefined;
  LIndex := IndexOf(AValue.Reference);

  if (LIndex < 0) and (Count >= MaximumNyxCanvasGuides) then
  begin
    raise ENyxCanvasPrecision.Create('Canvas guide capacity is exhausted');
  end;
  Result := Self;
  Result.FGuides := Copy(FGuides, 0, Length(FGuides));

  if LIndex < 0 then
  begin
    LIndex := Length(Result.FGuides);
    SetLength(Result.FGuides, LIndex + 1);
  end;
  Result.FGuides[LIndex] := AValue;
end;

function TNyxCanvasPrecisionOptions.WithoutGuide(
  const AReference: TNyxCanvasGuideRef): TNyxCanvasPrecisionOptions;
var
  LIndex: Integer;
  LNext: Integer;
begin
  RequireDefined;

  if not AReference.Defined then
  begin
    raise ENyxCanvasPrecision.Create('Guide removal requires an exact name');
  end;
  LIndex := IndexOf(AReference);
  Result := Self;

  if LIndex < 0 then
  begin
    Exit;
  end;
  Result.FGuides := Copy(FGuides, 0, Length(FGuides) - 1);
  for LNext := 0 to High(Result.FGuides) do
  begin

    if LNext < LIndex then
    begin
      Result.FGuides[LNext] := FGuides[LNext];
    end
    else
    begin
      Result.FGuides[LNext] := FGuides[LNext + 1];
    end;
  end;
end;

function TNyxCanvasPrecisionOptions.Snapping(AMode: TNyxCanvasGuideSnapping;
  ATolerancePixels: Double): TNyxCanvasPrecisionOptions;
begin
  RequireDefined;

  if not (AMode in [ncgspOff, ncgspVisible, ncgspAll]) then
  begin
    raise ENyxCanvasPrecision.Create('Unknown guide snapping policy');
  end;
  Positive(ATolerancePixels, 0, 64, 'Guide snap tolerance');
  Result := Self;
  Result.FSnapping := AMode;
  Result.FTolerance := ATolerancePixels;
end;

function TNyxCanvasPrecisionOptions.Guide(AIndex: Integer): TNyxCanvasGuide;
begin

  if (AIndex < 0) or (AIndex >= Count) then
  begin
    raise ENyxCanvasPrecision.Create('Guide index is outside this immutable options snapshot');
  end;
  Result := FGuides[AIndex];
end;

function TNyxCanvasPrecisionOptions.Contains(const AReference: TNyxCanvasGuideRef): Boolean;
begin
  Result := IndexOf(AReference) >= 0;
end;

function TNyxCanvasPrecisionOptions.Named(const AReference: TNyxCanvasGuideRef): TNyxCanvasGuide;
var
  LIndex: Integer;
begin
  LIndex := IndexOf(AReference);

  if LIndex < 0 then
  begin
    raise ENyxCanvasPrecision.Create('Guide name is not present');
  end;
  Result := FGuides[LIndex];
end;

function TNyxCanvasPrecisionOptions.NextGuide: TNyxCanvasGuideRef;
var
  LIndex: Integer;
begin
  RequireDefined;
  for LIndex := 1 to MaximumNyxCanvasGuides + 1 do
  begin
    Result := NyxCanvasGuideRef(TNyxText('Guide ') + IntToStr(LIndex));

    if not Contains(Result) then
    begin
      Exit;
    end;
  end;
  raise ENyxCanvasPrecision.Create('Canvas guide capacity is exhausted');
end;

function TNyxCanvasPrecisionOptions.ToData: TNyxDataValue;
var
  LGuides: array of TNyxDataValue;
  LIndex: Integer;
begin
  Result := NyxNull;

  if not Defined then
  begin
    Exit;
  end;
  SetLength(LGuides, Count);
  for LIndex := 0 to High(LGuides) do
  begin
    LGuides[LIndex] := FGuides[LIndex].ToData;
  end;
  Result := NyxObject([NyxField('version', NyxData(1)), NyxField('rulers', FRulers.ToData),
    NyxField('guides', NyxArray(LGuides)), NyxField('snapping', NyxData(Ord(FSnapping))),
    NyxField('tolerancePixels', NyxData(FTolerance))]);

  if NyxUTF8ByteCount(Result.ToJSON) > MaximumNyxCanvasPrecisionBytes then
  begin
    raise ENyxCanvasPrecision.Create('Canvas precision descriptor exceeds its byte budget');
  end;
end;

class function TNyxCanvasPrecisionOptions.FromData(
  const AData: TNyxDataValue): TNyxCanvasPrecisionOptions;
var
  LGuides: TNyxDataValue;
  LGuide: TNyxCanvasGuide;
  LMode: Integer;
  LIndex: Integer;
begin
  Result := Default(TNyxCanvasPrecisionOptions);

  if AData.Kind = ndNull then
  begin
    Exit;
  end;
  ExactFields(AData, ['version', 'rulers', 'guides', 'snapping', 'tolerancePixels']);

  if (AData.Field('version').AsInteger <> 1) or
    (NyxUTF8ByteCount(AData.ToJSON) > MaximumNyxCanvasPrecisionBytes) then
  begin
    raise ENyxCanvasPrecision.Create('Canvas precision version or byte budget is unsupported');
  end;
  LGuides := AData.Field('guides');

  if (LGuides.Kind <> ndArray) or (LGuides.Count > MaximumNyxCanvasGuides) then
  begin
    raise ENyxCanvasPrecision.Create('Canvas guide list exceeds its bounded capacity');
  end;
  LMode := AData.Field('snapping').AsInteger;

  if not (LMode in [Ord(ncgspOff), Ord(ncgspVisible), Ord(ncgspAll)]) then
  begin
    raise ENyxCanvasPrecision.Create('Unknown guide snapping policy');
  end;
  Result := NyxCanvasPrecisionOptions.WithRulers(TNyxCanvasRulers.FromData(AData.Field('rulers')))
    .Snapping(TNyxCanvasGuideSnapping(LMode), AData.Field('tolerancePixels').AsNumber);
  for LIndex := 0 to LGuides.Count - 1 do
  begin
    LGuide := TNyxCanvasGuide.FromData(LGuides.Item(LIndex));

    if Result.Contains(LGuide.Reference) then
    begin
      raise ENyxCanvasPrecision.Create('Precision data repeats a guide identity');
    end;
    Result := Result.WithGuide(LGuide);
  end;
end;

function TickCaption(AValue, AStep: Double): TNyxText;
var
  LFormat: TFormatSettings;
begin
  LFormat := FormatSettings;
  LFormat.DecimalSeparator := '.';

  if Abs(AValue) < Abs(AStep) * 1.0e-9 then
  begin
    AValue := 0;
  end;

  if (Abs(AValue) >= 1.0e9) or ((AValue <> 0) and (Abs(AValue) < 1.0e-6)) then
  begin
    Result := TNyxText(FormatFloat('0.######E+0', AValue, LFormat));
  end
  else
  begin
    Result := TNyxText(FormatFloat('0.##########', AValue, LFormat));
  end;
end;

function NyxCanvasRulerTicks(const ARulers: TNyxCanvasRulers;
  const ACamera: TNyxCanvasCamera): TNyxCanvasRulerTicks;
var
  LAxis: TNyxCanvasAxis;
  LExtent: Double;
  LCenter: Double;
  LOrigin: Double;
  LStep: Double;
  LRequired: Double;
  LFactor: Integer;
  LMinor: Double;
  LFirst: Double;
  LWorld: Double;
  LPixel: Double;
  LTickIndex: Double;
  LCount: Integer;
  LIndex: Integer;
  LBaseCount: Integer;
  LMajor: Boolean;
begin
  Result := nil;

  if not ARulers.Defined then
  begin
    Exit;
  end;

  if not ACamera.Defined then
  begin
    raise ENyxCanvasPrecision.Create('Ruler projection requires a defined camera');
  end;
  for LAxis := Low(TNyxCanvasAxis) to High(TNyxCanvasAxis) do
  begin

    if not (LAxis in ARulers.VisibleAxes) then
    begin
      Continue;
    end;

    if LAxis = ncaxX then
    begin
      LExtent := ACamera.ViewportWidth;
      LCenter := ACamera.WorldCenter.X;
      LOrigin := ARulers.Origin.X;
    end
    else
    begin
      LExtent := ACamera.ViewportHeight;
      LCenter := ACamera.WorldCenter.Y;
      LOrigin := ARulers.Origin.Y;
    end;

    if LExtent <= 0 then
    begin
      Continue;
    end;
    LRequired := Max(ARulers.MinimumPixels, LExtent * ARulers.MinorDivisions / 250) /
      ACamera.Zoom;
    { Bound the index even near either world extreme; no target relies on
      integer identity beyond IEEE-754's exact range. Precision may coarsen. }
    LRequired := Max(LRequired, Abs(LCenter - LOrigin) / 1.0e13 *
      ARulers.MinorDivisions);
    LStep := ARulers.Increment * ARulers.UnitSize;
    LFactor := 0;
    while LStep < LRequired do
    begin
      { Multiplication avoids target-specific logarithm rounding at decimal
        boundaries. Every iteration stores its admitted Double before reuse. }
      case LFactor of
        0, 2:
          begin
            LStep := LStep * 2;
          end;
        1:
          begin
            LStep := LStep * 2.5;
          end;
      end;
      LFactor := (LFactor + 1) mod 3;
    end;
    LMinor := LStep / ARulers.MinorDivisions;
    LFirst := Ceil((LCenter - LExtent / (2 * ACamera.Zoom) - LOrigin) / LMinor);
    LCount := Floor(LExtent / (LMinor * ACamera.Zoom)) + 2;

    if LCount > MaximumNyxCanvasRulerTicks div 2 then
    begin
      raise ENyxCanvasPrecision.Create('Ruler projection exceeds its admitted tick budget');
    end;
    LBaseCount := Length(Result);
    SetLength(Result, LBaseCount + LCount);
    LCount := 0;
    for LIndex := 0 to High(Result) - LBaseCount do
    begin
      LTickIndex := LFirst + LIndex;
      LWorld := LOrigin + LTickIndex * LMinor;
      LPixel := (LWorld - LCenter) * ACamera.Zoom + LExtent / 2;

      if (LPixel < -1.0e-7) or (LPixel > LExtent + 1.0e-7) then
      begin
        Continue;
      end;
      LMajor := Abs(LTickIndex - Round(LTickIndex / ARulers.MinorDivisions) *
        ARulers.MinorDivisions) < 0.25;
      Result[LBaseCount + LCount].Axis := LAxis;
      Result[LBaseCount + LCount].Pixel := Max(0, Min(LExtent, LPixel));
      Result[LBaseCount + LCount].Value := (LWorld - LOrigin) / ARulers.UnitSize;
      Result[LBaseCount + LCount].Major := LMajor;

      if LMajor then
      begin
        Result[LBaseCount + LCount].Caption := TickCaption(
          Result[LBaseCount + LCount].Value, LStep / ARulers.UnitSize);
      end;
      Inc(LCount);
    end;
    SetLength(Result, LBaseCount + LCount);
  end;
end;

function SnapNyxCanvasGuides(const AOptions: TNyxCanvasPrecisionOptions;
  const APoint: TNyxCanvasPoint; const ACamera: TNyxCanvasCamera;
  const AExclude: TNyxCanvasGuideRef): TNyxCanvasGuideSnap;
var
  LIndex: Integer;
  LGuide: TNyxCanvasGuide;
  LX: Double;
  LY: Double;
  LDistance: Double;
  LXDistance: Double;
  LYDistance: Double;
begin

  if not APoint.Defined or not ACamera.Defined then
  begin
    raise ENyxCanvasPrecision.Create('Guide snapping requires an admitted point and camera');
  end;
  Result := Default(TNyxCanvasGuideSnap);
  Result.Point := APoint;

  if not AOptions.Defined or (AOptions.SnapMode = ncgspOff) then
  begin
    Exit;
  end;
  LX := APoint.X;
  LY := APoint.Y;
  LXDistance := AOptions.TolerancePixels + 1;
  LYDistance := LXDistance;
  for LIndex := 0 to AOptions.Count - 1 do
  begin
    LGuide := AOptions.Guide(LIndex);

    if (LGuide.Reference.Name = AExclude.Name) or
      ((AOptions.SnapMode = ncgspVisible) and not LGuide.IsVisible) then
    begin
      Continue;
    end;

    if LGuide.Axis = ncaxX then
    begin
      LDistance := Abs(LGuide.Position - APoint.X) * ACamera.Zoom;

      if (LDistance <= AOptions.TolerancePixels) and (LDistance < LXDistance) then
      begin
        LX := LGuide.Position;
        LXDistance := LDistance;
        Result.XGuide := LGuide.Reference;
      end;
    end
    else
    begin
      LDistance := Abs(LGuide.Position - APoint.Y) * ACamera.Zoom;

      if (LDistance <= AOptions.TolerancePixels) and (LDistance < LYDistance) then
      begin
        LY := LGuide.Position;
        LYDistance := LDistance;
        Result.YGuide := LGuide.Reference;
      end;
    end;
  end;
  Result.Point := NyxCanvasPoint(LX, LY);
end;

end.
