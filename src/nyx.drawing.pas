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
unit nyx.drawing;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, Math, nyx.text, nyx.bytes, nyx.data, nyx.colors, nyx.theme,
  nyx.design.tokens.core, nyx.canvas.geometry;

const
  NyxDrawingKey = 'nyx.drawing';
  NyxDrawingProviderKey = 'nyx.drawingProvider';
  MaximumNyxDrawingCommands = 2048;
  MaximumNyxDrawingPathSteps = 1024;
  MaximumNyxDrawingBytes = 1048576;
  MaximumNyxDrawingSamplePoints = 65536;

type
  { Refusal before a drawing candidate can replace an admitted plan or builder. }
  ENyxDrawing = class(EArgumentException);
  { Transparent, literal RGB and semantic palette paint are distinct choices. }
  TNyxDrawingPaintKind = (ndpkNone, ndpkRGB, ndpkTheme);
  { Closed horizontal text origin; the vertical coordinate always addresses its top. }
  TNyxDrawingTextAnchor = (ndtaStart, ndtaCenter, ndtaEnd);

  { Optional immutable paint. None is transparent, independently of black.
    Theme roles resolve at presentation; a saved literal never follows a role. }
  TNyxDrawingPaint = record
  private
    FKind: TNyxDrawingPaintKind;
    FColor: TNyxRGBColor;
    FRole: TNyxThemeColor;
  public
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxDrawingPaint; static;
    property Kind: TNyxDrawingPaintKind read FKind;
    property Color: TNyxRGBColor read FColor;
    property Role: TNyxThemeColor read FRole;
  end;

  { Closed stroke/fill policy, in drawing view-box units. No stroke means width
    zero; an explicit stroke requires finite 0.1..64. These are copied values. }
  TNyxDrawingStyle = record
  private
    FFill: TNyxDrawingPaint;
    FStroke: TNyxDrawingPaint;
    FWidth: Double;
  public
    function Fill(const APaint: TNyxDrawingPaint): TNyxDrawingStyle;
    function Stroke(const APaint: TNyxDrawingPaint; AWidth: Double = 1): TNyxDrawingStyle;
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxDrawingStyle; static;
    property FillPaint: TNyxDrawingPaint read FFill;
    property StrokePaint: TNyxDrawingPaint read FStroke;
    property StrokeWidth: Double read FWidth;
  end;

  { Closed geometric instructions. Move begins a path, Line/Curve extend it,
    and Close retires its open figure. Ordinals retain the version-one wire. }
  TNyxDrawingStepKind = (ndskMove, ndskLine, ndskCurve, ndskClose);

  { Immutable value-only geometric instruction, obtained from an admitted path.
    Point reads return independent values; Curve owns two controls and one end.
    Default is absent, and all meaningful reads refuse it. No host is retained. }
  TNyxDrawingStep = record
  private
    FDefined: Boolean;
    FKind: TNyxDrawingStepKind;
    FFirst: TNyxCanvasPoint;
    FSecond: TNyxCanvasPoint;
    FEnd: TNyxCanvasPoint;
    function GetKind: TNyxDrawingStepKind;
  public
    { Move/Line admit index zero; Curve admits controls zero/one and end two.
      Close, absent steps and all other indices refuse. }
    function Point(AIndex: Integer): TNyxCanvasPoint;
    { Explicit version-one wire encoding; absent instructions refuse. }
    function ToData: TNyxDataValue;
    property Kind: TNyxDrawingStepKind read GetKind;
    property Defined: Boolean read FDefined;
  end;

  { One contiguous typed path. Coordinates/control points must belong to the
    plan's view box at admission. Close ends the figure; further segments refuse.
    JSON is an explicit persistence boundary, never SVG/code to execute.
    Step/StepCount expose immutable values without crossing that boundary. }
  TNyxDrawingPath = record
  private
    { Private immutable sequence. Append copies before writing; keeping typed
      steps here avoids reparsing every preceding JSON segment during fluent
      construction. Serialization happens once at the explicit boundary. }
    FSteps: array of TNyxDrawingStep;
    function Append(const AStep: TNyxDrawingStep): TNyxDrawingPath;
    function GetStepCount: Integer;
  public
    function LineTo(const APoint: TNyxCanvasPoint): TNyxDrawingPath;
    function CurveTo(const AFirst, ASecond, AEnd: TNyxCanvasPoint): TNyxDrawingPath;
    function Close: TNyxDrawingPath;
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxDrawingPath; static;
    { Bounded immutable value reads; they do not encode or parse wire data. }
    function Step(AIndex: Integer): TNyxDrawingStep;
    property StepCount: Integer read GetStepCount;
  end;

  { Detached sampled polyline in world units; callers own the mutable array and
    adapters never retain it beyond their paint/build operation. }
  TNyxDrawingPoints = array of TNyxCanvasPoint;

  { Closed live command vocabulary. Absent is the default/refusing record;
    Path/Text map explicitly to the existing version-one wire ordinals. }
  TNyxDrawingCommandKind = (ndckAbsent, ndckPath, ndckText);

  { Immutable admitted command value, obtained from INyxDrawingCommands. It owns
    typed geometry/paint/text only. Kind-specific getters refuse mismatched or
    absent commands; path extension always derives an independent sequence. }
  TNyxDrawingCommand = record
  private
    FKind: TNyxDrawingCommandKind;
    FPath: TNyxDrawingPath;
    FStyle: TNyxDrawingStyle;
    FText: TNyxText;
    FPosition: TNyxCanvasPoint;
    FPaint: TNyxDrawingPaint;
    FSize: Double;
    FAnchor: TNyxDrawingTextAnchor;
    FBold: Boolean;
    procedure RequireKind(AKind: TNyxDrawingCommandKind);
    function GetPath: TNyxDrawingPath;
    function GetStyle: TNyxDrawingStyle;
    function GetText: TNyxText;
    function GetPosition: TNyxCanvasPoint;
    function GetPaint: TNyxDrawingPaint;
    function GetSize: Double;
    function GetAnchor: TNyxDrawingTextAnchor;
    function GetBold: Boolean;
  public
    { Encode the existing version-one path/text schema; absent commands refuse. }
    function ToData: TNyxDataValue;
    property Kind: TNyxDrawingCommandKind read FKind;
    property Path: TNyxDrawingPath read GetPath;
    property Style: TNyxDrawingStyle read GetStyle;
    property Text: TNyxText read GetText;
    property Position: TNyxCanvasPoint read GetPosition;
    property Paint: TNyxDrawingPaint read GetPaint;
    property Size: Double read GetSize;
    property Anchor: TNyxDrawingTextAnchor read GetAnchor;
    property Bold: Boolean read GetBold;
  end;

  { Optional bounded typed read port. NyxDrawingCommands first captures complete
    creator intent, so implementing this port alone grants no admission bypass.
    A reader retains its immutable plan, never a document, view or application. }
  INyxDrawingCommands = interface(IInterface)
    ['{D1401014-4CA0-4C84-A708-000000000004}']
    function GetCount: Integer;
    { Zero-based bounded value read. Holding/extending a returned path cannot
      mutate this reader or a previously returned command. }
    function TypedCommand(AIndex: Integer): TNyxDrawingCommand;
    property Count: Integer read GetCount;
  end;

  { Immutable display list. Commands own typed normalized data, never callbacks,
    platform objects, a document, renderer or live mutable builder. Reads return
    independent immutable values. Providers may build any geometry with paths. }
  INyxDrawingPlan = interface(IInterface)
    ['{D1401014-4CA0-4C84-A708-000000000001}']
    function GetViewBox: TNyxCanvasBounds;
    function GetCount: Integer;
    function Command(AIndex: Integer): TNyxDataValue;
    function ToData: TNyxDataValue;
    property ViewBox: TNyxCanvasBounds read GetViewBox;
    property Count: Integer read GetCount;
  end;

  { Authoring-thread fluent builder. Build captures an independent plan;
    continued authoring never changes a previously published plan. A failed
    command preserves the builder. Paths/text are bounded before appending. }
  INyxDrawingBuilder = interface(IInterface)
    ['{D1401014-4CA0-4C84-A708-000000000002}']
    function Path(const APath: TNyxDrawingPath; const AStyle: TNyxDrawingStyle): INyxDrawingBuilder;
    function Text(const AText: TNyxText; const APosition: TNyxCanvasPoint;
      const APaint: TNyxDrawingPaint; ASize: Double = 14;
      AAnchor: TNyxDrawingTextAnchor = ndtaStart; ABold: Boolean = False): INyxDrawingBuilder;
    function Build: INyxDrawingPlan;
  end;

  { Distinct provider identity is portable configuration, never a class name,
    script or executable callback. A host explicitly registers its implementation. }
  TNyxDrawingProviderRef = record
  private
    FName: TNyxText;
  public
    property Name: TNyxText read FName;
    function Defined: Boolean;
  end;

  { Value-only UI-owned construction frame. Providers receive copied dimensions
    and immutable effective colors/metrics, and return a detached immutable plan.
    They must not borrow/mutate renderer/document objects or retain view cycles.
    Rendering/execution/callback handling stays with normal Nyx control adapters. }
  TNyxDrawingFrame = record
    Width: Double;
    Height: Double;
    Palette: TNyxThemeTokens;
  end;
  { UI-owned synchronous geometry producer returns an independent immutable plan.
    It must retain no renderer/model cycle; errors refuse the prepared picture. }
  INyxDrawingProvider = interface(IInterface)
    ['{D1401014-4CA0-4C84-A708-000000000003}']
    function Build(const AFrame: TNyxDrawingFrame): INyxDrawingPlan;
  end;

function NyxDrawingPaint(const AColor: TNyxRGBColor): TNyxDrawingPaint;
function NyxThemePaint(ARole: TNyxThemeColor): TNyxDrawingPaint;
function NyxDrawingStyle: TNyxDrawingStyle;
function NyxDrawingPath(const AStart: TNyxCanvasPoint): TNyxDrawingPath;
function NyxDrawingRectangle(const ABounds: TNyxCanvasBounds): TNyxDrawingPath;
function NewNyxDrawingPlan(const AViewBox: TNyxCanvasBounds): INyxDrawingBuilder;
{ Capture a live immutable display list. Exact private built-in plans have
  already passed command/sample/bounds/UTF-8 limits and can be retained directly.
  Other implementations are captured once through their complete wire value and
  strict decoder, without retaining the creator. Nil refuses. This adds no cache
  or mutable shared state; builders still detach every published command array. }
function CaptureNyxDrawingPlan(const APlan: INyxDrawingPlan): INyxDrawingPlan;
{ Capture full intent before looking up its typed reader. Nil/invalid creators
  refuse through the same boundary; the result owns only the admitted plan. }
function NyxDrawingCommands(const APlan: INyxDrawingPlan): INyxDrawingCommands;
function NyxDrawingPlanFromData(const AData: TNyxDataValue): INyxDrawingPlan;
function NyxDrawingProvider(const AName: TNyxText): TNyxDrawingProviderRef;
{ Shared deterministic cubic sampling. Adapters use the same points and map to
  their admitted logical allocation; native integer rounding remains explicit. }
function NyxDrawingPathPoints(const APath: TNyxDrawingPath): TNyxDrawingPoints;
function ResolveNyxDrawingPaint(const APaint: TNyxDrawingPaint; ATheme: TNyxTheme): TNyxRGBColor;
function NyxDrawingPalette(ATheme: TNyxTheme): TNyxThemeTokens;

implementation

const
  { Closed version-one JSON framing, including typed paints and number fields.
    These conservative bounds avoid encoding small live plans. Larger bounds
    always fall back to exact serialization instead of refusing valid intent. }
  DrawingRootByteBound = 256;
  DrawingCommandByteBound = 256;
  { Shared number formatting uses at most 17 significant digits, short exponent
    notation or ordinary decimals only for exponents -6..16: at most 25 bytes. }
  DrawingNumberByteBound = 32;
  DrawingPointByteBound = 2 * DrawingNumberByteBound + 4;
  { Step brackets/ordinal/comma spacing and worst-case escaped scalar length. }
  DrawingStepByteBound = 8;
  DrawingScalarByteBound = 12;

type
  { Immutable admitted typed storage. Build detaches the command array; nested
    paths own immutable step sequences and text is an owned value. No builder/view or
    application is retained and subsequent builder edits cannot change a plan. }
  TPlan = class(TInterfacedObject, INyxDrawingPlan, INyxDrawingCommands)
  private
    FBox: TNyxCanvasBounds;
    FCommands: array of TNyxDrawingCommand;
  public
    function GetViewBox: TNyxCanvasBounds;
    function GetCount: Integer;
    function Command(AIndex: Integer): TNyxDataValue;
    function TypedCommand(AIndex: Integer): TNyxDrawingCommand;
    function ToData: TNyxDataValue;
  end;
  { Single-author construction owner. Typed methods admit complete commands and
    their sampled-point costs before appending; Build captures independent plans
    with the same persistence-byte bound. No caller-owned array is borrowed. }
  TBuilder = class(TInterfacedObject, INyxDrawingBuilder)
  private
    FBox: TNyxCanvasBounds;
    FCommands: array of TNyxDrawingCommand;
    FSamplePoints: Integer;
    FByteBound: Integer;
    { Only the typed Path/Text entries call this, with their admitted point cost.
      Wire callers still pass through the complete strict path/style decoders. }
    procedure Add(const ACommand: TNyxDrawingCommand; ASamplePoints, AByteBound: Integer);
    procedure AdmitPoint(const APoint: TNyxCanvasPoint);
  public
    constructor Create(const ABox: TNyxCanvasBounds);
    function Path(const APath: TNyxDrawingPath; const AStyle: TNyxDrawingStyle): INyxDrawingBuilder;
    function Text(const AText: TNyxText; const APosition: TNyxCanvasPoint;
      const APaint: TNyxDrawingPaint; ASize: Double; AAnchor: TNyxDrawingTextAnchor;
      ABold: Boolean): INyxDrawingBuilder;
    function Build: INyxDrawingPlan;
  end;

procedure Fields(const AData: TNyxDataValue; const AKeys: array of TNyxText);
var
  LIndex: Integer;
begin

  if (AData.Kind <> ndObject) or (AData.Count <> Length(AKeys)) then
  begin
    raise ENyxDrawing.Create('Drawing value requires its exact closed fields');
  end;
  for LIndex := 0 to High(AKeys) do
  begin
    AData.Field(AKeys[LIndex]);
  end;
end;

function DrawingScalarCount(const AText: TNyxText): Integer;
var
  LIndex: Integer;
  LScalar: Integer;
begin
  Result := 0;
  LIndex := 1;
  while LIndex <= Length(AText) do
  begin

    if not NyxNextScalar(AText, LIndex, LScalar) then
    begin
      raise ENyxDrawing.Create('Drawing text contains malformed Unicode');
    end;
    Inc(Result);
  end;
end;

function PointData(const APoint: TNyxCanvasPoint): TNyxDataValue;
begin
  NyxCanvasPoint(APoint.X, APoint.Y);
  Result := NyxArray([NyxData(APoint.X), NyxData(APoint.Y)]);
end;

function ReadPoint(const AValue: TNyxDataValue): TNyxCanvasPoint;
begin

  if (AValue.Kind <> ndArray) or (AValue.Count <> 2) then
  begin
    raise ENyxDrawing.Create('Drawing point requires two numeric coordinates');
  end;
  Result := NyxCanvasPoint(AValue.Item(0).AsNumber, AValue.Item(1).AsNumber);
end;

function NyxDrawingPaint(const AColor: TNyxRGBColor): TNyxDrawingPaint;
begin
  Result := Default(TNyxDrawingPaint);

  if AColor.Defined then
  begin
    Result.FKind := ndpkRGB;
    Result.FColor := AColor;
  end;
end;

function NyxThemePaint(ARole: TNyxThemeColor): TNyxDrawingPaint;
begin
  NyxThemeColorName(ARole);
  Result := Default(TNyxDrawingPaint);
  Result.FKind := ndpkTheme;
  Result.FRole := ARole;
end;

function TNyxDrawingPaint.ToData: TNyxDataValue;
begin
  case FKind of
    ndpkNone: Result := NyxNull;
    ndpkRGB: Result := NyxObject([NyxField('rgb', NyxData(FColor.ToText))]);
    ndpkTheme: Result := NyxObject([NyxField('theme', NyxData(Ord(FRole)))]);
  end;
end;

class function TNyxDrawingPaint.FromData(const AData: TNyxDataValue): TNyxDrawingPaint;
var
  LRole: Integer;
begin
  Result := Default(TNyxDrawingPaint);

  if AData.Kind = ndNull then
  begin
    Exit;
  end;

  if (AData.Kind <> ndObject) or (AData.Count <> 1) then
  begin
    raise ENyxDrawing.Create('Drawing paint requires transparent, literal RGB or typed theme role');
  end;

  if AData.Key(0) = 'rgb' then
  begin
    Result := NyxDrawingPaint(TNyxRGBColor.FromText(AData.Field('rgb').AsText));

    if Result.Kind <> ndpkRGB then
    begin
      raise ENyxDrawing.Create('Literal drawing color must be defined');
    end;
  end
  else if AData.Key(0) = 'theme' then
  begin
    LRole := AData.Field('theme').AsInteger;

    if (LRole < Ord(Low(TNyxThemeColor))) or (LRole > Ord(High(TNyxThemeColor))) then
    begin
      raise ENyxDrawing.Create('Unknown drawing theme role');
    end;
    Result := NyxThemePaint(TNyxThemeColor(LRole));
  end
  else
  begin
    raise ENyxDrawing.Create('Unknown drawing paint field');
  end;
end;

function NyxDrawingStyle: TNyxDrawingStyle;
begin
  Result := Default(TNyxDrawingStyle);
end;

function TNyxDrawingStyle.Fill(const APaint: TNyxDrawingPaint): TNyxDrawingStyle;
begin
  Result := Self;
  Result.FFill := APaint;
end;

function TNyxDrawingStyle.Stroke(const APaint: TNyxDrawingPaint; AWidth: Double): TNyxDrawingStyle;
begin
  Result := Self;
  Result.FStroke := APaint;

  if Result.FStroke.Kind = ndpkNone then
  begin
    Result.FWidth := 0;
  end
  else
  begin

    if IsNan(AWidth) or IsInfinite(AWidth) or (AWidth < 0.1) or (AWidth > 64) then
    begin
      raise ENyxDrawing.Create('Drawing stroke width requires finite 0.1..64 view-box units');
    end;
    Result.FWidth := AWidth;
  end;
end;

function TNyxDrawingStyle.ToData: TNyxDataValue;
begin
  Result := NyxObject([NyxField('fill', FFill.ToData), NyxField('stroke', FStroke.ToData),
    NyxField('width', NyxData(FWidth))]);
end;

class function TNyxDrawingStyle.FromData(const AData: TNyxDataValue): TNyxDrawingStyle;
begin
  Fields(AData, ['fill', 'stroke', 'width']);
  Result := NyxDrawingStyle.Fill(TNyxDrawingPaint.FromData(AData.Field('fill')))
    .Stroke(TNyxDrawingPaint.FromData(AData.Field('stroke')), AData.Field('width').AsNumber);

  if (Result.StrokePaint.Kind = ndpkNone) and (AData.Field('width').AsNumber <> 0) then
  begin
    raise ENyxDrawing.Create('An absent drawing stroke has zero width');
  end;
end;

function TNyxDrawingStep.GetKind: TNyxDrawingStepKind;
begin

  if not FDefined then
  begin
    raise ENyxDrawing.Create('Construct a drawing step first');
  end;
  Result := FKind;
end;

function TNyxDrawingStep.Point(AIndex: Integer): TNyxCanvasPoint;
begin

  if (GetKind = ndskClose) or (AIndex < 0) or (AIndex > 2) or
    ((FKind <> ndskCurve) and (AIndex <> 0)) then
  begin
    raise ENyxDrawing.Create('Drawing step point is outside its instruction');
  end;
  case AIndex of
    0: Result := FFirst;
    1: Result := FSecond;
    2: Result := FEnd;
  end;
end;

function TNyxDrawingStep.ToData: TNyxDataValue;
begin
  case GetKind of
    ndskMove, ndskLine: Result := NyxArray([NyxData(Ord(FKind)), PointData(FFirst)]);
    ndskCurve: Result := NyxArray([NyxData(2), PointData(FFirst), PointData(FSecond), PointData(FEnd)]);
    ndskClose: Result := NyxArray([NyxData(3)]);
  end;
end;

{ Private typed construction is the only writer of instruction coordinates.
  Validate points immediately; later builder admission adds the owning view box. }
function DrawingStep(AKind: TNyxDrawingStepKind; const AFirst, ASecond,
  AEnd: TNyxCanvasPoint): TNyxDrawingStep;
begin
  Result := Default(TNyxDrawingStep);
  Result.FDefined := True;
  Result.FKind := AKind;

  if AKind <> ndskClose then
  begin
    Result.FFirst := NyxCanvasPoint(AFirst.X, AFirst.Y);
  end;

  if AKind = ndskCurve then
  begin
    Result.FSecond := NyxCanvasPoint(ASecond.X, ASecond.Y);
    Result.FEnd := NyxCanvasPoint(AEnd.X, AEnd.Y);
  end;
end;

function NyxDrawingPath(const AStart: TNyxCanvasPoint): TNyxDrawingPath;
begin
  Result := Default(TNyxDrawingPath);
  SetLength(Result.FSteps, 1);
  Result.FSteps[0] := DrawingStep(ndskMove, AStart, Default(TNyxCanvasPoint), Default(TNyxCanvasPoint));
end;

function TNyxDrawingPath.Append(const AStep: TNyxDrawingStep): TNyxDrawingPath;
begin

  if (Length(FSteps) = 0) or (Length(FSteps) >= MaximumNyxDrawingPathSteps) or
    (FSteps[High(FSteps)].Kind = ndskClose) then
  begin
    raise ENyxDrawing.Create('Construct an open path with available segment capacity');
  end;
  Result := Default(TNyxDrawingPath);
  Result.FSteps := Copy(FSteps, 0, Length(FSteps));
  SetLength(Result.FSteps, Length(FSteps) + 1);
  Result.FSteps[High(Result.FSteps)] := AStep;
end;

function TNyxDrawingPath.LineTo(const APoint: TNyxCanvasPoint): TNyxDrawingPath;
begin
  Result := Append(DrawingStep(ndskLine, APoint, Default(TNyxCanvasPoint), Default(TNyxCanvasPoint)));
end;

function TNyxDrawingPath.CurveTo(const AFirst, ASecond, AEnd: TNyxCanvasPoint): TNyxDrawingPath;
begin
  Result := Append(DrawingStep(ndskCurve, AFirst, ASecond, AEnd));
end;

function TNyxDrawingPath.Close: TNyxDrawingPath;
begin
  Result := Append(DrawingStep(ndskClose, Default(TNyxCanvasPoint),
    Default(TNyxCanvasPoint), Default(TNyxCanvasPoint)));
end;

function TNyxDrawingPath.ToData: TNyxDataValue;
var
  LSteps: array of TNyxDataValue;
  LIndex: Integer;
begin

  if Length(FSteps) = 0 then
  begin
    raise ENyxDrawing.Create('Construct a drawing path first');
  end;
  SetLength(LSteps, Length(FSteps));
  for LIndex := 0 to High(FSteps) do
  begin
    LSteps[LIndex] := FSteps[LIndex].ToData;
  end;
  Result := NyxArray(LSteps);
end;

function TNyxDrawingPath.GetStepCount: Integer;
begin
  Result := Length(FSteps);
end;

function TNyxDrawingPath.Step(AIndex: Integer): TNyxDrawingStep;
begin

  if (AIndex < 0) or (AIndex >= Length(FSteps)) then
  begin
    raise ENyxDrawing.Create('Drawing step is outside its path');
  end;
  Result := FSteps[AIndex];
end;

class function TNyxDrawingPath.FromData(const AData: TNyxDataValue): TNyxDrawingPath;
var
  LStep: TNyxDataValue;
  LKind: Integer;
  LIndex: Integer;
begin

  if (AData.Kind <> ndArray) or (AData.Count = 0) or
    (AData.Count > MaximumNyxDrawingPathSteps) then
  begin
    raise ENyxDrawing.Create('Drawing path exceeds its segment boundary');
  end;
  for LIndex := 0 to AData.Count - 1 do
  begin
    LStep := AData.Item(LIndex);

    if (LStep.Kind <> ndArray) or (LStep.Count = 0) then
    begin
      raise ENyxDrawing.Create('Drawing segment requires a closed typed shape');
    end;
    LKind := LStep.Item(0).AsInteger;

    if LIndex = 0 then
    begin

      if (LKind <> 0) or (LStep.Count <> 2) then
      begin
        raise ENyxDrawing.Create('Drawing path begins with one move');
      end;
      Result := NyxDrawingPath(ReadPoint(LStep.Item(1)));
    end
    else
    begin
      case LKind of
        1:
          begin

            if LStep.Count <> 2 then
            begin
              raise ENyxDrawing.Create('Line requires one endpoint');
            end;
            Result := Result.LineTo(ReadPoint(LStep.Item(1)));
          end;
        2:
          begin

            if LStep.Count <> 4 then
            begin
              raise ENyxDrawing.Create('Curve requires two controls and endpoint');
            end;
            Result := Result.CurveTo(ReadPoint(LStep.Item(1)), ReadPoint(LStep.Item(2)),
              ReadPoint(LStep.Item(3)));
          end;
        3:
          begin

            if LStep.Count <> 1 then
            begin
              raise ENyxDrawing.Create('Close takes no points');
            end;
            Result := Result.Close;
          end;
      else
        raise ENyxDrawing.Create('Unknown drawing path segment');
      end;
    end;
  end;
end;

function NyxDrawingRectangle(const ABounds: TNyxCanvasBounds): TNyxDrawingPath;
begin

  if not ABounds.Defined then
  begin
    raise ENyxDrawing.Create('Drawing rectangle requires admitted bounds');
  end;
  Result := NyxDrawingPath(NyxCanvasPoint(ABounds.Left, ABounds.Top))
    .LineTo(NyxCanvasPoint((ABounds.Left + ABounds.Width), ABounds.Top))
    .LineTo(NyxCanvasPoint((ABounds.Left + ABounds.Width), (ABounds.Top + ABounds.Height)))
    .LineTo(NyxCanvasPoint(ABounds.Left, (ABounds.Top + ABounds.Height))).Close;
end;

procedure TNyxDrawingCommand.RequireKind(AKind: TNyxDrawingCommandKind);
begin

  if FKind <> AKind then
  begin
    raise ENyxDrawing.Create('Drawing command does not contain the requested typed value');
  end;
end;

function TNyxDrawingCommand.GetPath: TNyxDrawingPath;
begin
  RequireKind(ndckPath);
  Result := FPath;
end;

function TNyxDrawingCommand.GetStyle: TNyxDrawingStyle;
begin
  RequireKind(ndckPath);
  Result := FStyle;
end;

function TNyxDrawingCommand.GetText: TNyxText;
begin
  RequireKind(ndckText);
  Result := FText;
end;

function TNyxDrawingCommand.GetPosition: TNyxCanvasPoint;
begin
  RequireKind(ndckText);
  Result := FPosition;
end;

function TNyxDrawingCommand.GetPaint: TNyxDrawingPaint;
begin
  RequireKind(ndckText);
  Result := FPaint;
end;

function TNyxDrawingCommand.GetSize: Double;
begin
  RequireKind(ndckText);
  Result := FSize;
end;

function TNyxDrawingCommand.GetAnchor: TNyxDrawingTextAnchor;
begin
  RequireKind(ndckText);
  Result := FAnchor;
end;

function TNyxDrawingCommand.GetBold: Boolean;
begin
  RequireKind(ndckText);
  Result := FBold;
end;

function TNyxDrawingCommand.ToData: TNyxDataValue;
begin
  case FKind of
    ndckPath: Result := NyxObject([NyxField('kind', NyxData(0)), NyxField('path', FPath.ToData),
      NyxField('style', FStyle.ToData)]);
    ndckText: Result := NyxObject([NyxField('kind', NyxData(1)), NyxField('text', NyxData(FText)),
      NyxField('position', PointData(FPosition)), NyxField('paint', FPaint.ToData),
      NyxField('size', NyxData(FSize)), NyxField('anchor', NyxData(Ord(FAnchor))),
      NyxField('bold', NyxData(FBold))]);
  else
    raise ENyxDrawing.Create('Construct a drawing command first');
  end;
end;

constructor TBuilder.Create(const ABox: TNyxCanvasBounds);
begin
  inherited Create;

  if not ABox.Defined or (ABox.Width <= 0) or (ABox.Height <= 0) then
  begin
    raise ENyxDrawing.Create('Drawing view box requires positive finite dimensions');
  end;
  FBox := ABox;
  { Version-one root syntax plus four bounded numeric coordinates. This exceeds
    its fixed ASCII punctuation/key cost and four <=32-byte number spellings. }
  FByteBound := DrawingRootByteBound;
end;

procedure TBuilder.AdmitPoint(const APoint: TNyxCanvasPoint);
begin
  NyxCanvasPoint(APoint.X, APoint.Y);

  if (APoint.X < FBox.Left) or (APoint.X > (FBox.Left + FBox.Width)) or
    (APoint.Y < FBox.Top) or (APoint.Y > (FBox.Top + FBox.Height)) then
  begin
    raise ENyxDrawing.Create('Drawing points/control points must belong to their declared view box');
  end;
end;

procedure TBuilder.Add(const ACommand: TNyxDrawingCommand; ASamplePoints, AByteBound: Integer);
begin

  if Length(FCommands) >= MaximumNyxDrawingCommands then
  begin
    raise ENyxDrawing.Create('Drawing command capacity exceeded');
  end;

  if FSamplePoints + ASamplePoints > MaximumNyxDrawingSamplePoints then
  begin
    raise ENyxDrawing.Create('Drawing sampled-point budget exceeded');
  end;
  SetLength(FCommands, Length(FCommands) + 1);
  FCommands[High(FCommands)] := ACommand;
  Inc(FSamplePoints, ASamplePoints);
  Inc(FByteBound, AByteBound);
end;

function TBuilder.Path(const APath: TNyxDrawingPath; const AStyle: TNyxDrawingStyle): INyxDrawingBuilder;
var
  LStep: TNyxDrawingStep;
  LCommand: TNyxDrawingCommand;
  LIndex: Integer;
  LPoint: Integer;
  LSamplePoints: Integer;
  LPointCount: Integer;
  LByteBound: Integer;
begin
  { Typed factories and the strict decoder are the private sequence's writers.
    Each current-box point is still admitted before this builder can append. }

  if APath.StepCount = 0 then
  begin
    raise ENyxDrawing.Create('Construct a drawing path first');
  end;
  LSamplePoints := 1;
  { Closed version-one command/style syntax and two finite typed paints fit
    256 ASCII bytes. NyxStateNumberText emits <=17 significant digits with a
    short exponent or bounded ordinary decimal: 32 bytes safely covers a Double.
    A point uses two such numbers and four punctuation/spacing bytes. Eight
    additional bytes cover every step's ordinal, brackets and comma spacing. }
  LByteBound := DrawingCommandByteBound;
  for LIndex := 0 to High(APath.FSteps) do
  begin
    LStep := APath.FSteps[LIndex];
    LPointCount := 1;

    if LStep.Kind = ndskCurve then
    begin
      LPointCount := 3;
    end
    else if LStep.Kind = ndskClose then
    begin
      LPointCount := 0;
    end;
    for LPoint := 0 to LPointCount - 1 do
    begin
      AdmitPoint(LStep.Point(LPoint));
    end;
    Inc(LByteBound, DrawingStepByteBound + LPointCount * DrawingPointByteBound);

    if LIndex > 0 then
    begin

      if LStep.Kind = ndskCurve then
      begin
        Inc(LSamplePoints, 32);
      end
      else
      begin
        Inc(LSamplePoints);
      end;
    end;
  end;
  LCommand := Default(TNyxDrawingCommand);
  LCommand.FKind := ndckPath;
  LCommand.FPath := APath;
  LCommand.FStyle := AStyle;
  Add(LCommand, LSamplePoints, LByteBound);
  Result := Self;
end;

function TBuilder.Text(const AText: TNyxText; const APosition: TNyxCanvasPoint;
  const APaint: TNyxDrawingPaint; ASize: Double; AAnchor: TNyxDrawingTextAnchor;
  ABold: Boolean): INyxDrawingBuilder;
var
  LIndex: Integer;
  LScalar: Integer;
  LScalars: Integer;
  LCommand: TNyxDrawingCommand;
begin
  AdmitPoint(APosition);
  LScalars := DrawingScalarCount(AText);

  if (LScalars > 4096) or IsNan(ASize) or IsInfinite(ASize) or
    (ASize < 1) or (ASize > 512) or
    (Ord(AAnchor) < Ord(Low(TNyxDrawingTextAnchor))) or
    (Ord(AAnchor) > Ord(High(TNyxDrawingTextAnchor))) then
  begin
    raise ENyxDrawing.Create('Drawing text requires bounded Unicode, font size and typed anchor');
  end;
  { This primitive is a single line. Explicitly refuse control characters
    instead of letting native TextOut truncate NUL or disagree with SVG. }
  LIndex := 1;
  while LIndex <= Length(AText) do
  begin
    NyxNextScalar(AText, LIndex, LScalar);

    if (LScalar < 32) or ((LScalar >= 127) and (LScalar <= 159)) then
    begin
      raise ENyxDrawing.Create('Drawing text requires a single printable line');
    end;
  end;
  LCommand := Default(TNyxDrawingCommand);
  LCommand.FKind := ndckText;
  LCommand.FText := AText;
  LCommand.FPosition := APosition;
  LCommand.FPaint := APaint;
  LCommand.FSize := ASize;
  LCommand.FAnchor := AAnchor;
  LCommand.FBold := ABold;
  { Twelve bytes per scalar also covers a JSON-escaped UTF-16 surrogate pair;
    the fixed allowance covers paints, position/font numbers and closed keys. }
  Add(LCommand, 0, DrawingCommandByteBound + LScalars * DrawingScalarByteBound);
  Result := Self;
end;

function TBuilder.Build: INyxDrawingPlan;
var
  LPlan: TPlan;
  {$ifopt C+}
  LBytes: Integer;
  {$endif}
begin
  LPlan := TPlan.Create;
  Result := LPlan;
  LPlan.FBox := FBox;
  LPlan.FCommands := Copy(FCommands, 0, Length(FCommands));

  { Safe small plans need no serialization. A conservative bound never rejects
    a larger valid plan: near the limit, the exact canonical boundary decides. }

  {$ifopt C+}
  { Checked qualification independently verifies the bound against whole wire
    output, including Unicode, small exponents and maximum membership. }
  LBytes := NyxUTF8ByteCount(LPlan.ToData.ToJSON);
  Assert(LBytes <= FByteBound, 'Drawing byte bound underestimated canonical output');
  {$else}

  if FByteBound > MaximumNyxDrawingBytes then
  begin
    LPlan.ToData;
  end;
  {$endif}
end;

function NewNyxDrawingPlan(const AViewBox: TNyxCanvasBounds): INyxDrawingBuilder;
begin
  Result := TBuilder.Create(AViewBox);
end;

function TPlan.GetViewBox: TNyxCanvasBounds;
begin
  Result := FBox;
end;

function CaptureNyxDrawingPlan(const APlan: INyxDrawingPlan): INyxDrawingPlan;
var
  LStorage: TPlan;
  LIdentity: INyxDrawingPlan;
begin

  if APlan = nil then
  begin
    raise ENyxDrawing.Create('Drawing capture requires a complete plan');
  end;

  if APlan is TPlan then
  begin
    { Aggregation may forward an object-instance query to an inner plan. The
      actual private class and its own interface must both match this input. }
    LStorage := APlan as TPlan;
    LIdentity := LStorage;

    if (LStorage.ClassType = TPlan) and (LIdentity = APlan) then
    begin
      Result := APlan;
      Exit;
    end;
  end;
  Result := NyxDrawingPlanFromData(APlan.ToData);
end;

function TPlan.GetCount: Integer;
begin
  Result := Length(FCommands);
end;

function TPlan.Command(AIndex: Integer): TNyxDataValue;
begin
  Result := TypedCommand(AIndex).ToData;
end;

function TPlan.TypedCommand(AIndex: Integer): TNyxDrawingCommand;
begin

  if (AIndex < 0) or (AIndex >= Length(FCommands)) then
  begin
    raise ENyxDrawing.Create('Drawing command index outside its plan');
  end;
  Result := FCommands[AIndex];
end;

function TPlan.ToData: TNyxDataValue;
var
  LCommands: array of TNyxDataValue;
  LIndex: Integer;
begin
  SetLength(LCommands, Length(FCommands));
  for LIndex := 0 to High(FCommands) do
  begin
    LCommands[LIndex] := FCommands[LIndex].ToData;
  end;
  Result := NyxObject([NyxField('version', NyxData(1)),
    NyxField('box', NyxArray([NyxData(FBox.Left), NyxData(FBox.Top),
      NyxData(FBox.Width), NyxData(FBox.Height)])), NyxField('commands', NyxArray(LCommands))]);

  if NyxUTF8ByteCount(Result.ToJSON) > MaximumNyxDrawingBytes then
  begin
    raise ENyxDrawing.Create('Drawing plan exceeds its persistence byte boundary');
  end;
end;

function NyxDrawingCommands(const APlan: INyxDrawingPlan): INyxDrawingCommands;
var
  LPlan: INyxDrawingPlan;
begin
  LPlan := CaptureNyxDrawingPlan(APlan);

  if not Supports(LPlan, INyxDrawingCommands, Result) then
  begin
    raise ENyxDrawing.Create('Admitted drawing has no typed command reader');
  end;
end;

function NyxDrawingPlanFromData(const AData: TNyxDataValue): INyxDrawingPlan;
var
  LBox: TNyxDataValue;
  LCommands: TNyxDataValue;
  LCommand: TNyxDataValue;
  LBuilder: INyxDrawingBuilder;
  LIndex: Integer;
  LAnchor: Integer;
begin
  Fields(AData, ['version', 'box', 'commands']);

  if AData.Field('version').AsInteger <> 1 then
  begin
    raise ENyxDrawing.Create('Unsupported drawing plan version');
  end;
  LBox := AData.Field('box');

  if (LBox.Kind <> ndArray) or (LBox.Count <> 4) then
  begin
    raise ENyxDrawing.Create('Drawing view box requires four numbers');
  end;
  LBuilder := NewNyxDrawingPlan(NyxCanvasBounds(LBox.Item(0).AsNumber, LBox.Item(1).AsNumber,
    LBox.Item(2).AsNumber, LBox.Item(3).AsNumber));
  LCommands := AData.Field('commands');

  if (LCommands.Kind <> ndArray) or (LCommands.Count > MaximumNyxDrawingCommands) then
  begin
    raise ENyxDrawing.Create('Drawing command capacity exceeded');
  end;
  for LIndex := 0 to LCommands.Count - 1 do
  begin
    LCommand := LCommands.Item(LIndex);
    case LCommand.Field('kind').AsInteger of
      0:
        begin
          Fields(LCommand, ['kind', 'path', 'style']);
          LBuilder.Path(TNyxDrawingPath.FromData(LCommand.Field('path')),
            TNyxDrawingStyle.FromData(LCommand.Field('style')));
        end;
      1:
        begin
          Fields(LCommand, ['kind', 'text', 'position', 'paint', 'size', 'anchor', 'bold']);
          LAnchor := LCommand.Field('anchor').AsInteger;

          if (LAnchor < Ord(Low(TNyxDrawingTextAnchor))) or
            (LAnchor > Ord(High(TNyxDrawingTextAnchor))) then
          begin
            raise ENyxDrawing.Create('Unknown drawing text anchor');
          end;
          LBuilder.Text(LCommand.Field('text').AsText, ReadPoint(LCommand.Field('position')),
            TNyxDrawingPaint.FromData(LCommand.Field('paint')), LCommand.Field('size').AsNumber,
            TNyxDrawingTextAnchor(LAnchor), LCommand.Field('bold').AsBoolean);
        end;
    else
      raise ENyxDrawing.Create('Unknown drawing command');
    end;
  end;
  Result := LBuilder.Build;
end;

function TNyxDrawingProviderRef.Defined: Boolean;
begin
  Result := FName <> '';
end;

function NyxDrawingProvider(const AName: TNyxText): TNyxDrawingProviderRef;
var
  LIndex: Integer;
  LScalar: Integer;
  LNonspace: Boolean;
begin

  if (DrawingScalarCount(AName) = 0) or (DrawingScalarCount(AName) > 128) then
  begin
    raise ENyxDrawing.Create('Drawing provider requires an exact bounded identity');
  end;
  LIndex := 1;
  LNonspace := False;
  while LIndex <= Length(AName) do
  begin

    if not NyxNextScalar(AName, LIndex, LScalar) or (LScalar < 32) or
      ((LScalar >= 127) and (LScalar <= 159)) then
    begin
      raise ENyxDrawing.Create('Drawing provider identity contains invalid text');
    end;
    LNonspace := LNonspace or not NyxScalarWhitespace(LScalar);
  end;

  if not LNonspace then
  begin
    raise ENyxDrawing.Create('Drawing provider identity requires a nonspace scalar');
  end;
  Result.FName := AName;
end;

function NyxDrawingPathPoints(const APath: TNyxDrawingPath): TNyxDrawingPoints;
var
  LStep: TNyxDrawingStep;
  LCurrent: TNyxCanvasPoint;
  LFirst: TNyxCanvasPoint;
  LSecond: TNyxCanvasPoint;
  LEnd: TNyxCanvasPoint;
  LStart: TNyxCanvasPoint;
  LIndex: Integer;
  LSample: Integer;
  LCount: Integer;
  LT: Double;
  LU: Double;

  procedure Add(const APoint: TNyxCanvasPoint);
  begin
    Result[LCount] := APoint;
    Inc(LCount);
  end;
begin
  Result := nil;
  if APath.StepCount = 0 then
  begin
    raise ENyxDrawing.Create('Construct a drawing path first');
  end;
  { Size once from admitted steps; avoid quadratic array copying for curves. }
  LCount := 1;
  for LIndex := 1 to APath.StepCount - 1 do
  begin

    if APath.Step(LIndex).Kind = ndskCurve then
    begin
      Inc(LCount, 32);
    end
    else
    begin
      Inc(LCount);
    end;
  end;
  SetLength(Result, LCount);
  LCount := 0;
  LStart := APath.Step(0).Point(0);
  LCurrent := LStart;
  Add(LCurrent);
  for LIndex := 1 to APath.StepCount - 1 do
  begin
    LStep := APath.Step(LIndex);
    case LStep.Kind of
      ndskLine:
        begin
          LCurrent := LStep.Point(0);
          Add(LCurrent);
        end;
      ndskCurve:
        begin
          LFirst := LStep.Point(0);
          LSecond := LStep.Point(1);
          LEnd := LStep.Point(2);
          for LSample := 1 to 32 do
          begin
            LT := LSample / 32;
            LU := 1 - LT;
            Add(NyxCanvasPoint(LU * LU * LU * LCurrent.X +
              3 * LU * LU * LT * LFirst.X + 3 * LU * LT * LT * LSecond.X + LT * LT * LT * LEnd.X,
              LU * LU * LU * LCurrent.Y + 3 * LU * LU * LT * LFirst.Y +
              3 * LU * LT * LT * LSecond.Y + LT * LT * LT * LEnd.Y));
          end;
          LCurrent := LEnd;
        end;
      ndskClose: Add(LStart);
    ndskMove:
      begin
        raise ENyxDrawing.Create('A drawing path may begin only once');
      end;
    end;
  end;
end;

function ResolveNyxDrawingPaint(const APaint: TNyxDrawingPaint; ATheme: TNyxTheme): TNyxRGBColor;
var
  LText: TNyxText;
begin
  Result := NyxNoColor;

  if APaint.Kind = ndpkRGB then
  begin
    Exit(APaint.Color);
  end;

  if APaint.Kind = ndpkNone then
  begin
    Exit;
  end;

  if ATheme = nil then
  begin
    raise ENyxDrawing.Create('Themed drawing requires its owning palette');
  end;
  case APaint.Role of
    ntcBackground: LText := ATheme.Background;
    ntcSurface: LText := ATheme.Surface;
    ntcText: LText := ATheme.Text;
    ntcMuted: LText := ATheme.Muted;
    ntcBorder: LText := ATheme.Border;
    ntcAccent: LText := ATheme.Accent;
    ntcAccentText: LText := ATheme.AccentText;
    ntcDanger: LText := ATheme.Danger;
    ntcDangerText: LText := ATheme.DangerText;
    ntcSuccess: LText := ATheme.Success;
    ntcSuccessText: LText := ATheme.SuccessText;
    ntcWarning: LText := ATheme.Warning;
    ntcWarningText: LText := ATheme.WarningText;
  end;
  Result := TNyxRGBColor.FromText(LText);
end;

function NyxDrawingPalette(ATheme: TNyxTheme): TNyxThemeTokens;
var
  LRole: TNyxThemeColor;
begin
  Result := NyxThemeTokens;
  for LRole := Low(TNyxThemeColor) to High(TNyxThemeColor) do
  begin
    Result := Result.Color(LRole, ResolveNyxDrawingPaint(NyxThemePaint(LRole), ATheme));
  end;
  Result := Result.Metric(ntmRadius, ATheme.Radius).Metric(ntmControlRadius, ATheme.ControlRadius)
    .Metric(ntmFontSize, ATheme.FontSize);
end;

end.
