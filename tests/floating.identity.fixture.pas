{ Whole native type witness. RTTI distinguishes representation from declared
  identity; Real and Double remain independently overloadable in this compiler. }
unit floating.identity.fixture;
{$mode delphi}{$H+}{$codepage utf8}
{$modeswitch implicitfunctionspecialization+}
interface

type
  { Transparent name for the same primitive Double definition. }
  TDoubleAlias = Double;
  { Independent declared type, sharing Double's storage representation. }
  TDoubleDistinct = type Double;

function SameFloating<T>(var ALeft, ARight: T): T;
function NativeFloatingObservations: AnsiString;
function NativeFloatingCalls: LongInt;
{ Exported generic bodies cannot capture an implementation-only variable. }
procedure RecordFloatingCall;

implementation

uses SysUtils, TypInfo;

var
  GCalls: LongInt;
  GExtended: Extended;

procedure RecordFloatingCall;
begin Inc(GCalls); end;

function SameFloating<T>(var ALeft, ARight: T): T;
begin
  RecordFloatingCall;
  Result := ALeft;
end;

function Describe<T>(const AValue: T): AnsiString;
var LInfo: PTypeInfo;
begin
  Inc(GCalls);
  LInfo := PTypeInfo(TypeInfo(T));

  if LInfo^.Kind <> tkFloat then raise Exception.Create('Expected native floating RTTI');
  Result := AnsiString(LInfo^.Name)+'/'+GetEnumName(TypeInfo(TFloatType),
    Ord(GetTypeData(LInfo)^.FloatType))+'/'+IntToStr(SizeOf(T));
end;

function Route(var AValue: Double): LongInt; overload;
begin Result := 1; end;

function Route(var AValue: Real): LongInt; overload;
begin Result := 2; end;

function Route(var AValue: TDoubleDistinct): LongInt; overload;
begin Result := 3; end;

function NativeFloatingObservations: AnsiString;
var
  LSingle: Single;
  LDouble: Double;
  LCurrency: Currency;
  LComp: Comp;
  LAlias: TDoubleAlias;
  LDistinct: TDoubleDistinct;
  LReal: Real;
begin
  GCalls := 0;
  LSingle := 1; LDouble := 2; GExtended := 3; LCurrency := 4;
  LComp := 5; LAlias := 6; LDistinct := 7; LReal := 8;
  Result := Describe(LSingle)+#10+Describe(LDouble)+#10+Describe(GExtended)+#10+
    Describe(LCurrency)+#10+Describe(LComp)+#10+Describe(LReal)+#10+
    Describe(LDistinct)+#10+Describe(SameFloating(LDouble,LAlias))+#10+
    'routes='+IntToStr(Route(LDouble))+IntToStr(Route(LReal))+IntToStr(Route(LDistinct))+#10;
end;

function NativeFloatingCalls: LongInt;
begin Result := GCalls; end;

end.
