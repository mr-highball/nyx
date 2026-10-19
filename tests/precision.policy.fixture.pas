{ Whole handwritten compiler witness for excess-precision policy lifetime.
  RTTI observes real arithmetic independently of the source-context analyzer. }
unit precision.policy.fixture;
{$mode delphi}{$H+}{$codepage utf8}
{$modeswitch implicitfunctionspecialization+}
interface

function NativePrecisionObservations: AnsiString;
function NativePrecisionInvocations: LongInt;

implementation

uses TypInfo;

var
  GObservations, GLabel: AnsiString;
  GInvocations: LongInt;

function ObservePrecision<T>(const AValue: T): T;
begin
  Inc(GInvocations);
  GObservations := GObservations+GLabel+'='+AnsiString(PTypeInfo(TypeInfo(T))^.Name)+#10;
  Result := AValue;
end;

{$excessprecision off}
procedure ObserveOff;
var
  LSingle: Single;
  LDouble: Double;
begin
  LSingle := 1.5;
  LDouble := 2.5;
  GLabel := 'off-single'; ObservePrecision(LSingle+LSingle);
  GLabel := 'off-mixed'; ObservePrecision(LSingle+LDouble);
end;

{$push}{$excessprecision on}
procedure ObserveOn;
var LSingle: Single;
begin
  LSingle := 1.5;
  GLabel := 'on-single'; ObservePrecision(LSingle+LSingle);
end;
{$pop}

procedure ObserveRestored;
var LSingle: Single;
begin
  LSingle := 1.5;
  GLabel := 'restored-single'; ObservePrecision(LSingle+LSingle);
end;

function NativePrecisionObservations: AnsiString;
begin
  ObserveOff;
  ObserveOn;
  ObserveRestored;
  Result := GObservations;
end;

function NativePrecisionInvocations: LongInt;
begin
  Result := GInvocations;
end;

end.
