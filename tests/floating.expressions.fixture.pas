{ Whole native arithmetic witness. Local precision requests are delayed until
  the next scanner call; instruction selection is immediate. Labels tie each
  independently compiled expression to the semantic test's original window. }
unit floating.expressions.fixture;
{$mode delphi}{$H+}{$codepage utf8}{$R+}
{$modeswitch implicitfunctionspecialization+}
interface

type
  { A distinct Double tests which operand's nominal type the compiler retains. }
  TNamedFloat = type Double;

var
  F32: Single;
  F64: Double;
  F80: Extended;
  Money: Currency;
  Integral: Comp;
  RealValue: Real;
  NamedValue: TNamedFloat;
  I32: LongInt;

function KeepFloat<T>(const AValue: T): T;
{ Exported generic code may call this helper without capturing a private var. }
procedure RecordKeep;
function NativeFloatingExpressions: AnsiString;
function NativeFloatingExpressionCalls: LongInt;
function NativeFloatingKeepCalls: LongInt;

implementation

uses TypInfo;

var GText: AnsiString; GCalls, GKeepCalls: LongInt;

procedure RecordKeep;
begin Inc(GKeepCalls); end;

function KeepFloat<T>(const AValue: T): T;
begin
  RecordKeep;
  Result := AValue;
end;

function Observe<T>(const AValue: T): AnsiString;
begin
  Inc(GCalls);
  Result := AnsiString(PTypeInfo(TypeInfo(T))^.Name);
end;

procedure Emit(const ALabel, AName: AnsiString);
begin GText := GText+ALabel+'='+AName+#10; end;

{$fputype X87}{$excessprecision off}
procedure X87Off;
begin
  Emit('off-single', Observe(F32 + F32));
  Emit('off-mixed', Observe(F32 + F64));
  Emit('off-reversed', Observe(F64 + F32));
  Emit('off-real-left', Observe(RealValue + F64));
  Emit('off-real-right', Observe(F64 + RealValue));
  Emit('off-named-left', Observe(NamedValue + F64));
  Emit('off-float-int', Observe(F32 + I32));
  Emit('off-int-float', Observe(I32 + F32));
  Emit('off-comp-pair', Observe(Integral + Integral));
  Emit('off-comp-int', Observe(Integral + I32));
  Emit('off-currency-mixed', Observe(Money + F64));
  Emit('off-currency-divide', Observe(Money / Integral));
  Emit('off-integer-divide', Observe(I32 / I32));
  Emit('off-negative', Observe(-F32));
  Emit('off-positive', Observe(+F32));
  Emit('off-compare', Observe(F32 < F64));
  Emit('off-mixed-compare', Observe(I32 < F64));
  Emit('off-currency-compare', Observe(Money > Integral));
  Emit('off-chain', Observe(F32 + F32 * F64));
  Emit('off-call', Observe(KeepFloat(F32 + F32) + F32));
end;

{$fputype X87}{$excessprecision on}
procedure X87On;
begin
  Emit('on-single', Observe(F32 + F32));
  Emit('on-mixed', Observe(F32 + F64));
  Emit('on-float-int', Observe(F32 + I32));
  Emit('on-real', Observe(RealValue + F64));
  Emit('on-currency', Observe(Money * F64));
  Emit('on-comp-pair', Observe(Integral + Integral));
  Emit('on-comp-int', Observe(Integral + I32));
  Emit('on-negative', Observe(-F32));
  Emit('on-positive', Observe(+F32));
  Emit('on-integer-divide', Observe(I32 / I32));
end;

{$fputype SSE2}{$excessprecision off}
procedure SSE2Off;
begin
  Emit('sse-single-divide', Observe(F32 / F32));
  Emit('sse-integer-divide', Observe(I32 / I32));
  Emit('sse-float-int', Observe(F32 + I32));
  Emit('sse-real-negative', Observe(-RealValue));
end;

{$fputype SSE2}{$excessprecision on}
procedure SSE2On;
begin
  Emit('sse-on-single', Observe(F32 + F32));
  Emit('sse-on-integer', Observe(I32 / I32));
  Emit('sse-on-negative', Observe(-F32));
  Emit('sse-on-positive', Observe(+F32));
end;

{$fputype X87}{$excessprecision off}
procedure Timing;
begin
  Emit('precision-before-right', Observe(F32 + {$excessprecision on} F32));
  {$excessprecision off}
  Emit('precision-after-right', Observe(F32 + F32 {$excessprecision on}));
  {$excessprecision off}
  Emit('precision-unary-tail', Observe(-F32 {$excessprecision on}));
  {$excessprecision off}
  Emit('precision-grouped', Observe((F32 + F32) + {$excessprecision on} F32));
  {$fputype X87}{$excessprecision off}
  Emit('fpu-before-right', Observe(I32 / {$fputype SSE2} I32));
  {$fputype X87}
  Emit('fpu-after-right', Observe(I32 / I32 {$fputype SSE2}));
  {$fputype X87}
  Emit('fpu-whole-expression', Observe(I32 / I32 + {$fputype SSE2} F32));
  {$fputype X87}
  Emit('fpu-grouped-expression', Observe((I32 / I32) + {$fputype SSE2} F32));
  {$excessprecision off}
  Emit('precision-ifopt-flush', Observe(F32 + F32 {$excessprecision on}{$ifopt R+}{$endif}));
  {$excessprecision off}{$push}{$excessprecision on}
  Emit('precision-pop-tail', Observe(F32 + F32 {$pop}));
  Emit('precision-after-pop', Observe(F32 + F32));
  {$macro on}{$define FLOATPAIR:=F32 + F32}
  Emit('precision-macro-tail', Observe(FLOATPAIR {$excessprecision on}));
end;

function NativeFloatingExpressions: AnsiString;
begin
  GText := ''; GCalls := 0; GKeepCalls := 0;
  F32 := 1; F64 := 2; F80 := 3; Money := 4; Integral := 5;
  RealValue := 6; NamedValue := 7; I32 := 8;
  X87Off; X87On; SSE2Off; SSE2On; Timing;
  Result := GText;
end;

function NativeFloatingExpressionCalls: LongInt;
begin Result := GCalls; end;

function NativeFloatingKeepCalls: LongInt;
begin Result := GKeepCalls; end;

end.
