{ Whole handwritten arithmetic witness. Generic RTTI reports the compiler's
  actual expression types; queries later inspect source without executing it. }
unit numeric.expressions.fixture;
{$mode delphi}{$H+}{$codepage utf8}
{$modeswitch implicitfunctionspecialization+}
interface

function ObserveNumeric<T>(const AValue: T): T;
{ This body deliberately changes its input value. The resolver may consume its
  declared type contract, never an assumption that a generic preserves values. }
function ResetNumeric<T>(const AValue: T): T;
{ The public bridge keeps generic templates independent of private storage
  and implementation-only TypInfo names. }
procedure RecordNumericType(AInfo: Pointer);
procedure RunNumericWitness;
function NumericObservations: AnsiString;
function NumericInvocations: LongInt;
function NumericBarrierValue: LongInt;
function NumericValue: LongInt;
function NumericSourceInvocations: LongInt;

implementation

uses TypInfo;

var
  GLabel, GObservations: AnsiString;
  GInvocations: LongInt;
  GBarrier: LongInt;
  GSourceInvocations: LongInt;

procedure RecordNumericType(AInfo: Pointer);
begin
  Inc(GInvocations);
  GObservations := GObservations+GLabel+'='+AnsiString(PTypeInfo(AInfo)^.Name)+#10;
end;

function ObserveNumeric<T>(const AValue: T): T;
begin
  RecordNumericType(TypeInfo(T));
  Result := AValue;
end;

function NumericObservations: AnsiString;
begin
  Result := GObservations;
end;

function ResetNumeric<T>(const AValue: T): T;
begin
  Result := Default(T);
end;

function NumericBarrierValue: LongInt;
begin
  Result := GBarrier;
end;

function NumericValue: LongInt;
begin
  Inc(GSourceInvocations);
  Result := 2;
end;

function NumericSourceInvocations: LongInt;
begin
  Result := GSourceInvocations;
end;

function NumericInvocations: LongInt;
begin
  Result := GInvocations;
end;

procedure RunNumericWitness;
var
  S8: ShortInt;
  U8: Byte;
  S16: SmallInt;
  U16: Word;
  S32: LongInt;
  U32: LongWord;
  S64: Int64;
  U64: QWord;
  NI: NativeInt;
  NU: NativeUInt;
  F32: Single;
  F64: Double;
begin
  S8 := 1; U8 := 1; S16 := 1; U16 := 1; S32 := 1; U32 := 1;
  S64 := 1; U64 := 1; NI := 1; NU := 1; F32 := 1.5; F64 := 1.5;
  GLabel := 'constant-add'; ObserveNumeric(1+2);
  GLabel := 'constant-byte-boundary'; ObserveNumeric(127+1);
  GLabel := 'constant-word-boundary'; ObserveNumeric(32767+1);
  GLabel := 'constant-long-boundary'; ObserveNumeric(2147483647+1);
  GLabel := 'constant-multiply'; ObserveNumeric(6*7);
  GLabel := 'constant-div'; ObserveNumeric(84 div 2);
  GLabel := 'constant-mod'; ObserveNumeric(85 mod 43);
  GLabel := 'constant-shift'; ObserveNumeric(1 shl 6);
  GLabel := 'constant-bitwise'; ObserveNumeric($ff and $0f);
  GLabel := 'constant-not'; ObserveNumeric(not 1);
  GLabel := 'constant-real'; ObserveNumeric(1.5+2.5);
  GLabel := 'short-add'; ObserveNumeric(S8+S8);
  GLabel := 'byte-add'; ObserveNumeric(U8+U8);
  GLabel := 'small-add'; ObserveNumeric(S16+S16);
  GLabel := 'word-add'; ObserveNumeric(U16+U16);
  GLabel := 'small-byte'; ObserveNumeric(S8+U8);
  GLabel := 'long-add'; ObserveNumeric(S32+S32);
  GLabel := 'unsigned-add'; ObserveNumeric(U32+U32);
  GLabel := 'mixed-long'; ObserveNumeric(S32+U32);
  GLabel := 'int64-long'; ObserveNumeric(S64+S32);
  GLabel := 'int64-unsigned'; ObserveNumeric(S64+U64);
  GLabel := 'qword-long'; ObserveNumeric(U64+S32);
  GLabel := 'short-negative'; ObserveNumeric(-S8);
  GLabel := 'unsigned-negative'; ObserveNumeric(-U32);
  GLabel := 'byte-not'; ObserveNumeric(not U8);
  GLabel := 'byte-shift'; ObserveNumeric(U8 shl U8);
  GLabel := 'native-add'; ObserveNumeric(NI+NI);
  GLabel := 'native-unsigned'; ObserveNumeric(NU+NU);
  GLabel := 'single-add'; ObserveNumeric(F32+F32);
  GLabel := 'single-double'; ObserveNumeric(F32+F64);
  GLabel := 'integer-divide'; ObserveNumeric(S32/S32);
  GLabel := 'integer-multiply'; ObserveNumeric(S32*S32);
  GLabel := 'word-bitwise'; ObserveNumeric(U16 or U8);
  GLabel := 'runtime-div'; ObserveNumeric(U16 div U8);
  GLabel := 'runtime-mod'; ObserveNumeric(S64 mod S32);
  GLabel := 'runtime-compare'; ObserveNumeric(S32 < U32);
  GLabel := 'function-barrier'; GBarrier := ObserveNumeric(ResetNumeric(1+2)+1);
  GLabel := 'returned-byte-add'; ObserveNumeric(ObserveNumeric(127+1)+1);
  GLabel := 'returned-small-add'; ObserveNumeric(ObserveNumeric((127+1)*2)+1);
  GLabel := 'routine-less'; ObserveNumeric(NumericValue < U32);
  GLabel := 'routine-less-equal'; ObserveNumeric(NumericValue <= U32);
  GLabel := 'qualified-routine-less'; ObserveNumeric(numeric.expressions.fixture.NumericValue < U32);
end;

end.
