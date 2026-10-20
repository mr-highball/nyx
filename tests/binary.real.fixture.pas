{ Native compiler oracle: literal values and their inferred generic types are
  recorded from actual i386 Extended bytes. Runtime casts observe nearest-even
  Single/Double narrowing with the previous FPU mode/mask restored on exit.
  No Nyx rounding/value implementation is used by this witness. }
unit binary.real.fixture;
{$mode delphi}{$H+}{$codepage utf8}{$minfpconstprec 64}
{$modeswitch implicitfunctionspecialization+}
interface

function NativeBinaryRealObservations: AnsiString;
function NativeBinaryRealCalls: LongInt;

implementation

uses SysUtils, TypInfo, Math;

{$if not defined(CPUI386)}
  {$fatal This byte oracle requires the pinned i386 native ABI}
{$endif}
{$if SizeOf(Extended) <> 10}
  {$fatal This byte oracle requires a ten-byte Extended}
{$endif}

type
  { Exact little-endian bytes of the native witness's checked ten-byte x87
    value. This is a test ABI observation, never a portable representation. }
  TExtendedBytes = array[0..9] of Byte;

var GText: AnsiString; GCalls: LongInt;

function Describe(const AValue: Extended): AnsiString;
var LBytes: TExtendedBytes; LAt, LBit, LExponent: Integer; LBits, LSign: AnsiString;
begin
  Move(AValue,LBytes,SizeOf(LBytes));
  LExponent := (Integer(LBytes[9] and $7f) shl 8)+LBytes[8];
  LSign := '0';

  if LBytes[9] and $80 <> 0 then LSign := '1';

  if LExponent = $7fff then Exit(LSign+'|overflow|0');
  LBits := '';
  for LAt := 7 downto 0 do
    for LBit := 7 downto 0 do
    begin

      if LBytes[LAt] and (1 shl LBit) <> 0 then LBits := LBits+'1'
      else LBits := LBits+'0';
    end;
  while (Length(LBits) > 1) and (LBits[1] = '0') do Delete(LBits,1,1);

  if LBits = '0' then Exit(LSign+'|0|0');

  if LExponent = 0 then LExponent := 1;
  Dec(LExponent,16383+63);
  while LBits[Length(LBits)] = '0' do
  begin
    SetLength(LBits,Length(LBits)-1); Inc(LExponent);
  end;
  Result := LSign+'|'+LBits+'|'+IntToStr(LExponent);
end;

procedure EmitValue(const ALabel, ASpelling, AType: AnsiString; const AValue: Extended);
var LSingle: Single; LDouble: Double;
begin
  Inc(GCalls); LSingle := Single(AValue); LDouble := Double(AValue);
  GText := GText+ALabel+'|'+ASpelling+'|'+AType+'|'+
    Describe(AValue)+'|'+Describe(Extended(LSingle))+'|'+Describe(Extended(LDouble))+#10;
end;

procedure Emit<T>(const ALabel, ASpelling: AnsiString; const AValue: T);
begin
  EmitValue(ALabel,ASpelling,AnsiString(PTypeInfo(TypeInfo(T))^.Name),Extended(AValue));
end;

function NativeBinaryRealObservations: AnsiString;
var LMode: TFPURoundingMode; LMask: TFPUExceptionMask;
begin
  GText := ''; GCalls := 0; LMode := GetRoundMode; LMask := GetExceptionMask;
  try
    SetRoundMode(rmNearest);
    SetExceptionMask([exInvalidOp,exDenormalized,exZeroDivide,exOverflow,exUnderflow,exPrecision]);
    Emit('zero','0.0',0.0);
    Emit('negative-zero','-0.0',-0.0);
    Emit('exact','1.25',1.25);
    Emit('recurring','0.1',0.1);
    Emit('negative','-0.1',-0.1);
    Emit('near-one','1.00000000000000000000001',1.00000000000000000000001);
    Emit('half-single','1.000000059604644775390625',1.000000059604644775390625);
    Emit('above-half-single','1.00000005960464477539062500000000000000000000000001',
      1.00000005960464477539062500000000000000000000000001);
    Emit('half-double','1.00000000000000011102230246251565404236316680908203125',
      1.00000000000000011102230246251565404236316680908203125);
    Emit('integer24','16777217.0',16777217.0);
    Emit('integer53','9007199254740993.0',9007199254740993.0);
    Emit('large-double','1.0e300',1.0e300);
    Emit('small-double','1.0e-300',1.0e-300);
    Emit('double-minimum','2.2250738585072014e-308',2.2250738585072014e-308);
    Emit('double-tiny','4.9406564584124654e-324',4.9406564584124654e-324);
    Emit('double-half-tiny','2.4703282292062327e-324',2.4703282292062327e-324);
    Emit('single-tiny','1.40129846432481707092372958328991613128026194187651577175706828388979108268586060148663818836212158203125e-45',
      1.40129846432481707092372958328991613128026194187651577175706828388979108268586060148663818836212158203125e-45);
    Emit('single-half-tiny','7.00649232162408535461864791644958065640130970938257885878534141944895541342930300743319094181060791015625e-46',
      7.00649232162408535461864791644958065640130970938257885878534141944895541342930300743319094181060791015625e-46);
    Emit('single-maximum','340282346638528859811704183484516925440.0',340282346638528859811704183484516925440.0);
    Emit('single-overflow-half','340282356779733661637539395458142568448.0',340282356779733661637539395458142568448.0);
    Emit('extended-large','1.0e4000',1.0e4000);
    Emit('extended-small','1.0e-4000',1.0e-4000);
    Emit('extended-subnormal','1.0e-4932',1.0e-4932);
    Result := GText;
  finally
    SetRoundMode(LMode); SetExceptionMask(LMask);
  end;
end;

function NativeBinaryRealCalls: LongInt;
begin
  Result := GCalls;
end;

end.
