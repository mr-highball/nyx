{ Independent whole native expressions observe unary constants and implicit
  integer/real conversions. Runtime variables retain runtime behavior; the
  witness's flag describes the authored case, never the compiler's inferred
  generic type or the value bytes emitted from its actual result. }
unit real.coercion.fixture;
{$mode delphi}{$H+}{$codepage utf8}
{$modeswitch implicitfunctionspecialization+}
interface

function NativeRealCoercions: AnsiString;
function NativeRealCoercionCalls: LongInt;

implementation

uses SysUtils, TypInfo;

{$if not defined(CPUI386)}
  {$fatal This conversion oracle requires the pinned i386 ABI}
{$endif}
{$if (SizeOf(Extended) <> 10) or (SizeOf(LongInt) <> 4) or (SizeOf(Int64) <> 8) or (SizeOf(QWord) <> 8)}
  {$fatal This conversion oracle requires its exact observed primitive storage}
{$endif}

type
  { Exact storage of a compiler-produced value after a checked lossless widening
    to the witness's native Extended. No portable ABI is inferred from it. }
  TCoercionBytes = array[0..9] of Byte;

var GText, GProfile: AnsiString; GCalls: LongInt;
  F32: Single; F64: Double; F80: Extended; I32: LongInt; I64: Int64; U64: QWord;

function Describe(const AValue: Extended): AnsiString;
var LBytes: TCoercionBytes; LAt, LBit, LExponent: Integer; LBits, LSign: AnsiString;
begin
  Move(AValue,LBytes,SizeOf(LBytes));
  LExponent := (Integer(LBytes[9] and $7f) shl 8)+LBytes[8]; LSign := '0';

  if LBytes[9] and $80 <> 0 then LSign := '1';

  if LExponent = $7fff then Exit(LSign+'|nonfinite|0');
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
  begin SetLength(LBits,Length(LBits)-1); Inc(LExponent); end;
  Result := LSign+'|'+LBits+'|'+IntToStr(LExponent);
end;

procedure EmitValue(const ALabel, AType: AnsiString; AConstant: Boolean; const AValue: Extended);
begin
  Inc(GCalls);
  GText := GText+GProfile+'|'+ALabel+'|'+IntToStr(Ord(AConstant))+'|'+AType+'|'+Describe(AValue)+#10;
end;

procedure Emit<T>(const ALabel: AnsiString; AConstant: Boolean; const AValue: T);
begin
  EmitValue(ALabel,AnsiString(PTypeInfo(TypeInfo(T))^.Name),AConstant,Extended(AValue));
end;

procedure EmitRelation<T>(const ALabel: AnsiString; AConstant: Boolean; const AValue: T);
begin
  Inc(GCalls);
  GText := GText+GProfile+'|'+ALabel+'|'+IntToStr(Ord(AConstant))+'|'+
    AnsiString(PTypeInfo(TypeInfo(T))^.Name)+'|'+IntToStr(Ord(Boolean(AValue)))+#10;
end;

{$minfpconstprec 32}{$excessprecision off}{$fputype x87}
procedure Off32;
begin
  GProfile := '32-off-x87';
  {$I real.coercion.cases.inc}
end;

{$minfpconstprec 64}{$excessprecision off}{$fputype x87}
procedure Off64;
begin
  GProfile := '64-off-x87';
  {$I real.coercion.cases.inc}
end;

{$minfpconstprec 32}{$excessprecision on}{$fputype x87}
procedure On32;
begin
  GProfile := '32-on-x87';
  {$I real.coercion.cases.inc}
end;

{$minfpconstprec 64}{$excessprecision on}{$fputype x87}
procedure On64;
begin
  GProfile := '64-on-x87';
  {$I real.coercion.cases.inc}
end;

{$minfpconstprec 32}{$excessprecision off}{$fputype sse2}
procedure SSE32;
begin
  GProfile := '32-off-sse2';
  {$I real.coercion.cases.inc}
end;

function NativeRealCoercions: AnsiString;
begin
  GText := ''; GCalls := 0;
  F32 := 2.5; F64 := 2.5; F80 := 2.5; I32 := 3; I64 := 3; U64 := 3;
  Off32; Off64; On32; On64; SSE32;
  Result := GText;
end;

function NativeRealCoercionCalls: LongInt;
begin
  Result := GCalls;
end;

end.
