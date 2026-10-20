{ The compiler independently types named constants and explicit real casts.
  Repeated local declaration/case includes retain five authored precision sites.
  Typed constants remain separate storage; their runtime proof flag is not an
  instruction to the compiler or a supplied generic type argument. }
unit real.declarations.fixture;
{$mode delphi}{$H+}{$codepage utf8}{$J-}
{$modeswitch implicitfunctionspecialization+}
interface

function NativeRealDeclarations: AnsiString;
function NativeRealDeclarationCalls: LongInt;

implementation

uses SysUtils, TypInfo;

{$if not defined(CPUI386)}
  {$fatal This declaration oracle requires the pinned i386 ABI}
{$endif}
{$if SizeOf(Extended) <> 10}
  {$fatal This declaration oracle requires ten-byte Extended storage}
{$endif}

type
  { Exact native bytes after lossless widening of the actual compiler result.
    They establish only this guarded witness's ABI, never a portable layout. }
  TDeclarationBytes = array[0..9] of Byte;
  { A distinct real alias tests that explicit casts preserve authored nominal
    identity rather than replacing every result with its primitive leaf. }
  TAmount = type Double;

var GText, GProfile: AnsiString; GCalls: LongInt; F64: Double;

function Describe(const AValue: Extended): AnsiString;
var LBytes: TDeclarationBytes; LAt, LBit, LExponent: Integer; LBits, LSign: AnsiString;
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

{$minfpconstprec 32}{$excessprecision off}{$fputype x87}
procedure Off32;
{$I real.declarations.constants.inc}
begin
  GProfile := '32-off-x87';
  {$I real.declarations.cases.inc}
end;

{$minfpconstprec 64}{$excessprecision off}{$fputype x87}
procedure Off64;
{$I real.declarations.constants.inc}
begin
  GProfile := '64-off-x87';
  {$I real.declarations.cases.inc}
end;

{$minfpconstprec 32}{$excessprecision on}{$fputype x87}
procedure On32;
{$I real.declarations.constants.inc}
begin
  GProfile := '32-on-x87';
  {$I real.declarations.cases.inc}
end;

{$minfpconstprec 64}{$excessprecision on}{$fputype x87}
procedure On64;
{$I real.declarations.constants.inc}
begin
  GProfile := '64-on-x87';
  {$I real.declarations.cases.inc}
end;

{$minfpconstprec 32}{$excessprecision off}{$fputype sse2}
procedure SSE32;
{$I real.declarations.constants.inc}
begin
  GProfile := '32-off-sse2';
  {$I real.declarations.cases.inc}
end;

function NativeRealDeclarations: AnsiString;
begin
  GText := ''; GCalls := 0; F64 := 2.5;
  Off32; Off64; On32; On64; SSE32;
  Result := GText;
end;

function NativeRealDeclarationCalls: LongInt;
begin
  Result := GCalls;
end;

end.
