{ Independent native compiler evidence for complete real expressions. The
  compiler chooses generic types and folds every expression in the shared case
  include. No Nyx arithmetic, supplied generic type argument or runtime variable
  replaces those expressions. Byte decoding is confined to the guarded ABI. }
unit real.operations.fixture;
{$mode delphi}{$H+}{$codepage utf8}
{$modeswitch implicitfunctionspecialization+}
interface

function NativeRealOperationObservations: AnsiString;
function NativeRealOperationCalls: LongInt;

implementation

uses SysUtils, TypInfo;

{$if not defined(CPUI386)}
  {$fatal This expression oracle requires the pinned i386 native ABI}
{$endif}
{$if SizeOf(Extended) <> 10}
  {$fatal This expression oracle requires a ten-byte Extended}
{$endif}

type
  { The tested native Extended storage, read only after the compiler has
    produced the independently typed expression passed to the generic helper. }
  TOperationBytes = array[0..9] of Byte;

var GText, GProfile: AnsiString; GCalls: LongInt;

function Describe(const AValue: Extended): AnsiString;
var LBytes: TOperationBytes; LAt, LBit, LExponent: Integer; LBits, LSign: AnsiString;
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

procedure EmitValue(const ALabel, ALeft, ARight, AType: AnsiString;
  AOperation: Integer; const AValue: Extended);
begin
  Inc(GCalls);
  GText := GText+GProfile+'|'+ALabel+'|'+IntToStr(AOperation)+'|'+ALeft+'|'+ARight+'|'+
    AType+'|'+Describe(AValue)+#10;
end;

procedure Emit<T>(const ALabel, ALeft, ARight: AnsiString;
  AOperation: Integer; const AValue: T);
begin
  EmitValue(ALabel,ALeft,ARight,AnsiString(PTypeInfo(TypeInfo(T))^.Name),AOperation,Extended(AValue));
end;

procedure EmitRelation<T>(const ALabel, ALeft, ARight: AnsiString;
  AOperation: Integer; const AValue: T);
begin
  Inc(GCalls);
  GText := GText+GProfile+'|'+ALabel+'|'+IntToStr(AOperation)+'|'+ALeft+'|'+ARight+'|'+
    AnsiString(PTypeInfo(TypeInfo(T))^.Name)+'|'+IntToStr(Ord(Boolean(AValue)))+#10;
end;

{$minfpconstprec 32}{$excessprecision off}{$fputype x87}
procedure Off32;
begin
  GProfile := '32-off-x87';
  {$I real.operations.cases.inc}
end;

{$minfpconstprec 64}{$excessprecision off}{$fputype x87}
procedure Off64;
begin
  GProfile := '64-off-x87';
  {$I real.operations.cases.inc}
end;

{$minfpconstprec 32}{$excessprecision on}{$fputype x87}
procedure On32;
begin
  GProfile := '32-on-x87';
  {$I real.operations.cases.inc}
end;

{$minfpconstprec 64}{$excessprecision on}{$fputype x87}
procedure On64;
begin
  GProfile := '64-on-x87';
  {$I real.operations.cases.inc}
end;

{$minfpconstprec 32}{$excessprecision off}{$fputype sse2}
procedure SSE32;
begin
  GProfile := '32-off-sse2';
  {$I real.operations.cases.inc}
end;

function NativeRealOperationObservations: AnsiString;
begin
  GText := ''; GCalls := 0;
  Off32; Off64; On32; On64; SSE32;
  Result := GText;
end;

function NativeRealOperationCalls: LongInt;
begin
  Result := GCalls;
end;

end.
