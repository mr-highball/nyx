{ Independent whole-source IEEE admission witness. Native generic deduction and
  guarded storage observations establish types and meanings without using Nyx's
  arithmetic. Repeated includes stay original resolver input; no body is invoked
  when the resolver examines those expressions. }
unit ieee.binding.fixture;
{$mode delphi}{$H+}{$codepage utf8}{$J-}
{$modeswitch implicitfunctionspecialization+}
{$IEEEERRORS OFF}{$minfpconstprec 64}{$excessprecision off}{$fputype x87}
interface

function NativeIEEEBindings: AnsiString;
function NativeIEEEBindingCalls: Integer;

implementation

uses SysUtils, TypInfo;

{$if not defined(CPUI386)}
  {$fatal This IEEE binding witness requires the pinned i386 ABI}
{$endif}
{$if SizeOf(Extended) <> 10}
  {$fatal This IEEE binding witness requires ten-byte Extended storage}
{$endif}

type
  { Only the guarded native witness interprets these bytes. NaN payload, sign
    and signaling bits never become a portable resolver contract. }
  TIEEEBindingBytes = array[0..9] of Byte;
  { A distinct authored real type must survive special-value casts and generic
    deduction without being flattened to its primitive representation. }
  TAmount = type Double;

const
  PInf = 1.0 / 0.0;
  NInf = -1.0 / 0.0;
  QNaN = 0.0 / 0.0;
  TypedInf: Double = 1.0 / 0.0;

var GText, GProfile: AnsiString; F64: Double; GCalls: Integer;

function Describe(const AValue: Extended): AnsiString; overload;
var LBytes: TIEEEBindingBytes; LExponent, LAt: Integer; LSign: AnsiString; LInfinity: Boolean;
begin
  Move(AValue,LBytes,SizeOf(LBytes));
  LExponent := (Integer(LBytes[9] and $7f) shl 8)+LBytes[8];
  LSign := '0';

  if LBytes[9] and $80 <> 0 then LSign := '1';

  if LExponent <> $7fff then Exit('finite|'+LSign);
  LInfinity := LBytes[7] = $80;
  for LAt := 0 to 6 do LInfinity := LInfinity and (LBytes[LAt] = 0);

  if LInfinity then Result := 'infinity|'+LSign
  else Result := 'nan|-';
end;

function Describe(const AValue: Boolean): AnsiString; overload;
begin
  Result := 'relation|'+IntToStr(Ord(AValue));
end;

function Keep<T>(const AValue: T): T;
begin
  Result := AValue;
end;

procedure Emit<T>(const ALabel: AnsiString; AConstant: Boolean; const AValue: T);
begin
  Inc(GCalls);
  GText := GText+GProfile+'|'+ALabel+'|'+IntToStr(Ord(AConstant))+'|'+
    AnsiString(PTypeInfo(TypeInfo(T))^.Name)+'|'+Describe(AValue)+#10;
end;

{$IEEEERRORS OFF}{$minfpconstprec 32}{$excessprecision off}{$fputype x87}
procedure Off32;
begin
  GProfile := '32-off-x87';
  {$I ieee.binding.cases.inc}
end;

{$IEEEERRORS OFF}{$minfpconstprec 64}{$excessprecision off}{$fputype x87}
procedure Off64;
begin
  GProfile := '64-off-x87';
  {$I ieee.binding.cases.inc}
end;

{$IEEEERRORS OFF}{$minfpconstprec 32}{$excessprecision on}{$fputype x87}
procedure Wide32;
begin
  GProfile := '32-on-x87';
  {$I ieee.binding.cases.inc}
end;

{$IEEEERRORS OFF}{$minfpconstprec 32}{$excessprecision off}{$fputype sse2}
procedure SSE32;
begin
  GProfile := '32-off-sse2';
  {$I ieee.binding.cases.inc}
end;

{$IEEEERRORS ON}{$minfpconstprec 64}{$excessprecision off}{$fputype x87}
procedure Checked;
begin
  GProfile := 'checked';
  Emit('negative-cast', True, Single(-1.0e300));
  Emit('negative-reference', True, NInf);
  Emit('nan-reference', True, QNaN);
  Emit('negate-negative', True, -NInf);
  Emit('subtract-negative', True, NInf - NInf);
  Emit('nan-divide-nonzero', True, QNaN / 2.0);
  Emit('nan-not-equal', True, QNaN <> QNaN);
  Emit('negative-less', True, NInf < 0.0);
  Emit('literal-checked-underflow', True, 1.0e-9999);
  Emit('literal-checked-negative-underflow', True, -1.0e-9999);
end;

{$IEEEERRORS ON}
procedure Delayed;
begin
  GProfile := 'delayed';
  Emit('operand-off', True, 1.0 / {$IEEEERRORS OFF}0.0);
  {$IEEEERRORS ON}
  Emit('flush-off', True, 1.0 / 0.0 {$IEEEERRORS OFF}{$ifopt H+}{$endif});
  {$IEEEERRORS ON}
  Emit('flush-off-inactive', True, 1.0 / 0.0 {$IEEEERRORS OFF}{$ifopt H-}{$endif});
  {$IEEEERRORS OFF}
  Emit('pending-on', True, 1.0 / 0.0 {$IEEEERRORS ON});
  {$IEEEERRORS OFF}
  Emit('literal-following-on', True, 1.0e9999 {$IEEEERRORS ON});
  {$IEEEERRORS ON}
  Emit('literal-grouped-off', True, {$IEEEERRORS OFF}(1.0e9999));
  {$IEEEERRORS ON}
  Emit('literal-before-minus-off', True, {$IEEEERRORS OFF}-1.0e9999);
  {$IEEEERRORS ON}
  Emit('literal-flush-off', True, {$IEEEERRORS OFF}{$ifopt H+}{$endif}1.0e9999);
  {$IEEEERRORS OFF}
  Emit('literal-negative-cast-following-on', True, Double(-1.0e9999 {$IEEEERRORS ON}));
end;

{$IEEEERRORS OFF}
function NativeIEEEBindings: AnsiString;
begin
  GText := ''; GCalls := 0; F64 := PInf;
  Off32; Off64; Wide32; SSE32; Checked; Delayed;
  Result := GText;
end;

function NativeIEEEBindingCalls: Integer;
begin
  Result := GCalls;
end;

end.
