{ Independent compiler-folded IEEE values under explicit nontrapping source
  settings. The byte classifier observes finite/infinity/NaN meaning only;
  NaN sign, payload and signaling bits are deliberately not compared. }
unit ieee.values.fixture;
{$mode delphi}{$H+}{$codepage utf8}{$IEEEERRORS OFF}
{$minfpconstprec 64}{$excessprecision off}
interface

function NativeIEEEValues: AnsiString;

implementation

uses SysUtils;

{$if not defined(CPUI386)}
  {$fatal This IEEE oracle requires the pinned i386 ABI}
{$endif}
{$if SizeOf(Extended) <> 10}
  {$fatal This IEEE oracle requires ten-byte Extended storage}
{$endif}

type
  { A guarded observation of native storage. No host conversion is used to
    classify a special value and no portable NaN payload contract is inferred. }
  TIEEEBytes = array[0..9] of Byte;

const PInf=1.0/0.0; NInf=-1.0/0.0; QNaN=0.0/0.0;
var GText: AnsiString;

procedure Emit(const ALabel: AnsiString; const AValue: Extended);
var LBytes: TIEEEBytes; LExponent, LAt: Integer; LKind, LSign: AnsiString; LInfinity: Boolean;
begin
  Move(AValue,LBytes,SizeOf(LBytes));
  LExponent := (Integer(LBytes[9] and $7f) shl 8)+LBytes[8]; LKind := 'finite'; LSign := '0';

  if LBytes[9] and $80 <> 0 then LSign := '1';

  if LExponent = $7fff then
  begin
    LInfinity := LBytes[7] = $80;
    for LAt := 0 to 6 do LInfinity := LInfinity and (LBytes[LAt] = 0);

    if LInfinity then LKind := 'infinity'
    else begin LKind := 'nan'; LSign := '-'; end;
  end;
  GText := GText+ALabel+'|'+LKind+'|'+LSign+#10;
end;

procedure Relation(const ALabel: AnsiString; AValue: Boolean);
begin
  GText := GText+ALabel+'|relation|'+IntToStr(Ord(AValue))+#10;
end;

function NativeIEEEValues: AnsiString;
begin
  GText := '';
  Emit('positive',PInf); Emit('negative',NInf); Emit('nan',QNaN);
  Emit('add-positive',PInf+2.0); Emit('add-negative',NInf+2.0);
  Emit('add-opposite',PInf+NInf); Emit('subtract-same',PInf-PInf);
  Emit('multiply-zero',PInf*0.0); Emit('multiply-negative',PInf*(-2.0));
  Emit('multiply-two-negative',NInf*(-2.0)); Emit('divide-infinities',PInf/PInf);
  Emit('divide-finite',1.0/PInf); Emit('divide-negative',(-1.0)/PInf);
  Emit('divide-negative-zero',0.0/NInf); Emit('infinity-divide',NInf/2.0);
  Emit('nan-add',QNaN+1.0); Emit('cast-infinity',Single(PInf));
  Emit('cast-nan',Single(QNaN)); Emit('cast-overflow',Single(1.0e300));
  Emit('cast-underflow',Single(1.0e-300)); Emit('cast-negative-underflow',Single(-1.0e-300));
  Relation('nan-equal',QNaN=QNaN); Relation('nan-not-equal',QNaN<>QNaN);
  Relation('nan-less',QNaN<1.0); Relation('nan-less-equal',QNaN<=1.0);
  Relation('nan-greater',QNaN>1.0); Relation('nan-greater-equal',QNaN>=1.0);
  Relation('infinity-equal',PInf=PInf); Relation('infinity-less',NInf<PInf);
  Relation('infinity-greater',PInf>1.0);
  Result := GText;
end;

end.
