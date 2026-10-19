{ Whole native source-selection witness. FPUTYPE changes arithmetic and its
  selector defines immediately; local PUSH/POP deliberately does not restore it. }
unit source.fpu.fixture;
{$mode delphi}{$H+}{$codepage utf8}
{$modeswitch implicitfunctionspecialization+}
interface

function NativeSourceFPUObservations: AnsiString;
function NativeSourceFPUCalls: LongInt;

implementation

uses TypInfo;

var
  GObservations, GLabel: AnsiString;
  GCalls: LongInt;

function Observe<T>(const AValue: T): AnsiString;
begin
  Inc(GCalls);
  Result := AnsiString(PTypeInfo(TypeInfo(T))^.Name);
end;

{$fputype X87}
procedure AtX87;
var LValue: LongInt;
begin
  LValue := 4;
  GLabel := 'x87'; GObservations := GObservations+GLabel+'='+Observe(LValue/LValue)+#10;
  {$ifdef FPUX87}GObservations := GObservations+'x87-define=true'+#10;{$endif}
end;

{$push}{$fputype SSE2}
procedure AtSSE2;
var LValue: LongInt;
begin
  LValue := 4;
  GLabel := 'sse2'; GObservations := GObservations+GLabel+'='+Observe(LValue/LValue)+#10;
  {$ifdef FPUSSE2}GObservations := GObservations+'sse2-define=true'+#10;{$endif}
  {$ifndef FPUX87}GObservations := GObservations+'old-x87-define=false'+#10;{$endif}
end;
{$pop}

procedure AfterPop;
var LValue: LongInt;
begin
  LValue := 4;
  GLabel := 'after-pop'; GObservations := GObservations+GLabel+'='+Observe(LValue/LValue)+#10;
  {$ifdef FPUSSE2}GObservations := GObservations+'after-pop-sse2=true'+#10;{$endif}
end;

{$fputype X87}{$undef FPUX87}{$define FPUSSE2}
procedure AfterDefine;
var LValue: LongInt;
begin
  LValue := 4;
  GLabel := 'after-define'; GObservations := GObservations+GLabel+'='+Observe(LValue/LValue)+#10;
end;

{$fputype X87}
procedure AfterReassert;
var LValue: LongInt;
begin
  LValue := 4;
  GLabel := 'after-reassert'; GObservations := GObservations+GLabel+'='+Observe(LValue/LValue)+#10;
  {$if defined(FPUX87) and defined(FPUSSE2)}GObservations := GObservations+'unrelated-define-retained=true'+#10;{$endif}
end;

function NativeSourceFPUObservations: AnsiString;
begin
  AtX87;
  AtSSE2;
  AfterPop;
  AfterDefine;
  AfterReassert;
  Result := GObservations;
end;

function NativeSourceFPUCalls: LongInt;
begin
  Result := GCalls;
end;

end.
