{ The compiler's arithmetic choice is independent of an authored define with
  another FPU's spelling. This whole witness executes no Nyx inference code. }
unit configured.fpu.fixture;
{$mode delphi}{$H+}{$codepage utf8}
{$modeswitch implicitfunctionspecialization+}
interface

function NativeConfiguredFPU: AnsiString;
function NativeSpoofedDivisionType: AnsiString;
function NativeConfiguredInvocations: LongInt;

implementation

uses TypInfo;

const
  {$ifdef FPUX87}CConfigured: AnsiString = 'x87';{$else}CConfigured: AnsiString = '';{$endif}

{$undef FPUX87}{$define FPUSSE2}

var
  GInvocations: LongInt;

function Observe<T>(const AValue: T): AnsiString;
begin
  Inc(GInvocations);
  Result := AnsiString(PTypeInfo(TypeInfo(T))^.Name);
end;

function NativeConfiguredFPU: AnsiString;
begin
  Result := CConfigured;
end;

function NativeSpoofedDivisionType: AnsiString;
var LValue: LongInt;
begin
  LValue := 4;
  Result := Observe(LValue/LValue);
end;

function NativeConfiguredInvocations: LongInt;
begin
  Result := GInvocations;
end;

end.
