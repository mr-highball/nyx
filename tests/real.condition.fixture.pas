{ Whole native conditional witness. Its original unit and repeated include are
  also Nyx input; truth is observed from the compiled branches, not supplied to
  the evaluator as expected generic arguments or reconstructed source snippets. }
unit real.condition.fixture;
{$mode delphi}{$H+}{$codepage utf8}
{$IEEEERRORS OFF}{$minfpconstprec 32}{$excessprecision off}{$fputype x87}
interface

function NativeRealConditions: AnsiString;
function NativeRealConditionCalls: Integer;

implementation

uses SysUtils;

{$if not defined(CPUI386)}
  {$fatal This conditional witness requires the pinned i386 compiler}
{$endif}
{$if SizeOf(Extended) <> 10}
  {$fatal This conditional witness requires ten-byte Extended storage}
{$endif}

const
  SingleValue = Single(0.1);
  SumOfSingles = SingleValue + SingleValue;
  SingleAlias = SingleValue;
  PositiveFlag = SingleValue > 0.0;
  PInf = 1.0 / 0.0;
  NInf = -1.0 / 0.0;
  QNaN = 0.0 / 0.0;

var GText, GProfile: AnsiString; GCalls: Integer;

procedure Emit(const AName: AnsiString; AValue: Boolean);
begin
  Inc(GCalls);
  GText := GText+GProfile+'|'+AName+'|'+IntToStr(Ord(AValue))+#10;
end;

{$IEEEERRORS ON}{$minfpconstprec 32}{$excessprecision off}{$fputype x87}
procedure Narrow;
begin
  GProfile := 'narrow';
  {$I real.condition.cases.inc}
end;

{$IEEEERRORS OFF}{$minfpconstprec 64}{$excessprecision on}{$fputype sse2}
procedure Wide;
begin
  GProfile := 'wide';
  {$I real.condition.cases.inc}
end;

procedure Local;
const SingleValue = Single(0.5);
begin
  GProfile := 'local';
  {$if SingleValue=0.5}Emit('local-shadow',True);{$else}Emit('local-shadow',False);{$endif}
end;

function NativeRealConditions: AnsiString;
begin
  GText := ''; GCalls := 0;
  Narrow; Wide; Local;
  Result := GText;
end;

function NativeRealConditionCalls: Integer;
begin
  Result := GCalls;
end;

end.
