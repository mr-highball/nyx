{ Whole native literal witness. An authored minimum precision is independent
  of excess precision and survives both a header MODE and local PUSH/POP.
  Generic RTTI observes actual deduction, without coercing literal arguments. }
{$minfpconstprec 64}
unit real.literals.fixture;
{$mode delphi}{$H+}{$codepage utf8}
{$modeswitch implicitfunctionspecialization+}
interface

function NativeRealLiterals: AnsiString;
function NativeRealLiteralCalls: LongInt;

implementation

uses TypInfo;

var GText: AnsiString; GCalls: LongInt;

function Observe<T>(const AValue: T): AnsiString;
begin
  Inc(GCalls);
  Result := AnsiString(PTypeInfo(TypeInfo(T))^.Name);
end;

procedure Emit(const ALabel, AName: AnsiString);
begin
  GText := GText+ALabel+'='+AName+#10;
end;

procedure HeaderMinimum;
begin
  Emit('header-64', Observe(1.25));
end;

{$minfpconstprec 32}
procedure Minimum32;
begin
  Emit('min32-exact', Observe(1.25));
  Emit('min32-recurring', Observe(0.1));
  Emit('min32-over24bits', Observe(16777217.0));
  Emit('min32-negative', Observe(-1.25));
  Emit('min32-positive', Observe(+1.25));
  Emit('min32-exponent', Observe(15.0e+1));
  Emit('min32-tiny', Observe(1.0e-40));
end;

{$push}{$minfpconstprec 00064}
procedure Minimum64;
begin
  Emit('min64-exact', Observe(1.25));
  Emit('min64-recurring', Observe(0.1));
  Emit('min64-over24bits', Observe(16777217.0));
  Emit('min64-over53bits', Observe(9007199254740993.0));
end;

{$pop}
procedure AfterPop;
begin
  Emit('after-pop', Observe(1.25));
end;

{$minfpconstprec default explanatory text}
procedure MinimumDefault;
begin
  Emit('default-exact', Observe(1.25));
  Emit('default-recurring', Observe(0.1));
  Emit('following-setting', Observe(1.25 {$minfpconstprec 64}));
  Emit('following-literal', Observe(1.25));
end;

function NativeRealLiterals: AnsiString;
begin
  GText := ''; GCalls := 0;
  HeaderMinimum;
  Minimum32;
  Minimum64;
  AfterPop;
  MinimumDefault;
  Result := GText;
end;

function NativeRealLiteralCalls: LongInt;
begin
  Result := GCalls;
end;

end.
