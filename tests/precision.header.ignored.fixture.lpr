{ Independent compiler oracle. Each deliberately late MODE is ignored with a
  warning, including while a precision change is pending. Implicit generic
  inference observes the actual type without coercing it through an argument. }
program precision_header_ignored_fixture;
{$mode delphi}{$H+}{$codepage utf8}
{$modeswitch implicitfunctionspecialization+}{$excessprecision off}
uses TypInfo;

function Observe<T>(const AValue: T): AnsiString;
begin
  Result := AnsiString(PTypeInfo(TypeInfo(T))^.Name);
end;

procedure Emit(const ALabel, AName: AnsiString);
begin
  WriteLn(ALabel,'=',AName);
end;

var F32: Single;
begin
  F32 := 2;
  Emit('ignored-late-mode', Observe(F32 + F32 {$mode objfpc}));
  Emit('ignored-pending-mode', Observe(F32 + F32 {$excessprecision on}{$mode objfpc}));
  Emit('after-pending', Observe(F32 + F32));
end.
