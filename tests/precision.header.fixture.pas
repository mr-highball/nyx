{ Whole native witness for an authored header MODE effect. The OFF before
  MODE is intentional: Delphi mode overrides it. Saved local settings later
  recover that mode-derived value, independently of an explicit ON request. }
{$excessprecision off}
unit precision.header.fixture;
{$mode delphi}{$H+}{$codepage utf8}
{$modeswitch implicitfunctionspecialization+}
interface

var F32: Single; I32: LongInt;

function NativeHeaderPrecision: AnsiString;
function NativeHeaderPrecisionCalls: LongInt;

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

procedure HeaderDefault;
begin
  Emit('header-add', Observe(F32 + F32));
  Emit('header-negative', Observe(-F32));
  Emit('header-positive', Observe(+F32));
  Emit('header-mixed', Observe(I32 + F32));
end;

{$push}{$excessprecision off}
procedure ExplicitOff;
begin
  Emit('explicit-off', Observe(F32 + F32));
end;

{$pop}
procedure RestoredHeader;
begin
  Emit('restored-header', Observe(F32 + F32));
end;

function NativeHeaderPrecision: AnsiString;
begin
  GText := ''; GCalls := 0; F32 := 2; I32 := 3;
  HeaderDefault;
  ExplicitOff;
  RestoredHeader;
  Result := GText;
end;

function NativeHeaderPrecisionCalls: LongInt;
begin
  Result := GCalls;
end;

end.
