{ Whole independently compiled string witness. Generic RTTI observes actual
  result types; explicit UTF-16 units observe values without Nyx evaluation.
  The same source can be compiled in Delphi and DelphiUnicode mode. }
{$ifdef NYX_STRING_UNICODE}
  {$mode delphiunicode}
{$else}
  {$mode delphi}
{$endif}
{$H+}{$codepage utf8}{$J-}{$modeswitch implicitfunctionspecialization+}
unit strings.expressions.fixture;

interface

function NativeStringExpressions: AnsiString;
function NativeStringExpressionCalls: Integer;
function NativeStringCapacities: AnsiString;

implementation

uses SysUtils, TypInfo;

type
  { Authored nominal identity lets the witness observe whether concatenation
    retains or promotes a distinct string type rather than guessing by name. }
  TCaption = type UnicodeString;

const
  Prefix = 'Hello';
  Letter = 'A';
  WideLetter = #256;
  Caption = Prefix + ', world';
  TypedCaption: UnicodeString = 'Stored';

var
  GText, GProfile: AnsiString;
  GCalls: Integer;
  A: AnsiString;
  U: UTF8String;
  D: UnicodeString;
  W: WideString;
  S: ShortString;
  Small: String[3];
  C: AnsiChar;
  WC: WideChar;
  Dist: TCaption;

function Describe(const AValue: UnicodeString): AnsiString; overload;
var LAt: Integer;
begin
  Result := 'text';
  for LAt := 1 to Length(AValue) do Result := Result + ',' + AnsiString(IntToStr(Ord(AValue[LAt])));
end;

function Describe(AValue: Boolean): AnsiString; overload;
begin
  Result := 'relation,' + AnsiString(IntToStr(Ord(AValue)));
end;

function Keep<T>(const AValue: T): T;
begin
  Result := AValue;
end;

procedure Emit<T>(const ALabel: AnsiString; AConstant: Boolean; const AValue: T);
begin
  Inc(GCalls);
  GText := GText + GProfile + '|' + ALabel + '|' + AnsiString(IntToStr(Ord(AConstant))) + '|' +
    AnsiString(PTypeInfo(TypeInfo(T))^.Name) + '|' + Describe(AValue) + #10;
end;

{$H+}
procedure LongStrings;
begin
  GProfile := 'long';
  {$I string.expression.cases.inc}
end;

{$H-}
procedure ShortStrings;
begin
  GProfile := 'short';
  {$I string.expression.cases.inc}
end;

{$H+}
function NativeStringExpressions: AnsiString;
begin
  GText := ''; GCalls := 0;
  A := 'Hello'; U := 'UTF8'; D := 'Unicode'; W := 'Wide'; S := 'Short';
  Small := 'ABC'; C := 'C'; WC := #256; Dist := 'Caption';
  LongStrings; ShortStrings;
  Result := GText;
end;

function NativeStringExpressionCalls: Integer;
begin
  Result := GCalls;
end;

function NativeStringCapacities: AnsiString;
begin
  Result := AnsiString(IntToStr(SizeOf(Small))) + '|' +
    AnsiString(IntToStr(SizeOf(Small + '!')));
end;

end.
