{ Whole handwritten literal witness. Generic specialization exposes its real
  RTTI name through a public native bridge; source inspection never calls it. }
unit literal.contracts.fixture;
{$ifdef NYX_LITERAL_UNICODE}
{$mode delphiunicode}
{$modeswitch unicodestrings+}
{$else}
{$mode delphi}
{$modeswitch unicodestrings-}
{$endif}
{$H+}{$codepage utf8}
{$modeswitch implicitfunctionspecialization+}
interface

type
  { A named generic result supplies a concrete member chain for ordinary
    completion. The native witness creates and frees one real specialization. }
  TLiteralBox<T> = class
  public
    Value: T;
  end;

function EchoLiteral<T>(const AValue: T): T;
function BoxLiteral<T>(const AValue: T): TLiteralBox<T>;
function NativeLiteralBoxValue: LongInt;
{ The compiler independently materializes the exact mixed byte/wide sequence,
  including NUL and a lone surrogate, for the portable value-identity check. }
function NativeLiteralCodeUnits: UnicodeString;
{ The generic template refers only to this exported bridge, whose implementation
  owns the actual TypInfo dependency and native observation storage. }
procedure RecordLiteralType(ATypeInfo: Pointer);
procedure RunLiteralWitness;
function LiteralObservations: AnsiString;
function LiteralInvocations: LongInt;

implementation

uses TypInfo;

var
  GLabel: AnsiString;
  GObservations: AnsiString;
  GInvocations: LongInt;

procedure RecordLiteralType(ATypeInfo: Pointer);
begin
  Inc(GInvocations);
  GObservations := GObservations + GLabel + '=' + AnsiString(PTypeInfo(ATypeInfo)^.Name) + #10;
end;

function EchoLiteral<T>(const AValue: T): T;
begin
  RecordLiteralType(TypeInfo(T));
  Result := AValue;
end;

function BoxLiteral<T>(const AValue: T): TLiteralBox<T>;
begin
  Result := TLiteralBox<T>.Create;
  Result.Value := AValue;
end;

function NativeLiteralBoxValue: LongInt;
var
  LBox: TLiteralBox<ShortInt>;
begin
  LBox := BoxLiteral(42);
  try
    Result := LBox.Value;
  finally
    LBox.Free;
  end;
end;

function NativeLiteralCodeUnits: UnicodeString;
begin
  Result := #195#169#256#0#$D800;
end;

function LiteralObservations: AnsiString;
begin
  Result := GObservations;
end;

function LiteralInvocations: LongInt;
begin
  Result := GInvocations;
end;

procedure RunLiteralWitness;
begin
  GLabel := 'zero'; EchoLiteral(0);
  GLabel := 'small'; EchoLiteral(42);
  GLabel := 'signed8-high'; EchoLiteral(127);
  GLabel := 'unsigned8-low'; EchoLiteral(128);
  GLabel := 'unsigned8-high'; EchoLiteral(255);
  GLabel := 'signed16-low-positive'; EchoLiteral(256);
  GLabel := 'signed16-high'; EchoLiteral(32767);
  GLabel := 'unsigned16-low'; EchoLiteral(32768);
  GLabel := 'unsigned16-high'; EchoLiteral(65535);
  GLabel := 'signed32-low-positive'; EchoLiteral(65536);
  GLabel := 'signed32-high'; EchoLiteral(2147483647);
  GLabel := 'unsigned32-low'; EchoLiteral(2147483648);
  GLabel := 'unsigned32-high'; EchoLiteral(4294967295);
  GLabel := 'signed64-low-positive'; EchoLiteral(4294967296);
  GLabel := 'signed64-high'; EchoLiteral(9223372036854775807);
  GLabel := 'unsigned64-low'; EchoLiteral(9223372036854775808);
  GLabel := 'unsigned64-high'; EchoLiteral(18446744073709551615);
  GLabel := 'negative-small'; EchoLiteral(-42);
  GLabel := 'signed8-low'; EchoLiteral(-128);
  GLabel := 'signed16-negative'; EchoLiteral(-129);
  GLabel := 'signed16-low'; EchoLiteral(-32768);
  GLabel := 'signed32-negative'; EchoLiteral(-32769);
  GLabel := 'signed32-low'; EchoLiteral(-2147483648);
  GLabel := 'signed64-negative'; EchoLiteral(-2147483649);
  GLabel := 'signed64-low'; EchoLiteral(-9223372036854775808);
  GLabel := 'unary-plus'; EchoLiteral(+42);
  GLabel := 'hex'; EchoLiteral($FF);
  GLabel := 'octal'; EchoLiteral(&377);
  GLabel := 'binary'; EchoLiteral(%11111111);
  GLabel := 'boolean-true'; EchoLiteral(True);
  GLabel := 'boolean-false'; EchoLiteral(False);
  GLabel := 'ascii-char'; EchoLiteral('A');
  GLabel := 'numeric-char'; EchoLiteral(#65);
  GLabel := 'zero-char'; EchoLiteral(#0);
  GLabel := 'byte-char'; EchoLiteral(#255);
  GLabel := 'wide-char'; EchoLiteral(#256);
  GLabel := 'wide-char-high'; EchoLiteral(#65535);
  GLabel := 'latin-char'; EchoLiteral('é');
  GLabel := 'greek-char'; EchoLiteral('λ');
  GLabel := 'astral'; EchoLiteral('😀');
  GLabel := 'empty-string'; EchoLiteral('');
  GLabel := 'ascii-string'; EchoLiteral('ab');
  GLabel := 'quoted-string'; EchoLiteral('a''b');
  GLabel := 'joined-string'; EchoLiteral('a'#10'b');
  GLabel := 'wide-joined'; EchoLiteral(#256#0);
  GLabel := 'numeric-astral'; EchoLiteral(#128512);
  GLabel := 'zero-padded-char'; EchoLiteral(#0000);
  GLabel := 'hex-padded-char'; EchoLiteral(#$0041);
  GLabel := 'wide-then-byte'; EchoLiteral(#256#255);
  GLabel := 'utf8-byte-prefix'; EchoLiteral(#195#169#256);
  GLabel := 'utf8-source-string'; EchoLiteral('éa');
  GLabel := 'real'; EchoLiteral(1.5);
  {$H-}
  GLabel := 'short-string'; EchoLiteral('ab');
  GLabel := 'short-wide-string'; EchoLiteral(#256#0);
  {$H+}
  {$ifdef NYX_LITERAL_ADJACENT}
  GLabel := 'spaced-string'; EchoLiteral('a' 'b');
  {$endif}
  {$ifdef NYX_LITERAL_NIL}
  GLabel := 'nil'; EchoLiteral(nil);
  {$endif}
end;

end.
