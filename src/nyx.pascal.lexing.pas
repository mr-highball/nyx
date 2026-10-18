{ nyx
  Copyright (c) 2020 mr-highball

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
}
unit nyx.pascal.lexing;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.text, nyx.scheduler;

type
  { Hard malformed-Unicode/range contract refusal. No partial lexical snapshot
    escapes. Ordinary unfinished Pascal instead supplies retained diagnostics. }
  ENyxPascalLexical = class(EArgumentException);
  { Caller-selected source/token/diagnostic/depth budgets refuse atomically,
    independently of recoverable source errors. No output is silently truncated. }
  ENyxPascalLexicalLimit = class(ENyxPascalLexical);
  { Cooperative analysis cancellation publishes no partial syntax/lexical data.
    Callers may translate this into their existing cancelled result status. }
  ENyxPascalAnalysisCancelled = class(ENyxPascalLexical);
  { Half-open Unicode-scalar range inside one exact physical source. No target
    byte/UTF-16 storage index, mutable text, interface or editor is retained. }
  TNyxPascalSpan = record
  private
    FStart: Integer;
    FLength: Integer;
  public
    function Finish: Integer;
    property Start: Integer read FStart;
    property Length: Integer read FLength;
  end;
  { Contextual words are recognized, not all reserved in every Pascal dialect.
    Escaped identifiers retain pkwNone even when their spelling matches a word. }
  TNyxPascalWord = (
    pkwNone, pkwAbsolute, pkwAbstract, pkwAlias, pkwAnd,
    pkwArray, pkwAs, pkwAsm, pkwAssembler, pkwAutomated,
    pkwBegin, pkwCase, pkwCdecl, pkwClass, pkwConst,
    pkwConstref, pkwConstructor, pkwContains, pkwDefault, pkwDelayed,
    pkwDeprecated, pkwDestructor, pkwDispid, pkwDispinterface, pkwDiv,
    pkwDo, pkwDownto, pkwDynamic, pkwElse, pkwEnd,
    pkwExcept, pkwExperimental, pkwExport, pkwExports, pkwExternal,
    pkwFar, pkwFile, pkwFinal, pkwFinalization, pkwFinally,
    pkwFor, pkwForward, pkwFunction, pkwGeneric, pkwGoto,
    pkwHelper, pkwIf, pkwImplementation, pkwImplements, pkwIn,
    pkwIndex, pkwInherited, pkwInitialization, pkwInline, pkwInterface,
    pkwInterrupt, pkwIs, pkwLabel, pkwLibrary, pkwLocal,
    pkwMessage, pkwMod, pkwName, pkwNear, pkwNil,
    pkwNodefault, pkwNoreturn, pkwNostackframe, pkwNot, pkwObject,
    pkwOf, pkwOn, pkwOperator, pkwOr, pkwOut,
    pkwOverload, pkwOverride, pkwPackage, pkwPacked, pkwPascal,
    pkwPlatform, pkwPrivate, pkwProcedure, pkwProgram, pkwProperty,
    pkwProtected, pkwPublic, pkwPublished, pkwRaise, pkwRead,
    pkwReadonly, pkwRecord, pkwReference, pkwRegister, pkwReintroduce,
    pkwRepeat, pkwRequires, pkwResident, pkwResourcestring, pkwSafecall,
    pkwSealed, pkwSet, pkwShl, pkwShr, pkwSoftfloat,
    pkwSpecialize, pkwStatic, pkwStdcall, pkwStored, pkwStrict,
    pkwString, pkwThen, pkwThreadvar, pkwTo, pkwTry,
    pkwType, pkwUnaligned, pkwUnit, pkwUntil, pkwUses,
    pkwVar, pkwVarargs, pkwVirtual, pkwWhile, pkwWith,
    pkwWrite, pkwWriteonly, pkwXor);
  { Longest physical symbol spelling. Alternate brackets stay distinguishable;
    parsing, dialect eligibility and operator meaning are separate from lexing. }
  TNyxPascalSymbol = (
    psoNone, psoPlus, psoMinus, psoStar, psoSlash,
    psoEqual, psoLess, psoGreater, psoOpenBracket, psoCloseBracket,
    psoDot, psoComma, psoOpenParen, psoCloseParen, psoColon,
    psoSemicolon, psoCaret, psoAt, psoAmpersand, psoRightBrace,
    psoAssign, psoNotEqual, psoSymmetricDifference, psoLessEqual, psoGreaterEqual,
    psoShiftLeft, psoShiftRight, psoPower, psoAddAssign, psoSubtractAssign,
    psoMultiplyAssign, psoDivideAssign, psoRange, psoAlternateOpenBracket, psoAlternateCloseBracket,
    psoCommentClose);
  { Lossless token families include every whitespace/comment/directive and one
    zero-length EOF. An invalid scalar remains visible rather than disappearing. }
  TNyxPascalTokenKind = (ptkWord, ptkNumber, ptkString, ptkCharacter,
    ptkSymbol, ptkWhitespace, ptkComment, ptkDirective, ptkInvalid, ptkEOF);
  { Numeric spelling only, never a lossy machine-number conversion or compiler
    admission. Decimal exponents/fractions are marked separately on their token. }
  TNyxPascalNumberBase = (pnbDecimal, pnbHexadecimal, pnbOctal, pnbBinary);
  { Closed recoverable lexical failures. Messages are stable English guidance;
    the exact source and scalar span remain available separately. }
  TNyxPascalLexicalIssue = (pliUnexpectedScalar, pliUnterminatedString,
    pliUnterminatedComment, pliMissingDigits, pliInvalidDigit, pliMissingExponent);
  { Detached diagnostic for one source span. It owns no source or mutable array. }
  TNyxPascalLexicalDiagnostic = record
  private
    FIssue: TNyxPascalLexicalIssue;
    FSpan: TNyxPascalSpan;
  public
    function MessageText: TNyxText;
    property Issue: TNyxPascalLexicalIssue read FIssue;
    property Span: TNyxPascalSpan read FSpan;
  end;
  { Copied lexical policies. Defaults match ordinary Delphi comment termination,
    ASCII identifiers and four MiB input; numerical spellings are classified
    independently of compiler-mode eligibility. Unicode identifiers are an
    explicit analysis extension, not a claim that a compiler accepts them.
    Records retain no interfaces; returned fluent values are independent. }
  TNyxPascalLexicalOptions = record
  private
    FDefined: Boolean;
    FNested: Boolean;
    FUnicodeIdentifiers: Boolean;
    FBytes: Integer;
    FTokens: Integer;
    FDiagnostics: Integer;
    FCommentDepth: Integer;
  public
    { Defaults are explicit; a zero-initialized options record refuses. Each
      fluent setter returns an independent copy and validates its positive budget. }
    class function Defaults: TNyxPascalLexicalOptions; static;
    { Nested termination is a caller-selected lexical policy, not a guessed mode. }
    function NestedComments(AValue: Boolean): TNyxPascalLexicalOptions;
    { Opt into category-based identifier spelling without compiler acceptance. }
    function UnicodeIdentifiers(AValue: Boolean): TNyxPascalLexicalOptions;
    { UTF-8 bytes on both targets; a browser UTF-16 length is never this budget. }
    function UpToSourceBytes(AValue: Integer): TNyxPascalLexicalOptions;
    { Includes trivia and the final EOF token; no truncated stream is returned. }
    function UpToTokens(AValue: Integer): TNyxPascalLexicalOptions;
    { Refuse excessive errors rather than silently dropping recovery information. }
    function UpToDiagnostics(AValue: Integer): TNyxPascalLexicalOptions;
    { Applies when nested comments are enabled, including the outer delimiter. }
    function UpToCommentDepth(AValue: Integer): TNyxPascalLexicalOptions;
    { Raises ENyxPascalLexical for undefined or nonpositive policies. }
    procedure Validate;
    property AllowNestedComments: Boolean read FNested;
    property AllowUnicodeIdentifiers: Boolean read FUnicodeIdentifiers;
    property MaximumSourceBytes: Integer read FBytes;
    property MaximumTokens: Integer read FTokens;
    property MaximumDiagnostics: Integer read FDiagnostics;
    property MaximumCommentDepth: Integer read FCommentDepth;
  end;
  { Immutable value token. Public positions are scalar-based, line/column are
    one-based with CRLF one newline. Private storage spans belong only to this
    snapshot. No per-token source copy, interface or reference cycle is retained. }
  TNyxPascalToken = record
  private
    FSpan: TNyxPascalSpan;
    FKind: TNyxPascalTokenKind;
    FWord: TNyxPascalWord;
    FSymbol: TNyxPascalSymbol;
    FBase: TNyxPascalNumberBase;
    FLine: Integer;
    FColumn: Integer;
    FStorage: Integer;
    FStorageLength: Integer;
    FComplete: Boolean;
    FEscaped: Boolean;
    FReal: Boolean;
  public
    property Span: TNyxPascalSpan read FSpan;
    property Kind: TNyxPascalTokenKind read FKind;
    property Word: TNyxPascalWord read FWord;
    property Symbol: TNyxPascalSymbol read FSymbol;
    property NumberBase: TNyxPascalNumberBase read FBase;
    property Line: Integer read FLine;
    property Column: Integer read FColumn;
    property Complete: Boolean read FComplete;
    property Escaped: Boolean read FEscaped;
    property RealNumber: Boolean read FReal;
  end;
  { Owns exact physical text and ordered immutable tokens/diagnostics. Every
    storage unit belongs to a token; EOF has no units. Getters borrow no caller,
    cancellation scope, file, GUI or mutable buffer. Returned records are values. }
  INyxPascalLexicalSnapshot = interface(IInterface)
    ['{98349E7C-C0ED-48DE-A518-427A04570B01}']
    function Source: TNyxText;
    function Options: TNyxPascalLexicalOptions;
    function Scalars: Integer;
    { Zero-based token access. Count includes EOF; bad ordinals refuse. Text
      returns the exact authored spelling, including every delimiter and escape. }
    function Count: Integer;
    function Token(AIndex: Integer): TNyxPascalToken;
    function Text(AIndex: Integer): TNyxText;
    { Word spelling without an escape ampersand; refuses another token family. }
    function Identifier(AIndex: Integer): TNyxText;
    { Token containing this scalar; a boundary selects the following token,
      and the complete source end selects EOF. Invalid offsets refuse. }
    function TokenAt(AOffset: Integer): Integer;
    { Exact scalar slice, including empty EOF; out-of-source ranges refuse. }
    function Slice(const ASpan: TNyxPascalSpan): TNyxText;
    function DiagnosticCount: Integer;
    function Diagnostic(AIndex: Integer): TNyxPascalLexicalDiagnostic;
  end;

{ Construct a checked nonnegative scalar range without associating a source. }
function NyxPascalSpan(AStart, ALength: Integer): TNyxPascalSpan;
{ Exact ASCII canonicalization for Pascal syntax words. Non-ASCII storage remains
  untouched; optional Unicode identifier equivalence is a separate semantic policy. }
function NyxPascalASCIIKey(const AText: TNyxText): TNyxText;
{ Build a detached complete token stream. Cancellation is borrowed only during
  construction and checked at entry, each token and bounded scalar intervals.
  Malformed Unicode and quotas are hard failures; unfinished strings recover at
  the next physical newline, unfinished comments retain the remaining source. }
function NewNyxPascalLexicalSnapshot(const ASource: TNyxText): INyxPascalLexicalSnapshot; overload;
function NewNyxPascalLexicalSnapshot(const ASource: TNyxText;
  const AOptions: TNyxPascalLexicalOptions;
  const ACancellation: INyxCancellationScope = nil): INyxPascalLexicalSnapshot; overload;

implementation

uses nyx.text.index, nyx.unicode, nyx.unicode.types;

const
  CWords: array[TNyxPascalWord] of TNyxText = (
    '', 'absolute', 'abstract', 'alias', 'and',
    'array', 'as', 'asm', 'assembler', 'automated',
    'begin', 'case', 'cdecl', 'class', 'const',
    'constref', 'constructor', 'contains', 'default', 'delayed',
    'deprecated', 'destructor', 'dispid', 'dispinterface', 'div',
    'do', 'downto', 'dynamic', 'else', 'end',
    'except', 'experimental', 'export', 'exports', 'external',
    'far', 'file', 'final', 'finalization', 'finally',
    'for', 'forward', 'function', 'generic', 'goto',
    'helper', 'if', 'implementation', 'implements', 'in',
    'index', 'inherited', 'initialization', 'inline', 'interface',
    'interrupt', 'is', 'label', 'library', 'local',
    'message', 'mod', 'name', 'near', 'nil',
    'nodefault', 'noreturn', 'nostackframe', 'not', 'object',
    'of', 'on', 'operator', 'or', 'out',
    'overload', 'override', 'package', 'packed', 'pascal',
    'platform', 'private', 'procedure', 'program', 'property',
    'protected', 'public', 'published', 'raise', 'read',
    'readonly', 'record', 'reference', 'register', 'reintroduce',
    'repeat', 'requires', 'resident', 'resourcestring', 'safecall',
    'sealed', 'set', 'shl', 'shr', 'softfloat',
    'specialize', 'static', 'stdcall', 'stored', 'strict',
    'string', 'then', 'threadvar', 'to', 'try',
    'type', 'unaligned', 'unit', 'until', 'uses',
    'var', 'varargs', 'virtual', 'while', 'with',
    'write', 'writeonly', 'xor');
  CSymbols: array[TNyxPascalSymbol] of TNyxText = (
    '', '+', '-', '*', '/',
    '=', '<', '>', '[', ']',
    '.', ',', '(', ')', ':',
    ';', '^', '@', '&', '}',
    ':=', '<>', '><', '<=', '>=',
    '<<', '>>', '**', '+=', '-=',
    '*=', '/=', '..', '(.', '.)',
    '*)');

type
  { Active nested block-comment delimiter, never a compiler conditional scope. }
  TCommentDelimiter = (pcdBrace, pcdParenthesis);
  { Owns its text/vectors. Cursor, cancellation borrow and quota counters exist
    only while Build runs; publication releases the cancellation scope. }
  TLexicalSnapshot = class(TInterfacedObject, INyxPascalLexicalSnapshot)
  private
    FSource: TNyxText;
    FOptions: TNyxPascalLexicalOptions;
    FTokens: array of TNyxPascalToken;
    FDiagnostics: array of TNyxPascalLexicalDiagnostic;
    FTokenCount: Integer;
    FDiagnosticCount: Integer;
    FStorage: Integer;
    FOffset: Integer;
    FLine: Integer;
    FColumn: Integer;
    FBytes: Integer;
    FPreviousCR: Boolean;
    FCancellation: INyxCancellationScope;
    function Current: Integer;
    function NextASCII(AChar: Char): Boolean;
    function IdentifierStart(AScalar: Integer): Boolean;
    function IdentifierPart(AScalar: Integer): Boolean;
    procedure CheckCancellation;
    procedure Consume;
    procedure Issue(AIssue: TNyxPascalLexicalIssue; AStart: Integer);
    procedure Add(const AToken: TNyxPascalToken);
    procedure WordToken(var AToken: TNyxPascalToken);
    procedure NumberToken(var AToken: TNyxPascalToken; ACharacter: Boolean);
    procedure StringToken(var AToken: TNyxPascalToken);
    procedure CommentToken(var AToken: TNyxPascalToken; ADelimiter: TCommentDelimiter);
    procedure Build;
  public
    constructor Create(const ASource: TNyxText; const AOptions: TNyxPascalLexicalOptions;
      const ACancellation: INyxCancellationScope);
    function Source: TNyxText;
    function Options: TNyxPascalLexicalOptions;
    function Scalars: Integer;
    function Count: Integer;
    function Token(AIndex: Integer): TNyxPascalToken;
    function Text(AIndex: Integer): TNyxText;
    function Identifier(AIndex: Integer): TNyxText;
    function TokenAt(AOffset: Integer): Integer;
    function Slice(const ASpan: TNyxPascalSpan): TNyxText;
    function DiagnosticCount: Integer;
    function Diagnostic(AIndex: Integer): TNyxPascalLexicalDiagnostic;
  end;

var
  { Initialized once, then read-only. Tables own only immutable syntax spellings
    and enum ordinals; no source snapshot, host object or session is retained. }
  GWords: TNyxTextIndex;
  GSymbols: TNyxTextIndex;

function NyxPascalSpan(AStart, ALength: Integer): TNyxPascalSpan;
begin

  if (AStart < 0) or (ALength < 0) or (ALength > High(Integer) - AStart) then
  begin
    raise ENyxPascalLexical.Create('Pascal span exceeds its nonnegative scalar domain');
  end;
  Result.FStart := AStart;
  Result.FLength := ALength;
end;

function TNyxPascalSpan.Finish: Integer;
begin
  Result := FStart + FLength;
end;

function NyxPascalASCIIKey(const AText: TNyxText): TNyxText;
var
  LAt: Integer;
begin
  Result := AText;
  for LAt := 1 to Length(Result) do
  begin

    if (Result[LAt] >= 'A') and (Result[LAt] <= 'Z') then
    begin
      Result[LAt] := Chr(Ord(Result[LAt]) + 32);
    end;
  end;
end;

function TNyxPascalLexicalDiagnostic.MessageText: TNyxText;
const
  CMessages: array[TNyxPascalLexicalIssue] of TNyxText = (
    'Unrecognized Pascal scalar', 'Finish this Pascal string before the newline',
    'Finish this Pascal block comment', 'Add digits after the numeric prefix',
    'This digit is outside the literal base', 'Add digits to this exponent');
begin
  Result := CMessages[FIssue];
end;

class function TNyxPascalLexicalOptions.Defaults: TNyxPascalLexicalOptions;
begin
  Result := Default(TNyxPascalLexicalOptions);
  Result.FDefined := True;
  Result.FBytes := 4 * 1024 * 1024;
  Result.FTokens := 400000;
  Result.FDiagnostics := 1024;
  Result.FCommentDepth := 256;
end;

procedure TNyxPascalLexicalOptions.Validate;
begin

  if not FDefined or (FBytes < 1) or (FTokens < 1) or (FDiagnostics < 1) or
    (FCommentDepth < 1) then
  begin
    raise ENyxPascalLexical.Create('Use explicit positive Pascal lexical options');
  end;
end;

function TNyxPascalLexicalOptions.NestedComments(AValue: Boolean): TNyxPascalLexicalOptions;
begin
  Validate;
  Result := Self;
  Result.FNested := AValue;
end;

function TNyxPascalLexicalOptions.UnicodeIdentifiers(AValue: Boolean): TNyxPascalLexicalOptions;
begin
  Validate;
  Result := Self;
  Result.FUnicodeIdentifiers := AValue;
end;

function TNyxPascalLexicalOptions.UpToSourceBytes(AValue: Integer): TNyxPascalLexicalOptions;
begin
  Result := Self;
  Result.FBytes := AValue;
  Result.Validate;
end;

function TNyxPascalLexicalOptions.UpToTokens(AValue: Integer): TNyxPascalLexicalOptions;
begin
  Result := Self;
  Result.FTokens := AValue;
  Result.Validate;
end;

function TNyxPascalLexicalOptions.UpToDiagnostics(AValue: Integer): TNyxPascalLexicalOptions;
begin
  Result := Self;
  Result.FDiagnostics := AValue;
  Result.Validate;
end;

function TNyxPascalLexicalOptions.UpToCommentDepth(AValue: Integer): TNyxPascalLexicalOptions;
begin
  Result := Self;
  Result.FCommentDepth := AValue;
  Result.Validate;
end;

constructor TLexicalSnapshot.Create(const ASource: TNyxText;
  const AOptions: TNyxPascalLexicalOptions; const ACancellation: INyxCancellationScope);
begin
  inherited Create;
  AOptions.Validate;
  FOptions := AOptions;
  FCancellation := ACancellation;
  CheckCancellation;
  { Target storage length is a cheap lower bound for UTF-8 byte length. Exact
    byte accounting and Unicode validation occur during scalar consumption. }

  if Length(ASource) > FOptions.MaximumSourceBytes then
  begin
    raise ENyxPascalLexicalLimit.Create('Pascal source exceeds its lexical byte budget');
  end;
  FSource := ASource;
  FStorage := 1;
  FLine := 1;
  FColumn := 1;
  try
    Build;
  finally
    FCancellation := nil;
  end;
end;

procedure TLexicalSnapshot.CheckCancellation;
begin

  if (FCancellation <> nil) and FCancellation.Cancelled then
  begin
    raise ENyxPascalAnalysisCancelled.Create('Pascal analysis cancelled');
  end;
end;

function TLexicalSnapshot.Current: Integer;
var
  LAt: Integer;
begin
  Result := -1;

  if FStorage > Length(FSource) then
  begin
    Exit;
  end;
  LAt := FStorage;

  if not NyxNextScalar(FSource, LAt, Result) then
  begin
    raise ENyxPascalLexical.Create('Pascal source contains malformed Unicode');
  end;
end;

function TLexicalSnapshot.NextASCII(AChar: Char): Boolean;
begin
  Result := (FStorage < Length(FSource)) and (FSource[FStorage + 1] = AChar);
end;

procedure TLexicalSnapshot.Consume;
var
  LScalar: Integer;
  LBytes: Integer;
begin
  LScalar := Current;

  if LScalar < 0 then
  begin
    Exit;
  end;
  NyxNextScalar(FSource, FStorage, LScalar);
  LBytes := 1;

  if LScalar > $FFFF then
  begin
    LBytes := 4;
  end
  else if LScalar > $7FF then
  begin
    LBytes := 3;
  end
  else if LScalar > $7F then
  begin
    LBytes := 2;
  end;

  if LBytes > FOptions.MaximumSourceBytes - FBytes then
  begin
    raise ENyxPascalLexicalLimit.Create('Pascal source exceeds its lexical byte budget');
  end;
  Inc(FBytes, LBytes);
  Inc(FOffset);

  if LScalar = 13 then
  begin
    Inc(FLine);
    FColumn := 1;
  end
  else if LScalar = 10 then
  begin

    if not FPreviousCR then
    begin
      Inc(FLine);
    end;
    FColumn := 1;
  end
  else
  begin
    Inc(FColumn);
  end;
  FPreviousCR := LScalar = 13;

  if FOffset mod 1024 = 0 then
  begin
    CheckCancellation;
  end;
end;

function TLexicalSnapshot.IdentifierStart(AScalar: Integer): Boolean;
begin
  Result := ((AScalar >= Ord('A')) and (AScalar <= Ord('Z'))) or
    ((AScalar >= Ord('a')) and (AScalar <= Ord('z'))) or (AScalar = Ord('_'));

  if not Result and FOptions.AllowUnicodeIdentifiers and (AScalar > 127) then
  begin
    Result := NyxScalarCategory(AScalar) in [nucUppercaseLetter, nucLowercaseLetter,
      nucTitlecaseLetter, nucModifierLetter, nucOtherLetter, nucLetterNumber];
  end;
end;

function TLexicalSnapshot.IdentifierPart(AScalar: Integer): Boolean;
begin
  Result := IdentifierStart(AScalar) or ((AScalar >= Ord('0')) and (AScalar <= Ord('9')));

  if not Result and FOptions.AllowUnicodeIdentifiers and (AScalar > 127) then
  begin
    Result := NyxScalarCategory(AScalar) in [nucNonspacingMark, nucSpacingMark,
      nucDecimalNumber, nucConnectorPunctuation];
  end;
end;

procedure TLexicalSnapshot.Issue(AIssue: TNyxPascalLexicalIssue; AStart: Integer);
var
  LCapacity: Integer;
begin

  if FDiagnosticCount >= FOptions.MaximumDiagnostics then
  begin
    raise ENyxPascalLexicalLimit.Create('Pascal diagnostics exceed their lexical budget');
  end;

  if FDiagnosticCount = Length(FDiagnostics) then
  begin
    LCapacity := FOptions.MaximumDiagnostics;

    if 16 <= FOptions.MaximumDiagnostics - FDiagnosticCount then
    begin
      LCapacity := FDiagnosticCount + 16;
    end;
    SetLength(FDiagnostics, LCapacity);
  end;
  FDiagnostics[FDiagnosticCount].FIssue := AIssue;
  FDiagnostics[FDiagnosticCount].FSpan := NyxPascalSpan(AStart, FOffset - AStart);
  Inc(FDiagnosticCount);
end;

procedure TLexicalSnapshot.Add(const AToken: TNyxPascalToken);
var
  LCapacity: Integer;
begin

  if FTokenCount >= FOptions.MaximumTokens then
  begin
    raise ENyxPascalLexicalLimit.Create('Pascal tokens exceed their lexical budget');
  end;

  if FTokenCount = Length(FTokens) then
  begin
    LCapacity := FOptions.MaximumTokens;

    if 256 + FTokenCount div 2 <= FOptions.MaximumTokens - FTokenCount then
    begin
      LCapacity := FTokenCount + 256 + FTokenCount div 2;
    end;
    SetLength(FTokens, LCapacity);
  end;
  FTokens[FTokenCount] := AToken;
  Inc(FTokenCount);
end;

procedure TLexicalSnapshot.WordToken(var AToken: TNyxPascalToken);
var
  LWord: TNyxText;
  LOrdinal: Integer;
begin
  AToken.FKind := ptkWord;
  Consume;
  while IdentifierPart(Current) do
  begin
    Consume;
  end;

  if not AToken.FEscaped then
  begin
    LWord := NyxPascalASCIIKey(Copy(FSource, AToken.FStorage, FStorage - AToken.FStorage));
    LOrdinal := GWords.IndexOf(LWord);

    if LOrdinal >= 0 then
    begin
      AToken.FWord := TNyxPascalWord(LOrdinal);
    end;
  end;
end;

function DigitValue(AScalar: Integer): Integer;
begin
  Result := -1;

  if (AScalar >= Ord('0')) and (AScalar <= Ord('9')) then
  begin
    Result := AScalar - Ord('0');
  end
  else if (AScalar >= Ord('A')) and (AScalar <= Ord('F')) then
  begin
    Result := AScalar - Ord('A') + 10;
  end
  else if (AScalar >= Ord('a')) and (AScalar <= Ord('f')) then
  begin
    Result := AScalar - Ord('a') + 10;
  end;
end;

procedure TLexicalSnapshot.NumberToken(var AToken: TNyxPascalToken; ACharacter: Boolean);
const
  CBases: array[TNyxPascalNumberBase] of Integer = (10, 16, 8, 2);
var
  LBase: Integer;
  LCount: Integer;
  LDigit: Integer;
  LInvalid: Boolean;
begin
  AToken.FKind := ptkNumber;

  if ACharacter then
  begin
    AToken.FKind := ptkCharacter;
    Consume;
  end;
  case Current of
    Ord('$'):
      begin
        AToken.FBase := pnbHexadecimal;
      end;
    Ord('&'):
      begin
        AToken.FBase := pnbOctal;
      end;
    Ord('%'):
      begin
        AToken.FBase := pnbBinary;
      end;
  end;

  if AToken.FBase <> pnbDecimal then
  begin
    Consume;
  end;
  LBase := CBases[AToken.FBase];
  LCount := 0;
  LInvalid := False;
  while True do
  begin
    LDigit := DigitValue(Current);

    if (LDigit < 0) or ((LBase = 10) and (LDigit >= 10)) then
    begin
      Break;
    end;
    LInvalid := LInvalid or (LDigit >= LBase);
    Inc(LCount);
    Consume;
  end;

  if LCount = 0 then
  begin
    AToken.FComplete := False;
    Issue(pliMissingDigits, AToken.FSpan.Start);
  end
  else if LInvalid then
  begin
    AToken.FComplete := False;
    Issue(pliInvalidDigit, AToken.FSpan.Start);
  end;

  if (AToken.FBase <> pnbDecimal) or ACharacter then
  begin
    Exit;
  end;

  if (Current = Ord('.')) and not NextASCII('.') and
    (FStorage < Length(FSource)) and (FSource[FStorage + 1] in ['0'..'9']) then
  begin
    AToken.FReal := True;
    Consume;
    while (Current >= Ord('0')) and (Current <= Ord('9')) do
    begin
      Consume;
    end;
  end;

  if (Current = Ord('e')) or (Current = Ord('E')) then
  begin
    AToken.FReal := True;
    Consume;

    if (Current = Ord('+')) or (Current = Ord('-')) then
    begin
      Consume;
    end;
    LCount := 0;
    while (Current >= Ord('0')) and (Current <= Ord('9')) do
    begin
      Inc(LCount);
      Consume;
    end;

    if LCount = 0 then
    begin
      AToken.FComplete := False;
      Issue(pliMissingExponent, AToken.FSpan.Start);
    end;
  end;
end;

procedure TLexicalSnapshot.StringToken(var AToken: TNyxPascalToken);
begin
  AToken.FKind := ptkString;
  Consume;
  while Current >= 0 do
  begin

    if Current in [10, 13] then
    begin
      Break;
    end;

    if Current = 39 then
    begin
      Consume;

      if Current <> 39 then
      begin
        Exit;
      end;
    end;
    Consume;
  end;
  AToken.FComplete := False;
  Issue(pliUnterminatedString, AToken.FSpan.Start);
end;

procedure TLexicalSnapshot.CommentToken(var AToken: TNyxPascalToken;
  ADelimiter: TCommentDelimiter);
var
  LStack: array of TCommentDelimiter;
  LDepth: Integer;
begin
  AToken.FKind := ptkComment;
  Consume;

  if ADelimiter = pcdParenthesis then
  begin
    Consume;
  end;

  if Current = Ord('$') then
  begin
    AToken.FKind := ptkDirective;
  end;
  SetLength(LStack, 1);
  LStack[0] := ADelimiter;
  LDepth := 1;
  while Current >= 0 do
  begin

    if ((LStack[LDepth - 1] = pcdBrace) and (Current = Ord('}'))) or
      ((LStack[LDepth - 1] = pcdParenthesis) and (Current = Ord('*')) and NextASCII(')')) then
    begin
      Consume;

      if LStack[LDepth - 1] = pcdParenthesis then
      begin
        Consume;
      end;
      Dec(LDepth);

      if LDepth = 0 then
      begin
        Exit;
      end;
      Continue;
    end;

    if FOptions.AllowNestedComments and ((Current = Ord('{')) or
      ((Current = Ord('(')) and NextASCII('*'))) then
    begin

      if LDepth >= FOptions.MaximumCommentDepth then
      begin
        raise ENyxPascalLexicalLimit.Create('Pascal comment nesting exceeds its lexical budget');
      end;

      if LDepth = Length(LStack) then
      begin
        SetLength(LStack, LDepth + 1);
      end;
      LStack[LDepth] := pcdBrace;

      if Current = Ord('(') then
      begin
        LStack[LDepth] := pcdParenthesis;
        Consume;
      end;
      Consume;
      Inc(LDepth);
      Continue;
    end;
    Consume;
  end;
  AToken.FComplete := False;
  Issue(pliUnterminatedComment, AToken.FSpan.Start);
end;

procedure TLexicalSnapshot.Build;
var
  LToken: TNyxPascalToken;
  LAt: Integer;
  LScalar: Integer;
  LOrdinal: Integer;
  LPair: TNyxText;
begin
  repeat
    CheckCancellation;
    LScalar := Current;
    LToken := Default(TNyxPascalToken);
    LToken.FSpan := NyxPascalSpan(FOffset, 0);
    LToken.FStorage := FStorage;
    LToken.FLine := FLine;
    LToken.FColumn := FColumn;
    LToken.FComplete := True;

    if LScalar < 0 then
    begin
      LToken.FKind := ptkEOF;
      Add(LToken);
      Break;
    end;

    if (LScalar = $FEFF) and (FOffset = 0) then
    begin
      { A physical UTF-8 BOM remains lossless trivia only at the source start. }
      LToken.FKind := ptkWhitespace;
      Consume;
    end
    else if LScalar in [9, 10, 11, 12, 13, 32] then
    begin
      LToken.FKind := ptkWhitespace;
      repeat
        Consume;
      until not (Current in [9, 10, 11, 12, 13, 32]);
    end
    else if IdentifierStart(LScalar) then
    begin
      WordToken(LToken);
    end
    else if (LScalar = Ord('&')) and (FStorage < Length(FSource)) then
    begin
      LAt := FStorage + 1;

      if not NyxNextScalar(FSource, LAt, LScalar) then
      begin
        raise ENyxPascalLexical.Create('Pascal source contains malformed Unicode');
      end;

      if IdentifierStart(LScalar) then
      begin
        LToken.FEscaped := True;
        Consume;
        WordToken(LToken);
      end
      else
      begin
        NumberToken(LToken, False);
      end;
    end
    else if (LScalar >= Ord('0')) and (LScalar <= Ord('9')) or
      (LScalar = Ord('$')) or (LScalar = Ord('%')) then
    begin
      NumberToken(LToken, False);
    end
    else if LScalar = Ord('#') then
    begin
      NumberToken(LToken, True);
    end
    else if LScalar = 39 then
    begin
      StringToken(LToken);
    end
    else if LScalar = Ord('{') then
    begin
      CommentToken(LToken, pcdBrace);
    end
    else if (LScalar = Ord('(')) and NextASCII('*') then
    begin
      CommentToken(LToken, pcdParenthesis);
    end
    else if (LScalar = Ord('/')) and NextASCII('/') then
    begin
      LToken.FKind := ptkComment;
      while (Current >= 0) and not (Current in [10, 13]) do
      begin
        Consume;
      end;
    end
    else
    begin
      LOrdinal := -1;

      if LScalar < 128 then
      begin
        LPair := Copy(FSource, FStorage, 2);
        LOrdinal := GSymbols.IndexOf(LPair);
      end;

      if LOrdinal > 0 then
      begin
        Consume;
        Consume;
      end
      else
      begin
        LOrdinal := GSymbols.IndexOf(NyxScalarText(LScalar));
        Consume;
      end;

      if LOrdinal > 0 then
      begin
        LToken.FKind := ptkSymbol;
        LToken.FSymbol := TNyxPascalSymbol(LOrdinal);
      end
      else
      begin
        LToken.FKind := ptkInvalid;
        LToken.FComplete := False;
        Issue(pliUnexpectedScalar, LToken.FSpan.Start);
      end;
    end;
    LToken.FSpan := NyxPascalSpan(LToken.FSpan.Start, FOffset - LToken.FSpan.Start);
    LToken.FStorageLength := FStorage - LToken.FStorage;
    Add(LToken);
  until False;
  SetLength(FTokens, FTokenCount);
  SetLength(FDiagnostics, FDiagnosticCount);
end;

function TLexicalSnapshot.Source: TNyxText;
begin
  Result := FSource;
end;

function TLexicalSnapshot.Options: TNyxPascalLexicalOptions;
begin
  Result := FOptions;
end;

function TLexicalSnapshot.Scalars: Integer;
begin
  Result := FOffset;
end;

function TLexicalSnapshot.Count: Integer;
begin
  Result := FTokenCount;
end;

function TLexicalSnapshot.Token(AIndex: Integer): TNyxPascalToken;
begin

  if (AIndex < 0) or (AIndex >= FTokenCount) then
  begin
    raise ENyxPascalLexical.Create('Pascal token index is outside its snapshot');
  end;
  Result := FTokens[AIndex];
end;

function TLexicalSnapshot.Text(AIndex: Integer): TNyxText;
var
  LToken: TNyxPascalToken;
begin
  LToken := Token(AIndex);
  Result := Copy(FSource, LToken.FStorage, LToken.FStorageLength);
end;

function TLexicalSnapshot.Identifier(AIndex: Integer): TNyxText;
var
  LToken: TNyxPascalToken;
begin
  LToken := Token(AIndex);

  if LToken.Kind <> ptkWord then
  begin
    raise ENyxPascalLexical.Create('Pascal identifier requires a word token');
  end;
  Result := Text(AIndex);

  if LToken.Escaped then
  begin
    Delete(Result, 1, 1);
  end;
end;

function TLexicalSnapshot.TokenAt(AOffset: Integer): Integer;
var
  LLow: Integer;
  LHigh: Integer;
  LMid: Integer;
begin

  if (AOffset < 0) or (AOffset > FOffset) then
  begin
    raise ENyxPascalLexical.Create('Pascal scalar offset is outside its snapshot');
  end;
  LLow := 0;
  LHigh := FTokenCount - 1;
  Result := LHigh;
  while LLow <= LHigh do
  begin
    LMid := LLow + (LHigh - LLow) div 2;

    if FTokens[LMid].Span.Start <= AOffset then
    begin
      Result := LMid;
      LLow := LMid + 1;
    end
    else
    begin
      LHigh := LMid - 1;
    end;
  end;
end;

function TLexicalSnapshot.Slice(const ASpan: TNyxPascalSpan): TNyxText;
var
  LToken: TNyxPascalToken;
  LStart: Integer;
  LFinish: Integer;
  LScalar: Integer;
  LAt: Integer;
begin

  if ASpan.Finish > FOffset then
  begin
    raise ENyxPascalLexical.Create('Pascal span is outside its exact source');
  end;
  LToken := Token(TokenAt(ASpan.Start));
  LStart := LToken.FStorage;
  for LAt := LToken.Span.Start to ASpan.Start - 1 do
  begin
    NyxNextScalar(FSource, LStart, LScalar);
  end;
  LToken := Token(TokenAt(ASpan.Finish));
  LFinish := LToken.FStorage;
  for LAt := LToken.Span.Start to ASpan.Finish - 1 do
  begin
    NyxNextScalar(FSource, LFinish, LScalar);
  end;
  Result := Copy(FSource, LStart, LFinish - LStart);
end;

function TLexicalSnapshot.DiagnosticCount: Integer;
begin
  Result := FDiagnosticCount;
end;

function TLexicalSnapshot.Diagnostic(AIndex: Integer): TNyxPascalLexicalDiagnostic;
begin

  if (AIndex < 0) or (AIndex >= FDiagnosticCount) then
  begin
    raise ENyxPascalLexical.Create('Pascal diagnostic index is outside its snapshot');
  end;
  Result := FDiagnostics[AIndex];
end;

function NewNyxPascalLexicalSnapshot(const ASource: TNyxText): INyxPascalLexicalSnapshot;
begin
  Result := NewNyxPascalLexicalSnapshot(ASource, TNyxPascalLexicalOptions.Defaults);
end;

function NewNyxPascalLexicalSnapshot(const ASource: TNyxText;
  const AOptions: TNyxPascalLexicalOptions;
  const ACancellation: INyxCancellationScope): INyxPascalLexicalSnapshot;
begin
  Result := TLexicalSnapshot.Create(ASource, AOptions, ACancellation);
end;

procedure InitializeVocabulary;
var
  LWord: TNyxPascalWord;
  LSymbol: TNyxPascalSymbol;
begin
  GWords := TNyxTextIndex.Create;
  GSymbols := TNyxTextIndex.Create;
  for LWord := Succ(Low(TNyxPascalWord)) to High(TNyxPascalWord) do
  begin
    GWords.AddFirst(CWords[LWord], Ord(LWord));
  end;
  for LSymbol := Succ(Low(TNyxPascalSymbol)) to High(TNyxPascalSymbol) do
  begin
    GSymbols.AddFirst(CSymbols[LSymbol], Ord(LSymbol));
  end;
end;

initialization
  InitializeVocabulary;
{$ifndef PAS2JS}
finalization
  GSymbols.Free;
  GWords.Free;
{$endif}
{ pas2js has no unit finalization. Its immutable vocabulary lives for the loaded
  module's lifetime; it retains no analyzed source, caller or cancellation scope. }
end.
