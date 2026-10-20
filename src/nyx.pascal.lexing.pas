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
    zero-length EOF. Assembly operands stay opaque rather than acquiring Pascal
    number/word semantics. Invalid Pascal scalars remain visible. }
  TNyxPascalTokenKind = (ptkWord, ptkNumber, ptkString, ptkCharacter,
    ptkSymbol, ptkWhitespace, ptkComment, ptkDirective, ptkInvalid, ptkEOF,
    ptkAssembly);
  { Lexical envelope, not instruction semantics. Assembly bodies retain opaque
    spelling while Pascal comments/directives and the terminating END stay
    visible. The current envelope follows FPC's x86 Intel/AT&T readers; other
    assembler comment/register-list dialects need explicit qualification. }
  TNyxPascalLexicalRegion = (plrPascal, plrAssembly);
  { Numeric spelling only, never a lossy machine-number conversion or compiler
    admission. Decimal exponents/fractions are marked separately on their token. }
  TNyxPascalNumberBase = (pnbDecimal, pnbHexadecimal, pnbOctal, pnbBinary);
  { Closed recoverable lexical failures. Messages are stable English guidance;
    the exact source and scalar span remain available separately. }
  TNyxPascalLexicalIssue = (pliUnexpectedScalar, pliUnterminatedString,
    pliUnterminatedComment, pliMissingDigits, pliInvalidDigit, pliMissingExponent,
    pliUnterminatedAssembly);
  { Detached diagnostic for one source span. It owns no source or mutable array. }
  TNyxPascalLexicalDiagnostic = record
  private
    FIssue: TNyxPascalLexicalIssue;
    FSpan: TNyxPascalSpan;
  public
    function MessageText: TNyxText;
    { Rebase a fragment diagnostic into its owning immutable source. }
    function Rebased(AScalarOffset: Integer): TNyxPascalLexicalDiagnostic;
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
    FRegion: TNyxPascalLexicalRegion;
  public
    { Rebase local fragment coordinates without changing classification. Offsets
      are nonnegative; the column offset applies only to the first local line.
      Storage uses target String units, independently of Unicode scalar spans. }
    function Rebased(AScalarOffset, AStorageOffset, ALineOffset,
      AColumnOffset: Integer): TNyxPascalToken;
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
    { Region on entry to this token; ASM enters assembly for its successor and
      terminating END leaves assembly for its successor. Includes retain this. }
    property Region: TNyxPascalLexicalRegion read FRegion;
  end;
  { Detached proof binding one owned scanner token to its exact spelling and
    identifier policy. Only complete self-contained Pascal atoms qualify; ASM,
    invalid/incomplete tokens and EOF-sensitive ampersands keep normal scanning.
    No source/reader/interface survives in this value. Default means unavailable,
    and raw token fields or another implementation's counters cannot create it. }
  TNyxPascalPreparedToken = record
  private
    FToken: TNyxPascalToken;
    FText: TNyxText;
    FUnicode: Boolean;
    FReady: Boolean;
  public
    function Eligible(const AOptions: TNyxPascalLexicalOptions): Boolean;
    function AtPosition(AScalar, AStorage, ALine, AColumn: Integer;
      const AOptions: TNyxPascalLexicalOptions): TNyxPascalToken;
    property Available: Boolean read FReady;
    property Token: TNyxPascalToken read FToken;
    property Text: TNyxText read FText;
  end;
  { Projection introduces only a single ASCII space or zero-length EOF. These
    closed boundary kinds cannot classify caller-supplied text or source code. }
  TNyxPascalProjectionBoundary = (ppbSpace, ppbEOF);
  { Owns exact physical text and ordered immutable tokens/diagnostics. Every
    storage unit belongs to a token; EOF has no units. Getters borrow no caller,
    cancellation scope, file, GUI or mutable buffer. Returned records are values. }
  INyxPascalLexicalSnapshot = interface(IInterface)
    ['{98349E7C-C0ED-48DE-A518-427A04570B01}']
    function Source: TNyxText;
    { Initial policy and fixed budgets; contextual construction may change
      comment nesting between tokens without changing this captured value. }
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
  { Immutable text value with a sealed in-process owner. Equal text from another
    owner grants no token reuse. This owns no file/codec/compiler facts, observer
    or execution scope; alternate implementations are copied before scanning. }
  INyxPascalLexicalInput = interface(IInterface)
    ['{75E546E7-3056-4149-B2C9-7E434A4A91AA}']
    function Text: TNyxText;
  end;
  { Detached physical construction work. Reused rows still invoke every current
    observer callback. Reads include rejected candidates; these counters confer
    neither input authority nor old conditional/compiler meaning. }
  INyxPascalContextualLexicalWork = interface(IInterface)
    ['{9AB5903E-04A9-497B-A093-6CF161CD83C4}']
    function ScannedTokens: Integer;
    function ReusedTokens: Integer;
    function CandidateReads: Integer;
  end;
  { Detached tokens already scanned at a construction boundary. The exact source
    is retained, but its unseen suffix grants no lexical authority. There is no
    manufactured EOF and this deliberately is not a complete lexical snapshot.
    Frozen values own no reader, visitor, cancellation scope or mutable vector. }
  INyxPascalLexicalPrefix = interface(IInterface)
    ['{5BA41120-48A0-4B45-B021-71F0517C6980}']
    function Source: TNyxText;
    function ScannedScalars: Integer;
    function Count: Integer;
    function Token(AIndex: Integer): TNyxPascalToken;
    function Text(AIndex: Integer): TNyxText;
    function DiagnosticCount: Integer;
    function Diagnostic(AIndex: Integer): TNyxPascalLexicalDiagnostic;
  end;
  { Optional immutable token-arena receipt. Copied/shared rows describe this
    freeze, including a translation wrapper's exclusive boundary token when its
    underlying arena retains one. Directory entries are copied references to
    fixed-size immutable blocks; diagnostics and other metadata remain separate.
    These storage counts grant no syntax-reuse authority or reduced work charge. }
  INyxPascalPrefixTokenStorage = interface(IInterface)
    ['{C529230E-877F-4819-B322-53B1F6F262E1}']
    function TokenRows: Integer;
    function CopiedTokenRows: Integer;
    function SharedTokenRows: Integer;
    function DirectoryEntries: Integer;
  end;
  { Revocable construction borrow. Freeze retains the already scanned prefix at
    the current synchronous callback, sharing only immutable complete blocks and
    copying the unfinished block; after the reader retires, Freeze refuses.
    Retaining this capability does not keep its borrowed lexer alive. }
  INyxPascalLexicalPrefixCapture = interface(IInterface)
    ['{16A6A7AC-A9B2-4375-84A1-47B64F0E47C8}']
    function Freeze: INyxPascalLexicalPrefix;
  end;
  { Optional visitor lifecycle, used only by source-aware translation. The
    capture may be retained during synchronous token visits; detached prefixes
    survive independently. Ordinary observers keep their existing contract. }
  INyxPascalLexicalConstructionObserver = interface(IInterface)
    ['{40EF33CA-E132-4504-9275-0550630BF134}']
    procedure BeginRead(const ACapture: INyxPascalLexicalPrefixCapture);
  end;
  { Synchronous construction-only visitor. Each complete token (including EOF)
    arrives once before scanning its successor. A translator may change the next
    block comment's nesting policy after a directive or textual include. The
    visitor never receives a mutable snapshot or controls quotas/source text;
    exceptions abort construction. Retained only until the factory returns.
    Its identity is distinct from a declared syntax snapshot: a visitor cannot
    be queried as source data. Rebuild consumers for the corrected identity. }
  INyxPascalLexicalObserver = interface(IInterface)
    ['{031EA0B8-3AD2-4E13-9CEA-5B067B7B8D67}']
    procedure AfterToken(AIndex: Integer; const AToken: TNyxPascalToken;
      const AText: TNyxText; var AAllowNestedComments: Boolean);
  end;
  { Optional construction-only region authority on the same borrowed observer.
    Textual include fragments may start/end inside assembly. A translator owns
    branch activity and selects the next region after its ordinary token visit;
    absent this capability the lexer follows physical ASM/END boundaries itself.
    No mutable snapshot or source text escapes through this capability. }
  INyxPascalLexicalRegionObserver = interface(IInterface)
    ['{98349E7C-C0ED-48DE-A518-427A04570B07}']
    function InitialRegion: TNyxPascalLexicalRegion;
    function NextRegion: TNyxPascalLexicalRegion;
  end;
  { Fixed-policy synchronous scan destination. Each token, including EOF, is
    emitted once with its exact local spelling and zero-based UTF-8 byte/storage
    starts. Diagnostics may arrive before their enclosing token. The scanner's
    token/diagnostic admission covers producing this final row; a receiver that
    performs additional work owns its separate admission. Callbacks are provisional
    until the scan returns successfully and may abort by raising. No reader,
    context, capture, mutable vector or policy authority crosses this boundary. }
  INyxPascalLexicalFragmentReceiver = interface(IInterface)
    ['{32CFE8F1-B75E-477D-9AC5-0AF7C5230E48}']
    procedure ReceiveToken(const AToken: TNyxPascalToken; const ASpelling: TNyxText;
      AByteStart, AStorageStart: Integer);
    procedure ReceiveDiagnostic(const ADiagnostic: TNyxPascalLexicalDiagnostic);
  end;
  { Completed fixed-policy scan facts. Coordinates remain separate domains;
    counts include EOF. Buffered rows describe only the scanner's own vectors,
    not any receiver storage. Direct emission retains zero such rows, and this
    value holds no receiver, source, execution scope or reusable scan authority. }
  TNyxPascalLexicalScanReceipt = record
  private
    FScalars, FBytes, FStorage, FTokens, FDiagnostics: Integer;
    FBufferedTokens, FBufferedDiagnostics: Integer;
  public
    property Scalars: Integer read FScalars;
    property UTF8Bytes: Integer read FBytes;
    property StorageUnits: Integer read FStorage;
    property Tokens: Integer read FTokens;
    property Diagnostics: Integer read FDiagnostics;
    property BufferedTokenRows: Integer read FBufferedTokens;
    property BufferedDiagnosticRows: Integer read FBufferedDiagnostics;
  end;

{ Construct a checked nonnegative scalar range without associating a source. }
function NyxPascalSpan(AStart, ALength: Integer): TNyxPascalSpan;
{ Exact ASCII canonicalization for Pascal syntax words. Non-ASCII storage remains
  untouched; optional Unicode identifier equivalence is a separate semantic policy. }
function NyxPascalASCIIKey(const AText: TNyxText): TNyxText;
{ Owned scanner/prefix producers alone bind classification to spelling. Unknown
  implementations return an unavailable value and retain ordinary scanning. }
function NyxPascalPreparedToken(const ASource: INyxPascalLexicalSnapshot;
  AIndex: Integer): TNyxPascalPreparedToken; overload;
function NyxPascalPreparedToken(const ASource: INyxPascalLexicalPrefix;
  AIndex: Integer): TNyxPascalPreparedToken; overload;
{ Construct a known literal boundary at checked virtual coordinates. }
function NyxPascalProjectionBoundaryToken(AKind: TNyxPascalProjectionBoundary;
  AScalar, AStorage, ALine, AColumn: Integer): TNyxPascalToken;
{ Build a detached complete token stream. Cancellation is borrowed only during
  construction and checked at entry, each token and bounded scalar intervals.
  Malformed Unicode and quotas are hard failures; unfinished strings recover at
  the next physical newline, unfinished comments retain the remaining source.
  Physical ASM/END regions retain opaque operands and ordinary Pascal comments/
  directives; instruction validation and target assembly semantics stay separate. }
function NewNyxPascalLexicalSnapshot(const ASource: TNyxText): INyxPascalLexicalSnapshot; overload;
function NewNyxPascalLexicalSnapshot(const ASource: TNyxText;
  const AOptions: TNyxPascalLexicalOptions;
  const ACancellation: INyxCancellationScope = nil): INyxPascalLexicalSnapshot; overload;
{ Ordered lexical construction without source rewriting. Options retain the
  initial policy/budgets; individual tokens reflect visitor-approved changes.
  The detached result retains neither visitor nor cancellation scope. }
function NewNyxPascalContextualLexicalSnapshot(const ASource: TNyxText;
  const AOptions: TNyxPascalLexicalOptions; const AObserver: INyxPascalLexicalObserver;
  const ACancellation: INyxCancellationScope = nil): INyxPascalLexicalSnapshot; overload;
{ Wrap one copied immutable text value. Malformed Unicode and lexical quotas
  remain scanner-owned; this value alone grants no classified token authority. }
function NewNyxPascalLexicalInput(const AText: TNyxText): INyxPascalLexicalInput;
{ Freeze an alternate text provider once; an owned immutable input keeps its
  exact identity. No token or compiler authority is inferred by this capture. }
function CaptureNyxPascalLexicalInput(const AInput: INyxPascalLexicalInput): INyxPascalLexicalInput;
{ An exact owned input can donate completed physical rows under matching fixed
  policies and current per-row nesting/region/position. Diagnostics are rescanned.
  Every current callback runs; mismatching rows use ordinary scanning, and no old
  context/answer/observer is retained. Foreign/other input owners donate nothing. }
function NewNyxPascalContextualLexicalSnapshot(const AInput: INyxPascalLexicalInput;
  const AOptions: TNyxPascalLexicalOptions; const AObserver: INyxPascalLexicalObserver;
  const ACancellation: INyxCancellationScope = nil;
  const APrevious: INyxPascalLexicalSnapshot = nil): INyxPascalLexicalSnapshot; overload;

{ Fixed-policy fragment scanner for immutable append continuations. Coordinates
  remain local; the caller supplies the exact entry region and whether a BOM is
  at the original source start. No include/observer/compiler authority is inferred. }
function NewNyxPascalLexicalFragment(const ASource: TNyxText;
  const AOptions: TNyxPascalLexicalOptions; ARegion: TNyxPascalLexicalRegion;
  ASourceStart: Boolean;
  const ACancellation: INyxCancellationScope = nil): INyxPascalLexicalSnapshot;

{ Scan directly into an explicit receiver without constructing intermediate
  token/diagnostic vectors. Uses exactly the fixed-policy fragment scanner,
  source-start/region rules and quotas above. The receiver is borrowed only for
  this call, including refusal/cancellation. No partial receipt is returned. }
function ScanNyxPascalLexicalFragment(const ASource: TNyxText;
  const AOptions: TNyxPascalLexicalOptions; ARegion: TNyxPascalLexicalRegion;
  ASourceStart: Boolean; const AReceiver: INyxPascalLexicalFragmentReceiver;
  const ACancellation: INyxCancellationScope = nil): TNyxPascalLexicalScanReceipt;

implementation

uses nyx.text.index, nyx.unicode, nyx.unicode.types, nyx.pascal.work;

const
  { Bounds tail recopying without a recursive chain or a mutable shared vector. }
  CLexicalPrefixBlockSize = 256;
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
  { A read-only text owner, deliberately independent of any byte/source view.
    Copy-on-write target strings keep later caller edits outside this value. }
  TLexicalInput = class(TInterfacedObject, INyxPascalLexicalInput)
  public
    Value: TNyxText;
    function Text: TNyxText;
  end;
  { Physical cursor after one token plus its entry comment policy. A diagnostic
    row is never replayed. Values contain no interface, source or reader borrow;
    token region/starting coordinates remain in the ordinary token itself. }
  TContextualTokenState = record
    Bytes, Line, Column: Integer;
    Nested: Boolean;
    HasDiagnostics: Boolean;
  end;
  { Active nested block-comment delimiter, never a compiler conditional scope. }
  TCommentDelimiter = (pcdBrace, pcdParenthesis);
  { In-unit ownership proof. A prepared value is read from the same immutable
    token/text storage, never supplied by a public classification counter. }
  IPreparedTokenOwner = interface(IInterface)
    ['{BF95FB2D-A625-491F-A895-3DF2182A6931}']
    function PreparedToken(AIndex: Integer): TNyxPascalPreparedToken;
  end;
  { Read-only block lease. A live lexer alone fills its unpublished tail; full
    blocks seal before sharing. Value rows retain no interfaces or reader. }
  ILexicalTokenBlock = interface(IInterface)
    function Token(AIndex: Integer): TNyxPascalToken;
    function State(AIndex: Integer): TContextualTokenState;
  end;
  { Independent directories may share completed blocks; their interface slots
    are copied explicitly rather than sharing a mutable dynamic-array vector. }
  TLexicalTokenBlocks = array of ILexicalTokenBlock;
  { Written directly during scanning, then sealed before sharing. A frozen
    partial block is a separate copy, never the lexer's unfinished writer. }
  TLexicalTokenBlock = class(TInterfacedObject, ILexicalTokenBlock)
  public
    Rows: array of TNyxPascalToken;
    { Only complete contextual readers fill these bounded parallel value rows.
      Frozen partial tails need only tokens and have no replay authority. }
    States: array of TContextualTokenState;
    function Token(AIndex: Integer): TNyxPascalToken;
    function State(AIndex: Integer): TContextualTokenState;
  end;
  { Owns exact text/vectors. Cursor and observer/cancellation borrows exist only
    during Build; publication retains initial options and contextually scanned
    tokens, releasing both external interfaces. No partial snapshot escapes. }
  TLexicalSnapshot = class(TInterfacedObject, INyxPascalLexicalSnapshot, IPreparedTokenOwner,
    INyxPascalContextualLexicalWork)
  private
    FSource: TNyxText;
    FInput: INyxPascalLexicalInput;
    { Borrowed only while the factory's previous-snapshot lease remains held.
      Publication retires this pointer; no predecessor chain survives. }
    FDonor: TLexicalSnapshot;
    FDonorAt: Integer;
    FPublished: Boolean;
    FRegionControlled: Boolean;
    FTokenNested: Boolean;
    FTokenDiagnostics: Integer;
    FScanned, FReused, FCandidateReads: Integer;
    FOptions: TNyxPascalLexicalOptions;
    { Actual token storage, not a freeze cache. Only the last unpublished page
      can be written; completed pages are immutable from their first sharing. }
    FTokenBlocks: TLexicalTokenBlocks;
    { Borrowed from this owner's directory during construction. Sealing a full
      page or retiring the reader revokes the only write path to that page. }
    FWriting: TLexicalTokenBlock;
    FDiagnostics: array of TNyxPascalLexicalDiagnostic;
    FTokenCount: Integer;
    FDiagnosticCount: Integer;
    FStorage: Integer;
    FOffset: Integer;
    FLine: Integer;
    FColumn: Integer;
    FBytes: Integer;
    FTokenByteStart: Integer;
    FPreviousCR: Boolean;
    FSourceStart: Boolean;
    FNested: Boolean;
    FObserver: INyxPascalLexicalObserver;
    FRegionObserver: INyxPascalLexicalRegionObserver;
    FRegion: TNyxPascalLexicalRegion;
    { An alternative construction destination; it never coexists with observers
      or prefix capture and retires on every constructor exit. }
    FReceiver: INyxPascalLexicalFragmentReceiver;
    FCancellation: INyxCancellationScope;
    function Current: Integer;
    function NextASCII(AChar: Char): Boolean;
    function IdentifierStart(AScalar: Integer): Boolean;
    function IdentifierPart(AScalar: Integer): Boolean;
    procedure CheckCancellation;
    procedure SetRegion(AValue: TNyxPascalLexicalRegion);
    procedure Consume;
    procedure Issue(AIssue: TNyxPascalLexicalIssue; AStart: Integer);
    procedure Add(const AToken: TNyxPascalToken);
    procedure WordToken(var AToken: TNyxPascalToken);
    procedure NumberToken(var AToken: TNyxPascalToken; ACharacter: Boolean);
    procedure StringToken(var AToken: TNyxPascalToken);
    procedure AssemblyToken(var AToken: TNyxPascalToken);
    procedure CommentToken(var AToken: TNyxPascalToken; ADelimiter: TCommentDelimiter);
    function TryReuse(out AToken: TNyxPascalToken): Boolean;
    procedure Build;
  public
    constructor Create(const ASource: TNyxText; const AOptions: TNyxPascalLexicalOptions;
      const ACancellation: INyxCancellationScope;
      const AObserver: INyxPascalLexicalObserver = nil;
      ARegion: TNyxPascalLexicalRegion = plrPascal; ASourceStart: Boolean = True;
      const AReceiver: INyxPascalLexicalFragmentReceiver = nil;
      const AInput: INyxPascalLexicalInput = nil; ADonor: TLexicalSnapshot = nil);
    function ScannedTokens: Integer;
    function ReusedTokens: Integer;
    function CandidateReads: Integer;
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
    function PreparedToken(AIndex: Integer): TNyxPascalPreparedToken;
  end;
  { Retains admitted rows through shared complete blocks and one private tail.
    The directory and diagnostics are detached; later scanning cannot change
    this view, and indexed access has no recursive snapshot-chain cost. }
  TFrozenLexicalPrefix = class(TInterfacedObject, INyxPascalLexicalPrefix,
    INyxPascalPrefixTokenStorage, IPreparedTokenOwner)
  private
    FSource: TNyxText;
    FOptions: TNyxPascalLexicalOptions;
    FScalars: Integer;
    FBlocks: TLexicalTokenBlocks;
    FCount: Integer;
    FShared: Integer;
    FDiagnostics: array of TNyxPascalLexicalDiagnostic;
  public
    constructor Create(AReader: TLexicalSnapshot);
    function Source: TNyxText;
    function ScannedScalars: Integer;
    function Count: Integer;
    function Token(AIndex: Integer): TNyxPascalToken;
    function Text(AIndex: Integer): TNyxText;
    function DiagnosticCount: Integer;
    function Diagnostic(AIndex: Integer): TNyxPascalLexicalDiagnostic;
    function TokenRows: Integer;
    function CopiedTokenRows: Integer;
    function SharedTokenRows: Integer;
    function DirectoryEntries: Integer;
    function PreparedToken(AIndex: Integer): TNyxPascalPreparedToken;
  end;
  { Owns no lexer reference. The constructor's local lease guarantees this
    bridge is detached on success and every exception path before retirement. }
  TLexicalPrefixCapture = class(TInterfacedObject, INyxPascalLexicalPrefixCapture)
  public
    Reader: TLexicalSnapshot;
    function Freeze: INyxPascalLexicalPrefix;
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

function TNyxPascalPreparedToken.Eligible(const AOptions: TNyxPascalLexicalOptions): Boolean;
begin
  AOptions.Validate;
  Result := FReady and (FUnicode = AOptions.AllowUnicodeIdentifiers);
end;

function TNyxPascalPreparedToken.AtPosition(AScalar, AStorage, ALine,
  AColumn: Integer; const AOptions: TNyxPascalLexicalOptions): TNyxPascalToken;
begin

  if not Eligible(AOptions) or (AScalar < 0) or (AStorage < 0) or
    (ALine < 1) or (AColumn < 1) or (AStorage >= High(Integer)) or
    (Length(FText) > High(Integer) - AStorage) or
    (FToken.Span.Length > High(Integer) - AColumn) then
  begin
    raise ENyxPascalLexical.Create('Prepared token needs matching policy and checked coordinates');
  end;
  Result := FToken;
  Result.FSpan := NyxPascalSpan(AScalar, FToken.Span.Length);
  Result.FStorage := AStorage + 1;
  Result.FStorageLength := Length(FText);
  Result.FLine := ALine;
  Result.FColumn := AColumn;
end;

function NyxPascalProjectionBoundaryToken(AKind: TNyxPascalProjectionBoundary;
  AScalar, AStorage, ALine, AColumn: Integer): TNyxPascalToken;
var
  LLength: Integer;
begin
  LLength := 0;

  if AKind = ppbSpace then
  begin
    LLength := 1;
  end
  else if AKind <> ppbEOF then
  begin
    raise ENyxPascalLexical.Create('Unknown projection boundary kind');
  end;

  if (AStorage < 0) or (AStorage >= High(Integer)) or (ALine < 1) or (AColumn < 1) or
    (LLength > High(Integer) - AColumn) then
  begin
    raise ENyxPascalLexical.Create('Projection boundary coordinates are outside their domain');
  end;
  Result := Default(TNyxPascalToken);
  Result.FSpan := NyxPascalSpan(AScalar, LLength);
  Result.FStorage := AStorage + 1;
  Result.FStorageLength := LLength;
  Result.FLine := ALine;
  Result.FColumn := AColumn;
  Result.FComplete := True;
  Result.FRegion := plrPascal;
  Result.FKind := ptkEOF;

  if AKind = ppbSpace then
  begin
    Result.FKind := ptkWhitespace;
  end;
end;

{ Complete Pascal atoms are stable when separated by ASCII spaces. ASM changes
  the envelope; a bare ampersand changes family when a suffix appears after EOF.
  All recovery/opaque/trivia tokens retain the scanner's full seam behavior. }
function PrepareOwnedToken(const AToken: TNyxPascalToken; const AText: TNyxText;
  const AOptions: TNyxPascalLexicalOptions): TNyxPascalPreparedToken;
begin
  Result := Default(TNyxPascalPreparedToken);

  if not AToken.Complete or (AToken.Region <> plrPascal) or
    (AToken.Span.Length = 0) or
    not (AToken.Kind in [ptkWord, ptkNumber, ptkCharacter, ptkString, ptkSymbol]) or
    (AToken.Word = pkwAsm) or (AToken.Symbol = psoAmpersand) then
  begin
    Exit;
  end;
  Result.FToken := AToken;
  Result.FText := AText;
  Result.FUnicode := AOptions.AllowUnicodeIdentifiers;
  Result.FReady := True;
end;

function TLexicalSnapshot.PreparedToken(AIndex: Integer): TNyxPascalPreparedToken;
begin
  Result := PrepareOwnedToken(Token(AIndex), Text(AIndex), FOptions);
end;

function TFrozenLexicalPrefix.PreparedToken(AIndex: Integer): TNyxPascalPreparedToken;
begin
  Result := PrepareOwnedToken(Token(AIndex), Text(AIndex), FOptions);
end;

function NyxPascalPreparedToken(const ASource: INyxPascalLexicalSnapshot;
  AIndex: Integer): TNyxPascalPreparedToken;
var
  LOwner: IPreparedTokenOwner;
begin
  Result := Default(TNyxPascalPreparedToken);

  if Supports(ASource, IPreparedTokenOwner, LOwner) then
  begin
    Result := LOwner.PreparedToken(AIndex);
  end;
end;

function NyxPascalPreparedToken(const ASource: INyxPascalLexicalPrefix;
  AIndex: Integer): TNyxPascalPreparedToken;
var
  LOwner: IPreparedTokenOwner;
begin
  Result := Default(TNyxPascalPreparedToken);

  if Supports(ASource, IPreparedTokenOwner, LOwner) then
  begin
    Result := LOwner.PreparedToken(AIndex);
  end;
end;

function TNyxPascalToken.Rebased(AScalarOffset, AStorageOffset, ALineOffset,
  AColumnOffset: Integer): TNyxPascalToken;
begin

  if (FStorage < 1) or (FLine < 1) or (FColumn < 1) or
    (AScalarOffset < 0) or (AStorageOffset < 0) or (ALineOffset < 0) or
    (AColumnOffset < 0) or (FStorage > High(Integer) - AStorageOffset) or
    (FLine > High(Integer) - ALineOffset) or
    ((FLine = 1) and (FColumn > High(Integer) - AColumnOffset)) then
  begin
    raise ENyxPascalLexical.Create('Fragment token rebase exceeds its coordinate domain');
  end;
  Result := Self;
  Result.FSpan := NyxPascalSpan(NyxPascalSpan(AScalarOffset, FSpan.Start).Finish, FSpan.Length);
  Result.FStorage := FStorage + AStorageOffset;
  Result.FLine := FLine + ALineOffset;

  if FLine = 1 then
  begin
    Result.FColumn := FColumn + AColumnOffset;
  end;
end;

function TNyxPascalLexicalDiagnostic.Rebased(AScalarOffset: Integer): TNyxPascalLexicalDiagnostic;
begin
  Result := Self;
  Result.FSpan := NyxPascalSpan(NyxPascalSpan(AScalarOffset, FSpan.Start).Finish, FSpan.Length);
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
    'This digit is outside the literal base', 'Add digits to this exponent',
    'Finish this assembly block with END');
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

function TLexicalInput.Text: TNyxText;
begin
  Result := Value;
end;

function NewNyxPascalLexicalInput(const AText: TNyxText): INyxPascalLexicalInput;
var
  LInput: TLexicalInput;
begin
  LInput := TLexicalInput.Create;
  Result := LInput;
  LInput.Value := AText;
end;

function CaptureNyxPascalLexicalInput(const AInput: INyxPascalLexicalInput): INyxPascalLexicalInput;
var
  LInput: INyxPascalLexicalInput;
begin
  LInput := AInput;

  if LInput = nil then raise ENyxPascalLexical.Create('Contextual scanning needs an explicit immutable input');

  if LInput is TLexicalInput then
  begin

    if ((LInput as TLexicalInput) as INyxPascalLexicalInput) = LInput then Exit(LInput);
  end;
  Result := NewNyxPascalLexicalInput(LInput.Text);
end;

function SameContextualPolicy(const ALeft, ARight: TNyxPascalLexicalOptions): Boolean;
begin
  { Initial nesting can differ: each candidate checks its actual entry policy.
    All fixed quotas and identifier classification remain exactly matched. }
  Result := (ALeft.MaximumSourceBytes = ARight.MaximumSourceBytes) and
    (ALeft.MaximumTokens = ARight.MaximumTokens) and
    (ALeft.MaximumDiagnostics = ARight.MaximumDiagnostics) and
    (ALeft.MaximumCommentDepth = ARight.MaximumCommentDepth) and
    (ALeft.AllowUnicodeIdentifiers = ARight.AllowUnicodeIdentifiers);
end;

constructor TLexicalSnapshot.Create(const ASource: TNyxText;
  const AOptions: TNyxPascalLexicalOptions; const ACancellation: INyxCancellationScope;
  const AObserver: INyxPascalLexicalObserver; ARegion: TNyxPascalLexicalRegion;
  ASourceStart: Boolean; const AReceiver: INyxPascalLexicalFragmentReceiver;
  const AInput: INyxPascalLexicalInput; ADonor: TLexicalSnapshot);
var
  LObserver: INyxPascalLexicalConstructionObserver;
  LCapture: TLexicalPrefixCapture;
  LLease: INyxPascalLexicalPrefixCapture;
begin
  inherited Create;
  AOptions.Validate;

  if (AReceiver <> nil) and (AObserver <> nil) then
  begin
    raise ENyxPascalLexical.Create('Direct lexical emission cannot borrow an observer');
  end;
  FReceiver := AReceiver;
  FInput := AInput;
  FOptions := AOptions;
  FNested := AOptions.AllowNestedComments;
  FSourceStart := ASourceStart;
  SetRegion(ARegion);
  FObserver := AObserver;
  Supports(AObserver, INyxPascalLexicalRegionObserver, FRegionObserver);
  FRegionControlled := FRegionObserver <> nil;

  if FRegionObserver <> nil then
  begin
    SetRegion(FRegionObserver.InitialRegion);
  end;
  FCancellation := ACancellation;
  CheckCancellation;

  if ADonor <> nil then
  begin
    NyxPascalAdmitAnalysis(FCancellation, papSourceProof);

    if ADonor.FPublished and (FInput <> nil) and (FInput = ADonor.FInput) and
      SameContextualPolicy(FOptions, ADonor.FOptions) and
      (FSourceStart = ADonor.FSourceStart) and
      (FRegionControlled = ADonor.FRegionControlled) then FDonor := ADonor;
  end;
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
  LCapture := nil;
  try

    if Supports(AObserver, INyxPascalLexicalConstructionObserver, LObserver) then
    begin
      LCapture := TLexicalPrefixCapture.Create;
      LLease := LCapture;
      LCapture.Reader := Self;
      LObserver.BeginRead(LLease);
    end;
    Build;
  finally

    if LCapture <> nil then
    begin
      LCapture.Reader := nil;
    end;
    FWriting := nil;
    FCancellation := nil;
    FObserver := nil;
    FRegionObserver := nil;
    FReceiver := nil;
    FDonor := nil;
  end;
  FPublished := True;
end;

constructor TFrozenLexicalPrefix.Create(AReader: TLexicalSnapshot);
var
  LAt: Integer;
  LRow: Integer;
  LFirst: Integer;
  LCount: Integer;
  LBlock: TLexicalTokenBlock;
  LLease: ILexicalTokenBlock;
begin
  inherited Create;
  AReader.CheckCancellation;
  FSource := AReader.FSource;
  FOptions := AReader.FOptions;
  FScalars := AReader.FOffset;
  FCount := AReader.FTokenCount;
  FShared := FCount div CLexicalPrefixBlockSize * CLexicalPrefixBlockSize;

  if FShared > FCount then
  begin
    raise ENyxPascalLexical.Create('Frozen lexical blocks exceed their producing reader');
  end;
  { These completed pages were created by admitted lexical scanning. Sharing
    their leases performs no token-row copy, including on the very first freeze.
    Every copied directory slot and private tail row still enters the budget. }
  LCount := FCount div CLexicalPrefixBlockSize;
  SetLength(FBlocks, LCount + Ord(FCount mod CLexicalPrefixBlockSize <> 0));
  for LAt := 0 to LCount - 1 do
  begin
    AReader.CheckCancellation;
    NyxPascalAdmitAnalysis(AReader.FCancellation, papFrozenRow);
    FBlocks[LAt] := AReader.FTokenBlocks[LAt];
  end;

  if FCount mod CLexicalPrefixBlockSize <> 0 then
  begin
    LFirst := LCount * CLexicalPrefixBlockSize;
    LBlock := TLexicalTokenBlock.Create;
    LLease := LBlock;
    SetLength(LBlock.Rows, FCount - LFirst);
    for LRow := 0 to High(LBlock.Rows) do
    begin
      AReader.CheckCancellation;
      NyxPascalAdmitAnalysis(AReader.FCancellation, papFrozenRow);
      LBlock.Rows[LRow] := AReader.Token(LFirst + LRow);
    end;
    FBlocks[LCount] := LLease;
  end;
  SetLength(FDiagnostics, AReader.FDiagnosticCount);
  for LAt := 0 to High(FDiagnostics) do
  begin
    AReader.CheckCancellation;
    NyxPascalAdmitAnalysis(AReader.FCancellation, papDiagnosticRow);
    FDiagnostics[LAt] := AReader.FDiagnostics[LAt];
  end;
end;

function TLexicalTokenBlock.Token(AIndex: Integer): TNyxPascalToken;
begin

  if (AIndex < 0) or (AIndex >= Length(Rows)) then
  begin
    raise ENyxPascalLexical.Create('Lexical token is outside its immutable block');
  end;
  Result := Rows[AIndex];
end;

function TLexicalTokenBlock.State(AIndex: Integer): TContextualTokenState;
begin

  if (AIndex < 0) or (AIndex >= Length(States)) then
    raise ENyxPascalLexical.Create('A contextual row has no completed physical state');
  Result := States[AIndex];
end;

function TLexicalSnapshot.ScannedTokens: Integer;
begin Result := FScanned; end;
function TLexicalSnapshot.ReusedTokens: Integer;
begin Result := FReused; end;
function TLexicalSnapshot.CandidateReads: Integer;
begin Result := FCandidateReads; end;

function TFrozenLexicalPrefix.TokenRows: Integer;
begin
  Result := FCount;
end;

function TFrozenLexicalPrefix.CopiedTokenRows: Integer;
begin
  Result := FCount - FShared;
end;

function TFrozenLexicalPrefix.SharedTokenRows: Integer;
begin
  Result := FShared;
end;

function TFrozenLexicalPrefix.DirectoryEntries: Integer;
begin
  Result := Length(FBlocks);
end;

function TLexicalPrefixCapture.Freeze: INyxPascalLexicalPrefix;
begin

  if Reader = nil then
  begin
    raise ENyxPascalLexical.Create('The lexical construction borrow has retired');
  end;
  Result := TFrozenLexicalPrefix.Create(Reader);
end;

function TFrozenLexicalPrefix.Source: TNyxText;
begin
  Result := FSource;
end;

function TFrozenLexicalPrefix.ScannedScalars: Integer;
begin
  Result := FScalars;
end;

function TFrozenLexicalPrefix.Count: Integer;
begin
  Result := FCount;
end;

function TFrozenLexicalPrefix.Token(AIndex: Integer): TNyxPascalToken;
begin

  if (AIndex < 0) or (AIndex >= FCount) then
  begin
    raise ENyxPascalLexical.Create('Token index is outside the frozen lexical prefix');
  end;
  Result := FBlocks[AIndex div CLexicalPrefixBlockSize].Token(AIndex mod CLexicalPrefixBlockSize);
end;

function TFrozenLexicalPrefix.Text(AIndex: Integer): TNyxText;
var
  LToken: TNyxPascalToken;
begin
  LToken := Token(AIndex);
  Result := Copy(FSource, LToken.FStorage, LToken.FStorageLength);
end;

function TFrozenLexicalPrefix.DiagnosticCount: Integer;
begin
  Result := Length(FDiagnostics);
end;

function TFrozenLexicalPrefix.Diagnostic(AIndex: Integer): TNyxPascalLexicalDiagnostic;
begin

  if (AIndex < 0) or (AIndex >= Length(FDiagnostics)) then
  begin
    raise ENyxPascalLexical.Create('Diagnostic index is outside the frozen lexical prefix');
  end;
  Result := FDiagnostics[AIndex];
end;

procedure TLexicalSnapshot.CheckCancellation;
begin

  if (FCancellation <> nil) and FCancellation.Cancelled then
  begin
    raise ENyxPascalAnalysisCancelled.Create('Pascal analysis cancelled');
  end;
end;

procedure TLexicalSnapshot.SetRegion(AValue: TNyxPascalLexicalRegion);
begin

  if (Ord(AValue) < Ord(Low(TNyxPascalLexicalRegion))) or
    (Ord(AValue) > Ord(High(TNyxPascalLexicalRegion))) then
  begin
    raise ENyxPascalLexical.Create('The lexical observer returned an invalid source region');
  end;
  FRegion := AValue;
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
  LDiagnostic: TNyxPascalLexicalDiagnostic;
begin
  NyxPascalAdmitAnalysis(FCancellation, papDiagnosticRow);

  if FDiagnosticCount >= FOptions.MaximumDiagnostics then
  begin
    raise ENyxPascalLexicalLimit.Create('Pascal diagnostics exceed their lexical budget');
  end;

  if FReceiver <> nil then
  begin
    LDiagnostic.FIssue := AIssue;
    LDiagnostic.FSpan := NyxPascalSpan(AStart, FOffset - AStart);
    Inc(FDiagnosticCount);
    FReceiver.ReceiveDiagnostic(LDiagnostic);
    Exit;
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
  LAt: Integer;
  LCapacity: Integer;
  LMaximumPages: Integer;
  LBlock: TLexicalTokenBlock;
  LLease: ILexicalTokenBlock;
begin

  if FTokenCount >= FOptions.MaximumTokens then
  begin
    raise ENyxPascalLexicalLimit.Create('Pascal tokens exceed their lexical budget');
  end;

  if FReceiver <> nil then
  begin
    { Build admitted this scan before classification. Its final row goes to
      the receiver directly; no temporary token-vector row is created/copied. }
    Inc(FTokenCount);
    FReceiver.ReceiveToken(AToken, Copy(FSource, AToken.FStorage,
      AToken.FStorageLength), FTokenByteStart, AToken.FStorage - 1);
  end
  else
  begin

    if FTokenCount mod CLexicalPrefixBlockSize = 0 then
    begin
      LBlock := TLexicalTokenBlock.Create;
      LLease := LBlock;
      LAt := FOptions.MaximumTokens - FTokenCount;

      if LAt > CLexicalPrefixBlockSize then
      begin
        LAt := CLexicalPrefixBlockSize;
      end;
      SetLength(LBlock.Rows, LAt);

      if FInput <> nil then SetLength(LBlock.States, LAt);
      LAt := FTokenCount div CLexicalPrefixBlockSize;

      if LAt = Length(FTokenBlocks) then
      begin
        { Geometric directory growth avoids reallocating all earlier leases for
          every newly created page. No capacity slot is an admitted token. }
        LMaximumPages := (FOptions.MaximumTokens - 1) div CLexicalPrefixBlockSize + 1;
        LCapacity := LMaximumPages;

        if 16 + LAt div 2 <= LMaximumPages - LAt then
        begin
          LCapacity := LAt + 16 + LAt div 2;
        end;
        SetLength(FTokenBlocks, LCapacity);
      end;
      FTokenBlocks[LAt] := LLease;
      FWriting := LBlock;
    end;
    LAt := FTokenCount mod CLexicalPrefixBlockSize;
    FWriting.Rows[LAt] := AToken;

    if FInput <> nil then
    begin
      { Final bounded row emission belongs to the scan/copy admission, not
        another traversal. State precedes the current observer callback. }
      FWriting.States[LAt].Bytes := FBytes;
      FWriting.States[LAt].Line := FLine;
      FWriting.States[LAt].Column := FColumn;
      FWriting.States[LAt].Nested := FTokenNested;
      FWriting.States[LAt].HasDiagnostics := FDiagnosticCount <> FTokenDiagnostics;
    end;
    Inc(FTokenCount);

    if FTokenCount mod CLexicalPrefixBlockSize = 0 then
    begin
      { Revoke writing before the observer can freeze this completed page. }
      FWriting := nil;
    end;
  end;

  if FObserver <> nil then
  begin
    FObserver.AfterToken(FTokenCount - 1, AToken, Text(FTokenCount - 1), FNested);
  end;

  if FRegionObserver <> nil then
  begin
    SetRegion(FRegionObserver.NextRegion);
  end
  else if AToken.Word = pkwAsm then
  begin
    FRegion := plrAssembly;
  end
  else if AToken.Word = pkwEnd then
  begin
    FRegion := plrPascal;
  end;
end;

procedure TLexicalSnapshot.AssemblyToken(var AToken: TNyxPascalToken);
var
  LQuote: Integer;
begin
  { Instruction operands are intentionally opaque, including register prefixes,
    target-specific numbers and punctuation. Retain word boundaries solely to
    recognize the unquoted END delimiter; comments/directives are scanned first. }
  AToken.FKind := ptkAssembly;

  if IdentifierStart(Current) then
  begin
    WordToken(AToken);

    if AToken.Word <> pkwEnd then
    begin
      AToken.FKind := ptkAssembly;
      AToken.FWord := pkwNone;
    end;
    Exit;
  end;
  LQuote := Current;
  Consume;

  if LQuote in [34, 39] then
  begin
    while (Current >= 0) and not (Current in [10, 13]) do
    begin

      if Current = LQuote then
      begin
        Consume;
        Exit;
      end;

      if Current = Ord('\') then
      begin
        { FPC's x86 assembly readers escape the following character in either
          quote style; Pascal doubled-quote rules do not apply in this envelope. }
        Consume;

        if (Current < 0) or (Current in [10, 13]) then
        begin
          Break;
        end;
      end;
      Consume;
    end;
    AToken.FComplete := False;
    Issue(pliUnterminatedString, AToken.FSpan.Start);
  end;
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

    if FNested and ((Current = Ord('{')) or
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

function TLexicalSnapshot.TryReuse(out AToken: TNyxPascalToken): Boolean;
var
  LToken: TNyxPascalToken;
  LState: TContextualTokenState;
begin
  Result := False;

  if FDonor = nil then Exit;
  while FDonorAt < FDonor.FTokenCount do
  begin
    CheckCancellation;
    { One bounded row operation reads the old token/state and, when eligible,
      emits that value in the new directory. Rejected probes are charged too. }
    NyxPascalAdmitAnalysis(FCancellation, papLexicalRow);
    Inc(FCandidateReads);
    LToken := FDonor.Token(FDonorAt);

    if LToken.FStorage > FStorage then Exit;

    if LToken.FStorage < FStorage then
    begin
      Inc(FDonorAt);
      Continue;
    end;
    LState := FDonor.FTokenBlocks[FDonorAt div CLexicalPrefixBlockSize]
      .State(FDonorAt mod CLexicalPrefixBlockSize);
    Inc(FDonorAt);

    if LState.HasDiagnostics or (LState.Nested <> FNested) or
      (LToken.Region <> FRegion) or (LToken.Span.Start <> FOffset) or
      (LToken.Line <> FLine) or (LToken.Column <> FColumn) then Exit;

    if LState.Bytes > FOptions.MaximumSourceBytes then
      raise ENyxPascalLexicalLimit.Create('Pascal source exceeds its lexical byte budget');
    AToken := LToken;
    FStorage := LToken.FStorage + LToken.FStorageLength;
    FOffset := LToken.Span.Finish;
    FBytes := LState.Bytes;
    FLine := LState.Line;
    FColumn := LState.Column;
    FPreviousCR := (FStorage > 1) and (FSource[FStorage - 1] = #13);
    Inc(FReused);
    Exit(True);
  end;
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
    FTokenByteStart := FBytes;
    FTokenNested := FNested;
    FTokenDiagnostics := FDiagnosticCount;

    if TryReuse(LToken) then
    begin
      Add(LToken);

      if LToken.Kind = ptkEOF then Break;
      Continue;
    end;
    NyxPascalAdmitAnalysis(FCancellation, papLexicalToken);
    Inc(FScanned);
    LScalar := Current;
    LToken := Default(TNyxPascalToken);
    LToken.FSpan := NyxPascalSpan(FOffset, 0);
    LToken.FStorage := FStorage;
    LToken.FLine := FLine;
    LToken.FColumn := FColumn;
    LToken.FComplete := True;
    LToken.FRegion := FRegion;

    if LScalar < 0 then
    begin
      { Textual include fragments may legitimately end in assembly; their
        translator validates the complete root rather than each physical EOF. }

      if (FRegion = plrAssembly) and (FRegionObserver = nil) then
      begin
        Issue(pliUnterminatedAssembly, FOffset);
      end;
      LToken.FKind := ptkEOF;
      Add(LToken);
      Break;
    end;

    if (LScalar = $FEFF) and (FOffset = 0) and FSourceStart then
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
    else if (FRegion = plrAssembly) and (LScalar <> Ord('{')) and
      not ((LScalar = Ord('(')) and NextASCII('*')) and
      not ((LScalar = Ord('/')) and NextASCII('/')) then
    begin
      AssemblyToken(LToken);
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

  { Receivers and ordinary observers may cancel during EOF emission as well as
    earlier callbacks. Completion cannot publish success after either cancels;
    already retained immutable windows remain independent of that refusal. }
  CheckCancellation;

  if FReceiver = nil then
  begin
    SetLength(FDiagnostics, FDiagnosticCount);
  end;
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
  Result := FTokenBlocks[AIndex div CLexicalPrefixBlockSize].Token(
    AIndex mod CLexicalPrefixBlockSize);
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

    if Token(LMid).Span.Start <= AOffset then
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

function NewNyxPascalContextualLexicalSnapshot(const ASource: TNyxText;
  const AOptions: TNyxPascalLexicalOptions; const AObserver: INyxPascalLexicalObserver;
  const ACancellation: INyxCancellationScope): INyxPascalLexicalSnapshot;
begin

  if AObserver = nil then
  begin
    raise ENyxPascalLexical.Create('Contextual lexical construction needs an explicit observer');
  end;
  Result := NewNyxPascalContextualLexicalSnapshot(NewNyxPascalLexicalInput(ASource),
    AOptions, AObserver, ACancellation);
end;

function NewNyxPascalContextualLexicalSnapshot(const AInput: INyxPascalLexicalInput;
  const AOptions: TNyxPascalLexicalOptions; const AObserver: INyxPascalLexicalObserver;
  const ACancellation: INyxCancellationScope;
  const APrevious: INyxPascalLexicalSnapshot): INyxPascalLexicalSnapshot;
var
  LInput: INyxPascalLexicalInput;
  LPrevious: INyxPascalLexicalSnapshot;
  LDonor: TLexicalSnapshot;
begin
  LPrevious := APrevious;
  LInput := AInput;
  AOptions.Validate;

  if (ACancellation <> nil) and ACancellation.Cancelled then
    raise ENyxPascalAnalysisCancelled.Create('Contextual lexical construction cancelled');

  if AObserver = nil then raise ENyxPascalLexical.Create('Contextual lexical construction needs an explicit observer');

  if LInput = nil then raise ENyxPascalLexical.Create('Contextual scanning needs an explicit immutable input');

  LInput := CaptureNyxPascalLexicalInput(LInput);
  LDonor := nil;

  if LPrevious is TLexicalSnapshot then
  begin
    LDonor := LPrevious as TLexicalSnapshot;

    if (LDonor as INyxPascalLexicalSnapshot) <> LPrevious then LDonor := nil;
  end;
  Result := TLexicalSnapshot.Create(LInput.Text, AOptions, ACancellation, AObserver,
    plrPascal, True, nil, LInput, LDonor);
end;

function NewNyxPascalLexicalFragment(const ASource: TNyxText;
  const AOptions: TNyxPascalLexicalOptions; ARegion: TNyxPascalLexicalRegion;
  ASourceStart: Boolean; const ACancellation: INyxCancellationScope): INyxPascalLexicalSnapshot;
begin
  Result := TLexicalSnapshot.Create(ASource, AOptions, ACancellation, nil, ARegion, ASourceStart);
end;

function ScanNyxPascalLexicalFragment(const ASource: TNyxText;
  const AOptions: TNyxPascalLexicalOptions; ARegion: TNyxPascalLexicalRegion;
  ASourceStart: Boolean; const AReceiver: INyxPascalLexicalFragmentReceiver;
  const ACancellation: INyxCancellationScope): TNyxPascalLexicalScanReceipt;
var
  LReader: TLexicalSnapshot;
begin

  if AReceiver = nil then
  begin
    raise ENyxPascalLexical.Create('Direct lexical scanning needs an explicit receiver');
  end;
  LReader := TLexicalSnapshot.Create(ASource, AOptions, ACancellation, nil,
    ARegion, ASourceStart, AReceiver);
  try
    Result := Default(TNyxPascalLexicalScanReceipt);
    Result.FScalars := LReader.FOffset;
    Result.FBytes := LReader.FBytes;
    Result.FStorage := LReader.FStorage - 1;
    Result.FTokens := LReader.FTokenCount;
    Result.FDiagnostics := LReader.FDiagnosticCount;
    { Direct receivers must never acquire the ordinary token-page destination.
      Validate the observed absence instead of concealing accidental buffering. }
    if Length(LReader.FTokenBlocks) <> 0 then
    begin
      raise ENyxPascalLexical.Create('Direct lexical scanning acquired buffered token pages');
    end;
    Result.FBufferedTokens := 0;
    Result.FBufferedDiagnostics := Length(LReader.FDiagnostics);
  finally
    LReader.Free;
  end;
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
