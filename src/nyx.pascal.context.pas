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
unit nyx.pascal.context;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.text, nyx.pascal.lexing;

type
  { Invalid captured facts are caller contract failures, never compiler errors.
    Construction and fluent changes perform no I/O or source execution. }
  ENyxPascalContext = class(EArgumentException);
  { Three-valued knowledge. Unknown must never silently become an absent define,
    an inactive branch or a successful semantic inference. }
  TNyxPascalKnowledge = (ppkUnknown, ppkFalse, ppkTrue);
  { Captured compiler identity only. Unknown supports editing before optional
    output/compiler configuration; this analyzer never borrows its own compiler's
    conditional symbols or assumes these metadata grant execution. }
  TNyxPascalCompilerFamily = (ppcUnknown, ppcFPC, ppcPas2JS);
  { Explicit language grammar. Unknown preserves an unsupported source mode;
    source directives may change it independently of the analyzer's compiler. }
  TNyxPascalLanguageMode = (ppmUnknown, ppmFPC, ppmObjFPC, ppmDelphi,
    ppmDelphiUnicode, ppmTP, ppmMacPas, ppmISO, ppmExtendedPascal);
  { Grammar-sensitive switches with independently known on/off values. This list
    does not pretend to enumerate every FPC switch; unknown directives diagnose. }
  TNyxPascalLanguageFeature = (ppfAdvancedRecords, ppfNestedProcVars,
    ppfAnonymousFunctions, ppfFunctionReferences, ppfTypeHelpers,
    ppfNestedComments, ppfScopedEnums, ppfUnicodeStrings, ppfOutParameters,
    ppfReplacementMacros, ppfClasses);
  { Symbolic record policy, not a computed ABI layout. Default and C retain
    target-dependent intent; an unknown policy never supplies guessed offsets. }
  TNyxPascalRecordAlignment = (praUnknown, praDefault, praC,
    pra1, pra2, pra4, pra8, pra16, pra32);
  { Minimum enum storage, distinct from an enumeration's actual required size.
    Default retains mode/target intent until the compiler context resolves it. }
  TNyxPascalEnumStorage = (pesUnknown, pesDefault, pes1, pes2, pes4);
  { Declared set-allocation granularity. Fixed retains the compiler's historical
    small/large-set policy; these values do not compute any target set layout. }
  TNyxPascalSetPacking = (pspUnknown, pspFixed, psp1, psp2, psp4, psp8);
  { Declared default convention only. Recording it proves neither target support
    nor binary compatibility between two procedural types. }
  TNyxPascalCallingConvention = (pccUnknown, pccDefault, pccRegister,
    pccCdecl, pccPascal, pccSafeCall, pccStdCall, pccOldFPC);
  { Closed supported local short switches. Global options such as debug output
    are deliberately outside this restore set; the wire spelling stays private. }
  TNyxPascalLocalSwitch = (plsBooleanEvaluation, plsAssertions, plsLongStrings,
    plsIOChecks, plsWritableConstants, plsOverflowChecks, plsRangeChecks,
    plsStackChecks, plsTypedAddress, plsVarStrings, plsStackFrames);
  { Value-owned local and module policies, independent of renderer/ABI types.
    Zero initialization means unknown. Fluent copies retain exact symbolic
    values; they never infer a compiler's defaults or target capabilities. }
  TNyxPascalCompilerPolicies = record
  private
    FInlining: TNyxPascalKnowledge;
    FRecords: TNyxPascalRecordAlignment;
    FEnums: TNyxPascalEnumStorage;
    FSets: TNyxPascalSetPacking;
    FCalling: TNyxPascalCallingConvention;
    FSmartLinking: TNyxPascalKnowledge;
  public
    class function Unknown: TNyxPascalCompilerPolicies; static;
    function WithInlining(AValue: TNyxPascalKnowledge): TNyxPascalCompilerPolicies;
    function AlignRecords(AValue: TNyxPascalRecordAlignment): TNyxPascalCompilerPolicies;
    function StoreEnums(AValue: TNyxPascalEnumStorage): TNyxPascalCompilerPolicies;
    function PackSets(AValue: TNyxPascalSetPacking): TNyxPascalCompilerPolicies;
    function CallBy(AValue: TNyxPascalCallingConvention): TNyxPascalCompilerPolicies;
    { Module-level request; neither PUSH/POP nor a source mode change restores
      it. This records intent, not linker availability or the effective output. }
    function RequestSmartLinking(AValue: TNyxPascalKnowledge): TNyxPascalCompilerPolicies;
    function SamePolicies(const AOther: TNyxPascalCompilerPolicies): Boolean;
    procedure Validate;
    property Inlining: TNyxPascalKnowledge read FInlining;
    property RecordAlignment: TNyxPascalRecordAlignment read FRecords;
    property EnumStorage: TNyxPascalEnumStorage read FEnums;
    property SetPacking: TNyxPascalSetPacking read FSets;
    property CallingConvention: TNyxPascalCallingConvention read FCalling;
    property SmartLinkingRequested: TNyxPascalKnowledge read FSmartLinking;
  end;
  { Simple named sizeof operand, distinct from compiler defines and arbitrary
    Pascal expressions. ASCII dotted identifiers are normalized without admitting
    injected directives, calls, indexing or generic syntax. Wider operands stay
    with semantic expression resolution, never a guessed intrinsic lookup. }
  TNyxPascalSizeReference = record
  private
    FName: TNyxText;
  public
    property Name: TNyxText read FName;
  end;
  { Unqualified declaration name for conditional declared(). It is distinct
    from a compiler define and admits no expression, qualification or escape. }
  TNyxPascalDeclarationRef = record
  private
    FName: TNyxText;
  public
    property Name: TNyxText read FName;
  end;
  { No captured result, an absent name, a measured size, or a size beyond the
    portable signed-32 domain. Undeclared is not a zero-byte type. }
  TNyxPascalSizeState = (pssUnknown, pssUndeclared, pssKnown, pssOutOfRange);
  { Value-owned measurement in an explicit pristine probe mode/H scope. It is
    compiler evidence, not proof that the same name binds in authored source.
    In particular Integer and System.Integer can denote different sized types. }
  TNyxPascalSizeFact = record
  private
    FState: TNyxPascalSizeState;
    FMode: TNyxPascalLanguageMode;
    FLongStrings: TNyxPascalKnowledge;
    FBytes: Integer;
  public
    function Bytes: Integer;
    function SameFact(const AOther: TNyxPascalSizeFact): Boolean;
    property State: TNyxPascalSizeState read FState;
    property ProbeMode: TNyxPascalLanguageMode read FMode;
    property ProbeLongStrings: TNyxPascalKnowledge read FLongStrings;
  end;
  { Bounded compiler version, stored without target-dependent floating arithmetic.
    Minor/patch have two decimal digits, matching FPC_FULLVERSION's convention. }
  TNyxPascalCompilerVersion = record
  private
    FMajor: Integer;
    FMinor: Integer;
    FPatch: Integer;
  public
    class function Create(AMajor, AMinor, APatch: Integer): TNyxPascalCompilerVersion; static;
    function FullNumber: Integer;
    property Major: Integer read FMajor;
    property Minor: Integer read FMinor;
    property Patch: Integer read FPatch;
  end;
  { Open compiler symbol spelling, distinct from Pascal declarations and file
    names. ASCII comparison is case-insensitive; there is no host path or macro. }
  TNyxPascalDefineRef = record
  private
    FName: TNyxText;
  public
    property Name: TNyxText read FName;
  end;
  { A captured define can be absent, present without a value, or an exact signed
    32-bit condition integer. Unknown retains missing capture/conditional effects.
    No replacement macro text is executed or expanded by this value. }
  TNyxPascalDefineKind = (ppdUnknown, ppdUndefined, ppdDefined, ppdInteger);
  { Detached define fact. IntegerValue refuses for another kind; strings and
    general compiler constants require a future explicit semantic value model. }
  TNyxPascalDefineValue = record
  private
    FKind: TNyxPascalDefineKind;
    FInteger: Integer;
  public
    function IsDefined: TNyxPascalKnowledge;
    function IntegerValue: Integer;
    property Kind: TNyxPascalDefineKind read FKind;
  end;
  { Recoverable condition analysis, independent of compiler admission. Unknown
    names and unsupported/malformed expressions never authorize a branch choice. }
  TNyxPascalConditionIssue = (pciNone, pciUnknownSymbol, pciUnsupported, pciMalformed);
  { One detached condition result. Boolean short-circuit knowledge may be known
    despite an unknown operand; unsupported syntax always leaves Truth unknown. }
  TNyxPascalConditionResult = record
  private
    FTruth: TNyxPascalKnowledge;
    FIssue: TNyxPascalConditionIssue;
  public
    property Truth: TNyxPascalKnowledge read FTruth;
    property Issue: TNyxPascalConditionIssue read FIssue;
  end;
  { Detached requests from one bounded conditional expression. Only unknown
    compiler symbols enter Symbol; named sizeof operands stay a separate vector
    and require source-scope binding before use. Unsupported syntax publishes
    no requests. No context, source, process or callback survives construction. }
  INyxPascalConditionNeeds = interface(IInterface)
    ['{98349E7C-C0ED-48DE-A518-427A04570C10}']
    function SyntaxSupported: Boolean;
    function SymbolCount: Integer;
    function Symbol(AIndex: Integer): TNyxPascalDefineRef;
    function SizeCount: Integer;
    function SizeName(AIndex: Integer): TNyxPascalSizeReference;
  end;
  { Optional declaration requests supplement the original planning interface.
    Names retain first-use order; unsupported expressions expose no requests. }
  INyxPascalConditionDeclarations = interface(IInterface)
    ['{98349E7C-C0ED-48DE-A518-427A04570C11}']
    function DeclarationCount: Integer;
    function DeclarationName(AIndex: Integer): TNyxPascalDeclarationRef;
  end;
  { A construction-local semantic authority bound to one immutable source site.
    It resolves names through actual scope, not pristine measurement spelling.
    Unknown is explicit; returned sizes must have verified bound-type provenance.
    The parser retains this authority only until its synchronous call returns. }
  INyxPascalConditionSemantics = interface(IInterface)
    ['{98349E7C-C0ED-48DE-A518-427A04570C12}']
    function IsDeclared(const AName: TNyxPascalDeclarationRef): TNyxPascalKnowledge;
    function SizeOfName(const AName: TNyxPascalSizeReference): TNyxPascalSizeFact;
  end;
  { Immutable captured compiler facts. Missing symbols are unknown until the
    caller explicitly seals the entire define set. Short options/features have
    separate knowledge, never guessed defaults. Each fluent change owns detached
    vectors and borrows no caller, UI, process, filesystem or mutable provider.
    Versions/modes are metadata; host capture must supply actual builtin defines. }
  INyxPascalCompilerContext = interface(IInterface)
    ['{98349E7C-C0ED-48DE-A518-427A04570C01}']
    function Family: TNyxPascalCompilerFamily;
    function Version: TNyxPascalCompilerVersion;
    function Mode: TNyxPascalLanguageMode;
    function DefinesComplete: Boolean;
    function Symbol(const AName: TNyxPascalDefineRef): TNyxPascalDefineValue;
    function Option(AName: Char): TNyxPascalKnowledge;
    function Feature(AFeature: TNyxPascalLanguageFeature): TNyxPascalKnowledge;
    { Captures an explicit source/compiler fact. At most 2048 names are retained;
      closed-set absence never overwrites an explicitly unknown fact. }
    function WithSymbol(const AName: TNyxPascalDefineRef;
      AKind: TNyxPascalDefineKind; AInteger: Integer = 0): INyxPascalCompilerContext;
    function CompleteDefines(AValue: Boolean): INyxPascalCompilerContext;
    { Known short-option facts require one ASCII letter; absent options stay
      unknown. Feature facts are typed independently of command-line spelling. }
    function WithOption(AName: Char; AValue: TNyxPascalKnowledge): INyxPascalCompilerContext;
    function WithFeature(AFeature: TNyxPascalLanguageFeature;
      AValue: TNyxPascalKnowledge): INyxPascalCompilerContext;
    { A mode directive resets feature/short-option knowledge. The caller supplies
      concrete defaults afterward; none are inferred from the analyzer's mode. }
    function InMode(AValue: TNyxPascalLanguageMode): INyxPascalCompilerContext;
    { An unavailable include/unknown side effect may change every mutable fact.
      Preserve compiler identity/version metadata, revoke all inferred state. }
    function UnknownState: INyxPascalCompilerContext;
    { Exact ordered capture identity, including explicit unknown symbol names,
      options/features and compiler metadata. This is not semantic equivalence. }
    function SameFacts(const AOther: INyxPascalCompilerContext): Boolean;
    { Detached enumeration supports exact identity and future cache keys; order
      is capture order, with updates retaining the first position. }
    function SymbolCount: Integer;
    function SymbolName(AIndex: Integer): TNyxPascalDefineRef;
  end;
  { Optional typed policy capture keeps the original context interface/GUID
    intact. Returned values are detached; updates return a new context owner. }
  INyxPascalContextPolicies = interface(IInterface)
    ['{98349E7C-C0ED-48DE-A518-427A04570C07}']
    function Policies: TNyxPascalCompilerPolicies;
    function WithPolicies(const AValue: TNyxPascalCompilerPolicies): INyxPascalCompilerContext;
  end;
  { Optional named-size evidence retains exact measurement scope and ordered
    query identity. It does not export a fabricated System symbol catalog or
    authorize unqualified lookups in an application's different lexical scope. }
  INyxPascalContextSizes = interface(IInterface)
    ['{98349E7C-C0ED-48DE-A518-427A04570C09}']
    function SizeCount: Integer;
    function SizeName(AIndex: Integer): TNyxPascalSizeReference;
    function SizeFact(const AName: TNyxPascalSizeReference): TNyxPascalSizeFact;
    function WithSizeFact(const AName: TNyxPascalSizeReference;
      const AValue: TNyxPascalSizeFact): INyxPascalCompilerContext;
  end;

function NyxPascalSizeReference(const AName: TNyxText): TNyxPascalSizeReference;
{ Validate one bounded ASCII declaration identifier, without compiler-symbol or
  member-expression meaning. Reserved grammar words refuse. }
function NyxPascalDeclaration(const AName: TNyxText): TNyxPascalDeclarationRef;
function NyxPascalSizeFact(AState: TNyxPascalSizeState;
  AMode: TNyxPascalLanguageMode = ppmUnknown; ALongStrings: TNyxPascalKnowledge = ppkUnknown;
  ABytes: Integer = 0): TNyxPascalSizeFact;
{ Read/write explicit pristine measurements; neither helper resolves an authored
  type binding. Legacy contexts have no captured sizes. }
function NyxPascalCapturedSize(const AContext: INyxPascalCompilerContext;
  const AName: TNyxPascalSizeReference): TNyxPascalSizeFact;
function NyxPascalWithCapturedSize(const AContext: INyxPascalCompilerContext;
  const AName: TNyxPascalSizeReference; const AValue: TNyxPascalSizeFact): INyxPascalCompilerContext;

{ Legacy contexts carry unknown policies. Alternate values are validated before
  capture; no default is borrowed from the analyzer's own build. }
function NyxPascalCompilerPolicies(const AContext: INyxPascalCompilerContext): TNyxPascalCompilerPolicies;
function NyxPascalWithCompilerPolicies(const AContext: INyxPascalCompilerContext;
  const AValue: TNyxPascalCompilerPolicies): INyxPascalCompilerContext;
{ Friendly closed short-switch mapping; parsing source spelling is a separate
  boundary. Unknown remains a valid explicitly captured fact. }
function NyxPascalLocalSwitchOption(AValue: TNyxPascalLocalSwitch): Char;
function NyxPascalWithLocalSwitch(const AContext: INyxPascalCompilerContext;
  ASwitch: TNyxPascalLocalSwitch; AValue: TNyxPascalKnowledge): INyxPascalCompilerContext;
{ Restore supported local policies/short switches/scoped-enum state. Preserve
  current symbol facts, global mode/features and macro enablement. The actual
  compiler oracle qualifies this distinction; no replacement table is restored. }
function RestoreNyxPascalLocalSettings(const ACurrent, ASaved: INyxPascalCompilerContext): INyxPascalCompilerContext;

{ Reject empty/over-128/non-ASCII names and identifiers starting with a digit. }
function NyxPascalDefine(const AName: TNyxText): TNyxPascalDefineRef;
{ Pure metadata constructor with unknown options/features and an open define set.
  No implicit FPC/PAS2JS/platform/version define is read from this build process. }
function NewNyxPascalCompilerContext(AFamily: TNyxPascalCompilerFamily;
  const AVersion: TNyxPascalCompilerVersion;
  AMode: TNyxPascalLanguageMode): INyxPascalCompilerContext;
{ Freeze caller-serialized alternate facts without retaining their producer.
  Preserve explicit unknowns, ordered symbols, option/feature values and identity;
  duplicate names or excess facts refuse before publication. No host facts enter. }
function CaptureNyxPascalCompilerContext(
  const AContext: INyxPascalCompilerContext): INyxPascalCompilerContext;
{ Explicit truth operations preserve unknown state without target Boolean casts. }
function NyxPascalNot(AValue: TNyxPascalKnowledge): TNyxPascalKnowledge;
function NyxPascalAnd(ALeft, ARight: TNyxPascalKnowledge): TNyxPascalKnowledge;
function NyxPascalOr(ALeft, ARight: TNyxPascalKnowledge): TNyxPascalKnowledge;
{ Bounded condition grammar: defined(), exact integer comparisons, parentheses,
  Boolean not/and/or/xor and unary signs, with Pascal operator precedence.
  Named declared()/sizeof() use an explicit site-bound semantic authority;
  without it their meaning stays unknown. General expressions stay unsupported.
  At most 16 KiB/512 tokens/64 nesting levels; hard lexical quotas still refuse. }
function AnalyzeNyxPascalCondition(const AContext: INyxPascalCompilerContext;
  const AExpression: TNyxText;
  const ASemantics: INyxPascalConditionSemantics = nil): TNyxPascalConditionResult;
{ Plans only the supported conditional grammar above plus simple named sizeof
  operands. This does not evaluate sizeof, bind a type or select a branch.
  Symbols are deduplicated in first-use order, at most 256; size operands at
  most 64. Existing lexical/depth quotas apply and refuse without partial output. }
function PlanNyxPascalCondition(const AContext: INyxPascalCompilerContext;
  const AExpression: TNyxText): INyxPascalConditionNeeds;

implementation

uses nyx.text.index;

function CaptureNyxPascalCompilerContext(const AContext: INyxPascalCompilerContext): INyxPascalCompilerContext;
var
  LAt: Integer;
  LName: TNyxPascalDefineRef;
  LValue: TNyxPascalDefineValue;
  LNumber: Integer;
  LFeature: TNyxPascalLanguageFeature;
  LCaptured: INyxPascalCompilerContext;
  LPolicies: INyxPascalContextPolicies;
  LCount: Integer;
  LSizes: INyxPascalContextSizes;
  LTargetSizes: INyxPascalContextSizes;
  LSizeCount: Integer;
  LSizeName: TNyxPascalSizeReference;
begin

  if AContext = nil then
  begin
    raise ENyxPascalContext.Create('This capture needs bounded compiler facts');
  end;
  LCount := AContext.SymbolCount;

  if (LCount < 0) or (LCount > 2048) then
  begin
    raise ENyxPascalContext.Create('This capture needs bounded compiler facts');
  end;
  LCaptured := NewNyxPascalCompilerContext(AContext.Family, AContext.Version, AContext.Mode);
  for LAt := 0 to LCount - 1 do
  begin
    LName := AContext.SymbolName(LAt);
    LValue := AContext.Symbol(LName);
    LNumber := 0;

    if LValue.Kind = ppdInteger then
    begin
      LNumber := LValue.IntegerValue;
    end;
    LCaptured := LCaptured.WithSymbol(LName, LValue.Kind, LNumber);

    if LCaptured.SymbolCount <> LAt + 1 then
    begin
      raise ENyxPascalContext.Create('An alternate compiler context contains duplicate symbol names');
    end;
  end;
  LCaptured := LCaptured.CompleteDefines(AContext.DefinesComplete);
  for LAt := Ord('A') to Ord('Z') do
  begin
    LCaptured := LCaptured.WithOption(Chr(LAt), AContext.Option(Chr(LAt)));
  end;
  for LFeature := Low(TNyxPascalLanguageFeature) to High(TNyxPascalLanguageFeature) do
  begin
    LCaptured := LCaptured.WithFeature(LFeature, AContext.Feature(LFeature));
  end;
  Supports(LCaptured, INyxPascalContextPolicies, LPolicies);
  LCaptured := LPolicies.WithPolicies(NyxPascalCompilerPolicies(AContext));

  if Supports(AContext, INyxPascalContextSizes, LSizes) then
  begin
    LSizeCount := LSizes.SizeCount;

    if (LSizeCount < 0) or (LSizeCount > 64) then
    begin
      raise ENyxPascalContext.Create('Captured size queries exceed their bounded vector');
    end;
    for LAt := 0 to LSizeCount - 1 do
    begin
      Supports(LCaptured, INyxPascalContextSizes, LTargetSizes);
      LSizeName := LSizes.SizeName(LAt);
      LCaptured := LTargetSizes.WithSizeFact(LSizeName, LSizes.SizeFact(LSizeName));
      Supports(LCaptured, INyxPascalContextSizes, LTargetSizes);

      if LTargetSizes.SizeCount <> LAt + 1 then
      begin
        raise ENyxPascalContext.Create('Captured size queries contain duplicate references');
      end;
    end;
  end;
  { Native managed result storage may alias the caller's target. Publish only
    after every alternate getter and policy validation has succeeded. }
  Result := LCaptured;
end;

type
  { Owned name/value pair with no interfaces or mutable external data. }
  TDefineFact = record
    Name: TNyxPascalDefineRef;
    Value: TNyxPascalDefineValue;
  end;
  { Ordered immutable query/result pair with managed text but no interfaces. }
  TCapturedSize = record
    Name: TNyxPascalSizeReference;
    Value: TNyxPascalSizeFact;
  end;
  { Persistent context implementation. Its index/vectors are detached on every
    change; readers may share only a fully constructed immutable instance. }
  TCompilerContext = class(TInterfacedObject, INyxPascalCompilerContext, INyxPascalContextPolicies,
    INyxPascalContextSizes)
  private
    FFamily: TNyxPascalCompilerFamily;
    FVersion: TNyxPascalCompilerVersion;
    FMode: TNyxPascalLanguageMode;
    FComplete: Boolean;
    FSymbols: array of TDefineFact;
    FNames: TNyxTextIndex;
    FOptions: array[0..25] of TNyxPascalKnowledge;
    FFeatures: array[TNyxPascalLanguageFeature] of TNyxPascalKnowledge;
    FPolicies: TNyxPascalCompilerPolicies;
    FSizes: array of TCapturedSize;
    function Clone: TCompilerContext;
  public
    constructor Create;
    destructor Destroy; override;
    function Family: TNyxPascalCompilerFamily;
    function Version: TNyxPascalCompilerVersion;
    function Mode: TNyxPascalLanguageMode;
    function DefinesComplete: Boolean;
    function Symbol(const AName: TNyxPascalDefineRef): TNyxPascalDefineValue;
    function Option(AName: Char): TNyxPascalKnowledge;
    function Feature(AFeature: TNyxPascalLanguageFeature): TNyxPascalKnowledge;
    function WithSymbol(const AName: TNyxPascalDefineRef;
      AKind: TNyxPascalDefineKind; AInteger: Integer): INyxPascalCompilerContext;
    function CompleteDefines(AValue: Boolean): INyxPascalCompilerContext;
    function WithOption(AName: Char; AValue: TNyxPascalKnowledge): INyxPascalCompilerContext;
    function WithFeature(AFeature: TNyxPascalLanguageFeature;
      AValue: TNyxPascalKnowledge): INyxPascalCompilerContext;
    function InMode(AValue: TNyxPascalLanguageMode): INyxPascalCompilerContext;
    function UnknownState: INyxPascalCompilerContext;
    function SameFacts(const AOther: INyxPascalCompilerContext): Boolean;
    function SymbolCount: Integer;
    function SymbolName(AIndex: Integer): TNyxPascalDefineRef;
    function Policies: TNyxPascalCompilerPolicies;
    function WithPolicies(const AValue: TNyxPascalCompilerPolicies): INyxPascalCompilerContext;
    function SizeCount: Integer;
    function SizeName(AIndex: Integer): TNyxPascalSizeReference;
    function SizeFact(const AName: TNyxPascalSizeReference): TNyxPascalSizeFact;
    function WithSizeFact(const AName: TNyxPascalSizeReference;
      const AValue: TNyxPascalSizeFact): INyxPascalCompilerContext;
  end;
  { Evaluator-only value domain, never a Pascal runtime expression or macro. }
  TConditionValueKind = (pcvUnknown, pcvBoolean, pcvInteger);
  { Value-owned evaluation operand; integer values remain exactly target-safe. }
  TConditionValue = record
    Kind: TConditionValueKind;
    Truth: TNyxPascalKnowledge;
    Number: Integer;
  end;
  { Owns value-only request vectors for a single expression. Unsupported syntax
    clears all vectors; checked getters cannot expose mutable backing storage. }
  TConditionNeeds = class(TInterfacedObject, INyxPascalConditionNeeds, INyxPascalConditionDeclarations)
  private
    FSupported: Boolean;
    FSymbols: array of TNyxPascalDefineRef;
    FSizes: array of TNyxPascalSizeReference;
    FDeclarations: array of TNyxPascalDeclarationRef;
    procedure NeedSymbol(const AName: TNyxPascalDefineRef);
    procedure NeedSize(const AName: TNyxPascalSizeReference);
    procedure NeedDeclaration(const AName: TNyxPascalDeclarationRef);
  public
    function SyntaxSupported: Boolean;
    function SymbolCount: Integer;
    function Symbol(AIndex: Integer): TNyxPascalDefineRef;
    function SizeCount: Integer;
    function SizeName(AIndex: Integer): TNyxPascalSizeReference;
    function DeclarationCount: Integer;
    function DeclarationName(AIndex: Integer): TNyxPascalDeclarationRef;
  end;
  { Owns one temporary lexical snapshot and borrows an immutable context for this
    call only. Depth/steps are bounded and no provider or file lookup occurs. }
  TConditionParser = class
  private
    FContext: INyxPascalCompilerContext;
    FLexical: INyxPascalLexicalSnapshot;
    FAt: Integer;
    FDepth: Integer;
    FIssue: TNyxPascalConditionIssue;
    FNeeds: TConditionNeeds;
    FSemantics: INyxPascalConditionSemantics;
    function Tok: TNyxPascalToken;
    procedure Step;
    procedure Issue(AValue: TNyxPascalConditionIssue);
    function Primary: TConditionValue;
    function Expression(AMinimum: Integer): TConditionValue;
    function Precedence: Integer;
    function Apply(const ALeft, ARight: TConditionValue;
      const AOperator: TNyxPascalToken): TConditionValue;
  public
    function Analyze(const AContext: INyxPascalCompilerContext;
      const AText: TNyxText): TNyxPascalConditionResult;
  end;

function NyxPascalDefine(const AName: TNyxText): TNyxPascalDefineRef;
var
  LAt: Integer;
begin

  if (Length(AName) = 0) or (Length(AName) > 128) then
  begin
    raise ENyxPascalContext.Create('A compiler symbol needs 1 to 128 ASCII identifier characters');
  end;
  for LAt := 1 to Length(AName) do
  begin

    if not (AName[LAt] in ['A'..'Z', 'a'..'z', '_']) and
      not ((LAt > 1) and (AName[LAt] in ['0'..'9'])) then
    begin
      raise ENyxPascalContext.Create('A compiler symbol needs an ASCII identifier');
    end;
  end;
  Result.FName := NyxPascalASCIIKey(AName);
end;

class function TNyxPascalCompilerVersion.Create(AMajor, AMinor, APatch: Integer): TNyxPascalCompilerVersion;
begin

  if (AMajor < 0) or (AMajor > 999) or (AMinor < 0) or (AMinor > 99) or
    (APatch < 0) or (APatch > 99) then
  begin
    raise ENyxPascalContext.Create('A compiler version exceeds its decimal component bounds');
  end;
  Result.FMajor := AMajor;
  Result.FMinor := AMinor;
  Result.FPatch := APatch;
end;

function TNyxPascalCompilerVersion.FullNumber: Integer;
begin
  Result := FMajor * 10000 + FMinor * 100 + FPatch;
end;

function NyxPascalDeclaration(const AName: TNyxText): TNyxPascalDeclarationRef;
var
  LChecked: TNyxPascalDefineRef;
  LLexical: INyxPascalLexicalSnapshot;
begin
  LChecked := NyxPascalDefine(AName);
  LLexical := NewNyxPascalLexicalSnapshot(LChecked.Name, TNyxPascalLexicalOptions.Defaults);

  if (LLexical.Count <> 2) or (LLexical.Token(0).Kind <> ptkWord) or
    (LLexical.Token(0).Word <> pkwNone) then
  begin
    raise ENyxPascalContext.Create('A declaration request needs one ordinary identifier');
  end;
  Result.FName := LChecked.Name;
end;

function TNyxPascalDefineValue.IsDefined: TNyxPascalKnowledge;
begin
  Result := ppkTrue;

  if FKind = ppdUnknown then
  begin
    Result := ppkUnknown;
  end
  else if FKind = ppdUndefined then
  begin
    Result := ppkFalse;
  end;
end;

function TNyxPascalDefineValue.IntegerValue: Integer;
begin

  if FKind <> ppdInteger then
  begin
    raise ENyxPascalContext.Create('This compiler symbol has no captured integer value');
  end;
  Result := FInteger;
end;

function NyxPascalNot(AValue: TNyxPascalKnowledge): TNyxPascalKnowledge;
begin
  Result := ppkUnknown;

  if AValue = ppkTrue then
  begin
    Result := ppkFalse;
  end
  else if AValue = ppkFalse then
  begin
    Result := ppkTrue;
  end;
end;

function NyxPascalAnd(ALeft, ARight: TNyxPascalKnowledge): TNyxPascalKnowledge;
begin
  Result := ppkUnknown;

  if (ALeft = ppkFalse) or (ARight = ppkFalse) then
  begin
    Result := ppkFalse;
  end
  else if (ALeft = ppkTrue) and (ARight = ppkTrue) then
  begin
    Result := ppkTrue;
  end;
end;

function NyxPascalOr(ALeft, ARight: TNyxPascalKnowledge): TNyxPascalKnowledge;
begin
  Result := ppkUnknown;

  if (ALeft = ppkTrue) or (ARight = ppkTrue) then
  begin
    Result := ppkTrue;
  end
  else if (ALeft = ppkFalse) and (ARight = ppkFalse) then
  begin
    Result := ppkFalse;
  end;
end;

function OptionIndex(AName: Char): Integer;
begin

  if AName in ['a'..'z'] then
  begin
    AName := Chr(Ord(AName) - 32);
  end;

  if not (AName in ['A'..'Z']) then
  begin
    raise ENyxPascalContext.Create('A short compiler option needs one ASCII letter');
  end;
  Result := Ord(AName) - Ord('A');
end;

constructor TCompilerContext.Create;
begin
  inherited Create;
  FNames := TNyxTextIndex.Create;
end;

destructor TCompilerContext.Destroy;
begin
  FNames.Free;
  inherited Destroy;
end;

function TCompilerContext.Clone: TCompilerContext;
var
  LAt: Integer;
  LFeature: TNyxPascalLanguageFeature;
begin
  Result := TCompilerContext.Create;
  try
    Result.FFamily := FFamily;
    Result.FVersion := FVersion;
    Result.FMode := FMode;
    Result.FComplete := FComplete;
    Result.FPolicies := FPolicies;
    Result.FSizes := Copy(FSizes);
    SetLength(Result.FSymbols, Length(FSymbols));
    for LAt := 0 to High(FSymbols) do
    begin
      Result.FSymbols[LAt] := FSymbols[LAt];
      Result.FNames.AddFirst(FSymbols[LAt].Name.Name, LAt);
    end;
    for LAt := 0 to High(FOptions) do
    begin
      Result.FOptions[LAt] := FOptions[LAt];
    end;
    for LFeature := Low(TNyxPascalLanguageFeature) to High(TNyxPascalLanguageFeature) do
    begin
      Result.FFeatures[LFeature] := FFeatures[LFeature];
    end;
  except
    Result.Free;
    raise;
  end;
end;

function TCompilerContext.Family: TNyxPascalCompilerFamily;
begin
  Result := FFamily;
end;

function TCompilerContext.Version: TNyxPascalCompilerVersion;
begin
  Result := FVersion;
end;

function TCompilerContext.Mode: TNyxPascalLanguageMode;
begin
  Result := FMode;
end;

function TCompilerContext.DefinesComplete: Boolean;
begin
  Result := FComplete;
end;

function TCompilerContext.Symbol(const AName: TNyxPascalDefineRef): TNyxPascalDefineValue;
var
  LAt: Integer;
begin
  NyxPascalDefine(AName.Name);
  Result := Default(TNyxPascalDefineValue);
  LAt := FNames.IndexOf(AName.Name);

  if LAt >= 0 then
  begin
    Result := FSymbols[LAt].Value;
  end
  else if FComplete then
  begin
    Result.FKind := ppdUndefined;
  end;
end;

function TCompilerContext.Option(AName: Char): TNyxPascalKnowledge;
begin
  Result := FOptions[OptionIndex(AName)];
end;

function TCompilerContext.Feature(AFeature: TNyxPascalLanguageFeature): TNyxPascalKnowledge;
begin
  Result := FFeatures[AFeature];
end;

function TCompilerContext.WithSymbol(const AName: TNyxPascalDefineRef;
  AKind: TNyxPascalDefineKind; AInteger: Integer): INyxPascalCompilerContext;
var
  LCopy: TCompilerContext;
  LAt: Integer;
begin
  NyxPascalDefine(AName.Name);

  if (Ord(AKind) < Ord(Low(TNyxPascalDefineKind))) or
    (Ord(AKind) > Ord(High(TNyxPascalDefineKind))) then
  begin
    raise ENyxPascalContext.Create('A captured compiler define has an invalid value kind');
  end;
  LAt := FNames.IndexOf(AName.Name);

  if (LAt < 0) and (Length(FSymbols) >= 2048) then
  begin
    raise ENyxPascalContext.Create('Captured compiler symbols exceed 2048 names');
  end;
  LCopy := Clone;
  Result := LCopy;

  if LAt < 0 then
  begin
    LAt := Length(LCopy.FSymbols);
    SetLength(LCopy.FSymbols, LAt + 1);
    LCopy.FSymbols[LAt].Name := AName;
    LCopy.FNames.AddFirst(AName.Name, LAt);
  end;
  LCopy.FSymbols[LAt].Value.FKind := AKind;
  LCopy.FSymbols[LAt].Value.FInteger := 0;

  if AKind = ppdInteger then
  begin
    LCopy.FSymbols[LAt].Value.FInteger := AInteger;
  end;
end;

function TCompilerContext.CompleteDefines(AValue: Boolean): INyxPascalCompilerContext;
var
  LCopy: TCompilerContext;
begin
  LCopy := Clone;
  Result := LCopy;
  LCopy.FComplete := AValue;
end;

function TCompilerContext.WithOption(AName: Char;
  AValue: TNyxPascalKnowledge): INyxPascalCompilerContext;
var
  LCopy: TCompilerContext;
  LAt: Integer;
begin

  if (Ord(AValue) < Ord(Low(TNyxPascalKnowledge))) or
    (Ord(AValue) > Ord(High(TNyxPascalKnowledge))) then
  begin
    raise ENyxPascalContext.Create('A captured compiler option has invalid knowledge');
  end;
  LAt := OptionIndex(AName);
  LCopy := Clone;
  Result := LCopy;
  LCopy.FOptions[LAt] := AValue;
end;

function TCompilerContext.WithFeature(AFeature: TNyxPascalLanguageFeature;
  AValue: TNyxPascalKnowledge): INyxPascalCompilerContext;
var
  LCopy: TCompilerContext;
begin

  if (Ord(AFeature) < Ord(Low(TNyxPascalLanguageFeature))) or
    (Ord(AFeature) > Ord(High(TNyxPascalLanguageFeature))) or
    (Ord(AValue) < Ord(Low(TNyxPascalKnowledge))) or
    (Ord(AValue) > Ord(High(TNyxPascalKnowledge))) then
  begin
    raise ENyxPascalContext.Create('A captured language feature has invalid knowledge');
  end;
  LCopy := Clone;
  Result := LCopy;
  LCopy.FFeatures[AFeature] := AValue;
end;

function TCompilerContext.InMode(AValue: TNyxPascalLanguageMode): INyxPascalCompilerContext;
var
  LCopy: TCompilerContext;
  LAt: Integer;
  LFeature: TNyxPascalLanguageFeature;
begin

  if (Ord(AValue) < Ord(Low(TNyxPascalLanguageMode))) or
    (Ord(AValue) > Ord(High(TNyxPascalLanguageMode))) then
  begin
    raise ENyxPascalContext.Create('A captured language mode is outside its closed choices');
  end;
  LCopy := Clone;
  Result := LCopy;
  LCopy.FMode := AValue;

  if AValue <> FMode then
  begin
    SetLength(LCopy.FSizes, 0);
  end;
  LCopy.FPolicies := TNyxPascalCompilerPolicies.Unknown
    .RequestSmartLinking(FPolicies.SmartLinkingRequested);
  for LAt := 0 to High(LCopy.FOptions) do
  begin
    LCopy.FOptions[LAt] := ppkUnknown;
  end;
  for LFeature := Low(TNyxPascalLanguageFeature) to High(TNyxPascalLanguageFeature) do
  begin
    LCopy.FFeatures[LFeature] := ppkUnknown;
  end;
end;

function TCompilerContext.UnknownState: INyxPascalCompilerContext;
var
  LCopy: TCompilerContext;
begin
  LCopy := TCompilerContext.Create;
  Result := LCopy;
  LCopy.FFamily := FFamily;
  LCopy.FVersion := FVersion;
  LCopy.FMode := ppmUnknown;
end;

function TCompilerContext.SymbolCount: Integer;
begin
  Result := Length(FSymbols);
end;

function TCompilerContext.SymbolName(AIndex: Integer): TNyxPascalDefineRef;
begin

  if (AIndex < 0) or (AIndex >= Length(FSymbols)) then
  begin
    raise ENyxPascalContext.Create('Captured compiler symbol index is outside its context');
  end;
  Result := FSymbols[AIndex].Name;
end;

function TCompilerContext.SameFacts(const AOther: INyxPascalCompilerContext): Boolean;
var
  LAt: Integer;
  LFeature: TNyxPascalLanguageFeature;
  LOther: TNyxPascalDefineValue;
  LSizes: INyxPascalContextSizes;
begin
  Result := False;

  if (AOther = nil) or (AOther.Family <> FFamily) or
    (AOther.Version.FullNumber <> FVersion.FullNumber) or (AOther.Mode <> FMode) or
    (AOther.DefinesComplete <> FComplete) or (AOther.SymbolCount <> SymbolCount) then
  begin
    Exit;
  end;

  if not FPolicies.SamePolicies(NyxPascalCompilerPolicies(AOther)) then
  begin
    Exit;
  end;

  if Supports(AOther, INyxPascalContextSizes, LSizes) then
  begin

    if LSizes.SizeCount <> Length(FSizes) then
    begin
      Exit;
    end;
    for LAt := 0 to High(FSizes) do
    begin

      if (LSizes.SizeName(LAt).Name <> FSizes[LAt].Name.Name) or
        not LSizes.SizeFact(LSizes.SizeName(LAt)).SameFact(FSizes[LAt].Value) then
      begin
        Exit;
      end;
    end;
  end
  else if Length(FSizes) <> 0 then
  begin
    Exit;
  end;
  for LAt := 0 to SymbolCount - 1 do
  begin
    { Enumeration is part of exact capture identity, including explicitly
      unknown names which otherwise compare equal to another missing name. }

    if AOther.SymbolName(LAt).Name <> FSymbols[LAt].Name.Name then
    begin
      Exit;
    end;
    LOther := AOther.Symbol(FSymbols[LAt].Name);

    if (LOther.Kind <> FSymbols[LAt].Value.Kind) or
      ((LOther.Kind = ppdInteger) and (LOther.IntegerValue <> FSymbols[LAt].Value.IntegerValue)) then
    begin
      Exit;
    end;
  end;
  for LAt := 0 to High(FOptions) do
  begin

    if AOther.Option(Chr(Ord('A') + LAt)) <> FOptions[LAt] then
    begin
      Exit;
    end;
  end;
  for LFeature := Low(TNyxPascalLanguageFeature) to High(TNyxPascalLanguageFeature) do
  begin

    if AOther.Feature(LFeature) <> FFeatures[LFeature] then
    begin
      Exit;
    end;
  end;
  Result := True;
end;

function NewNyxPascalCompilerContext(AFamily: TNyxPascalCompilerFamily;
  const AVersion: TNyxPascalCompilerVersion;
  AMode: TNyxPascalLanguageMode): INyxPascalCompilerContext;
var
  LContext: TCompilerContext;
begin

  if (Ord(AFamily) < Ord(Low(TNyxPascalCompilerFamily))) or
    (Ord(AFamily) > Ord(High(TNyxPascalCompilerFamily))) or
    (Ord(AMode) < Ord(Low(TNyxPascalLanguageMode))) or
    (Ord(AMode) > Ord(High(TNyxPascalLanguageMode))) then
  begin
    raise ENyxPascalContext.Create('Captured compiler identity is outside its closed choices');
  end;
  LContext := TCompilerContext.Create;
  Result := LContext;
  LContext.FFamily := AFamily;
  LContext.FVersion := AVersion;
  LContext.FMode := AMode;
end;

function NyxPascalSizeReference(const AName: TNyxText): TNyxPascalSizeReference;
var
  LAt: Integer;
  LStart: Integer;
  LParts: Integer;
  LLexical: INyxPascalLexicalSnapshot;
begin

  if (Length(AName) < 1) or (Length(AName) > 1024) then
  begin
    raise ENyxPascalContext.Create('A size reference needs a bounded dotted identifier');
  end;
  LStart := 1;
  LParts := 1;
  for LAt := 1 to Length(AName) do
  begin

    if AName[LAt] = '.' then
    begin

      if (LAt = LStart) or (LAt - LStart > 128) or (LParts >= 8) then
      begin
        raise ENyxPascalContext.Create('A size reference has an invalid qualification');
      end;
      LStart := LAt + 1;
      Inc(LParts);
    end
    else if not (AName[LAt] in ['A'..'Z', 'a'..'z', '_', '0'..'9']) or
      ((LAt = LStart) and (AName[LAt] in ['0'..'9'])) then
    begin
      raise ENyxPascalContext.Create('A size reference cannot inject a Pascal expression');
    end;
  end;

  if (LStart > Length(AName)) or (Length(AName) - LStart + 1 > 128) then
  begin
    raise ENyxPascalContext.Create('A size reference has an invalid final identifier');
  end;
  LLexical := NewNyxPascalLexicalSnapshot(AName, TNyxPascalLexicalOptions.Defaults);
  for LAt := 0 to LLexical.Count - 2 do
  begin

    if (LLexical.Token(LAt).Kind = ptkWord) and
      not (LLexical.Token(LAt).Word in [pkwNone, pkwString]) then
    begin
      raise ENyxPascalContext.Create('A size reference cannot use a reserved grammar word');
    end;
  end;
  Result.FName := NyxPascalASCIIKey(AName);
end;

function NyxPascalSizeFact(AState: TNyxPascalSizeState; AMode: TNyxPascalLanguageMode;
  ALongStrings: TNyxPascalKnowledge; ABytes: Integer): TNyxPascalSizeFact;
begin

  if (Ord(AState) < Ord(Low(TNyxPascalSizeState))) or
    (Ord(AState) > Ord(High(TNyxPascalSizeState))) or
    (Ord(AMode) < Ord(Low(TNyxPascalLanguageMode))) or
    (Ord(AMode) > Ord(High(TNyxPascalLanguageMode))) or
    (Ord(ALongStrings) < Ord(Low(TNyxPascalKnowledge))) or
    (Ord(ALongStrings) > Ord(High(TNyxPascalKnowledge))) or (ABytes < 0) or
    ((AState <> pssKnown) and (ABytes <> 0)) or
    ((AState <> pssUnknown) and ((AMode = ppmUnknown) or (ALongStrings = ppkUnknown))) then
  begin
    raise ENyxPascalContext.Create('A size measurement needs an exact supported value and probe scope');
  end;
  Result.FState := AState;
  Result.FMode := AMode;
  Result.FLongStrings := ALongStrings;
  Result.FBytes := ABytes;
end;

function TNyxPascalSizeFact.Bytes: Integer;
begin

  if FState <> pssKnown then
  begin
    raise ENyxPascalContext.Create('An unmeasured size has no byte value');
  end;
  Result := FBytes;
end;

function TNyxPascalSizeFact.SameFact(const AOther: TNyxPascalSizeFact): Boolean;
begin
  Result := (FState = AOther.FState) and (FMode = AOther.FMode) and
    (FLongStrings = AOther.FLongStrings) and (FBytes = AOther.FBytes);
end;

function TCompilerContext.SizeCount: Integer;
begin
  Result := Length(FSizes);
end;

function TCompilerContext.SizeName(AIndex: Integer): TNyxPascalSizeReference;
begin

  if (AIndex < 0) or (AIndex >= Length(FSizes)) then
  begin
    raise ENyxPascalContext.Create('Captured size index is outside its context');
  end;
  Result := FSizes[AIndex].Name;
end;

function TCompilerContext.SizeFact(const AName: TNyxPascalSizeReference): TNyxPascalSizeFact;
var
  LAt: Integer;
begin
  Result := NyxPascalSizeFact(pssUnknown);
  for LAt := 0 to High(FSizes) do
  begin

    if FSizes[LAt].Name.Name = AName.Name then
    begin
      Exit(FSizes[LAt].Value);
    end;
  end;
end;

function TCompilerContext.WithSizeFact(const AName: TNyxPascalSizeReference;
  const AValue: TNyxPascalSizeFact): INyxPascalCompilerContext;
var
  LCopy: TCompilerContext;
  LOwner: INyxPascalCompilerContext;
  LAt: Integer;
  LIndex: Integer;
  LBytes: Integer;
begin
  NyxPascalSizeReference(AName.Name);
  LBytes := 0;

  if AValue.State = pssKnown then
  begin
    LBytes := AValue.Bytes;
  end;
  NyxPascalSizeFact(AValue.State, AValue.ProbeMode, AValue.ProbeLongStrings, LBytes);
  LIndex := -1;
  for LAt := 0 to High(FSizes) do
  begin

    if FSizes[LAt].Name.Name = AName.Name then
    begin
      LIndex := LAt;
    end;
  end;

  if (LIndex < 0) and (Length(FSizes) >= 64) then
  begin
    raise ENyxPascalContext.Create('Captured size facts exceed 64 references');
  end;
  LCopy := Clone;
  LOwner := LCopy;

  if LIndex < 0 then
  begin
    LIndex := Length(LCopy.FSizes);
    SetLength(LCopy.FSizes, LIndex + 1);
  end;
  LCopy.FSizes[LIndex].Name := AName;
  LCopy.FSizes[LIndex].Value := AValue;
  Result := LOwner;
end;

function NyxPascalCapturedSize(const AContext: INyxPascalCompilerContext;
  const AName: TNyxPascalSizeReference): TNyxPascalSizeFact;
var
  LSizes: INyxPascalContextSizes;
begin

  if AContext = nil then
  begin
    raise ENyxPascalContext.Create('A captured size needs its compiler context');
  end;
  Result := NyxPascalSizeFact(pssUnknown);

  if Supports(AContext, INyxPascalContextSizes, LSizes) then
  begin
    Result := LSizes.SizeFact(AName);
  end;
end;

function NyxPascalWithCapturedSize(const AContext: INyxPascalCompilerContext;
  const AName: TNyxPascalSizeReference; const AValue: TNyxPascalSizeFact): INyxPascalCompilerContext;
var
  LContext: INyxPascalCompilerContext;
  LSizes: INyxPascalContextSizes;
begin
  LContext := CaptureNyxPascalCompilerContext(AContext);
  Supports(LContext, INyxPascalContextSizes, LSizes);
  Result := LSizes.WithSizeFact(AName, AValue);
end;

class function TNyxPascalCompilerPolicies.Unknown: TNyxPascalCompilerPolicies;
begin
  Result := Default(TNyxPascalCompilerPolicies);
end;

procedure TNyxPascalCompilerPolicies.Validate;
begin

  if (Ord(FInlining) < Ord(Low(TNyxPascalKnowledge))) or
    (Ord(FInlining) > Ord(High(TNyxPascalKnowledge))) or
    (Ord(FRecords) < Ord(Low(TNyxPascalRecordAlignment))) or
    (Ord(FRecords) > Ord(High(TNyxPascalRecordAlignment))) or
    (Ord(FEnums) < Ord(Low(TNyxPascalEnumStorage))) or
    (Ord(FEnums) > Ord(High(TNyxPascalEnumStorage))) or
    (Ord(FSets) < Ord(Low(TNyxPascalSetPacking))) or
    (Ord(FSets) > Ord(High(TNyxPascalSetPacking))) or
    (Ord(FSmartLinking) < Ord(Low(TNyxPascalKnowledge))) or
    (Ord(FSmartLinking) > Ord(High(TNyxPascalKnowledge))) or
    (Ord(FCalling) < Ord(Low(TNyxPascalCallingConvention))) or
    (Ord(FCalling) > Ord(High(TNyxPascalCallingConvention))) then
  begin
    raise ENyxPascalContext.Create('Compiler policy is outside its closed value domain');
  end;
end;

function TNyxPascalCompilerPolicies.WithInlining(AValue: TNyxPascalKnowledge): TNyxPascalCompilerPolicies;
begin
  Result := Self;
  Result.FInlining := AValue;
  Result.Validate;
end;

function TNyxPascalCompilerPolicies.AlignRecords(AValue: TNyxPascalRecordAlignment): TNyxPascalCompilerPolicies;
begin
  Result := Self;
  Result.FRecords := AValue;
  Result.Validate;
end;

function TNyxPascalCompilerPolicies.StoreEnums(AValue: TNyxPascalEnumStorage): TNyxPascalCompilerPolicies;
begin
  Result := Self;
  Result.FEnums := AValue;
  Result.Validate;
end;

function TNyxPascalCompilerPolicies.PackSets(AValue: TNyxPascalSetPacking): TNyxPascalCompilerPolicies;
begin
  Result := Self;
  Result.FSets := AValue;
  Result.Validate;
end;

function TNyxPascalCompilerPolicies.RequestSmartLinking(AValue: TNyxPascalKnowledge): TNyxPascalCompilerPolicies;
begin
  Result := Self;
  Result.FSmartLinking := AValue;
  Result.Validate;
end;

function TNyxPascalCompilerPolicies.CallBy(AValue: TNyxPascalCallingConvention): TNyxPascalCompilerPolicies;
begin
  Result := Self;
  Result.FCalling := AValue;
  Result.Validate;
end;

function TNyxPascalCompilerPolicies.SamePolicies(const AOther: TNyxPascalCompilerPolicies): Boolean;
begin
  Validate;
  AOther.Validate;
  Result := (FInlining = AOther.FInlining) and (FRecords = AOther.FRecords) and
    (FEnums = AOther.FEnums) and (FSets = AOther.FSets) and
    (FCalling = AOther.FCalling) and (FSmartLinking = AOther.FSmartLinking);
end;

function TCompilerContext.Policies: TNyxPascalCompilerPolicies;
begin
  Result := FPolicies;
end;

function TCompilerContext.WithPolicies(const AValue: TNyxPascalCompilerPolicies): INyxPascalCompilerContext;
var
  LCopy: TCompilerContext;
  LOwner: INyxPascalCompilerContext;
begin
  AValue.Validate;
  LCopy := Clone;
  LOwner := LCopy;
  LCopy.FPolicies := AValue;
  Result := LOwner;
end;

function NyxPascalCompilerPolicies(const AContext: INyxPascalCompilerContext): TNyxPascalCompilerPolicies;
var
  LPolicies: INyxPascalContextPolicies;
begin

  if AContext = nil then
  begin
    raise ENyxPascalContext.Create('Compiler policies need their context');
  end;
  Result := TNyxPascalCompilerPolicies.Unknown;

  if Supports(AContext, INyxPascalContextPolicies, LPolicies) then
  begin
    Result := LPolicies.Policies;
    Result.Validate;
  end;
end;

function NyxPascalWithCompilerPolicies(const AContext: INyxPascalCompilerContext;
  const AValue: TNyxPascalCompilerPolicies): INyxPascalCompilerContext;
var
  LContext: INyxPascalCompilerContext;
  LPolicies: INyxPascalContextPolicies;
begin
  AValue.Validate;
  LContext := CaptureNyxPascalCompilerContext(AContext);
  Supports(LContext, INyxPascalContextPolicies, LPolicies);
  Result := LPolicies.WithPolicies(AValue);
end;

function NyxPascalLocalSwitchOption(AValue: TNyxPascalLocalSwitch): Char;
const
  COptions: array[TNyxPascalLocalSwitch] of Char = ('B', 'C', 'H', 'I', 'J', 'Q', 'R', 'S', 'T', 'V', 'W');
begin

  if (Ord(AValue) < Ord(Low(TNyxPascalLocalSwitch))) or
    (Ord(AValue) > Ord(High(TNyxPascalLocalSwitch))) then
  begin
    raise ENyxPascalContext.Create('Local compiler switch is outside its closed domain');
  end;
  Result := COptions[AValue];
end;

function NyxPascalWithLocalSwitch(const AContext: INyxPascalCompilerContext;
  ASwitch: TNyxPascalLocalSwitch; AValue: TNyxPascalKnowledge): INyxPascalCompilerContext;
begin

  if AContext = nil then
  begin
    raise ENyxPascalContext.Create('A local switch needs its compiler context');
  end;
  Result := AContext.WithOption(NyxPascalLocalSwitchOption(ASwitch), AValue);
end;

function RestoreNyxPascalLocalSettings(const ACurrent, ASaved: INyxPascalCompilerContext): INyxPascalCompilerContext;
var
  LCurrent: INyxPascalCompilerContext;
  LSaved: INyxPascalCompilerContext;
  LSwitch: TNyxPascalLocalSwitch;
  LPolicies: INyxPascalContextPolicies;
begin
  LCurrent := CaptureNyxPascalCompilerContext(ACurrent);
  LSaved := CaptureNyxPascalCompilerContext(ASaved);

  if (LCurrent.Family <> LSaved.Family) or
    (LCurrent.Version.FullNumber <> LSaved.Version.FullNumber) then
  begin
    raise ENyxPascalContext.Create('Local settings belong to another compiler identity');
  end;
  for LSwitch := Low(TNyxPascalLocalSwitch) to High(TNyxPascalLocalSwitch) do
  begin
    LCurrent := NyxPascalWithLocalSwitch(LCurrent, LSwitch,
      LSaved.Option(NyxPascalLocalSwitchOption(LSwitch)));
  end;
  LCurrent := LCurrent.WithFeature(ppfScopedEnums, LSaved.Feature(ppfScopedEnums));
  Supports(LCurrent, INyxPascalContextPolicies, LPolicies);
  Result := LPolicies.WithPolicies(NyxPascalCompilerPolicies(LSaved)
    .RequestSmartLinking(NyxPascalCompilerPolicies(LCurrent).SmartLinkingRequested));
end;

function UnknownValue: TConditionValue;
begin
  Result := Default(TConditionValue);
end;

function TruthValue(AValue: TNyxPascalKnowledge): TConditionValue;
begin

  if (Ord(AValue) < Ord(Low(TNyxPascalKnowledge))) or
    (Ord(AValue) > Ord(High(TNyxPascalKnowledge))) then
  begin
    raise ENyxPascalContext.Create('Conditional semantic knowledge is outside its closed values');
  end;
  Result := Default(TConditionValue);
  Result.Kind := pcvBoolean;
  Result.Truth := AValue;
end;

function NumberValue(AValue: Integer): TConditionValue;
begin
  Result := Default(TConditionValue);
  Result.Kind := pcvInteger;
  Result.Number := AValue;
end;

function AsTruth(const AValue: TConditionValue): TNyxPascalKnowledge;
begin
  Result := ppkUnknown;

  if AValue.Kind = pcvBoolean then
  begin
    Result := AValue.Truth;
  end
  else if (AValue.Kind = pcvInteger) and (AValue.Number in [0, 1]) then
  begin
    Result := ppkFalse;

    if AValue.Number = 1 then
    begin
      Result := ppkTrue;
    end;
  end;
end;

function TConditionParser.Tok: TNyxPascalToken;
begin
  Result := FLexical.Token(FAt);
end;

procedure TConditionParser.Step;
begin

  if FAt < FLexical.Count - 1 then
  begin
    Inc(FAt);
  end;
  while (FAt < FLexical.Count - 1) and
    (Tok.Kind = ptkWhitespace) do
  begin
    Inc(FAt);
  end;
end;

procedure TConditionParser.Issue(AValue: TNyxPascalConditionIssue);
begin

  if (FIssue = pciNone) or (AValue > FIssue) then
  begin
    FIssue := AValue;
  end;
end;

function TConditionParser.Primary: TConditionValue;
var
  LName: TNyxText;
  LFact: TNyxPascalDefineValue;
  LNumber: Double;
  LAt: Integer;
  LDigit: Integer;
  LBase: Integer;
  LNegative: Boolean;
  LSize: TNyxPascalSizeReference;
  LDeclaration: TNyxPascalDeclarationRef;
  LMeasurement: TNyxPascalSizeFact;
begin
  Result := UnknownValue;
  Inc(FDepth);

  if FDepth > 64 then
  begin
    Issue(pciUnsupported);
    Dec(FDepth);
    Exit;
  end;
  try

    if (Tok.Kind = ptkSymbol) and (Tok.Symbol = psoOpenParen) then
    begin
      Step;
      Result := Expression(0);

      if (Tok.Kind = ptkSymbol) and (Tok.Symbol = psoCloseParen) then
      begin
        Step;
      end
      else
      begin
        Issue(pciMalformed);
      end;
    end
    else if (Tok.Kind = ptkWord) and (Tok.Word = pkwNot) then
    begin
      Step;
      Result := TruthValue(NyxPascalNot(AsTruth(Primary)));
    end
    else if (Tok.Kind = ptkSymbol) and (Tok.Symbol in [psoPlus, psoMinus]) then
    begin
      LNegative := Tok.Symbol = psoMinus;
      Step;
      Result := Primary;

      if Result.Kind <> pcvInteger then
      begin
        Result := UnknownValue;
        Issue(pciUnsupported);
      end
      else if LNegative then
      begin

        if Result.Number = Low(Integer) then
        begin
          Result := UnknownValue;
          Issue(pciUnsupported);
        end
        else
        begin
          Result.Number := -Result.Number;
        end;
      end;
    end
    else if (Tok.Kind = ptkNumber) and Tok.Complete and not Tok.RealNumber then
    begin
      LName := FLexical.Text(FAt);
      { A bare noncanonical decimal may follow compiler-version-specific
        conditional string rules. Do not normalize it into a guessed Boolean. }

      if (Tok.NumberBase = pnbDecimal) and (Length(LName) > 1) and (LName[1] = '0') then
      begin
        Issue(pciUnsupported);
      end;
      LBase := 10;
      LAt := 1;
      case Tok.NumberBase of
        pnbHexadecimal:
          begin
            LBase := 16;
            LAt := 2;
          end;
        pnbOctal:
          begin
            LBase := 8;
            LAt := 2;
          end;
        pnbBinary:
          begin
            LBase := 2;
            LAt := 2;
          end;
      else
        begin
          { Decimal spelling starts at its first digit. }
        end;
      end;
      LNumber := 0;
      while LAt <= Length(LName) do
      begin
        LDigit := Ord(LName[LAt]) - Ord('0');

        if LName[LAt] in ['A'..'F'] then
        begin
          LDigit := Ord(LName[LAt]) - Ord('A') + 10;
        end
        else if LName[LAt] in ['a'..'f'] then
        begin
          LDigit := Ord(LName[LAt]) - Ord('a') + 10;
        end;
        LNumber := LNumber * LBase + LDigit;

        if LNumber > High(Integer) then
        begin
          Issue(pciUnsupported);
          Step;
          Exit;
        end;
        Inc(LAt);
      end;
      Result := NumberValue(Trunc(LNumber));
      Step;
    end
    else if Tok.Kind = ptkWord then
    begin
      { Escaped Pascal identifiers are not compiler-symbol syntax. }

      if Tok.Escaped then
      begin
        Issue(pciUnsupported);
      end;
      LName := NyxPascalASCIIKey(FLexical.Identifier(FAt));
      Step;

      if LName = 'defined' then
      begin

        if (Tok.Kind <> ptkSymbol) or (Tok.Symbol <> psoOpenParen) then
        begin
          Issue(pciMalformed);
          Exit;
        end;
        Step;

        if Tok.Kind <> ptkWord then
        begin
          Issue(pciMalformed);
          Exit;
        end;

        if Tok.Escaped then
        begin
          Issue(pciUnsupported);
        end;
        LName := FLexical.Identifier(FAt);
        Step;

        if (Tok.Kind <> ptkSymbol) or (Tok.Symbol <> psoCloseParen) then
        begin
          Issue(pciMalformed);
          Exit;
        end;
        Step;
        LFact := FContext.Symbol(NyxPascalDefine(LName));
        Result := TruthValue(LFact.IsDefined);

        if Result.Truth = ppkUnknown then
        begin
          Issue(pciUnknownSymbol);

          if FNeeds <> nil then
          begin
            FNeeds.NeedSymbol(NyxPascalDefine(LName));
          end;
        end;
      end
      else if LName = 'declared' then
      begin

        if (Tok.Kind <> ptkSymbol) or (Tok.Symbol <> psoOpenParen) then
        begin
          Issue(pciMalformed);
          Exit;
        end;
        Step;

        if (Tok.Kind <> ptkWord) or Tok.Escaped then
        begin
          Issue(pciUnsupported);
          Exit;
        end;
        try
          LDeclaration := NyxPascalDeclaration(FLexical.Identifier(FAt));
        except
          on ENyxPascalContext do
          begin
            Issue(pciUnsupported);
            Exit;
          end;
        end;
        Step;

        if (Tok.Kind <> ptkSymbol) or (Tok.Symbol <> psoCloseParen) then
        begin
          Issue(pciUnsupported);
          Exit;
        end;
        Step;

        if FNeeds <> nil then
        begin
          FNeeds.NeedDeclaration(LDeclaration);
        end;

        if FSemantics <> nil then
        begin
          Result := TruthValue(FSemantics.IsDeclared(LDeclaration));
        end;

        if Result.Kind = pcvUnknown then
        begin
          Issue(pciUnknownSymbol);
        end
        else if Result.Truth = ppkUnknown then
        begin
          Issue(pciUnknownSymbol);
        end;
      end
      else if LName = 'sizeof' then
      begin
        { Planning names is deliberately independent of evaluating types. A
          pristine compiler probe cannot establish this authored scope's binding. }

        if (Tok.Kind <> ptkSymbol) or (Tok.Symbol <> psoOpenParen) then
        begin
          Issue(pciMalformed);
          Exit;
        end;
        Step;
        LName := '';
        while Tok.Kind = ptkWord do
        begin

          if Tok.Escaped then
          begin
            Issue(pciUnsupported);
            Exit;
          end;
          LName := LName + FLexical.Identifier(FAt);
          Step;

          if (Tok.Kind <> ptkSymbol) or (Tok.Symbol <> psoDot) then
          begin
            Break;
          end;
          LName := LName + '.';
          Step;
        end;

        if (Tok.Kind <> ptkSymbol) or (Tok.Symbol <> psoCloseParen) then
        begin
          Issue(pciUnsupported);
          Exit;
        end;
        Step;
        try
          LSize := NyxPascalSizeReference(LName);
        except
          on ENyxPascalContext do
          begin
            Issue(pciUnsupported);
            Exit;
          end;
        end;

        if FNeeds <> nil then
        begin
          FNeeds.NeedSize(LSize);
        end;

        if FSemantics <> nil then
        begin
          LMeasurement := FSemantics.SizeOfName(LSize);

          if LMeasurement.State = pssKnown then
          begin
            Result := NumberValue(LMeasurement.Bytes);
          end;
        end;

        if Result.Kind = pcvUnknown then
        begin
          Issue(pciUnknownSymbol);
        end;
      end
      else if (LName = 'true') or (LName = 'false') then
      begin
        Result := TruthValue(ppkFalse);

        if LName = 'true' then
        begin
          Result := TruthValue(ppkTrue);
        end;
      end
      else
      begin
        LFact := FContext.Symbol(NyxPascalDefine(LName));

        if LFact.Kind = ppdInteger then
        begin
          Result := NumberValue(LFact.IntegerValue);
        end
        else
        begin
          Issue(pciUnknownSymbol);

          if (FNeeds <> nil) and (LFact.IsDefined = ppkUnknown) then
          begin
            FNeeds.NeedSymbol(NyxPascalDefine(LName));
          end;
        end;
      end;
    end
    else
    begin
      Issue(pciMalformed);
      Step;
    end;
  finally
    Dec(FDepth);
  end;
end;

function TConditionParser.Precedence: Integer;
begin
  Result := -1;

  if (Tok.Kind = ptkSymbol) and (Tok.Symbol in [psoEqual, psoNotEqual,
    psoLess, psoGreater, psoLessEqual, psoGreaterEqual]) then
  begin
    Result := 10;
  end
  else if (Tok.Kind = ptkWord) and (Tok.Word in [pkwOr, pkwXor]) then
  begin
    Result := 20;
  end
  else if (Tok.Kind = ptkWord) and (Tok.Word = pkwAnd) then
  begin
    Result := 30;
  end;
end;

function TConditionParser.Apply(const ALeft, ARight: TConditionValue;
  const AOperator: TNyxPascalToken): TConditionValue;
var
  LLeft: TNyxPascalKnowledge;
  LRight: TNyxPascalKnowledge;
  LComparison: Boolean;
begin
  Result := UnknownValue;

  if AOperator.Kind = ptkWord then
  begin
    LLeft := AsTruth(ALeft);
    LRight := AsTruth(ARight);

    if ((ALeft.Kind = pcvInteger) and (LLeft = ppkUnknown)) or
      ((ARight.Kind = pcvInteger) and (LRight = ppkUnknown)) then
    begin
      Issue(pciUnsupported);
      Exit;
    end;
    case AOperator.Word of
      pkwAnd:
        begin
          Result := TruthValue(NyxPascalAnd(LLeft, LRight));
        end;
      pkwOr:
        begin
          Result := TruthValue(NyxPascalOr(LLeft, LRight));
        end;
      pkwXor:
        begin
          Result := TruthValue(ppkUnknown);

          if (LLeft <> ppkUnknown) and (LRight <> ppkUnknown) then
          begin
            Result := TruthValue(ppkFalse);

            if LLeft <> LRight then
            begin
              Result := TruthValue(ppkTrue);
            end;
          end;
        end;
    else
      begin
        Issue(pciUnsupported);
      end;
    end;
    Exit;
  end;

  if (ALeft.Kind = pcvUnknown) or (ARight.Kind = pcvUnknown) then
  begin
    Exit;
  end;

  if (ALeft.Kind <> pcvInteger) or (ARight.Kind <> pcvInteger) then
  begin
    Issue(pciUnsupported);
    Exit;
  end;
  LComparison := False;
  case AOperator.Symbol of
    psoEqual:
      begin
        LComparison := ALeft.Number = ARight.Number;
      end;
    psoNotEqual:
      begin
        LComparison := ALeft.Number <> ARight.Number;
      end;
    psoLess:
      begin
        LComparison := ALeft.Number < ARight.Number;
      end;
    psoGreater:
      begin
        LComparison := ALeft.Number > ARight.Number;
      end;
    psoLessEqual:
      begin
        LComparison := ALeft.Number <= ARight.Number;
      end;
    psoGreaterEqual:
      begin
        LComparison := ALeft.Number >= ARight.Number;
      end;
  else
    begin
      Issue(pciUnsupported);
    end;
  end;
  Result := TruthValue(ppkFalse);

  if LComparison then
  begin
    Result := TruthValue(ppkTrue);
  end;
end;

function TConditionParser.Expression(AMinimum: Integer): TConditionValue;
var
  LOperator: TNyxPascalToken;
  LRight: TConditionValue;
  LPrecedence: Integer;
begin
  Result := Primary;
  LPrecedence := Precedence;
  while LPrecedence >= AMinimum do
  begin
    LOperator := Tok;
    Step;
    LRight := Expression(LPrecedence + 1);
    Result := Apply(Result, LRight, LOperator);
    LPrecedence := Precedence;
  end;
end;

function TConditionParser.Analyze(const AContext: INyxPascalCompilerContext;
  const AText: TNyxText): TNyxPascalConditionResult;
begin

  if AContext = nil then
  begin
    raise ENyxPascalContext.Create('Condition analysis requires captured compiler facts');
  end;
  FContext := AContext;
  FLexical := NewNyxPascalLexicalSnapshot(AText, TNyxPascalLexicalOptions.Defaults
    .UpToSourceBytes(16384).UpToTokens(512));
  FAt := 0;

  if Tok.Kind = ptkWhitespace then
  begin
    Step;
  end;
  Result.FTruth := AsTruth(Expression(0));

  if (Tok.Kind <> ptkEOF) or (FLexical.DiagnosticCount <> 0) then
  begin
    Issue(pciUnsupported);
  end;

  if (Result.FTruth = ppkUnknown) and (FIssue = pciNone) then
  begin
    Issue(pciUnsupported);
  end;
  Result.FIssue := FIssue;

  if FIssue in [pciUnsupported, pciMalformed] then
  begin
    Result.FTruth := ppkUnknown;
  end;
end;

function AnalyzeNyxPascalCondition(const AContext: INyxPascalCompilerContext;
  const AExpression: TNyxText;
  const ASemantics: INyxPascalConditionSemantics): TNyxPascalConditionResult;
var
  LParser: TConditionParser;
begin
  LParser := TConditionParser.Create;
  try
    LParser.FSemantics := ASemantics;
    Result := LParser.Analyze(AContext, AExpression);
  finally
    LParser.Free;
  end;
end;

function TConditionNeeds.SyntaxSupported: Boolean;
begin
  Result := FSupported;
end;

function TConditionNeeds.SymbolCount: Integer;
begin
  Result := Length(FSymbols);
end;

function TConditionNeeds.Symbol(AIndex: Integer): TNyxPascalDefineRef;
begin

  if (AIndex < 0) or (AIndex >= SymbolCount) then
  begin
    raise ENyxPascalContext.Create('Conditional symbol request is outside this plan');
  end;
  Result := FSymbols[AIndex];
end;

function TConditionNeeds.SizeCount: Integer;
begin
  Result := Length(FSizes);
end;

function TConditionNeeds.SizeName(AIndex: Integer): TNyxPascalSizeReference;
begin

  if (AIndex < 0) or (AIndex >= SizeCount) then
  begin
    raise ENyxPascalContext.Create('Conditional size operand is outside this plan');
  end;
  Result := FSizes[AIndex];
end;

procedure TConditionNeeds.NeedSymbol(const AName: TNyxPascalDefineRef);
var
  LAt: Integer;
begin
  for LAt := 0 to High(FSymbols) do
  begin

    if FSymbols[LAt].Name = AName.Name then
    begin
      Exit;
    end;
  end;

  if SymbolCount >= 256 then
  begin
    raise ENyxPascalContext.Create('Conditional symbol requests exceed 256');
  end;
  LAt := SymbolCount;
  SetLength(FSymbols, LAt + 1);
  FSymbols[LAt] := AName;
end;

procedure TConditionNeeds.NeedSize(const AName: TNyxPascalSizeReference);
var
  LAt: Integer;
begin
  for LAt := 0 to High(FSizes) do
  begin

    if FSizes[LAt].Name = AName.Name then
    begin
      Exit;
    end;
  end;

  if SizeCount >= 64 then
  begin
    raise ENyxPascalContext.Create('Conditional size operands exceed 64');
  end;
  LAt := SizeCount;
  SetLength(FSizes, LAt + 1);
  FSizes[LAt] := AName;
end;

function TConditionNeeds.DeclarationCount: Integer;
begin
  Result := Length(FDeclarations);
end;

function TConditionNeeds.DeclarationName(AIndex: Integer): TNyxPascalDeclarationRef;
begin

  if (AIndex < 0) or (AIndex >= DeclarationCount) then
  begin
    raise ENyxPascalContext.Create('A declaration request is outside this plan');
  end;
  Result := FDeclarations[AIndex];
end;

procedure TConditionNeeds.NeedDeclaration(const AName: TNyxPascalDeclarationRef);
var
  LAt: Integer;
begin
  for LAt := 0 to High(FDeclarations) do
  begin

    if FDeclarations[LAt].Name = AName.Name then
    begin
      Exit;
    end;
  end;

  if DeclarationCount >= 64 then
  begin
    raise ENyxPascalContext.Create('Conditional declaration requests exceed 64');
  end;
  LAt := DeclarationCount;
  SetLength(FDeclarations, LAt + 1);
  FDeclarations[LAt] := AName;
end;

function PlanNyxPascalCondition(const AContext: INyxPascalCompilerContext;
  const AExpression: TNyxText): INyxPascalConditionNeeds;
var
  LParser: TConditionParser;
  LNeeds: TConditionNeeds;
  LOwner: INyxPascalConditionNeeds;
  LAnalysis: TNyxPascalConditionResult;
begin
  LNeeds := TConditionNeeds.Create;
  LOwner := LNeeds;
  LParser := TConditionParser.Create;
  try
    LParser.FNeeds := LNeeds;
    LAnalysis := LParser.Analyze(AContext, AExpression);
    LNeeds.FSupported := LAnalysis.Issue in [pciNone, pciUnknownSymbol];

    if not LNeeds.FSupported then
    begin
      SetLength(LNeeds.FSymbols, 0);
      SetLength(LNeeds.FSizes, 0);
      SetLength(LNeeds.FDeclarations, 0);
    end;
    Result := LOwner;
  finally
    LParser.Free;
  end;
end;

end.
