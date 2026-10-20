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

const
  { Ordered warning requests have a separate bounded capture quota. This is not
    a warning catalog or a claim that the configured compiler accepts every ID. }
  NyxPascalWarningRuleLimit = 2048;
  { Independent construction quotas for exact authored CODEALIGN groups. These
    bound analyzer storage, not the configured compiler's source language. }
  NyxPascalCodeAlignmentAssignmentLimit = 64;
  NyxPascalCodeAlignmentRuleLimit = 2048;
  { Independent quota for authored optimization requests; this bounds analyzer
    storage without asserting a compiler's accepted source size or pass set. }
  NyxPascalOptimizationRuleLimit = 2048;
  { Structural measurements have their own bounded namespace. A key is data,
    never a Pascal identifier or permission to compile caller-supplied source. }
  NyxPascalStorageFactLimit = 64;
  NyxPascalStorageKeyLimit = 1048576;
  NyxPascalStorageKeysLimit = 4194304;

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
  { Copied initial compiler-profile FPU selector. Empty means uncaptured.
    A name is a bounded compiler selector atom, not permission to choose instructions,
    an effective source setting, target support or a floating representation. }
  TNyxPascalFPUReference = record
  private
    FName: TNyxText;
  public
    function Known: Boolean;
    property Name: TNyxText read FName;
  end;
  { Source selection history is distinct from configured-profile history.
    Configured means no authored selection yet; Unknown revokes that fallback. }
  TNyxPascalSourceFPUKind = (pfsUnknown, pfsConfigured, pfsSelected);
  { Value-only selection, owning no interfaces or execution state. Its selected
    atom records context/source meaning, not target support or a float format. }
  TNyxPascalSourceFPUSelection = record
  private
    FKind: TNyxPascalSourceFPUKind;
    FReference: TNyxPascalFPUReference;
  public
    class function Unknown: TNyxPascalSourceFPUSelection; static;
    class function Configured: TNyxPascalSourceFPUSelection; static;
    class function Select(const AReference: TNyxPascalFPUReference): TNyxPascalSourceFPUSelection; static;
    procedure Validate;
    function SameSelection(const AOther: TNyxPascalSourceFPUSelection): Boolean;
    property Kind: TNyxPascalSourceFPUKind read FKind;
    property Reference: TNyxPascalFPUReference read FReference;
  end;
  { Numeric message IDs and named warning groups are distinct open namespaces.
    A group may map to several numeric messages in a particular compiler. }
  TNyxPascalWarningReferenceKind = (pwrNumber, pwrName);
  { Validated warning selector, not a compiler-catalog membership assertion.
    Names are canonical ASCII identifiers; Number refuses for a named group.
    The default empty record is invalid until created by a typed constructor. }
  TNyxPascalWarningRef = record
  private
    FKind: TNyxPascalWarningReferenceKind;
    FName: TNyxText;
    FNumber: Integer;
  public
    procedure Validate;
    function Number: Integer;
    function SameReference(const AOther: TNyxPascalWarningRef): Boolean;
    property Kind: TNyxPascalWarningReferenceKind read FKind;
    property Name: TNyxText read FName;
  end;
  { Authored per-warning request. Unknown records missing knowledge; none of
    these values computes effective verbosity or compiler alias membership. }
  TNyxPascalWarningAction = (pwaUnknown, pwaOn, pwaOff, pwaError);
  { Ordered value-only request. Activity preserves uncertain conditional writes
    without replacing the authored action. No source/context/interface is owned;
    source locations belong to a translation's separate directive receipts. }
  TNyxPascalWarningRule = record
  private
    FReference: TNyxPascalWarningRef;
    FAction: TNyxPascalWarningAction;
    FActivity: TNyxPascalKnowledge;
  public
    property Reference: TNyxPascalWarningRef read FReference;
    property Action: TNyxPascalWarningAction read FAction;
    property Activity: TNyxPascalKnowledge read FActivity;
  end;
  { Closed FPC CODEALIGN selectors. Record minimum/maximum code-generation
    requests are distinct from PACKRECORDS and do not measure a record ABI. }
  TNyxPascalCodeAlignmentSubject = (pcaProcedure, pcaJump, pcaLoop,
    pcaConstantMinimum, pcaConstantMaximum, pcaVariableMinimum, pcaVariableMaximum,
    pcaLocalMinimum, pcaLocalMaximum, pcaRecordMinimum, pcaRecordMaximum);
  { One exact signed-32 assignment. Zero is an authored value, never unknown.
    The compiler owns its eventual interpretation against target defaults. }
  TNyxPascalCodeAlignmentAssignment = record
  private
    FSubject: TNyxPascalCodeAlignmentSubject;
    FValue: Integer;
  public
    property Subject: TNyxPascalCodeAlignmentSubject read FSubject;
    property Value: Integer read FValue;
  end;
  { Immutable value-owned assignment group for one CODEALIGN request. Add copies
    its private scalar vector; duplicate selectors and order remain exact because
    FPC builds a temporary constraint packet separately for every directive.
    At most 64 assignments; an empty group cannot enter a context. No interfaces,
    source, compiler or effective layout is retained. }
  TNyxPascalCodeAlignmentRequest = record
  private
    FAssignments: array of TNyxPascalCodeAlignmentAssignment;
  public
    { Append one typed assignment. Procedure/jump/loop accept zero or powers of
      two through 256; other subjects preserve the compiler's signed-32 domain.
      Invalid values/subjects and excess assignments refuse before publication. }
    function Add(ASubject: TNyxPascalCodeAlignmentSubject;
      AValue: Integer): TNyxPascalCodeAlignmentRequest;
    { Number of authored assignments, including repeated selectors. }
    function Count: Integer;
    { Checked ordinal access; returned scalar values cannot mutate this group. }
    function Assignment(AIndex: Integer): TNyxPascalCodeAlignmentAssignment;
    { Exact ordered identity; empty/invalid groups refuse. }
    function SameRequest(const AOther: TNyxPascalCodeAlignmentRequest): Boolean;
    { Refuse an empty, excess or invalid request without changing its values. }
    procedure Validate;
  end;
  { Ordered authored request and conditional activity. Unknown activity retains
    both possible executions without claiming an effective alignment. Values
    contain no interface; the translation owns physical source receipts. }
  TNyxPascalCodeAlignmentRule = record
  private
    FRequest: TNyxPascalCodeAlignmentRequest;
    FActivity: TNyxPascalKnowledge;
  public
    property Request: TNyxPascalCodeAlignmentRequest read FRequest;
    property Activity: TNyxPascalKnowledge read FActivity;
  end;
  { Symbolic FPC optimizer resets. LevelTwo is ON, Disabled is OFF, Initial is
    DEFAULT. Target-specific membership and command-line initial flags remain
    compiler-owned; Unknown cannot construct an authored reset request. }
  TNyxPascalOptimizationMode = (pomUnknown, pomLevelTwo, pomDisabled, pomInitial);
  { Closed recognized FPC pass selectors, independent of target implementation
    and compiler admission. Level selectors toggle individual compiler flags;
    they do not expand into a preset. Whole-program optimization is separate. }
  TNyxPascalOptimizationFeature = (pofLevel1, pofLevel2, pofLevel3, pofLevel4,
    pofRegisterVariables, pofUncertain, pofSize, pofStackFrames, pofPeephole,
    pofLoopUnrolling, pofTailRecursion, pofCommonSubexpressions, pofDataFlow,
    pofLoopStrength, pofInstructionScheduling, pofAutomaticInlining, pofUseEBP,
    pofUseRBP, pofReorderFields, pofFastMath, pofDeadValues, pofRemoveEmptyProcedures,
    pofConstantPropagation, pofDeadStores, pofForceNoStackFrame, pofLoadModifyStore,
    pofUnusedParameters, pofConstants, pofForLoops);
  { Reset and single-pass toggle are distinct operations. Unknown keeps an
    unconstructed zero record invalid instead of silently requesting OFF. }
  TNyxPascalOptimizationRequestKind = (porkUnknown, porkMode, porkFeature);
  { Value-only authored optimizer operation. Checked accessors refuse the other
    variant; no target pass set, compiler, source or interface is retained. }
  TNyxPascalOptimizationRequest = record
  private
    FKind: TNyxPascalOptimizationRequestKind;
    FMode: TNyxPascalOptimizationMode;
    FFeature: TNyxPascalOptimizationFeature;
    FEnabled: Boolean;
  public
    { Construct a validated symbolic reset, independent of target defaults. }
    class function Select(AMode: TNyxPascalOptimizationMode): TNyxPascalOptimizationRequest; static;
    { Construct one typed pass request; false is an explicit disable request. }
    class function Toggle(AFeature: TNyxPascalOptimizationFeature;
      AEnabled: Boolean): TNyxPascalOptimizationRequest; static;
    { Refuse an empty/invalid variant or noncanonical unused fields. }
    procedure Validate;
    { Exact request identity, without inferring equivalent generated code. }
    function SameRequest(const AOther: TNyxPascalOptimizationRequest): Boolean;
    { Checked variant access; a toggle has no reset mode. }
    function RequestedMode: TNyxPascalOptimizationMode;
    { Checked variant access; a reset has no pass selector or enable value. }
    function Feature: TNyxPascalOptimizationFeature;
    { Checked explicit toggle value; a reset never supplies a fabricated flag. }
    function Enabled: Boolean;
    property Kind: TNyxPascalOptimizationRequestKind read FKind;
  end;
  { An exact optimizer request and conditional activity, retaining chronology
    through immutable context snapshots. Pending compiler activation and effective
    pass selection remain compiler-owned. Unknown activity grants no effective
    selection. Source positions belong to translation receipts. }
  TNyxPascalOptimizationRule = record
  private
    FRequest: TNyxPascalOptimizationRequest;
    FActivity: TNyxPascalKnowledge;
  public
    property Request: TNyxPascalOptimizationRequest read FRequest;
    property Activity: TNyxPascalKnowledge read FActivity;
  end;
  { Compiler evidence namespace. Bootstrap facts precede authored System aliases;
    pristine programs may already import those aliases. This provenance grants
    no source-edit, compiler-admission or application-execution authority. }
  TNyxPascalCompilerScope = (pcsoPristine, pcsoSystemBootstrap);
  { Explicit language grammar. Unknown preserves an unsupported source mode;
    source directives may change it independently of the analyzer's compiler. }
  TNyxPascalLanguageMode = (ppmUnknown, ppmFPC, ppmObjFPC, ppmDelphi,
    ppmDelphiUnicode, ppmTP, ppmMacPas, ppmISO, ppmExtendedPascal);
  { Grammar-sensitive switches with independently known on/off values. This list
    does not pretend to enumerate every FPC switch; unknown directives diagnose. }
  { Implicit specialization remains unknown until supplied by an exact capture
    or a qualified effective source directive; compiler capability/version alone
    does not enable it. Its module lifetime is separate from local settings. }
  TNyxPascalLanguageFeature = (ppfAdvancedRecords, ppfNestedProcVars,
    ppfAnonymousFunctions, ppfFunctionReferences, ppfTypeHelpers,
    ppfNestedComments, ppfScopedEnums, ppfUnicodeStrings, ppfOutParameters,
    ppfReplacementMacros, ppfClasses, ppfCVariables,
    ppfImplicitFunctionSpecialization);
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
  { Declared assembler reader intent. Default retains the configured target's
    selection rather than guessing Intel/AT&T from this analyzer's compiler.
    This value proves neither target availability nor instruction admission. }
  TNyxPascalAssemblerReader = (parUnknown, parDefault, parIntel, parATT);
  { Authored routine-codegen request, not a source type size or effective global
    register allocation. DEFAULT/NORMAL share one policy; Unknown never implies
    a zero-register request.
    Eight is retained for explicitly qualified FPC 3.3.1-or-newer contexts. }
  TNyxPascalFPURegisterPolicy = (pfrUnknown, pfrDefault, pfr0, pfr1, pfr2,
    pfr3, pfr4, pfr5, pfr6, pfr7, pfr8);
  { Closed supported local short switches. Global options such as debug output
    are deliberately outside this restore set; the wire spelling stays private. }
  TNyxPascalLocalSwitch = (plsBooleanEvaluation, plsAssertions, plsLongStrings,
    plsIOChecks, plsWritableConstants, plsOverflowChecks, plsRangeChecks,
    plsStackChecks, plsTypedAddress, plsVarStrings, plsStackFrames);
  { Closed source-level diagnostic categories. These verbosity requests are
    separate from numeric/named WARN rules; errors are never a suppressible
    category here. No compiler's default or message activation time is inferred. }
  TNyxPascalDiagnosticChannel = (pdcWarnings, pdcHints, pdcNotes);
  { Authored FPC pointer-check request only. Default names the owning compiler's
    initial setting, not a guessed Boolean. Unknown also covers uncertain source
    effects; no value proves target support, pointer validity or emitted checks. }
  TNyxPascalPointerCheckingRequest = (ppcrUnknown, ppcrDefault, ppcrOff, ppcrOn);
  { Captured or explicitly authored interface contract. Unknown is not a COM
    default; target support, ABI and reference-count execution stay separate. }
  TNyxPascalInterfaceStyle = (pifsUnknown, pifsCOM, pifsCORBA);
  { Authored minimum precision for real constants, not a host float format or
    rounding observation. DEFAULT retains its source intent; Unknown supplies
    no startup default. Qualified FPC source gives DEFAULT its separate meaning. }
  TNyxPascalRealPrecisionRequest = (prprUnknown, prprDefault, prprBinary32, prprBinary64);
  { Value-owned local and module policies, independent of renderer/ABI types.
    Zero initialization means unknown. Fluent copies retain exact symbolic
    values; they never infer a compiler's defaults or target capabilities.
    Diagnostic emission retains three scalar knowledge values, with no array or
    interface alias. Per-message rules and eventual emitted severity stay separate. }
  TNyxPascalCompilerPolicies = record
  private
    FInlining: TNyxPascalKnowledge;
    FRecords: TNyxPascalRecordAlignment;
    FEnums: TNyxPascalEnumStorage;
    FSets: TNyxPascalSetPacking;
    FCalling: TNyxPascalCallingConvention;
    FSmartLinking: TNyxPascalKnowledge;
    FGoto: TNyxPascalKnowledge;
    FAssembler: TNyxPascalAssemblerReader;
    FWarnings: TNyxPascalKnowledge;
    FHints: TNyxPascalKnowledge;
    FNotes: TNyxPascalKnowledge;
    FPointerChecking: TNyxPascalPointerCheckingRequest;
    FInterfaces: TNyxPascalInterfaceStyle;
    FExcessPrecision: TNyxPascalKnowledge;
    FModeExcessPrecision: TNyxPascalKnowledge;
    FMinimumRealPrecision: TNyxPascalRealPrecisionRequest;
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
    { Module GOTO/LABEL request, independent of local PUSH/POP. Unknown grants
      no dialect admission; this is not proof that a jump has a legal target. }
    function RequestGoto(AValue: TNyxPascalKnowledge): TNyxPascalCompilerPolicies;
    { Local reader intent participates in saved settings. No instruction parser
      or compiler callback is retained by this independent value copy. }
    function ReadAssemblyBy(AValue: TNyxPascalAssemblerReader): TNyxPascalCompilerPolicies;
    { Set one category's requested emission knowledge in an independent value.
      Unknown revokes knowledge; neither False nor True resolves message-specific
      WARN actions, compiler availability or delayed activation. Invalid enums
      refuse before any caller-owned value changes. }
    function EmitDiagnostics(AChannel: TNyxPascalDiagnosticChannel;
      AValue: TNyxPascalKnowledge): TNyxPascalCompilerPolicies;
    { Read one validated category; a missing capture remains Unknown. }
    function DiagnosticEmission(AChannel: TNyxPascalDiagnosticChannel): TNyxPascalKnowledge;
    { Local intent participates in capture, exact identity and saved settings.
      Pending activation and the meaning of Default remain compiler-owned. }
    function RequestPointerChecking(AValue: TNyxPascalPointerCheckingRequest): TNyxPascalCompilerPolicies;
    { An independent source/capture value. In the inspected FPC this module
      setting survives MODE and local PUSH/POP; missing defaults stay unknown. }
    function UseInterfaces(AValue: TNyxPascalInterfaceStyle): TNyxPascalCompilerPolicies;
    { Local authored request, independent of target format/FPU choice and
      pending activation. Even Unknown supersedes an earlier mode effect.
      PUSH/POP restores both provenance channels; InMode alone seeds neither. }
    function RequestExcessPrecision(AValue: TNyxPascalKnowledge): TNyxPascalCompilerPolicies;
    { A separately qualified source MODE effect, never an inferred startup
      default. Applying it supersedes the earlier explicit request. Callers
      still own compiler/version/header proof and node activation timing. }
    function ApplyModeExcessPrecision(AValue: TNyxPascalKnowledge): TNyxPascalCompilerPolicies;
    { Read the latest source/capture setting without conflating a MODE effect
      with an explicit EXCESSPRECISION request. This is not a node's effective
      flag: queued local switches retain their scanner activation contract. }
    function ExcessPrecisionSetting: TNyxPascalKnowledge;
    { Independent module setting. The qualified FPC scanner applies it before
      parsing the next real token, retaining it across local PUSH/POP and MODE.
      This request neither predicts a rounded value nor grants literal typing. }
    function RequestMinimumRealPrecision(AValue: TNyxPascalRealPrecisionRequest): TNyxPascalCompilerPolicies;
    function SamePolicies(const AOther: TNyxPascalCompilerPolicies): Boolean;
    procedure Validate;
    property Inlining: TNyxPascalKnowledge read FInlining;
    property RecordAlignment: TNyxPascalRecordAlignment read FRecords;
    property EnumStorage: TNyxPascalEnumStorage read FEnums;
    property SetPacking: TNyxPascalSetPacking read FSets;
    property CallingConvention: TNyxPascalCallingConvention read FCalling;
    property SmartLinkingRequested: TNyxPascalKnowledge read FSmartLinking;
    property GotoRequested: TNyxPascalKnowledge read FGoto;
    property AssemblerReader: TNyxPascalAssemblerReader read FAssembler;
    property PointerCheckingRequested: TNyxPascalPointerCheckingRequest read FPointerChecking;
    property InterfaceStyle: TNyxPascalInterfaceStyle read FInterfaces;
    property ExcessPrecisionRequested: TNyxPascalKnowledge read FExcessPrecision;
    property ModeExcessPrecision: TNyxPascalKnowledge read FModeExcessPrecision;
    property MinimumRealPrecisionRequested: TNyxPascalRealPrecisionRequest read FMinimumRealPrecision;
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
  { Exact structural/context receipt, separate from named primitive queries.
    Its bounded printable ASCII value contains no executable-source authority.
    Default/empty is invalid; construction validates before publication. }
  TNyxPascalStorageKey = record
  private
    FValue: TNyxText;
  public
    procedure Validate;
    function SameKey(const AOther: TNyxPascalStorageKey): Boolean;
    property Value: TNyxText read FValue;
  end;
  { A complete bound shape cannot be absent like a pristine name. Unknown and
    out-of-range remain distinct from a measured zero-byte empty record. }
  TNyxPascalStorageFactState = (psfsUnknown, psfsKnown, psfsOutOfRange);
  { Copied measurement only. Per-node environments belong to its separate key;
    one fabricated global mode/H cannot stand in for those distinct settings.
    No source/context/interface or earlier evidence owner is retained. }
  TNyxPascalStorageFact = record
  private
    FState: TNyxPascalStorageFactState;
    FBytes: Integer;
  public
    procedure Validate;
    function Bytes: Integer;
    function SameFact(const AOther: TNyxPascalStorageFact): Boolean;
    property State: TNyxPascalStorageFactState read FState;
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
  { Optional source-validated byte value for a conditional expression. It carries
    no synthetic pristine name or single global probe mode/H. Providers validate
    actual primitive or structural provenance before returning Known. Legacy
    providers keep their original interface and measurement validation contract. }
  INyxPascalConditionByteSizes = interface(IInterface)
    ['{CC12ECAC-1959-4BD8-A232-192BE42BF470}']
    function SizeOfBytes(const AName: TNyxPascalSizeReference): TNyxPascalStorageFact;
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
      concrete defaults afterward; none are inferred from the analyzer's mode.
      Qualified FPC 3.3.1 mode transitions retain the previous symbolic record
      packing request, as the compiler does; Unknown never becomes Default. }
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
  { Optional immutable compiler-profile observation, separate from mutable
    source defines and local settings. Replacing it is an explicit caller fact;
    this capability captures no host, producer or compiler process authority. }
  INyxPascalContextConfiguredFPU = interface(IInterface)
    ['{F309D792-7293-4315-B75D-78A6D6B9F8F2}']
    function ConfiguredFPU: TNyxPascalFPUReference;
    function WithConfiguredFPU(const AValue: TNyxPascalFPUReference): INyxPascalCompilerContext;
  end;
  { Optional source-selection lifetime, independent of initial configuration.
    Values are detached; a missing capability conveys unknown source history. }
  INyxPascalContextSourceFPU = interface(IInterface)
    ['{0FC29C44-72EF-496A-B062-2AC770E3426A}']
    function SourceFPU: TNyxPascalSourceFPUSelection;
    function WithSourceFPU(const AValue: TNyxPascalSourceFPUSelection): INyxPascalCompilerContext;
  end;
  { Optional typed policy capture keeps the original context interface/GUID
    intact. Returned values are detached; updates return a new context owner. }
  INyxPascalContextPolicies = interface(IInterface)
    ['{98349E7C-C0ED-48DE-A518-427A04570C07}']
    function Policies: TNyxPascalCompilerPolicies;
    function WithPolicies(const AValue: TNyxPascalCompilerPolicies): INyxPascalCompilerContext;
  end;
  { Optional immutable ordered warning requests. Repeated and overlapping named/
    numeric selectors retain chronology; this is not an effective-level map.
    Updates own detached bounded vectors. WithWarningRules freezes the producer's
    complete vector during the caller-serialized call and retains no producer. }
  INyxPascalContextWarnings = interface(IInterface)
    ['{276363B7-C055-4197-BB64-70BE562E2287}']
    function WarningCount: Integer;
    function WarningAt(AIndex: Integer): TNyxPascalWarningRule;
    function WithWarning(const AReference: TNyxPascalWarningRef;
      AAction: TNyxPascalWarningAction;
      AActivity: TNyxPascalKnowledge = ppkTrue): INyxPascalCompilerContext;
    function WithWarningRules(const ASource: INyxPascalContextWarnings): INyxPascalCompilerContext;
  end;
  { Optional immutable CODEALIGN request history, bounded to 2048 groups.
    Local PUSH/POP restores this vector; source mode changes retain it and unknown
    side effects revoke it. Group boundaries, assignment order and conditional
    activity are exact context identity, not a computed target constraint map.
    Replacement freezes an alternate producer and retains no producer lease. }
  INyxPascalContextCodeAlignments = interface(IInterface)
    ['{2706046A-DAD3-48E8-BA46-C0B9663C7493}']
    { Number of retained authored groups; absence grants no defaults. }
    function CodeAlignmentCount: Integer;
    { Checked ordinal access to an immutable, value-owned rule. }
    function CodeAlignmentAt(AIndex: Integer): TNyxPascalCodeAlignmentRule;
    { Append a validated complete group in a new context; preserve this owner. }
    function WithCodeAlignment(const ARequest: TNyxPascalCodeAlignmentRequest;
      AActivity: TNyxPascalKnowledge = ppkTrue): INyxPascalCompilerContext;
    { Freeze and replace the full vector atomically, never retain its producer. }
    function WithCodeAlignmentRules(
      const ASource: INyxPascalContextCodeAlignments): INyxPascalCompilerContext;
  end;
  { Optional immutable optimizer request history, bounded independently to 2048
    rows. PUSH/POP restores it; mode copies retain it; unknown effects revoke it.
    Requests remain symbolic, including ON/DEFAULT resets and uncertain writes.
    Freezing an alternate producer retains no producer/source/editor reference. }
  INyxPascalContextOptimizations = interface(IInterface)
    ['{519A22BF-6AE1-40A8-957F-D41E547E09C4}']
    { Retained authored rows; absence grants no effective optimizer defaults. }
    function OptimizationCount: Integer;
    { Checked ordinal access to an independent immutable value. }
    function OptimizationAt(AIndex: Integer): TNyxPascalOptimizationRule;
    { Append one validated typed request atomically in a new context. }
    function WithOptimization(const ARequest: TNyxPascalOptimizationRequest;
      AActivity: TNyxPascalKnowledge = ppkTrue): INyxPascalCompilerContext;
    { Freeze and replace the complete vector; failures preserve this owner. }
    function WithOptimizationRules(
      const ASource: INyxPascalContextOptimizations): INyxPascalCompilerContext;
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
  { Optional structural evidence, independent of the primitive-name namespace.
    Exact ordered values participate in capture/identity. Mode/local-setting
    changes retain the evidence; applicability still requires the entire key.
    Unknown effects revoke it. Alternate getters are frozen before publication. }
  INyxPascalContextStorage = interface(IInterface)
    ['{E7B10976-32B5-4CC8-872A-448E98CC9E02}']
    function StorageCount: Integer;
    function StorageKey(AIndex: Integer): TNyxPascalStorageKey;
    function StorageValueAt(AIndex: Integer): TNyxPascalStorageFact;
    function StorageFact(const AKey: TNyxPascalStorageKey): TNyxPascalStorageFact;
    function WithStorageFact(const AKey: TNyxPascalStorageKey;
      const AValue: TNyxPascalStorageFact): INyxPascalCompilerContext;
  end;

type
  { Optional immutable compiler-root evidence. Source contexts may retain one
    detached bootstrap context; bootstrap contexts cannot retain another context.
    This one-way class-owned lease survives grammar mode changes without
    reassigning primitive identity to an alias's or consumer's source settings. }
  INyxPascalCompilerScope = interface(IInterface)
    ['{98349E7C-C0ED-48DE-A518-427A04570C16}']
    { Namespace of this packet, independent of its current source mode. }
    function CaptureScope: TNyxPascalCompilerScope;
    { Retain the immutable detached root packet, or nil when not supplied. }
    function BootstrapContext: INyxPascalCompilerContext;
    { Clone with one matching root packet; nil detaches, reverse/nested leases refuse. }
    function WithBootstrapContext(const AContext: INyxPascalCompilerContext): INyxPascalCompilerContext;
  end;

{ Legacy contexts report pristine; invalid namespace values refuse. }
function NyxPascalCompilerScope(const AContext: INyxPascalCompilerContext): TNyxPascalCompilerScope;
{ Retain the immutable root packet; legacy contexts have none. }
function NyxPascalBootstrapContext(const AContext: INyxPascalCompilerContext): INyxPascalCompilerContext;
{ Freeze and attach distinct matching root evidence; nested/cyclic leases refuse.
  This does not establish SDK/target coherence beyond supplied captured facts. }
function NyxPascalWithBootstrapContext(const AContext, ABootstrap: INyxPascalCompilerContext): INyxPascalCompilerContext;

function NyxPascalSizeReference(const AName: TNyxText): TNyxPascalSizeReference;
{ Validate one bounded ASCII declaration identifier, without compiler-symbol or
  member-expression meaning. Reserved grammar words refuse. }
function NyxPascalDeclaration(const AName: TNyxText): TNyxPascalDeclarationRef;
function NyxPascalSizeFact(AState: TNyxPascalSizeState;
  AMode: TNyxPascalLanguageMode = ppmUnknown; ALongStrings: TNyxPascalKnowledge = ppkUnknown;
  ABytes: Integer = 0): TNyxPascalSizeFact;
{ Read/write measurements in the context's explicit evidence namespace; neither
  helper resolves an authored type binding. Legacy contexts have no captured sizes. }
function NyxPascalCapturedSize(const AContext: INyxPascalCompilerContext;
  const AName: TNyxPascalSizeReference): TNyxPascalSizeFact;
function NyxPascalWithCapturedSize(const AContext: INyxPascalCompilerContext;
  const AName: TNyxPascalSizeReference; const AValue: TNyxPascalSizeFact): INyxPascalCompilerContext;
{ Validate detached receipt data. These constructors make no compiler/provenance
  claim; an explicit host/caller must supply the actual matching measurement. }
function NyxPascalStorageKey(const AValue: TNyxText): TNyxPascalStorageKey;
function NyxPascalStorageFact(AState: TNyxPascalStorageFactState;
  ABytes: Integer = 0): TNyxPascalStorageFact;
{ An absent structural capability is Unknown, never a name-keyed fallback. }
function NyxPascalCapturedStorage(const AContext: INyxPascalCompilerContext;
  const AKey: TNyxPascalStorageKey): TNyxPascalStorageFact;
{ Atomically add/replace one exact structural measurement in a new context.
  This is explicit captured data, not native fulfillment or compiler execution. }
function NyxPascalWithCapturedStorage(const AContext: INyxPascalCompilerContext;
  const AKey: TNyxPascalStorageKey;
  const AValue: TNyxPascalStorageFact): INyxPascalCompilerContext;

{ Legacy contexts carry unknown policies. Alternate values are validated before
  capture; no default is borrowed from the analyzer's own build. }
function NyxPascalCompilerPolicies(const AContext: INyxPascalCompilerContext): TNyxPascalCompilerPolicies;
function NyxPascalWithCompilerPolicies(const AContext: INyxPascalCompilerContext;
  const AValue: TNyxPascalCompilerPolicies): INyxPascalCompilerContext;
{ Positive signed-32 message ID; membership stays with the configured compiler. }
function NyxPascalWarningNumber(AValue: Integer): TNyxPascalWarningRef;
{ Canonical open named group, bounded like compiler symbols. No alias expansion. }
function NyxPascalWarningName(const AName: TNyxText): TNyxPascalWarningRef;
{ Retain the immutable warning capability. Legacy contexts supply an empty
  detached capture; absence grants no default effective diagnostic level. }
function NyxPascalWarnings(const AContext: INyxPascalCompilerContext): INyxPascalContextWarnings;
{ Freeze the context and append one typed request atomically; no caller changes.
  Order is preserved even when a named selector could overlap a numeric one. }
function NyxPascalWithWarning(const AContext: INyxPascalCompilerContext;
  const AReference: TNyxPascalWarningRef; AAction: TNyxPascalWarningAction;
  AActivity: TNyxPascalKnowledge = ppkTrue): INyxPascalCompilerContext;
{ Retain the immutable authored alignment capability. Legacy contexts have an
  empty detached vector, which grants no target-default/effective-layout facts. }
function NyxPascalCodeAlignments(
  const AContext: INyxPascalCompilerContext): INyxPascalContextCodeAlignments;
{ Freeze the context and append one whole typed request atomically. Existing
  requests, caller-owned target facts and independent snapshots remain exact. }
function NyxPascalWithCodeAlignment(const AContext: INyxPascalCompilerContext;
  const ARequest: TNyxPascalCodeAlignmentRequest;
  AActivity: TNyxPascalKnowledge = ppkTrue): INyxPascalCompilerContext;
{ Retain immutable authored optimizer requests. A legacy context supplies an empty
  detached capture without inventing target defaults or effective pass membership. }
function NyxPascalOptimizations(
  const AContext: INyxPascalCompilerContext): INyxPascalContextOptimizations;
{ Freeze the caller's facts and append one typed request without retaining an
  alternate producer or changing independent context snapshots. }
function NyxPascalWithOptimization(const AContext: INyxPascalCompilerContext;
  const ARequest: TNyxPascalOptimizationRequest;
  AActivity: TNyxPascalKnowledge = ppkTrue): INyxPascalCompilerContext;
{ Friendly closed short-switch mapping; parsing source spelling is a separate
  boundary. Unknown remains a valid explicitly captured fact. }
function NyxPascalLocalSwitchOption(AValue: TNyxPascalLocalSwitch): Char;
function NyxPascalWithLocalSwitch(const AContext: INyxPascalCompilerContext;
  ASwitch: TNyxPascalLocalSwitch; AValue: TNyxPascalKnowledge): INyxPascalCompilerContext;
{ Restore local policies/short switches/scoped-enum state and authored warning/
  alignment/optimizer requests. Keep current symbol facts, global mode/features and macros.
  Warning requests do not predict compiler-specific delayed message activation;
  emitted diagnostics remain compiler-owned. No replacement table is restored. }
function RestoreNyxPascalLocalSettings(const ACurrent, ASaved: INyxPascalCompilerContext): INyxPascalCompilerContext;

{ Reject empty/over-128/non-ASCII names and identifiers starting with a digit. }
function NyxPascalDefine(const AName: TNyxText): TNyxPascalDefineRef;
{ Pure metadata constructor with unknown options/features and an open define set.
  No implicit FPC/PAS2JS/platform/version define is read from this build process. }
{ An explicit bounded selector, normalized like other compiler references.
  Default(TNyxPascalFPUReference) is the separate uncaptured value. }
function NyxPascalFPU(const AName: TNyxText): TNyxPascalFPUReference;
{ Legacy/absent capabilities return uncaptured. A present alternate value is
  validated and copied before publication, without retaining its producer. }
function NyxPascalConfiguredFPU(const AContext: INyxPascalCompilerContext): TNyxPascalFPUReference;
function NyxPascalWithConfiguredFPU(const AContext: INyxPascalCompilerContext;
  const AValue: TNyxPascalFPUReference): INyxPascalCompilerContext;
{ Read source history, not an authored define's spelling. Resolving Configured
  uses the separate initial snapshot; Unknown never borrows that fallback. }
function NyxPascalSourceFPU(const AContext: INyxPascalCompilerContext): TNyxPascalSourceFPUSelection;
function NyxPascalSelectedFPU(const AContext: INyxPascalCompilerContext): TNyxPascalFPUReference;
function NyxPascalWithSourceFPU(const AContext: INyxPascalCompilerContext;
  const AValue: TNyxPascalSourceFPUSelection): INyxPascalCompilerContext;

function NewNyxPascalCompilerContext(AFamily: TNyxPascalCompilerFamily;
  const AVersion: TNyxPascalCompilerVersion;
  AMode: TNyxPascalLanguageMode;
  AScope: TNyxPascalCompilerScope = pcsoPristine): INyxPascalCompilerContext;
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
  LBootstrap: INyxPascalCompilerContext;
  LWarnings: INyxPascalContextWarnings;
  LTargetWarnings: INyxPascalContextWarnings;
  LAlignments: INyxPascalContextCodeAlignments;
  LTargetAlignments: INyxPascalContextCodeAlignments;
  LOptimizations: INyxPascalContextOptimizations;
  LTargetOptimizations: INyxPascalContextOptimizations;
  LStorage: INyxPascalContextStorage;
  LTargetStorage: INyxPascalContextStorage;
  LStorageKey: TNyxPascalStorageKey;
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
  LCaptured := NewNyxPascalCompilerContext(AContext.Family, AContext.Version, AContext.Mode,
    NyxPascalCompilerScope(AContext));
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
  LCaptured := NyxPascalWithConfiguredFPU(LCaptured, NyxPascalConfiguredFPU(AContext));
  LCaptured := NyxPascalWithSourceFPU(LCaptured, NyxPascalSourceFPU(AContext));

  if Supports(AContext, INyxPascalContextWarnings, LWarnings) then
  begin
    Supports(LCaptured, INyxPascalContextWarnings, LTargetWarnings);
    LCaptured := LTargetWarnings.WithWarningRules(LWarnings);
  end;

  if Supports(AContext, INyxPascalContextCodeAlignments, LAlignments) then
  begin
    Supports(LCaptured, INyxPascalContextCodeAlignments, LTargetAlignments);
    LCaptured := LTargetAlignments.WithCodeAlignmentRules(LAlignments);
  end;

  if Supports(AContext, INyxPascalContextOptimizations, LOptimizations) then
  begin
    Supports(LCaptured, INyxPascalContextOptimizations, LTargetOptimizations);
    LCaptured := LTargetOptimizations.WithOptimizationRules(LOptimizations);
  end;

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
  if Supports(AContext, INyxPascalContextStorage, LStorage) then
  begin
    LSizeCount := LStorage.StorageCount;

    if (LSizeCount < 0) or (LSizeCount > NyxPascalStorageFactLimit) then
    begin
      raise ENyxPascalContext.Create('Structural evidence exceeds its bounded collection');
    end;
    for LAt := 0 to LSizeCount - 1 do
    begin
      LStorageKey := LStorage.StorageKey(LAt);
      Supports(LCaptured, INyxPascalContextStorage, LTargetStorage);
      LCaptured := LTargetStorage.WithStorageFact(LStorageKey, LStorage.StorageValueAt(LAt));
      Supports(LCaptured, INyxPascalContextStorage, LTargetStorage);

      if LTargetStorage.StorageCount <> LAt + 1 then
      begin
        raise ENyxPascalContext.Create('Captured structural evidence contains duplicate keys');
      end;
    end;
  end;
  LBootstrap := NyxPascalBootstrapContext(AContext);

  if LBootstrap <> nil then
  begin
    LCaptured := NyxPascalWithBootstrapContext(LCaptured, LBootstrap);
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
  { Ordered value-only structural receipt. Managed key text is immutable; this
    record contains no interface or reverse link to a source/measurement owner. }
  TCapturedStorage = record
    Key: TNyxPascalStorageKey;
    Value: TNyxPascalStorageFact;
  end;
  { Persistent context implementation. Its index/vectors are detached on every
    change; readers may share only a fully constructed immutable instance.
    Warning/alignment/optimizer requests own ordered values without producer/
    source back references. These requests survive mode changes through Clone. }
  TCompilerContext = class(TInterfacedObject, INyxPascalCompilerContext, INyxPascalContextPolicies,
    INyxPascalContextSizes, INyxPascalCompilerScope, INyxPascalContextWarnings,
    INyxPascalContextCodeAlignments, INyxPascalContextOptimizations, INyxPascalContextStorage,
    INyxPascalContextConfiguredFPU, INyxPascalContextSourceFPU)
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
    FWarnings: array of TNyxPascalWarningRule;
    FCodeAlignments: array of TNyxPascalCodeAlignmentRule;
    FOptimizations: array of TNyxPascalOptimizationRule;
    FSizes: array of TCapturedSize;
    FStorage: array of TCapturedStorage;
    FCaptureScope: TNyxPascalCompilerScope;
    FBootstrap: INyxPascalCompilerContext;
    FConfiguredFPU: TNyxPascalFPUReference;
    FSourceFPU: TNyxPascalSourceFPUSelection;
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
    function ConfiguredFPU: TNyxPascalFPUReference;
    function WithConfiguredFPU(const AValue: TNyxPascalFPUReference): INyxPascalCompilerContext;
    function SourceFPU: TNyxPascalSourceFPUSelection;
    function WithSourceFPU(const AValue: TNyxPascalSourceFPUSelection): INyxPascalCompilerContext;
    function WarningCount: Integer;
    function WarningAt(AIndex: Integer): TNyxPascalWarningRule;
    function WithWarning(const AReference: TNyxPascalWarningRef;
      AAction: TNyxPascalWarningAction;
      AActivity: TNyxPascalKnowledge): INyxPascalCompilerContext;
    function WithWarningRules(const ASource: INyxPascalContextWarnings): INyxPascalCompilerContext;
    function CodeAlignmentCount: Integer;
    function CodeAlignmentAt(AIndex: Integer): TNyxPascalCodeAlignmentRule;
    function WithCodeAlignment(const ARequest: TNyxPascalCodeAlignmentRequest;
      AActivity: TNyxPascalKnowledge): INyxPascalCompilerContext;
    function WithCodeAlignmentRules(
      const ASource: INyxPascalContextCodeAlignments): INyxPascalCompilerContext;
    function OptimizationCount: Integer;
    function OptimizationAt(AIndex: Integer): TNyxPascalOptimizationRule;
    function WithOptimization(const ARequest: TNyxPascalOptimizationRequest;
      AActivity: TNyxPascalKnowledge): INyxPascalCompilerContext;
    function WithOptimizationRules(
      const ASource: INyxPascalContextOptimizations): INyxPascalCompilerContext;
    function SizeCount: Integer;
    function SizeName(AIndex: Integer): TNyxPascalSizeReference;
    function SizeFact(const AName: TNyxPascalSizeReference): TNyxPascalSizeFact;
    function WithSizeFact(const AName: TNyxPascalSizeReference;
      const AValue: TNyxPascalSizeFact): INyxPascalCompilerContext;
    function StorageCount: Integer;
    function StorageKey(AIndex: Integer): TNyxPascalStorageKey;
    function StorageValueAt(AIndex: Integer): TNyxPascalStorageFact;
    function StorageFact(const AKey: TNyxPascalStorageKey): TNyxPascalStorageFact;
    function WithStorageFact(const AKey: TNyxPascalStorageKey;
      const AValue: TNyxPascalStorageFact): INyxPascalCompilerContext;
    function CaptureScope: TNyxPascalCompilerScope;
    function BootstrapContext: INyxPascalCompilerContext;
    function WithBootstrapContext(const AContext: INyxPascalCompilerContext): INyxPascalCompilerContext;
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
  FSourceFPU := TNyxPascalSourceFPUSelection.Configured;
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
    Result.FConfiguredFPU := FConfiguredFPU;
    Result.FSourceFPU := FSourceFPU;
    SetLength(Result.FWarnings, Length(FWarnings));
    for LAt := 0 to High(FWarnings) do
    begin
      Result.FWarnings[LAt] := FWarnings[LAt];
    end;
    SetLength(Result.FCodeAlignments, Length(FCodeAlignments));
    for LAt := 0 to High(FCodeAlignments) do
    begin
      Result.FCodeAlignments[LAt] := FCodeAlignments[LAt];
    end;
    SetLength(Result.FOptimizations, Length(FOptimizations));
    for LAt := 0 to High(FOptimizations) do
    begin
      Result.FOptimizations[LAt] := FOptimizations[LAt];
    end;
    Result.FSizes := Copy(FSizes);
    Result.FStorage := Copy(FStorage);
    Result.FCaptureScope := FCaptureScope;
    Result.FBootstrap := FBootstrap;
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
    .RequestSmartLinking(FPolicies.SmartLinkingRequested)
    .RequestGoto(FPolicies.GotoRequested)
    { Source mode changes reset language policies but retain verbosity. }
    .EmitDiagnostics(pdcWarnings, FPolicies.DiagnosticEmission(pdcWarnings))
    .EmitDiagnostics(pdcHints, FPolicies.DiagnosticEmission(pdcHints))
    .EmitDiagnostics(pdcNotes, FPolicies.DiagnosticEmission(pdcNotes));

  if (FFamily = ppcFPC) and (FVersion.FullNumber = 30301) and
    (AValue in [ppmFPC, ppmObjFPC, ppmDelphi, ppmDelphiUnicode, ppmTP]) then
  begin
    { The configured scanner's SetCompileMode flushes pending switches before
      selecting enum/set defaults, but leaves packrecords intact. Retain that
      authored request independently of the reset policies. This supplies no
      initial packing fact and grants no target layout/offset authority. Other
      compiler versions/families keep the previous conservative reset. }
    LCopy.FPolicies := LCopy.FPolicies.AlignRecords(FPolicies.RecordAlignment)
      { This scanner flushes pending locals before changing mode, and neither
        mode handler replaces cs_checkpointer. Retain the authored request only
        for this inspected compiler/version; this supplies no effective flag. }
      .RequestPointerChecking(FPolicies.PointerCheckingRequested)
      { The same scanner does not replace its independent interface setting. }
      .UseInterfaces(FPolicies.InterfaceStyle)
      .RequestMinimumRealPrecision(FPolicies.MinimumRealPrecisionRequested);
  end;
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
  LCopy.FCaptureScope := FCaptureScope;
  { Initial profile metadata is not a current source setting. Unknown source
    effects revoke mutable knowledge without changing this earlier observation. }
  LCopy.FConfiguredFPU := FConfiguredFPU;
  LCopy.FSourceFPU := TNyxPascalSourceFPUSelection.Unknown;
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
  LBootstrap: INyxPascalCompilerContext;
  LWarnings: INyxPascalContextWarnings;
  LWarning: TNyxPascalWarningRule;
  LAlignments: INyxPascalContextCodeAlignments;
  LAlignment: TNyxPascalCodeAlignmentRule;
  LOptimizations: INyxPascalContextOptimizations;
  LOptimization: TNyxPascalOptimizationRule;
  LStorage: INyxPascalContextStorage;
begin
  Result := False;

  if (AOther = nil) or (AOther.Family <> FFamily) or
    (AOther.Version.FullNumber <> FVersion.FullNumber) or (AOther.Mode <> FMode) or
    (AOther.DefinesComplete <> FComplete) or (AOther.SymbolCount <> SymbolCount) then
  begin
    Exit;
  end;

  if NyxPascalCompilerScope(AOther) <> FCaptureScope then
  begin
    Exit;
  end;
  if NyxPascalConfiguredFPU(AOther).Name <> FConfiguredFPU.Name then Exit;
  if not FSourceFPU.SameSelection(NyxPascalSourceFPU(AOther)) then Exit;
  LBootstrap := NyxPascalBootstrapContext(AOther);

  if (LBootstrap = nil) <> (FBootstrap = nil) then
  begin
    Exit;
  end;

  if FBootstrap <> nil then
  begin

    if (NyxPascalCompilerScope(LBootstrap) <> pcsoSystemBootstrap) or
      (NyxPascalBootstrapContext(LBootstrap) <> nil) or
      not FBootstrap.SameFacts(LBootstrap) then
    begin
      Exit;
    end;
  end;

  if not FPolicies.SamePolicies(NyxPascalCompilerPolicies(AOther)) then
  begin
    Exit;
  end;

  if Supports(AOther, INyxPascalContextWarnings, LWarnings) then
  begin

    if LWarnings.WarningCount <> Length(FWarnings) then
    begin
      Exit;
    end;
    for LAt := 0 to High(FWarnings) do
    begin
      LWarning := LWarnings.WarningAt(LAt);

      if not FWarnings[LAt].Reference.SameReference(LWarning.Reference) or
        (FWarnings[LAt].Action <> LWarning.Action) or
        (FWarnings[LAt].Activity <> LWarning.Activity) then
      begin
        Exit;
      end;
    end;
  end
  else if Length(FWarnings) <> 0 then
  begin
    Exit;
  end;

  if Supports(AOther, INyxPascalContextCodeAlignments, LAlignments) then
  begin

    if LAlignments.CodeAlignmentCount <> Length(FCodeAlignments) then
    begin
      Exit;
    end;
    for LAt := 0 to High(FCodeAlignments) do
    begin
      LAlignment := LAlignments.CodeAlignmentAt(LAt);

      if not FCodeAlignments[LAt].Request.SameRequest(LAlignment.Request) or
        (FCodeAlignments[LAt].Activity <> LAlignment.Activity) then
      begin
        Exit;
      end;
    end;
  end
  else if Length(FCodeAlignments) <> 0 then
  begin
    Exit;
  end;

  if Supports(AOther, INyxPascalContextOptimizations, LOptimizations) then
  begin

    if LOptimizations.OptimizationCount <> Length(FOptimizations) then
    begin
      Exit;
    end;
    for LAt := 0 to High(FOptimizations) do
    begin
      LOptimization := LOptimizations.OptimizationAt(LAt);

      if not FOptimizations[LAt].Request.SameRequest(LOptimization.Request) or
        (FOptimizations[LAt].Activity <> LOptimization.Activity) then
      begin
        Exit;
      end;
    end;
  end
  else if Length(FOptimizations) <> 0 then
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
  if Supports(AOther, INyxPascalContextStorage, LStorage) then
  begin

    if LStorage.StorageCount <> Length(FStorage) then
    begin
      Exit;
    end;
    for LAt := 0 to High(FStorage) do
    begin

      if (FStorage[LAt].Key.Value <> LStorage.StorageKey(LAt).Value) or
        not FStorage[LAt].Value.SameFact(LStorage.StorageValueAt(LAt)) then
      begin
        Exit;
      end;
    end;
  end
  else if Length(FStorage) <> 0 then
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
  AMode: TNyxPascalLanguageMode; AScope: TNyxPascalCompilerScope): INyxPascalCompilerContext;
var
  LContext: TCompilerContext;
begin

  if (Ord(AFamily) < Ord(Low(TNyxPascalCompilerFamily))) or
    (Ord(AFamily) > Ord(High(TNyxPascalCompilerFamily))) or
    (Ord(AMode) < Ord(Low(TNyxPascalLanguageMode))) or
    (Ord(AMode) > Ord(High(TNyxPascalLanguageMode))) or
    (Ord(AScope) < Ord(Low(TNyxPascalCompilerScope))) or
    (Ord(AScope) > Ord(High(TNyxPascalCompilerScope))) or
    ((AScope = pcsoSystemBootstrap) and (AFamily <> ppcFPC)) then
  begin
    raise ENyxPascalContext.Create('Captured compiler identity is outside its closed choices');
  end;
  LContext := TCompilerContext.Create;
  Result := LContext;
  LContext.FFamily := AFamily;
  LContext.FVersion := AVersion;
  LContext.FMode := AMode;
  LContext.FCaptureScope := AScope;
end;

function NyxPascalCompilerScope(const AContext: INyxPascalCompilerContext): TNyxPascalCompilerScope;
var
  LScope: INyxPascalCompilerScope;
begin
  Result := pcsoPristine;

  if Supports(AContext, INyxPascalCompilerScope, LScope) then
  begin
    Result := LScope.CaptureScope;
  end;

  if (Ord(Result) < Ord(Low(TNyxPascalCompilerScope))) or
    (Ord(Result) > Ord(High(TNyxPascalCompilerScope))) then
  begin
    raise ENyxPascalContext.Create('Compiler evidence has an invalid capture scope');
  end;
end;

function NyxPascalBootstrapContext(const AContext: INyxPascalCompilerContext): INyxPascalCompilerContext;
var
  LScope: INyxPascalCompilerScope;
begin
  Result := nil;

  if Supports(AContext, INyxPascalCompilerScope, LScope) then
  begin
    Result := LScope.BootstrapContext;
  end;
end;

function NyxPascalWithBootstrapContext(const AContext, ABootstrap: INyxPascalCompilerContext): INyxPascalCompilerContext;
var
  LScope: INyxPascalCompilerScope;
  LCaptured: INyxPascalCompilerContext;
begin
  LCaptured := AContext;

  if not Supports(LCaptured, INyxPascalCompilerScope, LScope) then
  begin
    LCaptured := CaptureNyxPascalCompilerContext(AContext);
    Supports(LCaptured, INyxPascalCompilerScope, LScope);
  end;
  Result := LScope.WithBootstrapContext(ABootstrap);
end;

function TCompilerContext.CaptureScope: TNyxPascalCompilerScope;
begin
  Result := FCaptureScope;
end;

function TCompilerContext.BootstrapContext: INyxPascalCompilerContext;
begin
  Result := FBootstrap;
end;

function TCompilerContext.WithBootstrapContext(const AContext: INyxPascalCompilerContext): INyxPascalCompilerContext;
var
  LCaptured: INyxPascalCompilerContext;
  LCopy: TCompilerContext;
  LAt: Integer;
  LFact: TNyxPascalStorageFact;
begin
  LCaptured := nil;

  if AContext <> nil then
  begin

    if (FCaptureScope <> pcsoPristine) or
      (NyxPascalCompilerScope(AContext) <> pcsoSystemBootstrap) or
      (NyxPascalBootstrapContext(AContext) <> nil) or
      (AContext.Family <> FFamily) or
      (AContext.Version.FullNumber <> FVersion.FullNumber) then
    begin
      raise ENyxPascalContext.Create('Bootstrap evidence needs a distinct matching compiler-root context');
    end;
    LCaptured := CaptureNyxPascalCompilerContext(AContext);
    for LAt := 0 to High(FStorage) do
    begin
      LFact := NyxPascalCapturedStorage(LCaptured, FStorage[LAt].Key);

      if (LFact.State <> psfsUnknown) and (FStorage[LAt].Value.State <> psfsUnknown) and
        not LFact.SameFact(FStorage[LAt].Value) then
      begin
        raise ENyxPascalContext.Create('Root and module structural evidence conflict');
      end;
    end;
  end;
  LCopy := Clone;
  Result := LCopy;
  LCopy.FBootstrap := LCaptured;
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

procedure TNyxPascalStorageKey.Validate;
var
  LAt: Integer;
begin

  if (Length(FValue) < 15) or (Length(FValue) > NyxPascalStorageKeyLimit) or
    (Copy(FValue, 1, 14) <> 'nyx.storage.1|') then
  begin
    raise ENyxPascalContext.Create('A structural key needs a complete bounded receipt');
  end;
  for LAt := 1 to Length(FValue) do
  begin

    if (Ord(FValue[LAt]) < 32) or (Ord(FValue[LAt]) > 126) then
    begin
      raise ENyxPascalContext.Create('Structural keys contain printable ASCII data only');
    end;
  end;
end;

function NyxPascalStorageKey(const AValue: TNyxText): TNyxPascalStorageKey;
var
  LKey: TNyxPascalStorageKey;
begin
  LKey.FValue := AValue;
  LKey.Validate;
  Result := LKey;
end;

function TNyxPascalStorageKey.SameKey(const AOther: TNyxPascalStorageKey): Boolean;
begin
  Validate;
  AOther.Validate;
  Result := FValue = AOther.FValue;
end;

procedure TNyxPascalStorageFact.Validate;
begin

  if (Ord(FState) < Ord(Low(TNyxPascalStorageFactState))) or
    (Ord(FState) > Ord(High(TNyxPascalStorageFactState))) or
    (FBytes < 0) or ((FState <> psfsKnown) and (FBytes <> 0)) then
  begin
    raise ENyxPascalContext.Create('A structural measurement needs canonical checked values');
  end;
end;

function NyxPascalStorageFact(AState: TNyxPascalStorageFactState;
  ABytes: Integer): TNyxPascalStorageFact;
var
  LFact: TNyxPascalStorageFact;
begin
  LFact.FState := AState;
  LFact.FBytes := ABytes;
  LFact.Validate;
  Result := LFact;
end;

function TNyxPascalStorageFact.Bytes: Integer;
begin
  Validate;

  if FState <> psfsKnown then
  begin
    raise ENyxPascalContext.Create('An unmeasured structural result has no byte value');
  end;
  Result := FBytes;
end;

function TNyxPascalStorageFact.SameFact(const AOther: TNyxPascalStorageFact): Boolean;
begin
  Validate;
  AOther.Validate;
  Result := (FState = AOther.FState) and (FBytes = AOther.FBytes);
end;

function TCompilerContext.StorageCount: Integer;
begin Result := Length(FStorage); end;

function TCompilerContext.StorageKey(AIndex: Integer): TNyxPascalStorageKey;
begin

  if (AIndex < 0) or (AIndex >= Length(FStorage)) then
  begin
    raise ENyxPascalContext.Create('Structural evidence index is outside its context');
  end;
  Result := FStorage[AIndex].Key;
end;

function TCompilerContext.StorageFact(const AKey: TNyxPascalStorageKey): TNyxPascalStorageFact;
var
  LAt: Integer;
begin
  AKey.Validate;
  Result := NyxPascalStorageFact(psfsUnknown);
  for LAt := 0 to High(FStorage) do
  begin

    if FStorage[LAt].Key.FValue = AKey.FValue then
    begin
      Exit(FStorage[LAt].Value);
    end;
  end;
end;

function TCompilerContext.StorageValueAt(AIndex: Integer): TNyxPascalStorageFact;
begin

  if (AIndex < 0) or (AIndex >= Length(FStorage)) then
  begin
    raise ENyxPascalContext.Create('Structural evidence index is outside its context');
  end;
  Result := FStorage[AIndex].Value;
end;

function TCompilerContext.WithStorageFact(const AKey: TNyxPascalStorageKey;
  const AValue: TNyxPascalStorageFact): INyxPascalCompilerContext;
var
  LAt, LIndex, LBytes: Integer;
  LCopy: TCompilerContext;
  LOwner: INyxPascalCompilerContext;
  LRootFact: TNyxPascalStorageFact;
begin
  AKey.Validate;
  AValue.Validate;

  if FBootstrap <> nil then
  begin
    LRootFact := NyxPascalCapturedStorage(FBootstrap, AKey);

    if (LRootFact.State <> psfsUnknown) and (AValue.State <> psfsUnknown) and
      not LRootFact.SameFact(AValue) then
    begin
      raise ENyxPascalContext.Create('Module and root structural evidence conflict');
    end;
  end;
  LIndex := -1;
  LBytes := Length(AKey.FValue);
  for LAt := 0 to High(FStorage) do
  begin

    if FStorage[LAt].Key.FValue = AKey.FValue then
    begin
      LIndex := LAt;
    end
    else
    begin

      if Length(FStorage[LAt].Key.FValue) > NyxPascalStorageKeysLimit - LBytes then
      begin
        raise ENyxPascalContext.Create('Structural keys exceed their aggregate context budget');
      end;
      Inc(LBytes, Length(FStorage[LAt].Key.FValue));
    end;
  end;

  if (LIndex < 0) and (Length(FStorage) >= NyxPascalStorageFactLimit) then
  begin
    raise ENyxPascalContext.Create('Structural evidence exceeds 64 context entries');
  end;
  LCopy := Clone;
  LOwner := LCopy;

  if LIndex < 0 then
  begin
    LIndex := Length(FStorage);
    SetLength(LCopy.FStorage, LIndex + 1);
  end;
  LCopy.FStorage[LIndex].Key := AKey;
  LCopy.FStorage[LIndex].Value := AValue;
  Result := LOwner;
end;

function NyxPascalCapturedStorage(const AContext: INyxPascalCompilerContext;
  const AKey: TNyxPascalStorageKey): TNyxPascalStorageFact;
var
  LStorage: INyxPascalContextStorage;
begin
  AKey.Validate;

  if AContext = nil then
  begin
    raise ENyxPascalContext.Create('Structural evidence needs its captured context');
  end;
  Result := NyxPascalStorageFact(psfsUnknown);

  if Supports(AContext, INyxPascalContextStorage, LStorage) then
  begin
    Result := LStorage.StorageFact(AKey);
    Result.Validate;
  end;
end;

function NyxPascalWithCapturedStorage(const AContext: INyxPascalCompilerContext;
  const AKey: TNyxPascalStorageKey;
  const AValue: TNyxPascalStorageFact): INyxPascalCompilerContext;
var
  LContext: INyxPascalCompilerContext;
  LStorage: INyxPascalContextStorage;
begin
  AKey.Validate;
  AValue.Validate;
  LContext := CaptureNyxPascalCompilerContext(AContext);
  Supports(LContext, INyxPascalContextStorage, LStorage);
  Result := LStorage.WithStorageFact(AKey, AValue);
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
    (Ord(FGoto) < Ord(Low(TNyxPascalKnowledge))) or
    (Ord(FGoto) > Ord(High(TNyxPascalKnowledge))) or
    (Ord(FAssembler) < Ord(Low(TNyxPascalAssemblerReader))) or
    (Ord(FAssembler) > Ord(High(TNyxPascalAssemblerReader))) or
    (Ord(FCalling) < Ord(Low(TNyxPascalCallingConvention))) or
    (Ord(FCalling) > Ord(High(TNyxPascalCallingConvention))) or
    (Ord(FWarnings) < Ord(Low(TNyxPascalKnowledge))) or
    (Ord(FWarnings) > Ord(High(TNyxPascalKnowledge))) or
    (Ord(FHints) < Ord(Low(TNyxPascalKnowledge))) or
    (Ord(FHints) > Ord(High(TNyxPascalKnowledge))) or
    (Ord(FNotes) < Ord(Low(TNyxPascalKnowledge))) or
    (Ord(FNotes) > Ord(High(TNyxPascalKnowledge))) or
    (Ord(FPointerChecking) < Ord(Low(TNyxPascalPointerCheckingRequest))) or
    (Ord(FPointerChecking) > Ord(High(TNyxPascalPointerCheckingRequest))) or
    (Ord(FInterfaces) < Ord(Low(TNyxPascalInterfaceStyle))) or
    (Ord(FInterfaces) > Ord(High(TNyxPascalInterfaceStyle))) or
    (Ord(FExcessPrecision) < Ord(Low(TNyxPascalKnowledge))) or
    (Ord(FExcessPrecision) > Ord(High(TNyxPascalKnowledge))) or
    (Ord(FModeExcessPrecision) < Ord(Low(TNyxPascalKnowledge))) or
    (Ord(FModeExcessPrecision) > Ord(High(TNyxPascalKnowledge))) or
    ((FExcessPrecision <> ppkUnknown) and (FModeExcessPrecision <> ppkUnknown)) or
    (Ord(FMinimumRealPrecision) < Ord(Low(TNyxPascalRealPrecisionRequest))) or
    (Ord(FMinimumRealPrecision) > Ord(High(TNyxPascalRealPrecisionRequest))) then
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

function TNyxPascalCompilerPolicies.RequestGoto(AValue: TNyxPascalKnowledge): TNyxPascalCompilerPolicies;
begin
  Result := Self;
  Result.FGoto := AValue;
  Result.Validate;
end;

function TNyxPascalCompilerPolicies.CallBy(AValue: TNyxPascalCallingConvention): TNyxPascalCompilerPolicies;
begin
  Result := Self;
  Result.FCalling := AValue;
  Result.Validate;
end;

function TNyxPascalCompilerPolicies.ReadAssemblyBy(AValue: TNyxPascalAssemblerReader): TNyxPascalCompilerPolicies;
begin
  Result := Self;
  Result.FAssembler := AValue;
  Result.Validate;
end;

function TNyxPascalCompilerPolicies.EmitDiagnostics(AChannel: TNyxPascalDiagnosticChannel;
  AValue: TNyxPascalKnowledge): TNyxPascalCompilerPolicies;
begin

  if (Ord(AChannel) < Ord(Low(TNyxPascalDiagnosticChannel))) or
    (Ord(AChannel) > Ord(High(TNyxPascalDiagnosticChannel))) then
  begin
    raise ENyxPascalContext.Create('A diagnostic category is outside its closed domain');
  end;
  Result := Self;
  case AChannel of
    pdcWarnings:
      Result.FWarnings := AValue;
    pdcHints:
      Result.FHints := AValue;
    pdcNotes:
      Result.FNotes := AValue;
  end;
  Result.Validate;
end;

function TNyxPascalCompilerPolicies.DiagnosticEmission(
  AChannel: TNyxPascalDiagnosticChannel): TNyxPascalKnowledge;
begin
  Validate;

  if (Ord(AChannel) < Ord(Low(TNyxPascalDiagnosticChannel))) or
    (Ord(AChannel) > Ord(High(TNyxPascalDiagnosticChannel))) then
  begin
    raise ENyxPascalContext.Create('A diagnostic category is outside its closed domain');
  end;
  case AChannel of
    pdcWarnings:
      Result := FWarnings;
    pdcHints:
      Result := FHints;
    pdcNotes:
      Result := FNotes;
  end;
end;

function TNyxPascalCompilerPolicies.RequestPointerChecking(
  AValue: TNyxPascalPointerCheckingRequest): TNyxPascalCompilerPolicies;
begin
  Result := Self;
  Result.FPointerChecking := AValue;
  Result.Validate;
end;

function TNyxPascalCompilerPolicies.UseInterfaces(AValue: TNyxPascalInterfaceStyle): TNyxPascalCompilerPolicies;
begin
  Result := Self;
  Result.FInterfaces := AValue;
  Result.Validate;
end;

function TNyxPascalCompilerPolicies.SamePolicies(const AOther: TNyxPascalCompilerPolicies): Boolean;
begin
  Validate;
  AOther.Validate;
  Result := (FInlining = AOther.FInlining) and (FRecords = AOther.FRecords) and
    (FEnums = AOther.FEnums) and (FSets = AOther.FSets) and
    (FCalling = AOther.FCalling) and (FSmartLinking = AOther.FSmartLinking) and
    (FAssembler = AOther.FAssembler) and (FGoto = AOther.FGoto) and
    (FWarnings = AOther.FWarnings) and (FHints = AOther.FHints) and
    (FNotes = AOther.FNotes) and (FPointerChecking = AOther.FPointerChecking) and
    (FInterfaces = AOther.FInterfaces) and (FExcessPrecision = AOther.FExcessPrecision) and
    (FModeExcessPrecision = AOther.FModeExcessPrecision) and
    (FMinimumRealPrecision = AOther.FMinimumRealPrecision);
end;

function TNyxPascalCompilerPolicies.RequestExcessPrecision(
  AValue: TNyxPascalKnowledge): TNyxPascalCompilerPolicies;
begin
  Result := Self;
  Result.FExcessPrecision := AValue;
  Result.FModeExcessPrecision := ppkUnknown;
  Result.Validate;
end;

function TNyxPascalCompilerPolicies.ApplyModeExcessPrecision(
  AValue: TNyxPascalKnowledge): TNyxPascalCompilerPolicies;
begin
  Result := Self;
  Result.FExcessPrecision := ppkUnknown;
  Result.FModeExcessPrecision := AValue;
  Result.Validate;
end;

function TNyxPascalCompilerPolicies.ExcessPrecisionSetting: TNyxPascalKnowledge;
begin
  Validate;
  Result := FExcessPrecision;

  if Result = ppkUnknown then Result := FModeExcessPrecision;
end;

function TNyxPascalCompilerPolicies.RequestMinimumRealPrecision(
  AValue: TNyxPascalRealPrecisionRequest): TNyxPascalCompilerPolicies;
begin
  Result := Self;
  Result.FMinimumRealPrecision := AValue;
  Result.Validate;
end;

function NyxPascalWarningNumber(AValue: Integer): TNyxPascalWarningRef;
begin

  if AValue < 1 then
  begin
    raise ENyxPascalContext.Create('A warning message ID needs a positive signed-32 value');
  end;
  Result := Default(TNyxPascalWarningRef);
  Result.FKind := pwrNumber;
  Result.FNumber := AValue;
  Result.FName := TNyxText(IntToStr(AValue));
end;

function NyxPascalWarningName(const AName: TNyxText): TNyxPascalWarningRef;
begin
  Result := Default(TNyxPascalWarningRef);
  Result.FKind := pwrName;
  Result.FName := NyxPascalASCIIKey(NyxPascalDefine(AName).Name);
end;

procedure TNyxPascalWarningRef.Validate;
begin

  if (Ord(FKind) < Ord(Low(TNyxPascalWarningReferenceKind))) or
    (Ord(FKind) > Ord(High(TNyxPascalWarningReferenceKind))) then
  begin
    raise ENyxPascalContext.Create('A warning selector is outside its closed namespaces');
  end;

  if FKind = pwrNumber then
  begin

    if (FNumber < 1) or (FName <> TNyxText(IntToStr(FNumber))) then
    begin
      raise ENyxPascalContext.Create('A numeric warning selector is not a canonical positive ID');
    end;
  end
  else if (FNumber <> 0) or (FName <> NyxPascalWarningName(FName).Name) then
  begin
    raise ENyxPascalContext.Create('A named warning selector is not canonical');
  end;
end;

function TNyxPascalWarningRef.Number: Integer;
begin
  Validate;

  if FKind <> pwrNumber then
  begin
    raise ENyxPascalContext.Create('A named warning group has no captured numeric message ID');
  end;
  Result := FNumber;
end;

function TNyxPascalWarningRef.SameReference(const AOther: TNyxPascalWarningRef): Boolean;
begin
  Validate;
  AOther.Validate;
  Result := (FKind = AOther.FKind) and (FName = AOther.FName);
end;

{ Validate before publishing or changing any context. Requests preserve their
  conditional activity separately from the authored action and alias namespace. }
function ValidatedWarningRule(const AReference: TNyxPascalWarningRef;
  AAction: TNyxPascalWarningAction; AActivity: TNyxPascalKnowledge): TNyxPascalWarningRule;
begin
  AReference.Validate;

  if (Ord(AAction) < Ord(Low(TNyxPascalWarningAction))) or
    (Ord(AAction) > Ord(High(TNyxPascalWarningAction))) or
    (Ord(AActivity) < Ord(Low(TNyxPascalKnowledge))) or
    (Ord(AActivity) > Ord(High(TNyxPascalKnowledge))) then
  begin
    raise ENyxPascalContext.Create('A warning request needs a known action/activity domain');
  end;
  Result := Default(TNyxPascalWarningRule);
  Result.FReference := AReference;
  Result.FAction := AAction;
  Result.FActivity := AActivity;
end;

class function TNyxPascalOptimizationRequest.Select(
  AMode: TNyxPascalOptimizationMode): TNyxPascalOptimizationRequest;
begin
  Result := Default(TNyxPascalOptimizationRequest);
  Result.FKind := porkMode;
  Result.FMode := AMode;
  Result.Validate;
end;

class function TNyxPascalOptimizationRequest.Toggle(AFeature: TNyxPascalOptimizationFeature;
  AEnabled: Boolean): TNyxPascalOptimizationRequest;
begin
  Result := Default(TNyxPascalOptimizationRequest);
  Result.FKind := porkFeature;
  Result.FFeature := AFeature;
  Result.FEnabled := AEnabled;
  Result.Validate;
end;

procedure TNyxPascalOptimizationRequest.Validate;
begin

  if (FKind = porkMode) and (Ord(FMode) > Ord(pomUnknown)) and
    (Ord(FMode) <= Ord(High(TNyxPascalOptimizationMode))) and
    (FFeature = Low(TNyxPascalOptimizationFeature)) and not FEnabled then
  begin
    Exit;
  end;

  if (FKind = porkFeature) and (FMode = pomUnknown) and
    (Ord(FFeature) >= Ord(Low(TNyxPascalOptimizationFeature))) and
    (Ord(FFeature) <= Ord(High(TNyxPascalOptimizationFeature))) then
  begin
    Exit;
  end;
  raise ENyxPascalContext.Create('An optimizer request needs a valid typed reset or pass toggle');
end;

function TNyxPascalOptimizationRequest.SameRequest(
  const AOther: TNyxPascalOptimizationRequest): Boolean;
begin
  Validate;
  AOther.Validate;
  Result := (FKind = AOther.FKind) and (FMode = AOther.FMode) and
    (FFeature = AOther.FFeature) and (FEnabled = AOther.FEnabled);
end;

function TNyxPascalOptimizationRequest.RequestedMode: TNyxPascalOptimizationMode;
begin
  Validate;

  if FKind <> porkMode then
  begin
    raise ENyxPascalContext.Create('A pass toggle has no optimizer reset mode');
  end;
  Result := FMode;
end;

function TNyxPascalOptimizationRequest.Feature: TNyxPascalOptimizationFeature;
begin
  Validate;

  if FKind <> porkFeature then
  begin
    raise ENyxPascalContext.Create('An optimizer reset has no pass selector');
  end;
  Result := FFeature;
end;

function TNyxPascalOptimizationRequest.Enabled: Boolean;
begin
  Validate;

  if FKind <> porkFeature then
  begin
    raise ENyxPascalContext.Create('An optimizer reset has no individual enable value');
  end;
  Result := FEnabled;
end;

{ Validate before cloning/publishing a context. Records retain only scalar intent
  and activity; no configured compiler or alternate producer survives capture. }
function ValidatedOptimizationRule(const ARequest: TNyxPascalOptimizationRequest;
  AActivity: TNyxPascalKnowledge): TNyxPascalOptimizationRule;
begin
  ARequest.Validate;

  if (Ord(AActivity) < Ord(Low(TNyxPascalKnowledge))) or
    (Ord(AActivity) > Ord(High(TNyxPascalKnowledge))) then
  begin
    raise ENyxPascalContext.Create('An optimizer request has invalid conditional activity');
  end;
  Result.FRequest := ARequest;
  Result.FActivity := AActivity;
end;

function TCompilerContext.OptimizationCount: Integer;
begin
  Result := Length(FOptimizations);
end;

function TCompilerContext.OptimizationAt(AIndex: Integer): TNyxPascalOptimizationRule;
begin

  if (AIndex < 0) or (AIndex >= Length(FOptimizations)) then
  begin
    raise ENyxPascalContext.Create('An optimizer request index is outside its exact context');
  end;
  Result := FOptimizations[AIndex];
end;

function TCompilerContext.WithOptimization(const ARequest: TNyxPascalOptimizationRequest;
  AActivity: TNyxPascalKnowledge): INyxPascalCompilerContext;
var
  LRule: TNyxPascalOptimizationRule;
  LCopy: TCompilerContext;
  LOwner: INyxPascalCompilerContext;
  LCount: Integer;
begin
  LRule := ValidatedOptimizationRule(ARequest, AActivity);
  LCount := Length(FOptimizations);

  if LCount >= NyxPascalOptimizationRuleLimit then
  begin
    raise ENyxPascalContext.Create('Optimizer requests exceed their bounded context vector');
  end;
  LCopy := Clone;
  LOwner := LCopy;
  SetLength(LCopy.FOptimizations, LCount + 1);
  LCopy.FOptimizations[LCount] := LRule;
  Result := LOwner;
end;

function TCompilerContext.WithOptimizationRules(
  const ASource: INyxPascalContextOptimizations): INyxPascalCompilerContext;
var
  LCopy: TCompilerContext;
  LOwner: INyxPascalCompilerContext;
  LCount: Integer;
  LAt: Integer;
  LRule: TNyxPascalOptimizationRule;
begin

  if ASource = nil then
  begin
    raise ENyxPascalContext.Create('Capturing optimizer requests needs an explicit source');
  end;
  LCount := ASource.OptimizationCount;

  if (LCount < 0) or (LCount > NyxPascalOptimizationRuleLimit) then
  begin
    raise ENyxPascalContext.Create('Captured optimizer requests exceed their bounded vector');
  end;
  LCopy := Clone;
  LOwner := LCopy;
  SetLength(LCopy.FOptimizations, LCount);
  for LAt := 0 to LCount - 1 do
  begin
    LRule := ASource.OptimizationAt(LAt);
    LCopy.FOptimizations[LAt] := ValidatedOptimizationRule(LRule.Request, LRule.Activity);
  end;
  Result := LOwner;
end;

function NyxPascalOptimizations(
  const AContext: INyxPascalCompilerContext): INyxPascalContextOptimizations;
var
  LCaptured: INyxPascalCompilerContext;
begin

  if AContext = nil then
  begin
    raise ENyxPascalContext.Create('Optimizer requests need their compiler context');
  end;

  if not Supports(AContext, INyxPascalContextOptimizations, Result) then
  begin
    LCaptured := CaptureNyxPascalCompilerContext(AContext);
    Supports(LCaptured, INyxPascalContextOptimizations, Result);
  end;
end;

function NyxPascalWithOptimization(const AContext: INyxPascalCompilerContext;
  const ARequest: TNyxPascalOptimizationRequest;
  AActivity: TNyxPascalKnowledge): INyxPascalCompilerContext;
var
  LCaptured: INyxPascalCompilerContext;
begin
  LCaptured := CaptureNyxPascalCompilerContext(AContext);
  Result := NyxPascalOptimizations(LCaptured).WithOptimization(ARequest, AActivity);
end;

{ Keep target requests exact, including zero and signed constraint values. Only
  the three code-label selectors have a closed power-of-two admission domain. }
procedure ValidateCodeAlignmentAssignment(ASubject: TNyxPascalCodeAlignmentSubject;
  AValue: Integer);
begin

  if (Ord(ASubject) < Ord(Low(TNyxPascalCodeAlignmentSubject))) or
    (Ord(ASubject) > Ord(High(TNyxPascalCodeAlignmentSubject))) then
  begin
    raise ENyxPascalContext.Create('A code-alignment subject is outside its closed choices');
  end;

  if (ASubject in [pcaProcedure, pcaJump, pcaLoop]) and
    not (AValue in [0, 1, 2, 4, 8, 16, 32, 64, 128]) and (AValue <> 256) then
  begin
    raise ENyxPascalContext.Create('A code-label alignment needs zero or a power of two through 256');
  end;
end;

function TNyxPascalCodeAlignmentRequest.Add(ASubject: TNyxPascalCodeAlignmentSubject;
  AValue: Integer): TNyxPascalCodeAlignmentRequest;
var
  LCount: Integer;
begin
  ValidateCodeAlignmentAssignment(ASubject, AValue);
  LCount := Length(FAssignments);

  if LCount >= NyxPascalCodeAlignmentAssignmentLimit then
  begin
    raise ENyxPascalContext.Create('One code-alignment group exceeds 64 assignments');
  end;
  Result.FAssignments := Copy(FAssignments, 0, LCount);
  SetLength(Result.FAssignments, LCount + 1);
  Result.FAssignments[LCount].FSubject := ASubject;
  Result.FAssignments[LCount].FValue := AValue;
end;

function TNyxPascalCodeAlignmentRequest.Count: Integer;
begin
  Result := Length(FAssignments);
end;

function TNyxPascalCodeAlignmentRequest.Assignment(AIndex: Integer): TNyxPascalCodeAlignmentAssignment;
begin

  if (AIndex < 0) or (AIndex >= Length(FAssignments)) then
  begin
    raise ENyxPascalContext.Create('An alignment assignment index is outside its exact group');
  end;
  Result := FAssignments[AIndex];
end;

procedure TNyxPascalCodeAlignmentRequest.Validate;
var
  LAt: Integer;
begin

  if (Count < 1) or (Count > NyxPascalCodeAlignmentAssignmentLimit) then
  begin
    raise ENyxPascalContext.Create('A code-alignment group needs one through 64 assignments');
  end;
  for LAt := 0 to Count - 1 do
  begin
    ValidateCodeAlignmentAssignment(FAssignments[LAt].Subject, FAssignments[LAt].Value);
  end;
end;

function TNyxPascalCodeAlignmentRequest.SameRequest(
  const AOther: TNyxPascalCodeAlignmentRequest): Boolean;
var
  LAt: Integer;
begin
  Validate;
  AOther.Validate;
  Result := False;

  if Count <> AOther.Count then
  begin
    Exit;
  end;
  for LAt := 0 to Count - 1 do
  begin

    if (FAssignments[LAt].Subject <> AOther.FAssignments[LAt].Subject) or
      (FAssignments[LAt].Value <> AOther.FAssignments[LAt].Value) then
    begin
      Exit;
    end;
  end;
  Result := True;
end;

{ Atomic validation for both ordinary append and alternate-producer capture.
  The private request vector is immutable, so sharing it retains no mutable host. }
function ValidatedCodeAlignmentRule(const ARequest: TNyxPascalCodeAlignmentRequest;
  AActivity: TNyxPascalKnowledge): TNyxPascalCodeAlignmentRule;
begin
  ARequest.Validate;

  if (Ord(AActivity) < Ord(Low(TNyxPascalKnowledge))) or
    (Ord(AActivity) > Ord(High(TNyxPascalKnowledge))) then
  begin
    raise ENyxPascalContext.Create('A code-alignment rule has invalid conditional activity');
  end;
  Result.FRequest := ARequest;
  Result.FActivity := AActivity;
end;

function TCompilerContext.CodeAlignmentCount: Integer;
begin
  Result := Length(FCodeAlignments);
end;

function TCompilerContext.CodeAlignmentAt(AIndex: Integer): TNyxPascalCodeAlignmentRule;
begin

  if (AIndex < 0) or (AIndex >= Length(FCodeAlignments)) then
  begin
    raise ENyxPascalContext.Create('A code-alignment index is outside its exact context');
  end;
  Result := FCodeAlignments[AIndex];
end;

function TCompilerContext.WithCodeAlignment(const ARequest: TNyxPascalCodeAlignmentRequest;
  AActivity: TNyxPascalKnowledge): INyxPascalCompilerContext;
var
  LRule: TNyxPascalCodeAlignmentRule;
  LCopy: TCompilerContext;
  LOwner: INyxPascalCompilerContext;
  LCount: Integer;
begin
  LRule := ValidatedCodeAlignmentRule(ARequest, AActivity);
  LCount := Length(FCodeAlignments);

  if LCount >= NyxPascalCodeAlignmentRuleLimit then
  begin
    raise ENyxPascalContext.Create('Code-alignment requests exceed 2048 groups');
  end;
  LCopy := Clone;
  LOwner := LCopy;
  SetLength(LCopy.FCodeAlignments, LCount + 1);
  LCopy.FCodeAlignments[LCount] := LRule;
  Result := LOwner;
end;

function TCompilerContext.WithCodeAlignmentRules(
  const ASource: INyxPascalContextCodeAlignments): INyxPascalCompilerContext;
var
  LCopy: TCompilerContext;
  LOwner: INyxPascalCompilerContext;
  LCount: Integer;
  LAt: Integer;
  LRule: TNyxPascalCodeAlignmentRule;
begin

  if ASource = nil then
  begin
    raise ENyxPascalContext.Create('Capturing alignment requests needs an explicit source');
  end;
  LCount := ASource.CodeAlignmentCount;

  if (LCount < 0) or (LCount > NyxPascalCodeAlignmentRuleLimit) then
  begin
    raise ENyxPascalContext.Create('Captured alignment requests exceed their bounded vector');
  end;
  LCopy := Clone;
  LOwner := LCopy;
  SetLength(LCopy.FCodeAlignments, LCount);
  for LAt := 0 to LCount - 1 do
  begin
    LRule := ASource.CodeAlignmentAt(LAt);
    LCopy.FCodeAlignments[LAt] := ValidatedCodeAlignmentRule(LRule.Request, LRule.Activity);
  end;
  Result := LOwner;
end;

function NyxPascalCodeAlignments(
  const AContext: INyxPascalCompilerContext): INyxPascalContextCodeAlignments;
var
  LCaptured: INyxPascalCompilerContext;
begin

  if AContext = nil then
  begin
    raise ENyxPascalContext.Create('Code-alignment requests need their compiler context');
  end;

  if not Supports(AContext, INyxPascalContextCodeAlignments, Result) then
  begin
    LCaptured := CaptureNyxPascalCompilerContext(AContext);
    Supports(LCaptured, INyxPascalContextCodeAlignments, Result);
  end;
end;

function NyxPascalWithCodeAlignment(const AContext: INyxPascalCompilerContext;
  const ARequest: TNyxPascalCodeAlignmentRequest;
  AActivity: TNyxPascalKnowledge): INyxPascalCompilerContext;
var
  LCaptured: INyxPascalCompilerContext;
begin
  LCaptured := CaptureNyxPascalCompilerContext(AContext);
  Result := NyxPascalCodeAlignments(LCaptured).WithCodeAlignment(ARequest, AActivity);
end;

function TCompilerContext.WarningCount: Integer;
begin
  Result := Length(FWarnings);
end;

function TCompilerContext.WarningAt(AIndex: Integer): TNyxPascalWarningRule;
begin

  if (AIndex < 0) or (AIndex >= Length(FWarnings)) then
  begin
    raise ENyxPascalContext.Create('A warning request index is outside its exact context');
  end;
  Result := FWarnings[AIndex];
end;

function TCompilerContext.WithWarning(const AReference: TNyxPascalWarningRef;
  AAction: TNyxPascalWarningAction;
  AActivity: TNyxPascalKnowledge): INyxPascalCompilerContext;
var
  LRule: TNyxPascalWarningRule;
  LCopy: TCompilerContext;
  LOwner: INyxPascalCompilerContext;
  LAt: Integer;
begin
  LRule := ValidatedWarningRule(AReference, AAction, AActivity);
  LAt := Length(FWarnings);

  if LAt >= NyxPascalWarningRuleLimit then
  begin
    raise ENyxPascalContext.Create('Ordered warning requests exceed their bounded context vector');
  end;
  LCopy := Clone;
  LOwner := LCopy;
  SetLength(LCopy.FWarnings, LAt + 1);
  LCopy.FWarnings[LAt] := LRule;
  Result := LOwner;
end;

function TCompilerContext.WithWarningRules(
  const ASource: INyxPascalContextWarnings): INyxPascalCompilerContext;
var
  LCopy: TCompilerContext;
  LOwner: INyxPascalCompilerContext;
  LCount: Integer;
  LAt: Integer;
  LRule: TNyxPascalWarningRule;
begin

  if ASource = nil then
  begin
    raise ENyxPascalContext.Create('Capturing warning rules needs an explicit source');
  end;
  LCount := ASource.WarningCount;

  if (LCount < 0) or (LCount > NyxPascalWarningRuleLimit) then
  begin
    raise ENyxPascalContext.Create('Captured warning requests exceed their bounded vector');
  end;
  LCopy := Clone;
  LOwner := LCopy;
  SetLength(LCopy.FWarnings, LCount);
  for LAt := 0 to LCount - 1 do
  begin
    LRule := ASource.WarningAt(LAt);
    LCopy.FWarnings[LAt] := ValidatedWarningRule(LRule.Reference, LRule.Action, LRule.Activity);
  end;
  Result := LOwner;
end;

function NyxPascalWarnings(const AContext: INyxPascalCompilerContext): INyxPascalContextWarnings;
var
  LCaptured: INyxPascalCompilerContext;
begin

  if AContext = nil then
  begin
    raise ENyxPascalContext.Create('Warning policies need their compiler context');
  end;

  if not Supports(AContext, INyxPascalContextWarnings, Result) then
  begin
    LCaptured := CaptureNyxPascalCompilerContext(AContext);
    Supports(LCaptured, INyxPascalContextWarnings, Result);
  end;
end;

function NyxPascalWithWarning(const AContext: INyxPascalCompilerContext;
  const AReference: TNyxPascalWarningRef; AAction: TNyxPascalWarningAction;
  AActivity: TNyxPascalKnowledge): INyxPascalCompilerContext;
var
  LCaptured: INyxPascalCompilerContext;
begin
  LCaptured := CaptureNyxPascalCompilerContext(AContext);
  Result := NyxPascalWarnings(LCaptured).WithWarning(AReference, AAction, AActivity);
end;

function TNyxPascalFPUReference.Known: Boolean;
begin
  Result := FName <> '';
end;

class function TNyxPascalSourceFPUSelection.Unknown: TNyxPascalSourceFPUSelection;
begin
  Result := Default(TNyxPascalSourceFPUSelection);
end;

class function TNyxPascalSourceFPUSelection.Configured: TNyxPascalSourceFPUSelection;
begin
  Result := Unknown;
  Result.FKind := pfsConfigured;
end;

class function TNyxPascalSourceFPUSelection.Select(
  const AReference: TNyxPascalFPUReference): TNyxPascalSourceFPUSelection;
begin
  Result := Unknown;
  Result.FReference := NyxPascalFPU(AReference.Name);
  Result.FKind := pfsSelected;
end;

procedure TNyxPascalSourceFPUSelection.Validate;
begin

  if (Ord(FKind) < Ord(Low(TNyxPascalSourceFPUKind))) or
    (Ord(FKind) > Ord(High(TNyxPascalSourceFPUKind))) then
  begin
    raise ENyxPascalContext.Create('Source FPU history is outside its closed states');
  end;
  case FKind of
    pfsUnknown,pfsConfigured:

      if FReference.Known then raise ENyxPascalContext.Create('Unselected FPU history cannot retain a selector');
    pfsSelected: NyxPascalFPU(FReference.Name);
  end;
end;

function TNyxPascalSourceFPUSelection.SameSelection(
  const AOther: TNyxPascalSourceFPUSelection): Boolean;
begin
  Validate;
  AOther.Validate;
  Result := (FKind = AOther.FKind) and (FReference.Name = AOther.FReference.Name);
end;

function NyxPascalSourceFPU(const AContext: INyxPascalCompilerContext): TNyxPascalSourceFPUSelection;
var LSource: INyxPascalContextSourceFPU;
begin
  Result := TNyxPascalSourceFPUSelection.Unknown;

  if Supports(AContext,INyxPascalContextSourceFPU,LSource) then Result := LSource.SourceFPU;
  Result.Validate;
end;

function NyxPascalSelectedFPU(const AContext: INyxPascalCompilerContext): TNyxPascalFPUReference;
var LSelection: TNyxPascalSourceFPUSelection;
begin
  Result := Default(TNyxPascalFPUReference);
  LSelection := NyxPascalSourceFPU(AContext);
  case LSelection.Kind of
    pfsUnknown: Exit;
    pfsConfigured: Result := NyxPascalConfiguredFPU(AContext);
    pfsSelected: Result := LSelection.Reference;
  end;
end;

function NyxPascalWithSourceFPU(const AContext: INyxPascalCompilerContext;
  const AValue: TNyxPascalSourceFPUSelection): INyxPascalCompilerContext;
var
  LSource: INyxPascalCompilerContext;
  LTarget: INyxPascalContextSourceFPU;
begin

  if AContext = nil then raise ENyxPascalContext.Create('Source FPU history needs its context');
  LSource := AContext;

  if not Supports(LSource,INyxPascalContextSourceFPU,LTarget) then
  begin
    LSource := CaptureNyxPascalCompilerContext(LSource);
    Supports(LSource,INyxPascalContextSourceFPU,LTarget);
  end;
  Result := LTarget.WithSourceFPU(AValue);
end;

function TCompilerContext.SourceFPU: TNyxPascalSourceFPUSelection;
begin
  Result := FSourceFPU;
end;

function TCompilerContext.WithSourceFPU(const AValue: TNyxPascalSourceFPUSelection): INyxPascalCompilerContext;
var LCopy: TCompilerContext;
begin
  AValue.Validate;
  LCopy := Clone;
  Result := LCopy;
  LCopy.FSourceFPU := AValue;
end;

function NyxPascalFPU(const AName: TNyxText): TNyxPascalFPUReference;
var LAt: Integer;
begin

  if (Length(AName) = 0) or (Length(AName) > 64) then
    raise ENyxPascalContext.Create('An FPU selector needs a bounded compiler atom');
  for LAt := 1 to Length(AName) do
  begin

    if not (AName[LAt] in ['a'..'z','A'..'Z','_','0'..'9']) then
      raise ENyxPascalContext.Create('An FPU selector is compiler data, not source text');
  end;
  Result.FName := NyxPascalASCIIKey(AName);
end;

function NyxPascalConfiguredFPU(const AContext: INyxPascalCompilerContext): TNyxPascalFPUReference;
var LSource: INyxPascalContextConfiguredFPU;
begin
  Result := Default(TNyxPascalFPUReference);

  if not Supports(AContext,INyxPascalContextConfiguredFPU,LSource) then Exit;
  Result := LSource.ConfiguredFPU;

  if Result.Known then Result := NyxPascalFPU(Result.Name);
end;

function NyxPascalWithConfiguredFPU(const AContext: INyxPascalCompilerContext;
  const AValue: TNyxPascalFPUReference): INyxPascalCompilerContext;
var
  LSource: INyxPascalCompilerContext;
  LTarget: INyxPascalContextConfiguredFPU;
begin

  if AContext = nil then raise ENyxPascalContext.Create('An FPU observation needs its compiler context');
  LSource := AContext;

  if not Supports(LSource,INyxPascalContextConfiguredFPU,LTarget) then
  begin
    LSource := CaptureNyxPascalCompilerContext(LSource);
    Supports(LSource,INyxPascalContextConfiguredFPU,LTarget);
  end;
  Result := LTarget.WithConfiguredFPU(AValue);
end;

function TCompilerContext.ConfiguredFPU: TNyxPascalFPUReference;
begin
  Result := FConfiguredFPU;
end;

function TCompilerContext.WithConfiguredFPU(const AValue: TNyxPascalFPUReference): INyxPascalCompilerContext;
var
  LValue: TNyxPascalFPUReference;
  LCopy: TCompilerContext;
begin
  LValue := Default(TNyxPascalFPUReference);

  if AValue.Known then LValue := NyxPascalFPU(AValue.Name);
  LCopy := Clone;
  Result := LCopy;
  LCopy.FConfiguredFPU := LValue;
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
  LInterfaces: TNyxPascalInterfaceStyle;
  LMinimum: TNyxPascalRealPrecisionRequest;
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
  LInterfaces := pifsUnknown;
  LMinimum := NyxPascalCompilerPolicies(LSaved).MinimumRealPrecisionRequested;

  if (LCurrent.Family = ppcFPC) and (LCurrent.Version.FullNumber = 30301) then
  begin
    { FPC's local settings stack does not store interfacetype. Restoring local
      switches cannot restore an earlier module-level interface request. }
    LInterfaces := NyxPascalCompilerPolicies(LCurrent).InterfaceStyle;
    { MINFPCONSTPREC is absent from this scanner's saved local settings. }
    LMinimum := NyxPascalCompilerPolicies(LCurrent).MinimumRealPrecisionRequested;
  end;
  Supports(LCurrent, INyxPascalContextPolicies, LPolicies);
  LCurrent := LPolicies.WithPolicies(NyxPascalCompilerPolicies(LSaved)
    .RequestSmartLinking(NyxPascalCompilerPolicies(LCurrent).SmartLinkingRequested)
    .RequestGoto(NyxPascalCompilerPolicies(LCurrent).GotoRequested)
    .UseInterfaces(LInterfaces).RequestMinimumRealPrecision(LMinimum));
  LCurrent := NyxPascalWarnings(LCurrent).WithWarningRules(NyxPascalWarnings(LSaved));
  LCurrent := NyxPascalCodeAlignments(LCurrent).WithCodeAlignmentRules(NyxPascalCodeAlignments(LSaved));
  Result := NyxPascalOptimizations(LCurrent).WithOptimizationRules(NyxPascalOptimizations(LSaved));
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
  LByteSizes: INyxPascalConditionByteSizes;
  LByteSize: TNyxPascalStorageFact;
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
          if Supports(FSemantics, INyxPascalConditionByteSizes, LByteSizes) then
          begin
            LByteSize := LByteSizes.SizeOfBytes(LSize);
            LByteSize.Validate;

            if LByteSize.State = psfsKnown then Result := NumberValue(LByteSize.Bytes);
          end
          else
          begin
            LMeasurement := FSemantics.SizeOfName(LSize);

            if LMeasurement.State = pssKnown then Result := NumberValue(LMeasurement.Bytes);
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
