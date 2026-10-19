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
unit nyx.pascal.translation;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.scheduler, nyx.pascal.lexing, nyx.pascal.context, nyx.pascal.files,
  nyx.pascal.encoding, nyx.pascal.macros;

type
  { Translation resource refusal is atomic. It differs from a recoverable missing
    include/unknown condition, which remains visible in the returned diagnostics. }
  ENyxPascalTranslationLimit = class(ENyxPascalContext);
  { Exact AddToken refusal: Count tokens were admitted, and the next token would
    exceed Limit. Inherits the existing atomic-refusal category for compatibility.
    Counts are validated scalar observations; this exception owns no partial
    translation, compiler context, callback or source lease. }
  ENyxPascalTokenLimit = class(ENyxPascalTranslationLimit)
  private
    FTokenCount: Integer;
    FTokenLimit: Integer;
  public
    { Requires a positive limit already reached by the observed count. }
    constructor Create(ATokenCount, ATokenLimit: Integer); reintroduce;
    property TokenCount: Integer read FTokenCount;
    property TokenLimit: Integer read FTokenLimit;
  end;
  { Explicit captured-context gaps; none is disguised as an inactive branch or a
    successful compiler/type-resolution result. Positions are always physical. }
  TNyxPascalTranslationIssue = (ptiUnknownCondition, ptiMissingInclude,
    ptiIncludeCycle, ptiUnbalancedConditional, ptiUnsupportedDirective,
    ptiMalformedDirective, ptiLexicalIssue, ptiLexicalPolicyChange, ptiSourceEncoding,
    ptiMacroContext, ptiMacroCycle, ptiCompilerSettings);
  { Exact pre-directive source site. PrefixCount excludes this directive;
    occurrence distinguishes repeated includes. Records retain no interfaces. }
  TNyxPascalConditionOrigin = record
    FileReference: TNyxPascalPhysicalFile;
    Occurrence: Integer;
    Span: TNyxPascalSpan;
    PrefixCount: Integer;
  end;
  { One retained unresolved condition after actual ordered macro expansion.
    Plannable is false after malformed syntax, failed expansion or an IFOPT
    request. Origin/State belong to this immutable translation; records contain
    no interface/callback and never grant compiler or source-edit authority. }
  TNyxPascalUnresolvedCondition = record
    Origin: TNyxPascalConditionOrigin;
    State: Integer;
    Expression: TNyxText;
    Plannable: Boolean;
  end;
  { Optional exact planning inputs. Expressions are captured by the owning
    translator, never reconstructed against a later macro environment. Prefix
    views expose only sites already inside their retained token interval. }
  INyxPascalUnresolvedConditions = interface(IInterface)
    ['{98349E7C-C0ED-48DE-A518-427A04570C14}']
    function UnresolvedCount: Integer;
    function UnresolvedAt(AIndex: Integer): TNyxPascalUnresolvedCondition;
  end;
  { One authored MAXFPUREGISTERS request at its exact source occurrence and
    pre-token state. The request belongs to routine code generation; it is not
    an effective global setting or a source type/ABI measurement. Unknown branch
    activity stays explicit. Values own no interface, source or callback. }
  TNyxPascalRegisterDirective = record
    Origin: TNyxPascalConditionOrigin;
    State: Integer;
    Activity: TNyxPascalKnowledge;
    Policy: TNyxPascalFPURegisterPolicy;
  end;
  { Optional immutable authored codegen intents. Includes retain their distinct
    occurrences; prefixes expose only already visited sites. These receipts do
    not synthesize a procedure's effective register allocation or extend its
    lifetime through POP/end-of-procedure. Checked ordinals refuse. }
  INyxPascalRegisterDirectives = interface(IInterface)
    ['{98349E7C-C0ED-48DE-A518-427A04570C15}']
    function RegisterDirectiveCount: Integer;
    function RegisterDirectiveAt(AIndex: Integer): TNyxPascalRegisterDirective;
  end;
  { One authored diagnostic-category toggle at its exact physical occurrence.
    State precedes the directive; Enabled is the authored request while Activity
    preserves uncertain participation. Value-only origins own no source/context/
    callback. This does not predict the compiler's delayed diagnostic emission. }
  TNyxPascalEmissionDirective = record
  private
    FOrigin: TNyxPascalConditionOrigin;
    FState: Integer;
    FActivity: TNyxPascalKnowledge;
    FChannel: TNyxPascalDiagnosticChannel;
    FEnabled: Boolean;
  public
    property Origin: TNyxPascalConditionOrigin read FOrigin;
    property State: Integer read FState;
    property Activity: TNyxPascalKnowledge read FActivity;
    property Channel: TNyxPascalDiagnosticChannel read FChannel;
    property Enabled: Boolean read FEnabled;
  end;
  { Optional checked category-emission receipts. Prefixes freeze only already
    visited values and retain their source separately; restoring settings does
    not erase historical intent. Analyzer diagnostics remain independent and
    cannot be suppressed through a source compiler-verbosity request. }
  INyxPascalEmissionDirectives = interface(IInterface)
    ['{4CC9D819-1AB9-4B23-A2D8-6B9A7D47667B}']
    function EmissionDirectiveCount: Integer;
    function EmissionDirectiveAt(AIndex: Integer): TNyxPascalEmissionDirective;
  end;
  { One authored per-warning request at its exact physical include occurrence.
    State is the pre-directive context and PrefixCount excludes the directive.
    The rule preserves conditional activity without resolving compiler aliases;
    values own no source/context/interface and grant no compilation authority. }
  TNyxPascalWarningDirective = record
  private
    FOrigin: TNyxPascalConditionOrigin;
    FState: Integer;
    FRule: TNyxPascalWarningRule;
  public
    property Origin: TNyxPascalConditionOrigin read FOrigin;
    property State: Integer read FState;
    property Rule: TNyxPascalWarningRule read FRule;
  end;
  { Optional checked warning receipts. Prefixes expose only already visited
    directives, retaining their original owning source and pre-directive state.
    A missing alternate-source capability refuses instead of fabricating rows. }
  INyxPascalWarningDirectives = interface(IInterface)
    ['{7C8B3428-B4AF-4D69-89FD-86AF17B25D00}']
    function WarningDirectiveCount: Integer;
    function WarningDirectiveAt(AIndex: Integer): TNyxPascalWarningDirective;
  end;
  { One complete typed CODEALIGN request at its physical include occurrence.
    State precedes the directive; the rule preserves assignment grouping and
    conditional activity. No context/interface or target-layout authority lives
    in this value. Restoring settings never removes the historical receipt. }
  TNyxPascalCodeAlignmentDirective = record
  private
    FOrigin: TNyxPascalConditionOrigin;
    FState: Integer;
    FRule: TNyxPascalCodeAlignmentRule;
  public
    property Origin: TNyxPascalConditionOrigin read FOrigin;
    property State: Integer read FState;
    property Rule: TNyxPascalCodeAlignmentRule read FRule;
  end;
  { Optional checked source receipts. Prefixes retain only visited sites and
    their original pre-state; a missing alternate-source capability refuses.
    The source owns immutable requests and retains no constructing visitor. }
  INyxPascalCodeAlignmentDirectives = interface(IInterface)
    ['{33241969-5958-4630-80D3-966E134495CE}']
    { Number of visited complete directive groups, including uncertain branches. }
    function CodeAlignmentDirectiveCount: Integer;
    { Checked ordinal access; returned values retain no mutable external owner. }
    function CodeAlignmentDirectiveAt(AIndex: Integer): TNyxPascalCodeAlignmentDirective;
  end;
  { One authored optimizer reset/toggle at its physical include occurrence.
    State precedes the directive. Activity and exact scalar request survive POP
    in this historical receipt; no compiler/source/interface is held by value. }
  TNyxPascalOptimizationDirective = record
  private
    FOrigin: TNyxPascalConditionOrigin;
    FState: Integer;
    FRule: TNyxPascalOptimizationRule;
  public
    property Origin: TNyxPascalConditionOrigin read FOrigin;
    property State: Integer read FState;
    property Rule: TNyxPascalOptimizationRule read FRule;
  end;
  { Optional bounded immutable optimizer receipts. Prefixes retain only visited
    sites with original pre-state; missing alternate-source support refuses. }
  INyxPascalOptimizationDirectives = interface(IInterface)
    ['{7D51CDE4-EAE6-40DE-A74A-DB2C9D2DB427}']
    { Count selected/uncertain source requests in physical traversal order. }
    function OptimizationDirectiveCount: Integer;
    { Checked access; a returned value cannot change the owning translation. }
    function OptimizationDirectiveAt(AIndex: Integer): TNyxPascalOptimizationDirective;
  end;
  { Optional synchronous facts for a still-unknown expression. Ordered semantic
    refinement supplies sealed answers, never mutable editor state. Origin,
    expanded expression and context belong to this exact construction. The
    translator owns this callback only until construction returns or refuses. }
  INyxPascalConditionalFacts = interface(IInterface)
    ['{98349E7C-C0ED-48DE-A518-427A04570C13}']
    function Evaluate(const AOrigin: TNyxPascalConditionOrigin;
      const AContext: INyxPascalCompilerContext; const AExpression: TNyxText): TNyxPascalKnowledge;
  end;
  { Immutable physical diagnostic. Occurrence distinguishes repeated includes;
    text is English guidance with no machine paths or borrowed exceptions. }
  TNyxPascalTranslationDiagnostic = record
  private
    FIssue: TNyxPascalTranslationIssue;
    FOccurrence: Integer;
    FSpan: TNyxPascalSpan;
    FMessage: TNyxText;
  public
    property Issue: TNyxPascalTranslationIssue read FIssue;
    property Occurrence: Integer read FOccurrence;
    property Span: TNyxPascalSpan read FSpan;
    property MessageText: TNyxText read FMessage;
  end;
  { One inclusion, not just one filename. Parent is -1 for the explicit root;
    IncludeSpan belongs to the parent's physical file. Token interval includes
    descendants; entry/exit state indices preserve textual include side effects. }
  TNyxPascalInclusion = record
  private
    FFile: Integer;
    FParent: Integer;
    FIncludeSpan: TNyxPascalSpan;
    FFirst: Integer;
    FCount: Integer;
    FEntry: Integer;
    FExit: Integer;
  public
    property FileIndex: Integer read FFile;
    property Parent: Integer read FParent;
    property IncludeSpan: TNyxPascalSpan read FIncludeSpan;
    property FirstToken: Integer read FFirst;
    property TokenCount: Integer read FCount;
    property EntryState: Integer read FEntry;
    property ExitState: Integer read FExit;
  end;
  { One source token with its inclusion and pre-token compiler state. Activity
    describes conditional participation only, never semantic validity. Physical
    token ordinals belong to their exact occurrence's lexical snapshot, including
    repeated visits with different policies. Records own no interfaces. }
  TNyxPascalTranslatedToken = record
  private
    FOccurrence: Integer;
    FPhysicalToken: Integer;
    FState: Integer;
    FActivity: TNyxPascalKnowledge;
    FExpansion: Integer;
    FExpansionToken: Integer;
    FExpanded: Boolean;
  public
    property Occurrence: Integer read FOccurrence;
    property PhysicalToken: Integer read FPhysicalToken;
    property State: Integer read FState;
    property Activity: TNyxPascalKnowledge read FActivity;
    { -1 for authored tokens. Expanded rows keep the original invocation's
      occurrence/token; effective spelling and recursive origins are separate. }
    property Expansion: Integer read FExpansion;
    property ExpansionToken: Integer read FExpansionToken;
    { False in legacy zero-initialized token records as well as authored rows. }
    property Expanded: Boolean read FExpanded;
  end;
  { Independent analysis budgets. Expanded bytes count repeated physical reads;
    token budgets include trivia/directives and only the root EOF. No limit
    silently truncates a published token/context/occurrence collection. }
  TNyxPascalTranslationOptions = record
  private
    FDefined: Boolean;
    FLexical: TNyxPascalLexicalOptions;
    FTokens: Integer;
    FBytes: Integer;
    FDepth: Integer;
    FOccurrences: Integer;
    FStates: Integer;
    FDiagnostics: Integer;
  public
    { Explicit defaults: 400000 tokens, 16 MiB expanded input, 64 include/condition
      levels, 4096 occurrences/states and 1024 diagnostics. Zeroed options refuse;
      all fluent setters return validated independent copies. }
    class function Defaults: TNyxPascalTranslationOptions; static;
    { Capture initial lexical policy and immutable per-occurrence budgets.
      Known source directives may change comment nesting between tokens. }
    function Lexical(const AValue: TNyxPascalLexicalOptions): TNyxPascalTranslationOptions;
    { Includes trivia/directives and the root EOF, across repeated includes. }
    function UpToTokens(AValue: Integer): TNyxPascalTranslationOptions;
    { Physical reads plus replacement, re-tokenization and boundary work bytes;
      repeated inclusions/expansions are charged independently. }
    function UpToExpandedBytes(AValue: Integer): TNyxPascalTranslationOptions;
    { Bounds recursive inclusions, replacement ancestry, conditional frames and
      the explicitly captured local compiler settings stack. }
    function UpToIncludeDepth(AValue: Integer): TNyxPascalTranslationOptions;
    { The root counts once; repeated files receive independent occurrences.
      Outer replacement packets have a separate collection with this same cap. }
    function UpToOccurrences(AValue: Integer): TNyxPascalTranslationOptions;
    { Limits retained compiler/replacement/settings-stack triples; only adjacent
      equal triples coalesce, including unchanged values behind a changed stack. }
    function UpToStates(AValue: Integer): TNyxPascalTranslationOptions;
    { Excess unresolved constructs refuse instead of silently dropping guidance. }
    function UpToDiagnostics(AValue: Integer): TNyxPascalTranslationOptions;
    { Undefined or nonpositive budgets raise before capturing any input. }
    procedure Validate;
    property LexicalOptions: TNyxPascalLexicalOptions read FLexical;
    property MaximumTokens: Integer read FTokens;
    property MaximumExpandedBytes: Integer read FBytes;
    property MaximumIncludeDepth: Integer read FDepth;
    property MaximumOccurrences: Integer read FOccurrences;
    property MaximumStates: Integer read FStates;
    property MaximumDiagnostics: Integer read FDiagnostics;
  end;
  { Owns frozen files, compiler facts, lexical caches and include-aware tokens.
    All indices/getters are checked and snapshot-local. Resolved means encountered
    translation constructs have known supported context, not grammar, type binding,
    compiler acceptance or execution. Unknown initial facts remain explicit.
    No mutable provider/editor/filesystem/process survives construction. }
  INyxPascalTranslation = interface(IInterface)
    ['{98349E7C-C0ED-48DE-A518-427A04570C03}']
    function Files: INyxPascalFileCatalog;
    function InitialContext: INyxPascalCompilerContext;
    function Resolved: Boolean;
    function Count: Integer;
    function Token(AIndex: Integer): TNyxPascalTranslatedToken;
    function PhysicalToken(AIndex: Integer): TNyxPascalToken;
    function Text(AIndex: Integer): TNyxText;
    { Legacy file-only access refuses differing occurrence tokenizations/policies.
      Prefer NyxPascalOccurrenceLexical when an origin supplies its occurrence. }
    function Physical(AFile: Integer): INyxPascalLexicalSnapshot;
    function OccurrenceCount: Integer;
    function Occurrence(AIndex: Integer): TNyxPascalInclusion;
    function StateCount: Integer;
    function State(AIndex: Integer): INyxPascalCompilerContext;
    function DiagnosticCount: Integer;
    function Diagnostic(AIndex: Integer): TNyxPascalTranslationDiagnostic;
  end;
  { Optional precise lexical views for repeated textual includes. PhysicalToken
    ordinals belong to this occurrence's snapshot, never another visit of its
    file. Returned snapshots are detached and retain no translation/visitor. }
  INyxPascalTranslationLexicalContexts = interface(IInterface)
    ['{98349E7C-C0ED-48DE-A518-427A04570C04}']
    function OccurrenceLexical(AOccurrence: Integer): INyxPascalLexicalSnapshot;
  end;
  { Optional effective-token/recursive-origin owner. The original translation
    methods continue to expose authored invocation tokens. A saved state also
    retains its exact immutable replacement table, independent of compiler facts. }
  INyxPascalTranslationMacros = interface(IInterface)
    ['{98349E7C-C0ED-48DE-A518-427A04570C06}']
    function Expansion(AIndex: Integer): INyxPascalMacroExpansion;
    function StateMacros(AIndex: Integer): INyxPascalMacroEnvironment;
  end;
  { Physical PUSH origin. The stack owns no document or renderer; an entry is
    identified by its inclusion and scalar range, including cross-include scopes. }
  TNyxPascalSettingsOrigin = record
    Occurrence: Integer;
    Span: TNyxPascalSpan;
  end;
  { Optional exact local-settings stack beside each historical state. Returned
    contexts are immutable captured values. Stack shape is part of state identity
    even when the current option values happen to match another state. }
  INyxPascalTranslationSettings = interface(IInterface)
    ['{98349E7C-C0ED-48DE-A518-427A04570C08}']
    { Unknown textual effects can change stack shape. Other getters refuse that
      state rather than presenting the previously captured frames as current. }
    function SettingsKnown(AState: Integer): Boolean;
    function SettingsDepth(AState: Integer): Integer;
    function SavedSettings(AState, ALevel: Integer): INyxPascalCompilerContext;
    function SettingsOrigin(AState, ALevel: Integer): TNyxPascalSettingsOrigin;
  end;
  { Optional construction-only source authority. Prepare selects an exact prefix;
    attached execution policy admits real work at its subsequent boundaries.
    EvaluatePrefix receives a detached pre-directive
    translation, never the live builder. Active physical readers expose frozen
    tokens only, so their ordinary completed-lexical getters refuse. Full source,
    origins, contexts and saved settings remain exact. No callback is retained by
    either the prefix or the final ordinary translation. }
  INyxPascalPrefixConditionalFacts = interface(IInterface)
    ['{E62BB77C-36B8-4A68-A664-BCB392FAF8D9}']
    function PreparePrefix(const AOrigin: TNyxPascalConditionOrigin;
      const AContext: INyxPascalCompilerContext; const AExpression: TNyxText): Boolean;
    function EvaluatePrefix(const AOrigin: TNyxPascalConditionOrigin;
      const AContext: INyxPascalCompilerContext; const AExpression: TNyxText;
      const APrefix: INyxPascalTranslation): TNyxPascalKnowledge;
  end;
  { Optional synchronous aggregate admission beside the unchanged full-view cap.
    Called once before storing each newly translated token, including trivia and
    EOF. It grants no access to mutable source and is released after construction. }
  INyxPascalTranslationWork = interface(IInterface)
    ['{239856F3-7EA2-4C53-81AB-0EBA32B4505E}']
    procedure AdmitToken(ATokensBefore, AViewLimit: Integer);
  end;
  { Detached token-validation receipt for one prefix. PreparedTokens includes
    newly examined rows in its owned freeze, or the full arbitrary-source scan.
    RetainedTokens describes earlier rows proven by the same private builder;
    ReusedTokens describes the wrapper's exact summary use. Counts confer no
    lineage/proof, compiler meaning or reduced work authority on an alternate
    implementation. Directive/diagnostic/offset copies remain separate work. }
  INyxPascalPrefixValidationWork = interface(IInterface)
    ['{8E33BB40-8028-4E18-8382-6C53290D1CAD}']
    function PreparedTokens: Integer;
    function RetainedTokens: Integer;
    function ReusedTokens: Integer;
  end;

{ Effective tokens may come from a replacement fragment; their Span is relative
  to that fragment. PhysicalToken/Text still describe the exact invocation.
  Callers retain the expansion trace for definition origins, never infer them
  from the length of the substituted spelling. Legacy translators pass through. }
function NyxPascalEffectiveToken(const ASource: INyxPascalTranslation;
  AIndex: Integer): TNyxPascalToken;
function NyxPascalEffectiveText(const ASource: INyxPascalTranslation;
  AIndex: Integer): TNyxText;
{ Only an owned translation can bind an effective ordinal to prepared lexical
  proof. The detached value owns no translation, occurrence, macro or context. }
function NyxPascalPreparedEffectiveToken(const ASource: INyxPascalTranslation;
  AIndex: Integer): TNyxPascalPreparedToken;

{ Exact common leading token count for snapshots from one owned chronological
  translation, -1 for unrelated/alternate sources. Immutable construction stamps
  establish lineage without hashes, full-prefix rescanning or live-builder access.
  This grants source-token continuity only; options, activity, projection and
  provisional syntax still require their independent checks. }
function NyxPascalSharedTranslationTokens(const ALeft,
  ARight: INyxPascalTranslation): Integer;

{ Decompose an existing complete directive token without interpreting it.
  The value offset uses Unicode scalars in that token's original storage;
  ASCII whitespace trimming preserves UTF-8 include/condition spelling. }
procedure NyxPascalDirectiveParts(const AText: TNyxText; out AName, AValue: TNyxText;
  out AValueOffset: Integer);

{ Read exact occurrence tokens, with legacy file-only fallback for alternate
  translators which have not exposed contextual lexing. Indices remain checked. }
function NyxPascalOccurrenceLexical(const ATranslation: INyxPascalTranslation;
  AOccurrence: Integer): INyxPascalLexicalSnapshot;
{ Map a decoded physical scalar range to its exact original source bytes.
  Occurrence identity is retained; no byte offset is inferred from UTF-8 text. }
function NyxPascalOriginalSpan(const ATranslation: INyxPascalTranslation;
  AOccurrence: Integer; const ASpan: TNyxPascalSpan): TNyxPascalByteSpan;

{ Freeze caller-serialized alternate inputs once and traverse only captured files.
  Root must name an explicit captured unit/program file; no include owner is guessed.
  Conditional define/option changes flow out of textual includes. Unknown effects
  revoke state knowledge. Inactive includes perform no lookup or state mutation.
  Cancellation is borrowed only during construction; every reader must join before
  its owner edits input files. No compiler application is run by translation. }
function NewNyxPascalTranslation(const AFiles: INyxPascalFileCatalog;
  const ARoot: TNyxPascalPhysicalFile; const AContext: INyxPascalCompilerContext;
  const AOptions: TNyxPascalTranslationOptions;
  const ACancellation: INyxCancellationScope = nil;
  const AConditions: INyxPascalConditionalFacts = nil): INyxPascalTranslation;

{ Rebind only an owned, published, resolved translation to a frozen current input
  catalog. Ordered names/search policy must match; every visited root/include
  keeps exact original bytes, origin, encoding, text and scalar/byte coordinates.
  Unvisited text may change and Files then exposes that current text. Nil means
  incompatible/unowned source, not a partially rebound result. This proves input
  equivalence only: semantic-world independence belongs to the condition owner. }
function RebindNyxPascalTranslation(const ASource: INyxPascalTranslation;
  const AFiles: INyxPascalFileCatalog;
  const ACancellation: INyxCancellationScope = nil): INyxPascalTranslation;

{ Immutable analysis prefix before an existing translation token. It invents no
  closing syntax or authored EOF token. State/occurrence exits stop at the site;
  full physical views remain exact for navigation. Known prefix participation
  and prior diagnostics determine Resolved independently of the unseen suffix.
  Input budgets/cancellation refuse atomically; no source/provider is mutated. }
function NewNyxPascalTranslationPrefix(const ASource: INyxPascalTranslation;
  ABeforeToken: Integer; const AOptions: TNyxPascalTranslationOptions;
  const ACancellation: INyxCancellationScope = nil): INyxPascalTranslation;

implementation

uses SysUtils, nyx.bytes, nyx.editing, nyx.pascal.work;

const
  { Immutable blocks bound each partial-tail copy and keep indexed reads flat. }
  CTranslationPrefixBlockSize = 256;
  { Compiler states carry several immutable leases. Small pages bound the
    private tail copied by each freeze without changing any analysis quota. }
  CTranslationStateBlockSize = 32;

type
  { Sealed bridge through exact occurrence/expansion storage. Public token
    fields, equal text and lineage counters do not confer this capability. }
  IPreparedTranslation = interface(IInterface)
    ['{36D80CB5-6EF1-4CCB-BE1B-56C409DA8D21}']
    function PreparedToken(AIndex: Integer): TNyxPascalPreparedToken;
  end;
  { File-only compatibility view. The first immutable occurrence is retained;
    differing later policies/tokens revoke ambiguous legacy access. }
  TPhysicalCache = class
    Lexical: INyxPascalLexicalSnapshot;
    Ambiguous: Boolean;
  end;
  { Forward source owner. Token visitors and summary constructors borrow it
    synchronously; published summaries never retain a constructing owner. }
  TTranslation = class;
  { Private in-unit proof of one immutable append history. Identity is an empty
    value lease with no source/reader back link; alternate translators do not
    acquire continuity merely by returning equal text or storage counters. }
  ITranslationLineage = interface(IInterface)
    ['{787C7FD7-1308-4E79-BF9A-9E81F5DC9CF5}']
    function LineageIdentity: IInterface;
  end;
  { Immutable token-domain facts only. Offset rows and an opaque lineage stamp
    retain no source, context, reader, callback or predecessor summary. A summary
    stops before one exact boundary token; diagnostics still filter separately. }
  IPrefixTokenSummary = interface(IInterface)
    function Count: Integer;
    function LineageIdentity: IInterface;
    function Occurrences: Integer;
    function MaximumOccurrence: Integer;
    function MaximumState: Integer;
    function MaximumExpansion: Integer;
    function Resolved: Boolean;
    function LastOffset(AOccurrence: Integer): Integer;
    function PreparedTokens: Integer;
    function RetainedTokens: Integer;
  end;
  { In-unit authority exposed only by detached owned freezes. Public storage/
    work receipts or equal source text cannot substitute for this exact proof. }
  IFrozenPrefixProof = interface(IInterface)
    ['{323C8BA7-1CB2-49FB-A20C-AFD4D398C02B}']
    function PrefixSummary(ABeforeToken: Integer): IPrefixTokenSummary;
  end;
  { Validates appended tokens against a fully detached frozen source, then owns
    copied occurrence offsets and scalar facts. Construction borrows that source
    only; successful values never retain it or the previous summary. }
  TPrefixTokenSummary = class(TInterfacedObject, IPrefixTokenSummary)
  private
    FCount: Integer;
    FLineage: IInterface;
    FOffsets: array of Integer;
    FMaximumOccurrence: Integer;
    FMaximumState: Integer;
    FMaximumExpansion: Integer;
    FResolved: Boolean;
    FPrepared: Integer;
    FRetained: Integer;
  public
    constructor Create(ASource: TTranslation; const APrevious: IPrefixTokenSummary;
      const ACancellation: INyxCancellationScope);
    function Count: Integer;
    function LineageIdentity: IInterface;
    function Occurrences: Integer;
    function MaximumOccurrence: Integer;
    function MaximumState: Integer;
    function MaximumExpansion: Integer;
    function Resolved: Boolean;
    function LastOffset(AOccurrence: Integer): Integer;
    function PreparedTokens: Integer;
    function RetainedTokens: Integer;
  end;
  { Owns one immutable completed source, bounded prefix ordinals, copied emission
    intents and diagnostic indices. Outward references retain original files and
    contexts, never a constructing visitor, callback, editor or mutable buffer. }
  TTranslationPrefix = class(TInterfacedObject, INyxPascalTranslation,
    INyxPascalTranslationLexicalContexts, INyxPascalTranslationMacros,
    INyxPascalTranslationSettings, INyxPascalUnresolvedConditions,
    INyxPascalRegisterDirectives, INyxPascalWarningDirectives,
    INyxPascalCodeAlignmentDirectives, INyxPascalOptimizationDirectives,
    INyxPascalEmissionDirectives, ITranslationLineage, INyxPascalPrefixValidationWork,
    IPreparedTranslation)
  private
    FSource: INyxPascalTranslation;
    FMacros: INyxPascalTranslationMacros;
    FSettings: INyxPascalTranslationSettings;
    FCount: Integer;
    FStates: Integer;
    FOccurrences: Integer;
    FExpansion: Integer;
    FResolved: Boolean;
    FPreparedTokens: Integer;
    FRetainedTokens: Integer;
    FReusedTokens: Integer;
    FDiagnostics: array of Integer;
    FUnresolved: INyxPascalUnresolvedConditions;
    FUnresolvedRows: array of Integer;
    FRegisterDirectives: INyxPascalRegisterDirectives;
    FRegisterRows: array of Integer;
    FWarningDirectives: INyxPascalWarningDirectives;
    FWarningRows: array of Integer;
    FCodeAlignmentDirectives: INyxPascalCodeAlignmentDirectives;
    FCodeAlignmentRows: array of Integer;
    FOptimizationDirectives: INyxPascalOptimizationDirectives;
    FOptimizationRows: array of Integer;
    FEmissionRows: array of TNyxPascalEmissionDirective;
  public
    constructor Create(const ASource: INyxPascalTranslation; ABeforeToken: Integer;
      const AOptions: TNyxPascalTranslationOptions; const ACancellation: INyxCancellationScope);
    function Files: INyxPascalFileCatalog;
    function InitialContext: INyxPascalCompilerContext;
    function Resolved: Boolean;
    function Count: Integer;
    function Token(AIndex: Integer): TNyxPascalTranslatedToken;
    function PhysicalToken(AIndex: Integer): TNyxPascalToken;
    function Text(AIndex: Integer): TNyxText;
    function Physical(AFile: Integer): INyxPascalLexicalSnapshot;
    function OccurrenceLexical(AOccurrence: Integer): INyxPascalLexicalSnapshot;
    function OccurrenceCount: Integer;
    function Occurrence(AIndex: Integer): TNyxPascalInclusion;
    function StateCount: Integer;
    function State(AIndex: Integer): INyxPascalCompilerContext;
    function DiagnosticCount: Integer;
    function Diagnostic(AIndex: Integer): TNyxPascalTranslationDiagnostic;
    function Expansion(AIndex: Integer): INyxPascalMacroExpansion;
    function StateMacros(AIndex: Integer): INyxPascalMacroEnvironment;
    function SettingsKnown(AState: Integer): Boolean;
    function SettingsDepth(AState: Integer): Integer;
    function SavedSettings(AState, ALevel: Integer): INyxPascalCompilerContext;
    function SettingsOrigin(AState, ALevel: Integer): TNyxPascalSettingsOrigin;
    function UnresolvedCount: Integer;
    function UnresolvedAt(AIndex: Integer): TNyxPascalUnresolvedCondition;
    function RegisterDirectiveCount: Integer;
    function RegisterDirectiveAt(AIndex: Integer): TNyxPascalRegisterDirective;
    function WarningDirectiveCount: Integer;
    function WarningDirectiveAt(AIndex: Integer): TNyxPascalWarningDirective;
    function CodeAlignmentDirectiveCount: Integer;
    function CodeAlignmentDirectiveAt(AIndex: Integer): TNyxPascalCodeAlignmentDirective;
    function OptimizationDirectiveCount: Integer;
    function OptimizationDirectiveAt(AIndex: Integer): TNyxPascalOptimizationDirective;
    function EmissionDirectiveCount: Integer;
    function EmissionDirectiveAt(AIndex: Integer): TNyxPascalEmissionDirective;
    function LineageIdentity: IInterface;
    function PreparedTokens: Integer;
    function RetainedTokens: Integer;
    function ReusedTokens: Integer;
    function PreparedToken(AIndex: Integer): TNyxPascalPreparedToken;
  end;
  { Advertises storage only when the retained source actually supplies its
    immutable arena receipt. Exclusive source bounds remain with the base view;
    the receipt describes the underlying arena, including its boundary token. }
  TStoredTranslationPrefix = class(TTranslationPrefix, INyxPascalPrefixTokenStorage)
  private
    function Storage: INyxPascalPrefixTokenStorage;
  public
    function TokenRows: Integer;
    function CopiedTokenRows: Integer;
    function SharedTokenRows: Integer;
    function DirectoryEntries: Integer;
  end;
  { Read-only translated block lease. The live owner fills an unpublished tail
    and seals completed pages before sharing; token values own no interfaces. }
  ITranslatedTokenBlock = interface(IInterface)
    function Token(AIndex: Integer): TNyxPascalTranslatedToken;
  end;
  { A snapshot owns its directory slots while completed blocks are shared.
    Mutable array slots are never aliased between builder and frozen snapshots. }
  TTranslatedTokenBlocks = array of ITranslatedTokenBlock;
  { Direct source-token destination. Completed pages never change; a frozen
    partial page owns copied rows and is never extended by the live translator. }
  TTranslatedTokenBlock = class(TInterfacedObject, ITranslatedTokenBlock)
  public
    Rows: array of TNyxPascalTranslatedToken;
    function Token(AIndex: Integer): TNyxPascalTranslatedToken;
  end;
  { One lexical construction's visitor. Owner is borrowed, never reference counted
    back to its translation; recursive includes get independent visitors. }
  TTranslationReader = class(TInterfacedObject, INyxPascalLexicalObserver,
    INyxPascalLexicalRegionObserver, INyxPascalLexicalConstructionObserver)
  public
    Owner: TTranslation;
    Occurrence: Integer;
    Included: Boolean;
    procedure BeginRead(const ACapture: INyxPascalLexicalPrefixCapture);
    procedure AfterToken(AIndex: Integer; const AToken: TNyxPascalToken;
      const AText: TNyxText; var AAllowNestedComments: Boolean);
    function InitialRegion: TNyxPascalLexicalRegion;
    function NextRegion: TNyxPascalLexicalRegion;
  end;
  { One conditional frame may cross include boundaries. Taken is three-valued;
    an unknown branch's writes revoke facts rather than merging a guessed path.
    EntryRegion isolates the lexical envelope of discarded alternate branches. }
  TConditionalFrame = record
    Parent: TNyxPascalKnowledge;
    Taken: TNyxPascalKnowledge;
    HasElse: Boolean;
    Occurrence: Integer;
    Span: TNyxPascalSpan;
    EntryRegion: TNyxPascalLexicalRegion;
  end;
  { Immutable private frame shares only an owned compiler snapshot and value
    origin. Historical/live vectors may retain it without a translation back link. }
  INyxSavedCompilerSettings = interface(IInterface)
    function Context: INyxPascalCompilerContext;
    function Origin: TNyxPascalSettingsOrigin;
  end;
  { Independent dynamic vectors retain immutable frames; no record contains an
    interface and no published vector shares mutable slots with the live stack. }
  TCompilerSettingsFrames = array of INyxSavedCompilerSettings;
  { Constructed once before entering the live stack, then never mutated. }
  TSavedCompilerSettings = class(TInterfacedObject, INyxSavedCompilerSettings)
  public
    Saved: INyxPascalCompilerContext;
    Site: TNyxPascalSettingsOrigin;
    function Context: INyxPascalCompilerContext;
    function Origin: TNyxPascalSettingsOrigin;
  end;
  { One append-only compiler state owns context/macros and a copied saved-stack
    vector. Its leases point outward to immutable values only; published state
    rows retain no producer, reader, callback, budget or predecessor. }
  ITranslatedCompilerState = interface(IInterface)
    function Context: INyxPascalCompilerContext;
    function Macros: INyxPascalMacroEnvironment;
    function SettingsKnown: Boolean;
    function SettingsDepth: Integer;
    function SavedFrame(AIndex: Integer): INyxSavedCompilerSettings;
    function SameStack(const AOther: TCompilerSettingsFrames; AKnown: Boolean): Boolean;
  end;
  { Dynamic interface slots belong to a class owner. Live append slots are never
    aliased by frozen snapshots, and no interface enters a record/static array. }
  TTranslatedCompilerStates = array of ITranslatedCompilerState;
  { Captures a state once, before its row is published. All fields stay private;
    consumers can inspect immutable leases but cannot replace saved frame slots. }
  TTranslatedCompilerState = class(TInterfacedObject, ITranslatedCompilerState)
  private
    FContext: INyxPascalCompilerContext;
    FMacros: INyxPascalMacroEnvironment;
    FFrames: TCompilerSettingsFrames;
    FKnown: Boolean;
  public
    constructor Create(const AContext: INyxPascalCompilerContext;
      const AMacros: INyxPascalMacroEnvironment; const AFrames: TCompilerSettingsFrames;
      AKnown: Boolean);
    function Context: INyxPascalCompilerContext;
    function Macros: INyxPascalMacroEnvironment;
    function SettingsKnown: Boolean;
    function SettingsDepth: Integer;
    function SavedFrame(AIndex: Integer): INyxSavedCompilerSettings;
    function SameStack(const AOther: TCompilerSettingsFrames; AKnown: Boolean): Boolean;
  end;
  { A page owns only immutable state leases. Completed pages can be shared by
    unrelated frozen directories without retaining the live append vector. }
  ITranslatedStateBlock = interface(IInterface)
    function State(AIndex: Integer): ITranslatedCompilerState;
  end;
  { Each owner has independent directory slots; a partial last page belongs only
    to that freeze and is never extended after publication. }
  TTranslatedStateBlocks = array of ITranslatedStateBlock;
  { Construction admits every copied lease before copying it. Cancellation is
    borrowed only for construction and cannot outlive this immutable page. }
  TTranslatedStateBlock = class(TInterfacedObject, ITranslatedStateBlock)
  private
    FRows: TTranslatedCompilerStates;
  public
    constructor Create(const ARows: TTranslatedCompilerStates; AFirst, ACount: Integer;
      const ACancellation: INyxCancellationScope);
    function State(AIndex: Integer): ITranslatedCompilerState;
  end;
  { Owns every cache/immutable vector. Construction alone retains cancellation;
    all published references point outward to data, never back to a document. }
  TTranslation = class(TInterfacedObject, INyxPascalTranslation,
    INyxPascalTranslationLexicalContexts, INyxPascalTranslationMacros,
    INyxPascalTranslationSettings, INyxPascalUnresolvedConditions,
    INyxPascalRegisterDirectives, INyxPascalWarningDirectives,
    INyxPascalCodeAlignmentDirectives, INyxPascalOptimizationDirectives,
    INyxPascalEmissionDirectives, ITranslationLineage, IPreparedTranslation)
  private
    FLineage: IInterface;
    FFiles: INyxPascalFileCatalog;
    FInitial: INyxPascalCompilerContext;
    FContext: INyxPascalCompilerContext;
    FEmissionDirectives: array of TNyxPascalEmissionDirective;
    FEmissionCount: Integer;
    FOptions: TNyxPascalTranslationOptions;
    FPhysical: array of TPhysicalCache;
    FLexical: array of INyxPascalLexicalSnapshot;
    { Live revocable captures are construction-only. Detached prefix windows
      instead own copied rows and never retain these borrowed capabilities. }
    FCaptures: array of INyxPascalLexicalPrefixCapture;
    FWindows: array of INyxPascalLexicalPrefix;
    { Actual storage in both live and frozen owners. Directories never alias
      mutable slots; only sealed full pages may be shared during construction. }
    FTokenBlocks: TTranslatedTokenBlocks;
    { Non-owning construction writer. Full-page sealing and constructor
      retirement revoke it; the directory owns every page lease independently. }
    FWriting: TTranslatedTokenBlock;
    FIsFrozen: Boolean;
    { Set only after all construction callbacks/writers retire successfully. }
    FPublished: Boolean;
    FSharedRows: Integer;
    { Live construction keeps only its latest pure summary and retires it on
      every exit. A detached freeze owns its immutable summary independently. }
    FTokenSummary: IPrefixTokenSummary;
    FOccurrences: array of TNyxPascalInclusion;
    { Only the live translation appends rows. Frozen views retain immutable
      pages; their completed-page cache retires with construction on every exit. }
    FStateRows: TTranslatedCompilerStates;
    FStateBlocks: TTranslatedStateBlocks;
    FCompilerFrames: TCompilerSettingsFrames;
    FSettingsKnown: Boolean;
    FMacros: INyxPascalMacroEnvironment;
    FExpansions: array of INyxPascalMacroExpansion;
    FBoundaryViews: array of INyxPascalSourceView;
    FDiagnostics: array of TNyxPascalTranslationDiagnostic;
    FUnresolved: array of TNyxPascalUnresolvedCondition;
    FUnresolvedCount: Integer;
    FRegisterDirectives: array of TNyxPascalRegisterDirective;
    FRegisterCount: Integer;
    FWarningDirectives: array of TNyxPascalWarningDirective;
    FWarningCount: Integer;
    FCodeAlignmentDirectives: array of TNyxPascalCodeAlignmentDirective;
    FCodeAlignmentCount: Integer;
    FOptimizationDirectives: array of TNyxPascalOptimizationDirective;
    FOptimizationCount: Integer;
    FFrames: array of TConditionalFrame;
    FTokenCount: Integer;
    FOccurrenceCount: Integer;
    FStateCount: Integer;
    FDiagnosticCount: Integer;
    FBytes: Integer;
    FDepth: Integer;
    FActive: TNyxPascalKnowledge;
    FResolved: Boolean;
    FBlocked: Boolean;
    FNested: Boolean;
    { The selected assembly envelope and current physical branch differ while
      scanning discarded conditionals; includes share both without a back link. }
    FRegion: TNyxPascalLexicalRegion;
    FScanRegion: TNyxPascalLexicalRegion;
    FModeSeen: Boolean;
    FCancellation: INyxCancellationScope;
    FConditions: INyxPascalConditionalFacts;
    FPrefixConditions: INyxPascalPrefixConditionalFacts;
    FWork: INyxPascalTranslationWork;
    procedure CheckCancellation;
    procedure Problem(AIssue: TNyxPascalTranslationIssue; AOccurrence: Integer;
      const ASpan: TNyxPascalSpan; const AMessage: TNyxText);
    procedure SaveState;
    function CompilerState(AIndex: Integer): ITranslatedCompilerState;
    procedure AddToken(AOccurrence, AToken: Integer; AExpansion: Integer = -1;
      AExpansionToken: Integer = -1);
    procedure Word(AOccurrence, AIndex: Integer; const AToken: TNyxPascalToken;
      const AText: TNyxText);
    procedure VerifyBoundary(AOccurrence: Integer; const ASpan: TNyxPascalSpan;
      const AExpansion: INyxPascalMacroExpansion);
    procedure Visit(AFile, AParent: Integer; const AInclude: TNyxPascalSpan);
    procedure Directive(AOccurrence: Integer; const AToken: TNyxPascalToken;
      const AText: TNyxText);
    function Condition(AOccurrence: Integer; const ASpan: TNyxPascalSpan;
      const AKind, AValue: TNyxText): TNyxPascalKnowledge;
    procedure ChangeSymbol(const AName: TNyxText; AKind: TNyxPascalDefineKind);
    procedure ChangeOption(AName: Char; AValue: TNyxPascalKnowledge);
    procedure ChangeOptions(const AText: TNyxText);
    procedure UnknownEffect;
  public
    constructor Create(const AFiles: INyxPascalFileCatalog;
      const ARoot: TNyxPascalPhysicalFile; const AContext: INyxPascalCompilerContext;
      const AOptions: TNyxPascalTranslationOptions;
      const ACancellation: INyxCancellationScope;
      const AConditions: INyxPascalConditionalFacts);
    { Freeze current admitted data, including the directive token used only as
      the subsequent prefix's exclusive boundary. No live callback is copied. }
    constructor CreatePrefix(ASource: TTranslation);
    { Shares sealed managed vectors, with private physical-cache holders and a
      fresh lineage. It retains no live reader, callback, budget or input world. }
    constructor CreateRebound(ASource: TTranslation; const AFiles: INyxPascalFileCatalog;
      const ACancellation: INyxCancellationScope);
    destructor Destroy; override;
    function Files: INyxPascalFileCatalog;
    function InitialContext: INyxPascalCompilerContext;
    function Resolved: Boolean;
    function Count: Integer;
    function Token(AIndex: Integer): TNyxPascalTranslatedToken;
    function PhysicalToken(AIndex: Integer): TNyxPascalToken;
    function Text(AIndex: Integer): TNyxText;
    function Physical(AFile: Integer): INyxPascalLexicalSnapshot;
    function OccurrenceLexical(AOccurrence: Integer): INyxPascalLexicalSnapshot;
    function PreparedToken(AIndex: Integer): TNyxPascalPreparedToken;
    function OccurrenceCount: Integer;
    function Occurrence(AIndex: Integer): TNyxPascalInclusion;
    function StateCount: Integer;
    function State(AIndex: Integer): INyxPascalCompilerContext;
    function DiagnosticCount: Integer;
    function Diagnostic(AIndex: Integer): TNyxPascalTranslationDiagnostic;
    function Expansion(AIndex: Integer): INyxPascalMacroExpansion;
    function StateMacros(AIndex: Integer): INyxPascalMacroEnvironment;
    function SettingsDepth(AState: Integer): Integer;
    function SettingsKnown(AState: Integer): Boolean;
    function SavedSettings(AState, ALevel: Integer): INyxPascalCompilerContext;
    function SettingsOrigin(AState, ALevel: Integer): TNyxPascalSettingsOrigin;
    function UnresolvedCount: Integer;
    function UnresolvedAt(AIndex: Integer): TNyxPascalUnresolvedCondition;
    function EmissionDirectiveCount: Integer;
    function EmissionDirectiveAt(AIndex: Integer): TNyxPascalEmissionDirective;
    function RegisterDirectiveCount: Integer;
    function RegisterDirectiveAt(AIndex: Integer): TNyxPascalRegisterDirective;
    function WarningDirectiveCount: Integer;
    function WarningDirectiveAt(AIndex: Integer): TNyxPascalWarningDirective;
    function CodeAlignmentDirectiveCount: Integer;
    function CodeAlignmentDirectiveAt(AIndex: Integer): TNyxPascalCodeAlignmentDirective;
    function OptimizationDirectiveCount: Integer;
    function OptimizationDirectiveAt(AIndex: Integer): TNyxPascalOptimizationDirective;
    function LineageIdentity: IInterface;
  end;
  { Only this detached construction exposes a token-storage receipt. All token
    rows, directories and physical windows are immutable and retain no reader. }
  TFrozenTranslation = class(TTranslation, INyxPascalPrefixTokenStorage,
    IFrozenPrefixProof)
  public
    function PrefixSummary(ABeforeToken: Integer): IPrefixTokenSummary;
    function TokenRows: Integer;
    function CopiedTokenRows: Integer;
    function SharedTokenRows: Integer;
    function DirectoryEntries: Integer;
  end;

constructor TPrefixTokenSummary.Create(ASource: TTranslation;
  const APrevious: IPrefixTokenSummary; const ACancellation: INyxCancellationScope);
var
  LAt: Integer;
  LToken: TNyxPascalTranslatedToken;
  LBoundary: TNyxPascalTranslatedToken;
  LSpan: TNyxPascalSpan;

  procedure Check;
  begin

    if (ACancellation <> nil) and ACancellation.Cancelled then
    begin
      raise ENyxPascalAnalysisCancelled.Create('Frozen token summary cancelled');
    end;
    NyxPascalAdmitAnalysis(ACancellation, papFrozenRow);
  end;

begin
  inherited Create;
  Check;

  if (ASource = nil) or not ASource.FIsFrozen or (ASource.FTokenCount < 1) then
  begin
    raise ENyxPascalTranslationLimit.Create('Token summaries need a detached owned boundary');
  end;
  FCount := ASource.FTokenCount - 1;
  FLineage := ASource.FLineage;
  FMaximumOccurrence := -1;
  FMaximumState := -1;
  FMaximumExpansion := -1;
  FResolved := True;
  LBoundary := ASource.Token(FCount);

  if (LBoundary.State < 0) or (LBoundary.State >= ASource.FStateCount) or
    (LBoundary.Occurrence < 0) or (LBoundary.Occurrence >= ASource.FOccurrenceCount) then
  begin
    raise ENyxPascalTranslationLimit.Create('A frozen summary boundary lost its exact owner');
  end;

  if APrevious <> nil then
  begin

    if (APrevious.LineageIdentity <> FLineage) or (APrevious.Count > FCount) or
      (APrevious.Occurrences > ASource.FOccurrenceCount) or
      (APrevious.MaximumState > LBoundary.State) then
    begin
      raise ENyxPascalTranslationLimit.Create('A token summary cannot cross source history or state order');
    end;
    FRetained := APrevious.Count;
    FMaximumOccurrence := APrevious.MaximumOccurrence;
    FMaximumState := APrevious.MaximumState;
    FMaximumExpansion := APrevious.MaximumExpansion;
    FResolved := APrevious.Resolved;
  end;
  SetLength(FOffsets, ASource.FOccurrenceCount);
  for LAt := 0 to High(FOffsets) do
  begin
    Check;
    FOffsets[LAt] := -1;

    if (APrevious <> nil) and (LAt < APrevious.Occurrences) then
    begin
      FOffsets[LAt] := APrevious.LastOffset(LAt);
    end;
  end;
  for LAt := FRetained to FCount - 1 do
  begin
    Check;
    LToken := ASource.Token(LAt);

    if (LToken.Occurrence < 0) or (LToken.Occurrence >= Length(FOffsets)) or
      (LToken.State < 0) or (LToken.State > LBoundary.State) then
    begin
      raise ENyxPascalTranslationLimit.Create('Frozen summary tokens have invalid state or occurrence ownership');
    end;
    LSpan := ASource.PhysicalToken(LAt).Span;

    if LSpan.Start > FOffsets[LToken.Occurrence] then
    begin
      FOffsets[LToken.Occurrence] := LSpan.Start;
    end;

    if LToken.Occurrence > FMaximumOccurrence then
    begin
      FMaximumOccurrence := LToken.Occurrence;
    end;

    if LToken.State > FMaximumState then
    begin
      FMaximumState := LToken.State;
    end;

    if LToken.Activity = ppkUnknown then
    begin
      FResolved := False;
    end;

    if LToken.Expanded and (LToken.Expansion > FMaximumExpansion) then
    begin
      FMaximumExpansion := LToken.Expansion;
    end;
    Inc(FPrepared);
  end;
end;

function TPrefixTokenSummary.Count: Integer;
begin
  Result := FCount;
end;

function TPrefixTokenSummary.LineageIdentity: IInterface;
begin
  Result := FLineage;
end;

function TPrefixTokenSummary.Occurrences: Integer;
begin
  Result := Length(FOffsets);
end;

function TPrefixTokenSummary.MaximumOccurrence: Integer;
begin
  Result := FMaximumOccurrence;
end;

function TPrefixTokenSummary.MaximumState: Integer;
begin
  Result := FMaximumState;
end;

function TPrefixTokenSummary.MaximumExpansion: Integer;
begin
  Result := FMaximumExpansion;
end;

function TPrefixTokenSummary.Resolved: Boolean;
begin
  Result := FResolved;
end;

function TPrefixTokenSummary.LastOffset(AOccurrence: Integer): Integer;
begin

  if (AOccurrence < 0) or (AOccurrence >= Length(FOffsets)) then
  begin
    raise ENyxPascalTranslationLimit.Create('An occurrence is outside its frozen token summary');
  end;
  Result := FOffsets[AOccurrence];
end;

function TPrefixTokenSummary.PreparedTokens: Integer;
begin
  Result := FPrepared;
end;

function TPrefixTokenSummary.RetainedTokens: Integer;
begin
  Result := FRetained;
end;

function TFrozenTranslation.PrefixSummary(ABeforeToken: Integer): IPrefixTokenSummary;
begin
  Result := nil;

  if (FTokenSummary <> nil) and (ABeforeToken = FTokenCount - 1) and
    (FTokenSummary.Count = ABeforeToken) and
    (FTokenSummary.LineageIdentity = FLineage) then
  begin
    Result := FTokenSummary;
  end;
end;

function TTranslatedTokenBlock.Token(AIndex: Integer): TNyxPascalTranslatedToken;
begin

  if (AIndex < 0) or (AIndex >= Length(Rows)) then
  begin
    raise ENyxPascalContext.Create('Translated token is outside its immutable block');
  end;
  Result := Rows[AIndex];
end;

function TTranslation.LineageIdentity: IInterface;
begin
  Result := FLineage;
end;

function TTranslationPrefix.LineageIdentity: IInterface;
var
  LSource: ITranslationLineage;
begin
  Result := nil;

  if Supports(FSource, ITranslationLineage, LSource) then
  begin
    Result := LSource.LineageIdentity;
  end;
end;

function NyxPascalSharedTranslationTokens(const ALeft,
  ARight: INyxPascalTranslation): Integer;
var
  LLeft: ITranslationLineage;
  LRight: ITranslationLineage;
  LIdentity: IInterface;
begin
  Result := -1;

  if not Supports(ALeft, ITranslationLineage, LLeft) or
    not Supports(ARight, ITranslationLineage, LRight) then
  begin
    Exit;
  end;
  LIdentity := LLeft.LineageIdentity;

  if (LIdentity = nil) or (LIdentity <> LRight.LineageIdentity) then
  begin
    Exit;
  end;
  Result := ALeft.Count;

  if ARight.Count < Result then
  begin
    Result := ARight.Count;
  end;
end;

function TFrozenTranslation.TokenRows: Integer;
begin
  Result := FTokenCount;
end;

function TFrozenTranslation.CopiedTokenRows: Integer;
begin
  Result := FTokenCount - FSharedRows;
end;

function TFrozenTranslation.SharedTokenRows: Integer;
begin
  Result := FSharedRows;
end;

function TFrozenTranslation.DirectoryEntries: Integer;
begin
  Result := Length(FTokenBlocks);
end;

function TStoredTranslationPrefix.Storage: INyxPascalPrefixTokenStorage;
begin

  if not Supports(FSource, INyxPascalPrefixTokenStorage, Result) then
  begin
    raise ENyxPascalContext.Create('A stored prefix lost its immutable arena receipt');
  end;
end;

function TStoredTranslationPrefix.TokenRows: Integer;
begin
  Result := Storage.TokenRows;
end;

function TStoredTranslationPrefix.CopiedTokenRows: Integer;
begin
  Result := Storage.CopiedTokenRows;
end;

function TStoredTranslationPrefix.SharedTokenRows: Integer;
begin
  Result := Storage.SharedTokenRows;
end;

function TStoredTranslationPrefix.DirectoryEntries: Integer;
begin
  Result := Storage.DirectoryEntries;
end;

function Capacity(ACount, AMaximum: Integer): Integer;
begin
  Result := AMaximum;

  if 32 + ACount div 2 <= AMaximum - ACount then
  begin
    Result := ACount + 32 + ACount div 2;
  end;
end;

constructor ENyxPascalTokenLimit.Create(ATokenCount, ATokenLimit: Integer);
begin

  if (ATokenLimit < 1) or (ATokenCount < ATokenLimit) then
  begin
    raise ENyxPascalContext.Create('A token-budget refusal needs its reached positive limit');
  end;
  inherited CreateFmt('Pascal translated tokens exceed their budget [used=%d limit=%d]',
    [ATokenCount, ATokenLimit]);
  FTokenCount := ATokenCount;
  FTokenLimit := ATokenLimit;
end;

class function TNyxPascalTranslationOptions.Defaults: TNyxPascalTranslationOptions;
begin
  Result := Default(TNyxPascalTranslationOptions);
  Result.FDefined := True;
  Result.FLexical := TNyxPascalLexicalOptions.Defaults;
  Result.FTokens := 400000;
  Result.FBytes := 16 * 1024 * 1024;
  Result.FDepth := 64;
  Result.FOccurrences := 4096;
  Result.FStates := 4096;
  Result.FDiagnostics := 1024;
end;

procedure TNyxPascalTranslationOptions.Validate;
begin
  FLexical.Validate;

  if not FDefined or (FTokens < 1) or (FBytes < 1) or (FDepth < 1) or
    (FOccurrences < 1) or (FStates < 1) or (FDiagnostics < 1) then
  begin
    raise ENyxPascalContext.Create('Use explicit positive Pascal translation budgets');
  end;
end;

function TNyxPascalTranslationOptions.Lexical(
  const AValue: TNyxPascalLexicalOptions): TNyxPascalTranslationOptions;
begin
  Result := Self;
  Result.FLexical := AValue;
  Result.Validate;
end;

function TNyxPascalTranslationOptions.UpToTokens(AValue: Integer): TNyxPascalTranslationOptions;
begin
  Result := Self;
  Result.FTokens := AValue;
  Result.Validate;
end;

function TNyxPascalTranslationOptions.UpToExpandedBytes(AValue: Integer): TNyxPascalTranslationOptions;
begin
  Result := Self;
  Result.FBytes := AValue;
  Result.Validate;
end;

function TNyxPascalTranslationOptions.UpToIncludeDepth(AValue: Integer): TNyxPascalTranslationOptions;
begin
  Result := Self;
  Result.FDepth := AValue;
  Result.Validate;
end;

function TNyxPascalTranslationOptions.UpToOccurrences(AValue: Integer): TNyxPascalTranslationOptions;
begin
  Result := Self;
  Result.FOccurrences := AValue;
  Result.Validate;
end;

function TNyxPascalTranslationOptions.UpToStates(AValue: Integer): TNyxPascalTranslationOptions;
begin
  Result := Self;
  Result.FStates := AValue;
  Result.Validate;
end;

function TNyxPascalTranslationOptions.UpToDiagnostics(AValue: Integer): TNyxPascalTranslationOptions;
begin
  Result := Self;
  Result.FDiagnostics := AValue;
  Result.Validate;
end;


constructor TTranslation.Create(const AFiles: INyxPascalFileCatalog;
  const ARoot: TNyxPascalPhysicalFile; const AContext: INyxPascalCompilerContext;
  const AOptions: TNyxPascalTranslationOptions;
  const ACancellation: INyxCancellationScope;
  const AConditions: INyxPascalConditionalFacts);
var
  LRoot: Integer;
  LAt: Integer;
  LDefinition: TNyxPascalMacroDefinition;
begin
  inherited Create;
  FLineage := TInterfacedObject.Create;
  AOptions.Validate;
  FOptions := AOptions;
  FCancellation := ACancellation;
  FConditions := AConditions;
  Supports(AConditions, INyxPascalPrefixConditionalFacts, FPrefixConditions);
  Supports(AConditions, INyxPascalTranslationWork, FWork);
  CheckCancellation;
  try
    FFiles := CaptureNyxPascalFiles(AFiles);
    FInitial := CaptureNyxPascalCompilerContext(AContext);
    FContext := FInitial;
    FMacros := NewNyxPascalMacroEnvironment;
    for LAt := 0 to FInitial.SymbolCount - 1 do
    begin

      if FInitial.Symbol(FInitial.SymbolName(LAt)).Kind = ppdInteger then
      begin
        LDefinition := Default(TNyxPascalMacroDefinition);
        LDefinition.Name := FInitial.SymbolName(LAt);
        LDefinition.Replacement := TNyxText(IntToStr(FInitial.Symbol(LDefinition.Name).IntegerValue));
        LDefinition.Occurrence := -1;
        LDefinition.Span := NyxPascalSpan(0, NyxTextScalarCount(LDefinition.Replacement));
        LDefinition.Known := True;
        FMacros := FMacros.WithDefinition(LDefinition);
      end;
    end;
    FNested := FOptions.LexicalOptions.AllowNestedComments;

    if (FInitial.Feature(ppfNestedComments) <> ppkUnknown) and
      ((FInitial.Feature(ppfNestedComments) = ppkTrue) <>
        FOptions.LexicalOptions.AllowNestedComments) then
    begin
      raise ENyxPascalContext.Create('Captured comment facts disagree with the lexical policy');
    end;
    SetLength(FPhysical, FFiles.Count);
    SetLength(FBoundaryViews, FFiles.Count);
    LRoot := FFiles.Find(ARoot);

    if LRoot < 0 then
    begin
      raise ENyxPascalContext.Create('The explicit translation root was not captured');
    end;
    FResolved := True;
    FSettingsKnown := True;
    FActive := ppkTrue;
    SaveState;
    Visit(LRoot, -1, NyxPascalSpan(0, 0));

    if not FBlocked then
    begin
      for LAt := 0 to High(FFrames) do
      begin
        Problem(ptiUnbalancedConditional, FFrames[LAt].Occurrence, FFrames[LAt].Span,
          'Finish this captured conditional block');
      end;

      if Length(FCompilerFrames) > 0 then
      begin
        Problem(ptiCompilerSettings, FCompilerFrames[0].Origin.Occurrence,
          FCompilerFrames[0].Origin.Span,
          'This settings stack continues beyond the unit; its next-unit continuation needs explicit capture');
      end;
    end;
  finally
    FCancellation := nil;
    FConditions := nil;
    FPrefixConditions := nil;
    FWork := nil;
    FWriting := nil;
    FStateBlocks := nil;
    FTokenSummary := nil;
  end;
  FPublished := True;
end;

constructor TTranslation.CreateRebound(ASource: TTranslation;
  const AFiles: INyxPascalFileCatalog; const ACancellation: INyxCancellationScope);
var
  LAt: Integer;

  procedure Lease;
  begin

    if (ACancellation <> nil) and ACancellation.Cancelled then
      raise ENyxPascalAnalysisCancelled.Create('Translation catalog rebind cancelled');
    NyxPascalAdmitAnalysis(ACancellation, papSourceProof);
  end;
begin
  inherited Create;
  Lease;
  FLineage := TInterfacedObject.Create;
  FFiles := AFiles;
  FInitial := ASource.FInitial;
  FContext := ASource.FContext;
  FOptions := ASource.FOptions;
  FResolved := ASource.FResolved;
  FTokenCount := ASource.FTokenCount;
  FOccurrenceCount := ASource.FOccurrenceCount;
  FStateCount := ASource.FStateCount;
  FDiagnosticCount := ASource.FDiagnosticCount;
  FBytes := ASource.FBytes;
  { Published vectors never mutate. Each assignment leases their immutable
    storage; raw physical-cache holders below are independently owned. }
  Lease; FTokenBlocks := ASource.FTokenBlocks;
  Lease; FStateRows := ASource.FStateRows;
  Lease; FLexical := ASource.FLexical;
  Lease; FWindows := ASource.FWindows;
  Lease; FOccurrences := ASource.FOccurrences;
  Lease; FExpansions := ASource.FExpansions;
  Lease; FBoundaryViews := ASource.FBoundaryViews;
  Lease; FDiagnostics := ASource.FDiagnostics;
  Lease; FUnresolved := ASource.FUnresolved;
  Lease; FEmissionDirectives := ASource.FEmissionDirectives;
  Lease; FRegisterDirectives := ASource.FRegisterDirectives;
  Lease; FWarningDirectives := ASource.FWarningDirectives;
  Lease; FCodeAlignmentDirectives := ASource.FCodeAlignmentDirectives;
  Lease; FOptimizationDirectives := ASource.FOptimizationDirectives;
  FUnresolvedCount := ASource.FUnresolvedCount;
  FEmissionCount := ASource.FEmissionCount;
  FRegisterCount := ASource.FRegisterCount;
  FWarningCount := ASource.FWarningCount;
  FCodeAlignmentCount := ASource.FCodeAlignmentCount;
  FOptimizationCount := ASource.FOptimizationCount;
  SetLength(FPhysical, AFiles.Count);
  for LAt := 0 to High(FPhysical) do
  begin
    Lease;

    if ASource.FPhysical[LAt] <> nil then
    begin
      FPhysical[LAt] := TPhysicalCache.Create;
      FPhysical[LAt].Lexical := ASource.FPhysical[LAt].Lexical;
      FPhysical[LAt].Ambiguous := ASource.FPhysical[LAt].Ambiguous;
    end;
  end;
  Lease;
  FPublished := True;
end;

constructor TTranslation.CreatePrefix(ASource: TTranslation);
var
  LAt: Integer;
  LRow: Integer;
  LFirst: Integer;
  LCount: Integer;
  LBlock: TTranslatedTokenBlock;
  LLease: ITranslatedTokenBlock;
  LDiagnostic: Integer;
begin
  inherited Create;

  if (ASource = nil) or ASource.FIsFrozen then
  begin
    raise ENyxPascalContext.Create('Only the live translation can freeze its admitted token blocks');
  end;
  FIsFrozen := True;
  FLineage := ASource.FLineage;
  FOptions := ASource.FOptions;
  FFiles := ASource.FFiles;
  FInitial := ASource.FInitial;
  FContext := ASource.FContext;
  FResolved := ASource.FResolved;
  FTokenCount := ASource.FTokenCount;
  FOccurrenceCount := ASource.FOccurrenceCount;
  FStateCount := ASource.FStateCount;
  FDiagnosticCount := ASource.FDiagnosticCount;
  FSharedRows := FTokenCount div CTranslationPrefixBlockSize * CTranslationPrefixBlockSize;

  if FSharedRows > FTokenCount then
  begin
    raise ENyxPascalContext.Create('Frozen translation blocks exceed their producing reader');
  end;
  { Source admission already created each completed page in its final storage.
    Only directory leases and unfinished tail rows need copying at a freeze. }
  LCount := FTokenCount div CTranslationPrefixBlockSize;
  SetLength(FTokenBlocks, LCount + Ord(FTokenCount mod CTranslationPrefixBlockSize <> 0));
  for LAt := 0 to LCount - 1 do
  begin
    ASource.CheckCancellation;
    NyxPascalAdmitAnalysis(ASource.FCancellation, papFrozenRow);
    FTokenBlocks[LAt] := ASource.FTokenBlocks[LAt];
  end;

  if FTokenCount mod CTranslationPrefixBlockSize <> 0 then
  begin
    LFirst := LCount * CTranslationPrefixBlockSize;
    LBlock := TTranslatedTokenBlock.Create;
    LLease := LBlock;
    SetLength(LBlock.Rows, FTokenCount - LFirst);
    for LRow := 0 to High(LBlock.Rows) do
    begin
      ASource.CheckCancellation;
      NyxPascalAdmitAnalysis(ASource.FCancellation, papFrozenRow);
      LBlock.Rows[LRow] := ASource.Token(LFirst + LRow);
    end;
    FTokenBlocks[LCount] := LLease;
  end;
  { Append history never changes earlier state rows. Cache completed pages once,
    then copy only directory leases and the private unfinished tail. Even these
    remaining copies are admitted; sharing does not discount performed work. }
  while Length(ASource.FStateBlocks) < FStateCount div CTranslationStateBlockSize do
  begin
    LAt := Length(ASource.FStateBlocks);
    LFirst := LAt * CTranslationStateBlockSize;
    NyxPascalAdmitAnalysis(ASource.FCancellation, papFrozenRow);
    SetLength(ASource.FStateBlocks, LAt + 1);
    ASource.FStateBlocks[LAt] := TTranslatedStateBlock.Create(ASource.FStateRows,
      LFirst, CTranslationStateBlockSize, ASource.FCancellation);
  end;
  LCount := Length(ASource.FStateBlocks);
  SetLength(FStateBlocks, LCount + Ord(FStateCount mod CTranslationStateBlockSize <> 0));
  for LAt := 0 to LCount - 1 do
  begin
    ASource.CheckCancellation;
    NyxPascalAdmitAnalysis(ASource.FCancellation, papFrozenRow);
    FStateBlocks[LAt] := ASource.FStateBlocks[LAt];
  end;

  if FStateCount mod CTranslationStateBlockSize <> 0 then
  begin
    LFirst := LCount * CTranslationStateBlockSize;
    NyxPascalAdmitAnalysis(ASource.FCancellation, papFrozenRow);
    FStateBlocks[LCount] := TTranslatedStateBlock.Create(ASource.FStateRows,
      LFirst, FStateCount - LFirst, ASource.FCancellation);
  end;
  NyxPascalAdmitAnalysis(ASource.FCancellation, papFrozenRow, Length(ASource.FExpansions));
  FExpansions := Copy(ASource.FExpansions, 0, Length(ASource.FExpansions));
  NyxPascalAdmitAnalysis(ASource.FCancellation, papDiagnosticRow, FDiagnosticCount);
  FDiagnostics := Copy(ASource.FDiagnostics, 0, FDiagnosticCount);
  FUnresolvedCount := ASource.FUnresolvedCount;
  NyxPascalAdmitAnalysis(ASource.FCancellation, papFrozenRow, FUnresolvedCount);
  FUnresolved := Copy(ASource.FUnresolved, 0, FUnresolvedCount);
  FEmissionCount := ASource.FEmissionCount;
  NyxPascalAdmitAnalysis(ASource.FCancellation, papFrozenRow, FEmissionCount);
  FEmissionDirectives := Copy(ASource.FEmissionDirectives, 0, FEmissionCount);
  FRegisterCount := ASource.FRegisterCount;
  NyxPascalAdmitAnalysis(ASource.FCancellation, papFrozenRow, FRegisterCount);
  FRegisterDirectives := Copy(ASource.FRegisterDirectives, 0, FRegisterCount);
  FWarningCount := ASource.FWarningCount;
  NyxPascalAdmitAnalysis(ASource.FCancellation, papFrozenRow, FWarningCount);
  FWarningDirectives := Copy(ASource.FWarningDirectives, 0, FWarningCount);
  FCodeAlignmentCount := ASource.FCodeAlignmentCount;
  NyxPascalAdmitAnalysis(ASource.FCancellation, papFrozenRow, FCodeAlignmentCount);
  FCodeAlignmentDirectives := Copy(ASource.FCodeAlignmentDirectives, 0, FCodeAlignmentCount);
  FOptimizationCount := ASource.FOptimizationCount;
  NyxPascalAdmitAnalysis(ASource.FCancellation, papFrozenRow, FOptimizationCount);
  FOptimizationDirectives := Copy(ASource.FOptimizationDirectives, 0, FOptimizationCount);
  SetLength(FPhysical, FFiles.Count);
  for LAt := 0 to High(FPhysical) do
  begin
    ASource.CheckCancellation;
    NyxPascalAdmitAnalysis(ASource.FCancellation, papFrozenRow);

    if ASource.FPhysical[LAt] <> nil then
    begin
      FPhysical[LAt] := TPhysicalCache.Create;
      FPhysical[LAt].Lexical := ASource.FPhysical[LAt].Lexical;
      FPhysical[LAt].Ambiguous := ASource.FPhysical[LAt].Ambiguous;
    end;
  end;
  NyxPascalAdmitAnalysis(ASource.FCancellation, papFrozenRow, FOccurrenceCount);
  FLexical := Copy(ASource.FLexical, 0, FOccurrenceCount);
  SetLength(FWindows, FOccurrenceCount);
  SetLength(FOccurrences, FOccurrenceCount);
  for LAt := 0 to FOccurrenceCount - 1 do
  begin
    ASource.CheckCancellation;
    NyxPascalAdmitAnalysis(ASource.FCancellation, papFrozenRow);
    FOccurrences[LAt] := ASource.FOccurrences[LAt];

    if ASource.FCaptures[LAt] <> nil then
    begin
      FWindows[LAt] := ASource.FCaptures[LAt].Freeze;
      FOccurrences[LAt].FCount := FTokenCount - FOccurrences[LAt].FFirst;
      FOccurrences[LAt].FExit := FStateCount - 1;
      for LDiagnostic := 0 to FWindows[LAt].DiagnosticCount - 1 do
      begin
        ASource.CheckCancellation;
        Problem(ptiLexicalIssue, LAt, FWindows[LAt].Diagnostic(LDiagnostic).Span,
          FWindows[LAt].Diagnostic(LDiagnostic).MessageText);
      end;
    end;
  end;
  { Physical windows now exist, including unfinished active readers. Validate
    only the suffix after the prior exact boundary; neither summary retains
    this snapshot or its producer. Publish the new construction cache last. }
  FTokenSummary := TPrefixTokenSummary.Create(Self, ASource.FTokenSummary,
    ASource.FCancellation);
  ASource.CheckCancellation;
  ASource.FTokenSummary := FTokenSummary;
end;

destructor TTranslation.Destroy;
var
  LAt: Integer;
begin
  for LAt := 0 to High(FPhysical) do
  begin
    FPhysical[LAt].Free;
  end;
  inherited Destroy;
end;

procedure TTranslation.CheckCancellation;
begin

  if (FCancellation <> nil) and FCancellation.Cancelled then
  begin
    raise ENyxPascalAnalysisCancelled.Create('Pascal translation cancelled');
  end;
end;

procedure TTranslation.Problem(AIssue: TNyxPascalTranslationIssue; AOccurrence: Integer;
  const ASpan: TNyxPascalSpan; const AMessage: TNyxText);
begin
  NyxPascalAdmitAnalysis(FCancellation, papDiagnosticRow);
  FResolved := False;

  if FDiagnosticCount >= FOptions.MaximumDiagnostics then
  begin
    raise ENyxPascalTranslationLimit.Create('Pascal translation diagnostics exceed their budget');
  end;

  if FDiagnosticCount = Length(FDiagnostics) then
  begin
    SetLength(FDiagnostics, Capacity(FDiagnosticCount, FOptions.MaximumDiagnostics));
  end;
  FDiagnostics[FDiagnosticCount].FIssue := AIssue;
  FDiagnostics[FDiagnosticCount].FOccurrence := AOccurrence;
  FDiagnostics[FDiagnosticCount].FSpan := ASpan;
  FDiagnostics[FDiagnosticCount].FMessage := AMessage;
  Inc(FDiagnosticCount);
end;

procedure TTranslation.SaveState;
begin

  if (FStateCount > 0) and FStateRows[FStateCount - 1].Context.SameFacts(FContext) and
    FStateRows[FStateCount - 1].Macros.SameMacros(FMacros) and
    FStateRows[FStateCount - 1].SameStack(FCompilerFrames, FSettingsKnown) then
  begin
    Exit;
  end;

  if FStateCount >= FOptions.MaximumStates then
  begin
    raise ENyxPascalTranslationLimit.Create('Pascal translation states exceed their budget');
  end;

  { Capture the context, macro, row slot and saved frame leases once. Pages and
    frozen directories separately admit every reference that they actually copy. }
  NyxPascalAdmitAnalysis(FCancellation, papFrozenRow, 3 + Length(FCompilerFrames));

  if FStateCount = Length(FStateRows) then
  begin
    SetLength(FStateRows, Capacity(FStateCount, FOptions.MaximumStates));
  end;
  FStateRows[FStateCount] := TTranslatedCompilerState.Create(FContext, FMacros,
    FCompilerFrames, FSettingsKnown);
  Inc(FStateCount);
end;

function TTranslation.CompilerState(AIndex: Integer): ITranslatedCompilerState;
begin

  if (AIndex < 0) or (AIndex >= FStateCount) then
  begin
    raise ENyxPascalContext.Create('Compiler state is outside its translation');
  end;

  if FIsFrozen then
  begin
    Exit(FStateBlocks[AIndex div CTranslationStateBlockSize].State(
      AIndex mod CTranslationStateBlockSize));
  end;
  Result := FStateRows[AIndex];
end;

constructor TTranslatedCompilerState.Create(const AContext: INyxPascalCompilerContext;
  const AMacros: INyxPascalMacroEnvironment; const AFrames: TCompilerSettingsFrames;
  AKnown: Boolean);
begin
  inherited Create;
  FContext := AContext;
  FMacros := AMacros;
  FFrames := Copy(AFrames);
  FKnown := AKnown;
end;

function TTranslatedCompilerState.Context: INyxPascalCompilerContext;
begin
  Result := FContext;
end;

function TTranslatedCompilerState.Macros: INyxPascalMacroEnvironment;
begin
  Result := FMacros;
end;

function TTranslatedCompilerState.SettingsKnown: Boolean;
begin
  Result := FKnown;
end;

function TTranslatedCompilerState.SettingsDepth: Integer;
begin
  Result := Length(FFrames);
end;

function TTranslatedCompilerState.SavedFrame(AIndex: Integer): INyxSavedCompilerSettings;
begin
  Result := FFrames[AIndex];
end;

constructor TTranslatedStateBlock.Create(const ARows: TTranslatedCompilerStates;
  AFirst, ACount: Integer; const ACancellation: INyxCancellationScope);
var
  LAt: Integer;
begin
  inherited Create;
  SetLength(FRows, ACount);
  for LAt := 0 to ACount - 1 do
  begin

    if (ACancellation <> nil) and ACancellation.Cancelled then
    begin
      raise ENyxPascalAnalysisCancelled.Create('Pascal state-page construction cancelled');
    end;
    NyxPascalAdmitAnalysis(ACancellation, papFrozenRow);
    FRows[LAt] := ARows[AFirst + LAt];
  end;
end;

function TTranslatedStateBlock.State(AIndex: Integer): ITranslatedCompilerState;
begin
  Result := FRows[AIndex];
end;

function TSavedCompilerSettings.Context: INyxPascalCompilerContext;
begin
  Result := Saved;
end;

function TSavedCompilerSettings.Origin: TNyxPascalSettingsOrigin;
begin
  Result := Site;
end;

function TTranslatedCompilerState.SameStack(const AOther: TCompilerSettingsFrames; AKnown: Boolean): Boolean;
var
  LAt: Integer;
begin
  Result := False;

  if (FKnown <> AKnown) or (Length(FFrames) <> Length(AOther)) then
  begin
    Exit;
  end;
  for LAt := 0 to High(FFrames) do
  begin

    if FFrames[LAt] <> AOther[LAt] then
    begin
      Exit;
    end;
  end;
  Result := True;
end;

procedure TTranslation.AddToken(AOccurrence, AToken: Integer; AExpansion: Integer;
  AExpansionToken: Integer);
var
  LAt: Integer;
  LToken: TNyxPascalTranslatedToken;
  LBlock: TTranslatedTokenBlock;
  LLease: ITranslatedTokenBlock;
begin
  CheckCancellation;

  if FTokenCount >= FOptions.MaximumTokens then
  begin
    raise ENyxPascalTokenLimit.Create(FTokenCount, FOptions.MaximumTokens);
  end;

  NyxPascalAdmitAnalysis(FCancellation, papSourceToken);

  if FWork <> nil then
  begin
    FWork.AdmitToken(FTokenCount, FOptions.MaximumTokens);
  end;

  if FTokenCount mod CTranslationPrefixBlockSize = 0 then
  begin
    LBlock := TTranslatedTokenBlock.Create;
    LLease := LBlock;
    LAt := FOptions.MaximumTokens - FTokenCount;

    if LAt > CTranslationPrefixBlockSize then
    begin
      LAt := CTranslationPrefixBlockSize;
    end;
    SetLength(LBlock.Rows, LAt);
    LAt := FTokenCount div CTranslationPrefixBlockSize;

    if LAt = Length(FTokenBlocks) then
    begin
      { Directory capacity grows independently of token count; unused nil slots
        are private and do not enter any frozen directory or token view. }
      SetLength(FTokenBlocks, Capacity(LAt,
        (FOptions.MaximumTokens - 1) div CTranslationPrefixBlockSize + 1));
    end;
    FTokenBlocks[LAt] := LLease;
    FWriting := LBlock;
  end;
  LToken := Default(TNyxPascalTranslatedToken);
  LToken.FOccurrence := AOccurrence;
  LToken.FPhysicalToken := AToken;
  LToken.FState := FStateCount - 1;
  LToken.FActivity := FActive;
  LToken.FExpansion := AExpansion;
  LToken.FExpansionToken := AExpansionToken;
  LToken.FExpanded := AExpansion >= 0;

  if FBlocked then
  begin
    LToken.FActivity := ppkUnknown;
  end;
  FWriting.Rows[FTokenCount mod CTranslationPrefixBlockSize] := LToken;
  Inc(FTokenCount);

  if FTokenCount mod CTranslationPrefixBlockSize = 0 then
  begin
    FWriting := nil;
  end;
end;

procedure TTranslation.Word(AOccurrence, AIndex: Integer; const AToken: TNyxPascalToken;
  const AText: TNyxText);
var
  LExpansion: INyxPascalMacroExpansion;
  LIndex: Integer;
  LAt: Integer;
  LName: TNyxPascalDefineRef;
begin

  try
    LName := NyxPascalDefine(AText);
  except
    on ENyxPascalContext do
    begin
      { Extended Unicode/long Pascal identifiers need not be macro symbols. }
      AddToken(AOccurrence, AIndex);
      Exit;
    end;
  end;

  if (FActive <> ppkTrue) or FBlocked or AToken.Escaped or
    (AToken.Word <> pkwNone) or not FMacros.Has(LName) or
    (FContext.Feature(ppfReplacementMacros) = ppkFalse) then
  begin
    AddToken(AOccurrence, AIndex);
    Exit;
  end;
  try

    if FContext.Feature(ppfReplacementMacros) = ppkUnknown then
    begin
      raise ENyxPascalMacro.Create('Replacement enablement is not captured');
    end;
    LExpansion := FMacros.Expand(AText, pmuSource, FOptions.LexicalOptions.NestedComments(FNested),
      FOptions.MaximumExpandedBytes - FBytes, FOptions.MaximumTokens - FTokenCount,
      FOptions.MaximumIncludeDepth, FCancellation);
    VerifyBoundary(AOccurrence, AToken.Span, LExpansion);

    if Length(FExpansions) >= FOptions.MaximumOccurrences then
    begin
      raise ENyxPascalTranslationLimit.Create('Replacement invocations exceed their occurrence budget');
    end;
    Inc(FBytes, LExpansion.WorkBytes);
    LIndex := Length(FExpansions);
    SetLength(FExpansions, LIndex + 1);
    FExpansions[LIndex] := LExpansion;
    for LAt := 0 to LExpansion.Count - 1 do
    begin
      AddToken(AOccurrence, AIndex, LIndex, LAt);
    end;
  except
    on LError: ENyxPascalMacroCycle do
    begin
      Problem(ptiMacroCycle, AOccurrence, AToken.Span, TNyxText(LError.Message));
      UnknownEffect;
      AddToken(AOccurrence, AIndex);
    end;
    on LError: ENyxPascalMacro do
    begin
      Problem(ptiMacroContext, AOccurrence, AToken.Span, TNyxText(LError.Message));
      UnknownEffect;
      AddToken(AOccurrence, AIndex);
    end;
  end;
end;

procedure TTranslation.VerifyBoundary(AOccurrence: Integer; const ASpan: TNyxPascalSpan;
  const AExpansion: INyxPascalMacroExpansion);
var
  LFile: Integer;
  LView: INyxPascalSourceView;
  LStart: Integer;
  LFinish: Integer;
  LLeft: TNyxText;
  LRight: TNyxText;
  LJoined: TNyxText;
  LLexical: INyxPascalLexicalSnapshot;
  LAt: Integer;
  LSpan: TNyxPascalSpan;
begin
  LFile := FOccurrences[AOccurrence].FileIndex;

  if FBoundaryViews[LFile] = nil then
  begin
    { One canonical UTF-8 boundary map per visited file avoids rescanning the
      entire prefix at every use. It is private analysis data, not external-byte
      provenance; the catalog continues to own the exact original encoding. }
    FBoundaryViews[LFile] := DecodeNyxPascalSource(NewNyxPascalSourceBytes(
      NyxEncodeUTF8(FFiles.TextAt(LFile)), pboAuthoringUTF8), NyxPascalUTF8Codec, FCancellation);
  end;
  LView := FBoundaryViews[LFile];
  LStart := ASpan.Start - 8;

  if LStart < 0 then
  begin
    LStart := 0;
  end;
  LFinish := ASpan.Finish + 8;

  if LFinish > LView.Scalars then
  begin
    LFinish := LView.Scalars;
  end;
  LLeft := NyxDecodeUTF8(LView.Original.Slice(LView.OriginalSpan(
    NyxPascalSpan(LStart, ASpan.Start - LStart))));
  LRight := NyxDecodeUTF8(LView.Original.Slice(LView.OriginalSpan(
    NyxPascalSpan(ASpan.Finish, LFinish - ASpan.Finish))));
  LJoined := LLeft + AExpansion.Joined + LRight;

  if NyxUTF8ByteCount(LJoined) > FOptions.MaximumExpandedBytes - FBytes - AExpansion.WorkBytes then
  begin
    raise ENyxPascalMacroLimit.Create('Replacement boundary work exceeds its byte budget');
  end;
  Inc(FBytes, NyxUTF8ByteCount(LJoined));
  LLexical := NewNyxPascalLexicalSnapshot(LJoined,
    FOptions.LexicalOptions.NestedComments(FNested), FCancellation);
  LStart := ASpan.Start - LStart;
  LFinish := LStart + NyxTextScalarCount(AExpansion.Joined);
  for LAt := 0 to LLexical.Count - 2 do
  begin
    LSpan := LLexical.Token(LAt).Span;

    if (LLexical.Token(LAt).Kind <> ptkWhitespace) and
      (((LSpan.Start < LStart) and (LSpan.Finish > LStart)) or
       ((LSpan.Start < LFinish) and (LSpan.Finish > LFinish))) then
    begin
      raise ENyxPascalMacro.Create('A replacement joins surrounding source tokens; textual reader composition is required');
    end;
  end;
end;

procedure TTranslation.UnknownEffect;
begin
  { An unknown textual effect may also change comment/string lexing. Keep the
    remaining physical capture for inspection, but do not interpret another
    directive using stale lexical defaults or manufacture unclosed-frame errors. }
  FContext := FContext.UnknownState;
  FMacros := FMacros.UnknownState;
  FSettingsKnown := False;
  FBlocked := True;
  SaveState;
end;

procedure TTranslationReader.AfterToken(AIndex: Integer; const AToken: TNyxPascalToken;
  const AText: TNyxText; var AAllowNestedComments: Boolean);
begin

  if not Included or (AToken.Kind <> ptkEOF) then
  begin

    if AToken.Word in [pkwAsm, pkwEnd] then
    begin

      if AToken.Word = pkwAsm then
      begin
        Owner.FScanRegion := plrAssembly;
      end
      else
      begin
        Owner.FScanRegion := plrPascal;
      end;

      if Owner.FActive = ppkTrue then
      begin
        Owner.FRegion := Owner.FScanRegion;
      end
      else if (Owner.FActive = ppkUnknown) and
        (Owner.FRegion <> Owner.FScanRegion) and not Owner.FBlocked then
      begin
        Owner.Problem(ptiLexicalPolicyChange, Occurrence, AToken.Span,
          'A conditional assembly boundary needs resolved branch activity');
        Owner.UnknownEffect;
      end;
    end;

    if AToken.Kind = ptkWord then
    begin
      Owner.Word(Occurrence, AIndex, AToken, AText);
    end
    else
    begin
      Owner.AddToken(Occurrence, AIndex);
    end;

    if (AToken.Kind = ptkDirective) and not Owner.FBlocked then
    begin
      Owner.Directive(Occurrence, AToken, AText);
    end;

    if (AToken.Kind = ptkEOF) and not Owner.FBlocked and
      (Owner.FRegion = plrAssembly) then
    begin
      Owner.Problem(ptiLexicalIssue, Occurrence, AToken.Span,
        'Finish this assembly block with END');
    end;
  end;
  { Includes share the caller's compiler state. In particular a nested include
    may change the policy used for this reader's very next physical token. }
  AAllowNestedComments := Owner.FNested;
end;

procedure TTranslationReader.BeginRead(const ACapture: INyxPascalLexicalPrefixCapture);
begin
  Owner.FCaptures[Occurrence] := ACapture;
end;

function TTranslationReader.InitialRegion: TNyxPascalLexicalRegion;
begin
  Result := Owner.FScanRegion;
end;

function TTranslationReader.NextRegion: TNyxPascalLexicalRegion;
begin
  Result := Owner.FScanRegion;
end;

function SameLexicalView(const ALeft, ARight: INyxPascalLexicalSnapshot;
  const ACancellation: INyxCancellationScope): Boolean;
var
  LAt: Integer;
  LLeft: TNyxPascalToken;
  LRight: TNyxPascalToken;
begin
  Result := False;

  if (ALeft.Count <> ARight.Count) or
    (ALeft.Options.AllowNestedComments <> ARight.Options.AllowNestedComments) then
  begin
    Exit;
  end;
  for LAt := 0 to ALeft.Count - 1 do
  begin

    if (ACancellation <> nil) and ACancellation.Cancelled then
    begin
      raise ENyxPascalAnalysisCancelled.Create('Pascal lexical comparison cancelled');
    end;
    LLeft := ALeft.Token(LAt);
    LRight := ARight.Token(LAt);
    { Both views scan the same immutable text with fixed identifier policy.
      Nesting can alter only token boundaries, families and completeness. }

    if (LLeft.Kind <> LRight.Kind) or (LLeft.Span.Start <> LRight.Span.Start) or
      (LLeft.Span.Length <> LRight.Span.Length) or (LLeft.Complete <> LRight.Complete) then
    begin
      Exit;
    end;

    if LLeft.Region <> LRight.Region then
    begin
      Exit;
    end;
  end;
  Result := True;
end;

procedure TTranslation.Visit(AFile, AParent: Integer; const AInclude: TNyxPascalSpan);
var
  LOccurrence: Integer;
  LAt: Integer;
  LAncestor: Integer;
  LBytes: Integer;
  LLexical: INyxPascalLexicalSnapshot;
  LView: INyxPascalSourceView;
  LReader: TTranslationReader;
  LObserver: INyxPascalLexicalObserver;
begin
  CheckCancellation;

  if FDepth >= FOptions.MaximumIncludeDepth then
  begin
    raise ENyxPascalTranslationLimit.Create('Pascal include depth exceeds its budget');
  end;

  if FOccurrenceCount >= FOptions.MaximumOccurrences then
  begin
    raise ENyxPascalTranslationLimit.Create('Pascal include occurrences exceed their budget');
  end;
  LAncestor := AParent;
  while LAncestor >= 0 do
  begin

    if (FOccurrences[LAncestor].FileIndex = AFile) and
      CompilerState(FOccurrences[LAncestor].EntryState).Context.SameFacts(FContext) then
    begin
      Problem(ptiIncludeCycle, AParent, AInclude,
        'This include repeats without changing its captured compiler state');
      UnknownEffect;
      Exit;
    end;
    LAncestor := FOccurrences[LAncestor].Parent;
  end;
  LView := NyxPascalSourceViewAt(FFiles, AFile);

  if LView = nil then
  begin

    if AParent < 0 then
    begin
      raise ENyxPascalSourceEncoding.CreateForFile(FFiles.FileAt(AFile));
    end;
    Problem(ptiSourceEncoding, AParent, AInclude,
      'This captured include needs an explicit readable encoding: ' + FFiles.FileAt(AFile).Name);
    UnknownEffect;
    Exit;
  end;
  LBytes := NyxUTF8ByteCount(LView.Text);

  if LView.Original.Count > LBytes then
  begin
    LBytes := LView.Original.Count;
  end;

  if LBytes > FOptions.MaximumExpandedBytes - FBytes then
  begin
    raise ENyxPascalTranslationLimit.Create('Expanded Pascal source exceeds its byte budget');
  end;
  Inc(FBytes, LBytes);
  LOccurrence := FOccurrenceCount;

  if LOccurrence = Length(FOccurrences) then
  begin
    SetLength(FOccurrences, Capacity(LOccurrence, FOptions.MaximumOccurrences));
    SetLength(FLexical, Length(FOccurrences));
    SetLength(FCaptures, Length(FOccurrences));
  end;
  FOccurrences[LOccurrence].FFile := AFile;
  FOccurrences[LOccurrence].FParent := AParent;
  FOccurrences[LOccurrence].FIncludeSpan := AInclude;
  FOccurrences[LOccurrence].FFirst := FTokenCount;
  FOccurrences[LOccurrence].FEntry := FStateCount - 1;
  Inc(FOccurrenceCount);
  Inc(FDepth);
  try

    LReader := TTranslationReader.Create;
    LObserver := LReader;
    LReader.Owner := Self;
    LReader.Occurrence := LOccurrence;
    LReader.Included := AParent >= 0;
    LLexical := NewNyxPascalContextualLexicalSnapshot(LView.Text,
      FOptions.LexicalOptions.NestedComments(FNested), LObserver, FCancellation);
    FLexical[LOccurrence] := LLexical;
    for LAt := 0 to LLexical.DiagnosticCount - 1 do
    begin
      Problem(ptiLexicalIssue, LOccurrence, LLexical.Diagnostic(LAt).Span,
        LLexical.Diagnostic(LAt).MessageText);
    end;

    if FPhysical[AFile] = nil then
    begin
      FPhysical[AFile] := TPhysicalCache.Create;
      FPhysical[AFile].Lexical := LLexical;
    end
    else if not FPhysical[AFile].Ambiguous then
    begin
      FPhysical[AFile].Ambiguous := not SameLexicalView(
        FPhysical[AFile].Lexical, LLexical, FCancellation);
    end;
    FOccurrences[LOccurrence].FCount := FTokenCount - FOccurrences[LOccurrence].FFirst;
    FOccurrences[LOccurrence].FExit := FStateCount - 1;
  finally
    FCaptures[LOccurrence] := nil;
    Dec(FDepth);
  end;
end;

procedure NyxPascalDirectiveParts(const AText: TNyxText; out AName, AValue: TNyxText;
  out AValueOffset: Integer);
var
  LAt: Integer;
  LStart: Integer;
  LFinish: Integer;
begin
  LAt := 1;
  while (LAt <= Length(AText)) and (AText[LAt] <> '$') do
  begin
    Inc(LAt);
  end;
  Inc(LAt);
  LStart := LAt;
  while (LAt <= Length(AText)) and (AText[LAt] in ['A'..'Z', 'a'..'z']) do
  begin
    Inc(LAt);
  end;
  AName := NyxPascalASCIIKey(Copy(AText, LStart, LAt - LStart));
  LFinish := Length(AText) - 1;

  if (Length(AText) >= 2) and (Copy(AText, Length(AText) - 1, 2) = '*)') then
  begin
    Dec(LFinish);
  end;
  { Trim only Pascal ASCII whitespace in the original storage. ANSI RTL Trim
    can transcode a supplementary-Unicode include name on a native host. }
  while (LAt <= LFinish) and (AText[LAt] in [#9..#13, ' ']) do
  begin
    Inc(LAt);
  end;
  while (LFinish >= LAt) and (AText[LFinish] in [#9..#13, ' ']) do
  begin
    Dec(LFinish);
  end;
  AValue := Copy(AText, LAt, LFinish - LAt + 1);
  AValueOffset := NyxTextScalarCount(Copy(AText, 1, LAt - 1));
end;

function TTranslation.Condition(AOccurrence: Integer; const ASpan: TNyxPascalSpan;
  const AKind, AValue: TNyxText): TNyxPascalKnowledge;
var
  LName: TNyxPascalDefineRef;
  LOption: TNyxPascalKnowledge;
  LExpanded: INyxPascalMacroExpansion;
  LValue: TNyxText;
  LOrigin: TNyxPascalConditionOrigin;
  LPlannable: Boolean;
  LAnalysis: TNyxPascalConditionResult;
  LSnapshot: INyxPascalTranslation;
  LPrefix: INyxPascalTranslation;
begin
  Result := ppkUnknown;
  LValue := AValue;
  LPlannable := True;
  try

    if (AKind = 'ifdef') or (AKind = 'ifndef') then
    begin
      LName := NyxPascalDefine(AValue);
      LValue := 'defined(' + LName.Name + ')';
      Result := FContext.Symbol(LName).IsDefined;

      if AKind = 'ifndef' then
      begin
        LValue := 'not ' + LValue;
        Result := NyxPascalNot(Result);
      end;
    end
    else if AKind = 'ifopt' then
    begin
      LPlannable := False;

      if (Length(AValue) = 2) and (AValue[2] in ['+', '-']) then
      begin
        LOption := FContext.Option(AValue[1]);
        Result := LOption;

        if AValue[2] = '-' then
        begin
          Result := NyxPascalNot(Result);
        end;
      end;
    end
    else
    begin
      LValue := AValue;

      if (FContext.Feature(ppfReplacementMacros) = ppkTrue) and (FMacros.Count > 0) then
      begin
        LExpanded := FMacros.Expand(AValue, pmuCondition,
          FOptions.LexicalOptions.NestedComments(FNested), FOptions.MaximumExpandedBytes - FBytes,
          FOptions.MaximumTokens - FTokenCount, FOptions.MaximumIncludeDepth, FCancellation);
        Inc(FBytes, LExpanded.WorkBytes);
        LValue := LExpanded.Joined;
      end;
      LAnalysis := AnalyzeNyxPascalCondition(FContext, LValue);
      Result := LAnalysis.Truth;
      LPlannable := LAnalysis.Issue in [pciNone, pciUnknownSymbol];

      if (Result = ppkUnknown) and (FConditions <> nil) then
      begin
        LOrigin.FileReference := FFiles.FileAt(FOccurrences[AOccurrence].FileIndex);
        LOrigin.Occurrence := AOccurrence;
        LOrigin.Span := ASpan;
        LOrigin.PrefixCount := FTokenCount - 1;
        if FPrefixConditions <> nil then
        begin
          try

            if FPrefixConditions.PreparePrefix(LOrigin, FContext, LValue) then
            begin
              LSnapshot := TFrozenTranslation.CreatePrefix(Self);
              LPrefix := NewNyxPascalTranslationPrefix(LSnapshot,
                LOrigin.PrefixCount, FOptions, FCancellation);
              Result := FPrefixConditions.EvaluatePrefix(LOrigin, FContext, LValue, LPrefix);
            end;
          except
            on LError: ENyxPascalTranslationLimit do
            begin
              raise;
            end;
            on LError: ENyxPascalContext do
            begin
              { Internal prefix authority cannot become an authored malformed
                condition and leave a partially admitted reader running. }
              raise ENyxPascalTranslationLimit.Create(
                'Conditional prefix authority refused: ' + LError.Message);
            end;
          end;
        end
        else
        begin
          Result := FConditions.Evaluate(LOrigin, FContext, LValue);
        end;

        if (Ord(Result) < Ord(Low(TNyxPascalKnowledge))) or
          (Ord(Result) > Ord(High(TNyxPascalKnowledge))) then
        begin
          raise ENyxPascalTranslationLimit.Create('Conditional authority returned invalid knowledge');
        end;
      end;
    end;
  except
    on ENyxPascalTranslationLimit do
    begin
      raise;
    end;
    on ENyxPascalMacroLimit do
    begin
      raise;
    end;
    on ENyxPascalContext do
    begin
      { A malformed authored condition still owns its structural frame. Do not
        accidentally consume the enclosing conditional's endif during recovery. }
      Result := ppkUnknown;
      LPlannable := False;
    end;
  end;

  if Result = ppkUnknown then
  begin

    if FUnresolvedCount >= FOptions.MaximumDiagnostics then
    begin
      raise ENyxPascalTranslationLimit.Create('Unresolved conditions exceed their source diagnostic budget');
    end;

    if FUnresolvedCount = Length(FUnresolved) then
    begin
      SetLength(FUnresolved, Capacity(FUnresolvedCount, FOptions.MaximumDiagnostics));
    end;
    FUnresolved[FUnresolvedCount].Origin.FileReference := FFiles.FileAt(FOccurrences[AOccurrence].FileIndex);
    FUnresolved[FUnresolvedCount].Origin.Occurrence := AOccurrence;
    FUnresolved[FUnresolvedCount].Origin.Span := ASpan;
    FUnresolved[FUnresolvedCount].Origin.PrefixCount := FTokenCount - 1;
    FUnresolved[FUnresolvedCount].State := Token(FTokenCount - 1).State;
    FUnresolved[FUnresolvedCount].Expression := LValue;
    FUnresolved[FUnresolvedCount].Plannable := LPlannable;
    Inc(FUnresolvedCount);
  end;
end;

procedure TTranslation.ChangeSymbol(const AName: TNyxText; AKind: TNyxPascalDefineKind);
begin

  if FActive = ppkUnknown then
  begin
    AKind := ppdUnknown;
  end;
  FContext := FContext.WithSymbol(NyxPascalDefine(AName), AKind);
  SaveState;
end;

procedure TTranslation.ChangeOption(AName: Char; AValue: TNyxPascalKnowledge);
begin

  if FActive = ppkUnknown then
  begin
    AValue := ppkUnknown;
  end;
  FContext := FContext.WithOption(AName, AValue);
  SaveState;
end;

procedure TTranslation.ChangeOptions(const AText: TNyxText);
var
  LAt: Integer;
  LLetter: Char;
  LSeen: set of 'A'..'Z';
  LValues: array['A'..'Z'] of TNyxPascalKnowledge;
  LValue: TNyxPascalKnowledge;
  LCandidate: INyxPascalCompilerContext;
  LFPC: Boolean;
begin
  { Captured FPC contexts continue only at a comma immediately after the sign.
    Other trailing characters, including whitespace followed by a comma, remain
    comment text; never interpret a later comma inside that annotation. Other
    compiler families retain strict separated groups, without assuming FPC's
    annotation rule or claiming their scanner has been qualified here.
    A committed comma still needs another complete supported switch. Validate
    that whole group before creating at most 26 context copies: last authored
    write wins, while a malformed group publishes no partial historical state. }
  LSeen := [];
  LFPC := FContext.Family = ppcFPC;
  LAt := 1;
  repeat

    if not LFPC then
    begin
      while (LAt <= Length(AText)) and (AText[LAt] in [#9..#13, ' ']) do
      begin
        Inc(LAt);
      end;
    end;

    if (LAt > Length(AText)) or not (AText[LAt] in ['a'..'z', 'A'..'Z']) then
    begin
      raise ENyxPascalContext.Create('A short switch needs one letter');
    end;
    LLetter := AText[LAt];

    if LLetter in ['a'..'z'] then
    begin
      LLetter := Chr(Ord(LLetter) - 32);
    end;
    Inc(LAt);

    if (LAt > Length(AText)) or not (AText[LAt] in ['+', '-']) then
    begin
      raise ENyxPascalContext.Create('A short switch needs plus or minus');
    end;
    LValue := ppkFalse;

    if AText[LAt] = '+' then
    begin
      LValue := ppkTrue;
    end;

    if FActive = ppkUnknown then
    begin
      LValue := ppkUnknown;
    end;

    if LLetter in ['a', 'A'] then
    begin
      { Boolean alignment aliases have mode-specific meanings. No independent
        Boolean option may replace that unresolved structured compiler policy. }
      raise ENyxPascalContext.Create('Boolean alignment needs mode-specific capture');
    end;
    Include(LSeen, LLetter);
    LValues[LLetter] := LValue;
    Inc(LAt);

    if not LFPC then
    begin
      while (LAt <= Length(AText)) and (AText[LAt] in [#9..#13, ' ']) do
      begin
        Inc(LAt);
      end;
    end;

    if LAt > Length(AText) then
    begin
      Break;
    end;

    if AText[LAt] <> ',' then
    begin

      if LFPC then
      begin
        Break;
      end;
      raise ENyxPascalContext.Create('Short switches need comma separators');
    end;
    Inc(LAt);
  until False;
  LCandidate := FContext;
  for LLetter := 'A' to 'Z' do
  begin

    if LLetter in LSeen then
    begin
      LCandidate := LCandidate.WithOption(LLetter, LValues[LLetter]);
    end;
  end;
  FContext := LCandidate;
  SaveState;
end;

function ModeNamed(const AName: TNyxText): TNyxPascalLanguageMode;
const
  CNames: array[TNyxPascalLanguageMode] of TNyxText =
    ('', 'fpc', 'objfpc', 'delphi', 'delphiunicode', 'tp', 'macpas', 'iso', 'extendedpascal');
var
  LMode: TNyxPascalLanguageMode;
begin
  Result := ppmUnknown;
  for LMode := Succ(Low(TNyxPascalLanguageMode)) to High(TNyxPascalLanguageMode) do
  begin

    if CNames[LMode] = NyxPascalASCIIKey(AName) then
    begin
      Exit(LMode);
    end;
  end;
end;

function FeatureNamed(const AName: TNyxText; out AFeature: TNyxPascalLanguageFeature): Boolean;
const
  CNames: array[TNyxPascalLanguageFeature] of TNyxText =
    ('advancedrecords', 'nestedprocvars', 'anonymousfunctions', 'functionreferences',
      'typehelpers', 'nestedcomments', 'scopedenums', 'unicodestrings', 'out', '', 'class', 'cvar');
var
  LFeature: TNyxPascalLanguageFeature;
begin
  for LFeature := Low(TNyxPascalLanguageFeature) to High(TNyxPascalLanguageFeature) do
  begin

    if (CNames[LFeature] <> '') and (CNames[LFeature] = AName) then
    begin
      AFeature := LFeature;
      Exit(True);
    end;
  end;
  Result := False;
end;

function LocalSwitchNamed(const AName: TNyxText; out ASwitch: TNyxPascalLocalSwitch): Boolean;
const
  CNames: array[TNyxPascalLocalSwitch] of TNyxText =
    ('booleval', 'assertions', 'longstrings', 'iochecks', 'writeableconst',
      'overflowchecks', 'rangechecks', 'stackchecking', 'typedaddress',
      'varstringchecks', 'stackframes');
var
  LSwitch: TNyxPascalLocalSwitch;
begin
  for LSwitch := Low(TNyxPascalLocalSwitch) to High(TNyxPascalLocalSwitch) do
  begin

    if CNames[LSwitch] = AName then
    begin
      ASwitch := LSwitch;
      Exit(True);
    end;
  end;
  Result := False;
end;

{ Layout directives consume one whitespace-delimited value; subsequent prose
  remains part of the exact directive comment. This is deliberately local to
  these closed policy readers, never a truncation rule for conditions, macros,
  quoted include paths or arbitrary directives. }
function LayoutDirectiveValue(const AValue: TNyxText): TNyxText;
var
  LAt: Integer;
begin
  LAt := 1;
  while (LAt <= Length(AValue)) and not (AValue[LAt] in [#9..#13, ' ']) do
  begin
    Inc(LAt);
  end;
  Result := NyxPascalASCIIKey(Copy(AValue, 1, LAt - 1));
end;

function RecordAlignmentNamed(const AValue: TNyxText): TNyxPascalRecordAlignment;
begin
  Result := praUnknown;

  if (AValue = 'default') or (AValue = 'normal') or (AValue = '0') then
  begin
    Result := praDefault;
  end
  else if AValue = 'c' then
  begin
    Result := praC;
  end
  else if AValue = '1' then
  begin
    Result := pra1;
  end
  else if AValue = '2' then
  begin
    Result := pra2;
  end
  else if AValue = '4' then
  begin
    Result := pra4;
  end
  else if AValue = '8' then
  begin
    Result := pra8;
  end
  else if AValue = '16' then
  begin
    Result := pra16;
  end
  else if AValue = '32' then
  begin
    Result := pra32;
  end;
end;

function EnumStorageNamed(const AValue: TNyxText): TNyxPascalEnumStorage;
begin
  Result := pesUnknown;

  if (AValue = 'default') or (AValue = 'normal') or (AValue = '0') then
  begin
    Result := pesDefault;
  end
  else if AValue = '1' then
  begin
    Result := pes1;
  end
  else if AValue = '2' then
  begin
    Result := pes2;
  end
  else if AValue = '4' then
  begin
    Result := pes4;
  end;
end;

function CallingNamed(const AValue: TNyxText): TNyxPascalCallingConvention;
const
  CNames: array[TNyxPascalCallingConvention] of TNyxText =
    ('', 'default', 'register', 'cdecl', 'pascal', 'safecall', 'stdcall', 'oldfpccall');
var
  LConvention: TNyxPascalCallingConvention;
begin
  for LConvention := pccDefault to High(TNyxPascalCallingConvention) do
  begin

    if CNames[LConvention] = AValue then
    begin
      Exit(LConvention);
    end;
  end;
  Result := pccUnknown;
end;

{ FPC's accepted set-packing spellings denote intent, not computed byte size. }
function SetPackingNamed(const AValue: TNyxText): TNyxPascalSetPacking;
begin
  Result := pspUnknown;

  if (AValue = 'fixed') or (AValue = 'default') or (AValue = 'normal') then
  begin
    Result := pspFixed;
  end
  else if AValue = '1' then
  begin
    Result := psp1;
  end
  else if AValue = '2' then
  begin
    Result := psp2;
  end
  else if AValue = '4' then
  begin
    Result := psp4;
  end
  else if AValue = '8' then
  begin
    Result := psp8;
  end;
end;

{ FPC's category toggle reader consumes one ON/OFF identifier or one sign.
  Remaining source belongs to the directive's comment and remains untouched in
  the physical token. Consume the whole identifier so OFFLINE/OFF1 never becomes
  OFF; DEFAULT and per-message ERROR are not category-emission states. }
function ParseDiagnosticEmission(const AValue: TNyxText): Boolean;
var
  LAt: Integer;
  LWord: TNyxText;
begin

  if AValue = '' then
  begin
    raise ENyxPascalContext.Create('A diagnostic category needs ON/OFF or a sign');
  end;

  if AValue[1] in ['+', '-'] then
  begin
    Exit(AValue[1] = '+');
  end;
  LAt := 1;
  while (LAt <= Length(AValue)) and (AValue[LAt] in ['a'..'z', 'A'..'Z', '0'..'9', '_']) do
  begin
    Inc(LAt);
  end;
  LWord := NyxPascalASCIIKey(Copy(AValue, 1, LAt - 1));

  if LWord = 'on' then
  begin
    Exit(True);
  end;

  if LWord = 'off' then
  begin
    Exit(False);
  end;
  raise ENyxPascalContext.Create('A diagnostic category needs ON/OFF or a sign');
end;

{ FPC consumes one selector and action, including adjacent +/- spelling.
  Explanatory trailing text stays in the physical directive receipt. Numeric and
  named selectors are not collapsed through a guessed compiler message catalog. }
procedure ParseWarningRequest(const AValue: TNyxText;
  out AReference: TNyxPascalWarningRef; out AAction: TNyxPascalWarningAction);
var
  LAt: Integer;
  LStart: Integer;
  LSelector: TNyxText;
  LAction: TNyxText;
  LNumber: Integer;
  LDigit: Integer;
  LNumeric: Boolean;
begin
  LAt := 1;
  while (LAt <= Length(AValue)) and (AValue[LAt] in [#9, #10, #13, ' ']) do
  begin
    Inc(LAt);
  end;
  LStart := LAt;
  LNumeric := True;
  while (LAt <= Length(AValue)) and
    (AValue[LAt] in ['A'..'Z', 'a'..'z', '0'..'9', '_']) do
  begin
    LNumeric := LNumeric and (AValue[LAt] in ['0'..'9']);
    Inc(LAt);
  end;
  LSelector := Copy(AValue, LStart, LAt - LStart);

  if LSelector = '' then
  begin
    raise ENyxPascalContext.Create('A warning directive needs a numeric or named selector');
  end;

  if LNumeric then
  begin
    LNumber := 0;
    for LStart := 1 to Length(LSelector) do
    begin
      LDigit := Ord(LSelector[LStart]) - Ord('0');

      if LNumber > (High(Integer) - LDigit) div 10 then
      begin
        raise ENyxPascalContext.Create('A warning message ID exceeds the signed-32 domain');
      end;
      LNumber := LNumber * 10 + LDigit;
    end;
    AReference := NyxPascalWarningNumber(LNumber);
  end
  else
  begin
    AReference := NyxPascalWarningName(LSelector);
  end;
  while (LAt <= Length(AValue)) and (AValue[LAt] in [#9, #10, #13, ' ']) do
  begin
    Inc(LAt);
  end;
  LStart := LAt;

  if (LAt <= Length(AValue)) and (AValue[LAt] in ['+', '-']) then
  begin
    Inc(LAt);
  end
  else
  begin
    while (LAt <= Length(AValue)) and
      (AValue[LAt] in ['A'..'Z', 'a'..'z', '0'..'9', '_']) do
    begin
      Inc(LAt);
    end;
  end;
  LAction := NyxPascalASCIIKey(Copy(AValue, LStart, LAt - LStart));
  AAction := pwaUnknown;

  if (LAction = 'on') or (LAction = '+') then
  begin
    AAction := pwaOn;
  end
  else if (LAction = 'off') or (LAction = '-') then
  begin
    AAction := pwaOff;
  end
  else if LAction = 'error' then
  begin
    AAction := pwaError;
  end
  else
  begin
    raise ENyxPascalContext.Create('A warning directive needs on, off or error');
  end;
end;

{ FPC's source directive reads one ASCII selector with readid, not the command-
  line comma list. Keep the original span, but never apply trailing selectors or
  explanatory text. Presets remain symbolic; named passes grant no target support. }
function ParseOptimizationRequest(const AValue: TNyxText): TNyxPascalOptimizationRequest;
const
  Names: array[TNyxPascalOptimizationFeature] of String = ('level1', 'level2', 'level3',
    'level4', 'regvar', 'uncertain', 'size', 'stackframe', 'peephole', 'loopunroll',
    'tailrec', 'cse', 'dfa', 'strength', 'schedule', 'autoinline', 'useebp', 'userbp',
    'orderfields', 'fastmath', 'deadvalues', 'removeemptyprocs', 'constprop', 'deadstore',
    'forcenostackframe', 'useloadmodifystore', 'unusedpara', 'consts', 'forloop');
var
  LAt: Integer;
  LName: TNyxText;
  LEnabled: Boolean;
  LFeature: TNyxPascalOptimizationFeature;
begin
  LAt := 1;
  while (LAt <= Length(AValue)) and (AValue[LAt] in ['A'..'Z', 'a'..'z', '0'..'9', '_']) do
  begin
    Inc(LAt);
  end;

  if (LAt = 1) or (LAt > 256) then
  begin
    raise ENyxPascalContext.Create('An optimization directive needs one bounded ASCII selector');
  end;
  LName := NyxPascalASCIIKey(Copy(AValue, 1, LAt - 1));

  if LName = 'on' then
  begin
    Exit(TNyxPascalOptimizationRequest.Select(pomLevelTwo));
  end;

  if LName = 'off' then
  begin
    Exit(TNyxPascalOptimizationRequest.Select(pomDisabled));
  end;

  if LName = 'default' then
  begin
    Exit(TNyxPascalOptimizationRequest.Select(pomInitial));
  end;
  LEnabled := Copy(LName, 1, 2) <> TNyxText('no');

  if not LEnabled then
  begin
    LName := Copy(LName, 3, Length(LName) - 2);
  end;
  for LFeature := Low(TNyxPascalOptimizationFeature) to High(TNyxPascalOptimizationFeature) do
  begin

    if LName = TNyxText(Names[LFeature]) then
    begin
      Exit(TNyxPascalOptimizationRequest.Toggle(LFeature, LEnabled));
    end;
  end;
  raise ENyxPascalContext.Create('This optimizer selector needs explicit compiler-policy support');
end;

{ Persistence boundary for the supported signed-decimal CODEALIGN spelling.
  Keep assignments in source order: FPC applies a temporary packet per directive,
  and duplicate/minimum/maximum assignments are not a flat last-value map.
  Alternate integer notation and malformed/unknown selectors refuse explicitly. }
function ParseCodeAlignmentRequest(const AValue: TNyxText): TNyxPascalCodeAlignmentRequest;
const
  Names: array[TNyxPascalCodeAlignmentSubject] of String = ('proc', 'jump', 'loop',
    'constmin', 'constmax', 'varmin', 'varmax', 'localmin', 'localmax', 'recordmin', 'recordmax');
var
  LAt: Integer;
  LStart: Integer;
  LName: TNyxText;
  LSubject: TNyxPascalCodeAlignmentSubject;
  LCandidate: TNyxPascalCodeAlignmentSubject;
  LFound: Boolean;
  LNegative: Boolean;
  LNumber: Integer;
  LDigit: Integer;
begin
  Result := Default(TNyxPascalCodeAlignmentRequest);
  LAt := 1;
  repeat
    while (LAt <= Length(AValue)) and (AValue[LAt] in [#9..#13, ' ']) do
    begin
      Inc(LAt);
    end;
    LStart := LAt;
    while (LAt <= Length(AValue)) and (AValue[LAt] in ['a'..'z', 'A'..'Z']) do
    begin
      Inc(LAt);
    end;
    LName := NyxPascalASCIIKey(Copy(AValue, LStart, LAt - LStart));
    LFound := False;
    LSubject := pcaProcedure;
    for LCandidate := Low(TNyxPascalCodeAlignmentSubject) to High(TNyxPascalCodeAlignmentSubject) do
    begin

      if LName = TNyxText(Names[LCandidate]) then
      begin
        LSubject := LCandidate;
        LFound := True;
        Break;
      end;
    end;

    if not LFound or (LAt > Length(AValue)) or (AValue[LAt] <> '=') then
    begin
      raise ENyxPascalContext.Create('CODEALIGN needs a supported selector followed by equals');
    end;
    Inc(LAt);
    LNegative := False;

    if (LAt <= Length(AValue)) and (AValue[LAt] in ['+', '-']) then
    begin
      LNegative := AValue[LAt] = '-';
      Inc(LAt);
    end;
    LStart := LAt;
    LNumber := 0;
    while (LAt <= Length(AValue)) and (AValue[LAt] in ['0'..'9']) do
    begin
      LDigit := Ord(AValue[LAt]) - Ord('0');
      { Accumulate negatively so the exact signed-32 minimum never overflows. }

      if LNumber < (Low(Integer) + LDigit) div 10 then
      begin
        raise ENyxPascalContext.Create('A code-alignment request exceeds signed-32');
      end;
      LNumber := LNumber * 10 - LDigit;
      Inc(LAt);
    end;

    if (LAt = LStart) or ((LAt <= Length(AValue)) and (AValue[LAt] <> ',')) or
      (not LNegative and (LNumber = Low(Integer))) then
    begin
      raise ENyxPascalContext.Create('A code-alignment request needs an exact signed-decimal integer');
    end;

    if not LNegative then
    begin
      LNumber := -LNumber;
    end;
    Result := Result.Add(LSubject, LNumber);

    if LAt > Length(AValue) then
    begin
      Break;
    end;
    Inc(LAt);
  until False;
  Result.Validate;
end;

procedure TTranslation.Directive(AOccurrence: Integer; const AToken: TNyxPascalToken;
  const AText: TNyxText);
var
  LName: TNyxText;
  LValue: TNyxText;
  LArgument: TNyxText;
  LCondition: TNyxPascalKnowledge;
  LAt: Integer;
  LFrame: TConditionalFrame;
  LMode: TNyxPascalLanguageMode;
  LFeature: TNyxPascalLanguageFeature;
  LOffset: Integer;
  LSeparator: Integer;
  LDefinition: TNyxPascalMacroDefinition;
  LPolicies: TNyxPascalCompilerPolicies;
  LAlignment: TNyxPascalRecordAlignment;
  LStorage: TNyxPascalEnumStorage;
  LSetPacking: TNyxPascalSetPacking;
  LCalling: TNyxPascalCallingConvention;
  LSwitch: TNyxPascalLocalSwitch;
  LSaved: TSavedCompilerSettings;
  LSavedLease: INyxSavedCompilerSettings;
  LWarningRef: TNyxPascalWarningRef;
  LWarningAction: TNyxPascalWarningAction;
  LWarningContext: INyxPascalCompilerContext;
  LWarnings: INyxPascalContextWarnings;
  LRegister: TNyxPascalFPURegisterPolicy;
  LPointerChecking: TNyxPascalPointerCheckingRequest;
  LRegisterValue: Integer;
  LReader: TNyxPascalAssemblerReader;
  LCodeAlignment: TNyxPascalCodeAlignmentRequest;
  LCodeAlignments: INyxPascalContextCodeAlignments;
  LAlignedContext: INyxPascalCompilerContext;
  LOptimization: TNyxPascalOptimizationRequest;
  LOptimizations: INyxPascalContextOptimizations;
  LOptimizedContext: INyxPascalCompilerContext;
  LChannel: TNyxPascalDiagnosticChannel;
  LEmissionEnabled: Boolean;
  LInterfaces: TNyxPascalInterfaceStyle;
begin
  NyxPascalDirectiveParts(AText, LName, LValue, LOffset);
  try

    if (LName = 'ifdef') or (LName = 'ifndef') or (LName = 'if') or (LName = 'ifopt') then
    begin

      if Length(FFrames) >= FOptions.MaximumIncludeDepth then
      begin
        raise ENyxPascalTranslationLimit.Create('Pascal conditional depth exceeds its budget');
      end;
      LCondition := ppkFalse;

      if FActive <> ppkFalse then
      begin
        LCondition := Condition(AOccurrence, AToken.Span, LName, LValue);

        if LCondition = ppkUnknown then
        begin
          Problem(ptiUnknownCondition, AOccurrence, AToken.Span,
            'This condition needs additional captured compiler or semantic facts');
        end;
      end;
      LFrame := Default(TConditionalFrame);
      LFrame.Parent := FActive;
      LFrame.Taken := LCondition;
      LFrame.Occurrence := AOccurrence;
      LFrame.Span := AToken.Span;
      LFrame.EntryRegion := FScanRegion;
      LAt := Length(FFrames);
      SetLength(FFrames, LAt + 1);
      FFrames[LAt] := LFrame;
      FActive := NyxPascalAnd(FActive, LCondition);
      Exit;
    end;

    if (LName = 'else') or (LName = 'elseif') or (LName = 'endif') or (LName = 'ifend') then
    begin
      LAt := High(FFrames);

      if LAt < 0 then
      begin
        Problem(ptiUnbalancedConditional, AOccurrence, AToken.Span,
          'This conditional boundary has no opening directive');
        UnknownEffect;
        FActive := ppkUnknown;
        Exit;
      end;
      LFrame := FFrames[LAt];

      if (LName = 'endif') or (LName = 'ifend') then
      begin
        FScanRegion := FRegion;

        if LFrame.Parent = ppkFalse then
        begin
          FScanRegion := LFrame.EntryRegion;
        end;
        FActive := LFrame.Parent;
        SetLength(FFrames, LAt);
      end
      else
      begin
        { Each discarded branch is scanned from its own entry envelope. Only
          selected tokens change FRegion; an inactive END cannot close live ASM. }
        FScanRegion := LFrame.EntryRegion;

        if LFrame.HasElse then
        begin
          Problem(ptiUnbalancedConditional, AOccurrence, AToken.Span,
            'A conditional branch cannot follow its final else');
          FActive := ppkUnknown;
          UnknownEffect;
          Exit;
        end;
        LCondition := ppkTrue;

        if LName = 'elseif' then
        begin

          if (LFrame.Parent = ppkFalse) or (LFrame.Taken = ppkTrue) then
          begin
            LCondition := ppkFalse;
          end
          else
          begin
            LCondition := Condition(AOccurrence, AToken.Span, 'if', LValue);

            if LCondition = ppkUnknown then
            begin
              Problem(ptiUnknownCondition, AOccurrence, AToken.Span,
                'This alternate condition needs additional captured facts');
            end;
          end;
        end
        else
        begin
          LFrame.HasElse := True;
        end;
        FActive := NyxPascalAnd(LFrame.Parent,
          NyxPascalAnd(NyxPascalNot(LFrame.Taken), LCondition));
        LFrame.Taken := NyxPascalOr(LFrame.Taken, LCondition);
        FFrames[LAt] := LFrame;
      end;
      Exit;
    end;

    if FActive = ppkFalse then
    begin
      Exit;
    end;

    if (LName = 'warnings') or (LName = 'hints') or (LName = 'notes') then
    begin

      if FContext.Family <> ppcFPC then
      begin
        Problem(ptiUnsupportedDirective, AOccurrence, AToken.Span,
          'Category emission requests need an explicit FPC context');
        UnknownEffect;
        Exit;
      end;
      LChannel := pdcWarnings;

      if LName = 'hints' then
      begin
        LChannel := pdcHints;
      end
      else if LName = 'notes' then
      begin
        LChannel := pdcNotes;
      end;
      LEmissionEnabled := ParseDiagnosticEmission(LValue);
      LCondition := ppkFalse;

      if LEmissionEnabled then
      begin
        LCondition := ppkTrue;
      end;

      if FActive = ppkUnknown then
      begin
        LCondition := ppkUnknown;
      end;

      if FEmissionCount >= FOptions.MaximumTokens then
      begin
        raise ENyxPascalTranslationLimit.Create('Emission requests exceed their construction budget');
      end;
      LWarningContext := NyxPascalWithCompilerPolicies(FContext,
        NyxPascalCompilerPolicies(FContext).EmitDiagnostics(LChannel, LCondition));

      if FEmissionCount = Length(FEmissionDirectives) then
      begin
        SetLength(FEmissionDirectives, Capacity(FEmissionCount, FOptions.MaximumTokens));
      end;
      FEmissionDirectives[FEmissionCount].FOrigin.FileReference :=
        FFiles.FileAt(FOccurrences[AOccurrence].FileIndex);
      FEmissionDirectives[FEmissionCount].FOrigin.Occurrence := AOccurrence;
      FEmissionDirectives[FEmissionCount].FOrigin.Span := AToken.Span;
      FEmissionDirectives[FEmissionCount].FOrigin.PrefixCount := FTokenCount - 1;
      FEmissionDirectives[FEmissionCount].FState := Token(FTokenCount - 1).State;
      FEmissionDirectives[FEmissionCount].FActivity := FActive;
      FEmissionDirectives[FEmissionCount].FChannel := LChannel;
      FEmissionDirectives[FEmissionCount].FEnabled := LEmissionEnabled;
      Inc(FEmissionCount);
      FContext := LWarningContext;
      SaveState;
      Exit;
    end;

    if LName = 'warn' then
    begin

      if FContext.Family <> ppcFPC then
      begin
        Problem(ptiUnsupportedDirective, AOccurrence, AToken.Span,
          'Per-warning source policies need an explicit FPC context');
        UnknownEffect;
        Exit;
      end;
      ParseWarningRequest(LValue, LWarningRef, LWarningAction);
      LWarnings := NyxPascalWarnings(FContext);

      if (LWarnings.WarningCount >= NyxPascalWarningRuleLimit) or
        (FWarningCount >= FOptions.MaximumTokens) then
      begin
        raise ENyxPascalTranslationLimit.Create('Warning requests exceed their construction budget');
      end;
      LWarningContext := LWarnings.WithWarning(LWarningRef, LWarningAction, FActive);

      if FWarningCount = Length(FWarningDirectives) then
      begin
        SetLength(FWarningDirectives, Capacity(FWarningCount, FOptions.MaximumTokens));
      end;
      FWarningDirectives[FWarningCount].FOrigin.FileReference :=
        FFiles.FileAt(FOccurrences[AOccurrence].FileIndex);
      FWarningDirectives[FWarningCount].FOrigin.Occurrence := AOccurrence;
      FWarningDirectives[FWarningCount].FOrigin.Span := AToken.Span;
      FWarningDirectives[FWarningCount].FOrigin.PrefixCount := FTokenCount - 1;
      FWarningDirectives[FWarningCount].FState := Token(FTokenCount - 1).State;
      LWarnings := NyxPascalWarnings(LWarningContext);
      FWarningDirectives[FWarningCount].FRule := LWarnings.WarningAt(LWarnings.WarningCount - 1);
      Inc(FWarningCount);
      FContext := LWarningContext;
      SaveState;
      Exit;
    end;

    if LName = 'optimization' then
    begin

      if FContext.Family <> ppcFPC then
      begin
        Problem(ptiUnsupportedDirective, AOccurrence, AToken.Span,
          'Optimization requests need an explicit FPC context');
        UnknownEffect;
        Exit;
      end;
      LOptimization := ParseOptimizationRequest(LValue);
      LOptimizations := NyxPascalOptimizations(FContext);

      if (LOptimizations.OptimizationCount >= NyxPascalOptimizationRuleLimit) or
        (FOptimizationCount >= FOptions.MaximumTokens) then
      begin
        raise ENyxPascalTranslationLimit.Create('Optimizer requests exceed their construction budget');
      end;
      LOptimizedContext := LOptimizations.WithOptimization(LOptimization, FActive);

      if FOptimizationCount = Length(FOptimizationDirectives) then
      begin
        SetLength(FOptimizationDirectives, Capacity(FOptimizationCount, FOptions.MaximumTokens));
      end;
      FOptimizationDirectives[FOptimizationCount].FOrigin.FileReference :=
        FFiles.FileAt(FOccurrences[AOccurrence].FileIndex);
      FOptimizationDirectives[FOptimizationCount].FOrigin.Occurrence := AOccurrence;
      FOptimizationDirectives[FOptimizationCount].FOrigin.Span := AToken.Span;
      FOptimizationDirectives[FOptimizationCount].FOrigin.PrefixCount := FTokenCount - 1;
      FOptimizationDirectives[FOptimizationCount].FState := Token(FTokenCount - 1).State;
      LOptimizations := NyxPascalOptimizations(LOptimizedContext);
      FOptimizationDirectives[FOptimizationCount].FRule :=
        LOptimizations.OptimizationAt(LOptimizations.OptimizationCount - 1);
      Inc(FOptimizationCount);
      FContext := LOptimizedContext;
      SaveState;
      Exit;
    end;

    if LName = 'codealign' then
    begin

      if FContext.Family <> ppcFPC then
      begin
        Problem(ptiUnsupportedDirective, AOccurrence, AToken.Span,
          'Code-alignment requests need an explicit FPC context');
        UnknownEffect;
        Exit;
      end;
      LCodeAlignment := ParseCodeAlignmentRequest(LValue);
      LCodeAlignments := NyxPascalCodeAlignments(FContext);

      if (LCodeAlignments.CodeAlignmentCount >= NyxPascalCodeAlignmentRuleLimit) or
        (FCodeAlignmentCount >= FOptions.MaximumTokens) then
      begin
        raise ENyxPascalTranslationLimit.Create('Code-alignment requests exceed their construction budget');
      end;
      LAlignedContext := LCodeAlignments.WithCodeAlignment(LCodeAlignment, FActive);

      if FCodeAlignmentCount = Length(FCodeAlignmentDirectives) then
      begin
        SetLength(FCodeAlignmentDirectives, Capacity(FCodeAlignmentCount, FOptions.MaximumTokens));
      end;
      FCodeAlignmentDirectives[FCodeAlignmentCount].FOrigin.FileReference :=
        FFiles.FileAt(FOccurrences[AOccurrence].FileIndex);
      FCodeAlignmentDirectives[FCodeAlignmentCount].FOrigin.Occurrence := AOccurrence;
      FCodeAlignmentDirectives[FCodeAlignmentCount].FOrigin.Span := AToken.Span;
      FCodeAlignmentDirectives[FCodeAlignmentCount].FOrigin.PrefixCount := FTokenCount - 1;
      FCodeAlignmentDirectives[FCodeAlignmentCount].FState := Token(FTokenCount - 1).State;
      LCodeAlignments := NyxPascalCodeAlignments(LAlignedContext);
      FCodeAlignmentDirectives[FCodeAlignmentCount].FRule :=
        LCodeAlignments.CodeAlignmentAt(LCodeAlignments.CodeAlignmentCount - 1);
      Inc(FCodeAlignmentCount);
      FContext := LAlignedContext;
      SaveState;
      Exit;
    end;

    if LName = 'maxfpuregisters' then
    begin

      if FContext.Family <> ppcFPC then
      begin
        raise ENyxPascalContext.Create('Register directives need an explicit FPC context');
      end;
      LArgument := NyxPascalASCIIKey(LValue);
      LRegister := pfrDefault;

      if (LArgument <> 'normal') and (LArgument <> 'default') then
      begin
        LRegisterValue := 0;

        if LArgument = '' then
        begin
          raise ENyxPascalContext.Create('A register directive needs a count or default policy');
        end;
        for LAt := 1 to Length(LArgument) do
        begin

          if not (LArgument[LAt] in ['0'..'9']) then
          begin
            raise ENyxPascalContext.Create('A register limit needs decimal digits');
          end;
          LRegisterValue := LRegisterValue * 10 + Ord(LArgument[LAt]) - Ord('0');

          if (LRegisterValue > 8) or ((LRegisterValue = 8) and
            (FContext.Version.FullNumber < 30301)) then
          begin
            raise ENyxPascalContext.Create('This register count needs a qualified compiler context');
          end;
        end;
        LRegister := TNyxPascalFPURegisterPolicy(Ord(pfr0) + LRegisterValue);
      end;

      if FRegisterCount >= FOptions.MaximumTokens then
      begin
        raise ENyxPascalTranslationLimit.Create('Register directive intents exceed the token budget');
      end;

      if FRegisterCount = Length(FRegisterDirectives) then
      begin
        SetLength(FRegisterDirectives, Capacity(FRegisterCount, FOptions.MaximumTokens));
      end;
      FRegisterDirectives[FRegisterCount].Origin.FileReference :=
        FFiles.FileAt(FOccurrences[AOccurrence].FileIndex);
      FRegisterDirectives[FRegisterCount].Origin.Occurrence := AOccurrence;
      FRegisterDirectives[FRegisterCount].Origin.Span := AToken.Span;
      FRegisterDirectives[FRegisterCount].Origin.PrefixCount := FTokenCount - 1;
      FRegisterDirectives[FRegisterCount].State := Token(FTokenCount - 1).State;
      FRegisterDirectives[FRegisterCount].Activity := FActive;
      FRegisterDirectives[FRegisterCount].Policy := LRegister;
      Inc(FRegisterCount);
      Exit;
    end;

    if LName = 'asmmode' then
    begin

      if (FContext.Family <> ppcFPC) or (FRegion = plrAssembly) then
      begin
        raise ENyxPascalContext.Create('Assembler reader changes need an FPC boundary outside ASM');
      end;
      LArgument := NyxPascalASCIIKey(LValue);
      LReader := parUnknown;

      if LArgument = 'att' then
      begin
        LReader := parATT;
      end
      else if LArgument = 'intel' then
      begin
        LReader := parIntel;
      end
      else if LArgument = 'default' then
      begin
        LReader := parDefault;
      end;

      if LReader = parUnknown then
      begin
        raise ENyxPascalContext.Create('This assembler reader needs explicit support');
      end;

      if FActive = ppkUnknown then
      begin
        LReader := parUnknown;
      end;
      FContext := NyxPascalWithCompilerPolicies(FContext,
        NyxPascalCompilerPolicies(FContext).ReadAssemblyBy(LReader));
      SaveState;
      Exit;
    end;

    if (LName = 'push') or (LName = 'pop') then
    begin

      if (LValue <> '') or (FActive <> ppkTrue) or (FContext.Family <> ppcFPC) then
      begin
        Problem(ptiCompilerSettings, AOccurrence, AToken.Span,
          'This settings-stack effect needs an unconditional captured FPC context');
        UnknownEffect;
        Exit;
      end;

      if LName = 'push' then
      begin

        if Length(FCompilerFrames) >= FOptions.MaximumIncludeDepth then
        begin
          raise ENyxPascalTranslationLimit.Create('Compiler settings stack exceeds its depth budget');
        end;
        LSaved := TSavedCompilerSettings.Create;
        LSavedLease := LSaved;
        LSaved.Saved := FContext;
        LSaved.Site.Occurrence := AOccurrence;
        LSaved.Site.Span := AToken.Span;
        SetLength(FCompilerFrames, Length(FCompilerFrames) + 1);
        FCompilerFrames[High(FCompilerFrames)] := LSavedLease;
      end
      else
      begin

        if Length(FCompilerFrames) = 0 then
        begin
          Problem(ptiCompilerSettings, AOccurrence, AToken.Span,
            'No saved settings frame was captured for this POP');
          UnknownEffect;
          Exit;
        end;
        FContext := RestoreNyxPascalLocalSettings(FContext,
          FCompilerFrames[High(FCompilerFrames)].Context);
        SetLength(FCompilerFrames, Length(FCompilerFrames) - 1);
      end;
      SaveState;
      Exit;
    end;

    if LName = 'checkpointer' then
    begin

      if FContext.Family <> ppcFPC then
      begin
        Problem(ptiUnsupportedDirective, AOccurrence, AToken.Span,
          'This pointer-check request needs a captured FPC source context');
        UnknownEffect;
        Exit;
      end;
      { The inspected scanner accepts ON/OFF/DEFAULT and + / - / * spellings.
        Keep DEFAULT symbolic: its initial value and target availability are not
        facts that an authored request can provide to this source analyzer. }
      LArgument := LayoutDirectiveValue(LValue);

      if (LArgument = 'on') or (LArgument = '+') then
      begin
        LPointerChecking := ppcrOn;
      end
      else if (LArgument = 'off') or (LArgument = '-') then
      begin
        LPointerChecking := ppcrOff;
      end
      else if (LArgument = 'default') or (LArgument = '*') then
      begin
        LPointerChecking := ppcrDefault;
      end
      else
      begin
        raise ENyxPascalContext.Create('Pointer checking needs on, off or default');
      end;

      if FActive = ppkUnknown then
      begin
        LPointerChecking := ppcrUnknown;
      end;
      FContext := NyxPascalWithCompilerPolicies(FContext,
        NyxPascalCompilerPolicies(FContext).RequestPointerChecking(LPointerChecking));
      SaveState;
      Exit;
    end;

    if LName = 'interfaces' then
    begin
      LArgument := NyxPascalASCIIKey(LValue);

      if LArgument = 'com' then
      begin
        LInterfaces := pifsCOM;
      end
      else if LArgument = 'corba' then
      begin
        LInterfaces := pifsCORBA;
      end
      else if LArgument = 'default' then
      begin
        { DEFAULT means this capture's initial setting, not the analyzer's
          compiler or an assumption based on the current source MODE. }
        LInterfaces := NyxPascalCompilerPolicies(FInitial).InterfaceStyle;
      end
      else
      begin
        raise ENyxPascalContext.Create('Interface style needs COM, CORBA or DEFAULT');
      end;

      if (FActive <> ppkTrue) or FBlocked then LInterfaces := pifsUnknown;
      FContext := NyxPascalWithCompilerPolicies(FContext,
        NyxPascalCompilerPolicies(FContext).UseInterfaces(LInterfaces));
      SaveState;
      Exit;
    end;

    if (LName = 'inline') or LocalSwitchNamed(LName, LSwitch) then
    begin
      LArgument := NyxPascalASCIIKey(LValue);

      if LArgument = 'on' then
      begin
        LCondition := ppkTrue;
      end
      else if LArgument = 'off' then
      begin
        LCondition := ppkFalse;
      end
      else
      begin
        raise ENyxPascalContext.Create('A Boolean compiler policy needs on or off');
      end;

      if FActive = ppkUnknown then
      begin
        LCondition := ppkUnknown;
      end;

      if LName = 'inline' then
      begin
        FContext := NyxPascalWithCompilerPolicies(FContext,
          NyxPascalCompilerPolicies(FContext).WithInlining(LCondition));
        SaveState;
      end
      else
      begin
        ChangeOption(NyxPascalLocalSwitchOption(LSwitch), LCondition);
      end;
      Exit;
    end;

    if (LName = 'packrecords') or (LName = 'align') or
      ((LName = 'a') and not (LValue = '+') and not (LValue = '-')) then
    begin
      LAlignment := RecordAlignmentNamed(LayoutDirectiveValue(LValue));

      if (LAlignment = praUnknown) or
        ((LName <> 'packrecords') and (LAlignment in [praC, praDefault])) then
      begin
        raise ENyxPascalContext.Create('This record alignment needs an explicit supported value');
      end;

      if FActive = ppkUnknown then
      begin
        LAlignment := praUnknown;
      end;
      LPolicies := NyxPascalCompilerPolicies(FContext).AlignRecords(LAlignment);
      FContext := NyxPascalWithCompilerPolicies(FContext, LPolicies);
      SaveState;
      Exit;
    end;

    if (LName = 'packenum') or (LName = 'minenumsize') or (LName = 'z') then
    begin
      LArgument := LayoutDirectiveValue(LValue);

      if (LName = 'z') and (LArgument = '+') then
      begin
        LArgument := '4';
      end
      else if (LName = 'z') and (LArgument = '-') then
      begin
        LArgument := '1';
      end;
      LStorage := EnumStorageNamed(LArgument);

      if LStorage = pesUnknown then
      begin
        raise ENyxPascalContext.Create('This enumeration storage policy needs a supported value');
      end;

      if FActive = ppkUnknown then
      begin
        LStorage := pesUnknown;
      end;
      FContext := NyxPascalWithCompilerPolicies(FContext,
        NyxPascalCompilerPolicies(FContext).StoreEnums(LStorage));
      SaveState;
      Exit;
    end;

    if LName = 'packset' then
    begin
      LSetPacking := SetPackingNamed(LayoutDirectiveValue(LValue));

      if LSetPacking = pspUnknown then
      begin
        raise ENyxPascalContext.Create('This set packing needs a supported explicit value');
      end;

      if FActive = ppkUnknown then
      begin
        LSetPacking := pspUnknown;
      end;
      FContext := NyxPascalWithCompilerPolicies(FContext,
        NyxPascalCompilerPolicies(FContext).PackSets(LSetPacking));
      SaveState;
      Exit;
    end;

    if (LName = 'smartlink') or (LName = 'goto') then
    begin
      LArgument := NyxPascalASCIIKey(LValue);
      LCondition := ppkUnknown;

      if LArgument = 'on' then
      begin
        LCondition := ppkTrue;
      end
      else if LArgument = 'off' then
      begin
        LCondition := ppkFalse;
      end
      else
      begin
        raise ENyxPascalContext.Create('A module switch needs on or off');
      end;

      if FActive = ppkUnknown then
      begin
        LCondition := ppkUnknown;
      end;

      if LName = 'goto' then
      begin
        FContext := NyxPascalWithCompilerPolicies(FContext,
          NyxPascalCompilerPolicies(FContext).RequestGoto(LCondition));
      end
      else
      begin
        FContext := NyxPascalWithCompilerPolicies(FContext,
          NyxPascalCompilerPolicies(FContext).RequestSmartLinking(LCondition));
      end;
      SaveState;
      Exit;
    end;

    if LName = 'calling' then
    begin
      LCalling := CallingNamed(NyxPascalASCIIKey(LValue));

      if LCalling = pccUnknown then
      begin
        raise ENyxPascalContext.Create('This calling convention needs a supported declared value');
      end;

      if FActive = ppkUnknown then
      begin
        LCalling := pccUnknown;
      end;
      FContext := NyxPascalWithCompilerPolicies(FContext,
        NyxPascalCompilerPolicies(FContext).CallBy(LCalling));
      SaveState;
      Exit;
    end;

    if (LName = 'define') or (LName = 'undef') then
    begin
      LSeparator := Pos(':=', LValue);
      LArgument := LValue;

      if (LName = 'define') and (LSeparator > 0) then
      begin
        LArgument := Trim(Copy(LValue, 1, LSeparator - 1));
        LDefinition := Default(TNyxPascalMacroDefinition);
        LDefinition.Name := NyxPascalDefine(LArgument);
        LDefinition.Replacement := Copy(LValue, LSeparator + 2, Length(LValue));
        LDefinition.Occurrence := AOccurrence;
        LDefinition.Span := NyxPascalSpan(AToken.Span.Start + LOffset +
          NyxTextScalarCount(Copy(LValue, 1, LSeparator + 1)),
          NyxTextScalarCount(LDefinition.Replacement));
        LDefinition.Known := (FActive = ppkTrue) and
          (FContext.Feature(ppfReplacementMacros) = ppkTrue);
        FMacros := FMacros.WithDefinition(LDefinition);
        ChangeSymbol(LArgument, ppdDefined);
      end
      else if LName = 'undef' then
      begin

        if FActive = ppkTrue then
        begin
          FMacros := FMacros.Without(NyxPascalDefine(LValue));
        end
        else
        begin
          LDefinition := Default(TNyxPascalMacroDefinition);
          LDefinition.Name := NyxPascalDefine(LValue);
          LDefinition.Occurrence := AOccurrence;
          LDefinition.Span := NyxPascalSpan(AToken.Span.Start, 0);
          FMacros := FMacros.WithDefinition(LDefinition);
        end;
        ChangeSymbol(LValue, ppdUndefined);
      end
      else
      begin

        if FMacros.Has(NyxPascalDefine(LValue)) then
        begin
          LDefinition := Default(TNyxPascalMacroDefinition);
          LDefinition.Name := NyxPascalDefine(LValue);
          LDefinition.Occurrence := AOccurrence;
          LDefinition.Span := NyxPascalSpan(AToken.Span.Start, 0);
          LDefinition.Known := FActive = ppkTrue;
          FMacros := FMacros.WithDefinition(LDefinition);
        end;
        ChangeSymbol(LValue, ppdDefined);
      end;
      Exit;
    end;

    if LName = 'macro' then
    begin
      LArgument := NyxPascalASCIIKey(LValue);

      if LArgument = 'on' then
      begin
        LCondition := ppkTrue;
      end
      else if LArgument = 'off' then
      begin
        LCondition := ppkFalse;
      end
      else
      begin
        raise ENyxPascalContext.Create('MACRO needs on or off');
      end;

      if FActive = ppkUnknown then
      begin
        LCondition := ppkUnknown;
      end;
      FContext := FContext.WithFeature(ppfReplacementMacros, LCondition);
      SaveState;
      Exit;
    end;

    if (Length(LName) = 1) and (Length(LValue) > 0) and
      (LValue[1] in ['+', '-']) and not
      ((LName = 'a') and ((LValue = '+') or (LValue = '-'))) then
    begin
      ChangeOptions(LName + LValue);
      Exit;
    end;

    if (LName = 'include') or ((LName = 'i') and not ((LValue = '+') or (LValue = '-'))) then
    begin
      LArgument := LValue;

      if (Length(LArgument) >= 2) and (LArgument[1] = #39) and
        (LArgument[Length(LArgument)] = #39) then
      begin
        LArgument := Copy(LArgument, 2, Length(LArgument) - 2);
      end;

      if (Pos('%', LArgument) > 0) or (Pos('*', LArgument) > 0) then
      begin
        Problem(ptiUnsupportedDirective, AOccurrence, AToken.Span,
          'Compiler-info substitutions and wildcard includes need explicit capture');
        UnknownEffect;
        Exit;
      end;
      LAt := FFiles.ResolveInclude(FFiles.FileAt(FOccurrences[AOccurrence].FileIndex), LArgument);

      if LAt < 0 then
      begin
        Problem(ptiMissingInclude, AOccurrence, AToken.Span,
          'This include was not present in the captured source search context');
        UnknownEffect;
      end
      else
      begin
        Visit(LAt, AOccurrence, AToken.Span);
      end;
      Exit;
    end;

    if (LName = 'a') and ((LValue = '+') or (LValue = '-')) then
    begin
      Problem(ptiCompilerSettings, AOccurrence, AToken.Span,
        'Boolean alignment aliases need their mode-specific policy; use an explicit captured alignment');
      UnknownEffect;
      Exit;
    end;

    if LName = 'mode' then
    begin
      LMode := ModeNamed(LValue);

      if FModeSeen then
      begin
        Problem(ptiMalformedDirective, AOccurrence, AToken.Span,
          'Only one source MODE directive is supported per compiler module');
        UnknownEffect;
        Exit;
      end;
      FModeSeen := True;

      if (FActive <> ppkTrue) or
        not ((LMode in [ppmDelphi, ppmTP]) or
          ((FContext.Family = ppcFPC) and (LMode in [ppmFPC, ppmObjFPC, ppmDelphiUnicode]))) then
      begin
        Problem(ptiLexicalPolicyChange, AOccurrence, AToken.Span,
          'This conditional or unsupported mode needs verified lexical defaults');
        FBlocked := True;
        UnknownEffect;
      end
      else
      begin
        FContext := FContext.InMode(LMode);
        FNested := LMode in [ppmFPC, ppmObjFPC];
        FContext := FContext.WithFeature(ppfNestedComments, ppkFalse);

        if FNested then
        begin
          FContext := FContext.WithFeature(ppfNestedComments, ppkTrue);
        end;

        if FContext.Family = ppcFPC then
        begin
          { Only independently qualified mode facts are restored after InMode
            revokes options/features. Version-sensitive callback/helper defaults
            remain unknown until explicitly captured or switched in source. }
          FContext := FContext.WithOption('H', ppkFalse)
            .WithFeature(ppfUnicodeStrings, ppkFalse)
            .WithFeature(ppfOutParameters, ppkFalse)
            .WithFeature(ppfClasses, ppkFalse);

          if LMode in [ppmDelphi, ppmDelphiUnicode, ppmTP, ppmMacPas, ppmISO,
            ppmExtendedPascal] then
          begin
            { FPC mode handling enables this module switch; later FPC/ObjFPC
              mode changes preserve it rather than restoring a local default. }
            FContext := NyxPascalWithCompilerPolicies(FContext,
              NyxPascalCompilerPolicies(FContext).RequestGoto(ppkTrue));
          end;

          if LMode in [ppmDelphi, ppmDelphiUnicode] then
          begin
            FContext := FContext.WithOption('H', ppkTrue);
          end;

          if LMode = ppmDelphiUnicode then
          begin
            FContext := FContext.WithFeature(ppfUnicodeStrings, ppkTrue);
          end;

          if LMode in [ppmDelphi, ppmDelphiUnicode, ppmObjFPC] then
          begin
            FContext := FContext.WithFeature(ppfOutParameters, ppkTrue)
              .WithFeature(ppfClasses, ppkTrue);
          end;
          FContext := FContext.WithSymbol(NyxPascalDefine('FPC_DELPHI'), ppdUndefined)
            .WithSymbol(NyxPascalDefine('FPC_OBJFPC'), ppdUndefined)
            .WithSymbol(NyxPascalDefine('FPC_TP'), ppdUndefined)
            .WithSymbol(NyxPascalDefine('FPC_MACPAS'), ppdUndefined)
            .WithSymbol(NyxPascalDefine('FPC_ISO'), ppdUndefined)
            .WithSymbol(NyxPascalDefine('FPC_EXTENDEDPASCAL'), ppdUndefined);

          if LMode in [ppmDelphi, ppmDelphiUnicode] then
          begin
            FContext := FContext.WithSymbol(NyxPascalDefine('FPC_DELPHI'), ppdDefined);
          end
          else if LMode = ppmTP then
          begin
            FContext := FContext.WithSymbol(NyxPascalDefine('FPC_TP'), ppdDefined);
          end
          else if LMode = ppmObjFPC then
          begin
            FContext := FContext.WithSymbol(NyxPascalDefine('FPC_OBJFPC'), ppdDefined);
          end;
        end;
        SaveState;
      end;
      Exit;
    end;

    if (LName = 'modeswitch') or (LName = 'nestedcomments') or (LName = 'scopedenums') then
    begin
      LArgument := NyxPascalASCIIKey(LValue);
      LCondition := ppkTrue;

      if LName <> 'modeswitch' then
      begin

        if LArgument = 'off' then
        begin
          LCondition := ppkFalse;
        end
        else if LArgument <> 'on' then
        begin
          raise ENyxPascalContext.Create('A Boolean directive needs on or off');
        end;
        LArgument := LName;
      end
      else if (Length(LArgument) > 0) and
        (LArgument[Length(LArgument)] in ['+', '-']) then
      begin

        if LArgument[Length(LArgument)] = '-' then
        begin
          LCondition := ppkFalse;
        end;
        Delete(LArgument, Length(LArgument), 1);
      end;

      if not FeatureNamed(LArgument, LFeature) then
      begin
        Problem(ptiUnsupportedDirective, AOccurrence, AToken.Span,
          'This language feature needs a captured typed policy');
        UnknownEffect;
        Exit;
      end;

      if FActive = ppkUnknown then
      begin
        LCondition := ppkUnknown;
      end;

      if (LFeature = ppfNestedComments) and
        (LCondition = ppkUnknown) then
      begin
        Problem(ptiLexicalPolicyChange, AOccurrence, AToken.Span,
          'Conditional comment termination is unknown; later directives cannot be interpreted');
        UnknownEffect;
        FBlocked := True;
      end
      else
      begin
        FContext := FContext.WithFeature(LFeature, LCondition);

        if LFeature = ppfNestedComments then
        begin
          FNested := LCondition = ppkTrue;
        end;
        SaveState;
      end;
      Exit;
    end;

    if (LName = 'codepage') and (NyxPascalASCIIKey(LValue) = 'utf8') then
    begin

      if NyxPascalSourceViewAt(FFiles, FOccurrences[AOccurrence].FileIndex).Kind <> psekUTF8 then
      begin
        Problem(ptiSourceEncoding, AOccurrence, AToken.Span,
          'The source CODEPAGE directive disagrees with its explicit decoded byte view');
        UnknownEffect;
      end;
      Exit;
    end;
    Problem(ptiUnsupportedDirective, AOccurrence, AToken.Span,
      'This directive needs explicit compiler-context support');
    UnknownEffect;
  except
    on LError: ENyxPascalMacroLimit do
    begin
      raise;
    end;
    on LError: ENyxPascalTranslationLimit do
    begin
      raise;
    end;
    on LError: ENyxPascalLexicalLimit do
    begin
      raise;
    end;
    on LError: ENyxPascalContext do
    begin
      Problem(ptiMalformedDirective, AOccurrence, AToken.Span,
        'This directive cannot be interpreted in its captured context');
      UnknownEffect;
      FActive := ppkUnknown;
    end;
  end;
end;

function TTranslation.Files: INyxPascalFileCatalog;
begin
  Result := FFiles;
end;

function TTranslation.InitialContext: INyxPascalCompilerContext;
begin
  Result := FInitial;
end;

function TTranslation.Resolved: Boolean;
begin
  Result := FResolved;
end;

function TTranslation.Count: Integer;
begin
  Result := FTokenCount;
end;

function TTranslation.Token(AIndex: Integer): TNyxPascalTranslatedToken;
begin

  if (AIndex < 0) or (AIndex >= FTokenCount) then
  begin
    raise ENyxPascalContext.Create('Translated token index is outside its snapshot');
  end;

  Result := FTokenBlocks[AIndex div CTranslationPrefixBlockSize].Token(
    AIndex mod CTranslationPrefixBlockSize);
end;

function TTranslation.PhysicalToken(AIndex: Integer): TNyxPascalToken;
var
  LToken: TNyxPascalTranslatedToken;
begin
  LToken := Token(AIndex);

  if (LToken.Occurrence < Length(FWindows)) and (FWindows[LToken.Occurrence] <> nil) then
  begin
    Exit(FWindows[LToken.Occurrence].Token(LToken.PhysicalToken));
  end;
  Result := OccurrenceLexical(LToken.Occurrence).Token(LToken.PhysicalToken);
end;

function TTranslation.PreparedToken(AIndex: Integer): TNyxPascalPreparedToken;
var
  LToken: TNyxPascalTranslatedToken;
begin
  LToken := Token(AIndex);

  if LToken.Expanded then
  begin
    Exit(NyxPascalPreparedMacroToken(Expansion(LToken.Expansion), LToken.ExpansionToken));
  end;

  if (LToken.Occurrence < Length(FWindows)) and (FWindows[LToken.Occurrence] <> nil) then
  begin
    Exit(NyxPascalPreparedToken(FWindows[LToken.Occurrence], LToken.PhysicalToken));
  end;
  Result := NyxPascalPreparedToken(OccurrenceLexical(LToken.Occurrence), LToken.PhysicalToken);
end;

function TTranslation.Text(AIndex: Integer): TNyxText;
var
  LToken: TNyxPascalTranslatedToken;
begin
  LToken := Token(AIndex);

  if (LToken.Occurrence < Length(FWindows)) and (FWindows[LToken.Occurrence] <> nil) then
  begin
    Exit(FWindows[LToken.Occurrence].Text(LToken.PhysicalToken));
  end;
  Result := OccurrenceLexical(LToken.Occurrence).Text(LToken.PhysicalToken);
end;

function TTranslation.Expansion(AIndex: Integer): INyxPascalMacroExpansion;
begin

  if (AIndex < 0) or (AIndex >= Length(FExpansions)) then
  begin
    raise ENyxPascalContext.Create('Replacement invocation is outside its translation');
  end;
  Result := FExpansions[AIndex];
end;

function TTranslation.StateMacros(AIndex: Integer): INyxPascalMacroEnvironment;
begin
  Result := CompilerState(AIndex).Macros;
end;

function TTranslation.SettingsDepth(AState: Integer): Integer;
begin

  if not SettingsKnown(AState) then
  begin
    raise ENyxPascalContext.Create('This compiler settings stack has unknown textual effects');
  end;
  Result := CompilerState(AState).SettingsDepth;
end;

function TTranslation.SettingsKnown(AState: Integer): Boolean;
begin
  Result := CompilerState(AState).SettingsKnown;
end;

function TTranslation.SavedSettings(AState, ALevel: Integer): INyxPascalCompilerContext;
begin

  if (ALevel < 0) or (ALevel >= SettingsDepth(AState)) then
  begin
    raise ENyxPascalContext.Create('Saved local settings are outside this historical stack');
  end;
  Result := CompilerState(AState).SavedFrame(ALevel).Context;
end;

function TTranslation.SettingsOrigin(AState, ALevel: Integer): TNyxPascalSettingsOrigin;
begin
  SavedSettings(AState, ALevel);
  Result := CompilerState(AState).SavedFrame(ALevel).Origin;
end;

function NyxPascalEffectiveToken(const ASource: INyxPascalTranslation;
  AIndex: Integer): TNyxPascalToken;
var
  LMacros: INyxPascalTranslationMacros;
  LToken: TNyxPascalTranslatedToken;
begin

  if ASource = nil then
  begin
    raise ENyxPascalContext.Create('Effective tokens need their owning translation');
  end;
  LToken := ASource.Token(AIndex);

  if LToken.Expanded and Supports(ASource, INyxPascalTranslationMacros, LMacros) then
  begin
    Result := LMacros.Expansion(LToken.Expansion).Token(LToken.ExpansionToken);
  end
  else
  begin
    Result := ASource.PhysicalToken(AIndex);
  end;
end;

function NyxPascalPreparedEffectiveToken(const ASource: INyxPascalTranslation;
  AIndex: Integer): TNyxPascalPreparedToken;
var
  LOwner: IPreparedTranslation;
begin
  Result := Default(TNyxPascalPreparedToken);

  if Supports(ASource, IPreparedTranslation, LOwner) then
  begin
    Result := LOwner.PreparedToken(AIndex);
  end;
end;

function NyxPascalEffectiveText(const ASource: INyxPascalTranslation;
  AIndex: Integer): TNyxText;
var
  LMacros: INyxPascalTranslationMacros;
  LToken: TNyxPascalTranslatedToken;
begin

  if ASource = nil then
  begin
    raise ENyxPascalContext.Create('Effective text needs its owning translation');
  end;
  LToken := ASource.Token(AIndex);

  if LToken.Expanded and Supports(ASource, INyxPascalTranslationMacros, LMacros) then
  begin
    Result := LMacros.Expansion(LToken.Expansion).Text(LToken.ExpansionToken);
  end
  else
  begin
    Result := ASource.Text(AIndex);
  end;
end;

function TTranslation.Physical(AFile: Integer): INyxPascalLexicalSnapshot;
begin
  FFiles.FileAt(AFile);

  if FPhysical[AFile] = nil then
  begin
    raise ENyxPascalContext.Create('This captured file has no translation occurrence');
  end;

  if FPhysical[AFile].Ambiguous then
  begin
    raise ENyxPascalContext.Create('This file has differing lexical contexts; select its exact occurrence');
  end;
  Result := FPhysical[AFile].Lexical;
end;

function TTranslation.OccurrenceLexical(AOccurrence: Integer): INyxPascalLexicalSnapshot;
begin
  Occurrence(AOccurrence);
  Result := FLexical[AOccurrence];

  if Result = nil then
  begin
    raise ENyxPascalContext.Create('This frozen prefix has no completed lexical view for its active reader');
  end;
end;

function NyxPascalOccurrenceLexical(const ATranslation: INyxPascalTranslation;
  AOccurrence: Integer): INyxPascalLexicalSnapshot;
var
  LContexts: INyxPascalTranslationLexicalContexts;
begin

  if ATranslation = nil then
  begin
    raise ENyxPascalContext.Create('A lexical occurrence needs its owning translation');
  end;

  if Supports(ATranslation, INyxPascalTranslationLexicalContexts, LContexts) then
  begin
    Result := LContexts.OccurrenceLexical(AOccurrence);
  end
  else
  begin
    Result := ATranslation.Physical(ATranslation.Occurrence(AOccurrence).FileIndex);
  end;
end;

function NyxPascalOriginalSpan(const ATranslation: INyxPascalTranslation;
  AOccurrence: Integer; const ASpan: TNyxPascalSpan): TNyxPascalByteSpan;
var
  LFile: Integer;
  LView: INyxPascalSourceView;
begin

  if ATranslation = nil then
  begin
    raise ENyxPascalContext.Create('Original coordinates need their owning translation');
  end;
  LFile := ATranslation.Occurrence(AOccurrence).FileIndex;
  LView := NyxPascalSourceViewAt(ATranslation.Files, LFile);

  if LView = nil then
  begin
    raise ENyxPascalSourceEncoding.CreateForFile(ATranslation.Files.FileAt(LFile));
  end;
  Result := LView.OriginalSpan(ASpan);
end;

function TTranslation.OccurrenceCount: Integer;
begin
  Result := FOccurrenceCount;
end;

function TTranslation.Occurrence(AIndex: Integer): TNyxPascalInclusion;
begin

  if (AIndex < 0) or (AIndex >= FOccurrenceCount) then
  begin
    raise ENyxPascalContext.Create('Include occurrence index is outside its translation');
  end;
  Result := FOccurrences[AIndex];
end;

function TTranslation.StateCount: Integer;
begin
  Result := FStateCount;
end;

function TTranslation.State(AIndex: Integer): INyxPascalCompilerContext;
begin

  if (AIndex < 0) or (AIndex >= FStateCount) then
  begin
    raise ENyxPascalContext.Create('Compiler-state index is outside its translation');
  end;
  Result := CompilerState(AIndex).Context;
end;

function TTranslation.DiagnosticCount: Integer;
begin
  Result := FDiagnosticCount;
end;

function TTranslation.Diagnostic(AIndex: Integer): TNyxPascalTranslationDiagnostic;
begin

  if (AIndex < 0) or (AIndex >= FDiagnosticCount) then
  begin
    raise ENyxPascalContext.Create('Translation diagnostic index is outside its snapshot');
  end;
  Result := FDiagnostics[AIndex];
end;

function TTranslation.UnresolvedCount: Integer;
begin
  Result := FUnresolvedCount;
end;

function TTranslation.OptimizationDirectiveCount: Integer;
begin
  Result := FOptimizationCount;
end;

function TTranslation.OptimizationDirectiveAt(AIndex: Integer): TNyxPascalOptimizationDirective;
begin

  if (AIndex < 0) or (AIndex >= FOptimizationCount) then
  begin
    raise ENyxPascalContext.Create('Optimizer directive index is outside its translation');
  end;
  Result := FOptimizationDirectives[AIndex];
end;

function TTranslation.CodeAlignmentDirectiveCount: Integer;
begin
  Result := FCodeAlignmentCount;
end;

function TTranslation.CodeAlignmentDirectiveAt(AIndex: Integer): TNyxPascalCodeAlignmentDirective;
begin

  if (AIndex < 0) or (AIndex >= FCodeAlignmentCount) then
  begin
    raise ENyxPascalContext.Create('Code-alignment directive index is outside its translation');
  end;
  Result := FCodeAlignmentDirectives[AIndex];
end;

function TTranslation.EmissionDirectiveCount: Integer;
begin
  Result := FEmissionCount;
end;

function TTranslation.EmissionDirectiveAt(AIndex: Integer): TNyxPascalEmissionDirective;
begin

  if (AIndex < 0) or (AIndex >= FEmissionCount) then
  begin
    raise ENyxPascalContext.Create('Emission directive index is outside its translation');
  end;
  Result := FEmissionDirectives[AIndex];
end;

function TTranslation.WarningDirectiveCount: Integer;
begin
  Result := FWarningCount;
end;

function TTranslation.WarningDirectiveAt(AIndex: Integer): TNyxPascalWarningDirective;
begin

  if (AIndex < 0) or (AIndex >= FWarningCount) then
  begin
    raise ENyxPascalContext.Create('Warning directive index is outside its translation');
  end;
  Result := FWarningDirectives[AIndex];
end;

function TTranslation.RegisterDirectiveCount: Integer;
begin
  Result := FRegisterCount;
end;

function TTranslation.RegisterDirectiveAt(AIndex: Integer): TNyxPascalRegisterDirective;
begin

  if (AIndex < 0) or (AIndex >= FRegisterCount) then
  begin
    raise ENyxPascalContext.Create('Register directive index is outside its translation');
  end;
  Result := FRegisterDirectives[AIndex];
end;

function TTranslation.UnresolvedAt(AIndex: Integer): TNyxPascalUnresolvedCondition;
begin

  if (AIndex < 0) or (AIndex >= FUnresolvedCount) then
  begin
    raise ENyxPascalContext.Create('Unresolved condition index is outside its translation');
  end;
  Result := FUnresolved[AIndex];
end;

constructor TTranslationPrefix.Create(const ASource: INyxPascalTranslation;
  ABeforeToken: Integer; const AOptions: TNyxPascalTranslationOptions;
  const ACancellation: INyxCancellationScope);
var
  LCancellation: INyxCancellationScope;
  LSourceCount: Integer;
  LOccurrences: Integer;
  LStates: Integer;
  LDiagnostics: Integer;
  LLast: array of Integer;
  LAt: Integer;
  LUsed: Integer;
  LToken: TNyxPascalTranslatedToken;
  LDiagnostic: TNyxPascalTranslationDiagnostic;
  LSpan: TNyxPascalSpan;
  LWarning: TNyxPascalWarningDirective;
  LAlignment: TNyxPascalCodeAlignmentDirective;
  LOptimization: TNyxPascalOptimizationDirective;
  LEmissions: INyxPascalEmissionDirectives;
  LEmission: TNyxPascalEmissionDirective;
  LEmissionCount: Integer;
  LProof: IFrozenPrefixProof;
  LSummary: IPrefixTokenSummary;
  LLastOffset: Integer;

  procedure Check;
  begin

    if (LCancellation <> nil) and LCancellation.Cancelled then
    begin
      raise ENyxPascalAnalysisCancelled.Create('Translation prefix cancelled');
    end;
    { Arbitrary token scans and remaining directive/diagnostic rows remain real
      admitted work. Only an exact private frozen summary avoids token rescans. }
    NyxPascalAdmitAnalysis(LCancellation, papFrozenRow);
  end;

begin
  inherited Create;
  LCancellation := ACancellation;
  AOptions.Validate;
  Check;

  if ASource = nil then
  begin
    raise ENyxPascalContext.Create('A translation prefix needs its immutable source');
  end;
  LSourceCount := ASource.Count;
  LOccurrences := ASource.OccurrenceCount;
  LStates := ASource.StateCount;
  LDiagnostics := ASource.DiagnosticCount;

  if (LSourceCount < 1) or (LSourceCount > AOptions.MaximumTokens) or
    (ABeforeToken < 0) or (ABeforeToken >= LSourceCount) or
    (LOccurrences < 1) or (LOccurrences > AOptions.MaximumOccurrences) or
    (LStates < 1) or (LStates > AOptions.MaximumStates) or
    (LDiagnostics < 0) or (LDiagnostics > AOptions.MaximumDiagnostics) then
  begin
    raise ENyxPascalTranslationLimit.Create('Translation prefix input exceeds its explicit bounds');
  end;
  FSource := ASource;
  Supports(FSource, INyxPascalTranslationMacros, FMacros);
  Supports(FSource, INyxPascalTranslationSettings, FSettings);
  FCount := ABeforeToken;
  Supports(FSource, INyxPascalRegisterDirectives, FRegisterDirectives);
  Supports(FSource, INyxPascalWarningDirectives, FWarningDirectives);
  Supports(FSource, INyxPascalCodeAlignmentDirectives, FCodeAlignmentDirectives);
  Supports(FSource, INyxPascalOptimizationDirectives, FOptimizationDirectives);

  if Supports(FSource, INyxPascalEmissionDirectives, LEmissions) then
  begin
    LEmissionCount := LEmissions.EmissionDirectiveCount;

    if (LEmissionCount < 0) or (LEmissionCount > AOptions.MaximumTokens) then
    begin
      raise ENyxPascalTranslationLimit.Create('Emission prefix directives exceed their input budget');
    end;
    SetLength(FEmissionRows, LEmissionCount);
    LUsed := 0;
    for LAt := 0 to LEmissionCount - 1 do
    begin
      Check;
      LEmission := LEmissions.EmissionDirectiveAt(LAt);

      if (Ord(LEmission.Channel) < Ord(Low(TNyxPascalDiagnosticChannel))) or
        (Ord(LEmission.Channel) > Ord(High(TNyxPascalDiagnosticChannel))) or
        (Ord(LEmission.Activity) < Ord(Low(TNyxPascalKnowledge))) or
        (Ord(LEmission.Activity) > Ord(High(TNyxPascalKnowledge))) or
        (LEmission.Origin.PrefixCount < 0) or (LEmission.Origin.PrefixCount >= LSourceCount) or
        (LEmission.State < 0) or (LEmission.State >= LStates) or
        (LEmission.Origin.Occurrence < 0) or (LEmission.Origin.Occurrence >= LOccurrences) then
      begin
        raise ENyxPascalContext.Create('An emission receipt is outside its source or value domain');
      end;
      LToken := FSource.Token(LEmission.Origin.PrefixCount);
      LSpan := FSource.PhysicalToken(LEmission.Origin.PrefixCount).Span;

      if (LEmission.State <> LToken.State) or (LEmission.Origin.Occurrence <> LToken.Occurrence) or
        (LEmission.Origin.Span.Start <> LSpan.Start) or (LEmission.Origin.Span.Length <> LSpan.Length) or
        (LEmission.Origin.FileReference.Name <>
          FSource.Files.FileAt(FSource.Occurrence(LToken.Occurrence).FileIndex).Name) then
      begin
        raise ENyxPascalContext.Create('An emission receipt does not match its original source token');
      end;

      if LEmission.Origin.PrefixCount < FCount then
      begin
        FEmissionRows[LUsed] := LEmission;
        Inc(LUsed);
      end;
    end;
    SetLength(FEmissionRows, LUsed);
  end;

  if FOptimizationDirectives <> nil then
  begin

    if (FOptimizationDirectives.OptimizationDirectiveCount < 0) or
      (FOptimizationDirectives.OptimizationDirectiveCount > AOptions.MaximumTokens) then
    begin
      raise ENyxPascalTranslationLimit.Create('Optimizer prefix directives exceed their input budget');
    end;
    SetLength(FOptimizationRows, FOptimizationDirectives.OptimizationDirectiveCount);
    LUsed := 0;
    for LAt := 0 to FOptimizationDirectives.OptimizationDirectiveCount - 1 do
    begin
      Check;
      LOptimization := FOptimizationDirectives.OptimizationDirectiveAt(LAt);
      LOptimization.Rule.Request.Validate;

      if (LOptimization.Origin.PrefixCount < 0) or (LOptimization.Origin.PrefixCount >= LSourceCount) or
        (LOptimization.State < 0) or (LOptimization.State >= LStates) or
        (LOptimization.Origin.Occurrence < 0) or (LOptimization.Origin.Occurrence >= LOccurrences) then
      begin
        raise ENyxPascalContext.Create('An optimizer receipt is outside its owning source');
      end;

      if LOptimization.Origin.PrefixCount < FCount then
      begin
        FOptimizationRows[LUsed] := LAt;
        Inc(LUsed);
      end;
    end;
    SetLength(FOptimizationRows, LUsed);
  end;

  if FCodeAlignmentDirectives <> nil then
  begin

    if (FCodeAlignmentDirectives.CodeAlignmentDirectiveCount < 0) or
      (FCodeAlignmentDirectives.CodeAlignmentDirectiveCount > AOptions.MaximumTokens) then
    begin
      raise ENyxPascalTranslationLimit.Create('Alignment prefix directives exceed their input budget');
    end;
    SetLength(FCodeAlignmentRows, FCodeAlignmentDirectives.CodeAlignmentDirectiveCount);
    LUsed := 0;
    for LAt := 0 to FCodeAlignmentDirectives.CodeAlignmentDirectiveCount - 1 do
    begin
      Check;
      LAlignment := FCodeAlignmentDirectives.CodeAlignmentDirectiveAt(LAt);
      LAlignment.Rule.Request.Validate;

      if (LAlignment.Origin.PrefixCount < 0) or (LAlignment.Origin.PrefixCount >= LSourceCount) or
        (LAlignment.State < 0) or (LAlignment.State >= LStates) or
        (LAlignment.Origin.Occurrence < 0) or (LAlignment.Origin.Occurrence >= LOccurrences) then
      begin
        raise ENyxPascalContext.Create('An alignment receipt is outside its owning source');
      end;

      if LAlignment.Origin.PrefixCount < FCount then
      begin
        FCodeAlignmentRows[LUsed] := LAt;
        Inc(LUsed);
      end;
    end;
    SetLength(FCodeAlignmentRows, LUsed);
  end;

  if FWarningDirectives <> nil then
  begin

    if (FWarningDirectives.WarningDirectiveCount < 0) or
      (FWarningDirectives.WarningDirectiveCount > AOptions.MaximumTokens) then
    begin
      raise ENyxPascalTranslationLimit.Create('Warning prefix directives exceed their input budget');
    end;
    SetLength(FWarningRows, FWarningDirectives.WarningDirectiveCount);
    LUsed := 0;
    for LAt := 0 to FWarningDirectives.WarningDirectiveCount - 1 do
    begin
      Check;
      LWarning := FWarningDirectives.WarningDirectiveAt(LAt);

      if (LWarning.Origin.PrefixCount < 0) or (LWarning.Origin.PrefixCount >= LSourceCount) or
        (LWarning.State < 0) or (LWarning.State >= LStates) or
        (LWarning.Origin.Occurrence < 0) or (LWarning.Origin.Occurrence >= LOccurrences) then
      begin
        raise ENyxPascalContext.Create('A warning receipt is outside its owning source');
      end;

      if LWarning.Origin.PrefixCount < FCount then
      begin
        FWarningRows[LUsed] := LAt;
        Inc(LUsed);
      end;
    end;
    SetLength(FWarningRows, LUsed);
  end;

  if FRegisterDirectives <> nil then
  begin

    if (FRegisterDirectives.RegisterDirectiveCount < 0) or
      (FRegisterDirectives.RegisterDirectiveCount > AOptions.MaximumTokens) then
    begin
      raise ENyxPascalTranslationLimit.Create('Register prefix directives exceed their input budget');
    end;
    SetLength(FRegisterRows, FRegisterDirectives.RegisterDirectiveCount);
    LUsed := 0;
    for LAt := 0 to FRegisterDirectives.RegisterDirectiveCount - 1 do
    begin
      Check;

      if FRegisterDirectives.RegisterDirectiveAt(LAt).Origin.PrefixCount < FCount then
      begin
        FRegisterRows[LUsed] := LAt;
        Inc(LUsed);
      end;
    end;
    SetLength(FRegisterRows, LUsed);
  end;
  Supports(FSource, INyxPascalUnresolvedConditions, FUnresolved);

  if FUnresolved <> nil then
  begin

    if (FUnresolved.UnresolvedCount < 0) or
      (FUnresolved.UnresolvedCount > AOptions.MaximumDiagnostics) then
    begin
      raise ENyxPascalTranslationLimit.Create('Unresolved prefix conditions exceed their input budget');
    end;
    SetLength(FUnresolvedRows, FUnresolved.UnresolvedCount);
    LUsed := 0;
    for LAt := 0 to FUnresolved.UnresolvedCount - 1 do
    begin
      Check;

      if FUnresolved.UnresolvedAt(LAt).Origin.PrefixCount < FCount then
      begin
        FUnresolvedRows[LUsed] := LAt;
        Inc(LUsed);
      end;
    end;
    SetLength(FUnresolvedRows, LUsed);
  end;
  LToken := FSource.Token(ABeforeToken);

  if (LToken.State < 0) or (LToken.State >= LStates) or
    (LToken.Occurrence < 0) or (LToken.Occurrence >= LOccurrences) then
  begin
    raise ENyxPascalContext.Create('Translation prefix site has invalid ownership');
  end;
  FStates := LToken.State + 1;
  FOccurrences := LToken.Occurrence + 1;
  FExpansion := -1;
  FResolved := True;

  if Supports(FSource, IFrozenPrefixProof, LProof) then
  begin
    LSummary := LProof.PrefixSummary(FCount);
  end;

  if LSummary <> nil then
  begin
    Check;

    if (LSummary.Occurrences <> LOccurrences) or
      (LSummary.MaximumState >= FStates) or
      (LSummary.MaximumOccurrence >= LOccurrences) then
    begin
      raise ENyxPascalTranslationLimit.Create('A frozen token summary lost its bounded source authority');
    end;

    if LSummary.MaximumOccurrence >= FOccurrences then
    begin
      FOccurrences := LSummary.MaximumOccurrence + 1;
    end;
    FExpansion := LSummary.MaximumExpansion;
    FResolved := LSummary.Resolved;
    FPreparedTokens := LSummary.PreparedTokens;
    FRetainedTokens := LSummary.RetainedTokens;
    FReusedTokens := FCount;
  end
  else
  begin
    SetLength(LLast, LOccurrences);
    for LAt := 0 to LOccurrences - 1 do
    begin
      Check;
      LLast[LAt] := -1;
    end;
    for LAt := 0 to FCount - 1 do
    begin
      Check;
      LToken := FSource.Token(LAt);

      if (LToken.Occurrence < 0) or (LToken.Occurrence >= LOccurrences) or
        (LToken.State < 0) or (LToken.State >= FStates) then
      begin
        raise ENyxPascalContext.Create('Translation prefix tokens have invalid state/order');
      end;

      if LToken.Occurrence >= FOccurrences then
      begin
        FOccurrences := LToken.Occurrence + 1;
      end;
      LSpan := FSource.PhysicalToken(LAt).Span;

      if LSpan.Start > LLast[LToken.Occurrence] then
      begin
        LLast[LToken.Occurrence] := LSpan.Start;
      end;

      if LToken.Activity = ppkUnknown then
      begin
        FResolved := False;
      end;

      if LToken.Expanded and (LToken.Expansion > FExpansion) then
      begin
        FExpansion := LToken.Expansion;
      end;
      Inc(FPreparedTokens);
    end;
  end;
  SetLength(FDiagnostics, LDiagnostics);
  LUsed := 0;
  for LAt := 0 to LDiagnostics - 1 do
  begin
    Check;
    LDiagnostic := FSource.Diagnostic(LAt);

    if (LDiagnostic.Occurrence < 0) or (LDiagnostic.Occurrence >= LOccurrences) then
    begin
      raise ENyxPascalContext.Create('Translation diagnostic has invalid occurrence ownership');
    end;

    if LSummary <> nil then
    begin
      LLastOffset := LSummary.LastOffset(LDiagnostic.Occurrence);
    end
    else
    begin
      LLastOffset := LLast[LDiagnostic.Occurrence];
    end;

    if LDiagnostic.Span.Start <= LLastOffset then
    begin
      FResolved := False;
      FDiagnostics[LUsed] := LAt;
      Inc(LUsed);
    end;
  end;
  SetLength(FDiagnostics, LUsed);
  Check;
end;

function TTranslationPrefix.Files: INyxPascalFileCatalog;
begin
  Result := FSource.Files;
end;

function TTranslationPrefix.PreparedTokens: Integer;
begin
  Result := FPreparedTokens;
end;

function TTranslationPrefix.RetainedTokens: Integer;
begin
  Result := FRetainedTokens;
end;

function TTranslationPrefix.ReusedTokens: Integer;
begin
  Result := FReusedTokens;
end;

function TTranslationPrefix.InitialContext: INyxPascalCompilerContext;
begin
  Result := FSource.InitialContext;
end;

function TTranslationPrefix.Resolved: Boolean;
begin
  Result := FResolved;
end;

function TTranslationPrefix.Count: Integer;
begin
  Result := FCount;
end;

function TTranslationPrefix.Token(AIndex: Integer): TNyxPascalTranslatedToken;
begin

  if (AIndex < 0) or (AIndex >= FCount) then
  begin
    raise ENyxPascalContext.Create('A token lies outside this translation prefix');
  end;
  Result := FSource.Token(AIndex);
end;

function TTranslationPrefix.PhysicalToken(AIndex: Integer): TNyxPascalToken;
begin
  Token(AIndex);
  Result := FSource.PhysicalToken(AIndex);
end;

function TTranslationPrefix.PreparedToken(AIndex: Integer): TNyxPascalPreparedToken;
begin
  Token(AIndex);
  Result := NyxPascalPreparedEffectiveToken(FSource, AIndex);
end;

function TTranslationPrefix.Text(AIndex: Integer): TNyxText;
begin
  Token(AIndex);
  Result := FSource.Text(AIndex);
end;

function TTranslationPrefix.Physical(AFile: Integer): INyxPascalLexicalSnapshot;
begin
  Result := FSource.Physical(AFile);
end;

function TTranslationPrefix.OccurrenceLexical(AOccurrence: Integer): INyxPascalLexicalSnapshot;
begin
  Occurrence(AOccurrence);
  Result := NyxPascalOccurrenceLexical(FSource, AOccurrence);
end;

function TTranslationPrefix.OccurrenceCount: Integer;
begin
  Result := FOccurrences;
end;

function TTranslationPrefix.Occurrence(AIndex: Integer): TNyxPascalInclusion;
begin

  if (AIndex < 0) or (AIndex >= FOccurrences) then
  begin
    raise ENyxPascalContext.Create('An inclusion lies outside this translation prefix');
  end;
  Result := FSource.Occurrence(AIndex);

  if Result.FFirst > FCount then
  begin
    raise ENyxPascalContext.Create('A prefix inclusion starts beyond its token window');
  end;

  if Result.FCount > FCount - Result.FFirst then
  begin
    Result.FCount := FCount - Result.FFirst;
  end;

  if Result.FExit >= FStates then
  begin
    Result.FExit := FStates - 1;
  end;
end;

function TTranslationPrefix.StateCount: Integer;
begin
  Result := FStates;
end;

function TTranslationPrefix.State(AIndex: Integer): INyxPascalCompilerContext;
begin

  if (AIndex < 0) or (AIndex >= FStates) then
  begin
    raise ENyxPascalContext.Create('A compiler state lies outside this translation prefix');
  end;
  Result := FSource.State(AIndex);
end;

function TTranslationPrefix.DiagnosticCount: Integer;
begin
  Result := Length(FDiagnostics);
end;

function TTranslationPrefix.Diagnostic(AIndex: Integer): TNyxPascalTranslationDiagnostic;
begin

  if (AIndex < 0) or (AIndex >= DiagnosticCount) then
  begin
    raise ENyxPascalContext.Create('A diagnostic lies outside this translation prefix');
  end;
  Result := FSource.Diagnostic(FDiagnostics[AIndex]);
end;

function TTranslationPrefix.UnresolvedCount: Integer;
begin

  if FUnresolved = nil then
  begin
    raise ENyxPascalContext.Create('This alternate source has no retained condition planning inputs');
  end;
  Result := Length(FUnresolvedRows);
end;

function TTranslationPrefix.OptimizationDirectiveCount: Integer;
begin

  if FOptimizationDirectives = nil then
  begin
    raise ENyxPascalContext.Create('This alternate source has no retained optimizer directive inputs');
  end;
  Result := Length(FOptimizationRows);
end;

function TTranslationPrefix.OptimizationDirectiveAt(AIndex: Integer): TNyxPascalOptimizationDirective;
begin

  if (AIndex < 0) or (AIndex >= OptimizationDirectiveCount) then
  begin
    raise ENyxPascalContext.Create('Optimizer directive index is outside this prefix');
  end;
  Result := FOptimizationDirectives.OptimizationDirectiveAt(FOptimizationRows[AIndex]);
end;

function TTranslationPrefix.CodeAlignmentDirectiveCount: Integer;
begin

  if FCodeAlignmentDirectives = nil then
  begin
    raise ENyxPascalContext.Create('This alternate source has no retained alignment directive inputs');
  end;
  Result := Length(FCodeAlignmentRows);
end;

function TTranslationPrefix.CodeAlignmentDirectiveAt(AIndex: Integer): TNyxPascalCodeAlignmentDirective;
begin

  if (AIndex < 0) or (AIndex >= CodeAlignmentDirectiveCount) then
  begin
    raise ENyxPascalContext.Create('Code-alignment directive index is outside this prefix');
  end;
  Result := FCodeAlignmentDirectives.CodeAlignmentDirectiveAt(FCodeAlignmentRows[AIndex]);
end;

function TTranslationPrefix.EmissionDirectiveCount: Integer;
begin
  Result := Length(FEmissionRows);
end;

function TTranslationPrefix.EmissionDirectiveAt(AIndex: Integer): TNyxPascalEmissionDirective;
begin

  if (AIndex < 0) or (AIndex >= Length(FEmissionRows)) then
  begin
    raise ENyxPascalContext.Create('Emission directive index is outside its prefix');
  end;
  Result := FEmissionRows[AIndex];
end;

function TTranslationPrefix.WarningDirectiveCount: Integer;
begin

  if FWarningDirectives = nil then
  begin
    raise ENyxPascalContext.Create('This alternate source has no retained warning directive inputs');
  end;
  Result := Length(FWarningRows);
end;

function TTranslationPrefix.WarningDirectiveAt(AIndex: Integer): TNyxPascalWarningDirective;
begin

  if (AIndex < 0) or (AIndex >= WarningDirectiveCount) then
  begin
    raise ENyxPascalContext.Create('Warning directive index is outside this prefix');
  end;
  Result := FWarningDirectives.WarningDirectiveAt(FWarningRows[AIndex]);
end;

function TTranslationPrefix.RegisterDirectiveCount: Integer;
begin

  if FRegisterDirectives = nil then
  begin
    raise ENyxPascalContext.Create('This alternate source has no retained register directive inputs');
  end;
  Result := Length(FRegisterRows);
end;

function TTranslationPrefix.RegisterDirectiveAt(AIndex: Integer): TNyxPascalRegisterDirective;
begin

  if (AIndex < 0) or (AIndex >= RegisterDirectiveCount) then
  begin
    raise ENyxPascalContext.Create('Register directive index is outside this prefix');
  end;
  Result := FRegisterDirectives.RegisterDirectiveAt(FRegisterRows[AIndex]);
end;

function TTranslationPrefix.UnresolvedAt(AIndex: Integer): TNyxPascalUnresolvedCondition;
begin

  if (AIndex < 0) or (AIndex >= Length(FUnresolvedRows)) then
  begin
    raise ENyxPascalContext.Create('Unresolved condition index is outside its source prefix');
  end;
  Result := FUnresolved.UnresolvedAt(FUnresolvedRows[AIndex]);
end;

function TTranslationPrefix.Expansion(AIndex: Integer): INyxPascalMacroExpansion;
begin

  if (FMacros = nil) or (AIndex < 0) or (AIndex > FExpansion) then
  begin
    raise ENyxPascalContext.Create('A macro expansion lies outside this translation prefix');
  end;
  Result := FMacros.Expansion(AIndex);
end;

function TTranslationPrefix.StateMacros(AIndex: Integer): INyxPascalMacroEnvironment;
begin
  State(AIndex);

  if FMacros = nil then
  begin
    raise ENyxPascalContext.Create('This source has no captured macro state');
  end;
  Result := FMacros.StateMacros(AIndex);
end;

function TTranslationPrefix.SettingsKnown(AState: Integer): Boolean;
begin
  State(AState);
  Result := False;

  if FSettings <> nil then
  begin
    Result := FSettings.SettingsKnown(AState);
  end;
end;

function TTranslationPrefix.SettingsDepth(AState: Integer): Integer;
begin

  if not SettingsKnown(AState) then
  begin
    raise ENyxPascalContext.Create('This prefix has no known settings stack');
  end;
  Result := FSettings.SettingsDepth(AState);
end;

function TTranslationPrefix.SavedSettings(AState, ALevel: Integer): INyxPascalCompilerContext;
begin
  SettingsDepth(AState);
  Result := FSettings.SavedSettings(AState, ALevel);
end;

function TTranslationPrefix.SettingsOrigin(AState, ALevel: Integer): TNyxPascalSettingsOrigin;
begin
  SettingsDepth(AState);
  Result := FSettings.SettingsOrigin(AState, ALevel);
end;

function NewNyxPascalTranslationPrefix(const ASource: INyxPascalTranslation;
  ABeforeToken: Integer; const AOptions: TNyxPascalTranslationOptions;
  const ACancellation: INyxCancellationScope): INyxPascalTranslation;
var
  LStorage: INyxPascalPrefixTokenStorage;
begin

  if Supports(ASource, INyxPascalPrefixTokenStorage, LStorage) then
  begin
    Result := TStoredTranslationPrefix.Create(ASource, ABeforeToken, AOptions, ACancellation);
  end
  else
  begin
    Result := TTranslationPrefix.Create(ASource, ABeforeToken, AOptions, ACancellation);
  end;
end;

function RebindNyxPascalTranslation(const ASource: INyxPascalTranslation;
  const AFiles: INyxPascalFileCatalog;
  const ACancellation: INyxCancellationScope): INyxPascalTranslation;
var
  LSource: TTranslation;
  LFiles: INyxPascalFileCatalog;
  LSeen: array of Boolean;
  LAt, LFile, LByte, LScalar: Integer;
  LOldBytes, LNewBytes: INyxPascalSourceBytes;
  LOldView, LNewView: INyxPascalSourceView;
  LOldText, LNewText: TNyxText;
  LCandidate: INyxPascalTranslation;

  procedure Proof;
  begin

    if (ACancellation <> nil) and ACancellation.Cancelled then
      raise ENyxPascalAnalysisCancelled.Create('Translation input proof cancelled');
    NyxPascalAdmitAnalysis(ACancellation, papSourceProof);
  end;
begin
  Result := nil;
  Proof;

  if not (ASource is TTranslation) then Exit;
  LSource := ASource as TTranslation;

  if not LSource.FPublished or not LSource.FResolved or (LSource.FOccurrenceCount < 1) then Exit;
  LFiles := CaptureNyxPascalFiles(AFiles);
  Proof;

  if (LFiles.Count <> LSource.FFiles.Count) or
    (LFiles.CasePolicy <> LSource.FFiles.CasePolicy) or
    (LFiles.DirectoryCount <> LSource.FFiles.DirectoryCount) then Exit;
  for LAt := 0 to LFiles.Count - 1 do
  begin
    Proof;

    if LFiles.FileAt(LAt).Name <> LSource.FFiles.FileAt(LAt).Name then Exit;
  end;
  for LAt := 0 to LFiles.DirectoryCount - 1 do
  begin
    Proof;

    if LFiles.DirectoryAt(LAt) <> LSource.FFiles.DirectoryAt(LAt) then Exit;
  end;
  SetLength(LSeen, LFiles.Count);
  for LAt := 0 to LSource.FOccurrenceCount - 1 do
  begin
    Proof;
    LFile := LSource.FOccurrences[LAt].FileIndex;

    if LSeen[LFile] then Continue;
    LSeen[LFile] := True;
    LOldBytes := NyxPascalSourceBytesAt(LSource.FFiles, LFile);
    LNewBytes := NyxPascalSourceBytesAt(LFiles, LFile);
    LOldView := NyxPascalSourceViewAt(LSource.FFiles, LFile);
    LNewView := NyxPascalSourceViewAt(LFiles, LFile);
    Proof;

    if (LOldBytes = nil) or (LNewBytes = nil) or (LOldView = nil) or (LNewView = nil) then Exit;

    if LOldBytes <> LNewBytes then
    begin
      Proof;

      if (LOldBytes.Count <> LNewBytes.Count) or (LOldBytes.Origin <> LNewBytes.Origin) then Exit;
      for LByte := 0 to LOldBytes.Count - 1 do
      begin
        Proof;

        if LOldBytes.ByteAt(LByte) <> LNewBytes.ByteAt(LByte) then Exit;
      end;
    end;

    if LOldView <> LNewView then
    begin
      Proof;

      if (LOldView.Kind <> LNewView.Kind) or (LOldView.Encoding.Name <> LNewView.Encoding.Name) or
        (LOldView.Scalars <> LNewView.Scalars) then Exit;
      LOldText := LOldView.Text;
      LNewText := LNewView.Text;

      if Length(LOldText) <> Length(LNewText) then Exit;
      for LScalar := 1 to Length(LOldText) do
      begin
        Proof;

        if LOldText[LScalar] <> LNewText[LScalar] then Exit;
      end;
      for LScalar := 0 to LOldView.Scalars do
      begin
        Proof;

        if LOldView.ByteOffset(LScalar) <> LNewView.ByteOffset(LScalar) then Exit;
      end;
    end;
  end;
  LCandidate := TTranslation.CreateRebound(LSource, LFiles, ACancellation);
  Proof;
  Result := LCandidate;
end;

function NewNyxPascalTranslation(const AFiles: INyxPascalFileCatalog;
  const ARoot: TNyxPascalPhysicalFile; const AContext: INyxPascalCompilerContext;
  const AOptions: TNyxPascalTranslationOptions;
  const ACancellation: INyxCancellationScope;
  const AConditions: INyxPascalConditionalFacts): INyxPascalTranslation;
begin
  Result := TTranslation.Create(AFiles, ARoot, AContext, AOptions, ACancellation, AConditions);
end;

end.
