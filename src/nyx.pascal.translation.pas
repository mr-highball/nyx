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
  { Explicit captured-context gaps; none is disguised as an inactive branch or a
    successful compiler/type-resolution result. Positions are always physical. }
  TNyxPascalTranslationIssue = (ptiUnknownCondition, ptiMissingInclude,
    ptiIncludeCycle, ptiUnbalancedConditional, ptiUnsupportedDirective,
    ptiMalformedDirective, ptiLexicalIssue, ptiLexicalPolicyChange, ptiSourceEncoding,
    ptiMacroContext, ptiMacroCycle, ptiCompilerSettings);
  { Exact pre-condition source site. PrefixCount excludes this directive;
    occurrence distinguishes repeated includes. Records retain no interfaces. }
  TNyxPascalConditionOrigin = record
    FileReference: TNyxPascalPhysicalFile;
    Occurrence: Integer;
    Span: TNyxPascalSpan;
    PrefixCount: Integer;
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

{ Effective tokens may come from a replacement fragment; their Span is relative
  to that fragment. PhysicalToken/Text still describe the exact invocation.
  Callers retain the expansion trace for definition origins, never infer them
  from the length of the substituted spelling. Legacy translators pass through. }
function NyxPascalEffectiveToken(const ASource: INyxPascalTranslation;
  AIndex: Integer): TNyxPascalToken;
function NyxPascalEffectiveText(const ASource: INyxPascalTranslation;
  AIndex: Integer): TNyxText;

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

{ Immutable analysis prefix before an existing translation token. It invents no
  closing syntax or authored EOF token. State/occurrence exits stop at the site;
  full physical views remain exact for navigation. Known prefix participation
  and prior diagnostics determine Resolved independently of the unseen suffix.
  Input budgets/cancellation refuse atomically; no source/provider is mutated. }
function NewNyxPascalTranslationPrefix(const ASource: INyxPascalTranslation;
  ABeforeToken: Integer; const AOptions: TNyxPascalTranslationOptions;
  const ACancellation: INyxCancellationScope = nil): INyxPascalTranslation;

implementation

uses SysUtils, nyx.bytes, nyx.editing;

type
  { File-only compatibility view. The first immutable occurrence is retained;
    differing later policies/tokens revoke ambiguous legacy access. }
  TPhysicalCache = class
    Lexical: INyxPascalLexicalSnapshot;
    Ambiguous: Boolean;
  end;
  { Borrowed only by synchronous token visitors while construction is active. }
  TTranslation = class;
  { Owns one immutable completed source, bounded prefix ordinals and diagnostic
    indices. All outward references retain original files/contexts, never a
    constructing visitor, callback, editor or mutable source buffer. }
  TTranslationPrefix = class(TInterfacedObject, INyxPascalTranslation,
    INyxPascalTranslationLexicalContexts, INyxPascalTranslationMacros,
    INyxPascalTranslationSettings)
  private
    FSource: INyxPascalTranslation;
    FMacros: INyxPascalTranslationMacros;
    FSettings: INyxPascalTranslationSettings;
    FCount: Integer;
    FStates: Integer;
    FOccurrences: Integer;
    FExpansion: Integer;
    FResolved: Boolean;
    FDiagnostics: array of Integer;
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
  end;
  { One lexical construction's visitor. Owner is borrowed, never reference counted
    back to its translation; recursive includes get independent visitors. }
  TTranslationReader = class(TInterfacedObject, INyxPascalLexicalObserver)
  public
    Owner: TTranslation;
    Occurrence: Integer;
    Included: Boolean;
    procedure AfterToken(AIndex: Integer; const AToken: TNyxPascalToken;
      const AText: TNyxText; var AAllowNestedComments: Boolean);
  end;
  { One conditional frame may cross include boundaries. Taken is three-valued;
    an unknown branch's writes revoke facts rather than merging a guessed path. }
  TConditionalFrame = record
    Parent: TNyxPascalKnowledge;
    Taken: TNyxPascalKnowledge;
    HasElse: Boolean;
    Occurrence: Integer;
    Span: TNyxPascalSpan;
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
  { Each historical state owns its copied frame vector. Frame interfaces point
    outward to immutable snapshots, never back into translation or source owners. }
  TCompilerSettingsStack = class
  public
    Frames: TCompilerSettingsFrames;
    Known: Boolean;
    function SameStack(const AOther: TCompilerSettingsFrames; AKnown: Boolean): Boolean;
  end;
  { Owns every cache/immutable vector. Construction alone retains cancellation;
    all published references point outward to data, never back to a document. }
  TTranslation = class(TInterfacedObject, INyxPascalTranslation,
    INyxPascalTranslationLexicalContexts, INyxPascalTranslationMacros,
    INyxPascalTranslationSettings)
  private
    FFiles: INyxPascalFileCatalog;
    FInitial: INyxPascalCompilerContext;
    FContext: INyxPascalCompilerContext;
    FOptions: TNyxPascalTranslationOptions;
    FPhysical: array of TPhysicalCache;
    FLexical: array of INyxPascalLexicalSnapshot;
    FTokens: array of TNyxPascalTranslatedToken;
    FOccurrences: array of TNyxPascalInclusion;
    FStates: array of INyxPascalCompilerContext;
    FMacroStates: array of INyxPascalMacroEnvironment;
    FSettingsStates: array of TCompilerSettingsStack;
    FCompilerFrames: TCompilerSettingsFrames;
    FSettingsKnown: Boolean;
    FMacros: INyxPascalMacroEnvironment;
    FExpansions: array of INyxPascalMacroExpansion;
    FBoundaryViews: array of INyxPascalSourceView;
    FDiagnostics: array of TNyxPascalTranslationDiagnostic;
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
    FModeSeen: Boolean;
    FCancellation: INyxCancellationScope;
    FConditions: INyxPascalConditionalFacts;
    procedure CheckCancellation;
    procedure Problem(AIssue: TNyxPascalTranslationIssue; AOccurrence: Integer;
      const ASpan: TNyxPascalSpan; const AMessage: TNyxText);
    procedure SaveState;
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
    procedure UnknownEffect;
  public
    constructor Create(const AFiles: INyxPascalFileCatalog;
      const ARoot: TNyxPascalPhysicalFile; const AContext: INyxPascalCompilerContext;
      const AOptions: TNyxPascalTranslationOptions;
      const ACancellation: INyxCancellationScope;
      const AConditions: INyxPascalConditionalFacts);
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
  end;

function Capacity(ACount, AMaximum: Integer): Integer;
begin
  Result := AMaximum;

  if 32 + ACount div 2 <= AMaximum - ACount then
  begin
    Result := ACount + 32 + ACount div 2;
  end;
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
  AOptions.Validate;
  FOptions := AOptions;
  FCancellation := ACancellation;
  FConditions := AConditions;
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
  end;
end;

destructor TTranslation.Destroy;
var
  LAt: Integer;
begin
  for LAt := 0 to High(FPhysical) do
  begin
    FPhysical[LAt].Free;
  end;
  for LAt := 0 to High(FSettingsStates) do
  begin
    FSettingsStates[LAt].Free;
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

  if (FStateCount > 0) and FStates[FStateCount - 1].SameFacts(FContext) and
    FMacroStates[FStateCount - 1].SameMacros(FMacros) and
    FSettingsStates[FStateCount - 1].SameStack(FCompilerFrames, FSettingsKnown) then
  begin
    Exit;
  end;

  if FStateCount >= FOptions.MaximumStates then
  begin
    raise ENyxPascalTranslationLimit.Create('Pascal translation states exceed their budget');
  end;

  if FStateCount = Length(FStates) then
  begin
    SetLength(FStates, Capacity(FStateCount, FOptions.MaximumStates));
    SetLength(FMacroStates, Length(FStates));
    SetLength(FSettingsStates, Length(FStates));
  end;
  FStates[FStateCount] := FContext;
  FMacroStates[FStateCount] := FMacros;
  FSettingsStates[FStateCount] := TCompilerSettingsStack.Create;
  FSettingsStates[FStateCount].Frames := Copy(FCompilerFrames);
  FSettingsStates[FStateCount].Known := FSettingsKnown;
  Inc(FStateCount);
end;

function TSavedCompilerSettings.Context: INyxPascalCompilerContext;
begin
  Result := Saved;
end;

function TSavedCompilerSettings.Origin: TNyxPascalSettingsOrigin;
begin
  Result := Site;
end;

function TCompilerSettingsStack.SameStack(const AOther: TCompilerSettingsFrames; AKnown: Boolean): Boolean;
var
  LAt: Integer;
begin
  Result := False;

  if (Known <> AKnown) or (Length(Frames) <> Length(AOther)) then
  begin
    Exit;
  end;
  for LAt := 0 to High(Frames) do
  begin

    if Frames[LAt] <> AOther[LAt] then
    begin
      Exit;
    end;
  end;
  Result := True;
end;

procedure TTranslation.AddToken(AOccurrence, AToken: Integer; AExpansion: Integer;
  AExpansionToken: Integer);
begin
  CheckCancellation;

  if FTokenCount >= FOptions.MaximumTokens then
  begin
    raise ENyxPascalTranslationLimit.Create('Pascal translated tokens exceed their budget');
  end;

  if FTokenCount = Length(FTokens) then
  begin
    SetLength(FTokens, Capacity(FTokenCount, FOptions.MaximumTokens));
  end;
  FTokens[FTokenCount].FOccurrence := AOccurrence;
  FTokens[FTokenCount].FPhysicalToken := AToken;
  FTokens[FTokenCount].FState := FStateCount - 1;
  FTokens[FTokenCount].FActivity := FActive;
  FTokens[FTokenCount].FExpansion := AExpansion;
  FTokens[FTokenCount].FExpansionToken := AExpansionToken;
  FTokens[FTokenCount].FExpanded := AExpansion >= 0;

  if FBlocked then
  begin
    FTokens[FTokenCount].FActivity := ppkUnknown;
  end;
  Inc(FTokenCount);
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
  end;
  { Includes share the caller's compiler state. In particular a nested include
    may change the policy used for this reader's very next physical token. }
  AAllowNestedComments := Owner.FNested;
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
      FStates[FOccurrences[LAncestor].EntryState].SameFacts(FContext) then
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
    Dec(FDepth);
  end;
end;

procedure SplitDirective(const AText: TNyxText; out AName, AValue: TNyxText;
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
begin
  Result := ppkUnknown;
  try

    if (AKind = 'ifdef') or (AKind = 'ifndef') then
    begin
      LName := NyxPascalDefine(AValue);
      Result := FContext.Symbol(LName).IsDefined;

      if AKind = 'ifndef' then
      begin
        Result := NyxPascalNot(Result);
      end;
    end
    else if AKind = 'ifopt' then
    begin

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
      Result := AnalyzeNyxPascalCondition(FContext, LValue).Truth;

      if (Result = ppkUnknown) and (FConditions <> nil) then
      begin
        LOrigin.FileReference := FFiles.FileAt(FOccurrences[AOccurrence].FileIndex);
        LOrigin.Occurrence := AOccurrence;
        LOrigin.Span := ASpan;
        LOrigin.PrefixCount := FTokenCount - 1;
        Result := FConditions.Evaluate(LOrigin, FContext, LValue);

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
    end;
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
      'typehelpers', 'nestedcomments', 'scopedenums', 'unicodestrings', 'out', '');
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
  LCalling: TNyxPascalCallingConvention;
  LSwitch: TNyxPascalLocalSwitch;
  LSaved: TSavedCompilerSettings;
  LSavedLease: INyxSavedCompilerSettings;
begin
  SplitDirective(AText, LName, LValue, LOffset);
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
        FActive := LFrame.Parent;
        SetLength(FFrames, LAt);
      end
      else
      begin

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
      LAlignment := RecordAlignmentNamed(NyxPascalASCIIKey(LValue));

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
      LArgument := NyxPascalASCIIKey(LValue);

      if (LName = 'z') and (LValue = '+') then
      begin
        LArgument := '4';
      end
      else if (LName = 'z') and (LValue = '-') then
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

    if (Length(LName) = 1) and ((LValue = '+') or (LValue = '-')) then
    begin
      LCondition := ppkFalse;

      if LValue = '+' then
      begin
        LCondition := ppkTrue;
      end;
      ChangeOption(LName[1], LCondition);
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
            .WithFeature(ppfOutParameters, ppkFalse);

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
            FContext := FContext.WithFeature(ppfOutParameters, ppkTrue);
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
  Result := FTokens[AIndex];
end;

function TTranslation.PhysicalToken(AIndex: Integer): TNyxPascalToken;
var
  LToken: TNyxPascalTranslatedToken;
begin
  LToken := Token(AIndex);
  Result := OccurrenceLexical(LToken.Occurrence).Token(LToken.PhysicalToken);
end;

function TTranslation.Text(AIndex: Integer): TNyxText;
var
  LToken: TNyxPascalTranslatedToken;
begin
  LToken := Token(AIndex);
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
  State(AIndex);
  Result := FMacroStates[AIndex];
end;

function TTranslation.SettingsDepth(AState: Integer): Integer;
begin

  if not SettingsKnown(AState) then
  begin
    raise ENyxPascalContext.Create('This compiler settings stack has unknown textual effects');
  end;
  Result := Length(FSettingsStates[AState].Frames);
end;

function TTranslation.SettingsKnown(AState: Integer): Boolean;
begin
  State(AState);
  Result := FSettingsStates[AState].Known;
end;

function TTranslation.SavedSettings(AState, ALevel: Integer): INyxPascalCompilerContext;
begin

  if (ALevel < 0) or (ALevel >= SettingsDepth(AState)) then
  begin
    raise ENyxPascalContext.Create('Saved local settings are outside this historical stack');
  end;
  Result := FSettingsStates[AState].Frames[ALevel].Context;
end;

function TTranslation.SettingsOrigin(AState, ALevel: Integer): TNyxPascalSettingsOrigin;
begin
  SavedSettings(AState, ALevel);
  Result := FSettingsStates[AState].Frames[ALevel].Origin;
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
  Result := FStates[AIndex];
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

  procedure Check;
  begin

    if (LCancellation <> nil) and LCancellation.Cancelled then
    begin
      raise ENyxPascalAnalysisCancelled.Create('Translation prefix cancelled');
    end;
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
  SetLength(LLast, LOccurrences);
  for LAt := 0 to LOccurrences - 1 do
  begin
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

    if LDiagnostic.Span.Start <= LLast[LDiagnostic.Occurrence] then
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
begin
  Result := TTranslationPrefix.Create(ASource, ABeforeToken, AOptions, ACancellation);
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
