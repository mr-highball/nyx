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
unit nyx.pascal.completion.owned;
{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.scheduler, nyx.pascal.lexing, nyx.pascal.context,
  nyx.pascal.files, nyx.pascal.translation, nyx.pascal.syntax,
  nyx.pascal.binding, nyx.pascal.modules, nyx.pascal.sources, nyx.pascal.completion,
  nyx.pascal.encoding, nyx.pascal.conditions;

type
  { Copied retention policy for source-independent module analysis. The limits
    bound retained entries, translated tokens and catalog bytes (conservatively
    counted per entry, even when catalogs share storage). They do not enlarge
    any parsing/query budget. Zero modules disables reuse. No world, callback,
    editor or interface is stored in this value. }
  TNyxPascalSourceReuseOptions = record
  private
    FDefined: Boolean;
    FModules: Integer;
    FTokens: Integer;
    FBytes: Integer;
    FSemanticReplay: Boolean;
    FSyntaxRevision: Boolean;
    FLexicalReuse: Boolean;
  public
    { Defaults enable ordered semantic revalidation and retain at most 16 modules,
      one million tokens and 64 MiB of catalog input, including retained prefixes.
      The bounded directory supports at most 256 entries. }
    class function Defaults: TNyxPascalSourceReuseOptions; static;
    function UpToModules(AValue: Integer): TNyxPascalSourceReuseOptions;
    function UpToTokens(AValue: Integer): TNyxPascalSourceReuseOptions;
    function UpToCatalogBytes(AValue: Integer): TNyxPascalSourceReuseOptions;
    { Recheck retained ordered semantic answers against each request's world.
      False keeps world-dependent modules cold while independent reuse remains. }
    function WithSemanticReplay(AValue: Boolean): TNyxPascalSourceReuseOptions;
    { Rebuild current projection origins, then reuse only proved lexical/parser
      prefixes after edits. False retains ordinary cold projection construction. }
    function WithSyntaxRevision(AValue: Boolean): TNyxPascalSourceReuseOptions;
    { Donate previous physical rows during fresh translation of current inputs.
      Every current directive/conditional callback still runs. }
    function WithLexicalReuse(AValue: Boolean): TNyxPascalSourceReuseOptions;
    procedure Validate;
    property MaximumModules: Integer read FModules;
    property MaximumTokens: Integer read FTokens;
    property MaximumCatalogBytes: Integer read FBytes;
    property SemanticReplay: Boolean read FSemanticReplay;
    property SyntaxRevision: Boolean read FSyntaxRevision;
    property LexicalReuse: Boolean read FLexicalReuse;
  end;
  { Independent copied analysis budgets. Per-module translation/syntax and
    semantic query limits retain their existing contracts; AggregateTokens caps
    translated tokens across every project/SDK root in this one request.
    No target, path, compiler process or mutable producer is stored here. }
  TNyxOwnedPascalCompletionOptions = record
  private
    FTranslation: TNyxPascalTranslationOptions;
    FSyntax: TNyxPascalSyntaxOptions;
    FBinding: TNyxPascalBindingOptions;
    FConditions: TNyxPascalConditionalOptions;
    FReuse: TNyxPascalSourceReuseOptions;
    FTokens: Integer;
    FReceiverTokens: Integer;
    FDependencyLoads: Integer;
    FDefined: Boolean;
  public
    class function Defaults: TNyxOwnedPascalCompletionOptions; static;
    function WithTranslation(const AValue: TNyxPascalTranslationOptions): TNyxOwnedPascalCompletionOptions;
    function WithSyntax(const AValue: TNyxPascalSyntaxOptions): TNyxOwnedPascalCompletionOptions;
    function WithBinding(const AValue: TNyxPascalBindingOptions): TNyxOwnedPascalCompletionOptions;
    { Bound ordered semantic work within each root as well as AggregateTokens. }
    function WithConditions(const AValue: TNyxPascalConditionalOptions): TNyxOwnedPascalCompletionOptions;
    function WithSourceReuse(const AValue: TNyxPascalSourceReuseOptions): TNyxOwnedPascalCompletionOptions;
    function UpToTokens(AValue: Integer): TNyxOwnedPascalCompletionOptions;
    function UpToReceiverTokens(AValue: Integer): TNyxOwnedPascalCompletionOptions;
    { Limit successful on-demand module additions across the entire request. }
    function UpToDependencyLoads(AValue: Integer): TNyxOwnedPascalCompletionOptions;
    procedure Validate;
    property Translation: TNyxPascalTranslationOptions read FTranslation;
    property Syntax: TNyxPascalSyntaxOptions read FSyntax;
    property Binding: TNyxPascalBindingOptions read FBinding;
    property Conditions: TNyxPascalConditionalOptions read FConditions;
    property SourceReuse: TNyxPascalSourceReuseOptions read FReuse;
    property AggregateTokens: Integer read FTokens;
    property ReceiverTokens: Integer read FReceiverTokens;
    property DependencyLoads: Integer read FDependencyLoads;
  end;
  { Closed assistance gaps, independent of compiler diagnostics or admission.
    Binding preserves its own typed issue/related name for focused context capture. }
  TNyxOwnedPascalCompletionGap = (pcgNone, pcgExpression, pcgPhysicalContext,
    pcgRepeatedInclude, pcgSourceIdentity, pcgBinding, pcgBudget, pcgCancelled, pcgInternal);
  { Physical declaration location within an explicit analysis root. These virtual
    names confer no filesystem/edit permission. Repeated inclusion has its exact
    occurrence; Span addresses the original declaration name in scalar units. }
  TNyxPascalCompletionDefinition = record
    Root: TNyxPascalPhysicalFile;
    FileReference: TNyxPascalPhysicalFile;
    Span: TNyxPascalSpan;
    Occurrence: Integer;
  end;
  { Optional in-process definition/gap metadata. The ordinary completion result
    and existing wire schema stay unchanged; codecs do not transport this
    capability. Ready rows retain their exact physical lexical source after the
    provider is released/replaced, so navigation need not reopen a changed file. }
  INyxOwnedPascalCompletionResult = interface(IInterface)
    ['{170458CB-C602-4E28-84F0-E90C2408EE01}']
    function Gap: TNyxOwnedPascalCompletionGap;
    function BindingIssue: TNyxPascalBindingIssue;
    function RelatedName: TNyxText;
    function Definition(AIndex: Integer): TNyxPascalCompletionDefinition;
    function DefinitionSource(AIndex: Integer): INyxPascalLexicalSnapshot;
  end;
  { Optional original-byte navigation for a ready row. The detached readable
    view retains exact bytes/encoding after provider/world retirement. Its origin
    distinguishes external bytes from canonical authoring UTF-8. No wire schema
    or file/process authority follows from these in-process coordinates. }
  INyxPascalCompletionByteOrigins = interface(IInterface)
    ['{170458CB-C602-4E28-84F0-E90C2408EE04}']
    function DefinitionView(AIndex: Integer): INyxPascalSourceView;
    function DefinitionByteSpan(AIndex: Integer): TNyxPascalByteSpan;
  end;
  { Optional synchronous cancellation for worker owners. Cancellation is borrowed
    only during this call and publishes a detached cancelled response, no rows.
    The original provider interface remains usable by existing Studio consumers. }
  INyxCancellablePascalCompletionProvider = interface(IInterface)
    ['{170458CB-C602-4E28-84F0-E90C2408EE02}']
    function CompleteWithCancellation(const ARequest: INyxPascalCompletionRequest;
      const ACancellation: INyxCancellationScope): INyxPascalCompletionResult;
  end;
  { Optional detached analysis receipt, independent of wire schemas and source
    publication. Includes every actually translated project/SDK root in insertion
    order; indexed but unused modules are absent. No world/provider is retained. }
  INyxPascalCompletionAnalysis = interface(IInterface)
    ['{170458CB-C602-4E28-84F0-E90C2408EE03}']
    function ModuleCount: Integer;
    function ModuleName(AIndex: Integer): TNyxPascalModuleRef;
    function ModuleRoot(AIndex: Integer): TNyxPascalPhysicalFile;
    function TranslatedTokens: Integer;
    function DependencyLoads: Integer;
  end;
  { Detached counters for actual module construction/rebinding in this request.
    ReuseWork includes admitted lookup/proof/publication work, including misses;
    it is part of TranslatedTokens' historical aggregate-work total. These are
    logical operations, not elapsed-time savings or semantic-world authority.
    No source, provider, world or wire-schema change follows from this receipt. }
  INyxPascalCompletionReuse = interface(IInterface)
    ['{1E0D7288-73BC-452B-83E2-05A831E6DE95}']
    function ConstructedModules: Integer;
    function ReusedModules: Integer;
    function ReuseWork: Integer;
  end;
  { Atomic value-only retention observation. CatalogBytes is conservatively
    counted input per entry, not a native/browser heap-size measurement. Limits
    remain the copied provider policy; this receipt grants no replay authority. }
  TNyxPascalSourceReuseState = record
    Modules: Integer;
    Tokens: Integer;
    CatalogBytes: Integer;
  end;
  { Optional in-process storage observation for hosts/tests. It copies three
    counters under the same short directory lock and retains no source or world. }
  INyxPascalCompletionReuseStorage = interface(IInterface)
    ['{ED992E29-007D-47A3-AFA6-1458A5C1E213}']
    function SourceReuseState: TNyxPascalSourceReuseState;
  end;
  { Detached current-request observations. An accepted module reevaluated all
    its saved answers in the current world. Declines include input mismatches or
    changed/unknown answers; these counters never grant replay authority. }
  INyxPascalCompletionRevalidation = interface(IInterface)
    ['{E1BC7811-2BBA-45E2-AE2B-8B05A3891F60}']
    function RevalidatedModules: Integer;
    function RevalidatedAnswers: Integer;
    function RevalidationDeclines: Integer;
  end;
  { Detached observations of current-source projection revisions after a cache
    miss. Prefix counts report lexical proof, not automatic parser restoration;
    parser checkpoints independently decide which metadata can remain. }
  INyxPascalCompletionRevision = interface(IInterface)
    ['{AE0EC959-8BE8-4A5E-8A7E-624868A95834}']
    function RevisedModules: Integer;
    function RevisionRetainedTokens: Integer;
    function SharedSyntaxModules: Integer;
  end;
  { Physical work summed over completed current source constructions, including
    dependency retries. Semantic/source rebinds perform no physical scanning.
    These scalar receipts retain no source/world and confer no reuse authority. }
  INyxPascalCompletionLexicalWork = interface(IInterface)
    ['{288024D5-B484-4CC4-BD6E-5EA44895661A}']
    function ScannedLexicalTokens: Integer;
    function ReusedLexicalTokens: Integer;
    function LexicalCandidateReads: Integer;
  end;

{ Capture explicit SDK files/roots and compiler facts once. Requests overlay all
  exact project buffers, preserving accepted membership and include ownership.
  The provider reads no disk, starts no compiler, executes no authored code and
  retains no editor. Hosts run synchronous analysis on their scheduler/worker.
  Receivers use source-owned postfix contracts: dotted names, declared fluent
  call results, grouping, pointers and array elements. Calls require every visible
  result to agree; argument admission, casts, helpers and general inference remain
  separate. No authored expression, callback, getter or factory executes.
  Repeated occurrences of the requested include refuse rather than choosing one.
  This factory installs no default provider or Studio configuration. }
function NewNyxOwnedPascalCompletionProvider(const AContext: INyxPascalCompilerContext;
  const AFiles: INyxPascalFileCatalog; const ASDKRoots: array of TNyxPascalPhysicalFile;
  const AOptions: TNyxOwnedPascalCompletionOptions): INyxPascalCompletionProvider;
{ Capture an explicit module index instead of eagerly translating SDK roots.
  Missing-module binding replies load only that indexed captured source, then
  restart semantic queries with fresh exact-world handles. Parsed earlier roots
  and aggregate counters survive retries. Unindexed/partial/context-incompatible
  inputs refuse; nothing searches the filesystem or fabricates export knowledge. }
function NewNyxIndexedPascalCompletionProvider(const AContext: INyxPascalCompilerContext;
  const AFiles: INyxPascalFileCatalog; const AModules: INyxPascalModuleCatalog;
  const AOptions: TNyxOwnedPascalCompletionOptions): INyxPascalCompletionProvider;

implementation

uses SysUtils, nyx.pascal.drafts, nyx.pascal.files.project, nyx.pascal.syntax.translated,
  nyx.pascal.work;

type
  { Internal immutable lease used by the bounded provider directory. A completed
    source/view and exact translation policy survive publication. World-sensitive
    sources additionally retain complete ordered unbound revalidation records;
    no bound type, semantic world or live query owner survives. }
  IModuleMemoEntry = interface(IInterface)
    ['{51FF0964-C808-47B6-AF27-01447FC5B7CA}']
    function View: INyxPascalTranslatedSyntax;
    function Translation: TNyxPascalTranslationOptions;
    function Semantic: INyxPascalSemanticTranslation;
  end;
  { Class-owned dynamic interface vectors keep entry lifetimes valid after
    eviction and outside the short native mutex; no interfaces live in records. }
  TModuleMemoEntries = array of IModuleMemoEntry;
  { Private immutable implementation. The owned condition construction must grant
    either independence or complete ordered revalidation before publication. }
  TModuleMemoEntry = class(TInterfacedObject, IModuleMemoEntry)
  public
    Snapshot: INyxPascalTranslatedSyntax;
    Policy: TNyxPascalTranslationOptions;
    Queries: INyxPascalSemanticTranslation;
    function View: INyxPascalTranslatedSyntax;
    function Translation: TNyxPascalTranslationOptions;
    function Semantic: INyxPascalSemanticTranslation;
  end;
  { An internally raised typed refusal; its bounded English message contains no
    host exception/path. The call owner converts it to a no-row detached result. }
  EOwnedGap = class(Exception)
  public
    GapKind: TNyxOwnedPascalCompletionGap;
    Issue: TNyxPascalBindingIssue;
    Related: TNyxText;
    constructor CreateGap(AGap: TNyxOwnedPascalCompletionGap; const AMessage: TNyxText;
      AIssue: TNyxPascalBindingIssue = pbiNone; const ARelated: TNyxText = '');
  end;
  { Value-only row locations, detached from parser/world lifetimes. }
  TDefinitionVector = array of TNyxPascalCompletionDefinition;
  { Value-owned analysis location; interfaces live only in the owning classes. }
  TModuleLocation = record
    Name: TNyxPascalModuleRef;
    Root: TNyxPascalPhysicalFile;
  end;
  { Delegates guarded insertion/preview to the original result owner. Only
    source-location values and typed gap metadata are added, without a cycle. }
  TOwnedResult = class(TInterfacedObject, INyxPascalCompletionResult,
    INyxOwnedPascalCompletionResult, INyxPascalCompletionAnalysis,
    INyxPascalCompletionByteOrigins, INyxPascalCompletionReuse,
    INyxPascalCompletionRevalidation, INyxPascalCompletionRevision,
    INyxPascalCompletionLexicalWork)
  public
    Base: INyxPascalCompletionResult;
    Definitions: TDefinitionVector;
    DefinitionSources: array of INyxPascalLexicalSnapshot;
    DefinitionViews: array of INyxPascalSourceView;
    Modules: array of TModuleLocation;
    Tokens: Integer;
    Loads: Integer;
    Built: Integer;
    Reused: Integer;
    ProofWork: Integer;
    ValidatedModules: Integer;
    ValidatedAnswers: Integer;
    ValidationDeclines: Integer;
    Revised: Integer;
    RevisionTokens: Integer;
    SharedSyntax: Integer;
    ScannedLexical, ReusedLexical, LexicalReads: Integer;
    GapValue: TNyxOwnedPascalCompletionGap;
    IssueValue: TNyxPascalBindingIssue;
    RelatedValue: TNyxText;
    function Request: INyxPascalCompletionRequest;
    function Status: TNyxPascalCompletionStatus;
    function MessageText: TNyxText;
    function Count: Integer;
    function Total: Integer;
    function Item(AIndex: Integer): TNyxPascalCompletionItem;
    function ReplaceStart: Integer;
    function ExpectedText: TNyxText;
    function Preview(AIndex: Integer; const ACurrent: INyxPascalSourceSnapshot;
      out ANewCaret: Integer): INyxPascalSourceSnapshot;
    function Gap: TNyxOwnedPascalCompletionGap;
    function BindingIssue: TNyxPascalBindingIssue;
    function RelatedName: TNyxText;
    function Definition(AIndex: Integer): TNyxPascalCompletionDefinition;
    function DefinitionSource(AIndex: Integer): INyxPascalLexicalSnapshot;
    function DefinitionView(AIndex: Integer): INyxPascalSourceView;
    function DefinitionByteSpan(AIndex: Integer): TNyxPascalByteSpan;
    function ModuleCount: Integer;
    function ModuleName(AIndex: Integer): TNyxPascalModuleRef;
    function ModuleRoot(AIndex: Integer): TNyxPascalPhysicalFile;
    function TranslatedTokens: Integer;
    function DependencyLoads: Integer;
    function ConstructedModules: Integer;
    function ReusedModules: Integer;
    function ReuseWork: Integer;
    function RevalidatedModules: Integer;
    function RevalidatedAnswers: Integer;
    function RevalidationDeclines: Integer;
    function RevisedModules: Integer;
    function RevisionRetainedTokens: Integer;
    function SharedSyntaxModules: Integer;
    function ScannedLexicalTokens: Integer;
    function ReusedLexicalTokens: Integer;
    function LexicalCandidateReads: Integer;
  end;
  { Frozen configuration plus a bounded immutable-source directory. Every call
    owns its own world/counters. The native mutex guards only fixed slot leases
    and retention counters; source comparisons, cancellation and analysis run
    outside it. Evicted leases retire outside the lock. }
  TOwnedProvider = class(TInterfacedObject, INyxPascalCompletionProvider,
    INyxCancellablePascalCompletionProvider, INyxPascalCompletionReuseStorage)
  private
    FMemo: TModuleMemoEntries;
    FMemoTokenCounts: array of Integer;
    FMemoByteCounts: array of Integer;
    FNext: Integer;
    FMemoCount: Integer;
    FMemoTokens: Integer;
    FMemoBytes: Integer;
    {$ifndef PAS2JS}
    FMemoLock: TRTLCriticalSection;
    FMemoLockReady: Boolean;
    {$endif}
    procedure EnterMemo;
    procedure LeaveMemo;
    function MemoEntries(const ACancel: INyxCancellationScope): TModuleMemoEntries;
    function ReuseModule(const AFile: TNyxPascalPhysicalFile;
      const AFiles: INyxPascalFileCatalog; const AContext: INyxPascalCompilerContext;
      const ATranslation: TNyxPascalTranslationOptions;
      const AWorld: INyxPascalSemanticWorld; const AConditions: TNyxPascalConditionalOptions;
      const ACancel: INyxCancellationScope; out AValidatedModules, AValidatedAnswers,
      ADeclines: Integer; out APrevious: INyxPascalTranslatedSyntax): INyxPascalTranslatedSyntax;
    procedure RememberModule(const AView: INyxPascalTranslatedSyntax;
      const AConstruction: INyxPascalSemanticTranslation;
      const ATranslation: TNyxPascalTranslationOptions;
      const ACancel: INyxCancellationScope);
  public
    constructor Create(const AOptions: TNyxOwnedPascalCompletionOptions);
    destructor Destroy; override;
  public
    Context: INyxPascalCompilerContext;
    Files: INyxPascalFileCatalog;
    Roots: array of TNyxPascalPhysicalFile;
    Modules: INyxPascalModuleCatalog;
    Options: TNyxOwnedPascalCompletionOptions;
    function Complete(const ARequest: INyxPascalCompletionRequest): INyxPascalCompletionResult;
    function CompleteWithCancellation(const ARequest: INyxPascalCompletionRequest;
      const ACancellation: INyxCancellationScope): INyxPascalCompletionResult;
    function SourceReuseState: TNyxPascalSourceReuseState;
  end;
  { One synchronous analysis call. Provider is borrowed only until this call joins;
    immutable sources/world and cancellation are retained for that same lifetime.
    No response retains this helper or a mutable host/editor. }
  TAnalysis = class
  private
    FProvider: TOwnedProvider;
    FRequest: INyxPascalCompletionRequest;
    FCancel: INyxCancellationScope;
    FFiles: INyxPascalFileCatalog;
    FWorld: INyxPascalSemanticWorld;
    FBuilding: array of TNyxText;
    FOwner: Integer;
    FTokens: Integer;
    FScan: Integer;
    FLoads: Integer;
    FBuilt: Integer;
    FReused: Integer;
    FReuseWork: Integer;
    FValidatedModules: Integer;
    FValidatedAnswers: Integer;
    FValidationDeclines: Integer;
    FRevised: Integer;
    FRevisionTokens: Integer;
    FSharedSyntax: Integer;
    FScannedLexical, FReusedLexical, FLexicalReads: Integer;
    FReceiverReady: Boolean;
    FReceiverExpressionStart: Integer;
    FReceiverPrefix: TNyxText;
    FReceiverExpected: TNyxText;
    FReceiverStart: Integer;
    FReceiverDot: Integer;
    procedure Check;
    procedure Scan;
    procedure AddModule(const AFile: TNyxPascalPhysicalFile; const AExpected: TNyxText;
      const AContext: INyxPascalCompilerContext = nil;
      const ADescriptor: INyxPascalModuleSource = nil; AReplace: Boolean = False);
    procedure BuildWorld;
    procedure RefineWorld;
    function LoadDependency(const AName: TNyxText): Boolean;
    procedure CaptureAnalysis(AResult: TOwnedResult);
    function Previous(const ALexical: INyxPascalLexicalSnapshot; AIndex: Integer): Integer;
    procedure Receiver(const ALexical: INyxPascalLexicalSnapshot;
      out AStart, ADot, AExpressionStart: Integer; out AExpected, APrefix: TNyxText);
    function Caller(const AFile: TNyxPascalPhysicalFile; ADot, AExpressionStart: Integer;
      out AExpressionWindow: TNyxPascalSpan): INyxPascalBindingSite;
    function Run: INyxPascalCompletionResult;
  public
    constructor Create(AProvider: TOwnedProvider;
      const ARequest: INyxPascalCompletionRequest; const ACancellation: INyxCancellationScope);
    function Complete: INyxPascalCompletionResult;
  end;

class function TNyxPascalSourceReuseOptions.Defaults: TNyxPascalSourceReuseOptions;
begin
  Result.FDefined := True;
  Result.FModules := 16;
  Result.FTokens := 1000000;
  Result.FBytes := 64 * 1024 * 1024;
  Result.FSemanticReplay := True;
  Result.FSyntaxRevision := True;
  Result.FLexicalReuse := True;
end;

procedure TNyxPascalSourceReuseOptions.Validate;
begin

  if not FDefined or (FModules < 0) or (FModules > 256) or
    (FTokens < 1) or (FTokens > 4000000) or
    (FBytes < 1) or (FBytes > 256 * 1024 * 1024) then
  begin
    raise ENyxPascalBinding.Create('Use explicit bounded source reuse limits');
  end;
end;

function TNyxPascalSourceReuseOptions.UpToModules(AValue: Integer): TNyxPascalSourceReuseOptions;
begin
  Result := Self;
  Result.FModules := AValue;
  Result.Validate;
end;

function TNyxPascalSourceReuseOptions.UpToTokens(AValue: Integer): TNyxPascalSourceReuseOptions;
begin
  Result := Self;
  Result.FTokens := AValue;
  Result.Validate;
end;

function TNyxPascalSourceReuseOptions.UpToCatalogBytes(AValue: Integer): TNyxPascalSourceReuseOptions;
begin
  Result := Self;
  Result.FBytes := AValue;
  Result.Validate;
end;

function TNyxPascalSourceReuseOptions.WithSemanticReplay(AValue: Boolean): TNyxPascalSourceReuseOptions;
begin
  Result := Self;
  Result.FSemanticReplay := AValue;
  Result.Validate;
end;

function TNyxPascalSourceReuseOptions.WithSyntaxRevision(AValue: Boolean): TNyxPascalSourceReuseOptions;
begin
  Result := Self;
  Result.FSyntaxRevision := AValue;
  Result.Validate;
end;

function TNyxPascalSourceReuseOptions.WithLexicalReuse(AValue: Boolean): TNyxPascalSourceReuseOptions;
begin
  Result := Self;
  Result.FLexicalReuse := AValue;
  Result.Validate;
end;

class function TNyxOwnedPascalCompletionOptions.Defaults: TNyxOwnedPascalCompletionOptions;
begin
  Result.FDefined := True;
  Result.FTranslation := TNyxPascalTranslationOptions.Defaults;
  Result.FSyntax := TNyxPascalSyntaxOptions.Defaults;
  Result.FBinding := TNyxPascalBindingOptions.Defaults.WithImplicitScopes(pispCompilerUnits);
  Result.FConditions := TNyxPascalConditionalOptions.Defaults;
  Result.FReuse := TNyxPascalSourceReuseOptions.Defaults;
  Result.FTokens := 1000000;
  Result.FReceiverTokens := 128;
  Result.FDependencyLoads := 128;
end;

procedure TNyxOwnedPascalCompletionOptions.Validate;
begin

  if not FDefined or (FTokens < 1) or (FReceiverTokens < 1) or (FDependencyLoads < 1) then
  begin
    raise ENyxPascalBinding.Create('Use explicit positive owned completion budgets');
  end;
  FTranslation.Validate;
  FSyntax.Validate;
  FBinding.Validate;
  FConditions.Validate;
  FReuse.Validate;
end;

function TNyxOwnedPascalCompletionOptions.WithTranslation(
  const AValue: TNyxPascalTranslationOptions): TNyxOwnedPascalCompletionOptions;
begin
  Result := Self;
  Result.FTranslation := AValue;
  Result.Validate;
end;

function TNyxOwnedPascalCompletionOptions.WithSyntax(
  const AValue: TNyxPascalSyntaxOptions): TNyxOwnedPascalCompletionOptions;
begin
  Result := Self;
  Result.FSyntax := AValue;
  Result.Validate;
end;

function TNyxOwnedPascalCompletionOptions.WithBinding(
  const AValue: TNyxPascalBindingOptions): TNyxOwnedPascalCompletionOptions;
begin
  Result := Self;
  Result.FBinding := AValue;
  Result.Validate;
end;

function TNyxOwnedPascalCompletionOptions.WithConditions(
  const AValue: TNyxPascalConditionalOptions): TNyxOwnedPascalCompletionOptions;
begin
  Result := Self;
  Result.FConditions := AValue;
  Result.Validate;
end;

function TNyxOwnedPascalCompletionOptions.WithSourceReuse(
  const AValue: TNyxPascalSourceReuseOptions): TNyxOwnedPascalCompletionOptions;
begin
  Result := Self;
  Result.FReuse := AValue;
  Result.Validate;
end;

function TNyxOwnedPascalCompletionOptions.UpToTokens(AValue: Integer): TNyxOwnedPascalCompletionOptions;
begin
  Result := Self;
  Result.FTokens := AValue;
  Result.Validate;
end;

function TNyxOwnedPascalCompletionOptions.UpToReceiverTokens(AValue: Integer): TNyxOwnedPascalCompletionOptions;
begin
  Result := Self;
  Result.FReceiverTokens := AValue;
  Result.Validate;
end;

function TNyxOwnedPascalCompletionOptions.UpToDependencyLoads(AValue: Integer): TNyxOwnedPascalCompletionOptions;
begin
  Result := Self;
  Result.FDependencyLoads := AValue;
  Result.Validate;
end;

constructor EOwnedGap.CreateGap(AGap: TNyxOwnedPascalCompletionGap;
  const AMessage: TNyxText; AIssue: TNyxPascalBindingIssue; const ARelated: TNyxText);
begin
  inherited Create(AMessage);
  GapKind := AGap;
  Issue := AIssue;
  Related := ARelated;
end;

function TOwnedResult.Request: INyxPascalCompletionRequest;
begin
  Result := Base.Request;
end;

function TOwnedResult.Status: TNyxPascalCompletionStatus;
begin
  Result := Base.Status;
end;

function TOwnedResult.MessageText: TNyxText;
begin
  Result := Base.MessageText;
end;

function TOwnedResult.Count: Integer;
begin
  Result := Base.Count;
end;

function TOwnedResult.Total: Integer;
begin
  Result := Base.Total;
end;

function TOwnedResult.Item(AIndex: Integer): TNyxPascalCompletionItem;
begin
  Result := Base.Item(AIndex);
end;

function TOwnedResult.ReplaceStart: Integer;
begin
  Result := Base.ReplaceStart;
end;

function TOwnedResult.ExpectedText: TNyxText;
begin
  Result := Base.ExpectedText;
end;

function TOwnedResult.Preview(AIndex: Integer; const ACurrent: INyxPascalSourceSnapshot;
  out ANewCaret: Integer): INyxPascalSourceSnapshot;
begin
  Result := Base.Preview(AIndex, ACurrent, ANewCaret);
end;

function TOwnedResult.Gap: TNyxOwnedPascalCompletionGap;
begin
  Result := GapValue;
end;

function TOwnedResult.BindingIssue: TNyxPascalBindingIssue;
begin
  Result := IssueValue;
end;

function TOwnedResult.RelatedName: TNyxText;
begin
  Result := RelatedValue;
end;

function TOwnedResult.Definition(AIndex: Integer): TNyxPascalCompletionDefinition;
begin
  Item(AIndex);
  Result := Definitions[AIndex];
end;

function TOwnedResult.DefinitionSource(AIndex: Integer): INyxPascalLexicalSnapshot;
begin
  Item(AIndex);
  Result := DefinitionSources[AIndex];
end;

function TOwnedResult.DefinitionView(AIndex: Integer): INyxPascalSourceView;
begin
  Item(AIndex);
  Result := DefinitionViews[AIndex];
end;

function TOwnedResult.DefinitionByteSpan(AIndex: Integer): TNyxPascalByteSpan;
begin
  Item(AIndex);
  Result := DefinitionViews[AIndex].OriginalSpan(Definitions[AIndex].Span);
end;

function TOwnedResult.ModuleCount: Integer;
begin
  Result := Length(Modules);
end;

function TOwnedResult.ModuleName(AIndex: Integer): TNyxPascalModuleRef;
begin

  if (AIndex < 0) or (AIndex >= Length(Modules)) then
  begin
    raise ENyxPascalContext.Create('Analysis module index is outside its receipt');
  end;
  Result := Modules[AIndex].Name;
end;

function TOwnedResult.ModuleRoot(AIndex: Integer): TNyxPascalPhysicalFile;
begin
  ModuleName(AIndex);
  Result := Modules[AIndex].Root;
end;

function TOwnedResult.TranslatedTokens: Integer;
begin
  Result := Tokens;
end;

function TOwnedResult.DependencyLoads: Integer;
begin
  Result := Loads;
end;

function TOwnedResult.ConstructedModules: Integer;
begin
  Result := Built;
end;

function TOwnedResult.ReusedModules: Integer;
begin
  Result := Reused;
end;

function TOwnedResult.ReuseWork: Integer;
begin
  Result := ProofWork;
end;

function TOwnedResult.RevalidatedModules: Integer;
begin Result := ValidatedModules; end;
function TOwnedResult.RevalidatedAnswers: Integer;
begin Result := ValidatedAnswers; end;
function TOwnedResult.RevalidationDeclines: Integer;
begin Result := ValidationDeclines; end;

function TOwnedResult.RevisedModules: Integer;
begin Result := Revised; end;
function TOwnedResult.RevisionRetainedTokens: Integer;
begin Result := RevisionTokens; end;
function TOwnedResult.SharedSyntaxModules: Integer;
begin Result := SharedSyntax; end;
function TOwnedResult.ScannedLexicalTokens: Integer;
begin Result := ScannedLexical; end;
function TOwnedResult.ReusedLexicalTokens: Integer;
begin Result := ReusedLexical; end;
function TOwnedResult.LexicalCandidateReads: Integer;
begin Result := LexicalReads; end;

function TModuleMemoEntry.View: INyxPascalTranslatedSyntax;
begin
  Result := Snapshot;
end;

function TModuleMemoEntry.Translation: TNyxPascalTranslationOptions;
begin
  Result := Policy;
end;

function TModuleMemoEntry.Semantic: INyxPascalSemanticTranslation;
begin Result := Queries; end;

{ Admission and cancellation are checked before the fixed slot operation, never
  from within the native mutex. Bulk units describe actual slot inspections or
  leases about to run, not historical work avoided by a cache hit. }
procedure AdmitReuse(const ACancel: INyxCancellationScope; AUnits: Integer = 1);
begin

  if (ACancel <> nil) and ACancel.Cancelled then
    raise ENyxPascalAnalysisCancelled.Create('Completion source reuse cancelled');
  NyxPascalAdmitAnalysis(ACancel, papSourceProof, AUnits);
end;

function SameTranslationOptions(const ALeft, ARight: TNyxPascalTranslationOptions): Boolean;
begin
  Result := (ALeft.MaximumTokens = ARight.MaximumTokens) and
    (ALeft.MaximumExpandedBytes = ARight.MaximumExpandedBytes) and
    (ALeft.MaximumIncludeDepth = ARight.MaximumIncludeDepth) and
    (ALeft.MaximumOccurrences = ARight.MaximumOccurrences) and
    (ALeft.MaximumStates = ARight.MaximumStates) and
    (ALeft.MaximumDiagnostics = ARight.MaximumDiagnostics) and
    (ALeft.LexicalOptions.AllowNestedComments = ARight.LexicalOptions.AllowNestedComments) and
    (ALeft.LexicalOptions.AllowUnicodeIdentifiers = ARight.LexicalOptions.AllowUnicodeIdentifiers) and
    (ALeft.LexicalOptions.MaximumSourceBytes = ARight.LexicalOptions.MaximumSourceBytes) and
    (ALeft.LexicalOptions.MaximumTokens = ARight.LexicalOptions.MaximumTokens) and
    (ALeft.LexicalOptions.MaximumDiagnostics = ARight.LexicalOptions.MaximumDiagnostics) and
    (ALeft.LexicalOptions.MaximumCommentDepth = ARight.LexicalOptions.MaximumCommentDepth);
end;

constructor TOwnedProvider.Create(const AOptions: TNyxOwnedPascalCompletionOptions);
begin
  inherited Create;
  AOptions.Validate;
  Options := AOptions;
  {$ifndef PAS2JS}
  InitCriticalSection(FMemoLock);
  FMemoLockReady := True;
  {$endif}
  SetLength(FMemo, Options.SourceReuse.MaximumModules);
  SetLength(FMemoTokenCounts, Length(FMemo));
  SetLength(FMemoByteCounts, Length(FMemo));
end;

destructor TOwnedProvider.Destroy;
begin
  { In-flight callers retain the provider through their public interface call;
    no query can enter this directory after its final owner retires. }
  FMemo := nil;
  {$ifndef PAS2JS}

  if FMemoLockReady then DoneCriticalSection(FMemoLock);
  {$endif}
  inherited Destroy;
end;

procedure TOwnedProvider.EnterMemo;
begin
  {$ifndef PAS2JS}EnterCriticalSection(FMemoLock);{$endif}
end;

procedure TOwnedProvider.LeaveMemo;
begin
  {$ifndef PAS2JS}LeaveCriticalSection(FMemoLock);{$endif}
end;

function TOwnedProvider.SourceReuseState: TNyxPascalSourceReuseState;
begin
  EnterMemo;
  try
    Result.Modules := FMemoCount;
    Result.Tokens := FMemoTokens;
    Result.CatalogBytes := FMemoBytes;
  finally
    LeaveMemo;
  end;
end;

function TOwnedProvider.MemoEntries(const ACancel: INyxCancellationScope): TModuleMemoEntries;
var
  LAt: Integer;
begin
  Result := nil;
  AdmitReuse(ACancel, Length(FMemo) + 1);
  SetLength(Result, Length(FMemo));
  EnterMemo;
  try
    for LAt := 0 to High(FMemo) do
      Result[LAt] := FMemo[(FNext + Length(FMemo) - 1 - LAt) mod Length(FMemo)];
  finally
    LeaveMemo;
  end;
end;

function TOwnedProvider.ReuseModule(const AFile: TNyxPascalPhysicalFile;
  const AFiles: INyxPascalFileCatalog; const AContext: INyxPascalCompilerContext;
  const ATranslation: TNyxPascalTranslationOptions;
  const AWorld: INyxPascalSemanticWorld; const AConditions: TNyxPascalConditionalOptions;
  const ACancel: INyxCancellationScope; out AValidatedModules, AValidatedAnswers,
  ADeclines: Integer; out APrevious: INyxPascalTranslatedSyntax): INyxPascalTranslatedSyntax;
var
  LEntries: TModuleMemoEntries;
  LView: INyxPascalTranslatedSyntax;
  LAt: Integer;
  LQueries, LCurrent: INyxPascalSemanticTranslation;
  LReplay: INyxPascalSemanticReplay;
begin
  Result := nil;
  AValidatedModules := 0;
  AValidatedAnswers := 0;
  ADeclines := 0;
  APrevious := nil;

  if Length(FMemo) = 0 then Exit;
  LEntries := MemoEntries(ACancel);
  for LAt := 0 to High(LEntries) do
  begin
    AdmitReuse(ACancel);

    if LEntries[LAt] = nil then Continue;
    LView := LEntries[LAt].View;

    if (LView.Source.Files.FileAt(LView.Source.Occurrence(0).FileIndex).Name <> AFile.Name) or
      not SameTranslationOptions(LEntries[LAt].Translation, ATranslation) or
      not LView.Source.InitialContext.SameFacts(AContext) then Continue;

    if Options.SourceReuse.SyntaxRevision or Options.SourceReuse.LexicalReuse then APrevious := LView;
    { Examine the newest matching key only. A used-file mismatch returns to the
      cold path rather than repeatedly proving older source generations. }
    LQueries := LEntries[LAt].Semantic;

    if LQueries = nil then
      Result := RebindNyxPascalTranslatedSyntax(LView, AFiles, ACancel)
    else
    begin
      LCurrent := RebindNyxPascalSemanticTranslation(LQueries, AFiles, AWorld, AConditions, ACancel);

      if LCurrent = nil then
        Inc(ADeclines)
      else
      begin
        Result := RebindNyxPascalTranslatedSource(LView, LCurrent.Source, ACancel);

        if Result <> nil then
        begin
          Supports(LCurrent, INyxPascalSemanticReplay, LReplay);
          AValidatedModules := 1;
          AValidatedAnswers := LReplay.RevalidatedAnswers;
        end;
      end;
    end;
    Exit;
  end;
end;

procedure TOwnedProvider.RememberModule(const AView: INyxPascalTranslatedSyntax;
  const AConstruction: INyxPascalSemanticTranslation;
  const ATranslation: TNyxPascalTranslationOptions;
  const ACancel: INyxCancellationScope);
var
  LEntry: TModuleMemoEntry;
  LLease: IModuleMemoEntry;
  LRetired: TModuleMemoEntries;
  LTokens, LBytes, LAt, LSlot: Integer;
  LDependencies: INyxPascalSemanticDependencies;
  LReplay: INyxPascalSemanticReplay;
begin

  if Length(FMemo) = 0 then Exit;
  LTokens := AView.Source.Count;
  LBytes := AView.Source.Files.ByteCount;

  if (LTokens > Options.SourceReuse.MaximumTokens) or
    (LBytes > Options.SourceReuse.MaximumCatalogBytes) then Exit;
  if (AConstruction = nil) or (AConstruction.Source <> AView.Source) or
    not Supports(AConstruction, INyxPascalSemanticDependencies, LDependencies) then Exit;

  if LDependencies.ConsultedWorld then
  begin

    if not Supports(AConstruction, INyxPascalSemanticReplay, LReplay) or
      not LReplay.ReplayAvailable or
      (LReplay.ReplayTokens > Options.SourceReuse.MaximumTokens - LTokens) or
      (LReplay.ReplayCatalogBytes > Options.SourceReuse.MaximumCatalogBytes - LBytes) then Exit;
    Inc(LTokens, LReplay.ReplayTokens);
    Inc(LBytes, LReplay.ReplayCatalogBytes);
  end;
  AdmitReuse(ACancel, Length(FMemo) + 1);
  LEntry := TModuleMemoEntry.Create;
  LLease := LEntry;
  LEntry.Snapshot := AView;
  LEntry.Policy := ATranslation;

  if LDependencies.ConsultedWorld then LEntry.Queries := AConstruction;
  SetLength(LRetired, Length(FMemo));
  EnterMemo;
  try
    for LAt := 0 to High(FMemo) do
    begin
      LSlot := (FNext + LAt) mod Length(FMemo);

      if (LAt = 0) or
        (FMemoTokens > Options.SourceReuse.MaximumTokens - LTokens) or
        (FMemoBytes > Options.SourceReuse.MaximumCatalogBytes - LBytes) then
      begin
        LRetired[LSlot] := FMemo[LSlot];

        if FMemo[LSlot] <> nil then Dec(FMemoCount);
        FMemo[LSlot] := nil;
        Dec(FMemoTokens, FMemoTokenCounts[LSlot]);
        Dec(FMemoBytes, FMemoByteCounts[LSlot]);
        FMemoTokenCounts[LSlot] := 0;
        FMemoByteCounts[LSlot] := 0;
      end;
    end;
    FMemo[FNext] := LLease;
    FMemoTokenCounts[FNext] := LTokens;
    FMemoByteCounts[FNext] := LBytes;
    Inc(FMemoTokens, LTokens);
    Inc(FMemoBytes, LBytes);
    Inc(FMemoCount);
    FNext := (FNext + 1) mod Length(FMemo);
  finally
    LeaveMemo;
  end;
  { No entry can reach its final release while the directory lock is held. }
  LRetired := nil;
end;

constructor TAnalysis.Create(AProvider: TOwnedProvider;
  const ARequest: INyxPascalCompletionRequest; const ACancellation: INyxCancellationScope);
begin
  inherited Create;
  FProvider := AProvider;
  FRequest := CaptureNyxPascalCompletionRequest(ARequest);
  FCancel := ACancellation;
end;

procedure TAnalysis.Check;
begin

  if (FCancel <> nil) and FCancel.Cancelled then
  begin
    raise ENyxPascalAnalysisCancelled.Create('Owned Pascal completion cancelled');
  end;
end;

procedure TAnalysis.Scan;
begin
  Check;

  if FScan >= FProvider.Options.ReceiverTokens then
  begin
    raise EOwnedGap.CreateGap(pcgBudget, 'The receiver expression exceeds its token budget');
  end;
  Inc(FScan);
end;

procedure TAnalysis.AddModule(const AFile: TNyxPascalPhysicalFile; const AExpected: TNyxText;
  const AContext: INyxPascalCompilerContext; const ADescriptor: INyxPascalModuleSource;
  AReplace: Boolean);
var
  LView: INyxPascalTranslatedSyntax;
  LSource: INyxPascalTranslation;
  LName: TNyxText;
  LFacts: INyxPascalCompilerContext;
  LOptions: TNyxPascalTranslationOptions;
  LConstruction: INyxPascalSemanticTranslation;
  LConditions: TNyxPascalConditionalOptions;
  LDependencies: INyxPascalSemanticDependencies;
  LProof: INyxPascalAnalysisBudget;
  LAttemptWork: Integer;
  LRetainAllowance: Integer;
  LValidatedModules, LValidatedAnswers, LDeclines: Integer;
  LRetainedReplay: INyxPascalSemanticReplay;
  LPreviousView: INyxPascalTranslatedSyntax;
  LRevision: INyxPascalProjectionRevisionWork;
  LLexicalWork: INyxPascalTranslationLexicalWork;
  LRevisionWork: Integer;
  LAt: Integer;
  LDepth: Integer;
  LRemaining: Integer;
begin
  Check;
  LFacts := AContext;
  LOptions := FProvider.Options.Translation;

  if LFacts = nil then
  begin
    LFacts := FProvider.Context;
  end
  else if ADescriptor <> nil then
  begin
    LOptions := ADescriptor.Translation;
  end;

  if (LFacts.Family <> FProvider.Context.Family) or
    (LFacts.Version.FullNumber <> FProvider.Context.Version.FullNumber) then
  begin
    raise EOwnedGap.CreateGap(pcgBinding, 'This indexed module needs a compatible captured compiler identity',
      pbiCompilerContext, AExpected);
  end;

  if NyxPascalSourceViewAt(FFiles, FFiles.Find(AFile)) = nil then
  begin
    raise EOwnedGap.CreateGap(pcgBinding, 'This captured module needs an explicit readable source encoding',
      pbiCompilerContext, AExpected);
  end;
  for LAt := 0 to High(FBuilding) do
  begin

    if FBuilding[LAt] = AFile.Name then
    begin
      raise EOwnedGap.CreateGap(pcgBinding, 'Conditional imports need a still-constructing source root',
        pbiIncompleteSource, AExpected);
    end;
  end;

  if Length(FBuilding) >= FProvider.Options.Binding.MaximumModules then
  begin
    raise EOwnedGap.CreateGap(pcgBudget, 'Conditional dependency construction exceeds its depth budget');
  end;
  LDepth := Length(FBuilding);
  LView := nil;
  LConstruction := nil;
  LRevisionWork := 0;
  SetLength(FBuilding, LDepth + 1);
  FBuilding[LDepth] := AFile.Name;
  try
    while True do
    begin
      Check;
      LRemaining := FProvider.Options.AggregateTokens - FTokens;

      if LRemaining < 1 then
      begin
        raise EOwnedGap.CreateGap(pcgBudget, 'Semantic source roots exceed the aggregate token budget');
      end;
      LConditions := FProvider.Options.Conditions;
      LConditions := LConditions.WithReplayCapture(
        (FProvider.Options.SourceReuse.MaximumModules > 0) and
        FProvider.Options.SourceReuse.SemanticReplay)
        .UpToReplayTokens(FProvider.Options.SourceReuse.MaximumTokens)
        .UpToReplayCatalogBytes(FProvider.Options.SourceReuse.MaximumCatalogBytes);

      if LRemaining < LConditions.MaximumTokens then
      begin
        LConditions := LConditions.UpToTokens(LRemaining);
      end;
      LAttemptWork := 0;

      if FProvider.Options.SourceReuse.MaximumModules > 0 then
      begin
        LProof := NewNyxPascalAnalysisBudget(LConditions.MaximumTokens, FCancel);
        try
          LView := FProvider.ReuseModule(AFile, FFiles, LFacts, LOptions,
            FWorld, LConditions, LProof, LValidatedModules, LValidatedAnswers, LDeclines, LPreviousView);
          Inc(FValidatedModules, LValidatedModules);
          Inc(FValidatedAnswers, LValidatedAnswers);
          Inc(FValidationDeclines, LDeclines);
        finally
          LAttemptWork := LProof.Receipt.Total;
          Inc(FTokens, LAttemptWork);
          Inc(FReuseWork, LAttemptWork);
          LProof.Retire;
        end;
      end;

      if LView <> nil then
      begin
        Inc(FReused);
        LSource := LView.Source;
        Break;
      end;

      if LAttemptWork >= LConditions.MaximumTokens then
        raise EOwnedGap.CreateGap(pcgBudget, 'Source input proof consumed the remaining module allowance');
      LConditions := LConditions.UpToTokens(LConditions.MaximumTokens - LAttemptWork);
      if (LPreviousView <> nil) and FProvider.Options.SourceReuse.LexicalReuse then
        LConstruction := ReviseNyxPascalSemanticTranslation(LPreviousView.Source,
          FFiles, AFile, LFacts, FWorld, LOptions, FProvider.Options.Syntax, LConditions, FCancel)
      else
        LConstruction := NewNyxPascalSemanticTranslation(FFiles, AFile, LFacts,
          FWorld, LOptions, FProvider.Options.Syntax, LConditions, FCancel);
      Inc(FBuilt);
      Inc(FTokens, LConstruction.WorkTokens);
      LSource := LConstruction.Source;

      if Supports(LSource, INyxPascalTranslationLexicalWork, LLexicalWork) then
      begin
        Inc(FScannedLexical, LLexicalWork.ScannedLexicalTokens);
        Inc(FReusedLexical, LLexicalWork.ReusedLexicalTokens);
        Inc(FLexicalReads, LLexicalWork.LexicalCandidateReads);
      end;

      if not LConstruction.Pending or (LConstruction.BindingIssue <> pbiMissingModule) or
        (LConstruction.RelatedName = '') then
      begin
        Break;
      end;

      if not LoadDependency(LConstruction.RelatedName) then
      begin
        Break;
      end;
    end;
  finally
    SetLength(FBuilding, LDepth);
  end;

  if (LView = nil) and (LPreviousView <> nil) and FProvider.Options.SourceReuse.SyntaxRevision then
  begin
    LRetainAllowance := LConditions.MaximumTokens - LConstruction.WorkTokens;
    LRemaining := FProvider.Options.AggregateTokens - FTokens;

    if LRemaining < LRetainAllowance then LRetainAllowance := LRemaining;

    if LRetainAllowance < 1 then
      raise EOwnedGap.CreateGap(pcgBudget, 'Current source revision exceeds the remaining analysis allowance');
    LProof := NewNyxPascalAnalysisBudget(LRetainAllowance, FCancel);
    try
      LView := ReviseNyxPascalTranslatedSyntax(LPreviousView, LSource, FProvider.Options.Syntax, LProof);

      if LView <> nil then
      begin
        Supports(LView, INyxPascalProjectionRevisionWork, LRevision);
        Inc(FRevised);
        Inc(FRevisionTokens, LRevision.RevisionRetainedTokens);

        if LRevision.WholeSyntaxShared then Inc(FSharedSyntax);
      end;
    finally
      LRevisionWork := LProof.Receipt.Total;
      Inc(FTokens, LProof.Receipt.Total);
      Inc(FReuseWork, LProof.Receipt.Total);
      LProof.Retire;
    end;
  end;

  if LView = nil then
    LView := NewNyxPascalTranslatedSyntax(LSource, FProvider.Options.Syntax, FCancel);
  LName := LView.Syntax.Scope(0).Name;

  if LName = '' then
  begin
    raise EOwnedGap.CreateGap(pcgSourceIdentity, 'This captured root has no declared module identity', pbiNone, AFile.Name);
  end;

  if (AExpected <> '') and (NyxPascalASCIIKey(LName) <> NyxPascalASCIIKey(AExpected)) then
  begin
    raise EOwnedGap.CreateGap(pcgSourceIdentity, 'This declared module differs from its project membership', pbiNone, AFile.Name);
  end;

  LAt := FWorld.FindModule(LName);

  if LAt >= 0 then
  begin

    if not AReplace or (FWorld.ModuleAt(LAt).Source.Files.FileAt(
      FWorld.ModuleAt(LAt).Source.Occurrence(0).FileIndex).Name <> AFile.Name) then
    begin
      raise EOwnedGap.CreateGap(pcgSourceIdentity, 'Two captured roots declare the same module', pbiNone, AFile.Name);
    end;
  end;
  FWorld := WithNyxPascalSemanticModule(FWorld, LView, FCancel);
  { Only the owning conditional traversal grants independence or complete
    revalidation records; diagnostic/answer counts and absence of a pending
    query are insufficient on their own.
    Publish after ordinary module/world coherence admission, never before it. }

  if (LConstruction <> nil) and not LConstruction.Pending and LSource.Resolved and
    LView.CompleteTranslation and Supports(LConstruction, INyxPascalSemanticDependencies, LDependencies) and
    (not LDependencies.ConsultedWorld or
      (FProvider.Options.SourceReuse.SemanticReplay and
        Supports(LConstruction, INyxPascalSemanticReplay, LRetainedReplay) and
        LRetainedReplay.ReplayAvailable)) and
    (FProvider.Options.SourceReuse.MaximumModules > 0) then
  begin
    LRetainAllowance := LConditions.MaximumTokens - LConstruction.WorkTokens - LRevisionWork;
    LRemaining := FProvider.Options.AggregateTokens - FTokens;

    if LRemaining < LRetainAllowance then LRetainAllowance := LRemaining;
    { Retention is optional. If its known fixed directory cost does not fit,
      perform no publication work; the complete cold result remains usable. }

    if LRetainAllowance >= FProvider.Options.SourceReuse.MaximumModules + 1 then
    begin
      LProof := NewNyxPascalAnalysisBudget(LRetainAllowance, FCancel);
      try
        FProvider.RememberModule(LView, LConstruction, LOptions, LProof);
      finally
        Inc(FTokens, LProof.Receipt.Total);
        Inc(FReuseWork, LProof.Receipt.Total);
        LProof.Retire;
      end;
    end;
  end;
end;

procedure TAnalysis.BuildWorld;
var
  LAt: Integer;
  LOther: Integer;
  LFile: TNyxPascalPhysicalFile;
  LSource: TNyxPascalSourceRef;
  LProjectRoot: Boolean;
  LExpected: TNyxText;
begin
  Check;

  if FWorld <> nil then
  begin
    Exit;
  end;
  FFiles := FProvider.Files;
  for LAt := 0 to FRequest.Sources.Count - 1 do
  begin
    Check;
    LSource := FRequest.Sources.FileReference(LAt);
    FFiles := WithNyxPascalProjectFile(FFiles, FRequest.Sources, LSource);
  end;
  FWorld := NewNyxPascalSemanticWorld(FProvider.Options.Binding);
  for LAt := 0 to High(FProvider.Roots) do
  begin
    Check;
    LProjectRoot := False;
    for LOther := 0 to FRequest.Sources.Count - 1 do
    begin
      Check;
      LSource := FRequest.Sources.FileReference(LOther);

      if LSource.Role <> npsInclude then
      begin
        LFile := NyxPascalProjectPhysicalFile(FRequest.Sources, LSource);
        LProjectRoot := LProjectRoot or
          (FFiles.Find(LFile) = FFiles.Find(FProvider.Roots[LAt]));
      end;
    end;

    if not LProjectRoot then
    begin
      AddModule(FProvider.Roots[LAt], '');
    end;
  end;
  for LAt := 0 to FRequest.Sources.Count - 1 do
  begin
    Check;
    LSource := FRequest.Sources.FileReference(LAt);

    if LSource.Role <> npsInclude then
    begin
      LExpected := NyxPascalProjectUnit(FRequest.Sources, LSource).Name;
      AddModule(NyxPascalProjectPhysicalFile(FRequest.Sources, LSource), LExpected);
    end;
  end;
  RefineWorld;
  LExpected := NyxPascalProjectUnit(FRequest.Sources, FRequest.Owner).Name;
  FOwner := FWorld.FindModule(LExpected);

  if FOwner < 0 then
  begin
    raise EOwnedGap.CreateGap(pcgSourceIdentity, 'The explicit completion owner has no captured module');
  end;
end;

procedure TAnalysis.RefineWorld;
var
  LAt: Integer;
  LPass: Integer;
  LBefore: Integer;
  LAfter: Integer;
  LDescriptorAt: Integer;
  LView: INyxPascalTranslatedSyntax;
  LDescriptor: INyxPascalModuleSource;
  LRoot: TNyxPascalPhysicalFile;
  LName: TNyxText;
  LChanged: Boolean;

  function Unknowns(const ASource: INyxPascalTranslation): Integer;
  var
    LDiagnostic: Integer;
  begin
    Result := 0;
    for LDiagnostic := 0 to ASource.DiagnosticCount - 1 do
    begin

      if ASource.Diagnostic(LDiagnostic).Issue = ptiUnknownCondition then
      begin
        Inc(Result);
      end;
    end;
  end;

begin
  { All supplied roots now exist. Revisit conditional exports without making
    caller root order a prerequisite. Only a reduction in unresolved conditions
    permits another sweep; ordinary token/pass/load budgets still own all work. }
  LPass := 0;
  repeat
    Check;
    LChanged := False;
    Inc(LPass);
    LAt := 0;
    while LAt < FWorld.Count do
    begin
      Check;
      LView := FWorld.ModuleAt(LAt);
      LBefore := Unknowns(LView.Source);

      if LBefore > 0 then
      begin
        LRoot := LView.Source.Files.FileAt(LView.Source.Occurrence(0).FileIndex);
        LName := LView.Syntax.Scope(0).Name;
        LDescriptor := nil;

        if FProvider.Modules <> nil then
        begin
          LDescriptorAt := FProvider.Modules.Find(NyxPascalModule(LName));

          if LDescriptorAt >= 0 then
          begin
            LDescriptor := FProvider.Modules.Entry(LDescriptorAt);

            if LDescriptor.Root.Name <> LRoot.Name then
            begin
              LDescriptor := nil;
            end;
          end;
        end;
        AddModule(LRoot, LName, LView.Source.InitialContext, LDescriptor, True);
        LAfter := Unknowns(FWorld.ModuleAt(LAt).Source);
        LChanged := LChanged or (LAfter < LBefore);
      end;
      Inc(LAt);
    end;

    if LChanged and (LPass >= FProvider.Options.Conditions.MaximumPasses) then
    begin
      raise EOwnedGap.CreateGap(pcgBudget, 'Conditional module refinement exceeds its pass budget');
    end;
  until not LChanged;
end;

function TAnalysis.LoadDependency(const AName: TNyxText): Boolean;
var
  LAt: Integer;
  LSource: INyxPascalModuleSource;
begin
  Check;
  Result := False;

  if (FProvider.Modules = nil) or (FWorld = nil) or (FWorld.FindModule(AName) >= 0) then
  begin
    Exit;
  end;
  LAt := FProvider.Modules.Find(NyxPascalModule(AName));

  if LAt < 0 then
  begin
    Exit;
  end;

  if (FLoads >= FProvider.Options.DependencyLoads) or
    (FWorld.Count >= FProvider.Options.Binding.MaximumModules) then
  begin
    raise EOwnedGap.CreateGap(pcgBudget, 'The query exceeds its indexed dependency-load budget');
  end;
  LSource := FProvider.Modules.Entry(LAt);
  AddModule(LSource.Root, LSource.Name.Name, LSource.Context, LSource);
  Inc(FLoads);

  if Length(FBuilding) = 0 then
  begin
    RefineWorld;
  end;
  Result := True;
end;

procedure TAnalysis.CaptureAnalysis(AResult: TOwnedResult);
var
  LAt: Integer;
  LView: INyxPascalTranslatedSyntax;
begin
  AResult.Tokens := FTokens;
  AResult.Loads := FLoads;
  AResult.Built := FBuilt;
  AResult.Reused := FReused;
  AResult.ProofWork := FReuseWork;
  AResult.ValidatedModules := FValidatedModules;
  AResult.ValidatedAnswers := FValidatedAnswers;
  AResult.ValidationDeclines := FValidationDeclines;
  AResult.Revised := FRevised;
  AResult.RevisionTokens := FRevisionTokens;
  AResult.SharedSyntax := FSharedSyntax;
  AResult.ScannedLexical := FScannedLexical;
  AResult.ReusedLexical := FReusedLexical;
  AResult.LexicalReads := FLexicalReads;

  if FWorld = nil then
  begin
    Exit;
  end;
  SetLength(AResult.Modules, FWorld.Count);
  for LAt := 0 to FWorld.Count - 1 do
  begin
    LView := FWorld.ModuleAt(LAt);
    AResult.Modules[LAt].Name := NyxPascalModule(LView.Syntax.Scope(0).Name);
    AResult.Modules[LAt].Root := LView.Source.Files.FileAt(LView.Source.Occurrence(0).FileIndex);
  end;
end;

function TAnalysis.Previous(const ALexical: INyxPascalLexicalSnapshot; AIndex: Integer): Integer;
begin
  Result := AIndex;
  while Result >= 0 do
  begin
    Scan;

    if not (ALexical.Token(Result).Kind in [ptkWhitespace, ptkComment, ptkDirective]) then
    begin
      Exit;
    end;
    Dec(Result);
  end;
end;

procedure TAnalysis.Receiver(const ALexical: INyxPascalLexicalSnapshot;
  out AStart, ADot, AExpressionStart: Integer; out AExpected, APrefix: TNyxText);
var
  LAt: Integer;
  LToken: TNyxPascalToken;
  LWord: Integer;
  LPrevious: Integer;
  LDepth: Integer;

  { Find the whole preceding postfix receiver. Balanced windows remain opaque;
    source-owned semantic binding below decides what their result can mean. }
  function Tail(AIndex: Integer): Integer;
  var
    LStack: array of TNyxPascalSymbol;
    LCount: Integer;
    LClose: TNyxPascalSymbol;
    LPart: TNyxPascalToken;
    LCall: Boolean;
  begin
    Inc(LDepth);
    try

      if LDepth > FProvider.Options.Binding.MaximumDepth then
        raise EOwnedGap.CreateGap(pcgBudget, 'Receiver nesting exceeds its configured depth');

      if AIndex < 0 then
        raise EOwnedGap.CreateGap(pcgExpression, 'A postfix receiver is incomplete');
      LPart := ALexical.Token(AIndex);

      if LPart.Kind = ptkWord then
      begin
        AExpressionStart := LPart.Span.Start;
        Result := Previous(ALexical, AIndex - 1);
      end
      else if (LPart.Kind = ptkSymbol) and (LPart.Symbol = psoCaret) then
      begin
        Result := Tail(Previous(ALexical, AIndex - 1));
      end
      else if (LPart.Kind = ptkSymbol) and (LPart.Symbol in [psoGreater, psoShiftRight]) then
      begin
        { A generic callee's closed type arguments belong to its name. The
          semantic type parser validates their grammar; this scan only keeps
          the entire physical receiver instead of dropping its cast target. }
        LCount := 1;

        if LPart.Symbol = psoShiftRight then LCount := 2;
        while LCount > 0 do
        begin
          AIndex := Previous(ALexical, AIndex - 1);

          if AIndex < 0 then raise EOwnedGap.CreateGap(pcgExpression, 'A generic receiver target is unfinished');
          LPart := ALexical.Token(AIndex);

          if LPart.Kind = ptkSymbol then
          begin
            case LPart.Symbol of
              psoGreater: Inc(LCount);
              psoShiftRight: Inc(LCount, 2);
              psoLess: Dec(LCount);
            else
              { Type syntax, separators and bounds retain their own owner. }
            end;
          end;

          if LCount > FProvider.Options.Binding.MaximumDepth then
            raise EOwnedGap.CreateGap(pcgBudget, 'Generic receiver nesting exceeds its configured depth');
        end;
        Result := Tail(Previous(ALexical, AIndex - 1));
      end
      else if (LPart.Kind = ptkSymbol) and (LPart.Symbol in [psoCloseParen, psoCloseBracket]) then
      begin
        LCall := LPart.Symbol = psoCloseBracket;
        LCount := 1;
        SetLength(LStack, 1);
        LStack[0] := LPart.Symbol;
        while LCount > 0 do
        begin
          AIndex := Previous(ALexical, AIndex - 1);

          if AIndex < 0 then
            raise EOwnedGap.CreateGap(pcgExpression, 'A postfix group is unfinished');
          LPart := ALexical.Token(AIndex);

          if LPart.Kind <> ptkSymbol then Continue;

          if LPart.Symbol in [psoCloseParen, psoCloseBracket] then
          begin

            if LCount >= FProvider.Options.Binding.MaximumDepth then
              raise EOwnedGap.CreateGap(pcgBudget, 'Receiver group nesting exceeds its configured depth');
            SetLength(LStack, LCount + 1);
            LStack[LCount] := LPart.Symbol;
            Inc(LCount);
          end
          else if LPart.Symbol in [psoOpenParen, psoOpenBracket] then
          begin
            LClose := psoCloseParen;

            if LPart.Symbol = psoOpenBracket then LClose := psoCloseBracket;

            if LStack[LCount - 1] <> LClose then
              raise EOwnedGap.CreateGap(pcgExpression, 'Postfix delimiters are crossed');
            Dec(LCount);
          end;
        end;
        AExpressionStart := LPart.Span.Start;
        Result := Previous(ALexical, AIndex - 1);

        if Result >= 0 then
        begin
          LPart := ALexical.Token(Result);
          LCall := LCall or ((LPart.Kind = ptkWord) and (LPart.Word = pkwNone)) or
            ((LPart.Kind = ptkSymbol) and (LPart.Symbol in [psoCloseParen, psoCloseBracket, psoCaret,
              psoGreater, psoShiftRight]));
        end;

        if LCall then Result := Tail(Result);
      end
      else raise EOwnedGap.CreateGap(pcgExpression, 'This receiver needs further expression inference');

      if (Result >= 0) and (ALexical.Token(Result).Symbol = psoDot) then
        Result := Tail(Previous(ALexical, Result - 1));
    finally
      Dec(LDepth);
    end;
  end;
begin
  AStart := FRequest.Caret;
  AExpected := '';
  APrefix := '';
  LAt := ALexical.TokenAt(FRequest.Caret);
  LToken := ALexical.Token(LAt);
  LWord := -1;

  if (LToken.Kind = ptkWord) then
  begin
    LWord := LAt;
  end
  else if (LAt > 0) and (ALexical.Token(LAt - 1).Kind = ptkWord) and
    (ALexical.Token(LAt - 1).Span.Finish = FRequest.Caret) then
  begin
    LWord := LAt - 1;
  end
  else if (LToken.Kind in [ptkString, ptkCharacter, ptkComment, ptkDirective]) and
    (LToken.Span.Start < FRequest.Caret) then
  begin
    raise EOwnedGap.CreateGap(pcgExpression, 'Completion caret is inside literal, comment or directive text');
  end;

  if LWord >= 0 then
  begin
    LToken := ALexical.Token(LWord);
    AStart := LToken.Span.Start;
    AExpected := ALexical.Text(LWord);
    APrefix := ALexical.Slice(NyxPascalSpan(AStart, FRequest.Caret - AStart));

    if LToken.Escaped and (APrefix <> '') then
    begin
      Delete(APrefix, 1, 1);
    end;
    LAt := LWord;
  end;
  LAt := Previous(ALexical, LAt - 1);

  if (LAt < 0) or (ALexical.Token(LAt).Symbol <> psoDot) then
  begin
    raise EOwnedGap.CreateGap(pcgExpression, 'This query needs a supported declared receiver before a member dot');
  end;
  ADot := ALexical.Token(LAt).Span.Start;
  LDepth := 0;
  LPrevious := Tail(Previous(ALexical, LAt - 1));

  if (LPrevious >= 0) and (ALexical.Token(LPrevious).Kind = ptkSymbol) and
    (ALexical.Token(LPrevious).Symbol in [psoCloseParen, psoCloseBracket,
      psoCaret, psoAt, psoPlus, psoMinus]) then
    raise EOwnedGap.CreateGap(pcgExpression, 'This receiver prefix needs expression inference');
end;

function TAnalysis.Caller(const AFile: TNyxPascalPhysicalFile; ADot, AExpressionStart: Integer;
  out AExpressionWindow: TNyxPascalSpan): INyxPascalBindingSite;
var
  LView: INyxPascalTranslatedSyntax;
  LAt: Integer;
  LFile: Integer;
  LOffset: Integer;
  LFound: Integer;
  LStart: Integer;
begin
  LView := FWorld.ModuleAt(FOwner);
  LFile := LView.Source.Files.Find(AFile);
  LFound := -1;
  LOffset := -1;
  LStart := -1;
  for LAt := 0 to LView.Source.OccurrenceCount - 1 do
  begin
    Check;

    if LView.Source.Occurrence(LAt).FileIndex = LFile then
    begin

      if LFound >= 0 then
      begin
        raise EOwnedGap.CreateGap(pcgRepeatedInclude, 'This file has multiple occurrences in its owner; an explicit occurrence query is required');
      end;
      LFound := LAt;
      LOffset := LView.VirtualAt(LAt, ADot);
      LStart := LView.VirtualAt(LAt, AExpressionStart);
    end;
  end;

  if (LOffset < 0) or (LStart < 0) or (LStart >= LOffset) then
  begin
    raise EOwnedGap.CreateGap(pcgPhysicalContext, 'The member dot has no known-active physical context in its explicit owner');
  end;
  AExpressionWindow := NyxPascalSpan(LStart, LOffset - LStart);
  Result := FWorld.At(FOwner, LView.Syntax.ScopeAt(LOffset), LOffset);
end;

{ UI detail is bounded by scalar, never by a byte/UTF-16 unit boundary. }
function ShortDetail(const AText: TNyxText): TNyxText;
var
  LAt: Integer;
  LScalar: Integer;
  LCount: Integer;
begin
  LAt := 1;
  LCount := 0;
  while (LAt <= Length(AText)) and (LCount < 2000) do
  begin
    NyxNextScalar(AText, LAt, LScalar);
    Inc(LCount);
  end;
  Result := Copy(AText, 1, LAt - 1);
end;

function CompletionKind(AKind: TNyxPascalSyntaxDeclarationKind): TNyxPascalCompletionKind;
begin
  case AKind of
    psdkType:
      begin
        Result := pckType;
      end;
    psdkRoutine:
      begin
        Result := pckMethod;
      end;
    psdkProperty:
      begin
        Result := pckProperty;
      end;
    psdkField:
      begin
        Result := pckField;
      end;
    psdkVariable:
      begin
        Result := pckVariable;
      end;
    psdkParameter:
      begin
        Result := pckParameter;
      end;
    psdkConstant, psdkEnumValue, psdkResourceString:
      begin
        Result := pckConstant;
      end;
    psdkLabel:
      begin
        { Label navigation needs a statement context. A label cannot be emitted
          as an ordinary expression-member completion row. }
        raise ENyxPascalBinding.Create('A statement label is not a value member');
      end;
  end;
end;

function TAnalysis.Run: INyxPascalCompletionResult;
var
  LFile: TNyxPascalPhysicalFile;
  LLex: INyxPascalLexicalSnapshot;
  LSite: INyxPascalBindingSite;
  LPrefix: TNyxText;
  LExpected: TNyxText;
  LStart: Integer;
  LDot: Integer;
  LBinding: INyxPascalResolution;
  LType: INyxPascalSemanticType;
  LAccess: TNyxPascalMemberAccess;
  LQueries: INyxPascalMemberQueries;
  LPage: INyxPascalMemberPage;
  LTotal: INyxPascalMemberTotal;
  LAt: Integer;
  LSymbol: INyxPascalBoundSymbol;
  LView: INyxPascalTranslatedSyntax;
  LOrigin: TNyxPascalSyntaxOrigin;
  LRows: array of TNyxPascalCompletionItem;
  LResult: TOwnedResult;
  LInsert: TNyxText;
  LNameLex: INyxPascalLexicalSnapshot;
  LExpressions: INyxPascalExpressionQueries;
  LExpressionWindow: TNyxPascalSpan;
begin
  BuildWorld;
  LFile := NyxPascalProjectPhysicalFile(FRequest.Sources, FRequest.FileReference);
  try
    LLex := FWorld.ModuleAt(FOwner).Source.Physical(
      FWorld.ModuleAt(FOwner).Source.Files.Find(LFile));
  except
    on ENyxPascalContext do
    begin
      raise EOwnedGap.CreateGap(pcgPhysicalContext, 'The requested file has no active captured inclusion');
    end;
  end;

  if not FReceiverReady then
  begin
    Receiver(LLex, FReceiverStart, FReceiverDot, FReceiverExpressionStart,
      FReceiverExpected, FReceiverPrefix);
    FReceiverReady := True;
  end;
  LStart := FReceiverStart;
  LDot := FReceiverDot;
  LExpected := FReceiverExpected;
  LPrefix := FReceiverPrefix;
  LSite := Caller(LFile, LDot, FReceiverExpressionStart, LExpressionWindow);
  Supports(FWorld, INyxPascalExpressionQueries, LExpressions);
  LBinding := LExpressions.ExpressionType(LSite, LExpressionWindow, LAccess, FCancel);

  if LBinding.Status <> pbsBound then
  begin
    raise EOwnedGap.CreateGap(pcgBinding, LBinding.MessageText,
      LBinding.Issue, LBinding.RelatedName);
  end;
  LType := LBinding.BoundType;
  Supports(FWorld, INyxPascalMemberQueries, LQueries);
  LPage := LQueries.Members(LType, LSite, TNyxPascalMemberQuery.Defaults
    .Matching(LPrefix).Skip(FRequest.Offset).Take(FRequest.Limit).ForAccess(LAccess).CountAll, FCancel);

  if LPage.ResultSet.Status <> pbsBound then
  begin
    raise EOwnedGap.CreateGap(pcgBinding, LPage.ResultSet.MessageText,
      LPage.ResultSet.Issue, LPage.ResultSet.RelatedName);
  end;
  Supports(LPage, INyxPascalMemberTotal, LTotal);
  LResult := TOwnedResult.Create;
  Result := LResult;
  SetLength(LRows, LPage.ResultSet.Count);
  SetLength(LResult.Definitions, Length(LRows));
  SetLength(LResult.DefinitionSources, Length(LRows));
  SetLength(LResult.DefinitionViews, Length(LRows));
  for LAt := 0 to High(LRows) do
  begin
    Check;
    LSymbol := LPage.ResultSet.SymbolAt(LAt);
    LView := FWorld.ModuleAt(LSymbol.ModuleIndex);
    LInsert := LSymbol.Declaration.Name;
    LNameLex := NewNyxPascalLexicalSnapshot(LInsert);

    if LNameLex.Token(0).Word <> pkwNone then
    begin
      LInsert := '&' + LInsert;
    end;
    LRows[LAt] := NyxPascalCompletion(LSymbol.Declaration.Name, LInsert,
      ShortDetail(LView.Syntax.Lexical.Slice(LSymbol.Declaration.HeaderSpan)),
      CompletionKind(LSymbol.Declaration.Kind));
    LOrigin := LSymbol.Origin;
    LResult.Definitions[LAt].FileReference := LView.Source.Files.FileAt(LOrigin.FileIndex);
    LResult.Definitions[LAt].Root := LView.Source.Files.FileAt(LView.Source.Occurrence(0).FileIndex);
    LResult.Definitions[LAt].Span := LOrigin.PhysicalSpan;
    LResult.Definitions[LAt].Occurrence := LOrigin.Occurrence;
    LResult.DefinitionSources[LAt] := NyxPascalOccurrenceLexical(
      LView.Source, LOrigin.Occurrence);
    LResult.DefinitionViews[LAt] := NyxPascalSourceViewAt(LView.Source.Files, LOrigin.FileIndex);
  end;
  Check;
  LResult.Base := NewNyxPascalCompletionResult(FRequest, pcsReady, '',
    LStart, LExpected, LRows, LTotal.Total);
  CaptureAnalysis(LResult);
end;

function TAnalysis.Complete: INyxPascalCompletionResult;
var
  LReply: TOwnedResult;
begin
  try
    Check;
    while True do
    begin
      try
        Exit(Run);
      except
        on LGap: EOwnedGap do
        begin

          if (LGap.GapKind = pcgBinding) and (LGap.Issue = pbiMissingModule) and
            (LGap.Related <> '') and LoadDependency(LGap.Related) then
          begin
            Continue;
          end;
          raise;
        end;
      end;
    end;
  except
    on LError: EOwnedGap do
    begin
      LReply := TOwnedResult.Create;
      Result := LReply;
      LReply.GapValue := LError.GapKind;
      LReply.IssueValue := LError.Issue;
      LReply.RelatedValue := LError.Related;
      LReply.Base := NewNyxPascalCompletionResult(FRequest, pcsUnavailable,
        ShortDetail(TNyxText(LError.Message) + ' ' + LError.Related), 0, '', [], 0);
    end;
    on ENyxPascalAnalysisCancelled do
    begin
      LReply := TOwnedResult.Create;
      Result := LReply;
      LReply.GapValue := pcgCancelled;
      LReply.Base := NewNyxPascalCompletionResult(FRequest, pcsCancelled,
        'Owned Pascal analysis was cancelled', 0, '', [], 0);
    end;
    on LError: ENyxPascalModuleContext do
    begin
      LReply := TOwnedResult.Create;
      Result := LReply;
      LReply.GapValue := pcgBinding;
      LReply.IssueValue := pbiCompilerContext;
      LReply.RelatedValue := LError.RelatedModule;
      LReply.Base := NewNyxPascalCompletionResult(FRequest, pcsUnavailable,
        TNyxText(LError.Message), 0, '', [], 0);
    end;
    on LError: Exception do
    begin
      LReply := TOwnedResult.Create;
      Result := LReply;
      LReply.GapValue := pcgInternal;

      if (LError is ENyxPascalAnalysisWorkLimit) or
        (LError is ENyxPascalLexicalLimit) or (LError is ENyxPascalBindingLimit) or
        (LError is ENyxPascalTranslationLimit) then
      begin
        LReply.GapValue := pcgBudget;
        LReply.Base := NewNyxPascalCompletionResult(FRequest, pcsUnavailable,
          'Owned Pascal analysis exceeded a configured budget', 0, '', [], 0);
      end
      else
      begin
        LReply.Base := NewNyxPascalCompletionResult(FRequest, pcsFailed,
          'Owned Pascal analysis could not complete this captured query', 0, '', [], 0);
      end;
    end;
  end;
  CaptureAnalysis(LReply);
end;

function TOwnedProvider.Complete(const ARequest: INyxPascalCompletionRequest): INyxPascalCompletionResult;
begin
  Result := CompleteWithCancellation(ARequest, nil);
end;

function TOwnedProvider.CompleteWithCancellation(const ARequest: INyxPascalCompletionRequest;
  const ACancellation: INyxCancellationScope): INyxPascalCompletionResult;
var
  LAnalysis: TAnalysis;
begin
  LAnalysis := TAnalysis.Create(Self, ARequest, ACancellation);
  try
    Result := LAnalysis.Complete;
  finally
    LAnalysis.Free;
  end;
end;

function NewNyxOwnedPascalCompletionProvider(const AContext: INyxPascalCompilerContext;
  const AFiles: INyxPascalFileCatalog; const ASDKRoots: array of TNyxPascalPhysicalFile;
  const AOptions: TNyxOwnedPascalCompletionOptions): INyxPascalCompletionProvider;
var
  LProvider: TOwnedProvider;
  LAt: Integer;
  LOther: Integer;
begin
  AOptions.Validate;
  LProvider := TOwnedProvider.Create(AOptions);
  Result := LProvider;
  LProvider.Context := CaptureNyxPascalCompilerContext(AContext);
  LProvider.Files := CaptureNyxPascalFiles(AFiles);

  if Length(ASDKRoots) > AOptions.Binding.MaximumModules then
  begin
    raise ENyxPascalBindingLimit.Create('Captured SDK roots exceed the configured module budget');
  end;
  SetLength(LProvider.Roots, Length(ASDKRoots));
  for LAt := 0 to High(ASDKRoots) do
  begin
    LProvider.Roots[LAt] := NyxPascalPhysicalFile(ASDKRoots[LAt].Name);

    if LProvider.Files.Find(LProvider.Roots[LAt]) < 0 then
    begin
      raise ENyxPascalContext.Create('A completion SDK root needs its exact captured source file');
    end;
    for LOther := 0 to LAt - 1 do
    begin

      if LProvider.Files.Find(LProvider.Roots[LAt]) = LProvider.Files.Find(LProvider.Roots[LOther]) then
      begin
        raise ENyxPascalContext.Create('Completion SDK roots must have independent physical identities');
      end;
    end;
  end;
end;

function NewNyxIndexedPascalCompletionProvider(const AContext: INyxPascalCompilerContext;
  const AFiles: INyxPascalFileCatalog; const AModules: INyxPascalModuleCatalog;
  const AOptions: TNyxOwnedPascalCompletionOptions): INyxPascalCompletionProvider;
var
  LProvider: TOwnedProvider;
begin
  AOptions.Validate;
  LProvider := TOwnedProvider.Create(AOptions);
  Result := LProvider;
  LProvider.Context := CaptureNyxPascalCompilerContext(AContext);
  LProvider.Files := CaptureNyxPascalFiles(AFiles);
  LProvider.Modules := CaptureNyxPascalModules(AModules, LProvider.Files);
end;
end.
