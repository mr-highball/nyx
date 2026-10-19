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
unit nyx.pascal.conditions;
{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.scheduler, nyx.pascal.context, nyx.pascal.files,
  nyx.pascal.translation, nyx.pascal.syntax, nyx.pascal.syntax.translated,
  nyx.pascal.binding, nyx.pascal.work;

type
  { Coarse compatibility stage of a pre-operation refusal. Prefix includes the
    actual lexical/copy/projection/parser/type/world/binding phases; detailed
    successful receipts expose their separate logical work counts. }
  TNyxPascalConditionalWorkPhase = (pcwpTranslation, pcwpPrefix, pcwpConstant);
  { Atomic streaming-work refusal. Counters describe the failed admission, not
    an estimated completed pass. Values own no partial source or callback. The
    separate ENyxPascalTokenLimit still identifies the unchanged full-view cap. }
  ENyxPascalConditionalWorkLimit = class(ENyxPascalTranslationLimit)
  private
    FPhase: TNyxPascalConditionalWorkPhase;
    FWorkBefore: Integer;
    FRequested: Integer;
    FMaximum: Integer;
    FTranslated: Integer;
    FRound: Integer;
    FHasAnalysisPhase: Boolean;
    FAnalysisPhase: TNyxPascalAnalysisPhase;
  public
    { Counts are nonnegative and the aggregate maximum is positive. Requested
      work exceeds the remaining allowance; the refinement round is one-based. }
    constructor Create(APhase: TNyxPascalConditionalWorkPhase; AWorkBefore,
      ARequested, AMaximum, ATranslated, ARound: Integer); reintroduce;
    { Detailed phase from actual admission, with the coarse compatibility stage
      derived here. Older directly constructed diagnostics have no such phase. }
    constructor CreateAnalysis(APhase: TNyxPascalAnalysisPhase; AWorkBefore,
      ARequested, AMaximum, ATranslated, ARound: Integer);
    property Phase: TNyxPascalConditionalWorkPhase read FPhase;
    property WorkBefore: Integer read FWorkBefore;
    property Requested: Integer read FRequested;
    property Maximum: Integer read FMaximum;
    property Translated: Integer read FTranslated;
    property RefinementRound: Integer read FRound;
    property HasAnalysisPhase: Boolean read FHasAnalysisPhase;
    property AnalysisPhase: TNyxPascalAnalysisPhase read FAnalysisPhase;
  end;
  { Which unchanged ceiling selected the failed translation's active cap. A tie
    retains both constraints; it is never mislabeled as a measured bottleneck. }
  TNyxPascalConditionalTokenScope = (pctsView, pctsAggregate, pctsViewAndAggregate);
  { Exact context for a token refusal during one ordered replay pass. All data
    are copied numbers observed before that attempted pass; no partial source,
    semantic world, replay authority or inner exception is retained. Existing
    ENyxPascalTranslationLimit/TokenLimit catchers continue to handle this type. }
  ENyxPascalConditionalTokenLimit = class(ENyxPascalTokenLimit)
  private
    FUsedBeforePass: Integer;
    FAggregateLimit: Integer;
    FViewLimit: Integer;
    FPassIndex: Integer;
    FRemaining: Integer;
    FScope: TNyxPascalConditionalTokenScope;
  public
    { Validate the active cap equals min(view limit, aggregate remainder).
      The attempted pass is one-based; refusal changes no quota or accounting. }
    constructor Create(ATokenCount, AActiveLimit, AUsedBeforePass,
      AAggregateLimit, AViewLimit, APassIndex: Integer); reintroduce;
    property UsedBeforePass: Integer read FUsedBeforePass;
    property AggregateLimit: Integer read FAggregateLimit;
    property ViewLimit: Integer read FViewLimit;
    property PassIndex: Integer read FPassIndex;
    property RemainingBeforePass: Integer read FRemaining;
    property Scope: TNyxPascalConditionalTokenScope read FScope;
  end;
  { Copied construction quotas. Refinement never silently truncates work or
    assumes a result after reaching a limit. These records contain no interfaces. }
  TNyxPascalConditionalOptions = record
  private
    FDefined: Boolean;
    FPasses: Integer;
    FTokens: Integer;
    FReplay: Boolean;
    FReplayTokens: Integer;
    FReplayBytes: Integer;
  public
    { Default: 64 ordered passes and four million total admitted logical work
      units. Tokens, copied/created rows and evaluator/binding steps are charged
      before execution, independently of unchanged full-view/grammar limits. }
    class function Defaults: TNyxPascalConditionalOptions; static;
    function UpToPasses(AValue: Integer): TNyxPascalConditionalOptions;
    function UpToTokens(AValue: Integer): TNyxPascalConditionalOptions;
    { Optional retention never grants an answer or enlarges source/query limits.
      Exceeding either retention bound drops the entire replay capability. }
    function WithReplayCapture(AValue: Boolean): TNyxPascalConditionalOptions;
    function UpToReplayTokens(AValue: Integer): TNyxPascalConditionalOptions;
    function UpToReplayCatalogBytes(AValue: Integer): TNyxPascalConditionalOptions;
    procedure Validate;
    property MaximumPasses: Integer read FPasses;
    property MaximumTokens: Integer read FTokens;
    property CaptureReplay: Boolean read FReplay;
    property MaximumReplayTokens: Integer read FReplayTokens;
    property MaximumReplayCatalogBytes: Integer read FReplayBytes;
  end;
  { Detached construction receipt. Source owns the exact final frozen files and
    ordinary translation diagnostics. Work counts actual admitted analysis
    operations, not an estimated complete-prefix size. Passes retains the logical
    refinement-round count (Answers + 1), not physical source traversal count;
    Pending identifies unresolved semantic authority, not compiler admission.
    Neither input world nor temporary binding sites survive this result. }
  INyxPascalSemanticTranslation = interface(IInterface)
    ['{98349E7C-C0ED-48DE-A518-427A04570C14}']
    function Source: INyxPascalTranslation;
    function WorkTokens: Integer;
    function Passes: Integer;
    function Answers: Integer;
    function Pending: Boolean;
    function Origin: TNyxPascalConditionOrigin;
    function BindingIssue: TNyxPascalBindingIssue;
    function RelatedName: TNyxText;
  end;
  { Detached compatibility totals. TranslationTokens counts emitted source tokens
    once, ConstantSteps counts evaluator steps, and PrefixTokens contains all
    remaining admitted analysis operations (including physical lexical scanning).
    Their sum equals WorkTokens. Detailed phases are available separately; these
    logical units are not elapsed time, CPU instructions or compiler execution. }
  INyxPascalSemanticWork = interface(IInterface)
    ['{E861FCCE-26B8-4CE1-AC36-C4717D79272B}']
    function TranslationTokens: Integer;
    function PrefixTokens: Integer;
    function ConstantSteps: Integer;
  end;
  { Successful detached phase receipt from the exact owning analysis operation.
    No live budget, cancellation parent, parser, world or callback is retained. }
  INyxPascalSemanticAnalysisWork = interface(IInterface)
    ['{628FC50E-DAD2-4BD6-842F-E4D434321126}']
    function AnalysisWork: TNyxPascalAnalysisReceipt;
  end;
  { Detached dependency receipt from the owning conditional traversal. False
    means no source condition consulted the supplied semantic world, including
    failed/unknown queries. It does not certify files, compiler facts or options:
    reuse must prove those separately and require a complete resolved source.
    Absence of this capability never grants world independence. }
  INyxPascalSemanticDependencies = interface(IInterface)
    ['{D79E7674-6EA8-4B9D-974C-F29389518A46}']
    function ConsultedWorld: Boolean;
  end;
  { Detached optional retention receipt. Availability requires every accepted
    semantic site in original order, a resolved final source and no pending
    query. Tokens conservatively count prefix tokens plus expression characters;
    catalog bytes count each retained prefix. These bounds are not heap estimates
    or replay authority: only the owning rebind factory checks private provenance. }
  INyxPascalSemanticReplay = interface(IInterface)
    ['{C39EBFB0-6A46-40E5-A9B1-D1DF5CA42958}']
    function ReplayAvailable: Boolean;
    function ReplayCount: Integer;
    function ReplayTokens: Integer;
    function ReplayCatalogBytes: Integer;
    function RevalidatedAnswers: Integer;
  end;

{ Resolve source-scoped conditions during one ordered traversal, using detached
  frozen pre-directive tokens and exact context at each synchronous source site.
  The first unanswered query blocks later source answers; no unseen declaration
  or unknown suffix state grants authority. No authored text or closing syntax is
  manufactured. Imported meanings come only from the supplied immutable world;
  missing modules/types/measurements remain explicit. Cancellation and resource
  refusal publish no partial receipt. World and context must belong to the same
  captured compiler/output configuration; source mode/H may differ per unit.
  Explicit size facts are trusted inputs: this factory does not establish SDK/
  target coherence. No compiler, filesystem or UI is invoked. }
function NewNyxPascalSemanticTranslation(const AFiles: INyxPascalFileCatalog;
  const ARoot: TNyxPascalPhysicalFile; const AContext: INyxPascalCompilerContext;
  const AWorld: INyxPascalSemanticWorld; const ATranslation: TNyxPascalTranslationOptions;
  const ASyntax: TNyxPascalSyntaxOptions; const AOptions: TNyxPascalConditionalOptions;
  const ACancellation: INyxCancellationScope = nil): INyxPascalSemanticTranslation;

{ Prove full current inputs, then reevaluate every retained semantic query in
  original order against a fresh supplied world, sharing only frozen prefix
  syntax and unbound plans. Changed/unknown answers or unavailable/foreign
  records return nil; cancellation/resource refusal publishes no partial source.
  No compiler, file, UI or authored expression is executed. }
function RebindNyxPascalSemanticTranslation(const APrevious: INyxPascalSemanticTranslation;
  const AFiles: INyxPascalFileCatalog; const AWorld: INyxPascalSemanticWorld;
  const AOptions: TNyxPascalConditionalOptions;
  const ACancellation: INyxCancellationScope = nil): INyxPascalSemanticTranslation;

implementation

uses SysUtils, nyx.pascal.lexing, nyx.pascal.constants, nyx.pascal.storage, nyx.pascal.values,
  nyx.pascal.requirements, nyx.pascal.encoding;

type
  { One private chronological source answer. All retained interfaces point to
    immutable source/context/grammar data, never a bound result or semantic world.
    The owning construction alone creates these records after successful queries. }
  TReplaySite = class
  public
    Origin: TNyxPascalConditionOrigin;
    Context: INyxPascalCompilerContext;
    Expression: TNyxText;
    Projection: INyxPascalTranslatedSyntax;
    Plan: INyxPascalConstantPlan;
    Legacy: Boolean;
    Expected: TNyxPascalKnowledge;
  end;
  { Per-query adapter owns only the current immutable prefix and a borrowed
    aggregate budget. Its child enforces the constant query's remaining limit;
    retirement drops the prefix without retiring the parent operation. }
  TSourceSizeScope = class(TInterfacedObject, INyxPascalAnalysisBudget,
    INyxPascalSourceSizeScope)
  public
    Budget: INyxPascalAnalysisBudget;
    View: INyxPascalTranslatedSyntax;
    SiteOrigin: TNyxPascalConditionOrigin;
    Retired: Boolean;
    procedure Cancel;
    function GetCancelled: Boolean;
    procedure Admit(APhase: TNyxPascalAnalysisPhase; AUnits: Integer = 1);
    function Receipt: TNyxPascalAnalysisReceipt;
    procedure Retire;
    function SourceSize(const ASite: INyxPascalBindingSite;
      const AName: TNyxPascalSizeReference; AMaximumWork: Integer; out AWork: Integer;
      out ARequirement: INyxPascalCompilerRequirement): TNyxPascalStorageFact;
  end;
  { Owns final data only. Publication happens after construction/checks; failure
    cleanup releases the source and all temporary authorities independently. }
  TSemanticTranslation = class(TInterfacedObject, INyxPascalSemanticTranslation,
    INyxPascalCompilerRequirements, INyxPascalSemanticWork,
    INyxPascalSemanticAnalysisWork, INyxPascalSemanticDependencies,
    INyxPascalSemanticReplay, INyxPascalConstantStorage,
    INyxPascalConstantStorageIdentity)
  public
    destructor Destroy; override;
    procedure DropReplay;
  public
    Snapshot: INyxPascalTranslation;
    Tokens: Integer;
    SourceWork: Integer;
    PrefixWork: Integer;
    ConstantWork: Integer;
    WorkReceipt: TNyxPascalAnalysisReceipt;
    Rounds: Integer;
    Accepted: Integer;
    HasQuestion: Boolean;
    WorldConsulted: Boolean;
    Site: TNyxPascalConditionOrigin;
    Issue: TNyxPascalBindingIssue;
    Related: TNyxText;
    Requirements: INyxPascalCompilerRequirements;
    Storage: INyxPascalConstantStorage;
    Sites: array of TReplaySite;
    ReplayIntact: Boolean;
    RetainedTokens: Integer;
    RetainedBytes: Integer;
    Revalidated: Integer;
    function ReplayAvailable: Boolean;
    function ReplayCount: Integer;
    function ReplayTokens: Integer;
    function ReplayCatalogBytes: Integer;
    function RevalidatedAnswers: Integer;
    function Source: INyxPascalTranslation;
    function WorkTokens: Integer;
    function TranslationTokens: Integer;
    function PrefixTokens: Integer;
    function ConstantSteps: Integer;
    function AnalysisWork: TNyxPascalAnalysisReceipt;
    function ConsultedWorld: Boolean;
    function Passes: Integer;
    function Answers: Integer;
    function Pending: Boolean;
    function Origin: TNyxPascalConditionOrigin;
    function BindingIssue: TNyxPascalBindingIssue;
    function RelatedName: TNyxText;
    function RequirementCount: Integer;
    function RequirementAt(AIndex: Integer): INyxPascalCompilerRequirement;
    function StoragePlan: INyxPascalStoragePlan;
    function StorageKey: TNyxPascalStorageKey;
    function StorageNodeCount: Integer;
    function StorageNode(AIndex: Integer): INyxPascalStorageShape;
    function StorageContext(AIndex: Integer): INyxPascalCompilerContext;
    function StorageDefinition(AIndex: Integer): INyxPascalBoundSymbol;
    function StorageIntrinsicOrigin(AIndex: Integer): TNyxPascalIntrinsicOrigin;
    function StorageIntrinsicReference(AIndex: Integer): TNyxPascalSizeReference;
  end;
  { A synchronous site-bound semantic authority. The site owns its prefix world
    outward; no result retains this object. Named size resolution follows bound
    aliases to a captured intrinsic with matching measurement mode/H policy.
    The byte-value capability consumes exact structural receipts independently
    of that legacy single-mode interface. Unmeasured layouts remain unknown;
    no value is inferred from the analyzer host ABI. }
  TScopeMeanings = class(TInterfacedObject, INyxPascalConditionSemantics,
    INyxPascalConditionByteSizes)
  public
    Site: INyxPascalBindingSite;
    Cancellation: INyxCancellationScope;
    Issue: TNyxPascalBindingIssue;
    Related: TNyxText;
    Requirements: INyxPascalCompilerRequirements;
    Storage: INyxPascalConstantStorage;
    ByteSize: TNyxPascalStorageFact;
    procedure Remember(const AAnswer: INyxPascalResolution; const AName: TNyxText);
    function IsDeclared(const AName: TNyxPascalDeclarationRef): TNyxPascalKnowledge;
    function SizeOfName(const AName: TNyxPascalSizeReference): TNyxPascalSizeFact;
    function SizeOfBytes(const AName: TNyxPascalSizeReference): TNyxPascalStorageFact;
  end;
  { Synchronous source authority during one traversal. Data is borrowed from the
    factory's live result lease; frozen prefix worlds point outward to immutable
    values and are released after each query. The first unanswered condition
    blocks later source answers. No published result retains this authority. }
  TConditionalReader = class(TInterfacedObject, INyxPascalConditionalFacts,
    INyxPascalPrefixConditionalFacts, INyxPascalTranslationWork)
  private
    FPrepared: Boolean;
    FLegacy: Boolean;
    FContext: INyxPascalCompilerContext;
    FExpression: TNyxText;
    FOrigin: TNyxPascalConditionOrigin;
    FPlan: INyxPascalConstantPlan;
    { Construction-only detached projection/current prefix world. Sealed lineage
      permits actual source/filter/metadata/type-plan reuse; every repeated scan,
      copy and semantic step is admitted through the operation's execution scope.
      No published query result points back to this reader or its budget. }
    FProjection: INyxPascalTranslatedSyntax;
    FPrefixWorld: INyxPascalSemanticWorld;
    FPrefixModuleName: TNyxText;
    procedure CheckCancellation;
    procedure AdmitExpression(const AText: TNyxText);
    function EvaluateView(const AOrigin: TNyxPascalConditionOrigin;
      const AContext: INyxPascalCompilerContext; const AExpression: TNyxText;
      const AView: INyxPascalTranslatedSyntax): TNyxPascalKnowledge;
    procedure RememberSite(const AOrigin: TNyxPascalConditionOrigin;
      const AContext: INyxPascalCompilerContext; const AExpression: TNyxText;
      const AView: INyxPascalTranslatedSyntax; ATruth: TNyxPascalKnowledge);
  public
    Data: TSemanticTranslation;
    World: INyxPascalSemanticWorld;
    Options: TNyxPascalConditionalOptions;
    SyntaxOptions: TNyxPascalSyntaxOptions;
    Cancellation: INyxCancellationScope;
    Budget: INyxPascalAnalysisBudget;
    procedure AdmitToken(ATokensBefore, AViewLimit: Integer);
    function PreparePrefix(const AOrigin: TNyxPascalConditionOrigin;
      const AContext: INyxPascalCompilerContext; const AExpression: TNyxText): Boolean;
    function EvaluatePrefix(const AOrigin: TNyxPascalConditionOrigin;
      const AContext: INyxPascalCompilerContext; const AExpression: TNyxText;
      const APrefix: INyxPascalTranslation): TNyxPascalKnowledge;
    function Evaluate(const AOrigin: TNyxPascalConditionOrigin;
      const AContext: INyxPascalCompilerContext; const AExpression: TNyxText): TNyxPascalKnowledge;
  end;

constructor ENyxPascalConditionalWorkLimit.Create(APhase: TNyxPascalConditionalWorkPhase;
  AWorkBefore, ARequested, AMaximum, ATranslated, ARound: Integer);
const
  CPhases: array[TNyxPascalConditionalWorkPhase] of String =
    ('translation', 'prefix', 'constant');
begin

  if (Ord(APhase) < Ord(Low(TNyxPascalConditionalWorkPhase))) or
    (Ord(APhase) > Ord(High(TNyxPascalConditionalWorkPhase))) or
    (AMaximum < 1) or (AWorkBefore < 0) or (AWorkBefore > AMaximum) or
    (ARequested < 1) or (ARequested <= AMaximum - AWorkBefore) or
    (ATranslated < 0) or (ARound < 1) then
  begin
    raise ENyxPascalContext.Create('Conditional work refusal needs exact exhausted admission counts');
  end;
  inherited CreateFmt('Ordered conditional work exceeds its aggregate budget ' +
    '[phase=%s used=%d requested=%d limit=%d translated=%d round=%d]',
    [CPhases[APhase], AWorkBefore, ARequested, AMaximum, ATranslated, ARound]);
  FPhase := APhase;
  FWorkBefore := AWorkBefore;
  FRequested := ARequested;
  FMaximum := AMaximum;
  FTranslated := ATranslated;
  FRound := ARound;
end;

constructor ENyxPascalConditionalWorkLimit.CreateAnalysis(APhase: TNyxPascalAnalysisPhase;
  AWorkBefore, ARequested, AMaximum, ATranslated, ARound: Integer);
var
  LPhase: TNyxPascalConditionalWorkPhase;
begin

  if (Ord(APhase) < Ord(Low(TNyxPascalAnalysisPhase))) or
    (Ord(APhase) > Ord(High(TNyxPascalAnalysisPhase))) then
  begin
    raise ENyxPascalContext.Create('Conditional analysis work needs its actual phase');
  end;
  LPhase := pcwpPrefix;

  if APhase = papSourceToken then
  begin
    LPhase := pcwpTranslation;
  end
  else if APhase = papConstantStep then
  begin
    LPhase := pcwpConstant;
  end;
  Create(LPhase, AWorkBefore, ARequested, AMaximum, ATranslated, ARound);
  FHasAnalysisPhase := True;
  FAnalysisPhase := APhase;
  Message := Message + ' [analysis=' + NyxPascalAnalysisPhaseName(APhase) + ']';
end;

constructor ENyxPascalConditionalTokenLimit.Create(ATokenCount, AActiveLimit,
  AUsedBeforePass, AAggregateLimit, AViewLimit, APassIndex: Integer);
const
  ScopeNames: array[TNyxPascalConditionalTokenScope] of String =
    ('translation view', 'conditional aggregate remainder', 'view and aggregate');
var
  LRemaining: Integer;
  LExpected: Integer;
begin

  if (AUsedBeforePass < 0) or (AAggregateLimit <= AUsedBeforePass) or
    (AViewLimit < 1) or (APassIndex < 1) then
  begin
    raise ENyxPascalContext.Create('Conditional token diagnostics need valid observed work limits');
  end;
  LRemaining := AAggregateLimit - AUsedBeforePass;
  LExpected := AViewLimit;

  if LRemaining < LExpected then
  begin
    LExpected := LRemaining;
  end;

  if AActiveLimit <> LExpected then
  begin
    raise ENyxPascalContext.Create('An active token cap must match its observed view and aggregate limits');
  end;
  inherited Create(ATokenCount, AActiveLimit);
  FUsedBeforePass := AUsedBeforePass;
  FAggregateLimit := AAggregateLimit;
  FViewLimit := AViewLimit;
  FPassIndex := APassIndex;
  FRemaining := LRemaining;
  FScope := pctsView;

  if LRemaining < AViewLimit then
  begin
    FScope := pctsAggregate;
  end
  else if LRemaining = AViewLimit then
  begin
    FScope := pctsViewAndAggregate;
  end;
  Message := Message + Format(' [scope=%s pass=%d aggregate-used=%d aggregate-limit=%d view-limit=%d]',
    [ScopeNames[FScope], FPassIndex, FUsedBeforePass, FAggregateLimit, FViewLimit]);
end;

class function TNyxPascalConditionalOptions.Defaults: TNyxPascalConditionalOptions;
begin
  Result := Default(TNyxPascalConditionalOptions);
  Result.FDefined := True;
  Result.FPasses := 64;
  Result.FTokens := 4000000;
  Result.FReplayTokens := 1000000;
  Result.FReplayBytes := 64 * 1024 * 1024;
end;

procedure TNyxPascalConditionalOptions.Validate;
begin

  if not FDefined or (FPasses < 1) or (FTokens < 1) or
    (FReplayTokens < 1) or (FReplayTokens > 4000000) or
    (FReplayBytes < 1) or (FReplayBytes > 256 * 1024 * 1024) then
  begin
    raise ENyxPascalTranslationLimit.Create('Use explicit positive conditional work limits');
  end;
end;

function TNyxPascalConditionalOptions.UpToPasses(AValue: Integer): TNyxPascalConditionalOptions;
begin
  Result := Self;
  Result.FPasses := AValue;
  Result.Validate;
end;

function TNyxPascalConditionalOptions.UpToTokens(AValue: Integer): TNyxPascalConditionalOptions;
begin
  Result := Self;
  Result.FTokens := AValue;
  Result.Validate;
end;


function TNyxPascalConditionalOptions.WithReplayCapture(AValue: Boolean): TNyxPascalConditionalOptions;
begin
  Result := Self;
  Result.FReplay := AValue;
  Result.Validate;
end;

function TNyxPascalConditionalOptions.UpToReplayTokens(AValue: Integer): TNyxPascalConditionalOptions;
begin
  Result := Self;
  Result.FReplayTokens := AValue;
  Result.Validate;
end;

function TNyxPascalConditionalOptions.UpToReplayCatalogBytes(AValue: Integer): TNyxPascalConditionalOptions;
begin
  Result := Self;
  Result.FReplayBytes := AValue;
  Result.Validate;
end;

destructor TSemanticTranslation.Destroy;
begin
  DropReplay;
  inherited Destroy;
end;

procedure TSemanticTranslation.DropReplay;
var
  LAt: Integer;
begin
  ReplayIntact := False;
  for LAt := 0 to High(Sites) do Sites[LAt].Free;
  Sites := nil;
  RetainedTokens := 0;
  RetainedBytes := 0;
end;

function TSemanticTranslation.ReplayAvailable: Boolean;
begin
  Result := ReplayIntact and not HasQuestion and (Snapshot <> nil) and
    Snapshot.Resolved and (Length(Sites) = Accepted);
end;

function TSemanticTranslation.ReplayCount: Integer;
begin
  Result := Length(Sites);
end;

function TSemanticTranslation.ReplayTokens: Integer;
begin
  Result := RetainedTokens;
end;

function TSemanticTranslation.ReplayCatalogBytes: Integer;
begin
  Result := RetainedBytes;
end;

function TSemanticTranslation.RevalidatedAnswers: Integer;
begin
  Result := Revalidated;
end;

procedure TSourceSizeScope.Cancel;
begin Retired := True; end;
function TSourceSizeScope.GetCancelled: Boolean;
begin Result := Retired or Budget.Cancelled; end;
procedure TSourceSizeScope.Admit(APhase: TNyxPascalAnalysisPhase; AUnits: Integer);
begin

  if Retired then raise ENyxPascalAnalysisCancelled.Create('Source-size scope retired');
  Budget.Admit(APhase, AUnits);
end;
function TSourceSizeScope.Receipt: TNyxPascalAnalysisReceipt;
begin Result := Budget.Receipt; end;
procedure TSourceSizeScope.Retire;
begin Retired := True; View := nil; end;
function TSourceSizeScope.SourceSize(const ASite: INyxPascalBindingSite;
  const AName: TNyxPascalSizeReference; AMaximumWork: Integer; out AWork: Integer;
  out ARequirement: INyxPascalCompilerRequirement): TNyxPascalStorageFact;
var
  LRequest: INyxPascalCompilerRequirement;
  LSourceRequest: INyxPascalSourceSizeRequirement;
  LValues: INyxPascalContextStorage;
  LAt, LVisits, LFile: Integer;
  LKey: TNyxPascalStorageKey;
  LChild: INyxPascalAnalysisBudget;
  LSourceView: INyxPascalSourceView;
  LCurrentSite: INyxPascalBindingSite;
begin
  AWork := 0;
  ARequirement := nil;
  Result := NyxPascalStorageFact(psfsUnknown);

  if Retired or (View = nil) then
  begin
    raise ENyxPascalAnalysisCancelled.Create('Source-size scope retired');
  end;

  if (ASite = nil) or (ASite.Caret > View.Syntax.Lexical.Scalars) or
    (ASite.ScopeIndex <> View.Syntax.ScopeAt(ASite.Caret)) or
    (ASite.World.ModuleAt(ASite.ModuleIndex) <> View) or
    not NyxPascalSourceWitnessContext(View) then Exit;

  if AMaximumWork < 1 then
  begin
    raise ENyxPascalConstantLimit.Create('Source-size lookup exceeds the remaining constant work');
  end;
  LChild := NewNyxPascalAnalysisBudget(AMaximumWork, Budget);
  try
    try
      if not NyxPascalSourceSizeReferenceSafe(View, AName, LChild) then Exit;
      LCurrentSite := ASite.World.At(ASite.ModuleIndex,
        View.Syntax.ScopeAt(View.Syntax.Lexical.Scalars), View.Syntax.Lexical.Scalars);

      if (ASite.Caret <> LCurrentSite.Caret) or (ASite.ScopeIndex <> LCurrentSite.ScopeIndex) then
      begin
        { Earlier constant initializers retain their own lookup site. A native
          expression at today's conditional may serve them only after ordinary
          binding proves the same visible symbol and exact named type chain. }

        if not NyxPascalSameStorageBinding(ASite, LCurrentSite,
          NyxPascalBindingName(AName.Name), LChild) then Exit;
      end;
      LFile := View.Source.Occurrence(SiteOrigin.Occurrence).FileIndex;
      LSourceView := NyxPascalSourceViewAt(View.Source.Files, LFile);

      if (LSourceView = nil) or (LSourceView.Kind <> psekUTF8) then Exit;
      LVisits := 0;
      for LAt := 0 to View.Source.OccurrenceCount - 1 do
      begin
        LChild.Admit(papConstantStep);

        if View.Source.Occurrence(LAt).FileIndex = LFile then Inc(LVisits);
      end;

      if LVisits <> 1 then Exit;
      LRequest := NewNyxPascalSourceSizeRequirement(AName, View, SiteOrigin, LChild);
      Supports(LRequest, INyxPascalSourceSizeRequirement, LSourceRequest);
      LKey := LSourceRequest.StorageKey;

      if Supports(LRequest.Context, INyxPascalContextStorage, LValues) then
      begin
        for LAt := 0 to LValues.StorageCount - 1 do
        begin
          LChild.Admit(papConstantStep);

          if Length(LValues.StorageKey(LAt).Value) = Length(LKey.Value) then
          begin
            LChild.Admit(papConstantStep, Length(LKey.Value));

            if LValues.StorageKey(LAt).Value = LKey.Value then
            begin
              Result := LValues.StorageValueAt(LAt);
              Exit;
            end;
          end;
        end;
      end;
      ARequirement := LRequest;
    except
      on E: ENyxPascalAnalysisWorkLimit do
      begin

        if (E.Maximum = AMaximumWork) and (E.WorkBefore = LChild.Receipt.Total) then
          raise ENyxPascalConstantLimit.Create(E.Message);
        raise;
      end;
    end;
  finally
    AWork := LChild.Receipt.Total;
    LChild.Retire;
  end;
end;

function TSemanticTranslation.Source: INyxPascalTranslation;
begin
  Result := Snapshot;
end;

function TSemanticTranslation.WorkTokens: Integer;
begin
  Result := Tokens;
end;

function TSemanticTranslation.TranslationTokens: Integer;
begin
  Result := SourceWork;
end;

function TSemanticTranslation.PrefixTokens: Integer;
begin
  Result := PrefixWork;
end;

function TSemanticTranslation.ConstantSteps: Integer;
begin
  Result := ConstantWork;
end;

function TSemanticTranslation.AnalysisWork: TNyxPascalAnalysisReceipt;
begin
  Result := WorkReceipt;
end;

function TSemanticTranslation.ConsultedWorld: Boolean;
begin
  Result := WorldConsulted;
end;

function TSemanticTranslation.Passes: Integer;
begin
  Result := Rounds;
end;

function TSemanticTranslation.Answers: Integer;
begin
  Result := Accepted;
end;

function TSemanticTranslation.Pending: Boolean;
begin
  Result := HasQuestion;
end;

function TSemanticTranslation.Origin: TNyxPascalConditionOrigin;
begin

  if not HasQuestion then
  begin
    raise ENyxPascalContext.Create('This semantic translation has no pending conditional site');
  end;
  Result := Site;
end;

function TSemanticTranslation.BindingIssue: TNyxPascalBindingIssue;
begin
  Result := Issue;
end;

function TSemanticTranslation.RelatedName: TNyxText;
begin
  Result := Related;
end;

function TSemanticTranslation.RequirementCount: Integer;
begin
  Result := 0;

  if Requirements <> nil then
  begin
    Result := Requirements.RequirementCount;
  end;
end;

function TSemanticTranslation.RequirementAt(AIndex: Integer): INyxPascalCompilerRequirement;
begin

  if Requirements = nil then
  begin
    raise ENyxPascalContext.Create('This conditional result has no compiler requirement');
  end;
  Result := Requirements.RequirementAt(AIndex);
end;

function TSemanticTranslation.StoragePlan: INyxPascalStoragePlan;
begin
  Result := nil;

  if Storage <> nil then
  begin
    Result := Storage.StoragePlan;
  end;
end;

function TSemanticTranslation.StorageNodeCount: Integer;
begin
  Result := 0;

  if Storage <> nil then
  begin
    Result := Storage.StorageNodeCount;
  end;
end;

function TSemanticTranslation.StorageNode(AIndex: Integer): INyxPascalStorageShape;
begin

  if Storage = nil then
  begin
    raise ENyxPascalBinding.Create('This conditional has no complete storage description');
  end;
  Result := Storage.StorageNode(AIndex);
end;

function TSemanticTranslation.StorageContext(AIndex: Integer): INyxPascalCompilerContext;
begin
  StorageNode(AIndex);
  Result := Storage.StorageContext(AIndex);
end;

function TSemanticTranslation.StorageKey: TNyxPascalStorageKey;
var
  LIdentity: INyxPascalConstantStorageIdentity;
begin

  if not Supports(Storage, INyxPascalConstantStorageIdentity, LIdentity) then
  begin
    raise ENyxPascalBinding.Create('No complete pending structural receipt is available');
  end;
  Result := LIdentity.StorageKey;
end;

function TSemanticTranslation.StorageDefinition(AIndex: Integer): INyxPascalBoundSymbol;
begin
  StorageNode(AIndex);
  Result := Storage.StorageDefinition(AIndex);
end;

function TSemanticTranslation.StorageIntrinsicOrigin(AIndex: Integer): TNyxPascalIntrinsicOrigin;
begin
  StorageNode(AIndex);
  Result := Storage.StorageIntrinsicOrigin(AIndex);
end;

function TSemanticTranslation.StorageIntrinsicReference(AIndex: Integer): TNyxPascalSizeReference;
begin
  StorageNode(AIndex);
  Result := Storage.StorageIntrinsicReference(AIndex);
end;

procedure TScopeMeanings.Remember(const AAnswer: INyxPascalResolution; const AName: TNyxText);
begin

  if Issue <> pbiNone then
  begin
    Exit;
  end;
  Requirements := CaptureNyxPascalRequirements(AAnswer);
  Issue := AAnswer.Issue;

  if Issue = pbiNone then
  begin
    Issue := pbiUnsupportedType;
  end;
  Related := AAnswer.RelatedName;

  if Related = '' then
  begin
    Related := AName;
  end;
end;

function TScopeMeanings.IsDeclared(const AName: TNyxPascalDeclarationRef): TNyxPascalKnowledge;
var
  LAnswer: INyxPascalResolution;
begin
  LAnswer := Site.World.Lookup(Site, NyxPascalBindingName(AName.Name), Cancellation);
  Result := ppkUnknown;
  case LAnswer.Status of
    pbsBound, pbsOverloads:
      Result := ppkTrue;
    pbsMissing:
      Result := ppkFalse;
  else
    Remember(LAnswer, AName.Name);
  end;
end;

function TScopeMeanings.SizeOfBytes(const AName: TNyxPascalSizeReference): TNyxPascalStorageFact;
var
  LNamed: TNyxPascalSizeFact;
begin
  ByteSize := NyxPascalStorageFact(psfsUnknown);
  LNamed := SizeOfName(AName);
  Result := ByteSize;

  if LNamed.State = pssKnown then Result := NyxPascalStorageFact(psfsKnown, LNamed.Bytes);
end;

function TScopeMeanings.SizeOfName(const AName: TNyxPascalSizeReference): TNyxPascalSizeFact;
var
  LAnswer: INyxPascalResolution;
  LType: INyxPascalSemanticType;
  LIdentity: INyxPascalIntrinsicIdentity;
  LContext: INyxPascalCompilerContext;
  LValue: INyxPascalConstantResult;
  LStorage: INyxPascalConstantStorage;
  LOptions: TNyxPascalConstantOptions;
  LAt: Integer;
  LBytes: Integer;
begin
  Result := NyxPascalSizeFact(pssUnknown);
  LAnswer := Site.World.Lookup(Site, NyxPascalBindingName(AName.Name), Cancellation);

  if (LAnswer.Status = pbsBound) and (LAnswer.BoundType = nil) and (LAnswer.Count = 1) then
  begin
    LAnswer := Site.World.TypeOfSymbol(LAnswer.SymbolAt(0), Cancellation);
  end;

  if (LAnswer.Status <> pbsBound) or (LAnswer.BoundType = nil) then
  begin
    Remember(LAnswer, AName.Name);
    Exit;
  end;
  LType := LAnswer.BoundType;
  LAt := 0;
  while (LType <> nil) and (LType.Kind in [pstAlias, pstDistinct]) do
  begin
    Inc(LAt);

    if LAt > Site.World.Options.MaximumDepth then
    begin
      raise ENyxPascalBindingLimit.Create('Conditional alias traversal exceeds its depth budget');
    end;
    LType := LType.Target;
  end;

  if (LType <> nil) and (LType.Kind <> pstBuiltin) then
  begin

    if Issue = pbiNone then
    begin
      { The legacy condition parser still owns its comparison/Boolean grammar.
        Delegate only the bound SizeOf query; nested array constants share that
        query's own recursion state and the same aggregate execution budget. }
      LOptions := TNyxPascalConstantOptions.Defaults.InDialect(pcdFPCConditional);
      LValue := PlanNyxPascalConstant('SizeOf(' + AName.Name + ')', LOptions, Cancellation)
        .Evaluate(Site, LOptions, Cancellation);
      Issue := LValue.Issue;
      Related := LValue.RelatedName;
      Requirements := CaptureNyxPascalRequirements(LValue);

      if (LValue.Issue = pbiNone) and (LValue.Value.Kind = pvvInteger) and
        TryStrToInt(String(LValue.Value.Number.ToText), LBytes) and (LBytes >= 0) then
      begin
        { This is already a source-validated conditional value. Do not retag its
          per-node layout provenance as a pristine single-mode size fact. }
        ByteSize := NyxPascalStorageFact(psfsKnown, LBytes);
      end;

      if Supports(LValue, INyxPascalConstantStorage, LStorage) and
        (LStorage.StoragePlan <> nil) then
      begin
        Storage := LStorage;
      end;

      if Related = '' then
      begin
        Related := AName.Name;
      end;
    end;
    Exit;
  end;

  if (LType = nil) or not Supports(LType, INyxPascalIntrinsicIdentity, LIdentity) then
  begin

    if Issue = pbiNone then
    begin
      Issue := pbiUnsupportedType;
      Related := AName.Name;
    end;
    Exit;
  end;
  LContext := LIdentity.IntrinsicContext;

  if ((LIdentity.IntrinsicOrigin = pioSystemBootstrap) <>
    (NyxPascalCompilerScope(LContext) = pcsoSystemBootstrap)) then
  begin

    if Issue = pbiNone then
    begin
      Issue := pbiCompilerContext;
      Related := LIdentity.IntrinsicReference.Name;
    end;
    Exit;
  end;
  Result := NyxPascalCapturedSize(LContext, LIdentity.IntrinsicReference);

  if (Result.State = pssKnown) and
    (Result.ProbeMode = LContext.Mode) and (Result.ProbeLongStrings = LContext.Option('H')) then
  begin
    Exit;
  end;
  Result := NyxPascalSizeFact(pssUnknown);

  if Issue = pbiNone then
  begin
    Issue := pbiCompilerContext;
    Related := LIdentity.IntrinsicReference.Name;
    Requirements := NewNyxPascalRequirements(NyxPascalPrimitiveRequirement(LType, Site));
  end;
end;


procedure TConditionalReader.CheckCancellation;
begin

  if (Cancellation <> nil) and Cancellation.Cancelled then
  begin
    raise ENyxPascalAnalysisCancelled.Create('Semantic conditional analysis cancelled');
  end;
end;

procedure TConditionalReader.AdmitExpression(const AText: TNyxText);
var
  LAt: Integer;
  LScalar: Integer;
begin
  CheckCancellation;
  LAt := 1;
  while LAt <= Length(AText) do
  begin
    Budget.Admit(papConditionInput);

    if not NyxNextScalar(AText, LAt, LScalar) then
    begin
      raise ENyxPascalLexical.Create('Conditional input contains malformed Unicode');
    end;
  end;
end;

procedure TConditionalReader.AdmitToken(ATokensBefore, AViewLimit: Integer);
begin

  if (ATokensBefore <> Data.SourceWork) or (ATokensBefore >= AViewLimit) then
  begin
    raise ENyxPascalTranslationLimit.Create('Conditional token admission lost its single-traversal identity');
  end;
  { The translator admits the source token before invoking this identity guard. }
  Inc(Data.SourceWork);
end;

function TConditionalReader.PreparePrefix(const AOrigin: TNyxPascalConditionOrigin;
  const AContext: INyxPascalCompilerContext; const AExpression: TNyxText): Boolean;
var
  LNeeds: INyxPascalConditionNeeds;
  LDeclarations: INyxPascalConditionDeclarations;
begin
  CheckCancellation;
  Result := False;

  if Data.HasQuestion then
  begin
    Exit;
  end;

  if FPrepared or (AOrigin.PrefixCount <> Data.SourceWork - 1) then
  begin
    raise ENyxPascalTranslationLimit.Create('Conditional prefix preparation lost its ordered source site');
  end;
  Data.HasQuestion := True;
  Data.Site := AOrigin;
  Data.Issue := pbiCompilerContext;
  Data.Related := '';
  Data.Requirements := nil;
  Data.Storage := nil;
  AdmitExpression(AExpression);
  LNeeds := PlanNyxPascalCondition(AContext, AExpression);
  Supports(LNeeds, INyxPascalConditionDeclarations, LDeclarations);
  FLegacy := LNeeds.SyntaxSupported and (LNeeds.SymbolCount = 0) and
    ((LNeeds.SizeCount > 0) or ((LDeclarations <> nil) and
      (LDeclarations.DeclarationCount > 0)));
  FPlan := nil;

  if not FLegacy then
  begin

    if AContext.Family <> ppcFPC then
    begin
      Exit;
    end;
    FPlan := PlanNyxPascalConstant(AExpression,
      TNyxPascalConstantOptions.Defaults.InDialect(pcdFPCConditional), Cancellation);

    if not FPlan.SyntaxSupported then
    begin
      Exit;
    end;
  end;
  FOrigin := AOrigin;
  FContext := AContext;
  FExpression := AExpression;
  FPrepared := True;
  Result := True;
end;

function TConditionalReader.EvaluatePrefix(const AOrigin: TNyxPascalConditionOrigin;
  const AContext: INyxPascalCompilerContext; const AExpression: TNyxText;
  const APrefix: INyxPascalTranslation): TNyxPascalKnowledge;
var
  LView: INyxPascalTranslatedSyntax;
begin
  CheckCancellation;
  Result := ppkUnknown;

  if not FPrepared or (APrefix = nil) or (APrefix.Count <> FOrigin.PrefixCount) or
    (AOrigin.FileReference.Name <> FOrigin.FileReference.Name) or
    (AOrigin.Occurrence <> FOrigin.Occurrence) or
    (AOrigin.Span.Start <> FOrigin.Span.Start) or (AOrigin.Span.Length <> FOrigin.Span.Length) or
    (AOrigin.PrefixCount <> FOrigin.PrefixCount) or
    (AExpression <> FExpression) or not FContext.SameFacts(AContext) then
    raise ENyxPascalTranslationLimit.Create('A frozen conditional prefix lost its exact prepared authority');
  try

    if not APrefix.Resolved then
    begin
      Data.Issue := pbiIncompleteSource;
      Exit;
    end;

    if FProjection <> nil then
      LView := AppendNyxPascalTranslatedSyntax(FProjection, APrefix, SyntaxOptions, Cancellation)
    else
      LView := NewNyxPascalTranslatedSyntax(APrefix, SyntaxOptions, Cancellation);
    Result := EvaluateView(AOrigin, AContext, AExpression, LView);
  finally
    FPrepared := False;
    FContext := nil;
    FExpression := '';
    FPlan := nil;
  end;
end;

procedure TConditionalReader.RememberSite(const AOrigin: TNyxPascalConditionOrigin;
  const AContext: INyxPascalCompilerContext; const AExpression: TNyxText;
  const AView: INyxPascalTranslatedSyntax; ATruth: TNyxPascalKnowledge);
var
  LSite: TReplaySite;
  LTokens, LBytes, LCount: Integer;
begin

  if not Options.CaptureReplay or not Data.ReplayIntact then Exit;
  LCount := Length(Data.Sites);
  LTokens := AView.Source.Count;
  LBytes := AView.Source.Files.ByteCount;

  if (LTokens > Options.MaximumReplayTokens - Data.RetainedTokens) or
    (Length(AExpression) > Options.MaximumReplayTokens - Data.RetainedTokens - LTokens) or
    (LBytes > Options.MaximumReplayCatalogBytes - Data.RetainedBytes) or
    (LCount >= Options.MaximumPasses - 1) or
    (LCount + 1 > Budget.Receipt.Maximum - Budget.Receipt.Total) then
  begin
    Data.DropReplay;
    Exit;
  end;
  Inc(LTokens, Length(AExpression));
  CheckCancellation;
  Budget.Admit(papSourceProof, LCount + 1);
  SetLength(Data.Sites, LCount + 1);
  LSite := TReplaySite.Create;
  Data.Sites[LCount] := LSite;
  LSite.Origin := AOrigin;
  LSite.Context := AContext;
  LSite.Expression := AExpression;
  LSite.Projection := AView;
  LSite.Plan := FPlan;
  LSite.Legacy := FLegacy;
  LSite.Expected := ATruth;
  Inc(Data.RetainedTokens, LTokens);
  Inc(Data.RetainedBytes, LBytes);
end;

function TConditionalReader.EvaluateView(const AOrigin: TNyxPascalConditionOrigin;
  const AContext: INyxPascalCompilerContext; const AExpression: TNyxText;
  const AView: INyxPascalTranslatedSyntax): TNyxPascalKnowledge;
var
  LView: INyxPascalTranslatedSyntax;
  LWorld: INyxPascalSemanticWorld;
  LMeanings: TScopeMeanings;
  LSemanticOwner: INyxPascalConditionSemantics;
  LName: TNyxText;
  LModule: Integer;
  LCaret: Integer;
  LScope: Integer;
  LAnalysis: TNyxPascalConditionResult;
  LOptions: TNyxPascalConstantOptions;
  LValue: INyxPascalConstantResult;
  LRemaining: Integer;
  LStorage: INyxPascalConstantStorage;
  LBoolean: Boolean;
  LSourceScope: TSourceSizeScope;
  LExecution: INyxPascalAnalysisBudget;
begin
  CheckCancellation;
  Result := ppkUnknown;
  LExecution := nil;

  if not FPrepared or (AView = nil) or
    (AView.Source.Count <> FOrigin.PrefixCount) or
    (AOrigin.FileReference.Name <> FOrigin.FileReference.Name) or
    (AOrigin.Occurrence <> FOrigin.Occurrence) or
    (AOrigin.Span.Start <> FOrigin.Span.Start) or
    (AOrigin.Span.Length <> FOrigin.Span.Length) or
    (AOrigin.PrefixCount <> FOrigin.PrefixCount) or
    (AExpression <> FExpression) or not FContext.SameFacts(AContext) then
  begin
    raise ENyxPascalTranslationLimit.Create('A frozen conditional prefix lost its exact prepared authority');
  end;
  try

    if not AView.Source.Resolved then
    begin
      Data.Issue := pbiIncompleteSource;
      Exit;
    end;

    LView := AView;
    FProjection := LView;
    LName := LView.Syntax.Scope(0).Name;

    if (LName = '') or not LView.CompleteTranslation then
    begin
      Data.Issue := pbiIncompleteSource;
      Exit;
    end;
    { An unfinished qualified unit header can still extend its module name.
      Restart from the captured base in that case, avoiding a ghost earlier
      root in the dependency namespace. Ordinary same-name prefixes replace
      only their own module and retain immutable dependency views. }

    { Mark before first admission to the supplied world's namespace. Even an
      unanswered query can depend on absence, incomplete imports or a source
      scope; counting only accepted answers would incorrectly authorize reuse. }
    Data.WorldConsulted := True;

    if (FPrefixWorld <> nil) and (FPrefixModuleName = LName) then
    begin
      LWorld := WithNyxPascalSemanticModule(FPrefixWorld, LView, Cancellation);
    end
    else
    begin
      LWorld := WithNyxPascalSemanticModule(World, LView, Cancellation);
    end;
    FPrefixWorld := LWorld;
    FPrefixModuleName := LName;
    LModule := LWorld.FindModule(LName);
    LCaret := LView.Syntax.Lexical.Scalars;
    LScope := LView.Syntax.ScopeAt(LCaret);
    LMeanings := TScopeMeanings.Create;
    LSemanticOwner := LMeanings;
    LMeanings.Site := LWorld.At(LModule, LScope, LCaret);
    LSourceScope := TSourceSizeScope.Create;
    LExecution := LSourceScope;
    LSourceScope.Budget := Budget;
    LSourceScope.View := LView;
    LSourceScope.SiteOrigin := AOrigin;
    LMeanings.Cancellation := LExecution;

    if FLegacy then
    begin
      AdmitExpression(AExpression);
      LAnalysis := AnalyzeNyxPascalCondition(AContext, AExpression, LSemanticOwner);
      Result := LAnalysis.Truth;
      Data.Issue := LMeanings.Issue;
      Data.Related := LMeanings.Related;
      Data.Requirements := LMeanings.Requirements;
      Data.Storage := LMeanings.Storage;
    end
    else
    begin
      LRemaining := Budget.Receipt.Maximum - Budget.Receipt.Total;

      if LRemaining < 1 then
      begin
        Budget.Admit(papConstantStep);
      end;
      LOptions := TNyxPascalConstantOptions.Defaults.InDialect(pcdFPCConditional);

      if LRemaining < LOptions.MaximumSteps then
      begin
        LOptions := LOptions.UpToSteps(LRemaining);
      end;
      LValue := FPlan.Evaluate(LMeanings.Site, LOptions, LExecution);
      Data.Issue := LValue.Issue;
      Data.Related := LValue.RelatedName;
      Data.Requirements := CaptureNyxPascalRequirements(LValue);

      if Supports(LValue, INyxPascalConstantStorage, LStorage) and
        (LStorage.StoragePlan <> nil) then
      begin
        Data.Storage := LStorage;
      end;

      if LValue.Value.TryTruth(LBoolean) then
      begin
        Result := ppkFalse;

        if LBoolean then
        begin
          Result := ppkTrue;
        end;
      end
      else if Data.Issue = pbiNone then
      begin
        Data.Issue := pbiUnsupportedType;
      end;
    end;

    if Result <> ppkUnknown then
    begin

      if Data.Rounds >= Options.MaximumPasses then
      begin
        raise ENyxPascalTranslationLimit.Create('Ordered conditional refinement exceeds its work budget');
      end;
      RememberSite(AOrigin, AContext, AExpression, LView, Result);
      Inc(Data.Accepted);
      Inc(Data.Rounds);
      Data.HasQuestion := False;
      Data.Issue := pbiNone;
      Data.Related := '';
      Data.Requirements := nil;
      Data.Storage := nil;
    end;
  finally
    if LExecution <> nil then
    begin
      LExecution.Retire;
    end;
    FPrepared := False;
    FContext := nil;
    FExpression := '';
    FPlan := nil;
  end;
end;

function TConditionalReader.Evaluate(const AOrigin: TNyxPascalConditionOrigin;
  const AContext: INyxPascalCompilerContext; const AExpression: TNyxText): TNyxPascalKnowledge;
begin
  Result := ppkUnknown;
  raise ENyxPascalTranslationLimit.Create('This source authority requires its frozen pre-directive prefix');
end;

function NewNyxPascalSemanticTranslation(const AFiles: INyxPascalFileCatalog;
  const ARoot: TNyxPascalPhysicalFile; const AContext: INyxPascalCompilerContext;
  const AWorld: INyxPascalSemanticWorld; const ATranslation: TNyxPascalTranslationOptions;
  const ASyntax: TNyxPascalSyntaxOptions; const AOptions: TNyxPascalConditionalOptions;
  const ACancellation: INyxCancellationScope): INyxPascalSemanticTranslation;
var
  LData: TSemanticTranslation;
  LOwner: INyxPascalSemanticTranslation;
  LReader: TConditionalReader;
  LAuthority: INyxPascalConditionalFacts;
  LBudget: INyxPascalAnalysisBudget;
begin
  AOptions.Validate;
  ATranslation.Validate;
  ASyntax.Validate;
  LData := TSemanticTranslation.Create;
  LOwner := LData;
  LData.Rounds := 1;
  LData.ReplayIntact := AOptions.CaptureReplay;
  LReader := TConditionalReader.Create;
  LAuthority := LReader;
  LReader.Data := LData;
  LReader.World := AWorld;

  if LReader.World = nil then
  begin
    LReader.World := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults
      .WithImplicitScopes(pispCompilerUnits));
  end;
  LReader.Options := AOptions;
  LReader.SyntaxOptions := ASyntax;
  LBudget := NewNyxPascalAnalysisBudget(AOptions.MaximumTokens, ACancellation);
  LReader.Budget := LBudget;
  LReader.Cancellation := LBudget;
  try
    try
      LData.Snapshot := NewNyxPascalTranslation(AFiles, ARoot, AContext,
        ATranslation, LBudget, LAuthority);
    except
      on LLimit: ENyxPascalAnalysisWorkLimit do
      begin
        raise ENyxPascalConditionalWorkLimit.CreateAnalysis(LLimit.Phase, LLimit.WorkBefore,
          LLimit.Requested, LLimit.Maximum, LData.SourceWork, LData.Rounds);
      end;
    end;
    LData.WorkReceipt := LBudget.Receipt;
    LData.Tokens := LData.WorkReceipt.Total;
    LData.ConstantWork := LData.WorkReceipt.Used(papConstantStep);
    LData.PrefixWork := LData.Tokens - LData.SourceWork - LData.ConstantWork;

    if (LData.SourceWork <> LData.Snapshot.Count) or
      (LData.SourceWork <> LData.WorkReceipt.Used(papSourceToken)) then
    begin
      raise ENyxPascalTranslationLimit.Create('Conditional work lost its final source-token identity');
    end;
    Result := LOwner;
  finally
    LBudget.Retire;
  end;
end;

function RebindNyxPascalSemanticTranslation(const APrevious: INyxPascalSemanticTranslation;
  const AFiles: INyxPascalFileCatalog; const AWorld: INyxPascalSemanticWorld;
  const AOptions: TNyxPascalConditionalOptions;
  const ACancellation: INyxCancellationScope): INyxPascalSemanticTranslation;
var
  LPriorOwner, LOwner: INyxPascalSemanticTranslation;
  LPrevious, LData: TSemanticTranslation;
  LReader: TConditionalReader;
  LAuthority: INyxPascalConditionalFacts;
  LBudget: INyxPascalAnalysisBudget;
  LSource: INyxPascalTranslation;
  LView: INyxPascalTranslatedSyntax;
  LSite: TReplaySite;
  LAt: Integer;
  LTruth: TNyxPascalKnowledge;
begin
  { Hold before clearing Result: native interface results can share the caller's
    destination, including an in-place rebind of the previous receipt. }
  LPriorOwner := APrevious;
  Result := nil;
  AOptions.Validate;
  LBudget := NewNyxPascalAnalysisBudget(AOptions.MaximumTokens, ACancellation);
  try
    LBudget.Admit(papSourceProof);

    if not (LPriorOwner is TSemanticTranslation) then Exit;
    LPrevious := LPriorOwner as TSemanticTranslation;

    if not LPrevious.ReplayAvailable then Exit;

    if Length(LPrevious.Sites) >= AOptions.MaximumPasses then
      raise ENyxPascalTranslationLimit.Create('Ordered conditional replay exceeds its unchanged pass limit');
    LSource := RebindNyxPascalTranslation(LPrevious.Snapshot, AFiles, LBudget);

    if LSource = nil then Exit;
    LData := TSemanticTranslation.Create;
    LOwner := LData;
    LData.Rounds := 1;
    LData.ReplayIntact := AOptions.CaptureReplay;
    LAuthority := TConditionalReader.Create;
    LReader := LAuthority as TConditionalReader;
    LReader.Data := LData;
    LReader.World := AWorld;

    if LReader.World = nil then
      LReader.World := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults
        .WithImplicitScopes(pispCompilerUnits));
    LReader.Options := AOptions;
    LReader.Budget := LBudget;
    LReader.Cancellation := LBudget;
    for LAt := 0 to High(LPrevious.Sites) do
    begin
      LBudget.Admit(papSourceProof);
      LSite := LPrevious.Sites[LAt];
      { The saved projection is a hypothesis based only on already checked
        earlier answers. Its original root/include bytes were proved above. }
      LView := RebindNyxPascalFrozenProjection(LSite.Projection, LSource, LBudget);

      if LView = nil then Exit;
      LBudget.Admit(papSourceProof);

      if not LSite.Context.SameFacts(LSource.State(
        LSource.Token(LSite.Origin.PrefixCount).State)) then Exit;
      LReader.FPrepared := True;
      LReader.FOrigin := LSite.Origin;
      LReader.FContext := LSite.Context;
      LReader.FExpression := LSite.Expression;
      LReader.FPlan := LSite.Plan;
      LReader.FLegacy := LSite.Legacy;
      LData.HasQuestion := True;
      LData.Site := LSite.Origin;
      LTruth := LReader.EvaluateView(LSite.Origin, LSite.Context, LSite.Expression, LView);

      if (LTruth = ppkUnknown) or (LTruth <> LSite.Expected) then Exit;
      Inc(LData.Revalidated);
    end;
    LBudget.Admit(papSourceProof);
    LData.Snapshot := LSource;
    LData.WorkReceipt := LBudget.Receipt;
    LData.Tokens := LData.WorkReceipt.Total;
    LData.ConstantWork := LData.WorkReceipt.Used(papConstantStep);
    LData.PrefixWork := LData.Tokens - LData.ConstantWork;
    { No source tokens were translated during replay. Actual proof, world and
      evaluator work remains charged; no old cold-work total is fabricated. }
    Result := LOwner;
  finally
    LBudget.Retire;
  end;
end;

end.
