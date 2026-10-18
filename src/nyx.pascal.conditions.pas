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
  nyx.pascal.binding;

type
  { Stage whose next real work could not be admitted. A source token, frozen/
    parsed prefix token and constant-evaluation step use the same aggregate
    allowance; none is discounted merely because its source was seen earlier. }
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
  public
    { Counts are nonnegative and the aggregate maximum is positive. Requested
      work exceeds the remaining allowance; the refinement round is one-based. }
    constructor Create(APhase: TNyxPascalConditionalWorkPhase; AWorkBefore,
      ARequested, AMaximum, ATranslated, ARound: Integer); reintroduce;
    property Phase: TNyxPascalConditionalWorkPhase read FPhase;
    property WorkBefore: Integer read FWorkBefore;
    property Requested: Integer read FRequested;
    property Maximum: Integer read FMaximum;
    property Translated: Integer read FTranslated;
    property RefinementRound: Integer read FRound;
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
  public
    { Default: 64 ordered passes and four million total translation/prefix
      tokens. This is bounded refinement, not an incremental-performance claim. }
    class function Defaults: TNyxPascalConditionalOptions; static;
    function UpToPasses(AValue: Integer): TNyxPascalConditionalOptions;
    function UpToTokens(AValue: Integer): TNyxPascalConditionalOptions;
    procedure Validate;
    property MaximumPasses: Integer read FPasses;
    property MaximumTokens: Integer read FTokens;
  end;
  { Detached construction receipt. Source owns the exact final frozen files and
    ordinary translation diagnostics. Work counts newly translated tokens,
    frozen/parsed prefixes and constant steps. Passes retains the logical
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
  { Detached actual admission totals. TranslationTokens counts newly stored
    source tokens once, PrefixTokens counts each frozen/parsed analysis prefix,
    and ConstantSteps counts the evaluator's admitted steps. Their sum equals
    WorkTokens. No timing, incremental-edit or compiler-execution claim follows. }
  INyxPascalSemanticWork = interface(IInterface)
    ['{E861FCCE-26B8-4CE1-AC36-C4717D79272B}']
    function TranslationTokens: Integer;
    function PrefixTokens: Integer;
    function ConstantSteps: Integer;
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

implementation

uses SysUtils, nyx.pascal.lexing, nyx.pascal.constants, nyx.pascal.values,
  nyx.pascal.requirements;

type
  { Owns final data only. Publication happens after construction/checks; failure
    cleanup releases the source and all temporary authorities independently. }
  TSemanticTranslation = class(TInterfacedObject, INyxPascalSemanticTranslation,
    INyxPascalCompilerRequirements, INyxPascalSemanticWork)
  public
    Snapshot: INyxPascalTranslation;
    Tokens: Integer;
    SourceWork: Integer;
    PrefixWork: Integer;
    ConstantWork: Integer;
    Rounds: Integer;
    Accepted: Integer;
    HasQuestion: Boolean;
    Site: TNyxPascalConditionOrigin;
    Issue: TNyxPascalBindingIssue;
    Related: TNyxText;
    Requirements: INyxPascalCompilerRequirements;
    function Source: INyxPascalTranslation;
    function WorkTokens: Integer;
    function TranslationTokens: Integer;
    function PrefixTokens: Integer;
    function ConstantSteps: Integer;
    function Passes: Integer;
    function Answers: Integer;
    function Pending: Boolean;
    function Origin: TNyxPascalConditionOrigin;
    function BindingIssue: TNyxPascalBindingIssue;
    function RelatedName: TNyxText;
    function RequirementCount: Integer;
    function RequirementAt(AIndex: Integer): INyxPascalCompilerRequirement;
  end;
  { A synchronous site-bound semantic authority. The site owns its prefix world
    outward; no result retains this object. Named size resolution follows bound
    aliases to a captured intrinsic with matching measurement mode/H policy.
    General layouts/expressions remain unknown, never inferred from a host ABI. }
  TScopeMeanings = class(TInterfacedObject, INyxPascalConditionSemantics)
  public
    Site: INyxPascalBindingSite;
    Cancellation: INyxCancellationScope;
    Issue: TNyxPascalBindingIssue;
    Related: TNyxText;
    Requirements: INyxPascalCompilerRequirements;
    procedure Remember(const AAnswer: INyxPascalResolution; const AName: TNyxText);
    function IsDeclared(const AName: TNyxPascalDeclarationRef): TNyxPascalKnowledge;
    function SizeOfName(const AName: TNyxPascalSizeReference): TNyxPascalSizeFact;
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
    procedure CheckCancellation;
    procedure Admit(AWork: Integer; APhase: TNyxPascalConditionalWorkPhase);
  public
    Data: TSemanticTranslation;
    World: INyxPascalSemanticWorld;
    Options: TNyxPascalConditionalOptions;
    SyntaxOptions: TNyxPascalSyntaxOptions;
    Cancellation: INyxCancellationScope;
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
end;

procedure TNyxPascalConditionalOptions.Validate;
begin

  if not FDefined or (FPasses < 1) or (FTokens < 1) then
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

function TScopeMeanings.SizeOfName(const AName: TNyxPascalSizeReference): TNyxPascalSizeFact;
var
  LAnswer: INyxPascalResolution;
  LType: INyxPascalSemanticType;
  LIdentity: INyxPascalIntrinsicIdentity;
  LContext: INyxPascalCompilerContext;
  LAt: Integer;
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

  if (LType = nil) or (LType.Kind <> pstBuiltin) or
    not Supports(LType, INyxPascalIntrinsicIdentity, LIdentity) then
  begin
    Issue := pbiUnsupportedType;
    Related := AName.Name;
    Exit;
  end;
  LContext := LIdentity.IntrinsicContext;

  if ((LIdentity.IntrinsicOrigin = pioSystemBootstrap) <>
    (NyxPascalCompilerScope(LContext) = pcsoSystemBootstrap)) then
  begin
    Issue := pbiCompilerContext;
    Related := LIdentity.IntrinsicReference.Name;
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

procedure TConditionalReader.Admit(AWork: Integer; APhase: TNyxPascalConditionalWorkPhase);
begin
  CheckCancellation;

  if AWork < 0 then
  begin
    raise ENyxPascalContext.Create('Conditional work cannot be negative');
  end;

  if AWork > Options.MaximumTokens - Data.Tokens then
  begin
    raise ENyxPascalConditionalWorkLimit.Create(APhase, Data.Tokens,
      AWork, Options.MaximumTokens, Data.SourceWork, Data.Rounds);
  end;
  Inc(Data.Tokens, AWork);
  case APhase of
    pcwpTranslation: Inc(Data.SourceWork, AWork);
    pcwpPrefix: Inc(Data.PrefixWork, AWork);
    pcwpConstant: Inc(Data.ConstantWork, AWork);
  end;
end;

procedure TConditionalReader.AdmitToken(ATokensBefore, AViewLimit: Integer);
begin

  if (ATokensBefore <> Data.SourceWork) or (ATokensBefore >= AViewLimit) then
  begin
    raise ENyxPascalTranslationLimit.Create('Conditional token admission lost its single-traversal identity');
  end;
  Admit(1, pcwpTranslation);
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
  Admit(AOrigin.PrefixCount, pcwpPrefix);
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
  LBoolean: Boolean;
begin
  CheckCancellation;
  Result := ppkUnknown;

  if not FPrepared or (APrefix = nil) or
    (APrefix.Count <> FOrigin.PrefixCount) or
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

    if not APrefix.Resolved then
    begin
      Data.Issue := pbiIncompleteSource;
      Exit;
    end;
    LView := NewNyxPascalTranslatedSyntax(APrefix, SyntaxOptions, Cancellation);
    LName := LView.Syntax.Scope(0).Name;

    if (LName = '') or not LView.CompleteTranslation then
    begin
      Data.Issue := pbiIncompleteSource;
      Exit;
    end;
    LWorld := World.WithModule(LView);
    LModule := LWorld.FindModule(LName);
    LCaret := LView.Syntax.Lexical.Scalars;
    LScope := LView.Syntax.ScopeAt(LCaret);
    LMeanings := TScopeMeanings.Create;
    LSemanticOwner := LMeanings;
    LMeanings.Site := LWorld.At(LModule, LScope, LCaret);
    LMeanings.Cancellation := Cancellation;

    if FLegacy then
    begin
      LAnalysis := AnalyzeNyxPascalCondition(AContext, AExpression, LSemanticOwner);
      Result := LAnalysis.Truth;
      Data.Issue := LMeanings.Issue;
      Data.Related := LMeanings.Related;
      Data.Requirements := LMeanings.Requirements;
    end
    else
    begin
      LRemaining := Options.MaximumTokens - Data.Tokens;

      if LRemaining < 1 then
      begin
        raise ENyxPascalConditionalWorkLimit.Create(pcwpConstant, Data.Tokens,
          1, Options.MaximumTokens, Data.SourceWork, Data.Rounds);
      end;
      LOptions := TNyxPascalConstantOptions.Defaults.InDialect(pcdFPCConditional);

      if LRemaining < LOptions.MaximumSteps then
      begin
        LOptions := LOptions.UpToSteps(LRemaining);
      end;
      LValue := FPlan.Evaluate(LMeanings.Site, LOptions, Cancellation);
      Admit(LValue.WorkUnits, pcwpConstant);
      Data.Issue := LValue.Issue;
      Data.Related := LValue.RelatedName;
      Data.Requirements := CaptureNyxPascalRequirements(LValue);

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
      Inc(Data.Accepted);
      Inc(Data.Rounds);
      Data.HasQuestion := False;
      Data.Issue := pbiNone;
      Data.Related := '';
      Data.Requirements := nil;
    end;
  finally
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
begin
  AOptions.Validate;
  ATranslation.Validate;
  ASyntax.Validate;
  LData := TSemanticTranslation.Create;
  LOwner := LData;
  LData.Rounds := 1;
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
  LReader.Cancellation := ACancellation;
  LData.Snapshot := NewNyxPascalTranslation(AFiles, ARoot, AContext,
    ATranslation, ACancellation, LAuthority);

  if LData.SourceWork <> LData.Snapshot.Count then
  begin
    raise ENyxPascalTranslationLimit.Create('Conditional work lost its final source-token identity');
  end;
  Result := LOwner;
end;

end.
