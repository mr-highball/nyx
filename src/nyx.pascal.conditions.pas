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
    ordinary translation diagnostics. Work counts every pass and parsed prefix;
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

{ Refine the first unknown conditional using only its already translated prefix.
  Every accepted answer is replayed at the exact site/expression/context on the
  next pass over the same frozen input. No authored text or closing syntax is
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
  { One sealed source query/answer. Context is owned in this class, never in a
    record/static interface vector. It retains no translation, world or callback. }
  TConditionalAnswer = class
    Origin: TNyxPascalConditionOrigin;
    Context: INyxPascalCompilerContext;
    Expression: TNyxText;
    Truth: TNyxPascalKnowledge;
  end;
  { Construction-only replay authority. Earlier decisions are immutable and
    checked in order. The first unanswered query is owned once; later unknown
    branches cannot donate declarations to that earlier query. }
  TConditionalReplay = class(TInterfacedObject, INyxPascalConditionalFacts)
  public
    Values: array of TConditionalAnswer;
    Next: Integer;
    Question: TConditionalAnswer;
    destructor Destroy; override;
    procedure BeginPass;
    procedure EndPass;
    procedure Accept(ATruth: TNyxPascalKnowledge);
    function Evaluate(const AOrigin: TNyxPascalConditionOrigin;
      const AContext: INyxPascalCompilerContext; const AExpression: TNyxText): TNyxPascalKnowledge;
  end;
  { Owns final data only. Publication happens after construction/checks; failure
    cleanup releases the source and all temporary authorities independently. }
  TSemanticTranslation = class(TInterfacedObject, INyxPascalSemanticTranslation,
    INyxPascalCompilerRequirements)
  public
    Snapshot: INyxPascalTranslation;
    Tokens: Integer;
    Rounds: Integer;
    Accepted: Integer;
    HasQuestion: Boolean;
    Site: TNyxPascalConditionOrigin;
    Issue: TNyxPascalBindingIssue;
    Related: TNyxText;
    Requirements: INyxPascalCompilerRequirements;
    function Source: INyxPascalTranslation;
    function WorkTokens: Integer;
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

destructor TConditionalReplay.Destroy;
var
  LAt: Integer;
begin
  Question.Free;
  for LAt := 0 to High(Values) do
  begin
    Values[LAt].Free;
  end;
  inherited Destroy;
end;

procedure TConditionalReplay.BeginPass;
begin
  Question.Free;
  Question := nil;
  Next := 0;
end;

procedure TConditionalReplay.EndPass;
begin

  if Next <> Length(Values) then
  begin
    raise ENyxPascalTranslationLimit.Create('A sealed conditional answer was not consumed in order');
  end;
end;

procedure TConditionalReplay.Accept(ATruth: TNyxPascalKnowledge);
var
  LAt: Integer;
begin

  if (Question = nil) or not (ATruth in [ppkFalse, ppkTrue]) then
  begin
    raise ENyxPascalTranslationLimit.Create('Only a known owned conditional answer can be sealed');
  end;
  Question.Truth := ATruth;
  LAt := Length(Values);
  SetLength(Values, LAt + 1);
  Values[LAt] := Question;
  Question := nil;
end;

function TConditionalReplay.Evaluate(const AOrigin: TNyxPascalConditionOrigin;
  const AContext: INyxPascalCompilerContext; const AExpression: TNyxText): TNyxPascalKnowledge;
var
  LExpected: TConditionalAnswer;
begin
  Result := ppkUnknown;

  if Next < Length(Values) then
  begin
    LExpected := Values[Next];

    if (LExpected.Origin.FileReference.Name <> AOrigin.FileReference.Name) or
      (LExpected.Origin.Occurrence <> AOrigin.Occurrence) or
      (LExpected.Origin.Span.Start <> AOrigin.Span.Start) or
      (LExpected.Origin.Span.Length <> AOrigin.Span.Length) or
      (LExpected.Origin.PrefixCount <> AOrigin.PrefixCount) or
      (LExpected.Expression <> AExpression) or not LExpected.Context.SameFacts(AContext) then
    begin
      raise ENyxPascalTranslationLimit.Create('A sealed conditional answer lost exact source authority');
    end;
    Inc(Next);
    Exit(LExpected.Truth);
  end;

  if Question = nil then
  begin
    Question := TConditionalAnswer.Create;
    Question.Origin := AOrigin;
    Question.Context := AContext;
    Question.Expression := AExpression;
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

function NewNyxPascalSemanticTranslation(const AFiles: INyxPascalFileCatalog;
  const ARoot: TNyxPascalPhysicalFile; const AContext: INyxPascalCompilerContext;
  const AWorld: INyxPascalSemanticWorld; const ATranslation: TNyxPascalTranslationOptions;
  const ASyntax: TNyxPascalSyntaxOptions; const AOptions: TNyxPascalConditionalOptions;
  const ACancellation: INyxCancellationScope): INyxPascalSemanticTranslation;
var
  LFiles: INyxPascalFileCatalog;
  LContext: INyxPascalCompilerContext;
  LWorld: INyxPascalSemanticWorld;
  LPrefixWorld: INyxPascalSemanticWorld;
  LPrefix: INyxPascalTranslation;
  LView: INyxPascalTranslatedSyntax;
  LCache: TConditionalReplay;
  LAuthority: INyxPascalConditionalFacts;
  LResult: TSemanticTranslation;
  LOwner: INyxPascalSemanticTranslation;
  LMeanings: TScopeMeanings;
  LSemanticOwner: INyxPascalConditionSemantics;
  LNeeds: INyxPascalConditionNeeds;
  LDeclarations: INyxPascalConditionDeclarations;
  LAnalysis: TNyxPascalConditionResult;
  LName: TNyxText;
  LModule: Integer;
  LCaret: Integer;
  LScope: Integer;
  LRemaining: Integer;
  LPassOptions: TNyxPascalTranslationOptions;
  LCancellation: INyxCancellationScope;
  LLegacyEligible: Boolean;
  LConstantPlan: INyxPascalConstantPlan;
  LConstantResult: INyxPascalConstantResult;
  LConstantOptions: TNyxPascalConstantOptions;
  LTruth: TNyxPascalKnowledge;
  LBoolean: Boolean;
begin
  AOptions.Validate;
  ATranslation.Validate;
  ASyntax.Validate;
  LCancellation := ACancellation;
  LFiles := CaptureNyxPascalFiles(AFiles);
  LContext := CaptureNyxPascalCompilerContext(AContext);
  LWorld := AWorld;

  if LWorld = nil then
  begin
    LWorld := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults
      .WithImplicitScopes(pispCompilerUnits));
  end;
  LCache := TConditionalReplay.Create;
  LAuthority := LCache;
  LResult := TSemanticTranslation.Create;
  LOwner := LResult;
  while True do
  begin

    if (LCancellation <> nil) and LCancellation.Cancelled then
    begin
      raise ENyxPascalAnalysisCancelled.Create('Semantic conditional analysis cancelled');
    end;
    LRemaining := AOptions.MaximumTokens - LResult.Tokens;

    if (LResult.Rounds >= AOptions.MaximumPasses) or (LRemaining < 1) then
    begin
      raise ENyxPascalTranslationLimit.Create('Ordered conditional refinement exceeds its work budget');
    end;
    LPassOptions := ATranslation;

    if LRemaining < ATranslation.MaximumTokens then
    begin
      LPassOptions := LPassOptions.UpToTokens(LRemaining);
    end;
    LCache.BeginPass;
    try
      LResult.Snapshot := NewNyxPascalTranslation(LFiles, ARoot, LContext,
        LPassOptions, LCancellation, LAuthority);
    except
      on LLimit: ENyxPascalTokenLimit do
      begin
        { Copy evidence before the RTL releases the original exception. A failed
          pass still publishes no source/result and charges no invented work. }
        raise ENyxPascalConditionalTokenLimit.Create(LLimit.TokenCount,
          LLimit.TokenLimit, LResult.Tokens, AOptions.MaximumTokens,
          ATranslation.MaximumTokens, LResult.Rounds + 1);
      end;
    end;
    Inc(LResult.Tokens, LResult.Snapshot.Count);
    Inc(LResult.Rounds);
    LCache.EndPass;
    LResult.Accepted := Length(LCache.Values);

    if LCache.Question = nil then
    begin
      Break;
    end;
    LResult.HasQuestion := True;
    LResult.Site := LCache.Question.Origin;
    LResult.Issue := pbiCompilerContext;
    LResult.Requirements := nil;
    LNeeds := PlanNyxPascalCondition(LCache.Question.Context, LCache.Question.Expression);
    Supports(LNeeds, INyxPascalConditionDeclarations, LDeclarations);

    LLegacyEligible := LNeeds.SyntaxSupported and (LNeeds.SymbolCount = 0) and
      ((LNeeds.SizeCount > 0) or (LDeclarations.DeclarationCount > 0));
    LConstantPlan := nil;

    if not LLegacyEligible then
    begin

      if LCache.Question.Context.Family <> ppcFPC then
      begin
        Break;
      end;

      { A constant-expression plan supplies a separate typed source authority.
        Unsupported syntax still stops here; no compiler symbol becomes a
        declaration value merely because an earlier reader could not parse it. }
      LConstantPlan := PlanNyxPascalConstant(LCache.Question.Expression,
        TNyxPascalConstantOptions.Defaults.InDialect(pcdFPCConditional), LCancellation);

      if not LConstantPlan.SyntaxSupported then
      begin
        Break;
      end;
    end;

    if LResult.Site.PrefixCount > AOptions.MaximumTokens - LResult.Tokens then
    begin
      raise ENyxPascalTranslationLimit.Create('Conditional syntax prefixes exceed the aggregate token budget');
    end;
    Inc(LResult.Tokens, LResult.Site.PrefixCount);
    LPrefix := NewNyxPascalTranslationPrefix(LResult.Snapshot,
      LResult.Site.PrefixCount, ATranslation, LCancellation);

    if not LPrefix.Resolved then
    begin
      LResult.Issue := pbiIncompleteSource;
      Break;
    end;
    LView := NewNyxPascalTranslatedSyntax(LPrefix, ASyntax, LCancellation);
    LName := LView.Syntax.Scope(0).Name;

    if (LName = '') or not LView.CompleteTranslation then
    begin
      LResult.Issue := pbiIncompleteSource;
      Break;
    end;
    LPrefixWorld := LWorld.WithModule(LView);
    LModule := LPrefixWorld.FindModule(LName);
    LCaret := LView.Syntax.Lexical.Scalars;
    { EOF belongs to an unfinished scope, including a newly opened empty body.
      Moving back one scalar could select its outer scope or a finished method. }
    LScope := LView.Syntax.ScopeAt(LCaret);
    LMeanings := TScopeMeanings.Create;
    LSemanticOwner := LMeanings;
    LMeanings.Site := LPrefixWorld.At(LModule, LScope, LCaret);
    LMeanings.Cancellation := LCancellation;
    LTruth := ppkUnknown;

    if LLegacyEligible then
    begin
      LAnalysis := AnalyzeNyxPascalCondition(LCache.Question.Context,
        LCache.Question.Expression, LSemanticOwner);
      LTruth := LAnalysis.Truth;
      LResult.Issue := LMeanings.Issue;
      LResult.Related := LMeanings.Related;
      LResult.Requirements := LMeanings.Requirements;
    end
    else
    begin
      LRemaining := AOptions.MaximumTokens - LResult.Tokens;

      if LRemaining < 1 then
      begin
        raise ENyxPascalTranslationLimit.Create('Constant conditions exhaust the aggregate work budget');
      end;
      LConstantOptions := TNyxPascalConstantOptions.Defaults.InDialect(pcdFPCConditional);

      if LConstantOptions.MaximumSteps > LRemaining then
      begin
        LConstantOptions := LConstantOptions.UpToSteps(LRemaining);
      end;
      LConstantResult := LConstantPlan.Evaluate(LMeanings.Site, LConstantOptions, LCancellation);
      Inc(LResult.Tokens, LConstantResult.WorkUnits);
      LResult.Issue := LConstantResult.Issue;
      LResult.Related := LConstantResult.RelatedName;
      LResult.Requirements := CaptureNyxPascalRequirements(LConstantResult);

      if LConstantResult.Value.TryTruth(LBoolean) then
      begin
        LTruth := ppkFalse;

        if LBoolean then
        begin
          LTruth := ppkTrue;
        end;
      end
      else if LResult.Issue = pbiNone then
      begin
        LResult.Issue := pbiUnsupportedType;
      end;
    end;

    if LTruth = ppkUnknown then
    begin
      Break;
    end;
    LCache.Accept(LTruth);
    LResult.HasQuestion := False;
    LResult.Issue := pbiNone;
    LResult.Related := '';
    LResult.Requirements := nil;
    LSemanticOwner := nil;
    LPrefixWorld := nil;
    LView := nil;
    LPrefix := nil;
    LConstantResult := nil;
    LConstantPlan := nil;
  end;
  Result := LOwner;
end;

end.
