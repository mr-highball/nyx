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

uses SysUtils, nyx.pascal.lexing;

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
  TSemanticTranslation = class(TInterfacedObject, INyxPascalSemanticTranslation)
  public
    Snapshot: INyxPascalTranslation;
    Tokens: Integer;
    Rounds: Integer;
    Accepted: Integer;
    HasQuestion: Boolean;
    Site: TNyxPascalConditionOrigin;
    Issue: TNyxPascalBindingIssue;
    Related: TNyxText;
    function Source: INyxPascalTranslation;
    function WorkTokens: Integer;
    function Passes: Integer;
    function Answers: Integer;
    function Pending: Boolean;
    function Origin: TNyxPascalConditionOrigin;
    function BindingIssue: TNyxPascalBindingIssue;
    function RelatedName: TNyxText;
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
    procedure Remember(const AAnswer: INyxPascalResolution; const AName: TNyxText);
    function IsDeclared(const AName: TNyxPascalDeclarationRef): TNyxPascalKnowledge;
    function SizeOfName(const AName: TNyxPascalSizeReference): TNyxPascalSizeFact;
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

procedure TScopeMeanings.Remember(const AAnswer: INyxPascalResolution; const AName: TNyxText);
begin

  if Issue <> pbiNone then
  begin
    Exit;
  end;
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
  Issue := pbiCompilerContext;
  Related := LIdentity.IntrinsicReference.Name;
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
    LResult.Snapshot := NewNyxPascalTranslation(LFiles, ARoot, LContext,
      LPassOptions, LCancellation, LAuthority);
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
    LNeeds := PlanNyxPascalCondition(LCache.Question.Context, LCache.Question.Expression);
    Supports(LNeeds, INyxPascalConditionDeclarations, LDeclarations);

    if not LNeeds.SyntaxSupported or (LNeeds.SymbolCount > 0) or
      ((LNeeds.SizeCount = 0) and (LDeclarations.DeclarationCount = 0)) then
    begin
      Break;
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
    LScope := 0;

    if LCaret > 0 then
    begin
      LScope := LView.Syntax.ScopeAt(LCaret - 1);
    end;
    LMeanings := TScopeMeanings.Create;
    LSemanticOwner := LMeanings;
    LMeanings.Site := LPrefixWorld.At(LModule, LScope, LCaret);
    LMeanings.Cancellation := LCancellation;
    LAnalysis := AnalyzeNyxPascalCondition(LCache.Question.Context,
      LCache.Question.Expression, LSemanticOwner);

    if LAnalysis.Truth = ppkUnknown then
    begin
      LResult.Issue := LMeanings.Issue;
      LResult.Related := LMeanings.Related;
      Break;
    end;
    LCache.Accept(LAnalysis.Truth);
    LResult.HasQuestion := False;
    LResult.Issue := pbiNone;
    LResult.Related := '';
    LSemanticOwner := nil;
    LPrefixWorld := nil;
    LView := nil;
    LPrefix := nil;
  end;
  Result := LOwner;
end;

end.
