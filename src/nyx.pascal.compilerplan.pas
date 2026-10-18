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
unit nyx.pascal.compilerplan;
{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.scheduler, nyx.pascal.lexing, nyx.pascal.context,
  nyx.pascal.translation, nyx.pascal.compilertrace;

type
  { Independent positive work/output quotas. Optional lookahead proposes only
    compiler-symbol queries from retained directive rows, never source meaning.
    Its work charge includes input/visited rows and candidate expression scalars.
    Refusal is atomic and never increases the translation's resource limits. }
  TNyxPascalCompilerPlanOptions = record
  private
    FDefined: Boolean;
    FConditions: Integer;
    FTokens: Integer;
    FLookahead: Boolean;
  public
    class function Defaults: TNyxPascalCompilerPlanOptions; static;
    function UpToConditions(AValue: Integer): TNyxPascalCompilerPlanOptions;
    function UpToTokens(AValue: Integer): TNyxPascalCompilerPlanOptions;
    function LookAheadConditions(AValue: Boolean): TNyxPascalCompilerPlanOptions;
    procedure Validate;
    property MaximumConditions: Integer read FConditions;
    property MaximumTokens: Integer read FTokens;
    property IncludeConditionalLookahead: Boolean read FLookahead;
  end;
  { Frozen unknown-condition requirements tied to exact physical occurrences and
    pre-token states. Needs retains separate compiler symbols, source sizeof and
    declared names; unsupported conditions never produce guessed compiler queries.
    The source lease owns bytes/context, with no editor/process/callback borrow. }
  INyxPascalCompilerPlan = interface(IInterface)
    ['{814416FA-B8F7-45DC-B6AC-83D29D238904}']
    function Source: INyxPascalTranslation;
    function Count: Integer;
    function Origin(AIndex: Integer): TNyxPascalConditionOrigin;
    function State(AIndex: Integer): Integer;
    function Expression(AIndex: Integer): TNyxText;
    function Needs(AIndex: Integer): INyxPascalConditionNeeds;
    { Freeze the base and add only compiler symbols missing from the pristine
      initial packet. Source-local uncertainty cannot be repaired by re-querying
      an already captured pristine name. Preserve base order/size requests and
      give observed needs priority within the existing 256-symbol job quota. }
    function CompilerProbe(const ABase: INyxPascalCompilerProbe): INyxPascalCompilerProbe;
  end;
  { Optional bounded proposals beyond actual expanded requirements. Candidates
    are not requirements or verified facts; the real compiler must measure them.
    CompilerProbe gives observed requirements priority within its existing quota.
    WorkUnits counts input rows plus lookahead rows/expression scalars. }
  INyxPascalCompilerCandidates = interface(IInterface)
    ['{F2C43341-73F5-4B1A-81B0-324519854A73}']
    function CandidateCount: Integer;
    function CandidateAt(AIndex: Integer): TNyxPascalDefineRef;
    function CandidatesTruncated: Boolean;
    function WorkUnits: Integer;
  end;

{ Inspect only retained unknown conditions and their owning translated rows,
  including repeated includes and already expanded expressions. Exact pre-token
  states feed the existing condition planner. Invalid correspondence,
  excess work/output or cancellation refuses the whole candidate. }
function PlanNyxPascalCompilerRequirements(const ASource: INyxPascalTranslation;
  const AOptions: TNyxPascalCompilerPlanOptions;
  const ACancellation: INyxCancellationScope = nil): INyxPascalCompilerPlan;

implementation

uses SysUtils, nyx.text.index, nyx.editing;

type
  { Construction owns detached vectors and a temporary cancellation lease.
    Published needs are separately owned interfaces in a class vector, never
    interface fields in records. Source data points only toward immutable inputs. }
  TCompilerPlan = class(TInterfacedObject, INyxPascalCompilerPlan, INyxPascalCompilerCandidates)
  private
    FSource: INyxPascalTranslation;
    FOrigins: array of TNyxPascalConditionOrigin;
    FStates: array of Integer;
    FExpressions: array of TNyxText;
    FNeeds: array of INyxPascalConditionNeeds;
    FCandidates: array of TNyxPascalDefineRef;
    FTruncated: Boolean;
    FWork: Integer;
    procedure CheckIndex(AIndex: Integer);
  public
    constructor Create(const ASource: INyxPascalTranslation;
      const AOptions: TNyxPascalCompilerPlanOptions; const ACancellation: INyxCancellationScope);
    function Source: INyxPascalTranslation;
    function Count: Integer;
    function Origin(AIndex: Integer): TNyxPascalConditionOrigin;
    function State(AIndex: Integer): Integer;
    function Expression(AIndex: Integer): TNyxText;
    function Needs(AIndex: Integer): INyxPascalConditionNeeds;
    function CompilerProbe(const ABase: INyxPascalCompilerProbe): INyxPascalCompilerProbe;
    function CandidateCount: Integer;
    function CandidateAt(AIndex: Integer): TNyxPascalDefineRef;
    function CandidatesTruncated: Boolean;
    function WorkUnits: Integer;
  end;

class function TNyxPascalCompilerPlanOptions.Defaults: TNyxPascalCompilerPlanOptions;
begin
  Result := Default(TNyxPascalCompilerPlanOptions);
  Result.FDefined := True;
  Result.FConditions := 1024;
  Result.FTokens := 1000000;
end;

procedure TNyxPascalCompilerPlanOptions.Validate;
begin

  if not FDefined or (FConditions < 1) or (FTokens < 1) then
  begin
    raise ENyxPascalContext.Create('Use positive compiler requirement planning limits');
  end;
end;

function TNyxPascalCompilerPlanOptions.UpToConditions(AValue: Integer): TNyxPascalCompilerPlanOptions;
begin
  Result := Self;
  Result.FConditions := AValue;
  Result.Validate;
end;

function TNyxPascalCompilerPlanOptions.UpToTokens(AValue: Integer): TNyxPascalCompilerPlanOptions;
begin
  Result := Self;
  Result.FTokens := AValue;
  Result.Validate;
end;

function TNyxPascalCompilerPlanOptions.LookAheadConditions(
  AValue: Boolean): TNyxPascalCompilerPlanOptions;
begin
  Result := Self;
  Result.FLookahead := AValue;
  Result.Validate;
end;

procedure CheckCancellation(const AValue: INyxCancellationScope);
begin

  if (AValue <> nil) and AValue.Cancelled then
  begin
    raise ENyxPascalAnalysisCancelled.Create('Compiler requirement planning cancelled');
  end;
end;

constructor TCompilerPlan.Create(const ASource: INyxPascalTranslation;
  const AOptions: TNyxPascalCompilerPlanOptions; const ACancellation: INyxCancellationScope);
var
  LInputs: INyxPascalUnresolvedConditions;
  LAt: Integer;
  LCount: Integer;
  LInput: TNyxPascalUnresolvedCondition;
  LRow: TNyxPascalTranslatedToken;
  LRequiredRows: array of Boolean;
  LToken: TNyxPascalToken;
  LName: TNyxText;
  LValue: TNyxText;
  LOffset: Integer;
  LNames: TNyxTextIndex;
  LNeeds: INyxPascalConditionNeeds;
  LSymbol: Integer;
  LCandidate: TNyxPascalDefineRef;

  procedure Charge(AValue: Integer);
  begin

    if (AValue < 0) or (AValue > AOptions.MaximumTokens - FWork) then
    begin
      raise ENyxPascalTranslationLimit.Create('Compiler candidate lookahead exceeds its work budget');
    end;
    Inc(FWork, AValue);
  end;
begin
  inherited Create;
  AOptions.Validate;
  CheckCancellation(ACancellation);

  if ASource = nil then
  begin
    raise ENyxPascalContext.Create('Compiler requirements need their immutable source translation');
  end;
  FSource := ASource;

  if (FSource.Count < 0) or (FSource.Count > AOptions.MaximumTokens) then
  begin
    raise ENyxPascalTranslationLimit.Create('Compiler requirement input exceeds its work budget');
  end;
  FWork := FSource.Count;

  if AOptions.IncludeConditionalLookahead then
  begin
    SetLength(LRequiredRows, FSource.Count);
  end;

  if not Supports(FSource, INyxPascalUnresolvedConditions, LInputs) then
  begin
    raise ENyxPascalContext.Create('Compiler requirements need retained ordered condition inputs');
  end;
  LCount := LInputs.UnresolvedCount;

  if (LCount < 0) or (LCount > AOptions.MaximumConditions) then
  begin
    raise ENyxPascalTranslationLimit.Create('Compiler requirement conditions exceed their output budget');
  end;
  SetLength(FOrigins, LCount);
  SetLength(FStates, LCount);
  SetLength(FExpressions, LCount);
  SetLength(FNeeds, LCount);
  for LAt := 0 to LCount - 1 do
  begin
    CheckCancellation(ACancellation);
    LInput := LInputs.UnresolvedAt(LAt);

    if (LInput.Origin.PrefixCount < 0) or (LInput.Origin.PrefixCount >= FSource.Count) or
      (LInput.State < 0) or (LInput.State >= FSource.StateCount) then
    begin
      raise ENyxPascalContext.Create('An unresolved condition has invalid source ownership');
    end;
    LRow := FSource.Token(LInput.Origin.PrefixCount);

    if (LRow.State <> LInput.State) or (LRow.Occurrence <> LInput.Origin.Occurrence) or
      (FSource.PhysicalToken(LInput.Origin.PrefixCount).Span.Start <> LInput.Origin.Span.Start) or
      (FSource.PhysicalToken(LInput.Origin.PrefixCount).Span.Length <> LInput.Origin.Span.Length) or
      (FSource.Files.FileAt(FSource.Occurrence(LRow.Occurrence).FileIndex).Name <>
        LInput.Origin.FileReference.Name) then
    begin
      raise ENyxPascalContext.Create('An unresolved condition differs from its owning source row');
    end;
    FOrigins[LAt] := LInput.Origin;

    if AOptions.IncludeConditionalLookahead then
    begin
      LRequiredRows[LInput.Origin.PrefixCount] := True;
    end;
    FStates[LAt] := LInput.State;
    FExpressions[LAt] := LInput.Expression;

    if LInput.Plannable then
    begin
      FNeeds[LAt] := PlanNyxPascalCondition(FSource.State(LInput.State), LInput.Expression);
    end
    else
    begin
      { Failed expansion/IFOPT cannot be recast as an ordinary name query. An
        explicitly unsupported plan retains the original source expression. }
      FNeeds[LAt] := PlanNyxPascalCondition(FSource.State(LInput.State), '');
    end;
  end;

  if not AOptions.IncludeConditionalLookahead then
  begin
    Exit;
  end;
  LNames := TNyxTextIndex.Create;
  try
    for LAt := 0 to FSource.Count - 1 do
    begin
      CheckCancellation(ACancellation);
      Charge(1);
      LRow := FSource.Token(LAt);

      if LRequiredRows[LAt] or (LRow.Expansion >= 0) then
      begin
        Continue;
      end;
      LToken := FSource.PhysicalToken(LAt);

      if (LToken.Kind <> ptkDirective) or not LToken.Complete then
      begin
        Continue;
      end;
      NyxPascalDirectiveParts(FSource.Text(LAt), LName, LValue, LOffset);

      if not ((LName = 'if') or (LName = 'elseif') or
        (LName = 'ifdef') or (LName = 'ifndef')) then
      begin
        Continue;
      end;
      Charge(NyxTextScalarCount(LValue));

      if (LName = 'ifdef') or (LName = 'ifndef') then
      begin
        try
          LCandidate := NyxPascalDefine(LValue);
        except
          on ENyxPascalContext do
          begin
            Continue;
          end;
        end;
        LValue := 'defined(' + LCandidate.Name + ')';
      end;
      LNeeds := PlanNyxPascalCondition(FSource.State(LRow.State), LValue);

      if not LNeeds.SyntaxSupported then
      begin
        Continue;
      end;
      for LSymbol := 0 to LNeeds.SymbolCount - 1 do
      begin
        LCandidate := LNeeds.Symbol(LSymbol);

        if (FSource.InitialContext.Symbol(LCandidate).Kind <> ppdUnknown) or
          (LNames.IndexOf(LCandidate.Name) >= 0) then
        begin
          Continue;
        end;

        if Length(FCandidates) = 256 then
        begin
          FTruncated := True;
          Exit;
        end;
        LNames.AddFirst(LCandidate.Name, Length(FCandidates));
        SetLength(FCandidates, Length(FCandidates) + 1);
        FCandidates[High(FCandidates)] := LCandidate;
      end;
    end;
  finally
    LNames.Free;
  end;
end;

function TCompilerPlan.CandidateCount: Integer;
begin
  Result := Length(FCandidates);
end;

function TCompilerPlan.CandidateAt(AIndex: Integer): TNyxPascalDefineRef;
begin

  if (AIndex < 0) or (AIndex >= Length(FCandidates)) then
  begin
    raise ENyxPascalContext.Create('Compiler candidate index is outside its exact plan');
  end;
  Result := FCandidates[AIndex];
end;

function TCompilerPlan.CandidatesTruncated: Boolean;
begin
  Result := FTruncated;
end;

function TCompilerPlan.WorkUnits: Integer;
begin
  Result := FWork;
end;

procedure TCompilerPlan.CheckIndex(AIndex: Integer);
begin

  if (AIndex < 0) or (AIndex >= Count) then
  begin
    raise ENyxPascalContext.Create('Compiler condition index is outside its source plan');
  end;
end;

function TCompilerPlan.Source: INyxPascalTranslation;
begin
  Result := FSource;
end;

function TCompilerPlan.Count: Integer;
begin
  Result := Length(FOrigins);
end;

function TCompilerPlan.Origin(AIndex: Integer): TNyxPascalConditionOrigin;
begin
  CheckIndex(AIndex);
  Result := FOrigins[AIndex];
end;

function TCompilerPlan.State(AIndex: Integer): Integer;
begin
  CheckIndex(AIndex);
  Result := FStates[AIndex];
end;

function TCompilerPlan.Expression(AIndex: Integer): TNyxText;
begin
  CheckIndex(AIndex);
  Result := FExpressions[AIndex];
end;

function TCompilerPlan.Needs(AIndex: Integer): INyxPascalConditionNeeds;
begin
  CheckIndex(AIndex);
  Result := FNeeds[AIndex];
end;

function TCompilerPlan.CompilerProbe(const ABase: INyxPascalCompilerProbe): INyxPascalCompilerProbe;
var
  LAt: Integer;
  LSymbol: Integer;
begin
  Result := CaptureNyxFPCCompilerProbe(ABase);
  for LAt := 0 to Count - 1 do
  begin
    for LSymbol := 0 to FNeeds[LAt].SymbolCount - 1 do
    begin

      if FSource.InitialContext.Symbol(FNeeds[LAt].Symbol(LSymbol)).Kind = ppdUnknown then
      begin
        Result := Result.WithSymbol(FNeeds[LAt].Symbol(LSymbol));
      end;
    end;
  end;
  for LAt := 0 to High(FCandidates) do
  begin

    if Result.Count >= 256 then
    begin
      Break;
    end;
    Result := Result.WithSymbol(FCandidates[LAt]);
  end;
end;

function PlanNyxPascalCompilerRequirements(const ASource: INyxPascalTranslation;
  const AOptions: TNyxPascalCompilerPlanOptions;
  const ACancellation: INyxCancellationScope): INyxPascalCompilerPlan;
begin
  Result := TCompilerPlan.Create(ASource, AOptions, ACancellation);
end;

end.
