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
  { Independent positive work/output quotas. Refusal publishes no partial plan;
    these values never increase the owning translation's own resource limits. }
  TNyxPascalCompilerPlanOptions = record
  private
    FDefined: Boolean;
    FConditions: Integer;
    FTokens: Integer;
  public
    class function Defaults: TNyxPascalCompilerPlanOptions; static;
    function UpToConditions(AValue: Integer): TNyxPascalCompilerPlanOptions;
    function UpToTokens(AValue: Integer): TNyxPascalCompilerPlanOptions;
    procedure Validate;
    property MaximumConditions: Integer read FConditions;
    property MaximumTokens: Integer read FTokens;
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
    { Freeze the base probe and add compiler symbols only, preserving first-use
      order and its existing size requests. Its 256-symbol quota still applies.
      This method launches nothing and makes no authored-scope size assertion. }
    function CompilerProbe(const ABase: INyxPascalCompilerProbe): INyxPascalCompilerProbe;
  end;

{ Inspect only retained unknown conditions and their owning translated rows,
  including repeated includes and already expanded expressions. Exact pre-token
  states feed the existing condition planner. Invalid correspondence,
  excess work/output or cancellation refuses the whole candidate. }
function PlanNyxPascalCompilerRequirements(const ASource: INyxPascalTranslation;
  const AOptions: TNyxPascalCompilerPlanOptions;
  const ACancellation: INyxCancellationScope = nil): INyxPascalCompilerPlan;

implementation

uses SysUtils;

type
  { Construction owns detached vectors and a temporary cancellation lease.
    Published needs are separately owned interfaces in a class vector, never
    interface fields in records. Source data points only toward immutable inputs. }
  TCompilerPlan = class(TInterfacedObject, INyxPascalCompilerPlan)
  private
    FSource: INyxPascalTranslation;
    FOrigins: array of TNyxPascalConditionOrigin;
    FStates: array of Integer;
    FExpressions: array of TNyxText;
    FNeeds: array of INyxPascalConditionNeeds;
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
begin
  inherited Create;
  AOptions.Validate;
  CheckCancellation(ACancellation);

  if ASource = nil then
  begin
    raise ENyxPascalContext.Create('Compiler requirements need their immutable source translation');
  end;
  FSource := ASource;

  if FSource.Count > AOptions.MaximumTokens then
  begin
    raise ENyxPascalTranslationLimit.Create('Compiler requirement input exceeds its work budget');
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
      Result := Result.WithSymbol(FNeeds[LAt].Symbol(LSymbol));
    end;
  end;
end;

function PlanNyxPascalCompilerRequirements(const ASource: INyxPascalTranslation;
  const AOptions: TNyxPascalCompilerPlanOptions;
  const ACancellation: INyxCancellationScope): INyxPascalCompilerPlan;
begin
  Result := TCompilerPlan.Create(ASource, AOptions, ACancellation);
end;

end.
