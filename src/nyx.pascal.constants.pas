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
unit nyx.pascal.constants;
{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.text, nyx.scheduler, nyx.pascal.lexing, nyx.pascal.context,
  nyx.pascal.syntax, nyx.pascal.types, nyx.pascal.binding, nyx.pascal.values,
  nyx.pascal.requirements;

type
  { Exhaustion/cancellation refuses the complete value query; semantic unknown
    remains a normal result. No partial dependency list is published on failure. }
  ENyxPascalConstantLimit = class(ENyxPascalBindingLimit);
  { Ordinary source expressions and FPC's separate preprocessor evaluator have
    different intrinsic/radix rules. A plan captures one explicit dialect;
    nested declaration initializers always use ordinary Pascal semantics. }
  TNyxPascalConstantDialect = (pcdPascal, pcdFPCConditional);
  { Positive copied work limits for parsing and recursive declaration evaluation.
    Defaults retain 512 tokens, 16 KiB per expression, 64 levels and 100000 work
    units, with at most 256 distinct declaration dependencies. Units count tokens,
    value visits and dependency work; each binder call additionally obeys the
    immutable world's own query budget. This is not a total CPU-time guarantee. }
  TNyxPascalConstantOptions = record
  private
    FDefined: Boolean;
    FSteps: Integer;
    FDepth: Integer;
    FDialect: TNyxPascalConstantDialect;
  public
    class function Defaults: TNyxPascalConstantOptions; static;
    function UpToSteps(AValue: Integer): TNyxPascalConstantOptions;
    function UpToDepth(AValue: Integer): TNyxPascalConstantOptions;
    function InDialect(AValue: TNyxPascalConstantDialect): TNyxPascalConstantOptions;
    procedure Validate;
    property MaximumSteps: Integer read FSteps;
    property MaximumDepth: Integer read FDepth;
    property Dialect: TNyxPascalConstantDialect read FDialect;
  end;
  { Detached exact result and typed refusal. Dependencies retain their immutable
    source world outward only; no editor, parser, query or reverse link survives.
    Value Unknown does not establish a false condition or compiler admission. }
  INyxPascalConstantResult = interface(IInterface)
    ['{B64C0231-97B9-4C76-90A6-B4713FB4C760}']
    function Value: TNyxPascalConstantValue;
    function Issue: TNyxPascalBindingIssue;
    function RelatedName: TNyxText;
    function WorkUnits: Integer;
    function DependencyCount: Integer;
    function Dependency(AIndex: Integer): INyxPascalBoundSymbol;
  end;
  { Immutable parsed expression, independent of any application/editor world.
    SyntaxSupported describes this bounded grammar, not value availability.
    Evaluation resolves names at the supplied exact site; nested constants use
    their declaration sites. Plans retain original text but no query or source-world lease. }
  INyxPascalConstantPlan = interface(IInterface)
    ['{D54FBE56-0A70-43F7-BD63-E71A0D584E17}']
    function SyntaxSupported: Boolean;
    function SourceText: TNyxText;
    function Evaluate(const ASite: INyxPascalBindingSite;
      const AOptions: TNyxPascalConstantOptions;
      const ACancellation: INyxCancellationScope = nil): INyxPascalConstantResult;
  end;

{ Parse exact integer/Boolean syntax, named constants and Low/High/SizeOf/
  Declared/Defined names. Width-sensitive bitwise/cast/string/real/call meanings
  remain explicit unknowns. Plans own no mutable producer or host object. }
function PlanNyxPascalConstant(const AExpression: TNyxText;
  const AOptions: TNyxPascalConstantOptions;
  const ACancellation: INyxCancellationScope = nil): INyxPascalConstantPlan;

implementation

uses nyx.data, nyx.numbers.decimal, nyx.pascal.syntax.translated;

type
  { Closed AST operation; qualified names remain distinct from intrinsic calls.
    Array indexing, casts and ordinary function invocation are not evaluated. }
  TConstantNodeKind = (cnValue, cnName, cnPositive, cnNegative, cnNot, cnBinary,
    cnLow, cnHigh, cnSize, cnDeclared, cnDefined, cnOrd);
  { Value-only node with snapshot-local child ordinals. Literal radix remains
    syntax provenance, allowing evaluation in its actual compiler context.
    No interface fields or target pointer survives in a plan's copied arena. }
  TConstantNode = record
    Kind: TConstantNodeKind;
    Literal: TNyxPascalConstantValue;
    LiteralBase: TNyxPascalNumberBase;
    Name: TNyxText;
    Left: Integer;
    Right: Integer;
    Operation: TNyxPascalValueOperation;
  end;
  { Owns only a lexical snapshot and value-node arena. Parsing is construction
    local; evaluation creates an independent bounded query for each invocation. }
  TConstantPlan = class(TInterfacedObject, INyxPascalConstantPlan)
  private
    FLexical: INyxPascalLexicalSnapshot;
    FNodes: array of TConstantNode;
    FAt: Integer;
    FDepth: Integer;
    FRoot: Integer;
    FSupported: Boolean;
    FOptions: TNyxPascalConstantOptions;
    FCancellation: INyxCancellationScope;
    function Tok: TNyxPascalToken;
    procedure Step;
    function Add(AKind: TConstantNodeKind): Integer;
    function Primary: Integer;
    function Expression(AMinimum: Integer): Integer;
    function Precedence(out AOperation: TNyxPascalValueOperation): Integer;
  public
    constructor Create(const AText: TNyxText; const AOptions: TNyxPascalConstantOptions;
      const ACancellation: INyxCancellationScope);
    function SyntaxSupported: Boolean;
    function SourceText: TNyxText;
    function Evaluate(const ASite: INyxPascalBindingSite;
      const AOptions: TNyxPascalConstantOptions;
      const ACancellation: INyxCancellationScope): INyxPascalConstantResult;
  end;
  { Final owned result. Only the query writes these fields before publication;
    checked dependency access cannot expose mutable vectors or a live evaluator. }
  TConstantResult = class(TInterfacedObject, INyxPascalConstantResult, INyxPascalCompilerRequirements)
  public
    Answer: TNyxPascalConstantValue;
    Reason: TNyxPascalBindingIssue;
    Related: TNyxText;
    Work: Integer;
    Dependencies: array of INyxPascalBoundSymbol;
    Requirements: INyxPascalCompilerRequirements;
    function Value: TNyxPascalConstantValue;
    function Issue: TNyxPascalBindingIssue;
    function RelatedName: TNyxText;
    function WorkUnits: Integer;
    function DependencyCount: Integer;
    function Dependency(AIndex: Integer): INyxPascalBoundSymbol;
    function RequirementCount: Integer;
    function RequirementAt(AIndex: Integer): INyxPascalCompilerRequirement;
  end;
  { Synchronous bounded source query. Active declaration keys detect recursion;
    result dependencies own immutable worlds without pointing back to this owner. }
  TConstantQuery = class
  private
    FOptions: TNyxPascalConstantOptions;
    FCancellation: INyxCancellationScope;
    FResult: TConstantResult;
    FDepth: Integer;
    FActive: array of TNyxText;
    procedure Tick(AWork: Integer = 1);
    procedure Refuse(AIssue: TNyxPascalBindingIssue; const AName: TNyxText = '');
    procedure RefuseBinding(const AAnswer: INyxPascalResolution; const AName: TNyxText);
    procedure RequirePrimitiveSize(const AType: INyxPascalSemanticType;
      const ASite: INyxPascalBindingSite);
    procedure Depend(const ASymbol: INyxPascalBoundSymbol);
    function ContextAt(const ASite: INyxPascalBindingSite): INyxPascalCompilerContext;
    function Resolve(const ASite: INyxPascalBindingSite; const AName: TNyxText): INyxPascalResolution;
    function DeclarationValue(const ASymbol: INyxPascalBoundSymbol): TNyxPascalConstantValue;
    function TextValue(const ASite: INyxPascalBindingSite; const AText: TNyxText): TNyxPascalConstantValue;
    function NodeValue(APlan: TConstantPlan; ANode: Integer;
      const ASite: INyxPascalBindingSite): TNyxPascalConstantValue;
    function BoundType(const ASite: INyxPascalBindingSite; const AName: TNyxText;
      out ASource: INyxPascalBoundSymbol): INyxPascalSemanticType;
    function OrdinalValue(const AType: INyxPascalSemanticType;
      const ASource: INyxPascalBoundSymbol; AHigh: Boolean;
      const AMember: TNyxText = ''): TNyxPascalConstantValue;
    function SizeValue(const AType: INyxPascalSemanticType): TNyxPascalConstantValue;
  public
    function Run(APlan: TConstantPlan; const ASite: INyxPascalBindingSite;
      const AOptions: TNyxPascalConstantOptions;
      const ACancellation: INyxCancellationScope): INyxPascalConstantResult;
  end;

class function TNyxPascalConstantOptions.Defaults: TNyxPascalConstantOptions;
begin
  Result.FDefined := True;
  Result.FSteps := 100000;
  Result.FDepth := 64;
  Result.FDialect := pcdPascal;
end;

procedure TNyxPascalConstantOptions.Validate;
begin

  if not FDefined or (FSteps < 1) or (FDepth < 1) or
    (Ord(FDialect) < Ord(Low(TNyxPascalConstantDialect))) or
    (Ord(FDialect) > Ord(High(TNyxPascalConstantDialect))) then
  begin
    raise ENyxPascalConstantLimit.Create('Use explicit positive constant work limits');
  end;
end;

function TNyxPascalConstantOptions.UpToSteps(AValue: Integer): TNyxPascalConstantOptions;
begin
  Result := Self;
  Result.FSteps := AValue;
  Result.Validate;
end;

function TNyxPascalConstantOptions.UpToDepth(AValue: Integer): TNyxPascalConstantOptions;
begin
  Result := Self;
  Result.FDepth := AValue;
  Result.Validate;
end;

function TNyxPascalConstantOptions.InDialect(
  AValue: TNyxPascalConstantDialect): TNyxPascalConstantOptions;
begin
  Result := Self;
  Result.FDialect := AValue;
  Result.Validate;
end;

function TConstantPlan.Tok: TNyxPascalToken;
begin
  Result := FLexical.Token(FAt);
end;

procedure TConstantPlan.Step;
begin
  repeat

    if (FCancellation <> nil) and FCancellation.Cancelled then
    begin
      raise ENyxPascalAnalysisCancelled.Create('Constant expression parsing cancelled');
    end;

    if Tok.Kind = ptkEOF then
    begin
      Exit;
    end;
    Inc(FAt);
  until not (Tok.Kind in [ptkWhitespace, ptkComment]);
end;

function TConstantPlan.Add(AKind: TConstantNodeKind): Integer;
begin

  if Length(FNodes) >= 512 then
  begin
    raise ENyxPascalConstantLimit.Create('Constant expression nodes exceed their budget');
  end;
  Result := Length(FNodes);
  SetLength(FNodes, Result + 1);
  FNodes[Result] := Default(TConstantNode);
  FNodes[Result].Kind := AKind;
  FNodes[Result].Left := -1;
  FNodes[Result].Right := -1;
end;

function TConstantPlan.Primary: Integer;
var
  LKind: TConstantNodeKind;
  LName: TNyxText;
  LAt: Integer;
  LBase: Integer;
  LEscaped: Boolean;
begin
  Inc(FDepth);

  if FDepth > FOptions.MaximumDepth then
  begin
    raise ENyxPascalConstantLimit.Create('Constant expression parsing exceeds its depth budget');
  end;
  try
    Result := Add(cnValue);

    if (Tok.Kind = ptkSymbol) and (Tok.Symbol = psoOpenParen) then
    begin
      Step;
      Result := Expression(0);

      if (Tok.Kind <> ptkSymbol) or (Tok.Symbol <> psoCloseParen) then
      begin
        FSupported := False;
        Exit;
      end;
      Step;
    end
    else if ((Tok.Kind = ptkSymbol) and (Tok.Symbol in [psoPlus, psoMinus])) or
      ((Tok.Kind = ptkWord) and (Tok.Word = pkwNot) and not Tok.Escaped) then
    begin
      LKind := cnPositive;

      if Tok.Symbol = psoMinus then
      begin
        LKind := cnNegative;
      end;

      if Tok.Word = pkwNot then
      begin
        LKind := cnNot;
      end;
      FNodes[Result].Kind := LKind;
      Step;
      LAt := Primary;
      FNodes[Result].Left := LAt;
    end
    else if (Tok.Kind = ptkNumber) and Tok.Complete and not Tok.RealNumber then
    begin
      LBase := 10;
      case Tok.NumberBase of
        pnbHexadecimal:
          begin
            LBase := 16;
          end;
        pnbBinary:
          begin
            LBase := 2;
          end;
        pnbOctal:
          begin
            LBase := 8;
          end;
      else
        begin
          LBase := 10;
        end;
      end;

      if (FOptions.Dialect = pcdFPCConditional) and (LBase = 10) and
        (Length(FLexical.Text(FAt)) > 1) and
        (FLexical.Text(FAt)[1] = '0') then
      begin
        { Directive strings with noncanonical decimal spelling have separate
          compiler rules; do not bypass the original conditional reader. }
        FSupported := False;
      end;
      try
        FNodes[Result].LiteralBase := Tok.NumberBase;
        FNodes[Result].Literal := NyxPascalIntegerToken(FLexical.Text(FAt), LBase);

        if (FOptions.Dialect = pcdFPCConditional) and (LBase <> 10) and
          (FNodes[Result].Literal.Number.CompareTo(TNyxDecimalNumber.FromDecimal(
            NyxDecimal('9223372036854775807'))) > 0) then
        begin
          { FPC scanner.try_parse_number first uses signed-64 Val. Radix
            literals with bit 63 set therefore differ from decimal UInt64. }
          FNodes[Result].Literal := TNyxPascalConstantValue.ExactInteger(
            FNodes[Result].Literal.Number.Subtract(TNyxDecimalNumber.FromDecimal(
              NyxDecimal('18446744073709551616'))));
        end;
      except
        on ENyxPascalValue do
        begin
          FSupported := False;
        end;
        on ENyxDecimalNumber do
        begin
          FSupported := False;
        end;
      end;
      Step;
    end
    else if Tok.Kind = ptkWord then
    begin
      LEscaped := Tok.Escaped;
      LName := NyxPascalASCIIKey(FLexical.Identifier(FAt));
      Step;

      if not LEscaped and ((LName = 'true') or (LName = 'false')) then
      begin
        FNodes[Result].Literal := TNyxPascalConstantValue.BooleanValue(LName = 'true');
        Exit;
      end;
      LKind := cnName;

      if not LEscaped and (Tok.Kind = ptkSymbol) and (Tok.Symbol = psoOpenParen) then
      begin

        if LName = 'ord' then
        begin
          FNodes[Result].Kind := cnOrd;
          Step;
          LAt := Expression(0);
          FNodes[Result].Left := LAt;

          if (Tok.Kind <> ptkSymbol) or (Tok.Symbol <> psoCloseParen) then
          begin
            FSupported := False;
            Exit;
          end;
          Step;
          Exit;
        end;

        if LName = 'low' then
        begin
          LKind := cnLow;

          if FOptions.Dialect = pcdFPCConditional then
          begin
            { The pinned FPC preprocessor recognizes HIGH, not LOW. Ordinary
              Pascal initializers still support both through this same plan. }
            FSupported := False;
          end;
        end
        else if LName = 'high' then
        begin
          LKind := cnHigh;
        end
        else if LName = 'sizeof' then
        begin
          LKind := cnSize;
        end
        else if LName = 'declared' then
        begin
          LKind := cnDeclared;
        end
        else if LName = 'defined' then
        begin
          LKind := cnDefined;
        end
        else
        begin
          FSupported := False;
          Exit;
        end;
        Step;

        if Tok.Kind <> ptkWord then
        begin
          FSupported := False;
          Exit;
        end;

        if (LKind = cnDefined) and Tok.Escaped then
        begin
          FSupported := False;
        end;
        LName := NyxPascalASCIIKey(FLexical.Identifier(FAt));
        Step;
      end;
      while (Tok.Kind = ptkSymbol) and (Tok.Symbol = psoDot) do
      begin
        Step;

        if Tok.Kind <> ptkWord then
        begin
          FSupported := False;
          Exit;
        end;
        LName := LName + '.' + NyxPascalASCIIKey(FLexical.Identifier(FAt));
        Step;
      end;
      FNodes[Result].Kind := LKind;
      FNodes[Result].Name := LName;
      try

        if LKind = cnDefined then
        begin
          NyxPascalDefine(LName);
        end
        else
        begin
          NyxPascalBindingName(LName);
        end;
      except
        on ENyxPascalContext do
        begin
          FSupported := False;
        end;
        on ENyxPascalBinding do
        begin
          FSupported := False;
        end;
      end;

      if LKind <> cnName then
      begin

        if (Tok.Kind <> ptkSymbol) or (Tok.Symbol <> psoCloseParen) then
        begin
          FSupported := False;
          Exit;
        end;
        Step;
      end;
    end
    else
    begin
      FSupported := False;
      Step;
    end;
  finally
    Dec(FDepth);
  end;
end;

function TConstantPlan.Precedence(out AOperation: TNyxPascalValueOperation): Integer;
begin
  Result := -1;
  AOperation := pvoAdd;

  if Tok.Kind = ptkSymbol then
  begin
    case Tok.Symbol of
      psoPlus:
        begin
          AOperation := pvoAdd;
          Result := 30;
        end;
      psoMinus:
        begin
          AOperation := pvoSubtract;
          Result := 30;
        end;
      psoStar:
        begin
          AOperation := pvoMultiply;
          Result := 40;
        end;
      psoShiftLeft:
        begin

          if FOptions.Dialect = pcdPascal then
          begin
            AOperation := pvoShiftLeft64;
            Result := 40;
          end;
        end;
      psoShiftRight:
        begin

          if FOptions.Dialect = pcdPascal then
          begin
            AOperation := pvoShiftRight64;
            Result := 40;
          end;
        end;
      psoEqual:
        begin
          AOperation := pvoEqual;
          Result := 10;
        end;
      psoNotEqual:
        begin
          AOperation := pvoNotEqual;
          Result := 10;
        end;
      psoLess:
        begin
          AOperation := pvoLess;
          Result := 10;
        end;
      psoLessEqual:
        begin
          AOperation := pvoLessEqual;
          Result := 10;
        end;
      psoGreater:
        begin
          AOperation := pvoGreater;
          Result := 10;
        end;
      psoGreaterEqual:
        begin
          AOperation := pvoGreaterEqual;
          Result := 10;
        end;
    else
      begin
        Result := -1;
      end;
    end;
  end
  else if (Tok.Kind = ptkWord) and not Tok.Escaped then
  begin
    case Tok.Word of
      pkwDiv:
        begin
          AOperation := pvoDivide;
          Result := 40;
        end;
      pkwMod:
        begin
          AOperation := pvoRemainder;
          Result := 40;
        end;
      pkwShl:
        begin
          AOperation := pvoShiftLeft64;
          Result := 40;
        end;
      pkwShr:
        begin
          AOperation := pvoShiftRight64;
          Result := 40;
        end;
      pkwAnd:
        begin
          AOperation := pvoAnd;
          Result := 40;
        end;
      pkwOr:
        begin
          AOperation := pvoOr;
          Result := 30;
        end;
      pkwXor:
        begin
          AOperation := pvoXor;
          Result := 30;
        end;
    else
      begin
        Result := -1;
      end;
    end;
  end;
end;

function TConstantPlan.Expression(AMinimum: Integer): Integer;
var
  LOperation: TNyxPascalValueOperation;
  LPrecedence: Integer;
  LParent: Integer;
  LRight: Integer;
begin
  Result := Primary;
  LPrecedence := Precedence(LOperation);
  while LPrecedence >= AMinimum do
  begin
    Step;
    LParent := Add(cnBinary);
    FNodes[LParent].Operation := LOperation;
    FNodes[LParent].Left := Result;
    LRight := Expression(LPrecedence + 1);
    FNodes[LParent].Right := LRight;
    Result := LParent;
    LPrecedence := Precedence(LOperation);
  end;
end;

constructor TConstantPlan.Create(const AText: TNyxText;
  const AOptions: TNyxPascalConstantOptions; const ACancellation: INyxCancellationScope);
begin
  inherited Create;
  AOptions.Validate;
  FOptions := AOptions;
  FCancellation := ACancellation;
  try
    FLexical := NewNyxPascalLexicalSnapshot(AText,
      TNyxPascalLexicalOptions.Defaults.UpToSourceBytes(16384).UpToTokens(512), ACancellation);

    if FLexical.Count > AOptions.MaximumSteps then
    begin
      raise ENyxPascalConstantLimit.Create('Constant expression input exceeds its work budget');
    end;
    FSupported := FLexical.DiagnosticCount = 0;

    if Tok.Kind in [ptkWhitespace, ptkComment] then
    begin
      Step;
    end;
    FRoot := Expression(0);
    FSupported := FSupported and (Tok.Kind = ptkEOF);
  finally
    FCancellation := nil;
  end;
end;

function TConstantPlan.SyntaxSupported: Boolean;
begin
  Result := FSupported;
end;

function TConstantPlan.SourceText: TNyxText;
begin
  Result := FLexical.Source;
end;

function TConstantPlan.Evaluate(const ASite: INyxPascalBindingSite;
  const AOptions: TNyxPascalConstantOptions;
  const ACancellation: INyxCancellationScope): INyxPascalConstantResult;
var
  LQuery: TConstantQuery;
begin
  LQuery := TConstantQuery.Create;
  try
    Result := LQuery.Run(Self, ASite, AOptions, ACancellation);
  finally
    LQuery.Free;
  end;
end;

function TConstantResult.Value: TNyxPascalConstantValue;
begin
  Result := Answer;
end;
function TConstantResult.Issue: TNyxPascalBindingIssue;
begin
  Result := Reason;
end;
function TConstantResult.RelatedName: TNyxText;
begin
  Result := Related;
end;
function TConstantResult.WorkUnits: Integer;
begin
  Result := Work;
end;
function TConstantResult.DependencyCount: Integer;
begin
  Result := Length(Dependencies);
end;

function TConstantResult.RequirementCount: Integer;
begin
  Result := 0;

  if Requirements <> nil then
  begin
    Result := Requirements.RequirementCount;
  end;
end;

function TConstantResult.RequirementAt(AIndex: Integer): INyxPascalCompilerRequirement;
begin

  if Requirements = nil then
  begin
    raise ENyxPascalContext.Create('This constant result has no compiler requirement');
  end;
  Result := Requirements.RequirementAt(AIndex);
end;
function TConstantResult.Dependency(AIndex: Integer): INyxPascalBoundSymbol;
begin

  if (AIndex < 0) or (AIndex >= Length(Dependencies)) then
  begin
    raise ENyxPascalBinding.Create('A constant dependency index is outside its result');
  end;
  Result := Dependencies[AIndex];
end;

procedure TConstantQuery.Tick(AWork: Integer);
begin

  if (FCancellation <> nil) and FCancellation.Cancelled then
  begin
    raise ENyxPascalAnalysisCancelled.Create('Constant source evaluation cancelled');
  end;

  if (AWork < 0) or (AWork > FOptions.MaximumSteps - FResult.Work) then
  begin
    raise ENyxPascalConstantLimit.Create('Constant source evaluation exceeds its work budget');
  end;
  Inc(FResult.Work, AWork);
end;

procedure TConstantQuery.Refuse(AIssue: TNyxPascalBindingIssue; const AName: TNyxText);
begin

  if FResult.Reason = pbiNone then
  begin
    FResult.Reason := AIssue;

    if FResult.Reason = pbiNone then
    begin
      FResult.Reason := pbiUnsupportedType;
    end;
    FResult.Related := AName;
  end;
end;

procedure TConstantQuery.Depend(const ASymbol: INyxPascalBoundSymbol);
var
  LAt: Integer;
begin

  if ASymbol = nil then
  begin
    Exit;
  end;
  for LAt := 0 to High(FResult.Dependencies) do
  begin
    Tick;

    if (FResult.Dependencies[LAt].World = ASymbol.World) and
      (FResult.Dependencies[LAt].ModuleIndex = ASymbol.ModuleIndex) and
      (FResult.Dependencies[LAt].DeclarationIndex = ASymbol.DeclarationIndex) then
    begin
      Exit;
    end;
  end;

  if Length(FResult.Dependencies) >= 256 then
  begin
    raise ENyxPascalConstantLimit.Create('Constant source dependencies exceed their output budget');
  end;
  SetLength(FResult.Dependencies, Length(FResult.Dependencies) + 1);
  FResult.Dependencies[High(FResult.Dependencies)] := ASymbol;
end;

procedure TConstantQuery.RefuseBinding(const AAnswer: INyxPascalResolution;
  const AName: TNyxText);
var
  LRelated: TNyxText;
begin

  if FResult.Reason = pbiNone then
  begin
    FResult.Requirements := CaptureNyxPascalRequirements(AAnswer);
  end;
  LRelated := AAnswer.RelatedName;

  if LRelated = '' then
  begin
    LRelated := AName;
  end;
  Refuse(AAnswer.Issue, LRelated);
end;

function TConstantQuery.ContextAt(const ASite: INyxPascalBindingSite): INyxPascalCompilerContext;
var
  LView: INyxPascalTranslatedSyntax;
  LOrigin: Integer;
begin
  LView := ASite.World.ModuleAt(ASite.ModuleIndex);
  LOrigin := LView.OriginAt(ASite.Caret);
  Result := nil;

  if LOrigin >= 0 then
  begin
    Result := LView.Source.State(LView.Origin(LOrigin).State);
  end
  else if (ASite.Caret = LView.Syntax.Lexical.Scalars) and LView.CompleteTranslation then
  begin
    Result := LView.Source.State(LView.Source.Occurrence(0).ExitState);
  end;
end;

procedure TConstantQuery.RequirePrimitiveSize(const AType: INyxPascalSemanticType;
  const ASite: INyxPascalBindingSite);
begin

  if (FResult.Reason <> pbiCompilerContext) or
    ((FResult.Requirements <> nil) and (FResult.Requirements.RequirementCount > 0)) then
  begin
    Exit;
  end;
  FResult.Requirements := NewNyxPascalRequirements(NyxPascalPrimitiveRequirement(AType, ASite));
end;
function TConstantQuery.Resolve(const ASite: INyxPascalBindingSite;
  const AName: TNyxText): INyxPascalResolution;
begin
  Tick;
  Result := ASite.World.Lookup(ASite, NyxPascalBindingName(AName), FCancellation);

  if Result.Status <> pbsBound then
  begin
    RefuseBinding(Result, AName);
  end;
end;

function TConstantQuery.TextValue(const ASite: INyxPascalBindingSite;
  const AText: TNyxText): TNyxPascalConstantValue;
var
  LPlan: TConstantPlan;
begin
  Tick;
  LPlan := TConstantPlan.Create(AText, FOptions.InDialect(pcdPascal), FCancellation);
  try
    Tick(LPlan.FLexical.Count);
    Result := TNyxPascalConstantValue.Unknown;

    if not LPlan.FSupported then
    begin
      Refuse(pbiUnsupportedType);
      Exit;
    end;
    Result := NodeValue(LPlan, LPlan.FRoot, ASite);
  finally
    LPlan.Free;
  end;
end;

function TConstantQuery.DeclarationValue(const ASymbol: INyxPascalBoundSymbol): TNyxPascalConstantValue;
var
  LAt: Integer;
  LKey: TNyxText;
  LDeclaration: TNyxPascalSyntaxDeclaration;
  LView: INyxPascalTranslatedSyntax;
  LSite: INyxPascalBindingSite;
  LType: INyxPascalResolution;
begin
  Result := TNyxPascalConstantValue.Unknown;
  Depend(ASymbol);
  LDeclaration := ASymbol.Declaration;

  if (LDeclaration.Flags * [psdfIncomplete, psdfConditional, psdfUnsupported]) <> [] then
  begin
    Refuse(pbiIncompleteSource, LDeclaration.Name);
    Exit;
  end;
  LKey := TNyxText(IntToStr(ASymbol.ModuleIndex)) + ':' + TNyxText(IntToStr(ASymbol.DeclarationIndex));
  for LAt := 0 to High(FActive) do
  begin
    Tick;

    if FActive[LAt] = LKey then
    begin
      Refuse(pbiRecursiveType, LDeclaration.Name);
      Exit;
    end;
  end;
  SetLength(FActive, Length(FActive) + 1);
  FActive[High(FActive)] := LKey;
  try
    LView := ASymbol.World.ModuleAt(ASymbol.ModuleIndex);

    if LDeclaration.Kind = psdkEnumValue then
    begin
      LType := ASymbol.World.TypeOfSymbol(ASymbol, FCancellation);

      if (LType.Status = pbsBound) and (LType.BoundType <> nil) then
      begin
        Result := OrdinalValue(LType.BoundType, ASymbol, False, LDeclaration.Name);
      end
      else
      begin
        RefuseBinding(LType, LDeclaration.Name);
      end;
      Exit;
    end;

    if (LDeclaration.Kind <> psdkConstant) or (LDeclaration.TypeSpan.Length <> 0) then
    begin
      Refuse(pbiWrongMemberKind, LDeclaration.Name);
      Exit;
    end;

    if LDeclaration.ExpressionSpan.Length = 0 then
    begin
      Refuse(pbiIncompleteSource, LDeclaration.Name);
      Exit;
    end;
    LSite := ASymbol.World.At(ASymbol.ModuleIndex, LDeclaration.Scope, LDeclaration.ExpressionSpan.Start);
    Result := TextValue(LSite, LView.Syntax.Lexical.Slice(LDeclaration.ExpressionSpan));
  finally
    SetLength(FActive, Length(FActive) - 1);
  end;
end;

function TConstantQuery.BoundType(const ASite: INyxPascalBindingSite;
  const AName: TNyxText; out ASource: INyxPascalBoundSymbol): INyxPascalSemanticType;
var
  LFound: INyxPascalResolution;
  LAt: Integer;
begin
  Result := nil;
  ASource := nil;
  LFound := Resolve(ASite, AName);

  if (LFound.Status = pbsBound) and (LFound.BoundType = nil) and (LFound.Count = 1) then
  begin
    Depend(LFound.SymbolAt(0));
    ASource := LFound.SymbolAt(0);
    LFound := ASite.World.TypeOfSymbol(LFound.SymbolAt(0), FCancellation);
  end;

  if (LFound.Status <> pbsBound) or (LFound.BoundType = nil) then
  begin
    RefuseBinding(LFound, AName);
    Exit;
  end;
  Result := LFound.BoundType;
  LAt := 0;
  while (Result <> nil) and (Result.Kind in [pstAlias, pstDistinct]) do
  begin
    Tick;
    Inc(LAt);

    if LAt > FOptions.MaximumDepth then
    begin
      raise ENyxPascalConstantLimit.Create('Constant ordinal aliases exceed their depth budget');
    end;
    Depend(Result.Definition);

    if Result.Definition <> nil then
    begin
      { Constructed range/enum leaves need not have a Definition of their own.
        Keep the last declared alias anchor, including an inline variable type,
        rather than searching the world for a coincidentally equal type name. }
      ASource := Result.Definition;
    end;
    Result := Result.Target;
  end;
end;

function TConstantQuery.OrdinalValue(const AType: INyxPascalSemanticType;
  const ASource: INyxPascalBoundSymbol; AHigh: Boolean;
  const AMember: TNyxText): TNyxPascalConstantValue;
var
  LSyntax: INyxPascalTypeSyntax;
  LNode: Integer;
  LChild: Integer;
  LDefinition: INyxPascalBoundSymbol;
  LSite: INyxPascalBindingSite;
  LLower: TNyxPascalConstantValue;
  LUpper: TNyxPascalConstantValue;
  LIdentity: INyxPascalIntrinsicIdentity;
  LContext: INyxPascalCompilerContext;
  LFact: TNyxPascalSizeFact;
  LPower: TNyxDecimalNumber;
  LAt: Integer;
  LBits: Integer;
  LSigned: Boolean;
  LExpectedBytes: Integer;
  LType: INyxPascalSemanticType;
begin
  Tick;
  Result := TNyxPascalConstantValue.Unknown;

  if AType = nil then
  begin
    Exit;
  end;
  LType := AType;
  LDefinition := ASource;
  LAt := 0;
  while (LType <> nil) and (LType.Kind in [pstAlias, pstDistinct]) do
  begin
    Tick;
    Inc(LAt);

    if LAt > FOptions.MaximumDepth then
    begin
      raise ENyxPascalConstantLimit.Create('Ordinal source ownership exceeds its alias depth budget');
    end;

    if LType.Definition <> nil then
    begin
      LDefinition := LType.Definition;
    end;
    LType := LType.Target;
  end;

  if LType = nil then
  begin
    Refuse(pbiUnsupportedType);
    Exit;
  end;
  Depend(LDefinition);

  if (LType.Kind in [pstSubrange, pstEnumeration]) and (LDefinition <> nil) then
  begin
    LSyntax := LType.Syntax;
    LNode := LType.SyntaxNode;

    { The anchor must own this exact lexical arena. A structural resemblance or
      a generic projection from another owner cannot supply declaration scope. }

    if not LSyntax.Complete or (LSyntax.Lexical <>
      LDefinition.World.ModuleAt(LDefinition.ModuleIndex).Syntax.Lexical) then
    begin
      Refuse(pbiIncompleteSource, LDefinition.Declaration.Name);
      Exit;
    end;
    LSite := LDefinition.World.At(LDefinition.ModuleIndex, LDefinition.Declaration.Scope,
      LSyntax.Node(LNode).Span.Start);

    if LType.Kind = pstSubrange then
    begin
      LLower := TextValue(LSite, LSyntax.Lexical.Slice(
        LSyntax.Node(LSyntax.Child(LNode, ptrLowerBound)).Span));
      LUpper := TextValue(LSite, LSyntax.Lexical.Slice(
        LSyntax.Node(LSyntax.Child(LNode, ptrUpperBound)).Span));

      if (LLower.Kind <> pvvInteger) or (LUpper.Kind <> pvvInteger) or
        (LLower.Number.CompareTo(LUpper.Number) > 0) then
      begin
        Refuse(pbiUnsupportedType, LDefinition.Declaration.Name);
        Exit;
      end;
      Result := LLower;

      if AHigh then
      begin
        Result := LUpper;
      end;
      Exit;
    end;
    LUpper := TNyxPascalConstantValue.IntegerValue(-1);
    LChild := LSyntax.Child(LNode, ptrEnumMember);
    LAt := 0;
    LLower := TNyxPascalConstantValue.Unknown;
    while LChild >= 0 do
    begin
      Tick;
      LNode := LSyntax.Child(LChild, ptrDefault);

      if LNode >= 0 then
      begin
        LSite := LDefinition.World.At(LDefinition.ModuleIndex, LDefinition.Declaration.Scope,
          LSyntax.Node(LChild).Span.Start);
        Result := TextValue(LSite, LSyntax.Lexical.Slice(LSyntax.Node(LNode).Span));
      end
      else
      begin
        Result := LUpper.Operate(pvoAdd, TNyxPascalConstantValue.IntegerValue(1));
      end;

      if (Result.Kind <> pvvInteger) or ((LAt > 0) and
        (Result.Number.CompareTo(LUpper.Number) <= 0)) then
      begin
        Refuse(pbiUnsupportedType, LDefinition.Declaration.Name);
        Exit(TNyxPascalConstantValue.Unknown);
      end;

      if LAt = 0 then
      begin
        LLower := Result;
      end;

      if (AMember <> '') and (NyxPascalASCIIKey(LSyntax.Node(LChild).Name) = NyxPascalASCIIKey(AMember)) then
      begin
        Result := Result.AsOrdinal;
        Exit;
      end;
      LUpper := Result;
      Inc(LAt);
      LChild := LSyntax.Node(LChild).NextSibling;
    end;

    if AMember <> '' then
    begin
      Refuse(pbiMissingName, AMember);
      Exit(TNyxPascalConstantValue.Unknown);
    end;
    Result := LLower;

    if AHigh then
    begin
      Result := LUpper;
    end;
    Result := Result.AsOrdinal;
    Exit;
  end;

  if (LType.Kind <> pstBuiltin) or
    not Supports(LType, INyxPascalIntrinsicIdentity, LIdentity) then
  begin
    Refuse(pbiUnsupportedType);
    Exit;
  end;
  LContext := LIdentity.IntrinsicContext;
  LFact := NyxPascalCapturedSize(LContext, LIdentity.IntrinsicReference);

  if (LContext.Family <> ppcFPC) or (LContext.Version.FullNumber = 0) or
    ((LIdentity.IntrinsicOrigin = pioSystemBootstrap) <>
      (NyxPascalCompilerScope(LContext) = pcsoSystemBootstrap)) or
    (LFact.State <> pssKnown) or
    (LFact.ProbeMode <> LContext.Mode) or (LFact.ProbeLongStrings <> LContext.Option('H')) then
  begin
    Refuse(pbiCompilerContext, LIdentity.IntrinsicReference.Name);
    Exit;
  end;
  LSigned := LType.Builtin in [pbbInteger, pbbShortInt, pbbSmallInt, pbbLongInt,
    pbbInt64, pbbNativeInt];
  LExpectedBytes := 0;
  case LType.Builtin of
    pbbByte, pbbShortInt:
      begin
        LExpectedBytes := 1;
      end;
    pbbWord, pbbSmallInt:
      begin
        LExpectedBytes := 2;
      end;
    pbbLongWord, pbbLongInt:
      begin
        LExpectedBytes := 4;
      end;
    pbbInt64, pbbQWord:
      begin
        LExpectedBytes := 8;
      end;
    pbbInteger, pbbNativeInt, pbbNativeUInt:
      begin
        LExpectedBytes := LFact.Bytes;
      end;
  else
    begin
      Refuse(pbiUnsupportedType, LIdentity.IntrinsicReference.Name);
      Exit;
    end;
  end;

  if (LExpectedBytes <> LFact.Bytes) or not (LFact.Bytes in [1, 2, 4, 8]) then
  begin
    Refuse(pbiCompilerContext, LIdentity.IntrinsicReference.Name);
    Exit;
  end;
  LBits := LFact.Bytes * 8;

  if LSigned then
  begin
    Dec(LBits);
  end;
  LPower := TNyxDecimalNumber.FromInteger(1);
  for LAt := 1 to LBits do
  begin
    Tick;
    LPower := LPower.Add(LPower);
  end;

  if AHigh then
  begin
    Result := TNyxPascalConstantValue.ExactInteger(LPower.Subtract(TNyxDecimalNumber.FromInteger(1)));
  end
  else if LSigned then
  begin
    Result := TNyxPascalConstantValue.ExactInteger(TNyxDecimalNumber.FromInteger(0).Subtract(LPower));
  end
  else
  begin
    Result := TNyxPascalConstantValue.IntegerValue(0);
  end;

  if not LSigned then
  begin
    Result := Result.AsUnsigned;
  end;
end;

function TConstantQuery.SizeValue(const AType: INyxPascalSemanticType): TNyxPascalConstantValue;
var
  LIdentity: INyxPascalIntrinsicIdentity;
  LContext: INyxPascalCompilerContext;
  LFact: TNyxPascalSizeFact;
begin
  Result := TNyxPascalConstantValue.Unknown;

  if (AType = nil) or (AType.Kind <> pstBuiltin) or
    not Supports(AType, INyxPascalIntrinsicIdentity, LIdentity) then
  begin
    Refuse(pbiUnsupportedType);
    Exit;
  end;
  LContext := LIdentity.IntrinsicContext;
  LFact := NyxPascalCapturedSize(LContext, LIdentity.IntrinsicReference);

  if ((LIdentity.IntrinsicOrigin = pioSystemBootstrap) <>
    (NyxPascalCompilerScope(LContext) = pcsoSystemBootstrap)) or
    (LFact.State <> pssKnown) or (LFact.ProbeMode <> LContext.Mode) or
    (LFact.ProbeLongStrings <> LContext.Option('H')) then
  begin
    Refuse(pbiCompilerContext, LIdentity.IntrinsicReference.Name);
    Exit;
  end;
  Result := TNyxPascalConstantValue.IntegerValue(LFact.Bytes);
end;

function TConstantQuery.NodeValue(APlan: TConstantPlan; ANode: Integer;
  const ASite: INyxPascalBindingSite): TNyxPascalConstantValue;
var
  LNode: TConstantNode;
  LLeft: TNyxPascalConstantValue;
  LRight: TNyxPascalConstantValue;
  LFound: INyxPascalResolution;
  LType: INyxPascalSemanticType;
  LContext: INyxPascalCompilerContext;
  LFact: TNyxPascalDefineValue;
  LSource: INyxPascalBoundSymbol;
begin
  Tick;
  Inc(FDepth);

  if FDepth > FOptions.MaximumDepth then
  begin
    raise ENyxPascalConstantLimit.Create('Constant source evaluation exceeds its depth budget');
  end;
  try
    Result := TNyxPascalConstantValue.Unknown;
    LNode := APlan.FNodes[ANode];
    case LNode.Kind of
      cnOrd:
        begin
          Result := NodeValue(APlan, LNode.Left, ASite).OrdValue;

          if (APlan.FOptions.Dialect = pcdFPCConditional) and (Result.Kind = pvvInteger) and
            (Result.Number.CompareTo(TNyxDecimalNumber.FromDecimal(
              NyxDecimal('9223372036854775807'))) > 0) then
          begin
            { FPC's ORD condition uses asInt, independently of HIGH's unsigned
              result. Preserve that signed projection only at this boundary. }
            Result := TNyxPascalConstantValue.ExactInteger(Result.Number.Subtract(
              TNyxDecimalNumber.FromDecimal(NyxDecimal('18446744073709551616'))));
          end;
        end;
      cnValue:
        begin
          Result := LNode.Literal;

          if (Result.Kind = pvvInteger) and (LNode.LiteralBase <> pnbDecimal) and
            (Result.Number.CompareTo(TNyxDecimalNumber.FromDecimal(
              NyxDecimal('9223372036854775807'))) > 0) then
          begin
            LContext := ContextAt(ASite);

            if (LContext = nil) or (LContext.Family <> ppcFPC) or
              (LContext.Version.FullNumber = 0) then
            begin
              Refuse(pbiCompilerContext);
              Exit(TNyxPascalConstantValue.Unknown);
            end;
            { Ordinary FPC source also interprets a high-bit radix token as a
              signed pattern. Keep raw token interchange mathematical; only
              this compiler-bound source evaluation selects that projection. }
            Result := TNyxPascalConstantValue.ExactInteger(Result.Number.Subtract(
              TNyxDecimalNumber.FromDecimal(NyxDecimal('18446744073709551616'))));
          end;
        end;
      cnPositive, cnNegative, cnNot:
        begin
          LLeft := NodeValue(APlan, LNode.Left, ASite);
          case LNode.Kind of
            cnPositive:
              begin

                if LLeft.Kind = pvvInteger then
                begin
                  Result := LLeft;
                end;
              end;
            cnNegative:
              begin
                Result := LLeft.Negated;
              end;
            cnNot:
              begin

                if LLeft.Kind = pvvBoolean then
                begin
                  Result := TNyxPascalConstantValue.BooleanValue(not LLeft.BooleanResult);
                end;
              end;
          else
            begin
              Result := TNyxPascalConstantValue.Unknown;
            end;
          end;
        end;
      cnBinary:
        begin

          if LNode.Operation in [pvoShiftLeft64, pvoShiftRight64] then
          begin
            LContext := ContextAt(ASite);

            if (LContext = nil) or (LContext.Family <> ppcFPC) or
              (LContext.Version.FullNumber = 0) then
            begin
              Refuse(pbiCompilerContext);
              Exit;
            end;
          end;
          LLeft := NodeValue(APlan, LNode.Left, ASite);
          LRight := NodeValue(APlan, LNode.Right, ASite);

          if (LLeft.Kind = pvvInteger) and (LRight.Kind = pvvInteger) and
            (LNode.Operation in [pvoAnd, pvoOr, pvoXor]) then
          begin
            LContext := ContextAt(ASite);

            if (LContext = nil) or (LContext.Family <> ppcFPC) or
              (LContext.Version.FullNumber = 0) then
            begin
              Refuse(pbiCompilerContext);
              Exit;
            end;
          end;
          Result := LLeft.Operate(LNode.Operation, LRight);
        end;
      cnDefined:
        begin
          LContext := ContextAt(ASite);

          if (LContext <> nil) and (Pos('.', LNode.Name) = 0) then
          begin
            LFact := LContext.Symbol(NyxPascalDefine(LNode.Name));

            if LFact.IsDefined <> ppkUnknown then
            begin
              Result := TNyxPascalConstantValue.BooleanValue(LFact.IsDefined = ppkTrue);
            end;
          end;

          if Result.Kind = pvvUnknown then
          begin
            Refuse(pbiCompilerContext, LNode.Name);
          end;
        end;
      cnDeclared:
        begin
          LFound := ASite.World.Lookup(ASite, NyxPascalBindingName(LNode.Name), FCancellation);
          case LFound.Status of
            pbsBound, pbsOverloads:
              begin
                Result := TNyxPascalConstantValue.BooleanValue(True);
              end;
            pbsMissing:
              begin
                Result := TNyxPascalConstantValue.BooleanValue(False);
              end;
          else
            RefuseBinding(LFound, LNode.Name);
          end;
        end;
      cnName:
        begin
          LContext := ContextAt(ASite);

          if (LContext <> nil) and (Pos('.', LNode.Name) = 0) and (Length(LNode.Name) <= 128) then
          begin
            LFact := LContext.Symbol(NyxPascalDefine(LNode.Name));

            if LFact.Kind = ppdInteger then
            begin
              Exit(TNyxPascalConstantValue.IntegerValue(LFact.IntegerValue));
            end;
          end;
          LFound := Resolve(ASite, LNode.Name);

          if (LFound.Status = pbsBound) and (LFound.Count = 1) then
          begin
            Result := DeclarationValue(LFound.SymbolAt(0));
          end;
        end;
      cnLow, cnHigh, cnSize:
        begin
          LType := BoundType(ASite, LNode.Name, LSource);

          if LType <> nil then
          begin

            if LNode.Kind = cnSize then
            begin
              Result := SizeValue(LType);
            end
            else
            begin
              Result := OrdinalValue(LType, LSource, LNode.Kind = cnHigh);
            end;

            if Result.Kind = pvvUnknown then
            begin
              RequirePrimitiveSize(LType, ASite);
            end;
          end;
        end;
    end;

    if (APlan.FOptions.Dialect = pcdFPCConditional) and (Result.Kind = pvvOrdinal) then
    begin
      Result := Result.OrdValue;
    end;

    if Result.Kind = pvvUnknown then
    begin
      Refuse(pbiUnsupportedType, LNode.Name);
    end;
  finally
    Dec(FDepth);
  end;
end;

function TConstantQuery.Run(APlan: TConstantPlan; const ASite: INyxPascalBindingSite;
  const AOptions: TNyxPascalConstantOptions;
  const ACancellation: INyxCancellationScope): INyxPascalConstantResult;
var
  LOwner: INyxPascalConstantResult;
  LContext: INyxPascalCompilerContext;
begin
  AOptions.Validate;

  if AOptions.Dialect <> APlan.FOptions.Dialect then
  begin
    raise ENyxPascalBinding.Create('A parsed constant plan retains its explicit language dialect');
  end;

  if (ASite = nil) or (ASite.World = nil) then
  begin
    raise ENyxPascalBinding.Create('A constant evaluation needs an exact source binding site');
  end;
  FOptions := AOptions;
  FCancellation := ACancellation;
  FResult := TConstantResult.Create;
  LOwner := FResult;
  Tick(APlan.FLexical.Count);

  if AOptions.Dialect = pcdFPCConditional then
  begin
    LContext := ContextAt(ASite);

    if (LContext = nil) or (LContext.Family <> ppcFPC) or
      (LContext.Version.FullNumber = 0) then
    begin
      Refuse(pbiCompilerContext);
      Exit(LOwner);
    end;
  end;

  if APlan.FSupported then
  begin
    FResult.Answer := NodeValue(APlan, APlan.FRoot, ASite);
  end
  else
  begin
    Refuse(pbiUnsupportedType);
  end;

  if FResult.Reason <> pbiNone then
  begin
    FResult.Answer := TNyxPascalConstantValue.Unknown;
  end;
  FCancellation := nil;
  Result := LOwner;
end;

function PlanNyxPascalConstant(const AExpression: TNyxText;
  const AOptions: TNyxPascalConstantOptions;
  const ACancellation: INyxCancellationScope): INyxPascalConstantPlan;
begin
  Result := TConstantPlan.Create(AExpression, AOptions, ACancellation);
end;

end.
