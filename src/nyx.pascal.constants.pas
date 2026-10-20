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
  nyx.pascal.requirements, nyx.pascal.storage;

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
  { Optional source-bound storage description for a pending SizeOf. A non-nil
    plan is a complete data shape, never a measured size or permission to compile
    it under arbitrary defaults. Nodes are in child-before-parent order; their
    shape leases identify exact children, including repeated grouped fields.
    Each node retains its own declaration/context. Primitive identity is separate
    from record policy, and checked intrinsic getters require a primitive node.
    No query, mutable provider, cancellation or application callback survives.
    Native measurement/fulfillment is a separate, still required authority. }
  INyxPascalConstantStorage = interface(IInterface)
    ['{4FC1D610-F52B-46CC-B8CE-81C17F953A1D}']
    function StoragePlan: INyxPascalStoragePlan;
    function StorageNodeCount: Integer;
    function StorageNode(AIndex: Integer): INyxPascalStorageShape;
    function StorageContext(AIndex: Integer): INyxPascalCompilerContext;
    function StorageDefinition(AIndex: Integer): INyxPascalBoundSymbol;
    function StorageIntrinsicOrigin(AIndex: Integer): TNyxPascalIntrinsicOrigin;
    function StorageIntrinsicReference(AIndex: Integer): TNyxPascalSizeReference;
  end;
  { Optional complete structural/context identity. It excludes stored layout
    answers themselves, so inserting a receipt cannot recursively change its
    own lookup key. It is not nominal identity or compiler-execution authority.
    Missing/incomplete storage refuses this checked getter. }
  INyxPascalConstantStorageIdentity = interface(IInterface)
    ['{D44D97AD-891A-4D2E-92C8-01478C37D93D}']
    function StorageKey: TNyxPascalStorageKey;
  end;
  { Construction-only source witness authority, supplied by an ordered
    conditional reader with the exact physical site. No completed value or
    source snapshot retains this callback or its execution budget. }
  INyxPascalSourceSizeScope = interface(IInterface)
    ['{49C68C42-2053-49E3-AB0B-19321755EA6D}']
    function SourceSize(const ASite: INyxPascalBindingSite;
      const AName: TNyxPascalSizeReference; AMaximumWork: Integer; out AWork: Integer;
      out ARequirement: INyxPascalCompilerRequirement): TNyxPascalStorageFact;
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

uses nyx.data, nyx.numbers.decimal, nyx.pascal.syntax.translated, nyx.pascal.work;

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
  TConstantResult = class(TInterfacedObject, INyxPascalConstantResult,
    INyxPascalCompilerRequirements, INyxPascalConstantStorage,
    INyxPascalConstantStorageIdentity)
  public
    Answer: TNyxPascalConstantValue;
    Reason: TNyxPascalBindingIssue;
    Related: TNyxText;
    Work: Integer;
    Dependencies: array of INyxPascalBoundSymbol;
    Requirements: INyxPascalCompilerRequirements;
    Storage: INyxPascalConstantStorage;
    function Value: TNyxPascalConstantValue;
    function Issue: TNyxPascalBindingIssue;
    function RelatedName: TNyxText;
    function WorkUnits: Integer;
    function DependencyCount: Integer;
    function Dependency(AIndex: Integer): INyxPascalBoundSymbol;
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
  { Construction-local storage capture becomes a read-only response only after
    every child and context has qualified. Parallel dynamic interface vectors
    retain no earlier builder, parent, active recursion or execution scope.
    Context leases retain the existing frozen compiler snapshots; declaration
    leases point outward to their immutable world. Failure publishes no rows. }
  TConstantStorage = class(TInterfacedObject, INyxPascalConstantStorage,
    INyxPascalConstantStorageIdentity)
  public
    Plan: INyxPascalStoragePlan;
    Key: TNyxPascalStorageKey;
    Shapes: array of INyxPascalStorageShape;
    Contexts: array of INyxPascalCompilerContext;
    Definitions: array of INyxPascalBoundSymbol;
    Origins: array of TNyxPascalIntrinsicOrigin;
    References: array of TNyxPascalSizeReference;
    procedure Add(const AShape: INyxPascalStorageShape;
      const AContext: INyxPascalCompilerContext;
      const ADefinition: INyxPascalBoundSymbol;
      const AIntrinsic: INyxPascalIntrinsicIdentity;
      const ACancellation: INyxCancellationScope);
    procedure CheckIndex(AIndex: Integer);
    function StoragePlan: INyxPascalStoragePlan;
    function StorageKey: TNyxPascalStorageKey;
    function StorageNodeCount: Integer;
    function StorageNode(AIndex: Integer): INyxPascalStorageShape;
    function StorageContext(AIndex: Integer): INyxPascalCompilerContext;
    function StorageDefinition(AIndex: Integer): INyxPascalBoundSymbol;
    function StorageIntrinsicOrigin(AIndex: Integer): TNyxPascalIntrinsicOrigin;
    function StorageIntrinsicReference(AIndex: Integer): TNyxPascalSizeReference;
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
    function StorageShape(const AType: INyxPascalSemanticType;
      const ASource: INyxPascalBoundSymbol; const ASite: INyxPascalBindingSite;
      AStorage: TConstantStorage): INyxPascalStorageShape;
    function StorageDimension(const ASyntax: INyxPascalTypeSyntax; ANode: Integer;
      const ASite: INyxPascalBindingSite): Integer;
    function StorageEvidenceKey(AStorage: TConstantStorage): TNyxPascalStorageKey;
    function StorageEvidenceReady(AStorage: TConstantStorage): Boolean;
    function StoredSize(const AContext: INyxPascalCompilerContext;
      const AKey: TNyxPascalStorageKey): TNyxPascalStorageFact;
    function SizeValue(const AType: INyxPascalSemanticType;
      const ASource: INyxPascalBoundSymbol;
      const ASite: INyxPascalBindingSite;
      const AName: TNyxPascalSizeReference): TNyxPascalConstantValue;
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
  NyxPascalAdmitAnalysis(FCancellation, papConstantPlan);
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
  NyxPascalAdmitAnalysis(FCancellation, papConstantPlan);

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

function TConstantResult.StoragePlan: INyxPascalStoragePlan;
begin
  Result := nil;

  if Storage <> nil then
  begin
    Result := Storage.StoragePlan;
  end;
end;

function TConstantResult.StorageNodeCount: Integer;
begin
  Result := 0;

  if Storage <> nil then
  begin
    Result := Storage.StorageNodeCount;
  end;
end;

function TConstantResult.StorageNode(AIndex: Integer): INyxPascalStorageShape;
begin

  if Storage = nil then
  begin
    raise ENyxPascalBinding.Create('This result has no complete storage description');
  end;
  Result := Storage.StorageNode(AIndex);
end;

function TConstantResult.StorageKey: TNyxPascalStorageKey;
var
  LIdentity: INyxPascalConstantStorageIdentity;
begin

  if not Supports(Storage, INyxPascalConstantStorageIdentity, LIdentity) then
  begin
    raise ENyxPascalBinding.Create('No complete structural receipt is available');
  end;
  Result := LIdentity.StorageKey;
end;

function TConstantResult.StorageContext(AIndex: Integer): INyxPascalCompilerContext;
begin
  StorageNode(AIndex);
  Result := Storage.StorageContext(AIndex);
end;

function TConstantResult.StorageDefinition(AIndex: Integer): INyxPascalBoundSymbol;
begin
  StorageNode(AIndex);
  Result := Storage.StorageDefinition(AIndex);
end;

function TConstantResult.StorageIntrinsicOrigin(AIndex: Integer): TNyxPascalIntrinsicOrigin;
begin
  StorageNode(AIndex);
  Result := Storage.StorageIntrinsicOrigin(AIndex);
end;

function TConstantResult.StorageIntrinsicReference(AIndex: Integer): TNyxPascalSizeReference;
begin
  StorageNode(AIndex);
  Result := Storage.StorageIntrinsicReference(AIndex);
end;

procedure TConstantStorage.Add(const AShape: INyxPascalStorageShape;
  const AContext: INyxPascalCompilerContext;
  const ADefinition: INyxPascalBoundSymbol;
  const AIntrinsic: INyxPascalIntrinsicIdentity;
  const ACancellation: INyxCancellationScope);
var
  LAt: Integer;
begin
  NyxPascalAdmitAnalysis(ACancellation, papTypeNode);
  LAt := Length(Shapes);

  if LAt >= TNyxPascalStorageOptions.Defaults.MaximumNodes then
  begin
    raise ENyxPascalConstantLimit.Create('Source storage exceeds its node budget');
  end;
  SetLength(Shapes, LAt + 1);
  SetLength(Contexts, LAt + 1);
  SetLength(Definitions, LAt + 1);
  SetLength(Origins, LAt + 1);
  SetLength(References, LAt + 1);
  Shapes[LAt] := AShape;
  Contexts[LAt] := AContext;
  Definitions[LAt] := ADefinition;

  if AIntrinsic <> nil then
  begin
    Origins[LAt] := AIntrinsic.IntrinsicOrigin;
    References[LAt] := AIntrinsic.IntrinsicReference;
  end;
end;

procedure TConstantStorage.CheckIndex(AIndex: Integer);
begin

  if (AIndex < 0) or (AIndex >= Length(Shapes)) then
  begin
    raise ENyxPascalBinding.Create('A storage node index is outside its captured result');
  end;
end;

function TConstantStorage.StoragePlan: INyxPascalStoragePlan;
begin Result := Plan; end;
function TConstantStorage.StorageKey: TNyxPascalStorageKey;
begin

  if Plan = nil then
  begin
    raise ENyxPascalBinding.Create('No complete structural receipt is available');
  end;
  Result := Key;
end;
function TConstantStorage.StorageNodeCount: Integer;
begin Result := Length(Shapes); end;

function TConstantStorage.StorageNode(AIndex: Integer): INyxPascalStorageShape;
begin
  CheckIndex(AIndex);
  Result := Shapes[AIndex];
end;

function TConstantStorage.StorageContext(AIndex: Integer): INyxPascalCompilerContext;
begin
  CheckIndex(AIndex);
  Result := Contexts[AIndex];
end;

function TConstantStorage.StorageDefinition(AIndex: Integer): INyxPascalBoundSymbol;
begin
  CheckIndex(AIndex);
  Result := Definitions[AIndex];
end;

function TConstantStorage.StorageIntrinsicOrigin(AIndex: Integer): TNyxPascalIntrinsicOrigin;
begin

  if StorageNode(AIndex).Kind <> pskPrimitive then
  begin
    raise ENyxPascalBinding.Create('Only a primitive storage node has intrinsic identity');
  end;
  Result := Origins[AIndex];
end;

function TConstantStorage.StorageIntrinsicReference(AIndex: Integer): TNyxPascalSizeReference;
begin
  StorageIntrinsicOrigin(AIndex);
  Result := References[AIndex];
end;

procedure TConstantQuery.Tick(AWork: Integer);
begin
  NyxPascalAdmitAnalysis(FCancellation, papConstantStep, AWork);

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

  if (FResult.Reason <> pbiCompilerContext) or (FResult.Storage <> nil) or
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

{ Builtin classification alone grants no layout. Keep the full closed spelling,
  including AnsiChar/Char and Real/Double distinctions, for a later compiler job. }
function StoragePrimitive(AValue: TNyxPascalBuiltin;
  out APrimitive: TNyxPascalStoragePrimitive): Boolean;
begin
  Result := True;
  case AValue of
    pbbByte: APrimitive := pspByte;
    pbbShortInt: APrimitive := pspShortInt;
    pbbWord: APrimitive := pspWord;
    pbbSmallInt: APrimitive := pspSmallInt;
    pbbLongWord: APrimitive := pspLongword;
    pbbLongInt: APrimitive := pspLongint;
    pbbQWord: APrimitive := pspQWord;
    pbbInt64: APrimitive := pspInt64;
    pbbInteger: APrimitive := pspInteger;
    pbbNativeInt: APrimitive := pspNativeInt;
    pbbNativeUInt: APrimitive := pspNativeUInt;
    pbbPointer: APrimitive := pspPointer;
    pbbBoolean: APrimitive := pspBoolean;
    pbbByteBool: APrimitive := pspByteBool;
    pbbWordBool: APrimitive := pspWordBool;
    pbbLongBool: APrimitive := pspLongBool;
    pbbChar: APrimitive := pspChar;
    pbbAnsiChar: APrimitive := pspAnsiChar;
    pbbWideChar: APrimitive := pspWideChar;
    pbbSingle: APrimitive := pspSingle;
    pbbDouble: APrimitive := pspDouble;
    pbbExtended: APrimitive := pspExtended;
    pbbReal: APrimitive := pspReal;
    pbbCurrency: APrimitive := pspCurrency;
    pbbComp: APrimitive := pspComp;
    pbbAnsiString: APrimitive := pspAnsiString;
    pbbUTF8String: APrimitive := pspUTF8String;
    pbbUnicodeString: APrimitive := pspUnicodeString;
    pbbWideString: APrimitive := pspWideString;
    pbbShortString: APrimitive := pspShortString;
  else
    APrimitive := pspByte;
    Result := False;
  end;
end;

function StorageForm(const AFlags: TNyxPascalTypeFlags): TNyxPascalStorageForm;
begin
  Result := psfOrdinary;

  if ptfPacked in AFlags then
  begin
    Result := psfPacked;
  end;

  if ptfBitPacked in AFlags then
  begin
    Result := psfBitPacked;
  end;
end;

function TConstantQuery.StorageDimension(const ASyntax: INyxPascalTypeSyntax;
  ANode: Integer; const ASite: INyxPascalBindingSite): Integer;
var
  LSyntax: INyxPascalTypeSyntax;
  LLower, LUpper: TNyxPascalConstantValue;
  LLength: TNyxDecimalNumber;
  LType: INyxPascalSemanticType;
  LSource: INyxPascalBoundSymbol;
  LSpan, LRange: TNyxPascalSpan;
  LToken: TNyxPascalToken;
  LAt, LDepth: Integer;
  LHasRange: Boolean;
begin
  Result := 0;
  Tick;
  LSpan := ASyntax.Node(ANode).Span;
  LHasRange := False;
  LDepth := 0;
  LRange := NyxPascalSpan(0, 0);
  LAt := ASyntax.Lexical.TokenAt(LSpan.Start);
  { The declaration's opaque dimension can contain full constant expressions,
    including parentheses. Split its actual top-level range token; do not use
    text matching, synthesize source or start an independent value query. }
  while LAt < ASyntax.Lexical.Count do
  begin
    Tick;
    LToken := ASyntax.Lexical.Token(LAt);

    if (LToken.Kind = ptkEOF) or (LToken.Span.Start >= LSpan.Finish) then
    begin
      Break;
    end;

    if LToken.Kind = ptkSymbol then
    begin
      case LToken.Symbol of
        psoOpenParen, psoOpenBracket: Inc(LDepth);
        psoCloseParen, psoCloseBracket: Dec(LDepth);
        psoRange:
          begin

            if LDepth = 0 then
            begin

              if LHasRange then
              begin
                Refuse(pbiUnsupportedType);
                Exit;
              end;
              LHasRange := True;
              LRange := LToken.Span;
            end;
          end;
      else
        begin
          { Other symbols belong to a bound expression, not its delimiter. }
        end;
      end;
    end;
    Inc(LAt);
  end;

  if LHasRange then
  begin
    LLower := TextValue(ASite, ASyntax.Lexical.Slice(
      NyxPascalSpan(LSpan.Start, LRange.Start - LSpan.Start)));
    LUpper := TextValue(ASite, ASyntax.Lexical.Slice(
      NyxPascalSpan(LRange.Finish, LSpan.Finish - LRange.Finish)));
  end
  else
  begin
    { A named ordinal dimension still binds at this exact declaration. Its
      range/enum dependencies join this query, rather than a later caller site. }
    LSyntax := NewNyxPascalTypeSyntax(ASyntax.Lexical, LSpan,
      TNyxPascalTypeOptions.Defaults, FCancellation);

    if not LSyntax.Complete or
      not (LSyntax.Node(LSyntax.Root).Kind in [ptxName, ptxQualified]) then
    begin
      Refuse(pbiUnsupportedType);
      Exit;
    end;
    LType := BoundType(ASite, LSyntax.Lexical.Slice(LSyntax.Window), LSource);
    LLower := OrdinalValue(LType, LSource, False).OrdValue;
    LUpper := OrdinalValue(LType, LSource, True).OrdValue;

    if (LLower.Kind = pvvUnknown) or (LUpper.Kind = pvvUnknown) then
    begin
      RequirePrimitiveSize(LType, ASite);
    end;
  end;

  if (LLower.Kind <> pvvInteger) or (LUpper.Kind <> pvvInteger) or
    (LLower.Number.CompareTo(LUpper.Number) > 0) then
  begin
    Refuse(pbiUnsupportedType);
    Exit;
  end;
  LLength := LUpper.Number.Subtract(LLower.Number).Add(TNyxDecimalNumber.FromInteger(1));

  if LLength.CompareTo(TNyxDecimalNumber.FromInteger(2147483647)) > 0 then
  begin
    Refuse(pbiUnsupportedType);
    Exit;
  end;
  Result := StrToInt(String(LLength.ToText));
end;

function TConstantQuery.StorageShape(const AType: INyxPascalSemanticType;
  const ASource: INyxPascalBoundSymbol; const ASite: INyxPascalBindingSite;
  AStorage: TConstantStorage): INyxPascalStorageShape;
var
  LType: INyxPascalSemanticType;
  LDefinition, LField: INyxPascalBoundSymbol;
  LView: INyxPascalTranslatedSyntax;
  LSyntax: INyxPascalTypeSyntax;
  LSite: INyxPascalBindingSite;
  LIdentity: INyxPascalIntrinsicIdentity;
  LSubstituted: INyxPascalSubstitutedSymbol;
  LContext: INyxPascalCompilerContext;
  LFact: TNyxPascalSizeFact;
  LPrimitive: TNyxPascalStoragePrimitive;
  LAlignment: TNyxPascalStorageAlignment;
  LForm: TNyxPascalStorageForm;
  LRecord: INyxPascalStorageRecord;
  LChild: INyxPascalStorageShape;
  LResolved: INyxPascalResolution;
  LDeclaration, LMember: TNyxPascalSyntaxDeclaration;
  LScope: TNyxPascalSyntaxScope;
  LVariants: INyxPascalVariantSyntax;
  LKey: TNyxText;
  LAt, LActive, LDimension: Integer;
  LDimensions: array of Integer;
begin
  Tick;
  Result := nil;
  Inc(FDepth);
  LActive := Length(FActive);
  LDimensions := nil;
  try

    if FDepth > FOptions.MaximumDepth then
    begin
      raise ENyxPascalConstantLimit.Create('Source storage exceeds the constant query depth budget');
    end;

    if (AType = nil) or (AType.World <> ASite.World) then
    begin
      Refuse(pbiUnsupportedType);
      Exit;
    end;
    LType := AType;
    LDefinition := ASource;

    if LType.Definition <> nil then
    begin
      LDefinition := LType.Definition;
    end;
    Depend(LDefinition);

    if LType.Kind in [pstAlias, pstDistinct] then
    begin
      Exit(StorageShape(LType.Target, LDefinition, ASite, AStorage));
    end;

    if LType.Kind = pstBuiltin then
    begin

      if not Supports(LType, INyxPascalIntrinsicIdentity, LIdentity) or
        not StoragePrimitive(LType.Builtin, LPrimitive) then
      begin
        Refuse(pbiUnsupportedType);
        Exit;
      end;
      LContext := LIdentity.IntrinsicContext;
      LFact := NyxPascalCapturedSize(LContext, LIdentity.IntrinsicReference);

      if (LContext.Family <> ppcFPC) or (LContext.Version.FullNumber = 0) or
        (LContext.Mode = ppmUnknown) or (LFact.State <> pssKnown) or
        (LFact.ProbeMode <> LContext.Mode) or
        (LFact.ProbeLongStrings <> LContext.Option('H')) or
        ((LIdentity.IntrinsicOrigin = pioSystemBootstrap) <>
          (NyxPascalCompilerScope(LContext) = pcsoSystemBootstrap)) then
      begin
        Refuse(pbiCompilerContext, LIdentity.IntrinsicReference.Name);
        RequirePrimitiveSize(LType, ASite);
        Exit;
      end;
      Result := NewNyxPascalStoragePrimitive(LPrimitive, FCancellation);
      AStorage.Add(Result, LContext, LDefinition, LIdentity, FCancellation);
      Exit;
    end;

    if (LDefinition = nil) or (LDefinition.World <> ASite.World) then
    begin
      Refuse(pbiUnsupportedType);
      Exit;
    end;
    LDeclaration := LDefinition.Declaration;

    if LDeclaration.Flags * [psdfIncomplete, psdfConditional, psdfUnsupported, psdfForward] <> [] then
    begin
      Refuse(pbiIncompleteSource, LDeclaration.Name);
      Exit;
    end;
    LKey := 'storage:' + TNyxText(IntToStr(LDefinition.ModuleIndex)) + ':' +
      TNyxText(IntToStr(LDefinition.DeclarationIndex));

    if LType.Kind = pstArray then
    begin
      { Nested anonymous arrays can share a declaration anchor. Their exact
        syntax-node ordinals remain distinct within that same lexical owner. }
      LKey := LKey + ':array:' + TNyxText(IntToStr(LType.SyntaxNode));
    end;
    for LAt := 0 to High(FActive) do
    begin
      Tick;

      if FActive[LAt] = LKey then
      begin
        Refuse(pbiRecursiveType, LDeclaration.Name);
        Exit;
      end;
    end;
    SetLength(FActive, LActive + 1);
    FActive[LActive] := LKey;
    LView := LDefinition.World.ModuleAt(LDefinition.ModuleIndex);
    LSite := LDefinition.World.At(LDefinition.ModuleIndex, LDeclaration.Scope,
      LDeclaration.TypeSpan.Start);
    LContext := ContextAt(LSite);

    if (LContext = nil) or (LContext.Family <> ppcFPC) or
      (LContext.Version.FullNumber = 0) or (LContext.Mode = ppmUnknown) then
    begin
      Refuse(pbiCompilerContext, LDeclaration.Name);
      Exit;
    end;

    if LType.Kind = pstArray then
    begin
      LSyntax := LType.Syntax;

      if (LSyntax = nil) or not LSyntax.Complete or
        (LSyntax.Lexical <> LView.Syntax.Lexical) then
      begin
        Refuse(pbiIncompleteSource, LDeclaration.Name);
        Exit;
      end;
      LDimension := LSyntax.Child(LType.SyntaxNode, ptrDimension);
      while (LDimension >= 0) and (LSyntax.Node(LDimension).Role = ptrDimension) do
      begin
        Tick;

        if Length(LDimensions) >= FOptions.MaximumDepth - FDepth then
        begin
          raise ENyxPascalConstantLimit.Create('Storage array dimensions exceed the query depth budget');
        end;
        LAt := StorageDimension(LSyntax, LDimension, LSite);

        if LAt < 1 then
        begin
          Exit;
        end;
        SetLength(LDimensions, Length(LDimensions) + 1);
        LDimensions[High(LDimensions)] := LAt;
        LDimension := LSyntax.Node(LDimension).NextSibling;
      end;

      if Length(LDimensions) = 0 then
      begin
        Refuse(pbiUnsupportedType, LDeclaration.Name);
        Exit;
      end;
      LChild := StorageShape(LType.Target, LDefinition, LSite, AStorage);

      if LChild = nil then
      begin
        Exit;
      end;
      LForm := StorageForm(LSyntax.Node(LType.SyntaxNode).Flags);
      for LAt := High(LDimensions) downto 0 do
      begin
        Tick;
        LChild := NewNyxPascalStorageArray(LChild, LDimensions[LAt], LForm,
          TNyxPascalStorageOptions.Defaults, FCancellation);
        AStorage.Add(LChild, LContext, LDefinition, nil, FCancellation);
      end;
      Exit(LChild);
    end;

    if (LType.Kind <> pstDeclared) or (LDeclaration.TypeForm <> pstfRecord) or
      (LDeclaration.NestedScope < 0) then
    begin
      Refuse(pbiUnsupportedType, LDeclaration.Name);
      Exit;
    end;

    if (LDeclaration.GenericSpan.Length > 0) or
      (Supports(LDefinition, INyxPascalSubstitutedSymbol, LSubstituted) and
        (LSubstituted.Environment <> nil)) then
    begin
      Refuse(pbiGenericContext, LDeclaration.Name);
      Exit;
    end;
    LScope := LView.Syntax.Scope(LDeclaration.NestedScope);

    if not LScope.Complete or LScope.UnsupportedSyntax or
      not Supports(LView.Syntax, INyxPascalVariantSyntax, LVariants) then
    begin
      Refuse(pbiIncompleteSource, LDeclaration.Name);
      Exit;
    end;
    for LAt := 0 to LVariants.SelectorCount - 1 do
    begin
      Tick;

      if LVariants.SelectorAt(LAt).RecordScope = LDeclaration.NestedScope then
      begin
        { A union cannot be flattened into sequential fields. Empty/nested arms
          still need their own representation; no convenient branch is chosen. }
        Refuse(pbiUnsupportedType, LDeclaration.Name);
        Exit;
      end;
    end;
    case NyxPascalCompilerPolicies(LContext).RecordAlignment of
      praDefault: LAlignment := psaDefault;
      praC: LAlignment := psaC;
      pra1: LAlignment := psa1;
      pra2: LAlignment := psa2;
      pra4: LAlignment := psa4;
      pra8: LAlignment := psa8;
      pra16: LAlignment := psa16;
      pra32: LAlignment := psa32;
    else
      { Missing initial/default capture is a real context prerequisite. A native
        compiler must not receive DEFAULT merely because this value is Unknown. }
      Refuse(pbiCompilerContext, LDeclaration.Name);
      Exit;
    end;
    LSyntax := LView.TypeOfDeclaration(LDefinition.DeclarationIndex,
      TNyxPascalTypeOptions.Defaults, FCancellation);

    if not LSyntax.Complete then
    begin
      Refuse(pbiIncompleteSource, LDeclaration.Name);
      Exit;
    end;
    LRecord := NewNyxPascalStorageRecord(StorageForm(LSyntax.Node(LSyntax.Root).Flags),
      LAlignment, FCancellation);
    LAt := LScope.FirstDeclaration;
    while LAt >= 0 do
    begin
      Tick;
      LMember := LView.Syntax.Declaration(LAt);

      if LMember.Scope <> LDeclaration.NestedScope then
      begin
        Refuse(pbiIncompleteSource, LDeclaration.Name);
        Exit;
      end;

      if (LMember.Kind in [psdkField, psdkVariable]) and
        not (psdfClassMember in LMember.Flags) then
      begin

        if (LMember.VariantArm >= 0) or
          (LMember.Flags * [psdfIncomplete, psdfConditional, psdfUnsupported] <> []) then
        begin
          Refuse(pbiIncompleteSource, LMember.Name);
          Exit;
        end;
        LField := LDefinition.World.Symbol(LDefinition.ModuleIndex, LAt);
        Depend(LField);
        LResolved := LDefinition.World.TypeOfSymbol(LField, FCancellation);

        if (LResolved.Status <> pbsBound) or (LResolved.BoundType = nil) then
        begin
          RefuseBinding(LResolved, LMember.Name);
          Exit;
        end;
        LChild := StorageShape(LResolved.BoundType, LField,
          LDefinition.World.At(LDefinition.ModuleIndex, LMember.Scope,
            LMember.TypeSpan.Start), AStorage);

        if LChild = nil then
        begin
          Exit;
        end;
        LRecord := LRecord.Field(LChild, TNyxPascalStorageOptions.Defaults, FCancellation);
      end
      else if not (LMember.Kind in [psdkType, psdkConstant, psdkRoutine, psdkProperty,
        psdkVariable, psdkField]) then
      begin
        Refuse(pbiUnsupportedType, LMember.Name);
        Exit;
      end;
      LAt := LMember.Next;
    end;
    Result := LRecord;
    AStorage.Add(Result, LContext, LDefinition, nil, FCancellation);
  finally
    SetLength(FActive, LActive);
    Dec(FDepth);
  end;
end;

function TConstantQuery.StorageEvidenceKey(AStorage: TConstantStorage): TNyxPascalStorageKey;
var
  LParts: TNyxStrings;
  LBytes, LAt: Integer;

  procedure Part(const AText: TNyxText);
  var
    LText: TNyxText;
  begin
    LText := TNyxText(IntToStr(Length(AText))) + ':' + AText + '|';
    Tick(Length(LText));

    if Length(LText) > NyxPascalStorageKeyLimit - LBytes then
    begin
      raise ENyxPascalConstantLimit.Create('Structural identity exceeds its byte budget');
    end;
    Inc(LBytes, Length(LText));
    LParts.Add(LText);
  end;

  procedure Number(AValue: Integer);
  begin Part(TNyxText(IntToStr(AValue))); end;

  procedure Context(const AContext: INyxPascalCompilerContext; ADepth: Integer);
  var
    LIndex, LAssignment: Integer;
    LFeature: TNyxPascalLanguageFeature;
    LName: TNyxPascalDefineRef;
    LPolicy: TNyxPascalCompilerPolicies;
    LWarnings: INyxPascalContextWarnings;
    LWarning: TNyxPascalWarningRule;
    LAlignments: INyxPascalContextCodeAlignments;
    LAlignment: TNyxPascalCodeAlignmentRule;
    LOptimizations: INyxPascalContextOptimizations;
    LOptimization: TNyxPascalOptimizationRule;
    LSizes: INyxPascalContextSizes;
    LFact: TNyxPascalSizeFact;
    LBootstrap: INyxPascalCompilerContext;
    LFPU: TNyxPascalFPUReference;
    LSourceFPU: TNyxPascalSourceFPUSelection;
  begin

    if (AContext = nil) or (ADepth > 1) then
    begin
      raise ENyxPascalBinding.Create('Storage identity needs a bounded captured compiler context');
    end;
    Number(Ord(AContext.Family)); Number(AContext.Version.FullNumber);
    Number(Ord(AContext.Mode)); Number(Ord(NyxPascalCompilerScope(AContext)));
    Number(Ord(AContext.DefinesComplete)); Number(AContext.SymbolCount);
    for LIndex := 0 to AContext.SymbolCount - 1 do
    begin
      LName := AContext.SymbolName(LIndex);
      Part(LName.Name); Number(Ord(AContext.Symbol(LName).Kind));

      if AContext.Symbol(LName).Kind = ppdInteger then
      begin
        Number(AContext.Symbol(LName).IntegerValue);
      end;
    end;
    for LIndex := Ord('A') to Ord('Z') do Number(Ord(AContext.Option(Chr(LIndex))));
    for LFeature := Low(TNyxPascalLanguageFeature) to High(TNyxPascalLanguageFeature) do
      Number(Ord(AContext.Feature(LFeature)));
    LPolicy := NyxPascalCompilerPolicies(AContext);
    Number(Ord(LPolicy.Inlining)); Number(Ord(LPolicy.RecordAlignment));
    Number(Ord(LPolicy.EnumStorage)); Number(Ord(LPolicy.SetPacking));
    Number(Ord(LPolicy.CallingConvention)); Number(Ord(LPolicy.SmartLinkingRequested));
    Number(Ord(LPolicy.GotoRequested)); Number(Ord(LPolicy.AssemblerReader));
    Number(Ord(LPolicy.DiagnosticEmission(pdcWarnings)));
    Number(Ord(LPolicy.DiagnosticEmission(pdcHints)));
    Number(Ord(LPolicy.DiagnosticEmission(pdcNotes)));

    if LPolicy.PointerCheckingRequested <> ppcrUnknown then
    begin
      { Tagged extension cannot collide with the following numeric warning
        count in older identities. Unspecified requests retain their old key;
        explicit Default/Off/On each qualify their own exact context meaning. }
      Part('pointer-check-request-v1');
      Number(Ord(LPolicy.PointerCheckingRequested));
    end;

    if LPolicy.InterfaceStyle <> pifsUnknown then
    begin
      { Keep unspecified contexts' exact prior identity while distinguishing
        each captured/authored style from every following numeric field. }
      Part('interface-style-v1');
      Number(Ord(LPolicy.InterfaceStyle));
    end;
    if LPolicy.ExcessPrecisionRequested <> ppkUnknown then
    begin
      { Tagged optional extension preserves every prior unspecified identity.
        ON and OFF cannot collide with each other or the next numeric field. }
      Part('excess-precision-request-v1');
      Number(Ord(LPolicy.ExcessPrecisionRequested));
    end;
    if LPolicy.ModeExcessPrecision <> ppkUnknown then
    begin
      { Equal Boolean settings with different source provenance are distinct
        identities. Contexts without an authored MODE effect keep the prior key. }
      Part('mode-excess-precision-v1');
      Number(Ord(LPolicy.ModeExcessPrecision));
    end;
    LWarnings := NyxPascalWarnings(AContext);
    LFPU := NyxPascalConfiguredFPU(AContext);

    if LFPU.Known then
    begin
      { Initial profile metadata has a separate optional identity namespace.
        Source defines or later local settings cannot impersonate this field. }
      Part('configured-fpu-v1');
      Part(LFPU.Name);
    end;
    LSourceFPU := NyxPascalSourceFPU(AContext);

    if LSourceFPU.Kind <> pfsConfigured then
    begin
      { The configured fallback keeps the original key form. Unknown source
        history must not collide with it or with an explicit selection. }
      Part('source-fpu-selection-v1');
      Number(Ord(LSourceFPU.Kind));
      Part(LSourceFPU.Reference.Name);
    end;
    Number(LWarnings.WarningCount);
    for LIndex := 0 to LWarnings.WarningCount - 1 do
    begin
      LWarning := LWarnings.WarningAt(LIndex);
      Number(Ord(LWarning.Reference.Kind));

      if LWarning.Reference.Kind = pwrNumber then
      begin Number(LWarning.Reference.Number); end
      else
      begin Part(LWarning.Reference.Name); end;
      Number(Ord(LWarning.Action)); Number(Ord(LWarning.Activity));
    end;
    LAlignments := NyxPascalCodeAlignments(AContext);
    Number(LAlignments.CodeAlignmentCount);
    for LIndex := 0 to LAlignments.CodeAlignmentCount - 1 do
    begin
      LAlignment := LAlignments.CodeAlignmentAt(LIndex);
      Number(Ord(LAlignment.Activity)); Number(LAlignment.Request.Count);
      for LAssignment := 0 to LAlignment.Request.Count - 1 do
      begin
        Number(Ord(LAlignment.Request.Assignment(LAssignment).Subject));
        Number(LAlignment.Request.Assignment(LAssignment).Value);
      end;
    end;
    LOptimizations := NyxPascalOptimizations(AContext);
    Number(LOptimizations.OptimizationCount);
    for LIndex := 0 to LOptimizations.OptimizationCount - 1 do
    begin
      LOptimization := LOptimizations.OptimizationAt(LIndex);
      Number(Ord(LOptimization.Activity)); Number(Ord(LOptimization.Request.Kind));

      if LOptimization.Request.Kind = porkMode then
      begin Number(Ord(LOptimization.Request.RequestedMode)); end
      else
      begin
        Number(Ord(LOptimization.Request.Feature)); Number(Ord(LOptimization.Request.Enabled));
      end;
    end;

    if Supports(AContext, INyxPascalContextSizes, LSizes) then
    begin
      Number(LSizes.SizeCount);
      for LIndex := 0 to LSizes.SizeCount - 1 do
      begin
        Part(LSizes.SizeName(LIndex).Name);
        LFact := LSizes.SizeFact(LSizes.SizeName(LIndex));
        Number(Ord(LFact.State)); Number(Ord(LFact.ProbeMode)); Number(Ord(LFact.ProbeLongStrings));

        if LFact.State = pssKnown then Number(LFact.Bytes);
      end;
    end
    else
    begin Number(0); end;
    LBootstrap := NyxPascalBootstrapContext(AContext);
    Number(Ord(LBootstrap <> nil));

    if LBootstrap <> nil then Context(LBootstrap, ADepth + 1);
    { Structural answers are deliberately excluded. No old context or source
      lease enters the receipt, and adding its matching answer stays stable. }
  end;

begin
  LParts := TNyxStrings.Create;
  LBytes := Length('nyx.storage.1|');
  try
    LParts.Add('nyx.storage.1|');
    Part(AStorage.Plan.Key);
    Number(Length(AStorage.Shapes));
    for LAt := 0 to High(AStorage.Shapes) do
    begin
      Context(AStorage.Contexts[LAt], 0);
      Number(Ord(AStorage.Shapes[LAt].Kind));

      if AStorage.Shapes[LAt].Kind = pskPrimitive then
      begin
        Number(Ord(AStorage.Origins[LAt]));
        Part(AStorage.References[LAt].Name);
      end;
    end;
    { Admit the final constructor's validation pass; getters later borrow the
      already validated immutable value without rescanning its text. }
    Tick(LBytes);
    Result := NyxPascalStorageKey(LParts.Join);
  finally
    LParts.Free;
  end;
end;

function TConstantQuery.StorageEvidenceReady(AStorage: TConstantStorage): Boolean;
var
  LContext: INyxPascalCompilerContext;
  LAlignments: INyxPascalContextCodeAlignments;
  LOptimizations: INyxPascalContextOptimizations;
  LAt, LRule: Integer;
begin
  Result := False;
  for LAt := 0 to High(AStorage.Contexts) do
  begin
    Tick;
    LContext := AStorage.Contexts[LAt];

    if (LContext.Family <> ppcFPC) or (LContext.Version.FullNumber = 0) or
      (LContext.Mode = ppmUnknown) or (LContext.Option('H') = ppkUnknown) then Exit;
    LAlignments := NyxPascalCodeAlignments(LContext);
    for LRule := 0 to LAlignments.CodeAlignmentCount - 1 do
    begin
      Tick;

      if LAlignments.CodeAlignmentAt(LRule).Activity = ppkUnknown then Exit;
    end;
    LOptimizations := NyxPascalOptimizations(LContext);
    for LRule := 0 to LOptimizations.OptimizationCount - 1 do
    begin
      Tick;

      if LOptimizations.OptimizationAt(LRule).Activity = ppkUnknown then Exit;
    end;
  end;
  Result := True;
end;

function TConstantQuery.StoredSize(const AContext: INyxPascalCompilerContext;
  const AKey: TNyxPascalStorageKey): TNyxPascalStorageFact;
var
  LValues: INyxPascalContextStorage;
  LKey: TNyxPascalStorageKey;
  LAt, LCount: Integer;
begin
  Tick;
  Result := NyxPascalStorageFact(psfsUnknown);

  if not Supports(AContext, INyxPascalContextStorage, LValues) then Exit;
  LCount := LValues.StorageCount;

  if (LCount < 0) or (LCount > NyxPascalStorageFactLimit) then
  begin
    raise ENyxPascalContext.Create('Structural evidence exceeds its bounded collection');
  end;
  for LAt := 0 to LCount - 1 do
  begin
    Tick;
    LKey := LValues.StorageKey(LAt);

    if Length(LKey.Value) = Length(AKey.Value) then
    begin
      { Charge the complete candidate comparison before examining its bytes.
        Indexed immutable values avoid a second hidden text-key lookup. }
      Tick(Length(LKey.Value));

      if LKey.Value = AKey.Value then
      begin
        Result := LValues.StorageValueAt(LAt);
        Result.Validate;
        Exit;
      end;
    end;
  end;
end;

function TConstantQuery.SizeValue(const AType: INyxPascalSemanticType;
  const ASource: INyxPascalBoundSymbol;
  const ASite: INyxPascalBindingSite;
  const AName: TNyxPascalSizeReference): TNyxPascalConstantValue;
var
  LIdentity: INyxPascalIntrinsicIdentity;
  LContext: INyxPascalCompilerContext;
  LFact: TNyxPascalSizeFact;
  LStorage: TConstantStorage;
  LStorageLease: INyxPascalConstantStorage;
  LShape: INyxPascalStorageShape;
  LStorageFact: TNyxPascalStorageFact;
  LRootContext: INyxPascalCompilerContext;
  LSourceScope: INyxPascalSourceSizeScope;
  LRequirement: INyxPascalCompilerRequirement;
  LSourceWork: Integer;
begin
  Result := TNyxPascalConstantValue.Unknown;

  if (AType <> nil) and (AType.Kind <> pstBuiltin) and (FResult.Reason = pbiNone) then
  begin
    if (AType.Definition <> nil) and
      (AType.Definition.Declaration.Flags * [psdfIncomplete, psdfConditional, psdfUnsupported] = []) and
      Supports(FCancellation, INyxPascalSourceSizeScope, LSourceScope) then
    begin
      LSourceWork := 0;
      try
        LStorageFact := LSourceScope.SourceSize(ASite, AName,
          FOptions.MaximumSteps - FResult.Work, LSourceWork, LRequirement);
      finally
        { The child scope has already admitted these steps to the aggregate.
          Retain them in this query's independent limit without charging twice. }
        Inc(FResult.Work, LSourceWork);
      end;

      if LStorageFact.State = psfsKnown then
      begin
        Depend(AType.Definition);
        Exit(TNyxPascalConstantValue.IntegerValue(LStorageFact.Bytes));
      end;

      if LRequirement <> nil then
      begin
        FResult.Requirements := NewNyxPascalRequirements(LRequirement);
        Refuse(pbiCompilerContext, AName.Name);
        Exit;
      end;
    end;
    LStorage := TConstantStorage.Create;
    LStorageLease := LStorage;
    try
      LShape := StorageShape(AType, ASource, ASite, LStorage);

      if (LShape <> nil) and (FResult.Reason = pbiNone) then
      begin
        LStorage.Plan := PlanNyxPascalStorage(LShape,
          TNyxPascalStorageOptions.Defaults, FCancellation);
        LStorage.Key := StorageEvidenceKey(LStorage);
        LRootContext := LStorage.Definitions[High(LStorage.Definitions)].World.ModuleAt(
          LStorage.Definitions[High(LStorage.Definitions)].ModuleIndex).Source.InitialContext;
        LStorageFact := NyxPascalStorageFact(psfsUnknown);

        if StorageEvidenceReady(LStorage) then
        begin
          LStorageFact := StoredSize(LRootContext, LStorage.Key);

          if (LStorageFact.State = psfsUnknown) and (NyxPascalBootstrapContext(LRootContext) <> nil) then
          begin
            LStorageFact := StoredSize(NyxPascalBootstrapContext(LRootContext), LStorage.Key);
          end;
        end;

        if LStorageFact.State = psfsKnown then
        begin
          Exit(TNyxPascalConstantValue.IntegerValue(LStorageFact.Bytes));
        end;
        FResult.Storage := LStorageLease;
        Refuse(pbiCompilerContext, LStorage.Definitions[High(LStorage.Definitions)].Declaration.Name);
      end;
    except
      on LError: ENyxPascalStorageLimit do
        raise ENyxPascalConstantLimit.Create(LError.Message);
    end;
    Exit;
  end;

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
              Result := SizeValue(LType, LSource, ASite, NyxPascalSizeReference(LNode.Name));
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
