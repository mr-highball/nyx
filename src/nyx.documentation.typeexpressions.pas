{ Detached declared type expressions, reusing Nyx's owned type grammar. MIT. }
unit nyx.documentation.typeexpressions;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.text, nyx.scheduler, nyx.pascal.lexing,
  nyx.pascal.types, nyx.pascal.work;

type
  { An invalid or excessive expression never publishes a partial syntax owner. }
  ENyxDocumentationTypeExpression = class(EArgumentException);
  { Cancellation is distinct from malformed authored type text. }
  ENyxDocumentationTypeExpressionCancelled = class(ENyxDocumentationTypeExpression);
  { The enclosing declaration supplies this role. Identical array syntax in a
    parameter and a type declaration does not establish identical type intent. }
  TNyxDocumentationTypeOccurrence = (ndtoParameter, ndtoResult,
    ndtoPropertyIndex, ndtoPropertyValue, ndtoDeclaration);
  { Existing lexer/type policies plus one shared work ceiling. Bytes, tokens and
    nodes are aggregate admitted storage, including abandoned attempts, not
    fresh limits for each expression. Depth keeps maximum-nesting meaning. }
  TNyxDocumentationTypeExpressionLimits = record
  private
    FLexical: TNyxPascalLexicalOptions;
    FTypes: TNyxPascalTypeOptions;
    FWork: Integer;
  public
    class function Defaults: TNyxDocumentationTypeExpressionLimits; static;
    function Bytes(AValue: Integer): TNyxDocumentationTypeExpressionLimits;
    function Tokens(AValue: Integer): TNyxDocumentationTypeExpressionLimits;
    function Nodes(AValue: Integer): TNyxDocumentationTypeExpressionLimits;
    function Depth(AValue: Integer): TNyxDocumentationTypeExpressionLimits;
    function Work(AValue: Integer): TNyxDocumentationTypeExpressionLimits;
    procedure Validate;
    property Lexical: TNyxPascalLexicalOptions read FLexical;
    property Types: TNyxPascalTypeOptions read FTypes;
    property MaximumWork: Integer read FWork;
  end;
  { Optional outward aggregate policy. No documentation references or registry
    types enter this unit. An operation borrows this policy only until Retire. }
  INyxDocumentationTypeAdmission = interface(IInterface)
    ['{DB455393-951F-4924-A995-022A343AA201}']
    procedure AdmitTypeWork(APhase: TNyxPascalAnalysisPhase; AUnits: Integer);
    function RemainingTypeBytes: Integer;
  end;
  { Immutable text and existing syntax over a copied, zero-based small window.
    Name uses are maximal qualified designators, generic bases/arguments and
    authored string types. Namespace components and opaque bounds remain exact
    syntax, without inferred nominal identity or a retained declaration owner. }
  INyxDocumentationTypeExpression = interface(IInterface)
    ['{DB455393-951F-4924-A995-022A343AA202}']
    function Text: TNyxText;
    function Occurrence: TNyxDocumentationTypeOccurrence;
    function Syntax: INyxPascalTypeSyntax;
    function NamedUseCount: Integer;
    function NamedUseNode(AIndex: Integer): Integer;
    function NamedUseArity(AIndex: Integer): Integer;
  end;
  { A construction-only scope owns shared counters and exact immutable-owner
    receipts. Every reference consumes work; independent equal owners consume
    storage fully. Retire releases all outward leases and refuses further work.
    Neither this scope nor cancellation is retained by an expression. }
  INyxDocumentationTypeConstruction = interface(INyxCancellationScope)
    ['{DB455393-951F-4924-A995-022A343AA203}']
    procedure Spend(AUnits: Integer = 1);
    procedure AdmitText(const AValue: TNyxText);
    function Limits: TNyxDocumentationTypeExpressionLimits;
    function Receipt: TNyxPascalAnalysisReceipt;
    procedure Retire;
  end;

function NewNyxDocumentationTypeConstruction(
  const ALimits: TNyxDocumentationTypeExpressionLimits;
  const ACancellation: INyxCancellationScope = nil;
  const AAdmission: INyxDocumentationTypeAdmission = nil): INyxDocumentationTypeConstruction;
{ Validate the sealed construction owner before trusting a public capability.
  This performs no allocation and grants no budget reset to alternate scopes. }
procedure CheckNyxDocumentationTypeConstruction(const AValue: INyxDocumentationTypeConstruction);
function NewNyxDocumentationTypeExpression(const AText: TNyxText;
  AOccurrence: TNyxDocumentationTypeOccurrence;
  const AConstruction: INyxDocumentationTypeConstruction): INyxDocumentationTypeExpression;
function CaptureNyxDocumentationTypeExpression(const AValue: INyxDocumentationTypeExpression;
  const AConstruction: INyxDocumentationTypeConstruction): INyxDocumentationTypeExpression;

implementation

uses nyx.bytes;

type
  { Private borrowed owner, used only under the public expression's lease. }
  TExpression = class;
  { A public alternate cannot earn reuse by advertising counts or another
    expression's private owner; exact public interface identity is checked too. }
  IExpressionOwner = interface(IInterface)
    ['{DB455393-951F-4924-A995-022A343AA204}']
    function Owner: TExpression;
  end;
  { Sole captured syntax owner. Its arrays contain ordinals only; there is no
    parser scope, supplier, declaration unit, editor or admission back-reference. }
  TExpression = class(TInterfacedObject, INyxDocumentationTypeExpression, IExpressionOwner)
  public
    Value: INyxPascalTypeSyntax;
    Role: TNyxDocumentationTypeOccurrence;
    NamedNodes, Arities: array of Integer;
    Policy: TNyxDocumentationTypeExpressionLimits;
    TextBytes: Integer;
    function Owner: TExpression;
    function Text: TNyxText;
    function Occurrence: TNyxDocumentationTypeOccurrence;
    function Syntax: INyxPascalTypeSyntax;
    function NamedUseCount: Integer;
    function NamedUseNode(AIndex: Integer): Integer;
    function NamedUseArity(AIndex: Integer): Integer;
  end;
  { Mutable counters exist only during construction. The existing parser's
    analysis calls reach the same scope for all expressions. Receipts retain
    exact private owners only and disappear when the operation retires. }
  TConstruction = class(TInterfacedObject, INyxCancellationScope,
    INyxDocumentationTypeConstruction, INyxPascalAnalysisBudget)
  private
    FPolicy: TNyxDocumentationTypeExpressionLimits;
    FAnalysis: INyxPascalAnalysisBudget;
    FOuter: INyxDocumentationTypeAdmission;
    FBytes, FTokens, FNodes: Integer;
    FCancelled, FRetired: Boolean;
    FOwners: array of INyxDocumentationTypeExpression;
    FOwnerCount: Integer;
    procedure Check;
    procedure Reserve(ABytes, ATokens, ANodes: Integer);
    procedure Remember(const AValue: INyxDocumentationTypeExpression);
    function Seen(const AValue: INyxDocumentationTypeExpression): Boolean;
  public
    constructor Create(const ALimits: TNyxDocumentationTypeExpressionLimits;
      const ACancellation: INyxCancellationScope;
      const AAdmission: INyxDocumentationTypeAdmission);
    procedure Cancel;
    function GetCancelled: Boolean;
    procedure Admit(APhase: TNyxPascalAnalysisPhase; AUnits: Integer = 1);
    procedure Spend(AUnits: Integer = 1);
    procedure AdmitText(const AValue: TNyxText);
    function Limits: TNyxDocumentationTypeExpressionLimits;
    function Receipt: TNyxPascalAnalysisReceipt;
    procedure Retire;
  end;
  { Borrowed access to this unit's construction state. Alternate implementations
    may not supply counters or receipt authority through the public interface. }
  IConstructionOwner = interface(IInterface)
    ['{DB455393-951F-4924-A995-022A343AA205}']
    function Construction: TConstruction;
  end;
  { A sealed extension provides the borrow without exposing mutable counters. }
  TOwnedConstruction = class(TConstruction, IConstructionOwner)
  public
    function Construction: TConstruction;
  end;

class function TNyxDocumentationTypeExpressionLimits.Defaults: TNyxDocumentationTypeExpressionLimits;
begin
  Result.FLexical := TNyxPascalLexicalOptions.Defaults;
  Result.FTypes := TNyxPascalTypeOptions.Defaults;
  Result.FWork := 2000000;
end;

procedure TNyxDocumentationTypeExpressionLimits.Validate;
begin
  FLexical.Validate;
  FTypes.Validate;

  if FWork < 1 then
    raise ENyxDocumentationTypeExpression.Create('Use a positive shared type-expression work allowance');
end;

function TNyxDocumentationTypeExpressionLimits.Bytes(AValue: Integer): TNyxDocumentationTypeExpressionLimits;
begin
  Result := Self;
  Result.FLexical := FLexical.UpToSourceBytes(AValue);
  Result.Validate;
end;

function TNyxDocumentationTypeExpressionLimits.Tokens(AValue: Integer): TNyxDocumentationTypeExpressionLimits;
begin
  Result := Self;
  Result.FLexical := FLexical.UpToTokens(AValue);
  Result.Validate;
end;

function TNyxDocumentationTypeExpressionLimits.Nodes(AValue: Integer): TNyxDocumentationTypeExpressionLimits;
begin
  Result := Self;
  Result.FTypes := FTypes.UpToNodes(AValue);
  Result.Validate;
end;

function TNyxDocumentationTypeExpressionLimits.Depth(AValue: Integer): TNyxDocumentationTypeExpressionLimits;
begin
  Result := Self;
  Result.FTypes := FTypes.UpToDepth(AValue);
  Result.Validate;
end;

function TNyxDocumentationTypeExpressionLimits.Work(AValue: Integer): TNyxDocumentationTypeExpressionLimits;
begin
  Result := Self;
  Result.FWork := AValue;
  Result.Validate;
end;

constructor TConstruction.Create(const ALimits: TNyxDocumentationTypeExpressionLimits;
  const ACancellation: INyxCancellationScope;
  const AAdmission: INyxDocumentationTypeAdmission);
begin
  inherited Create;
  ALimits.Validate;
  FPolicy := ALimits;
  FAnalysis := NewNyxPascalAnalysisBudget(ALimits.MaximumWork, ACancellation);
  FOuter := AAdmission;
  Check;
end;

procedure TConstruction.Check;
begin

  if GetCancelled then
    raise ENyxDocumentationTypeExpressionCancelled.Create('Type-expression construction cancelled or retired');
end;

procedure TConstruction.Cancel;
begin
  FCancelled := True;
end;

function TConstruction.GetCancelled: Boolean;
begin
  Result := FCancelled or FRetired or ((FAnalysis <> nil) and FAnalysis.Cancelled);
end;

procedure TConstruction.Reserve(ABytes, ATokens, ANodes: Integer);
begin
  Check;

  if (ABytes < 0) or (ATokens < 0) or (ANodes < 0) or
    (ABytes > FPolicy.Lexical.MaximumSourceBytes - FBytes) or
    (ATokens > FPolicy.Lexical.MaximumTokens - FTokens) or
    (ANodes > FPolicy.Types.MaximumNodes - FNodes) then
    raise ENyxDocumentationTypeExpression.Create('Shared type-expression storage allowance exhausted');

  if (FOuter <> nil) and (ABytes > FOuter.RemainingTypeBytes - FBytes) then
    raise ENyxDocumentationTypeExpression.Create('Remaining aggregate type-expression copy allowance exhausted');
  Inc(FBytes, ABytes);
  Inc(FTokens, ATokens);
  Inc(FNodes, ANodes);
  Check;
end;

procedure TConstruction.Admit(APhase: TNyxPascalAnalysisPhase; AUnits: Integer);
var
  LReceipt: TNyxPascalAnalysisReceipt;
begin
  Check;

  if (Ord(APhase) < Ord(Low(TNyxPascalAnalysisPhase))) or
    (Ord(APhase) > Ord(High(TNyxPascalAnalysisPhase))) then
    raise EArgumentException.Create('Unknown Pascal analysis work phase');
  LReceipt := FAnalysis.Receipt;

  if (AUnits < 1) or (AUnits > LReceipt.Maximum - LReceipt.Total) then
    raise ENyxDocumentationTypeExpression.Create('Shared type-expression work allowance exhausted');

  if APhase = papLexicalToken then
    Reserve(0, AUnits, 0)
  else if APhase = papTypeNode then
    Reserve(0, 0, AUnits);

  if FOuter <> nil then
    FOuter.AdmitTypeWork(APhase, AUnits);
  FAnalysis.Admit(APhase, AUnits);
end;

procedure TConstruction.Spend(AUnits: Integer);
begin
  Admit(papTypeProof, AUnits);
end;

procedure TConstruction.AdmitText(const AValue: TNyxText);
begin
  Check;

  if (Length(AValue) > FPolicy.Lexical.MaximumSourceBytes - FBytes) or
    ((FOuter <> nil) and (Length(AValue) > FOuter.RemainingTypeBytes - FBytes)) then
    raise ENyxDocumentationTypeExpression.Create('Copied metadata exceeds remaining construction bytes');
  Spend(Length(AValue) + 1);
  Reserve(NyxUTF8ByteCount(AValue), 0, 0);
end;

function TConstruction.Limits: TNyxDocumentationTypeExpressionLimits;
begin
  Check;
  Result := FPolicy;
end;

function TConstruction.Receipt: TNyxPascalAnalysisReceipt;
begin
  Result := FAnalysis.Receipt;
end;

procedure TConstruction.Retire;
begin
  FRetired := True;
  FOwners := nil;
  FOwnerCount := 0;
  FOuter := nil;
  FAnalysis.Retire;
end;

function TConstruction.Seen(const AValue: INyxDocumentationTypeExpression): Boolean;
var
  LAt: Integer;
begin
  Result := False;
  Spend;
  for LAt := 0 to FOwnerCount - 1 do
  begin
    Spend;

    if FOwners[LAt] = AValue then
      Exit(True);
  end;
end;

procedure TConstruction.Remember(const AValue: INyxDocumentationTypeExpression);
var
  LSlot, LCapacity: Integer;
begin
  Spend;
  LSlot := FOwnerCount;

  if LSlot >= FPolicy.Types.MaximumNodes then
    raise ENyxDocumentationTypeExpression.Create('Expression receipt allowance exhausted');

  if LSlot = Length(FOwners) then
  begin
    LCapacity := LSlot + 16 + LSlot div 2;

    if LCapacity > FPolicy.Types.MaximumNodes then
      LCapacity := FPolicy.Types.MaximumNodes;
    Spend(LSlot + LCapacity + 1);
    SetLength(FOwners, LCapacity);
  end;
  FOwners[LSlot] := AValue;
  Inc(FOwnerCount);
end;

function TOwnedConstruction.Construction: TConstruction;
begin
  Result := Self;
end;

function NewNyxDocumentationTypeConstruction(
  const ALimits: TNyxDocumentationTypeExpressionLimits;
  const ACancellation: INyxCancellationScope;
  const AAdmission: INyxDocumentationTypeAdmission): INyxDocumentationTypeConstruction;
begin
  Result := TOwnedConstruction.Create(ALimits, ACancellation, AAdmission);
end;

function ConstructionOwner(const AValue: INyxDocumentationTypeConstruction): TConstruction;
var
  LOwner: IConstructionOwner;
  LCanonical: INyxDocumentationTypeConstruction;
begin
  Result := nil;

  if not Supports(AValue, IConstructionOwner, LOwner) then
    raise ENyxDocumentationTypeExpression.Create('Use an owned shared type-expression construction');
  Result := LOwner.Construction;

  if Result = nil then
    raise ENyxDocumentationTypeExpression.Create('Missing type-expression construction owner');
  LCanonical := Result;

  if LCanonical <> AValue then
    raise ENyxDocumentationTypeExpression.Create('Alternate construction cannot borrow another owner');
  Result.Check;
end;

function TExpression.Owner: TExpression;
begin
  Result := Self;
end;

procedure CheckNyxDocumentationTypeConstruction(const AValue: INyxDocumentationTypeConstruction);
begin
  ConstructionOwner(AValue);
end;

function TExpression.Text: TNyxText;
begin
  Result := Value.Lexical.Source;
end;

function TExpression.Occurrence: TNyxDocumentationTypeOccurrence;
begin
  Result := Role;
end;

function TExpression.Syntax: INyxPascalTypeSyntax;
begin
  Result := Value;
end;

function TExpression.NamedUseCount: Integer;
begin
  Result := Length(NamedNodes);
end;

function TExpression.NamedUseNode(AIndex: Integer): Integer;
begin

  if (AIndex < 0) or (AIndex >= Length(NamedNodes)) then
    raise ENyxDocumentationTypeExpression.Create('Named type-use ordinal out of range');
  Result := NamedNodes[AIndex];
end;

function TExpression.NamedUseArity(AIndex: Integer): Integer;
begin
  NamedUseNode(AIndex);
  Result := Arities[AIndex];
end;

function NewNyxDocumentationTypeExpression(const AText: TNyxText;
  AOccurrence: TNyxDocumentationTypeOccurrence;
  const AConstruction: INyxDocumentationTypeConstruction): INyxDocumentationTypeExpression;
var
  LBudget: TConstruction;
  LOwner: TExpression;
  LLease: INyxDocumentationTypeExpression;
  LLexical: INyxPascalLexicalSnapshot;
  LNode, LParent: TNyxPascalTypeNode;
  LAt, LSlot, LChild, LArity, LTextBytes: Integer;
begin
  Result := nil;
  LBudget := ConstructionOwner(AConstruction);
  LBudget.Spend;

  if (Ord(AOccurrence) < 0) or (Ord(AOccurrence) > Ord(High(TNyxDocumentationTypeOccurrence))) then
    raise ENyxDocumentationTypeExpression.Create('Unknown declared type occurrence role');

  if Length(AText) > LBudget.FPolicy.Lexical.MaximumSourceBytes - LBudget.FBytes then
    raise ENyxDocumentationTypeExpression.Create('Type-expression copy exceeds remaining source bytes');

  if (LBudget.FOuter <> nil) and
    (Length(AText) > LBudget.FOuter.RemainingTypeBytes - LBudget.FBytes) then
    raise ENyxDocumentationTypeExpression.Create('Type-expression copy exceeds remaining aggregate bytes');
  LBudget.Spend(Length(AText) + 1);
  LTextBytes := NyxUTF8ByteCount(AText);
  LBudget.Reserve(LTextBytes, 0, 0);
  try
    LLexical := NewNyxPascalLexicalSnapshot(AText, LBudget.FPolicy.Lexical, AConstruction);
  except
    on E: ENyxPascalAnalysisCancelled do
      raise ENyxDocumentationTypeExpressionCancelled.Create(E.Message);
  end;

  if LLexical.DiagnosticCount <> 0 then
    raise ENyxDocumentationTypeExpression.Create('A type expression needs complete lexical syntax');
  LOwner := TExpression.Create;
  LLease := LOwner;
  LOwner.Role := AOccurrence;
  LOwner.Policy := LBudget.FPolicy;
  LOwner.TextBytes := LTextBytes;
  try
    LOwner.Value := NewNyxPascalTypeSyntax(LLexical, NyxPascalSpan(0, LLexical.Scalars),
      LBudget.FPolicy.Types, AConstruction);
  except
    on E: ENyxPascalAnalysisCancelled do
      raise ENyxDocumentationTypeExpressionCancelled.Create(E.Message);
  end;

  if not LOwner.Value.Complete or (LOwner.Value.DiagnosticCount <> 0) then
    raise ENyxDocumentationTypeExpression.Create('Type-expression grammar is incomplete or requires its original declaration owner');
  LBudget.Spend(LOwner.Value.Count * 2 + 1);
  SetLength(LOwner.NamedNodes, LOwner.Value.Count);
  SetLength(LOwner.Arities, LOwner.Value.Count);
  LSlot := 0;
  for LAt := 0 to LOwner.Value.Count - 1 do
  begin
    LBudget.Spend;
    LNode := LOwner.Value.Node(LAt);

    if not (LNode.Kind in [ptxName, ptxQualified, ptxString]) then
      Continue;

    if LNode.Parent >= 0 then
    begin
      LBudget.Spend;
      LParent := LOwner.Value.Node(LNode.Parent);

      if (LParent.Kind = ptxQualified) and (LNode.Role in [ptrBase, ptrMember]) then
        Continue;
    end;
    LArity := 0;

    if LNode.Parent >= 0 then
    begin
      LBudget.Spend;
      LParent := LOwner.Value.Node(LNode.Parent);

      if (LParent.Kind = ptxGeneric) and (LNode.Role = ptrBase) then
      begin
        LChild := LParent.FirstChild;
        while LChild >= 0 do
        begin
          LBudget.Spend;
          LParent := LOwner.Value.Node(LChild);

          if LParent.Role = ptrArgument then
            Inc(LArity);
          LChild := LParent.NextSibling;
        end;
      end;
    end;
    LOwner.NamedNodes[LSlot] := LAt;
    LOwner.Arities[LSlot] := LArity;
    Inc(LSlot);
  end;
  LBudget.Spend(LSlot * 2 + 1);
  SetLength(LOwner.NamedNodes, LSlot);
  SetLength(LOwner.Arities, LSlot);
  LBudget.Remember(LLease);
  LBudget.Check;
  Result := LLease;
end;

function CaptureNyxDocumentationTypeExpression(const AValue: INyxDocumentationTypeExpression;
  const AConstruction: INyxDocumentationTypeConstruction): INyxDocumentationTypeExpression;
var
  LBudget: TConstruction;
  LPrivate: IExpressionOwner;
  LOwner: TExpression;
  LCanonical, LFrozen: INyxDocumentationTypeExpression;
  LText: TNyxText;
  LRole: TNyxDocumentationTypeOccurrence;
  LCount, LAt: Integer;
begin
  Result := nil;
  LBudget := ConstructionOwner(AConstruction);
  LBudget.Spend;

  if AValue = nil then
    raise ENyxDocumentationTypeExpression.Create('A declared type expression is required');

  if Supports(AValue, IExpressionOwner, LPrivate) then
  begin
    LOwner := LPrivate.Owner;

    if LOwner <> nil then
    begin
      LCanonical := LOwner;

      if (LCanonical = AValue) and
        (LOwner.Policy.Types.MaximumDepth = LBudget.FPolicy.Types.MaximumDepth) and
        (LOwner.Policy.Lexical.AllowUnicodeIdentifiers = LBudget.FPolicy.Lexical.AllowUnicodeIdentifiers) and
        (LOwner.Policy.Lexical.MaximumCommentDepth = LBudget.FPolicy.Lexical.MaximumCommentDepth) and
        (LOwner.Policy.Lexical.AllowNestedComments = LBudget.FPolicy.Lexical.AllowNestedComments) then
      begin

        if not LBudget.Seen(LCanonical) then
        begin
          LBudget.Reserve(LOwner.TextBytes, LOwner.Value.Lexical.Count, LOwner.Value.Count);
          LBudget.Remember(LCanonical);
        end;
        LBudget.Check;
        Result := LCanonical;
        Exit;
      end;
    end;
  end;
  LCount := AValue.NamedUseCount;

  if (LCount < 0) or (LCount > LBudget.FPolicy.Types.MaximumNodes - LBudget.FNodes) then
    raise ENyxDocumentationTypeExpression.Create('Alternate type-expression name allowance exhausted');
  LBudget.Spend;
  LText := AValue.Text;
  LBudget.Spend;
  LRole := AValue.Occurrence;
  LFrozen := NewNyxDocumentationTypeExpression(LText, LRole, AConstruction);

  if LFrozen.NamedUseCount <> LCount then
    raise ENyxDocumentationTypeExpression.Create('Alternate expression named-use count disagrees with its text');
  for LAt := 0 to LCount - 1 do
  begin
    LBudget.Spend;

    if (AValue.NamedUseNode(LAt) <> LFrozen.NamedUseNode(LAt)) or
      (AValue.NamedUseArity(LAt) <> LFrozen.NamedUseArity(LAt)) then
      raise ENyxDocumentationTypeExpression.Create('Alternate expression name occurrence disagrees with its text');
  end;
  LBudget.Spend;

  if (AValue.NamedUseCount <> LCount) or (AValue.Text <> LText) or (AValue.Occurrence <> LRole) then
    raise ENyxDocumentationTypeExpression.Create('Alternate expression changed before publication');
  LBudget.Check;
  Result := LFrozen;
end;

end.
