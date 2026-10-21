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
    ndtoPropertyIndex, ndtoPropertyValue, ndtoDeclaration,
    ndtoImplementedInterface, ndtoField, ndtoVariable, ndtoConstant);
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
  { Construction-only windows over one immutable Pascal text value. Two checked
    scalar/storage positions avoid repeating earlier Unicode scans for forward
    or repeated windows. Every copied window still reserves its exact bytes;
    backwards requests pay for any prefix that must actually be scanned again.
    Retire releases both source and construction. Published metadata must never
    retain this temporary owner, which supplies no source/binding authority. }
  INyxDocumentationTextWindows = interface(IInterface)
    ['{DB455393-951F-4924-A995-022A343AA206}']
    function CopyWindow(const ASpan: TNyxPascalSpan): TNyxText;
    procedure Retire;
  end;

function NewNyxDocumentationTypeConstruction(
  const ALimits: TNyxDocumentationTypeExpressionLimits;
  const ACancellation: INyxCancellationScope = nil;
  const AAdmission: INyxDocumentationTypeAdmission = nil): INyxDocumentationTypeConstruction;
{ Validate the sealed construction owner before trusting a public capability.
  This performs no allocation and grants no budget reset to alternate scopes. }
procedure CheckNyxDocumentationTypeConstruction(const AValue: INyxDocumentationTypeConstruction);
{ Reserve a known UTF-8 byte count before allocating formatted metadata, using
  the same sealed construction and outward aggregate allowance as type windows.
  This admits one operation and bytes only: the caller separately admits any
  scalar inspection/formatting work and supplies the exact eventual byte count.
  It cannot reset limits or grant authority to an alternate construction. }
procedure ReserveNyxDocumentationTypeBytes(ABytes: Integer;
  const AConstruction: INyxDocumentationTypeConstruction);
function NewNyxDocumentationTextWindows(const ASource: TNyxText;
  const AConstruction: INyxDocumentationTypeConstruction): INyxDocumentationTextWindows;
{ Copy one scalar window only after its exact UTF-8 byte cost is reserved.
  Prefix/window Unicode scans consume the caller's shared work allowance.
  The source is a synchronous borrowed text value, never a retained unit owner. }
function CopyNyxDocumentationTypeTextWindow(const ASource: TNyxText;
  const ASpan: TNyxPascalSpan;
  const AConstruction: INyxDocumentationTypeConstruction): TNyxText;
function NewNyxDocumentationTypeExpression(const AText: TNyxText;
  AOccurrence: TNyxDocumentationTypeOccurrence;
  const AConstruction: INyxDocumentationTypeConstruction): INyxDocumentationTypeExpression;
function CaptureNyxDocumentationTypeExpression(const AValue: INyxDocumentationTypeExpression;
  const AConstruction: INyxDocumentationTypeConstruction): INyxDocumentationTypeExpression;

implementation

uses nyx.bytes;

type
  { Identity-only creation arena. Expressions may retain this empty token after
    their construction retires; it holds no scope, source, policy or callback. }
  TExpressionArena = class(TInterfacedObject);
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
    { Written once before first publication. Reuse still requires the exact
      ordinary interface at this slot in the active construction's vector. }
    CreationArena: IInterface;
    CreationSlot: Integer;
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
    FArena: IInterface;
    procedure Check;
    procedure Reserve(ABytes, ATokens, ANodes: Integer);
    procedure Remember(const AValue: INyxDocumentationTypeExpression;
      ACreated: TExpression = nil);
    function Seen(const AValue: INyxDocumentationTypeExpression;
      AOwner: TExpression): Boolean;
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
  { A source value and its two checkpoints belong to this exact operation.
    Checkpoints commit only after a complete admitted copy. Holding a retired
    cursor cannot revive a construction or retain its source/cancellation lease. }
  TTextWindows = class(TInterfacedObject, INyxDocumentationTextWindows)
  private
    FSource: TNyxText;
    FConstruction: INyxDocumentationTypeConstruction;
    FStartScalar, FStartUnit, FEndScalar, FEndUnit: Integer;
  public
    constructor Create(const ASource: TNyxText;
      const AConstruction: INyxDocumentationTypeConstruction);
    destructor Destroy; override;
    function CopyWindow(const ASpan: TNyxPascalSpan): TNyxText;
    procedure Retire;
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
  FArena := TExpressionArena.Create;
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
var
  LSelf: INyxDocumentationTypeConstruction;
  LAnalysis: INyxPascalAnalysisBudget;
begin
  LSelf := Self;
  LAnalysis := FAnalysis;
  Result := FCancelled or FRetired;

  if not Result and (LAnalysis <> nil) then
    Result := LAnalysis.Cancelled;
  { A parent's cancellation getter is outward code too. Retirement during that
    call must remain visible even when the parent itself returns False. }
  Result := Result or FCancelled or FRetired;
end;

procedure TConstruction.Reserve(ABytes, ATokens, ANodes: Integer);
var
  LSelf: INyxDocumentationTypeConstruction;
  LOuter: INyxDocumentationTypeAdmission;
  LRemaining: Integer;
  procedure CheckStorage;
  begin

    if (ABytes < 0) or (ATokens < 0) or (ANodes < 0) or
      (ABytes > FPolicy.Lexical.MaximumSourceBytes - FBytes) or
      (ATokens > FPolicy.Lexical.MaximumTokens - FTokens) or
      (ANodes > FPolicy.Types.MaximumNodes - FNodes) then
      raise ENyxDocumentationTypeExpression.Create('Shared type-expression storage allowance exhausted');
  end;
begin
  LSelf := Self;
  LOuter := FOuter;
  Check;
  CheckStorage;

  if LOuter <> nil then
  begin
    LRemaining := LOuter.RemainingTypeBytes;
    Check;
    { The outward getter may have used this same construction recursively.
      Recheck local counters before committing the original reservation. }
    CheckStorage;

    if LRemaining < FBytes then
      raise ENyxDocumentationTypeExpression.Create('Remaining aggregate type-expression copy allowance exhausted');

    if ABytes > LRemaining - FBytes then
      raise ENyxDocumentationTypeExpression.Create('Remaining aggregate type-expression copy allowance exhausted');
  end;
  Inc(FBytes, ABytes);
  Inc(FTokens, ATokens);
  Inc(FNodes, ANodes);
  Check;
end;

procedure TConstruction.Admit(APhase: TNyxPascalAnalysisPhase; AUnits: Integer);
var
  LReceipt: TNyxPascalAnalysisReceipt;
  LSelf: INyxDocumentationTypeConstruction;
  LAnalysis: INyxPascalAnalysisBudget;
  LOuter: INyxDocumentationTypeAdmission;
begin
  { Admission is outward code. Keep the receiver and its borrowed owners alive
    if that code releases the caller's last reference or retires this scope. }
  LSelf := Self;
  LAnalysis := FAnalysis;
  LOuter := FOuter;
  Check;

  if (Ord(APhase) < Ord(Low(TNyxPascalAnalysisPhase))) or
    (Ord(APhase) > Ord(High(TNyxPascalAnalysisPhase))) then
    raise EArgumentException.Create('Unknown Pascal analysis work phase');
  LReceipt := LAnalysis.Receipt;

  if (AUnits < 1) or (AUnits > LReceipt.Maximum - LReceipt.Total) then
    raise ENyxDocumentationTypeExpression.Create('Shared type-expression work allowance exhausted');

  if APhase = papLexicalToken then
    Reserve(0, AUnits, 0)
  else if APhase = papTypeNode then
    Reserve(0, 0, AUnits);

  if LOuter <> nil then
  begin
    LOuter.AdmitTypeWork(APhase, AUnits);
    Check;
  end;
  LAnalysis.Admit(APhase, AUnits);
  Check;
end;

procedure TConstruction.Spend(AUnits: Integer);
begin
  Admit(papTypeProof, AUnits);
end;

procedure TConstruction.AdmitText(const AValue: TNyxText);
var
  LSelf: INyxDocumentationTypeConstruction;
  LOuter: INyxDocumentationTypeAdmission;
  LText: TNyxText;
  LRemaining: Integer;
begin
  LSelf := Self;
  LOuter := FOuter;
  LText := AValue;
  Check;

  if Length(LText) > FPolicy.Lexical.MaximumSourceBytes - FBytes then
    raise ENyxDocumentationTypeExpression.Create('Copied metadata exceeds remaining construction bytes');

  if LOuter <> nil then
  begin
    LRemaining := LOuter.RemainingTypeBytes;
    Check;

    if LRemaining < FBytes then
      raise ENyxDocumentationTypeExpression.Create('Copied metadata exceeds remaining construction bytes');

    if Length(LText) > LRemaining - FBytes then
      raise ENyxDocumentationTypeExpression.Create('Copied metadata exceeds remaining construction bytes');
  end;
  Spend(Length(LText) + 1);
  Reserve(NyxUTF8ByteCount(LText), 0, 0);
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
var
  LSelf: INyxDocumentationTypeConstruction;
begin
  { Releasing a policy may execute its destructor and drop the caller's last
    scope reference. Keep this receiver alive through its complete retirement. }
  LSelf := Self;
  FRetired := True;
  FOwners := nil;
  FOwnerCount := 0;
  FArena := nil;
  FOuter := nil;
  FAnalysis.Retire;
end;

function TConstruction.Seen(const AValue: INyxDocumentationTypeExpression;
  AOwner: TExpression): Boolean;
var
  LAt: Integer;
begin
  Result := False;
  Spend;

  if AOwner.CreationArena = FArena then
  begin
    Spend(3);
    LAt := AOwner.CreationSlot;

    if (LAt < 0) or (LAt >= FOwnerCount) then
      raise ENyxDocumentationTypeExpression.Create('A creation receipt is outside its retained owner vector');

    if FOwners[LAt] <> AValue then
      raise ENyxDocumentationTypeExpression.Create('A creation receipt disagrees with its exact retained expression');
    Exit(True);
  end;
  { Foreign constructions retain full charged lookup. Equal text and an old
    creation arena never supply a receipt in a different construction. }
  LAt := 0;
  while LAt < FOwnerCount do
  begin
    Spend;

    if FOwners[LAt] = AValue then
      Exit(True);
    Inc(LAt);
  end;
end;

procedure TConstruction.Remember(const AValue: INyxDocumentationTypeExpression;
  ACreated: TExpression);
var
  LSlot, LCapacity: Integer;
  LValue: INyxDocumentationTypeExpression;
begin
  LValue := AValue;
  Spend;

  if ACreated <> nil then
  begin
    Spend(3);

    if ((ACreated as INyxDocumentationTypeExpression) <> LValue) or
      (ACreated.CreationArena <> nil) then
      raise ENyxDocumentationTypeExpression.Create('Only a new exact expression may receive a creation receipt');
  end;
  while True do
  begin
    LSlot := FOwnerCount;

    if LSlot >= FPolicy.Types.MaximumNodes then
      raise ENyxDocumentationTypeExpression.Create('Expression receipt allowance exhausted');

    if LSlot < Length(FOwners) then
      Break;
    LCapacity := LSlot + 16 + LSlot div 2;

    if LCapacity > FPolicy.Types.MaximumNodes then
      LCapacity := FPolicy.Types.MaximumNodes;
    Spend(LSlot + LCapacity + 1);
    { Growth admission may append or grow recursively. Observe the current
      count after that callback, before allocating or stamping the new owner. }
    LSlot := FOwnerCount;

    if LSlot < Length(FOwners) then
      Break;

    if LSlot >= LCapacity then
      Continue;
    SetLength(FOwners, LCapacity);
    Break;
  end;
  FOwners[LSlot] := LValue;
  Inc(FOwnerCount);

  if ACreated <> nil then
  begin
    ACreated.CreationArena := FArena;
    ACreated.CreationSlot := LSlot;
  end;
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

procedure ReserveNyxDocumentationTypeBytes(ABytes: Integer;
  const AConstruction: INyxDocumentationTypeConstruction);
var
  LConstruction: INyxDocumentationTypeConstruction;
  LBudget: TConstruction;
begin
  { Outward byte admission may release the caller's slot or retire this scope.
    Keep the sealed owner alive and reuse its existing refusal boundaries. }
  LConstruction := AConstruction;
  LBudget := ConstructionOwner(LConstruction);
  LBudget.Spend;
  LBudget.Reserve(ABytes, 0, 0);
end;

constructor TTextWindows.Create(const ASource: TNyxText;
  const AConstruction: INyxDocumentationTypeConstruction);
var
  LBudget: TConstruction;
begin
  inherited Create;
  LBudget := ConstructionOwner(AConstruction);
  LBudget.Spend;
  LBudget.Check;

  { Retaining a Pascal string value does not copy its character storage. The
    existing byte allowance governs the windows actually copied, not unrelated
    source prefixes; every scalar visited still consumes shared work. }
  FSource := ASource;
  FConstruction := AConstruction;
  FStartUnit := 1;
  FEndUnit := 1;
end;

destructor TTextWindows.Destroy;
begin
  Retire;
  inherited Destroy;
end;

procedure TTextWindows.Retire;
begin
  FSource := '';
  FConstruction := nil;
  FStartScalar := 0;
  FStartUnit := 1;
  FEndScalar := 0;
  FEndUnit := 1;
end;

function NewNyxDocumentationTextWindows(const ASource: TNyxText;
  const AConstruction: INyxDocumentationTypeConstruction): INyxDocumentationTextWindows;
var
  LCandidate: INyxDocumentationTextWindows;
  LConstruction: INyxDocumentationTypeConstruction;
  LSource: TNyxText;
begin
  Result := nil;
  LConstruction := AConstruction;
  LSource := ASource;
  CheckNyxDocumentationTypeConstruction(LConstruction);
  LCandidate := TTextWindows.Create(LSource, LConstruction);
  CheckNyxDocumentationTypeConstruction(LConstruction);
  Result := LCandidate;
end;

function TTextWindows.CopyWindow(const ASpan: TNyxPascalSpan): TNyxText;
var
  LBudget: TConstruction;
  LConstruction: INyxDocumentationTypeConstruction;
  LIndex, LOrdinal, LStart, LScalar, LBytes, LWidth: Integer;
  LCandidate: TNyxText;
  procedure CheckLive;
  begin
    LBudget.Check;

    if FConstruction <> LConstruction then
      raise ENyxDocumentationTypeExpressionCancelled.Create('Text windows retired during copy');
  end;
begin
  Result := '';

  if FConstruction = nil then
    raise ENyxDocumentationTypeExpressionCancelled.Create('Text windows retired');
  { An outward admission may retire this cursor and release its last stored
    construction lease. Keep the active borrow alive until the call unwinds. }
  LConstruction := FConstruction;
  LBudget := ConstructionOwner(LConstruction);
  LBudget.Spend;
  CheckLive;

  if (ASpan.Start < 0) or (ASpan.Length < 0) or
    (ASpan.Length > High(Integer) - ASpan.Start) or
    (ASpan.Start > Length(FSource)) or
    (ASpan.Length > LBudget.FPolicy.Lexical.MaximumSourceBytes - LBudget.FBytes) then
    raise ENyxDocumentationTypeExpression.Create('Text window exceeds its scalar or remaining copy allowance');
  LIndex := 1;
  LOrdinal := 0;

  if FStartScalar <= ASpan.Start then
  begin
    LIndex := FStartUnit;
    LOrdinal := FStartScalar;
  end;

  if (FEndScalar <= ASpan.Start) and (FEndScalar >= LOrdinal) then
  begin
    LIndex := FEndUnit;
    LOrdinal := FEndScalar;
  end;
  while LOrdinal < ASpan.Start do
  begin
    LBudget.Spend;
    CheckLive;

    if not NyxNextScalar(FSource, LIndex, LScalar) then
      raise ENyxDocumentationTypeExpression.Create('Text window starts outside well-formed Unicode source');
    Inc(LOrdinal);
  end;
  LStart := LIndex;
  LBytes := 0;
  while LOrdinal < ASpan.Finish do
  begin
    LBudget.Spend;
    CheckLive;

    if not NyxNextScalar(FSource, LIndex, LScalar) then
      raise ENyxDocumentationTypeExpression.Create('Text window ends outside well-formed Unicode source');

    if LScalar < $80 then
      LWidth := 1
    else if LScalar < $800 then
      LWidth := 2
    else if LScalar < $10000 then
      LWidth := 3
    else
      LWidth := 4;

    if LWidth > LBudget.FPolicy.Lexical.MaximumSourceBytes - LBudget.FBytes - LBytes then
      raise ENyxDocumentationTypeExpression.Create('Text window exceeds its remaining UTF-8 copy allowance');
    Inc(LBytes, LWidth);
    Inc(LOrdinal);
  end;
  { Exact byte reservation precedes allocation even on a repeated copy. Reuse
    saves only scans already proved over this immutable value, never storage. }
  LBudget.Reserve(LBytes, 0, 0);
  CheckLive;
  LCandidate := Copy(FSource, LStart, LIndex - LStart);
  CheckLive;
  FStartScalar := ASpan.Start;
  FStartUnit := LStart;
  FEndScalar := LOrdinal;
  FEndUnit := LIndex;
  Result := LCandidate;
end;

function CopyNyxDocumentationTypeTextWindow(const ASource: TNyxText;
  const ASpan: TNyxPascalSpan;
  const AConstruction: INyxDocumentationTypeConstruction): TNyxText;
var
  LBudget: TConstruction;
  LIndex, LOrdinal, LStart, LScalar, LBytes, LWidth: Integer;
  LCandidate: TNyxText;
begin
  Result := '';
  LBudget := ConstructionOwner(AConstruction);
  LBudget.Spend;

  if (ASpan.Start < 0) or (ASpan.Length < 0) or
    (ASpan.Length > High(Integer) - ASpan.Start) or
    (ASpan.Start > Length(ASource)) or
    (ASpan.Length > LBudget.FPolicy.Lexical.MaximumSourceBytes - LBudget.FBytes) then
    raise ENyxDocumentationTypeExpression.Create('Text window exceeds its scalar or remaining copy allowance');
  LIndex := 1;
  LOrdinal := 0;
  while LOrdinal < ASpan.Start do
  begin
    LBudget.Spend;

    if not NyxNextScalar(ASource, LIndex, LScalar) then
      raise ENyxDocumentationTypeExpression.Create('Text window starts outside well-formed Unicode source');
    Inc(LOrdinal);
  end;
  LStart := LIndex;
  LBytes := 0;
  while LOrdinal < ASpan.Finish do
  begin
    LBudget.Spend;

    if not NyxNextScalar(ASource, LIndex, LScalar) then
      raise ENyxDocumentationTypeExpression.Create('Text window ends outside well-formed Unicode source');

    if LScalar < $80 then
      LWidth := 1
    else if LScalar < $800 then
      LWidth := 2
    else if LScalar < $10000 then
      LWidth := 3
    else
      LWidth := 4;

    if LWidth > LBudget.FPolicy.Lexical.MaximumSourceBytes - LBudget.FBytes - LBytes then
      raise ENyxDocumentationTypeExpression.Create('Text window exceeds its remaining UTF-8 copy allowance');
    Inc(LBytes, LWidth);
    Inc(LOrdinal);
  end;
  { Reserve also checks the outward remaining allowance. No source substring has
    been allocated yet, including when a preceding window consumed the budget. }
  LBudget.Reserve(LBytes, 0, 0);
  LBudget.Check;
  LCandidate := Copy(ASource, LStart, LIndex - LStart);
  LBudget.Check;
  Result := LCandidate;
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
  LConstruction: INyxDocumentationTypeConstruction;
  LOuter: INyxDocumentationTypeAdmission;
  LText: TNyxText;
  LOwner: TExpression;
  LLease: INyxDocumentationTypeExpression;
  LLexical: INyxPascalLexicalSnapshot;
  LNode, LParent: TNyxPascalTypeNode;
  LAt, LSlot, LChild, LArity, LTextBytes, LRemaining: Integer;
begin
  LConstruction := AConstruction;
  LText := AText;
  LBudget := ConstructionOwner(LConstruction);
  LBudget.Spend;

  if (Ord(AOccurrence) < 0) or (Ord(AOccurrence) > Ord(High(TNyxDocumentationTypeOccurrence))) then
    raise ENyxDocumentationTypeExpression.Create('Unknown declared type occurrence role');

  if Length(LText) > LBudget.FPolicy.Lexical.MaximumSourceBytes - LBudget.FBytes then
    raise ENyxDocumentationTypeExpression.Create('Type-expression copy exceeds remaining source bytes');

  LOuter := LBudget.FOuter;

  if LOuter <> nil then
  begin
    LRemaining := LOuter.RemainingTypeBytes;
    LBudget.Check;

    if LRemaining < LBudget.FBytes then
      raise ENyxDocumentationTypeExpression.Create('Type-expression copy exceeds remaining aggregate bytes');

    if Length(LText) > LRemaining - LBudget.FBytes then
      raise ENyxDocumentationTypeExpression.Create('Type-expression copy exceeds remaining aggregate bytes');
  end;
  LBudget.Spend(Length(LText) + 1);
  LTextBytes := NyxUTF8ByteCount(LText);
  LBudget.Reserve(LTextBytes, 0, 0);
  try
    LLexical := NewNyxPascalLexicalSnapshot(LText, LBudget.FPolicy.Lexical, LConstruction);
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
      LBudget.FPolicy.Types, LConstruction);
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
  LBudget.Remember(LLease, LOwner);
  LBudget.Check;
  Result := LLease;
end;

function CaptureNyxDocumentationTypeExpression(const AValue: INyxDocumentationTypeExpression;
  const AConstruction: INyxDocumentationTypeConstruction): INyxDocumentationTypeExpression;
var
  LBudget: TConstruction;
  LConstruction: INyxDocumentationTypeConstruction;
  LPrivate: IExpressionOwner;
  LOwner: TExpression;
  LInput, LCanonical, LFrozen: INyxDocumentationTypeExpression;
  LText: TNyxText;
  LRole: TNyxDocumentationTypeOccurrence;
  LCount, LAt: Integer;
begin
  LInput := AValue;
  LConstruction := AConstruction;
  LBudget := ConstructionOwner(LConstruction);
  LBudget.Spend;

  if LInput = nil then
    raise ENyxDocumentationTypeExpression.Create('A declared type expression is required');

  if Supports(LInput, IExpressionOwner, LPrivate) then
  begin
    LOwner := LPrivate.Owner;

    if LOwner <> nil then
    begin
      LCanonical := LOwner;

      if (LCanonical = LInput) and
        (LOwner.Policy.Types.MaximumDepth = LBudget.FPolicy.Types.MaximumDepth) and
        (LOwner.Policy.Lexical.AllowUnicodeIdentifiers = LBudget.FPolicy.Lexical.AllowUnicodeIdentifiers) and
        (LOwner.Policy.Lexical.MaximumCommentDepth = LBudget.FPolicy.Lexical.MaximumCommentDepth) and
        (LOwner.Policy.Lexical.AllowNestedComments = LBudget.FPolicy.Lexical.AllowNestedComments) then
      begin

        if not LBudget.Seen(LCanonical, LOwner) then
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
  LCount := LInput.NamedUseCount;

  if (LCount < 0) or (LCount > LBudget.FPolicy.Types.MaximumNodes - LBudget.FNodes) then
    raise ENyxDocumentationTypeExpression.Create('Alternate type-expression name allowance exhausted');
  LBudget.Spend;
  LText := LInput.Text;
  LBudget.Spend;
  LRole := LInput.Occurrence;
  LFrozen := NewNyxDocumentationTypeExpression(LText, LRole, LConstruction);

  if LFrozen.NamedUseCount <> LCount then
    raise ENyxDocumentationTypeExpression.Create('Alternate expression named-use count disagrees with its text');
  for LAt := 0 to LCount - 1 do
  begin
    LBudget.Spend;

    if (LInput.NamedUseNode(LAt) <> LFrozen.NamedUseNode(LAt)) or
      (LInput.NamedUseArity(LAt) <> LFrozen.NamedUseArity(LAt)) then
      raise ENyxDocumentationTypeExpression.Create('Alternate expression name occurrence disagrees with its text');
  end;
  LBudget.Spend;

  if (LInput.NamedUseCount <> LCount) or (LInput.Text <> LText) or (LInput.Occurrence <> LRole) then
    raise ENyxDocumentationTypeExpression.Create('Alternate expression changed before publication');
  LBudget.Check;
  Result := LFrozen;
end;

end.
