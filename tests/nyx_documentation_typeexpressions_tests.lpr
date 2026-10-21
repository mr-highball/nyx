{ Rich declared signatures, shared admission and mandatory source windows. MIT. }
program nyx_documentation_typeexpressions_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.bytes, nyx.scheduler, nyx.pascal.lexing,
  nyx.pascal.syntax, nyx.pascal.types, nyx.pascal.work,
  nyx.documentation.typeexpressions, nyx.documentation,
  nyx.documentation.registration, nyx.documentation.registry,
  nyx.catalog, nyx.schema, nyx.callbacks, nyx.test.documentation
  {$ifdef PAS2JS}, Web{$else}, Classes{$endif};

type
  { A small outer allowance is independent of the child's larger local default.
    Counters prove refusal before later parsing rather than only final framing. }
  TOuterAdmission = class(TInterfacedObject, INyxDocumentationTypeAdmission)
  public
    Bytes, Work: Integer;
    procedure AdmitTypeWork(APhase: TNyxPascalAnalysisPhase; AUnits: Integer);
    function RemainingTypeBytes: Integer;
  end;
  { One-name inputs have no large type-proof copy before receipt growth. This
    policy reenters at that admitted growth, or releases the caller's last
    input/scope handles. It owns no expression or construction back-reference. }
  TReceiptAdmission = class(TInterfacedObject, INyxDocumentationTypeAdmission)
  public
    GrowArmed, DropArmed: Boolean;
    GrowthCalls: Integer;
    destructor Destroy; override;
    procedure AdmitTypeWork(APhase: TNyxPascalAnalysisPhase; AUnits: Integer);
    function RemainingTypeBytes: Integer;
  end;
  { A parent may retire the child while returning False itself. The child must
    reobserve its own retired state before publishing or admitting more work. }
  TReceiptCancellation = class(TInterfacedObject, INyxCancellationScope)
  public
    RetireOnRead, CancelledValue: Boolean;
    RetireReads: Integer;
    procedure Cancel;
    function GetCancelled: Boolean;
  end;
  { Mutable expression supplier exposes borrowed-read counters. Capture must
    parse its copied text and never retain its source syntax or executable self. }
  TAlternateExpression = class(TInterfacedObject, INyxDocumentationTypeExpression)
  public
    Value: INyxDocumentationTypeExpression;
    TextReads, NodeReads: Integer;
    function Text: TNyxText;
    function Occurrence: TNyxDocumentationTypeOccurrence;
    function Syntax: INyxPascalTypeSyntax;
    function NamedUseCount: Integer;
    function NamedUseNode(AIndex: Integer): Integer;
    function NamedUseArity(AIndex: Integer): Integer;
  end;
  { A binding supplier refuses its fluent method. The capture path must build
    one private vector rather than perform B immutable whole-vector copies. }
  TAlternateUse = class(TInterfacedObject, INyxDocumentationTypeUse)
  public
    Value: INyxDocumentationTypeUse;
    SuppliedBinding: TNyxDocumentationRef;
    Reads, MutateAt: Integer;
    function Reference: TNyxDocumentationRef;
    function Expression: INyxDocumentationTypeExpression;
    function BindingCount: Integer;
    function BindingAt(AIndex: Integer): TNyxDocumentationRef;
    function WithBinding(AIndex: Integer; const AReference: TNyxDocumentationRef): INyxDocumentationTypeUse;
  end;
  { The parameter deliberately returns an alternate type-use interface instead
    of eagerly freezing it in a fixture constructor. }
  TAlternateParameter = class(TInterfacedObject, INyxDocumentationParameterDeclaration)
  public
    Value: INyxDocumentationTypeUse;
    DeclaredName, DeclaredDefault: TNyxText;
    TypeReads: Integer;
    function Name: TNyxText;
    function TypeUse: INyxDocumentationTypeUse;
    function Passing: TNyxDocumentationParameterMode;
    function HasDefault: Boolean;
    function DefaultPascal: TNyxText;
    function WithDefault(const APascal: TNyxText): INyxDocumentationParameterDeclaration;
  end;
  { Optional callable producer can cancel on the final envelope read. Legacy
    core capture must keep its historical cancellation exception class. }
  TAlternateMember = class(TInterfacedObject, INyxDocumentationMember, INyxDocumentationMemberSignature)
  public
    Value: INyxDocumentationMember;
    Rich: INyxDocumentationRichSignature;
    Scope: INyxCancellationScope;
    Reads: Integer;
    LegacyMutation: Integer;
    ChangedValueType: Boolean;
    destructor Destroy; override;
    function Identity: TNyxDocumentationRef;
    function Kind: TNyxDocumentationMemberKind;
    function Summary: TNyxText;
    function Evidence: TNyxDocumentationEvidence;
    function Signature: INyxDocumentationSignature;
    function ValueType: TNyxDocumentationRef;
    function ExampleCount: Integer;
    function ExampleAt(AIndex: Integer): TNyxDocumentationExample;
    function RelatedCount: Integer;
    function RelatedAt(AIndex: Integer): TNyxDocumentationRef;
    function WithSignature(const AValue: INyxDocumentationSignature): INyxDocumentationMember;
    function OfType(const AValue: TNyxDocumentationRef): INyxDocumentationMember;
    function WithExample(const AValue: TNyxDocumentationExample): INyxDocumentationMember;
    function WithRelated(const AValue: TNyxDocumentationRef): INyxDocumentationMember;
    function DeclaredSignature: INyxDocumentationRichSignature;
  end;

var
  GChecks, GDestroyed: Integer;
  GReceiptDisposed: Integer;
  GReceiptConstruction: INyxDocumentationTypeConstruction;
  GReceiptInput, GReceiptNested: INyxDocumentationTypeExpression;
  GReceiptText: TNyxText;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin
  Inc(GChecks);

  if not AValue then
    raise Exception.Create(AReason);
end;

function Ref(const AName: TNyxText; AArity: Integer = 0): TNyxDocumentationRef;
begin
  Result := NyxDocumentationType('mechanics.expressions', AName, AArity);
end;

function Construction: INyxDocumentationTypeConstruction;
begin
  Result := NewNyxDocumentationTypeConstruction(
    TNyxDocumentationTypeExpressionLimits.Defaults.Bytes(NyxDocumentationMaximumBytes));
end;

procedure TOuterAdmission.AdmitTypeWork(APhase: TNyxPascalAnalysisPhase; AUnits: Integer);
begin
  Inc(Work, AUnits);
end;

function TOuterAdmission.RemainingTypeBytes: Integer;
begin
  Result := Bytes;
end;

destructor TReceiptAdmission.Destroy;
begin
  Inc(GReceiptDisposed);
  inherited Destroy;
end;

procedure TReceiptAdmission.AdmitTypeWork(APhase: TNyxPascalAnalysisPhase; AUnits: Integer);
begin

  if (Ord(APhase) < Ord(Low(TNyxPascalAnalysisPhase))) or
    (Ord(APhase) > Ord(High(TNyxPascalAnalysisPhase))) or (AUnits < 1) then
    raise Exception.Create('Receipt policy received an invalid admission');

  if DropArmed then
  begin
    DropArmed := False;
    GReceiptConstruction := nil;
    GReceiptInput := nil;
    GReceiptText := '';
  end;

  if GrowArmed and (APhase = papTypeProof) and (AUnits > 4) then
  begin
    { For the one-character/one-node input all ordinary explicit proof charges
      are at most3. This observes real receipt growth without a guessed ordinal
      or a new private instrumentation capability. Clear before reentering. }
    GrowArmed := False;
    Inc(GrowthCalls);
    GReceiptNested := NewNyxDocumentationTypeExpression('B', ndtoParameter,
      GReceiptConstruction);
  end;
end;

function TReceiptAdmission.RemainingTypeBytes: Integer;
begin
  Result := NyxDocumentationMaximumBytes;
end;

procedure TReceiptCancellation.Cancel;
begin
  CancelledValue := True;
end;

function TReceiptCancellation.GetCancelled: Boolean;
begin

  if RetireOnRead then
  begin
    RetireOnRead := False;
    Inc(RetireReads);
    GReceiptConstruction.Retire;
  end;
  Result := CancelledValue;
end;

function TAlternateExpression.Text: TNyxText;
begin
  Inc(TextReads);
  Result := Value.Text;
end;

function TAlternateExpression.Occurrence: TNyxDocumentationTypeOccurrence;
begin
  Result := Value.Occurrence;
end;

function TAlternateExpression.Syntax: INyxPascalTypeSyntax;
begin
  Result := Value.Syntax;
end;

function TAlternateExpression.NamedUseCount: Integer;
begin
  Result := Value.NamedUseCount;
end;

function TAlternateExpression.NamedUseNode(AIndex: Integer): Integer;
begin
  Inc(NodeReads);
  Result := Value.NamedUseNode(AIndex);
end;

function TAlternateExpression.NamedUseArity(AIndex: Integer): Integer;
begin
  Result := Value.NamedUseArity(AIndex);
end;

function TAlternateUse.Reference: TNyxDocumentationRef;
begin
  Result := Value.Reference;
end;

function TAlternateUse.Expression: INyxDocumentationTypeExpression;
begin
  Result := Value.Expression;
end;

function TAlternateUse.BindingCount: Integer;
begin
  Result := Value.BindingCount;
end;

function TAlternateUse.BindingAt(AIndex: Integer): TNyxDocumentationRef;
begin
  Inc(Reads);
  Result := Value.BindingAt(AIndex);

  if SuppliedBinding.Defined then
    Result := SuppliedBinding;

  if Reads = MutateAt then
    Result := Ref('Changed');
end;

function TAlternateUse.WithBinding(AIndex: Integer;
  const AReference: TNyxDocumentationRef): INyxDocumentationTypeUse;
begin
  Result := nil;
  raise Exception.Create('Capture must not invoke a supplier fluent method');
end;

function TAlternateParameter.Name: TNyxText;
begin
  Result := 'AHandler';

  if DeclaredName <> '' then
    Result := DeclaredName;
end;

function TAlternateParameter.TypeUse: INyxDocumentationTypeUse;
begin
  Inc(TypeReads);
  Result := Value;
end;

function TAlternateParameter.Passing: TNyxDocumentationParameterMode;
begin
  Result := ndpConst;
end;

function TAlternateParameter.HasDefault: Boolean;
begin
  Result := DeclaredDefault <> '';
end;

function TAlternateParameter.DefaultPascal: TNyxText;
begin
  Result := DeclaredDefault;
end;

function TAlternateParameter.WithDefault(const APascal: TNyxText): INyxDocumentationParameterDeclaration;
begin
  Result := nil;
  raise Exception.Create('Capture must not invoke a supplier fluent method');
end;

destructor TAlternateMember.Destroy;
begin
  Inc(GDestroyed);
  inherited Destroy;
end;

function TAlternateMember.Identity: TNyxDocumentationRef;
begin
  Result := Value.Identity;
end;

function TAlternateMember.Kind: TNyxDocumentationMemberKind;
begin
  Result := Value.Kind;
end;

function TAlternateMember.Summary: TNyxText;
begin
  Result := Value.Summary;
end;

function TAlternateMember.Evidence: TNyxDocumentationEvidence;
begin
  Result := Value.Evidence;
end;

function TAlternateMember.Signature: INyxDocumentationSignature;
begin
  Result := Value.Signature;
end;

function TAlternateMember.ValueType: TNyxDocumentationRef;
begin

  if ChangedValueType then
    Result := Ref('TValue')
  else
    Result := Value.ValueType;
end;

function TAlternateMember.ExampleCount: Integer;
begin
  Result := Value.ExampleCount;
end;

function TAlternateMember.ExampleAt(AIndex: Integer): TNyxDocumentationExample;
begin
  Result := Value.ExampleAt(AIndex);
end;

function TAlternateMember.RelatedCount: Integer;
begin
  Result := Value.RelatedCount;
end;

function TAlternateMember.RelatedAt(AIndex: Integer): TNyxDocumentationRef;
begin
  Result := Value.RelatedAt(AIndex);
end;

function TAlternateMember.WithSignature(const AValue: INyxDocumentationSignature): INyxDocumentationMember;
begin
  Result := nil;
  raise Exception.Create('Capture must not invoke a supplier fluent method');
end;

function TAlternateMember.OfType(const AValue: TNyxDocumentationRef): INyxDocumentationMember;
begin
  Result := nil;
  raise Exception.Create('Capture must not invoke a supplier fluent method');
end;

function TAlternateMember.WithExample(const AValue: TNyxDocumentationExample): INyxDocumentationMember;
begin
  Result := nil;
  raise Exception.Create('Capture must not invoke a supplier fluent method');
end;

function TAlternateMember.WithRelated(const AValue: TNyxDocumentationRef): INyxDocumentationMember;
begin
  Result := nil;
  raise Exception.Create('Capture must not invoke a supplier fluent method');
end;

function TAlternateMember.DeclaredSignature: INyxDocumentationRichSignature;
begin
  Inc(Reads);
  Result := Rich;

  if (Reads = 2) and (Scope <> nil) then
    Scope.Cancel;

  if (Reads = 2) and (LegacyMutation = 1) then
    Value := Value.WithSignature(NewNyxDocumentationSignature([], Ref('TBag')));

  if (Reads = 2) and (LegacyMutation = 2) then
    ChangedValueType := True;
end;

function ArrayUse(const AScope: INyxDocumentationTypeConstruction;
  ARole: TNyxDocumentationTypeOccurrence; ABound: Boolean = True;
  const AText: TNyxText = 'array of TValue'): INyxDocumentationTypeUse;
begin
  Result := NewNyxDocumentationExpressionTypeUse(
    NewNyxDocumentationTypeExpression(AText, ARole, AScope), AScope);

  if ABound then
    Result := Result.WithBinding(0, Ref('TValue'));
end;

function Callable(const AScope: INyxDocumentationTypeConstruction;
  ABound: Boolean = True; const AText: TNyxText = 'array of TValue';
  const AName: TNyxText = 'WithValues'): INyxDocumentationMember;
var
  LParameters: TNyxDocumentationParameterDeclarations;
begin
  SetLength(LParameters, 2);
  LParameters[0] := NewNyxDocumentationParameterDeclaration('ACaption',
    NewNyxDocumentationNamedTypeUse(Ref('TCaption')), ndpConst).WithDefault('''Hello;:'#0#13#10'Ω''');
  LParameters[1] := NewNyxDocumentationParameterDeclaration('AValues', ArrayUse(AScope, ndtoParameter, ABound, AText), ndpConst);
  Result := NewNyxDocumentationRichMember(NyxDocumentationMember(Ref('TBag'), AName), ndmFluentMethod,
    'Retain exact named and open-array argument types.',
    NewNyxDocumentationRichSignature(LParameters, NewNyxDocumentationNamedTypeUse(Ref('TBag')), AScope), AScope);
end;

function Detached(const AValue: INyxDocumentationMember): INyxDocumentationMember;
var
  LOwner: TAlternateMember;
  LLease: INyxDocumentationMember;
  LRich: INyxDocumentationMemberSignature;
begin
  LOwner := TAlternateMember.Create;
  LLease := LOwner;
  LOwner.Value := AValue;
  Supports(AValue, INyxDocumentationMemberSignature, LRich);
  LOwner.Rich := LRich.DeclaredSignature;
  Result := CaptureNyxDocumentationMember(LLease);
end;

procedure SyntaxCases;
var
  LScope: INyxDocumentationTypeConstruction;
  LExpression, LOther: INyxDocumentationTypeExpression;
  LNode: TNyxPascalTypeNode;
  LAt: Integer;
  LRefused: Boolean;
begin
  LScope := Construction;
  try
    LExpression := NewNyxDocumentationTypeExpression('array of TValue', ndtoParameter, LScope);
    LNode := LExpression.Syntax.Node(LExpression.Syntax.Root);
    Check((LNode.Kind = ptxArray) and (LExpression.Syntax.Child(LExpression.Syntax.Root, ptrDimension) < 0) and
      (LExpression.NamedUseCount = 1) and (LExpression.Occurrence = ndtoParameter),
      'Open parameter array shape retains its explicit occurrence and named element');
    LOther := NewNyxDocumentationTypeExpression('array of TValue', ndtoDeclaration, LScope);
    Check((LOther.Occurrence <> LExpression.Occurrence) and (LOther.Text = LExpression.Text),
      'Same type text in a declaration does not erase open-parameter occurrence identity');
    LExpression := NewNyxDocumentationTypeExpression('A.B<T>.C<U>', ndtoParameter, LScope);
    Check((LExpression.NamedUseCount = 4) and (LExpression.NamedUseArity(0) = 1) and
      (LExpression.NamedUseArity(2) = 1) and
      (LExpression.Syntax.Lexical.Slice(LExpression.Syntax.Node(LExpression.NamedUseNode(0)).Span) = TNyxText('A.B')) and
      (LExpression.Syntax.Lexical.Slice(LExpression.Syntax.Node(LExpression.NamedUseNode(2)).Span) = TNyxText('A.B<T>.C')),
      'Qualified/generic traversal keeps complete designators and every separate argument use');
    LOther := NewNyxDocumentationTypeExpression('A.B.C<T>', ndtoParameter, LScope);
    Check((LOther.NamedUseCount = 2) and (LOther.NamedUseArity(0) = 1),
      'Namespace components do not become fabricated separate nominal type leaves');
    LOther := NewNyxDocumentationTypeExpression('array[0..High(TIndex)] of TValue', ndtoDeclaration, LScope);
    LAt := LOther.Syntax.Child(LOther.Syntax.Root, ptrDimension);
    Check((LOther.NamedUseCount = 1) and
      (LOther.Syntax.Lexical.Slice(LOther.Syntax.Node(LAt).Span) = TNyxText('0..High(TIndex)')),
      'Opaque bounds retain complete text without inventing type references for expression identifiers');
    LOther := NewNyxDocumentationTypeExpression('function(const A: TList<T>; var Buffer): TResult; cdecl', ndtoParameter, LScope);
    Check((LOther.NamedUseCount = 3) and (LOther.Syntax.Node(LOther.Syntax.Root).Kind = ptxFunction),
      'Existing procedure grammar owns grouped modes, untyped storage and result syntax');
    LOther := NewNyxDocumentationTypeExpression('string[80]', ndtoParameter, LScope);
    Check((LOther.NamedUseCount = 1) and (LOther.NamedUseArity(0) = 0),
      'Authored string syntax retains an unresolved type use rather than a guessed System target');
    for LAt := 0 to 2 do
    begin
      LRefused := False;
      LOther := nil;
      try
        case LAt of
          0: LOther := NewNyxDocumentationTypeExpression('array of', ndtoParameter, LScope);
          1: LOther := NewNyxDocumentationTypeExpression('record Count: Integer; end', ndtoParameter, LScope);
          2: LOther := NewNyxDocumentationTypeExpression('TList<{$ifdef A}T{$endif}>', ndtoParameter, LScope);
        end;
      except
        on ENyxDocumentationTypeExpression do
          LRefused := True;
      end;
      Check(LRefused and (LOther = nil), 'Incomplete/context-dependent type text refuses atomically ' + IntToStr(LAt));
    end;
  finally
    LScope.Retire;
  end;
  Check(LExpression.Syntax.Complete and (LExpression.Text = TNyxText('A.B<T>.C<U>')),
    'Published expressions survive retiring every construction lease');
  LRefused := False;
  try
    LScope.Spend;
  except
    on ENyxDocumentationTypeExpressionCancelled do
      LRefused := True;
  end;
  Check(LRefused, 'A retired construction cannot silently open a fresh allowance');
end;

procedure CaptureReleasedInputs(ACaptureExisting: Boolean;
  out AFrozen: INyxDocumentationTypeExpression);
var
  LPolicy: TReceiptAdmission;
  LAdmission: INyxDocumentationTypeAdmission;
begin
  AFrozen := nil;
  LPolicy := TReceiptAdmission.Create;
  LAdmission := LPolicy;
  GReceiptConstruction := NewNyxDocumentationTypeConstruction(
    TNyxDocumentationTypeExpressionLimits.Defaults, nil, LAdmission);
  GReceiptText := 'array of T';
  try

    if ACaptureExisting then
      GReceiptInput := NewNyxDocumentationTypeExpression(GReceiptText,
        ndtoParameter, GReceiptConstruction);
    LPolicy.DropArmed := True;

    if ACaptureExisting then
      AFrozen := CaptureNyxDocumentationTypeExpression(GReceiptInput, GReceiptConstruction)
    else
      AFrozen := NewNyxDocumentationTypeExpression(GReceiptText,
        ndtoParameter, GReceiptConstruction);
    Check(not LPolicy.DropArmed and (GReceiptConstruction = nil) and
      (GReceiptInput = nil) and (GReceiptText = ''),
      'Outward admission really releases the last caller construction and input handles');
  finally

    if GReceiptConstruction <> nil then
      GReceiptConstruction.Retire;
    GReceiptConstruction := nil;
    GReceiptInput := nil;
    GReceiptText := '';
  end;
end;

procedure ReceiptArenaCases;
const
  CMany = 192;
  CForeign: TNyxText = 'array of T';
var
  LScope: INyxDocumentationTypeConstruction;
  LValues: array of INyxDocumentationTypeExpression;
  LFirst, LSecond, LCopy, LOuter, LDetached: INyxDocumentationTypeExpression;
  LPolicy: TReceiptAdmission;
  LAdmission: INyxDocumentationTypeAdmission;
  LParent: TReceiptCancellation;
  LParentLease: INyxCancellationScope;
  LAt, LBefore, LDisposed, LBytes: Integer;
  LFailure: Boolean;
begin
  LScope := Construction;
  SetLength(LValues, CMany);
  try
    for LAt := 0 to CMany - 1 do
      LValues[LAt] := NewNyxDocumentationTypeExpression(
        'TReceipt' + TNyxText(IntToStr(LAt)), ndtoParameter, LScope);
    LBefore := LScope.Receipt.Total;
    for LAt := CMany - 1 downto 0 do
    begin
      LCopy := CaptureNyxDocumentationTypeExpression(LValues[LAt], LScope);
      Check(LCopy = LValues[LAt], 'Same-construction receipt reuse preserves exact immutable expression identity');
    end;
    { This generous linear envelope distinguishes direct receipt lookup from
      scanning the complete prefix for each expression, without an exact work
      count, first-fit search, larger quota or reduced public source witness. }
    Check(LScope.Receipt.Total - LBefore <= CMany * 16,
      'Reuse after many retained receipts consumes bounded linear lookup work');
  finally
    LScope.Retire;
  end;
  Check(LValues[0].Text = 'TReceipt0',
    'Retired creation arenas leave immutable values readable without retaining construction');
  LValues := nil;
  LScope := Construction;
  try
    LFirst := NewNyxDocumentationTypeExpression(CForeign, ndtoParameter, LScope);
    LSecond := NewNyxDocumentationTypeExpression(CForeign, ndtoParameter, LScope);
    Check(LFirst <> LSecond, 'Independent equal expressions have distinct captured owners');
  finally
    LScope.Retire;
  end;
  LBytes := NyxUTF8ByteCount(CForeign);
  LScope := NewNyxDocumentationTypeConstruction(
    TNyxDocumentationTypeExpressionLimits.Defaults.Bytes(LBytes));
  try
    LCopy := NewNyxDocumentationTypeExpression('A', ndtoParameter, LScope);
    LCopy := nil;
    LFailure := False;
    try
      LCopy := CaptureNyxDocumentationTypeExpression(LFirst, LScope);
    except
      on ENyxDocumentationTypeExpression do LFailure := True;
    end;
    Check(LFailure and (LCopy = nil),
      'A retired foreign arena and colliding old slot cannot bypass remaining storage admission');
  finally
    LScope.Retire;
  end;
  LScope := NewNyxDocumentationTypeConstruction(
    TNyxDocumentationTypeExpressionLimits.Defaults.Bytes(LBytes * 2));
  try
    LCopy := NewNyxDocumentationTypeExpression('A', ndtoParameter, LScope);
    LCopy := CaptureNyxDocumentationTypeExpression(LFirst, LScope);
    Check((LCopy = LFirst) and
      (CaptureNyxDocumentationTypeExpression(LFirst, LScope) = LFirst),
      'A foreign owner pays its first storage capture and keeps its normal repeated receipt');
    LCopy := nil;
    LFailure := False;
    try
      LCopy := CaptureNyxDocumentationTypeExpression(LSecond, LScope);
    except
      on ENyxDocumentationTypeExpression do LFailure := True;
    end;
    Check(LFailure and (LCopy = nil),
      'An independently captured equal expression pays storage fully rather than borrowing the first owner');
  finally
    LScope.Retire;
  end;
  LScope := NewNyxDocumentationTypeConstruction(
    TNyxDocumentationTypeExpressionLimits.Defaults.Bytes(LBytes * 2));
  try
    Check((CaptureNyxDocumentationTypeExpression(LFirst, LScope) = LFirst) and
      (CaptureNyxDocumentationTypeExpression(LSecond, LScope) = LSecond),
      'Adequate exact capacity admits both distinct foreign owners without stale-slot rejection');
  finally
    LScope.Retire;
  end;
  LPolicy := TReceiptAdmission.Create;
  LAdmission := LPolicy;
  GReceiptConstruction := NewNyxDocumentationTypeConstruction(
    TNyxDocumentationTypeExpressionLimits.Defaults, nil, LAdmission);
  GReceiptNested := nil;
  LPolicy.GrowArmed := True;
  try
    LOuter := NewNyxDocumentationTypeExpression('A', ndtoParameter, GReceiptConstruction);
    Check((LPolicy.GrowthCalls = 1) and not LPolicy.GrowArmed and
      (GReceiptNested <> nil) and (GReceiptNested.Text = 'B'),
      'The outward receipt-growth admission recursively creates a real sibling expression');
    Check((CaptureNyxDocumentationTypeExpression(GReceiptNested, GReceiptConstruction) = GReceiptNested) and
      (CaptureNyxDocumentationTypeExpression(LOuter, GReceiptConstruction) = LOuter),
      'Recursive growth keeps both exact committed receipt slots without overwrite');
  finally
    GReceiptConstruction.Retire;
    GReceiptConstruction := nil;
    GReceiptNested := nil;
    LAdmission := nil;
  end;
  for LAt := 0 to 1 do
  begin
    LDisposed := GReceiptDisposed;
    CaptureReleasedInputs(LAt <> 0, LDetached);
    Check((GReceiptDisposed = LDisposed + 1) and (LDetached <> nil) and
      (LDetached.Text = CForeign) and (LDetached.Occurrence = ndtoParameter),
      'New/capture input leases survive caller release and retain no outward policy, mode=' + TNyxText(IntToStr(LAt)));
  end;
  LParent := TReceiptCancellation.Create;
  LParentLease := LParent;
  GReceiptConstruction := NewNyxDocumentationTypeConstruction(
    TNyxDocumentationTypeExpressionLimits.Defaults, LParentLease);
  LParent.RetireOnRead := True;
  LFailure := False;
  LCopy := nil;
  try
    try
      LCopy := NewNyxDocumentationTypeExpression('A', ndtoParameter, GReceiptConstruction);
    except
      on ENyxDocumentationTypeExpressionCancelled do LFailure := True;
    end;
    Check(LFailure and (LCopy = nil) and (LParent.RetireReads = 1) and
      not LParent.CancelledValue and (LDetached.Text = CForeign),
      'Retirement during a parent cancellation getter remains visible even when the parent returns False');
  finally
    GReceiptConstruction.Retire;
    GReceiptConstruction := nil;
  end;
end;

procedure SharedBudgetCases;
var
  LBuild, LScope: INyxDocumentationTypeConstruction;
  LFirst, LSecond, LResult: INyxDocumentationTypeExpression;
  LOuter: TOuterAdmission;
  LOuterLease: INyxDocumentationTypeAdmission;
  LAlternate: TAlternateExpression;
  LAlternateLease: INyxDocumentationTypeExpression;
  LBefore: TNyxPascalAnalysisReceipt;
  LRefused: Boolean;
  LText: TNyxText;
  LAt: Integer;
begin
  LText := 'T' + StringOfChar('x', 59);
  LBuild := Construction;
  LFirst := NewNyxDocumentationTypeExpression(LText, ndtoParameter, LBuild);
  LSecond := NewNyxDocumentationTypeExpression(LText, ndtoParameter, LBuild);
  LBuild.Retire;
  LOuter := TOuterAdmission.Create;
  LOuterLease := LOuter;
  LOuter.Bytes := 100;
  LScope := NewNyxDocumentationTypeConstruction(TNyxDocumentationTypeExpressionLimits.Defaults, nil, LOuterLease);
  try
    LResult := CaptureNyxDocumentationTypeExpression(LFirst, LScope);
    Check(LResult = LFirst, 'Exact private captured owner can retain its immutable small syntax');
    LResult := CaptureNyxDocumentationTypeExpression(LFirst, LScope);
    Check(LResult = LFirst, 'Repeated exact owner consumes a reference without duplicating retained storage');
    LResult := nil;
    LRefused := False;
    try
      LResult := CaptureNyxDocumentationTypeExpression(LSecond, LScope);
    except
      on ENyxDocumentationTypeExpression do
        LRefused := True;
    end;
    Check(LRefused and (LResult = nil), 'Independent equal syntax retains its full copy-capacity charge');
  finally
    LScope.Retire;
  end;
  LAlternate := TAlternateExpression.Create;
  LAlternateLease := LAlternate;
  LAlternate.Value := LSecond;
  LScope := NewNyxDocumentationTypeConstruction(TNyxDocumentationTypeExpressionLimits.Defaults, nil, LOuterLease);
  try
    LResult := CaptureNyxDocumentationTypeExpression(LFirst, LScope);
    LBefore := LScope.Receipt;
    LResult := nil;
    LRefused := False;
    try
      LResult := CaptureNyxDocumentationTypeExpression(LAlternateLease, LScope);
    except
      on ENyxDocumentationTypeExpression do
        LRefused := True;
    end;
    Check(LRefused and (LResult = nil) and (LAlternate.TextReads = 1) and (LAlternate.NodeReads = 0) and
      (LScope.Receipt.Used(papLexicalToken) = LBefore.Used(papLexicalToken)),
      'Two60-byte windows cannot bypass100 remaining outer bytes; second refuses before lexical or later occurrence getters');
  finally
    LScope.Retire;
  end;
  for LAt := 0 to 1 do
  begin

    if LAt = 0 then
      LScope := NewNyxDocumentationTypeConstruction(TNyxDocumentationTypeExpressionLimits.Defaults.Nodes(1))
    else
      LScope := NewNyxDocumentationTypeConstruction(TNyxDocumentationTypeExpressionLimits.Defaults.Tokens(2));
    try
      LResult := CaptureNyxDocumentationTypeExpression(LFirst, LScope);
      LResult := nil;
      LRefused := False;
      try
        LResult := CaptureNyxDocumentationTypeExpression(LSecond, LScope);
      except
        on ENyxDocumentationTypeExpression do
          LRefused := True;
      end;
      Check(LRefused and (LResult = nil), 'Retained node/token limits are shared across independent expressions ' + IntToStr(LAt));
    finally
      LScope.Retire;
    end;
  end;
end;

procedure BindingCases;
var
  LBuild, LScope: INyxDocumentationTypeConstruction;
  LUse: INyxDocumentationTypeUse;
  LAlternate: TAlternateUse;
  LUseLease: INyxDocumentationTypeUse;
  LParameter: TAlternateParameter;
  LParameters: TNyxDocumentationParameterDeclarations;
  LSignature: INyxDocumentationRichSignature;
  LRefused: Boolean;
  LAt: Integer;
  LText: TNyxText;
begin
  LText := 'procedure(';
  for LAt := 0 to 31 do
  begin

    if LAt > 0 then
      LText := LText + ';';
    LText := LText + 'P' + IntToStr(LAt) + ':T' + IntToStr(LAt);
  end;
  LText := LText + ')';
  LBuild := Construction;
  LUse := NewNyxDocumentationExpressionTypeUse(NewNyxDocumentationTypeExpression(LText, ndtoParameter, LBuild), LBuild);
  for LAt := 0 to 31 do
    LUse := LUse.WithBinding(LAt, Ref('T' + IntToStr(LAt)));
  LBuild.Retire;
  LAlternate := TAlternateUse.Create;
  LUseLease := LAlternate;
  LAlternate.Value := LUse;
  LParameter := TAlternateParameter.Create;
  SetLength(LParameters, 1);
  LParameters[0] := LParameter;
  LParameter.Value := LUseLease;
  LScope := Construction;
  try
    LSignature := NewNyxDocumentationRichSignature(LParameters, nil, LScope);
    Check((LAlternate.Reads = 64) and (LSignature.ParameterAt(0).TypeUse.BindingCount = 32) and
      LSignature.ParameterAt(0).TypeUse.BindingAt(31).Same(Ref('T31')),
      'Multi-binding alternate capture reads each slot twice and builds one exact immutable map');
  finally
    LScope.Retire;
  end;
  LAlternate.Reads := 0;
  LAlternate.MutateAt := 64;
  LScope := Construction;
  LSignature := nil;
  LRefused := False;
  try
    try
      LSignature := NewNyxDocumentationRichSignature(LParameters, nil, LScope);
    except
      on ENyxDocumentation do
        LRefused := True;
    end;
    Check(LRefused and (LSignature = nil), 'Mutation at the last binding read refuses the entire rich signature');
  finally
    LScope.Retire;
  end;
  LAlternate.Reads := 0;
  LAlternate.MutateAt := 0;
  LScope := NewNyxDocumentationTypeConstruction(TNyxDocumentationTypeExpressionLimits.Defaults.Work(40));
  LSignature := nil;
  LRefused := False;
  try
    try
      LSignature := NewNyxDocumentationRichSignature(LParameters, nil, LScope);
    except
      on ENyxDocumentationTypeExpression do
        LRefused := True;
    end;
    Check(LRefused and (LSignature = nil) and (LAlternate.Reads < 64),
      'A small shared work allowance refuses before the remaining binding getters');
  finally
    LScope.Retire;
  end;
end;

procedure MetadataBudgetCases;
var
  LBuild, LScope: INyxDocumentationTypeConstruction;
  LUse: INyxDocumentationTypeUse;
  LAlternate: TAlternateUse;
  LUseLease: INyxDocumentationTypeUse;
  LParameter, LLast: TAlternateParameter;
  LParameters: TNyxDocumentationParameterDeclarations;
  LSignature: INyxDocumentationRichSignature;
  LOuter: TOuterAdmission;
  LOuterLease: INyxDocumentationTypeAdmission;
  LLongRef: TNyxDocumentationRef;
  LText, LName: TNyxText;
  LAt: Integer;
  LRefused: Boolean;
begin
  LName := 'T' + StringOfChar('x', 255);
  LLongRef := NyxDocumentationType('u' + StringOfChar('n', 255), LName);
  LBuild := NewNyxDocumentationTypeConstruction(TNyxDocumentationTypeExpressionLimits.Defaults);
  try
    LUse := NewNyxDocumentationExpressionTypeUse(
      NewNyxDocumentationTypeExpression('procedure(A:TOne;B:TTwo)', ndtoParameter, LBuild), LBuild);
  finally
    LBuild.Retire;
  end;
  LAlternate := TAlternateUse.Create;
  LUseLease := LAlternate;
  LAlternate.Value := LUse;
  LAlternate.SuppliedBinding := LLongRef;
  LParameter := TAlternateParameter.Create;
  SetLength(LParameters, 1);
  LParameters[0] := LParameter;
  LParameter.Value := LUseLease;
  LOuter := TOuterAdmission.Create;
  LOuterLease := LOuter;
  LOuter.Bytes := 100;
  LScope := NewNyxDocumentationTypeConstruction(TNyxDocumentationTypeExpressionLimits.Defaults, nil, LOuterLease);
  LSignature := nil;
  LRefused := False;
  try
    try
      LSignature := NewNyxDocumentationRichSignature(LParameters, nil, LScope);
    except
      on ENyxDocumentationTypeExpression do
        LRefused := True;
    end;
    Check(LRefused and (LSignature = nil) and (LAlternate.Reads = 1),
      'New binding fields consume remaining outer capacity before the next binding supplier read');
  finally
    LScope.Retire;
  end;

  LText := 'procedure(';
  for LAt := 0 to 599 do
  begin

    if LAt > 0 then
      LText := LText + ';';
    LText := LText + 'P' + IntToStr(LAt) + ':' + LName;
  end;
  LText := LText + ')';
  LBuild := NewNyxDocumentationTypeConstruction(TNyxDocumentationTypeExpressionLimits.Defaults);
  try
    LUse := NewNyxDocumentationExpressionTypeUse(
      NewNyxDocumentationTypeExpression(LText, ndtoParameter, LBuild), LBuild);
  finally
    LBuild.Retire;
  end;
  LAlternate.Value := LUse;
  LAlternate.Reads := 0;
  LScope := NewNyxDocumentationTypeConstruction(TNyxDocumentationTypeExpressionLimits.Defaults);
  LSignature := nil;
  LRefused := False;
  try
    try
      LSignature := NewNyxDocumentationRichSignature(LParameters, nil, LScope);
    except
      on ENyxDocumentation do
        LRefused := True;
    end;
    Check(LRefused and (LSignature = nil) and (LAlternate.Reads > 0) and (LAlternate.Reads < 600),
      'Binding-heavy type use refuses the unchanged256KiB bound before reading all binding rows');
  finally
    LScope.Retire;
  end;

  LBuild := NewNyxDocumentationTypeConstruction(TNyxDocumentationTypeExpressionLimits.Defaults);
  try
    LUse := ArrayUse(LBuild, ndtoParameter, True, 'array of TValue' + StringOfChar(' ', 3800));
  finally
    LBuild.Retire;
  end;
  SetLength(LParameters, NyxDocumentationMaximumParameters);
  LLast := nil;
  for LAt := 0 to High(LParameters) do
  begin
    LParameter := TAlternateParameter.Create;
    LParameters[LAt] := LParameter;
    LParameter.Value := LUse;
    LParameter.DeclaredName := 'A' + IntToStr(LAt);
    LParameter.DeclaredDefault := StringOfChar('x', 1024);
    LLast := LParameter;
  end;
  LScope := NewNyxDocumentationTypeConstruction(TNyxDocumentationTypeExpressionLimits.Defaults);
  LSignature := nil;
  LRefused := False;
  try
    try
      LSignature := NewNyxDocumentationRichSignature(LParameters, nil, LScope);
    except
      on ENyxDocumentation do
        LRefused := True;
    end;
    Check(LRefused and (LSignature = nil) and (LLast.TypeReads = 0),
      'Repeated syntax with defaults cannot bypass signature256KiB even when copied syntax is shared exactly');
  finally
    LScope.Retire;
  end;
end;

procedure FullTypeBudgetCases;
var
  LBuild, LScope: INyxDocumentationTypeConstruction;
  LSource, LFrozen: INyxDocumentationType;
  LBudget: INyxPascalAnalysisBudget;
  LRich: INyxDocumentationMemberSignature;
  LRefused: Boolean;
  LBefore, LRequested: Integer;
begin
  LBuild := Construction;
  try
    LSource := NewNyxDocumentationType(Ref('TBag'), ndtClass, 'Whole captured declaration.')
      .WithMember(Callable(LBuild, True, 'array of TValue', 'WithFirst'))
      .WithMember(Callable(LBuild, True, 'array of TValue', 'WithSecond'));
  finally
    LBuild.Retire;
  end;
  LScope := NewNyxDocumentationTypeConstruction(TNyxDocumentationTypeExpressionLimits.Defaults.Nodes(3));
  LFrozen := nil;
  LRefused := False;
  try
    try
      LFrozen := CaptureNyxDocumentationType(LSource, LScope);
    except
      on ENyxDocumentationTypeExpression do
        LRefused := True;
    end;
    Check(LRefused and (LFrozen = nil) and (LSource.MemberCount = 2),
      'Whole-type capture shares node admission across both independent member expressions atomically');
  finally
    LScope.Retire;
  end;
  LBudget := NewNyxPascalAnalysisBudget(5);
  LFrozen := nil;
  LRefused := False;
  LBefore := -1;
  LRequested := 0;
  try
    LFrozen := CaptureNyxDocumentationType(LSource, LBudget);
  except
    on E: ENyxPascalAnalysisWorkLimit do
    begin
      LRefused := True;
      LBefore := E.WorkBefore;
      LRequested := E.Requested;
    end;
  end;
  Check(LRefused and (LFrozen = nil) and (LBudget.Receipt.Total = LBefore) and
    (LRequested > 5 - LBefore),
    'Whole-type capture forwards the caller work ceiling through its member capture');
  LBudget.Retire;
  LScope := Construction;
  try
    LFrozen := CaptureNyxDocumentationType(LSource, LScope);
  finally
    LScope.Retire;
  end;
  Check((LFrozen.MemberCount = 2) and Supports(LFrozen.MemberAt(1), INyxDocumentationMemberSignature, LRich) and
    (LRich.DeclaredSignature.ParameterAt(1).TypeUse.Expression.Text = 'array of TValue'),
    'Adequate whole-type capture retains rich member specialization after its shared scope retires');
end;

procedure MemberCases;
var
  LScope: INyxDocumentationTypeConstruction;
  LMember, LCopy, LFrozen, LPropertyMember: INyxDocumentationMember;
  LRich, LCopyRich: INyxDocumentationMemberSignature;
  LProperty: INyxDocumentationRichProperty;
  LSimple: INyxDocumentationProperty;
  LParameters: TNyxDocumentationParameterDeclarations;
  LValue: INyxDocumentationTypeUse;
  LOwner: TAlternateMember;
  LLease: INyxDocumentationMember;
  LCancel: INyxCancellationScope;
  LBefore, LAt: Integer;
  LRefused: Boolean;
begin
  LScope := Construction;
  try
    LMember := Callable(LScope);
    Check((LMember.Signature = nil) and Supports(LMember, INyxDocumentationMemberSignature, LRich) and
      (LRich.DeclaredSignature.ParameterCount = 2), 'Mixed named/open-array callable keeps old signature explicitly unknown');
    Check(LRich.DeclaredSignature.ParameterAt(0).DefaultPascal = TNyxText('''Hello;:'#0#13#10'Ω'''),
      'Managed parameter defaults preserve exact punctuation, Unicode, NUL and CRLF');
    LCopy := LMember.WithExample(NyxDocumentationExample('Use values', 'Bag.WithValues(''Hi'', Values);')).WithRelated(Ref('TValue'));
    Check(Supports(LCopy, INyxDocumentationMemberSignature, LCopyRich) and
      (LCopyRich.DeclaredSignature.ParameterAt(1).TypeUse.Expression.Text = TNyxText('array of TValue')) and
      (LMember.ExampleCount = 0), 'Fluent copies retain specialized signature and prior immutable readers');
    LCopy := LMember.WithSignature(NewNyxDocumentationSignature([], Ref('TBag')));
    Check((LCopy.Signature <> nil) and (LCopy.Signature.ParameterCount = 0) and
      Supports(LCopy, INyxDocumentationMemberSignature, LCopyRich) and (LCopyRich.DeclaredSignature = nil) and
      (LRich.DeclaredSignature <> nil), 'Explicit legacy signature replacement revokes stale rich data without mutating old readers');
    SetLength(LParameters, 2);
    LParameters[0] := NewNyxDocumentationParameterDeclaration('AIndex', NewNyxDocumentationNamedTypeUse(Ref('TIndex')));
    LParameters[1] := NewNyxDocumentationParameterDeclaration('AGroup', ArrayUse(LScope, ndtoPropertyIndex));
    LValue := ArrayUse(LScope, ndtoPropertyValue);
    LPropertyMember := NewNyxDocumentationRichProperty(NyxDocumentationMember(Ref('TBag'), 'Items'),
      'Keep complex index and value expressions.', LParameters, LValue, [ndpaRead], True, LScope);
    Check(Supports(LPropertyMember, INyxDocumentationRichProperty, LProperty) and
      not Supports(LPropertyMember, INyxDocumentationProperty, LSimple) and not LPropertyMember.ValueType.Defined and
      (LProperty.ParameterCount = 2) and LProperty.DefaultProperty,
      'Complex property exposes no invalid copied named records or fabricated value type');
    LCopy := LPropertyMember.OfType(Ref('TValue')).WithRelated(Ref('TIndex'));
    Check(Supports(LCopy, INyxDocumentationRichProperty, LProperty) and
      LProperty.ValueUse.Reference.Same(Ref('TValue')) and
      (LProperty.ParameterAt(1).TypeUse.Expression.Occurrence = ndtoPropertyIndex),
      'OfType replaces rich property value use while preserving its complex indices');
  finally
    LScope.Retire;
  end;
  LBefore := GDestroyed;
  LFrozen := Detached(LMember);
  Check((GDestroyed = LBefore + 1) and Supports(LFrozen, INyxDocumentationMemberSignature, LCopyRich),
    'Captured optional supplier retires; rich result retains only immutable metadata');
  LCancel := NewNyxCancellationScope;
  LOwner := TAlternateMember.Create;
  LLease := LOwner;
  LOwner.Value := LMember;
  LOwner.Rich := LRich.DeclaredSignature;
  LOwner.Scope := LCancel;
  LFrozen := nil;
  LRefused := False;
  try
    LFrozen := CaptureNyxDocumentationMember(LLease, LCancel);
  except
    on ENyxDocumentationCancelled do
      LRefused := True;
  end;
  Check(LRefused and (LFrozen = nil), 'Last optional envelope cancellation keeps the existing core exception class and atomic refusal');
  LCancel := NewNyxCancellationScope;
  LCancel.Cancel;
  LRefused := False;
  try
    LFrozen := CaptureNyxDocumentationMember(LMember, LCancel);
  except
    on ENyxDocumentationCancelled do
      LRefused := True;
  end;
  Check(LRefused and (LFrozen = nil), 'Pre-cancelled core capture keeps its historical exception class');
  LOwner := TAlternateMember.Create;
  LLease := LOwner;
  for LAt := 1 to 2 do
  begin
    LOwner.Value := LMember;
    LOwner.Rich := LRich.DeclaredSignature;
    LOwner.Reads := 0;
    LOwner.LegacyMutation := LAt;
    LOwner.ChangedValueType := False;
    LFrozen := nil;
    LRefused := False;
    try
      LFrozen := CaptureNyxDocumentationMember(LLease);
    except
      on ENyxDocumentation do
        LRefused := True;
    end;
    Check(LRefused and (LFrozen = nil), 'Late legacy Signature/ValueType mutation cannot publish conflicting rich metadata ' + IntToStr(LAt));
  end;
  LRefused := False;
  try
    NyxDocumentationParameter('Invalid', Default(TNyxDocumentationRef));
  except
    on ENyxDocumentation do
      LRefused := True;
  end;
  Check(LRefused, 'Old named-parameter construction still refuses an undefined type');
end;

procedure RoleCases;
var
  LScope: INyxDocumentationTypeConstruction;
  LParameters: TNyxDocumentationParameterDeclarations;
  LSignature: INyxDocumentationRichSignature;
  LMember: INyxDocumentationMember;
  LUse: INyxDocumentationTypeUse;
  LRich: INyxDocumentationMemberSignature;
  LAt: Integer;
  LRefused: Boolean;
begin
  LScope := Construction;
  try
    SetLength(LParameters, 1);
    LParameters[0] := NewNyxDocumentationParameterDeclaration('AText', NewNyxDocumentationNamedTypeUse(Ref('TCaption')));
    LSignature := NewNyxDocumentationRichSignature(LParameters, NewNyxDocumentationNamedTypeUse(Ref('TBag')), LScope);
    LMember := NewNyxDocumentationRichMember(NyxDocumentationMember(Ref('TBag'), 'Named'), ndmMethod,
      'Named rich signature projects coherently.', LSignature, LScope);
    Check((LMember.Signature <> nil) and (LMember.Signature.ParameterCount = 1) and
      LMember.Signature.ParameterAt(0).TypeReference.Same(Ref('TCaption')) and
      Supports(LMember, INyxDocumentationMemberSignature, LRich),
      'Wholly named richer metadata preserves a truthful strict legacy projection');
    LMember := CaptureNyxDocumentationMember(LMember);
    Check((LMember.Signature <> nil) and Supports(LMember, INyxDocumentationMemberSignature, LRich),
      'Capturing coherent named/rich signatures does not revoke either representation');
    LUse := ArrayUse(LScope, ndtoResult, False);
    LMember := NewNyxDocumentationRichMember(NyxDocumentationMember(NyxDocumentationUnit('mechanics.expressions'), 'MakeValues'),
      ndmFactory, 'A genuine unit-owned factory can retain an array result.',
      NewNyxDocumentationRichSignature(nil, LUse.WithBinding(0, Ref('TValue')), LScope), LScope);
    Check((LMember.Signature = nil) and LMember.Identity.Owner.IsUnit and not LUse.BindingAt(0).Defined,
      'Complex function results keep unit ownership and immutable explicit binding copies');
    for LAt := 0 to 5 do
    begin
      LRefused := False;
      try
        case LAt of
          0: NewNyxDocumentationNamedTypeUse(NyxDocumentationUnit('mechanics.expressions'));
          1: LUse.WithBinding(0, NyxDocumentationUnit('mechanics.expressions'));
          2: begin
            LParameters[0] := NewNyxDocumentationParameterDeclaration('Wrong', ArrayUse(LScope, ndtoDeclaration));
            NewNyxDocumentationRichSignature(LParameters, nil, LScope);
          end;
          3: NewNyxDocumentationRichSignature(nil, ArrayUse(LScope, ndtoParameter), LScope);
          4: begin
            LParameters[0] := NewNyxDocumentationParameterDeclaration('Wrong', ArrayUse(LScope, ndtoParameter));
            NewNyxDocumentationRichProperty(NyxDocumentationMember(Ref('TBag'), 'Wrong'), '', LParameters,
              nil, [], False, LScope);
          end;
          5: NewNyxDocumentationRichMember(NyxDocumentationMember(Ref('TBag'), 'Wrong'), ndmProperty, '', LSignature, LScope);
        end;
      except
        on ENyxDocumentation do
          LRefused := True;
      end;
      Check(LRefused, 'Invalid type/occurrence/member role refuses ' + IntToStr(LAt));
    end;
    LUse := NewNyxDocumentationExpressionTypeUse(NewNyxDocumentationTypeExpression('TList<T>', ndtoParameter, LScope), LScope);
    LRefused := False;
    try
      LUse.WithBinding(0, Ref('TList'));
    except
      on ENyxDocumentation do
        LRefused := True;
    end;
    Check(LRefused, 'An explicit generic-base binding cannot erase its declared argument arity');
  finally
    LScope.Retire;
  end;
end;

function Declarations(ABound: Boolean; const AText: TNyxText): TNyxDocumentationDeclarations;
var
  LScope: INyxDocumentationTypeConstruction;
begin
  Result := nil;
  LScope := Construction;
  try
    SetLength(Result, 4);
    Result[0] := NewNyxDocumentationType(Ref('TValue'), ndtInterface, 'Synthetic element.');
    Result[1] := NewNyxDocumentationIntrinsic(Ref('TCaption'), ndifString, 'Synthetic caption family.');
    Result[2] := NewNyxDocumentationIntrinsic(Ref('TIndex'), ndifInteger, 'Synthetic index family.');
    Result[3] := NewNyxDocumentationType(Ref('TBag'), ndtClass, 'Synthetic callable owner.')
      .WithMember(Callable(LScope, ABound, AText));
  finally
    LScope.Retire;
  end;
end;

procedure IndexCases;
var
  LRegistry: TNyxDocumentationRegistry;
  LCatalog, LBad: TNyxCatalog;
  LBefore, LIndex: INyxDocumentationIndex;
  LRich: INyxDocumentationMemberSignature;
  LBundle: INyxDocumentationRegistration;
  LOrigin: TNyxDocumentationRef;
  LVersion, LAt, LBeforeWork, LRequested: Integer;
  LRefused: Boolean;
  LParentBudget: INyxPascalAnalysisBudget;
begin
  LOrigin := NyxDocumentationUnit('mechanics.expressions');
  LRegistry := TNyxDocumentationRegistry.Create;
  LCatalog := TNyxCatalog.Create;
  LBad := TNyxCatalog.Create;
  try
    LBundle := NewNyxDocumentationRegistration(LOrigin, Ref('TBag'), Declarations(True, 'array of TValue'));
    Check(Pos('rich-signature', LBundle.DeclarationContentAt(3)) > 0,
      'Present richer syntax participates in exact registration framing');
    LCatalog.RegisterKind('rich-mechanics', 'Rich mechanics', 'Custom', False, LBundle);
    LIndex := LRegistry.Refresh(CaptureNyxSchemas, CaptureNyxCallbacks, LCatalog.Capture, 0);
    Check((LIndex.DeclarationCount = 4) and Supports(LIndex.FindMember(NyxDocumentationMember(Ref('TBag'), 'WithValues')),
      INyxDocumentationMemberSignature, LRich) and (LRich.DeclaredSignature.ParameterCount = 2),
      'Actual creator registration discovers the complete supplied rich metadata closure');
    LBefore := LIndex;
    LVersion := LRegistry.Version;
    for LAt := 0 to 1 do
    begin
      LBad.Free;
      LBad := TNyxCatalog.Create;

      if LAt = 0 then
        LBad.RegisterKind('unbound', 'Unbound', 'Custom', False,
          NewNyxDocumentationRegistration(LOrigin, Ref('TBag'), Declarations(False, 'array of TValue')))
      else
      begin
        LBad.RegisterKind('first', 'First', 'Custom', False, LBundle);
        LBad.RegisterKind('second', 'Second', 'Custom', False,
          NewNyxDocumentationRegistration(LOrigin, Ref('TBag'), Declarations(True, 'packed array of TValue')));
      end;
      LIndex := nil;
      LRefused := False;
      try
        LIndex := LRegistry.Refresh(CaptureNyxSchemas, CaptureNyxCallbacks, LBad.Capture, LVersion);
      except
        on ENyxDocumentation do
          LRefused := True;
      end;
      Check(LRefused and (LIndex = nil) and (LRegistry.Current = LBefore) and (LRegistry.Version = LVersion),
        'Unbound expression or exact syntax conflict preserves the prior index/version ' + IntToStr(LAt));
    end;
    LParentBudget := NewNyxPascalAnalysisBudget(5);
    LIndex := nil;
    LRefused := False;
    LBeforeWork := -1;
    LRequested := 0;
    try
      LIndex := LRegistry.Refresh(CaptureNyxSchemas, CaptureNyxCallbacks, LCatalog.Capture,
        LVersion, TNyxDocumentationIndexLimits.Defaults, LParentBudget);
    except
      on E: ENyxPascalAnalysisWorkLimit do
      begin
        LRefused := True;
        LBeforeWork := E.WorkBefore;
        LRequested := E.Requested;
      end;
    end;
    Check(LRefused and (LIndex = nil) and (LParentBudget.Receipt.Total = LBeforeWork) and
      (LRequested > 5 - LBeforeWork) and
      (LRegistry.Current = LBefore) and (LRegistry.Version = LVersion),
      'Index scope forwards actual type work to the caller analysis budget and preserves atomic publication');
    LParentBudget.Retire;
  finally
    LBad.Free;
    LCatalog.Free;
    LRegistry.Free;
  end;
end;

{$ifndef PAS2JS}
function RepositoryArgument: String;
begin

  if ParamCount <> 1 then
    raise EArgumentException.Create('Usage: nyx_documentation_typeexpressions_tests <repository-root>');
  Result := IncludeTrailingPathDelimiter(ExpandFileName(ParamStr(1)));

  if not DirectoryExists(Result) or not FileExists(Result + 'PROJECT.md') or
    not FileExists(Result + 'src' + PathDelim + 'nyx.contract.pas') or
    not FileExists(Result + 'src' + PathDelim + 'nyx.data.pas') then
    raise EArgumentException.Create('Repository root must contain PROJECT.md, src/nyx.contract.pas and src/nyx.data.pas');
end;

function SourceSyntax(const APath: String): INyxPascalSyntaxSnapshot;
var
  LStream: TMemoryStream;
  LBytes: TNyxBytes;
  LOptions: TNyxPascalSyntaxOptions;
begin
  Result := nil;
  LOptions := TNyxPascalSyntaxOptions.Defaults;
  LStream := TMemoryStream.Create;
  try
    LStream.LoadFromFile(APath);

    if LStream.Size > LOptions.LexicalOptions.MaximumSourceBytes then
      raise Exception.Create('Actual source exceeds the existing lexical allowance');
    SetLength(LBytes, Integer(LStream.Size));

    if Length(LBytes) > 0 then
      Move(LStream.Memory^, LBytes[0], Length(LBytes));
    Result := NewNyxPascalSyntaxSnapshot(NyxDecodeUTF8(LBytes), LOptions);
  finally
    LStream.Free;
  end;
end;

procedure ActualWindow(const ASource: INyxPascalSyntaxSnapshot; ARoutine: Integer;
  const AParameter, AElement: TNyxText);
var
  LDeclaration: TNyxPascalSyntaxDeclaration;
  LParameter, LElement: Integer;
  LText: TNyxText;
  LScope: INyxDocumentationTypeConstruction;
  LExpression: INyxDocumentationTypeExpression;
  LDeclared: INyxPascalDeclaredTypeSyntax;
begin
  LDeclaration := ASource.Declaration(ARoutine);
  Check((LDeclaration.Kind = psdkRoutine) and
    ((LDeclaration.Flags * [psdfIncomplete, psdfUnsupported, psdfConditional]) = []) and
    (LDeclaration.NestedScope >= 0) and ASource.Scope(LDeclaration.NestedScope).Complete,
    'Actual open-array routine has its complete declared parameter owner');
  LParameter := ASource.Named(LDeclaration.NestedScope, AParameter);
  Check((LParameter >= 0) and (ASource.Scope(LDeclaration.NestedScope).DeclarationCount = 1),
    'Actual routine retains exactly its one named parameter');
  LDeclaration := ASource.Declaration(LParameter);
  LText := ASource.Lexical.Slice(LDeclaration.TypeSpan);
  Check((LDeclaration.Kind = psdkParameter) and (LDeclaration.ParameterMode = pspmConst) and
    (Trim(LText) = 'array of ' + AElement), 'Actual const open-array type is copied from its original exact window');
  LScope := Construction;
  try
    LExpression := NewNyxDocumentationTypeExpression(LText, ndtoParameter, LScope);
  finally
    LScope.Retire;
  end;
  LElement := LExpression.Syntax.Child(LExpression.Syntax.Root, ptrElement);
  Check((LExpression.Syntax.Node(LExpression.Syntax.Root).Kind = ptxArray) and
    (LExpression.Syntax.Child(LExpression.Syntax.Root, ptrDimension) < 0) and
    (LExpression.Syntax.Node(LElement).Name = AElement) and (LExpression.NamedUseCount = 1) and
    Supports(LExpression.Syntax, INyxPascalDeclaredTypeSyntax, LDeclared) and (LDeclared.Declarations = nil) and
    (LExpression.Syntax.Lexical.Source = LText) and (LExpression.Syntax.Window.Start = 0),
    'Detached actual expression retains array/element syntax and no whole-unit owner or guessed binding');
end;

procedure ActualSources(const ARepository: String);
var
  LSyntax: INyxPascalSyntaxSnapshot;
  LNames: array of TNyxText;
  LAt, LOwner, LRoutine, LCount: Integer;
  LDeclaration: TNyxPascalSyntaxDeclaration;
  LElement: TNyxText;
begin
  LSyntax := SourceSyntax(ARepository + 'src' + PathDelim + 'nyx.contract.pas');
  SetLength(LNames, 5);
  LNames[0] := 'TNyxTextDomain';
  LNames[1] := 'TNyxTimeDomain';
  LNames[2] := 'TNyxBooleanDomain';
  LNames[3] := 'TNyxIntegerDomain';
  LNames[4] := 'TNyxNumberDomain';
  LCount := 0;
  for LAt := 0 to High(LNames) do
  begin
    LOwner := LSyntax.Named(0, LNames[LAt]);
    Check(LOwner >= 0, 'Mandatory actual domain owner exists: ' + LNames[LAt]);
    LRoutine := LSyntax.Named(LSyntax.Declaration(LOwner).NestedScope, 'Choices');
    while LRoutine >= 0 do
    begin
      LDeclaration := LSyntax.Declaration(LRoutine);
      LElement := Trim(LSyntax.Lexical.Slice(LSyntax.Declaration(
        LSyntax.Named(LDeclaration.NestedScope, 'AValues')).TypeSpan));

      if Copy(LElement, 1, 9) <> 'array of ' then
        raise Exception.Create('Actual Choices declaration changed its open-array signature');
      LElement := Copy(LElement, 10, Length(LElement));
      ActualWindow(LSyntax, LRoutine, 'AValues', LElement);
      Inc(LCount);
      LRoutine := LDeclaration.NextNamed;
    end;
  end;
  Check(LCount = 7, 'All seven public actual Choices overloads are inspected without a hidden skip');
  LSyntax := SourceSyntax(ARepository + 'src' + PathDelim + 'nyx.data.pas');
  LRoutine := LSyntax.Named(0, 'NyxObject');
  Check(LRoutine >= 0, 'Actual global NyxObject declaration exists');
  ActualWindow(LSyntax, LRoutine, 'AFields', 'TNyxDataField');
  LRoutine := LSyntax.Named(0, 'NyxArray');
  Check(LRoutine >= 0, 'Actual global NyxArray declaration exists');
  ActualWindow(LSyntax, LRoutine, 'AItems', 'TNyxDataValue');
end;

var
  LRepository: String;
{$endif}
begin
  try
    {$ifndef PAS2JS}
    LRepository := RepositoryArgument;
    {$endif}
    SyntaxCases;
    ReceiptArenaCases;
    SharedBudgetCases;
    BindingCases;
    MetadataBudgetCases;
    FullTypeBudgetCases;
    MemberCases;
    RoleCases;
    IndexCases;
    {$ifndef PAS2JS}
    ActualSources(LRepository);
    {$endif}
    Check((DocumentationControlsConstructed = 0) and (DocumentationCallbacksConstructed = 0) and
      (DocumentationCallbacksInvoked = 0) and (DocumentationGlobalFactoryCalls = 0) and
      (DocumentationGlobalRoutineCalls = 0), 'Inspection executes no documented control, callback, factory or example');
    {$ifdef PAS2JS}
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' portable rich documentation checks';
    {$else}
    WriteLn('PASS ', GChecks, ' rich documentation checks / all9 actual open-array windows inspected');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.textContent := 'FAIL ' + E.Message;
      {$else}
      WriteLn(StdErr, E.ClassName, ': ', E.Message, ' at check count ', GChecks);
      DumpExceptionBackTrace(StdErr);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
