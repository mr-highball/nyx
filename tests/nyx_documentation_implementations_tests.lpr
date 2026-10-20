{ Declared interface applications remain separate from class ancestry. MIT. }
program nyx_documentation_implementations_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.bytes, nyx.types, nyx.scheduler,
  nyx.pascal.lexing, nyx.pascal.syntax, nyx.pascal.types, nyx.pascal.work,
  nyx.documentation.typeexpressions, nyx.documentation,
  nyx.documentation.registration, nyx.documentation.registry,
  nyx.schema, nyx.callbacks, nyx.catalog, nyx.test.documentation
  {$ifdef PAS2JS}, Web{$else}, Classes{$endif};

type
  { An alternate vector can lie about its count, replace a final item or cancel
    on its last count read. Its lifetime must end with the capture operation. }
  TVectorSource = class(TInterfacedObject, INyxDocumentationInterfaceImplementations)
  public
    Value: INyxDocumentationInterfaceImplementations;
    Replacement: INyxDocumentationTypeUse;
    CountReads, ItemReads, ClaimedCount, ChangeItemAt, CancelCountAt: Integer;
    Lie: Boolean;
    Cancellation: INyxCancellationScope;
    destructor Destroy; override;
    function Count: Integer;
    function ItemAt(AIndex: Integer): INyxDocumentationTypeUse;
  end;
  { Existing named-use contracts are mutable only in this adversarial supplier.
    A late binding change must not survive a neighbouring vector observation. }
  TUseSource = class(TInterfacedObject, INyxDocumentationTypeUse)
  public
    Value: INyxDocumentationTypeUse;
    BindingReads, ChangeAt: Integer;
    function Reference: TNyxDocumentationRef;
    function Expression: INyxDocumentationTypeExpression;
    function BindingCount: Integer;
    function BindingAt(AIndex: Integer): TNyxDocumentationRef;
    function WithBinding(AIndex: Integer; const AReference: TNyxDocumentationRef): INyxDocumentationTypeUse;
  end;
  { No private-owner capability is borrowed. This supplier forwards legacy
    facts but can change new presence or a legacy envelope before publication. }
  TTypeSource = class(TInterfacedObject, INyxDocumentationType,
    INyxDocumentationDeclaredInterfaces)
  public
    Value: INyxDocumentationType;
    Interfaces, Replacement: INyxDocumentationInterfaceImplementations;
    Reads, SwapAt, Mutation, CancelAt, AfterCancel: Integer;
    PulseAbsent: Boolean;
    Cancellation: INyxCancellationScope;
    destructor Destroy; override;
    procedure Observe;
    function ImplementedInterfaces: INyxDocumentationInterfaceImplementations;
    function Identity: TNyxDocumentationRef;
    function Kind: TNyxDocumentationTypeKind;
    function Summary: TNyxText;
    function Evidence: TNyxDocumentationEvidence;
    function AliasTarget: TNyxDocumentationRef;
    function BaseCount: Integer;
    function BaseAt(AIndex: Integer): TNyxDocumentationRef;
    function MemberCount: Integer;
    function MemberAt(AIndex: Integer): INyxDocumentationMember;
    function ExampleCount: Integer;
    function ExampleAt(AIndex: Integer): TNyxDocumentationExample;
    function RelatedCount: Integer;
    function RelatedAt(AIndex: Integer): TNyxDocumentationRef;
    function WithAlias(const ATarget: TNyxDocumentationRef): INyxDocumentationType;
    function WithBase(const ABase: TNyxDocumentationRef): INyxDocumentationType;
    function WithMember(const AMember: INyxDocumentationMember): INyxDocumentationType;
    function WithExample(const AValue: TNyxDocumentationExample): INyxDocumentationType;
    function WithRelated(const AValue: TNyxDocumentationRef): INyxDocumentationType;
  end;
  { Caller-owned remaining allowance and retirement are observable. No result
    may retain this lease or read the next supplier after outward cancellation. }
  TAdmission = class(TInterfacedObject, INyxDocumentationTypeAdmission)
  public
    Calls, Remaining, CancelAt: Integer;
    Limited: Boolean;
    Target: INyxCancellationScope;
    destructor Destroy; override;
    procedure AdmitTypeWork(APhase: TNyxPascalAnalysisPhase; AUnits: Integer);
    function RemainingTypeBytes: Integer;
  end;

var
  GChecks, GDisposed, GLeases: Integer;
  {$ifndef PAS2JS}
  GActual: Integer;
  {$endif}

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin
  Inc(GChecks);

  if not AValue then
    raise Exception.Create(AReason);
end;

function R(const AName: TNyxText; AArity: Integer = 0): TNyxDocumentationRef;
begin
  Result := NyxDocumentationType('mechanics.implementations', AName, AArity);
end;

function Scope: INyxDocumentationTypeConstruction;
begin
  Result := NewNyxDocumentationTypeConstruction(TNyxDocumentationTypeExpressionLimits.Defaults.Bytes(NyxDocumentationMaximumBytes));
end;

function InterfacesOf(const AType: INyxDocumentationType): INyxDocumentationInterfaceImplementations;
var
  LCapability: INyxDocumentationDeclaredInterfaces;
begin
  Result := nil;
  Check(Supports(AType, INyxDocumentationDeclaredInterfaces, LCapability), 'Captured type exposes optional declaration facts');
  Result := LCapability.ImplementedInterfaces;
end;

function NamedVector(const AName: TNyxText;
  const AConstruction: INyxDocumentationTypeConstruction): INyxDocumentationInterfaceImplementations;
begin
  Result := NewNyxDocumentationInterfaceImplementations([NewNyxDocumentationNamedTypeUse(R(AName))], AConstruction);
end;

function GenericUse(const AText: TNyxText;
  const AConstruction: INyxDocumentationTypeConstruction;
  const ARoot: TNyxDocumentationRef): INyxDocumentationTypeUse;
var
  LExpression: INyxDocumentationTypeExpression;
  LRoot, LAt: Integer;
begin
  LExpression := NewNyxDocumentationTypeExpression(AText, ndtoImplementedInterface, AConstruction);
  Result := NewNyxDocumentationExpressionTypeUse(LExpression, AConstruction);
  LRoot := LExpression.Syntax.Root;

  if LExpression.Syntax.Node(LRoot).Kind = ptxGeneric then
    LRoot := LExpression.Syntax.Child(LRoot, ptrBase);
  for LAt := 0 to LExpression.NamedUseCount - 1 do
  begin

    if LExpression.NamedUseNode(LAt) = LRoot then
      Result := Result.WithBinding(LAt, ARoot)
    else if LExpression.NamedUseArity(LAt) = 1 then
      Result := Result.WithBinding(LAt, R('TOwner', 1))
    else
      Result := Result.WithBinding(LAt, R('TValue'));
  end;
end;

destructor TVectorSource.Destroy;
begin
  Inc(GDisposed);
  inherited Destroy;
end;

function TVectorSource.Count: Integer;
begin
  Inc(CountReads);
  Result := Value.Count;

  if Lie then
    Result := ClaimedCount;

  if (CancelCountAt > 0) and (CountReads >= CancelCountAt) and (Cancellation <> nil) then
    Cancellation.Cancel;
end;

function TVectorSource.ItemAt(AIndex: Integer): INyxDocumentationTypeUse;
begin
  Inc(ItemReads);
  Result := Value.ItemAt(AIndex);

  if (ChangeItemAt > 0) and (ItemReads >= ChangeItemAt) then
    Result := Replacement;
end;

function TUseSource.Reference: TNyxDocumentationRef;
begin
  Result := Value.Reference;
end;

function TUseSource.Expression: INyxDocumentationTypeExpression;
begin
  Result := Value.Expression;
end;

function TUseSource.BindingCount: Integer;
begin
  Result := Value.BindingCount;
end;

function TUseSource.BindingAt(AIndex: Integer): TNyxDocumentationRef;
begin
  Inc(BindingReads);
  Result := Value.BindingAt(AIndex);

  if (ChangeAt > 0) and (BindingReads >= ChangeAt) then
    Result := R('Changed', Result.GenericArity);
end;

function TUseSource.WithBinding(AIndex: Integer; const AReference: TNyxDocumentationRef): INyxDocumentationTypeUse;
begin
  Result := nil;
  raise Exception.Create('Inspection cannot call alternate fluent binding');
end;

destructor TTypeSource.Destroy;
begin
  Inc(GDisposed);
  inherited Destroy;
end;

procedure TTypeSource.Observe;
begin

  if (Cancellation <> nil) and Cancellation.Cancelled then
    Inc(AfterCancel);
end;

function TTypeSource.ImplementedInterfaces: INyxDocumentationInterfaceImplementations;
begin
  Observe;
  Inc(Reads);
  Result := Interfaces;

  if (SwapAt > 0) and (Reads >= SwapAt) then
    Result := Replacement;

  if PulseAbsent and (Reads in [2, 3]) then
    Result := nil;

  if (CancelAt > 0) and (Reads >= CancelAt) and (Cancellation <> nil) then
    Cancellation.Cancel;
end;

function TTypeSource.Identity: TNyxDocumentationRef;
begin
  Observe;
  Result := Value.Identity;

  if (Mutation = 1) and (Reads >= 2) then
    Result := R('Changed');
end;

function TTypeSource.Kind: TNyxDocumentationTypeKind;
begin
  Observe;
  Result := Value.Kind;

  if (Mutation = 2) and (Reads >= 2) then
    Result := ndtInterface;
end;

function TTypeSource.Summary: TNyxText;
begin
  Observe;
  Result := Value.Summary;

  if (Mutation = 3) and (Reads >= 2) then
    Result := 'Changed.';
end;

function TTypeSource.Evidence: TNyxDocumentationEvidence;
begin
  Observe;
  Result := Value.Evidence;

  if (Mutation = 4) and (Reads >= 2) then
    Result := ndeCatalogDeclaration;
end;

function TTypeSource.AliasTarget: TNyxDocumentationRef;
begin Observe; Result := Value.AliasTarget; end;
function TTypeSource.BaseCount: Integer;
begin Observe; Result := Value.BaseCount; end;
function TTypeSource.BaseAt(AIndex: Integer): TNyxDocumentationRef;
begin Observe; Result := Value.BaseAt(AIndex); end;
function TTypeSource.MemberCount: Integer;
begin Observe; Result := Value.MemberCount; end;
function TTypeSource.MemberAt(AIndex: Integer): INyxDocumentationMember;
begin Observe; Result := Value.MemberAt(AIndex); end;
function TTypeSource.ExampleCount: Integer;
begin Observe; Result := Value.ExampleCount; end;
function TTypeSource.ExampleAt(AIndex: Integer): TNyxDocumentationExample;
begin Observe; Result := Value.ExampleAt(AIndex); end;
function TTypeSource.RelatedCount: Integer;
begin Observe; Result := Value.RelatedCount; end;
function TTypeSource.RelatedAt(AIndex: Integer): TNyxDocumentationRef;
begin Observe; Result := Value.RelatedAt(AIndex); end;
function TTypeSource.WithAlias(const ATarget: TNyxDocumentationRef): INyxDocumentationType;
begin Result := nil; raise Exception.Create('No alternate fluent alias'); end;
function TTypeSource.WithBase(const ABase: TNyxDocumentationRef): INyxDocumentationType;
begin Result := nil; raise Exception.Create('No alternate fluent base'); end;
function TTypeSource.WithMember(const AMember: INyxDocumentationMember): INyxDocumentationType;
begin Result := nil; raise Exception.Create('No alternate fluent member'); end;
function TTypeSource.WithExample(const AValue: TNyxDocumentationExample): INyxDocumentationType;
begin Result := nil; raise Exception.Create('No alternate fluent example'); end;
function TTypeSource.WithRelated(const AValue: TNyxDocumentationRef): INyxDocumentationType;
begin Result := nil; raise Exception.Create('No alternate fluent related'); end;

destructor TAdmission.Destroy;
begin
  Inc(GLeases);
  inherited Destroy;
end;

procedure TAdmission.AdmitTypeWork(APhase: TNyxPascalAnalysisPhase; AUnits: Integer);
begin

  if (Ord(APhase) < Ord(Low(TNyxPascalAnalysisPhase))) or
    (Ord(APhase) > Ord(High(TNyxPascalAnalysisPhase))) or (AUnits < 1) then
    raise EArgumentException.Create('Known positive work required');

  if Limited and (AUnits > Remaining) then
    raise ENyxDocumentationTypeExpression.Create('Caller remaining work exhausted');

  if Limited then
    Dec(Remaining, AUnits);
  Inc(Calls);

  if (CancelAt > 0) and (Calls >= CancelAt) and (Target <> nil) then
    Target.Cancel;
end;

function TAdmission.RemainingTypeBytes: Integer;
begin
  Result := NyxDocumentationMaximumBytes;
end;

procedure ShapeCases;
var
  LScope: INyxDocumentationTypeConstruction;
  LPlain, LValue, LCopy: INyxDocumentationType;
  LVector: INyxDocumentationInterfaceImplementations;
  LUse: INyxDocumentationTypeUse;
  LRefused: Boolean;
  LAt: Integer;
begin
  LScope := Scope;
  try
    LPlain := NewNyxDocumentationType(R('C'), ndtClass, 'Class.');
    LCopy := CaptureNyxDocumentationType(LPlain, LScope);
    Check((InterfacesOf(LCopy) = nil) and (LScope.Receipt.Total = 0), 'Old absent shape preserves work and unknown intent');
    LVector := NamedVector('IContract', LScope);
    LValue := WithNyxDocumentationImplementedInterfaces(LPlain, LVector, LScope);
    LCopy := LValue.WithBase(R('TBase')).WithMember(NewNyxDocumentationMember(
      NyxDocumentationMember(R('C'), 'Own'), ndmMethod, 'Own.'))
      .WithExample(NyxDocumentationExample('Read', 'ReadValue;')).WithRelated(R('IContract'));
    Check((InterfacesOf(LPlain) = nil) and (InterfacesOf(LCopy).Count = 1) and
      (LCopy.BaseCount = 1) and (LCopy.MemberCount = 1), 'Copies retain implementations independently of bases and members');
    LCopy := CaptureNyxDocumentationType(LCopy, LScope);
    Check(InterfacesOf(LCopy).ItemAt(0).Reference.Same(R('IContract')), 'Captured vector retains exact declaring reference');
    LCopy := WithNyxDocumentationImplementedInterfaces(LCopy, NewNyxDocumentationInterfaceImplementations([], LScope), LScope);
    Check((InterfacesOf(LCopy) <> nil) and (InterfacesOf(LCopy).Count = 0), 'Known empty remains distinct from unknown');
    LCopy := WithNyxDocumentationImplementedInterfaces(LCopy, nil, LScope);
    Check((InterfacesOf(LCopy) = nil) and (LCopy.MemberCount = 1), 'Clear removes the shape and retains ordinary metadata');
    LUse := GenericUse('TOwner<TValue>.IContract<TValue>', LScope, R('IContract', 1));
    LVector := NewNyxDocumentationInterfaceImplementations([LUse], LScope);
    Check((LVector.ItemAt(0).Expression.Text = 'TOwner<TValue>.IContract<TValue>') and
      not LVector.ItemAt(0).BindingAt(0).Same(R('IContract', 1)), 'Applied root is found by topology rather than binding zero');
    for LAt := 0 to 4 do
    begin
      LRefused := False;
      try
        case LAt of
          0: LVector := NewNyxDocumentationInterfaceImplementations([NewNyxDocumentationNamedTypeUse(R('IGeneric', 1))], LScope);
          1: LVector := NewNyxDocumentationInterfaceImplementations([NewNyxDocumentationNamedTypeUse(R('IContract')), NewNyxDocumentationNamedTypeUse(R('IContract'))], LScope);
          2: LVector := NewNyxDocumentationInterfaceImplementations([NewNyxDocumentationExpressionTypeUse(NewNyxDocumentationTypeExpression('array of IContract', ndtoImplementedInterface, LScope), LScope)], LScope);
          3: LVector := NewNyxDocumentationInterfaceImplementations([NewNyxDocumentationExpressionTypeUse(NewNyxDocumentationTypeExpression('IContract', ndtoParameter, LScope), LScope)], LScope);
          4: LCopy := WithNyxDocumentationImplementedInterfaces(NewNyxDocumentationType(R('I'), ndtInterface, 'Interface.'), NamedVector('IContract', LScope), LScope);
        end;
      except
        on ENyxDocumentation do LRefused := True;
      end;
      Check(LRefused, 'Unapplied generic/duplicate/non-designator/wrong role/wrong owner refuses ' + IntToStr(LAt));
    end;
    LRefused := False;
    try LCopy := WithNyxDocumentationImplementedInterfaces(LPlain, NamedVector('C', LScope), LScope);
    except on ENyxDocumentation do LRefused := True; end;
    Check(LRefused, 'Class self reference cannot masquerade as its interface');
  finally LScope.Retire; end;
  Check(InterfacesOf(LValue).Count = 1, 'Retirement leaves only immutable relationship data');
end;

procedure ReuseCases;
const
  CText = 'IContract<TValue>';
var
  LPrepare, LScope: INyxDocumentationTypeConstruction;
  LShared, LIndependent: INyxDocumentationTypeUse;
  LFirst, LSecond: INyxDocumentationInterfaceImplementations;
  LAt: Integer;
  LRefused: Boolean;
begin
  LPrepare := Scope;
  try
    LShared := NewNyxDocumentationExpressionTypeUse(NewNyxDocumentationTypeExpression(CText, ndtoImplementedInterface, LPrepare), LPrepare);
    LIndependent := NewNyxDocumentationExpressionTypeUse(NewNyxDocumentationTypeExpression(CText, ndtoImplementedInterface, LPrepare), LPrepare);
  finally LPrepare.Retire; end;
  for LAt := 0 to 1 do
  begin
    { One copied expression plus two one-byte count fields fits exactly. The
      independently captured equal expression cannot borrow its storage receipt. }
    LScope := NewNyxDocumentationTypeConstruction(TNyxDocumentationTypeExpressionLimits.Defaults.Bytes(NyxUTF8ByteCount(CText) + 2));
    try
      LFirst := NewNyxDocumentationInterfaceImplementations([LShared], LScope);
      LSecond := nil;
      LRefused := False;
      try

        if LAt = 0 then
          LSecond := NewNyxDocumentationInterfaceImplementations([LShared], LScope)
        else
          LSecond := NewNyxDocumentationInterfaceImplementations([LIndependent], LScope);
      except
        on ENyxDocumentationTypeExpression do LRefused := True;
      end;
      Check((LAt = 0) <> LRefused, 'Exact canonical expression reuse differs from independently captured equal syntax');
      Check((LFirst.Count = 1) and ((LSecond = nil) = LRefused), 'Copy allowance refusal retains earlier captured vector only');
    finally LScope.Retire; end;
  end;
end;

procedure AlternateCases;
var
  LScope: INyxDocumentationTypeConstruction;
  LSource: TTypeSource;
  LLease, LResult: INyxDocumentationType;
  LVector: TVectorSource;
  LVectorLease: INyxDocumentationInterfaceImplementations;
  LUse: TUseSource;
  LUseLease: INyxDocumentationTypeUse;
  LAt: Integer;
  LRefused: Boolean;
begin
  for LAt := 0 to 11 do
  begin
    LScope := Scope;
    LSource := TTypeSource.Create;
    LLease := LSource;
    LSource.Value := NewNyxDocumentationType(R('C'), ndtClass, 'Class.');
    LVector := TVectorSource.Create;
    LVectorLease := LVector;
    LVector.Value := NamedVector('IContract', LScope);
    LSource.Interfaces := LVectorLease;
    try
      case LAt of
        1: begin LVector.Lie := True; LVector.ClaimedCount := NyxDocumentationMaximumLinks + 1; end;
        2: begin LVector.Replacement := NewNyxDocumentationNamedTypeUse(R('Changed')); LVector.ChangeItemAt := 2; end;
        3: begin LVector.Cancellation := LScope; LVector.CancelCountAt := 2; end;
        4: LSource.SwapAt := 2;
        5: begin LSource.Interfaces := nil; LSource.Replacement := LVectorLease; LSource.SwapAt := 2; end;
        6..9: LSource.Mutation := LAt - 5;
        10: begin LSource.Cancellation := LScope; LSource.CancelAt := 2; end;
        11:
          begin
            LUse := TUseSource.Create;
            LUseLease := LUse;
            LUse.Value := GenericUse('IContract<TValue>', LScope, R('IContract', 1));
            LVector.Value := NewNyxDocumentationInterfaceImplementations([LUseLease], LScope);
            LUse.BindingReads := 0;
            LUse.ChangeAt := 5;
            { Keep the alternate item rather than the constructor's frozen copy. }
            LVector.Replacement := LUseLease;
            LVector.ChangeItemAt := 1;
          end;
      end;
      LResult := nil;
      LRefused := False;
      try LResult := CaptureNyxDocumentationType(LLease, LScope);
      except on ENyxDocumentation do LRefused := True; end;
      Check((LAt = 0) <> LRefused, 'Stable alternate succeeds; changing/lying/cancelling suppliers refuse ' + IntToStr(LAt));

      if LAt <> 0 then
        Check(LResult = nil, 'Refusal publishes no partial class');

      if LAt = 1 then
        Check(LVector.ItemReads = 0, 'Count bound refuses before any item getter');

      if LAt = 10 then
        Check(LSource.AfterCancel = 0, 'Cancellation stops subsequent source getters');
    finally LScope.Retire; end;
  end;
  LScope := Scope;
  LSource := TTypeSource.Create;
  LLease := LSource;
  LSource.Value := NewNyxDocumentationType(R('C'), ndtClass, 'Class.');
  LSource.Interfaces := NamedVector('IContract', LScope);
  LSource.PulseAbsent := True;
  try
    LResult := nil;
    LRefused := False;
    try LResult := WithNyxDocumentationImplementedInterfaces(LLease, nil, LScope);
    except on ENyxDocumentation do LRefused := True; end;
    Check(LRefused and (LResult = nil), 'Outer value/nil/nil/value presence cannot borrow inner capture authority');
  finally LScope.Retire; end;
end;

function RetiredValue: INyxDocumentationType;
var
  LScope: INyxDocumentationTypeConstruction;
  LAdmission: TAdmission;
  LAdmissionLease: INyxDocumentationTypeAdmission;
  LSource: TTypeSource;
  LLease: INyxDocumentationType;
  LVector: TVectorSource;
begin
  LAdmission := TAdmission.Create;
  LAdmissionLease := LAdmission;
  LScope := NewNyxDocumentationTypeConstruction(TNyxDocumentationTypeExpressionLimits.Defaults, nil, LAdmissionLease);
  LSource := TTypeSource.Create;
  LLease := LSource;
  LSource.Value := NewNyxDocumentationType(R('C'), ndtClass, 'Class.');
  LVector := TVectorSource.Create;
  LVector.Value := NamedVector('IContract', LScope);
  LSource.Interfaces := LVector;
  try Result := CaptureNyxDocumentationType(LLease, LScope);
  finally LScope.Retire; end;
end;

procedure BoundsCases;
var
  LScope: INyxDocumentationTypeConstruction;
  LSource: TTypeSource;
  LLease, LResult, LFirst: INyxDocumentationType;
  LVector: TVectorSource;
  LAdmission: TAdmission;
  LAdmissionLease: INyxDocumentationTypeAdmission;
  LRefused: Boolean;
  LAt, LBefore, LBeforeLeases, LLastAdmission: Integer;
begin
  for LAt := 0 to 1 do
  begin
    LScope := Scope;
    LVector := TVectorSource.Create;
    LVector.Value := NewNyxDocumentationInterfaceImplementations([
      NewNyxDocumentationNamedTypeUse(R('IContract')), NewNyxDocumentationNamedTypeUse(R('IOther'))], LScope);
    LSource := TTypeSource.Create;
    LLease := LSource;
    LSource.Value := NewNyxDocumentationType(R('C'), ndtClass, 'Class.');
    LSource.Interfaces := LVector;
    LScope.Retire;

    if LAt = 0 then
      LScope := NewNyxDocumentationTypeConstruction(TNyxDocumentationTypeExpressionLimits.Defaults.Work(1))
    else
      LScope := NewNyxDocumentationTypeConstruction(TNyxDocumentationTypeExpressionLimits.Defaults.Bytes(3));
    try
      LResult := nil;
      LRefused := False;
      try LResult := CaptureNyxDocumentationType(LLease, LScope);
      except on ENyxDocumentationTypeExpression do LRefused := True; end;
      Check(LRefused and (LResult = nil), 'Small shared work/copy allowance refuses atomically');

      if LAt = 0 then
        Check((LVector.CountReads = 0) and (LVector.ItemReads = 0), 'Work refusal precedes vector supplier reads')
      else
        Check(LVector.ItemReads = 1, 'Byte refusal never reads a later row or resets copy admission');
    finally LScope.Retire; end;
  end;
  LAdmission := TAdmission.Create;
  LAdmissionLease := LAdmission;
  LScope := NewNyxDocumentationTypeConstruction(TNyxDocumentationTypeExpressionLimits.Defaults, nil, LAdmissionLease);
  try
    LSource := TTypeSource.Create;
    LLease := LSource;
    LSource.Value := NewNyxDocumentationType(R('C'), ndtClass, 'Class.');
    LSource.Interfaces := NamedVector('IContract', LScope);
    LFirst := CaptureNyxDocumentationType(LLease, LScope);
    LAdmission.Limited := True;
    LAdmission.Remaining := 0;
    LSource.Reads := 0;
    LResult := nil;
    LRefused := False;
    try LResult := CaptureNyxDocumentationType(LLease, LScope);
    except on ENyxDocumentationTypeExpression do LRefused := True; end;
    Check(LRefused and (LResult = nil) and (LSource.Reads = 0) and (InterfacesOf(LFirst).Count = 1),
      'A second capture uses caller remaining work and preserves the first immutable result');
  finally LScope.Retire; end;
  LAdmission := TAdmission.Create;
  LAdmissionLease := LAdmission;
  LScope := NewNyxDocumentationTypeConstruction(TNyxDocumentationTypeExpressionLimits.Defaults, nil, LAdmissionLease);
  try
    LFirst := CaptureNyxDocumentationType(LLease, LScope);
    LLastAdmission := LAdmission.Calls;
  finally LScope.Retire; end;
  LAdmission := TAdmission.Create;
  LAdmission.CancelAt := LLastAdmission;
  LAdmissionLease := LAdmission;
  LScope := NewNyxDocumentationTypeConstruction(TNyxDocumentationTypeExpressionLimits.Defaults, nil, LAdmissionLease);
  LAdmission.Target := LScope;
  try
    LResult := nil;
    LRefused := False;
    try LResult := CaptureNyxDocumentationType(LLease, LScope);
    except on ENyxDocumentationCancelled do LRefused := True; end;
    Check(LRefused and (LResult = nil) and (LAdmission.Calls = LLastAdmission) and
      (InterfacesOf(LFirst).Count = 1), 'Last outward admission revokes publication without revoking the prior value');
  finally
    LAdmission.Target := nil;
    LScope.Retire;
  end;
  LBefore := GDisposed;
  LBeforeLeases := GLeases;
  LResult := RetiredValue;
  Check((GDisposed = LBefore + 2) and (GLeases = LBeforeLeases + 1) and
    (InterfacesOf(LResult).Count = 1), 'Published type retains no alternate producer or outward admission');
end;

function Bundle(ACase: Integer; AEmpty: Boolean = False): INyxDocumentationRegistration;
var
  LScope: INyxDocumentationTypeConstruction;
  LDeclarations: TNyxDocumentationDeclarations;
  LClass: INyxDocumentationType;
  LUse: INyxDocumentationTypeUse;
begin
  LScope := Scope;
  try
    SetLength(LDeclarations, 5);
    LClass := NewNyxDocumentationType(R('C'), ndtClass, 'Class.').WithBase(R('TBase'))
      .WithMember(NewNyxDocumentationMember(NyxDocumentationMember(R('C'), 'Own'), ndmMethod, 'Own.'));
    LUse := GenericUse('TOwner<TValue>.IContract<TValue>', LScope, R('IContract', 1));

    if ACase = 4 then
      LUse := NewNyxDocumentationExpressionTypeUse(NewNyxDocumentationTypeExpression('IContract<TValue>', ndtoImplementedInterface, LScope), LScope);

    if AEmpty then
      LClass := WithNyxDocumentationImplementedInterfaces(LClass, NewNyxDocumentationInterfaceImplementations([], LScope), LScope)
    else
      LClass := WithNyxDocumentationImplementedInterfaces(LClass, NewNyxDocumentationInterfaceImplementations([LUse], LScope), LScope);
    LDeclarations[0] := LClass;
    LDeclarations[1] := NewNyxDocumentationType(R('TBase'), ndtClass, 'Base.')
      .WithMember(NewNyxDocumentationMember(NyxDocumentationMember(R('TBase'), 'Inherited'), ndmMethod, 'Inherited.'));

    if ACase = 1 then
      LDeclarations[2] := NewNyxDocumentationType(R('IContract', 1), ndtClass, 'Wrong root.')
    else
      LDeclarations[2] := NewNyxDocumentationType(R('IContract', 1), ndtInterface, 'Interface.')
        .WithMember(NewNyxDocumentationMember(NyxDocumentationMember(R('IContract', 1), 'InterfaceOnly'), ndmMethod, 'Interface only.'));
    LDeclarations[3] := NewNyxDocumentationType(R('TValue'), ndtRecord, 'Value.');
    LDeclarations[4] := NewNyxDocumentationType(R('TOwner', 1), ndtClass, 'Generic owner.');

    if ACase = 2 then
      LDeclarations[2] := NewNyxDocumentationType(R('Unrelated', 1), ndtInterface, 'Other.');

    if ACase = 3 then
      LDeclarations[3] := NewNyxDocumentationType(R('UnrelatedValue'), ndtRecord, 'Other.');
    Result := NewNyxDocumentationRegistration(NyxDocumentationUnit('mechanics.implementations'), R('C'), LDeclarations,
      [ndplatformNative, ndplatformBrowser]);
  finally LScope.Retire; end;
end;

function Frame(const AText: TNyxText): TNyxText;
begin
  Result := IntToStr(NyxUTF8ByteCount(AText)) + ':' + AText;
end;

procedure IndexCases;
var
  LRegistry: TNyxDocumentationRegistry;
  LCatalog: TNyxCatalog;
  LIndex, LBefore: INyxDocumentationIndex;
  LAt, LVersion: Integer;
  LRefused: Boolean;
  LScope: INyxDocumentationTypeConstruction;
  LDeclarations: TNyxDocumentationDeclarations;
  LRegistration: INyxDocumentationRegistration;
  LExpected: TNyxText;
begin
  LScope := Scope;
  try
    SetLength(LDeclarations, 1);
    LDeclarations[0] := NewNyxDocumentationType(R('C'), ndtClass, 'Class.');
    LExpected := Frame(R('C').Key) + Frame('0') + Frame('Class.') + Frame('0') + Frame('') +
      Frame('0') + Frame('0') + Frame('0') + Frame('0');
    LRegistration := NewNyxDocumentationRegistration(NyxDocumentationUnit('mechanics.implementations'), R('C'), LDeclarations);
    Check(LRegistration.DeclarationContentAt(0) = LExpected, 'Unknown shape retains independently constructed old framing');
    LDeclarations[0] := WithNyxDocumentationImplementedInterfaces(LDeclarations[0], NewNyxDocumentationInterfaceImplementations([], LScope), LScope);
    LRegistration := NewNyxDocumentationRegistration(NyxDocumentationUnit('mechanics.implementations'), R('C'), LDeclarations);
    Check(LRegistration.DeclarationContentAt(0) = LExpected + Frame('implemented-interfaces') + Frame('0'), 'Known empty has distinct exact framing');
    LDeclarations[0] := WithNyxDocumentationImplementedInterfaces(LDeclarations[0], nil, LScope);
    LRegistration := NewNyxDocumentationRegistration(NyxDocumentationUnit('mechanics.implementations'), R('C'), LDeclarations);
    Check(LRegistration.DeclarationContentAt(0) = LExpected, 'Clear restores old framing without stale fields');
  finally LScope.Retire; end;
  LRegistry := TNyxDocumentationRegistry.Create;
  LCatalog := TNyxCatalog.Create;
  try
    LCatalog.RegisterKind('good', 'Good', 'Custom', False, Bundle(0));
    LIndex := LRegistry.Refresh(CaptureNyxSchemas, CaptureNyxCallbacks, LCatalog.Capture, 0);
    Check((LIndex.DeclarationCount = 5) and (LIndex.Members(R('C')).Count = 2) and
      (LIndex.Members(R('IContract', 1)).Count = 1), 'Argument classes/records resolve; implemented interfaces add no synthetic class members');
    LBefore := LIndex;
    LVersion := LRegistry.Version;
    for LAt := 1 to 5 do
    begin
      LCatalog.Free;
      LCatalog := TNyxCatalog.Create;

      if LAt = 5 then
      begin
        LCatalog.RegisterKind('first', 'First', 'Custom', False, Bundle(0));
        LCatalog.RegisterKind('second', 'Second', 'Custom', False, Bundle(0, True));
      end
      else
        LCatalog.RegisterKind('invalid', 'Invalid', 'Custom', False, Bundle(LAt));
      LIndex := nil;
      LRefused := False;
      try LIndex := LRegistry.Refresh(CaptureNyxSchemas, CaptureNyxCallbacks, LCatalog.Capture, LVersion);
      except on ENyxDocumentation do LRefused := True; end;
      Check(LRefused and (LIndex = nil) and (LRegistry.Current = LBefore) and (LRegistry.Version = LVersion),
        'Wrong/missing/unbound/conflicting relationship refuses without replacing Current/version ' + IntToStr(LAt));
    end;
  finally
    LCatalog.Free;
    LRegistry.Free;
  end;
end;

procedure AddDeclaration(ACatalog: TNyxCatalog; const AKind: TNyxText;
  const ADeclaration: INyxDocumentationType; APlatforms: TNyxDocumentationPlatforms);
var
  LDeclarations: TNyxDocumentationDeclarations;
begin
  SetLength(LDeclarations, 1);
  LDeclarations[0] := ADeclaration;
  ACatalog.RegisterKind(AKind, AKind, 'Custom', False, NewNyxDocumentationRegistration(
    NyxDocumentationUnit('mechanics.implementations'), ADeclaration.Identity, LDeclarations, APlatforms));
end;

procedure PlatformCases;
var
  LScope: INyxDocumentationTypeConstruction;
  LRegistry: TNyxDocumentationRegistry;
  LCatalog: TNyxCatalog;
  LClass, LAlias, LSecond, LArgument: INyxDocumentationType;
  LIndex, LBefore: INyxDocumentationIndex;
  LOwnerPlatforms, LAliasPlatforms, LArgumentPlatforms: TNyxDocumentationPlatforms;
  LAt, LVersion: Integer;
  LRefused, LExpected: Boolean;
begin
  LRegistry := TNyxDocumentationRegistry.Create;
  try
    for LAt := 0 to 7 do
    begin
      LScope := Scope;
      LCatalog := TNyxCatalog.Create;
      try
        LOwnerPlatforms := [ndplatformNative, ndplatformBrowser];
        LAliasPlatforms := LOwnerPlatforms;
        LArgumentPlatforms := LOwnerPlatforms;
        LClass := WithNyxDocumentationImplementedInterfaces(NewNyxDocumentationType(R('C'), ndtClass, 'Class.'),
          NewNyxDocumentationInterfaceImplementations([GenericUse('IAlias<TValue>', LScope, R('IAlias', 1))], LScope), LScope);
        LAlias := NewNyxDocumentationType(R('IAlias', 1), ndtAlias, 'Alias.').WithAlias(R('IContract', 1));
        LSecond := NewNyxDocumentationType(R('ISecond', 1), ndtInterface, 'Other interface.');
        LArgument := NewNyxDocumentationType(R('TValue'), ndtRecord, 'Argument record.');
        case LAt of
          1: LAlias := NewNyxDocumentationTargetAlias(R('IAlias', 1), 'Alias.', [
            NyxDocumentationAliasTarget(npfNativeLCL, R('TWrong')),
            NyxDocumentationAliasTarget(npfBrowser, R('IContract', 1))]);
          2, 3:
            begin
              LAlias := NewNyxDocumentationTargetAlias(R('IAlias', 1), 'Alias.', [
                NyxDocumentationAliasTarget(npfBrowser, R('IContract', 1))]);
              LAliasPlatforms := [ndplatformBrowser];

              if LAt = 3 then
                LOwnerPlatforms := [ndplatformBrowser];
            end;
          4: begin LOwnerPlatforms := [ndplatformNative]; LArgumentPlatforms := [ndplatformBrowser]; end;
          5: LArgument := NewNyxDocumentationType(R('TValue'), ndtClass, 'Argument class.');
          6:
            begin
              LAlias := NewNyxDocumentationType(R('IAlias', 1), ndtAlias, 'Alias.').WithAlias(R('ISecond', 1));
              LSecond := NewNyxDocumentationType(R('ISecond', 1), ndtAlias, 'Cycle.').WithAlias(R('IAlias', 1));
            end;
          7: LArgumentPlatforms := [];
        end;
        AddDeclaration(LCatalog, 'class', LClass, LOwnerPlatforms);
        AddDeclaration(LCatalog, 'alias', LAlias, LAliasPlatforms);
        AddDeclaration(LCatalog, 'interface', NewNyxDocumentationType(R('IContract', 1), ndtInterface, 'Interface.'), [ndplatformNative, ndplatformBrowser]);
        AddDeclaration(LCatalog, 'argument', LArgument, LArgumentPlatforms);
        AddDeclaration(LCatalog, 'wrong', NewNyxDocumentationType(R('TWrong'), ndtClass, 'Wrong root.'), [ndplatformNative, ndplatformBrowser]);
        AddDeclaration(LCatalog, 'second', LSecond, [ndplatformNative, ndplatformBrowser]);
        LBefore := LRegistry.Current;
        LVersion := LRegistry.Version;
        LIndex := nil;
        LRefused := False;
        try LIndex := LRegistry.Refresh(CaptureNyxSchemas, CaptureNyxCallbacks, LCatalog.Capture, LVersion);
        except on ENyxDocumentation do LRefused := True; end;
        LExpected := LAt in [0, 3, 5, 7];
        Check(LExpected <> LRefused, 'Alias root/argument availability and cycle rules retain declared facts ' + IntToStr(LAt));

        if LRefused then
          Check((LIndex = nil) and (LRegistry.Current = LBefore) and (LRegistry.Version = LVersion), 'Availability/kind refusal preserves the prior publication');
      finally
        LCatalog.Free;
        LScope.Retire;
      end;
    end;
  finally LRegistry.Free; end;
end;

{$ifndef PAS2JS}
function RepositoryArgument: String;
begin

  if ParamCount <> 1 then
    raise EArgumentException.Create('Usage: nyx_documentation_implementations_tests <repository-root>');
  Result := IncludeTrailingPathDelimiter(ExpandFileName(ParamStr(1)));

  if not DirectoryExists(Result) or not FileExists(Result + 'PROJECT.md') or
    not FileExists(Result + 'src/nyx.controls.pas') or not FileExists(Result + 'src/nyx.controls.types.inc') or
    not FileExists(Result + 'tests/nyx.test.documentation.pas') or
    not FileExists(Result + 'src/nyx.application.queryfeedback.pas') or
    not FileExists(Result + 'src/nyx.linq.execution.pas') or not FileExists(Result + 'src/nyx.documentation.pas') then
    raise EArgumentException.Create('Repository root requires all actual relationship source owners');
end;

function SourceSyntax(const APath: String; AFragment: TNyxPascalSyntaxFragment = psfModule): INyxPascalSyntaxSnapshot;
var
  LStream: TMemoryStream;
  LBytes: TNyxBytes;
  LOptions: TNyxPascalSyntaxOptions;
begin
  Result := nil;
  LOptions := TNyxPascalSyntaxOptions.Defaults.Fragment(AFragment);
  LStream := TMemoryStream.Create;
  try
    LStream.LoadFromFile(APath);

    if LStream.Size > LOptions.LexicalOptions.MaximumSourceBytes then
      raise Exception.Create('Actual relationship source exceeds lexical allowance');
    SetLength(LBytes, Integer(LStream.Size));

    if Length(LBytes) > 0 then
      Move(LStream.Memory^, LBytes[0], Length(LBytes));
    Result := NewNyxPascalSyntaxSnapshot(NyxDecodeUTF8(LBytes), LOptions);
  finally LStream.Free; end;
end;

function FullType(const ASource: INyxPascalSyntaxSnapshot; const AName: TNyxText;
  AForm: TNyxPascalSyntaxTypeForm): TNyxPascalSyntaxDeclaration;
var
  LAt, LFound: Integer;
  LDeclaration: TNyxPascalSyntaxDeclaration;
begin
  Result := Default(TNyxPascalSyntaxDeclaration);
  LFound := 0;
  for LAt := 0 to ASource.DeclarationCount - 1 do
  begin
    LDeclaration := ASource.Declaration(LAt);

    if (LDeclaration.Kind = psdkType) and (LDeclaration.Name = AName) and
      not (psdfForward in LDeclaration.Flags) then
    begin
      Check((LDeclaration.TypeForm = AForm) and (LDeclaration.NestedScope >= 0) and
        (LDeclaration.Flags * [psdfIncomplete, psdfUnsupported, psdfConditional] = []) and
        ASource.Scope(LDeclaration.NestedScope).Complete, 'Actual type has its complete original declared shape');
      Result := LDeclaration;
      Inc(LFound);
    end;
  end;
  Check(LFound = 1, 'Exactly one actual full type is required: ' + AName);
end;

procedure ActualSources(const ARepository: String);
var
  LControls, LTypes, LFixture, LCore, LQuery, LExecution: INyxPascalSyntaxSnapshot;
  LClass, LInterface, LOutcome: TNyxPascalSyntaxDeclaration;
  LScope: INyxDocumentationTypeConstruction;
  LVector: INyxDocumentationInterfaceImplementations;
  LUse: INyxDocumentationTypeUse;
  LExpression: INyxDocumentationTypeExpression;
  LRoot, LRootSlot, LArgumentSlot, LAt: Integer;
begin
  LControls := SourceSyntax(ARepository + 'src/nyx.controls.pas');
  LTypes := SourceSyntax(ARepository + 'src/nyx.controls.types.inc', psfTypes);
  Check((LControls.Scope(0).Name = 'nyx.controls') and
    (Pos('{$I nyx.controls.types.inc}', LControls.Lexical.Source) > 0), 'Actual control unit owns this maintained type include');
  LClass := FullType(LTypes, 'TNyxBadge', pstfClass);
  LInterface := FullType(LTypes, 'INyxBadge', pstfInterface);
  Check(LTypes.Lexical.Slice(LClass.AncestorSpan) = '(TNyxCaptionControl, INyxBadge)', 'Badge class base and declared interface remain separate actual source facts');
  LScope := Scope;
  try
    LVector := NewNyxDocumentationInterfaceImplementations([NewNyxDocumentationNamedTypeUse(
      NyxDocumentationType(LControls.Scope(0).Name, LInterface.Name))], LScope);
    Check(LVector.ItemAt(0).Reference.Same(NyxDocumentationType('nyx.controls', 'INyxBadge')), 'Actual badge interface vector retains the original declaring unit');
    Inc(GActual);
    LFixture := SourceSyntax(ARepository + 'tests/nyx.test.documentation.pas');
    LCore := SourceSyntax(ARepository + 'src/nyx.documentation.pas');
    LClass := FullType(LFixture, 'TDocumentationBadge', pstfClass);
    LInterface := FullType(LFixture, 'IDocumentationBadge', pstfInterface);
    LOutcome := FullType(LCore, 'INyxDocumentationSource', pstfInterface);
    Check((LFixture.Scope(0).Name = 'nyx.test.documentation') and (LCore.Scope(0).Name = 'nyx.documentation') and
      (LFixture.Lexical.Slice(LClass.AncestorSpan) = '(TNyxBadge, IDocumentationBadge, INyxDocumentationSource)'), 'Actual extension owns two distinct interface relationships');
    LVector := NewNyxDocumentationInterfaceImplementations([
      NewNyxDocumentationNamedTypeUse(NyxDocumentationType(LFixture.Scope(0).Name, LInterface.Name)),
      NewNyxDocumentationNamedTypeUse(NyxDocumentationType(LCore.Scope(0).Name, LOutcome.Name))], LScope);
    Check((LVector.Count = 2) and (LVector.ItemAt(1).Reference.UnitName = 'nyx.documentation'), 'Both actual extension interfaces retain their exact units');
    Inc(GActual);
    LQuery := SourceSyntax(ARepository + 'src/nyx.application.queryfeedback.pas');
    LExecution := SourceSyntax(ARepository + 'src/nyx.linq.execution.pas');
    LClass := FullType(LQuery, 'TQueryFeedbackPort', pstfClass);
    LInterface := FullType(LExecution, 'INyxQueryResultPort', pstfInterface);
    LOutcome := FullType(LExecution, 'TNyxQueryOutcome', pstfRecord);
    Check((LQuery.Scope(0).Name = 'nyx.application.queryfeedback') and (LExecution.Scope(0).Name = 'nyx.linq.execution') and
      (LExecution.Lexical.Slice(LInterface.GenericSpan) = '<T>') and
      (LQuery.Lexical.Slice(LClass.AncestorSpan) = '(TInterfacedObject, INyxQueryResultPort<TNyxQueryOutcome>)'), 'Concrete generic application and its real declarations are mandatory');
    LExpression := NewNyxDocumentationTypeExpression('INyxQueryResultPort<TNyxQueryOutcome>', ndtoImplementedInterface, LScope);
    LUse := NewNyxDocumentationExpressionTypeUse(LExpression, LScope);
    LRoot := LExpression.Syntax.Child(LExpression.Syntax.Root, ptrBase);
    LRootSlot := -1;
    LArgumentSlot := -1;
    for LAt := 0 to LExpression.NamedUseCount - 1 do
    begin

      if LExpression.NamedUseNode(LAt) = LRoot then
      begin
        LRootSlot := LAt;
        LUse := LUse.WithBinding(LAt, NyxDocumentationType(LExecution.Scope(0).Name, LInterface.Name, 1));
      end
      else
      begin
        Check(LExpression.Syntax.Node(LExpression.NamedUseNode(LAt)).Name = LOutcome.Name, 'Actual argument occurrence names the witnessed concrete record');
        LArgumentSlot := LAt;
        LUse := LUse.WithBinding(LAt, NyxDocumentationType(LExecution.Scope(0).Name, LOutcome.Name));
      end;
    end;
    Check((LRootSlot >= 0) and (LArgumentSlot >= 0) and (LExpression.NamedUseCount = 2), 'Actual root and concrete argument are selected by typed topology');
    LVector := NewNyxDocumentationInterfaceImplementations([LUse], LScope);
    Check((LVector.ItemAt(0).Expression.Text = 'INyxQueryResultPort<TNyxQueryOutcome>') and
      (LVector.ItemAt(0).BindingAt(LRootSlot).GenericArity = 1) and
      LVector.ItemAt(0).BindingAt(LArgumentSlot).Same(NyxDocumentationType('nyx.linq.execution', 'TNyxQueryOutcome')),
      'Actual generic relationship preserves application and concrete argument without fake formal bindings');
    Inc(GActual);
    Check(GActual = 3, 'All badge, extension and concrete generic source relationships inspected');
    { These are selected relationship vectors. No partial core class/unit is
      constructed or registered, and no complete source adoption is claimed. }
  finally LScope.Retire; end;
end;

var
  LRepository: String;
{$endif}
begin
  try
    {$ifndef PAS2JS}
    LRepository := RepositoryArgument;
    {$endif}
    ShapeCases;
    ReuseCases;
    AlternateCases;
    BoundsCases;
    IndexCases;
    PlatformCases;
    {$ifndef PAS2JS}
    ActualSources(LRepository);
    {$endif}
    Check((DocumentationControlsConstructed = 0) and (DocumentationCallbacksConstructed = 0) and
      (DocumentationCallbacksInvoked = 0) and (DocumentationGlobalFactoryCalls = 0) and
      (DocumentationGlobalRoutineCalls = 0), 'Inspection executes no control, callback, factory or example');
    {$ifdef PAS2JS}
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' portable implementation checks';
    {$else}
    WriteLn('PASS ', GChecks, ' implementation checks / all3 actual source relationships inspected');
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
