{ Scoped formal metadata, independent of compiler substitution. MIT. }
program nyx_documentation_formals_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.bytes, nyx.types, nyx.scheduler,
  nyx.pascal.lexing, nyx.pascal.syntax, nyx.pascal.types, nyx.pascal.work,
  nyx.documentation.typeexpressions, nyx.documentation,
  nyx.documentation.registration, nyx.documentation.registry,
  nyx.catalog, nyx.schema, nyx.callbacks
  {$ifdef PAS2JS}, Web{$else}, Classes{$endif};

type
  { Borrowed scalar suppliers can change an identity after its initial read. }
  TFormalSource = class(TInterfacedObject, INyxDocumentationFormalParameter)
  public
    Value: INyxDocumentationFormalParameter;
    Reads, Mutation, CancelAt: Integer;
    Cancellation: INyxCancellationScope;
    destructor Destroy; override;
    procedure Observe;
    function Owner: TNyxDocumentationRef;
    function ParameterIndex: Integer;
    function Name: TNyxText;
  end;
  { A mutable vector exposes count preflight and final item-identity checks. }
  TParametersSource = class(TInterfacedObject, INyxDocumentationGenericParameters)
  public
    Value: INyxDocumentationGenericParameters;
    Item: INyxDocumentationFormalParameter;
    CountReads, ItemReads, ClaimedCount, ReplaceAt, CancelAt: Integer;
    Lie: Boolean;
    Cancellation: INyxCancellationScope;
    destructor Destroy; override;
    function Count: Integer;
    function ParameterAt(AIndex: Integer): INyxDocumentationFormalParameter;
  end;
  { Alternate use facts exercise nominal/formal dual claims and late map changes. }
  TUseSource = class(TInterfacedObject, INyxDocumentationTypeUse,
    INyxDocumentationFormalBindings)
  public
    Value: INyxDocumentationTypeUse;
    Formal: INyxDocumentationFormalParameter;
    Reads, ReplaceAt, DropAt: Integer;
    Dual: Boolean;
    function Reference: TNyxDocumentationRef;
    function Expression: INyxDocumentationTypeExpression;
    function BindingCount: Integer;
    function BindingAt(AIndex: Integer): TNyxDocumentationRef;
    function FormalAt(AIndex: Integer): INyxDocumentationFormalParameter;
    function WithBinding(AIndex: Integer; const AReference: TNyxDocumentationRef): INyxDocumentationTypeUse;
  end;
  { Source metadata can pulse nil or alter its envelope while capture proceeds.
    No private captured-owner receipt is advertised by this alternate. }
  TTypeSource = class(TInterfacedObject, INyxDocumentationType,
    INyxDocumentationGenericDeclaration)
  public
    Value: INyxDocumentationType;
    Parameters: INyxDocumentationGenericParameters;
    Reads, Mutation, CancelAt, AfterCancel, MemberReads: Integer;
    Pulse: Boolean;
    Cancellation: INyxCancellationScope;
    destructor Destroy; override;
    procedure Observe;
    function GenericParameters: INyxDocumentationGenericParameters;
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
  { The outer caller can cancel at the last outward admission. Published values
    must not retain this lease or its mutable cancellation target. }
  TAdmission = class(TInterfacedObject, INyxDocumentationTypeAdmission)
  public
    Calls, CancelAt, Remaining: Integer;
    DropScopeAt: Integer;
    NamesAt: Integer;
    ClearNames: Boolean;
    RetireScope: Boolean;
    Target: INyxCancellationScope;
    destructor Destroy; override;
    procedure AdmitTypeWork(APhase: TNyxPascalAnalysisPhase; AUnits: Integer);
    function RemainingTypeBytes: Integer;
  end;

var
  GChecks, GDisposed, GLeases: Integer;
  GCallerScope: INyxDocumentationTypeConstruction;
  GInputNames: TNyxDocumentationGenericNames;
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
  Result := NyxDocumentationType('mechanics.formals', AName, AArity);
end;

function Scope: INyxDocumentationTypeConstruction;
begin
  Result := NewNyxDocumentationTypeConstruction(TNyxDocumentationTypeExpressionLimits.Defaults.Bytes(NyxDocumentationMaximumBytes));
end;

function Names(const AValues: array of TNyxText): TNyxDocumentationGenericNames;
var
  LAt: Integer;
begin
  Result := nil;
  SetLength(Result, Length(AValues));
  for LAt := 0 to High(AValues) do
    Result[LAt] := AValues[LAt];
end;

function ParametersOf(const AValue: IInterface): INyxDocumentationGenericParameters;
var
  LCapability: INyxDocumentationGenericDeclaration;
begin
  Result := nil;
  Check(Supports(AValue, INyxDocumentationGenericDeclaration, LCapability), 'Captured declaration exposes its optional generic metadata');
  Result := LCapability.GenericParameters;
end;

function FormalOf(const AUse: INyxDocumentationTypeUse; AIndex: Integer = 0): INyxDocumentationFormalParameter;
var
  LCapability: INyxDocumentationFormalBindings;
begin
  Result := nil;
  Check(Supports(AUse, INyxDocumentationFormalBindings, LCapability), 'Captured use exposes optional formal slots');
  Result := LCapability.FormalAt(AIndex);
end;

function UseFormal(const AName: TNyxText; const AFormal: INyxDocumentationFormalParameter;
  ARole: TNyxDocumentationTypeOccurrence; const AScope: INyxDocumentationTypeConstruction): INyxDocumentationTypeUse;
begin
  Result := WithNyxDocumentationFormalBinding(NewNyxDocumentationExpressionTypeUse(
    NewNyxDocumentationTypeExpression(AName, ARole, AScope), AScope), 0, AFormal, AScope);
end;

destructor TFormalSource.Destroy;
begin
  Inc(GDisposed);
  inherited Destroy;
end;

procedure TFormalSource.Observe;
begin
  Inc(Reads);

  if (CancelAt > 0) and (Reads = CancelAt) then
    Cancellation.Cancel;
end;

function TFormalSource.Owner: TNyxDocumentationRef;
begin
  Observe;
  Result := Value.Owner;

  if (Mutation = 1) and (Reads > 3) then
    Result := R('Other', 1);
end;

function TFormalSource.ParameterIndex: Integer;
begin
  Observe;
  Result := Value.ParameterIndex;

  if (Mutation = 2) and (Reads > 3) then
    Inc(Result);
end;

function TFormalSource.Name: TNyxText;
begin
  Observe;
  Result := Value.Name;

  if (Mutation = 3) and (Reads > 3) then
    Result := 'Changed';
end;

destructor TParametersSource.Destroy;
begin
  Inc(GDisposed);
  inherited Destroy;
end;

function TParametersSource.Count: Integer;
begin
  Inc(CountReads);

  if (CancelAt > 0) and (CountReads = CancelAt) then
    Cancellation.Cancel;

  if Lie then
    Result := ClaimedCount
  else
    Result := Value.Count;
end;

function TParametersSource.ParameterAt(AIndex: Integer): INyxDocumentationFormalParameter;
begin
  Inc(ItemReads);
  Result := Value.ParameterAt(AIndex);

  if (Item <> nil) and ((ReplaceAt = 0) or (ItemReads >= ReplaceAt)) then
    Result := Item;
end;

function TUseSource.Reference: TNyxDocumentationRef;
begin Result := Value.Reference; end;
function TUseSource.Expression: INyxDocumentationTypeExpression;
begin Result := Value.Expression; end;
function TUseSource.BindingCount: Integer;
begin Result := Value.BindingCount; end;
function TUseSource.BindingAt(AIndex: Integer): TNyxDocumentationRef;
begin
  Result := Value.BindingAt(AIndex);

  if Dual then
    Result := R('Nominal');
end;
function TUseSource.FormalAt(AIndex: Integer): INyxDocumentationFormalParameter;
var
  LCapability: INyxDocumentationFormalBindings;
begin
  Inc(Reads);
  Supports(Value, INyxDocumentationFormalBindings, LCapability);
  Result := LCapability.FormalAt(AIndex);

  if (Formal <> nil) and ((ReplaceAt = 0) or (Reads >= ReplaceAt)) then
    Result := Formal;

  if (DropAt > 0) and (Reads >= DropAt) then
    Result := nil;
end;
function TUseSource.WithBinding(AIndex: Integer; const AReference: TNyxDocumentationRef): INyxDocumentationTypeUse;
begin
  Result := nil;
  raise Exception.Create('Inspecting metadata must not invoke alternate fluent code');
end;

destructor TTypeSource.Destroy;
begin Inc(GDisposed); inherited Destroy; end;
procedure TTypeSource.Observe;
begin

  if (Cancellation <> nil) and Cancellation.Cancelled then
    Inc(AfterCancel);
end;
function TTypeSource.GenericParameters: INyxDocumentationGenericParameters;
begin
  Observe;
  Inc(Reads);
  Result := Parameters;

  if (Mutation = 4) and (Reads = 1) then
    Result := nil;

  if Pulse and (Reads = 2) then
    Result := nil;

  if (CancelAt > 0) and (Reads = CancelAt) then
    Cancellation.Cancel;
end;
function TTypeSource.Identity: TNyxDocumentationRef;
begin
  Observe; Result := Value.Identity;

  if (Mutation = 1) and (Reads > 0) then Result := R('Changed', 1);
end;
function TTypeSource.Kind: TNyxDocumentationTypeKind;
begin
  Observe; Result := Value.Kind;

  if (Mutation = 2) and (Reads > 0) then Result := ndtRecord;
end;
function TTypeSource.Summary: TNyxText;
begin
  Observe; Result := Value.Summary;

  if (Mutation = 3) and (Reads > 0) then Result := 'Changed.';
end;
function TTypeSource.Evidence: TNyxDocumentationEvidence;
begin Observe; Result := Value.Evidence; end;
function TTypeSource.AliasTarget: TNyxDocumentationRef;
begin Observe; Result := Value.AliasTarget; end;
function TTypeSource.BaseCount: Integer;
begin Observe; Result := Value.BaseCount; end;
function TTypeSource.BaseAt(AIndex: Integer): TNyxDocumentationRef;
begin Observe; Result := Value.BaseAt(AIndex); end;
function TTypeSource.MemberCount: Integer;
begin Observe; Result := Value.MemberCount; end;
function TTypeSource.MemberAt(AIndex: Integer): INyxDocumentationMember;
begin Observe; Inc(MemberReads); Result := Value.MemberAt(AIndex); end;
function TTypeSource.ExampleCount: Integer;
begin Observe; Result := Value.ExampleCount; end;
function TTypeSource.ExampleAt(AIndex: Integer): TNyxDocumentationExample;
begin Observe; Result := Value.ExampleAt(AIndex); end;
function TTypeSource.RelatedCount: Integer;
begin Observe; Result := Value.RelatedCount; end;
function TTypeSource.RelatedAt(AIndex: Integer): TNyxDocumentationRef;
begin Observe; Result := Value.RelatedAt(AIndex); end;
function TTypeSource.WithAlias(const ATarget: TNyxDocumentationRef): INyxDocumentationType;
begin Result := nil; raise Exception.Create('No alternate fluent execution'); end;
function TTypeSource.WithBase(const ABase: TNyxDocumentationRef): INyxDocumentationType;
begin Result := nil; raise Exception.Create('No alternate fluent execution'); end;
function TTypeSource.WithMember(const AMember: INyxDocumentationMember): INyxDocumentationType;
begin Result := nil; raise Exception.Create('No alternate fluent execution'); end;
function TTypeSource.WithExample(const AValue: TNyxDocumentationExample): INyxDocumentationType;
begin Result := nil; raise Exception.Create('No alternate fluent execution'); end;
function TTypeSource.WithRelated(const AValue: TNyxDocumentationRef): INyxDocumentationType;
begin Result := nil; raise Exception.Create('No alternate fluent execution'); end;

destructor TAdmission.Destroy;
begin Inc(GLeases); inherited Destroy; end;
procedure TAdmission.AdmitTypeWork(APhase: TNyxPascalAnalysisPhase; AUnits: Integer);
begin

  if (Ord(APhase) < Ord(Low(TNyxPascalAnalysisPhase))) or
    (Ord(APhase) > Ord(High(TNyxPascalAnalysisPhase))) or (AUnits <= 0) then
    raise Exception.Create('Outward work must be a closed positive request');
  Inc(Calls);

  if (DropScopeAt > 0) and (Calls = DropScopeAt) then
  begin

    if RetireScope then
      GCallerScope.Retire;
    GCallerScope := nil;
  end;

  if (CancelAt > 0) and (Calls = CancelAt) then
    Target.Cancel;

  if (NamesAt > 0) and (Calls = NamesAt) then
  begin

    if ClearNames then GInputNames := nil
    else GInputNames[0] := 'Changed';
  end;
end;
function TAdmission.RemainingTypeBytes: Integer;
begin Result := Remaining; end;

procedure Mechanics;
var
  LScope: INyxDocumentationTypeConstruction;
  LParameters, LOwn: INyxDocumentationGenericParameters;
  LType, LCopy: INyxDocumentationType;
  LMember: INyxDocumentationMember;
  LUse, LOther: INyxDocumentationTypeUse;
  LCleared: INyxDocumentationTypeUse;
  LComparison: INyxDocumentationTypeConstruction;
  LArguments: TNyxDocumentationParameterDeclarations;
  LSignature: INyxDocumentationRichSignature;
  LWork: Integer;
  LIdentity: TNyxDocumentationRef;
  LRefused: Boolean;
  LAt: Integer;
begin
  LScope := Scope;
  try
    LParameters := NewNyxDocumentationGenericParameters(R('C', 1), Names(['T']), LScope);
    Check((LParameters.Count = 1) and LParameters.ParameterAt(0).Owner.Same(R('C', 1)) and
      (LParameters.ParameterAt(0).ParameterIndex = 0) and (LParameters.ParameterAt(0).Name = 'T'), 'Formal is exact declaring owner plus own ordinal and name');
    LType := NewNyxDocumentationType(R('C', 1), ndtClass, 'Class.');
    Check(ParametersOf(LType) = nil, 'Old generic arity alone supplies no parameter metadata');
    LType := WithNyxDocumentationGenericParameters(LType, LParameters, LScope);
    LCopy := LType.WithExample(NyxDocumentationExample('Example', 'C := nil;')).WithRelated(R('C', 1));
    Check(ParametersOf(LCopy).ParameterAt(0).Owner.Same(R('C', 1)), 'Type fluent copying retains exact generic scope');
    LUse := UseFormal('T', LParameters.ParameterAt(0), ndtoParameter, LScope);
    Check(not LUse.BindingAt(0).Defined and (FormalOf(LUse).Name = 'T'), 'Legacy nominal slot remains undefined for a known formal');
    LOther := LUse.WithBinding(0, R('Nominal'));
    Check(LOther.BindingAt(0).Same(R('Nominal')) and (FormalOf(LOther) = nil) and
      (FormalOf(LUse) <> nil), 'Nominal replacement clears its formal without mutating the prior value');
    LCleared := WithNyxDocumentationFormalBinding(LUse, 0, nil, LScope);
    Check(not LCleared.BindingAt(0).Defined and (FormalOf(LCleared) = nil) and
      (FormalOf(LUse) <> nil), 'Formal clear restores unknown nominal occurrence without revoking the prior formal');
    SetLength(LArguments, 1);
    LArguments[0] := NewNyxDocumentationParameterDeclaration('A', LCleared);
    LComparison := Scope;
    try
      LSignature := NewNyxDocumentationRichSignature(LArguments, nil, LComparison);
      LWork := LComparison.Receipt.Total;
    finally LComparison.Retire; end;
    LArguments[0] := NewNyxDocumentationParameterDeclaration('A',
      NewNyxDocumentationExpressionTypeUse(LUse.Expression, LScope));
    LComparison := Scope;
    try
      LSignature := NewNyxDocumentationRichSignature(LArguments, nil, LComparison);
      Check((LComparison.Receipt.Total = LWork) and (LSignature.ParameterCount = 1),
        'All-nil clear uses exactly the old absent-map capture work');
    finally LComparison.Retire; end;
    LIdentity := NyxDocumentationMember(R('C', 1), 'Mix', 'OwnT');
    LOwn := NewNyxDocumentationGenericParameters(LIdentity, Names(['T', 'U']), LScope);
    LMember := WithNyxDocumentationGenericParameters(NewNyxDocumentationMember(LIdentity, ndmMethod, 'Mix.'), LOwn, LScope);
    Check((LMember.Identity.GenericArity = 1) and (ParametersOf(LMember).Count = 2) and
      ParametersOf(LMember).ParameterAt(0).Owner.Same(LIdentity), 'Routine-own arity is independent of containing-type arity');
    LMember := LMember.WithExample(NyxDocumentationExample('No execution', 'Mix<T,U>;'));
    Check(ParametersOf(LMember).Count = 2, 'Member fluent copying retains own scope');
    Check(ParametersOf(WithNyxDocumentationGenericParameters(LMember, nil, LScope)) = nil, 'Member nil attachment clears to unknown');
    LCopy := WithNyxDocumentationGenericParameters(LType, nil, LScope);
    Check(ParametersOf(LCopy) = nil, 'Type clear preserves old unknown meaning');
    for LAt := 0 to 4 do
    begin
      LRefused := False;
      try
        case LAt of
          0: LOwn := NewNyxDocumentationGenericParameters(R('C', 1), Names([]), LScope);
          1: LOwn := NewNyxDocumentationGenericParameters(R('Pair', 2), Names(['T', 't']), LScope);
          2: LOwn := NewNyxDocumentationGenericParameters(NyxDocumentationUnit('mechanics.formals'), Names(['T']), LScope);
          3: LOther := UseFormal('Other', LParameters.ParameterAt(0), ndtoParameter, LScope);
          4: LOther := UseFormal('T.Member', LParameters.ParameterAt(0), ndtoParameter, LScope);
        end;
      except on ENyxDocumentation do LRefused := True; end;
      Check(LRefused, 'Wrong arity, duplicate name, unit owner or non-bare slot refuses');
    end;
  finally LScope.Retire; end;
  Check((LParameters.ParameterAt(0).Name = 'T') and (FormalOf(LUse) <> nil), 'Retiring construction leaves detached formal results readable');
end;

procedure SpecializedCopies;
var
  LScope: INyxDocumentationTypeConstruction;
  LType, LTypeCopy: INyxDocumentationType;
  LMember, LCopy: INyxDocumentationMember;
  LUse: INyxDocumentationTypeUse;
  LParameters, LEmpty: INyxDocumentationGenericParameters;
  LArguments: TNyxDocumentationParameterDeclarations;
  LProperty: INyxDocumentationProperty;
  LRichProperty: INyxDocumentationRichProperty;
  LRich: INyxDocumentationMemberSignature;
  LDeclaration: INyxDocumentationMemberDeclaration;
  LDefinition: INyxDocumentationDeclaredTypeExpression;
  LIdentity: TNyxDocumentationRef;
begin
  LScope := Scope;
  try
    LIdentity := NyxDocumentationMember(R('C', 1), 'Item');
    LEmpty := NewNyxDocumentationGenericParameters(LIdentity, Names([]), LScope);
    LMember := NewNyxDocumentationProperty(LIdentity, 'Item.',
      [NyxDocumentationParameter('Index', R('Index'))], [ndpaRead], True).OfType(R('Value'));
    LMember := WithNyxDocumentationGenericParameters(LMember, LEmpty, LScope);
    LMember := WithNyxDocumentationMemberIntent(LMember,
      NewNyxDocumentationMemberIntent(False, False, psavPublic, prcDefault, LScope), LScope);
    LCopy := CaptureNyxDocumentationMember(LMember.WithRelated(R('Value')), LScope);
    Check(Supports(LCopy, INyxDocumentationProperty, LProperty) and
      Supports(LCopy, INyxDocumentationMemberDeclaration, LDeclaration) and
      (LProperty.ParameterCount = 1) and LProperty.DefaultProperty and
      (LDeclaration.DeclarationIntent.Visibility = psavPublic) and
      (ParametersOf(LCopy).Count = 0), 'Simple property and authored intent retain a known-empty own scope');
    LIdentity := NyxDocumentationMember(R('C', 1), 'Choose', 'OwnU');
    LParameters := NewNyxDocumentationGenericParameters(LIdentity, Names(['U']), LScope);
    LUse := UseFormal('U', LParameters.ParameterAt(0), ndtoResult, LScope);
    LMember := NewNyxDocumentationRichMember(LIdentity, ndmMethod, 'Choose.',
      NewNyxDocumentationRichSignature(nil, LUse, LScope), LScope);
    LCopy := WithNyxDocumentationGenericParameters(LMember, LParameters, LScope)
      .WithExample(NyxDocumentationExample('Inert', 'Choose<U>;'));
    LCopy := CaptureNyxDocumentationMember(LCopy, LScope);
    Check(Supports(LCopy, INyxDocumentationMemberSignature, LRich) and
      (LCopy.Signature = nil) and (ParametersOf(LCopy).Count = 1) and
      FormalOf(LRich.DeclaredSignature.ResultUse).Owner.Same(LIdentity),
      'Rich callable copying retains formal scope without an invalid legacy signature');
    LIdentity := NyxDocumentationMember(R('C', 1), 'Lookup');
    LEmpty := NewNyxDocumentationGenericParameters(LIdentity, Names([]), LScope);
    SetLength(LArguments, 1);
    LArguments[0] := NewNyxDocumentationParameterDeclaration('Keys',
      NewNyxDocumentationExpressionTypeUse(NewNyxDocumentationTypeExpression(
        'array of Key', ndtoPropertyIndex, LScope), LScope));
    LMember := NewNyxDocumentationRichProperty(LIdentity, 'Lookup.', LArguments,
      NewNyxDocumentationNamedTypeUse(R('Value')), [ndpaRead], False, LScope);
    LCopy := WithNyxDocumentationGenericParameters(LMember, LEmpty, LScope).OfType(R('OtherValue'));
    LCopy := CaptureNyxDocumentationMember(LCopy, LScope);
    Check(Supports(LCopy, INyxDocumentationRichProperty, LRichProperty) and
      (LCopy.Signature = nil) and (ParametersOf(LCopy).Count = 0) and
      (LRichProperty.ParameterAt(0).TypeUse.Expression.Text = 'array of Key') and
      LRichProperty.ValueUse.Reference.Same(R('OtherValue')),
      'Rich property value replacement preserves its indices and known own scope');
    LParameters := NewNyxDocumentationGenericParameters(R('Vector', 1), Names(['T']), LScope);
    LUse := WithNyxDocumentationFormalBinding(NewNyxDocumentationExpressionTypeUse(
      NewNyxDocumentationTypeExpression('array of T', ndtoDeclaration, LScope), LScope),
      0, LParameters.ParameterAt(0), LScope);
    LType := NewNyxDocumentationDeclaredType(R('Vector', 1), 'Array vector.', LUse, LScope);
    LType := WithNyxDocumentationGenericParameters(LType, LParameters, LScope);
    LTypeCopy := CaptureNyxDocumentationType(LType.WithRelated(R('C', 1)), LScope);
    Check(Supports(LTypeCopy, INyxDocumentationDeclaredTypeExpression, LDefinition) and
      (LDefinition.Family = ptxArray) and (ParametersOf(LTypeCopy).Count = 1) and
      FormalOf(LDefinition.Definition).Owner.Same(R('Vector', 1)),
      'Declared expression copying preserves its exact formal declaration owner');
  finally LScope.Retire; end;
end;

procedure Alternates;
var
  LScope, LPreparation: INyxDocumentationTypeConstruction;
  LParameters: INyxDocumentationGenericParameters;
  LVector: TParametersSource;
  LFormal: TFormalSource;
  LSource: TTypeSource;
  LLease, LResult, LFirst: INyxDocumentationType;
  LAt: Integer;
  LRefused: Boolean;
begin
  LPreparation := Scope;
  try
    LParameters := NewNyxDocumentationGenericParameters(R('C', 1), Names(['T']), LPreparation);
    LFirst := WithNyxDocumentationGenericParameters(NewNyxDocumentationType(R('C', 1), ndtClass, 'Class.'), LParameters, LPreparation);
  finally LPreparation.Retire; end;
  for LAt := 0 to 12 do
  begin
    LScope := Scope;
    LVector := TParametersSource.Create;
    LVector.Value := LParameters;
    LSource := TTypeSource.Create;
    LLease := LSource;
    LSource.Value := NewNyxDocumentationType(R('C', 1), ndtClass, 'Class.');
    LSource.Parameters := LVector;
    LSource.Cancellation := LScope;
    case LAt of
      1: begin LVector.Lie := True; LVector.ClaimedCount := 33; end;
      2: begin
        LFormal := TFormalSource.Create; LFormal.Value := LParameters.ParameterAt(0);
        LVector.Item := LFormal; LVector.ReplaceAt := 2;
      end;
      3: begin LVector.CancelAt := 2; LVector.Cancellation := LScope; end;
      4, 5, 6: begin
        LFormal := TFormalSource.Create; LFormal.Value := LParameters.ParameterAt(0);
        LFormal.Mutation := LAt - 3; LVector.Item := LFormal;
      end;
      7: LSource.Pulse := True;
      8, 9, 10: LSource.Mutation := LAt - 7;
      11: LSource.CancelAt := 1;
      12: LSource.Mutation := 4;
    end;
    LResult := nil;
    LRefused := False;
    try
      try LResult := CaptureNyxDocumentationType(LLease, LScope);
      except on ENyxDocumentation do LRefused := True; end;

      if LAt = 0 then
        Check(not LRefused and (ParametersOf(LResult).Count = 1), 'Stable alternate scope captures without executing fluent code')
      else
        Check(LRefused and (LResult = nil), 'Lying, late scalar/topology/presence or envelope changes refuse atomically');

      if LAt = 1 then
        Check(LVector.ItemReads = 0, 'An oversized vector refuses before any parameter getter');

      if LAt = 11 then
        Check(LSource.AfterCancel = 0, 'A cancelling generic getter prevents all following source reads');
    finally
      LSource.Cancellation := nil;
      LVector.Cancellation := nil;
      LScope.Retire;
      LLease := nil;
    end;
    Check(ParametersOf(LFirst).ParameterAt(0).Name = 'T', 'Alternate refusal leaves an independent prior value intact');
  end;
end;

procedure AlternateUses;
var
  LPreparation, LScope: INyxDocumentationTypeConstruction;
  LParameters, LOther: INyxDocumentationGenericParameters;
  LUse, LUnbound, LLease: INyxDocumentationTypeUse;
  LSource: TUseSource;
  LFormal: TFormalSource;
  LResult: INyxDocumentationRichSignature;
  LAt: Integer;
  LRefused: Boolean;
begin
  LPreparation := Scope;
  try
    LParameters := NewNyxDocumentationGenericParameters(R('C', 1), Names(['T']), LPreparation);
    LOther := NewNyxDocumentationGenericParameters(R('Other', 1), Names(['T']), LPreparation);
    LUse := UseFormal('T', LParameters.ParameterAt(0), ndtoResult, LPreparation);
    LUnbound := NewNyxDocumentationExpressionTypeUse(LUse.Expression, LPreparation);
  finally LPreparation.Retire; end;
  for LAt := 0 to 6 do
  begin
    LScope := Scope;
    LSource := TUseSource.Create;
    LLease := LSource;
    LSource.Value := LUse;
    case LAt of
      1: LSource.Dual := True;
      2: begin LSource.Formal := LOther.ParameterAt(0); LSource.ReplaceAt := 2; end;
      3, 4: begin
        LFormal := TFormalSource.Create;
        LFormal.Value := LParameters.ParameterAt(0);
        LSource.Formal := LFormal;

        if LAt = 3 then LFormal.Mutation := 3
        else begin LFormal.CancelAt := 3; LFormal.Cancellation := LScope; end;
      end;
      5: begin LSource.Value := LUnbound; LSource.Formal := LParameters.ParameterAt(0); LSource.ReplaceAt := 2; end;
      6: LSource.DropAt := 2;
    end;
    LResult := nil;
    LRefused := False;
    try
      try LResult := NewNyxDocumentationRichSignature(nil, LLease, LScope);
      except on ENyxDocumentation do LRefused := True; end;

      if LAt = 0 then
        Check(not LRefused and (FormalOf(LResult.ResultUse) <> nil), 'Alternate formal map is frozen in a rich result')
      else
        Check(LRefused and (LResult = nil), 'Dual, changed, cancelling or nil-transition formal maps refuse atomically');

      if LAt = 4 then
        Check(LFormal.Reads = 3, 'Cancelling formal name getter prevents all later scalar reads');
    finally

      if LAt = 4 then LFormal.Cancellation := nil;
      LScope.Retire;
      LLease := nil;
    end;
  end;
end;

{ Finish the constructor's hidden function-result temporaries before testing
  release of the caller's sole construction slot from an outward callback. }
procedure CreateCallerScope(const AAdmission: INyxDocumentationTypeAdmission);
begin
  GCallerScope := NewNyxDocumentationTypeConstruction(TNyxDocumentationTypeExpressionLimits.Defaults,
    nil, AAdmission);
end;

procedure CallerSlotCases;
var
  LPreparation: INyxDocumentationTypeConstruction;
  LParameters, LResultParameters: INyxDocumentationGenericParameters;
  LUse, LResultUse: INyxDocumentationTypeUse;
  LAdmission: TAdmission;
  LLease: INyxDocumentationTypeAdmission;
  LAt: Integer;
  LRefused: Boolean;
begin
  LPreparation := Scope;
  try
    LParameters := NewNyxDocumentationGenericParameters(R('C', 1), Names(['T']), LPreparation);
    LUse := NewNyxDocumentationExpressionTypeUse(NewNyxDocumentationTypeExpression('T', ndtoResult, LPreparation), LPreparation);
  finally LPreparation.Retire; end;
  for LAt := 0 to 3 do
  begin
    LAdmission := TAdmission.Create;
    LLease := LAdmission;
    LAdmission.Remaining := NyxDocumentationMaximumBytes;
    LAdmission.DropScopeAt := 1;
    LAdmission.RetireScope := Odd(LAt);
    CreateCallerScope(LLease);
    LRefused := False;
    LResultUse := nil;
    LResultParameters := nil;
    try
      try

        if LAt < 2 then
          LResultParameters := NewNyxDocumentationGenericParameters(R('C', 1), Names(['T']), GCallerScope)
        else
          LResultUse := WithNyxDocumentationFormalBinding(LUse, 0, LParameters.ParameterAt(0), GCallerScope);
      except on ENyxDocumentationCancelled do LRefused := True; end;

      if Odd(LAt) then
        Check(LRefused and (LResultParameters = nil) and (LResultUse = nil) and (GCallerScope = nil),
          'Retire-and-clear revokes publication without destroying the active private scope lease')
      else
        Check(not LRefused and (GCallerScope = nil) and ((LResultParameters <> nil) or (LResultUse <> nil)),
          'Clearing the caller slot leaves a local construction lease through publication');
    finally

      if GCallerScope <> nil then GCallerScope.Retire;
      GCallerScope := nil;
      LLease := nil;
    end;
  end;
end;

procedure InputNameCases;
var
  LScope: INyxDocumentationTypeConstruction;
  LResult, LPrior: INyxDocumentationGenericParameters;
  LAdmission: TAdmission;
  LAt, LLast: Integer;
  LRefused: Boolean;
begin
  GInputNames := Names(['T']);
  LAdmission := TAdmission.Create;
  LAdmission.Remaining := NyxDocumentationMaximumBytes;
  LScope := NewNyxDocumentationTypeConstruction(TNyxDocumentationTypeExpressionLimits.Defaults, nil, LAdmission);
  try
    LPrior := NewNyxDocumentationGenericParameters(R('C', 1), GInputNames, LScope);
    LLast := LAdmission.Calls;
  finally LScope.Retire; end;
  for LAt := 0 to 1 do
  begin
    GInputNames := Names(['T']);
    LAdmission := TAdmission.Create;
    LAdmission.Remaining := NyxDocumentationMaximumBytes;
    LAdmission.ClearNames := LAt = 0;

    if LAt = 0 then LAdmission.NamesAt := 1
    else LAdmission.NamesAt := LLast;
    LScope := NewNyxDocumentationTypeConstruction(TNyxDocumentationTypeExpressionLimits.Defaults, nil, LAdmission);
    LResult := nil;
    LRefused := False;
    try
      try LResult := NewNyxDocumentationGenericParameters(R('C', 1), GInputNames, LScope);
      except on ENyxDocumentation do LRefused := True; end;

      if LAt = 0 then
        Check(not LRefused and (LResult <> nil) and (Length(GInputNames) = 0) and
          (LResult.ParameterAt(0).Name = 'T') and (LPrior.ParameterAt(0).Name = 'T'),
          'Name input case 0: clearing the caller handle preserves the passed value and its retained backing')
      else
        Check(LRefused and (LResult = nil) and (LAdmission.Calls = LLast) and
          (GInputNames[0] = 'Changed') and (LPrior.ParameterAt(0).Name = 'T'),
          'Name input case 1: shared element mutation at final admission refuses without revoking the prior value');
    finally LScope.Retire; end;
  end;
  GInputNames := nil;
  LScope := Scope;
  LScope.Cancel;
  LResult := nil;
  LRefused := False;
  try
    try LResult := NewNyxDocumentationGenericParameters(R('C', 1), Names(['T']), LScope);
    except on ENyxDocumentationCancelled do LRefused := True; end;
    Check(LRefused and (LResult = nil), 'Pre-cancelled new declaration capture preserves the core cancellation exception family');
  finally LScope.Retire; end;
end;

function Retired: INyxDocumentationType;
var
  LScope, LPreparation: INyxDocumentationTypeConstruction;
  LParameters: INyxDocumentationGenericParameters;
  LSource: TTypeSource;
  LLease: INyxDocumentationType;
  LAdmission: TAdmission;
begin
  LPreparation := Scope;
  try LParameters := NewNyxDocumentationGenericParameters(R('C', 1), Names(['T']), LPreparation);
  finally LPreparation.Retire; end;
  LAdmission := TAdmission.Create;
  LAdmission.Remaining := NyxDocumentationMaximumBytes;
  LScope := NewNyxDocumentationTypeConstruction(TNyxDocumentationTypeExpressionLimits.Defaults,
    nil, LAdmission);
  LSource := TTypeSource.Create;
  LLease := LSource;
  LSource.Value := NewNyxDocumentationType(R('C', 1), ndtClass, 'Class.');
  LSource.Parameters := LParameters;
  try Result := CaptureNyxDocumentationType(LLease, LScope);
  finally LScope.Retire; end;
end;

procedure AdmissionCases;
var
  LPreparation, LScope: INyxDocumentationTypeConstruction;
  LParameters: INyxDocumentationGenericParameters;
  LType, LResult, LFirst, LLease: INyxDocumentationType;
  LAdmission: TAdmission;
  LVector: TParametersSource;
  LSource: TTypeSource;
  LCalls, LBefore, LLeases: Integer;
  LAt: Integer;
  LUse: INyxDocumentationTypeUse;
  LRefused: Boolean;
begin
  LPreparation := Scope;
  try
    LParameters := NewNyxDocumentationGenericParameters(R('C', 1), Names(['T']), LPreparation);
    LType := WithNyxDocumentationGenericParameters(NewNyxDocumentationType(R('C', 1), ndtClass, 'Class.'), LParameters, LPreparation);
    LUse := UseFormal('T', LParameters.ParameterAt(0), ndtoResult, LPreparation);
    for LAt := 1 to 3 do
      LType := LType.WithMember(NewNyxDocumentationRichMember(NyxDocumentationMember(R('C', 1),
        'Read' + IntToStr(LAt)), ndmMethod, 'Read.', NewNyxDocumentationRichSignature(nil, LUse, LPreparation), LPreparation));
  finally LPreparation.Retire; end;
  LVector := TParametersSource.Create;
  LVector.Value := LParameters;
  LSource := TTypeSource.Create;
  LLease := LSource;
  LSource.Value := NewNyxDocumentationType(R('C', 1), ndtClass, 'Class.');
  LSource.Parameters := LVector;
  LScope := NewNyxDocumentationTypeConstruction(TNyxDocumentationTypeExpressionLimits.Defaults.Work(1));
  LRefused := False;
  LResult := nil;
  try
    try LResult := CaptureNyxDocumentationType(LLease, LScope);
    except on ENyxDocumentationTypeExpression do LRefused := True; end;
    Check(LRefused and (LResult = nil) and (LVector.ItemReads = 0), 'Shared small work refuses before copying a parameter row');
  finally LScope.Retire; end;
  LScope := NewNyxDocumentationTypeConstruction(TNyxDocumentationTypeExpressionLimits.Defaults.Bytes(8));
  LRefused := False;
  LResult := nil;
  try
    try LResult := CaptureNyxDocumentationType(LLease, LScope);
    except on ENyxDocumentationTypeExpression do LRefused := True; end;
    Check(LRefused and (LResult = nil) and (LVector.ItemReads <= 1), 'Shared byte refusal admits no later parameter or partial scope');
  finally LScope.Retire; end;
  LSource.Value := LType;
  LSource.MemberReads := 0;
  LScope := NewNyxDocumentationTypeConstruction(TNyxDocumentationTypeExpressionLimits.Defaults.Bytes(64));
  LRefused := False;
  LResult := nil;
  try
    try LResult := CaptureNyxDocumentationType(LLease, LScope);
    except on ENyxDocumentationTypeExpression do LRefused := True; end;
    Check(LRefused and (LResult = nil) and (LSource.MemberReads < 3),
      'One shared full-type allowance refuses before a later rich member without a fresh member budget');
  finally LScope.Retire; end;
  LType := NewNyxDocumentationType(R('C', 1), ndtClass, 'Class.');
  LPreparation := Scope;
  try LType := WithNyxDocumentationGenericParameters(LType, LParameters, LPreparation);
  finally LPreparation.Retire; end;
  LAdmission := TAdmission.Create;
  LAdmission.Remaining := NyxDocumentationMaximumBytes;
  LScope := NewNyxDocumentationTypeConstruction(TNyxDocumentationTypeExpressionLimits.Defaults, nil, LAdmission);
  try
    LFirst := CaptureNyxDocumentationType(LType, LScope);
    LCalls := LAdmission.Calls;
  finally LScope.Retire; end;
  LAdmission := TAdmission.Create;
  LAdmission.Remaining := NyxDocumentationMaximumBytes;
  LAdmission.CancelAt := LCalls;
  LScope := NewNyxDocumentationTypeConstruction(TNyxDocumentationTypeExpressionLimits.Defaults, nil, LAdmission);
  LAdmission.Target := LScope;
  LRefused := False;
  LResult := nil;
  try
    try LResult := CaptureNyxDocumentationType(LType, LScope);
    except on ENyxDocumentationCancelled do LRefused := True; end;
    Check(LRefused and (LResult = nil) and (LAdmission.Calls = LCalls) and
      (ParametersOf(LFirst).Count = 1), 'Last outward admission revokes publication while the prior value remains readable');
  finally LAdmission.Target := nil; LScope.Retire; end;
  LBefore := GDisposed;
  LLeases := GLeases;
  LResult := Retired;
  Check((GDisposed = LBefore + 1) and (GLeases = LLeases + 1) and
    (ParametersOf(LResult).Count = 1), 'Published descriptors retain neither alternate source nor outward admission');
end;

function Bundle(ACase: Integer): INyxDocumentationRegistration;
var
  LScope: INyxDocumentationTypeConstruction;
  LClass, LInterface, LUnit: INyxDocumentationType;
  LParameters, LOtherParameters, LRoutineParameters: INyxDocumentationGenericParameters;
  LClassFormal: INyxDocumentationFormalParameter;
  LIdentity, LMethodIdentity: TNyxDocumentationRef;
  LUse, LResultUse, LImplementation: INyxDocumentationTypeUse;
  LMember, LMethod: INyxDocumentationMember;
  LDeclarations: TNyxDocumentationDeclarations;
  LMethodParameters: TNyxDocumentationParameterDeclarations;
  LInnerName: TNyxText;
begin
  LScope := Scope;
  try
    LParameters := NewNyxDocumentationGenericParameters(R('C', 1), Names(['T']), LScope);
    LClassFormal := LParameters.ParameterAt(0);
    LIdentity := NyxDocumentationMember(NyxDocumentationUnit('mechanics.formals'), 'Identity', 'GenericT');
    LRoutineParameters := NewNyxDocumentationGenericParameters(LIdentity, Names(['T']), LScope);
    LUse := UseFormal('T', LClassFormal, ndtoResult, LScope);

    if ACase = 1 then
      LUse := UseFormal('T', LRoutineParameters.ParameterAt(0), ndtoResult, LScope);


    if ACase = 5 then
      LUse := NewNyxDocumentationExpressionTypeUse(NewNyxDocumentationTypeExpression('T', ndtoResult, LScope), LScope);
    LClass := NewNyxDocumentationType(R('C', 1), ndtClass, 'Class.');

    if ACase = 3 then
      LParameters := NewNyxDocumentationGenericParameters(R('C', 1), Names(['U']), LScope);

    if ACase <> 2 then
      LClass := WithNyxDocumentationGenericParameters(LClass, LParameters, LScope);
    LMember := NewNyxDocumentationRichMember(NyxDocumentationMember(R('C', 1), 'Read'),
      ndmMethod, 'Read.', NewNyxDocumentationRichSignature(nil, LUse, LScope), LScope);

    if ACase <> 8 then
      LMember := WithNyxDocumentationGenericParameters(LMember,
        NewNyxDocumentationGenericParameters(LMember.Identity, Names([]), LScope), LScope);
    LClass := LClass.WithMember(LMember);
    LMethodIdentity := NyxDocumentationMember(R('C', 1), 'Mix', 'OwnT');
    LInnerName := 'U';

    if ACase = 7 then LInnerName := 't';
    LOtherParameters := NewNyxDocumentationGenericParameters(LMethodIdentity, Names([LInnerName]), LScope);
    LResultUse := UseFormal('T', LClassFormal, ndtoResult, LScope);
    SetLength(LMethodParameters, 1);
    LMethodParameters[0] := NewNyxDocumentationParameterDeclaration('AInner',
      UseFormal(LInnerName, LOtherParameters.ParameterAt(0), ndtoParameter, LScope));
    LMethod := WithNyxDocumentationGenericParameters(NewNyxDocumentationRichMember(LMethodIdentity,
      ndmMethod, 'Mix.', NewNyxDocumentationRichSignature(LMethodParameters, LResultUse, LScope), LScope), LOtherParameters, LScope);
    LClass := LClass.WithMember(LMethod);
    LInterface := WithNyxDocumentationGenericParameters(NewNyxDocumentationType(R('IContract', 1), ndtInterface, 'Interface.'),
      NewNyxDocumentationGenericParameters(R('IContract', 1), Names(['U']), LScope), LScope);
    LImplementation := NewNyxDocumentationExpressionTypeUse(NewNyxDocumentationTypeExpression('IContract<T>', ndtoImplementedInterface, LScope), LScope);
    LImplementation := LImplementation.WithBinding(0, R('IContract', 1));
    LImplementation := WithNyxDocumentationFormalBinding(LImplementation, 1, LClassFormal, LScope);

    if ACase = 6 then
      LImplementation := UseFormal('T', LClassFormal, ndtoImplementedInterface, LScope);
    LClass := WithNyxDocumentationImplementedInterfaces(LClass, NewNyxDocumentationInterfaceImplementations([LImplementation], LScope), LScope);
    LResultUse := UseFormal('T', LRoutineParameters.ParameterAt(0), ndtoResult, LScope);

    if ACase = 4 then
      LResultUse := UseFormal('T', NewNyxDocumentationGenericParameters(NyxDocumentationMember(
        NyxDocumentationUnit('mechanics.formals'), 'Identity', 'OtherOverload'), Names(['T']), LScope).ParameterAt(0), ndtoResult, LScope);
    LMember := WithNyxDocumentationGenericParameters(NewNyxDocumentationRichMember(LIdentity, ndmRoutine,
      'Identity.', NewNyxDocumentationRichSignature(nil, LResultUse, LScope), LScope), LRoutineParameters, LScope);
    LUnit := NewNyxDocumentationType(NyxDocumentationUnit('mechanics.formals'), ndtUnit, 'Synthetic unit.').WithMember(LMember);
    SetLength(LDeclarations, 3);
    LDeclarations[0] := LClass;
    LDeclarations[1] := LInterface;
    LDeclarations[2] := LUnit;
    Result := NewNyxDocumentationRegistration(NyxDocumentationUnit('mechanics.formals'), R('C', 1), LDeclarations,
      [ndplatformNative, ndplatformBrowser]);
  finally LScope.Retire; end;
end;

function Frame(const AText: TNyxText): TNyxText;
begin Result := IntToStr(NyxUTF8ByteCount(AText)) + ':' + AText; end;

procedure IndexCases;
var
  LRegistry: TNyxDocumentationRegistry;
  LCatalog: TNyxCatalog;
  LIndex, LBefore: INyxDocumentationIndex;
  LVersion, LAt: Integer;
  LScope: INyxDocumentationTypeConstruction;
  LDeclarations: TNyxDocumentationDeclarations;
  LRegistration: INyxDocumentationRegistration;
  LExpected: TNyxText;
  LRefused: Boolean;
begin
  LScope := Scope;
  try
    SetLength(LDeclarations, 1);
    LDeclarations[0] := NewNyxDocumentationType(R('Plain'), ndtClass, 'Plain.');
    LRegistration := NewNyxDocumentationRegistration(NyxDocumentationUnit('mechanics.formals'), R('Plain'), LDeclarations);
    LExpected := Frame(R('Plain').Key) + Frame('0') + Frame('Plain.') + Frame('0') + Frame('') +
      Frame('0') + Frame('0') + Frame('0') + Frame('0');
    Check(LRegistration.DeclarationContentAt(0) = LExpected, 'Absent metadata preserves independent legacy framing');
    LDeclarations[0] := WithNyxDocumentationGenericParameters(LDeclarations[0],
      NewNyxDocumentationGenericParameters(R('Plain'), Names([]), LScope), LScope);
    LRegistration := NewNyxDocumentationRegistration(NyxDocumentationUnit('mechanics.formals'), R('Plain'), LDeclarations);
    Check(LRegistration.DeclarationContentAt(0) = LExpected + Frame('generic-parameters') + Frame('0'), 'Known empty parameters have exact distinct framing');
    LDeclarations[0] := WithNyxDocumentationGenericParameters(LDeclarations[0], nil, LScope);
    LRegistration := NewNyxDocumentationRegistration(NyxDocumentationUnit('mechanics.formals'), R('Plain'), LDeclarations);
    Check(LRegistration.DeclarationContentAt(0) = LExpected, 'Clear restores exact old framing');
  finally LScope.Retire; end;
  LRegistry := TNyxDocumentationRegistry.Create;
  LCatalog := TNyxCatalog.Create;
  try
    LCatalog.RegisterKind('formal', 'Formal', 'Custom', False, Bundle(0));
    LIndex := LRegistry.Refresh(CaptureNyxSchemas, CaptureNyxCallbacks, LCatalog.Capture, 0);
    Check((LIndex.DeclarationCount = 3) and (LIndex.Members(R('C', 1)).Count = 2),
      'Class/routine scopes and formal interface arguments close without expanding interface members');
    LBefore := LIndex;
    LVersion := LRegistry.Version;
    for LAt := 1 to 8 do
    begin
      LCatalog.Free;
      LCatalog := TNyxCatalog.Create;
      LCatalog.RegisterKind('invalid', 'Invalid', 'Custom', False, Bundle(LAt));
      LIndex := nil;
      LRefused := False;
      try LIndex := LRegistry.Refresh(CaptureNyxSchemas, CaptureNyxCallbacks, LCatalog.Capture, LVersion);
      except on ENyxDocumentation do LRefused := True; end;
      Check(LRefused and (LIndex = nil) and (LRegistry.Current = LBefore) and
        (LRegistry.Version = LVersion), 'Cross-owner, missing/unknown/shadowed scope, wrong name/overload, unbound or formal root preserves prior index/version');
    end;
    LCatalog.Free;
    LCatalog := TNyxCatalog.Create;
    LCatalog.RegisterKind('first', 'First', 'Custom', False, Bundle(0));
    LCatalog.RegisterKind('conflict', 'Conflict', 'Custom', False, Bundle(3));
    LIndex := nil;
    LRefused := False;
    try LIndex := LRegistry.Refresh(CaptureNyxSchemas, CaptureNyxCallbacks, LCatalog.Capture, LVersion);
    except
      on E: ENyxDocumentation do
        LRefused := Pos('Conflicting documentation declaration:', E.Message) = 1;
    end;
    Check(LRefused and (LIndex = nil) and (LRegistry.Current = LBefore) and
      (LRegistry.Version = LVersion), 'Different formal facts conflict at the exact duplicate declaration before replacing a valid index');
  finally LCatalog.Free; LRegistry.Free; end;
end;

{$ifndef PAS2JS}
function ReadSource(const APath: String): TNyxText;
var
  LStream: TFileStream;
  LBytes: TNyxBytes;
begin
  LStream := TFileStream.Create(APath, fmOpenRead or fmShareDenyWrite);
  try

    if LStream.Size > TNyxPascalLexicalOptions.Defaults.MaximumSourceBytes then
      raise Exception.Create('Actual source exceeds the existing lexical allowance');
    SetLength(LBytes, Integer(LStream.Size));

    if Length(LBytes) <> 0 then
      LStream.ReadBuffer(LBytes[0], Length(LBytes));
  finally LStream.Free; end;
  Result := NyxDecodeUTF8(LBytes);
end;

function FullType(const ASource: INyxPascalSyntaxSnapshot; const AName: TNyxText): Integer;
var
  LAt: Integer;
  LDeclaration: TNyxPascalSyntaxDeclaration;
begin
  Result := -1;
  for LAt := 0 to ASource.DeclarationCount - 1 do
  begin
    LDeclaration := ASource.Declaration(LAt);

    if (LDeclaration.Kind = psdkType) and (LDeclaration.Scope = 0) and
      (LDeclaration.Name = AName) and (LDeclaration.NestedScope >= 0) then
    begin
      Check(Result < 0, 'The actual type has one complete declaring source owner');
      Check((LDeclaration.Flags * [psdfIncomplete, psdfUnsupported, psdfForward] = []) and
        ASource.Scope(LDeclaration.NestedScope).Complete and
        not ASource.Scope(LDeclaration.NestedScope).UnsupportedSyntax,
        'The selected source declaration has a closed supported member scope');
      Result := LAt;
    end;
  end;
  Check(Result >= 0, 'The actual declared type is mandatory');
end;

procedure ActualSources(const ARepository: String);
var
  LSource: INyxPascalSyntaxSnapshot;
  LScope: INyxDocumentationTypeConstruction;
  LParameters, LRoutine: INyxDocumentationGenericParameters;
  LAt, LType, LMember, LGlobal: Integer;
  LIdentity, LGlobalIdentity: TNyxDocumentationRef;
  LDescriptor: INyxDocumentationType;
  LMemberDescriptor: INyxDocumentationMember;
  LUse: INyxDocumentationTypeUse;
  LName: TNyxText;
begin
  LScope := Scope;
  try
    LSource := NewNyxPascalSyntaxSnapshot(ReadSource(IncludeTrailingPathDelimiter(ARepository) + 'src/nyx.linq.aggregates.pas'));
    for LAt := 0 to 1 do
    begin

      if LAt = 0 then LName := 'TNyxKeyValue' else LName := 'INyxKeyValue';
      LType := FullType(LSource, LName);
      Check(((LAt = 0) and (LSource.Declaration(LType).TypeForm = pstfClass)) or
        ((LAt = 1) and (LSource.Declaration(LType).TypeForm = pstfInterface)),
        'Actual class/interface families are checked rather than inferred from their names');
      Check(LSource.Lexical.Slice(LSource.Declaration(LType).GenericSpan) = '<TKey, TValue>',
        'Both actual generic declarations retain their own complete parameter window');
      LIdentity := NyxDocumentationType(LSource.Scope(0).Name, LSource.Declaration(LType).Name, 2);
      LParameters := NewNyxDocumentationGenericParameters(LIdentity, Names(['TKey', 'TValue']), LScope);

      if LAt = 0 then
        LDescriptor := NewNyxDocumentationType(LIdentity, ndtClass, 'Selected actual class scope.')
      else
        LDescriptor := NewNyxDocumentationType(LIdentity, ndtInterface, 'Selected actual interface scope.');
      LDescriptor := WithNyxDocumentationGenericParameters(LDescriptor, LParameters, LScope);
      LMember := LSource.Named(LSource.Declaration(LType).NestedScope, 'GetKey');
      Check((LMember >= 0) and (LSource.Declaration(LMember).Kind = psdkRoutine) and
        (LSource.Declaration(LMember).Flags * [psdfIncomplete, psdfUnsupported] = []) and
        (LSource.Lexical.Slice(LSource.Declaration(LMember).TypeSpan) = 'TKey'),
        'The actual getter uses that owner''s formal parameter');
      LUse := UseFormal('TKey', LParameters.ParameterAt(0), ndtoResult, LScope);
      Check(FormalOf(LUse).Owner.Same(LIdentity), 'Selected class/interface uses preserve distinct original formal owners');
      Inc(GActual);
    end;
    LSource := NewNyxPascalSyntaxSnapshot(ReadSource(IncludeTrailingPathDelimiter(ARepository) + 'tests/signature.contracts.fixture.pas'));
    LType := FullType(LSource, 'TSignatureBox');
    LMember := LSource.Named(LSource.Declaration(LType).NestedScope, 'Replace');
    LGlobal := LSource.Named(0, 'Identity');
    Check((LMember >= 0) and (LGlobal >= 0) and
      (LSource.Declaration(LType).TypeForm = pstfClass) and
      (LSource.Declaration(LMember).Kind = psdkRoutine) and
      (LSource.Declaration(LGlobal).Kind = psdkRoutine) and
      (LSource.Declaration(LMember).Flags * [psdfIncomplete, psdfUnsupported] = []) and
      (LSource.Declaration(LGlobal).Flags * [psdfIncomplete, psdfUnsupported] = []) and
      (LSource.Declaration(LMember).GenericSpan.Length = 0) and
      (LSource.Lexical.Slice(LSource.Declaration(LMember).TypeSpan) = 'T') and
      (LSource.Lexical.Slice(LSource.Declaration(LGlobal).TypeSpan) = 'T') and
      (LSource.Lexical.Slice(LSource.Declaration(LType).GenericSpan) = '<T>') and
      (LSource.Lexical.Slice(LSource.Declaration(LGlobal).GenericSpan) = '<T>') and
      (LSource.Declaration(LGlobal).OwnerName = ''), 'Class-owned Replace T and unit-routine-owned Identity T remain distinct actual syntax');
    LIdentity := NyxDocumentationType(LSource.Scope(0).Name, 'TSignatureBox', 1);
    LParameters := NewNyxDocumentationGenericParameters(LIdentity, Names(['T']), LScope);
    LGlobalIdentity := NyxDocumentationMember(NyxDocumentationUnit(LSource.Scope(0).Name), 'Identity', 'GenericT');
    LRoutine := NewNyxDocumentationGenericParameters(LGlobalIdentity, Names(['T']), LScope);
    LMemberDescriptor := WithNyxDocumentationGenericParameters(NewNyxDocumentationMember(LGlobalIdentity, ndmRoutine,
      'Selected actual global scope.'), LRoutine, LScope);
    Check((LMemberDescriptor.Identity.GenericArity = 0) and
      not LParameters.ParameterAt(0).Owner.Same(LRoutine.ParameterAt(0).Owner),
      'Global generic scope uses its actual unit member, with no containing class or borrowed class arity');
    Inc(GActual);
    Check(GActual = 3, 'All actual generic class/interface and routine source sets are mandatory');
  finally LScope.Retire; end;
end;
{$endif}

{$ifndef PAS2JS}
var
  LRepository: String;
{$endif}
begin
  try
    {$ifndef PAS2JS}

    if ParamCount <> 1 then
      raise Exception.Create('Exactly one valid repository-root argument is required before check zero');
    LRepository := ExpandFileName(ParamStr(1));

    if not DirectoryExists(LRepository) or
      not FileExists(IncludeTrailingPathDelimiter(LRepository) + 'PROJECT.md') or
      not FileExists(IncludeTrailingPathDelimiter(LRepository) + 'src/nyx.linq.aggregates.pas') or
      not FileExists(IncludeTrailingPathDelimiter(LRepository) + 'tests/signature.contracts.fixture.pas') then
      raise Exception.Create('A valid repository root with both actual source owners is required before check zero');
    {$endif}
    Mechanics;
    SpecializedCopies;
    Alternates;
    AlternateUses;
    CallerSlotCases;
    InputNameCases;
    AdmissionCases;
    IndexCases;
    {$ifndef PAS2JS}
    ActualSources(LRepository);
    {$endif}
    {$ifndef PAS2JS}
    WriteLn('PASS ', GChecks, ' scoped formal checks; all', GActual, ' actual source sets');
    {$else}
    WriteLn('PASS ', GChecks, ' portable scoped formal checks');
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' portable scoped formal checks';
    {$endif}
  except
    on E: Exception do
    begin
      WriteLn(E.ClassName, ': ', E.Message, ' at check count ', GChecks);
      {$ifdef PAS2JS}document.body.textContent := 'FAIL ' + E.ClassName + ': ' + E.Message + ' at check count ' + IntToStr(GChecks);{$endif}
      {$ifndef PAS2JS}DumpExceptionBackTrace(Output);{$endif}
      ExitCode := 1;
    end;
  end;
end.
