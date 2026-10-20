{ Authored member intent is immutable metadata, not compiler policy. MIT. }
program nyx_documentation_memberdeclarations_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.bytes, nyx.scheduler, nyx.pascal.lexing,
  nyx.pascal.syntax, nyx.pascal.work, nyx.documentation.typeexpressions,
  nyx.documentation, nyx.documentation.registration, nyx.documentation.registry,
  nyx.catalog, nyx.schema, nyx.callbacks, nyx.test.documentation
  {$ifdef PAS2JS}, Web{$else}, Classes{$endif};

type
  { Scalar supplier changes one observation or cancels on a chosen convention
    read. Inspection must freeze data and never retain this mutable producer. }
  TIntentSupplier = class(TInterfacedObject, INyxDocumentationMemberIntent)
  public
    ClassValue, StaticValue: Boolean;
    VisibilityValue: TNyxPascalSyntaxVisibility;
    ConventionValue: TNyxPascalRoutineConvention;
    Reads, ClassReads, StaticReads, VisibilityReads, ConventionReads: Integer;
    Mutation, ChangeAt, CancelAt: Integer;
    Cancellation: INyxCancellationScope;
    constructor Create;
    destructor Destroy; override;
    function DeclaredClass: Boolean;
    function DeclaredStatic: Boolean;
    function Visibility: TNyxPascalSyntaxVisibility;
    function CallingConvention: TNyxPascalRoutineConvention;
  end;
  { A declared member offers the optional capability without any private owner.
    Fluent calls refuse; envelope and nil/value changes are final-read fixtures. }
  TMemberSupplier = class(TInterfacedObject, INyxDocumentationMember,
    INyxDocumentationMemberDeclaration)
  public
    Value: INyxDocumentationMember;
    IntentValue, ChangedIntent: INyxDocumentationMemberIntent;
    IntentReads, OtherReads, SwapAt, CancelAt, Mutation: Integer;
    KindReads, KindReadsAtCancel: Integer;
    PulseAbsent, CancelFirstSignature, CancelFinalIdentity: Boolean;
    Replacement: INyxDocumentationSignature;
    Cancellation: INyxCancellationScope;
    destructor Destroy; override;
    function DeclarationIntent: INyxDocumentationMemberIntent;
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
  end;
  { Outward leases have observable retirement. Shared fixed-byte/work limits
    reject before later member observations, rather than only at final framing. }
  TAdmissionProbe = class(TInterfacedObject, INyxCancellationScope,
    INyxDocumentationTypeAdmission)
  public
    CancelledValue, LimitWork: Boolean;
    Bytes, WorkRemaining, Calls, CancelAt: Integer;
    Target: INyxCancellationScope;
    destructor Destroy; override;
    procedure Cancel;
    function GetCancelled: Boolean;
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

function Ref(const AName: TNyxText): TNyxDocumentationRef;
begin
  Result := NyxDocumentationType('mechanics.memberintent', AName);
end;

function Plain(const AName: TNyxText = 'M'): INyxDocumentationMember;
begin
  Result := NewNyxDocumentationMember(NyxDocumentationMember(Ref('TExample'), AName), ndmMethod, 'Method.');
end;

function Construction: INyxDocumentationTypeConstruction;
begin
  Result := NewNyxDocumentationTypeConstruction(
    TNyxDocumentationTypeExpressionLimits.Defaults.Bytes(NyxDocumentationMaximumBytes));
end;

function IntentOf(const AValue: INyxDocumentationMember): INyxDocumentationMemberIntent;
var
  LDeclaration: INyxDocumentationMemberDeclaration;
begin
  Result := nil;
  Check(Supports(AValue, INyxDocumentationMemberDeclaration, LDeclaration), 'Captured member exposes optional intent access');
  Result := LDeclaration.DeclarationIntent;
end;

constructor TIntentSupplier.Create;
begin
  inherited Create;
  ClassValue := True;
  VisibilityValue := psavPublic;
  ConventionValue := prcDefault;
  ChangeAt := 2;
end;

destructor TIntentSupplier.Destroy;
begin
  Inc(GDisposed);
  inherited Destroy;
end;

function TIntentSupplier.DeclaredClass: Boolean;
begin
  Inc(Reads);
  Inc(ClassReads);
  Result := ClassValue;

  if (Mutation = 1) and (ClassReads >= ChangeAt) then
    Result := not Result;
end;

function TIntentSupplier.DeclaredStatic: Boolean;
begin
  Inc(Reads);
  Inc(StaticReads);
  Result := StaticValue;

  if (Mutation = 2) and (StaticReads >= ChangeAt) then
    Result := not Result;
end;

function TIntentSupplier.Visibility: TNyxPascalSyntaxVisibility;
begin
  Inc(Reads);
  Inc(VisibilityReads);
  Result := VisibilityValue;

  if (Mutation = 3) and (VisibilityReads >= ChangeAt) then
    Result := psavPrivate;
end;

function TIntentSupplier.CallingConvention: TNyxPascalRoutineConvention;
begin
  Inc(Reads);
  Inc(ConventionReads);

  if (CancelAt > 0) and (ConventionReads >= CancelAt) and (Cancellation <> nil) then
    Cancellation.Cancel;
  Result := ConventionValue;

  if (Mutation = 4) and (ConventionReads >= ChangeAt) then
    Result := prcCdecl;
end;

destructor TMemberSupplier.Destroy;
begin
  Inc(GDisposed);
  inherited Destroy;
end;

function TMemberSupplier.DeclarationIntent: INyxDocumentationMemberIntent;
begin
  Inc(IntentReads);

  if (CancelAt > 0) and (IntentReads >= CancelAt) and (Cancellation <> nil) then
    Cancellation.Cancel;
  Result := IntentValue;

  if (SwapAt > 0) and (IntentReads >= SwapAt) then
    Result := ChangedIntent;

  if PulseAbsent and (IntentReads in [2, 3]) then
    Result := nil;
end;

function TMemberSupplier.Identity: TNyxDocumentationRef;
begin
  Inc(OtherReads);
  Result := Value.Identity;

  if (Mutation = 1) and (IntentReads >= 2) then
    Result := NyxDocumentationMember(Ref('TExample'), 'Other');

  if CancelFinalIdentity and (IntentReads >= 4) and (Cancellation <> nil) then
  begin
    KindReadsAtCancel := KindReads;
    Cancellation.Cancel;
  end;
end;

function TMemberSupplier.Kind: TNyxDocumentationMemberKind;
begin
  Inc(OtherReads);
  Inc(KindReads);
  Result := Value.Kind;

  if (Mutation = 2) and (IntentReads >= 2) then
    Result := ndmProperty;
end;

function TMemberSupplier.Summary: TNyxText;
begin
  Inc(OtherReads);
  Result := Value.Summary;

  if (Mutation = 3) and (IntentReads >= 2) then
    Result := 'Changed.';
end;

function TMemberSupplier.Evidence: TNyxDocumentationEvidence;
begin
  Inc(OtherReads);
  Result := Value.Evidence;

  if (Mutation = 4) and (IntentReads >= 2) then
    Result := ndeSchemaDeclaration;
end;

function TMemberSupplier.Signature: INyxDocumentationSignature;
begin
  Inc(OtherReads);
  Result := Value.Signature;

  if (Mutation = 5) and (IntentReads >= 2) then
    Result := Replacement;

  if CancelFirstSignature and (Cancellation <> nil) then
    Cancellation.Cancel;
end;

function TMemberSupplier.ValueType: TNyxDocumentationRef;
begin
  Inc(OtherReads);
  Result := Value.ValueType;

  if (Mutation = 6) and (IntentReads >= 2) then
    Result := Ref('TItem');
end;

function TMemberSupplier.ExampleCount: Integer;
begin
  Inc(OtherReads);
  Result := Value.ExampleCount;
end;

function TMemberSupplier.ExampleAt(AIndex: Integer): TNyxDocumentationExample;
begin
  Inc(OtherReads);
  Result := Value.ExampleAt(AIndex);
end;

function TMemberSupplier.RelatedCount: Integer;
begin
  Inc(OtherReads);
  Result := Value.RelatedCount;
end;

function TMemberSupplier.RelatedAt(AIndex: Integer): TNyxDocumentationRef;
begin
  Inc(OtherReads);
  Result := Value.RelatedAt(AIndex);
end;

function TMemberSupplier.WithSignature(const AValue: INyxDocumentationSignature): INyxDocumentationMember;
begin
  Result := nil;
  raise Exception.Create('Capture must not invoke alternate fluent signature mutation');
end;

function TMemberSupplier.OfType(const AValue: TNyxDocumentationRef): INyxDocumentationMember;
begin
  Result := nil;
  raise Exception.Create('Capture must not invoke alternate fluent type mutation');
end;

function TMemberSupplier.WithExample(const AValue: TNyxDocumentationExample): INyxDocumentationMember;
begin
  Result := nil;
  raise Exception.Create('Capture must not invoke alternate fluent example mutation');
end;

function TMemberSupplier.WithRelated(const AValue: TNyxDocumentationRef): INyxDocumentationMember;
begin
  Result := nil;
  raise Exception.Create('Capture must not invoke alternate fluent link mutation');
end;

destructor TAdmissionProbe.Destroy;
begin
  Inc(GLeases);
  inherited Destroy;
end;

procedure TAdmissionProbe.Cancel;
begin
  CancelledValue := True;
end;

function TAdmissionProbe.GetCancelled: Boolean;
begin
  Result := CancelledValue;
end;

procedure TAdmissionProbe.AdmitTypeWork(APhase: TNyxPascalAnalysisPhase; AUnits: Integer);
begin

  if (Ord(APhase) < Ord(Low(TNyxPascalAnalysisPhase))) or
    (Ord(APhase) > Ord(High(TNyxPascalAnalysisPhase))) or (AUnits < 1) then
    raise EArgumentException.Create('Known positive intent work is required');

  if LimitWork and (AUnits > WorkRemaining) then
    raise ENyxDocumentationTypeExpression.Create('Shared test work exhausted');

  if LimitWork then
    Dec(WorkRemaining, AUnits);
  Inc(Calls);

  if (CancelAt > 0) and (Calls >= CancelAt) and (Target <> nil) then
    Target.Cancel;
end;

function TAdmissionProbe.RemainingTypeBytes: Integer;
begin
  Result := Bytes;
end;

procedure ShapeCases;
var
  LScope: INyxDocumentationTypeConstruction;
  LIntent, LUnknown: INyxDocumentationMemberIntent;
  LPlain, LValue, LCopy: INyxDocumentationMember;
  LProperty: INyxDocumentationProperty;
  LRich: INyxDocumentationMemberSignature;
  LRichProperty: INyxDocumentationRichProperty;
  LUse: INyxDocumentationTypeUse;
  LParameters: TNyxDocumentationParameterDeclarations;
begin
  LScope := Construction;
  try
    LPlain := Plain;
    Check(IntentOf(LPlain) = nil, 'Legacy member intent remains explicitly unknown');
    LCopy := CaptureNyxDocumentationMember(LPlain, LScope);
    Check((IntentOf(LCopy) = nil) and (LScope.Receipt.Total = 0), 'Legacy capture keeps absent work and intent unchanged');
    LIntent := NewNyxDocumentationMemberIntent(True, True, psavPublic, prcCdecl, LScope);
    LValue := WithNyxDocumentationMemberIntent(LPlain, LIntent, LScope);
    Check(IntentOf(LPlain) = nil, 'Attachment cannot mutate an earlier member');
    LCopy := LValue.WithExample(NyxDocumentationExample('Read', 'ReadValue;')).WithRelated(Ref('TExample'));
    Check(IntentOf(LCopy).DeclaredClass and IntentOf(LCopy).DeclaredStatic and
      (IntentOf(LCopy).Visibility = psavPublic) and (IntentOf(LCopy).CallingConvention = prcCdecl),
      'Fluent prose/link copies retain all authored intent');
    LCopy := LCopy.WithSignature(NewNyxDocumentationSignature([], Default(TNyxDocumentationRef)));
    Check(IntentOf(LCopy).DeclaredStatic and (LCopy.Signature <> nil), 'Legacy signature replacement retains independent intent');
    LCopy := WithNyxDocumentationMemberIntent(LCopy, nil, LScope);
    Check((IntentOf(LCopy) = nil) and (LCopy.ExampleCount = 1) and (LCopy.Signature <> nil), 'Explicit clear returns unknown without discarding other metadata');
    LUnknown := NewNyxDocumentationMemberIntent(False, False, psavUnspecified, prcDefault, LScope);
    Check((LUnknown.Visibility = psavUnspecified) and (LUnknown.CallingConvention = prcDefault),
      'Unspecified visibility and absent convention never become effective public/default ABI facts');
    LUnknown := NewNyxDocumentationMemberIntent(False, True, psavUnspecified, prcDefault, LScope);
    Check(not LUnknown.DeclaredClass and LUnknown.DeclaredStatic,
      'Independent scalar modifiers never infer an authored class keyword from static');
    LIntent := NewNyxDocumentationMemberIntent(False, False, psavPublic, prcDefault, LScope);
    LValue := NewNyxDocumentationProperty(NyxDocumentationMember(Ref('TExample'), 'Items'), 'Indexed.',
      [NyxDocumentationParameter('Index', Ref('TItem'))], [ndpaRead], True).OfType(Ref('TItem'));
    LValue := WithNyxDocumentationMemberIntent(LValue, LIntent, LScope);
    LCopy := LValue.WithExample(NyxDocumentationExample('Read', 'ReadItem;'));
    Check(Supports(LCopy, INyxDocumentationProperty, LProperty) and
      (LProperty.ParameterCount = 1) and LProperty.DefaultProperty and
      (LProperty.DeclaredAccess = [ndpaRead]) and (LCopy.Signature = nil) and
      (IntentOf(LCopy).Visibility = psavPublic), 'Simple property specialization survives intent and prose');
    LUse := NewNyxDocumentationExpressionTypeUse(NewNyxDocumentationTypeExpression(
      'array of TItem', ndtoParameter, LScope), LScope);
    SetLength(LParameters, 1);
    LParameters[0] := NewNyxDocumentationParameterDeclaration('Items', LUse, ndpConst);
    LValue := NewNyxDocumentationRichMember(NyxDocumentationMember(Ref('TExample'), 'Take'), ndmMethod,
      'Take items.', NewNyxDocumentationRichSignature(LParameters, nil, LScope), LScope);
    LValue := WithNyxDocumentationMemberIntent(LValue, LIntent, LScope);
    LCopy := CaptureNyxDocumentationMember(LValue.WithRelated(Ref('TExample')), LScope);
    Check(Supports(LCopy, INyxDocumentationMemberSignature, LRich) and
      (LRich.DeclaredSignature.ParameterAt(0).TypeUse.Expression.Text = 'array of TItem') and
      (LCopy.Signature = nil) and not IntentOf(LCopy).DeclaredClass, 'Rich callable remains rich without a fabricated named signature');
    LUse := NewNyxDocumentationExpressionTypeUse(NewNyxDocumentationTypeExpression(
      'array of TItem', ndtoPropertyIndex, LScope), LScope);
    LParameters[0] := NewNyxDocumentationParameterDeclaration('Items', LUse);
    LValue := NewNyxDocumentationRichProperty(NyxDocumentationMember(Ref('TExample'), 'Lookup'),
      'Complex index.', LParameters, NewNyxDocumentationNamedTypeUse(Ref('TItem')), [ndpaRead], False, LScope);
    LValue := WithNyxDocumentationMemberIntent(LValue, LIntent, LScope).OfType(Ref('TOther'));
    Check(Supports(LValue, INyxDocumentationRichProperty, LRichProperty) and
      (LRichProperty.ParameterAt(0).TypeUse.Expression.Occurrence = ndtoPropertyIndex) and
      LRichProperty.ValueUse.Reference.Same(Ref('TOther')) and (LValue.Signature = nil) and
      (IntentOf(LValue).Visibility = psavPublic), 'Rich property value replacement retains indices and authored intent');
  finally
    LScope.Retire;
  end;
  Check(IntentOf(LValue).Visibility = psavPublic, 'Published intent is usable after construction retirement');
end;

procedure AlternateCases;
var
  LAt: Integer;
  LScope: INyxDocumentationTypeConstruction;
  LSource: TMemberSupplier;
  LLease, LResult: INyxDocumentationMember;
  LIntent: TIntentSupplier;
  LIntentLease: INyxDocumentationMemberIntent;
  LRefused: Boolean;
begin
  for LAt := 0 to 15 do
  begin
    LSource := TMemberSupplier.Create;
    LSource.Value := Plain;
    LSource.Replacement := NewNyxDocumentationSignature([], Default(TNyxDocumentationRef));
    LLease := LSource;
    LIntent := TIntentSupplier.Create;
    LIntentLease := LIntent;
    LSource.IntentValue := LIntentLease;
    LScope := Construction;
    LResult := nil;
    try
      case LAt of
        1..4: LIntent.Mutation := LAt;
        5: begin LSource.IntentValue := nil; LSource.ChangedIntent := LIntentLease; LSource.SwapAt := 2; end;
        6: LSource.SwapAt := 2;
        7..12: LSource.Mutation := LAt - 6;
        13: begin LIntent.Cancellation := LScope; LIntent.CancelAt := 3; end;
        14: begin LSource.Cancellation := LScope; LSource.CancelAt := 2; end;
        15: begin LIntent.Mutation := 1; LIntent.ChangeAt := 3; end;
      end;
      LRefused := False;
      try
        LResult := CaptureNyxDocumentationMember(LLease, LScope);
      except
        on ENyxDocumentation do LRefused := True;
      end;
      Check((LAt = 0) <> LRefused, 'Stable alternate captures; changing intent/scalars/envelope/cancellation refuses ' + IntToStr(LAt));

      if LAt = 0 then
        Check(IntentOf(LResult).DeclaredClass and not IntentOf(LResult).DeclaredStatic,
          'Captured modifiers retain authored values without inferring static')
      else
        Check(LResult = nil, 'Alternate refusal exposes no partial member');
    finally
      LScope.Retire;
    end;
  end;
  LSource := TMemberSupplier.Create;
  LSource.Value := Plain;
  LLease := LSource;
  LScope := Construction;
  try
    LResult := CaptureNyxDocumentationMember(LLease, LScope);
    Check((IntentOf(LResult) = nil) and (LSource.IntentReads = 2), 'Stable nil capability is observed again and stays unknown');
    LScope.Cancel;
    LSource.IntentReads := 0;
    LRefused := False;
    try
      LResult := CaptureNyxDocumentationMember(LLease, LScope);
    except
      on ENyxDocumentationCancelled do LRefused := True;
    end;
    Check(LRefused and (LSource.IntentReads = 0), 'Pre-cancelled core capture preserves its original cancellation class before getters');
  finally
    LScope.Retire;
  end;
end;

procedure AttachmentCases;
var
  LAt: Integer;
  LSource: TMemberSupplier;
  LLease, LResult: INyxDocumentationMember;
  LIntent: TIntentSupplier;
  LIntentLease: INyxDocumentationMemberIntent;
  LScope: INyxDocumentationTypeConstruction;
  LProbe: TAdmissionProbe;
  LProbeLease: INyxDocumentationTypeAdmission;
  LRefused: Boolean;
begin
  for LAt := 0 to 4 do
  begin
    LSource := TMemberSupplier.Create;
    LSource.Value := Plain;
    LLease := LSource;
    LIntent := TIntentSupplier.Create;
    LIntentLease := LIntent;
    LSource.IntentValue := LIntentLease;
    LProbe := TAdmissionProbe.Create;
    LProbe.Bytes := NyxDocumentationMaximumBytes;
    LProbeLease := LProbe;
    LScope := NewNyxDocumentationTypeConstruction(TNyxDocumentationTypeExpressionLimits.Defaults, nil, LProbeLease);
    LProbe.Target := LScope;
    LSource.Cancellation := LScope;
    try
      case LAt of
        0: LProbe.CancelAt := 2;
        1: LProbe.CancelAt := 3;
        2: LSource.CancelFirstSignature := True;
        3: LSource.CancelFinalIdentity := True;
        4: LSource.PulseAbsent := True;
      end;
      LResult := nil;
      LRefused := False;
      try
        LResult := WithNyxDocumentationMemberIntent(LLease, nil, LScope);
      except
        on ENyxDocumentation do LRefused := True;
      end;
      Check(LRefused and (LResult = nil), 'Attachment cancellation or intent presence oscillation refuses atomically ' + IntToStr(LAt));
      case LAt of
        0: Check((LSource.OtherReads = 0) and (LSource.IntentReads = 0), 'Revoked signature admission reads no source fields');
        1, 2: Check((LSource.OtherReads = 1) and (LSource.IntentReads = 0), 'Revocation before first intent observation stops after the signature');
        3: Check((LSource.IntentReads = 4) and (LSource.KindReads = LSource.KindReadsAtCancel), 'Cancelling final identity prevents the following kind getter');
        4: Check(LSource.IntentReads = 4, 'Value/nil/nil/value intent observations refuse before a missing captured scalar owner is used');
      end;
    finally
      LProbe.Target := nil;
      LScope.Retire;
    end;
  end;
end;

function RetiredValue: INyxDocumentationMember;
var
  LSource: TMemberSupplier;
  LLease: INyxDocumentationMember;
  LIntent: TIntentSupplier;
  LIntentLease: INyxDocumentationMemberIntent;
  LProbe: TAdmissionProbe;
  LProbeLease: INyxDocumentationTypeAdmission;
  LCancellation: INyxCancellationScope;
  LScope: INyxDocumentationTypeConstruction;
begin
  LProbe := TAdmissionProbe.Create;
  LProbe.Bytes := NyxDocumentationMaximumBytes;
  LProbeLease := LProbe;
  LCancellation := LProbe;
  LScope := NewNyxDocumentationTypeConstruction(TNyxDocumentationTypeExpressionLimits.Defaults, LCancellation, LProbeLease);
  LSource := TMemberSupplier.Create;
  LSource.Value := Plain;
  LLease := LSource;
  LIntent := TIntentSupplier.Create;
  LIntentLease := LIntent;
  LSource.IntentValue := LIntentLease;
  try
    Result := CaptureNyxDocumentationMember(LLease, LScope);
  finally
    LScope.Retire;
  end;
end;

procedure BoundsCases;
var
  LSource: TMemberSupplier;
  LLease, LResult, LFirst: INyxDocumentationMember;
  LIntent: TIntentSupplier;
  LIntentLease, LFixed: INyxDocumentationMemberIntent;
  LScope: INyxDocumentationTypeConstruction;
  LProbe: TAdmissionProbe;
  LProbeLease: INyxDocumentationTypeAdmission;
  LRefused: Boolean;
  LBefore, LBeforeLeases, LAt, LLastAdmission, LConventionReads: Integer;
  LType, LCapturedType: INyxDocumentationType;
begin
  LSource := TMemberSupplier.Create;
  LSource.Value := Plain;
  LLease := LSource;
  LIntent := TIntentSupplier.Create;
  LIntentLease := LIntent;
  LSource.IntentValue := LIntentLease;
  LScope := NewNyxDocumentationTypeConstruction(TNyxDocumentationTypeExpressionLimits.Defaults.Work(1));
  try
    LResult := nil;
    LRefused := False;
    try LResult := CaptureNyxDocumentationMember(LLease, LScope);
    except on ENyxDocumentationTypeExpression do LRefused := True; end;
    Check(LRefused and (LResult = nil) and (LIntent.Reads = 0) and (LSource.OtherReads = 0),
      'Small shared work refuses before scalar or legacy-envelope copying');
  finally LScope.Retire; end;
  LSource.IntentReads := 0;
  LScope := NewNyxDocumentationTypeConstruction(TNyxDocumentationTypeExpressionLimits.Defaults.Bytes(3));
  try
    LResult := nil;
    LRefused := False;
    try LResult := CaptureNyxDocumentationMember(LLease, LScope);
    except on ENyxDocumentationTypeExpression do LRefused := True; end;
    Check(LRefused and (LResult = nil) and (LIntent.Reads = 4) and (LSource.OtherReads = 0),
      'Four fixed fields cannot copy under three bytes or read later member fields');
  finally LScope.Retire; end;
  LProbe := TAdmissionProbe.Create;
  LProbe.Bytes := NyxDocumentationMaximumBytes;
  LProbeLease := LProbe;
  LScope := NewNyxDocumentationTypeConstruction(TNyxDocumentationTypeExpressionLimits.Defaults, nil, LProbeLease);
  try
    LFirst := CaptureNyxDocumentationMember(LLease, LScope);
    LProbe.LimitWork := True;
    LProbe.WorkRemaining := 0;
    LSource.IntentReads := 0;
    LResult := nil;
    LRefused := False;
    try LResult := CaptureNyxDocumentationMember(LLease, LScope);
    except on ENyxDocumentationTypeExpression do LRefused := True; end;
    Check(LRefused and (LResult = nil) and (LSource.IntentReads = 0) and IntentOf(LFirst).DeclaredClass,
      'Later captures share actual remaining admission and preserve earlier immutable results');
  finally LScope.Retire; end;
  LProbe := TAdmissionProbe.Create;
  LProbe.Bytes := NyxDocumentationMaximumBytes;
  LProbeLease := LProbe;
  LScope := NewNyxDocumentationTypeConstruction(TNyxDocumentationTypeExpressionLimits.Defaults, nil, LProbeLease);
  try
    LFirst := CaptureNyxDocumentationMember(LLease, LScope);
    LLastAdmission := LProbe.Calls;
  finally LScope.Retire; end;
  LProbe := TAdmissionProbe.Create;
  LProbe.Bytes := NyxDocumentationMaximumBytes;
  LProbe.CancelAt := LLastAdmission;
  LProbeLease := LProbe;
  LScope := NewNyxDocumentationTypeConstruction(TNyxDocumentationTypeExpressionLimits.Defaults, nil, LProbeLease);
  LProbe.Target := LScope;
  LConventionReads := LIntent.ConventionReads;
  try
    LResult := nil;
    LRefused := False;
    try LResult := CaptureNyxDocumentationMember(LLease, LScope);
    except on ENyxDocumentationCancelled do LRefused := True; end;
    Check(LRefused and (LResult = nil) and (LProbe.Calls = LLastAdmission) and
      (LIntent.ConventionReads = LConventionReads + 2),
      'Last outward admission revokes publication before the final borrowed scalar getter');
    Check(IntentOf(LFirst).DeclaredClass, 'Cancellation leaves the earlier independent intent readable');
  finally
    LProbe.Target := nil;
    LScope.Retire;
  end;
  LScope := Construction;
  try
    LFixed := NewNyxDocumentationMemberIntent(False, False, psavPublic, prcDefault, LScope);
    LType := NewNyxDocumentationType(Ref('TExample'), ndtClass, 'Class.')
      .WithMember(WithNyxDocumentationMemberIntent(Plain('M'), LFixed, LScope))
      .WithMember(WithNyxDocumentationMemberIntent(Plain('N'), LFixed, LScope));
  finally LScope.Retire; end;
  LScope := NewNyxDocumentationTypeConstruction(TNyxDocumentationTypeExpressionLimits.Defaults.Bytes(4));
  try
    LCapturedType := nil;
    LRefused := False;
    try LCapturedType := CaptureNyxDocumentationType(LType, LScope);
    except on ENyxDocumentationTypeExpression do LRefused := True; end;
    Check(LRefused and (LCapturedType = nil) and (LType.MemberCount = 2),
      'Full-type capture cannot reset the four-field allowance for its next member');
  finally LScope.Retire; end;
  for LAt := 0 to 2 do
  begin
    LScope := Construction;
    try
      LFixed := NewNyxDocumentationMemberIntent(LAt <> 2, LAt <> 2, psavPublic, prcCdecl, LScope);
      case LAt of
        0: LFirst := NewNyxDocumentationMember(NyxDocumentationMember(NyxDocumentationUnit('mechanics.memberintent'), 'Global'), ndmRoutine, 'Global.');
        1: LFirst := NewNyxDocumentationMember(NyxDocumentationMember(Ref('TEnum'), 'Value'), ndmEnumValue, 'Enum value.');
        2: LFirst := NewNyxDocumentationProperty(NyxDocumentationMember(Ref('TExample'), 'Value'), 'Property.', [], [], False);
      end;
      LResult := nil;
      LRefused := False;
      try LResult := WithNyxDocumentationMemberIntent(LFirst, LFixed, LScope);
      except on ENyxDocumentation do LRefused := True; end;
      Check(LRefused and (LResult = nil), 'Global/enum/non-callable role cannot fabricate member intent ' + IntToStr(LAt));
    finally LScope.Retire; end;
  end;
  LBefore := GDisposed;
  LBeforeLeases := GLeases;
  LResult := RetiredValue;
  Check((GDisposed = LBefore + 2) and (GLeases = LBeforeLeases + 1) and IntentOf(LResult).DeclaredClass,
    'Published member owns neither alternate producer nor cancellation/admission lease');
end;

function Frame(const AValue: TNyxText): TNyxText;
begin
  Result := IntToStr(NyxUTF8ByteCount(AValue)) + ':' + AValue;
end;

function Bundle(AKnown, AClass: Boolean): INyxDocumentationRegistration;
var
  LScope: INyxDocumentationTypeConstruction;
  LValue: INyxDocumentationMember;
  LDeclarations: TNyxDocumentationDeclarations;
begin
  LScope := Construction;
  try
    LValue := Plain;

    if AKnown then
      LValue := WithNyxDocumentationMemberIntent(LValue,
        NewNyxDocumentationMemberIntent(AClass, False, psavPublic, prcDefault, LScope), LScope);
    SetLength(LDeclarations, 1);
    LDeclarations[0] := NewNyxDocumentationType(Ref('TExample'), ndtClass, 'Class.').WithMember(LValue);
    Result := NewNyxDocumentationRegistration(NyxDocumentationUnit('mechanics.memberintent'), Ref('TExample'), LDeclarations);
  finally LScope.Retire; end;
end;

procedure IndexCases;
var
  LRegistry: TNyxDocumentationRegistry;
  LCatalog, LBad: TNyxCatalog;
  LIndex, LBefore: INyxDocumentationIndex;
  LRegistration: INyxDocumentationRegistration;
  LExpected: TNyxText;
  LVersion: Integer;
  LRefused: Boolean;
  LScope: INyxDocumentationTypeConstruction;
  LDeclarations: TNyxDocumentationDeclarations;
begin
  LRegistration := Bundle(False, False);
  LExpected := Frame(Ref('TExample').Key) + Frame('0') + Frame('Class.') + Frame('0') + Frame('') +
    Frame('0') + Frame('0') + Frame('0') + Frame('1') +
    Frame(NyxDocumentationMember(Ref('TExample'), 'M').Key) + Frame('2') + Frame('Method.') +
    Frame('0') + Frame('') + Frame('0') + Frame('0') + Frame('0');
  Check(LRegistration.DeclarationContentAt(0) = LExpected, 'Nil intent retains independently constructed legacy bytes exactly');
  LScope := Construction;
  try
    SetLength(LDeclarations, 1);
    LDeclarations[0] := NewNyxDocumentationType(Ref('TExample'), ndtClass, 'Class.')
      .WithMember(WithNyxDocumentationMemberIntent(Bundle(True, True).DeclarationAt(0).MemberAt(0), nil, LScope));
    LRegistration := NewNyxDocumentationRegistration(NyxDocumentationUnit('mechanics.memberintent'), Ref('TExample'), LDeclarations);
    Check(LRegistration.DeclarationContentAt(0) = LExpected, 'Explicit clear restores legacy framing without a stale marker or scalar');
  finally LScope.Retire; end;
  LRegistration := Bundle(True, True);
  Check(Pos('member-declaration-intent', LRegistration.DeclarationContentAt(0)) > 0, 'Known modifiers participate in exact content');
  LRegistry := TNyxDocumentationRegistry.Create;
  LCatalog := TNyxCatalog.Create;
  LBad := TNyxCatalog.Create;
  try
    LCatalog.RegisterKind('intent', 'Intent', 'Custom', False, LRegistration);
    LIndex := LRegistry.Refresh(CaptureNyxSchemas, CaptureNyxCallbacks, LCatalog.Capture, 0);
    Check((LIndex.DeclarationCount = 1) and IntentOf(LIndex.FindMember(NyxDocumentationMember(Ref('TExample'), 'M'))).DeclaredClass,
      'Actual registration/index retains authored intent without new reference edges');
    LBefore := LIndex;
    LVersion := LRegistry.Version;
    LBad.RegisterKind('first', 'First', 'Custom', False, LRegistration);
    LBad.RegisterKind('second', 'Second', 'Custom', False, Bundle(True, False));
    LIndex := nil;
    LRefused := False;
    try LIndex := LRegistry.Refresh(CaptureNyxSchemas, CaptureNyxCallbacks, LBad.Capture, LVersion);
    except on ENyxDocumentation do LRefused := True; end;
    Check(LRefused and (LIndex = nil) and (LRegistry.Current = LBefore) and (LRegistry.Version = LVersion),
      'Changed modifier content refuses atomically before replacing the prior index/version');
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
    raise EArgumentException.Create('Usage: nyx_documentation_memberdeclarations_tests <repository-root>');
  Result := IncludeTrailingPathDelimiter(ExpandFileName(ParamStr(1)));

  if not DirectoryExists(Result) or not FileExists(Result + 'PROJECT.md') or
    not FileExists(Result + 'src' + PathDelim + 'nyx.data.pas') or
    not FileExists(Result + 'src' + PathDelim + 'nyx.model.pas') then
    raise EArgumentException.Create('Repository root requires PROJECT.md and actual data/model units');
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
      raise Exception.Create('Actual member source exceeds existing lexical allowance');
    SetLength(LBytes, Integer(LStream.Size));

    if Length(LBytes) > 0 then
      Move(LStream.Memory^, LBytes[0], Length(LBytes));
    Result := NewNyxPascalSyntaxSnapshot(NyxDecodeUTF8(LBytes), LOptions);
  finally
    LStream.Free;
  end;
end;

procedure ActualMember(const ASource: INyxPascalSyntaxSnapshot; ADeclaration: Integer;
  const AOwner, AName: TNyxText; AClass, AStatic: Boolean; AKind: TNyxDocumentationMemberKind);
var
  LDeclaration: TNyxPascalSyntaxDeclaration;
  LValue: INyxDocumentationMember;
  LIntent: INyxDocumentationMemberIntent;
  LScope: INyxDocumentationTypeConstruction;
  LMemberScope: TNyxPascalSyntaxScope;
begin
  LDeclaration := ASource.Declaration(ADeclaration);
  LMemberScope := ASource.Scope(LDeclaration.Scope);
  Check((LDeclaration.Name = AName) and (LDeclaration.Kind = psdkRoutine) and
    (LMemberScope.Kind = psskMembers) and (LMemberScope.OwnerDeclaration >= 0) and
    (ASource.Declaration(LMemberScope.OwnerDeclaration).Name = AOwner) and
    (LDeclaration.Flags * [psdfIncomplete, psdfIncompleteHeader, psdfUnsupported, psdfConditional] = []) and
    ((psdfClassMember in LDeclaration.Flags) = AClass) and
    ((psdfStatic in LDeclaration.Flags) = AStatic) and (LDeclaration.Visibility = psavPublic),
    'Actual selected member owns complete authored modifiers and explicit public visibility');
  LScope := Construction;
  try
    LIntent := NewNyxDocumentationMemberIntent(psdfClassMember in LDeclaration.Flags,
      psdfStatic in LDeclaration.Flags, LDeclaration.Visibility, LDeclaration.CallingConvention, LScope);
    LValue := WithNyxDocumentationMemberIntent(NewNyxDocumentationMember(NyxDocumentationMember(
      NyxDocumentationType(ASource.Scope(0).Name, AOwner), AName), AKind, 'Selected actual declaration.'), LIntent, LScope);
  finally
    LScope.Retire;
  end;
  LIntent := IntentOf(LValue);
  Check((LIntent.DeclaredClass = AClass) and (LIntent.DeclaredStatic = AStatic) and
    (LIntent.Visibility = LDeclaration.Visibility) and
    (LIntent.CallingConvention = LDeclaration.CallingConvention) and (LValue.Signature = nil),
    'Actual scalar intent detaches without inventing a complete signature, class or compiler ABI');
  Inc(GActual);
end;

{ A forward class entry has no member scope. Follow its owned same-name chain
  to the actual complete declaration rather than assuming the first ordinal. }
function FullOwner(const ASource: INyxPascalSyntaxSnapshot; const AName: TNyxText): Integer;
begin
  Result := ASource.Named(0, AName);
  while Result >= 0 do
  begin

    if (ASource.Declaration(Result).Kind = psdkType) and
      (ASource.Declaration(Result).NestedScope >= 0) and
      not (psdfForward in ASource.Declaration(Result).Flags) then
      Exit;
    Result := ASource.Declaration(Result).NextNamed;
  end;
end;

procedure ActualSources(const ARepository: String);
var
  LSyntax: INyxPascalSyntaxSnapshot;
  LOwner, LMember, LCount: Integer;
begin
  LSyntax := SourceSyntax(ARepository + 'src' + PathDelim + 'nyx.data.pas');
  Check(LSyntax.Scope(0).Name = 'nyx.data', 'Mandatory data unit retains its actual declared identity');
  LOwner := FullOwner(LSyntax, 'TNyxDataValue');
  Check(LOwner >= 0, 'Mandatory actual data value owner exists');
  LMember := LSyntax.Named(LSyntax.Declaration(LOwner).NestedScope, 'ParseJSON');
  Check(LMember >= 0, 'Mandatory actual ParseJSON exists');
  ActualMember(LSyntax, LMember, 'TNyxDataValue', 'ParseJSON', True, True, ndmMethod);
  LMember := LSyntax.Named(LSyntax.Declaration(LOwner).NestedScope, 'ToJSON');
  Check(LMember >= 0, 'Mandatory actual instance ToJSON exists');
  ActualMember(LSyntax, LMember, 'TNyxDataValue', 'ToJSON', False, False, ndmMethod);
  LSyntax := SourceSyntax(ARepository + 'src' + PathDelim + 'nyx.model.pas');
  Check(LSyntax.Scope(0).Name = 'nyx.model', 'Mandatory model unit retains its actual declared identity');
  LOwner := FullOwner(LSyntax, 'TNyxNode');
  Check(LOwner >= 0, 'Mandatory actual node owner exists');
  LMember := LSyntax.Named(LSyntax.Declaration(LOwner).NestedScope, 'CreateRealized');
  Check(LMember >= 0, 'Mandatory actual class/static construction function exists');
  ActualMember(LSyntax, LMember, 'TNyxNode', 'CreateRealized', True, True, ndmMethod);
  LMember := LSyntax.Named(LSyntax.Declaration(LOwner).NestedScope, 'Named');
  Check(LMember >= 0, 'Mandatory actual instance fluent member exists');
  ActualMember(LSyntax, LMember, 'TNyxNode', 'Named', False, False, ndmMethod);
  LMember := LSyntax.Named(LSyntax.Declaration(LOwner).NestedScope, 'Create');
  LCount := 0;
  while LMember >= 0 do
  begin
    ActualMember(LSyntax, LMember, 'TNyxNode', 'Create', False, False, ndmConstructor);
    Inc(LCount);
    LMember := LSyntax.Declaration(LMember).NextNamed;
  end;
  Check((LCount = 3) and (GActual = 7), 'All three actual node constructors and all seven required members are inspected');
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
    AlternateCases;
    AttachmentCases;
    BoundsCases;
    IndexCases;
    {$ifndef PAS2JS}
    ActualSources(LRepository);
    {$endif}
    Check((DocumentationControlsConstructed = 0) and (DocumentationCallbacksConstructed = 0) and
      (DocumentationCallbacksInvoked = 0) and (DocumentationGlobalFactoryCalls = 0) and
      (DocumentationGlobalRoutineCalls = 0), 'Intent inspection executes no documented control, callback, factory or example');
    {$ifdef PAS2JS}
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' portable member intent checks';
    {$else}
    WriteLn('PASS ', GChecks, ' member intent checks / all7 actual members inspected');
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
