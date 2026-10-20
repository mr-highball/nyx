{ Portable documentation contract qualification. MIT license, see LICENSE. }
program nyx_documentation_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.types, nyx.data, nyx.scheduler, nyx.contract,
  nyx.event.payload, nyx.schema, nyx.catalog, nyx.documentation,
  nyx.documentation.schema, nyx.test.documentation
  {$ifdef PAS2JS}, Web{$endif};

type
  { An alternate producer deliberately shares its route vector. Capture must
    detach it without using Execute, transport serialization or a hidden node. }
  TAlternateSchema = class(TInterfacedObject, INyxSchemaSnapshot, INyxSchemaInspection)
  public
    Event: TNyxEventSchema;
    Scope: INyxCancellationScope;
    CancelOnRead: Boolean;
    ClaimedKinds, ClaimedEvents: Integer;
    ExecuteCalls, TransportCalls, KindReads: Integer;
    function GetRevision: Integer;
    function ToData: TNyxDataValue;
    procedure Execute(const AAction: INyxSchemaAction);
    function KindCount: Integer;
    function KindAt(AIndex: Integer): TNyxKindRef;
    function FindKind(const AKind: TNyxKindRef): Integer;
    function PropertyCount(AKind: Integer): Integer;
    function PropertyAt(AKind, AIndex: Integer): TNyxPropertyInfo;
    function EventCount(AKind: Integer): Integer;
    function EventAt(AKind, AIndex: Integer): TNyxEventSchema;
  end;

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin
  Inc(GChecks);

  if not ACondition then
    raise Exception.Create(AReason);
end;

function TAlternateSchema.GetRevision: Integer;
begin
  Result := 7;
end;

function TAlternateSchema.ToData: TNyxDataValue;
begin
  Result := NyxNull;
  Inc(TransportCalls);
  raise Exception.Create('Typed documentation must not decode transport');
end;

procedure TAlternateSchema.Execute(const AAction: INyxSchemaAction);
begin
  Inc(ExecuteCalls);
  raise Exception.Create('Typed documentation must not execute a schema environment');
end;

function TAlternateSchema.KindCount: Integer;
begin
  Result := ClaimedKinds;
end;

function TAlternateSchema.KindAt(AIndex: Integer): TNyxKindRef;
begin
  Inc(KindReads);

  if AIndex <> 0 then
    raise Exception.Create('Alternate kind ordinal');
  Result := NyxCustomKind('alternate-documentation');
end;

function TAlternateSchema.FindKind(const AKind: TNyxKindRef): Integer;
begin
  Result := -1;

  if AKind.Name = 'alternate-documentation' then
    Result := 0;
end;

function TAlternateSchema.PropertyCount(AKind: Integer): Integer;
begin
  Result := 0;
end;

function TAlternateSchema.PropertyAt(AKind, AIndex: Integer): TNyxPropertyInfo;
begin
  Result := Default(TNyxPropertyInfo);
  raise Exception.Create('No alternate properties');
end;

function TAlternateSchema.EventCount(AKind: Integer): Integer;
begin
  Result := ClaimedEvents;
end;

function TAlternateSchema.EventAt(AKind, AIndex: Integer): TNyxEventSchema;
begin

  if AIndex <> 0 then
    raise Exception.Create('Alternate event ordinal');

  if CancelOnRead then
    Scope.Cancel;
  Result := Event;
end;

procedure RunUnitDeclarations;
var
  LUnit, LOtherUnit, LType: TNyxDocumentationRef;
  LDeclaration, LCopy, LOld, LClass, LBounded: INyxDocumentationType;
  LField: INyxDocumentationMember;
  LCancel: INyxCancellationScope;
  LRejected: Boolean;
  LAt: Integer;
begin
  LUnit := NyxDocumentationUnit('nyx.test.documentation');
  LOtherUnit := NyxDocumentationUnit('nyx.test.documentation.other');
  LType := NyxDocumentationType('Sample.Controls', 'TBadge', 1);
  Check(LUnit.Defined and LUnit.IsUnit and not LUnit.IsMember and
    (LUnit.TypeName = '') and (LUnit.GenericArity = 0) and
    LUnit.Same(NyxDocumentationUnit('NYX.TEST.DOCUMENTATION')),
    'A unit identity has its actual qualified name without a fake type or generic arity');
  Check((LType.Key = TNyxText('sample.controls|tbadge|1||')) and
    (NyxDocumentationMember(LType, 'Build', 'FromText').Key =
      TNyxText('sample.controls|tbadge|1|build|fromtext')) and
    not LUnit.Same(NyxDocumentationType('nyx.test', 'documentation')) and
    not LUnit.Same(LOtherUnit),
    'Existing type/member keys stay exact and cannot collide with unit identities');
  Check((Ord(ndtClass) = 0) and (Ord(ndtCallback) = 5) and
    (Ord(ndmConstructor) = 0) and (Ord(ndmEnumValue) = 7),
    'Appending unit/routine kinds preserves existing public enum ordinals');
  LDeclaration := DocumentationUnitReference;
  Check((LDeclaration.Kind = ndtUnit) and LDeclaration.Identity.Same(LUnit) and
    (LDeclaration.BaseCount = 0) and not LDeclaration.AliasTarget.Defined and
    (LDeclaration.MemberCount = 3), 'Global declarations retain an actual unit owner without class ancestry');
  Check((LDeclaration.MemberAt(0).Kind = ndmFactory) and
    LDeclaration.MemberAt(0).Identity.Owner.Same(LUnit) and
    LDeclaration.MemberAt(0).Signature.ResultType.Same(
      NyxDocumentationType('nyx.test.documentation', 'IDocumentationBadge')),
    'A global factory preserves its specialized public result and declaring unit');
  Check((LDeclaration.MemberAt(1).Kind = ndmRoutine) and
    (LDeclaration.MemberAt(2).Kind = ndmRoutine) and
    (LDeclaration.MemberAt(1).Signature.ParameterCount = 0) and
    (LDeclaration.MemberAt(2).Signature.ParameterCount = 1) and
    not LDeclaration.MemberAt(1).Identity.Same(LDeclaration.MemberAt(2).Identity) and
    (LDeclaration.MemberAt(1).Identity.Key =
      TNyxText('nyx.test.documentation||0|documentationgreeting|default')),
    'Real global overloads retain separate exact keys and zero/one-parameter signatures');
  Check(LDeclaration.MemberAt(2).Identity.Owner.IsUnit and
    not LDeclaration.MemberAt(2).Identity.IsUnit and
    LDeclaration.MemberAt(2).Identity.IsMember,
    'A unit member remains a member while its owner is the unit identity');
  LCopy := CaptureNyxDocumentationType(LDeclaration);
  LCopy := CaptureNyxDocumentationType(LCopy);
  Check(LCopy.Identity.IsUnit and
    LCopy.MemberAt(2).Identity.Same(LDeclaration.MemberAt(2).Identity) and
    (LCopy.ExampleAt(0).Pascal = LDeclaration.ExampleAt(0).Pascal),
    'Same-variable unit capture detaches declarations and retains inert examples');
  LOld := NewNyxDocumentationType(LUnit, ndtUnit, 'Unit globals');
  LCopy := LOld.WithMember(LDeclaration.MemberAt(0));
  LClass := TDocumentationBadge.Reference.WithRelated(LUnit);
  Check((LOld.MemberCount = 0) and (LCopy.MemberCount = 1) and
    LClass.RelatedAt(LClass.RelatedCount - 1).Same(LUnit),
    'Unit members compose immutably and a real class may link to its declaring unit');
  LField := NewNyxDocumentationMember(
    NyxDocumentationMember(LUnit, 'DocumentationGlobalRoutineCalls'), ndmField,
    'Count explicit routine invocations in this fixture.')
    .OfType(NyxDocumentationType('System', 'Integer'));
  LCopy := LCopy.WithMember(LField);
  Check((LCopy.MemberAt(1).Kind = ndmField) and
    LCopy.MemberAt(1).Identity.Owner.IsUnit and
    (LCopy.MemberAt(1).Signature = nil), 'A global variable is a typed unit field, not a callable class member');
  for LAt := 0 to 16 do
  begin
    LRejected := False;
    try
      case LAt of
        0: NewNyxDocumentationType(LUnit, ndtClass, '');
        1: NewNyxDocumentationType(LType, ndtUnit, '');
        2: NyxDocumentationType('Sample.Controls', '');
        3: NyxDocumentationParameter('Value', LUnit);
        4: NewNyxDocumentationSignature([], LUnit);
        5: NewNyxDocumentationMember(NyxDocumentationMember(LType, 'Value'),
             ndmProperty, '').OfType(LUnit);
        6: NewNyxDocumentationType(NyxDocumentationType('Sample', 'Alias'),
             ndtAlias, '').WithAlias(LUnit);
        7: NewNyxDocumentationType(LType, ndtClass, '').WithBase(LUnit);
        8: NewNyxDocumentationType(LUnit, ndtUnit, '').WithBase(LType);
        9: NewNyxDocumentationMember(NyxDocumentationMember(LUnit, 'Create'), ndmConstructor, '');
        10: NewNyxDocumentationMember(NyxDocumentationMember(LUnit, 'Method'), ndmMethod, '');
        11: NewNyxDocumentationMember(NyxDocumentationMember(LType, 'Routine'), ndmRoutine, '');
        12: NewNyxDocumentationMember(NyxDocumentationMember(LUnit, 'Property'), ndmProperty, '');
        13: NewNyxDocumentationMember(NyxDocumentationMember(LUnit, 'Fluent'), ndmFluentMethod, '');
        14: NewNyxDocumentationMember(NyxDocumentationMember(LUnit, 'Event'), ndmEvent, '');
        15: NewNyxDocumentationMember(NyxDocumentationMember(LUnit, 'EnumValue'), ndmEnumValue, '');
        16: NyxDocumentationMember(NyxDocumentationMember(LUnit, 'Outer'), 'Inner');
      end;
    except
      on ENyxDocumentation do LRejected := True;
    end;
    Check(LRejected, 'Invalid unit/type/member role combination refuses ' + IntToStr(LAt));
  end;
  LRejected := False;
  try
    LCopy.WithMember(NewNyxDocumentationMember(
      NyxDocumentationMember(LOtherUnit, 'NewDocumentationBadge'), ndmFactory, ''));
  except
    on ENyxDocumentation do LRejected := True;
  end;
  Check(LRejected and (LCopy.MemberCount = 2), 'A global declaration cannot move to a different unit owner');
  LRejected := False;
  try
    NyxDocumentationUnit(StringOfChar('U', 257));
  except
    on ENyxDocumentation do LRejected := True;
  end;
  Check(LRejected, 'Unit names retain the existing identity scalar allowance');
  LRejected := False;
  try
    NyxDocumentationUnit('javascript:alert');
  except
    on ENyxDocumentation do LRejected := True;
  end;
  Check(LRejected, 'A unit reference cannot carry a URL scheme in place of Pascal identity');
  LBounded := LOld;
  for LAt := 0 to NyxDocumentationMaximumMembers - 1 do
  begin
    LBounded := LBounded.WithMember(NewNyxDocumentationMember(
      NyxDocumentationMember(LUnit, 'Routine' + IntToStr(LAt)), ndmRoutine, ''));
  end;
  LRejected := False;
  try
    LBounded.WithMember(NewNyxDocumentationMember(
      NyxDocumentationMember(LUnit, 'OneTooMany'), ndmRoutine, ''));
  except
    on ENyxDocumentation do LRejected := True;
  end;
  Check(LRejected and (LBounded.MemberCount = NyxDocumentationMaximumMembers),
    'Unit declarations share the existing member allowance without mutating earlier readers');
  LCancel := NewNyxCancellationScope;
  LCancel.Cancel;
  LRejected := False;
  try
    LCopy := CaptureNyxDocumentationType(LDeclaration, LCancel);
  except
    on ENyxDocumentationCancelled do LRejected := True;
  end;
  Check(LRejected and (LCopy = nil), 'Cancelled unit capture publishes no partial declaration');
  Check((DocumentationControlsConstructed = 0) and (DocumentationCallbacksConstructed = 0) and
    (DocumentationCallbacksInvoked = 0) and (DocumentationGlobalFactoryCalls = 0) and
    (DocumentationGlobalRoutineCalls = 0), 'Unit/type/member inspection invokes no global factory, routine or callback body');
end;

{ Interface-valued factory/fluent expressions may retain compiler temporaries
  for this caller's whole lexical lifetime. The outer test checks retirement
  after this procedure returns, when every named and temporary owner is gone. }
procedure ExerciseGlobalDeclarations;
var
  LBadge: IDocumentationBadge;
begin
  LBadge := NewDocumentationBadge('global-factory');
  LBadge := LBadge.WithGreeting(DocumentationGreeting('Ada'));
  Check((LBadge.Greeting = TNyxText('Hello Ada')) and
    (DocumentationGreeting = TNyxText(DocumentationGreetingDefault)) and
    (DocumentationGlobalFactoryCalls = 1) and (DocumentationGlobalRoutineCalls = 2),
    'Explicit global factory and overloaded routine execution agrees with the declared public contracts');
  LBadge := nil;
end;

procedure Run;
const
  CExact: TNyxText = '// 🚀 𐐷' + #0 + #13#10 + 'Badge.WithGreeting(''Ada'');';
var
  LType, LOld, LFrozen, LInterface, LCallback, LLarge: INyxDocumentationType;
  LMember, LField: INyxDocumentationMember;
  LSignature: INyxDocumentationSignature;
  LOwner, LText, LAlias: TNyxDocumentationRef;
  LCatalog: TNyxCatalog;
  LBefore, LSnapshot: INyxSchemaSnapshot;
  LInspection, LBeforeInspection: INyxSchemaInspection;
  LBound, LMissing, LAlternateBound: INyxDocumentationSchema;
  LProperty: TNyxPropertyInfo;
  LEvent: TNyxEventSchema;
  LAlternate: TAlternateSchema;
  LAlternateSnapshot: INyxSchemaSnapshot;
  LCancel: INyxCancellationScope;
  LBadge: IDocumentationBadge;
  LSource: INyxDocumentationSource;
  LBeforeData: TNyxText;
  LRejected: Boolean;
  LAt, LRevision, LMembers, LExamples: Integer;
begin
  RunUnitDeclarations;
  LOwner := NyxDocumentationType('Sample.Controls', 'TBadge', 1);
  LText := NyxDocumentationType('nyx.text', 'TNyxText');
  Check(LOwner.Same(NyxDocumentationType('sample.controls', 'tbadge', 1)) and
    not LOwner.Same(NyxDocumentationType('sample.controls', 'TBadge', 0)),
    'Qualified identity retains case-insensitive Pascal names and explicit generic arity');
  Check(not NyxDocumentationType('A.B', 'C').Same(NyxDocumentationType('A', 'B.C')),
    'Unit and nested-type boundaries cannot collapse into the same identity');
  Check(not NyxDocumentationMember(LOwner, 'Build', 'FromText').Same(
    NyxDocumentationMember(LOwner, 'Build', 'FromNumber')),
    'Stable declared overload keys retain independent member identities');
  LRejected := False;
  try
    NyxDocumentationType('javascript:alert', 'Unsafe');
  except
    on ENyxDocumentation do LRejected := True;
  end;
  Check(LRejected, 'Related identities cannot carry executable schemes or arbitrary paths');
  LSignature := NewNyxDocumentationSignature([
    NyxDocumentationParameter('AName', LText, ndpConst).WithDefault('''Ada''')], LOwner);
  Check((LSignature.ParameterCount = 1) and LSignature.ParameterAt(0).HasDefault and
    (LSignature.ParameterAt(0).DefaultPascal = TNyxText('''Ada''')) and
    LSignature.ResultType.Same(LOwner) and (LSignature.Evidence = ndeAuthoredDeclaration),
    'Declared signatures preserve typed defaults and original result identity without observed-runtime claims');
  LRejected := False;
  try
    NewNyxDocumentationSignature([NyxDocumentationParameter('Value', LText),
      NyxDocumentationParameter('value', LText)], LOwner);
  except
    on ENyxDocumentation do LRejected := True;
  end;
  Check(LRejected, 'Duplicate Pascal parameter names refuse before publication');
  LMember := NewNyxDocumentationMember(NyxDocumentationMember(LOwner, 'WithGreeting'),
    ndmFluentMethod, 'Keep the specialized fluent contract.').WithSignature(LSignature);
  LOld := NewNyxDocumentationType(LOwner, ndtClass, 'Friendly badge');
  LType := LOld.WithBase(NyxDocumentationType('nyx.controls', 'TNyxBadge'))
    .WithMember(LMember).WithExample(NyxDocumentationExample('Exact inert Pascal', CExact));
  Check((LOld.MemberCount = 0) and (LType.MemberCount = 1) and
    (LType.ExampleAt(0).Pascal = CExact), 'Fluent metadata detaches vectors and retains exact supplementary/NUL/CRLF text');
  LRejected := False;
  try
    LType.WithMember(LMember);
  except
    on ENyxDocumentation do LRejected := True;
  end;
  Check(LRejected and (LType.MemberCount = 1), 'Duplicate member/overload publication leaves the prior declaration intact');
  LRejected := False;
  try
    NewNyxDocumentationType(NyxDocumentationType('Other', 'TBadge'), ndtClass, '')
      .WithMember(LMember);
  except
    on ENyxDocumentation do LRejected := True;
  end;
  Check(LRejected, 'An inherited member cannot be copied into a different declaring owner');
  LField := NewNyxDocumentationMember(NyxDocumentationMember(LOwner, 'Greeting'), ndmProperty, '').OfType(LText);
  Check((LField.Signature = nil) and LField.ValueType.Same(LText),
    'A missing call signature remains unknown while a declared property type remains explicit');
  LAlias := NyxDocumentationType('Sample.Controls', 'TGreeting');
  LFrozen := NewNyxDocumentationType(LAlias, ndtAlias, '').WithAlias(LText);
  Check(LFrozen.Identity.Same(LAlias) and LFrozen.AliasTarget.Same(LText),
    'Aliases retain their nominal owner separately from the referenced type');
  LFrozen := CaptureNyxDocumentationType(LType);
  LFrozen := CaptureNyxDocumentationType(LFrozen);
  Check(LFrozen.MemberAt(0).Identity.Same(LMember.Identity) and
    (LFrozen.ExampleAt(0).Pascal = CExact), 'Detached same-variable capture preserves metadata without borrowing a producer');
  LCancel := NewNyxCancellationScope;
  LCancel.Cancel;
  LRejected := False;
  try
    LFrozen := CaptureNyxDocumentationType(LType, LCancel);
  except
    on ENyxDocumentationCancelled do LRejected := True;
  end;
  Check(LRejected and (LFrozen = nil), 'Cancelled metadata capture publishes no partial descriptor');

  LCatalog := TNyxCatalog.Create;
  try
    LBefore := CaptureNyxSchemas;
    Check(Supports(LBefore, INyxSchemaInspection, LBeforeInspection),
      'Existing immutable schema snapshots expose optional typed inspection');
    RegisterDocumentationFixture(LCatalog);
    LSnapshot := CaptureNyxSchemas;
    LRevision := LSnapshot.Revision;
    RegisterDocumentationFixture(LCatalog);
    Check(NyxSchemaRevision = LRevision, 'Repeated declaration registration preserves existing registry generation');
    Check(Supports(LSnapshot, INyxSchemaInspection, LInspection) and
      (LInspection.FindKind(NyxCustomKind(DocumentationFixtureKind)) >= 0) and
      (LBeforeInspection.FindKind(NyxCustomKind(DocumentationFixtureKind)) < 0),
      'Later registration cannot mutate a previously captured metadata environment');
    LType := TDocumentationBadge.Reference;
    LInterface := TDocumentationBadge.InterfaceReference;
    LCallback := TDocumentationCallback.Reference;
    LBeforeData := LSnapshot.ToData.ToJSON;
    LBound := CaptureNyxDocumentationSchema(LType, NyxCustomKind(DocumentationFixtureKind),
      LSnapshot, LCatalog);
    Check(LBound.HasSchema and LBound.HasCatalog and not LBound.HasPrimitive and
      (LBound.PropertyCount = 1) and (LBound.EventCount = 1),
      'Explicit creator binding reuses captured extension facts without guessing a primitive implementation');
    LProperty := LBound.PropertyAt(0);
    Check((LProperty.DefaultValue = TNyxText(DocumentationGreetingDefault)) and
      (LProperty.Support.Browser = ncCustom) and (LProperty.Support.Native = ncCustom) and
      (LBound.FactsEvidence = ndeSchemaDeclaration),
      'Defaults and declared target support come directly from the existing schema owner');
    Check((LBound.Summary = TNyxText(DocumentationFixtureSummary)) and
      (LBound.SummaryEvidence = ndeCatalogDeclaration),
      'Creator prose retains explicit catalog precedence over decoration prose');
    LEvent := LBound.EventAt(0);
    Check((LEvent.Trigger = ntNamed) and LEvent.Payload.Defined and
      (LEvent.Payload.Kind = nepScalar), 'Named callback payload metadata retains its original typed specification');
    LEvent.Title := 'Changed detached title';
    SetLength(LEvent.Routes, 1);
    Check((LSnapshot.ToData.ToJSON = LBeforeData) and
      (LBound.EventAt(0).Title <> LEvent.Title) and (Length(LBound.EventAt(0).Routes) = 0),
      'Returned event records/vectors cannot mutate captured schema or documentation readers');
    Check((LInterface.Kind = ndtInterface) and (LInterface.BaseCount = 1) and
      (LCallback.Kind = ndtCallback) and (LCallback.BaseCount = 1) and
      LCallback.BaseAt(0).Same(NyxDocumentationType('nyx.events', 'TNyxEventCallback')) and
      (LCallback.MemberAt(0).Signature.ParameterCount = 2),
      'Public interface inheritance and callback parameter/result identities remain declared metadata');
    Check((DocumentationControlsConstructed = 0) and (DocumentationCallbacksConstructed = 0) and
      (DocumentationCallbacksInvoked = 0), 'Metadata lookup and registration construct no documented controls or callbacks');
    LMissing := CaptureNyxDocumentationSchema(LType, NyxCustomKind('missing-documentation'), LSnapshot);
    Check(not LMissing.HasSchema and not LMissing.HasCatalog and not LMissing.HasPrimitive and
      (LMissing.PropertyCount = 0), 'Absent registrations remain explicit unknown metadata');
    LMissing := CaptureNyxDocumentationSchema(LType, NyxCustomKind('button'), LSnapshot, LCatalog);
    Check(LMissing.HasPrimitive and (LMissing.PrimitiveInfo.Kind = TNyxText('button')),
      'Known primitive capability grades reuse the original primitive metadata without control construction');
    RegisterNyxSchema(NyxCustomKind('empty-documentation'), [], []);
    LMissing := CaptureNyxDocumentationSchema(LType, NyxCustomKind('empty-documentation'), CaptureNyxSchemas);
    Check(LMissing.HasSchema and (LMissing.PropertyCount = 0),
      'A genuinely empty schema remains distinct from a missing schema');
    LBadge := TDocumentationBadge.Create('friendly').WithGreeting('Ada');
    Check((LBadge.Greeting = TNyxText('Ada')) and Supports(LBadge, INyxDocumentationSource, LSource),
      'An explicitly constructed extension retains its specialized public contract and optional decoration');
    LFrozen := LSource.Documentation;
    LBadge := nil;
    LSource := nil;
    Check((DocumentationControlsDestroyed = 1) and (LFrozen.MemberCount = 3) and
      (LFrozen.MemberAt(0).Kind = ndmConstructor) and
      (LFrozen.MemberAt(0).Signature.ParameterAt(0).Name = TNyxText('AID')),
      'Retained documentation does not retain the original control or document');
    ExerciseGlobalDeclarations;
    Check((DocumentationControlsConstructed = 2) and (DocumentationControlsDestroyed = 2) and
      (DocumentationCallbacksConstructed = 0) and (DocumentationCallbacksInvoked = 0),
      'Explicit factory ownership retires its control independently and executes no callback');
  finally
    LCatalog.Free;
  end;

  LAlternate := TAlternateSchema.Create;
  LAlternateSnapshot := LAlternate;
  LAlternate.ClaimedKinds := 1;
  LAlternate.ClaimedEvents := 1;
  LAlternate.Event := NyxNamedEventSchema(NyxEvent('Sample'), 'Sample', 'Declared event',
    ncCustom, ncCustom, NyxSignalPayload);
  SetLength(LAlternate.Event.Routes, 1);
  LAlternate.Event.Routes[0].Trigger := ntClick;
  LAlternate.Event.Routes[0].OriginID := 'original';
  LAlternate.Event.Routes[0].Payload := NyxSignalPayload;
  LAlternateBound := CaptureNyxDocumentationSchema(LType,
    NyxCustomKind('alternate-documentation'), LAlternateSnapshot);
  LAlternate.Event.Routes[0].OriginID := 'later producer';
  LEvent := LAlternateBound.EventAt(0);
  LEvent.Routes[0].OriginID := 'later reader';
  Check((LAlternateBound.EventAt(0).Routes[0].OriginID = TNyxText('original')) and
    (LAlternate.ExecuteCalls = 0) and (LAlternate.TransportCalls = 0),
    'Alternate routes are detached without executing environments or decoding a second metadata protocol');
  LAlternate.ClaimedKinds := 4097;
  LAt := LAlternate.KindReads;
  LRejected := False;
  try
    LAlternateBound := CaptureNyxDocumentationSchema(LType,
      NyxCustomKind('alternate-documentation'), LAlternateSnapshot);
  except
    on ENyxDocumentation do LRejected := True;
  end;
  Check(LRejected and (LAlternateBound = nil) and (LAlternate.KindReads = LAt),
    'Oversized alternate kind counts refuse before row reads and publication');
  LAlternate.ClaimedKinds := 1;
  LAlternate.Event.Title := StringOfChar('x', NyxDocumentationMaximumBytes + 1);
  LRejected := False;
  try
    LAlternateBound := CaptureNyxDocumentationSchema(LType,
      NyxCustomKind('alternate-documentation'), LAlternateSnapshot);
  except
    on ENyxDocumentation do LRejected := True;
  end;
  Check(LRejected and (LAlternateBound = nil),
    'Oversized alternate text refuses before copying routes or publishing metadata');
  LAlternate.Event.Title := 'Sample';
  LAlternate.Scope := NewNyxCancellationScope;
  LAlternate.CancelOnRead := True;
  LRejected := False;
  try
    LAlternateBound := CaptureNyxDocumentationSchema(LType,
      NyxCustomKind('alternate-documentation'), LAlternateSnapshot, nil, LAlternate.Scope);
  except
    on ENyxDocumentationCancelled do LRejected := True;
  end;
  Check(LRejected and (LAlternateBound = nil), 'Cancellation during a metadata getter refuses the entire captured result');
  LAlternateSnapshot := nil;

  LOwner := NyxDocumentationType('Sample', 'Large');
  LLarge := NewNyxDocumentationType(LOwner, ndtRecord, 'Bounded metadata');
  for LAt := 0 to 63 do
  begin
    LLarge := LLarge.WithMember(NewNyxDocumentationMember(
      NyxDocumentationMember(LOwner, 'Field' + IntToStr(LAt)), ndmField,
      StringOfChar('x', 2048)).OfType(LText));
  end;
  LMembers := LLarge.MemberCount;
  LRejected := False;
  LExamples := 0;
  for LAt := 0 to NyxDocumentationMaximumExamples - 1 do
  begin
    LOld := LLarge;
    try
      LLarge := LLarge.WithExample(NyxDocumentationExample('Large inert example', StringOfChar('x', 16384)));
      Inc(LExamples);
    except
      on ENyxDocumentation do
      begin
        LRejected := True;
        LLarge := LOld;
        Break;
      end;
    end;
  end;
  Check(LRejected and (LLarge.MemberCount = LMembers) and (LLarge.ExampleCount = LExamples),
    'All members/examples share an aggregate UTF-8 allowance and refuse without changing earlier readers');
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' documentation checks';
    {$else}
    WriteLn('PASS ', GChecks, ' documentation checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.textContent := 'FAIL ' + E.Message;
      {$else}
      WriteLn(StdErr, E.Message);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
