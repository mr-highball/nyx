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
