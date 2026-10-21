{ Registration/index mechanics and truthful extension refusal. MIT, see LICENSE. }
program nyx_documentation_registry_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.types, nyx.data, nyx.model, nyx.scheduler, nyx.schema,
  nyx.catalog, nyx.catalog.labels, nyx.callbacks, nyx.documentation,
  nyx.documentation.registration, nyx.documentation.registry,
  nyx.documentation.declarations,
  nyx.test.documentation {$ifdef PAS2JS}, Web{$endif};

type
  { Hostile metadata producer exercises capture without constructing a control
    or pretending to supply any undocumented core declaration. }
  TAlternateRegistration = class(TInterfacedObject, INyxDocumentationRegistration)
  public
    Value: INyxDocumentationRegistration;
    ClaimedCount, DeclarationReads: Integer;
    Cancellation: INyxCancellationScope;
    function OriginUnit: TNyxDocumentationRef;
    function Primary: TNyxDocumentationRef;
    function Platforms: TNyxDocumentationPlatforms;
    function DeclarationCount: Integer;
    function DeclarationAt(AIndex: Integer): INyxDocumentationType;
    function DeclarationContentAt(AIndex: Integer): TNyxText;
    function ContentBytes: Integer;
    function ForPrimary(const APrimary: TNyxDocumentationRef): INyxDocumentationRegistration;
  end;
  { Controlled alternate catalog deliberately violates immutable-source rules
    to test conflict/cancellation/generation admission. Ordinary copied facts
    still come from a real catalog snapshot, never an executable recipe. }
  TAlternateCatalog = class(TInterfacedObject, INyxCatalogSnapshot, INyxCatalogDocumentation)
  public
    Base: INyxCatalogSnapshot;
    Replacement: INyxDocumentationRegistration;
    ReplaceAll, ChangeOnRead: Boolean;
    ReplaceIndex, Bump: Integer;
    function Identity: Integer;
    function Revision: Integer;
    function Count: Integer;
    function ItemAt(AIndex: Integer): TNyxComponentInfo;
    function DocumentationAt(AIndex: Integer): INyxDocumentationRegistration;
  end;
  { A single synthetic callback row exercises the declared-primary role guard;
    it provides no implementation class or factory for an inspector to call. }
  TAlternateCallbacks = class(TInterfacedObject, INyxCallbackSnapshot, INyxCallbackDocumentation)
  public
    Value: INyxDocumentationRegistration;
    function Revision: Integer;
    function Count: Integer;
    function HandlerAt(AIndex: Integer): TNyxHandlerRef;
    function DocumentationAt(AIndex: Integer): INyxDocumentationRegistration;
  end;
  { Metadata-only reentrancy checks that a publisher's expected local version
    is revalidated after suppliers return. No cross-registry transaction occurs. }
  TReentrantCatalog = class(TInterfacedObject, INyxCatalogSnapshot, INyxCatalogDocumentation)
  public
    Registry: TNyxDocumentationRegistry;
    Schemas: INyxSchemaSnapshot;
    Callbacks: INyxCallbackSnapshot;
    Base: INyxCatalogSnapshot;
    Entered: Boolean;
    function Identity: Integer;
    function Revision: Integer;
    function Count: Integer;
    function ItemAt(AIndex: Integer): TNyxComponentInfo;
    function DocumentationAt(AIndex: Integer): INyxDocumentationRegistration;
  end;

var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AMessage: TNyxText);
begin
  Inc(GChecks);

  if not AValue then
    raise Exception.Create(AMessage);
end;

function TAlternateRegistration.OriginUnit: TNyxDocumentationRef;
begin
  Result := Value.OriginUnit;
end;

function TAlternateRegistration.Primary: TNyxDocumentationRef;
begin
  Result := Value.Primary;
end;

function TAlternateRegistration.Platforms: TNyxDocumentationPlatforms;
begin
  Result := Value.Platforms;
end;

function TAlternateRegistration.DeclarationCount: Integer;
begin
  Result := ClaimedCount;
end;

function TAlternateRegistration.DeclarationAt(AIndex: Integer): INyxDocumentationType;
begin
  Inc(DeclarationReads);

  if Cancellation <> nil then
    Cancellation.Cancel;
  Result := Value.DeclarationAt(AIndex);
end;

function TAlternateRegistration.DeclarationContentAt(AIndex: Integer): TNyxText;
begin
  Result := '';
  raise Exception.Create('Producer equality data must never be trusted');
end;

function TAlternateRegistration.ContentBytes: Integer;
begin
  Result := 0;
  raise Exception.Create('Producer byte accounting must never be trusted');
end;

function TAlternateRegistration.ForPrimary(const APrimary: TNyxDocumentationRef): INyxDocumentationRegistration;
begin
  Result := nil;
  raise Exception.Create('Producer fluent methods must never run during capture');
end;

function TAlternateCatalog.Identity: Integer;
begin
  Result := Base.Identity;
end;

function TAlternateCatalog.Revision: Integer;
begin
  Result := Base.Revision + Bump;
end;

function TAlternateCatalog.Count: Integer;
begin
  Result := Base.Count;
end;

function TAlternateCatalog.ItemAt(AIndex: Integer): TNyxComponentInfo;
begin

  if ChangeOnRead then
    Inc(Bump);
  Result := Base.ItemAt(AIndex);
end;

function TAlternateCatalog.DocumentationAt(AIndex: Integer): INyxDocumentationRegistration;
var
  LDocumentation: INyxCatalogDocumentation;
begin

  if not Supports(Base, INyxCatalogDocumentation, LDocumentation) then
    raise Exception.Create('Alternate catalog requires its captured documentation capability');
  Result := LDocumentation.DocumentationAt(AIndex);

  if (AIndex = ReplaceIndex) or (ReplaceAll and (Result <> nil)) then
    Result := Replacement;
end;

function TAlternateCallbacks.Revision: Integer;
begin
  Result := 1;
end;

function TAlternateCallbacks.Count: Integer;
begin
  Result := 1;
end;

function TAlternateCallbacks.HandlerAt(AIndex: Integer): TNyxHandlerRef;
begin

  if AIndex <> 0 then
    raise Exception.Create('Synthetic callback ordinal');
  Result := NyxHandler('TMechanicsCallback');
end;

function TAlternateCallbacks.DocumentationAt(AIndex: Integer): INyxDocumentationRegistration;
begin
  HandlerAt(AIndex);
  Result := Value;
end;

function TReentrantCatalog.Identity: Integer;
begin
  Result := Base.Identity;
end;

function TReentrantCatalog.Revision: Integer;
begin
  Result := Base.Revision;
end;

function TReentrantCatalog.Count: Integer;
begin
  Result := Base.Count;
end;

function TReentrantCatalog.ItemAt(AIndex: Integer): TNyxComponentInfo;
begin

  if not Entered then
  begin
    Entered := True;
    Registry.Refresh(Schemas, Callbacks, Base, Registry.Version);
  end;
  Result := Base.ItemAt(AIndex);
end;

function TReentrantCatalog.DocumentationAt(AIndex: Integer): INyxDocumentationRegistration;
var
  LDocumentation: INyxCatalogDocumentation;
begin

  if not Supports(Base, INyxCatalogDocumentation, LDocumentation) then
    raise Exception.Create('Reentrant fixture requires its captured documentation capability');
  Result := LDocumentation.DocumentationAt(AIndex);
end;

function MechanicsType(const AName: TNyxText): TNyxDocumentationRef;
begin
  Result := NyxDocumentationType('registry.mechanics', AName);
end;

{ This intentionally synthetic graph qualifies algorithms, not a supported core
  API or an extension replacement. The genuine badge closure is tested below
  with every real external link intact and must currently refuse publication. }
function MechanicsBundle(AChange: Integer = 0): INyxDocumentationRegistration;
var
  LUnit: TNyxDocumentationRef;
  LRoot, LFlag, LBase, LDerived, LAlias: INyxDocumentationType;
  LSummary: TNyxText;
  LDeclarations: TNyxDocumentationDeclarations;
begin
  LUnit := NyxDocumentationUnit('registry.mechanics');
  LRoot := NewNyxDocumentationType(LUnit, ndtUnit, 'Synthetic index mechanics.')
    .WithMember(NewNyxDocumentationMember(NyxDocumentationMember(LUnit, 'NewMechanics'),
      ndmFactory, 'Synthetic creator identity.').WithSignature(
        NewNyxDocumentationSignature([], MechanicsType('Derived'))));
  LFlag := NewNyxDocumentationType(MechanicsType('Flag'), ndtEnum, 'Synthetic enum reference.');
  LSummary := 'Keep <script>, NUL ' + #0 + ', CRLF' + #13#10 + ' and Unicode: Ω.';

  if AChange = 1 then
    LSummary := 'A conflicting authored summary.';
  LBase := NewNyxDocumentationType(MechanicsType('Base'), ndtClass, LSummary)
    .WithMember(NewNyxDocumentationMember(NyxDocumentationMember(MechanicsType('Base'), 'Echo', 'One'),
      ndmMethod, 'Keep the first overload.').WithSignature(NewNyxDocumentationSignature([
        NyxDocumentationParameter('AValue', MechanicsType('Flag'))], MechanicsType('Flag'))))
    .WithMember(NewNyxDocumentationMember(NyxDocumentationMember(MechanicsType('Base'), 'Echo', 'Two'),
      ndmMethod, 'Keep the second overload.').WithSignature(NewNyxDocumentationSignature([
        NyxDocumentationParameter('AFirst', MechanicsType('Flag')),
        NyxDocumentationParameter('ASecond', MechanicsType('Flag'))], MechanicsType('Flag'))));
  LDerived := NewNyxDocumentationType(MechanicsType('Derived'), ndtClass, 'Synthetic inherited API.')
    .WithBase(MechanicsType('Alias')).WithRelated(LUnit);
  LAlias := NewNyxDocumentationType(MechanicsType('Alias'), ndtAlias, 'Retain an alias identity.')
    .WithAlias(MechanicsType('Base'));

  if AChange = 2 then
    LBase := LBase.WithBase(MechanicsType('Derived'));

  if AChange = 3 then
    LDerived := LDerived.WithRelated(NyxDocumentationType('missing.owner', 'Missing'));

  if AChange = 4 then
    LAlias := NewNyxDocumentationType(MechanicsType('Alias'), ndtAlias, 'Invalid base alias.')
      .WithAlias(MechanicsType('Flag'));

  if AChange = 5 then
    LAlias := NewNyxDocumentationType(MechanicsType('Alias'), ndtAlias, 'An incomplete alias builder.');
  SetLength(LDeclarations, 5);
  LDeclarations[0] := LRoot;
  LDeclarations[1] := LDerived;
  LDeclarations[2] := LFlag;
  LDeclarations[3] := LAlias;
  LDeclarations[4] := LBase;
  Result := NewNyxDocumentationRegistration(LUnit, MechanicsType('Derived'),
    LDeclarations, [ndplatformNative, ndplatformBrowser]);
end;

procedure ExpectRefusal(ARegistry: TNyxDocumentationRegistry;
  const ASchemas: INyxSchemaSnapshot; const ACallbacks: INyxCallbackSnapshot;
  const ACatalog: INyxCatalogSnapshot; const ALimits: TNyxDocumentationIndexLimits;
  const AReason: TNyxText; const ACancellation: INyxCancellationScope = nil;
  const AErrorFragment: TNyxText = '');
var
  LPrevious: INyxDocumentationIndex;
  LVersion: Integer;
  LRefused: Boolean;
begin
  LPrevious := ARegistry.Current;
  LVersion := ARegistry.Version;
  LRefused := False;
  try
    ARegistry.Refresh(ASchemas, ACallbacks, ACatalog, LVersion, ALimits, ACancellation);
  except
    on E: ENyxDocumentation do
      LRefused := (AErrorFragment = '') or (Pos(AErrorFragment, E.Message) > 0);
  end;
  Check(LRefused and (ARegistry.Current = LPrevious) and (ARegistry.Version = LVersion), AReason);
end;

{ Incomplete explicit packages are refusal inputs, never a stand-in for the
  genuine source closure. Preparation must leave all real registries intact. }
procedure ExpectFixtureSourceRefusal(ACatalog: TNyxCatalog;
  const APackage: INyxDocumentationPackage; ALegacyInstalled: Boolean);
var
  LSchemas: INyxSchemaSnapshot;
  LCallbacks: INyxCallbackSnapshot;
  LCatalog: INyxCatalogSnapshot;
  LRefused: Boolean;
  LMessage: String;
begin
  LSchemas := CaptureNyxSchemas;
  LCallbacks := CaptureNyxCallbacks;
  LCatalog := ACatalog.Capture;
  LRefused := False;
  try
    RegisterDocumentationFixture(ACatalog, APackage);
  except
    on E: ENyxDocumentation do
    begin
      LRefused := True;
      LMessage := E.Message;
    end;
  end;
  Check(LRefused and (CaptureNyxSchemas.Revision = LSchemas.Revision) and
    (CaptureNyxCallbacks.Revision = LCallbacks.Revision) and
    (ACatalog.Capture.Revision = LCatalog.Revision),
    'An unavailable source fixture package refuses before any real registry or catalog publication');

  if ALegacyInstalled then
    Check(Pos('legacy metadata', LMessage) > 0,
      'A source registration cannot silently replace the already installed legacy owner');
end;

procedure Run;
var
  LCatalog, LOtherCatalog: TNyxCatalog;
  LRegistry: TNyxDocumentationRegistry;
  LSchemas, LNewSchemas, LTransported: INyxSchemaSnapshot;
  LCallbacks, LNewCallbacks: INyxCallbackSnapshot;
  LCatalogCapture, LOldCatalog, LAlternateCapture: INyxCatalogSnapshot;
  LCatalogDocs: INyxCatalogDocumentation;
  LSchemaDocs: INyxSchemaDocumentation;
  LSchemaInspection: INyxSchemaInspection;
  LCallbackDocs: INyxCallbackDocumentation;
  LBundle, LFrozen, LBad: INyxDocumentationRegistration;
  LAlternate: TAlternateRegistration;
  LAlternateLease: INyxDocumentationRegistration;
  LOverlay: TAlternateCatalog;
  LOverlayLease: INyxCatalogSnapshot;
  LWrongCallbacks: TAlternateCallbacks;
  LWrongCallbackLease: INyxCallbackSnapshot;
  LReentrant: TReentrantCatalog;
  LReentrantLease: INyxCatalogSnapshot;
  LInitial, LCurrent, LNext: INyxDocumentationIndex;
  LMembers: INyxDocumentationMembers;
  LGeneration: TNyxDocumentationGeneration;
  LInfo: TNyxComponentInfo;
  LScope: INyxCancellationScope;
  LDefaults: TNyxDocumentationIndexLimits;
  LUnit, LFactory: TNyxDocumentationRef;
  LAt, LOrdinal, LRevision, LVersion: Integer;
  LRefused: Boolean;
  LWire: TNyxText;
  LDeclarations: TNyxDocumentationDeclarations;
  LRefusalChunks: TNyxDocumentationDeclarationChunks;
  LRefusalPackage: INyxDocumentationPackage;
begin
  LDefaults := TNyxDocumentationIndexLimits.Defaults;
  LCatalog := TNyxCatalog.Create;
  LRegistry := TNyxDocumentationRegistry.Create;
  try
    LSchemas := CaptureNyxSchemas;
    LCallbacks := CaptureNyxCallbacks;
    LOldCatalog := LCatalog.Capture;
    ExpectFixtureSourceRefusal(LCatalog, nil, False);
    { This intentionally incomplete package supplies only its unit owner. It
      cannot be used to claim a complete fixture, core or source-derived API. }
    SetLength(LRefusalChunks, 1);
    LRefusalChunks[0] := NewNyxDocumentationDeclarationChunk(
      NyxDocumentationUnit('nyx.test.documentation'), [NewNyxDocumentationType(
        NyxDocumentationUnit('nyx.test.documentation'), ndtUnit, 'Refusal input.')]);
    LRefusalPackage := NewNyxDocumentationPackage(LRefusalChunks,
      TNyxDocumentationContributionLimits.Defaults);
    ExpectFixtureSourceRefusal(LCatalog, LRefusalPackage, False);
    LInitial := LRegistry.Refresh(LSchemas, LCallbacks, LOldCatalog, 0);
    Check((LInitial.Version = 1) and (LInitial.DeclarationCount = 0),
      'Legacy registrations produce explicit coverage gaps, never invented declarations');
    Check(LInitial.UndocumentedCount = LInitial.BindingCount,
      'Every undocumented legacy row is counted');
    LGeneration := LInitial.Generation;
    Check((LGeneration.SchemaRevision = LSchemas.Revision) and
      (LGeneration.CallbackRevision = LCallbacks.Revision) and
      (LGeneration.CatalogIdentity = LOldCatalog.Identity) and
      (LGeneration.CatalogRevision = LOldCatalog.Revision), 'Generation owns all captured authorities');

    LBundle := MechanicsBundle;
    LUnit := NyxDocumentationUnit('registry.mechanics');
    LFactory := NyxDocumentationMember(LUnit, 'NewMechanics');
    Check((LBundle.DeclarationCount = 5) and (LBundle.ContentBytes > 0),
      'A complete bounded metadata bundle is independently owned');
    Check(LBundle.ForPrimary(LFactory).Primary.Same(LFactory),
      'Creator mapping retains the exact global factory owner');
    LAlternate := TAlternateRegistration.Create;
    LAlternateLease := LAlternate;
    LAlternate.Value := LBundle;
    LAlternate.ClaimedCount := LBundle.DeclarationCount;
    LFrozen := CaptureNyxDocumentationRegistration(LAlternateLease);
    LAlternate.Value := MechanicsBundle(1);
    Check(LFrozen.DeclarationContentAt(4) = LBundle.DeclarationContentAt(4),
      'Capture freezes alternate declarations and ignores claimed equality/byte getters');
    LAlternate.ClaimedCount := NyxDocumentationRegistrationMaximumDeclarations + 1;
    LAlternate.DeclarationReads := 0;
    LRefused := False;
    try
      CaptureNyxDocumentationRegistration(LAlternateLease);
    except
      on ENyxDocumentation do
        LRefused := True;
    end;
    Check(LRefused and (LAlternate.DeclarationReads = 0),
      'Oversized alternate counts refuse before requesting a declaration');
    LRefused := False;
    SetLength(LDeclarations, 2);
    LDeclarations[0] := LBundle.DeclarationAt(2);
    LDeclarations[1] := LDeclarations[0];
    try
      NewNyxDocumentationRegistration(LUnit, MechanicsType('Flag'), LDeclarations);
    except
      on ENyxDocumentation do
        LRefused := True;
    end;
    Check(LRefused, 'Duplicate identities within one registration refuse');
    LRefused := False;
    SetLength(LDeclarations, 1);
    try
      NewNyxDocumentationRegistration(NyxDocumentationUnit('other.unit'), MechanicsType('Flag'),
        LDeclarations);
    except
      on ENyxDocumentation do
        LRefused := True;
    end;
    Check(LRefused, 'Registration provenance cannot claim another unit');
    LRefused := False;
    try
      LBundle.ForPrimary(NyxDocumentationMember(MechanicsType('Base'), 'Echo', 'One'));
    except
      on ENyxDocumentation do
        LRefused := True;
    end;
    Check(LRefused, 'A class method cannot masquerade as a global creator primary');

    LCatalog.RegisterKind('registry-mechanics', 'Index mechanics', 'Custom', False, LBundle);
    LOrdinal := LCatalog.IndexOf('registry-mechanics');
    LCatalogCapture := LCatalog.Capture;
    Check(Supports(LCatalogCapture, INyxCatalogDocumentation, LCatalogDocs),
      'An actual catalog snapshot exposes optional typed declaration attachments');
    Check(LCatalogDocs.DocumentationAt(LOrdinal).Primary.Same(MechanicsType('Derived')),
      'Metadata discovery comes from the actual catalog registration');
    LInfo := LCatalogCapture.ItemAt(LOrdinal);
    LInfo.Title := 'A caller changed its copied row';
    Check(LCatalogCapture.ItemAt(LOrdinal).Title = 'Index mechanics', 'Catalog facts detach from callers');
    LCurrent := LRegistry.Refresh(LSchemas, LCallbacks, LCatalogCapture, 1);
    Check((LCurrent.DeclarationCount = 5) and (LInitial.DeclarationCount = 0),
      'Publication replaces only the local current owner and preserves retained indices');
    LGeneration := LCurrent.Generation;
    Check(not LGeneration.Same(LInitial.Generation), 'Registration changes its source generation');
    for LAt := 1 to LCurrent.DeclarationCount - 1 do
      Check(LCurrent.DeclarationAt(LAt - 1).Identity.Key < LCurrent.DeclarationAt(LAt).Identity.Key,
        'Declarations have deterministic qualified ordering');
    Check(LCurrent.FindDeclaration(NyxDocumentationType('REGISTRY.MECHANICS', 'flag')).Kind = ndtEnum,
      'Qualified Pascal identity is independent of spelling case');
    Check(Pos(#0, LCurrent.FindDeclaration(MechanicsType('Base')).Summary) > 0,
      'NUL, markup and exact Unicode prose remain inert metadata');
    LMembers := LCurrent.Members(MechanicsType('Derived'));
    Check((LMembers.Count = 2) and (LCurrent.Members(MechanicsType('Derived'), False).Count = 0),
      'Inherited queries traverse aliases without inventing locally declared members');
    Check(LMembers.ItemAt(0).Identity.Owner.Same(MechanicsType('Base')) and
      (LMembers.ItemAt(0).Identity.OverloadKey <> LMembers.ItemAt(1).Identity.OverloadKey),
      'Inherited overloads retain their original owners and exact alternatives');
    Check((LCurrent.FindMember(NyxDocumentationMember(MechanicsType('Base'), 'Echo', 'One')) <> nil) and
      (LCurrent.FindMember(NyxDocumentationMember(MechanicsType('Derived'), 'Echo', 'One')) = nil),
      'Exact member lookup never fabricates a derived declaration identity');
    LRefused := False;
    try
      LCurrent.BindingAt(LCurrent.BindingCount);
    except
      on ENyxDocumentation do
        LRefused := True;
    end;
    Check(LRefused, 'Index ordinal absence is explicit');
    LRefused := False;
    try
      LFrozen.DeclarationAt(-1);
    except
      on ENyxDocumentation do
        LRefused := True;
    end;
    Check(LRefused, 'Registration ordinal absence is explicit');
    LRefused := False;
    try
      LMembers.ItemAt(2);
    except
      on ENyxDocumentation do
        LRefused := True;
    end;
    Check(LRefused, 'Inherited member ordinals refuse outside their captured vector');

    LRevision := LCatalogCapture.Revision;
    LRefused := False;
    try
      LCatalog.RegisterKind('registry-mechanics', 'Duplicate', 'Custom', False, LBundle);
    except
      on ENyxModel do
        LRefused := True;
    end;
    Check(LRefused and (LCatalog.Capture.Revision = LRevision),
      'Legacy duplicate registration refuses before publishing a generation');
    LCatalog.Describe(NyxCustomKind('registry-mechanics'), pgOther, [], 'mechanics', 'Changed prose.');
    Check((LCatalog.Capture.Revision = LRevision + 1) and
      (LCatalogCapture.ItemAt(LOrdinal).Discovery.Description = ''),
      'Describe changes its catalog generation and preserves previous copied facts');
    LOtherCatalog := TNyxCatalog.Create;
    try
      Check(LOtherCatalog.Capture.Identity <> LCatalogCapture.Identity,
        'Independent catalogs never share generation identity merely because revisions match');
    finally
      LOtherCatalog.Free;
    end;
    LCatalog.RegisterKind('registry-mechanics-copy', 'Repeated metadata', 'Custom', False, LBundle);
    LCatalogCapture := LCatalog.Capture;
    LCurrent := LRegistry.Refresh(LSchemas, LCallbacks, LCatalogCapture, LRegistry.Version);
    Check(LCurrent.DeclarationCount = 5, 'Identical declarations from distinct real registrations deduplicate');

    LOverlay := TAlternateCatalog.Create;
    LOverlayLease := LOverlay;
    LOverlay.Base := LCatalogCapture;
    LOverlay.ReplaceIndex := LOrdinal;
    LOverlay.Replacement := MechanicsBundle(1);
    ExpectRefusal(LRegistry, LSchemas, LCallbacks, LOverlayLease, LDefaults,
      'Conflicting duplicate declarations preserve the previous index');
    LOverlay.ReplaceAll := True;
    LOverlay.Replacement := MechanicsBundle(2);
    ExpectRefusal(LRegistry, LSchemas, LCallbacks, LOverlayLease, LDefaults,
      'Base and alias cycles refuse without Pascal recursion or partial publication', nil, 'Cyclic');
    LOverlay.Replacement := MechanicsBundle(3);
    ExpectRefusal(LRegistry, LSchemas, LCallbacks, LOverlayLease, LDefaults,
      'Unresolved external type links refuse before publication', nil, 'Unresolved documentation');
    LOverlay.Replacement := MechanicsBundle(4);
    ExpectRefusal(LRegistry, LSchemas, LCallbacks, LOverlayLease, LDefaults,
      'An alias to an enum cannot masquerade as a class base');
    LOverlay.Replacement := MechanicsBundle(5);
    ExpectRefusal(LRegistry, LSchemas, LCallbacks, LOverlayLease, LDefaults,
      'A targetless alias builder cannot enter a complete index or displace its previous owner',
      nil, 'requires its target');
    LOverlay.ReplaceAll := False;
    SetLength(LDeclarations, LBundle.DeclarationCount);
    for LAt := 0 to LBundle.DeclarationCount - 1 do
      LDeclarations[LAt] := LBundle.DeclarationAt(LAt);
    LOverlay.Replacement := NewNyxDocumentationRegistration(LUnit, LBundle.Primary,
      LDeclarations, [ndplatformNative]);
    ExpectRefusal(LRegistry, LSchemas, LCallbacks, LOverlayLease, LDefaults,
      'Conflicting declared platform intent is not merged into invented availability');
    LOverlay.Replacement := LBundle;
    LOverlay.ChangeOnRead := True;
    ExpectRefusal(LRegistry, LSchemas, LCallbacks, LOverlayLease, LDefaults,
      'A changed source generation refuses at final admission');
    LOverlay.ChangeOnRead := False;

    LWrongCallbacks := TAlternateCallbacks.Create;
    LWrongCallbackLease := LWrongCallbacks;
    LWrongCallbacks.Value := LBundle.ForPrimary(LFactory);
    ExpectRefusal(LRegistry, LSchemas, LWrongCallbackLease, LCatalogCapture, LDefaults,
      'Callback mappings cannot claim a unit factory as their implementation API');
    ExpectRefusal(LRegistry, LSchemas, LCallbacks, LCatalogCapture, LDefaults.Declarations(1),
      'Declaration occurrences share an aggregate count across registries');
    ExpectRefusal(LRegistry, LSchemas, LCallbacks, LCatalogCapture, LDefaults.Bindings(1),
      'Legacy and decorated rows share an aggregate binding allowance');
    ExpectRefusal(LRegistry, LSchemas, LCallbacks, LCatalogCapture, LDefaults.Bytes(1),
      'Every captured metadata field shares aggregate bytes');
    ExpectRefusal(LRegistry, LSchemas, LCallbacks, LCatalogCapture, LDefaults.Work(1),
      'All phases share the same work allowance');
    LScope := NewNyxCancellationScope;
    LScope.Cancel;
    ExpectRefusal(LRegistry, LSchemas, LCallbacks, LCatalogCapture, LDefaults,
      'Cancelled inspection leaves the previous index exact', LScope);
    LAlternate.Value := LBundle;
    LAlternate.ClaimedCount := LBundle.DeclarationCount;
    LAlternate.Cancellation := NewNyxCancellationScope;
    LOverlay.Replacement := LAlternateLease;
    ExpectRefusal(LRegistry, LSchemas, LCallbacks, LOverlayLease, LDefaults,
      'Mid-capture cancellation publishes no partial index', LAlternate.Cancellation);
    LAlternate.Cancellation := nil;
    LRefused := False;
    try
      LRegistry.Refresh(LSchemas, LCallbacks, LCatalogCapture, LRegistry.Version - 1);
    except
      on ENyxDocumentation do
        LRefused := True;
    end;
    Check(LRefused and (LRegistry.Current = LCurrent), 'Stale local publication versions refuse');

    LReentrant := TReentrantCatalog.Create;
    LReentrantLease := LReentrant;
    LReentrant.Registry := LRegistry;
    LReentrant.Base := LCatalogCapture;
    LReentrant.Schemas := LSchemas;
    LReentrant.Callbacks := LCallbacks;
    LVersion := LRegistry.Version;
    LRefused := False;
    try
      LRegistry.Refresh(LSchemas, LCallbacks, LReentrantLease, LVersion);
    except
      on ENyxDocumentation do
        LRefused := True;
    end;
    Check(LRefused and (LRegistry.Version = LVersion + 1),
      'Outer refresh refuses after a completed reentrant publication changes the expected version');
    LCurrent := LRegistry.Current;
    LReentrantLease := nil;

    RegisterNyxSchema(NyxCustomKind('registry-mechanics'), [], [], LBundle);
    LNewSchemas := CaptureNyxSchemas;
    LOverlay.Replacement := LBundle.ForPrimary(LFactory);
    LOverlay.ReplaceAll := True;
    ExpectRefusal(LRegistry, LNewSchemas, LCallbacks, LOverlayLease, LDefaults,
      'Schema and catalog for the same creator must agree on the declared public API');
    LWire := LNewSchemas.ToData.ToJSON;
    LTransported := ReadNyxSchemaSnapshot(LNewSchemas.ToData);
    Check(LTransported.ToData.ToJSON = LWire, 'Documentation attachments never change schema transport bytes');
    Check(Supports(LTransported, INyxSchemaDocumentation, LSchemaDocs),
      'A decoded schema explicitly reports its local documentation capability boundary');
    Check(not LSchemaDocs.HasLocalDocumentation, 'Wire metadata cannot invent a local declaration generation');
    ExpectRefusal(LRegistry, LTransported, LCallbacks, LCatalogCapture, LDefaults,
      'Discovery explicitly refuses transport-only schema generations');

    { The genuine extension uses actual callback/control/global metadata. Its
      missing core dependencies are retained, not replaced by mechanics types. }
    RegisterDocumentationFixture(LCatalog);
    LNewSchemas := CaptureNyxSchemas;
    LNewCallbacks := CaptureNyxCallbacks;
    LAlternateCapture := LCatalog.Capture;
    ExpectFixtureSourceRefusal(LCatalog, LRefusalPackage, True);
    Check(Supports(LNewSchemas, INyxSchemaInspection, LSchemaInspection),
      'An actual creator snapshot exposes typed schema inspection');
    Check(Supports(LNewSchemas, INyxSchemaDocumentation, LSchemaDocs),
      'That same creator snapshot exposes optional declaration attachments');
    LAt := LSchemaInspection.FindKind(NyxCustomKind(DocumentationFixtureKind));
    LBad := LSchemaDocs.DocumentationAt(LAt);
    Check((LBad <> nil) and (LBad.DeclarationCount = 4) and
      LBad.Primary.Same(NyxDocumentationMember(NyxDocumentationUnit('nyx.test.documentation'), 'NewDocumentationBadge')),
      'Actual schema discovery retains the real global creator and complete extension declarations');
    Check(Supports(LAlternateCapture, INyxCatalogDocumentation, LCatalogDocs),
      'The captured extension palette exposes its typed declaration capability');
    Check(LCatalogDocs.DocumentationAt(LCatalog.IndexOf(DocumentationFixtureKind)).Primary.Same(LBad.Primary),
      'Actual creator schema and catalog use the same declaration-owned metadata');
    Check(Supports(LNewCallbacks, INyxCallbackDocumentation, LCallbackDocs),
      'The captured callback registry exposes optional declaration attachments');
    LAt := LNewCallbacks.Count - 1;
    Check(LCallbackDocs.DocumentationAt(LAt).Primary.Same(
      NyxDocumentationType('nyx.test.documentation', 'TDocumentationCallback')),
      'Actual callback metadata names its class explicitly without constructing it');
    LRevision := LAlternateCapture.Revision;
    RegisterDocumentationFixture(LCatalog);
    Check((LCatalog.Capture.Revision = LRevision) and
      (CaptureNyxCallbacks.Revision = LNewCallbacks.Revision) and
      (CaptureNyxSchemas.Revision = LNewSchemas.Revision), 'Repeated extension setup does not register a second help inventory');
    ExpectRefusal(LRegistry, LNewSchemas, LNewCallbacks, LAlternateCapture, LDefaults,
      'The complete real extension refuses its currently undocumented core closure',
      nil, 'Unresolved documentation');
    Check((LRegistry.Current = LCurrent) and
      (LBad.DeclarationAt(1).BaseAt(0).UnitName = 'nyx.controls') and
      (LBad.DeclarationAt(3).BaseAt(0).UnitName = 'nyx.events'),
      'Prior valid index and exact external core links survive the failed genuine extension gate');
    Check((DocumentationControlsConstructed = 0) and (DocumentationControlsDestroyed = 0) and
      (DocumentationCallbacksConstructed = 0) and (DocumentationCallbacksInvoked = 0) and
      (DocumentationGlobalFactoryCalls = 0) and (DocumentationGlobalRoutineCalls = 0),
      'Inspection never constructs or executes the genuine control, callback, factory or routines');
    LCatalog.Free;
    LCatalog := nil;
    LNext := LRegistry.Current;
    Check((LNext.DeclarationCount = 5) and (LMembers.Count = 2) and
      (LNext.FindMember(NyxDocumentationMember(MechanicsType('Base'), 'Echo', 'Two')) <> nil),
      'Index/member captures remain valid after their mutable catalog retires');
  finally
    LCatalog.Free;
    LRegistry.Free;
  end;
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' documentation registry checks';
    {$else}
    WriteLn('PASS ', GChecks, ' documentation registry checks');
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
