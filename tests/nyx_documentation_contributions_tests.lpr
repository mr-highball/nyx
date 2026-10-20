{ Canonical source contributions; synthetic mechanics only. MIT, see LICENSE. }
program nyx_documentation_contributions_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.types, nyx.scheduler, nyx.catalog, nyx.schema, nyx.callbacks,
  nyx.documentation, nyx.documentation.registration, nyx.documentation.declarations,
  nyx.documentation.registry, nyx.test.documentation {$ifdef PAS2JS}, Web{$endif};

type
  { Mutable source producer exercises detached capture. Claimed identity,
    content and byte receipts deliberately refuse: consumers must rebuild them. }
  TAlternateChunk = class(TInterfacedObject, INyxDocumentationDeclarationChunk)
  public
    Origin: TNyxDocumentationRef;
    Values: TNyxDocumentationDeclarations;
    CountClaim, Reads: Integer;
    Cancellation: INyxCancellationScope;
    destructor Destroy; override;
    function Identity: Integer;
    function OriginUnit: TNyxDocumentationRef;
    function Platforms: TNyxDocumentationPlatforms;
    function DeclarationCount: Integer;
    function DeclarationAt(AIndex: Integer): INyxDocumentationType;
    function DeclarationContentAt(AIndex: Integer): TNyxText;
    function ContentBytes: Integer;
  end;
  { A package supplier exposes source references and explicit limits only.
    Its derived lookup/count/identity claims must never become retained facts. }
  TAlternatePackage = class(TInterfacedObject, INyxDocumentationPackage)
  public
    Values: TNyxDocumentationDeclarationChunks;
    CountClaim, Reads, CancelAtRead: Integer;
    Bounds: TNyxDocumentationContributionLimits;
    Cancellation: INyxCancellationScope;
    ReplaceReference: Boolean;
    destructor Destroy; override;
    function Identity: Integer;
    function Limits: TNyxDocumentationContributionLimits;
    function ChunkCount: Integer;
    function ChunkAt(AIndex: Integer): INyxDocumentationDeclarationChunk;
    function DeclarationCount: Integer;
    function FindDeclaration(const AIdentity: TNyxDocumentationRef): INyxDocumentationType;
    function FindContent(const AIdentity: TNyxDocumentationRef): TNyxText;
  end;
  { A synchronous caller may cancel inside borrowed field admission; retaining
    an already captured source must still observe cancellation before return. }
  TCancellingAdmission = class(TInterfacedObject, INyxDocumentationAdmission)
  public
    Scope: INyxCancellationScope;
    procedure AdmitField(ABytes: Integer);
  end;
  { Optional alternate binding may contradict its owner or change while its
    primary view is read. The inspector must revalidate instead of trusting it. }
  TAlternateBinding = class(TInterfacedObject, INyxDocumentationRegistration,
    INyxDocumentationContributionBinding)
  public
    Value: INyxDocumentationRegistration;
    PackageValue: INyxDocumentationPackage;
    ChangedPackage: INyxDocumentationPackage;
    PrimaryValue: TNyxDocumentationRef;
    DeclarationValue: INyxDocumentationType;
    ChangePrimary: Boolean;
    DeclarationReads: Integer;
    destructor Destroy; override;
    function PackageOwner: INyxDocumentationPackage;
    function OriginUnit: TNyxDocumentationRef;
    function Primary: TNyxDocumentationRef;
    function Platforms: TNyxDocumentationPlatforms;
    function DeclarationCount: Integer;
    function DeclarationAt(AIndex: Integer): INyxDocumentationType;
    function DeclarationContentAt(AIndex: Integer): TNyxText;
    function ContentBytes: Integer;
    function ForPrimary(const APrimary: TNyxDocumentationRef): INyxDocumentationRegistration;
  end;
  { A genuine alternate captured catalog supplies its attachments directly.
    This exercises index admission rather than an earlier catalog capture that
    would already have frozen the mutable binding/package/chunk suppliers.
    One complete ordinal permutation puts the two controlled rows first while
    retaining every other real backing row in its original relative order. }
  TAlternateCatalog = class(TInterfacedObject, INyxCatalogSnapshot, INyxCatalogDocumentation)
  public
    Value: INyxCatalogSnapshot;
    Attachments: TNyxDocumentationRegistrations;
    Ordinals: array of Integer;
    DocumentationReads: Integer;
    function Identity: Integer;
    function Revision: Integer;
    function Count: Integer;
    function ItemAt(AIndex: Integer): TNyxComponentInfo;
    function DocumentationAt(AIndex: Integer): INyxDocumentationRegistration;
  end;
  { Field counts observe both a fresh capture and a canonical retained capture;
    their work receipts must include every framed field, not one bulk call. }
  TCountingAdmission = class(TInterfacedObject, INyxDocumentationAdmission)
  public
    Fields, Bytes: Integer;
    procedure AdmitField(ABytes: Integer);
  end;

var
  GChecks, GChunkSuppliersDestroyed, GPackageSuppliersDestroyed, GBindingSuppliersDestroyed: Integer;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin
  Inc(GChecks);

  if not AValue then
    raise Exception.Create(AReason);
end;

function SourceUnit: TNyxDocumentationRef;
begin
  Result := NyxDocumentationUnit('mechanics.contributions');
end;

function ProductRef(AIndex: Integer): TNyxDocumentationRef;
begin
  Result := NyxDocumentationType('mechanics.contributions', 'IProduct' + IntToStr(AIndex));
end;

function FactoryRef(AIndex: Integer): TNyxDocumentationRef;
begin
  Result := NyxDocumentationMember(SourceUnit, 'NewProduct' + IntToStr(AIndex));
end;

{ This complete synthetic unit descriptor owns all81 factory members together.
  These are algorithm inputs, never descriptors pretending to document real
  System/core types or an actual generated control implementation. }
function SourceDeclarations(AChange: Integer = 0): TNyxDocumentationDeclarations;
var
  LAt: Integer;
  LUnit: INyxDocumentationType;
  LSummary: TNyxText;
begin
  Result := nil;
  SetLength(Result, 82);
  LUnit := NewNyxDocumentationType(SourceUnit, ndtUnit, 'Synthetic complete factory unit.');
  for LAt := 0 to 80 do
  begin
    LSummary := 'Synthetic product interface ' + IntToStr(LAt);

    if (AChange = 1) and (LAt = 0) then
      LSummary := 'Conflicting source fact.';
    Result[LAt + 1] := NewNyxDocumentationType(ProductRef(LAt), ndtInterface, LSummary);
    LUnit := LUnit.WithMember(NewNyxDocumentationMember(FactoryRef(LAt), ndmFactory,
      'Keep the actual declared synthetic result.').WithSignature(
        NewNyxDocumentationSignature([], ProductRef(LAt))));
  end;

  if AChange = 2 then
    LUnit := LUnit.WithRelated(NyxDocumentationType('missing.owner', 'StillMissing'));
  Result[0] := LUnit.WithExample(NyxDocumentationExample('Inert text', '// <script>' + #0#13#10 + 'Ω'));
end;

function SourceChunk(AChange: Integer = 0): INyxDocumentationDeclarationChunk;
begin
  Result := NewNyxDocumentationDeclarationChunk(SourceUnit, SourceDeclarations(AChange),
    [ndplatformNative, ndplatformBrowser]);
end;

function PackageFor(const AChunk: INyxDocumentationDeclarationChunk): INyxDocumentationPackage;
var
  LChunks: TNyxDocumentationDeclarationChunks;
begin
  SetLength(LChunks, 1);
  LChunks[0] := AChunk;
  Result := NewNyxDocumentationPackage(LChunks, TNyxDocumentationContributionLimits.Defaults);
end;

{ Actual catalog registration stores package bindings, without constructing
  product controls or resolving an implementation class from the kind text. }
procedure RegisterBindings(ACatalog: TNyxCatalog;
  const AFirst: INyxDocumentationPackage; const ASecond: INyxDocumentationPackage = nil);
var
  LAt: Integer;
  LPackage: INyxDocumentationPackage;
begin
  for LAt := 0 to 80 do
  begin
    LPackage := AFirst;

    if (ASecond <> nil) and (LAt >= 40) then
      LPackage := ASecond;
    ACatalog.RegisterKind('contribution-product-' + IntToStr(LAt),
      'Synthetic product', 'Custom', False,
      NewNyxDocumentationBinding(LPackage, FactoryRef(LAt), [ndplatformNative, ndplatformBrowser]));
  end;
end;

procedure RefuseRefresh(ARegistry: TNyxDocumentationRegistry;
  ACatalog: TNyxCatalog; const ALimits: TNyxDocumentationIndexLimits;
  const AFragment: TNyxText; const ACancellation: INyxCancellationScope = nil);
var
  LBefore: INyxDocumentationIndex;
  LVersion: Integer;
  LRefused: Boolean;
begin
  LBefore := ARegistry.Current;
  LVersion := ARegistry.Version;
  LRefused := False;
  try
    ARegistry.Refresh(CaptureNyxSchemas, CaptureNyxCallbacks, ACatalog.Capture,
      LVersion, ALimits, ACancellation);
  except
    on E: ENyxDocumentation do
      LRefused := (AFragment = '') or (Pos(AFragment, E.Message) > 0);
  end;
  Check(LRefused and (ARegistry.Current = LBefore) and (ARegistry.Version = LVersion),
    'Atomic source publication refusal: ' + AFragment);
end;

destructor TAlternateChunk.Destroy;
begin
  Inc(GChunkSuppliersDestroyed);
  inherited Destroy;
end;

function TAlternateChunk.Identity: Integer;
begin
  Result := 0;
  raise Exception.Create('Chunk identity claim must not be read');
end;

function TAlternateChunk.OriginUnit: TNyxDocumentationRef;
begin
  Result := Origin;
end;

function TAlternateChunk.Platforms: TNyxDocumentationPlatforms;
begin
  Result := [ndplatformNative, ndplatformBrowser];
end;

function TAlternateChunk.DeclarationCount: Integer;
begin
  Result := CountClaim;
end;

function TAlternateChunk.DeclarationAt(AIndex: Integer): INyxDocumentationType;
begin
  Inc(Reads);

  if Cancellation <> nil then
    Cancellation.Cancel;
  Result := Values[AIndex];
end;

function TAlternateChunk.DeclarationContentAt(AIndex: Integer): TNyxText;
begin
  Result := '';
  raise Exception.Create('Chunk content claim must not be read');
end;

function TAlternateChunk.ContentBytes: Integer;
begin
  Result := 0;
  raise Exception.Create('Chunk byte claim must not be read');
end;

destructor TAlternatePackage.Destroy;
begin
  Inc(GPackageSuppliersDestroyed);
  inherited Destroy;
end;

function TAlternatePackage.Identity: Integer;
begin
  Result := 0;
  raise Exception.Create('Package identity claim must not be read');
end;

function TAlternatePackage.Limits: TNyxDocumentationContributionLimits;
begin
  Result := Bounds;
end;

function TAlternatePackage.ChunkCount: Integer;
begin
  Result := CountClaim;
end;

function TAlternatePackage.ChunkAt(AIndex: Integer): INyxDocumentationDeclarationChunk;
begin
  Inc(Reads);

  if (Cancellation <> nil) and ((CancelAtRead = 0) or (Reads = CancelAtRead)) then
    Cancellation.Cancel;
  Result := Values[AIndex];

  if ReplaceReference then
    Values[AIndex] := nil;
end;

procedure TCancellingAdmission.AdmitField(ABytes: Integer);
begin
  Scope.Cancel;
end;

function TAlternatePackage.DeclarationCount: Integer;
begin
  Result := 0;
  raise Exception.Create('Package count claim must not be read');
end;

function TAlternatePackage.FindDeclaration(const AIdentity: TNyxDocumentationRef): INyxDocumentationType;
begin
  Result := nil;
  raise Exception.Create('Package lookup claim must not be read');
end;

function TAlternatePackage.FindContent(const AIdentity: TNyxDocumentationRef): TNyxText;
begin
  Result := '';
  raise Exception.Create('Package content claim must not be read');
end;

function TAlternateBinding.PackageOwner: INyxDocumentationPackage;
begin
  Result := PackageValue;
end;

destructor TAlternateBinding.Destroy;
begin
  Inc(GBindingSuppliersDestroyed);
  inherited Destroy;
end;

function TAlternateBinding.OriginUnit: TNyxDocumentationRef;
begin
  Result := Value.OriginUnit;
end;

function TAlternateBinding.Primary: TNyxDocumentationRef;
begin
  Result := PrimaryValue;
end;

function TAlternateBinding.Platforms: TNyxDocumentationPlatforms;
begin
  Result := Value.Platforms;
end;

function TAlternateBinding.DeclarationCount: Integer;
begin
  Result := 1;
end;

function TAlternateBinding.DeclarationAt(AIndex: Integer): INyxDocumentationType;
begin
  Inc(DeclarationReads);

  if ChangePrimary then
    PrimaryValue := FactoryRef(1);

  if ChangedPackage <> nil then
    PackageValue := ChangedPackage;
  Result := DeclarationValue;
end;

function TAlternateCatalog.Identity: Integer;
begin
  Result := Value.Identity;
end;

function TAlternateCatalog.Revision: Integer;
begin
  Result := Value.Revision;
end;

function TAlternateCatalog.Count: Integer;
begin
  Result := Value.Count;
end;

function TAlternateCatalog.ItemAt(AIndex: Integer): TNyxComponentInfo;
begin
  Result := Value.ItemAt(Ordinals[AIndex]);
end;

function TAlternateCatalog.DocumentationAt(AIndex: Integer): INyxDocumentationRegistration;
begin
  Inc(DocumentationReads);
  Result := Attachments[Ordinals[AIndex]];
end;

procedure TCountingAdmission.AdmitField(ABytes: Integer);
begin
  Inc(Fields);
  Inc(Bytes, ABytes);
end;

function TAlternateBinding.DeclarationContentAt(AIndex: Integer): TNyxText;
begin
  Result := '';
  raise Exception.Create('Binding content claim must not be read');
end;

function TAlternateBinding.ContentBytes: Integer;
begin
  Result := 0;
  raise Exception.Create('Binding byte claim must not be read');
end;

function TAlternateBinding.ForPrimary(const APrimary: TNyxDocumentationRef): INyxDocumentationRegistration;
begin
  Result := nil;
  raise Exception.Create('Capture must not call alternate ForPrimary');
end;

{ Lexical return retires compiler-held interface temporaries, making supplier
  destruction observable without assuming an assignment releases a temporary. }
function FreezeAlternateChunk: INyxDocumentationDeclarationChunk;
var
  LOwner: TAlternateChunk;
  LLease: INyxDocumentationDeclarationChunk;
begin
  LOwner := TAlternateChunk.Create;
  LLease := LOwner;
  LOwner.Origin := SourceUnit;
  LOwner.Values := SourceDeclarations;
  LOwner.CountClaim := Length(LOwner.Values);
  Result := CaptureNyxDocumentationDeclarationChunk(LLease);
  LOwner.Values[1] := NewNyxDocumentationType(ProductRef(0), ndtInterface, 'Mutated after capture.');
end;

function FreezeAlternatePackage(const AChunk: INyxDocumentationDeclarationChunk): INyxDocumentationPackage;
var
  LOwner: TAlternatePackage;
  LLease: INyxDocumentationPackage;
begin
  LOwner := TAlternatePackage.Create;
  LLease := LOwner;
  LOwner.Bounds := TNyxDocumentationContributionLimits.Defaults;
  LOwner.CountClaim := 1;
  SetLength(LOwner.Values, 1);
  LOwner.Values[0] := AChunk;
  Result := CaptureNyxDocumentationPackage(LLease);
  LOwner.Values[0] := nil;
end;

{ An alternate binding likewise retires before its captured view is inspected. }
function FreezeAlternateBinding(const AValue: INyxDocumentationRegistration): INyxDocumentationRegistration;
var
  LOwner: TAlternateBinding;
  LLease: INyxDocumentationRegistration;
  LCapability: INyxDocumentationContributionBinding;
begin
  Supports(AValue, INyxDocumentationContributionBinding, LCapability);
  LOwner := TAlternateBinding.Create;
  LLease := LOwner;
  LOwner.Value := AValue;
  LOwner.PackageValue := LCapability.PackageOwner;
  LOwner.PrimaryValue := AValue.Primary;
  LOwner.DeclarationValue := AValue.DeclarationAt(0);
  Result := CaptureNyxDocumentationRegistration(LLease);
end;

{ Index limits are deliberately smaller than the alternate package's defaults.
  A preceding registered source consumes real allowance before the alternate
  supplier is entered; counters distinguish pre-copy refusal from late refusal. }
procedure CheckRemainingIndexAdmission(const ASourceChunk: INyxDocumentationDeclarationChunk);
var
  LRegistry: TNyxDocumentationRegistry;
  LCatalog: TNyxCatalog;
  LCatalogOwner: TAlternateCatalog;
  LCatalogLease: INyxCatalogSnapshot;
  LBackingDocumentation: INyxCatalogDocumentation;
  LChunkOwner: TAlternateChunk;
  LChunkLease: INyxDocumentationDeclarationChunk;
  LPackageOwner: TAlternatePackage;
  LPackageLease: INyxDocumentationPackage;
  LBindingOwner: TAlternateBinding;
  LBindingLease: INyxDocumentationRegistration;
  LPrefixOrigin, LPrefixType: TNyxDocumentationRef;
  LPrefixDeclarations: TNyxDocumentationDeclarations;
  LPrefixPackage: INyxDocumentationPackage;
  LPrefixBinding: INyxDocumentationRegistration;
  LIndex, LBefore: INyxDocumentationIndex;
  LLimits: TNyxDocumentationIndexLimits;
  LVersion, LAt, LBytes, LPrefixOrdinal, LAlternateOrdinal, LSlot: Integer;
  LContent: TNyxText;
  LRefused: Boolean;
begin
  LPrefixOrigin := NyxDocumentationUnit('mechanics.admission');
  LPrefixType := NyxDocumentationType('mechanics.admission', 'IConsumed');
  SetLength(LPrefixDeclarations, 1);
  LPrefixDeclarations[0] := NewNyxDocumentationType(LPrefixType, ndtInterface,
    'This preceding source consumes aggregate allowance.');
  LPrefixPackage := PackageFor(NewNyxDocumentationDeclarationChunk(LPrefixOrigin, LPrefixDeclarations));
  LPrefixBinding := NewNyxDocumentationBinding(LPrefixPackage, LPrefixType);
  LRegistry := TNyxDocumentationRegistry.Create;
  LCatalog := TNyxCatalog.Create;
  try
    LCatalog.RegisterKind('consumed-source', 'Consumed source', 'Custom', False, LPrefixBinding);
    LBefore := LRegistry.Refresh(CaptureNyxSchemas, CaptureNyxCallbacks, LCatalog.Capture, 0);
    LVersion := LRegistry.Version;
    LCatalog.RegisterKind('alternate-source', 'Alternate source', 'Custom', False);
    LCatalogOwner := TAlternateCatalog.Create;
    LCatalogLease := LCatalogOwner;
    LCatalogOwner.Value := LCatalog.Capture;
    Check(Supports(LCatalogOwner.Value, INyxCatalogDocumentation, LBackingDocumentation),
      'Alternate snapshot retains the backing catalog documentation authority');
    SetLength(LCatalogOwner.Attachments, LCatalogOwner.Value.Count);
    SetLength(LCatalogOwner.Ordinals, LCatalogOwner.Value.Count);
    for LAt := 0 to LCatalogOwner.Value.Count - 1 do
      LCatalogOwner.Attachments[LAt] := LBackingDocumentation.DocumentationAt(LAt);
    LPrefixOrdinal := LCatalog.IndexOf('consumed-source');
    LAlternateOrdinal := LCatalog.IndexOf('alternate-source');
    Check((LPrefixOrdinal >= 0) and (LAlternateOrdinal >= 0) and
      (LPrefixOrdinal <> LAlternateOrdinal), 'Controlled attachments keep their actual registered kind ordinals');
    LCatalogOwner.Attachments[LPrefixOrdinal] := LPrefixBinding;
    LCatalogOwner.Ordinals[0] := LPrefixOrdinal;
    LCatalogOwner.Ordinals[1] := LAlternateOrdinal;
    LSlot := 2;
    for LAt := 0 to LCatalogOwner.Value.Count - 1 do
    begin

      if (LAt <> LPrefixOrdinal) and (LAt <> LAlternateOrdinal) then
      begin
        LCatalogOwner.Ordinals[LSlot] := LAt;
        Inc(LSlot);
      end;
    end;
    Check(LSlot = LCatalogOwner.Value.Count,
      'Alternate snapshot order retains every real backing row exactly once');
    LChunkOwner := TAlternateChunk.Create;
    LChunkLease := LChunkOwner;
    LChunkOwner.Origin := SourceUnit;
    LChunkOwner.Values := SourceDeclarations;
    LChunkOwner.CountClaim := Length(LChunkOwner.Values);
    LPackageOwner := TAlternatePackage.Create;
    LPackageLease := LPackageOwner;
    LPackageOwner.Bounds := TNyxDocumentationContributionLimits.Defaults;
    LPackageOwner.CountClaim := 1;
    SetLength(LPackageOwner.Values, 1);
    LPackageOwner.Values[0] := LChunkLease;
    LBindingOwner := TAlternateBinding.Create;
    LBindingLease := LBindingOwner;
    LBindingOwner.Value := NewNyxDocumentationBinding(PackageFor(ASourceChunk), FactoryRef(0));
    LBindingOwner.PackageValue := LPackageLease;
    LBindingOwner.PrimaryValue := FactoryRef(0);
    LBindingOwner.DeclarationValue := LChunkOwner.Values[0];
    LCatalogOwner.Attachments[LAlternateOrdinal] := LBindingLease;
    for LAt := 0 to 2 do
    begin
      LLimits := TNyxDocumentationIndexLimits.Defaults;
      case LAt of
        0: LLimits := LLimits.Declarations(82);
        1: LLimits := LLimits.Bytes(ASourceChunk.ContentBytes div 2);
        2: LLimits := LLimits.Work(512);
      end;
      LChunkOwner.Reads := 0;
      LPackageOwner.Reads := 0;
      LBindingOwner.DeclarationReads := 0;
      LCatalogOwner.DocumentationReads := 0;
      LRefused := False;
      LIndex := nil;
      try
        LIndex := LRegistry.Refresh(CaptureNyxSchemas, CaptureNyxCallbacks,
          LCatalogLease, LVersion, LLimits);
      except
        on ENyxDocumentation do
          LRefused := True;
      end;
      Check(LRefused and (LIndex = nil) and (LRegistry.Current = LBefore) and
        (LRegistry.Version = LVersion), 'Remaining index allowance refuses atomically ' + IntToStr(LAt));
      Check((LCatalogOwner.DocumentationReads = 2) and (LPackageOwner.Reads > 0) and
        (LBindingOwner.DeclarationReads = 0),
        'Remaining index allowance reaches alternate capture and refuses before duplicate primary copying ' + IntToStr(LAt));

      if LAt = 0 then
        Check(LChunkOwner.Reads = 0, 'Remaining declaration allowance reserves before any alternate declaration getter')
      else
        Check((LChunkOwner.Reads > 0) and (LChunkOwner.Reads < LChunkOwner.CountClaim),
          'Remaining byte/work allowance stops source getters before following declarations ' + IntToStr(LAt));
    end;
    LIndex := LRegistry.Refresh(CaptureNyxSchemas, CaptureNyxCallbacks,
      LCatalogLease, LVersion, TNyxDocumentationIndexLimits.Defaults.Declarations(83));
    Check((LIndex.DeclarationCount = 83) and
      (LIndex.FindDeclaration(SourceUnit).MemberCount = 81) and
      (LBindingOwner.DeclarationReads = 1),
      'Adequate remaining allowance captures the complete alternate source and validates its primary once');
    Check((LIndex.BindingCount = LBefore.BindingCount + 1) and
      (LIndex.UndocumentedCount = LBefore.UndocumentedCount),
      'Complete alternate traversal preserves every backing binding and undocumented row');
    LBytes := 0;
    CaptureNyxDocumentationDeclaration(LIndex.FindDeclaration(SourceUnit), SourceUnit, LContent, LBytes);
    Check(LContent = ASourceChunk.DeclarationContentAt(0),
      'Forwarded admission preserves exact complete source framing');
  finally
    LCatalog.Free;
    LRegistry.Free;
  end;
end;

procedure CheckCapturedFieldWork;
var
  LFresh, LRetained: TCountingAdmission;
  LFreshLease, LRetainedLease: INyxDocumentationAdmission;
  LChunk, LRetainedChunk: INyxDocumentationDeclarationChunk;
begin
  LFresh := TCountingAdmission.Create;
  LFreshLease := LFresh;
  LRetained := TCountingAdmission.Create;
  LRetainedLease := LRetained;
  LChunk := NewNyxDocumentationDeclarationChunk(SourceUnit, SourceDeclarations, [], nil, LFreshLease);
  LRetainedChunk := CaptureNyxDocumentationDeclarationChunk(LChunk, nil, LRetainedLease);
  Check(SameNyxDocumentationChunk(LChunk, LRetainedChunk) and (LFresh.Fields > 1) and
    (LRetained.Fields = LFresh.Fields) and (LRetained.Bytes = LFresh.Bytes),
    'Canonical admission replays every privately captured field work receipt and exact bytes');
end;

procedure Run;
var
  LChunk, LCopy, LFrozenChunk, LShapeChunk: INyxDocumentationDeclarationChunk;
  LPackage, LOtherPackage, LCopyPackage, LFrozenPackage: INyxDocumentationPackage;
  LChunks: TNyxDocumentationDeclarationChunks;
  LDeclarations: TNyxDocumentationDeclarations;
  LBinding, LFrozenBinding, LLegacy: INyxDocumentationRegistration;
  LCapability: INyxDocumentationContributionBinding;
  LIntrinsic: INyxDocumentationIntrinsic;
  LTargets: INyxDocumentationAliasTargets;
  LGeneration, LOldGeneration: INyxDocumentationContributionGeneration;
  LIndex, LOldIndex: INyxDocumentationIndex;
  LRegistry: TNyxDocumentationRegistry;
  LCatalog, LSharedCatalog, LIndependentCatalog, LBadCatalog: TNyxCatalog;
  LAlternateChunk: TAlternateChunk;
  LAlternateChunkLease: INyxDocumentationDeclarationChunk;
  LAlternatePackage: TAlternatePackage;
  LAlternatePackageLease: INyxDocumentationPackage;
  LAlternateBinding: TAlternateBinding;
  LAlternateBindingLease: INyxDocumentationRegistration;
  LAdmission: TCancellingAdmission;
  LAdmissionLease: INyxDocumentationAdmission;
  LLimits: TNyxDocumentationContributionLimits;
  LScope: INyxCancellationScope;
  LAt, LBefore, LSourceIdentity: Integer;
  LRefused: Boolean;
begin
  LChunk := SourceChunk;
  CheckCapturedFieldWork;
  CheckRemainingIndexAdmission(LChunk);
  LPackage := PackageFor(LChunk);
  Check((LChunk.DeclarationCount = 82) and (LChunk.DeclarationAt(0).MemberCount = 81),
    'One complete synthetic unit retains all81 factories and distinct result interfaces');
  Check(SameNyxDocumentationChunk(LChunk, CaptureNyxDocumentationDeclarationChunk(LChunk)),
    'Verified canonical chunk capture preserves exact copied ownership');
  Check(SameNyxDocumentationPackage(LPackage, CaptureNyxDocumentationPackage(LPackage)),
    'Verified canonical package capture retains its immutable owner');
  LOtherPackage := PackageFor(LChunk);
  Check(not SameNyxDocumentationPackage(LPackage, LOtherPackage) and
    SameNyxDocumentationChunk(LPackage.ChunkAt(0), LOtherPackage.ChunkAt(0)),
    'Different package envelopes retain one exact shared source chunk');
  LCopy := SourceChunk;
  Check(not SameNyxDocumentationChunk(LChunk, LCopy) and
    (LChunk.DeclarationContentAt(0) = LCopy.DeclarationContentAt(0)),
    'Independently captured equivalent source has independent copied ownership');
  LCopyPackage := PackageFor(LCopy);
  LBefore := GChunkSuppliersDestroyed;
  LFrozenChunk := FreezeAlternateChunk;
  Check((GChunkSuppliersDestroyed = LBefore + 1) and
    (LFrozenChunk.DeclarationContentAt(1) = LChunk.DeclarationContentAt(1)),
    'Alternate chunk facts detach and its mutable supplier retires');
  LBefore := GPackageSuppliersDestroyed;
  LFrozenPackage := FreezeAlternatePackage(LChunk);
  Check((GPackageSuppliersDestroyed = LBefore + 1) and
    SameNyxDocumentationChunk(LFrozenPackage.ChunkAt(0), LChunk),
    'Alternate package captures its reference envelope without recopying canonical facts');
  LBinding := NewNyxDocumentationBinding(LPackage, FactoryRef(0), [ndplatformNative, ndplatformBrowser]);
  Check(Supports(LBinding, INyxDocumentationContributionBinding, LCapability) and
    SameNyxDocumentationPackage(LCapability.PackageOwner, LPackage) and
    (LBinding.DeclarationCount = 1) and (LBinding.DeclarationAt(0).MemberCount = 81),
    'Creator view references its package and complete unit, never a fabricated single-factory unit');
  Check((CaptureNyxDocumentationRegistration(LBinding) = LBinding) and
    LBinding.ForPrimary(FactoryRef(80)).Primary.Same(FactoryRef(80)),
    'Shared registration capture and ForPrimary preserve canonical package bindings');
  Check(Supports(LBinding.ForPrimary(ProductRef(0)), INyxDocumentationContributionBinding, LCapability) and
    SameNyxDocumentationChunk(LCapability.PackageOwner.ChunkAt(0), LChunk),
    'A type primary retains the same source owner instead of inventing an implementation class');
  LBefore := GBindingSuppliersDestroyed;
  LFrozenBinding := FreezeAlternateBinding(LBinding);
  Check((GBindingSuppliersDestroyed = LBefore + 1) and LFrozenBinding.Primary.Same(FactoryRef(0)),
    'Captured registration view retains no mutable binding supplier');
  SetLength(LDeclarations, 2);
  LDeclarations[0] := NewNyxDocumentationIntrinsic(
    NyxDocumentationType('mechanics.contribution.shapes', 'Flag'), ndifBoolean, 'Synthetic family.');
  LDeclarations[1] := NewNyxDocumentationTargetAlias(
    NyxDocumentationType('mechanics.contribution.shapes', 'FlagAlias'), 'Exact synthetic target.', [
      NyxDocumentationAliasTarget(npfAny, LDeclarations[0].Identity)]);
  LShapeChunk := NewNyxDocumentationDeclarationChunk(
    NyxDocumentationUnit('mechanics.contribution.shapes'), LDeclarations, [ndplatformNative]);
  Check(Supports(LShapeChunk.DeclarationAt(0), INyxDocumentationIntrinsic, LIntrinsic) and
    (LIntrinsic.Family = ndifBoolean) and
    Supports(LShapeChunk.DeclarationAt(1), INyxDocumentationAliasTargets, LTargets) and
    LTargets.TargetFor(npfAny).Same(LDeclarations[0].Identity),
    'Shared declaration framing/capture preserves specialized intrinsic and alias interfaces');
  LRefused := False;
  try
    NewNyxDocumentationBinding(PackageFor(LShapeChunk), LDeclarations[0].Identity,
      [ndplatformNative, ndplatformBrowser]);
  except
    on ENyxDocumentation do
      LRefused := True;
  end;
  Check(LRefused, 'A binding cannot promise a platform excluded by its explicit source declaration');

  LAlternateBinding := TAlternateBinding.Create;
  LAlternateBindingLease := LAlternateBinding;
  LAlternateBinding.Value := LBinding;
  LAlternateBinding.PackageValue := LPackage;
  LAlternateBinding.PrimaryValue := FactoryRef(0);
  LAlternateBinding.DeclarationValue := LChunk.DeclarationAt(0);
  LFrozenBinding := CaptureNyxDocumentationRegistration(LAlternateBindingLease);
  Check(LFrozenBinding.Primary.Same(FactoryRef(0)) and
    Supports(LFrozenBinding, INyxDocumentationContributionBinding, LCapability) and
    SameNyxDocumentationPackage(LCapability.PackageOwner, LPackage),
    'Alternate binding is rebuilt against its exact immutable package');
  LAlternateBinding.DeclarationValue := LChunk.DeclarationAt(1);
  LRefused := False;
  LFrozenBinding := nil;
  try
    LFrozenBinding := CaptureNyxDocumentationRegistration(LAlternateBindingLease);
  except
    on ENyxDocumentation do
      LRefused := True;
  end;
  Check(LRefused and (LFrozenBinding = nil), 'Contradictory primary view refuses atomically');
  LAlternateBinding.DeclarationValue := LChunk.DeclarationAt(0);
  LAlternateBinding.ChangePrimary := True;
  LRefused := False;
  try
    CaptureNyxDocumentationRegistration(LAlternateBindingLease);
  except
    on ENyxDocumentation do
      LRefused := True;
  end;
  Check(LRefused, 'A changed alternate binding primary is rejected before publication');
  LAlternateBinding.ChangePrimary := False;
  LAlternateBinding.PrimaryValue := FactoryRef(0);
  LAlternateBinding.ChangedPackage := LOtherPackage;
  LRefused := False;
  try
    CaptureNyxDocumentationRegistration(LAlternateBindingLease);
  except
    on ENyxDocumentation do
      LRefused := True;
  end;
  Check(LRefused, 'Changing to an equivalent package during binding capture revokes its exact source generation');

  for LAt := 0 to 6 do
  begin
    LRefused := False;
    try
      case LAt of
        0: LChunk.DeclarationAt(-1);
        1: LPackage.ChunkAt(1);
        2: LBinding.DeclarationAt(1);
        3: NewNyxDocumentationBinding(LPackage, SourceUnit);
        4: NewNyxDocumentationBinding(LPackage, FactoryRef(81));
        5: LBinding.ForPrimary(NyxDocumentationType('another.unit', 'Foreign'));
        6: NewNyxDocumentationDeclarationChunk(NyxDocumentationUnit('another.unit'), SourceDeclarations);
      end;
    except
      on ENyxDocumentation do
        LRefused := True;
    end;
    Check(LRefused, 'Invalid contribution identity/ordinal refuses ' + IntToStr(LAt));
  end;
  SetLength(LChunks, 2);
  LChunks[0] := LChunk;
  LChunks[1] := LChunk;
  LRefused := False;
  try
    NewNyxDocumentationPackage(LChunks, TNyxDocumentationContributionLimits.Defaults);
  except
    on ENyxDocumentation do
      LRefused := True;
  end;
  Check(LRefused, 'One flat package refuses a redundant identical source reference');
  SetLength(LChunks, 1);
  LChunks[0] := LChunk;
  LRefused := False;
  try
    NewNyxDocumentationPackage(LChunks, Default(TNyxDocumentationContributionLimits));
  except
    on ENyxDocumentation do
      LRefused := True;
  end;
  Check(LRefused, 'Zeroed contribution limits do not acquire implicit defaults');
  for LAt := 0 to 2 do
  begin
    LLimits := TNyxDocumentationContributionLimits.Defaults;
    case LAt of
      0: LLimits := LLimits.Declarations(81);
      1: LLimits := LLimits.Bytes(1);
      2: LLimits := LLimits.Work(1);
    end;
    SetLength(LChunks, 1);
    LChunks[0] := LChunk;
    LFrozenPackage := nil;
    LRefused := False;
    try
      LFrozenPackage := NewNyxDocumentationPackage(LChunks, LLimits);
    except
      on ENyxDocumentation do
        LRefused := True;
    end;
    Check(LRefused and (LFrozenPackage = nil), 'Caller-owned package ceiling refuses atomically ' + IntToStr(LAt));
  end;
  LAlternateChunk := TAlternateChunk.Create;
  LAlternateChunkLease := LAlternateChunk;
  LAlternateChunk.Origin := SourceUnit;
  LAlternateChunk.Values := SourceDeclarations;
  LAlternateChunk.CountClaim := 129;
  LRefused := False;
  try
    CaptureNyxDocumentationDeclarationChunk(LAlternateChunkLease);
  except
    on ENyxDocumentation do
      LRefused := True;
  end;
  Check(LRefused and (LAlternateChunk.Reads = 0), 'Oversized chunk refuses before row reads or copied allocation');
  LAlternateChunk.CountClaim := 82;
  LAlternateChunk.Cancellation := NewNyxCancellationScope;
  LFrozenChunk := nil;
  LRefused := False;
  try
    LFrozenChunk := CaptureNyxDocumentationDeclarationChunk(LAlternateChunkLease, LAlternateChunk.Cancellation);
  except
    on ENyxDocumentationCancelled do
      LRefused := True;
  end;
  Check(LRefused and (LFrozenChunk = nil), 'Mid-source cancellation retains no partial chunk');
  LAlternatePackage := TAlternatePackage.Create;
  LAlternatePackageLease := LAlternatePackage;
  LAlternatePackage.Bounds := TNyxDocumentationContributionLimits.Defaults;
  LAlternatePackage.CountClaim := 4097;
  LRefused := False;
  try
    CaptureNyxDocumentationPackage(LAlternatePackageLease);
  except
    on ENyxDocumentation do
      LRefused := True;
  end;
  Check(LRefused and (LAlternatePackage.Reads = 0), 'Oversized package refuses before source reference reads');
  LAlternatePackage.CountClaim := 1;
  SetLength(LAlternatePackage.Values, 1);
  LAlternatePackage.Values[0] := LChunk;
  LAlternatePackage.ReplaceReference := True;
  LFrozenPackage := nil;
  LRefused := False;
  try
    LFrozenPackage := CaptureNyxDocumentationPackage(LAlternatePackageLease);
  except
    on ENyxDocumentation do
      LRefused := True;
  end;
  Check(LRefused and (LFrozenPackage = nil), 'Changing a source reference during package capture refuses atomically');
  LAlternatePackage.ReplaceReference := False;
  LAlternatePackage.Values[0] := LChunk;
  LAlternatePackage.Reads := 0;
  LAlternatePackage.CancelAtRead := 2;
  LAlternatePackage.Cancellation := NewNyxCancellationScope;
  LFrozenPackage := nil;
  LRefused := False;
  try
    LFrozenPackage := CaptureNyxDocumentationPackage(LAlternatePackageLease, LAlternatePackage.Cancellation);
  except
    on ENyxDocumentationCancelled do
      LRefused := True;
  end;
  Check(LRefused and (LFrozenPackage = nil) and (LAlternatePackage.Reads = 2),
    'Cancellation from the final borrowed package getter publishes no retained owner');
  LScope := NewNyxCancellationScope;
  LAdmission := TCancellingAdmission.Create;
  LAdmissionLease := LAdmission;
  LAdmission.Scope := LScope;
  LFrozenChunk := nil;
  LRefused := False;
  try
    LFrozenChunk := CaptureNyxDocumentationDeclarationChunk(LChunk, LScope, LAdmissionLease);
  except
    on ENyxDocumentationCancelled do
      LRefused := True;
  end;
  Check(LRefused and (LFrozenChunk = nil), 'Cancellation inside retained-chunk admission is rechecked before publication');
  LScope := NewNyxCancellationScope;
  LScope.Cancel;
  LFrozenPackage := nil;
  LRefused := False;
  try
    LFrozenPackage := CaptureNyxDocumentationPackage(LPackage, LScope);
  except
    on ENyxDocumentationCancelled do
      LRefused := True;
  end;
  Check(LRefused and (LFrozenPackage = nil), 'Cancellation also refuses the canonical retained-owner path');

  LRegistry := TNyxDocumentationRegistry.Create;
  LCatalog := TNyxCatalog.Create;
  LSharedCatalog := TNyxCatalog.Create;
  LIndependentCatalog := TNyxCatalog.Create;
  LBadCatalog := TNyxCatalog.Create;
  try
    RegisterBindings(LCatalog, LPackage);
    LIndex := LRegistry.Refresh(CaptureNyxSchemas, CaptureNyxCallbacks, LCatalog.Capture,
      LRegistry.Version, TNyxDocumentationIndexLimits.Defaults.Declarations(82));
    Check((LIndex.DeclarationCount = 82) and (LIndex.FindDeclaration(SourceUnit).MemberCount = 81),
      'All81 real catalog registration references retain82 source declarations, without6642 copies');
    Check(Supports(LIndex, INyxDocumentationContributionGeneration, LGeneration) and
      (LGeneration.PackageCount = 1) and (LGeneration.ChunkCount = 1) and
      SameNyxDocumentationChunk(LGeneration.ChunkAt(0), LChunk),
      'Optional generation retains exact package/chunk identities independently of old authority records');
    LOldIndex := LIndex;
    LOldGeneration := LGeneration;
    RegisterBindings(LSharedCatalog, LPackage, LOtherPackage);
    LIndex := LRegistry.Refresh(CaptureNyxSchemas, CaptureNyxCallbacks, LSharedCatalog.Capture,
      LRegistry.Version, TNyxDocumentationIndexLimits.Defaults.Declarations(82));
    Check(Supports(LIndex, INyxDocumentationContributionGeneration, LGeneration) and
      (LGeneration.PackageCount = 2) and (LGeneration.ChunkCount = 1) and (LIndex.DeclarationCount = 82),
      'Two package envelopes sharing one exact core chunk charge its copied declarations once');
    Check((LOldGeneration.PackageCount = 1) and (LOldIndex.DeclarationCount = 82),
      'A later source generation preserves the earlier immutable package/index readers');
    RegisterBindings(LIndependentCatalog, LPackage, LCopyPackage);
    RefuseRefresh(LRegistry, LIndependentCatalog, TNyxDocumentationIndexLimits.Defaults.Declarations(82),
      'declaration occurrence');
    LIndex := LRegistry.Refresh(CaptureNyxSchemas, CaptureNyxCallbacks, LIndependentCatalog.Capture,
      LRegistry.Version, TNyxDocumentationIndexLimits.Defaults.Declarations(164));
    Check(Supports(LIndex, INyxDocumentationContributionGeneration, LGeneration) and
      (LGeneration.ChunkCount = 2) and (LIndex.DeclarationCount = 82),
      'Equal independently copied chunks charge fully before equivalent declarations deduplicate');
    RegisterBindings(LBadCatalog, LPackage, PackageFor(SourceChunk(1)));
    RefuseRefresh(LRegistry, LBadCatalog, TNyxDocumentationIndexLimits.Defaults, 'Conflicting documentation');
    LBadCatalog.Free;
    LBadCatalog := TNyxCatalog.Create;
    RegisterBindings(LBadCatalog, PackageFor(SourceChunk(2)));
    RefuseRefresh(LRegistry, LBadCatalog, TNyxDocumentationIndexLimits.Defaults, 'Unresolved documentation');
    RefuseRefresh(LRegistry, LCatalog, TNyxDocumentationIndexLimits.Defaults.Work(1), 'work');
    RefuseRefresh(LRegistry, LCatalog, TNyxDocumentationIndexLimits.Defaults.Bytes(1), '');
    RefuseRefresh(LRegistry, LCatalog, TNyxDocumentationIndexLimits.Defaults, '', LScope);

    { The qualified legacy bundle path still charges all82 declarations for
      each independently captured registration; it receives no owner shortcut. }
    LBadCatalog.Free;
    LBadCatalog := TNyxCatalog.Create;
    LDeclarations := SourceDeclarations;
    LLegacy := NewNyxDocumentationRegistration(SourceUnit, FactoryRef(0), LDeclarations,
      [ndplatformNative, ndplatformBrowser]);
    Check(not Supports(LLegacy, INyxDocumentationContributionBinding, LCapability),
      'Old bundle construction does not acquire a contribution capability');
    for LAt := 0 to 80 do
      LBadCatalog.RegisterKind('legacy-product-' + IntToStr(LAt), 'Synthetic legacy product', 'Custom', False,
        LLegacy.ForPrimary(FactoryRef(LAt)));
    RefuseRefresh(LRegistry, LBadCatalog, TNyxDocumentationIndexLimits.Defaults, 'declaration occurrence');

    LSourceIdentity := LGeneration.ChunkAt(1).Identity;
    LIndependentCatalog.Free;
    LIndependentCatalog := nil;
    LCopy := nil;
    LCopyPackage := nil;
    Check((LIndex.FindDeclaration(SourceUnit).MemberCount = 81) and
      (LGeneration.ChunkAt(1).Identity = LSourceIdentity),
      'Retained source generation survives its catalog and caller source variables retiring');
    for LAt := 0 to 1 do
    begin
      LRefused := False;
      try
        case LAt of
          0: LGeneration.PackageAt(-1);
          1: LGeneration.ChunkAt(LGeneration.ChunkCount);
        end;
      except
        on ENyxDocumentation do
          LRefused := True;
      end;
      Check(LRefused, 'Source generation ordinal refusal ' + IntToStr(LAt));
    end;

    { Preserve the real existing extension's complete missing-core refusal.
      No synthetic graph above substitutes for its actual source dependencies. }
    RegisterDocumentationFixture(LCatalog);
    RefuseRefresh(LRegistry, LCatalog, TNyxDocumentationIndexLimits.Defaults, 'Unresolved documentation');
    Check((DocumentationControlsConstructed = 0) and (DocumentationCallbacksConstructed = 0) and
      (DocumentationCallbacksInvoked = 0) and (DocumentationGlobalFactoryCalls = 0) and
      (DocumentationGlobalRoutineCalls = 0),
      'Source inspection and actual fixture registration execute no control, callback, factory or routine body');
  finally
    LBadCatalog.Free;
    LIndependentCatalog.Free;
    LSharedCatalog.Free;
    LCatalog.Free;
    LRegistry.Free;
  end;
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' documentation contribution checks';
    {$else}
    WriteLn('PASS ', GChecks, ' documentation contribution checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.textContent := 'FAIL ' + E.Message;
      {$else}
      WriteLn(StdErr, E.ClassName, ': ', E.Message, ' after ', GChecks, ' completed checks');
      DumpExceptionBackTrace(StdErr);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
