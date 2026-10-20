{ Immutable discovery from existing registration owners. MIT license, see LICENSE. }
unit nyx.documentation.registry;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.text, nyx.types, nyx.scheduler, nyx.documentation,
  nyx.documentation.registration, nyx.documentation.declarations,
  nyx.schema, nyx.callbacks, nyx.catalog;

type
  { Independent captured authority generations. This vector promises no atomic
    transaction between schema, callback and catalog registration. Catalog
    identity distinguishes unrelated instances at the same local revision. }
  TNyxDocumentationGeneration = record
    SchemaRevision, CallbackRevision, CatalogIdentity, CatalogRevision: Integer;
    function Same(const AOther: TNyxDocumentationGeneration): Boolean;
  end;
  { Aggregate index allowances supplement, never increase, declaration/bundle
    limits. Work counts bounded fields, links and comparison/traversal steps.
    Zero-initialized limits refuse; Defaults is an explicit ordinary boundary. }
  TNyxDocumentationIndexLimits = record
  private
    FDeclarations, FBindings, FBytes, FWork: Integer;
  public
    class function Defaults: TNyxDocumentationIndexLimits; static;
    function Declarations(AValue: Integer): TNyxDocumentationIndexLimits;
    function Bindings(AValue: Integer): TNyxDocumentationIndexLimits;
    function Bytes(AValue: Integer): TNyxDocumentationIndexLimits;
    function Work(AValue: Integer): TNyxDocumentationIndexLimits;
    procedure Validate;
    property DeclarationLimit: Integer read FDeclarations;
    property BindingLimit: Integer read FBindings;
    property ByteLimit: Integer read FBytes;
    property WorkLimit: Integer read FWork;
  end;
  { The owner remains explicit even when schema and catalog share one kind.
    No executable callback or recipe is exposed through these value rows. }
  TNyxDocumentationBindingKind = (ndbindingSchema, ndbindingCallback, ndbindingCatalog);
  { One copied discovery row. An undocumented legacy row carries no Primary or
    origin/availability claims; it remains visible as an explicit coverage gap. }
  TNyxDocumentationBinding = record
    Owner: TNyxDocumentationBindingKind;
    Name: TNyxText;
    Documented: Boolean;
    Origin, Primary: TNyxDocumentationRef;
    Platforms: TNyxDocumentationPlatforms;
  end;
  { Bounded member query returns original declaration owners, including diamond
    bases only once. Overload identities remain separate; no winner is guessed. }
  INyxDocumentationMembers = interface(IInterface)
    ['{017BE334-5525-4315-BAE2-E5E46A8CBD03}']
    function Count: Integer;
    function ItemAt(AIndex: Integer): INyxDocumentationMember;
  end;
  { Frozen deterministic discovery contains only inert declarations/value rows.
    Find methods return nil for absence; invalid ordinals refuse. A valid index
    may report legacy undocumented rows, but every present documentation link
    must resolve. Version is local publication order; Generation owns sources. }
  INyxDocumentationIndex = interface(IInterface)
    ['{017BE334-5525-4315-BAE2-E5E46A8CBD02}']
    function Version: Integer;
    function Generation: TNyxDocumentationGeneration;
    function DeclarationCount: Integer;
    function DeclarationAt(AIndex: Integer): INyxDocumentationType;
    function FindDeclaration(const AIdentity: TNyxDocumentationRef): INyxDocumentationType;
    function FindMember(const AIdentity: TNyxDocumentationRef): INyxDocumentationMember;
    function Members(const AOwner: TNyxDocumentationRef; AInherited: Boolean = True;
      const ACancellation: INyxCancellationScope = nil): INyxDocumentationMembers;
    function BindingCount: Integer;
    function BindingAt(AIndex: Integer): TNyxDocumentationBinding;
    function UndocumentedCount: Integer;
  end;
  { Optional local source generation retains exact immutable package/chunk
    owners. Numeric identities are process-local, not portable source hashes;
    the original three-authority generation record keeps its existing meaning. }
  INyxDocumentationContributionGeneration = interface(IInterface)
    ['{A4680A42-26F4-4F00-9BA3-73A4BD13E104}']
    function PackageCount: Integer;
    function PackageAt(AIndex: Integer): INyxDocumentationPackage;
    function ChunkCount: Integer;
    function ChunkAt(AIndex: Integer): INyxDocumentationDeclarationChunk;
  end;
  { Caller-owned publication, serialized like its UI/startup registration owners.
    Refresh builds privately and checks expected Version again before swapping.
    Cancellation, incomplete closure or conflict leaves Current and Version
    exact. It never captures globals implicitly or coordinates a transaction
    between the independent authorities. There is no mutable global help list. }
  TNyxDocumentationRegistry = class
  private
    FCurrent: INyxDocumentationIndex;
    FVersion: Integer;
  public
    function Refresh(const ASchemas: INyxSchemaSnapshot;
      const ACallbacks: INyxCallbackSnapshot; const ACatalog: INyxCatalogSnapshot;
      AExpectedVersion: Integer; const ALimits: TNyxDocumentationIndexLimits;
      const ACancellation: INyxCancellationScope = nil): INyxDocumentationIndex; overload;
    function Refresh(const ASchemas: INyxSchemaSnapshot;
      const ACallbacks: INyxCallbackSnapshot; const ACatalog: INyxCatalogSnapshot;
      AExpectedVersion: Integer;
      const ACancellation: INyxCancellationScope = nil): INyxDocumentationIndex; overload;
    property Current: INyxDocumentationIndex read FCurrent;
    property Version: Integer read FVersion;
  end;

implementation

uses nyx.bytes;

type
  { Query-local work/cancellation owner never survives into an index. Byte
    charging includes repeated registration occurrences, not only deduplicated
    declarations, so duplicate publishers cannot multiply an allowance. }
  TDocumentationBudget = class(TInterfacedObject, INyxDocumentationAdmission,
    INyxDocumentationContributionAdmission)
  private
    FAdmittedChunks: TNyxDocumentationDeclarationChunks;
    FAdmittedReferences: TNyxDocumentationPackages;
  public
    Limits: TNyxDocumentationIndexLimits;
    Cancellation: INyxCancellationScope;
    RemainingWork, RemainingBytes, Occurrences: Integer;
    constructor Create(const ALimits: TNyxDocumentationIndexLimits;
      const ACancellation: INyxCancellationScope);
    procedure Spend;
    procedure ChargeBytes(AValue: Integer);
    procedure Text(const AValue: TNyxText);
    procedure AdmitField(ABytes: Integer);
    procedure AdmitContributionWork(ACount: Integer);
    procedure AdmitContributionDeclarations(ACount: Integer);
    function ContributionChunkAdmitted(const AChunk: INyxDocumentationDeclarationChunk): Boolean;
    procedure RememberContributionChunk(const AChunk: INyxDocumentationDeclarationChunk);
    function ContributionReferencesAdmitted(const APackage: INyxDocumentationPackage): Boolean;
    procedure RememberContributionReferences(const APackage: INyxDocumentationPackage);
  end;
  { Member result owns immutable interfaces only, independently of index life. }
  TDocumentationMembers = class(TInterfacedObject, INyxDocumentationMembers)
  public
    Items: array of INyxDocumentationMember;
    ItemCount: Integer;
    function Count: Integer;
    function ItemAt(AIndex: Integer): INyxDocumentationMember;
  end;
  { Parallel vectors keep interfaces in managed classes. Binding/generation
    records remain ordinary copyable values; captured registration producers,
    catalogs and schema Execute capabilities are never retained. }
  TDocumentationIndex = class(TInterfacedObject, INyxDocumentationIndex,
    INyxDocumentationContributionGeneration)
  public
    APIs: array of INyxDocumentationType;
    Contents: array of TNyxText;
    Platforms: array of TNyxDocumentationPlatforms;
    Rows: array of TNyxDocumentationBinding;
    SourcePackages: TNyxDocumentationPackages;
    SourceChunks: TNyxDocumentationDeclarationChunks;
    CapturedGeneration: TNyxDocumentationGeneration;
    CapturedVersion, Missing: Integer;
    Limits: TNyxDocumentationIndexLimits;
    function Version: Integer;
    function Generation: TNyxDocumentationGeneration;
    function DeclarationCount: Integer;
    function DeclarationAt(AIndex: Integer): INyxDocumentationType;
    function FindDeclaration(const AIdentity: TNyxDocumentationRef): INyxDocumentationType;
    function FindMember(const AIdentity: TNyxDocumentationRef): INyxDocumentationMember;
    function Members(const AOwner: TNyxDocumentationRef; AInherited: Boolean;
      const ACancellation: INyxCancellationScope): INyxDocumentationMembers;
    function BindingCount: Integer;
    function BindingAt(AIndex: Integer): TNyxDocumentationBinding;
    function UndocumentedCount: Integer;
    function PackageCount: Integer;
    function PackageAt(AIndex: Integer): INyxDocumentationPackage;
    function ChunkCount: Integer;
    function ChunkAt(AIndex: Integer): INyxDocumentationDeclarationChunk;
    procedure AddDeclaration(const AValue: INyxDocumentationType;
      const AContent: TNyxText; APlatforms: TNyxDocumentationPlatforms;
      ABudget: TDocumentationBudget);
    procedure AddPackage(const AValue: INyxDocumentationPackage;
      ABudget: TDocumentationBudget);
    procedure Add(const ARow: TNyxDocumentationBinding;
      const ARegistration: INyxDocumentationRegistration; ABudget: TDocumentationBudget);
    procedure ValidateClosure(ABudget: TDocumentationBudget);
    procedure Sort(ABudget: TDocumentationBudget);
  end;

function TNyxDocumentationGeneration.Same(const AOther: TNyxDocumentationGeneration): Boolean;
begin
  Result := (SchemaRevision = AOther.SchemaRevision) and
    (CallbackRevision = AOther.CallbackRevision) and
    (CatalogIdentity = AOther.CatalogIdentity) and (CatalogRevision = AOther.CatalogRevision);
end;

class function TNyxDocumentationIndexLimits.Defaults: TNyxDocumentationIndexLimits;
begin
  Result.FDeclarations := 4096;
  Result.FBindings := 8192;
  Result.FBytes := 16 * 1024 * 1024;
  Result.FWork := 2000000;
end;

function TNyxDocumentationIndexLimits.Declarations(AValue: Integer): TNyxDocumentationIndexLimits;
begin
  Result := Self;
  Result.FDeclarations := AValue;
  Result.Validate;
end;

function TNyxDocumentationIndexLimits.Bindings(AValue: Integer): TNyxDocumentationIndexLimits;
begin
  Result := Self;
  Result.FBindings := AValue;
  Result.Validate;
end;

function TNyxDocumentationIndexLimits.Bytes(AValue: Integer): TNyxDocumentationIndexLimits;
begin
  Result := Self;
  Result.FBytes := AValue;
  Result.Validate;
end;

function TNyxDocumentationIndexLimits.Work(AValue: Integer): TNyxDocumentationIndexLimits;
begin
  Result := Self;
  Result.FWork := AValue;
  Result.Validate;
end;

procedure TNyxDocumentationIndexLimits.Validate;
begin

  if (FDeclarations < 1) or (FDeclarations > 4096) or
    (FBindings < 1) or (FBindings > 8192) or
    (FBytes < 1) or (FBytes > 16 * 1024 * 1024) or
    (FWork < 1) or (FWork > 2000000) then
    raise ENyxDocumentation.Create('Invalid documentation aggregate allowances');
end;

constructor TDocumentationBudget.Create(const ALimits: TNyxDocumentationIndexLimits;
  const ACancellation: INyxCancellationScope);
begin
  inherited Create;
  ALimits.Validate;
  Limits := ALimits;
  Cancellation := ACancellation;
  RemainingWork := ALimits.WorkLimit;
  RemainingBytes := ALimits.ByteLimit;
  Spend;
end;

procedure TDocumentationBudget.Spend;
begin

  if (Cancellation <> nil) and Cancellation.Cancelled then
    raise ENyxDocumentationCancelled.Create('Documentation index cancelled');

  if RemainingWork = 0 then
    raise ENyxDocumentation.Create('Documentation aggregate work exhausted');
  Dec(RemainingWork);
end;

procedure TDocumentationBudget.ChargeBytes(AValue: Integer);
begin
  Spend;

  if (AValue < 0) or (AValue > RemainingBytes) then
    raise ENyxDocumentation.Create('Documentation aggregate bytes exhausted');
  Dec(RemainingBytes, AValue);
end;

procedure TDocumentationBudget.Text(const AValue: TNyxText);
begin
  Spend;

  if Length(AValue) > RemainingBytes then
    raise ENyxDocumentation.Create('Documentation aggregate text exhausted');
  ChargeBytes(NyxUTF8ByteCount(AValue));
end;

procedure TDocumentationBudget.AdmitField(ABytes: Integer);
begin
  ChargeBytes(ABytes);
end;

procedure TDocumentationBudget.AdmitContributionWork(ACount: Integer);
begin

  if (Cancellation <> nil) and Cancellation.Cancelled then
    raise ENyxDocumentationCancelled.Create('Documentation index cancelled');

  if (ACount < 0) or (ACount > RemainingWork) then
    raise ENyxDocumentation.Create('Documentation aggregate work exhausted');
  Dec(RemainingWork, ACount);
end;

procedure TDocumentationBudget.AdmitContributionDeclarations(ACount: Integer);
begin
  Spend;

  if (ACount < 1) or (ACount > Limits.DeclarationLimit - Occurrences) then
    raise ENyxDocumentation.Create('Documentation declaration occurrence allowance exhausted');
  Inc(Occurrences, ACount);
end;

function TDocumentationBudget.ContributionChunkAdmitted(
  const AChunk: INyxDocumentationDeclarationChunk): Boolean;
var
  LAt: Integer;
begin
  Spend;
  { Same validates the private captured-owner receipt even for an empty list;
    an alternate implementation's numeric identity never earns a cost skip. }
  SameNyxDocumentationChunk(AChunk, AChunk);
  Result := False;
  for LAt := 0 to High(FAdmittedChunks) do
  begin
    Spend;

    if SameNyxDocumentationChunk(FAdmittedChunks[LAt], AChunk) then
      Exit(True);
  end;
end;

procedure TDocumentationBudget.RememberContributionChunk(
  const AChunk: INyxDocumentationDeclarationChunk);
var
  LSlot: Integer;
begin

  if ContributionChunkAdmitted(AChunk) then
    Exit;
  LSlot := Length(FAdmittedChunks);

  if LSlot >= Limits.DeclarationLimit then
    raise ENyxDocumentation.Create('Documentation source receipt allowance exhausted');
  SetLength(FAdmittedChunks, LSlot + 1);
  FAdmittedChunks[LSlot] := AChunk;
end;

function TDocumentationBudget.ContributionReferencesAdmitted(
  const APackage: INyxDocumentationPackage): Boolean;
var
  LAt: Integer;
begin
  Spend;
  SameNyxDocumentationPackage(APackage, APackage);
  Result := False;
  for LAt := 0 to High(FAdmittedReferences) do
  begin
    Spend;

    if SameNyxDocumentationPackage(FAdmittedReferences[LAt], APackage) then
      Exit(True);
  end;
end;

procedure TDocumentationBudget.RememberContributionReferences(
  const APackage: INyxDocumentationPackage);
var
  LSlot: Integer;
begin

  if ContributionReferencesAdmitted(APackage) then
    Exit;
  LSlot := Length(FAdmittedReferences);

  if LSlot >= Limits.BindingLimit then
    raise ENyxDocumentation.Create('Documentation package receipt allowance exhausted');
  SetLength(FAdmittedReferences, LSlot + 1);
  FAdmittedReferences[LSlot] := APackage;
end;

function TDocumentationMembers.Count: Integer;
begin
  Result := ItemCount;
end;

function TDocumentationMembers.ItemAt(AIndex: Integer): INyxDocumentationMember;
begin

  if (AIndex < 0) or (AIndex >= Count) then
    raise ENyxDocumentation.Create('Documentation member index out of range');
  Result := Items[AIndex];
end;

function TDocumentationIndex.Version: Integer;
begin
  Result := CapturedVersion;
end;

function TDocumentationIndex.Generation: TNyxDocumentationGeneration;
begin
  Result := CapturedGeneration;
end;

function TDocumentationIndex.DeclarationCount: Integer;
begin
  Result := Length(APIs);
end;

function TDocumentationIndex.DeclarationAt(AIndex: Integer): INyxDocumentationType;
begin

  if (AIndex < 0) or (AIndex >= DeclarationCount) then
    raise ENyxDocumentation.Create('Documentation declaration index out of range');
  Result := APIs[AIndex];
end;

function TDocumentationIndex.FindDeclaration(const AIdentity: TNyxDocumentationRef): INyxDocumentationType;
var
  LLow, LHigh, LMid: Integer;
  LKey, LCurrent: TNyxText;
begin
  Result := nil;

  if not AIdentity.Defined or AIdentity.IsMember then
    Exit;
  LKey := AIdentity.Key;
  LLow := 0;
  LHigh := High(APIs);
  while LLow <= LHigh do
  begin
    LMid := LLow + (LHigh - LLow) div 2;
    LCurrent := APIs[LMid].Identity.Key;

    if LCurrent = LKey then
      Exit(APIs[LMid]);

    if LCurrent < LKey then
      LLow := LMid + 1
    else
      LHigh := LMid - 1;
  end;
end;

function TDocumentationIndex.FindMember(const AIdentity: TNyxDocumentationRef): INyxDocumentationMember;
var
  LType: INyxDocumentationType;
  LAt: Integer;
begin
  Result := nil;

  if not AIdentity.IsMember then
    Exit;
  LType := FindDeclaration(AIdentity.Owner);

  if LType = nil then
    Exit;
  for LAt := 0 to LType.MemberCount - 1 do
  begin

    if LType.MemberAt(LAt).Identity.Same(AIdentity) then
      Exit(LType.MemberAt(LAt));
  end;
end;

function TDocumentationIndex.BindingCount: Integer;
begin
  Result := Length(Rows);
end;

function TDocumentationIndex.BindingAt(AIndex: Integer): TNyxDocumentationBinding;
begin

  if (AIndex < 0) or (AIndex >= BindingCount) then
    raise ENyxDocumentation.Create('Documentation binding index out of range');
  Result := Rows[AIndex];
end;

function TDocumentationIndex.UndocumentedCount: Integer;
begin
  Result := Missing;
end;

function TDocumentationIndex.PackageCount: Integer;
begin
  Result := Length(SourcePackages);
end;

function TDocumentationIndex.PackageAt(AIndex: Integer): INyxDocumentationPackage;
begin

  if (AIndex < 0) or (AIndex >= PackageCount) then
    raise ENyxDocumentation.Create('Captured package ordinal is out of range');
  Result := SourcePackages[AIndex];
end;

function TDocumentationIndex.ChunkCount: Integer;
begin
  Result := Length(SourceChunks);
end;

function TDocumentationIndex.ChunkAt(AIndex: Integer): INyxDocumentationDeclarationChunk;
begin

  if (AIndex < 0) or (AIndex >= ChunkCount) then
    raise ENyxDocumentation.Create('Captured source chunk ordinal is out of range');
  Result := SourceChunks[AIndex];
end;

procedure TDocumentationIndex.AddDeclaration(const AValue: INyxDocumentationType;
  const AContent: TNyxText; APlatforms: TNyxDocumentationPlatforms;
  ABudget: TDocumentationBudget);
var
  LOther, LSlot: Integer;
begin
  ABudget.Spend;
  LSlot := -1;
  for LOther := 0 to High(APIs) do
  begin
    ABudget.Spend;

    if APIs[LOther].Identity.Same(AValue.Identity) then
    begin
      LSlot := LOther;
      Break;
    end;
  end;

  if LSlot >= 0 then
  begin

    if (Contents[LSlot] <> AContent) or (Platforms[LSlot] <> APlatforms) then
      raise ENyxDocumentation.Create('Conflicting documentation declaration: ' + AValue.Identity.Key);
  end
  else
  begin
    LSlot := Length(APIs);
    SetLength(APIs, LSlot + 1);
    SetLength(Contents, LSlot + 1);
    SetLength(Platforms, LSlot + 1);
    APIs[LSlot] := AValue;
    Contents[LSlot] := AContent;
    Platforms[LSlot] := APlatforms;
  end;
end;

procedure TDocumentationIndex.AddPackage(const AValue: INyxDocumentationPackage;
  ABudget: TDocumentationBudget);
var
  LPackage: INyxDocumentationPackage;
  LChunk: INyxDocumentationDeclarationChunk;
  LAt, LOther, LDeclaration, LSlot: Integer;
  LSeen, LReferencesAdmitted: Boolean;
begin
  LPackage := CaptureNyxDocumentationPackage(AValue, ABudget.Cancellation, ABudget);
  for LAt := 0 to PackageCount - 1 do
  begin
    ABudget.Spend;

    if SameNyxDocumentationPackage(SourcePackages[LAt], LPackage) then
      Exit;
  end;
  ABudget.Text(IntToStr(LPackage.Identity));
  LReferencesAdmitted := ABudget.ContributionReferencesAdmitted(LPackage);
  for LAt := 0 to LPackage.ChunkCount - 1 do
  begin
    ABudget.Spend;
    LChunk := LPackage.ChunkAt(LAt);
    { A package/chunk edge is a retained reference even if another package
      already retained this copied source. Charge its fields independently. }

    if not LReferencesAdmitted then
    begin
      ABudget.Text(LChunk.OriginUnit.Key);
      ABudget.Text(IntToStr(LChunk.Identity));
    end;
    LSeen := False;
    for LOther := 0 to ChunkCount - 1 do
    begin
      ABudget.Spend;

      if SameNyxDocumentationChunk(SourceChunks[LOther], LChunk) then
      begin
        LSeen := True;
        Break;
      end;
    end;

    if LSeen then
      Continue;

    LChunk := CaptureNyxDocumentationDeclarationChunk(LChunk, ABudget.Cancellation, ABudget);
    for LDeclaration := 0 to LChunk.DeclarationCount - 1 do
      AddDeclaration(LChunk.DeclarationAt(LDeclaration), LChunk.DeclarationContentAt(LDeclaration),
        LChunk.Platforms, ABudget);
    LSlot := ChunkCount;
    SetLength(SourceChunks, LSlot + 1);
    SourceChunks[LSlot] := LChunk;
  end;
  LSlot := PackageCount;
  SetLength(SourcePackages, LSlot + 1);
  SourcePackages[LSlot] := LPackage;
  ABudget.RememberContributionReferences(LPackage);
end;

procedure TDocumentationIndex.Add(const ARow: TNyxDocumentationBinding;
  const ARegistration: INyxDocumentationRegistration; ABudget: TDocumentationBudget);
var
  LRegistration: INyxDocumentationRegistration;
  LRow: TNyxDocumentationBinding;
  LAt, LSlot: Integer;
  LAPI: INyxDocumentationType;
  LContribution: INyxDocumentationContributionBinding;
  LContributed: Boolean;
begin
  ABudget.Spend;

  if BindingCount = ABudget.Limits.BindingLimit then
    raise ENyxDocumentation.Create('Documentation binding allowance exhausted');
  ABudget.Text(ARow.Name);

  if Trim(ARow.Name) = '' then
    raise ENyxDocumentation.Create('A documentation binding requires its registration name');
  for LAt := 0 to High(Rows) do
  begin
    ABudget.Spend;

    if (Rows[LAt].Owner = ARow.Owner) and
      (((ARow.Owner = ndbindingCallback) and SameText(Rows[LAt].Name, ARow.Name)) or
      ((ARow.Owner <> ndbindingCallback) and (Rows[LAt].Name = ARow.Name))) then
      raise ENyxDocumentation.Create('Duplicate registration owner row');
  end;
  LRow := ARow;
  LContributed := (ARegistration <> nil) and
    Supports(ARegistration, INyxDocumentationContributionBinding, LContribution);

  if (ARegistration <> nil) and not LContributed then
  begin
    LAt := ARegistration.DeclarationCount;

    if (LAt < 1) or (LAt > ABudget.Limits.DeclarationLimit - ABudget.Occurrences) then
      raise ENyxDocumentation.Create('Documentation declaration occurrence allowance exhausted');
  end;

  LRegistration := CaptureNyxDocumentationRegistration(ARegistration, ABudget.Cancellation, ABudget);
  LRow.Documented := LRegistration <> nil;

  if LRegistration = nil then
    Inc(Missing)
  else
  begin

    if not LContributed then
    begin

      if LRegistration.DeclarationCount > ABudget.Limits.DeclarationLimit - ABudget.Occurrences then
        raise ENyxDocumentation.Create('Documentation declaration occurrence allowance exhausted');
      Inc(ABudget.Occurrences, LRegistration.DeclarationCount);
    end;
    LRow.Primary := LRegistration.Primary;
    LRow.Origin := LRegistration.OriginUnit;
    LRow.Platforms := LRegistration.Platforms;
    { Callback mappings declare an actual class API, never a guessed global
      factory or an interface inferred from its registration handler spelling. }

    if LRow.Owner = ndbindingCallback then
    begin

      if LRow.Primary.IsMember then
        raise ENyxDocumentation.Create('Callback documentation requires a declared class primary');
      for LAt := 0 to LRegistration.DeclarationCount - 1 do
      begin
        LAPI := LRegistration.DeclarationAt(LAt);

        if LAPI.Identity.Same(LRow.Primary) and not (LAPI.Kind in [ndtClass, ndtCallback]) then
          raise ENyxDocumentation.Create('Callback documentation primary is not a class');
      end;
    end;

    if LContributed then
    begin
      Supports(LRegistration, INyxDocumentationContributionBinding, LContribution);
      { These are binding/reference fields, not another copy of its complete
        source declaration. A numeric identity alone never skips source costs. }
      ABudget.Text(LRow.Origin.Key);
      ABudget.Text(LRow.Primary.Key);
      ABudget.Text(IntToStr(Ord(ndplatformNative in LRow.Platforms)) +
        IntToStr(Ord(ndplatformBrowser in LRow.Platforms)));
      ABudget.Text(IntToStr(LContribution.PackageOwner.Identity));
      AddPackage(LContribution.PackageOwner, ABudget);
    end
    else
      for LAt := 0 to LRegistration.DeclarationCount - 1 do
        AddDeclaration(LRegistration.DeclarationAt(LAt), LRegistration.DeclarationContentAt(LAt),
          LRegistration.Platforms, ABudget);
  end;
  for LAt := 0 to High(Rows) do
  begin
    ABudget.Spend;

    if (Rows[LAt].Owner <> ndbindingCallback) and (LRow.Owner <> ndbindingCallback) and
      (Rows[LAt].Owner <> LRow.Owner) and (Rows[LAt].Name = LRow.Name) and
      Rows[LAt].Documented and LRow.Documented then
    begin

      if not Rows[LAt].Primary.Same(LRow.Primary) or (Rows[LAt].Platforms <> LRow.Platforms) then
        raise ENyxDocumentation.Create('Creator schema/catalog public API attachments conflict');
    end;
  end;
  LSlot := Length(Rows);
  SetLength(Rows, LSlot + 1);
  Rows[LSlot] := LRow;
end;

procedure TDocumentationIndex.Sort(ABudget: TDocumentationBudget);
var
  LAt, LPos: Integer;
  LAPI: INyxDocumentationType;
  LContent, LKey: TNyxText;
  LPlatforms: TNyxDocumentationPlatforms;
  LRow: TNyxDocumentationBinding;
begin
  { Stable insertion uses the same charged work bound as every other phase.
    Even deliberately reverse-ordered producers cannot create unbounded work. }
  for LAt := 1 to High(APIs) do
  begin
    LAPI := APIs[LAt];
    LContent := Contents[LAt];
    LPlatforms := Platforms[LAt];
    LKey := LAPI.Identity.Key;
    LPos := LAt;
    while LPos > 0 do
    begin
      ABudget.Spend;

      if APIs[LPos - 1].Identity.Key <= LKey then
        Break;
      APIs[LPos] := APIs[LPos - 1];
      Contents[LPos] := Contents[LPos - 1];
      Platforms[LPos] := Platforms[LPos - 1];
      Dec(LPos);
    end;
    APIs[LPos] := LAPI;
    Contents[LPos] := LContent;
    Platforms[LPos] := LPlatforms;
  end;
  for LAt := 1 to High(Rows) do
  begin
    LRow := Rows[LAt];
    LPos := LAt;
    while LPos > 0 do
    begin
      ABudget.Spend;

      if (Ord(Rows[LPos - 1].Owner) < Ord(LRow.Owner)) or
        ((Rows[LPos - 1].Owner = LRow.Owner) and (Rows[LPos - 1].Name <= LRow.Name)) then
        Break;
      Rows[LPos] := Rows[LPos - 1];
      Dec(LPos);
    end;
    Rows[LPos] := LRow;
  end;
end;

{ Absence is a real branch result. No target-specific alias borrows another
  projection's case, and Any resolves only an actually common target. }
function AliasFor(const AType: INyxDocumentationType;
  APlatform: TNyxPlatform): TNyxDocumentationRef;
var
  LTargets: INyxDocumentationAliasTargets;
begin
  Result := AType.AliasTarget;

  if Supports(AType, INyxDocumentationAliasTargets, LTargets) then
  begin
    Result := Default(TNyxDocumentationRef);

    if LTargets.HasTarget(APlatform) then
      Result := LTargets.TargetFor(APlatform);
  end;
end;

procedure TDocumentationIndex.ValidateClosure(ABudget: TDocumentationBudget);
var
  LAt, LLink, LMember, LParameter, LEdge, LDepth, LTarget, LScan: Integer;
  LAPI: INyxDocumentationType;
  LMethod: INyxDocumentationMember;
  LSignature: INyxDocumentationSignature;
  LProperty: INyxDocumentationProperty;
  LStates, LStack, LEdges: array of Integer;
  LReference: TNyxDocumentationRef;
  LTargets: INyxDocumentationAliasTargets;
  LPlatform: TNyxPlatform;
  LKind: TNyxDocumentationTypeKind;
  function DeclaredFor(AIndex: Integer; APlatform: TNyxPlatform): Boolean;
  begin
    Result := ((APlatform = npfBrowser) and (ndplatformBrowser in Platforms[AIndex])) or
      ((APlatform = npfNativeLCL) and (ndplatformNative in Platforms[AIndex]));
  end;
  procedure Require(const AReference: TNyxDocumentationRef);
  begin
    ABudget.Spend;

    if not AReference.Defined then
      Exit;

    if AReference.IsMember then
    begin

      if FindMember(AReference) = nil then
        raise ENyxDocumentation.Create('Unresolved documentation member: ' + AReference.Key);
    end
    else if FindDeclaration(AReference) = nil then
      raise ENyxDocumentation.Create('Unresolved documentation type or unit: ' + AReference.Key);
  end;
  function EffectiveKind(const AReference: TNyxDocumentationRef;
    APlatform: TNyxPlatform; ARequired: Boolean;
    out AKind: TNyxDocumentationTypeKind): Boolean;
  var
    LCurrent: INyxDocumentationType;
    LSteps: Integer;
    LNextReference: TNyxDocumentationRef;
  begin
    Result := False;
    LCurrent := FindDeclaration(AReference);
    LSteps := 0;
    while (LCurrent <> nil) and (LCurrent.Kind = ndtAlias) do
    begin
      ABudget.Spend;
      Inc(LSteps);

      if LSteps > DeclarationCount then
        raise ENyxDocumentation.Create('Cyclic documentation alias');
      LNextReference := AliasFor(LCurrent, APlatform);

      if not LNextReference.Defined then
      begin

        if ARequired then
          raise ENyxDocumentation.Create('A base alias target is absent for a declared platform');
        Exit;
      end;
      LCurrent := FindDeclaration(LNextReference);
    end;

    if LCurrent = nil then
      raise ENyxDocumentation.Create('Unresolved declared base alias');
    AKind := LCurrent.Kind;
    Result := True;
  end;
begin
  for LAt := 0 to High(APIs) do
  begin
    ABudget.Spend;
    LAPI := APIs[LAt];

    if (LAPI.Kind = ndtAlias) and not LAPI.AliasTarget.Defined and
      not Supports(LAPI, INyxDocumentationAliasTargets, LTargets) then
      raise ENyxDocumentation.Create('Complete documentation alias requires its target: ' + LAPI.Identity.Key);
    Require(LAPI.AliasTarget);

    if Supports(LAPI, INyxDocumentationAliasTargets, LTargets) then
    begin
      for LLink := 0 to LTargets.TargetCount - 1 do
        Require(LTargets.TargetAt(LLink).Target);
      for LPlatform := npfBrowser to npfNativeLCL do
      begin

        if DeclaredFor(LAt, LPlatform) and not LTargets.HasTarget(LPlatform) then
          raise ENyxDocumentation.Create('An alias target is absent for its declared platform availability');
      end;
    end;
    for LLink := 0 to LAPI.BaseCount - 1 do
    begin
      Require(LAPI.BaseAt(LLink));

      for LPlatform := npfBrowser to npfNativeLCL do
      begin

        if (Platforms[LAt] <> []) and not DeclaredFor(LAt, LPlatform) then
          Continue;

        if EffectiveKind(LAPI.BaseAt(LLink), LPlatform, DeclaredFor(LAt, LPlatform), LKind) then
        begin

          if ((LAPI.Kind = ndtInterface) and (LKind <> ndtInterface)) or
            ((LAPI.Kind in [ndtClass, ndtCallback]) and not (LKind in [ndtClass, ndtCallback])) then
            raise ENyxDocumentation.Create('Incompatible declared inheritance kinds');
        end;
      end;
    end;
    for LLink := 0 to LAPI.RelatedCount - 1 do
      Require(LAPI.RelatedAt(LLink));
    for LMember := 0 to LAPI.MemberCount - 1 do
    begin
      ABudget.Spend;
      LMethod := LAPI.MemberAt(LMember);
      Require(LMethod.ValueType);
      for LLink := 0 to LMethod.RelatedCount - 1 do
        Require(LMethod.RelatedAt(LLink));
      LSignature := LMethod.Signature;

      if LSignature <> nil then
      begin
        Require(LSignature.ResultType);
        for LParameter := 0 to LSignature.ParameterCount - 1 do
          Require(LSignature.ParameterAt(LParameter).TypeReference);
      end;

      if Supports(LMethod, INyxDocumentationProperty, LProperty) then
        for LParameter := 0 to LProperty.ParameterCount - 1 do
          Require(LProperty.ParameterAt(LParameter).TypeReference);
    end;
  end;
  { Iterative colour traversal checks base/alias edges without Pascal recursion
    or stack growth proportional to hostile declaration depth. Related links
    may legitimately form cycles; they were resolved above, not traversed. }
  SetLength(LStates, Length(APIs));
  SetLength(LStack, Length(APIs));
  SetLength(LEdges, Length(APIs));
  for LPlatform := npfBrowser to npfNativeLCL do
  begin
    for LAt := 0 to High(LStates) do
      LStates[LAt] := 0;
    for LAt := 0 to High(APIs) do
    begin

      if LStates[LAt] <> 0 then
        Continue;
      LDepth := 0;
      LStack[0] := LAt;
      LEdges[0] := 0;
      LStates[LAt] := 1;
      while LDepth >= 0 do
      begin
        ABudget.Spend;
        LScan := LStack[LDepth];
        LAPI := APIs[LScan];
        LEdge := LEdges[LDepth];
        Inc(LEdges[LDepth]);
        LReference := Default(TNyxDocumentationRef);

        if LEdge < LAPI.BaseCount then
          LReference := LAPI.BaseAt(LEdge)
        else if LEdge = LAPI.BaseCount then
          LReference := AliasFor(LAPI, LPlatform);

        if not LReference.Defined then
        begin
          LStates[LScan] := 2;
          Dec(LDepth);
          Continue;
        end;
        LTarget := -1;
        for LScan := 0 to High(APIs) do
        begin
          ABudget.Spend;

          if APIs[LScan].Identity.Same(LReference) then
          begin
            LTarget := LScan;
            Break;
          end;
        end;

        if LTarget < 0 then
          raise ENyxDocumentation.Create('Unresolved inheritance/alias edge');

        if LStates[LTarget] = 1 then
          raise ENyxDocumentation.Create('Cyclic documentation inheritance or alias');

        if LStates[LTarget] = 0 then
        begin
          LStates[LTarget] := 1;
          Inc(LDepth);
          LStack[LDepth] := LTarget;
          LEdges[LDepth] := 0;
        end;
      end;
    end;
  end;
end;

function TDocumentationIndex.Members(const AOwner: TNyxDocumentationRef;
  AInherited: Boolean; const ACancellation: INyxCancellationScope): INyxDocumentationMembers;
var
  LOwner: TDocumentationMembers;
  LLease: INyxDocumentationMembers;
  LBudget: TDocumentationBudget;
  LBudgetLease: INyxDocumentationAdmission;
  LQueue: array of INyxDocumentationType;
  LVisited: array of TNyxDocumentationRef;
  LType: INyxDocumentationType;
  LAt, LNext, LBase, LSeen, LSlot, LLinks, LCapacity: Integer;
  LFound: Boolean;
  LReference: TNyxDocumentationRef;
begin
  LBudget := TDocumentationBudget.Create(Limits, ACancellation);
  LBudgetLease := LBudget;
  try
    LOwner := TDocumentationMembers.Create;
    LLease := LOwner;
    LType := FindDeclaration(AOwner);

    if LType = nil then
      raise ENyxDocumentation.Create('Member query requires a captured declaration');
    SetLength(LQueue, DeclarationCount);
    SetLength(LVisited, DeclarationCount);
    LQueue[0] := LType;
    LVisited[0] := LType.Identity;
    LNext := 1;
    LAt := 0;
    while LAt < LNext do
    begin
      LBudget.Spend;
      LType := LQueue[LAt];
      for LSlot := 0 to LType.MemberCount - 1 do
      begin
        LBudget.Spend;

        if LOwner.ItemCount = Length(LOwner.Items) then
        begin
          LCapacity := Length(LOwner.Items) * 2;

          if LCapacity = 0 then
            LCapacity := 8;
          SetLength(LOwner.Items, LCapacity);
        end;
        LOwner.Items[LOwner.ItemCount] := LType.MemberAt(LSlot);
        Inc(LOwner.ItemCount);
      end;

      if AInherited then
      begin
        { A caller may inspect TargetFor(platform), then query that exact owner.
          The target-neutral legacy query never guesses one branch or reports
          an empty inherited API merely because no common target is known. }

        if (LType.Kind = ndtAlias) and not LType.AliasTarget.Defined then
          raise ENyxDocumentation.Create('Select an explicit alias platform target before inherited member lookup');
        LLinks := LType.BaseCount + Ord(LType.AliasTarget.Defined);
        for LBase := 0 to LLinks - 1 do
        begin
          LReference := LType.AliasTarget;

          if LBase < LType.BaseCount then
            LReference := LType.BaseAt(LBase);
          LFound := False;
          for LSeen := 0 to LNext - 1 do
          begin
            LBudget.Spend;

            if LVisited[LSeen].Same(LReference) then
              LFound := True;
          end;

          if not LFound then
          begin
            LVisited[LNext] := LReference;
            LQueue[LNext] := FindDeclaration(LVisited[LNext]);
            Inc(LNext);
          end;
        end;
      end;
      Inc(LAt);
    end;
    LBudget.Spend;
    Result := LLease;
  finally
    LBudgetLease := nil;
  end;
end;

function TNyxDocumentationRegistry.Refresh(const ASchemas: INyxSchemaSnapshot;
  const ACallbacks: INyxCallbackSnapshot; const ACatalog: INyxCatalogSnapshot;
  AExpectedVersion: Integer; const ACancellation: INyxCancellationScope): INyxDocumentationIndex;
begin
  Result := Refresh(ASchemas, ACallbacks, ACatalog, AExpectedVersion,
    TNyxDocumentationIndexLimits.Defaults, ACancellation);
end;

function TNyxDocumentationRegistry.Refresh(const ASchemas: INyxSchemaSnapshot;
  const ACallbacks: INyxCallbackSnapshot; const ACatalog: INyxCatalogSnapshot;
  AExpectedVersion: Integer; const ALimits: TNyxDocumentationIndexLimits;
  const ACancellation: INyxCancellationScope): INyxDocumentationIndex;
var
  LOwner: TDocumentationIndex;
  LLease: INyxDocumentationIndex;
  LBudget: TDocumentationBudget;
  LBudgetLease: INyxDocumentationAdmission;
  LSchemas: INyxSchemaInspection;
  LSchemaDocumentation: INyxSchemaDocumentation;
  LCallbackDocumentation: INyxCallbackDocumentation;
  LCatalogDocumentation: INyxCatalogDocumentation;
  LGeneration: TNyxDocumentationGeneration;
  LRow: TNyxDocumentationBinding;
  LAt, LCount: Integer;
  LCounts: array[0..2] of Integer;
begin

  if (AExpectedVersion <> FVersion) or (FVersion = High(Integer)) then
    raise ENyxDocumentation.Create('Documentation publication version is stale or exhausted');

  if (ASchemas = nil) or (ACallbacks = nil) or (ACatalog = nil) then
    raise ENyxDocumentation.Create('Documentation discovery requires all three captured authorities');

  if not Supports(ASchemas, INyxSchemaInspection, LSchemas) or
    not Supports(ASchemas, INyxSchemaDocumentation, LSchemaDocumentation) or
    not Supports(ACallbacks, INyxCallbackDocumentation, LCallbackDocumentation) or
    not Supports(ACatalog, INyxCatalogDocumentation, LCatalogDocumentation) then
    raise ENyxDocumentation.Create('Captured authority lacks typed documentation inspection');

  if not LSchemaDocumentation.HasLocalDocumentation then
    raise ENyxDocumentation.Create('Transported schema supplies no local declaration generation');
  LBudget := TDocumentationBudget.Create(ALimits, ACancellation);
  LBudgetLease := LBudget;
  try
    LOwner := TDocumentationIndex.Create;
    LLease := LOwner;
    LOwner.Limits := ALimits;
    LGeneration.SchemaRevision := ASchemas.Revision;
    LGeneration.CallbackRevision := ACallbacks.Revision;
    LGeneration.CatalogIdentity := ACatalog.Identity;
    LGeneration.CatalogRevision := ACatalog.Revision;

    if (LGeneration.SchemaRevision < 0) or (LGeneration.CallbackRevision < 0) or
      (LGeneration.CatalogIdentity < 1) or (LGeneration.CatalogRevision < 0) then
      raise ENyxDocumentation.Create('Invalid captured authority generation');
    LOwner.CapturedGeneration := LGeneration;
    LOwner.CapturedVersion := FVersion + 1;
    LCount := LSchemas.KindCount;
    LCounts[0] := LCount;

    if (LCount < 0) or (LCount > ALimits.BindingLimit) then
      raise ENyxDocumentation.Create('Schema discovery row allowance exhausted');
    for LAt := 0 to LCount - 1 do
    begin
      LBudget.Spend;
      LRow := Default(TNyxDocumentationBinding);
      LRow.Owner := ndbindingSchema;
      LRow.Name := LSchemas.KindAt(LAt).Name;
      LOwner.Add(LRow, LSchemaDocumentation.DocumentationAt(LAt), LBudget);
    end;
    LCount := ACallbacks.Count;
    LCounts[1] := LCount;

    if (LCount < 0) or (LCount > ALimits.BindingLimit - LOwner.BindingCount) then
      raise ENyxDocumentation.Create('Callback discovery row allowance exhausted');
    for LAt := 0 to LCount - 1 do
    begin
      LBudget.Spend;
      LRow := Default(TNyxDocumentationBinding);
      LRow.Owner := ndbindingCallback;
      LRow.Name := ACallbacks.HandlerAt(LAt).Name;
      LOwner.Add(LRow, LCallbackDocumentation.DocumentationAt(LAt), LBudget);
    end;
    LCount := ACatalog.Count;
    LCounts[2] := LCount;

    if (LCount < 0) or (LCount > ALimits.BindingLimit - LOwner.BindingCount) then
      raise ENyxDocumentation.Create('Catalog discovery row allowance exhausted');
    for LAt := 0 to LCount - 1 do
    begin
      LBudget.Spend;
      LRow := Default(TNyxDocumentationBinding);
      LRow.Owner := ndbindingCatalog;
      LRow.Name := ACatalog.ItemAt(LAt).Kind;
      LOwner.Add(LRow, LCatalogDocumentation.DocumentationAt(LAt), LBudget);
    end;
    LOwner.Sort(LBudget);
    LOwner.ValidateClosure(LBudget);
    LBudget.Spend;

    if (AExpectedVersion <> FVersion) or
      (LGeneration.SchemaRevision <> ASchemas.Revision) or
      (LGeneration.CallbackRevision <> ACallbacks.Revision) or
      (LGeneration.CatalogIdentity <> ACatalog.Identity) or
      (LGeneration.CatalogRevision <> ACatalog.Revision) or
      (LCounts[0] <> LSchemas.KindCount) or (LCounts[1] <> ACallbacks.Count) or
      (LCounts[2] <> ACatalog.Count) or not LSchemaDocumentation.HasLocalDocumentation then
      raise ENyxDocumentation.Create('Documentation generation changed before publication');
    FCurrent := LLease;
    Inc(FVersion);
    Result := LLease;
  finally
    LBudgetLease := nil;
  end;
end;

end.
