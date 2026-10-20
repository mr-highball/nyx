{ Source-owned documentation contributions. MIT license, see LICENSE. }
unit nyx.documentation.declarations;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.text, nyx.scheduler, nyx.documentation,
  nyx.documentation.registration;

type
  { Explicit caller-owned aggregate capture ceilings. Defaults reuse the
    existing index ceilings; they prove no fit for a generated source closure.
    Chunks still retain their separate 128-declaration/4 MiB bundle ceilings. }
  TNyxDocumentationContributionLimits = record
  private
    FDeclarations, FBytes, FWork: Integer;
  public
    class function Defaults: TNyxDocumentationContributionLimits; static;
    function Declarations(AValue: Integer): TNyxDocumentationContributionLimits;
    function Bytes(AValue: Integer): TNyxDocumentationContributionLimits;
    function Work(AValue: Integer): TNyxDocumentationContributionLimits;
    procedure Validate;
    property DeclarationLimit: Integer read FDeclarations;
    property ByteLimit: Integer read FBytes;
    property WorkLimit: Integer read FWork;
  end;
  { A copied source chunk owns complete individual declarations from one unit.
    Chunks are storage boundaries, not partial replacements for a unit/class
    declaration. No public primary, creator or executable belongs to a chunk. }
  INyxDocumentationDeclarationChunk = interface(IInterface)
    ['{A4680A42-26F4-4F00-9BA3-73A4BD13E101}']
    function Identity: Integer;
    function OriginUnit: TNyxDocumentationRef;
    function Platforms: TNyxDocumentationPlatforms;
    function DeclarationCount: Integer;
    function DeclarationAt(AIndex: Integer): INyxDocumentationType;
    function DeclarationContentAt(AIndex: Integer): TNyxText;
    function ContentBytes: Integer;
  end;
  { Managed storage keeps source owners outside copied value records. }
  TNyxDocumentationDeclarationChunks = array of INyxDocumentationDeclarationChunk;
  { A flat immutable package retains exact captured chunk owners. References
    between declarations remain qualified values; packages own no recursive
    import graph or mutable producer, control, callback or source document. }
  INyxDocumentationPackage = interface(IInterface)
    ['{A4680A42-26F4-4F00-9BA3-73A4BD13E102}']
    function Identity: Integer;
    function Limits: TNyxDocumentationContributionLimits;
    function ChunkCount: Integer;
    function ChunkAt(AIndex: Integer): INyxDocumentationDeclarationChunk;
    function DeclarationCount: Integer;
    function FindDeclaration(const AIdentity: TNyxDocumentationRef): INyxDocumentationType;
    function FindContent(const AIdentity: TNyxDocumentationRef): TNyxText;
  end;
  { Immutable packages are retained by real registration bindings and snapshots. }
  TNyxDocumentationPackages = array of INyxDocumentationPackage;
  { Optional registration capability separates a public creator reference from
    copied source facts. Legacy getters expose the complete declaration owning
    Primary; this capability retains its whole package without recopying it. }
  INyxDocumentationContributionBinding = interface(IInterface)
    ['{A4680A42-26F4-4F00-9BA3-73A4BD13E103}']
    function PackageOwner: INyxDocumentationPackage;
  end;
  { Optional query-local admission carries the caller's remaining aggregate
    allowance through alternate registration/package/chunk capture. Receipts
    recognize exact canonical owners only; no supplier receives or retains it. }
  INyxDocumentationContributionAdmission = interface(IInterface)
    ['{A4680A42-26F4-4F00-9BA3-73A4BD13E105}']
    procedure AdmitContributionWork(ACount: Integer);
    procedure AdmitContributionDeclarations(ACount: Integer);
    function ContributionChunkAdmitted(const AChunk: INyxDocumentationDeclarationChunk): Boolean;
    procedure RememberContributionChunk(const AChunk: INyxDocumentationDeclarationChunk);
    function ContributionReferencesAdmitted(const APackage: INyxDocumentationPackage): Boolean;
    procedure RememberContributionReferences(const APackage: INyxDocumentationPackage);
  end;

function NewNyxDocumentationDeclarationChunk(const AOrigin: TNyxDocumentationRef;
  const ADeclarations: TNyxDocumentationDeclarations;
  APlatforms: TNyxDocumentationPlatforms = [];
  const ACancellation: INyxCancellationScope = nil;
  const AAdmission: INyxDocumentationAdmission = nil): INyxDocumentationDeclarationChunk;
function CaptureNyxDocumentationDeclarationChunk(const AValue: INyxDocumentationDeclarationChunk;
  const ACancellation: INyxCancellationScope = nil;
  const AAdmission: INyxDocumentationAdmission = nil): INyxDocumentationDeclarationChunk;
function NewNyxDocumentationPackage(const AChunks: TNyxDocumentationDeclarationChunks;
  const ALimits: TNyxDocumentationContributionLimits;
  const ACancellation: INyxCancellationScope = nil;
  const AAdmission: INyxDocumentationAdmission = nil): INyxDocumentationPackage;
function CaptureNyxDocumentationPackage(const AValue: INyxDocumentationPackage;
  const ACancellation: INyxCancellationScope = nil;
  const AAdmission: INyxDocumentationAdmission = nil): INyxDocumentationPackage;
function NewNyxDocumentationBinding(const APackage: INyxDocumentationPackage;
  const APrimary: TNyxDocumentationRef;
  APlatforms: TNyxDocumentationPlatforms = [];
  const ACancellation: INyxCancellationScope = nil;
  const AAdmission: INyxDocumentationAdmission = nil): INyxDocumentationRegistration;
{ Shared registration handoff recognizes the optional path before invoking its
  old bundle capture. False means legacy; a malformed optional producer raises.
  Mutable alternate suppliers are checked and never retained. }
function TryCaptureNyxDocumentationContributionBinding(
  const AValue: INyxDocumentationRegistration;
  out AFrozen: INyxDocumentationRegistration;
  const ACancellation: INyxCancellationScope = nil;
  const AAdmission: INyxDocumentationAdmission = nil): Boolean;
{ Exact verified captured identity, rather than a producer's numeric claim,
  decides copied ownership. Both arguments must already be captured owners. }
function SameNyxDocumentationChunk(const ALeft, ARight: INyxDocumentationDeclarationChunk): Boolean;
function SameNyxDocumentationPackage(const ALeft, ARight: INyxDocumentationPackage): Boolean;

implementation

uses nyx.bytes;

type
  { Forward canonical chunk class for the private captured-owner receipt. }
  TDeclarationChunk = class;
  { Forward canonical package class for the private captured-owner receipt. }
  TDeclarationPackage = class;
  { Forward immutable registration view, independent of declaration copying. }
  TContributionBinding = class;
  { A private receipt is checked against both the concrete owner and its exact
    canonical public interface, following the source-size requirement owner. }
  IChunkOwner = interface(IInterface)
    ['{A4680A42-26F4-4F00-9BA3-73A4BD13E111}']
    function ChunkOwner: TDeclarationChunk;
  end;
  { A retained package can be recognized without a mutable global owner cache. }
  IPackageOwner = interface(IInterface)
    ['{A4680A42-26F4-4F00-9BA3-73A4BD13E112}']
    function CapturedPackageOwner: TDeclarationPackage;
  end;
  { Only this immutable binding may bypass alternate-provider validation. }
  IBindingOwner = interface(IInterface)
    ['{A4680A42-26F4-4F00-9BA3-73A4BD13E113}']
    function BindingOwner: TContributionBinding;
  end;
  { Capture-local aggregate work/byte owner. Retained chunks charge their exact
    admitted field count in bulk; alternate fields charge before append. }
  TContributionBudget = class(TInterfacedObject, INyxDocumentationAdmission)
  private
    FRemainingDeclarations, FRemainingBytes, FRemainingWork: Integer;
    FCancellation: INyxCancellationScope;
    FOuter: INyxDocumentationContributionAdmission;
    procedure SpendLocal(ACount: Integer);
  public
    constructor Create(const ALimits: TNyxDocumentationContributionLimits;
      const ACancellation: INyxCancellationScope;
      const AAdmission: INyxDocumentationAdmission);
    procedure Spend(ACount: Integer = 1);
    procedure Declarations(ACount: Integer);
    procedure AdmitField(ABytes: Integer);
    procedure CapturedFields(ABytes, AFields: Integer);
  end;
  { A borrowed admission forwarder counts actual framing fields. Neither the
    forwarder nor its caller's budget survives into the immutable chunk. }
  TChunkAdmission = class(TInterfacedObject, INyxDocumentationAdmission)
  public
    ForwardAdmission: INyxDocumentationAdmission;
    LocalBudget: TContributionBudget;
    Fields: Integer;
    procedure AdmitField(ABytes: Integer);
  end;
  { Source declarations/content are privately frozen, with value availability.
    The field count is an admitted copy receipt, never a producer claim. }
  TDeclarationChunk = class(TInterfacedObject, INyxDocumentationDeclarationChunk, IChunkOwner)
  private
    FIdentity, FBytes, FFields: Integer;
    FOrigin: TNyxDocumentationRef;
    FPlatforms: TNyxDocumentationPlatforms;
    FDeclarations: TNyxDocumentationDeclarations;
    FContents: array of TNyxText;
  public
    function ChunkOwner: TDeclarationChunk;
    function Identity: Integer;
    function OriginUnit: TNyxDocumentationRef;
    function Platforms: TNyxDocumentationPlatforms;
    function DeclarationCount: Integer;
    function DeclarationAt(AIndex: Integer): INyxDocumentationType;
    function DeclarationContentAt(AIndex: Integer): TNyxText;
    function ContentBytes: Integer;
  end;
  { Flat packages retain canonical chunks. Different packages can share a core
    chunk without another copy; independent equal chunks retain distinct owners. }
  TDeclarationPackage = class(TInterfacedObject, INyxDocumentationPackage, IPackageOwner)
  private
    FIdentity, FDeclarations: Integer;
    FLimits: TNyxDocumentationContributionLimits;
    FChunks: TNyxDocumentationDeclarationChunks;
  public
    function CapturedPackageOwner: TDeclarationPackage;
    function Identity: Integer;
    function Limits: TNyxDocumentationContributionLimits;
    function ChunkCount: Integer;
    function ChunkAt(AIndex: Integer): INyxDocumentationDeclarationChunk;
    function DeclarationCount: Integer;
    function FindDeclaration(const AIdentity: TNyxDocumentationRef): INyxDocumentationType;
    function FindContent(const AIdentity: TNyxDocumentationRef): TNyxText;
  end;
  { A creator view retains its package and a complete existing declaration;
    it owns no separately captured declaration or executable implementation. }
  TContributionBinding = class(TInterfacedObject, INyxDocumentationRegistration,
    INyxDocumentationContributionBinding, IBindingOwner)
  private
    FPackage: INyxDocumentationPackage;
    FOrigin, FPrimary: TNyxDocumentationRef;
    FPlatforms: TNyxDocumentationPlatforms;
    FDeclaration: INyxDocumentationType;
    FContent: TNyxText;
    FBytes: Integer;
  public
    function BindingOwner: TContributionBinding;
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

var
  GContributionIdentity: Integer;
  {$ifndef PAS2JS}
  GContributionIdentityLock: TRTLCriticalSection;
  {$endif}

procedure Cancelled(const AValue: INyxCancellationScope);
begin

  if (AValue <> nil) and AValue.Cancelled then
    raise ENyxDocumentationCancelled.Create('Documentation contribution capture cancelled');
end;

function NextIdentity: Integer;
begin
  {$ifndef PAS2JS}
  EnterCriticalSection(GContributionIdentityLock);
  {$endif}
  try

    if GContributionIdentity = High(Integer) then
      raise ENyxDocumentation.Create('Documentation contribution identities exhausted');
    Inc(GContributionIdentity);
    Result := GContributionIdentity;
  finally
    {$ifndef PAS2JS}
    LeaveCriticalSection(GContributionIdentityLock);
    {$endif}
  end;
end;

function FramedBytes(const AText: TNyxText): Integer;
var
  LBytes: Integer;
begin
  LBytes := NyxUTF8ByteCount(AText);
  Result := Length(IntToStr(LBytes)) + 1 + LBytes;
end;

class function TNyxDocumentationContributionLimits.Defaults: TNyxDocumentationContributionLimits;
begin
  Result.FDeclarations := 4096;
  Result.FBytes := 16 * 1024 * 1024;
  Result.FWork := 2000000;
end;

function TNyxDocumentationContributionLimits.Declarations(AValue: Integer): TNyxDocumentationContributionLimits;
begin
  Result := Self;
  Result.FDeclarations := AValue;
  Result.Validate;
end;

function TNyxDocumentationContributionLimits.Bytes(AValue: Integer): TNyxDocumentationContributionLimits;
begin
  Result := Self;
  Result.FBytes := AValue;
  Result.Validate;
end;

function TNyxDocumentationContributionLimits.Work(AValue: Integer): TNyxDocumentationContributionLimits;
begin
  Result := Self;
  Result.FWork := AValue;
  Result.Validate;
end;

procedure TNyxDocumentationContributionLimits.Validate;
begin

  if (FDeclarations < 1) or (FDeclarations > 4096) or
    (FBytes < 1) or (FBytes > 16 * 1024 * 1024) or
    (FWork < 1) or (FWork > 2000000) then
    raise ENyxDocumentation.Create('Invalid documentation contribution allowances');
end;

constructor TContributionBudget.Create(const ALimits: TNyxDocumentationContributionLimits;
  const ACancellation: INyxCancellationScope;
  const AAdmission: INyxDocumentationAdmission);
begin
  inherited Create;
  ALimits.Validate;
  FRemainingDeclarations := ALimits.DeclarationLimit;
  FRemainingBytes := ALimits.ByteLimit;
  FRemainingWork := ALimits.WorkLimit;
  FCancellation := ACancellation;
  Supports(AAdmission, INyxDocumentationContributionAdmission, FOuter);
  Spend;
end;

procedure TContributionBudget.SpendLocal(ACount: Integer);
begin
  Cancelled(FCancellation);

  if (ACount < 0) or (ACount > FRemainingWork) then
    raise ENyxDocumentation.Create('Documentation contribution work exhausted');
  Dec(FRemainingWork, ACount);
end;

procedure TContributionBudget.Spend(ACount: Integer);
begin
  SpendLocal(ACount);

  if FOuter <> nil then
    FOuter.AdmitContributionWork(ACount);
end;

procedure TContributionBudget.Declarations(ACount: Integer);
begin
  Spend;

  if (ACount < 1) or (ACount > FRemainingDeclarations) then
    raise ENyxDocumentation.Create('Documentation contribution declaration allowance exhausted');
  Dec(FRemainingDeclarations, ACount);
end;

procedure TContributionBudget.AdmitField(ABytes: Integer);
begin
  CapturedFields(ABytes, 1);
end;

procedure TContributionBudget.CapturedFields(ABytes, AFields: Integer);
begin
  { Field forwarding separately admits these same fields to the outer owner;
    only non-field package operations use Spend's outer work forwarding. }
  SpendLocal(AFields);

  if (ABytes < 0) or (ABytes > FRemainingBytes) then
    raise ENyxDocumentation.Create('Documentation contribution bytes exhausted');
  Dec(FRemainingBytes, ABytes);
end;

procedure TChunkAdmission.AdmitField(ABytes: Integer);
begin

  if LocalBudget <> nil then
    LocalBudget.AdmitField(ABytes);

  if ForwardAdmission <> nil then
    ForwardAdmission.AdmitField(ABytes);
  Inc(Fields);
end;

procedure AdmitWork(const AAdmission: INyxDocumentationAdmission; ACount: Integer = 1);
var
  LOuter: INyxDocumentationContributionAdmission;
begin

  if Supports(AAdmission, INyxDocumentationContributionAdmission, LOuter) then
    LOuter.AdmitContributionWork(ACount);
end;

procedure AdmitDeclarations(const AAdmission: INyxDocumentationAdmission; ACount: Integer);
var
  LOuter: INyxDocumentationContributionAdmission;
begin

  if Supports(AAdmission, INyxDocumentationContributionAdmission, LOuter) then
    LOuter.AdmitContributionDeclarations(ACount);
end;

procedure ReplayFields(const AAdmission: INyxDocumentationAdmission; ABytes, AFields: Integer);
var
  LAt: Integer;
begin

  if AAdmission = nil then
    Exit;
  { The total byte receipt precedes publication; the remaining zero-byte
    fields replay every actual captured field's work, rather than one bulk job. }
  AAdmission.AdmitField(ABytes);
  for LAt := 1 to AFields - 1 do
    AAdmission.AdmitField(0);
end;

function OwnedChunk(const AValue: INyxDocumentationDeclarationChunk): TDeclarationChunk;
var
  LReceipt: IChunkOwner;
begin
  Result := nil;

  if (AValue = nil) or not Supports(AValue, IChunkOwner, LReceipt) then
    Exit;
  Result := LReceipt.ChunkOwner;

  if (Result = nil) or not (Result is TDeclarationChunk) or
    ((Result as INyxDocumentationDeclarationChunk) <> AValue) then
    raise ENyxDocumentation.Create('A declaration chunk has a foreign captured owner');
end;

function OwnedPackage(const AValue: INyxDocumentationPackage): TDeclarationPackage;
var
  LReceipt: IPackageOwner;
begin
  Result := nil;

  if (AValue = nil) or not Supports(AValue, IPackageOwner, LReceipt) then
    Exit;
  Result := LReceipt.CapturedPackageOwner;

  if (Result = nil) or not (Result is TDeclarationPackage) or
    ((Result as INyxDocumentationPackage) <> AValue) then
    raise ENyxDocumentation.Create('A documentation package has a foreign captured owner');
end;

function SameNyxDocumentationChunk(const ALeft, ARight: INyxDocumentationDeclarationChunk): Boolean;
begin

  if (OwnedChunk(ALeft) = nil) or (OwnedChunk(ARight) = nil) then
    raise ENyxDocumentation.Create('Chunk identity requires captured source owners');
  Result := ALeft = ARight;
end;

function SameNyxDocumentationPackage(const ALeft, ARight: INyxDocumentationPackage): Boolean;
begin

  if (OwnedPackage(ALeft) = nil) or (OwnedPackage(ARight) = nil) then
    raise ENyxDocumentation.Create('Package identity requires captured source owners');
  Result := ALeft = ARight;
end;

function BuildChunk(const AOrigin: TNyxDocumentationRef;
  const ADeclarations: TNyxDocumentationDeclarations;
  APlatforms: TNyxDocumentationPlatforms; const ACancellation: INyxCancellationScope;
  const AAdmission: INyxDocumentationAdmission;
  ALocalBudget: TContributionBudget;
  const ASource: INyxDocumentationDeclarationChunk;
  ACount: Integer): INyxDocumentationDeclarationChunk;
var
  LOwner: TDeclarationChunk;
  LLease: INyxDocumentationDeclarationChunk;
  LAdmission: TChunkAdmission;
  LAdmissionLease: INyxDocumentationAdmission;
  LAt, LOther: Integer;
  LCurrent: INyxDocumentationType;
begin
  Result := nil;
  Cancelled(ACancellation);
  AOrigin.Validate;

  if not AOrigin.IsUnit or (ACount < 1) or
    (ACount > NyxDocumentationRegistrationMaximumDeclarations) or
    ((ASource = nil) and (Length(ADeclarations) <> ACount)) then
    raise ENyxDocumentation.Create('A source chunk needs its unit and bounded declarations');
  LOwner := TDeclarationChunk.Create;
  LLease := LOwner;
  LOwner.FOrigin := AOrigin;
  LOwner.FPlatforms := APlatforms;
  LAdmission := TChunkAdmission.Create;
  LAdmissionLease := LAdmission;
  LAdmission.ForwardAdmission := AAdmission;
  LAdmission.LocalBudget := ALocalBudget;
  LOwner.FBytes := FramedBytes(AOrigin.Key);
  LAdmission.AdmitField(LOwner.FBytes);
  SetLength(LOwner.FDeclarations, ACount);
  SetLength(LOwner.FContents, ACount);
  for LAt := 0 to ACount - 1 do
  begin
    Cancelled(ACancellation);

    if ALocalBudget <> nil then
      ALocalBudget.Spend
    else
      AdmitWork(AAdmission);

    if ASource = nil then
      LCurrent := ADeclarations[LAt]
    else
      LCurrent := ASource.DeclarationAt(LAt);
    LOwner.FDeclarations[LAt] := CaptureNyxDocumentationDeclaration(LCurrent,
      AOrigin, LOwner.FContents[LAt], LOwner.FBytes, ACancellation, LAdmissionLease);
    for LOther := 0 to LAt - 1 do
    begin

      if ALocalBudget <> nil then
        ALocalBudget.Spend
      else
        AdmitWork(AAdmission);

      if LOwner.FDeclarations[LAt].Identity.Same(LOwner.FDeclarations[LOther].Identity) then
        raise ENyxDocumentation.Create('Duplicate declaration in one source chunk');
    end;
  end;
  LOwner.FFields := LAdmission.Fields;
  Cancelled(ACancellation);
  LOwner.FIdentity := NextIdentity;
  Cancelled(ACancellation);
  Result := LLease;
end;

function NewNyxDocumentationDeclarationChunk(const AOrigin: TNyxDocumentationRef;
  const ADeclarations: TNyxDocumentationDeclarations;
  APlatforms: TNyxDocumentationPlatforms; const ACancellation: INyxCancellationScope;
  const AAdmission: INyxDocumentationAdmission): INyxDocumentationDeclarationChunk;
var
  LFrozen: INyxDocumentationDeclarationChunk;
  LOuter: INyxDocumentationContributionAdmission;
begin
  Result := nil;
  Cancelled(ACancellation);

  if (Length(ADeclarations) < 1) or
    (Length(ADeclarations) > NyxDocumentationRegistrationMaximumDeclarations) then
    raise ENyxDocumentation.Create('Source chunk declaration allowance exceeded');
  AdmitDeclarations(AAdmission, Length(ADeclarations));
  LFrozen := BuildChunk(AOrigin, ADeclarations, APlatforms, ACancellation,
    AAdmission, nil, nil, Length(ADeclarations));

  if Supports(AAdmission, INyxDocumentationContributionAdmission, LOuter) then
    LOuter.RememberContributionChunk(LFrozen);
  Cancelled(ACancellation);
  Result := LFrozen;
end;

function CaptureChunk(const AValue: INyxDocumentationDeclarationChunk;
  const ACancellation: INyxCancellationScope;
  const AAdmission: INyxDocumentationAdmission;
  ALocalBudget: TContributionBudget): INyxDocumentationDeclarationChunk;
var
  LSource: INyxDocumentationDeclarationChunk;
  LOwned: TDeclarationChunk;
  LOrigin: TNyxDocumentationRef;
  LPlatforms: TNyxDocumentationPlatforms;
  LCount: Integer;
  LFrozen: INyxDocumentationDeclarationChunk;
  LOuter: INyxDocumentationContributionAdmission;
  LAdmitted: Boolean;
begin
  LSource := AValue;
  Result := nil;
  Cancelled(ACancellation);

  if LSource = nil then
    raise ENyxDocumentation.Create('A documentation source chunk is required');
  LOwned := OwnedChunk(LSource);

  if LOwned <> nil then
  begin
    LAdmitted := Supports(AAdmission, INyxDocumentationContributionAdmission, LOuter);

    if LAdmitted then
      LAdmitted := LOuter.ContributionChunkAdmitted(LSource);

    if ALocalBudget <> nil then
    begin
      ALocalBudget.Declarations(Length(LOwned.FDeclarations));
      ALocalBudget.CapturedFields(LOwned.FBytes, LOwned.FFields);
    end;

    if not LAdmitted then
    begin
      AdmitDeclarations(AAdmission, Length(LOwned.FDeclarations));
      ReplayFields(AAdmission, LOwned.FBytes, LOwned.FFields);

      if LOuter <> nil then
        LOuter.RememberContributionChunk(LSource);
    end;
    Cancelled(ACancellation);
    Exit(LSource);
  end;
  LCount := LSource.DeclarationCount;

  if (LCount < 1) or (LCount > NyxDocumentationRegistrationMaximumDeclarations) then
    raise ENyxDocumentation.Create('Source chunk declaration allowance exceeded');

  if ALocalBudget <> nil then
    ALocalBudget.Declarations(LCount);
  AdmitDeclarations(AAdmission, LCount);
  LOrigin := LSource.OriginUnit;
  LPlatforms := LSource.Platforms;

  if (LCount <> LSource.DeclarationCount) or not LOrigin.Same(LSource.OriginUnit) or
    (LPlatforms <> LSource.Platforms) then
    raise ENyxDocumentation.Create('Source chunk changed during capture');
  { Borrow and freeze one declaration at a time. An exhausted field allowance
    therefore refuses before reading any following source declaration. }
  LFrozen := BuildChunk(LOrigin, nil, LPlatforms,
    ACancellation, AAdmission, ALocalBudget, LSource, LCount);

  if (LCount <> LSource.DeclarationCount) or not LOrigin.Same(LSource.OriginUnit) or
    (LPlatforms <> LSource.Platforms) then
    raise ENyxDocumentation.Create('Source chunk changed before publication');

  if Supports(AAdmission, INyxDocumentationContributionAdmission, LOuter) then
    LOuter.RememberContributionChunk(LFrozen);
  Cancelled(ACancellation);
  Result := LFrozen;
end;

function CaptureNyxDocumentationDeclarationChunk(const AValue: INyxDocumentationDeclarationChunk;
  const ACancellation: INyxCancellationScope;
  const AAdmission: INyxDocumentationAdmission): INyxDocumentationDeclarationChunk;
begin
  Result := nil;
  Result := CaptureChunk(AValue, ACancellation, AAdmission, nil);
end;

function NewNyxDocumentationPackage(const AChunks: TNyxDocumentationDeclarationChunks;
  const ALimits: TNyxDocumentationContributionLimits;
  const ACancellation: INyxCancellationScope;
  const AAdmission: INyxDocumentationAdmission): INyxDocumentationPackage;
var
  LOwner: TDeclarationPackage;
  LLease: INyxDocumentationPackage;
  LBudget: TContributionBudget;
  LBudgetLease: INyxDocumentationAdmission;
  LChunk: INyxDocumentationDeclarationChunk;
  LAt, LOther, LDeclaration, LPrevious: Integer;
  LExisting, LCurrent: INyxDocumentationType;
  LOuter: INyxDocumentationContributionAdmission;

  procedure Reference(const AText: TNyxText);
  begin
    LBudget.AdmitField(FramedBytes(AText));
    AdmitWork(AAdmission);

    if AAdmission <> nil then
      AAdmission.AdmitField(NyxUTF8ByteCount(AText));
  end;

begin
  Result := nil;
  ALimits.Validate;
  Cancelled(ACancellation);

  if (Length(AChunks) < 1) or (Length(AChunks) > ALimits.DeclarationLimit) then
    raise ENyxDocumentation.Create('A flat package needs bounded nonempty source chunks');
  LOwner := TDeclarationPackage.Create;
  LLease := LOwner;
  LOwner.FLimits := ALimits;
  LBudget := TContributionBudget.Create(ALimits, ACancellation, AAdmission);
  LBudgetLease := LBudget;
  for LAt := 0 to High(AChunks) do
  begin
    LBudget.Spend;

    if AChunks[LAt] = nil then
      raise ENyxDocumentation.Create('A package cannot retain an absent source chunk');
    { Capture reads one bounded source count and reserves both remaining
      allowances before obtaining any alternate declaration or allocating rows. }
    LChunk := CaptureChunk(AChunks[LAt], ACancellation, AAdmission, LBudget);
    Reference(LChunk.OriginUnit.Key);
    Reference(IntToStr(LChunk.Identity));
    for LOther := 0 to LOwner.ChunkCount - 1 do
    begin
      LBudget.Spend;

      if SameNyxDocumentationChunk(LChunk, LOwner.FChunks[LOther]) then
        raise ENyxDocumentation.Create('A flat package repeats the same source chunk reference');
    end;
    for LDeclaration := 0 to LChunk.DeclarationCount - 1 do
    begin
      LBudget.Spend;
      LCurrent := LChunk.DeclarationAt(LDeclaration);
      for LOther := 0 to LOwner.ChunkCount - 1 do
        for LPrevious := 0 to LOwner.FChunks[LOther].DeclarationCount - 1 do
        begin
          LBudget.Spend;
          LExisting := LOwner.FChunks[LOther].DeclarationAt(LPrevious);

          if LExisting.Identity.Same(LCurrent.Identity) then
          begin

            if (LOwner.FChunks[LOther].DeclarationContentAt(LPrevious) <>
              LChunk.DeclarationContentAt(LDeclaration)) or
              (LOwner.FChunks[LOther].Platforms <> LChunk.Platforms) then
              raise ENyxDocumentation.Create('Conflicting declaration in a source package');
          end;
        end;
    end;
    Inc(LOwner.FDeclarations, LChunk.DeclarationCount);
    SetLength(LOwner.FChunks, LAt + 1);
    LOwner.FChunks[LAt] := LChunk;
  end;
  LBudget.Spend;
  LOwner.FIdentity := NextIdentity;

  if Supports(AAdmission, INyxDocumentationContributionAdmission, LOuter) then
    LOuter.RememberContributionReferences(LLease);
  Cancelled(ACancellation);
  Result := LLease;
end;

function CaptureNyxDocumentationPackage(const AValue: INyxDocumentationPackage;
  const ACancellation: INyxCancellationScope;
  const AAdmission: INyxDocumentationAdmission): INyxDocumentationPackage;
var
  LSource: INyxDocumentationPackage;
  LChunks: TNyxDocumentationDeclarationChunks;
  LLimits: TNyxDocumentationContributionLimits;
  LAt, LCount: Integer;
  LFrozen: INyxDocumentationPackage;
begin
  LSource := AValue;
  Result := nil;
  Cancelled(ACancellation);

  if LSource = nil then
    raise ENyxDocumentation.Create('A documentation package is required');

  if OwnedPackage(LSource) <> nil then
  begin
    Cancelled(ACancellation);
    Exit(LSource);
  end;
  AdmitWork(AAdmission);
  LLimits := LSource.Limits;
  LLimits.Validate;
  LCount := LSource.ChunkCount;

  if (LCount < 1) or (LCount > LLimits.DeclarationLimit) then
    raise ENyxDocumentation.Create('Package source chunk allowance exceeded');
  AdmitWork(AAdmission, LCount);
  SetLength(LChunks, LCount);
  for LAt := 0 to LCount - 1 do
  begin
    Cancelled(ACancellation);
    LChunks[LAt] := LSource.ChunkAt(LAt);
  end;

  if (LCount <> LSource.ChunkCount) or
    (LLimits.DeclarationLimit <> LSource.Limits.DeclarationLimit) or
    (LLimits.ByteLimit <> LSource.Limits.ByteLimit) or
    (LLimits.WorkLimit <> LSource.Limits.WorkLimit) then
    raise ENyxDocumentation.Create('Package envelope changed during capture');
  LFrozen := NewNyxDocumentationPackage(LChunks, LLimits, ACancellation, AAdmission);

  if (LCount <> LSource.ChunkCount) or
    (LLimits.DeclarationLimit <> LSource.Limits.DeclarationLimit) or
    (LLimits.ByteLimit <> LSource.Limits.ByteLimit) or
    (LLimits.WorkLimit <> LSource.Limits.WorkLimit) then
    raise ENyxDocumentation.Create('Package envelope changed before publication');
  for LAt := 0 to LCount - 1 do
  begin
    Cancelled(ACancellation);
    AdmitWork(AAdmission);

    if LChunks[LAt] <> LSource.ChunkAt(LAt) then
      raise ENyxDocumentation.Create('Package source references changed during capture');
  end;
  Cancelled(ACancellation);
  Result := LFrozen;
end;

function TDeclarationChunk.ChunkOwner: TDeclarationChunk;
begin
  Result := Self;
end;

function TDeclarationChunk.Identity: Integer;
begin
  Result := FIdentity;
end;

function TDeclarationChunk.OriginUnit: TNyxDocumentationRef;
begin
  Result := FOrigin;
end;

function TDeclarationChunk.Platforms: TNyxDocumentationPlatforms;
begin
  Result := FPlatforms;
end;

function TDeclarationChunk.DeclarationCount: Integer;
begin
  Result := Length(FDeclarations);
end;

function TDeclarationChunk.DeclarationAt(AIndex: Integer): INyxDocumentationType;
begin

  if (AIndex < 0) or (AIndex >= DeclarationCount) then
    raise ENyxDocumentation.Create('Source chunk declaration ordinal is out of range');
  Result := FDeclarations[AIndex];
end;

function TDeclarationChunk.DeclarationContentAt(AIndex: Integer): TNyxText;
begin
  DeclarationAt(AIndex);
  Result := FContents[AIndex];
end;

function TDeclarationChunk.ContentBytes: Integer;
begin
  Result := FBytes;
end;

function TDeclarationPackage.CapturedPackageOwner: TDeclarationPackage;
begin
  Result := Self;
end;

function TDeclarationPackage.Identity: Integer;
begin
  Result := FIdentity;
end;

function TDeclarationPackage.Limits: TNyxDocumentationContributionLimits;
begin
  Result := FLimits;
end;

function TDeclarationPackage.ChunkCount: Integer;
begin
  Result := Length(FChunks);
end;

function TDeclarationPackage.ChunkAt(AIndex: Integer): INyxDocumentationDeclarationChunk;
begin

  if (AIndex < 0) or (AIndex >= ChunkCount) then
    raise ENyxDocumentation.Create('Package chunk ordinal is out of range');
  Result := FChunks[AIndex];
end;

function TDeclarationPackage.DeclarationCount: Integer;
begin
  Result := FDeclarations;
end;

function TDeclarationPackage.FindDeclaration(const AIdentity: TNyxDocumentationRef): INyxDocumentationType;
var
  LChunk, LAt: Integer;
begin
  Result := nil;
  for LChunk := 0 to ChunkCount - 1 do
    for LAt := 0 to FChunks[LChunk].DeclarationCount - 1 do
    begin

      if FChunks[LChunk].DeclarationAt(LAt).Identity.Same(AIdentity) then
        Exit(FChunks[LChunk].DeclarationAt(LAt));
    end;
end;

function TDeclarationPackage.FindContent(const AIdentity: TNyxDocumentationRef): TNyxText;
var
  LChunk, LAt: Integer;
begin
  Result := '';
  for LChunk := 0 to ChunkCount - 1 do
    for LAt := 0 to FChunks[LChunk].DeclarationCount - 1 do
    begin

      if FChunks[LChunk].DeclarationAt(LAt).Identity.Same(AIdentity) then
        Exit(FChunks[LChunk].DeclarationContentAt(LAt));
    end;
end;

function NewNyxDocumentationBinding(const APackage: INyxDocumentationPackage;
  const APrimary: TNyxDocumentationRef; APlatforms: TNyxDocumentationPlatforms;
  const ACancellation: INyxCancellationScope;
  const AAdmission: INyxDocumentationAdmission): INyxDocumentationRegistration;
var
  LOwner: TContributionBinding;
  LLease: INyxDocumentationRegistration;
  LPackage: INyxDocumentationPackage;
  LChunkOwner: INyxDocumentationDeclarationChunk;
  LCurrent: INyxDocumentationType;
  LRoot: TNyxDocumentationRef;
  LAt, LChunk, LDeclaration: Integer;
  LFound: Boolean;
begin
  Result := nil;
  LPackage := CaptureNyxDocumentationPackage(APackage, ACancellation, AAdmission);
  APrimary.Validate;

  if APrimary.IsUnit then
    raise ENyxDocumentation.Create('A contribution binding needs its public type or unit factory');
  LRoot := APrimary;

  if APrimary.IsMember then
    LRoot := APrimary.Owner;
  LOwner := TContributionBinding.Create;
  LLease := LOwner;
  for LChunk := 0 to LPackage.ChunkCount - 1 do
  begin
    AdmitWork(AAdmission);
    LChunkOwner := LPackage.ChunkAt(LChunk);
    for LDeclaration := 0 to LChunkOwner.DeclarationCount - 1 do
    begin
      Cancelled(ACancellation);
      AdmitWork(AAdmission);
      LCurrent := LChunkOwner.DeclarationAt(LDeclaration);

      if LCurrent.Identity.Same(LRoot) then
      begin

        if (LChunkOwner.Platforms <> []) and ((APlatforms - LChunkOwner.Platforms) <> []) then
          raise ENyxDocumentation.Create('Binding availability exceeds its declared source availability');
        LOwner.FDeclaration := LCurrent;
        LOwner.FContent := LChunkOwner.DeclarationContentAt(LDeclaration);
      end;
    end;
  end;

  if LOwner.FDeclaration = nil then
    raise ENyxDocumentation.Create('Contribution primary declaration is absent');
  LFound := not APrimary.IsMember;

  if APrimary.IsMember then
    for LAt := 0 to LOwner.FDeclaration.MemberCount - 1 do
    begin
      AdmitWork(AAdmission);

      if LOwner.FDeclaration.MemberAt(LAt).Identity.Same(APrimary) then
      begin

        if not APrimary.Owner.IsUnit or
          (LOwner.FDeclaration.MemberAt(LAt).Kind <> ndmFactory) then
          raise ENyxDocumentation.Create('A contribution member primary must be an actual unit factory');
        LFound := True;
      end;
    end;

  if not LFound then
    raise ENyxDocumentation.Create('Contribution factory primary is absent');
  LOwner.FPackage := LPackage;
  LOwner.FPrimary := APrimary;
  LOwner.FOrigin := NyxDocumentationUnit(APrimary.UnitName);
  LOwner.FPlatforms := APlatforms;
  LOwner.FBytes := FramedBytes(LOwner.FOrigin.Key) + FramedBytes(APrimary.Key) +
    NyxUTF8ByteCount(LOwner.FContent);

  if LOwner.FBytes > NyxDocumentationRegistrationMaximumBytes then
    raise ENyxDocumentation.Create('A contribution binding view exceeds the legacy bundle ceiling');
  Cancelled(ACancellation);
  Result := LLease;
end;

function TryCaptureNyxDocumentationContributionBinding(
  const AValue: INyxDocumentationRegistration;
  out AFrozen: INyxDocumentationRegistration;
  const ACancellation: INyxCancellationScope;
  const AAdmission: INyxDocumentationAdmission): Boolean;
var
  LSource: INyxDocumentationRegistration;
  LCapability: INyxDocumentationContributionBinding;
  LReceipt: IBindingOwner;
  LOwner: TContributionBinding;
  LPackage, LSourcePackage: INyxDocumentationPackage;
  LOrigin, LPrimary: TNyxDocumentationRef;
  LPlatforms: TNyxDocumentationPlatforms;
  LRebuilt: INyxDocumentationRegistration;
  LDeclaration: INyxDocumentationType;
  LContent: TNyxText;
  LBytes: Integer;
begin
  LSource := AValue;
  AFrozen := nil;
  Result := False;
  Cancelled(ACancellation);

  if (LSource = nil) or not Supports(LSource, INyxDocumentationContributionBinding, LCapability) then
    Exit;
  Result := True;

  if Supports(LSource, IBindingOwner, LReceipt) then
  begin
    LOwner := LReceipt.BindingOwner;

    if (LOwner = nil) or not (LOwner is TContributionBinding) or
      ((LOwner as INyxDocumentationRegistration) <> LSource) then
      raise ENyxDocumentation.Create('A contribution binding has a foreign captured owner');
    Cancelled(ACancellation);
    AFrozen := LSource;
    Exit;
  end;
  LOrigin := LSource.OriginUnit;
  LPrimary := LSource.Primary;
  LPlatforms := LSource.Platforms;

  if LSource.DeclarationCount <> 1 then
    raise ENyxDocumentation.Create('A contribution binding view owns exactly one complete primary declaration');
  LSourcePackage := LCapability.PackageOwner;
  LPackage := CaptureNyxDocumentationPackage(LSourcePackage, ACancellation, AAdmission);
  LRebuilt := NewNyxDocumentationBinding(LPackage, LPrimary, LPlatforms, ACancellation, AAdmission);

  if not LOrigin.Same(LRebuilt.OriginUnit) then
    raise ENyxDocumentation.Create('Contribution binding origin contradicts its primary');
  LBytes := 0;
  AdmitWork(AAdmission);
  LDeclaration := CaptureNyxDocumentationDeclaration(LSource.DeclarationAt(0), LOrigin,
    LContent, LBytes, ACancellation, AAdmission);

  if not LDeclaration.Identity.Same(LRebuilt.DeclarationAt(0).Identity) or
    (LContent <> LRebuilt.DeclarationContentAt(0)) or (LSource.DeclarationCount <> 1) or
    not LOrigin.Same(LSource.OriginUnit) or not LPrimary.Same(LSource.Primary) or
    (LPlatforms <> LSource.Platforms) or (LSourcePackage <> LCapability.PackageOwner) then
    raise ENyxDocumentation.Create('Contribution binding changed or contradicts its captured package');
  Cancelled(ACancellation);
  AFrozen := LRebuilt;
end;

function TContributionBinding.BindingOwner: TContributionBinding;
begin
  Result := Self;
end;

function TContributionBinding.PackageOwner: INyxDocumentationPackage;
begin
  Result := FPackage;
end;

function TContributionBinding.OriginUnit: TNyxDocumentationRef;
begin
  Result := FOrigin;
end;

function TContributionBinding.Primary: TNyxDocumentationRef;
begin
  Result := FPrimary;
end;

function TContributionBinding.Platforms: TNyxDocumentationPlatforms;
begin
  Result := FPlatforms;
end;

function TContributionBinding.DeclarationCount: Integer;
begin
  Result := 1;
end;

function TContributionBinding.DeclarationAt(AIndex: Integer): INyxDocumentationType;
begin

  if AIndex <> 0 then
    raise ENyxDocumentation.Create('Contribution primary declaration ordinal is out of range');
  Result := FDeclaration;
end;

function TContributionBinding.DeclarationContentAt(AIndex: Integer): TNyxText;
begin
  DeclarationAt(AIndex);
  Result := FContent;
end;

function TContributionBinding.ContentBytes: Integer;
begin
  Result := FBytes;
end;

function TContributionBinding.ForPrimary(const APrimary: TNyxDocumentationRef): INyxDocumentationRegistration;
begin

  if not NyxDocumentationUnit(APrimary.UnitName).Same(FOrigin) then
    raise ENyxDocumentation.Create('ForPrimary cannot claim another original source unit');
  Result := NewNyxDocumentationBinding(FPackage, APrimary, FPlatforms);
end;

{$ifndef PAS2JS}
initialization
  InitCriticalSection(GContributionIdentityLock);

finalization
  DoneCriticalSection(GContributionIdentityLock);
{$endif}

end.
