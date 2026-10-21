{ Checked source-selection inputs for declaration-owned documentation. MIT. }
unit nyx.documentation.source;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.text, nyx.scheduler, nyx.pascal.binding,
  nyx.documentation, nyx.documentation.registration,
  nyx.documentation.declarations, nyx.documentation.typeexpressions;

type
  { A complete source selection cannot be represented or bound faithfully.
    Refusal confers no permission to omit a declaration or invent a dependency. }
  ENyxDocumentationSource = class(ENyxDocumentation);
  { A selection addresses one declaration in the supplied immutable world.
    DeclarationIndex=-1 selects the complete public globals of that module.
    These operation-local ordinals never become persistent documentation keys. }
  TNyxDocumentationSourceScope = record
    ModuleIndex: Integer;
    DeclarationIndex: Integer;
  end;
  { Named managed input storage permits a local value lease before an outward
    admission callback; shared element changes still require final agreement. }
  TNyxDocumentationSourceScopes = array of TNyxDocumentationSourceScope;
  { Creator role is supplied by the actual registration/generator owner for
    this exact global declaration. Its spelling never infers factory authority. }
  TNyxDocumentationSourceCreator = record
    ModuleIndex: Integer;
    DeclarationIndex: Integer;
  end;
  { The adapter copies and validates every explicit creator annotation against
    selected complete unit globals, retaining no caller-owned mutable vector. }
  TNyxDocumentationSourceCreators = array of TNyxDocumentationSourceCreator;

{ Capture complete selected public scopes and their genuine referenced type
  closure. Only Nyx's exact immutable world/projection owners are admitted;
  this proves implementation ownership, never compiler or filesystem authority.
  Translation contexts, physical/include origins and binding prerequisites stay
  with their existing owners. No source loading or declaration execution occurs.
  The construction is borrowed for this entire operation and is not retired. }
function CaptureNyxDocumentationSourcePackage(
  const AWorld: INyxPascalSemanticWorld;
  const AScopes: TNyxDocumentationSourceScopes;
  const ACreators: TNyxDocumentationSourceCreators;
  APlatforms: TNyxDocumentationPlatforms;
  const AConstruction: INyxDocumentationTypeConstruction): INyxDocumentationPackage;

implementation

uses nyx.bytes, nyx.pascal.lexing, nyx.pascal.syntax,
  nyx.pascal.syntax.translated, nyx.pascal.types, nyx.pascal.work,
  nyx.documentation.signatures, nyx.pascal.names, nyx.pascal.context;

type
  { An operation-local typed parameter supplier preserves an already bound use
    without invoking a public constructor that starts an independent capture. }
  TSourceParameter = class(TInterfacedObject, INyxDocumentationParameterDeclaration)
  public
    ParameterName, DefaultText: TNyxText;
    ParameterType: INyxDocumentationTypeUse;
    Mode: TNyxDocumentationParameterMode;
    DefaultPresent: Boolean;
    function Name: TNyxText;
    function TypeUse: INyxDocumentationTypeUse;
    function Passing: TNyxDocumentationParameterMode;
    function HasDefault: Boolean;
    function DefaultPascal: TNyxText;
    function WithDefault(const APascal: TNyxText): INyxDocumentationParameterDeclaration;
  end;
  { Source suppliers are private construction values. One final ordinary
    capture freezes their member vectors; fluent methods are never executed. }
  TSourceType = class(TInterfacedObject, INyxDocumentationType)
  public
    ID, AliasID: TNyxDocumentationRef;
    TypeKind: TNyxDocumentationTypeKind;
    Bases: array of TNyxDocumentationRef;
    Members: array of INyxDocumentationMember;
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
  { Exact world ordinals and a stable authored identity permit recursive type
    references without recursively copying the same declaration. In-progress
    entries expose only identity; no partially constructed package escapes. }
  TSourceEntry = class
  public
    ModuleIndex, DeclarationIndex: Integer;
    ID: TNyxDocumentationRef;
    Value: INyxDocumentationType;
    Building: Boolean;
  end;
  { A checked module lease owns reusable scalar windows and a single declared
    signature session across all selected scopes. Both retire before package
    publication; repeated scopes never revalidate the whole immutable unit. }
  TSourceModule = class
  public
    ModuleIndex: Integer;
    View: INyxPascalTranslatedSyntax;
    Syntax: INyxPascalSyntaxSnapshot;
    Windows: INyxDocumentationTextWindows;
    SignatureSource: INyxDocumentationSignatureSource;
    UnitID: TNyxDocumentationRef;
    RoutineOrdinals: TNyxDocumentationRoutineOrdinals;
    RoutineSignatures: TNyxDocumentationRoutineSignatures;
    destructor Destroy; override;
  end;
  { All source traversal, binding work and contribution framing share one
    caller-owned construction. Canonical chunk receipts charge copied facts
    once; independently captured equivalent chunks never earn that receipt. }
  TSourceCapture = class(TInterfacedObject, INyxCancellationScope,
    INyxPascalAnalysisBudget, INyxDocumentationAdmission,
    INyxDocumentationContributionAdmission)
  private
    FWorld: INyxPascalSemanticWorld;
    FConstruction: INyxDocumentationTypeConstruction;
    FExecution: INyxCancellationScope;
    FScopes: TNyxDocumentationSourceScopes;
    FCreators: TNyxDocumentationSourceCreators;
    FPlatforms: TNyxDocumentationPlatforms;
    FModules: array of TSourceModule;
    FEntries: array of TSourceEntry;
    FRoots: array of INyxDocumentationCompilerRoot;
    FChunks: TNyxDocumentationDeclarationChunks;
    FPackages: TNyxDocumentationPackages;
    FBytes, FDeclarations: Integer;
    FNamespaceDepth: Integer;
    FRetired, FCancelled: Boolean;
    procedure Check;
    procedure Work(AUnits: Integer = 1);
    function Module(AModule: Integer): TSourceModule;
    procedure NeedDeclaration(const AValue: TNyxPascalSyntaxDeclaration);
    function PublicDeclaration(const AValue: TNyxPascalSyntaxDeclaration): Boolean;
    function CompleteType(AModule, ADeclaration: Integer): Integer;
    function TypeReference(AModule, ADeclaration: Integer): TNyxDocumentationRef;
    function ResolutionReference(const AValue: INyxPascalResolution): TNyxDocumentationRef;
    function ResolutionRoot(const AValue: INyxPascalResolution): INyxDocumentationCompilerRoot;
    function BindUse(AModule, ADeclaration: Integer;
      const AValue: INyxDocumentationTypeUse): INyxDocumentationTypeUse;
    function TypeUse(AModule, ADeclaration: Integer;
      const ASpan: TNyxPascalSpan;
      ARole: TNyxDocumentationTypeOccurrence): INyxDocumentationTypeUse;
    function Selector(AModule, ADeclaration: Integer): TNyxText;
    function IsCreator(AModule, ADeclaration: Integer): Boolean;
    function Member(AModule, ADeclaration: Integer;
      const AOwner: TNyxDocumentationRef): INyxDocumentationMember;
    procedure PrepareSignatures(AModule, AScope: Integer);
    function Signature(AModule, ADeclaration: Integer): INyxDocumentationRichSignature;
    procedure Members(AOwner: TSourceType; AModule, AScope: Integer;
      AGlobals: Boolean);
    procedure Ancestry(AOwner: TSourceType; AModule, ADeclaration: Integer;
      out AInterfaces: INyxDocumentationInterfaceImplementations);
    function CaptureType(AModule, ADeclaration: Integer): TNyxDocumentationRef;
    procedure CaptureGlobals(AModule: Integer);
    function PackageValue: INyxDocumentationPackage;
    procedure CheckInputs(const AScopes: TNyxDocumentationSourceScopes;
      const ACreators: TNyxDocumentationSourceCreators);
  public
    constructor Create(const AWorld: INyxPascalSemanticWorld;
      const AConstruction: INyxDocumentationTypeConstruction;
      APlatforms: TNyxDocumentationPlatforms);
    destructor Destroy; override;
    procedure Cancel;
    function GetCancelled: Boolean;
    procedure Admit(APhase: TNyxPascalAnalysisPhase; AUnits: Integer = 1);
    function Receipt: TNyxPascalAnalysisReceipt;
    procedure Retire;
    procedure AdmitField(ABytes: Integer);
    procedure AdmitContributionWork(ACount: Integer);
    procedure AdmitContributionDeclarations(ACount: Integer);
    function ContributionChunkAdmitted(const AChunk: INyxDocumentationDeclarationChunk): Boolean;
    procedure RememberContributionChunk(const AChunk: INyxDocumentationDeclarationChunk);
    function ContributionReferencesAdmitted(const APackage: INyxDocumentationPackage): Boolean;
    procedure RememberContributionReferences(const APackage: INyxDocumentationPackage);
    function Capture(const AScopes: TNyxDocumentationSourceScopes;
      const ACreators: TNyxDocumentationSourceCreators): INyxDocumentationPackage;
  end;

function TSourceParameter.Name: TNyxText;
begin
  Result := ParameterName;
end;

function TSourceParameter.TypeUse: INyxDocumentationTypeUse;
begin
  Result := ParameterType;
end;

function TSourceParameter.Passing: TNyxDocumentationParameterMode;
begin
  Result := Mode;
end;

function TSourceParameter.HasDefault: Boolean;
begin
  Result := DefaultPresent;
end;

function TSourceParameter.DefaultPascal: TNyxText;
begin
  Result := DefaultText;
end;

function TSourceParameter.WithDefault(const APascal: TNyxText): INyxDocumentationParameterDeclaration;
begin
  Result := nil;
  raise ENyxDocumentationSource.Create('A source parameter cannot execute fluent replacement: ' + APascal);
end;

function TSourceType.Identity: TNyxDocumentationRef;
begin
  Result := ID;
end;

function TSourceType.Kind: TNyxDocumentationTypeKind;
begin
  Result := TypeKind;
end;

function TSourceType.Summary: TNyxText;
begin
  { No prose is inferred from a type name or a second help inventory. }
  Result := '';
end;

function TSourceType.Evidence: TNyxDocumentationEvidence;
begin
  Result := ndeAuthoredDeclaration;
end;

function TSourceType.AliasTarget: TNyxDocumentationRef;
begin
  Result := AliasID;
end;

function TSourceType.BaseCount: Integer;
begin
  Result := Length(Bases);
end;

function TSourceType.BaseAt(AIndex: Integer): TNyxDocumentationRef;
begin

  if (AIndex < 0) or (AIndex >= Length(Bases)) then
    raise ENyxDocumentationSource.Create('Source base ordinal is outside its captured vector');
  Result := Bases[AIndex];
end;

function TSourceType.MemberCount: Integer;
begin
  Result := Length(Members);
end;

function TSourceType.MemberAt(AIndex: Integer): INyxDocumentationMember;
begin

  if (AIndex < 0) or (AIndex >= Length(Members)) then
    raise ENyxDocumentationSource.Create('Source member ordinal is outside its captured vector');
  Result := Members[AIndex];
end;

function TSourceType.ExampleCount: Integer;
begin
  Result := 0;
end;

function TSourceType.ExampleAt(AIndex: Integer): TNyxDocumentationExample;
begin
  Result := Default(TNyxDocumentationExample);
  raise ENyxDocumentationSource.CreateFmt('Source examples are absent: %d', [AIndex]);
end;

function TSourceType.RelatedCount: Integer;
begin
  Result := 0;
end;

function TSourceType.RelatedAt(AIndex: Integer): TNyxDocumentationRef;
begin
  Result := Default(TNyxDocumentationRef);
  raise ENyxDocumentationSource.CreateFmt('Source related references are absent: %d', [AIndex]);
end;

function TSourceType.WithAlias(const ATarget: TNyxDocumentationRef): INyxDocumentationType;
begin
  Result := nil;
  raise ENyxDocumentationSource.Create('A source supplier cannot execute fluent alias replacement: ' + ATarget.Key);
end;

function TSourceType.WithBase(const ABase: TNyxDocumentationRef): INyxDocumentationType;
begin
  Result := nil;
  raise ENyxDocumentationSource.Create('A source supplier cannot execute fluent ancestry replacement: ' + ABase.Key);
end;

function TSourceType.WithMember(const AMember: INyxDocumentationMember): INyxDocumentationType;
begin
  Result := nil;

  if AMember = nil then
    raise ENyxDocumentationSource.Create('A source supplier cannot append an absent member');
  raise ENyxDocumentationSource.Create('A source supplier cannot execute fluent member replacement');
end;

function TSourceType.WithExample(const AValue: TNyxDocumentationExample): INyxDocumentationType;
begin
  Result := nil;
  raise ENyxDocumentationSource.Create('A source supplier cannot execute example replacement: ' + AValue.Title);
end;

function TSourceType.WithRelated(const AValue: TNyxDocumentationRef): INyxDocumentationType;
begin
  Result := nil;
  raise ENyxDocumentationSource.Create('A source supplier cannot execute related replacement: ' + AValue.Key);
end;

destructor TSourceModule.Destroy;
begin

  if SignatureSource <> nil then
    SignatureSource.Retire;
  SignatureSource := nil;

  if Windows <> nil then
    Windows.Retire;
  Windows := nil;
  Syntax := nil;
  View := nil;
  inherited Destroy;
end;

constructor TSourceCapture.Create(const AWorld: INyxPascalSemanticWorld;
  const AConstruction: INyxDocumentationTypeConstruction;
  APlatforms: TNyxDocumentationPlatforms);
begin
  inherited Create;
  FWorld := AWorld;
  FConstruction := AConstruction;
  FPlatforms := APlatforms;
  Check;

  if not NyxPascalOwnedSemanticWorld(FWorld) then
    raise ENyxDocumentationSource.Create('Source capture requires the exact immutable Nyx world owner');
end;

destructor TSourceCapture.Destroy;
begin
  Retire;
  inherited Destroy;
end;

procedure TSourceCapture.Check;
begin

  if FRetired or FCancelled then
    raise ENyxDocumentationCancelled.Create('Source documentation capture retired or cancelled');
  CheckNyxDocumentationTypeConstruction(FConstruction);
end;

procedure TSourceCapture.Work(AUnits: Integer);
begin
  Check;
  FConstruction.Spend(AUnits);
  Check;
end;

procedure TSourceCapture.Cancel;
begin
  FCancelled := True;
end;

function TSourceCapture.GetCancelled: Boolean;
begin
  Result := FRetired or FCancelled;

  if not Result then
    Result := FConstruction.Cancelled;
end;

procedure TSourceCapture.Admit(APhase: TNyxPascalAnalysisPhase; AUnits: Integer);
begin

  if (Ord(APhase) < Ord(Low(TNyxPascalAnalysisPhase))) or
    (Ord(APhase) > Ord(High(TNyxPascalAnalysisPhase))) then
    raise ENyxDocumentationSource.Create('Unknown source capture analysis phase');
  Work(AUnits);
end;

function TSourceCapture.Receipt: TNyxPascalAnalysisReceipt;
begin
  Check;
  Result := FConstruction.Receipt;
end;

procedure TSourceCapture.Retire;
var
  LAt: Integer;
begin

  if FRetired then
    Exit;
  FRetired := True;
  for LAt := 0 to Length(FModules) - 1 do
    FModules[LAt].Free;
  FModules := nil;
  for LAt := 0 to Length(FEntries) - 1 do
    FEntries[LAt].Free;
  FEntries := nil;
  FRoots := nil;
  FChunks := nil;
  FPackages := nil;
  FWorld := nil;
  FConstruction := nil;
  { The public entry still holds its independent local interface lease while
    this temporary query-execution self-reference is broken. }
  FExecution := nil;
end;

procedure TSourceCapture.AdmitField(ABytes: Integer);
var
  LMaximum: Integer;
begin
  Work;
  LMaximum := TNyxDocumentationContributionLimits.Defaults.ByteLimit;

  if (ABytes < 0) or (ABytes > LMaximum - FBytes) then
    raise ENyxDocumentationSource.Create('Source package framing exceeds its aggregate byte allowance');
  Inc(FBytes, ABytes);
end;

procedure TSourceCapture.AdmitContributionWork(ACount: Integer);
begin
  Work(ACount);
end;

procedure TSourceCapture.AdmitContributionDeclarations(ACount: Integer);
begin
  Work;

  if (ACount < 0) or
    (ACount > TNyxDocumentationContributionLimits.Defaults.DeclarationLimit - FDeclarations) then
    raise ENyxDocumentationSource.Create('Source package exceeds its aggregate declaration allowance');
  Inc(FDeclarations, ACount);
end;

function TSourceCapture.ContributionChunkAdmitted(const AChunk: INyxDocumentationDeclarationChunk): Boolean;
var
  LAt: Integer;
begin
  Result := False;
  for LAt := 0 to Length(FChunks) - 1 do
  begin
    Work;

    if SameNyxDocumentationChunk(FChunks[LAt], AChunk) then
      Exit(True);
  end;
end;

procedure TSourceCapture.RememberContributionChunk(const AChunk: INyxDocumentationDeclarationChunk);
var
  LCount: Integer;
begin

  if ContributionChunkAdmitted(AChunk) then
    Exit;

  if not SameNyxDocumentationChunk(AChunk, AChunk) then
    raise ENyxDocumentationSource.Create('Only an exact canonical source chunk can earn a copy receipt');
  LCount := Length(FChunks);
  Work(LCount + 1);
  SetLength(FChunks, LCount + 1);
  FChunks[LCount] := AChunk;
end;

function TSourceCapture.ContributionReferencesAdmitted(const APackage: INyxDocumentationPackage): Boolean;
var
  LAt: Integer;
begin
  Result := False;
  for LAt := 0 to Length(FPackages) - 1 do
  begin
    Work;

    if SameNyxDocumentationPackage(FPackages[LAt], APackage) then
      Exit(True);
  end;
end;

procedure TSourceCapture.RememberContributionReferences(const APackage: INyxDocumentationPackage);
var
  LCount: Integer;
begin

  if ContributionReferencesAdmitted(APackage) then
    Exit;

  if not SameNyxDocumentationPackage(APackage, APackage) then
    raise ENyxDocumentationSource.Create('Only an exact canonical package can earn a reference receipt');
  LCount := Length(FPackages);
  Work(LCount + 1);
  SetLength(FPackages, LCount + 1);
  FPackages[LCount] := APackage;
end;

function TSourceCapture.Module(AModule: Integer): TSourceModule;
var
  LAt, LCount, LImportAt, LImportModule, LBoundary: Integer;
  LValue: TSourceModule;
  LRoot: TNyxPascalSyntaxScope;
  LImport: TNyxPascalSyntaxImport;
begin
  Work;

  if (AModule < 0) or (AModule >= FWorld.Count) then
    raise ENyxDocumentationSource.Create('Source module ordinal is outside the exact world');
  for LAt := 0 to Length(FModules) - 1 do
  begin
    Work;

    if FModules[LAt].ModuleIndex = AModule then
      Exit(FModules[LAt]);
  end;
  LValue := TSourceModule.Create;
  try
    LValue.ModuleIndex := AModule;
    LValue.View := FWorld.ModuleAt(AModule);

    if not NyxPascalOwnedTranslatedSyntax(LValue.View) or
      not LValue.View.CompleteTranslation or not LValue.View.Source.Resolved or
      (LValue.View.BlockedAt <> -1) or (LValue.View.Source.DiagnosticCount <> 0) then
      raise ENyxDocumentationSource.Create('Source module requires its complete resolved owned translation');
    LValue.Syntax := LValue.View.Syntax;

    if (LValue.Syntax.DiagnosticCount <> 0) or
      (LValue.Syntax.Lexical.DiagnosticCount <> 0) or (LValue.Syntax.ScopeCount < 1) then
      raise ENyxDocumentationSource.Create('Source module has lexical or declaration diagnostics');
    LRoot := LValue.Syntax.Scope(0);

    if (LRoot.Kind <> psskModule) or not LRoot.Complete or LRoot.UnsupportedSyntax then
      raise ENyxDocumentationSource.Create('Source module scope is incomplete or unsupported');
    LValue.UnitID := NyxDocumentationUnit(LRoot.Name);
    LValue.Windows := NewNyxDocumentationTextWindows(LValue.Syntax.Lexical.Source, FConstruction);
    LCount := Length(FModules);
    Work(LCount + 1);
    SetLength(FModules, LCount + 1);
    FModules[LCount] := LValue;
    Result := LValue;
    LValue := nil;
    { Publish into this operation's private module directory before following
      cyclic imports. A failed descendant still discards the entire operation;
      being present in World alone never certifies namespace closure. }

    if FNamespaceDepth >= FWorld.Options.MaximumDepth then
      raise ENyxDocumentationSource.Create('Actual public namespace closure exceeds its existing binding depth');
    Inc(FNamespaceDepth);
    try
      LBoundary := FWorld.InterfaceBoundary(AModule);
      for LImportAt := 0 to Result.Syntax.ImportCount - 1 do
      begin
        Work;
        LImport := Result.Syntax.ImportAt(LImportAt);

        if (LImport.Scope <> 0) or (LImport.Span.Start >= LBoundary) then
          Continue;

        if LImport.Conditional then
          raise ENyxDocumentationSource.Create('A public imported namespace has uncertain source participation');
        Work;
        LImportModule := FWorld.FindModule(LImport.Name);

        if LImportModule < 0 then
          raise ENyxDocumentationSource.Create('A required public imported namespace is absent: ' + LImport.Name);
        Module(LImportModule);
      end;
    finally
      Dec(FNamespaceDepth);
    end;
  finally
    LValue.Free;
  end;
end;

procedure TSourceCapture.NeedDeclaration(const AValue: TNyxPascalSyntaxDeclaration);
begin
  Work;

  if AValue.Anonymous or
    (AValue.Flags * [psdfIncomplete, psdfIncompleteHeader, psdfConditional, psdfUnsupported] <> []) then
    raise ENyxDocumentationSource.Create('A selected declaration is incomplete, anonymous or unsupported: ' + AValue.Name);

  if (AValue.Kind = psdkField) and (AValue.VariantArm >= 0) then
    raise ENyxDocumentationSource.Create('A variant record field needs its actual arm relationship metadata: ' + AValue.Name);
end;

function TSourceCapture.PublicDeclaration(const AValue: TNyxPascalSyntaxDeclaration): Boolean;
begin
  { Unspecified remains unspecified on the captured intent. Keeping those rows
    avoids omitting the ordinary unqualified interface/record member surface;
    no effective compiler visibility is assigned to an unspecified class row. }
  Result := AValue.Visibility in [psavUnspecified, psavInterface,
    psavPublic, psavPublished, psavAutomated];
end;

function TSourceCapture.CompleteType(AModule, ADeclaration: Integer): Integer;
var
  LModule: TSourceModule;
  LValue, LCandidate: TNyxPascalSyntaxDeclaration;
  LAt, LFound: Integer;
begin
  LModule := Module(AModule);
  Work;
  LValue := LModule.Syntax.Declaration(ADeclaration);
  NeedDeclaration(LValue);

  if LValue.Kind <> psdkType then
    raise ENyxDocumentationSource.Create('A source type selection must name an actual type declaration');
  Result := ADeclaration;

  if not (psdfForward in LValue.Flags) then
    Exit;
  LFound := -1;
  LAt := LModule.Syntax.Named(LValue.Scope, LValue.Name);
  while LAt >= 0 do
  begin
    Work;
    LCandidate := LModule.Syntax.Declaration(LAt);

    if (LCandidate.Kind = psdkType) and not (psdfForward in LCandidate.Flags) then
    begin

      if LFound >= 0 then
        raise ENyxDocumentationSource.Create('More than one complete declaration owns a forward type');
      LFound := LAt;
    end;
    LAt := LCandidate.NextNamed;
  end;

  if LFound < 0 then
    raise ENyxDocumentationSource.Create('A forward source type has no complete captured declaration');
  Result := LFound;
end;

function TSourceCapture.TypeReference(AModule, ADeclaration: Integer): TNyxDocumentationRef;
var
  LModule: TSourceModule;
  LValue: TNyxPascalSyntaxDeclaration;
begin
  LModule := Module(AModule);
  Work;
  LValue := LModule.Syntax.Declaration(ADeclaration);
  NeedDeclaration(LValue);

  if (LValue.Kind <> psdkType) or (LValue.Scope <> 0) then
    raise ENyxDocumentationSource.Create('Nested or non-type declarations require explicit supported declaring identity');

  if not PublicDeclaration(LValue) then
    raise ENyxDocumentationSource.Create('A selected type has no public source declaration at its actual owner');

  if LValue.GenericSpan.Length <> 0 then
    raise ENyxDocumentationSource.Create('Complete own generic-formal enumeration is required for: ' + LValue.Name);
  Result := NyxDocumentationType(LModule.UnitID.UnitName, LValue.Name);
end;

function TSourceCapture.ResolutionReference(const AValue: INyxPascalResolution): TNyxDocumentationRef;
var
  LSymbol: INyxPascalBoundSymbol;
begin
  Work;

  if (AValue = nil) or (AValue.World <> FWorld) then
    raise ENyxDocumentationSource.Create('Type binding did not retain the exact source world');

  if AValue.Status <> pbsBound then
    raise ENyxDocumentationSource.Create('A required source type is unresolved: ' + AValue.RelatedName + ' ' + AValue.MessageText);

  if AValue.Count = 1 then
    LSymbol := AValue.SymbolAt(0)
  else if (AValue.Count = 0) and (AValue.BoundType <> nil) then
    LSymbol := AValue.BoundType.Definition
  else
    LSymbol := nil;

  if (LSymbol = nil) or (LSymbol.World <> FWorld) or
    (LSymbol.Declaration.Kind <> psdkType) then
    raise ENyxDocumentationSource.Create('A required type has no genuine captured declaring scope; compiler-root fact closure is unavailable');
  Result := CaptureType(LSymbol.ModuleIndex, LSymbol.DeclarationIndex);
end;

function TSourceCapture.ResolutionRoot(const AValue: INyxPascalResolution): INyxDocumentationCompilerRoot;
var
  LType: INyxPascalSemanticType;
  LIdentity: INyxPascalIntrinsicIdentity;
  LContext, LExpected: INyxPascalCompilerContext;
  LName: TNyxPascalSizeReference;
  LFamily: TNyxDocumentationIntrinsicFamily;
  LSystem, LAt: Integer;
  LCandidate: INyxDocumentationCompilerRoot;
begin
  Result := nil;
  Work;

  if (AValue = nil) or (AValue.World <> FWorld) or (AValue.Status <> pbsBound) or
    (AValue.Count <> 0) then
    Exit;
  LType := AValue.BoundType;

  if (LType = nil) or (LType.World <> FWorld) or (LType.Kind <> pstBuiltin) then
    Exit;

  if not Supports(LType, INyxPascalIntrinsicIdentity, LIdentity) or
    (LIdentity.IntrinsicOrigin <> pioSystemBootstrap) then
    raise ENyxDocumentationSource.Create('A compiler root lacks exact System bootstrap provenance');
  LSystem := FWorld.FindModule('System');

  if LSystem < 0 then
    raise ENyxDocumentationSource.Create('A compiler root cannot replace a missing System owner');
  LContext := LIdentity.IntrinsicContext;
  LExpected := FWorld.ModuleAt(LSystem).Source.InitialContext;

  if NyxPascalCompilerScope(LExpected) <> pcsoSystemBootstrap then
    LExpected := NyxPascalBootstrapContext(LExpected);
  LName := LIdentity.IntrinsicReference;
  Work(Length(LName.Name) + 1);

  if (LContext = nil) or (LContext <> LExpected) or
    (NyxPascalCompilerScope(LContext) <> pcsoSystemBootstrap) or
    (Pos('.', LName.Name) <> 0) then
    raise ENyxDocumentationSource.Create('Compiler-root facts disagree with the exact initial System owner');
  case LType.Builtin of
    pbbBoolean, pbbByteBool, pbbWordBool, pbbLongBool: LFamily := ndifBoolean;
    pbbInteger, pbbByte, pbbWord, pbbSmallInt, pbbShortInt, pbbLongInt,
    pbbLongWord, pbbInt64, pbbQWord, pbbNativeInt, pbbNativeUInt: LFamily := ndifInteger;
    pbbSingle, pbbDouble, pbbExtended, pbbReal, pbbCurrency, pbbComp: LFamily := ndifReal;
    pbbChar, pbbAnsiChar, pbbWideChar: LFamily := ndifCharacter;
    pbbAnsiString, pbbUTF8String, pbbUnicodeString, pbbWideString, pbbShortString: LFamily := ndifString;
    pbbPointer: LFamily := ndifPointer;
  else
    raise ENyxDocumentationSource.Create('This semantic builtin has no declared documentation family');
  end;
  for LAt := 0 to High(FRoots) do
  begin
    Work(Length(LName.Name) + 1);

    if (FRoots[LAt].Name.Name = LName.Name) and (FRoots[LAt].Facts = LContext) then
    begin

      if FRoots[LAt].Family <> LFamily then
        raise ENyxDocumentationSource.Create('One exact root owner supplied contradictory families');
      Exit(FRoots[LAt]);
    end;
  end;
  LCandidate := NewNyxDocumentationCompilerRoot(LName, LFamily, LContext, FConstruction);
  Work(Length(FRoots) + 1);
  SetLength(FRoots, Length(FRoots) + 1);
  FRoots[High(FRoots)] := LCandidate;
  Check;
  Result := LCandidate;
end;

function TSourceCapture.BindUse(AModule, ADeclaration: Integer;
  const AValue: INyxDocumentationTypeUse): INyxDocumentationTypeUse;
var
  LModule: TSourceModule;
  LDeclaration: TNyxPascalSyntaxDeclaration;
  LExpression: INyxDocumentationTypeExpression;
  LSyntax: INyxPascalTypeSyntax;
  LNode, LParent: TNyxPascalTypeNode;
  LResolution: INyxPascalResolution;
  LRef: TNyxDocumentationRef;
  LAt: Integer;
  LName: TNyxText;
  LBound: INyxDocumentationTypeUse;
  LRoot: INyxDocumentationCompilerRoot;
  function Designator(ANode: Integer; ADepth: Integer): TNyxText;
  var
    LPart: TNyxPascalTypeNode;
    LBase, LMember: Integer;
    LLeft, LRight: TNyxText;
  begin
    Work;

    if ADepth > FWorld.Options.MaximumDepth then
      raise ENyxDocumentationSource.Create('A checked named type designator exceeds the existing binding depth');
    LPart := LSyntax.Node(ANode);

    if LPart.Kind in [ptxName, ptxString] then
      Exit(LPart.Name);

    if LPart.Kind <> ptxQualified then
      raise ENyxDocumentationSource.Create('A qualified type requires a supported exact nominal designator');
    LBase := LSyntax.Child(ANode, ptrBase);
    LMember := LSyntax.Child(ANode, ptrMember);

    if (LBase < 0) or (LMember < 0) then
      raise ENyxDocumentationSource.Create('A qualified type lacks one of its checked name components');
    LLeft := Designator(LBase, ADepth + 1);
    LRight := Designator(LMember, ADepth + 1);
    Work(Length(LLeft) + Length(LRight) + 1);

    if (Length(LLeft) > 256) or (Length(LRight) > 255 - Length(LLeft)) then
      raise ENyxDocumentationSource.Create('A source type name exceeds the existing qualified identity allowance');
    Result := LLeft + '.' + LRight;
  end;
begin
  Result := nil;

  if AValue = nil then
    Exit;
  LModule := Module(AModule);
  Work;
  LDeclaration := LModule.Syntax.Declaration(ADeclaration);
  LExpression := AValue.Expression;

  if LExpression = nil then
    raise ENyxDocumentationSource.Create('Source uses require exact checked syntax rather than an invented named reference');
  LSyntax := LExpression.Syntax;
  LBound := AValue;
  for LAt := 0 to LExpression.NamedUseCount - 1 do
  begin
    Work;
    LNode := LSyntax.Node(LExpression.NamedUseNode(LAt));
    LName := Designator(LExpression.NamedUseNode(LAt), 0);
    { A source String(codepage) explicitly requests the ANSI base independently
      of H. The existing binder owns that grammar and missing-root refusal. }

    if (LNode.Kind = ptxString) and (LNode.Parent >= 0) then
    begin
      Work;
      LParent := LSyntax.Node(LNode.Parent);

      if LParent.Kind = ptxEncodedString then
      begin
        Work;

        if LSyntax.Child(LNode.Parent, ptrBase) = LExpression.NamedUseNode(LAt) then
          LName := 'AnsiString';
      end;
    end;
    LResolution := FWorld.Lookup(FWorld.At(AModule, LDeclaration.Scope,
      LDeclaration.NameSpan.Start), NyxPascalBindingName(LName), FExecution);
    Check;
    LRoot := ResolutionRoot(LResolution);

    if LRoot <> nil then
    begin
      LBound := WithNyxDocumentationCompilerRootBinding(LBound, LAt, LRoot, FConstruction);
      Continue;
    end;
    LRef := ResolutionReference(LResolution);

    if LRef.GenericArity <> LExpression.NamedUseArity(LAt) then
      raise ENyxDocumentationSource.Create('A source type application has no matching declared generic arity');
    { WithBinding preserves the immutable expression. Every copied binding
      slot is admitted, including preceding slots copied by this old method. }
    Work(LBound.BindingCount + 1);
    LBound := LBound.WithBinding(LAt, LRef);
  end;
  Check;
  Result := LBound;
end;

function TSourceCapture.TypeUse(AModule, ADeclaration: Integer;
  const ASpan: TNyxPascalSpan;
  ARole: TNyxDocumentationTypeOccurrence): INyxDocumentationTypeUse;
var
  LModule: TSourceModule;
  LExpression: INyxDocumentationTypeExpression;
  LText: TNyxText;
begin
  Result := nil;

  if ASpan.Length = 0 then
    Exit;
  LModule := Module(AModule);
  LText := LModule.Windows.CopyWindow(ASpan);
  LExpression := NewNyxDocumentationTypeExpression(LText, ARole, FConstruction);
  Result := BindUse(AModule, ADeclaration,
    NewNyxDocumentationExpressionTypeUse(LExpression, FConstruction));
end;

function TSourceCapture.Selector(AModule, ADeclaration: Integer): TNyxText;
var
  LModule: TSourceModule;
  LValue, LParameter: TNyxPascalSyntaxDeclaration;
  LAt: Integer;
  LSelector: TNyxText;
  procedure Add(const AText: TNyxText);
  begin
    Work(Length(AText) + 1);

    if Length(AText) > 256 - Length(LSelector) then
      raise ENyxDocumentationSource.Create('Authored overload selector exceeds its existing identity allowance');
    LSelector := LSelector + AText;
  end;
  procedure AddType(const ASpan: TNyxPascalSpan);
  var
    LToken: TNyxPascalToken;
    LTokenAt, LByte: Integer;
    LText: TNyxText;
    LBytes: TNyxBytes;
  begin
    Add('.T');

    if ASpan.Length = 0 then
      Exit;
    Work;
    LTokenAt := LModule.Syntax.Lexical.TokenAt(ASpan.Start);
    while LTokenAt < LModule.Syntax.Lexical.Count do
    begin
      Work;
      LToken := LModule.Syntax.Lexical.Token(LTokenAt);

      if LToken.Span.Start >= ASpan.Finish then
        Break;

      if not (LToken.Kind in [ptkWhitespace, ptkComment, ptkEOF]) then
      begin

        if (LToken.Span.Start < ASpan.Start) or (LToken.Span.Finish > ASpan.Finish) then
          raise ENyxDocumentationSource.Create('An authored selector window cuts a significant type token');
        LText := LModule.Windows.CopyWindow(LToken.Span);

        if LToken.Kind = ptkWord then
        begin
          Work(Length(LText) + 1);
          LText := NyxPascalASCIIKey(LModule.Syntax.Lexical.Identifier(LTokenAt));
        end;
        Work(Length(LText) + 1);
        LBytes := NyxEncodeUTF8(LText);
        Add('.K' + IntToStr(Ord(LToken.Kind)) + 'L' + IntToStr(Length(LBytes)) + '_');
        for LByte := 0 to Length(LBytes) - 1 do
          Add(IntToHex(LBytes[LByte], 2));
      end;
      Inc(LTokenAt);
    end;
  end;
begin
  LModule := Module(AModule);
  Work;
  LValue := LModule.Syntax.Declaration(ADeclaration);

  if LValue.GenericSpan.Length <> 0 then
    raise ENyxDocumentationSource.Create('Routine-own generic formals require their complete checked declaration query');
  LSelector := 'R' + IntToStr(Ord(LValue.RoutineKind));

  if LValue.NestedScope >= 0 then
  begin
    LAt := LModule.Syntax.Scope(LValue.NestedScope).FirstDeclaration;
    while LAt >= 0 do
    begin
      Work;
      LParameter := LModule.Syntax.Declaration(LAt);

      if LParameter.Kind <> psdkParameter then
        Break;
      Add('.P' + IntToStr(Ord(LParameter.ParameterMode)));
      AddType(LParameter.TypeSpan);
      LAt := LParameter.Next;
    end;
  end;
  Add('.Result');
  AddType(LValue.TypeSpan);
  Result := LSelector;
end;

function TSourceCapture.IsCreator(AModule, ADeclaration: Integer): Boolean;
var
  LAt: Integer;
begin
  Result := False;
  for LAt := 0 to Length(FCreators) - 1 do
  begin
    Work;

    if (FCreators[LAt].ModuleIndex = AModule) and
      (FCreators[LAt].DeclarationIndex = ADeclaration) then
      Exit(True);
  end;
end;

function TSourceCapture.Member(AModule, ADeclaration: Integer;
  const AOwner: TNyxDocumentationRef): INyxDocumentationMember;
var
  LModule: TSourceModule;
  LValue, LParameter: TNyxPascalSyntaxDeclaration;
  LID: TNyxDocumentationRef;
  LKind: TNyxDocumentationMemberKind;
  LSignature: INyxDocumentationRichSignature;
  LParameters: TNyxDocumentationParameterDeclarations;
  LType, LResult: INyxDocumentationTypeUse;
  LMode: TNyxDocumentationParameterMode;
  LAccess: TNyxDocumentationPropertyAccesses;
  LAt, LCount: Integer;
  LInitializer: TNyxText;
  LParameterOwner: TSourceParameter;
begin
  Result := nil;
  LModule := Module(AModule);
  Work;
  LValue := LModule.Syntax.Declaration(ADeclaration);
  NeedDeclaration(LValue);
  LID := Default(TNyxDocumentationRef);

  if (LValue.Kind <> psdkRoutine) or (LValue.RoutineKind <> prkOperator) then
    LID := NyxDocumentationMember(AOwner, LValue.Name);
  case LValue.Kind of
    psdkRoutine:
      begin

        if LValue.RoutineKind = prkOperator then
          LID := NyxDocumentationOperator(AOwner, LValue.OperatorKind,
            LValue.Name, Selector(AModule, ADeclaration))
        else
          LID := NyxDocumentationMember(AOwner, LValue.Name, Selector(AModule, ADeclaration));
        case LValue.RoutineKind of
          prkConstructor: LKind := ndmConstructor;
          prkDestructor: LKind := ndmDestructor;
          prkOperator: LKind := ndmOperator;
          prkProcedure, prkFunction:
            begin

              if AOwner.IsUnit then
                LKind := ndmRoutine
              else
                LKind := ndmMethod;
            end;
          else raise ENyxDocumentationSource.Create('A source routine has no supported authored kind');
        end;

        if IsCreator(AModule, ADeclaration) then
        begin

          if not AOwner.IsUnit or not (LValue.RoutineKind in [prkFunction, prkProcedure]) then
            raise ENyxDocumentationSource.Create('An explicit creator must be an actual selected public global routine');
          LKind := ndmFactory;
        end;
        LSignature := Signature(AModule, ADeclaration);
        Work(LSignature.ParameterCount + 1);
        SetLength(LParameters, LSignature.ParameterCount);
        for LAt := 0 to LSignature.ParameterCount - 1 do
        begin
          Work;
          LParameters[LAt] := LSignature.ParameterAt(LAt);
          LType := BindUse(AModule, ADeclaration, LParameters[LAt].TypeUse);

          if LType <> nil then
          begin
            Work;
            LParameterOwner := TSourceParameter.Create;
            LParameterOwner.ParameterName := LParameters[LAt].Name;
            LParameterOwner.ParameterType := LType;
            LParameterOwner.Mode := LParameters[LAt].Passing;
            LParameterOwner.DefaultPresent := LParameters[LAt].HasDefault;
            LParameterOwner.DefaultText := LParameters[LAt].DefaultPascal;
            LParameters[LAt] := LParameterOwner;
          end;
        end;
        LResult := BindUse(AModule, ADeclaration, LSignature.ResultUse);
        LSignature := NewNyxDocumentationRichSignature(LParameters, LResult, FConstruction);
        Result := NewNyxDocumentationRichMember(LID, LKind, '', LSignature, FConstruction);
      end;
    psdkProperty:
      begin
        LParameters := nil;

        if LValue.NestedScope >= 0 then
        begin
          LAt := LModule.Syntax.Scope(LValue.NestedScope).FirstDeclaration;
          while LAt >= 0 do
          begin
            Work;
            LParameter := LModule.Syntax.Declaration(LAt);
            NeedDeclaration(LParameter);

            if (LParameter.Kind <> psdkParameter) or
              (LParameter.ExpressionSpan.Length <> 0) then
              raise ENyxDocumentationSource.Create('A property index has unsupported declaration payload');
            LCount := Length(LParameters);

            if LCount >= NyxDocumentationMaximumParameters then
              raise ENyxDocumentationSource.Create('Source property indices exceed their existing allowance');
            LType := TypeUse(AModule, LAt, LParameter.TypeSpan, ndtoPropertyIndex);
            LMode := TNyxDocumentationParameterMode(Ord(LParameter.ParameterMode));
            Work(LCount + 1);
            SetLength(LParameters, LCount + 1);
            LParameterOwner := TSourceParameter.Create;
            LParameterOwner.ParameterName := LParameter.Name;
            LParameterOwner.ParameterType := LType;
            LParameterOwner.Mode := LMode;
            LParameters[LCount] := LParameterOwner;
            LAt := LParameter.Next;
          end;
        end;
        LAccess := [];

        if pspaRead in LValue.PropertyAccess then
          Include(LAccess, ndpaRead);

        if pspaWrite in LValue.PropertyAccess then
          Include(LAccess, ndpaWrite);
        LType := TypeUse(AModule, ADeclaration, LValue.TypeSpan, ndtoPropertyValue);
        Result := NewNyxDocumentationRichProperty(LID, '', LParameters,
          LType, LAccess, LValue.DefaultProperty, FConstruction);
      end;
    psdkField:
      Result := NewNyxDocumentationValueMember(LID, '',
        TypeUse(AModule, ADeclaration, LValue.TypeSpan, ndtoField), FConstruction);
    psdkConstant, psdkVariable, psdkResourceString:
      begin
        LType := nil;
        case LValue.Kind of
          psdkConstant:
            begin
              LKind := ndmConstant;
              LType := TypeUse(AModule, ADeclaration, LValue.TypeSpan, ndtoConstant);
            end;
          psdkVariable:
            begin
              LKind := ndmVariable;
              LType := TypeUse(AModule, ADeclaration, LValue.TypeSpan, ndtoVariable);
            end;
          else LKind := ndmResourceString;
        end;
        LInitializer := '';

        if LValue.ExpressionSpan.Length > 0 then
          LInitializer := LModule.Windows.CopyWindow(LValue.ExpressionSpan);
        Result := NewNyxDocumentationStorageMember(LID, LKind, '', LType,
          LValue.ExpressionSpan.Length > 0, LInitializer, FConstruction);
      end;
    psdkEnumValue:
      begin

        if LValue.ExpressionSpan.Length <> 0 then
          raise ENyxDocumentationSource.Create('An authored enum initializer requires explicit declaration metadata');
        Result := NewNyxDocumentationMember(LID, ndmEnumValue, '').OfType(AOwner);
      end;
    else raise ENyxDocumentationSource.Create('A public source member has no faithful documentation representation: ' + LValue.Name);
  end;
  Result := WithNyxDocumentationMemberIntent(Result,
    NewNyxDocumentationMemberIntent(psdfClassMember in LValue.Flags,
      psdfStatic in LValue.Flags, LValue.Visibility, LValue.CallingConvention,
      FConstruction), FConstruction);
end;

procedure TSourceCapture.PrepareSignatures(AModule, AScope: Integer);
var
  LModule: TSourceModule;
  LOrdinals: TNyxDocumentationRoutineOrdinals;
  LSignatures: TNyxDocumentationRoutineSignatures;
  LScope: TNyxPascalSyntaxScope;
  LValue: TNyxPascalSyntaxDeclaration;
  LAt, LCount, LExisting, LRead, LPrevious: Integer;
begin
  LOrdinals := nil;
  LModule := Module(AModule);
  Work;
  LScope := LModule.Syntax.Scope(AScope);
  LAt := LScope.FirstDeclaration;
  LRead := 0;
  while LAt >= 0 do
  begin
    Work;
    Inc(LRead);

    if LRead > LScope.DeclarationCount then
      raise ENyxDocumentationSource.Create('Source signature links exceed their complete scope');
    LValue := LModule.Syntax.Declaration(LAt);

    if PublicDeclaration(LValue) and (LValue.Kind = psdkRoutine) then
    begin
      NeedDeclaration(LValue);
      LExisting := -1;
      for LPrevious := 0 to Length(LModule.RoutineOrdinals) - 1 do
      begin
        Work;

        if LModule.RoutineOrdinals[LPrevious] = LAt then
        begin
          LExisting := LPrevious;
          Break;
        end;
      end;

      if LExisting < 0 then
      begin
        LCount := Length(LOrdinals);

        if LCount >= NyxDocumentationMaximumMembers then
          raise ENyxDocumentationSource.Create('Complete source callable scope exceeds its existing member allowance');
        Work(LCount + 1);
        SetLength(LOrdinals, LCount + 1);
        LOrdinals[LCount] := LAt;
      end;
    end;
    LAt := LValue.Next;
  end;

  if Length(LOrdinals) = 0 then
    Exit;
  { One session per exact module survives all selected class/interface/global
    groups. Its checked immutable rows need no independent header reparse or
    repeated complete-unit comparison; copied type/default windows still pay. }

  if LModule.SignatureSource = nil then
    LModule.SignatureSource := NewNyxDocumentationSignatureSource(LModule.View, FConstruction);
  LSignatures := LModule.SignatureSource.CaptureDeclared(LOrdinals);
  LExisting := Length(LModule.RoutineOrdinals);
  Work(LExisting * 2 + Length(LOrdinals) * 2 + 1);
  SetLength(LModule.RoutineOrdinals, LExisting + Length(LOrdinals));
  SetLength(LModule.RoutineSignatures, LExisting + Length(LOrdinals));
  for LAt := 0 to Length(LOrdinals) - 1 do
  begin
    Work;
    LModule.RoutineOrdinals[LExisting + LAt] := LOrdinals[LAt];
    LModule.RoutineSignatures[LExisting + LAt] := LSignatures[LAt];
  end;
end;

function TSourceCapture.Signature(AModule, ADeclaration: Integer): INyxDocumentationRichSignature;
var
  LModule: TSourceModule;
  LAt: Integer;
begin
  LModule := Module(AModule);
  for LAt := 0 to Length(LModule.RoutineOrdinals) - 1 do
  begin
    Work;

    if LModule.RoutineOrdinals[LAt] = ADeclaration then
      Exit(LModule.RoutineSignatures[LAt]);
  end;
  Result := nil;
  raise ENyxDocumentationSource.Create('A source routine was not admitted with its complete sibling signature batch');
end;

procedure TSourceCapture.Members(AOwner: TSourceType; AModule, AScope: Integer;
  AGlobals: Boolean);
var
  LModule: TSourceModule;
  LScope: TNyxPascalSyntaxScope;
  LValue: TNyxPascalSyntaxDeclaration;
  LAt, LCount, LRead: Integer;
begin
  LModule := Module(AModule);
  Work;
  LScope := LModule.Syntax.Scope(AScope);

  if not LScope.Complete or LScope.UnsupportedSyntax then
    raise ENyxDocumentationSource.Create('A selected public declaration scope is incomplete or unsupported');
  PrepareSignatures(AModule, AScope);
  LAt := LScope.FirstDeclaration;
  LRead := 0;
  while LAt >= 0 do
  begin
    Work;
    Inc(LRead);

    if LRead > LScope.DeclarationCount then
      raise ENyxDocumentationSource.Create('Source declaration links exceed their complete scope');
    LValue := LModule.Syntax.Declaration(LAt);

    if (LValue.Scope <> AScope) then
      raise ENyxDocumentationSource.Create('Source declaration link changed scope');

    if PublicDeclaration(LValue) then
    begin
      NeedDeclaration(LValue);

      if LValue.Kind in [psdkType, psdkEnumValue] then
      begin

        if not AGlobals and (LValue.Kind = psdkType) then
          raise ENyxDocumentationSource.Create('A selected public nested type requires supported nested declaring identity');
        { A unit root groups its globals; type declarations retain independent
          identities and enter only through selection or genuine type closure. }
      end
      else
      begin
        LCount := Length(AOwner.Members);

        if LCount >= NyxDocumentationMaximumMembers then
          raise ENyxDocumentationSource.Create('Complete source member scope exceeds its existing 128-member allowance');
        Work(LCount + 1);
        SetLength(AOwner.Members, LCount + 1);
        AOwner.Members[LCount] := Member(AModule, LAt, AOwner.ID);
      end;
    end;
    LAt := LValue.Next;
  end;
end;

procedure TSourceCapture.Ancestry(AOwner: TSourceType; AModule, ADeclaration: Integer;
  out AInterfaces: INyxDocumentationInterfaceImplementations);
var
  LModule: TSourceModule;
  LValue: TNyxPascalSyntaxDeclaration;
  LToken: TNyxPascalToken;
  LUses: array of INyxDocumentationTypeUse;
  LUse: INyxDocumentationTypeUse;
  LSyntax: INyxPascalTypeSyntax;
  LAt, LFirst, LLast, LAngle, LParen, LBracket, LCount: Integer;
  LTarget: TNyxDocumentationRef;
  LKind: TNyxDocumentationTypeKind;
  function TargetKind(const AReference: TNyxDocumentationRef): TNyxDocumentationTypeKind;
  var
    LRef: TNyxDocumentationRef;
    LEntryAt, LDepth: Integer;
    LType: INyxDocumentationType;
  begin
    LRef := AReference;
    LDepth := 0;
    repeat
      Work;
      Inc(LDepth);

      if LDepth > FWorld.Options.MaximumDepth then
        raise ENyxDocumentationSource.Create('Source ancestor alias closure exceeds its existing binding depth');
      LType := nil;
      for LEntryAt := 0 to Length(FEntries) - 1 do
      begin
        Work;

        if FEntries[LEntryAt].ID.Same(LRef) then
        begin
          LType := FEntries[LEntryAt].Value;
          Break;
        end;
      end;

      if LType = nil then
        raise ENyxDocumentationSource.Create('A source ancestor is incomplete or cyclic');
      Result := LType.Kind;

      if Result <> ndtAlias then
        Exit;
      LRef := LType.AliasTarget;

      if not LRef.Defined then
        raise ENyxDocumentationSource.Create('An ancestor alias has no exact target in this projection');
    until False;
  end;
  procedure AddAncestor(AStart, AFinish: Integer);
  var
    LBaseCount: Integer;
  begin

    if AFinish <= AStart then
      raise ENyxDocumentationSource.Create('An authored ancestor list contains an empty application');
    LUse := TypeUse(AModule, ADeclaration,
      NyxPascalSpan(AStart, AFinish - AStart), ndtoImplementedInterface);
    LSyntax := LUse.Expression.Syntax;
    Work;

    if not (LSyntax.Node(LSyntax.Root).Kind in [ptxName, ptxQualified, ptxGeneric]) or
      (LUse.BindingCount < 1) then
      raise ENyxDocumentationSource.Create('An ancestor must retain a complete nominal application');
    LTarget := LUse.BindingAt(0);
    LKind := TargetKind(LTarget);

    if (AOwner.TypeKind = ndtClass) and (LKind = ndtClass) then
    begin

      if (Length(AOwner.Bases) <> 0) or (Length(LUses) <> 0) or
        (LSyntax.Node(LSyntax.Root).Kind = ptxGeneric) then
        raise ENyxDocumentationSource.Create('A class base must be the one exact representable authored first ancestor');
      Work;
      SetLength(AOwner.Bases, 1);
      AOwner.Bases[0] := LTarget;
    end
    else if (AOwner.TypeKind = ndtInterface) and (LKind = ndtInterface) then
    begin

      if LSyntax.Node(LSyntax.Root).Kind = ptxGeneric then
        raise ENyxDocumentationSource.Create('A generic interface base requires an application-preserving base capability');
      LBaseCount := Length(AOwner.Bases);

      if LBaseCount >= NyxDocumentationMaximumLinks then
        raise ENyxDocumentationSource.Create('Source interface bases exceed their existing allowance');
      Work(LBaseCount + 1);
      SetLength(AOwner.Bases, LBaseCount + 1);
      AOwner.Bases[LBaseCount] := LTarget;
    end
    else if (AOwner.TypeKind = ndtClass) and (LKind = ndtInterface) then
    begin
      LCount := Length(LUses);

      if LCount >= NyxDocumentationMaximumLinks then
        raise ENyxDocumentationSource.Create('Source implemented interfaces exceed their existing allowance');
      Work(LCount + 1);
      SetLength(LUses, LCount + 1);
      LUses[LCount] := LUse;
    end
    else
      raise ENyxDocumentationSource.Create('An authored ancestor has an incompatible actual class/interface declaration');
  end;
begin
  AInterfaces := nil;
  LModule := Module(AModule);
  Work;
  LValue := LModule.Syntax.Declaration(ADeclaration);

  if LValue.AncestorSpan.Length > 0 then
  begin
    Work;
    LAt := LModule.Syntax.Lexical.TokenAt(LValue.AncestorSpan.Start);
    LToken := LModule.Syntax.Lexical.Token(LAt);

    if LToken.Symbol <> psoOpenParen then
      raise ENyxDocumentationSource.Create('A checked ancestor clause must begin with its authored delimiter');
    LFirst := LToken.Span.Finish;
    LLast := LFirst;
    LAngle := 0;
    LParen := 0;
    LBracket := 0;
    Inc(LAt);
    while LAt < LModule.Syntax.Lexical.Count do
    begin
      Work;
      LToken := LModule.Syntax.Lexical.Token(LAt);

      if LToken.Span.Start >= LValue.AncestorSpan.Finish then
        raise ENyxDocumentationSource.Create('A checked ancestor clause lost its closing delimiter');

      if LToken.Kind in [ptkWhitespace, ptkComment] then
      begin
        Inc(LAt);
        Continue;
      end;

      if (LToken.Symbol = psoCloseParen) and (LAngle = 0) and
        (LParen = 0) and (LBracket = 0) then
      begin
        AddAncestor(LFirst, LLast);
        Break;
      end;

      if (LToken.Symbol = psoComma) and (LAngle = 0) and
        (LParen = 0) and (LBracket = 0) then
      begin
        AddAncestor(LFirst, LLast);
        LFirst := LToken.Span.Finish;
      end
      else
      begin
        { This only finds the existing checked clause's comma windows. Every
          application is admitted by the existing standalone type parser above;
          no declaration or generic argument grammar is duplicated here. }
        case LToken.Symbol of
          psoLess: Inc(LAngle);
          psoGreater: Dec(LAngle);
          psoOpenParen: Inc(LParen);
          psoCloseParen: Dec(LParen);
          psoOpenBracket: Inc(LBracket);
          psoCloseBracket: Dec(LBracket);
          else begin end;
        end;

        if (LAngle < 0) or (LParen < 0) or (LBracket < 0) then
          raise ENyxDocumentationSource.Create('An ancestor application has inconsistent checked delimiters');
        LLast := LToken.Span.Finish;
      end;
      Inc(LAt);
    end;
  end;
  { No clause is an explicit empty implemented-interface vector for a class;
    no implicit compiler TObject/IInterface base is invented. }

  if AOwner.TypeKind = ndtClass then
    AInterfaces := NewNyxDocumentationInterfaceImplementations(LUses, FConstruction);
end;

function TSourceCapture.CaptureType(AModule, ADeclaration: Integer): TNyxDocumentationRef;
var
  LModule: TSourceModule;
  LEntry: TSourceEntry;
  LValue: TNyxPascalSyntaxDeclaration;
  LSupplier: TSourceType;
  LLease: INyxDocumentationType;
  LInterfaces: INyxDocumentationInterfaceImplementations;
  LDefinition: INyxDocumentationTypeUse;
  LSyntax: INyxPascalTypeSyntax;
  LAt, LCount, LEnum: Integer;
  LEnumValue: TNyxPascalSyntaxDeclaration;
  LKind: TNyxPascalTypeKind;
begin
  ADeclaration := CompleteType(AModule, ADeclaration);
  LModule := Module(AModule);
  Result := TypeReference(AModule, ADeclaration);
  for LAt := 0 to Length(FEntries) - 1 do
  begin
    Work;

    if (FEntries[LAt].ModuleIndex = AModule) and
      (FEntries[LAt].DeclarationIndex = ADeclaration) then
      Exit;
  end;
  LCount := Length(FEntries);

  if LCount >= TNyxDocumentationContributionLimits.Defaults.DeclarationLimit then
    raise ENyxDocumentationSource.Create('Genuine source type closure exceeds its existing aggregate allowance');
  Work(LCount + 1);
  LEntry := TSourceEntry.Create;
  LEntry.ModuleIndex := AModule;
  LEntry.DeclarationIndex := ADeclaration;
  LEntry.ID := Result;
  LEntry.Building := True;
  SetLength(FEntries, LCount + 1);
  FEntries[LCount] := LEntry;
  Work;
  LValue := LModule.Syntax.Declaration(ADeclaration);
  NeedDeclaration(LValue);
  LSupplier := TSourceType.Create;
  LLease := LSupplier;
  LSupplier.ID := Result;
  case LValue.TypeForm of
    pstfClass: LSupplier.TypeKind := ndtClass;
    pstfInterface: LSupplier.TypeKind := ndtInterface;
    pstfRecord: LSupplier.TypeKind := ndtRecord;
    pstfEnumeration: LSupplier.TypeKind := ndtEnum;
    else LSupplier.TypeKind := ndtAlias;
  end;

  if LValue.TypeForm = pstfEnumeration then
  begin
    LEnum := LModule.Syntax.Scope(LValue.Scope).FirstDeclaration;
    while LEnum >= 0 do
    begin
      Work;
      LEnumValue := LModule.Syntax.Declaration(LEnum);

      if (LEnumValue.Kind = psdkEnumValue) and
        (LEnumValue.TypeSpan.Start = LValue.NameSpan.Start) and
        (LEnumValue.TypeSpan.Length = LValue.NameSpan.Length) then
      begin
        LCount := Length(LSupplier.Members);

        if LCount >= NyxDocumentationMaximumMembers then
          raise ENyxDocumentationSource.Create('Complete source enumeration exceeds its existing member allowance');
        Work(LCount + 1);
        SetLength(LSupplier.Members, LCount + 1);
        LSupplier.Members[LCount] := Member(AModule, LEnum, LSupplier.ID);
      end;
      LEnum := LEnumValue.Next;
    end;
    LEntry.Value := CaptureNyxDocumentationType(LLease, FConstruction);
  end
  else if LValue.TypeForm in [pstfClass, pstfInterface, pstfRecord] then
  begin

    if LValue.NestedScope < 0 then
      raise ENyxDocumentationSource.Create('A selected structured source type has no complete member scope');
    Ancestry(LSupplier, AModule, ADeclaration, LInterfaces);
    Members(LSupplier, AModule, LValue.NestedScope, False);
    LEntry.Value := CaptureNyxDocumentationType(LLease, FConstruction);

    if LInterfaces <> nil then
      LEntry.Value := WithNyxDocumentationImplementedInterfaces(LEntry.Value,
        LInterfaces, FConstruction);
  end
  else
  begin
    LDefinition := TypeUse(AModule, ADeclaration, LValue.TypeSpan, ndtoDeclaration);

    if LDefinition = nil then
      raise ENyxDocumentationSource.Create('A source type has no authored definition');
    LSyntax := LDefinition.Expression.Syntax;
    Work;
    LKind := LSyntax.Node(LSyntax.Root).Kind;

    if LKind in [ptxName, ptxQualified] then
    begin

      if LDefinition.BindingCount <> 1 then
        raise ENyxDocumentationSource.Create('A named alias needs its one exact nominal type binding');

      if LDefinition.BindingAt(0).Defined then
      begin
        LSupplier.AliasID := LDefinition.BindingAt(0);
        LEntry.Value := CaptureNyxDocumentationType(LLease, FConstruction);
      end
      else
        LEntry.Value := NewNyxDocumentationAliasDefinition(LEntry.ID, '', LDefinition, FConstruction);
    end
    else
      LEntry.Value := NewNyxDocumentationDeclaredType(LEntry.ID, '', LDefinition, FConstruction);
  end;
  LEntry.Value := WithNyxDocumentationGenericParameters(LEntry.Value,
    NewNyxDocumentationGenericParameters(LEntry.ID, nil, FConstruction), FConstruction);
  LEntry.Building := False;
  Check;
end;

procedure TSourceCapture.CaptureGlobals(AModule: Integer);
var
  LModule: TSourceModule;
  LEntry: TSourceEntry;
  LSupplier: TSourceType;
  LLease: INyxDocumentationType;
  LAt, LCount: Integer;
begin
  LModule := Module(AModule);
  for LAt := 0 to Length(FEntries) - 1 do
  begin
    Work;

    if (FEntries[LAt].ModuleIndex = AModule) and
      (FEntries[LAt].DeclarationIndex = -1) then
      raise ENyxDocumentationSource.Create('A complete public unit scope was selected more than once');
  end;
  LCount := Length(FEntries);

  if LCount >= TNyxDocumentationContributionLimits.Defaults.DeclarationLimit then
    raise ENyxDocumentationSource.Create('Complete selected source units exceed their existing declaration allowance');
  Work(LCount + 1);
  LEntry := TSourceEntry.Create;
  LEntry.ModuleIndex := AModule;
  LEntry.DeclarationIndex := -1;
  LEntry.ID := LModule.UnitID;
  LEntry.Building := True;
  SetLength(FEntries, LCount + 1);
  FEntries[LCount] := LEntry;
  LSupplier := TSourceType.Create;
  LLease := LSupplier;
  LSupplier.ID := LModule.UnitID;
  LSupplier.TypeKind := ndtUnit;
  Members(LSupplier, AModule, 0, True);
  LEntry.Value := CaptureNyxDocumentationType(LLease, FConstruction);
  LEntry.Building := False;
end;

function TSourceCapture.PackageValue: INyxDocumentationPackage;
var
  LChunks: TNyxDocumentationDeclarationChunks;
  LDeclarations: TNyxDocumentationDeclarations;
  LModule, LAt, LStart, LCount, LChunk: Integer;
  LOrigin: TNyxDocumentationRef;
  LAdmission: INyxDocumentationAdmission;
begin
  Result := nil;
  LAdmission := Self;
  LChunks := nil;
  for LModule := 0 to Length(FModules) - 1 do
  begin
    LDeclarations := nil;
    for LAt := 0 to Length(FEntries) - 1 do
    begin
      Work;

      if FEntries[LAt].ModuleIndex <> FModules[LModule].ModuleIndex then
        Continue;

      if FEntries[LAt].Building or (FEntries[LAt].Value = nil) then
        raise ENyxDocumentationSource.Create('A referenced source declaration never completed');
      LCount := Length(LDeclarations);
      Work(LCount + 1);
      SetLength(LDeclarations, LCount + 1);
      LDeclarations[LCount] := FEntries[LAt].Value;
    end;
    LStart := 0;
    LOrigin := FModules[LModule].UnitID;
    while LStart < Length(LDeclarations) do
    begin
      LCount := Length(LDeclarations) - LStart;

      if LCount > NyxDocumentationRegistrationMaximumDeclarations then
        LCount := NyxDocumentationRegistrationMaximumDeclarations;
      Work(LCount + 1);
      LChunk := Length(LChunks);
      SetLength(LChunks, LChunk + 1);
      LChunks[LChunk] := NewNyxDocumentationDeclarationChunk(LOrigin,
        Copy(LDeclarations, LStart, LCount), FPlatforms, FConstruction, LAdmission);
      Inc(LStart, LCount);
    end;
  end;
  Check;
  Result := NewNyxDocumentationPackage(LChunks,
    TNyxDocumentationContributionLimits.Defaults, FConstruction, LAdmission);
end;

function TSourceCapture.Capture(const AScopes: TNyxDocumentationSourceScopes;
  const ACreators: TNyxDocumentationSourceCreators): INyxDocumentationPackage;
var
  LScopes: TNyxDocumentationSourceScopes;
  LCreators: TNyxDocumentationSourceCreators;
  LCandidate: INyxDocumentationPackage;
  LAt, LOther: Integer;
  LFound: Boolean;
  LModule: TSourceModule;
  LDeclaration: TNyxPascalSyntaxDeclaration;
begin
  Result := nil;
  LScopes := AScopes;
  LCreators := ACreators;
  Work;

  if (Length(LScopes) < 1) or
    (Length(LScopes) > TNyxDocumentationContributionLimits.Defaults.DeclarationLimit) or
    (Length(LCreators) > TNyxDocumentationContributionLimits.Defaults.DeclarationLimit) then
    raise ENyxDocumentationSource.Create('Use bounded nonempty source scopes and bounded creator annotations');
  Work(Length(LScopes) + Length(LCreators));
  FScopes := Copy(LScopes);
  FCreators := Copy(LCreators);
  for LAt := 0 to Length(FCreators) - 1 do
  begin
    Work;
    LFound := False;
    for LOther := 0 to Length(FScopes) - 1 do
    begin
      Work;

      if (FScopes[LOther].ModuleIndex = FCreators[LAt].ModuleIndex) and
        (FScopes[LOther].DeclarationIndex = -1) then
        LFound := True;
    end;

    if not LFound then
      raise ENyxDocumentationSource.Create('Every explicit creator must belong to a selected complete public unit scope');
    LModule := Module(FCreators[LAt].ModuleIndex);
    Work;

    if (FCreators[LAt].DeclarationIndex < 0) or
      (FCreators[LAt].DeclarationIndex >= LModule.Syntax.DeclarationCount) then
      raise ENyxDocumentationSource.Create('Creator annotation declaration ordinal is outside its exact source');
    LDeclaration := LModule.Syntax.Declaration(FCreators[LAt].DeclarationIndex);
    NeedDeclaration(LDeclaration);

    if (LDeclaration.Scope <> 0) or not PublicDeclaration(LDeclaration) or
      (LDeclaration.Kind <> psdkRoutine) then
      raise ENyxDocumentationSource.Create('Creator annotation does not name an actual public global routine');
    for LOther := 0 to LAt - 1 do
    begin
      Work;

      if (FCreators[LOther].ModuleIndex = FCreators[LAt].ModuleIndex) and
        (FCreators[LOther].DeclarationIndex = FCreators[LAt].DeclarationIndex) then
        raise ENyxDocumentationSource.Create('Duplicate exact source creator annotation');
    end;
  end;
  { This interface is held by the public entry's independent local lease.
    Retire breaks this temporary self-reference before that entry publishes. }
  FExecution := Self;
  for LAt := 0 to Length(FScopes) - 1 do
  begin
    Work;

    if FScopes[LAt].DeclarationIndex < -1 then
      raise ENyxDocumentationSource.Create('A source selection requires a declaration or the explicit whole-unit sentinel');

    if FScopes[LAt].DeclarationIndex = -1 then
      CaptureGlobals(FScopes[LAt].ModuleIndex)
    else
      CaptureType(FScopes[LAt].ModuleIndex, FScopes[LAt].DeclarationIndex);
  end;
  LCandidate := PackageValue;
  Work(Length(FScopes) + Length(FCreators));
  Check;
  Result := LCandidate;
end;

procedure TSourceCapture.CheckInputs(const AScopes: TNyxDocumentationSourceScopes;
  const ACreators: TNyxDocumentationSourceCreators);
var
  LAt: Integer;
begin
  { The caller reserved this bounded comparison before its final outward
    cancellation check. No callback is invoked after these observations. }

  if (Length(AScopes) <> Length(FScopes)) or
    (Length(ACreators) <> Length(FCreators)) then
    raise ENyxDocumentationSource.Create('Caller input vector changed before source publication');
  for LAt := 0 to Length(FScopes) - 1 do

    if (AScopes[LAt].ModuleIndex <> FScopes[LAt].ModuleIndex) or
      (AScopes[LAt].DeclarationIndex <> FScopes[LAt].DeclarationIndex) then
      raise ENyxDocumentationSource.Create('Caller source selection changed before publication');
  for LAt := 0 to Length(FCreators) - 1 do

    if (ACreators[LAt].ModuleIndex <> FCreators[LAt].ModuleIndex) or
      (ACreators[LAt].DeclarationIndex <> FCreators[LAt].DeclarationIndex) then
      raise ENyxDocumentationSource.Create('Caller creator annotation changed before publication');
end;

function CaptureNyxDocumentationSourcePackage(
  const AWorld: INyxPascalSemanticWorld;
  const AScopes: TNyxDocumentationSourceScopes;
  const ACreators: TNyxDocumentationSourceCreators;
  APlatforms: TNyxDocumentationPlatforms;
  const AConstruction: INyxDocumentationTypeConstruction): INyxDocumentationPackage;
var
  LWorld: INyxPascalSemanticWorld;
  LScope: INyxDocumentationTypeConstruction;
  LScopes: TNyxDocumentationSourceScopes;
  LCreators: TNyxDocumentationSourceCreators;
  LOwner: TSourceCapture;
  LLease: INyxCancellationScope;
  LCandidate: INyxDocumentationPackage;
begin
  Result := nil;
  LWorld := AWorld;
  LScope := AConstruction;
  LScopes := AScopes;
  LCreators := ACreators;
  LOwner := TSourceCapture.Create(LWorld, LScope, APlatforms);
  LLease := LOwner;
  try
    LCandidate := LOwner.Capture(LScopes, LCreators);
  finally
    LOwner.Retire;
  end;
  CheckNyxDocumentationTypeConstruction(LScope);
  LOwner.CheckInputs(LScopes, LCreators);
  Result := LCandidate;
end;

end.
