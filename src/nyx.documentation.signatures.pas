{ Nyx source-owned documentation signatures. MIT license, see LICENSE. }
unit nyx.documentation.signatures;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.text, nyx.pascal.syntax, nyx.pascal.syntax.translated, nyx.documentation,
  nyx.documentation.typeexpressions;

type
  { A selected declaration cannot be captured faithfully. This is a metadata
    refusal, not a compiler diagnostic or permission to execute a source unit. }
  ENyxDocumentationSignature = class(ENyxDocumentation);
  { An ordered, atomic batch of detached signatures. Entries correspond to the
    caller's exact snapshot-local ordinals, never stable member identities. }
  TNyxDocumentationRoutineSignatures = array of INyxDocumentationRichSignature;
  { A managed selection keeps its backing storage alive across outward admission.
    Ordinals belong only to the session's exact immutable translated snapshot. }
  TNyxDocumentationRoutineOrdinals = array of Integer;
  { A construction-only lease over one complete canonical translated owner.
    Repeated selections share one scalar cursor rather than rechecking complete
    unit text. Each selected window, type and detached field remains admitted.
    Retire releases all source/construction leases; returned signatures do not
    retain this session. It establishes no disk, compiler or nominal authority. }
  INyxDocumentationSignatureSource = interface(IInterface)
    ['{E81F93CB-59BD-4B38-9F5D-26C348D92921}']
    function CaptureDeclared(const ARoutines: TNyxDocumentationRoutineOrdinals): TNyxDocumentationRoutineSignatures;
    procedure Retire;
  end;

{ Only the exact built-in immutable translated owner is accepted, with complete
  resolved translation and diagnostic-free complete module syntax. Alternate
  snapshots keep using the independent checked APIs below. The caller owns the
  shared construction and must retire this session before publishing a package. }
function NewNyxDocumentationSignatureSource(
  const ASource: INyxPascalTranslatedSyntax;
  const AConstruction: INyxDocumentationTypeConstruction): INyxDocumentationSignatureSource;

{ Capture only a procedure/function's declared signature. The ordinal belongs
  to this exact snapshot, not a stable overload identity. A bounded header copy
  is reparsed by Nyx's existing declaration parser under the supplied shared
  construction; an alternate snapshot never supplies checked syntax authority.
  Parameter names are logical identifiers (without an escape ampersand). Type
  and default windows preserve the supplied snapshot's text, including comments.
  Named uses stay unbound. No full-member, physical-origin, factory, compiler,
  visibility, calling-convention or implementation claim is returned.
  The operation does not retire caller-owned construction. Published metadata
  retains neither the supplier, header parser nor construction/cancellation. }
function CaptureNyxDocumentationRoutineSignature(
  const ASource: INyxPascalSyntaxSnapshot; ARoutine: Integer;
  const AConstruction: INyxDocumentationTypeConstruction): INyxDocumentationRichSignature;
{ Capture up to the existing per-type member ceiling from one stable source.
  Shared text windows reuse proven prefix scans; each header is still reparsed
  and every copied byte/parameter/type consumes the same construction. Final
  agreement covers all selected declaration/parameter receipts and the complete
  source value. Duplicates, stale input or any refused member publish no batch.
  This does not discover complete units, bind names or assign member identities. }
function CaptureNyxDocumentationRoutineSignatures(
  const ASource: INyxPascalSyntaxSnapshot; const ARoutines: array of Integer;
  const AConstruction: INyxDocumentationTypeConstruction): TNyxDocumentationRoutineSignatures;
{ The source-adoption path also preserves constructors, destructors, operators
  and routine-owned generic headers. It returns only their declared parameter
  and result shape: generic owners, operator identity and member intent remain
  the source adapter's separate checked facts. The older entry points above
  retain their strict non-generic procedure/function contract. }
function CaptureNyxDocumentationDeclaredRoutineSignature(
  const ASource: INyxPascalSyntaxSnapshot; ARoutine: Integer;
  const AConstruction: INyxDocumentationTypeConstruction): INyxDocumentationRichSignature;
{ The same atomic bounded batch admission and final supplier agreement apply
  to every selected lifecycle/operator/generic signature. No routine is invoked. }
function CaptureNyxDocumentationDeclaredRoutineSignatures(
  const ASource: INyxPascalSyntaxSnapshot; const ARoutines: array of Integer;
  const AConstruction: INyxDocumentationTypeConstruction): TNyxDocumentationRoutineSignatures;

implementation

uses nyx.pascal.lexing, nyx.pascal.translation, nyx.bytes;

type
  { Construction-only parameter supplier. The ordinary rich-signature capture
    copies it under the caller's allowance; it never escapes into the result or
    starts the independent scope used by the public fluent parameter factory. }
  TParsedParameter = class(TInterfacedObject, INyxDocumentationParameterDeclaration)
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
  { Only an owned header with no authored type and an admitted passing mode
    creates this marker. Nil type alone never stands for a successful capture. }
  TParsedUntypedParameter = class(TParsedParameter, INyxDocumentationUntypedParameter)
  public
    function DeclaredUntyped: Boolean;
  end;
  { Value observations of the bounded parameter prefix, including one optional
    following local declaration. Ordinals have authority only during this call;
    no snapshot, control, executable callback or mutable source is retained. }
  TParameterWitness = record
    Ordinal: Integer;
    Declaration: TNyxPascalSyntaxDeclaration;
  end;
  { At most64 parameters plus one boundary observation. Allocation follows the
    independently checked owned header count, never a supplier-provided size. }
  TParameterWitnesses = array of TParameterWitness;
  { A batch keeps exactly the bounded fields already observed by one capture.
    Later siblings cannot invalidate an earlier selected row unnoticed. Values
    hold no source, parser, construction or cancellation interface. }
  TSignatureWitness = class
  public
    Ordinal: Integer;
    Routine: TNyxPascalSyntaxDeclaration;
    Scope: TNyxPascalSyntaxScope;
    Parameters: TParameterWitnesses;
  end;
  { Construction-local class owners are released even when a later row refuses.
    This is not a publicly retained result or alternative source snapshot. }
  TSignatureWitnesses = array of TSignatureWitness;
  { Immutable syntax needs no alternate-supplier receipts or header reparse.
    This temporary owner retains one cursor across complete sibling scopes;
    local leases protect in-flight calls from reentrant caller retirement. }
  TSignatureSource = class(TInterfacedObject, INyxDocumentationSignatureSource)
  private
    FSource: INyxPascalTranslatedSyntax;
    FSyntax: INyxPascalSyntaxSnapshot;
    FWindows: INyxDocumentationTextWindows;
    FConstruction: INyxDocumentationTypeConstruction;
    FDeclarations, FScopes, FScalars: Integer;
    FRetired, FBusy: Boolean;
    procedure CheckLive(const AConstruction: INyxDocumentationTypeConstruction);
    procedure Spend(const AConstruction: INyxDocumentationTypeConstruction; AUnits: Integer = 1);
    function CaptureOne(ARoutine: Integer;
      const ASyntax: INyxPascalSyntaxSnapshot;
      const AWindows: INyxDocumentationTextWindows;
      const AConstruction: INyxDocumentationTypeConstruction): INyxDocumentationRichSignature;
  public
    constructor Create(const ASource: INyxPascalTranslatedSyntax;
      const AConstruction: INyxDocumentationTypeConstruction);
    destructor Destroy; override;
    procedure Initialize;
    function CaptureDeclared(const ARoutines: TNyxDocumentationRoutineOrdinals): TNyxDocumentationRoutineSignatures;
    procedure Retire;
  end;

function TParsedParameter.Name: TNyxText;
begin
  Result := ParameterName;
end;

function TParsedParameter.TypeUse: INyxDocumentationTypeUse;
begin
  Result := ParameterType;
end;

function TParsedParameter.Passing: TNyxDocumentationParameterMode;
begin
  Result := Mode;
end;

function TParsedParameter.HasDefault: Boolean;
begin
  Result := DefaultPresent;
end;

function TParsedParameter.DefaultPascal: TNyxText;
begin
  Result := DefaultText;
end;

function TParsedParameter.WithDefault(const APascal: TNyxText): INyxDocumentationParameterDeclaration;
var
  LNext: TParsedParameter;
begin
  Result := nil;

  if (ParameterType = nil) or (APascal = '') or (Length(APascal) > 1024) or
    (Mode in [ndpVar, ndpOut]) then
    raise ENyxDocumentationSignature.Create('A parsed parameter requires a bounded non-writable default');

  if NyxUTF8ByteCount(APascal) > 1024 then
    raise ENyxDocumentationSignature.Create('A parsed parameter default exceeds its UTF-8 allowance');
  { This is still a data-only supplier. Publication always passes through
    NewNyxDocumentationRichSignature and its caller-owned aggregate admission. }
  LNext := TParsedParameter.Create;
  LNext.ParameterName := ParameterName;
  LNext.ParameterType := ParameterType;
  LNext.Mode := Mode;
  LNext.DefaultPresent := True;
  LNext.DefaultText := APascal;
  Result := LNext;
end;

function TParsedUntypedParameter.DeclaredUntyped: Boolean;
begin
  Result := True;
end;

function SameSpan(const ALeft, ARight: TNyxPascalSpan): Boolean;
begin
  Result := (ALeft.Start = ARight.Start) and (ALeft.Length = ARight.Length);
end;

function SameLexicalPolicy(const ALeft, ARight: TNyxPascalLexicalOptions): Boolean;
begin
  Result := (ALeft.AllowNestedComments = ARight.AllowNestedComments) and
    (ALeft.AllowUnicodeIdentifiers = ARight.AllowUnicodeIdentifiers) and
    (ALeft.MaximumSourceBytes = ARight.MaximumSourceBytes) and
    (ALeft.MaximumTokens = ARight.MaximumTokens) and
    (ALeft.MaximumDiagnostics = ARight.MaximumDiagnostics) and
    (ALeft.MaximumCommentDepth = ARight.MaximumCommentDepth);
end;

procedure NeedSpan(const ASpan: TNyxPascalSpan; AStart, ALength: Integer;
  ARequired: Boolean);
begin

  if (ASpan.Start < 0) or (ASpan.Length < 0) or
    (AStart < 0) or (ALength < 0) or (AStart > High(Integer) - ALength) or
    (ASpan.Start > High(Integer) - ASpan.Length) then
    raise ENyxDocumentationSignature.Create('Signature span exceeds its scalar domain');

  if (ASpan.Length = 0) and not ARequired then
    Exit;

  if (ARequired and (ASpan.Length = 0)) or (ASpan.Start < AStart) or
    (ASpan.Start > AStart + ALength) or
    (ASpan.Length > AStart + ALength - ASpan.Start) then
    raise ENyxDocumentationSignature.Create('Signature span is outside its selected header');
end;

procedure NeedDeclaration(const AValue: TNyxPascalSyntaxDeclaration);
begin

  if (AValue.Flags * [psdfIncomplete, psdfIncompleteHeader,
    psdfUnsupported, psdfConditional]) <> [] then
    raise ENyxDocumentationSignature.Create('Signature declaration is incomplete or context dependent');

  if (Length(AValue.Name) < 1) or (Length(AValue.Name) > 256) or
    (Length(AValue.OwnerName) > 256) then
    raise ENyxDocumentationSignature.Create('Signature name exceeds its metadata allowance');
end;

function SameDeclaration(const ALeft, ARight: TNyxPascalSyntaxDeclaration): Boolean;
begin
  { All declaration-facing fields relevant to this selected header are checked
    again. Unused body/linkage/visibility metadata grants no result authority. }
  Result := (ALeft.Kind = ARight.Kind) and (ALeft.RoutineKind = ARight.RoutineKind) and
    (ALeft.OperatorKind = ARight.OperatorKind) and
    (ALeft.Name = ARight.Name) and (ALeft.OwnerName = ARight.OwnerName) and
    (ALeft.Flags = ARight.Flags) and (ALeft.Scope = ARight.Scope) and
    (ALeft.NestedScope = ARight.NestedScope) and (ALeft.Next = ARight.Next) and
    (ALeft.ParameterMode = ARight.ParameterMode) and
    SameSpan(ALeft.NameSpan, ARight.NameSpan) and
    SameSpan(ALeft.HeaderSpan, ARight.HeaderSpan) and
    SameSpan(ALeft.TypeSpan, ARight.TypeSpan) and
    SameSpan(ALeft.GenericSpan, ARight.GenericSpan) and
    SameSpan(ALeft.ExpressionSpan, ARight.ExpressionSpan);
end;

function SameScope(const ALeft, ARight: TNyxPascalSyntaxScope): Boolean;
begin
  Result := (ALeft.Kind = ARight.Kind) and (ALeft.Parent = ARight.Parent) and
    (ALeft.OwnerDeclaration = ARight.OwnerDeclaration) and
    (ALeft.FirstDeclaration = ARight.FirstDeclaration) and
    (ALeft.DeclarationCount = ARight.DeclarationCount) and
    (ALeft.Complete = ARight.Complete) and
    (ALeft.UnsupportedSyntax = ARight.UnsupportedSyntax) and
    SameSpan(ALeft.Span, ARight.Span) and (ALeft.Name = ARight.Name);
end;

function ShiftedSpan(const ALocal, ASource: TNyxPascalSpan; AOffset: Integer): Boolean;
begin

  if (ALocal.Length = 0) and (ASource.Length = 0) then
    Exit(True);
  Result := (ALocal.Start <= High(Integer) - AOffset) and
    (ASource.Start = ALocal.Start + AOffset) and (ASource.Length = ALocal.Length);
end;

function PassingMode(AValue: TNyxPascalSyntaxParameterMode): TNyxDocumentationParameterMode;
begin

  if (Ord(AValue) < Ord(Low(TNyxPascalSyntaxParameterMode))) or
    (Ord(AValue) > Ord(High(TNyxPascalSyntaxParameterMode))) then
    raise ENyxDocumentationSignature.Create('Unknown source parameter passing mode');
  case AValue of
    pspmValue: Result := ndpValue;
    pspmConst: Result := ndpConst;
    pspmVar: Result := ndpVar;
    pspmOut: Result := ndpOut;
    pspmConstRef: Result := ndpConstRef;
  end;
end;

constructor TSignatureSource.Create(const ASource: INyxPascalTranslatedSyntax;
  const AConstruction: INyxDocumentationTypeConstruction);
begin
  inherited Create;
  FSource := ASource;
  FConstruction := AConstruction;
end;

destructor TSignatureSource.Destroy;
begin
  Retire;
  inherited Destroy;
end;

procedure TSignatureSource.CheckLive(const AConstruction: INyxDocumentationTypeConstruction);
begin

  if FRetired or (AConstruction = nil) or (FConstruction <> AConstruction) then
    raise ENyxDocumentationTypeExpressionCancelled.Create('Signature source retired');
  CheckNyxDocumentationTypeConstruction(AConstruction);

  if FRetired or (FConstruction <> AConstruction) then
    raise ENyxDocumentationTypeExpressionCancelled.Create('Signature source retired during admission');
end;

procedure TSignatureSource.Spend(const AConstruction: INyxDocumentationTypeConstruction;
  AUnits: Integer);
begin
  CheckLive(AConstruction);
  AConstruction.Spend(AUnits);
  CheckLive(AConstruction);
end;

procedure TSignatureSource.Initialize;
var
  LConstruction: INyxDocumentationTypeConstruction;
  LSource: INyxPascalTranslatedSyntax;
  LTranslation: INyxPascalTranslation;
  LLexical: INyxPascalLexicalSnapshot;
  LRoot: TNyxPascalSyntaxScope;
  LPolicy, LExpected: TNyxPascalLexicalOptions;
begin
  LConstruction := FConstruction;
  LSource := FSource;

  if not NyxPascalOwnedTranslatedSyntax(LSource) then
    raise ENyxDocumentationSignature.Create('A signature source requires the exact owned translated snapshot');
  Spend(LConstruction);

  if not LSource.CompleteTranslation then
    raise ENyxDocumentationSignature.Create('A signature source requires complete resolved translation');
  Spend(LConstruction);

  if LSource.BlockedAt <> -1 then
    raise ENyxDocumentationSignature.Create('A signature source contains an uncertain translated prefix');
  Spend(LConstruction);
  LTranslation := LSource.Source;
  Spend(LConstruction);

  if (LTranslation = nil) or not LTranslation.Resolved then
    raise ENyxDocumentationSignature.Create('A signature source requires resolved translation');
  Spend(LConstruction);

  if LTranslation.DiagnosticCount <> 0 then
    raise ENyxDocumentationSignature.Create('A signature source contains translation diagnostics');
  Spend(LConstruction);
  FSyntax := LSource.Syntax;
  Spend(LConstruction);

  if FSyntax.DiagnosticCount <> 0 then
    raise ENyxDocumentationSignature.Create('A signature source contains declaration diagnostics');
  Spend(LConstruction);
  FDeclarations := FSyntax.DeclarationCount;
  Spend(LConstruction);
  FScopes := FSyntax.ScopeCount;

  if (FDeclarations < 0) or (FScopes < 1) then
    raise ENyxDocumentationSignature.Create('A signature source has no complete module scope');
  Spend(LConstruction);
  LRoot := FSyntax.Scope(0);

  if (LRoot.Kind <> psskModule) or not LRoot.Complete or LRoot.UnsupportedSyntax then
    raise ENyxDocumentationSignature.Create('A signature source module is incomplete or unsupported');
  Spend(LConstruction);
  LLexical := FSyntax.Lexical;
  Spend(LConstruction);

  if LLexical.DiagnosticCount <> 0 then
    raise ENyxDocumentationSignature.Create('A signature source contains lexical diagnostics');
  Spend(LConstruction);
  FScalars := LLexical.Scalars;
  Spend(LConstruction);
  LPolicy := LLexical.Options;
  LExpected := LConstruction.Limits.Lexical;

  if (LPolicy.AllowNestedComments <> LExpected.AllowNestedComments) or
    (LPolicy.AllowUnicodeIdentifiers <> LExpected.AllowUnicodeIdentifiers) then
    raise ENyxDocumentationSignature.Create('A signature source needs the caller lexical policy');
  Spend(LConstruction);
  FWindows := NewNyxDocumentationTextWindows(LLexical.Source, LConstruction);
  CheckLive(LConstruction);
end;

procedure TSignatureSource.Retire;
begin

  if FRetired then
    Exit;
  FRetired := True;

  if FWindows <> nil then
    FWindows.Retire;
  FWindows := nil;
  FSyntax := nil;
  FSource := nil;
  FConstruction := nil;
end;

function TSignatureSource.CaptureOne(ARoutine: Integer;
  const ASyntax: INyxPascalSyntaxSnapshot;
  const AWindows: INyxDocumentationTextWindows;
  const AConstruction: INyxDocumentationTypeConstruction): INyxDocumentationRichSignature;
var
  LRoutine, LParameter: TNyxPascalSyntaxDeclaration;
  LScope: TNyxPascalSyntaxScope;
  LRows: array of TNyxPascalSyntaxDeclaration;
  LParameters: TNyxDocumentationParameterDeclarations;
  LBuilder: TParsedParameter;
  LExpression: INyxDocumentationTypeExpression;
  LResult: INyxDocumentationTypeUse;
  LCandidate: INyxDocumentationRichSignature;
  LText: TNyxText;
  LAt, LCount, LNext: Integer;
begin
  Result := nil;
  Spend(AConstruction);

  if (ARoutine < 0) or (ARoutine >= FDeclarations) then
    raise ENyxDocumentationSignature.Create('Selected routine is outside this signature source');
  LRoutine := ASyntax.Declaration(ARoutine);
  NeedDeclaration(LRoutine);

  if (LRoutine.Kind <> psdkRoutine) or
    not (LRoutine.RoutineKind in [prkProcedure, prkFunction, prkConstructor,
      prkDestructor, prkOperator]) then
    raise ENyxDocumentationSignature.Create('Select a representable declared routine');
  NeedSpan(LRoutine.HeaderSpan, 0, FScalars, True);
  NeedSpan(LRoutine.NameSpan, LRoutine.HeaderSpan.Start, LRoutine.HeaderSpan.Length, True);
  NeedSpan(LRoutine.GenericSpan, LRoutine.HeaderSpan.Start, LRoutine.HeaderSpan.Length, False);
  NeedSpan(LRoutine.TypeSpan, LRoutine.HeaderSpan.Start, LRoutine.HeaderSpan.Length,
    LRoutine.RoutineKind = prkFunction);

  if (LRoutine.RoutineKind in [prkProcedure, prkConstructor, prkDestructor]) and
    (LRoutine.TypeSpan.Length <> 0) then
    raise ENyxDocumentationSignature.Create('This declared routine cannot have a result');

  if (LRoutine.Scope < 0) or (LRoutine.Scope >= FScopes) or
    (LRoutine.NestedScope < 0) or (LRoutine.NestedScope >= FScopes) then
    raise ENyxDocumentationSignature.Create('A declared routine needs its complete parameter scope');
  Spend(AConstruction);
  LScope := ASyntax.Scope(LRoutine.NestedScope);

  if (LScope.Kind <> psskRoutine) or not LScope.Complete or LScope.UnsupportedSyntax or
    (LScope.OwnerDeclaration <> ARoutine) or (LScope.Parent <> LRoutine.Scope) or
    (LScope.DeclarationCount < 0) then
    raise ENyxDocumentationSignature.Create('A declared routine parameter scope is incomplete');
  { Only the bounded checked parameter prefix is visited. Ordinary body locals
    never enter the detached signature or require a scan through a routine body. }
  Spend(AConstruction, NyxDocumentationMaximumParameters + 1);
  SetLength(LRows, NyxDocumentationMaximumParameters);
  LCount := 0;
  LNext := LScope.FirstDeclaration;
  while LNext <> -1 do
  begin
    Spend(AConstruction);

    if (LNext < 0) or (LNext >= FDeclarations) then
      raise ENyxDocumentationSignature.Create('A declared parameter lies outside its owned source');
    LParameter := ASyntax.Declaration(LNext);
    NeedDeclaration(LParameter);

    if LParameter.Scope <> LRoutine.NestedScope then
      raise ENyxDocumentationSignature.Create('A declared parameter belongs to another routine');

    if LParameter.Kind <> psdkParameter then
    begin

      if (LRoutine.RoutineKind = prkOperator) and (LParameter.Kind = psdkVariable) and
        SameSpan(LParameter.TypeSpan, LRoutine.TypeSpan) and
        (LRoutine.TypeSpan.Length > 0) then
        NeedSpan(LParameter.NameSpan, LRoutine.HeaderSpan.Start, LRoutine.HeaderSpan.Length, True)
      else if LParameter.NameSpan.Start < LRoutine.HeaderSpan.Finish then
        raise ENyxDocumentationSignature.Create('A declared header contains an unsupported parameter row');
      Break;
    end;

    if (LCount >= NyxDocumentationMaximumParameters) or (LCount >= LScope.DeclarationCount) then
      raise ENyxDocumentationSignature.Create('A declared signature exceeds its parameter allowance');
    NeedSpan(LParameter.NameSpan, LRoutine.HeaderSpan.Start, LRoutine.HeaderSpan.Length, True);
    NeedSpan(LParameter.TypeSpan, LRoutine.HeaderSpan.Start, LRoutine.HeaderSpan.Length, False);
    NeedSpan(LParameter.ExpressionSpan, LRoutine.HeaderSpan.Start, LRoutine.HeaderSpan.Length,
      psdfDefaultArgument in LParameter.Flags);

    if not (psdfDefaultArgument in LParameter.Flags) and (LParameter.ExpressionSpan.Length <> 0) then
      raise ENyxDocumentationSignature.Create('An absent parameter default retains unexpected text');
    LRows[LCount] := LParameter;
    Inc(LCount);
    LNext := LParameter.Next;
  end;

  if (LNext = -1) and (LScope.DeclarationCount <> LCount) then
    raise ENyxDocumentationSignature.Create('A complete parameter scope has inconsistent declaration counts');
  Spend(AConstruction, LCount + 1);
  SetLength(LParameters, LCount);
  for LAt := 0 to LCount - 1 do
  begin
    Spend(AConstruction);
    LParameter := LRows[LAt];

    if LParameter.TypeSpan.Length = 0 then
    begin

      if not (LParameter.ParameterMode in [pspmConst, pspmVar, pspmOut]) or
        (psdfDefaultArgument in LParameter.Flags) then
        raise ENyxDocumentationSignature.Create('This declared parameter requires an explicit type');
      LBuilder := TParsedUntypedParameter.Create;
    end
    else
      LBuilder := TParsedParameter.Create;
    LParameters[LAt] := LBuilder;
    LBuilder.ParameterName := LParameter.Name;
    LBuilder.Mode := PassingMode(LParameter.ParameterMode);
    LBuilder.DefaultPresent := psdfDefaultArgument in LParameter.Flags;

    if LParameter.TypeSpan.Length <> 0 then
    begin
      LText := AWindows.CopyWindow(LParameter.TypeSpan);
      CheckLive(AConstruction);
      LExpression := NewNyxDocumentationTypeExpression(LText, ndtoParameter, AConstruction);
      CheckLive(AConstruction);
      LBuilder.ParameterType := NewNyxDocumentationExpressionTypeUse(LExpression, AConstruction);
      CheckLive(AConstruction);
    end;

    if LBuilder.DefaultPresent then
    begin
      LBuilder.DefaultText := AWindows.CopyWindow(LParameter.ExpressionSpan);
      CheckLive(AConstruction);
    end;
  end;
  LResult := nil;

  if LRoutine.TypeSpan.Length <> 0 then
  begin
    LText := AWindows.CopyWindow(LRoutine.TypeSpan);
    CheckLive(AConstruction);
    LExpression := NewNyxDocumentationTypeExpression(LText, ndtoResult, AConstruction);
    CheckLive(AConstruction);
    LResult := NewNyxDocumentationExpressionTypeUse(LExpression, AConstruction);
    CheckLive(AConstruction);
  end;
  LCandidate := NewNyxDocumentationRichSignature(LParameters, LResult, AConstruction);
  Spend(AConstruction);
  Result := LCandidate;
end;

function TSignatureSource.CaptureDeclared(
  const ARoutines: TNyxDocumentationRoutineOrdinals): TNyxDocumentationRoutineSignatures;
var
  LInput, LSelection: TNyxDocumentationRoutineOrdinals;
  LConstruction: INyxDocumentationTypeConstruction;
  LSyntax: INyxPascalSyntaxSnapshot;
  LWindows: INyxDocumentationTextWindows;
  LLease: INyxDocumentationSignatureSource;
  LCandidate: TNyxDocumentationRoutineSignatures;
  LAt, LBefore: Integer;
begin
  Result := nil;
  { Dynamic-array and interface value leases precede the first outward call.
    Releasing caller variables cannot invalidate in-flight borrowed storage. }
  LInput := ARoutines;
  LLease := Self;
  LConstruction := FConstruction;
  LSyntax := FSyntax;
  LWindows := FWindows;
  CheckLive(LConstruction);

  if FBusy then
    raise ENyxDocumentationSignature.Create('A signature source cannot capture reentrantly');
  FBusy := True;
  try

    if (Length(LInput) < 1) or (Length(LInput) > NyxDocumentationMaximumMembers) then
      raise ENyxDocumentationSignature.Create('A signature selection exceeds its existing member allowance');
    Spend(LConstruction, Length(LInput) * 2 + 1);
    LSelection := Copy(LInput);
    SetLength(LCandidate, Length(LSelection));
    for LAt := 0 to High(LSelection) do
    begin
      Spend(LConstruction);

      if (LSelection[LAt] < 0) or (LSelection[LAt] >= FDeclarations) then
        raise ENyxDocumentationSignature.Create('Selected routine is outside this signature source');
      for LBefore := 0 to LAt - 1 do
      begin
        Spend(LConstruction);

        if LSelection[LBefore] = LSelection[LAt] then
          raise ENyxDocumentationSignature.Create('Duplicate declared routine selection');
      end;
    end;
    for LAt := 0 to High(LSelection) do
      LCandidate[LAt] := CaptureOne(LSelection[LAt], LSyntax, LWindows, LConstruction);
    Spend(LConstruction, Length(LSelection) + 1);
    { No callback follows agreement. Shared-element mutation at the last
      outward admission refuses, while a caller dropping its handle is safe. }
    for LAt := 0 to High(LSelection) do
    begin

      if LInput[LAt] <> LSelection[LAt] then
        raise ENyxDocumentationSignature.Create('Caller selection changed before signature publication');
    end;
    Result := LCandidate;
  finally
    FBusy := False;
    LLease := nil;
  end;
end;

function NewNyxDocumentationSignatureSource(
  const ASource: INyxPascalTranslatedSyntax;
  const AConstruction: INyxDocumentationTypeConstruction): INyxDocumentationSignatureSource;
var
  LSource: INyxPascalTranslatedSyntax;
  LConstruction: INyxDocumentationTypeConstruction;
  LCandidate: INyxDocumentationSignatureSource;
  LOwner: TSignatureSource;
begin
  Result := nil;
  LSource := ASource;
  LConstruction := AConstruction;

  if not NyxPascalOwnedTranslatedSyntax(LSource) then
    raise ENyxDocumentationSignature.Create('A signature source requires the exact owned translated snapshot');
  CheckNyxDocumentationTypeConstruction(LConstruction);
  LOwner := TSignatureSource.Create(LSource, LConstruction);
  LCandidate := LOwner;
  LOwner.Initialize;
  CheckNyxDocumentationTypeConstruction(LConstruction);
  Result := LCandidate;
end;

function CaptureRoutineSignature(
  const ASource: INyxPascalSyntaxSnapshot; ARoutine: Integer;
  const AConstruction: INyxDocumentationTypeConstruction;
  const AWindows: INyxDocumentationTextWindows;
  const AExpectedLexical: INyxPascalLexicalSnapshot;
  AWitness: TSignatureWitness; ADeclared: Boolean): INyxDocumentationRichSignature;
var
  LLexical: INyxPascalLexicalSnapshot;
  LParsed: INyxPascalSyntaxSnapshot;
  LSupplierPolicy, LPolicy: TNyxPascalLexicalOptions;
  LSyntaxPolicy: TNyxPascalSyntaxOptions;
  LRoutine, LOwnedRoutine, LParameter, LOriginal: TNyxPascalSyntaxDeclaration;
  LScope, LOwnedScope, LModule: TNyxPascalSyntaxScope;
  LParameters: TNyxDocumentationParameterDeclarations;
  LWitnesses: TParameterWitnesses;
  LBuilder: TParsedParameter;
  LResultUse: INyxDocumentationTypeUse;
  LExpression: INyxDocumentationTypeExpression;
  LCandidate: INyxDocumentationRichSignature;
  LText, LHeader, LAgain, LTypeText: TNyxText;
  LDeclarationCount, LScopeCount, LScalars, LAt, LNext, LOriginalAt,
    LRoot, LParameterCount, LObserved, LBoundary, LOwnedBoundary: Integer;
  procedure AdmitSource(AUnits: Integer = 1);
  begin
    AConstruction.Spend(AUnits);
    { Legacy single selection retains its original receipt/observations. A batch
      also revokes the next supplier getter when outward admission cancels. }

    if ADeclared or (AWindows <> nil) then
      CheckNyxDocumentationTypeConstruction(AConstruction);
  end;
begin
  Result := nil;
  CheckNyxDocumentationTypeConstruction(AConstruction);
  AdmitSource;

  if ASource = nil then
    raise ENyxDocumentationSignature.Create('A source syntax snapshot is required');
  AdmitSource;
  LDeclarationCount := ASource.DeclarationCount;

  if (ARoutine < 0) or (LDeclarationCount < 1) or (ARoutine >= LDeclarationCount) then
    raise ENyxDocumentationSignature.Create('Selected routine ordinal is outside this snapshot');
  AdmitSource;
  LRoutine := ASource.Declaration(ARoutine);
  NeedDeclaration(LRoutine);

  if (LRoutine.Kind <> psdkRoutine) or
    (not ADeclared and (not (LRoutine.RoutineKind in [prkProcedure, prkFunction]) or
      (LRoutine.GenericSpan.Length <> 0))) or
    (ADeclared and not (LRoutine.RoutineKind in [prkProcedure, prkFunction,
      prkConstructor, prkDestructor, prkOperator])) then
    raise ENyxDocumentationSignature.Create('Select a representable non-generic procedure/function header');
  AdmitSource;
  LScopeCount := ASource.ScopeCount;

  if (LRoutine.Scope < 0) or (LRoutine.Scope >= LScopeCount) or
    (LRoutine.NestedScope < 0) or (LRoutine.NestedScope >= LScopeCount) then
    raise ENyxDocumentationSignature.Create('Selected routine has no valid parameter scope');
  AdmitSource;
  LScope := ASource.Scope(LRoutine.NestedScope);

  if (LScope.Kind <> psskRoutine) or (LScope.OwnerDeclaration <> ARoutine) or
    (LScope.Parent <> LRoutine.Scope) or not LScope.Complete or LScope.UnsupportedSyntax or
    (LScope.DeclarationCount < 0) or (Length(LScope.Name) > 256) then
    raise ENyxDocumentationSignature.Create('Selected routine does not own a complete parameter scope');
  AdmitSource;
  LLexical := ASource.Lexical;

  if LLexical = nil then
    raise ENyxDocumentationSignature.Create('Selected syntax has no lexical source');

  if (AExpectedLexical <> nil) and (LLexical <> AExpectedLexical) then
    raise ENyxDocumentationSignature.Create('Batch lexical owner changed before selected capture');
  AdmitSource;
  LScalars := LLexical.Scalars;
  NeedSpan(LRoutine.HeaderSpan, 0, LScalars, True);
  NeedSpan(LRoutine.NameSpan, LRoutine.HeaderSpan.Start, LRoutine.HeaderSpan.Length, True);
  NeedSpan(LRoutine.TypeSpan, LRoutine.HeaderSpan.Start, LRoutine.HeaderSpan.Length,
    LRoutine.RoutineKind = prkFunction);

  if (LRoutine.RoutineKind in [prkProcedure, prkConstructor, prkDestructor]) and
    (LRoutine.TypeSpan.Length <> 0) then
    raise ENyxDocumentationSignature.Create('This routine cannot claim a function result');

  if ADeclared then
    NeedSpan(LRoutine.GenericSpan, LRoutine.HeaderSpan.Start, LRoutine.HeaderSpan.Length, False);
  AdmitSource;
  LSupplierPolicy := LLexical.Options;
  LSupplierPolicy.Validate;
  LPolicy := AConstruction.Limits.Lexical;

  if (LSupplierPolicy.AllowNestedComments <> LPolicy.AllowNestedComments) or
    (LSupplierPolicy.AllowUnicodeIdentifiers <> LPolicy.AllowUnicodeIdentifiers) then
    raise ENyxDocumentationSignature.Create('Selected header needs a different lexical policy');
  if AWindows = nil then
  begin
    AdmitSource;
    LText := LLexical.Source;
    LHeader := CopyNyxDocumentationTypeTextWindow(LText, LRoutine.HeaderSpan, AConstruction);
    LText := '';
  end
  else
    LHeader := AWindows.CopyWindow(LRoutine.HeaderSpan);
  LSyntaxPolicy := TNyxPascalSyntaxOptions.Defaults.Fragment(psfInterfaceDeclarations).Lexical(LPolicy);
  try
    LParsed := NewNyxPascalSyntaxSnapshot(LHeader, LSyntaxPolicy, AConstruction);
  except
    on E: ENyxPascalAnalysisCancelled do
      raise ENyxDocumentationTypeExpressionCancelled.Create(E.Message);
  end;

  if (LParsed.Lexical.DiagnosticCount <> 0) or (LParsed.DiagnosticCount <> 0) or
    (LParsed.Lexical.Scalars <> LRoutine.HeaderSpan.Length) then
    raise ENyxDocumentationSignature.Create('Selected header cannot be reparsed as complete declared syntax');
  for LAt := 0 to LParsed.Lexical.Count - 1 do
  begin
    AdmitSource;

    if LParsed.Lexical.Token(LAt).Kind in [ptkDirective, ptkAssembly] then
      raise ENyxDocumentationSignature.Create('Selected header contains context-dependent source');
  end;
  AdmitSource;
  LModule := LParsed.Scope(0);

  if not LModule.Complete or LModule.UnsupportedSyntax or
    (LModule.DeclarationCount <> 1) then
    raise ENyxDocumentationSignature.Create('Selected window must contain exactly one declared routine');
  LRoot := LModule.FirstDeclaration;
  AdmitSource;
  LOwnedRoutine := LParsed.Declaration(LRoot);
  NeedDeclaration(LOwnedRoutine);

  if (LOwnedRoutine.Kind <> psdkRoutine) or (LOwnedRoutine.Scope <> 0) or
    (LOwnedRoutine.RoutineKind <> LRoutine.RoutineKind) or
    (LOwnedRoutine.Name <> LRoutine.Name) or (LOwnedRoutine.OwnerName <> LRoutine.OwnerName) or
    (not ADeclared and (LOwnedRoutine.GenericSpan.Length <> 0)) or
    (ADeclared and ((LOwnedRoutine.OperatorKind <> LRoutine.OperatorKind) or
      not ShiftedSpan(LOwnedRoutine.GenericSpan, LRoutine.GenericSpan, LRoutine.HeaderSpan.Start))) or
    not ShiftedSpan(LOwnedRoutine.NameSpan, LRoutine.NameSpan, LRoutine.HeaderSpan.Start) or
    not ShiftedSpan(LOwnedRoutine.TypeSpan, LRoutine.TypeSpan, LRoutine.HeaderSpan.Start) then
    raise ENyxDocumentationSignature.Create('Supplied declaration disagrees with the owned header');
  AdmitSource;
  LOwnedScope := LParsed.Scope(LOwnedRoutine.NestedScope);
  LParameterCount := LOwnedScope.DeclarationCount;
  LOwnedBoundary := -1;

  if ADeclared and (LOwnedRoutine.RoutineKind = prkOperator) then
  begin
    { FPC operators may name their result between the parameter list and colon.
      That variable belongs to the header, never the public parameter vector. }
    LParameterCount := 0;
    LNext := LOwnedScope.FirstDeclaration;
    while LNext <> -1 do
    begin
      AdmitSource;

      if (LNext < 0) or (LNext >= LParsed.DeclarationCount) or
        (LParameterCount > NyxDocumentationMaximumParameters) then
        raise ENyxDocumentationSignature.Create('Operator header exceeds its bounded parameter prefix');
      LParameter := LParsed.Declaration(LNext);
      NeedDeclaration(LParameter);

      if LParameter.Kind <> psdkParameter then
      begin

        if (LParameter.Kind <> psdkVariable) or (LParameter.Next <> -1) or
          (LOwnedRoutine.TypeSpan.Length = 0) or
          not SameSpan(LParameter.TypeSpan, LOwnedRoutine.TypeSpan) then
          raise ENyxDocumentationSignature.Create('Operator header has an unsupported result declaration');
        LOwnedBoundary := LNext;
        Break;
      end;
      Inc(LParameterCount);
      LNext := LParameter.Next;
    end;
  end;

  if not LOwnedScope.Complete or LOwnedScope.UnsupportedSyntax or
    (LOwnedScope.Kind <> psskRoutine) or (LOwnedScope.OwnerDeclaration <> LRoot) or
    (LParameterCount < 0) or (LParameterCount > NyxDocumentationMaximumParameters) or
    (LScope.DeclarationCount < LParameterCount) then
    raise ENyxDocumentationSignature.Create('Selected signature exceeds its complete parameter allowance');
  AdmitSource(LParameterCount + 1);
  SetLength(LParameters, LParameterCount);
  SetLength(LWitnesses, LParameterCount + 1);
  LNext := LOwnedScope.FirstDeclaration;
  LOriginalAt := LScope.FirstDeclaration;
  LObserved := 0;
  for LAt := 0 to LParameterCount - 1 do
  begin
    AdmitSource;

    if (LOriginalAt < 0) or (LOriginalAt >= LDeclarationCount) then
      raise ENyxDocumentationSignature.Create('Supplied parameter prefix ends before its owned header');
    LOriginal := ASource.Declaration(LOriginalAt);
    NeedDeclaration(LOriginal);
    AdmitSource;
    LParameter := LParsed.Declaration(LNext);
    NeedDeclaration(LParameter);

    if (LParameter.Kind <> psdkParameter) or (LOriginal.Kind <> psdkParameter) or
      (LOriginal.Scope <> LRoutine.NestedScope) or
      (LOriginal.Name <> LParameter.Name) or
      (LOriginal.ParameterMode <> LParameter.ParameterMode) or
      ((psdfDefaultArgument in LOriginal.Flags) <> (psdfDefaultArgument in LParameter.Flags)) or
      not ShiftedSpan(LParameter.NameSpan, LOriginal.NameSpan, LRoutine.HeaderSpan.Start) or
      not ShiftedSpan(LParameter.TypeSpan, LOriginal.TypeSpan, LRoutine.HeaderSpan.Start) or
      not ShiftedSpan(LParameter.ExpressionSpan, LOriginal.ExpressionSpan, LRoutine.HeaderSpan.Start) then
      raise ENyxDocumentationSignature.Create('Supplied parameter order or declaration disagrees with its owned header');
    NeedSpan(LParameter.TypeSpan, 0, LParsed.Lexical.Scalars,
      not ADeclared or (LParameter.TypeSpan.Length <> 0));
    NeedSpan(LParameter.ExpressionSpan, 0, LParsed.Lexical.Scalars,
      psdfDefaultArgument in LParameter.Flags);

    if not (psdfDefaultArgument in LParameter.Flags) and
      (LParameter.ExpressionSpan.Length <> 0) then
      raise ENyxDocumentationSignature.Create('Absent parameter default retains unexpected text');
    LWitnesses[LObserved].Ordinal := LOriginalAt;
    LWitnesses[LObserved].Declaration := LOriginal;
    Inc(LObserved);
    if LParameter.TypeSpan.Length = 0 then
    begin

      if not ADeclared or not (LParameter.ParameterMode in [pspmConst, pspmVar, pspmOut]) or
        (psdfDefaultArgument in LParameter.Flags) then
        raise ENyxDocumentationSignature.Create('This declared parameter requires an explicit type');
      LBuilder := TParsedUntypedParameter.Create;
    end
    else
      LBuilder := TParsedParameter.Create;
    LParameters[LAt] := LBuilder;
    LBuilder.ParameterName := LParameter.Name;
    LBuilder.Mode := PassingMode(LParameter.ParameterMode);
    LBuilder.DefaultPresent := psdfDefaultArgument in LParameter.Flags;
    if LParameter.TypeSpan.Length <> 0 then
    begin
      LTypeText := CopyNyxDocumentationTypeTextWindow(LHeader, LParameter.TypeSpan, AConstruction);
      LExpression := NewNyxDocumentationTypeExpression(LTypeText, ndtoParameter, AConstruction);
      LBuilder.ParameterType := NewNyxDocumentationExpressionTypeUse(LExpression, AConstruction);
    end;

    if LBuilder.DefaultPresent then
      LBuilder.DefaultText := CopyNyxDocumentationTypeTextWindow(LHeader,
        LParameter.ExpressionSpan, AConstruction);
    LNext := LParameter.Next;
    LOriginalAt := LOriginal.Next;
  end;

  if LNext <> LOwnedBoundary then
    raise ENyxDocumentationSignature.Create('Owned parameter list retains an unexpected declaration');
  { Implementation scopes may have ordinary locals after the parameter prefix.
    They are not copied as signature facts. One boundary read catches extra
    parameters without traversing arbitrary local declarations or bodies. }
  LBoundary := LOriginalAt;

  if LBoundary <> -1 then
  begin
    AdmitSource;

    if (LBoundary < 0) or (LBoundary >= LDeclarationCount) then
      raise ENyxDocumentationSignature.Create('Supplied parameter boundary is outside its snapshot');
    LOriginal := ASource.Declaration(LBoundary);

    if LOwnedBoundary <> -1 then
    begin
      AdmitSource;
      LParameter := LParsed.Declaration(LOwnedBoundary);

      if (LOriginal.Kind <> psdkVariable) or (LOriginal.Scope <> LRoutine.NestedScope) or
        (LOriginal.Name <> LParameter.Name) or
        not ShiftedSpan(LParameter.NameSpan, LOriginal.NameSpan, LRoutine.HeaderSpan.Start) or
        not ShiftedSpan(LParameter.TypeSpan, LOriginal.TypeSpan, LRoutine.HeaderSpan.Start) then
        raise ENyxDocumentationSignature.Create('Operator result declaration disagrees with its owned header');
    end
    else
    begin

      if (LOriginal.Kind = psdkParameter) or (LOriginal.Scope <> LRoutine.NestedScope) or
        (LOriginal.NameSpan.Start < LRoutine.HeaderSpan.Finish) then
        raise ENyxDocumentationSignature.Create('Supplied signature has an inconsistent parameter boundary');
    end;
    LWitnesses[LObserved].Ordinal := LBoundary;
    LWitnesses[LObserved].Declaration := LOriginal;
    Inc(LObserved);
  end
  else if LScope.DeclarationCount <> LParameterCount then
    raise ENyxDocumentationSignature.Create('Supplied routine scope count disagrees with its parameter boundary');
  LResultUse := nil;

  if (LOwnedRoutine.RoutineKind = prkFunction) or
    (ADeclared and (LOwnedRoutine.RoutineKind = prkOperator) and
      (LOwnedRoutine.TypeSpan.Length <> 0)) then
  begin
    NeedSpan(LOwnedRoutine.TypeSpan, 0, LParsed.Lexical.Scalars, True);
    LTypeText := CopyNyxDocumentationTypeTextWindow(LHeader, LOwnedRoutine.TypeSpan, AConstruction);
    LExpression := NewNyxDocumentationTypeExpression(LTypeText, ndtoResult, AConstruction);
    LResultUse := NewNyxDocumentationExpressionTypeUse(LExpression, AConstruction);
  end;
  LCandidate := NewNyxDocumentationRichSignature(LParameters, LResultUse, AConstruction);
  { Reobserve only the bounded context used above. Equality of supplier
    interfaces or claimed receipts never substitutes for these observations. }
  AdmitSource;

  if (AWindows = nil) and not ADeclared then
  begin

    if (ASource.DeclarationCount <> LDeclarationCount) or
      (ASource.ScopeCount <> LScopeCount) or (ASource.Lexical <> LLexical) or
      (LLexical.Scalars <> LScalars) or not SameLexicalPolicy(LLexical.Options, LSupplierPolicy) then
      raise ENyxDocumentationSignature.Create('Source envelope changed before signature publication');
  end
  else
  begin

    if ASource.DeclarationCount <> LDeclarationCount then
      raise ENyxDocumentationSignature.Create('Selected batch declaration extent changed');
    AdmitSource;

    if ASource.ScopeCount <> LScopeCount then
      raise ENyxDocumentationSignature.Create('Selected batch scope extent changed');
    AdmitSource;

    if ASource.Lexical <> LLexical then
      raise ENyxDocumentationSignature.Create('Selected batch lexical owner changed');
    AdmitSource;

    if LLexical.Scalars <> LScalars then
      raise ENyxDocumentationSignature.Create('Selected batch scalar extent changed');
    AdmitSource;

    if not SameLexicalPolicy(LLexical.Options, LSupplierPolicy) then
      raise ENyxDocumentationSignature.Create('Selected batch lexical policy changed');
  end;
  AdmitSource;

  if (AWindows = nil) and not ADeclared then
  begin

    if not SameDeclaration(ASource.Declaration(ARoutine), LRoutine) or
      not SameScope(ASource.Scope(LRoutine.NestedScope), LScope) then
      raise ENyxDocumentationSignature.Create('Selected routine changed before signature publication');
  end
  else
  begin

    if not SameDeclaration(ASource.Declaration(ARoutine), LRoutine) then
      raise ENyxDocumentationSignature.Create('Selected batch routine changed before publication');
    AdmitSource;

    if not SameScope(ASource.Scope(LRoutine.NestedScope), LScope) then
      raise ENyxDocumentationSignature.Create('Selected batch scope changed before publication');
  end;
  for LAt := 0 to LObserved - 1 do
  begin
    AdmitSource;

    if not SameDeclaration(ASource.Declaration(LWitnesses[LAt].Ordinal), LWitnesses[LAt].Declaration) then
      raise ENyxDocumentationSignature.Create('Selected parameter changed before signature publication');
  end;

  if AWindows = nil then
  begin
    AdmitSource;
    LText := LLexical.Source;
    LAgain := CopyNyxDocumentationTypeTextWindow(LText, LRoutine.HeaderSpan, AConstruction);
    AdmitSource(Length(LHeader) + 1);

    if LAgain <> LHeader then
      raise ENyxDocumentationSignature.Create('Selected header text changed before signature publication');
  end;

  if AWitness <> nil then
  begin
    AdmitSource(LObserved + 4);
    CheckNyxDocumentationTypeConstruction(AConstruction);
    AWitness.Ordinal := ARoutine;
    AWitness.Routine := LRoutine;
    AWitness.Scope := LScope;
    SetLength(LWitnesses, LObserved);
    AWitness.Parameters := LWitnesses;
  end;
  AdmitSource;
  CheckNyxDocumentationTypeConstruction(AConstruction);
  Result := LCandidate;
end;

function CaptureNyxDocumentationRoutineSignature(
  const ASource: INyxPascalSyntaxSnapshot; ARoutine: Integer;
  const AConstruction: INyxDocumentationTypeConstruction): INyxDocumentationRichSignature;
begin
  Result := CaptureRoutineSignature(ASource, ARoutine, AConstruction, nil, nil, nil, False);
end;

function CaptureRoutineSignatures(
  const ASource: INyxPascalSyntaxSnapshot; const ARoutines: array of Integer;
  const AConstruction: INyxDocumentationTypeConstruction;
  ADeclared: Boolean): TNyxDocumentationRoutineSignatures;
var
  LSource: INyxPascalSyntaxSnapshot;
  LConstruction: INyxDocumentationTypeConstruction;
  LLexical: INyxPascalLexicalSnapshot;
  LPolicy: TNyxPascalLexicalOptions;
  LWindows: INyxDocumentationTextWindows;
  LText, LFinalText: TNyxText;
  LDeclarations, LScopes, LScalars, LAt, LBefore, LParameter: Integer;
  { Caller arrays may be changed by an outward admission/getter callback. }
  LSelection: array of Integer;
  LWitnesses: TSignatureWitnesses;
  LValues: TNyxDocumentationRoutineSignatures;
begin
  Result := nil;
  { Const interface arguments are borrowed slots on some native ABIs. Retain
    their exact values before outward callbacks can release caller variables. }
  LSource := ASource;
  LConstruction := AConstruction;
  CheckNyxDocumentationTypeConstruction(LConstruction);
  LConstruction.Spend;
  CheckNyxDocumentationTypeConstruction(LConstruction);

  if (LSource = nil) or (Length(ARoutines) < 1) or
    (Length(ARoutines) > NyxDocumentationMaximumMembers) then
    raise ENyxDocumentationSignature.Create('A signature batch requires a bounded nonempty selection');
  LConstruction.Spend(Length(ARoutines) + 1);
  CheckNyxDocumentationTypeConstruction(LConstruction);
  SetLength(LSelection, Length(ARoutines));
  for LAt := 0 to High(ARoutines) do
    LSelection[LAt] := ARoutines[LAt];
  LConstruction.Spend;
  CheckNyxDocumentationTypeConstruction(LConstruction);
  LDeclarations := LSource.DeclarationCount;
  for LAt := 0 to High(LSelection) do
  begin
    LConstruction.Spend;
    CheckNyxDocumentationTypeConstruction(LConstruction);

    if (LSelection[LAt] < 0) or (LSelection[LAt] >= LDeclarations) then
      raise ENyxDocumentationSignature.Create('Batch routine ordinal is outside its exact snapshot');
    for LBefore := 0 to LAt - 1 do
    begin
      LConstruction.Spend;
      CheckNyxDocumentationTypeConstruction(LConstruction);

      if LSelection[LBefore] = LSelection[LAt] then
        raise ENyxDocumentationSignature.Create('A signature batch cannot repeat a selected ordinal');
    end;
  end;
  LConstruction.Spend;
  CheckNyxDocumentationTypeConstruction(LConstruction);
  LScopes := LSource.ScopeCount;
  LConstruction.Spend;
  CheckNyxDocumentationTypeConstruction(LConstruction);
  LLexical := LSource.Lexical;

  if LLexical = nil then
    raise ENyxDocumentationSignature.Create('A signature batch requires its lexical owner');
  LConstruction.Spend;
  CheckNyxDocumentationTypeConstruction(LConstruction);
  LPolicy := LLexical.Options;
  LPolicy.Validate;
  LConstruction.Spend;
  CheckNyxDocumentationTypeConstruction(LConstruction);
  LScalars := LLexical.Scalars;
  LConstruction.Spend;
  CheckNyxDocumentationTypeConstruction(LConstruction);
  LText := LLexical.Source;
  CheckNyxDocumentationTypeConstruction(LConstruction);

  if Length(LText) >= High(Integer) then
    raise ENyxDocumentationSignature.Create('Batch source exceeds its comparison domain');
  LWindows := NewNyxDocumentationTextWindows(LText, LConstruction);
  try
    LConstruction.Spend(Length(LSelection) + 1);
    CheckNyxDocumentationTypeConstruction(LConstruction);
    SetLength(LValues, Length(LSelection));
    SetLength(LWitnesses, Length(LSelection));
    try
      for LAt := 0 to High(LSelection) do
      begin
        LConstruction.Spend;
        CheckNyxDocumentationTypeConstruction(LConstruction);
        LWitnesses[LAt] := TSignatureWitness.Create;
        LValues[LAt] := CaptureRoutineSignature(LSource, LSelection[LAt],
          LConstruction, LWindows, LLexical, LWitnesses[LAt], ADeclared);
      end;
      { A later getter may change an earlier selected row. Reobserve the exact
        bounded receipt for every sibling before publishing the result vector. }
      for LAt := 0 to High(LWitnesses) do
      begin
        LConstruction.Spend;
        CheckNyxDocumentationTypeConstruction(LConstruction);

        if not SameDeclaration(LSource.Declaration(LWitnesses[LAt].Ordinal), LWitnesses[LAt].Routine) then
          raise ENyxDocumentationSignature.Create('Earlier batch routine changed before publication');
        LConstruction.Spend;
        CheckNyxDocumentationTypeConstruction(LConstruction);

        if not SameScope(LSource.Scope(LWitnesses[LAt].Routine.NestedScope), LWitnesses[LAt].Scope) then
          raise ENyxDocumentationSignature.Create('Earlier batch scope changed before publication');
        for LParameter := 0 to High(LWitnesses[LAt].Parameters) do
        begin
          LConstruction.Spend;
          CheckNyxDocumentationTypeConstruction(LConstruction);

          if not SameDeclaration(LSource.Declaration(LWitnesses[LAt].Parameters[LParameter].Ordinal),
            LWitnesses[LAt].Parameters[LParameter].Declaration) then
            raise ENyxDocumentationSignature.Create('Earlier batch parameter changed before publication');
        end;
      end;
      LConstruction.Spend;
      CheckNyxDocumentationTypeConstruction(LConstruction);

      if LSource.DeclarationCount <> LDeclarations then
        raise ENyxDocumentationSignature.Create('Batch declaration count changed');
      LConstruction.Spend;
      CheckNyxDocumentationTypeConstruction(LConstruction);

      if LSource.ScopeCount <> LScopes then
        raise ENyxDocumentationSignature.Create('Batch scope count changed');
      LConstruction.Spend;
      CheckNyxDocumentationTypeConstruction(LConstruction);

      if LSource.Lexical <> LLexical then
        raise ENyxDocumentationSignature.Create('Batch lexical owner changed');
      LConstruction.Spend;
      CheckNyxDocumentationTypeConstruction(LConstruction);

      if not SameLexicalPolicy(LLexical.Options, LPolicy) then
        raise ENyxDocumentationSignature.Create('Batch lexical policy changed');
      LConstruction.Spend;
      CheckNyxDocumentationTypeConstruction(LConstruction);

      if LLexical.Scalars <> LScalars then
        raise ENyxDocumentationSignature.Create('Batch scalar extent changed');
      LConstruction.Spend;
      CheckNyxDocumentationTypeConstruction(LConstruction);
      LFinalText := LLexical.Source;
      CheckNyxDocumentationTypeConstruction(LConstruction);
      LConstruction.Spend(Length(LText) + 1);
      CheckNyxDocumentationTypeConstruction(LConstruction);

      if LText <> LFinalText then
        raise ENyxDocumentationSignature.Create('Complete batch source changed before publication');
      LConstruction.Spend(Length(LSelection) + 2);
      CheckNyxDocumentationTypeConstruction(LConstruction);
      { No outward call follows this final input agreement. Even admission at
        the last publication boundary cannot swap or duplicate caller ordinals. }
      for LAt := 0 to High(LSelection) do
      begin

        if ARoutines[LAt] <> LSelection[LAt] then
          raise ENyxDocumentationSignature.Create('Caller selection changed before batch publication');
      end;
      Result := LValues;
    finally
      for LAt := 0 to High(LWitnesses) do
        LWitnesses[LAt].Free;
    end;
  finally
    LWindows.Retire;
  end;
end;

function CaptureNyxDocumentationRoutineSignatures(
  const ASource: INyxPascalSyntaxSnapshot; const ARoutines: array of Integer;
  const AConstruction: INyxDocumentationTypeConstruction): TNyxDocumentationRoutineSignatures;
begin
  Result := CaptureRoutineSignatures(ASource, ARoutines, AConstruction, False);
end;

function CaptureNyxDocumentationDeclaredRoutineSignature(
  const ASource: INyxPascalSyntaxSnapshot; ARoutine: Integer;
  const AConstruction: INyxDocumentationTypeConstruction): INyxDocumentationRichSignature;
var
  LSource: INyxPascalSyntaxSnapshot;
  LScope: INyxDocumentationTypeConstruction;
begin
  LSource := ASource;
  LScope := AConstruction;
  Result := CaptureRoutineSignature(LSource, ARoutine, LScope, nil, nil, nil, True);
end;

function CaptureNyxDocumentationDeclaredRoutineSignatures(
  const ASource: INyxPascalSyntaxSnapshot; const ARoutines: array of Integer;
  const AConstruction: INyxDocumentationTypeConstruction): TNyxDocumentationRoutineSignatures;
begin
  Result := CaptureRoutineSignatures(ASource, ARoutines, AConstruction, True);
end;

end.
