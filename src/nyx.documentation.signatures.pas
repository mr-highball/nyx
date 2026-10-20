{ Nyx source-owned documentation signatures. MIT license, see LICENSE. }
unit nyx.documentation.signatures;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.text, nyx.pascal.syntax, nyx.documentation,
  nyx.documentation.typeexpressions;

type
  { A selected declaration cannot be captured faithfully. This is a metadata
    refusal, not a compiler diagnostic or permission to execute a source unit. }
  ENyxDocumentationSignature = class(ENyxDocumentation);

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

implementation

uses nyx.pascal.lexing, nyx.bytes;

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

  if (APascal = '') or (Length(APascal) > 1024) or
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

function CaptureNyxDocumentationRoutineSignature(
  const ASource: INyxPascalSyntaxSnapshot; ARoutine: Integer;
  const AConstruction: INyxDocumentationTypeConstruction): INyxDocumentationRichSignature;
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
    LRoot, LParameterCount, LObserved, LBoundary: Integer;
begin
  Result := nil;
  CheckNyxDocumentationTypeConstruction(AConstruction);
  AConstruction.Spend;

  if ASource = nil then
    raise ENyxDocumentationSignature.Create('A source syntax snapshot is required');
  AConstruction.Spend;
  LDeclarationCount := ASource.DeclarationCount;

  if (ARoutine < 0) or (LDeclarationCount < 1) or (ARoutine >= LDeclarationCount) then
    raise ENyxDocumentationSignature.Create('Selected routine ordinal is outside this snapshot');
  AConstruction.Spend;
  LRoutine := ASource.Declaration(ARoutine);
  NeedDeclaration(LRoutine);

  if (LRoutine.Kind <> psdkRoutine) or
    not (LRoutine.RoutineKind in [prkProcedure, prkFunction]) or
    (LRoutine.GenericSpan.Length <> 0) then
    raise ENyxDocumentationSignature.Create('Select a representable non-generic procedure/function header');
  AConstruction.Spend;
  LScopeCount := ASource.ScopeCount;

  if (LRoutine.Scope < 0) or (LRoutine.Scope >= LScopeCount) or
    (LRoutine.NestedScope < 0) or (LRoutine.NestedScope >= LScopeCount) then
    raise ENyxDocumentationSignature.Create('Selected routine has no valid parameter scope');
  AConstruction.Spend;
  LScope := ASource.Scope(LRoutine.NestedScope);

  if (LScope.Kind <> psskRoutine) or (LScope.OwnerDeclaration <> ARoutine) or
    (LScope.Parent <> LRoutine.Scope) or not LScope.Complete or LScope.UnsupportedSyntax or
    (LScope.DeclarationCount < 0) or (Length(LScope.Name) > 256) then
    raise ENyxDocumentationSignature.Create('Selected routine does not own a complete parameter scope');
  AConstruction.Spend;
  LLexical := ASource.Lexical;

  if LLexical = nil then
    raise ENyxDocumentationSignature.Create('Selected syntax has no lexical source');
  AConstruction.Spend;
  LScalars := LLexical.Scalars;
  NeedSpan(LRoutine.HeaderSpan, 0, LScalars, True);
  NeedSpan(LRoutine.NameSpan, LRoutine.HeaderSpan.Start, LRoutine.HeaderSpan.Length, True);
  NeedSpan(LRoutine.TypeSpan, LRoutine.HeaderSpan.Start, LRoutine.HeaderSpan.Length,
    LRoutine.RoutineKind = prkFunction);

  if (LRoutine.RoutineKind = prkProcedure) and (LRoutine.TypeSpan.Length <> 0) then
    raise ENyxDocumentationSignature.Create('A procedure cannot claim a function result');
  AConstruction.Spend;
  LSupplierPolicy := LLexical.Options;
  LSupplierPolicy.Validate;
  LPolicy := AConstruction.Limits.Lexical;

  if (LSupplierPolicy.AllowNestedComments <> LPolicy.AllowNestedComments) or
    (LSupplierPolicy.AllowUnicodeIdentifiers <> LPolicy.AllowUnicodeIdentifiers) then
    raise ENyxDocumentationSignature.Create('Selected header needs a different lexical policy');
  AConstruction.Spend;
  LText := LLexical.Source;
  LHeader := CopyNyxDocumentationTypeTextWindow(LText, LRoutine.HeaderSpan, AConstruction);
  LText := '';
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
    AConstruction.Spend;

    if LParsed.Lexical.Token(LAt).Kind in [ptkDirective, ptkAssembly] then
      raise ENyxDocumentationSignature.Create('Selected header contains context-dependent source');
  end;
  AConstruction.Spend;
  LModule := LParsed.Scope(0);

  if not LModule.Complete or LModule.UnsupportedSyntax or
    (LModule.DeclarationCount <> 1) then
    raise ENyxDocumentationSignature.Create('Selected window must contain exactly one declared routine');
  LRoot := LModule.FirstDeclaration;
  AConstruction.Spend;
  LOwnedRoutine := LParsed.Declaration(LRoot);
  NeedDeclaration(LOwnedRoutine);

  if (LOwnedRoutine.Kind <> psdkRoutine) or (LOwnedRoutine.Scope <> 0) or
    (LOwnedRoutine.RoutineKind <> LRoutine.RoutineKind) or
    (LOwnedRoutine.Name <> LRoutine.Name) or (LOwnedRoutine.OwnerName <> LRoutine.OwnerName) or
    (LOwnedRoutine.GenericSpan.Length <> 0) or
    not ShiftedSpan(LOwnedRoutine.NameSpan, LRoutine.NameSpan, LRoutine.HeaderSpan.Start) or
    not ShiftedSpan(LOwnedRoutine.TypeSpan, LRoutine.TypeSpan, LRoutine.HeaderSpan.Start) then
    raise ENyxDocumentationSignature.Create('Supplied declaration disagrees with the owned header');
  AConstruction.Spend;
  LOwnedScope := LParsed.Scope(LOwnedRoutine.NestedScope);
  LParameterCount := LOwnedScope.DeclarationCount;

  if not LOwnedScope.Complete or LOwnedScope.UnsupportedSyntax or
    (LOwnedScope.Kind <> psskRoutine) or (LOwnedScope.OwnerDeclaration <> LRoot) or
    (LParameterCount < 0) or (LParameterCount > NyxDocumentationMaximumParameters) or
    (LScope.DeclarationCount < LParameterCount) then
    raise ENyxDocumentationSignature.Create('Selected signature exceeds its complete parameter allowance');
  AConstruction.Spend(LParameterCount + 1);
  SetLength(LParameters, LParameterCount);
  SetLength(LWitnesses, LParameterCount + 1);
  LNext := LOwnedScope.FirstDeclaration;
  LOriginalAt := LScope.FirstDeclaration;
  LObserved := 0;
  for LAt := 0 to LParameterCount - 1 do
  begin
    AConstruction.Spend;

    if (LOriginalAt < 0) or (LOriginalAt >= LDeclarationCount) then
      raise ENyxDocumentationSignature.Create('Supplied parameter prefix ends before its owned header');
    LOriginal := ASource.Declaration(LOriginalAt);
    NeedDeclaration(LOriginal);
    AConstruction.Spend;
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
    NeedSpan(LParameter.TypeSpan, 0, LParsed.Lexical.Scalars, True);
    NeedSpan(LParameter.ExpressionSpan, 0, LParsed.Lexical.Scalars,
      psdfDefaultArgument in LParameter.Flags);

    if not (psdfDefaultArgument in LParameter.Flags) and
      (LParameter.ExpressionSpan.Length <> 0) then
      raise ENyxDocumentationSignature.Create('Absent parameter default retains unexpected text');
    LWitnesses[LObserved].Ordinal := LOriginalAt;
    LWitnesses[LObserved].Declaration := LOriginal;
    Inc(LObserved);
    LBuilder := TParsedParameter.Create;
    LParameters[LAt] := LBuilder;
    LBuilder.ParameterName := LParameter.Name;
    LBuilder.Mode := PassingMode(LParameter.ParameterMode);
    LBuilder.DefaultPresent := psdfDefaultArgument in LParameter.Flags;
    LTypeText := CopyNyxDocumentationTypeTextWindow(LHeader, LParameter.TypeSpan, AConstruction);
    LExpression := NewNyxDocumentationTypeExpression(LTypeText, ndtoParameter, AConstruction);
    LBuilder.ParameterType := NewNyxDocumentationExpressionTypeUse(LExpression, AConstruction);

    if LBuilder.DefaultPresent then
      LBuilder.DefaultText := CopyNyxDocumentationTypeTextWindow(LHeader,
        LParameter.ExpressionSpan, AConstruction);
    LNext := LParameter.Next;
    LOriginalAt := LOriginal.Next;
  end;

  if LNext <> -1 then
    raise ENyxDocumentationSignature.Create('Owned parameter list retains an unexpected declaration');
  { Implementation scopes may have ordinary locals after the parameter prefix.
    They are not copied as signature facts. One boundary read catches extra
    parameters without traversing arbitrary local declarations or bodies. }
  LBoundary := LOriginalAt;

  if LBoundary <> -1 then
  begin
    AConstruction.Spend;

    if (LBoundary < 0) or (LBoundary >= LDeclarationCount) then
      raise ENyxDocumentationSignature.Create('Supplied parameter boundary is outside its snapshot');
    LOriginal := ASource.Declaration(LBoundary);

    if (LOriginal.Kind = psdkParameter) or (LOriginal.Scope <> LRoutine.NestedScope) or
      (LOriginal.NameSpan.Start < LRoutine.HeaderSpan.Finish) then
      raise ENyxDocumentationSignature.Create('Supplied signature has an inconsistent parameter boundary');
    LWitnesses[LObserved].Ordinal := LBoundary;
    LWitnesses[LObserved].Declaration := LOriginal;
    Inc(LObserved);
  end
  else if LScope.DeclarationCount <> LParameterCount then
    raise ENyxDocumentationSignature.Create('Supplied routine scope count disagrees with its parameter boundary');
  LResultUse := nil;

  if LOwnedRoutine.RoutineKind = prkFunction then
  begin
    NeedSpan(LOwnedRoutine.TypeSpan, 0, LParsed.Lexical.Scalars, True);
    LTypeText := CopyNyxDocumentationTypeTextWindow(LHeader, LOwnedRoutine.TypeSpan, AConstruction);
    LExpression := NewNyxDocumentationTypeExpression(LTypeText, ndtoResult, AConstruction);
    LResultUse := NewNyxDocumentationExpressionTypeUse(LExpression, AConstruction);
  end;
  LCandidate := NewNyxDocumentationRichSignature(LParameters, LResultUse, AConstruction);
  { Reobserve only the bounded context used above. Equality of supplier
    interfaces or claimed receipts never substitutes for these observations. }
  AConstruction.Spend;

  if (ASource.DeclarationCount <> LDeclarationCount) or
    (ASource.ScopeCount <> LScopeCount) or (ASource.Lexical <> LLexical) or
    (LLexical.Scalars <> LScalars) or not SameLexicalPolicy(LLexical.Options, LSupplierPolicy) then
    raise ENyxDocumentationSignature.Create('Source envelope changed before signature publication');
  AConstruction.Spend;

  if not SameDeclaration(ASource.Declaration(ARoutine), LRoutine) or
    not SameScope(ASource.Scope(LRoutine.NestedScope), LScope) then
    raise ENyxDocumentationSignature.Create('Selected routine changed before signature publication');
  for LAt := 0 to LObserved - 1 do
  begin
    AConstruction.Spend;

    if not SameDeclaration(ASource.Declaration(LWitnesses[LAt].Ordinal), LWitnesses[LAt].Declaration) then
      raise ENyxDocumentationSignature.Create('Selected parameter changed before signature publication');
  end;
  AConstruction.Spend;
  LText := LLexical.Source;
  LAgain := CopyNyxDocumentationTypeTextWindow(LText, LRoutine.HeaderSpan, AConstruction);
  AConstruction.Spend(Length(LHeader) + 1);

  if LAgain <> LHeader then
    raise ENyxDocumentationSignature.Create('Selected header text changed before signature publication');
  AConstruction.Spend;
  CheckNyxDocumentationTypeConstruction(AConstruction);
  Result := LCandidate;
end;

end.
