{ Source-derived registration integration. MIT license, see LICENSE. }
program nyx_documentation_source_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.types, nyx.documentation, nyx.documentation.typeexpressions,
  nyx.documentation.signatures, nyx.documentation.registration,
  nyx.documentation.source, nyx.documentation.declarations,
  nyx.pascal.work,
  nyx.pascal.syntax, nyx.pascal.context,
  nyx.scheduler, nyx.pascal.files, nyx.pascal.lexing, nyx.pascal.translation,
  nyx.pascal.types, nyx.pascal.syntax.translated, nyx.pascal.binding
  {$ifndef PAS2JS}, Classes, LazFileUtils, nyx.bytes, nyx.pascal.encoding,
  nyx.pascal.files.native, nyx.pascal.modules, nyx.pascal.compilertrace,
  nyx.pascal.compilerplan, nyx.pascal.requirements, nyx.pascal.fulfillment,
  nyx.pascal.witness, nyx.pascal.conditions, nyx.pascal.names,
  nyx.studio.pascalprofiles,
  nyx.controls.documentation, nyx.documentation.registry, nyx.catalog,
  nyx.schema, nyx.callbacks, nyx.test.documentation{$endif};

type
  { A complete ordinary interface delegate deliberately exposes the same owned
    facts. Forwarding getters never turns this separate object into the sealed
    world owner accepted for nominal/source-adapter inspection. }
  TWorldDelegate = class(TInterfacedObject, INyxPascalSemanticWorld)
  public
    Value: INyxPascalSemanticWorld;
    GetterReads: Integer;
    function Options: TNyxPascalBindingOptions;
    function Count: Integer;
    function ModuleAt(AIndex: Integer): INyxPascalTranslatedSyntax;
    function FindModule(const AName: TNyxText): Integer;
    function InterfaceBoundary(AModule: Integer): Integer;
    function WithModule(const AView: INyxPascalTranslatedSyntax): INyxPascalSemanticWorld;
    function At(AModule, AScope, ACaret: Integer): INyxPascalBindingSite;
    function Symbol(AModule, ADeclaration: Integer): INyxPascalBoundSymbol;
    function Lookup(const ASite: INyxPascalBindingSite; const AName: TNyxPascalBindingName;
      const ACancellation: INyxCancellationScope): INyxPascalResolution;
    function TypeOfSymbol(const ASymbol: INyxPascalBoundSymbol;
      const ACancellation: INyxCancellationScope): INyxPascalResolution;
    function Member(const AType: INyxPascalSemanticType; const ASite: INyxPascalBindingSite;
      const AName: TNyxPascalBindingName; AAccess: TNyxPascalMemberAccess;
      const ACancellation: INyxCancellationScope): INyxPascalResolution;
  end;
  { A qualification-only outward policy mutates shared caller inputs at one
    measured final admission, or cancels before subsequent source traversal.
    It owns no source world and never supplies declaration authority. }
  TSourceAdmission = class(TInterfacedObject, INyxDocumentationTypeAdmission)
  public
    Used, Trigger: Integer;
    Action: Integer;
    procedure AdmitTypeWork(APhase: TNyxPascalAnalysisPhase; AUnits: Integer);
    function RemainingTypeBytes: Integer;
  end;
  { The projection delegate shares all immutable source observations, while
    retaining its own ordinary interface identity. Owner admission must refuse
    it without executing a claimed source or granting physical provenance. }
  TViewDelegate = class(TInterfacedObject, INyxPascalTranslatedSyntax)
  public
    Value: INyxPascalTranslatedSyntax;
    function Source: INyxPascalTranslation;
    function Syntax: INyxPascalSyntaxSnapshot;
    function CompleteTranslation: Boolean;
    function BlockedAt: Integer;
    function OriginCount: Integer;
    function Origin(AIndex: Integer): TNyxPascalSyntaxOrigin;
    function OriginAt(AVirtualOffset: Integer): Integer;
    function VirtualAt(AOccurrence, APhysicalOffset: Integer): Integer;
    function Part(AIndex: Integer; const ASpan: TNyxPascalSpan): TNyxPascalSyntaxOrigin;
    function TypeOfDeclaration(AIndex: Integer; const AOptions: TNyxPascalTypeOptions;
      const ACancellation: INyxCancellationScope): INyxPascalTypeSyntax;
  end;

var
  GChecks: Integer;
  GSourceScopes: TNyxDocumentationSourceScopes;
  GSourceScope: INyxDocumentationTypeConstruction;
  {$ifndef PAS2JS}
  GVerifiedInputs, GProfileRuntime, GWindowsRuntime: TNyxText;
  GRequiredCompiler, GRequiredOutput, GSymbolReplay, GPreparedReplay: TNyxText;
  GPrimitiveReplay, GFinalProfile, GImportInputs: TNyxText;
  {$endif}

function TWorldDelegate.Options: TNyxPascalBindingOptions;
begin Result := Value.Options; end;
function TWorldDelegate.Count: Integer;
begin Inc(GetterReads); Result := Value.Count; end;
function TWorldDelegate.ModuleAt(AIndex: Integer): INyxPascalTranslatedSyntax;
begin Inc(GetterReads); Result := Value.ModuleAt(AIndex); end;
function TWorldDelegate.FindModule(const AName: TNyxText): Integer;
begin Result := Value.FindModule(AName); end;
function TWorldDelegate.InterfaceBoundary(AModule: Integer): Integer;
begin Result := Value.InterfaceBoundary(AModule); end;
function TWorldDelegate.WithModule(const AView: INyxPascalTranslatedSyntax): INyxPascalSemanticWorld;
begin Result := Value.WithModule(AView); end;
function TWorldDelegate.At(AModule, AScope, ACaret: Integer): INyxPascalBindingSite;
begin Result := Value.At(AModule, AScope, ACaret); end;
function TWorldDelegate.Symbol(AModule, ADeclaration: Integer): INyxPascalBoundSymbol;
begin Result := Value.Symbol(AModule, ADeclaration); end;
function TWorldDelegate.Lookup(const ASite: INyxPascalBindingSite; const AName: TNyxPascalBindingName;
  const ACancellation: INyxCancellationScope): INyxPascalResolution;
begin Result := Value.Lookup(ASite, AName, ACancellation); end;
function TWorldDelegate.TypeOfSymbol(const ASymbol: INyxPascalBoundSymbol;
  const ACancellation: INyxCancellationScope): INyxPascalResolution;
begin Result := Value.TypeOfSymbol(ASymbol, ACancellation); end;
function TWorldDelegate.Member(const AType: INyxPascalSemanticType; const ASite: INyxPascalBindingSite;
  const AName: TNyxPascalBindingName; AAccess: TNyxPascalMemberAccess;
  const ACancellation: INyxCancellationScope): INyxPascalResolution;
begin Result := Value.Member(AType, ASite, AName, AAccess, ACancellation); end;

function TViewDelegate.Source: INyxPascalTranslation;
begin Result := Value.Source; end;
function TViewDelegate.Syntax: INyxPascalSyntaxSnapshot;
begin Result := Value.Syntax; end;
function TViewDelegate.CompleteTranslation: Boolean;
begin Result := Value.CompleteTranslation; end;
function TViewDelegate.BlockedAt: Integer;
begin Result := Value.BlockedAt; end;
function TViewDelegate.OriginCount: Integer;
begin Result := Value.OriginCount; end;
function TViewDelegate.Origin(AIndex: Integer): TNyxPascalSyntaxOrigin;
begin Result := Value.Origin(AIndex); end;
function TViewDelegate.OriginAt(AVirtualOffset: Integer): Integer;
begin Result := Value.OriginAt(AVirtualOffset); end;
function TViewDelegate.VirtualAt(AOccurrence, APhysicalOffset: Integer): Integer;
begin Result := Value.VirtualAt(AOccurrence, APhysicalOffset); end;
function TViewDelegate.Part(AIndex: Integer; const ASpan: TNyxPascalSpan): TNyxPascalSyntaxOrigin;
begin Result := Value.Part(AIndex, ASpan); end;
function TViewDelegate.TypeOfDeclaration(AIndex: Integer; const AOptions: TNyxPascalTypeOptions;
  const ACancellation: INyxCancellationScope): INyxPascalTypeSyntax;
begin Result := Value.TypeOfDeclaration(AIndex, AOptions, ACancellation); end;

procedure TSourceAdmission.AdmitTypeWork(APhase: TNyxPascalAnalysisPhase; AUnits: Integer);
begin

  if (Ord(APhase) < Ord(Low(TNyxPascalAnalysisPhase))) or
    (Ord(APhase) > Ord(High(TNyxPascalAnalysisPhase))) or (AUnits < 1) then
    raise Exception.Create('The source fixture received an invalid work phase');
  Inc(Used, AUnits);

  if (Trigger > 0) and (Used >= Trigger) then
  begin
    Trigger := 0;
    case Action of
      1: GSourceScopes[0].DeclarationIndex := 123456;
      2: GSourceScope.Cancel;
      3: GSourceScope := nil;
      else begin end;
    end;
  end;
end;

function TSourceAdmission.RemainingTypeBytes: Integer;
begin
  Result := 4 * 1024 * 1024;
end;

procedure Check(AValue: Boolean; const AReason: String);
begin
  Inc(GChecks);

  if not AValue then
    raise Exception.Create(AReason);
end;

procedure PortableSourceCapture;
const
  CSource: TNyxText = 'unit SourceCapture; interface ' +
    'type TMarker = record end; procedure Ping; implementation procedure Ping; begin end; end.';
var
  LWorld: INyxPascalSemanticWorld;
  LFiles: INyxPascalFileCatalog;
  LContext: INyxPascalCompilerContext;
  LView: INyxPascalTranslatedSyntax;
  LScopes: TNyxDocumentationSourceScopes;
  LCreators: TNyxDocumentationSourceCreators;
  LScope: INyxDocumentationTypeConstruction;
  LPackage, LPrevious: INyxDocumentationPackage;
  LUnit, LRecord: INyxDocumentationType;
  LFailure: Boolean;
  LPolicy: TSourceAdmission;
  LAdmission: INyxDocumentationTypeAdmission;
  LWork: Integer;
  LAlternate: TWorldDelegate;
  LAlternateLease: INyxPascalSemanticWorld;
  LMissingWorld: INyxPascalSemanticWorld;
begin
  LCreators := nil;
  LContext := NewNyxPascalCompilerContext(ppcFPC,
    TNyxPascalCompilerVersion.Create(3, 3, 1), ppmDelphi);
  LFiles := NewNyxPascalFileCatalog(pfcExact).WithFile(
    NyxPascalPhysicalFile('SourceCapture.pas'), CSource);
  LView := NewNyxPascalTranslatedSyntax(NewNyxPascalTranslation(LFiles,
    NyxPascalPhysicalFile('SourceCapture.pas'), LContext,
    TNyxPascalTranslationOptions.Defaults), TNyxPascalSyntaxOptions.Defaults);
  LWorld := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults).WithModule(LView);
  SetLength(LScopes, 2);
  LScopes[0].ModuleIndex := 0;
  LScopes[0].DeclarationIndex := -1;
  LScopes[1].ModuleIndex := 0;
  LScopes[1].DeclarationIndex := LView.Syntax.Named(0, 'TMarker');
  LScope := NewNyxDocumentationTypeConstruction(TNyxDocumentationTypeExpressionLimits.Defaults);
  try
    LPackage := CaptureNyxDocumentationSourcePackage(LWorld, LScopes, LCreators,
      [], LScope);
  finally
    LScope.Retire;
  end;
  LUnit := LPackage.FindDeclaration(NyxDocumentationUnit('SourceCapture'));
  LRecord := LPackage.FindDeclaration(NyxDocumentationType('SourceCapture', 'TMarker'));
  Check((LPackage.DeclarationCount = 2) and (LPackage.ChunkCount = 1) and
    (LUnit <> nil) and (LRecord <> nil) and (LUnit.MemberCount = 1) and
    (LUnit.MemberAt(0).Kind = ndmRoutine) and (LRecord.Kind = ndtRecord),
    'The actual owned translation of a small portable mechanics input captures complete globals and one selected type without a help inventory');
  LPrevious := LPackage;
  SetLength(LCreators, 1);
  LCreators[0].ModuleIndex := 0;
  LCreators[0].DeclarationIndex := LView.Syntax.Named(0, 'Ping');
  LScope := NewNyxDocumentationTypeConstruction(TNyxDocumentationTypeExpressionLimits.Defaults);
  try
    LPackage := CaptureNyxDocumentationSourcePackage(LWorld, LScopes, LCreators,
      [], LScope);
  finally
    LScope.Retire;
  end;
  LUnit := LPackage.FindDeclaration(NyxDocumentationUnit('SourceCapture'));
  Check((LUnit.MemberAt(0).Kind = ndmFactory) and
    (LPrevious.FindDeclaration(LUnit.Identity).MemberAt(0).Kind = ndmRoutine),
    'Only explicit source-owner creator annotation changes role; prior published metadata remains immutable');
  LScopes[1] := LScopes[0];
  LFailure := False;
  LScope := NewNyxDocumentationTypeConstruction(TNyxDocumentationTypeExpressionLimits.Defaults);
  try
    try
      CaptureNyxDocumentationSourcePackage(LWorld, LScopes, LCreators, [], LScope);
    except
      on ENyxDocumentationSource do LFailure := True;
    end;
  finally
    LScope.Retire;
  end;
  Check(LFailure and (LPrevious.DeclarationCount = 2),
    'A duplicate complete unit selection refuses instead of publishing a partial replacement');
  LFailure := False;
  LScope := NewNyxDocumentationTypeConstruction(TNyxDocumentationTypeExpressionLimits.Defaults.Work(1));
  try
    try
      CaptureNyxDocumentationSourcePackage(LWorld, LScopes, LCreators, [], LScope);
    except
      on ENyxDocumentationTypeExpression do LFailure := True;
    end;
  finally
    LScope.Retire;
  end;
  Check(LFailure and (LPrevious.DeclarationCount = 2),
    'The complete source operation uses one caller work allowance and leaves retained success untouched on refusal');
  LScopes[1].DeclarationIndex := LView.Syntax.Named(0, 'TMarker');
  LPolicy := TSourceAdmission.Create;
  LAdmission := LPolicy;
  LScope := NewNyxDocumentationTypeConstruction(
    TNyxDocumentationTypeExpressionLimits.Defaults, nil, LAdmission);
  try
    LPackage := CaptureNyxDocumentationSourcePackage(LWorld, LScopes, LCreators, [], LScope);
    LWork := LPolicy.Used;
  finally
    LScope.Retire;
  end;
  Check(LWork > 1, 'The final input-mutation witness has actual shared work admissions');
  GSourceScopes := Copy(LScopes);
  LPolicy := TSourceAdmission.Create;
  LAdmission := LPolicy;
  LPolicy.Trigger := LWork;
  LPolicy.Action := 1;
  LFailure := False;
  LScope := NewNyxDocumentationTypeConstruction(
    TNyxDocumentationTypeExpressionLimits.Defaults, nil, LAdmission);
  try
    try
      CaptureNyxDocumentationSourcePackage(LWorld, GSourceScopes, LCreators, [], LScope);
    except
      on ENyxDocumentationSource do LFailure := True;
    end;
  finally
    LScope.Retire;
  end;
  Check(LFailure and (GSourceScopes[0].DeclarationIndex = 123456) and
    (LPrevious.DeclarationCount = 2),
    'Shared caller selection mutation at the last outward admission refuses before sole package publication');
  GSourceScopes := Copy(LScopes);
  LPolicy := TSourceAdmission.Create;
  LAdmission := LPolicy;
  LPolicy.Trigger := 1;
  LPolicy.Action := 2;
  GSourceScope := NewNyxDocumentationTypeConstruction(
    TNyxDocumentationTypeExpressionLimits.Defaults, nil, LAdmission);
  LFailure := False;
  try
    try
      CaptureNyxDocumentationSourcePackage(LWorld, GSourceScopes, LCreators, [], GSourceScope);
    except
      on ENyxDocumentationTypeExpressionCancelled do LFailure := True;
      on ENyxDocumentationCancelled do LFailure := True;
    end;
  finally
    GSourceScope.Retire;
    GSourceScope := nil;
  end;
  Check(LFailure and (LPrevious.DeclarationCount = 2),
    'Outward admission cancellation stops source capture without publishing a provisional package');
  LPolicy := TSourceAdmission.Create;
  LAdmission := LPolicy;
  LPolicy.Trigger := 1;
  LPolicy.Action := 3;
  GSourceScope := NewNyxDocumentationTypeConstruction(
    TNyxDocumentationTypeExpressionLimits.Defaults, nil, LAdmission);
  LPackage := CaptureNyxDocumentationSourcePackage(LWorld, LScopes, LCreators, [], GSourceScope);
  Check((GSourceScope = nil) and (LPackage.DeclarationCount = 2),
    'Local construction leases survive clearing the caller last interface during outward admission');
  LAlternate := TWorldDelegate.Create;
  LAlternateLease := LAlternate;
  LAlternate.Value := LWorld;
  LScope := NewNyxDocumentationTypeConstruction(TNyxDocumentationTypeExpressionLimits.Defaults);
  LFailure := False;
  try
    try
      CaptureNyxDocumentationSourcePackage(LAlternateLease, LScopes, LCreators, [], LScope);
    except
      on ENyxDocumentationSource do LFailure := True;
    end;
  finally
    LScope.Retire;
  end;
  Check(LFailure and (LAlternate.GetterReads = 0),
    'An alternate world cannot borrow sealed authority; refusal precedes its module/count getters');
  LFiles := NewNyxPascalFileCatalog(pfcExact).WithFile(
    NyxPascalPhysicalFile('MissingNamespace.pas'),
    'unit MissingNamespace; interface uses AbsentNamespace; ' +
    'type TMarker = record end; implementation end.');
  LView := NewNyxPascalTranslatedSyntax(NewNyxPascalTranslation(LFiles,
    NyxPascalPhysicalFile('MissingNamespace.pas'), LContext,
    TNyxPascalTranslationOptions.Defaults), TNyxPascalSyntaxOptions.Defaults);
  LMissingWorld := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults).WithModule(LView);
  SetLength(LScopes, 1);
  LScopes[0].ModuleIndex := 0;
  LScopes[0].DeclarationIndex := LView.Syntax.Named(0, 'TMarker');
  LCreators := nil;
  LScope := NewNyxDocumentationTypeConstruction(TNyxDocumentationTypeExpressionLimits.Defaults);
  LFailure := False;
  try
    try
      CaptureNyxDocumentationSourcePackage(LMissingWorld, LScopes, LCreators, [], LScope);
    except
      on ENyxDocumentationSource do LFailure := True;
    end;
  finally
    LScope.Retire;
  end;
  Check(LFailure and (LPrevious.DeclarationCount = 2),
    'Even a selected empty type cannot hide its actual missing public imported namespace');
  GSourceScopes := nil;
end;

procedure CheckedOwnerBoundary;
var
  LWorld, LAlternateWorld: INyxPascalSemanticWorld;
  LView, LAlternateView: INyxPascalTranslatedSyntax;
  LWorldDelegate: TWorldDelegate;
  LViewDelegate: TViewDelegate;
  LFiles: INyxPascalFileCatalog;
  LContext: INyxPascalCompilerContext;
begin
  LWorld := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults);
  LWorldDelegate := TWorldDelegate.Create;
  LAlternateWorld := LWorldDelegate;
  LWorldDelegate.Value := LWorld;
  Check(NyxPascalOwnedSemanticWorld(LWorld) and not NyxPascalOwnedSemanticWorld(nil) and
    not NyxPascalOwnedSemanticWorld(LAlternateWorld),
    'Exact world ownership refuses a complete delegating interface with identical public facts');
  LFiles := NewNyxPascalFileCatalog(pfcExact).WithFile(NyxPascalPhysicalFile('Owned.pas'),
    'unit Owned; interface type TItem = record Value: Integer; end; implementation end.');
  LContext := NewNyxPascalCompilerContext(ppcFPC,
    TNyxPascalCompilerVersion.Create(3, 3, 1), ppmDelphi);
  LView := NewNyxPascalTranslatedSyntax(NewNyxPascalTranslation(LFiles,
    NyxPascalPhysicalFile('Owned.pas'), LContext, TNyxPascalTranslationOptions.Defaults),
    TNyxPascalSyntaxOptions.Defaults);
  LViewDelegate := TViewDelegate.Create;
  LAlternateView := LViewDelegate;
  LViewDelegate.Value := LView;
  Check(NyxPascalOwnedTranslatedSyntax(LView) and not NyxPascalOwnedTranslatedSyntax(nil) and
    not NyxPascalOwnedTranslatedSyntax(LAlternateView),
    'Exact projection ownership refuses an alternate sharing every genuine source observation');
end;

procedure CallableIdentities;
var
  LOwner, LOperator, LOrdinary: TNyxDocumentationRef;
  LMember: INyxDocumentationMember;
  LScope: INyxDocumentationTypeConstruction;
  LSyntax: INyxPascalSyntaxSnapshot;
  LSignature: INyxDocumentationRichSignature;
  LFailed: Boolean;
begin
  LOwner := NyxDocumentationType('Actual.Source', 'TValue');
  LOperator := NyxDocumentationOperator(LOwner, pokAdd, '+', 'Two.Values');
  LOrdinary := NyxDocumentationMember(LOwner, 'Add', 'Two.Values');
  Check(LOperator.IsOperator and (LOperator.OperatorKind = pokAdd) and
    (LOperator.OperatorToken = '+') and
    (LOperator.OperatorOverloadKey = 'Two.Values') and
    not LOperator.Same(LOrdinary),
    'Authored operator identity preserves family/token/selector without colliding with an ordinary member');
  Check(NyxDocumentationOperator(LOwner, pokAdd, 'Add').OperatorOverloadKey = '',
    'A named operator retains explicit absent overload selection');
  LFailed := False;
  try
    NyxDocumentationMember(LOwner, 'operator +', 'NyxOperator.K2');
  except
    on ENyxDocumentation do LFailed := True;
  end;
  Check(LFailed, 'The unchanged ordinary member factory still refuses symbolic/keyword designators');
  LScope := NewNyxDocumentationTypeConstruction(TNyxDocumentationTypeExpressionLimits.Defaults);
  LSyntax := NewNyxPascalSyntaxSnapshot('destructor Destroy;',
    TNyxPascalSyntaxOptions.Defaults.Fragment(psfInterfaceDeclarations));
  LSignature := CaptureNyxDocumentationDeclaredRoutineSignature(LSyntax, 0, LScope);
  LMember := NewNyxDocumentationRichMember(NyxDocumentationMember(LOwner, 'Destroy'),
    ndmDestructor, '', LSignature, LScope);
  Check((LSignature.ParameterCount = 0) and (LSignature.ResultUse = nil) and
    (LMember.Kind = ndmDestructor), 'A destructor is a declared lifecycle operation with no fabricated result');
  LFailed := False;
  try
    CaptureNyxDocumentationRoutineSignature(LSyntax, 0, LScope);
  except
    on ENyxDocumentationSignature do LFailed := True;
  end;
  Check(LFailed, 'The old strict procedure/function extraction surface retains its lifecycle refusal');
  LScope.Retire;
end;

procedure StorageShapes;
var
  LScope: INyxDocumentationTypeConstruction;
  LMember, LCopy: INyxDocumentationMember;
  LStorage: INyxDocumentationStorageDeclaration;
  LValue: INyxDocumentationMemberValue;
  LUse: INyxDocumentationTypeUse;
  LUnit: TNyxDocumentationRef;
  LType: INyxDocumentationType;
  LFirst, LSecond: TNyxText;
  LBytes: Integer;
  LRefused: Boolean;
begin
  LScope := NewNyxDocumentationTypeConstruction(TNyxDocumentationTypeExpressionLimits.Defaults);
  try
    LUnit := NyxDocumentationUnit('source.mechanics');
    LMember := NewNyxDocumentationStorageMember(NyxDocumentationMember(LUnit, 'Greeting'),
      ndmConstant, '', nil, True, '''Hello''', LScope);
    LCopy := CaptureNyxDocumentationMember(LMember.WithExample(
      NyxDocumentationExample('Read the declaration', 'Caption := Greeting;')), LScope);
    Check((LCopy.Kind = ndmConstant) and not LCopy.ValueType.Defined and
      Supports(LCopy, INyxDocumentationStorageDeclaration, LStorage) and
      not LStorage.HasDeclaredType and LStorage.HasInitializer and
      (LStorage.InitializerPascal = '''Hello''') and (LMember.ExampleCount = 0),
      'A constant retains known unwritten type and exact inert initializer through specialized copies');
    LType := NewNyxDocumentationType(LUnit, ndtUnit, '').WithMember(LMember);
    LBytes := 0;
    CaptureNyxDocumentationDeclaration(LType, LUnit, LFirst, LBytes, LScope);
    LMember := NewNyxDocumentationStorageMember(NyxDocumentationMember(LUnit, 'Greeting'),
      ndmConstant, '', nil, True, '''Bonjour''', LScope);
    LType := NewNyxDocumentationType(LUnit, ndtUnit, '').WithMember(LMember);
    LBytes := 0;
    CaptureNyxDocumentationDeclaration(LType, LUnit, LSecond, LBytes, LScope);
    Check(LFirst <> LSecond, 'Initializer-only source changes remain part of exact immutable declaration equality');
    LUse := NewNyxDocumentationExpressionTypeUse(NewNyxDocumentationTypeExpression(
      'Integer', ndtoVariable, LScope), LScope);
    LMember := NewNyxDocumentationStorageMember(NyxDocumentationMember(LUnit, 'Invocations'),
      ndmVariable, '', LUse, False, '', LScope);
    LCopy := CaptureNyxDocumentationMember(LMember, LScope);
    Check(Supports(LCopy, INyxDocumentationStorageDeclaration, LStorage) and
      LStorage.HasDeclaredType and not LStorage.HasInitializer and
      Supports(LCopy, INyxDocumentationMemberValue, LValue) and
      (LValue.DeclaredValueType.Expression.Occurrence = ndtoVariable),
      'A variable retains its authored type occurrence separately from missing initializer syntax');
    LRefused := False;
    try
      LCopy.OfType(Default(TNyxDocumentationRef));
    except
      on ENyxDocumentation do LRefused := True;
    end;
    Check(LRefused, 'A variable copy cannot erase its required written type');
    LMember := NewNyxDocumentationStorageMember(NyxDocumentationMember(LUnit, 'Caption'),
      ndmResourceString, '', nil, True, '''Welcome''', LScope);
    Check((LMember.Kind = ndmResourceString) and Supports(LMember,
      INyxDocumentationStorageDeclaration, LStorage) and not LStorage.HasDeclaredType,
      'A resource string remains a declaration family without a fabricated authored String annotation');
    LRefused := False;
    try
      NewNyxDocumentationStorageMember(NyxDocumentationMember(LUnit, 'Wrong'),
        ndmConstant, '', nil, False, '', LScope);
    except
      on ENyxDocumentation do LRefused := True;
    end;
    Check(LRefused, 'A complete ordinary constant cannot silently omit its initializer');
    LRefused := False;
    try
      NewNyxDocumentationStorageMember(NyxDocumentationMember(LUnit, 'Wrong'),
        ndmVariable, '', nil, False, '', LScope);
    except
      on ENyxDocumentation do LRefused := True;
    end;
    Check(LRefused, 'Unknown variable type cannot masquerade as known authored type absence');
  finally
    LScope.Retire;
  end;
  Check(LStorage.InitializerPascal = '''Welcome''',
    'Published initializer syntax retains no source/construction lifetime');
end;

procedure UntypedAndFieldShapes;
var
  LScope: INyxDocumentationTypeConstruction;
  LSyntax: INyxPascalSyntaxSnapshot;
  LSignature: INyxDocumentationRichSignature;
  LParameter: INyxDocumentationParameterDeclaration;
  LUntyped: INyxDocumentationUntypedParameter;
  LUse: INyxDocumentationTypeUse;
  LField, LCopy: INyxDocumentationMember;
  LValue: INyxDocumentationMemberValue;
  LType: INyxDocumentationType;
  LDeclared: INyxDocumentationDeclaredTypeExpression;
  LOwner: TNyxDocumentationRef;
  LMode: TNyxDocumentationParameterMode;
  LFailed: Boolean;
  LParameters: TNyxDocumentationParameterDeclarations;
begin
  LScope := NewNyxDocumentationTypeConstruction(TNyxDocumentationTypeExpressionLimits.Defaults);
  try
    LOwner := NyxDocumentationType('Actual.Source', 'TValue');
    LSyntax := NewNyxPascalSyntaxSnapshot('function QueryInterface(out obj): LongInt;',
      TNyxPascalSyntaxOptions.Defaults.Fragment(psfInterfaceDeclarations));
    LSignature := CaptureNyxDocumentationDeclaredRoutineSignature(LSyntax, 0, LScope);
    LParameter := LSignature.ParameterAt(0);
    Check(Supports(LParameter, INyxDocumentationUntypedParameter, LUntyped) and
      LUntyped.DeclaredUntyped and (LParameter.TypeUse = nil) and
      (LParameter.Passing = ndpOut) and not LParameter.HasDefault,
      'An actual untyped out header retains explicit unknown-type absence without a fabricated type');
    LField := NewNyxDocumentationRichMember(NyxDocumentationMember(LOwner, 'QueryInterface'),
      ndmMethod, '', LSignature, LScope);
    Check(LField.Signature = nil,
      'An untyped parameter cannot be projected into the unchanged strict named signature');
    LFailed := False;
    try
      CaptureNyxDocumentationRoutineSignature(LSyntax, 0, LScope);
    except
      on ENyxDocumentationSignature do LFailed := True;
    end;
    Check(LFailed, 'The legacy source-signature entry still refuses untyped parameters');
    for LMode := Low(TNyxDocumentationParameterMode) to High(TNyxDocumentationParameterMode) do
    begin
      LFailed := False;
      try
        LParameter := NewNyxDocumentationUntypedParameterDeclaration('Data', LMode, LScope);
      except
        on ENyxDocumentation do LFailed := True;
      end;
      Check(LFailed = not (LMode in [ndpConst, ndpVar, ndpOut]),
        'Only declared const/var/out modes admit an explicitly untyped parameter ' + IntToStr(Ord(LMode)));
    end;
    LParameter := NewNyxDocumentationUntypedParameterDeclaration('Data', ndpConst, LScope);
    LFailed := False;
    try
      LParameter.WithDefault('0');
    except
      on ENyxDocumentation do LFailed := True;
    end;
    Check(LFailed, 'Untyped fluent copies cannot acquire a default');
    SetLength(LParameters, 1);
    LParameters[0] := LParameter;
    LFailed := False;
    try
      NewNyxDocumentationRichProperty(NyxDocumentationMember(LOwner, 'Items'), '',
        LParameters, nil, [ndpaRead], False, LScope);
    except
      on ENyxDocumentation do LFailed := True;
    end;
    Check(LFailed, 'A callable untyped parameter cannot masquerade as a property index');
    LUse := NewNyxDocumentationExpressionTypeUse(NewNyxDocumentationTypeExpression(
      'array[0..7] of Byte', ndtoField, LScope), LScope);
    LField := NewNyxDocumentationValueMember(NyxDocumentationMember(LOwner, 'D4'), '', LUse, LScope);
    LCopy := CaptureNyxDocumentationMember(LField.WithExample(
      NyxDocumentationExample('Field', 'Value.D4[0] := 1;')), LScope);
    Check(Supports(LCopy, INyxDocumentationMemberValue, LValue) and
      (LValue.DeclaredValueType.Expression.Occurrence = ndtoField) and
      (LValue.DeclaredValueType.Expression.Text = 'array[0..7] of Byte') and
      not LCopy.ValueType.Defined and (LField.ExampleCount = 0),
      'Compound field types retain exact field syntax through capture and immutable fluent copies');
    LCopy := LCopy.OfType(NyxDocumentationType('Actual.Source', 'TReplacement'));
    Check(Supports(LCopy, INyxDocumentationMemberValue, LValue) and
      (LValue.DeclaredValueType.Expression = nil) and LCopy.ValueType.Same(
        NyxDocumentationType('Actual.Source', 'TReplacement')),
      'Explicit named OfType replacement clears the prior compound field edges');
    LFailed := False;
    try
      NewNyxDocumentationRichProperty(NyxDocumentationMember(LOwner, 'Wrong'), '',
        nil, LUse, [ndpaRead], False, LScope);
    except
      on ENyxDocumentation do LFailed := True;
    end;
    Check(LFailed, 'Field occurrence syntax cannot be reused as an authored property value');
    LUse := NewNyxDocumentationExpressionTypeUse(NewNyxDocumentationTypeExpression(
      '^TValue', ndtoDeclaration, LScope), LScope);
    LType := NewNyxDocumentationDeclaredType(NyxDocumentationType('Actual.Source', 'PValue'), '', LUse, LScope);
    Check(Supports(LType, INyxDocumentationDeclaredTypeExpression, LDeclared) and
      (LDeclared.Family = ptxPointer) and not LType.AliasTarget.Defined,
      'A declared pointer retains pointer syntax rather than an invented alias');
    LUse := NewNyxDocumentationExpressionTypeUse(NewNyxDocumentationTypeExpression(
      'class of TValue', ndtoDeclaration, LScope), LScope);
    LType := NewNyxDocumentationDeclaredType(NyxDocumentationType('Actual.Source', 'TValueClass'), '', LUse, LScope);
    Check(Supports(LType, INyxDocumentationDeclaredTypeExpression, LDeclared) and
      (LDeclared.Family = ptxClassOf), 'An authored class-of declaration retains its actual family');
  finally
    LScope.Retire;
  end;
  Check((LDeclared.Definition.Expression.Text = 'class of TValue') and
    (LParameter.TypeUse = nil) and LUntyped.DeclaredUntyped,
    'Published untyped and compound values retain no construction lifetime');
end;

{$ifndef PAS2JS}
type
  { A bounded read result over explicit manifest names. It preserves the same
    native header owner's associations without discovering directories or
    retaining a file handle, mutable manifest or installed SDK source. }
  TRetainedDirectory = class(TInterfacedObject, INyxPascalDirectoryCapture)
  public
    FileValues: INyxPascalFileCatalog;
    ModuleValues: INyxPascalModuleCatalog;
    UnindexedValues: array of TNyxPascalPhysicalFile;
    function Files: INyxPascalFileCatalog;
    function Modules: INyxPascalModuleCatalog;
    function UnindexedCount: Integer;
    function Unindexed(AIndex: Integer): TNyxPascalPhysicalFile;
  end;
  { One explicitly captured import source enters the ordinary atomic packet
    overlay. This temporary producer owns no installed file or compiler and
    cannot discover or replace another retained source. }
  TImportPacket = class(TInterfacedObject, INyxPascalFilePackets)
  public
    Value: INyxPascalFilePacket;
    function Count: Integer;
    function Packet(AIndex: Integer): INyxPascalFilePacket;
  end;
  { Immutable output leases keep genuine replayed RTL inputs separate from the
    prospective local compilation-input overlay used for Nyx source. This owner
    contains no compiler process, open source handle or filesystem fallback. }
  TRetainedRTL = class
  public
    Files: INyxPascalFileCatalog;
    Modules: INyxPascalModuleCatalog;
    World: INyxPascalSemanticWorld;
    Bootstrap: INyxFPCCompilerCapture;
    LocalContext: INyxPascalCompilerContext;
    FactJobs: Integer;
  end;
  { Qualification owns explicit repository capture and already retained SDK
    inputs. It loads actual public imports into the ordinary immutable world;
    no missing module/include is replaced by an empty namespace. }
  TProductSourceWorld = class
  private
    FDepth: Integer;
    FLoading, FLoaded: array of TNyxText;
  public
    Files: INyxPascalFileCatalog;
    Modules: INyxPascalModuleCatalog;
    World: INyxPascalSemanticWorld;
    LocalContext: INyxPascalCompilerContext;
    procedure Load(const AName: TNyxText);
  end;

function TRetainedDirectory.Files: INyxPascalFileCatalog;
begin Result := FileValues; end;
function TRetainedDirectory.Modules: INyxPascalModuleCatalog;
begin Result := ModuleValues; end;
function TRetainedDirectory.UnindexedCount: Integer;
begin Result := Length(UnindexedValues); end;
function TRetainedDirectory.Unindexed(AIndex: Integer): TNyxPascalPhysicalFile;
begin

  if (AIndex < 0) or (AIndex >= Length(UnindexedValues)) then
    raise Exception.Create('Retained unindexed source ordinal is outside the explicit manifest');
  Result := UnindexedValues[AIndex];
end;

function TImportPacket.Count: Integer;
begin Result := 1; end;

function TImportPacket.Packet(AIndex: Integer): INyxPascalFilePacket;
begin

  if (AIndex <> 0) or (Value = nil) then
    raise Exception.Create('The explicit import overlay owns exactly one complete source packet');
  Result := Value;
end;

function ReadBytes(const APath: TNyxText): TNyxBytes;
var
  LStream: TFileStream;
begin
  Result := nil;
  LStream := TFileStream.Create(String(APath), fmOpenRead or fmShareDenyWrite);
  try

    if (LStream.Size < 0) or (LStream.Size > 4 * 1024 * 1024) then
      raise Exception.Create('A retained source/transcript exceeds its unchanged four-MiB input bound');
    SetLength(Result, LStream.Size);

    if Length(Result) > 0 then
      LStream.ReadBuffer(Result[0], Length(Result));
  finally
    LStream.Free;
  end;
end;

function ReadText(const APath: TNyxText): TNyxText;
begin
  Result := NyxDecodeUTF8(ReadBytes(APath));
end;

function ReadProbe(const ADirectory: TNyxText;
  const AProbe: INyxPascalCompilerProbe;
  const ACancellation: INyxCancellationScope = nil): INyxFPCCompilerCapture;
var
  LFile: TNyxText;
  LCode: Integer;
begin
  LFile := 'nyxcompilercontext.pas';
  LCode := 0;

  if NyxFPCProbeScope(AProbe) = pcsoSystemBootstrap then
  begin
    LFile := 'system.pp';
    LCode := 1;
  end;
  Check(ReadText(ADirectory + '/' + LFile) = AProbe.Source,
    'Every retained compiler input must equal the complete current typed request before transcript replay');
  Result := ReadNyxFPCCompilerCapture(AProbe, LFile,
    ReadText(ADirectory + '/compiler-profile.log'), LCode,
    TNyxPascalCompilerTraceOptions.Defaults, ACancellation);
end;

function ReadManifestInputs(const ADirectory, AManifest: TNyxText;
  AExpectedCount: Integer): INyxPascalDirectoryCapture;
var
  LOwner: TRetainedDirectory;
  LLease: INyxPascalDirectoryCapture;
  LNames: TNyxStrings;
  LName, LHeader, LExtension: TNyxText;
  LFile: TNyxPascalPhysicalFile;
  LBytes: INyxPascalSourceBytes;
  LView: INyxPascalSourceView;
  LByteFiles: INyxPascalByteFileCatalog;
  LAt, LUnindexed: Integer;
begin
  Result := nil;
  LNames := TNyxStrings.Create;
  try
    LNames.Text := ReadText(AManifest);

    if (AExpectedCount < 1) or (AExpectedCount > 4096) or (LNames.Count <> AExpectedCount) then
      raise Exception.Create('The retained source manifest must match its complete prescribed count');
    LOwner := TRetainedDirectory.Create;
    LLease := LOwner;
    LOwner.FileValues := NewNyxPascalFileCatalog(pfcExact);
    LOwner.ModuleValues := NewNyxPascalModuleCatalog;
    for LAt := 0 to LNames.Count - 1 do
    begin
      LName := LNames[LAt];
      LFile := NyxPascalPhysicalFile(LName);

      if (LFile.Name <> LName) or (LOwner.FileValues.Find(LFile) >= 0) then
        raise Exception.Create('Only distinct exact normalized manifest paths may be read');
      LBytes := NewNyxPascalSourceBytes(ReadBytes(ADirectory + '/' + LName));
      LView := nil;
      try
        LView := DecodeNyxPascalSource(LBytes, NyxPascalUTF8Codec);
      except
        on ENyxPascalSourceDecodeError do LView := nil;
      end;

      if not Supports(LOwner.FileValues, INyxPascalByteFileCatalog, LByteFiles) then
        raise Exception.Create('Retained source catalog lost its exact byte owner');
      LOwner.FileValues := LByteFiles.WithByteSource(LFile, LBytes, LView);
      LExtension := TNyxText(LowerCase(ExtractFileExt(String(LName))));

      if (LView = nil) or not ((LExtension = '.pp') or (LExtension = '.pas')) then
        Continue;
      LHeader := NyxPascalCapturedModuleHeader(LView.Text, TNyxPascalDirectoryOptions.Defaults);

      if LHeader = '' then
      begin
        LUnindexed := Length(LOwner.UnindexedValues);
        SetLength(LOwner.UnindexedValues, LUnindexed + 1);
        LOwner.UnindexedValues[LUnindexed] := LFile;
      end
      else
        LOwner.ModuleValues := LOwner.ModuleValues.WithModule(
          NewNyxPascalModuleSource(NyxPascalModule(LHeader), LFile));
    end;
    Check(LOwner.FileValues.Count = AExpectedCount,
      'Every prescribed retained physical source is read once in the original manifest order');
    Result := LLease;
  finally
    LNames.Free;
  end;
end;

function ReadRetainedInputs(const ADirectory: TNyxText): INyxPascalDirectoryCapture;
begin
  Result := ReadManifestInputs(ADirectory, GVerifiedInputs + '/original156-paths.txt', 156);
end;

function MissingFacts(const AContext: INyxPascalCompilerContext;
  const AProbe: INyxPascalCompilerProbe): Boolean;
var
  LAt: Integer;
  LSizes: INyxPascalSizeProbe;
begin
  for LAt := 0 to AProbe.Count - 1 do
  begin

    if AContext.Symbol(AProbe.Symbol(LAt)).Kind = ppdUnknown then
      Exit(True);
  end;

  if Supports(AProbe, INyxPascalSizeProbe, LSizes) then
  begin
    for LAt := 0 to LSizes.SizeCount - 1 do
    begin

      if NyxPascalCapturedSize(AContext, LSizes.SizeName(LAt)).State = pssUnknown then
        Exit(True);
    end;
  end;
  Result := False;
end;

function ReplayPreparation(const AFiles: INyxPascalFileCatalog;
  const AModule: INyxPascalModuleSource; const ABaseProbe: INyxPascalCompilerProbe;
  const ADirectory: TNyxText): INyxPascalModuleSource;
var
  LContext, LInitial: INyxPascalCompilerContext;
  LTranslation: INyxPascalTranslation;
  LIncludes: INyxPascalCompilerIncludeCandidates;
  LPlan: INyxPascalCompilerPlan;
  LCandidates: INyxPascalCompilerCandidates;
  LProbe: INyxPascalCompilerProbe;
  LFresh, LAccumulated: INyxFPCCompilerCapture;
  LOptions: TNyxPascalTranslationOptions;
  LPlanOptions: TNyxPascalCompilerPlanOptions;
  LRemaining, LJobs: Integer;
begin
  LContext := AModule.Context;
  LInitial := LContext;
  LRemaining := 8000000;
  LJobs := 0;
  LAccumulated := nil;
  LPlanOptions := TNyxPascalCompilerPlanOptions.Defaults.LookAheadConditions(True).UpToTokens(2000000);
  LIncludes := PlanNyxPascalCompilerIncludeCandidates(AFiles, AModule.Root,
    AModule.Translation.LexicalOptions, LPlanOptions);
  Dec(LRemaining, LIncludes.WorkUnits);
  while True do
  begin

    if LRemaining < 1 then
      raise Exception.Create('Retained module replay exhausted the original aggregate preparation allowance');
    LOptions := AModule.Translation;

    if LOptions.MaximumTokens > LRemaining then
      LOptions := LOptions.UpToTokens(LRemaining);
    LTranslation := NewNyxPascalTranslation(AFiles, AModule.Root, LContext, LOptions);
    Dec(LRemaining, LTranslation.Count);
    LPlanOptions := TNyxPascalCompilerPlanOptions.Defaults.LookAheadConditions(True).UpToTokens(2000000);

    if (LPlanOptions.MaximumTokens >= LTranslation.Count) and
      (LPlanOptions.MaximumTokens - LTranslation.Count > LRemaining) then
      LPlanOptions := LPlanOptions.UpToTokens(LTranslation.Count + LRemaining);
    LPlan := PlanNyxPascalCompilerRequirementsWithIncludes(LTranslation, LPlanOptions, LIncludes);

    if Supports(LPlan, INyxPascalCompilerCandidates, LCandidates) then
      Dec(LRemaining, LCandidates.WorkUnits - LTranslation.Count);
    LProbe := LPlan.CompilerProbe(ABaseProbe);

    if not MissingFacts(LContext, LProbe) then
      Break;

    if LJobs >= 8 then
      raise Exception.Create('Retained module replay exhausted the original eight-job correspondence bound');
    Inc(LJobs);
    LFresh := ReadProbe(ADirectory + '/probe-' + TNyxText(IntToStr(LJobs)), LProbe);

    if LAccumulated = nil then
      LAccumulated := LFresh
    else
      LAccumulated := LAccumulated.Merge(LFresh);
    LContext := NyxPascalWithBootstrapContext(LAccumulated.Context, NyxPascalBootstrapContext(LInitial));
  end;
  Check(((NyxPascalASCIIKey(AModule.Name.Name) = 'system') and (LJobs = 3)) or
    ((NyxPascalASCIIKey(AModule.Name.Name) = 'objpas') and (LJobs = 1)),
    'Original preparation uses exactly the retained3System/1ObjPas typed request sequence');
  Result := AModule.InContext(LContext, AModule.Translation);
end;

function ReplayRequirement(const ADirectory: TNyxText;
  const ARequirement: INyxPascalCompilerRequirement;
  const AOwner: INyxFPCCompilerCapture): INyxFPCCompilerCapture;
var
  LProbe: INyxPascalCompilerProbe;
  LFresh: INyxFPCCompilerCapture;
  LCaptured: INyxPascalDirectoryCapture;
  LRetained: INyxPascalFileCatalog;
  LWitness: INyxPascalSourceWitness;
begin

  if ARequirement.Kind = pcrSourceSize then
  begin
    LCaptured := ReadRetainedInputs(ADirectory + '/inputs');
    LRetained := LCaptured.Files.WithIncludeDirectory('rtl/i386').WithIncludeDirectory('rtl/win')
      .WithIncludeDirectory('rtl/inc').WithIncludeDirectory('rtl/win32');
    LWitness := RestoreNyxPascalSourceWitness(ARequirement, AOwner, LRetained);
    Result := LWitness.Read(ReadText(ADirectory + '/compiler-source.log'), 1,
      TNyxPascalCompilerTraceOptions.Defaults);
    Exit;
  end;

  if NyxPascalCompilerScope(ARequirement.Context) <> pcsoSystemBootstrap then
    raise Exception.Create('The accepted original RTL sequence requests primitive facts from its compiler-root owner');
  LProbe := NewNyxFPCSystemBootstrapProbe(NyxPascalDefine('NYX_REQUIRED_FACT'));
  LProbe := NyxFPCProbeSizeOf(LProbe, ARequirement.Reference);
  LFresh := ReadProbe(ADirectory, LProbe);
  Result := MergeNyxFPCSizeCapture(AOwner, LFresh, ARequirement.Reference);
end;

{ The receipt reporter is shared with the later prepared-context continuation;
  the original namespace journey retains the same source and assertions. }
procedure ReportNamespaceSemantic(const AStage, AName: TNyxText;
  const AValue: INyxPascalSemanticTranslation;
  const AView: INyxPascalTranslatedSyntax); forward;

procedure ReplayRTL(const ADirectory: TNyxText; AResult: TRetainedRTL);
var
  LCaptured: INyxPascalDirectoryCapture;
  LProbe: INyxPascalCompilerProbe;
  LContext: INyxPascalCompilerContext;
  LModule: INyxPascalModuleSource;
  LSource: INyxPascalSemanticTranslation;
  LView: INyxPascalTranslatedSyntax;
  LRequirements: INyxPascalCompilerRequirements;
  LRequirement: INyxPascalCompilerRequirement;
  LCaptures, LReplies: INyxPascalCompilerCaptures;
  LPlan: INyxPascalCompilerFulfillmentPlan;
  LApplied: INyxPascalCompilerFulfillment;
  LReply: INyxFPCCompilerCapture;
  LName: TNyxText;
  LMode: TNyxPascalLanguageMode;
  LUnit, LJobs, LAt, LOwner: Integer;
begin
  LCaptured := ReadRetainedInputs(ADirectory + '/original-inputs');
  Check(LCaptured.Modules.Count = 47,
    'The exact original156 inputs retain all47 actual header-derived module root associations');
  AResult.Files := LCaptured.Files.WithIncludeDirectory('rtl/i386').WithIncludeDirectory('rtl/win')
    .WithIncludeDirectory('rtl/inc').WithIncludeDirectory('rtl/win32');
  AResult.Modules := LCaptured.Modules;
  AResult.World := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults.WithImplicitScopes(pispCompilerUnits));
  LProbe := NewNyxFPCSystemBootstrapProbe(NyxPascalDefine('ACTUAL_SYSTEM_ROOT'));
  LProbe := NyxFPCProbeSizeOf(LProbe, NyxPascalSizeReference('AnsiChar'));
  LProbe := NyxFPCProbeSizeOf(LProbe, NyxPascalSizeReference('WideChar'));
  LProbe := NyxFPCProbeSizeOf(LProbe, NyxPascalSizeReference('Char'));
  LProbe := NyxFPCProbeSizeOf(LProbe, NyxPascalSizeReference('Comp'));
  LProbe := NyxFPCProbeSizeOf(LProbe, NyxPascalSizeReference('Byte'));
  AResult.Bootstrap := ReadProbe(ADirectory + '/compiler-root', LProbe);
  for LUnit := 0 to 1 do
  begin
    LName := 'System';
    LMode := ppmFPC;

    if LUnit = 1 then
    begin
      LName := 'ObjPas';
      LMode := ppmObjFPC;
    end;
    LProbe := NewNyxFPCCompilerProbe(NyxPascalDefine('ACTUAL_' + LName), LMode);
    LProbe := NyxFPCProbeSizeOf(LProbe, NyxPascalSizeReference('System.Char'));
    LContext := ReadProbe(ADirectory + '/base-' + LName, LProbe).Context;
    LContext := NyxPascalWithBootstrapContext(LContext, AResult.Bootstrap.Context);
    LModule := AResult.Modules.Entry(AResult.Modules.Find(NyxPascalModule(LName))).InContext(LContext,
      TNyxPascalTranslationOptions.Defaults.UpToTokens(2000000)
        .Lexical(TNyxPascalLexicalOptions.Defaults.NestedComments(True)));
    LModule := ReplayPreparation(AResult.Files, LModule, LProbe, ADirectory + '/prepared-' + LName);
    AResult.Modules := AResult.Modules.WithModule(LModule);
    LSource := NewNyxPascalSemanticTranslation(AResult.Files, LModule.Root, LModule.Context,
      AResult.World, LModule.Translation, TNyxPascalSyntaxOptions.Defaults.UpToDeclarations(100000),
      TNyxPascalConditionalOptions.Defaults.UpToTokens(4000000));

    if LUnit = 0 then
    begin
      LRequirements := CaptureNyxPascalRequirements(LSource);
      Check(LSource.Pending and (LRequirements.RequirementCount = 1),
        'Whole retained System first publishes one typed missing compiler-root fact');
      LRequirement := LRequirements.RequirementAt(0);
      Check(LRequirement.Reference.Name = 'word', 'The first exact retained System requirement is Word');
      LReply := ReplayRequirement(ADirectory + '/required-fact', LRequirement, AResult.Bootstrap);
      LModule := ResolveNyxPascalModuleRequirement(AResult.Files, LModule, LRequirement,
        AResult.Bootstrap, LReply);
      AResult.Bootstrap := LReply;
      AResult.Modules := AResult.Modules.WithModule(LModule);
      LCaptures := NewNyxPascalCompilerCaptures.WithCapture(AResult.Bootstrap);
      LJobs := 0;
      while True do
      begin
        LSource := NewNyxPascalSemanticTranslation(AResult.Files, LModule.Root, LModule.Context,
          AResult.World, LModule.Translation, TNyxPascalSyntaxOptions.Defaults.UpToDeclarations(100000),
          TNyxPascalConditionalOptions.Defaults.UpToTokens(4000000));

        if not LSource.Pending then
          Break;

        if LJobs >= 8 then
          raise Exception.Create('Retained semantic replay exhausted the original eight-fact bound');
        LPlan := PlanNyxPascalCompilerFulfillment(AResult.Files, AResult.Modules, LSource, LCaptures, 8 - LJobs);
        Check(LPlan.Count = 1, 'Each retained semantic stage corresponds to exactly one complete current request');
        LReplies := NewNyxPascalCompilerCaptures;
        for LAt := 0 to LPlan.Count - 1 do
          LReplies := LReplies.WithCapture(ReplayRequirement(ADirectory + '/semantic-facts-' +
            TNyxText(IntToStr(LJobs + 1)) + '/fact-' + TNyxText(IntToStr(LAt + 1)),
            LPlan.RequirementAt(LAt), LPlan.OwnerAt(LAt)));
        LApplied := LPlan.ApplyReplies(LReplies);
        Inc(LJobs, LApplied.Jobs);
        LCaptures := LApplied.Captures;
        AResult.Modules := LApplied.Modules;
        LModule := AResult.Modules.Entry(AResult.Modules.Find(NyxPascalModule(LName)));
        LOwner := LCaptures.Find(NyxPascalBootstrapContext(LModule.Context));

        if LOwner < 0 then
          raise Exception.Create('The exact updated compiler-root owner was not retained');
        AResult.Bootstrap := LCaptures.CaptureAt(LOwner);
      end;
      Check(LJobs = 6, 'All6 actual dynamic primitive/source-layout replies replay in their original typed order');
      AResult.FactJobs := LJobs;
    end;
    LView := NewNyxPascalTranslatedSyntax(LSource.Source,
      TNyxPascalSyntaxOptions.Defaults.UpToDeclarations(100000));
    Check(not LSource.Pending and (LView.Syntax.DiagnosticCount = 0),
      'The complete genuine RTL owner resolves without skipped conditions or manufactured declarations');
    Check(((LUnit = 0) and (LView.Syntax.DeclarationCount = 13101) and (LSource.Answers = 31)) or
      ((LUnit = 1) and (LView.Syntax.DeclarationCount = 209) and (LSource.Answers = 1)),
      'Retained original System/ObjPas agree on their complete declaration and semantic-answer counts');
    ReportNamespaceSemantic('original-final', LName, LSource, LView);
    AResult.World := AResult.World.WithModule(LView);
  end;
end;

function ProfileProbe(const AIdentity: TNyxText): INyxPascalCompilerProbe;
begin
  Result := NewNyxFPCCompilerProbe(NyxPascalDefine(AIdentity), ppmDelphi)
    .WithSymbol(NyxPascalDefine('PAS2JS'))
    .WithSymbol(NyxPascalDefine('FPC_DOTTEDUNITS'))
    .WithSymbol(NyxPascalDefine('UNICODERTL'))
    .WithSymbol(NyxPascalDefine('WIN32'))
    .WithSymbol(NyxPascalDefine('CPUI386'));
  Result := NyxFPCProbeSizeOf(Result, NyxPascalSizeReference('Integer'));
  Result := NyxFPCProbeSizeOf(Result, NyxPascalSizeReference('System.Integer'));
  Result := NyxFPCProbeSizeOf(Result, NyxPascalSizeReference('Pointer'));
  Result := NyxFPCProbeSizeOf(Result, NyxPascalSizeReference('NyxMissingSizeName'));
end;

{ The retained Windows47 request was prepared from these exact28 files in
  capture order. Existing conditional planning supplies the symbol operands;
  this replay path never invents a complete define set or invokes a compiler. }
function WindowsProbe(const AFiles: INyxPascalFileCatalog): INyxPascalCompilerProbe;
var
  LFile, LAt, LNameEnd, LQuery: Integer;
  LLexical: INyxPascalLexicalSnapshot;
  LBody, LName: TNyxText;
  LNeeds: INyxPascalConditionNeeds;
  LContext: INyxPascalCompilerContext;
begin
  Result := NewNyxFPCCompilerProbe(NyxPascalDefine('WINDOWS_SDK'), ppmObjFPC);
  LContext := NewNyxPascalCompilerContext(ppcUnknown,
    TNyxPascalCompilerVersion.Create(0, 0, 0), ppmUnknown);
  for LFile := 0 to AFiles.Count - 1 do
  begin
    LLexical := NewNyxPascalLexicalSnapshot(AFiles.TextAt(LFile), TNyxPascalLexicalOptions.Defaults);
    for LAt := 0 to LLexical.Count - 1 do
    begin

      if LLexical.Token(LAt).Kind <> ptkDirective then
        Continue;
      LBody := LLexical.Slice(LLexical.Token(LAt).Span);

      if Copy(LBody, 1, 2) = '{$' then
        LBody := Copy(LBody, 3, Length(LBody) - 3)
      else
        LBody := Copy(LBody, 4, Length(LBody) - 5);
      LBody := TNyxText(Trim(LBody));
      LNameEnd := 1;
      while (LNameEnd <= Length(LBody)) and not (LBody[LNameEnd] in [#9, #10, #13, ' ']) do
        Inc(LNameEnd);
      LName := NyxPascalASCIIKey(Copy(LBody, 1, LNameEnd - 1));
      LBody := TNyxText(Trim(Copy(LBody, LNameEnd, Length(LBody))));

      if (LName = 'ifdef') or (LName = 'ifndef') then
      begin
        LNameEnd := 1;
        while (LNameEnd <= Length(LBody)) and not (LBody[LNameEnd] in [#9, #10, #13, ' ']) do
          Inc(LNameEnd);
        LBody := 'defined(' + Copy(LBody, 1, LNameEnd - 1) + ')';
      end
      else if (LName <> 'if') and (LName <> 'elseif') then
        Continue;
      LNeeds := PlanNyxPascalCondition(LContext, LBody);
      for LQuery := 0 to LNeeds.SymbolCount - 1 do
        Result := Result.WithSymbol(LNeeds.Symbol(LQuery));
    end;
  end;
end;

{ Report the retained typed frontier before the unchanged completeness check.
  Planning reads captured condition inputs only; it neither guesses a compiler
  request nor fulfills one. Planning refusal is reported without replacing the
  original completeness assertion. }
procedure ReportSemanticFrontier(const AValue: INyxPascalSemanticTranslation;
  const AView: INyxPascalTranslatedSyntax;
  const AExistingPlan: INyxPascalCompilerPlan = nil;
  AAllowFreshPlan: Boolean = True);
var
  LSource: INyxPascalTranslation;
  LOrigin: TNyxPascalConditionOrigin;
  LRequirements: INyxPascalCompilerRequirements;
  LRequirement: INyxPascalCompilerRequirement;
  LSourceSize: INyxPascalSourceSizeRequirement;
  LPlan: INyxPascalCompilerPlan;
  LNeeds: INyxPascalConditionNeeds;
  LDeclarations: INyxPascalConditionDeclarations;
  LDiagnostic: TNyxPascalTranslationDiagnostic;
  LSyntaxOrigin: TNyxPascalSyntaxOrigin;
  LAt, LNeed, LOriginIndex: Integer;
begin
  LSource := AValue.Source;
  WriteLn('semantic frontier / pending=', AValue.Pending,
    ' issue=', Ord(AValue.BindingIssue), ' related=', AValue.RelatedName,
    ' work=', AValue.WorkTokens, ' passes=', AValue.Passes,
    ' answers=', AValue.Answers, ' resolved=', LSource.Resolved,
    ' complete=', AView.CompleteTranslation, ' blocked=', AView.BlockedAt);

  if AValue.Pending then
  begin
    LOrigin := AValue.Origin;
    WriteLn('semantic origin / file=', LOrigin.FileReference.Name,
      ' occurrence=', LOrigin.Occurrence, ' start=', LOrigin.Span.Start,
      ' length=', LOrigin.Span.Length, ' prefix=', LOrigin.PrefixCount);
  end;
  Flush(Output);
  LRequirements := CaptureNyxPascalRequirements(AValue);
  WriteLn('semantic compiler requirements / count=', LRequirements.RequirementCount);
  for LAt := 0 to LRequirements.RequirementCount - 1 do
  begin
    LRequirement := LRequirements.RequirementAt(LAt);
    WriteLn('semantic compiler requirement / ordinal=', LAt,
      ' kind=', Ord(LRequirement.Kind), ' reference=', LRequirement.Reference.Name,
      ' contextState=', LRequirement.ContextState,
      ' sourceOffset=', LRequirement.SourceOffset, ' originIndex=', LRequirement.OriginIndex,
      ' scope=', Ord(NyxPascalCompilerScope(LRequirement.Context)),
      ' family=', Ord(LRequirement.Context.Family),
      ' version=', LRequirement.Context.Version.FullNumber,
      ' mode=', Ord(LRequirement.Context.Mode),
      ' owner=', LRequirement.OwnerSource.Source.Files.FileAt(
        LRequirement.OwnerSource.Source.Occurrence(0).FileIndex).Name,
      ' observer=', LRequirement.Source.Source.Files.FileAt(
        LRequirement.Source.Source.Occurrence(0).FileIndex).Name);

    if Supports(LRequirement, INyxPascalSourceSizeRequirement, LSourceSize) then
      WriteLn('semantic source-size requirement / key=', LSourceSize.StorageKey.Value,
        ' file=', LSourceSize.Origin.FileReference.Name,
        ' occurrence=', LSourceSize.Origin.Occurrence,
        ' start=', LSourceSize.Origin.Span.Start,
        ' length=', LSourceSize.Origin.Span.Length,
        ' prefix=', LSourceSize.Origin.PrefixCount);
  end;
  WriteLn('semantic source diagnostics / count=', LSource.DiagnosticCount);
  for LAt := 0 to LSource.DiagnosticCount - 1 do
  begin
    LDiagnostic := LSource.Diagnostic(LAt);
    Write('semantic source diagnostic / ordinal=', LAt,
      ' issue=', Ord(LDiagnostic.Issue), ' occurrence=', LDiagnostic.Occurrence,
      ' start=', LDiagnostic.Span.Start, ' length=', LDiagnostic.Span.Length);

    if (LDiagnostic.Occurrence >= 0) and
      (LDiagnostic.Occurrence < LSource.OccurrenceCount) then
      Write(' file=', LSource.Files.FileAt(
        LSource.Occurrence(LDiagnostic.Occurrence).FileIndex).Name);
    WriteLn(' / ', LDiagnostic.MessageText);
  end;
  WriteLn('semantic syntax diagnostics / count=', AView.Syntax.DiagnosticCount,
    ' lexical=', AView.Syntax.Lexical.DiagnosticCount);
  for LAt := 0 to AView.Syntax.DiagnosticCount - 1 do
  begin
    WriteLn('semantic syntax diagnostic / ordinal=', LAt,
      ' issue=', Ord(AView.Syntax.Diagnostic(LAt).Issue),
      ' start=', AView.Syntax.Diagnostic(LAt).Span.Start,
      ' length=', AView.Syntax.Diagnostic(LAt).Span.Length,
      ' / ', AView.Syntax.Diagnostic(LAt).MessageText);
    LOriginIndex := AView.OriginAt(AView.Syntax.Diagnostic(LAt).Span.Start);

    if LOriginIndex >= 0 then
    begin
      LSyntaxOrigin := AView.Origin(LOriginIndex);
      WriteLn('semantic syntax physical origin / ordinal=', LAt,
        ' file=', LSource.Files.FileAt(LSyntaxOrigin.FileIndex).Name,
        ' occurrence=', LSyntaxOrigin.Occurrence,
        ' start=', LSyntaxOrigin.PhysicalSpan.Start,
        ' length=', LSyntaxOrigin.PhysicalSpan.Length,
        ' state=', LSyntaxOrigin.State, ' expansion=', LSyntaxOrigin.Expansion,
        ' translationTokenText=', LSource.Text(LSyntaxOrigin.TranslationToken));
    end
    else
      WriteLn('semantic syntax physical origin / ordinal=', LAt,
        ' unmapped projection separator or end; no physical affinity inferred');
  end;
  for LAt := 0 to AView.Syntax.Lexical.DiagnosticCount - 1 do
    WriteLn('semantic lexical diagnostic / ordinal=', LAt,
      ' issue=', Ord(AView.Syntax.Lexical.Diagnostic(LAt).Issue),
      ' start=', AView.Syntax.Lexical.Diagnostic(LAt).Span.Start,
      ' length=', AView.Syntax.Lexical.Diagnostic(LAt).Span.Length);
  Flush(Output);
  try
    LPlan := AExistingPlan;

    if LPlan = nil then
    begin

      if not AAllowFreshPlan then
      begin
        WriteLn('semantic unresolved conditions / no retained final plan; diagnostic replanning is not admitted');
        Flush(Output);
        Exit;
      end;
      LPlan := PlanNyxPascalCompilerRequirements(LSource,
        TNyxPascalCompilerPlanOptions.Defaults);
    end
    else if LPlan.Source <> LSource then
      raise Exception.Create('A diagnostic plan must retain the exact final semantic source');
    WriteLn('semantic unresolved conditions / count=', LPlan.Count);
    for LAt := 0 to LPlan.Count - 1 do
    begin
      LOrigin := LPlan.Origin(LAt);
      LNeeds := LPlan.Needs(LAt);
      WriteLn('semantic unresolved condition / ordinal=', LAt,
        ' file=', LOrigin.FileReference.Name, ' occurrence=', LOrigin.Occurrence,
        ' start=', LOrigin.Span.Start, ' length=', LOrigin.Span.Length,
        ' prefix=', LOrigin.PrefixCount, ' state=', LPlan.State(LAt),
        ' supported=', LNeeds.SyntaxSupported, ' / ', LPlan.Expression(LAt));
      for LNeed := 0 to LNeeds.SymbolCount - 1 do
        WriteLn('semantic condition symbol / ', LNeeds.Symbol(LNeed).Name,
          ' initialKind=', Ord(LSource.InitialContext.Symbol(LNeeds.Symbol(LNeed)).Kind),
          ' currentKind=', Ord(LSource.State(LPlan.State(LAt)).Symbol(LNeeds.Symbol(LNeed)).Kind));
      for LNeed := 0 to LNeeds.SizeCount - 1 do
        WriteLn('semantic condition size / ', LNeeds.SizeName(LNeed).Name);

      if Supports(LNeeds, INyxPascalConditionDeclarations, LDeclarations) then
        for LNeed := 0 to LDeclarations.DeclarationCount - 1 do
          WriteLn('semantic condition declaration / ', LDeclarations.DeclarationName(LNeed).Name);
    end;
  except
    on E: ENyxPascalContext do
      WriteLn('semantic condition planning refused / ', E.ClassName, ': ', E.Message);
  end;
  Flush(Output);
end;

{ Continue from the actual immutable semantic frontier, whose answered source
  conditions can expose requirements absent from a raw translation. Each reply
  extends the archived symbols and exact default anchor; the original ordered
  size queries remain unchanged and no added size request is admitted. Planning
  shares the native four-job/4M allowance (archived job included), while every
  semantic attempt shares the original four-million-work/64-pass
  ceiling. Candidate descriptors/results remain local until normal return. }
function ContinueSemanticSymbols(const AFiles: INyxPascalFileCatalog;
  const AWorld: INyxPascalSemanticWorld; const AModule: INyxPascalModuleSource;
  const ASemantic: INyxPascalSemanticTranslation;
  const AProbe: INyxPascalCompilerProbe; const APrevious: INyxFPCCompilerCapture;
  APriorWork: Integer; out APreparedModule: INyxPascalModuleSource;
  out AFinalPlan: INyxPascalCompilerPlan): INyxPascalSemanticTranslation;
const
  CPreparationWork = 4000000;
  CMaximumJobs = 4;
var
  LFiles: INyxPascalFileCatalog;
  LWorld: INyxPascalSemanticWorld;
  LSource: INyxPascalTranslation;
  LPlan: INyxPascalCompilerPlan;
  LPlanWork: INyxPascalCompilerCandidates;
  LProbe, LNext: INyxPascalCompilerProbe;
  LOriginalSizes, LSizes: INyxPascalSizeProbe;
  LAccumulated, LFresh, LCoherent: INyxFPCCompilerCapture;
  LPlanOptions: TNyxPascalCompilerPlanOptions;
  LSemanticOptions: TNyxPascalConditionalOptions;
  LBootstrap, LActualBootstrap, LContext: INyxPascalCompilerContext;
  LModule, LCandidate: INyxPascalModuleSource;
  LSemantic, LNextSemantic: INyxPascalSemanticTranslation;
  LName: TNyxPascalDefineRef;
  LRemainingPlan, LUsedSemantic, LUsedPasses, LWork, LPasses,
    LJobs, LAt, LUnknown, LCapturedJobs: Integer;
  LDirectory, LReplyKind: TNyxText;
  LReplayed, LOutputCreated: Boolean;
begin
  Result := nil;
  APreparedModule := nil;
  AFinalPlan := nil;
  LFiles := AFiles;
  LWorld := AWorld;
  LModule := AModule;
  LSemantic := ASemantic;
  LProbe := CaptureNyxFPCCompilerProbe(AProbe);
  LAccumulated := FreezeNyxFPCCompilerCapture(APrevious);
  LSemanticOptions := TNyxPascalConditionalOptions.Defaults;

  if (APriorWork < 0) or (APriorWork > CPreparationWork) then
    raise Exception.Create('The archived required-symbol plan exhausted the native preparation allowance');
  LRemainingPlan := CPreparationWork - APriorWork;
  LUsedSemantic := LSemantic.WorkTokens;
  LUsedPasses := LSemantic.Passes;

  if (LUsedSemantic < 0) or (LUsedSemantic > LSemanticOptions.MaximumTokens) or
    (LUsedPasses < 1) or (LUsedPasses > LSemanticOptions.MaximumPasses) then
    raise ENyxPascalTranslationLimit.Create('The initial semantic result exceeds its shared work/pass allowance');
  LBootstrap := NyxPascalBootstrapContext(LModule.Context);
  LContext := NyxPascalWithBootstrapContext(LAccumulated.Context, LBootstrap);

  if not LContext.SameFacts(LModule.Context) or
    not LSemantic.Source.InitialContext.SameFacts(LModule.Context) or
    (NyxFPCProbeScope(LProbe) <> pcsoPristine) then
    raise Exception.Create('Semantic continuation must retain the complete archived module/capture identity');

  if not Supports(LProbe, INyxPascalSizeProbe, LOriginalSizes) then
    raise Exception.Create('Required-symbol continuation must retain the archived typed size-query vector');
  LJobs := 1;
  LCapturedJobs := 0;
  LReplayed := False;
  LOutputCreated := False;
  WriteLn('semantic symbol continuation / priorJobs=', LJobs,
    ' maximumJobs=', CMaximumJobs, ' priorPlanWork=', APriorWork,
    ' remainingPlanWork=', LRemainingPlan, ' initialSemanticWork=', LUsedSemantic,
    ' initialSemanticPasses=', LUsedPasses, ' retainedSizeQueries=', LOriginalSizes.SizeCount,
    ' semanticMaximumWork=',
    LSemanticOptions.MaximumTokens, ' semanticMaximumPasses=', LSemanticOptions.MaximumPasses,
    ' retainedReplySupplied=', GSymbolReplay <> '');
  Flush(Output);
  LPlan := nil;
  while LSemantic.Pending do
  begin

    if LRemainingPlan < 1 then
      raise ENyxPascalTranslationLimit.Create('Semantic symbol planning exhausted the shared native preparation allowance');
    LSource := LSemantic.Source;
    LPlanOptions := TNyxPascalCompilerPlanOptions.Defaults;

    if LPlanOptions.MaximumTokens > LRemainingPlan then
      LPlanOptions := LPlanOptions.UpToTokens(LRemainingPlan);
    LPlan := PlanNyxPascalCompilerRequirements(LSource, LPlanOptions);

    if not Supports(LPlan, INyxPascalCompilerCandidates, LPlanWork) then
      raise Exception.Create('The semantic source plan has no exact work receipt');
    LWork := LPlanWork.WorkUnits;

    if (LWork < LSource.Count) or (LWork > LRemainingPlan) then
      raise ENyxPascalTranslationLimit.Create('Semantic source planning exceeds its remaining native allowance');
    Dec(LRemainingPlan, LWork);
    LNext := LPlan.CompilerProbe(LProbe);

    if not Supports(LNext, INyxPascalSizeProbe, LSizes) or
      (LSizes.SizeCount <> LOriginalSizes.SizeCount) then
      raise Exception.Create('Semantic source symbol planning changed the archived size-query count');
    for LAt := 0 to LOriginalSizes.SizeCount - 1 do
    begin

      if LSizes.SizeName(LAt).Name <> LOriginalSizes.SizeName(LAt).Name then
        raise Exception.Create('Semantic source symbol planning changed an archived typed size operand or its order');
    end;
    LUnknown := 0;
    for LAt := 0 to LNext.Count - 1 do
    begin
      LName := LNext.Symbol(LAt);

      if LAccumulated.Context.Symbol(LName).Kind <> ppdUnknown then
        Continue;

      if (LAt < LProbe.Count) or
        (LSource.InitialContext.Symbol(LName).Kind <> ppdUnknown) then
        raise Exception.Create('A symbol request is not a newly unknown initial semantic-source fact');
      Inc(LUnknown);
      WriteLn('semantic symbol request / attempt=', LJobs,
        ' ordinal=', LAt, ' name=', LName.Name);
    end;
    WriteLn('semantic symbol plan / attempt=', LJobs,
      ' sourceTokens=', LSource.Count, ' conditions=', LPlan.Count,
      ' work=', LWork, ' remainingPlanWork=', LRemainingPlan,
      ' previousNames=', LProbe.Count, ' requestedNames=', LNext.Count,
      ' retainedSizeQueries=', LSizes.SizeCount,
      ' newInitialSymbols=', LUnknown, ' usedSemanticWork=', LUsedSemantic,
      ' usedSemanticPasses=', LUsedPasses);
    Flush(Output);

    if LUnknown = 0 then
    begin
      WriteLn('semantic symbol stop / no eligible new initial symbols; typed source/size/import requirements remain unresolved');
      Flush(Output);
      Break;
    end;

    if LUnknown <> LNext.Count - LProbe.Count then
      raise Exception.Create('A semantic continuation request contains an ineligible appended symbol');

    if LJobs >= CMaximumJobs then
      raise ENyxPascalContext.Create('Semantic symbol continuation exhausted the four-job ceiling including the archived job');

    if (LUsedSemantic >= LSemanticOptions.MaximumTokens) or
      (LUsedPasses >= LSemanticOptions.MaximumPasses) then
      raise ENyxPascalTranslationLimit.Create('Semantic symbol continuation exhausted its shared work/pass ceiling');

    if (GSymbolReplay <> '') and not LReplayed then
    begin
      Inc(LJobs);
      WriteLn('semantic symbol replay / admittedJob=', LJobs,
        ' maximumJobs=', CMaximumJobs, ' directory=', GSymbolReplay);
      Flush(Output);
      { The exact current typed request must equal the retained source before
        any reply is parsed. A mismatch refuses; it never launches a substitute
        compiler job or trims/reinterprets the accepted request. }
      LFresh := ReadProbe(GSymbolReplay, LNext);
      LReplayed := True;
      LReplyKind := 'retained';
    end
    else
    begin

      if not LOutputCreated then
      begin

        if not CreateDirUTF8(GRequiredOutput) then
          raise Exception.Create('The explicit fresh semantic-symbol output directory could not be created');
        LOutputCreated := True;
      end;
      LDirectory := GRequiredOutput + '/probe-' + TNyxText(IntToStr(LJobs));
      Inc(LJobs);
      Inc(LCapturedJobs);
      WriteLn('semantic symbol capture / admittedJob=', LJobs,
        ' maximumJobs=', CMaximumJobs, ' directory=', LDirectory);
      Flush(Output);
      LFresh := CaptureNyxFPCCompilerCapture(GRequiredCompiler, LDirectory, LNext,
        TNyxPascalCompilerTraceOptions.Defaults);
      LReplyKind := 'native';
    end;
    LCoherent := LAccumulated.Merge(LFresh);
    { Merge verifies the full observed-default anchor and shared facts. The
      cumulative request also requires every earlier fact in this exact reply;
      independent compiler-root evidence remains a separate attachment. }

    if not LCoherent.Context.SameFacts(LFresh.Context) then
      raise Exception.Create('A semantic-source reply omitted or changed previously captured compiler facts');
    LContext := NyxPascalWithBootstrapContext(LCoherent.Context, LBootstrap);
    LActualBootstrap := NyxPascalBootstrapContext(LContext);

    if (LBootstrap = nil) <> (LActualBootstrap = nil) then
      raise Exception.Create('Semantic symbol continuation changed independent bootstrap presence');

    if (LBootstrap <> nil) and not LBootstrap.SameFacts(LActualBootstrap) then
      raise Exception.Create('Semantic symbol continuation changed independent bootstrap facts');
    LCandidate := LModule.InContext(LContext, LModule.Translation);
    WriteLn('semantic symbol reply / admittedJob=', LJobs,
      ' kind=', LReplyKind,
      ' completeDefaultAnchor=TRUE previousFacts=TRUE independentBootstrap=TRUE',
      ' remainingSemanticWork=', LSemanticOptions.MaximumTokens - LUsedSemantic,
      ' remainingSemanticPasses=', LSemanticOptions.MaximumPasses - LUsedPasses);
    Flush(Output);
    LNextSemantic := NewNyxPascalSemanticTranslation(LFiles, LCandidate.Root,
      LCandidate.Context, LWorld, LCandidate.Translation,
      TNyxPascalSyntaxOptions.Defaults.UpToDeclarations(100000),
      LSemanticOptions.UpToTokens(LSemanticOptions.MaximumTokens - LUsedSemantic)
        .UpToPasses(LSemanticOptions.MaximumPasses - LUsedPasses));
    LWork := LNextSemantic.WorkTokens;
    LPasses := LNextSemantic.Passes;

    if (LWork < 0) or (LWork > LSemanticOptions.MaximumTokens - LUsedSemantic) or
      (LPasses < 1) or (LPasses > LSemanticOptions.MaximumPasses - LUsedPasses) then
      raise ENyxPascalTranslationLimit.Create('A semantic continuation result exceeds its admitted remaining work/passes');
    Inc(LUsedSemantic, LWork);
    Inc(LUsedPasses, LPasses);
    LAccumulated := LCoherent;
    LProbe := LNext;
    LModule := LCandidate;
    LSemantic := LNextSemantic;
    LPlan := nil;
    WriteLn('semantic symbol attempt / continuationJobs=', LJobs - 1,
      ' replayedJobs=', Ord(LReplayed), ' freshCaptures=', LCapturedJobs,
      ' totalJobs=', LJobs, ' semanticWork=', LWork,
      ' semanticPasses=', LPasses, ' totalSemanticWork=', LUsedSemantic,
      ' totalSemanticPasses=', LUsedPasses, ' remainingPlanWork=', LRemainingPlan,
      ' pending=', LSemantic.Pending, ' issue=', Ord(LSemantic.BindingIssue),
      ' answers=', LSemantic.Answers, ' related=', LSemantic.RelatedName);
    Flush(Output);
  end;

  if (GSymbolReplay <> '') and not LReplayed then
    raise Exception.Create('The explicit retained continuation reply has no matching current source-requested job');
  WriteLn('semantic symbol totals / continuationJobs=', LJobs - 1,
    ' replayedJobs=', Ord(LReplayed), ' freshCaptures=', LCapturedJobs,
    ' totalJobs=', LJobs, ' planWork=', CPreparationWork - LRemainingPlan,
    ' semanticWork=', LUsedSemantic, ' semanticPasses=', LUsedPasses);
  Flush(Output);
  APreparedModule := LModule;
  AFinalPlan := LPlan;
  Result := LSemantic;
end;

{ Restore an explicitly supplied accepted cumulative request as intent, then
  verify its terminal reply against every prior query and the complete default
  anchor. This path never infers a query from a filename or recollects a reply. }
function ReadPreparedProfile(const ADirectory: TNyxText;
  const APreviousProbe: INyxPascalCompilerProbe;
  const APrevious: INyxFPCCompilerCapture; AExpectedCount: Integer;
  const APreparation: INyxPascalAnalysisBudget;
  out AProbe: INyxPascalCompilerProbe): INyxFPCCompilerCapture;
var
  LProbe: INyxPascalCompilerProbe;
  LPreviousSizes, LSizes: INyxPascalSizeProbe;
  LFresh, LCoherent: INyxFPCCompilerCapture;
  LAt, LBefore: Integer;
begin
  Result := nil;
  AProbe := nil;
  LBefore := APreparation.Receipt.Total;
  LProbe := RestoreNyxFPCCompilerProbe(ReadText(ADirectory + '/nyxcompilercontext.pas'),
    TNyxPascalCompilerTraceOptions.Defaults, APreparation);

  if (LProbe.Count <> AExpectedCount) or (LProbe.Count <= APreviousProbe.Count) or
    (LProbe.Identity.Name <> APreviousProbe.Identity.Name) or
    (LProbe.Mode <> APreviousProbe.Mode) or
    (NyxFPCProbeScope(LProbe) <> pcsoPristine) or
    (NyxFPCProbeScope(APreviousProbe) <> pcsoPristine) then
    raise Exception.Create('A prepared cumulative request changed its exact identity, source mode, scope or accepted cohort count');
  for LAt := 0 to APreviousProbe.Count - 1 do

    if LProbe.Symbol(LAt).Name <> APreviousProbe.Symbol(LAt).Name then
      raise Exception.Create('A prepared cumulative request changed the archived ordered symbol prefix');
  for LAt := APreviousProbe.Count to LProbe.Count - 1 do

    if APrevious.Context.Symbol(LProbe.Symbol(LAt)).Kind <> ppdUnknown then
      raise Exception.Create('A prepared request appended a symbol already established by the preceding capture');

  if not Supports(APreviousProbe, INyxPascalSizeProbe, LPreviousSizes) or
    not Supports(LProbe, INyxPascalSizeProbe, LSizes) or
    (LPreviousSizes.SizeCount <> 4) or (LSizes.SizeCount <> LPreviousSizes.SizeCount) then
    raise Exception.Create('A prepared request must retain the original four typed size queries');
  for LAt := 0 to LPreviousSizes.SizeCount - 1 do

    if LSizes.SizeName(LAt).Name <> LPreviousSizes.SizeName(LAt).Name then
      raise Exception.Create('A prepared request changed the archived typed size names or their order');
  LFresh := ReadProbe(ADirectory, LProbe, APreparation);
  LCoherent := APrevious.Merge(LFresh);

  if not LCoherent.Context.SameFacts(LFresh.Context) then
    raise Exception.Create('A prepared terminal reply omitted or changed an earlier complete compiler fact');
  WriteLn('prepared profile replay / previousNames=', APreviousProbe.Count,
    ' acceptedNames=', LProbe.Count, ' sizeQueries=', LSizes.SizeCount,
    ' completeDefaultAnchor=TRUE orderedPrefix=TRUE previousFacts=TRUE',
    ' work=', APreparation.Receipt.Total - LBefore,
    ' totalPreparationWork=', APreparation.Receipt.Total,
    ' directory=', ADirectory);
  Flush(Output);
  AProbe := LProbe;
  Result := LCoherent;
end;

{ Current required-only planning retains its existing local cap and debits its
  exact detached work once against the same preparation operation used to restore
  saved requests. The planner's cancellation argument does not forward work. }
function PreparedSourcePlan(const ASource: INyxPascalTranslation;
  const APreparation: INyxPascalAnalysisBudget): INyxPascalCompilerPlan;
var
  LOptions: TNyxPascalCompilerPlanOptions;
  LPlan: INyxPascalCompilerPlan;
  LWork: INyxPascalCompilerCandidates;
  LRemaining: Integer;
begin
  Result := nil;
  LRemaining := APreparation.Receipt.Maximum - APreparation.Receipt.Total;

  if LRemaining < 1 then
    raise ENyxPascalTranslationLimit.Create('Prepared source planning exhausted its shared four-million allowance');
  LOptions := TNyxPascalCompilerPlanOptions.Defaults;

  if LOptions.MaximumTokens > LRemaining then
    LOptions := LOptions.UpToTokens(LRemaining);
  LPlan := PlanNyxPascalCompilerRequirements(ASource, LOptions, APreparation);

  if not Supports(LPlan, INyxPascalCompilerCandidates, LWork) or
    (LWork.WorkUnits < ASource.Count) or (LWork.WorkUnits > LRemaining) then
    raise ENyxPascalTranslationLimit.Create('Prepared source planning has no exact admitted work receipt');
  APreparation.Admit(papSourceProof, LWork.WorkUnits);
  Result := LPlan;
end;

{ Print detached semantic costs for each real namespace construction, including
  every admitted analysis phase. Reading this receipt grants no reuse authority. }
procedure ReportNamespaceSemantic(const AStage, AName: TNyxText;
  const AValue: INyxPascalSemanticTranslation;
  const AView: INyxPascalTranslatedSyntax);
var
  LWork: INyxPascalSemanticAnalysisWork;
  LReceipt: TNyxPascalAnalysisReceipt;
  LPhase: TNyxPascalAnalysisPhase;
begin
  WriteLn('namespace semantic / stage=', AStage, ' name=', AName,
    ' files=', AValue.Source.Files.Count, ' declarations=', AView.Syntax.DeclarationCount,
    ' work=', AValue.WorkTokens, ' passes=', AValue.Passes,
    ' answers=', AValue.Answers, ' pending=', AValue.Pending,
    ' sourceResolved=', AValue.Source.Resolved, ' complete=', AView.CompleteTranslation,
    ' maximumWork=4000000 maximumPasses=64 viewMaximum=2000000 syntaxMaximum=100000');

  if not Supports(AValue, INyxPascalSemanticAnalysisWork, LWork) then
    raise Exception.Create('A constructed namespace omitted its detached analysis receipt');
  LReceipt := LWork.AnalysisWork;

  if LReceipt.Total <> AValue.WorkTokens then
    raise Exception.Create('A namespace analysis receipt disagrees with its exact semantic work');
  WriteLn('namespace receipt / stage=', AStage, ' name=', AName,
    ' total=', LReceipt.Total, ' admittedMaximum=', LReceipt.Maximum);
  for LPhase := Low(TNyxPascalAnalysisPhase) to High(TNyxPascalAnalysisPhase) do

    if LReceipt.Used(LPhase) > 0 then
      WriteLn('namespace analysis / stage=', AStage, ' name=', AName,
        ' phase=', NyxPascalAnalysisPhaseName(LPhase), ' units=', LReceipt.Used(LPhase));
  Flush(Output);
end;

{ Rebuild current meanings after a validated independent root delta. Each old
  checked namespace supplies its exact physical catalog and search order; only
  updated descriptors enter the new empty world. No old view is retagged. }
function RebuildRootWorld(const APrevious: INyxPascalSemanticWorld;
  const AModules: INyxPascalModuleCatalog;
  const ABootstrap: INyxFPCCompilerCapture): INyxPascalSemanticWorld;
var
  LWorld: INyxPascalSemanticWorld;
  LModule: INyxPascalModuleSource;
  LFiles: INyxPascalFileCatalog;
  LSource: INyxPascalTranslation;
  LSemantic: INyxPascalSemanticTranslation;
  LView, LPreviousView: INyxPascalTranslatedSyntax;
  LLexical: INyxPascalTranslationLexicalWork;
  LRoot: INyxPascalCompilerContext;
  LName: TNyxText;
  LAt, LModuleAt, LPreviousAt, LExpectedDeclarations, LExpectedAnswers: Integer;
begin
  Result := nil;
  LWorld := NewNyxPascalSemanticWorld(APrevious.Options);
  for LAt := 0 to 2 do
  begin
    LName := 'System';
    LExpectedDeclarations := 13101;
    LExpectedAnswers := 31;

    if LAt = 1 then
    begin
      LName := 'ObjPas';
      LExpectedDeclarations := 209;
      LExpectedAnswers := 1;
    end
    else if LAt = 2 then
    begin
      LName := 'Windows';
      LExpectedDeclarations := 29823;
    end;
    LPreviousAt := APrevious.FindModule(LName);
    LModuleAt := AModules.Find(NyxPascalModule(LName));

    if (LPreviousAt < 0) or (LModuleAt < 0) then
      raise Exception.Create('A root rebuild lacks an original checked namespace/updated descriptor: ' + LName);
    LPreviousView := APrevious.ModuleAt(LPreviousAt);
    LFiles := LPreviousView.Source.Files;
    LModule := AModules.Entry(LModuleAt);
    LRoot := NyxPascalBootstrapContext(LModule.Context);

    if (LRoot = nil) or not LRoot.SameFacts(ABootstrap.Context) or
      (LModule.Root.Name <> LPreviousView.Source.Files.FileAt(
        LPreviousView.Source.Occurrence(0).FileIndex).Name) or
      ((LAt < 2) and (LFiles.Count <> 156)) or
      ((LAt = 2) and (LFiles.Count <> 28)) then
      raise Exception.Create('A root rebuild changed a namespace root, exact source catalog or independent root authority');
    WriteLn('namespace rebuild begin / name=', LName, ' files=', LFiles.Count,
      ' root=', LModule.Root.Name);
    Flush(Output);

    if LAt < 2 then
    begin
      LSemantic := NewNyxPascalSemanticTranslation(LFiles, LModule.Root,
        LModule.Context, LWorld, LModule.Translation,
        TNyxPascalSyntaxOptions.Defaults.UpToDeclarations(100000),
        TNyxPascalConditionalOptions.Defaults.UpToTokens(4000000));
      LSource := LSemantic.Source;
      LView := NewNyxPascalTranslatedSyntax(LSource,
        TNyxPascalSyntaxOptions.Defaults.UpToDeclarations(100000));
      ReportNamespaceSemantic('updated-root', LName, LSemantic, LView);

      if LSemantic.Pending or (LSemantic.Answers <> LExpectedAnswers) then
        raise Exception.Create('A rebuilt namespace changed its original complete ordered semantic answers: ' + LName);
    end
    else
    begin
      { Windows originally used complete plain translation, so this rebuild
        preserves that contract and reports physical work without inventing
        semantic answers, passes or a semantic receipt. }
      LSource := NewNyxPascalTranslation(LFiles, LModule.Root,
        LModule.Context, LModule.Translation);
      LView := NewNyxPascalTranslatedSyntax(LSource,
        TNyxPascalSyntaxOptions.Defaults.UpToDeclarations(100000));
      WriteLn('namespace translation / stage=updated-root name=Windows files=',
        LFiles.Count, ' declarations=', LView.Syntax.DeclarationCount,
        ' translatedTokens=', LSource.Count,
        ' semanticWork=not-applicable passes=not-applicable answers=not-applicable');

      if Supports(LSource, INyxPascalTranslationLexicalWork, LLexical) then
        WriteLn('namespace lexical / stage=updated-root name=Windows scanned=',
          LLexical.ScannedLexicalTokens, ' reused=', LLexical.ReusedLexicalTokens,
          ' candidates=', LLexical.LexicalCandidateReads);
      Flush(Output);
    end;

    if not LSource.Resolved or not LView.CompleteTranslation or
      (LSource.DiagnosticCount <> 0) or (LView.Syntax.DiagnosticCount <> 0) or
      (LView.Syntax.DeclarationCount <> LExpectedDeclarations) then
      raise Exception.Create('A rebuilt genuine namespace is incomplete or changed its full declaration count: ' + LName);
    LWorld := LWorld.WithModule(LView);
  end;
  Result := LWorld;
end;

{ Replay the accepted primitive packet only after the original six fact jobs.
  A real checked System alias derives the current typed requirement; neither
  its source position nor a missing compiler-root leaf is manufactured here. }
procedure ReplayPreparedPrimitive(const AWorld: INyxPascalSemanticWorld;
  const AModules: INyxPascalModuleCatalog; const APrevious: INyxFPCCompilerCapture;
  APriorJobs: Integer; const APreparation: INyxPascalAnalysisBudget;
  out AUpdatedModules: INyxPascalModuleCatalog;
  out AUpdatedWorld: INyxPascalSemanticWorld;
  out AUpdatedRoot: INyxFPCCompilerCapture);
var
  LProbe, LExpected: INyxPascalCompilerProbe;
  LSizes: INyxPascalSizeProbe;
  LView: INyxPascalTranslatedSyntax;
  LType: INyxPascalTypeSyntax;
  LDeclaration: TNyxPascalSyntaxDeclaration;
  LResolution: INyxPascalResolution;
  LRequirements: INyxPascalCompilerRequirements;
  LRequirement: INyxPascalCompilerRequirement;
  LReference: TNyxPascalSizeReference;
  LOrigin: TNyxPascalSyntaxOrigin;
  LCaptures, LReplies: INyxPascalCompilerCaptures;
  LPlan: INyxPascalCompilerFulfillmentPlan;
  LApplied: INyxPascalCompilerFulfillment;
  LFresh, LRoot: INyxFPCCompilerCapture;
  LModules: INyxPascalModuleCatalog;
  LWorld: INyxPascalSemanticWorld;
  LSystem, LAt, LOwner, LBefore: Integer;
begin
  AUpdatedModules := nil;
  AUpdatedWorld := nil;
  AUpdatedRoot := nil;

  if APriorJobs <> 6 then
    raise Exception.Create('Primitive replay follows the unchanged original six fact jobs');
  LBefore := APreparation.Receipt.Total;
  LProbe := RestoreNyxFPCCompilerProbe(ReadText(GPrimitiveReplay + '/system.pp'),
    TNyxPascalCompilerTraceOptions.Defaults, APreparation);
  LExpected := NewNyxFPCSystemBootstrapProbe(NyxPascalDefine('NYX_REQUIRED_FACT'));

  if (NyxFPCProbeScope(LProbe) <> pcsoSystemBootstrap) or
    (LProbe.Mode <> ppmFPC) or (LProbe.Identity.Name <> LExpected.Identity.Name) or
    (LProbe.Count <> LExpected.Count) or
    not Supports(LProbe, INyxPascalSizeProbe, LSizes) or (LSizes.SizeCount <> 1) then
    raise Exception.Create('The retained primitive request must be one exact bootstrap size query with the original guard vector');
  for LAt := 0 to LExpected.Count - 1 do
  begin
    APreparation.Admit(papSourceProof);

    if LProbe.Symbol(LAt).Name <> LExpected.Symbol(LAt).Name then
      raise Exception.Create('Primitive replay changed an ordered bootstrap guard');
  end;
  LReference := LSizes.SizeName(0);

  if Pos('.', LReference.Name) <> 0 then
    raise Exception.Create('The retained bootstrap primitive must retain its bare compiler-root identity');

  if NyxPascalCapturedSize(APrevious.Context, LReference).State <> pssUnknown then
    raise Exception.Create('The retained primitive must add its one previously unknown fact after the old six replies');
  LSystem := AWorld.FindModule('System');

  if LSystem < 0 then
    raise Exception.Create('Primitive replay needs the original complete System namespace');
  LView := AWorld.ModuleAt(LSystem);

  if not LView.CompleteTranslation or (LView.Syntax.DiagnosticCount <> 0) or
    (LView.Source.Files.Count <> 156) or
    not APrevious.Context.SameFacts(NyxPascalBootstrapContext(LView.Source.InitialContext)) then
    raise Exception.Create('Primitive replay changed its original156 checked System source/root owner');
  LResolution := nil;
  LDeclaration := Default(TNyxPascalSyntaxDeclaration);
  for LAt := 0 to LView.Syntax.DeclarationCount - 1 do
  begin
    APreparation.Admit(papSourceProof);
    LDeclaration := LView.Syntax.Declaration(LAt);

    if (LDeclaration.Kind <> psdkType) or (LDeclaration.Scope <> 0) or
      (LDeclaration.Visibility <> psavInterface) or (LDeclaration.GenericSpan.Length <> 0) or
      (LDeclaration.Flags * [psdfIncomplete, psdfIncompleteHeader, psdfConditional,
        psdfUnsupported, psdfForward] <> []) then Continue;
    LType := LView.TypeOfDeclaration(LAt, TNyxPascalTypeOptions.Defaults, APreparation);

    if not LType.Complete or (LType.Node(LType.Root).Kind <> ptxName) then Continue;
    APreparation.Admit(papSourceProof, Length(LType.Node(LType.Root).Name) + 1);

    if NyxPascalASCIIKey(LType.Node(LType.Root).Name) <> LReference.Name then Continue;
    LResolution := AWorld.TypeOfSymbol(AWorld.Symbol(LSystem, LAt), APreparation);
    Break;
  end;

  if LResolution = nil then
    raise Exception.Create('The retained primitive has no actual simple-alias use in the current complete System owner');
  LRequirements := CaptureNyxPascalRequirements(LResolution);

  if (LResolution.Status <> pbsUnresolved) or (LResolution.Issue <> pbiCompilerContext) or
    (LRequirements.RequirementCount <> 1) then
    raise Exception.Create('The actual System alias must derive the current one-fact bootstrap prerequisite');
  LRequirement := LRequirements.RequirementAt(0);

  if (LRequirement.Kind <> pcrPrimitiveSize) or
    (LRequirement.Reference.Name <> LReference.Name) or
    not LRequirement.Context.SameFacts(APrevious.Context) or
    (LRequirement.OwnerSource <> LView) or (LRequirement.Source <> LView) or
    (LRequirement.SourceOffset < LDeclaration.TypeSpan.Start) or
    (LRequirement.SourceOffset >= LDeclaration.TypeSpan.Finish) or
    (LRequirement.OriginIndex < 0) or
    (LView.OriginAt(LRequirement.SourceOffset) <> LRequirement.OriginIndex) then
    raise Exception.Create('The derived primitive requirement changed its exact alias type window, origin or original root authority');
  LOrigin := LView.Origin(LRequirement.OriginIndex);
  LCaptures := NewNyxPascalCompilerCaptures.WithCapture(APrevious);
  LPlan := PlanNyxPascalCompilerFulfillment(LView.Source.Files, AModules,
    LResolution, LCaptures, 1, APreparation);

  if (LPlan.Count <> 1) or (LPlan.RequirementAt(0).Reference.Name <> LReference.Name) then
    raise Exception.Create('The primitive replay plan must retain that one actual source-owned requirement');
  { ApplyReplies requires the complete one-fact delta, retaining every old
    compiler fact and the observed-default anchor rather than the raw packet. }
  LFresh := MergeNyxFPCSizeCapture(APrevious,
    ReadProbe(GPrimitiveReplay, LProbe, APreparation), LReference);
  LReplies := NewNyxPascalCompilerCaptures.WithCapture(LFresh);
  LApplied := LPlan.ApplyReplies(LReplies, APreparation);

  if (LApplied.Jobs <> 1) or (APriorJobs + LApplied.Jobs > 8) then
    raise Exception.Create('Primitive replay changed the preserved seven-of-eight logical fact count');
  LModules := LApplied.Modules;
  LOwner := LApplied.Captures.Find(NyxPascalBootstrapContext(
    LModules.Entry(LModules.Find(NyxPascalModule('System'))).Context));

  if LOwner < 0 then
    raise Exception.Create('Primitive replay lost the independently updated compiler-root capture');
  LRoot := LApplied.Captures.CaptureAt(LOwner);
  WriteLn('prepared primitive replay / reference=', LReference.Name,
    ' bytes=', NyxPascalCapturedSize(LRoot.Context, LReference).Bytes,
    ' alias=', LDeclaration.Name,
    ' file=', LView.Source.Files.FileAt(LOrigin.FileIndex).Name,
    ' sourceOffset=', LRequirement.SourceOffset, ' originIndex=', LRequirement.OriginIndex,
    ' ownerFiles=', LView.Source.Files.Count,
    ' fullDefaultAnchor=TRUE previousFacts=TRUE exactOneFact=TRUE',
    ' priorDynamicJobs=', APriorJobs, ' replayedDynamicJobs=1 totalDynamicJobs=',
    APriorJobs + LApplied.Jobs, ' maximumDynamicJobs=8 freshFactCaptures=0',
    ' work=', APreparation.Receipt.Total - LBefore,
    ' totalPreparationWork=', APreparation.Receipt.Total,
    ' directory=', GPrimitiveReplay);
  Flush(Output);
  LWorld := RebuildRootWorld(AWorld, LModules, LRoot);
  AUpdatedModules := LModules;
  AUpdatedWorld := LWorld;
  AUpdatedRoot := LRoot;
end;

{ Prepare only the two actual missing SysUtils implementation imports. Their
  complete real units must prove their exported namespaces before DECLARED can
  establish absence. Each new namespace has its existing independent 4M/64
  analysis allowance; neither construction spends or resets a SysUtils attempt.
  All candidates remain local until both units pass, and later SysUtils still
  controls publication of the whole updated source world. }
procedure PrepareImportedNamespaces(var AFiles: INyxPascalFileCatalog;
  var AModules: INyxPascalModuleCatalog; var AWorld: INyxPascalSemanticWorld;
  const AInitial: INyxPascalCompilerContext;
  const APreparation: INyxPascalAnalysisBudget);
var
  LExtra: INyxPascalDirectoryCapture;
  LExtraBytes: INyxPascalByteFileCatalog;
  LProducer: TImportPacket;
  LPackets: INyxPascalFilePackets;
  LFiles: INyxPascalFileCatalog;
  LModules: INyxPascalModuleCatalog;
  LWorld: INyxPascalSemanticWorld;
  LModule: INyxPascalModuleSource;
  LSemantic: INyxPascalSemanticTranslation;
  LView: INyxPascalTranslatedSyntax;
  LName: TNyxText;
  LAt, LModuleAt: Integer;
begin

  if (AFiles.Count <> 216) or (AInitial = nil) or
    (NyxPascalCompilerScope(AInitial) <> pcsoPristine) or
    (NyxPascalBootstrapContext(AInitial) = nil) or
    (AInitial.Feature(ppfNestedComments) = ppkUnknown) then
    raise Exception.Create('Actual imports need the original216 sources and exact prepared initial context with independent root facts');
  LExtra := ReadManifestInputs(GImportInputs, GImportInputs + '/import1-paths.txt', 1);

  if (LExtra.UnindexedCount <> 0) or (LExtra.Modules.Count <> 1) or
    (LExtra.Modules.Find(NyxPascalModule('SysConst')) <> 0) or
    (LExtra.Files.FileAt(0).Name <> 'rtl/objpas/sysconst.pp') or
    (LExtra.Modules.Entry(0).Root.Name <> LExtra.Files.FileAt(0).Name) or
    not Supports(LExtra.Files, INyxPascalByteFileCatalog, LExtraBytes) or
    (LExtraBytes.SourceViewAt(0) = nil) or
    (AFiles.Find(LExtra.Files.FileAt(0)) >= 0) or
    (AModules.Find(NyxPascalModule('SysConst')) >= 0) then
    raise Exception.Create('The explicit extra input must own one new exact decoded SysConst header/root without replacing retained inputs');
  LProducer := TImportPacket.Create;
  LPackets := LProducer;
  LProducer.Value := NewNyxPascalFilePacket(LExtra.Files.FileAt(0),
    LExtraBytes.SourceBytesAt(0), LExtraBytes.SourceViewAt(0), APreparation);
  LFiles := OverlayNyxPascalFilePackets(AFiles, LPackets, APreparation);
  LModules := AModules.WithModule(LExtra.Modules.Entry(0));
  LWorld := AWorld;

  if LFiles.Count <> 217 then
    raise Exception.Create('Only the one requested import source may extend the original216 catalog');
  for LAt := 0 to 1 do
  begin
    LName := 'WinDirs';

    if LAt = 1 then LName := 'SysConst';
    APreparation.Admit(papSourceProof);
    LModuleAt := LModules.Find(NyxPascalModule(LName));

    if (LModuleAt < 0) or (LWorld.FindModule(LName) >= 0) then
      raise Exception.Create('An actual import needs one captured root and no preexisting substitute namespace: ' + LName);
    LModule := LModules.Entry(LModuleAt);

    if LFiles.Find(LModule.Root) < 0 then
      raise Exception.Create('The captured imported root is outside the exact current source catalog: ' + LName);
    { The pristine input is the accepted profile, never a later source state.
      The unit's own MODE/H directives establish its local compiler settings. }
    LModule := LModule.InContext(AInitial,
      TNyxPascalTranslationOptions.Defaults.UpToTokens(2000000)
        .Lexical(TNyxPascalLexicalOptions.Defaults.NestedComments(
          AInitial.Feature(ppfNestedComments) = ppkTrue)));
    LSemantic := NewNyxPascalSemanticTranslation(LFiles, LModule.Root,
      LModule.Context, LWorld, LModule.Translation,
      TNyxPascalSyntaxOptions.Defaults.UpToDeclarations(100000),
      TNyxPascalConditionalOptions.Defaults.UpToTokens(4000000));
    LView := NewNyxPascalTranslatedSyntax(LSemantic.Source,
      TNyxPascalSyntaxOptions.Defaults.UpToDeclarations(100000));
    ReportNamespaceSemantic('actual-import', LName, LSemantic, LView);

    if LSemantic.Pending or not LView.CompleteTranslation or
      not LView.Source.Resolved or (LView.Source.DiagnosticCount <> 0) or
      (LView.Syntax.DiagnosticCount <> 0) or
      (NyxPascalASCIIKey(LView.Syntax.Scope(0).Name) <> NyxPascalASCIIKey(LName)) then
    begin
      ReportSemanticFrontier(LSemantic, LView, nil, False);
      raise Exception.Create('The actual imported namespace must be complete before SysUtils analysis: ' + LName);
    end;
    LModules := LModules.WithModule(LModule);
    LWorld := LWorld.WithModule(LView);
  end;
  AFiles := LFiles;
  AModules := LModules;
  AWorld := LWorld;
end;

{ Profile-first maintenance admits at most the actual fourth cumulative symbol
  job. Both semantic constructions share one work/pass ceiling; the remaining
  source/size/declaration frontier stays an ordinary final completeness failure. }
function ContinueProfileFirstSymbols(const AFiles: INyxPascalFileCatalog;
  const AWorld: INyxPascalSemanticWorld; const AModule: INyxPascalModuleSource;
  const ASemantic: INyxPascalSemanticTranslation;
  const AProbe: INyxPascalCompilerProbe; const APrevious: INyxFPCCompilerCapture;
  const ABootstrap: INyxPascalCompilerContext;
  const APreparation: INyxPascalAnalysisBudget;
  out APreparedModule: INyxPascalModuleSource;
  out AFinalPlan: INyxPascalCompilerPlan): INyxPascalSemanticTranslation;
var
  LPlan: INyxPascalCompilerPlan;
  LNext: INyxPascalCompilerProbe;
  LOriginalSizes, LSizes: INyxPascalSizeProbe;
  LRequirements: INyxPascalCompilerRequirements;
  LFresh, LCoherent: INyxFPCCompilerCapture;
  LContext, LRestoredRoot: INyxPascalCompilerContext;
  LModule: INyxPascalModuleSource;
  LSemantic: INyxPascalSemanticTranslation;
  LView: INyxPascalTranslatedSyntax;
  LOptions: TNyxPascalConditionalOptions;
  LName: TNyxPascalDefineRef;
  LAt, LUnknown, LUsedWork, LUsedPasses: Integer;
  LDirectory: TNyxText;
begin
  Result := nil;
  APreparedModule := nil;
  AFinalPlan := nil;
  LOptions := TNyxPascalConditionalOptions.Defaults;
  LUsedWork := ASemantic.WorkTokens;
  LUsedPasses := ASemantic.Passes;

  if (LUsedWork < 0) or (LUsedWork > LOptions.MaximumTokens) or
    (LUsedPasses < 1) or (LUsedPasses > LOptions.MaximumPasses) then
    raise ENyxPascalTranslationLimit.Create('The profile-first initial SysUtils result exceeds its unchanged shared ceiling');
  LContext := NyxPascalWithBootstrapContext(APrevious.Context, ABootstrap);

  if not LContext.SameFacts(AModule.Context) or
    not ASemantic.Source.InitialContext.SameFacts(AModule.Context) or
    (AProbe.Count <> 36) or (NyxFPCProbeScope(AProbe) <> pcsoPristine) or
    not Supports(AProbe, INyxPascalSizeProbe, LOriginalSizes) or
    (LOriginalSizes.SizeCount <> 4) then
    raise Exception.Create('Profile-first continuation changed its exact prepared36/four-size/independent-root source identity');
  LView := NewNyxPascalTranslatedSyntax(ASemantic.Source,
    TNyxPascalSyntaxOptions.Defaults.UpToDeclarations(100000));
  ReportNamespaceSemantic('profile-first-initial', 'SysUtils', ASemantic, LView);

  if not ASemantic.Pending then
  begin
    APreparedModule := AModule;
    Result := ASemantic;
    Exit;
  end;
  LPlan := PreparedSourcePlan(ASemantic.Source, APreparation);
  LRequirements := CaptureNyxPascalRequirements(ASemantic);

  if LRequirements.RequirementCount <> 0 then
  begin
    ReportSemanticFrontier(ASemantic, LView, LPlan, False);
    raise Exception.Create('Profile-first SysUtils has another typed source/size prerequisite; no additional fact job is admitted');
  end;
  LNext := LPlan.CompilerProbe(AProbe);

  if (LNext.Count < AProbe.Count) or (LNext.Mode <> AProbe.Mode) or
    (LNext.Identity.Name <> AProbe.Identity.Name) or
    (NyxFPCProbeScope(LNext) <> pcsoPristine) or
    not Supports(LNext, INyxPascalSizeProbe, LSizes) or
    (LSizes.SizeCount <> LOriginalSizes.SizeCount) then
    raise Exception.Create('The fourth actual symbol cohort changed its original request envelope');
  for LAt := 0 to AProbe.Count - 1 do
  begin
    APreparation.Admit(papSourceProof);

    if LNext.Symbol(LAt).Name <> AProbe.Symbol(LAt).Name then
      raise Exception.Create('The fourth actual cohort changed the exact ordered36-name prefix');
  end;
  for LAt := 0 to LOriginalSizes.SizeCount - 1 do
  begin
    APreparation.Admit(papSourceProof);

    if LSizes.SizeName(LAt).Name <> LOriginalSizes.SizeName(LAt).Name then
      raise Exception.Create('The fourth actual cohort changed an archived typed size or its order');
  end;
  LUnknown := 0;
  for LAt := AProbe.Count to LNext.Count - 1 do
  begin
    APreparation.Admit(papSourceProof);
    LName := LNext.Symbol(LAt);

    if (APrevious.Context.Symbol(LName).Kind <> ppdUnknown) or
      (ASemantic.Source.InitialContext.Symbol(LName).Kind <> ppdUnknown) then
      raise Exception.Create('The fourth cohort appended a previously known or source-local symbol');
    Inc(LUnknown);
    WriteLn('profile-first symbol request / ordinal=', LAt, ' name=', LName.Name);
  end;
  WriteLn('profile-first symbol plan / conditions=', LPlan.Count,
    ' previousNames=', AProbe.Count, ' requestedNames=', LNext.Count,
    ' newInitialSymbols=', LUnknown, ' retainedSizeQueries=', LSizes.SizeCount,
    ' priorSymbolJobs=3 maximumSymbolJobs=4',
    ' totalPreparationWork=', APreparation.Receipt.Total,
    ' initialSemanticWork=', LUsedWork, ' initialSemanticPasses=', LUsedPasses);
  Flush(Output);

  if LUnknown = 0 then
  begin
    WriteLn('profile-first stop / no eligible new initial symbols; actual source/declaration prerequisites remain');
    Flush(Output);
    APreparedModule := AModule;
    AFinalPlan := LPlan;
    Result := ASemantic;
    Exit;
  end;

  if (LUnknown <> LNext.Count - AProbe.Count) or
    (LUsedWork >= LOptions.MaximumTokens) or (LUsedPasses >= LOptions.MaximumPasses) or
    (APreparation.Receipt.Total >= APreparation.Receipt.Maximum) then
    raise ENyxPascalTranslationLimit.Create('The fourth actual cohort has no complete remaining preparation/semantic allowance');

  if not CreateDirUTF8(GRequiredOutput) then
    raise Exception.Create('The explicit fresh profile-first output directory could not be created');
  LDirectory := GRequiredOutput + '/probe-4';
  WriteLn('profile-first symbol capture / admittedSymbolJob=4 maximumSymbolJobs=4',
    ' freshFactCaptures=0 replayedFactJobs=1 totalDynamicJobs=7 maximumDynamicJobs=8',
    ' directory=', LDirectory);
  Flush(Output);
  LFresh := CaptureNyxFPCCompilerCapture(GRequiredCompiler, LDirectory, LNext,
    TNyxPascalCompilerTraceOptions.Defaults, APreparation);
  LCoherent := APrevious.Merge(LFresh);

  if not LCoherent.Context.SameFacts(LFresh.Context) then
    raise Exception.Create('The fourth cumulative reply omitted or changed an earlier complete compiler fact');
  LContext := NyxPascalWithBootstrapContext(LCoherent.Context, ABootstrap);
  LRestoredRoot := NyxPascalBootstrapContext(LContext);

  if (LRestoredRoot = nil) or not LRestoredRoot.SameFacts(ABootstrap) then
    raise Exception.Create('The fourth cumulative reply changed the independent replayed compiler-root authority');
  LModule := AModule.InContext(LContext, AModule.Translation);
  WriteLn('profile-first symbol reply / admittedSymbolJob=4',
    ' completeDefaultAnchor=TRUE previousFacts=TRUE independentBootstrap=TRUE',
    ' remainingSemanticWork=', LOptions.MaximumTokens - LUsedWork,
    ' remainingSemanticPasses=', LOptions.MaximumPasses - LUsedPasses,
    ' totalPreparationWork=', APreparation.Receipt.Total);
  Flush(Output);
  LSemantic := NewNyxPascalSemanticTranslation(AFiles, LModule.Root,
    LModule.Context, AWorld, LModule.Translation,
    TNyxPascalSyntaxOptions.Defaults.UpToDeclarations(100000),
    LOptions.UpToTokens(LOptions.MaximumTokens - LUsedWork)
      .UpToPasses(LOptions.MaximumPasses - LUsedPasses));

  if (LSemantic.WorkTokens < 0) or
    (LSemantic.WorkTokens > LOptions.MaximumTokens - LUsedWork) or
    (LSemantic.Passes < 1) or (LSemantic.Passes > LOptions.MaximumPasses - LUsedPasses) then
    raise ENyxPascalTranslationLimit.Create('Profile-first SysUtils exceeded the exact remaining work/pass allowance');
  Inc(LUsedWork, LSemantic.WorkTokens);
  Inc(LUsedPasses, LSemantic.Passes);
  LView := NewNyxPascalTranslatedSyntax(LSemantic.Source,
    TNyxPascalSyntaxOptions.Defaults.UpToDeclarations(100000));
  ReportNamespaceSemantic('profile-first-after-symbols', 'SysUtils', LSemantic, LView);
  LPlan := PreparedSourcePlan(LSemantic.Source, APreparation);
  WriteLn('profile-first totals / symbolJobs=4 maximumSymbolJobs=4',
    ' totalDynamicJobs=7 maximumDynamicJobs=8 freshFactCaptures=0',
    ' preparationWork=', APreparation.Receipt.Total, ' preparationMaximum=4000000',
    ' semanticWork=', LUsedWork, ' semanticMaximum=4000000',
    ' semanticPasses=', LUsedPasses, ' semanticMaximumPasses=64',
    ' pending=', LSemantic.Pending, ' issue=', Ord(LSemantic.BindingIssue),
    ' answers=', LSemantic.Answers, ' related=', LSemantic.RelatedName);
  Flush(Output);
  { There is no fifth job or third construction. Retain the exact final plan
    for diagnostics and let the unchanged whole-source assertion decide. }
  APreparedModule := LModule;
  AFinalPlan := LPlan;
  Result := LSemantic;
end;

{ A prepared profile removes already accepted symbol-discovery rounds, not any
  actual namespace/type requirement. Only the single current bootstrap primitive
  request is eligible here; later frontiers remain ordinary completeness failures. }
function ContinuePreparedRoot(const AFiles: INyxPascalFileCatalog;
  const APreviousWorld: INyxPascalSemanticWorld;
  const AModules: INyxPascalModuleCatalog;
  const AModule: INyxPascalModuleSource;
  const ASemantic: INyxPascalSemanticTranslation;
  const AProbe: INyxPascalCompilerProbe;
  const ABootstrap: INyxFPCCompilerCapture; APriorFactJobs: Integer;
  const APreparation: INyxPascalAnalysisBudget;
  out AUpdatedModules: INyxPascalModuleCatalog;
  out AUpdatedWorld: INyxPascalSemanticWorld;
  out AUpdatedRoot: INyxFPCCompilerCapture;
  out AUpdatedModule: INyxPascalModuleSource;
  out AFinalPlan: INyxPascalCompilerPlan): INyxPascalSemanticTranslation;
var
  LPlan: INyxPascalCompilerPlan;
  LNext: INyxPascalCompilerProbe;
  LPreviousSizes, LSizes: INyxPascalSizeProbe;
  LRequirements: INyxPascalCompilerRequirements;
  LRequirement: INyxPascalCompilerRequirement;
  LCaptures: INyxPascalCompilerCaptures;
  LFactPlan: INyxPascalCompilerFulfillmentPlan;
  LApplied: INyxPascalCompilerFulfillment;
  LModule: INyxPascalModuleSource;
  LModules: INyxPascalModuleCatalog;
  LWorld: INyxPascalSemanticWorld;
  LRoot: INyxFPCCompilerCapture;
  LSemantic: INyxPascalSemanticTranslation;
  LView: INyxPascalTranslatedSyntax;
  LOptions: TNyxPascalConditionalOptions;
  LAt, LOwner, LUsedWork, LUsedPasses: Integer;
begin
  Result := nil;
  AUpdatedModules := nil;
  AUpdatedWorld := nil;
  AUpdatedRoot := nil;
  AUpdatedModule := nil;
  AFinalPlan := nil;
  LOptions := TNyxPascalConditionalOptions.Defaults;
  LUsedWork := ASemantic.WorkTokens;
  LUsedPasses := ASemantic.Passes;
  LView := NewNyxPascalTranslatedSyntax(ASemantic.Source,
    TNyxPascalSyntaxOptions.Defaults.UpToDeclarations(100000));
  ReportNamespaceSemantic('prepared-initial', 'SysUtils', ASemantic, LView);

  if (ASemantic.Source.DiagnosticCount <> 0) or (LView.Syntax.DiagnosticCount <> 0) then
  begin
    { A genuine pending condition reports its uncertainty before its requested
      compiler fact exists. Preserve every source/syntax diagnostic here; none
      grants authority or disappears from the final completeness assertion.
      The current-site typed requirement below, followed by generic ownership
      preflight, is the sole admission to this one bounded read-only fact job.
      An independent defect may still refuse the final complete source package. }
    ReportSemanticFrontier(ASemantic, LView, nil, False);
  end;
  LPlan := PreparedSourcePlan(ASemantic.Source, APreparation);
  LNext := LPlan.CompilerProbe(AProbe);

  if (LNext.Count <> AProbe.Count) or
    not Supports(AProbe, INyxPascalSizeProbe, LPreviousSizes) or
    not Supports(LNext, INyxPascalSizeProbe, LSizes) or
    (LSizes.SizeCount <> LPreviousSizes.SizeCount) then
  begin
    ReportSemanticFrontier(ASemantic, LView, LPlan, False);
    raise Exception.Create('Prepared semantic source requests another symbol/size cohort; no unreviewed continuation job is admitted');
  end;
  for LAt := 0 to AProbe.Count - 1 do

    if LNext.Symbol(LAt).Name <> AProbe.Symbol(LAt).Name then
      raise Exception.Create('Prepared semantic planning changed an accepted ordered symbol');
  for LAt := 0 to LPreviousSizes.SizeCount - 1 do

    if LSizes.SizeName(LAt).Name <> LPreviousSizes.SizeName(LAt).Name then
      raise Exception.Create('Prepared semantic planning changed an accepted ordered size query');
  LRequirements := CaptureNyxPascalRequirements(ASemantic);

  if not ASemantic.Pending or (ASemantic.BindingIssue <> pbiCompilerContext) or
    (LRequirements.RequirementCount <> 1) then
  begin
    ReportSemanticFrontier(ASemantic, LView, LPlan, False);
    raise Exception.Create('Prepared SysUtils must expose its one actual typed primitive requirement before collection');
  end;
  LRequirement := LRequirements.RequirementAt(0);

  if (LRequirement.Kind <> pcrPrimitiveSize) or
    (LRequirement.Reference.Name <> 'longword') or
    (NyxPascalCompilerScope(LRequirement.Context) <> pcsoSystemBootstrap) or
    not LRequirement.Context.SameFacts(ABootstrap.Context) or
    not LRequirement.OwnerSource.Source.Files.SameFiles(
      APreviousWorld.ModuleAt(APreviousWorld.FindModule('System')).Source.Files) then
    raise Exception.Create('The prepared frontier changed its genuine Longword/bootstrap/original-System source requirement');

  if (APriorFactJobs <> 6) or (APriorFactJobs >= 8) or
    (LUsedWork < 0) or (LUsedWork >= LOptions.MaximumTokens) or
    (LUsedPasses < 1) or (LUsedPasses >= LOptions.MaximumPasses) or
    (APreparation.Receipt.Total >= APreparation.Receipt.Maximum) then
    raise ENyxPascalTranslationLimit.Create('The actual primitive continuation has no remaining original fact/work/pass allowance');
  LModules := AModules.WithModule(AModule);
  LCaptures := NewNyxPascalCompilerCaptures.WithCapture(ABootstrap);
  LFactPlan := PlanNyxPascalCompilerFulfillment(LRequirement.OwnerSource.Source.Files,
    LModules, ASemantic, LCaptures, 1, APreparation);

  if (LFactPlan.Count <> 1) or
    (LFactPlan.RequirementAt(0).Reference.Name <> LRequirement.Reference.Name) then
    raise Exception.Create('The generic fulfillment plan must preserve the one exact actual requirement');
  WriteLn('prepared primitive admission / reference=', LRequirement.Reference.Name,
    ' kind=', Ord(LRequirement.Kind),
    ' scope=', Ord(NyxPascalCompilerScope(LRequirement.Context)),
    ' family=', Ord(LRequirement.Context.Family),
    ' version=', LRequirement.Context.Version.FullNumber,
    ' mode=', Ord(LRequirement.Context.Mode),
    ' owner=', LRequirement.OwnerSource.Source.Files.FileAt(
      LRequirement.OwnerSource.Source.Occurrence(0).FileIndex).Name,
    ' observer=', LRequirement.Source.Source.Files.FileAt(
      LRequirement.Source.Source.Occurrence(0).FileIndex).Name,
    ' sourceOffset=', LRequirement.SourceOffset, ' originIndex=', LRequirement.OriginIndex,
    ' ownerFiles=', LRequirement.OwnerSource.Source.Files.Count,
    ' priorDynamicJobs=', APriorFactJobs, ' maximumDynamicJobs=8 admittedNewJobs=1',
    ' symbolJobs=3 maximumSymbolJobs=4 preparationWork=', APreparation.Receipt.Total,
    ' remainingSemanticWork=', LOptions.MaximumTokens - LUsedWork,
    ' remainingSemanticPasses=', LOptions.MaximumPasses - LUsedPasses);
  Flush(Output);
  LApplied := FulfillNyxFPCCompilerRequirements(GRequiredCompiler, GRequiredOutput,
    LRequirement.OwnerSource.Source.Files, LModules, ASemantic, LCaptures, 1,
    TNyxPascalCompilerTraceOptions.Defaults, APreparation);

  if (LApplied.Jobs <> 1) or (APriorFactJobs + LApplied.Jobs > 8) then
    raise Exception.Create('Actual primitive fulfillment changed its admitted single-job count');
  LModules := LApplied.Modules;
  LModule := LModules.Entry(LModules.Find(AModule.Name));
  LOwner := LApplied.Captures.Find(NyxPascalBootstrapContext(LModule.Context));

  if LOwner < 0 then
    raise Exception.Create('Actual primitive fulfillment lost the exact updated independent root capture');
  LRoot := LApplied.Captures.CaptureAt(LOwner);
  LWorld := RebuildRootWorld(APreviousWorld, LModules, LRoot);
  LSemantic := NewNyxPascalSemanticTranslation(AFiles, LModule.Root,
    LModule.Context, LWorld, LModule.Translation,
    TNyxPascalSyntaxOptions.Defaults.UpToDeclarations(100000),
    LOptions.UpToTokens(LOptions.MaximumTokens - LUsedWork)
      .UpToPasses(LOptions.MaximumPasses - LUsedPasses));

  if (LSemantic.WorkTokens < 0) or
    (LSemantic.WorkTokens > LOptions.MaximumTokens - LUsedWork) or
    (LSemantic.Passes < 1) or
    (LSemantic.Passes > LOptions.MaximumPasses - LUsedPasses) then
    raise ENyxPascalTranslationLimit.Create('The prepared SysUtils continuation exceeded its shared work/pass remainder');
  Inc(LUsedWork, LSemantic.WorkTokens);
  Inc(LUsedPasses, LSemantic.Passes);
  LView := NewNyxPascalTranslatedSyntax(LSemantic.Source,
    TNyxPascalSyntaxOptions.Defaults.UpToDeclarations(100000));
  ReportNamespaceSemantic('updated-root', 'SysUtils', LSemantic, LView);
  AFinalPlan := PreparedSourcePlan(LSemantic.Source, APreparation);
  WriteLn('prepared continuation totals / symbolJobs=3 maximumSymbolJobs=4',
    ' priorDynamicJobs=', APriorFactJobs, ' newDynamicJobs=', LApplied.Jobs,
    ' totalDynamicJobs=', APriorFactJobs + LApplied.Jobs, ' maximumDynamicJobs=8',
    ' preparationWork=', APreparation.Receipt.Total, ' preparationMaximum=4000000',
    ' semanticWork=', LUsedWork, ' semanticMaximum=4000000',
    ' semanticPasses=', LUsedPasses, ' semanticMaximumPasses=64',
    ' pending=', LSemantic.Pending, ' issue=', Ord(LSemantic.BindingIssue),
    ' answers=', LSemantic.Answers, ' related=', LSemantic.RelatedName);
  Flush(Output);
  AUpdatedModules := LModules;
  AUpdatedWorld := LWorld;
  AUpdatedRoot := LRoot;
  AUpdatedModule := LModule;
  Result := LSemantic;
end;

procedure ReplayDependencies(AResult: TRetainedRTL);
var
  LWindows, LAll: INyxPascalDirectoryCapture;
  LFiles: INyxPascalFileCatalog;
  LProbe, LPreparedProbe: INyxPascalCompilerProbe;
  LCapture, LPreparedCapture, LUpdatedRoot: INyxFPCCompilerCapture;
  LContext: INyxPascalCompilerContext;
  LModule, LPreparedModule: INyxPascalModuleSource;
  LUpdatedModules: INyxPascalModuleCatalog;
  LUpdatedWorld, LSemanticWorld: INyxPascalSemanticWorld;
  LSource, LOriginal: INyxPascalTranslation;
  LSemantic, LContinuedSemantic: INyxPascalSemanticTranslation;
  LView: INyxPascalTranslatedSyntax;
  LPlan, LContinuationPlan: INyxPascalCompilerPlan;
  LPriorPlanWork: INyxPascalCompilerCandidates;
  LPreparation: INyxPascalAnalysisBudget;
  LLexical: INyxPascalTranslationLexicalWork;
  LBytes: INyxPascalByteFileCatalog;
  LAt, LIndex, LOpaque: Integer;
begin
  LWindows := ReadManifestInputs(GVerifiedInputs, GVerifiedInputs + '/windows28-paths.txt', 28);
  LProbe := WindowsProbe(LWindows.Files);
  Check(LProbe.Count = 47, 'The complete exact Windows28 source set rebuilds its retained47 typed symbol requests');
  LCapture := ReadProbe(GWindowsRuntime, LProbe);
  LFiles := LWindows.Files.WithIncludeDirectory('rtl/win/wininc').WithIncludeDirectory('rtl/inc');
  LModule := LWindows.Modules.Entry(LWindows.Modules.Find(NyxPascalModule('Windows')));
  LContext := NyxPascalWithBootstrapContext(LCapture.Context, AResult.Bootstrap.Context);
  Check(LContext.Feature(ppfNestedComments) <> ppkUnknown,
    'The retained Windows context must own an explicit initial comment policy');
  LModule := LModule.InContext(LContext, TNyxPascalTranslationOptions.Defaults.UpToTokens(2000000)
    .Lexical(TNyxPascalLexicalOptions.Defaults.NestedComments(
      LContext.Feature(ppfNestedComments) = ppkTrue)));
  LSource := NewNyxPascalTranslation(LFiles, LModule.Root, LModule.Context, LModule.Translation);
  LView := NewNyxPascalTranslatedSyntax(LSource, TNyxPascalSyntaxOptions.Defaults.UpToDeclarations(100000));
  Check(LSource.Resolved and LView.CompleteTranslation and (LView.Syntax.DiagnosticCount = 0),
    'The actual Windows namespace is complete from its original checked includes, not a fabricated empty import');

  if LView.Syntax.DeclarationCount <> 29823 then
    raise Exception.Create('The original complete Windows namespace changed its29823 declaration count');
  WriteLn('restored Windows / declarations=', LView.Syntax.DeclarationCount);
  WriteLn('namespace translation / stage=original name=Windows files=', LFiles.Count,
    ' translatedTokens=', LSource.Count,
    ' semanticWork=not-applicable passes=not-applicable answers=not-applicable');

  if Supports(LSource, INyxPascalTranslationLexicalWork, LLexical) then
    WriteLn('namespace lexical / stage=original name=Windows scanned=',
      LLexical.ScannedLexicalTokens, ' reused=', LLexical.ReusedLexicalTokens,
      ' candidates=', LLexical.LexicalCandidateReads);
  Flush(Output);
  AResult.World := AResult.World.WithModule(LView);
  AResult.Modules := AResult.Modules.WithModule(LModule);
  LAll := ReadManifestInputs(GVerifiedInputs, GVerifiedInputs + '/all216-paths.txt', 216);
  Check((LAll.Modules.Count = 47) and Supports(LAll.Files, INyxPascalByteFileCatalog, LBytes),
    'The complete216 sources preserve all47 roots and original bytes');
  LOpaque := 0;
  for LAt := 0 to LAll.Files.Count - 1 do
  begin

    if LBytes.SourceViewAt(LAt) <> nil then
      Continue;
    Inc(LOpaque);
    Check((LAll.Files.FileAt(LAt).Name = 'rtl/objpas/sysutils/strg.inc') and
      (LBytes.SourceBytesAt(LAt).Count = 1239) and (LBytes.SourceBytesAt(LAt).ByteAt($39F) = $81),
      'The retained opaque legacy include keeps its exact bytes and never gains guessed text');
  end;
  Check(LOpaque = 1, 'All216 original sources retain exactly the previously established opaque include');
  LFiles := LAll.Files.WithIncludeDirectory('rtl/i386').WithIncludeDirectory('rtl/win')
    .WithIncludeDirectory('rtl/inc').WithIncludeDirectory('rtl/win32')
    .WithIncludeDirectory('rtl/objpas/sysutils');
  LIndex := LAll.Modules.Find(NyxPascalModule('SysUtils'));
  Check(LIndex >= 0, 'The genuine SysUtils root belongs to its captured unit header');
  LModule := LAll.Modules.Entry(LIndex);
  LCapture := ReadProbe(GProfileRuntime + '/delphi', ProfileProbe('ACTUAL_DELPHI'));
  { Rebuild the original conditional request before attaching separately restored
    compiler-root facts. Its exact emitted input must still match the archive. }
  LOriginal := NewNyxPascalTranslation(LFiles, LModule.Root, LCapture.Context,
    TNyxPascalTranslationOptions.Defaults);
  LPreparation := nil;

  if GPreparedReplay <> '' then
    LPreparation := NewNyxPascalAnalysisBudget(4000000);
  try

    if LPreparation <> nil then
      LPlan := PreparedSourcePlan(LOriginal, LPreparation)
    else
      LPlan := PlanNyxPascalCompilerRequirements(LOriginal, TNyxPascalCompilerPlanOptions.Defaults);
    LProbe := LPlan.CompilerProbe(ProfileProbe('RTL_REQUIREMENTS'));
    Check(LProbe.Count = 33, 'The real SysUtils continuation rebuilds the exact retained33-name request without guessed defines');
    LCapture := ReadProbe(GProfileRuntime + '/rtl-required', LProbe);

    if LPreparation <> nil then
    begin
      { Restore the archived33 and exact cumulative35/36 replies first. Optional
        prepared-import mode also restores the accepted fourth57-name reply
        before any SysUtils construction. No captured context is retagged and
        no accepted query is collected again. }
      LPreparedCapture := ReadPreparedProfile(GSymbolReplay, LProbe, LCapture, 35,
        LPreparation, LPreparedProbe);
      LCapture := LPreparedCapture;
      LProbe := LPreparedProbe;
      LPreparedCapture := ReadPreparedProfile(GPreparedReplay, LProbe, LCapture, 36,
        LPreparation, LPreparedProbe);
      LCapture := LPreparedCapture;
      LProbe := LPreparedProbe;

      if GFinalProfile <> '' then
      begin
        LPreparedCapture := ReadPreparedProfile(GFinalProfile, LProbe, LCapture, 57,
          LPreparation, LPreparedProbe);
        LCapture := LPreparedCapture;
        LProbe := LPreparedProbe;
        WriteLn('prepared symbol inputs / archivedJobs=1 replayedJobs=3 totalJobs=4',
          ' maximumJobs=4 freshSymbolCaptures=0 preparationWork=', LPreparation.Receipt.Total);
      end
      else
        WriteLn('prepared symbol inputs / archivedJobs=1 replayedJobs=2 totalJobs=3',
          ' maximumJobs=4 freshSymbolCaptures=0 preparationWork=', LPreparation.Receipt.Total);
      Flush(Output);
    end;
    LSemanticWorld := AResult.World;

    if GPrimitiveReplay <> '' then
    begin
      ReplayPreparedPrimitive(AResult.World, AResult.Modules, AResult.Bootstrap,
        AResult.FactJobs, LPreparation, LUpdatedModules, LUpdatedWorld, LUpdatedRoot);
      LSemanticWorld := LUpdatedWorld;
      LContext := NyxPascalWithBootstrapContext(LCapture.Context, LUpdatedRoot.Context);
    end
    else
      LContext := NyxPascalWithBootstrapContext(LCapture.Context, AResult.Bootstrap.Context);

    if GFinalProfile <> '' then
    begin
      PrepareImportedNamespaces(LFiles, LUpdatedModules, LUpdatedWorld,
        LContext, LPreparation);
      LSemanticWorld := LUpdatedWorld;
    end;
    Check(LContext.Feature(ppfNestedComments) <> ppkUnknown,
      'The retained SysUtils context must own an explicit initial comment policy');
    LModule := LModule.InContext(LContext, TNyxPascalTranslationOptions.Defaults.UpToTokens(2000000)
      .Lexical(TNyxPascalLexicalOptions.Defaults.NestedComments(
        LContext.Feature(ppfNestedComments) = ppkTrue)));
    LSemantic := NewNyxPascalSemanticTranslation(LFiles, LModule.Root, LContext, LSemanticWorld,
      LModule.Translation, TNyxPascalSyntaxOptions.Defaults.UpToDeclarations(100000),
      TNyxPascalConditionalOptions.Defaults.UpToTokens(4000000));
    LContinuationPlan := nil;

    if (GPreparedReplay = '') and (GSymbolReplay <> '') and not LSemantic.Pending then
      raise Exception.Create('The explicit retained continuation reply has no pending current semantic source to match');

    if GFinalProfile <> '' then
    begin
      { All four accepted symbol jobs are already inputs. Report the one real
        construction without invoking any collection or continuation path. }
      LUpdatedModules := LUpdatedModules.WithModule(LModule);
      LContinuationPlan := PreparedSourcePlan(LSemantic.Source, LPreparation);
      LView := NewNyxPascalTranslatedSyntax(LSemantic.Source,
        TNyxPascalSyntaxOptions.Defaults.UpToDeclarations(100000));
      ReportNamespaceSemantic('prepared-imports', 'SysUtils', LSemantic, LView);
      WriteLn('prepared imports totals / symbolJobs=4 maximumSymbolJobs=4',
        ' totalDynamicJobs=7 maximumDynamicJobs=8 freshFactCaptures=0 freshSymbolCaptures=0',
        ' preparationWork=', LPreparation.Receipt.Total, ' preparationMaximum=4000000',
        ' semanticWork=', LSemantic.WorkTokens, ' semanticMaximum=4000000',
        ' semanticPasses=', LSemantic.Passes, ' semanticMaximumPasses=64');
      Flush(Output);
    end
    else if GPrimitiveReplay <> '' then
    begin
      LUpdatedModules := LUpdatedModules.WithModule(LModule);
      LContinuedSemantic := ContinueProfileFirstSymbols(LFiles, LUpdatedWorld,
        LModule, LSemantic, LProbe, LCapture, LUpdatedRoot.Context, LPreparation,
        LPreparedModule, LContinuationPlan);
      LSemantic := LContinuedSemantic;
      LModule := LPreparedModule;
      LContext := LModule.Context;
      LUpdatedModules := LUpdatedModules.WithModule(LModule);
    end
    else if LPreparation <> nil then
    begin
      LContinuedSemantic := ContinuePreparedRoot(LFiles, AResult.World, AResult.Modules,
        LModule, LSemantic, LProbe, AResult.Bootstrap, AResult.FactJobs, LPreparation,
        LUpdatedModules, LUpdatedWorld, LUpdatedRoot, LPreparedModule, LContinuationPlan);
      LSemantic := LContinuedSemantic;
      LModule := LPreparedModule;
      LContext := LModule.Context;
    end
    else if (GRequiredCompiler <> '') and LSemantic.Pending then
    begin

      if not Supports(LPlan, INyxPascalCompilerCandidates, LPriorPlanWork) then
        raise Exception.Create('The archived33-name continuation has no exact preparation work receipt');
      LContinuedSemantic := ContinueSemanticSymbols(LFiles, AResult.World, LModule, LSemantic,
        LProbe, LCapture, LPriorPlanWork.WorkUnits, LPreparedModule, LContinuationPlan);
      LSemantic := LContinuedSemantic;
      LModule := LPreparedModule;
      LContext := LModule.Context;
    end;
    LView := NewNyxPascalTranslatedSyntax(LSemantic.Source,
      TNyxPascalSyntaxOptions.Defaults.UpToDeclarations(100000));
    WriteLn('restored SysUtils / declarations=', LView.Syntax.DeclarationCount,
      ' pending=', LSemantic.Pending, ' related=', LSemantic.RelatedName);
    Flush(Output);

    if LSemantic.Pending or not LView.CompleteTranslation or
      (LView.Syntax.DiagnosticCount <> 0) then
      ReportSemanticFrontier(LSemantic, LView, LContinuationPlan, GRequiredCompiler = '');
    Check(not LSemantic.Pending and LView.CompleteTranslation and (LView.Syntax.DiagnosticCount = 0),
      'The real SysUtils dependency must resolve all actual source/context/import prerequisites before package extraction');

    if LPreparation <> nil then
    begin
      { Publish the requalified namespaces/root only after the unchanged complete
        SysUtils assertion; failed preparation leaves the prior owner readable. }
      AResult.Modules := LUpdatedModules;
      AResult.World := LUpdatedWorld;
      AResult.Bootstrap := LUpdatedRoot;
      Inc(AResult.FactJobs);
    end;
    AResult.Files := LFiles;
    AResult.Modules := AResult.Modules.WithModule(LModule);
    AResult.World := AResult.World.WithModule(LView);
    AResult.LocalContext := LContext;
  finally

    if LPreparation <> nil then
    begin
      { A failed real unit must retain the actual preparation cost too. This
        detached read neither replans source nor grants another allowance. }
      try
        WriteLn('prepared final receipt / work=', LPreparation.Receipt.Total,
          ' maximum=', LPreparation.Receipt.Maximum,
          ' final57Input=', GFinalProfile <> '');
        Flush(Output);
      finally
        LPreparation.Retire;
      end;
    end;
  end;
end;

procedure TProductSourceWorld.Load(const AName: TNyxText);
var
  LAt, LModuleIndex, LBoundary, LCount: Integer;
  LModule: INyxPascalModuleSource;
  LContext: INyxPascalCompilerContext;
  LSemantic: INyxPascalSemanticTranslation;
  LView: INyxPascalTranslatedSyntax;
  LImport: TNyxPascalSyntaxImport;
  LOptions: TNyxPascalTranslationOptions;
  LKey: TNyxText;
begin
  LKey := NyxPascalASCIIKey(AName);
  for LAt := 0 to Length(FLoaded) - 1 do

    if FLoaded[LAt] = LKey then
      Exit;
  for LAt := 0 to Length(FLoading) - 1 do

    if FLoading[LAt] = LKey then
      Exit;

  if FDepth >= World.Options.MaximumDepth then
    raise Exception.Create('Actual product import closure exceeds its existing binding depth: ' + AName);
  LModuleIndex := World.FindModule(AName);
  LAt := Modules.Find(NyxPascalModule(AName));

  if LAt < 0 then
    raise Exception.Create('Actual product import has no captured header/root association: ' + AName);
  LModule := Modules.Entry(LAt);
  LContext := LModule.Context;

  if LContext = nil then
    LContext := LocalContext;

  if (LContext = nil) or (LContext.Feature(ppfNestedComments) = ppkUnknown) then
    raise Exception.Create('Actual product source lacks its initial captured comment policy: ' + AName);
  LOptions := LModule.Translation.Lexical(TNyxPascalLexicalOptions.Defaults
    .NestedComments(LContext.Feature(ppfNestedComments) = ppkTrue));
  LCount := Length(FLoading);
  SetLength(FLoading, LCount + 1);
  FLoading[LCount] := LKey;
  Inc(FDepth);
  try

    if LModuleIndex >= 0 then
      LView := World.ModuleAt(LModuleIndex)
    else
    begin
      LSemantic := NewNyxPascalSemanticTranslation(Files, LModule.Root,
        LContext, World, LOptions, TNyxPascalSyntaxOptions.Defaults,
        TNyxPascalConditionalOptions.Defaults);
      LView := NewNyxPascalTranslatedSyntax(LSemantic.Source, TNyxPascalSyntaxOptions.Defaults);

      if LSemantic.Pending or not LView.CompleteTranslation or
        not LView.Source.Resolved or (LView.Source.DiagnosticCount <> 0) or
        (LView.Syntax.DiagnosticCount <> 0) then
        raise Exception.Create('Actual product source is incomplete: ' + AName +
          ' related=' + LSemantic.RelatedName + ' source diagnostics=' +
          IntToStr(LView.Source.DiagnosticCount) + ' syntax diagnostics=' +
          IntToStr(LView.Syntax.DiagnosticCount));
      World := World.WithModule(LView);
    end;
    LModuleIndex := World.FindModule(AName);

    if LModuleIndex < 0 then
      raise Exception.Create('Actual translated module header disagrees with its captured source association');
    LBoundary := World.InterfaceBoundary(LModuleIndex);
    for LAt := 0 to LView.Syntax.ImportCount - 1 do
    begin
      LImport := LView.Syntax.ImportAt(LAt);

      if (LImport.Scope <> 0) or (LImport.Span.Start >= LBoundary) then
        Continue;

      if LImport.Conditional then
        raise Exception.Create('An actual public source import still has uncertain participation: ' + LImport.Name);
      Load(LImport.Name);
    end;
    LAt := Length(FLoaded);
    SetLength(FLoaded, LAt + 1);
    FLoaded[LAt] := LKey;
  finally
    Dec(FDepth);
    SetLength(FLoading, LCount);
  end;
end;

procedure ActualSourcePackage(const ARepository: TNyxText; ARTL: TRetainedRTL);
var
  LSource: TProductSourceWorld;
  LCaptured: INyxPascalDirectoryCapture;
  LScope: INyxDocumentationTypeConstruction;
  LScopes: TNyxDocumentationSourceScopes;
  LCreators: TNyxDocumentationSourceCreators;
  LPackage: INyxDocumentationPackage;
  LPlain, LCatalog, LSecondCatalog: TNyxCatalog;
  LRegistry: TNyxDocumentationRegistry;
  LBaseline, LIndex, LPrevious: INyxDocumentationIndex;
  LGeneration: INyxDocumentationContributionGeneration;
  LSnapshot: INyxCatalogSnapshot;
  LCatalogDocs: INyxCatalogDocumentation;
  LBinding: INyxDocumentationContributionBinding;
  LRegistration: INyxDocumentationRegistration;
  LControls, LFixture, LAt, LCount, LDocumented: Integer;
  LCancellation: INyxCancellationScope;
  LFailed: Boolean;
  function FixtureType(const AName: TNyxText): Integer;
  begin
    Result := LSource.World.ModuleAt(LFixture).Syntax.Named(0, AName);

    if Result < 0 then
      raise Exception.Create('The actual complete fixture source is missing a selected declared type: ' + AName);
  end;
  procedure Unexecuted;
  begin
    Check((DocumentationControlsConstructed = 0) and
      (DocumentationControlsDestroyed = 0) and
      (DocumentationCallbacksConstructed = 0) and
      (DocumentationCallbacksInvoked = 0) and
      (DocumentationGlobalFactoryCalls = 0) and
      (DocumentationGlobalRoutineCalls = 0),
      'Actual source capture, registration and index inspection execute no fixture control/callback/global body');
  end;
begin
  LSource := TProductSourceWorld.Create;
  LPlain := TNyxCatalog.Create;
  LRegistry := TNyxDocumentationRegistry.Create;
  LCatalog := nil;
  LSecondCatalog := nil;
  try
    LBaseline := LRegistry.Refresh(CaptureNyxSchemas, CaptureNyxCallbacks, LPlain.Capture, 0);
    Unexecuted;
    try
      LSource.Files := ARTL.Files;
      LSource.Modules := ARTL.Modules;
      LSource.World := ARTL.World;
      { This explicit prospective compilation input is separate from the
        original archived/bootstrap context. Root's actual native invocation
        must carry -uPAS2JS; no default or missing compiler fact is inferred. }
      LSource.LocalContext := ARTL.LocalContext.WithSymbol(NyxPascalDefine('PAS2JS'), ppdUndefined);
      LCaptured := CaptureNyxPascalSourceDirectory(ARepository + '/src',
        LSource.Files, LSource.Modules,
        TNyxPascalDirectoryOptions.Defaults.Under('src'), LSource.LocalContext);
      LCaptured := CaptureNyxPascalSourceDirectory(ARepository + '/tests',
        LCaptured.Files, LCaptured.Modules,
        TNyxPascalDirectoryOptions.Defaults.Under('tests').OnlyFile('nyx.test.documentation.pas'),
        LSource.LocalContext);
      Check(LCaptured.UnindexedCount = 0,
        'Every selected product unit retains its real bounded header association');
      LSource.Files := LCaptured.Files.WithIncludeDirectory('src');
      LSource.Modules := LCaptured.Modules;
      LSource.Load('nyx.controls');
      LSource.Load('nyx.test.documentation');
      LControls := LSource.World.FindModule('nyx.controls');
      LFixture := LSource.World.FindModule('nyx.test.documentation');
      Check((LControls >= 0) and (LFixture >= 0),
        'Actual complete product and extension sources share one genuine imported world');
      SetLength(LScopes, 5);
      LScopes[0].ModuleIndex := LControls;
      LScopes[0].DeclarationIndex := -1;
      LScopes[1].ModuleIndex := LFixture;
      LScopes[1].DeclarationIndex := -1;
      LScopes[2].ModuleIndex := LFixture;
      LScopes[2].DeclarationIndex := FixtureType('IDocumentationBadge');
      LScopes[3].ModuleIndex := LFixture;
      LScopes[3].DeclarationIndex := FixtureType('TDocumentationBadge');
      LScopes[4].ModuleIndex := LFixture;
      LScopes[4].DeclarationIndex := FixtureType('TDocumentationCallback');
      LScope := NewNyxDocumentationTypeConstruction(TNyxDocumentationTypeExpressionLimits.Defaults);
      try
        LCreators := NyxControlsDocumentationCreators(LSource.World, LControls, LScope);
        Check(Length(LCreators) = Ord(High(TNyxKind)) - Ord(Low(TNyxKind)) + 1,
          'The actual generated creator owner annotates every built-in kind');
        LCount := Length(LCreators);
        LScope.Spend(LCount + 1);
        CheckNyxDocumentationTypeConstruction(LScope);
        SetLength(LCreators, LCount + 1);
        LCreators[LCount].ModuleIndex := LFixture;
        LCreators[LCount].DeclarationIndex :=
          LSource.World.ModuleAt(LFixture).Syntax.Named(0, 'NewDocumentationBadge');
        LPackage := CaptureNyxDocumentationSourcePackage(LSource.World,
          LScopes, LCreators, [ndplatformNative], LScope);
      finally
        LScope.Retire;
      end;
      Check((LPackage.FindDeclaration(NyxDocumentationUnit('nyx.controls')) <> nil) and
        (LPackage.FindDeclaration(NyxDocumentationUnit('nyx.test.documentation')) <> nil),
        'The canonical package owns both complete selected unit-global declarations');
      LCatalog := TNyxCatalog.Create(NewNyxControlsDocumentationSource(LPackage));
      RegisterDocumentationFixture(LCatalog, LPackage);
      LIndex := LRegistry.Refresh(CaptureNyxSchemas, CaptureNyxCallbacks,
        LCatalog.Capture, LBaseline.Version);
      Check(Supports(LIndex, INyxDocumentationContributionGeneration, LGeneration) and
        (LGeneration.PackageCount = 1) and SameNyxDocumentationPackage(
          LGeneration.PackageAt(0), LPackage),
        'Real schema/callback/catalog discovery retains one exact canonical source package');
      LSnapshot := LCatalog.Capture;
      Check(Supports(LSnapshot, INyxCatalogDocumentation, LCatalogDocs),
        'The actual catalog snapshot exposes optional source-owned declaration attachments');
      LDocumented := 0;
      for LAt := 0 to LSnapshot.Count - 1 do
      begin
        LRegistration := LCatalogDocs.DocumentationAt(LAt);

        if LRegistration = nil then
          Continue;
        Check(Supports(LRegistration, INyxDocumentationContributionBinding, LBinding) and
          SameNyxDocumentationPackage(LBinding.PackageOwner, LPackage),
          'Every real built-in/fixture creator binding reuses the exact package owner');
        Inc(LDocumented);
      end;
      Check(LDocumented = Length(LCreators),
        'All81 actual built-in creators and the real extension factory are attached');
      LSecondCatalog := TNyxCatalog.Create(NewNyxControlsDocumentationSource(LPackage));
      RegisterDocumentationFixture(LSecondCatalog);
      Check(LSecondCatalog.IndexOf(DocumentationFixtureKind) >= 0,
        'The original fixture registration call reuses its installed source package for another ordinary catalog');
      LPrevious := LIndex;
      LCancellation := NewNyxCancellationScope;
      LCancellation.Cancel;
      LFailed := False;
      try
        LRegistry.Refresh(CaptureNyxSchemas, CaptureNyxCallbacks,
          LCatalog.Capture, LIndex.Version, LCancellation);
      except
        on ENyxDocumentationCancelled do LFailed := True;
      end;
      Check(LFailed and (LRegistry.Current = LPrevious) and
        (LRegistry.Version = LPrevious.Version),
        'Cancelled real index publication retains exact prior Current and Version');
      Unexecuted;
      WriteLn('actual source package / declarations=', LPackage.DeclarationCount,
        ' chunks=', LPackage.ChunkCount, ' creators=', Length(LCreators),
        ' index=', LIndex.DeclarationCount);
      Flush(Output);
    except
      on E: Exception do
      begin

        if LIndex = nil then
          Check((LRegistry.Current = LBaseline) and (LRegistry.Version = LBaseline.Version),
            'An unavailable genuine source/type/namespace prerequisite preserves the prior valid index');
        Unexecuted;
        raise;
      end;
    end;
  finally
    LSecondCatalog.Free;
    LCatalog.Free;
    LRegistry.Free;
    LPlain.Free;
    LSource.Free;
  end;
end;

procedure RequireArguments(out ARepository, ARuntime: TNyxText);
begin

  if (ParamCount <> 5) and (ParamCount <> 7) and (ParamCount <> 8) and
    (ParamCount <> 9) and (ParamCount <> 10) and (ParamCount <> 12) then
    raise Exception.Create('Supply repository, accepted RTL runtime, verified216 inputs, retained profile runtime and retained Windows compiler directory; optional compiler and fresh-output arguments must be supplied together, followed optionally by one retained continuation directory, both exact prepared35/36 reply directories, those two plus the accepted primitive reply directory, or that complete form plus the accepted57 profile and explicit import-input directory');
  ARepository := ExcludeTrailingPathDelimiter(ExpandFileNameUTF8(ParamStr(1)));
  ARuntime := ExcludeTrailingPathDelimiter(ExpandFileNameUTF8(ParamStr(2)));
  GVerifiedInputs := ExcludeTrailingPathDelimiter(ExpandFileNameUTF8(ParamStr(3)));
  GProfileRuntime := ExcludeTrailingPathDelimiter(ExpandFileNameUTF8(ParamStr(4)));
  GWindowsRuntime := ExcludeTrailingPathDelimiter(ExpandFileNameUTF8(ParamStr(5)));
  GRequiredCompiler := '';
  GRequiredOutput := '';
  GSymbolReplay := '';
  GPreparedReplay := '';
  GPrimitiveReplay := '';
  GFinalProfile := '';
  GImportInputs := '';

  if ParamCount >= 7 then
  begin
    GRequiredCompiler := ExpandFileNameUTF8(ParamStr(6));
    GRequiredOutput := ExcludeTrailingPathDelimiter(ExpandFileNameUTF8(ParamStr(7)));

    if not FileExistsUTF8(GRequiredCompiler) or FileExistsUTF8(GRequiredOutput) or
      DirectoryExistsUTF8(GRequiredOutput) or
      not DirectoryExistsUTF8(ExtractFileDir(GRequiredOutput)) then
      raise Exception.Create('Optional required-symbol collection needs an explicit compiler and a fresh directory with an existing parent');
  end;

  if ParamCount >= 8 then
  begin
    GSymbolReplay := ExcludeTrailingPathDelimiter(ExpandFileNameUTF8(ParamStr(8)));

    if not FileExistsUTF8(GSymbolReplay + '/nyxcompilercontext.pas') or
      not FileExistsUTF8(GSymbolReplay + '/compiler-profile.log') then
      raise Exception.Create('The explicit retained continuation directory must own its exact source and complete terminal transcript');
  end;

  if ParamCount >= 9 then
  begin
    GPreparedReplay := ExcludeTrailingPathDelimiter(ExpandFileNameUTF8(ParamStr(9)));

    if (GPreparedReplay = GSymbolReplay) or
      not FileExistsUTF8(GPreparedReplay + '/nyxcompilercontext.pas') or
      not FileExistsUTF8(GPreparedReplay + '/compiler-profile.log') then
      raise Exception.Create('Prepared mode needs two distinct explicit directories with exact cumulative sources and terminal transcripts');
  end;

  if ParamCount >= 10 then
  begin
    GPrimitiveReplay := ExcludeTrailingPathDelimiter(ExpandFileNameUTF8(ParamStr(10)));

    if (GPrimitiveReplay = GSymbolReplay) or (GPrimitiveReplay = GPreparedReplay) or
      not FileExistsUTF8(GPrimitiveReplay + '/system.pp') or
      not FileExistsUTF8(GPrimitiveReplay + '/compiler-profile.log') then
      raise Exception.Create('Profile-first mode needs the distinct accepted primitive source and terminal reply directory');
  end;

  if ParamCount = 12 then
  begin
    GFinalProfile := ExcludeTrailingPathDelimiter(ExpandFileNameUTF8(ParamStr(11)));
    GImportInputs := ExcludeTrailingPathDelimiter(ExpandFileNameUTF8(ParamStr(12)));

    if (GFinalProfile = GSymbolReplay) or (GFinalProfile = GPreparedReplay) or
      (GFinalProfile = GPrimitiveReplay) or
      not FileExistsUTF8(GFinalProfile + '/nyxcompilercontext.pas') or
      not FileExistsUTF8(GFinalProfile + '/compiler-profile.log') or
      not FileExistsUTF8(GImportInputs + '/import1-paths.txt') or
      not FileExistsUTF8(GImportInputs + '/rtl/objpas/sysconst.pp') then
      raise Exception.Create('Prepared-import mode needs the distinct accepted57 source/log pair and one explicit retained SysConst manifest/source');
  end;

  if not FileExistsUTF8(ARepository + '/src/nyx.controls.pas') or
    not FileExistsUTF8(ARepository + '/tests/nyx.test.documentation.pas') or
    not FileExistsUTF8(ARuntime + '/compiler-root/system.pp') or
    not FileExistsUTF8(ARuntime + '/original-inputs/rtl/win32/system.pp') or
    not FileExistsUTF8(GVerifiedInputs + '/original156-paths.txt') or
    not FileExistsUTF8(GVerifiedInputs + '/windows28-paths.txt') or
    not FileExistsUTF8(GVerifiedInputs + '/all216-paths.txt') or
    not FileExistsUTF8(GProfileRuntime + '/delphi/nyxcompilercontext.pas') or
    not FileExistsUTF8(GProfileRuntime + '/rtl-required/nyxcompilercontext.pas') or
    not FileExistsUTF8(GWindowsRuntime + '/nyxcompilercontext.pas') then
    raise Exception.Create('Both source-adoption arguments must identify complete existing repository/retained inputs');
end;
{$endif}

{$ifndef PAS2JS}
var
  LRepository, LRuntime: TNyxText;
  LRTL: TRetainedRTL;
{$endif}
begin
  try
    {$ifndef PAS2JS}RequireArguments(LRepository, LRuntime);{$endif}
    CheckedOwnerBoundary;
    CallableIdentities;
    UntypedAndFieldShapes;
    StorageShapes;
    PortableSourceCapture;
    {$ifndef PAS2JS}
    LRTL := TRetainedRTL.Create;
    try
      ReplayRTL(LRuntime, LRTL);
      ReplayDependencies(LRTL);
      ActualSourcePackage(LRepository, LRTL);
    finally
      LRTL.Free;
    end;
    {$endif}
    WriteLn('PASS ', GChecks, ' portable source-adoption contract checks');
    {$ifndef PAS2JS}Flush(Output);{$endif}
  except
    on E: Exception do
    begin
      WriteLn('FAIL ', E.ClassName, ': ', E.Message, ' at check count ', GChecks);
      {$ifndef PAS2JS}DumpExceptionBackTrace(Output);{$endif}
      {$ifndef PAS2JS}Flush(Output);{$endif}
      ExitCode := 1;
    end;
  end;
end.
