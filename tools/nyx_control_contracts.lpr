{ nyx
  Copyright (c) 2020 mr-highball

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
}

program nyx_control_contracts;

{$mode delphi}{$H+}
{$codepage utf8}

uses
  Classes,
  SysUtils,
  StrUtils,
  nyx.text,
  nyx.bytes,
  nyx.types,
  nyx.model,
  nyx.catalog,
  nyx.pascal.lexing,
  nyx.pascal.syntax,
  nyx.pascal.types;

type
  { The public families carry scalar/property meaning. The finite kind map is
    reviewed here; the emitted Pascal is checked in and compiled normally.
    This maintenance tool never runs inside Studio or a consumer application. }
  TControlFamily = (cfCaption, cfContainer, cfText, cfBoolean, cfInteger,
    cfImage, cfChoice, cfReference, cfSplit);
  { Checking is the default and never writes source. Candidate generation owns
    a new directory, leaving all reviewed repository includes untouched. }
  TContractOutputMode = (comCheck, comCandidate);
  { Command-line paths belong to this one maintenance invocation. SourceRoot
    supplies descriptor/license inputs; CandidateRoot is always a fresh leaf. }
  TContractOptions = record
    Mode: TContractOutputMode;
    SourceRoot, CandidateRoot: String;
    Help: Boolean;
  end;
  { One authored facade transformation. Emission and declaration admission
    consume this same map; method signatures still come from descriptor source. }
  TFacadeContract = record
    SourceFile, SourceClass, Facade, Member, GUID: String;
    AdmissionMember, AdmissionResult: String;
  end;
  { One checked method projection drives all three facade emission sites.
    Headers and forwarding names retain source spelling; only admitted exact
    unqualified self type names are mapped to the managed facade. No compiler
    binding, ABI conversion or executable descriptor is inferred here. }
  TFacadeMethod = record
    PublicHeader, ImplementationHeader, Arguments, Name: String;
    IsFunction, IsFluent: Boolean;
  end;
  { Cached value projections avoid reparsing a method separately for interface,
    class and implementation output. The source scanner still owns discovery. }
  TFacadeMethods = array of TFacadeMethod;
  { Owned typed IR of the three emitted declaration includes, decoded from the
    exact serialized bytes. It retains complete syntax/scopes/parameters/defaults
    and properties, never live lists or an alternative signature parser. External
    type spelling remains unbound; this proves neither compiler acceptance nor
    a complete documentation/core closure. All construction quotas stay default. }
  TContractDeclarations = class
  private
    FTypes, FFacades, FFactories: INyxPascalSyntaxSnapshot;
    function Capture(ALines: TStringList; AFragment: TNyxPascalSyntaxFragment;
      const APath: String): INyxPascalSyntaxSnapshot;
    function FullType(const ASyntax: INyxPascalSyntaxSnapshot; const AName: String;
      AForm: TNyxPascalSyntaxTypeForm): TNyxPascalSyntaxDeclaration;
    function Routine(const ASyntax: INyxPascalSyntaxSnapshot; AScope: Integer;
      const AName, AResult: String; AKind: TNyxPascalRoutineKind): TNyxPascalSyntaxDeclaration;
    procedure Parameter(const ASyntax: INyxPascalSyntaxSnapshot; AScope: Integer;
      const AName, AType: String; AMode: TNyxPascalSyntaxParameterMode; ADefault: Boolean);
    function ParameterCount(const ASyntax: INyxPascalSyntaxSnapshot; AScope: Integer): Integer;
  public
    constructor Create(ATypes, AFacades, AFactories: TStringList);
    procedure RequireContracts;
    procedure EmitDocumentation(ALines: TStringList);
    procedure EmitCreatorSourceSelection(ALines: TStringList);
  end;
const
  CSourceRootOption = '--source-root';
  CCheckOption = '--check';
  COutputDirectoryOption = '--output-dir';
  CHelpOption = '--help';
  CTypesInclude = 'src/nyx.controls.types.inc';
  CFacadesInclude = 'src/nyx.controls.facades.inc';
  CFactoriesInclude = 'src/nyx.controls.factories.inc';
  CDeclarationFailure = 'Generated declarations refused: ';
  CFacades: array[0..3] of TFacadeContract = (
    (SourceFile: 'src/nyx.model.pas'; SourceClass: 'TNyxNodeConfig';
      Facade: 'NyxConfiguration'; Member: 'Configure'; GUID: '{737A7921-4621-4C6F-8C01-030000000001}';
      AdmissionMember: 'Done'; AdmissionResult: 'INyxControl'),
    (SourceFile: 'src/nyx.model.pas'; SourceClass: 'TNyxNodeBindings';
      Facade: 'NyxBindings'; Member: 'Binds'; GUID: '{AF627AE8-40CC-49C0-8681-E5A8F1FC1008}';
      AdmissionMember: 'Done'; AdmissionResult: 'INyxControl'),
    (SourceFile: 'src/nyx.contract.pas'; SourceClass: 'TNyxContract';
      Facade: 'NyxControlContract'; Member: 'Contract'; GUID: '{737A7921-4621-4C6F-8C01-030000000003}';
      AdmissionMember: 'Snapshot'; AdmissionResult: 'TNyxDataValue'),
    (SourceFile: 'src/nyx.data.pas'; SourceClass: 'TNyxExtensions';
      Facade: 'NyxControlExtensions'; Member: 'Extensions'; GUID: '{737A7921-4621-4C6F-8C01-030000000004}';
      AdmissionMember: 'ToJSON'; AdmissionResult: 'TNyxText')
  );
  CFamilyNames: array[TControlFamily] of String = ('CaptionControl', 'LayoutControl',
    'TextInput', 'BooleanInput', 'IntegerInput', 'ImageControl', 'ChoiceControl',
    'ReferenceControl', 'SplitControl');
  CKindFamilies: array[TNyxKind] of TControlFamily = (
    cfContainer, cfContainer, cfContainer, cfContainer, cfContainer, cfContainer,
    cfContainer, cfContainer, cfContainer, cfContainer, cfContainer,
    cfCaption, cfCaption, cfCaption, cfCaption, cfText, cfText, cfBoolean,
    cfBoolean, cfBoolean, cfChoice, cfInteger, cfInteger, cfText, cfText, cfText,
    cfCaption, cfCaption, cfCaption, cfImage, cfImage, cfInteger, cfCaption,
    cfCaption, cfCaption, cfCaption, cfCaption, cfText, cfContainer, cfReference, cfSplit,
    cfContainer, cfContainer, cfText, cfContainer, cfContainer, cfContainer,
    cfContainer, cfInteger, cfChoice, cfInteger, cfContainer, cfContainer,
    cfInteger, cfContainer, cfContainer, cfContainer, cfContainer, cfContainer,
    cfContainer, cfContainer, cfContainer, cfContainer, cfContainer, cfContainer,
    cfContainer, cfContainer, cfContainer, cfInteger, cfContainer, cfContainer,
    cfContainer, cfContainer, cfContainer, cfContainer, cfContainer, cfContainer,
    cfContainer, cfContainer, cfCaption, cfContainer
  );
var
  GTypes: TStringList;
  GFactories: TStringList;
  GImplementation: TStringList;
  GFacadeTypes: TStringList;
  GFacadeImplementation: TStringList;
  GDocumentation: TStringList;
  GOptions: TContractOptions;

function SourcePath(const ARelative: String): String;
begin
  Result := IncludeTrailingPathDelimiter(GOptions.SourceRoot) + ARelative;
end;

function ReadOptions: TContractOptions;
var
  LIndex: Integer;
  LArgument: String;
  LSourceGiven, LModeGiven: Boolean;
  function PathArgument: String;
  begin
    Inc(LIndex);

    if (LIndex > ParamCount) or (ParamStr(LIndex) = '') or
      (Copy(ParamStr(LIndex), 1, 2) = '--') then
      raise Exception.Create('A path is required after ' + LArgument);
    Result := ExpandFileName(ParamStr(LIndex));
  end;
begin
  Result.Mode := comCheck;
  Result.SourceRoot := GetCurrentDir;
  Result.CandidateRoot := '';
  Result.Help := False;
  LSourceGiven := False;
  LModeGiven := False;
  LIndex := 1;
  while LIndex <= ParamCount do
  begin
    LArgument := ParamStr(LIndex);

    if LArgument = CSourceRootOption then
    begin

      if LSourceGiven then
        raise Exception.Create('--source-root may be supplied only once');
      Result.SourceRoot := PathArgument;
      LSourceGiven := True;
    end
    else if (LArgument = CCheckOption) or (LArgument = COutputDirectoryOption) then
    begin

      if LModeGiven then
        raise Exception.Create('Choose either --check or --output-dir once');
      LModeGiven := True;

      if LArgument = COutputDirectoryOption then
      begin
        Result.Mode := comCandidate;
        Result.CandidateRoot := PathArgument;
      end;
    end
    else if LArgument = CHelpOption then
      Result.Help := True
    else
      raise Exception.Create('Unknown argument: ' + LArgument);
    Inc(LIndex);
  end;
end;

procedure WriteUsage;
begin
  WriteLn('Nyx control-contract maintenance');
  WriteLn('  nyx_control_contracts [--source-root <repository>] [--check]');
  WriteLn('  nyx_control_contracts [--source-root <repository>] --output-dir <new-directory>');
  WriteLn('No arguments checks five includes and their creator-documentation companion without writing.');
  WriteLn('Candidate output retains src/ names for review; existing destinations refuse.');
  WriteLn('Exit 0: match/generated; exit 2: source differs; exit 1: invalid input or failure.');
end;

function Stem(const AName: TNyxText): String;
var
  LIndex: Integer;
  LUpper: Boolean;
  LChar: Char;
begin
  Result := '';
  LUpper := True;
  for LIndex := 1 to Length(AName) do
  begin
    LChar := AName[LIndex];

    if LChar in ['-', '/'] then
    begin
      LUpper := True;
    end
    else
    begin

      if LUpper then
      begin
        LChar := UpCase(LChar);
      end;
      Result := Result + LChar;
      LUpper := False;
    end;
  end;
end;

function KindGUID(const AName: TNyxText): String;
var
  LHash: QWord;
  LIndex: Integer;
begin
  { Stable name-based GUIDs do not change when a future kind is inserted into
    the enum. Multiplication fits in QWord before truncation to 32 bits. }
  LHash := 2166136261;
  for LIndex := 1 to Length(AName) do
  begin
    LHash := ((LHash xor Ord(AName[LIndex])) * 16777619) and $FFFFFFFF;
  end;
  Result := '{737A7921-4621-4C6F-8C01-0200' + IntToHex(LHash, 8) + '}';
end;

function TContractDeclarations.Capture(ALines: TStringList;
  AFragment: TNyxPascalSyntaxFragment; const APath: String): INyxPascalSyntaxSnapshot;
var
  LStream: TMemoryStream;
  LBytes: TNyxBytes;
  LOptions: TNyxPascalSyntaxOptions;
  LAt: Integer;
  LScope: TNyxPascalSyntaxScope;
  LDeclaration: TNyxPascalSyntaxDeclaration;
begin
  LOptions := TNyxPascalSyntaxOptions.Defaults.Fragment(AFragment);
  LStream := TMemoryStream.Create;
  try
    ALines.SaveToStream(LStream);

    if LStream.Size > LOptions.LexicalOptions.MaximumSourceBytes then
      raise Exception.Create(CDeclarationFailure + APath + ' exceeds its existing source allowance');
    SetLength(LBytes, Integer(LStream.Size));

    if Length(LBytes) > 0 then
      Move(LStream.Memory^, LBytes[0], Length(LBytes));
    Result := NewNyxPascalSyntaxSnapshot(NyxDecodeUTF8(LBytes), LOptions);
  finally
    LStream.Free;
  end;

  if Result.Lexical.DiagnosticCount <> 0 then
    raise Exception.Create(CDeclarationFailure + APath + ' has lexical diagnostics');

  if Result.DiagnosticCount <> 0 then
    raise Exception.Create(CDeclarationFailure + APath + ': ' + String(Result.Diagnostic(0).MessageText));
  for LAt := 0 to Result.ScopeCount - 1 do
  begin
    LScope := Result.Scope(LAt);

    if not LScope.Complete or LScope.UnsupportedSyntax then
      raise Exception.Create(CDeclarationFailure + APath + ' has incomplete/unsupported scope ' + String(LScope.Name));
  end;
  for LAt := 0 to Result.DeclarationCount - 1 do
  begin
    LDeclaration := Result.Declaration(LAt);

    if (LDeclaration.Flags * [psdfIncomplete, psdfIncompleteHeader,
      psdfUnsupported, psdfConditional]) <> [] then
      raise Exception.Create(CDeclarationFailure + APath + ' has unsupported declaration ' + String(LDeclaration.Name));
  end;
end;

constructor TContractDeclarations.Create(ATypes, AFacades, AFactories: TStringList);
begin
  inherited Create;
  FTypes := Capture(ATypes, psfTypes, CTypesInclude);
  FFacades := Capture(AFacades, psfTypes, CFacadesInclude);
  FFactories := Capture(AFactories, psfInterfaceDeclarations, CFactoriesInclude);
end;

function TContractDeclarations.FullType(const ASyntax: INyxPascalSyntaxSnapshot;
  const AName: String; AForm: TNyxPascalSyntaxTypeForm): TNyxPascalSyntaxDeclaration;
var
  LAt, LCount: Integer;
  LDeclaration: TNyxPascalSyntaxDeclaration;
begin
  Result := Default(TNyxPascalSyntaxDeclaration);
  LCount := 0;
  LAt := ASyntax.Named(0, TNyxText(AName));
  while LAt >= 0 do
  begin
    LDeclaration := ASyntax.Declaration(LAt);

    if (LDeclaration.Kind = psdkType) and
      not (psdfForward in LDeclaration.Flags) then
    begin

      if (LDeclaration.Name <> TNyxText(AName)) or
        (LDeclaration.TypeForm <> AForm) or (LDeclaration.NestedScope < 0) then
        raise Exception.Create(CDeclarationFailure + 'wrong full type shape for ' + AName);
      Result := LDeclaration;
      Inc(LCount);
    end;
    LAt := LDeclaration.NextNamed;
  end;

  if LCount <> 1 then
    raise Exception.Create(CDeclarationFailure + 'requires exactly one full type ' + AName);
end;

function TContractDeclarations.Routine(const ASyntax: INyxPascalSyntaxSnapshot;
  AScope: Integer; const AName, AResult: String;
  AKind: TNyxPascalRoutineKind): TNyxPascalSyntaxDeclaration;
var
  LAt: Integer;
begin
  LAt := ASyntax.Named(AScope, TNyxText(AName));

  if LAt < 0 then
    raise Exception.Create(CDeclarationFailure + 'missing routine ' + AName);
  Result := ASyntax.Declaration(LAt);

  if (Result.Kind <> psdkRoutine) or (Result.RoutineKind <> AKind) or
    (Result.NextNamed >= 0) or
    (Trim(ASyntax.Lexical.Slice(Result.TypeSpan)) <> TNyxText(AResult)) then
    raise Exception.Create(CDeclarationFailure + 'wrong routine/result shape for ' + AName);
end;

procedure TContractDeclarations.Parameter(const ASyntax: INyxPascalSyntaxSnapshot;
  AScope: Integer; const AName, AType: String;
  AMode: TNyxPascalSyntaxParameterMode; ADefault: Boolean);
var
  LAt: Integer;
  LDeclaration: TNyxPascalSyntaxDeclaration;
begin
  LAt := ASyntax.Named(AScope, TNyxText(AName));

  if LAt < 0 then
    raise Exception.Create(CDeclarationFailure + 'missing parameter ' + AName);
  LDeclaration := ASyntax.Declaration(LAt);

  if (LDeclaration.Kind <> psdkParameter) or
    (LDeclaration.ParameterMode <> AMode) or (LDeclaration.NextNamed >= 0) or
    (Trim(ASyntax.Lexical.Slice(LDeclaration.TypeSpan)) <> TNyxText(AType)) or
    ((psdfDefaultArgument in LDeclaration.Flags) <> ADefault) or
    (ADefault and (LDeclaration.ExpressionSpan.Length = 0)) then
    raise Exception.Create(CDeclarationFailure + 'wrong parameter/type/default for ' + AName);
end;

function TContractDeclarations.ParameterCount(const ASyntax: INyxPascalSyntaxSnapshot;
  AScope: Integer): Integer;
var
  LAt: Integer;
  LDeclaration: TNyxPascalSyntaxDeclaration;
begin
  Result := 0;
  LAt := ASyntax.Scope(AScope).FirstDeclaration;
  while LAt >= 0 do
  begin
    LDeclaration := ASyntax.Declaration(LAt);

    if LDeclaration.Kind = psdkParameter then
      Inc(Result);
    LAt := LDeclaration.Next;
  end;
end;

procedure TContractDeclarations.RequireContracts;
var
  LKind: TNyxKind;
  LAt: Integer;
  LStem, LInterface, LFamily: String;
  LType, LClass, LRoutine: TNyxPascalSyntaxDeclaration;
begin

  if (FTypes.Scope(0).DeclarationCount <> 3 * (Ord(High(TNyxKind)) + 1)) or
    (FFactories.Scope(0).DeclarationCount <> Ord(High(TNyxKind)) + 1) or
    (FFacades.Scope(0).DeclarationCount <> 2 * Length(CFacades)) then
    raise Exception.Create(CDeclarationFailure + 'unexpected top-level declaration coverage');
  for LKind := Low(TNyxKind) to High(TNyxKind) do
  begin
    LStem := Stem(NyxKindName(LKind));
    LInterface := 'INyx' + LStem;
    LFamily := CFamilyNames[CKindFamilies[LKind]];
    LType := FullType(FTypes, LInterface, pstfInterface);
    LClass := FullType(FTypes, 'TNyx' + LStem, pstfClass);

    if (Trim(FTypes.Lexical.Slice(LType.AncestorSpan)) <> TNyxText('(INyx' + LFamily + ')')) or
      (Trim(FTypes.Lexical.Slice(LClass.AncestorSpan)) <> TNyxText('(TNyx' + LFamily + ', ' + LInterface + ')')) then
      raise Exception.Create(CDeclarationFailure + 'wrong declared family for ' + LStem);
    LRoutine := Routine(FTypes, LType.NestedScope, 'WithText', LInterface, prkFunction);
    Parameter(FTypes, LRoutine.NestedScope, 'AText', 'TNyxText', pspmConst, False);

    if ParameterCount(FTypes, LRoutine.NestedScope) <> 1 then
      raise Exception.Create(CDeclarationFailure + 'wrong WithText parameters for ' + LStem);
    Routine(FTypes, LClass.NestedScope, 'WithText', LInterface, prkFunction);
    LRoutine := Routine(FTypes, LClass.NestedScope, 'Create', '', prkConstructor);
    Parameter(FTypes, LRoutine.NestedScope, 'AID', 'TNyxText', pspmConst, True);
    Parameter(FTypes, LRoutine.NestedScope, 'AConstruction', 'TNyxConstruction', pspmValue, True);

    if ParameterCount(FTypes, LRoutine.NestedScope) <> 2 then
      raise Exception.Create(CDeclarationFailure + 'wrong constructor parameters for ' + LStem);
    LRoutine := Routine(FFactories, 0, 'NewNyx' + LStem, LInterface, prkFunction);
    Parameter(FFactories, LRoutine.NestedScope, 'AID', 'TNyxText', pspmConst, True);
    Parameter(FFactories, LRoutine.NestedScope, 'AConstruction', 'TNyxConstruction', pspmValue, True);

    if ParameterCount(FFactories, LRoutine.NestedScope) <> 2 then
      raise Exception.Create(CDeclarationFailure + 'wrong factory parameters for ' + LStem);
  end;
  for LAt := 0 to High(CFacades) do
  begin
    LType := FullType(FFacades, 'I' + CFacades[LAt].Facade, pstfInterface);
    LClass := FullType(FFacades, 'T' + CFacades[LAt].Facade, pstfClass);
    LRoutine := Routine(FFacades, LType.NestedScope,
      CFacades[LAt].AdmissionMember, CFacades[LAt].AdmissionResult, prkFunction);

    if ParameterCount(FFacades, LRoutine.NestedScope) <> 0 then
      raise Exception.Create(CDeclarationFailure + 'wrong facade query parameters for ' + CFacades[LAt].Facade);
    LRoutine := Routine(FFacades, LClass.NestedScope, 'Create', '', prkConstructor);
    Parameter(FFacades, LRoutine.NestedScope, 'AOwner', 'INyxControl', pspmConst, False);

    if ParameterCount(FFacades, LRoutine.NestedScope) <> 1 then
      raise Exception.Create(CDeclarationFailure + 'wrong facade owner parameters for ' + CFacades[LAt].Facade);
  end;
  WriteLn('PASS typed declaration IR / ', Ord(High(TNyxKind)) + 1,
    ' kinds / ', Length(CFacades), ' facades');
end;

procedure TContractDeclarations.EmitDocumentation(ALines: TStringList);
var
  LKind: TNyxKind;
  LStem, LFactoryName, LResultName, LIDDefault, LConstructionDefault: String;
  LFactory, LParameter: TNyxPascalSyntaxDeclaration;
begin
  ALines.Add('unit nyx.controls.documentation;');
  ALines.Add('');
  ALines.Add('{$mode delphi}{$H+}{$codepage utf8}');
  ALines.Add('');
  ALines.Add('interface');
  ALines.Add('');
  ALines.Add('uses nyx.catalog, nyx.documentation.declarations,');
  ALines.Add('  nyx.documentation.source, nyx.documentation.typeexpressions, nyx.pascal.binding;');
  ALines.Add('');
  ALines.Add('{ Bind an explicitly supplied declaration package to the generated built-in');
  ALines.Add('  creators. No files, backend, controls or callbacks are opened or executed.');
  ALines.Add('  This companion maps declarations; it does not manufacture missing core facts');
  ALines.Add('  or turn a partial source package into a complete API reference. }');
  ALines.Add('function NewNyxControlsDocumentationSource(');
  ALines.Add('  const APackage: INyxDocumentationPackage): INyxCatalogDocumentationSource;');
  ALines.Add('{ Match generated creator intent to exact current public source declarations.');
  ALines.Add('  Returned ordinals belong only to this world and are input annotations for');
  ALines.Add('  CaptureNyxDocumentationSourcePackage, never persisted member identities.');
  ALines.Add('  The caller owns the shared construction; this function does not retire it. }');
  ALines.Add('function NyxControlsDocumentationCreators(const AWorld: INyxPascalSemanticWorld;');
  ALines.Add('  AModuleIndex: Integer; const AConstruction: INyxDocumentationTypeConstruction):');
  ALines.Add('  TNyxDocumentationSourceCreators;');
  ALines.Add('');
  ALines.Add('implementation');
  ALines.Add('');
  ALines.Add('uses SysUtils, nyx.text, nyx.types, nyx.documentation,');
  ALines.Add('  nyx.documentation.registration, nyx.pascal.types, nyx.pascal.syntax,');
  ALines.Add('  nyx.pascal.syntax.translated;');
  ALines.Add('');
  ALines.Add('type');
  ALines.Add('  { One captured package supplies the complete real unit descriptor. Exact');
  ALines.Add('    source-provided member identities survive overload lookup; enum ordinals');
  ALines.Add('    never become member identities. The catalog borrows this immutable mapping');
  ALines.Add('    only while creating its ordinary retained package bindings. }');
  ALines.Add('  TControlDocumentationSource = class(TInterfacedObject, INyxCatalogDocumentationSource)');
  ALines.Add('  private');
  ALines.Add('    FPackage: INyxDocumentationPackage;');
  ALines.Add('    FUnit: INyxDocumentationType;');
  ALines.Add('    FPrimaries: array[TNyxKind] of TNyxDocumentationRef;');
  ALines.Add('    function RequireFactory(const AName, AResult, AIDDefault,');
  ALines.Add('      AConstructionDefault: TNyxText): TNyxDocumentationRef;');
  ALines.Add('  public');
  ALines.Add('    constructor Create(const APackage: INyxDocumentationPackage);');
  ALines.Add('    function PackageOwner: INyxDocumentationPackage;');
  ALines.Add('    function PrimaryForKind(AKind: TNyxKind): TNyxDocumentationRef;');
  ALines.Add('  end;');
  ALines.Add('');
  EmitCreatorSourceSelection(ALines);
  ALines.Add('{ Only an exact bare name and its explicit nominal binding match these');
  ALines.Add('  generated signatures. Compound syntax, unresolved/formal names and alias');
  ALines.Add('  substitution never collapse into a convenient legacy named projection. }');
  ALines.Add('function SameNamedUse(const AUse: INyxDocumentationTypeUse;');
  ALines.Add('  const AExpected: TNyxDocumentationRef): Boolean;');
  ALines.Add('var');
  ALines.Add('  LExpression: INyxDocumentationTypeExpression;');
  ALines.Add('begin');
  ALines.Add('  Result := False;');
  ALines.Add('');
  ALines.Add('  if AUse = nil then');
  ALines.Add('    Exit;');
  ALines.Add('  LExpression := AUse.Expression;');
  ALines.Add('');
  ALines.Add('  if LExpression = nil then');
  ALines.Add('    Exit(AUse.Reference.Same(AExpected));');
  ALines.Add('');
  ALines.Add('  if not LExpression.Syntax.Complete or');
  ALines.Add('    (LExpression.Syntax.Root < 0) or');
  ALines.Add('    (LExpression.Syntax.Node(LExpression.Syntax.Root).Kind <> ptxName) or');
  ALines.Add('    (LExpression.NamedUseCount <> 1) or');
  ALines.Add('    (LExpression.NamedUseNode(0) <> LExpression.Syntax.Root) or');
  ALines.Add('    (LExpression.NamedUseArity(0) <> 0) or (AUse.BindingCount <> 1) then');
  ALines.Add('    Exit;');
  ALines.Add('  Result := AUse.BindingAt(0).Same(AExpected);');
  ALines.Add('end;');
  ALines.Add('');
  ALines.Add('function SameLegacyParameter(const AParameter: TNyxDocumentationParameter;');
  ALines.Add('  const AName: TNyxText; const AType: TNyxDocumentationRef;');
  ALines.Add('  AMode: TNyxDocumentationParameterMode; const ADefault: TNyxText): Boolean;');
  ALines.Add('begin');
  ALines.Add('  Result := (AParameter.Name = AName) and (AParameter.Passing = AMode) and');
  ALines.Add('    AParameter.TypeReference.Same(AType) and AParameter.HasDefault and');
  ALines.Add('    (AParameter.DefaultPascal = ADefault);');
  ALines.Add('end;');
  ALines.Add('');
  ALines.Add('function SameRichParameter(const AParameter: INyxDocumentationParameterDeclaration;');
  ALines.Add('  const AName: TNyxText; const AType: TNyxDocumentationRef;');
  ALines.Add('  AMode: TNyxDocumentationParameterMode; const ADefault: TNyxText): Boolean;');
  ALines.Add('begin');
  ALines.Add('  Result := (AParameter <> nil) and (AParameter.Name = AName) and');
  ALines.Add('    (AParameter.Passing = AMode) and SameNamedUse(AParameter.TypeUse, AType) and');
  ALines.Add('    AParameter.HasDefault and (AParameter.DefaultPascal = ADefault);');
  ALines.Add('end;');
  ALines.Add('');
  ALines.Add('function TControlDocumentationSource.RequireFactory(const AName, AResult,');
  ALines.Add('  AIDDefault, AConstructionDefault: TNyxText): TNyxDocumentationRef;');
  ALines.Add('var');
  ALines.Add('  LIndex, LMatches: Integer;');
  ALines.Add('  LMember: INyxDocumentationMember;');
  ALines.Add('  LSignature: INyxDocumentationSignature;');
  ALines.Add('  LCapability: INyxDocumentationMemberSignature;');
  ALines.Add('  LRich: INyxDocumentationRichSignature;');
  ALines.Add('  LMatchesShape: Boolean;');
  ALines.Add('begin');
  ALines.Add('  Result := Default(TNyxDocumentationRef);');
  ALines.Add('  LMatches := 0;');
  ALines.Add('  for LIndex := 0 to FUnit.MemberCount - 1 do');
  ALines.Add('  begin');
  ALines.Add('    LMember := FUnit.MemberAt(LIndex);');
  ALines.Add('');
  ALines.Add('    if LMember.Identity.MemberName <> AName then');
  ALines.Add('      Continue;');
  ALines.Add('');
  ALines.Add('    if not (LMember.Kind in [ndmFactory, ndmRoutine]) then');
  ALines.Add('      Continue;');
  ALines.Add('    LRich := nil;');
  ALines.Add('');
  ALines.Add('    if Supports(LMember, INyxDocumentationMemberSignature, LCapability) then');
  ALines.Add('      LRich := LCapability.DeclaredSignature;');
  ALines.Add('');
  ALines.Add('    if LRich <> nil then');
  ALines.Add('    begin');
  ALines.Add('      LMatchesShape := (LRich.ParameterCount = 2) and');
  ALines.Add('        SameNamedUse(LRich.ResultUse, NyxDocumentationType(''nyx.controls'', AResult)) and');
  ALines.Add('        SameRichParameter(LRich.ParameterAt(0), ''AID'',');
  ALines.Add('          NyxDocumentationType(''nyx.text'', ''TNyxText''), ndpConst, AIDDefault) and');
  ALines.Add('        SameRichParameter(LRich.ParameterAt(1), ''AConstruction'',');
  ALines.Add('          NyxDocumentationType(''nyx.controls'', ''TNyxConstruction''), ndpValue, AConstructionDefault);');
  ALines.Add('    end');
  ALines.Add('    else');
  ALines.Add('    begin');
  ALines.Add('      LSignature := LMember.Signature;');
  ALines.Add('      LMatchesShape := (LSignature <> nil) and (LSignature.ParameterCount = 2) and');
  ALines.Add('        LSignature.ResultType.Same(NyxDocumentationType(''nyx.controls'', AResult)) and');
  ALines.Add('        SameLegacyParameter(LSignature.ParameterAt(0), ''AID'',');
  ALines.Add('          NyxDocumentationType(''nyx.text'', ''TNyxText''), ndpConst, AIDDefault) and');
  ALines.Add('        SameLegacyParameter(LSignature.ParameterAt(1), ''AConstruction'',');
  ALines.Add('          NyxDocumentationType(''nyx.controls'', ''TNyxConstruction''), ndpValue, AConstructionDefault);');
  ALines.Add('    end;');
  ALines.Add('');
  ALines.Add('    if not LMatchesShape then');
  ALines.Add('      Continue;');
  ALines.Add('    Inc(LMatches);');
  ALines.Add('    Result := LMember.Identity;');
  ALines.Add('  end;');
  ALines.Add('');
  ALines.Add('  if LMatches <> 1 then');
  ALines.Add('    raise ENyxDocumentation.Create(''Generated creator metadata is missing, stale or ambiguous: '' + AName);');
  ALines.Add('end;');
  ALines.Add('');
  ALines.Add('constructor TControlDocumentationSource.Create(const APackage: INyxDocumentationPackage);');
  ALines.Add('begin');
  ALines.Add('  inherited Create;');
  ALines.Add('  FPackage := CaptureNyxDocumentationPackage(APackage);');
  ALines.Add('');
  ALines.Add('  if FPackage = nil then');
  ALines.Add('    raise ENyxDocumentation.Create(''Generated creator metadata requires its declaration package'');');
  ALines.Add('  FUnit := FPackage.FindDeclaration(NyxDocumentationUnit(''nyx.controls''));');
  ALines.Add('');
  ALines.Add('  if FUnit = nil then');
  ALines.Add('    raise ENyxDocumentation.Create(''Generated creator metadata requires the complete controls unit'');');
  ALines.Add('  { These expectations come from the same checked declaration IR that emits');
  ALines.Add('    interfaces, classes and factories. No second author-maintained help map is');
  ALines.Add('    consulted. A supplied package retains its original declaration provenance. }');
  for LKind := Low(TNyxKind) to High(TNyxKind) do
  begin
    LStem := Stem(NyxKindName(LKind));
    LFactory := Routine(FFactories, 0, 'NewNyx' + LStem, 'INyx' + LStem, prkFunction);
    LFactoryName := String(FFactories.Lexical.Slice(LFactory.NameSpan));
    LResultName := String(FFactories.Lexical.Slice(LFactory.TypeSpan));
    LParameter := FFactories.Declaration(FFactories.Named(LFactory.NestedScope, 'AID'));
    LIDDefault := String(FFactories.Lexical.Slice(LParameter.ExpressionSpan));
    LParameter := FFactories.Declaration(FFactories.Named(LFactory.NestedScope, 'AConstruction'));
    LConstructionDefault := String(FFactories.Lexical.Slice(LParameter.ExpressionSpan));
    ALines.Add('  FPrimaries[nk' + LStem + '] := RequireFactory(' +
      QuotedStr(LFactoryName) + ', ' + QuotedStr(LResultName) + ', ' +
      QuotedStr(LIDDefault) + ', ' + QuotedStr(LConstructionDefault) + ');');
  end;
  ALines.Add('end;');
  ALines.Add('');
  ALines.Add('function TControlDocumentationSource.PackageOwner: INyxDocumentationPackage;');
  ALines.Add('begin');
  ALines.Add('  Result := FPackage;');
  ALines.Add('end;');
  ALines.Add('');
  ALines.Add('function TControlDocumentationSource.PrimaryForKind(AKind: TNyxKind): TNyxDocumentationRef;');
  ALines.Add('begin');
  ALines.Add('');
  ALines.Add('  if (Ord(AKind) < Ord(Low(TNyxKind))) or (Ord(AKind) > Ord(High(TNyxKind))) then');
  ALines.Add('    raise ENyxDocumentation.Create(''Unknown generated creator kind'');');
  ALines.Add('  Result := FPrimaries[AKind];');
  ALines.Add('end;');
  ALines.Add('');
  ALines.Add('function NewNyxControlsDocumentationSource(');
  ALines.Add('  const APackage: INyxDocumentationPackage): INyxCatalogDocumentationSource;');
  ALines.Add('begin');
  ALines.Add('  Result := TControlDocumentationSource.Create(APackage);');
  ALines.Add('end;');
  ALines.Add('');
  ALines.Add('end.');
end;

procedure TContractDeclarations.EmitCreatorSourceSelection(ALines: TStringList);
var
  LKind: TNyxKind;
  LStem: String;
  LFactory, LParameter: TNyxPascalSyntaxDeclaration;
  LName, LResult, LIDDefault, LConstructionDefault: String;
begin
  ALines.Add('function NyxControlsDocumentationCreators(const AWorld: INyxPascalSemanticWorld;');
  ALines.Add('  AModuleIndex: Integer; const AConstruction: INyxDocumentationTypeConstruction):');
  ALines.Add('  TNyxDocumentationSourceCreators;');
  ALines.Add('var');
  ALines.Add('  LWorld: INyxPascalSemanticWorld;');
  ALines.Add('  LView: INyxPascalTranslatedSyntax;');
  ALines.Add('  LSyntax: INyxPascalSyntaxSnapshot;');
  ALines.Add('  LConstruction: INyxDocumentationTypeConstruction;');
  ALines.Add('  LWindows: INyxDocumentationTextWindows;');
  ALines.Add('  LValues: TNyxDocumentationSourceCreators;');
  ALines.Add('');
  ALines.Add('  procedure Spend(AUnits: Integer = 1);');
  ALines.Add('  begin');
  ALines.Add('    CheckNyxDocumentationTypeConstruction(LConstruction);');
  ALines.Add('    LConstruction.Spend(AUnits);');
  ALines.Add('    CheckNyxDocumentationTypeConstruction(LConstruction);');
  ALines.Add('  end;');
  ALines.Add('');
  ALines.Add('  procedure Refuse(const AName: TNyxText);');
  ALines.Add('  begin');
  ALines.Add('    raise ENyxDocumentation.Create(''Generated creator source is missing, stale or ambiguous: '' + AName);');
  ALines.Add('  end;');
  ALines.Add('');
  ALines.Add('  procedure RequireFactory(AKind: TNyxKind; const AName, AResult,');
  ALines.Add('    AIDDefault, AConstructionDefault: TNyxText);');
  ALines.Add('  var');
  ALines.Add('    LAt, LFound, LMatches, LParameterAt, LParameterCount: Integer;');
  ALines.Add('    LDeclaration, LParameter: TNyxPascalSyntaxDeclaration;');
  ALines.Add('');
  ALines.Add('    procedure RequireParameter(const AExpectedName, AExpectedType,');
  ALines.Add('      AExpectedDefault: TNyxText; AMode: TNyxPascalSyntaxParameterMode);');
  ALines.Add('    begin');
  ALines.Add('      Spend;');
  ALines.Add('');
  ALines.Add('      if (LParameter.Name <> AExpectedName) or (LParameter.ParameterMode <> AMode) or');
  ALines.Add('        (LParameter.Flags <> [psdfDefaultArgument]) or');
  ALines.Add('        (LParameter.TypeSpan.Length = 0) or (LParameter.ExpressionSpan.Length = 0) then');
  ALines.Add('        Refuse(AName);');
  ALines.Add('');
  ALines.Add('      if (Trim(LWindows.CopyWindow(LParameter.TypeSpan)) <> AExpectedType) or');
  ALines.Add('        (LWindows.CopyWindow(LParameter.ExpressionSpan) <> AExpectedDefault) then');
  ALines.Add('        Refuse(AName);');
  ALines.Add('    end;');
  ALines.Add('');
  ALines.Add('  begin');
  ALines.Add('    Spend;');
  ALines.Add('    LFound := -1;');
  ALines.Add('    LMatches := 0;');
  ALines.Add('    LAt := LSyntax.Named(0, AName);');
  ALines.Add('    while LAt >= 0 do');
  ALines.Add('    begin');
  ALines.Add('      Spend;');
  ALines.Add('      LDeclaration := LSyntax.Declaration(LAt);');
  ALines.Add('');
  ALines.Add('      if LDeclaration.Visibility = psavInterface then');
  ALines.Add('      begin');
  ALines.Add('        Inc(LMatches);');
  ALines.Add('        LFound := LAt;');
  ALines.Add('      end;');
  ALines.Add('      LAt := LDeclaration.NextNamed;');
  ALines.Add('    end;');
  ALines.Add('');
  ALines.Add('    if LMatches <> 1 then');
  ALines.Add('      Refuse(AName);');
  ALines.Add('    LDeclaration := LSyntax.Declaration(LFound);');
  ALines.Add('');
  ALines.Add('    if (LDeclaration.Kind <> psdkRoutine) or (LDeclaration.RoutineKind <> prkFunction) or');
  ALines.Add('      (LDeclaration.Scope <> 0) or (LDeclaration.OwnerName <> '''') or');
  ALines.Add('      (LDeclaration.Flags * [psdfIncomplete, psdfConditional, psdfUnsupported,');
  ALines.Add('        psdfIncompleteHeader, psdfClassMember, psdfStatic] <> []) or');
  ALines.Add('      (LDeclaration.GenericSpan.Length <> 0) or (LDeclaration.TypeSpan.Length = 0) or');
  ALines.Add('      (LDeclaration.NestedScope < 0) then');
  ALines.Add('      Refuse(AName);');
  ALines.Add('');
  ALines.Add('    if not LSyntax.Scope(LDeclaration.NestedScope).Complete or');
  ALines.Add('      LSyntax.Scope(LDeclaration.NestedScope).UnsupportedSyntax then');
  ALines.Add('      Refuse(AName);');
  ALines.Add('    LParameterAt := LSyntax.Scope(LDeclaration.NestedScope).FirstDeclaration;');
  ALines.Add('    LParameterCount := 0;');
  ALines.Add('    while LParameterAt >= 0 do');
  ALines.Add('    begin');
  ALines.Add('      Spend;');
  ALines.Add('      LParameter := LSyntax.Declaration(LParameterAt);');
  ALines.Add('');
  ALines.Add('      if (LParameter.Kind <> psdkParameter) or');
  ALines.Add('        (LParameter.Scope <> LDeclaration.NestedScope) then');
  ALines.Add('        Refuse(AName);');
  ALines.Add('      case LParameterCount of');
  ALines.Add('        0: RequireParameter(''AID'', ''TNyxText'', AIDDefault, pspmConst);');
  ALines.Add('        1: RequireParameter(''AConstruction'', ''TNyxConstruction'', AConstructionDefault, pspmValue);');
  ALines.Add('      else');
  ALines.Add('        Refuse(AName);');
  ALines.Add('      end;');
  ALines.Add('      Inc(LParameterCount);');
  ALines.Add('      LParameterAt := LParameter.Next;');
  ALines.Add('    end;');
  ALines.Add('');
  ALines.Add('    if (LParameterCount <> 2) or');
  ALines.Add('      (Trim(LWindows.CopyWindow(LDeclaration.TypeSpan)) <> AResult) then');
  ALines.Add('      Refuse(AName);');
  ALines.Add('    LValues[Ord(AKind)].ModuleIndex := AModuleIndex;');
  ALines.Add('    LValues[Ord(AKind)].DeclarationIndex := LFound;');
  ALines.Add('  end;');
  ALines.Add('');
  ALines.Add('begin');
  ALines.Add('  Result := nil;');
  ALines.Add('  { Passed values are leased before any admission callback. Only exact owned');
  ALines.Add('    immutable views are inspected; no alternate getter supplies source facts. }');
  ALines.Add('  LWorld := AWorld;');
  ALines.Add('  LConstruction := AConstruction;');
  ALines.Add('  CheckNyxDocumentationTypeConstruction(LConstruction);');
  ALines.Add('  Spend;');
  ALines.Add('');
  ALines.Add('  if not NyxPascalOwnedSemanticWorld(LWorld) then');
  ALines.Add('    Refuse(''world'');');
  ALines.Add('');
  ALines.Add('  if (AModuleIndex < 0) or (AModuleIndex >= LWorld.Count) then');
  ALines.Add('    Refuse(''module'');');
  ALines.Add('  LView := LWorld.ModuleAt(AModuleIndex);');
  ALines.Add('');
  ALines.Add('  if not NyxPascalOwnedTranslatedSyntax(LView) then');
  ALines.Add('    Refuse(''view'');');
  ALines.Add('');
  ALines.Add('  if not LView.CompleteTranslation or not LView.Source.Resolved then');
  ALines.Add('    Refuse(''translation'');');
  ALines.Add('  LSyntax := LView.Syntax;');
  ALines.Add('');
  ALines.Add('  if (LSyntax.DiagnosticCount <> 0) or (LSyntax.ScopeCount = 0) then');
  ALines.Add('    Refuse(''syntax'');');
  ALines.Add('');
  ALines.Add('  if (LSyntax.Scope(0).Name <> ''nyx.controls'') or');
  ALines.Add('    not LSyntax.Scope(0).Complete or LSyntax.Scope(0).UnsupportedSyntax then');
  ALines.Add('    Refuse(''nyx.controls'');');
  ALines.Add('  Spend(Ord(High(TNyxKind)) + 2);');
  ALines.Add('  SetLength(LValues, Ord(High(TNyxKind)) + 1);');
  ALines.Add('  LWindows := NewNyxDocumentationTextWindows(LSyntax.Lexical.Source, LConstruction);');
  ALines.Add('  try');
  for LKind := Low(TNyxKind) to High(TNyxKind) do
  begin
    LStem := Stem(NyxKindName(LKind));
    LFactory := Routine(FFactories, 0, 'NewNyx' + LStem, 'INyx' + LStem, prkFunction);
    LName := String(FFactories.Lexical.Slice(LFactory.NameSpan));
    LResult := String(FFactories.Lexical.Slice(LFactory.TypeSpan));
    LParameter := FFactories.Declaration(FFactories.Named(LFactory.NestedScope, 'AID'));
    LIDDefault := String(FFactories.Lexical.Slice(LParameter.ExpressionSpan));
    LParameter := FFactories.Declaration(FFactories.Named(LFactory.NestedScope, 'AConstruction'));
    LConstructionDefault := String(FFactories.Lexical.Slice(LParameter.ExpressionSpan));
    ALines.Add('    RequireFactory(nk' + LStem + ', ' + QuotedStr(LName) + ', ' +
      QuotedStr(LResult) + ', ' + QuotedStr(LIDDefault) + ', ' + QuotedStr(LConstructionDefault) + ');');
  end;
  ALines.Add('    CheckNyxDocumentationTypeConstruction(LConstruction);');
  ALines.Add('    Result := LValues;');
  ALines.Add('  finally');
  ALines.Add('    LWindows.Retire;');
  ALines.Add('  end;');
  ALines.Add('end;');
  ALines.Add('');
end;

procedure EmitControls;
var
  LKind: TNyxKind;
  LStem: String;
  LFamily: String;
  LInterface: String;
  LClass: String;
  LSeen: TStringList;
  LCatalog: TNyxCatalog;
  LRecipe: TNyxNode;

  procedure EmitParts(ANode: TNyxNode; const APrefix: String;
    ADeclarations, AImplementations: Boolean);
  var
    LIndex: Integer;
    LPart: TNyxNode;
    LPath: String;
    LName: String;
    LPartType: String;
  begin
    for LIndex := 0 to ANode.Count - 1 do
    begin
      LPart := ANode.Children[LIndex];
      LPath := String(LPart.Prop(NyxAttributeName(atPart)));

      if LPath = '' then
      begin
        Continue;
      end;

      if APrefix <> '' then
      begin
        LPath := APrefix + '/' + LPath;
      end;
      LName := Stem(LPath);
      LPartType := Stem(LPart.Kind);

      if not EndsText(LPartType, LName) then
      begin
        LName := LName + LPartType;
      end;

      if SameText(LName, 'Label') then
      begin
        LName := 'CaptionLabel';
      end;

      if ADeclarations then
      begin
        GTypes.Add('    { Retains the named ' + LPath + ' part; missing/retyped parts raise a contract error. }');
        GTypes.Add('    function Get' + LName + ': INyx' + LPartType + ';');
        GTypes.Add('    property ' + LName + ': INyx' + LPartType + ' read Get' + LName + ';');
      end;

      if AImplementations then
      begin
        GImplementation.Add('function ' + LClass + '.Get' + LName + ': INyx' + LPartType + ';');
        GImplementation.Add('begin');
        GImplementation.Add('  Result := Part(NyxPart(''' + LPath + ''')) as INyx' + LPartType + ';');
        GImplementation.Add('end;');
        GImplementation.Add('');
      end;
      EmitParts(LPart, LPath, ADeclarations, AImplementations);
    end;
  end;
begin
  LSeen := TStringList.Create;
  LCatalog := TNyxCatalog.Create;
  LRecipe := nil;
  try
    { Named compound accessors may return a later interface in the catalog. }
    for LKind := Low(TNyxKind) to High(TNyxKind) do
    begin
      GTypes.Add('  INyx' + Stem(NyxKindName(LKind)) + ' = interface;');
    end;
    GTypes.Add('');
    for LKind := Low(TNyxKind) to High(TNyxKind) do
    begin
      LStem := Stem(NyxKindName(LKind));
      LInterface := 'INyx' + LStem;
      LClass := 'TNyx' + LStem;
      LFamily := CFamilyNames[CKindFamilies[LKind]];
      FreeAndNil(LRecipe);

      if (LKind >= nkLabeledButton) and (LKind <> nkSlotOverride) then
      begin
        LRecipe := LCatalog.NewNode(LKind, 'contract-recipe');
      end;

      if LSeen.IndexOf(KindGUID(NyxKindName(LKind))) >= 0 then
      begin
        raise Exception.Create('Control interface GUID collision');
      end;
      LSeen.Add(KindGUID(NyxKindName(LKind)));
      GTypes.Add('  { Specialized ' + String(NyxKindName(LKind)) +
        ' contract; inherited properties retain the ' + LFamily + ' family.');
      GTypes.Add('    WithText returns this exact interface for further fluent composition. }');
      GTypes.Add('  ' + LInterface + ' = interface(INyx' + LFamily + ')');
      GTypes.Add('    [''' + KindGUID(NyxKindName(LKind)) + ''']');
      GTypes.Add('    function WithText(const AText: TNyxText): ' + LInterface + ';');

      if LKind in [nkCanvas, nkToolCanvas] then
      begin
        GTypes.Add('    { A distinct saved scene reference; absent means an empty workspace. }');
        GTypes.Add('    function GetScene: TNyxCanvasRef;');
        GTypes.Add('    procedure SetScene(const AScene: TNyxCanvasRef);');
        GTypes.Add('    function WithScene(const AScene: TNyxCanvasRef): ' + LInterface + ';');
        GTypes.Add('    property Scene: TNyxCanvasRef read GetScene write SetScene;');
      end;

      if LKind = nkProgress then
      begin
        GTypes.Add('    { Activity presentation preserves the stored Integer progress value. }');
        GTypes.Add('    function WithIndeterminate(AValue: Boolean): INyxProgress;');
      end;

      if LKind = nkDrawing then
      begin
        GTypes.Add('    { Immutable typed vectors/text; an optional named host provider may replace the plan. }');
        GTypes.Add('    function GetPlan: INyxDrawingPlan;');
        GTypes.Add('    procedure SetPlan(const APlan: INyxDrawingPlan);');
        GTypes.Add('    function WithPlan(const APlan: INyxDrawingPlan): INyxDrawing;');
        GTypes.Add('    function GetProvider: TNyxDrawingProviderRef;');
        GTypes.Add('    procedure SetProvider(const AProvider: TNyxDrawingProviderRef);');
        GTypes.Add('    function WithProvider(const AProvider: TNyxDrawingProviderRef): INyxDrawing;');
        GTypes.Add('    property Plan: INyxDrawingPlan read GetPlan write SetPlan;');
        GTypes.Add('    property Provider: TNyxDrawingProviderRef read GetProvider write SetProvider;');
      end;

      if LKind = nkSlider then
      begin
        GTypes.Add('    { NumberValue reads numeric meaning; writes require a declared Number domain.');
        GTypes.Add('      The inherited Integer Value remains explicit and refuses fractions. }');
        GTypes.Add('    function GetNumberValue: Double;');
        GTypes.Add('    procedure SetNumberValue(AValue: Double);');
        GTypes.Add('    function WithNumber(AValue: Double): INyxSlider;');
        GTypes.Add('    property NumberValue: Double read GetNumberValue write SetNumberValue;');
      end;

      if LKind = nkDate then
      begin
        GTypes.Add('    { Calendar parts are typed; Value is the explicit canonical text boundary. }');
        GTypes.Add('    function GetDateValue: TNyxCalendarDate;');
        GTypes.Add('    procedure SetDateValue(const AValue: TNyxCalendarDate);');
        GTypes.Add('    function WithDate(const AValue: TNyxCalendarDate): INyxDate;');
        GTypes.Add('    property DateValue: TNyxCalendarDate read GetDateValue write SetDateValue;');
      end;

      if LKind = nkTime then
      begin
        GTypes.Add('    { Clock parts/precision are typed; empty remains distinct from midnight.');
        GTypes.Add('      Value is the explicit wire boundary; malformed wire reads refuse. }');
        GTypes.Add('    function GetTimeValue: TNyxClockTime;');
        GTypes.Add('    procedure SetTimeValue(const AValue: TNyxClockTime);');
        GTypes.Add('    function WithTime(const AValue: TNyxClockTime): INyxTime;');
        GTypes.Add('    property TimeValue: TNyxClockTime read GetTimeValue write SetTimeValue;');
      end;

      if LKind = nkColor then
      begin
        GTypes.Add('    { Typed optional RGB; Value is the exact hexadecimal persistence boundary. }');
        GTypes.Add('    function GetColorValue: TNyxRGBColor;');
        GTypes.Add('    procedure SetColorValue(const AValue: TNyxRGBColor);');
        GTypes.Add('    function WithColor(const AValue: TNyxRGBColor): INyxColor;');
        GTypes.Add('    property ColorValue: TNyxRGBColor read GetColorValue write SetColorValue;');
      end;

      if LRecipe <> nil then
      begin
        EmitParts(LRecipe, '', True, False);
      end;
      GTypes.Add('  end;');
      GTypes.Add('');
      GTypes.Add('  { Default managed ' + String(NyxKindName(LKind)) +
        ' implementation. Subclass for behavior or implement the interface');
      GTypes.Add('    independently; adapters require only the retained portable Node contract. }');
      GTypes.Add('  ' + LClass + ' = class(TNyx' + LFamily + ', ' + LInterface + ')');
      GTypes.Add('  public');
      GTypes.Add('    constructor Create(const AID: TNyxText = '''';');
      GTypes.Add('      AConstruction: TNyxConstruction = ncoDefault); reintroduce;');
      GTypes.Add('    function WithText(const AText: TNyxText): ' + LInterface + ';');

      if LKind in [nkCanvas, nkToolCanvas] then
      begin
        GTypes.Add('    function GetScene: TNyxCanvasRef;');
        GTypes.Add('    procedure SetScene(const AScene: TNyxCanvasRef);');
        GTypes.Add('    function WithScene(const AScene: TNyxCanvasRef): ' + LInterface + ';');
        GTypes.Add('    property Scene: TNyxCanvasRef read GetScene write SetScene;');
      end;

      if LKind = nkProgress then
      begin
        GTypes.Add('    function WithIndeterminate(AValue: Boolean): INyxProgress;');
      end;

      if LKind = nkDrawing then
      begin
        GTypes.Add('    function GetPlan: INyxDrawingPlan;');
        GTypes.Add('    procedure SetPlan(const APlan: INyxDrawingPlan);');
        GTypes.Add('    function WithPlan(const APlan: INyxDrawingPlan): INyxDrawing;');
        GTypes.Add('    function GetProvider: TNyxDrawingProviderRef;');
        GTypes.Add('    procedure SetProvider(const AProvider: TNyxDrawingProviderRef);');
        GTypes.Add('    function WithProvider(const AProvider: TNyxDrawingProviderRef): INyxDrawing;');
        GTypes.Add('    property Plan: INyxDrawingPlan read GetPlan write SetPlan;');
        GTypes.Add('    property Provider: TNyxDrawingProviderRef read GetProvider write SetProvider;');
      end;

      if LKind = nkSlider then
      begin
        GTypes.Add('    function GetNumberValue: Double;');
        GTypes.Add('    procedure SetNumberValue(AValue: Double);');
        GTypes.Add('    function WithNumber(AValue: Double): INyxSlider;');
        GTypes.Add('    property NumberValue: Double read GetNumberValue write SetNumberValue;');
      end;

      if LKind = nkDate then
      begin
        GTypes.Add('    function GetDateValue: TNyxCalendarDate;');
        GTypes.Add('    procedure SetDateValue(const AValue: TNyxCalendarDate);');
        GTypes.Add('    function WithDate(const AValue: TNyxCalendarDate): INyxDate;');
        GTypes.Add('    property DateValue: TNyxCalendarDate read GetDateValue write SetDateValue;');
      end;

      if LKind = nkTime then
      begin
        GTypes.Add('    function GetTimeValue: TNyxClockTime;');
        GTypes.Add('    procedure SetTimeValue(const AValue: TNyxClockTime);');
        GTypes.Add('    function WithTime(const AValue: TNyxClockTime): INyxTime;');
        GTypes.Add('    property TimeValue: TNyxClockTime read GetTimeValue write SetTimeValue;');
      end;

      if LKind = nkColor then
      begin
        GTypes.Add('    function GetColorValue: TNyxRGBColor;');
        GTypes.Add('    procedure SetColorValue(const AValue: TNyxRGBColor);');
        GTypes.Add('    function WithColor(const AValue: TNyxRGBColor): INyxColor;');
        GTypes.Add('    property ColorValue: TNyxRGBColor read GetColorValue write SetColorValue;');
      end;

      if LRecipe <> nil then
      begin
        EmitParts(LRecipe, '', True, False);
      end;
      GTypes.Add('  end;');
      GTypes.Add('');
      GFactories.Add('{ Own a managed ' + String(NyxKindName(LKind)) +
        '. Default compounds include independently owned recipe parts;');
      GFactories.Add('  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }');
      GFactories.Add('function NewNyx' + LStem + '(const AID: TNyxText = '''';');
      GFactories.Add('  AConstruction: TNyxConstruction = ncoDefault): ' + LInterface + ';');
      GFactories.Add('');
      GImplementation.Add('constructor ' + LClass + '.Create(const AID: TNyxText;');
      GImplementation.Add('  AConstruction: TNyxConstruction);');
      GImplementation.Add('begin');
      GImplementation.Add('  inherited Create(nk' + LStem + ', AID, AConstruction);');
      GImplementation.Add('end;');
      GImplementation.Add('');
      GImplementation.Add('function ' + LClass + '.WithText(const AText: TNyxText): ' + LInterface + ';');
      GImplementation.Add('begin');
      GImplementation.Add('  Text := AText;');
      GImplementation.Add('  Result := Self as ' + LInterface + ';');
      GImplementation.Add('end;');
      GImplementation.Add('');

      if LKind in [nkCanvas, nkToolCanvas] then
      begin
        GImplementation.Add('function ' + LClass + '.GetScene: TNyxCanvasRef;');
        GImplementation.Add('begin');

        if LKind = nkCanvas then
        begin
          GImplementation.Add('  Result := Node.CanvasScene;');
        end
        else
        begin
          GImplementation.Add('  Result := Node.Part(NyxPart(''workspace'')).CanvasScene;');
        end;
        GImplementation.Add('end;');
        GImplementation.Add('');
        GImplementation.Add('procedure ' + LClass + '.SetScene(const AScene: TNyxCanvasRef);');
        GImplementation.Add('begin');

        if LKind = nkCanvas then
        begin
          GImplementation.Add('  Node.CanvasScene := AScene;');
        end
        else
        begin
          GImplementation.Add('  Node.Part(NyxPart(''workspace'')).CanvasScene := AScene;');
        end;
        GImplementation.Add('end;');
        GImplementation.Add('');
        GImplementation.Add('function ' + LClass + '.WithScene(const AScene: TNyxCanvasRef): ' + LInterface + ';');
        GImplementation.Add('begin');
        GImplementation.Add('  SetScene(AScene);');
        GImplementation.Add('  Result := Self as ' + LInterface + ';');
        GImplementation.Add('end;');
        GImplementation.Add('');
      end;
      if LKind = nkDrawing then
      begin
        GImplementation.Add('function TNyxDrawing.GetPlan: INyxDrawingPlan;');
        GImplementation.Add('begin');
        GImplementation.Add('');
        GImplementation.Add('  if Node.Extensions.Has(NyxExtension(NyxDrawingKey)) then');
        GImplementation.Add('  begin');
        GImplementation.Add('    Result := NyxDrawingPlanFromData(Node.Extensions.Value(NyxExtension(NyxDrawingKey)));');
        GImplementation.Add('  end');
        GImplementation.Add('  else');
        GImplementation.Add('  begin');
        GImplementation.Add('    Result := NewNyxDrawingPlan(NyxCanvasBounds(0, 0, 100, 100)).Build;');
        GImplementation.Add('  end;');
        GImplementation.Add('end;');
        GImplementation.Add('');
        GImplementation.Add('procedure TNyxDrawing.SetPlan(const APlan: INyxDrawingPlan);');
        GImplementation.Add('begin');
        GImplementation.Add('');
        GImplementation.Add('  if APlan = nil then');
        GImplementation.Add('  begin');
        GImplementation.Add('    raise ENyxModel.Create(''Drawing plan is required'');');
        GImplementation.Add('  end;');
        GImplementation.Add('  Node.Extensions.SetValue(NyxExtension(NyxDrawingKey), NyxDrawingPlanFromData(APlan.ToData).ToData);');
        GImplementation.Add('end;');
        GImplementation.Add('');
        GImplementation.Add('function TNyxDrawing.WithPlan(const APlan: INyxDrawingPlan): INyxDrawing;');
        GImplementation.Add('begin');
        GImplementation.Add('  SetPlan(APlan);');
        GImplementation.Add('  Result := Self as INyxDrawing;');
        GImplementation.Add('end;');
        GImplementation.Add('');
        GImplementation.Add('function TNyxDrawing.GetProvider: TNyxDrawingProviderRef;');
        GImplementation.Add('begin');
        GImplementation.Add('  Result := Default(TNyxDrawingProviderRef);');
        GImplementation.Add('');
        GImplementation.Add('  if Node.Extensions.Has(NyxExtension(NyxDrawingProviderKey)) then');
        GImplementation.Add('  begin');
        GImplementation.Add('    Result := NyxDrawingProvider(Node.Extensions.Value(NyxExtension(NyxDrawingProviderKey)).AsText);');
        GImplementation.Add('  end;');
        GImplementation.Add('end;');
        GImplementation.Add('');
        GImplementation.Add('procedure TNyxDrawing.SetProvider(const AProvider: TNyxDrawingProviderRef);');
        GImplementation.Add('begin');
        GImplementation.Add('');
        GImplementation.Add('  if not AProvider.Defined then');
        GImplementation.Add('  begin');
        GImplementation.Add('    Node.Extensions.Remove(NyxExtension(NyxDrawingProviderKey));');
        GImplementation.Add('  end');
        GImplementation.Add('  else');
        GImplementation.Add('  begin');
        GImplementation.Add('    Node.Extensions.SetValue(NyxExtension(NyxDrawingProviderKey), NyxData(NyxDrawingProvider(AProvider.Name).Name));');
        GImplementation.Add('  end;');
        GImplementation.Add('end;');
        GImplementation.Add('');
        GImplementation.Add('function TNyxDrawing.WithProvider(const AProvider: TNyxDrawingProviderRef): INyxDrawing;');
        GImplementation.Add('begin');
        GImplementation.Add('  SetProvider(AProvider);');
        GImplementation.Add('  Result := Self as INyxDrawing;');
        GImplementation.Add('end;');
        GImplementation.Add('');
      end;

      if LKind = nkProgress then
      begin
        GImplementation.Add('function TNyxProgress.WithIndeterminate(AValue: Boolean): INyxProgress;');
        GImplementation.Add('begin');
        GImplementation.Add('  Node.Configure.Indeterminate(AValue).Done;');
        GImplementation.Add('  Result := Self as INyxProgress;');
        GImplementation.Add('end;');
        GImplementation.Add('');
      end;
      GImplementation.Add('function NewNyx' + LStem + '(const AID: TNyxText;');
      GImplementation.Add('  AConstruction: TNyxConstruction): ' + LInterface + ';');
      GImplementation.Add('begin');
      GImplementation.Add('  Result := ' + LClass + '.Create(AID, AConstruction);');
      GImplementation.Add('end;');
      GImplementation.Add('');

      if LKind = nkSlider then
      begin
        GImplementation.Add('function TNyxSlider.GetNumberValue: Double;');
        GImplementation.Add('begin');
        GImplementation.Add('  Result := NyxNodeValueDomain(Node).ReadWire(Node.Prop(''value'', ''0'')).AsNumber;');
        GImplementation.Add('end;');
        GImplementation.Add('');
        GImplementation.Add('procedure TNyxSlider.SetNumberValue(AValue: Double);');
        GImplementation.Add('begin');
        GImplementation.Add('  GetConfigure.Value(AValue);');
        GImplementation.Add('end;');
        GImplementation.Add('');
        GImplementation.Add('function TNyxSlider.WithNumber(AValue: Double): INyxSlider;');
        GImplementation.Add('begin');
        GImplementation.Add('  SetNumberValue(AValue);');
        GImplementation.Add('  Result := Self as INyxSlider;');
        GImplementation.Add('end;');
        GImplementation.Add('');
      end;

      if LKind = nkDate then
      begin
        GImplementation.Add('function TNyxDate.GetDateValue: TNyxCalendarDate;');
        GImplementation.Add('begin');
        GImplementation.Add('  Result := TNyxCalendarDate.FromText(Value);');
        GImplementation.Add('end;');
        GImplementation.Add('');
        GImplementation.Add('procedure TNyxDate.SetDateValue(const AValue: TNyxCalendarDate);');
        GImplementation.Add('begin');
        GImplementation.Add('  GetConfigure.Value(AValue);');
        GImplementation.Add('end;');
        GImplementation.Add('');
        GImplementation.Add('function TNyxDate.WithDate(const AValue: TNyxCalendarDate): INyxDate;');
        GImplementation.Add('begin');
        GImplementation.Add('  SetDateValue(AValue);');
        GImplementation.Add('  Result := Self as INyxDate;');
        GImplementation.Add('end;');
        GImplementation.Add('');
      end;

      if LKind = nkTime then
      begin
        GImplementation.Add('function TNyxTime.GetTimeValue: TNyxClockTime;');
        GImplementation.Add('begin');
        GImplementation.Add('  Result := TNyxClockTime.FromText(Value);');
        GImplementation.Add('end;');
        GImplementation.Add('');
        GImplementation.Add('procedure TNyxTime.SetTimeValue(const AValue: TNyxClockTime);');
        GImplementation.Add('begin');
        GImplementation.Add('  GetConfigure.Value(AValue);');
        GImplementation.Add('end;');
        GImplementation.Add('');
        GImplementation.Add('function TNyxTime.WithTime(const AValue: TNyxClockTime): INyxTime;');
        GImplementation.Add('begin');
        GImplementation.Add('  SetTimeValue(AValue);');
        GImplementation.Add('  Result := Self as INyxTime;');
        GImplementation.Add('end;');
        GImplementation.Add('');
      end;

      if LRecipe <> nil then
      begin
        EmitParts(LRecipe, '', False, True);
      end;

      if LKind = nkColor then
      begin
        GImplementation.Add('function TNyxColor.GetColorValue: TNyxRGBColor;');
        GImplementation.Add('begin');
        GImplementation.Add('  Result := TNyxRGBColor.FromText(Value);');
        GImplementation.Add('end;');
        GImplementation.Add('');
        GImplementation.Add('procedure TNyxColor.SetColorValue(const AValue: TNyxRGBColor);');
        GImplementation.Add('begin');
        GImplementation.Add('  GetConfigure.Value(AValue);');
        GImplementation.Add('end;');
        GImplementation.Add('');
        GImplementation.Add('function TNyxColor.WithColor(const AValue: TNyxRGBColor): INyxColor;');
        GImplementation.Add('begin');
        GImplementation.Add('  SetColorValue(AValue);');
        GImplementation.Add('  Result := Self as INyxColor;');
        GImplementation.Add('end;');
        GImplementation.Add('');
      end;
    end;
    GImplementation.Add('function NewNyxBuiltinControl(AKind: TNyxKind; const AID: TNyxText;');
    GImplementation.Add('  AConstruction: TNyxConstruction): INyxControl;');
    GImplementation.Add('begin');
    GImplementation.Add('  case AKind of');
    for LKind := Low(TNyxKind) to High(TNyxKind) do
    begin
      LStem := Stem(NyxKindName(LKind));
      GImplementation.Add('    nk' + LStem + ':');
      GImplementation.Add('    begin');
      GImplementation.Add('      Result := NewNyx' + LStem + '(AID, AConstruction);');
      GImplementation.Add('    end;');
    end;
    GImplementation.Add('  end;');
    GImplementation.Add('end;');
    GImplementation.Add('');
    GImplementation.Add('function RetainNyxControl(ANode: TNyxNode): INyxControl;');
    GImplementation.Add('var');
    GImplementation.Add('  LKind: TNyxKind;');
    GImplementation.Add('  LReference: INyxNode;');
    GImplementation.Add('begin');
    GImplementation.Add('');
    GImplementation.Add('  if ANode = nil then');
    GImplementation.Add('  begin');
    GImplementation.Add('    raise ENyxModel.Create(''A retained descriptor is required'');');
    GImplementation.Add('  end;');
    GImplementation.Add('');
    GImplementation.Add('  LReference := ANode.ComponentReference;');
    GImplementation.Add('');
    GImplementation.Add('  if (LReference <> nil) and Supports(LReference, INyxControl, Result) then');
    GImplementation.Add('  begin');
    GImplementation.Add('    Exit;');
    GImplementation.Add('  end;');
    GImplementation.Add('');
    GImplementation.Add('  if not TryNyxKind(ANode.Kind, LKind) then');
    GImplementation.Add('  begin');
    GImplementation.Add('    Exit(TNyxControl.CreateFromNode(ANode));');
    GImplementation.Add('  end;');
    GImplementation.Add('  case LKind of');
    for LKind := Low(TNyxKind) to High(TNyxKind) do
    begin
      LStem := Stem(NyxKindName(LKind));
      GImplementation.Add('    nk' + LStem + ':');
      GImplementation.Add('    begin');
      GImplementation.Add('      Result := TNyx' + LStem + '.CreateFromNode(ANode);');
      GImplementation.Add('    end;');
    end;
    GImplementation.Add('  end;');
    GImplementation.Add('end;');
  finally
    LRecipe.Free;
    LCatalog.Free;
    LSeen.Free;
  end;
end;

function FacadeMethod(const ASignature, ASourceClass, AFacade: String): TFacadeMethod;
var
  LSyntax: INyxPascalSyntaxSnapshot;
  LOptions: TNyxPascalSyntaxOptions;
  LBytes: TNyxBytes;
  LDeclaration, LRoutineDeclaration: TNyxPascalSyntaxDeclaration;
  LScope: TNyxPascalSyntaxScope;
  LAt, LRoutine, LSignatureTail, LImplementationEnd: Integer;
  LHeaderToken: TNyxPascalToken;
  LMarks: array of Boolean;
  LCandidate: TFacadeMethod;
  LSelfKey: TNyxText;
  LLastType: TNyxPascalSpan;
  LArguments: TNyxStrings;
  function RawUTF8(const AText: TNyxText): String;
  var
    LEncoded: TNyxBytes;
  begin
    LEncoded := NyxEncodeUTF8(AText);
    SetLength(Result, Length(LEncoded));

    if Length(LEncoded) > 0 then
      Move(LEncoded[0], Result[1], Length(LEncoded));
  end;
  procedure TypeWindow(const ASpan: TNyxPascalSpan; AResult: Boolean);
  var
    LTypes: INyxPascalTypeSyntax;
    LNode, LParent: TNyxPascalTypeNode;
    LIndex, LToken: Integer;
  begin

    if ASpan.Length = 0 then
      Exit;
    LTypes := NewNyxPascalTypeSyntax(LSyntax.Lexical, ASpan,
      TNyxPascalTypeOptions.Defaults);

    if not LTypes.Complete or (LTypes.DiagnosticCount <> 0) then
      raise Exception.Create(CDeclarationFailure + CFacadesInclude + ' has an unsupported facade type');
    for LIndex := 0 to LTypes.Count - 1 do
    begin
      LNode := LTypes.Node(LIndex);

      if (LNode.Kind <> ptxName) or (NyxPascalASCIIKey(LNode.Name) <> LSelfKey) then
        Continue;

      if LNode.Parent >= 0 then
      begin
        LParent := LTypes.Node(LNode.Parent);

        if LParent.Kind = ptxQualified then
          Continue;
      end;
      { The existing facade bridge replaces a direct builder result with Self.
        A parameter or nested result needs an actual conversion bridge; changing
        its type token alone would publish an incompatible delegated call. }

      if not AResult or (LIndex <> LTypes.Root) then
        raise Exception.Create(CDeclarationFailure + CFacadesInclude + ' needs an explicit bridge for a compound or parameter self type');
      LToken := LSyntax.Lexical.TokenAt(LNode.Span.Start);

      if (LSyntax.Lexical.Token(LToken).Span.Start <> LNode.Span.Start) or
        (LSyntax.Lexical.Token(LToken).Span.Length <> LNode.Span.Length) then
        raise Exception.Create(CDeclarationFailure + CFacadesInclude + ' has an inconsistent self type token');
      LMarks[LToken] := True;
      LCandidate.IsFluent := True;
    end;
  end;
  function Header(AEnd: Integer; AQualify: Boolean): String;
  var
    LParts: TNyxStrings;
    LIndex, LStart: Integer;
    LToken: TNyxPascalToken;
  begin
    LParts := TNyxStrings.Create;
    try
      LStart := 0;

      if AQualify then
      begin
        LParts.Add(LSyntax.Lexical.Slice(NyxPascalSpan(0, LRoutineDeclaration.NameSpan.Start)));
        LParts.Add(TNyxText('T' + AFacade + '.'));
        LStart := LRoutineDeclaration.NameSpan.Start;
      end;
      for LIndex := 0 to High(LMarks) do
      begin

        if not LMarks[LIndex] then
          Continue;
        LToken := LSyntax.Lexical.Token(LIndex);

        if (LToken.Span.Start < LStart) or (LToken.Span.Finish > AEnd) then
          raise Exception.Create(CDeclarationFailure + CFacadesInclude + ' has an out-of-header rewrite');
        LParts.Add(LSyntax.Lexical.Slice(NyxPascalSpan(LStart, LToken.Span.Start - LStart)));
        LParts.Add(TNyxText('I' + AFacade));
        LStart := LToken.Span.Finish;
      end;
      LParts.Add(LSyntax.Lexical.Slice(NyxPascalSpan(LStart, AEnd - LStart)));
      Result := RawUTF8(LParts.Join(''));
    finally
      LParts.Free;
    end;
  end;
begin
  Result := Default(TFacadeMethod);
  LCandidate := Default(TFacadeMethod);
  { One owned parse supplies names, forwarding order, header boundaries and
    actual type windows. Literal text, identifiers containing the class name and
    qualified designators are not evidence of a direct self result. }
  LOptions := TNyxPascalSyntaxOptions.Defaults.Fragment(psfInterfaceDeclarations);

  if Length(ASignature) > LOptions.LexicalOptions.MaximumSourceBytes then
    raise Exception.Create(CDeclarationFailure + CFacadesInclude + ' exceeds its forwarding source allowance');
  SetLength(LBytes, Length(ASignature));

  if Length(LBytes) > 0 then
    Move(ASignature[1], LBytes[0], Length(LBytes));
  LSyntax := NewNyxPascalSyntaxSnapshot(NyxDecodeUTF8(LBytes), LOptions);

  if (LSyntax.Lexical.DiagnosticCount <> 0) or (LSyntax.DiagnosticCount <> 0) then
    raise Exception.Create(CDeclarationFailure + CFacadesInclude + ' has an invalid forwarding signature');
  for LAt := 0 to LSyntax.ScopeCount - 1 do
  begin
    LScope := LSyntax.Scope(LAt);

    if not LScope.Complete or LScope.UnsupportedSyntax then
      raise Exception.Create(CDeclarationFailure + CFacadesInclude + ' has incomplete forwarding syntax');
  end;
  LRoutine := -1;
  for LAt := 0 to LSyntax.DeclarationCount - 1 do
  begin
    LDeclaration := LSyntax.Declaration(LAt);

    if (LDeclaration.Flags * [psdfIncomplete, psdfIncompleteHeader,
      psdfUnsupported, psdfConditional]) <> [] then
      raise Exception.Create(CDeclarationFailure + CFacadesInclude + ' has uncertain forwarding syntax');

    if (LDeclaration.Scope = 0) and (LDeclaration.Kind <> psdkRoutine) then
      raise Exception.Create(CDeclarationFailure + CFacadesInclude + ' has an extra forwarding declaration');

    if (LDeclaration.Kind = psdkRoutine) and (LDeclaration.Scope = 0) then
    begin

      if (LRoutine <> -1) or
        not (LDeclaration.RoutineKind in [prkFunction, prkProcedure]) then
        raise Exception.Create(CDeclarationFailure + CFacadesInclude + ' needs one forwarding routine');
      LRoutine := LAt;
    end;
  end;

  if LRoutine = -1 then
    raise Exception.Create(CDeclarationFailure + CFacadesInclude + ' has no forwarding routine');
  LRoutineDeclaration := LSyntax.Declaration(LRoutine);
  LSignatureTail := LRoutineDeclaration.NameSpan.Finish;

  if (LRoutineDeclaration.GenericSpan.Length <> 0) or (LRoutineDeclaration.OwnerName <> '') then
    raise Exception.Create(CDeclarationFailure + CFacadesInclude + ' has a context-dependent facade routine');
  LSelfKey := NyxPascalASCIIKey(TNyxText(ASourceClass));
  SetLength(LMarks, LSyntax.Lexical.Count);
  LCandidate.Name := RawUTF8(LSyntax.Lexical.Slice(LRoutineDeclaration.NameSpan));
  LCandidate.IsFunction := LRoutineDeclaration.RoutineKind = prkFunction;
  LArguments := TNyxStrings.Create;
  try
    LScope := LSyntax.Scope(LRoutineDeclaration.NestedScope);
    LAt := LScope.FirstDeclaration;
    LLastType := NyxPascalSpan(0, 0);
    while LAt <> -1 do
    begin
      LDeclaration := LSyntax.Declaration(LAt);

      if LDeclaration.Kind <> psdkParameter then
        raise Exception.Create(CDeclarationFailure + CFacadesInclude + ' has an unexpected forwarding declaration');
      LArguments.Add(LSyntax.Lexical.Slice(LDeclaration.NameSpan));

      if LDeclaration.NameSpan.Finish > LSignatureTail then
        LSignatureTail := LDeclaration.NameSpan.Finish;

      if LDeclaration.TypeSpan.Finish > LSignatureTail then
        LSignatureTail := LDeclaration.TypeSpan.Finish;

      if LDeclaration.ExpressionSpan.Finish > LSignatureTail then
        LSignatureTail := LDeclaration.ExpressionSpan.Finish;

      if (LDeclaration.TypeSpan.Start <> LLastType.Start) or
        (LDeclaration.TypeSpan.Length <> LLastType.Length) then
      begin
        TypeWindow(LDeclaration.TypeSpan, False);
        LLastType := LDeclaration.TypeSpan;
      end;
      LAt := LDeclaration.Next;
    end;
    LCandidate.Arguments := RawUTF8(LArguments.Join(', '));
  finally
    LArguments.Free;
  end;
  TypeWindow(LRoutineDeclaration.TypeSpan, True);

  if LRoutineDeclaration.TypeSpan.Finish > LSignatureTail then
    LSignatureTail := LRoutineDeclaration.TypeSpan.Finish;
  { HeaderSpan also contains overload/other routine suffixes. The owned type
    and default spans locate the completed signature's semantic tail. Find its
    next real lexical semicolon, never punctuation inside strings/comments or
    earlier nested type/default expressions. Public suffixes remain untouched. }
  LImplementationEnd := -1;
  LAt := LSyntax.Lexical.TokenAt(LSignatureTail);
  while LAt < LSyntax.Lexical.Count do
  begin
    LHeaderToken := LSyntax.Lexical.Token(LAt);

    if LHeaderToken.Span.Start >= LRoutineDeclaration.HeaderSpan.Finish then
      Break;

    if (LHeaderToken.Kind = ptkSymbol) and (LHeaderToken.Symbol = psoSemicolon) then
    begin
      LImplementationEnd := LHeaderToken.Span.Finish;
      Break;
    end;
    Inc(LAt);
  end;

  if LImplementationEnd < 0 then
    raise Exception.Create(CDeclarationFailure + CFacadesInclude + ' has no checked signature terminator');
  LCandidate.PublicHeader := Header(LSyntax.Lexical.Scalars, False);
  LCandidate.ImplementationHeader := Header(LImplementationEnd, True);
  Result := LCandidate;
end;

procedure EmitFacade(const AFile, ASourceClass, AFacade, AMember, AGUID: String);
var
  LLines: TStringList;
  LMethods: TStringList;
  LParsedMethods: TFacadeMethods;
  LIndex: Integer;
  LSignature: String;
  LHeader: String;
  LName: String;
  LInClass: Boolean;
  LPublic: Boolean;
  LFunction: Boolean;
  LFluent: Boolean;
  LCall: String;
  LArguments: String;
begin
  LLines := TStringList.Create;
  LMethods := TStringList.Create;
  try
    LLines.LoadFromFile(AFile);
    LInClass := False;
    LPublic := False;
    LIndex := 0;
    while LIndex < LLines.Count do
    begin
      LSignature := Trim(LLines[LIndex]);

      if LSignature = ASourceClass + ' = class' then
      begin
        LInClass := True;
      end
      else if LInClass and (LSignature = 'public') then
      begin
        LPublic := True;
      end
      else if LInClass and (LSignature = 'end;') then
      begin
        Break;
      end
      else if LPublic and ((Pos('function ', LSignature) = 1) or
        (Pos('procedure ', LSignature) = 1)) then
      begin
        while (Pos(';', LSignature) = 0) or
          ((Pos('(', LSignature) > 0) and (Pos(')', LSignature) = 0)) do
        begin
          Inc(LIndex);
          LSignature := LSignature + ' ' + Trim(LLines[LIndex]);
        end;
        { Methods taking raw stores/returning owned raw clones remain at the
          explicit descriptor boundary, rather than leaking borrowed stores. }

        if (Pos('Assign(', LSignature) = 0) and (Pos('Overlay(', LSignature) = 0) and
          (Pos('Clone:', LSignature) = 0) then
        begin
          LMethods.Add(LSignature);
        end;
      end;
      Inc(LIndex);
    end;

    if LMethods.Count = 0 then
    begin
      raise Exception.Create('No public facade methods: ' + ASourceClass);
    end;
    SetLength(LParsedMethods, LMethods.Count);
    for LIndex := 0 to LMethods.Count - 1 do
      LParsedMethods[LIndex] := FacadeMethod(LMethods[LIndex], ASourceClass, AFacade);
    GFacadeTypes.Add('  { Managed ' + AMember + ' authoring retains its control. Returned values are');
    GFacadeTypes.Add('    independent snapshots; release this interface normally, never Free it. }');
    GFacadeTypes.Add('  I' + AFacade + ' = interface(IInterface)');
    GFacadeTypes.Add('    [''' + AGUID + ''']');
    for LIndex := 0 to LMethods.Count - 1 do
    begin
      LSignature := LParsedMethods[LIndex].PublicHeader;

      if Pos('function Done:', LSignature) = 1 then
      begin
        LSignature := 'function Done: INyxControl;';
      end;

      if (AMember = 'Configure') and (Pos('function MenuBar(', LSignature) = 1) then
      begin
        GFacadeTypes.Add('    { Saved immutable grouping; an explicit mask differs from inheritance. }');
      end;

      if (AMember = 'Configure') and (Pos('TNyxClockTime', LSignature) > 0) then
      begin
        GFacadeTypes.Add('    { Retains exact clock precision; no time is distinct from midnight. }');
      end;

      if (AMember = 'Configure') and (Pos('TNyxRGBColor', LSignature) > 0) then
      begin
        GFacadeTypes.Add('    { Retains exact optional RGB; absence is distinct from black. }');
      end;

      if (AMember = 'Configure') and (Pos('TNyxImageSource', LSignature) > 0) then
      begin
        GFacadeTypes.Add('    { Portable source is copied; fit/anchors remain closed Pascal choices across');
        GFacadeTypes.Add('      default, platform and responsive scopes. No decoder or host is retained. }');
      end;

      if (AMember = 'Binds') and (Pos('function Image(', LSignature) = 1) then
      begin
        GFacadeTypes.Add('    { Read-only image source; locale selection and image-kind admission stay');
        GFacadeTypes.Add('      distinct from scalar text bindings. The facade retains its control. }');
      end;

      if (AMember = 'Configure') and (Pos('function Clear(', LSignature) = 1) then
      begin
        GFacadeTypes.Add('    { Retains an explicit empty property in this exact authored scope. }');
      end;

      if (AMember = 'Configure') and (Pos('function Reset(', LSignature) = 1) then
      begin
        GFacadeTypes.Add('    { Remove this exact scoped property to permit inherited defaults. Repeating');
        GFacadeTypes.Add('      is harmless; portable-only properties refuse target/responsive scopes. }');
      end;

      if (AMember = 'Contract') and (Pos('function Value(', LSignature) = 1) and
        (Pos('TNyxTimeDomain', LSignature) > 0) then
      begin
        GFacadeTypes.Add('    { Distinct clock bounds/choices; every admitted declaration is copied. }');
      end;
      GFacadeTypes.Add('    ' + LSignature);
    end;
    GFacadeTypes.Add('  end;');
    GFacadeTypes.Add('');
    GFacadeTypes.Add('  { Fresh, acyclic facade: it owns the control interface; the control does');
    GFacadeTypes.Add('    not cache this facade. Fluent chains return the same managed object. }');
    GFacadeTypes.Add('  T' + AFacade + ' = class(TInterfacedObject, I' + AFacade + ')');
    GFacadeTypes.Add('  private');
    GFacadeTypes.Add('    FOwner: INyxControl;');

    if AMember = 'Configure' then
    begin
      GFacadeTypes.Add('    FPlatform: TNyxPlatform;');
      GFacadeTypes.Add('    FViewport: TNyxViewportCondition;');
      GFacadeTypes.Add('    FPresentation: TNyxPresentationRef;');
      GFacadeTypes.Add('    function ConfigurationScope: TNyxNodeConfig;');
    end;
    GFacadeTypes.Add('  public');
    GFacadeTypes.Add('    constructor Create(const AOwner: INyxControl);');
    for LIndex := 0 to LMethods.Count - 1 do
    begin
      LSignature := LParsedMethods[LIndex].PublicHeader;

      if Pos('function Done:', LSignature) = 1 then
      begin
        LSignature := 'function Done: INyxControl;';
      end;
      GFacadeTypes.Add('    ' + LSignature);
    end;
    GFacadeTypes.Add('  end;');
    GFacadeTypes.Add('');
    GFacadeImplementation.Add('constructor T' + AFacade + '.Create(const AOwner: INyxControl);');
    GFacadeImplementation.Add('begin');
    GFacadeImplementation.Add('  inherited Create;');
    GFacadeImplementation.Add('');
    GFacadeImplementation.Add('  if AOwner = nil then');
    GFacadeImplementation.Add('  begin');
    GFacadeImplementation.Add('    raise ENyxModel.Create(''A managed facade requires its control'');');
    GFacadeImplementation.Add('  end;');
    GFacadeImplementation.Add('  FOwner := AOwner;');
    GFacadeImplementation.Add('end;');
    GFacadeImplementation.Add('');

    if AMember = 'Configure' then
    begin
      GFacadeImplementation.Add('function TNyxConfiguration.ConfigurationScope: TNyxNodeConfig;');
      GFacadeImplementation.Add('begin');
      GFacadeImplementation.Add('  Result := FOwner.Node.Configure.ForPlatform(FPlatform);');
      GFacadeImplementation.Add('');
      GFacadeImplementation.Add('  if FPresentation.Defined then');
      GFacadeImplementation.Add('  begin');
      GFacadeImplementation.Add('    Result := Result.WhenPresentation(FPresentation);');
      GFacadeImplementation.Add('  end');
      GFacadeImplementation.Add('  else');
      GFacadeImplementation.Add('  begin');
      GFacadeImplementation.Add('    Result := Result.WhenViewport(FViewport);');
      GFacadeImplementation.Add('  end;');
      GFacadeImplementation.Add('end;');
      GFacadeImplementation.Add('');
    end;
    for LIndex := 0 to LMethods.Count - 1 do
    begin
      LFunction := LParsedMethods[LIndex].IsFunction;
      LName := LParsedMethods[LIndex].Name;
      LFluent := LParsedMethods[LIndex].IsFluent;
      LHeader := LParsedMethods[LIndex].ImplementationHeader;

      if LName = 'Done' then
      begin
        LHeader := 'function T' + AFacade + '.Done: INyxControl;';
      end;
      GFacadeImplementation.Add(LHeader);

      if (AMember = 'Configure') and ((LName = 'ForPlatform') or
        (LName = 'WhenViewport') or (LName = 'WhenPresentation')) then
      begin
        GFacadeImplementation.Add('var');
        GFacadeImplementation.Add('  LFacade: TNyxConfiguration;');
      end;
      GFacadeImplementation.Add('begin');
      LCall := 'FOwner.Node.' + AMember + '.' + LName;

      if AMember = 'Configure' then
      begin
        LCall := 'ConfigurationScope.' + LName;
      end;

      LArguments := LParsedMethods[LIndex].Arguments;

      if LArguments <> '' then
      begin
        LCall := LCall + '(' + LArguments + ')';
      end;

      if (AMember = 'Configure') and (LName = 'ForPlatform') then
      begin
        GFacadeImplementation.Add('  LFacade := TNyxConfiguration.Create(FOwner);');
        GFacadeImplementation.Add('  LFacade.FPlatform := APlatform;');
        GFacadeImplementation.Add('  LFacade.FViewport := FViewport;');
        GFacadeImplementation.Add('  LFacade.FPresentation := FPresentation;');
        GFacadeImplementation.Add('  Result := LFacade;');
      end
      else if (AMember = 'Configure') and (LName = 'WhenViewport') then
      begin
        GFacadeImplementation.Add('  LFacade := TNyxConfiguration.Create(FOwner);');
        GFacadeImplementation.Add('  LFacade.FPlatform := FPlatform;');

        if LArguments = 'AWidth' then
        begin
          GFacadeImplementation.Add('  LFacade.FViewport := TNyxViewportCondition.FromWidth(AWidth);');
        end
        else
        begin
          GFacadeImplementation.Add('  LFacade.FViewport := ACondition;');
        end;
        GFacadeImplementation.Add('  Result := LFacade;');
      end
      else if (AMember = 'Configure') and (LName = 'WhenPresentation') then
      begin
        GFacadeImplementation.Add('  ConfigurationScope.WhenPresentation(AReference);');
        GFacadeImplementation.Add('  LFacade := TNyxConfiguration.Create(FOwner);');
        GFacadeImplementation.Add('  LFacade.FPlatform := FPlatform;');
        GFacadeImplementation.Add('  LFacade.FPresentation := AReference;');
        GFacadeImplementation.Add('  Result := LFacade;');
      end
      else if LName = 'Done' then
      begin
        GFacadeImplementation.Add('  Result := FOwner;');
      end
      else if LFluent then
      begin
        GFacadeImplementation.Add('  ' + LCall + ';');

        if (AMember = 'Binds') and (LArguments = 'AExpression') then
        begin
          GFacadeImplementation.Add('  Result := Self;');
        end
        else
        begin
          GFacadeImplementation.Add('  Result := Self as I' + AFacade + ';');
        end;
      end
      else if LFunction then
      begin
        GFacadeImplementation.Add('  Result := ' + LCall + ';');
      end
      else
      begin
        GFacadeImplementation.Add('  ' + LCall + ';');
      end;
      GFacadeImplementation.Add('end;');
      GFacadeImplementation.Add('');
    end;
  finally
    LMethods.Free;
    LLines.Free;
  end;
end;

procedure PrepareInclude(ALines: TStringList);
var
  LLicense: TStringList;
  LSource: String;
  LFinish: Integer;
begin
  LLicense := TStringList.Create;
  try
    LLicense.LoadFromFile(SourcePath('src/nyx.model.pas'));
    LSource := LLicense.Text;
    LFinish := Pos('unit nyx.model;', LSource);

    if LFinish = 0 then
      raise Exception.Create('License source is not the Nyx model unit');
    LSource := Copy(LSource, 1, LFinish - 1);
    LSource := StringReplace(LSource, #13#10, #10, [rfReplaceAll]);
  finally
    LLicense.Free;
  end;
  { Includes contain owned Pascal only. Their source is this maintained tool
    and the existing typed descriptor declarations, not external templates. }
  ALines.Insert(0, '{ Maintained by tools/nyx_control_contracts.lpr; edit the contract source, then regenerate. }');
  ALines.Insert(0, LSource);
  { Stable source ends with exactly one newline, including regenerated facades.
    Trailing blank records otherwise create noise on every contract update. }
  while (ALines.Count > 0) and (Trim(ALines[ALines.Count - 1]) = '') do
  begin
    ALines.Delete(ALines.Count - 1);
  end;
  ALines.LineBreak := #10;
end;

{ Compare serialized bytes, including BOM/newline policy. Text equality would
  conceal a stale encoding or line ending in the actual maintained include. }
function CheckInclude(ALines: TStringList; const ARelative: String): Boolean;
var
  LExpected, LActual: TMemoryStream;
begin
  Result := False;

  if not FileExists(SourcePath(ARelative)) then
  begin
    WriteLn('DIFF missing ', ARelative);
    Exit;
  end;
  LExpected := TMemoryStream.Create;
  LActual := TMemoryStream.Create;
  try
    ALines.SaveToStream(LExpected);
    LActual.LoadFromFile(SourcePath(ARelative));
    Result := (LExpected.Size = LActual.Size);

    if Result and (LExpected.Size > 0) then
      Result := CompareMem(LExpected.Memory, LActual.Memory, LExpected.Size);

    if Result then
      WriteLn('MATCH ', ARelative)
    else
      WriteLn('DIFF content ', ARelative);
  finally
    LActual.Free;
    LExpected.Free;
  end;
end;

{ Creation is exclusive and happens only after all six outputs are prepared.
  Never merge into an old candidate or replace maintained includes; a failed write leaves
  its incomplete candidate for inspection instead of deleting caller files. }
procedure SaveCandidate;
var
  LSourceDirectory: String;
begin

  if not CreateDir(GOptions.CandidateRoot) then
    raise Exception.Create('Cannot create fresh candidate directory: ' + GOptions.CandidateRoot);
  LSourceDirectory := IncludeTrailingPathDelimiter(GOptions.CandidateRoot) + 'src';

  if not CreateDir(LSourceDirectory) then
    raise Exception.Create('Cannot create candidate src directory');
  LSourceDirectory := IncludeTrailingPathDelimiter(LSourceDirectory);
  GTypes.SaveToFile(LSourceDirectory + 'nyx.controls.types.inc');
  GFactories.SaveToFile(LSourceDirectory + 'nyx.controls.factories.inc');
  GImplementation.SaveToFile(LSourceDirectory + 'nyx.controls.implementation.inc');
  GFacadeTypes.SaveToFile(LSourceDirectory + 'nyx.controls.facades.inc');
  GFacadeImplementation.SaveToFile(LSourceDirectory + 'nyx.controls.facades.implementation.inc');
  GDocumentation.SaveToFile(LSourceDirectory + 'nyx.controls.documentation.pas');
end;

function Run: Integer;
var
  LMatches: Boolean;
  LAt: Integer;
  LDeclarations: TContractDeclarations;
begin
  Result := 0;
  GOptions := ReadOptions;

  if GOptions.Help then
  begin
    WriteUsage;
    Exit;
  end;

  if not DirectoryExists(GOptions.SourceRoot) then
    raise Exception.Create('Source root does not exist: ' + GOptions.SourceRoot);

  if (GOptions.Mode = comCandidate) and
    (DirectoryExists(GOptions.CandidateRoot) or FileExists(GOptions.CandidateRoot)) then
    raise Exception.Create('Candidate directory must not already exist: ' + GOptions.CandidateRoot);
  GTypes := TStringList.Create;
  GFactories := TStringList.Create;
  GImplementation := TStringList.Create;
  GFacadeTypes := TStringList.Create;
  GFacadeImplementation := TStringList.Create;
  GDocumentation := TStringList.Create;
  try
    EmitControls;
    for LAt := 0 to High(CFacades) do
      EmitFacade(SourcePath(CFacades[LAt].SourceFile), CFacades[LAt].SourceClass,
        CFacades[LAt].Facade, CFacades[LAt].Member, CFacades[LAt].GUID);
    PrepareInclude(GTypes);
    PrepareInclude(GFactories);
    PrepareInclude(GImplementation);
    PrepareInclude(GFacadeTypes);
    PrepareInclude(GFacadeImplementation);
    { Retain the admitted source IR until candidate/check publication returns.
      No recovered, unsupported or partial declaration snapshot can escape. }
    LDeclarations := TContractDeclarations.Create(GTypes, GFacadeTypes, GFactories);
    try
      LDeclarations.RequireContracts;
      LDeclarations.EmitDocumentation(GDocumentation);
      PrepareInclude(GDocumentation);

      if GOptions.Mode = comCandidate then
      begin
        SaveCandidate;
        WriteLn('PASS candidate contracts for ', Ord(High(TNyxKind)) + 1,
          ' kinds and four managed authoring facades: ', GOptions.CandidateRoot);
      end
      else
      begin
        LMatches := CheckInclude(GTypes, 'src/nyx.controls.types.inc');
        LMatches := CheckInclude(GFactories, 'src/nyx.controls.factories.inc') and LMatches;
        LMatches := CheckInclude(GImplementation, 'src/nyx.controls.implementation.inc') and LMatches;
        LMatches := CheckInclude(GFacadeTypes, 'src/nyx.controls.facades.inc') and LMatches;
        LMatches := CheckInclude(GFacadeImplementation, 'src/nyx.controls.facades.implementation.inc') and LMatches;
        LMatches := CheckInclude(GDocumentation, 'src/nyx.controls.documentation.pas') and LMatches;

        if LMatches then
          WriteLn('PASS maintained contracts for ', Ord(High(TNyxKind)) + 1,
            ' kinds and four managed authoring facades')
        else
          Result := 2;
      end;
    finally
      LDeclarations.Free;
    end;
  finally
    GDocumentation.Free;
    GFacadeImplementation.Free;
    GFacadeTypes.Free;
    GImplementation.Free;
    GFactories.Free;
    GTypes.Free;
  end;
end;

var
  LExitCode: Integer;
begin
  try
    LExitCode := Run;
  except
    on E: Exception do
    begin
      WriteLn(StdErr, 'ERROR ', E.Message);
      LExitCode := 1;
    end;
  end;
  { All managed emitters retire before terminating, including refusal paths. }
  ExitCode := LExitCode;
end.
