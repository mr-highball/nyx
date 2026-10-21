{ Native maintenance CLI qualification. MIT license, see LICENSE. }
program nyx_control_contract_generator_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses Classes, SysUtils, Process, nyx.text, nyx.bytes, nyx.types,
  nyx.pascal.context, nyx.pascal.lexing, nyx.pascal.files,
  nyx.pascal.translation, nyx.pascal.syntax,
  nyx.pascal.syntax.translated, nyx.pascal.binding,
  nyx.documentation, nyx.documentation.source,
  nyx.documentation.typeexpressions, nyx.controls.documentation;

const
  CIncludes: array[0..5] of String = ('nyx.controls.types.inc',
    'nyx.controls.factories.inc', 'nyx.controls.implementation.inc',
    'nyx.controls.facades.inc', 'nyx.controls.facades.implementation.inc',
    'nyx.controls.documentation.pas');
  CSources: array[0..2] of String = ('nyx.model.pas', 'nyx.contract.pas', 'nyx.data.pas');
var
  GChecks, GInvocations: Integer;
  GExecutable, GRepository, GRoot: String;

procedure Check(ACondition: Boolean; const AReason: String);
begin
  Inc(GChecks);

  if not ACondition then
    raise Exception.Create(AReason);
end;

function Under(const ARoot, AName: String): String;
begin
  Result := IncludeTrailingPathDelimiter(ARoot) + AName;
end;

function FileBytes(const APath: String): TMemoryStream;
begin
  Result := TMemoryStream.Create;
  try
    Result.LoadFromFile(APath);
  except
    Result.Free;
    raise;
  end;
end;

function SameBytes(const ALeft, ARight: TMemoryStream): Boolean;
begin
  Result := ALeft.Size = ARight.Size;

  if Result and (ALeft.Size > 0) then
    Result := CompareMem(ALeft.Memory, ARight.Memory, ALeft.Size);
end;

function SameFiles(const ALeft, ARight: String): Boolean;
var
  LLeft, LRight: TMemoryStream;
begin
  LLeft := FileBytes(ALeft);
  try
    LRight := FileBytes(ARight);
    try
      Result := SameBytes(LLeft, LRight);
    finally
      LRight.Free;
    end;
  finally
    LLeft.Free;
  end;
end;

function SourceText(const APath: String): TNyxText;
var
  LStream: TMemoryStream;
  LBytes: TNyxBytes;
begin
  LStream := FileBytes(APath);
  try

    if LStream.Size > TNyxPascalLexicalOptions.Defaults.MaximumSourceBytes then
      raise Exception.Create('Actual source exceeds the existing lexical allowance');
    SetLength(LBytes, Integer(LStream.Size));

    if Length(LBytes) > 0 then
      Move(LStream.Memory^, LBytes[0], Length(LBytes));
    Result := NyxDecodeUTF8(LBytes);
  finally
    LStream.Free;
  end;
end;

procedure CreatorSourceSelections;
const
  CMarker: TNyxText = 'function NewNyxBadge(';
  CChanged: TNyxText = 'function NewNyxBadgeStale(';
var
  LFiles, LChangedFiles: INyxPascalFileCatalog;
  LContext: INyxPascalCompilerContext;
  LView: INyxPascalTranslatedSyntax;
  LWorld: INyxPascalSemanticWorld;
  LConstruction: INyxDocumentationTypeConstruction;
  LValues, LCandidate: TNyxDocumentationSourceCreators;
  LSelected: array of Boolean;
  LDeclaration: TNyxPascalSyntaxDeclaration;
  LFactoryText, LChangedText: TNyxText;
  LAt, LPosition, LWork: Integer;
  LRefused: Boolean;

  procedure AddFile(const AName: TNyxText);
  begin
    LFiles := LFiles.WithFile(NyxPascalPhysicalFile(AName),
      SourceText(Under(GRepository, 'src/' + String(AName))));
  end;

  procedure SelectWorld(const AFiles: INyxPascalFileCatalog);
  begin
    LView := NewNyxPascalTranslatedSyntax(NewNyxPascalTranslation(AFiles,
      NyxPascalPhysicalFile('nyx.controls.pas'), LContext,
      TNyxPascalTranslationOptions.Defaults), TNyxPascalSyntaxOptions.Defaults);
    Check(LView.CompleteTranslation and LView.Source.Resolved and
      (LView.Source.DiagnosticCount = 0) and (LView.Syntax.DiagnosticCount = 0),
      'Creator selection inspects the complete actual unit and all five literal includes');
    { The selector inspects declared factory shapes only. This exact owned world
      intentionally makes no claim about resolving imports or nominal bindings;
      those remain prerequisites of the full source-package integration. }
    LWorld := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults).WithModule(LView);
  end;

begin
  LFiles := NewNyxPascalFileCatalog(pfcExact);
  AddFile('nyx.controls.pas');
  AddFile('nyx.controls.types.inc');
  AddFile('nyx.controls.facades.inc');
  AddFile('nyx.controls.factories.inc');
  AddFile('nyx.controls.implementation.inc');
  AddFile('nyx.controls.facades.implementation.inc');
  LContext := NewNyxPascalCompilerContext(ppcFPC,
    TNyxPascalCompilerVersion.Create(3, 3, 1), ppmDelphi).
    WithSymbol(NyxPascalDefine('PAS2JS'), ppdUndefined);
  SelectWorld(LFiles);
  LConstruction := NewNyxDocumentationTypeConstruction(TNyxDocumentationTypeExpressionLimits.Defaults);
  try
    LValues := NyxControlsDocumentationCreators(LWorld, 0, LConstruction);
    LWork := LConstruction.Receipt.Total;
    Check(Length(LValues) = Ord(High(TNyxKind)) + 1,
      'The generated companion selects every built-in creator from actual source');
    SetLength(LSelected, LView.Syntax.DeclarationCount);
    for LAt := 0 to High(LValues) do
    begin
      Check((LValues[LAt].ModuleIndex = 0) and
        (LValues[LAt].DeclarationIndex >= 0) and
        (LValues[LAt].DeclarationIndex < LView.Syntax.DeclarationCount),
        'Each annotation belongs to a real declaration in this exact world');
      LDeclaration := LView.Syntax.Declaration(LValues[LAt].DeclarationIndex);
      Check((LDeclaration.Kind = psdkRoutine) and
        (LDeclaration.RoutineKind = prkFunction) and
        (LDeclaration.Visibility = psavInterface) and (LDeclaration.Scope = 0),
        'Each generated creator selects an actual public global function');
      Check(not LSelected[LValues[LAt].DeclarationIndex],
        'Two built-in kinds never acquire the same source creator annotation');
      LSelected[LValues[LAt].DeclarationIndex] := True;
    end;
    LDeclaration := LView.Syntax.Declaration(LValues[Ord(nkBadge)].DeclarationIndex);
    Check((LDeclaration.Name = 'NewNyxBadge') and
      (LView.Syntax.Lexical.Slice(LDeclaration.TypeSpan) = 'INyxBadge'),
      'Badge creator annotation retains its actual specialized interface result');
    Check(LWork <= TNyxDocumentationTypeExpressionLimits.Defaults.MaximumWork,
      'All creator shape checks share the unchanged construction allowance');
  finally
    LConstruction.Retire;
  end;
  LCandidate := nil;
  LRefused := False;
  try
    LCandidate := NyxControlsDocumentationCreators(LWorld, 0, LConstruction);
  except
    on ENyxDocumentationTypeExpressionCancelled do LRefused := True;
  end;
  Check(LRefused and (Length(LCandidate) = 0) and
    (Length(LValues) = Ord(High(TNyxKind)) + 1),
    'Retired construction refuses another selector operation without replacing prior annotations');

  { Change one real factory declaration in an immutable copied file catalog.
    All other original unit/include bytes remain present; no repository file is
    written and no reduced substitute source can satisfy this stale-input case. }
  LFactoryText := SourceText(Under(GRepository, 'src/nyx.controls.factories.inc'));
  LPosition := Pos(CMarker, LFactoryText);
  Check(LPosition > 0, 'Stale-source fixture finds the actual badge factory declaration');
  LChangedText := Copy(LFactoryText, 1, LPosition - 1) + CChanged +
    Copy(LFactoryText, LPosition + Length(CMarker), Length(LFactoryText));
  LChangedFiles := LFiles.WithFile(NyxPascalPhysicalFile('nyx.controls.factories.inc'), LChangedText);
  SelectWorld(LChangedFiles);
  LConstruction := NewNyxDocumentationTypeConstruction(TNyxDocumentationTypeExpressionLimits.Defaults);
  LRefused := False;
  try
    try
      LCandidate := NyxControlsDocumentationCreators(LWorld, 0, LConstruction);
    except
      on ENyxDocumentation do LRefused := True;
    end;
    Check(LRefused and (Length(LCandidate) = 0) and
      (Length(LValues) = Ord(High(TNyxKind)) + 1),
      'A stale real factory name refuses atomically while prior annotations remain readable');
  finally
    LConstruction.Retire;
  end;
  WriteLn('actual generated creator selections / kinds=', Length(LValues),
    ' shared work=', LWork, ' / unbound declaration shapes only');
end;

procedure CopyBytes(const AFrom, ATo: String);
var
  LBytes: TMemoryStream;
begin
  LBytes := FileBytes(AFrom);
  try
    LBytes.SaveToFile(ATo);
  finally
    LBytes.Free;
  end;
end;

{ Launch the actual compiled maintenance executable, with structured arguments
  and no shell. Each terminal transcript survives in the fresh test directory;
  this fixture never launches a compiler, browser, server or generated program. }
function Invoke(const ADirectory: String; const AArguments: array of TProcessString;
  AExpectedExit: Integer): String;
var
  LExitStatus, LLaunchStatus: Integer;
  LLog: TStringList;
begin
  Inc(GInvocations);
  LExitStatus := -1;
  LLaunchStatus := RunCommandInDir(ADirectory, GExecutable, AArguments, Result,
    LExitStatus, [poStderrToOutPut, poNoConsole], swoHIDE);
  LLog := TStringList.Create;
  try
    LLog.Text := Result;
    LLog.SaveToFile(Under(GRoot, 'case-' + IntToStr(GInvocations) + '.log'));
  finally
    LLog.Free;
  end;
  Check((LLaunchStatus = 0) and (LExitStatus = AExpectedExit),
    'Generator invocation ' + IntToStr(GInvocations) + ' has expected terminal status');
  Check(Pos('0 unfreed memory blocks : 0', Result) > 0,
    'Generator invocation ' + IntToStr(GInvocations) + ' retires with a clean heap');
end;

procedure Run;
var
  LIndex: Integer;
  LFirst, LSecond, LReplica, LMissing, LRefused, LFile, LOutput, LInvalid: String;
  LForwarding, LForwardCandidate: String;
  LBridge: String;
  LBridgeCase: Integer;
  LOriginal: array[0..8] of TMemoryStream;
  LCurrent: TMemoryStream;
  LStream: TFileStream;
  LMarker: RawByteString;
  LDescriptor: TStringList;
  LOriginalDeclaration, LInvalidDeclaration: String;
begin

  if ParamCount <> 3 then
    raise Exception.Create('Expected generator executable, repository root and fresh runtime directory');
  GExecutable := ExpandFileName(ParamStr(1));
  GRepository := ExpandFileName(ParamStr(2));
  GRoot := ExpandFileName(ParamStr(3));

  if DirectoryExists(GRoot) or FileExists(GRoot) or not CreateDir(GRoot) then
    raise Exception.Create('Qualification runtime must be a new directory');
  FillChar(LOriginal, SizeOf(LOriginal), 0);
  try
    for LIndex := 0 to High(CIncludes) do
      LOriginal[LIndex] := FileBytes(Under(GRepository, 'src/' + CIncludes[LIndex]));
    for LIndex := 0 to High(CSources) do
      LOriginal[LIndex + Length(CIncludes)] := FileBytes(Under(GRepository, 'src/' + CSources[LIndex]));

    LOutput := Invoke(GRepository, [], 0);
    Check(Pos('PASS maintained contracts', LOutput) > 0,
      'Default invocation checks the five maintained includes and creator-documentation companion');
    Check(Pos('PASS typed declaration IR / 81 kinds / 4 facades', LOutput) > 0,
      'Every actual generated interface/class/factory and facade reaches typed declaration admission');
    LFirst := Under(GRoot, 'candidate one');
    LSecond := Under(GRoot, 'candidate two');
    LOutput := Invoke(GRoot, ['--source-root', GRepository, '--output-dir', LFirst], 0);
    Check(Pos('PASS candidate contracts', LOutput) > 0,
      'Explicit candidate generation works outside the repository with spaces in paths');
    Invoke(GRoot, ['--source-root', GRepository, '--output-dir', LSecond], 0);
    for LIndex := 0 to High(CIncludes) do
    begin
      Check(SameFiles(Under(LFirst, 'src/' + CIncludes[LIndex]),
        Under(GRepository, 'src/' + CIncludes[LIndex])),
        'Candidate retains exact maintained bytes for ' + CIncludes[LIndex]);
      Check(SameFiles(Under(LFirst, 'src/' + CIncludes[LIndex]),
        Under(LSecond, 'src/' + CIncludes[LIndex])),
        'Repeated generation is deterministic for ' + CIncludes[LIndex]);
    end;
    Invoke(GRoot, ['--source-root', GRepository, '--output-dir', LFirst], 1);
    for LIndex := 0 to High(CIncludes) do
      Check(SameFiles(Under(LFirst, 'src/' + CIncludes[LIndex]),
        Under(LSecond, 'src/' + CIncludes[LIndex])),
        'Existing candidate refusal preserves every file');

    LFile := Under(GRoot, 'caller-owned-file');
    LMarker := 'Preserve this caller-owned candidate name.';
    LStream := TFileStream.Create(LFile, fmCreate);
    try
      LStream.WriteBuffer(LMarker[1], Length(LMarker));
    finally
      LStream.Free;
    end;
    Invoke(GRoot, ['--source-root', GRepository, '--output-dir', LFile], 1);
    LCurrent := FileBytes(LFile);
    try
      Check((LCurrent.Size = Length(LMarker)) and
        CompareMem(LCurrent.Memory, @LMarker[1], Length(LMarker)),
        'An existing file is neither replaced nor treated as a candidate directory');
    finally
      LCurrent.Free;
    end;
    Invoke(GRoot, ['--unknown'], 1);
    Invoke(GRoot, ['--output-dir'], 1);
    Invoke(GRoot, ['--source-root', GRepository, '--source-root', GRepository], 1);
    LRefused := Under(GRoot, 'conflicting candidate');
    Invoke(GRoot, ['--check', '--output-dir', LRefused], 1);
    Check(not DirectoryExists(LRefused), 'Conflicting modes refuse before filesystem publication');
    LOutput := Invoke(GRoot, ['--help'], 0);
    Check(Pos('--output-dir <new-directory>', LOutput) > 0,
      'Help needs no descriptor inputs in the working directory');

    { A copied source tree is the only mutated fixture. No maintained include is
      edited or removed to demonstrate drift and missing-output behavior. }
    LReplica := Under(GRoot, 'replica');
    LMissing := Under(GRoot, 'missing outputs');
    Check(CreateDir(LReplica) and CreateDir(Under(LReplica, 'src')) and
      CreateDir(LMissing) and CreateDir(Under(LMissing, 'src')), 'Create isolated source fixtures');
    for LIndex := 0 to High(CSources) do
    begin
      CopyBytes(Under(GRepository, 'src/' + CSources[LIndex]), Under(LReplica, 'src/' + CSources[LIndex]));
      CopyBytes(Under(GRepository, 'src/' + CSources[LIndex]), Under(LMissing, 'src/' + CSources[LIndex]));
    end;
    for LIndex := 0 to High(CIncludes) do
      CopyBytes(Under(LFirst, 'src/' + CIncludes[LIndex]), Under(LReplica, 'src/' + CIncludes[LIndex]));
    Invoke(LReplica, ['--check'], 0);
    LStream := TFileStream.Create(Under(LReplica, 'src/' + CIncludes[0]), fmOpenWrite);
    try
      LStream.Seek(0, soEnd);
      LStream.WriteBuffer(LMarker[1], Length(LMarker));
    finally
      LStream.Free;
    end;
    LOutput := Invoke(LReplica, ['--check'], 2);
    Check(Pos('DIFF content src/' + CIncludes[0], LOutput) > 0,
      'Byte drift returns a distinct status and names the stale include');
    LCurrent := FileBytes(Under(LReplica, 'src/' + CIncludes[0]));
    try
      Check(LCurrent.Size = LOriginal[0].Size + Length(LMarker),
        'Check mode leaves the drifted source intact');
    finally
      LCurrent.Free;
    end;
    LOutput := Invoke(LMissing, [], 2);
    Check((Pos('DIFF missing src/' + CIncludes[0], LOutput) > 0) and
      (Pos('DIFF missing src/' + CIncludes[5], LOutput) > 0),
      'Missing output checking inspects the entire maintained output set');
    Check(not FileExists(Under(LMissing, 'src/' + CIncludes[0])),
      'Default checking does not fill missing source files');
    LRefused := Under(GRoot, 'unprepared candidate');
    Invoke(GRoot, ['--output-dir', LRefused], 1);
    Check(not DirectoryExists(LRefused), 'Failed descriptor preparation publishes no candidate');

    { The maintenance scanner still extracts existing public descriptor methods.
      A balanced but malformed parameter header used to pass that scanner and
      escape as invalid Pascal. Admission now uses the existing syntax owner,
      without compiling or executing this deliberately invalid source fixture. }
    LInvalid := Under(GRoot, 'invalid declaration');
    Check(CreateDir(LInvalid) and CreateDir(Under(LInvalid, 'src')),
      'Create a separate invalid descriptor fixture');
    for LIndex := 0 to High(CSources) do
      CopyBytes(Under(GRepository, 'src/' + CSources[LIndex]), Under(LInvalid, 'src/' + CSources[LIndex]));
    LDescriptor := TStringList.Create;
    try
      LFile := Under(LInvalid, 'src/nyx.model.pas');
      LDescriptor.LoadFromFile(LFile);
      LOriginalDeclaration := 'function Placeholder(const AValue: TNyxText): TNyxNodeConfig;';
      LInvalidDeclaration := 'function Placeholder(const AValue: ): TNyxNodeConfig;';
      Check(Pos(LOriginalDeclaration, LDescriptor.Text) > 0,
        'The negative fixture changes an actual current descriptor method');
      LDescriptor.Text := StringReplace(LDescriptor.Text, LOriginalDeclaration,
        LInvalidDeclaration, []);
      LDescriptor.SaveToFile(LFile);
    finally
      LDescriptor.Free;
    end;
    LOutput := Invoke(LInvalid, ['--check'], 1);
    Check(Pos('Generated declarations refused: src/nyx.controls.facades.inc', LOutput) > 0,
      'Malformed emitted signature refuses through the typed facade source owner');
    LRefused := Under(GRoot, 'invalid declaration candidate');
    Invoke(LInvalid, ['--output-dir', LRefused], 1);
    Check(not DirectoryExists(LRefused),
      'Typed source refusal precedes every candidate directory or file write');

    { Punctuation in an actual descriptor's default value must never become
      another forwarded argument. Added ordinary method headers also exercise
      grouped const/var/out parameters, escaped names and a constant expression.
      Only this copied source changes; the generated program is never executed. }
    LForwarding := Under(GRoot, 'forwarding declarations');
    Check(CreateDir(LForwarding) and CreateDir(Under(LForwarding, 'src')),
      'Create an isolated forwarding descriptor fixture');
    for LIndex := 0 to High(CSources) do
      CopyBytes(Under(GRepository, 'src/' + CSources[LIndex]), Under(LForwarding, 'src/' + CSources[LIndex]));
    LDescriptor := TStringList.Create;
    try
      LFile := Under(LForwarding, 'src/nyx.model.pas');
      LDescriptor.LoadFromFile(LFile);
      Check(Pos(LOriginalDeclaration, LDescriptor.Text) > 0,
        'The forwarding fixture extends an actual maintained method');
      LDescriptor.Text := StringReplace(LDescriptor.Text, LOriginalDeclaration,
        'function Placeholder(const AValue: TNyxText = ''left;middle:right''): TNyxNodeConfig;' + LineEnding +
        '    procedure ForwardValues(const AFirst, ASecond: TNyxText; var AThird: TNyxText; out AFourth: TNyxText);' + LineEnding +
        '    function ForwardEscaped(const &type: TNyxText): TNyxNodeConfig;' + LineEnding +
        '    function ForwardExpression(const AValue: Integer = Ord('';'')): TNyxNodeConfig;' + LineEnding +
        '    function TNyxNodeConfigCaption(const ATNyxNodeConfigText: TNyxText = ''TNyxNodeConfig''): TNyxText;' + LineEnding +
        '    function KeepTypeName(const AValue: TNyxNodeConfigSuffix): TNyxNodeConfig;' + LineEnding +
        '    function KeepNamespace(const AValue: TNyxNodeConfig.TLeaf): TNyxNodeConfig;' + LineEnding +
        '    function ConstantDefault(const AValue: TNyxText = '': TNyxNodeConfig;''): TNyxText;' + LineEnding +
        '    function KeepCallback(const AValue: procedure(TNyxNodeConfig: TNyxText)): TNyxNodeConfig;' + LineEnding +
        '    function ReturnTyped: { TNyxNodeConfig remains comment text } TNyxNodeConfig;', []);
      LDescriptor.SaveToFile(LFile);
    finally
      LDescriptor.Free;
    end;
    LForwardCandidate := Under(GRoot, 'forwarding candidate');
    LOutput := Invoke(LForwarding, ['--output-dir', LForwardCandidate], 0);
    Check(Pos('PASS typed declaration IR / 81 kinds / 4 facades', LOutput) > 0,
      'Forwarding fixtures retain the full actual generated contract admission');
    LDescriptor := TStringList.Create;
    try
      LDescriptor.LoadFromFile(Under(LForwardCandidate, 'src/nyx.controls.facades.inc'));
      Check(Pos('function Placeholder(const AValue: TNyxText = ''left;middle:right''): INyxConfiguration;',
        LDescriptor.Text) > 0, 'Default punctuation survives in the declared public signature');
      Check(Pos('function TNyxNodeConfigCaption(const ATNyxNodeConfigText: TNyxText = ''TNyxNodeConfig''): TNyxText;',
        LDescriptor.Text) > 0, 'Method/argument identifiers and inert default text retain the source class substring');
      Check(Pos('function KeepTypeName(const AValue: TNyxNodeConfigSuffix): INyxConfiguration;',
        LDescriptor.Text) > 0, 'A longer type identifier stays exact while a direct self result becomes the facade');
      Check(Pos('function KeepNamespace(const AValue: TNyxNodeConfig.TLeaf): INyxConfiguration;',
        LDescriptor.Text) > 0, 'Namespace components do not acquire an inferred facade identity');
      Check(Pos('function KeepCallback(const AValue: procedure(TNyxNodeConfig: TNyxText)): INyxConfiguration;',
        LDescriptor.Text) > 0, 'A procedural parameter name is not rewritten as a type name');
      Check(Pos('function ReturnTyped: { TNyxNodeConfig remains comment text } INyxConfiguration;',
        LDescriptor.Text) > 0, 'Only the admitted type token changes; surrounding comment spelling remains exact');
      LDescriptor.LoadFromFile(Under(LForwardCandidate, 'src/nyx.controls.facades.implementation.inc'));
      Check(Pos('  ConfigurationScope.Placeholder(AValue);', LDescriptor.Text) > 0,
        'Default string punctuation contributes no extra forwarding argument');
      Check(Pos('  ConfigurationScope.ForwardValues(AFirst, ASecond, AThird, AFourth);', LDescriptor.Text) > 0,
        'Grouped names and const/var/out modes forward exactly once in declaration order');
      Check(Pos('  ConfigurationScope.ForwardEscaped(&type);', LDescriptor.Text) > 0,
        'Forwarded escaped parameter retains its authored ampersand');
      Check(Pos('  ConfigurationScope.ForwardExpression(AValue);', LDescriptor.Text) > 0,
        'Nested constant-expression punctuation is never treated as parameter syntax');
      Check(Pos('  Result := ConfigurationScope.TNyxNodeConfigCaption(ATNyxNodeConfigText);', LDescriptor.Text) > 0,
        'Source method and argument names survive in the actual delegated implementation');
      Check(Pos('function TNyxConfiguration.ConstantDefault(const AValue: TNyxText = '': TNyxNodeConfig;''): TNyxText;',
        LDescriptor.Text) > 0, 'The implementation header ends at the parsed terminator, preserving default semicolons');
      Check(Pos('  Result := ConfigurationScope.ConstantDefault(AValue);', LDescriptor.Text) > 0,
        'Self-like literal text does not turn a non-self result into a fluent result');
    finally
      LDescriptor.Free;
    end;

    { No array/parameter conversion exists in this facade. Both shapes must
      refuse before disk publication, rather than merely changing a type token
      and generating an incompatible delegated call. Each is a copied fixture;
      no generated program or undefined imported type is compiled/executed. }
    for LBridgeCase := 0 to 1 do
    begin
      LBridge := Under(GRoot, 'unbridged self ' + IntToStr(LBridgeCase));
      Check(CreateDir(LBridge) and CreateDir(Under(LBridge, 'src')),
        'Create a copied descriptor for the explicit typed-bridge refusal');
      for LIndex := 0 to High(CSources) do
        CopyBytes(Under(GRepository, 'src/' + CSources[LIndex]), Under(LBridge, 'src/' + CSources[LIndex]));
      LDescriptor := TStringList.Create;
      try
        LFile := Under(LBridge, 'src/nyx.model.pas');
        LDescriptor.LoadFromFile(LFile);

        if LBridgeCase = 0 then
          LInvalidDeclaration := 'function Unbridged(const AValue: TNyxNodeConfig): TNyxNodeConfig;'
        else
          LInvalidDeclaration := 'function Unbridged: TArray<TNyxNodeConfig>;';
        LDescriptor.Text := StringReplace(LDescriptor.Text, LOriginalDeclaration,
          LOriginalDeclaration + LineEnding + '    ' + LInvalidDeclaration, []);
        LDescriptor.SaveToFile(LFile);
      finally
        LDescriptor.Free;
      end;
      LOutput := Invoke(LBridge, ['--check'], 1);
      Check(Pos('needs an explicit bridge for a compound or parameter self type', LOutput) > 0,
        'Unsupported self use refuses through the checked type shape');
      LRefused := Under(GRoot, 'unbridged self candidate ' + IntToStr(LBridgeCase));
      Invoke(LBridge, ['--output-dir', LRefused], 1);
      Check(not DirectoryExists(LRefused), 'Typed bridge refusal precedes every candidate directory/file write');
    end;

    CreatorSourceSelections;
    for LIndex := 0 to High(LOriginal) do
    begin

      if LIndex < Length(CIncludes) then
        LFile := Under(GRepository, 'src/' + CIncludes[LIndex])
      else
        LFile := Under(GRepository, 'src/' + CSources[LIndex - Length(CIncludes)]);
      LCurrent := FileBytes(LFile);
      try
        Check(SameBytes(LOriginal[LIndex], LCurrent), 'All original descriptor/include bytes remain exact');
      finally
        LCurrent.Free;
      end;
    end;
  finally
    for LIndex := 0 to High(LOriginal) do
      LOriginal[LIndex].Free;
  end;
end;

begin
  try
    Run;
    WriteLn('PASS ', GChecks, ' control-contract generator checks / ', GInvocations,
      ' joined maintenance invocations');
  except
    on E: Exception do
    begin
      { Retire the handled assertion after reporting its actual source trace.
        A failed consumer must not leave an unhandled-exception heap artifact. }
      WriteLn(StdErr, E.ClassName, ': ', E.Message);
      DumpExceptionBackTrace(StdErr);
      ExitCode := 1;
    end;
  end;
end.
