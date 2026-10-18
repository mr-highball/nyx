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
program nyx_pascal_profile_tests;
{$mode delphi}{$H+}{$codepage utf8}

uses Classes, SysUtils, LazFileUtils, nyx.text, nyx.bytes, nyx.editing, nyx.scheduler,
  nyx.pascal.context, nyx.pascal.lexing, nyx.pascal.compilertrace, nyx.pascal.encoding,
  nyx.studio.pascalprofiles, nyx.pascal.files, nyx.pascal.files.native,
  nyx.pascal.modules, nyx.pascal.translation, nyx.pascal.names, nyx.pascal.syntax,
  nyx.pascal.project, nyx.pascal.drafts, nyx.pascal.sources,
  nyx.pascal.binding, nyx.pascal.completion, nyx.pascal.completion.owned, nyx.pascal.conditions;

type
  { Cancels at the first post-launch poll. The host must join its exact compiler
    family and release the source lease before returning this expected refusal. }
  TAfterLaunchCancellation = class(TInterfacedObject, INyxCancellationScope)
  private
    FPolls: Integer;
  public
    procedure Cancel;
    function GetCancelled: Boolean;
  end;

var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AReason: String);
begin
  Inc(GChecks);

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
end;

procedure TAfterLaunchCancellation.Cancel;
begin
  FPolls := 3;
end;

function TAfterLaunchCancellation.GetCancelled: Boolean;
begin
  Inc(FPolls);
  Result := FPolls > 2;
end;

function Probe(const AIdentity: TNyxText;
  AMode: TNyxPascalLanguageMode = ppmDelphi): INyxPascalCompilerProbe;
begin
  Result := NewNyxFPCCompilerProbe(NyxPascalDefine(AIdentity), AMode)
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

procedure ReleasedSource(const ADirectory: TNyxText);
var
  LStream: TFileStream;
begin
  LStream := TFileStream.Create(String(IncludeTrailingPathDelimiter(ADirectory) +
    'nyxcompilercontext.pas'), fmOpenReadWrite or fmShareExclusive);
  try
    Check(LStream.Size > 0, 'The joined compiler releases its exact retained private source lease');
  finally
    LStream.Free;
  end;
end;

function NativeProfiles(const ACompiler, ADirectory: TNyxText): INyxPascalCompilerContext;
var
  LOther: INyxPascalCompilerContext;
  LProbe: INyxPascalCompilerProbe;
  LRejected: Boolean;
  LOptions: TNyxPascalCompilerTraceOptions;
begin
  LOptions := TNyxPascalCompilerTraceOptions.Defaults;
  LProbe := Probe('ACTUAL_DELPHI');
  Result := CaptureNyxFPCCompilerContext(ACompiler, ADirectory + '/delphi', LProbe, LOptions);
  Check((Result.Family = ppcFPC) and (Result.Version.FullNumber >= 30301) and
    (Result.Mode = ppmDelphi) and (Result.Option('H') = ppkTrue),
    'A real selected compiler supplies its own numeric version and source defaults');
  Check((Result.Symbol(NyxPascalDefine('WIN32')).IsDefined = ppkTrue) and
    (Result.Symbol(NyxPascalDefine('CPUI386')).IsDefined = ppkTrue) and
    (Result.Symbol(NyxPascalDefine('PAS2JS')).IsDefined = ppkFalse) and
    (Result.Symbol(NyxPascalDefine('FPC_DOTTEDUNITS')).IsDefined = ppkFalse) and
    (Result.Symbol(NyxPascalDefine('UNICODERTL')).IsDefined = ppkFalse),
    'Actual Win32/i386 and explicitly queried RTL identities replace invented host facts');
  Check(not Result.DefinesComplete and
    (Result.Symbol(NyxPascalDefine('UNASKED_FUTURE_SYMBOL')).IsDefined = ppkUnknown) and
    (Result.Feature(ppfAnonymousFunctions) = ppkUnknown),
    'The compiler transcript does not invent global absence or feature enablement');
  Check((NyxPascalCapturedSize(Result, NyxPascalSizeReference('Integer')).Bytes = 4) and
    (NyxPascalCapturedSize(Result, NyxPascalSizeReference('System.Integer')).Bytes = 2) and
    (NyxPascalCapturedSize(Result, NyxPascalSizeReference('Pointer')).Bytes = 4) and
    (NyxPascalCapturedSize(Result, NyxPascalSizeReference('NyxMissingSizeName')).State = pssUndeclared),
    'The selected Win32 compiler keeps pristine mode alias, System export, pointer and absent name distinct');
  Check((NyxPascalCapturedSize(Result, NyxPascalSizeReference('Integer')).ProbeMode = ppmDelphi) and
    (NyxPascalCapturedSize(Result, NyxPascalSizeReference('Integer')).ProbeLongStrings = ppkTrue),
    'Measured sizes retain their exact probe mode and string policy without authored-scope authority');
  WriteLn('actual profile / version=', Result.Version.FullNumber,
    ' observed=', Result.SymbolCount, ' queried=', LProbe.Count);
  ReleasedSource(ADirectory + '/delphi');
  LOther := CaptureNyxFPCCompilerContext(ACompiler, ADirectory + '/objfpc',
    Probe('ACTUAL_OBJFPC', ppmObjFPC), LOptions);
  Check((LOther.Option('H') = ppkFalse) and
    (LOther.Feature(ppfNestedComments) = ppkTrue) and
    (LOther.Symbol(NyxPascalDefine('FPC_OBJFPC')).IsDefined = ppkTrue) and
    (LOther.Symbol(NyxPascalDefine('FPC_DELPHI')).IsDefined = ppkFalse),
    'A different source-mode query captures its own exact native state');
  Check((NyxPascalCapturedSize(LOther, NyxPascalSizeReference('Integer')).Bytes = 4) and
    (NyxPascalCapturedSize(LOther, NyxPascalSizeReference('System.Integer')).Bytes = 2) and
    (NyxPascalCapturedSize(LOther, NyxPascalSizeReference('Integer')).ProbeLongStrings = ppkFalse),
    'ObjFPC measurements remain separately scoped even when their byte counts agree');
  LRejected := False;
  try
    CaptureNyxFPCCompilerContext(ACompiler, ADirectory + '/delphi', LProbe, LOptions);
  except
    on ENyxPascalContext do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Existing native profile inputs cannot be overwritten');
  LRejected := False;
  try
    CaptureNyxFPCCompilerContext(ACompiler, ADirectory + '/cancelled', LProbe,
      LOptions, TAfterLaunchCancellation.Create);
  except
    on ENyxPascalAnalysisCancelled do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Post-launch cancellation refuses the whole context');
  ReleasedSource(ADirectory + '/cancelled');
  LRejected := False;
  try
    CaptureNyxFPCCompilerContext(ACompiler, ADirectory + '/excess', LProbe,
      LOptions.UpToBytes(1));
  except
    on ENyxPascalContext do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Excess compiler output refuses after retiring the exact family');
  ReleasedSource(ADirectory + '/excess');
  LRejected := False;
  try
    CaptureNyxFPCCompilerContext(ACompiler, ADirectory + '/deadline', LProbe,
      LOptions, nil, 1);
  except
    on ENyxPascalContext do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'An expired compiler budget retires its exact family before refusing');
  ReleasedSource(ADirectory + '/deadline');
end;

{ Qualification consumer of detached conditional requests. It reads only actual
  unresolved source spans and their pre-token states; size requirements remain
  recorded scope gaps, never automatic aliases of pristine compiler facts. }
function RTLRequests(const ATranslation: INyxPascalTranslation): INyxPascalCompilerProbe;
var
  LAt: Integer;
  LToken: Integer;
  LNameAt: Integer;
  LName: TNyxText;
  LText: TNyxText;
  LBody: TNyxText;
  LDiagnostic: TNyxPascalTranslationDiagnostic;
  LRow: TNyxPascalTranslatedToken;
  LNeeds: INyxPascalConditionNeeds;
  LQuery: Integer;
  LContext: INyxPascalCompilerContext;
begin
  Result := Probe('RTL_REQUIREMENTS');
  for LAt := 0 to ATranslation.DiagnosticCount - 1 do
  begin
    LDiagnostic := ATranslation.Diagnostic(LAt);

    if LDiagnostic.Issue <> ptiUnknownCondition then
    begin
      Continue;
    end;
    LContext := nil;
    for LToken := 0 to ATranslation.Count - 1 do
    begin
      LRow := ATranslation.Token(LToken);

      if (LRow.Occurrence = LDiagnostic.Occurrence) and
        (ATranslation.PhysicalToken(LToken).Span.Start = LDiagnostic.Span.Start) then
      begin
        LContext := ATranslation.State(LRow.State);
        Break;
      end;
    end;
    Check(LContext <> nil, 'An actual unresolved condition retains its exact pre-token compiler state');
    LText := NyxPascalOccurrenceLexical(ATranslation, LDiagnostic.Occurrence).Slice(LDiagnostic.Span);

    if Copy(LText, 1, 2) = '{$' then
    begin
      LBody := Copy(LText, 3, Length(LText) - 3);
    end
    else
    begin
      Check(Copy(LText, 1, 3) = '(*$', 'Only an actual compiler-directive token enters planning');
      LBody := Copy(LText, 4, Length(LText) - 5);
    end;
    LBody := TNyxText(Trim(LBody));
    LNameAt := 1;
    while (LNameAt <= Length(LBody)) and not (LBody[LNameAt] in [#9, #10, #13, ' ']) do
    begin
      Inc(LNameAt);
    end;
    LName := NyxPascalASCIIKey(Copy(LBody, 1, LNameAt - 1));
    LBody := TNyxText(Trim(Copy(LBody, LNameAt, Length(LBody))));

    if (LName = 'ifdef') or (LName = 'ifndef') then
    begin
      LBody := 'defined(' + LBody + ')';
    end
    else if (LName <> 'if') and (LName <> 'elseif') then
    begin
      Continue;
    end;
    LNeeds := PlanNyxPascalCondition(LContext, LBody);
    WriteLn('actual request / supported=', LNeeds.SyntaxSupported, ' symbols=',
      LNeeds.SymbolCount, ' scoped sizes=', LNeeds.SizeCount, ' ', LBody);
    for LQuery := 0 to LNeeds.SymbolCount - 1 do
    begin
      Result := Result.WithSymbol(LNeeds.Symbol(LQuery));
    end;
  end;
end;

procedure ActualRTL(const ARoot, ACompiler, ASourceRoot, AArtifacts: TNyxText;
  const AContext: INyxPascalCompilerContext);
var
  LCapture: INyxPascalDirectoryCapture;
  LAttempt: INyxPascalDirectoryCapture;
  LFiles: INyxPascalFileCatalog;
  LTranslation: INyxPascalTranslation;
  LMacroContext: INyxPascalTranslationMacros;
  LAt: Integer;
  LTargetContextGap: Boolean;
  LInliningReached: Boolean;
  LConditions: Integer;
  LMissing: Boolean;
  LDiagnostic: TNyxPascalTranslationDiagnostic;
  LFile: TNyxPascalPhysicalFile;
  LRoot: TNyxPascalPhysicalFile;
  LSources: INyxPascalSourceSnapshot;
  LProvider: INyxPascalCompletionProvider;
  LReply: INyxPascalCompletionResult;
  LMetadata: INyxOwnedPascalCompletionResult;
  LPrefix: TNyxText;
  LEncodingRefused: Boolean;
  LBeforeFiles: Integer;
  LBeforeModules: Integer;
  LByteFiles: INyxPascalByteFileCatalog;
  LOpaque: Integer;
  LRaw: INyxPascalSourceBytes;
  LExport: TFileStream;
  LOutput: TNyxBytes;
  LDirectory: TNyxText;
  LRequested: INyxPascalCompilerProbe;
  LVerified: INyxPascalCompilerContext;
  LRemaining: Integer;
  LScoped: INyxPascalSemanticTranslation;
begin
  LCapture := CaptureNyxPascalSourceDirectory(ARoot + '/src',
    NewNyxPascalFileCatalog(pfcExact), NewNyxPascalModuleCatalog,
    TNyxPascalDirectoryOptions.Defaults.Under('sdk'));
  LCapture := CaptureNyxPascalSourceDirectory(ASourceRoot + '/rtl/win',
    LCapture.Files, LCapture.Modules, TNyxPascalDirectoryOptions.Defaults.Under('rtl/win'));
  LBeforeFiles := LCapture.Files.Count;
  LBeforeModules := LCapture.Modules.Count;
  LEncodingRefused := False;
  try
    LAttempt := CaptureNyxPascalSourceDirectory(ASourceRoot + '/rtl/objpas/sysutils',
      LCapture.Files, LCapture.Modules,
      TNyxPascalDirectoryOptions.Defaults.Under('rtl/objpas/sysutils'));
  except
    on LEncoding: ENyxPascalSourceEncoding do
    begin
      LEncodingRefused := True;
      Check(Pos('rtl/objpas/sysutils/', LEncoding.FileReference.Name) = 1,
        'The exact real legacy source is reported without a private host path');
      WriteLn('actual RTL encoding gap / ', LEncoding.FileReference.Name);
    end;
  end;
  Check(LEncodingRefused and (LAttempt = nil) and (LCapture.Files.Count = LBeforeFiles) and
    (LCapture.Modules.Count = LBeforeModules),
    'The actual legacy include directory refuses atomically without rewriting bytes or seeds');
  LCapture := CaptureNyxPascalSourceDirectory(ASourceRoot + '/rtl/objpas/sysutils',
    LCapture.Files, LCapture.Modules, TNyxPascalDirectoryOptions.Defaults
      .Under('rtl/objpas/sysutils').WithSourcePolicy(pscpUTF8ViewsWithOpaqueBytes));
  Supports(LCapture.Files, INyxPascalByteFileCatalog, LByteFiles);
  LDirectory := AArtifacts + '/retained-includes';
  Check(not DirectoryExistsUTF8(LDirectory) and CreateDirUTF8(LDirectory),
    'Whole original include bytes are exported only into a fresh owned evidence directory');
  LOpaque := 0;
  for LAt := LBeforeFiles to LCapture.Files.Count - 1 do
  begin
    LRaw := LByteFiles.SourceBytesAt(LAt);
    LOutput := LRaw.Slice(NyxPascalByteSpan(0, LRaw.Count));
    LExport := TFileStream.Create(String(LDirectory + '/' +
      ExtractFileName(LCapture.Files.FileAt(LAt).Name) + '.bin'), fmCreate);
    try

      if Length(LOutput) > 0 then
      begin
        LExport.WriteBuffer(LOutput[0], Length(LOutput));
      end;
    finally
      LExport.Free;
    end;

    if LByteFiles.SourceViewAt(LAt) = nil then
    begin
      Inc(LOpaque);
      Check(LCapture.Files.FileAt(LAt).Name = TNyxText('rtl/objpas/sysutils/strg.inc'),
        'The complete real include capture reports the original uninterpreted source');
      LRaw := LByteFiles.SourceBytesAt(LAt);
      Check((LRaw.Count = 1239) and (LRaw.ByteAt($39F) = $81),
        'Real non-UTF-8 string bytes survive without any guessed decoding');
    end;
  end;
  Check((LCapture.Files.Count = LBeforeFiles + 48) and
    (LCapture.Modules.Count = LBeforeModules) and (LOpaque = 1),
    'All 48 actual include sources are retained; an opaque view does not drop its file');
  LFiles := LCapture.Files.WithIncludeDirectory('rtl/objpas/sysutils');
  LRoot := LCapture.Modules.Entry(LCapture.Modules.Find(NyxPascalModule('SysUtils'))).Root;
  LTranslation := NewNyxPascalTranslation(LFiles, LRoot, AContext,
    TNyxPascalTranslationOptions.Defaults);
  LMissing := False;
  LTargetContextGap := False;
  LInliningReached := False;
  LConditions := 0;
  for LAt := 0 to LTranslation.DiagnosticCount - 1 do
  begin
    LDiagnostic := LTranslation.Diagnostic(LAt);
    LMissing := LMissing or (LDiagnostic.Issue = ptiMissingInclude);
    LFile := LTranslation.Files.FileAt(LTranslation.Occurrence(LDiagnostic.Occurrence).FileIndex);

    if LDiagnostic.Issue = ptiUnknownCondition then
    begin
      Inc(LConditions);
    end;
    LTargetContextGap := LTargetContextGap or
      ((LDiagnostic.Issue = ptiUnsupportedDirective) and
      (LFile.Name = TNyxText('rtl/objpas/sysutils/syshelph.inc')) and
      (NyxPascalOccurrenceLexical(LTranslation, LDiagnostic.Occurrence).Slice(
        LDiagnostic.Span) = TNyxText('{$fatal Unsupported Integer type size}')));
    WriteLn('actual RTL / ', LFile.Name, ':', LDiagnostic.Span.Start,
      ' issue=', Ord(LDiagnostic.Issue), ' ', LDiagnostic.MessageText);
  end;
  for LAt := 0 to LTranslation.StateCount - 1 do
  begin
    LInliningReached := LInliningReached or
      (NyxPascalCompilerPolicies(LTranslation.State(LAt)).Inlining = ppkTrue);
  end;
  Check((LTranslation.OccurrenceCount > 2) and not LMissing and LInliningReached and
    LTargetContextGap and (LConditions = 38),
    'Actual policies reach the retained target-size/conditional context gaps without invented branch choices');
  WriteLn('actual RTL context / occurrences=', LTranslation.OccurrenceCount,
    ' unknown conditions=', LConditions);
  LRequested := RTLRequests(LTranslation);
  Check(LRequested.Count > Probe('BASELINE').Count,
    'Actual unresolved RTL conditions contribute explicit missing compiler queries');
  LVerified := CaptureNyxFPCCompilerContext(ACompiler, AArtifacts + '/rtl-required',
    LRequested, TNyxPascalCompilerTraceOptions.Defaults);
  LTranslation := NewNyxPascalTranslation(LFiles, LRoot, LVerified,
    TNyxPascalTranslationOptions.Defaults);
  Check(LTranslation.Files.FileAt(LTranslation.Occurrence(0).FileIndex).Name = LRoot.Name,
    'The compiler continuation retains the actual original SysUtils root, independently of diagnostic includes');
  LRemaining := 0;
  for LAt := 0 to LTranslation.DiagnosticCount - 1 do
  begin
    LDiagnostic := LTranslation.Diagnostic(LAt);

    if LDiagnostic.Issue = ptiUnknownCondition then
    begin
      Inc(LRemaining);
    end;
    LFile := LFiles.FileAt(LTranslation.Occurrence(LDiagnostic.Occurrence).FileIndex);
    WriteLn('remaining RTL / ', LFile.Name, ':', LDiagnostic.Span.Start,
      ' issue=', Ord(LDiagnostic.Issue), ' ', LDiagnostic.MessageText);

    if LDiagnostic.Issue = ptiUnknownCondition then
    begin
      WriteLn('remaining expression / ', NyxPascalOccurrenceLexical(LTranslation,
        LDiagnostic.Occurrence).Slice(LDiagnostic.Span));
    end;
  end;
  Check((LRequested.Count = 33) and (LRemaining = 6) and
    not LTranslation.Resolved and not LVerified.DefinesComplete,
    'One bounded real-compiler continuation resolves requested names while retaining source type/syntax gaps');
  WriteLn('actual query continuation / requested=', LRequested.Count,
    ' unknown conditions before=', LConditions, ' after=', LRemaining);
  LRequested := RTLRequests(LTranslation);
  WriteLn('remaining planned symbols / ', LRequested.Count - Probe('BASELINE').Count);
  Check(Supports(LTranslation, INyxPascalTranslationMacros, LMacroContext) and
    (LMacroContext.StateMacros(LTranslation.StateCount - 1).Definition(
      NyxPascalDefine('SUT')).Replacement = TNyxText('sysutils')) and
    (LMacroContext.StateMacros(LTranslation.StateCount - 1).Definition(
      NyxPascalDefine('BU')).Replacement = TNyxText('baseunix')),
    'Actual nondotted RTL retains both unconditional ordered replacement bodies');
  WriteLn('actual capture / files=', LFiles.Count, ' modules=', LCapture.Modules.Count);
  LScoped := NewNyxPascalSemanticTranslation(LFiles, LRoot, LVerified, nil,
    TNyxPascalTranslationOptions.Defaults, TNyxPascalSyntaxOptions.Defaults,
    TNyxPascalConditionalOptions.Defaults);
  WriteLn('actual RTL scoped frontier / answers=', LScoped.Answers,
    ' pending=', LScoped.Pending, ' issue=', Ord(LScoped.BindingIssue), ' ', LScoped.RelatedName);
  Check(LScoped.Pending and not LScoped.Source.Resolved,
    'Actual RTL with an empty auxiliary world retains its scope/import prerequisites without choosing a guessed branch');
  LProvider := NewNyxIndexedPascalCompletionProvider(LVerified, LFiles, LCapture.Modules,
    TNyxOwnedPascalCompletionOptions.Defaults.UpToTokens(4000000));
  LPrefix := 'unit Consumer; {$mode delphi} interface uses nyx.controls; implementation ' +
    'procedure Run; var Badge:INyxBadge; begin Badge.';
  LSources := CaptureNyxPascalSources(NewNyxPascalProject(NyxPascalUnit('Consumer'),
    'unit Consumer; interface implementation end.'))
    .WithBuffer(TNyxPascalSourceRef.Main, LPrefix + 'WithTexx; end; end.');
  LReply := LProvider.Complete(NewNyxPascalCompletionRequest(LSources)
    .At(TNyxPascalSourceRef.Main, NyxTextScalarCount(LPrefix) + 6));
  Supports(LReply, INyxOwnedPascalCompletionResult, LMetadata);
  Check((LReply.Status = pcsUnavailable) and (LMetadata.BindingIssue = pbiIncompleteSource) and
    (LMetadata.RelatedName = TNyxText('SysUtils')),
    'Actual profile/SDK inputs retain incomplete RTL meaning instead of fabricated exports');
end;

begin
  try
    Check(ParamCount = 4, 'Supply repository, configured FPC, FPC source root and fresh E: runtime');
    Check(not DirectoryExistsUTF8(ParamStr(4)) and CreateDirUTF8(ParamStr(4)),
      'The native profile consumer owns a fresh explicit runtime');
    ActualRTL(TNyxText(ParamStr(1)), TNyxText(ParamStr(2)), TNyxText(ParamStr(3)), TNyxText(ParamStr(4)),
      NativeProfiles(TNyxText(ParamStr(2)), TNyxText(ParamStr(4))));
    WriteLn('PASS ', GChecks, ' actual compiler/RTL profile checks');
  except
    on LError: Exception do
    begin
      WriteLn('FAIL / ', LError.Message);
      DumpExceptionBackTrace(StdErr);
      ExitCode := 1;
    end;
  end;
end.
