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
program nyx_pascal_moduleprepare_native;
{$mode delphi}{$H+}{$codepage utf8}

uses Classes, SysUtils, LazFileUtils, nyx.text, nyx.bytes, nyx.scheduler,
  nyx.pascal.files, nyx.pascal.files.native, nyx.pascal.encoding, nyx.pascal.modules, nyx.pascal.context,
  nyx.pascal.compilertrace, nyx.pascal.compilerplan, nyx.pascal.translation,
  nyx.pascal.syntax, nyx.pascal.syntax.translated, nyx.pascal.conditions,
  nyx.pascal.binding, nyx.studio.pascalprofiles, nyx.pascal.lexing,
  nyx.pascal.project, nyx.pascal.names, nyx.pascal.drafts, nyx.pascal.sources,
  nyx.pascal.completion, nyx.pascal.completion.owned, nyx.editing, preparation.fixture;
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

procedure Controlled(const ARepo, ACompiler, AOutput: TNyxText);
var
  LCapture: INyxPascalDirectoryCapture;
  LContext: INyxPascalCompilerContext;
  LProbe: INyxPascalCompilerProbe;
  LModule: INyxPascalModuleSource;
  LPrepared: INyxFPCModulePreparation;
  LOther: INyxFPCModulePreparation;
  LProvider: INyxPascalCompletionProvider;
  LSources: INyxPascalSourceSnapshot;
  LReply: INyxPascalCompletionResult;
  LPrefix: TNyxText;
  LCancel: INyxCancellationScope;
  LFailed: Boolean;
begin
  Check(RunPreparedValue = 23, 'The whole original unit/include executes the actual compiler-selected branch');
  LCapture := CaptureNyxPascalSourceDirectory(ARepo + '/tests', NewNyxPascalFileCatalog(pfcExact),
    NewNyxPascalModuleCatalog, TNyxPascalDirectoryOptions.Defaults.Under('source')
      .OnlyFile('preparation.fixture.pas'));
  LCapture := CaptureNyxPascalSourceDirectory(ARepo + '/tests', LCapture.Files, LCapture.Modules,
    TNyxPascalDirectoryOptions.Defaults.Under('source').OnlyFile('preparation.fixture.inc'));
  LProbe := NewNyxFPCCompilerProbe(NyxPascalDefine('PREPARATION'), ppmDelphi);
  LContext := CaptureNyxFPCCompilerContext(ACompiler, AOutput + '/base', LProbe,
    TNyxPascalCompilerTraceOptions.Defaults);
  LModule := LCapture.Modules.Entry(0).InContext(LContext, TNyxPascalTranslationOptions.Defaults);
  LPrepared := PrepareNyxFPCModule(ACompiler, AOutput + '/prepared', LCapture.Files, LModule,
    LProbe, TNyxFPCModulePreparationOptions.Defaults);
  WriteLn('controlled preparation / probes=', LPrepared.Probes, ' conditions=', LPrepared.Requirements.Count);
  Check(LPrepared.Source.Resolved and (LPrepared.Requirements.Count = 0) and
    (LPrepared.Probes > 0) and not LPrepared.ModuleSource.Context.DefinesComplete,
    'Actual compiler-symbol preparation resolves the whole source while keeping future facts open');
  Check((LPrepared.ModuleSource.Context.Symbol(NyxPascalDefine('NYX_PREPARATION_UNDEFINED_ALPHA')).IsDefined = ppkFalse) and
    (LPrepared.ModuleSource.Context.Symbol(NyxPascalDefine('NYX_PREPARATION_UNDEFINED_BETA')).IsDefined = ppkFalse) and
    (LModule.Context.Symbol(NyxPascalDefine('NYX_PREPARATION_UNDEFINED_ALPHA')).IsDefined = ppkUnknown),
    'Verified absences are detached from the original descriptor and never guessed from helper build symbols');
  LProvider := NewNyxIndexedPascalCompletionProvider(LContext, LPrepared.Source.Files,
    LCapture.Modules.WithModule(LPrepared.ModuleSource),
    TNyxOwnedPascalCompletionOptions.Defaults.WithBinding(TNyxPascalBindingOptions.Defaults));
  LPrefix := 'unit Consumer; interface uses preparation.fixture; implementation ' +
    'procedure Run; var Item:TPreparedValue; begin Item.Val';
  LSources := CaptureNyxPascalSources(NewNyxPascalProject(NyxPascalUnit('Consumer'),
    'unit Consumer; interface implementation end.')).WithBuffer(TNyxPascalSourceRef.Main, LPrefix + '; end; end.');
  LReply := LProvider.Complete(NewNyxPascalCompletionRequest(LSources)
    .At(TNyxPascalSourceRef.Main, NyxTextScalarCount(LPrefix)));
  Check((LReply.Status = pcsReady) and (LReply.Count = 1) and (LReply.Item(0).Name = TNyxText('Value')),
    'The existing indexed loader consumes a real prepared descriptor and original member source');
  LOther := PrepareNyxFPCModule(ACompiler, AOutput + '/no-new-probe', LCapture.Files,
    LPrepared.ModuleSource, LPrepared.Requirements.CompilerProbe(LProbe),
    TNyxFPCModulePreparationOptions.Defaults);
  Check((LOther.Probes = 0) and LOther.Source.Resolved,
    'Already prepared facts require no additional compiler job');
  LOther := nil;
  LFailed := False;
  try
    LOther := PrepareNyxFPCModule(ACompiler, AOutput + '/token-limit', LCapture.Files, LModule,
      LProbe, TNyxFPCModulePreparationOptions.Defaults.UpToTokens(1));
  except
    on ENyxPascalTranslationLimit do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and (LOther = nil), 'Aggregate translation exhaustion publishes no partial prepared module');
  LCancel := NewNyxCancellationScope;
  LCancel.Cancel;
  LFailed := False;
  try
    LOther := PrepareNyxFPCModule(ACompiler, AOutput + '/cancelled', LCapture.Files, LModule,
      LProbe, TNyxFPCModulePreparationOptions.Defaults, LCancel);
  except
    on ENyxPascalAnalysisCancelled do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and (LOther = nil) and not DirectoryExistsUTF8(AOutput + '/cancelled'),
    'Cancellation before admission creates no output or partial result');
end;

{ Whole real RTL roots retain their original bytes and explicit per-unit modes.
  Remaining scoped/compiler-private grammar is printed, never replaced by stubs. }
procedure ActualUnits(const ACompiler, AFPCSource, AOutput: TNyxText);
var
  LCapture: INyxPascalDirectoryCapture;
  LFiles: INyxPascalFileCatalog;
  LContext: INyxPascalCompilerContext;
  LProbe: INyxPascalCompilerProbe;
  LPrepared: INyxFPCModulePreparation;
  LModule: INyxPascalModuleSource;
  LModules: INyxPascalModuleCatalog;
  LByteFiles: INyxPascalByteFileCatalog;
  LBytes: TNyxBytes;
  LStream: TFileStream;
  LAt: Integer;
  LUnit: Integer;
  LOutputPath: TNyxText;
  LMode: TNyxPascalLanguageMode;
  LName: TNyxText;
  LDiagnostic: TNyxPascalTranslationDiagnostic;
  LProvider: INyxPascalCompletionProvider;
  LSources: INyxPascalSourceSnapshot;
  LReply: INyxPascalCompletionResult;
  LMetadata: INyxOwnedPascalCompletionResult;
  LReceipt: INyxPascalCompletionAnalysis;
  LPrefix: TNyxText;
  LView: INyxPascalTranslatedSyntax;
  LSource: INyxPascalSemanticTranslation;
  LWorld: INyxPascalSemanticWorld;
begin
  LCapture := CaptureNyxPascalSourceDirectory(AFPCSource + '/rtl/win32',
    NewNyxPascalFileCatalog(pfcExact), NewNyxPascalModuleCatalog,
    TNyxPascalDirectoryOptions.Defaults.Under('rtl/win32').WithSourcePolicy(pscpUTF8ViewsWithOpaqueBytes));
  LCapture := CaptureNyxPascalSourceDirectory(AFPCSource + '/rtl/win', LCapture.Files, LCapture.Modules,
    TNyxPascalDirectoryOptions.Defaults.Under('rtl/win').WithSourcePolicy(pscpUTF8ViewsWithOpaqueBytes));
  LCapture := CaptureNyxPascalSourceDirectory(AFPCSource + '/rtl/inc', LCapture.Files, LCapture.Modules,
    TNyxPascalDirectoryOptions.Defaults.Under('rtl/inc').WithSourcePolicy(pscpUTF8ViewsWithOpaqueBytes));
  LCapture := CaptureNyxPascalSourceDirectory(AFPCSource + '/rtl/i386', LCapture.Files, LCapture.Modules,
    TNyxPascalDirectoryOptions.Defaults.Under('rtl/i386').WithSourcePolicy(pscpUTF8ViewsWithOpaqueBytes));
  LCapture := CaptureNyxPascalSourceDirectory(AFPCSource + '/rtl/objpas', LCapture.Files, LCapture.Modules,
    TNyxPascalDirectoryOptions.Defaults.Under('rtl/objpas').OnlyFile('objpas.pp'));
  LFiles := LCapture.Files.WithIncludeDirectory('rtl/i386').WithIncludeDirectory('rtl/win')
    .WithIncludeDirectory('rtl/inc');
  LModules := LCapture.Modules;
  WriteLn('actual compiler units / files=', LFiles.Count, ' modules=', LModules.Count);
  Supports(LFiles, INyxPascalByteFileCatalog, LByteFiles);
  for LAt := 0 to LFiles.Count - 1 do
  begin
    LOutputPath := AOutput + '/original-inputs/' + LFiles.FileAt(LAt).Name;

    if not ForceDirectoriesUTF8(ExtractFileDir(LOutputPath)) then
    begin
      raise Exception.Create('Exact original input output needs an owned directory');
    end;
    LBytes := LByteFiles.SourceBytesAt(LAt).Slice(NyxPascalByteSpan(0, LByteFiles.SourceBytesAt(LAt).Count));
    LStream := TFileStream.Create(String(LOutputPath), fmCreate);
    try

      if Length(LBytes) > 0 then
      begin
        LStream.WriteBuffer(LBytes[0], Length(LBytes));
      end;
    finally
      LStream.Free;
    end;
  end;
  LWorld := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults.WithImplicitScopes(pispCompilerUnits));
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
    LContext := CaptureNyxFPCCompilerContext(ACompiler, AOutput + '/base-' + LName,
      LProbe, TNyxPascalCompilerTraceOptions.Defaults);
    Check((LContext.Symbol(NyxPascalDefine('CPUI386')).IsDefined = ppkTrue) and
      (LContext.Symbol(NyxPascalDefine('WIN32')).IsDefined = ppkTrue),
      'The explicitly selected source roots match the actual configured Win32/i386 compiler');
    LModule := LModules.Entry(LModules.Find(NyxPascalModule(LName))).InContext(LContext,
      TNyxPascalTranslationOptions.Defaults.UpToTokens(2000000)
        .Lexical(TNyxPascalLexicalOptions.Defaults.NestedComments(True)));
    LPrepared := PrepareNyxFPCModule(ACompiler, AOutput + '/prepared-' + LName, LFiles, LModule,
      LProbe, TNyxFPCModulePreparationOptions.Defaults.WithPlan(
        TNyxPascalCompilerPlanOptions.Defaults.UpToTokens(2000000)).UpToTokens(8000000));
    LModules := LModules.WithModule(LPrepared.ModuleSource);
    WriteLn('actual unit / ', LName, ' probes=', LPrepared.Probes,
      ' tokens=', LPrepared.Source.Count, ' visits=', LPrepared.Source.OccurrenceCount,
      ' diagnostics=', LPrepared.Source.DiagnosticCount, ' remaining=', LPrepared.Requirements.Count);

    if LUnit = 0 then
    begin
      Check(not LPrepared.Source.Resolved and (LPrepared.Probes = 1) and
        (LPrepared.Requirements.Count = 0) and
        (LPrepared.Source.DiagnosticCount = 1) and
        (LPrepared.Source.Diagnostic(0).Issue = ptiUnsupportedDirective) and
        (NyxPascalOccurrenceLexical(LPrepared.Source, LPrepared.Source.Diagnostic(0).Occurrence)
          .Slice(LPrepared.Source.Diagnostic(0).Span) = TNyxText('{$maxfpuregisters 0}')),
        'Actual System clears grouped-switch/register errors and retains the next explicit compiler policy');
    end
    else
    begin
      Check((LPrepared.Probes = 1) and (LPrepared.Requirements.Count = 1) and
        (LPrepared.Requirements.Needs(0).SymbolCount = 0) and
        (LPrepared.Requirements.Needs(0).SizeCount = 1) and
        (NyxPascalCapturedSize(LPrepared.ModuleSource.Context,
          NyxPascalSizeReference('System.Char')).State = pssKnown),
        'A pristine Char measurement never resolves actual ObjPas source binding by spelling');
    end;
    for LAt := 0 to LPrepared.Source.DiagnosticCount - 1 do
    begin

      if LAt >= 6 then
      begin
        Break;
      end;
      LDiagnostic := LPrepared.Source.Diagnostic(LAt);
      WriteLn('actual unit gap / ', LName, ' issue=', Ord(LDiagnostic.Issue), ' / ',
        LPrepared.Source.Files.FileAt(LPrepared.Source.Occurrence(LDiagnostic.Occurrence).FileIndex).Name,
        ' / ', LDiagnostic.MessageText, ' / ', NyxPascalOccurrenceLexical(LPrepared.Source,
          LDiagnostic.Occurrence).Slice(LDiagnostic.Span));
    end;
    for LAt := 0 to LPrepared.Requirements.Count - 1 do
    begin

      if LAt >= 6 then
      begin
        Break;
      end;
      WriteLn('actual requirement / ', LPrepared.Requirements.Origin(LAt).FileReference.Name,
        ' symbols=', LPrepared.Requirements.Needs(LAt).SymbolCount,
        ' sizes=', LPrepared.Requirements.Needs(LAt).SizeCount, ' / ', LPrepared.Requirements.Expression(LAt));
    end;
    LSource := NewNyxPascalSemanticTranslation(LFiles, LModule.Root, LPrepared.ModuleSource.Context,
      LWorld, LModule.Translation, TNyxPascalSyntaxOptions.Defaults.UpToDeclarations(100000),
      TNyxPascalConditionalOptions.Defaults.UpToTokens(4000000));
    LView := NewNyxPascalTranslatedSyntax(LSource.Source,
      TNyxPascalSyntaxOptions.Defaults.UpToDeclarations(100000));
    WriteLn('actual semantic unit / ', LName, ' answers=', LSource.Answers,
      ' pending=', LSource.Pending, ' issue=', Ord(LSource.BindingIssue), ' related=', LSource.RelatedName,
      ' declarations=', LView.Syntax.DeclarationCount, ' syntax diagnostics=', LView.Syntax.DiagnosticCount);
    LWorld := LWorld.WithModule(LView);
  end;
  Check((LModules.Entry(LModules.Find(NyxPascalModule('System'))).Context.Mode = ppmFPC) and
    (LModules.Entry(LModules.Find(NyxPascalModule('ObjPas'))).Context.Mode = ppmObjFPC),
    'Prepared real compiler units retain independent captured source modes');
  LProvider := NewNyxIndexedPascalCompletionProvider(LContext, LFiles, LModules,
    TNyxOwnedPascalCompletionOptions.Defaults.UpToTokens(8000000)
      .WithTranslation(TNyxPascalTranslationOptions.Defaults
        .Lexical(TNyxPascalLexicalOptions.Defaults.NestedComments(True)))
      .WithSyntax(TNyxPascalSyntaxOptions.Defaults.UpToDeclarations(100000)));
  LPrefix := 'unit Consumer; {$mode delphi} interface implementation procedure Run; ' +
    'var Value:TObject; begin Value.Fr';
  LSources := CaptureNyxPascalSources(NewNyxPascalProject(NyxPascalUnit('Consumer'),
    'unit Consumer; interface implementation end.')).WithBuffer(TNyxPascalSourceRef.Main,
      LPrefix + '; end; end.');
  LReply := LProvider.Complete(NewNyxPascalCompletionRequest(LSources)
    .At(TNyxPascalSourceRef.Main, NyxTextScalarCount(LPrefix)));
  Supports(LReply, INyxOwnedPascalCompletionResult, LMetadata);
  Supports(LReply, INyxPascalCompletionAnalysis, LReceipt);
  WriteLn('actual prepared indexed frontier / status=', Ord(LReply.Status),
    ' gap=', Ord(LMetadata.Gap), ' issue=', Ord(LMetadata.BindingIssue),
    ' related=', LMetadata.RelatedName, ' modules=', LReceipt.ModuleCount,
    ' loads=', LReceipt.DependencyLoads, ' message=', LReply.MessageText);
  Check((LReply.Status = pcsUnavailable) and (LMetadata.Gap = pcgBinding) and
    (LMetadata.BindingIssue = pbiIncompleteSource) and
    (NyxPascalASCIIKey(LMetadata.RelatedName) = TNyxText('objpas')) and
    (LReceipt.ModuleCount = 3) and (LReceipt.DependencyLoads = 2),
    'Actual prepared roots retain their remaining source-language refusal rather than synthetic exports');
end;

begin
  try
    Check(ParamCount = 4, 'Supply repository, configured FPC, FPC source root and fresh owned output');
    Check(not DirectoryExistsUTF8(ParamStr(4)) and CreateDirUTF8(ParamStr(4)),
      'The native module consumer owns fresh compiler/evidence output');
    Controlled(TNyxText(ParamStr(1)), TNyxText(ParamStr(2)), TNyxText(ParamStr(4)));
    ActualUnits(TNyxText(ParamStr(2)), TNyxText(ParamStr(3)), TNyxText(ParamStr(4)));
    WriteLn('PASS ', GChecks, ' native module preparation checks');
  except
    on LError: Exception do
    begin
      WriteLn('FAIL / ', LError.Message);
      DumpExceptionBackTrace(StdErr);
      ExitCode := 1;
    end;
  end;
end.
