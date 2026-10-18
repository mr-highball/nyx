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
  LBootstrap: INyxPascalCompilerContext;
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
  LInspection: INyxPascalSemanticWorld;
  LAnswer: INyxPascalResolution;
  LType: INyxPascalSemanticType;
  LIdentity: INyxPascalIntrinsicIdentity;
  LHints: INyxPascalHintSyntax;
  LHintOrigin: Integer;
  LNoErrSeen: Boolean;
  LIntrinsics: INyxPascalIntrinsicSyntax;
  LIntrinsic: TNyxPascalIntrinsicDeclaration;
  LWordSwapSeen: Boolean;
  LQuestionPrefix: INyxPascalTranslation;
  LQuestionView: INyxPascalTranslatedSyntax;
  LQuestionScope: Integer;
  LQuestionOwner: Integer;
  LQuestionOrigin: Integer;
  LRegisterDirectives: INyxPascalRegisterDirectives;
  LSyntaxLimited: Boolean;
  LPublicVariable: Boolean;
  LInterfaceRoutine: Boolean;
  LDeclaration: TNyxPascalSyntaxDeclaration;
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
    .WithIncludeDirectory('rtl/inc').WithIncludeDirectory('rtl/win32');
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
  LProbe := NewNyxFPCSystemBootstrapProbe(NyxPascalDefine('ACTUAL_SYSTEM_ROOT'));
  LProbe := NyxFPCProbeSizeOf(LProbe, NyxPascalSizeReference('AnsiChar'));
  LProbe := NyxFPCProbeSizeOf(LProbe, NyxPascalSizeReference('WideChar'));
  LProbe := NyxFPCProbeSizeOf(LProbe, NyxPascalSizeReference('Char'));
  LProbe := NyxFPCProbeSizeOf(LProbe, NyxPascalSizeReference('Comp'));
  LProbe := NyxFPCProbeSizeOf(LProbe, NyxPascalSizeReference('Byte'));
  LBootstrap := CaptureNyxFPCCompilerContext(ACompiler, AOutput + '/compiler-root',
    LProbe, TNyxPascalCompilerTraceOptions.Defaults);
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
    LContext := NyxPascalWithBootstrapContext(LContext, LBootstrap);
    Check((LContext.Symbol(NyxPascalDefine('CPUI386')).IsDefined = ppkTrue) and
      (LContext.Symbol(NyxPascalDefine('WIN32')).IsDefined = ppkTrue),
      'The explicitly selected source roots match the actual configured Win32/i386 compiler');
    LModule := LModules.Entry(LModules.Find(NyxPascalModule(LName))).InContext(LContext,
      TNyxPascalTranslationOptions.Defaults.UpToTokens(2000000)
        .Lexical(TNyxPascalLexicalOptions.Defaults.NestedComments(True)));
    { More of the original RTL is now readable, exposing additional compiler
      facts. Select eight bounded probe jobs explicitly for this qualification;
      the product's four-probe default and parser depth remain unchanged. }
    LPrepared := PrepareNyxFPCModule(ACompiler, AOutput + '/prepared-' + LName, LFiles, LModule,
      LProbe, TNyxFPCModulePreparationOptions.Defaults.WithPlan(
        TNyxPascalCompilerPlanOptions.Defaults.UpToTokens(2000000)).UpToTokens(8000000).UpToProbes(8));
    LModules := LModules.WithModule(LPrepared.ModuleSource);
    Check(NyxPascalBootstrapContext(LPrepared.ModuleSource.Context).SameFacts(LBootstrap),
      'Every pristine symbol capture retains the independent measured compiler-root packet');
    WriteLn('actual unit / ', LName, ' probes=', LPrepared.Probes,
      ' tokens=', LPrepared.Source.Count, ' visits=', LPrepared.Source.OccurrenceCount,
      ' diagnostics=', LPrepared.Source.DiagnosticCount, ' remaining=', LPrepared.Requirements.Count);

    if LUnit = 0 then
    begin
      Supports(LPrepared.Source, INyxPascalRegisterDirectives, LRegisterDirectives);
      Check(not LPrepared.Source.Resolved and (LPrepared.Probes > 0) and
        (LRegisterDirectives <> nil) and (LRegisterDirectives.RegisterDirectiveCount > 0) and
        (LRegisterDirectives.RegisterDirectiveAt(0).Policy = pfr0) and
        (LRegisterDirectives.RegisterDirectiveAt(0).Activity = ppkTrue) and
        (LRegisterDirectives.RegisterDirectiveAt(0).Origin.FileReference.Name =
          TNyxText('rtl/inc/systemh.inc')),
        'Actual System retains its exact routine-register intent and advances to genuine remaining source gaps');
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
    LSource := nil;
    LView := nil;
    LSyntaxLimited := False;
    try
      LSource := NewNyxPascalSemanticTranslation(LFiles, LModule.Root, LPrepared.ModuleSource.Context,
        LWorld, LModule.Translation, TNyxPascalSyntaxOptions.Defaults.UpToDeclarations(100000),
        TNyxPascalConditionalOptions.Defaults.UpToTokens(4000000));
      LView := NewNyxPascalTranslatedSyntax(LSource.Source,
        TNyxPascalSyntaxOptions.Defaults.UpToDeclarations(100000));
    except
      on LError: ENyxPascalSyntaxLimit do
      begin
        { The unchanged parser ceiling exposes the next real source-language
          prerequisite. No synthetic root, shortened input or increased ceiling
          is used to turn this failed whole-source construction into success. }
        LSyntaxLimited := True;
        WriteLn('actual semantic unit gap / ', LName, ' / ', LError.Message);
      end;
    end;

    if LUnit = 0 then
    begin
      Check(not LSyntaxLimited and (LView <> nil),
        'The original System declaration view no longer exhausts syntax depth after native linkage');
      LPublicVariable := False;
      LInterfaceRoutine := False;
      LNoErrSeen := False;
      LWordSwapSeen := False;
      Supports(LView.Syntax, INyxPascalHintSyntax, LHints);
      Supports(LView.Syntax, INyxPascalIntrinsicSyntax, LIntrinsics);
      for LAt := 0 to LView.Syntax.DeclarationCount - 1 do
      begin
        LDeclaration := LView.Syntax.Declaration(LAt);

        if not LWordSwapSeen and (NyxPascalASCIIKey(LDeclaration.Name) = TNyxText('swap')) and
          (plfConstantIntrinsic in LDeclaration.Linkage) then
        begin
          LWordSwapSeen := True;
          LIntrinsic := LIntrinsics.IntrinsicAt(LDeclaration.FirstIntrinsic);
          Check((LIntrinsic.Kind = pikConstant) and LIntrinsic.Complete and
            (LView.Syntax.Lexical.Slice(LIntrinsic.OperandSpan) = TNyxText('fpc_in_const_swap_word')),
            'The original System Swap keeps its typed constant role and compiler-specific symbolic operand');
          LHintOrigin := LView.OriginAt(LIntrinsic.KeySpan.Start);
          Check((LHintOrigin >= 0) and
            (LView.Source.Files.FileAt(LView.Origin(LHintOrigin).FileIndex).Name = TNyxText('rtl/inc/systemh.inc')),
            'Actual constant intrinsic metadata retains its exact original source owner');
        end;

        if NyxPascalASCIIKey(LDeclaration.Name) = TNyxText('noerrmsg') then
        begin
          LNoErrSeen := True;
          Check((LView.Syntax.Lexical.Slice(LDeclaration.TypeSpan) = TNyxText('Boolean')) and
            (LView.Syntax.Lexical.Slice(LDeclaration.ExpressionSpan) = TNyxText('False')),
            'Actual NoErrMsg retains separate Boolean type and False initializer');
          LHintOrigin := LView.OriginAt(LHints.HintAt(LDeclaration.FirstHint).Span.Start);
          Check((LHints.HintAt(LDeclaration.FirstHint).Kind = phkPlatform) and (LHintOrigin >= 0) and
            (LView.Source.Files.FileAt(LView.Origin(LHintOrigin).FileIndex).Name = TNyxText('rtl/inc/systemh.inc')),
            'Actual NoErrMsg platform metadata retains the original included source owner');
        end;

        if NyxPascalASCIIKey(LDeclaration.Name) = TNyxText('char') then
        begin
          WriteLn('actual Char declaration / scope=', LDeclaration.Scope,
            ' incomplete=', psdfIncomplete in LDeclaration.Flags,
            ' conditional=', psdfConditional in LDeclaration.Flags,
            ' unsupported=', psdfUnsupported in LDeclaration.Flags,
            ' type=', LView.Syntax.Lexical.Slice(LDeclaration.TypeSpan));
          LInspection := LWorld.WithModule(LView);
          LAnswer := LInspection.Lookup(LInspection.At(LInspection.FindModule('System'),
            LDeclaration.Scope, LDeclaration.TypeSpan.Finish), NyxPascalBindingName('Char'));
          Check((LAnswer.Status = pbsBound) and (LAnswer.Count = 1),
            'Original System Char is an authored declaration at its exact source site');
          LAnswer := LInspection.TypeOfSymbol(LAnswer.SymbolAt(0));
          LType := LAnswer.BoundType;
          while (LType <> nil) and (LType.Kind in [pstAlias, pstDistinct]) do
          begin
            LType := LType.Target;
          end;
          Check((LAnswer.Status = pbsBound) and Supports(LType, INyxPascalIntrinsicIdentity, LIdentity) and
            (LIdentity.IntrinsicOrigin = pioSystemBootstrap) and
            (NyxPascalCapturedSize(LIdentity.IntrinsicContext, LIdentity.IntrinsicReference).Bytes = 1),
            'The unchanged actual System alias binds to the separately measured compiler primitive');
        end;

        if NyxPascalASCIIKey(LDeclaration.Name) = TNyxText('exitcode') then
        begin
          LPublicVariable := (LDeclaration.Scope = 0) and
            (LDeclaration.Visibility = psavInterface) and (plfPublic in LDeclaration.Linkage);
        end;

        if NyxPascalASCIIKey(LDeclaration.Name) = TNyxText('prefetch') then
        begin
          LInterfaceRoutine := (LDeclaration.Scope = 0) and
            (LDeclaration.Visibility = psavInterface) and (plfIntrinsic in LDeclaration.Linkage);
        end;
      end;
      Check(LPublicVariable, 'Actual public ExitCode retains unit-interface membership and declared linker intent');
      Check(LNoErrSeen, 'The unchanged whole System view contains its hinted initialized NoErrMsg declaration');
      Check(LWordSwapSeen, 'The unchanged whole System view contains the attributed Swap routine');
      Check(LInterfaceRoutine, 'Actual following intrinsic routines remain interface siblings rather than nested bodies');
      LQuestionPrefix := NewNyxPascalTranslationPrefix(LSource.Source, LSource.Origin.PrefixCount,
        LModule.Translation);
      LQuestionView := NewNyxPascalTranslatedSyntax(LQuestionPrefix, TNyxPascalSyntaxOptions.Defaults);
      LQuestionScope := LQuestionView.Syntax.ScopeAt(LQuestionView.Syntax.Lexical.Scalars);
      LQuestionOwner := -1;
      while LQuestionScope >= 0 do
      begin
        LAt := LQuestionView.Syntax.Scope(LQuestionScope).OwnerDeclaration;

        if (LAt >= 0) and
          (NyxPascalASCIIKey(LQuestionView.Syntax.Declaration(LAt).Name) = TNyxText('tobject')) then
        begin
          LQuestionOwner := LAt;
          Break;
        end;
        LQuestionScope := LQuestionView.Syntax.Scope(LQuestionScope).Parent;
      end;
      WriteLn('actual enclosing query / answers=', LSource.Answers, ' pending=', LSource.Pending,
        ' issue=', Ord(LSource.BindingIssue), ' related=', LSource.RelatedName);
      for LAt := 0 to LQuestionView.Syntax.DiagnosticCount - 1 do
      begin

        if LQuestionView.Syntax.Diagnostic(LAt).Issue = psiUnsupportedSyntax then
        begin
          LQuestionOrigin := LQuestionView.OriginAt(LQuestionView.Syntax.Diagnostic(LAt).Span.Start);
          WriteLn('actual enclosing diagnostic / issue=', Ord(LQuestionView.Syntax.Diagnostic(LAt).Issue),
            ' / ', LQuestionView.Syntax.Diagnostic(LAt).MessageText, ' / ',
            LQuestionView.Syntax.Lexical.Slice(LQuestionView.Syntax.Diagnostic(LAt).Span));

          if LQuestionOrigin >= 0 then
          begin
            WriteLn('actual enclosing physical / ', LQuestionView.Source.Files.FileAt(
              LQuestionView.Origin(LQuestionOrigin).FileIndex).Name, ' / scalar=',
              LQuestionView.Origin(LQuestionOrigin).PhysicalSpan.Start);
          end;
          Break;
        end;
      end;
      Check(LSource.Pending and (LSource.BindingIssue = pbiUnsupportedScope) and (LSource.Answers = 4) and
        (LQuestionOwner >= 0) and
        (NyxPascalASCIIKey(LQuestionView.Syntax.Declaration(LQuestionOwner).Name) = TNyxText('tobject')) and
        (psdfIncomplete in LQuestionView.Syntax.Declaration(LQuestionOwner).Flags),
        'The open TObject query now retains the earlier real unsupported module syntax prerequisite');
    end;

    if not LSyntaxLimited then
    begin
      WriteLn('actual semantic unit / ', LName, ' answers=', LSource.Answers,
        ' pending=', LSource.Pending, ' issue=', Ord(LSource.BindingIssue), ' related=', LSource.RelatedName,
        ' declarations=', LView.Syntax.DeclarationCount, ' syntax diagnostics=', LView.Syntax.DiagnosticCount);
      LWorld := LWorld.WithModule(LView);
    end;
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
  Check((LReply.Status = pcsUnavailable) and (LMetadata.Gap = pcgBinding) and (LReply.Count = 0),
    'Ordinary indexed loading reaches an explicit remaining source/binding refusal without invented suggestions');
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
