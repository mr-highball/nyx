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
program nyx_pascal_windows_native;
{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, Windows, LazFileUtils, nyx.text, nyx.editing, nyx.pascal.context,
  nyx.pascal.compilertrace, nyx.studio.pascalprofiles, nyx.pascal.files,
  nyx.pascal.files.native, nyx.pascal.modules, nyx.pascal.lexing,
  nyx.pascal.translation, nyx.pascal.syntax, nyx.pascal.syntax.translated, nyx.pascal.types,
  nyx.pascal.binding, nyx.pascal.project, nyx.pascal.names, nyx.pascal.drafts,
  nyx.pascal.sources, nyx.pascal.completion, nyx.pascal.completion.owned;

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

{ Read the original captured directive tokens only to prepare explicit compiler
  symbol queries. This test never marks the SDK's future define set complete or
  changes bytes/exports to obtain a successful translation. }
function Requests(const AFiles: INyxPascalFileCatalog): INyxPascalCompilerProbe;
var
  LFile: Integer;
  LAt: Integer;
  LNameEnd: Integer;
  LQuery: Integer;
  LLexical: INyxPascalLexicalSnapshot;
  LBody: TNyxText;
  LName: TNyxText;
  LNeeds: INyxPascalConditionNeeds;
  LContext: INyxPascalCompilerContext;
begin
  Result := NewNyxFPCCompilerProbe(NyxPascalDefine('WINDOWS_SDK'), ppmObjFPC);
  LContext := NewNyxPascalCompilerContext(ppcUnknown,
    TNyxPascalCompilerVersion.Create(0, 0, 0), ppmUnknown);
  for LFile := 0 to AFiles.Count - 1 do
  begin
    LLexical := NewNyxPascalLexicalSnapshot(AFiles.TextAt(LFile),
      TNyxPascalLexicalOptions.Defaults);
    for LAt := 0 to LLexical.Count - 1 do
    begin

      if LLexical.Token(LAt).Kind <> ptkDirective then
      begin
        Continue;
      end;
      LBody := LLexical.Slice(LLexical.Token(LAt).Span);

      if Copy(LBody, 1, 2) = '{$' then
      begin
        LBody := Copy(LBody, 3, Length(LBody) - 3);
      end
      else
      begin
        LBody := Copy(LBody, 4, Length(LBody) - 5);
      end;
      LBody := TNyxText(Trim(LBody));
      LNameEnd := 1;
      while (LNameEnd <= Length(LBody)) and not (LBody[LNameEnd] in [#9, #10, #13, ' ']) do
      begin
        Inc(LNameEnd);
      end;
      LName := NyxPascalASCIIKey(Copy(LBody, 1, LNameEnd - 1));
      LBody := TNyxText(Trim(Copy(LBody, LNameEnd, Length(LBody))));

      if (LName = 'ifdef') or (LName = 'ifndef') then
      begin
        { FPC accepts a trailing explanatory label; only its first identifier
          is the operand. The planner validates it before the compiler query. }
        LNameEnd := 1;
        while (LNameEnd <= Length(LBody)) and not (LBody[LNameEnd] in [#9, #10, #13, ' ']) do
        begin
          Inc(LNameEnd);
        end;
        LBody := 'defined(' + Copy(LBody, 1, LNameEnd - 1) + ')';
      end
      else if (LName <> 'if') and (LName <> 'elseif') then
      begin
        Continue;
      end;
      LNeeds := PlanNyxPascalCondition(LContext, LBody);
      for LQuery := 0 to LNeeds.SymbolCount - 1 do
      begin
        Result := Result.WithSymbol(LNeeds.Symbol(LQuery));
      end;
    end;
  end;
end;

procedure ActualWindows(const ACompiler, ASource, AOutput: TNyxText);
var
  LCapture: INyxPascalDirectoryCapture;
  LFiles: INyxPascalFileCatalog;
  LProbe: INyxPascalCompilerProbe;
  LContext: INyxPascalCompilerContext;
  LRoot: TNyxPascalPhysicalFile;
  LOptions: TNyxPascalTranslationOptions;
  LSource: INyxPascalTranslation;
  LView: INyxPascalTranslatedSyntax;
  LAt: Integer;
  LDiagnostic: TNyxPascalTranslationDiagnostic;
  LName: TNyxText;
  LTime: Windows.TSystemTime;
  LOrigin: TNyxPascalSyntaxOrigin;
  LOriginAt: Integer;
  LPrefix: TNyxText;
  LSources: INyxPascalSourceSnapshot;
  LProvider: INyxPascalCompletionProvider;
  LReply: INyxPascalCompletionResult;
  LMetadata: INyxOwnedPascalCompletionResult;
  LReceipt: INyxPascalCompletionAnalysis;
  LCallback: INyxPascalTypeSyntax;
  LOperatorSyntax: INyxPascalOperatorSyntax;
  LOperatorWorld: INyxPascalSemanticWorld;
  LOperatorQueries: INyxPascalOperatorQueries;
  LRecordType: INyxPascalSemanticType;
  LOperatorReply: INyxPascalResolution;
  LRecordScope: Integer;
begin
  LTime.wYear := 2026;
  Check(LTime.wYear = 2026, 'The selected native SDK exposes and executes its actual record member');
  LCapture := CaptureNyxPascalSourceDirectory(ASource + '/rtl/win32',
    NewNyxPascalFileCatalog(pfcExact), NewNyxPascalModuleCatalog,
    TNyxPascalDirectoryOptions.Defaults.Under('rtl/win32'));
  LCapture := CaptureNyxPascalSourceDirectory(ASource + '/rtl/win/wininc',
    LCapture.Files, LCapture.Modules,
    TNyxPascalDirectoryOptions.Defaults.Under('rtl/win/wininc'));
  LCapture := CaptureNyxPascalSourceDirectory(ASource + '/rtl/inc',
    LCapture.Files, LCapture.Modules,
    TNyxPascalDirectoryOptions.Defaults.Under('rtl/inc').OnlyFile('typshrdh.inc'));
  LCapture := CaptureNyxPascalSourceDirectory(ASource + '/rtl/inc',
    LCapture.Files, LCapture.Modules,
    TNyxPascalDirectoryOptions.Defaults.Under('rtl/inc').OnlyFile('typshrd.inc'));
  LFiles := LCapture.Files.WithIncludeDirectory('rtl/win/wininc')
    .WithIncludeDirectory('rtl/inc');
  LAt := LCapture.Modules.Find(NyxPascalModule('Windows'));
  Check(LAt >= 0, 'The real namespace is captured from its source header, not guessed from its filename');
  LRoot := LCapture.Modules.Entry(LAt).Root;
  LProbe := Requests(LFiles);
  WriteLn('actual Windows capture / files=', LFiles.Count, ' modules=', LCapture.Modules.Count,
    ' explicit symbol queries=', LProbe.Count);
  LContext := CaptureNyxFPCCompilerContext(ACompiler, AOutput + '/compiler', LProbe,
    TNyxPascalCompilerTraceOptions.Defaults);
  Check(not LContext.DefinesComplete, 'Selected compiler queries do not seal future names');
  LOptions := TNyxPascalTranslationOptions.Defaults.UpToTokens(2000000)
    .Lexical(TNyxPascalLexicalOptions.Defaults.NestedComments(True));
  LSource := NewNyxPascalTranslation(LFiles, LRoot, LContext, LOptions);
  Check(LSource.Files.TextAt(LSource.Occurrence(0).FileIndex) = LFiles.TextAt(LFiles.Find(LRoot)),
    'The whole original Windows root survives source analysis exactly');
  WriteLn('actual Windows translation / tokens=', LSource.Count,
    ' occurrences=', LSource.OccurrenceCount, ' diagnostics=', LSource.DiagnosticCount);
  for LAt := 0 to LSource.DiagnosticCount - 1 do
  begin
    LDiagnostic := LSource.Diagnostic(LAt);
    LName := LFiles.FileAt(LSource.Occurrence(LDiagnostic.Occurrence).FileIndex).Name;
    WriteLn('Windows issue / ', LName, ':', LDiagnostic.Span.Start, ' / ', Ord(LDiagnostic.Issue),
      ' / ', NyxPascalOccurrenceLexical(LSource, LDiagnostic.Occurrence).Slice(LDiagnostic.Span));
    Check(LDiagnostic.Issue <> ptiMissingInclude, 'Every visited Windows include is explicitly captured');
  end;
  LView := NewNyxPascalTranslatedSyntax(LSource, TNyxPascalSyntaxOptions.Defaults);
  WriteLn('actual Windows syntax / declarations=', LView.Syntax.DeclarationCount,
    ' diagnostics=', LView.Syntax.DiagnosticCount, ' complete translation=', LView.CompleteTranslation);
  Check(LSource.Resolved and LView.CompleteTranslation and
    (LView.Syntax.DeclarationCount > 7000),
    'Whole real Windows translation reaches declared syntax without replacement SDK exports');
  LAt := LView.Syntax.Named(0, 'BFFCALLBACK');
  LCallback := LView.TypeOfDeclaration(LAt, TNyxPascalTypeOptions.Defaults);
  Check(LCallback.Complete and (LCallback.Node(LCallback.Root).Kind = ptxFunction) and
    (LCallback.Node(LCallback.Root).CallingConvention = pccStdcall),
    'The real Windows callback declaration retains its complete authored signature and stdcall directive');
  LOrigin := LView.Origin(LView.OriginAt(LCallback.Node(LCallback.Root).Span.Finish - 1));
  Check((LFiles.FileAt(LOrigin.FileIndex).Name = TNyxText('rtl/win/wininc/base.inc')) and
    (LSource.Physical(LOrigin.FileIndex).Slice(LOrigin.PhysicalSpan) = TNyxText('stdcall')),
    'The actual SDK convention has its original physical include/token origin');
  Check(LView.Syntax.Named(0, 'stdcall') < 0,
    'Calling directives never become invented exported SDK type declarations');
  Supports(LView.Syntax, INyxPascalOperatorSyntax, LOperatorSyntax);
  LAt := LView.Syntax.Named(0, 'TSize');
  LRecordScope := LView.Syntax.Declaration(LAt).NestedScope;
  Check((LOperatorSyntax.NamedOperator(LRecordScope, pokEqual) >= 0) and
    (LView.Syntax.Named(LRecordScope, '=') < 0),
    'Actual geometry record operators have typed identities separate from ordinary members');
  { Inspect this exact captured record. A handle grants no whole-unit export or
    expression meaning; no synthetic implicit units are supplied to this world. }
  LOperatorWorld := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults
    .WithImplicitScopes(pispCompilerUnits)).WithModule(LView);
  LRecordType := LOperatorWorld.TypeOfSymbol(LOperatorWorld.Symbol(0, LAt)).BoundType;
  Supports(LOperatorWorld, INyxPascalOperatorQueries, LOperatorQueries);
  LOperatorReply := LOperatorQueries.OperatorMembers(LRecordType,
    LOperatorWorld.At(0, 0, LView.Syntax.Lexical.Scalars), pokEqual);
  Check((LOperatorReply.Status = pbsBound) and (LOperatorReply.Count = 1) and
    (LOperatorReply.SymbolAt(0).Declaration.OperatorKind = pokEqual),
    'The exact real TSize record exposes its bounded equality declaration without claiming full SDK binding');
  LOrigin := LOperatorReply.SymbolAt(0).Origin;
  Check((LFiles.FileAt(LOrigin.FileIndex).Name = TNyxText('rtl/inc/typshrdh.inc')) and
    (LSource.Physical(LOrigin.FileIndex).Slice(LOrigin.PhysicalSpan) = TNyxText('=')),
    'The actual operator declaration retains its original physical include token');
  for LAt := 0 to LView.Syntax.DiagnosticCount - 1 do
  begin

    if LAt >= 8 then
    begin
      Break;
    end;
    LOriginAt := LView.OriginAt(LView.Syntax.Diagnostic(LAt).Span.Start);

    if LOriginAt >= 0 then
    begin
      LOrigin := LView.Origin(LOriginAt);
      WriteLn('Windows grammar / ', LFiles.FileAt(LOrigin.FileIndex).Name, ':',
        LOrigin.PhysicalSpan.Start, ' / ', LView.Syntax.Diagnostic(LAt).MessageText,
        ' / ', LView.Syntax.Lexical.Slice(LView.Syntax.Diagnostic(LAt).Span));
    end;
  end;
  LProvider := NewNyxIndexedPascalCompletionProvider(LContext, LFiles,
    LCapture.Modules.WithModule(LCapture.Modules.Entry(
      LCapture.Modules.Find(NyxPascalModule('Windows'))).InContext(LContext, LOptions)),
    TNyxOwnedPascalCompletionOptions.Defaults.WithTranslation(LOptions).UpToTokens(4000000));
  LPrefix := 'unit Consumer; {$mode delphi} interface uses Windows; implementation ' +
    'procedure Run; var Clock:TSystemTime; begin Clock.wY';
  LSources := CaptureNyxPascalSources(NewNyxPascalProject(NyxPascalUnit('Consumer'),
    'unit Consumer; interface implementation end.'))
    .WithBuffer(TNyxPascalSourceRef.Main, LPrefix + '; end; end.');
  LReply := LProvider.Complete(NewNyxPascalCompletionRequest(LSources)
    .At(TNyxPascalSourceRef.Main, NyxTextScalarCount(LPrefix)));
  Check(Supports(LReply, INyxOwnedPascalCompletionResult, LMetadata) and
    Supports(LReply, INyxPascalCompletionAnalysis, LReceipt),
    'Actual SDK completion retains the ordinary typed refusal and loaded-source receipt');
  WriteLn('actual Windows completion / status=', Ord(LReply.Status),
    ' gap=', Ord(LMetadata.Gap), ' issue=', Ord(LMetadata.BindingIssue),
    ' related=', LMetadata.RelatedName, ' modules=', LReceipt.ModuleCount,
    ' loads=', LReceipt.DependencyLoads);
  Check((LReply.Status = pcsUnavailable) and (LMetadata.Gap = pcgBinding) and
    (LMetadata.BindingIssue = pbiUnsupportedScope) and (LReceipt.ModuleCount = 2) and
    (LReceipt.DependencyLoads = 1),
    'Ordinary loading reaches the real Windows grammar boundary rather than inventing exports or reporting an uncaptured unit');
end;

begin
  try
    Check(ParamCount = 3, 'Supply configured FPC, FPC source root and a fresh owned output directory');
    Check(not DirectoryExistsUTF8(ParamStr(3)) and CreateDirUTF8(ParamStr(3)),
      'The actual SDK consumer uses fresh owned compiler output');
    ActualWindows(TNyxText(ParamStr(1)), TNyxText(ParamStr(2)), TNyxText(ParamStr(3)));
    WriteLn('PASS ', GChecks, ' actual Windows SDK checks');
  except
    on LError: Exception do
    begin
      WriteLn('FAIL / ', LError.Message);
      DumpExceptionBackTrace(StdErr);
      ExitCode := 1;
    end;
  end;
end.
