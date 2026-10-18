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
program nyx_pascal_dependencies_tests;
{$mode delphi}{$H+}{$codepage utf8}
uses SysUtils, nyx.text, nyx.editing, nyx.scheduler, nyx.pascal.context, nyx.pascal.files,
  nyx.pascal.modules, nyx.pascal.translation, nyx.pascal.binding,
  nyx.pascal.names, nyx.pascal.project, nyx.pascal.drafts, nyx.pascal.sources,
  nyx.pascal.completion, nyx.pascal.completion.owned;
const
  CBase: TNyxText = 'unit Consumer; interface implementation end.';
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

function Facts: INyxPascalCompilerContext;
begin
  Result := NewNyxPascalCompilerContext(ppcFPC,
    TNyxPascalCompilerVersion.Create(3, 3, 1), ppmDelphi)
    .WithSymbol(NyxPascalDefine('FPC'), ppdDefined)
    .WithSymbol(NyxPascalDefine('FPC_FULLVERSION'), ppdInteger, 30301)
    .CompleteDefines(True);
end;

function Root(const AName, AFile: TNyxText): INyxPascalModuleSource;
begin
  Result := NewNyxPascalModuleSource(NyxPascalModule(AName), NyxPascalPhysicalFile(AFile));
end;

function Query(const AUses, AType, AExpression: TNyxText): INyxPascalCompletionRequest;
var
  LSources: INyxPascalSourceSnapshot;
  LPrefix: TNyxText;
begin
  LPrefix := 'unit Consumer; {$mode delphi} interface uses ' + AUses + '; implementation ' +
    'procedure Run; var Current:' + AType + '; begin ';
  LSources := CaptureNyxPascalSources(NewNyxPascalProject(NyxPascalUnit('Consumer'), CBase))
    .WithBuffer(TNyxPascalSourceRef.Main, LPrefix + AExpression + '; end; end.');
  Result := NewNyxPascalCompletionRequest(LSources).At(TNyxPascalSourceRef.Main,
    NyxTextScalarCount(LPrefix + AExpression));
end;

function Analysis(const AReply: INyxPascalCompletionResult): INyxPascalCompletionAnalysis;
begin
  Check(Supports(AReply, INyxPascalCompletionAnalysis, Result),
    'Indexed analysis retains a detached loaded-module receipt');
end;

function Gap(const AReply: INyxPascalCompletionResult): INyxOwnedPascalCompletionResult;
begin
  Check(Supports(AReply, INyxOwnedPascalCompletionResult, Result),
    'Typed refusal metadata remains available on indexed replies');
end;

procedure LazyWorld;
var
  LFiles: INyxPascalFileCatalog;
  LModules: INyxPascalModuleCatalog;
  LOld: INyxPascalModuleCatalog;
  LProvider: INyxPascalCompletionProvider;
  LReply: INyxPascalCompletionResult;
  LReceipt: INyxPascalCompletionAnalysis;
  LQuery: INyxPascalCompletionRequest;
  LMetadata: INyxOwnedPascalCompletionResult;
begin
  LFiles := NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('sdk/system.pp'),
      'unit System; {$mode delphi} interface type TObject=class end; IInterface=interface end; implementation end.')
    .WithFile(NyxPascalPhysicalFile('sdk/templates.pp'),
      'unit Templates; {$mode delphi} interface type TBadge=class public ' +
      'function WithText:TBadge; end; TBox<T>=class public Value:T; end; implementation end.')
    .WithFile(NyxPascalPhysicalFile('sdk/unused.pp'),
      'This is deliberately not a module and must not be translated');
  LOld := NewNyxPascalModuleCatalog.WithModule(Root('System', 'sdk/system.pp'));
  LModules := LOld.WithModule(Root('Templates', 'sdk/templates.pp'))
    .WithModule(Root('Unused', 'sdk/unused.pp'));
  Check((LOld.Count = 1) and (LModules.Count = 3) and
    (LOld.Find(NyxPascalModule('Templates')) < 0),
    'Adding indexed modules preserves old metadata snapshots');
  Check(LModules.Find(NyxPascalModule('tEmPlAtEs')) = 1,
    'Module namespaces use explicit Pascal case folding');
  LProvider := NewNyxIndexedPascalCompletionProvider(Facts, LFiles, LModules,
    TNyxOwnedPascalCompletionOptions.Defaults.UpToReceiverTokens(12));
  LQuery := Query('Templates', 'TBox<TBadge>', 'Current.Value.WithTe');
  LReply := LProvider.Complete(LQuery);
  Check((LReply.Status = pcsReady) and (LReply.Count = 1) and
    (LReply.Item(0).Name = TNyxText('WithText')), 'An indexed generic receiver loads only required module roots');
  LReceipt := Analysis(LReply);
  Check((LReceipt.ModuleCount = 3) and (LReceipt.DependencyLoads = 2) and
    (LReceipt.ModuleName(0).Name = TNyxText('Consumer')) and
    (LReceipt.ModuleName(1).Name = TNyxText('Templates')) and
    (LReceipt.ModuleName(2).Name = TNyxText('System')) and
    (LReceipt.TranslatedTokens > 0),
    'Loaded roots retain request order and unrelated malformed SDK roots remain untouched');
  LMetadata := Gap(LReply);
  LProvider := nil;
  Check((LReceipt.ModuleRoot(1).Name = TNyxText('sdk/templates.pp')) and
    (LMetadata.DefinitionSource(0).Scalars > 0),
    'Analysis/definition receipts survive provider retirement without a reverse cycle');
  LProvider := NewNyxIndexedPascalCompletionProvider(Facts, LFiles, LModules,
    TNyxOwnedPascalCompletionOptions.Defaults.UpToDependencyLoads(1));
  LReply := LProvider.Complete(LQuery);
  Check((LReply.Status = pcsUnavailable) and (LReply.Count = 0) and
    (Gap(LReply).Gap = pcgBudget) and (Analysis(LReply).DependencyLoads = 1),
    'Dependency limits keep aggregate state and publish no partial insertion');
  LProvider := NewNyxIndexedPascalCompletionProvider(Facts, LFiles, LModules,
    TNyxOwnedPascalCompletionOptions.Defaults.UpToTokens(LReceipt.TranslatedTokens - 1));
  LReply := LProvider.Complete(LQuery);
  Check((LReply.Status = pcsUnavailable) and (Gap(LReply).Gap = pcgBudget),
    'Aggregate token accounting survives semantic restarts instead of resetting per dependency');
end;

procedure ImportAuthority;
var
  LFiles: INyxPascalFileCatalog;
  LModules: INyxPascalModuleCatalog;
  LProvider: INyxPascalCompletionProvider;
  LReply: INyxPascalCompletionResult;
  LReceipt: INyxPascalCompletionAnalysis;
  LOpen: INyxPascalCompilerContext;
begin
  LFiles := NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('early.pp'),
      'unit Earlier; {$mode delphi} interface type TCollision=record EarlyField:Integer; end; implementation end.')
    .WithFile(NyxPascalPhysicalFile('late.pp'),
      'unit Later; {$mode delphi} interface type TCollision=record LateField:Integer; end; implementation end.');
  LModules := NewNyxPascalModuleCatalog.WithModule(Root('Earlier', 'early.pp'))
    .WithModule(Root('Later', 'late.pp'));
  LProvider := NewNyxIndexedPascalCompletionProvider(Facts, LFiles, LModules,
    TNyxOwnedPascalCompletionOptions.Defaults);
  LReply := LProvider.Complete(Query('Earlier, Later', 'TCollision', 'Current.Late'));
  Check((LReply.Status = pcsReady) and (LReply.Count = 1) and
    (LReply.Item(0).Name = TNyxText('LateField')), 'The last direct import wins after focused loading');
  LReceipt := Analysis(LReply);
  Check((LReceipt.DependencyLoads = 1) and (LReceipt.ModuleName(1).Name = TNyxText('Later')),
    'An earlier same-named export is never loaded speculatively');
  LProvider := NewNyxIndexedPascalCompletionProvider(Facts, LFiles,
    NewNyxPascalModuleCatalog.WithModule(Root('Earlier', 'early.pp')),
    TNyxOwnedPascalCompletionOptions.Defaults);
  LReply := LProvider.Complete(Query('Earlier, Later', 'TCollision', 'Current.Early'));
  Check((LReply.Status = pcsUnavailable) and (Gap(LReply).BindingIssue = pbiMissingModule) and
    (Gap(LReply).RelatedName = TNyxText('Later')) and (Analysis(LReply).DependencyLoads = 0),
    'An unindexed later import cannot be bypassed by an earlier captured export');
  LReply := LProvider.Complete(Query('Earlier, Later', 'Earlier.TCollision', 'Current.Early'));
  Check((LReply.Status = pcsReady) and (LReply.Count = 1),
    'Explicit qualification still resolves the deliberately selected earlier unit');
  LFiles := LFiles.WithFile(NyxPascalPhysicalFile('late.pp'),
    'unit Later; {$mode delphi} interface var &implementation:Integer; ' +
    '{$ifdef UNKNOWN_BRANCH}type TCollision=record LaterField:Integer; end; {$endif} implementation end.');
  LOpen := Facts.CompleteDefines(False);
  LModules := LModules.WithModule(Root('Later', 'late.pp')
    .InContext(LOpen, TNyxPascalTranslationOptions.Defaults));
  LProvider := NewNyxIndexedPascalCompletionProvider(Facts, LFiles, LModules,
    TNyxOwnedPascalCompletionOptions.Defaults);
  LReply := LProvider.Complete(Query('Earlier, Later', 'TCollision', 'Current.Early'));
  Check((LReply.Status = pcsUnavailable) and (LReply.Count = 0) and
    (Gap(LReply).BindingIssue = pbiIncompleteSource) and (Gap(LReply).RelatedName = TNyxText('Later')),
    'An unfinished/unknown interface and escaped implementation name cannot prove export absence');
  LFiles := LFiles.WithFile(NyxPascalPhysicalFile('late.pp'),
    'unit Later; {$mode delphi} interface type TOpen=class public Value:Integer; implementation end.');
  LModules := LModules.WithModule(Root('Later', 'late.pp'));
  LProvider := NewNyxIndexedPascalCompletionProvider(Facts, LFiles, LModules,
    TNyxOwnedPascalCompletionOptions.Defaults);
  LReply := LProvider.Complete(Query('Earlier, Later', 'TCollision', 'Current.Early'));
  Check((LReply.Status = pcsUnavailable) and
    (Gap(LReply).BindingIssue = pbiIncompleteSource) and (Gap(LReply).RelatedName = TNyxText('Later')),
    'An implementation token inside an unclosed member scope cannot certify complete exports');
end;

procedure UnitContexts;
var
  LFiles: INyxPascalFileCatalog;
  LModules: INyxPascalModuleCatalog;
  LProvider: INyxPascalCompletionProvider;
  LReply: INyxPascalCompletionResult;
  LContext: INyxPascalCompilerContext;
  LRejected: Boolean;
begin
  LFiles := NewNyxPascalFileCatalog(pfcExact).WithFile(NyxPascalPhysicalFile('unit.pp'),
    'unit Configured; {$mode delphi} interface {$ifdef SDK_FEATURE}' +
    'type TData=record EnabledField:Integer; end; {$else}' +
    'type TData=record OtherField:Integer; end; {$endif} implementation end.');
  LContext := Facts.WithSymbol(NyxPascalDefine('SDK_FEATURE'), ppdDefined);
  LModules := NewNyxPascalModuleCatalog.WithModule(Root('Configured', 'unit.pp')
    .InContext(LContext, TNyxPascalTranslationOptions.Defaults));
  LProvider := NewNyxIndexedPascalCompletionProvider(Facts, LFiles, LModules,
    TNyxOwnedPascalCompletionOptions.Defaults);
  LReply := LProvider.Complete(Query('Configured', 'TData', 'Current.Enabled'));
  Check((LReply.Status = pcsReady) and (LReply.Count = 1) and
    (LReply.Item(0).Name = TNyxText('EnabledField')),
    'Explicit per-unit compiler facts are independent of application/profile facts');
  LContext := NewNyxPascalCompilerContext(ppcFPC,
    TNyxPascalCompilerVersion.Create(3, 2, 2), ppmDelphi);
  LModules := LModules.WithModule(Root('Configured', 'unit.pp')
    .InContext(LContext, TNyxPascalTranslationOptions.Defaults));
  LProvider := NewNyxIndexedPascalCompletionProvider(Facts, LFiles, LModules,
    TNyxOwnedPascalCompletionOptions.Defaults);
  LReply := LProvider.Complete(Query('Configured', 'TData', 'Current.Enabled'));
  Check((LReply.Status = pcsUnavailable) and
    (Gap(LReply).BindingIssue = pbiCompilerContext) and (Analysis(LReply).DependencyLoads = 0),
    'A module from another compiler version cannot silently enter the same source world');
  LRejected := False;
  try
    CaptureNyxPascalModules(NewNyxPascalModuleCatalog
      .WithModule(Root('One', 'unit.pp')).WithModule(Root('Two', 'unit.pp')), LFiles);
  except
    on ENyxPascalContext do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Physical aliases need an explicit compiler contract rather than duplicated root identities');
end;

begin
  try
    LazyWorld;
    ImportAuthority;
    UnitContexts;
    WriteLn('PASS ', GChecks, ' indexed dependency checks');
  except
    on LError: Exception do
    begin
      WriteLn('FAIL / ', LError.Message);
      {$ifndef PAS2JS}DumpExceptionBackTrace(StdErr);{$endif}
      ExitCode := 1;
    end;
  end;
end.
