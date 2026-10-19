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
program nyx_pascal_coherence_tests;
{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.editing, nyx.scheduler, nyx.pascal.context, nyx.pascal.lexing,
  nyx.pascal.files, nyx.pascal.translation, nyx.pascal.syntax,
  nyx.pascal.syntax.translated, nyx.pascal.binding, nyx.pascal.work,
  nyx.pascal.modules, nyx.pascal.names, nyx.pascal.project, nyx.pascal.sources,
  nyx.pascal.drafts, nyx.pascal.completion, nyx.pascal.completion.owned;

var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AReason: String);
begin
  Inc(GChecks);

  if not AValue then raise Exception.Create(AReason);
end;

{ Controlled evidence values exercise consistency, not the executing CPU's ABI.
  The maintained native consumer separately reuses its actual captured packet. }
function Root(ABytes: Integer; AState: TNyxPascalSizeState = pssKnown;
  AMode: TNyxPascalLanguageMode = ppmFPC; AStrings: TNyxPascalKnowledge = ppkFalse): INyxPascalCompilerContext;
begin
  Result := NewNyxPascalCompilerContext(ppcFPC, TNyxPascalCompilerVersion.Create(3, 3, 1),
    ppmFPC, pcsoSystemBootstrap).WithOption('H', ppkFalse);
  Result := NyxPascalWithCapturedSize(Result, NyxPascalSizeReference('Pointer'),
    NyxPascalSizeFact(AState, AMode, AStrings, ABytes));
end;

function Source(const ARoot: INyxPascalCompilerContext = nil;
  AMode: TNyxPascalLanguageMode = ppmDelphi): INyxPascalCompilerContext;
begin
  Result := NewNyxPascalCompilerContext(ppcFPC, TNyxPascalCompilerVersion.Create(3, 3, 1), AMode);

  if ARoot <> nil then Result := NyxPascalWithBootstrapContext(Result, ARoot);
end;

function View(const AName: TNyxText; const AContext: INyxPascalCompilerContext): INyxPascalTranslatedSyntax;
begin
  Result := NewNyxPascalTranslatedSyntax(NewNyxPascalTranslation(NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile(AName + '.pas'), 'unit ' + AName +
    '; interface type TObject=class end; TMarker=class Value:LongInt; end; implementation end.'),
    NyxPascalPhysicalFile(AName + '.pas'), AContext, TNyxPascalTranslationOptions.Defaults),
    TNyxPascalSyntaxOptions.Defaults);
end;

function World(const AContext: INyxPascalCompilerContext): INyxPascalSemanticWorld;
begin
  Result := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults).WithModule(View('System', AContext));
end;

procedure Conflict(const ALeft, ARight: INyxPascalCompilerContext);
var
  LWorld, LAttempt: INyxPascalSemanticWorld;
  LRejected: Boolean;
begin
  LWorld := World(ALeft);
  LRejected := False;
  try
    LAttempt := LWorld.WithModule(View('Consumer', ARight));
  except
    on E: ENyxPascalModuleContext do LRejected := E.RelatedModule = TNyxText('System');
  end;
  Check(LRejected and (LAttempt = nil) and (LWorld.Count = 1),
    'Conflicting captured measurements refuse atomically with their related module');
  Check(LWorld.ModuleAt(0).Source.InitialContext.SameFacts(ALeft),
    'Rejected admission preserves every exact earlier compiler fact');
end;

procedure Compatible(const ALeft, ARight: INyxPascalCompilerContext);
var
  LWorld: INyxPascalSemanticWorld;
begin
  LWorld := World(ALeft).WithModule(View('Consumer', ARight));
  Check((LWorld.Count = 2) and LWorld.ModuleAt(0).Source.InitialContext.SameFacts(ALeft) and
    LWorld.ModuleAt(1).Source.InitialContext.SameFacts(ARight),
    'Nonconflicting partial/source-qualified evidence retains both original owners without merging');
end;

procedure Evidence;
var
  LKey: TNyxPascalStorageKey;
  LRoot, LOther: INyxPascalCompilerContext;
begin
  Conflict(Source(Root(4)), Source(Root(8)));
  Conflict(Source(Root(4)), Source(Root(0, pssUndeclared)));
  Conflict(Source(Root(4)), Source(Root(0, pssOutOfRange)));
  Conflict(Root(4), Source(Root(8)));
  Conflict(Source(Root(4)), Root(8));
  Conflict(Source, NewNyxPascalCompilerContext(ppcPas2JS,
    TNyxPascalCompilerVersion.Create(3, 3, 1), ppmDelphi));
  Conflict(Source, NewNyxPascalCompilerContext(ppcFPC,
    TNyxPascalCompilerVersion.Create(3, 2, 2), ppmDelphi));
  Compatible(Source(Root(4)), Source(Root(4), ppmObjFPC));
  Compatible(Source(Root(4)), Source(Root(0, pssUnknown)));
  Compatible(Source, Source(Root(8)));
  Compatible(Source(Root(4)), Source);
  Compatible(Source(Root(4)), Source(Root(8, pssKnown, ppmDelphi, ppkTrue)));
  Compatible(Source(Root(4)), NyxPascalWithCapturedSize(Source, NyxPascalSizeReference('Pointer'),
    NyxPascalSizeFact(pssKnown, ppmDelphi, ppkTrue, 8)));
  Compatible(Source(Root(4)).WithSymbol(NyxPascalDefine('LOCAL_FEATURE'), ppdDefined),
    Source(Root(4)).WithSymbol(NyxPascalDefine('LOCAL_FEATURE'), ppdUndefined).WithOption('R', ppkTrue));
  LKey := NyxPascalStorageKey('nyx.storage.1|shared-structural-key-v1');
  Conflict(NyxPascalWithCapturedStorage(Source, LKey, NyxPascalStorageFact(psfsKnown, 4)),
    NyxPascalWithCapturedStorage(Source, LKey, NyxPascalStorageFact(psfsKnown, 8)));
  Conflict(NyxPascalWithCapturedStorage(Source, LKey, NyxPascalStorageFact(psfsKnown, 4)),
    NyxPascalWithCapturedStorage(Source, LKey, NyxPascalStorageFact(psfsOutOfRange)));
  Compatible(NyxPascalWithCapturedStorage(Source, LKey, NyxPascalStorageFact(psfsUnknown)),
    NyxPascalWithCapturedStorage(Source, LKey, NyxPascalStorageFact(psfsKnown, 8)));
  Compatible(NyxPascalWithCapturedStorage(Source, LKey, NyxPascalStorageFact(psfsKnown, 4)),
    NyxPascalWithCapturedStorage(Source, NyxPascalStorageKey('nyx.storage.1|another-context-key'), NyxPascalStorageFact(psfsKnown, 8)));
  LRoot := NyxPascalWithCapturedStorage(Root(4), LKey, NyxPascalStorageFact(psfsKnown, 4));
  LOther := NyxPascalWithCapturedStorage(Root(4), LKey, NyxPascalStorageFact(psfsKnown, 8));
  Conflict(Source(LRoot), Source(LOther));
  Conflict(Source(LRoot), NyxPascalWithCapturedStorage(Source, LKey, NyxPascalStorageFact(psfsKnown, 8)));
  Conflict(NyxPascalWithCapturedStorage(Source, LKey, NyxPascalStorageFact(psfsKnown, 4)), Source(LOther));
end;

procedure ReplacementAndWork;
var
  LWorld, LNext, LAttempt: INyxPascalSemanticWorld;
  LView: INyxPascalTranslatedSyntax;
  LBudget: INyxPascalAnalysisBudget;
  LCancel: INyxCancellationScope;
  LFound: INyxPascalResolution;
  LAt, LFit: Integer;
  LRejected: Boolean;
begin
  LWorld := World(Source(Root(4)));
  LView := View('Consumer', Source(Root(4), ppmObjFPC));
  LFit := -1;
  for LAt := 1 to 3000 do
  begin
    LAttempt := nil;
    LBudget := NewNyxPascalAnalysisBudget(LAt);
    try
      try
        LAttempt := WithNyxPascalSemanticModule(LWorld, LView, LBudget);
        Check((LAttempt <> nil) and (LAttempt.Count = 2), 'First sufficient admission allowance publishes the complete world');
        LNext := LAttempt;
        LFit := LAt;
      except
        on ENyxPascalAnalysisWorkLimit do
          Check((LAttempt = nil) and (LBudget.Receipt.Total <= LAt), 'Every smaller evidence/copy/boundary allowance refuses without a partial world');
      end;
    finally
      LBudget.Retire;
    end;

    if LFit > 0 then Break;
  end;
  Check(LFit > 1, 'Actual module admission has a finite first fit');
  WriteLn('coherence work / first fit=', LFit, ' every smaller allowance refused');
  LFound := LNext.Lookup(LNext.At(1, 0, LNext.ModuleAt(1).Syntax.Lexical.Scalars), NyxPascalBindingName('TMarker'));
  Check((LFound.Status = pbsBound) and (LNext.TypeOfSymbol(LFound.SymbolAt(0)).Status = pbsBound),
    'Published world and symbols remain usable after work retirement');
  LCancel := NewNyxCancellationScope;
  LCancel.Cancel;
  LAttempt := nil;
  LRejected := False;
  try
    LAttempt := WithNyxPascalSemanticModule(LWorld, LView, LCancel);
  except
    on ENyxPascalAnalysisCancelled do LRejected := True;
  end;
  Check(LRejected and (LAttempt = nil), 'Cancelled evidence checks publish no partial world');
  LAttempt := nil;
  LRejected := False;
  try
    LAttempt := LNext.WithModule(View('System', Source(Root(8))));
  except
    on E: ENyxPascalModuleContext do LRejected := E.RelatedModule = TNyxText('Consumer');
  end;
  Check(LRejected and (LAttempt = nil) and (LNext.Count = 2), 'Replacing System cannot borrow a stale consumer root from another configuration');
  LNext := LWorld.WithModule(View('System', Source(Root(8))));
  Check((LNext.Count = 1) and (NyxPascalCapturedSize(NyxPascalBootstrapContext(
    LNext.ModuleAt(0).Source.InitialContext), NyxPascalSizeReference('Pointer')).Bytes = 8),
    'A sole replaced module does not conflict with its retired predecessor');
  Check(NyxPascalCapturedSize(NyxPascalBootstrapContext(LWorld.ModuleAt(0).Source.InitialContext),
    NyxPascalSizeReference('Pointer')).Bytes = 4, 'Retained earlier worlds keep their own configuration');
end;

{ Prefix reconstruction retains one immutable initial context. A checked parent
  world can reuse that exact evidence proof, not merely a similar value/target. }
procedure ExactContextReuse;
const
  CText: TNyxText = 'unit Expanding; interface type TOne=class end; TTwo=class end; implementation end.';
var
  LRoot, LContext: INyxPascalCompilerContext;
  LSource, LPrefix: INyxPascalTranslation;
  LBefore, LAfter, LEqualCopy: INyxPascalTranslatedSyntax;
  LWorld, LNext, LAttempt: INyxPascalSemanticWorld;
  LBudget: INyxPascalAnalysisBudget;
  LAt, LFirst, LLast: Integer;
  LRejected: Boolean;
begin
  LRoot := Root(4);
  for LAt := 1 to 8 do
    LRoot := NyxPascalWithCapturedSize(LRoot, NyxPascalSizeReference('Primitive' + TNyxText(IntToStr(LAt))),
      NyxPascalSizeFact(pssKnown, ppmFPC, ppkFalse, LAt));
  LContext := Source(LRoot);
  LSource := NewNyxPascalTranslation(NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('expanding.pas'), CText), NyxPascalPhysicalFile('expanding.pas'),
    LContext, TNyxPascalTranslationOptions.Defaults);
  LFirst := -1;
  LLast := -1;
  for LAt := 0 to LSource.Count - 1 do
  begin

    if LSource.Text(LAt) = TNyxText('TTwo') then LFirst := LAt;

    if LSource.Text(LAt) = TNyxText('implementation') then LLast := LAt + 1;
  end;
  Check((LFirst > 0) and (LLast > LFirst), 'Two original prefix boundaries are retained');
  LPrefix := NewNyxPascalTranslationPrefix(LSource, LFirst, TNyxPascalTranslationOptions.Defaults);
  LBefore := NewNyxPascalTranslatedSyntax(LPrefix, TNyxPascalSyntaxOptions.Defaults);
  LPrefix := NewNyxPascalTranslationPrefix(LSource, LLast, TNyxPascalTranslationOptions.Defaults);
  LAfter := AppendNyxPascalTranslatedSyntax(LBefore, LPrefix, TNyxPascalSyntaxOptions.Defaults);
  Check(LBefore.Source.InitialContext = LAfter.Source.InitialContext,
    'Sealed prefix views retain the same immutable initial-context object');
  LWorld := World(LContext).WithModule(LBefore);
  LBudget := NewNyxPascalAnalysisBudget(10000);
  try
    LNext := WithNyxPascalSemanticModule(LWorld, LAfter, LBudget);
    Check((LNext.Count = 2) and (LBudget.Receipt.Used(papWorldEntry) = 3),
      'Exact context reuse charges one proof plus two retained module-vector entries');
  finally
    LBudget.Retire;
  end;
  LEqualCopy := View('Expanding', CaptureNyxPascalCompilerContext(LContext));
  Check((LEqualCopy.Source.InitialContext <> LAfter.Source.InitialContext) and
    LEqualCopy.Source.InitialContext.SameFacts(LAfter.Source.InitialContext),
    'A value-equal independently captured context is a different proof identity');
  LBudget := NewNyxPascalAnalysisBudget(10000);
  try
    LNext := WithNyxPascalSemanticModule(LNext, LEqualCopy, LBudget);
    Check(LBudget.Receipt.Used(papWorldEntry) > 3, 'Independent captures still perform the complete qualified evidence comparison');
  finally
    LBudget.Retire;
  end;
  LBudget := NewNyxPascalAnalysisBudget(1);
  LRejected := False;
  try
    try
      LAttempt := WithNyxPascalSemanticModule(LWorld, LAfter, LBudget);
    except
      on ENyxPascalAnalysisWorkLimit do LRejected := True;
    end;
    Check(LRejected and (LAttempt = nil), 'Reusing evidence cannot bypass vector/cancellation budgets or expose a partial world');
  finally
    LBudget.Retire;
  end;
end;

{ Exercise existing indexed-provider dependency loading and its unchanged typed
  gap metadata. A configuration conflict is unavailable context, not internal
  failure or a partial completion page; no authored code or HTTP host executes. }
procedure CompletionPath;
const
  CText: TNyxText = 'unit Consumer; interface uses Leaf; var Current:TMarker; ' +
    'implementation procedure Run; begin Current.Val; end; end.';
var
  LFiles: INyxPascalFileCatalog;
  LModules: INyxPascalModuleCatalog;
  LProvider: INyxPascalCompletionProvider;
  LPort: INyxCancellablePascalCompletionProvider;
  LSources: INyxPascalSourceSnapshot;
  LRequest: INyxPascalCompletionRequest;
  LReply: INyxPascalCompletionResult;
  LMetadata: INyxOwnedPascalCompletionResult;
  LContext: INyxPascalCompilerContext;
  LBudget: INyxPascalAnalysisBudget;
begin
  LContext := Source(Root(4));
  LFiles := NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('system.pas'), 'unit System; interface type TObject=class end; implementation end.')
    .WithFile(NyxPascalPhysicalFile('leaf.pas'), 'unit Leaf; interface type TMarker=class Value:LongInt; end; implementation end.');
  LModules := NewNyxPascalModuleCatalog
    .WithModule(NewNyxPascalModuleSource(NyxPascalModule('System'), NyxPascalPhysicalFile('system.pas')).InContext(LContext, TNyxPascalTranslationOptions.Defaults))
    .WithModule(NewNyxPascalModuleSource(NyxPascalModule('Leaf'), NyxPascalPhysicalFile('leaf.pas')).InContext(Source(Root(8)), TNyxPascalTranslationOptions.Defaults));
  LProvider := NewNyxIndexedPascalCompletionProvider(LContext, LFiles, LModules, TNyxOwnedPascalCompletionOptions.Defaults);
  LSources := CaptureNyxPascalSources(NewNyxPascalProject(NyxPascalUnit('Consumer'), CText));
  LRequest := NewNyxPascalCompletionRequest(LSources).At(TNyxPascalSourceRef.Main, Pos('Current.Val;', CText) - 1 + Length('Current.Val'));
  LReply := LProvider.Complete(LRequest);
  Supports(LReply, INyxOwnedPascalCompletionResult, LMetadata);
  Check((LReply.Status = pcsUnavailable) and (LReply.Count = 0) and (LReply.Total = 0) and
    (LMetadata.Gap = pcgBinding) and (LMetadata.BindingIssue = pbiCompilerContext) and
    (LMetadata.RelatedName = TNyxText('Consumer')), 'Indexed loading surfaces a typed compiler-context conflict with no partial rows');
  Check(LSources.Text(TNyxPascalSourceRef.Main) = CText, 'Unavailable completion never changes authored project source');
  LModules := LModules.WithModule(NewNyxPascalModuleSource(NyxPascalModule('Leaf'), NyxPascalPhysicalFile('leaf.pas'))
    .InContext(LContext.InMode(ppmObjFPC), TNyxPascalTranslationOptions.Defaults));
  LProvider := NewNyxIndexedPascalCompletionProvider(LContext, LFiles, LModules, TNyxOwnedPascalCompletionOptions.Defaults);
  LReply := LProvider.Complete(LRequest);
  Check((LReply.Status = pcsReady) and (LReply.Count = 1) and (LReply.Item(0).Name = TNyxText('Value')),
    'Compatible captured roots allow the ordinary completion/dependency journey');
  Supports(LProvider, INyxCancellablePascalCompletionProvider, LPort);
  LBudget := NewNyxPascalAnalysisBudget(1);
  try
    LReply := LPort.CompleteWithCancellation(LRequest, LBudget);
    Supports(LReply, INyxOwnedPascalCompletionResult, LMetadata);
    Check((LReply.Status = pcsUnavailable) and (LReply.Count = 0) and (LMetadata.Gap = pcgBudget),
      'A caller work-budget refusal is reported as a budget gap rather than internal failure');
  finally
    LBudget.Retire;
  end;
end;

begin
  try
    Evidence;
    ReplacementAndWork;
    ExactContextReuse;
    CompletionPath;
    WriteLn('PASS ', GChecks, ' captured module coherence checks');
  except
    on E: Exception do
    begin
      WriteLn('FAIL ', E.Message);
      {$ifndef PAS2JS}DumpExceptionBackTrace(StdErr);{$endif}
      ExitCode := 1;
    end;
  end;
end.
