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
program nyx_pascal_condition_tests;
{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.scheduler, nyx.pascal.lexing, nyx.pascal.context,
  nyx.pascal.files, nyx.pascal.translation, nyx.pascal.syntax,
  nyx.pascal.syntax.translated, nyx.pascal.binding, nyx.pascal.conditions;

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

function Facts(AMode: TNyxPascalLanguageMode = ppmDelphi): INyxPascalCompilerContext;
begin
  Result := NewNyxPascalCompilerContext(ppcFPC,
    TNyxPascalCompilerVersion.Create(3, 3, 1), AMode).CompleteDefines(True);
end;

function World: INyxPascalSemanticWorld;
var
  LFiles: INyxPascalFileCatalog;
  LSystem: INyxPascalCompilerContext;
  LOptions: TNyxPascalTranslationOptions;
begin
  { Controlled portable facts qualify the algorithm. Separate native compiler
    consumers supply real measured contexts; these are not SDK replacements. }
  LSystem := NyxPascalWithCapturedSize(Facts(ppmFPC).WithOption('H', ppkFalse),
    NyxPascalSizeReference('System.Byte'), NyxPascalSizeFact(pssKnown, ppmFPC, ppkFalse, 1));
  LSystem := NyxPascalWithCapturedSize(LSystem, NyxPascalSizeReference('System.SmallInt'),
    NyxPascalSizeFact(pssKnown, ppmFPC, ppkFalse, 2));
  LFiles := NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('system.pas'),
      'unit System; interface type Integer=SmallInt; implementation end.')
    .WithFile(NyxPascalPhysicalFile('objpas.pas'),
      'unit ObjPas; {$mode objfpc} interface type Integer=LongInt; implementation end.');
  LOptions := TNyxPascalTranslationOptions.Defaults.Lexical(
    TNyxPascalLexicalOptions.Defaults.NestedComments(True));
  Result := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults
    .WithImplicitScopes(pispCompilerUnits))
    .WithModule(NewNyxPascalTranslatedSyntax(NewNyxPascalTranslation(LFiles,
      NyxPascalPhysicalFile('system.pas'), LSystem, LOptions), TNyxPascalSyntaxOptions.Defaults))
    .WithModule(NewNyxPascalTranslatedSyntax(NewNyxPascalTranslation(LFiles,
      NyxPascalPhysicalFile('objpas.pas'), Facts, TNyxPascalTranslationOptions.Defaults),
      TNyxPascalSyntaxOptions.Defaults));
end;

function Analyze(const AText: TNyxText; const AWorld: INyxPascalSemanticWorld;
  const AOptions: TNyxPascalConditionalOptions): INyxPascalSemanticTranslation;
begin
  Result := NewNyxPascalSemanticTranslation(NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('case.pas'), AText), NyxPascalPhysicalFile('case.pas'),
    Facts, AWorld, TNyxPascalTranslationOptions.Defaults,
    TNyxPascalSyntaxOptions.Defaults, AOptions);
end;

procedure Prefixes;
const
  CText: TNyxText = 'unit ConditionCase; interface ' +
    '{$if declared(TFuture)}{$fatal Future name leaked}{$endif}' +
    'type TFuture=Byte; ' +
    '{$if sizeof(TFuture)=1}{$define CORRECT_BYTE}{$else}{$fatal Wrong byte}{$endif}' +
    'implementation end.';
var
  LResult: INyxPascalSemanticTranslation;
  LFinal: INyxPascalCompilerContext;
  LNeeds: INyxPascalConditionNeeds;
  LDeclarations: INyxPascalConditionDeclarations;
begin
  LNeeds := PlanNyxPascalCondition(Facts, 'declared(TFuture) or declared(TFUTURE)');
  Check(LNeeds.SyntaxSupported and Supports(LNeeds, INyxPascalConditionDeclarations, LDeclarations) and
    (LDeclarations.DeclarationCount = 1) and (LDeclarations.DeclarationName(0).Name = TNyxText('tfuture')),
    'Declaration requests are typed, deduplicated and distinct from compiler defines');
  LResult := Analyze(CText, World, TNyxPascalConditionalOptions.Defaults);
  WriteLn('prefix result / answers=', LResult.Answers, ' passes=', LResult.Passes,
    ' pending=', LResult.Pending, ' issue=', Ord(LResult.BindingIssue), ' ', LResult.RelatedName);
  Check(LResult.Source.Resolved and not LResult.Pending and
    (LResult.Answers = 2) and (LResult.Passes = 3),
    'Ordered scope semantics resolve an absent future name before its later declaration and alias size');
  LFinal := LResult.Source.State(LResult.Source.StateCount - 1);
  Check(LFinal.Symbol(NyxPascalDefine('CORRECT_BYTE')).IsDefined = ppkTrue,
    'The actual admitted branch publishes its ordinary compiler state');
  Check(LResult.Source.Files.TextAt(0) = CText, 'Refinement preserves every original source scalar');
  LResult := Analyze('unit ConditionCase; interface type Integer=Byte; ' +
    '{$if (sizeof(Integer)=1) and (sizeof(System.Integer)=2)}{$define SHADOW_OK}{$endif}' +
    'implementation end.', World, TNyxPascalConditionalOptions.Defaults);
  Check(LResult.Source.Resolved and (LResult.Answers = 1) and
    (LResult.Source.State(LResult.Source.StateCount - 1).Symbol(
      NyxPascalDefine('SHADOW_OK')).IsDefined = ppkTrue),
    'A local alias and qualified System alias resolve independently through their bound source targets');
end;

procedure Refusals;
var
  LResult: INyxPascalSemanticTranslation;
  LRejected: Boolean;
  LCancellation: INyxCancellationScope;
begin
  LResult := Analyze('unit ConditionCase; interface {$if declared(TAbsent)}type T=Byte;{$endif}implementation end.',
    nil, TNyxPascalConditionalOptions.Defaults);
  Check(LResult.Pending and (LResult.BindingIssue = pbiMissingModule) and
    (LResult.RelatedName = TNyxText('ObjPas')),
    'An uncaptured implicit unit cannot prove a declaration absent');
  LResult := Analyze('unit ConditionCase; interface type TRecord=record X:Byte; end; ' +
    '{$if sizeof(TRecord)=1}const Wrong=1;{$endif}implementation end.',
    World, TNyxPascalConditionalOptions.Defaults);
  Check(LResult.Pending and (LResult.BindingIssue = pbiUnsupportedType) and
    not LResult.Source.Resolved, 'Unsupported layout does not borrow a host or guessed record size');
  LResult := nil;
  LRejected := False;
  try
    LResult := Analyze('unit ConditionCase; interface {$if declared(TAbsent)}{$endif}implementation end.',
      World, TNyxPascalConditionalOptions.Defaults.UpToPasses(1));
  except
    on ENyxPascalTranslationLimit do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LResult = nil), 'Pass exhaustion publishes no partially refined result');
  LRejected := False;
  try
    LResult := Analyze('unit ConditionCase; interface implementation end.',
      World, TNyxPascalConditionalOptions.Defaults.UpToTokens(1));
  except
    on ENyxPascalTranslationLimit do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LResult = nil), 'Aggregate exhaustion refuses before a partial source is published');
  LCancellation := NewNyxCancellationScope;
  LCancellation.Cancel;
  LRejected := False;
  try
    LResult := NewNyxPascalSemanticTranslation(NewNyxPascalFileCatalog(pfcExact)
      .WithFile(NyxPascalPhysicalFile('case.pas'), 'unit ConditionCase; interface implementation end.'),
      NyxPascalPhysicalFile('case.pas'), Facts, World,
      TNyxPascalTranslationOptions.Defaults.UpToTokens(1), TNyxPascalSyntaxOptions.Defaults,
      TNyxPascalConditionalOptions.Defaults.UpToTokens(1), LCancellation);
  except
    on ENyxPascalAnalysisCancelled do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LResult = nil), 'Cancellation retains no partial semantic result');
end;

{ Both unchanged ceilings refuse atomically; streaming work records the actual
  failing admission instead of assigning it to a reconstructed whole pass. }
procedure ExpectTokenLimit(const AFiles: INyxPascalFileCatalog;
  const AWorld: INyxPascalSemanticWorld; AViewLimit, AAggregateLimit,
  AWorkBefore, ASourceBefore, ARound: Integer; AViewFailure: Boolean;
  APhase: TNyxPascalConditionalWorkPhase = pcwpTranslation);
var
  LResult: INyxPascalSemanticTranslation;
  LRejected: Boolean;
begin
  LResult := nil;
  LRejected := False;
  try
    LResult := NewNyxPascalSemanticTranslation(AFiles, NyxPascalPhysicalFile('case.pas'),
      Facts, AWorld, TNyxPascalTranslationOptions.Defaults.UpToTokens(AViewLimit),
      TNyxPascalSyntaxOptions.Defaults,
      TNyxPascalConditionalOptions.Defaults.UpToTokens(AAggregateLimit));
  except
    on LLimit: ENyxPascalTokenLimit do
    begin
      LRejected := True;
      Check(AViewFailure and (LLimit.TokenCount = AViewLimit) and
        (LLimit.TokenLimit = AViewLimit), 'The unchanged whole-view ceiling still bounds all logical tokens');
    end;
    on LLimit: ENyxPascalConditionalWorkLimit do
    begin
      LRejected := True;
      Check(not AViewFailure and (LLimit.Phase = APhase) and
        (LLimit.WorkBefore = AWorkBefore) and (LLimit.Maximum = AAggregateLimit) and
        (LLimit.Translated = ASourceBefore) and (LLimit.RefinementRound = ARound) and
        (LLimit.Requested > LLimit.Maximum - LLimit.WorkBefore),
        'Aggregate refusal retains the exact real stage, work and ordered source boundary');
    end;
  end;
  Check(LRejected and (LResult = nil), 'Typed work diagnostics publish no partial semantic result');
end;

procedure TokenBudgets;
const
  CPlain: TNyxText = 'unit BudgetCase; interface implementation end.';
  CReplay: TNyxText = 'unit BudgetCase; interface type Owned=Byte; ' +
    '{$if declared(Owned)}const Yes=1;{$endif} implementation end.';
  CConstant: TNyxText = 'unit BudgetCase; interface const Amount=2; ' +
    '{$if Amount=2}const Yes=1;{$endif} implementation end.';
var
  LFiles: INyxPascalFileCatalog;
  LRaw: INyxPascalTranslation;
  LLimited: INyxPascalTranslation;
  LUnresolved: INyxPascalUnresolvedConditions;
  LWorld: INyxPascalSemanticWorld;
  LResult: INyxPascalSemanticTranslation;
  LWork: INyxPascalSemanticWork;
  LCount: Integer;
  LPrefix: Integer;
  LRejected: Boolean;
begin
  LWorld := World;
  LFiles := NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('case.pas'), CPlain);
  LRaw := NewNyxPascalTranslation(LFiles, NyxPascalPhysicalFile('case.pas'),
    Facts, TNyxPascalTranslationOptions.Defaults);
  LCount := LRaw.Count;
  LLimited := nil;
  LRejected := False;
  try
    LLimited := NewNyxPascalTranslation(LFiles, NyxPascalPhysicalFile('case.pas'),
      Facts, TNyxPascalTranslationOptions.Defaults.UpToTokens(LCount - 1));
  except
    on LLimit: ENyxPascalTokenLimit do
    begin
      LRejected := True;
      Check((LLimit.TokenCount = LCount - 1) and (LLimit.TokenLimit = LCount - 1),
        'Plain translation retains exact admitted work without conditional metadata');
    end;
  end;
  Check(LRejected and (LLimited = nil), 'Plain token exhaustion retains atomic refusal');
  LLimited := NewNyxPascalTranslation(LFiles, NyxPascalPhysicalFile('case.pas'),
    Facts, TNyxPascalTranslationOptions.Defaults.UpToTokens(LCount));
  Check(LLimited.Count = LCount, 'An exactly fitting translation still succeeds at its original ceiling');
  ExpectTokenLimit(LFiles, LWorld, LCount - 1, LCount * 4, 0, 0, 1, True);
  ExpectTokenLimit(LFiles, LWorld, LCount + 1, LCount - 1, LCount - 1, LCount - 1, 1, False);
  ExpectTokenLimit(LFiles, LWorld, LCount - 1, LCount - 1, 0, 0, 1, True);
  Check(LFiles.TextAt(0) = CPlain, 'Failure observations leave the exact captured source unchanged');

  LFiles := NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('case.pas'), CReplay);
  LRaw := NewNyxPascalTranslation(LFiles, NyxPascalPhysicalFile('case.pas'),
    Facts, TNyxPascalTranslationOptions.Defaults);
  Check(Supports(LRaw, INyxPascalUnresolvedConditions, LUnresolved) and
    (LUnresolved.UnresolvedCount = 1), 'The replay case has one independently observed semantic question');
  LCount := LRaw.Count;
  LPrefix := LUnresolved.UnresolvedAt(0).Origin.PrefixCount;
  ExpectTokenLimit(LFiles, LWorld, LCount, LCount + LPrefix - 1,
    LCount + LPrefix - 1, LCount - 1, 2, False);
  ExpectTokenLimit(LFiles, LWorld, LCount, 2 * LPrefix,
    LPrefix + 1, LPrefix + 1, 1, False, pcwpPrefix);
  LResult := NewNyxPascalSemanticTranslation(LFiles, NyxPascalPhysicalFile('case.pas'),
    Facts, LWorld, TNyxPascalTranslationOptions.Defaults.UpToTokens(LCount),
    TNyxPascalSyntaxOptions.Defaults,
    TNyxPascalConditionalOptions.Defaults.UpToTokens(LCount + LPrefix));
  Check((LResult.Passes = 2) and (LResult.Answers = 1) and LResult.Source.Resolved and
    (LResult.WorkTokens = LCount + LPrefix),
    'An exactly fitting traversal charges the actual source once and its parsed prefix');
  Check(Supports(LResult, INyxPascalSemanticWork, LWork) and
    (LWork.TranslationTokens = LResult.Source.Count) and
    (LWork.PrefixTokens = LPrefix) and (LWork.ConstantSteps = 0),
    'Detached work receipts identify genuine single-traversal source reuse');
  Check((LFiles.TextAt(0) = CReplay) and (LResult.Source.Files.TextAt(0) = CReplay),
    'A failed attempt followed by admission preserves every original source scalar');

  LFiles := NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('case.pas'), CConstant);
  LRaw := NewNyxPascalTranslation(LFiles, NyxPascalPhysicalFile('case.pas'),
    Facts, TNyxPascalTranslationOptions.Defaults);
  Check(Supports(LRaw, INyxPascalUnresolvedConditions, LUnresolved) and
    (LUnresolved.UnresolvedCount = 1), 'The constant case has one exact source-scoped condition');
  LPrefix := LUnresolved.UnresolvedAt(0).Origin.PrefixCount;
  ExpectTokenLimit(LFiles, LWorld, LRaw.Count, 2 * LPrefix + 1,
    2 * LPrefix + 1, LPrefix + 1, 1, False, pcwpConstant);
  LResult := NewNyxPascalSemanticTranslation(LFiles, NyxPascalPhysicalFile('case.pas'),
    Facts, LWorld, TNyxPascalTranslationOptions.Defaults, TNyxPascalSyntaxOptions.Defaults,
    TNyxPascalConditionalOptions.Defaults);
  Check(LResult.Source.Resolved and (LResult.Answers = 1) and
    Supports(LResult, INyxPascalSemanticWork, LWork) and (LWork.ConstantSteps > 0) and
    (LResult.WorkTokens = LWork.TranslationTokens + LWork.PrefixTokens + LWork.ConstantSteps),
    'Successful constant evaluation retains all three actual work charges');
end;

procedure OrderedReaderState;
const
  CRoot: TNyxText = 'unit ReaderState; {$push}{$H-}{$macro on}' +
    '{$define SharedByte:=Byte}{$if not declared(TFuture)}' +
    '{$modeswitch nestedcomments}{$endif} interface ' +
    '{$include parts.inc} type TFuture=SharedByte; {$include parts.inc}' +
    '{ outer { inner } tail } {$pop} implementation end.';
  CPart: TNyxText = '{$if declared(TFuture)}{$define SAW_AFTER}' +
    '{$ifopt H-}{$define SAVED_POLICY}{$endif}' +
    '{$push}{$H+}{$pop}{$else}{$define SAW_BEFORE}{$endif}';
var
  LFiles: INyxPascalFileCatalog;
  LResult: INyxPascalSemanticTranslation;
  LWork: INyxPascalSemanticWork;
  LState: INyxPascalCompilerContext;
  LSettings: INyxPascalTranslationSettings;
begin
  LFiles := NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('case.pas'), CRoot)
    .WithFile(NyxPascalPhysicalFile('parts.inc'), CPart);
  LResult := NewNyxPascalSemanticTranslation(LFiles, NyxPascalPhysicalFile('case.pas'),
    Facts.WithOption('H', ppkTrue).WithFeature(ppfNestedComments, ppkFalse), World,
    TNyxPascalTranslationOptions.Defaults, TNyxPascalSyntaxOptions.Defaults,
    TNyxPascalConditionalOptions.Defaults);
  LState := LResult.Source.State(LResult.Source.StateCount - 1);
  Check(LResult.Source.Resolved and not LResult.Pending and (LResult.Answers = 3) and
    (LResult.Source.OccurrenceCount = 3),
    'One live reader resolves separate visits to an include against their actual preceding declarations');
  Check((LState.Symbol(NyxPascalDefine('SAW_BEFORE')).IsDefined = ppkTrue) and
    (LState.Symbol(NyxPascalDefine('SAW_AFTER')).IsDefined = ppkTrue) and
    (LState.Symbol(NyxPascalDefine('SAVED_POLICY')).IsDefined = ppkTrue),
    'Source conditions and included option checks retain chronological branch authority');
  Check((LState.Option('H') = ppkTrue) and (LState.Feature(ppfNestedComments) = ppkTrue) and
    Supports(LResult.Source, INyxPascalTranslationSettings, LSettings) and
    (LSettings.SettingsDepth(LResult.Source.StateCount - 1) = 0),
    'Source-aware prefix evaluation keeps module lexing while restoring complete local settings lifetime');
  Check(Supports(LResult, INyxPascalSemanticWork, LWork) and
    (LWork.TranslationTokens = LResult.Source.Count) and
    (LResult.WorkTokens = LWork.TranslationTokens + LWork.PrefixTokens + LWork.ConstantSteps) and
    (LResult.Source.Files.TextAt(0) = CRoot) and (LResult.Source.Files.TextAt(1) = CPart),
    'Whole source and actual accounting remain exact across repeated include callbacks');
  LResult := Analyze('unit Unanswered; interface type TRecord=record X:Byte; end; ' +
    '{$if sizeof(TRecord)=1}const Maybe=1;{$endif}' +
    '{$if declared(TRecord)}{$define LATER}{$endif} implementation end.',
    World, TNyxPascalConditionalOptions.Defaults);
  Check(LResult.Pending and (LResult.Answers = 0) and
    (LResult.BindingIssue = pbiUnsupportedType) and
    (LResult.Source.State(LResult.Source.StateCount - 1).Symbol(
      NyxPascalDefine('LATER')).IsDefined = ppkUnknown),
    'An unanswered earlier condition prevents later source-scoped guesses and branch donation');
end;

begin
  try
    Prefixes;
    Refusals;
    TokenBudgets;
    OrderedReaderState;
    WriteLn('PASS ', GChecks, ' ordered semantic conditional checks');
  except
    on LError: Exception do
    begin
      WriteLn('FAIL / ', LError.Message);
      {$ifndef PAS2JS}DumpExceptionBackTrace(StdErr);{$endif}
      ExitCode := 1;
    end;
  end;
end.
