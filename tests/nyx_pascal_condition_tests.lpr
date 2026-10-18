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
      TNyxPascalTranslationOptions.Defaults, TNyxPascalSyntaxOptions.Defaults,
      TNyxPascalConditionalOptions.Defaults, LCancellation);
  except
    on ENyxPascalAnalysisCancelled do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LResult = nil), 'Cancellation retains no partial semantic result');
end;

begin
  try
    Prefixes;
    Refusals;
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
