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
program nyx_pascal_macro_tests;
{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.bytes, nyx.editing, nyx.scheduler,
  nyx.pascal.context, nyx.pascal.lexing, nyx.pascal.macros, nyx.pascal.files,
  nyx.pascal.translation, nyx.pascal.syntax, nyx.pascal.syntax.translated,
  nyx.pascal.modules, nyx.pascal.sources, nyx.pascal.project, nyx.pascal.names,
  nyx.pascal.drafts, nyx.pascal.completion, nyx.pascal.completion.owned;

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

function Definition(const AName, ABody: TNyxText; AOccurrence: Integer = 0): TNyxPascalMacroDefinition;
begin
  Result := Default(TNyxPascalMacroDefinition);
  Result.Name := NyxPascalDefine(AName);
  Result.Replacement := ABody;
  Result.Occurrence := AOccurrence;
  Result.Span := NyxPascalSpan(100, NyxTextScalarCount(ABody));
  Result.Known := True;
end;

function Facts: INyxPascalCompilerContext;
begin
  Result := NewNyxPascalCompilerContext(ppcFPC, TNyxPascalCompilerVersion.Create(3, 3, 1),
    ppmDelphi).CompleteDefines(True).WithFeature(ppfReplacementMacros, ppkFalse);
end;

function Expanded(const AEnvironment: INyxPascalMacroEnvironment;
  const AText: TNyxText; AUse: TNyxPascalMacroUse = pmuSource): INyxPascalMacroExpansion;
begin
  Result := AEnvironment.Expand(AText, AUse, TNyxPascalLexicalOptions.Defaults,
    65536, 4096, 32);
end;

procedure Environment;
var
  LBefore: INyxPascalMacroEnvironment;
  LEnvironment: INyxPascalMacroEnvironment;
  LExpansion: INyxPascalMacroExpansion;
  LDefinition: TNyxPascalMacroDefinition;
  LCancelled: INyxCancellationScope;
  LRejected: Boolean;
begin
  LBefore := NewNyxPascalMacroEnvironment.WithDefinition(Definition('Value', '11'));
  LEnvironment := LBefore.WithDefinition(Definition('AliasValue', 'Value', 1));
  Check(Expanded(LEnvironment, 'aliasVALUE').Joined = TNyxText('11'), 'Names compare without ASCII case');
  LEnvironment := LEnvironment.WithDefinition(Definition('Value', '13'));
  LExpansion := Expanded(LEnvironment, 'AliasValue');
  Check((LExpansion.Joined = TNyxText('13')) and (LExpansion.TraceCount = 2) and
    (LExpansion.Trace(1).Parent = 0) and (LExpansion.Trace(0).Definition.Occurrence = 1),
    'Aliases are late-bound and retain recursive definition ancestry');
  Check(Expanded(LBefore, 'Value').Joined = TNyxText('11'), 'Redefinition preserves earlier tables');
  Check(Expanded(LEnvironment, '&Value ''Value'' // Value').Joined =
    TNyxText('&Value ''Value'' // Value'), 'Escapes, strings and comments retain literal spelling');
  LExpansion := Expanded(LEnvironment, 'defined(Value) and (AliasValue = 13)', pmuCondition);
  Check(LExpansion.Joined = TNyxText('defined(Value) and (13 = 13)'),
    'Condition replacement protects defined arguments');
  LEnvironment := LEnvironment.WithDefinition(Definition('Value', ''));
  Check(Expanded(LEnvironment, 'Value True').Joined = TNyxText(' True'), 'Empty replacement retains its surrounding tokens');
  LEnvironment := LEnvironment.Without(NyxPascalDefine('Value'));
  Check(Expanded(LEnvironment, 'Value').Joined = TNyxText('Value'), 'Undef removes replacement status');
  LEnvironment := LEnvironment.WithDefinition(Definition('Value', 'AliasValue'));
  LRejected := False;
  LExpansion := nil;
  try
    LExpansion := Expanded(LEnvironment, 'AliasValue');
  except
    on ENyxPascalMacroCycle do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LExpansion = nil), 'A recursive cycle publishes no partial effective tokens');
  LDefinition := Definition('Value', '13');
  LDefinition.Known := False;
  LEnvironment := LEnvironment.WithDefinition(LDefinition);
  LRejected := False;
  try
    Expanded(LEnvironment, 'Value');
  except
    on ENyxPascalMacro do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'A conditional replacement never chooses an invented body');
  LEnvironment := LBefore.WithDefinition(Definition('DirectiveValue', '(*$define OTHER*)'));
  LRejected := False;
  try
    Expanded(LEnvironment, 'DirectiveValue');
  except
    on ENyxPascalMacro do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Unsupported textual directive effects refuse before publication');
  LRejected := False;
  try
    LBefore.Expand('Value', pmuSource, TNyxPascalLexicalOptions.Defaults, 5, 100, 32);
  except
    on ENyxPascalMacroLimit do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Expanded work charges both invocation and replacement bytes');
  LEnvironment := LBefore.WithDefinition(Definition('AliasValue', 'Value'));
  LRejected := False;
  try
    LEnvironment.Expand('AliasValue', pmuSource, TNyxPascalLexicalOptions.Defaults, 65536, 4096, 1);
  except
    on ENyxPascalMacroLimit do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'A finite alias chain cannot bypass the selected recursion depth');
  LEnvironment := LBefore.WithDefinition(Definition('Many', '1 + 2'));
  LRejected := False;
  try
    LEnvironment.Expand('Many', pmuSource, TNyxPascalLexicalOptions.Defaults, 65536, 2, 32);
  except
    on ENyxPascalMacroLimit do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Excess effective tokens refuse without truncating a returned row set');
  LEnvironment := LBefore.WithDefinition(Definition('Caption', TNyxText('''Ready 🚀''')));
  Check(Expanded(LEnvironment, 'Caption').Joined = TNyxText('''Ready 🚀'''),
    'Supplementary Unicode survives exact replacement fragments');
  LCancelled := NewNyxCancellationScope;
  LCancelled.Cancel;
  LRejected := False;
  try
    LBefore.Expand('Value', pmuSource, TNyxPascalLexicalOptions.Defaults,
      65536, 4096, 32, LCancelled);
  except
    on ENyxPascalAnalysisCancelled do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Cancellation does not become a recoverable expansion diagnostic');
  LExpansion := Expanded(LBefore, 'Value');
  LBefore := nil;
  LEnvironment := nil;
  Check((LExpansion.Joined = TNyxText('11')) and
    (LExpansion.Trace(0).Definition.Replacement = TNyxText('11')),
    'Results retain exact fragments and traces after environment retirement');
end;

function Translate(const AText: TNyxText): INyxPascalTranslation;
begin
  Result := NewNyxPascalTranslation(NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('main.pas'), AText), NyxPascalPhysicalFile('main.pas'),
    Facts.WithSymbol(NyxPascalDefine('UNKNOWN'), ppdUnknown), TNyxPascalTranslationOptions.Defaults);
end;

procedure Translation;
const
  CMain: TNyxText = 'unit Main; interface {$macro on} {$define QUAL:=Parts.}' +
    '{$define COUNT:=11}{$define ALIASCOUNT:=COUNT}' +
    '{$if defined(COUNT) and (ALIASCOUNT = 11)} type TReady = record end; {$endif}' +
    '{$I parts.inc} var Item:QUAL TData; implementation end.';
  CParts: TNyxText = TNyxText('// Dedicated Unicode qualification 🚀') + #10 +
    '{$define QUAL:=Other.}';
var
  LSource: INyxPascalTranslation;
  LMacros: INyxPascalTranslationMacros;
  LView: INyxPascalTranslatedSyntax;
  LToken: TNyxPascalTranslatedToken;
  LAt: Integer;
  LExpanded: Integer;
  LFirstState: Integer;
  LLastState: Integer;
  LOrigin: TNyxPascalSyntaxOrigin;
begin
  LSource := NewNyxPascalTranslation(NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('main.pas'), CMain)
    .WithFile(NyxPascalPhysicalFile('parts.inc'), CParts), NyxPascalPhysicalFile('main.pas'),
    Facts, TNyxPascalTranslationOptions.Defaults);
  Check(LSource.Resolved and (LSource.OccurrenceCount = 2),
    'Recursive conditional replacement and include side effects remain resolved');
  Supports(LSource, INyxPascalTranslationMacros, LMacros);
  LExpanded := -1;
  LFirstState := -1;
  LLastState := -1;
  for LAt := 0 to LSource.Count - 1 do
  begin
    LToken := LSource.Token(LAt);

    if LToken.Expanded then
    begin
      LExpanded := LAt;
      Check(LSource.Text(LAt) = TNyxText('QUAL'), 'Physical access retains the exact invocation');
      Check(LMacros.Expansion(LToken.Expansion).Trace(0).Definition.Occurrence = 1,
        'Include-defined replacements retain their independent physical definition');
      Check(NyxPascalOccurrenceLexical(LSource, 1).Slice(
        LMacros.Expansion(LToken.Expansion).Trace(0).Definition.Span) = TNyxText('Other.'),
        'Definition spans remain exact after a supplementary Unicode prefix');
    end;

    if LMacros.StateMacros(LToken.State).Has(NyxPascalDefine('QUAL')) then
    begin

      if LFirstState < 0 then
      begin
        LFirstState := LToken.State;
      end;
      LLastState := LToken.State;
    end;
  end;
  Check((LExpanded >= 0) and (LFirstState <> LLastState) and
    (LMacros.StateMacros(LFirstState).Definition(NyxPascalDefine('QUAL')).Replacement = TNyxText('Parts.')) and
    (LMacros.StateMacros(LLastState).Definition(NyxPascalDefine('QUAL')).Replacement = TNyxText('Other.')),
    'Saved states distinguish replacement bodies even when Boolean define facts match');
  LView := NewNyxPascalTranslatedSyntax(LSource, TNyxPascalSyntaxOptions.Defaults);
  Check(LView.CompleteTranslation and (Pos('Other . TData', LView.Syntax.Lexical.Source) > 0),
    'Declared syntax consumes effective tokens while retaining authored source');
  for LAt := 0 to LView.OriginCount - 1 do
  begin
    LOrigin := LView.Origin(LAt);

    if LOrigin.Expansion >= 0 then
    begin
      Check(LOrigin.PhysicalSpan.Length = 4, 'Expanded origins keep the complete invocation range');
      Check(LView.VirtualAt(LOrigin.Occurrence, LOrigin.PhysicalSpan.Start) = -1,
        'A macro invocation does not invent a unique expanded caret');
      Check(LView.Part(LAt, NyxPascalSpan(LOrigin.VirtualSpan.Start, 1)).PhysicalSpan.Length = 4,
        'Partial virtual token slices retain nonlinear physical provenance');
    end;
  end;
  LSource := Translate('unit Main; interface {$macro on}{$define A:=B}{$define B:=A} type A; implementation end.');
  Check(not LSource.Resolved and (LSource.Diagnostic(0).Issue = ptiMacroCycle),
    'Recursive source replacements stop certainty with a typed physical diagnostic');
  LSource := Translate('unit Main; interface {$macro on}{$define A:=1}{$macro off} const A=2; implementation end.');
  Check(LSource.Resolved, 'Disabled replacement leaves ordinary source names intact');
  LSource := Translate('unit Main; interface {$macro on}{$if UNKNOWN}{$define A:=1}{$endif} const X=A; implementation end.');
  Check(not LSource.Resolved, 'Conditional replacement writes cannot authorize a definite substitution');
  LSource := Translate('unit Main; interface {$macro on}{$define NUMBER:=1} const Value=NUMBER.5; implementation end.');
  Check(not LSource.Resolved and (LSource.Diagnostic(0).Issue = ptiMacroContext),
    'Cross-boundary token fusion refuses instead of publishing a different numeric meaning');
  LSource := Translate('unit Main; interface {$macro on}{$define A:=1}{$I missing.inc} implementation end.');
  Supports(LSource, INyxPascalTranslationMacros, LMacros);
  Check(not LMacros.StateMacros(LSource.StateCount - 1).Definition(NyxPascalDefine('A')).Known,
    'Unavailable include effects revoke replacement knowledge while retaining source evidence');
end;

procedure Completion;
const
  CUnit: TNyxText = 'unit MacroFixture; interface {$macro on}' +
    '{$define MEMBER:=AlphaField:TValue;} type TValue=record end;' +
    'TData=record MEMBER end; implementation end.';
var
  LFiles: INyxPascalFileCatalog;
  LModules: INyxPascalModuleCatalog;
  LSources: INyxPascalSourceSnapshot;
  LProvider: INyxPascalCompletionProvider;
  LReply: INyxPascalCompletionResult;
  LDefinitions: INyxOwnedPascalCompletionResult;
  LPrefix: TNyxText;
begin
  LFiles := NewNyxPascalFileCatalog(pfcExact).WithFile(NyxPascalPhysicalFile('macrofixture.pas'), CUnit);
  LModules := NewNyxPascalModuleCatalog.WithModule(NewNyxPascalModuleSource(
    NyxPascalModule('MacroFixture'), NyxPascalPhysicalFile('macrofixture.pas')));
  LPrefix := 'unit Consumer; interface uses MacroFixture; implementation ' +
    'procedure Run; var Data:TData; begin Data.';
  LSources := CaptureNyxPascalSources(NewNyxPascalProject(NyxPascalUnit('Consumer'),
    'unit Consumer; interface implementation end.'))
    .WithBuffer(TNyxPascalSourceRef.Main, LPrefix + 'Alphx; end; end.');
  LProvider := NewNyxIndexedPascalCompletionProvider(Facts, LFiles, LModules,
    TNyxOwnedPascalCompletionOptions.Defaults);
  LReply := LProvider.Complete(NewNyxPascalCompletionRequest(LSources)
    .At(TNyxPascalSourceRef.Main, NyxTextScalarCount(LPrefix) + 4));
  Check((LReply.Status = pcsReady) and (LReply.Count = 1) and
    (LReply.Item(0).InsertText = TNyxText('AlphaField')), 'Declared member completion consumes replacement-generated members');
  Supports(LReply, INyxOwnedPascalCompletionResult, LDefinitions);
  Check(LDefinitions.DefinitionSource(0).Slice(LDefinitions.Definition(0).Span) = TNyxText('MEMBER'),
    'Completion definitions retain the authored invocation rather than an invented physical name range');
end;

begin
  try
    Environment;
    Translation;
    Completion;
    WriteLn('PASS ', GChecks, ' ordered replacement macro checks');
  except
    on LError: Exception do
    begin
      WriteLn('FAIL / ', LError.Message);
      {$ifndef PAS2JS}DumpExceptionBackTrace(StdErr);{$endif}
      ExitCode := 1;
    end;
  end;
end.
