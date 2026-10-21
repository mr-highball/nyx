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
program nyx_pascal_compilerplan_tests;
{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.scheduler, nyx.pascal.lexing, nyx.pascal.context,
  nyx.pascal.files, nyx.pascal.translation, nyx.pascal.compilertrace,
  nyx.pascal.compilerplan;

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

procedure OrderedRequirements;
var
  LSource: INyxPascalTranslation;
  LPrefix: INyxPascalTranslation;
  LInputs: INyxPascalUnresolvedConditions;
  LPlan: INyxPascalCompilerPlan;
  LProbe: INyxPascalCompilerProbe;
  LSizes: INyxPascalSizeProbe;
  LDeclarations: INyxPascalConditionDeclarations;
  LFiles: INyxPascalFileCatalog;
  LContext: INyxPascalCompilerContext;
  LCancel: INyxCancellationScope;
  LFailed: Boolean;
  LBefore: Integer;
begin
  LContext := NewNyxPascalCompilerContext(ppcFPC,
    TNyxPascalCompilerVersion.Create(3, 3, 1), ppmObjFPC)
    .WithSymbol(NyxPascalDefine('LOCAL'), ppdUndefined);
  LFiles := NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('unit.pp'),
      'unit UnitNeeds; {$macro on} {$define QUERY:=EXT_NAME} interface ' +
      '{$i needs.inc} {$define LOCAL} {$i needs.inc} ' +
      '{$if sizeof(Char)=2} const W=1; {$endif} ' +
      '{$if declared(TAbsent)} const D=1; {$endif} ' +
      '{$ifopt W+} const O=1; {$endif} implementation end.')
    .WithFile(NyxPascalPhysicalFile('needs.inc'),
      '{$ifndef LOCAL} {$if QUERY>0} const A=1; {$endif} ' +
      '{$else} {$if ANOTHER>0} const B=1; {$endif} {$endif}');
  LSource := NewNyxPascalTranslation(LFiles, NyxPascalPhysicalFile('unit.pp'),
    LContext, TNyxPascalTranslationOptions.Defaults);
  LPlan := PlanNyxPascalCompilerRequirements(LSource, TNyxPascalCompilerPlanOptions.Defaults);
  Check(LPlan.Count = 5, 'Only the actual unresolved condition sites enter planning');
  WriteLn('actual planned expression / ', LPlan.Expression(0), ' symbols=', LPlan.Needs(0).SymbolCount);

  if LPlan.Needs(0).SymbolCount > 0 then
  begin
    WriteLn('actual planned name / ', LPlan.Needs(0).Symbol(0).Name);
  end;
  Check((LPlan.Origin(0).Occurrence <> LPlan.Origin(1).Occurrence) and
    (LPlan.State(0) <> LPlan.State(1)) and
    (LPlan.Origin(0).FileReference.Name = TNyxText('needs.inc')),
    'Repeated includes retain exact independent pre-token states and origins');
  Check((LPlan.Needs(0).SymbolCount = 1) and
    (NyxPascalASCIIKey(LPlan.Needs(0).Symbol(0).Name) = TNyxText('ext_name')) and
    (Pos('EXT_NAME', LPlan.Expression(0)) > 0),
    'Requirements consume the actual expanded expression rather than the macro invocation name');
  Check((LPlan.Needs(2).SymbolCount = 0) and (LPlan.Needs(2).SizeCount = 1),
    'Authored sizeof stays a source binding requirement');
  Supports(LPlan.Needs(3), INyxPascalConditionDeclarations, LDeclarations);
  Check((LDeclarations.DeclarationCount = 1) and (LPlan.Needs(3).SymbolCount = 0),
    'Authored declared stays a source declaration requirement');
  Check(not LPlan.Needs(4).SyntaxSupported and (LPlan.Needs(4).SymbolCount = 0),
    'Unknown IFOPT is retained without guessed compiler symbol requests');
  LProbe := NyxFPCProbeSizeOf(NewNyxFPCCompilerProbe(NyxPascalDefine('PLAN'), ppmObjFPC)
    .WithSymbol(NyxPascalDefine('EXISTING')), NyxPascalSizeReference('System.Char'));
  LBefore := LProbe.Count;
  LProbe := LPlan.CompilerProbe(LProbe);
  Supports(LProbe, INyxPascalSizeProbe, LSizes);
  Check((LProbe.Count = LBefore + 2) and
    (NyxPascalASCIIKey(LProbe.Symbol(LBefore).Name) = TNyxText('ext_name')) and
    (NyxPascalASCIIKey(LProbe.Symbol(LBefore + 1).Name) = TNyxText('another')) and (LSizes.SizeCount = 1) and
    (NyxPascalASCIIKey(LSizes.SizeName(0).Name) = TNyxText('system.char')),
    'Probe extension preserves base order/measurements and adds only missing compiler symbols');
  Check(LPlan.CompilerProbe(LProbe).Source = LProbe.Source,
    'Repeated preparation is an exact idempotent request, not another work item');
  LPrefix := NewNyxPascalTranslationPrefix(LSource, LPlan.Origin(1).PrefixCount,
    TNyxPascalTranslationOptions.Defaults);
  Check(PlanNyxPascalCompilerRequirements(LPrefix,
    TNyxPascalCompilerPlanOptions.Defaults).Count = 1,
    'A prefix excludes the current/later unresolved sites while retaining exact earlier inputs');
  LPlan := nil;
  LFailed := False;
  try
    LPlan := PlanNyxPascalCompilerRequirements(LSource,
      TNyxPascalCompilerPlanOptions.Defaults.UpToConditions(1));
  except
    on ENyxPascalTranslationLimit do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and (LPlan = nil), 'Excess conditions refuse the complete candidate');
  LCancel := NewNyxCancellationScope;
  LCancel.Cancel;
  LFailed := False;
  try
    LPlan := PlanNyxPascalCompilerRequirements(LSource,
      TNyxPascalCompilerPlanOptions.Defaults, LCancel);
  except
    on ENyxPascalAnalysisCancelled do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and (LPlan = nil), 'Cancelled source planning publishes no partial requirements');
  LSource := NewNyxPascalTranslation(LFiles.WithFile(NyxPascalPhysicalFile('unit.pp'),
    'unit Cycle; {$macro on} {$define LOOP:=LOOP} interface {$if LOOP} {$endif} implementation end.'),
    NyxPascalPhysicalFile('unit.pp'), LContext, TNyxPascalTranslationOptions.Defaults);
  LPlan := PlanNyxPascalCompilerRequirements(LSource, TNyxPascalCompilerPlanOptions.Defaults);
  LProbe := NewNyxFPCCompilerProbe(NyxPascalDefine('CYCLE'), ppmObjFPC);
  Check((LPlan.Count = 1) and not LPlan.Needs(0).SyntaxSupported and
    (LPlan.CompilerProbe(LProbe).Source = LProbe.Source),
    'Failed macro expansion is never reinterpreted as an external compiler name');
  LFailed := False;
  try
    LPlan.State(1);
  except
    on ENyxPascalContext do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed, 'Condition ordinals remain checked and snapshot-local');
  LSource := NewNyxPascalTranslation(LFiles.WithFile(NyxPascalPhysicalFile('unit.pp'),
    'unit Malformed; interface {$if true,} {$endif} implementation end.'),
    NyxPascalPhysicalFile('unit.pp'), LContext, TNyxPascalTranslationOptions.Defaults);
  Supports(LSource, INyxPascalUnresolvedConditions, LInputs);
  Check((LInputs.UnresolvedCount = 1) and not LInputs.UnresolvedAt(0).Plannable,
    'Malformed syntax is unplannable at the retained source boundary itself');
end;

{ Lookahead proposes compiler measurements from existing directive rows. It
  never changes source participation, manufactures type sizes or admits syntax. }
procedure ConditionalLookahead;
const
  CSource: TNyxText = 'unit Ahead; interface {$if defined(NEEDED)}{$endif}' +
    '{$NYX_CAPTURE_BOUNDARY ON}{$if defined(FUTURE_NAME)}{$endif}' +
    '{$if sizeof(TLocal)=4}{$endif}{$if declared(TSome)}{$endif}{$ifopt R+}{$endif}' +
    'const S=''{$if defined(STRING_FAKE)}'';' + #10 +
    '// {$if defined(COMMENT_FAKE)}' + #10 + 'implementation end.';
var
  LContext: INyxPascalCompilerContext;
  LFiles: INyxPascalFileCatalog;
  LSource: INyxPascalTranslation;
  LPlan: INyxPascalCompilerPlan;
  LCandidates: INyxPascalCompilerCandidates;
  LProbe: INyxPascalCompilerProbe;
  LBase: INyxPascalCompilerProbe;
  LSizes: INyxPascalSizeProbe;
  LCancel: INyxCancellationScope;
  LFailed: Boolean;
  LAt: Integer;
  LMany: TNyxText;
  LBefore: Integer;
begin
  LContext := NewNyxPascalCompilerContext(ppcFPC,
    TNyxPascalCompilerVersion.Create(3, 3, 1), ppmObjFPC);
  LFiles := NewNyxPascalFileCatalog(pfcExact).WithFile(NyxPascalPhysicalFile('ahead.pas'), CSource);
  LSource := NewNyxPascalTranslation(LFiles, NyxPascalPhysicalFile('ahead.pas'),
    LContext, TNyxPascalTranslationOptions.Defaults);
  LPlan := PlanNyxPascalCompilerRequirements(LSource, TNyxPascalCompilerPlanOptions.Defaults);
  Supports(LPlan, INyxPascalCompilerCandidates, LCandidates);
  Check((LPlan.Count = 1) and (LCandidates.CandidateCount = 0) and
    (LCandidates.WorkUnits = LSource.Count), 'General plans keep minimal observed requirements by default');
  LPlan := PlanNyxPascalCompilerRequirements(LSource,
    TNyxPascalCompilerPlanOptions.Defaults.LookAheadConditions(True));
  Supports(LPlan, INyxPascalCompilerCandidates, LCandidates);
  Check((LPlan.Count = 1) and (LCandidates.CandidateCount = 1) and not LCandidates.CandidatesTruncated,
    'Lookahead keeps proposals separate from actual unresolved requirements');
  Check((LCandidates.CandidateAt(0).Name = TNyxText('future_name')) and
    (LCandidates.WorkUnits > LSource.Count),
    'Only the retained future directive proposes a missing compiler symbol');
  LBase := NyxFPCProbeSizeOf(NewNyxFPCCompilerProbe(NyxPascalDefine('AHEAD'), ppmObjFPC),
    NyxPascalSizeReference('System.Char'));
  LBefore := LBase.Count;
  LProbe := LPlan.CompilerProbe(LBase);
  Supports(LProbe, INyxPascalSizeProbe, LSizes);
  Check((LProbe.Count = LBefore + 2) and (LProbe.Symbol(LBefore).Name = TNyxText('needed')) and
    (LProbe.Symbol(LBefore + 1).Name = TNyxText('future_name')) and (LSizes.SizeCount = 1),
    'Observed expanded needs precede optional candidates while preserving explicit size requests');
  Check(not LSource.Resolved and
    (LSource.InitialContext.Symbol(NyxPascalDefine('FUTURE_NAME')).Kind = ppdUnknown),
    'Unmeasured candidates neither become facts nor resolve an unknown source effect');
  Check(LSource.Files.TextAt(0) = CSource, 'Lookahead retains the entire original source packet');
  LBase := NewNyxFPCCompilerProbe(NyxPascalDefine('QUOTA'), ppmObjFPC);
  for LAt := LBase.Count to 254 do
  begin
    LBase := LBase.WithSymbol(NyxPascalDefine('BASE_' + TNyxText(IntToStr(LAt))));
  end;
  LProbe := LPlan.CompilerProbe(LBase);
  Check((LProbe.Count = 256) and (LProbe.Symbol(255).Name = TNyxText('needed')),
    'Required queries retain priority at the existing 256-symbol ceiling');
  LFailed := False;
  try
    LCandidates.CandidateAt(1);
  except
    on ENyxPascalContext do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed, 'Candidate getters retain checked snapshot-local bounds');
  LPlan := nil;
  Check(LCandidates.CandidateAt(0).Name = TNyxText('future_name'),
    'The detached candidate capability owns its immutable plan after another handle retires');
  LFailed := False;
  try
    LPlan := PlanNyxPascalCompilerRequirements(LSource,
      TNyxPascalCompilerPlanOptions.Defaults.LookAheadConditions(True).UpToTokens(LSource.Count));
  except
    on ENyxPascalTranslationLimit do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and (LPlan = nil), 'Lookahead work exhaustion publishes no partial plan');
  LCancel := NewNyxCancellationScope;
  LCancel.Cancel;
  LFailed := False;
  try
    LPlan := PlanNyxPascalCompilerRequirements(LSource,
      TNyxPascalCompilerPlanOptions.Defaults.LookAheadConditions(True), LCancel);
  except
    on ENyxPascalAnalysisCancelled do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and (LPlan = nil), 'Cancelled lookahead publishes no partial candidate owner');
  LSource := NewNyxPascalTranslation(LFiles, NyxPascalPhysicalFile('ahead.pas'),
    LContext.WithSymbol(NyxPascalDefine('FUTURE_NAME'), ppdUndefined),
    TNyxPascalTranslationOptions.Defaults);
  LPlan := PlanNyxPascalCompilerRequirements(LSource,
    TNyxPascalCompilerPlanOptions.Defaults.LookAheadConditions(True));
  Supports(LPlan, INyxPascalCompilerCandidates, LCandidates);
  Check(LCandidates.CandidateCount = 0,
    'A known pristine absence is not queried again after a source-context uncertainty');
  LSource := NewNyxPascalTranslation(LFiles.WithFile(NyxPascalPhysicalFile('ahead.pas'),
    'unit Scoped; interface {$ifdef CONTROL}{$define KNOWN}{$endif}' +
    '{$ifdef KNOWN}const V=1;{$endif} implementation end.'), NyxPascalPhysicalFile('ahead.pas'),
    LContext.WithSymbol(NyxPascalDefine('KNOWN'), ppdUndefined),
    TNyxPascalTranslationOptions.Defaults);
  LPlan := PlanNyxPascalCompilerRequirements(LSource, TNyxPascalCompilerPlanOptions.Defaults);
  LBase := NewNyxFPCCompilerProbe(NyxPascalDefine('SCOPED'), ppmObjFPC);
  LProbe := LPlan.CompilerProbe(LBase);
  Check((LPlan.Count = 2) and (LPlan.Needs(1).SymbolCount = 1) and
    (LProbe.Count = LBase.Count + 1) and (LProbe.Symbol(LBase.Count).Name = TNyxText('control')),
    'Source-local uncertainty about a known pristine name is not another compiler query');
  LSource := NewNyxPascalTranslation(LFiles.WithFile(NyxPascalPhysicalFile('ahead.pas'),
    'unit Cycle; {$macro on}{$define LOOP:=LOOP} interface {$if LOOP}{$endif} implementation end.'),
    NyxPascalPhysicalFile('ahead.pas'), LContext, TNyxPascalTranslationOptions.Defaults);
  LPlan := PlanNyxPascalCompilerRequirements(LSource,
    TNyxPascalCompilerPlanOptions.Defaults.LookAheadConditions(True));
  Supports(LPlan, INyxPascalCompilerCandidates, LCandidates);
  Check((LCandidates.CandidateCount = 0) and not LPlan.Needs(0).SyntaxSupported,
    'A retained failed macro site is never recast as a raw external candidate');
  LMany := 'unit Many; interface {$NYX_CAPTURE_BOUNDARY ON}';
  for LAt := 0 to 256 do
  begin
    LMany := LMany + '{$ifdef FUTURE_' + TNyxText(IntToStr(LAt)) + '}{$endif}';
  end;
  LMany := LMany + ' implementation end.';
  LSource := NewNyxPascalTranslation(LFiles.WithFile(NyxPascalPhysicalFile('ahead.pas'), LMany),
    NyxPascalPhysicalFile('ahead.pas'), LContext, TNyxPascalTranslationOptions.Defaults);
  LPlan := PlanNyxPascalCompilerRequirements(LSource,
    TNyxPascalCompilerPlanOptions.Defaults.LookAheadConditions(True));
  Supports(LPlan, INyxPascalCompilerCandidates, LCandidates);
  Check((LCandidates.CandidateCount = 256) and LCandidates.CandidatesTruncated,
    'Extra proposals stop explicitly at their existing compiler-query ceiling');
  Check(LPlan.CompilerProbe(NewNyxFPCCompilerProbe(NyxPascalDefine('MANY'), ppmObjFPC)).Count = 256,
    'Truncated lookahead never exceeds the compiler probe name quota');
end;

{ Required rows use the exact translated compiler context; speculative raw
  includes have no such owner and cannot inherit the analyzer's own compiler.
  A sizeof-looking IFDEF is therefore a macro query only when qualified. }
procedure ConditionalOperandPlanning;
var
  LFiles: INyxPascalFileCatalog;
  LContext: INyxPascalCompilerContext;
  LSource: INyxPascalTranslation;
  LPlan, LSaved: INyxPascalCompilerPlan;
  LProbe, LBase: INyxPascalCompilerProbe;
  LSizes: INyxPascalSizeProbe;
  LCandidates: INyxPascalCompilerCandidates;
  LIncludes: INyxPascalCompilerIncludeCandidates;
  LCancel: INyxCancellationScope;
  LFailed: Boolean;
begin
  LContext := NewNyxPascalCompilerContext(ppcFPC,
    TNyxPascalCompilerVersion.Create(3, 3, 1), ppmDelphi);
  LFiles := NewNyxPascalFileCatalog(pfcExact).WithFile(NyxPascalPhysicalFile('operand.pas'),
    'unit Operand; interface {$ifdef sizeof(NativeInt)>=8}const Maybe=1;{$endif} implementation end.');
  LSource := NewNyxPascalTranslation(LFiles, NyxPascalPhysicalFile('operand.pas'),
    LContext, TNyxPascalTranslationOptions.Defaults);
  LPlan := PlanNyxPascalCompilerRequirements(LSource, TNyxPascalCompilerPlanOptions.Defaults);
  Check((LPlan.Count = 1) and LPlan.Needs(0).SyntaxSupported and
    (LPlan.Needs(0).SymbolCount = 1) and
    (LPlan.Needs(0).Symbol(0).Name = TNyxText('sizeof')) and
    (LPlan.Needs(0).SizeCount = 0),
    'The authored IFDEF needs only its actual first macro operand, never a guessed NativeInt size');
  LBase := NewNyxFPCCompilerProbe(NyxPascalDefine('OPERAND'), ppmDelphi);
  LProbe := LPlan.CompilerProbe(LBase);
  Check(Supports(LProbe, INyxPascalSizeProbe, LSizes) and
    (LProbe.Count = LBase.Count + 1) and
    (LProbe.Symbol(LBase.Count).Name = TNyxText('sizeof')) and (LSizes.SizeCount = 0),
    'A qualified trailing annotation adds one symbol request and no size operand');
  LFiles := LFiles.WithFile(NyxPascalPhysicalFile('operand.pas'),
    'unit Operand; interface {$if defined(NEEDED)}{$endif}' +
    '{$NYX_CAPTURE_BOUNDARY ON}{$ifdef FUTURE(note)}const Maybe=1;{$endif} implementation end.');
  LSource := NewNyxPascalTranslation(LFiles, NyxPascalPhysicalFile('operand.pas'),
    LContext, TNyxPascalTranslationOptions.Defaults);
  LPlan := PlanNyxPascalCompilerRequirements(LSource,
    TNyxPascalCompilerPlanOptions.Defaults.LookAheadConditions(True));
  Check(Supports(LPlan, INyxPascalCompilerCandidates, LCandidates) and
    (LPlan.Count = 1) and (LCandidates.CandidateCount = 1) and
    (LCandidates.CandidateAt(0).Name = TNyxText('future')),
    'Context-aware lookahead shares the qualified first-operand rule beyond the retained required prefix');
  LSaved := LPlan;
  LFailed := False;
  try
    PlanNyxPascalCompilerRequirements(LSource,
      TNyxPascalCompilerPlanOptions.Defaults.LookAheadConditions(True).UpToTokens(LSource.Count));
  except
    on ENyxPascalTranslationLimit do LFailed := True;
  end;
  Check(LFailed and (LSaved.Count = 1),
    'Trailing-operand lookahead still charges existing work before publication and preserves the prior plan');
  LCancel := NewNyxCancellationScope;
  LCancel.Cancel;
  LFailed := False;
  try
    PlanNyxPascalCompilerRequirements(LSource,
      TNyxPascalCompilerPlanOptions.Defaults.LookAheadConditions(True), LCancel);
  except
    on ENyxPascalAnalysisCancelled do LFailed := True;
  end;
  Check(LFailed and (LSaved.Count = 1),
    'Cancelled qualified-operand planning retains the same atomic refusal contract');
  LFiles := NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('raw.pas'), 'unit Raw; interface {$i raw.inc} implementation end.')
    .WithFile(NyxPascalPhysicalFile('raw.inc'), '{$ifdef FUTURE(note)}{$endif}');
  LIncludes := PlanNyxPascalCompilerIncludeCandidates(LFiles,
    NyxPascalPhysicalFile('raw.pas'), TNyxPascalLexicalOptions.Defaults,
    TNyxPascalCompilerPlanOptions.Defaults);
  Check((LIncludes.FilesVisited = 2) and (LIncludes.Count = 0),
    'Captured raw include candidates without compiler context retain strict whole-operand syntax');
  LContext := NewNyxPascalCompilerContext(ppcFPC,
    TNyxPascalCompilerVersion.Create(3, 2, 2), ppmDelphi);
  LSource := NewNyxPascalTranslation(LFiles, NyxPascalPhysicalFile('raw.pas'),
    LContext, TNyxPascalTranslationOptions.Defaults);
  LPlan := PlanNyxPascalCompilerRequirements(LSource,
    TNyxPascalCompilerPlanOptions.Defaults.LookAheadConditions(True));
  Check(Supports(LPlan, INyxPascalCompilerCandidates, LCandidates) and
    (LPlan.Count = 1) and not LPlan.Needs(0).SyntaxSupported and
    (LCandidates.CandidateCount = 0) and (LPlan.CompilerProbe(LBase).Source = LBase.Source),
    'An unqualified compiler version cannot reinterpret a trailing operand as a pristine candidate');
end;

begin
  try
    OrderedRequirements;
    ConditionalLookahead;
    ConditionalOperandPlanning;
    WriteLn('PASS ', GChecks, ' compiler requirement planning checks');
  except
    on LError: Exception do
    begin
      WriteLn('FAIL / ', LError.Message);
      {$ifndef PAS2JS}DumpExceptionBackTrace(StdErr);{$endif}
      ExitCode := 1;
    end;
  end;
end.
