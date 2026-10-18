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

begin
  try
    OrderedRequirements;
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
