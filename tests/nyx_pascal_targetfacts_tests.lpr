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
program nyx_pascal_targetfacts_tests;
{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.pascal.context, nyx.pascal.compilertrace;

type
  { Adversarial alternate request. Oversized optional counts must be checked
    once before any element callback; changed source must never gain host trust. }
  TAlternateProbe = class(TInterfacedObject, INyxPascalCompilerProbe, INyxPascalSizeProbe)
  public
    Base: INyxPascalCompilerProbe;
    Excess: Boolean;
    ChangedSource: Boolean;
    CountCalls: Integer;
    ElementCalls: Integer;
    function Identity: TNyxPascalDefineRef;
    function Mode: TNyxPascalLanguageMode;
    function Count: Integer;
    function Symbol(AIndex: Integer): TNyxPascalDefineRef;
    function WithSymbol(const AName: TNyxPascalDefineRef): INyxPascalCompilerProbe;
    function Source: TNyxText;
    function SizeCount: Integer;
    function SizeName(AIndex: Integer): TNyxPascalSizeReference;
    function WithSizeOf(const AName: TNyxPascalSizeReference): INyxPascalCompilerProbe;
  end;

var
  GChecks: Integer;

function TAlternateProbe.Identity: TNyxPascalDefineRef;
begin
  Result := Base.Identity;
end;

function TAlternateProbe.Mode: TNyxPascalLanguageMode;
begin
  Result := Base.Mode;
end;

function TAlternateProbe.Count: Integer;
begin
  Result := Base.Count;
end;

function TAlternateProbe.Symbol(AIndex: Integer): TNyxPascalDefineRef;
begin
  Result := Base.Symbol(AIndex);
end;

function TAlternateProbe.WithSymbol(const AName: TNyxPascalDefineRef): INyxPascalCompilerProbe;
begin
  Result := Base.WithSymbol(AName);
end;

function TAlternateProbe.Source: TNyxText;
begin
  Result := Base.Source;

  if ChangedSource then
  begin
    Result := Result + 'changed';
  end;
end;

function TAlternateProbe.SizeCount: Integer;
begin
  Inc(CountCalls);
  Result := 0;

  if Excess then
  begin
    Result := 65;
  end;
end;

function TAlternateProbe.SizeName(AIndex: Integer): TNyxPascalSizeReference;
begin
  Inc(ElementCalls);
  Result := NyxPascalSizeReference('Integer');
end;

function TAlternateProbe.WithSizeOf(const AName: TNyxPascalSizeReference): INyxPascalCompilerProbe;
begin
  Result := NyxFPCProbeSizeOf(Base, AName);
end;

procedure Check(AValue: Boolean; const AReason: String);
begin
  Inc(GChecks);

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
end;

function Context: INyxPascalCompilerContext;
begin
  Result := NewNyxPascalCompilerContext(ppcFPC,
    TNyxPascalCompilerVersion.Create(3, 3, 1), ppmDelphi);
end;

procedure RefuseName(const AName: TNyxText);
var
  LRejected: Boolean;
begin
  LRejected := False;
  try
    NyxPascalSizeReference(AName);
  except
    on ENyxPascalContext do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Only checked simple named size operands may reach a compiler probe');
end;

procedure Values;
var
  LBase: INyxPascalCompilerContext;
  LMeasured: INyxPascalCompilerContext;
  LOther: INyxPascalCompilerContext;
  LSizes: INyxPascalContextSizes;
  LFact: TNyxPascalSizeFact;
  LRejected: Boolean;
  LAt: Integer;
begin
  Check(NyxPascalSizeReference('System.INTEGER').Name = TNyxText('system.integer'),
    'Names normalize ASCII case while preserving qualification');
  RefuseName('');
  RefuseName('Integer()');
  RefuseName('Table[0]');
  RefuseName('TList<Integer>');
  RefuseName('a..b');
  RefuseName('a.');
  RefuseName('9Type');
  RefuseName('type');
  RefuseName('Integer)}{$fatal injected');
  LBase := Context.WithSymbol(NyxPascalDefine('APP'), ppdDefined);
  LMeasured := NyxPascalWithCapturedSize(LBase, NyxPascalSizeReference('Integer'),
    NyxPascalSizeFact(pssKnown, ppmDelphi, ppkTrue, 4));
  LMeasured := NyxPascalWithCapturedSize(LMeasured, NyxPascalSizeReference('System.Integer'),
    NyxPascalSizeFact(pssKnown, ppmDelphi, ppkTrue, 2));
  LMeasured := NyxPascalWithCapturedSize(LMeasured, NyxPascalSizeReference('TEmpty'),
    NyxPascalSizeFact(pssKnown, ppmDelphi, ppkTrue, 0));
  Check(NyxPascalCapturedSize(LBase, NyxPascalSizeReference('Integer')).State = pssUnknown,
    'A persistent measured context does not mutate its original');
  Check((NyxPascalCapturedSize(LMeasured, NyxPascalSizeReference('Integer')).Bytes = 4) and
    (NyxPascalCapturedSize(LMeasured, NyxPascalSizeReference('System.Integer')).Bytes = 2) and
    (NyxPascalCapturedSize(LMeasured, NyxPascalSizeReference('TEmpty')).Bytes = 0),
    'Mode aliases, qualified exports and a known zero remain independent facts');
  LOther := CaptureNyxPascalCompilerContext(LMeasured);
  Check(LMeasured.SameFacts(LOther) and not LMeasured.SameFacts(LBase),
    'Capture and equality include the complete ordered size vector');
  LOther := NyxPascalWithCapturedSize(LOther, NyxPascalSizeReference('Integer'),
    NyxPascalSizeFact(pssKnown, ppmDelphi, ppkTrue, 1));
  Check(not LMeasured.SameFacts(LOther) and
    (NyxPascalCapturedSize(LMeasured, NyxPascalSizeReference('Integer')).Bytes = 4),
    'Fact replacement owns a new vector and changes identity');
  Check((NyxPascalCapturedSize(LMeasured.InMode(ppmObjFPC),
    NyxPascalSizeReference('Integer')).State = pssUnknown) and
    (NyxPascalCapturedSize(LMeasured.UnknownState,
    NyxPascalSizeReference('Integer')).State = pssUnknown) and
    (NyxPascalCapturedSize(LMeasured.InMode(ppmDelphi),
    NyxPascalSizeReference('Integer')).Bytes = 4),
    'Unknown effects and changed modes revoke prior measurements; unchanged mode retains provenance');
  Check(AnalyzeNyxPascalCondition(LMeasured, 'sizeof(Integer)=4').Truth = ppkUnknown,
    'Pristine measurements do not silently authorize authored-scope sizeof evaluation');
  LRejected := False;
  try
    LFact := NyxPascalSizeFact(pssUnknown);
    LAt := LFact.Bytes;
  except
    on ENyxPascalContext do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Unknown size has no usable numeric zero');
  LRejected := False;
  try
    NyxPascalSizeFact(pssUndeclared, ppmUnknown, ppkTrue);
  except
    on ENyxPascalContext do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'A measured absence still requires its compiler scope');
  LOther := LBase;
  for LAt := 0 to 63 do
  begin
    LOther := NyxPascalWithCapturedSize(LOther,
      NyxPascalSizeReference('TValue' + TNyxText(IntToStr(LAt))),
      NyxPascalSizeFact(pssKnown, ppmDelphi, ppkTrue, LAt));
  end;
  LRejected := False;
  try
    NyxPascalWithCapturedSize(LOther, NyxPascalSizeReference('Overflow'),
      NyxPascalSizeFact(pssKnown, ppmDelphi, ppkTrue, 1));
  except
    on ENyxPascalContext do
    begin
      LRejected := True;
    end;
  end;
  Supports(LOther, INyxPascalContextSizes, LSizes);
  Check(LRejected and (LSizes.SizeCount = 64), 'A size budget refusal retains the complete previous context');
end;

procedure Planning;
var
  LContext: INyxPascalCompilerContext;
  LNeeds: INyxPascalConditionNeeds;
  LAt: Integer;
  LText: TNyxText;
  LRejected: Boolean;
begin
  LContext := Context.WithSymbol(NyxPascalDefine('KNOWN'), ppdInteger, 3);
  LNeeds := PlanNyxPascalCondition(LContext,
    'defined(Future) and (MISSING >= 4) and defined(FUTURE) and (KNOWN=3)');
  Check(LNeeds.SyntaxSupported and (LNeeds.SymbolCount = 2) and
    (LNeeds.Symbol(0).Name = TNyxText('future')) and
    (LNeeds.Symbol(1).Name = TNyxText('missing')) and (LNeeds.SizeCount = 0),
    'A condition requests only missing compiler facts in stable deduplicated order');
  LNeeds := PlanNyxPascalCondition(LContext,
    '(sizeof(Integer)=4) and (sizeof(System.Integer)=2) and (sizeof(INTEGER)>0)');
  Check(LNeeds.SyntaxSupported and (LNeeds.SymbolCount = 0) and (LNeeds.SizeCount = 2) and
    (LNeeds.SizeName(0).Name = TNyxText('integer')) and
    (LNeeds.SizeName(1).Name = TNyxText('system.integer')),
    'Named size requirements never become compiler defines or lose their qualification');
  LNeeds := PlanNyxPascalCondition(LContext, 'defined(Future) and declared(TThing)');
  Check(not LNeeds.SyntaxSupported and (LNeeds.SymbolCount = 0) and (LNeeds.SizeCount = 0),
    'An unsupported function cannot publish a speculative partial request');
  LNeeds := PlanNyxPascalCondition(LContext, 'sizeof(Thing[0])=4');
  Check(not LNeeds.SyntaxSupported and (LNeeds.SizeCount = 0),
    'Complex sizeof operands remain outside this named planning contract');
  LText := '';
  for LAt := 0 to 64 do
  begin

    if LText <> '' then
    begin
      LText := LText + 'or ';
    end;
    LText := LText + 'sizeof(T' + TNyxText(IntToStr(LAt)) + ')';
  end;
  LRejected := False;
  LNeeds := nil;
  try
    LNeeds := PlanNyxPascalCondition(LContext, LText);
  except
    on ENyxPascalContext do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LNeeds = nil), 'Planning quotas refuse without an observable partial plan');
end;

function Note(const AValue: TNyxText): TNyxText;
begin
  Result := 'probe.pas(4,2) Note: (2025) User defined: NYX_CONTEXT_sizes_' + AValue + #10;
end;

function Trace(const ASizeRows: TNyxText): TNyxText;
begin
  Result := '(3101) Macro defined: FPC'#10 +
    '(3103) Macro FPC_VERSION set to 3'#10 + '(3103) Macro FPC_RELEASE set to 3'#10 +
    '(3103) Macro FPC_PATCH set to 1'#10 + '(3103) Macro FPC_FULLVERSION set to 30301'#10 +
    '(3104) Compiling probe.pas'#10 + Note('BEGIN') + Note('D_0') + Note('D_1') +
    Note('U_2') + Note('U_3') + Note('U_4') + Note('U_5') + Note('U_6') + Note('U_7') +
    ASizeRows + Note('H_TRUE') + Note('END') + '(1008) 80 lines compiled, 0.1 sec'#10;
end;

function Bits(AIndex, ABytes: Integer): TNyxText;
var
  LBit: Integer;
  LParts: TNyxStrings;
begin
  LParts := TNyxStrings.Create;
  try
    LParts.Add(Note('S_' + TNyxText(IntToStr(AIndex)) + '_K'));
    for LBit := 0 to 30 do
    begin
      LParts.Add(Note('S_' + TNyxText(IntToStr(AIndex)) + '_B_' +
        TNyxText(IntToStr(LBit)) + '_' + TNyxText(IntToStr((ABytes shr LBit) and 1))));
    end;
    Result := LParts.Join;
  finally
    LParts.Free;
  end;
end;

procedure RefuseTrace(const AProbe: INyxPascalCompilerProbe; const AText: TNyxText);
var
  LRejected: Boolean;
  LResult: INyxPascalCompilerContext;
begin
  LRejected := False;
  try
    LResult := ReadNyxFPCCompilerContext(AProbe, 'probe.pas', AText, 0,
      TNyxPascalCompilerTraceOptions.Defaults);
  except
    on ENyxPascalContext do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LResult = nil), 'Malformed size replies cannot publish a partial compiler context');
end;

procedure Protocol;
var
  LProbe: INyxPascalCompilerProbe;
  LContext: INyxPascalCompilerContext;
  LSizes: INyxPascalSizeProbe;
  LRows: TNyxText;
begin
  LProbe := NewNyxFPCCompilerProbe(NyxPascalDefine('SIZES'));
  LProbe := NyxFPCProbeSizeOf(LProbe, NyxPascalSizeReference('Integer'));
  LProbe := LProbe.WithSymbol(NyxPascalDefine('FPC'));
  LProbe := NyxFPCProbeSizeOf(LProbe, NyxPascalSizeReference('TAbsent'));
  LProbe := NyxFPCProbeSizeOf(LProbe, NyxPascalSizeReference('THuge'));
  LProbe := NyxFPCProbeSizeOf(LProbe, NyxPascalSizeReference('TZero'));
  LProbe := NyxFPCProbeSizeOf(LProbe, NyxPascalSizeReference('INTEGER'));
  LProbe := CaptureNyxFPCCompilerProbe(LProbe);
  Supports(LProbe, INyxPascalSizeProbe, LSizes);
  Check((LProbe.Count = 8) and (LSizes.SizeCount = 4) and
    (Pos('sizeof(integer)', LProbe.Source) > 0),
    'Fluent symbol/size additions and checked capture preserve both independent query vectors');
  LRows := Bits(0, High(Integer)) + Note('S_1_U') + Note('S_2_O') + Bits(3, 0);
  LContext := ReadNyxFPCCompilerContext(LProbe, 'probe.pas', Trace(LRows), 0,
    TNyxPascalCompilerTraceOptions.Defaults);
  Check((NyxPascalCapturedSize(LContext, NyxPascalSizeReference('Integer')).Bytes = High(Integer)) and
    (NyxPascalCapturedSize(LContext, NyxPascalSizeReference('TAbsent')).State = pssUndeclared) and
    (NyxPascalCapturedSize(LContext, NyxPascalSizeReference('THuge')).State = pssOutOfRange) and
    (NyxPascalCapturedSize(LContext, NyxPascalSizeReference('TZero')).Bytes = 0),
    'Independent transcript rows retain signed-range boundary, absent, oversize and known-zero facts');
  RefuseTrace(LProbe, Trace(''));
  RefuseTrace(LProbe, Trace(Note('S_1_U') + LRows));
  RefuseTrace(LProbe, Trace(Note('S_0_K') + LRows));
  RefuseTrace(LProbe, Trace(Note('S_0_K') + Note('S_0_B_0_2')));
  RefuseTrace(LProbe, Trace(Note('S_0_K') + Note('S_0_B_1_0')));
  RefuseTrace(LProbe, Trace(LRows + Note('S_4_U')));
end;

procedure AlternateRequests;
var
  LAlternate: TAlternateProbe;
  LRequest: INyxPascalCompilerProbe;
  LCaptured: INyxPascalCompilerProbe;
  LRejected: Boolean;
begin
  LAlternate := TAlternateProbe.Create;
  LRequest := LAlternate;
  LAlternate.Base := NewNyxFPCCompilerProbe(NyxPascalDefine('SIZES'));
  LAlternate.Excess := True;
  LRejected := False;
  try
    LCaptured := CaptureNyxFPCCompilerProbe(LRequest);
  except
    on ENyxPascalContext do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LCaptured = nil) and (LAlternate.CountCalls = 1) and
    (LAlternate.ElementCalls = 0), 'An oversized alternate query refuses before enumeration or publication');
  LAlternate.Excess := False;
  LAlternate.ChangedSource := True;
  LRejected := False;
  try
    LCaptured := CaptureNyxFPCCompilerProbe(LRequest);
  except
    on ENyxPascalContext do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LCaptured = nil), 'Optional query capture preserves exact checked source admission');
end;

begin
  try
    Values;
    Planning;
    Protocol;
    AlternateRequests;
    WriteLn('PASS ', GChecks, ' target facts and conditional request checks');
  except
    on LError: Exception do
    begin
      WriteLn('FAIL / ', LError.Message);
      ExitCode := 1;
    end;
  end;
end.
