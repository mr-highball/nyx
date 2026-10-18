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
program nyx_pascal_compilertrace_tests;
{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.scheduler, nyx.pascal.context,
  nyx.pascal.lexing, nyx.pascal.compilertrace;

type
  { Deliberately excess alternate producer. Its small finite excess prevents a
    failing regression from hanging the fixture; Calls proves the reader refuses
    before consulting any element, rather than walking an unchecked count. }
  TExcessProbe = class(TInterfacedObject, INyxPascalCompilerProbe)
  public
    Base: INyxPascalCompilerProbe;
    Calls: Integer;
    function Identity: TNyxPascalDefineRef;
    function Mode: TNyxPascalLanguageMode;
    function Count: Integer;
    function Symbol(AIndex: Integer): TNyxPascalDefineRef;
    function WithSymbol(const AName: TNyxPascalDefineRef): INyxPascalCompilerProbe;
    function Source: TNyxText;
  end;

const
  CFile: TNyxText = 'probe.pas';
  CPrefix: TNyxText = 'NYX_CONTEXT_qualify_';
  CVersion: TNyxText = '(3101) Macro defined: FPC'#10 +
    '(3101) Macro defined: CPUI386'#10 +
    '(3103) Macro FPC_VERSION set to 3'#10 +
    '(3103) Macro FPC_RELEASE set to 3'#10 +
    '(3103) Macro FPC_PATCH set to 1'#10 +
    '(3103) Macro FPC_FULLVERSION set to 30301'#10;

var
  GChecks: Integer;

function TExcessProbe.Identity: TNyxPascalDefineRef;
begin
  Result := Base.Identity;
end;

function TExcessProbe.Mode: TNyxPascalLanguageMode;
begin
  Result := Base.Mode;
end;

function TExcessProbe.Count: Integer;
begin
  Result := 257;
end;

function TExcessProbe.Symbol(AIndex: Integer): TNyxPascalDefineRef;
begin
  Inc(Calls);
  Result := Base.Symbol(0);
end;

function TExcessProbe.WithSymbol(const AName: TNyxPascalDefineRef): INyxPascalCompilerProbe;
begin
  Result := Base.WithSymbol(AName);
end;

function TExcessProbe.Source: TNyxText;
begin
  Result := Base.Source;
end;

procedure Check(AValue: Boolean; const AReason: String);
begin
  Inc(GChecks);

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
end;

function Message(const AValue: TNyxText): TNyxText;
begin
  Result := CFile + '(4,2) Note: (2025) User defined: ' + CPrefix + AValue + #10;
end;

function Frame(const AExtra: TNyxText = ''): TNyxText;
begin
  { Literal protocol example, independent of the source generator. Actual
    compiler output is exercised by the native profile/SDK consumer. }
  Result := CVersion + AExtra + '(3104) Compiling probe.pas'#10 +
    Message('BEGIN') + Message('D_0') + Message('D_1') + Message('U_2') +
    Message('U_3') + Message('U_4') + Message('U_5') + Message('U_6') +
    Message('U_7') + Message('U_8') + Message('H_TRUE') + Message('END') +
    '(1008) 80 lines compiled, 0.1 sec'#10;
end;

function Probe: INyxPascalCompilerProbe;
begin
  Result := NewNyxFPCCompilerProbe(NyxPascalDefine('QUALIFY'))
    .WithSymbol(NyxPascalDefine('FPC_DOTTEDUNITS'));
end;

procedure Refuses(const AProbe: INyxPascalCompilerProbe; const AText: TNyxText;
  const AReason: String; AExitCode: Integer = 0);
var
  LRejected: Boolean;
begin
  LRejected := False;
  try
    ReadNyxFPCCompilerContext(AProbe, CFile, AText, AExitCode,
      TNyxPascalCompilerTraceOptions.Defaults);
  except
    on ENyxPascalContext do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, AReason);
end;

procedure Protocol;
var
  LProbe: INyxPascalCompilerProbe;
  LBefore: INyxPascalCompilerProbe;
  LContext: INyxPascalCompilerContext;
  LText: TNyxText;
  LRejected: Boolean;
  LAt: Integer;
  LCancellation: INyxCancellationScope;
  LAlternate: INyxPascalCompilerProbe;
  LExcess: TExcessProbe;
begin
  LProbe := Probe;
  LBefore := LProbe;
  LProbe := LProbe.WithSymbol(NyxPascalDefine('EXTRA'));
  Check((LBefore.Count = 9) and (LProbe.Count = 10) and
    (LBefore.WithSymbol(NyxPascalDefine('fpc_dottedunits')).Count = 9),
    'Persistent checked symbol queries retain old source and case identity');
  LProbe := LBefore;
  Check((Pos('{$mode delphi}', LProbe.Source) > 0) and
    (Pos('{$ifdef fpc_dottedunits}', LProbe.Source) > 0),
    'Generated input has its selected source mode and explicit requested symbol');
  LContext := ReadNyxFPCCompilerContext(LProbe, CFile, Frame(
    '(3103) Macro LOW_VALUE set to -2147483648'#10 +
    '(3103) Macro HIGH_VALUE set to 2147483647'#10 +
    '(3103) Macro TOO_LARGE set to 2147483648'#10 +
    '(3103) Macro REPLACEMENT set to other.member'#10 +
    '(3101) Macro defined: RETIRED'#10'(3102) Macro undefined: RETIRED'#10),
    0, TNyxPascalCompilerTraceOptions.Defaults);
  Check((LContext.Version.FullNumber = 30301) and (LContext.Mode = ppmDelphi) and
    (LContext.Family = ppcFPC) and (LContext.Option('H') = ppkTrue),
    'Actual transcript version and option observations become detached typed facts');
  Check(not LContext.DefinesComplete and
    (LContext.Symbol(NyxPascalDefine('FPC_DOTTEDUNITS')).IsDefined = ppkFalse) and
    (LContext.Symbol(NyxPascalDefine('UNQUERIED')).IsDefined = ppkUnknown) and
    (LContext.Symbol(NyxPascalDefine('CPUI386')).IsDefined = ppkTrue),
    'Observed positives and queried absences never seal an unobserved define set');
  Check((LContext.Symbol(NyxPascalDefine('LOW_VALUE')).IntegerValue = Low(Integer)) and
    (LContext.Symbol(NyxPascalDefine('HIGH_VALUE')).IntegerValue = High(Integer)) and
    (LContext.Symbol(NyxPascalDefine('TOO_LARGE')).Kind = ppdDefined) and
    (LContext.Symbol(NyxPascalDefine('REPLACEMENT')).Kind = ppdDefined) and
    (LContext.Symbol(NyxPascalDefine('RETIRED')).IsDefined = ppkFalse),
    'Signed values are exact; oversized/replacement values never masquerade as integers');
  LText := Frame;
  Refuses(LProbe, LText, 'A failed compiler cannot publish its otherwise complete output', 1);
  Refuses(LProbe, StringReplace(LText, String(Message('END')), '', []),
    'Missing source end refuses');
  Refuses(LProbe, LText + Message('END'), 'A repeated end refuses');
  Refuses(LProbe, StringReplace(LText, String(Message('D_1')), String(Message('D_0')), []),
    'Out-of-order/duplicate name rows refuse');
  Refuses(LProbe, StringReplace(LText, 'Note: (2025)', 'Warning: (2025)', []),
    'Another diagnostic family cannot stand in for a note reply');
  Refuses(LProbe, StringReplace(LText, 'probe.pas(4,2)', 'other.pas(4,2)', []),
    'Another source owner cannot provide a query row');
  Refuses(LProbe, StringReplace(LText, '(1008) 80 lines compiled, 0.1 sec', '', []),
    'A truncated transcript after its query end still refuses');
  Refuses(LProbe, StringReplace(LText, 'FPC_PATCH set to 1', 'FPC_PATCH set to 2', []),
    'Inconsistent numeric version facts refuse');
  Refuses(LProbe, StringReplace(LText, String(Message('H_TRUE')), String(Message('H_FALSE') +
    '(3101) Macro defined: LATE'#10), []), 'Mutations inside the fixed query block refuse');
  Refuses(LProbe, LText + 'probe.pas(8,2) Error: (1000) Failed'#10,
    'An error diagnostic refuses even when a supplied exit code was incorrectly zero');
  Refuses(LProbe, '(3104) Compiling dependency.pas'#10 + LText,
    'A dependency source rebuild is outside this compiler capture contract');
  Refuses(LProbe, StringReplace(LText, String(Message('D_1')), String(Message('U_1')), []),
    'Observed source-mode identity must match the requested context');
  Refuses(NewNyxFPCCompilerProbe(NyxPascalDefine('OTHER')), LText,
    'A transcript cannot be silently attached to another query identity');
  LRejected := False;
  try
    ReadNyxFPCCompilerContext(LProbe, CFile, LText, 0,
      TNyxPascalCompilerTraceOptions.Defaults.UpToBytes(12));
  except
    on ENyxPascalContext do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Byte limits refuse the whole context');
  LRejected := False;
  try
    ReadNyxFPCCompilerContext(LProbe, CFile, LText, 0,
      TNyxPascalCompilerTraceOptions.Defaults.UpToLines(2));
  except
    on ENyxPascalContext do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Line work limits refuse the whole context');
  LRejected := False;
  try
    ReadNyxFPCCompilerContext(LProbe, CFile, LText, 0,
      TNyxPascalCompilerTraceOptions.Defaults.UpToLineLength(12));
  except
    on ENyxPascalContext do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Per-line budgets refuse the whole context');
  LCancellation := NewNyxCancellationScope;
  LCancellation.Cancel;
  LRejected := False;
  try
    ReadNyxFPCCompilerContext(LProbe, CFile, LText, 0,
      TNyxPascalCompilerTraceOptions.Defaults, LCancellation);
  except
    on ENyxPascalAnalysisCancelled do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Cancellation publishes no partially captured compiler facts');
  LExcess := TExcessProbe.Create;
  LAlternate := LExcess;
  LExcess.Base := LBefore;
  LRejected := False;
  try
    ReadNyxFPCCompilerContext(LAlternate, CFile, LText, 0,
      TNyxPascalCompilerTraceOptions.Defaults);
  except
    on ENyxPascalContext do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LExcess.Calls = 0),
    'An excess alternate query count refuses before any element is enumerated');
  for LAt := LProbe.Count to 255 do
  begin
    LProbe := LProbe.WithSymbol(NyxPascalDefine('NAME_' + TNyxText(IntToStr(LAt))));
  end;
  LRejected := False;
  try
    LProbe.WithSymbol(NyxPascalDefine('EXCESS'));
  except
    on ENyxPascalContext do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LProbe.Count = 256) and (LBefore.Count = 9),
    'Excess query vectors refuse without changing retained requests');
end;

begin
  try
    Protocol;
    WriteLn('PASS ', GChecks, ' portable compiler fact protocol checks');
  except
    on LError: Exception do
    begin
      WriteLn('FAIL / ', LError.Message);
      {$ifndef PAS2JS}DumpExceptionBackTrace(StdErr);{$endif}
      ExitCode := 1;
    end;
  end;
end.
