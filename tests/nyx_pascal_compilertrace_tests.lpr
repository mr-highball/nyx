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
  nyx.pascal.lexing, nyx.pascal.compilertrace, nyx.pascal.work
  {$ifndef PAS2JS}, Classes, nyx.bytes{$endif};

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

  { Foreign capture producer for conflict and detachment checks. It owns its
    supplied snapshots only; merging must copy them and must not retain this
    wrapper or treat metadata as an application-execution receipt. }
  TCaptureOverride = class(TInterfacedObject, INyxFPCCompilerCapture)
  public
    Base: INyxFPCCompilerCapture;
    Value: INyxPascalCompilerContext;
    Defaults: INyxPascalCompilerContext;
    function Context: INyxPascalCompilerContext;
    function ObservedDefaults: INyxPascalCompilerContext;
    function Merge(const ANext: INyxFPCCompilerCapture): INyxFPCCompilerCapture;
  end;

  { Operation-local hostile policy. It can cancel after a real admission,
    re-enter restoration under the same parent budget, or release the caller's
    last text/scope slots. Results must retain none of this mutable execution. }
  TRestorePolicy = class(TInterfacedObject, INyxCancellationScope, INyxPascalAnalysisBudget)
  public
    Budget: INyxPascalAnalysisBudget;
    NestedSource: TNyxText;
    NestedResult: INyxPascalCompilerProbe;
    Calls, CancelAt, ReenterAt, AfterCancellation: Integer;
    Cancelled, Entered, ReleaseFromCancelled, Released: Boolean;
    destructor Destroy; override;
    procedure Cancel;
    function GetCancelled: Boolean;
    procedure Admit(APhase: TNyxPascalAnalysisPhase; AUnits: Integer);
    function Receipt: TNyxPascalAnalysisReceipt;
    procedure Retire;
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
  GRestoreSource: TNyxText;
  GRestoreOptions: TNyxPascalCompilerTraceOptions;
  GRestoreScope: INyxCancellationScope;
  GRestorePoliciesDestroyed: Integer;

destructor TRestorePolicy.Destroy;
begin
  Inc(GRestorePoliciesDestroyed);
  inherited Destroy;
end;

procedure TRestorePolicy.Cancel;
begin
  Cancelled := True;
end;

function TRestorePolicy.GetCancelled: Boolean;
begin

  if ReleaseFromCancelled and not Released then
  begin
    Released := True;
    GRestoreSource := 'changed caller input';
    GRestoreOptions := TNyxPascalCompilerTraceOptions.Defaults.UpToBytes(1);
    GRestoreScope := nil;
  end;
  Result := Cancelled or ((Budget <> nil) and Budget.Cancelled);
end;

procedure TRestorePolicy.Admit(APhase: TNyxPascalAnalysisPhase; AUnits: Integer);
begin

  if Cancelled then Inc(AfterCancellation);
  Inc(Calls);

  if (Calls = ReenterAt) and not Entered then
  begin
    Entered := True;
    NestedResult := RestoreNyxFPCCompilerProbe(NestedSource,
      TNyxPascalCompilerTraceOptions.Defaults, Budget);
  end;

  if Budget <> nil then Budget.Admit(APhase, AUnits);

  if Calls = CancelAt then Cancelled := True;
end;

function TRestorePolicy.Receipt: TNyxPascalAnalysisReceipt;
begin
  Result := Budget.Receipt;
end;

procedure TRestorePolicy.Retire;
begin
  Cancelled := True;

  if Budget <> nil then Budget.Retire;
end;

function TExcessProbe.Identity: TNyxPascalDefineRef;
begin
  Result := Base.Identity;
end;

function TCaptureOverride.Context: INyxPascalCompilerContext;
begin
  Result := Value;
end;

function TCaptureOverride.ObservedDefaults: INyxPascalCompilerContext;
begin

  if Defaults <> nil then Exit(Defaults);
  Result := Base.ObservedDefaults;
end;

function TCaptureOverride.Merge(const ANext: INyxFPCCompilerCapture): INyxFPCCompilerCapture;
begin
  Result := nil;
  raise ENyxPascalContext.Create('This qualification wrapper supplies capture only');
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

{ Independent protocol packets qualify value/cohort behavior. Actual configured
  compiler/SDK jobs remain the separate native preparation qualification. }
procedure CohortMerging;
var
  LFirst: INyxFPCCompilerCapture;
  LNext: INyxFPCCompilerCapture;
  LMerged: INyxFPCCompilerCapture;
  LSingleFact: INyxFPCCompilerCapture;
  LWithSize: INyxFPCCompilerCapture;
  LWithStorage: INyxFPCCompilerCapture;
  LStorageKey: TNyxPascalStorageKey;
  LOverride: TCaptureOverride;
  LForeign: INyxFPCCompilerCapture;
  LFirstProbe: INyxPascalCompilerProbe;
  LNextProbe: INyxPascalCompilerProbe;
  LText: TNyxText;
  LAt: Integer;
  LRejected: Boolean;

  function ExtraReply(const AReply: TNyxText; const AObserved: TNyxText = ''): TNyxText;
  var
    LFrame: TNyxText;
    LInsert: Integer;
  begin
    LFrame := Frame(AObserved);
    LInsert := Pos(Message('H_TRUE'), LFrame);
    Result := Copy(LFrame, 1, LInsert - 1) + Message(AReply) +
      Copy(LFrame, LInsert, Length(LFrame) - LInsert + 1);
  end;

begin
  LFirstProbe := Probe.WithSymbol(NyxPascalDefine('ALPHA'));
  LNextProbe := Probe.WithSymbol(NyxPascalDefine('BETA'));
  LFirst := ReadNyxFPCCompilerCapture(LFirstProbe, CFile, ExtraReply('U_9'), 0,
    TNyxPascalCompilerTraceOptions.Defaults);
  LNext := ReadNyxFPCCompilerCapture(LNextProbe, CFile, ExtraReply('D_9'), 0,
    TNyxPascalCompilerTraceOptions.Defaults);
  Check((LFirst.Context.Symbol(NyxPascalDefine('ALPHA')).Kind = ppdUndefined) and
    (LFirst.ObservedDefaults.Symbol(NyxPascalDefine('ALPHA')).Kind = ppdUnknown),
    'Explicit verified absences stay separate from the observed-default anchor');
  Check((LFirst.ObservedDefaults.Symbol(NyxPascalDefine('FPC_DELPHI')).Kind = ppdUnknown) and
    (LFirst.Context.Symbol(NyxPascalDefine('FPC_DELPHI')).IsDefined = ppkTrue),
    'Explicit source-mode guard replies do not become initial macro observations');
  LMerged := LFirst.Merge(LNext);
  Check((LMerged.Context.Symbol(NyxPascalDefine('ALPHA')).Kind = ppdUndefined) and
    (LMerged.Context.Symbol(NyxPascalDefine('BETA')).IsDefined = ppkTrue),
    'Independent query cohorts retain old verified absences and new presences together');
  Check((LFirst.Context.Symbol(NyxPascalDefine('BETA')).Kind = ppdUnknown) and
    (LNext.Context.Symbol(NyxPascalDefine('ALPHA')).Kind = ppdUnknown),
    'Cohort merging preserves both original immutable captures');
  Check(LMerged.ObservedDefaults.SameFacts(LFirst.ObservedDefaults),
    'Merged facts retain the exact independent compiler compatibility anchor');
  { A fresh one-query transcript is only a partial observation. The module
    fulfillment boundary requires the complete old owner plus that one fact,
    including verified absences that the fresh query did not request again. }
  LOverride := TCaptureOverride.Create;
  LForeign := LOverride;
  LOverride.Base := LNext;
  LOverride.Value := NyxPascalWithCapturedSize(LNext.Context,
    NyxPascalSizeReference('Measured'), NyxPascalSizeFact(pssKnown, ppmDelphi, ppkTrue, 1));
  LSingleFact := MergeNyxFPCSizeCapture(LMerged, LForeign,
    NyxPascalSizeReference('Measured'));
  Check(not LSingleFact.Context.SameFacts(LForeign.Context) and
    (LForeign.Context.Symbol(NyxPascalDefine('ALPHA')).Kind = ppdUnknown),
    'A partial fresh size packet cannot stand in for the complete one-fact reply');
  Check((LSingleFact.Context.Symbol(NyxPascalDefine('ALPHA')).Kind = ppdUndefined) and
    (LSingleFact.Context.Symbol(NyxPascalDefine('BETA')).IsDefined = ppkTrue) and
    (NyxPascalCapturedSize(LSingleFact.Context, NyxPascalSizeReference('Measured')).Bytes = 1),
    'The complete one-fact reply preserves prior presence and absence while adding its requested value');
  Check(LSingleFact.ObservedDefaults.SameFacts(LMerged.ObservedDefaults) and
    (NyxPascalCapturedSize(LMerged.Context, NyxPascalSizeReference('Measured')).State = pssUnknown),
    'One-fact reply normalization preserves the full default anchor and immutable previous owner');
  Check(LSingleFact.Context.SameFacts(MergeNyxFPCSizeCapture(LMerged,
    LSingleFact, NyxPascalSizeReference('Measured')).Context),
    'A complete one-fact reply satisfies the same exact delta proof without losing earlier facts');
  LForeign := nil;
  LSingleFact := nil;
  LNext := ReadNyxFPCCompilerCapture(LFirstProbe, CFile, ExtraReply('D_9'), 0,
    TNyxPascalCompilerTraceOptions.Defaults);
  LRejected := False;
  try
    LMerged := LMerged.Merge(LNext);
  except
    on ENyxPascalContext do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LMerged.Context.Symbol(NyxPascalDefine('ALPHA')).Kind = ppdUndefined),
    'A conflicting explicit presence refuses before replacing the accumulated packet');
  LNext := ReadNyxFPCCompilerCapture(LNextProbe, CFile,
    ExtraReply('D_9', '(3101) Macro defined: OTHER_TARGET'#10), 0,
    TNyxPascalCompilerTraceOptions.Defaults);
  LRejected := False;
  try
    LMerged := LMerged.Merge(LNext);
  except
    on ENyxPascalContext do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Changed compiler defaults refuse even with the same version and source mode');
  LText := ExtraReply('D_9');
  LAt := Pos(Message('D_1'), LText);
  LText := Copy(LText, 1, LAt - 1) + Message('U_1') +
    Copy(LText, LAt + Length(Message('D_1')), Length(LText));
  LAt := Pos(Message('U_2'), LText);
  LText := Copy(LText, 1, LAt - 1) + Message('D_2') +
    Copy(LText, LAt + Length(Message('U_2')), Length(LText));
  LNext := ReadNyxFPCCompilerCapture(NewNyxFPCCompilerProbe(NyxPascalDefine('QUALIFY'), ppmObjFPC)
    .WithSymbol(NyxPascalDefine('FPC_DOTTEDUNITS')).WithSymbol(NyxPascalDefine('BETA')),
    CFile, LText, 0, TNyxPascalCompilerTraceOptions.Defaults);
  LRejected := False;
  try
    LMerged := LMerged.Merge(LNext);
  except
    on ENyxPascalContext do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Different source-mode captures cannot be merged through matching ordinary symbols');
  LOverride := TCaptureOverride.Create;
  LForeign := LOverride;
  LOverride.Base := LMerged;
  LOverride.Value := LMerged.Context.WithSymbol(NyxPascalDefine('FPC_FULLVERSION'), ppdDefined);
  LWithSize := LMerged.Merge(LForeign);
  Check(LWithSize.Context.Symbol(NyxPascalDefine('FPC_FULLVERSION')).IntegerValue = 30301,
    'A presence-only cohort reply cannot erase an already captured exact integer');
  LOverride.Value := LMerged.Context.WithSymbol(NyxPascalDefine('FPC_FULLVERSION'), ppdInteger, 30302);
  LRejected := False;
  try
    LMerged := LMerged.Merge(LForeign);
  except
    on ENyxPascalContext do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and
    (LMerged.Context.Symbol(NyxPascalDefine('FPC_FULLVERSION')).IntegerValue = 30301),
    'Conflicting exact integer evidence refuses atomically');
  LOverride.Value := NyxPascalWithCapturedSize(LMerged.Context, NyxPascalSizeReference('Measured'),
    NyxPascalSizeFact(pssKnown, ppmDelphi, ppkTrue, 1));
  LWithSize := LMerged.Merge(LForeign);
  Check(NyxPascalCapturedSize(LWithSize.Context, NyxPascalSizeReference('Measured')).Bytes = 1,
    'New exact size evidence survives a compatible immutable cohort merge');
  LOverride.Value := NyxPascalWithCapturedSize(LMerged.Context, NyxPascalSizeReference('Measured'),
    NyxPascalSizeFact(pssKnown, ppmDelphi, ppkTrue, 2));
  LRejected := False;
  try
    LWithSize := LWithSize.Merge(LForeign);
  except
    on ENyxPascalContext do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and
    (NyxPascalCapturedSize(LWithSize.Context, NyxPascalSizeReference('Measured')).Bytes = 1),
    'Conflicting size evidence refuses atomically');
  { Foreign metadata checks qualify copy/merge only, never a new native compiler
    execution receipt. Structural keys cannot enter the primitive-name vector. }
  LStorageKey := NyxPascalStorageKey('nyx.storage.1|cohort');
  LOverride.Value := NyxPascalWithCapturedStorage(LMerged.Context, LStorageKey,
    NyxPascalStorageFact(psfsKnown, 7));
  LWithStorage := LMerged.Merge(LForeign);
  Check((NyxPascalCapturedStorage(LWithStorage.Context, LStorageKey).Bytes = 7) and
    (NyxPascalCapturedStorage(LMerged.Context, LStorageKey).State = psfsUnknown),
    'Structural cohort evidence merges without changing the previous owner');
  Check(FreezeNyxFPCCompilerCapture(LWithStorage).Context.SameFacts(LWithStorage.Context),
    'Freezing a complete capture preserves detached structural receipts');
  LOverride.Value := NyxPascalWithCapturedStorage(LMerged.Context, LStorageKey,
    NyxPascalStorageFact(psfsKnown, 8));
  LRejected := False;
  try
    LWithStorage := LWithStorage.Merge(LForeign);
  except
    on ENyxPascalContext do LRejected := True;
  end;
  Check(LRejected and (NyxPascalCapturedStorage(LWithStorage.Context, LStorageKey).Bytes = 7),
    'A conflicting structural observation refuses atomically');
  LOverride.Value := LMerged.Context;
  LOverride.Defaults := NyxPascalWithCapturedStorage(LMerged.ObservedDefaults, LStorageKey,
    NyxPascalStorageFact(psfsKnown, 7));
  LRejected := False;
  try
    LWithStorage := LWithStorage.Merge(LForeign);
  except
    on ENyxPascalContext do LRejected := True;
  end;
  Check(LRejected, 'A structural reply cannot masquerade as an observed-default anchor');
  LOverride.Defaults := nil;
  LOverride.Value := NyxPascalWithWarning(LMerged.Context, NyxPascalWarningNumber(42), pwaOff);
  LRejected := False;
  try
    LMerged := LMerged.Merge(LForeign);
  except
    on ENyxPascalContext do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Authored source-warning state cannot masquerade as a pristine compiler cohort');
  LForeign := nil;
  LFirst := nil;
  LNext := nil;
  Check((LMerged.Context.Symbol(NyxPascalDefine('ALPHA')).Kind = ppdUndefined) and
    (LMerged.Context.Symbol(NyxPascalDefine('BETA')).IsDefined = ppkTrue),
    'Merged portable values outlive their original captures and foreign producer');
end;

{ Compare request intent only. A canonical source is not a compiler transcript,
  a configured target, a measured fact or a source-package admission receipt. }
procedure SameRestoredProbe(const AExpected, AActual: INyxPascalCompilerProbe);
var
  LExpectedSizes, LActualSizes: INyxPascalSizeProbe;
  LAt: Integer;
begin
  Check((AActual <> nil) and (AActual.Identity.Name = AExpected.Identity.Name) and
    (AActual.Mode = AExpected.Mode) and (NyxFPCProbeScope(AActual) = NyxFPCProbeScope(AExpected)) and
    (AActual.Count = AExpected.Count) and (AActual.Source = AExpected.Source),
    'Canonical restoration preserves complete request source, identity, mode, scope and name count');
  for LAt := 0 to AExpected.Count - 1 do
    Check(AActual.Symbol(LAt).Name = AExpected.Symbol(LAt).Name,
      'Restored query names preserve their exact canonical order');
  Check(Supports(AExpected, INyxPascalSizeProbe, LExpectedSizes) and
    Supports(AActual, INyxPascalSizeProbe, LActualSizes) and
    (LExpectedSizes.SizeCount = LActualSizes.SizeCount),
    'Canonical restoration preserves the separately typed size-query vector');
  for LAt := 0 to LExpectedSizes.SizeCount - 1 do
    Check(LExpectedSizes.SizeName(LAt).Name = LActualSizes.SizeName(LAt).Name,
      'Restored size names preserve qualification and ordinal without claiming a reply');
end;

procedure RefusesRestoration(const ASource: TNyxText;
  const AOptions: TNyxPascalCompilerTraceOptions; const AReason: String);
var
  LCandidate: INyxPascalCompilerProbe;
  LRejected: Boolean;
begin
  LRejected := False;
  LCandidate := nil;
  try LCandidate := RestoreNyxFPCCompilerProbe(ASource, AOptions);
  except on ENyxPascalContext do LRejected := True; end;
  Check(LRejected and (LCandidate = nil), AReason);
end;

procedure Restoration;
const
  CModes: array[0..4] of TNyxPascalLanguageMode =
    (ppmFPC, ppmObjFPC, ppmDelphi, ppmDelphiUnicode, ppmTP);
var
  LProbe, LRestored, LHeld, LCandidate: INyxPascalCompilerProbe;
  LText, LBad, LLongName: TNyxText;
  LOptions: TNyxPascalCompilerTraceOptions;
  LBudget: INyxPascalAnalysisBudget;
  LPolicy: TRestorePolicy;
  LPolicyScope: INyxPascalAnalysisBudget;
  LCancel: INyxCancellationScope;
  LAt, LCase, LWork, LCalls, LCancelAt: Integer;
  LRejected: Boolean;
begin
  LOptions := TNyxPascalCompilerTraceOptions.Defaults;
  for LAt := Low(CModes) to High(CModes) do
  begin
    LProbe := NewNyxFPCCompilerProbe(NyxPascalDefine('Begin_D_0_S_0_End'), CModes[LAt]);
    LRestored := RestoreNyxFPCCompilerProbe(LProbe.Source, LOptions);
    SameRestoredProbe(LProbe, LRestored);
  end;
  LProbe := NyxFPCProbeSizeOf(NewNyxFPCSystemBootstrapProbe(NyxPascalDefine('Root_Begin')),
    NyxPascalSizeReference('Word'));
  SameRestoredProbe(LProbe, RestoreNyxFPCCompilerProbe(LProbe.Source, LOptions));
  LProbe := NewNyxFPCCompilerProbe(NyxPascalDefine(TNyxText(StringOfChar('x', 128))));
  LProbe := LProbe.WithSymbol(NyxPascalDefine(TNyxText(StringOfChar('a', 128))));
  LLongName := TNyxText(StringOfChar('b', 128));
  for LAt := 1 to 6 do LLongName := LLongName + '.' + TNyxText(StringOfChar('b', 128));
  LLongName := LLongName + '.' + TNyxText(StringOfChar('c', 121));
  Check(Length(LLongName) = 1024, 'The positive qualified size name reaches the existing exact spelling bound');
  LProbe := NyxFPCProbeSizeOf(LProbe, NyxPascalSizeReference(LLongName));
  SameRestoredProbe(LProbe, RestoreNyxFPCCompilerProbe(LProbe.Source, LOptions));
  LProbe := NewNyxFPCCompilerProbe(NyxPascalDefine('Maximum'));
  for LAt := LProbe.Count to 255 do
    LProbe := LProbe.WithSymbol(NyxPascalDefine('Name_' + TNyxText(IntToStr(LAt))));
  for LAt := 0 to 63 do
    LProbe := NyxFPCProbeSizeOf(LProbe, NyxPascalSizeReference('Measured_' + TNyxText(IntToStr(LAt))));
  SameRestoredProbe(LProbe, RestoreNyxFPCCompilerProbe(LProbe.Source, LOptions));
  LProbe := NyxFPCProbeSizeOf(NyxFPCProbeSizeOf(Probe,
    NyxPascalSizeReference('Integer')), NyxPascalSizeReference('System.Integer'));
  LText := LProbe.Source;
  LHeld := RestoreNyxFPCCompilerProbe(LText, LOptions.UpToBytes(Length(LText)));
  SameRestoredProbe(LProbe, LHeld);
  RefusesRestoration(LText, LOptions.UpToBytes(Length(LText) - 1),
    'One byte below the exact ASCII source length refuses without truncation');
  RefusesRestoration(LText, LOptions.UpToLines(1), 'Request restoration retains the existing line-work ceiling');
  RefusesRestoration(LText, LOptions.UpToLineLength(8), 'A short line ceiling refuses the whole request');
  for LCase := 0 to 15 do
  begin
    case LCase of
      0: LBad := #10 + LText;
      1: LBad := LText + #10;
      2: LBad := TNyxText(StringReplace(String(LText), #10, #13#10, [rfReplaceAll]));
      3: LBad := TNyxText('🚀') + LText;
      4: LBad := TNyxText(StringReplace(String(LText), '{$ifdef fpc_dottedunits}', '{$ifdef fpc}', []));
      5: LBad := TNyxText(StringReplace(String(LText), '{$ifdef fpc}', '{$ifdef other_guard}', []));
      6: LBad := TNyxText(StringReplace(String(LText), 'D_8}', 'D_80}', []));
      7: LBad := TNyxText(StringReplace(String(LText), 'S_0_B_3_0}', 'S_0_B_3_1}', []));
      8: LBad := TNyxText(StringReplace(String(LText), 'sizeof(integer) >= 0', 'sizeof(pointer) >= 0', []));
      9: LBad := TNyxText(StringReplace(String(LText), '{$mode delphi}', '{$mode macpas}', []));
      10: LBad := LText + '{$fatal unexpected_bootstrap_stop}'#10;
      11: LBad := Copy(LText, 1, Length(LText) - 2);
      12: LBad := TNyxText(StringReplace(String(LText), 'program NyxCompilerContext;',
        'program NyxCompilerContext; { extra source }', []));
      13: LBad := TNyxText(StringReplace(String(LText), '{$ifdef fpc_dottedunits}', '{$ifdef bad' + #0 + 'name}', []));
      14: LBad := TNyxText(StringReplace(String(LText), '2147483647', '2147483646', []));
      15: LBad := TNyxText(StringReplace(String(LText), 'S_1_K}', 'S_2_K}', []));
    end;
    Check(LBad <> LText, 'The malformed canonical-source fixture changes its intended bytes: ' + IntToStr(LCase));
    RefusesRestoration(LBad, LOptions,
      'Noncanonical source cannot be normalized into another request: ' + IntToStr(LCase));
  end;
  Check(LHeld.Source = LText, 'All malformed-source refusals preserve the separately held last valid request');
  LBudget := NewNyxPascalAnalysisBudget(1000000);
  LRestored := RestoreNyxFPCCompilerProbe(LText, LOptions, LBudget);
  LWork := LBudget.Receipt.Total;
  Check((LWork > Length(LText)) and (LBudget.Receipt.Used(papSourceProof) = LWork),
    'Input, query-vector and streamed comparison work enters the existing source-proof phase');
  LBudget.Retire;
  LBudget := NewNyxPascalAnalysisBudget(LWork);
  SameRestoredProbe(LProbe, RestoreNyxFPCCompilerProbe(LText, LOptions, LBudget));
  Check(LBudget.Receipt.Total = LWork, 'Exact work allowance admits the same canonical request deterministically');
  LBudget.Retire;
  LBudget := NewNyxPascalAnalysisBudget(LWork - 1);
  LCandidate := nil;
  LRejected := False;
  try LCandidate := RestoreNyxFPCCompilerProbe(LText, LOptions, LBudget);
  except on ENyxPascalAnalysisWorkLimit do LRejected := True; end;
  Check(LRejected and (LCandidate = nil) and (LBudget.Receipt.Total <= LWork - 1),
    'One less work unit refuses before an unadmitted comparison or result can publish');
  LBudget.Retire;
  LBudget := NewNyxPascalAnalysisBudget(LWork * 2 - 1);
  LRestored := RestoreNyxFPCCompilerProbe(LText, LOptions, LBudget);
  LCandidate := nil;
  LRejected := False;
  try LCandidate := RestoreNyxFPCCompilerProbe(LText, LOptions, LBudget);
  except on ENyxPascalAnalysisWorkLimit do LRejected := True; end;
  Check(LRejected and (LCandidate = nil) and (LRestored.Source = LText) and
    (LBudget.Receipt.Total <= LWork * 2 - 1), 'Two restorations share the actual remaining parent allowance');
  LBudget.Retire;
  LCancel := NewNyxCancellationScope;
  LCancel.Cancel;
  LCandidate := nil;
  LRejected := False;
  try LCandidate := RestoreNyxFPCCompilerProbe(LText, LOptions, LCancel);
  except on ENyxPascalAnalysisCancelled do LRejected := True; end;
  Check(LRejected and (LCandidate = nil), 'Pre-cancelled restoration publishes no request');
  LPolicy := TRestorePolicy.Create;
  LPolicyScope := LPolicy;
  LPolicy.Budget := NewNyxPascalAnalysisBudget(1000000);
  LRestored := RestoreNyxFPCCompilerProbe(LText, LOptions, LPolicyScope);
  LCalls := LPolicy.Calls;
  Check((LCalls > 2) and (LRestored.Source = LText), 'Successful restoration exposes real outward admission boundaries');
  LPolicyScope := nil;
  for LCase := 0 to 2 do
  begin
    case LCase of
      0: LCancelAt := 1;
      1: LCancelAt := LCalls div 2;
      2: LCancelAt := LCalls;
    end;
    LPolicy := TRestorePolicy.Create;
    LPolicyScope := LPolicy;
    LPolicy.Budget := NewNyxPascalAnalysisBudget(1000000);
    LPolicy.CancelAt := LCancelAt;
    LCandidate := nil;
    LRejected := False;
    try LCandidate := RestoreNyxFPCCompilerProbe(LText, LOptions, LPolicyScope);
    except on ENyxPascalAnalysisCancelled do LRejected := True; end;
    Check(LRejected and (LCandidate = nil) and (LPolicy.Calls = LCancelAt) and
      (LPolicy.AfterCancellation = 0) and (LHeld.Source = LText),
      'Cancellation at an actual first/middle/final admission stops exactly and preserves the prior request');
    LPolicyScope := nil;
  end;
  LPolicy := TRestorePolicy.Create;
  LPolicyScope := LPolicy;
  LPolicy.Budget := NewNyxPascalAnalysisBudget(1000000);
  LPolicy.NestedSource := NewNyxFPCSystemBootstrapProbe(NyxPascalDefine('Nested')).Source;
  LPolicy.ReenterAt := 1;
  LRestored := RestoreNyxFPCCompilerProbe(LText, LOptions, LPolicyScope);
  Check(LPolicy.Entered and (LPolicy.NestedResult <> nil) and
    (LPolicy.NestedResult.Source = LPolicy.NestedSource) and (LRestored.Source = LText) and
    (LPolicy.Receipt.Total > LWork),
    'Reentrant restoration retains independent input/cursors and charges both operations to one parent');
  LPolicyScope := nil;
end;

{ Keep native function-result interface temporaries in the explicit caller's
  lexical scope; the outer check then observes actual retirement, not a compiler
  temporary that is still alive in its current procedure. }
procedure RestoreReleasedInputs(const AText: TNyxText; out AResult: INyxPascalCompilerProbe);

  { End class-to-interface conversion temporaries before the hostile getter;
    GRestoreScope must be the caller's actual last policy lease. }
  procedure Prepare;
  var
    LPolicy: TRestorePolicy;
  begin
    GRestoreSource := AText;
    GRestoreOptions := TNyxPascalCompilerTraceOptions.Defaults;
    LPolicy := TRestorePolicy.Create;
    GRestoreScope := LPolicy;
    LPolicy.Budget := NewNyxPascalAnalysisBudget(1000000);
    LPolicy.ReleaseFromCancelled := True;
  end;

begin
  Prepare;
  AResult := RestoreNyxFPCCompilerProbe(GRestoreSource, GRestoreOptions, GRestoreScope);
  GRestoreScope := nil;
end;

procedure RestorationLifetime;
var
  LResult: INyxPascalCompilerProbe;
  LText: TNyxText;
  LBefore: Integer;
begin
  LText := Probe.Source;
  LBefore := GRestorePoliciesDestroyed;
  RestoreReleasedInputs(LText, LResult);
  Check((LResult.Source = LText) and (GRestoreScope = nil) and
    (GRestoreSource = 'changed caller input') and (GRestoreOptions.MaximumBytes = 1),
    'Restoration leases original text/options/last scope before outward cancellation getters can replace them');
  Check(GRestorePoliciesDestroyed = LBefore + 1,
    'Published canonical request outlives and retains no caller execution policy');
end;

{$ifndef PAS2JS}
{ Optional inputs are explicit files, never discovered configuration or compiler
  jobs. The first pair retains the old canonical-request-only contract. A second
  pair supplies the already accepted bootstrap request and its terminal log for
  a separate parser regression; it grants no new execution provenance. }
procedure RequireNativeRestorationInputs;
var
  LAt: Integer;
begin

  if not (ParamCount in [0, 2, 4]) then
    raise Exception.Create('Supply no arguments, two canonical request sources, or those sources followed by one retained bootstrap source/log pair');
  for LAt := 1 to ParamCount do
  begin

    if not FileExists(ParamStr(LAt)) then
      raise Exception.Create('Each explicit retained source/log input must exist');
  end;
end;

procedure NativeRestorationInputs;
var
  LStream: TFileStream;
  LBytes, LRendered: TNyxBytes;
  LText: TNyxText;
  LProbe, LBootstrap: INyxPascalCompilerProbe;
  LSizes: INyxPascalSizeProbe;
  LCapture: INyxFPCCompilerCapture;
  LFact: TNyxPascalSizeFact;
  LAt, LByte, LLast: Integer;
  LEqual: Boolean;
begin

  if ParamCount = 0 then Exit;
  LBytes := nil;
  LRendered := nil;
  LLast := 2;

  if ParamCount = 4 then LLast := 3;
  for LAt := 1 to LLast do
  begin
    LStream := TFileStream.Create(ParamStr(LAt), fmOpenRead or fmShareDenyWrite);
    try

      if (LStream.Size < 1) or
        (LStream.Size > TNyxPascalCompilerTraceOptions.Defaults.MaximumBytes) then
        raise Exception.Create('Explicit canonical request input exceeds its unchanged byte limit');
      SetLength(LBytes, Integer(LStream.Size));
      LStream.ReadBuffer(LBytes[0], Length(LBytes));
    finally
      LStream.Free;
    end;
    LText := NyxDecodeUTF8(LBytes);
    LProbe := RestoreNyxFPCCompilerProbe(LText, TNyxPascalCompilerTraceOptions.Defaults);
    Check(LProbe.Source = LText, 'Explicit retained source restores only its exact canonical request intent');
    LRendered := NyxEncodeUTF8(LProbe.Source);
    LEqual := Length(LRendered) = Length(LBytes);

    if LEqual then
    begin
      for LByte := 0 to High(LBytes) do
      begin

        if LBytes[LByte] <> LRendered[LByte] then
        begin
          LEqual := False;
          Break;
        end;
      end;
    end;
    Check(LEqual, 'The shared emitter remains byte-identical to the explicit retained pre-emitter request');
    WriteLn('canonical request file / ordinal=', LAt, ' symbols=', LProbe.Count,
      ' mode=', Ord(LProbe.Mode), ' scope=', Ord(NyxFPCProbeScope(LProbe)));

    if LAt = 3 then
    begin
      LBootstrap := NewNyxFPCSystemBootstrapProbe(NyxPascalDefine('NYX_REQUIRED_FACT'));
      Check((NyxFPCProbeScope(LProbe) = pcsoSystemBootstrap) and
        (LProbe.Identity.Name = 'nyx_required_fact') and (LProbe.Mode = ppmFPC) and
        (LProbe.Count = LBootstrap.Count) and Supports(LProbe, INyxPascalSizeProbe, LSizes) and
        (LSizes.SizeCount = 1) and (LSizes.SizeName(0).Name = 'longword'),
        'The explicit accepted bootstrap request retains its identity, scope and sole typed operand');
      LStream := TFileStream.Create(ParamStr(4), fmOpenRead or fmShareDenyWrite);
      try

        if (LStream.Size < 1) or
          (LStream.Size > TNyxPascalCompilerTraceOptions.Defaults.MaximumBytes) then
          raise Exception.Create('Explicit terminal compiler log exceeds its unchanged byte limit');
        SetLength(LBytes, Integer(LStream.Size));
        LStream.ReadBuffer(LBytes[0], Length(LBytes));
      finally
        LStream.Free;
      end;
      LCapture := ReadNyxFPCCompilerCapture(LProbe, 'system.pp', NyxDecodeUTF8(LBytes), 1,
        TNyxPascalCompilerTraceOptions.Defaults);
      LFact := NyxPascalCapturedSize(LCapture.Context, LSizes.SizeName(0));
      Check((NyxPascalCompilerScope(LCapture.Context) = pcsoSystemBootstrap) and
        (LCapture.Context.Family = ppcFPC) and (LCapture.Context.Version.FullNumber = 30301) and
        (LCapture.Context.Mode = ppmFPC) and (LCapture.Context.Option('H') = ppkFalse),
        'The complete saved transcript retains its actual FPC bootstrap compiler/mode/H observations');
      { Four is an observed fixture result in this specific retained transcript,
        not a size inferred for another compiler, target or caller context. }
      Check((LFact.State = pssKnown) and (LFact.Bytes = 4) and
        (NyxPascalCapturedSize(LCapture.ObservedDefaults, LSizes.SizeName(0)).State = pssUnknown),
        'The saved reply establishes its observed value separately from unqueried defaults');
    end;
  end;
end;
{$endif}

begin
  try
    {$ifndef PAS2JS}
    RequireNativeRestorationInputs;
    {$endif}
    Protocol;
    CohortMerging;
    Restoration;
    RestorationLifetime;
    {$ifndef PAS2JS}NativeRestorationInputs;{$endif}
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
