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
program nyx_pascal_optimization_tests;
{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, Classes, nyx.text, nyx.bytes, nyx.scheduler, nyx.pascal.context,
  nyx.pascal.files, nyx.pascal.lexing, nyx.pascal.translation,
  nyx.pascal.syntax, nyx.pascal.syntax.translated
  {$ifndef PAS2JS}, optimization.policy.fixture{$endif};

type
  { Mutable capture-only source. Independent context capture must survive later
    changes and retirement; InvalidAt deliberately supplies an empty request to
    qualify failure after an earlier valid row, without retaining a host/editor. }
  TOptimizationSeed = class(TInterfacedObject, INyxPascalContextOptimizations)
  public
    Count: Integer;
    InvalidAt: Integer;
    Rule: TNyxPascalOptimizationRule;
    function OptimizationCount: Integer;
    function OptimizationAt(AIndex: Integer): TNyxPascalOptimizationRule;
    function WithOptimization(const ARequest: TNyxPascalOptimizationRequest;
      AActivity: TNyxPascalKnowledge): INyxPascalCompilerContext;
    function WithOptimizationRules(
      const ASource: INyxPascalContextOptimizations): INyxPascalCompilerContext;
  end;

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

function TOptimizationSeed.OptimizationCount: Integer;
begin
  Result := Count;
end;

function TOptimizationSeed.OptimizationAt(AIndex: Integer): TNyxPascalOptimizationRule;
begin

  if AIndex = InvalidAt then
  begin
    Exit(Default(TNyxPascalOptimizationRule));
  end;
  Result := Rule;
end;

function TOptimizationSeed.WithOptimization(const ARequest: TNyxPascalOptimizationRequest;
  AActivity: TNyxPascalKnowledge): INyxPascalCompilerContext;
begin
  Result := nil;
  raise ENyxPascalContext.Create('This seed supplies capture only');
end;

function TOptimizationSeed.WithOptimizationRules(
  const ASource: INyxPascalContextOptimizations): INyxPascalCompilerContext;
begin
  Result := nil;
  raise ENyxPascalContext.Create('This seed supplies capture only');
end;

function Facts: INyxPascalCompilerContext;
begin
  Result := NewNyxPascalCompilerContext(ppcFPC,
    TNyxPascalCompilerVersion.Create(3, 3, 1), ppmDelphi).CompleteDefines(True)
    .WithSymbol(NyxPascalDefine('MAYBE'), ppdUnknown);
end;

function Translate(const AText: TNyxText; const APart: TNyxText = ''): INyxPascalTranslation;
begin
  Result := NewNyxPascalTranslation(NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('main.pas'), AText)
    .WithFile(NyxPascalPhysicalFile('part.inc'), APart), NyxPascalPhysicalFile('main.pas'),
    Facts, TNyxPascalTranslationOptions.Defaults);
end;

function Marker(const ASource: INyxPascalTranslation; const AName: TNyxText): Integer;
var
  LAt: Integer;
begin
  for LAt := 0 to ASource.Count - 1 do
  begin

    if ASource.Text(LAt) = AName then
    begin
      Exit(LAt);
    end;
  end;
  raise Exception.Create('A compiler-state marker is absent');
end;

procedure ContextOwnership;
var
  LBase: INyxPascalCompilerContext;
  LSaved: INyxPascalCompilerContext;
  LCurrent: INyxPascalCompilerContext;
  LCopy: INyxPascalCompilerContext;
  LProducer: TOptimizationSeed;
  LSeed: INyxPascalContextOptimizations;
  LRequest: TNyxPascalOptimizationRequest;
  LRejected: Boolean;
begin
  LBase := Facts;
  LRequest := TNyxPascalOptimizationRequest.Toggle(pofFastMath, False);
  LSaved := NyxPascalWithOptimization(LBase, TNyxPascalOptimizationRequest.Select(pomInitial));
  LCurrent := NyxPascalWithOptimization(LSaved, LRequest, ppkUnknown);
  Check(NyxPascalOptimizations(LBase).OptimizationCount = 0, 'Append preserves the original context');
  Check((NyxPascalOptimizations(LCurrent).OptimizationCount = 2) and
    (NyxPascalOptimizations(LCurrent).OptimizationAt(1).Activity = ppkUnknown),
    'Chronological requests retain independent conditional activity');
  Check((NyxPascalOptimizations(LSaved).OptimizationAt(0).Request.RequestedMode = pomInitial) and
    not NyxPascalOptimizations(LCurrent).OptimizationAt(1).Request.Enabled,
    'Initial reset and explicit disable remain distinct authored intents');
  Check(CaptureNyxPascalCompilerContext(LCurrent).SameFacts(LCurrent),
    'Whole context capture preserves all optimizer requests');
  Check(not LCurrent.SameFacts(LSaved), 'Optimizer history participates in exact context identity');
  Check(not LRequest.SameRequest(TNyxPascalOptimizationRequest.Toggle(pofFastMath, True)),
    'Opposite enable values retain distinct request identities');
  Check(NyxPascalOptimizations(LCurrent.InMode(ppmObjFPC)).OptimizationCount = 2,
    'Grammar mode copies retain optimizer intent');
  Check(NyxPascalOptimizations(LCurrent.UnknownState).OptimizationCount = 0,
    'Unknown textual effects revoke current optimizer knowledge');
  LCopy := RestoreNyxPascalLocalSettings(LCurrent.WithSymbol(NyxPascalDefine('KEEP'), ppdDefined), LSaved);
  Check((NyxPascalOptimizations(LCopy).OptimizationCount = 1) and
    (LCopy.Symbol(NyxPascalDefine('KEEP')).IsDefined = ppkTrue),
    'Local restoration recovers optimizer history while keeping later definitions');
  LProducer := TOptimizationSeed.Create;
  LSeed := LProducer;
  LProducer.Count := 2;
  LProducer.InvalidAt := -1;
  LProducer.Rule := NyxPascalOptimizations(LSaved).OptimizationAt(0);
  LCopy := NyxPascalOptimizations(LBase).WithOptimizationRules(LSeed);
  LProducer.Rule := NyxPascalOptimizations(LCurrent).OptimizationAt(1);
  Check(NyxPascalOptimizations(LCopy).OptimizationAt(0).Request.RequestedMode = pomInitial,
    'Alternate producers are frozen independently');
  LProducer.InvalidAt := 1;
  LRejected := False;
  try
    LCopy := NyxPascalOptimizations(LBase).WithOptimizationRules(LSeed);
  except
    on ENyxPascalContext do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (NyxPascalOptimizations(LCopy).OptimizationCount = 2),
    'A later invalid alternate getter preserves the previous publication');
  LSeed := nil;
  Check(NyxPascalOptimizations(LCopy).OptimizationAt(1).Request.RequestedMode = pomInitial,
    'Captured requests survive producer retirement');
  LRejected := False;
  try
    Default(TNyxPascalOptimizationRequest).Validate;
  except
    on ENyxPascalContext do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'A default zero record cannot silently request OFF');
  LRejected := False;
  try
    LRequest.RequestedMode;
  except
    on ENyxPascalContext do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'A feature toggle cannot masquerade as a preset');
  LRejected := False;
  try
    TNyxPascalOptimizationRequest.Select(pomDisabled).Feature;
  except
    on ENyxPascalContext do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'An optimizer reset has no invented feature selector');
end;

procedure SourceLifetime;
var
  LSource: INyxPascalTranslation;
  LPrefix: INyxPascalTranslation;
  LRows: INyxPascalOptimizationDirectives;
  LPrefixRows: INyxPascalOptimizationDirectives;
  LRow: TNyxPascalOptimizationDirective;
begin
  LSource := Translate('unit Main; {English and a Unicode check: 😀}' +
    '{$optimization OFF}{$push}{$i part.inc}{$mode objfpc}{$i part.inc}' +
    'interface var BeforePop:Integer;{$pop}var AfterPop:Integer;{$ifdef MAYBE}' +
    '{$optimization ON}{$endif}{$ifdef ABSENT}{$optimization unknown}{$endif}' +
    'var AfterMaybe:Integer; implementation end.', '{$optimization NOFASTMATH,FASTMATH}');
  Supports(LSource, INyxPascalOptimizationDirectives, LRows);
  Check(LRows.OptimizationDirectiveCount = 4, 'Inactive branches stay absent; repeated includes retain receipts');
  Check(NyxPascalOptimizations(LSource.State(LSource.Token(Marker(LSource, 'BeforePop')).State))
    .OptimizationCount = 3, 'Included optimizer intent survives source mode changes');
  Check(NyxPascalOptimizations(LSource.State(LSource.Token(Marker(LSource, 'AfterPop')).State))
    .OptimizationCount = 1, 'POP restores the earlier authored request vector');
  Check(NyxPascalOptimizations(LSource.State(LSource.Token(Marker(LSource, 'AfterMaybe')).State))
    .OptimizationAt(1).Activity = ppkUnknown, 'Unknown branch activity never becomes an effective preset');
  Check(not LSource.Resolved and (LSource.DiagnosticCount = 1) and
    (LSource.Diagnostic(0).Issue = ptiUnknownCondition), 'Supported requests do not hide unresolved conditions');
  LRow := LRows.OptimizationDirectiveAt(2);
  Check((LRow.Rule.Request.Feature = pofFastMath) and not LRow.Rule.Request.Enabled,
    'FPC source reads only the first selector, independently of command-line list syntax');
  Check((LRow.Origin.FileReference.Name = TNyxText('part.inc')) and
    (LRow.Origin.Occurrence <> LRows.OptimizationDirectiveAt(1).Origin.Occurrence),
    'Repeated includes retain independent physical occurrences');
  Check(NyxPascalOccurrenceLexical(LSource, LRow.Origin.Occurrence).Slice(LRow.Origin.Span) =
    TNyxText('{$optimization NOFASTMATH,FASTMATH}'), 'Navigation retains every original source scalar');
  Check(NyxPascalOptimizations(LSource.State(LRow.State)).OptimizationCount = 2,
    'Receipt state precedes its own source request');
  LPrefix := NewNyxPascalTranslationPrefix(LSource, LRow.Origin.PrefixCount,
    TNyxPascalTranslationOptions.Defaults);
  Supports(LPrefix, INyxPascalOptimizationDirectives, LPrefixRows);
  Check(LPrefixRows.OptimizationDirectiveCount = 2, 'Prefix reads exclude their boundary directive');
  LSource := nil;
  LRows := nil;
  Check(LPrefixRows.OptimizationDirectiveAt(1).Rule.Request.Feature = pofFastMath,
    'Immutable prefix receipts survive the original caller');
end;

procedure Refusals;
var
  LSource: INyxPascalTranslation;
  LFull: INyxPascalCompilerContext;
  LProducer: TOptimizationSeed;
  LSeed: INyxPascalContextOptimizations;
  LCancel: INyxCancellationScope;
  LRejected: Boolean;
begin
  LSource := Translate('{$optimization OFF}{$optimization UNKNOWN_PASS}var Marker:Integer;');
  Check(not LSource.Resolved and (LSource.Diagnostic(0).Issue = ptiMalformedDirective) and
    (NyxPascalOptimizations(LSource.State(LSource.Token(Marker(LSource, 'Marker')).State))
      .OptimizationCount = 0), 'An unknown selector refuses and revokes current settings knowledge');
  LSource := Translate('{$optimization }');
  Check(not LSource.Resolved, 'An empty optimizer request refuses visibly');
  LSource := NewNyxPascalTranslation(NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('wrong.pas'), '{$optimization ON}'),
    NyxPascalPhysicalFile('wrong.pas'), NewNyxPascalCompilerContext(ppcPas2JS,
      TNyxPascalCompilerVersion.Create(3, 3, 1), ppmDelphi), TNyxPascalTranslationOptions.Defaults);
  Check(not LSource.Resolved and (LSource.Diagnostic(0).Issue = ptiUnsupportedDirective),
    'FPC requests do not fabricate another compiler family policy');
  LProducer := TOptimizationSeed.Create;
  LSeed := LProducer;
  LProducer.Count := NyxPascalOptimizationRuleLimit;
  LProducer.InvalidAt := -1;
  LProducer.Rule := NyxPascalOptimizations(NyxPascalWithOptimization(Facts,
    TNyxPascalOptimizationRequest.Select(pomInitial))).OptimizationAt(0);
  LFull := NyxPascalOptimizations(Facts).WithOptimizationRules(LSeed);
  LSource := nil;
  LRejected := False;
  try
    LSource := NewNyxPascalTranslation(NewNyxPascalFileCatalog(pfcExact)
      .WithFile(NyxPascalPhysicalFile('full.pas'), '{$optimization ON}'),
      NyxPascalPhysicalFile('full.pas'), LFull, TNyxPascalTranslationOptions.Defaults);
  except
    on ENyxPascalTranslationLimit do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LSource = nil), 'Exhaustion exposes no partial translation');
  LProducer.Count := NyxPascalOptimizationRuleLimit + 1;
  LRejected := False;
  try
    NyxPascalOptimizations(Facts).WithOptimizationRules(LSeed);
  except
    on ENyxPascalContext do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Excess alternate rows refuse before enumeration');
  LCancel := NewNyxCancellationScope;
  LCancel.Cancel;
  LRejected := False;
  try
    LSource := NewNyxPascalTranslation(NewNyxPascalFileCatalog(pfcExact)
      .WithFile(NyxPascalPhysicalFile('cancel.pas'), '{$optimization ON}'),
      NyxPascalPhysicalFile('cancel.pas'), Facts, TNyxPascalTranslationOptions.Defaults, LCancel);
  except
    on ENyxPascalAnalysisCancelled do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LSource = nil), 'Cancellation publishes no partial optimizer source');
end;

{$ifndef PAS2JS}
function OriginalFile(const APath: TNyxText): TNyxText;
var
  LStream: TFileStream;
  LBytes: TNyxBytes;
begin
  LStream := TFileStream.Create(String(APath), fmOpenRead or fmShareDenyWrite);
  try
    SetLength(LBytes, LStream.Size);
    LStream.ReadBuffer(LBytes[0], Length(LBytes));
  finally
    LStream.Free;
  end;
  Result := NyxDecodeUTF8(LBytes);
end;

procedure NativeCompanion(const ARoot: TNyxText);
var
  LMain: TNyxText;
  LPart: TNyxText;
  LSource: INyxPascalTranslation;
  LView: INyxPascalTranslatedSyntax;
  LRows: INyxPascalOptimizationDirectives;
  LFeature: TNyxPascalOptimizationFeature;
  LAt: Integer;
begin
  Check(OptimizationFixtureValue(20) = 42, 'The whole original compiler companion executes ordinary Pascal');
  LMain := OriginalFile(ARoot + '/tests/optimization.policy.fixture.pas');
  LPart := OriginalFile(ARoot + '/tests/optimization.policy.fixture.inc');
  LSource := NewNyxPascalTranslation(NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('optimization.policy.fixture.pas'), LMain)
    .WithFile(NyxPascalPhysicalFile('optimization.policy.fixture.inc'), LPart),
    NyxPascalPhysicalFile('optimization.policy.fixture.pas'), Facts, TNyxPascalTranslationOptions.Defaults);
  LView := NewNyxPascalTranslatedSyntax(LSource, TNyxPascalSyntaxOptions.Defaults);
  Supports(LSource, INyxPascalOptimizationDirectives, LRows);
  Check(LSource.Resolved and (LView.Syntax.DiagnosticCount = 0),
    'The exact compiled unit/include retains complete supported syntax');
  Check(LRows.OptimizationDirectiveCount = 62, 'All reset/pass requests retain original source chronology');
  for LFeature := Low(TNyxPascalOptimizationFeature) to High(TNyxPascalOptimizationFeature) do
  begin
    LAt := 1 + Ord(LFeature) * 2;
    Check((LRows.OptimizationDirectiveAt(LAt).Rule.Request.Feature = LFeature) and
      LRows.OptimizationDirectiveAt(LAt).Rule.Request.Enabled and
      (LRows.OptimizationDirectiveAt(LAt + 1).Rule.Request.Feature = LFeature) and
      not LRows.OptimizationDirectiveAt(LAt + 1).Rule.Request.Enabled,
      'Each actual compiled pass spelling maps to its exact typed enable/disable request');
  end;
  Check((LRows.OptimizationDirectiveAt(1).Origin.FileReference.Name =
    TNyxText('optimization.policy.fixture.inc')) and
    (LSource.Files.TextAt(0) = LMain) and (LSource.Files.TextAt(1) = LPart),
    'Compiler companions keep exact source/include ownership and every scalar');
end;
{$endif}

begin
  try
    ContextOwnership;
    SourceLifetime;
    Refusals;
    {$ifndef PAS2JS}
    Check(ParamCount = 1, 'Supply the repository root for whole compiler companions');
    NativeCompanion(TNyxText(ParamStr(1)));
    {$endif}
    WriteLn('PASS ', GChecks, ' authored optimizer intent and local-lifetime checks');
  except
    on LError: Exception do
    begin
      WriteLn('FAIL / ', LError.Message);
      {$ifndef PAS2JS}DumpExceptionBackTrace(StdErr);{$endif}
      ExitCode := 1;
    end;
  end;
end.
