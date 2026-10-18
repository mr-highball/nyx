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
program nyx_pascal_alignment_tests;
{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, Classes, nyx.text, nyx.bytes, nyx.scheduler, nyx.pascal.context,
  nyx.pascal.files, nyx.pascal.lexing, nyx.pascal.translation,
  nyx.pascal.syntax, nyx.pascal.syntax.translated
  {$ifndef PAS2JS}, code.alignment.fixture{$endif};

type
  { Mutable capture-only producer. Qualification changes/retires the producer
    after freezing; no compiler, source or editor is retained. InvalidAt supplies
    a deliberately empty request to exercise atomic alternate-getter refusal. }
  TAlignmentSeed = class(TInterfacedObject, INyxPascalContextCodeAlignments)
  public
    Count: Integer;
    InvalidAt: Integer;
    Rule: TNyxPascalCodeAlignmentRule;
    function CodeAlignmentCount: Integer;
    function CodeAlignmentAt(AIndex: Integer): TNyxPascalCodeAlignmentRule;
    function WithCodeAlignment(const ARequest: TNyxPascalCodeAlignmentRequest;
      AActivity: TNyxPascalKnowledge): INyxPascalCompilerContext;
    function WithCodeAlignmentRules(
      const ASource: INyxPascalContextCodeAlignments): INyxPascalCompilerContext;
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

function TAlignmentSeed.CodeAlignmentCount: Integer;
begin
  Result := Count;
end;

function TAlignmentSeed.CodeAlignmentAt(AIndex: Integer): TNyxPascalCodeAlignmentRule;
begin

  if AIndex = InvalidAt then
  begin
    Exit(Default(TNyxPascalCodeAlignmentRule));
  end;
  Result := Rule;
end;

function TAlignmentSeed.WithCodeAlignment(const ARequest: TNyxPascalCodeAlignmentRequest;
  AActivity: TNyxPascalKnowledge): INyxPascalCompilerContext;
begin
  Result := nil;
  raise ENyxPascalContext.Create('This seed supplies capture only');
end;

function TAlignmentSeed.WithCodeAlignmentRules(
  const ASource: INyxPascalContextCodeAlignments): INyxPascalCompilerContext;
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
  raise Exception.Create('A state marker is absent');
end;

procedure ContextOwnership;
var
  LRequest: TNyxPascalCodeAlignmentRequest;
  LExtended: TNyxPascalCodeAlignmentRequest;
  LBase: INyxPascalCompilerContext;
  LSaved: INyxPascalCompilerContext;
  LCurrent: INyxPascalCompilerContext;
  LCopy: INyxPascalCompilerContext;
  LProducer: TAlignmentSeed;
  LSeed: INyxPascalContextCodeAlignments;
  LRejected: Boolean;
  LAt: Integer;
begin
  LRequest := Default(TNyxPascalCodeAlignmentRequest).Add(pcaConstantMinimum, 16);
  LExtended := LRequest.Add(pcaConstantMaximum, 8).Add(pcaConstantMinimum, 4);
  Check((LRequest.Count = 1) and (LExtended.Count = 3), 'Fluent request vectors are independent');
  Check((LExtended.Assignment(0).Value = 16) and
    (LExtended.Assignment(2).Value = 4), 'Repeated subjects retain assignment chronology');
  LBase := Facts;
  LSaved := NyxPascalWithCodeAlignment(LBase, LRequest);
  LCurrent := NyxPascalWithCodeAlignment(LSaved, LExtended, ppkUnknown);
  Check(NyxPascalCodeAlignments(LBase).CodeAlignmentCount = 0, 'Append preserves the base context');
  Check((NyxPascalCodeAlignments(LCurrent).CodeAlignmentCount = 2) and
    (NyxPascalCodeAlignments(LCurrent).CodeAlignmentAt(1).Activity = ppkUnknown),
    'Separate directive groups retain explicit uncertain activity');
  Check(CaptureNyxPascalCompilerContext(LCurrent).SameFacts(LCurrent),
    'Whole context capture preserves exact request groups');
  Check(not LCurrent.SameFacts(LSaved), 'Requests participate in context identity');
  Check(not LSaved.SameFacts(NyxPascalWithCodeAlignment(LBase,
    Default(TNyxPascalCodeAlignmentRequest).Add(pcaConstantMaximum, 16))),
    'Equal numeric values do not collapse distinct subjects');
  Check(NyxPascalCodeAlignments(LCurrent.InMode(ppmObjFPC)).CodeAlignmentCount = 2,
    'Mode changes retain code-alignment requests');
  Check(NyxPascalCodeAlignments(LCurrent.UnknownState).CodeAlignmentCount = 0,
    'Unknown textual effects revoke current request knowledge');
  LCopy := RestoreNyxPascalLocalSettings(LCurrent.WithSymbol(NyxPascalDefine('KEEP'), ppdDefined), LSaved);
  Check((NyxPascalCodeAlignments(LCopy).CodeAlignmentCount = 1) and
    (LCopy.Symbol(NyxPascalDefine('KEEP')).IsDefined = ppkTrue),
    'Local restoration recovers groups while preserving later symbol facts');
  Check(NyxPascalCodeAlignments(LCurrent).CodeAlignmentCount = 2,
    'Restoration leaves the prior context independent');
  LProducer := TAlignmentSeed.Create;
  LSeed := LProducer;
  LProducer.Count := 2;
  LProducer.InvalidAt := -1;
  LProducer.Rule := NyxPascalCodeAlignments(LSaved).CodeAlignmentAt(0);
  LCopy := NyxPascalCodeAlignments(LBase).WithCodeAlignmentRules(LSeed);
  LProducer.Rule := NyxPascalCodeAlignments(LCurrent).CodeAlignmentAt(1);
  Check(NyxPascalCodeAlignments(LCopy).CodeAlignmentAt(0).Request.SameRequest(LRequest),
    'Alternate producer values are frozen independently');
  LProducer.InvalidAt := 1;
  LRejected := False;
  try
    LCopy := NyxPascalCodeAlignments(LBase).WithCodeAlignmentRules(LSeed);
  except
    on ENyxPascalContext do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (NyxPascalCodeAlignments(LCopy).CodeAlignmentCount = 2),
    'A later invalid alternate getter preserves the existing publication');
  LProducer.Count := NyxPascalCodeAlignmentRuleLimit + 1;
  LRejected := False;
  try
    NyxPascalCodeAlignments(LBase).WithCodeAlignmentRules(LSeed);
  except
    on ENyxPascalContext do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Excess alternate groups refuse before enumeration');
  LSeed := nil;
  Check(NyxPascalCodeAlignments(LCopy).CodeAlignmentCount = 2,
    'The frozen context survives producer retirement');
  LRejected := False;
  try
    LRequest.Add(pcaProcedure, 3);
  except
    on ENyxPascalContext do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Non-power-of-two code-label alignment refuses');
  LExtended := Default(TNyxPascalCodeAlignmentRequest).Add(pcaProcedure, 0)
    .Add(pcaJump, 256).Add(pcaLocalMinimum, Low(Integer)).Add(pcaLocalMaximum, High(Integer));
  Check((LExtended.Assignment(0).Value = 0) and
    (LExtended.Assignment(2).Value = Low(Integer)), 'Zero and signed-32 requests remain exact');
  LExtended := Default(TNyxPascalCodeAlignmentRequest);
  for LAt := 1 to NyxPascalCodeAlignmentAssignmentLimit do
  begin
    LExtended := LExtended.Add(pcaConstantMinimum, LAt);
  end;
  LRejected := False;
  try
    LExtended := LExtended.Add(pcaConstantMinimum, NyxPascalCodeAlignmentAssignmentLimit + 1);
  except
    on ENyxPascalContext do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LExtended.Count = NyxPascalCodeAlignmentAssignmentLimit),
    'Assignment exhaustion preserves the complete prior value');
end;

procedure SourceLifetime;
var
  LSource: INyxPascalTranslation;
  LPrefix: INyxPascalTranslation;
  LReceipts: INyxPascalCodeAlignmentDirectives;
  LPrefixReceipts: INyxPascalCodeAlignmentDirectives;
  LReceipt: TNyxPascalCodeAlignmentDirective;
  LAt: Integer;
begin
  LSource := Translate('unit Main; {English with a Unicode check: 😀}' +
    '{$codealign CONSTMIN=16}{$push}{$i part.inc}{$mode objfpc}{$i part.inc}' +
    'interface var BeforePop:Integer;{$pop}var AfterPop:Integer;{$ifdef MAYBE}' +
    '{$codealign jump=4}{$endif}{$ifdef ABSENT}{$codealign broken}{$endif}' +
    'var AfterMaybe:Integer; implementation end.', '{$codealign constmax=8,constmin=4,proc=0}');
  Supports(LSource, INyxPascalCodeAlignmentDirectives, LReceipts);
  Check(LReceipts.CodeAlignmentDirectiveCount = 4, 'Inactive requests stay absent; repeated includes retain rows');
  Check(NyxPascalCodeAlignments(LSource.State(LSource.Token(Marker(LSource, 'BeforePop')).State))
    .CodeAlignmentCount = 3, 'Included request groups survive mode changes before POP');
  Check(NyxPascalCodeAlignments(LSource.State(LSource.Token(Marker(LSource, 'AfterPop')).State))
    .CodeAlignmentCount = 1, 'POP restores the original request vector');
  Check(NyxPascalCodeAlignments(LSource.State(LSource.Token(Marker(LSource, 'AfterMaybe')).State))
    .CodeAlignmentAt(1).Activity = ppkUnknown, 'Unknown conditional writes retain their activity');
  Check(not LSource.Resolved and (LSource.DiagnosticCount = 1) and
    (LSource.Diagnostic(0).Issue = ptiUnknownCondition), 'Supported intents do not hide unknown conditions');
  LReceipt := LReceipts.CodeAlignmentDirectiveAt(2);
  Check((LReceipt.Origin.FileReference.Name = TNyxText('part.inc')) and
    (LReceipt.Origin.Occurrence <> LReceipts.CodeAlignmentDirectiveAt(1).Origin.Occurrence),
    'Repeated includes keep their own physical occurrences');
  Check(NyxPascalOccurrenceLexical(LSource, LReceipt.Origin.Occurrence).Slice(LReceipt.Origin.Span) =
    TNyxText('{$codealign constmax=8,constmin=4,proc=0}'), 'The exact physical directive span is navigable');
  Check(NyxPascalCodeAlignments(LSource.State(LReceipt.State)).CodeAlignmentCount = 2,
    'Receipt state precedes its own directive');
  LPrefix := NewNyxPascalTranslationPrefix(LSource, LReceipt.Origin.PrefixCount,
    TNyxPascalTranslationOptions.Defaults);
  Supports(LPrefix, INyxPascalCodeAlignmentDirectives, LPrefixReceipts);
  Check(LPrefixReceipts.CodeAlignmentDirectiveCount = 2, 'A prefix excludes its boundary directive');
  LAt := Marker(LSource, 'BeforePop');
  LPrefix := NewNyxPascalTranslationPrefix(LSource, LAt, TNyxPascalTranslationOptions.Defaults);
  Supports(LPrefix, INyxPascalCodeAlignmentDirectives, LPrefixReceipts);
  LSource := nil;
  LReceipts := nil;
  Check((LPrefixReceipts.CodeAlignmentDirectiveCount = 3) and
    (LPrefixReceipts.CodeAlignmentDirectiveAt(2).Rule.Request.Count = 3),
    'Visited immutable receipts survive their original caller');
end;

procedure Refusals;
const
  Bad: array[0..7] of String = ('proc=3', 'constmin=2147483648', 'constmin=-2147483649',
    'maxcrecord=8', 'constmin=16,jump=3', 'constmin=16,', 'constmin=$10', '');
var
  LSource: INyxPascalTranslation;
  LReceipts: INyxPascalCodeAlignmentDirectives;
  LCancel: INyxCancellationScope;
  LProducer: TAlignmentSeed;
  LSeed: INyxPascalContextCodeAlignments;
  LFull: INyxPascalCompilerContext;
  LRejected: Boolean;
  LAt: Integer;
begin
  for LAt := Low(Bad) to High(Bad) do
  begin
    LSource := Translate('{$codealign constmin=4}{$codealign ' + TNyxText(Bad[LAt]) + '}var Marker:Integer;');
    Supports(LSource, INyxPascalCodeAlignmentDirectives, LReceipts);
    Check(not LSource.Resolved and (LSource.Diagnostic(0).Issue = ptiMalformedDirective) and
      (LReceipts.CodeAlignmentDirectiveCount = 1) and
      (NyxPascalCodeAlignments(LSource.State(LSource.Token(Marker(LSource, 'Marker')).State))
        .CodeAlignmentCount = 0), 'Malformed/unsupported input publishes no partial group and revokes knowledge');
  end;
  LSource := Translate('{$codealign constmin=-2147483648,constmax=2147483647,proc=0}var Exact:Integer;');
  Supports(LSource, INyxPascalCodeAlignmentDirectives, LReceipts);
  Check(LSource.Resolved and (LReceipts.CodeAlignmentDirectiveAt(0).Rule.Request.Assignment(0).Value =
    Low(Integer)), 'Signed-decimal parsing preserves both exact signed-32 endpoints');
  LSource := NewNyxPascalTranslation(NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('wrong.pas'), '{$codealign constmin=16}'),
    NyxPascalPhysicalFile('wrong.pas'), NewNyxPascalCompilerContext(ppcPas2JS,
      TNyxPascalCompilerVersion.Create(3, 3, 1), ppmDelphi), TNyxPascalTranslationOptions.Defaults);
  Check(not LSource.Resolved and (LSource.Diagnostic(0).Issue = ptiUnsupportedDirective),
    'An FPC request never becomes an invented pas2js compiler fact');
  LProducer := TAlignmentSeed.Create;
  LSeed := LProducer;
  LProducer.Count := NyxPascalCodeAlignmentRuleLimit;
  LProducer.InvalidAt := -1;
  LProducer.Rule := NyxPascalCodeAlignments(NyxPascalWithCodeAlignment(Facts,
    Default(TNyxPascalCodeAlignmentRequest).Add(pcaConstantMinimum, 16))).CodeAlignmentAt(0);
  LFull := NyxPascalCodeAlignments(Facts).WithCodeAlignmentRules(LSeed);
  LSource := nil;
  LRejected := False;
  try
    LSource := NewNyxPascalTranslation(NewNyxPascalFileCatalog(pfcExact)
      .WithFile(NyxPascalPhysicalFile('full.pas'), '{$codealign proc=16}'),
      NyxPascalPhysicalFile('full.pas'), LFull, TNyxPascalTranslationOptions.Defaults);
  except
    on ENyxPascalTranslationLimit do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LSource = nil), 'Request exhaustion publishes no partial translation');
  LSource := nil;
  LCancel := NewNyxCancellationScope;
  LCancel.Cancel;
  LRejected := False;
  try
    LSource := NewNyxPascalTranslation(NewNyxPascalFileCatalog(pfcExact)
      .WithFile(NyxPascalPhysicalFile('cancel.pas'), '{$codealign proc=16}'),
      NyxPascalPhysicalFile('cancel.pas'), Facts, TNyxPascalTranslationOptions.Defaults, LCancel);
  except
    on ENyxPascalAnalysisCancelled do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LSource = nil), 'Cancellation exposes no partial source publication');
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
  LReceipts: INyxPascalCodeAlignmentDirectives;
begin
  Check(AlignmentFixtureValue(20) = 42, 'The compiled whole native companion executes ordinary Pascal');
  LMain := OriginalFile(ARoot + '/tests/code.alignment.fixture.pas');
  LPart := OriginalFile(ARoot + '/tests/code.alignment.fixture.inc');
  LSource := NewNyxPascalTranslation(NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('code.alignment.fixture.pas'), LMain)
    .WithFile(NyxPascalPhysicalFile('code.alignment.fixture.inc'), LPart),
    NyxPascalPhysicalFile('code.alignment.fixture.pas'), Facts, TNyxPascalTranslationOptions.Defaults);
  LView := NewNyxPascalTranslatedSyntax(LSource, TNyxPascalSyntaxOptions.Defaults);
  Supports(LSource, INyxPascalCodeAlignmentDirectives, LReceipts);
  Check(LSource.Resolved and (LView.Syntax.DiagnosticCount = 0),
    'The exact compiled unit/include retains complete supported directive syntax');
  Check((LReceipts.CodeAlignmentDirectiveCount = 4) and
    (LReceipts.CodeAlignmentDirectiveAt(0).Rule.Request.Count = 11),
    'All eleven compiler selectors retain the original complete group');
  Check((LReceipts.CodeAlignmentDirectiveAt(1).Origin.FileReference.Name =
    TNyxText('code.alignment.fixture.inc')) and
    (LSource.Files.TextAt(0) = LMain) and (LSource.Files.TextAt(1) = LPart),
    'Whole compiler companions keep every original source/include scalar');
end;
{$endif}

begin
  try
    ContextOwnership;
    SourceLifetime;
    Refusals;
    {$ifndef PAS2JS}
    Check(ParamCount = 1, 'Supply the repository root for whole compiled companions');
    NativeCompanion(TNyxText(ParamStr(1)));
    {$endif}
    WriteLn('PASS ', GChecks, ' authored code-alignment ownership and lifetime checks');
  except
    on LError: Exception do
    begin
      WriteLn('FAIL / ', LError.Message);
      {$ifndef PAS2JS}DumpExceptionBackTrace(StdErr);{$endif}
      ExitCode := 1;
    end;
  end;
end.
