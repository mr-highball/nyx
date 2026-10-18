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
program nyx_pascal_warning_tests;
{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, Classes, nyx.text, nyx.bytes, nyx.scheduler, nyx.pascal.context,
  nyx.pascal.files, nyx.pascal.lexing, nyx.pascal.translation, nyx.pascal.syntax,
  nyx.pascal.syntax.translated
  {$ifndef PAS2JS}, warning.policy.fixture{$endif};

type
  { Mutable capture-only producer used to qualify detachment, checked alternate
    getters and retirement. It borrows no compiler/source/editor and intentionally
    refuses mutations; successful capture must retain only copied rule values. }
  TSeedWarnings = class(TInterfacedObject, INyxPascalContextWarnings)
  public
    Count: Integer;
    InvalidAt: Integer;
    Rule: TNyxPascalWarningRule;
    destructor Destroy; override;
    function WarningCount: Integer;
    function WarningAt(AIndex: Integer): TNyxPascalWarningRule;
    function WithWarning(const AReference: TNyxPascalWarningRef;
      AAction: TNyxPascalWarningAction;
      AActivity: TNyxPascalKnowledge): INyxPascalCompilerContext;
    function WithWarningRules(const ASource: INyxPascalContextWarnings): INyxPascalCompilerContext;
  end;

var
  GChecks: Integer;
  GProducerDestroyed: Boolean;

procedure Check(AValue: Boolean; const AReason: String);
begin
  Inc(GChecks);

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
end;

destructor TSeedWarnings.Destroy;
begin
  GProducerDestroyed := True;
  inherited Destroy;
end;

function TSeedWarnings.WarningCount: Integer;
begin
  Result := Count;
end;

function TSeedWarnings.WarningAt(AIndex: Integer): TNyxPascalWarningRule;
begin

  if AIndex = InvalidAt then
  begin
    Exit(Default(TNyxPascalWarningRule));
  end;
  Result := Rule;
end;

function TSeedWarnings.WithWarning(const AReference: TNyxPascalWarningRef;
  AAction: TNyxPascalWarningAction;
  AActivity: TNyxPascalKnowledge): INyxPascalCompilerContext;
begin
  Result := nil;
  raise ENyxPascalContext.Create('This qualification producer supplies capture only');
end;

function TSeedWarnings.WithWarningRules(
  const ASource: INyxPascalContextWarnings): INyxPascalCompilerContext;
begin
  Result := nil;
  raise ENyxPascalContext.Create('This qualification producer supplies capture only');
end;

function Facts: INyxPascalCompilerContext;
begin
  Result := NewNyxPascalCompilerContext(ppcFPC,
    TNyxPascalCompilerVersion.Create(3, 3, 1), ppmDelphi).CompleteDefines(True)
    .WithSymbol(NyxPascalDefine('KEEP'), ppdDefined)
    .WithSymbol(NyxPascalDefine('UNKNOWN'), ppdUnknown);
end;

function Translate(const AMain: TNyxText; const APart: TNyxText = ''): INyxPascalTranslation;
begin
  Result := NewNyxPascalTranslation(NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('main.pas'), AMain)
    .WithFile(NyxPascalPhysicalFile('part.inc'), APart), NyxPascalPhysicalFile('main.pas'),
    Facts, TNyxPascalTranslationOptions.Defaults);
end;

function StateAt(const ASource: INyxPascalTranslation; const AName: TNyxText): Integer;
var
  LAt: Integer;
begin
  for LAt := 0 to ASource.Count - 1 do
  begin

    if ASource.Text(LAt) = AName then
    begin
      Exit(ASource.Token(LAt).State);
    end;
  end;
  raise Exception.Create('The original state marker is absent');
end;

procedure CaptureAndIdentity;
var
  LBase: INyxPascalCompilerContext;
  LSaved: INyxPascalCompilerContext;
  LCurrent: INyxPascalCompilerContext;
  LCopy: INyxPascalCompilerContext;
  LProducer: TSeedWarnings;
  LSeed: INyxPascalContextWarnings;
  LRejected: Boolean;
begin
  LBase := Facts;
  LSaved := NyxPascalWithWarning(LBase, NyxPascalWarningNumber(3018), pwaOff);
  LCurrent := NyxPascalWithWarning(LSaved, NyxPascalWarningName('SYMBOL_DEPRECATED'), pwaOn);
  LCurrent := NyxPascalWithWarning(LCurrent, NyxPascalWarningNumber(3018), pwaError, ppkUnknown);
  Check(NyxPascalWarnings(LBase).WarningCount = 0, 'Fluent changes preserve the original context');
  Check((NyxPascalWarnings(LCurrent).WarningCount = 3) and
    (NyxPascalWarnings(LCurrent).WarningAt(1).Reference.Name = TNyxText('symbol_deprecated')),
    'Named groups and repeated numeric selectors retain canonical ordered requests');
  Check((NyxPascalWarnings(LCurrent).WarningAt(2).Action = pwaError) and
    (NyxPascalWarnings(LCurrent).WarningAt(2).Activity = ppkUnknown),
    'Conditional activity does not erase the authored action');
  Check(CaptureNyxPascalCompilerContext(LCurrent).SameFacts(LCurrent),
    'Whole context capture preserves every ordered warning rule');
  Check(not LSaved.SameFacts(LCurrent), 'Warning requests participate in exact context identity');
  LCopy := NyxPascalWithWarning(LBase, NyxPascalWarningName('symbol_deprecated'), pwaOn);
  LCopy := NyxPascalWithWarning(LCopy, NyxPascalWarningNumber(3018), pwaOff);
  Check(not LCopy.SameFacts(NyxPascalWithWarning(LSaved,
    NyxPascalWarningName('symbol_deprecated'), pwaOn)), 'Warning chronology is part of identity');
  Check(NyxPascalWarnings(LCurrent.InMode(ppmObjFPC)).WarningCount = 3,
    'Grammar changes retain authored requests without predicting effective message levels');
  Check(NyxPascalWarnings(LCurrent.UnknownState).WarningCount = 0,
    'Unknown textual effects revoke current warning knowledge');
  LCurrent := LCurrent.WithSymbol(NyxPascalDefine('ADDED'), ppdDefined);
  LCopy := RestoreNyxPascalLocalSettings(LCurrent, LSaved);
  Check((NyxPascalWarnings(LCopy).WarningCount = 1) and
    (LCopy.Symbol(NyxPascalDefine('ADDED')).IsDefined = ppkTrue),
    'Authored settings restoration recovers warning requests while keeping later definitions');
  Check((NyxPascalWarnings(LCurrent).WarningCount = 3) and
    (NyxPascalWarnings(LSaved).WarningCount = 1), 'Restoration preserves both independent inputs');
  LRejected := False;
  try
    NyxPascalWarningNumber(0);
  except
    on ENyxPascalContext do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Invalid numeric selector domains refuse');
  LRejected := False;
  try
    NyxPascalWarningName('not.a.warning');
  except
    on ENyxPascalContext do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'A named warning reference cannot contain expressions or qualification');
  LRejected := False;
  try
    NyxPascalWarningName('future_warning').Number;
  except
    on ENyxPascalContext do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'An open named group cannot invent a numeric message ID');
  LRejected := False;
  try
    NyxPascalWarnings(LBase).WarningAt(0);
  except
    on ENyxPascalContext do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Warning-rule getters enforce their exact bounds');
  LProducer := TSeedWarnings.Create;
  LSeed := LProducer;
  LProducer.Count := 2;
  LProducer.InvalidAt := -1;
  LProducer.Rule := NyxPascalWarnings(LSaved).WarningAt(0);
  LCopy := NyxPascalWarnings(LBase).WithWarningRules(LSeed);
  LProducer.Rule := NyxPascalWarnings(LCurrent).WarningAt(2);
  Check((NyxPascalWarnings(LCopy).WarningCount = 2) and
    (NyxPascalWarnings(LCopy).WarningAt(0).Action = pwaOff),
    'Alternate capture copies values rather than retaining its mutable producer');
  LProducer.InvalidAt := 1;
  LRejected := False;
  try
    LCopy := NyxPascalWarnings(LCopy).WithWarningRules(LSeed);
  except
    on ENyxPascalContext do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (NyxPascalWarnings(LCopy).WarningAt(0).Action = pwaOff),
    'A failed alternate getter preserves the complete caller context');
  LProducer.InvalidAt := -1;
  LProducer.Count := NyxPascalWarningRuleLimit;
  LCurrent := NyxPascalWarnings(LBase).WithWarningRules(LSeed);
  LRejected := False;
  try
    LCurrent := NyxPascalWithWarning(LCurrent, NyxPascalWarningNumber(42), pwaOn);
  except
    on ENyxPascalContext do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (NyxPascalWarnings(LCurrent).WarningCount = NyxPascalWarningRuleLimit),
    'The warning-vector quota refuses atomically at its unchanged bound');
  GProducerDestroyed := False;
  LSeed := nil;
  Check(GProducerDestroyed and (NyxPascalWarnings(LCopy).WarningCount = 2),
    'A retired producer leaves no reverse lease in captured warning values');
end;

procedure SourceOwnership;
var
  LSource: INyxPascalTranslation;
  LReceipts: INyxPascalWarningDirectives;
  LPrefix: INyxPascalTranslation;
  LPrefixReceipts: INyxPascalWarningDirectives;
  LSettings: INyxPascalTranslationSettings;
  LInside: Integer;
  LAfter: Integer;
  LReceipt: TNyxPascalWarningDirective;
  LRejected: Boolean;
begin
  LSource := Translate('unit Main; interface {$WARN 3018 OFF}{$PUSH}{$I part.inc}' +
    'var InsideMarker:Integer; {$POP}var AfterMarker:Integer; implementation end.',
    '{$WARN SYMBOL_DEPRECATED ERROR}{$define ADDED}');
  Supports(LSource, INyxPascalWarningDirectives, LReceipts);
  Supports(LSource, INyxPascalTranslationSettings, LSettings);
  LInside := StateAt(LSource, 'InsideMarker');
  LAfter := StateAt(LSource, 'AfterMarker');
  Check(LSource.Resolved and (LReceipts.WarningDirectiveCount = 2),
    'Cross-include warning requests retain complete source classification');
  Check((NyxPascalWarnings(LSource.State(LInside)).WarningCount = 2) and
    (NyxPascalWarnings(LSource.State(LAfter)).WarningCount = 1),
    'PUSH/POP restores authored warning-request scope across textual inclusion');
  Check((LSettings.SettingsDepth(LInside) = 1) and
    (NyxPascalWarnings(LSettings.SavedSettings(LInside, 0)).WarningCount = 1),
    'Historical frames retain their exact saved warning scope');
  Check(LSource.State(LAfter).Symbol(NyxPascalDefine('ADDED')).IsDefined = ppkTrue,
    'Warning restoration does not rewind later symbol definitions');
  LReceipt := LReceipts.WarningDirectiveAt(1);
  Check((LReceipt.Origin.FileReference.Name = TNyxText('part.inc')) and
    (LReceipt.Rule.Reference.Kind = pwrName) and (LReceipt.Rule.Action = pwaError) and
    (LReceipt.Rule.Activity = ppkTrue), 'The included receipt retains typed action and exact source owner');
  Check((NyxPascalOccurrenceLexical(LSource, LReceipt.Origin.Occurrence).Slice(LReceipt.Origin.Span) =
      TNyxText('{$WARN SYMBOL_DEPRECATED ERROR}')) and
    (NyxPascalWarnings(LSource.State(LReceipt.State)).WarningCount = 1),
    'The exact physical directive and its pre-directive context remain independent');
  LPrefix := NewNyxPascalTranslationPrefix(LSource, LReceipt.Origin.PrefixCount,
    TNyxPascalTranslationOptions.Defaults);
  Supports(LPrefix, INyxPascalWarningDirectives, LPrefixReceipts);
  Check((LPrefixReceipts.WarningDirectiveCount = 1) and
    (LPrefixReceipts.WarningDirectiveAt(0).Rule.Reference.Number = 3018),
    'A prefix excludes its current directive and every later warning receipt');
  LRejected := False;
  try
    LReceipts.WarningDirectiveAt(2);
  except
    on ENyxPascalContext do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Directive getters enforce their exact source bounds');
  LSource := nil;
  LReceipts := nil;
  Check(LPrefixReceipts.WarningDirectiveAt(0).Origin.FileReference.Name = TNyxText('main.pas'),
    'A retained prefix owns its source without a visitor/editor back reference');
  LSource := Translate('unit Main; interface {$I part.inc}{$I part.inc} implementation end.',
    '{$WARN future_warning-}');
  Supports(LSource, INyxPascalWarningDirectives, LReceipts);
  Check(LSource.Resolved and (LReceipts.WarningDirectiveCount = 2) and
    (LReceipts.WarningDirectiveAt(0).Origin.Occurrence <> LReceipts.WarningDirectiveAt(1).Origin.Occurrence),
    'Repeated includes retain distinct authored warning occurrences');
  LSource := Translate('unit Main; interface {$ifdef UNKNOWN}{$WARN 42 ON}{$endif}' +
    'var Marker:Integer; implementation end.');
  Check(not LSource.Resolved and
    (NyxPascalWarnings(LSource.State(StateAt(LSource, 'Marker'))).WarningAt(0).Activity = ppkUnknown) and
    (LSource.State(StateAt(LSource, 'Marker')).Symbol(NyxPascalDefine('KEEP')).IsDefined = ppkTrue),
    'Conditional warning intent stays uncertain without revoking unrelated known facts');
  LSource := Translate('unit Main; interface {$ifdef ABSENT}{$WARN 42 BROKEN}{$endif} implementation end.');
  Supports(LSource, INyxPascalWarningDirectives, LReceipts);
  Check(LSource.Resolved and (LReceipts.WarningDirectiveCount = 0),
    'Inactive malformed warning requests have no source-context effect');
  LSource := Translate(TNyxText('unit Main; // 🚀 dedicated physical-origin input') + #10 +
    'interface {$WARN 0003018+ Keep this explanatory text}var Marker:Integer; implementation end.');
  Supports(LSource, INyxPascalWarningDirectives, LReceipts);
  LReceipt := LReceipts.WarningDirectiveAt(0);
  Check(LSource.Resolved and (LReceipt.Rule.Reference.Number = 3018) and
    (LReceipt.Rule.Action = pwaOn) and
    (NyxPascalOccurrenceLexical(LSource, LReceipt.Origin.Occurrence).Slice(LReceipt.Origin.Span) =
      TNyxText('{$WARN 0003018+ Keep this explanatory text}')),
    'Numeric shorthand, explanatory text and supplementary scalar origins stay exact');
end;

procedure Refusals;
var
  LSource: INyxPascalTranslation;
  LPartial: INyxPascalTranslation;
  LBase: INyxPascalCompilerContext;
  LProducer: TSeedWarnings;
  LSeed: INyxPascalContextWarnings;
  LCancel: INyxCancellationScope;
  LRejected: Boolean;
begin
  LSource := Translate('unit Main; interface {$WARN 42 DEFAULT}var Marker:Integer; implementation end.');
  Check(not LSource.Resolved and (LSource.Diagnostic(0).Issue = ptiMalformedDirective),
    'Unsupported action spellings retain a visible malformed-directive boundary');
  LSource := Translate('unit Main; interface {$WARN 2147483648 OFF} implementation end.');
  Check(not LSource.Resolved and (LSource.Diagnostic(0).Issue = ptiMalformedDirective),
    'An out-of-domain numeric selector refuses without overflowing');
  LSource := NewNyxPascalTranslation(NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('wrong.pas'), 'unit Wrong; interface {$WARN 42 OFF} implementation end.'),
    NyxPascalPhysicalFile('wrong.pas'), NewNyxPascalCompilerContext(ppcPas2JS,
      TNyxPascalCompilerVersion.Create(3, 3, 1), ppmDelphi), TNyxPascalTranslationOptions.Defaults);
  Check(not LSource.Resolved and (LSource.Diagnostic(0).Issue = ptiUnsupportedDirective),
    'A compiler-family mismatch remains explicit context uncertainty');
  LSource := Translate('unit Main; interface {$WARN 42 OFF}{$NYX_UNKNOWN_POLICY ON}' +
    'var Marker:Integer; implementation end.');
  Check(not LSource.Resolved and
    (NyxPascalWarnings(LSource.State(StateAt(LSource, 'Marker'))).WarningCount = 0),
    'An unknown later textual effect revokes warning knowledge');
  LProducer := TSeedWarnings.Create;
  LSeed := LProducer;
  LProducer.Count := NyxPascalWarningRuleLimit;
  LProducer.InvalidAt := -1;
  LProducer.Rule := NyxPascalWarnings(NyxPascalWithWarning(Facts,
    NyxPascalWarningNumber(42), pwaOff)).WarningAt(0);
  LBase := NyxPascalWarnings(Facts).WithWarningRules(LSeed);
  LPartial := nil;
  LRejected := False;
  try
    LPartial := NewNyxPascalTranslation(NewNyxPascalFileCatalog(pfcExact)
      .WithFile(NyxPascalPhysicalFile('limit.pas'), '{$WARN 43 ON}'), NyxPascalPhysicalFile('limit.pas'),
      LBase, TNyxPascalTranslationOptions.Defaults);
  except
    on ENyxPascalTranslationLimit do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LPartial = nil), 'Warning-construction exhaustion publishes no partial translation');
  LCancel := NewNyxCancellationScope;
  LCancel.Cancel;
  LRejected := False;
  try
    LPartial := NewNyxPascalTranslation(NewNyxPascalFileCatalog(pfcExact)
      .WithFile(NyxPascalPhysicalFile('cancel.pas'), '{$WARN 43 ON}'), NyxPascalPhysicalFile('cancel.pas'),
      Facts, TNyxPascalTranslationOptions.Defaults, LCancel);
  except
    on ENyxPascalAnalysisCancelled do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LPartial = nil), 'Cancellation publishes no partial warning translation');
end;

{$ifndef PAS2JS}
function OriginalFile(const APath: TNyxText): TNyxText;
var
  LStream: TFileStream;
  LBytes: TNyxBytes;
begin
  LStream := TFileStream.Create(String(APath), fmOpenRead or fmShareDenyWrite);
  try
    Check((LStream.Size > 0) and (LStream.Size < 1024 * 1024), 'The original compiler packet is bounded');
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
  LReceipts: INyxPascalWarningDirectives;
begin
  Check(WarningFixtureValue = 42, 'The whole original native unit/include executes its authored behavior');
  LMain := OriginalFile(ARoot + '/tests/warning.policy.fixture.pas');
  LPart := OriginalFile(ARoot + '/tests/warning.policy.fixture.inc');
  LSource := NewNyxPascalTranslation(NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('warning.policy.fixture.pas'), LMain)
    .WithFile(NyxPascalPhysicalFile('warning.policy.fixture.inc'), LPart),
    NyxPascalPhysicalFile('warning.policy.fixture.pas'), Facts, TNyxPascalTranslationOptions.Defaults);
  LView := NewNyxPascalTranslatedSyntax(LSource, TNyxPascalSyntaxOptions.Defaults);
  Supports(LSource, INyxPascalWarningDirectives, LReceipts);
  Check(LView.CompleteTranslation and (LView.Syntax.DiagnosticCount = 0),
    'The whole compiled companion retains complete source/include declaration syntax');
  Check((LSource.Files.TextAt(0) = LMain) and (LSource.Files.TextAt(1) = LPart),
    'Source inspection keeps every original companion scalar');
  Check((LReceipts.WarningDirectiveCount = 2) and
    (LReceipts.WarningDirectiveAt(0).Origin.FileReference.Name = TNyxText('warning.policy.fixture.inc')) and
    (LReceipts.WarningDirectiveAt(1).Rule.Reference.Number = 3018),
    'Both native warning requests keep their typed selector and exact included source owner');
end;
{$endif}

begin
  try
    CaptureAndIdentity;
    SourceOwnership;
    Refusals;
    {$ifndef PAS2JS}
    Check(ParamCount = 1, 'Supply repository root for the whole native compiler companion');
    NativeCompanion(TNyxText(ParamStr(1)));
    {$endif}
    WriteLn('PASS ', GChecks, ' ordered warning-policy ownership and source checks');
  except
    on LError: Exception do
    begin
      WriteLn('FAIL / ', LError.Message);
      {$ifndef PAS2JS}DumpExceptionBackTrace(StdErr);{$endif}
      ExitCode := 1;
    end;
  end;
end.
