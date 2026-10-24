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
program nyx_collection_text_layout_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.collections.presentation,
  nyx.collections.text.layout
  {$ifdef PAS2JS}, Web{$endif};

type
  { Deterministic borrowed metric qualifies layout/control flow only. It can
    simulate wider glyphs, nonmonotone shaping or a real receiver failure;
    none of these substituted extents claims a platform font measurement. }
  TMetric = class
  public
    Calls: Integer;
    WideW: Boolean;
    Fail: Boolean;
    Invalid: Boolean;
    Shaped: Boolean;
    function Measure(const AText: TNyxText): Double;
  end;

const
  CPath: TNyxText = 'a🌙bcd';
  CExact: TNyxText = 'x' + #0 + 'y';
  CCombining: TNyxText = 'éx';
  CJoiners: TNyxText = '👩‍👩x';
  CFlag: TNyxText = '🇨🇦x';
  CShaped: TNyxText = 'qfíx';

var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AMessage: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(AMessage);
  end;
  Inc(GChecks);
end;

function TMetric.Measure(const AText: TNyxText): Double;
var
  LIndex: Integer;
  LScalar: Integer;
begin
  Inc(Calls);

  if Fail then
  begin
    raise Exception.Create('exact metric receiver failure');
  end;

  if Invalid then
  begin
    Exit(-1);
  end;

  if Shaped then
  begin

    if AText = 'qfi' then
    begin
      Exit(2);
    end;

    if AText = 'qf' then
    begin
      Exit(3);
    end;
  end;
  Result := 0;
  LIndex := 1;
  while NyxNextScalar(AText, LIndex, LScalar) do
  begin
    Result := Result + 1;

    if WideW and (LScalar = Ord('W')) then
    begin
      Result := Result + 1;
    end;
  end;
end;

procedure Run;
var
  LMetric: TMetric;
  LLayout: TNyxTableTextLayout;
  LCopy: TNyxTableTextLayout;
  LPolicy: TNyxColumnTextPolicy;
  LLimits: TNyxTableTextWorkLimits;
  LRefused: Boolean;
  LBefore: Integer;
  LNatural: TNyxTableTextNaturalWidth;
  I: Integer;
  LJoined: TNyxText;
  LFits: Boolean;
begin
  LMetric := TMetric.Create;
  try
    LLimits := TNyxTableTextWorkLimits.Defaults;
    LPolicy := NyxColumnText;
    LLayout := LayoutNyxTableText('abc', LPolicy, 3, LLimits,
      {$ifdef PAS2JS}@{$endif}LMetric.Measure);
    Check((LLayout.Outcome = ntloComplete) and (LLayout.Count = 1) and
      (LLayout.LineAt(0) = 'abc'), 'exact-width single line fits without an invented overflow');
    LLayout := LayoutNyxTableText('abcde', LPolicy, 3, LLimits,
      {$ifdef PAS2JS}@{$endif}LMetric.Measure);
    Check((LLayout.Outcome = ntloClipped) and (LLayout.LineAt(0) = 'abc'),
      'explicit clip reports its visual loss independently of the original value');
    LLayout := LayoutNyxTableText('abcde', LPolicy.Ellipsis, 3, LLimits,
      {$ifdef PAS2JS}@{$endif}LMetric.Measure);
    Check((LLayout.Outcome = ntloEllipsized) and (LLayout.LineAt(0) = TNyxText('ab…')),
      'ellipsis reserves the actual metric width of its visual marker');
    LLayout := LayoutNyxTableText('abc', LPolicy.Ellipsis, 0, LLimits,
      {$ifdef PAS2JS}@{$endif}LMetric.Measure);
    Check((LLayout.Outcome = ntloClipped) and (LLayout.LineAt(0) = ''),
      'a zero clip never pretends that an ellipsis glyph fits');
    LPolicy := NyxColumnText.Wrap(8);
    LLayout := LayoutNyxTableText(CPath, LPolicy, 2, LLimits,
      {$ifdef PAS2JS}@{$endif}LMetric.Measure);
    Check((LLayout.Outcome = ntloComplete) and (LLayout.Count = 3) and
      (LLayout.LineAt(0) = TNyxText('a🌙')) and (LLayout.LineAt(1) = 'bc') and
      (LLayout.LineAt(2) = 'd'), 'unspaced paths wrap at complete supplementary scalar boundaries');
    Check(LLayout.LineAt(0) + LLayout.LineAt(1) + LLayout.LineAt(2) = CPath,
      'soft wrapping preserves every original path byte/scalar in order');
    LLayout := LayoutNyxTableText(CCombining, LPolicy, 1, LLimits,
      {$ifdef PAS2JS}@{$endif}LMetric.Measure);
    Check((LLayout.Count = 2) and (LLayout.LineAt(0) = TNyxText('é')) and
      (LLayout.LineAt(1) = 'x'), 'combining sequence stays together even when this metric makes it wider than the clip');
    LLayout := LayoutNyxTableText(CJoiners, LPolicy, 2, LLimits,
      {$ifdef PAS2JS}@{$endif}LMetric.Measure);
    Check((LLayout.Count = 2) and (LLayout.LineAt(0) = TNyxText('👩‍👩')) and
      (LLayout.LineAt(1) = 'x'), 'emoji joiners remain with both surrounding scalars');
    LLayout := LayoutNyxTableText(CFlag, LPolicy, 1, LLimits,
      {$ifdef PAS2JS}@{$endif}LMetric.Measure);
    Check((LLayout.Count = 2) and (LLayout.LineAt(0) = TNyxText('🇨🇦')) and
      (LLayout.LineAt(1) = 'x'), 'a regional-indicator pair is never split into unrelated flags');
    LMetric.Shaped := True;
    LLayout := LayoutNyxTableText(CShaped, LPolicy, 2, LLimits,
      {$ifdef PAS2JS}@{$endif}LMetric.Measure);
    LJoined := '';
    LFits := True;
    for I := 0 to LLayout.Count - 1 do
    begin
      LJoined := LJoined + LLayout.LineAt(I);
      LFits := LFits and (LMetric.Measure(LLayout.LineAt(I)) <= 2);
    end;
    Check((LLayout.Outcome = ntloComplete) and LFits and (LJoined = CShaped),
      'every complete line rechecks the exact safe prefix when shorter shaped text can be wider');
    LMetric.Shaped := False;
    LLayout := LayoutNyxTableText('alpha' + #13#10#13#10 + 'beta' + #10,
      LPolicy, 10, LLimits, {$ifdef PAS2JS}@{$endif}LMetric.Measure);
    Check((LLayout.Outcome = ntloComplete) and (LLayout.Count = 4) and
      (LLayout.LineAt(0) = 'alpha') and (LLayout.LineAt(1) = '') and
      (LLayout.LineAt(2) = 'beta') and (LLayout.LineAt(3) = ''),
      'hard CRLF and final LF retain actual empty visible lines');
    LLayout := LayoutNyxTableText('a b c', LPolicy, 3, LLimits,
      {$ifdef PAS2JS}@{$endif}LMetric.Measure);
    Check((LLayout.Count = 2) and (LLayout.LineAt(0) = 'a ') and
      (LLayout.LineAt(1) = 'b c'), 'soft word boundaries retain whitespace without trimming data');
    LLayout := LayoutNyxTableText('abcdefghi', LPolicy.Wrap(2), 3, LLimits,
      {$ifdef PAS2JS}@{$endif}LMetric.Measure);
    Check((LLayout.Count = 2) and (LLayout.LineAt(0) = 'abc') and
      (LLayout.LineAt(1) = TNyxText('de…')) and (LLayout.Outcome = ntloEllipsized),
      'the final allowed wrap line visibly reports remaining content');
    LLayout := LayoutNyxTableText('first' + #10 + 'second', LPolicy.Wrap(1), 10,
      LLimits, {$ifdef PAS2JS}@{$endif}LMetric.Measure);
    Check((LLayout.Count = 1) and (LLayout.LineAt(0) = TNyxText('first…')) and
      (LLayout.Outcome = ntloEllipsized), 'a hidden hard-line tail also receives a visible marker');
    LNatural := MeasureNaturalNyxTableText('one' + #13#10 + 'widest line' + #10,
      LPolicy.Wrap(1), LLimits, {$ifdef PAS2JS}@{$endif}LMetric.Measure);
    Check((LNatural.Admission = nttaReady) and (LNatural.Width = 11) and
      (LNatural.Measurements = 3), 'Fit measures the widest complete hard line including an empty tail, independent of visual clamping');
    LNatural := MeasureNaturalNyxTableText('one' + #10 + 'two', LPolicy,
      LLimits.Measurements(1), {$ifdef PAS2JS}@{$endif}LMetric.Measure);
    Check((LNatural.Admission = nttaMeasurementLimit) and (LNatural.Width = 0) and
      (LNatural.Measurements = 1), 'full-text Fit refuses partial work rather than authorizing a prefix width');
    LBefore := LMetric.Calls;
    LNatural := MeasureNaturalNyxTableText(CPath, LPolicy, LLimits.UTF8Bytes(3),
      {$ifdef PAS2JS}@{$endif}LMetric.Measure);
    Check((LNatural.Admission = nttaTextLimit) and (LMetric.Calls = LBefore),
      'paint and Fit use the same exact upfront UTF-8 admission');
    LLayout := UnavailableNyxTableTextLayout(nttaHostMetrics,
      'unsupported actual font style');
    Check((LLayout.Outcome = ntloUnavailable) and (LLayout.Admission = nttaHostMetrics) and
      (LLayout.LineAt(0) = '[Layout unavailable]') and (LLayout.Measurements = 0),
      'an unsupported host metric is an explicit owner-free visible refusal without fabricated measurements');
    Check((LLimits.MaximumCells = 4096) and (LLimits.Cells(2).MaximumCells = 2),
      'callers configure the bounded visible-cell pass independently of saved policies');
    LLayout := LayoutNyxTableText(CExact, LPolicy, 2, LLimits,
      {$ifdef PAS2JS}@{$endif}LMetric.Measure);
    Check((LLayout.Count = 2) and (LLayout.LineAt(0) + LLayout.LineAt(1) = CExact),
      'embedded NUL remains part of the exact display projection');
    LBefore := LMetric.Calls;
    LLayout := LayoutNyxTableText('text', LPolicy.Wrap(1000), 8, LLimits,
      {$ifdef PAS2JS}@{$endif}LMetric.Measure);
    Check((LLayout.Outcome = ntloUnavailable) and (LLayout.Admission = nttaLineLimit) and
      (LMetric.Calls = LBefore) and (LLayout.Diagnostic <> ''),
      'authored/runtime line mismatch refuses visibly before entering a metric');
    LLayout := LayoutNyxTableText(CPath, LPolicy, 8, LLimits.UTF8Bytes(3),
      {$ifdef PAS2JS}@{$endif}LMetric.Measure);
    Check((LLayout.Outcome = ntloUnavailable) and (LLayout.Admission = nttaTextLimit) and
      (LMetric.Calls = LBefore), 'byte budget refuses before measurement without truncating input');
    LMetric.Calls := 0;
    LLayout := LayoutNyxTableText('abcdef', LPolicy, 1, LLimits.Measurements(1),
      {$ifdef PAS2JS}@{$endif}LMetric.Measure);
    Check((LLayout.Outcome = ntloUnavailable) and
      (LLayout.Admission = nttaMeasurementLimit) and (LLayout.Measurements = 1) and
      (LMetric.Calls = 1), 'metric-call ceiling stops actual callback work with a separate refusal');
    LMetric.Fail := True;
    LRefused := False;
    try
      LLayout := LayoutNyxTableText('abc', LPolicy, 3, LLimits,
        {$ifdef PAS2JS}@{$endif}LMetric.Measure);
    except
      on E: Exception do
      begin
        LRefused := E.Message = 'exact metric receiver failure';
      end;
    end;
    Check(LRefused, 'application metric errors propagate exactly rather than being swallowed as refusal');
    LMetric.Fail := False;
    LMetric.Invalid := True;
    LRefused := False;
    try
      LLayout := LayoutNyxTableText('abc', LPolicy, 3, LLimits,
        {$ifdef PAS2JS}@{$endif}LMetric.Measure);
    except
      on E: EArgumentException do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'invalid actual font extents refuse instead of corrupting line searches');
    LMetric.Invalid := False;
    LMetric.WideW := False;
    LLayout := LayoutNyxTableText('Wii', NyxColumnText, 2, LLimits,
      {$ifdef PAS2JS}@{$endif}LMetric.Measure);
    LMetric.WideW := True;
    LCopy := LayoutNyxTableText('Wii', NyxColumnText, 2, LLimits,
      {$ifdef PAS2JS}@{$endif}LMetric.Measure);
    Check((LLayout.LineAt(0) = 'Wi') and (LCopy.LineAt(0) = 'W'),
      'each supplied font metric independently controls the admitted visible prefix');
    LCopy := LLayout.Copy;
  finally
    LMetric.Free;
  end;
  Check((LCopy.LineAt(0) = 'Wi') and (LLayout.LineAt(0) = 'Wi'),
    'copied measured lines remain readable after the borrowed metric owner retires');
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-table-text-layout', 'passed');
    document.body.setAttribute('data-table-text-layout-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' bounded table text layout checks; substituted metrics only');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-table-text-layout', 'failed');
      document.body.setAttribute('data-table-text-layout-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
