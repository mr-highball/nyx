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

program nyx_scroll_chrome_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, Math, nyx.scroll.chrome
  {$ifdef PAS2JS}, Web{$endif};

var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AReason: String);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

procedure Run;
const
  CScales: array[0..4] of Double = (0.1, 0.5, 1.0, 1.5, 4.0);
var
  LRange: TNyxScrollRange;
  LGeometry: TNyxScrollChromeGeometry;
  LCopy: TNyxScrollChromeGeometry;
  LMetrics: TNyxScrollChromeMetrics;
  LScale: Double;
  LIndex: Integer;
  LPixel: Integer;
  LValue: Integer;
  LPrevious: Integer;
  LFailed: Boolean;
begin
  LMetrics := NyxScrollChromeMetrics;
  LGeometry := LayoutNyxScrollChrome(100, 0.5, NyxScrollRange(0, 9, 0, 1), LMetrics);
  Check(LGeometry.Arrow = 9, 'half-pixel metric ties round upward on both targets');
  LMetrics.ArrowLength := 0;
  LMetrics.MinimumThumbLength := 0;
  LGeometry := LayoutNyxScrollChrome(5, 1, NyxScrollRange(0, 9, 0, 5), LMetrics);
  Check(LGeometry.ThumbLength = 3, 'half-pixel page fractions use the same tie policy');
  LGeometry := LayoutNyxScrollChrome(3, 1, NyxScrollRange(0, 1, 0, 0), LMetrics);
  Check(LGeometry.PositionAt(1, 0) = 1, 'half-unit travel uses the same tie policy');
  LMetrics := NyxScrollChromeMetrics;
  LRange := NyxScrollRange(Low(Integer), High(Integer), 70001, 18);
  Check(LRange.LastPosition = High(Integer) - 17, 'page changes the inclusive last position');
  Check(LRange.Clamp(TNyxScrollWide(Low(Integer)) - 1) = Low(Integer), 'lower clamping preserves the full signed range');
  Check(LRange.Clamp(TNyxScrollWide(High(Integer)) + 1) = LRange.LastPosition, 'upper clamping never wraps');
  for LIndex := Low(CScales) to High(CScales) do
  begin
    LScale := CScales[LIndex];
    LGeometry := LayoutNyxScrollChrome(320, LScale, LRange, LMetrics);
    Check((LGeometry.ThumbStart >= LGeometry.TrackStart) and
      (LGeometry.ThumbStart + LGeometry.ThumbLength <= 320 - LGeometry.Arrow),
      'all thumb pixels stay inside the admitted track');
    Check(LGeometry.Hit(-1) = nscpNone, 'negative pixels hit no action');
    Check(LGeometry.Hit(320) = nscpNone, 'the extent endpoint is exclusive');
    Check(LGeometry.Hit(0) = nscpDecrement, 'the leading arrow has its own action');
    Check(LGeometry.Hit(319) = nscpIncrement, 'the trailing arrow has its own action');
    Check(LGeometry.Hit(LGeometry.ThumbStart) = nscpThumb, 'the leading thumb pixel belongs to the thumb');
    Check(LGeometry.PositionAt(LGeometry.TrackStart, 0) = Low(Integer),
      'dragging to the first track pixel is exact');
    Check(LGeometry.PositionAt(320, 0) = LRange.LastPosition,
      'dragging to the final track pixel is exact beyond 16 bits');
    LPrevious := Low(Integer);
    for LPixel := LGeometry.TrackStart to 320 - LGeometry.Arrow do
    begin
      LValue := LGeometry.PositionAt(LPixel, 0);
      Check(LValue >= LPrevious, 'thumb conversion remains monotonic across the signed boundary');
      LPrevious := LValue;
    end;
    LCopy := LGeometry;
    LRange.Position := Low(Integer);
    Check(LCopy.Source.Position = 70001, 'a copied geometry retains its independent range value');
    LRange.Position := 70001;
  end;
  LRange := NyxScrollRange(Low(Integer), High(Integer), 0, TNyxScrollWide(4294967296));
  LGeometry := LayoutNyxScrollChrome(200, 1, LRange, LMetrics);
  Check((LGeometry.ThumbLength = LGeometry.TrackLength) and
    (LGeometry.PositionAt(100, 0) = Low(Integer)), 'a full page has no drag travel');
  LGeometry := LayoutNyxScrollChrome(0, 1, NyxScrollRange(0, 0, 0, 0), LMetrics);
  Check(LGeometry.Hit(0) = nscpNone, 'an empty physical allocation has no hit region');
  LGeometry := LayoutNyxScrollChrome(2, 1, NyxScrollRange(0, 100, 0, 1), LMetrics);
  Check((LGeometry.TrackLength = 0) and (LGeometry.Arrow = 1),
    'tiny allocations divide available pixels without negative geometry');
  LFailed := False;
  try
    NyxScrollRange(2, 1, 0, 0);
  except
    on ENyxScrollChrome do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed, 'unordered source ranges refuse');
  LFailed := False;
  try
    NyxScrollRange(0, 5, 0, 7);
  except
    on ENyxScrollChrome do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed, 'oversized pages refuse');
  LFailed := False;
  try
    LayoutNyxScrollChrome(40, 0, NyxScrollRange(0, 5, 0, 1), LMetrics);
  except
    on ENyxScrollChrome do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed, 'a zero scale refuses without publishing geometry');
  {$ifdef PAS2JS}
  LFailed := False;
  try
    NyxScrollRange(0, 10, 0, 0.5);
  except
    on ENyxScrollChrome do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed, 'browser wide storage admits only ordinal page sizes');
  LFailed := False;
  try
    LRange.Clamp(NaN);
  except
    on ENyxScrollChrome do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed, 'browser wide storage refuses a non-finite position');
  {$endif}
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-scroll-chrome-result', 'passed');
    document.body.setAttribute('data-scroll-chrome-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' portable scroll-chrome checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-scroll-chrome-result', 'failed');
      document.body.setAttribute('data-scroll-chrome-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
