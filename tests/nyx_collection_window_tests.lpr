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
program nyx_collection_window_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  {$ifdef PAS2JS}Web,{$endif}
  SysUtils, Math, nyx.collections.window;

var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AReason: String);
begin

  if not AValue then
  begin
    raise Exception.Create('Collection row geometry: ' + AReason);
  end;
  Inc(GChecks);
end;

procedure Run;
var
  LGeometry: TNyxCollectionRowGeometry;
  LWindow: TNyxCollectionRowWindow;
  LRefused: Boolean;
  LIndex: Integer;
  LExpected: Double;
  LHeights: array[0..256] of Double;
  LTotal: Double;
begin
  LGeometry := TNyxCollectionRowGeometry.Create(4096, 32);
  try
    Check(LGeometry.Total = 131072, 'uniform total includes every logical row');
    Check(LGeometry.OffsetAt(4095) = 131040, 'distant row start');
    Check(LGeometry.IndexAt(131041) = 4095, 'distant pixel maps to its row');
    Check(LGeometry.IndexAt(LGeometry.Total) = 4096, 'end offset is exclusive');
    LWindow := LGeometry.Window(32000, 320, 6);
    Check((LWindow.First = 994) and (LWindow.AfterLast = 1016) and
      (LWindow.Count = 22), 'bounded middle viewport and overscan');
    Check((LWindow.BeforePixels = 31808) and (LWindow.AfterPixels = 98560),
      'spacers preserve uniform logical extent');
    LWindow := LGeometry.Window(0, 320, High(Integer));
    Check((LWindow.First = 0) and (LWindow.AfterLast = 4096),
      'large overscan clips without signed overflow');
    LWindow := LGeometry.Window(1.0e300, 1.0e300, 6);
    Check((LWindow.First = 4096) and (LWindow.Count = 0),
      'large finite out-of-source geometry remains empty');
    LGeometry.Reset(4, 40);
    Check(LGeometry.Measure(1, 80), 'wrapped second row admits actual growth');
    Check(LGeometry.Measure(2, 20) and LGeometry.Measure(3, 60),
      'independent shorter and taller rows');
    Check((LGeometry.Total = 200) and (LGeometry.OffsetAt(2) = 120),
      'measured prefix and total');
    Check(not LGeometry.Measure(1, 80), 'same measurement is a no-op');
    LWindow := LGeometry.Window(50, 75, 0);
    Check((LWindow.First = 1) and (LWindow.AfterLast = 3) and
      (LWindow.BeforePixels = 40) and (LWindow.AfterPixels = 60),
      'partial measured rows intersect the viewport');
    LWindow := LGeometry.Window(40, 80, 0);
    Check((LWindow.First = 1) and (LWindow.AfterLast = 2),
      'exact boundary excludes following row');
    LWindow := LGeometry.Window(50, 0, 6);
    Check((LWindow.First = 1) and (LWindow.Count = 0),
      'zero extent does not realize invisible rows');
    LRefused := False;
    try
      LGeometry.Reset(-1, 20);
    except
      on EArgumentException do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LGeometry.Count = 4) and (LGeometry.Total = 200),
      'invalid reset retains complete prior geometry');
    LRefused := False;
    try
      LGeometry.Measure(1, NaN);
    except
      on EArgumentException do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LGeometry.HeightAt(1) = 80) and (LGeometry.Total = 200),
      'non-finite measurement refuses before mutation');
    LRefused := False;
    try
      LGeometry.Measure(4, 10);
    except
      on EArgumentOutOfRangeException do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LGeometry.Total = 200), 'end is not a measurable row');
    LRefused := False;
    try
      LWindow := LGeometry.Window(0, 20, -1);
    except
      on EArgumentException do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'negative overscan refuses');
    LRefused := False;
    try
      LWindow := LGeometry.Window(Infinity, 20, 0);
    except
      on EArgumentException do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'non-finite viewport refuses');
    { A separate linear sum is the oracle for non-power-of-two mixed heights.
      Binary fractions avoid treating floating noise as a contract assertion. }
    LGeometry.Reset(Length(LHeights), 32);
    LTotal := 0;
    for LIndex := 0 to High(LHeights) do
    begin
      LHeights[LIndex] := 12 + (LIndex mod 19) * 0.125;
      LGeometry.Measure(LIndex, LHeights[LIndex]);
      LTotal := LTotal + LHeights[LIndex];
    end;
    LExpected := 0;
    for LIndex := 0 to High(LHeights) do
    begin
      Check(LGeometry.OffsetAt(LIndex) = LExpected, 'mixed-height linear prefix oracle');
      Check(LGeometry.IndexAt(LExpected) = LIndex, 'mixed-height exact boundary oracle');
      Check(LGeometry.IndexAt(LExpected + LHeights[LIndex] / 2) = LIndex,
        'mixed-height interior oracle');
      LExpected := LExpected + LHeights[LIndex];
    end;
    Check((LGeometry.Total = LTotal) and (LGeometry.IndexAt(LTotal) = Length(LHeights)),
      'mixed-height final exclusive end');
    LGeometry.Reset(0, 32);
    LWindow := LGeometry.Window(0, 640, 6);
    Check((LGeometry.Total = 0) and (LWindow.Count = 0) and
      (LWindow.BeforePixels = 0) and (LWindow.AfterPixels = 0), 'empty source');
  finally
    LGeometry.Free;
  end;
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-row-geometry', 'passed');
    document.body.setAttribute('data-row-geometry-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' measured collection row geometry checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-row-geometry', 'failed');
      document.body.setAttribute('data-event-error', LException.Message);
      {$else}
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.

