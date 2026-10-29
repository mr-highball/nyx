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

unit nyx.scroll.chrome;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils;

type
  { Platform-independent scroll direction; range values remain in the host's
    native content units, never in thumb pixels. }
  TNyxScrollAxis = (nsaHorizontal, nsaVertical);

  { A hit identifies one semantic action. Page areas exclude the thumb and the
    arrows, including when a small bar cannot fit full arrow/thumb metrics. }
  TNyxScrollChromePart = (nscpNone, nscpDecrement, nscpPageDecrement,
    nscpThumb, nscpPageIncrement, nscpIncrement);

  { Invalid range/metric input refuses before a native window is changed. }
  ENyxScrollChrome = class(Exception);

  { Wide ordinal intermediates cover every signed-32-bit span exactly. Native
    FPC uses Int64; pas2js uses its exact Number integer domain with explicit
    finite/integral admission, without claiming unsupported browser Int64 math. }
  TNyxScrollWide = {$ifdef PAS2JS}Double{$else}Int64{$endif};

  { Detached inclusive signed-32-bit source range. PageSize is widened so the
    complete signed range (4294967296 units) is representable on both targets.
    All position math stays exact at endpoints, without 16-bit message packing. }
  TNyxScrollRange = record
    Minimum: Integer;
    Maximum: Integer;
    Position: Integer;
    PageSize: TNyxScrollWide;
    procedure Validate;
    function LastPosition: Integer;
    function Clamp(AValue: TNyxScrollWide): Integer;
  end;

  { Logical metrics are copied, never progressively rescaled. MinimumPixel is
    an explicit physical visibility floor; callers can choose larger logical
    targets for touch while the geometry/hit areas remain the same contract. }
  TNyxScrollChromeMetrics = record
    ArrowLength: Integer;
    MinimumThumbLength: Integer;
    MinimumPixel: Integer;
    procedure Validate;
  end;

  { One immutable-by-convention axis layout in applied physical pixels. Its
    original range owns conversion in both directions. Nonnegative metric and
    travel ties round upward on both compilers, independently of the host's
    floating-point rounding mode. No host/control is kept. }
  TNyxScrollChromeGeometry = record
    Extent: Integer;
    Arrow: Integer;
    TrackStart: Integer;
    TrackLength: Integer;
    ThumbStart: Integer;
    ThumbLength: Integer;
    Source: TNyxScrollRange;
    function Hit(APixel: Integer): TNyxScrollChromePart;
    function PositionAt(APixel, AGrabOffset: Integer): Integer;
  end;

function NyxScrollRange(AMinimum, AMaximum, APosition: Integer;
  APageSize: TNyxScrollWide): TNyxScrollRange;
function NyxScrollChromeMetrics: TNyxScrollChromeMetrics;
function LayoutNyxScrollChrome(AExtent: Integer; AScale: Double;
  const ARange: TNyxScrollRange;
  const AMetrics: TNyxScrollChromeMetrics): TNyxScrollChromeGeometry;

implementation

uses Math;

procedure TNyxScrollRange.Validate;
var
  LCount: TNyxScrollWide;
begin
  LCount := TNyxScrollWide(Maximum) - TNyxScrollWide(Minimum) + 1;

  if (Maximum < Minimum) or (PageSize < 0) or (PageSize > LCount)
    {$ifdef PAS2JS}or IsNan(PageSize) or IsInfinite(PageSize) or
      (Trunc(PageSize) <> PageSize){$endif} then
  begin
    raise ENyxScrollChrome.Create('Scroll range requires ordered bounds and an admitted page');
  end;
end;

function TNyxScrollRange.LastPosition: Integer;
var
  LLast: TNyxScrollWide;
begin
  Validate;
  LLast := TNyxScrollWide(Maximum);

  if PageSize > 0 then
  begin
    LLast := LLast - PageSize + 1;
  end;
  Result := Round(LLast);
end;

function TNyxScrollRange.Clamp(AValue: TNyxScrollWide): Integer;
var
  LLast: Integer;
begin
  LLast := LastPosition;
  {$ifdef PAS2JS}

  if IsNan(AValue) or IsInfinite(AValue) or (Trunc(AValue) <> AValue) then
  begin
    raise ENyxScrollChrome.Create('Scroll positions require finite ordinal values');
  end;
  {$endif}

  if AValue < Minimum then
  begin
    Result := Minimum;
  end
  else if AValue > LLast then
  begin
    Result := LLast;
  end
  else
  begin
    Result := Round(AValue);
  end;
end;

function NyxScrollRange(AMinimum, AMaximum, APosition: Integer;
  APageSize: TNyxScrollWide): TNyxScrollRange;
begin
  Result.Minimum := AMinimum;
  Result.Maximum := AMaximum;
  Result.Position := APosition;
  Result.PageSize := APageSize;
  Result.Validate;
  Result.Position := Result.Clamp(APosition);
end;

procedure TNyxScrollChromeMetrics.Validate;
begin

  if (ArrowLength < 0) or (MinimumThumbLength < 0) or (MinimumPixel < 1) then
  begin
    raise ENyxScrollChrome.Create('Scroll metrics require nonnegative lengths and a positive pixel floor');
  end;
end;

function NyxScrollChromeMetrics: TNyxScrollChromeMetrics;
begin
  Result.ArrowLength := 17;
  Result.MinimumThumbLength := 18;
  Result.MinimumPixel := 1;
end;

function Metric(AValue: Integer; AScale: Double; AFloor: Integer): Integer;
var
  LValue: Double;
begin
  LValue := AValue * AScale;

  if IsNan(LValue) or IsInfinite(LValue) or (LValue > High(Integer) - 1.0) then
  begin
    raise ENyxScrollChrome.Create('Scroll metric exceeds its physical integer range');
  end;
  Result := Max(AFloor, Trunc(LValue + 0.5));
end;

function LayoutNyxScrollChrome(AExtent: Integer; AScale: Double;
  const ARange: TNyxScrollRange;
  const AMetrics: TNyxScrollChromeMetrics): TNyxScrollChromeGeometry;
var
  LCount: Double;
  LTravel: Integer;
  LSpan: Double;
  LOffset: Double;
begin
  ARange.Validate;
  AMetrics.Validate;

  if (AExtent < 0) or IsNan(AScale) or IsInfinite(AScale) or (AScale <= 0) then
  begin
    raise ENyxScrollChrome.Create('Scroll layout requires a nonnegative extent and finite positive scale');
  end;
  Result := Default(TNyxScrollChromeGeometry);
  Result.Source := ARange;
  Result.Source.Position := ARange.Clamp(ARange.Position);
  Result.Extent := AExtent;

  if AExtent = 0 then
  begin
    Exit;
  end;
  Result.Arrow := Min(AExtent div 2, Metric(AMetrics.ArrowLength, AScale, 0));
  Result.TrackStart := Result.Arrow;
  Result.TrackLength := AExtent - Result.Arrow * 2;
  LCount := Double(TNyxScrollWide(ARange.Maximum) - TNyxScrollWide(ARange.Minimum) + 1);
  Result.ThumbLength := Min(Result.TrackLength,
    Max(Metric(AMetrics.MinimumThumbLength, AScale, AMetrics.MinimumPixel),
      Trunc(Result.TrackLength * (ARange.PageSize / LCount) + 0.5)));
  LTravel := Result.TrackLength - Result.ThumbLength;
  LSpan := Double(TNyxScrollWide(ARange.LastPosition) - TNyxScrollWide(ARange.Minimum));
  LOffset := 0;

  if LSpan > 0 then
  begin
    LOffset := (TNyxScrollWide(Result.Source.Position) - TNyxScrollWide(ARange.Minimum)) / LSpan;
  end
  else
  begin
    Result.ThumbLength := Result.TrackLength;
  end;
  Result.ThumbStart := Result.TrackStart + Trunc(LTravel * LOffset + 0.5);
end;

function TNyxScrollChromeGeometry.Hit(APixel: Integer): TNyxScrollChromePart;
begin
  Result := nscpNone;

  if (APixel < 0) or (APixel >= Extent) then
  begin
    Exit;
  end;

  if APixel < Arrow then
  begin
    Exit(nscpDecrement);
  end;

  if APixel >= Extent - Arrow then
  begin
    Exit(nscpIncrement);
  end;

  if APixel < ThumbStart then
  begin
    Exit(nscpPageDecrement);
  end;

  if APixel < ThumbStart + ThumbLength then
  begin
    Exit(nscpThumb);
  end;
  Result := nscpPageIncrement;
end;

function TNyxScrollChromeGeometry.PositionAt(APixel, AGrabOffset: Integer): Integer;
var
  LTravel: Integer;
  LPixel: TNyxScrollWide;
  LSpan: Double;
begin
  LTravel := TrackLength - ThumbLength;

  if LTravel <= 0 then
  begin
    Exit(Source.Minimum);
  end;
  LPixel := TNyxScrollWide(APixel) - TNyxScrollWide(AGrabOffset) - TNyxScrollWide(TrackStart);

  if LPixel <= 0 then
  begin
    Exit(Source.Minimum);
  end;

  if LPixel >= LTravel then
  begin
    Exit(Source.LastPosition);
  end;
  LSpan := Double(TNyxScrollWide(Source.LastPosition) - TNyxScrollWide(Source.Minimum));
  Result := Source.Clamp(TNyxScrollWide(Source.Minimum) +
    Trunc(LSpan * (LPixel / LTravel) + 0.5));
end;

end.
