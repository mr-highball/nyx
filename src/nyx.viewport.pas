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
unit nyx.viewport;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses nyx.text, nyx.types;

type
  { Preserve the input device's units. Native LCL supplies signed wheel ticks;
    adapters normalize 120 ticks to one detent, never guess a pixel distance.
    Browser deltas retain CSS pixels, lines or pages without conversion. Positive
    X/Y point right/down. A wheel request need not produce any viewport movement. }
  TNyxWheelUnit = (nwuPixels, nwuLines, nwuPages, nwuDetents);
  TNyxWheelSnapshot = record
  private
    FMarker: TNyxText;
    FX, FY, FZ: Double;
    FUnit: TNyxWheelUnit;
    FModifiers: TNyxKeyModifiers;
    FCanCancel: Boolean;
    function GetDefined: Boolean;
  public
    property Defined: Boolean read GetDefined;
    property X: Double read FX;
    property Y: Double read FY;
    property Z: Double read FZ;
    property Units: TNyxWheelUnit read FUnit;
    property Modifiers: TNyxKeyModifiers read FModifiers;
    property CanCancel: Boolean read FCanCancel;
  end;

  { Scroll offsets are control-local, not wheel deltas. Native scrollbar ranges
    may use widget-specific units; expose that fact rather than calling them CSS
    pixels. Grid rows/columns and list items have explicit units. An unsupported
    axis is undefined. The containing viewport dimensions are logical pixels. }
  TNyxViewportUnit = (nvuLogicalPixels, nvuRows, nvuColumns, nvuItems,
    nvuNativeUnits);
  TNyxViewportAxis = record
  private
    FMarker: TNyxText;
    FPosition, FExtent, FPage: Double;
    FUnit: TNyxViewportUnit;
    function GetDefined: Boolean;
  public
    property Defined: Boolean read GetDefined;
    property Position: Double read FPosition;
    property Extent: Double read FExtent;
    property PageSize: Double read FPage;
    property Units: TNyxViewportUnit read FUnit;
  end;
  TNyxViewportSnapshot = record
  private
    FMarker: TNyxText;
    FX, FY: TNyxViewportAxis;
    FWidth, FHeight: Double;
    function GetDefined: Boolean;
  public
    function SamePosition(const AOther: TNyxViewportSnapshot): Boolean;
    property Defined: Boolean read GetDefined;
    property X: TNyxViewportAxis read FX;
    property Y: TNyxViewportAxis read FY;
    property Width: Double read FWidth;
    property Height: Double read FHeight;
  end;

{ All snapshots own only values. Construction validates finite dimensions and
  deltas before publication. Signed positions retain browser RTL/overscroll data.
  The initialized marker distinguishes default records from actual observations. }
function NyxWheel(AX, AY, AZ: Double; AUnits: TNyxWheelUnit;
  AModifiers: TNyxKeyModifiers; ACanCancel: Boolean): TNyxWheelSnapshot;
function NyxViewportAxis(APosition, AExtent, APage: Double;
  AUnits: TNyxViewportUnit): TNyxViewportAxis;
function NyxViewport(const AX, AY: TNyxViewportAxis;
  AWidth, AHeight: Double): TNyxViewportSnapshot;

implementation

uses Math, SysUtils;

procedure AdmitFinite(AValue: Double);
begin

  if IsNan(AValue) or IsInfinite(AValue) then
  begin
    raise EArgumentException.Create('Viewport and wheel values must be finite');
  end;
end;

function TNyxWheelSnapshot.GetDefined: Boolean;
begin
  Result := FMarker = 'wheel-1';
end;

function TNyxViewportAxis.GetDefined: Boolean;
begin
  Result := FMarker = 'viewport-axis-1';
end;

function TNyxViewportSnapshot.GetDefined: Boolean;
begin
  Result := FMarker = 'viewport-1';
end;

function TNyxViewportSnapshot.SamePosition(const AOther: TNyxViewportSnapshot): Boolean;
begin
  Result := Defined and AOther.Defined and
    (X.Defined = AOther.X.Defined) and (Y.Defined = AOther.Y.Defined);

  if Result and X.Defined then
  begin
    Result := (X.Position = AOther.X.Position) and (X.Units = AOther.X.Units);
  end;

  if Result and Y.Defined then
  begin
    Result := (Y.Position = AOther.Y.Position) and (Y.Units = AOther.Y.Units);
  end;
end;

function NyxWheel(AX, AY, AZ: Double; AUnits: TNyxWheelUnit;
  AModifiers: TNyxKeyModifiers; ACanCancel: Boolean): TNyxWheelSnapshot;
begin
  AdmitFinite(AX);
  AdmitFinite(AY);
  AdmitFinite(AZ);
  Result.FMarker := 'wheel-1';
  Result.FX := AX;
  Result.FY := AY;
  Result.FZ := AZ;
  Result.FUnit := AUnits;
  Result.FModifiers := AModifiers;
  Result.FCanCancel := ACanCancel;
end;

function NyxViewportAxis(APosition, AExtent, APage: Double;
  AUnits: TNyxViewportUnit): TNyxViewportAxis;
begin
  AdmitFinite(APosition);
  AdmitFinite(AExtent);
  AdmitFinite(APage);

  if (AExtent < 0) or (APage < 0) then
  begin
    raise EArgumentException.Create('Viewport extents and page sizes cannot be negative');
  end;
  Result.FMarker := 'viewport-axis-1';
  Result.FPosition := APosition;
  Result.FExtent := AExtent;
  Result.FPage := APage;
  Result.FUnit := AUnits;
end;

function NyxViewport(const AX, AY: TNyxViewportAxis;
  AWidth, AHeight: Double): TNyxViewportSnapshot;
begin
  AdmitFinite(AWidth);
  AdmitFinite(AHeight);

  if (AWidth < 0) or (AHeight < 0) then
  begin
    raise EArgumentException.Create('Viewport dimensions cannot be negative');
  end;
  Result.FMarker := 'viewport-1';
  Result.FX := AX;
  Result.FY := AY;
  Result.FWidth := AWidth;
  Result.FHeight := AHeight;
end;

end.
