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
unit nyx.layout.viewport;

{$mode delphi}{$H+}{$codepage utf8}

interface

type
  { Integer logical pixels match the layout engine, independently of a native
    window's much smaller coordinate domain. Bounds own values only. Construction
    rejects negative sizes and endpoints outside signed 32-bit layout space. }
  TNyxViewportBox = record
  private
    FDefined: Boolean;
    FX: Integer;
    FY: Integer;
    FWidth: Integer;
    FHeight: Integer;
    function GetRight: Integer;
    function GetBottom: Integer;
  public
    function Translated(AX, AY: Integer): TNyxViewportBox;
    property Defined: Boolean read FDefined;
    property X: Integer read FX;
    property Y: Integer read FY;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property Right: Integer read GetRight;
    property Bottom: Integer read GetBottom;
  end;

  { Logical clip and physical window allocation are separate. A wholly offscreen
    face has zero physical area but retains its original logical box/control.
    Small intersecting faces retain full size and origin, including negative
    positions: parent clipping preserves their native paint/input coordinates.
    An oversized/out-of-domain face receives the exact visible intersection.
    ContentOffset addresses that intersection inside its original logical face. }
  TNyxViewportPlacement = record
  private
    FClip: TNyxViewportBox;
    FX: Integer;
    FY: Integer;
    FWidth: Integer;
    FHeight: Integer;
    FContentOffsetX: Integer;
    FContentOffsetY: Integer;
    FScreenX: Integer;
    FScreenY: Integer;
  public
    function HasArea: Boolean;
    property Clip: TNyxViewportBox read FClip;
    property X: Integer read FX;
    property Y: Integer read FY;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property ScreenX: Integer read FScreenX;
    property ScreenY: Integer read FScreenY;
    property ContentOffsetX: Integer read FContentOffsetX;
    property ContentOffsetY: Integer read FContentOffsetY;
  end;

function NyxViewportBox(AX, AY, AWidth, AHeight: Integer): TNyxViewportBox;
{ Project one face against an already intersected parent's visible clip. Parent
  origin is its actual window origin, which can differ from the clip origin.
  Platform limits are explicit, inclusive magnitudes/sizes; invalid limits or an
  unrepresentable clip refuse rather than silently truncating geometry. }
function NyxPlaceViewport(const ABox, AParentClip: TNyxViewportBox;
  AParentX, AParentY, AMaximumPosition, AMaximumSize: Integer): TNyxViewportPlacement;

implementation

uses
  Math, SysUtils;

function Wide(AValue: Integer): Double;
begin
  { Assignment widening also works in stable FPC 3.2. Its Delphi dialect does
    not admit every explicit integer-to-Double cast supported by trunk/pas2js. }
  Result := AValue;
end;

function Coordinate(AValue: Double): Integer;
begin

  if (AValue < Low(Integer)) or (AValue > High(Integer)) then
  begin
    raise EArgumentException.Create('Logical viewport coordinate exceeds layout space');
  end;
  Result := Integer(Trunc(AValue));
end;

function NyxViewportBox(AX, AY, AWidth, AHeight: Integer): TNyxViewportBox;
begin

  if (AWidth < 0) or (AHeight < 0) then
  begin
    raise EArgumentException.Create('Logical viewport sizes must be nonnegative');
  end;
  Coordinate(Wide(AX) + AWidth);
  Coordinate(Wide(AY) + AHeight);
  Result := Default(TNyxViewportBox);
  Result.FDefined := True;
  Result.FX := AX;
  Result.FY := AY;
  Result.FWidth := AWidth;
  Result.FHeight := AHeight;
end;

function TNyxViewportBox.GetRight: Integer;
begin
  Result := FX + FWidth;
end;

function TNyxViewportBox.GetBottom: Integer;
begin
  Result := FY + FHeight;
end;

function TNyxViewportBox.Translated(AX, AY: Integer): TNyxViewportBox;
begin

  if not Defined then
  begin
    raise EArgumentException.Create('Translate requires initialized logical bounds');
  end;
  Result := NyxViewportBox(Coordinate(Wide(FX) + AX),
    Coordinate(Wide(FY) + AY), FWidth, FHeight);
end;

function TNyxViewportPlacement.HasArea: Boolean;
begin
  Result := FClip.Defined and (FClip.Width > 0) and (FClip.Height > 0);
end;

function NyxPlaceViewport(const ABox, AParentClip: TNyxViewportBox;
  AParentX, AParentY, AMaximumPosition, AMaximumSize: Integer): TNyxViewportPlacement;
var
  LLeft: Integer;
  LTop: Integer;
  LRight: Integer;
  LBottom: Integer;
  LX: Double;
  LY: Double;
begin

  if not ABox.Defined or not AParentClip.Defined or
    (AMaximumPosition < 1) or (AMaximumSize < 1) then
  begin
    raise EArgumentException.Create('Viewport placement requires bounds and valid platform limits');
  end;
  Result := Default(TNyxViewportPlacement);
  LLeft := Max(ABox.X, AParentClip.X);
  LTop := Max(ABox.Y, AParentClip.Y);
  LRight := Min(ABox.Right, AParentClip.Right);
  LBottom := Min(ABox.Bottom, AParentClip.Bottom);

  if (LRight <= LLeft) or (LBottom <= LTop) then
  begin
    Result.FClip := NyxViewportBox(AParentClip.X, AParentClip.Y, 0, 0);
    Result.FScreenX := AParentX;
    Result.FScreenY := AParentY;
    Exit;
  end;
  Result.FClip := NyxViewportBox(LLeft, LTop, LRight - LLeft, LBottom - LTop);
  LX := Wide(ABox.X) - AParentX;
  LY := Wide(ABox.Y) - AParentY;

  if (Abs(LX) <= AMaximumPosition) and (Abs(LY) <= AMaximumPosition) and
    (ABox.Width <= AMaximumSize) and (ABox.Height <= AMaximumSize) then
  begin
    Result.FX := Coordinate(LX);
    Result.FY := Coordinate(LY);
    Result.FWidth := ABox.Width;
    Result.FHeight := ABox.Height;
    Result.FScreenX := ABox.X;
    Result.FScreenY := ABox.Y;
    Exit;
  end;
  LX := Wide(LLeft) - AParentX;
  LY := Wide(LTop) - AParentY;

  if (Abs(LX) > AMaximumPosition) or (Abs(LY) > AMaximumPosition) or
    (Result.FClip.Width > AMaximumSize) or (Result.FClip.Height > AMaximumSize) then
  begin
    raise EArgumentException.Create('Visible viewport exceeds platform coordinate limits');
  end;
  Result.FX := Coordinate(LX);
  Result.FY := Coordinate(LY);
  Result.FWidth := Result.FClip.Width;
  Result.FHeight := Result.FClip.Height;
  Result.FScreenX := LLeft;
  Result.FScreenY := LTop;
  Result.FContentOffsetX := Coordinate(Wide(LLeft) - ABox.X);
  Result.FContentOffsetY := Coordinate(Wide(LTop) - ABox.Y);
end;

end.
