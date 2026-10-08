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
unit nyx.colors;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text;

type
  { Invalid RGB channels and unsupported wire formats are argument failures. }
  ENyxColorValue = class(EArgumentException);

  { Immutable optional eight-bit sRGB color. No target color handles, palette
    indices, alpha or floating-point conversion enter this portable value.
    Default/NoColor means absent, independently of defined black. Channel reads
    on absence refuse. Copies own only scalar channels and immutable wire text.
    Numeric construction uses lowercase hexadecimal; persistence parsing retains
    the exact admitted letter case so accepted source/history remain lossless. }
  TNyxRGBColor = record
  private
    FDefined: Boolean;
    FRed: Integer;
    FGreen: Integer;
    FBlue: Integer;
    FWire: TNyxText;
    procedure RequireDefined;
    function GetRed: Integer;
    function GetGreen: Integer;
    function GetBlue: Integer;
  public
    { Validate all signed arguments in 0..255 before any narrowing/conversion. }
    class function FromRGB(ARed, AGreen, ABlue: Integer): TNyxRGBColor; static;
    { Explicit persistence/control boundary: empty or exactly #rrggbb, ASCII
      hexadecimal. No trimming, names, shorthand, alpha or wide-gamut coercion. }
    class function FromText(const AText: TNyxText): TNyxRGBColor; static;
    function ToText: TNyxText;
    { Optional-value equality compares channels, independently of wire case. }
    function SameColor(const AOther: TNyxRGBColor): Boolean;
    property Defined: Boolean read FDefined;
    property Red: Integer read GetRed;
    property Green: Integer read GetGreen;
    property Blue: Integer read GetBlue;
  end;

{ Ordinary authoring uses signed numeric channels; NoColor never means black. }
function NyxRGB(ARed, AGreen, ABlue: Integer): TNyxRGBColor;
function NyxNoColor: TNyxRGBColor;
{ Empty succeeds with absence. Malformed text returns False and an empty result;
  no partially parsed color or exception escapes this control-boundary helper. }
function TryNyxRGB(const AText: TNyxText; out AValue: TNyxRGBColor): Boolean;

implementation

function NyxNoColor: TNyxRGBColor;
begin
  Result := Default(TNyxRGBColor);
end;

function NyxRGB(ARed, AGreen, ABlue: Integer): TNyxRGBColor;
begin
  Result := TNyxRGBColor.FromRGB(ARed, AGreen, ABlue);
end;

class function TNyxRGBColor.FromRGB(ARed, AGreen, ABlue: Integer): TNyxRGBColor;
const
  CHex: TNyxText = '0123456789abcdef';

  function Channel(AValue: Integer): TNyxText;
  begin
    Result := CHex[AValue div 16 + 1] + CHex[AValue mod 16 + 1];
  end;

begin

  if (ARed < 0) or (ARed > 255) or (AGreen < 0) or (AGreen > 255) or
    (ABlue < 0) or (ABlue > 255) then
  begin
    raise ENyxColorValue.Create('RGB channels require whole values from zero to 255');
  end;
  Result := NyxNoColor;
  Result.FDefined := True;
  Result.FRed := ARed;
  Result.FGreen := AGreen;
  Result.FBlue := ABlue;
  Result.FWire := '#' + Channel(ARed) + Channel(AGreen) + Channel(ABlue);
end;

function TryNyxRGB(const AText: TNyxText; out AValue: TNyxRGBColor): Boolean;
var
  LIndex: Integer;
  LDigit: Integer;
  LChannels: array[0..2] of Integer;
begin
  AValue := NyxNoColor;
  Result := False;

  if AText = '' then
  begin
    Exit(True);
  end;

  if (Length(AText) <> 7) or (AText[1] <> '#') then
  begin
    Exit;
  end;
  for LIndex := 0 to 2 do
  begin
    LChannels[LIndex] := 0;
  end;
  for LIndex := 2 to 7 do
  begin
    LDigit := -1;

    if (AText[LIndex] >= '0') and (AText[LIndex] <= '9') then
    begin
      LDigit := Ord(AText[LIndex]) - Ord('0');
    end
    else if (AText[LIndex] >= 'a') and (AText[LIndex] <= 'f') then
    begin
      LDigit := Ord(AText[LIndex]) - Ord('a') + 10;
    end
    else if (AText[LIndex] >= 'A') and (AText[LIndex] <= 'F') then
    begin
      LDigit := Ord(AText[LIndex]) - Ord('A') + 10;
    end;

    if LDigit < 0 then
    begin
      Exit;
    end;
    LChannels[(LIndex - 2) div 2] := LChannels[(LIndex - 2) div 2] * 16 + LDigit;
  end;
  AValue := NyxRGB(LChannels[0], LChannels[1], LChannels[2]);
  AValue.FWire := AText;
  Result := True;
end;

class function TNyxRGBColor.FromText(const AText: TNyxText): TNyxRGBColor;
begin

  if not TryNyxRGB(AText, Result) then
  begin
    raise ENyxColorValue.Create('An RGB color requires empty or exactly #rrggbb');
  end;
end;

procedure TNyxRGBColor.RequireDefined;
begin

  if not FDefined then
  begin
    raise ENyxColorValue.Create('An absent color has no RGB channels');
  end;
end;

function TNyxRGBColor.GetRed: Integer;
begin
  RequireDefined;
  Result := FRed;
end;

function TNyxRGBColor.GetGreen: Integer;
begin
  RequireDefined;
  Result := FGreen;
end;

function TNyxRGBColor.GetBlue: Integer;
begin
  RequireDefined;
  Result := FBlue;
end;

function TNyxRGBColor.ToText: TNyxText;
begin
  Result := FWire;
end;

function TNyxRGBColor.SameColor(const AOther: TNyxRGBColor): Boolean;
begin
  Result := (FDefined = AOther.FDefined) and (FRed = AOther.FRed) and
    (FGreen = AOther.FGreen) and (FBlue = AOther.FBlue);
end;

end.
