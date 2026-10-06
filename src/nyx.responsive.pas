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
unit nyx.responsive;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.types;

type
  { Immutable condition on the available rendering viewport, in logical pixels.
    Lower bounds are inclusive; upper bounds are exclusive. Zero upper bound
    means unbounded. Any is also the deliberately initialized default record.
    This describes available space, independent of operating system, device
    names and build targets. No component or renderer is retained. }
  TNyxViewportWidth = record
  private
    FMinimum: Integer;
    FMaximum: Integer;
  public
    { Any restores ordinary defaults for the current platform scope. }
    class function Any: TNyxViewportWidth; static;
    { Below requires a positive exclusive upper bound. AtLeast accepts zero;
      Between requires a nonnegative lower bound and a greater upper bound.
      Invalid intervals raise EArgumentException without creating a scope. }
    class function Below(APixels: Integer): TNyxViewportWidth; static;
    class function AtLeast(APixels: Integer): TNyxViewportWidth; static;
    class function Between(AMinimum, AMaximum: Integer): TNyxViewportWidth; static;
    { Invalid host widths raise EArgumentException, including NaN/infinity. }
    function Matches(AWidth: Double): Boolean;
    function Same(const AOther: TNyxViewportWidth): Boolean;
    function IsAny: Boolean;
    function Caption: TNyxText;
    { Crafted source spelling and strict canonical persistence identity. }
    function Pascal: TNyxText;
    property Minimum: Integer read FMinimum;
    property Maximum: Integer read FMaximum;
  end;

  { Orientation describes the available host rectangle, never a device sensor.
    Positive square hosts match only Square; zero-sized hosts match only Any. }
  TNyxViewportOrientation = (nvoAny, nvoPortrait, nvoLandscape, nvoSquare);

  { Copied, immutable conjunction of width, height and orientation. Fluent
    methods replace their own axis, retaining the other conditions. Logical
    pixel intervals use the same inclusive/exclusive bounds as ViewportWidth.
    No renderer, component or application is retained. }
  TNyxViewportCondition = record
  private
    FWidth: TNyxViewportWidth;
    FHeight: TNyxViewportWidth;
    FOrientation: TNyxViewportOrientation;
    function GetWidthMinimum: Integer;
    function GetWidthMaximum: Integer;
    function GetHeightMinimum: Integer;
    function GetHeightMaximum: Integer;
  public
    class function Any: TNyxViewportCondition; static;
    class function FromWidth(const AWidth: TNyxViewportWidth): TNyxViewportCondition; static;
    function WidthBelow(APixels: Integer): TNyxViewportCondition;
    function WidthAtLeast(APixels: Integer): TNyxViewportCondition;
    function WidthBetween(AMinimum, AMaximum: Integer): TNyxViewportCondition;
    function HeightBelow(APixels: Integer): TNyxViewportCondition;
    function HeightAtLeast(APixels: Integer): TNyxViewportCondition;
    function HeightBetween(AMinimum, AMaximum: Integer): TNyxViewportCondition;
    function Orientation(AValue: TNyxViewportOrientation): TNyxViewportCondition;
    { Invalid/nonfinite host dimensions raise before projection. }
    function Matches(AWidth, AHeight: Double): Boolean;
    function Same(const AOther: TNyxViewportCondition): Boolean;
    function IsAny: Boolean;
    function IsWidthOnly: Boolean;
    function Caption: TNyxText;
    function Pascal: TNyxText;
    { Always emits a condition expression. Pascal keeps legacy width shorthand
      for overloaded WhenViewport calls; named definitions require this type. }
    function PascalCondition: TNyxText;
    property WidthMinimum: Integer read GetWidthMinimum;
    property WidthMaximum: Integer read GetWidthMaximum;
    property HeightMinimum: Integer read GetHeightMinimum;
    property HeightMaximum: Integer read GetHeightMaximum;
    property OrientationValue: TNyxViewportOrientation read FOrientation;
  end;

{ Closed orientation spellings at persistence and generated-source boundaries. }
function NyxViewportOrientationName(AValue: TNyxViewportOrientation): TNyxText;
function NyxViewportOrientationPascal(AValue: TNyxViewportOrientation): TNyxText;

{ Presentation scopes admit the same typed attributes as platform scopes.
  Identity, events, ownership, scalar defaults and state domains stay portable.
  The key is an explicit persistence/extension boundary, never authoring text. }
function NyxViewportKey(const AWidth: TNyxViewportWidth; APlatform: TNyxPlatform;
  AAttribute: TNyxAttribute): TNyxText; overload;
function NyxViewportKey(const ACondition: TNyxViewportCondition; APlatform: TNyxPlatform;
  AAttribute: TNyxAttribute): TNyxText; overload;
{ Reject malformed, noncanonical, unsupported or empty conditions. Out values
  always initialize; callers must inspect the Boolean before using them. }
function TryNyxViewportKey(const AKey: TNyxText; out AWidth: TNyxViewportWidth;
  out APlatform: TNyxPlatform; out AAttribute: TNyxAttribute): Boolean; overload;
function TryNyxViewportKey(const AKey: TNyxText; out ACondition: TNyxViewportCondition;
  out APlatform: TNyxPlatform; out AAttribute: TNyxAttribute): Boolean; overload;

implementation

uses Math;

function NyxViewportOrientationName(AValue: TNyxViewportOrientation): TNyxText;
const
  CNames: array[TNyxViewportOrientation] of TNyxText =
    ('any', 'portrait', 'landscape', 'square');
begin

  if (Ord(AValue) < Ord(Low(TNyxViewportOrientation))) or
    (Ord(AValue) > Ord(High(TNyxViewportOrientation))) then
  begin
    raise EArgumentException.Create('Unknown viewport orientation');
  end;
  Result := CNames[AValue];
end;

function NyxViewportOrientationPascal(AValue: TNyxViewportOrientation): TNyxText;
const
  CNames: array[TNyxViewportOrientation] of TNyxText =
    ('nvoAny', 'nvoPortrait', 'nvoLandscape', 'nvoSquare');
begin
  NyxViewportOrientationName(AValue);
  Result := CNames[AValue];
end;

class function TNyxViewportCondition.Any: TNyxViewportCondition;
begin
  Result.FWidth := TNyxViewportWidth.Any;
  Result.FHeight := TNyxViewportWidth.Any;
  Result.FOrientation := nvoAny;
end;

class function TNyxViewportCondition.FromWidth(
  const AWidth: TNyxViewportWidth): TNyxViewportCondition;
begin
  Result := Any;
  Result.FWidth := AWidth;
end;

function TNyxViewportCondition.GetWidthMinimum: Integer;
begin
  Result := FWidth.Minimum;
end;

function TNyxViewportCondition.GetWidthMaximum: Integer;
begin
  Result := FWidth.Maximum;
end;

function TNyxViewportCondition.GetHeightMinimum: Integer;
begin
  Result := FHeight.Minimum;
end;

function TNyxViewportCondition.GetHeightMaximum: Integer;
begin
  Result := FHeight.Maximum;
end;

function TNyxViewportCondition.WidthBelow(APixels: Integer): TNyxViewportCondition;
begin
  Result := Self;
  Result.FWidth := TNyxViewportWidth.Below(APixels);
end;

function TNyxViewportCondition.WidthAtLeast(APixels: Integer): TNyxViewportCondition;
begin
  Result := Self;
  Result.FWidth := TNyxViewportWidth.AtLeast(APixels);
end;

function TNyxViewportCondition.WidthBetween(AMinimum, AMaximum: Integer): TNyxViewportCondition;
begin
  Result := Self;
  Result.FWidth := TNyxViewportWidth.Between(AMinimum, AMaximum);
end;

function TNyxViewportCondition.HeightBelow(APixels: Integer): TNyxViewportCondition;
begin
  Result := Self;
  Result.FHeight := TNyxViewportWidth.Below(APixels);
end;

function TNyxViewportCondition.HeightAtLeast(APixels: Integer): TNyxViewportCondition;
begin
  Result := Self;
  Result.FHeight := TNyxViewportWidth.AtLeast(APixels);
end;

function TNyxViewportCondition.HeightBetween(AMinimum, AMaximum: Integer): TNyxViewportCondition;
begin
  Result := Self;
  Result.FHeight := TNyxViewportWidth.Between(AMinimum, AMaximum);
end;

function TNyxViewportCondition.Orientation(
  AValue: TNyxViewportOrientation): TNyxViewportCondition;
begin
  { Validate the closed value even when callers explicitly cast an ordinal. }
  NyxViewportOrientationName(AValue);
  Result := Self;
  Result.FOrientation := AValue;
end;

function TNyxViewportCondition.Matches(AWidth, AHeight: Double): Boolean;
var
  LWidthMatches: Boolean;
  LHeightMatches: Boolean;
begin
  { Evaluate both dimensions before short-circuiting: an invalid height must
    refuse even when the width lies outside this condition. }
  LWidthMatches := FWidth.Matches(AWidth);

  if IsNan(AHeight) or IsInfinite(AHeight) or (AHeight < 0) then
  begin
    raise EArgumentException.Create('Viewport height must be finite and nonnegative');
  end;
  LHeightMatches := FHeight.Matches(AHeight);
  Result := LWidthMatches and LHeightMatches;

  if (FOrientation <> nvoAny) then
  begin
    Result := Result and (AWidth > 0) and (AHeight > 0);
    case FOrientation of
      nvoAny:
        begin
          { Already handled above; included for exhaustive closed-value checking. }
        end;
      nvoPortrait:
        begin
          Result := Result and (AHeight > AWidth);
        end;
      nvoLandscape:
        begin
          Result := Result and (AWidth > AHeight);
        end;
      nvoSquare:
        begin
          Result := Result and (AWidth = AHeight);
        end;
    end;
  end;
end;

function TNyxViewportCondition.Same(const AOther: TNyxViewportCondition): Boolean;
begin
  Result := FWidth.Same(AOther.FWidth) and FHeight.Same(AOther.FHeight) and
    (FOrientation = AOther.FOrientation);
end;

function TNyxViewportCondition.IsWidthOnly: Boolean;
begin
  Result := FHeight.IsAny and (FOrientation = nvoAny);
end;

function TNyxViewportCondition.IsAny: Boolean;
begin
  Result := FWidth.IsAny and IsWidthOnly;
end;

function TNyxViewportCondition.Caption: TNyxText;
begin

  if IsWidthOnly then
  begin
    Exit(FWidth.Caption);
  end;
  Result := 'Width: ' + FWidth.Caption + '; height: ' + FHeight.Caption;

  if FOrientation <> nvoAny then
  begin
    Result := Result + '; ' + NyxViewportOrientationName(FOrientation);
  end;
end;

function TNyxViewportCondition.Pascal: TNyxText;
begin
  { Preserve existing width-only source byte-for-byte. }

  if IsWidthOnly then
  begin
    Exit(FWidth.Pascal);
  end;
  Result := PascalCondition;
end;

function TNyxViewportCondition.PascalCondition: TNyxText;

  function Axis(const AInterval: TNyxViewportWidth; const AName: TNyxText): TNyxText;
  begin

    if AInterval.IsAny then
    begin
      Exit('');
    end;

    if AInterval.Maximum = 0 then
    begin
      Exit('.' + AName + 'AtLeast(' + TNyxText(IntToStr(AInterval.Minimum)) + ')');
    end;

    if AInterval.Minimum = 0 then
    begin
      Exit('.' + AName + 'Below(' + TNyxText(IntToStr(AInterval.Maximum)) + ')');
    end;
    Result := '.' + AName + 'Between(' + TNyxText(IntToStr(AInterval.Minimum)) + ', ' +
      TNyxText(IntToStr(AInterval.Maximum)) + ')';
  end;

begin
  Result := 'TNyxViewportCondition.Any' + Axis(FWidth, 'Width') + Axis(FHeight, 'Height');

  if FOrientation <> nvoAny then
  begin
    Result := Result + '.Orientation(' + NyxViewportOrientationPascal(FOrientation) + ')';
  end;
end;

class function TNyxViewportWidth.Any: TNyxViewportWidth;
begin
  Result.FMinimum := 0;
  Result.FMaximum := 0;
end;

class function TNyxViewportWidth.Below(APixels: Integer): TNyxViewportWidth;
begin

  if APixels <= 0 then
  begin
    raise EArgumentException.Create('Viewport upper bound must be positive');
  end;
  Result.FMinimum := 0;
  Result.FMaximum := APixels;
end;

class function TNyxViewportWidth.AtLeast(APixels: Integer): TNyxViewportWidth;
begin

  if APixels < 0 then
  begin
    raise EArgumentException.Create('Viewport lower bound cannot be negative');
  end;
  Result.FMinimum := APixels;
  Result.FMaximum := 0;
end;

class function TNyxViewportWidth.Between(AMinimum, AMaximum: Integer): TNyxViewportWidth;
begin

  if (AMinimum < 0) or (AMaximum <= AMinimum) then
  begin
    raise EArgumentException.Create('Viewport interval must be nonempty and nonnegative');
  end;
  Result.FMinimum := AMinimum;
  Result.FMaximum := AMaximum;
end;

function TNyxViewportWidth.Matches(AWidth: Double): Boolean;
begin

  if IsNan(AWidth) or IsInfinite(AWidth) or (AWidth < 0) then
  begin
    raise EArgumentException.Create('Viewport width must be finite and nonnegative');
  end;
  Result := (AWidth >= FMinimum) and ((FMaximum = 0) or (AWidth < FMaximum));
end;

function TNyxViewportWidth.Same(const AOther: TNyxViewportWidth): Boolean;
begin
  Result := (FMinimum = AOther.FMinimum) and (FMaximum = AOther.FMaximum);
end;

function TNyxViewportWidth.IsAny: Boolean;
begin
  Result := (FMinimum = 0) and (FMaximum = 0);
end;

function TNyxViewportWidth.Caption: TNyxText;
begin

  if IsAny then
  begin
    Exit('Any width');
  end;

  if FMaximum = 0 then
  begin
    Exit('At least ' + TNyxText(IntToStr(FMinimum)) + ' px');
  end;

  if FMinimum = 0 then
  begin
    Exit('Below ' + TNyxText(IntToStr(FMaximum)) + ' px');
  end;
  Result := TNyxText(IntToStr(FMinimum)) + ' to below ' +
    TNyxText(IntToStr(FMaximum)) + ' px';
end;

function TNyxViewportWidth.Pascal: TNyxText;
begin

  if IsAny then
  begin
    Exit('TNyxViewportWidth.Any');
  end;

  if FMaximum = 0 then
  begin
    Exit('TNyxViewportWidth.AtLeast(' + TNyxText(IntToStr(FMinimum)) + ')');
  end;

  if FMinimum = 0 then
  begin
    Exit('TNyxViewportWidth.Below(' + TNyxText(IntToStr(FMaximum)) + ')');
  end;
  Result := 'TNyxViewportWidth.Between(' + TNyxText(IntToStr(FMinimum)) + ', ' +
    TNyxText(IntToStr(FMaximum)) + ')';
end;

function NyxViewportKey(const AWidth: TNyxViewportWidth; APlatform: TNyxPlatform;
  AAttribute: TNyxAttribute): TNyxText;
begin

  if AWidth.IsAny then
  begin
    Exit(NyxPlatformKey(APlatform, AAttribute));
  end;

  if not NyxPlatformAttribute(AAttribute) then
  begin
    raise EArgumentException.Create('Viewport scope requires a presentation attribute');
  end;
  Result := '@nyx.viewport:' + TNyxText(IntToStr(AWidth.Minimum)) + ':' +
    TNyxText(IntToStr(AWidth.Maximum)) + ':' + NyxPlatformName(APlatform) + ':' +
    NyxAttributeName(AAttribute);
end;

function TryNyxViewportKey(const AKey: TNyxText; out AWidth: TNyxViewportWidth;
  out APlatform: TNyxPlatform; out AAttribute: TNyxAttribute): Boolean;
var
  LParts: array[0..3] of TNyxText;
  LIndex: Integer;
  LStart: Integer;
  LEnd: Integer;
  LMinimum: Integer;
  LMaximum: Integer;
  LPlatform: TNyxPlatform;
  LFound: Boolean;
begin
  Result := False;
  AWidth := TNyxViewportWidth.Any;
  APlatform := npfAny;
  AAttribute := atText;

  if Copy(AKey, 1, 14) <> '@nyx.viewport:' then
  begin
    Exit;
  end;
  LStart := 15;
  for LIndex := 0 to 2 do
  begin
    LEnd := LStart;
    while (LEnd <= Length(AKey)) and (AKey[LEnd] <> ':') do
    begin
      Inc(LEnd);
    end;

    if LEnd > Length(AKey) then
    begin
      Exit;
    end;
    LParts[LIndex] := Copy(AKey, LStart, LEnd - LStart);
    LStart := LEnd + 1;
  end;
  LParts[3] := Copy(AKey, LStart, MaxInt);

  if not TryStrToInt(LParts[0], LMinimum) or
    not TryStrToInt(LParts[1], LMaximum) or (LMinimum < 0) or
    (LMaximum < 0) or ((LMaximum <> 0) and (LMaximum <= LMinimum)) or
    ((LMinimum = 0) and (LMaximum = 0)) then
  begin
    Exit;
  end;
  LFound := False;
  for LPlatform := npfAny to npfNativeLCL do
  begin

    if LParts[2] = NyxPlatformName(LPlatform) then
    begin
      APlatform := LPlatform;
      LFound := True;
      Break;
    end;
  end;

  if not LFound or not TryNyxAttribute(LParts[3], AAttribute) or
    not NyxPlatformAttribute(AAttribute) then
  begin
    Exit;
  end;
  AWidth.FMinimum := LMinimum;
  AWidth.FMaximum := LMaximum;
  Result := AKey = NyxViewportKey(AWidth, APlatform, AAttribute);
end;

function NyxViewportKey(const ACondition: TNyxViewportCondition; APlatform: TNyxPlatform;
  AAttribute: TNyxAttribute): TNyxText;
begin

  if ACondition.IsWidthOnly then
  begin
    Exit(NyxViewportKey(ACondition.FWidth, APlatform, AAttribute));
  end;

  if not NyxPlatformAttribute(AAttribute) then
  begin
    raise EArgumentException.Create('Viewport scope requires a presentation attribute');
  end;
  Result := '@nyx.viewport-size:' + TNyxText(IntToStr(ACondition.WidthMinimum)) + ':' +
    TNyxText(IntToStr(ACondition.WidthMaximum)) + ':' +
    TNyxText(IntToStr(ACondition.HeightMinimum)) + ':' +
    TNyxText(IntToStr(ACondition.HeightMaximum)) + ':' +
    NyxViewportOrientationName(ACondition.OrientationValue) + ':' +
    NyxPlatformName(APlatform) + ':' + NyxAttributeName(AAttribute);
end;

function TryNyxViewportKey(const AKey: TNyxText; out ACondition: TNyxViewportCondition;
  out APlatform: TNyxPlatform; out AAttribute: TNyxAttribute): Boolean;
var
  LParts: array[0..6] of TNyxText;
  LBounds: array[0..3] of Integer;
  LIndex: Integer;
  LStart: Integer;
  LEnd: Integer;
  LWidth: TNyxViewportWidth;
  LCandidate: TNyxViewportCondition;
  LOrientation: TNyxViewportOrientation;
  LPlatform: TNyxPlatform;
  LAttribute: TNyxAttribute;
  LFound: Boolean;
begin
  Result := False;
  ACondition := TNyxViewportCondition.Any;
  APlatform := npfAny;
  AAttribute := atText;

  if TryNyxViewportKey(AKey, LWidth, LPlatform, LAttribute) then
  begin
    ACondition := TNyxViewportCondition.FromWidth(LWidth);
    APlatform := LPlatform;
    AAttribute := LAttribute;
    Exit(True);
  end;

  if Copy(AKey, 1, 19) <> '@nyx.viewport-size:' then
  begin
    Exit;
  end;
  LStart := 20;
  for LIndex := 0 to 5 do
  begin
    LEnd := LStart;
    while (LEnd <= Length(AKey)) and (AKey[LEnd] <> ':') do
    begin
      Inc(LEnd);
    end;

    if LEnd > Length(AKey) then
    begin
      Exit;
    end;
    LParts[LIndex] := Copy(AKey, LStart, LEnd - LStart);
    LStart := LEnd + 1;
  end;
  LParts[6] := Copy(AKey, LStart, MaxInt);
  for LIndex := 0 to 3 do
  begin

    if not TryStrToInt(LParts[LIndex], LBounds[LIndex]) or (LBounds[LIndex] < 0) then
    begin
      Exit;
    end;
  end;

  if ((LBounds[1] <> 0) and (LBounds[1] <= LBounds[0])) or
    ((LBounds[3] <> 0) and (LBounds[3] <= LBounds[2])) then
  begin
    Exit;
  end;
  LCandidate := TNyxViewportCondition.Any;
  LCandidate.FWidth.FMinimum := LBounds[0];
  LCandidate.FWidth.FMaximum := LBounds[1];
  LCandidate.FHeight.FMinimum := LBounds[2];
  LCandidate.FHeight.FMaximum := LBounds[3];
  LFound := False;
  for LOrientation := Low(TNyxViewportOrientation) to High(TNyxViewportOrientation) do
  begin

    if LParts[4] = NyxViewportOrientationName(LOrientation) then
    begin
      LCandidate.FOrientation := LOrientation;
      LFound := True;
      Break;
    end;
  end;

  if not LFound or LCandidate.IsWidthOnly then
  begin
    Exit;
  end;
  LFound := False;
  for LPlatform := npfAny to npfNativeLCL do
  begin

    if LParts[5] = NyxPlatformName(LPlatform) then
    begin
      LFound := True;
      Break;
    end;
  end;

  if not LFound or not TryNyxAttribute(LParts[6], LAttribute) or
    not NyxPlatformAttribute(LAttribute) then
  begin
    Exit;
  end;

  if AKey <> NyxViewportKey(LCandidate, LPlatform, LAttribute) then
  begin
    Exit;
  end;
  ACondition := LCandidate;
  APlatform := LPlatform;
  AAttribute := LAttribute;
  Result := True;
end;

end.
