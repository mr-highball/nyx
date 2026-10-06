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

{ Presentation scopes admit the same typed attributes as platform scopes.
  Identity, events, ownership, scalar defaults and state domains stay portable.
  The key is an explicit persistence/extension boundary, never authoring text. }
function NyxViewportKey(const AWidth: TNyxViewportWidth; APlatform: TNyxPlatform;
  AAttribute: TNyxAttribute): TNyxText;
{ Reject malformed, noncanonical, unsupported or empty conditions. Out values
  always initialize; callers must inspect the Boolean before using them. }
function TryNyxViewportKey(const AKey: TNyxText; out AWidth: TNyxViewportWidth;
  out APlatform: TNyxPlatform; out AAttribute: TNyxAttribute): Boolean;

implementation

uses Math;

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

end.
