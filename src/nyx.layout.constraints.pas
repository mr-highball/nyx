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

unit nyx.layout.constraints;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils;

const
  { Matches the portable scalar geometry admission budget. Host viewport extents
    may be larger, but authored bounds cannot exceed this per-axis value. }
  MaximumNyxLayoutBound = 100000;

type
  { Independent logical-pixel range. A present zero maximum is different from
    an absent maximum. Every builder copies Self and validates before returning;
    a failed change never changes a retained policy. No control is retained. }
  TNyxSizeRange = record
  private
    FMinimum: Integer;
    FMaximum: Integer;
    FHasMinimum: Boolean;
    FHasMaximum: Boolean;
  public
    function Minimum(AValue: Integer): TNyxSizeRange;
    function Maximum(AValue: Integer): TNyxSizeRange;
    function WithoutMinimum: TNyxSizeRange;
    function WithoutMaximum: TNyxSizeRange;
    { Negative proposed extents become zero. An explicit minimum may overflow
      available space; adapters keep that leading overflow reachable. }
    function Clamp(AValue: Integer): Integer;
    procedure Validate;
    property HasMinimum: Boolean read FHasMinimum;
    property HasMaximum: Boolean read FHasMaximum;
    property MinimumValue: Integer read FMinimum;
    property MaximumValue: Integer read FMaximum;
  end;
  TNyxSizeRanges = array of TNyxSizeRange;

  { Two independent axis ranges. Applying this complete value replaces/clears
    the four authored bounds; individual configuration methods change one bound.
    It is portable presentation, independent of platform widgets and ownership. }
  TNyxSizeConstraints = record
  private
    FWidth: TNyxSizeRange;
    FHeight: TNyxSizeRange;
  public
    function MinimumWidth(AValue: Integer): TNyxSizeConstraints;
    function MaximumWidth(AValue: Integer): TNyxSizeConstraints;
    function MinimumHeight(AValue: Integer): TNyxSizeConstraints;
    function MaximumHeight(AValue: Integer): TNyxSizeConstraints;
    function Width(const ARange: TNyxSizeRange): TNyxSizeConstraints;
    function Height(const ARange: TNyxSizeRange): TNyxSizeConstraints;
    procedure Validate;
    property WidthRange: TNyxSizeRange read FWidth;
    property HeightRange: TNyxSizeRange read FHeight;
  end;

{ Unbounded defaults, explicitly initializing every native scalar. }
function NyxSizeRange: TNyxSizeRange;
function NyxSizeConstraints: TNyxSizeConstraints;

implementation

function NyxSizeRange: TNyxSizeRange;
begin
  Result := Default(TNyxSizeRange);
end;

function NyxSizeConstraints: TNyxSizeConstraints;
begin
  Result := Default(TNyxSizeConstraints);
end;

procedure TNyxSizeRange.Validate;
begin

  if (FHasMinimum and ((FMinimum < 0) or (FMinimum > MaximumNyxLayoutBound))) or
    (FHasMaximum and ((FMaximum < 0) or (FMaximum > MaximumNyxLayoutBound))) then
  begin
    raise EArgumentException.Create('Layout bounds require admitted nonnegative logical pixels');
  end;

  if FHasMinimum and FHasMaximum and (FMinimum > FMaximum) then
  begin
    raise EArgumentException.Create('A minimum layout bound cannot exceed its maximum');
  end;
end;

function TNyxSizeRange.Minimum(AValue: Integer): TNyxSizeRange;
begin
  Result := Self;
  Result.FMinimum := AValue;
  Result.FHasMinimum := True;
  Result.Validate;
end;

function TNyxSizeRange.Maximum(AValue: Integer): TNyxSizeRange;
begin
  Result := Self;
  Result.FMaximum := AValue;
  Result.FHasMaximum := True;
  Result.Validate;
end;

function TNyxSizeRange.WithoutMinimum: TNyxSizeRange;
begin
  Result := Self;
  Result.FMinimum := 0;
  Result.FHasMinimum := False;
  Result.Validate;
end;

function TNyxSizeRange.WithoutMaximum: TNyxSizeRange;
begin
  Result := Self;
  Result.FMaximum := 0;
  Result.FHasMaximum := False;
  Result.Validate;
end;

function TNyxSizeRange.Clamp(AValue: Integer): Integer;
begin
  Validate;
  Result := AValue;

  if Result < 0 then
  begin
    Result := 0;
  end;

  if FHasMaximum and (Result > FMaximum) then
  begin
    Result := FMaximum;
  end;

  if FHasMinimum and (Result < FMinimum) then
  begin
    Result := FMinimum;
  end;
end;

function TNyxSizeConstraints.MinimumWidth(AValue: Integer): TNyxSizeConstraints;
begin
  Result := Self;
  Result.FWidth := FWidth.Minimum(AValue);
end;

function TNyxSizeConstraints.MaximumWidth(AValue: Integer): TNyxSizeConstraints;
begin
  Result := Self;
  Result.FWidth := FWidth.Maximum(AValue);
end;

function TNyxSizeConstraints.MinimumHeight(AValue: Integer): TNyxSizeConstraints;
begin
  Result := Self;
  Result.FHeight := FHeight.Minimum(AValue);
end;

function TNyxSizeConstraints.MaximumHeight(AValue: Integer): TNyxSizeConstraints;
begin
  Result := Self;
  Result.FHeight := FHeight.Maximum(AValue);
end;

function TNyxSizeConstraints.Width(const ARange: TNyxSizeRange): TNyxSizeConstraints;
begin
  ARange.Validate;
  Result := Self;
  Result.FWidth := ARange;
end;

function TNyxSizeConstraints.Height(const ARange: TNyxSizeRange): TNyxSizeConstraints;
begin
  ARange.Validate;
  Result := Self;
  Result.FHeight := ARange;
end;

procedure TNyxSizeConstraints.Validate;
begin
  FWidth.Validate;
  FHeight.Validate;
end;

end.
