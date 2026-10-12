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
unit nyx.numbers.single;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.linq.numeric;

type
  { Distinct portable binary32 value. The installed pas2js compiler marks Single
    unsupported and aliases generic signatures inconsistently. A typed record
    preserves the precision contract instead of silently substituting Double.
    Default is zero; every factory narrows explicitly before storing a value.
    The Double read is exact because every binary32 value is representable there. }
  TNyxSingleNumber = record
  private
    FValue: Double;
  public
    class function FromNumber(AValue: Double;
      APolicy: TNyxNumberPolicy = nnpFinite): TNyxSingleNumber; static;
    property Value: Double read FValue;
  end;

{ Explicit nearest/ties-even IEEE binary32 narrowing, including subnormals,
  independent of browser Single aliases and the native FPU rounding mode.
  Finite policy refuses non-finite input/rounded overflow; IEEE explicitly permits
  NaN/infinity. Calculations use exact power-of-two scaling and small integers;
  no typed-array/DOM dependency, global FPU state change or hidden JS is needed. }
function NyxRoundSingle(AValue: Double; APolicy: TNyxNumberPolicy = nnpFinite): Double;

implementation

uses
  Math, nyx.linq;

const
  CSingleOverflow: Double = 3.40282356779733661637539395458142568448e38;
  CSingleQuantum: Double = 1.401298464324817070923729583289916131280e-45;

function NyxRoundSingle(AValue: Double; APolicy: TNyxNumberPolicy): Double;
var
  LMagnitude: Double;
  LScaled: Double;
  LStep: Double;
  LFraction: Double;
  LRounded: Double;
  LInteger: Integer;
  LExponent: Integer;
  LIndex: Integer;
begin

  if (Ord(APolicy) < Ord(Low(TNyxNumberPolicy))) or
    (Ord(APolicy) > Ord(High(TNyxNumberPolicy))) then
  begin
    raise ENyxQuery.Create('Unknown single-precision number policy');
  end;

  if IsNan(AValue) or IsInfinite(AValue) then
  begin

    if APolicy = nnpFinite then
    begin
      raise ENyxQuery.Create('Finite Single refuses NaN/infinity');
    end;
    Result := AValue;
    Exit;
  end;

  if AValue = 0 then
  begin
    Result := AValue;
    Exit;
  end;
  LMagnitude := Abs(AValue);

  if LMagnitude >= CSingleOverflow then
  begin

    if APolicy = nnpFinite then
    begin
      raise ENyxQuery.Create('Finite Single overflow');
    end;

    if AValue < 0 then
    begin
      Result := NegInfinity;
    end
    else
    begin
      Result := Infinity;
    end;
    Exit;
  end;
  LScaled := LMagnitude;
  LExponent := 0;
  while LScaled >= 2 do
  begin
    LScaled := LScaled / 2;
    Inc(LExponent);
  end;
  while (LScaled < 1) and (LExponent > -126) do
  begin
    LScaled := LScaled * 2;
    Dec(LExponent);
  end;
  LStep := CSingleQuantum;

  if LExponent > -126 then
  begin
    for LIndex := -125 to LExponent do
    begin
      LStep := LStep * 2;
    end;
  end;
  LScaled := LMagnitude / LStep;
  LInteger := Trunc(LScaled);
  LFraction := LScaled - LInteger;

  if (LFraction > 0.5) or ((LFraction = 0.5) and ((LInteger mod 2) <> 0)) then
  begin
    Inc(LInteger);
  end;
  LRounded := LInteger * LStep;

  if AValue < 0 then
  begin
    LRounded := -LRounded;
  end;
  { LRounded is already a representable binary32 value, returned exactly through
    Double on both targets without unsupported browser Single aliases. }
  Result := LRounded;
end;

class function TNyxSingleNumber.FromNumber(AValue: Double;
  APolicy: TNyxNumberPolicy): TNyxSingleNumber;
begin
  Result.FValue := NyxRoundSingle(AValue, APolicy);
end;

end.
