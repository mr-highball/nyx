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
unit nyx.numbers.integer64;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.data;

type
  ENyxInteger64 = class(Exception);
  ENyxInteger64Overflow = class(ENyxInteger64);
  { Exact signed-64-domain value on both targets. Cardinal limbs avoid browser
    numbers losing integer bits above 2^53. Default is ordinary zero. Factories
    enforce [-2^63, 2^63-1]; negative zero is normalized. Arithmetic is checked,
    independent of compiler switches. No mutable arrays or target/renderer types. }
  TNyxInteger64 = record
  private
    FHigh: Cardinal;
    FLow: Cardinal;
    FNegative: Boolean;
    function Combine(const AOther: TNyxInteger64; ASubtract: Boolean): TNyxInteger64;
  public
    { Ordinary native/browser Integer entry, including Low(Integer). }
    class function FromInteger(AValue: Integer): TNyxInteger64; static;
    { Explicit exact magnitude limbs. Negative 2^63 is permitted; positive 2^63
      and any larger magnitude refuse. This is a typed code-first constructor. }
    class function FromParts(AHigh, ALow: Cardinal; ANegative: Boolean = False):
      TNyxInteger64; static;
    { Strict integer text is an explicit interchange boundary. Decimal fractions,
      exponent forms, whitespace, plus signs and leading zeros refuse. -0 becomes
      zero. No floating conversion or global codepage/locale changes occur. }
    class function Parse(const AText: TNyxText): TNyxInteger64; static;
    function Add(const AOther: TNyxInteger64): TNyxInteger64;
    function Subtract(const AOther: TNyxInteger64): TNyxInteger64;
    { Numeric ordering returns -1/0/1 without potentially overflowing subtraction. }
    function CompareTo(const AOther: TNyxInteger64): Integer;
    { Exact decimal digits and Nyx data admission preserve values above 2^53. }
    function ToText: TNyxText;
    function ToDecimal: TNyxDecimal;
    { Refuse narrowing by returning False/zero when Integer cannot hold the value. }
    function TryInteger(out AValue: Integer): Boolean;
    { Explicit approximate IEEE projection, suitable for approximate means/plots.
      ToText/ToDecimal remain the exact persistence and binding contracts. }
    function ApproximateDouble: Double;
    property HighPart: Cardinal read FHigh;
    property LowPart: Cardinal read FLow;
    property Negative: Boolean read FNegative;
  end;

implementation

class function TNyxInteger64.FromParts(AHigh, ALow: Cardinal;
  ANegative: Boolean): TNyxInteger64;
begin

  if (AHigh > $80000000) or
    ((AHigh = $80000000) and ((ALow <> 0) or not ANegative)) then
  begin
    raise ENyxInteger64Overflow.Create('Integer64 magnitude exceeds signed domain');
  end;
  Result.FHigh := AHigh;
  Result.FLow := ALow;
  Result.FNegative := ANegative and ((AHigh <> 0) or (ALow <> 0));
end;

class function TNyxInteger64.FromInteger(AValue: Integer): TNyxInteger64;
var
  LMagnitude: Cardinal;
begin

  if AValue < 0 then
  begin
    { Avoid negating Low(Integer) while retaining its exact magnitude. }
    LMagnitude := Cardinal(-(AValue + 1)) + 1;
  end
  else
  begin
    LMagnitude := Cardinal(AValue);
  end;
  Result := FromParts(0, LMagnitude, AValue < 0);
end;

class function TNyxInteger64.Parse(const AText: TNyxText): TNyxInteger64;
var
  LNegative: Boolean;
  LStart: Integer;
  LIndex: Integer;
  LLimb: Integer;
  LDigits: array[0..3] of Cardinal;
  LCarry: Cardinal;
  LCurrent: Cardinal;
  LHigh: Cardinal;
  LLow: Cardinal;
begin

  if AText = '' then
  begin
    raise ENyxInteger64.Create('Integer64 text is empty');
  end;
  LNegative := AText[1] = '-';
  LStart := 1;

  if LNegative then
  begin
    LStart := 2;
  end;

  if (LStart > Length(AText)) or
    ((AText[LStart] = '0') and (LStart < Length(AText))) then
  begin
    raise ENyxInteger64.Create('Integer64 requires strict integer text');
  end;
  for LLimb := 0 to 3 do
  begin
    LDigits[LLimb] := 0;
  end;
  Result := FromInteger(0);
  for LIndex := LStart to Length(AText) do
  begin

    if (AText[LIndex] < '0') or (AText[LIndex] > '9') then
    begin
      raise ENyxInteger64.Create('Integer64 requires strict integer text');
    end;
    LCarry := Ord(AText[LIndex]) - Ord('0');
    for LLimb := 3 downto 0 do
    begin
      LCurrent := LDigits[LLimb] * 10 + LCarry;
      LDigits[LLimb] := LCurrent and $ffff;
      LCarry := LCurrent shr 16;
    end;

    if LCarry <> 0 then
    begin
      raise ENyxInteger64Overflow.Create('Integer64 text exceeds its domain');
    end;
    LHigh := (LDigits[0] shl 16) or LDigits[1];
    LLow := (LDigits[2] shl 16) or LDigits[3];
    Result := FromParts(LHigh, LLow, LNegative);
  end;
end;

function TNyxInteger64.Combine(const AOther: TNyxInteger64;
  ASubtract: Boolean): TNyxInteger64;
var
  LLeft: array[0..3] of Cardinal;
  LRight: array[0..3] of Cardinal;
  LResult: array[0..3] of Cardinal;
  LLeftLarger: Boolean;
  LOtherNegative: Boolean;
  LNegative: Boolean;
  LIndex: Integer;
  LCarry: Integer;
  LCurrent: Integer;
  LHigh: Cardinal;
  LLow: Cardinal;
begin
  LLeft[0] := FHigh shr 16;
  LLeft[1] := FHigh and $ffff;
  LLeft[2] := FLow shr 16;
  LLeft[3] := FLow and $ffff;
  LRight[0] := AOther.FHigh shr 16;
  LRight[1] := AOther.FHigh and $ffff;
  LRight[2] := AOther.FLow shr 16;
  LRight[3] := AOther.FLow and $ffff;
  { pas2js Boolean xor currently emits numeric 0/1. Inequality preserves a real
    Boolean for later equality/sign decisions on both targets. }
  LOtherNegative := AOther.FNegative <> ASubtract;
  LCarry := 0;

  if FNegative = LOtherNegative then
  begin
    LNegative := FNegative;
    for LIndex := 3 downto 0 do
    begin
      LCurrent := Integer(LLeft[LIndex]) + Integer(LRight[LIndex]) + LCarry;
      LResult[LIndex] := Cardinal(LCurrent) and $ffff;
      LCarry := LCurrent shr 16;
    end;

    if LCarry <> 0 then
    begin
      raise ENyxInteger64Overflow.Create('Integer64 addition overflow');
    end;
  end
  else
  begin
    LLeftLarger := (FHigh > AOther.FHigh) or
      ((FHigh = AOther.FHigh) and (FLow >= AOther.FLow));
    LNegative := FNegative;

    if not LLeftLarger then
    begin
      LNegative := LOtherNegative;
    end;
    for LIndex := 3 downto 0 do
    begin

      if LLeftLarger then
      begin
        LCurrent := Integer(LLeft[LIndex]) - Integer(LRight[LIndex]) - LCarry;
      end
      else
      begin
        LCurrent := Integer(LRight[LIndex]) - Integer(LLeft[LIndex]) - LCarry;
      end;
      LCarry := 0;

      if LCurrent < 0 then
      begin
        Inc(LCurrent, 65536);
        LCarry := 1;
      end;
      LResult[LIndex] := Cardinal(LCurrent);
    end;
  end;
  LHigh := (LResult[0] shl 16) or LResult[1];
  LLow := (LResult[2] shl 16) or LResult[3];
  Result := FromParts(LHigh, LLow, LNegative);
end;

function TNyxInteger64.Add(const AOther: TNyxInteger64): TNyxInteger64;
begin
  Result := Combine(AOther, False);
end;

function TNyxInteger64.Subtract(const AOther: TNyxInteger64): TNyxInteger64;
begin
  Result := Combine(AOther, True);
end;

function TNyxInteger64.CompareTo(const AOther: TNyxInteger64): Integer;
begin
  Result := 0;

  if FNegative <> AOther.FNegative then
  begin

    if FNegative then
    begin
      Result := -1;
    end
    else
    begin
      Result := 1;
    end;
    Exit;
  end;

  if (FHigh < AOther.FHigh) or ((FHigh = AOther.FHigh) and (FLow < AOther.FLow)) then
  begin
    Result := -1;
  end
  else
  begin

    if (FHigh > AOther.FHigh) or ((FHigh = AOther.FHigh) and (FLow > AOther.FLow)) then
    begin
      Result := 1;
    end;
  end;

  if FNegative then
  begin
    Result := -Result;
  end;
end;

function TNyxInteger64.ToText: TNyxText;
var
  LDigits: array[0..3] of Cardinal;
  LIndex: Integer;
  LCarry: Cardinal;
  LCurrent: Cardinal;
  LNonzero: Boolean;
begin
  LDigits[0] := FHigh shr 16;
  LDigits[1] := FHigh and $ffff;
  LDigits[2] := FLow shr 16;
  LDigits[3] := FLow and $ffff;
  Result := '';
  repeat
    LCarry := 0;
    LNonzero := False;
    for LIndex := 0 to 3 do
    begin
      LCurrent := LCarry * 65536 + LDigits[LIndex];
      LDigits[LIndex] := LCurrent div 10;
      LCarry := LCurrent mod 10;
      LNonzero := LNonzero or (LDigits[LIndex] <> 0);
    end;
    Result := TNyxText(Chr(48 + LCarry)) + Result;
  until not LNonzero;

  if FNegative then
  begin
    Result := '-' + Result;
  end;
end;

function TNyxInteger64.ToDecimal: TNyxDecimal;
begin
  Result := NyxDecimal(ToText);
end;

function TNyxInteger64.TryInteger(out AValue: Integer): Boolean;
begin
  AValue := 0;
  Result := (FHigh = 0) and
    ((FLow <= Cardinal(High(Integer))) or (FNegative and (FLow = $80000000)));

  if Result then
  begin

    if FNegative then
    begin

      if FLow = $80000000 then
      begin
        AValue := Low(Integer);
      end
      else
      begin
        AValue := -Integer(FLow);
      end;
    end
    else
    begin
      AValue := Integer(FLow);
    end;
  end;
end;

function TNyxInteger64.ApproximateDouble: Double;
var
  LHigh: Double;
  LLow: Double;
begin
  LHigh := FHigh;
  LLow := FLow;
  Result := LHigh * 4294967296.0 + LLow;

  if FNegative then
  begin
    Result := -Result;
  end;
end;

end.
