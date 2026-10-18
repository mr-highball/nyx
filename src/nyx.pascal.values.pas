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
unit nyx.pascal.values;
{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.text, nyx.data, nyx.numbers.decimal;

type
  { Invalid author-supplied values are contract failures. Unsupported source
    operations instead produce Unknown; neither path invokes Pascal code. }
  ENyxPascalValue = class(EArgumentException);
  { Constant knowledge, independent of a host Variant or runtime data binding.
    Integer admits exactly [-2^63, 2^64-1]; wider compiler domains stay unknown. }
  TNyxPascalValueKind = (pvvUnknown, pvvBoolean, pvvInteger, pvvOrdinal);
  { Width-independent supported integer/Boolean operations. Integer bitwise
    operators, casts and real arithmetic require separate compiler type rules. }
  TNyxPascalValueOperation = (pvoAdd, pvoSubtract, pvoMultiply, pvoDivide,
    pvoRemainder, pvoEqual, pvoNotEqual, pvoLess, pvoLessEqual, pvoGreater,
    pvoGreaterEqual, pvoAnd, pvoOr, pvoXor);
  { A copied exact value, with no interfaces or mutable owner. Decimal arithmetic
    provides integer arithmetic without browser floating-point rounding. Default
    is Unknown. Checked getters refuse an incompatible kind. }
  TNyxPascalConstantValue = record
  private
    FKind: TNyxPascalValueKind;
    FNumber: TNyxDecimalNumber;
    FBoolean: Boolean;
    FUnsigned: Boolean;
  public
    class function Unknown: TNyxPascalConstantValue; static;
    class function BooleanValue(AValue: Boolean): TNyxPascalConstantValue; static;
    class function IntegerValue(AValue: Integer): TNyxPascalConstantValue; static;
    { Exact interchange; fractional/out-of-domain numbers raise before return. }
    class function ExactInteger(const AValue: TNyxDecimalNumber): TNyxPascalConstantValue; static;
    function Number: TNyxDecimalNumber;
    function BooleanResult: Boolean;
    { Explicit unsigned-64 intent. It preserves zero/small values after unsigned
      arithmetic; negative input refuses. Mixed-sign/wrapping rules stay unknown. }
    function AsUnsigned: TNyxPascalConstantValue;
    function IsUnsigned: Boolean;
    { An enum ordinal is deliberately distinct from an integer. Operations on
      nominal enum types remain unsupported until type identity is qualified;
      only explicit Ord projects its exact ordinal into integer arithmetic. }
    function AsOrdinal: TNyxPascalConstantValue;
    function OrdValue: TNyxPascalConstantValue;
    { Negation/operations refuse unsupported kinds, zero division or overflow
      by returning Unknown. No modular wrapping or guessed width is applied. }
    function Negated: TNyxPascalConstantValue;
    function Operate(AOperation: TNyxPascalValueOperation;
      const AOther: TNyxPascalConstantValue): TNyxPascalConstantValue;
    { True only for known Boolean or the exact integers zero/one. }
    function TryTruth(out AValue: Boolean): Boolean;
    property Kind: TNyxPascalValueKind read FKind;
  end;

{ One complete Pascal integer token, with explicit radix. Underscores, real
  numbers and signs are outside this token entry; the parser owns unary signs.
  Malformed/out-of-domain spelling refuses, never rounds or normalizes source. }
function NyxPascalIntegerToken(const ASpelling: TNyxText;
  ABase: Integer): TNyxPascalConstantValue;

implementation

function Exact(const AText: TNyxText): TNyxDecimalNumber;
begin
  Result := TNyxDecimalNumber.FromDecimal(NyxDecimal(AText));
end;

function Magnitude(const AValue: TNyxDecimalNumber): TNyxDecimalNumber;
begin
  Result := AValue;

  if Result.Negative then
  begin
    Result := TNyxDecimalNumber.FromInteger(0).Subtract(Result);
  end;
end;

function TimesTen(const AValue: TNyxDecimalNumber): TNyxDecimalNumber;
var
  LTwice: TNyxDecimalNumber;
  LEight: TNyxDecimalNumber;
begin
  LTwice := AValue.Add(AValue);
  LEight := LTwice.Add(LTwice);
  LEight := LEight.Add(LEight);
  Result := LEight.Add(LTwice);
end;

function Product(const ALeft, ARight: TNyxDecimalNumber): TNyxDecimalNumber;
var
  LDigits: TNyxText;
  LAt: Integer;
  LDigit: Integer;
  LCount: Integer;
  LRight: TNyxDecimalNumber;
begin
  Result := TNyxDecimalNumber.FromInteger(0);
  LDigits := Magnitude(ALeft).ToText;
  LRight := Magnitude(ARight);
  for LAt := 1 to Length(LDigits) do
  begin
    Result := TimesTen(Result);
    LDigit := Ord(LDigits[LAt]) - Ord('0');
    for LCount := 1 to LDigit do
    begin
      Result := Result.Add(LRight);
    end;
  end;

  if ALeft.Negative <> ARight.Negative then
  begin
    Result := TNyxDecimalNumber.FromInteger(0).Subtract(Result);
  end;
end;

procedure Divide(const ALeft, ARight: TNyxDecimalNumber;
  out AQuotient, ARemainder: TNyxDecimalNumber);
var
  LDigits: TNyxText;
  LResult: TNyxText;
  LAt: Integer;
  LDigit: Integer;
  LDivisor: TNyxDecimalNumber;
begin
  LDivisor := Magnitude(ARight);

  if LDivisor.CompareTo(TNyxDecimalNumber.FromInteger(0)) = 0 then
  begin
    raise ENyxPascalValue.Create('A constant divisor cannot be zero');
  end;
  ARemainder := TNyxDecimalNumber.FromInteger(0);
  LDigits := Magnitude(ALeft).ToText;
  LResult := '';
  for LAt := 1 to Length(LDigits) do
  begin
    ARemainder := TimesTen(ARemainder).Add(
      TNyxDecimalNumber.FromInteger(Ord(LDigits[LAt]) - Ord('0')));
    LDigit := 0;
    while ARemainder.CompareTo(LDivisor) >= 0 do
    begin
      ARemainder := ARemainder.Subtract(LDivisor);
      Inc(LDigit);
    end;
    LResult := LResult + TNyxText(Chr(Ord('0') + LDigit));
  end;
  while (Length(LResult) > 1) and (LResult[1] = '0') do
  begin
    Delete(LResult, 1, 1);
  end;
  AQuotient := Exact(LResult);

  if ALeft.Negative <> ARight.Negative then
  begin
    AQuotient := TNyxDecimalNumber.FromInteger(0).Subtract(AQuotient);
  end;

  if ALeft.Negative then
  begin
    ARemainder := TNyxDecimalNumber.FromInteger(0).Subtract(ARemainder);
  end;
end;

class function TNyxPascalConstantValue.Unknown: TNyxPascalConstantValue;
begin
  Result := Default(TNyxPascalConstantValue);
end;

class function TNyxPascalConstantValue.BooleanValue(AValue: Boolean): TNyxPascalConstantValue;
begin
  Result := Unknown;
  Result.FKind := pvvBoolean;
  Result.FBoolean := AValue;
end;

class function TNyxPascalConstantValue.IntegerValue(AValue: Integer): TNyxPascalConstantValue;
begin
  Result := ExactInteger(TNyxDecimalNumber.FromInteger(AValue));
end;

class function TNyxPascalConstantValue.ExactInteger(
  const AValue: TNyxDecimalNumber): TNyxPascalConstantValue;
begin

  if (AValue.Scale <> 0) or (AValue.CompareTo(Exact('-9223372036854775808')) < 0) or
    (AValue.CompareTo(Exact('18446744073709551615')) > 0) then
  begin
    raise ENyxPascalValue.Create('A constant integer exceeds the supported exact domain');
  end;
  Result := Unknown;
  Result.FKind := pvvInteger;
  Result.FNumber := AValue;
  Result.FUnsigned := AValue.CompareTo(Exact('9223372036854775807')) > 0;
end;

function TNyxPascalConstantValue.Number: TNyxDecimalNumber;
begin

  if FKind <> pvvInteger then
  begin
    raise ENyxPascalValue.Create('This constant is not an exact integer');
  end;
  Result := FNumber;
end;

function TNyxPascalConstantValue.BooleanResult: Boolean;
begin

  if FKind <> pvvBoolean then
  begin
    raise ENyxPascalValue.Create('This constant is not a Boolean');
  end;
  Result := FBoolean;
end;

function TNyxPascalConstantValue.AsUnsigned: TNyxPascalConstantValue;
begin
  Result := Unknown;

  if (FKind = pvvInteger) and not FNumber.Negative then
  begin
    Result := Self;
    Result.FUnsigned := True;
  end;
end;

function TNyxPascalConstantValue.IsUnsigned: Boolean;
begin
  Result := (FKind = pvvInteger) and FUnsigned;
end;

function TNyxPascalConstantValue.TryTruth(out AValue: Boolean): Boolean;
begin
  AValue := False;
  Result := FKind = pvvBoolean;

  if Result then
  begin
    AValue := FBoolean;
    Exit;
  end;

  if FKind = pvvInteger then
  begin
    AValue := FNumber.CompareTo(TNyxDecimalNumber.FromInteger(1)) = 0;
    Result := AValue or (FNumber.CompareTo(TNyxDecimalNumber.FromInteger(0)) = 0);
  end;
end;

function TNyxPascalConstantValue.AsOrdinal: TNyxPascalConstantValue;
begin
  Result := Unknown;

  if FKind = pvvInteger then
  begin
    Result := Self;
    Result.FKind := pvvOrdinal;
  end;
end;

function TNyxPascalConstantValue.OrdValue: TNyxPascalConstantValue;
begin
  Result := Unknown;

  if FKind in [pvvInteger, pvvOrdinal] then
  begin
    Result := ExactInteger(FNumber);
  end
  else if FKind = pvvBoolean then
  begin
    Result := IntegerValue(Ord(FBoolean));
  end;
end;

function TNyxPascalConstantValue.Negated: TNyxPascalConstantValue;
begin
  Result := Unknown;

  if FKind <> pvvInteger then
  begin
    Exit;
  end;
  try
    Result := ExactInteger(TNyxDecimalNumber.FromInteger(0).Subtract(FNumber));

    if not FUnsigned and Result.FUnsigned then
    begin
      Result := Unknown;
    end;
  except
    on ENyxPascalValue do
    begin
      Result := Unknown;
    end;
    on ENyxDecimalNumber do
    begin
      Result := Unknown;
    end;
  end;
end;

function TNyxPascalConstantValue.Operate(AOperation: TNyxPascalValueOperation;
  const AOther: TNyxPascalConstantValue): TNyxPascalConstantValue;
var
  LComparison: Integer;
  LBoolean: Boolean;
  LQuotient: TNyxDecimalNumber;
  LRemainder: TNyxDecimalNumber;
begin
  Result := Unknown;

  if (FKind = pvvUnknown) or (AOther.FKind = pvvUnknown) then
  begin
    Exit;
  end;

  if (FKind = pvvBoolean) and (AOther.FKind = pvvBoolean) then
  begin
    case AOperation of
      pvoEqual:
        begin
          LBoolean := FBoolean = AOther.FBoolean;
        end;
      pvoNotEqual:
        begin
          LBoolean := FBoolean <> AOther.FBoolean;
        end;
      pvoAnd:
        begin
          LBoolean := FBoolean and AOther.FBoolean;
        end;
      pvoOr:
        begin
          LBoolean := FBoolean or AOther.FBoolean;
        end;
      pvoXor:
        begin
          LBoolean := FBoolean <> AOther.FBoolean;
        end;
    else
      Exit;
    end;
    Exit(BooleanValue(LBoolean));
  end;

  if (FKind <> pvvInteger) or (AOther.FKind <> pvvInteger) then
  begin
    Exit;
  end;

  if ((FUnsigned or AOther.FUnsigned) and
    (FNumber.Negative or AOther.FNumber.Negative)) then
  begin
    { Implicit signed/unsigned coercion depends on compiler types. Refuse that
      uncertainty rather than silently choosing mathematical or modular order. }
    Exit;
  end;
  try
    case AOperation of
      pvoAdd:
        begin
          Result := ExactInteger(FNumber.Add(AOther.FNumber));
        end;
      pvoSubtract:
        begin
          Result := ExactInteger(FNumber.Subtract(AOther.FNumber));
        end;
      pvoMultiply:
        begin
          Result := ExactInteger(Product(FNumber, AOther.FNumber));
        end;
      pvoDivide, pvoRemainder:
        begin
          Divide(FNumber, AOther.FNumber, LQuotient, LRemainder);

          if AOperation = pvoDivide then
          begin
            Result := ExactInteger(LQuotient);
          end
          else
          begin
            Result := ExactInteger(LRemainder);
          end;
        end;
      pvoEqual, pvoNotEqual, pvoLess, pvoLessEqual, pvoGreater, pvoGreaterEqual:
        begin
          LComparison := FNumber.CompareTo(AOther.FNumber);
          case AOperation of
            pvoEqual:
              begin
                LBoolean := LComparison = 0;
              end;
            pvoNotEqual:
              begin
                LBoolean := LComparison <> 0;
              end;
            pvoLess:
              begin
                LBoolean := LComparison < 0;
              end;
            pvoLessEqual:
              begin
                LBoolean := LComparison <= 0;
              end;
            pvoGreater:
              begin
                LBoolean := LComparison > 0;
              end;
            pvoGreaterEqual:
              begin
                LBoolean := LComparison >= 0;
              end;
          else
            LBoolean := False;
          end;
          Result := BooleanValue(LBoolean);
        end;
    else
      begin
        Result := Unknown;
      end;
    end;

    if Result.FKind = pvvInteger then
    begin

      if FUnsigned or AOther.FUnsigned then
      begin
        Result := Result.AsUnsigned;
      end
      else if Result.FUnsigned then
      begin
        { Two signed operands cannot silently promote after signed overflow. }
        Result := Unknown;
      end;
    end;
  except
    on ENyxPascalValue do
    begin
      Result := Unknown;
    end;
    on ENyxDecimalNumber do
    begin
      Result := Unknown;
    end;
  end;
end;

function NyxPascalIntegerToken(const ASpelling: TNyxText;
  ABase: Integer): TNyxPascalConstantValue;
var
  LAt: Integer;
  LStart: Integer;
  LDigit: Integer;
  LMultiply: Integer;
  LValue: TNyxDecimalNumber;
  LPrevious: TNyxDecimalNumber;
begin

  if not (ABase in [2, 8, 10, 16]) or (Length(ASpelling) > 65) then
  begin
    raise ENyxPascalValue.Create('An integer token needs a supported radix and bounded spelling');
  end;
  LStart := 1;

  if ABase <> 10 then
  begin
    LStart := 2;

    if (ASpelling = '') or
      not (((ABase = 2) and (ASpelling[1] = '%')) or
      ((ABase = 8) and (ASpelling[1] = '&')) or
      ((ABase = 16) and (ASpelling[1] = '$'))) then
    begin
      raise ENyxPascalValue.Create('An integer radix prefix differs from its requested base');
    end;
  end;

  if Length(ASpelling) < LStart then
  begin
    raise ENyxPascalValue.Create('An integer token needs digits');
  end;
  LValue := TNyxDecimalNumber.FromInteger(0);
  for LAt := LStart to Length(ASpelling) do
  begin
    LDigit := Ord(ASpelling[LAt]) - Ord('0');

    if ASpelling[LAt] in ['A'..'F'] then
    begin
      LDigit := Ord(ASpelling[LAt]) - Ord('A') + 10;
    end
    else if ASpelling[LAt] in ['a'..'f'] then
    begin
      LDigit := Ord(ASpelling[LAt]) - Ord('a') + 10;
    end;

    if (LDigit < 0) or (LDigit >= ABase) then
    begin
      raise ENyxPascalValue.Create('An integer token contains a digit outside its radix');
    end;
    LPrevious := LValue;
    for LMultiply := 2 to ABase do
    begin
      LValue := LValue.Add(LPrevious);
    end;
    LValue := LValue.Add(TNyxDecimalNumber.FromInteger(LDigit));
    Result := TNyxPascalConstantValue.ExactInteger(LValue);
  end;
end;

end.
