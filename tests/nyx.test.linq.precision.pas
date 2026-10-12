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
unit nyx.test.linq.precision;

{$mode delphi}{$H+}{$codepage utf8}

interface

function RunNyxPrecisionChecks: Integer;

implementation

uses
  SysUtils, Math, nyx.text, nyx.data, nyx.linq, nyx.linq.numeric,
  nyx.numbers.decimal, nyx.linq.decimal, nyx.numbers.single, nyx.linq.single
  {$ifndef PAS2JS}, nyx.numbers.single.native{$endif};

type
  TOptionalDecimal = INyxOptional<TNyxDecimalNumber>;
  TOptionalSingle = INyxOptional<TNyxSingleNumber>;

var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create('Precision: ' + AReason);
  end;
  Inc(GChecks);
end;

function Decimal(const AValue: TNyxText): TNyxDecimalNumber;
begin
  Result := TNyxDecimalNumber.FromDecimal(NyxDecimal(AValue));
end;

procedure Decimals;
var
  LValue: TNyxDecimalNumber;
  LOther: TNyxDecimalNumber;
  LMaximum: TNyxDecimalNumber;
  LValues: TNyxSequenceArray<TNyxDecimalNumber>;
  LSource: INyxSequence<TNyxDecimalNumber>;
  LOptions: array of TOptionalDecimal;
  LOptionalSource: INyxSequence<TOptionalDecimal>;
  LOptional: TOptionalDecimal;
  LRejected: Boolean;
  LPresent: Boolean;
  LMode: TNyxDecimalRounding;
  LIndex: Integer;
  LData: TNyxDataValue;
begin
  LValue := Decimal('0.1').Add(Decimal('0.2'));
  Check(LValue.ToText = '0.3', 'decimal addition never enters binary floating arithmetic');
  LOther := Decimal('1.2300');
  Check((LOther.ToText = '1.23') and (LOther.CompareTo(Decimal('123e-2')) = 0),
    'decimal scale/exponent spellings normalize numerically');
  LMaximum := TNyxDecimalNumber.FromParts($ffffffff, $ffffffff, $ffffffff);
  Check(LMaximum.ToText = '79228162514264337593543950335', 'all 96 coefficient bits remain exact');
  LValue := LMaximum.Subtract(LMaximum);
  Check((LValue.ToText = '0') and not LValue.Negative, 'decimal cancellation normalizes zero');
  LValue := Decimal('-0.000');
  Check((LValue.ToText = '0') and not LValue.Negative, 'negative decimal zero normalizes');
  LRejected := False;
  try
    LValue := LMaximum.Add(TNyxDecimalNumber.FromInteger(1));
  except
    on LException: ENyxDecimalOverflow do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'decimal coefficient overflow refuses');
  LRejected := False;
  try
    LValue := Decimal('1.245').Quantize(2);
  except
    on LException: ENyxDecimalPrecision do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'exact quantization refuses digit loss');
  Check(Decimal('1.245').Quantize(2, ndrToEven).ToText = '1.24', 'nearest ties-even rounds an even retained digit');
  Check(Decimal('1.255').Quantize(2, ndrToEven).ToText = '1.26', 'nearest ties-even rounds an odd retained digit');
  Check(Decimal('1.2451').Quantize(2, ndrToEven).ToText = '1.25', 'rounding examines all discarded digits once');
  Check(Decimal('-1.245').Quantize(2, ndrAwayFromZero).ToText = '-1.25', 'nearest away-from-zero tie preserves sign');
  Check(Decimal('-1.241').Quantize(2, ndrFloor).ToText = '-1.25', 'floor rounding follows signed value');
  Check(Decimal('-1.249').Quantize(2, ndrCeiling).ToText = '-1.24', 'ceiling rounding follows signed value');
  Check(Decimal('-1.249').Quantize(2, ndrTowardZero).ToText = '-1.24', 'toward-zero rounding truncates signed magnitude');
  LValue := TNyxDecimalNumber.FromDecimal(NyxDecimal('1e-29'), ndrToEven);
  Check(LValue.ToText = '0', 'sub-quantum decimal nearest rounding returns zero');
  LValue := TNyxDecimalNumber.FromDecimal(NyxDecimal('-1e-29'), ndrFloor);
  Check(LValue.ToText = '-0.0000000000000000000000000001', 'directed tiny negative rounding retains one quantum');
  LRejected := False;
  try
    LValue := Decimal('1e-29');
  except
    on LException: ENyxDecimalPrecision do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'exact source admission refuses under-quantum digit loss');
  LData := NyxData(Decimal('9007199254740993.25').ToDecimal);
  Check(LData.ToJSON = '9007199254740993.25', 'decimal data preserves fractional digits above 2^53');
  SetLength(LValues, 2);
  LValues[0] := Decimal('0.1');
  LValues[1] := Decimal('0.2');
  LSource := TNyxSequence<TNyxDecimalNumber>.From(LValues);
  Check(TNyxDecimalAggregate.Sum(LSource).ToText = '0.3', 'decimal aggregate returns exact sum');
  Check(TNyxDecimalAggregate.Average(LSource).ToText = '0.15', 'decimal mean divides exactly');
  LValues[0] := LMaximum;
  LValues[1] := LMaximum;
  LSource := TNyxSequence<TNyxDecimalNumber>.From(LValues);
  Check(TNyxDecimalAggregate.Average(LSource).CompareTo(LMaximum) = 0,
    'wider intermediate lets maximum-valued mean succeed');
  LRejected := False;
  try
    LValue := TNyxDecimalAggregate.Sum(LSource);
  except
    on LException: ENyxDecimalOverflow do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'wide intermediate still refuses unrepresentable final sum');
  SetLength(LValues, 3);
  LValues[0] := TNyxDecimalNumber.FromInteger(1);
  LValues[1] := TNyxDecimalNumber.FromInteger(0);
  LValues[2] := TNyxDecimalNumber.FromInteger(0);
  LSource := TNyxSequence<TNyxDecimalNumber>.From(LValues);
  Check(TNyxDecimalAggregate.Average(LSource).ToText = '0.3333333333333333333333333333',
    'nonterminating mean rounds explicitly to portable scale');
  LRejected := False;
  try
    LValue := TNyxDecimalAggregate.Average(LSource, ndrExact);
  except
    on LException: ENyxDecimalPrecision do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'caller can require exact nonterminating mean refusal');
  LPresent := TNyxDecimalAggregate.TryAverage(TNyxSequence<TNyxDecimalNumber>.Empty, LValue);
  Check(not LPresent and (LValue.ToText = '0'), 'empty decimal TryAverage clears initialized zero');
  LOptions := nil;
  SetLength(LOptions, 2);
  LOptions[0] := TNyxOptional<TNyxDecimalNumber>.None;
  LOptions[1] := TNyxOptional<TNyxDecimalNumber>.Some(TNyxDecimalNumber.FromInteger(0));
  LOptionalSource := TNyxSequence<TOptionalDecimal>.From(LOptions);
  LOptional := TNyxDecimalAggregate.SumOptional(LOptionalSource);
  Check(LOptional.HasValue and (LOptional.Value.ToText = '0'), 'optional decimal preserves present zero');
  LOptions[1] := TNyxOptional<TNyxDecimalNumber>.None;
  LOptionalSource := TNyxSequence<TOptionalDecimal>.From(LOptions);
  LOptional := TNyxDecimalAggregate.AverageOptional(LOptionalSource);
  Check(not LOptional.HasValue, 'all-None decimal average is absent');
  LOptions[1] := nil;
  LOptionalSource := TNyxSequence<TOptionalDecimal>.From(LOptions);
  LRejected := False;
  try
    LOptional := TNyxDecimalAggregate.SumOptional(LOptionalSource);
  except
    on LException: ENyxQuery do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'nil optional decimal interface refuses');
  LValues[0] := Decimal('1.0');
  LValues[1] := Decimal('1e0');
  LValues[2] := Decimal('2');
  LSource := TNyxSequence<TNyxDecimalNumber>.From(LValues);
  Check(LSource.Distinct(NewNyxDecimalEquality).Count = 2, 'canonical decimal equality/hash agrees across spellings');
  Check(LSource.OrderDescending(NewNyxDecimalComparer).First.ToText = '2',
    'decimal ordering preserves numeric meaning');
  for LIndex := -64 to 64 do
  begin
    LValue := TNyxDecimalNumber.FromInteger(LIndex).Add(Decimal('0.125'));
    LOther := LValue.Subtract(Decimal('0.125'));
    Check(LOther.ToText = TNyxText(IntToStr(LIndex)), 'decimal arithmetic round-trips integer plus fraction');
  end;
  for LMode := Low(TNyxDecimalRounding) to High(TNyxDecimalRounding) do
  begin
    Check(Decimal('1.25').Quantize(2, LMode).ToText = '1.25', 'every rounding mode preserves an already exact value');
  end;
end;

procedure Singles;
const
  CQuantum: Double = 1.401298464324817070923729583289916131280e-45;
  CMaximum: Double = 3.40282346638528859811704183484516925440e38;
var
  LValues: TNyxSequenceArray<TNyxSingleNumber>;
  LSource: INyxSequence<TNyxSingleNumber>;
  LValue: Double;
  LResult: TNyxSingleNumber;
  LPresent: Boolean;
  LRejected: Boolean;
  LOptions: array of TOptionalSingle;
  LOptionalSource: INyxSequence<TOptionalSingle>;
  LOptional: TOptionalSingle;
  {$ifndef PAS2JS}
  LNative: TNyxNativeSingles;
  LRoundMode: TFPURoundingMode;
  {$endif}
begin
  {$ifndef PAS2JS}
  LRoundMode := GetRoundMode;
  {$endif}
  Check(NyxRoundSingle(16777217) = 16777216, 'Single narrowing chooses even lower midpoint');
  Check(NyxRoundSingle(16777219) = 16777220, 'Single narrowing chooses even upper midpoint');
  Check(NyxRoundSingle(CQuantum) = CQuantum, 'Single retains smallest positive subnormal');
  Check(NyxRoundSingle(CQuantum / 2) = 0, 'half-subnormal rounds to even zero');
  Check(NyxRoundSingle(CQuantum * 1.5) = CQuantum * 2, 'subnormal midpoint rounds to even upper quantum');
  Check(NyxRoundSingle(CMaximum) = CMaximum, 'maximum finite Single narrows without overflow');
  LRejected := False;
  try
    LValue := NyxRoundSingle(CMaximum * 2);
  except
    on LException: ENyxQuery do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'finite Single narrowing refuses rounded overflow');
  Check(IsInfinite(NyxRoundSingle(CMaximum * 2, nnpIEEE)), 'explicit Single IEEE policy permits infinity');
  SetLength(LValues, 3);
  LValues[0] := TNyxSingleNumber.FromNumber(16777216);
  LValues[1] := TNyxSingleNumber.FromNumber(1);
  LValues[2] := TNyxSingleNumber.FromNumber(-16777216);
  LSource := TNyxSequence<TNyxSingleNumber>.From(LValues);
  Check(TNyxSingleAggregate.Sum(LSource).Value = 1, 'wide Single sum retains cancellation-lost addend');
  LValues[0] := TNyxSingleNumber.FromNumber(2);
  LValues[1] := TNyxSingleNumber.FromNumber(4);
  LValues[2] := TNyxSingleNumber.FromNumber(6);
  LSource := TNyxSequence<TNyxSingleNumber>.From(LValues);
  Check(TNyxSingleAggregate.Average(LSource).Value = 4, 'Single mean returns narrowed portable result');
  LPresent := TNyxSingleAggregate.TryAverage(TNyxSequence<TNyxSingleNumber>.Empty, LResult);
  Check(not LPresent and (LResult.Value = 0), 'empty Single TryAverage clears zero');
  SetLength(LOptions, 2);
  LOptions[0] := TNyxOptional<TNyxSingleNumber>.None;
  LOptions[1] := TNyxOptional<TNyxSingleNumber>.Some(TNyxSingleNumber.FromNumber(0));
  LOptionalSource := TNyxSequence<TOptionalSingle>.From(LOptions);
  LOptional := TNyxSingleAggregate.SumOptional(LOptionalSource);
  Check(LOptional.HasValue and (LOptional.Value.Value = 0), 'optional Single sum preserves zero');
  LOptions[1] := TNyxOptional<TNyxSingleNumber>.None;
  LOptionalSource := TNyxSequence<TOptionalSingle>.From(LOptions);
  LOptional := TNyxSingleAggregate.AverageOptional(LOptionalSource);
  Check(not LOptional.HasValue, 'all-None Single mean is absent');
  {$ifndef PAS2JS}
  Check(GetRoundMode = LRoundMode, 'Single arithmetic preserves native FPU rounding state');
  SetLength(LNative, 2);
  LNative[0] := 0.25;
  LNative[1] := 0.5;
  LSource := LNative.AsNyx;
  LNative[0] := 99;
  LNative := nil;
  Check(TNyxSingleAggregate.Sum(LSource).Value = 0.75,
    'native Single helper captures independent exact binary32 values');
  Check(NyxSingleToNative(TNyxSingleNumber.FromNumber(1.25)) = 1.25,
    'native Single bridge preserves representable value');
  {$endif}
end;

function RunNyxPrecisionChecks: Integer;
begin
  GChecks := 0;
  Decimals;
  Singles;
  Result := GChecks;
end;

end.
