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
unit nyx.linq.decimal;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.linq, nyx.linq.numeric, nyx.numbers.decimal;

type
  { Complete exact accumulation before admitting one decimal result. Sum defaults
    to refusing precision loss; mean defaults to nearest ties-even. Other policies
    are caller choices. A wider owned intermediate prevents needless mean overflow.
    Empty sum=0, required empty mean refuses, Try clears zero. Nil inputs/cursors
    refuse; original source/decimal failures propagate without partial output. }
  TNyxDecimalAggregate = class
  public
    class function Sum(const ASource: INyxSequence<TNyxDecimalNumber>;
      ARounding: TNyxDecimalRounding = ndrExact): TNyxDecimalNumber; static;
    class function TryAverage(const ASource: INyxSequence<TNyxDecimalNumber>;
      out AValue: TNyxDecimalNumber;
      ARounding: TNyxDecimalRounding = ndrToEven): Boolean; static;
    class function Average(const ASource: INyxSequence<TNyxDecimalNumber>;
      ARounding: TNyxDecimalRounding = ndrToEven): TNyxDecimalNumber; static;
    { None skips only explicit absence. Nil optional interfaces refuse; Some(0)
      is present. All-None sum is Some(0), all-None average is None. }
    class function SumOptional(const ASource: INyxSequence<INyxOptional<TNyxDecimalNumber>>;
      ARounding: TNyxDecimalRounding = ndrExact): INyxOptional<TNyxDecimalNumber>; static;
    class function AverageOptional(const ASource: INyxSequence<INyxOptional<TNyxDecimalNumber>>;
      ARounding: TNyxDecimalRounding = ndrToEven): INyxOptional<TNyxDecimalNumber>; static;
  end;
  TQueryDecimalPresent = class(TInterfacedObject, INyxPredicate<INyxOptional<TNyxDecimalNumber>>)
    function Test(const AValue: INyxOptional<TNyxDecimalNumber>): Boolean;
  end;
  TQueryDecimalValue = class(TInterfacedObject,
    INyxSelector<INyxOptional<TNyxDecimalNumber>, TNyxDecimalNumber>)
    function Project(const AValue: INyxOptional<TNyxDecimalNumber>): TNyxDecimalNumber;
  end;

{ Immutable numeric-order/equality factories. Canonical equivalent decimal
  values share a hash, independently of admitted source scale/exponent spelling. }
function NewNyxDecimalComparer: INyxComparer<TNyxDecimalNumber>;
function NewNyxDecimalEquality: INyxEquality<TNyxDecimalNumber>;

implementation

uses
  nyx.linq.grouping;

type
  TOptionalDecimal = INyxOptional<TNyxDecimalNumber>;
  TDecimalComparer = class(TInterfacedObject, INyxComparer<TNyxDecimalNumber>)
    function Compare(const ALeft, ARight: TNyxDecimalNumber): Integer;
  end;
  TDecimalEquality = class(TInterfacedObject, INyxEquality<TNyxDecimalNumber>)
  private
    FText: INyxEquality<TNyxText>;
  public
    constructor Create;
    function Equal(const ALeft, ARight: TNyxDecimalNumber): Boolean;
    function Hash(const AValue: TNyxDecimalNumber): Cardinal;
  end;

function Accumulate(const ASource: INyxSequence<TNyxDecimalNumber>): TNyxDecimalAccumulator;
var
  LCursor: INyxEnumerator<TNyxDecimalNumber>;
begin

  if ASource = nil then
  begin
    raise ENyxQuery.Create('Decimal aggregate requires a source');
  end;
  Result := TNyxDecimalAccumulator.Create;
  try
    LCursor := ASource.GetEnumerator;

    if LCursor = nil then
    begin
      raise ENyxQuery.Create('Decimal aggregate source returned no cursor');
    end;
    while LCursor.MoveNext do
    begin
      Result.Push(LCursor.Current);
    end;
  except
    Result.Free;
    raise;
  end;
end;

class function TNyxDecimalAggregate.Sum(const ASource: INyxSequence<TNyxDecimalNumber>;
  ARounding: TNyxDecimalRounding): TNyxDecimalNumber;
var
  LSum: TNyxDecimalAccumulator;
begin
  LSum := Accumulate(ASource);
  try
    Result := LSum.Total(ARounding);
  finally
    LSum.Free;
  end;
end;

class function TNyxDecimalAggregate.TryAverage(const ASource: INyxSequence<TNyxDecimalNumber>;
  out AValue: TNyxDecimalNumber; ARounding: TNyxDecimalRounding): Boolean;
var
  LSum: TNyxDecimalAccumulator;
begin
  AValue := TNyxDecimalNumber.FromInteger(0);
  LSum := Accumulate(ASource);
  try
    Result := LSum.Count > 0;

    if Result then
    begin
      AValue := LSum.Average(ARounding);
    end;
  finally
    LSum.Free;
  end;
end;

class function TNyxDecimalAggregate.Average(const ASource: INyxSequence<TNyxDecimalNumber>;
  ARounding: TNyxDecimalRounding): TNyxDecimalNumber;
begin

  if not TryAverage(ASource, Result, ARounding) then
  begin
    raise ENyxQueryEmpty.Create('Decimal average requires a nonempty source');
  end;
end;

function TQueryDecimalPresent.Test(const AValue: INyxOptional<TNyxDecimalNumber>): Boolean;
begin

  if AValue = nil then
  begin
    raise ENyxQuery.Create('Optional decimal source contains a nil interface');
  end;
  Result := AValue.HasValue;
end;

function TQueryDecimalValue.Project(const AValue: INyxOptional<TNyxDecimalNumber>):
  TNyxDecimalNumber;
begin
  Result := AValue.Value;
end;

function Present(const ASource: INyxSequence<TOptionalDecimal>): INyxSequence<TNyxDecimalNumber>;
var
  LPredicate: INyxPredicate<TOptionalDecimal>;
  LSelector: INyxSelector<TOptionalDecimal, TNyxDecimalNumber>;
  LFiltered: INyxSequence<TOptionalDecimal>;
begin

  if ASource = nil then
  begin
    raise ENyxQuery.Create('Optional decimal aggregate requires a source');
  end;
  LPredicate := TQueryDecimalPresent.Create;
  LSelector := TQueryDecimalValue.Create;
  LFiltered := ASource.Where(LPredicate);
  Result := TNyxSelect<TOptionalDecimal, TNyxDecimalNumber>.From(LFiltered, LSelector);
end;

class function TNyxDecimalAggregate.SumOptional(const ASource: INyxSequence<TOptionalDecimal>;
  ARounding: TNyxDecimalRounding): TOptionalDecimal;
var
  LValues: INyxSequence<TNyxDecimalNumber>;
  LSum: TNyxDecimalNumber;
begin
  LValues := Present(ASource);
  LSum := Sum(LValues, ARounding);
  Result := TNyxOptional<TNyxDecimalNumber>.Some(LSum);
end;

class function TNyxDecimalAggregate.AverageOptional(const ASource: INyxSequence<TOptionalDecimal>;
  ARounding: TNyxDecimalRounding): TOptionalDecimal;
var
  LValues: INyxSequence<TNyxDecimalNumber>;
  LMean: TNyxDecimalNumber;
begin
  LValues := Present(ASource);

  if TryAverage(LValues, LMean, ARounding) then
  begin
    Result := TNyxOptional<TNyxDecimalNumber>.Some(LMean);
  end
  else
  begin
    Result := TNyxOptional<TNyxDecimalNumber>.None;
  end;
end;

function TDecimalComparer.Compare(const ALeft, ARight: TNyxDecimalNumber): Integer;
begin
  Result := ALeft.CompareTo(ARight);
end;

constructor TDecimalEquality.Create;
begin
  inherited Create;
  FText := NewNyxTextEquality;
end;

function TDecimalEquality.Equal(const ALeft, ARight: TNyxDecimalNumber): Boolean;
begin
  Result := ALeft.CompareTo(ARight) = 0;
end;

function TDecimalEquality.Hash(const AValue: TNyxDecimalNumber): Cardinal;
begin
  Result := FText.Hash(AValue.ToText);
end;

function NewNyxDecimalComparer: INyxComparer<TNyxDecimalNumber>;
begin
  Result := TDecimalComparer.Create;
end;

function NewNyxDecimalEquality: INyxEquality<TNyxDecimalNumber>;
begin
  Result := TDecimalEquality.Create;
end;

end.
