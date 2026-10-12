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
unit nyx.linq.integer64;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.linq, nyx.linq.numeric, nyx.numbers.integer64;

type
  { Streaming exact signed-64 sums. Empty Sum is zero, overflow refuses without
    a partial result. Approximate means explicitly use Double and the shared
    stable running-mean contract; empty required means refuse, Try clears zero.
    Source/callback failures propagate; each terminal owns an independent cursor. }
  TNyxInteger64Aggregate = class
  public
    class function Sum(const ASource: INyxSequence<TNyxInteger64>): TNyxInteger64; static;
    class function TryApproximateAverage(const ASource: INyxSequence<TNyxInteger64>;
      out AValue: Double): Boolean; static;
    class function ApproximateAverage(const ASource: INyxSequence<TNyxInteger64>): Double; static;
    { Explicit None is skipped, including for present zero. Nil optional interfaces
      refuse. Optional empty sum is Some(0); optional empty mean is None. }
    class function SumOptional(const ASource: INyxSequence<INyxOptional<TNyxInteger64>>):
      INyxOptional<TNyxInteger64>; static;
    class function ApproximateAverageOptional(
      const ASource: INyxSequence<INyxOptional<TNyxInteger64>>): INyxOptional<Double>; static;
  end;
  { Compiler-visible owned optional adapters; no UI/source/callback back-links. }
  TQueryInteger64Present = class(TInterfacedObject, INyxPredicate<INyxOptional<TNyxInteger64>>)
    function Test(const AValue: INyxOptional<TNyxInteger64>): Boolean;
  end;
  TQueryInteger64Value = class(TInterfacedObject,
    INyxSelector<INyxOptional<TNyxInteger64>, TNyxInteger64>)
    function Project(const AValue: INyxOptional<TNyxInteger64>): TNyxInteger64;
  end;

{ Immutable signed-order comparer and canonical equality/hash factories.
  Equal canonical values have equal hashes; zero never retains a negative sign. }
function NewNyxInteger64Comparer: INyxComparer<TNyxInteger64>;
function NewNyxInteger64Equality: INyxEquality<TNyxInteger64>;

implementation

uses
  nyx.linq.grouping;

type
  TOptional64 = INyxOptional<TNyxInteger64>;
  TInteger64Comparer = class(TInterfacedObject, INyxComparer<TNyxInteger64>)
    function Compare(const ALeft, ARight: TNyxInteger64): Integer;
  end;
  TInteger64Equality = class(TInterfacedObject, INyxEquality<TNyxInteger64>)
  private
    FText: INyxEquality<TNyxText>;
  public
    constructor Create;
    function Equal(const ALeft, ARight: TNyxInteger64): Boolean;
    function Hash(const AValue: TNyxInteger64): Cardinal;
  end;

function Approximate(const AValue: TNyxInteger64): Double;
begin
  Result := AValue.ApproximateDouble;
end;

class function TNyxInteger64Aggregate.Sum(
  const ASource: INyxSequence<TNyxInteger64>): TNyxInteger64;
var
  LCursor: INyxEnumerator<TNyxInteger64>;
begin

  if ASource = nil then
  begin
    raise ENyxQuery.Create('Integer64 sum requires a source');
  end;
  LCursor := ASource.GetEnumerator;

  if LCursor = nil then
  begin
    raise ENyxQuery.Create('Integer64 sum source returned no cursor');
  end;
  Result := TNyxInteger64.FromInteger(0);
  while LCursor.MoveNext do
  begin
    Result := Result.Add(LCursor.Current);
  end;
end;

class function TNyxInteger64Aggregate.TryApproximateAverage(
  const ASource: INyxSequence<TNyxInteger64>; out AValue: Double): Boolean;
var
  LProjection: INyxSequence<Double>;
  LSelector: INyxSelector<TNyxInteger64, Double>;
begin
  LSelector := TNyxSelector<TNyxInteger64, Double>.From(Approximate);
  LProjection := TNyxSelect<TNyxInteger64, Double>.From(ASource, LSelector);
  Result := TNyxNumberAggregate.TryAverage(LProjection, AValue);
end;

class function TNyxInteger64Aggregate.ApproximateAverage(
  const ASource: INyxSequence<TNyxInteger64>): Double;
begin

  if not TryApproximateAverage(ASource, Result) then
  begin
    raise ENyxQueryEmpty.Create('Integer64 mean requires a nonempty source');
  end;
end;

function TQueryInteger64Present.Test(const AValue: INyxOptional<TNyxInteger64>): Boolean;
begin

  if AValue = nil then
  begin
    raise ENyxQuery.Create('Optional Integer64 source contains a nil interface');
  end;
  Result := AValue.HasValue;
end;

function TQueryInteger64Value.Project(const AValue: INyxOptional<TNyxInteger64>): TNyxInteger64;
begin
  Result := AValue.Value;
end;

function Present(const ASource: INyxSequence<TOptional64>): INyxSequence<TNyxInteger64>;
var
  LPredicate: INyxPredicate<TOptional64>;
  LSelector: INyxSelector<TOptional64, TNyxInteger64>;
  LFiltered: INyxSequence<TOptional64>;
begin

  if ASource = nil then
  begin
    raise ENyxQuery.Create('Optional Integer64 aggregate requires a source');
  end;
  LPredicate := TQueryInteger64Present.Create;
  LSelector := TQueryInteger64Value.Create;
  LFiltered := ASource.Where(LPredicate);
  Result := TNyxSelect<TOptional64, TNyxInteger64>.From(LFiltered, LSelector);
end;

class function TNyxInteger64Aggregate.SumOptional(const ASource: INyxSequence<TOptional64>):
  INyxOptional<TNyxInteger64>;
var
  LValues: INyxSequence<TNyxInteger64>;
  LSum: TNyxInteger64;
begin
  LValues := Present(ASource);
  LSum := Sum(LValues);
  Result := TNyxOptional<TNyxInteger64>.Some(LSum);
end;

class function TNyxInteger64Aggregate.ApproximateAverageOptional(
  const ASource: INyxSequence<TOptional64>): INyxOptional<Double>;
var
  LValues: INyxSequence<TNyxInteger64>;
  LMean: Double;
begin
  LValues := Present(ASource);

  if TryApproximateAverage(LValues, LMean) then
  begin
    Result := TNyxOptional<Double>.Some(LMean);
  end
  else
  begin
    Result := TNyxOptional<Double>.None;
  end;
end;

function TInteger64Comparer.Compare(const ALeft, ARight: TNyxInteger64): Integer;
begin
  Result := ALeft.CompareTo(ARight);
end;

constructor TInteger64Equality.Create;
begin
  inherited Create;
  FText := NewNyxTextEquality;
end;

function TInteger64Equality.Equal(const ALeft, ARight: TNyxInteger64): Boolean;
begin
  Result := ALeft.CompareTo(ARight) = 0;
end;

function TInteger64Equality.Hash(const AValue: TNyxInteger64): Cardinal;
begin
  Result := FText.Hash(AValue.ToText);
end;

function NewNyxInteger64Comparer: INyxComparer<TNyxInteger64>;
begin
  Result := TInteger64Comparer.Create;
end;

function NewNyxInteger64Equality: INyxEquality<TNyxInteger64>;
begin
  Result := TInteger64Equality.Create;
end;

end.
