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
unit nyx.linq.single;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.linq, nyx.linq.numeric, nyx.numbers.single;

type
  { Typed inputs hold explicitly narrowed binary32 values on both targets.
    Shared compensated Double accumulation and stable mean computation
    then narrow the final result once. Finite/IEEE policy is explicit; no ambient
    FPU change. Empty sum=0, required empty mean refuses, Try clears ordinary zero.
    Original failures propagate and optional contracts follow other numeric types. }
  TNyxSingleAggregate = class
  public
    class function Sum(const ASource: INyxSequence<TNyxSingleNumber>;
      APolicy: TNyxNumberPolicy = nnpFinite): TNyxSingleNumber; static;
    class function TryAverage(const ASource: INyxSequence<TNyxSingleNumber>; out AValue: TNyxSingleNumber;
      APolicy: TNyxNumberPolicy = nnpFinite): Boolean; static;
    class function Average(const ASource: INyxSequence<TNyxSingleNumber>;
      APolicy: TNyxNumberPolicy = nnpFinite): TNyxSingleNumber; static;
    class function SumOptional(const ASource: INyxSequence<INyxOptional<TNyxSingleNumber>>;
      APolicy: TNyxNumberPolicy = nnpFinite): INyxOptional<TNyxSingleNumber>; static;
    class function AverageOptional(const ASource: INyxSequence<INyxOptional<TNyxSingleNumber>>;
      APolicy: TNyxNumberPolicy = nnpFinite): INyxOptional<TNyxSingleNumber>; static;
  end;
  { Compiler-visible immutable precision and optional adapters. }
  TQuerySingleProjection = class(TInterfacedObject, INyxSelector<TNyxSingleNumber, Double>)
  private
    FPolicy: TNyxNumberPolicy;
  public
    constructor Create(APolicy: TNyxNumberPolicy);
    function Project(const AValue: TNyxSingleNumber): Double;
  end;
  TQuerySinglePresent = class(TInterfacedObject, INyxPredicate<INyxOptional<TNyxSingleNumber>>)
    function Test(const AValue: INyxOptional<TNyxSingleNumber>): Boolean;
  end;
  TQuerySingleValue = class(TInterfacedObject, INyxSelector<INyxOptional<TNyxSingleNumber>, TNyxSingleNumber>)
    function Project(const AValue: INyxOptional<TNyxSingleNumber>): TNyxSingleNumber;
  end;

implementation

type
  TOptionalSingle = INyxOptional<TNyxSingleNumber>;

constructor TQuerySingleProjection.Create(APolicy: TNyxNumberPolicy);
begin
  inherited Create;
  FPolicy := APolicy;
end;

function TQuerySingleProjection.Project(const AValue: TNyxSingleNumber): Double;
begin
  Result := NyxRoundSingle(AValue.Value, FPolicy);
end;

function Narrowed(const ASource: INyxSequence<TNyxSingleNumber>;
  APolicy: TNyxNumberPolicy): INyxSequence<Double>;
var
  LSelector: INyxSelector<TNyxSingleNumber, Double>;
  LValidation: Double;
begin
  { Validate policy even for empty inputs, before obtaining any source cursor. }
  LValidation := NyxRoundSingle(0, APolicy);
  LSelector := TQuerySingleProjection.Create(APolicy);
  Result := TNyxSelect<TNyxSingleNumber, Double>.From(ASource, LSelector);
end;

class function TNyxSingleAggregate.Sum(const ASource: INyxSequence<TNyxSingleNumber>;
  APolicy: TNyxNumberPolicy): TNyxSingleNumber;
var
  LValues: INyxSequence<Double>;
  LSum: Double;
begin
  LValues := Narrowed(ASource, APolicy);
  LSum := TNyxNumberAggregate.Sum(LValues, APolicy);
  Result := TNyxSingleNumber.FromNumber(LSum, APolicy);
end;

class function TNyxSingleAggregate.TryAverage(const ASource: INyxSequence<TNyxSingleNumber>;
  out AValue: TNyxSingleNumber; APolicy: TNyxNumberPolicy): Boolean;
var
  LValues: INyxSequence<Double>;
  LMean: Double;
begin
  AValue := TNyxSingleNumber.FromNumber(0);
  LValues := Narrowed(ASource, APolicy);
  Result := TNyxNumberAggregate.TryAverage(LValues, LMean, APolicy);

  if Result then
  begin
    AValue := TNyxSingleNumber.FromNumber(LMean, APolicy);
  end;
end;

class function TNyxSingleAggregate.Average(const ASource: INyxSequence<TNyxSingleNumber>;
  APolicy: TNyxNumberPolicy): TNyxSingleNumber;
begin

  if not TryAverage(ASource, Result, APolicy) then
  begin
    raise ENyxQueryEmpty.Create('TNyxSingleNumber average requires a nonempty source');
  end;
end;

function TQuerySinglePresent.Test(const AValue: INyxOptional<TNyxSingleNumber>): Boolean;
begin

  if AValue = nil then
  begin
    raise ENyxQuery.Create('Optional TNyxSingleNumber source contains a nil interface');
  end;
  Result := AValue.HasValue;
end;

function TQuerySingleValue.Project(const AValue: INyxOptional<TNyxSingleNumber>): TNyxSingleNumber;
begin
  Result := AValue.Value;
end;

function Present(const ASource: INyxSequence<TOptionalSingle>): INyxSequence<TNyxSingleNumber>;
var
  LPredicate: INyxPredicate<TOptionalSingle>;
  LSelector: INyxSelector<TOptionalSingle, TNyxSingleNumber>;
  LFiltered: INyxSequence<TOptionalSingle>;
begin

  if ASource = nil then
  begin
    raise ENyxQuery.Create('Optional TNyxSingleNumber aggregate requires a source');
  end;
  LPredicate := TQuerySinglePresent.Create;
  LSelector := TQuerySingleValue.Create;
  LFiltered := ASource.Where(LPredicate);
  Result := TNyxSelect<TOptionalSingle, TNyxSingleNumber>.From(LFiltered, LSelector);
end;

class function TNyxSingleAggregate.SumOptional(const ASource: INyxSequence<TOptionalSingle>;
  APolicy: TNyxNumberPolicy): TOptionalSingle;
var
  LValues: INyxSequence<TNyxSingleNumber>;
  LSum: TNyxSingleNumber;
begin
  LValues := Present(ASource);
  LSum := Sum(LValues, APolicy);
  Result := TNyxOptional<TNyxSingleNumber>.Some(LSum);
end;

class function TNyxSingleAggregate.AverageOptional(const ASource: INyxSequence<TOptionalSingle>;
  APolicy: TNyxNumberPolicy): TOptionalSingle;
var
  LValues: INyxSequence<TNyxSingleNumber>;
  LMean: TNyxSingleNumber;
begin
  LValues := Present(ASource);

  if TryAverage(LValues, LMean, APolicy) then
  begin
    Result := TNyxOptional<TNyxSingleNumber>.Some(LMean);
  end
  else
  begin
    Result := TNyxOptional<TNyxSingleNumber>.None;
  end;
end;

end.
