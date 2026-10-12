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

unit nyx.linq.numeric;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.linq;

type
  { Typed absence: zero, nil and empty text can all be present values. None.Value
    refuses; optional numeric operators skip only explicit absent entries, never
    silently interpret a nil optional interface as absence. }
  INyxOptional<T> = interface(IInterface)
    function GetHasValue: Boolean;
    function GetValue: T;
    property HasValue: Boolean read GetHasValue;
    property Value: T read GetValue;
  end;
  TNyxOptional<T> = class
  public
    class function Some(const AValue: T): INyxOptional<T>; static;
    class function None: INyxOptional<T>; static;
  end;
  TQueryOptional<T> = class(TInterfacedObject, INyxOptional<T>)
  private
    FValue: T;
    FHasValue: Boolean;
  public
    constructor Create(const AValue: T; AHasValue: Boolean);
    function GetHasValue: Boolean;
    function GetValue: T;
  end;
  { Finite rejects NaN/infinity and finite overflow. IEEE explicitly propagates
    non-finite values/overflow; behavior is implemented without changing global
    FPU exception masks. Both policies are portable closed choices. }
  TNyxNumberPolicy = (nnpFinite, nnpIEEE);
  TNyxIntegerAggregate = class
  public
    { Checked Integer sum: empty=0. Average streams a stable Double running mean,
      avoiding a silently rounded wide integer total. Empty required average
      refuses; TryAverage returns False and zero. Counts are checked Integer. }
    class function Sum(const ASource: INyxSequence<Integer>): Integer; static;
    class function TryAverage(const ASource: INyxSequence<Integer>;
      out AValue: Double): Boolean; static;
    class function Average(const ASource: INyxSequence<Integer>): Double; static;
    class function SumOptional(const ASource: INyxSequence<INyxOptional<Integer>>):
      INyxOptional<Integer>; static;
    class function AverageOptional(const ASource: INyxSequence<INyxOptional<Integer>>):
      INyxOptional<Double>; static;
  end;
  TNyxNumberAggregate = class
  public
    { Double sum uses compensated streaming accumulation, with explicit policy.
      Average uses an overflow-aware running mean: two large finite values can
      average successfully even when their ordinary sum is outside Double.
      No partial result on source/optional/value failure. Empty sums are zero;
      all-absent optional average is None; optional sum is present zero. }
    class function Sum(const ASource: INyxSequence<Double>;
      APolicy: TNyxNumberPolicy = nnpFinite): Double; static;
    class function TryAverage(const ASource: INyxSequence<Double>; out AValue: Double;
      APolicy: TNyxNumberPolicy = nnpFinite): Boolean; static;
    class function Average(const ASource: INyxSequence<Double>;
      APolicy: TNyxNumberPolicy = nnpFinite): Double; static;
    class function SumOptional(const ASource: INyxSequence<INyxOptional<Double>>;
      APolicy: TNyxNumberPolicy = nnpFinite): INyxOptional<Double>; static;
    class function AverageOptional(const ASource: INyxSequence<INyxOptional<Double>>;
      APolicy: TNyxNumberPolicy = nnpFinite): INyxOptional<Double>; static;
  end;

{ Independently checked scalar addition. Integer overflow always refuses,
  independently of compiler overflow switches; numeric policy is explicit. }
function NyxAddQueryInteger(ALeft, ARight: Integer): Integer;
function NyxAddQueryNumber(ALeft, ARight: Double;
  APolicy: TNyxNumberPolicy = nnpFinite): Double;

implementation

uses
  Math, nyx.linq.aggregates;

const
  CQueryMaxDouble: Double = 1.7976931348623157e308;

type
  TOptionalInteger = INyxOptional<Integer>;
  TOptionalNumber = INyxOptional<Double>;
  TNumberAccumulator = record
    Sum: Double;
    Correction: Double;
    Count: Integer;
    Mean: Double;
  end;
  TOptionalIntegerSelector = class(TInterfacedObject,
    INyxSelector<INyxOptional<Integer>, Integer>)
    function Project(const AValue: INyxOptional<Integer>): Integer;
  end;
  TOptionalNumberSelector = class(TInterfacedObject,
    INyxSelector<INyxOptional<Double>, Double>)
    function Project(const AValue: INyxOptional<Double>): Double;
  end;
  TOptionalIntegerPredicate = class(TInterfacedObject, INyxPredicate<INyxOptional<Integer>>)
    function Test(const AValue: INyxOptional<Integer>): Boolean;
  end;
  TOptionalNumberPredicate = class(TInterfacedObject, INyxPredicate<INyxOptional<Double>>)
    function Test(const AValue: INyxOptional<Double>): Boolean;
  end;

procedure AdmitNumberPolicy(APolicy: TNyxNumberPolicy);
begin

  if (Ord(APolicy) < Ord(Low(TNyxNumberPolicy))) or
    (Ord(APolicy) > Ord(High(TNyxNumberPolicy))) then
  begin
    raise ENyxQuery.Create('Numeric aggregate requires a valid number policy');
  end;
end;

function NyxAddQueryInteger(ALeft, ARight: Integer): Integer;
begin

  if ((ARight > 0) and (ALeft > High(Integer) - ARight)) or
    ((ARight < 0) and (ALeft < Low(Integer) - ARight)) then
  begin
    raise ENyxQuery.Create('Integer aggregate overflow');
  end;
  Result := ALeft + ARight;
end;

function NyxAddQueryNumber(ALeft, ARight: Double; APolicy: TNyxNumberPolicy): Double;
var
  LHalf: Double;
  LRightHalf: Double;
  LHalfLimit: Double;
begin
  AdmitNumberPolicy(APolicy);

  if IsNan(ALeft) or IsNan(ARight) then
  begin

    if APolicy = nnpFinite then
    begin
      raise ENyxQuery.Create('Finite aggregate refuses NaN');
    end;
    Exit(NaN);
  end;

  if IsInfinite(ALeft) or IsInfinite(ARight) then
  begin

    if APolicy = nnpFinite then
    begin
      raise ENyxQuery.Create('Finite aggregate refuses infinity');
    end;

    if IsInfinite(ALeft) and IsInfinite(ARight) and (ALeft <> ARight) then
    begin
      Exit(NaN);
    end;

    if IsInfinite(ALeft) then
    begin
      Exit(ALeft);
    end;
    Exit(ARight);
  end;

  LHalfLimit := CQueryMaxDouble / 2;

  if (((ALeft > 0) and (ARight > 0)) or ((ALeft < 0) and (ARight < 0))) and
    ((Abs(ALeft) >= LHalfLimit) or (Abs(ARight) >= LHalfLimit)) then
  begin
    { Power-of-two scaling is exact for these large operands. Commit each
      operation to Double so native extended intermediates cannot change the
      browser overflow decision. The actual risky addition is never executed. }
    LHalf := ALeft / 2;
    LRightHalf := ARight / 2;
    LHalf := LHalf + LRightHalf;

    if Abs(LHalf) <= LHalfLimit then
    begin
      Result := LHalf * 2;
      Exit;
    end;

    if APolicy = nnpFinite then
    begin
      raise ENyxQuery.Create('Finite numeric aggregate overflow');
    end;

    if ARight > 0 then
    begin
      Exit(Infinity);
    end;
    Exit(NegInfinity);
  end;
  Result := ALeft + ARight;
end;

constructor TQueryOptional<T>.Create(const AValue: T; AHasValue: Boolean);
begin
  inherited Create;
  FHasValue := AHasValue;

  if AHasValue then
  begin
    FValue := AValue;
  end;
end;

function TQueryOptional<T>.GetHasValue: Boolean;
begin
  Result := FHasValue;
end;

function TQueryOptional<T>.GetValue: T;
begin

  if not FHasValue then
  begin
    raise ENyxQueryEmpty.Create('Optional value is absent');
  end;
  Result := FValue;
end;

class function TNyxOptional<T>.Some(const AValue: T): INyxOptional<T>;
begin
  Result := TQueryOptional<T>.Create(AValue, True);
end;

class function TNyxOptional<T>.None: INyxOptional<T>;
var
  LEmpty: TQueryEmptyValue<T>;
begin
  LEmpty := TQueryEmptyValue<T>.Create;
  try
    Result := TQueryOptional<T>.Create(LEmpty.Value, False);
  finally
    LEmpty.Free;
  end;
end;

class function TNyxIntegerAggregate.Sum(const ASource: INyxSequence<Integer>): Integer;
var
  LCursor: INyxEnumerator<Integer>;
begin

  if ASource = nil then
  begin
    raise ENyxQuery.Create('Integer sum requires a source');
  end;
  LCursor := ASource.GetEnumerator;

  if LCursor = nil then
  begin
    raise ENyxQuery.Create('Integer sum source returned no cursor');
  end;
  Result := 0;
  while LCursor.MoveNext do
  begin
    Result := NyxAddQueryInteger(Result, LCursor.Current);
  end;
end;

procedure PushMean(var AState: TNumberAccumulator; AValue: Double;
  APolicy: TNyxNumberPolicy);
var
  LValue: Double;
  LWeight: Double;
  LPrevious: Double;
  LContribution: Double;
begin
  LValue := NyxAddQueryNumber(0, AValue, APolicy);
  AState.Count := NyxIncrementQueryCount(AState.Count);

  if AState.Count = 1 then
  begin
    AState.Mean := LValue;
    Exit;
  end;

  if IsNan(AState.Mean) or IsNan(LValue) or IsInfinite(AState.Mean) or IsInfinite(LValue) then
  begin
    AState.Mean := NyxAddQueryNumber(AState.Mean, LValue, APolicy);
    Exit;
  end;
  LWeight := 1 / AState.Count;

  if ((AState.Mean < 0) and (LValue > 0)) or
    ((AState.Mean > 0) and (LValue < 0)) then
  begin
    { Opposite-sign subtraction might overflow, while weighted addition cannot. }
    LPrevious := 1 - LWeight;
    LPrevious := AState.Mean * LPrevious;
    LContribution := LValue * LWeight;
    AState.Mean := LPrevious + LContribution;
  end
  else
  begin
    { Same-sign difference is bounded and the new mean stays between values. }
    LContribution := LValue - AState.Mean;
    LContribution := LContribution * LWeight;
    AState.Mean := AState.Mean + LContribution;
  end;
end;

class function TNyxIntegerAggregate.TryAverage(const ASource: INyxSequence<Integer>;
  out AValue: Double): Boolean;
var
  LCursor: INyxEnumerator<Integer>;
  LState: TNumberAccumulator;
begin
  AValue := 0;

  if ASource = nil then
  begin
    raise ENyxQuery.Create('Integer average requires a source');
  end;
  LState.Sum := 0;
  LState.Correction := 0;
  LState.Count := 0;
  LState.Mean := 0;
  LCursor := ASource.GetEnumerator;

  if LCursor = nil then
  begin
    raise ENyxQuery.Create('Integer average source returned no cursor');
  end;
  while LCursor.MoveNext do
  begin
    PushMean(LState, LCursor.Current, nnpFinite);
  end;
  Result := LState.Count > 0;

  if Result then
  begin
    AValue := LState.Mean;
  end;
end;

class function TNyxIntegerAggregate.Average(const ASource: INyxSequence<Integer>): Double;
begin

  if not TryAverage(ASource, Result) then
  begin
    raise ENyxQueryEmpty.Create('Integer average requires a nonempty source');
  end;
end;

class function TNyxNumberAggregate.Sum(const ASource: INyxSequence<Double>;
  APolicy: TNyxNumberPolicy): Double;
var
  LCursor: INyxEnumerator<Double>;
  LSum: Double;
  LCorrection: Double;
  LNext: Double;
  LValue: Double;
  LDelta: Double;
begin
  AdmitNumberPolicy(APolicy);

  if ASource = nil then
  begin
    raise ENyxQuery.Create('Number sum requires a source');
  end;
  LSum := 0;
  LCorrection := 0;
  LCursor := ASource.GetEnumerator;

  if LCursor = nil then
  begin
    raise ENyxQuery.Create('Number sum source returned no cursor');
  end;
  while LCursor.MoveNext do
  begin
    LValue := LCursor.Current;
    LNext := NyxAddQueryNumber(LSum, LValue, APolicy);

    if not IsNan(LNext) and not IsInfinite(LNext) then
    begin
      { Neumaier compensation retains small addends lost to cancellation.
        Magnitude selection keeps the correction difference bounded. }

      if Abs(LSum) >= Abs(LValue) then
      begin
        LDelta := LSum - LNext;
        LDelta := LDelta + LValue;
      end
      else
      begin
        LDelta := LValue - LNext;
        LDelta := LDelta + LSum;
      end;
      LCorrection := NyxAddQueryNumber(LCorrection, LDelta, APolicy);
    end
    else
    begin
      LCorrection := 0;
    end;
    LSum := LNext;
  end;
  Result := NyxAddQueryNumber(LSum, LCorrection, APolicy);
end;

class function TNyxNumberAggregate.TryAverage(const ASource: INyxSequence<Double>;
  out AValue: Double; APolicy: TNyxNumberPolicy): Boolean;
var
  LCursor: INyxEnumerator<Double>;
  LState: TNumberAccumulator;
begin
  AValue := 0;
  AdmitNumberPolicy(APolicy);

  if ASource = nil then
  begin
    raise ENyxQuery.Create('Number average requires a source');
  end;
  LState.Sum := 0;
  LState.Correction := 0;
  LState.Count := 0;
  LState.Mean := 0;
  LCursor := ASource.GetEnumerator;

  if LCursor = nil then
  begin
    raise ENyxQuery.Create('Number average source returned no cursor');
  end;
  while LCursor.MoveNext do
  begin
    PushMean(LState, LCursor.Current, APolicy);
  end;
  Result := LState.Count > 0;

  if Result then
  begin
    AValue := LState.Mean;
  end;
end;

class function TNyxNumberAggregate.Average(const ASource: INyxSequence<Double>;
  APolicy: TNyxNumberPolicy): Double;
begin

  if not TryAverage(ASource, Result, APolicy) then
  begin
    raise ENyxQueryEmpty.Create('Number average requires a nonempty source');
  end;
end;

function TOptionalIntegerSelector.Project(const AValue: INyxOptional<Integer>): Integer;
begin
  Result := AValue.Value;
end;

function TOptionalNumberSelector.Project(const AValue: INyxOptional<Double>): Double;
begin
  Result := AValue.Value;
end;

function TOptionalIntegerPredicate.Test(const AValue: INyxOptional<Integer>): Boolean;
begin

  if AValue = nil then
  begin
    raise ENyxQuery.Create('Optional integer entry is an invalid nil interface');
  end;
  Result := AValue.HasValue;
end;

function TOptionalNumberPredicate.Test(const AValue: INyxOptional<Double>): Boolean;
begin

  if AValue = nil then
  begin
    raise ENyxQuery.Create('Optional number entry is an invalid nil interface');
  end;
  Result := AValue.HasValue;
end;

class function TNyxIntegerAggregate.SumOptional(
  const ASource: INyxSequence<INyxOptional<Integer>>): INyxOptional<Integer>;
var
  LFilter: INyxPredicate<INyxOptional<Integer>>;
  LProject: INyxSelector<INyxOptional<Integer>, Integer>;
  LValues: INyxSequence<Integer>;
begin

  if ASource = nil then
  begin
    raise ENyxQuery.Create('Optional integer sum requires a source');
  end;
  LFilter := TOptionalIntegerPredicate.Create;
  LProject := TOptionalIntegerSelector.Create;
  LValues := TNyxSelect<TOptionalInteger, Integer>.From(ASource.Where(LFilter), LProject);
  Result := TNyxOptional<Integer>.Some(Sum(LValues));
end;

class function TNyxIntegerAggregate.AverageOptional(
  const ASource: INyxSequence<INyxOptional<Integer>>): INyxOptional<Double>;
var
  LFilter: INyxPredicate<INyxOptional<Integer>>;
  LProject: INyxSelector<INyxOptional<Integer>, Integer>;
  LValues: INyxSequence<Integer>;
  LValue: Double;
begin

  if ASource = nil then
  begin
    raise ENyxQuery.Create('Optional integer average requires a source');
  end;
  LFilter := TOptionalIntegerPredicate.Create;
  LProject := TOptionalIntegerSelector.Create;
  LValues := TNyxSelect<TOptionalInteger, Integer>.From(ASource.Where(LFilter), LProject);

  if TryAverage(LValues, LValue) then
  begin
    Result := TNyxOptional<Double>.Some(LValue);
  end
  else
  begin
    Result := TNyxOptional<Double>.None;
  end;
end;

class function TNyxNumberAggregate.SumOptional(
  const ASource: INyxSequence<INyxOptional<Double>>;
  APolicy: TNyxNumberPolicy): INyxOptional<Double>;
var
  LFilter: INyxPredicate<INyxOptional<Double>>;
  LProject: INyxSelector<INyxOptional<Double>, Double>;
  LValues: INyxSequence<Double>;
begin
  AdmitNumberPolicy(APolicy);

  if ASource = nil then
  begin
    raise ENyxQuery.Create('Optional number sum requires a source');
  end;
  LFilter := TOptionalNumberPredicate.Create;
  LProject := TOptionalNumberSelector.Create;
  LValues := TNyxSelect<TOptionalNumber, Double>.From(ASource.Where(LFilter), LProject);
  Result := TNyxOptional<Double>.Some(Sum(LValues, APolicy));
end;

class function TNyxNumberAggregate.AverageOptional(
  const ASource: INyxSequence<INyxOptional<Double>>;
  APolicy: TNyxNumberPolicy): INyxOptional<Double>;
var
  LFilter: INyxPredicate<INyxOptional<Double>>;
  LProject: INyxSelector<INyxOptional<Double>, Double>;
  LValues: INyxSequence<Double>;
  LValue: Double;
begin
  AdmitNumberPolicy(APolicy);

  if ASource = nil then
  begin
    raise ENyxQuery.Create('Optional number average requires a source');
  end;
  LFilter := TOptionalNumberPredicate.Create;
  LProject := TOptionalNumberSelector.Create;
  LValues := TNyxSelect<TOptionalNumber, Double>.From(ASource.Where(LFilter), LProject);

  if TryAverage(LValues, LValue, APolicy) then
  begin
    Result := TNyxOptional<Double>.Some(LValue);
  end
  else
  begin
    Result := TNyxOptional<Double>.None;
  end;
end;

end.
