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
unit nyx.test.linq.authoring;

{$mode delphi}{$H+}{$codepage utf8}
{$include nyx.callback.features.inc}

interface

function RunNyxLinqAuthoringChecks: Integer;

implementation

uses
  SysUtils, nyx.text, nyx.data, nyx.linq, nyx.linq.numeric,
  nyx.linq.integer64, nyx.numbers.integer64, nyx.callback.scope
  {$ifndef PAS2JS}, nyx.numbers.integer64.native{$endif};

type
  TOptional64 = INyxOptional<TNyxInteger64>;
  EAuthoringCallback = class(Exception);

var
  GChecks: Integer;
  GCalls: Integer;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create('LINQ authoring: ' + AReason);
  end;
  Inc(GChecks);
end;

function Even(const AValue: Integer): Boolean;
begin
  Inc(GCalls);
  Result := (AValue mod 2) = 0;
end;

function Three(const AValue: Integer): Boolean;
begin
  Inc(GCalls);
  Result := AValue = 3;
end;

function Never(const AValue: Integer): Boolean;
begin
  Inc(GCalls);
  Result := False;
end;

function Fail(const AValue: Integer): Boolean;
begin
  Result := False;
  raise EAuthoringCallback.Create('original predicate failure');
end;

function NeverInterface(const AValue: IInterface): Boolean;
begin
  Result := False;
end;

procedure PredicateAuthoring;
var
  LSource: INyxSequence<Integer>;
  LEmpty: INyxSequence<Integer>;
  LPredicate: INyxPredicate<Integer>;
  LReject: INyxPredicate<Integer>;
  LInterfaces: INyxSequence<IInterface>;
  LInterfacePredicate: INyxPredicate<IInterface>;
  LPayload: IInterface;
  LValue: Integer;
  LPresent: Boolean;
  LRejected: Boolean;
  LLease: INyxCallbackLease;
  LThreshold: Integer;

  function Above(const AValue: Integer): Boolean;
  begin
    Result := AValue > LThreshold;
  end;

begin
  LSource := TNyxSequence<Integer>.From([1, 2, 3, 4]);
  LEmpty := TNyxSequence<Integer>.Empty;
  LPredicate := TNyxPredicate<Integer>.From(Even);
  LReject := TNyxPredicate<Integer>.From(Never);
  GCalls := 0;
  Check((LSource.First(LPredicate) = 2) and (GCalls = 2),
    'predicate First stops at first matching row');
  GCalls := 0;
  Check((LSource.Last(LPredicate) = 4) and (GCalls = 4),
    'predicate Last consumes matching source once');
  GCalls := 0;
  Check((LSource.Count(LPredicate) = 2) and (GCalls = 4),
    'predicate Count consumes matching source once');
  GCalls := 0;
  Check((LSource.Single(TNyxPredicate<Integer>.From(Three)) = 3) and (GCalls = 4),
    'predicate Single checks complete matching cardinality');
  LValue := 99;
  LPresent := LSource.TryFirst(LReject, LValue);
  Check(not LPresent and (LValue = 0), 'predicate TryFirst clears absent output');
  LValue := 99;
  LPresent := LSource.TryLast(LReject, LValue);
  Check(not LPresent and (LValue = 0), 'predicate TryLast clears absent output');
  LValue := 99;
  LPresent := LSource.TrySingle(LReject, LValue);
  Check(not LPresent and (LValue = 0), 'predicate TrySingle clears absent output');
  Check(LSource.FirstOrDefaultWhere(LReject) = 0, 'predicate default First uses initialized zero');
  Check(LSource.LastOrDefaultWhere(LReject, 91) = 91, 'predicate Last keeps explicit fallback');
  Check(LSource.SingleOrDefaultWhere(LReject, 92) = 92, 'predicate Single keeps explicit fallback');
  Check(LEmpty.LastOrDefaultWhere(LPredicate) = 0, 'empty predicate default Last is zero');
  Check(LEmpty.SingleOrDefaultWhere(LPredicate) = 0, 'empty predicate default Single is zero');
  LRejected := False;
  try
    LSource.SingleOrDefaultWhere(LPredicate, 99);
  except
    on LException: ENyxQueryCardinality do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'predicate Single default never conceals multiple matches');
  LRejected := False;
  try
    LSource.First(LReject);
  except
    on LException: ENyxQueryEmpty do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'required predicate First refuses absence');
  LRejected := False;
  try
    LSource.Count(nil);
  except
    on LException: ENyxQuery do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'nil terminal predicate refuses even without matching rows');
  LRejected := False;
  try
    LSource.Last(TNyxPredicate<Integer>.From(Fail));
  except
    on LException: EAuthoringCallback do
    begin
      LRejected := LException.Message = 'original predicate failure';
    end;
  end;
  Check(LRejected, 'terminal predicate preserves original failure');
  Check(LSource.OrderDescending(NewNyxIntegerComparer).First = 4,
    'whole-value descending shortcut uses stable ordering');
  Check(LSource.Order(NewNyxIntegerComparer).First = 1,
    'whole-value ascending shortcut uses stable ordering');
  LInterfaces := TNyxSequence<IInterface>.Empty;
  LInterfacePredicate := TNyxPredicate<IInterface>.From(NeverInterface);
  LPayload := LInterfaces.FirstOrDefault(LInterfacePredicate);
  Check(LPayload = LInterfacePredicate, 'predicate implementation can itself be an interface-valued fallback');
  LPayload := LInterfaces.FirstOrDefaultWhere(LInterfacePredicate, LInterfacePredicate);
  Check(LPayload = LInterfacePredicate, 'named predicate form keeps interface fallback unambiguous');
  LThreshold := 2;
  LLease := NewNyxCallbackLease;
  try
    LPredicate := TNyxPredicate<Integer>.FromNested(Above, LLease);
    Check(LSource.First(LPredicate) = 3, 'predicate terminal accepts scoped nested function');
    {$ifdef NYX_HAS_INLINE_CALLBACKS}
    Check(LSource.Last(TNyxPredicate<Integer>.FromInline(
      function(const AValue: Integer): Boolean
      begin
        Result := AValue > LThreshold;
      end)) = 4, 'predicate terminal accepts qualified inline capture');
    {$endif}
  finally
    LLease.Close;
  end;
  LRejected := False;
  try
    LSource.Count(LPredicate);
  except
    on LException: ENyxCallbackExpired do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'predicate terminal preserves closed nested lease');
end;

procedure ExactArithmetic;
const
  CMaximum: TNyxText = '9223372036854775807';
  CMinimum: TNyxText = '-9223372036854775808';
var
  LMaximum: TNyxInteger64;
  LMinimum: TNyxInteger64;
  LValue: TNyxInteger64;
  LOther: TNyxInteger64;
  LNumber: Integer;
  LRejected: Boolean;
  LIndex: Integer;
  LParsed: TNyxInteger64;
  LBad: array[0..5] of TNyxText;
  LData: TNyxDataValue;
begin
  LMaximum := TNyxInteger64.Parse(CMaximum);
  LMinimum := TNyxInteger64.Parse(CMinimum);
  Check((LMaximum.ToText = CMaximum) and (LMinimum.ToText = CMinimum),
    'signed Integer64 boundaries retain every decimal digit');
  LValue := TNyxInteger64.FromParts($200000, 1);
  Check(LValue.ToText = '9007199254740993', 'typed limb constructor stays exact above 2^53');
  LOther := LValue.Add(TNyxInteger64.FromInteger(1));
  Check(LOther.ToText = '9007199254740994', 'wide addition retains low integer bit');
  LOther := LOther.Subtract(LValue);
  Check(LOther.ToText = '1', 'wide subtraction preserves cancellation-lost bit');
  LOther := LMinimum.Subtract(LMinimum);
  Check((LOther.ToText = '0') and not LOther.Negative, 'minimum minus minimum normalizes zero');
  LOther := LMaximum.Add(LMinimum);
  Check(LOther.ToText = '-1', 'opposite signed boundaries cancel exactly');
  LOther := TNyxInteger64.FromParts(0, $ffffffff).Add(TNyxInteger64.FromInteger(1));
  Check((LOther.HighPart = 1) and (LOther.LowPart = 0), 'magnitude addition carries across Cardinal');
  LOther := LOther.Subtract(TNyxInteger64.FromInteger(1));
  Check((LOther.HighPart = 0) and (LOther.LowPart = $ffffffff), 'magnitude subtraction borrows across Cardinal');
  Check(LMinimum.CompareTo(LMaximum) = -1, 'wide comparator never subtracts signed extremes');
  Check(TNyxInteger64.FromInteger(Low(Integer)).TryInteger(LNumber) and
    (LNumber = Low(Integer)), 'negative Integer boundary enters/narrows exactly');
  Check(not LValue.TryInteger(LNumber) and (LNumber = 0), 'wide narrowing refuses with zero output');
  LData := NyxData(LValue.ToDecimal);
  Check(LData.ToJSON = '9007199254740993', 'wide value enters exact Nyx data without Double rounding');
  LOther := TNyxInteger64.Parse('-0');
  Check((LOther.ToText = '0') and not LOther.Negative, 'negative textual zero is normalized');
  LRejected := False;
  try
    LOther := LMaximum.Add(TNyxInteger64.FromInteger(1));
  except
    on LException: ENyxInteger64Overflow do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'positive signed overflow refuses');
  LRejected := False;
  try
    LOther := LMinimum.Subtract(TNyxInteger64.FromInteger(1));
  except
    on LException: ENyxInteger64Overflow do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'negative signed overflow refuses');
  LBad[0] := '9223372036854775808';
  LBad[1] := '-9223372036854775809';
  LBad[2] := '01';
  LBad[3] := '+1';
  LBad[4] := '1.0';
  LBad[5] := '1e3';
  for LIndex := 0 to 5 do
  begin
    LRejected := False;
    try
      LOther := TNyxInteger64.Parse(LBad[LIndex]);
    except
      on LException: ENyxInteger64 do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'strict integer boundary rejects unsupported spelling/domain');
  end;
  for LIndex := -512 to 512 do
  begin
    LValue := TNyxInteger64.FromInteger(LIndex);
    LOther := LValue.Add(TNyxInteger64.FromInteger(257));
    Check(LOther.ToText = TNyxText(IntToStr(LIndex + 257)),
      'checked arithmetic preserves signed small-domain results');
    LParsed := TNyxInteger64.Parse(LValue.ToText);
    Check(LParsed.CompareTo(LValue) = 0, 'strict integer text round-trips signed values');
  end;
end;

procedure WideQueries;
var
  LValues: TNyxSequenceArray<TNyxInteger64>;
  LSource: INyxSequence<TNyxInteger64>;
  LMaximum: TNyxInteger64;
  LResult: TNyxInteger64;
  LOptionals: array of TOptional64;
  LOptionalSource: INyxSequence<TOptional64>;
  LSum: TOptional64;
  LMean: INyxOptional<Double>;
  LMeanValue: Double;
  LPresent: Boolean;
  LRejected: Boolean;
  {$ifndef PAS2JS}
  LNative: TNyxNativeIntegers64;
  {$endif}
begin
  SetLength(LValues, 3);
  LValues[0] := TNyxInteger64.Parse('9007199254740993');
  LValues[1] := TNyxInteger64.Parse('-9007199254740992');
  LValues[2] := TNyxInteger64.FromInteger(3);
  LSource := TNyxSequence<TNyxInteger64>.From(LValues);
  LResult := TNyxInteger64Aggregate.Sum(LSource);
  Check(LResult.ToText = '4', 'streaming wide sum keeps cancellation-sensitive low bits');
  Check(LSource.Order(NewNyxInteger64Comparer).First.ToText = '-9007199254740992',
    'wide values participate in fluent exact ordering');
  Check(LSource.Distinct(NewNyxInteger64Equality).Count = 3,
    'wide equality/hash keeps distinct values above 2^53');
  LResult := TNyxInteger64Aggregate.Sum(TNyxSequence<TNyxInteger64>.Empty);
  Check(LResult.ToText = '0', 'empty exact sum is initialized zero');
  LPresent := TNyxInteger64Aggregate.TryApproximateAverage(
    TNyxSequence<TNyxInteger64>.Empty, LMeanValue);
  Check(not LPresent and (LMeanValue = 0), 'empty approximate mean clears output');
  LValues[0] := TNyxInteger64.FromInteger(2);
  LValues[1] := TNyxInteger64.FromInteger(4);
  LValues[2] := TNyxInteger64.FromInteger(6);
  LSource := TNyxSequence<TNyxInteger64>.From(LValues);
  Check(TNyxInteger64Aggregate.ApproximateAverage(LSource) = 4,
    'explicit approximate mean uses stable shared numeric contract');
  LMaximum := TNyxInteger64.Parse('9223372036854775807');
  SetLength(LValues, 2);
  LValues[0] := LMaximum;
  LValues[1] := TNyxInteger64.FromInteger(1);
  LSource := TNyxSequence<TNyxInteger64>.From(LValues);
  LRejected := False;
  try
    LResult := TNyxInteger64Aggregate.Sum(LSource);
  except
    on LException: ENyxInteger64Overflow do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'wide sum propagates checked overflow without partial admission');
  SetLength(LOptionals, 2);
  LOptionals[0] := TNyxOptional<TNyxInteger64>.None;
  LOptionals[1] := TNyxOptional<TNyxInteger64>.Some(TNyxInteger64.FromInteger(0));
  LOptionalSource := TNyxSequence<TOptional64>.From(LOptionals);
  LSum := TNyxInteger64Aggregate.SumOptional(LOptionalSource);
  Check(LSum.HasValue and (LSum.Value.ToText = '0'), 'optional wide sum preserves present zero');
  LOptionals[1] := TNyxOptional<TNyxInteger64>.None;
  LOptionalSource := TNyxSequence<TOptional64>.From(LOptionals);
  LMean := TNyxInteger64Aggregate.ApproximateAverageOptional(LOptionalSource);
  Check(not LMean.HasValue, 'all-None approximate wide mean is absent');
  LOptionals[1] := nil;
  LOptionalSource := TNyxSequence<TOptional64>.From(LOptionals);
  LRejected := False;
  try
    LSum := TNyxInteger64Aggregate.SumOptional(LOptionalSource);
  except
    on LException: ENyxQuery do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'nil optional wide interface is an invalid contract');
  {$ifndef PAS2JS}
  Check(NyxInteger64ToNative(LMaximum) = High(Int64), 'native maximum bridge is exact');
  Check(NyxInteger64FromNative(Low(Int64)).ToText = '-9223372036854775808',
    'native minimum bridge never negates its signed minimum');
  SetLength(LNative, 2);
  LNative[0] := Int64(9007199254740993);
  LNative[1] := Int64(-9007199254740992);
  LSource := LNative.AsNyx;
  LNative[0] := 99;
  LNative := nil;
  LResult := TNyxInteger64Aggregate.Sum(LSource);
  Check(LResult.ToText = '1', 'native helper captures detached exact wide values');
  {$endif}
end;

function RunNyxLinqAuthoringChecks: Integer;
begin
  GChecks := 0;
  PredicateAuthoring;
  ExactArithmetic;
  WideQueries;
  Result := GChecks;
end;

end.
