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

unit nyx.test.linq.aggregates;

{$mode delphi}{$H+}{$codepage utf8}
{$include nyx.callback.features.inc}

interface

function RunNyxAggregateChecks: Integer;

implementation

uses
  SysUtils, Math, nyx.text, nyx.linq, nyx.linq.grouping,
  nyx.linq.aggregates, nyx.linq.numeric, nyx.callback.scope;

type
  TIntFold = INyxFoldValue<Integer, Integer>;
  TIntKeyValue = INyxKeyValue<Integer, Integer>;
  TTextFold = INyxFoldValue<TNyxText, TNyxText>;
  TOptionalInteger = INyxOptional<Integer>;
  TOptionalNumber = INyxOptional<Double>;
  EAggregateCallback = class(Exception);
  TCollisionEquality = class(TInterfacedObject, INyxEquality<Integer>)
  public
    Hashes: Integer;
    Comparisons: Integer;
    Collision: Boolean;
    function Equal(const ALeft, ARight: Integer): Boolean;
    function Hash(const AValue: Integer): Cardinal;
  end;
  TEntry = record
    Key: Integer;
    Title: TNyxText;
  end;
  TLifetime = class
    Created: Integer;
    Destroyed: Integer;
  end;
  IAccumulator = interface
    ['{A38F2085-805D-4DD0-9E29-8C388CE385B5}']
    function GetTotal: Integer;
  end;
  TAccumulator = class(TInterfacedObject, IAccumulator)
  private
    FLifetime: TLifetime;
    FTotal: Integer;
  public
    constructor Create(ALifetime: TLifetime; ATotal: Integer);
    destructor Destroy; override;
    function GetTotal: Integer;
  end;
  TManagedFold = INyxFoldValue<IAccumulator, Integer>;
  TManagedResult = INyxKeyValue<Integer, IAccumulator>;
  TManagedSeed = class(TInterfacedObject, INyxSelector<Integer, IAccumulator>)
  private
    FLifetime: TLifetime;
  public
    constructor Create(ALifetime: TLifetime);
    function Project(const AKey: Integer): IAccumulator;
  end;
  TManagedFolder = class(TInterfacedObject, INyxSelector<TManagedFold, IAccumulator>)
  private
    FLifetime: TLifetime;
  public
    constructor Create(ALifetime: TLifetime);
    function Project(const AValue: TManagedFold): IAccumulator;
  end;
  TNilSource = class(TNyxSequence<Integer>)
    function GetEnumerator: INyxEnumerator<Integer>; override;
  end;
  TNilOptionalSource = class(TNyxSequence<TOptionalInteger>)
    function GetEnumerator: INyxEnumerator<TOptionalInteger>; override;
  end;

var
  GChecks: Integer;
  GKeys: Integer;
  GFolds: Integer;
  GSeeds: Integer;
  GResultCalls: Integer;
  GTextContext: TTextFold;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxQuery.Create('Aggregate qualification: ' + AReason);
  end;
  Inc(GChecks);
end;

function SumFolder(const AValue: TIntFold): Integer;
begin
  Inc(GFolds);
  Result := NyxAddQueryInteger(AValue.Accumulator, AValue.Value);
end;

function FailFolder(const AValue: TIntFold): Integer;
begin
  Result := AValue.Accumulator;
  raise EAggregateCallback.Create('folder failed / 🌙');
end;

function ExactKey(const AValue: Integer): Integer;
begin
  Inc(GKeys);
  Result := AValue;
end;

function Parity(const AValue: Integer): Integer;
begin
  Inc(GKeys);
  Result := AValue mod 2;
end;

function FailKey(const AValue: Integer): Integer;
begin
  Result := AValue;

  if AValue = 3 then
  begin
    raise EAggregateCallback.Create('key failed / 🌙');
  end;
end;

function SeedForKey(const AValue: Integer): Integer;
begin
  Inc(GSeeds);
  Result := AValue * 10;
end;

function ResultText(const AValue: Integer): TNyxText;
begin
  Inc(GResultCalls);
  Result := TNyxText(IntToStr(AValue)) + TNyxText(' / 🌙');
end;

function TextFolder(const AValue: TTextFold): TNyxText;
begin
  GTextContext := AValue;
  Result := AValue.Accumulator + AValue.Value;
end;

function TextKey(const AValue: TNyxText): TNyxText;
begin
  Result := AValue;
end;

function EntryKey(const AValue: TEntry): Integer;
begin
  Inc(GKeys);
  Result := AValue.Key;
end;

function TCollisionEquality.Equal(const ALeft, ARight: Integer): Boolean;
begin
  Inc(Comparisons);
  Result := ALeft = ARight;
end;

function TCollisionEquality.Hash(const AValue: Integer): Cardinal;
begin
  Inc(Hashes);

  if Collision then
  begin
    Result := High(Cardinal);
  end
  else
  begin
    Result := NewNyxIntegerEquality.Hash(AValue);
  end;
end;

constructor TAccumulator.Create(ALifetime: TLifetime; ATotal: Integer);
begin
  inherited Create;
  FLifetime := ALifetime;
  FTotal := ATotal;
  Inc(FLifetime.Created);
end;

destructor TAccumulator.Destroy;
begin
  Inc(FLifetime.Destroyed);
  inherited Destroy;
end;

function TAccumulator.GetTotal: Integer;
begin
  Result := FTotal;
end;

constructor TManagedSeed.Create(ALifetime: TLifetime);
begin
  inherited Create;
  FLifetime := ALifetime;
end;

function TManagedSeed.Project(const AKey: Integer): IAccumulator;
begin
  Result := TAccumulator.Create(FLifetime, AKey * 10);
end;

constructor TManagedFolder.Create(ALifetime: TLifetime);
begin
  inherited Create;
  FLifetime := ALifetime;
end;

function TManagedFolder.Project(const AValue: TManagedFold): IAccumulator;
var
  LAccumulator: IAccumulator;
begin
  LAccumulator := AValue.Accumulator;
  Result := TAccumulator.Create(FLifetime,
    NyxAddQueryInteger(LAccumulator.GetTotal, AValue.Value));
end;

function TNilSource.GetEnumerator: INyxEnumerator<Integer>;
begin
  Result := nil;
end;

function TNilOptionalSource.GetEnumerator: INyxEnumerator<TOptionalInteger>;
begin
  Result := nil;
end;

procedure Folds;
var
  LSource: INyxSequence<Integer>;
  LInvalid: INyxSequence<Integer>;
  LFolder: INyxSelector<TIntFold, Integer>;
  LRefused: Boolean;
  LText: TNyxText;
begin
  LSource := NyxRange(1, 3);
  LFolder := TNyxSelector<TIntFold, Integer>.From(SumFolder);
  GFolds := 0;
  Check(TNyxAggregate<Integer, Integer>.Fold(LSource, 10, LFolder) = 16, 'seeded fold accumulates each source row');
  Check(GFolds = 3, 'seeded folder enters once per consumed row');
  GFolds := 0;
  Check(TNyxReduce<Integer>.Fold(LSource, LFolder) = 6, 'unseeded reduction uses first value as seed');
  Check(GFolds = 2, 'unseeded folder skips first source value');
  Check(TNyxAggregate<Integer, Integer>.Fold(TNyxSequence<Integer>.Empty, 10, LFolder) = 10,
    'empty seeded fold returns its typed seed');
  GResultCalls := 0;
  LText := TNyxAggregateResult<Integer, Integer, TNyxText>.Fold(LSource, 10, LFolder,
    TNyxSelector<Integer, TNyxText>.From(ResultText));
  Check((LText = TNyxText('16 / 🌙')) and (GResultCalls = 1), 'typed result selector enters once after complete fold');
  LRefused := False;
  try
    TNyxReduce<Integer>.Fold(TNyxSequence<Integer>.Empty, LFolder);
  except
    on ENyxQueryEmpty do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'unseeded empty fold refuses rather than inventing a seed');
  LRefused := False;
  try
    TNyxAggregate<Integer, Integer>.Fold(LSource, 0, TNyxSelector<TIntFold, Integer>.From(FailFolder));
  except
    on EAggregateCallback do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'folder failure propagates its original type');
  LRefused := False;
  { Own the invalid fixture before a generic call can raise; a raw constructor
    expression otherwise leaves its implicit interface conversion unretired. }
  LInvalid := TNilSource.Create;
  try
    TNyxAggregate<Integer, Integer>.Fold(LInvalid, 0, LFolder);
  except
    on ENyxQuery do
    begin
      LRefused := True;
    end;
  end;
  LInvalid := nil;
  Check(LRefused, 'nil aggregate source cursor refuses');
  LText := TNyxAggregate<TNyxText, TNyxText>.Fold(
    TNyxSequence<TNyxText>.From([TNyxText('🌙'), TNyxText(#0)]), 'Seed / ',
    TNyxSelector<TTextFold, TNyxText>.From(TextFolder));
  Check(LText = TNyxText('Seed / 🌙') + TNyxText(#0), 'fold preserves supplementary/NUL text');
  Check(GTextContext.Accumulator = TNyxText('Seed / 🌙'), 'retained immutable fold context owns its prior accumulator');
  GTextContext := nil;
end;

procedure Keyed;
var
  LSource: INyxSequence<Integer>;
  LQuery: INyxSequence<TIntKeyValue>;
  LCursor: INyxEnumerator<TIntKeyValue>;
  LOther: INyxEnumerator<TIntKeyValue>;
  LValues: TNyxSequenceArray<TIntKeyValue>;
  LRefused: Boolean;
  LProbe: TCollisionEquality;
  LEquality: INyxEquality<Integer>;
  LKeys: INyxSelector<Integer, Integer>;
  LTextCounts: INyxSequence<INyxKeyValue<TNyxText, Integer>>;
  LTextValues: TNyxSequenceArray<INyxKeyValue<TNyxText, Integer>>;
  LIndex: Integer;
begin
  LSource := TNyxSequence<Integer>.From([3, 1, 3, 2, 1]);
  LKeys := TNyxSelector<Integer, Integer>.From(ExactKey);
  LProbe := TCollisionEquality.Create;
  LProbe.Collision := True;
  LEquality := LProbe;
  GKeys := 0;
  LQuery := TNyxCountBy<Integer, Integer>.From(LSource, LKeys, LEquality);
  LCursor := LQuery.GetEnumerator;
  LOther := LQuery.GetEnumerator;
  Check((GKeys = 0) and (LProbe.Hashes = 0), 'key aggregation construction/acquisition is deferred');
  Check(LQuery.Take(0).Count = 0, 'zero take enters no keyed preparation');
  Check(LCursor.MoveNext and (GKeys = 5) and (LProbe.Hashes = 5),
    'complete indexed admission projects/hashes once per row');
  Check((LCursor.Current.Key = 3) and (LCursor.Current.Value = 2), 'first-key order and duplicate count remain stable');
  Check(LOther.MoveNext and (GKeys = 10), 'each aggregate cursor admits independently');
  LCursor := nil;
  LOther := nil;
  LValues := LQuery.ToArray;
  Check((Length(LValues) = 3) and (LValues[1].Key = 1) and (LValues[1].Value = 2)
    and (LValues[2].Key = 2) and (LValues[2].Value = 1), 'colliding keys preserve exact count/order');
  GSeeds := 0;
  LQuery := TNyxAggregateBy<Integer, Integer, Integer>.From(LSource,
    TNyxSelector<Integer, Integer>.From(Parity), NewNyxIntegerEquality,
    TNyxSelector<Integer, Integer>.From(SeedForKey), TNyxSelector<TIntFold, Integer>.From(SumFolder));
  LValues := LQuery.ToArray;
  Check((GSeeds = 2) and (LValues[0].Key = 1) and (LValues[0].Value = 18)
    and (LValues[1].Value = 2), 'per-key seed enters once and typed folder preserves group accumulation');
  LCursor := TNyxCountBy<Integer, Integer>.From(LSource,
    TNyxSelector<Integer, Integer>.From(FailKey), NewNyxIntegerEquality).GetEnumerator;
  LRefused := False;
  try
    LCursor.MoveNext;
  except
    on EAggregateCallback do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and not LCursor.MoveNext, 'late key failure exposes no partial aggregate sequence');
  LTextCounts := TNyxCountBy<TNyxText, TNyxText>.From(
    TNyxSequence<TNyxText>.From([TNyxText('A🌙') + TNyxText(#0), TNyxText('a🌙') + TNyxText(#0)]),
    TNyxSelector<TNyxText, TNyxText>.From(TextKey), NewNyxTextEquality(ntoAsciiInsensitive));
  LTextValues := LTextCounts.ToArray;
  Check((Length(LTextValues) = 1) and (LTextValues[0].Value = 2)
    and (LTextValues[0].Key = TNyxText('A🌙') + TNyxText(#0)), 'folded scalar text keys preserve the first exact key');
  LProbe := TCollisionEquality.Create;
  LEquality := LProbe;
  LQuery := TNyxCountBy<Integer, Integer>.From(NyxRange(0, 4096).Concat(NyxRange(0, 4096)),
    LKeys, LEquality);
  LValues := LQuery.ToArray;
  Check((Length(LValues) = 4096) and (LProbe.Hashes = 8192) and (LProbe.Comparisons = 4096),
    'large key counts cache hashes during growth instead of retaining/scanning full groups');
  for LIndex := 0 to High(LValues) do
  begin
    Check((LValues[LIndex].Key = LIndex) and (LValues[LIndex].Value = 2), 'complete large keyed count/order');
  end;
  LRefused := False;
  try
    NyxIncrementQueryCount(High(Integer));
  except
    on ENyxQuery do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'count overflow refuses before arithmetic');
end;

procedure Extrema;
var
  LSource: INyxSequence<Integer>;
  LEntries: TNyxSequenceArray<TEntry>;
  LResult: TEntry;
  LRefused: Boolean;
  LValue: Integer;
  LPresent: Boolean;
begin
  LSource := TNyxSequence<Integer>.From([High(Integer), Low(Integer), 0]);
  Check(TNyxExtrema<Integer>.Min(LSource, NewNyxIntegerComparer) = Low(Integer), 'minimum supports both signed limits');
  Check(TNyxExtrema<Integer>.Max(LSource, NewNyxIntegerComparer) = High(Integer), 'maximum compares without subtracting extremes');
  LPresent := TNyxExtrema<Integer>.TryMin(TNyxSequence<Integer>.Empty,
    NewNyxIntegerComparer, LValue);
  Check(not LPresent and (LValue = 0), 'empty optional extrema clears ordinary initialized result');
  SetLength(LEntries, 3);
  LEntries[0].Key := 1;
  LEntries[0].Title := 'First';
  LEntries[1].Key := 1;
  LEntries[1].Title := 'Second';
  LEntries[2].Key := 3;
  LEntries[2].Title := 'Third';
  GKeys := 0;
  LResult := TNyxExtremaBy<TEntry, Integer>.MinBy(TNyxSequence<TEntry>.From(LEntries),
    TNyxSelector<TEntry, Integer>.From(EntryKey), NewNyxIntegerComparer);
  Check((LResult.Title = 'First') and (GKeys = 3), 'key extrema project once and retain first ties');
  LResult := TNyxExtremaBy<TEntry, Integer>.MaxBy(TNyxSequence<TEntry>.From(LEntries),
    TNyxSelector<TEntry, Integer>.From(EntryKey), NewNyxIntegerComparer);
  Check(LResult.Title = 'Third', 'key extrema return the original typed source value');
  LRefused := False;
  try
    TNyxExtrema<Integer>.Max(TNyxSequence<Integer>.Empty, NewNyxIntegerComparer);
  except
    on ENyxQueryEmpty do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'required empty extrema refuses');
end;

procedure Numeric;
const
  CMaximum: Double = 1.7976931348623157e308;
var
  LRefused: Boolean;
  LMean: Double;
  LInts: INyxSequence<Integer>;
  LNumbers: INyxSequence<Double>;
  LOptionalInts: TNyxSequenceArray<TOptionalInteger>;
  LOptionalNumbers: TNyxSequenceArray<TOptionalNumber>;
  LOptionalInt: TOptionalInteger;
  LOptionalNumber: TOptionalNumber;
  LInteger: Integer;
  LInvalid: INyxSequence<TOptionalInteger>;
begin
  LInts := TNyxSequence<Integer>.From([1, 2, 3]);
  Check(TNyxIntegerAggregate.Sum(LInts) = 6, 'integer sum is exact');
  Check(TNyxIntegerAggregate.Sum(TNyxSequence<Integer>.Empty) = 0, 'empty integer sum is zero');
  Check(TNyxIntegerAggregate.Average(LInts) = 2, 'integer average streams a Double mean');
  Check(TNyxIntegerAggregate.Average(TNyxSequence<Integer>.From([Low(Integer), High(Integer)])) = -0.5,
    'integer mean preserves opposite signed limits without an overflowing total');
  Check(not TNyxIntegerAggregate.TryAverage(TNyxSequence<Integer>.Empty, LMean) and (LMean = 0),
    'empty optional average returns absence and zero');
  LRefused := False;
  try
    TNyxIntegerAggregate.Sum(TNyxSequence<Integer>.From([High(Integer), 1]));
  except
    on ENyxQuery do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'integer sum refuses positive overflow');
  LRefused := False;
  try
    NyxAddQueryInteger(Low(Integer), -1);
  except
    on ENyxQuery do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'integer sum refuses negative overflow');
  LNumbers := TNyxSequence<Double>.From([1e16, 1, -1e16]);
  Check(TNyxNumberAggregate.Sum(LNumbers) = 1, 'compensated sum retains cancellation-lost addend');
  Check(TNyxNumberAggregate.Average(TNyxSequence<Double>.From([CMaximum, CMaximum])) = CMaximum,
    'large finite average avoids a needless overflowing total');
  Check(TNyxNumberAggregate.Average(TNyxSequence<Double>.From([CMaximum, -CMaximum])) = 0,
    'opposite large finite average avoids overflowing subtraction');
  Check(NyxAddQueryNumber(CMaximum, 1) = CMaximum, 'near-limit rounded addition agrees across targets');
  LRefused := False;
  try
    TNyxNumberAggregate.Sum(TNyxSequence<Double>.From([CMaximum, CMaximum]));
  except
    on ENyxQuery do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'finite number policy refuses overflow portably');
  Check(IsInfinite(TNyxNumberAggregate.Sum(TNyxSequence<Double>.From([CMaximum, CMaximum]), nnpIEEE)),
    'explicit IEEE policy propagates overflowing infinity');
  Check(IsNan(TNyxNumberAggregate.Sum(TNyxSequence<Double>.From([Infinity, NegInfinity]), nnpIEEE)),
    'explicit opposite infinities produce NaN without global FPU mask changes');
  LRefused := False;
  try
    TNyxNumberAggregate.Sum(TNyxSequence<Double>.From([NaN]));
  except
    on ENyxQuery do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'default finite policy refuses NaN');
  Check(IsNan(TNyxNumberAggregate.Average(TNyxSequence<Double>.From([NaN, 1]), nnpIEEE)),
    'explicit IEEE mean propagates NaN');
  LOptionalInt := TNyxOptional<Integer>.None;
  Check(not LOptionalInt.HasValue, 'typed None is explicit absence');
  LRefused := False;
  try
    LInteger := LOptionalInt.Value;
    Check(LInteger = 0, 'absent getter unexpectedly returned');
  except
    on ENyxQueryEmpty do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'absent optional getter refuses');
  SetLength(LOptionalInts, 3);
  LOptionalInts[0] := TNyxOptional<Integer>.Some(0);
  LOptionalInts[1] := TNyxOptional<Integer>.None;
  LOptionalInts[2] := TNyxOptional<Integer>.Some(4);
  LOptionalInt := TNyxIntegerAggregate.SumOptional(TNyxSequence<TOptionalInteger>.From(LOptionalInts));
  Check(LOptionalInt.HasValue and (LOptionalInt.Value = 4), 'optional integer sum skips only explicit absence');
  LOptionalNumber := TNyxIntegerAggregate.AverageOptional(TNyxSequence<TOptionalInteger>.From(LOptionalInts));
  Check(LOptionalNumber.HasValue and (LOptionalNumber.Value = 2), 'present zero participates in optional average');
  LOptionalNumber := TNyxIntegerAggregate.AverageOptional(TNyxSequence<TOptionalInteger>.Empty);
  Check(not LOptionalNumber.HasValue, 'all-absent optional average is None');
  SetLength(LOptionalNumbers, 2);
  LOptionalNumbers[0] := TNyxOptional<Double>.Some(2.5);
  LOptionalNumbers[1] := TNyxOptional<Double>.Some(3.5);
  LOptionalNumber := TNyxNumberAggregate.SumOptional(TNyxSequence<TOptionalNumber>.From(LOptionalNumbers));
  Check(LOptionalNumber.HasValue and (LOptionalNumber.Value = 6), 'optional number sum retains typed values');
  LOptionalNumbers[0] := nil;
  LRefused := False;
  try
    TNyxNumberAggregate.SumOptional(TNyxSequence<TOptionalNumber>.From(LOptionalNumbers));
  except
    on ENyxQuery do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'nil optional entry refuses instead of silently becoming None');
  LInvalid := TNilOptionalSource.Create;
  LRefused := False;
  try
    TNyxIntegerAggregate.SumOptional(LInvalid);
  except
    on ENyxQuery do
    begin
      LRefused := True;
    end;
  end;
  LInvalid := nil;
  Check(LRefused, 'optional adapters refuse invalid source cursors portably');
end;

procedure CallbackScopes;
var
  LLease: INyxCallbackLease;
  LVariable: TNyxSelectorNested<TIntFold, Integer>;
  LFolder: INyxSelector<TIntFold, Integer>;
  LQuery: INyxSequence<TIntKeyValue>;
  LOffset: Integer;
  LRefused: Boolean;

  function NestedFolder(const AValue: TIntFold): Integer;
  begin
    Result := AValue.Accumulator + AValue.Value + LOffset;
  end;

begin
  LOffset := 2;
  LLease := NewNyxCallbackLease;
  try
    LVariable := NestedFolder;
    LFolder := TNyxSelector<TIntFold, Integer>.FromNested(LVariable, LLease);
    Check(TNyxAggregate<Integer, Integer>.Fold(NyxRange(1, 3), 0, LFolder) = 12,
      'named nested fold callback preserves scalar scope');
    {$ifdef NYX_HAS_INLINE_CALLBACKS}
    Check(TNyxAggregate<Integer, Integer>.Fold(NyxRange(1, 3), 0,
      TNyxSelector<TIntFold, Integer>.FromInline(
        function(const AValue: TIntFold): Integer
        begin
          Result := AValue.Accumulator + AValue.Value + LOffset;
        end)) = 12, 'qualified inline folder preserves typed context and scalar capture');
    {$endif}
    LQuery := TNyxAggregateBy<Integer, Integer, Integer>.Seeded(NyxRange(1, 3),
      TNyxSelector<Integer, Integer>.From(Parity), NewNyxIntegerEquality, 0, LFolder);
  finally
    LLease.Close;
  end;
  LRefused := False;
  try
    LQuery.Any;
  except
    on ENyxCallbackExpired do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'closed aggregate folder lease refuses deferred frame entry');
end;

procedure ManagedSeeds;
var
  LLifetime: TLifetime;
  LSeed: INyxSelector<Integer, IAccumulator>;
  LFolder: INyxSelector<TManagedFold, IAccumulator>;
  LQuery: INyxSequence<TManagedResult>;
  LCursor: INyxEnumerator<TManagedResult>;
  LResult: TManagedResult;
  LRead: IAccumulator;
begin
  LLifetime := TLifetime.Create;
  try
    LSeed := TManagedSeed.Create(LLifetime);
    LFolder := TManagedFolder.Create(LLifetime);
    LQuery := TNyxAggregateBy<Integer, Integer, IAccumulator>.From(
      TNyxSequence<Integer>.From([1, 3, 2]), TNyxSelector<Integer, Integer>.From(Parity),
      NewNyxIntegerEquality, LSeed, LFolder);
    LCursor := LQuery.GetEnumerator;
    LQuery := nil;
    Check(LCursor.MoveNext, 'managed per-key accumulator exists');
    LResult := LCursor.Current;
    LRead := LResult.Value;
    Check(LRead.GetTotal = 14, 'fresh managed seed and immutable replacements preserve first-key accumulation');
    LRead := nil;
    LSeed := nil;
    LFolder := nil;
    LCursor := nil;
    Check(LLifetime.Created - LLifetime.Destroyed = 1, 'retained keyed result owns only its managed accumulator');
    LRead := LResult.Value;
    LResult := nil;
    Check(LRead.GetTotal = 14, 'independent reader survives keyed result release');
    LRead := nil;
    Check(LLifetime.Created = LLifetime.Destroyed, 'last managed aggregate reader retires all accumulators once');
  finally
    LRead := nil;
    LResult := nil;
    LCursor := nil;
    LQuery := nil;
    LSeed := nil;
    LFolder := nil;
    LLifetime.Free;
  end;
end;

function RunNyxAggregateChecks: Integer;
begin
  GChecks := 0;
  try
    Folds;
    Keyed;
    Extrema;
    Numeric;
    CallbackScopes;
    ManagedSeeds;
    Result := GChecks;
  finally
    GTextContext := nil;
  end;
end;

end.
