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

unit nyx.linq.aggregates;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.linq;

type
  { Immutable owned fold arguments. Accumulator and Value retain their exact
    Pascal types and ordinary managed ownership. A retained context never refers
    back to a source, cursor, mutable aggregate builder or UI. }
  INyxFoldValue<TAccumulator, T> = interface(IInterface)
    function GetAccumulator: TAccumulator;
    function GetValue: T;
    property Accumulator: TAccumulator read GetAccumulator;
    property Value: T read GetValue;
  end;
  TNyxFoldValue<TAccumulator, T> = class(TInterfacedObject,
    INyxFoldValue<TAccumulator, T>)
  private
    FAccumulator: TAccumulator;
    FValue: T;
  public
    constructor Create(const AAccumulator: TAccumulator; const AValue: T);
    function GetAccumulator: TAccumulator;
    function GetValue: T;
  end;
  { Independently retained keyed result. First equivalent source key supplies
    Key; Value is the completed typed accumulator, never a partial builder view. }
  INyxKeyValue<TKey, TValue> = interface(IInterface)
    function GetKey: TKey;
    function GetValue: TValue;
    property Key: TKey read GetKey;
    property Value: TValue read GetValue;
  end;
  TNyxKeyValue<TKey, TValue> = class(TInterfacedObject, INyxKeyValue<TKey, TValue>)
  private
    FKey: TKey;
    FValue: TValue;
  public
    constructor Create(const AKey: TKey; const AValue: TValue);
    function GetKey: TKey;
    function GetValue: TValue;
  end;

  { Immediate single-pass seeded fold. Empty input returns the captured seed;
    every consumed row invokes the folder once. Seed/payload ownership remains
    ordinary: mutable shared objects are not implicitly cloned or synchronized.
    Source/callback failure returns no result and propagates unchanged. }
  TNyxAggregate<T, TAccumulator> = class
  public
    class function Fold(const ASource: INyxSequence<T>; const ASeed: TAccumulator;
      const AFolder: INyxSelector<INyxFoldValue<TAccumulator, T>, TAccumulator>):
      TAccumulator; static;
  end;
  TNyxAggregateResult<T, TAccumulator, TResult> = class
  public
    { Result selector runs once after a successful fold, including an empty seeded
      input. All nil dependencies refuse before any source/callback entry. }
    class function Fold(const ASource: INyxSequence<T>; const ASeed: TAccumulator;
      const AFolder: INyxSelector<INyxFoldValue<TAccumulator, T>, TAccumulator>;
      const AResult: INyxSelector<TAccumulator, TResult>): TResult; static;
  end;
  TNyxReduce<T> = class
  public
    { First input value is the seed, and only subsequent rows invoke the folder.
      Empty input raises ENyxQueryEmpty; one input returns without folder entry. }
    class function Fold(const ASource: INyxSequence<T>;
      const AFolder: INyxSelector<INyxFoldValue<T, T>, T>): T; static;
  end;

  TNyxAggregateBy<T, TKey, TAccumulator> = class
  public
    { Deferred complete keyed admission. Each cursor projects/hashes once per row,
      retains first-key order and seeds each new key once. From permits fresh
      per-key managed seeds; Seeded reuses ordinary copies of the supplied seed.
      Mutable seed aliases remain the caller's explicit ownership contract. }
    class function From(const ASource: INyxSequence<T>;
      const AKey: INyxSelector<T, TKey>; const AEquality: INyxEquality<TKey>;
      const ASeed: INyxSelector<TKey, TAccumulator>;
      const AFolder: INyxSelector<INyxFoldValue<TAccumulator, T>, TAccumulator>):
      INyxSequence<INyxKeyValue<TKey, TAccumulator>>; static;
    class function Seeded(const ASource: INyxSequence<T>;
      const AKey: INyxSelector<T, TKey>; const AEquality: INyxEquality<TKey>;
      const ASeed: TAccumulator;
      const AFolder: INyxSelector<INyxFoldValue<TAccumulator, T>, TAccumulator>):
      INyxSequence<INyxKeyValue<TKey, TAccumulator>>; static;
  end;
  TNyxCountBy<T, TKey> = class
  public
    { Checked Integer per-key counts, including duplicates; empty input has no
      results. No full source groups are stored solely to count their members. }
    class function From(const ASource: INyxSequence<T>;
      const AKey: INyxSelector<T, TKey>; const AEquality: INyxEquality<TKey>):
      INyxSequence<INyxKeyValue<TKey, Integer>>; static;
  end;

  TNyxExtremaBy<T, TKey> = class
  private
    class function TryPick(const ASource: INyxSequence<T>;
      const AKey: INyxSelector<T, TKey>; const AComparer: INyxComparer<TKey>;
      AMaximum: Boolean; out AValue: T): Boolean; static;
  public
    { Immediate streaming extrema. Keys are projected once per row; first ties
      remain selected. Try* clears absence to ordinary initialized T; required
      forms raise ENyxQueryEmpty. Caller comparers specify NaN/locale semantics. }
    class function TryMinBy(const ASource: INyxSequence<T>;
      const AKey: INyxSelector<T, TKey>; const AComparer: INyxComparer<TKey>;
      out AValue: T): Boolean; static;
    class function TryMaxBy(const ASource: INyxSequence<T>;
      const AKey: INyxSelector<T, TKey>; const AComparer: INyxComparer<TKey>;
      out AValue: T): Boolean; static;
    class function MinBy(const ASource: INyxSequence<T>;
      const AKey: INyxSelector<T, TKey>; const AComparer: INyxComparer<TKey>): T; static;
    class function MaxBy(const ASource: INyxSequence<T>;
      const AKey: INyxSelector<T, TKey>; const AComparer: INyxComparer<TKey>): T; static;
  end;
  TNyxExtrema<T> = class
  public
    class function TryMin(const ASource: INyxSequence<T>;
      const AComparer: INyxComparer<T>; out AValue: T): Boolean; static;
    class function TryMax(const ASource: INyxSequence<T>;
      const AComparer: INyxComparer<T>; out AValue: T): Boolean; static;
    class function Min(const ASource: INyxSequence<T>; const AComparer: INyxComparer<T>): T; static;
    class function Max(const ASource: INyxSequence<T>; const AComparer: INyxComparer<T>): T; static;
  end;

  { Compiler-visible generic support. Raw builders belong only to their active
    cursor; completed key/value objects own detached values and no builder link. }
  TQuerySeed<TKey, TAccumulator> = class(TInterfacedObject,
    INyxSelector<TKey, TAccumulator>)
  private
    FValue: TAccumulator;
  public
    constructor Create(const AValue: TAccumulator);
    function Project(const AKey: TKey): TAccumulator;
  end;
  TQueryCountFolder<T> = class(TInterfacedObject,
    INyxSelector<INyxFoldValue<Integer, T>, Integer>)
    function Project(const AValue: INyxFoldValue<Integer, T>): Integer;
  end;
  TQueryAggregateCursor<T, TKey, TAccumulator> =
    class(TEnumerator<INyxKeyValue<TKey, TAccumulator>>)
  private
    FSource: INyxSequence<T>;
    FKey: INyxSelector<T, TKey>;
    FEquality: INyxEquality<TKey>;
    FSeed: INyxSelector<TKey, TAccumulator>;
    FFolder: INyxSelector<INyxFoldValue<TAccumulator, T>, TAccumulator>;
    FValues: array of INyxKeyValue<TKey, TAccumulator>;
    FIndex: Integer;
    FStarted: Boolean;
    procedure Prepare;
    procedure Retire;
  protected
    function Advance(out AValue: INyxKeyValue<TKey, TAccumulator>): Boolean; override;
  public
    constructor Create(const ASource: INyxSequence<T>;
      const AKey: INyxSelector<T, TKey>; const AEquality: INyxEquality<TKey>;
      const ASeed: INyxSelector<TKey, TAccumulator>;
      const AFolder: INyxSelector<INyxFoldValue<TAccumulator, T>, TAccumulator>);
  end;
  TQueryAggregateSequence<T, TKey, TAccumulator> =
    class(TNyxSequence<INyxKeyValue<TKey, TAccumulator>>)
  private
    FSource: INyxSequence<T>;
    FKey: INyxSelector<T, TKey>;
    FEquality: INyxEquality<TKey>;
    FSeed: INyxSelector<TKey, TAccumulator>;
    FFolder: INyxSelector<INyxFoldValue<TAccumulator, T>, TAccumulator>;
  public
    constructor Create(const ASource: INyxSequence<T>;
      const AKey: INyxSelector<T, TKey>; const AEquality: INyxEquality<TKey>;
      const ASeed: INyxSelector<TKey, TAccumulator>;
      const AFolder: INyxSelector<INyxFoldValue<TAccumulator, T>, TAccumulator>);
    function GetEnumerator: INyxEnumerator<INyxKeyValue<TKey, TAccumulator>>; override;
  end;
  TQueryEmptyValue<T> = class
  public
    Value: T;
  end;

{ Shared checked count addition, also independently qualifiable at High(Integer).
  Negative/maximum counts refuse before arithmetic or another row admission. }
function NyxIncrementQueryCount(AValue: Integer): Integer;

implementation

constructor TNyxFoldValue<TAccumulator, T>.Create(const AAccumulator: TAccumulator;
  const AValue: T);
begin
  inherited Create;
  FAccumulator := AAccumulator;
  FValue := AValue;
end;

function TNyxFoldValue<TAccumulator, T>.GetAccumulator: TAccumulator;
begin
  Result := FAccumulator;
end;

function TNyxFoldValue<TAccumulator, T>.GetValue: T;
begin
  Result := FValue;
end;

constructor TNyxKeyValue<TKey, TValue>.Create(const AKey: TKey; const AValue: TValue);
begin
  inherited Create;
  FKey := AKey;
  FValue := AValue;
end;

function TNyxKeyValue<TKey, TValue>.GetKey: TKey;
begin
  Result := FKey;
end;

function TNyxKeyValue<TKey, TValue>.GetValue: TValue;
begin
  Result := FValue;
end;

class function TNyxAggregate<T, TAccumulator>.Fold(const ASource: INyxSequence<T>;
  const ASeed: TAccumulator;
  const AFolder: INyxSelector<INyxFoldValue<TAccumulator, T>, TAccumulator>): TAccumulator;
var
  LCursor: INyxEnumerator<T>;
  LValue: T;
  LContext: INyxFoldValue<TAccumulator, T>;
begin

  if (ASource = nil) or (AFolder = nil) then
  begin
    raise ENyxQuery.Create('Aggregate requires source and owned folder');
  end;
  Result := ASeed;
  LCursor := ASource.GetEnumerator;

  if LCursor = nil then
  begin
    raise ENyxQuery.Create('Aggregate source returned no cursor');
  end;
  while LCursor.MoveNext do
  begin
    LValue := LCursor.Current;
    LContext := TNyxFoldValue<TAccumulator, T>.Create(Result, LValue);
    Result := AFolder.Project(LContext);
  end;
end;

class function TNyxAggregateResult<T, TAccumulator, TResult>.Fold(
  const ASource: INyxSequence<T>; const ASeed: TAccumulator;
  const AFolder: INyxSelector<INyxFoldValue<TAccumulator, T>, TAccumulator>;
  const AResult: INyxSelector<TAccumulator, TResult>): TResult;
var
  LValue: TAccumulator;
begin

  if (ASource = nil) or (AFolder = nil) or (AResult = nil) then
  begin
    raise ENyxQuery.Create('Result aggregate requires source, folder and result selector');
  end;
  LValue := TNyxAggregate<T, TAccumulator>.Fold(ASource, ASeed, AFolder);
  Result := AResult.Project(LValue);
end;

class function TNyxReduce<T>.Fold(const ASource: INyxSequence<T>;
  const AFolder: INyxSelector<INyxFoldValue<T, T>, T>): T;
var
  LCursor: INyxEnumerator<T>;
  LValue: T;
  LContext: INyxFoldValue<T, T>;
begin

  if (ASource = nil) or (AFolder = nil) then
  begin
    raise ENyxQuery.Create('Reduction requires source and owned folder');
  end;
  LCursor := ASource.GetEnumerator;

  if LCursor = nil then
  begin
    raise ENyxQuery.Create('Reduction source returned no cursor');
  end;

  if not LCursor.MoveNext then
  begin
    raise ENyxQueryEmpty.Create('Reduction requires a nonempty source');
  end;
  Result := LCursor.Current;
  while LCursor.MoveNext do
  begin
    LValue := LCursor.Current;
    LContext := TNyxFoldValue<T, T>.Create(Result, LValue);
    Result := AFolder.Project(LContext);
  end;
end;

constructor TQuerySeed<TKey, TAccumulator>.Create(const AValue: TAccumulator);
begin
  inherited Create;
  FValue := AValue;
end;

function TQuerySeed<TKey, TAccumulator>.Project(const AKey: TKey): TAccumulator;
begin
  Result := FValue;
end;

function NyxIncrementQueryCount(AValue: Integer): Integer;
begin

  if (AValue < 0) or (AValue = High(Integer)) then
  begin
    raise ENyxQuery.Create('Query count exceeds the nonnegative Integer domain');
  end;
  Result := AValue + 1;
end;

function TQueryCountFolder<T>.Project(const AValue: INyxFoldValue<Integer, T>): Integer;
begin
  Result := NyxIncrementQueryCount(AValue.Accumulator);
end;

constructor TQueryAggregateCursor<T, TKey, TAccumulator>.Create(
  const ASource: INyxSequence<T>; const AKey: INyxSelector<T, TKey>;
  const AEquality: INyxEquality<TKey>; const ASeed: INyxSelector<TKey, TAccumulator>;
  const AFolder: INyxSelector<INyxFoldValue<TAccumulator, T>, TAccumulator>);
begin
  inherited Create;
  FSource := ASource;
  FKey := AKey;
  FEquality := AEquality;
  FSeed := ASeed;
  FFolder := AFolder;
end;

procedure TQueryAggregateCursor<T, TKey, TAccumulator>.Retire;
begin
  FSource := nil;
  FKey := nil;
  FEquality := nil;
  FSeed := nil;
  FFolder := nil;
end;

procedure TQueryAggregateCursor<T, TKey, TAccumulator>.Prepare;
var
  LIndex: TQueryKeySet<TKey>;
  LKeys: TQueryBuffer<TKey>;
  LAccumulators: TNyxSequenceArray<TAccumulator>;
  LCursor: INyxEnumerator<T>;
  LValue: T;
  LKey: TKey;
  LSeed: TAccumulator;
  LContext: INyxFoldValue<TAccumulator, T>;
  LPosition: Integer;
  LCapacity: Integer;
  LKeyValues: TNyxSequenceArray<TKey>;
begin
  LAccumulators := nil;
  LKeyValues := nil;
  LIndex := TQueryKeySet<TKey>.Create(FEquality);
  LKeys := nil;
  try
    LKeys := TQueryBuffer<TKey>.Create(High(Integer));
    LCursor := FSource.GetEnumerator;

    if LCursor = nil then
    begin
      raise ENyxQuery.Create('Key aggregate source returned no cursor');
    end;
    while LCursor.MoveNext do
    begin
      LValue := LCursor.Current;
      LKey := FKey.Project(LValue);

      if LIndex.FindOrAdd(LKey, LPosition) then
      begin

        if LPosition = Length(LAccumulators) then
        begin
          LCapacity := 16;

          if LPosition > 0 then
          begin

            if LPosition > High(Integer) div 2 then
            begin
              LCapacity := High(Integer);
            end
            else
            begin
              LCapacity := LPosition * 2;
            end;
          end;
          SetLength(LAccumulators, LCapacity);
        end;
        LSeed := FSeed.Project(LKey);
        LAccumulators[LPosition] := LSeed;
        LKeys.Add(LKey);
      end;
      LContext := TNyxFoldValue<TAccumulator, T>.Create(LAccumulators[LPosition], LValue);
      LAccumulators[LPosition] := FFolder.Project(LContext);
    end;
    LKeyValues := LKeys.ToArray;
    SetLength(FValues, LKeys.Count);
    for LPosition := 0 to High(FValues) do
    begin
      FValues[LPosition] := TNyxKeyValue<TKey, TAccumulator>.Create(
        LKeyValues[LPosition], LAccumulators[LPosition]);
    end;
    Retire;
  finally
    LKeys.Free;
    LIndex.Free;
  end;
end;

function TQueryAggregateCursor<T, TKey, TAccumulator>.Advance(
  out AValue: INyxKeyValue<TKey, TAccumulator>): Boolean;
begin
  try

    if not FStarted then
    begin
      FStarted := True;
      Prepare;
    end;
    Result := FIndex < Length(FValues);

    if Result then
    begin
      AValue := FValues[FIndex];
      Inc(FIndex);
    end
    else
    begin
      FValues := nil;
    end;
  except
    FValues := nil;
    Retire;
    raise;
  end;
end;

constructor TQueryAggregateSequence<T, TKey, TAccumulator>.Create(
  const ASource: INyxSequence<T>; const AKey: INyxSelector<T, TKey>;
  const AEquality: INyxEquality<TKey>; const ASeed: INyxSelector<TKey, TAccumulator>;
  const AFolder: INyxSelector<INyxFoldValue<TAccumulator, T>, TAccumulator>);
begin
  inherited Create;

  if (ASource = nil) or (AKey = nil) or (AEquality = nil) or (ASeed = nil) or (AFolder = nil) then
  begin
    raise ENyxQuery.Create('Key aggregate requires source, key/equality, seed and folder');
  end;
  FSource := ASource;
  FKey := AKey;
  FEquality := AEquality;
  FSeed := ASeed;
  FFolder := AFolder;
end;

function TQueryAggregateSequence<T, TKey, TAccumulator>.GetEnumerator:
  INyxEnumerator<INyxKeyValue<TKey, TAccumulator>>;
begin
  Result := TQueryAggregateCursor<T, TKey, TAccumulator>.Create(
    FSource, FKey, FEquality, FSeed, FFolder);
end;

class function TNyxAggregateBy<T, TKey, TAccumulator>.From(
  const ASource: INyxSequence<T>; const AKey: INyxSelector<T, TKey>;
  const AEquality: INyxEquality<TKey>; const ASeed: INyxSelector<TKey, TAccumulator>;
  const AFolder: INyxSelector<INyxFoldValue<TAccumulator, T>, TAccumulator>):
  INyxSequence<INyxKeyValue<TKey, TAccumulator>>;
begin
  Result := TQueryAggregateSequence<T, TKey, TAccumulator>.Create(
    ASource, AKey, AEquality, ASeed, AFolder);
end;

class function TNyxAggregateBy<T, TKey, TAccumulator>.Seeded(
  const ASource: INyxSequence<T>; const AKey: INyxSelector<T, TKey>;
  const AEquality: INyxEquality<TKey>; const ASeed: TAccumulator;
  const AFolder: INyxSelector<INyxFoldValue<TAccumulator, T>, TAccumulator>):
  INyxSequence<INyxKeyValue<TKey, TAccumulator>>;
var
  LSeed: INyxSelector<TKey, TAccumulator>;
begin
  LSeed := TQuerySeed<TKey, TAccumulator>.Create(ASeed);
  Result := From(ASource, AKey, AEquality, LSeed, AFolder);
end;

class function TNyxCountBy<T, TKey>.From(const ASource: INyxSequence<T>;
  const AKey: INyxSelector<T, TKey>; const AEquality: INyxEquality<TKey>):
  INyxSequence<INyxKeyValue<TKey, Integer>>;
var
  LFolder: INyxSelector<INyxFoldValue<Integer, T>, Integer>;
begin
  LFolder := TQueryCountFolder<T>.Create;
  Result := TNyxAggregateBy<T, TKey, Integer>.Seeded(ASource, AKey, AEquality, 0, LFolder);
end;

class function TNyxExtremaBy<T, TKey>.TryPick(const ASource: INyxSequence<T>;
  const AKey: INyxSelector<T, TKey>; const AComparer: INyxComparer<TKey>;
  AMaximum: Boolean; out AValue: T): Boolean;
var
  LCursor: INyxEnumerator<T>;
  LValue: T;
  LKey: TKey;
  LBest: TKey;
  LBestValue: T;
  LSign: Integer;
  LEmpty: TQueryEmptyValue<T>;
begin

  if (ASource = nil) or (AKey = nil) or (AComparer = nil) then
  begin
    raise ENyxQuery.Create('Extrema requires source, key and comparer');
  end;
  LEmpty := TQueryEmptyValue<T>.Create;
  try
    AValue := LEmpty.Value;
  finally
    LEmpty.Free;
  end;
  Result := False;
  LCursor := ASource.GetEnumerator;

  if LCursor = nil then
  begin
    raise ENyxQuery.Create('Extrema source returned no cursor');
  end;
  while LCursor.MoveNext do
  begin
    LValue := LCursor.Current;
    LKey := AKey.Project(LValue);

    if not Result then
    begin
      Result := True;
      LBest := LKey;
      LBestValue := LValue;
    end
    else
    begin
      LSign := AComparer.Compare(LKey, LBest);

      if (AMaximum and (LSign > 0)) or (not AMaximum and (LSign < 0)) then
      begin
        LBest := LKey;
        LBestValue := LValue;
      end;
    end;
  end;

  if Result then
  begin
    AValue := LBestValue;
  end;
end;

class function TNyxExtremaBy<T, TKey>.TryMinBy(const ASource: INyxSequence<T>;
  const AKey: INyxSelector<T, TKey>; const AComparer: INyxComparer<TKey>;
  out AValue: T): Boolean;
begin
  Result := TryPick(ASource, AKey, AComparer, False, AValue);
end;

class function TNyxExtremaBy<T, TKey>.TryMaxBy(const ASource: INyxSequence<T>;
  const AKey: INyxSelector<T, TKey>; const AComparer: INyxComparer<TKey>;
  out AValue: T): Boolean;
begin
  Result := TryPick(ASource, AKey, AComparer, True, AValue);
end;

class function TNyxExtremaBy<T, TKey>.MinBy(const ASource: INyxSequence<T>;
  const AKey: INyxSelector<T, TKey>; const AComparer: INyxComparer<TKey>): T;
begin

  if not TryMinBy(ASource, AKey, AComparer, Result) then
  begin
    raise ENyxQueryEmpty.Create('MinBy requires a nonempty source');
  end;
end;

class function TNyxExtremaBy<T, TKey>.MaxBy(const ASource: INyxSequence<T>;
  const AKey: INyxSelector<T, TKey>; const AComparer: INyxComparer<TKey>): T;
begin

  if not TryMaxBy(ASource, AKey, AComparer, Result) then
  begin
    raise ENyxQueryEmpty.Create('MaxBy requires a nonempty source');
  end;
end;

class function TNyxExtrema<T>.TryMin(const ASource: INyxSequence<T>;
  const AComparer: INyxComparer<T>; out AValue: T): Boolean;
var
  LKey: INyxSelector<T, T>;
begin
  LKey := TQuerySetIdentity<T>.Create;
  Result := TNyxExtremaBy<T, T>.TryMinBy(ASource, LKey, AComparer, AValue);
end;

class function TNyxExtrema<T>.TryMax(const ASource: INyxSequence<T>;
  const AComparer: INyxComparer<T>; out AValue: T): Boolean;
var
  LKey: INyxSelector<T, T>;
begin
  LKey := TQuerySetIdentity<T>.Create;
  Result := TNyxExtremaBy<T, T>.TryMaxBy(ASource, LKey, AComparer, AValue);
end;

class function TNyxExtrema<T>.Min(const ASource: INyxSequence<T>;
  const AComparer: INyxComparer<T>): T;
begin

  if not TryMin(ASource, AComparer, Result) then
  begin
    raise ENyxQueryEmpty.Create('Min requires a nonempty source');
  end;
end;

class function TNyxExtrema<T>.Max(const ASource: INyxSequence<T>;
  const AComparer: INyxComparer<T>): T;
begin

  if not TryMax(ASource, AComparer, Result) then
  begin
    raise ENyxQueryEmpty.Create('Max requires a nonempty source');
  end;
end;

end.
