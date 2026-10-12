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

unit nyx.linq.materialization;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.linq, nyx.linq.aggregates;

type
  ENyxQueryDuplicateKey = class(ENyxQuery);
  TNyxDuplicatePolicy = (ndpRefuse, ndpKeepFirst, ndpReplaceValue);

  { Owned immutable materialized list, matching Nyx snapshot ownership. Reads
    preserve encounter order and exact values; ItemAt refuses bounds. AsSequence
    and ToArray capture detached outer vectors, without a back link to the list.
    Mutable nested payloads retain ordinary Pascal sharing/ownership semantics. }
  INyxList<T> = interface(IInterface)
    function GetCount: Integer;
    function ItemAt(AIndex: Integer): T;
    function AsSequence: INyxSequence<T>;
    function ToArray: TNyxSequenceArray<T>;
    property Count: Integer read GetCount;
  end;
  { Immutable indexed dictionary. First-key encounter order is retained even when
    ReplaceValue selects the last equivalent value. TryGetValue clears absence;
    GetValue raises ENyxQueryEmpty. Entry sequences/keys/values independently own
    data after dictionary disposal. Equality/keys must remain stable while indexed. }
  INyxDictionary<TKey, TValue> = interface(IInterface)
    function GetCount: Integer;
    function ContainsKey(const AKey: TKey): Boolean;
    function TryGetValue(const AKey: TKey; out AValue: TValue): Boolean;
    function GetValue(const AKey: TKey): TValue;
    function EntryAt(AIndex: Integer): INyxKeyValue<TKey, TValue>;
    function Entries: INyxSequence<INyxKeyValue<TKey, TValue>>;
    function Keys: INyxSequence<TKey>;
    function Values: INyxSequence<TValue>;
    property Count: Integer read GetCount;
  end;
  INyxHashSet<T> = interface(IInterface)
    function GetCount: Integer;
    function Contains(const AValue: T): Boolean;
    function AsSequence: INyxSequence<T>;
    property Count: Integer read GetCount;
  end;
  TNyxMaterialize<T> = class
  public
    { Immediate complete admission; source failure returns no partial container.
      No mutation authority over a live collection/UI is implied. }
    class function ToList(const ASource: INyxSequence<T>): INyxList<T>; static;
    class function ToHashSet(const ASource: INyxSequence<T>;
      const AEquality: INyxEquality<T>): INyxHashSet<T>; static;
  end;
  TNyxDictionary<TSource, TKey, TValue> = class
  public
    { Key/hash runs once per consumed row. Value runs for new/replaced entries;
      KeepFirst skips duplicate value projection, Refuse rejects before it.
      Policies are explicit, validated before source/callback entry. }
    class function From(const ASource: INyxSequence<TSource>;
      const AKey: INyxSelector<TSource, TKey>;
      const AValue: INyxSelector<TSource, TValue>;
      const AEquality: INyxEquality<TKey>;
      ADuplicates: TNyxDuplicatePolicy = ndpRefuse): INyxDictionary<TKey, TValue>; static;
  end;

  { Compiler-visible support. Snapshot constructors copy arrays before accepting
    raw index ownership; a constructor failure leaves that index with its caller. }
  TQueryListSnapshot<T> = class(TInterfacedObject, INyxList<T>)
  private
    FValues: TNyxSequenceArray<T>;
  public
    constructor Create(const AValues: TNyxSequenceArray<T>);
    function GetCount: Integer;
    function ItemAt(AIndex: Integer): T;
    function AsSequence: INyxSequence<T>;
    function ToArray: TNyxSequenceArray<T>;
  end;
  TQueryDictionarySnapshot<TKey, TValue> = class(TInterfacedObject,
    INyxDictionary<TKey, TValue>)
  private
    FKeys: TNyxSequenceArray<TKey>;
    FValues: TNyxSequenceArray<TValue>;
    FIndex: TQueryKeySet<TKey>;
    FEmpty: TValue;
  public
    constructor Create(const AKeys: TNyxSequenceArray<TKey>;
      const AValues: TNyxSequenceArray<TValue>; AIndex: TQueryKeySet<TKey>);
    destructor Destroy; override;
    function GetCount: Integer;
    function ContainsKey(const AKey: TKey): Boolean;
    function TryGetValue(const AKey: TKey; out AValue: TValue): Boolean;
    function GetValue(const AKey: TKey): TValue;
    function EntryAt(AIndex: Integer): INyxKeyValue<TKey, TValue>;
    function Entries: INyxSequence<INyxKeyValue<TKey, TValue>>;
    function Keys: INyxSequence<TKey>;
    function Values: INyxSequence<TValue>;
  end;
  TQuerySetSnapshot<T> = class(TInterfacedObject, INyxHashSet<T>)
  private
    FValues: INyxDictionary<T, Boolean>;
  public
    constructor Create(const AValues: INyxDictionary<T, Boolean>);
    function GetCount: Integer;
    function Contains(const AValue: T): Boolean;
    function AsSequence: INyxSequence<T>;
  end;
  TQueryTrueSelector<T> = class(TInterfacedObject, INyxSelector<T, Boolean>)
    function Project(const AValue: T): Boolean;
  end;

implementation

constructor TQueryListSnapshot<T>.Create(const AValues: TNyxSequenceArray<T>);
var
  LIndex: Integer;
begin
  inherited Create;
  SetLength(FValues, Length(AValues));
  for LIndex := 0 to High(FValues) do
  begin
    FValues[LIndex] := AValues[LIndex];
  end;
end;

function TQueryListSnapshot<T>.GetCount: Integer;
begin
  Result := Length(FValues);
end;

function TQueryListSnapshot<T>.ItemAt(AIndex: Integer): T;
begin

  if (AIndex < 0) or (AIndex >= Length(FValues)) then
  begin
    raise ENyxQueryEmpty.Create('Materialized list index is absent');
  end;
  Result := FValues[AIndex];
end;

function TQueryListSnapshot<T>.AsSequence: INyxSequence<T>;
begin
  Result := TNyxSequence<T>.From(FValues);
end;

function TQueryListSnapshot<T>.ToArray: TNyxSequenceArray<T>;
var
  LIndex: Integer;
begin
  Result := nil;
  SetLength(Result, Length(FValues));
  for LIndex := 0 to High(Result) do
  begin
    Result[LIndex] := FValues[LIndex];
  end;
end;

class function TNyxMaterialize<T>.ToList(const ASource: INyxSequence<T>): INyxList<T>;
var
  LCursor: INyxEnumerator<T>;
  LBuffer: TQueryBuffer<T>;
  LValue: T;
  LValues: TNyxSequenceArray<T>;
begin

  if ASource = nil then
  begin
    raise ENyxQuery.Create('List materialization requires a source');
  end;
  LBuffer := TQueryBuffer<T>.Create(High(Integer));
  try
    LCursor := ASource.GetEnumerator;

    if LCursor = nil then
    begin
      raise ENyxQuery.Create('List materialization source returned no cursor');
    end;
    while LCursor.MoveNext do
    begin
      LValue := LCursor.Current;
      LBuffer.Add(LValue);
    end;
    LValues := LBuffer.ToArray;
    Result := TQueryListSnapshot<T>.Create(LValues);
  finally
    LBuffer.Free;
  end;
end;

constructor TQueryDictionarySnapshot<TKey, TValue>.Create(
  const AKeys: TNyxSequenceArray<TKey>; const AValues: TNyxSequenceArray<TValue>;
  AIndex: TQueryKeySet<TKey>);
var
  LPosition: Integer;
begin
  inherited Create;
  SetLength(FKeys, Length(AKeys));
  SetLength(FValues, Length(AValues));
  for LPosition := 0 to High(FKeys) do
  begin
    FKeys[LPosition] := AKeys[LPosition];
    FValues[LPosition] := AValues[LPosition];
  end;
  { Accept exclusive index ownership only after every potentially failing copy. }
  FIndex := AIndex;
end;

destructor TQueryDictionarySnapshot<TKey, TValue>.Destroy;
begin
  FIndex.Free;
  inherited Destroy;
end;

function TQueryDictionarySnapshot<TKey, TValue>.GetCount: Integer;
begin
  Result := Length(FKeys);
end;

function TQueryDictionarySnapshot<TKey, TValue>.ContainsKey(const AKey: TKey): Boolean;
var
  LIndex: Integer;
begin
  Result := FIndex.TryIndexOf(AKey, LIndex);
end;

function TQueryDictionarySnapshot<TKey, TValue>.TryGetValue(const AKey: TKey;
  out AValue: TValue): Boolean;
var
  LIndex: Integer;
begin
  AValue := FEmpty;
  Result := FIndex.TryIndexOf(AKey, LIndex);

  if Result then
  begin
    AValue := FValues[LIndex];
  end;
end;

function TQueryDictionarySnapshot<TKey, TValue>.GetValue(const AKey: TKey): TValue;
begin

  if not TryGetValue(AKey, Result) then
  begin
    raise ENyxQueryEmpty.Create('Dictionary key is absent');
  end;
end;

function TQueryDictionarySnapshot<TKey, TValue>.EntryAt(AIndex: Integer):
  INyxKeyValue<TKey, TValue>;
begin

  if (AIndex < 0) or (AIndex >= Length(FKeys)) then
  begin
    raise ENyxQueryEmpty.Create('Dictionary entry index is absent');
  end;
  Result := TNyxKeyValue<TKey, TValue>.Create(FKeys[AIndex], FValues[AIndex]);
end;

function TQueryDictionarySnapshot<TKey, TValue>.Entries:
  INyxSequence<INyxKeyValue<TKey, TValue>>;
type
  TEntry = INyxKeyValue<TKey, TValue>;
var
  LEntries: TNyxSequenceArray<TEntry>;
  LIndex: Integer;
begin
  LEntries := nil;
  SetLength(LEntries, Length(FKeys));
  for LIndex := 0 to High(LEntries) do
  begin
    LEntries[LIndex] := EntryAt(LIndex);
  end;
  Result := TNyxSequence<TEntry>.From(LEntries);
end;

function TQueryDictionarySnapshot<TKey, TValue>.Keys: INyxSequence<TKey>;
begin
  Result := TNyxSequence<TKey>.From(FKeys);
end;

function TQueryDictionarySnapshot<TKey, TValue>.Values: INyxSequence<TValue>;
begin
  Result := TNyxSequence<TValue>.From(FValues);
end;

class function TNyxDictionary<TSource, TKey, TValue>.From(
  const ASource: INyxSequence<TSource>; const AKey: INyxSelector<TSource, TKey>;
  const AValue: INyxSelector<TSource, TValue>; const AEquality: INyxEquality<TKey>;
  ADuplicates: TNyxDuplicatePolicy): INyxDictionary<TKey, TValue>;
var
  LCursor: INyxEnumerator<TSource>;
  LIndex: TQueryKeySet<TKey>;
  LKeys: TQueryBuffer<TKey>;
  LValues: TNyxSequenceArray<TValue>;
  LKeyValues: TNyxSequenceArray<TKey>;
  LSourceValue: TSource;
  LKey: TKey;
  LValue: TValue;
  LPosition: Integer;
  LCapacity: Integer;
  LNew: Boolean;
begin

  if (ASource = nil) or (AKey = nil) or (AValue = nil) or (AEquality = nil) then
  begin
    raise ENyxQuery.Create('Dictionary requires source, key/value selectors and equality');
  end;

  if (Ord(ADuplicates) < Ord(Low(TNyxDuplicatePolicy))) or
    (Ord(ADuplicates) > Ord(High(TNyxDuplicatePolicy))) then
  begin
    raise ENyxQuery.Create('Dictionary requires a valid duplicate policy');
  end;
  LValues := nil;
  LKeyValues := nil;
  LIndex := TQueryKeySet<TKey>.Create(AEquality);
  LKeys := nil;
  try
    LKeys := TQueryBuffer<TKey>.Create(High(Integer));
    LCursor := ASource.GetEnumerator;

    if LCursor = nil then
    begin
      raise ENyxQuery.Create('Dictionary source returned no cursor');
    end;
    while LCursor.MoveNext do
    begin
      LSourceValue := LCursor.Current;
      LKey := AKey.Project(LSourceValue);
      LNew := LIndex.FindOrAdd(LKey, LPosition);

      if not LNew then
      begin
        case ADuplicates of
          ndpRefuse:
          begin
            raise ENyxQueryDuplicateKey.Create('Dictionary has an equivalent duplicate key');
          end;
          ndpKeepFirst:
          begin
            Continue;
          end;
          ndpReplaceValue:
          begin
            { Project the replacement while keeping the first equivalent key. }
          end;
        end;
      end;
      LValue := AValue.Project(LSourceValue);

      if LNew then
      begin

        if LPosition = Length(LValues) then
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
          SetLength(LValues, LCapacity);
        end;
        LKeys.Add(LKey);
      end;
      LValues[LPosition] := LValue;
    end;
    LKeyValues := LKeys.ToArray;
    SetLength(LValues, LKeys.Count);
    Result := TQueryDictionarySnapshot<TKey, TValue>.Create(LKeyValues, LValues, LIndex);
    LIndex := nil;
  finally
    LKeys.Free;
    LIndex.Free;
  end;
end;

constructor TQuerySetSnapshot<T>.Create(const AValues: INyxDictionary<T, Boolean>);
begin
  inherited Create;
  FValues := AValues;
end;

function TQuerySetSnapshot<T>.GetCount: Integer;
begin
  Result := FValues.Count;
end;

function TQuerySetSnapshot<T>.Contains(const AValue: T): Boolean;
begin
  Result := FValues.ContainsKey(AValue);
end;

function TQuerySetSnapshot<T>.AsSequence: INyxSequence<T>;
begin
  Result := FValues.Keys;
end;

function TQueryTrueSelector<T>.Project(const AValue: T): Boolean;
begin
  Result := True;
end;

class function TNyxMaterialize<T>.ToHashSet(const ASource: INyxSequence<T>;
  const AEquality: INyxEquality<T>): INyxHashSet<T>;
var
  LIdentity: INyxSelector<T, T>;
  LTrue: INyxSelector<T, Boolean>;
  LDictionary: INyxDictionary<T, Boolean>;
begin
  LIdentity := TQuerySetIdentity<T>.Create;
  LTrue := TQueryTrueSelector<T>.Create;
  LDictionary := TNyxDictionary<T, T, Boolean>.From(ASource, LIdentity, LTrue,
    AEquality, ndpKeepFirst);
  Result := TQuerySetSnapshot<T>.Create(LDictionary);
end;

end.
