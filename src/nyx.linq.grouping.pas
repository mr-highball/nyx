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
unit nyx.linq.grouping;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.linq;

type
  { Equality's portable interface lives in nyx.linq so ordinary fluent sequence
    operations and grouping use the same Pascal type without unit cycles. }
  TNyxEquality<T> = class
  public
    { Equality is Compare=0; the caller's typed hash selector follows that same
      relation. Existing scoped/inline comparer and selector lifetimes apply. }
    class function From(const AComparer: INyxComparer<T>;
      const AHash: INyxSelector<T, Cardinal>): INyxEquality<T>; static;
  end;

  { Immutable, independently retained group. Key is the first encountered equal
    key; Values preserve encounter order and may outlive source/lookup/cursors.
    Managed payloads retain ordinary ownership, without source/UI back references. }
  INyxGroup<TKey, TElement> = interface(IInterface)
    function GetKey: TKey;
    function GetValues: INyxSequence<TElement>;
    function GetCount: Integer;
    property Key: TKey read GetKey;
    property Values: INyxSequence<TElement> read GetValues;
    property Count: Integer read GetCount;
  end;
  { Immutable indexed multi-map. GroupAt/Groups preserve first-key encounter
    order. Absent GroupFor returns nil; ValuesFor returns an empty sequence.
    Bounds refuse; no mutation or shared mutable enumeration cursor is exposed. }
  INyxLookup<TKey, TElement> = interface(IInterface)
    function GetCount: Integer;
    function GetItemCount: Integer;
    function Has(const AKey: TKey): Boolean;
    function GroupFor(const AKey: TKey): INyxGroup<TKey, TElement>;
    function ValuesFor(const AKey: TKey): INyxSequence<TElement>;
    function GroupAt(AIndex: Integer): INyxGroup<TKey, TElement>;
    function Groups: INyxSequence<INyxGroup<TKey, TElement>>;
    property Count: Integer read GetCount;
    property ItemCount: Integer read GetItemCount;
  end;
  TNyxLookup<TSource, TKey, TElement> = class
  public
    { Immediate terminal admission: each row projects its key and element once.
      Callback/source failure returns no partial lookup. Duplicate keys group;
      Integer row/group/slot limits refuse before arithmetic overflow. }
    class function From(const ASource: INyxSequence<TSource>;
      const AKey: INyxSelector<TSource, TKey>;
      const AElement: INyxSelector<TSource, TElement>;
      const AEquality: INyxEquality<TKey>): INyxLookup<TKey, TElement>; static;
  end;
  TNyxGroupBy<TSource, TKey, TElement> = class
  public
    { Deferred grouping materializes completely on first movement, never at
      construction/GetEnumerator. Every cursor builds independently; source
      failures cannot expose a successful partial group sequence. }
    class function From(const ASource: INyxSequence<TSource>;
      const AKey: INyxSelector<TSource, TKey>;
      const AElement: INyxSelector<TSource, TElement>;
      const AEquality: INyxEquality<TKey>): INyxSequence<INyxGroup<TKey, TElement>>; static;
  end;

function NewNyxIntegerEquality: INyxEquality<Integer>;
function NewNyxBooleanEquality: INyxEquality<Boolean>;
{ Exact Unicode scalar hashing/equality; optional ASCII-insensitive folding is
  explicit. Embedded NUL is significant and malformed text refuses on Hash too. }
function NewNyxTextEquality(AMode: TNyxTextOrder = ntoOrdinal): INyxEquality<TNyxText>;

type
  { Compiler-visible support types. Builders own their raw mutable buckets until
    admission publishes immutable groups. Immutable groups never retain builders. }
  TNyxEqualityAdapter<T> = class(TInterfacedObject, INyxEquality<T>)
  private
    FComparer: INyxComparer<T>;
    FHash: INyxSelector<T, Cardinal>;
  public
    constructor Create(const AComparer: INyxComparer<T>; const AHash: INyxSelector<T, Cardinal>);
    function Equal(const ALeft, ARight: T): Boolean;
    function Hash(const AValue: T): Cardinal;
  end;
  TNyxGroup<TKey, TElement> = class(TInterfacedObject, INyxGroup<TKey, TElement>)
  private
    FKey: TKey;
    FValues: INyxSequence<TElement>;
    FCount: Integer;
  public
    constructor Create(const AKey: TKey; const AValues: array of TElement);
    function GetKey: TKey;
    function GetValues: INyxSequence<TElement>;
    function GetCount: Integer;
  end;
  TNyxGroupBucket<TKey, TElement> = class
  public
    Key: TKey;
    Hash: Cardinal;
    Values: TNyxSequenceArray<TElement>;
    Count: Integer;
    procedure Append(const AValue: TElement);
  end;
  TNyxLookupSnapshot<TKey, TElement> = class(TInterfacedObject, INyxLookup<TKey, TElement>)
  private
    FGroups: array of INyxGroup<TKey, TElement>;
    FHashes: array of Cardinal;
    FSlots: array of Integer;
    FEquality: INyxEquality<TKey>;
    FItems: Integer;
    function Find(const AKey: TKey; AHash: Cardinal): Integer;
  public
    constructor Create(const AGroups: array of INyxGroup<TKey, TElement>;
      const AHashes: array of Cardinal; const ASlots: array of Integer;
      const AEquality: INyxEquality<TKey>; AItemCount: Integer);
    function GetCount: Integer;
    function GetItemCount: Integer;
    function Has(const AKey: TKey): Boolean;
    function GroupFor(const AKey: TKey): INyxGroup<TKey, TElement>;
    function ValuesFor(const AKey: TKey): INyxSequence<TElement>;
    function GroupAt(AIndex: Integer): INyxGroup<TKey, TElement>;
    function Groups: INyxSequence<INyxGroup<TKey, TElement>>;
  end;
  TNyxLookupBuilder<TKey, TElement> = class
  private
    FBuckets: array of TNyxGroupBucket<TKey, TElement>;
    FSlots: array of Integer;
    FEquality: INyxEquality<TKey>;
    FCount: Integer;
    FItems: Integer;
    procedure GrowSlots;
    function FindSlot(const AKey: TKey; AHash: Cardinal): Integer;
  public
    constructor Create(const AEquality: INyxEquality<TKey>);
    destructor Destroy; override;
    procedure Add(const AKey: TKey; const AValue: TElement);
    function Finish: INyxLookup<TKey, TElement>;
  end;
  TNyxGroupEnumerator<TSource, TKey, TElement> = class(TEnumerator<INyxGroup<TKey, TElement>>)
  private
    FSource: INyxSequence<TSource>;
    FKey: INyxSelector<TSource, TKey>;
    FElement: INyxSelector<TSource, TElement>;
    FEquality: INyxEquality<TKey>;
    FLookup: INyxLookup<TKey, TElement>;
    FIndex: Integer;
  protected
    function Advance(out AValue: INyxGroup<TKey, TElement>): Boolean; override;
  public
    constructor Create(const ASource: INyxSequence<TSource>;
      const AKey: INyxSelector<TSource, TKey>; const AElement: INyxSelector<TSource, TElement>;
      const AEquality: INyxEquality<TKey>);
  end;
  TNyxGroupSequence<TSource, TKey, TElement> = class(TNyxSequence<INyxGroup<TKey, TElement>>)
  private
    FSource: INyxSequence<TSource>;
    FKey: INyxSelector<TSource, TKey>;
    FElement: INyxSelector<TSource, TElement>;
    FEquality: INyxEquality<TKey>;
  public
    constructor Create(const ASource: INyxSequence<TSource>;
      const AKey: INyxSelector<TSource, TKey>; const AElement: INyxSelector<TSource, TElement>;
      const AEquality: INyxEquality<TKey>);
    function GetEnumerator: INyxEnumerator<INyxGroup<TKey, TElement>>; override;
  end;

implementation

type
  TIntegerEquality = class(TInterfacedObject, INyxEquality<Integer>)
    function Equal(const ALeft, ARight: Integer): Boolean;
    function Hash(const AValue: Integer): Cardinal;
  end;
  TBooleanEquality = class(TInterfacedObject, INyxEquality<Boolean>)
    function Equal(const ALeft, ARight: Boolean): Boolean;
    function Hash(const AValue: Boolean): Cardinal;
  end;
  TTextEquality = class(TInterfacedObject, INyxEquality<TNyxText>)
  private
    FMode: TNyxTextOrder;
    FComparer: INyxComparer<TNyxText>;
  public
    constructor Create(AMode: TNyxTextOrder);
    function Equal(const ALeft, ARight: TNyxText): Boolean;
    function Hash(const AValue: TNyxText): Cardinal;
  end;

function TIntegerEquality.Equal(const ALeft, ARight: Integer): Boolean;
begin
  Result := ALeft = ARight;
end;

function TIntegerEquality.Hash(const AValue: Integer): Cardinal;
begin

  if AValue < 0 then
  begin
    Result := Cardinal(AValue - Low(Integer));
  end
  else
  begin
    Result := Cardinal(AValue) + Cardinal($80000000);
  end;
end;

function TBooleanEquality.Equal(const ALeft, ARight: Boolean): Boolean;
begin
  Result := ALeft = ARight;
end;

function TBooleanEquality.Hash(const AValue: Boolean): Cardinal;
begin
  Result := Ord(AValue);
end;

constructor TTextEquality.Create(AMode: TNyxTextOrder);
begin
  inherited Create;
  FMode := AMode;
  FComparer := NewNyxTextComparer(AMode);
end;

function TTextEquality.Equal(const ALeft, ARight: TNyxText): Boolean;
begin
  Result := FComparer.Compare(ALeft, ARight) = 0;
end;

function TTextEquality.Hash(const AValue: TNyxText): Cardinal;
var
  LIndex: Integer;
  LScalar: Integer;
  {$ifdef PAS2JS}
  LHash: NativeInt;
  {$else}
  LHash: Int64;
  {$endif}
begin
  LIndex := 1;
  LHash := 0;
  while LIndex <= Length(AValue) do
  begin

    if not NyxNextScalar(AValue, LIndex, LScalar) then
    begin
      raise ENyxQuery.Create('Text equality requires valid Unicode scalars');
    end;

    if (FMode = ntoAsciiInsensitive) and (LScalar >= 65) and (LScalar <= 90) then
    begin
      Inc(LScalar, 32);
    end;
    { The product stays below 2^36, exact in checked Int64 and browser NativeInt.
      A prime modulus gives portable arithmetic without disabled overflow checks. }
    LHash := (LHash * 31 + LScalar) mod 2147483647;
  end;
  Result := Cardinal(LHash);
end;

function NewNyxIntegerEquality: INyxEquality<Integer>;
begin
  Result := TIntegerEquality.Create;
end;

function NewNyxBooleanEquality: INyxEquality<Boolean>;
begin
  Result := TBooleanEquality.Create;
end;

function NewNyxTextEquality(AMode: TNyxTextOrder): INyxEquality<TNyxText>;
begin
  Result := TTextEquality.Create(AMode);
end;

constructor TNyxEqualityAdapter<T>.Create(const AComparer: INyxComparer<T>;
  const AHash: INyxSelector<T, Cardinal>);
begin
  inherited Create;
  FComparer := AComparer;
  FHash := AHash;
end;

function TNyxEqualityAdapter<T>.Equal(const ALeft, ARight: T): Boolean;
begin
  Result := FComparer.Compare(ALeft, ARight) = 0;
end;

function TNyxEqualityAdapter<T>.Hash(const AValue: T): Cardinal;
begin
  Result := FHash.Project(AValue);
end;

class function TNyxEquality<T>.From(const AComparer: INyxComparer<T>;
  const AHash: INyxSelector<T, Cardinal>): INyxEquality<T>;
begin

  if (AComparer = nil) or (AHash = nil) then
  begin
    raise ENyxQuery.Create('Equality requires owned comparer and hash callbacks');
  end;
  Result := TNyxEqualityAdapter<T>.Create(AComparer, AHash);
end;

constructor TNyxGroup<TKey, TElement>.Create(const AKey: TKey; const AValues: array of TElement);
begin
  inherited Create;
  FKey := AKey;
  FValues := TNyxSequence<TElement>.From(AValues);
  FCount := Length(AValues);
end;

function TNyxGroup<TKey, TElement>.GetKey: TKey;
begin
  Result := FKey;
end;

function TNyxGroup<TKey, TElement>.GetValues: INyxSequence<TElement>;
begin
  Result := FValues;
end;

function TNyxGroup<TKey, TElement>.GetCount: Integer;
begin
  Result := FCount;
end;

procedure TNyxGroupBucket<TKey, TElement>.Append(const AValue: TElement);
var
  LCapacity: Integer;
begin

  if Count = High(Integer) then
  begin
    raise ENyxQuery.Create('Group item count exceeds Integer range');
  end;

  if Count = Length(Values) then
  begin
    LCapacity := Length(Values);

    if LCapacity = 0 then
    begin
      LCapacity := 4;
    end
    else
    begin

      if LCapacity > High(Integer) div 2 then
      begin
        raise ENyxQuery.Create('Group capacity exceeds Integer range');
      end;
      LCapacity := LCapacity * 2;
    end;
    SetLength(Values, LCapacity);
  end;
  Values[Count] := AValue;
  Inc(Count);
end;

constructor TNyxLookupBuilder<TKey, TElement>.Create(const AEquality: INyxEquality<TKey>);
begin
  inherited Create;
  FEquality := AEquality;
  GrowSlots;
end;

destructor TNyxLookupBuilder<TKey, TElement>.Destroy;
var
  LIndex: Integer;
begin
  for LIndex := 0 to FCount - 1 do
  begin
    FBuckets[LIndex].Free;
  end;
  inherited Destroy;
end;

procedure TNyxLookupBuilder<TKey, TElement>.GrowSlots;
var
  LSize: Integer;
  LIndex: Integer;
  LSlot: Integer;
begin
  LSize := Length(FSlots);

  if LSize = 0 then
  begin
    LSize := 16;
  end
  else
  begin

    if LSize > High(Integer) div 2 then
    begin
      raise ENyxQuery.Create('Lookup slot capacity exceeds Integer range');
    end;
    LSize := LSize * 2;
  end;
  SetLength(FSlots, LSize);
  for LIndex := 0 to High(FSlots) do
  begin
    FSlots[LIndex] := -1;
  end;
  for LIndex := 0 to FCount - 1 do
  begin
    LSlot := Integer(FBuckets[LIndex].Hash mod Cardinal(LSize));
    while FSlots[LSlot] >= 0 do
    begin
      LSlot := (LSlot + 1) mod LSize;
    end;
    FSlots[LSlot] := LIndex;
  end;
end;

function TNyxLookupBuilder<TKey, TElement>.FindSlot(const AKey: TKey; AHash: Cardinal): Integer;
var
  LGroup: Integer;
begin
  Result := Integer(AHash mod Cardinal(Length(FSlots)));
  while FSlots[Result] >= 0 do
  begin
    LGroup := FSlots[Result];

    if (FBuckets[LGroup].Hash = AHash) and FEquality.Equal(FBuckets[LGroup].Key, AKey) then
    begin
      Exit;
    end;
    Result := (Result + 1) mod Length(FSlots);
  end;
end;

procedure TNyxLookupBuilder<TKey, TElement>.Add(const AKey: TKey; const AValue: TElement);
var
  LHash: Cardinal;
  LSlot: Integer;
  LCapacity: Integer;
  LBucket: TNyxGroupBucket<TKey, TElement>;
begin

  if FItems = High(Integer) then
  begin
    raise ENyxQuery.Create('Lookup item count exceeds Integer range');
  end;
  LHash := FEquality.Hash(AKey);
  LSlot := FindSlot(AKey, LHash);

  if FSlots[LSlot] < 0 then
  begin

    if FCount >= Length(FSlots) div 2 then
    begin
      GrowSlots;
      LSlot := FindSlot(AKey, LHash);
    end;

    if FCount = Length(FBuckets) then
    begin
      LCapacity := Length(FBuckets);

      if LCapacity = 0 then
      begin
        LCapacity := 8;
      end
      else
      begin

        if LCapacity > High(Integer) div 2 then
        begin
          raise ENyxQuery.Create('Lookup group capacity exceeds Integer range');
        end;
        LCapacity := LCapacity * 2;
      end;
      SetLength(FBuckets, LCapacity);
    end;
    LBucket := TNyxGroupBucket<TKey, TElement>.Create;
    FBuckets[FCount] := LBucket;
    { Own the bucket before generic key assignment: a caller's managed record
      assignment can raise, and builder cleanup must still retire this bucket. }
    Inc(FCount);
    LBucket.Key := AKey;
    LBucket.Hash := LHash;
    FSlots[LSlot] := FCount - 1;
  end;
  FBuckets[FSlots[LSlot]].Append(AValue);
  Inc(FItems);
end;

function TNyxLookupBuilder<TKey, TElement>.Finish: INyxLookup<TKey, TElement>;
var
  LGroups: array of INyxGroup<TKey, TElement>;
  LHashes: array of Cardinal;
  LIndex: Integer;
begin
  SetLength(LGroups, FCount);
  SetLength(LHashes, FCount);
  for LIndex := 0 to FCount - 1 do
  begin
    SetLength(FBuckets[LIndex].Values, FBuckets[LIndex].Count);
    LGroups[LIndex] := TNyxGroup<TKey, TElement>.Create(FBuckets[LIndex].Key, FBuckets[LIndex].Values);
    LHashes[LIndex] := FBuckets[LIndex].Hash;
  end;
  Result := TNyxLookupSnapshot<TKey, TElement>.Create(LGroups, LHashes, FSlots, FEquality, FItems);
end;

constructor TNyxLookupSnapshot<TKey, TElement>.Create(const AGroups: array of INyxGroup<TKey, TElement>;
  const AHashes: array of Cardinal; const ASlots: array of Integer;
  const AEquality: INyxEquality<TKey>; AItemCount: Integer);
var
  LIndex: Integer;
begin
  inherited Create;
  FEquality := AEquality;
  FItems := AItemCount;
  SetLength(FGroups, Length(AGroups));
  SetLength(FHashes, Length(AHashes));
  SetLength(FSlots, Length(ASlots));
  for LIndex := 0 to High(FGroups) do
  begin
    FGroups[LIndex] := AGroups[LIndex];
    FHashes[LIndex] := AHashes[LIndex];
  end;
  for LIndex := 0 to High(FSlots) do
  begin
    FSlots[LIndex] := ASlots[LIndex];
  end;
end;

function TNyxLookupSnapshot<TKey, TElement>.Find(const AKey: TKey; AHash: Cardinal): Integer;
var
  LSlot: Integer;
  LKey: TKey;
begin
  LSlot := Integer(AHash mod Cardinal(Length(FSlots)));
  while FSlots[LSlot] >= 0 do
  begin
    Result := FSlots[LSlot];
    LKey := FGroups[Result].Key;

    if (FHashes[Result] = AHash) and FEquality.Equal(LKey, AKey) then
    begin
      Exit;
    end;
    LSlot := (LSlot + 1) mod Length(FSlots);
  end;
  Result := -1;
end;

function TNyxLookupSnapshot<TKey, TElement>.GetCount: Integer;
begin
  Result := Length(FGroups);
end;

function TNyxLookupSnapshot<TKey, TElement>.GetItemCount: Integer;
begin
  Result := FItems;
end;

function TNyxLookupSnapshot<TKey, TElement>.Has(const AKey: TKey): Boolean;
begin
  Result := Find(AKey, FEquality.Hash(AKey)) >= 0;
end;

function TNyxLookupSnapshot<TKey, TElement>.GroupFor(const AKey: TKey): INyxGroup<TKey, TElement>;
var
  LIndex: Integer;
begin
  LIndex := Find(AKey, FEquality.Hash(AKey));
  Result := nil;

  if LIndex >= 0 then
  begin
    Result := FGroups[LIndex];
  end;
end;

function TNyxLookupSnapshot<TKey, TElement>.ValuesFor(const AKey: TKey): INyxSequence<TElement>;
var
  LGroup: INyxGroup<TKey, TElement>;
begin
  LGroup := GroupFor(AKey);

  if LGroup = nil then
  begin
    Exit(TNyxSequence<TElement>.Empty);
  end;
  Result := LGroup.Values;
end;

function TNyxLookupSnapshot<TKey, TElement>.GroupAt(AIndex: Integer): INyxGroup<TKey, TElement>;
begin

  if (AIndex < 0) or (AIndex >= Length(FGroups)) then
  begin
    raise ENyxQuery.Create('Lookup group index is absent');
  end;
  Result := FGroups[AIndex];
end;

function TNyxLookupSnapshot<TKey, TElement>.Groups: INyxSequence<INyxGroup<TKey, TElement>>;
type
  TGroup = INyxGroup<TKey, TElement>;
begin
  Result := TNyxSequence<TGroup>.From(FGroups);
end;

class function TNyxLookup<TSource, TKey, TElement>.From(const ASource: INyxSequence<TSource>;
  const AKey: INyxSelector<TSource, TKey>; const AElement: INyxSelector<TSource, TElement>;
  const AEquality: INyxEquality<TKey>): INyxLookup<TKey, TElement>;
var
  LBuilder: TNyxLookupBuilder<TKey, TElement>;
  LCursor: INyxEnumerator<TSource>;
  LValue: TSource;
  LKey: TKey;
  LElement: TElement;
begin

  if (ASource = nil) or (AKey = nil) or (AElement = nil) or (AEquality = nil) then
  begin
    raise ENyxQuery.Create('Lookup requires source, key/element selectors and typed equality');
  end;
  LBuilder := TNyxLookupBuilder<TKey, TElement>.Create(AEquality);
  try
    LCursor := ASource.GetEnumerator;

    if LCursor = nil then
    begin
      raise ENyxQuery.Create('Lookup source returned no cursor');
    end;
    while LCursor.MoveNext do
    begin
      LValue := LCursor.Current;
      LKey := AKey.Project(LValue);
      LElement := AElement.Project(LValue);
      LBuilder.Add(LKey, LElement);
    end;
    Result := LBuilder.Finish;
  finally
    LBuilder.Free;
  end;
end;

constructor TNyxGroupEnumerator<TSource, TKey, TElement>.Create(const ASource: INyxSequence<TSource>;
  const AKey: INyxSelector<TSource, TKey>; const AElement: INyxSelector<TSource, TElement>;
  const AEquality: INyxEquality<TKey>);
begin
  inherited Create;
  FSource := ASource;
  FKey := AKey;
  FElement := AElement;
  FEquality := AEquality;
end;

function TNyxGroupEnumerator<TSource, TKey, TElement>.Advance(out AValue: INyxGroup<TKey, TElement>): Boolean;
begin
  try

    if FSource <> nil then
    begin
      FLookup := TNyxLookup<TSource, TKey, TElement>.From(FSource, FKey, FElement, FEquality);
      FSource := nil;
      FKey := nil;
      FElement := nil;
      FEquality := nil;
    end;
    Result := FIndex < FLookup.Count;

    if Result then
    begin
      AValue := FLookup.GroupAt(FIndex);
      Inc(FIndex);
    end
    else
    begin
      FLookup := nil;
    end;
  except
    FLookup := nil;
    FSource := nil;
    FKey := nil;
    FElement := nil;
    FEquality := nil;
    raise;
  end;
end;

constructor TNyxGroupSequence<TSource, TKey, TElement>.Create(const ASource: INyxSequence<TSource>;
  const AKey: INyxSelector<TSource, TKey>; const AElement: INyxSelector<TSource, TElement>;
  const AEquality: INyxEquality<TKey>);
begin
  inherited Create;
  FSource := ASource;
  FKey := AKey;
  FElement := AElement;
  FEquality := AEquality;
end;

function TNyxGroupSequence<TSource, TKey, TElement>.GetEnumerator: INyxEnumerator<INyxGroup<TKey, TElement>>;
begin
  Result := TNyxGroupEnumerator<TSource, TKey, TElement>.Create(FSource, FKey, FElement, FEquality);
end;

class function TNyxGroupBy<TSource, TKey, TElement>.From(const ASource: INyxSequence<TSource>;
  const AKey: INyxSelector<TSource, TKey>; const AElement: INyxSelector<TSource, TElement>;
  const AEquality: INyxEquality<TKey>): INyxSequence<INyxGroup<TKey, TElement>>;
begin

  if (ASource = nil) or (AKey = nil) or (AElement = nil) or (AEquality = nil) then
  begin
    raise ENyxQuery.Create('GroupBy requires source, key/element selectors and typed equality');
  end;
  Result := TNyxGroupSequence<TSource, TKey, TElement>.Create(ASource, AKey, AElement, AEquality);
end;

end.
