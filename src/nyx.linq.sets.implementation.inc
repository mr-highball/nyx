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

constructor TQueryKeySet<TKey>.Create(const AEquality: INyxEquality<TKey>);
begin
  inherited Create;
  FEquality := AEquality;
  Grow;
end;

procedure TQueryKeySet<TKey>.Grow;
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
      raise ENyxQuery.Create('Query set capacity exceeds Integer range');
    end;
    LSize := LSize * 2;
  end;
  SetLength(FSlots, LSize);
  for LIndex := 0 to High(FSlots) do
  begin
    FSlots[LIndex] := -1;
  end;
  for LIndex := 0 to FUsed - 1 do
  begin
    LSlot := Integer(FHashes[LIndex] mod Cardinal(LSize));
    while FSlots[LSlot] >= 0 do
    begin
      LSlot := (LSlot + 1) mod LSize;
    end;
    FSlots[LSlot] := LIndex;
  end;
end;

function TQueryKeySet<TKey>.FindSlot(const AKey: TKey; AHash: Cardinal): Integer;
var
  LIndex: Integer;
begin
  Result := Integer(AHash mod Cardinal(Length(FSlots)));
  while FSlots[Result] >= 0 do
  begin
    LIndex := FSlots[Result];

    if (FHashes[LIndex] = AHash) and FEquality.Equal(FKeys[LIndex], AKey) then
    begin
      Exit;
    end;
    Result := (Result + 1) mod Length(FSlots);
  end;
end;

function TQueryKeySet<TKey>.Add(const AKey: TKey): Boolean;
var
  LIndex: Integer;
begin
  Result := FindOrAdd(AKey, LIndex);
end;

function TQueryKeySet<TKey>.FindOrAdd(const AKey: TKey; out AIndex: Integer): Boolean;
var
  LHash: Cardinal;
  LSlot: Integer;
  LIndex: Integer;
  LCapacity: Integer;
begin
  LHash := FEquality.Hash(AKey);
  LSlot := FindSlot(AKey, LHash);
  LIndex := FSlots[LSlot];

  if LIndex >= 0 then
  begin
    AIndex := LIndex;
    Result := not FPresent[LIndex];

    if Result then
    begin
      FPresent[LIndex] := True;
      Inc(FCount);
    end;
    Exit;
  end;

  if FUsed >= Length(FSlots) div 2 then
  begin
    Grow;
    LSlot := FindSlot(AKey, LHash);
  end;

  if FUsed = Length(FKeys) then
  begin
    LCapacity := Length(FKeys);

    if LCapacity = 0 then
    begin
      LCapacity := 8;
    end
    else
    begin

      if LCapacity > High(Integer) div 2 then
      begin
        raise ENyxQuery.Create('Query set key capacity exceeds Integer range');
      end;
      LCapacity := LCapacity * 2;
    end;
    SetLength(FKeys, LCapacity);
    SetLength(FHashes, LCapacity);
    SetLength(FPresent, LCapacity);
  end;
  { All outer storage belongs to the index before generic assignment can raise.
    Failed admission is private to a terminal cursor, never a partial result. }
  FKeys[FUsed] := AKey;
  FHashes[FUsed] := LHash;
  FPresent[FUsed] := True;
  FSlots[LSlot] := FUsed;
  AIndex := FUsed;
  Inc(FUsed);
  Inc(FCount);
  Result := True;
end;

function TQueryKeySet<TKey>.Remove(const AKey: TKey): Boolean;
var
  LIndex: Integer;
begin
  LIndex := FSlots[FindSlot(AKey, FEquality.Hash(AKey))];
  Result := False;

  if (LIndex >= 0) and FPresent[LIndex] then
  begin
    FPresent[LIndex] := False;
    Dec(FCount);
    Result := True;
  end;
end;

function TQuerySetIdentity<T>.Project(const AValue: T): T;
begin
  Result := AValue;
end;

constructor TQuerySetCursor<T, TKey>.Create(const ASource, AOther: INyxSequence<T>;
  const AKeys: INyxSequence<TKey>; const AKey: INyxSelector<T, TKey>;
  const AEquality: INyxEquality<TKey>; AOperation: TQuerySetOperation);
begin
  inherited Create;
  FSource := ASource;
  FOther := AOther;
  FKeys := AKeys;
  FKey := AKey;
  FEquality := AEquality;
  FOperation := AOperation;
end;

destructor TQuerySetCursor<T, TKey>.Destroy;
begin
  Retire;
  inherited Destroy;
end;

procedure TQuerySetCursor<T, TKey>.Retire;
begin
  FSource := nil;
  FOther := nil;
  FKeys := nil;
  FKey := nil;
  FEquality := nil;
  FCursor := nil;
  FreeAndNil(FSet);
end;

procedure TQuerySetCursor<T, TKey>.Prepare;
var
  LKeys: INyxEnumerator<TKey>;
  LKey: TKey;
begin
  FStarted := True;
  FCursor := FSource.GetEnumerator;
  FSource := nil;

  if FCursor = nil then
  begin
    raise ENyxQuery.Create('Query set source returned no cursor');
  end;
  FFirstPending := FCursor.MoveNext;
  FExhausted := not FFirstPending;
  FSet := TQueryKeySet<TKey>.Create(FEquality);

  if not FFirstPending or not (FOperation in [qsoIntersect, qsoExcept]) then
  begin
    Exit;
  end;
  { Nonempty left input owns its first pending value while the complete other
    key input is admitted. A failure here emits nothing. Empty left skips keys. }
  LKeys := FKeys.GetEnumerator;
  FKeys := nil;

  if LKeys = nil then
  begin
    raise ENyxQuery.Create('Query set key input returned no cursor');
  end;
  while LKeys.MoveNext do
  begin
    LKey := LKeys.Current;
    FSet.Add(LKey);
  end;
end;

function TQuerySetCursor<T, TKey>.Advance(out AValue: T): Boolean;
var
  LValue: T;
  LKey: TKey;
  LAvailable: Boolean;
  LKeep: Boolean;
begin
  try

    if not FStarted then
    begin
      Prepare;
    end;
    while True do
    begin

      if (FOperation = qsoIntersect) and (FSet.Count = 0) then
      begin
        Retire;
        Exit(False);
      end;

      if FFirstPending then
      begin
        LAvailable := True;
        FFirstPending := False;
      end
      else if FExhausted then
      begin
        LAvailable := False;
      end
      else
      begin
        LAvailable := FCursor.MoveNext;
        FExhausted := not LAvailable;
      end;

      if not LAvailable then
      begin

        if (FOperation = qsoUnion) and not FReadingOther then
        begin
          FCursor := nil;
          FCursor := FOther.GetEnumerator;
          FOther := nil;
          FReadingOther := True;
          FExhausted := False;

          if FCursor = nil then
          begin
            raise ENyxQuery.Create('Union source returned no cursor');
          end;
          Continue;
        end;
        Retire;
        Exit(False);
      end;
      LValue := FCursor.Current;
      LKey := FKey.Project(LValue);

      if FOperation = qsoIntersect then
      begin
        LKeep := FSet.Remove(LKey);
      end
      else
      begin
        LKeep := FSet.Add(LKey);
      end;

      if LKeep then
      begin
        AValue := LValue;
        Exit(True);
      end;
    end;
  except
    Retire;
    raise;
  end;
end;

constructor TQuerySetSequence<T, TKey>.Create(const ASource, AOther: INyxSequence<T>;
  const AKeys: INyxSequence<TKey>; const AKey: INyxSelector<T, TKey>;
  const AEquality: INyxEquality<TKey>; AOperation: TQuerySetOperation);
begin
  inherited Create;

  if (ASource = nil) or (AKey = nil) or (AEquality = nil) then
  begin
    raise ENyxQuery.Create('Query sets require a source, typed key and equality');
  end;

  if (AOperation = qsoUnion) and (AOther = nil) then
  begin
    raise ENyxQuery.Create('Union requires another typed value source');
  end;

  if (AOperation in [qsoIntersect, qsoExcept]) and (AKeys = nil) then
  begin
    raise ENyxQuery.Create('Intersect/Except require another typed key source');
  end;
  FSource := ASource;
  FOther := AOther;
  FKeys := AKeys;
  FKey := AKey;
  FEquality := AEquality;
  FOperation := AOperation;
end;

function TQuerySetSequence<T, TKey>.GetEnumerator: INyxEnumerator<T>;
begin
  Result := TQuerySetCursor<T, TKey>.Create(FSource, FOther, FKeys, FKey,
    FEquality, FOperation);
end;

class function TNyxSetBy<T, TKey>.DistinctBy(const ASource: INyxSequence<T>;
  const AKey: INyxSelector<T, TKey>;
  const AEquality: INyxEquality<TKey>): INyxSequence<T>;
begin
  Result := TQuerySetSequence<T, TKey>.Create(ASource, nil, nil, AKey,
    AEquality, qsoDistinct);
end;

class function TNyxSetBy<T, TKey>.UnionBy(const ASource, AOther: INyxSequence<T>;
  const AKey: INyxSelector<T, TKey>;
  const AEquality: INyxEquality<TKey>): INyxSequence<T>;
begin
  Result := TQuerySetSequence<T, TKey>.Create(ASource, AOther, nil, AKey,
    AEquality, qsoUnion);
end;

class function TNyxSetBy<T, TKey>.IntersectBy(const ASource: INyxSequence<T>;
  const AKeys: INyxSequence<TKey>; const AKey: INyxSelector<T, TKey>;
  const AEquality: INyxEquality<TKey>): INyxSequence<T>;
begin
  Result := TQuerySetSequence<T, TKey>.Create(ASource, nil, AKeys, AKey,
    AEquality, qsoIntersect);
end;

class function TNyxSetBy<T, TKey>.ExceptBy(const ASource: INyxSequence<T>;
  const AKeys: INyxSequence<TKey>; const AKey: INyxSelector<T, TKey>;
  const AEquality: INyxEquality<TKey>): INyxSequence<T>;
begin
  Result := TQuerySetSequence<T, TKey>.Create(ASource, nil, AKeys, AKey,
    AEquality, qsoExcept);
end;

function TNyxSequence<T>.Distinct(const AEquality: INyxEquality<T>): INyxSequence<T>;
var
  LIdentity: INyxSelector<T, T>;
begin
  LIdentity := TQuerySetIdentity<T>.Create;
  Result := TNyxSetBy<T, T>.DistinctBy(Self, LIdentity, AEquality);
end;

function TNyxSequence<T>.Union(const AOther: INyxSequence<T>;
  const AEquality: INyxEquality<T>): INyxSequence<T>;
var
  LIdentity: INyxSelector<T, T>;
begin
  LIdentity := TQuerySetIdentity<T>.Create;
  Result := TNyxSetBy<T, T>.UnionBy(Self, AOther, LIdentity, AEquality);
end;

function TNyxSequence<T>.Intersect(const AOther: INyxSequence<T>;
  const AEquality: INyxEquality<T>): INyxSequence<T>;
var
  LIdentity: INyxSelector<T, T>;
begin
  LIdentity := TQuerySetIdentity<T>.Create;
  Result := TNyxSetBy<T, T>.IntersectBy(Self, AOther, LIdentity, AEquality);
end;

function TNyxSequence<T>.ExceptWith(const AOther: INyxSequence<T>;
  const AEquality: INyxEquality<T>): INyxSequence<T>;
var
  LIdentity: INyxSelector<T, T>;
begin
  LIdentity := TQuerySetIdentity<T>.Create;
  Result := TNyxSetBy<T, T>.ExceptBy(Self, AOther, LIdentity, AEquality);
end;

function TNyxSequence<T>.Contains(const AValue: T;
  const AEquality: INyxEquality<T>): Boolean;
var
  LCursor: INyxEnumerator<T>;
  LValue: T;
begin

  if AEquality = nil then
  begin
    raise ENyxQuery.Create('Contains requires typed equality');
  end;
  LCursor := GetEnumerator;

  if LCursor = nil then
  begin
    raise ENyxQuery.Create('Contains source returned no cursor');
  end;
  while LCursor.MoveNext do
  begin
    LValue := LCursor.Current;

    if AEquality.Equal(LValue, AValue) then
    begin
      Exit(True);
    end;
  end;
  Result := False;
end;

function TNyxSequence<T>.SequenceEqual(const AOther: INyxSequence<T>;
  const AEquality: INyxEquality<T>): Boolean;
var
  LLeft: INyxEnumerator<T>;
  LRight: INyxEnumerator<T>;
  LLeftValue: T;
  LRightValue: T;
  LHasLeft: Boolean;
  LHasRight: Boolean;
begin

  if (AOther = nil) or (AEquality = nil) then
  begin
    raise ENyxQuery.Create('SequenceEqual requires another source and typed equality');
  end;
  LLeft := GetEnumerator;
  LRight := AOther.GetEnumerator;

  if (LLeft = nil) or (LRight = nil) then
  begin
    raise ENyxQuery.Create('SequenceEqual source returned no cursor');
  end;
  while True do
  begin
    LHasLeft := LLeft.MoveNext;
    LHasRight := LRight.MoveNext;

    if LHasLeft <> LHasRight then
    begin
      Exit(False);
    end;

    if not LHasLeft then
    begin
      Exit(True);
    end;
    LLeftValue := LLeft.Current;
    LRightValue := LRight.Current;

    if not AEquality.Equal(LLeftValue, LRightValue) then
    begin
      Exit(False);
    end;
  end;
end;
