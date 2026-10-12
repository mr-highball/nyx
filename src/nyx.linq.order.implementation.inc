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

constructor TNyxFunctionComparer<T>.Create(AFunction: TNyxCompareFunction<T>);
begin
  inherited Create;
  FFunction := AFunction;
end;

function TNyxFunctionComparer<T>.Compare(const ALeft, ARight: T): Integer;
begin
  Result := FFunction(ALeft, ARight);
end;

class function TNyxComparer<T>.From(
  AFunction: TNyxCompareFunction<T>): INyxComparer<T>;
begin

  if not Assigned(AFunction) then
  begin
    raise ENyxQuery.Create('Comparer requires a function');
  end;
  Result := TNyxFunctionComparer<T>.Create(AFunction);
end;

function TNyxOrdering<T>.ThenBy(const AOther: INyxOrdering<T>): INyxOrdering<T>;
begin

  if AOther = nil then
  begin
    raise ENyxQuery.Create('ThenBy requires an ordering recipe');
  end;
  Result := TNyxCombinedOrdering<T>.Create(Self, AOther);
end;

constructor TNyxCombinedKeys.Create(const APrimary, ASecondary: INyxOrderKeys);
begin
  inherited Create;

  if (APrimary = nil) or (ASecondary = nil) then
  begin
    raise ENyxQuery.Create('An ordering recipe returned no prepared keys');
  end;
  FPrimary := APrimary;
  FSecondary := ASecondary;
end;

function TNyxCombinedKeys.Compare(ALeft, ARight: Integer): Integer;
begin
  Result := FPrimary.Compare(ALeft, ARight);

  if Result = 0 then
  begin
    Result := FSecondary.Compare(ALeft, ARight);
  end;
end;

constructor TNyxCombinedOrdering<T>.Create(
  const APrimary, ASecondary: INyxOrdering<T>);
begin
  inherited Create;
  FPrimary := APrimary;
  FSecondary := ASecondary;
end;

function TNyxCombinedOrdering<T>.Prepare(const AValues: array of T): INyxOrderKeys;
var
  LPrimary: INyxOrderKeys;
  LSecondary: INyxOrderKeys;
begin
  LPrimary := FPrimary.Prepare(AValues);
  LSecondary := FSecondary.Prepare(AValues);
  Result := TNyxCombinedKeys.Create(LPrimary, LSecondary);
end;

constructor TNyxCachedKeys<T>.Create(const AValues: array of T;
  const AComparer: INyxComparer<T>; ADirection: TNyxOrderDirection);
var
  LIndex: Integer;
begin
  inherited Create;
  FComparer := AComparer;
  FDirection := ADirection;
  SetLength(FValues, Length(AValues));
  for LIndex := 0 to High(AValues) do
  begin
    FValues[LIndex] := AValues[LIndex];
  end;
end;

function TNyxCachedKeys<T>.Compare(ALeft, ARight: Integer): Integer;
var
  LCompared: Integer;
begin

  if (ALeft < 0) or (ARight < 0) or (ALeft >= Length(FValues)) or
    (ARight >= Length(FValues)) then
  begin
    raise ENyxQuery.Create('Ordering key index is outside its input');
  end;
  LCompared := FComparer.Compare(FValues[ALeft], FValues[ARight]);
  Result := 0;

  if LCompared < 0 then
  begin
    Result := -1;
  end
  else
  begin

    if LCompared > 0 then
    begin
      Result := 1;
    end;
  end;

  if FDirection = nodDescending then
  begin
    Result := -Result;
  end;
end;

constructor TNyxValueOrdering<T>.Create(const AComparer: INyxComparer<T>;
  ADirection: TNyxOrderDirection);
begin
  inherited Create;

  if (AComparer = nil) or (Ord(ADirection) < Ord(Low(TNyxOrderDirection))) or
    (Ord(ADirection) > Ord(High(TNyxOrderDirection))) then
  begin
    raise ENyxQuery.Create('Ordering requires a comparer and valid direction');
  end;
  FComparer := AComparer;
  FDirection := ADirection;
end;

function TNyxValueOrdering<T>.Prepare(const AValues: array of T): INyxOrderKeys;
begin
  Result := TNyxCachedKeys<T>.Create(AValues, FComparer, FDirection);
end;

constructor TNyxKeyOrdering<TSource, TKey>.Create(
  const ASelector: INyxSelector<TSource, TKey>; const AComparer: INyxComparer<TKey>;
  ADirection: TNyxOrderDirection);
begin
  inherited Create;

  if (ASelector = nil) or (AComparer = nil) or
    (Ord(ADirection) < Ord(Low(TNyxOrderDirection))) or
    (Ord(ADirection) > Ord(High(TNyxOrderDirection))) then
  begin
    raise ENyxQuery.Create('Key ordering requires a selector, comparer and direction');
  end;
  FSelector := ASelector;
  FComparer := AComparer;
  FDirection := ADirection;
end;

function TNyxKeyOrdering<TSource, TKey>.Prepare(
  const AValues: array of TSource): INyxOrderKeys;
var
  LKeys: TNyxSequenceArray<TKey>;
  LIndex: Integer;
begin
  SetLength(LKeys, Length(AValues));
  for LIndex := 0 to High(AValues) do
  begin
    LKeys[LIndex] := FSelector.Project(AValues[LIndex]);
  end;
  Result := TNyxCachedKeys<TKey>.Create(LKeys, FComparer, FDirection);
end;

class function TNyxValueOrder<T>.By(const AComparer: INyxComparer<T>;
  ADirection: TNyxOrderDirection): INyxOrdering<T>;
begin
  Result := TNyxValueOrdering<T>.Create(AComparer, ADirection);
end;

class function TNyxKeyOrder<TSource, TKey>.By(
  const ASelector: INyxSelector<TSource, TKey>; const AComparer: INyxComparer<TKey>;
  ADirection: TNyxOrderDirection): INyxOrdering<TSource>;
begin
  Result := TNyxKeyOrdering<TSource, TKey>.Create(ASelector, AComparer, ADirection);
end;

constructor TNyxOrderedEnumerator<T>.Create(const ASource: INyxSequence<T>;
  const AOrdering: INyxOrdering<T>);
begin
  inherited Create;
  FSource := ASource;
  FOrdering := AOrdering;
end;

procedure TNyxOrderedEnumerator<T>.PrepareRows;
var
  LKeys: INyxOrderKeys;
  LTemporary: array of Integer;
  LCount: Integer;
  LIndex: Integer;
  LWidth: Integer;
  LStart: Integer;
  LMiddle: Integer;
  LStop: Integer;
  LLeft: Integer;
  LRight: Integer;
  LWrite: Integer;
  LChooseLeft: Boolean;
begin
  FValues := FSource.ToArray;
  LCount := Length(FValues);
  LKeys := FOrdering.Prepare(FValues);

  if LKeys = nil then
  begin
    raise ENyxQuery.Create('An ordering recipe returned no prepared keys');
  end;
  SetLength(FRows, LCount);
  SetLength(LTemporary, LCount);
  for LIndex := 0 to LCount - 1 do
  begin
    FRows[LIndex] := LIndex;
  end;
  LWidth := 1;
  while LWidth < LCount do
  begin
    LStart := 0;
    while LStart < LCount do
    begin
      { Bound every addition to Count, including lengths near High(Integer).
        Stable ties choose the earlier left source ordinal. }
      LMiddle := LCount;

      if LWidth < LCount - LStart then
      begin
        LMiddle := LStart + LWidth;
      end;
      LStop := LCount;

      if LWidth < LCount - LMiddle then
      begin
        LStop := LMiddle + LWidth;
      end;
      LLeft := LStart;
      LRight := LMiddle;
      for LWrite := LStart to LStop - 1 do
      begin
        LChooseLeft := LRight >= LStop;

        if (LLeft < LMiddle) and (LRight < LStop) then
        begin
          LChooseLeft := LKeys.Compare(FRows[LLeft], FRows[LRight]) <= 0;
        end;

        if LChooseLeft then
        begin
          LTemporary[LWrite] := FRows[LLeft];
          Inc(LLeft);
        end
        else
        begin
          LTemporary[LWrite] := FRows[LRight];
          Inc(LRight);
        end;
      end;
      LStart := LStop;
    end;
    for LIndex := 0 to LCount - 1 do
    begin
      FRows[LIndex] := LTemporary[LIndex];
    end;

    if LWidth > LCount div 2 then
    begin
      LWidth := LCount;
    end
    else
    begin
      LWidth := LWidth * 2;
    end;
  end;
  { Preparation succeeded completely. The cursor owns values/permutation now;
    recipes, callbacks and input sequences can retire before the first yield. }
  FSource := nil;
  FOrdering := nil;
  FPrepared := True;
end;

function TNyxOrderedEnumerator<T>.Advance(out AValue: T): Boolean;
begin

  if not FPrepared then
  begin
    PrepareRows;
  end;
  Result := FIndex < Length(FRows);

  if Result then
  begin
    AValue := FValues[FRows[FIndex]];
    Inc(FIndex);
  end;
end;

constructor TNyxOrderedSequence<T>.Create(const ASource: INyxSequence<T>;
  const AOrdering: INyxOrdering<T>);
begin
  inherited Create;
  FSource := ASource;
  FOrdering := AOrdering;
end;

function TNyxOrderedSequence<T>.GetEnumerator: INyxEnumerator<T>;
begin
  Result := TNyxOrderedEnumerator<T>.Create(FSource, FOrdering);
end;

function TNyxSequence<T>.OrderBy(const AOrdering: INyxOrdering<T>): INyxSequence<T>;
begin

  if AOrdering = nil then
  begin
    raise ENyxQuery.Create('OrderBy requires an ordering recipe');
  end;
  Result := TNyxOrderedSequence<T>.Create(Self, AOrdering);
end;

function TNyxIntegerComparer.Compare(const ALeft, ARight: Integer): Integer;
begin
  Result := 0;

  if ALeft < ARight then
  begin
    Result := -1;
  end
  else
  begin

    if ALeft > ARight then
    begin
      Result := 1;
    end;
  end;
end;

function TNyxBooleanComparer.Compare(const ALeft, ARight: Boolean): Integer;
begin
  Result := Ord(ALeft) - Ord(ARight);
end;

constructor TNyxTextComparer.Create(AMode: TNyxTextOrder);
begin
  inherited Create;

  if (Ord(AMode) < Ord(Low(TNyxTextOrder))) or
    (Ord(AMode) > Ord(High(TNyxTextOrder))) then
  begin
    raise ENyxQuery.Create('Text ordering requires a valid comparison mode');
  end;
  FMode := AMode;
end;

function TNyxTextComparer.Compare(const ALeft, ARight: TNyxText): Integer;
var
  LLeftIndex: Integer;
  LRightIndex: Integer;
  LLeft: Integer;
  LRight: Integer;
  LHasLeft: Boolean;
  LHasRight: Boolean;
begin
  Result := 0;
  LLeftIndex := 1;
  LRightIndex := 1;
  repeat
    LHasLeft := LLeftIndex <= Length(ALeft);
    LHasRight := LRightIndex <= Length(ARight);

    if LHasLeft and not NyxNextScalar(ALeft, LLeftIndex, LLeft) then
    begin
      raise ENyxQuery.Create('Text ordering requires valid Unicode');
    end;

    if LHasRight and not NyxNextScalar(ARight, LRightIndex, LRight) then
    begin
      raise ENyxQuery.Create('Text ordering requires valid Unicode');
    end;

    if Result = 0 then
    begin

      if not LHasLeft or not LHasRight then
      begin
        Result := Ord(LHasLeft) - Ord(LHasRight);
      end
      else
      begin

        if FMode = ntoAsciiInsensitive then
        begin

          if (LLeft >= Ord('A')) and (LLeft <= Ord('Z')) then
          begin
            Inc(LLeft, 32);
          end;

          if (LRight >= Ord('A')) and (LRight <= Ord('Z')) then
          begin
            Inc(LRight, 32);
          end;
        end;

        if LLeft < LRight then
        begin
          Result := -1;
        end
        else
        begin

          if LLeft > LRight then
          begin
            Result := 1;
          end;
        end;
      end;
    end;
  until not LHasLeft and not LHasRight;
end;

constructor TNyxNumberComparer.Create(ANaN: TNyxNaNOrder);
begin
  inherited Create;

  if (Ord(ANaN) < Ord(Low(TNyxNaNOrder))) or (Ord(ANaN) > Ord(High(TNyxNaNOrder))) then
  begin
    raise ENyxQuery.Create('Number ordering requires a valid NaN policy');
  end;
  FNaN := ANaN;
end;

function TNyxNumberComparer.Compare(const ALeft, ARight: Double): Integer;
var
  LLeftNaN: Boolean;
  LRightNaN: Boolean;
begin
  Result := 0;
  LLeftNaN := IsNan(ALeft);
  LRightNaN := IsNan(ARight);

  if LLeftNaN or LRightNaN then
  begin

    if FNaN = nnoRefuse then
    begin
      raise ENyxQuery.Create('This number ordering refuses NaN');
    end;
    Result := Ord(LRightNaN) - Ord(LLeftNaN);

    if FNaN = nnoLast then
    begin
      Result := -Result;
    end;
  end
  else
  begin

    if ALeft < ARight then
    begin
      Result := -1;
    end
    else
    begin

      if ALeft > ARight then
      begin
        Result := 1;
      end;
    end;
  end;
end;

function NewNyxIntegerComparer: INyxComparer<Integer>;
begin
  Result := TNyxIntegerComparer.Create;
end;

function NewNyxBooleanComparer: INyxComparer<Boolean>;
begin
  Result := TNyxBooleanComparer.Create;
end;

function NewNyxTextComparer(AMode: TNyxTextOrder): INyxComparer<TNyxText>;
begin
  Result := TNyxTextComparer.Create(AMode);
end;

function NewNyxNumberComparer(ANaN: TNyxNaNOrder): INyxComparer<Double>;
begin
  Result := TNyxNumberComparer.Create(ANaN);
end;
