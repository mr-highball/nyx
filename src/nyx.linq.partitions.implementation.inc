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

constructor TQueryBuffer<T>.Create(ALimit: Integer);
begin
  inherited Create;
  FLimit := ALimit;
end;

procedure TQueryBuffer<T>.Add(const AValue: T);
var
  LCapacity: Integer;
begin

  if FCount >= FLimit then
  begin
    raise ENyxQuery.Create('Query buffer exceeds its admitted Integer limit');
  end;

  if FCount = Length(FValues) then
  begin
    LCapacity := 16;

    if FCount > 0 then
    begin

      if FCount > High(Integer) div 2 then
      begin
        LCapacity := High(Integer);
      end
      else
      begin
        LCapacity := FCount * 2;
      end;
    end;

    if LCapacity > FLimit then
    begin
      LCapacity := FLimit;
    end;
    SetLength(FValues, LCapacity);
  end;
  FValues[FCount] := AValue;
  Inc(FCount);
end;

function TQueryBuffer<T>.ToArray: TNyxSequenceArray<T>;
var
  LIndex: Integer;
begin
  Result := nil;
  SetLength(Result, FCount);
  for LIndex := 0 to FCount - 1 do
  begin
    Result[LIndex] := FValues[LIndex];
  end;
end;

constructor TQueryRing<T>.Create(ALimit: Integer);
begin
  inherited Create;
  FLimit := ALimit;
end;

function TQueryRing<T>.Offset(AOffset: Integer): Integer;
begin
  { Avoid Head+Offset overflow even for an admitted Integer-sized ring. }

  if AOffset >= Length(FValues) - FHead then
  begin
    Result := AOffset - (Length(FValues) - FHead);
  end
  else
  begin
    Result := FHead + AOffset;
  end;
end;

procedure TQueryRing<T>.Grow;
var
  LValues: TNyxSequenceArray<T>;
  LCapacity: Integer;
  LIndex: Integer;
begin
  LValues := nil;
  LCapacity := 16;

  if Length(FValues) > 0 then
  begin

    if Length(FValues) > High(Integer) div 2 then
    begin
      LCapacity := High(Integer);
    end
    else
    begin
      LCapacity := Length(FValues) * 2;
    end;
  end;

  if LCapacity > FLimit then
  begin
    LCapacity := FLimit;
  end;
  SetLength(LValues, LCapacity);
  for LIndex := 0 to FCount - 1 do
  begin
    LValues[LIndex] := FValues[Offset(LIndex)];
  end;
  FValues := LValues;
  FHead := 0;
end;

function TQueryRing<T>.Push(const AValue: T; out AReplaced: T): Boolean;
begin
  AReplaced := FEmpty;

  if FLimit <= 0 then
  begin
    raise ENyxQuery.Create('A zero-sized query ring cannot retain values');
  end;
  Result := FCount = FLimit;

  if Result then
  begin
    AReplaced := FValues[FHead];
    FValues[FHead] := AValue;
    FHead := Offset(1);
  end
  else
  begin

    if FCount = Length(FValues) then
    begin
      Grow;
    end;
    FValues[Offset(FCount)] := AValue;
    Inc(FCount);
  end;
end;

function TQueryRing<T>.Pop(out AValue: T): Boolean;
begin
  AValue := FEmpty;
  Result := FCount > 0;

  if Result then
  begin
    AValue := FValues[FHead];
    FValues[FHead] := FEmpty;
    FHead := Offset(1);
    Dec(FCount);
  end;
end;

function NyxRange(AStart, ACount: Integer; AStep: Integer): INyxSequence<Integer>;
var
  {$ifdef PAS2JS}
  LSpan: NativeInt;
  LStep: NativeInt;
  {$else}
  LSpan: Int64;
  LStep: Int64;
  {$endif}
begin

  if (ACount < 0) or (AStep = 0) then
  begin
    raise ENyxQuery.Create('Range requires nonnegative count and nonzero step');
  end;

  if ACount > 0 then
  begin
    LStep := AStep;

    if LStep > 0 then
    begin
      LSpan := High(Integer);
      LSpan := LSpan - AStart;
    end
    else
    begin
      LSpan := AStart;
      LSpan := LSpan - Low(Integer);
      LStep := -LStep;
    end;
    { Span is at most 2^32-1, exact on both targets. Division validates before
      multiplication could exceed browser safe integers or checked native range. }

    if ACount - 1 > LSpan div LStep then
    begin
      raise ENyxQuery.Create('Range would leave the Integer domain');
    end;
  end;
  Result := TQueryRangeSequence.Create(AStart, ACount, AStep);
end;

constructor TQueryRangeCursor.Create(AStart, ACount, AStep: Integer);
begin
  inherited Create;
  FValue := AStart;
  FRemaining := ACount;
  FStep := AStep;
  FFirst := True;
end;

function TQueryRangeCursor.Advance(out AValue: Integer): Boolean;
begin
  Result := FRemaining > 0;

  if Result then
  begin

    if not FFirst then
    begin
      FValue := FValue + FStep;
    end;
    FFirst := False;
    AValue := FValue;
    Dec(FRemaining);
  end;
end;

constructor TQueryRangeSequence.Create(AStart, ACount, AStep: Integer);
begin
  inherited Create;
  FStart := AStart;
  FCount := ACount;
  FStep := AStep;
end;

function TQueryRangeSequence.GetEnumerator: INyxEnumerator<Integer>;
begin
  Result := TQueryRangeCursor.Create(FStart, FCount, FStep);
end;

function TQueryRangeSequence.NonEnumeratedCount(out AValue: TNyxQueryCount): Boolean;
begin
  AValue := NyxQueryCount(Cardinal(FCount));
  Result := True;
end;

constructor TQueryGenerateCursor<T>.Create(const AValue: T; ACount: Integer;
  const ANext: INyxSelector<T, T>);
begin
  inherited Create;
  FValue := AValue;
  FRemaining := ACount;
  FFirst := True;
  FNext := ANext;
end;

function TQueryGenerateCursor<T>.Advance(out AValue: T): Boolean;
begin
  try
    Result := FRemaining > 0;

    if Result then
    begin

      if not FFirst and (FNext <> nil) then
      begin
        FValue := FNext.Project(FValue);
      end;
      FFirst := False;
      AValue := FValue;
      Dec(FRemaining);
    end
    else
    begin
      FNext := nil;
      FValue := FEmpty;
    end;
  except
    FRemaining := 0;
    FNext := nil;
    FValue := FEmpty;
    raise;
  end;
end;

constructor TQueryGenerateSequence<T>.Create(const AValue: T; ACount: Integer;
  const ANext: INyxSelector<T, T>);
begin
  inherited Create;
  FValue := AValue;
  FCount := ACount;
  FNext := ANext;
end;

function TQueryGenerateSequence<T>.GetEnumerator: INyxEnumerator<T>;
begin
  Result := TQueryGenerateCursor<T>.Create(FValue, FCount, FNext);
end;

function TQueryGenerateSequence<T>.NonEnumeratedCount(out AValue: TNyxQueryCount): Boolean;
begin
  AValue := NyxQueryCount(Cardinal(FCount));
  Result := True;
end;

class function TNyxSequence<T>.RepeatValue(const AValue: T; ACount: Integer): INyxSequence<T>;
begin

  if ACount < 0 then
  begin
    raise ENyxQuery.Create('Repeat count must be nonnegative');
  end;
  Result := TQueryGenerateSequence<T>.Create(AValue, ACount, nil);
end;

class function TNyxSequence<T>.Iterate(const ASeed: T; ACount: Integer;
  const ANext: INyxSelector<T, T>): INyxSequence<T>;
begin

  if (ACount < 0) or (ANext = nil) then
  begin
    raise ENyxQuery.Create('Iterate requires nonnegative count and owned next selector');
  end;
  Result := TQueryGenerateSequence<T>.Create(ASeed, ACount, ANext);
end;

constructor TQueryInsertCursor<T>.Create(const ASource: INyxSequence<T>;
  const AValue: T; AOperation: TQueryInsertOperation);
begin
  inherited Create;
  FSource := ASource;
  FValue := AValue;
  FOperation := AOperation;
end;

procedure TQueryInsertCursor<T>.Retire;
begin
  FSource := nil;
  FCursor := nil;
  FValue := FEmpty;
  FFinished := True;
end;

function TQueryInsertCursor<T>.Advance(out AValue: T): Boolean;
begin
  try

    if FFinished then
    begin
      Exit(False);
    end;

    if (FOperation = qioPrepend) and not FInserted then
    begin
      FInserted := True;
      AValue := FValue;
      FValue := FEmpty;
      Exit(True);
    end;

    if FCursor = nil then
    begin
      FCursor := FSource.GetEnumerator;
      FSource := nil;

      if FCursor = nil then
      begin
        raise ENyxQuery.Create('Inserted-value source returned no cursor');
      end;
    end;

    if FCursor.MoveNext then
    begin
      FHadValue := True;
      AValue := FCursor.Current;
      Exit(True);
    end;

    if (FOperation = qioAppend) or ((FOperation = qioDefault) and not FHadValue) then
    begin
      AValue := FValue;
      Retire;
      Exit(True);
    end;
    Retire;
    Result := False;
  except
    Retire;
    raise;
  end;
end;

constructor TQueryInsertSequence<T>.Create(const ASource: INyxSequence<T>;
  const AValue: T; AOperation: TQueryInsertOperation);
begin
  inherited Create;
  FSource := ASource;
  FValue := AValue;
  FOperation := AOperation;
end;

function TQueryInsertSequence<T>.GetEnumerator: INyxEnumerator<T>;
begin
  Result := TQueryInsertCursor<T>.Create(FSource, FValue, FOperation);
end;

function TNyxSequence<T>.Append(const AValue: T): INyxSequence<T>;
begin
  Result := TQueryInsertSequence<T>.Create(Self, AValue, qioAppend);
end;

function TNyxSequence<T>.Prepend(const AValue: T): INyxSequence<T>;
begin
  Result := TQueryInsertSequence<T>.Create(Self, AValue, qioPrepend);
end;

function TNyxSequence<T>.DefaultIfEmpty: INyxSequence<T>;
begin
  Result := DefaultIfEmpty(FEmptyValue);
end;

function TNyxSequence<T>.DefaultIfEmpty(const AFallback: T): INyxSequence<T>;
begin
  Result := TQueryInsertSequence<T>.Create(Self, AFallback, qioDefault);
end;

constructor TQueryTailCursor<T>.Create(const ASource: INyxSequence<T>;
  ACount: Integer; AOperation: TQueryTailOperation);
begin
  inherited Create;
  FSource := ASource;
  FLimit := ACount;
  FOperation := AOperation;
end;

destructor TQueryTailCursor<T>.Destroy;
begin
  Retire;
  inherited Destroy;
end;

procedure TQueryTailCursor<T>.Retire;
begin
  FSource := nil;
  FCursor := nil;
  FreeAndNil(FRing);
  FFinished := True;
end;

function TQueryTailCursor<T>.Advance(out AValue: T): Boolean;
var
  LValue: T;
  LDiscard: T;
begin
  try

    if FFinished then
    begin
      Exit(False);
    end;

    if (FOperation = qtoTake) and (FLimit = 0) then
    begin
      Retire;
      Exit(False);
    end;

    if not FPrepared then
    begin
      FCursor := FSource.GetEnumerator;
      FSource := nil;

      if FCursor = nil then
      begin
        raise ENyxQuery.Create('Tail source returned no cursor');
      end;

      if FLimit > 0 then
      begin
        FRing := TQueryRing<T>.Create(FLimit);
      end;
      FPrepared := True;

      if FOperation = qtoTake then
      begin
        while FCursor.MoveNext do
        begin
          LValue := FCursor.Current;
          FRing.Push(LValue, LDiscard);
        end;
        FCursor := nil;
      end;
    end;

    if FOperation = qtoTake then
    begin
      Result := FRing.Pop(AValue);

      if not Result then
      begin
        Retire;
      end;
      Exit;
    end;
    while FCursor.MoveNext do
    begin
      LValue := FCursor.Current;

      if FLimit = 0 then
      begin
        AValue := LValue;
        Exit(True);
      end;

      if FRing.Push(LValue, AValue) then
      begin
        Exit(True);
      end;
    end;
    Retire;
    Result := False;
  except
    Retire;
    raise;
  end;
end;

constructor TQueryTailSequence<T>.Create(const ASource: INyxSequence<T>;
  ACount: Integer; AOperation: TQueryTailOperation);
begin
  inherited Create;
  FSource := ASource;
  FCount := ACount;
  FOperation := AOperation;
end;

function TQueryTailSequence<T>.GetEnumerator: INyxEnumerator<T>;
begin
  Result := TQueryTailCursor<T>.Create(FSource, FCount, FOperation);
end;

function TNyxSequence<T>.SkipLast(ACount: Integer): INyxSequence<T>;
begin

  if ACount < 0 then
  begin
    raise ENyxQuery.Create('SkipLast count must be nonnegative');
  end;
  Result := TQueryTailSequence<T>.Create(Self, ACount, qtoSkip);
end;

function TNyxSequence<T>.TakeLast(ACount: Integer): INyxSequence<T>;
begin

  if ACount < 0 then
  begin
    raise ENyxQuery.Create('TakeLast count must be nonnegative');
  end;
  Result := TQueryTailSequence<T>.Create(Self, ACount, qtoTake);
end;

constructor TQueryReverseCursor<T>.Create(const ASource: INyxSequence<T>);
begin
  inherited Create;
  FSource := ASource;
end;

function TQueryReverseCursor<T>.Advance(out AValue: T): Boolean;
var
  LCursor: INyxEnumerator<T>;
  LValue: T;
  LBuffer: TQueryBuffer<T>;
begin
  try

    if not FStarted then
    begin
      FStarted := True;
      LBuffer := TQueryBuffer<T>.Create(High(Integer));
      try
        LCursor := FSource.GetEnumerator;
        FSource := nil;

        if LCursor = nil then
        begin
          raise ENyxQuery.Create('Reverse source returned no cursor');
        end;
        while LCursor.MoveNext do
        begin
          LValue := LCursor.Current;
          LBuffer.Add(LValue);
        end;
        FValues := LBuffer.ToArray;
        FIndex := LBuffer.Count - 1;
      finally
        LBuffer.Free;
      end;
    end;
    Result := FIndex >= 0;

    if Result then
    begin
      AValue := FValues[FIndex];
      Dec(FIndex);
    end
    else
    begin
      FValues := nil;
    end;
  except
    FSource := nil;
    FValues := nil;
    raise;
  end;
end;

constructor TQueryReverseSequence<T>.Create(const ASource: INyxSequence<T>);
begin
  inherited Create;
  FSource := ASource;
end;

function TQueryReverseSequence<T>.GetEnumerator: INyxEnumerator<T>;
begin
  Result := TQueryReverseCursor<T>.Create(FSource);
end;

function TNyxSequence<T>.Reverse: INyxSequence<T>;
begin
  Result := TQueryReverseSequence<T>.Create(Self);
end;

constructor TQueryChunkCursor<T>.Create(const ASource: INyxSequence<T>; ASize: Integer);
begin
  inherited Create;
  FSource := ASource;
  FSize := ASize;
end;

function TQueryChunkCursor<T>.Advance(out AValue: INyxSequence<T>): Boolean;
var
  LValues: TNyxSequenceArray<T>;
  LValue: T;
  LBuffer: TQueryBuffer<T>;
begin
  LValues := nil;
  try

    if FFinished then
    begin
      Exit(False);
    end;

    if FCursor = nil then
    begin
      FCursor := FSource.GetEnumerator;
      FSource := nil;

      if FCursor = nil then
      begin
        raise ENyxQuery.Create('Chunk source returned no cursor');
      end;
    end;
    LBuffer := TQueryBuffer<T>.Create(FSize);
    try
      while LBuffer.Count < FSize do
      begin

        if not FCursor.MoveNext then
        begin
          FCursor := nil;
          FFinished := True;
          Break;
        end;
        LValue := FCursor.Current;
        LBuffer.Add(LValue);
      end;
      Result := LBuffer.Count > 0;

      if Result then
      begin
        LValues := LBuffer.ToArray;
        AValue := TNyxSequence<T>.From(LValues);
      end;
    finally
      LBuffer.Free;
    end;
  except
    FSource := nil;
    FCursor := nil;
    FFinished := True;
    raise;
  end;
end;

constructor TQueryChunkSequence<T>.Create(const ASource: INyxSequence<T>; ASize: Integer);
begin
  inherited Create;
  FSource := ASource;
  FSize := ASize;
end;

function TQueryChunkSequence<T>.GetEnumerator: INyxEnumerator<INyxSequence<T>>;
begin
  Result := TQueryChunkCursor<T>.Create(FSource, FSize);
end;

class function TNyxChunk<T>.From(const ASource: INyxSequence<T>; ASize: Integer):
  INyxSequence<INyxSequence<T>>;
begin

  if (ASource = nil) or (ASize <= 0) then
  begin
    raise ENyxQuery.Create('Chunk requires a source and positive size');
  end;
  Result := TQueryChunkSequence<T>.Create(ASource, ASize);
end;

function TNyxSequence<T>.TryElementAt(AIndex: Integer; out AValue: T): Boolean;
var
  LCursor: INyxEnumerator<T>;
begin
  AValue := FEmptyValue;
  Result := False;

  if AIndex < 0 then
  begin
    Exit;
  end;
  LCursor := GetEnumerator;

  if LCursor = nil then
  begin
    raise ENyxQuery.Create('Element source returned no cursor');
  end;
  while LCursor.MoveNext do
  begin

    if AIndex = 0 then
    begin
      AValue := LCursor.Current;
      Exit(True);
    end;
    Dec(AIndex);
  end;
end;

function TNyxSequence<T>.FirstOrDefault: T;
begin
  Result := FirstOrDefault(FEmptyValue);
end;

function TNyxSequence<T>.FirstOrDefault(const AFallback: T): T;
begin

  if not TryFirst(Result) then
  begin
    Result := AFallback;
  end;
end;

function TNyxSequence<T>.LastOrDefault: T;
begin
  Result := LastOrDefault(FEmptyValue);
end;

function TNyxSequence<T>.LastOrDefault(const AFallback: T): T;
begin

  if not TryLast(Result) then
  begin
    Result := AFallback;
  end;
end;

function TNyxSequence<T>.SingleOrDefault: T;
begin
  Result := SingleOrDefault(FEmptyValue);
end;

function TNyxSequence<T>.SingleOrDefault(const AFallback: T): T;
begin

  if not TrySingle(Result) then
  begin
    Result := AFallback;
  end;
end;

function TNyxSequence<T>.ElementAtOrDefault(AIndex: Integer): T;
begin
  Result := ElementAtOrDefault(AIndex, FEmptyValue);
end;

function TNyxSequence<T>.ElementAtOrDefault(AIndex: Integer; const AFallback: T): T;
begin

  if not TryElementAt(AIndex, Result) then
  begin
    Result := AFallback;
  end;
end;
