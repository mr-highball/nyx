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

constructor TNyxIndexedValue<T>.Create(const AValue: T; AIndex: Integer);
begin
  inherited Create;
  FValue := AValue;
  FIndex := AIndex;
end;

function TNyxIndexedValue<T>.GetValue: T;
begin
  Result := FValue;
end;

function TNyxIndexedValue<T>.GetIndex: Integer;
begin
  Result := FIndex;
end;

function NextNyxQueryIndex(var AIndex: Integer; var AExhausted: Boolean): Integer;
begin

  if AExhausted or (AIndex < 0) then
  begin
    raise ENyxQuery.Create('Query ordinal exceeds Integer range');
  end;
  Result := AIndex;

  if AIndex = High(Integer) then
  begin
    AExhausted := True;
  end
  else
  begin
    Inc(AIndex);
  end;
end;

constructor TIndexedPredicateAdapter<T>.Create(const APredicate: INyxPredicate<INyxIndexedValue<T>>);
begin
  inherited Create;
  FPredicate := APredicate;
end;

function TIndexedPredicateAdapter<T>.Test(const AValue: T; AIndex: Integer): Boolean;
var
  LInput: INyxIndexedValue<T>;
begin
  LInput := TNyxIndexedValue<T>.Create(AValue, AIndex);
  Result := FPredicate.Test(LInput);
end;

class function TNyxIndexedPredicate<T>.From(AFunction: TNyxPredicateFunction<INyxIndexedValue<T>>):
  INyxIndexedPredicate<T>;
type
  TContext = INyxIndexedValue<T>;
begin
  Result := Wrap(TNyxPredicate<TContext>.From(AFunction));
end;

class function TNyxIndexedPredicate<T>.Wrap(const APredicate: INyxPredicate<INyxIndexedValue<T>>):
  INyxIndexedPredicate<T>;
begin

  if APredicate = nil then
  begin
    raise ENyxQuery.Create('Indexed predicate requires an owned callback');
  end;
  Result := TIndexedPredicateAdapter<T>.Create(APredicate);
end;

constructor TIndexedSelectorAdapter<TSource, TResult>.Create(
  const ASelector: INyxSelector<INyxIndexedValue<TSource>, TResult>);
begin
  inherited Create;
  FSelector := ASelector;
end;

function TIndexedSelectorAdapter<TSource, TResult>.Project(const AValue: TSource; AIndex: Integer): TResult;
var
  LInput: INyxIndexedValue<TSource>;
begin
  LInput := TNyxIndexedValue<TSource>.Create(AValue, AIndex);
  Result := FSelector.Project(LInput);
end;

class function TNyxIndexedSelector<TSource, TResult>.From(
  AFunction: TNyxSelectorFunction<INyxIndexedValue<TSource>, TResult>): INyxIndexedSelector<TSource, TResult>;
type
  TContext = INyxIndexedValue<TSource>;
begin
  Result := Wrap(TNyxSelector<TContext, TResult>.From(AFunction));
end;

class function TNyxIndexedSelector<TSource, TResult>.Wrap(
  const ASelector: INyxSelector<INyxIndexedValue<TSource>, TResult>): INyxIndexedSelector<TSource, TResult>;
begin

  if ASelector = nil then
  begin
    raise ENyxQuery.Create('Indexed selector requires an owned callback');
  end;
  Result := TIndexedSelectorAdapter<TSource, TResult>.Create(ASelector);
end;

constructor TIndexedStageEnumerator<T>.Create(const ASource: INyxEnumerator<T>;
  const APredicate: INyxIndexedPredicate<T>; AOperation: TIndexedSequenceOperation);
begin
  inherited Create;
  FSource := ASource;
  FPredicate := APredicate;
  FOperation := AOperation;
  FSkipping := AOperation = isoSkipWhile;
end;

procedure TIndexedStageEnumerator<T>.Retire;
begin
  FSource := nil;
  FPredicate := nil;
end;

function TIndexedStageEnumerator<T>.Advance(out AValue: T): Boolean;
var
  LIndex: Integer;
  LAccepted: Boolean;
begin
  try
    if FSource = nil then
    begin
      raise ENyxQuery.Create('Indexed source returned no cursor');
    end;
    while FSource.MoveNext do
    begin
      AValue := FSource.Current;
      { After SkipWhile's boundary no callback/ordinal is needed, even for a
        source larger than Integer range. The prefix contract is already fixed. }

      if (FOperation = isoSkipWhile) and not FSkipping then
      begin
        Exit(True);
      end;
      LIndex := NextNyxQueryIndex(FIndex, FExhausted);
      LAccepted := FPredicate.Test(AValue, LIndex);
      case FOperation of
        isoWhere:
        begin

          if LAccepted then
          begin
            Exit(True);
          end;
        end;
        isoSkipWhile:
        begin
          FSkipping := LAccepted;

          if not FSkipping then
          begin
            FPredicate := nil;
            Exit(True);
          end;
        end;
        isoTakeWhile:
        begin

          if LAccepted then
          begin
            Exit(True);
          end;
          Break;
        end;
      end;
    end;
    Retire;
    Result := False;
  except
    Retire;
    raise;
  end;
end;

constructor TIndexedStageSequence<T>.Create(const ASource: INyxSequence<T>;
  const APredicate: INyxIndexedPredicate<T>; AOperation: TIndexedSequenceOperation);
begin
  inherited Create;
  FSource := ASource;
  FPredicate := APredicate;
  FOperation := AOperation;
end;

function TIndexedStageSequence<T>.GetEnumerator: INyxEnumerator<T>;
begin
  Result := TIndexedStageEnumerator<T>.Create(FSource.GetEnumerator, FPredicate, FOperation);
end;

function TNyxSequence<T>.WhereIndexed(const APredicate: INyxIndexedPredicate<T>): INyxSequence<T>;
begin

  if APredicate = nil then
  begin
    raise ENyxQuery.Create('WhereIndexed requires an owned predicate');
  end;
  Result := TIndexedStageSequence<T>.Create(Self, APredicate, isoWhere);
end;

function TNyxSequence<T>.SkipWhileIndexed(const APredicate: INyxIndexedPredicate<T>): INyxSequence<T>;
begin

  if APredicate = nil then
  begin
    raise ENyxQuery.Create('SkipWhileIndexed requires an owned predicate');
  end;
  Result := TIndexedStageSequence<T>.Create(Self, APredicate, isoSkipWhile);
end;

function TNyxSequence<T>.TakeWhileIndexed(const APredicate: INyxIndexedPredicate<T>): INyxSequence<T>;
begin

  if APredicate = nil then
  begin
    raise ENyxQuery.Create('TakeWhileIndexed requires an owned predicate');
  end;
  Result := TIndexedStageSequence<T>.Create(Self, APredicate, isoTakeWhile);
end;

constructor TIndexedSelectEnumerator<TSource, TResult>.Create(const ASource: INyxEnumerator<TSource>;
  const ASelector: INyxIndexedSelector<TSource, TResult>);
begin
  inherited Create;
  FSource := ASource;
  FSelector := ASelector;
end;

function TIndexedSelectEnumerator<TSource, TResult>.Advance(out AValue: TResult): Boolean;
var
  LIndex: Integer;
  LSourceValue: TSource;
begin
  try
    if FSource = nil then
    begin
      raise ENyxQuery.Create('Indexed source returned no cursor');
    end;
    Result := FSource.MoveNext;

    if Result then
    begin
      LIndex := NextNyxQueryIndex(FIndex, FExhausted);
      LSourceValue := FSource.Current;
      AValue := FSelector.Project(LSourceValue, LIndex);
    end
    else
    begin
      FSource := nil;
      FSelector := nil;
    end;
  except
    FSource := nil;
    FSelector := nil;
    raise;
  end;
end;

constructor TIndexedSelectSequence<TSource, TResult>.Create(const ASource: INyxSequence<TSource>;
  const ASelector: INyxIndexedSelector<TSource, TResult>);
begin
  inherited Create;
  FSource := ASource;
  FSelector := ASelector;
end;

function TIndexedSelectSequence<TSource, TResult>.GetEnumerator: INyxEnumerator<TResult>;
begin
  Result := TIndexedSelectEnumerator<TSource, TResult>.Create(FSource.GetEnumerator, FSelector);
end;

class function TNyxSelectIndexed<TSource, TResult>.From(const ASource: INyxSequence<TSource>;
  const ASelector: INyxIndexedSelector<TSource, TResult>): INyxSequence<TResult>;
begin

  if (ASource = nil) or (ASelector = nil) then
  begin
    raise ENyxQuery.Create('Indexed Select requires a source and owned selector');
  end;
  Result := TIndexedSelectSequence<TSource, TResult>.Create(ASource, ASelector);
end;

function TNyxSequence<T>.SelectIndexed(const ASelector: INyxIndexedSelector<T, T>): INyxSequence<T>;
begin
  Result := TNyxSelectIndexed<T, T>.From(Self, ASelector);
end;
