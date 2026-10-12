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

constructor TNyxFunctionAction<T>.Create(AFunction: TNyxActionFunction<T>);
begin
  inherited Create;
  FFunction := AFunction;
end;

procedure TNyxFunctionAction<T>.Execute(const AValue: T);
begin
  FFunction(AValue);
end;

class function TNyxAction<T>.From(AFunction: TNyxActionFunction<T>): INyxAction<T>;
begin

  if not Assigned(AFunction) then
  begin
    raise ENyxQuery.Create('ForEach action requires a procedure');
  end;
  Result := TNyxFunctionAction<T>.Create(AFunction);
end;

constructor TNyxNestedPredicate<T>.Create(AFunction: TNyxPredicateNested<T>;
  const ALease: INyxCallbackLease);
begin
  inherited Create;

  if not Assigned(AFunction) or (ALease = nil) then
  begin
    raise ENyxQuery.Create('Nested predicate requires a callback and scope lease');
  end;
  FLifetime := ALease.Lifetime;

  if FLifetime = nil then
  begin
    raise ENyxQuery.Create('Nested callback lease returned no lifetime port');
  end;
  FLifetime.RequireEntry;
  FFunction := AFunction;
end;

function TNyxNestedPredicate<T>.Test(const AValue: T): Boolean;
begin
  FLifetime.RequireEntry;
  Result := FFunction(AValue);
end;

constructor TNyxNestedSelector<TSource, TResult>.Create(
  AFunction: TNyxSelectorNested<TSource, TResult>; const ALease: INyxCallbackLease);
begin
  inherited Create;

  if not Assigned(AFunction) or (ALease = nil) then
  begin
    raise ENyxQuery.Create('Nested selector requires a callback and scope lease');
  end;
  FLifetime := ALease.Lifetime;

  if FLifetime = nil then
  begin
    raise ENyxQuery.Create('Nested callback lease returned no lifetime port');
  end;
  FLifetime.RequireEntry;
  FFunction := AFunction;
end;

function TNyxNestedSelector<TSource, TResult>.Project(const AValue: TSource): TResult;
begin
  FLifetime.RequireEntry;
  Result := FFunction(AValue);
end;

constructor TNyxNestedAction<T>.Create(AFunction: TNyxActionNested<T>;
  const ALease: INyxCallbackLease);
begin
  inherited Create;

  if not Assigned(AFunction) or (ALease = nil) then
  begin
    raise ENyxQuery.Create('Nested action requires a callback and scope lease');
  end;
  FLifetime := ALease.Lifetime;

  if FLifetime = nil then
  begin
    raise ENyxQuery.Create('Nested callback lease returned no lifetime port');
  end;
  FLifetime.RequireEntry;
  FFunction := AFunction;
end;

procedure TNyxNestedAction<T>.Execute(const AValue: T);
begin
  FLifetime.RequireEntry;
  FFunction(AValue);
end;

class function TNyxPredicate<T>.FromNested(AFunction: TNyxPredicateNested<T>;
  const ALease: INyxCallbackLease): INyxPredicate<T>;
begin
  Result := TNyxNestedPredicate<T>.Create(AFunction, ALease);
end;

class function TNyxSelector<TSource, TResult>.FromNested(
  AFunction: TNyxSelectorNested<TSource, TResult>;
  const ALease: INyxCallbackLease): INyxSelector<TSource, TResult>;
begin
  Result := TNyxNestedSelector<TSource, TResult>.Create(AFunction, ALease);
end;

class function TNyxAction<T>.FromNested(AFunction: TNyxActionNested<T>;
  const ALease: INyxCallbackLease): INyxAction<T>;
begin
  Result := TNyxNestedAction<T>.Create(AFunction, ALease);
end;

constructor TNyxNestedComparer<T>.Create(AFunction: TNyxCompareNested<T>;
  const ALease: INyxCallbackLease);
begin
  inherited Create;

  if not Assigned(AFunction) or (ALease = nil) then
  begin
    raise ENyxQuery.Create('Nested comparer requires a callback and scope lease');
  end;
  FLifetime := ALease.Lifetime;

  if FLifetime = nil then
  begin
    raise ENyxQuery.Create('Nested callback lease returned no lifetime port');
  end;
  FLifetime.RequireEntry;
  FFunction := AFunction;
end;

function TNyxNestedComparer<T>.Compare(const ALeft, ARight: T): Integer;
begin
  FLifetime.RequireEntry;
  Result := FFunction(ALeft, ARight);
end;

class function TNyxComparer<T>.FromNested(AFunction: TNyxCompareNested<T>;
  const ALease: INyxCallbackLease): INyxComparer<T>;
begin
  Result := TNyxNestedComparer<T>.Create(AFunction, ALease);
end;

procedure TNyxSequence<T>.ForEach(const AAction: INyxAction<T>);
var
  LCursor: INyxEnumerator<T>;
begin

  if AAction = nil then
  begin
    raise ENyxQuery.Create('ForEach requires an owned action');
  end;
  LCursor := GetEnumerator;
  while LCursor.MoveNext do
  begin
    AAction.Execute(LCursor.Current);
  end;
end;

{$ifdef NYX_HAS_INLINE_CALLBACKS}
constructor TNyxInlinePredicate<T>.Create(const AFunction: TNyxPredicateReference<T>;
  const ACapture: INyxCallbackCapture);
begin
  inherited Create;

  if not Assigned(AFunction) then
  begin
    raise ENyxQuery.Create('Inline predicate requires a function reference');
  end;
  FFunction := AFunction;
  FCapture := ACapture;
end;

function TNyxInlinePredicate<T>.Test(const AValue: T): Boolean;
begin
  Result := FFunction(AValue);
end;

constructor TNyxInlineSelector<TSource, TResult>.Create(
  const AFunction: TNyxSelectorReference<TSource, TResult>;
  const ACapture: INyxCallbackCapture);
begin
  inherited Create;

  if not Assigned(AFunction) then
  begin
    raise ENyxQuery.Create('Inline selector requires a function reference');
  end;
  FFunction := AFunction;
  FCapture := ACapture;
end;

function TNyxInlineSelector<TSource, TResult>.Project(const AValue: TSource): TResult;
begin
  Result := FFunction(AValue);
end;

constructor TNyxInlineAction<T>.Create(const AFunction: TNyxActionReference<T>;
  const ACapture: INyxCallbackCapture);
begin
  inherited Create;

  if not Assigned(AFunction) then
  begin
    raise ENyxQuery.Create('Inline action requires a procedure reference');
  end;
  FFunction := AFunction;
  FCapture := ACapture;
end;

constructor TNyxInlineComparer<T>.Create(const AFunction: TNyxCompareReference<T>;
  const ACapture: INyxCallbackCapture);
begin
  inherited Create;

  if not Assigned(AFunction) then
  begin
    raise ENyxQuery.Create('Inline comparer requires a function reference');
  end;
  FFunction := AFunction;
  FCapture := ACapture;
end;

function TNyxInlineComparer<T>.Compare(const ALeft, ARight: T): Integer;
begin
  Result := FFunction(ALeft, ARight);
end;

class function TNyxComparer<T>.FromInline(
  const AFunction: TNyxCompareReference<T>;
  const ACapture: INyxCallbackCapture): INyxComparer<T>;
begin
  Result := TNyxInlineComparer<T>.Create(AFunction, ACapture);
end;

procedure TNyxInlineAction<T>.Execute(const AValue: T);
begin
  FFunction(AValue);
end;

class function TNyxPredicate<T>.FromInline(
  const AFunction: TNyxPredicateReference<T>;
  const ACapture: INyxCallbackCapture): INyxPredicate<T>;
begin
  Result := TNyxInlinePredicate<T>.Create(AFunction, ACapture);
end;

class function TNyxSelector<TSource, TResult>.FromInline(
  const AFunction: TNyxSelectorReference<TSource, TResult>;
  const ACapture: INyxCallbackCapture): INyxSelector<TSource, TResult>;
begin
  Result := TNyxInlineSelector<TSource, TResult>.Create(AFunction, ACapture);
end;

class function TNyxAction<T>.FromInline(
  const AFunction: TNyxActionReference<T>;
  const ACapture: INyxCallbackCapture): INyxAction<T>;
begin
  Result := TNyxInlineAction<T>.Create(AFunction, ACapture);
end;

function TNyxSequence<T>.Where(
  const APredicate: TNyxPredicateReference<T>;
  const ACapture: INyxCallbackCapture): INyxSequence<T>;
begin
  Result := Where(TNyxPredicate<T>.FromInline(APredicate, ACapture));
end;

function TNyxSequence<T>.Select(
  const ASelector: TNyxSelectorReference<T, T>;
  const ACapture: INyxCallbackCapture): INyxSequence<T>;
begin
  Result := Select(TNyxSelector<T, T>.FromInline(ASelector, ACapture));
end;

procedure TNyxSequence<T>.ForEach(const AAction: TNyxActionReference<T>;
  const ACapture: INyxCallbackCapture);
begin
  ForEach(TNyxAction<T>.FromInline(AAction, ACapture));
end;
{$endif}
