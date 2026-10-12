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

function TNyxSequence<T>.Order(const AComparer: INyxComparer<T>;
  ADirection: TNyxOrderDirection): INyxSequence<T>;
var
  LRecipe: INyxOrdering<T>;
begin
  LRecipe := TNyxValueOrder<T>.By(AComparer, ADirection);
  Result := OrderBy(LRecipe);
end;

function TNyxSequence<T>.OrderDescending(const AComparer: INyxComparer<T>): INyxSequence<T>;
begin
  Result := Order(AComparer, nodDescending);
end;

function TNyxSequence<T>.Count(const APredicate: INyxPredicate<T>): Integer;
var
  LFiltered: INyxSequence<T>;
begin
  LFiltered := Where(APredicate);
  Result := LFiltered.Count;
end;

function TNyxSequence<T>.First(const APredicate: INyxPredicate<T>): T;
var
  LFiltered: INyxSequence<T>;
begin
  LFiltered := Where(APredicate);
  Result := LFiltered.First;
end;

function TNyxSequence<T>.Last(const APredicate: INyxPredicate<T>): T;
var
  LFiltered: INyxSequence<T>;
begin
  LFiltered := Where(APredicate);
  Result := LFiltered.Last;
end;

function TNyxSequence<T>.Single(const APredicate: INyxPredicate<T>): T;
var
  LFiltered: INyxSequence<T>;
begin
  LFiltered := Where(APredicate);
  Result := LFiltered.Single;
end;

function TNyxSequence<T>.TryFirst(const APredicate: INyxPredicate<T>; out AValue: T): Boolean;
var
  LFiltered: INyxSequence<T>;
begin
  LFiltered := Where(APredicate);
  Result := LFiltered.TryFirst(AValue);
end;

function TNyxSequence<T>.FirstOrDefaultWhere(const APredicate: INyxPredicate<T>): T;
begin
  Result := FirstOrDefaultWhere(APredicate, FEmptyValue);
end;

function TNyxSequence<T>.FirstOrDefaultWhere(const APredicate: INyxPredicate<T>;
  const AFallback: T): T;
begin

  if not TryFirst(APredicate, Result) then
  begin
    Result := AFallback;
  end;
end;

function TNyxSequence<T>.TryLast(const APredicate: INyxPredicate<T>; out AValue: T): Boolean;
var
  LFiltered: INyxSequence<T>;
begin
  LFiltered := Where(APredicate);
  Result := LFiltered.TryLast(AValue);
end;

function TNyxSequence<T>.LastOrDefaultWhere(const APredicate: INyxPredicate<T>): T;
begin
  Result := LastOrDefaultWhere(APredicate, FEmptyValue);
end;

function TNyxSequence<T>.LastOrDefaultWhere(const APredicate: INyxPredicate<T>;
  const AFallback: T): T;
begin

  if not TryLast(APredicate, Result) then
  begin
    Result := AFallback;
  end;
end;

function TNyxSequence<T>.TrySingle(const APredicate: INyxPredicate<T>; out AValue: T): Boolean;
var
  LFiltered: INyxSequence<T>;
begin
  LFiltered := Where(APredicate);
  Result := LFiltered.TrySingle(AValue);
end;

function TNyxSequence<T>.SingleOrDefaultWhere(const APredicate: INyxPredicate<T>): T;
begin
  Result := SingleOrDefaultWhere(APredicate, FEmptyValue);
end;

function TNyxSequence<T>.SingleOrDefaultWhere(const APredicate: INyxPredicate<T>;
  const AFallback: T): T;
begin

  if not TrySingle(APredicate, Result) then
  begin
    Result := AFallback;
  end;
end;
