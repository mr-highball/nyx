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

  { Order direction and text/NaN policies are closed Pascal choices. Text uses
    Unicode scalar order, independent of UTF-8/UTF-16 storage and host locale.
    ASCII-insensitive deliberately folds only A..Z; it is not Unicode casefold.
    Number comparison accepts infinities/signed zero and explicitly orders or
    refuses NaN, without subtracting floating-point values. }
  TNyxOrderDirection = (nodAscending, nodDescending);
  TNyxTextOrder = (ntoOrdinal, ntoAsciiInsensitive);
  TNyxNaNOrder = (nnoFirst, nnoLast, nnoRefuse);

  { Owned pure comparer. Any negative/zero/positive Integer is accepted; Nyx
    normalizes the sign before descending order so Low(Integer) is safe.
    A caller must provide a consistent ordering and its callback lifetime. }
  INyxComparer<T> = interface(IInterface)
    function Compare(const ALeft, ARight: T): Integer;
  end;
  TNyxCompareFunction<T> = function(const ALeft, ARight: T): Integer;
  TNyxCompareNested<T> = function(const ALeft, ARight: T): Integer
    {$ifndef PAS2JS}is nested{$endif};
  {$ifdef NYX_HAS_INLINE_CALLBACKS}
  TNyxCompareReference<T> = reference to function(const ALeft, ARight: T): Integer;
  {$endif}
  TNyxComparer<T> = class
  public
    class function From(AFunction: TNyxCompareFunction<T>): INyxComparer<T>; static;
    class function FromNested(AFunction: TNyxCompareNested<T>;
      const ALease: INyxCallbackLease): INyxComparer<T>; static;
    {$ifdef NYX_HAS_INLINE_CALLBACKS}
    class function FromInline(const AFunction: TNyxCompareReference<T>;
      const ACapture: INyxCallbackCapture = nil): INyxComparer<T>; static;
    {$endif}
  end;

  { An independently prepared immutable key set. Indexes refer to the exact
    captured input array. It retains keys/comparers, never its recipe, source
    query, UI or store. Out-of-range indexes refuse before reading a key. }
  INyxOrderKeys = interface(IInterface)
    ['{2F1B74F2-5E68-4B4E-9C51-923F917659BD}']
    function Compare(ALeft, ARight: Integer): Integer;
  end;

  { Immutable fluent recipe. ThenBy returns a separately owned composition;
    earlier recipes never acquire later keys. Prepare is the extension boundary
    used once per enumeration: key selectors run once per input/recipe key.
    They must not mutate the query input. Nil prepared keys are refused. }
  INyxOrdering<T> = interface(IInterface)
    function ThenBy(const AOther: INyxOrdering<T>): INyxOrdering<T>;
    function Prepare(const AValues: array of T): INyxOrderKeys;
  end;

  { Compare whole values, or project a strongly typed key first. By constructs
    a recipe only; no selector/comparer executes before enumeration begins. }
  TNyxValueOrder<T> = class
  public
    class function By(const AComparer: INyxComparer<T>;
      ADirection: TNyxOrderDirection = nodAscending): INyxOrdering<T>; static;
  end;
  TNyxKeyOrder<TSource, TKey> = class
  public
    class function By(const ASelector: INyxSelector<TSource, TKey>;
      const AComparer: INyxComparer<TKey>;
      ADirection: TNyxOrderDirection = nodAscending): INyxOrdering<TSource>; static;
  end;

{ Immutable built-in comparer factories. Integer never subtracts; False precedes
  True. Text refuses malformed Unicode when compared and preserves NUL/exact
  scalar values. Custom comparers can supply other documented application rules. }
function NewNyxIntegerComparer: INyxComparer<Integer>;
function NewNyxBooleanComparer: INyxComparer<Boolean>;
function NewNyxTextComparer(AMode: TNyxTextOrder = ntoOrdinal): INyxComparer<TNyxText>;
function NewNyxNumberComparer(ANaN: TNyxNaNOrder = nnoFirst): INyxComparer<Double>;

type
