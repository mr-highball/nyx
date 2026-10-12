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

type
  { Compiler-visible generic implementation dependencies, as with the other
    sequence cursors. Public authoring uses the INyx* recipes and factories. }
  TNyxFunctionComparer<T> = class(TInterfacedObject, INyxComparer<T>)
  private
    FFunction: TNyxCompareFunction<T>;
  public
    constructor Create(AFunction: TNyxCompareFunction<T>);
    function Compare(const ALeft, ARight: T): Integer;
  end;
  TNyxOrdering<T> = class(TInterfacedObject, INyxOrdering<T>)
  public
    function ThenBy(const AOther: INyxOrdering<T>): INyxOrdering<T>;
    function Prepare(const AValues: array of T): INyxOrderKeys; virtual; abstract;
  end;
  TNyxCombinedKeys = class(TInterfacedObject, INyxOrderKeys)
  private
    FPrimary: INyxOrderKeys;
    FSecondary: INyxOrderKeys;
  public
    constructor Create(const APrimary, ASecondary: INyxOrderKeys);
    function Compare(ALeft, ARight: Integer): Integer;
  end;
  TNyxCombinedOrdering<T> = class(TNyxOrdering<T>)
  private
    FPrimary: INyxOrdering<T>;
    FSecondary: INyxOrdering<T>;
  public
    constructor Create(const APrimary, ASecondary: INyxOrdering<T>);
    function Prepare(const AValues: array of T): INyxOrderKeys; override;
  end;
  TNyxCachedKeys<T> = class(TInterfacedObject, INyxOrderKeys)
  private
    FValues: TNyxSequenceArray<T>;
    FComparer: INyxComparer<T>;
    FDirection: TNyxOrderDirection;
  public
    constructor Create(const AValues: array of T; const AComparer: INyxComparer<T>;
      ADirection: TNyxOrderDirection);
    function Compare(ALeft, ARight: Integer): Integer;
  end;
  TNyxValueOrdering<T> = class(TNyxOrdering<T>)
  private
    FComparer: INyxComparer<T>;
    FDirection: TNyxOrderDirection;
  public
    constructor Create(const AComparer: INyxComparer<T>;
      ADirection: TNyxOrderDirection);
    function Prepare(const AValues: array of T): INyxOrderKeys; override;
  end;
  TNyxKeyOrdering<TSource, TKey> = class(TNyxOrdering<TSource>)
  private
    FSelector: INyxSelector<TSource, TKey>;
    FComparer: INyxComparer<TKey>;
    FDirection: TNyxOrderDirection;
  public
    constructor Create(const ASelector: INyxSelector<TSource, TKey>;
      const AComparer: INyxComparer<TKey>; ADirection: TNyxOrderDirection);
    function Prepare(const AValues: array of TSource): INyxOrderKeys; override;
  end;
  TNyxOrderedEnumerator<T> = class(TEnumerator<T>)
  private
    FSource: INyxSequence<T>;
    FOrdering: INyxOrdering<T>;
    FValues: TNyxSequenceArray<T>;
    FRows: array of Integer;
    FIndex: Integer;
    FPrepared: Boolean;
    procedure PrepareRows;
  protected
    function Advance(out AValue: T): Boolean; override;
  public
    constructor Create(const ASource: INyxSequence<T>;
      const AOrdering: INyxOrdering<T>);
  end;
  TNyxOrderedSequence<T> = class(TNyxSequence<T>)
  private
    FSource: INyxSequence<T>;
    FOrdering: INyxOrdering<T>;
  public
    constructor Create(const ASource: INyxSequence<T>;
      const AOrdering: INyxOrdering<T>);
    function GetEnumerator: INyxEnumerator<T>; override;
  end;

  TNyxIntegerComparer = class(TInterfacedObject, INyxComparer<Integer>)
    function Compare(const ALeft, ARight: Integer): Integer;
  end;
  TNyxBooleanComparer = class(TInterfacedObject, INyxComparer<Boolean>)
    function Compare(const ALeft, ARight: Boolean): Integer;
  end;
  TNyxTextComparer = class(TInterfacedObject, INyxComparer<TNyxText>)
  private
    FMode: TNyxTextOrder;
  public
    constructor Create(AMode: TNyxTextOrder);
    function Compare(const ALeft, ARight: TNyxText): Integer;
  end;
  TNyxNumberComparer = class(TInterfacedObject, INyxComparer<Double>)
  private
    FNaN: TNyxNaNOrder;
  public
    constructor Create(ANaN: TNyxNaNOrder);
    function Compare(const ALeft, ARight: Double): Integer;
  end;
