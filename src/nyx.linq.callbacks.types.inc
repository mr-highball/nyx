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
  { Adapters own either an ordinary function, a revocable borrowed nested
    function, or a compiler-managed function reference. No adapter owns a lease;
    retaining a query cannot prolong a caller's nested-frame permission. }
  TNyxFunctionAction<T> = class(TInterfacedObject, INyxAction<T>)
  private
    FFunction: TNyxActionFunction<T>;
  public
    constructor Create(AFunction: TNyxActionFunction<T>);
    procedure Execute(const AValue: T);
  end;
  TNyxNestedPredicate<T> = class(TInterfacedObject, INyxPredicate<T>)
  private
    FFunction: TNyxPredicateNested<T>;
    FLifetime: INyxCallbackLifetime;
  public
    constructor Create(AFunction: TNyxPredicateNested<T>;
      const ALease: INyxCallbackLease);
    function Test(const AValue: T): Boolean;
  end;
  TNyxNestedSelector<TSource, TResult> = class(TInterfacedObject,
    INyxSelector<TSource, TResult>)
  private
    FFunction: TNyxSelectorNested<TSource, TResult>;
    FLifetime: INyxCallbackLifetime;
  public
    constructor Create(AFunction: TNyxSelectorNested<TSource, TResult>;
      const ALease: INyxCallbackLease);
    function Project(const AValue: TSource): TResult;
  end;
  TNyxNestedAction<T> = class(TInterfacedObject, INyxAction<T>)
  private
    FFunction: TNyxActionNested<T>;
    FLifetime: INyxCallbackLifetime;
  public
    constructor Create(AFunction: TNyxActionNested<T>;
      const ALease: INyxCallbackLease);
    procedure Execute(const AValue: T);
  end;
  TNyxNestedComparer<T> = class(TInterfacedObject, INyxComparer<T>)
  private
    FFunction: TNyxCompareNested<T>;
    FLifetime: INyxCallbackLifetime;
  public
    constructor Create(AFunction: TNyxCompareNested<T>;
      const ALease: INyxCallbackLease);
    function Compare(const ALeft, ARight: T): Integer;
  end;

  {$ifdef NYX_HAS_INLINE_CALLBACKS}
  TNyxInlinePredicate<T> = class(TInterfacedObject, INyxPredicate<T>)
  private
    FFunction: TNyxPredicateReference<T>;
    FCapture: INyxCallbackCapture;
  public
    constructor Create(const AFunction: TNyxPredicateReference<T>;
      const ACapture: INyxCallbackCapture);
    function Test(const AValue: T): Boolean;
  end;
  TNyxInlineSelector<TSource, TResult> = class(TInterfacedObject,
    INyxSelector<TSource, TResult>)
  private
    FFunction: TNyxSelectorReference<TSource, TResult>;
    FCapture: INyxCallbackCapture;
  public
    constructor Create(const AFunction: TNyxSelectorReference<TSource, TResult>;
      const ACapture: INyxCallbackCapture);
    function Project(const AValue: TSource): TResult;
  end;
  TNyxInlineAction<T> = class(TInterfacedObject, INyxAction<T>)
  private
    FFunction: TNyxActionReference<T>;
    FCapture: INyxCallbackCapture;
  public
    constructor Create(const AFunction: TNyxActionReference<T>;
      const ACapture: INyxCallbackCapture);
    procedure Execute(const AValue: T);
  end;
  TNyxInlineComparer<T> = class(TInterfacedObject, INyxComparer<T>)
  private
    FFunction: TNyxCompareReference<T>;
    FCapture: INyxCallbackCapture;
  public
    constructor Create(const AFunction: TNyxCompareReference<T>;
      const ACapture: INyxCallbackCapture);
    function Compare(const ALeft, ARight: T): Integer;
  end;
  {$endif}
