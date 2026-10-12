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
  { Public adapter factories retain an ordinary owned callback over the typed
    context. From supplies global functions; Wrap also accepts the existing
    scoped nested or compiler-selected inline adapters, preserving their leases,
    origin-thread/capture behavior rather than introducing another lifetime model. }
  TNyxIndexedPredicate<T> = class
  public
    class function From(AFunction: TNyxPredicateFunction<INyxIndexedValue<T>>): INyxIndexedPredicate<T>; static;
    class function Wrap(const APredicate: INyxPredicate<INyxIndexedValue<T>>): INyxIndexedPredicate<T>; static;
  end;
  TNyxIndexedSelector<TSource, TResult> = class
  public
    class function From(AFunction: TNyxSelectorFunction<INyxIndexedValue<TSource>, TResult>):
      INyxIndexedSelector<TSource, TResult>; static;
    class function Wrap(const ASelector: INyxSelector<INyxIndexedValue<TSource>, TResult>):
      INyxIndexedSelector<TSource, TResult>; static;
  end;
  TNyxSelectIndexed<TSource, TResult> = class
  public
    { Type-changing deferred projection. GetEnumerator owns its own cursor and
      ordinal state; construction and obtaining a cursor invoke no projector. }
    class function From(const ASource: INyxSequence<TSource>;
      const ASelector: INyxIndexedSelector<TSource, TResult>): INyxSequence<TResult>; static;
  end;

  { Compiler-visible implementation dependencies for native generic expansion. }
  TIndexedSequenceOperation = (isoWhere, isoSkipWhile, isoTakeWhile);
  TIndexedPredicateAdapter<T> = class(TInterfacedObject, INyxIndexedPredicate<T>)
  private
    FPredicate: INyxPredicate<INyxIndexedValue<T>>;
  public
    constructor Create(const APredicate: INyxPredicate<INyxIndexedValue<T>>);
    function Test(const AValue: T; AIndex: Integer): Boolean;
  end;
  TIndexedSelectorAdapter<TSource, TResult> = class(TInterfacedObject,
    INyxIndexedSelector<TSource, TResult>)
  private
    FSelector: INyxSelector<INyxIndexedValue<TSource>, TResult>;
  public
    constructor Create(const ASelector: INyxSelector<INyxIndexedValue<TSource>, TResult>);
    function Project(const AValue: TSource; AIndex: Integer): TResult;
  end;
  TIndexedStageEnumerator<T> = class(TEnumerator<T>)
  private
    FSource: INyxEnumerator<T>;
    FPredicate: INyxIndexedPredicate<T>;
    FOperation: TIndexedSequenceOperation;
    FIndex: Integer;
    FExhausted: Boolean;
    FSkipping: Boolean;
    procedure Retire;
  protected
    function Advance(out AValue: T): Boolean; override;
  public
    constructor Create(const ASource: INyxEnumerator<T>;
      const APredicate: INyxIndexedPredicate<T>; AOperation: TIndexedSequenceOperation);
  end;
  TIndexedStageSequence<T> = class(TNyxSequence<T>)
  private
    FSource: INyxSequence<T>;
    FPredicate: INyxIndexedPredicate<T>;
    FOperation: TIndexedSequenceOperation;
  public
    constructor Create(const ASource: INyxSequence<T>;
      const APredicate: INyxIndexedPredicate<T>; AOperation: TIndexedSequenceOperation);
    function GetEnumerator: INyxEnumerator<T>; override;
  end;
  TIndexedSelectEnumerator<TSource, TResult> = class(TEnumerator<TResult>)
  private
    FSource: INyxEnumerator<TSource>;
    FSelector: INyxIndexedSelector<TSource, TResult>;
    FIndex: Integer;
    FExhausted: Boolean;
  protected
    function Advance(out AValue: TResult): Boolean; override;
  public
    constructor Create(const ASource: INyxEnumerator<TSource>;
      const ASelector: INyxIndexedSelector<TSource, TResult>);
  end;
  TIndexedSelectSequence<TSource, TResult> = class(TNyxSequence<TResult>)
  private
    FSource: INyxSequence<TSource>;
    FSelector: INyxIndexedSelector<TSource, TResult>;
  public
    constructor Create(const ASource: INyxSequence<TSource>;
      const ASelector: INyxIndexedSelector<TSource, TResult>);
    function GetEnumerator: INyxEnumerator<TResult>; override;
  end;
