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
unit nyx.linq;

{$mode delphi}{$H+}{$codepage utf8}
{$modeswitch typehelpers}
{$include nyx.callback.features.inc}

interface

uses
  SysUtils,
  nyx.text,
  nyx.callback.scope;

const
  { Compiler-selected syntax capability. Runtime query ownership still requires
    actual qualification, especially on development compiler snapshots. }
  NyxHasInlineCallbacks = {$ifdef NYX_HAS_INLINE_CALLBACKS}True{$else}False{$endif};
  NyxInlineRetainsInterfaces = NyxHasInlineCallbacks and
    {$ifdef PAS2JS}False{$else}True{$endif};

type
  { Query errors distinguish invalid arguments, absent elements and cardinality
    failures from exceptions raised by a caller's predicate/projector. Caller
    exceptions retain their original type and message. }
  ENyxQuery = class(Exception);
  ENyxQueryEmpty = class(ENyxQuery);
  ENyxQueryCardinality = class(ENyxQuery);
  TNyxSequenceArray<T> = array of T;
  { Exact nonnegative signed-64-domain count on both targets. Two Cardinal limbs
    avoid pas2js Int64 limitations and unsafe number arithmetic above 2^53.
    Default record is zero; increment refuses 2^63-1 overflow. }
  TNyxQueryCount = record
  private
    FHigh: Cardinal;
    FLow: Cardinal;
  public
    { Return a detached count plus one; maximum-domain overflow raises ENyxQuery. }
    function Incremented: TNyxQueryCount;
    { Exact detached sum, including carries above the browser safe-integer range.
      Signed-64-domain overflow refuses without changing either input. }
    function Added(const AValue: TNyxQueryCount): TNyxQueryCount;
    { Format exact decimal digits without locale or unsafe browser Int64 maths. }
    function ToText: TNyxText;
    { Narrow only an exactly representable Integer; absent output becomes zero. }
    function TryInteger(out AValue: Integer): Boolean;
    property HighPart: Cardinal read FHigh;
    property LowPart: Cardinal read FLow;
  end;
  { Optional non-enumerating count capability. Unknown adapters need not implement
    it; querying this interface must enter no source or executable callback. }
  INyxSequenceCount = interface(IInterface)
    ['{414D25C0-458C-461F-9697-C793B781EFB5}']
    function NonEnumeratedCount(out AValue: TNyxQueryCount): Boolean;
  end;

  { An enumerator owns its inputs independently of the sequence that created it.
    Current is valid only after a successful MoveNext and before the next call.
    Completion and failure are terminal. Each enumeration has its own cursor;
    retained predicates/projectors may themselves contain caller-owned state.
    Enumerators are single-consumer objects, not shared worker cursors. }
  INyxEnumerator<T> = interface(IInterface)
    function MoveNext: Boolean;
    function GetCurrent: T;
    property Current: T read GetCurrent;
  end;

  { Managed callback implementations own their state; scoped nested adapters
    borrow a frame behind a revocable entry port. A sequence retains these
    interfaces until release.
    Test/Project execute only during enumeration; repeatable pure callbacks are
    recommended. No query method authorizes mutation of UI or live collections. }
  INyxPredicate<T> = interface(IInterface)
    function Test(const AValue: T): Boolean;
  end;
  INyxSelector<TSource, TResult> = interface(IInterface)
    function Project(const AValue: TSource): TResult;
  end;
  INyxAction<T> = interface(IInterface)
    procedure Execute(const AValue: T);
  end;

  { Stable owned equality shared by sets, membership and grouping. Equal is an
    equivalence relation; equal keys MUST have equal Cardinal hashes. Collisions
    compare typed keys. Keys/callback behavior remain stable for a retained index;
    arbitrary mutable objects are neither frozen nor synchronized. Generic string
    conversion, locale and RTTI never silently supply equality. }
  INyxEquality<T> = interface(IInterface)
    function Equal(const ALeft, ARight: T): Boolean;
    function Hash(const AValue: T): Cardinal;
  end;

  { Indexed callbacks receive an owned immutable value/ordinal context. The ordinal is
    zero-based in THIS operator's input, including values it subsequently filters.
    Standard owned/nested/inline adapters can be reused for these interfaces.
    The managed context may be retained; its value follows ordinary Pascal
    ownership. Browser COM interfaces cannot be fields of Pascal records. }
  INyxIndexedValue<T> = interface(IInterface)
    function GetValue: T;
    function GetIndex: Integer;
    property Value: T read GetValue;
    property Index: Integer read GetIndex;
  end;
  TNyxIndexedValue<T> = class(TInterfacedObject, INyxIndexedValue<T>)
  private
    FValue: T;
    FIndex: Integer;
  public
    constructor Create(const AValue: T; AIndex: Integer);
    function GetValue: T;
    function GetIndex: Integer;
  end;
  INyxIndexedPredicate<T> = interface(IInterface)
    function Test(const AValue: T; AIndex: Integer): Boolean;
  end;
  INyxIndexedSelector<TSource, TResult> = interface(IInterface)
    function Project(const AValue: TSource; AIndex: Integer): TResult;
  end;

  TNyxActionFunction<T> = procedure(const AValue: T);
  { Named nested variables use native nestedprocvars and ordinary browser
    function values. FromNested requires a lease closed before the enclosing
    routine returns, and entry stays on the originating native thread.
    Inline references retain scalar capture state. Interface captures use
    NyxCapture explicitly for portable Pascal reference-counted ownership. }
  TNyxPredicateNested<T> = function(const AValue: T): Boolean
    {$ifndef PAS2JS}is nested{$endif};
  TNyxSelectorNested<TSource, TResult> = function(const AValue: TSource): TResult
    {$ifndef PAS2JS}is nested{$endif};
  TNyxActionNested<T> = procedure(const AValue: T) {$ifndef PAS2JS}is nested{$endif};
  {$ifdef NYX_HAS_INLINE_CALLBACKS}
  TNyxPredicateReference<T> = reference to function(const AValue: T): Boolean;
  TNyxSelectorReference<TSource, TResult> = reference to function(const AValue: TSource): TResult;
  TNyxActionReference<T> = reference to procedure(const AValue: T);
  {$endif}

  { Convenience From adapters for ordinary global Pascal functions. These signatures
    accept neither borrowed object methods nor nested stack closures. State-bearing
    callbacks implement the owned interfaces above. Parallel safety is a separate
    caller contract, not implied by using a function adapter. }
  TNyxPredicateFunction<T> = function(const AValue: T): Boolean;
  TNyxSelectorFunction<TSource, TResult> = function(const AValue: TSource): TResult;
  TNyxPredicate<T> = class
  public
    class function From(AFunction: TNyxPredicateFunction<T>): INyxPredicate<T>; static;
    class function FromNested(AFunction: TNyxPredicateNested<T>;
      const ALease: INyxCallbackLease): INyxPredicate<T>; static;
    {$ifdef NYX_HAS_INLINE_CALLBACKS}
    class function FromInline(const AFunction: TNyxPredicateReference<T>;
      const ACapture: INyxCallbackCapture = nil): INyxPredicate<T>; static;
    {$endif}
  end;
  TNyxSelector<TSource, TResult> = class
  public
    class function From(AFunction: TNyxSelectorFunction<TSource, TResult>):
      INyxSelector<TSource, TResult>; static;
    class function FromNested(AFunction: TNyxSelectorNested<TSource, TResult>;
      const ALease: INyxCallbackLease): INyxSelector<TSource, TResult>; static;
    {$ifdef NYX_HAS_INLINE_CALLBACKS}
    class function FromInline(const AFunction: TNyxSelectorReference<TSource, TResult>;
      const ACapture: INyxCallbackCapture = nil):
      INyxSelector<TSource, TResult>; static;
    {$endif}
  end;
  { ForEach is synchronous. Its callback can perform caller-authorized work on
    the caller's thread; neither ForEach nor any adapter schedules UI/worker work. }
  TNyxAction<T> = class
  public
    class function From(AFunction: TNyxActionFunction<T>): INyxAction<T>; static;
    class function FromNested(AFunction: TNyxActionNested<T>;
      const ALease: INyxCallbackLease): INyxAction<T>; static;
    {$ifdef NYX_HAS_INLINE_CALLBACKS}
    class function FromInline(const AFunction: TNyxActionReference<T>;
      const ACapture: INyxCallbackCapture = nil): INyxAction<T>; static;
    {$endif}
  end;

  { Immutable ordering recipes and typed comparers. These independent contracts
    avoid a circular generic sequence/ordered-sequence hierarchy on FPC. }
  {$include nyx.linq.order.contract.inc}

  { Deferred, repeatable sequence contract. Fluent operators own their upstream
    sequence without evaluating it. Where composes with prior filters, rather
    than replacing them. Skip/Take reject negative counts; Take(0) never enters
    its source. Concatenation retains both inputs and their order.
    Terminal methods enumerate once, short-circuit where possible, and release
    their cursor even when caller code raises. Count refuses Integer overflow.
    ToArray returns a detached vector; object/interface elements retain normal
    Pascal reference semantics, never an implicit deep clone of objects.
    Managed interface elements are retained; raw class instances are borrowed
    and must outlive every query/cursor that can read them. Inner mutable arrays
    in arbitrary caller records also retain normal Pascal sharing semantics.
    First/Last/Single raise ENyxQueryEmpty on absence; Single/TrySingle raise
    ENyxQueryCardinality for multiple elements. Try* clears absent output to
    the type's ordinary initialized value. ElementAt is zero-based and refuses
    negative/out-of-range positions. All on an empty sequence returns True. }
  INyxSequence<T> = interface(IInterface)
    function GetEnumerator: INyxEnumerator<T>;
    function Where(const APredicate: INyxPredicate<T>): INyxSequence<T>; overload;
    {$ifdef NYX_HAS_INLINE_CALLBACKS}
    function Where(const APredicate: TNyxPredicateReference<T>;
      const ACapture: INyxCallbackCapture = nil): INyxSequence<T>; overload;
    {$endif}
    function Skip(ACount: Integer): INyxSequence<T>;
    function Take(ACount: Integer): INyxSequence<T>;
    function SkipWhile(const APredicate: INyxPredicate<T>): INyxSequence<T>;
    function TakeWhile(const APredicate: INyxPredicate<T>): INyxSequence<T>;
    { Nonnegative tail counts; zero TakeLast does not enter the source. Storage
      grows with encountered values up to the requested count. Reverse admits
      the complete source before yielding. Each cursor owns independent storage. }
    function SkipLast(ACount: Integer): INyxSequence<T>;
    function TakeLast(ACount: Integer): INyxSequence<T>;
    function Reverse: INyxSequence<T>;
    { Typed values retain ordinary ownership; Prepend yields before source entry.
      DefaultIfEmpty yields its fallback only when the source is actually empty. }
    function Append(const AValue: T): INyxSequence<T>;
    function Prepend(const AValue: T): INyxSequence<T>;
    function DefaultIfEmpty: INyxSequence<T>; overload;
    function DefaultIfEmpty(const AFallback: T): INyxSequence<T>; overload;
    { Independent zero-based ordinals; repeated filters reset their own input
      ordinals. Prefix callbacks stop once their boundary has been reached. }
    function WhereIndexed(const APredicate: INyxIndexedPredicate<T>): INyxSequence<T>;
    function SkipWhileIndexed(const APredicate: INyxIndexedPredicate<T>): INyxSequence<T>;
    function TakeWhileIndexed(const APredicate: INyxIndexedPredicate<T>): INyxSequence<T>;
    function SelectIndexed(const ASelector: INyxIndexedSelector<T, T>): INyxSequence<T>;
    function Select(const ASelector: INyxSelector<T, T>): INyxSequence<T>; overload;
    {$ifdef NYX_HAS_INLINE_CALLBACKS}
    function Select(const ASelector: TNyxSelectorReference<T, T>;
      const ACapture: INyxCallbackCapture = nil): INyxSequence<T>; overload;
    {$endif}
    function Concat(const AOther: INyxSequence<T>): INyxSequence<T>;
    { Streaming first-occurrence set order with independent per-cursor indexes.
      Intersect/Except admit the other input before yielding a nonempty left
      input. All dependencies refuse nil at construction; Take(0) enters none. }
    function Distinct(const AEquality: INyxEquality<T>): INyxSequence<T>;
    function Union(const AOther: INyxSequence<T>;
      const AEquality: INyxEquality<T>): INyxSequence<T>;
    function Intersect(const AOther: INyxSequence<T>;
      const AEquality: INyxEquality<T>): INyxSequence<T>;
    function ExceptWith(const AOther: INyxSequence<T>;
      const AEquality: INyxEquality<T>): INyxSequence<T>;
    { Immediate short-circuit comparisons use Equal, without hashing or preparing
      indexes. SequenceEqual compares order and multiplicity; two empty inputs
      are equal. Caller equality/source failures propagate unchanged. }
    function Contains(const AValue: T; const AEquality: INyxEquality<T>): Boolean;
    function SequenceEqual(const AOther: INyxSequence<T>;
      const AEquality: INyxEquality<T>): Boolean;
    { Stable deferred ordering materializes this input on the first movement,
      caches projected keys once and retains source order for equal keys.
      ThenBy belongs to the typed ordering recipe; each cursor owns its data. }
    function OrderBy(const AOrdering: INyxOrdering<T>): INyxSequence<T>;
    { Stable whole-value ordering uses the existing owned recipe contract. }
    function Order(const AComparer: INyxComparer<T>;
      ADirection: TNyxOrderDirection = nodAscending): INyxSequence<T>;
    function OrderDescending(const AComparer: INyxComparer<T>): INyxSequence<T>;
    { Predicate terminals preserve short-circuit/cardinality rules, callback
      leases/captures and original failures. Nil predicates refuse. }
    function Count: Integer; overload;
    function Count(const APredicate: INyxPredicate<T>): Integer; overload;
    function Any: Boolean; overload;
    function Any(const APredicate: INyxPredicate<T>): Boolean; overload;
    function All(const APredicate: INyxPredicate<T>): Boolean;
    function First: T; overload;
    function First(const APredicate: INyxPredicate<T>): T; overload;
    function TryFirst(out AValue: T): Boolean; overload;
    function TryFirst(const APredicate: INyxPredicate<T>; out AValue: T): Boolean; overload;
    function Last: T; overload;
    function Last(const APredicate: INyxPredicate<T>): T; overload;
    function TryLast(out AValue: T): Boolean; overload;
    function TryLast(const APredicate: INyxPredicate<T>; out AValue: T): Boolean; overload;
    function Single: T; overload;
    function Single(const APredicate: INyxPredicate<T>): T; overload;
    function TrySingle(out AValue: T): Boolean; overload;
    function TrySingle(const APredicate: INyxPredicate<T>; out AValue: T): Boolean; overload;
    function ElementAt(AIndex: Integer): T;
    { Immediate typed fallbacks. Single still refuses multiple elements.
      TryElementAt/ElementAtOrDefault regard negative positions as absent and
      enter no source. No implicit browser Default(T) or untyped sentinel. }
    function TryElementAt(AIndex: Integer; out AValue: T): Boolean;
    function FirstOrDefault: T; overload;
    function FirstOrDefault(const AFallback: T): T; overload;
    function LastOrDefault: T; overload;
    function LastOrDefault(const AFallback: T): T; overload;
    function SingleOrDefault: T; overload;
    function SingleOrDefault(const AFallback: T): T; overload;
    { Named Where forms distinguish predicates from interface-valued fallbacks.
      Defaults are ordinary initialized T; Single still refuses multiple matches. }
    function FirstOrDefaultWhere(const APredicate: INyxPredicate<T>): T; overload;
    function FirstOrDefaultWhere(const APredicate: INyxPredicate<T>; const AFallback: T): T; overload;
    function LastOrDefaultWhere(const APredicate: INyxPredicate<T>): T; overload;
    function LastOrDefaultWhere(const APredicate: INyxPredicate<T>; const AFallback: T): T; overload;
    function SingleOrDefaultWhere(const APredicate: INyxPredicate<T>): T; overload;
    function SingleOrDefaultWhere(const APredicate: INyxPredicate<T>; const AFallback: T): T; overload;
    function ElementAtOrDefault(AIndex: Integer): T; overload;
    function ElementAtOrDefault(AIndex: Integer; const AFallback: T): T; overload;
    function ToArray: TNyxSequenceArray<T>;
    procedure ForEach(const AAction: INyxAction<T>); overload;
    {$ifdef NYX_HAS_INLINE_CALLBACKS}
    procedure ForEach(const AAction: TNyxActionReference<T>;
      const ACapture: INyxCallbackCapture = nil); overload;
    {$endif}
  end;

  { Public extension base: custom sequences implement only GetEnumerator.
    From copies the caller's outer array immediately. It preserves exact values
    and permits independent enumeration after the caller changes/releases its
    array. All later operators remain deferred. Empty uses the same contract. }
  TNyxSequence<T> = class(TInterfacedObject, INyxSequence<T>)
  private
    { Use the type's ordinary initialized object field for absent outputs.
      pas2js Default(Integer) emits Low(Integer), unlike native Default(Integer).
      Object-field initialization preserves zero/empty managed values on both
      targets without pointer tricks, untyped casts or global codepage changes. }
    FEmptyValue: T;
  public
    class function From(const AValues: array of T): INyxSequence<T>; static;
    class function Empty: INyxSequence<T>; static;
    { Count is nonnegative and admission is immediate; values/callbacks are
      captured but enumeration is deferred. Iterate emits seed first and invokes
      the owned next selector only between results, never after the last one. }
    class function RepeatValue(const AValue: T; ACount: Integer): INyxSequence<T>; static;
    class function Iterate(const ASeed: T; ACount: Integer;
      const ANext: INyxSelector<T, T>): INyxSequence<T>; static;
    function GetEnumerator: INyxEnumerator<T>; virtual; abstract;
    function Where(const APredicate: INyxPredicate<T>): INyxSequence<T>; overload;
    {$ifdef NYX_HAS_INLINE_CALLBACKS}
    function Where(const APredicate: TNyxPredicateReference<T>;
      const ACapture: INyxCallbackCapture = nil): INyxSequence<T>; overload;
    {$endif}
    function Skip(ACount: Integer): INyxSequence<T>;
    function Take(ACount: Integer): INyxSequence<T>;
    function SkipWhile(const APredicate: INyxPredicate<T>): INyxSequence<T>;
    function TakeWhile(const APredicate: INyxPredicate<T>): INyxSequence<T>;
    function SkipLast(ACount: Integer): INyxSequence<T>;
    function TakeLast(ACount: Integer): INyxSequence<T>;
    function Reverse: INyxSequence<T>;
    function Append(const AValue: T): INyxSequence<T>;
    function Prepend(const AValue: T): INyxSequence<T>;
    function DefaultIfEmpty: INyxSequence<T>; overload;
    function DefaultIfEmpty(const AFallback: T): INyxSequence<T>; overload;
    function WhereIndexed(const APredicate: INyxIndexedPredicate<T>): INyxSequence<T>;
    function SkipWhileIndexed(const APredicate: INyxIndexedPredicate<T>): INyxSequence<T>;
    function TakeWhileIndexed(const APredicate: INyxIndexedPredicate<T>): INyxSequence<T>;
    function SelectIndexed(const ASelector: INyxIndexedSelector<T, T>): INyxSequence<T>;
    function Select(const ASelector: INyxSelector<T, T>): INyxSequence<T>; overload;
    {$ifdef NYX_HAS_INLINE_CALLBACKS}
    function Select(const ASelector: TNyxSelectorReference<T, T>;
      const ACapture: INyxCallbackCapture = nil): INyxSequence<T>; overload;
    {$endif}
    function Concat(const AOther: INyxSequence<T>): INyxSequence<T>;
    function Distinct(const AEquality: INyxEquality<T>): INyxSequence<T>;
    function Union(const AOther: INyxSequence<T>;
      const AEquality: INyxEquality<T>): INyxSequence<T>;
    function Intersect(const AOther: INyxSequence<T>;
      const AEquality: INyxEquality<T>): INyxSequence<T>;
    function ExceptWith(const AOther: INyxSequence<T>;
      const AEquality: INyxEquality<T>): INyxSequence<T>;
    function Contains(const AValue: T; const AEquality: INyxEquality<T>): Boolean;
    function SequenceEqual(const AOther: INyxSequence<T>;
      const AEquality: INyxEquality<T>): Boolean;
    function OrderBy(const AOrdering: INyxOrdering<T>): INyxSequence<T>;
    { Stable whole-value ordering uses the existing owned recipe contract. }
    function Order(const AComparer: INyxComparer<T>;
      ADirection: TNyxOrderDirection = nodAscending): INyxSequence<T>;
    function OrderDescending(const AComparer: INyxComparer<T>): INyxSequence<T>;
    { Predicate terminals preserve short-circuit/cardinality rules, callback
      leases/captures and original failures. Nil predicates refuse. }
    function Count: Integer; overload;
    function Count(const APredicate: INyxPredicate<T>): Integer; overload;
    function Any: Boolean; overload;
    function Any(const APredicate: INyxPredicate<T>): Boolean; overload;
    function All(const APredicate: INyxPredicate<T>): Boolean;
    function First: T; overload;
    function First(const APredicate: INyxPredicate<T>): T; overload;
    function TryFirst(out AValue: T): Boolean; overload;
    function TryFirst(const APredicate: INyxPredicate<T>; out AValue: T): Boolean; overload;
    function Last: T; overload;
    function Last(const APredicate: INyxPredicate<T>): T; overload;
    function TryLast(out AValue: T): Boolean; overload;
    function TryLast(const APredicate: INyxPredicate<T>; out AValue: T): Boolean; overload;
    function Single: T; overload;
    function Single(const APredicate: INyxPredicate<T>): T; overload;
    function TrySingle(out AValue: T): Boolean; overload;
    function TrySingle(const APredicate: INyxPredicate<T>; out AValue: T): Boolean; overload;
    function ElementAt(AIndex: Integer): T;
    function TryElementAt(AIndex: Integer; out AValue: T): Boolean;
    function FirstOrDefault: T; overload;
    function FirstOrDefault(const AFallback: T): T; overload;
    function LastOrDefault: T; overload;
    function LastOrDefault(const AFallback: T): T; overload;
    function SingleOrDefault: T; overload;
    function SingleOrDefault(const AFallback: T): T; overload;
    { Named Where forms distinguish predicates from interface-valued fallbacks.
      Defaults are ordinary initialized T; Single still refuses multiple matches. }
    function FirstOrDefaultWhere(const APredicate: INyxPredicate<T>): T; overload;
    function FirstOrDefaultWhere(const APredicate: INyxPredicate<T>; const AFallback: T): T; overload;
    function LastOrDefaultWhere(const APredicate: INyxPredicate<T>): T; overload;
    function LastOrDefaultWhere(const APredicate: INyxPredicate<T>; const AFallback: T): T; overload;
    function SingleOrDefaultWhere(const APredicate: INyxPredicate<T>): T; overload;
    function SingleOrDefaultWhere(const APredicate: INyxPredicate<T>; const AFallback: T): T; overload;
    function ElementAtOrDefault(AIndex: Integer): T; overload;
    function ElementAtOrDefault(AIndex: Integer; const AFallback: T): T; overload;
    function ToArray: TNyxSequenceArray<T>;
    procedure ForEach(const AAction: INyxAction<T>); overload;
    {$ifdef NYX_HAS_INLINE_CALLBACKS}
    procedure ForEach(const AAction: TNyxActionReference<T>;
      const ACapture: INyxCallbackCapture = nil); overload;
    {$endif}
  end;

  { Type-changing Select cannot be a generic interface method in the supported
    Pascal dialect. This typed factory returns an ordinary fluent TResult
    sequence; neither source nor selector executes during construction. }
  TNyxSelect<TSource, TResult> = class
  public
    class function From(const ASource: INyxSequence<TSource>;
      const ASelector: INyxSelector<TSource, TResult>): INyxSequence<TResult>; static;
  end;

  { Optional concrete-array extension syntax, available on both targets.
    AsNyx copies the outer vector exactly like TNyxSequence<T>.From. Arbitrary
    record/interface/object types use that generic factory without coercion. }
  TNyxIntegers = TArray<Integer>;
  TNyxTexts = TArray<TNyxText>;
  TNyxIntegersHelper = type helper for TNyxIntegers
    function AsNyx: INyxSequence<Integer>;
  end;
  TNyxTextsHelper = type helper for TNyxTexts
    function AsNyx: INyxSequence<TNyxText>;
  end;

type
  { Compiler-visible implementation types. FPC expands public generics in the
    consuming unit and requires their dependencies in the interface section.
    Applications use the INyx* contracts/factories above, not these cursors. }
  TFunctionPredicate<T> = class(TInterfacedObject, INyxPredicate<T>)
  private
    FFunction: TNyxPredicateFunction<T>;
  public
    constructor Create(AFunction: TNyxPredicateFunction<T>);
    function Test(const AValue: T): Boolean;
  end;
  TFunctionSelector<TSource, TResult> = class(TInterfacedObject,
    INyxSelector<TSource, TResult>)
  private
    FFunction: TNyxSelectorFunction<TSource, TResult>;
  public
    constructor Create(AFunction: TNyxSelectorFunction<TSource, TResult>);
    function Project(const AValue: TSource): TResult;
  end;

  { The guarded cursor makes exception/EOF behavior uniform for every operator.
    No cursor owns its sequence, so a custom source can retire independently. }
  TEnumerator<T> = class(TInterfacedObject, INyxEnumerator<T>)
  private
    FCurrent: T;
    FValid: Boolean;
    FEnded: Boolean;
  protected
    function Advance(out AValue: T): Boolean; virtual; abstract;
  public
    function MoveNext: Boolean;
    function GetCurrent: T;
  end;

  TArrayEnumerator<T> = class(TEnumerator<T>)
  private
    FValues: TNyxSequenceArray<T>;
    FIndex: Integer;
  protected
    function Advance(out AValue: T): Boolean; override;
  public
    constructor Create(const AValues: TNyxSequenceArray<T>);
  end;
  TArraySequence<T> = class(TNyxSequence<T>, INyxSequenceCount)
  private
    FValues: TNyxSequenceArray<T>;
  public
    constructor Create(const AValues: array of T);
    function GetEnumerator: INyxEnumerator<T>; override;
    function NonEnumeratedCount(out AValue: TNyxQueryCount): Boolean;
  end;

  TSequenceOperation = (soWhere, soSkip, soTake, soSkipWhile, soTakeWhile);
  TStageEnumerator<T> = class(TEnumerator<T>)
  private
    FSource: INyxEnumerator<T>;
    FPredicate: INyxPredicate<T>;
    FOperation: TSequenceOperation;
    FRemaining: Integer;
    FSkipping: Boolean;
  protected
    function Advance(out AValue: T): Boolean; override;
  public
    constructor Create(const ASource: INyxEnumerator<T>;
      AOperation: TSequenceOperation; ACount: Integer;
      const APredicate: INyxPredicate<T>);
  end;
  TStageSequence<T> = class(TNyxSequence<T>)
  private
    FSource: INyxSequence<T>;
    FPredicate: INyxPredicate<T>;
    FOperation: TSequenceOperation;
    FCount: Integer;
  public
    constructor Create(const ASource: INyxSequence<T>;
      AOperation: TSequenceOperation; ACount: Integer;
      const APredicate: INyxPredicate<T>);
    function GetEnumerator: INyxEnumerator<T>; override;
  end;

  TSelectEnumerator<TSource, TResult> = class(TEnumerator<TResult>)
  private
    FSource: INyxEnumerator<TSource>;
    FSelector: INyxSelector<TSource, TResult>;
  protected
    function Advance(out AValue: TResult): Boolean; override;
  public
    constructor Create(const ASource: INyxEnumerator<TSource>;
      const ASelector: INyxSelector<TSource, TResult>);
  end;
  TSelectSequence<TSource, TResult> = class(TNyxSequence<TResult>)
  private
    FSource: INyxSequence<TSource>;
    FSelector: INyxSelector<TSource, TResult>;
  public
    constructor Create(const ASource: INyxSequence<TSource>;
      const ASelector: INyxSelector<TSource, TResult>);
    function GetEnumerator: INyxEnumerator<TResult>; override;
  end;

  TConcatEnumerator<T> = class(TEnumerator<T>)
  private
    FLeft: INyxEnumerator<T>;
    FRightSource: INyxSequence<T>;
    FRight: INyxEnumerator<T>;
  protected
    function Advance(out AValue: T): Boolean; override;
  public
    constructor Create(const ALeft: INyxEnumerator<T>;
      const ARight: INyxSequence<T>);
  end;
  TConcatSequence<T> = class(TNyxSequence<T>)
  private
    FLeft: INyxSequence<T>;
    FRight: INyxSequence<T>;
  public
    constructor Create(const ALeft, ARight: INyxSequence<T>);
    function GetEnumerator: INyxEnumerator<T>; override;
  end;

{$include nyx.linq.order.types.inc}
{$include nyx.linq.callbacks.types.inc}
{$include nyx.linq.indexed.types.inc}
{$include nyx.linq.sets.types.inc}
{$include nyx.linq.partitions.types.inc}

{ Checked finite Integer range; count >= 0, step <> 0. The final emitted value
  must remain in Integer range, validated without unsafe browser multiplication.
  Empty ranges emit nothing; positive/negative steps preserve exact values. }
function NyxRange(AStart, ACount: Integer; AStep: Integer = 1): INyxSequence<Integer>;
{ Construct exact portable count limbs; High must be at most $7fffffff. }
function NyxQueryCount(ALow: Cardinal; AHigh: Cardinal = 0): TNyxQueryCount;

{ Compiler support for checked zero-based Integer ordinals. Every built-in
  indexed cursor starts at zero. The last valid ordinal is allowed once; another
  movement refuses before entering a callback. Negative state refuses as well. }
function NextNyxQueryIndex(var AIndex: Integer; var AExhausted: Boolean): Integer;

implementation

uses
  Math;

{$include nyx.linq.order.implementation.inc}
{$include nyx.linq.callbacks.implementation.inc}
{$include nyx.linq.indexed.implementation.inc}
{$include nyx.linq.sets.implementation.inc}
{$include nyx.linq.partitions.implementation.inc}
{$include nyx.linq.convenience.implementation.inc}

function NyxQueryCount(ALow: Cardinal; AHigh: Cardinal): TNyxQueryCount;
begin

  if AHigh > $7fffffff then
  begin
    raise ENyxQuery.Create('Wide query count exceeds signed 64-bit domain');
  end;
  Result.FHigh := AHigh;
  Result.FLow := ALow;
end;

function TNyxQueryCount.Incremented: TNyxQueryCount;
begin
  Result := Self;

  if FLow = High(Cardinal) then
  begin

    if FHigh = $7fffffff then
    begin
      raise ENyxQuery.Create('Wide query count overflow');
    end;
    Result.FLow := 0;
    Inc(Result.FHigh);
  end
  else
  begin
    Inc(Result.FLow);
  end;
end;

function TNyxQueryCount.Added(const AValue: TNyxQueryCount): TNyxQueryCount;
var
  LLow: Cardinal;
  LHigh: Cardinal;
  LCarry: Cardinal;
  LRoom: Cardinal;
begin
  { Split low limbs into base-65536 digits. Every intermediate is exactly
    representable on both targets; no Cardinal wrap or unsafe wide number occurs. }
  LLow := (FLow mod 65536) + (AValue.FLow mod 65536);
  LHigh := (FLow div 65536) + (AValue.FLow div 65536) + (LLow div 65536);
  LCarry := LHigh div 65536;
  LRoom := $7fffffff - FHigh;

  if (AValue.FHigh > LRoom) or ((AValue.FHigh = LRoom) and (LCarry <> 0)) then
  begin
    raise ENyxQuery.Create('Wide query count overflow');
  end;
  Result.FLow := (LHigh mod 65536) * 65536 + (LLow mod 65536);
  Result.FHigh := FHigh + AValue.FHigh + LCarry;
end;

function TNyxQueryCount.TryInteger(out AValue: Integer): Boolean;
begin
  AValue := 0;
  Result := (FHigh = 0) and (FLow <= Cardinal(High(Integer)));

  if Result then
  begin
    AValue := Integer(FLow);
  end;
end;

function TNyxQueryCount.ToText: TNyxText;
var
  LDigits: array[0..3] of Cardinal;
  LIndex: Integer;
  LCarry: Cardinal;
  LCurrent: Cardinal;
  LNonzero: Boolean;
begin
  LDigits[0] := FHigh shr 16;
  LDigits[1] := FHigh and $ffff;
  LDigits[2] := FLow shr 16;
  LDigits[3] := FLow and $ffff;
  Result := '';
  repeat
    LCarry := 0;
    LNonzero := False;
    for LIndex := 0 to 3 do
    begin
      { A base-65536 division limb stays below 655360, exact on both targets. }
      LCurrent := LCarry * 65536 + LDigits[LIndex];
      LDigits[LIndex] := LCurrent div 10;
      LCarry := LCurrent mod 10;
      LNonzero := LNonzero or (LDigits[LIndex] <> 0);
    end;
    Result := TNyxText(Chr(48 + LCarry)) + Result;
  until not LNonzero;
end;

function TEnumerator<T>.MoveNext: Boolean;
begin
  FValid := False;
  Result := False;

  if FEnded then
  begin
    Exit;
  end;
  try
    Result := Advance(FCurrent);
    FValid := Result;
    FEnded := not Result;
  except
    FEnded := True;
    raise;
  end;
end;

constructor TFunctionPredicate<T>.Create(AFunction: TNyxPredicateFunction<T>);
begin
  inherited Create;
  FFunction := AFunction;
end;

function TFunctionPredicate<T>.Test(const AValue: T): Boolean;
begin
  Result := FFunction(AValue);
end;

constructor TFunctionSelector<TSource, TResult>.Create(
  AFunction: TNyxSelectorFunction<TSource, TResult>);
begin
  inherited Create;
  FFunction := AFunction;
end;

function TFunctionSelector<TSource, TResult>.Project(const AValue: TSource): TResult;
begin
  Result := FFunction(AValue);
end;

class function TNyxPredicate<T>.From(
  AFunction: TNyxPredicateFunction<T>): INyxPredicate<T>;
begin

  if not Assigned(AFunction) then
  begin
    raise ENyxQuery.Create('Predicate requires a function');
  end;
  Result := TFunctionPredicate<T>.Create(AFunction);
end;

class function TNyxSelector<TSource, TResult>.From(
  AFunction: TNyxSelectorFunction<TSource, TResult>): INyxSelector<TSource, TResult>;
begin

  if not Assigned(AFunction) then
  begin
    raise ENyxQuery.Create('Selector requires a function');
  end;
  Result := TFunctionSelector<TSource, TResult>.Create(AFunction);
end;

function TEnumerator<T>.GetCurrent: T;
begin

  if not FValid then
  begin
    raise ENyxQuery.Create('Query cursor has no current element');
  end;
  Result := FCurrent;
end;

constructor TArrayEnumerator<T>.Create(const AValues: TNyxSequenceArray<T>);
begin
  inherited Create;
  { Private sequence storage is immutable. The cursor shares that vector only;
    public materialization always copies it. }
  FValues := AValues;
end;

function TArrayEnumerator<T>.Advance(out AValue: T): Boolean;
begin
  Result := FIndex < Length(FValues);

  if Result then
  begin
    AValue := FValues[FIndex];
    Inc(FIndex);
  end;
end;

constructor TArraySequence<T>.Create(const AValues: array of T);
var
  LIndex: Integer;
begin
  inherited Create;
  SetLength(FValues, Length(AValues));
  for LIndex := 0 to High(AValues) do
  begin
    FValues[LIndex] := AValues[LIndex];
  end;
end;

function TArraySequence<T>.GetEnumerator: INyxEnumerator<T>;
begin
  Result := TArrayEnumerator<T>.Create(FValues);
end;

function TArraySequence<T>.NonEnumeratedCount(out AValue: TNyxQueryCount): Boolean;
begin
  AValue := NyxQueryCount(Cardinal(Length(FValues)));
  Result := True;
end;

constructor TStageEnumerator<T>.Create(const ASource: INyxEnumerator<T>;
  AOperation: TSequenceOperation; ACount: Integer;
  const APredicate: INyxPredicate<T>);
begin
  inherited Create;
  FSource := ASource;
  FOperation := AOperation;
  FRemaining := ACount;
  FPredicate := APredicate;
  FSkipping := True;
end;

function TStageEnumerator<T>.Advance(out AValue: T): Boolean;
var
  LValue: T;
begin
  Result := False;

  if (FOperation = soTake) and (FRemaining = 0) then
  begin
    Exit;
  end;

  if FSource = nil then
  begin
    raise ENyxQuery.Create('Sequence stage source returned no cursor');
  end;
  while FSource.MoveNext do
  begin
    LValue := FSource.Current;
    case FOperation of
      soWhere:
        begin

          if not FPredicate.Test(LValue) then
          begin
            Continue;
          end;
        end;
      soSkip:
        begin

          if FRemaining > 0 then
          begin
            Dec(FRemaining);
            Continue;
          end;
        end;
      soTake:
        begin
          Dec(FRemaining);
        end;
      soSkipWhile:
        begin

          if FSkipping then
          begin
            FSkipping := FPredicate.Test(LValue);

            if FSkipping then
            begin
              Continue;
            end;
          end;
        end;
      soTakeWhile:
        begin

          if not FPredicate.Test(LValue) then
          begin
            Exit;
          end;
        end;
    end;
    AValue := LValue;
    Result := True;
    Exit;
  end;
end;

constructor TStageSequence<T>.Create(const ASource: INyxSequence<T>;
  AOperation: TSequenceOperation; ACount: Integer;
  const APredicate: INyxPredicate<T>);
begin
  inherited Create;
  FSource := ASource;
  FOperation := AOperation;
  FCount := ACount;
  FPredicate := APredicate;
end;

function TStageSequence<T>.GetEnumerator: INyxEnumerator<T>;
begin
  Result := TStageEnumerator<T>.Create(FSource.GetEnumerator, FOperation,
    FCount, FPredicate);
end;

constructor TSelectEnumerator<TSource, TResult>.Create(
  const ASource: INyxEnumerator<TSource>;
  const ASelector: INyxSelector<TSource, TResult>);
begin
  inherited Create;
  FSource := ASource;
  FSelector := ASelector;
end;

function TSelectEnumerator<TSource, TResult>.Advance(out AValue: TResult): Boolean;
begin

  if FSource = nil then
  begin
    raise ENyxQuery.Create('Projection source returned no cursor');
  end;
  Result := FSource.MoveNext;

  if Result then
  begin
    AValue := FSelector.Project(FSource.Current);
  end;
end;

constructor TSelectSequence<TSource, TResult>.Create(
  const ASource: INyxSequence<TSource>;
  const ASelector: INyxSelector<TSource, TResult>);
begin
  inherited Create;
  FSource := ASource;
  FSelector := ASelector;
end;

function TSelectSequence<TSource, TResult>.GetEnumerator: INyxEnumerator<TResult>;
begin
  Result := TSelectEnumerator<TSource, TResult>.Create(FSource.GetEnumerator, FSelector);
end;

constructor TConcatEnumerator<T>.Create(const ALeft: INyxEnumerator<T>;
  const ARight: INyxSequence<T>);
begin
  inherited Create;
  FLeft := ALeft;
  FRightSource := ARight;
end;

function TConcatEnumerator<T>.Advance(out AValue: T): Boolean;
begin

  if FLeft <> nil then
  begin

    if FLeft.MoveNext then
    begin
      AValue := FLeft.Current;
      Exit(True);
    end;
    FLeft := nil;
  end;

  if FRight = nil then
  begin
    FRight := FRightSource.GetEnumerator;
    FRightSource := nil;
  end;
  Result := FRight.MoveNext;

  if Result then
  begin
    AValue := FRight.Current;
  end;
end;

constructor TConcatSequence<T>.Create(const ALeft, ARight: INyxSequence<T>);
begin
  inherited Create;
  FLeft := ALeft;
  FRight := ARight;
end;

function TConcatSequence<T>.GetEnumerator: INyxEnumerator<T>;
begin
  Result := TConcatEnumerator<T>.Create(FLeft.GetEnumerator, FRight);
end;

class function TNyxSequence<T>.From(const AValues: array of T): INyxSequence<T>;
begin
  Result := TArraySequence<T>.Create(AValues);
end;

class function TNyxSequence<T>.Empty: INyxSequence<T>;
begin
  Result := From([]);
end;

function TNyxSequence<T>.Where(const APredicate: INyxPredicate<T>): INyxSequence<T>;
begin

  if APredicate = nil then
  begin
    raise ENyxQuery.Create('Where requires an owned predicate');
  end;
  Result := TStageSequence<T>.Create(Self, soWhere, 0, APredicate);
end;

function TNyxSequence<T>.Skip(ACount: Integer): INyxSequence<T>;
begin

  if ACount < 0 then
  begin
    raise ENyxQuery.Create('Skip count must be nonnegative');
  end;
  Result := TStageSequence<T>.Create(Self, soSkip, ACount, nil);
end;

function TNyxSequence<T>.Take(ACount: Integer): INyxSequence<T>;
begin

  if ACount < 0 then
  begin
    raise ENyxQuery.Create('Take count must be nonnegative');
  end;

  if ACount = 0 then
  begin
    Result := Empty;
  end
  else
  begin
    Result := TStageSequence<T>.Create(Self, soTake, ACount, nil);
  end;
end;

function TNyxSequence<T>.SkipWhile(const APredicate: INyxPredicate<T>): INyxSequence<T>;
begin

  if APredicate = nil then
  begin
    raise ENyxQuery.Create('SkipWhile requires an owned predicate');
  end;
  Result := TStageSequence<T>.Create(Self, soSkipWhile, 0, APredicate);
end;

function TNyxSequence<T>.TakeWhile(const APredicate: INyxPredicate<T>): INyxSequence<T>;
begin

  if APredicate = nil then
  begin
    raise ENyxQuery.Create('TakeWhile requires an owned predicate');
  end;
  Result := TStageSequence<T>.Create(Self, soTakeWhile, 0, APredicate);
end;

function TNyxSequence<T>.Select(const ASelector: INyxSelector<T, T>): INyxSequence<T>;
begin
  Result := TNyxSelect<T, T>.From(Self, ASelector);
end;

function TNyxSequence<T>.Concat(const AOther: INyxSequence<T>): INyxSequence<T>;
begin

  if AOther = nil then
  begin
    raise ENyxQuery.Create('Concat requires a second sequence');
  end;
  Result := TConcatSequence<T>.Create(Self, AOther);
end;

function TNyxSequence<T>.Count: Integer;
var
  LCursor: INyxEnumerator<T>;
begin
  Result := 0;
  LCursor := GetEnumerator;
  while LCursor.MoveNext do
  begin

    if Result = High(Integer) then
    begin
      raise ENyxQuery.Create('Query count exceeds the supported Integer range');
    end;
    Inc(Result);
  end;
end;

function TNyxSequence<T>.Any: Boolean;
var
  LCursor: INyxEnumerator<T>;
begin
  LCursor := GetEnumerator;
  Result := LCursor.MoveNext;
end;

function TNyxSequence<T>.Any(const APredicate: INyxPredicate<T>): Boolean;
begin
  Result := Where(APredicate).Any;
end;

function TNyxSequence<T>.All(const APredicate: INyxPredicate<T>): Boolean;
var
  LCursor: INyxEnumerator<T>;
begin

  if APredicate = nil then
  begin
    raise ENyxQuery.Create('All requires an owned predicate');
  end;
  LCursor := GetEnumerator;
  while LCursor.MoveNext do
  begin

    if not APredicate.Test(LCursor.Current) then
    begin
      Exit(False);
    end;
  end;
  Result := True;
end;

function TNyxSequence<T>.TryFirst(out AValue: T): Boolean;
var
  LCursor: INyxEnumerator<T>;
begin
  AValue := FEmptyValue;
  LCursor := GetEnumerator;

  if LCursor = nil then
  begin
    raise ENyxQuery.Create('First source returned no cursor');
  end;
  Result := LCursor.MoveNext;

  if Result then
  begin
    AValue := LCursor.Current;
  end;
end;

function TNyxSequence<T>.First: T;
begin

  if not TryFirst(Result) then
  begin
    raise ENyxQueryEmpty.Create('First requires a nonempty sequence');
  end;
end;

function TNyxSequence<T>.TryLast(out AValue: T): Boolean;
var
  LCursor: INyxEnumerator<T>;
begin
  AValue := FEmptyValue;
  Result := False;
  LCursor := GetEnumerator;

  if LCursor = nil then
  begin
    raise ENyxQuery.Create('Last source returned no cursor');
  end;
  while LCursor.MoveNext do
  begin
    AValue := LCursor.Current;
    Result := True;
  end;
end;

function TNyxSequence<T>.Last: T;
begin

  if not TryLast(Result) then
  begin
    raise ENyxQueryEmpty.Create('Last requires a nonempty sequence');
  end;
end;

function TNyxSequence<T>.TrySingle(out AValue: T): Boolean;
var
  LCursor: INyxEnumerator<T>;
begin
  AValue := FEmptyValue;
  LCursor := GetEnumerator;

  if LCursor = nil then
  begin
    raise ENyxQuery.Create('Single source returned no cursor');
  end;
  Result := LCursor.MoveNext;

  if Result then
  begin
    AValue := LCursor.Current;

    if LCursor.MoveNext then
    begin
      AValue := FEmptyValue;
      raise ENyxQueryCardinality.Create('Single requires exactly one element');
    end;
  end;
end;

function TNyxSequence<T>.Single: T;
begin

  if not TrySingle(Result) then
  begin
    raise ENyxQueryEmpty.Create('Single requires a nonempty sequence');
  end;
end;

function TNyxSequence<T>.ElementAt(AIndex: Integer): T;
var
  LCursor: INyxEnumerator<T>;
begin

  if AIndex < 0 then
  begin
    raise ENyxQuery.Create('Element index must be nonnegative');
  end;
  LCursor := GetEnumerator;
  while LCursor.MoveNext do
  begin

    if AIndex = 0 then
    begin
      Exit(LCursor.Current);
    end;
    Dec(AIndex);
  end;
  raise ENyxQueryEmpty.Create('Element index is outside the sequence');
end;

function TNyxSequence<T>.ToArray: TNyxSequenceArray<T>;
var
  LCursor: INyxEnumerator<T>;
  LCount: Integer;
  LCapacity: Integer;
begin
  Result := nil;
  LCount := 0;
  LCursor := GetEnumerator;
  while LCursor.MoveNext do
  begin

    if LCount = Length(Result) then
    begin

      if LCount = High(Integer) then
      begin
        raise ENyxQuery.Create('Query materialization exceeds the Integer range');
      end;
      LCapacity := 16;

      if LCount > 0 then
      begin

        if LCount > High(Integer) div 2 then
        begin
          LCapacity := High(Integer);
        end
        else
        begin
          LCapacity := LCount * 2;
        end;
      end;
      SetLength(Result, LCapacity);
    end;
    Result[LCount] := LCursor.Current;
    Inc(LCount);
  end;
  SetLength(Result, LCount);
end;

class function TNyxSelect<TSource, TResult>.From(
  const ASource: INyxSequence<TSource>;
  const ASelector: INyxSelector<TSource, TResult>): INyxSequence<TResult>;
begin

  if (ASource = nil) or (ASelector = nil) then
  begin
    raise ENyxQuery.Create('Select requires an owned source and selector');
  end;
  Result := TSelectSequence<TSource, TResult>.Create(ASource, ASelector);
end;

function TNyxIntegersHelper.AsNyx: INyxSequence<Integer>;
begin
  Result := TNyxSequence<Integer>.From(Self);
end;

function TNyxTextsHelper.AsNyx: INyxSequence<TNyxText>;
begin
  Result := TNyxSequence<TNyxText>.From(Self);
end;

end.
