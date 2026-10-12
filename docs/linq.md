# Typed deferred sequences

[Fluent API](fluent-api.md) · [Collections](collections.md) ·
[Full LINQ/parallel task](../TODO/NS-1_linq-parallel_01.md) · [Operator coverage](linq-coverage.md)

`nyx.linq` supplies generic, reference-counted sequences for ordinary Pascal
values. `nyx.linq.collections` adds immutable Nyx snapshot entry points. This
implementation supplies deferred sequences, indexed/flattening projection, stable
ordering, grouping, lookups, joins, typed sets/membership, generation/chunks/tails,
typed element fallbacks, numeric/keyed aggregates and owned partitioned projection.
Further materialization/native helpers, wider numeric domains, predicate/from-end
convenience, operator-wide parallel policies and complete Studio integration
remain under the full task.

Use owned callback interfaces for state-bearing predicates and selectors.
Ordinary global Pascal functions have convenient adapters:

```pascal
uses
  nyx.linq;

function IsEven(const AValue: Integer): Boolean;
begin
  Result := (AValue mod 2) = 0;
end;

function DoubleValue(const AValue: Integer): Integer;
begin
  Result := AValue * 2;
end;

procedure ExploreValues;
var
  LInput: TNyxIntegers;
  LQuery: INyxSequence<Integer>;
  LValues: TNyxSequenceArray<Integer>;
begin
  SetLength(LInput, 6);
  LInput[0] := 1;
  LInput[1] := 2;
  LInput[2] := 3;
  LInput[3] := 4;
  LInput[4] := 5;
  LInput[5] := 6;

  LQuery := LInput.AsNyx
    .Where(TNyxPredicate<Integer>.From(IsEven))
    .Select(TNyxSelector<Integer, Integer>.From(DoubleValue))
    .Skip(1)
    .Take(2);

  // Construction is deferred. This produces [8, 12].
  LValues := LQuery.ToArray;
end;
```

`AsNyx` is available on the named `TNyxIntegers` and `TNyxTexts` array aliases
and `INyxCollectionSnapshot`. Both targets support these helpers. Other arrays,
including independent native generic-array specializations, use
`TNyxSequence<T>.From(Values)`. Broader native RTL collection helpers remain
required by the full task. The factory captures a detached outer array before
returning; changing or releasing the caller's array does not change this source.

Queries capture values and own their callbacks. A later materialization repeats
enumeration; it does not repeat a live-store capture. Every cursor has its own
position and survives release of its creating query. Pure callbacks provide
repeatable results. State-bearing callbacks implement `INyxPredicate<T>` or
`INyxSelector<TSource, TResult>`; adapters do not borrow an object receiver or a
nested stack frame.

For projection into another Pascal type:

```pascal
LNames := TNyxSelect<Integer, TNyxText>.From(
  LNumbers,
  TNyxSelector<Integer, TNyxText>.From(DescribeNumber));
```

`DescribeNumber` has signature
`function(const AValue: Integer): TNyxText`. The result is an ordinary fluent
`INyxSequence<TNyxText>`. The separate factory keeps both types explicit within
the supported dialect's generic-interface limitations.

## Stable typed ordering

An ordering recipe combines typed keys before it is passed to the sequence:

```pascal
LOrder := TNyxKeyOrder<TTask, Integer>.By(
  TNyxSelector<TTask, Integer>.From(Priority),
  NewNyxIntegerComparer,
  nodDescending)
  .ThenBy(TNyxKeyOrder<TTask, TNyxText>.By(
    TNyxSelector<TTask, TNyxText>.From(Title),
    NewNyxTextComparer));

LTasks := LTasks.OrderBy(LOrder);
```

`Priority` and `Title` are typed key functions; `LOrder` is an
`INyxOrdering<TTask>`. The recipe is immutable. ThenBy leaves its original
recipe intact, and equal keys retain the original input order. Construction of
a recipe, query or cursor does not evaluate keys. First movement materializes
the complete source, projects each key once per row, and performs a stable
merge with bounded index arithmetic. Each enumeration owns its values/keys;
materialized cursors survive source/query/recipe disposal.

Integer comparison never subtracts, and arbitrary comparer signs are normalized
before descending order. Text comparison uses Unicode scalar order on both
UTF-8 and UTF-16 targets, preserving supplementary characters and NUL.
`ntoAsciiInsensitive` explicitly folds A..Z; it is not a locale or full Unicode
casefold. Number comparers support infinities/signed zero and caller-selected
`nnoFirst`, `nnoLast` or `nnoRefuse` for NaN. Comparer/selector exceptions
retain their original type; a preparation failure yields no sorted rows.

## Grouping, lookups and joins

`nyx.linq.grouping` supplies `INyxEquality<TKey>`, immutable groups and indexed
lookups. Equality is explicit: `Equal` must be a stable equivalence relation,
and equal keys must have equal `Cardinal` hashes. Collisions compare typed keys;
they never become false matches. `NewNyxIntegerEquality`, `NewNyxBooleanEquality`
and `NewNyxTextEquality` provide portable defaults. Text hashes use Unicode
scalars, preserve embedded NUL and refuse malformed text. ASCII-insensitive
equality is an explicit option. No generic string conversion, locale or RTTI
silently decides equality. `TNyxEquality<T>.From` combines an owned comparer
(Compare=0) and typed hash selector for caller-defined keys.

```pascal
// ParentKey returns TNyxText; KeepRow returns its TNyxCollectionItem argument.
LGroups := TNyxGroupBy<TNyxCollectionItem, TNyxText, TNyxCollectionItem>.From(
  LRows,
  TNyxSelector<TNyxCollectionItem, TNyxText>.From(ParentKey),
  TNyxSelector<TNyxCollectionItem, TNyxCollectionItem>.From(KeepRow),
  NewNyxTextEquality);

// Immediate indexed admission of the same rows.
LLookup := TNyxLookup<TNyxCollectionItem, TNyxText, TNyxCollectionItem>.From(
  LRows,
  TNyxSelector<TNyxCollectionItem, TNyxText>.From(ParentKey),
  TNyxSelector<TNyxCollectionItem, TNyxCollectionItem>.From(KeepRow),
  NewNyxTextEquality);
LChildren := LLookup.ValuesFor('chapter');
```

Declare `LGroups` as `INyxSequence<INyxGroup<TNyxText, TNyxCollectionItem>>`,
`LLookup` as `INyxLookup<TNyxText, TNyxCollectionItem>` and `LChildren` as
`INyxSequence<TNyxCollectionItem>`. A local alias for the group interface keeps
callback signatures and nested generic specializations readable.

GroupBy construction and cursor acquisition do not enumerate. First movement
admits the complete source, projecting each row's key and element once. Each
cursor builds independently. Groups preserve first-key encounter order and
their values preserve source order; the first equivalent key becomes Group.Key.
ToLookup's factory admits immediately and exposes Count (groups), ItemCount
(rows), GroupAt, Groups, Has, GroupFor and ValuesFor. Absent GroupFor returns nil;
absent ValuesFor returns an ordinary empty sequence. Bounds, nil dependencies,
invalid source cursors and Integer capacity overflow refuse. Failed admission
exposes no partial lookup/group sequence. Open-addressed indexing caches hashes
while growing; arbitrary poor hashes can still degrade performance.

Groups and their value sequences can outlive source, lookup and cursors. They
own detached outer storage and retain ordinary managed payloads. They do not
freeze mutable objects or make caller equality thread-safe. Keys and equality
must remain stable for the lifetime of a retained lookup.

`nyx.linq.joins` adds `TNyxJoin<TLeft, TRight, TKey, TResult>.Inner`, `.Left`,
`.Right`, `.Full` and `TNyxGroupJoin<TOuter, TInner, TKey, TResult>.From`.
Each factory takes two typed sources, two key selectors, a result selector and
key equality. Inner results use `INyxPairValue<TLeft, TRight>` with Outer/Inner
partners. Outer results use `INyxJoinValue<TLeft, TRight>` with Left/Right and
separate HasLeft/HasRight flags. Check presence before reading a partner: an
absent getter raises `ENyxQuery`. Zero, empty text and a nil interface can all
be explicitly present; an absent side retains no supplied payload.

All joins defer enumeration until movement. Inner/left/full follow left order,
then right order within each matching key. Duplicate keys emit every Cartesian
pair. Full appends unmatched right rows in right order. Right mirrors left while
preserving the caller's Left/Right identities and follows right encounter order.
Each cursor builds an independent lookup of the non-driving side; full retains
right row ordinals to identify unmatched rows. Empty driving input skips the
other source, except full joins. A result already available does not advance the
next driving row prematurely. Source/key/equality/result failures propagate
unchanged and make that cursor terminal. Prior emitted rows remain emitted;
complete query/collection publication retains its separate atomic admission.

GroupJoin produces one result per outer row. Its
`INyxGroupJoinValue<TOuter, TInner>` supplies Outer and an independently retained
Matches sequence, including empty matches. Key/result callbacks run once per
outer row; retained contexts/matches do not refer back to a cursor or UI. Existing
owned, scoped nested and compiler-gated inline selector adapters work for these
key/result signatures. Close borrowed-frame leases before returning their frames.
Generic support implementation classes have distinct names across arities to
avoid the qualified pas2js runtime naming collision; generated JavaScript is not
patched. Complete grouping/lookup preparation is synchronous within a movement,
so source capture turns cannot preempt expensive preparation.

## Typed sets and membership

Ordinary fluent sequences support `Distinct`, `Union`, `Intersect` and
`ExceptWith`, all with an explicit `INyxEquality<T>`. Pascal reserves `except`,
so the ordinary exclusion method uses `ExceptWith`. The equality interface lives
in `nyx.linq`; grouping's existing equality factories provide portable defaults.
Construction and cursor acquisition enter neither source nor callback. Each
cursor owns its sources, key selector and mutable index independently.

```pascal
LNumbers := LNumbers
  .Distinct(NewNyxIntegerEquality)
  .ExceptWith(LExcludedNumbers, NewNyxIntegerEquality);

LRows := TNyxSetBy<TNyxCollectionItem, TNyxText>.DistinctBy(
  LRows,
  TNyxSelector<TNyxCollectionItem, TNyxText>.From(RowIdentity),
  NewNyxTextEquality);
```

`RowIdentity` returns the row's exact `Ref.ID`. Key-based results keep the first
qualifying **source value**, including its specialized fields and managed data.
`TNyxSetBy<T, TKey>` exposes DistinctBy, UnionBy, IntersectBy and ExceptBy. UnionBy
takes two sequences of T and projects keys in both. IntersectBy/ExceptBy take
the source of T plus a second sequence of TKey. Key callbacks run once per
consumed source row; the intersection can stop when all matching keys are used.
Owned, scoped nested and qualified inline selectors retain their existing rules.

Distinct streams first occurrences. Union streams distinct left values followed
by unseen right values; it acquires the right cursor only after left EOF.
Intersect emits each matching key once in left encounter order. ExceptWith/
ExceptBy emit the first left occurrence of each key absent from the other input.
For intersect/exclusion, first movement checks the left input, then admits the
complete other key input before any result; empty left skips it. Empty membership
immediately ends intersection, while empty exclusion still deduplicates left.
Take(0) enters no source or index preparation.

Indexes cache hashes while growing and compare colliding keys. Encounter order
comes from sources, never hash-slot order. Memory grows with distinct encountered
keys; intersect/exclusion also retain their other-input index. Consumed intersect
keys remain owned as probe witnesses until retirement, preserving collision
chains. Hash quality affects performance; there is no general worst-case speed
or production-budget claim. Mutable keys/equality must remain stable while indexed.
EOF/error retires sources, callbacks and indexes promptly; the guarded cursor's
last Current retains its ordinary value ownership until the cursor is released.

Nil dependencies and source cursors refuse. Source/key/hash/equality exceptions
propagate unchanged and make that cursor terminal. Previously emitted streaming
results remain emitted; full query/collection publication still admits only a
complete candidate. Failed preparation never publishes partial key membership.
These serial per-cursor operators feed existing deferred parallel submissions;
they do not establish operator-wide parallel set execution or browser workers.

`Contains(Value, Equality)` and `SequenceEqual(Other, Equality)` are immediate
short-circuit terminals. They compare through Equal without hashing/preparing an
index. Contains ends at the first match; SequenceEqual compares order and
multiplicity, stops at the first mismatch and checks lengths. Two empty sequences
are equal. Failed/nil sources/equality refuse under the same ownership/exception
rules. No membership operation mutates a source, live collection or UI.

## Generation, partitions and fallbacks

`NyxRange(Start, Count, Step)` produces a deferred finite Integer sequence. Count
is nonnegative and Step nonzero, defaulting to one. Positive and negative steps
are supported, including Low(Integer). Admission checks the final value with a
bounded span/division before any potentially unsafe multiplication. Each emitted
value stays in the Integer domain on native and browser. Empty ranges do no
arithmetic; valid enormous counts can be consumed as small streaming prefixes.

```pascal
LValues := NyxRange(-2, 5).ToArray;  // [-2, -1, 0, 1, 2]
LValues := NyxRange(5, 3, -2).ToArray;  // [5, 3, 1]
LValues := TNyxSequence<Integer>.RepeatValue(7, 3).ToArray;
LValues := TNyxSequence<Integer>.Iterate(1, 4,
  TNyxSelector<Integer, Integer>.From(NextOddNumber)).ToArray;
```

Pascal reserves `repeat`, so its factory is named RepeatValue. RepeatValue
captures a typed value and nonnegative count. Iterate captures a seed, count and
owned `INyxSelector<T,T>`: the seed is first, and the next selector runs only
between subsequent results. It never runs after the final result or for an empty
sequence. Named nested and compiler-gated inline adapters retain their normal
lease/capture rules. Source values and callbacks are retained without evaluation;
every cursor generates independently. Callback failure is original and terminal.
This typed recurrence does not claim the reference's preview generic-math Sequence.

Append/Prepend capture a typed value; Prepend can yield it before acquiring the
source cursor. DefaultIfEmpty yields one fallback only for a truly empty source.
Its no-argument form uses the type's ordinary initialized field value, preserving
zero/empty/nil on both compilers without pas2js's differing Default(T). The
explicit form retains the caller's typed fallback. Nonempty input is preserved.

SkipLast/TakeLast accept nonnegative counts. SkipLast streams with a bounded
lookahead ring; TakeLast admits the complete input into a bounded ring, then
emits the retained tail in encounter order. Zero SkipLast passes through; zero
TakeLast acquires no source. Storage grows geometrically with encountered values,
up to the requested count: a High(Integer) tail on a short input allocates only
its actual small buffer. Reverse fully admits an independent copy before yielding
in reverse order. Buffered preparation failures expose no partial tail/reversal.
Complete preparation still executes within one movement and cannot be preempted
by cooperative source-capture turns; production budgets remain open.

```pascal
LChunks := TNyxChunk<Integer>.From(NyxRange(0, 5), 2);
// LChunks: INyxSequence<INyxSequence<Integer>>, holding [0,1], [2,3], [4].
LTail := NyxRange(0, 5).TakeLast(2);  // [3,4]
LPrefix := NyxRange(0, 5).SkipLast(2);  // [0,1,2]
```

The independent Chunk factory avoids recursive generic expansion in the fluent
sequence interface. Size is positive; construction/cursor acquisition does not
enter a source. It reads at most Size rows per result without next-row lookahead,
emits no empty chunks/padding, and preserves a short final chunk. Buffers grow
with actual rows, including huge sizes on short input. Each chunk is an owned
detached ordinary sequence which can outlive all outer cursors/sources. A failed
partial chunk emits nothing; previously emitted chunks remain emitted. Managed
payloads preserve ordinary ownership. An owned buffer object avoids the qualified
pas2js static-generic var-array converter failure for interface payloads.

FirstOrDefault/LastOrDefault/SingleOrDefault offer ordinary initialized or explicit
typed fallback values. Single still raises cardinality failure for multiple rows.
TryElementAt uses zero-based positions and clears absent output; negative indices
are absent without acquiring a source. ElementAtOrDefault chooses the ordinary
initialized or explicit fallback on absence. Required ElementAt retains its
existing invalid-negative/absent failure behavior. Invalid cursors refuse through
portable query exceptions for these new terminals; genuine source/callback
failures propagate unchanged. Predicate-specific/from-end convenience remains
under full acceptance; callers can compose Where with these terminals today.

Nil sources/invalid counts/sizes refuse at the relevant factory boundary. EOF/error
retires buffers, source cursors and callbacks promptly; the guarded cursor's final
Current keeps its usual value ownership until cursor disposal. No operation
implicitly mutates a live store or authorizes UI access. Owned chunks, reversal
and tails feed the existing complete, revision-aware parallel/control publication.

## Typed folds, keyed and numeric aggregates

`nyx.linq.aggregates` supplies single-pass terminal folds through an owned,
immutable `INyxFoldValue<TAccumulator,T>` with Accumulator and Value properties.
`TNyxAggregate<T,TAccumulator>.Fold` accepts source, typed seed and folder;
empty input returns the seed. `TNyxReduce<T>.Fold` uses the first value as seed
and refuses empty input. `TNyxAggregateResult<T,TAccumulator,TResult>.Fold`
adds a result selector that runs once after successful completion. Nil dependencies
refuse before entry; source/folder/result exceptions remain original. Mutable
seed/payload aliases keep ordinary caller ownership and synchronization rules;
complete admission does not roll back user callback side effects.

```pascal
type
  TAmountFold = INyxFoldValue<Integer, TInvoice>;

function AddAmount(const AValue: TAmountFold): Integer;
begin
  Result := NyxAddQueryInteger(AValue.Accumulator, AValue.Value.Amount);
end;

// LInvoices: INyxSequence<TInvoice>; Amount is an Integer in the caller's model.
LTotal := TNyxAggregate<TInvoice, Integer>.Fold(LInvoices, 0,
  TNyxSelector<TAmountFold, Integer>.From(AddAmount));
```

Managed contexts can outlive the fold and retain exact specialized values, without
source/cursor/UI links. Existing owned, scoped nested and compiler-gated inline
selector adapters apply; interface captures use the existing portable capture
contract. Closing a borrowed-frame lease prevents deferred keyed folder entry.

CountBy/AggregateBy return ordinary deferred sequences of immutable
`INyxKeyValue<TKey,TAccumulator>`. Their factories are
`TNyxCountBy<T,TKey>.From` and `TNyxAggregateBy<T,TKey,TAccumulator>.From/Seeded`.
From accepts source, key selector, typed equality, a per-key seed selector and
folder. Seeded accepts a constant typed seed instead. Use the per-key selector
to create independently owned mutable seeds; constant mutable seed copies can
alias by the caller's deliberate choice. Every new key seeds once, every consumed
row projects/hashes/folds once, and first equivalent keys retain encounter order.
Hash collisions compare typed keys; growth reuses cached hashes. CountBy retains
per-key counts rather than every grouped source row.

Construction/GetEnumerator enters no source/callback. First movement completes
one independent indexed admission before emitting results. Failures expose no
partial keyed sequence; successful key/value objects retain detached managed
accumulators after source/cursor retirement. Integer counts refuse overflow through
`NyxIncrementQueryCount`. Complete preparation is synchronous within a movement;
parallel source-capture turns cannot preempt it and full budgets remain open.

`TNyxExtrema<T>` offers Min/Max and TryMin/TryMax with explicit typed comparers.
`TNyxExtremaBy<T,TKey>` offers the corresponding By forms with typed key selectors.
They stream once, project each key once, retain the first tie and return the
original source value. Try forms clear empty/failed output to ordinary initialized
T; required empty forms raise ENyxQueryEmpty. Comparers define Unicode/locale/NaN
behavior explicitly. No subtracting generic extreme values or string conversion
supplies comparison.

`nyx.linq.numeric` provides `TNyxIntegerAggregate` and `TNyxNumberAggregate`:
Sum, TryAverage, Average, SumOptional and AverageOptional. Integer Sum is checked
in the Integer domain regardless of compiler overflow flags. Empty sum is zero;
required empty average refuses and TryAverage returns absence/zero. Double Sum
uses compensated streaming accumulation to preserve small cancellation-lost
addends. Average uses a running mean, with bounded same-sign difference and
opposite-sign weighting, so large finite averages can succeed without an
overflowing total. Counts are checked Integer. These are floating results with
ordinary rounding, not exact arbitrary-precision/decimal arithmetic.

Number policy is typed and caller-selectable: `nnpFinite` defaults to rejecting
NaN/infinity and overflow; `nnpIEEE` propagates them explicitly. Large same-sign
addition scales by powers of two and commits intermediates to Double before an
overflow decision, avoiding target-dependent extended intermediate precision or
global FPU exception-mask changes. Opposite infinities produce NaN explicitly.
Numeric source/value failures return no partial aggregate result.

`INyxOptional<T>` distinguishes explicit HasValue from the value. Some(0),
Some(nil) and Some(empty text) remain present; None.Value raises ENyxQueryEmpty.
Create values with `TNyxOptional<T>.Some/None`. Optional numeric operators skip
only explicit absent entries: a nil optional interface refuses. Optional Sum is
present zero for empty/all-absent input; optional Average is None. Caller sources
can project specialized fields into typed numeric sequences before aggregation.
Additional numeric domains and convenience forms remain under full acceptance.
Actual control qualification joins per-parent CountBy results back into rows and
displays their numeric values after complete revision-aware parallel publication.

## Nested and inline callbacks

Include `nyx.callback.features.inc` in each application unit using inline
syntax, and add Nyx's source directory to the compiler's include path (`-Fi`).
The include checks `FPC_FULLVERSION` and `PAS2JS_FULLVERSION`. Qualified 3.3.1+
families enable functionreferences/anonymousfunctions; older versions retain
the ordinary managed adapters. `NYX_DISABLE_INLINE_CALLBACKS` explicitly
selects that fallback without reinstalling a working compiler.
`NyxHasInlineCallbacks` exposes the selected syntax capability.

```pascal
{$include nyx.callback.features.inc}

// Inside an application routine:
{$ifdef NYX_HAS_INLINE_CALLBACKS}
LQuery := LValues.AsNyx
  .Where(function(const AValue: Integer): Boolean
    begin
      Result := AValue > LThreshold;
    end)
  .Select(function(const AValue: Integer): Integer
    begin
      Result := AValue + LOffset;
    end);

LQuery.ForEach(procedure(const AValue: Integer)
  begin
    Inc(LTotal, AValue);
  end);
{$endif}
```

Inline functions/procedures are also available through FromInline on predicate,
selector, comparer and action factories. These permit type-changing projection
and typed ordering. ForEach is synchronous on the caller's thread. Compiler
function references keep scalar captures beyond the enclosing routine; they do
not make mutable captures thread-safe.

Native FPC retains captured Pascal interfaces automatically. The installed
pas2js compiler releases an interface local at routine exit even when a lexical
function captures it. Portable code therefore supplies explicit owners:

```pascal
LQuery := LValues.AsNyx.Where(
  function(const AValue: Integer): Boolean
  begin
    Result := LRules.Accept(AValue);
  end,
  NyxCapture([LRules]));
```

`LRules` is a reference-counted application interface. NyxCapture copies and
retains the supplied interfaces until the last callback releases the capture.
This is the same contract on both targets. `NyxInlineRetainsInterfaces` reports
automatic compiler retention; explicit captures keep portable authoring uniform.
Raw class pointers still require an independent lifetime or a revocable port.
Captures must not retain their own callback/query, which would form a cycle.

Named nested variables have dedicated typed adapters:

```pascal
LLease := NewNyxCallbackLease;
try
  LPredicateVariable := AboveThreshold;
  LQuery := LValues.AsNyx.Where(
    TNyxPredicate<Integer>.FromNested(LPredicateVariable, LLease));
  LQuery.ForEach(TNyxAction<Integer>.FromNested(AddToTotal, LLease));
finally
  LLease.Close;
end;
```

`LPredicateVariable` is a `TNyxPredicateNested<Integer>`; selectors, comparers
and procedures have their own nested variable families too. Native declarations
use nestedprocvars; browser declarations use ordinary nested function values.
Always Close the lease before returning from the enclosing routine. Retaining a
lease does not extend a native stack frame. A retained query/cursor refuses after
Close or last-lease release, and another native thread refuses before dereferencing
the borrowed frame. Owning function references or managed implementations are the
appropriate choices for work that escapes this scope.

Syntax alone is not qualification: actual native/browser capture lifetimes and
the explicit disable path are exercised. The FPC
[function-reference announcement](https://lists.freepascal.org/fpc-pascal/2022-May/060511.html)
and [nested procedural type reference](https://www.freepascal.org/daily/doc/ref/refse17.html)
describe the language distinction. Development snapshots still need their own
execution evidence.

| Operation | Behavior |
| --- | --- |
| Where | Deferred filter; repeated calls compose in pipeline order |
| Select | Deferred same-type mapping; the typed factory permits a type change |
| Skip / Take | Nonnegative counts; zero Take does not enter the upstream source |
| SkipWhile / TakeWhile | Prefix partitioning; callbacks stop when the prefix ends |
| Concat | Deferred left-then-right sequence; right enumeration starts after left EOF |
| OrderBy / recipe ThenBy | Deferred stable multi-key sorting; key projection once per row/key |
| Count | Full enumeration; Integer overflow refuses |
| Any / All | Short-circuit; empty Any is False and empty All is True |
| First / Last | Empty input raises ENyxQueryEmpty |
| Single / TrySingle | Multiple input raises ENyxQueryCardinality |
| TryFirst / TryLast / TrySingle | False on absence, clearing the output to its initialized value |
| ElementAt | Zero-based access; negative or absent positions refuse |
| ToArray | One enumeration into a separately owned outer vector |
| ForEach | Synchronous owned action, named nested procedure or supported inline procedure |

Indexed filtering and projection use owned immutable contexts:

```pascal
function KeepAlternate(const AValue: INyxIndexedValue<Integer>): Boolean;
begin
  Result := (AValue.Index mod 2) = 0;
end;

LQuery := LInput.AsNyx.WhereIndexed(
  TNyxIndexedPredicate<Integer>.From(KeepAlternate));
```

`WhereIndexed`, `SkipWhileIndexed`, `TakeWhileIndexed` and `SelectIndexed` are
ordinary fluent sequence methods. `TNyxSelectIndexed<TSource,TResult>.From`
provides type-changing indexed projection. Each stage counts its own input from
zero, including rows it filters out; a later indexed stage starts over on its
own input. Prefix callbacks stop at their boundary. Checked ordinals allow the
last valid Integer once and refuse another callback entry without overflow.

`INyxIndexedValue<T>` owns its Value/Index and may be retained independently.
Fields follow normal Pascal ownership; arbitrary mutable payloads are not deep
cloned or synchronized. `Wrap` accepts ordinary managed predicates/selectors over
that context, including existing FromNested leases and qualified FromInline
references with explicit NyxCapture where needed. This keeps one callback lifetime
contract instead of silently borrowing an object or a stack frame.

Flattening lives in `nyx.linq.projection`:

```pascal
LRows := TNyxSelectMany<TSection, TItem>.From(
  LSections,
  TNyxSelector<TSection, INyxSequence<TItem>>.From(ItemsInSection));
```

`FromIndexed` accepts an indexed collection selector. Empty inner sequences are
skipped; their outer ordinal still counts. Nil returned sequences/cursors are
explicit errors. Construction and acquiring the flatten cursor invoke no outer
enumeration or collection callback. One inner cursor is active at a time, retaining
its own inputs while the temporary inner sequence can retire. Inner exhaustion,
failure and early cursor disposal release that cursor promptly. EOF/failure is
terminal; original callback errors retain their class/message. Take(0) enters
neither outer nor inner. The returned typed sequence supports ordinary fluent
operators and direct parallel pipeline submission.

`TNyxSelectManyResult<TOuter,TInner,TResult>` additionally accepts a selector over
`INyxPairValue<TOuter,TInner>`. Its immutable managed Outer/Inner context preserves
specialized values and may be retained. The correct outer value stays alive while
its inner values are mapped; no result callback enters for an empty inner sequence.
Both ordinary and indexed collection variants are supported. Unindexed flattening
does not impose an ordinal limit merely for an internal implementation detail.

The browser compiler refuses COM interfaces in record fields. Indexed/pair contexts
therefore expose portable immutable interfaces backed by managed objects, allowing
interface-valued payloads on both targets. A caller-retained context legitimately
keeps its values alive. Compiler-created managed array/getter temporaries can also
retain values until routine exit; exact lifetime qualification explicitly releases
all caller references before testing last-owner retirement.

The maintained projection packet executes **8339 native / 8335 HTTP browser**
complete LINQ checks, including 51 new projection/flattening assertions. Original
source/control/partition/scheduler consumers remain **108/108**, **77/76**,
**4163/4162**, **55/52**. The inline-disabled complete-source consumer executes
**8331/8327** on the installed compiler family; this is not an older compiler
installation claim. All native heaps are clean. The actual pipeline duplicates
then indexed-filters generic values and bound rows before partitioned delivery,
preserving full admission, stale-revision refusal and actual control retirement.
Full grouping/joins/sets/aggregates/helpers, operator-wide parallel policies,
Studio integration and production budgets remain required by the coverage map.

Current is valid only after successful movement. EOF and callback failures are
terminal for that cursor. Callback exceptions keep their original type and
message; they do not produce an apparent successful partial materialization.

Managed interface elements remain retained. Raw class-instance elements are
borrowed and must outlive all queries/cursors that read them. Arbitrary records
containing mutable inner arrays retain ordinary Pascal sharing semantics; Nyx
cannot infer a deep clone for an unknown application type. These distinctions
also inform the upcoming explicit parallel-input ownership contract.

For Nyx data rows:

```pascal
LRows := LTasks.Snapshot.AsNyx
  .Where(TNyxPredicate<TNyxCollectionItem>.From(IsUrgent));
```

This captures the exact immutable row values/identities from that snapshot.
Later store edits and store/document disposal do not change the captured rows.
Neither query construction nor evaluation edits the store. The serializable
bound-control filter/sort policy remains in
[collection queries](collection-queries.md); executable application functions
are not silently encoded as wire predicates.

Owned execution is available through `nyx.linq.execution`:

```pascal
LSnapshot := LTasks.Snapshot;
LReceiver := NyxCollectionQueryReceiver(LScheduler, LTasks);
LRun := TNyxQuery<TNyxCollectionItem>.Submit(
  LSnapshot.AsNyx.Where(TNyxPredicate<TNyxCollectionItem>.From(IsUrgent)),
  LScheduler, LReceiver.Port);
```

The default computes later on UI on both targets, then delivers the complete
result through a separate UI queue entry. A retained run reports Pending,
Computing, AwaitingUI, Succeeded, Discarded, Cancelled or Failed. `Failure` retains
the exact callback/publication diagnostic; Phase distinguishes an empty diagnostic
from success. Query failures never deliver a partial array. `Cancel` is safe from
a native worker and is checked around cursor movements; arbitrary callbacks and
an operator's full preparation are not forcibly interrupted. There is no blocking
Wait/Join API. Keep the UI event loop running for worker-to-UI handoff.

`NyxCollectionQueryReceiver` (unit `nyx.linq.publication`) captures the exact target
revision. It admits the complete candidate against that key/schema, then assigns
it in one ordinary atomic collection publication. Stale revisions, duplicate
identities, schema/budget failures and validator failures preserve accepted data.
Observers retain ordinary committed-notification semantics: an observer error
can report failure after the collection has committed. No asynchronous wrapper
can undo a receiver that already entered or committed.

Keep the receiver lease while its owning view is active. Close it on UI before
navigation/disposal, or release its last lease there. Retained queued ports then
discard; a newly created lease supplies a distinct generation. Method receivers
use `TNyxQueryReceiver<T>.Bind(LScheduler, ReceiveValues)` with the same explicit
borrowed-object lifetime. The query receipt owns neither a view nor a scheduler.

Pass `neThreaded` explicitly to require a real native worker. It refuses on the
browser; `neAsynchronous` uses native workers or the browser event loop. Worker
queries require independently owned immutable inputs and thread-safe callbacks.
Managed interfaces, anonymous captures and arbitrary Pascal records do not make
mutable state thread-safe. Scoped nested functions enforce their origin thread.
One ordinary query enumerates serially on its selected thread; partitioned
projection below supplies concurrent workers, while full operator-wide policies
remain required work. Browser
event-loop scheduling never claims parallel threads.

The executed semantic-source companion mounts actual browser/LCL collection
controls and checks complete publication, changed revisions, revoked receivers,
queued cancellation, callback/candidate failure, main-thread delivery and real
native worker execution. Run `tools/build.ps1 -Target linq-execution` with a fresh
`-LinqEvidenceDirectory` and exact MCP `-LinqSourceDirectory`; execute the matched
`linq-execution.html` host over HTTP separately. This qualifies the bounded owned
execution/publication path, not full parallelism or the complete LINQ task.

The design reference is [ezthreads](https://github.com/mr-highball/ezthreads),
inspected at public commit `a0a391120ba14dd4eb6b3983e874f29336227230`.
Its fluent retained-work, pool and synchronized-completion patterns inform the
new contract; it is not an installed Nyx dependency.

`nyx.linq.parallel` supplies owned partitioned projection:

```pascal
LSnapshot := LTasks.Snapshot;
LRows := LSnapshot.AsNyx.ToArray;
LReceiver := NyxCollectionQueryReceiver(LScheduler, LTasks);
LRun := TNyxParallelSelect<TNyxCollectionItem, TNyxCollectionItem>.Submit(
  LRows,
  TNyxPartitionSelector<TNyxCollectionItem, TNyxCollectionItem>.From(PrepareTask),
  LScheduler,
  LReceiver.Port,
  TNyxParallelOptions.Defaults
    .Concurrency(4)
    .PartitionSize(128)
    .Order(npoSource)
    .Failures(npfStop));
```

The outer input vector is captured on UI before submission returns. The caller
may change/release it afterward. Immutable Nyx row values are suitable inputs;
raw object pointers and arbitrary mutable inner arrays retain ordinary Pascal
ownership and are not implicitly cloned or made thread-safe. The factory never
shares a mutable query cursor across threads. `ToArray` here is an explicit
terminal capture; the sequence overload below offers deferred capture instead.

For stateful transforms, implement `INyxPartitionSelectorFactory<TSource,TResult>`.
`NewSelector(Partition, SourceOffset)` runs on UI once per admitted partition and
returns an owned selector with independent state. Shared mutable state requires
explicit caller synchronization. Pure global functions can use the factory above;
each partition receives a fresh adapter. Scoped nested callbacks keep their lease/
origin-thread checks; an owning anonymous reference does not itself synchronize
captured state. Factory exceptions or nil selectors fail the whole operation.

Defaults requires native workers, four in-flight partitions, 128 rows per
partition, a 1,048,576-row input limit and source order. Concurrency is 1..64,
partition size 1..65,536, and caller-tunable input limit 1..16,777,216. Zeroed
options refuse. These bound row counts/queued work, not the byte size or cost of
an arbitrary application value. The shared scheduler's pool bounds actual
threads; capacity refusal fails the receipt and cancels siblings without retry
or implicit UI fallback. No worker thread is created or joined by this layer.

Browser callers explicitly choose `.Execution(npxDeferredUI)` for partitioned
event-loop work; required workers refuse there, including empty input. The same
deferred choice works natively and preserves query semantics without claiming
parallel threads. Computation is deferred, and final result delivery is on UI
through the same revocable method/collection receiver leases.

`npoSource` preserves original input ordinals despite out-of-order worker replies.
`npoCompletion` concatenates partitions in UI-observed reply order, preserving
the order inside each partition. This is not a promise about physical CPU finish
timestamps. `npfStop` stops scheduling and cooperatively cancels siblings after
the first observed failure. `npfCollect` lets other partitions finish; each
failing partition stops at its first failing row. Neither policy delivers a
partial result. `INyxParallelRun.FailureCount/FailureAt` retain the failed partition,
source offset and exact diagnostic in observation order. Empty messages are still
failures; the ordinary receipt retains the first observed terminal diagnostic.

Cancellation also cancels attached pending scheduler tickets, reclaiming pool
capacity. Running selectors check cancellation between rows; already entered
callbacks/receivers finish under their ordinary ownership contract. The UI must
keep pumping its event loop so workers can post their completion safely. There
is no blocking Join/Wait. Closing a receiver prevents late UI entry; its captured
collection revision separately refuses changed data. Full candidate admission
still precedes one atomic collection publication.

The maintained `linq-parallel` build uses fresh semantic companion source and
executes **4,163 native / 4,162 HTTP browser** checks plus **77/76** owned-execution
regression checks. A native event gate proves two worker callbacks overlap and
forces completion order to differ from source order. All 4,097 type-changing
results, the final short partition, owned selector teardown, both failure
policies, running/queued cancellation, capacity refusal, actual controls and
retired/stale collections are checked. Browser disposal is explicit and native
heaps are clean. Repeated row checks are not feature credit or a performance
budget. Complete LINQ operators, parallel pipeline policies, browser worker
transport, Studio authoring and production budgets remain open.

Deferred queries can also be submitted directly:

```pascal
LQuery := LValues.AsNyx
  .Where(TNyxPredicate<Integer>.From(IsEven))
  .OrderBy(TNyxValueOrder<Integer>.By(NewNyxIntegerComparer))
  .Select(TNyxSelector<Integer, Integer>.From(DoubleValue));

LRun := TNyxParallelSelect<Integer, TNyxText>.Submit(
  LQuery,
  TNyxPartitionSelector<Integer, TNyxText>.From(DescribeValue),
  LScheduler,
  LTextReceiver.Port,
  TNyxParallelOptions.Defaults.CaptureRowsPerTurn(256));
```

Submit retains the recipe and evaluates nothing. Each submission later owns one
independent cursor, capturing at most the configured output-row quantum per UI
turn before yielding through `INyxSchedulerTurns`. No worker factory enters
until EOF and complete input-limit admission. Source failure, overflow, cancellation
or expired scoped callbacks therefore start no partial partition work. The source
cursor retires before worker admission, and caller recipe references may be
released after submission. Captured inputs remain bounded owned vectors internally;
the caller need not manage an intermediate array.

The source pipeline shapes serially on UI. The separate projection partitions use
the selected native-worker or explicit deferred-UI mode. A MoveNext call can itself
scan many upstream elements, and sorting can materialize its whole source before
yielding a row. Capture quantum does not preempt caller callbacks, filtering scans
or operator preparation. Streaming/operator-wide parallel execution remains open.

Built-in cooperative turns use the browser timer and qualified native RTL delayed
queue. Native syntax is compiler-version gated for the inspected 3.3.1 family;
older or alternative schedulers report unsupported capability. The original
INyxScheduler interface/GUID is unchanged. Alternatives explicitly implement
INyxSchedulerTurns to support direct sequence submission. A turn offers scheduling
opportunity, not a latency, physical input or painted-frame guarantee. Independent
continuation tickets avoid a recursive parent chain growing with source length.

The `linq-pipeline` target executes the actual semantic-source companion with
**108/108** native/HTTP browser pipeline checks, including real UI polling between
capture batches, independent repeated submissions, exact source/expired-frame
failures, bounded admission, cancellation, sorted row publication and stale/retired
controls. It retains the **4163/4162** partition, **77/76** receipt and **55/52**
scheduler regressions. Native heaps are clean and new browser consumers explicitly
dispose their owned fixtures. Use exact `-LinqSourceDirectory` and a new
`-LinqEvidenceDirectory`; matched HTTP execution remains explicit. No complete
operator family, task or production budget is inferred from these counts.

Run `tools/build.ps1 -Target linq` for checked native execution and a matched
browser host. Supply `-LinqSourceDirectory` with the exact semantic mixed
collection companion to include compiled authored-default integration, and
`-LinqEvidenceDirectory` with a new output directory to retain prior evidence.
HTTP execution of `linq.html` separately verifies `data-nyx-linq="passed"`.
The full ordering/callback companion executes 8288 native / 8284 browser checks.
The inline-disabled standalone consumer executes 8278 / 8274. These include
complete 4,096-row ordering/tie checks and captured interface retirement; repeated
row assertions are not separate feature acceptance or a performance budget.
The original foundation includes 4,096-row
queries, independent cursors, ownership, exact text/numbers and failure behavior.
Managed interface elements remain alive through source/cursor/reader transitions
and retire exactly once on both targets.
This establishes the deferred/ordering/callback foundation, not full LINQ or parallel/UI acceptance.

The native-only extra case covers explicitly numbered enum declarations, which
the installed pas2js compiler refuses before generating a query. Portable enum,
Boolean, Integer, Double, text and record cases run on both targets. This compiler
capability difference remains explicit; it is not counted as browser qualification.
