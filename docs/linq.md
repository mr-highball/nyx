# Typed deferred sequences

[Fluent API](fluent-api.md) · [Collections](collections.md) ·
[Full LINQ/parallel task](../TODO/NS-1_linq-parallel_01.md) · [Operator coverage](linq-coverage.md)

`nyx.linq` supplies generic, reference-counted sequences for ordinary Pascal
values. `nyx.linq.collections` adds immutable Nyx snapshot entry points. This
implementation supplies deferred sequences, indexed/flattening projection, stable
ordering, grouping, lookups, joins, typed sets/membership, generation/chunks/tails,
typed element fallbacks, numeric/keyed aggregates and owned partitioned projection.
Owned materialization and native RTL capture extend those contracts. Wider numeric
domains, remaining standard convenience, operator-wide parallel policies and
complete Studio integration remain under the full task.

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

## Owned materialization and native helpers

`nyx.linq.materialization` supplies reference-counted portable immutable list,
dictionary and hash-set views. They follow Nyx snapshot ownership: materialize
once, own detached outer storage and retain exact ordinary managed values. They
do not grant mutation authority over a live store or UI. `ToArray` and native
editable copies let callers change detached data; mutable nested payloads still
retain ordinary sharing and synchronization rules. Raw object elements remain
borrowed and must outlive every reader of them.

```pascal
LList := TNyxMaterialize<Integer>.ToList(LNumbers);
LSet := TNyxMaterialize<Integer>.ToHashSet(LNumbers, NewNyxIntegerEquality);

LAmounts := TNyxDictionary<TInvoice, TNyxText, Integer>.From(
  LInvoices,
  TNyxSelector<TInvoice, TNyxText>.From(InvoiceID),
  TNyxSelector<TInvoice, Integer>.From(InvoiceAmount),
  NewNyxTextEquality,
  ndpRefuse);
```

Declare these as `INyxList<Integer>`, `INyxHashSet<Integer>` and
`INyxDictionary<TNyxText,Integer>`. List Count/ItemAt preserve order/multiplicity;
AsSequence and ToArray independently own detached data. Hash sets preserve
first-occurrence order and expose Count, Contains and AsSequence. Dictionary
Count, ContainsKey, TryGetValue/GetValue, EntryAt, Entries, Keys and Values use
indexed typed equality. TryGetValue clears absence; required absent keys/indices
raise ENyxQueryEmpty. Entry sequences/objects and queries survive disposal of
their creating materialized containers without back-reference cycles.

Duplicate policy is closed and caller-selectable: `ndpRefuse` (default),
`ndpKeepFirst`, `ndpReplaceValue`. KeepFirst skips the duplicate value projector;
Refuse rejects before it; ReplaceValue projects the replacement while retaining
the first equivalent key and its encounter position. Each consumed row projects/
hashes its key once. Collisions compare typed keys; growth caches hashes. Nil
dependencies and invalid policies/cursors refuse; source/key/value/equality
failures expose no partial container and propagate unchanged. Constructor/index
ownership transfers only after successful copies. Keys/equality remain stable
while indexed; arbitrary mutable objects are not frozen or made thread-safe.

`nyx.linq.native` adapts native `Generics.Collections` containers.
`TNyxNativeQuery<T>.FromEnumerable` captures any typed RTL enumerable immediately,
then releases its native enumerator. Caller mutation/disposal cannot change that
query's detached vector. The caller owns the container and synchronizes capture
against concurrent writes. Interface payloads retain normally; copying borrowed
objects does not keep them alive after an owning TObjectList frees them. Project
owned value data while those objects are alive, or keep the objects alive.
`ToRTLList` returns a detached **caller-owned** mutable `TList<T>`; free it.
`TNyxNativeDictionary<TKey,TValue>.FromDictionary` returns owned immutable entries
in actual native enumeration order, without claiming native insertion order.

The qualified FPC probe rejects generic class helpers. It also treats independent
generic specializations distinctly during helper lookup. Common extension calls
therefore use exported native aliases `TNyxIntegerList`, `TNyxTextList`,
`TNyxIntegerQueue`, `TNyxIntegerStack`:

```pascal
var
  LItems: TNyxIntegerList;
  LQuery: INyxSequence<Integer>;
begin
  LItems := TNyxIntegerList.Create;
  try
    LItems.Add(3);
    LItems.Add(1);
    LQuery := LItems.AsNyx;
  finally
    LItems.Free;
  end;
  // LQuery independently retains [3,1].
end;
```

Arbitrary existing `TList<T>`/queue/stack types use the generic capture factory.
New native code can opt into RTL-derived `TNyxQueryList<T>`, `TNyxQueryQueue<T>`
and `TNyxQueryStack<T>` for generic `.AsNyx` method syntax. These preserve RTL
container ownership; they do not form another UI toolkit. Native-only facilities
are conditional, while owned portable materialized containers run on both targets.
Existing named integer/text array helpers and the generic From(array) factory
remain available. `TNyxStrings.AsNyx` adds both-target exact text capture through
owned portable text storage, avoiding ANSI TStrings conversion.

Queue capture retains FIFO contents without Dequeue. Qualified FPC stack native
enumeration is bottom-first; stack `.AsNyx` defaults to top-first pop order without
Pop/Push mutation. `nsoNativeEncounter` explicitly selects native enumeration;
`nsoPopOrder` selects pop order through `FromStack`. This is qualified against
the installed RTL, not an assumption about every compiler version. Actual control
pipelines materialize parent counts, allowed-key sets and rows before complete
revision-aware parallel collection publication.

## Fluent worker query programs

`nyx.linq.pipeline` supplies immutable `INyxParallelProgram<TInput,TOutput>`
recipes. `TNyxParallelSource<T>.From(Source)` retains a deferred source without
evaluating it. Where, same-type Select and WithOptions return independently owned
programs. Each admitted partition receives a fresh entire callback chain; stages
execute in row order under the explicitly selected worker or deferred-UI policy.
A filtered row never invokes any later projection. Ordinary callback/managed/raw
object ownership and thread-safety contracts still apply.

```pascal
LProgram := TNyxParallelSource<TOrder>.From(LOrders)
  .Where(TNyxPartitionPredicate<TOrder>.From(IsReady))
  .Select(TNyxPartitionSelector<TOrder, TOrder>.From(PrepareOrder))
  .WithOptions(TNyxParallelOptions.Defaults
    .Concurrency(4).PartitionSize(128));

LRun := LProgram.Submit(LScheduler, LReceiver.Port);
```

Global function factories create fresh adapters. State-bearing applications
implement `INyxPartitionPredicateFactory<T>` or the existing typed selector
factory to supply independently owned per-partition state. Factories run on UI
before parallel callbacks; using an interface does not make shared mutable state
thread-safe. Browser required workers still refuse explicitly. Selecting
`npxDeferredUI` is an intentional event-loop alternative.

Type-changing stages use the typed factory because Pascal has no generic methods:

```pascal
LDescriptions := TNyxParallelMap<TOrder, TOrder, TNyxText>.From(LProgram,
  TNyxPartitionSelector<TOrder, TNyxText>.From(DescribeOrder));
LTextRun := LDescriptions.Submit(LScheduler, LTextReceiver.Port);
```

Submit reuses bounded source capture/coordinator options for concurrency, input
limits, ordering, stop/collect diagnostics and cancellation. One complete filtered
vector reaches the ordinary revocable/revision-aware destination on UI. No
partial filtered result is published after a source/factory/stage failure. An
entered row chain completes; cooperative cancellation is checked between input
rows. Worker code never enters the destination port or reads a live collection.
Real control pipelines filter an invalid duplicate candidate on the worker before
its later exact numeric projection and atomic collection admission.

`AsSequential` creates an ordinary independent query with the same stages. Its
callback factory runs on the enumerating caller's thread at first row entry;
empty sources obtain no callback. Cursors each receive fresh mutable stage state,
and EOF/failure promptly retires it. Ordinary ordering/grouping/materialization
can follow this explicit boundary. Program Source/Factory/Options are read-only
typed extension ports; extensions can compose without mutating earlier programs.

This qualifies fused filter/map and sequential boundaries, not operator-wide
parallel grouping/join/ordering/aggregate algorithms, browser worker transport,
streaming merges, Studio authoring, production budgets or installed delivery.
Those remain with the original full LINQ/parallel and scheduler owners.

## Decimal and binary32 precision

`nyx.numbers.decimal` supplies `TNyxDecimalNumber`: an exact 96-bit coefficient,
scale 0..28 and sign, with owned canonical decimal digits. This domain follows
the coefficient/scale shape described by the [Decimal reference](https://learn.microsoft.com/en-us/dotnet/api/system.decimal?view=net-10.0);
Nyx specifies its own portable Pascal operations and explicit rounding contract.
Default/negative zero normalize to zero. `FromInteger` and `FromParts` are typed
authoring entry; `FromDecimal` interprets the existing exact Nyx decimal reference,
including fraction/exponent spelling, without reading it through Double.

```pascal
LPrice := TNyxDecimalNumber.FromParts(0, 0, 125, 2); // 1.25
LTax := TNyxDecimalNumber.FromParts(0, 0, 5, 2);     // 0.05
LTotal := LPrice.Add(LTax);
LAmount.Configure.Text(LTotal.ToText).Done;
```

Add/Subtract, numeric comparison, Quantize and exact ToText/ToDecimal preserve
decimal meaning. `ndrExact` refuses discarded nonzero digits. Explicit alternatives
are nearest ties-even, nearest ties-away-from-zero, toward-zero, signed floor and
signed ceiling. Rounded admission uses original digits once, including all
discarded digits and division remainder; it avoids double rounding. Magnitudes
outside the final coefficient domain refuse. Tiny/excess-scale values can round
under a caller policy without allocating exponent-sized storage.

`nyx.linq.decimal` supplies required/Try/optional Sum and Average plus canonical
comparer/equality/hash. Sum defaults to exact final admission; mean defaults to
nearest ties-even. Wider owned intermediate accumulation lets the mean of two
maximum values succeed, while their unrepresentable sum still refuses. Empty
sum is zero; all-None optional mean is None and sum is Some(0). Present zero stays
present; nil optional interfaces refuse. Count is checked Integer; original source
and decimal failures expose no partial result. Mounted control pipelines publish
exact decimal totals after complete worker computation.

`nyx.numbers.single` supplies the distinct `TNyxSingleNumber` binary32 value.
The installed pas2js compiler marks Pascal Single unsupported and rejects a
generic Single/Double selector, so the portable contract uses an explicitly
narrowed value record. `FromNumber` performs nearest/ties-even binary32 narrowing,
including subnormals. Its Double Value read is exact for an already binary32
value. Finite and IEEE policies are explicit. Pure Pascal power-of-two scaling
and small integer rounding need no DOM/JS/typed-array dependency or ambient FPU
state change; performance budgets remain open.

`nyx.linq.single` supplies required/Try/optional sums and means. Compensated Double
accumulation/stable mean precede final binary32 narrowing. Native code uses
`nyx.numbers.single.native`, `TNyxNativeSingles.AsNyx` and the lossless native
Single bridges. The exported array helper captures independently of later caller
changes. These contracts complete the currently supplied classic Integer/
signed64/Double/decimal/binary32 domains; they do not claim all unsigned/generic-
math/custom numeric domains, production budgets or full parallel/Studio delivery.

## Predicate terminals and exact signed 64-bit values

Sequences provide `Count(Predicate)`, `First(Predicate)`, `Last(Predicate)`,
`Single(Predicate)` and their Try variants. They retain ordinary short-circuit,
absence, cardinality and original callback failure behavior. Scoped nested and
qualified inline adapters keep their lifetime contract. Nil predicates refuse.

```pascal
LFirst := LOrders.First(LReady);
LReadyCount := LOrders.Count(LReady);
LLast := LOrders.LastOrDefaultWhere(LReady, LFallback);
LAscending := LOrders.Order(LComparer);
LDescending := LOrders.OrderDescending(LComparer);
```

Predicate default calls deliberately use the `FirstOrDefaultWhere`,
`LastOrDefaultWhere` and `SingleOrDefaultWhere` names, with ordinary initialized
or explicit typed fallbacks. This keeps a predicate implementation usable as an
interface-valued fallback without selecting a different overload. Multiple
matches still refuse Single, including its fallback form. Whole-value ordering
shortcuts use the existing stable independent typed recipes; key-based ordering
continues through `TNyxKeyOrder` and its explicit direction.

`nyx.numbers.integer64` provides the exact portable `TNyxInteger64` value. Default
is zero; `FromInteger` and typed `FromParts` construct the signed 64-bit domain.
Checked Add/Subtract, CompareTo, decimal ToText and ToDecimal preserve every bit
above 2^53 on both targets. Negative zero normalizes to zero; signed overflow
refuses regardless of compiler switches. TryInteger narrows only when exact.

```pascal
LAmount := TNyxInteger64.FromParts($200000, 1); // 9007199254740993
LAmount := LAmount.Add(TNyxInteger64.FromInteger(3));
LExactData := NyxData(LAmount.ToDecimal);
LDisplay.Configure.Text(LAmount.ToText).Done;
```

Parse is an explicit strict integer-text interchange boundary, not floating
numeric conversion: fractions, exponent notation, whitespace, plus signs,
leading zeros and out-of-domain values refuse. `ApproximateDouble` is explicitly
approximate; exact data/text remain separate. The signed minimum enters and
formats without negating an overflowing native value.

`nyx.linq.integer64` supplies streaming exact Sum, explicit approximate required/
Try means, optional sums/means, and immutable signed comparer/equality/hash
factories. Empty sum is zero; all-None optional sum is Some(0), mean is None.
Present zero remains present; nil optional interfaces refuse. Original failures
and checked overflow propagate without admitting a partial result.

Native code imports `nyx.numbers.integer64.native` for lossless Int64 bridges
and the exported `TNyxNativeIntegers64` array's `.AsNyx` helper. Capture detaches
values before caller mutation/disposal. The browser uses the portable exact
representation instead of pretending its numeric Int64 is lossless. Actual
parallel workers create owned exact vectors; mounted LCL/browser table text
retains 9007199254740996 after publication. Single/decimal/unsigned/wider numeric
domains, full performance/parallel/Studio authoring and installed delivery remain
in the original task.

## Combining sequences and typed runtime queries

`nyx.linq.combinations` provides two-input `TNyxZip`, pair-producing
`TNyxZipPairs`, three-input `TNyxZipThree` and triple-producing `TNyxZipTriples`.
Typed result selectors receive immutable reference-counted pair/triple contexts;
ordinary, named nested and qualified inline adapters work without untyped casts.
`TNyxIndex<T>.From(Source)` yields immutable `INyxIndexedValue<T>` contexts with
independent checked zero-based ordinals. Retained contexts preserve managed values
after source/query/cursor retirement; raw objects remain borrowed.

```pascal
LPairs := TNyxZipPairs<TOrder, Integer>.From(LOrders, NyxRange(1, 100));
LIndexedOrders := TNyxIndex<TOrder>.From(LOrders);
```

Construction and cursor creation enter no input. Zip moves left-to-right and
stops at the shortest sequence. If a later input ends, earlier inputs have moved
once on that terminal attempt. An empty earlier input never obtains a later
cursor, and an unmatched row never calls the result selector. This follows the
corresponding-row contract in the [standard Zip reference](https://learn.microsoft.com/en-us/dotnet/api/system.linq.enumerable.zip?view=net-10.0),
with Pascal-owned managed contexts. Nil contracts refuse; original failures are
terminal and promptly retire every input.

`nyx.linq.runtime` adds `TNyxClassQuery<T>.OfType/Cast` over borrowed `TObject`
inputs and `TNyxInterfaceQuery<T>.OfType/Cast` over managed `IInterface` inputs.
OfType excludes nil and incompatible capabilities. Cast retains typed nil but
raises `ENyxQueryCast` on an incompatible row. Both defer source entry and avoid
implicit numeric/text conversion. Interface methods remain specialized and
reference-counted after filtering.

Declare interface GUIDs for portable runtime membership. Native FPC needs a
declared nonzero GUID; pas2js can also provide compiler-generated runtime-local
identities. `TNyxInterfaceQuery<T>.Supported` exposes this capability without
entering any source/callback. Generated identities are not portable persistence
or cross-process identities. Unsupported targets refuse construction.

For records and application-defined conversions, `TNyxTypeQuery<TSource, TResult>`
accepts an owned selector returning `INyxOptional<TResult>`. Some is compatible,
including present zero/nil/empty text; None is incompatible. OfType skips None;
Cast refuses it. A nil optional interface is an invalid converter contract.
Nested leases, interface captures and original callback exceptions keep their
ordinary semantics. Actual table pipelines validate typed records this way before
complete parallel publication.

## Typed positions, slices and caller randomness

`nyx.linq.positions` uses `TNyxPosition.FromStart(Offset)` and
`TNyxPosition.FromEnd(Distance)`. Offsets must be nonnegative; factory creation is
required. FromEnd(1) means the last element and FromEnd(0) is the exclusive end,
absent as an element without source entry. `TNyxPositions<T>` provides required,
Try and explicit-fallback access. Start access streams; end access consumes the
whole input while retaining at most the requested actual-row tail.

```pascal
LLast := TNyxPositions<TOrder>.ElementAt(LOrders, TNyxPosition.FromEnd(1));
LRecent := TNyxSlice<TOrder>.Between(LOrders,
  TNyxPosition.FromEnd(10), TNyxPosition.FromEnd(0));
```

Slices are deferred half-open intervals with boundaries clamped to the actual
source length. Start-only bounds stream and avoid the unused tail. End-relative
slices admit an independent complete owned vector before yielding; failure
exposes no partial slice. Known empty/reversed intervals enter no source. Huge
offsets do not preallocate huge vectors. Exact text/managed values retain normal
ownership. Production budgets and wider streaming/parallel preparation remain
in the original LINQ task.

`nyx.linq.random` supplies `TNyxShuffle<T>.From(Source, RandomFactory)`. Each cursor
admits a detached vector before requesting an independently owned `INyxRandom`
and applies Fisher-Yates with checked exclusive bounds. Source/factory/RNG
failures stay original and terminal; empty/single rows need no generator.
The caller's input and global RTL random state remain unchanged.

```pascal
LShuffled := TNyxShuffle<TOrder>.From(LOrders, NyxSeededRandom(42));
```

`NyxSeededRandom` gives independently restarted xorshift32 generators with
rejection sampling and identical native/browser golden results. Zero maps to a
fixed nonzero seed. This is repeatable application randomness; cryptographic or
other application randomness uses a caller-defined `INyxRandomFactory`.
`NextBelow` must return within its positive exclusive bound. A factory must not
share mutable generator state between consumers without caller coordination.

## Terminal feedback without blocking the UI

`TNyxQueryObservation.Observe` consumes either a serial or parallel run and
delivers one copied `TNyxQueryOutcome` through a normal result port on UI. The
outcome contains a terminal enum phase and exact portable diagnostic text,
including an empty diagnostic. Even an already-terminal run delivers later.
The observer polls through the scheduler's optional delayed UI capability,
using `TNyxQueryObservationOptions.Defaults.CheckEvery(16)`; callers may choose
1..60000 milliseconds. This is an earliest scheduling opportunity, not a deadline.
Only one delayed ticket is retained, and there is no blocking wait or busy loop.

The returned `INyxQueryRun` describes observation delivery. Its Succeeded phase
means the outcome port accepted the value, even when the observed query failed.
Cancel on this receipt stops observation only; cancel the original run separately
to stop computation. A generation or a revocable method receiver suppresses late
feedback. Port exceptions fail only the observation receipt. Nil arguments,
invalid intervals and missing delayed-UI capability refuse before admission.
Shutdown releases skipped queued observers through ordinary UI-owned teardown.
Receipt terminality does not prove worker retirement: cancellation is cooperative,
entered callbacks may still finish, and later diagnostics remain on the original
run. Observation never joins work or promises to roll back an entered receiver.

Applications can reuse `nyx.application.queryfeedback` to update a bound status
label without writing another callback class:

```pascal
LFeedback := NyxQueryFeedback(LContext, NyxTextState('summary'),
  NyxIntegerState('query-request'), LRequest,
  TNyxQueryFeedbackText.Defaults.Cancelled('Stopped safely.'));

LObservation := TNyxQueryObservation.Observe(LRun, LContext.Scheduler,
  LFeedback, TNyxQueryObservationOptions.Defaults, LContext.Generation);
```

The text/request slots must already exist with their exact Pascal types. The
port captures the mounted generation and expected request. Active publication
runs on UI; retired views/contexts and superseded requests return False before
state entry. Failure and cancellation update only the selected text slot.
Success/discard preserve result-port messages, including stale-source explanations.
Fluent FailurePrefix, EmptyFailure and Cancelled text allow caller localization
and presentation. The port retains a context lease, generation and scheduler,
with no widget/document/store ownership or cycle back into the view.

The English workbench is regenerated through authenticated semantic import and
paired history, then executed with actual browser/LCL bound labels. Controlled
terminal receipts qualify error/cancellation presentation separately from its
real parallel calculation. Detailed partition error UI, explicit cancel controls,
production budgets and observing-release installation remain open.

## Nested and inline callbacks

Include `nyx.callback.features.inc` in each application unit using inline
syntax, and add Nyx's source directory to the compiler's include path (`-Fi`).
The include checks `FPC_FULLVERSION` and `PAS2JS_FULLVERSION`, enabling inline
syntax at 30301 or newer. Both installed 3.3.1 compilers have executed closure
and lifetime qualification; a version gate does not qualify every development
snapshot. Older versions retain
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

The maintained callback boundary fixture additionally returns predicate,
selector, procedure and comparer adapters from a routine that has already
returned. All four refuse entry before touching its departed frame. Nil
callbacks and missing/closed leases refuse at construction; escaped owning
procedure/comparer references retain shared scalar captures. These checks run
on both targets and in the explicitly inline-disabled consumer. Disabling
inline syntax does not disable the typed named nested procedure adapters.

The maintained `linq` build target compiles the shared consumer for native FPC
and pas2js and executes its native checks. Use a new evidence directory for each
run:

```powershell
./tools/build.ps1 -Target linq -LinqEvidenceDirectory build/callbacks/default
./tools/build.ps1 -Target linq -LinqDisableInlineCallbacks `
  -LinqEvidenceDirectory build/callbacks/fallback
```

The second command passes `NYX_DISABLE_INLINE_CALLBACKS` to both compilers;
nested predicate, selector, procedure and comparer checks still execute. Each
packet includes a matched `browser/linq.html` host. Execute that page over HTTP
and require its `data-nyx-linq="passed"` result to establish browser behavior.
Compiling the host alone does not do so. This fallback run uses the selected
compiler and does not establish compatibility with an untested older release.

## Exact wide counts

`nyx.linq.counts` provides `TNyxCounts<T>.LongCount(Source)`, its owned predicate
variant, and `TryGetNonEnumeratedCount`. `TNyxQueryCount` represents a nonnegative
signed-64-domain count with two exact Cardinal limbs. This avoids imposing the
browser's unsafe numeric representation above 2^53 on portable callers.
`ToText` formats all decimal digits exactly; `TryInteger` narrows only when safe,
and `Incremented` refuses overflow at 9223372036854775807.

```pascal
LTotal := TNyxCounts<TOrder>.LongCount(LOrders, LAccepted);
LTotalLabel.Configure.Text(LTotal.ToText).Done;

if TNyxCounts<TOrder>.TryGetNonEnumeratedCount(LOrders, LTotal) then
begin
  LTotalLabel.Configure.Text(LTotal.ToText).Done;
end;
```

The metadata query obtains no enumerator and enters no executable callback.
Arrays, ranges and RepeatValue/Iterate expose their known counts through optional
`INyxSequenceCount`; filtered/custom sources can refuse without evaluation.
False clears the output even when a custom provider writes a candidate first.
Nil inputs/cursors refuse. LongCount itself enumerates one independent cursor,
preserves original callback failures and grants no mutation authority. Actual
LCL/browser pipelines count detached complete rows before parallel publication.

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
can undo a receiver that has already committed. Closure or generation cancellation
during validation refuses the result before installation. Final admission uses
captured noncalling readiness, so it never asks an application getter to restore
withdrawn authority. Cancellation after that admission does not roll back commit.

Alternative stores must expose `INyxAtomicCollection`, whose preparations expose
`INyxPublicationReadiness`; supplied generations must expose
`INyxCancellationReadiness`. Missing capabilities refuse explicitly. Nyx's owned
implementations supply these facets. Readiness checks must inspect owned flags
without application callbacks. The complete result retains ordinary `Assign`
semantics: a changed dataset replaces rows, including their incarnation receipts.
This receiver does not infer an incremental merge from repeated IDs. Use ordinary
revision-checked collection edits when the producer knows stable-row changes.

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

Parallel reductions combine independently owned partition states, then deliver
one complete typed result:

```pascal
LSum := TNyxParallelNumeric.IntegerSum
  .WithOptions(TNyxParallelOptions.Defaults.PartitionSize(256).Concurrency(4));

LRun := LSum.Submit(LValues.AsNyx, LScheduler, LIntegerReceiver.Port, LGeneration);
```

Import `nyx.linq.reductions`. `TNyxParallelCounts<T>.LongCount` supplies an exact
two-limb count plan. `TNyxQueryCount.Added` combines counts without losing values
above 2^53. IntegerSum explicitly checks every partial/final addition on both
targets; a partial overflow refuses even if another grouping could fit. Other
numeric laws/rounding and domain summaries use typed custom reduction/fold
contracts without converting through untyped numbers.

`TNyxParallelReduction<T,TPartial,TResult>.Submit` accepts eager or deferred
inputs, an `INyxPartitionAccumulatorFactory<T,TPartial>`, an
`INyxParallelMergerFactory<TPartial,TResult>`, scheduler, result port, options and
optional generation. Factories enter on UI after full bounded source admission.
Each partition owns its accumulator; Add receives original input ordinals and
cancellation context. Finish returns one complete owned partial after its rows
succeed. Intermediate results use O(partition-count) states; source capture remains
a bounded owned vector.

After every successful reply, a fresh UI-created merger executes under the SAME
worker/event-loop policy as the accumulators. Merge receives partials in source
order or UI-observed completion order. Expensive merging therefore runs off UI
when workers were selected. One result then enters ordinary revocable delivery.
Source/factory/capacity/partition/merge errors and cancellation publish no partial
result. Stop/collect retain owned diagnostics; final merge errors have no partition
diagnostic. Empty input enters no accumulator, creates one merger and supplies an
empty partial vector. Required browser threads refuse even for empty input;
DeferredUI is explicit. No blocking UI join is needed.

`TNyxProgramReduction<TInput,TOutput,TPartial,TResult>.Submit` fuses a program's
Where/Select/type-changing stages before accumulation. Filtered rows never enter
Add and input ordinals remain exact. Its options govern the run; a completely
filtered partition still finishes its own seed. Earlier programs stay unchanged.

`TNyxParallelFold<T,TAccumulator>.From` creates an immutable plan from fresh seed,
folder and combiner factories. Seeded copies ordinary Pascal values; From uses
INyxReductionSeed for independent mutable objects. Seeds enter on UI once per
partition and once for merge. Use identity seeds and associative combine laws
when results must be independent of grouping. Shared objects, inner arrays and
callback state are not implicitly cloned/synchronized. Nonassociative operations,
floating-point rounding and checked overflow can depend on grouping/order.
WithOptions creates an independent plan.

Folder/combiner callbacks receive existing immutable INyxFoldValue contexts.
Scoped/compiler-gated selector adapters keep their ordinary lifetime contracts.
The final combiner factory receives (0,0), creating a separate callback chain.
Native compilers can require named aliases for nested generic factory types
inside argument lists; aliases also keep hand-authored signatures readable.

The maintained `-LinqReductions` option mounts the exact semantic companion in
ordinary application hosts. HTTP `linq-reductions.html` reports
`data-nyx-reductions="passed"` and explicit disposal. Native gates force reverse
completion and a running merger during application disposal. Managed ownership,
4,097-row input/short tail, configured order, exact supplementary text, native
merging, fused sums/counts, seeds, failures/overflow and one admitted UI update
execute. These assertions establish no general latency/memory budget or complete
parallel operator breadth. Wider numeric/quantifier policies, Studio authoring,
browser workers, other widgetsets, budgets and installed delivery remain open.

Parallel programs also have a terminal action operation:

```pascal
procedure ProcessItem(const AValue: Integer; ASourceIndex: Integer;
  const AExecution: INyxExecution);
begin

  if AExecution.Cancelled then
  begin
    Exit;
  end;
  { Work with owned data. Use Scheduler.PostUI(OwnedWork, AExecution) for UI. }
end;

LRun := LProgram.ForAll(
  LScheduler,
  TNyxPartitionAction<Integer>.From(ProcessItem),
  LGeneration);
```

`TNyxParallelForAll<T>.Submit` also accepts a typed array or deferred sequence,
an `INyxPartitionActionFactory<T>`, scheduler, options and optional generation.
Factories enter on UI after complete bounded source admission. Each partition
receives an independent owned `INyxParallelAction<T>`; custom reference-counted
implementations can retain independent state or explicitly synchronized shared
state. Program Where/Select/type-changing maps execute before the action; filtered
rows never enter it. ASourceIndex is the original input ordinal through those
stages, rather than the filtered output position.

The shared coordinator runs actions without allocating per-partition or merged
result vectors. Input capture remains bounded owned data. Callback order inside
each partition follows the input; order across concurrent partitions is unspecified.
Result-order options do not serialize side effects. Workers requires actual native
threads; DeferredUI chooses the event loop explicitly on either target. Empty input
creates no action, while required-worker capability still validates before admission.

Actions commit their effects individually. Cancellation and stop/collect failures
cannot roll back an entered action. Stop suppresses later admission after the first
observed failure. Collect continues other partitions and keeps each failed
partition's first diagnostic; that partition stops at its failing row. Source or
input-limit admission failures enter no actions. Ordinary INyxParallelRun retains
the exact phase/failure/partition diagnostics after owned work retires.

Each action receives an owned INyxExecution whose Cancelled observes its query and
mounted generation, and whose Cancel cancels the whole run. Use that context as
PostUI's parent so child work also retires after navigation. Workers may not read
or mutate live controls/stores. Posted work should own exact inputs and a revocable
receiver; direct receiver entry remains protected by its own generation. A terminal
success means all actions finished and UI observed their partition replies. Child
work has its own execution/diagnostics; observe those tokens if the application
requires their completion. No blocking wait/join is introduced.

Named procedures use From; named nested procedural variables use FromNested with
their enclosing INyxCallbackLease. Construction, factory entry and every invocation
check the lease before static-link access. Close it in finally before leaving the
frame. A live native nested frame refuses entry from another thread. Compiler-gated
FromInline owns its function reference; pass NyxCapture for interface captures on
both targets. Fresh wrappers still share lexical captures, which must be immutable
or synchronized for worker execution. NYX_DISABLE_INLINE_CALLBACKS preserves the
ordinary and scoped APIs. Owned action interfaces remain available without inline
syntax. Include nyx.callback.features.inc in caller units using inline syntax.

The maintained -LinqActions build option exercises these contracts through an exact
semantic companion and ordinary application hosts. HTTP linq-actions.html reports
data-nyx-actions="passed" and explicit disposal. These consumers qualify actual
native overlap, filtered/type-changing actions, individual admitted UI updates,
stop/collect diagnostics, owned action retirement, context cancellation, expired/
foreign-thread nested entry and escaped interface captures. Workload/latency
budgets, broader parallel operators, browser workers, Studio authoring and installed
delivery remain requirements of the full task.

Page-owned work can follow the same generation as its mounted event router:

```pascal
LGeneration := NyxEventGeneration(LApplication.View.Events);
LReceiver := NyxCollectionQueryReceiver(
  LScheduler,
  LApplication.Collections.Collection(NyxCollection('tasks')),
  LGeneration);

LRun := LProgram.Submit(LScheduler, LReceiver.Port, LGeneration);
```

Import `nyx.events`, `nyx.linq.execution`, `nyx.linq.publication` and the desired
query/program units. Capture after the page is mounted. The ordinary query and
both parallel input overloads accept the same optional `INyxCancellationScope`.
`TNyxQueryReceiver<T>.Bind` also accepts it for a borrowed method receiver. Pass
the generation to both submission and receiver: submission stops obsolete work,
while the receiver protects even direct extension calls to its retained port.
The generation contains no page, renderer, document, scheduler or receiver.

Successful replacement, navigation, unmount and application disposal cancel the
existing event generation before borrowed controls retire. Unknown-page refusal
preserves the accepted generation. Fresh page mounts receive independent tokens;
old work cannot enter a new view. Cancellation is observed at existing computation
and UI-delivery checkpoints, and through receipt Phase reads. An entered callback
can finish; later rows/factories and delivery refuse. A completed receipt preserves
its terminal history. This adds no blocking wait/join or ownership cycle.

Nil deliberately leaves application-wide background work independent of navigation.
Close still handles a receiver that retires earlier than its page. Retained leases
must be released on UI. Alternative event routers opt into INyxEventGeneration;
missing/unsupported/closed routers refuse capture. Existing INyxEvents stays intact.

The MCP companion tool's optional `navigation` argument composes a second English
page in the same revision-aware paired transaction. The maintained build option
`-LinqNavigation`, together with its exact two-page `-LinqSourceDirectory`, executes
the real native application host and builds the matched browser host. HTTP
`linq-lifetime.html` qualifies `data-nyx-lifetime="passed"` and explicit disposal.
Current-source checks cover queued/awaiting/capturing work, eager parallel inputs,
fresh mounts, unchanged results and retained collection/method ports after
disposal. Native gates establish an actual entered worker without blocking UI.
Broader operator preparation/token composition, Studio authoring, other
widgetsets, browser workers, budgets and installed delivery remain open.

## Parallel numeric plans

`nyx.linq.numeric.parallel` supplies immutable plans for checked Integer/exact
signed64 sums, stable Integer/Double means, explicitly approximate signed64 means,
compensated Double sums, binary32 sums/means and exact decimal sums/means. Public
factory classes are `TNyxParallelInteger`, `TNyxParallelInteger64`,
`TNyxParallelNumber`, `TNyxParallelSingle` and `TNyxParallelDecimal`.

```pascal
LMean := TNyxParallelNumber.Average(nnpFinite, nmaOptional)
  .WithOptions(LOptions);
LReceiver := TNyxQueryReceiver<INyxOptional<Double>>.Bind(
  LScheduler, ReceiveMean, LGeneration);

LRun := LMean.Submit(LNumbers, LScheduler, LReceiver.Port, LGeneration);
```

Capture detached inputs/scheduler/view generation on UI. The receiver accepts
one completed typed result on UI, and its lease must retire before the borrowed
method owner. `nmaOptional` produces None for empty/all-absent means;
`nmaRequired` fails the receipt without publication. Present zero is a value.
Sums use typed zero identities. Finite policy refuses nonfinite values/overflow;
IEEE is an explicit caller choice. No global FPU/codepage state changes occur.

Integer/signed64 partial additions are checked, including final merge. A
partition can refuse overflow even when another grouping could fit. Mean
computation is separate: weighted partial means avoid sum overflow and unsafe
opposite-sign subtraction. Double sum uses compensation per partition and
merges both sum and correction. Binary32 inputs accumulate in Double, then
narrow once at final admission. Binary rounding depends on partition/order;
these plans do not promise bit-for-bit serial equivalence for every grouping.

Decimal plans retain `TNyxDecimalAccumulation` as detached exact wide state.
Partial totals can exceed decimal96; only the final Total/Average rounds/adopts
under the selected policy. Thus two maximum decimals can average successfully.
Snapshots survive builder edits/destruction; merges preserve both inputs and
refuse count overflow atomically. Explicit Empty and Default represent the
empty identity. The ordinary decimal accumulator can Snapshot and Merge the
same contract; intermediate precision is never silently narrowed.

`INyxNumericKernel<T, TPartial, TResult>` lets custom domains supply Empty,
Add, Merge and Finish. A plan owns an immutable/synchronized kernel; each
partition and final merger gets independent state. Empty runs during UI
partition construction and again under the merge policy, so it must also be
independent of UI/store state. Mutable reference-valued states must be fresh;
an interface does not imply cloning or thread safety.

`TNyxNumericProgram<TInput, TValue, TPartial, TResult>.Submit` composes an
immutable filter/map program with a plan. The plan's explicitly configured
options govern that run; the original program stays unchanged.
`TNyxNumericOptionals<T, TPartial, TResult>.IgnoreAbsent(Plan)` skips explicit
None, refuses nil optional interfaces and preserves the original result type,
options, empty laws and policies. Optional sums remain scalar zero on all-None
input with this explicit adapter; means already return typed optional results.

All plans reuse complete bounded source capture, owned partials and a final
worker/event-loop merge. Stop/collect, source/capacity failures and cancellation
publish no partial result. Required workers refuse on browser; choose DeferredUI
explicitly. `CaptureRowsPerTurn` trades capture throughput against UI turn size;
it cannot preempt an arbitrarily expensive source callback. Checked harness
measurements for 128/8192/131072 rows separate capture from work/delivery, with
one partial per 2048 rows. They are not release latency/frame/memory budgets.
Native delayed capture/polling dominates the default large sample; both default
and caller-tuned measurements remain in the work record rather than concealing
the slower path. Streaming capture and complete production budgets remain open.

`-LinqNumeric` builds/runs the maintained actual-control consumer with an exact
semantic source. HTTP `linq-numeric.html` reports `data-nyx-numeric="passed"`,
stage/row phase timings and explicit disposal. Complete regressions include
the unchanged serial precision paths and prior action/terminal/query consumers.

## Bounded streaming input

Deferred parallel submissions now accept the typed
`TNyxParallelOptions.Input(npiStreaming)` policy. Snapshot remains the default:
it finishes UI enumeration before admitting a partition. Eager open arrays are
already captured and ignore this deferred-sequence policy. Earlier options and
programs remain immutable when another policy is selected.

```pascal
LOptions := TNyxParallelOptions.Defaults
  .Input(npiStreaming)
  .Concurrency(4)
  .PartitionSize(512)
  .CaptureRowsPerTurn(128)
  .RetirementCheckEvery(16);

LPlan := TNyxParallelInteger.Sum.WithOptions(LOptions);
LRun := LPlan.Submit(LInput, LScheduler, LReceiver.Port, LGeneration);
```

The source cursor stays on UI. Each turn advances at most the capture quantum;
complete detached partitions can enter workers before source EOF. Full worker
slots pause enumeration. A reply schedules another independent bounded turn,
avoiding an input-sized cancellation chain or busy capture polling. Input storage
is bounded by the admitted partitions plus one partition-sized UI buffer. Complete
projection output still grows with rows; numeric/keyed reductions retain their
ordinary compact partials. The configured total input limit still applies.

Source/completion order, stop/collect diagnostics and final partial partitions
retain the existing contracts. EOF and every successful reply are required before
the one completed result or final reduction merge is admitted. A source Current/
MoveNext/capacity failure after earlier worker entry cancels the run and publishes
nothing partial. Streaming explicitly permits worker/action side effects before
later source failure; those effects are not rolled back. Choose snapshot when
source admission must finish before any callback enters.

Source preparation and arbitrary MoveNext/Current callbacks cannot be preempted.
An upstream whole-input operator such as sorting keeps its own preparation
barrier. This policy supplies bounded producer/worker overlap, not unbounded
infinite inputs or partial UI delivery. Browser DeferredUI remains event-loop
execution; required worker mode still refuses there.

Streaming admits both `INyxSchedulerTurns` and the optional
`INyxSchedulerDelays` capability. Delays preserve existing scheduler interfaces:
`PostAfter(Work, Milliseconds)` is UI-only, accepts 1..60000 ms and gives an
earliest scheduling opportunity rather than a deadline. Missing capabilities
refuse explicitly; ordinary snapshot adapters remain usable. Pending cancellation
skips work entry while its UI courier still releases owned work on UI.

An independent delayed owner keeps an open source off worker teardown, even when
navigation cancels the query before a worker returns. It performs only retirement
checks, never source enumeration. `RetirementCheckEvery` selects 1..1000 ms,
default 16. Its ticket intentionally survives query cancellation; scheduler
shutdown still skips/releases it on UI, where its owned teardown clears the source.
This prevents an entered/cancelled worker from destroying the last UI cursor.
Host scheduling controls actual cancellation/cleanup latency.

`-LinqStreaming` adds the maintained actual-control consumer with an exact semantic
companion. `linq-streaming.html` reports `data-nyx-stream="passed"`, per-stage
elapsed/first-worker-read receipts and explicit disposal. Checked native and HTTP
browser cases cover bounded capture, pre-EOF entry, empty/tail input, late source/
factory/worker failures, compact numeric/keyed/program results, navigation and
shutdown. Native gates prove full-slot backpressure and reverse completion.
Matched 8192-row snapshot/streaming measurements remain development/harness
evidence, not release/frame/paint/peak-memory budgets. Broader streaming operator
strategies, browser workers, Studio authoring and delivery remain under the task.

## Parallel grouping and per-key summaries

`nyx.linq.grouping.parallel` composes owned keyed plans with the same bounded
worker coordinator, failure policies and generation-bound UI publication.
`TNyxParallelGroupBy<T, TKey, TElement>.From` accepts independent key/element
selector factories and an equality factory. Each row projects each key/element
once. The completed sequence owns immutable `INyxGroup<TKey, TElement>` values;
retained groups survive source, plan and application disposal.

`TNyxParallelCountBy<T, TKey>.From` returns checked Integer counts;
`LongCounts` returns exact `TNyxQueryCount` values. `TNyxParallelAggregateBy`
accepts an `INyxNumericKernel` for compact per-key sums, means or custom state.
Count/aggregate partials retain distinct keys and states instead of row-result
vectors. GroupBy necessarily retains all projected elements. These bounds do
not remove the coordinator's owned input capture.

```pascal
LCounts := TNyxParallelCountBy<TOrder, TNyxText>.From(
  LCustomerKeys, LCustomerEqualities).WithOptions(LOptions);
LReceiver := TNyxQueryReceiver<TCustomerCounts>.Bind(
  LScheduler, ReceiveCustomerCounts, LGeneration);
LRun := LCounts.Submit(LOrders, LScheduler, LReceiver.Port, LGeneration);
```

Here `TCustomerCounts` is the named alias for
`INyxSequence<INyxKeyValue<TNyxText, Integer>>`. Named aliases keep native
generic parsing and application declarations readable. Capture detached inputs,
scheduler and mounted generation on UI; retain the receiver lease until work
retires and revoke it before its borrowed method owner leaves.

Source order preserves ordinary first-key representation, group encounter order
and element order. Explicit completion order uses the first observed partition
for equivalent-key representation and follows that partition order for values
and folds. It is intentionally different from source order. Empty input publishes
one owned empty sequence without entering partition callbacks. Overflow, source,
key/hash/kernel failure and cancellation publish no partial sequence.

`INyxParallelEqualityFactory<TKey>` creates a fresh owned equality on UI for
each partition and final merge (`-1, 0`). Returned equalities execute under the
selected worker/event-loop policy and must share the same equivalence/hash law.
Equal keys must have equal hashes; keys and laws stay stable while indexed.
`TNyxParallelEquality` supports named functions, lease-guarded nested functions,
compiler-gated inline functions with `NyxCapture`, and explicit `SharedImmutable`.
Factories may capture UI-readable configuration when constructing detached
callbacks; returned callbacks must not read live UI/stores. Neither fresh wrappers
nor interface reference counting imply thread safety for shared mutable captures.

`INyxKeyedKernel<T, TKey, TState, TResult>` supplies per-key Empty/Add/Merge/Finish.
Its factory creates independent partition/final callbacks on UI; all four kernel
operations execute under the chosen policy. Empty creates independent mutable
state, Merge treats completed partial states as sealed, and Finish returns an
owned result. Associative identity and isolation laws remain caller contracts.
The numeric adapter shares an immutable/synchronized numeric kernel while giving
every new key its own state. `TNyxProgramKeyed.Submit` accepts filter/map/type
changes; the keyed plan's options govern without mutating the reusable program.

`-LinqGrouping` adds the maintained actual-control consumer. Its HTTP host
`linq-grouping.html` reports `data-nyx-keyed="passed"`, check count and explicit
disposal. Both-target tests cover nested/escaped inline factory lifetimes,
collisions, exact Unicode/NUL keys, compact summaries, failure/cancellation and
actual table publication. Native tests force reverse completion. A 4096-row,
16-key program retains 128 partial keys and 16 completed values. Streaming,
browser workers, production budgets, broader parallel operators and installed
delivery remain under the original task.

## Parallel quantifiers and extrema

`nyx.linq.terminals` extends the shared worker engine with presence, predicate
and program `Any`/`All`, plus whole-value/projected-key/program extrema. The
reference inventory was checked against Microsoft's
[parallel Any](https://learn.microsoft.com/en-us/dotnet/api/system.linq.parallelenumerable.any?view=net-10.0),
[parallel All](https://learn.microsoft.com/en-us/dotnet/api/system.linq.parallelenumerable.all?view=net-10.0)
and [order preservation](https://learn.microsoft.com/en-us/dotnet/standard/parallel-programming/order-preservation-in-plinq)
contracts on 2026-10-12. Nyx's asynchronous receipts, typed absence and ownership
remain its own portable Pascal contract.

```pascal
// Capture scheduler, generation and independent inputs on UI first.
LMinimumReceiver := TNyxQueryReceiver<INyxOptional<Integer>>.Bind(
  LScheduler, ReceiveMinimum, LGeneration);
LComparers := TNyxParallelComparer<Integer>.From(NewNyxIntegerComparer);

LRun := TNyxParallelValueExtrema<Integer>.Minimum(
  TNyxSequence<Integer>.From([4, 2, 9]),
  LComparers,
  LScheduler,
  LMinimumReceiver.Port,
  LOptions,
  LGeneration);
```

Keep the receiver lease until completion and close it before its borrowed method
receiver retires. `ReceiveMinimum` accepts an array containing one
`INyxOptional<Integer>` on UI. Empty input delivers None; present zero/nil values
remain distinct. Reading an absent Value raises ENyxQueryEmpty. Cancellation or
failure delivers nothing. Required worker capability never silently falls back.

`TNyxParallelQuantifiers<T>.Any` accepts a sequence with or without an owned
partition predicate factory; `.All` accepts one. The program counterparts consume
accepted filter/map outputs and reuse the program's options. Empty Any is False;
empty All is True. Default `nqeDecisive` uses a fresh synchronized monotone decision
per submission. Observed decisions suppress later predicate factory/row entry
and later program row evaluation. Already entered callbacks may finish/fail.
`nqeComplete` evaluates every accepted row unless ordinary failure/cancellation
intervenes. Both modes wait for owned successful replies/final merge before one
UI publication. A logical decision never cancels the query or hides an entered
failure. Stop/collect diagnostics remain ordinary partition diagnostics.

Deferred input still reaches EOF across bounded UI capture turns before workers
start. A decisive result does not bypass source admission/errors or input limits.
Pending partition plumbing/program adapter construction can still run; the
decision suppresses application predicate creation and row evaluation at checked
boundaries. Streaming capture/early scheduling and workload budgets remain open.

`TNyxParallelExtrema<T, TKey>` supplies Minimum/Maximum with per-partition key
selectors and comparer factories. Program extrema preserve original INPUT
ordinals through filtering/type changes. Each accepted row projects its key once;
each worker keeps one candidate slot and emits one immutable partial in Finish.
Final comparison uses the selected worker/event-loop policy. The earliest source
tie wins even in completion order. Comparer signs are tested directly, never
negated/subtracted, preserving Low(Integer). Optional results retain ordinary
managed payload ownership and never reference a mutable candidate.

Comparer factories create independent owned comparers on UI for each partition
and for final merge (-1,0). From accepts a named factory function; FromNested
protects its borrowed frame with INyxCallbackLease; compiler-gated FromInline
owns its function reference and optional NyxCapture. Returned comparers still
need independent worker-safe captures. SharedImmutable is an explicit caller
promise of immutable/synchronized state, not a hidden clone. Both installed
compilers execute escaped factory capture/refusal and inline-disabled paths.

The maintained `-LinqTerminals` option and exact semantic source execute actual
native application controls and build their browser counterpart. HTTP
`linq-terminals.html` exposes `data-nyx-terminals="passed"` and explicit disposal.
The complete LINQ task still owns wider numeric/grouping/streaming policies,
performance, Studio authoring, browser workers and preserving installed delivery.

## Application callbacks and a runnable workbench

New Studio callback stubs derive from `TNyxApplicationEventCallback`. Ordinary
browser/LCL application hosts attach an `INyxApplicationContext` before event
registration. Existing `TNyxEventCallback` implementations remain supported;
other managed bases can implement `INyxApplicationEventCallback`. Standalone
renderer callers may supply an explicit fourth `BindNyxCallbacks` argument.
Unbound access refuses rather than guessing a global application.

`Context.State` is borrowed while its host is active. Collections, Scheduler and
Generation provide typed runtime capabilities. All context access is UI-only:
capture independent inputs, scheduler and generation during Invoke before
starting workers. Attachment precedes the first view mount, so it is too early
to capture that generation. The context retains no application, document,
renderer, router or control. Hosts revoke before teardown/on failed mounting;
retained callbacks refuse before dereferencing a retired host. Last lease release
also revokes on UI. Independent hosts receive separate contexts/runtime stores.

The [task workbench](../examples/query-workbench/README.md) includes its exact
semantic project/source pair and independent native/browser entry. Its generated
button callback calls `StartTaskSummary(Context)`. An immutable predicate and
fresh partition folds calculate exact count/wide points. One owned result port
checks view generation, request version and source revision before updating the
bound label. Native workers and explicit browser deferred execution use the
same portable recipe. Installed MCP compiler diagnostics are kept separate from
current-source application execution.

The native-only extra case covers explicitly numbered enum declarations, which
the installed pas2js compiler refuses before generating a query. Portable enum,
Boolean, Integer, Double, text and record cases run on both targets. This compiler
capability difference remains explicit; it is not counted as browser qualification.
