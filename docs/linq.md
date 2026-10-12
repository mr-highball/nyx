# Typed deferred sequences

[Fluent API](fluent-api.md) · [Collections](collections.md) ·
[Full LINQ/parallel task](../TODO/NS-1_linq-parallel_01.md) · [Operator coverage](linq-coverage.md)

`nyx.linq` supplies generic, reference-counted sequences for ordinary Pascal
values. `nyx.linq.collections` adds immutable Nyx snapshot entry points. This
implementation supplies deferred sequences, stable ordering and owned partitioned
projection for the user's full LINQ and safe parallel request. Grouping, joins,
flattening, sets, generation, aggregates, operator-wide parallel policies and
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
