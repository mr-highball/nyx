# Typed deferred sequences

[Fluent API](fluent-api.md) · [Collections](collections.md) ·
[Full LINQ/parallel task](../TODO/NS-1_linq-parallel_01.md)

`nyx.linq` supplies generic, reference-counted sequences for ordinary Pascal
values. `nyx.linq.collections` adds immutable Nyx snapshot entry points. This
initial implementation is the foundation of the user's full LINQ and safe
parallel-execution request; ordering, grouping, joins, flattening, set operators,
generation, aggregates and parallel/UI publication remain under the full task.

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

| Operation | Behavior |
| --- | --- |
| Where | Deferred filter; repeated calls compose in pipeline order |
| Select | Deferred same-type mapping; the typed factory permits a type change |
| Skip / Take | Nonnegative counts; zero Take does not enter the upstream source |
| SkipWhile / TakeWhile | Prefix partitioning; callbacks stop when the prefix ends |
| Concat | Deferred left-then-right sequence; right enumeration starts after left EOF |
| Count | Full enumeration; Integer overflow refuses |
| Any / All | Short-circuit; empty Any is False and empty All is True |
| First / Last | Empty input raises ENyxQueryEmpty |
| Single / TrySingle | Multiple input raises ENyxQueryCardinality |
| TryFirst / TryLast / TrySingle | False on absence, clearing the output to its initialized value |
| ElementAt | Zero-based access; negative or absent positions refuse |
| ToArray | One enumeration into a separately owned outer vector |

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

The existing scheduler supplies bounded native workers, cancellation and UI
delivery. The parallel query layer must still integrate ordering/failures,
owned inputs, cancellation and revision-aware publication into actual controls.
This generic sequence core does not make a mutable callback or live collection
thread-safe. Browser event-loop scheduling is distinct from real worker support.

The design reference is [ezthreads](https://github.com/mr-highball/ezthreads),
inspected at public commit `a0a391120ba14dd4eb6b3983e874f29336227230`.
Its fluent retained-work, pool and synchronized-completion patterns inform the
new contract; it is not an installed Nyx dependency.

Run `tools/build.ps1 -Target linq` for checked native execution and a matched
browser host. Supply `-LinqSourceDirectory` with the exact semantic mixed
collection companion to include compiled authored-default integration, and
`-LinqEvidenceDirectory` with a new output directory to retain prior evidence.
HTTP execution of `linq.html` separately verifies `data-nyx-linq="passed"`.
The full companion currently executes 92 native / 91 browser checks, including 4,096-row
queries, independent cursors, ownership, exact text/numbers and failure behavior.
Managed interface elements remain alive through source/cursor/reader transitions
and retire exactly once on both targets.
This establishes the deferred foundation, not full LINQ or parallel/UI acceptance.

The native-only extra case covers explicitly numbered enum declarations, which
the installed pas2js compiler refuses before generating a query. Portable enum,
Boolean, Integer, Double, text and record cases run on both targets. This compiler
capability difference remains explicit; it is not counted as browser qualification.
