# Typed deferred sequences

[Fluent API](fluent-api.md) · [Collections](collections.md) ·
[Full LINQ/parallel task](../TODO/NS-1_linq-parallel_01.md)

`nyx.linq` supplies generic, reference-counted sequences for ordinary Pascal
values. `nyx.linq.collections` adds immutable Nyx snapshot entry points. This
implementation supplies the foundation and stable ordering for the user's full LINQ and safe
parallel-execution request; grouping, joins, flattening, set operators,
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
