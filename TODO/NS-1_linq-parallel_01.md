# NS-1_linq-parallel_01 — Typed LINQ and safe parallel query execution

[North stars](../MILESTONES.md) · [Task catalog](README.md) · [Task flow](../TASKFLOW.MD)

**Description:**

Deliver the user's full LINQ-like support, fluent parallel/threading execution,
safe UI/collection interaction and native helper syntax resembling C# extension
methods. Queries cover ordinary typed Pascal values as well as Nyx snapshots;
this is broader than the existing serializable collection filter/sort policy.
Use [ezthreads](https://github.com/mr-highball/ezthreads) as design inspiration,
without installing it as a dependency or modifying its source. NS-1 owns the
portable contract; existing scheduler, NS-2 adapter, NS-3 performance and NS-4
Studio owners retain their integration and quality requirements. Completion
credit remains pending assessment, and a subset of operators earns no full credit.

Starting evidence: generic interfaces compile on both installed compilers;
Nyx already has immutable collection snapshots, typed predicates/sort policies,
cooperative cancellation, a bounded native worker pool and deferred UI delivery.
The 2026-10-12 return path in WORK.md owns implementation and evidence.

**Acceptance Criteria:**

- Generic, reference-counted, deferred/repeatable sequences cover filtering,
  projection (including type changes), flattening, ordering with stable secondary
  keys, grouping, joins/group joins, partitioning, concatenation, set operations,
  generation, element/cardinality operations and typed aggregates. Empty,
  duplicate, overflow, exception and comparer behavior is documented and tested
  on executed browser and checked native consumers.
- Maintain a named operator coverage map against the
  [standard LINQ operator reference](https://learn.microsoft.com/en-us/dotnet/api/system.linq.enumerable?view=net-10.0)
  and [parallel-query semantics](https://learn.microsoft.com/en-us/dotnet/standard/parallel-programming/introduction-to-plinq).
  Include indexed predicates/projections, SelectMany, Chunk, Append/Prepend,
  default/fallback element operations, Contains/SequenceEqual, key-based set
  variants, MinBy/MaxBy, CountBy/AggregateBy, Reverse, Range/Repeat, stable
  OrderBy/ThenBy, typed grouping/lookups/dictionaries and inner/outer/group joins.
  Runtime type filtering/conversion must preserve Pascal typing and target
  capabilities. Numeric/optional values and caller comparers have explicit
  portable semantics; operator overload counts are not completion evidence.
- Native helpers expose natural calls on supported arrays/RTL collections,
  without taking ownership of caller objects or retaining mutable enumeration
  state. Ordinary Nyx snapshot integration preserves identity/revision, exact
  Unicode/numeric values and independent enumeration across later store edits.
- Support the user's named nested procedure/function variables and, when the
  qualified compiler supports them, inline anonymous function/procedure references.
  Compiler-version conditionals and actual probes select syntax; ordinary owned
  interfaces remain available on older compilers. Deferred/parallel consumers
  distinguish borrowed stack frames from owning captures, refuse expired or
  wrong-thread nested entry and qualify escaped closure/interface lifetimes.
- Fluent parallel execution specifies bounded concurrency, ordering, cancellation
  and failure behavior. Real native workers execute independent owned inputs;
  caller-provided callbacks have an explicit concurrency/lifetime contract.
  Browser worker support or unsupported capability is explicit; event-loop
  scheduling never masquerades as parallel threads. Sequential alternatives
  remain available on both targets through the same query semantics.
- UI result delivery uses the scheduler and revocable ownership/generation ports;
  navigation, disposal, cancellation, late results and changed collection
  revisions cannot mutate a retired UI or publish partial/stale collections.
  Joining or awaiting work cannot deadlock the UI. Both-target actual controls
  and collection observers establish these behaviors, beyond worker-only tests.
- Studio has English semantic-MCP-authored query/parallel examples, relevant
  diagnostics and generated/handwritten source interoperability. Both-target
  execution, ownership teardown, measurable large-data workloads, API documentation
  and independently callable examples qualify the complete feature.

**Blockers**:

- [Structured state and owned collections](DONE/NS-1_state-collections_01.md)
- [Specialized managed contracts](DONE/NS-1_component-interfaces_01.md)
- [Events and scheduler contract](NS-1_event-scheduler_01.md) for full parallel/UI acceptance.

**2026-10-12 bounded execution evidence:** `TNyxQuery<T>.Submit` owns computation
and queues complete results through revocable typed receivers. A revision-captured
collection receiver admits a complete candidate and assigns once. **77 LCL / 76
executed browser** actual-control checks and **38** fresh owning semantic source/
history checks qualify this bounded path. Default deferred UI and explicit native
worker execution have distinct capability behavior. Each query enumerates
serially; concurrent partitions/merge/failure policies, running-worker cancellation,
browser worker transport, full operators/helpers and Studio integration remain
required. No full criterion/task/DONE/credit closes. See current WORK.md evidence.

**2026-10-12 partitioned projection evidence:** Typed concurrency/partition/input
limits, source/completion order, stop/collect diagnostics and independently owned
selector factories execute **4,163 native / 4,162 HTTP browser** checks, with
**77/76** receipt/control regressions and **38** fresh semantic source/history
checks. Real native callbacks overlap, short-tail/type-changing results preserve
source order, both failure policies prevent partial publication and cancellation/
capacity/stale/retired control paths pass. Browser chooses deferred UI explicitly;
required workers refuse. Deferred sequence integration, operator-wide parallel
semantics, browser workers, full operators/helpers and Studio/budget/delivery
outcomes remain required. No full criterion/task/DONE/credit closes.
