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

**2026-10-12 deferred pipeline evidence:** Sequence submission retains without
evaluating, captures one independent cursor across cooperative UI turns and
admits the complete bounded input before workers. **108/108** actual native/HTTP
browser checks qualify source failure/limits/lifetime/interleaving and collection
publication, with **4163/4162**, **77/76**, **55/52** regressions and **38** fresh
semantic checks. Optional scheduler turn capability preserves original interface
compatibility and refuses unsupported adapters. [Operator coverage](../docs/linq-coverage.md)
records all remaining named breadth. Three execution batches close no full
criterion; stop their variants and resume indexed/flattening/grouping/join
operator delivery. Full parallel semantics/helpers/Studio/budgets/delivery remain.

**2026-10-12 indexed projection/flattening evidence:** Immutable managed contexts,
independent ordinals, indexed prefix/projection, ordinary/indexed SelectMany and
typed outer/inner result selectors qualify **51** new assertions inside complete
**8339/8335** native/HTTP browser checks. Inline-disabled consumers pass
**8331/8327**; actual duplicate/index-filter pipelines retain **108/108** control
and current partition/delivery/scheduler evidence. The first operator batch closes
no full criterion: grouping/lookups/joins, sets/generation/aggregates/optional/
materialization/helpers and wider parallel/Studio/budget/delivery remain required.
Stop current variants and follow grouping/join delivery next.

**2026-10-12 grouping/join evidence:** Deferred stable grouping, immutable indexed
lookup and inner/group/left/right/full joins qualify **12490/12486** complete
native/HTTP browser checks and **12480/12476** inline-disabled consumers. Hash
collisions/exact Unicode, duplicate multiplicity, explicit optional partners,
managed lifetimes and scoped/inline callbacks execute. **111/111** actual-control
pipeline checks display joined numeric data after parallel publication, with
stale/retired refusal and current regressions. Two operator batches close no full
criterion: stop their variants and follow the original coverage map's remaining
sets/generation/partition/optional/aggregate/materialization/native helpers;
wider parallel/Studio/budget/installed delivery outcomes remain required.

**2026-10-12 sets/membership evidence:** Ordinary/key-based Distinct/Union/
Intersect/Except and immediate Contains/SequenceEqual share typed equality,
independent owned indexes and short-circuit semantics. Managed keys/values,
collisions, exact text and nested/inline callbacks execute **16649/16645** complete
native/HTTP browser and **16638/16634** inline-disabled checks. Actual-control
pipelines deduplicate/intersect row identity keys before parallel publication:
**111/111**, with current delivery/partition/scheduler regressions. This third
family closes no full criterion. Stop its variants and follow generation/
partition/optional/aggregate/materialization/native helpers; wider parallel/
Studio/budget/installed delivery outcomes remain required.

**2026-10-12 generation/partition evidence:** Checked Integer NyxRange, typed
RepeatValue/Iterate, Append/Prepend/default sequences, independent owned chunks,
bounded actual-row tail rings/reversal and typed element fallbacks execute
**16729/16725** complete native/HTTP browser and **16717/16713** inline-disabled
checks. Callback scope/managed retirement and **111/111** actual-control pipelines
retain short-tail chunks before parallel publication, with current regressions.
This fourth family closes no full criterion. Stop its variants and follow
numeric/keyed aggregates, materialization/native helpers and predicate/from-end
convenience; wider parallel/Studio/budget/installed delivery remain required.

**2026-10-12 aggregate evidence:** Typed seeded/unseeded/result folds, complete
indexed CountBy/AggregateBy, stable extrema/key extrema and Integer/Double/
optional numeric contracts execute **20885/20881** complete native/HTTP browser
and **20872/20868** inline-disabled checks. Managed contexts/per-key seeds,
collision/Unicode, number policies/overflow and scoped/inline folders execute.
**111/111** actual-control pipelines display CountBy totals after parallel
publication, retaining current regressions. This fifth family closes no full
criterion. Stop its variants; resume materialization/native helpers, remaining
convenience/wider numeric domains and full parallel/Studio/budget/delivery.
