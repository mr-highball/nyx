# LINQ capability coverage

[API and ownership](linq.md) · [Full acceptance owner](../TODO/NS-1_linq-parallel_01.md)

This map keeps the user's full operator/parallel request visible. Implemented
means the named current-source contract has checked native and executed browser
evidence; it does not mean the complete operator family, Studio integration,
target parity or production budgets are accepted. Partial means other named
variants/outcomes remain required. Open work stays with the full task, not a
competing backlog or additional completion credit.

The naming inventory was checked on 2026-10-12 against Microsoft's
[Enumerable reference](https://learn.microsoft.com/en-us/dotnet/api/system.linq.enumerable?view=net-10.0)
and [parallel-query introduction](https://learn.microsoft.com/en-us/dotnet/standard/parallel-programming/introduction-to-plinq).
Those pages describe .NET contracts; Nyx specifies portable Pascal equivalents,
ownership and explicit capabilities. The reference can include preview entries;
listing a name below does not claim a released .NET version or Nyx support.

| Reference family / named operations | Nyx current source | Remaining acceptance |
| --- | --- | --- |
| AsEnumerable, Empty | Typed arrays/snapshots/owned text entry; detached native RTL enumerable capture, named helpers and generic RTL-derived containers | Broader supported native collections and production budgets |
| Where | Deferred ordinary/indexed predicates; owned/scoped/inline adapters; fused per-partition program filters | Wider parallel filtering/index policies and production budgets |
| Select | Ordinary/indexed same-type and type-changing projections; fused typed worker program maps | Wider parallel projection/index policies and production budgets |
| SelectMany | Owned deferred ordinary/indexed flattening and typed result selectors | Wider streaming/cancellation policies and production budgets |
| Skip, Take, SkipWhile, TakeWhile | Nonnegative and ordinary/indexed prefixes; typed half-open start/from-end slices | Wider parallel policies and production budgets |
| SkipLast, TakeLast | Deferred bounded rings grow with actual rows; independent tail order/ownership | Wider parallel/streaming tail policies and production budgets |
| Chunk | Detached owned ordinary sequences, short-tail handling and deferred source entry | Wider parallel chunk policies and production budgets |
| Append, Prepend, Concat | Deferred insertion/concatenation with owned values; Prepend defers source entry | Wider parallel/streaming policies and production budgets |
| Reverse | Deferred complete admission, independent owned reversal | Parallel reversal policies and production budgets |
| Range, Repeat, Sequence | Checked NyxRange, typed RepeatValue/Iterate; exact both-target generation | Preview generic-math Sequence, wider numeric generation and production budgets |
| Any, All | Ordinary terminals plus owned parallel presence/predicate/program forms; per-run global decisive or complete evaluation, vacuous empty rules and complete UI publication | Streaming capture/early scheduling budgets and broader terminal policies |
| Count, LongCount, TryGetNonEnumeratedCount | Integer/predicate Count; exact two-limb counts, checked wide addition, parallel count plans and optional non-enumerating metadata | Wider performance/parallel quantifier policies and budgets |
| First, Last, Single | Required/Try cardinality and predicate terminals preserve short-circuit/absence/failures | Broader optional/parallel terminal policies and production budgets |
| FirstOrDefault, LastOrDefault, SingleOrDefault, ElementAtOrDefault, DefaultIfEmpty | Typed explicit/ordinary fallbacks; named predicate Where variants avoid interface-payload ambiguity | Broader optional/parallel convenience and production budgets |
| ElementAt | Required/optional/fallback Integer access; typed start/from-end positions and bounded actual-row tails | Wider position/parallel policies and production budgets |
| Contains, SequenceEqual | Typed equality and short-circuit terminals; no hash/index preparation | Parallel terminal policies and production budgets |
| Distinct, Union, Intersect, Except | Deferred first-occurrence sets with independent indexed equality; Pascal ExceptWith spelling | Operator-wide parallel sets, wider streaming policies and production budgets |
| DistinctBy, UnionBy, IntersectBy, ExceptBy | Typed projected keys, retained original values, cached hashes/collision comparison | Operator-wide parallel keys and production budgets |
| Order, OrderDescending, OrderBy, OrderByDescending, ThenBy, ThenByDescending | Whole-value Order/OrderDescending; immutable key/secondary recipes with typed directions and stable cached keys | Broader key convenience/comparer performance and parallel merge policies |
| GroupBy, CountBy, AggregateBy | Deferred stable groups plus owned parallel keyed plans: immutable groups, checked/exact counts and compact numeric/custom kernels; source/completion policies, typed factories and complete UI publication | Result convenience forms, wider domains, parallel lookup/join/set strategies, streaming and production budgets |
| Join, GroupJoin | Deferred typed inner/group joins, duplicate multiplicity and retained managed contexts/matches | Broader performance/parallel join policies and production budgets |
| LeftJoin, RightJoin, FullJoin | Typed optional partners, stable left/right/full joins with explicit presence | Parallel/streaming policies and production budgets; reference release status reviewed per delivery |
| Min, Max, MinBy, MaxBy | Typed streaming comparers plus owned parallel whole-value/projected-key/program extrema; explicit absence, cached keys and original-source tie selection | Wider numeric/optional convenience, streaming and production budgets |
| Sum, Average, Aggregate | Typed folds and owned numeric plans for Integer/signed64/Double/decimal/binary32; checked sums, stable/explicit approximate means, exact wide decimal merges, optional input lifting and custom kernels | Broader optional convenience, unsigned/generic-math domains, parallel grouping and production budgets |
| Zip | Deferred two/three input, retained managed pair/triple contexts and typed result selectors; explicit shortest-input evaluation | Wider performance/parallel policies and production budgets |
| Index | Deferred retained ordinal/value contexts; checked independent zero-based ordinals | Wider parallel policies and production budgets |
| Cast, OfType | Typed class/interface membership and explicit optional converters; observable target identity capability | Wider native/runtime convenience and production budgets |
| ToArray, ToList, ToDictionary, ToLookup, ToHashSet | Detached vectors and immutable owned portable list/dictionary/lookup/set factories; typed duplicate policies and native editable RTL copies | Wider materialization/native convenience, mutable live-store integration and production budgets |
| Shuffle | Deferred detached Fisher-Yates permutation; caller-owned independent RNG factories and portable seeded rejection sampling | Wider randomness/parallel policies and production budgets |
| ForEach / parallel ForAll | Synchronous ForEach; eager/deferred and fused-program ForAll with owned per-partition actions, original input ordinals, cancellation contexts and safe UI children | Direct indexed serial convenience, wider workload/terminal policies and production budgets |

The existing serializable Nyx collection query policy is a separate design/wire
contract. It does not establish any open generic operator above, and executable
callbacks are not silently converted to serialized strings.

| Parallel reference concern | Nyx current source | Remaining acceptance |
| --- | --- | --- |
| Entry and sequential boundaries | Immutable fused filter/map programs; explicit snapshot or bounded streaming producer/worker input, ordinary AsSequential boundary and deferred UI alternative | Operator-wide parallel composition, whole-operator streaming strategies and production budgets |
| Degree of parallelism | Typed bounded concurrency/partition size plus shared native pool | Full workload budgets and scheduler admission interactions |
| Ordered/unordered results | Source order or UI-observed partition completion order through snapshot/streaming capture | Wider operator-specific merge strategies and production budgets |
| Cancellation | Receipt cancels queued tickets; mounted event generations link ordinary/eager/deferred/program/action runs to page retirement; query action contexts extend retirement to posted UI children | Wider token composition, expensive operator preparation and additional host cases |
| Failure aggregation | Stop/collect policies with owned partition/source-offset diagnostics; complete-result admission | Operator-wide failure/aggregate policies and extensible diagnostic presentation |
| Reduction/aggregate merge | Independent accumulators and final policy-bound merges; global quantifiers, typed extrema/numeric/keyed summaries with complete UI publication across snapshot/streaming capture | Wider operator/streaming strategies and production budgets |
| Worker capability | Actual native overlap; browser explicitly refuses required threads | Browser worker transport and supported transferable callback programs |
| UI/collection safety | Revocable method leases, captured target revisions and complete atomic assignment; typed terminal observers and request/generation-bound state feedback; actual application navigation/disposal revokes ports | Broader application/resource integration, detailed error/cancel controls, Studio authoring and other widgetsets |

Evidence is recorded in WORK.md: deferred/ordering/callback packets, owned query
delivery, actual partition overlap and deferred source integration. Repeated row
assertions and overload counts earn no full criterion/task credit. Neither this
map nor a successful generic consumer closes the full LINQ/parallel task.
