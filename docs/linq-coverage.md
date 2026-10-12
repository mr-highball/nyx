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
| AsEnumerable, Empty | Typed sequence/factory and named array/snapshot AsNyx helpers | Native RTL collection helpers and independent extension use |
| Where | Deferred ordinary/indexed predicates; owned/scoped/inline adapters | Operator-wide parallel filtering and production budgets |
| Select | Ordinary/indexed same-type and type-changing projections | Parallel operator-wide projection policies and budgets |
| SelectMany | Owned deferred ordinary/indexed flattening and typed result selectors | Wider streaming/cancellation policies and production budgets |
| Skip, Take, SkipWhile, TakeWhile | Nonnegative and ordinary/indexed prefix policies | Range/from-end forms and wider parallel policies |
| SkipLast, TakeLast | Deferred bounded rings grow with actual rows; independent tail order/ownership | Wider parallel/streaming tail policies and production budgets |
| Chunk | Detached owned ordinary sequences, short-tail handling and deferred source entry | Wider parallel chunk policies and production budgets |
| Append, Prepend, Concat | Deferred insertion/concatenation with owned values; Prepend defers source entry | Wider parallel/streaming policies and production budgets |
| Reverse | Deferred complete admission, independent owned reversal | Parallel reversal policies and production budgets |
| Range, Repeat, Sequence | Checked NyxRange, typed RepeatValue/Iterate; exact both-target generation | Preview generic-math Sequence, wider numeric generation and production budgets |
| Any, All | Short-circuit and empty behavior implemented | Parallel predicate/quantifier execution policies |
| Count, LongCount, TryGetNonEnumeratedCount | Integer Count implemented | Wider portable counts, predicate shortcuts and explicit optional count capability |
| First, Last, Single | Required cardinality and Try* absence implemented | Predicate/default/fallback families |
| FirstOrDefault, LastOrDefault, SingleOrDefault, ElementAtOrDefault, DefaultIfEmpty | Typed explicit/ordinary initialized fallbacks; cardinality and empty sequence semantics | Predicate-specific and broader optional convenience forms |
| ElementAt | Required and optional zero-based Integer access; negative optional index avoids source | From-end access and wider position policies |
| Contains, SequenceEqual | Typed equality and short-circuit terminals; no hash/index preparation | Parallel terminal policies and production budgets |
| Distinct, Union, Intersect, Except | Deferred first-occurrence sets with independent indexed equality; Pascal ExceptWith spelling | Operator-wide parallel sets, wider streaming policies and production budgets |
| DistinctBy, UnionBy, IntersectBy, ExceptBy | Typed projected keys, retained original values, cached hashes/collision comparison | Operator-wide parallel keys and production budgets |
| Order, OrderDescending, OrderBy, OrderByDescending, ThenBy, ThenByDescending | Immutable OrderBy/ThenBy recipes; typed ascending/descending directions, stable cached keys | Convenience families, broader key/comparer performance and parallel merge policies |
| GroupBy, CountBy, AggregateBy | Deferred stable GroupBy and immutable retained groups with indexed equality | CountBy/AggregateBy, result convenience forms and parallel grouping/aggregate policies |
| Join, GroupJoin | Deferred typed inner/group joins, duplicate multiplicity and retained managed contexts/matches | Broader performance/parallel join policies and production budgets |
| LeftJoin, RightJoin, FullJoin | Typed optional partners, stable left/right/full joins with explicit presence | Parallel/streaming policies and production budgets; reference release status reviewed per delivery |
| Min, Max, MinBy, MaxBy | Open | Typed comparison, absence/ties and numeric policies |
| Sum, Average, Aggregate | Open | Checked typed numeric/optional accumulation, seeds/results and failure policies |
| Zip | Open | Two/three input and typed result-selector contracts |
| Index | Open | Typed ordinal/value pairs and checked ordinals |
| Cast, OfType | Open | Pascal-safe typed runtime capability/conversion contracts on both targets |
| ToArray, ToList, ToDictionary, ToLookup, ToHashSet | Detached ToArray and immutable indexed ToLookup factory implemented | Typed lists/dictionaries/sets, duplicate-key policies and broader materialization helpers |
| Shuffle | Open | Caller-controlled randomness, repeatability and owned enumeration |
| ForEach / parallel ForAll | Synchronous ForEach implemented; partitioned projection produces owned complete results | Indexed/actions and safe concurrent terminal execution |

The existing serializable Nyx collection query policy is a separate design/wire
contract. It does not establish any open generic operator above, and executable
callbacks are not silently converted to serialized strings.

| Parallel reference concern | Nyx current source | Remaining acceptance |
| --- | --- | --- |
| Entry and sequential boundaries | Array/owned sequence submissions; deferred source capture on UI; explicit deferred UI alternative | Fluent operator-wide partition composition and sequential/parallel boundaries |
| Degree of parallelism | Typed bounded concurrency/partition size plus shared native pool | Full workload budgets and scheduler admission interactions |
| Ordered/unordered results | Source order or UI-observed partition completion order | Streaming and operator-specific merge strategies |
| Cancellation | Receipt cancels queued tickets; running callbacks stop between rows | Wider token composition, expensive operator preparation and additional host cases |
| Failure aggregation | Stop/collect policies with owned partition/source-offset diagnostics; complete-result admission | Operator-wide failure/aggregate policies and extensible diagnostic presentation |
| Worker capability | Actual native overlap; browser explicitly refuses required threads | Browser worker transport and supported transferable callback programs |
| UI/collection safety | Revocable method leases, captured target revisions and complete atomic assignment | Full application/navigation integration, Studio authoring and other widgetsets |

Evidence is recorded in WORK.md: deferred/ordering/callback packets, owned query
delivery, actual partition overlap and deferred source integration. Repeated row
assertions and overload counts earn no full criterion/task credit. Neither this
map nor a successful generic consumer closes the full LINQ/parallel task.
