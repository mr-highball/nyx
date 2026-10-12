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
| Where | Deferred predicates compose; owned/scoped/inline adapters | Indexed predicate variants; operator-wide parallel filtering |
| Select | Same-type fluent projection and type-changing TNyxSelect | Indexed projections; parallel pipeline projection policies |
| SelectMany | Open | Typed flattening, result selectors and indexed variants |
| Skip, Take, SkipWhile, TakeWhile | Nonnegative/prefix policies implemented | Indexed prefix variants and range/from-end forms |
| SkipLast, TakeLast | Open | Bounded tail buffers, independent deferred cursors |
| Chunk | Open | Owned short-tail chunk values, deferred cursors |
| Append, Prepend, Concat | Concat implemented | Append/Prepend with independent managed element lifetimes |
| Reverse | Open | Deferred independent reversal and ownership |
| Range, Repeat, Sequence | Open | Checked bounds/generation and typed retained values; preview inventory reviewed per delivery |
| Any, All | Short-circuit and empty behavior implemented | Parallel predicate/quantifier execution policies |
| Count, LongCount, TryGetNonEnumeratedCount | Integer Count implemented | Wider portable counts, predicate shortcuts and explicit optional count capability |
| First, Last, Single | Required cardinality and Try* absence implemented | Predicate/default/fallback families |
| FirstOrDefault, LastOrDefault, SingleOrDefault, ElementAtOrDefault, DefaultIfEmpty | Try* provides explicit absence for three element operations | Full typed fallback/default sequence/element contracts |
| ElementAt | Zero-based Integer access implemented | From-end access and optional result variants |
| Contains, SequenceEqual | Open | Typed equality/comparer contracts and short-circuit behavior |
| Distinct, Union, Intersect, Except | Open | Generic equality and deferred set operations |
| DistinctBy, UnionBy, IntersectBy, ExceptBy | Open | Typed projected keys with exact equality/ownership |
| Order, OrderDescending, OrderBy, OrderByDescending, ThenBy, ThenByDescending | Immutable OrderBy/ThenBy recipes; typed ascending/descending directions, stable cached keys | Convenience families, broader key/comparer performance and parallel merge policies |
| GroupBy, CountBy, AggregateBy | Open | Owned groups, key equality, accumulators, exact ordering and failure semantics |
| Join, GroupJoin | Open | Typed inner/group joins and managed key/result lifetimes |
| LeftJoin, RightJoin, FullJoin | Open | Typed optional partners and outer-join variants; reference release status checked per delivery |
| Min, Max, MinBy, MaxBy | Open | Typed comparison, absence/ties and numeric policies |
| Sum, Average, Aggregate | Open | Checked typed numeric/optional accumulation, seeds/results and failure policies |
| Zip | Open | Two/three input and typed result-selector contracts |
| Index | Open | Typed ordinal/value pairs and checked ordinals |
| Cast, OfType | Open | Pascal-safe typed runtime capability/conversion contracts on both targets |
| ToArray, ToList, ToDictionary, ToLookup, ToHashSet | Detached ToArray implemented | Typed materialization containers, lookup/equality and duplicate-key policies |
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
