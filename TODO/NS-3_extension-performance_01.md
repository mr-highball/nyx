# NS-3_extension-performance_01 — Extension SDK and measured performance

[North stars](../MILESTONES.md) · [Task catalog](README.md) · [Task flow](../TASKFLOW.MD)

**Description:**

Deliver extension sdk and measured performance as part of the user's full Nyx and Nyx Studio outcome.
Starting evidence: inherited fluent browser prototype; new units and validation
are tracked in [WORK.md](../WORK.md). North-star owner: NS-3.
Completion credit: pending evidence-backed assessment; no credit is earned by partial code.

Observed query-fixture cost (2026-10-07): checked native semantic collections
pass 117 with zero leaks, but allocate about 77 GB cumulatively under heap tracing;
the focused long-value query path passes 19 with about 760 MB cumulative
allocations. These totals are neither live resident memory nor production timing.
The shared browser fixture did not reach readiness within the existing driver
bound, including focused attempts; the cause is not established. Measure/bound
long-predicate candidate/source/codec work with the existing source/workflow owners
before claiming large-value responsiveness. Do not increase timeouts or count a
browser compile as execution. Existing ordinary authenticated English query
composition/builds pass separately. No performance criterion closes. See
[the query workflow packet](../WORK.md#current-return-path-bounded-collection-query-mcp--2026-10-07).

**Acceptance Criteria:**

- Consumers derive/fork recipes, replace target adapters and add themed subcomponents without editing Nyx internals.
- Documented frame/update/memory budgets are measured with large data/layout fixtures on both targets.
- Virtualization, incremental updates, teardown and reusable-instance isolation are tested.

**Blockers**

- [NS-3_components_01](NS-3_components_01.md) must have accepted evidence (update link when moved to DONE).

## Materialized collection control measurements — 2026-10-03

The runtime collection bridge measures all three target controls simultaneously
against an admitted four-way hierarchy. Twenty committed integer updates include
store publication, view validation and control synchronization; seed construction
is excluded. Correct values/revisions, row counts, 21 refreshes per attachment and
owned disconnect gate CSV output.

| Target | Rows | Mount (ms) | Twenty updates (ms) |
| --- | ---: | ---: | ---: |
| FPC 3.3.1 / Win32 LCL | 512 | 15 | 297 |
| FPC 3.3.1 / Win32 LCL | 4096 | 93 | 1844 |
| pas2js 3.3.1 / headless Edge | 512 | 112.8 | 936.9 |
| pas2js 3.3.1 / headless Edge | 4096 | 487.5 | 5378 |

These are observed samples, not cross-target speed comparisons or production
frame budgets. Native widgets were hidden and browser time used the real clock;
paint, physical input, device scaling and sustained-load memory were not measured.
All rows remain materialized. Native lists rebuild text; tables visit model cells;
trees preserve structure for scalar edits but relocate nodes for structural edits.
Owned teardown and reusable-instance isolation have focused both-target evidence.
This task still owns virtualization, changed-row synchronization, deep hierarchy
behavior, sustained memory and defined frame/update budgets. No criterion closes
from these baseline measurements. Reproduction and exact artifacts are linked
from [collection views](../docs/collection-views.md) and the next WORK record.
