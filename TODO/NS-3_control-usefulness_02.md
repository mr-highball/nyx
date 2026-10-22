# NS-3_control-usefulness_02 — Let table users filter and sort columns

[North star NS-3](../MILESTONES.md#assessment) · [Task catalog](README.md) ·
[Usefulness review](DONE/NS-3_control-usefulness_01.md) · [Task flow](../TASKFLOW.MD)

**Description:**

Give users of a Nyx table visible, understandable column filtering and sorting
without requiring every application to construct its own grid query editor.
The external debugger report motivates this outcome; the external application's
source has not been inspected and is not evidence of a particular Nyx defect.

North star: NS-3. Milestone: M3/components01 advanced data-control behavior.
Completion credit: pending assessment within original component scope; no
duplicate credit for existing query, state, DataToolbar or parity work.

Starting evidence: [typed queries](../docs/collection-queries.md),
[collection views](../docs/collection-views.md),
[query owner](../src/nyx.collections.query.pas) and
[live DataToolbar](NS-3_data-toolbar_02.md). Existing filtering/stable ordering
are foundations. The missing result to review and deliver is the table user's
discoverable column interaction and its portable, authored configuration.
The existing public `NewNyxQueryEditor` is available for advanced predicate/sort
editing; reuse it instead of introducing another parser/editor for the same model.
The current native adapter installs no header query handlers; browser headings
are plain `th` text. These are source observations, not an execution verdict for
the external application.
DataToolbar and FilterBar integration must reuse this query policy rather than
introducing another store, query evaluator or component-specific lifecycle.

Ordered implementation packets preserve all seven criteria below:

| Packet | Useful result | Still required afterward |
| --- | --- | --- |
| P1 — Prepared query publication | Optional opaque intent capture and query preparation on the existing view; real state/two-view group is all-or-refused and observers see all committed models | Canonical contributions, visible controls, authoring and full02 |
| P2 — Shared query contributions | One view-owned explicitly admitted baseline (CurrentQuery by default; authored via explicit rebase), independent user predicates/shared sort, exact leases and explicit owner/global Clear with ABA/lifetime guards | Header/form controls and complete target/source adoption |
| P3 — Useful table commands | Real native/browser header/keyboard filtering/sort, active chips/counts/empty recovery and coherent peer surface drafts | Ordinary Studio/history/save/generated integration and full qualification |
| P4 — Authored workbench | Typed policy/event/help/source/persistence/semantic agreement and own saved/generated two-view workbench at both targets/widths | Any unmet original criterion, full parity/quality and delivery |

P1 has a separately reviewed local qualification recorded in
[WORK](../WORK.md#prepared-collection-query-publication--2026-10-21): root owns
the existing view/prepared integration/build; helper owns only the independent
new contract and public focused consumer. Native89 public checks, unchanged
query56 and its own compiled-query57 pass with clean heaps. Matching browser
consumers and both Studio mains compile only; seven compilers have zero owned
warnings. All readers are joined/audited with reviewed source unchanged. No
header/session/renderer/Studio/Q1 source scope follows. Equal ConfigureQuery
remains a true no-op; changed A→B→A intent invalidates old captures. Source updates
before Prepare are valid; a preparation reserves the current atomic source/view
and refuses a simultaneous same-source row-edit transaction. This is a documented
API boundary, not a claim that the full table interaction exists.
All seven acceptance criteria remain open; P1 adds no duplicate product credit.
Next is the single view-owned P2 contribution/session result, not another P1
fixture/compiler variant. Default baseline admission preserves the accepted
CurrentQuery; a second surface joins existing canonical contributions instead of
recapturing their effective query. Explicit authored restoration is a prepared
rebase, not automatic behavior when acquiring/releasing an owner lease.

P2 now has a separately bounded native-local API qualification in
[WORK](../WORK.md#shared-collection-query-sessions--2026-10-21), with the matching
[Q1 ownership rule](NS-3_data-toolbar_02.md) specified before implementation.
One optional session on the existing view owns baseline/independent predicates/
shared sort. Typed fluent batches, revocable leases, copied observations and
checked metadata transfer use the existing prepared-query participant. Root owns
view/bindings/build/docs; the existing helper owns the new contract/public consumer.
Native93 new public checks plus unchanged prepared-query89/query56 pass with
clean heaps. Final8 compilers have zero owned warnings; browser assertions and
both Studio mains compile only. All actual readers are joined and exact audited,
including retained first identifier failure and superseded enum warnings. No
header/renderer/Studio/Q1 implementation or browser execution is released by P2.
All original criteria remain open; the next outcome is P3's actual useful table
commands and shared form, not another model/compiler variant.

**Acceptance Criteria:**

- Expose immutable typed fluent per-column capabilities through specialized
  table/binding interfaces: allowed filter operators, sortability, comparison
  policy and initial/default behavior. Field identities and values are
  schema-checked. Text, numeric and Boolean columns offer appropriate operations;
  optional/empty values and any additional admitted scalar types have documented
  semantics. No behavioral magic strings or widget downcasts are required.
- A user can open an accessible column filter affordance, see/edit its current
  value, apply it, clear one column and clear all user filters. Active filters
  and filtered/total result counts are visible; no results has a useful recovery
  action. Keyboard operation and a usable compact/pointer presentation exist on
  actual native/LCL and browser controls. Authors can opt out per column or table.
- Header interaction exposes ascending/descending/default order and a visible
  current sort indicator. Explicit typed policy admits single or multiple sort
  keys; the user can inspect precedence and reset it. Equal values retain stable
  deterministic ordering and type-correct comparisons. Filtering and sorting
  operate on the intended result before paging, not just the painted viewport.
- Compose user predicates with the admitted authored query. Define coexistence
  with DataToolbar/FilterBar/programmatic query replacement and version ownership.
  Before implementation release, settle the existing DataToolbar Q1 same-target/
  query-authority rule with that owner. This is the shared integration contract,
  not a new hard dependency on all DataToolbar02 acceptance or another evaluator.
  Clear restores the current valid authored baseline, never an obsolete snapshot
  or another owner's query. Schema loss, invalid drafts, stale/reentrant input
  and cancelled work refuse without partial publication or lost editor text.
- Preserve source data, stable row identity, hidden selection, current-cell edit
  rules and independent reusable/view state. Emit meaningful typed before/after
  query/sort/filter events with old/new policy and origin where applicable;
  cancellable stages precede publication. Existing ordered multi-registration
  callbacks and scheduler/UI ownership remain effective.
- Studio exposes supported column properties/events and explanatory help. Saved
  authoring policy, local reusable overrides, paired Undo/Redo, crafted generated
  Pascal and its source reader agree. Distinguish portable defaults from optional
  user-session preferences. Existing semantic operations expose the same intent
  in bounded revision-aware reads/mutations, without installing a new tool server.
- Demonstrate the public contract in a Nyx data workbench on actual native and
  browser targets: two independent tables, combined filters, typed comparisons,
  multiple sort keys, empty results, reset, paging, hidden selection, active drafts,
  replacement and stale commands. Independently build that journey's own emitted
  Pascal, retain refusals and clean native heaps, and publish target limitations.
  A metadata-only test or application-specific filter textbox is insufficient.

**Blockers**

- [Control usefulness review](DONE/NS-3_control-usefulness_01.md) supplies the reviewed
  interaction gaps, default policy and concrete target/source boundary.
- [Observable collections](DONE/NS-1_state-collections_01.md) supply the accepted
  store, typed schema and selection/query publication foundation.
