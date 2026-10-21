# NS-3_control-usefulness_02 — Let table users filter and sort columns

[North star NS-3](../MILESTONES.md#assessment) · [Task catalog](README.md) ·
[Usefulness review](NS-3_control-usefulness_01.md) · [Task flow](../TASKFLOW.MD)

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

- [Control usefulness review](NS-3_control-usefulness_01.md) supplies the reviewed
  interaction gaps, default policy and concrete target/source boundary.
- [Observable collections](DONE/NS-1_state-collections_01.md) supply the accepted
  store, typed schema and selection/query publication foundation.
