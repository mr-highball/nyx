# NS-3_data-toolbar_02 — Coordinate a toolbar with a collection query

[North stars](../MILESTONES.md) · [Task catalog](README.md) · [Task flow](../TASKFLOW.MD)

**Description:**

Make a public opt-in DataToolbar perform real search, sort and Clear on an
existing List/Table collection view in an ordinary application. This is the
runtime/API deliverable beneath [Studio DataToolbar authoring](NS-3_data-toolbar_01.md).
The parent retains authored intent, paired history, Save/Open and exact generated
application acceptance.

North star: NS-3, original components criteria1/3; no extra completion credit.
The static toolbar recipe, semantic actions and INyxCollectionView.ConfigureQuery
already exist. Static faces alone do not qualify this task.

**Acceptance Criteria:**

- Copied typed options identify a scoped List/Table and schema-checked search/
  sort fields, comparison and order. Missing, foreign, ambiguous or incompatible
  targets refuse before query changes. Static absence and explicit Disabled
  preserve current behavior; configuring does not rewrite caller event policies.
- Real control input changes displayed membership/order through ConfigureQuery.
  Search composes with the admitted authored query; an accessible visible Clear
  restores that exact baseline. Empty results, repeated searches and sort changes
  are useful results, not just updated configuration metadata.
- Preserve data rows, defaults, paging/typeahead ownership and hidden selection
  membership. Define ownership when the target/query changes while attached;
  Clear or disposal must never blindly restore an obsolete baseline.
- Use the common prepared coordinator path and accepted event generation.
  Cancellation, stale input, replacement and independent reusable instances
  cannot mutate a foreign or retired view. Refusal is unchanged; observer failure
  after publication remains committed.
- Execute a public application consumer with independent instances, typing,
  sort, Clear, empty results and lifecycle/refusal cases on native with clean
  heaps. Compile matching browser assertions. Document caller use and static
  compatibility; Studio/save/generated integration remains explicitly open.

**Blockers**

- [Extensible prepared coordinators](NS-3_coordinators_01.md) supply the common
  mounting path, without a third component-specific renderer lifecycle vector.
- [Observable state and collections](DONE/NS-1_state-collections_01.md) and the
  existing query owner supply authoritative data and query semantics.
