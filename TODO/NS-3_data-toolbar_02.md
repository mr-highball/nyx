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

**Work packets and handoff:**

| Packet | Reviewable outcome | Dependency and finish line |
| --- | --- | --- |
| Q1 — Useful public toolbar | Typed target/field options and real Search, Sort and Clear operate on an existing List/Table query | Accepted common coordinator integration. Root reviews query ownership and visible-input reset before releasing runtime/consumer scopes. Clear composes with and restores the admitted authored baseline. |
| Q2 — Independent application | Two ordinary toolbars exercise input, empty results, hidden selection, target/query changes and retirement without changing rows/defaults | Q1. Native clean heaps, matching browser assertions and documented static compatibility qualify all five criteria below; Studio/save/generated adoption stays in01. |

Resolve two concrete seams before coding Q1. The current static recipe exposes
search/sort/create/refresh, so opt-in configuration needs an admitted named Clear
action while preserving the default recipe/GUID. Reset visible Search and typed
Sort through their existing scalar input/binding owner; projection Text changes
captions, not input values. Specify ownership of intervening target/query changes
so Clear/disposal never restore an obsolete baseline. Use ConfigureQuery and the
existing state/selection/event owners; no replacement engine follows.

The read-only owner review narrows that reset gap: live input admits through
LiveBindings and State.Apply, while an INyxTextInput.Value setter only configures
its node. A future scoped scalar command must reuse the existing live owner,
exact accepted roles/generation and copied typed values without exposing a Node
or controller. Admit Search and Sort together; two unrelated sequential writes
cannot promise one Clear operation. Existing caller bindings/read-only/domain
rules still apply. Specify validation/publication across scalar reset and query
change before source release rather than assuming those owners commit together.

Capture the baseline query and the last query actually owned by this attachment.
Before each mutation, verify the exact view/store/source and last-owned query;
an externally replaced query/target must refuse or retire this authority, never
be overwritten by Clear. Query notification failure remains committed. The
opt-in ordinary Clear button uses the existing Clear semantic; compatible custom
parts/captions/callbacks survive, and an occupied incompatible role refuses the
whole candidate. The static four existing accessors/GUID remain unchanged.

Read-only design can advance while prerequisites are pending. Acceptance remains
downstream of the complete coordinator task, including05's actual-browser gate;
this packet list does not bypass that dependency or authorize a host rollout.

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
