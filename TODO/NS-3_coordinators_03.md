# NS-3_coordinators_03 — Mount MasterDetail through the common coordinator path

[North stars](../MILESTONES.md) · [Task catalog](README.md) · [Task flow](../TASKFLOW.MD)

**Description:**

Keep an ordinary configured MasterDetail working while moving its automatic
mounting, projection and event enrichment onto the public prepared coordinator
path. Today both renderers maintain a separate MasterDetail lifecycle vector.
The existing selected-record, Studio and exact generated-application journeys
are compatibility evidence, not evidence that this migration is complete.

Owner: [common coordinators](NS-3_coordinators_01.md), original criteria1–4,
serving [components](NS-3_components_01.md) criteria1/3. This is an independently
usable migration deliverable; it adds no credit. Reusable/embedded adoption and
the parent's complete browser integration remain separate acceptance gates.

Read-only review found a concrete mixed-ownership gap: the current detail
projection writes title/description directly and tracks private text baselines.
Its availability marker covers Edit, not portable text ownership. A generic
factory claiming the same text can initially agree and later contend after a
selection change. Equal initial strings must not bypass ownership admission.

**Acceptance Criteria:**

- Existing Configured/Disabled/Inherit and static-absence behavior remains
  compatible. Existing authored applications do not acquire an extra mandatory
  caller-registration step. Built-in and caller registrations compose through
  immutable candidate snapshots with deterministic conflicts.
- Both renderers use the common prepared lifetime for automatic MasterDetail,
  preserving initial detail text/Edit availability before physical Build/Sync,
  dormant observations and the exact prospective event generation. No duplicate
  automatic mount or component-specific retirement vector remains for it.
- The same selection/snapshot owner drives visible details. Empty, removed or
  query-hidden selections preserve authored placeholders and disable Edit;
  data/defaults and independent instances remain unchanged.
- Shared role admission refuses competing title/description owners, including
  an ancestor creator whose initial text is identical and existing manual
  MasterDetail attachment. Refusal releases only candidate claims; replacement
  retires displaced claims. The old view stays usable. Availability constraints
  still compose without overwriting authored, bound or caller-owned policies.
- Existing manual APIs, GUIDs and specialized runtime lookup remain compatible.
  Optional common prequeue enrichment preserves copied original-record Details
  across queued selection changes and retains an independent scalar Value
  contract. Exact role proof and refusal to overwrite unrelated Details remain.
  Generic clicks and unrelated routes retain payload and scheduling policies.
- Actual ordinary application input proves selection/Edit, replacement, remount,
  cancellation and late-command refusal. Precommit failure preserves the old
  owner; postcommit failure remains committed and cleanup attempts every owner.
  Native checks have clean heaps; matching browser consumers keep all assertions
  and compile. These compiles do not qualify browser execution or parent reuse.

**Blockers**

- [Caller-owned registration](DONE/NS-3_coordinators_02.md) supplies the usable common
  prepared path and guarded projection/refresh contract.
- Existing collection/event/publication owners remain authoritative; no new
  selection, state, history or routing engine is introduced.
- Browser execution and observing require an admitted host/session. This task
  grants no host/config retry, cleanup, new tool/wire rollout or extra agent.
