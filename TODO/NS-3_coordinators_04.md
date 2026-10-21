# NS-3_coordinators_04 — Mount Kanban through the common coordinator path

[North stars](../MILESTONES.md) · [Task catalog](README.md) · [Task flow](../TASKFLOW.MD)

**Description:**

Keep a configured Kanban's real card movement working while moving automatic
mounting and retirement onto the public prepared coordinator path. Current
Kanban runtime, ordinary controls and Studio/save/generated journeys qualify
the existing implementation; each renderer still owns a separate Kanban vector.

Owner: [common coordinators](NS-3_coordinators_01.md), original criteria1–4,
serving [components](NS-3_components_01.md) criteria1/3. This migration adds no
credit. It can follow registration independently of MasterDetail's migration;
the parent retains their joint integration and full reuse acceptance.

**Acceptance Criteria:**

- Existing Configured/Disabled/Inherit and static-absence behavior remains
  compatible, including customized headings, Move faces and named-part payload.
  Existing authored applications need no new mandatory registration step.
  Caller registration does not silently suppress or duplicate built-in behavior.
- Both renderers use the common prepared lifetime for automatic Kanban. All
  three lane views, dormant observations and initial action availability prepare
  against the same prospective event generation before Build/Sync. No separate
  component-specific automatic lifetime vector remains for it.
- Actual accessible Move input and existing drag behavior update stable card
  identity through the existing collection/revision owner. Ordering, query and
  hidden membership retain their contracts; independent boards cannot move
  each other's cards. Caller event scheduling policies and before-callback
  observation remain unchanged, including asynchronous Click. DragStart
  negotiates before clearing prior drag state; DragEnd still performs cleanup.
- A failed lane/role/revision candidate refuses the entire board before retiring
  the accepted view. Same-ID replacement, disabled intent, inherited intent and
  full unmount/remount cannot leave partial subscriptions or live stale commands.
  Revocation precedes controls; disconnect attempts every owned cleanup, and
  notification failure after installation remains committed.
- Existing manual APIs, GUIDs and specialized runtime lookup retain behavior.
  Ordinary application input and applicable Studio/save/generated compatibility
  checks exercise the migrated path rather than a separate hand-attached fallback.
  Native execution has clean heaps; matching browser assertions compile without
  claiming browser input or full parent acceptance.

**Blockers**

- [Caller-owned registration](DONE/NS-3_coordinators_02.md) supplies the common prepared
  path and immutable factory snapshots.
- Existing collection/event/publication owners and Kanban policies remain the
  authority. No second movement, selection, state or history engine is introduced.
- Browser execution/observing requires an admitted host/session; no host/config
  retry, cleanup, new tool/wire rollout or extra agent is authorized here.
