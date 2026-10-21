# NS-3_coordinators_01 — Extensible prepared component coordinators

[North stars](../MILESTONES.md) · [Task catalog](README.md) · [Task flow](../TASKFLOW.MD)

**Description:**

Make opt-in compound behavior creator-extensible through the existing prepared
publication and event-view owners. Current Kanban/MasterDetail behavior requires
separate lifecycle vectors and dispatch wiring in both renderers. A common
mounting/factory path must preserve those policies and allow creator-owned
behavior in an ordinary application without editing renderer internals.

Owner and credit remain [components](NS-3_components_01.md) criteria1/3, returning
to [Studio authoring](NS-4_studio-authoring_01.md) and native/browser parity. This
is a prerequisite for the useful DataToolbar outcome. Registry-only work does
not accept this migration/reuse task. It supplies no second state, selection,
event or history engine.

**Acceptance Criteria:**

- Public managed registration has explicit application/renderer ownership and
  typed kind/intent identities. Duplicate/conflicting claims have deterministic
  handling; explicit replacement, when supported, affects future candidates only.
  Each candidate owns an immutable registry snapshot. Saved metadata never loads
  executable factories or grants host authority; absence preserves static behavior.
- One common candidate traversal/mount lifetime uses existing prepared
  publication/readiness and the same prospective event generation. Factories
  receive checked realized roles, scoped collection authority and revocable
  refresh capability. They retain no widgets, document or controller. Initial
  presentation/availability is ready before Build/Sync; staging is dormant.
- Outward validation precedes retirement and is followed by final local fences.
  Install adopts preallocated flags/ownership; old generations revoke before
  control teardown. Refusal preserves the accepted view. Retirement releases
  preparation-only resources; disconnect attempts every displaced cleanup.
  Postcommit failures remain committed and never trigger fake rollback/retry.
- Existing Kanban/MasterDetail manual APIs, GUIDs and useful behavior remain
  compatible. Optional prequeue enrichment preserves copied Details and scalar
  Value independently. Generic runtime lookup keeps existing typed accessors
  without another lifecycle branch for each new coordinator.
- An explicitly registered creator-owned coordinator works in normal reusable
  instances and embedded content on both application hosts without renderer
  edits. Cancellation, stale input, reentrant validation and disposal preserve
  instance isolation. Native/browser evidence and compile-only limits stay explicit.

Scope mapping: original criteria1-3 are shared integration gates; criterion4
and the generic creator/reuse portion of criterion5 remain here. The DataToolbar
portion of criterion5 is required by
[DataToolbar authoring](NS-3_data-toolbar_01.md), downstream of this API/migration.
That use is not a reverse prerequisite; components still requires both outcomes.
Credit is unchanged and counted once.

**Blockers**

- [Caller-owned behavior registration](NS-3_coordinators_02.md) owns the public
  registration/mount contract and an independently authored application consumer.
  This parent keeps compatibility migration and reusable/embedded instances.
- [Observable state and collections](DONE/NS-1_state-collections_01.md) supply the
  accepted data authority; existing prepared event/publication owners and locally
  qualified Kanban/MasterDetail policies supply the lifecycle starting point.
- Full target quality/observing remains with [parity](NS-2_parity-accessibility_01.md).
  No host/config retry, cleanup, new MCP tool or wire schema is authorized here.
