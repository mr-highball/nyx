# NS-3_control-usefulness_05 — Preserve query choices when reconfiguring Kanban lanes

[North star NS-3](../MILESTONES.md#assessment) · [Task catalog](README.md) ·
[Task flow](../TASKFLOW.MD) · [Table query authoring](NS-3_control-usefulness_02.md)

**Description:**

Keep an author's explicit collection query-control choices when the existing
Kanban editor rebuilds a lane binding. Source review of
`NyxKanbanEditorLaneSpec` in [nyx.kanban.editor](../src/nyx.kanban.editor.pas)
found that it preserves a previous caption, sizing, typeahead and paging, but
omits the new table and column query policies. That is a preservation gap even
though the standard lane is a List and has no table-header filter surface.
This is source evidence; no new execution failure has been asserted.

North star: NS-3, within the existing advanced-component authoring and reuse
scope. Completion credit: none separately; this repair must not duplicate
component, query or parity credit. This task adds no Kanban filter UI, query
evaluator, collection owner or projection restriction. The shared descriptor
already admits these policy fields; do not narrow its admission to hide loss.

The unrelated Studio collection-intent rebuild is owned by P4 in task02. Fixing
that path does not prove this lane-specific builder is lossless. Keep this
separate source owner and its actual required checks visible.

**Acceptance Criteria:**

- A valid previous lane retains its explicitly declared table policy and its
  retained text column's explicitly declared query policy when reconfigured,
  alongside its existing caption/sizing/typeahead/paging behavior. Preserve the
  existing intended lane filter, scope, read-only mode and single selection.
- Absent policies remain absent: resolved library defaults must not become
  explicit declarations or silently promote old version1–6 bindings to version7.
  Fresh lane creation keeps current defaults. Invalid previous lane shapes still
  refuse before returning a partial result; do not weaken current admission.
- Exercise this existing public builder with explicit opt-out/operator/text
  choices and absent policies. Verify caller options and previous specs remain
  unchanged, independent lanes do not inherit another lane's policy, and codec/
  crafted-source round trips retain the admitted result. Use the existing
  Kanban authoring regression for any affected ordinary editor path; retain
  applicable compatibility evidence rather than replay unrelated families.
- Qualify the changed boundary natively with clean heaps and compile the matching
  browser assertions. Report that this preserves saved metadata on a List lane;
  it does not establish browser input or add a table-header behavior to Kanban.

**Blockers**

- [Observable collection foundation](DONE/NS-1_state-collections_01.md) supplies
  the accepted immutable schema/view owners. The existing v7 policy contract and
  P3 native-local receipts are starting evidence in [WORK](../WORK.md), not a
  dependency on closing all of task02 or permission to run a host/compiler.
