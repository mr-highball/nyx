# NS-3_control-usefulness_05 — Preserve query choices when reconfiguring Kanban lanes

[North star NS-3](../../MILESTONES.md#assessment) · [Task catalog](../README.md) ·
[Task flow](../../TASKFLOW.MD) · [Table query authoring](NS-3_control-usefulness_02.md)

**Description:**

Keep an author's explicit collection query-control choices when the existing
Kanban editor rebuilds a lane binding. Source review of
`NyxKanbanEditorLaneSpec` in [nyx.kanban.editor](../../src/nyx.kanban.editor.pas)
found that it preserves a previous caption, sizing, typeahead and paging, but
omits the new table and column query policies. That is a preservation gap even
though the standard lane is a List and has no table-header filter surface.
Accepted on 2026-10-25 after reproducing that loss and qualifying the repair
against all four unchanged criteria below. Exact declarations now survive;
resolved defaults remain absent.

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

- [Observable collection foundation](NS-1_state-collections_01.md) supplies
  the accepted immutable schema/view owners. The existing v7 policy contract and
  P3 native-local receipts are starting evidence in [WORK](../../WORK.md), not a
  dependency on closing all of task02 or permission to run a host/compiler.

**Accepted evidence — 2026-10-25:**

- The strengthened public-builder regression fails at check 33 against the
  exact earlier implementation. The final negative control exits normally with
  a complete clean heap; the first failure's `Halt` retained its active exception,
  so that harness cleanup was repaired without changing the failure assertion.
- The implementation copies column/table query values only when their explicit
  declaration flags are present. Existing lane admission, filter, key, field,
  scope, read-only and single-selection behavior is unchanged.
- The editor regression passes 197 checked native assertions with a complete
  clean heap. It covers all descriptor versions 1–6, fresh/absent declarations,
  opt-outs, empty/restricted operators and text comparison, nine independent
  table/column combinations across all three lanes, unchanged caller options/
  specifications and peer independence, including explicit default-valued
  declarations. One 46-lane document survives codec
  and crafted-source admission exactly. Its emitted Pascal unit compiles for
  both FPC and pas2js.
- The existing Studio candidate regression passes 170 checked native assertions
  with a complete clean heap. Explicit policies survive ordinary reusable
  reconfiguration, Disable and Inherit, while the original definition, sibling,
  schema/refusal and source round-trip checks remain intact. Matching editor
  and candidate browser assertions compile. Browser input is not asserted.
- Native compilers are warning-free. The three browser compilers retain the
  existing seven installed RTL warnings each, with no owned warning. Evidence
  is `.local/qualification/kanban-query-policy-01`; all actual readers are joined
  and the source freeze has ended. This accepts saved metadata preservation on
  a List lane, adds no table-header UI and earns no separate completion credit.

The later-family follow-up under [workbench03](../NS-3_control-usefulness_03.md)
was refined after projection validation showed those choices are Table-only.
It now covers early refusal of incompatible raw descriptors, rather than new
presentation support on List lanes. This query repair does not accept that
broader authoring/reuse outcome.
