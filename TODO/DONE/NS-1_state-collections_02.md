# NS-1_state-collections_02 — Preserve explicit row replacement across equal values

[North star NS-1](../../MILESTONES.md#assessment) · [Task catalog](../README.md) ·
[Task flow](../../TASKFLOW.MD)

**Description:**

An atomic batch can remove an existing row and reinsert the same ID with equal
values/order. The former net-value no-op optimization suppressed that publication,
so live views never receive the removal and an old editor/selection/disclosure can
inherit a replacement identity. Draft-recovery work reproduced this through the
ordinary mounted notification path: source revision remained unchanged.

Publish explicit retirement of an original row while preserving genuine value/
ordering no-ops, including inserting/removing a row absent from the original
dataset. Reuse the existing atomic admission, ordered log and revision owner.
This is a discovered prerequisite of [live inspection04](../NS-3_control-usefulness_04.md),
not a new store or schema/wire format. Original foundation acceptance remains
historical evidence; this correction closes only the gap below.

North star: NS-1. Completion credit: pending assessment within original structured
state/runtime identity outcomes, counted once.

**Acceptance Criteria:**

- Removing/reinserting an original ID publishes one new revision and the exact
  ordered remove/insert log even when values and final order are identical.
  Unchanged updates/assignments, move-away/back and insertion/removal of a new
  transient ID retain their original no-op behavior.
- Validator failure rejects the entire equal-value replacement, retaining the
  old snapshot/revision, independent views and observer state. Successful
  installation uses the existing prepared-publication protocol and byte/row/edit
  limits; no extra callback or alternate mutation path is introduced.
- Shared native and browser behavior checks qualify same-value replacement and
  remaining no-ops. Actual mounted consumers show that old editing/selection/
  disclosure does not migrate to the replacement. Retain clean native heaps,
  compiler evidence and target execution limits separately; compilation alone
  does not accept browser interaction.
- Document the distinction between value-only no-ops and explicit row retirement
  in the public collection contract and keep its regression in maintained checks.

**Blockers**

- [Observable structured state](NS-1_state-collections_01.md) supplies the
  existing accepted collection/publication foundation.

**Accepted — 2026-10-31:** All four original criteria are met. The original store
correction retains explicit retirement through final equality; 162 shared checks
execute on both targets, preserving exact logs, veto/limits and genuine no-ops.

Mounted qualification exposed a remaining shared-view defect: following views
retained old selected membership despite clearing disclosure. Admission now indexes
removed IDs and clears their old membership/focus/anchor, preserving unrelated
selection and the existing missing-row cursor fallback. Query/page-only projections
retain their prior behavior. All allocation remains before noncalling installation.
Maintained inspection checks pass 70 per target, including List/Table/Tree rollback
and independent views. The full view suite passes12429 per target, plus31 actual
browser control checks. Public collection comments and documentation explain the
value-no-op/identity-retirement distinction.

Actual native and browser Table recovery separates the exact old draft, including
supplementary Unicode/NUL and caret, from the equal-value replacement. Native
handwritten/generated journeys pass 57 per width; browser equivalents pass 42 per
measured 1280x900/390x844 viewport. The maintained Tree consumer adds an isolated
replacement/veto journey without weakening its original single-deletion assertion.
Final handwritten/generated native runs pass 60 per width; browser runs pass 118
per measured viewport. Actual widget selection and disclosure retire while peer
intent survives. All native runs free every heap block; browser error logs are empty.

Evidence: collection-safety-return-01, final-04/generated-05/tree-physical-07 and
compatibility-06, detailed in WORK.md. Initial mounted failures remain retained.
Full clipboard compatibility passes1755 per browser width and 1904/1893 native.
This accepts only the scoped identity correction; live-inspection04's remaining
context, native Tree editing, physical/assistive quality and measured scale remain
open. No duplicate north-star credit or completion percentage is assigned.
