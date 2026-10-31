# NS-1_state-collections_02 — Preserve explicit row replacement across equal values

[North star NS-1](../MILESTONES.md#assessment) · [Task catalog](README.md) ·
[Task flow](../TASKFLOW.MD)

**Description:**

An atomic batch can remove an existing row and reinsert the same ID with equal
values/order. The former net-value no-op optimization suppressed that publication,
so live views never receive the removal and an old editor/selection/disclosure can
inherit a replacement identity. Draft-recovery work reproduced this through the
ordinary mounted notification path: source revision remained unchanged.

Publish explicit retirement of an original row while preserving genuine value/
ordering no-ops, including inserting/removing a row absent from the original
dataset. Reuse the existing atomic admission, ordered log and revision owner.
This is a discovered prerequisite of [live inspection04](NS-3_control-usefulness_04.md),
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

- [Observable structured state](DONE/NS-1_state-collections_01.md) supplies the
  existing accepted collection/publication foundation.

The existing authorized HTTP preview/browser connection is now available. The
current return is its remaining shared and actual mounted browser acceptance;
WORK.md owns the matching native evidence and any necessary source repair.

The correction now preserves explicit removal of an original row through final
value equality, reusing existing publication. Native collection checks pass 162,
including validator refusal and existing genuine no-ops. New model-mounted and
actual native Table recovery checks show an old draft is separated from equal-
value replacement; handwritten/generated applications pass at 1280/390. Matching
shared/browser application paths compile only. Public documentation and maintained
regressions are updated. Browser execution and full task acceptance remain open;
this task stays in TODO with no completion-credit change. WORK owns the receipts
and limits rather than making any source immutable.
