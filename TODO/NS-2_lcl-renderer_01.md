# NS-2_lcl-renderer_01 — Lazarus projection

[North stars](../MILESTONES.md) · [Task catalog](README.md) · [Task flow](../TASKFLOW.MD)

**Description:**

Deliver lazarus projection as part of the user's full Nyx and Nyx Studio outcome.
Starting evidence: inherited fluent browser prototype; new units and validation
are tracked in [WORK.md](../WORK.md). North-star owner: NS-2.
Completion credit: pending evidence-backed assessment; no credit is earned by partial code.

**Acceptance Criteria:**

- The same document and compounds project to LCL controls with equivalent values/events/layout semantics.
- Native control interaction, ownership teardown and resize behavior are exercised.
- Target capability differences are explicit; unsupported kinds do not silently substitute misleading controls.

**Blockers**

- [NS-1_model_01](DONE/NS-1_model_01.md) must have accepted evidence (update link when moved to DONE).
- [NS-1_catalog-theme_01](DONE/NS-1_catalog-theme_01.md) has accepted evidence.

## Proportional and hidden flow — 2026-10-04

Rows now reserve authored fixed widths before distributing weighted space;
explicit-height and nested columns distribute height independently of whether
an outer call supplied it. Hidden controls retain their ownership/drafts but
consume no size, weight or gap in row/column/grid placement. Cumulative rounding
assigns odd pixels without overflowing weighted Integer products. Batched LCL
geometry prevents a previously hidden label's automatic anchor pass from
restoring its stale position. Actual parent content width caps authored widths.

One native-MCP-authored 27-node review compiles unchanged for browser/LCL.
The physical consumer passes 2,084 native checks with zero leaks; browser passes
2,094 desktop and 2,095 actual-390. Memo focus/selection, literal collection
selection/rows and split descendants remain mounted during resize/toggle checks.
Property, bound-selection, editing and Studio regressions pass. The original
project is restored byte for byte through paired history after reviewed cleanup.

This advances criteria 1/2 without accepting this task. Unspecified intrinsic
row widths, root height, wrapping/alignment, widget metrics and complete target
breadth retain their original acceptance paths. See [layout](../docs/layout.md)
and the current WORK packet; no capability grade or criterion was weakened.

## Typed layout policies — 2026-10-04

Public value-owned policies and managed enum methods now cross persistence,
generated/admitted Pascal, bounded MCP metadata and both adapters. Actual native
caption widths, authored-width text height, wrap/alignment/justification,
implicit spacers, retained pixel metrics and root height propagation qualify
2,169 checked LCL controls/arithmetic assertions with zero leaks. The identical
semantic source also passes 2,214 desktop and 2,215 actual-390 browser checks;
Studio consumes the public policy and retains editing through resizing.

Criteria 1/2 advance but remain open for their complete intended scope. This and
the preceding allocation batch make two consecutive batches without accepting
a full criterion. Reassessment changes the next action to the existing
[protected semantic review prerequisite](NS-4_agent-workflows_01.md), whose
absence currently forces test history onto the user's Redo stack. Return here
for the remaining intrinsic constraints, native scaling/widget metrics,
accessibility, target breadth and native Studio. See WORK.md and the layout guide;
no scope, support grade or completion credit was reduced.
