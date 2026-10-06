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

## Discovered original-size viewport prerequisite — 2026-10-05

The codegen/editor capture consumer retains all 128/512/2048 controls and original
paired source byte sizes. Studio's hierarchy now uses one bounded public Nyx
tree with typed independent rows. At 2048 controls, the actual canvas root still
requests 81963 pixels of native height: `TNyxLCLRenderer.Layout` passes it to
`TWinControl.SetBounds`, and LCL refuses its widget size range. Failure logs and
stack evidence are retained in WORK.md; no controls are dropped or heights
silently clamped. This gap belongs to the existing native scaling/layout
acceptance path and is a prerequisite for codegen criterion 3's ordinary editor
outcome. Qualify explicit logical scroll extent, safe widget geometry, access to
all descendants, exact selection/input and teardown through the public designer
viewport, retaining browser/LCL parity. This discovery accepts no criterion and
does not reset this task's existing no-closure count of 2.
## Logical native viewport qualified — 2026-10-05

The original 81963-pixel canvas failure is resolved through complete logical
extents and bounded physical projection, retaining every descendant/control.
Public viewport observation/navigation keeps selection painting scroll-neutral.
The native adapter reuses standard LCL scrollbar controls while its inherited
physical offsets stay zero; ordinary small views retain automatic native scrolling.
Actual Win32 mixed-control checks pass 4118, original 128/512/2048 Studio input
passes 45, and portable geometry/viewport/source regressions pass 17/46/39, all
with zero leaks. Current browser counterparts/Studio compile but retain the host
execution gate. English review captures are separate from Unicode qualification.

Criteria 1/2 advance in this bounded scope, without accepting either full original
criterion. Giant native inputs/custom faces and large split panes need explicit
adapters; non-panel client offsets, widgetset/DPI metrics, logical resize events,
accessibility and complete target breadth retain their existing gates. No support
grade, scope or credit is weakened. No-closure count advances 2→3. Reassessment
changes the next action back to original codegen criterion 3's detached
visual/structural reconciliation and comfortable source editing. WORK.md records
failed attempts, final commands/evidence and the unchanged browser/service block.

## Retained structural arrangement — 2026-10-06

Criteria 1/2 now include a separate ownership/publication gate for rearranging an
unchanged realized control set. All owner arrays and implementation anchors are
prepared before publication. Ordinary physical hosts reuse their exact controls,
drafts, ranges and callbacks through reparent/reorder and reversal. Special pane
hosts, additions/removals, changed contracts/creators and live bindings still refuse.
The original scalar guard is unchanged. Shared checks pass 34 per compiler/browser;
exact MCP-authored source passes 91 actual Win32 / 75 actual browser checks at
ordinary and 390-pixel host widths. Native editing regression passes 118, zero leaks.
Both MCP application jobs compile identical English source; both Studios compile.

No complete original criterion is accepted. Renderer no-closure advances 4→5 once;
authoring 22, workflow 9, codegen 28 and delivery 1 remain. End this local foundation
batch. Return to fluent alternate structures, bound-state continuity, ordinary
Studio presentation switching and original parity/accessibility/performance gates.
[WORK.md](../WORK.md#retained-structural-arrangement--2026-10-06) records evidence,
failures, preservation and remote status. The main LAN release remains unchanged.

## Retained bound state — 2026-10-06

Criteria 1/2 now include coordinator-owned admission of identical scalar binding
contracts during retained rearrangement. Independent candidate and baseline
copies use the same idle runtime store; logical nodes, actual controls, the
original subscription and accepted values remain attached. Numeric drafts stay
independent; exact field restoration reads current bound state and resets only
that face. State validation/notification and command reentry refuse publication.
The original unbound guard remains strict and avoids extra state-projection clones.

Actual Win32/browser controls pass 61 checks each against the unchanged bounded
MCP source, including width/visibility, two independent stores, supplementary text,
carets/focus, repeated reparent/reversal, external writes, callbacks, complete
restore refusal, descriptor/constructor refusal and teardown. Native heap tracing
reports zero leaks. The existing retained controls pass 92 native / 76 browser;
shared ownership remains 34 per native compiler/executed browser. State/binding
attachments are explicitly typed fixtures: the semantic base is MCP-authored,
but general MCP state/binding authoring remains an open workflow capability.

No complete original criterion closes. Renderer no-closure advances 5→6 once;
authoring 22, workflow 9, codegen 28 and delivery 1 remain. Stop local same-control
foundation expansion and return to fluent alternate recipes and publication,
ordinary Studio switching and the original parity/accessibility/performance gates.
IME continuity, changed control sets, special pane hosts and other widgetsets/DPI
remain unqualified. Main LAN release and all eight project pairs stay untouched.
See [evidence and return path](../WORK.md#retained-bound-state--2026-10-06).

## Allocation cost in ordinary Studio — 2026-10-06

The unchanged 30-check native Inspector/worker/Undo journey now allocates
23,493,324 blocks versus 78,252,212 before the change, with zero leaks. Exact
property-name lookup avoids temporary candidate names; no persistent measurement
cache or new tree ownership is introduced. Scalar measurement instrumentation is
opt-in. Actual native layout controls pass 2,169 and container inputs pass 27;
browser counterparts pass 2,214 desktop / 2,215 exact-390 and 27 respectively.
The unchanged browser Studio journey passes 74 at each viewport.

Criteria 1/2 advance in this bounded performance prerequisite. Complete native
breadth, widgetset/DPI, accessibility and release performance remain open; neither
full original criterion is accepted. No-closure advances 3→4 once for the integrated
result. Return to the authoring ownership/publication gate for structural
presentations while retaining the original parity and remaining prerequisite
requirements. [WORK.md](../WORK.md#allocation-free-property-lookup--2026-10-06)
records the rejected cache experiment, existing generation regression and exact
qualification/deployment limits.
