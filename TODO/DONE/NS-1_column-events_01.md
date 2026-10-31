# NS-1_column-events_01 — Author typed Table column-change callbacks

[North stars](../../MILESTONES.md#assessment) · [Task catalog](../README.md) ·
[Table workbench](../NS-3_control-usefulness_03.md) · [Task flow](../../TASKFLOW.MD)

**Description:**

Give application authors a first-class, typed columns-changed event through
Nyx's public fluent contract and Studio's ordinary Events workflow. A practical
use is showing which field changed or saving a user's arrangement after resize,
reorder, show/hide, pin/unpin, Fit or reset, without reverse-engineering a generic
view notification or binding to DOM/LCL internals.

At task opening, source supplied the runtime operations, but `TColumnPreparation.Notify`
called the existing view observer with nil. `TNyxTrigger` had no column-change
event, and the standard collection schema advertised selection changes rather
than an arrangement callback. Custom named events do not supply that missing
built-in bridge. Source evidence and the full current workbench regression are
recorded in WORK's workbench criterion assessment.

Owner: NS-1, consumed by workbench03 criterion6 and Nyx Studio. No separate
percentage or duplicate component credit is assigned. Extend the existing event
router, scheduler, publication and schema/generation owners. This is one useful
event family, not a replacement event system or complete collection-event audit.
The broader scheduler owner and original workbench criteria remain intact.

**Accepted — 2026-10-31:** All five original criteria are satisfied for the
qualified pas2js browser and Win32 LCL implementations. This closes the scoped
column-event prerequisite, without accepting the full parent workbench or its
separate Win32 cursor/message and hardware/assistive requirements.

| Original criterion | Accepted evidence |
| --- | --- |
| 1 — Typed contract | Native/browser48 qualify immutable copied deltas, exact typed fields, opaque view identity, serials, explicit origins, optional capability and prior interface compatibility. |
| 2 — Publication semantics | The same48 and actual generated applications retain grouped admission, no-op/stale/refused/cancelled behavior, mixed observer order, notification failures and retired/hidden generation boundaries. |
| 3 — Target bridges | Both actual managers/Fit/reset and programmatic paths execute. Native85 at1280x820/390x820 also drive the actual LCL sizing handlers and their normal HeaderSized/release path, including LTR/RTL, Escape and unchanged sizing. Browser54 at verified1280x650/390x650 retains actual resize-handle keyboard dispatch. Multiple policies, detached async payloads and successor refusal remain. |
| 4 — Studio/MCP | Source15 plus the live revision-aware MCP/Inspector journey qualify typed help, ordinary Add/source focus, two ordered callbacks, UI-queue policy, guarded handwritten implementations, strict saved metadata, paired exact Undo/Redo and stale-revision refusal. Unbound Tables do not advertise this functioning event. |
| 5 — Useful saved application | Both delegated compilers accept the same source/design bytes, verified against all downloaded manifest entries. That exact exported application reports the changed field and records preferences; native85/browser54 exercise callbacks, failures, cancellation, retirement and independent peers. The native caption and actual header width also agree after resizing. Native heaps are clean; ordinary compact browser Hide visibly changes status while its peer retains the column. |

Evidence packets are column-events-01, column-events-integration02 and
column-events-native-input03. Current product sources match the retained
14-file product identity set; only the qualification consumer changes in the
last packet. WORK retains hashes, dimensions, clean heaps and earlier failures.
The new browser output is byte-identical to the already-executed54-check artifact.
The first cancellation assertion sent only LCL's after-interface key remainder;
entering the actual KeyDown/KeyUp handlers repairs that harness. No dependency
or product source is patched, no internal grid state is assigned and no
HeaderSized callback is fabricated. Software-driven widget input is reported
as such. The parent's original cursor assertion and pending desktop runner
remain unchanged and unwaived.

**Acceptance Criteria:**

- Public typed registration exposes committed column-arrangement changes with
  an immutable, detached payload: scoped control/view identity, before/after
  serials, stable typed fields and meaningful width/order/visibility/pin changes.
  Distinguish reset/user/programmatic origins where authoritative; never invent
  an origin from the final value. Payloads own no node/widget/renderer cycle and
  grant no publication authority. Built-in authoring uses closed types and fluent
  constructs; strings remain explicit persistence/extension boundaries.
- An admitted changed operation emits once, including grouped changes. No-op,
  refused, cancelled or stale requests emit no successful-change event. Define
  compatible recompose/replacement behavior; hidden preparation candidates must
  not dispatch application callbacks. Preserve the existing generic observers,
  their order and committed-notification failure semantics. A throwing receiver
  cannot roll back installed preferences or prevent independent observers.
- Browser and LCL bridge actual manager, resize/Fit and public programmatic
  operations through the same contract. Preserve earlier interface GUIDs and
  alternative implementations through an optional capability. Retired mounts,
  delayed work and same-ID successors cannot inherit an old event's authority.
  Multiple registrations and per-event execution policy use the existing router
  and scheduler. Async payload reads remain detached; UI access keeps existing
  thread checks. Do not add a second scheduler or fabricate an async veto stage.
- Studio exposes the applicable callback, typed payload help and multiple
  registrations for the selected bound Table. Ordinary Add navigates to the
  generated implementation/template through existing source handling. Crafted
  Pascal, strict saved metadata, paired Undo/Redo and bounded revision-aware MCP
  authoring/inspection agree. Missing capability is described honestly; an
  unbound or unsupported face must not promise a functioning event.
- Qualify a useful public application authored through Studio/MCP: a real column
  operation updates a status or records a preference using the callback payload.
  Execute its own saved/generated browser and native application at desktop and
  compact allocations. Check no-op/refusal, multiple callbacks, callback failure,
  cancellation/retirement, independent peers and exact Unicode field identity.
  Retain failures and clean native heaps, distinguishing owned widget/synthetic
  input from hardware/assistive checks. Do not substitute a store-only fixture
  or another latency benchmark for the authored workflow.

**Blockers**

- [Observable collections](NS-1_state-collections_01.md) supplies the accepted
  typed view/publication and independent-runtime foundation.
- [Changed state presentation](NS-2_state-projection-sync_01.md) supplies
  accepted notification/recovery behavior for the callback's bound status output.
