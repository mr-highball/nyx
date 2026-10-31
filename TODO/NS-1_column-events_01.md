# NS-1_column-events_01 — Author typed Table column-change callbacks

[North stars](../MILESTONES.md#assessment) · [Task catalog](README.md) ·
[Table workbench](NS-3_control-usefulness_03.md) · [Task flow](../TASKFLOW.MD)

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

Progress on2026-10-31: criteria1,2,4 and5 are accepted. The typed data/registration
contract retains native/browser48 and Studio source15. Live Studio Events/Add
focuses the generated implementation; bounded MCP authoring retains two ordered
callbacks, UI-queue policy, exact paired Undo/Redo and stale-revision refusal.
Both delegated application compilers accept the same saved source/design bytes.
That exact exported unit passes native52 at1280x820/390x820 and browser54 at
verified1280x650/390x650, including actual manager Hide/Fit/grouped Reset,
no-op/refusal, callback failure/cancellation, async copied payloads, retired and
same-ID mounts, independent peers and supplementary Unicode field identity.
Native heaps are clean. An ordinary compact browser Hide also visibly changes
the status to pid while the peer retains that column. These are owned widgets
and browser automation, not hardware/assistive qualification.

Criterion3 and the complete task remain open for the actual native resize path.
Keep the original native pointer gate and pending desktop result; no fabricated
HeaderSized callback substitutes for it. WORK owns exact source identities,
manifests and retained harness/timeout failures in column-events-integration02.
Earlier column-events-01 failures remain. This is no full workbench03 criterion6,
Studio or L2 acceptance. Do not replay another equivalent payload/manager/
preference fixture; finish the native input gap and return to the parent task.

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

- [Observable collections](DONE/NS-1_state-collections_01.md) supplies the accepted
  typed view/publication and independent-runtime foundation.
- [Changed state presentation](DONE/NS-2_state-projection-sync_01.md) supplies
  accepted notification/recovery behavior for the callback's bound status output.
