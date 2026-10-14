# NS-1_canvas-flow_01 — Typed embedded-control and script behavior graphs

[North stars](../MILESTONES.md) · [Task catalog](README.md) · [Task flow](../TASKFLOW.MD)

**Description:**

The user's 2026-10-13 enhancement makes embedded Nyx controls optionally expose
input/output ports. Designers retain static compiled behavior or enable end-user
graph wiring from events/values to other controls and interpreted scripts. NS-1
owns the typed port/graph/runtime contract. Visual diagram edges remain a separate
representation: drawing a line never silently creates behavior. NS-3 canvas/diagram
controls and NS-4 Studio/MCP consume this same contract. This newly required scope
earns no planning or duplicate component credit.

**Acceptance Criteria:**

- Specialized managed interfaces expose distinct stable port references,
  direction, data-versus-event/action semantics, payload/value type, help,
  multiplicity and creator-defined names. Component properties, published event
  payloads and commands supply discoverable capabilities. Unsupported/read-only
  inputs are not fabricated. Scripts declare typed input/output schemas.
- Typed authoring enables behavior wiring and shows/hides ports per workspace
  or item. Visibility, graph execution and static callbacks are separate choices.
  Ordinary controls remain usable with port chrome hidden; explicit mixed
  callback/graph policy prevents accidental duplicate execution.
- Wire admission validates the whole candidate graph, exact owners and type
  compatibility. Conversions require explicit typed transforms. Missing ports,
  incompatible payloads, cardinality conflicts, cycles and retired controls
  refuse or follow a documented explicitly selected feedback policy. Cycles
  never become an accidental infinite execution loop.
- Runtime defines data propagation, event order/fan-out, grouped writes,
  reentrancy, errors, cancellation and budgets. Nyx scheduler policy governs
  sequential/asynchronous/worker work and UI publication. Independent runtime
  instances own revocable subscriptions without control/document/renderer cycles
  or stale deliveries after navigation/disposal.
- Public Nyx canvas/diagram controls let users drag outputs to compatible inputs,
  with diagnostics, keyboard/accessibility alternatives, enablement and confirmed
  removal. Scripts receive control events/data and publish typed control updates.
  User extension requires no library fork.
- Versioned persistence, crafted Pascal and source admission retain schemas,
  edges and programs. Bounded semantic queries expose ports/neighborhood/errors;
  grouped expected-revision edits are one undoable operation. Pending drafts and
  failures preserve the accepted pair.
- English event-to-script-to-control and data-to-control examples execute in
  actual browser/LCL applications with multiple registrations, independent
  instances and retirement. Budgets, desktop/narrow presentation and observing
  rollout are qualified separately from graph-only fixtures.

**Blockers**

- [Managed component interfaces](DONE/NS-1_component-interfaces_01.md).
- [Owned document model](DONE/NS-1_model_01.md).
- [Canvas behavior programs](NS-1_canvas-behaviors_01.md).

Canvas is a related host consumer, avoiding a cyclic prerequisite for the graph
engine. Actual control/script/canvas integration is still required for acceptance.
