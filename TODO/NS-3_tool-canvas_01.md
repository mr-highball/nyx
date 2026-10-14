# NS-3_tool-canvas_01 — Embedded-control and script Tool Canvas

[North stars](../MILESTONES.md) · [Task catalog](README.md) · [Task flow](../TASKFLOW.MD)

**Description:**

The user explicitly requests Tool Canvas as a second extended workspace control,
alongside the draw.io-style diagram editor. Its intended consumer builds interactive
tools by adding normal Nyx controls, exposing typed ports and wiring controls or
scripts. NS-3 owns the public specialized compound recipe and starter experience;
canvas, behavior programs and typed graph execution remain separately credited
prerequisites. It is not a second rendering toolkit or a requirement to finish
the entire diagram editor first. This expanded scope earns no planning credit.
The subsequent [nonvisual Logic collection](NS-3_logic-components_01.md) adds
reusable file/JSON/query/text/network/data/flow operations with typed ports.

**Acceptance Criteria:**

- A managed specialized Tool Canvas factory/interface composes the public canvas,
  Nyx controls, control/script palette, port inspector, source editor, connection
  diagnostics and execution feedback. Named reusable parts, public customization
  and independently owned instances permit creators to reshape the experience.
- Designers add existing/custom Nyx controls and scripts, toggle port chrome,
  inspect compatible inputs/outputs and create/remove behavior wires using
  pointer or accessible keyboard commands. Normal control editing/input and
  static behavior remain usable alongside explicitly enabled graph behavior.
- Run/pause/stop and per-item/graph enablement expose real runtime state. Typed
  payloads, script diagnostics and execution failures are visible; cancellation,
  stale results, removal and navigation retire subscriptions/work safely.
- Portable projects retain scene items, control subtrees, programs, graph edges,
  resources and policy. Readable Pascal and ordinary paired Studio history preserve
  exact meaning. Bounded MCP composition/query/mutation/build operations use the
  same public contract, expected revisions and grouped undoable edits.
- An English starter tool connects ordinary input/control events to a script and
  resulting control updates, including embedded/hosted image resources. It runs
  in actual browser/LCL consumers with independent projects and retained editing.
  Desktop/narrow previews, input/accessibility, budgets and observing delivery
  qualify the intended experience separately from portable graph fixtures.

**Blockers**

- [Infinite canvas workspace](NS-3_canvas-workspace_01.md).
- [Pascal behavior programs](NS-1_canvas-behaviors_01.md).
- [Typed behavior graphs](NS-1_canvas-flow_01.md).
- [Nonvisual logic components](NS-3_logic-components_01.md).

Current-source 2026-10-14 checkpoint adds the specialized managed Tool Canvas,
independent named workspace/inspector parts and an ordinary Nyx port inspector.
Embedded static callbacks and saved graph routes share application state through
revocable item-owned contexts. The inspector searches/pages real event/state
ports and controls graph pause/cancellation without disabling static callbacks.
WORK.md owns exact input/source/history evidence and retained failures. No full
criterion closes: wire authoring/keyboard gestures, script palette/editor, richer
semantic ports, independent-project breadth, quality/budgets and delivery remain.
Next advance useful operation breadth and connection editing, rather than adding
another read-only port face or claiming the full requested nonvisual catalog.
