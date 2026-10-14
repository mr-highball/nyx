# NS-3_diagram-editor_01 — Composed diagram editor and connectivity

[North stars](../MILESTONES.md) · [Task catalog](README.md) · [Task flow](../TASKFLOW.MD)

**Description:**

The user requests a diagram designer with draw.io-equivalent behavior as the
proof and extended control built on Nyx's general infinite workspace. NS-3 owns
the diagram engine and public reusable editor recipe; Studio and agents consume
that same API. Preserve the full behavior goal through an explicit reference
matrix rather than calling a boxes-and-arrows demo complete. Reference behavior
comes from the official [features](https://www.drawio.com/docs/features/),
[manual](https://www.drawio.com/docs/manual/),
[layers](https://www.drawio.com/docs/manual/layers/) and
[editor configuration](https://www.drawio.com/docs/reference/configure-diagram-editor/)
checked 2026-10-13. Nyx implementation/assets remain independently authored.
No completion allocation or aggregate credit is inferred from the expanded scope.

**Acceptance Criteria:**

- Own a published reference matrix for canvas/pan/zoom, shapes/libraries,
  text/formatting, selection/marquee, move/resize/rotate, alignment/distribution,
  snapping/guides, grouping/containers/swimlanes, layers/locking/visibility,
  multiple pages, clipboard/duplicate, undo/redo, search, keyboard/touch and
  import/export. Every gap has an owner; feature parity requires passed evidence.
- Typed owned graph objects expose stable vertices, ports, endpoints and edges;
  connectivity remains valid while shapes move, resize, rotate, group or retire.
  Support fixed/floating attachment, free ends, self-loops, multiple/parallel edges,
  straight/polyline/orthogonal/cubic curves, editable waypoints, labels,
  independent arrowheads and styles, rerouting and deterministic hit-testing.
  Optional [behavior wires](NS-1_canvas-flow_01.md) use typed control/script ports;
  visual diagram connectivity and executable connectivity remain distinct.
- Extensible shape/connector libraries accept custom Nyx content, resource-bound
  hosted/embedded images, named ports and behavior programs. Derived reusable
  recipes retain independent state/resources/customization and ownership.
- Public specialized interfaces/factories build the entire editor with Nyx,
  including palettes, inspector, layers/pages, minimap, toolbar and accessible
  alternatives. Adaptive presentation preserves usable canvas area and text.
- Portable serialization and readable generated Pascal retain graph references,
  styles/programs/resources and exact history. Bounded MCP tools inspect nearby
  items/connections and batch semantic graph edits using expected revisions,
  atomic admission, diagnostics and paired Undo/Redo.
- Qualify the behavior matrix on actual browser/LCL controls, including failed
  edits, drafts, independent projects and retirement. Selective desktop/narrow
  previews prove aesthetics; input, accessibility, large-scene budgets and
  observing delivery are qualified separately. Draw.io compatibility import/export
  is an explicit format contract, not assumed from interaction similarity.

**Blockers**

- [Infinite canvas workspace](NS-3_canvas-workspace_01.md).
- [Canvas behavior programs](NS-1_canvas-behaviors_01.md).
- [Typed behavior graphs](NS-1_canvas-flow_01.md).
