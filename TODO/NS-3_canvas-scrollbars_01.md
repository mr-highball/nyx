# NS-3_canvas-scrollbars_01 — Coherent scroll chrome in zoomed controls

[North stars](../MILESTONES.md) · [Tasks](README.md) ·
[Canvas zoom owner](NS-3_canvas-workspace_02.md) · [Task flow](../TASKFLOW.MD)

**Description:**

Keep scrollable embedded controls usable and visually coherent through uniform
canvas zoom. The same full Workshop's native diagnostic redraw shows fixed-size
scroll arrows/tracks dominating reduced text and client areas. Source inspection
confirms standard TMemo scrollbars while native uniform projection records
geometry/fonts/constraints and specialized grid metrics, without a general
scroll-chrome projection contract. Treat displayed geometry/input as a required
baseline; the diagnostic image alone does not qualify physical rendering.

This is a granular prerequisite of NS-3/canvas-workspace02's existing complete
live-control/chrome requirement, counted once within that owner. Reuse the current
scrolling, renderer and canvas abstractions. Any caller-facing choice must be
typed, portable and extensible; platform limitations must be explicit without
accepting an unsupported native fallback as parity. Ordinary unzoomed scrolling
and the two older canvas policies retain their existing intent.

**Acceptance Criteria:**

- Logical scroll chrome and client allocation agree with the applied uniform
  factor, including tracks, thumbs, arrows, corners and actual hit regions. Define
  minimum interaction geometry and honest caller choices through typed contracts,
  with theme support. Preserve platform-default behavior outside the opt-in scope.
- Reuse one mechanism across supported scrollable projections, including memo,
  code editor/block, scroll area and bound data controls. Retain actual scroll
  position, selection/caret, unfinished edits, read-only/enabled state and visible
  content through repeated zoom, resize, query refresh and return to scale1.
- Native/browser input exercises real scrolling, keyboard/wheel actions, focus
  reveal and edge/corner bounds at wide/compact sizes. Original source/geometry
  cannot drift or compound. Retirement/reentry during host callouts cannot publish
  stale projection or lose the retained accepted frame. Keep bounded collection
  and existing admission/performance limits.
- The same saved/generated Workshop and a mixed scrollable-control consumer
  demonstrate the result on both adapters. Inspect representative rendering and
  distinguish diagnostic redraw, actual display and physical hardware evidence.
  An allocation-only assertion or fixed native fallback is insufficient.
- If a public policy is added, preserve older descriptor/default behavior and
  carry the typed choice through crafted Pascal, literal admission, ordinary
  Studio properties, semantic edits, paired history and Save/Open. Document
  actual widgetset support and unavailable behavior without changing dependencies.

**Blockers**

- [Managed component interfaces](DONE/NS-1_component-interfaces_01.md).
