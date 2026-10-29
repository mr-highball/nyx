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

## Partial Win32 checkpoint — 2026-10-28

Portable typed ranges/geometry and the reusable native attachment now project
nonclient allocation and hit regions while retaining the real HWND. Focused
native86 checks cover memo focus/selection/anchors, read-only/disabled input,
source replacement, full-width vertical and horizontal positions, deferred grid
thumb commit, input-epoch cancellation, tiny clients and retirement. Portable
geometry executes1418 native/1420 browser checks; the extra two browser checks
refuse foreign numeric inputs. Existing native live-control67 checks pass at
both widths. Complete native heaps are clean. The maintained `scroll-chrome`
build target retains these checks and stages the browser geometry fixture.

The same exact saved/generated Workshop passes its original full native175/175
runtime and ordinary Studio54/58 history/Save/Open journeys. The first compact
failure retained an older410px HWND width inside a341px viewport; realizing LCL
bounds before frame recalculation repairs it without changing its assertion or
deadlines. Final widgetset/queued-input guards have separate focused evidence.
WORK.md retains exact snapshots, failed attempts, limitations and target evidence.

The task remains open. Other scrollable families, browser/native input breadth,
actual display/assistive geometry, caller choices and remaining original criteria
still need qualification. At this checkpoint the native PrintTo diagnostic
followed the stock nonclient path; the continuation below addresses that gap.

## Native print continuation — 2026-10-28

The shared Win32 attachment now prints its live nonclient face instead of stock
scrollbars. It preserves borrowed DC state, measured origins, restrictive clips,
visibility flags and nested children. The original baseline changes the caller
clip; the first repair reveals a missing client-origin translation. Both failures
are retained. Visual review also exposes reversed arrow glyphs and active-looking
thumbs on fully paged ranges; semantic pixel/input checks qualify both repairs.

Native453 covers the four supported families, both axes, three scales, hidden/
disabled refusal, explicit hidden previews, a recreated one-pixel memo and actual
client-paint retirement. Its complete heap is clean. Existing native86 input and
both67 live-canvas checks remain separate. The same exact saved/generated
Workshop retains its full175 journey at both widths; WORK.md owns the final
source snapshots and rendered inspection. The maintained target retains this
new consumer. Zero owned foreground captures are available, so no diagnostic
image is relabelled as actual display. All original remaining criteria stay open.
