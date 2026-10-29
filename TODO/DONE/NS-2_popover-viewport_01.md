# NS-2_popover-viewport_01 — Keep embedded browser popovers reachable

[North stars](../../MILESTONES.md) · [Task catalog](../README.md) ·
[Table workbench](../NS-3_control-usefulness_03.md) · [Task flow](../../TASKFLOW.MD)

**Description:**

Keep ordinary Nyx menus, inspectors and other popovers inside the actually
visible part of an embedded browser application. The service monitor's compiled
Studio preview reproduces a pointer failure: its iframe starts44 pixels above
the clipping layer, while the first context-menu action starts21 pixels below
the iframe origin. Its click centre is clipped. Enter activates the same action,
and full-value inspection correctly preserves the unfinished Service edit.

The browser popover currently places against its own window/VisualViewport.
That rectangle does not account for a clipped parent browsing context. Reuse the
portable popover rectangle/placement contract and browser adapter; do not change
table actions, discard drafts, select another control or require maximizing the
editor to hide the geometry failure. This serves NS-2's target/layout quality
and the existing table/Studio acceptance criteria, without additional credit.
WORK.md and the private service-browser-workflow-01 packet retain the actual
geometry, failed pointer attempt, successful keyboard path and saved producer.

**Acceptance Criteria:**

- Place against the visible embedded viewport in the caller's coordinate space,
  including relevant same-origin iframe ancestors and overflow clipping. Cover
  negative offsets, borders/padding, independent axes and supported axis-aligned
  scale. Preserve existing top-level and visual-viewport behavior. State limits
  for unavailable parent geometry and unsupported transforms honestly.
- Opening and ordinary ancestor scroll/resize keep the whole reachable popup
  allocation within that viewport, with content scrolling when necessary.
  Hidden/detached or wholly clipped anchors do not strand an active presentation.
- Borrowed host/window observations and listeners retire on close/destruction;
  queued or reentrant work cannot revive a closed presenter or affect a successor.
  Preserve contextual families, focus return, cancellation and actual input.
- A maintained Pascal browser consumer reproduces the original geometry failure
  and exercises the repaired embedded cases. Retain existing portable/native
  placement checks and demonstrate ordinary top-level compatibility.
- Return the same saved service monitor through Studio's compiled preview at
  wide/compact sizes. Pointer activation reaches the first cell-menu action,
  complete raw inspection works and the exact suspended edit resumes. Preserve
  the accepted project/Pascal/default records; retain representative images.

**Acceptance — 2026-10-29:**

All five criteria are qualified. The maintained
[embedded browser consumer](../../tests/nyx_popover_embedded_controls.lpr)
reproduces the original vertical clipping failure before the adapter repair.
The final consumer passes at1280x720 and390x844, including frame borders/padding,
negative offsets, independent overflow axes, uniform/nonuniform positive scale,
scroll/resize, nested ancestor clips and hidden/wholly clipped anchor retirement.
Released presenters do not revive after parent movement. Its63/64 checks include
bounded readiness polling; they are not separate feature credit.

The unchanged ordinary browser assertions pass42 at both widths, and nested
focus/input/retirement assertions pass25 at both widths. The native consumer
passes40 actual Win32/control checks, including portable placement, with a
complete zero-unfreed heap. The optional `?run` mode only releases the existing
screenshot checkpoint; no assertion is omitted. The adapter reuses its existing
layout timer and installs no additional parent observation hooks. The documented
contract states cross-origin and unsupported-transform limits explicitly.

The same exact saved Service monitor builds through MCP with its Table selected.
Ordinary compiled-preview pointer input reaches the first cell-menu action at
both widths, opens the complete raw gateway command and resumes the exact
unfinished Service edit. Its authored pair/default records, selection, revision
and history availability remain unchanged. Qualified browser assets are served
by both existing QA/LAN processes; all three LAN project pairs and output settings
remain exact. The external popover-viewport-01 packet retains baseline/final
results, compiler logs, runtime observations, images and delivery receipts.

This accepts only the embedded-popover prerequisite. Original table-workbench,
physical-device/assistive, broader platform and production-quality outcomes stay
with their existing owners. Return directly to that workbench; no further
geometry fixture expansion is needed without another concrete failure.

**Blockers**

- None.
