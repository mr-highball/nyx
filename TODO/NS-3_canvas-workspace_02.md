# NS-3_canvas-workspace_02 — Readable, interactive canvas zoom

[Milestones](../MILESTONES.md) · [Tasks](README.md) ·
[Canvas owner](NS-3_canvas-workspace_01.md) · [Tool Canvas](NS-3_tool-canvas_01.md)

**Description:**

Deliver a useful zoomed workspace with live embedded Nyx controls, consuming the
original canvas owner's transform, interaction and visual-quality requirements.
This is a granular prerequisite of that existing outcome, with no duplicate
completion allocation. The current scaling choices intentionally reflow an
allocation with unchanged fonts, or preserve its logical size. Neither supplies
uniform scaling of a live composed control tree.

Actual browser qualification of the same Studio-built JSON Workshop exposes the
user-facing gap: Fit admits a 285x609 host for the authored 450x960 notebook,
while its reflowed card becomes977 pixels tall. The formatted result starts
below the clipped host. Phone input is functional, but the card is also clipped
horizontally. These are observed DOM/paint results, not a failed HTTP transport.
WORK.md owns the retained screenshots and measurements. Preserve both existing
policies and their historical behavior; implement the missing capability in
the shared library/adapters rather than special-casing the example or Studio.

**Acceptance Criteria:**

- A strongly typed fluent scaling choice supports uniform zoom of an ordinary
  live embedded Nyx subtree. World layout, typography, spacing, images and chrome
  scale coherently; zoom does not repeatedly reflow or compound prior scaling.
  Keep the two existing policy ordinals, semantics and persisted encodings.
- Browser and LCL implementations retain interactive controls, exact text,
  selection/caret, enabled/read-only state, binding, events and resource lifetimes.
  Do not replace controls with frozen images. Document actual widgetset limits
  and meaningful caller choices; an unsupported native fallback alone does not
  establish this task's target parity.
- Fit/reveal/culling and descendant visibility use the actual painted/control
  extents and clipping chain. A successful complete Fit cannot leave a result or
  action clipped merely because reflow outgrows the allocated item. Preserve the
  existing honest best-effort result when caller bounds make full fit impossible.
- Pointer positions, hit testing, editing, drag/capture, wheel/pinch anchors,
  keyboard focus/reveal and accessible navigation remain correct through zoom
  and pan. Qualify actual browser and native input separately from numerical
  tests and physical hardware. Exercise supplementary Unicode and repeated zoom
  transitions with retained focused editors.
- Versioned declarations, crafted Pascal, literal source admission, ordinary
  Studio controls and bounded semantic scene operations preserve the choice
  through Save/Open and paired Undo/Redo. User-authored older documents keep
  their intent; changing the starter does not silently change existing projects.
- The same public Workshop and a useful multi-control scene demonstrate readable
  navigation at wide/compact sizes with real rendered results, reachable actions
  and retained accepted frames. Use the same accepted pair for target builds;
  inspect the finished browser/native presentations rather than accepting an
  allocation-only assertion. Respect existing admission/performance limits and
  retain refusal/retirement evidence.

**Blockers**

- [Managed component interfaces](DONE/NS-1_component-interfaces_01.md).
- No missing external input prevents implementation. Existing canvas ownership,
  source/history, navigation and target renderer contracts are editable sources.
