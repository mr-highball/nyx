# NS-2_split-platform_02 — Splitter drag snaps back

[North stars](../MILESTONES.md) · [Task catalog](README.md) · [Task flow](../TASKFLOW.MD)

**Status:** Open bug; queued for later investigation at the user's request.

**Reported:** 2026-10-20, during external Nyx testing. The user reports that a
splitter snaps back to its original position when dragged anywhere. This report
has not yet been independently reproduced. The affected application, build,
adapter, orientation and input device are not established.

**Expected behavior:**

Dragging an enabled splitter updates the pane boundary continuously within its
configured limits. Releasing the pointer retains the user's final position;
ordinary layout or view updates do not restore the original configured position.

**Reproduction to establish:**

1. Record the affected application/build, browser or LCL widgetset, viewport,
   splitter orientation, initial position and any size constraints.
2. Drag the splitter grip in both directions. Observe whether it snaps back
   during movement, on release, or after the next layout/publication.
3. Repeat in a minimal public Nyx split-pane consumer and the affected application.
   Check whether dragging content outside the grip also triggers the reset.

**Investigation scope:**

Trace the drag proposal through accepted runtime position, layout and subsequent
view synchronization. Check pointer capture, coordinate conversion and whether
an initial authored value is reapplied over the current user position. These are
investigation leads, not an established cause. Preserve caller-configured limits
and explicit programmatic resets.

**Acceptance criteria:**

- Reproduce the external report and retain a failing regression before the fix.
- Verify horizontal and vertical drags, repeated drags and release retain the
  accepted position through ordinary reflow and view synchronization, with no
  unexplained reset to the initial value.
- Verify nested panes and min/max constraints without oscillation, out-of-range
  positions or unintended resizing from ordinary child-content interaction.
- Check pointer/touch cancellation, capture loss and disposal: no stuck drag or
  stale callback may move a retired pane. Disabled splitters retain their policy.
- Check keyboard resizing and the unaffected adapter for regressions. Qualify
  real browser/LCL interaction separately from compilation; document any remaining
  platform-specific gap rather than claiming parity from a build alone.
- Preserve intentional caller-driven position changes and explicit resets;
  document the boundary between initial configuration and current user state.

**Ownership and related work:**

NS-2 renderer/runtime regression, consumed by NS-4 Studio. The prior
[split-pane/platform task](DONE/NS-2_split-platform_01.md) is historical acceptance
evidence; this ticket tracks the newly reported regression independently. See the
[browser](NS-2_browser-renderer_01.md), [LCL](NS-2_lcl-renderer_01.md) and
[parity](NS-2_parity-accessibility_01.md) owners for broader requirements.
Planning adds no completion credit or counter change. Implementation is deferred;
continue the current product task after filing this ticket.
