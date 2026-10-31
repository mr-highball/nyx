# NS-2_split-platform_02 — Splitter drag snaps back

[North stars](../MILESTONES.md) · [Task catalog](README.md) · [Task flow](../TASKFLOW.MD)

**Status:** Open regression; native release defect repaired and both-axis
release/reflow regression criteria accepted. External-application reproduction
and trusted browser capture/device evidence remain open.

**Reported:** 2026-10-20, during external Nyx testing. The user reports that a
splitter snaps back to its original position when dragged anywhere. A matching
release defect is now reproduced in a public Win32/LCL consumer. The originally
affected application, build, adapter, orientation and input device remain
unestablished; this narrower reproduction does not identify that application.

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
Planning adds no completion credit or counter change. The original filing
deferred implementation; the current WORK entry now owns active investigation.

**Native progress — 2026-10-25:**

- The former fixture called protected mouse handlers directly. Complete native
  message delivery exposes LCL's automatic capture release before `MouseUp`:
  Nyx interprets it as capture loss, cancels the drag and restores its baseline.
  The retained pre-fix run fails the release-position assertion with a clean
  heap. The grip now disables automatic capture and uses its existing explicit
  acquire/commit/release sequence; genuine loss still cancels.
- The current fixture passes 145 checked native assertions through injected
  Win32 window messages. It exercises repeated drags in both orientations,
  nested independence, in-drag/post-release Sync, host reflow, authored/runtime
  separation, explicit replacement configuration, child-content input, bounds,
  pane minima, Unicode drafts/ranges, read-only/disabled policy, Escape, capture
  loss, keyboard callbacks and unmount during capture. Its complete heap is clean.
- Qualification and earlier test-driver failures are retained under ignored
  `.local/qualification/splitter-snapback-01`. Native message injection is distinct
  from physical mouse/touch testing. Browser compilation does not establish
  rendered browser behavior. The unchanged original acceptance criteria above
  remain the conditions for DONE; this bounded native repair adds no broad parity
  or completion credit.

**Browser return — 2026-10-30:**

- The unchanged public-control consumer executes38 checks at1280x900 and390x844.
  Its actual DOM controls retain Unicode input/ranges through resizing, permission
  cancellation and minima, and qualify keyboard hooks/default consumption and
  callback disposal. These synthesized events do not obtain trusted capture.
- The existing Studio consumer now waits for queued presentation publication and
  recognizes hidden retained source controls. All original20 checks pass at both
  measured widths, followed by the original nine compact/desktop return checks
  requiring the child's full20-check pass. Draft/range/focus, independent scrolls
  and the chosen position survive the relevant resize and shell changes.
- Source98648722/artifact84bc5244, failures, routing/driver corrections and exact
  receipts are retained under external splitter-browser-return-01. Standalone
  error logs are empty; the iframe logs one unattributed MutationObserver error,
  so a clean iframe-console claim is not made. No product splitter code changes
  in this checkpoint. The native release source remains unchanged since2bbcf42f.
- Original criteria remain open, including both browser axes/nested repeated Sync,
  original external reproduction and trusted capture/device behavior. WORK owns
  the reassessment and bounded return; no new task or completion credit is added.

**Both-axis regression acceptance — 2026-10-31:**

The maintained public-control consumer now passes132 browser checks at measured
1280x900 and390x844, with empty error logs. Both axes contain an opposite-axis
nested split. Each repeated drag continues after in-drag Sync, commits exactly
one change on release, ignores subsequent capture loss and retains the accepted
runtime position after another Sync. Actual width/height reflow preserves both
preferences and the exact memo, supplementary-Unicode draft, range and focus.
Content input, Escape/late release, disabled input, explicit authored replacement
and active-input/queued-resize retirement also pass. The prior minimum/bounds and
typed keyboard/default/callback checks remain intact.

The same source634308df separately compiles for Win32/LCL and passes the original
145 native message checks, freeing all122272 heap blocks. Compilation has zero
warnings and two existing notes. Its854.2-second build is retained as an actual
slow build, with advancing process I/O and no restart; no build-speed claim is
made. Native executabled85ef9ec, browser artifact12e9e176/runtimea6c5edc8 and the
complete receipts are retained under external splitter-browser-axes-01.

Criteria2/3/5/6 have accepted regression evidence within this actual Win32/LCL and
browser matrix. These results establish injected native messages and synthesized
DOM pointer/keyboard behavior; they do not identify the original external app or
obtain trusted browser pointer capture. Criteria1/4 remain open for those inputs.
No runtime splitter source changes, task completion or north-star credit is
assigned. WORK records the stop point and return to the ranked control workflow.
