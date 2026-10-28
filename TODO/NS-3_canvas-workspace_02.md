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
- [Reliable browser Tree disclosure](DONE/NS-3_tree-disclosure_01.md), discovered in
  the same compact Studio hierarchy; retain the original authoring journey.
- No missing external input prevents implementation. Existing canvas ownership,
  source/history, navigation and target renderer contracts are editable sources.

## Partial implementation checkpoint — 2026-10-27

The opt-in typed choice, shared logical/physical frame, navigation, descriptor6,
crafted generation/literal replay and current-source semantic shapes exist.
Browser transforms preserve a live logical subtree and pass23/23 actual control
checks at desktop/compact widths, plus direct typing/binding/button interaction.
The old Fit passes with more vertical space and fails at the original review
height; both observations are retained. Native contracts pass30, with54 existing
document/history and36 navigation checks and clean heaps. WORK.md owns snapshots,
failed first attempts and final executed evidence.

The native implementation now separates world layout from physical HWND clipping
and restores absolute geometry/font/constraint baselines before each projection.
Its same composed consumer passes37 checks at configured1280/390 host widths:
retained Unicode/focus/selection, geometry/font scaling, full ancestor clipping,
live bound memo edits and button callbacks. Both heaps are clean. The existing
native workspace consumer passes38 checks with a clean heap. WORK.md retains the
first zoom-in/leaked-panel failure and the repaired runs separately.

The next checkpoint normalizes pointer/drag payloads and inverse screen mapping
on both adapters, removes the second browser designer correction, and scales
native button/surface paint metrics and original glyph draws. The same consumer
passes67 native checks at both configured widths, with clean heaps, and46 actual
browser checks at1280/390. Browser clicks, typing/binding and action activation
also pass. Browser visibility queries now include ancestor overflow clips. The
input revision retains124 native group,38 native workspace and74/74 browser group
regression checks. Native painted output is a diagnostic redraw; the real display
capture refused its foreground guard and is not replaced with a display claim.

Native grids now retain absolute columns, default/header/sparse measured body
heights and title/toolbar/alignment metrics through logical adapter updates.
Reentrant native resize admission no longer captures a partially projected tree.
Plain/wrapped/resizable consumers pass107/129/112 checks respectively at each
configured1280/390 width, including actual native F2 drafts/caret/selection,
scroll anchors, query refresh, zoom-aware header dragging, stale release refusal
and retirement during an entered update. All six runs report zero leaked blocks.
Wrapped checks require admitted multi-line text in the current visible window;
query/editor recovery remeasures that bounded window after its final scroll.
Their captures are diagnostic redraws, separately inspected from displayed output.
The native default-height setter still visits its own row storage; bounded Nyx
metric copying alone establishes no large-dataset zoom performance budget.

Pinned panes now retain matching sparse body heights and actual screen row tops
through repeated zoom, distinct vertical scrolls, horizontal scrolling and query
refresh. Keyboard Fit keeps the same logical field width across zoom, including
a pinned field, without committing its unfinished Unicode editor. Its actual
header button keeps scaled bounds after scrolling. The strengthened consumers
pass107/129/122/145 plain/wrapped/resizable/pinned checks at each1280/390 width;
all eight runs report zero leaked blocks. WORK.md retains the initial logical
fit-width, pinned-row, horizontal-scroll and post-scroll button failures.

The reusable Embedded controls form now exposes Reflow, Keep size and Uniform
zoom through ordinary native Studio's paired Inspector queue. Seeded workspace
selection, actual select/button/menu input, exact Undo/Redo, stale/draft refusal
and warned Save/Open pass54/58 at1280/390. Both widths save identical project and
crafted Pascal bytes from the accepted MCP Workshop. Its own compiled application
passes126/126 native and120/120 browser checks, including live Unicode edits,
actual fitted result/action bounds, formatting, program edits and file/HTTP
outcomes through explicit controlled hosts. The existing timing ceilings remain
unchanged; WORK.md retains contention failures and isolated reruns. Full browser
Studio interaction remains separate from its successful compilation.

Injected operation cards now honor the mounted uniform policy through an optional
read-only typed facet. Their stable world allocations, navigation, culling and
pointer conversion share the renderer factor; floating tools and earlier policies
retain their semantics. Repeated zoom and actual native header dragging pass45
checks in all four wide/compact mixed/empty cases, with zero unfreed blocks.
Existing Keep size navigation passes61/64/62/65; browser counterparts pass37 in
each uniform case and48/49 per width for Keep size, plus trusted half-zoom dragging.

The exact saved/generated Workshop's browser journey passes169 at both widths.
Its actual mixed-frame checks preserve positions and forbid zoom-induced overlap.
Saved rows310 world units apart already overlap320-unit allocations, so the
existing honest Limited result is required rather than silently moving them.
The same full native journey passes175 per width with assertion/range/overflow
checks and unchanged deadlines. The full heap-traced wide run times out at
stage22 and remains separate; focused heaps are clean, but the complete traced
journey and performance requirements are not accepted. Selective rendered review
retains exact formatted Unicode/large-integer output through phone/desktop resize
and actual Format/Zoom/Fit clicks. Both native formatted captures are diagnostic
redraws. Ordinary browser Studio and observing delivery remain next.

Retained failure: ordinary browser Studio opens the selected canvas Inspector after repairing
the browser managed-control construction pin. The new shared constructor/form
regression passes13 and the existing managed-control suite194 on each target,
with complete clean native heaps. Choosing Uniform alone does not mutate the
project, and ordinary baseline Save preserves the exact MCP seed. Actual Apply
still hits the existing30000ms source-worker deadline, retaining Reflow and the
complete accepted pair. Worker error/timeout diagnostics and draft preservation
now pass10 browser checks. No full browser Apply/history/Save/Open or delivery
acceptance is claimed. The preparation cost is also tracked by the existing
[measured-performance owner](NS-3_extension-performance_01.md); preserve strict
admission and the deadline while repairing its concrete critical path. WORK.md
records the retained timing evidence and exact next journey.

The full original desktop Studio journey now passes with the measured-performance
container-read correction. Ordinary Uniform Apply completes within the unchanged
worker deadline, acknowledges normally, and preserves the exact accepted pair at18.
Ordinary Undo19 restores the complete original Reflow pair, Redo20 returns Uniform,
and real server Save writes the identical project plus adjacent design/Pascal.
After another ordinary Undo21, Open saved project restores that exact Uniform
pair at22, independently exported through MCP. Its worker/local-delivery samples
are18.2s/15.9s: improved, still not near-real-time qualification. Native188/browser195
data checks and native140 source/design plus queue10 pass with clean native heaps;
both editor targets/backend rebuild. The automatic pre-open browser backup has no
observed download receipt and is not a physical save proof. Original compact
ordinary Studio, full traced native, visual/assistive and remaining interaction
criteria stay open. Next use this same public saved Workshop for compact editor
qualification, then matched delivery; stop standalone container/source variants.

The same full compact Studio journey now also passes at390x844. Its ordinary
hierarchy exposed a browser disclosure/default race; the accepted library fix
retains original input epoch/source and refuses cancelled or stale defaults.
Actual pointer summary activation and keyboard navigation work at both widths.
Compact Undo restores the exact original Reflow pair, Uniform Apply26 returns
the complete accepted pair, Undo27/Redo28 retain both, and real paired Save plus
Undo29/Open30 restore the identical saved Uniform packet/Pascal. A racing export
correctly refuses an intervening revision; an acknowledged rerun passes. Input
observation timeouts and full refresh after semantic reads remain measured
performance work. Fit is visually inspected on the same reopened compact design;
physical/assistive, complete native traced authoring, all clipping/interaction and
original canvas criteria stay open. No smaller fixture substitutes for this run.

Other internal native scroll/drawing metrics, nested canvases, full drag/capture/
keyboard/IME/assistive journeys and complete pre-publication admission for changed
logical layouts remain implementation/qualification work. Full browser Studio
selection, observing delivery and complete native/browser visual/assistive
quality remain open. The installed LAN payload and example defaults are unchanged.
Retain these gates and the authored overlap constraint on this owner rather
than starting more standalone table/source variants;
this task remains unaccepted with no duplicate completion credit.
