# NS-4_studio-chrome_01 — Make Studio a shared Nyx design showcase

[Task flow](../../TASKFLOW.MD) · [Studio authoring](../NS-4_studio-authoring_01.md) ·
[Accepted tasks](README.md) · [North stars](../../MILESTONES.md)

## Description

The user's explicit facelift request owns this work under NS-4. Studio must
showcase the same typed Nyx controls that application authors use, with useful
visual hierarchy on FPC/LCL and pas2js. Starting evidence was the crowded shell
captured in the user's screenshot and the accepted C1 Save/Open checkpoint163635c.
This independently requested scope has no assigned original-goal completion
credit; no earlier component, authoring or parity criterion is closed by it.

## Acceptance Criteria

- [x] Add a pure typed button presentation contract for image/resource-backed
  glyphs, normalized atlas cells, theme ink, logical size/gap, caption mode,
  density, alignment and font weight. Preserve old managed interface contracts
  through an optional facet. Validate canonical persistence and source/Inspector
  round trips.
- [x] Render and measure that contract in both adapters, using existing resource,
  cache, cancellation and image lifecycle owners. Glyphs are decorative inside
  the existing button; keep full command names, keyboard/focus/pressed/disabled
  behavior and caption fallback when imagery is unavailable.
- [x] Apply a shared Studio chrome profile with clear project/activity navigation,
  grouped primary/secondary commands, readable typography, useful inspector
  Properties/Events hierarchy, and responsive compact/touch behavior. Keep
  existing command IDs, selection, scroll, source drafts, splitters and history.
- [x] Package the selected generated glyph atlas locally with prompt/provenance
  and resource reuse. Do not ship rejected transparency variants or depend on
  remote assets/packages. Studio chrome and edited-project theme remain independent.
- [x] Inspect and repair encountered control/property/event gaps through shared
  metadata and editor routes. New behavior must remain typed and fluently
  authored; styling must not hide capabilities or break editing/Save/Open.
- [x] Review/freeze both source scopes, run focused portable/adapter checks and both
  Studio builds, then inspect actual desktop and compact browser/native results.
  Retain old checks and every failure. Temporary loopback visual hosts are
  stopped after review; no LAN/config/MCP/wire expansion follows. Actual DPI,
  hardware and assistive-technology limits are reported at their real scope.

## Blockers

- None for this scoped request. Full Studio authoring, coordinator reuse/parity,
  C2 and the deferred resolver/splitter tasks retain their independent gates.

## Completion evidence — 2026-10-21

Root and Lil' Scooty independently accept immutable qualification5DBE355E093266D7BC746DA282AA6B06EE74C55DDCBE10E29147F84C6B9E2AE1:
34 owned/1795 Pascal/13 foreign/16 extra input pins; all actual readers joined.
The selected composite has14 successful compilers, zero Nyx warnings and49
upstream RTL warning occurrences. Nine accepted native executions have clean heaps.

- Presentation161 and configuration67 pass native and actual browser checks.
  Existing interface layouts stay intact; the six maintained contract outputs
  match for81 kinds/four facades. Scope/order/noncanonical source reconciliation,
  malformed drafts, stale transactions, caption fallback and glyph bounds qualify.
- Ordinary Studio chrome passes48 checks at actual native client1280 and52 at390.
  Both products compile. Actual browser1280x900/390x900 captures and UI journeys
  verify typed Apply, paired Undo/Redo,36 discovered button events including four
  image lifecycle callbacks, assets disclosure, glyphs, compact bounds and source
  expansion. Browser consoles are empty. The phone source editor occupies717.5px
  of a900px viewport when expanded.
- Unchanged whole MasterDetail Studio passes165/175 through actual Save/Open.
  Its FOUR newly produced project/Pascal files match across widths at3530B9EE/
  AD890025; each packet.Source equals its9074-character emitted file. Those exact
  new paths strict-admit and independently compile/run36/36 at1280/390. Matching
  whole/compiled browser branches compile only; no historical pair is substituted.
- The final browser sourceE49B163C adds only one blank line before an if. Newly
  compiled previewE8E7DF31 and productC2427AFE are byte-identical to the earlier
  browser-tested artifacts. That emitted identity supports actual-browser reuse;
  unchanged focused/generator/native dependencies are reused explicitly.
- Original atlas59981D9C is preserved at1254x1254 with proportional4x4 cells,
  its exact imagegen prompt and matching embedded bytes in
  [asset provenance](../../studio/assets/glyphs/README.md).

Retained exclusions include the initial corrupt fixture JSON, compiler
portability/declaration errors, numeric-domain and optional-lookup fixture
refusals, nonexistent command entry, and the incomplete browser bundle without
its separate Pascal worker. None is relabeled as passing evidence. All owned
hosts are stopped/ports empty and temporary viewport overrides reset; successful
preview tabs17/18 closed. Initial bare-root404 tab16 was left unmarked for automatic
cleanup after the browser URL policy rejected its close.

Native execution was Windows LCL. Browser compact checks used a controlled
viewport; no physical phone, macOS, hardware DPI, assistive-technology or measured
performance claim follows. Heap allocation totals are cumulative, not retained
memory. Original no-closure25 and all earlier criterion/credit boundaries remain.
See [the work record](../../WORK.md#studio-facelift--2026-10-21).