# NS-2_presentation-viewport_01 — Consistent viewport rules in section hosts

[North stars](../MILESTONES.md) · [Tasks](README.md) · [Task flow](../TASKFLOW.MD)

**Description:**

Keep authored viewport presentations meaningful when a view is divided into
independent sections or embedded hosts. Distinguish application viewport rules
from intentional container rules through the public typed contract. Studio's
Project actions declare one desktop column and two compact columns; the actual
1280-pixel native editor nevertheless displays two narrow columns in its sidebar.
Source review indicates that an independently rendered section evaluates automatic
viewport presentations against its allocated host. Qualify that cause before
changing the contract; do not substitute a Studio-only width exception.

North star: NS-2, consumed by Studio's shell and project lifecycle. No separate
completion credit; the original renderer, section and Studio criteria remain.
Starting evidence: [current work](../WORK.md), the Project navigation composition,
both renderers' viewport application, and the accepted staged-section foundation.
The private studio-project-lifecycle-02 packet retains the desktop switcher image.

**Acceptance Criteria:**

- The same authored viewport rule resolves consistently in standalone, sectioned
  and embedded views at desktop and compact sizes. Explicit container rules keep
  their local meaning. Document the inherited context, opt-in changes and defaults.
- Both adapters preserve resize/reflow, zoom, source/caret, scroll and child
  ownership. Retired hosts cannot publish a viewport into a successor. Preserve
  persistence and readable typed generation for any new configuration.
- Regressions exercise real hosts at different outer and section widths, resize,
  remount and nested sections. Native Project action captions fit their intended
  allocation, and actual browser/native Studio navigation retains its project pair.
  Compilation or isolated rule evaluation alone does not establish acceptance.

**Blockers**

- [Staged view sections](DONE/NS-2_section-publication_01.md).

**Current evidence — 2026-10-28**

Typed runtime viewport sources now span both adapters and Studio section hosts;
local container measurements remain independent. The maintained
`viewport-context` target executes15 source-ownership and22 actual native host
checks and stages browser consumers. Actual browser execution passes15/21.
The native Studio section recovery passes111 and its desktop capture shows the
authored one-column Project actions with fitting captions. Content authoring/replay
passes66 and the existing native live-content consumer passes63; all native
heaps are clean. See [current work](../WORK.md) for failures and the private packet,
and [the public host contract](../docs/view-sections.md#enclosing-presentation-space).

The complete browser Studio recovery now passes101, including the unchanged real
capture/acknowledgment. Extended real-host native28/browser27 exercise half-scale
embedding, actual nested scrolling and enclosing resize with preserved drafts,
focus and supplementary selection. Current-service native project journeys pass
78 desktop/90 compact with clean heaps. Ordinary browser desktop and actual390x844
Project panels have fitting glyph-bearing captions; compact source Expand/Close
works. The matched LAN payload preserves all three original pairs and configuration.

Browser project return retains the exact unfinished Unicode source but loses its
caret and reports a local/shared mismatch. Initial claim compares selection/view
as well as the pair; conflict prevents presentation restoration. Qualify and repair
this under [project recovery](NS-4_project-recovery_01.md), then return here. The
observed52s return and2876ms fixture event-loop lag remain performance failures.
This second batch reassesses the next action: stop viewport variants and repair
the actual editor return. This task stays open without DONE or completion credit.
