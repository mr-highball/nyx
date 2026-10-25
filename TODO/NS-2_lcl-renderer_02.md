# NS-2_lcl-renderer_02 — Efficient native geometry publication

[Native renderer](NS-2_lcl-renderer_01.md) ·
[Workbench consumer](NS-3_control-usefulness_03.md) · [Task flow](../TASKFLOW.MD)

**Description:**

Reduce the cost of publishing a complete layout to native child windows while
preserving the public Nyx contract and ordinary LCL controls. This prerequisite
serves the renderer's resize/interaction criteria and the workbench's real native
authoring experience. It earns no separate completion credit.

The existing 512-control regression retains exact faces and final allocations,
but 24 resize pairs take about 50–56 seconds on the recorded Win32 host with checked
O2 builds. Removing heap tracing leaves 49.6 seconds. Separate form measurements
and 12 private stack samples place most work in LCL's native-bound publication;
nine samples contain that path. The final four-pair observer records 520 native
parent-panel publications, with no logical button bounds changes. Their inclusive
timings sum to 6.89 seconds within a 10.84-second instrumented resize phase;
overlapping phase clocks must not be added. These counts identify parent
publication as the next boundary; they do not establish redundant native writes.
WORK owns source identities, diagnostic limits and the discarded painting trials.

**Implementation sequence:**

1. Use the existing optional phase/surface/form observers to establish the exact
   changed parent windows, native publications and retained leaf geometry. Keep
   the same visible workload and distinguish checked heap-traced diagnostics
   from application-oriented timing. Do not infer redundant native calls merely
   from unchanged logical leaf rectangles.
2. Implement bounded coalescing or grouped publication at an appropriate owned
   LCL adapter seam. Reuse LCL's coordinate, constraint, client-area, scrollbar
   and widgetset behavior; preserve current queries during allocation. Preparation
   must not leave LCL's anticipated client bounds inconsistent with live windows.
   Dependency sources remain unmodified. Keep a correct ordinary path for
   unsupported hosts, custom contracts and platform APIs that are unavailable.
3. Integrate with ordinary retained refresh and replacement, then qualify actual
   native workbench/Studio controls and the same saved application's own compiled
   code. The profiling fixture alone does not accept editor responsiveness.

**Acceptance Criteria:**

- A visible, mounted native view publishes its complete intended geometry with
  materially reduced measured update cost on the unchanged 512-control workload.
  State exact widgetset, compiler flags, update counts, timings and memory evidence;
  retain the prior baseline and any failed approaches. Do not weaken runtime
  bounds or assertions to obtain a pass, or substitute a hidden/offscreen workload.
- Public typed layout, constraints, viewport/container rules, scrolling, clipping,
  split/tab/reusable hosts and actual LCL control semantics remain coherent.
  Intermediate client-area queries and native message/callback ordering have an
  explicit contract. Native controls keep keyboard, accessibility, focus, selection
  and partially edited drafts; no browser or caller-specific native API is required.
- Every pending native write belongs to the exact original mount/control/window
  generation. Replacement, unmount, disposal, window recreation, reentrant callbacks
  and cancelled/failed preparation cannot publish into a successor. Preserve native
  parent ownership and existing physical-frame retirement. Describe partial native
  failure honestly rather than promising rollback of irreversible platform effects.
- Checked real-control regressions cover shrinking/growing/empty views, nested
  scrolling, retained rearrangement, callback retirement, refusal and recovery at
  wide/compact sizes, with complete clean native heaps. Unsupported widgetsets
  retain the correct existing behavior; their portability is qualified separately.
- Repeat the relevant ordinary Studio authoring and saved/generated application
  journeys against the final implementation. Inspect representative captures and
  measure visible interaction; compilation, counters, one stack sample or a faster
  isolated loop cannot establish full parity or whole-editor acceptance.

**Prerequisites and scope:**

- Build on the existing native physical-frame, projection, viewport and control
  ownership contracts. This task does not require its broad renderer parent to
  be accepted before implementation can start.
- [Workbench03](NS-3_control-usefulness_03.md) retains all seven original criteria;
  this native prerequisite does not replace browser, source/history, usability,
  instrumented lifetime or delivery acceptance.
- Necessary Nyx library, adapter and shared-contract changes are authorized by
  AGENTS.md. This sequence coordinates work and validation; it adds no approval gate.

**Progress — 2026-10-25:**

- The owned raw layout panel now groups bounded compatible Win32 siblings at
  LCL's synchronous realization seam. Exact mount/window receipts are rechecked
  when native commands arrive; LCL still owns anticipated client areas and final
  realization. Unknown cohorts use ordinary publication.
- A new reentrant-edit test exposed an older queued rectangle overwriting a
  callback's newer allocation. Delivery now reads the current intended rectangle
  and rejects retired/recreated authority. Failure evidence remains retained.
- The matched checked/O2/profile/heap-traced 512-control workload passes 28,826
  checks before and after, including actual positions, outer sizes and client
  sizes after every displayed width. The 24 resize pairs measure 78,578 ms before
  and 32,797 ms after. Both heaps are complete and clean; this is an isolated
  workload result, not ordinary Studio latency or whole-task acceptance.
- The corresponding ordinary checked/O2/heap-traced runs, without optional
  profilers, pass the same 28,826 checks and measure 54,078/30,938 ms. Lead with
  this approximately 43% reduction for the ordinary-build workload; profiling
  changes the apparent gain. Every comparison heap is complete and clean.
- Actual callback/incarnation/fallback checks, ordinary retained/scrolling/Studio
  regressions and saved-application qualification are recorded in WORK.md as
  they finish. Full original interaction/platform/Studio criteria remain open.
