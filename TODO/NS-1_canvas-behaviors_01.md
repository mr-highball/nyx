# NS-1_canvas-behaviors_01 — Typed Pascal canvas behavior programs

[North stars](../MILESTONES.md) · [Task catalog](README.md) · [Task flow](../TASKFLOW.MD)

**Description:**

The user's canvas request includes a lightweight Pascal interpreter for complex
creator-defined interaction. NS-1 owns an optional portable, typed Pascal subset
with a documented grammar and capabilities. Existing source parsing and computed
expressions supply reusable infrastructure; neither already proves a runtime
interpreter. Scripts remain explicit authored programs, not raw property strings
implicitly interpreted as behavior. Native compiled callbacks remain supported.
No JavaScript evaluator/framework or external language runtime is introduced.
This new scope earns no credit from its plan or interpreter-only fixtures.

**Acceptance Criteria:**

- Specify and implement Delphi-dialect variables, typed expressions, conditionals,
  bounded iteration, named procedures/functions and typed event arguments needed
  by meaningful canvas behaviors. Public managed compiled-program/diagnostic
  interfaces expose exact UTF-8 source spans, version and capability requirements.
- Bind an explicit typed host API for scene queries, camera, resources, state and
  proposed edits. A program cannot acquire arbitrary files, process execution or
  host objects from strings. Caller-selected capabilities and execution budgets
  are observable/configurable, with bounded cancellation and clear failures.
- Parse/type-check before execution; deterministic owned program/context values
  run on FPC and pas2js. Step/recursion/allocation/time quotas and infinite-loop,
  malformed-input, overflow, missing-reference and retired-owner cases preserve
  accepted state. Programs never retain renderer/document ownership cycles.
- Collect program edits as one revision-guarded admitted transaction. Scheduler
  publication uses Nyx's existing UI abstraction; cancellation and stale output
  do not partially mutate the scene. Multiple event registrations preserve policy.
- Public Nyx editors and bounded MCP program inspection/replace/diagnostics feed
  ordinary paired source/history. Script source never silently becomes native
  application code. Persist/export exact programs and capabilities independently.
- English examples execute useful custom layout, connector validation and
  interaction behavior in actual browser/LCL workspace consumers. Measure
  representative costs and qualify observing rollout independently.

**Blockers**

- [Owned document model](DONE/NS-1_model_01.md).
- [Managed component interfaces](DONE/NS-1_component-interfaces_01.md).

The canvas is a related host consumer, not a cyclic prerequisite for the
independently qualified language/runtime contract. Its actual integration remains
part of the acceptance above.

Current-source runtime Tool Canvas authoring (2026-10-16) reuses this task's
accepted source editor/interpreter contracts in a live sandbox. Prepared program
edits replace affected graph owners with shared meaning revisions; invalid/stale
source/signatures/removal preserve accepted behavior and unfinished text. Applied
programs share paired source/history saving with graphs/masks. WORK.md separates
native control execution, compiler-backed continuation and saved applications from
browser compilation and open full parity/quality/budgets/observing/delivery. No
grammar, host capability, document/MCP schema or rollout is added. Behavior-program
no-closure remains zero; Graph/Logic is thirty-two. No full task/credit/DONE closes.
Next advance useful runtime Logic node faces/placement under existing canvas/graph
owners; mixed state/scene transactions and the full interpreter acceptance remain.

Current-source boundary (2026-10-15): managed immutable programs now compile and
execute an explicit version-1 Delphi-dialect subset, including typed inputs/
outputs, named/nested/forward routines, strict scalar expressions and bounded
iteration. Exact UTF-8 diagnostics, typed opaque handles, explicit capability
grants/frozen queries and a detached proposal journal run on both targets.
Metadata counts admit before allocation; invocation state/counters remain owned.
The scalar-state courier captures UI state and publishes all outputs through one
revision/generation-guarded State.Apply; proposed host edits explicitly refuse.

The [public contract](../docs/programs.md) documents grammar, limits and authority.
[WORK.md](../WORK.md#current-return-path-typed-pascal-behavior-programs--2026-10-15)
owns 69 native/executed-browser engine checks and 28/26 actual native/browser
workbench checks, including independent apps, stale/failed/cancelled/retired
publication, reentrant conversion retirement and actual entered native-worker
cancellation. These are runtime
consumers, not Studio program editing, semantic import or observing release
evidence. Behavior-program no-closure is one; original criteria and credit remain.
Next implement scene/camera/resource whole-result admission through typed owning
adapters. Ordinary document/source/history/Studio/MCP program ownership and useful
canvas event/script consumers remain required; do not add interpreter variants
or close this task from the scalar workbench alone.

Current-source canvas admission (2026-10-15) now uses optional prepared scene/
view layout interfaces, preserving the original interfaces. A managed immutable
capture supplies typed item/camera/materialized-resource queries and stable opaque
handles. Complete journals validate and merge chronological placement changes;
the view admits physical allocations before scene/camera publication. Exact scene
and presentation revisions, foreign-capture refusal and final cancellation/
retirement/reentry guards preserve accepted runtime state. Scalar outputs refuse
instead of pretending state and scene have a shared commit. Hosted resources use
the existing loader/cache before explicit materialization; queries perform no I/O.

WORK.md owns 558/554 native/browser scene regression, 31/30 program checks and
57/69 actual native/browser workspace journeys, including a real embedded Nyx
button compiling and executing resource-driven named layout behavior. Counts
include prior baseline cases and both widths overlap; they are not additive task
credit. Behavior-program no-closure advances one → two; original criteria remain
open. Reassessment now selects saved program document/source/Studio/MCP ownership
and event/script routing through the existing scheduler, retaining exact paired
history and actual native/browser consumers. Stop standalone interpreter/host/
renderer variants. Full event payloads, mixed owner transactions, useful diagram
behavior, production budgets, quality/parity and observing delivery remain open.

Saved program authoring (2026-10-15) now provides document-owned immutable
definitions, version-15 persistence, typed source/literal admission and ordinary
paired Define/Remove commands. A public Nyx card supplies local checking, exact
draft retention, typed parameters, host presets and confirmed removal in both
actual Studio adapters. Bounded program scopes extend nyx_logic, with proposed
diagnostics and grouped semantic edits using existing revision/retry/history.
The original fifth criterion is now accepted in current source. WORK.md's
2026-10-15 program-workshop packet owns 25 native/24 executed-browser handwritten
continuation checks, six complete companion comparisons on each target and 176
authenticated HTTP/ordinary browser/LCL checks. The latter includes eleven real
compiler jobs, native form input, independent observers, bounded diagnostics,
exact retries and paired Undo/Redo. Unchanged helpers remain computed through
chronological replacement/deletion/recreation; a later helper edit still changes
the complete constructed model. Applications/pages/reusable roots compile on
both targets; this is not application execution or observing-release evidence.

Behavior-program no-closure resets three → zero; Graph/Logic remains twenty-two.
The task stays open without full credit or a DONE move. Stop program/editor/query
variants. Return to saved canvas event/script routes through the scheduler and
useful actual consumers, retaining cancellation/registration policy. Mixed
state/scene transactions, richer payloads, production costs, useful diagrams,
full quality/parity and preserving delivery remain required.

Saved event/program integration (2026-10-15) now provides specialized
INyxProgramLogic declarations, independent pure runtime resolution and typed
scalar/JSON/byte ports through existing control routes, including reusable Tool
Canvas items. Existing ordering, concurrency, cancellation and guarded state
publication remain authoritative. Crafted source/literal admission, typed public
Studio choices, draft restoration and exact paired history have current-source
evidence: 40 shared checks, 374 authenticated semantic checks/four application
compiler jobs, two complete companion comparisons per target, 14/16 native and
13/15 executed-browser runtime checks, and 16 actual Studio form checks per
adapter. Browser widths overlap. WORK.md owns exact receipts and retained failures.

This advances the original fourth/sixth criteria without closing either: scene
handles, host journals and mixed state/scene publication still need their owning
transaction adapter; useful diagram behavior and production costs remain open.
The timed native form journey takes about 109 seconds on this qualification host;
its cost is not accepted as editor performance. MCP snapshot preview currently
refuses embedded executable behavior without an application context; its workflow
owner retains that gap. Behavior-program no-closure advances zero → one;
Graph/Logic remains twenty-two. No full task, credit, DONE, parity or rollout
closes. Stop pure-program/editor variants and return to the existing mixed-owner
publication boundary and useful actual canvas/diagram behavior.

Coordinated publication (2026-10-15) now qualifies the original fourth criterion
in current source. Optional scene/view preparations and typed output delivery use
the existing PublishNyxGroup: state, geometry and camera/cache all install before
observation. Complete outputs, exact revisions, weak page/application authority,
viewport admission and cancellation fence the whole result. Existing scene-only
interfaces preserve their output refusal. Actual embedded button registrations
observe the complete first frame; the ordinary query scheduler enters a native
worker or browser UI queue before UI-owned delivery. There is no second router
or scheduler and no implicit authority for saved pure program routes.

WORK.md owns 132 new checks on each actual adapter (189/201 complete native/browser
journeys), desktop/CSS-390 HTTP execution, clean native heaps and the corrected
pre-install exception-phase regression. Browser captures show the bound status.
Native caption/model checks pass. The apparent foreground-capture omission was
subsequently corrected as a fixture cache bug: Canvas.CopyRect invalidates stale
bitmap bytes whereas raw GDI did not. Actual native pixels now show the status;
one actual host hit-test assertion per adapter brings totals to 133 frame checks
and 190/202 complete journeys. Broader layout/style parity remains under the canvas
owner's original second/fourth/sixth criteria. Counts overlap baselines/widths.

Behavior-program no-closure resets one → zero; Graph/Logic remains twenty-two.
The fifth and fourth criteria are accepted in current source, not the full task,
credit, DONE, quality/parity or observing delivery. Stop publication/capture variants.
Next finish a useful canvas/diagram consumer and its saved side-effecting route
through the existing owning delivery; preserve the corrected visual qualification
without claiming full aesthetics/parity. Full event payloads, production budgets,
preview policy and preserving delivery retain their existing owners.
