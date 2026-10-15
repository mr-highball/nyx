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
