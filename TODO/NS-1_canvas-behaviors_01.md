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
