# NS-1_model_01 — Portable owned document tree

[North stars](../../MILESTONES.md) · [Task catalog](../README.md) · [Task flow](../../TASKFLOW.MD)

**Description:**

Deliver portable owned document tree as part of the user's full Nyx and Nyx Studio outcome.
Starting evidence: inherited fluent browser prototype; new units and validation
are tracked in [WORK.md](../../WORK.md). North-star owner: NS-1.
Completion credit: pending evidence-backed assessment; no credit is earned by partial code.

Accepted 2026-10-02: the checked FPC 3.2.0 fixtures and pas2js 3.3.1
fixtures executed successfully in headless Edge over the Pascal HTTP service.
The suite reports 25 core checks plus 19 composition/designer checks. Heap tracing
reports zero unfreed blocks. Source:
[model](../../src/nyx.model.pas), [core fixtures](../../tests/nyx.test.core.pas),
[composition fixtures](../../tests/nyx.test.studio.pas).
This accepts the bounded structural contract, not production renderer breadth
or the entire NS-1 outcome.

**Acceptance Criteria:**

- Node/document ownership, cycle refusal, stable identity and detached clone semantics are exercised in checked native fixtures.
- Pages and reusable definitions share a renderer-independent Delphi-dialect API.
- The same fixtures execute in the browser; malformed structure and duplicate identity are rejected.

**Blockers**

- None.
