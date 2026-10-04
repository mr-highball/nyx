# NS-5_compile-service_01 — Pascal HTTP builds

[North stars](../MILESTONES.md) · [Task catalog](README.md) · [Task flow](../TASKFLOW.MD)

**Description:**

Deliver pascal http builds as part of the user's full Nyx and Nyx Studio outcome.
Starting evidence: inherited fluent browser prototype; new units and validation
are tracked in [WORK.md](../WORK.md). North-star owner: NS-5.
Completion credit: pending evidence-backed assessment; no credit is earned by partial code.

**Acceptance Criteria:**

- A Pascal-only HTTP service generates and builds requested views, reusable components and full browser/LCL applications.
- Responses contain success/failure, target/scope, compiler diagnostics and admitted artifacts.
- Native/browser builds and error paths are tested; compilation cannot execute client-supplied shell text.
- Compiler profiles can be configured while Studio is running. Empty profiles
  are admitted, with actionable diagnostics only when that output is built.

**Blockers**

- [NS-1_codegen_01](NS-1_codegen_01.md) must have accepted evidence (update link when moved to DONE).
