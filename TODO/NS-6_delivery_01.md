# NS-6_delivery_01 — Clean setup, CI and packaging

[North stars](../MILESTONES.md) · [Task catalog](README.md) · [Task flow](../TASKFLOW.MD)

**Description:**

Deliver clean setup, ci and packaging as part of the user's full Nyx and Nyx Studio outcome.
Starting evidence: inherited fluent browser prototype; new units and validation
are tracked in [WORK.md](../WORK.md). North-star owner: NS-6.
Completion credit: pending evidence-backed assessment; no credit is earned by partial code.

**Acceptance Criteria:**

- A clean checkout has documented compiler overrides and repeatable Pascal build/test commands.
- Supported OS/widgetset/compiler matrix and matched browser runtime are verified by CI artifacts.
- Packages/examples are independently runnable; legacy compatibility and migration policy are explicit.
- Owned Pascal builds are free of actionable compiler warnings on the qualified
  native/LCL and browser compilers. Defensive admission, Unicode and ownership
  remain tested; dependency warnings stay visible and separately attributed.
  Intentional compiler advisories require narrowly scoped documented exceptions.

Warning-cleanup evidence (2026-10-04): the installed checked FPC 3.2.0 core,
generated consumers and server report zero warnings; actual FPC 3.3.1/win32 LCL
layout/Agents consumers and current MCP native application compile do too.
pas2js 3.3.1 Studio/shared consumers and the current MCP browser compile report
zero owned warnings, with seven visible installed-RTL `classes.pas` warnings.
Executed native/browser core/designer checks pass 30/1,542; actual layout passes
2,169 LCL, 2,214 desktop and 2,215 exact-390 browser checks. Real project-directory
junction refusal and Studio resize/split retention pass. See the dated packet in
[WORK](../WORK.md#compiler-warning-cleanup--2026-10-04) for commands, source
fingerprints, explicit ownership exceptions and preserved failed inventory logs.

This qualifies the bounded warning repair on the installed pair, not every
maintained harness, the supported-platform CI matrix or packaging. Original
criteria/prerequisites remain; the task stays open. One logical NS-6 cleanup batch
has closed no complete delivery criterion (count 1). End this inventory/repair
batch and return to the existing concurrent-project acceptance gates; their
recorded reassessment and no-closure count 2 are unchanged.

**Blockers**

- [NS-2_parity-accessibility_01](NS-2_parity-accessibility_01.md) must have accepted evidence (update link when moved to DONE).
- [NS-5_service-reload_01](NS-5_service-reload_01.md) must have accepted evidence (update link when moved to DONE).
