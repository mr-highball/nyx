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

The 2026-10-05 source-responsiveness qualification also rebuilds the ordinary
Win32 LCL fixture, which retained nine warnings outside that earlier bounded
inventory. Explicit portable text comparisons, direct UTF-8 widget input and
complete inspector-effect handling remove them without warning suppressions.
The source packet owns the actual execution results in WORK.md. This incidental
fixture repair does not accept the full compiler/platform matrix, reopen the
ended inventory batch or reset this task's existing no-closure count of 1.

## Frozen backend/browser candidate — 2026-10-06

Subsequent bar-workflow candidate (2026-10-07): frozen `0ce846f` verifies 234 files,
29 preparation/refusal, 39 installed runtime, 72 protocol recovery, five abrupt-
process and 14 portable ownership checks. Admission/round trip of the protected
checkpoint copy passes 106; authenticated full Studio candidate journeys pass
513 desktop/456 compact with four exact compiler inputs each. Automatic approval
review rejected the already authorized primary replacement before execution.
The LAN still serves its preceding frozen payload; no deployed authority claim,
original criterion closure or delivery count change is inferred. Delivery remains
at 2; retain verified payload/backups and the exact protected baseline. See
[the packet](../WORK.md#current-return-path-menu-bar-workflow-delivery--2026-10-07).

Criterion 3 gains a maintained Pascal release preparer and verifier. Owned
compiler sources are frozen before backend/editor/worker/preview compilation;
matched runtime, production HTML and MIT license complete a strict byte manifest.
Private configuration/projects and prior artifacts are excluded. Existing
destinations and malformed/mixed candidates refuse. The installed pair is
qualified independently; this is staging, not a deployed or complete distribution.
See [the guide](../docs/studio-releases.md) and [current evidence](../WORK.md).

The preserving observing return (2026-10-07) freezes product `6fc231e` into a
229-file release. Preparation (29), installed runtime (39), actual protocol
recovery (72), abrupt-process (5) and portable ownership (14) pass with zero
leaks and owned warnings. Copied actual checkpoint admission passes 106 and
requires byte-identical complete durable history. The existing NS-5 owner
delivers the exact payload at the original LAN executable path; served frontend
bytes and all frozen files verify after four real MCP application/view builds.
Nine exact pairs/full checkpoint and fourteen other services remain unchanged.
This extends criterion 3 evidence without another delivery count, full criterion
closure or supported-matrix claim. See
[the current packet](../WORK.md#current-return-path-menu-editor-observing-delivery--2026-10-07).

The preceding integrated menu-editor return (2026-10-07) freezes product `c24ae36` into a
new 229-file candidate. Exact preparation (29), installed runtime (39), actual
protocol recovery (66), abrupt-process (5) and portable ownership (14) pass,
with zero leaks and owned warnings. Qualification uses the unchanged semantic
content-editor companion; current menu applications compile through `nyx_build`
on both targets. No listener or new LAN delivery is claimed; primary services,
complete pairs and checkpoint remain exact. This extends criterion 3 evidence
without a further delivery count, full criterion closure or supported-matrix claim.
See [the packet](../WORK.md#current-return-path-public-menu-editor--2026-10-07).

No complete original criterion closes. Delivery no-closure advances 1→2 once;
the warning inventory remains ended. Return to the existing service/reload and
workflow owners for session preservation and observing HTTP qualification; the
typed runtime-root and native session/history boundaries now have integrated
protocol/worker/recovery evidence under NS-5, without another delivery count or
criterion credit. The protected older-service migration and observing HTTP gates
remain open. Native Studio packaging
and the supported-platform CI matrix
remain open. No task moves to DONE.

**Blockers**

- [NS-2_parity-accessibility_01](NS-2_parity-accessibility_01.md) must have accepted evidence (update link when moved to DONE).
- [NS-5_service-reload_01](NS-5_service-reload_01.md) must have accepted evidence (update link when moved to DONE).
