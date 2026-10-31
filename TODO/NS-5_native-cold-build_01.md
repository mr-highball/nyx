# NS-5_native-cold-build_01 — Finish practical native builds through Studio

[North star NS-5](../MILESTONES.md#assessment) · [Task catalog](README.md) ·
[Compile service](NS-5_compile-service_01.md) · [Task flow](../TASKFLOW.MD)

**Description:**

The semantically authored Service actions application builds for the browser and
its exact generated unit compiles and executes natively. Two Studio native jobs
on 2026-10-30, including a solo rerun, exhaust the existing 60-second compiler
budget while rebuilding library units into fresh per-job directories. No native
artifact is admitted. This prevents a normal user from building a growing Nyx
application through the editor despite valid Pascal and a configured toolchain.

Qualify a practical cold native build path under explicit host policy. Assess
compiler/library reuse and trusted budget configuration without weakening source,
compiler/profile identity, cancellation or artifact admission. Requests must not
gain arbitrary compiler arguments, paths, environment values or unbounded work.
Defaults and any explicit host override must remain clear and bounded. This is
part of compile service01, with no independent milestone credit.

**Acceptance Criteria:**

- The retained Service actions project completes a native application build
  through Studio's ordinary semantic build tool from a genuine cold state.
  Execute the admitted application at desktop and compact sizes; preserve the
  exact source/design/profile identities, useful action and stale-context refusal.
- Preserve meaningful time/log limits, process-tree retirement, source leases,
  cancellation and stale-publication refusal. An intentionally insufficient
  budget still refuses without an artifact or late descendant publication.
- Any reusable compiler output is isolated by the actual compiler, library,
  target and relevant settings. A source or configuration change cannot silently
  reuse incompatible units. Do not qualify success using arbitrary old caches.
- Distinguish compiler work, runtime interaction, warm/cold observations and
  source-only checks. Keep existing projects, history, output choices, fixed
  listeners and external toolchains intact. Use the configured scratch volume.

**Blockers**

- None external. Raw failures and independent native consumer evidence are
  recorded in WORK.md's Table actions return. Return to the existing
  [typed Table action prerequisite](NS-3_collection-cell-context_01.md) afterward.
