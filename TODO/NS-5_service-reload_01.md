# NS-5_service-reload_01 — Reliable compiler workers and reload

[North stars](../MILESTONES.md) · [Task catalog](README.md) · [Task flow](../TASKFLOW.MD)

**Description:**

Deliver reliable compiler workers and reload as part of the user's full Nyx and Nyx Studio outcome.
Starting evidence: inherited fluent browser prototype; new units and validation
are tracked in [WORK.md](../WORK.md). North-star owner: NS-5.
Completion credit: pending evidence-backed assessment; no credit is earned by partial code.

**Acceptance Criteria:**

- Bound admission, worker concurrency, timeouts, isolation, retention and cancellation are explicit and exercised.
- Cache reuse, stale-result refusal, fast view reload and full application execution preserve accepted work.
- Projects/includes/assets, structured diagnostic locations and compiler failures are integrated into Studio.

**Blockers**

- [NS-5_compile-service_01](NS-5_compile-service_01.md) must have accepted evidence (update link when moved to DONE).
- [NS-4_studio-slice_01](NS-4_studio-slice_01.md) must have accepted evidence (update link when moved to DONE).

## Compiled native preview consumer — 2026-10-05

The native candidate admits succeeded/current byte manifests, downloads and
verifies the exact executable, rechecks source/output before activation and owns
its process separately from Studio. Actual compiled reply controls and running
view replacement execute; verified mismatch/cancellation preserve the previous
process and detach callbacks. Stop retains independent compiler work. Current
helper errors navigate exact supplementary-Unicode positions in the actual
native editor. See [the packet](../WORK.md#native-compiler-and-compiled-preview-consumer--2026-10-05).

Evidence uses a suspended private protocol engine and the unchanged existing
HTTP artifact listener. New editor-route deployment, complete cache/retention/
cancellation, inactive-project jobs, native browser-launch lifecycle and measured
reload performance remain open. Per-wait IO/connect limits are not whole-request
deadlines; stalled-worker retirement retains criterion 1. Original criteria and
blockers remain unchanged; this does not accept the full task.
