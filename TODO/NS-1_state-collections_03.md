# NS-1_state-collections_03 — Fence query delivery after outward validation

[Tasks](README.md) · [Task flow](../TASKFLOW.MD)

**Description:**

The collection query receiver checks cancellation/closure before calling Assign,
but collection validators can revoke that receiver or cancel its generation
before installation. Add final noncalling admission using existing prepared
collection publication and captured cancellation readiness. Do not introduce a
second store, scheduler or result engine. This is a prerequisite of the live
producer journey in [control usefulness04](NS-3_control-usefulness_04.md).

North star: NS-1. Completion credit: pending assessment, counted once within
existing structured publication/lifetime outcomes.

**Acceptance Criteria:**

- Closure or generation cancellation during validation prevents installation and
  reports discarded delivery without altering source values/revision/observers.
  Stale revision, malformed input and validation veto retain atomic refusal.
- Capture final readiness before validators. After they run, inspect only owned
  flags/readiness before the existing noncalling install; no late application
  getter may recreate authority. Unsupported alternative capabilities refuse
  explicitly. Notification failure after commit retains committed data.
- Preserve reference-counted call/receiver lifetimes through reentrant closure
  and release. Execute native/browser regressions and mounted producer journeys;
  retain negative evidence, clean native heaps and explicit target limits.
- Document capability requirements, cancellation timing and ordinary source
  notification semantics, and retain the regression in maintained checks.

**Blockers**

- [Observable collections](DONE/NS-1_state-collections_01.md) supplies prepared
  collection admission and exact revision ownership.

The existing authorized HTTP preview/browser connection is now available. The
current return is actual browser execution and alternative-capability refusal
qualification, using the existing native evidence and maintained consumers.

Native progress on 2026-10-25: the retained negative regression reproduced an
installation after validator closure. Seven focused checks now pass with clean
heap evidence, including cancellation during validation, original veto/stale
refusal, exact Unicode/NUL text and post-commit notification failure. The same
fixture builds for pas2js and is registered in maintained workbench checks.
Native mounted handwritten/generated producer journeys and existing query/
publication/scheduler checks also pass. Actual browser execution and alternative-
implementation capability-refusal qualification remain open; do not move to DONE.
