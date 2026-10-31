# NS-1_state-collections_03 — Fence query delivery after outward validation

[Tasks](../README.md) · [Task flow](../../TASKFLOW.MD)

**Description:**

The collection query receiver checks cancellation/closure before calling Assign,
but collection validators can revoke that receiver or cancel its generation
before installation. Add final noncalling admission using existing prepared
collection publication and captured cancellation readiness. Do not introduce a
second store, scheduler or result engine. This is a prerequisite of the live
producer journey in [control usefulness04](../NS-3_control-usefulness_04.md).

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

- [Observable collections](NS-1_state-collections_01.md) supplies prepared
  collection admission and exact revision ownership.

**Accepted — 2026-10-31:** All four original criteria are met. The final fence
retains the original prepared publication protocol and captures noncalling
readiness before outward validation. Closure/cancellation refuse installation;
stale, malformed and vetoed data retain atomic refusal. Notification errors retain
the committed result. Public receiver comments and docs/linq.md document required
facets, cancellation timing, Assign semantics and ordinary notifications.

The original seven checks first pass natively with a clean heap, then fail in the
browser: closure returns discarded delivery but the caller's store loses its
snapshot. Generated-code review shows constructor Supports out fields receiving
temporary releases without a separate field lease. The receiver now acquires
facets into locals and explicitly retains them in fields. Reentrant Close/release
no longer retires the caller's source. This is ownership repair, not an alternate
store, result engine or late authority callback.

Eleven maintained checks now execute on both targets, with clean native heaps
and empty browser error logs. Alternative cancellation without readiness refuses
without reading its getter; an ordinary alternative store without prepared
publication refuses at capture; an alternative plan without final readiness
retires its reservation before Validate/Install/Notify. Malformed results also
refuse before accepted source/observer changes. Every original assertion remains.

Complete mounted handwritten/generated live-inspection journeys pass 138 browser
checks per measured 1280x900/390x844 viewport; native handwritten passes 89/88
(including fresh source export), generated 88/88. These include immutable producer
submission, cancellation, supersession, stale source, peer independence and
retired delivery. All native heaps are clean. Evidence: collection-safety-return-01,
before-01/repair-03/final-04/generated-05; WORK owns exact identities, initial
failures and target limits. Full physical/assistive and scale qualification remain
with live-inspection04; no duplicate north-star credit is assigned.
