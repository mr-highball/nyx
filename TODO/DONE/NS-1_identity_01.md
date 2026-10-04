# NS-1_identity_01 — Portable design and runtime identity

[North stars](../../MILESTONES.md) · [Task catalog](../README.md) · [Task flow](../../TASKFLOW.MD)

**Description:**

Resolve identity boundaries before deeper shared-state and composition work.
Original design IDs are bounded by native UTF-8 bytes versus browser UTF-16 units;
qualified reusable runtime IDs can exceed that original limit or collide with
authored separators. Preserve original IDs and serialized designs while defining
one portable admission policy and unambiguous independently realized identities.
North-star owner: NS-1. Starting evidence: the structural model is accepted and
instance-part tests exercise design/runtime lookup, but boundary cases remain.
Completion credit: pending evidence-backed assessment; no partial credit.

**Acceptance Criteria:**

- One documented Unicode-aware length/admission policy yields the same decisions and useful diagnostics in native and browser fixtures.
- Nested reusable instances and payloads have deterministic, collision-free runtime identities without applying the authored-ID limit to a qualified path; original design IDs remain unchanged.
- Persistence, generated compilation, selection/event lookup and failed-admission recovery exercise long/Unicode/separator cases on both targets.

**Blockers**

- [NS-1_model_01](NS-1_model_01.md) has accepted evidence.
- [NS-1_catalog-theme_01](NS-1_catalog-theme_01.md) has accepted evidence.

## Acceptance evidence — 2026-10-03

Accepted all three original criteria without changing their scope. The
[public identity contract](../../docs/identity.md) specifies exact 1–128 scalar
admission, malformed/control/whitespace failures, source/editable/runtime roles,
injective slash/tilde escaping and independent qualified-key budgets.

[Shared identity fixtures](../../tests/nyx.test.identity.pas) execute 36 checks
on FPC and pas2js, within 153 shared checks. Persistence, isolated dependency
cloning, generated Pascal reconstruction/realization and invalid Studio imports
cover maximum-length supplementary names and nested independent overrides.
[Browser](../../tests/nyx_browser_journey.lpr) and
[native](../../tests/nyx_lcl_tests.lpr) journeys exercise runtime-first and explicit
lookup, inherited/payload selection, emitted source/owner IDs and accepted-view
recovery. Browser journeys pass 41 checks; native catalog projections remain 74.

[HTTP fixtures](../../tests/nyx_http_tests.lpr) pass 31 checks, including ten
page/definition/instance/derived/application scopes, Unicode percent-encoded
selection, preserved source IDs, overflow rejection before compilation and
same-origin LAN admission. A scoped view includes only reachable definitions;
the service no longer prefixes IDs and cannot overflow an admitted authored ID.
Studio chrome uses bounded ordinal IDs while retaining full authored IDs in
selection/view/instance command data. Current portable heap tracing reports
424136 allocations/frees and zero unfreed
blocks, including scoped view and rejected runtime/design admission paths.

This accepts the identity foundation. Large-document indexing, broader codec
budgets, state bindings, source synchronization and complete renderer/Studio
production outcomes remain with their open task owners. See [WORK](../../WORK.md).
