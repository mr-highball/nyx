# NS-4_studio-reference_10 - Define bounded public-declaration maintenance capture

[North stars](../MILESTONES.md) · [Task catalog](README.md) · [Task flow](../TASKFLOW.MD)

**Description:**

Deferred on 2026-10-21. Reassess a distinct source-maintenance contract for exact
public interfaces/globals under [descriptors](NS-4_studio-reference_01.md) and
[discovery](NS-4_studio-reference_02.md), so generating API help need not require
unrelated private implementation-body semantics. North-star owner: NS-4; no new
credit or automatic replacement of existing whole-source acceptance.

Starting owners: [source adapter](../src/nyx.documentation.source.pas),
[translated syntax](../src/nyx.pascal.syntax.translated.pas), modules/context,
signature sessions and source consumer. Current capture requires complete whole
units. This architectural prerequisite was queued for reassessment, not implemented.

**Acceptance Criteria:**

- Specify the exact public interface/global selection boundary and the proof
  needed for its conditional activity, source/include origins, types and imports.
- Retain all declarations and dependencies within that boundary; no exported
  prefix, hand-maintained signature list, skipped directive or guessed fact may
  stand in for complete public API capture.
- Separate public-surface proof from full-unit semantic resolution in typed
  ownership and diagnostics. An implementation-dependent public fact still
  requires its actual authority or refuses explicitly.
- Demonstrate that irrelevant private body changes need not recapture unchanged
  public metadata, while public/include/context changes invalidate it correctly
  under existing byte/work/cancellation limits.
- Keep the original full resolver/source consumer gate and its failed results
  unchanged and independently visible. Qualify any new maintenance consumer
  separately before proposing adoption under the parent reference criteria.
- Compare resulting faithful descriptors with a fully resolved unit where
  available on native/browser targets; runtime import independence remains the
  separate [static-emission task08](NS-4_studio-reference_08.md).

**Blockers**

- None for the architectural/proof reassessment beyond existing typed owners.
- Adoption requires evidence for the full parent reference criteria; writing
  this task does not relax the unchanged complete-source/package gate.
