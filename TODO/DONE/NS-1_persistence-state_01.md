# NS-1_persistence-state_01 — Design persistence and shared state

[North stars](../../MILESTONES.md) · [Task catalog](../README.md) · [Task flow](../../TASKFLOW.MD)

**Description:**

Deliver design persistence and shared state as part of the user's full Nyx and Nyx Studio outcome.
Starting evidence: inherited fluent browser prototype; new units and validation
are tracked in [WORK.md](../../WORK.md). North-star owner: NS-1.
Completion credit: pending evidence-backed assessment; overall product completion remains unscored.

Accepted 2026-10-03 on `hello-nyx`: all three original criteria pass for the
admitted version-1 design/scalar contract. Native FPC 3.2.0 and executed pas2js
fixtures pass 645 checks; actual browser/LCL bindings pass 48/50; native-emitted
source reconstructs the complete design and passes 23 browser checks. Twenty
intended type-rejection cases pass per compiler. All 57 LAN HTTP checks pass,
including ten view/application scopes and byte-identical client/service source.
Heap tracing reports 26185443 allocations/frees and zero unfreed blocks.
Maintained code and Studio consume the public APIs; detailed artifact identities,
commands and remaining full-product limits are in [WORK](../../WORK.md).

**Acceptance Criteria:**

- Versioned designs round-trip Unicode, extension fields, pages and components on both targets.
- Failed imports and invalid commands preserve accepted state; history/binding semantics are documented.
- Portable state and event behavior has shared fixtures and browser/native interaction evidence.

**Blockers**

- [NS-1_model_01](NS-1_model_01.md) has accepted owned-model evidence.
- [NS-1_identity_01](NS-1_identity_01.md) has accepted evidence for portable Unicode identifier admission and runtime lookup.

## Integrated evidence — 2026-10-03

[Typed state](../../docs/state.md) now owns text/Boolean/signed-32-bit/finite-Double
values, atomic candidate validation, revision guards, immutable changes and token
lifetimes. Defaults survive version-1 persistence, document/view clones, Studio
history, native/pas2js generated execution and ten HTTP build scopes. Strict
Pascal JSON decoding preserves NUL/Unicode and rejects decoded duplicate keys
with shared budgets. Current validation is recorded in [WORK](../../WORK.md).

Immutable typed binding descriptors, application-owned runtime state/navigation
and shared candidate admission now integrate both renderers. Named references,
independent reusable overrides and clear descriptors survive persistence and
compiled source. Existing controls retain identity and unrelated drafts;
rejections preserve accepted state, while committed notification failures have
distinct typed diagnostics. Current shared fixtures total 570; 41 browser /
43 native binding checks and seventeen compiler type-rejection cases per target pass.
See [bindings](../../docs/bindings.md) and [WORK](../../WORK.md) for HTTP evidence.

Studio state/binding authoring now uses public Nyx compositions and a portable
command router. Creation, exact default edits, reference-preserving renames,
removal admission, binding clear/inheritance and undo/redo pass 37 shared checks,
23 desktop and 23 exact-390-pixel browser checks, and 10 actual LCL-control checks.
Focused new-default drafts survive panel changes; rejected commands retain the
accepted design and browser canvas. See [Studio authoring](../../docs/studio-state.md).

Root/node extension persistence now retains immutable typed nested data, exact
Unicode/NUL and admitted decimal spelling. Unknown fields survive save/load,
clones, ordinary Studio property edits/history, generated FPC/pas2js execution
and isolated builds. Standard fields and aggregate byte/depth/member budgets
are protected before publication. Reusable instance/part overlays retain template
and sibling independence. Executed 64 shared data checks and 21 native-generated
browser reconstruction checks; two new core guards reject rounded integer/version
admission. See [extensions](../../docs/extensions.md).

Typed runtime/design triggers and portable actions now bridge a shared handler.
Owned scalar values and exact source/origin/target IDs survive later commands,
navigation and view/application disposal. Payload conversion precedes publication;
design triggers cannot execute runtime actions. Open Unicode names survive wire
data, generated FPC/pas2js execution and ten isolated build scopes. Executed 32
shared event checks and ten additional actual-control checks per adapter.
Typed part references/override modes and recipe-kind references also pass shared
ownership/inheritance checks. See [events](../../docs/events.md).

Declared scalar compound/extension values and named fields now supply exact
binding/event families through [typed fluent contracts](../../docs/contracts.md).
Ranges/choices, origin/semantic-source payload declarations, explicit signals and
separate payload IDs survive independent recipe/reusable ownership, versioned
data, history and compiled source. Seventy-five shared contract checks include
signed-32-bit inspectors, strict draft rejection, imported spelling, read-cache
invalidation and numeric selection presentation. Actual adapters exercise search,
rating, number and full-range integer fields. Read caching compares immutable
namespace snapshots rather than trusting an externally bypassable revision.

All three original criteria are accepted without changing their text: exact
round trips; failed import/command recovery and documented history/bindings; and
shared plus actual both-target state/event behavior. This accepts the version-1
persistence/scalar foundation. Required [observable structured state](NS-1_state-collections_01.md)
is an explicit open follow-up for production data controls. Arbitrary extension
property schemas, full component/parity/Studio depth, measured large-document
performance and editable source retain their open task owners. This acceptance
does not complete the full user outcome or assign unallocated percentage credit.
