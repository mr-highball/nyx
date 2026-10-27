# NS-4_agent-workflows_02 — Configure DataToolbar through semantic operations

[Milestones](../MILESTONES.md) · [Tasks](README.md) · [Current work](../WORK.md)

**Description:**

An agent should configure a useful DataToolbar through the same typed policy
candidate as Studio's Inspector. Current authenticated transaction discovery
supports scalar/state/collection edits and several compound contracts, but exposes
no DataToolbar Configure/Disable/Inherit operation. Ordinary scalar properties
do not describe its scoped target, search field, sort choices and query context.
Editing Pascal or injecting an opaque extension is not the focused semantic
authoring experience requested for Studio.

Reuse [DataToolbar authoring01](NS-3_data-toolbar_01.md)'s copied options and
candidate admission, including required Clear/selector parts and provenance.
Extend the existing transaction/inspection surfaces rather than introducing
another toolbar engine or a separate tool for every control. Ownership and credit
remain with original NS-4 agent-workflows01 and Studio authoring criterion2.
The restored service's Input playground proves semantic connection, grouping and
backend compilation; it does not configure a DataToolbar or accept this task.

**Acceptance Criteria:**

- Bounded inspection reports configured/disabled/inherited behavior, exact scoped
  target and typed search/sort/query-context choices without opaque wire blobs,
  complete document/source dumps or executable owner tokens.
- Existing revision-aware transactions capture Configure/Disable/Inherit through
  a closed typed descriptor and the same detached candidate owner as ordinary
  Studio. Intent, required role/domain changes and crafted Pascal publish in one
  undoable command. Reject unsupported fields and wrong reference/value families.
- Preserve customized parts, callbacks, inherited definitions, siblings, selector
  provenance and unrelated drafts. Stale revision, invalid target/field/domain,
  unfinished source and incompatible roles refuse without partial edits.
- Exercise actual authenticated semantic composition in an isolated project,
  bounded readback, exact paired Undo/Redo and primary-project preservation. Export
  that accepted project and compile its own application on browser and LCL through
  the existing backend. Distinguish executed behavior, observed UI and compilation.
- Return the same project to ordinary Studio's Inspector/preview and retain all
  original authoring/runtime, browser, physical-input, visual, assistive and parity
  criteria. Do not claim full acceptance from source/model tests alone.

**Blockers**

- [Primary semantic workflows01](NS-4_agent-workflows_01.md) owns authentication,
  revision/permission/context guards, grouped publication and observing delivery.
- [DataToolbar authoring01](NS-3_data-toolbar_01.md) and
  [runtime02](NS-3_data-toolbar_02.md) own options, required roles, copied candidate
  and actual query/input semantics; their original acceptance remains required.
- [Coordinator01](NS-3_coordinators_01.md) and
  [parity01](NS-2_parity-accessibility_01.md) retain complete integration/quality
  requirements. Qualified native checkpoints permit necessary independent
  implementation; these dependencies add no source-permission gate or waiver.
