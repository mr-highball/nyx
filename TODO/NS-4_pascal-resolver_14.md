# NS-4_pascal-resolver_14 - Deliver reviewed atomic symbol rename

[North stars](../MILESTONES.md) · [Task catalog](README.md) · [Task flow](../TASKFLOW.MD)

**Description:**

Deferred on 2026-10-21. Use exact reference answers to support a reviewed rename
through existing paired source editing under
[Studio authoring](NS-4_studio-authoring_01.md). North-star owner: NS-4; no added
credit. Rename remains a separate unsupported outcome until these checks pass.

Starting owners: [source edits](../studio/nyx.studio.sourceedits.pas),
[file changes](../src/nyx.pascal.filechanges.pas), existing source compilation/
publication/history and resolver reference identities.

**Acceptance Criteria:**

- Validate the new Pascal name and analyze collisions, binding changes,
  qualified names, overloads, interface/implementation pairs and affected imports.
- Show the complete proposed file/range changes for user review, preserving
  comments, unrelated strings and source outside the exact symbol references.
- Refuse incomplete/ambiguous analysis, readonly SDK edits and unsupported
  macro-generated identifiers; never apply a misleading partial rename.
- Compile/validate through existing source admission and publish all changes as
  one revision-guarded ordinary history step; failure/cancellation/stale input
  leaves every file, draft, selection and prior history unchanged.
- Execute multi-file native/browser review, acceptance, Undo/Redo and project
  switch journeys with exact include/Unicode ranges and owned lifetimes.

**Blockers**

- [Exact project references](NS-4_pascal-resolver_13.md).
- Existing compiler-backed grouped source admission remains authoritative;
  unavailable compilation cannot be replaced with a parser-only success claim.
