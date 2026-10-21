# NS-4_studio-reference_09 - Adopt generated metadata in linked registration

[North stars](../MILESTONES.md) · [Task catalog](README.md) · [Task flow](../TASKFLOW.MD)

**Description:**

Deferred on 2026-10-21. Finish the automatic runtime adoption portion of
[reference discovery](NS-4_studio-reference_02.md), using emitted metadata rather
than an explicit maintenance capture. North-star owner: NS-4; no additional
credit. Existing supplied-package registration/lifetime mechanics remain evidence,
not automatic complete native/browser help.

Starting owners: [documentation registration](../src/nyx.documentation.registration.pas),
[registry](../src/nyx.documentation.registry.pas),
[catalog](../src/nyx.catalog.pas), generated companion and fixture integration.

**Acceptance Criteria:**

- Let linked built-in and extension registrations discover their emitted package
  through the real schema/callback/creator lifecycle with one source of truth.
- Verify repeated initialization, ordering, duplicate/conflicting identities,
  unloaded/unlinked boundaries and exact retained package lifetimes.
- Change an actual decorated extension declaration, regenerate and observe the
  corresponding index change without editing a help-only inventory.
- Preserve atomic index Current/version on invalid or incomplete registrations;
  keep separate existing registries' authority and avoid an unimplemented dynamic
  unload or cross-registry transaction claim.
- Execute equal logical discovery on native/pas2js with explicit target differences,
  no runtime source/compiler/backend access and no inspected-code execution.
- Hand off the real index to existing reference03 browsing and reference04
  coverage/static QA tasks; do not duplicate their user-interface or delivery gates.

**Blockers**

- [Static package emission](NS-4_studio-reference_08.md).
