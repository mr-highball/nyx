# NS-4_pascal-resolver_10 - Finish ordinary definition navigation

[North stars](../MILESTONES.md) · [Task catalog](README.md) · [Task flow](../TASKFLOW.MD)

**Description:**

Deferred on 2026-10-21. Complete physical-source navigation and return journeys
for the owned resolver under [Studio authoring](NS-4_studio-authoring_01.md).
North-star owner: NS-4; existing F12, include origins and navigation trail are
retained evidence, not broad default-editor/input qualification.

Starting owners: [definition](../src/nyx.pascal.definition.pas),
[navigation](../src/nyx.pascal.navigation.pas),
[Studio outline](../studio/nyx.studio.sourceoutline.pas) and
[source files](../studio/nyx.studio.sourcefiles.pas).

**Acceptance Criteria:**

- Resolve actual declarations from caret and completion rows across project
  units, helper files, repeated includes and captured readonly SDK inputs.
- Preserve declaration-versus-implementation and overload identity; ambiguous,
  inactive, unsupported or unavailable targets remain explicit refusals.
- Open the correct physical file/occurrence/span without guessing affinity at a
  projection separator; readonly sources cannot acquire an editing authority.
- Back/forward navigation restores file, caret/selection and scroll while keeping
  independent drafts, accepted code and Undo/Redo intact across project switches.
- Execute native/browser wide/compact keyboard and pointer journeys, stale-result
  refusal and retirement with the owned provider rather than a fixture response.

**Blockers**

- [Canonical type answers](NS-4_pascal-resolver_04.md),
  [incremental identities](NS-4_pascal-resolver_07.md),
  [cancellation](NS-4_pascal-resolver_08.md) and
  [target context](NS-4_pascal-resolver_11.md).
