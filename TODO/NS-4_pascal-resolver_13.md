# NS-4_pascal-resolver_13 - Find exact symbol references across a project

[North stars](../MILESTONES.md) · [Task catalog](README.md) · [Task flow](../TASKFLOW.MD)

**Description:**

Deferred on 2026-10-21. Add semantic reference discovery under
[Studio authoring](NS-4_studio-authoring_01.md), separate from definition lookup.
North-star owner: NS-4; credit stays with the original editing outcome. Existing
definition/navigation does not establish reference finding or safe rename; both
remain explicitly open in [the navigation guide](../docs/pascal-projects.md#definition-lookup-from-the-source-caret).

Starting owners: binding/modules, [definition identities](../src/nyx.pascal.definition.pas),
[owned provider](../src/nyx.pascal.completion.owned.pas), physical source origins
and existing Studio query/navigation seams.

**Acceptance Criteria:**

- Identify references by the exact canonical declaration/overload/formal owner,
  distinguishing unrelated same-spelling names, shadowing and qualified members.
- Report independently paged physical unit/include locations, occurrence and
  read/write/declaration roles where actually established; no guessed roles.
- State the analyzed project boundary and completeness. Missing imports,
  inactive/uncertain conditions, macros and unsupported bindings cannot silently
  produce a complete empty result.
- Preserve exact frozen buffer/context identities, cancellation and budgets;
  stale pages cannot navigate or authorize edits to changed source.
- Present usable native/browser result navigation and validate real multi-unit,
  repeated-include, generic/overload and Unicode witnesses without source mutation.

**Blockers**

- Affected [canonical identity rules](NS-4_pascal-resolver_04.md),
  [dependency coherence](NS-4_pascal-resolver_07.md) and
  [cancellation](NS-4_pascal-resolver_08.md).
