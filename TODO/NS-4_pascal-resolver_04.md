# NS-4_pascal-resolver_04 - Finish canonical type and helper resolution

[North stars](../MILESTONES.md) · [Task catalog](README.md) · [Task flow](../TASKFLOW.MD)

**Description:**

Deferred on 2026-10-21. Close remaining canonical type/member and actual helper
activation gaps for [Studio authoring](NS-4_studio-authoring_01.md). North-star
owner: NS-4; credit stays with that outcome. Existing aliases, properties,
records, inherited members and primitive-helper prefix proofs are partial
evidence, not complete type equivalence or helper activation.

Starting owners: [types](../src/nyx.pascal.types.pas),
[binding](../src/nyx.pascal.binding.pas),
[semantic completion](../src/nyx.pascal.completion.owned.pas) and
[documented contracts](../docs/pascal-projects.md#nyx-owned-completion-provider).

**Acceptance Criteria:**

- Assess remaining unsupported families against actual source witnesses and
  publish a finite supported/refused matrix before implementation.
- Preserve declaration identity through aliases, qualified/imported types,
  records, interfaces, indexed/default properties and inherited member results.
- Implement genuine helper activation, target/parent lookup and captured
  MultipleHelpers policy without broadening the existing narrow lexical proof.
- Distinguish class, instance, static, property and helper receiver authority;
  retain shadowing, missing imports and unfinished-source uncertainty.
- Qualify completion, definition origins and negative ambiguities from the same
  canonical answers, with actual compiler acceptance where needed and unchanged
  type/binding/cancellation limits. Unsupported ABI/layout remains explicit.

**Blockers**

- [Exact compiler facts](NS-4_pascal-resolver_03.md) only for affected type rules
  that require target/compiler authority; ordinary scoped rules need no new probe.
