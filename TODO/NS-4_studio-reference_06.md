# NS-4_studio-reference_06 - Capture authored generic descriptor ownership

[North stars](../MILESTONES.md) · [Task catalog](README.md) · [Task flow](../TASKFLOW.MD)

**Description:**

Deferred on 2026-10-21. Connect existing managed generic documentation metadata
to actual source formals for [portable descriptors](NS-4_studio-reference_01.md).
North-star owner: NS-4; credit stays with the parent. The source adapter's
GenericSpan refusal remains a real gap despite qualified formal metadata.

Starting owners: [source capture](../src/nyx.documentation.source.pas),
[signatures](../src/nyx.documentation.signatures.pas),
[type expressions](../src/nyx.documentation.typeexpressions.pas),
[descriptor values](../src/nyx.documentation.pas), registration/registry and
their consumers. Existing managed formals retain owner/index/name; faithful
constraint/default metadata needs an explicit typed representation and its
copy/framing/closure support, not just source attachment to those existing slots.

**Acceptance Criteria:**

- Capture actual type-own and routine-own formal names, order, constraints and
  origins through existing syntax/binding owners, including nested shadowing.
- Attach each formal-dependent use to its real stable declaration owner;
  declaration ordinals and inferred synthetic parameters are not persisted identities.
- Preserve explicit constraints/defaults and absent information; do not inject
  an implicit TObject constraint or confuse representation with call inference.
- Qualify any required typed constraint/default descriptor additions through
  construction, sealing, content identity, registration and dependency closure;
  retain existing absence behavior and refuse unsupported forms explicitly.
- Keep complete scope/global capture and exact alternate final observations;
  unsupported/missing generic semantics still refuse before package publication.
- Qualify actual generic declarations plus changed-owner, duplicate-formal,
  cancellation and budget cases through native and executed browser consumers.

**Blockers**

- None beyond the existing formal metadata/syntax/binding contracts. Full call
  inference remains [resolver task05](NS-4_pascal-resolver_05.md), not this descriptor task.
