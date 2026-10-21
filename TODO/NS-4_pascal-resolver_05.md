# NS-4_pascal-resolver_05 - Complete generic call and overload answers

[North stars](../MILESTONES.md) · [Task catalog](README.md) · [Task flow](../TASKFLOW.MD)

**Description:**

Deferred on 2026-10-21. Finish remaining routine-generic inference, constraints
and overload selection for [Studio authoring](NS-4_studio-authoring_01.md).
North-star owner: NS-4; no new credit allocation. Existing explicit/implicit
generic applications, formal-dependent values and operator queries are retained
starting evidence, not a complete compiler-equivalent call resolver.

Starting owners: [binding](../src/nyx.pascal.binding.pas),
[type plans](../src/nyx.pascal.types.pas) and the existing generic-binding,
routine-application, inference and operator consumers in tests/.

**Acceptance Criteria:**

- Enumerate actual remaining failing call witnesses and distinguish generic
  substitution, constraint checking, candidate eligibility and ranking.
- Preserve type-own versus routine-own formal owners, nested shadowing,
  defaults and authored overload/operator identity throughout specialization.
- Return a unique call result only when justified by captured dialect rules;
  preserve explicit ambiguity, unsupported constraints and missing authority.
- Match compiler behavior for named/nested callbacks and supported anonymous
  forms using actual version capabilities, never a guessed FPC cutoff.
- Exercise receiver/result completion and definition navigation for accepted
  calls, plus negative ambiguity/constraint/overflow/cancellation cases under
  existing budgets. Do not execute user callbacks to infer types.

**Blockers**

- Existing qualified canonical type contracts are the starting prerequisite.
  Any newly required type rule belongs to the affected portion of
  [type/helper task04](NS-4_pascal-resolver_04.md); completing all helper
  activation is not a prerequisite for ordinary generic-call implementation.
