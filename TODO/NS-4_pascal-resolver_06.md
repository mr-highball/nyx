# NS-4_pascal-resolver_06 - Close expression and conversion coverage gaps

[North stars](../MILESTONES.md) · [Task catalog](README.md) · [Task flow](../TASKFLOW.MD)

**Description:**

Deferred on 2026-10-21. Close the remaining source-owned expression typing and
conversion families needed by [Studio authoring](NS-4_studio-authoring_01.md).
North-star owner: NS-4; credit stays with the parent. Exact integer, real,
string, pointer, cast and operator mechanisms already have focused evidence;
this task must identify remaining gaps rather than restate that work as new.

Starting owners: [constants](../src/nyx.pascal.constants.pas),
[numeric values](../src/nyx.pascal.numeric.pas),
[real values](../src/nyx.pascal.reals.pas),
[binding](../src/nyx.pascal.binding.pas) and their maintained expression consumers.

**Acceptance Criteria:**

- Establish a finite expression/conversion matrix from current refusal paths
  and actual Nyx/RTL declarations, including contextual expected types.
- Preserve declaration-site binding, signedness, overflow, enum identity,
  string/code-page rules, pointer distinctions and captured FPU behavior.
- Keep constant evaluation distinct from runtime expression typing; refuse
  unavailable target facts or unsupported folding rather than guess values.
- Verify accepted cases against the pinned compiler and preserve exact negative
  cases, overload interactions and source/include origins.
- Share the same typed answers with conditions and editor queries without a
  second expression engine, hidden evaluation, quota increases or user-code execution.

**Blockers**

- [Canonical type/helper answers](NS-4_pascal-resolver_04.md).
- [Generic/overload answers](NS-4_pascal-resolver_05.md) for affected call/operator cases.
