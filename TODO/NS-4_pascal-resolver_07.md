# NS-4_pascal-resolver_07 - Deliver dependency-aware incremental analysis

[North stars](../MILESTONES.md) · [Task catalog](README.md) · [Task flow](../TASKFLOW.MD)

**Description:**

Deferred on 2026-10-21. Make repeated real-project editor queries reuse valid
analysis with correct dependency invalidation under
[Studio authoring](NS-4_studio-authoring_01.md). North-star owner: NS-4; no added
credit. Existing bounded lexical/projection reuse is qualified separately from
full dependency-aware semantic reuse and production latency.

Starting owners: [owned completion](../src/nyx.pascal.completion.owned.pas),
[modules](../src/nyx.pascal.modules.pas),
[work accounting](../src/nyx.pascal.work.pas) and existing contextual-reuse,
dependencies, source-rebind and semantic-replay consumers.

**Acceptance Criteria:**

- Define cache identities using exact source/buffer membership, include
  occurrences, compiler/default context, imported interface and ordered answers.
- Reuse unchanged plans/worlds only when their complete dependencies match;
  invalidate affected callers after unit/include/define/mode/type changes.
- Preserve old immutable answers and safely refuse changed or incomplete input;
  no cached absence may survive unknown imports or a changed source owner.
- Measure cold, warm, local-edit, include-edit and dependency-edit work/latency/
  memory on representative real projects with declared targets and exact output
  equality; report misses and remaining limits instead of asserting speedup.
- Preserve existing caps, cancellation and owned lifetime/eviction behavior.
  Full reconstruction or whole-source recharging is not incremental evidence.

**Blockers**

- Final real-library corpus qualification consumes
  [SysUtils](NS-4_pascal-resolver_01.md) and
  [Classes closure](NS-4_pascal-resolver_02.md). Implementation and measurements
  may begin against already qualified real project captures; full library
  closure is not a prerequisite for all incremental work.
