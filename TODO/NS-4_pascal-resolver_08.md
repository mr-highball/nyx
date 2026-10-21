# NS-4_pascal-resolver_08 - Qualify cancellable resolver scheduling

[North stars](../MILESTONES.md) · [Task catalog](README.md) · [Task flow](../TASKFLOW.MD)

**Description:**

Deferred on 2026-10-21. Finish worker/cancellation/resource behavior for real
resolver requests under [Studio authoring](NS-4_studio-authoring_01.md), using
Nyx's scheduler contracts. North-star owner: NS-4; no duplicated service credit.
Existing cancellable budgets and immutable result owners remain the baseline.

Starting owners: [work budgets](../src/nyx.pascal.work.pas),
[owned provider](../src/nyx.pascal.completion.owned.pas),
[Studio completion](../studio/nyx.studio.completion.pas) and existing scheduler,
completion and source-worker lifecycle consumers.

**Acceptance Criteria:**

- Route expensive analysis through supported target scheduling without parsing
  on the UI thread or implying browser threads where only cooperative work exists.
- Cancel superseded queries and retired projects/controls during actual parsing,
  translation, binding and reuse; check cancellation before costly copies/publication.
- Reject late results using exact revision/buffer/context identity; cancellation
  must not replace accepted state, drafts, history or current suggestions.
- Join/retire native work and browser workers/listeners without leaks, orphaned
  processes, surviving callbacks or use after release.
- Exercise saturation, timeout/budget refusal and rapid typing/project switches
  on actual supported targets, recording response/memory bounds and clean heaps.

**Blockers**

- None beyond existing scheduler and resolver contracts; implementation is deferred.
