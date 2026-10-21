# NS-4_pascal-resolver_09 - Adopt the owned resolver in ordinary completion

[North stars](../MILESTONES.md) · [Task catalog](README.md) · [Task flow](../TASKFLOW.MD)

**Description:**

Deferred on 2026-10-21. Deliver the selected MIT Nyx-owned provider through the
ordinary Nyx-built source editor under [Studio authoring](NS-4_studio-authoring_01.md).
North-star owner: NS-4; existing popup/provider seams are partial evidence, not
default owned-engine adoption. Do not introduce a CodeTools dependency.

Starting owners: [portable completion](../src/nyx.pascal.completion.pas),
[owned provider](../src/nyx.pascal.completion.owned.pas),
[Studio completion](../studio/nyx.studio.completion.pas) and
[source input](../studio/nyx.studio.sourceinput.pas).

**Acceptance Criteria:**

- Select/configure the owned engine through existing typed provider seams;
  unavailable compiler context gives a useful explicit status and usable editor.
- Exercise real typed invocation, filtering/paging, keyboard/mouse/touch selection,
  dismissal and insertion in native and browser editors, including expanded source.
- Replace the entire intended Unicode-scalar identifier and preserve neighboring
  text, caret/selection, drafts and one ordinary Undo/Redo publication.
- Refuse stale buffers, includes, revisions, context and project switches without
  insertion; retain accessibility/focus and caller cancellation.
- Demonstrate actual Nyx and real captured library suggestions, not controlled
  fixture replies. Keep supported/refused language cases visible and independently
  qualify wide/compact input, latency and recovery.

**Blockers**

- [Canonical types](NS-4_pascal-resolver_04.md),
  [generic calls](NS-4_pascal-resolver_05.md) and
  [expressions](NS-4_pascal-resolver_06.md).
- [Incremental analysis](NS-4_pascal-resolver_07.md),
  [cancellable scheduling](NS-4_pascal-resolver_08.md) and
  [target context](NS-4_pascal-resolver_11.md).
