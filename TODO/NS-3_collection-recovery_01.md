# NS-3_collection-recovery_01 — Review recovered table edits through ordinary controls

[North star NS-3](../MILESTONES.md#assessment) · [Task catalog](README.md) ·
[Table workbench](NS-3_control-usefulness_03.md) · [Task flow](../TASKFLOW.MD)

**Description:**

The saved Service monitor's ordinary copy/filter journey exposes a user-facing
gap: filtering an edited row out transfers its unfinished text to the existing
inert recovery journal, but a standard Table offers no way to review that journal.
Reopening the row shows accepted text by design. Existing source and recovery
tests deliberately prohibit silently reapplying a recovered draft; retain that
contract. The public live-inspection example has application-specific review
controls, which are starting evidence rather than a reusable standard capability.

Implement reusable, specialized Nyx recovery review controls and a reachable
entry point on ordinary editable tables, including an empty filtered result.
Use the existing journal, immutable entry identities, popover/menu ownership and
explicit clipboard host. Do not create a second draft store, automatic restore,
implicit discard or new clipboard authority. This is a prerequisite of the
existing table workbench; it adds no separate completion percentage or credit.

**Acceptance Criteria:**

- A typed specialized compound presents a detached entry's exact item/field,
  typed loss reason, before/after revisions, unfinished text and accepted value.
  Labels distinguish inert recovered text from live editing. Preview escaping
  never changes the retained Unicode, NUL, line endings, caret or raw copy value.
- Ordinary Table users can discover a journal count and open bounded review,
  including when all rows are filtered out. Multiple entries remain individually
  navigable by stable captured identity. Counts and navigation tolerate a newer
  capture or deliberate external discard without displaying another entry as the
  original one. Alternative mounts may lack the optional capability honestly.
- Readonly review and exact raw Copy use existing Nyx controls and explicitly
  configured hosts. Inspecting/navigating does not commit a live draft, modify
  accepted data, apply a recovered value, discard an entry or grant file/network
  access. Disabled/unavailable clipboard results remain visible. Deletion and
  automatic restoration are outside this task; existing explicit journal APIs
  remain available to application authors.
- Ownership is explicit and acyclic: detached presentation retains values only;
  runtime controllers retire subscriptions, requests and borrowed target access.
  Old callbacks cannot act on replacement mounts. Existing recovery capacity,
  source identity checks and original input admission remain authoritative.
- The same saved/generated Service monitor reproduces the missing entry point,
  then reviews its actual filtered-out draft through browser and native controls
  at wide/compact widths. Preserve exact paired source/history, local output
  choices and unrelated projects. Retain meaningful existing regression checks,
  failure evidence, clean native heaps and honest physical/AT/parity limits.

**Blockers**

- None for implementation. Existing draft recovery, managed presentation, exact
  text controls and configured clipboard hosts are available.
