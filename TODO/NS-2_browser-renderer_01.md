# NS-2_browser-renderer_01 — Browser projection

[North stars](../MILESTONES.md) · [Task catalog](README.md) · [Task flow](../TASKFLOW.MD)

**Description:**

Deliver browser projection as part of the user's full Nyx and Nyx Studio outcome.
Starting evidence: inherited fluent browser prototype; new units and validation
are tracked in [WORK.md](../WORK.md). North-star owner: NS-2.
Completion credit: pending evidence-backed assessment; no credit is earned by partial code.

**Acceptance Criteria:**

- One multi-page/reusable document renders semantic browser controls with theme/layout and events.
- A representative compound supports actual input/activation; runtime updates preserve focus and target identity.
- Desktop/narrow screenshots and runtime readiness/failure recovery are verified over HTTP.

**Blockers**

- [NS-1_model_01](DONE/NS-1_model_01.md) must have accepted evidence (update link when moved to DONE).
- [NS-1_catalog-theme_01](DONE/NS-1_catalog-theme_01.md) has accepted evidence.

## Structural recipe consumer — 2026-10-06

The public managed content contract selects distinct reusable control sets lazily
on initial mounts and explicit remounts. Actual browser checks pass 42 against
the unchanged compiled supplementary-name companion, including independent stores,
input/memo sets, exact edits, callbacks once, repeated remounts and invalid inactive
branch preservation. Shared browser contracts pass 50. See
[evidence and limits](../WORK.md#content-recipes--2026-10-06).

Original criteria remain open. Explicit remounts retire their old controls and
lease; live resize/manual structural publication, retained identity/focus/drafts,
desktop/narrow visual quality and the ordinary Studio journey retain their original
owners. Teardown captures do not qualify visual aesthetics or physical phone input.
