# NS-3_components_01 — Advanced component depth and reuse

[North stars](../MILESTONES.md) · [Task catalog](README.md) · [Task flow](../TASKFLOW.MD)

**Description:**

Deliver advanced component depth and reuse as part of the user's full Nyx and Nyx Studio outcome.
Starting evidence: inherited fluent browser prototype; new units and validation
are tracked in [WORK.md](../WORK.md). North-star owner: NS-3.
Completion credit: pending evidence-backed assessment; no credit is earned by partial code.

**Acceptance Criteria:**

- Cover layout, inputs, data, navigation, media, overlays, feedback and authoring components with usable implementations.
- Compound parts, nested named slots, variants, state, events and arbitrary added actions are customizable and reusable.
- Advanced behavior (data virtualization, sorting/filtering, trees, tables, dialogs, pickers, drag/drop) is demonstrated beyond static recipes.

**Blockers**

- [Observable structured state](DONE/NS-1_state-collections_01.md) supplies typed
  collection/item bindings for the required production data-control families.

- [NS-1_catalog-theme_01](DONE/NS-1_catalog-theme_01.md) has accepted evidence.
- [NS-2_parity-accessibility_01](NS-2_parity-accessibility_01.md) must have accepted evidence (update link when moved to DONE).

## Managed confirmation behavior — 2026-10-06

Criterion 3 now includes an actual managed browser/LCL presentation of an
independently cloned confirmation recipe, typed focus/sizing, arbitrary added
actions, cancellation, ordered callbacks, callback-driven reopen/release and
host retirement. Actual Win32 passes 36 with zero leaks; desktop/narrow browser
passes 35 each using the same exact MCP-authored companion. Studio consumes the
shared recipe for its inline registration warning; actual native queue checks
pass 57 and source workspace 30, leak-free. See
[evidence](../WORK.md#managed-confirmation-presentation--2026-10-06) and
[public usage](../docs/confirmation.md).

Stop this local confirmation batch. No original criterion or renderer prerequisite
closes. The later [NS-4 real-worker packet](../WORK.md#real-browser-worker-and-contextual-warnings--2026-10-06)
qualifies ordinary browser Studio's shared inline warning; authenticated observing
deployment remains open. Full modal/picker/
overlay behavior, nested native modality, hardware/assistive input, widgetsets,
accessibility, aesthetics, virtualization/performance and other production
families retain their original acceptance. Protected history/observing rollout
is unchanged; offline checks cannot establish that service prerequisite.

## Creator metadata — 2026-10-03

All default kinds have high-level descriptions, one intent group and useful
cross-cutting labels/aliases. The public catalog Describe API lets creators
describe custom kinds and recipes with typed metadata and descriptive user text.
Studio consumes it in optional grouped discovery, search, native/browser hints
and touch-visible Details; the reference generator includes the same data.
The selected component's Inspector now displays this same creator explanation
above Properties/Events, preserving custom recipe intent instead of substituting
layout-root help. [Contextual help evidence](../WORK.md#selected-component-intent-help--2026-10-03)
includes executed shared suites, desktop/exact-390px Studio and actual LCL labels.
[Evidence](../WORK.md#optional-component-discovery-and-creator-help--2026-10-03)
includes shared/native/browser consumers. This is discovery/help integration;
the original production behavior, accessibility, parity and depth criteria stay open.
