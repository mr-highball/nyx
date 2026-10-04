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
