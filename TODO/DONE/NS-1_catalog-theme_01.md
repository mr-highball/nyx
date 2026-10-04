# NS-1_catalog-theme_01 — Composable catalog and semantic themes

[North stars](../../MILESTONES.md) · [Task catalog](../README.md) · [Task flow](../../TASKFLOW.MD)

**Description:**

Deliver composable catalog and semantic themes as part of the user's full Nyx and Nyx Studio outcome.
Starting evidence: inherited fluent browser prototype; new units and validation
are tracked in [WORK.md](../../WORK.md). North-star owner: NS-1.
Completion credit: pending evidence-backed assessment; no credit is earned by partial code.

**Acceptance Criteria:**

- Primitives and compound recipes expose documented defaults and named parts.
- Recipes can be instantiated independently, customized and derived without patching renderers.
- Theme tokens render coherently in both targets; actual representative visual evidence is recorded.

Accepted 2026-10-03 for the implemented public contract. The generated
[reference](../../docs/components-reference.md)
documents all 75 kinds and named compound parts. Shared fixtures, actual browser/LCL
controls and compiled Pascal exercise successive primitive derivation, factory
precedence, independent instances, nested part overrides and payload ownership.
Studio exposes the same API through its Nyx inspector/palette and history.
[WORK.md](../../WORK.md) records 114 shared, 35 DOM and 20 HTTP checks, including
primitive/reusable insertion into a customized slot and rollback ownership.

The theme criterion is accepted from inspected light/dark/390-pixel narrow,
instance-part and custom-palette captures on both targets. Native captured pixels
verify surface, accent, borders and focus/blur; five browser variants each pass
15 computed-style/layout checks. Caller-selected font/radii, simultaneous browser
palettes and invalid-theme recovery are exercised. Native journeys also retain
Space/Enter, disabled-ancestor refusal and Unicode accessible names. Checked
portable heap tracing reports 380760 allocations/frees and zero unfreed blocks.
The [theme guide](../../docs/themes.md) specifies public lifetime/validation and
native control contracts. Sources: [native widgets](../../src/nyx.widgets.lcl.pas),
[native capture](../../tests/nyx_lcl_visual.lpr),
[browser capture](../../tests/nyx_browser_visual.lpr).

This accepts the composable catalog/theme foundation. Full production family
behavior, property-level capability parity, general responsive layout, high DPI,
accessibility and measured performance retain their existing task owners.
Rounded child clipping and widgetset-painted selection/scrollbar chrome remain
explicit limits. No broader milestone or completion percentage is inferred.

**Blockers**

- [NS-1_model_01](NS-1_model_01.md) has accepted evidence.
