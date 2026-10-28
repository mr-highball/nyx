# NS-2_button-caption_01 — Readable captions in constrained button hosts

[North stars](../MILESTONES.md) · [Tasks](README.md) · [Task flow](../TASKFLOW.MD)

**Description:**

Make caption behavior explicit and useful when glyph-bearing buttons receive
limited width. The native Studio Project switcher currently clips long project
names. The native button paint path forces single-line text, while its preferred
measurement assumes an unconstrained caption. Give application authors a typed,
portable policy with coherent measurement and rendering rather than relying on
browser wrapping or caller-specific native drawing.

North star: NS-2, consumed by component usefulness and Studio project navigation.
No separate completion credit; original button, renderer and Studio criteria
remain. Starting evidence: [current work](../WORK.md), native button paint and
preferred-size calculation, and the private studio-project-lifecycle-02 switcher
capture. The separate viewport-context task owns incorrect column allocation.
The2026-10-28 canvas-uniform-native-return-01 Studio diagnostic redraw also clips
the ordinary Import project or paired files caption. Retain this representative
Project action with the switcher cases; it is not a separate button workaround.

**Acceptance Criteria:**

- Typed caption flow/overflow choices define wrapping, constrained measurement,
  alignment, glyph placement and minimum touch size across both adapters. Preserve
  existing defaults, serialization, generated Pascal and creator presentation.
- Long English and supplementary Unicode captions remain readable at narrow and
  wide sizes, with coherent preferred height and no glyph overlap. Accessible
  names and keyboard/focus behavior remain complete when overflow is intentional.
- Real button regressions cover resize, retained updates and wrapped/one-line
  faces. Studio's ordinary project switcher and a saved generated consumer exercise
  the policy on both targets. Inspect representative rendered results; compilation
  and geometry counts alone do not establish visual acceptance.

**Blockers**

- None.
