# NS-2_parity-accessibility_01 — Parity, accessibility and responsive layouts

[North stars](../MILESTONES.md) · [Task catalog](README.md) · [Task flow](../TASKFLOW.MD)

**Description:**

Deliver parity, accessibility and responsive layouts as part of the user's full Nyx and Nyx Studio outcome.
Starting evidence: inherited fluent browser prototype; new units and validation
are tracked in [WORK.md](../WORK.md). North-star owner: NS-2.
Completion credit: pending evidence-backed assessment; no credit is earned by partial code.

**Acceptance Criteria:**

- Publish a control/property/event capability matrix backed by shared tests and real target journeys.
- Keyboard navigation, focus, disabled/read-only state, validation and accessible naming meet documented semantics.
- Responsive layout, themes, native scaling and representative visual quality have target evidence.

**Blockers**

- [NS-2_browser-renderer_01](NS-2_browser-renderer_01.md) must have accepted evidence (update link when moved to DONE).
- [NS-2_lcl-renderer_01](NS-2_lcl-renderer_01.md) must have accepted evidence (update link when moved to DONE).

## Proportional and hidden flow — 2026-10-04

The later proportional/hidden-flow packet is documented in
[layout](../docs/layout.md) and WORK.md. It qualifies one shared semantic review
on actual LCL and desktop/390 browser controls, including retained editing focus,
collection/split descendants and width capping. It also corrects browser zero
weight and minimum-height behavior. Original renderer prerequisites and all three
criteria remain open for their full intended scope; natural sizing, richer
responsive policies, hardware/widgetsets, scaling and accessibility are not
inferred from this bounded geometry evidence.

The following typed-policy packet extends this evidence through enum/value
authoring, unchanged compiled semantic source, actual wrap/alignment/sizing and
retained input state: 2,169 native / 2,214 desktop / 2,215 actual-390 checks.
Nyx Studio passes its desktop and four-width resize journeys. All three original
criteria and renderer prerequisites remain open; intrinsic/shrinking constraints,
typographic scaling, representative aesthetics and complete accessibility are
still required. Two consecutive implementation batches have closed no full
criterion. The reassessed next action follows the existing NS-4 protected review
workspace prerequisite, then returns to these original target outcomes.

## Collection interaction boundary — 2026-10-03

Automatically generated collection editors now inherit enabled/read-only policy
from their root and ancestors on both targets. Disabled callbacks preserve
selection; read-only permits selection and refuses edits. Browser design mode
prevents collection data writes. This evidence does not close universal policy
parity: ordinary scalar inputs still use their local read-only setting, and
ancestor read-only behavior for that family remains under this task's original
interaction criterion. No criterion is weakened or accepted from collection-only
coverage. See [WORK](../WORK.md#authored-collection-bindings-and-studio--2026-10-03).

## Integrated keyboard focus — 2026-10-04

Bound collections now restore actual browser focus after a removed row, keep an
empty composite reachable and remove disabled entries from the Tab order.
Read-only text-cell inputs stay enabled and focusable; read-only checkboxes use
disabled host behavior. Browser grid editors support F2/Enter, in-row Tab,
Escape and ordinary Tab exit. Host-generated keyboard input qualifies ordered
compound actions and disabled descendants. Actual LCL controls and compiled
MCP-authored source retain native focus, standard editing and semantic activation.
Studio and agents display the same verified intent/help metadata.

The [packet](../WORK.md#mcp-authored-keyboard-review--2026-10-04) passes 154/154
native selection checks with zero leaks, 181 executed browser checks, host
keyboard and retained gesture journeys, 52/52 Studio, 177 HTTP and 55 MCP.
Row-oriented navigation is the documented scope. Full cell-grid patterns,
typeahead, assistive technology, physical hardware/IME and other widgetsets
retain criterion 2; visual/scaling breadth retains criterion 3. No original
criterion or renderer prerequisite is weakened or marked accepted by this packet.

## Property capabilities and inherited input policy — 2026-10-04

Criterion 1's publication deliverable now has a generated control/property/event
matrix for all 76 default catalog kinds. The 41 primitive entries already include
the component reference; it must not be counted a second time. Typed immutable
property support distinguishes contract, presentation, interaction and custom
meaning, with target grades and help. Studio and bounded MCP queries consume the
same schema. Native image/link/placeholder and custom-face boundaries are explicit.
This is implementation metadata backed by current consumer evidence, not visual
approval or hardware qualification of every property combination.

The scalar ancestor read-only gap recorded above is resolved. Both adapters and
portable bindings/compound commands use one immutable inherited interaction
policy, including unbound action targets. Child false cannot defeat ancestor
read-only; rejected physical drafts publish no text hooks or state change.
Keyboard observation and deliberate programmatic state updates remain available.
Selectors without standard read-only modes explicitly have basic restore support.

Current evidence: 171 native/executed-browser interaction contracts, 45 actual
controls on each adapter, 69 desktop / 69 exact-390 Studio checks, 71 actual native
Studio checks, 29/29 portable semantic-agent checks and 30 real MCP HTTP checks.
The matrix and guide are [here](../docs/capabilities.md); commands/artifacts are in
[WORK](../WORK.md#property-capabilities-and-inherited-input-policy--2026-10-04).
Full task acceptance still requires its original renderer prerequisites and
criteria 2/3. Universal accessibility, focus/navigation, widgetsets, scaling and
production component breadth remain open; no acceptance requirement is weakened.
