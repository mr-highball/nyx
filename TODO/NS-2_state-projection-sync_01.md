# NS-2_state-projection-sync_01 — Update bound presentations without resynchronizing unrelated controls

[North stars](../MILESTONES.md#assessment) · [Task catalog](README.md) ·
[Live inspection consumer](NS-3_control-usefulness_04.md) · [Task flow](../TASKFLOW.MD)

**Description:**

An ordinary bound status change currently calls the renderer's complete Sync.
That revisits unrelated controls and viewport/layout work while the collection
has already published its own admitted row update. The current same-application
timing and source review are recorded in WORK's publication-cost reassessment.
This affects the user's live application experience, beyond a Table-specific
optimization. Preserve the existing complete validation and publication contract.

North-star owner: NS-2; the portable binding owner and both renderers supply the
implementation. NS-3 live inspection and Nyx Studio consume it. No separate
completion credit is allocated before evidence-backed assessment. The original
100ms p95/250ms maximum application requirements remain owned by workbench04;
this prerequisite does not replace or relax them.

**Acceptance Criteria:**

- Ordinary public state writes can synchronize their admitted presentation
  changes without replaying unrelated control setters. Use typed copied changes
  and a documented target capability/fallback contract. Authors keep the same
  fluent bindings; neither target internals nor raw property strings become an
  application requirement. Existing callback/interface consumers remain valid.
- Complete candidate, scalar/domain, computed-expression, behavior and projection
  admission still precede publication. Missing/changed contracts, unsupported
  target faces and uncertain dependencies use the existing complete path. A
  dependency hint, matching count/revision or retained old candidate never grants
  admission or substitutes for evaluating current computed output.
- Text changes settle actual wrapping, intrinsic dimensions, parent allocation,
  viewport/container presentations and accessibility names. Boolean visibility,
  enabled/read-only state, value changes and multiple affected controls retain
  their existing semantics or deliberately take the complete fallback. Custom
  factories and content hosts must not silently miss required synchronization.
- Publication retains exact runtime node/control identities, unfinished drafts,
  caret, scroll, focus and independent collection peers. Application observers
  retain order and committed-notification failure behavior. Prepared changes
  own no renderer/widget cycle; replacement, callback retirement, cancellation,
  composition and failed target publication cannot affect a successor.
- Execute meaningful ordinary native and browser binding/control regressions,
  including a changing status beside an active Table editor, a wrapped caption,
  computed multiple outputs, conditional layout/visibility and target refusal.
  Retain baseline failures and complete clean native heaps. Compilation alone
  does not establish target behavior, input or layout correctness.
- Return to the unchanged public 10000-service/five-field/160px application and
  its exact generated Pascal at desktop/compact widths. Measure ordinary builds
  separately from instrumented diagnostics, preserve every original assertion
  and latency threshold, and identify which costs improved or remain. Do not
  substitute a label-only benchmark or infer complete L2/Studio acceptance from
  this narrower prerequisite. Document target/widgetset/physical/AT limits.

**Blockers**

- [Scalar state admission](DONE/NS-1_persistence-state_01.md) supplies the existing
  accepted atomic state/binding foundation. Build on its current retirement and
  prepared-publication consumers; do not reopen its historical task identity.
