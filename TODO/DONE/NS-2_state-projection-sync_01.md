# NS-2_state-projection-sync_01 — Update bound presentations without resynchronizing unrelated controls

[North stars](../../MILESTONES.md#assessment) · [Task catalog](../README.md) ·
[Live inspection consumer](../NS-3_control-usefulness_04.md) · [Task flow](../../TASKFLOW.MD)

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

- [Scalar state admission](NS-1_persistence-state_01.md) supplies the existing
  accepted atomic state/binding foundation. Build on its current retirement and
  prepared-publication consumers; do not reopen its historical task identity.

**Acceptance — 2026-10-31:**

The portable copied change contract and conservative browser/LCL target paths
are implemented. The current WORK entry distinguishes model, real-control,
lifetime, compatibility and same-application timing evidence. Full candidate
admission remains independent of every observation. Missing/changed contracts,
opaque target receivers and unsupported faces retain complete synchronization.

All six unchanged criteria are accepted for the scoped changed-presentation
capability. The original application responsiveness requirement remains with
workbench04; this prerequisite requires its unchanged measurement and honest cost
assessment, not that parent's complete latency acceptance.

| Criterion | Accepted evidence |
| --- | --- |
| 1 — Typed optional target path | Copied property/reference changes, conservative face preflight and an actual unrelated-caption sentinel on both targets; legacy callbacks retain complete Sync. |
| 2 — Complete admission | 26 shared capture/coordinator checks execute under FPC/pas2js, including current computed output, refused candidates, changed contracts, runtime-policy mutation/removal and committed notification failure. Observations never grant admission. |
| 3 — Actual presentation settlement | Nine real container-control checks per target/width prove wrapping reduces a contained fill area's allocated height, synchronously changes a button's caption/enabled state and restores them on contraction. The browser repair reads current CSS content boxes and bounds repeated full passes; detached preparation retains staged geometry. |
| 4 — Continuity, observers and retirement | Actual projection controls execute 23 native/24 browser checks per width. A real setter/layout reaction fails partway, later observers still receive complete committed state and a different-key write repairs the whole target in the same stack. Exact Unicode draft/caret/focus, physical identities, independent peers and entered-frame retirement remain. Nested browser staging cleanup and disclosure focus execute 29/27 checks per width. |
| 5 — Compatibility and native heaps | Current browser bindings execute 170 per width; earlier current native binding 85+79, clipboard 1964/1953, presentation 167 and logical viewport 4205 remain applicable to the unchanged native adapter. Both new native control consumers have complete clean heaps at 1280/390. All original assertions and useful failures remain. |
| 6 — Original application return | The exact 10000-service/five-field/two-160px-table handwritten/generated consumers remain unchanged. Native ordinary and separately heap-traced runs execute 204 per variant/width with clean heaps in policy-04. Current browser runs execute 167 per variant/width and explicitly retire both views; each retains four failed original timing budgets. Unrelated setter replay is eliminated for supported changes, but whole-application latency and general speedup are not established. |

Evidence packets are `state-projection-sync-01/policy-04` and
`state-projection-edge-01`; [WORK](../../WORK.md) owns their source identities,
receipts, baseline failures and final assessment. The latter's `framed-final`
cohort verifies actual browsing contexts at 1280x900/390x844. Earlier ignored or
delayed viewport overrides do not qualify those widths. An observer console
error also reproduces in a static iframe with zero application scripts; retain
that tool-side comparison rather than claiming empty logs.

Qualification covers Win32 LCL and the current in-app browser, with emulated
compact contexts. Physical phone/IME/assistive input, other widgetsets/OSes,
complete L2 and Studio acceptance remain separate. No new north-star percentage
or duplicate completion credit is assigned. Return to the original workbench
criteria; stop equivalent projection/performance fixtures.
