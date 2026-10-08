# NS-1_time-values_01 — Typed clock-time values and field contracts

[Milestones](../../MILESTONES.md) · [Catalog](../README.md) · [Task flow](../../TASKFLOW.MD)

**Description:**

Supply the strongly typed prerequisite for the existing native/browser time-field
and advanced-picker requirement. A native text fallback or a locale-based widget
swap cannot establish the same portable value. This is required scope from NS-2
renderer criterion 3 and NS-3 component criteria 1/3, not additional completion
credit. Those full criteria and their other families remain with their original
owners. Allocation: pending assessment; one NS-1 owner for this shared contract.

Preparation checkpoint (2026-10-07): portable values/domains and managed source integration
are implemented in maintained Pascal. Checked FPC passes 1,613 clock checks,
49 unchanged calendar checks and exact compiled reconstruction, leak-free.
Six wrong argument families refused on each compiler. Browser fixtures/companion,
Studio and worker compiled with zero owned warnings; execution was then unverified.
Imported intermediate step-base refusal now retains exact atomic Metadata.
See [evidence and limits](../../WORK.md#current-return-path-typed-time-field-prerequisite--2026-10-07)
and [public usage](../../docs/time-fields.md). This prerequisite was not yet accepted.

Two partial preparation batches triggered reassessment and stopped fixture
expansion. Criterion 4 required executed pas2js fixtures through an admitted HTTP/
browser path. The accepted execution below supplies that missing evidence;
no equivalent previously rejected service launch is retried. Broader native/browser
picker acceptance still requires its original physical consumer evidence.

**Acceptance Criteria:**

- Immutable Delphi-dialect time-of-day values distinguish empty from midnight,
  preserve valid ASCII wire precision and compare exact integer milliseconds.
  Typed parts/precision reject invalid or lossy values without locale, time zones,
  DOM/LCL types, timestamps or mutable shared storage.
- Fluent typed domains admit exact time values, optional empty values, inclusive
  ordinary/midnight-crossing ranges, choices and explicit millisecond steps.
  Descriptor validation is atomic; malformed/wrong-family data retains the prior
  specification and application state. Portable persistence/history retain meaning.
- Public managed time-control configuration, crafted generated Pascal and bounded
  source reconstruction retain values/domains without behavioral property strings.
  Wrong argument families fail on FPC and pas2js. Exact emitted source compiles
  and reconstructs the document; unknown/invalid metadata fails before publication.
- Checked FPC and executed pas2js shared fixtures establish independent ownership,
  exact text/precision, admission and source/persistence behavior. Compilation alone
  does not accept browser execution or native/browser physical picker behavior.

**Blockers**

- [Portable model](NS-1_model_01.md) supplies independent owned descriptors.
- [Typed state](NS-1_state-collections_01.md) supplies immutable scalar stores.
- [Managed interfaces](NS-1_component-interfaces_01.md) supplies authoring lifetime.

Return to the original [LCL renderer](../NS-2_lcl-renderer_01.md),
[browser renderer](../NS-2_browser-renderer_01.md),
[parity](../NS-2_parity-accessibility_01.md) and
[advanced components](../NS-3_components_01.md) for actual time fields/pickers,
focus/drafts, native ownership, accessibility and visuals. Studio/MCP value-domain
authoring remains with [the existing workflow owner](../NS-4_agent-workflows_01.md).
No new standalone fixture can substitute for those consumer requirements.

Standards: [HTML time syntax](https://html.spec.whatwg.org/multipage/common-microsyntaxes.html#valid-time-string)
and [time controls/ranges](https://html.spec.whatwg.org/multipage/input.html#time-state-(type=time)),
checked 2026-10-08. This models a clock reading, not an instant or date/time zone.

## Accepted delivery — 2026-10-08

All four original prerequisite criteria are accepted, solo. Their scope is the
portable clock value/domain/authoring contract; physical pickers and complete
Studio/parity/component outcomes remain with the original consumers above.

| Original criterion | Authoritative evidence |
| --- | --- |
| Immutable exact values | `nyx.times` depends only on SysUtils/portable text; shared `nyx_time_values_tests` executes empty/midnight, all minute readings, valid/invalid ASCII wire, precise parts, independent copies, comparison and lossy-precision refusal on FPC and actual pas2js. |
| Typed atomic domains | The same executed suite covers inclusive/overnight ranges, exact steps and bases, independent AnyStep/choices, duplicate/wrong-family/overflow refusal. `nyx.test.times` checks descriptor publication, independent runtime stores and authored defaults, atomic group refusal and persistence. |
| Managed authoring/source | `nyx.test.times` checks specialized interfaces, typed values/domains/precision, exact persistence/source reconstruction, malformed source refusal and paired Undo/Redo. Exact emitted `nyx_time_reconstruction` compiles and executes on both targets; all six wrong argument families refuse on each compiler for the intended expected type. |
| Both-target checked ownership/admission | Maintained FPC passes **1,613** clock checks plus **49** calendar regressions; actual HTTP browser execution reports the identical counts. Exact reconstruction passes on both targets. Checked native suite and native browser-driver receipts report zero unfreed allocations. |

Evidence under ignored `build/time-prerequisite-execution/`: `maintained.log`,
`values-native-final-compile.log`, `values-native-final.log`,
`values-browser-compile.log`, `values-browser-deferred.log`,
`reconstruction-browser.log` and terminal DOM receipts. The suite's assertions
are unchanged; browser startup defers them to the ordinary event loop so HTTP
navigation can complete first. Phase markers are diagnostic, never readiness.
The initial navigation timeout remains retained. Both affected compilers have
zero owned warnings; the matched pas2js RTL retains seven upstream warnings per
browser program. A fresh static directory on an identity-verified existing host
serves the fixtures; no backend/runtime/configuration or user design is replaced.

This prerequisite's retained two-batch no-closure checkpoint is resolved by actual
acceptance. Other owners' counts remain workflow/resource/authoring/renderer/
codegen/delivery **23/9/39/19/28/3**. Completion credits remain pending assessment;
the existing picker/parity allocation is not duplicated. See
[the acceptance packet](../../WORK.md#current-return-path-executed-clock-prerequisite--2026-10-08).
