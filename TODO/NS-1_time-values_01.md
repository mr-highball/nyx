# NS-1_time-values_01 — Typed clock-time values and field contracts

[Milestones](../MILESTONES.md) · [Catalog](README.md) · [Task flow](../TASKFLOW.MD)

**Description:**

Supply the strongly typed prerequisite for the existing native/browser time-field
and advanced-picker requirement. A native text fallback or a locale-based widget
swap cannot establish the same portable value. This is required scope from NS-2
renderer criterion 3 and NS-3 component criteria 1/3, not additional completion
credit. Those full criteria and their other families remain with their original
owners. Allocation: pending assessment; one NS-1 owner for this shared contract.

Checkpoint (2026-10-07): portable values/domains and managed source integration
are implemented in maintained Pascal. Checked FPC passes 1,613 clock checks,
49 unchanged calendar checks and exact compiled reconstruction, leak-free.
Six wrong argument families refuse on each compiler. Browser fixtures/companion,
Studio and worker compile with zero owned warnings; execution remains unverified.
Imported intermediate step-base refusal now retains exact atomic Metadata.
See [evidence and limits](../WORK.md#current-return-path-typed-time-field-prerequisite--2026-10-07)
and [public usage](../docs/time-fields.md). This prerequisite is not DONE.

Two partial batches without full acceptance trigger reassessment: stop fixture
expansion. Criterion 4 requires executed pas2js fixtures through an admitted HTTP/
browser path. No equivalent previously rejected launch is retried. Independent
native-picker preparation can inspect existing adapter ownership/input contracts;
consumer acceptance still requires this complete prerequisite and both-target input.
No original renderer/component criterion or established counter closes/resets.

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

- [Portable model](DONE/NS-1_model_01.md) supplies independent owned descriptors.
- [Typed state](DONE/NS-1_state-collections_01.md) supplies immutable scalar stores.
- [Managed interfaces](DONE/NS-1_component-interfaces_01.md) supplies authoring lifetime.

Return to the original [LCL renderer](NS-2_lcl-renderer_01.md),
[browser renderer](NS-2_browser-renderer_01.md),
[parity](NS-2_parity-accessibility_01.md) and
[advanced components](NS-3_components_01.md) for actual time fields/pickers,
focus/drafts, native ownership, accessibility and visuals. Studio/MCP value-domain
authoring remains with [the existing workflow owner](NS-4_agent-workflows_01.md).
No new standalone fixture can substitute for those consumer requirements.

Standards: [HTML time syntax](https://html.spec.whatwg.org/multipage/common-microsyntaxes.html#valid-time-string)
and [time controls/ranges](https://html.spec.whatwg.org/multipage/input.html#time-state-(type=time)),
checked 2026-10-07. This models a clock reading, not an instant or date/time zone.
