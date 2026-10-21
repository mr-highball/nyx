# NS-4_pascal-resolver_03 - Close exact source-requested compiler facts

[North stars](../MILESTONES.md) · [Task catalog](README.md) · [Task flow](../TASKFLOW.MD)

**Description:**

Deferred on 2026-10-21. Complete the authoritative request/reply path for facts
the actual source still needs, under [Studio authoring](NS-4_studio-authoring_01.md).
North-star owner: NS-4; no additional completion credit. Typed requirements,
bootstrap separation, size fulfillment and exact saved reply restoration already
exist; this task covers the remaining real-source gaps, not their reimplementation.

Starting owners: [requirements](../src/nyx.pascal.requirements.pas),
[fulfillment](../src/nyx.pascal.fulfillment.pas),
[compiler trace](../src/nyx.pascal.compilertrace.pas),
[context](../src/nyx.pascal.context.pas) and the
[retained frontier](../WORK.md#active-priority--back-to-core-nyx-2026-10-20).

**Acceptance Criteria:**

- Expose each actual unsupported condition's required fact with exact source,
  occurrence, observing context and owning compiler scope; distinguish a missing
  fact from an unsupported operation or deliberately unknown imported name.
- Validate ordered symbol/size replies, compiler identity, captured defaults,
  prior facts and independent bootstrap before applying the minimal real delta.
- Preserve pristine, bootstrap and source-local lifetimes through mode changes,
  includes and saved settings; never retag a source observation as a root fact.
- Charge planning, replay and fulfillment to existing shared quotas. Deduplicate
  only identical owning requests; refuse exhausted budgets without resetting
  counters, changing cohorts silently or recollecting accepted replies.
- Demonstrate real-source progress and negative changed-owner/partial-reply/
  missing-fact cases with clean heap and unchanged previous publication.

**Blockers**

- None in code beyond existing typed owners.
- External: any new compiler measurement needs a separately admitted exact
  request within the governing quotas. This deferred task grants no operation.
