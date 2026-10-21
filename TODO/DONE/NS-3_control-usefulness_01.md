# NS-3_control-usefulness_01 — Review every control as an application user

[North star NS-3](../../MILESTONES.md#assessment) · [Task catalog](../README.md) ·
[Component outcome](../NS-3_components_01.md) · [Task flow](../../TASKFLOW.MD)

**Description:**

Accepted on2026-10-21 as a review/planning deliverable only. The source-bound
[80-kind user-action matrix](../../docs/control-usefulness.md) covers the exact
544ca92 Pascal baseline and191E catalog:35 root rows plus45 helper rows,80 unique,
zero missing/extra/duplicates. Both primary review and independent cross-review
pass all six unchanged criteria. Root checks current API/source locators and
106 local artifact links; source1795/foreign13 remain exact. Fourteen proposed
contract directions explicitly state typed ownership, default/override and
semantic event meaning without claiming those APIs are implemented. Service
workbench stories and ranked existing owners provide the follow-up pull queue.
Zero compilers, new executions, probes or hosts are part of this review; historical
receipts/failures retain their original boundaries. No component/parity/authoring
criterion or product completion credit follows. All three implementation packets
and parent prerequisites remain open. [WORK](../../WORK.md#control-usefulness-review--2026-10-21)
records qualification, exact source scope and the next column-query design.

The user's 2026-10-21 external process/service-debugger experience exposed a
practical question: can a person using a Nyx application filter a native table's
columns without its author rebuilding that interaction? The reported application
is not available in this repository, so its implementation has not been diagnosed.
Review the public Nyx contract and actual control behavior before attributing a
missing affordance to either Nyx or the application author.

Review the complete current catalog, primitive and compound, for everyday user
usefulness. A control existing, rendering attractively, or declaring an event is
not sufficient. Determine what a user can accomplish, what an application author
must explicitly enable, and what remains inaccessible or unimplemented on each
target. Include reusable custom parts and the public extension path.

North star: NS-3. Milestone: M3, advanced behavior and useful component depth under
[components01](../NS-3_components_01.md), especially its three original criteria.
Completion credit: pending evidence-backed assessment within that existing scope;
filing or accepting a review earns no implementation or additional goal credit.
Original component, renderer, accessibility and Studio criteria remain intact.

Starting evidence: [catalog](../../src/nyx.catalog.pas),
[typed kinds](../../src/nyx.types.pas),
[specialized controls](../../src/nyx.controls.types.inc),
[configuration facades](../../src/nyx.controls.facades.inc),
[component guide](../../docs/components.md),
[collection behavior](../../docs/collection-views.md) and
[typed queries](../../docs/collection-queries.md). Filtering, stable ordering,
selection, paging, native on-demand values and browser row windows already have
implementations; they must not be reported as wholly absent because an end-user
command surface is missing. Source inspection is evidence of exposure, not a new
claim of executed target behavior.

Initial source review (2026-10-21), pending the complete audit above:

| Area | Existing foundation | Review/implementation gap |
| --- | --- | --- |
| Queries | Typed predicates, stable multi-key ordering and runtime ConfigureQuery | Native adapter installs no column filter/sort command surface; browser headings are plain text |
| Columns | Typed sizing; native retention of externally changed widths | Standard user resize controls and portable column management |
| Cells | Navigation, current-cell editing, row selection; inherited native grid copy behavior | A defined both-target copy/range/context-action contract |
| Large data | Native on-demand cell reads; measured browser row windows | Stable live inspection controls and measured whole-view responsiveness |

These findings come from the [query declarations](../../src/nyx.collections.query.pas),
[native event attachment](../../src/nyx.collections.lcl.pas),
[browser heading construction](../../src/nyx.collections.browser.pas) and the
existing collection guide. Inherited native copy behavior is not a claim that
Nyx already defines portable range/clipboard semantics. The external debugger
has not been reproduced, and none of these source reads is a new runtime verdict.
Separate literal `Items` tables from schema-bound collection tables when checking
these capabilities. The layout `Grid` is not a data-grid contract; `Table` and
table-bearing compounds must be traced to their actual collection binding.

This file is the linked subcatalog and M3 scope map for the four requested
follow-ups. The existing component entry in the main catalog/milestone map keeps
ownership; unrelated pending edits to those two files are preserved.

| Packet | Delivered result | Order |
| --- | --- | --- |
| This review | Complete per-kind usefulness matrix, ranked gaps and concrete follow-up owners | First |
| [Column filtering and sorting](../NS-3_control-usefulness_02.md) | Discoverable end-user table queries using the existing query model | After the review |
| [Table workbench interactions](../NS-3_control-usefulness_03.md) | Usable column management, cell actions and keyboard workflows | After the review; coordinate with query UI |
| [Live operational data](../NS-3_control-usefulness_04.md) | Stable inspection of changing/large results with visible update controls | After the review; reuse the other packets where applicable |

**Acceptance Criteria:**

- Publish a source-revision-bound matrix covering every registered default kind
  and compound recipe, with no silently omitted controls. For each applicable
  interaction identify: public typed configuration, actual end-user affordance,
  callbacks and payloads, Studio authoring exposure, native/browser status,
  relevant existing execution evidence and remaining verification. Classify
  supported, opt-in, implemented but unexposed, target gap, absent and not
  applicable separately. Do not equate an inherited widget feature or metadata
  declaration with a supported portable Nyx feature.
- Review task-oriented journeys across all families: forms/input/validation and
  text editing; list/table/tree browsing, searching and editing; navigation/tabs/
  disclosure; menus/dialogs/focus; media/resources; feedback/progress; layout/
  splitters; canvas/tool/authoring controls. Include empty/loading/error states,
  disabled versus read-only behavior, keyboard and pointer/touch equivalents,
  discoverable help, meaningful defaults and reversible actions where applicable.
- Use a process/service-debugger-style data workbench as the first concrete
  scenario: find a record, filter specific fields, order results, inspect/copy a
  value, retain context during updates and distinguish an empty result from a
  failed or paused feed. Trace each gap to its shared contract, target adapter,
  authoring exposure or caller configuration. Request the external application's
  source only if a claim about that particular implementation is necessary.
  Concrete stories include finding stopped services by status, ordering numeric
  CPU values, revealing a full executable path and copying selected record IDs.
- Rank findings by user impact and frequency, with a reproducible public-API
  example, expected versus current behavior, affected targets and an explicit
  owner/finish line. Cover the complete catalog even where no change is needed.
  Extend an existing task when it already owns the gap; create granular packets
  only for genuinely separate results. The table packets below do not replace
  the rest of the catalog review.
- Each proposed enhancement states its typed fluent/interface surface, useful
  semantic events, opt-in/default policy and required Studio/property/event/help/
  persistence/source/semantic-tool adoption. Record unsupported target behavior
  explicitly. Existing event ordering, multiple registrations, scheduler rules,
  lifecycle and reusable-instance isolation remain requirements.
- Independently review the matrix, prioritization and dependency map. Separate
  inspected source, reused receipts and newly executed journeys; preserve failing
  cases. This review closes only when its actionable artifact and owners exist,
  not when the later implementation tasks are merely proposed.

**Blockers**

- None. This is a review/planning deliverable using existing source and evidence;
  it does not depend on full component or parity acceptance. Any newly needed
  execution authority is recorded at the relevant validation gate.
