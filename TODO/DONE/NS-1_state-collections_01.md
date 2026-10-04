# NS-1_state-collections_01 — Observable structured state

[North stars](../../MILESTONES.md) · [Task catalog](../README.md) · [Task flow](../../TASKFLOW.MD)

**Description:**

Provide strongly typed observable structured state for the user's advanced list,
table, tree and compound-component vision. The admitted scalar state and immutable
extension data are the starting foundation; opaque JSON arrays are not an
observable collection implementation. This is required product scope beyond the
version-1 scalar persistence/state foundation. North-star owner: NS-1.
Accepted 2026-10-03 from the evidence below. Overall weighting remains pending
assessment; this bounded acceptance does not complete its consuming product goals.

**Acceptance Criteria:**

- Typed collection/item/field references expose defined schemas and ordered,
  independently owned values through a documented Pascal-first fluent API.
- Atomic insert/remove/reorder/item updates preserve accepted state on rejection
  and have owned change snapshots, explicit subscription lifetimes and tested
  reusable-instance/default isolation on FPC and pas2js.
- Versioned persistence, Studio history and compiled generated source reproduce
  collection meaning and Unicode without weakening existing scalar/wire admission.
- Shared bindings and actual browser/LCL controls exercise collection edits,
  selection identity and rejected-command recovery; large-data update costs and
  remaining virtualization limits are measured and assigned to their task owners.

**Blockers**

- [Persistence and shared scalar state](NS-1_persistence-state_01.md) has
  accepted version-1 both-target evidence.

Broader component virtualization/performance remains with
[NS-3_extension-performance_01](../NS-3_extension-performance_01.md). This task does
not substitute for complete production list/table/tree behavior or full Studio.

## Typed store foundation — 2026-10-03

Criterion 1 is accepted for the documented public Pascal contract: distinct
collection/item/four-family field references, immutable fluent schemas and item
values, ordered admission, specialized managed interfaces and scoped stable
identity. Criterion 2's atomic store/snapshot/subscription/default-clone portions
are implemented and evaluated on both targets; actual reusable-instance
integration remains open. Criteria 3/4 remain open in full. No task moves to DONE
or receives completion credit from this bounded foundation.

Evidence: 92 shared checks on checked FPC 3.2.0 and executed pas2js, an optimized
FPC 3.3.1 run, 210671 allocations/frees with zero unfreed blocks, 16384-row and
8-MiB rejection/metadata tests, and seven intended wrong-type rejections from
each compiler. The maintained native shared suite totals 1399 checks. Both-target
benchmarks verify results before measuring setup, twenty transactions and twenty
thousand identity lookups. Exact artifacts and corrections are recorded in
[WORK](../../WORK.md#typed-observable-collection-foundation--2026-10-03).
The public API, lifetimes, error semantics, budgets and costs are documented in
[collections](../../docs/collections.md).

Consecutive batches without full criterion closure: 0, because criterion 1 closes
in this batch. Next integrated package: document/runtime collection registries,
versioned persistence, history and compiled generated reconstruction, preserving
existing scalar/wire admission and Unicode. Then consume that contract through
real browser/LCL bindings and list/table/tree controls, including stable selection,
rejected-command recovery and reusable-instance isolation. Store-only fixtures
cannot close those remaining criteria. Production virtualization/performance
keeps its original NS-3 owner; no acceptance criteria are weakened.

## Registry, wire and compiled source integration — 2026-10-03

Criterion 3 is accepted: versioned design/descriptor admission, Unicode/finite
numbers, exact paired Studio source/history, actual compiled reconstruction and
all six delegated application/page/reusable scopes on both targets. Version-1
opaque collection-named extensions are preserved; explicit collisions reject.
Criterion 1 remains accepted. Atomic store/default-clone/sibling-application
portions of criterion 2 are evaluated, including actual browser/LCL mounts and
navigation; independent reusable-instance collection bindings remain open.
Criterion 4 remains open. No task moves to DONE or gains full-product credit.

Evidence: 149 collection cases, 1456 native/executed-browser shared cases, three
compiled reconstruction checks per target, 15 managed target checks (five new
actual application cases), existing 49 browser interactions and 135 HTTP checks.
Exact logs, corrected failures, lifetime/Unicode decisions and service identity
are in [WORK](../../WORK.md#collection-registries-persistence-and-source--2026-10-03).
Public contracts and budgets are in [collections](../../docs/collections.md).

Consecutive batches without full criterion closure: 0, because criterion 3 closes
here. Next package: typed shared collection bindings consumed by actual browser/
LCL list/table/tree controls and independent reusable instances. Prove stable
selection identity, insert/remove/reorder/edit operations, rejected-command
recovery and owned subscription disposal through real controls, Studio's public
Nyx workflow and compiled/isolated consumers. Measure adapter update costs; full
virtualization remains with the original NS-3 performance owner. Original codegen/
Studio source UX and full component outcomes retain their original owners.

## Typed views and actual control integration — 2026-10-03

Criterion 2 is now accepted in full: the existing atomic edits/snapshot/token
evidence is joined by actual reusable-instance list bindings on browser and LCL,
with independent application/instance/default values. Ordered view observers,
last-owner release and unmount during real editor/selection callbacks have
checked/optimized native and executed-browser evidence. Criteria 1/3 remain
accepted. Criterion 4's runtime control/edit/identity/recovery and cost portions
are evaluated, but authored control bindings, automatic application scopes and
the public Studio workflow remain open. No task moves to DONE or receives full
completion credit.

Evidence: 149 existing collection cases plus 32 shared view cases; 1490 native
and executed-browser shared checks; 25 native / 26 browser actual collection
control checks, also executed in an exact 390-pixel host. Native lifetime runs
report zero unfreed blocks, including optimized real controls. Full native and
browser journeys retain scalar bindings, Studio authoring and compiled-source
behavior. All 38 intended wrong-type cases reject per compiler. Correctness-gated
512/4096-row mounted measurements assign remaining materialization/update costs
to [NS-3 performance](../NS-3_extension-performance_01.md). Exact corrections and
artifacts are in [WORK](../../WORK.md#typed-collection-views-and-real-controls--2026-10-03);
the public runtime contract is in [collection views](../../docs/collection-views.md).

Consecutive batches without full criterion closure: 0, because criterion 2 closes
here. Next integrated package closes criterion 4's authored boundary: typed
node-owned view specifications, composition/override inheritance, wire/source/
history, automatic application/instance mounting and Nyx-built Studio commands.
Exercise compiled/isolated consumers, navigation/default isolation and rejected
candidate recovery on both targets; propagate root disabled/read-only state to
row editors. This does not weaken or transfer any original acceptance criterion.
Production virtualization/depth and the original source UX/performance outcomes
remain with their existing owners.

## Authored bindings and Studio acceptance — 2026-10-03

Criterion 4 is accepted. Node-owned typed view specifications survive v3 wire,
composition/part clear/inheritance, source reconciliation and exact paired history.
Automatic application/instance controllers retain stores, selection and hidden-page
validators through navigation. Real browser/LCL controls enforce row edit and
selection policy, recover rejected commands and use independent reusable defaults.
Studio's Nyx-built Data/Bindings commands author the same public contract. Compiled
application, isolated page and reusable consumers reproduce those specifications.
Prior correctness-gated 512/4096-row adapter measurements remain applicable; their
materialization, virtualization and production budgets retain the original NS-3
owner rather than being credited as production performance here.

Evidence: 27 new shared authoring checks and 25 native / 26 executed-browser
automatic-control/Studio checks; six compiled reconstruction checks per target;
four Unicode packet/design/envelope/source boundaries; forty intended type
rejections per compiler; 147 HTTP checks, including saved job designs for all six
collection scopes. Checked native ownership frees all 22091995 allocated blocks.
The existing 1490 shared checks, native/browser journeys, 60-check exact-390 Studio
workflow and 24-check compact discovery/help workflow remain green. Exact logs and
limits are in [WORK](../../WORK.md#authored-collection-bindings-and-studio--2026-10-03).

The service's manual application clone omitted collection defaults. The v3 build
gate exposed that omission; it now uses the complete document clone, and the HTTP
harness compares saved job designs against the compiled companion's defaults and
bindings. Criterion 3 is revalidated with this stronger boundary evidence. Failed
build attempts are retained as corrections, not counted as acceptance.

All four original criteria are accepted and this task moves to DONE. Consecutive
batches without criterion closure: 0, because criterion 4 closes here. No original
criterion is weakened or transferred. The return path is original codegen/source
UX and measured large-document reconciliation, still open at its count of 6.
Full native Studio, production data controls, universal interaction/accessibility
parity and delivery quality remain with their existing task owners.
