# NS-3_control-usefulness_02 — Let table users filter and sort columns

[North star NS-3](../MILESTONES.md#assessment) · [Task catalog](README.md) ·
[Usefulness review](DONE/NS-3_control-usefulness_01.md) · [Task flow](../TASKFLOW.MD)

**Description:**

Give users of a Nyx table visible, understandable column filtering and sorting
without requiring every application to construct its own grid query editor.
The external debugger report motivates this outcome; the external application's
source has not been inspected and is not evidence of a particular Nyx defect.

North star: NS-3. Milestone: M3/components01 advanced data-control behavior.
Completion credit: pending assessment within original component scope; no
duplicate credit for existing query, state, DataToolbar or parity work.

Starting evidence: [typed queries](../docs/collection-queries.md),
[collection views](../docs/collection-views.md),
[query owner](../src/nyx.collections.query.pas) and
[live DataToolbar](NS-3_data-toolbar_02.md). Existing filtering/stable ordering
are foundations. The missing result to review and deliver is the table user's
discoverable column interaction and its portable, authored configuration.
The existing public `NewNyxQueryEditor` is available for advanced predicate/sort
editing; reuse it instead of introducing another parser/editor for the same model.
At the initial review the native adapter installed no header query handlers;
browser headings were plain `th` text. These are baseline source observations,
not an execution verdict for
the external application.
DataToolbar and FilterBar integration must reuse this query policy rather than
introducing another store, query evaluator or component-specific lifecycle.

Ordered implementation packets preserve all seven criteria below:

| Packet | Useful result | Still required afterward |
| --- | --- | --- |
| P1 — Prepared query publication | Optional opaque intent capture and query preparation on the existing view; real state/two-view group is all-or-refused and observers see all committed models | Canonical contributions, visible controls, authoring and full02 |
| P2 — Shared query contributions | One view-owned explicitly admitted baseline (CurrentQuery by default; authored via explicit rebase), independent user predicates/shared sort, exact leases and explicit owner/global Clear with ABA/lifetime guards | Header/form controls and complete target/source adoption |
| P3 — Useful table commands | Real native/browser header/keyboard filtering/sort, active chips/counts/empty recovery and coherent peer surface drafts | Ordinary Studio/history/save/generated integration and full qualification |
| P4 — Authored workbench | Typed policy/event/help/source/persistence/semantic agreement and own saved/generated two-view workbench at both targets/widths | Any unmet original criterion, full parity/quality and delivery |

P1 has a separately reviewed local qualification recorded in
[WORK](../WORK.md#prepared-collection-query-publication--2026-10-21): root owns
the existing view/prepared integration/build; helper owns only the independent
new contract and public focused consumer. Native89 public checks, unchanged
query56 and its own compiled-query57 pass with clean heaps. Matching browser
consumers and both Studio mains compile only; seven compilers have zero owned
warnings. All readers are joined/audited with reviewed source unchanged. No
header/session/renderer/Studio/Q1 source scope follows. Equal ConfigureQuery
remains a true no-op; changed A→B→A intent invalidates old captures. Source updates
before Prepare are valid; a preparation reserves the current atomic source/view
and refuses a simultaneous same-source row-edit transaction. This is a documented
API boundary, not a claim that the full table interaction exists.
All seven acceptance criteria remain open; P1 adds no duplicate product credit.
Next is the single view-owned P2 contribution/session result, not another P1
fixture/compiler variant. Default baseline admission preserves the accepted
CurrentQuery; a second surface joins existing canonical contributions instead of
recapturing their effective query. Explicit authored restoration is a prepared
rebase, not automatic behavior when acquiring/releasing an owner lease.

P2 now has a separately bounded native-local API qualification in
[WORK](../WORK.md#shared-collection-query-sessions--2026-10-21), with the matching
[Q1 ownership rule](NS-3_data-toolbar_02.md) specified before implementation.
One optional session on the existing view owns baseline/independent predicates/
shared sort. Typed fluent batches, revocable leases, copied observations and
checked metadata transfer use the existing prepared-query participant. Root owns
view/bindings/build/docs; the existing helper owns the new contract/public consumer.
Native93 new public checks plus unchanged prepared-query89/query56 pass with
clean heaps. Final8 compilers have zero owned warnings; browser assertions and
both Studio mains compile only. All actual readers are joined and exact audited,
including retained first identifier failure and superseded enum warnings. No
header/renderer/Studio/Q1 implementation or browser execution is released by P2.
All original criteria remain open; the next outcome is P3's actual useful table
commands and shared form, not another model/compiler variant.

P3 has a separate source release in [WORK](../WORK.md#table-query-workbench--2026-10-21):
root owns saved typed policy/source parity and target mount/presenter integration;
the existing helper owns the shared form/controller and ordinary live-input
consumer. Existing sessions, query parsing, events, popovers and live input
bindings remain the owners. Independently reviewed/frozen source now passes782
actual native checks at each1280/390 with clean heaps. Matching browser assertions
and both Studio mains compile; final10 compiler composite has zero owned warnings.
Own matching document/Pascal exports prove codec/source only. All readers are
joined and audited, including retained compiler/input failures and an additive
structural-focus assertion. Actual browser input, full Studio/events/semantic
authoring, history/Save/Open and compiled own companion stay required; none of
the seven original criteria below closes from this native-local P3 result.

P4's remaining work is ordered below. These are finish lines within the original
criteria6–7, not new completion credit or permission to change source/run readers.

P4A now has a separately reviewed source release in
[WORK](../WORK.md#table-query-policy-authoring--2026-10-21). It implements ordinary
policy forms, typed paired intent/admission and preservation only. Native actual
Inspector/history checks and matching browser/product compilation must qualify
that result before any local claim. P4B/C and all original acceptance stay open;
this source release supplies no browser-host authority or new product credit.

P4A's first native journey is still unqualified. The reviewed diagnostic compiles
and reaches144 actual1280 checks, including table/text-column Apply and paired
Undo/Redo, before the fixed300s guard interrupts during later form work. It is
not an overall pass or clean-teardown result. Queue126/native clean and browser
compilation retain compatibility. [WORK](../WORK.md#table-query-policy-authoring--2026-10-21)
owns the exact joined audit, retained failures and repeated18-24s refresh/47s Apply
observations. The following existing-profile observation retains the same source;
do not relax internal waits, remove checks or treat a smaller run as full P4A.
All seven criteria remain unchanged and open.

The next bounded observation uses the existing native phase/layout profile hooks
on the unchanged consumer at1280 with the same300s guard. Only opt-in build/tool
plumbing and future process-receipt identity changed. That observation is now
terminal:144/stage2 interrupted again, one compiler with zero warnings. Corrected
metadata retains13 complete paints and one incomplete; repeated shell rendering
dominates. [WORK](../WORK.md#table-query-policy-authoring--2026-10-21) retains the
first parser error/addendum, nested timing limits and exact source boundaries.
Stop further profile variants. Next review a whole-executable budget covering
the complete unchanged11-stage journey using the existing explicit timeout option,
while its default300s,60s Ready deadline,2400 global Drive-poll bound and all
predicates stay unchanged. This is functional qualification, not release-performance acceptance.
Independent workload review supports a fixed1800s PER WIDTH outer allocation:
the unchanged fixture contains10 mutations and20 Undo/Redo actions plus other
input. Compact duration is still unproven; an internal60s failure must still fail.
No run or speculative production repair is released by this note. Full P4A remains
the original authored journey; core no-closure33 carries without credit/DONE.

The complete P4A functional packet now has a separate tool-only release in
[WORK](../WORK.md#table-query-policy-authoring--2026-10-21): existing explicit
1800s PER WIDTH, all Pascal and original operation/poll/assertion bounds exact.
Independent review/freeze precedes native1280/390 plus matching browser compile;
no new reader or qualification is asserted by the release alone. First failure
stops that packet before any repair. P4B/C and all seven criteria remain required.

That first complete-packet attempt is now terminal and independently reviewed.
One native compiler has zero warnings; actual1280 reaches stage1/check133, then
fails the ORIGINAL Ready60s deadline during text-column Apply after the paired
update. It exits1 itself before the1800s outer cap.390 and matching new-consumer
browser compilation remain unattempted. Equal allocation/free totals have only
a truncated heap footer, not a complete clean-heap receipt. Immutable audit
24732435 and [WORK](../WORK.md#table-query-policy-authoring--2026-10-21) retain all
inputs/failures. Pending optimistic policy reconstruction invalidates the exact
editor baseline before commit; keeping accepted presentation plus existing locks
is a read-only repair candidate, not yet a sufficient timing diagnosis. No source
or rerun is released. Preserve consumerEE1CEAA1, all fixtures, waits and predicates;
P4A/B/C and every original criterion stay open. Core33 advances34 without credit.

The next separately released repair keeps accepted query-policy presentation
during all four pending policy actions, preserving exact locks/capture/baseline
admission and draft ownership. Retained profile evidence shows pending and
committed Inspector replacements around the same text-column Apply. Existing
pending-descriptor checks are extended additively; full consumerEE1CEAA1 remains
exact. Source/tool review and freeze precede pending native/browser, both Studio
compilers and the unchanged full1280/390 journey plus browser compile. No Ready/
poll/fixture relaxation or new profiler is admitted. This is a repair candidate;
full P4A and all original criteria remain unqualified until actual results.

The first focused pending native reader is retained as a fixture failure: an
invalid empty operator draft raises the documented `EArgumentException`, while
the new test catches only `ENyxCollection`. The first valid case completes and
the heap is clean. One new fixture case warning is also retained. Root's narrow
catch/fallback repair needs fresh review/freeze before the same fixed packet;
productionDA08/fullEE1C and every predicate/wait remain exact. No full UI/browser
result or task acceptance follows. WORK records joined audit62379BAB and core35.

The repaired packet is now terminal/root joined, audited95B25BDD with13owned/
1807Pascal/foreign13/extra8 exact. Pending180 has a complete clean heap; unchanged
full actual1280/390 pass266/267 with all11 stages and original waits/assertions.
Both full stderr heap footers truncate at256bytes, so native-local P4A remains
unqualified. Five accepted compilers have zero owned/14 upstream warning
occurrences. Matching full browser compilation fails1150 at `@Closed`, which
resolves the imported browser enum instead of the fixture callback; keep that
failure and its separate seven upstream warnings. Core35 advances36 without
original closure/credit/DONE. WORK owns the next separate callback-name repair/
single-browser-compiler scope and subsequent unchanged-executable heap capture.
No new source/tool/runtime is released by this result; no browser execution is
asserted. P4B/C and all seven criteria stay required.

| Packet | User-visible result and evidence | Preserve / remains required |
| --- | --- | --- |
| P4A — Author query controls | Ordinary Inspector configures typed table/column filtering, sorting, allowed comparisons and text comparison; explicit Use Nyx defaults is distinct from Restore inherited binding. One paired candidate/history step per admitted form intent, including local reusable overrides and stale/pending-draft refusal | Every unrelated column/scope/selection/sizing edit preserves existing declared policies; absent choices remain absent; runtime filter contributions are not design defaults |
| P4B — Discover and operate semantically | Shared event/help discovery exposes exact Before/Changed query names and copied Details, bounded binding reads expose requested declared/effective policies, existing revision-aware changes accept strict v7 and the same typed intents | Before is sequential/cancellable, Changed follows normal policy; Inspector and semantic policy choices must agree with admission. No new server or runtime rollout is implied |
| P4C — Prove the authored workbench | Actual Studio two-instance configuration, paired Undo/Redo, real Save/Open and that journey's OWN strictly admitted emitted Pascal, independently compiled/executed at both widths; actual browser input under an applicable host override | Preserve definition/sibling/source/default isolation, callbacks, query/paging/hidden selection/draft/lifetime behavior. P3 authored8441/A861 exports cannot substitute for this new saved/generated pair |

The historical pre-P4A readiness review found a concrete prerequisite:
`ApplyNyxStudioCollectionViewIntent` reconstructed views for title/mode/scope/
selection/parent/column/sizing edits without copying declared query policies.
P4A now preserves table policy only when `HasTableQueryPolicy`, and each surviving
column policy only when `HasQueryPolicy`, matching field identity and family. Copying
resolved defaults unconditionally would corrupt absence/version compatibility.
Adding/removing a column must preserve every survivor; a new column starts with
absent policy. Keep deliberate fresh-bind and last-column-clear behavior.
Copy-based query/paging edits already preserve these choices.

The proposed ordinary intent seam appends typed setter/default actions to the
existing collection intent and reuses its exact schema/full effective-spec
baseline and `sdaCollection` queue. Policy payloads belong only to their actions;
default actions clear only the selected raw choice. Preserve former intent wire
shapes/ordinals and reject pending Pascal before candidate mutation. Studio,
semantic changes and preview must use this one admission path. Generated built-in
query callback references should use the existing enum-valued factory, with
matching uses/source-reader support, rather than a behavioral string literal.

The reviewed typed seam appends table/column setter and default actions, with
separate immutable table/column policy payloads. Setters require a defined
policy; default actions require no policy payload. Column actions retain the
schema-qualified field, all four are table-only structural proposals, and the
exact full-spec baseline is required even for restoring a default. Whole-form
Apply emits one intent; option changes remain draft. Existing intent ordinals
and former eleven/thirteen-member encodings remain unchanged. Library defaults
and whole-binding Inherit are deliberately different; no per-policy ancestor
merge is implied by clearing one policy in a local whole-spec override.

Event policy discovery also needs a shared typed restriction: Before-query must
offer/admit sequential execution only, including a declared empty callback
stream. An additive metadata capability should default old creator records to
all four policies, using the existing managed-marker compatibility pattern.
Preserve/intersect matching restrictions; do not widen a built-in restriction
when merging creator metadata. Inspector choices/capture, bounded agent event
reads and effective realized candidate admission must use that one capability.
Do not hide validation inside route-only processing, skip empty streams or grant
built-in custom Emit authority. Changed and unrestricted legacy events retain
their existing choices; raw descriptor shape admission remains separate.
A disjoint creator/built-in policy intersection is an effective-owner error:
diagnose it rather than treating the empty set as legacy unrestricted metadata
or selecting a fallback. Scheduler support is a distinct check and cannot supply
event-policy admission for a declared stream with zero callbacks/routes.

A separate previous-spec rebuild in the Kanban editor has the same preservation
gap and is tracked in [usefulness05](NS-3_control-usefulness_05.md). It adds no
hard dependency or implicit P4 source ownership; no new Kanban filter UI follows.
The P3 browser artifacts and existing loopback host are prepared/verified. A
specific user override for actual browser tests and same-regression reruns is
pending; that request is not authorization and no host/browser has been started.

**Acceptance Criteria:**

- Expose immutable typed fluent per-column capabilities through specialized
  table/binding interfaces: allowed filter operators, sortability, comparison
  policy and initial/default behavior. Field identities and values are
  schema-checked. Text, numeric and Boolean columns offer appropriate operations;
  optional/empty values and any additional admitted scalar types have documented
  semantics. No behavioral magic strings or widget downcasts are required.
- A user can open an accessible column filter affordance, see/edit its current
  value, apply it, clear one column and clear all user filters. Active filters
  and filtered/total result counts are visible; no results has a useful recovery
  action. Keyboard operation and a usable compact/pointer presentation exist on
  actual native/LCL and browser controls. Authors can opt out per column or table.
- Header interaction exposes ascending/descending/default order and a visible
  current sort indicator. Explicit typed policy admits single or multiple sort
  keys; the user can inspect precedence and reset it. Equal values retain stable
  deterministic ordering and type-correct comparisons. Filtering and sorting
  operate on the intended result before paging, not just the painted viewport.
- Compose user predicates with the admitted authored query. Define coexistence
  with DataToolbar/FilterBar/programmatic query replacement and version ownership.
  Before implementation release, settle the existing DataToolbar Q1 same-target/
  query-authority rule with that owner. This is the shared integration contract,
  not a new hard dependency on all DataToolbar02 acceptance or another evaluator.
  Clear restores the current valid authored baseline, never an obsolete snapshot
  or another owner's query. Schema loss, invalid drafts, stale/reentrant input
  and cancelled work refuse without partial publication or lost editor text.
- Preserve source data, stable row identity, hidden selection, current-cell edit
  rules and independent reusable/view state. Emit meaningful typed before/after
  query/sort/filter events with old/new policy and origin where applicable;
  cancellable stages precede publication. Existing ordered multi-registration
  callbacks and scheduler/UI ownership remain effective.
- Studio exposes supported column properties/events and explanatory help. Saved
  authoring policy, local reusable overrides, paired Undo/Redo, crafted generated
  Pascal and its source reader agree. Distinguish portable defaults from optional
  user-session preferences. Existing semantic operations expose the same intent
  in bounded revision-aware reads/mutations, without installing a new tool server.
- Demonstrate the public contract in a Nyx data workbench on actual native and
  browser targets: two independent tables, combined filters, typed comparisons,
  multiple sort keys, empty results, reset, paging, hidden selection, active drafts,
  replacement and stale commands. Independently build that journey's own emitted
  Pascal, retain refusals and clean native heaps, and publish target limitations.
  A metadata-only test or application-specific filter textbox is insufficient.

**Blockers**

- [Control usefulness review](DONE/NS-3_control-usefulness_01.md) supplies the reviewed
  interaction gaps, default policy and concrete target/source boundary.
- [Observable collections](DONE/NS-1_state-collections_01.md) supply the accepted
  store, typed schema and selection/query publication foundation.
