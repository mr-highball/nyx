# NS-3_control-usefulness_04 — Inspect changing and large results without losing context

[North star NS-3](../MILESTONES.md#assessment) · [Task catalog](README.md) ·
[Usefulness review](DONE/NS-3_control-usefulness_01.md) · [Task flow](../TASKFLOW.MD)

**Description:**

A process/service debugger, log viewer or monitoring dashboard needs more than a
table that accepts rows. Make changing data comfortable to inspect: preserve the
user's place, offer explicit follow/pause behavior and keep the UI responsive
while updates arrive. The useful result is a public Nyx data-view policy and
demonstration, reusable by application authors.

North star: NS-3. Milestone: M3/components01 advanced data behavior; measurements
also feed the existing [extension/performance owner](NS-3_extension-performance_01.md).
Completion credit: pending assessment within those original outcomes, counted
once. This task does not accept full catalog performance, parity or delivery.

Starting evidence: [collection views](../docs/collection-views.md),
[native on-demand grid](../src/nyx.collections.lcl.grid.pas),
[selection snapshots](../src/nyx.collections.selection.pas),
[paging](../src/nyx.collections.paging.pas) and
[scheduler](../src/nyx.scheduler.pas). Native values are already read on demand
and browser tables already window rows; model/query/geometry work can still be
dataset-sized. Reuse these capabilities and measure the remaining costs rather
than labeling every current table unvirtualized or introducing another store.
This is optional data-view behavior; the debugger example does not imply every
application requires a streaming source or a new feed/state engine.

Ordered pull packets preserve every criterion below. Each needs separately
reviewed source/validation scope; L1 alone does not qualify scale or accept04.

| Packet | Useful finish line | Remains open afterward |
| --- | --- | --- |
| L1 | Stable live inspection, visible follow/pause/resume and feed states, independent views and lifecycle refusal | Measured scale/paint/input/memory and full task acceptance |
| L2 | Predeclared budgets measured on that same useful mounted journey at both targets/widths, with offscreen correctness and teardown | Any unmet original criterion and broader production performance/parity |

**Cross-family inspection checklist — Lists, Tables and Trees:**

The 2026-10-22 refinement keeps live/context behavior here, table column commands
in [02](DONE/NS-3_control-usefulness_02.md), presentation/copy in
[03](NS-3_control-usefulness_03.md), and coordinated List/Table search in the
existing [DataToolbar runtime](NS-3_data-toolbar_02.md). Tree find/reveal uses
the existing hierarchy/query owners under [components01](NS-3_components_01.md)
and this task; it does not expand DataToolbar's target families. This creates
no new search engine or reciprocal prerequisite on those tasks.

- Typeahead moves focus among displayed labels; it is not a membership filter.
  A public example and Studio/help must say which capability is active. Do not
  imply that the Table workbench also installs search controls on a List/Tree.
- Demonstrate finding a deeply nested process in a filtered Tree. Required
  ancestors provide navigable context; distinguish actual matches from retained
  context ancestors and disclosed rows, with clearly named counts and search
  scope. Inclusion of an ancestor does not itself expand it: reveal an admitted
  match's path deliberately. Clearing this surface's search retains surviving
  selected identities and deliberate disclosure changes; it must not restore an
  old expansion snapshot over later user input. Define focus/scroll recovery
  and explain intervening deletions. Reuse existing hierarchy/query and stable-
  selection owners; state unsupported combinations explicitly.
- Make hidden selection understandable: show a selected-but-not-visible count
  or equivalent discoverable explanation and an explicit reveal/clear action
  where supported. A reveal which requires relaxing a filter needs a deliberate
  choice; it must not silently alter the accepted query or select another row.
  Withdrawing one owner's search may still leave authored/peer filters hiding
  a selection; explain that state rather than promising an impossible reveal.
- Pause one of two views while services are inserted, updated or removed. Keep
  the paused view's inspection readable with a stale/pending indication; the
  peer remains live. Resume deliberately, honor bounded retention/overflow and
  explain a lost anchor or deleted edited row without overwriting its draft.

L1 and the existing family-specific owners deliver these interactions; L2
measures that same mounted workload. Existing lazy cell reads, row windows or
query snapshots alone do not establish the user-facing result or performance.

**Acceptance Criteria:**

- Expose typed opt-in update/follow policies with clear defaults and caller
  override. Users can pause displayed updates to inspect a record, see that the
  view is paused/stale and resume deliberately. Define queued/coalesced changes,
  bounded retention and overflow behavior. Pausing presentation must not silently
  cancel an application's data source or discard updates without its chosen policy.
- Preserve stable row/field selection, focus, scroll anchor and unfinished edits
  across inserts, deletes, sorting/filtering and refresh. Define what happens
  when an inspected or edited row disappears or becomes hidden, including an
  informative recovery path. Follow-newest is deliberate; unrelated updates
  must not repeatedly scroll users away from what they are reading.
- Loading, refreshing, empty, filtered-empty, failed and paused states have
  distinct visible and accessible presentations with relevant retry/reset/resume
  actions. Application authors can compose or replace those parts using Nyx
  controls and resource bindings. Event payloads identify update origin, revision
  and affected stable identities where meaningful.
- Background producers submit immutable data through Nyx's existing scheduler/
  collection publication rules. UI/collection ownership remains safe; batching,
  cancellation, stale completions and overload have documented typed policies.
  Replace/unmount/dispose revokes former authority before cleanup. A live feed
  must not block input, publish into a retired view or disturb a peer instance.
- State exact supported scale and reproducible input/update/paint/memory budgets
  before qualification. Measure mounted, visible native and browser workloads
  with wide/compact geometry, active filters and interactive inspection. Separate
  provider/model/query/geometry costs from painted cells and draft caches; a low
  read count or hidden-control timing is not a full responsiveness result.
- Public specialized interfaces, Studio properties/events/help, portable saved
  defaults, generated Pascal/source reading and revision-aware semantic tooling
  share the policy. Runtime pause/focus/preferences do not create design history
  unless explicitly authored. Existing list/tree/table selection and paging
  semantics remain coherent; document which control families support each option.
- Demonstrate a deterministic replayable process/service-style feed through the
  public API, with two independent views, user pause/resume, row churn, errors,
  filter/sort changes, queued updates and lifecycle refusal. Execute actual native
  and browser input and build the demonstration's own saved/generated application.
  Retain failed/budget-exceeded outcomes, clean native heaps and target limits;
  do not substitute backend-only assertions for the user's inspection experience.

**Blockers**

- [Control usefulness review](DONE/NS-3_control-usefulness_01.md) supplies the ranked
  operational-data gaps and concrete acceptance workload.
- [Observable collections](DONE/NS-1_state-collections_01.md) supply authoritative
  data, revision and selection publication. Existing scheduler/query/windowing
  owners remain the implementation foundation.
- [Equal-value row replacement](NS-1_state-collections_02.md) closes a reproduced
  no-op folding gap that otherwise transfers an old draft to a replacement ID.

L1 now includes a data-only unfinished-draft recovery journal, native Table
capture and public Nyx recovery review. Handwritten/generated native applications
each pass 24 focused recovery checks per width (1280/390), alongside the existing
inspection journey. Changed values, same-ID replacement and hidden rows preserve
text before closing the editor; confirmed discard affects only its exact entry.
Advisory journal warnings do not supply a hard memory bound. Browser behavior,
native Tree inline editors, remaining feed presentations, saved policy/
Studio/MCP adoption and all original criteria remain open. No DONE or completion
credit is assigned from these bounded checks. See WORK for final source/evidence
cohorts and retained failed qualification attempts.

Deliberate flat-view following now has typed opt-in defaults and a specialized
runtime interface, public Follow/Keep buttons, independent selection/peer intent
and acknowledgement only after successful physical refresh. Native List/plain-
grid and handwritten/generated arranged-Table journeys exercise tail visibility;
model checks cover empty results, pause refusal, query/source distinctions and
failure/retry. This advances the follow portion above; remaining feed-state
presentations, Tree, memory/scale, browser execution and saved/Studio/source/MCP
adoption remain open under the unchanged original criteria.
