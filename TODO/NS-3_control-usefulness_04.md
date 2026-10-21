# NS-3_control-usefulness_04 — Inspect changing and large results without losing context

[North star NS-3](../MILESTONES.md#assessment) · [Task catalog](README.md) ·
[Usefulness review](NS-3_control-usefulness_01.md) · [Task flow](../TASKFLOW.MD)

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

- [Control usefulness review](NS-3_control-usefulness_01.md) supplies the ranked
  operational-data gaps and concrete acceptance workload.
- [Observable collections](DONE/NS-1_state-collections_01.md) supply authoritative
  data, revision and selection publication. Existing scheduler/query/windowing
  owners remain the implementation foundation.
