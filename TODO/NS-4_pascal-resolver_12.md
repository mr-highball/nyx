# NS-4_pascal-resolver_12 - Qualify and deliver the owned editing engine

[North stars](../MILESTONES.md) · [Task catalog](README.md) · [Task flow](../TASKFLOW.MD)

**Description:**

Deferred on 2026-10-21. Accept the complete owned-resolver portion of
[Studio authoring](NS-4_studio-authoring_01.md) through actual end-user evidence.
North-star owner: NS-4; this is the aggregate acceptance gate, not new credit or
a replacement for [delivery](NS-6_delivery_01.md) and
[independent adoption](NS-6_adoption_01.md).

Starting evidence and retained failures are in the
[parking checkpoint](../WORK.md#active-priority--back-to-core-nyx-2026-10-20).
Large focused test counts and compiled browser consumers do not establish this gate.

**Acceptance Criteria:**

- Maintain a finite language/capability matrix with actual real-source witnesses,
  unsupported cases and exact target/compiler identities; assess completeness
  against outcomes rather than test counts or time spent.
- Run real multi-unit Nyx/library projects and unfinished-buffer editing through
  the ordinary source editor, including completion, definition/return, source
  edits, reference finding, reviewed rename, context changes, recovery and cancellation.
- Execute native and browser input/accessibility checks, Unicode/IME scenarios,
  wide/compact views and representative production work/latency/memory workloads.
- Verify clean builds, owned warning debt, lifetime isolation, no execution of
  inspected user code, and deterministic result/source provenance.
- Deliver documented supported setup and examples through existing packaging and
  preserving rollout owners; independently reproduce the core authoring journey.
- Update the parent acceptance assessment only for evidenced outcomes, retaining
  all failed/partial results and unresolved original criteria.

**Blockers**

- [Real library closure](NS-4_pascal-resolver_02.md),
  [completion adoption](NS-4_pascal-resolver_09.md) and
  [definition journeys](NS-4_pascal-resolver_10.md), including their prerequisites.
- [Reviewed semantic rename](NS-4_pascal-resolver_14.md), including exact
  reference discovery and refusal of unsafe partial edits.
- External: actual target input/observation and authorized delivery environment.
