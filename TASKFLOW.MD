# Task flow

[Athena standard](athena/docs/task-flow.md) · [Milestones](MILESTONES.md) ·
[Open tasks](TODO/README.md) · [Current work](WORK.md)

## Governing standard

Apply the linked Athena task-flow standard for task identity, prerequisite
traversal, scope-preserving splits, progress checkpoints, acceptance, evidence,
and TODO-to-DONE moves.

## Local records

- Canonical milestone file: [MILESTONES.md](MILESTONES.md).
- Open task catalog: [TODO/README.md](TODO/README.md).
- Accepted task catalog: [TODO/DONE/README.md](TODO/DONE/README.md).
- Current batch and evidence: [WORK.md](WORK.md).
- Project decisions and invariants: [PROJECT.md](PROJECT.md).
- Architecture boundaries: [docs/architecture.md](docs/architecture.md).

## Project decisions

- Priority is the first end-to-end vertical slice, followed by semantic parity,
  breadth, Studio depth, and release quality.
- Planning does not create an approval boundary; continue through authorized
  implementation tasks and their prerequisites.
- No completion percentage or task credit is assigned before the baseline and
  acceptance evidence are assessed. `Pending assessment` is intentional and
  earns no completion credit.
- No minimum milestone contribution has been specified.
- [WORK.md](WORK.md) owns the current criterion, deliverable, stop condition,
  and consecutive batches without criterion closure.
- Newly discovered required work receives the next unused task identity and is
  linked before returning to the current task.

## Parallel work and handoffs

While the goal is active, Lil' Scooty keeps one scoped assignment and identifies
the next unmet prerequisite from the canonical task catalog. A source-ready
handoff does not empty that queue. Report the owning criterion, deliverable,
changed files, remaining checks and next candidate to root; continue independent
read-only review of that prerequisite while implementation ownership or
qualification is pending.

Root coordinates separate file ownership and admits all compiler/readers. Both
agents pause Pascal edits while any admitted reader is live, and resume only
after root has joined every reader and released the next scope. During a freeze,
use useful read-only investigation or separately owned documentation work. Pull
from existing tasks and their prerequisites; do not manufacture busywork,
recurring cleanup or new acceptance credit to keep an agent occupied. A concrete
external blocker is reported with its unblock condition and return task.

At each handoff, keep the helper queue current with the active deliverable, its
owned files, the next eligible prerequisite and the action available during
review. Root replenishes that queue before releasing the current assignment;
another user request is not needed. Lil' Scooty may pull independent read-only
work immediately and reports the next concrete scope for ownership coordination
before editing. If no useful authorized work is eligible, report the exact
blocker instead of silently waiting or inventing work. A task's source-ready
state remains separate from acceptance and DONE.

