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

