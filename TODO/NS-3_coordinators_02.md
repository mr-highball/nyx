# NS-3_coordinators_02 — Register a caller-owned prepared behavior

[North stars](../MILESTONES.md) · [Task catalog](README.md) · [Task flow](../TASKFLOW.MD)

**Description:**

Deliver the usable extension API behind
[common component coordinators](NS-3_coordinators_01.md): an application creator
registers one behavior and mounts it through existing prepared view lifetimes
without changing a renderer for that behavior. The parent retains migration,
reuse and full integration acceptance.

North star: NS-3, original components criteria1/3. Credit stays with the parent
outcome; this split earns none. Existing Kanban/MasterDetail prepared mounts,
publication readiness and event generations are the starting evidence. They
are not evidence that third-party registration already works.

The first useful creator example is a Selection Summary: an existing bound
multiple-selection List, retained-selection-count Label and accessible Clear
Button. Count includes query-hidden retained members and is labeled accordingly;
Clear uses that same view's selection owner. Two independent instances must
preserve rows, query and authored defaults. This example serves the criteria
below; it creates no new component kind or selection engine.

Creator factories are trusted Pascal code, not a security sandbox. Their public
contract should distinguish model lifetime from active mount authority: built-in
control interfaces retain nodes, not weak parents/document/widgets or permission
to project into retired views. A scoped role projection privately retains the
exact candidate roles; copied discovery and guarded text/availability plus weak
refresh suffice for this example. No borrowed Node/GetNode/controller is offered
as a supposedly revocable capability. Current read-only design is not an accepted
implementation; root releases separate source scopes after the Interact checkpoint.

**Acceptance Criteria:**

- A public managed registration contract has explicit application/renderer
  ownership, typed kind/intent identities and deterministic duplicate/conflict
  handling. Future candidates capture immutable registry snapshots; replacement
  cannot rewrite an already accepted view.
- A factory receives checked realized roles and scoped, revocable capabilities
  through the existing preparation path. It cannot retain borrowed widgets,
  document or controller. Saved metadata never loads executable factories or
  grants file, HTTP, compiler or other host authority.
- One independently authored useful behavior mounts through the common path
  in an ordinary application. Its initial presentation is ready before Build,
  its observations remain dormant before commit and its input uses the same
  accepted event generation. No behavior-specific renderer branch is needed.
- Refusal, abandoned preparation, final-fence cancellation, reentrant validation,
  replacement and postcommit notification failure obey existing publication
  semantics. Registration and runtime disposal retire every owned resource.
- Publish a documented creator example and execute native public-API consumers
  with clean heaps; compile matching browser consumers without omitting their
  assertions. Preserve old manual APIs and GUIDs. Full migration and reusable/
  embedded adoption stay open in the parent task.

**Blockers**

- [Observable state and collections](DONE/NS-1_state-collections_01.md) supply
  existing data authority. Existing publication/event-view owners supply lifetime;
  this task creates no second state, selection, event or history engine.
