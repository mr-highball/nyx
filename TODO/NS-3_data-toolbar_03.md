# NS-3_data-toolbar_03 — Make active and retained filters understandable

[North stars](../MILESTONES.md) · [Task catalog](README.md) · [Task flow](../TASKFLOW.MD)

**Description:**

Give a person using a DataToolbar a clear view of all filters/order affecting
the target, including peer and retained contributions after page navigation.
Provide ordinary Nyx controls for inspecting and explicitly withdrawing those
contributions. Reuse the canonical query session and existing query-workbench
forms/commands; do not create a second query engine or infer ownership from a
diagnostic origin string.

North star: NS-3 components criteria1/3, consumed by DataToolbar authoring01 and
Studio authoring. No separate completion credit. Runtime02 remains the owner of
Search/Sort/local Clear, exact actor authority and grouped scalar/query changes.

The real reusable App journey in runtime02 establishes the gap: navigating away
revokes the old toolbar actor while its accepted predicate remains an explicit
orphan. Returning creates a fresh actor. Local Clear correctly cannot claim that
previous actor's filter. The current fixture removes it through an explicit
programmatic session reset; an ordinary user needs visible query context and a
direct control for this operation. This is a UX gap, not permission to silently
steal ownership, restore an old baseline or erase a peer's current filters.

Independent implementation can proceed while full runtime/coordinator/parity
acceptance remains open. These prerequisites govern acceptance, not source access.

**Acceptance Criteria:**

- A configured toolbar exposes bounded, useful query status with current matched
  counts, local/peer/retained predicates and shared sort ownership. English defaults
  explain intent without treating registration IDs as user-facing names. Typed
  options allow status/detail presentation to be customized or hidden; the static
  four-part recipe and original specialized interface/GUID remain unchanged.
- A visible ordinary control opens or reveals a reusable Nyx-built query detail
  surface for List and Table targets. It consumes copied session observations and
  existing query-workbench operations. A person can withdraw one exact contribution,
  reset ordering or deliberately clear all user query contributions. Local Clear
  and global reset have visibly distinct scope. No executable actor token enters
  saved designs, source generation or descriptive MCP responses.
- Scalar presentation and query metadata stay truthful after peer/global edits.
  A removed contribution must not keep looking active merely because an input
  still has its former text. Reset only fields for which the caller has actual
  current authority; preserve independent binding/read-only contracts and report
  conflicts rather than fabricating a successful reset. Reuse existing prepared
  scalar/state/query publication and scheduler ownership.
- In two ordinary Apps with reusable instances, type a filter, navigate away/back,
  recognize the retained filter and remove it through actual query controls.
  Preserve other instances, peers, source rows, defaults and hidden selection.
  Fresh actors never adopt an old token by name. Stale detail forms/commands,
  failed page candidates and disposed views cannot change a successor.
- Preserve ordered caller callbacks and their execution policies. Refusal leaves
  the accepted models unchanged; notification failure remains committed. Validate
  native wide/compact input with clean heaps and matching browser assertions;
  qualify browser interaction separately from compilation. Exercise exact
  Save/Open/generated adoption with authoring01 before its acceptance.

**Blockers**

- [DataToolbar runtime](NS-3_data-toolbar_02.md) supplies the live scoped input,
  collection target, session actor and common coordinator lifetime.
- [Accepted collection filtering](DONE/NS-3_control-usefulness_02.md) supplies
  canonical contributions and existing query-workbench operations.
- [Prepared coordinators](NS-3_coordinators_01.md) retains its full original
  target/parity acceptance; no new component-specific renderer lifetime follows.
