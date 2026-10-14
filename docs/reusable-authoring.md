# Named reusable copies

[Components](components.md) · [Inspector groups](inspector-groups.md) ·
[Public Nyx form](../src/nyx.reusable.editor.pas)

Select an authored control and open Properties → Component options → Create a
reusable copy. Enter the definition name, then choose Preview names or Create
reusable copy. The original stays in place. The copy appears among the project's
reusable components; open it there to edit or insert independent instances.
The existing Make reusable shortcut still chooses automatic names and opens
the new definition immediately.

The naming form is an ordinary Nyx Card composed from specialized Input, Label,
Button and preformatted Code interfaces. Its named parts are `name`, `preview`, `create` and
`summary`. Studio consumes this public component on browser and LCL.

```pascal
LProposal := NyxReusableProposal(LDocument,
  NyxControl('reply'), NyxComponent('note'));

LNameForm := NewNyxReusableEditor('reusable-names', LDocument,
  NyxControl('reply'));
LPage.Add(LNameForm);
```

The document and source are borrowed only while constructing the form or
proposal. The proposal retains typed references, an exact baseline and an
independently copied identity map, with no tree, store or renderer ownership.
Its `Identities` property returns a detached array on both compilers. Use the
existing typed `NyxDeriveComponent` operation to apply that complete map through
the owning application's admission and history contract.

The definition name remains exact, including Unicode. Occupied names refuse.
Descendants combine a bounded definition prefix with their existing purpose
and use numeric suffixes to avoid every occupied page/component identity.
For example, source `title` becomes `note-title`; an existing `note-title`
makes the new identity `note-title-2`. Scalar-boundary clipping keeps long names
within the portable identity limit. The complete detached clone is validated
before a proposal returns; named parts, contracts and bindings retain ordinary
derivation semantics. Application defaults stay owned by the document.

Preview displays the definition, complete descendant count and at most eight
source/destination pairs on separate lines in a bounded scrollable face.
It changes presentation only and remains available
with unfinished Pascal. Creation submits one existing typed project transaction
to the ordinary source worker. A successful command publishes design and Pascal
together; one Undo/Redo restores that exact pair. Pending Pascal requires explicit
resolution before creation. Successful named creation keeps the original selected.

The form captures the exact source subtree and occupied identities. Selection,
subtree or identity changes require reopening it; queued commands also retain
the ordinary full-pair/schema guards. Same-named buttons borrowed from a different
form refuse. Name drafts survive collapse/reopen and matching reconstruction;
an absent form parks its copied draft, while a changed context retires it.
These transient drafts do not become application state or cross-launch preferences.

Collapsing a browser disclosure from a control command can conceal its focused
input before that command returns. The adapter schedules the necessary focus
notification through Nyx's UI queue without weakening the binding coordinator's
reentry guard. The work port borrows its renderer, owns copied identities and is
revoked on mount retirement. Reopening, replacement or caller focus redirects
take precedence. Direct store-driven disclosure changes retain synchronous focus
recovery. Native retains its existing adapter behavior.

`tools/build.ps1 -Target reusables` includes shared form/proposal checks. Optional
`-ReusableSourceDirectory <exact MCP workshop export>` also compiles/runs the
ordinary native controller and stages `reusable-studio.html` with its matching
source-worker module. That fixture expects the English `home`/`reply` workshop,
its nested `title`, `body`, `actions/send` parts and an occupied `note-title`.
`-ReusableEvidenceDirectory <new directory>` preserves earlier packets.
Serve the staged browser over an admitted HTTP host and observe desktop and narrow
viewports; no build command starts a listener or changes an enrolled user project.

Current-source control/worker evidence is recorded in WORK.md. The observing
frontend retains its previous release until separately qualified delivery.
Compiler-backed handwritten derivation, editable per-descendant names, larger
project budgets, phone hardware, assistive technology and complete reusable/editor
parity remain under the original component, authoring and workflow criteria.
