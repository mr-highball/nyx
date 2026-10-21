# Named reusable copies

[Components](components.md) · [Inspector groups](inspector-groups.md) ·
[Public Nyx form](../src/nyx.reusable.editor.pas)

## Instance parts

Select a reusable instance to open its Parts editor. **Hide part** removes a
named piece from this instance's realized content. The definition and other
instances keep their original content. Hidden rows stay in the editor with
**Restore original**, which removes the local override and inherits the current
definition again. Restore means inheritance, rather than recovery of an earlier
local customization. Undo restores the exact prior design and Pascal together.

Nested paths such as `actions/send` can be hidden independently. The instance
root stays intact. A part with customized properties or its own override payload
cannot be silently converted to a removal; the editor explains why Hide is
unavailable. Dependent child overrides retain ordinary
full-document validation. Reopen the editor after its instance, definition or
captured project baseline changes.

The public [Parts form](../src/nyx.parts.editor.pas) is composed entirely from
Nyx controls and returns copied typed intent. It retains no document, renderer,
Studio session or compiler. A custom host can inspect that intent and submit it
through its own admission policy; Studio uses the existing shared source-command
queue for both targets. Pending Pascal must be explicitly resolved before an
instance part operation can publish.

```pascal
LParts := NewNyxPartsEditor('note-parts', LDocument,
  NyxControl('first-note'));
LPage.Add(LParts);
```

**Edit part properties** opens the existing customization choices, including the
instance root. This disclosure keeps the structural Parts actions easy to reach.
Saved design, generated Pascal and Undo/Redo share the same existing remove/
inherit descriptor contract; this editor introduces no parallel layout model.

`tools/build.ps1 -Target reusables` includes the Parts contract consumer and
the ordinary native Studio journey at measured desktop/compact host widths.
It stages `parts-editor.html` and `parts-studio.html` with matching Pascal-built
browser consumers and source worker. Executed browser input requires the
separately admitted HTTP environment; a compiled page alone does not qualify it.
WORK.md records the actual current results and remaining target/quality gates.

The public Parts focus anchor is presentation-only. A host can capture the
currently focused Parts button with `TryCaptureNyxPartsEditorFocus`, then use
`Resolve` against its newly mounted form to find the current action for that
same instance and named path. It contains no document, widget or callback.
Missing forms, changed instances and absent paths yield an undefined control
reference. Hosts still own project/load guards and should observe focus just
before replacement, so a user's later choice takes precedence over an earlier
queued action. This anchor never authorizes an edit or changes source/history.

Native custom hosts can obtain a typed runtime reference from an exact mounted
widget with `TNyxLCLRenderer.ControlReference`. It also recognizes the separate
input of a composite field and returns an undefined reference for nil or foreign
controls. Resolve that reference in the current renderer root before capturing
the Parts anchor; neither lookup retains the widget or its owning view.

New native keyboard focus reveals its control through the renderer's nested
scroll scopes. A custom host that is replaying presentation can instead call
`TNyxLCLRenderer.FocusWithoutScroll(ID, Identity)` after resolving the new Parts
action. This preserves the user's current viewport, including deliberate
scrolling away from a still-focused action. Ordinary focus callbacks still run;
their redirect to another control can reveal that control. Replay intent is
scoped to the exact runtime target and its revocable mount generation, so it
cannot spill into a same-ID replacement. The method retains no native control
after its focus call returns.

Studio samples the current Inspector scroll offsets just before replacing its
focused Parts form. It restores those offsets only after the same instance and
named path resolve in the accepted replacement. This keeps visible actions in
place while also respecting deliberate scrolling away; source navigation keeps
its own focus priority.

## Creating a reusable copy

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
