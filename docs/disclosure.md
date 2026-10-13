# Retained disclosure sections

[Components](components.md) · [Public contract](../src/nyx.disclosure.pas) ·
[Generated reference](components-reference.md)

`NewNyxDisclosure` returns the reference-counted `INyxDisclosure` interface.
Its ordinary owned recipe contains a `header` Button and a retained `body`
Column. These named parts remain editable through the same public Nyx API as
other components. It introduces no DOM or LCL types into portable authoring.

```pascal
LDetails := NewNyxDisclosure('advanced-settings');
LDetails.WithText('Advanced settings');
LDetails.Configure.Value(False).Done;
LDetails.Part(NyxPart('body')).Add(
  NewNyxMemo('notes').Configure
    .Text('Notes')
    .Value('Keep a few ideas here.')
    .Done);
```

Boolean `Value` is desired expansion. Bind it with
`Binds.Value(NyxBooleanState('details-open'))` for an independent runtime store
and atomic two-way header actions. Reusable instances retain independent
expansion and owned descendants. Closing hides the body and excludes it from
ordinary native flow allocation; controls and unfinished input remain mounted.
Opening still respects caller-authored and bound visibility restrictions.
Read-only rejects header changes while programmatic state updates remain valid.
Runtime actions retain the authored document and its state defaults.

An empty header caption inherits the root `Text`. Explicit header text wins.
Header actions address the compound Boolean value. Arbitrary body controls keep
their existing event identity; placing an editor inside a disclosure does not
redirect its changes to the wrapper.

Derived recipes may select distinct named parts through an immutable policy:

```pascal
SetNyxDisclosureOptions(LDetails,
  TNyxDisclosureOptions.Defaults
    .Header(NyxPart('header'))
    .Body(NyxPart('body')));
```

The header must project a Button outside the body. The body must project a
container. Complete candidate admission refuses missing parts, a header inside
the body, invalid policy data and non-Boolean values. Persistence stores the
versioned known extension; generated code emits this typed construct and
specialized interfaces. The ordinary source reader requires distinct `NyxPart`
references and reconstructs the same policy without evaluating arbitrary Pascal.
Nodes own their parts; borrowed validation references are never retained.

Both adapters consume the same visibility gate. Browser headers expose
`aria-expanded` and `aria-controls` with the retained body's exact identity.
The browser uses a real Button, following the
[WAI disclosure pattern](https://www.w3.org/WAI/ARIA/apg/patterns/disclosure/).
Closing through application state recovers focus from content to its available
header. Nested closed bodies recover to the outermost closed header. Opening
leaves focus in place, and closing another section never claims unrelated focus.
The settled transition delivers one `OnAfterExit` before one `OnAfterEnter`.
An exit callback's deliberate focus redirect or view retirement takes precedence.
An enter callback may dispose the renderer; independent application state must
outlive its own publication and notification. The adapters check view revisions
before touching captured controls after callbacks, and retain no model cycle.

Current qualification covers programmatic physical control activation,
binding updates, independent instances, retained memo drafts, readonly refusal,
source admission and executed generated reconstruction. Actual native/browser
controls also qualify nested close, unrelated focus, callback redirects, ordered
notifications and disposal from the recovered enter callback. Trusted Chromium
pointer/Enter/Space/Tab/typing and a state close inside a real keyboard callback
pass at desktop and CSS-390. This establishes those host defaults; native
keyboard messages, physical hardware/IME, assistive technology, additional
widgetsets and broader visual quality remain open.

Studio uses this public component for its [inspector groups](inspector-groups.md)
and independent Tab pages section. Both hosts capture
the transient expansion alongside copied form drafts through shell refresh.
This view state does not add a design/source command or alter exported projects.
The ordinary inspector/source queue passes actual native and browser checks;
connected host lifecycle and preserving observing-release delivery remain open.

Run `tools/build.ps1 -Target disclosure -DisclosureEvidenceDirectory <NEW-path>`
to execute native controls and generated Pascal, then compile browser inputs.
Serve the staged `browser/disclosure.html` through an existing HTTP host and
observe `data-nyx-disclosure="passed"` for browser execution evidence.
On Windows, the same target also compiles the Pascal `nyx_disclosure_input`
owner. Run it with the staged page's `#review` URL, a NEW capture directory and
CSS width to qualify trusted input. It uses the component's exact runtime
identities, without script evaluation or semantic document mutation.

The installed authenticated MCP companion is still the primary design workflow.
`nyx_tabs_companion <config.toml> <NEW-export> --disclosure` retains the observing
release's explicit new-kind refusal, creates its tab companion in one undoable
semantic transaction, exports bounded context, checks paired history, requests
the ordinary installed build and discards its owned review. It preserves the
primary project. The refusal establishes a delivery gap; it does not qualify
the new kind on that older deployment.
