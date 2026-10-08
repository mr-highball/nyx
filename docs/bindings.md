# Typed live bindings

[State](state.md) · [Fluent API](fluent-api.md) · [Evidence](../WORK.md)

A node owns binding descriptors independently of controls and runtime stores.
Named images use a distinct typed resource selector and read-only `Binds.Image`
projection, documented in [image resources](resources.md#images-from-project-resources).
They never advertise scalar text-state binding merely because persisted image
source bytes use a textual wire representation.
Its borrowed `Binds` object is the public fluent authoring contract. Declare a
typed reference once and reuse it for defaults, controls and commands:

```pascal
LReplyTextState := NyxTextState('reply');
LCanPostBooleanState := NyxBooleanState('can-post');

LDocument.State
  .SetValue(LReplyTextState, '')
  .SetValue(LCanPostBooleanState, True);

LReplyMemo := TNyxNode.Create(nkMemo, 'reply-memo');
LPage.Add(LReplyMemo);
LReplyMemo.Configure.Text('Write a reply').Done;
LReplyMemo.Binds.Value(LReplyTextState).Done;

LReplyCaption := TNyxNode.Create(nkLabel, 'reply-caption');
LPage.Add(LReplyCaption);
LReplyCaption.Binds.Text(LReplyTextState).Done;

LPostReplyButton.Binds.Enabled(LCanPostBooleanState).Done;
```

The reference variables above have types `TNyxTextStateRef` and
`TNyxBooleanStateRef`. Studio's generator emits those typed declarations and
purposeful names, initializes each reference once, then shares it across
defaults and binding blocks. Closed binding targets/directions use Pascal enums.

| Fluent target | Reference meaning |
| --- | --- |
| Text | Explicit caption projection of text, Boolean, integer or number |
| Value | Exact editable text/Boolean/integer/number kind admitted by the control |
| Enabled, Visible, ReadOnly, Pressed | Boolean |
| Placeholder, Hint, AccessibleName | Text |
| Width, Height, Left, Top, Padding, Gap, Columns, Flex | Integer layout metric |
| Minimum, Maximum | Integer range limit where the control admits it |

A field's concrete kind is checked during admission: checkboxes use Boolean,
spin/slider/progress use integer, ordinary text fields use text, and
`niNumber` inputs accept number or integer. Unsupported property/kind combinations
are rejected. Captions use explicit locale-independent scalar projection.
Numeric values also obey effective range limits. Pressed supplies browser
ARIA/native LCL accessible-value intent; full assistive-technology verification
and persistent widgetset toggle behavior remain separate parity work.

`Value` defaults to `bdTwoWay`: an accepted control edit writes typed state.
Use `bdFromState` for a read-only projection. Other targets project from state;
control actions cannot write them. Application commands may update any admitted
typed key directly. A binding never silently invents a missing key or changes
its kind.

Memo/code-editor projections normalize CRLF and lone CR to LF. Mounting or an
unchanged control notification preserves the exact stored default; an accepted
edit stores the edited text. Single-line value projections reject line breaks.
Recognized control text rejects NUL before publication because native widgets
cannot represent it losslessly. Typed state, persistence and opaque extension
data continue to preserve NUL and Unicode exactly. Unbound controls obey the
same text admission and multiline projection rules.

Reusable definitions carry the same descriptors. Instance part overrides can
replace a binding or call `Binds.Clear(bpValue)` to remove an inherited binding.
The clear descriptor survives persistence and generation. Realization copies
metadata independently; definitions and sibling instances retain their contracts.
An unbound part continues using ordinary runtime properties. `Binds.Inherit(bpValue)`
removes a local descriptor, allowing the definition's binding to apply again.

Browser and native application hosts own independent runtime copies through
`nyx.application.state`. Their `State` property survives `ShowPage`. All pages,
including unmounted pages, validate proposed snapshots before publication.
The application borrows an unchanged authored document; destroy the application
before editing/freeing that document. `View` is a borrowed renderer for identity
lookup, custom factory registration and event/error callbacks. Native `Mount`
constructs an admitted window without starting a blocking message loop;
`Run` also shows the window and runs Lazarus's loop.

A standalone renderer owns a fresh default-state copy unless `Render` receives
a supplied runtime store. That store is borrowed and must outlive the mounted
view. Unmount/destroy disconnects the coordinator before freeing controls/model
and owned state. Passing a renderer's own `State` back into `Render` explicitly
retains that owned store across a full remount; a failed candidate preserves its
ownership. Browser design mode projects defaults without subscribing or
writing application state.

Edits and portable compound actions stage an independent runtime view. Typed
wire parsing, enabled/visible/read-only rules, property constraints and store
validators run before accepted mutation. A rejected edit restores physical values
on the same controls and suppresses its semantic event. Accepted commands publish
one state batch, synchronize consumers in place, then emit the semantic event.
Both adapters now use the same [typed handler](events.md). Enum triggers and
owned scalar values distinguish physical origin, semantic source and action
target. Payload conversion occurs before publication; retained event copies
remain valid after navigation and view disposal.
Compounds such as search clearing and number stepping use this shared path.

Adapters retain control/node identity, avoid same-value text assignments, and
preserve unfinished field drafts when an unrelated state key changes. A change
to that field's own accepted value replaces its draft. Native duplicate change
notifications containing the accepted value are ignored; restoration cannot emit
a synthetic success. Browser editing uses the platform's change/commit event.
Native numeric text inputs commit through Lazarus's editing-complete event, so
unfinished signs/decimal drafts stay editable. Native spin/range/Boolean controls
retain their ordinary change semantics.

`OnBindingError` receives a typed `TNyxBindingFailure`:
`nbfRejected` means accepted state was preserved;
`nbfNotificationFailed` means state committed and a later notification failed.
The latter is not retried or rolled back. `LastBindingError` and
`LastBindingFailure` retain the diagnostic until the next accepted control
command (`nbfNone`). Store writes made directly by application code still raise
the state exceptions documented in [state](state.md).

Bound custom factories require an updater callback. It borrows the admitted node
and target, refreshes extension-owned markup/children in place, and does not
write state. The renderer snapshots each mounted updater independently of later
registry changes. Failed candidate creation/updates preserve the accepted view.
Standard input controls returned by custom factories retain the ordinary event
bridge.

Version-1 nodes optionally serialize `bindings` as bounded typed descriptors.
Targets, kinds and directions use closed wire mappings; duplicate targets,
unknown choices, wrong types, malformed clears and missing/wrong-kind keys fail
admission. Documents without descriptors keep the earlier node shape.

Current evidence includes shared metadata/transaction/customization/history
fixtures, compiled native-to-browser reconstruction, and browser/LCL control
journeys. The initial correctness path clones views for admission and validates
all application pages; indexing and measured large-document update performance
remain open. Studio now exposes [typed state and binding authoring](studio-state.md)
through public Nyx controls and undoable portable commands. Structured collections,
arbitrary extension property schemas, broader event/parity contracts and the full native
Studio controller remain required.

Compound self values, named scalar fields and payloads now use explicit
[typed domains](contracts.md), including range/choice admission. Generic containers
expose no invented self value; Studio narrows its binding choices accordingly.
