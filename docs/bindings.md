# Typed live bindings

[State](state.md) · [Fluent API](fluent-api.md) · [Evidence](../WORK.md)

The coordinator admits and publishes the complete command before synchronizing
target controls and delivering semantic callbacks. Targets may opt into
`OnValueSync` for an unbound edit whose complete staged projection differs only
in that exact leaf's value. The Boolean result accepts field synchronization or
requests ordinary `OnSync`; the default remains complete synchronization. This
hook does not skip property/store validation or alter ownership/history.

Browser and LCL adapters use it for standard input/memo faces, retaining caret,
scroll and value baselines when the physical text already matches. Composing,
custom or content-sized controls use the full path, as do state bindings and
compound actions. Structural content observation remains queued. A hook exception
leaves the admitted projection committed, matching the existing notification
contract. Both callbacks borrow their receivers; the field node is borrowed only
for the synchronous call and must not be retained or used to retire the view.

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

## Coordinated accepted input values

`nyx.binding.inputs` exposes typed capture and prepared assignment through
`TNyxLiveBindings.Inputs`. This is the same live binding/state owner used by
ordinary input. It can join a [prepared collection query](collection-queries.md)
so Search, Sort, bound mirrors and query membership install together before any
observer runs:

```pascal
LInputs := ALiveBindings.Inputs;
LCapture := LInputs.Capture([
  NyxControl('people-search'),
  NyxControl('people-sort')]);

LReset := LInputs.Prepare(LCapture, [
  NyxInputValue(NyxControl('people-search'), ''),
  NyxInputValue(NyxControl('people-sort'), 'Name')]);

PublishNyxGroup([LReset, APreparedQuery]);
```

Here `LInputs` is `INyxInputCommands`, `LCapture` is `INyxInputCapture`, and
`LReset` is `INyxPreparedPublication`. `APreparedQuery` comes from the query
owner's own current capture; preparing input does not grant query authority.
The sort face in this example has a text value. Boolean, integer and number
faces take their corresponding typed `NyxInputValue` overloads.

A capture contains 1..64 distinct exact runtime control references and copied
typed values. `HasValueAt` distinguishes an absent value from explicit empty
text; `ValueAt` refuses absence. Captures remain readable after disposal but
cannot authorize a successor, another instance or an A-to-B-to-A command
sequence. Retained command ports are weak and report `Connected = False` after
their live owner retires.

Preparation accepts only captured roles and their declared scalar families.
Visibility, enabled/read-only state, writable bindings, domains, portable text
rules and numeric limits all participate in admission. A value and its bound
minimum can change in the same candidate. Conflicting assignments to one bound
state key refuse the whole batch. Source-only, resource and expression bindings
do not become writable. Unbound values use the runtime projection without
inventing state keys or changing authored defaults.

The prepared group reserves the existing view and state owner until retirement.
Refusal or abandonment changes neither; a notification failure remains committed
and independent input/query notifications still receive their attempt. Reset
does not synthesize Click or Change callbacks. These commands run on the UI
thread, with the existing finite view observation serial.

An active browser/LCL renderer exposes the same port as `View.Inputs`, adding
physical input admission and synchronization. It returns nil while unmounted or
in design mode. Its capture owns copied draft text and a weak view proof; it
retains no widget or renderer. Newer typing, active composition/pointer capture,
replacement and disposal refuse before installation. Unsupported opaque input
faces refuse instead of claiming to reset an inaccessible control.

A successful reset invalidates only the requested fields' accepted-value
baselines before ordinary Sync. This restores a requested unfinished draft even
when its accepted value is unchanged, while unrelated drafts retain their usual
baselines. Custom updaters and target failures keep the existing committed
notification semantics. A model-only binding owner may omit the physical hooks.

Read-only phase signals continue observing the accepted root while an input
preparation owns a prospective candidate. They grant no write reentry. This lets
a newly starting composition invalidate that preparation without raising an
internal native message exception. A genuine composition end restores readiness.
Prepared groups remain synchronous UI-thread work; callers must not hold them
across an event-loop yield.

Current native evidence includes29 real-control checks at each configured width
1280/390,45 model/query checks and85 binding/104 state regressions, with four
complete clean heaps. Synthetic Win32 messages qualify the composition bridge;
they do not prove hardware IME, assistive or other-widgetset behavior. Both
matching browser consumers compile only. The optional `INyxCoordinatorInputs`
facet now reserves exact scalar roles before Build and returns weak commands for
only those roles. The trusted adapter resolves them against the exact adopted
tree; a pending candidate cannot borrow the previous project's input owner.
Captures copy values and proofs without retaining a renderer or node. Existing
coordinator interfaces and role-info record layout remain unchanged. Separate
optional target and behavior facts provide bounded discovery without raw nodes.

The [DataToolbar application](components.md#search-sort-and-clear-for-data-controls)
uses this port for actual Search/Sort/Clear on List/Table controls, including
grouped input/query reset. [Runtime02](../TODO/NS-3_data-toolbar_02.md) owns its
current qualification and remaining gates. These bounded native checks do not
accept that task or its broader prerequisites.
