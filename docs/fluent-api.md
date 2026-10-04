# Typed fluent configuration

[Architecture](architecture.md) · [Components](components.md) · [Current work](../WORK.md)

Application code and Studio-generated Pascal use the same public typed contract:

Compound self values, named fields, ranges/choices and scalar payloads use
[typed fluent contracts](contracts.md). Factory-created compound parts generate
purposeful locals such as `LSearchQueryInput` rather than internal part serials.

```pascal
LReplyMemo := TNyxNode.Create(nkMemo, 'reply');
LCard.Add(LReplyMemo); // Ownership transfers before configuration can fail.
LReplyMemo.Configure
  .Text('Write a reply')
  .Placeholder('Your thoughts…')
  .ReadOnly(False)
  .PartName(NyxPart('reply'))
  .Done;

LCard.Configure
  .Layout(nlColumn)
  .Gap(10)
  .Padding(20)
  .Surface(True)
  .Done;
```

Import `nyx.types` and `nyx.model`. A node owns its lazily allocated
`TNyxNodeConfig`; `Configure` borrows that object and `Done` returns the node.
Never free the facade or retain it after freeing its node. Clones create their
own configuration objects. Configure before transferring a child only when
the caller already has an exception-safe ownership boundary for that child.

The 75 built-in kinds use `TNyxKind`; `nkSlotOverride` also describes persisted
instance customization. `TNyxCatalog.NewNode(nkCommentThread, 'discussion')`
instantiates a complete compound. `TNyxNode.Create(nkMemo, 'reply')` creates a
primitive or explicit node. Custom recipes use `NyxCustomKind` references;
text overloads remain for codec/legacy compatibility. Extensions do not have to
patch the enum.

| Configuration | Pascal argument |
| --- | --- |
| Layout | `TNyxLayoutMode`: column, row, grid, absolute |
| Variant | `TNyxVariant`; `TNyxStyleRef` from `NyxStyle` for extended themes |
| Action | `TNyxAction`: clear, dismiss, toggle, select, increment, decrement |
| Projection | `TNyxKind`; `TNyxKindRef` from `NyxCustomKind` for custom adapters |
| Part override operation | `TNyxOverrideMode` |
| Input type | `TNyxInputType` |
| Padding, gap, dimensions, columns, range limits | `Integer`, checked before mutation |
| Enabled, visible, readonly, surface, compound, pressed | `Boolean` |
| Value | Distinct text, integer and Boolean overloads |
| Caption, placeholder, items, hints, accessible text | `TNyxText` |
| Named parts, targets and override paths | `TNyxPartRef` from `NyxPart` |
| Event references | `TNyxEventRef` from `NyxEvent` |
| Reusable definition references | `TNyxComponentRef` from `NyxComponent` |

Event names and user-defined part/component names are application data with open
vocabularies. Their reference types are distinct: a part cannot be passed as an
event. Behavior keywords are enum values. Cross-property constraints and
reference existence remain document/command admission checks; individual integer
bounds fail before changing a property.

`Clear(TNyxAttribute)` preserves an explicitly empty optional property.
`Extension(key, value)` stores consumer-owned data and refuses recognized built-in
keys. `SetProp` and `Metadata(TNyxAttribute, value)` remain explicit low-level
codec/legacy boundaries. Generated built-in options use typed methods;
noncanonical legacy representations can use Metadata to preserve original data.
Serialized version-1 designs remain text-based interchange data. That storage
format does not dictate the application's Pascal authoring API.
Document/node [structured extensions](extensions.md) use a separate owned store
with typed references and immutable values. Generated nested objects/arrays use
the same public constructors, preserving exact decimal spelling and Unicode.

Generated locals describe purpose and control type (`LReplyMemo`,
`LCreateProjectButton`, `LCommentsCommentThread`). Authored IDs supply the purpose;
type-bearing names avoid repetition. Unrelated insertions retain these names,
including insertion of another memo. Case, punctuation, truncation and numeric
suffix collisions are admitted across the complete local namespace.
State defaults and bindings also share purposeful typed locals such as
`LReplyTextState`; the generator initializes each open reference once. Node-owned
`Binds` blocks use those references and closed direction/property enums. See
[live bindings](bindings.md) for control admission, instance unbinding and lifetimes.
Keys already ending in their scalar type or typed-state suffix retain that meaning
without doubling it: `replyText` and `replyTextState` both use a `ReplyTextState`
base, with namespace collision suffixes only when needed.
Configuration blocks preserve property order, authored identities and extension
data. Blank lines separate control construction; the generated document owns
each control before configuration, so failure frees the complete partial tree.
No-op generation is deterministic.

[Typed state defaults](state.md) use text/Boolean/integer/number references and
ordinary Pascal arguments in generated source. Controls containing embedded NUL
use the public scalar encoder; typed UTF-8 runs preserve it across FPC's older
literal conversions. Ordinary user captions remain ordinary quoted literals.

Studio design mode selects and edits ordinary input fields. A canvas memo change
is an undoable design command and updates generated source. Editing an inherited
named field creates/updates an instance-only override. Its reusable definition
and other instances retain their values. Selection retains mounted canvas
controls; desktop chrome can refresh through public `MoveHost` without replacing
the fields. Focused canvas drafts survive layout-mode transitions. Delayed output
profile callbacks preserve pending shell input. Canvas scroll is restored for the
same active view. Browser input
uses its normal change/commit event. Typed configuration, default, binding,
contract and extension source edits share the portable candidate/history boundary;
broader source synchronization remains open.

Evidence: shared FPC/pas2js fixtures, generated source reconstruction, real
desktop/390-pixel canvas editing journeys, native catalog/control projections and
delegated browser/LCL builds. Twenty negative Pascal fixtures require actual type
diagnostics from each compiler, covering layout, spacing, Boolean and reference
arguments, including distinct state references, numeric state values and binding
reference/kind/target arguments, extension reference/value arguments, event
triggers/handler signatures, actions, part/kind reference families and override
modes. Typed observable defaults and live control
bindings are integrated. [Observable collections](collections.md) add specialized
managed store/snapshot/subscription interfaces, typed schema fields and atomic
ordered edits. Authored/runtime registries, wire persistence and paired source/
history are integrated; actual data controls and their visual binding editors
remain open.
Studio's [state and binding editors](studio-state.md)
consume the same contract. [Typed callbacks](events.md) carry owned scalar data
through both renderers. Part lookup/overrides accept `TNyxPartRef`, override modes
accept enums, and extension recipe registration accepts `TNyxKindRef`; omitted
override modes preserve existing operations. Text overloads remain explicit
metadata/legacy boundaries. [Declared scalar domains](contracts.md) now use typed
fluent value objects; arbitrary extension property schemas, broader Pascal
synchronization and broader event/renderer parity remain active work.

## Crafted source and editable configuration

Generated locals convey purpose and control type, such as
`LProjectDescriptionMemo` and `LSearchQueryInput`. The same public fluent methods
are available to handwritten code. State references are named once and reused;
layout/action choices are enums; numeric and Boolean values retain their Pascal
types. Explicit metadata methods preserve noncanonical imported data without
pretending it is a typed behavior argument.

Studio's optional Pascal view is an editable Nyx code-editor. **Apply Pascal**
admits typed control/state declarations, specialized factories and class
constructors, control/root ownership, project `Result.Title`, `Configure` blocks,
reference initialization, scalar defaults, fluent `Binds`/`Contract` blocks and
document/control `Extensions` construction as one
undoable command. The reader accepts ordinary comments/spacing, quoted text,
typed text concatenation, signed Integer/finite Double literals, Boolean values,
enum symbols and distinct reference constructors. It calls the public typed
contract on a fresh independent document, then validates the complete document and
persistence budgets before publication.

Declared state references must be initialized before use. Their declarations,
factories and consumers must agree on the Pascal scalar family; changing all
three consistently can change that family. Changing a key updates every
subsequent use of its local.
Inline references can add new defaults without adding a declaration:

```pascal
LReplyTextState := NyxTextState('discussion');
Result.State
  .SetValue(LReplyTextState, 'Write something thoughtful.')
  .SetValue(NyxIntegerState('reply-count'), 0);

LReplyMemo.Binds
  .Value(LReplyTextState, bdTwoWay)
  .Done;
```

Defaults support typed `SetValue` and `Remove`. Number defaults accept Integer
widening; text coercion is refused. Binding blocks may be added, removed or
reordered after their control is owned. `Clear(bpValue)` records an explicit
unbind; `Inherit(bpValue)` removes the local descriptor. Only `Value` accepts a
direction argument. Wrong reference families, missing defaults and invalid
control ranges fail whole-document admission before accepted controls change.

Domains use the same typed factories as handwritten Nyx. `Range` belongs to
Integer/Number domains; `Choices` accepts an open array of that domain's scalar
family. A Number argument may widen an Integer. `Value`, named `Field`, `On`,
`Signal` and `NoValue` retain their public meanings. Imported descriptor spelling
and member order remain explicit through typed `Contract.Metadata` construction.

```pascal
LReplyMemo.Contract
  .Value(NyxTextDomain.Choices(['', 'Ready']))
  .On(ntChange, NyxTargetValue, NyxTextDomain);

LReplyMemo.Extensions.SetValue(NyxExtension('app.validation'), NyxObject([
  NyxField('limit', NyxData(NyxDecimal('9007199254740993'))),
  NyxField('labels', NyxArray([NyxData('Ready'), NyxNull]))
]));
```

Extensions accept typed `SetValue`/`Remove` and nested NyxObject/NyxField/NyxArray/
NyxData/NyxNull constructors. Exact decimals use NyxDecimal text explicitly.
Omission removes the local binding, declaration or payload rather than leaving
stale cloned data behind. Required defaults, named parts, choices, reserved fields,
duplicate keys and complete persistence budgets still run before publication.
Choice constraints admit saved fallback values as well as bound defaults.

The source delimiters are ordinary Pascal comments:

```pascal
// <nyx:views>
// Studio synchronizes this builder; keep application helpers outside it.
function BuildNyxDocument: TNyxDocument;
// declarations and owned fluent construction
// </nyx:views>
```

Keep application helpers/imports outside this builder. Visual edits reconcile
the accepted builder with the changed design. Deliberate control and typed state
locals survive; new defaults avoid their reserved names. Identifier changes leave
captions, hints, literals and comments untouched. Unchanged typed expressions
retain their spelling, including expressions inside structured data. Comments
within changed values or deleted controls remain Pascal comments. Helpers and
imports outside the delimiters remain exact.

Removing a visual control or extracting a standalone view prunes complete
construction, ownership and metadata statements by admitted control identity.
A handwritten constructor may appear earlier than its ownership call; retained
statements keep that order and their exact expressions. Grouped declarations
retain the remaining local names and notes. Configure and binding sections
reconcile independently, so a factory's WithText/default recipe does not make
new configuration attach to an unrelated ownership call. Complete candidate
verification still runs before publishing the reduced source/design pair.

Rename an existing local consistently in its declaration and uses, retaining a
valid specialized interface/factory assignment and ownership sequence.
For example, `LNotesMemo: INyxMemo` may become `LJournalMemo: INyxMemo`.
State names stay independent of their application keys; Studio's state-key
rename also retains a deliberate Pascal local. Duplicate/reserved names, wrong
interfaces and incompatible constructors remain rejected drafts.

Code edits can add/remove controls and pages, create reusable definitions and
instances, change control kind with its declaration, and move ownership
statements. `Result.AddPage` and `Result.AddComponent` own roots; `Add` and typed
`Insert(Integer, child)` own descendants. Declare and construct controls before
adoption; admit the parent before adding children. Configure/bind only after
adoption. Every constructed local must acquire one document/tree owner. Removing
the declaration, construction and uses removes its meaning from the fresh
document; a dangling use, duplicate identity/owner or cycle is rejected.

Specialized factories and implementation constructors retain their interface
typing, including fluent `WithText`:

```pascal
LReplyMemo := NewNyxMemo('reply').WithText('Write a reply');
LDiscussionColumn.Add(LReplyMemo);
LReplyMemo.Binds.Value(LReplyTextState).Done;

LSendButton := TNyxButton.Create('send').WithText('Send reply');
LDiscussionColumn.Add(LSendButton);
```

Default compound factories construct their independent named recipe parts.
`ncoDescriptor` explicitly reconstructs an expanded descriptor instead.
`NewNyxBuiltinControl` returns `INyxControl`; its runtime kind does not make it
assignable to a narrower interface. `NewNyxControl` takes a distinct custom kind
reference and retains that open base contract. Existing `TNyxNode` constructors
remain the explicit descriptor compatibility path. Grouped declarations and
unused interface locals are supported and reserve their authored names.

Configuration, binding, contract, callback and extension declarations may live
after their owned controls. Reconciliation groups them by stable owner and
fluent section, retaining unchanged
declaration sites and exact extension order. A changed group can be gathered at
its first authored site; surrounding notes remain. Candidate verification checks
the complete reconstructed design before a visual command publishes either
member. Ambiguous overlapping edits or the bounded reconciliation budget produce
a diagnostic and keep the accepted pair and redo history. Undo/redo restores
exact source snapshots, including authored spelling and comments.

A malformed, wrong-typed, unsupported or stale draft leaves the accepted design,
Pascal and redo history intact. **Restore accepted** discards the buffer explicitly;
**Save draft** downloads it independently. A pending draft survives hiding the
code view and later visual edits, but cannot overwrite those edits without an
explicit merge. Browser recovery stores accepted companion Pascal and the draft
beside its local design recovery. Portable `.nyx` and companion `.pas` exports
remain separate.

Failed **Apply Pascal** shows a diagnostic beside the retained source. **Go to
line:column** explicitly focuses its site without replacing the draft. Positions
use one-based Unicode scalar columns; adapters translate them to browser/Win32
UTF-16 caret units. Typing clears the previous diagnostic, and a stale navigation
action rejects rather than targeting a later buffer. Errors without a trustworthy
site remain readable without a guessed source link. Restore/success removes the
diagnostic. Compiler-owned helper errors still appear in the build log; structured
compiler-file navigation remains with Studio/service work.

`TNyxStudioSession.SourceDiagnostic` returns an owned typed record for the exact
current draft. `ENyxSource.DiagnosticText` retains UTF-8 natively instead of relying
on the ANSI exception-message boundary. These diagnostics are presentation,
independent of saved design/source history and output configuration.

This is a bounded declarative edit reader with one construction per local and
one Configure block per control. Explicit nonempty control identities keep
source reconstruction deterministic. Keep the owned `TNyxDocument.Create` /
`try` / `except Result.Free; raise` builder frame. Arbitrary expressions/control
flow, direct specialized property assignments and part-interface assignments
inside the managed builder remain unsupported; keep ordinary application helpers
outside it. Broader editing UX and large-document performance remain open.
Accepted companion Pascal is included in HTTP application and
isolated-view builds, including handwritten helpers and callbacks; ordinary helper
syntax/type errors appear as compiler diagnostics. Actual native Nyx editor/button
events exercise shared source/property commands; the complete native Studio
controller remains separate work.
