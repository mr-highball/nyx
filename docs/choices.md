# Stable choice values and friendly captions

Nyx selects can keep a stable value independently from the caption shown to a
person. This is useful for translated text, duplicate labels and settings whose
display names change over time. Values remain exact portable text identities.

```pascal
uses nyx.controls, nyx.choices, nyx.select.configuration;

var
  LOperationSelect: INyxSelect;
begin
  LOperationSelect := NewNyxSelect('operation');
  LOperationSelect.Configure.Text('Operation').Value('parse-json').Done;
  NyxSelectConfigure(LOperationSelect).Choices(
    NewNyxChoices
      .WithChoice(NyxChoice('parse-json'), 'Read JSON')
      .WithChoice(NyxChoice('format-json'), 'Format JSON')
      .WithChoice(NyxChoice(''), 'None')
  );
end;
```

The ordinary value binding and change events use the key. Both renderers display
the caption. Duplicate captions are valid; duplicate exact keys refuse before
publication. An unknown value leaves the control unselected. It never adopts the
first row or overwrites the model. An explicitly empty key is a real option and
differs from having no physical selection.

Keys are opaque identities. A typed select can retain CR/LF/NUL in a key because
only its admitted caption enters native widget text. The same rule applies to
bound state, unbound input and programmatic values. Legacy caption-valued selects
and ordinary text inputs keep their existing single-line/widget restrictions.

`INyxChoiceItems` is reference counted and immutable. `WithChoice` returns a new
list without changing earlier lists. Nodes normalize alternative implementations
through one `ToData` call and retain their own immutable value. They do not retain
the provider. Failed admission preserves the accepted set, including refusal of
same-node reentrant choice mutations. Ordinary descriptor mutation remains
confined to its owning authoring/UI thread.

`NyxSameChoiceItems` compares logical keys, captions and order. Equal normalized
snapshots avoid option reconstruction even when they are separate managed
instances; it does not admit a foreign provider into a model.

An empty `NewNyxChoices` deliberately masks inherited choices. Use
`NyxSelectConfigure(LSelect).InheritChoices` to remove the local declaration.
Reusable instances and named-part descriptors can use the implementation-neutral
`SetNyxChoiceItems` attachment; document composition checks their effective select
projection. Replacement and inheritance never edit the reusable definition.

While a typed set is present, it supplies the options instead of legacy newline
`Items`. Removing it restores those legacy options. Caption/order changes retain
the mounted input and preserve a selected key. Choice refresh does not publish
an invented value change.

Each set admits at most 1,024 rows and 256 KiB of logical UTF-8 key/caption text.
Captions are single-line display text. Exact keys may include other admitted
Unicode or control scalars; indexes and translated captions never identify them.
The independent descriptor has version one. Saved designs containing typed
choices select version seventeen; earlier opaque `choiceItems` fields keep their
extension meaning, and conflicting promotion refuses before saving or history
publication. Removing the final typed set restores the earlier canonical format
when no later feature remains.

Generated Pascal uses specialized, purpose-named control interfaces and fluent
`NewNyxChoices.WithChoice` blocks. The strict source reader admits those public
constructors and the typed attachment without running a provider. Literal facade
replay and executed-companion customization coverage remain under
[the choice authoring task](../TODO/NS-1_choice-items_01.md).

Studio's Component options contains a **Choices** editor. Choose a row to edit
its exact key and caption; Save, Add, Move and Remove each propose one complete
undoable replacement. **No choices** declares an empty local set. **Use inherited
choices** removes that declaration and restores inherited choices or legacy
Items. Unfinished fields remain separate from the accepted Pascal/design pair.
Changing the accepted choice context retires an older form draft. A pending
Pascal draft must be resolved before a choice edit publishes.

Special-character keys use an explicit **Escaped key** checkbox and quoted JSON
string notation, such as `"line\r\n\u0000"`. Ordinary keys are entered directly.
This keeps exceptional keys editable without sending control characters into
a single-line native text widget. Display captions remain ordinary text.

The reusable library form is `NewNyxChoiceEditor` in `nyx.choices.editor`.
`CaptureNyxChoiceEditor` returns typed intent and its exact local/effective
baseline; it never edits the document. Hosts recheck the baseline and use their
ordinary atomic commands. `TNyxChoiceEditorDraft` parks copied unfinished input
without retaining a document or widget. Studio consumes this same public form
through its paired source-command queue. Typed command callers can prepare
`NyxSetChoices(NyxControl('operation'), LChoices)` or `NyxInheritChoices(...)`
from `nyx.studio.edits` and combine them with other project changes.

The existing MCP tools expose choices without a whole-document response:

```json
{"id":"operation","choices":true,"choiceScope":"effective",
 "choiceOffset":0,"choiceLimit":4,"textLimit":80,"expectedRevision":12}
```

`nyx_node` returns `choices.scope`, `localDeclared`, `defined`, `total`, `offset`
and a row page. Each row has an index plus separate `key` and `caption` windows
containing `value`, `offset`, `totalScalars` and `truncated`. Use `textOffset` to
continue a window at the same revision; a truncated key is not a usable identity.
The default row count is eight, maximum sixteen. `choiceScope: "local"` reads
only the authored declaration; effective scope independently resolves reuse.
An absent set is `defined: false`, distinct from an explicitly empty set.

`nyx_transaction` accepts `choices-set` with `id` and the version-one `choices`
descriptor, or `choices-inherit` with only `id`. These are explicit wire names;
Pascal authors use the typed commands above. Related operations share the
existing expected revision, operation ID, permission/draft guards and one paired
Undo. Duplicate keys, unknown fields, incompatible owners and late group failures
refuse atomically. Inherit requires an existing local set. When removing typed
choices would make an opaque default invalid for the legacy widget, update that
default in the same transaction.

The shared semantic boundary and actual Inspector/source-worker controls have
native/browser checks. Ordinary whole-project Save/Open with this new field and
authenticated observing-service rollout remain separate qualification work;
the broader authoring task is still open.

The Logic editor consumes this contract: operation titles are captions, and kind
references remain stable values. Multiline creator titles are projected to one
line in the selector while their original metadata, help and search text remain
intact. Its repeated Configure actions carry operation-specific accessible names.
