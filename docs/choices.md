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
constructors and the typed attachment without running a provider. Full ordinary
Inspector/MCP choice editing and executed-companion customization coverage remain
under [the choice authoring task](../TODO/NS-1_choice-items_01.md).

The Logic editor consumes this contract: operation titles are captions, and kind
references remain stable values. Multiline creator titles are projected to one
line in the selector while their original metadata, help and search text remain
intact. Its repeated Configure actions carry operation-specific accessible names.
