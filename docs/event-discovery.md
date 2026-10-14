# Event discovery

[Events](events.md) · [Inspector groups](inspector-groups.md) ·
[Public component](../src/nyx.event.discovery.pas)

Studio's Events tab offers a complete list, search, event families and a
With callbacks checkbox. Search covers event names, creator descriptions and
registered Pascal handler/registration names and intent-family captions.
Terms combine: “key press” finds
the before/main/after KeyPress family; choosing Focus narrows that same query.
Clear filters restores the complete list. A pending removal review remains
visible even when its event falls outside the query, with an explicit summary.

Discovery is presentation state. It keeps event cards, policy fields and source
buttons mounted. Filtering adds no design/Pascal history command, and remains
available while Pascal is pending. Callback mutations retain their ordinary
draft-resolution, revision and removal-confirmation requirements.
The current observer still has its earlier Events presentation until delivery.

The reusable library contract is an immutable typed value and an ordinary
Nyx Card/Input/Select/Checkbox/Button composition:

```pascal
LFilter := NyxEventFilter
  .Query('key press')
  .Category(necKeyboard)
  .WithCallbacks(True);

LFilterCard := NewNyxEventFilterEditor('callback-discovery', LFilter);
LPage.Add(LFilterCard);
```

The managed Card owns its fields. Closed category and part enums supply the
public configuration; UI captions are decoded only at the explicit form boundary.
Malformed choices, Boolean input or discovery contexts refuse. Additional
application text remains ordinary TNyxText. Matching folds ASCII A..Z only;
supplementary Unicode and other characters compare exactly without locale or
normalization changes. All whitespace-separated terms must occur in the copied
search context. Categories overlap where metadata exposes multiple contexts.

Component authors call TagNyxEventDiscovery with typed schema, registration
information and optional pinned-review intent. It copies scalar help/identities
and categories at its private extension boundary, retaining no source schema,
callback or renderer. Contexts survive independent realization/codec copies.
Re-tag a card after its registrations/help change. ApplyNyxEventFilter validates
the complete tagged group before changing visibility; untagged controls and
unrelated field values remain untouched. Adapters then synchronize existing
controls on UI. ReadNyxEventFilterEditor reads only its complete scoped field group.

Studio remembers committed discovery choices through ordinary refresh and
Properties/Events navigation. Browser refresh also captures unfinished query
typing. Native open projects carry the copied presentation value. These filters
are transient and do not enter exported projects or cross-launch preferences.

Both controllers expose an owned ProjectSnapshot value for embedded observation.
It includes accepted design/Pascal and the exact pending draft/base even when a
compact panel parks the physical source editor. It grants no mutation authority;
input qualification still edits real controls and waits for public readiness.

The maintained event-inspectors build accepts an optional new
EventEvidenceDirectory, preserving earlier compiler/consumer/export evidence.
Native fixtures exercise actual queued callbacks and controls; browser execution
uses its matched source-worker module over HTTP. Hardware/IME/assistive input,
other widgetsets, production budgets and complete editor quality remain separate
qualification requirements.
