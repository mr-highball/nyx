# Typed state and saved defaults

[Fluent API](fluent-api.md) · [Design model](architecture.md) · [Evidence](../WORK.md)

`nyx.state` owns portable scalar values independently of browser or Lazarus
controls. A document owns authored defaults. Browser/native application hosts own
independent runtime copies without changing what Studio saves. [Typed bindings](bindings.md)
connect controls, compound actions and page navigation to those stores.

```pascal
LDocument.State
  .SetValue(NyxTextState('reply'), '')
  .SetValue(NyxBooleanState('can-post'), True)
  .SetValue(NyxIntegerState('reply-count'), 0)
  .SetValue(NyxNumberState('completion'), 0.25);

LRuntimeState := LDocument.State.Clone;
try
  LRuntimeState.Apply([
    NyxStateValue(NyxTextState('reply'), 'A new idea'),
    NyxStateValue(NyxIntegerState('reply-count'), 1)
  ], LRuntimeState.Revision);
finally
  LRuntimeState.Free;
end;
```

The four reference types select typed getters/setters and batch helpers. A part
reference cannot become a state reference; numeric text cannot become a number
argument. Open names are application data. Declare and reuse a typed reference
in an application owner when several controls or commands use the same key.
`Value(key)` and `NyxStateAssign(key, taggedValue)` are explicit codec/bulk
boundaries. They retain the value kind and refuse implicit conversion.

| Value | Meaning |
| --- | --- |
| Text | Exact valid Unicode; empty text and embedded controls are allowed |
| Boolean | Pascal Boolean |
| Integer | Signed 32-bit integer on both targets |
| Number | Finite IEEE Double; signed zero is canonicalized |

Keys require 1..128 Unicode scalars, meaningful content, and no C0/C1 controls or
line separators. A store admits at most 1024 entries and 1 MiB of content: UTF-8
bytes for keys/text and 1/4/8 bytes for Boolean/integer/number payloads. This is a
content budget, not a whole-heap measurement. Getters refuse a missing key or a
different kind. Removing a key differs from setting empty text.

`Apply` validates a detached candidate and publishes one revision. Duplicate
keys within a batch, stale revisions, kind changes, malformed text, exhausted
budgets and validator rejection preserve all accepted values and their order.
Existing positions remain stable; new keys append in request order. No-op sets,
missing removals and empty batches do not advance the revision or notify.
`Assign` explicitly replaces a dataset and can change kinds after validation.
An order-only replacement publishes once with `OrderChanged=True`.

Revisions belong to a particular store instance. Clone retains its data revision;
serialized designs contain values rather than revisions. Loading or restoring
history creates a new store. Do not use a freed/replaced store's revision as an
application-wide or persistent design revision.

`Subscribe` returns a caller-owned token with an optional validator and observer.
Validators inspect a complete read-only proposed store. Observers inspect the
complete committed store, with typed before/after changes. Read `HadValue` and
`HasValue` before accessing optional values. Change objects and candidate stores
are borrowed only for that callback; copied scalar values can be retained.

Callbacks may disconnect/free tokens. Notification snapshots token identities and
skips disconnected listeners safely. Writes and new subscriptions during either
phase are refused. Put dependent edits into one domain-command batch. Free a
receiver's tokens before the receiver; freeing the store detaches surviving
tokens. Never free the store, candidate or borrowed change object inside a callback.
Stores and their listeners use one UI event thread.

A validator failure precedes publication. An observer failure follows publication:
the store still notifies remaining listeners, then raises `ENyxStateNotification`.
That exception means the update committed. Catching its `ENyxState` ancestor does
not imply rollback. Clone copies values without listeners.

Version 1 designs optionally contain `state`, keyed by application names. Each
entry has exactly a `type` and `value`. Text/Boolean/integer use matching JSON
types; integer defaults require a signed 32-bit integer spelling, preventing
fractional digits from being rounded away through Double. The version tag is the
exact integer `1`. Numbers use tagged decimal strings so older native JSON formatters retain
all Double precision. This is a persistence representation; Pascal authoring uses
Double arguments and typed references. Empty stores omit the field, preserving
the earlier wire shape. Isolated page/component builds copy all application
defaults into an independent store.

Numeric conversion uses a compact round-tripping decimal, at most 17 significant
digits, without changing global locale settings. Integral number defaults are
generated as real literals such as `10000000000000000.0`; they retain number
meaning without FPC's integer-to-Double bit-cast ambiguity. Imported decimals must
follow the complete JSON grammar within 128 characters. Overflow and nonzero
underflow to zero are refused consistently. Exact signed zero remains zero.

`nyx.json` supplies strict Pascal decoding into standard fpjson containers on
both targets. It preserves NUL/Unicode escapes, refuses unpaired surrogates and
decoded duplicate keys, and enforces 4 MiB UTF-8 input, 270 container levels and
1024 members per object. Older native scanners lose escaped NUL; browser
`JSON.parse` collapses duplicates. Neither decides accepted design meaning.

Studio's `SetStateValues` admits a detached document before recording one history
entry and publishing it. Failed imports/commands and no-ops preserve the accepted
design and redo history. Undo/redo restore all saved defaults. Its current bounded
snapshot history copies full documents; command deltas, byte-symmetric history
limits and large-document performance remain open. Bound defaults also pass
effective control/range admission before history changes. [Studio state authoring](studio-state.md)
now exposes saved defaults and typed bindings through Nyx controls and shared
session routing, including rename migration, inheritance and rejected-edit recovery.

Shared fixtures exercise typed batches, lifetimes, rejection, precision, Unicode,
persistence and history. Native-generated Pascal executes on native FPC and in
pas2js browser reconstruction checks. HTTP fixtures compile state-bearing page,
reusable, instance and application scopes for both outputs. This establishes the
typed state/persistence foundation. Live binding and application navigation now
have browser/LCL interaction evidence, documented in [bindings](bindings.md).
Broader extension/event contracts, structured collections and production authoring
still keep the task open.
