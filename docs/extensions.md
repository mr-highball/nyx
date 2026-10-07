# Structured extension data

[Fluent API](fluent-api.md) · [State](state.md) · [Evidence](../WORK.md)

Documents and nodes own an `Extensions` store for application data. Unknown
version-1 root and node fields are retained in this store when importing a design.
The codec preserves their typed meaning, member order, exact Unicode/NUL strings,
and admitted decimal spelling through export, history, clones and compilation.

Use typed values and a distinct open reference:

```pascal
LAssetsExtension := NyxExtension('application.assets');

LDocument.Extensions.SetValue(LAssetsExtension, NyxObject([
  NyxField('schema', NyxData(1)),
  NyxField('enabled', NyxData(True)),
  NyxField('items', NyxArray([
    NyxObject([
      NyxField('name', NyxData('Welcome illustration')),
      NyxField('source', NyxData('assets/welcome.svg'))
    ])
  ]))
]));
```

`LAssetsExtension` has type `TNyxExtensionRef`. `NyxData` overloads accept
text, Boolean, signed 32-bit Integer, finite Double or `TNyxDecimal`.
`NyxNull`, `NyxObject`, `NyxArray` and `NyxField` construct nested data.
An ordinary text value such as `'true'` remains text. Raw values and references
from another family do not compile as extension arguments.

`TNyxDataValue` is an immutable snapshot with the closed `TNyxDataKind` enum.
Arrays, fields, JSON containers and owning nodes are never borrowed by a value.
Factory inputs may be released or changed afterward. Store reads and `Field`/
`Item` return independent value snapshots; reassignment cannot change a stored
value. `Copy` explicitly preserves these semantics under pas2js record behavior.
Default values/references require construction before use.

Snapshots index immediate container members once, retaining canonical JSON and
exact decoded keys with offset/length arrays. `Count` and ordered `Key` reads do
not decode JSON; `Field` searches those keys and `Item` addresses an exact span.
Only the requested child is admitted into its own independent snapshot. A small
child therefore does not retain its former parent, unrelated siblings or a
mutable JSON tree. `Copy` explicitly copies the index arrays on both compilers.
`AsText` reuses its admitted scalar text. No index/cache fields enter persistence
or generated source; all interchange types and canonical formatting remain the
same. If formatting expands input beyond the decoder's UTF-8 read budget,
cached container/text reads retain the former refusal rather than bypassing it.

`tools/build.ps1 -Target data-read` runs an optimized native read sample with
range/overflow/I/O checks and stages the same Pascal browser workload. Its CSV
separates construction/read time and checks every exact value/order. Timing uses
no heap tracing; native ownership regressions run separately. Execute the staged
`data-read.html` over HTTP for browser samples. Neither a native timing nor a
browser compilation establishes another target's speed, overall Studio latency
or production memory/frame budgets. See
[current evidence](../WORK.md#current-return-path-indexed-structured-value-reads--2026-10-07).

`AsText` and `AsBoolean` require matching kinds. `AsInteger` requires a
signed 32-bit integer spelling; fractions and exponent forms are not coerced.
`AsNumber` explicitly converts a numeric value to approximate IEEE Double.
Use `AsDecimal.Text` for the exact admitted representation:

```pascal
LExternalID := NyxData(NyxDecimal('9007199254740993'));
LPreciseRatio := NyxData(NyxDecimal('1.234567890123456789'));
```

Decimals preserve digits beyond Double's mantissa, exponent spelling and signed
zero. They remain within Nyx's finite numeric admission domain; this is a data
preservation contract, not an arbitrary-precision arithmetic engine. JSON
`ParseJSON`/`ToJSON` and store `LoadJSON` are explicit interchange boundaries.
The shared decoder retains numeric spelling through ordinary fpjson Clone.
Explicit fpjson setters/Clear discard that spelling and use its normal formatter.

Keys are exact, case-sensitive Unicode data, including empty or NUL-containing
keys. They do not use a name=value text list. A namespace reduces the risk of
future standard-field collisions. Scope protects recognized document fields
(version, title, state, pages, components) and node fields (kind, id, props,
children, bindings). Nested object members are application data and may use those
same names. Existing string-valued node `Configure.Extension` properties remain
a separate compatible boundary.

`SetValue`, `LoadJSON`, `Assign` and `Overlay` admit complete candidates
before mutation. Rejections retain entry order and values. `Remove` is a no-op
for an absent key. Stores have 1,019 extension members per owner, reserving five
standard fields within the shared 1,024-member JSON limit. JSON admission also
enforces valid Unicode, decoded duplicate-key refusal, 4 MiB UTF-8 and depth limits.
Export checks the complete design against import budgets: combining otherwise
valid payloads cannot produce an unreadable saved file.

Clone/view builds copy project data and each retained node's data. Reusable
instances overlay whole values by key, then named-part rules overlay the affected
part. Nested objects are replaced without implicit deep merging. Runtime changes
do not mutate definitions, sibling instances or authored overrides. A null value
is ordinary data, not an inherited-deletion sentinel.

Portable Studio `SetExtension`/`RemoveExtension` commands choose the document
or selected authored owner through `TNyxStudioExtensionOwner`. They admit a
detached design before publishing one history command. No-ops/rejections preserve
accepted handles and redo. Normal property edits, save/load and source export
retain this data without a separate extension editor.

Generated Pascal uses the same typed nested constructors with readable blocks;
it does not insert JSON blobs or framework-specific code. Native-generated source
executes on FPC and pas2js, preserving precision, NUL and reusable overrides.
The current evidence includes 64 shared data checks and complete generated-design
comparison on both runtimes. Structured observable state/collection bindings
and creator property/event schema registration now have their own consumer
contracts. See [named event producers](events.md#creator-defined-named-events)
for typed payload declarations, actual target factories and Studio/MCP authoring.
Measured indexing/update costs retain their separate performance owner.
