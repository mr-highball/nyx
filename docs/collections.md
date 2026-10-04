# Observable collections

[Architecture](architecture.md) · [Scalar state](studio-state.md) ·
[Accepted collection foundation](../TODO/DONE/NS-1_state-collections_01.md)

`nyx.collections` provides typed, ordered state for data controls. Its public
contract uses reference-counted interfaces, immutable schema/item values and
collection-scoped item identities. It has no DOM or LCL dependencies. Authored
registries, independent application stores, versioned persistence, generated
Pascal and paired source/history are integrated. Runtime list/table/tree bindings
and stable selection are documented in [collection views](collection-views.md).
Dedicated visual collection authoring and automatic authored-control mounting
remain open.

## Typed construction

Use distinct field references for each scalar family. A Boolean field cannot be
passed to a text operation, and a collection reference cannot substitute for an
item reference. Open application names remain text at the reference factories;
behavior and scalar arguments have Pascal types.

```pascal
uses
  nyx.collections,
  nyx.contract;

var
  LTasksKey: TNyxCollectionRef;
  LTitleField: TNyxTextFieldRef;
  LCompleteField: TNyxBooleanFieldRef;
  LPriorityField: TNyxIntegerFieldRef;
  LSchema: TNyxCollectionSchema;
  LDesignTask: TNyxItemRef;
  LTasks: INyxCollection;
  LBefore: INyxCollectionSnapshot;
begin
  LTasksKey := NyxCollection('tasks');
  LTitleField := NyxTextField('title');
  LCompleteField := NyxBooleanField('complete');
  LPriorityField := NyxIntegerField('priority');
  LSchema := NyxCollectionSchema
    .Text(LTitleField, 'Untitled task')
    .Boolean(LCompleteField, False)
    .Integer(LPriorityField, 1, NyxIntegerDomain.Range(1, 5));
  LDesignTask := NyxItem(LTasksKey, 'design');
  LTasks := NewNyxCollection(LTasksKey, LSchema);

  { Insertion materializes unspecified fields from their schema defaults. }
  LTasks.Append(NyxCollectionItem(LDesignTask)
    .WithValue(LTitleField, 'Design a thoughtful interface'));
  LBefore := LTasks.Snapshot;

  { One detached proposal, one publication, with an optimistic revision guard. }
  LTasks.Apply([
    NyxUpdate(NyxCollectionItem(LDesignTask).WithValue(LCompleteField, True)),
    NyxInsert(1, NyxCollectionItem(NyxItem(LTasksKey, 'review'))
      .WithValue(LTitleField, 'Review on both targets'))
  ], LBefore.Revision);

  { LBefore still contains the original unfinished task after either edit. }
end;
```

Text, Boolean, Integer and Number schema methods accept their matching optional
domain objects from `nyx.contract`. Defaults and admitted values follow existing
Nyx scalar validation: valid Unicode, portable integers, finite Double values and
the declared choices/range. There is no conversion between scalar families.
Typed factories supply an unconstrained domain of the matching family when no
extra constraint is specified. The explicit descriptor `Schema.Field` boundary
also permits `NyxNoDomain`: the default still declares the field's scalar type,
while the additional domain is absent. These meanings remain distinct on the
wire and in generated source.

Factories initialize references and schemas. `Default(TNyxCollectionRef)` and
other default reference/schema/edit records are invalid and rejected. Names and
item IDs are case-sensitive, contain 1–128 Unicode scalars, and refuse control
characters and all-whitespace names. Text values can retain embedded NUL and
supplementary Unicode; these identity restrictions do not restrict value text.

Schemas retain deliberate field order. Duplicate names are refused across
families. Fluent schema and item builders return independent values: extending a
schema or calling `WithValue` does not modify an earlier value or a live store.
The descriptor accessors expose tagged scalar values for inspectors and future
wire admission; ordinary authoring uses typed fields.

## Edits, snapshots and rejection

`Append`, `Insert`, `Remove`, `Move`, `Update` and `Replace` return the specialized
`INyxCollection` mutation interface. `Apply` accepts typed edit factories and
admits the complete candidate before publication. It evaluates edits in order,
so a later step can address a row inserted earlier in the same batch. Insert
positions span `0..Count`; move positions describe the final sequence after
removing the moving row and span `0..Count-1`.

`Update` changes supplied fields and retains the others. `Replace` materializes
unspecified fields from schema defaults. Unknown fields, wrong scalar families,
domain failures, duplicate identities, wrong collection scopes, unknown rows,
bad positions, stale revisions and budget failures reject the complete batch.
The accepted snapshot, revision and subscriptions remain unchanged.

`Snapshot` returns an owned read-only `INyxCollectionSnapshot`. Retain it across
later edits or store disposal. Row/schema reads return independent values; the
implementation can share its private immutable rows safely. `Item` requires an
existing identity, `IndexOf` returns -1 for a missing identity in the correct
scope, and `Has` tests existence. Wrong scopes are rejected in each operation.
Visible indexes can change while item references remain stable.

An admitted batch advances the revision once. Pure no-op operations are omitted
from its change log. A batch whose final dataset exactly equals its baseline
does not publish, notify or advance revision. Pass a nonnegative expected revision
to reject stale writes; -1 disables that comparison, and lower values are invalid.

`Assign` admits an entire read-only snapshot with the same collection key and
ordered schema. It reports removals followed by insertions when meaning changes.
It supports whole-dataset replacement without the 256-edit batch limit, while
enforcing the same item/payload limits. `Clone` creates an independent store at
the same revision and logical scope, with no subscriptions. Default builders,
registries and sibling application stores are tested for isolation; collection
bindings in actual reusable component instances remain an integration gate.

## Authored registries and runtime stores

`TNyxDocument.Collections` exposes managed `INyxCollectionDefaults`. Define a key,
schema and complete ordered item array, or pass an owned snapshot to its explicit
descriptor overload. Admission normalizes all rows and scopes before replacing a
definition; replacement retains its original position. Returned snapshots are
immutable and start at revision zero. Foreign implementations' reported byte
counts, revisions or mutable backing data are not trusted. Registry Clone forks
its mutable list while sharing only admitted immutable values.

`TNyxApplicationState.Collections`, `TNyxBrowserApplication.Collections` and
`TNyxLCLApplication.Collections` expose `INyxCollections`. Its keys come from the
document; `Collection(LTasksKey)` returns an independent mutable `INyxCollection`.
Applications seed revision zero without synthetic edits/notifications and retain
their registry through navigation. Runtime registry Clone retains current values
and revisions in fresh stores with no observers. Retained registries/stores have
no borrowed application/document/renderer pointer and may outlive those owners.

Authored defaults admit up to 64 collections and 8 MiB aggregate logical payload,
including empty datasets' schema/default metadata. Replace at the count limit is
permitted when the complete new payload fits. Each runtime store can grow within
its independent store limits; these are separate from the wire budget.

## Versioned designs and crafted source

Designs with typed collection defaults use version 2. Their `collections` field
contains a separately versioned descriptor: version 1 with an ordered definitions
array. Each definition has its key, ordered schema and ordered items. Fields have
names, tagged scalar defaults and explicit domain data. Row values follow schema
order. Text/Boolean/Integer retain JSON types; Number values use finite decimal
strings. Runtime revisions and subscriptions are not design persistence.

Designs without typed defaults retain version 1. An existing version-1 opaque
root extension named `collections` stays application data and is not reinterpreted.
Defining typed defaults while retaining that extension refuses validation/export
with a collision diagnostic, preserving both values for explicit resolution.
Version 2 recognizes the collection field while retaining other unknown root/node
data and the existing scalar rules. Unsupported descriptor versions, missing/
unknown fields, duplicate definitions/fields/rows, wrong scalar kinds, invalid
domains and row/schema arity differences reject complete detached admission.

The shared 4-MiB JSON byte budget still applies to the entire design/packet, along
with existing depth/member/Unicode/numeric admission. An in-memory dataset fitting
its 8-MiB logical budget may therefore exceed current export capacity. Export
rejects the complete oversized packet and retains in-memory values; it cannot
truncate rows or weaken the shared admission gate.

Generated Pascal uses `Result.Collections.Define`, typed schema/field/item
factories and fluent `WithValue`. Values equal to schema defaults are omitted
from row declarations. Defined domains keep typed range/choice expressions;
explicit absence uses `Schema.Field`, a typed scalar factory and `NyxNoDomain`.
The bounded builder grammar admits one definition per key, not arbitrary runtime
calls or collection locals. Ordinary helpers outside the frame remain compiler-owned.

Studio's shared source/session boundary admits typed collection source edits,
preserves crafted comments through visual changes, pairs exact design/source
undo/redo, and retains rejected drafts without changing accepted state or redo.
Isolated page/subtree/reusable documents preserve independent defaults; reduced
companions reproduce them. Dedicated visual collection editors, actual bound
controls and complete native Studio remain with their task owners.

## Observers and lifetime

`Subscribe` returns an `INyxCollectionSubscription`. Keep the token while the
subscription is needed and explicitly call `Disconnect` before freeing its
callback receiver. Disconnect is idempotent. Releasing the last token reference
also disconnects, but compiler-created interface temporaries can delay that
release until the containing routine exits. Tokens borrow the store; outstanding
tokens become disconnected when the store is disposed. They form no store cycle.

An optional validator receives the complete immutable candidate and owned
`INyxCollectionChanges` before publication. During validation, the store's
`Snapshot` still returns its baseline. Any validator exception refuses publication.
Observers run in registration order after publication. Every remaining observer
sees the committed dataset, even if an earlier observer throws. Observer failure
raises `ENyxCollectionNotification` after dispatch: the edit has already committed,
so callers must not interpret that exception as a rejected transaction or retry
the mutation blindly.

Callbacks may disconnect tokens, retain changes/snapshots, or release their
application's store reference. The mutation keeps its implementation alive until
dispatch finishes. Writes and new subscriptions from validators/observers are
refused to prevent recursive publication and unstable registration order. A
disconnected later observer is skipped. The store clears its dispatch guard on
every exit, including callback failure.

Changes retain `Before` and `After` datasets and ordered operation descriptors.
Each step exposes its kind, identity, before/after positions and row values at
that step. The absent side of an insertion/removal has index -1; asking for its
absent row raises an error. Consumers can replay the step log without borrowing
the store or callback stack.

Stores and subscriptions belong to their owning UI thread. They do not lock or
create workers. Scheduler-backed application code must marshal worker results
to that thread before writing. Browser and native notifications have the same
serial store contract; event execution policies remain the scheduler's concern.

## Budgets and measured costs

| Limit | Admitted maximum |
| --- | --- |
| Fields per schema | 64 |
| Items per collection | 16,384 |
| Edits in one Apply | 256 |
| Connected subscriptions | 1,024 |
| Logical collection payload | 8 MiB |

Payload includes the collection key, field names, defaults, domain descriptors,
row IDs, field names and UTF-8/scalar values. It excludes Pascal object/index
overhead, temporary candidates and retained historical snapshots; it is an
admission budget, not a bound on process memory. Schema metadata is budgeted even
for an empty store. Cached admitted row sizes avoid rescanning unchanged large
text values for every update.

Immutable snapshots use an identity hash index. Publication currently copies
the outer row vector and rebuilds that index: an update has an O(row count)
component. Batch editing reduces repeated publication. This is a documented
foundation cost, with production control update/virtualization work still owned
by [the component performance task](../TODO/NS-3_extension-performance_01.md).

`tools/nyx_collection_benchmark.lpr` verifies values, retained snapshots and lookup
checksums before reporting CSV. It measures setup in 256-row batches, twenty
single-field transactions and twenty thousand stable identity lookups. This
2026-10-03 checked-build observation is specific to the local machine/toolchains:

| Items | Native setup / 20 updates / lookups, ms | Browser setup / 20 updates / lookups, ms | Payload bytes |
| --- | --- | --- | --- |
| 512 | 0 / 31 / 0 | 27.2 / 60.1 / 24.1 | 15,853 |
| 4,096 | 157 / 125 / 0 | 238.7 / 390.7 / 27.3 | 130,053 |
| 16,384 | 2,187 / 516 / 15 | 2,529.5 / 1,449.1 / 27.6 | 529,653 |

Native `GetTickCount64` has a coarse observed clock quantum; zero means below
measurement resolution. Browser numbers use `performance.now` in headless Edge
with no virtual-time budget. They are measurements, not cross-platform speed
guarantees or production frame-time acceptance. No DOM/LCL mounting, rendering,
selection binding or virtualization is measured by this tool.

Run `./tools/build.ps1 -Target collections` for checked native execution and both
browser compilation targets. Serve `collections.html` and
`collection-benchmark.html` through Studio to execute their browser programs.
See [building](building.md) and [current evidence](../WORK.md) for the full suite,
heap checks and compiler rejection fixtures.
