# Collection views and target controls

[Collection stores](collections.md) · [Scalar bindings](studio-state.md) ·
[Accepted collection foundation](../TODO/DONE/NS-1_state-collections_01.md)

The shared `INyxCollectionView` contract projects a typed collection into a list,
table or hierarchy. Browser and LCL renderers can attach it to an already mounted
Nyx control through `BindCollection`. The ordinary authored path uses the same
immutable specification through a specialized control's `Binds.Collection`.
Both renderers mount authored bindings automatically. Studio's Nyx-built Data
and Bindings panels edit saved defaults and those typed specifications.

```pascal
LTasksTable := NewNyxTable('tasks-table');
LTasksTable.Binds
  .Collection(
    NyxCollectionView(NyxCollection('tasks'))
      .Column(NyxTextField('title'), 'Task', cmEditable)
      .Column(NyxBooleanField('complete'), 'Complete'))
  .Done;
LPage.Add(LTasksTable);
```

Declare `LTasksTable: INyxTable` and import `nyx.controls`, `nyx.collections` and
`nyx.collections.view.types`. Saved collections are defined on
`LDocument.Collections`; their schemas must match the typed column references.
`Binds.ClearCollection` records explicit unbinding. `Binds.InheritCollection`
removes the local descriptor, restoring a reusable definition's binding. These
choices are distinct in clones, part overrides, source and paired history.

## Typed construction

```pascal
uses
  nyx.collections,
  nyx.collections.view.types,
  nyx.collections.view,
  nyx.collections.mount;

var
  LTasksView: INyxCollectionView;
  LTasksMount: INyxCollectionMount;
begin
  { The application owns an independent runtime registry. The view retains its
    store; the renderer owns the attachment to the mounted NyxTable control. }
  LTasksView := NewNyxCollectionView(
    LApplication.Collections.Collection(NyxCollection('tasks')),
    NyxCollectionView(NyxCollection('tasks'))
      .Column(NyxTextField('title'), 'Task', cmEditable)
      .Column(NyxBooleanField('complete'), 'Complete', cmEditable)
      .Column(NyxIntegerField('priority'), 'Priority'),
    cpTable);
  LTasksMount := LApplication.View.BindCollection('tasks-table', LTasksView);
end;
```

`NyxCollectionView` returns an immutable fluent specification. Its four `Column`
overloads require the matching text, Boolean, integer or number field reference.
Completed specifications contain 1–64 distinct columns. The collection key and
field families must exactly match the admitted store schema. Closed choices use
`TNyxCollectionScope`, `TNyxCollectionProjection` and `TNyxCollectionCellMode`.

`ToData`/`FromData` preserve the canonical version-1 specification for single
selection, with key, scope, optional parent field and ordered columns. Explicit
multiple selection uses version 2 and its closed `selection` member. The document
envelope remains version 3. A null packet means absent;
unknown fields, choices, versions and duplicate columns reject. Design version 3
reserves `collectionView` for this packet on a node. Version 1/2 fields with that
name remain opaque extension data; a collision rejects promotion to version 3.

Lists display the first column and expose stable item selection. Tables display
all declared columns. Trees display the first column and require a text parent
field supplied through `.Parent(NyxTextField('parent'))`. Table/tree cells marked
`cmEditable` can commit edits; read-only cells reject writes. List editing can
use another form or the typed view API.

## Selection, edits and failures

`Select` accepts a collection-scoped `TNyxItemRef`. Single selection remains the
default. Import `nyx.collections.selection` and choose `.Selection(nsmMultiple)`
on the view specification to admit multiple identities:

```pascal
LTasksList := NewNyxList('tasks-list');
LTasksList.Binds.Collection(
  NyxCollectionView(NyxCollection('tasks'))
    .Column(NyxTextField('title'), 'Task')
    .Selection(nsmMultiple)).Done;

LTasksView.Select(LFirstTask);
LTasksView.Select(LReviewTask, nsaToggle);
LTasksView.Select(LNextTask, nsaFocus);
```

The managed immutable `Selection` snapshot exposes `Count`, `Contains`, `ItemAt`,
`Focus`, `Anchor` and `DataRevision`. Membership, keyboard focus and range anchor
are independent scoped identities. Items are returned in admitted dataset order.
`Selected` remains the compatible single-item convenience; it prefers the focused
selected item, then the first member, and rejects empty membership.

`SetSelection` admits the complete candidate before publishing once. Duplicate,
unknown or foreign identities, invalid focus/anchor, or multiple members in Single
reject atomically. Equal membership/focus/anchor emits no duplicate notification.
`nsaRange` replaces a range; `nsaAddRange` keeps existing members. Programmatic
ranges follow dataset order. Target adapters pass the actual visible hierarchy
to `SelectRange`, so collapsed descendants do not enter a keyboard range.
`SelectAll` requires Multiple and includes the whole dataset. `ClearSelection`
removes membership, focus and anchor.

Selection survives value updates and reorder operations. Removal prunes missing
members; a removed focus may move to a nearby cursor without selecting it. A
retained snapshot still owns its earlier identities and retains no dataset,
widget or receiver. Each bound view owns its selection independently, including
views sharing an application store. Application page navigation preserves those
views. Native grid cell/editor position stays separate from row membership.

Actual browser/LCL list, table and tree controls support arrows, Home/End,
modifier-assisted focus, toggle/range/additive-range and select-all. Trees support
left/right parent, child and expansion navigation. Canonical before-key callbacks
run first and can consume a default. Read-only data permits selection; disabled
controls refuse forced gestures. Browser rows expose their selected state.
Lists/trees retain a visible row entry; tables have one data-cell Tab entry and
grid/row/gridcell semantics, with row/column dimensions and cell indices.

These choices are mapped to the W3C [listbox](https://www.w3.org/WAI/ARIA/apg/patterns/listbox/),
[tree view](https://www.w3.org/WAI/ARIA/apg/patterns/treeview/) and
[grid](https://www.w3.org/WAI/ARIA/apg/patterns/grid/) guidance. The current adapter
uses the modifier-assisted selection model. Full pattern qualification remains
open, including paging, cell/column selection, assistive technology checks and
physical-device testing. This delivery does not establish blanket accessibility
conformance.

`NyxCallbacks(LTasksList).OnSelectionChange` authors multiple ordered handlers and
their normal scheduler policy. Runtime subscriptions use
`LEvents.OnSelectionChange(NyxControlEvents('tasks-list')).Subscribe(LCallback)`.
Callbacks receive `HasCollectionSelection=True` and immutable typed
`SelectionBefore`/`Selection` value snapshots, including focus and anchor. Value
snapshots accommodate the matched pas2js compiler's restriction on managed
interface fields in records. The live view still returns a managed interface.
Notifications follow accepted publication, cannot consume it, and retain semantic
compound source/origin routing. Teardown revokes borrowed producers before widgets
are freed. Studio's Bindings choice, Events cards, source templates, paired
undo/redo and bounded semantic agent event discovery consume the same contract.

The ordinary edit path is typed:

```pascal
LTasksView.Edit(LTasksView.Selected, 0,
  TNyxStateValue.FromText('Review the interface'));
```

The supplied value must match the column family and the declared store domain.
`Apply` admits a complete batch before publication. `EditWire` and attachment
`EditCell` are explicit adapter boundaries: text from a target editor passes
through the matching scalar parser and domain before changing accepted state.
`TNyxCollectionColumn.Read(Item)` reads that column's exact typed scalar; missing
fields or incompatible families reject. Adapters compare accepted scalars per
cell, preserving an unfinished draft across selection and unrelated publications.
Completed no-op or rejected edits restore only their addressed item/column.
Browser editors retain focus through keyed row relocation; native editors retain
their draft when selection or another row's values change.

An attachment exposes `Failure` and `ErrorText`. Rejected input returns false
and restores the accepted display. An observer failure after publication reports
`nbfNotificationFailed` and returns true, because the change was committed.
Successful edits clear the previous diagnostic. Notification failure never
promises a rollback of an already published snapshot.

Browser text retains embedded NUL directly. LCL displays text containing NUL as
a JSON-quoted string and decodes its edited representation at the adapter
boundary. Other text is displayed normally. Exact Unicode remains in the store.

## Hierarchies and reusable instances

An empty parent value identifies a root. Child rows may precede their parents.
Every nonempty parent must resolve in the same collection, and the complete
candidate must be acyclic. An iterative validation walk avoids recursive Pascal
stack growth. An atomic batch may remove a subtree or change several parents;
admission evaluates the final candidate before any control update.

`NewNyxCollectionContext` captures independent authored defaults and exposes an
application registry. `Resolve` uses a view specification and the actual runtime
reusable-owner ID. Application scope returns the shared application store.
`.Scoped(csInstance)` resolves an independent store for each exact owner/key
pair, seeded from authored defaults. Repeated resolution reuses that store;
other instances and application mutations do not change its defaults. Empty
instance owners reject, and a context admits at most 1024 local owner/key pairs.
Composition propagates the nearest reusable owner's runtime ID automatically,
including nested instances and override payloads. A standalone page/view uses
its root ID as the local owner. Scope IDs are runtime metadata and never enter
saved designs or generated source. Manual `Resolve` remains available for custom
runtime consumers.

Applications retain one `INyxCollectionBindings` controller per page in a shared
context. Hidden pages continue validating data; navigation preserves edited
application/instance stores and each view's selected item. A standalone renderer
creates an independent context. Supplied controllers must match the realized
IDs, scopes, specs and projections before replacing accepted controls.

## Ownership and target behavior

Views retain their stores and accepted snapshots. Their store subscription and
observer tokens disconnect deterministically without a reference cycle. Multiple
observers execute in registration order; cancellation can suppress a later
observer in the same dispatch. Mutations and new subscriptions during dispatch
reject. A nil change argument denotes a selection-only notification. Receivers
are borrowed: disconnect their tokens before disposing receivers.

An attachment retains its view and borrows its target control. The renderer
disconnects attachments before removing controls during unmount. Retained
attachment interfaces then remain safe to inspect and disconnect, with
`Connected = False`; accessing their released view rejects. A new render needs
a new attachment; application controllers retain their view through navigation.
Only one collection attachment may own a mounted control. `CollectionView(ID)`
returns a retained automatically mounted view by exact runtime identity.

Root/ancestor enabled and read-only settings propagate into generated editors.
Read-only permits selection while rejecting edits; disabled controls reject
selection and editing. Browser design mode disables data editing. Physical
callbacks enforce this policy even when invoked programmatically; application
code can still edit its independent store through the typed runtime API.

## Studio and persistence

The project **Data** panel creates collections, adds fields from four typed
families, edits field defaults and saved row values, and adds/removes rows.
The selected list/table/tree's **Bindings** panel chooses a collection, data
scope, column titles/editability and tree parent fields. Columns can be added
or removed, and bindings cleared or inherited. These are undoable commands on
detached candidates; invalid hierarchy/schema edits and removal of used defaults
retain the accepted design/source pair. Escaped text editors preserve NUL and
supplementary Unicode. Field/collection names are currently assigned by Studio;
custom names, domains and ordering can be authored in the typed Pascal workspace.

Authored view bindings select design version 3. Collection defaults without view
bindings retain version 2; ordinary scalar-only designs retain version 1. Earlier
opaque node fields named `collectionView` remain extension data. Promotion with
a conflicting typed binding rejects rather than reinterpreting that data.
Version 3 uses the strict, independently versioned view specification packet.
Generated source uses specialized managed controls and fluent objects/enums;
visual edits preserve admitted column expressions/comments.

The browser adapter keeps keyed row elements and editor identity during updates.
It restores focus with `preventScroll` after structural relocation and retains
Nyx's registered root callbacks. Row and editor keys reach the owning control's
keyboard registrations; a consumed key suppresses default row selection. Active
command/callback frames retain their attachments and views until returning, even
when an observer releases the last owner or unmounts the target. The LCL adapter
uses actual `TListBox`,
`TStringGrid` and `TTreeView` widgets. Tree nodes retain identity across edits and
relocation; existing widget selection/edit handlers are restored at disconnect.

Bound browser collections offer one Tab entry. Lists/trees use arrows, Home and
End among visible rows; selection and keyboard focus are separate. Removing the
focused row transfers physical focus to the surviving model cursor. An empty
collection remains reachable, while a disabled collection has no Tab entry.
Restoring focus uses `preventScroll` and never takes focus from another control.
Collapsed tree descendants stay outside the visible navigation order; leaf
nodes do not advertise a collapsed parent.

## Bound table cell navigation

Browser and LCL tables share typed `TNyxGridMove` intentions and zero-based
data-cell positions; the header is excluded from navigation. Left/Right move
within a row, Up/Down retain the column, Home/End address that row's endpoints,
and Control+Home/End address the first/last data cells. Edges clip without wrapping.
Horizontal movement preserves row membership. Vertical movement keeps the
existing modifier model: Shift extends its anchored row range, Control moves
focus only, and Control+Shift extends additively. Cells and columns do not become
selection members. Cursor columns are runtime presentation, never saved defaults.

Enter or F2 addresses the current cell's editor. A noneditable column does not
redirect to a different column. Active text editors retain ordinary caret keys.
Browser Tab/Shift+Tab visit eligible editors in that row; Tab at a boundary leaves
through ordinary host traversal. Escape restores an uncommitted browser draft
before blur and returns to that exact cell. F2 also restores cell navigation,
using normal blur/validation. Read-only browser text remains inspectable; a
read-only checkbox uses disabled HTML behavior. LCL retains its standard editor
validation/discard and widget Tab behavior; read-only grids refuse editor entry.

The browser grid has one data-cell Tab entry; editors and rows add none while
navigating. The cell itself receives focus, so readonly columns remain reachable.
Per-cell `aria-readonly`, column indices and row indices include the accessible
header. Native initial columns fit admitted content using LCL font measurement;
later publications retain user widths and active drafts. This is presentation
only. Canonical key callbacks can consume defaults; selection callbacks can
retire the view safely on both targets.

The ordinary exact MCP-authored English table passes 28 checked Win32 controls
and 43 HTTP browser controls per desktop/CSS-390. Trusted Chromium keys and
Tab/Shift+Tab add 30 host checks per width, including current numeric entry,
Escape discard, editor boundaries and exact row membership. Selection regression
passes 155 native and 182 browser checks. See
[the current packet](../WORK.md#current-return-path-bound-grid-cell-navigation--2026-10-07).
The [WAI grid pattern](https://www.w3.org/WAI/ARIA/apg/patterns/grid/), rechecked
2026-10-07, guides this contract. Paging, cell/column selection, virtualization,
sorting/filtering, hardware/IME/assistive technology and other widgetsets/DPI
remain open. Host emulation and controlled LCL messages do not prove full APG
conformance or physical Android input.

## List and tree typeahead

Bound lists and trees share a portable managed `INyxTypeAhead` engine. Typing a
printable Unicode scalar searches the first displayed column, beginning after
the current focus and wrapping once. Repeating a single letter cycles through
matches. A rapid extended prefix first tests the current match. The default
window is one second, measured on a monotonic clock. No match leaves selection
unchanged. Search uses the actual visible tree order, excluding descendants of
collapsed branches. A match replaces membership and moves focus, consistent
with the existing modifier-assisted selection model.

Unicode 17 default full case folding supplies locale-independent comparison,
including supplementary characters and one-to-many mappings. Authored text
stays exact; folding neither normalizes nor removes accents. The pinned data,
license and Pascal regeneration path are described [here](../data/unicode/README.md).
The engine bounds transient state to 64 typed scalars. Search streams only the
required label prefix; controls still materialize their complete dataset.

Runtime policy uses an immutable fluent value:

```pascal
uses nyx.typeahead;

LDestinations := LRenderer.CollectionMount('destination-list');
LDestinations.ConfigureTypeAhead(
  NyxTypeAhead.WindowMilliseconds(800).Match(ntmFolded));
```

`Enabled(False)` disables search; `ntmExact` preserves case. A policy requires a
factory-defined value and a window of 1..60000 milliseconds. Invalid replacement
refuses before changing the accepted search. Each mount owns its own engine and
borrows the pure label reader only during search. Retained mount handles report
disconnected after unmount and refuse configuration. Prefix/focus/time never
enter saved documents, stores or Undo history. Dataset revisions, navigation,
effective interaction changes and external model cursor changes reset the buffer;
selection refresh alone preserves it. Persisted/Inspector policy authoring is
still an open Studio capability, rather than a new unvalidated wire field.

Control/Meta shortcuts, Alt/AltGr text, IME composition, cell/label editors and
consumed key callbacks retain their host ownership. Space keeps its existing
toggle-selection meaning, so spaces are not part of a search prefix. Read-only
controls permit selection; disabled controls preserve it. Native adapters use
the admitted UTF-8 character callback, never translate a virtual key into text,
and restore the previous callback at disconnect. A queued character following
consumed KeyDown is also consumed. Browser focus reveals the matched item using
the host's ordinary focus scrolling; selection callbacks may retire the view
before that focus step.

This follows [listbox](https://www.w3.org/WAI/ARIA/apg/patterns/listbox/) and
[treeview](https://www.w3.org/WAI/ARIA/apg/patterns/treeview/) typeahead guidance.
Current checked Win32 controls pass 67 assertions with zero leaks, and real
browser consumers pass 68 at CSS widths 1076 and 576. They consume the identical
English MCP-authored companion. Synthetic DOM events and native callback routing
qualify the default's semantics, not trusted hardware, IME, assistive technology,
other widgetsets or complete APG conformance. Literal unbound items and grid
typeahead are outside this bound list/tree packet.

To reproduce, create an independent empty `nyx_reviews` workspace at the current
primary revision. Apply the `layout` group in
[the review recipe](../tests/typeahead-review.operations.json) through
`nyx_transaction`, then its `collections` group through `nyx_collections`, each
at the fresh exact revision with a unique operation ID. Export accepted
`nyx_source` windows of at most 60 lines at one revision to
`build/typeahead/source/nyx.generated.view.pas`, preserving exact lines. Run
`tools/build.ps1 -Target typeahead`; `-TypeAheadSourceDirectory` selects another
semantic export. Serve the resulting browser fixture through an already-owned
static host, and qualify it using the Pascal browser capture/input harness.
Discard only the owned review at its exact current revision when finished.

All rows are currently materialized. Native lists rebuild their item text, tables
visit all visible-model cells, and tree structure changes relocate nodes.
Selection and scalar tree changes preserve the existing hierarchy. A refresh
count describes completed adapter passes, not browser frames or native paints.
Virtualization, incremental large-data work and documented frame/memory budgets
retain their owner in [extension/performance](../TODO/NS-3_extension-performance_01.md).

## Studio collection authoring

The Project Data section edits saved typed schemas, defaults and rows. The
selected list/table/tree exposes its collection binding through the Inspector's
Bindings panel. The seventeen existing operations now capture immutable Pascal
intent before source synchronization: collection/row/field identity, scalar
family, notation, projection and enum choices travel independently of widgets.
View column order may differ from schema order; edits follow the exact field.

Ordinary Studio controls submit that intent to the shared isolated command queue.
An admitted candidate publishes source and design together as one Undo entry.
Newer waiting scalar/title/choice text stays visible; structural changes lock the
exact collection or authored view before repaint. Rejected numeric text returns
to its accepted value while retaining the field's focus. Reusable overrides,
later navigation, independent Pascal drafts and new loads retain their existing
ownership guards. Pending presentation is not admission: missing/mismatched
typed metadata keeps the last displayable binding until replay reports its
diagnostic. Renderers do not borrow a worker's document or mutable store.

The current native journey qualifies these ordinary controls and exact emitted
companion; browser Studio, its module worker and asynchronous DOM journey
compile. Current browser execution/observing deployment, comfortable large-project
editing, wider widget metrics and accessibility retain their original gates.
Structured collection semantic operations remain with the existing
[agent workflow owner](../TODO/NS-4_agent-workflows_01.md); the active MCP release
is not changed by this inspector integration. See [WORK.md](../WORK.md).

## Reproducible checks

`tools/build.ps1 -Target collection-views` compiles the portable view fixtures,
both adapters, their real-control journey and the mounted-control benchmark.
The browser `collection-views.html?host=1` journey runs inside an exact 390-pixel
iframe. This is viewport emulation, not physical phone testing.

`tools/build.ps1 -Target collection-authoring` verifies saved binding admission,
composition, paired source/history, automatically mounted browser/LCL controls,
Studio commands and compiled application/page/component consumers. It also checks
Unicode packet/envelope reconstruction and fifty intended type errors per compiler.
`tools/build.ps1 -Target selection` prepares a crafted companion, executes the
actual native selection journey, compiles/repeats it and compiles the same browser
consumer. Execute `selection.html` to qualify that browser journey. All artifacts
can be staged with `-BrowserOutput` while Studio remains live.

Execute `collection-authoring.html`, `collection-authoring-generated.html` and
`collection-unicode.html` after compilation to check the browser consumers.

`tools/build.ps1 -Target collection-inspectors` runs typed isolated request/reply
checks, source regressions, pending refusal presentation, real standalone native
controls, the existing collection authoring consumer and the exact compiled
companion with its native table callback. English desktop/390 captures belong to
its `controls/` directory. The `browser/` directory stages Studio, its matched
Pascal module worker and the four `collection-inspector*.html` qualification
hosts. The build command launches no listener and replaces no observing project
or live service. Compilation alone does not qualify those browser journeys.

`collection-view-benchmark.html` measures mounting and twenty committed integer
updates with a list, table and four-way tree simultaneously attached, at 512 and
4096 rows. Correct values, revisions, row counts, subscription disposal and
refresh counts gate output. Setup excludes seed construction. Update time
includes store publication, view validation and target synchronization. Native
widgets run in a hidden form; browser timing uses `performance.now` without a
virtual-time budget. Neither measurement includes a paint/interaction latency
guarantee or establishes a production frame budget.
