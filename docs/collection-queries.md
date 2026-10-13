# Typed collection queries

[Collection views](collection-views.md) · [Stores](collections.md) ·
[Current integration evidence](../WORK.md#current-return-path-bounded-collection-query-mcp--2026-10-07)

[Generic LINQ sequences](linq.md) also support ordinary application values and
owned executable callbacks. Their snapshot entry point complements this
serializable bound-control policy.

Studio's query editor has **Preview project defaults**. It captures the current
form without applying it, displays complete matching counts and a bounded row
sample, and shows the same typed Pascal expression used by companion generation.
Malformed drafts show a refusal while preserving their text. The last report is
a copied snapshot, labelled with its source revision; further form edits require
another Preview. Project/selection/view/revision changes retire that presentation.
Accepted design/Pascal, unfinished source/base and Undo/Redo remain unchanged.

After a report is captured, **Expand preview** opens a read-only Nyx workspace
through the public modal host. The same copied report uses available viewport
space, an independent palette and a typed 16-pixel font metric. Close or Escape
dismisses it; another Preview is required to capture newer form text. Selection,
page, project or accepted revision changes retire the report and close an open
reader. Expansion never installs a query or adds history.

Library hosts can reuse `NewNyxQueryPreviewReader` for modal or embedded report
presentation. Its parts are specialized Nyx row, label, button and code editor
controls. The caller owns the interface/document and routes Close; the reader
copies text and holds no session/store reference. Studio uses the same component
on browser and LCL. Code editors honor the typed theme font metric on both targets.

`nyx.collections.query.preview` exposes the same read-only evaluation for library
callers. `PreviewNyxCollectionQuery` accepts an immutable source, bound view,
projection, candidate policy and `TNyxQueryPreviewOptions`. Default limits are
5000 source rows, 12 displayed rows and 32768 report scalars; fluent copies let
callers choose their own admitted bounds. Wrong fields/collection, malformed tree
parents/cycles, source/output limits and query failures refuse before a report
returns. Tree results include required ancestors; disclosure/paging remain runtime
presentation. Source/store/document/widgets are not retained by the returned value.

`EvaluateNyxCollectionQueryPreview` provides the shared bounded immutable result
snapshot for callers that need structured rows rather than a formatted report.
It admits the same source, fields, hierarchy and query before returning; it owns
no mutable store, document or widget and publishes nothing to a runtime binding.

In current source, `nyx_collections` has a read-only `preview` mode. Supply an
exact owner, `source` (`local`, `effective`, or `restorable`), `expectedRevision`,
a typed `query` descriptor and one `section`. A null query explicitly previews
unfiltered defaults; omission is refused. The three sections keep responses small:

- `summary` returns source/matching counts and identity, with no rows or Pascal.
- `rows` returns eight IDs by default, at most twenty per page. Optional `fields`
  selects up to sixteen exact schema names; text cells show eighty Unicode scalars
  with full scalar count/truncation, while numeric/Boolean values remain typed.
- `pascal` returns an exact Unicode scalar window of the generated expression:
  default `count` 2048, maximum 4096, with `offset`, `nextOffset` and full length.

Each section re-evaluates at the required revision, scanning at most 5000 authored
source rows. Source/owner/revision/workspace identity is explicit; tree counts
include required ancestors. It never installs the candidate, evaluates a live
runtime store or applies disclosure/paging. Read-only access is sufficient and
unfinished Pascal, navigation and history are retained. Unknown/cross-section
arguments, incompatible schemas, stale revisions and exceeded bounds refuse.
Ordinary mutation remains a separate grouped query edit with its existing guards.

```json
{
  "mode": "preview",
  "owner": "tasks-table",
  "source": "effective",
  "expectedRevision": 7,
  "query": null,
  "section": "rows",
  "limit": 8,
  "fields": ["caption", "priority"]
}
```

`tools/build.ps1 -Target semantic-query-preview` builds/runs checked shared
native and suspended real MCP routing/persistence consumers, then stages the
matched browser consumer. `-GridEvidenceDirectory` chooses a fresh evidence root.
It also exports the exact five-change qualification builder, compiles it with
FPC and pas2js, and compares its executed complete design and Boolean cells with
the admitted seed. Grouped and direct authoring must agree; source admission
alone is not evidence that the generated unit compiled or executed.
Execute `semantic-query-preview.html` over an admitted HTTP host to qualify the
browser; no listener or user project is started by the build. Current-source
discovery and the durable wrapper are separate from live HTTP authentication,
observing input and installed tool rollout. `WORK.md` records those distinctions.

The ordinary browser inspector has a maintained trusted-input journey:
`tools/build.ps1 -Target query-studio-browser -GridSourceDirectory <source>`
builds its stock Studio controller, input host and MCP companion. Export the
companion with `nyx_collection_companion <config.toml> <new directory> preview`
to author text/numeric saved policies and two pages as one semantic command.
The owning review is exported, checked with paired Undo/Redo and discarded;
primary work is retained. Targets and application compilers are not prerequisites.

Serve `query-studio-browser.html` with its matching generated JavaScript/RTL on
an admitted host. Run `nyx_query_studio_input <URL> <new capture directory> <width>`
at desktop and compact widths. Trusted host input edits query fields and clicks
Preview; selection/navigation stay semantic. The ordinary controller receives the
exact compiled companion through its public asynchronous editor exchange and
retains an independent pending source draft and existing Redo. Its exchange is
in-process: this does not claim authenticated HTTP preview or installed rollout.
Mobile uses exclusive panes, so canvas observations explicitly visit Design and
return to Inspector. Captures qualify layout and host input, not phone hardware,
IME, assistive technology or another widgetset. The expanded reader supplies the
wider report presentation when the desktop inspector is narrow.

Sample text preserves printable Unicode and visibly escapes NUL/control scalars.
Cells and display identities clip on scalar boundaries, while the Pascal policy
keeps exact admitted names/values. `TNyxCodegen.QueryExpression` shares the ordinary
emitter and admits formatting indentation only. This preview explores authored
defaults, rather than a running application's independent store. Generic LINQ and
parallel application callbacks keep their separate executable contracts.

`tools/build.ps1 -Target query-preview -GridSourceDirectory <semantic source>
-GridEvidenceDirectory <new evidence directory>` qualifies native controls and
ordinary native Studio, then compiles browser controls/ordinary Studio. HTTP
execution remains explicit. Desktop/CSS-390 browser fixtures acknowledge the
visible report before disposal. The later ordinary browser input journey above
qualifies the stock embedded controller. Live observing HTTP and installed
semantic preview retain their existing acceptance owners.

`nyx.collections.query` supplies immutable filters and stable ordering for bound
lists, tables and trees. The portable contract contains no DOM/LCL types, locale
dependency or executable predicate callback. Application fields remain distinct
typed references; comparisons take Boolean, Integer, Double or `TNyxText` values.

```pascal
uses nyx.collections, nyx.collections.query;

var
  LTitle: TNyxTextFieldRef;
  LComplete: TNyxBooleanFieldRef;
  LPriority: TNyxIntegerFieldRef;
  LPolicy: TNyxCollectionQuery;
begin
  LTitle := NyxTextField('title');
  LComplete := NyxBooleanField('complete');
  LPriority := NyxIntegerField('priority');

  LPolicy := NyxCollectionQuery
    .Where(
      NyxWhere(LComplete).EqualTo(False)
        .AndAlso(NyxWhere(LPriority).AtLeast(2)))
    .OrderBy(LPriority, nsdDescending)
    .ThenBy(LTitle, nsdAscending, nqtAsciiInsensitive);
end;
```

`INyxCollectionPredicate` is an immutable managed interface. `AndAlso`, `OrElse`
and `Negated` compose independent trees. A field facade returns the matching
predicate family; it cannot apply text search to an Integer or numeric ordering
to a Boolean. `Where` replaces a query's filter. `WithoutFilter` clears only its
filter; `Unsorted` clears only ordering. `OrderBy` starts a new ordering and
`ThenBy` adds a distinct secondary field. Secondary ordering without a primary,
duplicate sort fields and default/uninitialized sort references refuse.

| Field family | Predicate methods |
| --- | --- |
| Text | `EqualTo`, `NotEqualTo`, `Contains`, `StartsWith`, `EndsWith` |
| Boolean | `EqualTo`, `NotEqualTo` |
| Integer / Number | `EqualTo`, `NotEqualTo`, `LessThan`, `AtMost`, `GreaterThan`, `AtLeast` |

Text uses Unicode scalar ordering/search. `nqtExact` is the default;
`nqtAsciiInsensitive` folds A–Z only. Other scripts, accents and combining
sequences remain exact. Neither mode normalizes text or promises locale-aware
dictionary ordering. Contains/prefix/suffix accept an empty needle. Numeric
comparison uses the admitted scalar family and never parses display text.
All four field families support `NyxSort`/`OrderBy`/`ThenBy`; Boolean orders
False before True, and Numbers retain the shared finite-Double contract.

## Authored defaults and independent runtime policies

An authored binding can include a reusable default:

```pascal
LTasksTable.Binds
  .Collection(
    NyxCollectionView(NyxCollection('tasks'))
      .Column(LTitle, 'Task', cmEditable)
      .Column(LPriority, 'Priority', cmEditable)
      .Selection(nsmMultiple)
      .Query(LPolicy))
  .Done;
```

A live `INyxCollectionView` copies that default. `ConfigureQuery` fluently
returns the view after admitting a complete policy. It changes runtime view
state, not document defaults, the source store's order or source revision:

```pascal
LTasksView.ConfigureQuery(
  LPolicy.Where(NyxWhere(LTitle).Contains(LSearchText, nqtAsciiInsensitive)));

// Restore the complete source without clearing selected item identities.
LTasksView.ConfigureQuery(NyxCollectionQuery);
```

Different views over the same store have independent policies. The result
snapshot retains stable item references and the actual source revision; a query
change can produce a different result at the same revision. Adapters must use
snapshot identity/order, not source revision alone, when refreshing controls.
Result lookup remains indexed. The projection retains the source, so `DataBytes`
reports retained source payload rather than discounting hidden rows.

Equal ordering keys retain source order, including descending sorts. Multiple
keys use a stable merge with cached typed scalar keys. Tree filtering includes
matching rows and their required ancestors; it does not include arbitrary
descendants of a matching parent. Parent indexes are remapped into the result,
and child-before-parent sources remain valid. Cycles or missing parents refuse.

Filtering retains selected source identities. Only a source removal prunes them.
An invisible keyboard focus moves to a nearby visible row; an empty result has
no focus. Anchors can remain hidden. Focus/toggle/additive range preserve hidden
membership, while replacement range and `SelectAll` replace membership with the
admitted result. `SetSelection` can include hidden members but requires visible
focus. `Selected` can name a hidden member; `CellText` still reads accepted text
for that source identity. Range positions use query order, not mutable source-store positions.

Sorting preserves an active unchanged scalar editor's item, draft and caret on
both adapters. Hiding its item ends editing and discards that uncommitted draft.
An accepted scalar change normalizes the editor instead of preserving stale text.

## Ownership, admission and persistence

Query records explicitly copy their sort arrays. They retain a closed immutable
predicate descriptor because pas2js cannot put COM interfaces in record fields.
`Filter` returns a separately owned interface tree; an evaluator retains it once
for the complete evaluation. The library projector does not parse a predicate
per row. External predicate implementations are normalized through `ToData`,
so attached policies retain no external callback/owner or reference cycle.

Wrong schema field families and missing fields reject before a view changes.
Invalid policy admission leaves its prior query, snapshot, selection and observer
count intact. View mutation is confined to the UI thread; reentrant mutations
refuse. Query/selection notifications have nil collection `Changes`; source
publications still carry their complete immutable change log. Observer failure
is reported after publication under the existing collection notification contract.

The closed wire boundary allows at most 64 predicate nodes, depth 16, eight
distinct sort fields and 65536 canonical UTF-8 bytes. Unknown members/operators,
inappropriate comparison families, unsupported enum names, wrong scalar types
and noncanonical empty objects refuse. A null query means an absent policy.

Query-free view descriptors retain their exact version-1 single/version-2
multiple encoding. A nonempty authored query uses view descriptor version 3,
with explicit selection and query fields. The outer document version remains
unchanged. Generated Pascal uses the public typed factories and readable logical
blocks; its admitted and ordinarily compiled builders reproduce the design.

`tools/build.ps1 -Target collection-query` checks the portable codec/source
contract, compiles its generated builder, runs actual Win32 controls using the
previous authenticated MCP companion, and stages both browser consumers. Supply
`GridSourceDirectory` for that existing export. Execute `collection-query.html`
and `collection-query-controls.html` over HTTP for browser evidence. This command
starts no listener and changes no project or Codex enrollment.

## Reusable query authoring form

`nyx.collections.query.editor` supplies `NewNyxQueryEditor`, a compound of
specialized Nyx controls. Studio consumes it in Properties → Bindings for a
bound list, table or tree. Applications can mount the same form:

```pascal
LQueryCard := NewNyxQueryEditor('task-query', NyxControl('tasks-table'),
  LTasks.Schema, LTasksTable.Node.CollectionView);
LInspector.Add(LQueryCard);

// Named parts retain the ordinary public configuration/customization contract.
LQueryCard.Part(NyxPart('field')).Configure.Text('Choose a task field').Done;
```

Choose a field to add a predicate initialized from its typed default. Existing
predicates offer only comparisons suitable for their family; Boolean input is a
checkbox, numeric text requires exact signed Integer/finite Number notation,
and text remains exact Unicode. AND/OR actions can target any predicate or
subtree. Toggle NOT wraps/unwraps that subtree; removal collapses an empty branch
or keeps its remaining child. Sort cards offer exact field choices, direction,
text matching and earlier/later/removal actions. Duplicate fields, inappropriate
text matching and policy budgets refuse the complete candidate.

`CaptureNyxQueryEditor` returns a value-only `TNyxQueryEditorChange`: exact owner,
schema/binding baseline and independent typed query. It changes neither tree.
Hosts compare `NyxQueryEditorBaseline` against their current binding/schema,
then admit the replacement through their own undoable candidate or apply it to
a runtime view. Save and structural actions capture all current form values;
clearing/removing an invalid subtree can recover it while unrelated invalid
input still refuses. Fields/binding/defaults/domains define the baseline;
unrelated row contents do not. Captions are bounded presentation; a separate
mapping retains exact Unicode field identities, including embedded newlines.

`TNyxQueryEditorDraft` reuses the owned scalar-form draft protocol. Partial text
survives an unrelated repaint or hidden panel. Changed owner, schema, binding or
project retires it. The draft borrows no node, renderer or document. Query forms
use typed viewport rules to reduce narrow padding without reducing text size.

Studio routes actual compound child origins into its independent source queue.
It captures one complete query replacement and rechecks its baseline on the
worker before publishing one paired document/Pascal Undo step. Pending structural
edits and handwritten Pascal drafts refuse. MCP discovery now describes recursive
typed query alternatives, query-bearing version-3 bindings and the existing
collection tool's `query` intent. JSON Schema describes shape/families; the
ordinary Pascal decoder additionally enforces total depth/nodes/bytes, finite
values and distinct sort fields. Authentication/observing qualification belongs
to the current backend workflow, not a schema-only fixture.

`tools/build.ps1 -Target collection-query-editor` consumes the existing exact
semantic grid export through `GridSourceDirectory`, checks physical Win32 forms
and ordinary native Studio, then compiles the HTTP browser form, source worker
and full Studio. Its outputs stay under an owned build directory; it launches no
listener and changes no active project or enrollment. Execute
`collection-query-editor.html` over HTTP with its matching worker/RTL and copied
companion source for browser evidence.

The form packet qualifies library policies, persistence/source and actual controls.
The subsequent bounded MCP workflow below qualifies authenticated current-backend
query edits and compilation. An observing full browser Studio journey remains
required integration. The existing deployed/frozen services are unchanged.
Paging, virtualization, broader
large-data budgets, production styling and hardware/IME/assistive-technology
qualification keep their original task owners.

## Bounded semantic query authoring

The existing `nyx_collections` tool now supplies `query` predicate pages and
`query-value` windows for an exact local/effective/restorable binding. Predicates
carry child paths and short scalar previews; ordering has at most eight typed
keys. Agents retrieve only the required expected value, rather than a full
recursive binding/schema/document. Every response reports the authoritative
revision. See [the tool contract](studio-agents.md#structured-collection-context-and-authoring).

Pascal consumers can replace only a binding's query:

```pascal
LChanges := NyxCollectionPatch([
  NyxSetCollectionQuery(
    NyxBindingOwner('tasks-table'), NyxCollection('tasks'), cpTable, LPolicy)]);
```

`NyxSetCollectionQuery` requires the exact effective owner, key and projection.
It preserves columns, scope, parent mapping and selection; an empty
`NyxCollectionQuery` clears filtering/ordering. Inherited bindings gain an
independently owned local override. A semantic caller submits the group's
`expectedRevision` and operation ID; candidate replay derives its current
schema/binding baseline and uses the same ordinary Studio admission. Related
row/default/query changes publish as one paired Undo step. An incompatible field,
late failure, pending Pascal draft or stale revision preserves the accepted pair.

`tools/build.ps1 -Target collection-query-workflow` runs the focused checked
native admission/context/history journey, compiles its pas2js counterpart with
matched RTL, and builds `nyx_grid_companion`/`nyx_query_companion`. Outputs stay
under `build/query-workflow/maintained/`. It launches no listener, changes no
configuration and authors no active project. First own a current backend with
its explicit private enrollment and compiler profile. The grid companion creates
an English project and exports its workspace handle; the query companion takes
that handle, a new evidence directory and that backend's editor URL:

```powershell
& './build/query-workflow/maintained/tool/nyx_grid_companion.exe' `
  '<explicit config.toml>' '<new grid export directory>'
& './build/query-workflow/maintained/tool/nyx_query_companion.exe' `
  '<explicit config.toml>' '<exported owned grid workspace>' `
  '<new query evidence directory>' '<editor URL without trailing slash>'
```

The authenticated companion inspects bounded context, groups a row/query change,
checks stale refusal and exact source Undo/Redo, then requests ordinary browser
and LCL application/view builds. Each terminal successful receipt is compared
with the actual HTTP compiler input and accepted source. The receipt explicitly
leaves `browserUIQualified` false: compilation does not execute either application
or establish observing editor/input behavior. `WORK.md` owns current results and
the separate browser readiness/performance gate.
