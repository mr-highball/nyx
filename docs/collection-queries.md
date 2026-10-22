# Typed collection queries

[Collection views](collection-views.md) · [Stores](collections.md) ·
[Current integration evidence](../WORK.md#current-return-path-bounded-collection-query-mcp--2026-10-07)

[Generic LINQ sequences](linq.md) also support ordinary application values and
owned executable callbacks. Their snapshot entry point complements this
serializable bound-control policy.

## Prepared runtime query changes

The optional `INyxCollectionQueryPublication` facet preserves the original
`INyxCollectionView` interface/GUID. Obtain it through
`NyxCollectionQueryPublication(View)` in `nyx.collections.view`; the contract lives
in `nyx.collections.query.publication`, independently of renderers and controls.
Unsupported alternate views refuse rather than silently perform sequential
query/state writes. The public native consumer passes89 checks with a clean heap;
matching browser assertions compile only. Current qualification is recorded in
[WORK](../WORK.md#prepared-collection-query-publication--2026-10-21).

```pascal
LQueryOwner := NyxCollectionQueryPublication(LServicesView);
LCapture := LQueryOwner.CaptureQuery;
LPolicy := LCapture.Query
  .Where(NyxWhere(NyxTextField('status')).EqualTo('stopped'))
  .OrderBy(NyxNumberField('cpu'), nsdDescending);
LPreparedQuery := LQueryOwner.PrepareQuery(LCapture, LPolicy);

// APreparedState is an existing prepared scalar/state publication, for example
// resetting the visible Search and Sort values through their real state owner.
PublishNyxGroup([APreparedState, LPreparedQuery]);
```

Declare `LQueryOwner: INyxCollectionQueryPublication`,
`LCapture: INyxCollectionQueryCapture`, `LPolicy: TNyxCollectionQuery` and
`LPreparedQuery: INyxPreparedPublication`. Existing collection/query/state and
publication units supply the other values. The application owns its schema,
fields and prepared state; descriptor Text/Value setters are not live commands.

A capture owns copied query intent and an inert exact-view identity, not source
rows, a store or the view itself. A capture from another view refuses even when
its query is identical. Actual A→B→A query changes invalidate the original
capture; an equal ordinary ConfigureQuery remains a complete no-op and keeps it
valid. Row changes before Prepare are allowed. Preparation uses the current
source, retains that source and reserves its exact view through retirement.

The prepared API requires a prepared/atomic source. Query, selection, paging,
new subscriptions and source-row preparation cannot bypass its reservation;
disconnecting an existing observer remains safe. A simultaneous same-source
row-edit/query group explicitly refuses. Different views over the same unchanged
store may prepare queries together. Ordinary non-atomic ConfigureQuery retains
its existing immediate behavior, without claiming this coordinated capability.

Validation computes the full filtered/sorted/unpaged/page/selection/hierarchy
candidate. The final readiness fence reads private reservation/source/intent
facts only; installation adopts preallocated values and retains displaced leases.
Every participant installs before notifications. Later refusal leaves all models
unchanged, abandonment releases its reservation, and postcommit observer failure
remains committed while other independent observers are attempted. Repeated
Retire cannot undo an installed result. Equal prepared policies retain existing
snapshot identities and do not notify the query view.

This is model authority, not a grant to a mounted application's callbacks.
UI commands must still validate their current owner/event generation. Canonical
toolbar/header contributions, visible column controls, rich events and ordinary
Studio/save/generated authoring remain under
[column filter/sort02](../TODO/NS-3_control-usefulness_02.md). No new query parser,
store, scheduler or semantic server is introduced by this facet.

The maintained `tools/build.ps1 -Target query-publication` command checks the
native contract and stages the matching browser program. Supply a NEW
`-QueryPublicationEvidenceDirectory` on your scratch drive to retain its outputs.
It does not launch a host or execute the browser program.

## Shared filter and sort owners

`nyx.collections.query.session` defines an optional shared session on the same
live collection view. Obtain it with `NyxCollectionQuerySession(View)` from
`nyx.collections.view`. Its current qualification evidence
is recorded in [WORK](../WORK.md#shared-collection-query-sessions--2026-10-21).
The public native consumer passes93 checks with a clean heap, and matching
browser assertions compile only. It adds no table header controls or Studio
authoring for them yet.

```pascal
LServiceQueries := NyxCollectionQuerySession(LServicesView);
LHeaderOwner := LServiceQueries.AcquireOwner(
  NyxQueryOrigin('column-header'), 'Status column');
LToolbarOwner := LServiceQueries.AcquireOwner(
  NyxQueryOrigin('data-toolbar'), 'Service search');

LColumnChanges := NyxQuerySessionEdits
  .Filter(NyxQueryChannel('status'),
    NyxWhere(NyxTextField('status')).EqualTo('stopped'))
  .SortBy(NyxCollectionQuery.OrderBy(NyxNumberField('cpu'), nsdDescending));
LColumnPublication := LServiceQueries.PrepareSession(
  LServiceQueries.Capture, LHeaderOwner, LColumnChanges);
PublishNyxGroup([LColumnPublication]);
```

Declare `LServiceQueries: INyxCollectionQuerySession`, owners as
`INyxCollectionQueryOwner`, changes as `TNyxQuerySessionEdits`, and the prepared
publication as `INyxPreparedPublication`. Origins/channels are typed open
identities for metadata; they do not select behavior through magic strings.
The actual comparison and order use the existing typed query constructs.

The initial baseline is the current accepted query. Acquiring a second owner
joins the existing session; it never turns already composed user filters into
another baseline. Independent owner/channel predicates AND with that baseline.
One shared user sort vector replaces baseline sorting. An empty `SortBy` selects
unsorted source order explicitly; `ResetSort` instead restores baseline sorting.
Sort-only owners retain their label/origin in `SortOwner`, so peer surfaces can
show the current shared choice or Custom instead of a stale private caption.

`ClearFilter(Channel)` removes only that actor's slot. `ClearOwner` removes its
slots and resets sorting only while that actor still owns the shared vector.
`ClearAllFilters`, `ResetSort` and `ResetAll` are distinct explicit choices.
Enumerate bounded `Count`/`ContributionAt` metadata for filter chips;
`RemoveFilter(OwnerID, Channel)` explicitly removes an active or orphan chip.
An ID is presentation data, never command authority. Same-origin reacquisition
gets a fresh token and cannot implicitly adopt earlier contributions.

Each command requires the exact current session snapshot and a live scoped
owner. Changed A→B→A metadata, cross-view snapshots, raw policy takeover and
revoked owners refuse before mutation. A same-sort ownership transfer advances
intent even when displayed rows stay equal. Exact same-owner/value edits remain
no-op. `Revoke` and final owner release affect lifetime flags only; accepted
filters remain visible until explicitly withdrawn. `Connected` in a snapshot
describes capture time. A pending preparation retains only the inert lifetime
port, so explicit Revoke or final actor release also revokes its readiness fence.
Callers keep their owner lease alive through an intended publication.

Actual raw `ConfigureQuery`/prepared-query replacement starts a fresh baseline
and generation. Equal raw assignments remain no-op. `PrepareRebase` explicitly
restores the exact complete authored specification's baseline while retaining
user contributions and current user sorting. Merely opening or closing a panel
cannot restore authored defaults. Baseline, contributions and sort descriptors
are all schema-checked, including baseline sorting currently hidden by an override.
Combined trees retain the existing predicate/node/depth/byte and sort limits.

The existing prepared query participant reserves the view and publishes query,
canonical metadata and any prepared scalar Search/Sort reset together. Final
readiness reads only retained private flags/references; every model installs
before notification. Refusal changes none, and observer failure remains committed.
Atomic source support is required for these prepared commands. Ordinary legacy
non-atomic query behavior retains its existing immediate API.

Checked binding recomposition requires the same store, projection and complete
specification. It copies the captured baseline/contributions/sort into fresh
target authority, with disconnected remapped metadata IDs. The source view stays
independent; old owners cannot command the new view. Snapshots retain no source,
store, view, document or executable owner lease. `PrepareImport` is explicit
copied-metadata takeover, not latest-source-at-commit synchronization. Unsupported
session/atomic alternatives retain the existing query-copy fallback. Nothing here
grants an expired mounted UI callback a new event generation.

## Query editor preview

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
discovery and the durable wrapper are separate from live socket authentication,
observing input and installed tool rollout. The maintained native consumer also
passes real FPC HTTP request/response objects through the suspended production
dispatcher. Its generated owned bearer, origin/host/path checks, initialization,
operator permissions, revision/row budgets, supplementary UTF-8/NUL values and
private review ownership run before semantic preview. Identical client names
retain independent authority; disconnect retires that authority. No listener is
started, so this evidence qualifies server admission and dispatch rather than
TCP framing or delivery to the observing release. `WORK.md` records those limits.

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

## Table user controls: native qualification

The P3 workbench implementation adds ordinary header/filter form commands over
the existing view-owned query session. The ordinary native workbench passes782
checks at each actual1280/390 client with clean heaps. Matching browser assertions
and both Studio mains compile; browser input and full Studio authoring remain
separate gates. [WORK](../WORK.md#table-query-workbench--2026-10-21) records the
bounded compiler/input evidence and retained failures. The original seven
filtering task criteria remain open.

Bound scalar tables resolve absent policies to enabled family-appropriate
filtering and sorting. Literal `Items` has no collection/query owner. Authors can
narrow the public contract without widget downcasts:

```pascal
LPeople := NyxCollectionView(NyxCollection('people'))
  .Column(NyxTextField('name'), 'Name')
    .ColumnQuery(NyxColumnQuery.TextComparison(nqtAsciiInsensitive)
      .Operators([nqcEqual, nqcContains, nqcStartsWith]))
  .Column(NyxIntegerField('age'), 'Age')
    .ColumnQuery(NyxColumnQuery.Operators([nqcEqual, nqcAtLeast, nqcAtMost]))
  .Column(NyxBooleanField('active'), 'Active')
    .ColumnQuery(NyxColumnQuery.Sorting(False))
  .TableQuery(NyxTableQuery.SortMode(nqsmMultiple));
```

`Filtering(False)`/`Sorting(False)` opt out per column or table;
`NyxTableQuery.Enabled(False)` opts out of all table query commands. These choices
do not clear an existing authored query. `UseDefaultColumnQuery` restores only
the preceding column's absent choice; `UseDefaultTableQuery` restores the table
default. Explicit choices use strict nested view descriptor version7. With no
explicit query choices, former version1–6 descriptors remain byte-compatible.
Generated Pascal and the closed source reader use these same fluent builders.

Text admits equal/not-equal/contains/starts-with/ends-with. Boolean admits
equal/not-equal. Integer/Number add less/at-most/greater/at-least. Unsupported
operator families, duplicates and unknown enum/wire values refuse before a new
policy is returned. Text comparison defaults to exact Unicode scalar behavior;
the explicit ASCII-insensitive choice changes only A–Z folding. Empty text is
a value. The current collection schema requires a correctly typed value for
every declared field, so nullable/missing-field operators are not implied.

The native header's arrow and `Alt+Down` open the shared Nyx form; its title
cycles ascending/descending/default, with Shift admitting another key when
multiple sorting is enabled. Browser headers expose separate keyboard-focusable
sort and filter buttons. Header markers show filters, sort precedence and the
matching/source counts; the form contains full removable contribution chips,
explicit resets and empty-result recovery. This bounded presentation does not
claim an always-open chip toolbar. Native column-resize boundaries and an active
cell editor keep their existing gestures/key ownership.

Matching rows are counted **before paging**; source rows count the source store.
The latter is not a count of rows eligible under an authored baseline. Clear
Owner withdraws only that surface's filters and its still-owned sort. Clear All
Filters, Reset Sort and Reset All are explicit shared-session commands. A raw
query takeover invalidates older work; a fresh input can join the new session.
Every superseding input advances a private serial even when its Clear is a model
no-op, so delayed Apply cannot replay over newer user intent. Invalid drafts
stay in actual mounted form controls until corrected/replaced deliberately.
Closing/dismissing the form refuses its pending commands; a fresh table input can
reopen the retained draft. An explicit Discard draft and reload action replaces
it from current accepted rules when its original capture is stale. A successful
Apply/reset retires that exact form and
the next Open observes current accepted chips/rules. A refusal or veto preserves
the existing draft. Native header activation occurs on mouse release after the
widget's ordinary resize/focus decision. Explicit form focus is honored on first
open, reopening an existing draft and structural replacement after former-host
retirement, without rebuilding a retained draft merely to focus it.

`NyxQueryWorkbenchEvent(nqweBeforeChange)` and `nqweChanged` use the existing
ordered named event streams. The typed `NyxQueryWorkbenchChange` decoder returns
copied Details. Before carries the accepted observation and requested edits;
its `HasAfter=False` explicitly avoids inventing a second composition engine.
Sequential before callbacks may cancel their execution; failed, asynchronous
or incomplete before callbacks refuse publication. Physical input `Consume`
is not a semantic query veto. Changed carries actual accepted old/new snapshots
after grouped publication and supports ordinary scheduler policies. The host
passes exact runtime and design identities independently for reusable instances.

`tools/build.ps1 -Target query-workbench` takes a new
`QueryWorkbenchEvidenceDirectory`, runs the ordinary native consumer at actual
client1280/390, and compiles matching browser assertions without launching a
server. `QueryWorkbenchPlatform` can supply an already known LCL unit target
such as `i386-win32`. This command does not qualify full Studio/history/own
saved-generated application flows or the separate actual-browser input gate.

## Studio query-control choices

A bound table's collection Inspector separates the authored query from the
commands its users may operate. Table choices control filtering, sorting and
single/multiple sort keys. Each column can narrow filtering, sorting, operators
and text comparison according to its scalar family. Applying one complete form
uses one ordinary collection intent and the paired document/Pascal history.
Changing an option alone remains a draft. Unrelated title, sizing, selection or
column edits preserve every surviving raw query-control declaration.

“Use Nyx defaults” removes that one explicit table/column policy. An absent
choice stays absent in saved/source output; resolved defaults are not silently
written back. On a reusable instance this creates the same local whole-binding
override as other collection edits. “Inherit binding” instead removes the local
binding override and resumes its current definition, including its policies.
Clearing one policy does not merge that property from an ancestor.

The ordinary queue carries typed `scaTableQuery`, `scaColumnQuery`,
`scaDefaultTableQuery` and `scaDefaultColumnQuery` proposals. Setters contain the
matching immutable `TNyxTableQueryPolicy` or `TNyxColumnQueryPolicy`; default actions
contain no policy. All four require the captured full effective-spec/schema
baseline. A changed owner, field family, binding or pending Pascal draft refuses
before the accepted pair changes. Runtime filters remain independent user-session
contributions rather than authored defaults.

The existing collection patch transports those same intents, using closed
`table-query`, `column-query`, `default-table-query` and `default-column-query`
action names at the external JSON boundary. The strict decoder and advertised
schema share the baseline, table projection, field-family and payload rules.
Complete binding transport admits strict version7 query-control descriptors;
the older versions keep their former shapes when no policy was declared.

`tools/build.ps1 -Target query-authoring -QueryAuthoringPlatform i386-win32
-QueryAuthoringEvidenceDirectory <new-directory>` is the maintained ordinary
policy-form regression. It executes native1280/390 and compiles matching browser
assertions without launching a host. Source release is not qualification: current
results and the still-required event/discovery, own Save/Open/generated application
and actual-browser gates are recorded in [WORK](../WORK.md#table-query-policy-authoring--2026-10-21).

The policy regression owns each native process and bounds execution to 300 seconds
by default (`-QueryAuthoringTimeoutSeconds`). A timeout retains the raw stdout,
stderr and process receipt; an interrupted run proves neither passing input nor
clean teardown. `-QueryAuthoringDiagnostic` runs the same native1280 fixture and
assertions with phase markers, leaving 390 and browser compilation unattempted.
It is a diagnostic gate; it does not replace full target/width qualification.

The current diagnostic reached144 checks at native1280 before that limit,
without an assertion failure or overall pass. Repeated checked-build refreshes
took18-24 seconds and Apply about47 seconds. Existing phase/layout profiling
locates shell rendering as the dominant completed parent interval, up to21.7s;
nested control construction, handle initialization and layout are not additive.
Queue compatibility passes126 native checks with a
clean heap. Native390, matching new-consumer browser compilation and the complete
ordinary authored journey remain unqualified. Keep interrupted logs and do not
infer clean teardown from a forced stop. [WORK](../WORK.md#table-query-policy-authoring--2026-10-21)
records the exact source/evidence boundary.

`-QueryAuthoringProfile` adds only the existing Pascal phase/layout clocks and
counters to the native checked build. Combine it with `-QueryAuthoringDiagnostic`
to observe that same1280 journey under the same execution bound. Composition,
realization, build, Sync/layout and source timings can overlap; use completed
enclosing phases and their nested detail rather than summing all printed totals.
This diagnostic does not establish release-build performance or full acceptance.
The complete functional journey has many Apply/Undo/Redo cycles. Its separately
reviewed total execution budget can use the existing explicit timeout option;
that does not change the300s default or original operation deadlines/assertions.
Current results and any future admission remain in WORK.

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
