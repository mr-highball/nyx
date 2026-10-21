# Components and compound recipes

[Architecture](architecture.md) · [Catalog](../src/nyx.catalog.pas) ·
[Default recipes](../src/nyx.recipes.pas) · [Defaults and named parts](components-reference.md)

[Portable component documentation](component-documentation.md) provides immutable
typed API descriptors and inspection of existing schema/catalog facts. Automatic
reference discovery and the full Studio reference browser remain ongoing work.

Nyx currently defines 41 primitive/layout/authoring kinds and 36 compound recipes. A recipe
is an ordinary owned tree built from reusable primitives. Named parts are
editable nodes rather than inaccessible renderer internals. Native and browser
adapters consume the same expanded tree.

[Disclosure sections](disclosure.md) add a specialized Boolean compound with
editable header/body parts. Both adapters retain input while collapsed, respect
caller visibility and preserve body event identity. Studio consumes the public
component for collapsible Tab pages; observing delivery and full accessibility
qualification remain open.

Studio now uses [independent inspector groups](inspector-groups.md) around its
existing component, data, responsive and property editors. Typed presentation
choices retain collapsed sections and drafts without changing the design pair.

[Retained tabs](tabs.md) now use shared typed initial selection, activation,
orientation and wrapping with specialized interfaces and independent reusable
instances. Both adapters retain page controls/drafts and emit ordered selection
callbacks. Ordinary typed policy authoring/source reading is qualified; full
family, connected/native host lifecycle and accessibility/widgetset acceptance
remain open; declared breadth and Basic capability ratings are unchanged.

Bound tables add shared typed data-cell navigation and current-cell editing,
independently of row membership. The same specialized MCP-exported application
exercises both actual targets; browser Tab traversal and narrow horizontal
scrolling retain authored text scale. See [table behavior](collection-views.md#bound-table-cell-navigation).
Typed filtering/stable ordering and incremental scalar-row refresh are integrated;
see [queries](collection-queries.md) and
[refresh behavior](collection-views.md#incremental-value-refresh).
Production paging, virtualization and richer table styling remain open; declared
catalog breadth does not establish those implementations.

Compound scalar values and editable named fields now have explicit [typed
contracts](contracts.md). Rating/stepper selections are integers; segmented
choices are text; a search action carries its query field. Layout-only roots
declare no self value. Recipe instantiation retains independently owned root
contracts, extension data and bindings along with its parts.

[Typed selection styling](selection-styles.md) lets those selectable compounds
choose distinct selected/unselected variants or retain every part's authored
variant. Derived Pressed and accepted state still follow the ordinary contract;
independent reusable overrides, source admission and paired history are qualified.

[Task progress](progress.md) supports known completion and unknown activity through
typed Boolean state, specialized fluent controls and independent reusable bindings.
Native displayed animation and browser numeric-accessibility transitions are
qualified separately from full feedback-family quality and observing rollout.

Specialized calendar and clock controls also have immutable typed values and
fluent domains; see [calendar fields](date-fields.md) and [clock fields](time-fields.md).
Typed authoring does not by itself qualify every target's picker implementation.

Image controls and reusable media parts accept [portable typed PNG/JPEG sources](images.md),
shared fit/position and responsive/platform policies. Checked ordinary Win32
decoding and exact emitted source are qualified; current browser execution,
asset import/resolution and full media parity remain open.

Text APIs use `TNyxText` from `nyx.text`: native UTF-8 and browser Unicode. Source
units declare `{$codepage utf8}`. `Props` is an owned `TNyxStrings` collection;
it preserves property order, empty values and case-sensitive keys. UTF-8 captions
survive JSON, history, generated Pascal, HTTP and actual LCL controls without
changing the host application's system codepage.
The [identity contract](identity.md) documents authored Unicode limits, escaped
reusable runtime keys, editable owners and explicit target-handle lookup.

```pascal
LCatalog := TNyxCatalog.Create;
try
  LActivityListCard := LCatalog.NewNode(nkListCard, 'activity');
  LDocument.AddPage(LActivityListCard);
  LActivityListCard.Part(NyxPart('title')).Configure.Text('My activity').Done;
  LActivityListCard.Part(NyxPart('list')).Configure
    .Items('First item' + #10 + 'Second item').Done;
  LActivityListCard.Part(NyxPart('actions')).Add(
    TNyxNode.Create(nkButton, 'activity-add').Configure
      .PartName(NyxPart('floating'))
      .Text('+ Add')
      .OnClick(NyxEvent('add'))
      .Done
  );
finally
  LCatalog.Free;
end;
```

The document owns the admitted tree. `Part` borrows a node; slash paths access
nested slots, such as `Part('actions/floating')`. Missing slots raise an explicit
model diagnostic. Changes to one factory instance leave every other instance
and its registry template unchanged.

Derive a recipe by customizing a factory tree, then calling
`RegisterRecipe(NyxCustomKind('my-list'), 'My list', 'Custom', LTemplate)`. The registry copies
the borrowed template; free the construction tree afterward. `NewNode` creates
fresh owned parts and distinct IDs. Adding native/browser factories handles a
custom primitive whose target behavior differs from a normal container.

Primitive derivation works through the same API. Register a customized button as
`accept-button`, then derive another recipe from it. Its semantic `Kind` remains
the extension name; persisted `projection-kind` retains the physical button
contract through JSON and generated compilation. The default adapters apply its
input/event/layout semantics without needing the construction-time catalog.
Leaf controls remain leaves; put a button and label inside a row to compose a
labeled button. Exact semantic factories take precedence over base factories;
a registered button factory can also serve derived button recipes.

The shared [schema](../src/nyx.schema.pas) defines primitive capabilities and
typed property defaults. `NyxProperties(Node, Document)` returns caller-owned
metadata, including inherited reusable defaults. Known booleans, choices and
bounded decimal integers are admitted; unknown extension properties survive.
Studio consumes these fields through ordinary Nyx selects, inputs and number
fields. Failed edits/imports retain the accepted project and redo history.
`Available` describes a projection, not full family or property parity; native
date, time and color have owned LCL calendar, clock and RGB picker fields with
Basic support. See [calendar](date-fields.md), [clock](time-fields.md) and
[RGB](colors.md) contracts for exact semantics and qualification limits.
Custom factories own their capability
semantics. Missing projections raise diagnostics instead of rendering an empty box.

Reusable definitions can also be customized per instance, including content:

```pascal
LDocument.AddComponent(LCatalog.NewNode(nkListCard, 'activity-definition'));
LActivityComponent := TNyxNode.Create(nkComponent, 'activity');
LDocument.AddPage(LActivityComponent);
LActivityComponent.Configure.Component(NyxComponent('activity-definition')).Done;
LActivityComponent.OverridePart(NyxPart('title')).Configure.Text('My activity').Done;
LActivityComponent.OverridePart(NyxPart('actions'), noAppend).Add(
  LCatalog.NewNode(nkButton, 'activity-add').Configure
    .PartName(NyxPart('floating'))
    .Text('+ Add')
    .OnClick(NyxEvent('add'))
    .Done
);
```

`OverridePart` returns an instance-owned `slot-override` descriptor. Its properties
customize the realized target; its owned children are independent payloads.
`properties` changes values, `append`/`prepend` insert content, `replace` substitutes
exactly one part while preserving its named contract, and `remove` deletes the
part. The `.` path addresses the reusable root for properties/content. Repeated
calls with omitted mode preserve an existing operation. Rules run in stored order;
later paths must still exist after earlier replacements/removals.

Definitions and other instances remain unchanged. Ordinary instance children are
rejected: additions need explicit descriptors. Duplicate paths, malformed
payloads, protected identity metadata, missing paths, leaf-child additions and
invalid effective values receive diagnostics. Root removal/replacement requires
changing the reference itself. Nested paths such as `activity/actions` work after
reusable expansion. Descriptors survive clone, history, JSON, derived recipes and
Pascal compilation; appended payloads retain their own selectable design IDs.

In Studio, select an instance and choose **Customize** for a named part or
**Customize content** for its root. The inspector edits that part independently.
For a layout part, palette additions or **Use** reusable views append content to
that instance through the same insertion contract. Undo/redo
uses the same portable document contract. The current mode/path fields expose
the operation; dedicated replacement/removal authoring affordances remain open.

The Properties inspector also offers [named reusable copies](reusable-authoring.md)
of authored subtrees, with collision-free purpose-based identities, a read-only
preview and one paired Undo step. The existing automatic Make reusable shortcut
remains available.

Browser factories return a new detached DOM element. Native factories return a
new unparented control owned by the supplied `AOwner`. Both renderers build a
candidate before replacing the accepted view. A factory failure preserves the
current controls, realized values and event bindings; partial candidates are freed.
This guarantee belongs to adapter mounting. Studio currently rebuilds its outer
shell and nested canvas on a design refresh; retaining a mounted canvas through
every shell failure remains part of incremental authoring work.

`code-editor` uses a browser textarea and an LCL memo with a portable `value`,
`readonly` and `change` contract. `design-surface` hosts nested Nyx views.
Studio builds its shell in [nyx.studio.view.pas](../studio/nyx.studio.view.pas),
using these catalog components. `ElementFor`/`ControlFor` expose borrowed hosts
by stable identity. Unmount a nested view before replacing/freeing its containing
host. Syntax services and complete native Studio workflows remain separate work.

The shared action vocabulary includes clear, increment/decrement, segmented
selection, toggle and dismissal. `emit` forwards a semantic event from the inner
control to its nearest compound root. Applications handle domain events such as
search, submit, export and navigation. Each reusable instance has independent
runtime values; definition edits and instance state have different lifetimes.

| Family | Defined recipes |
| --- | --- |
| Actions | labeled-button, split-button, command-bar |
| Inputs/forms | search-field, form-field, login-form, settings-panel, date-range, number-stepper, segmented-control, rating, property-grid |
| Data | data-toolbar, filter-bar, master-detail, list-card, data-card, kanban-board, timeline, floating-action-panel |
| Navigation | pagination, breadcrumbs, sidebar-nav, wizard-step, stepper |
| Dashboard | stat-card, metric-grid |
| Feedback | empty-state, confirmation-dialog, notification-card, toast |
| Media/social | profile-card, media-card, avatar-group, comment-thread |

These are defined composition recipes with tested ownership and selected
interactions. Production behavior is still being completed. The Kanban recipe
defines three lanes and action slots; the explicit [typed Kanban attachment](#typed-kanban-lane-movement)
adds selected-card movement to its configured bound lists. Automatic Studio
runtime attachment, arbitrary lane counts and complete card editing remain open.
The confirmation recipe supports inline use and a
[managed browser/LCL presentation](confirmation.md) with actual focus, cancellation,
ordered callbacks and independent content. Data
virtualization, overlays, picker parity, accessibility and broader interaction
coverage remain explicit [component work](../TODO/NS-3_components_01.md).

Existing `INyxElement` units and demos remain as historical code. The new model
preserves fluent composition and observable interaction concepts while providing
one inspectable, serializable contract for Studio and both target adapters.

## Typed Kanban lane movement

`nyx.kanban` composes the existing specialized `INyxKanbanBoard`, collection views,
events and ordinary Nyx buttons. Configure an authored board before mounting it:

```pascal
LPlan := NyxKanbanLane('plan');
LBuild := NyxKanbanLane('build');
LDone := NyxKanbanLane('complete');
LOptions := NyxKanbanOptions(NyxCollection('cards'),
  NyxTextField('title'), NyxTextField('lane'))
  .Lane(LPlan, 'Plan')
  .Lane(LBuild, 'Build')
  .Lane(LDone, 'Complete')
  .Scoped(csInstance);

LBoard := NewNyxKanbanBoard('workflow');
ConfigureNyxKanbanBoard(LBoard, LOptions);
LDocument.AddComponent(LBoard);
```

The document supplies an ordinary collection schema/default dataset with those
typed text fields. A card's lane is a configured open lane reference, independently
of its caption or row position. This helper targets the existing three-lane recipe;
it does not redefine every Kanban workflow as three stages. Instance scope gives
each reusable occurrence its own live dataset. Existing Add actions remain
application-owned; the attachment does not invent application card content.

After mounting, retain the **actual realized board**, the same collection-view
set used by the renderer, and that renderer's events/scheduler:

```pascal
LBinding := NewNyxKanbanBinding(LMountedBoard, LOptions, LMountedViews,
  LRenderer.Events, LRenderer.Events.Scheduler, RefreshBoard);
```

`RefreshBoard` synchronizes the current renderer. Disconnect the managed binding
before retiring that mounted page or renderer. It owns no renderer or document;
its callback ports revoke their borrowed receiver. A captured move can outlive
disconnection safely and then refuses publication. A new view generation requires
a new attachment, even when its control names match the previous view.

Select a card in a lane and activate the destination's **Move selected card here**
button, using normal keyboard or pointer input. Local drag/drop moves the selected
card through the same operation. Dragging another row does not silently select it.
Foreign/external transfers grant no mutation authority. Public callers can capture
the same command with `CaptureMove` and explicit item/source/destination/revision,
then `Commit` or `Cancel` it.

Default Click movement runs synchronously before ordinary application callbacks;
their own execution policy remains the caller's choice. Gesture negotiation needs
an active sequential platform response. Deferred/threaded gesture callbacks cannot
retroactively offer or accept a drag. Configure, capture, commit, cancel and
disconnect on the owning UI thread; marshal worker intent through the scheduler.

Moves update the lane field without removing card identity. The current source
must still visibly contain the card; an old filtered selection is insufficient.
The exact shared store/revision and mounted lifetime are checked again after all
validators and before installation. This uses existing atomic `PrepareAssign`
with its Assign-style change receipts. Alternate collections must supply that
atomic capability. Refused, stale and cancelled work preserves accepted data.
Notification failures occur after committed installation: `Committed` stays true,
and callers must not retry the operation as though it had rolled back.

`nyx.kanban.intent` adds optional portable automatic-movement metadata:

```pascal
SetNyxKanbanIntent(LBoard, TNyxKanbanIntent.Configured(LOptions));
SetNyxKanbanIntent(LInstance, TNyxKanbanIntent.Disabled);
InheritNyxKanbanIntent(LInstance);
```

Configured intent replaces the complete typed options. Disabled is a local mask;
inherit removes that mask and reveals the definition's value. JSON and crafted
Pascal preserve the distinction and customized parts. The setter writes metadata
only and does not call Configure or duplicate children. Both renderers now prepare
automatic attachment before building controls, then activate it with the accepted
view. Native contract and ordinary/generated desktop/compact consumers qualify
this runtime ownership. Ordinary native Studio configuration, paired history,
Save/Open and the exact saved generated application also have local qualification;
browser consumers compile only. Browser execution and full component/application
parity, quality and delivery remain open.

Generated Pascal retains normal list bindings, queries and added buttons. The
application can opt into automatic lifetime ownership with configured intent.
Without that intent, the explicit mounted attachment remains available and static
boards retain their behavior. Design mode keeps movement dormant. Custom recipe
parts must preserve the required specialized lanes/lists/actions or admission
refuses instead of borrowing another instance's view. Renderers expose the accepted
`KanbanBinding` by exact realized ID for programmatic commands; disabled or unmounted
boards return nil. A failed replacement keeps the previous view active.

Automatic boards now share the prepared coordinator path with MasterDetail and
caller-owned behavior. Built-in registration composes with the caller's immutable
candidate snapshot; configured applications need no additional registration.
`CoordinatorBinding` preserves the exact specialized `INyxKanbanBinding` returned
by `KanbanBinding`. Its lane lists and Move buttons are the actual retained
controls, preserving specialized configuration and read-only admission.
The library's internal canonical adapter feeds the existing movement engine;
the public creator projection retains its copied, guarded contract.

Explicit binding `Disconnect` revokes that attachment and releases its own
availability constraints without cancelling the enclosing event generation.
This permits a deliberate manual reattachment to the same realized board;
the caller owns that manual lifetime. A retained former control facade still
owns its old node, but cannot change a same-ID successor or grant a retired
command new authority. Automatic refresh uses the common generation-guarded weak
host port. The scoped [common migration](../TODO/DONE/NS-3_coordinators_04.md)
qualifies139 native contract checks,121/121 ordinary controls,114/124 unchanged
Studio checks and15/15 applications compiled from that journey's own saved pair,
all with clean heaps. All16 compilers pass with zero Nyx warnings; matching browser
programs compile only. Creator/MasterDetail and publication compatibility remain
qualified; full reusable/embedded browser and original parity criteria stay open.
[The work record](../WORK.md#kanban-common-coordinator-migration--2026-10-21)
records exact source/pair identities and evidence boundaries.

`tools/build.ps1 -Target kanban -KanbanEvidenceDirectory <new directory>` includes
the public contract consumer, ordinary native desktop/compact journey and exact
emitted-companion reconstruction. It stages `kanban.html` with the Pascal browser
consumer. `kanban.html?review` keeps the English two-board workspace interactive
on an independently admitted host; the native executable also accepts
`<new evidence directory> <width> --review`. The default run is a finite test.
WORK.md owns actual current results. Compilation, synthesized native drag slots,
physical input, accessibility, full performance and observing delivery remain
separate evidence boundaries.

### Configure Kanban in Studio

Select a concrete Kanban board, a reusable board instance or an authored named
board part. Its Properties panel offers **Kanban movement**, built entirely from
ordinary Nyx controls. Choose an existing collection, distinct text fields for
card titles and lane values, runtime scope and three lane values/action labels.
Without a usable collection, the form explains the prerequisite and disables
Apply. Existing customized headings remain yours; action labels describe the
movement destinations without replacing those headings.

**Apply board configuration** updates all three list bindings and automatic intent
in one paired design/Pascal/history operation. A single Undo restores the whole
previous pair. Existing Move buttons, styling, column captions/sizing, typeahead,
paging and local payload remain intact. Missing Move buttons are ordinary Nyx
buttons added through the lane's authored ownership. Incompatible role/column
shapes refuse atomically instead of discarding customization.

**Disable automatic movement** stores a local mask and retains the lists and
customizations. **Use inherited configuration** removes the exact local intent;
if a definition supplies another configured collection, all three behavioral
bindings are reconciled together while local presentation remains. Inheriting a
static definition leaves the ordinary bindings in place. Independent reusable
instances keep their separate stores unless the caller selects application scope.

Embedded editors can reuse `NewNyxKanbanEditor` and its copied typed
`TNyxKanbanEditorChange`. Studio captures that change with exact authored/schema/
source guards and admits `NyxKanbanPolicyPatch` through the existing project
transaction. Incomplete input can be parked in `TNyxKanbanEditorDraft`; a changed
owner/schema retires it. These forms and changes retain no borrowed controller.

`tools/build.ps1 -Target kanban-studio -KanbanEvidenceDirectory <new directory>`
maintains the typed form/candidate checks, actual native Studio journey and exact
compiled saved companion at1280/390. It also stages `kanban-studio.html` for
ordinary and compiled browser consumers without starting a server. Current native
results are63/165/114/124/15/15 with clean heaps; browser artifacts compile only.
[WORK.md](../WORK.md) retains the missing-import failure and exact repair evidence.

## Creator behavior in ordinary applications

The [Selection Summary example](../examples/selection-summary/README.md) shows
an independently registered Pascal behavior built from standard Nyx controls.
Register its factory through an application's borrowed `View.CoordinatorRegistry`
before native `Mount` or browser `Run`. That caller registry supplies executable
authority; saved intent alone cannot instantiate a factory or acquire host access.

The separate [reusable application consumer](../tests/nyx_coordinator_reuse_controls.lpr)
exercises two actual App objects, each mounting two reusable workspaces with
Summary, MasterDetail and Kanban. Instance-scoped bindings must isolate their
twelve runtime stores. Nested parts resolve on the realized workspace, and
`Supports` on `CoordinatorBinding` retrieves the specialized behavior interface.
Realization may project an authoring compound to a standard Card; an authoring
subtype and its runtime binding have separate contracts.

Both applications borrow one unchanged authored document. Build refusal pages
before mounting and use `ShowPage` to exercise navigation refusal; preserve the
accepted root and usable input when a required role is missing. Destroy both
applications before changing or freeing their borrowed document.

`tools/build.ps1 -Target coordinator-reuse -CoordinatorReuseEvidenceDirectory <new directory>`
executes this ordinary native application at1280/390 and compiles the matching
browser assertions. It preserves separate width logs and starts no listener.
[Reuse05](../TODO/NS-3_coordinators_05.md) retains full both-host reusable/embedded
acceptance. [WORK.md](../WORK.md) records actual outcomes;
the command and consumer source alone do not establish that acceptance.
The current ordinary native application result is588/588 at1280/390 with clean
heaps, preserving the original267 reusable checks before the embedded cases.
Matching browser assertions compile; both compilers have zero Nyx warnings.
Its separate identical reusable/embedded design/Pascal exports qualify authored/
default isolation, not Save/Open or generated-application execution. R2's local
embedded adoption qualifies natively; actual browser input/lifetimes remain open.

Embedded saved Canvas items inherit the parent's captured caller registration
membership before Build. An accepted Canvas keeps that membership for later
Scene content replacements; a mutable parent registry change belongs to the next
parent candidate. Children compose their own built-ins. A child's explicit
nonnil `CoordinatorRegistry`, including an empty registry, overrides caller
membership only; nil restores inheritance for its next candidate. Each child
keeps its own generation, collection authority and weak refresh. The
[example's registry rules](../examples/selection-summary/README.md#embedded-registry-rules)
describe the existing structural-presentation remount path without invented
child source or a second state/event engine. [Reuse05](../TODO/NS-3_coordinators_05.md)
owns the actual integration and target-execution evidence.

The first executed1280/390 browser consumer currently fails during Selection
Summary's managed construction, before completion. The unchanged native results
do not qualify that browser path; [WORK.md](../WORK.md#first-reusableembedded-browser-execution--2026-10-21)
retains the exact failures, host retirement and separate ownership-repair proposal.

## Component discovery and creator descriptions

Studio offers **List** and **Grouped** palette modes. Each component has one
purpose group; cross-cutting intent labels help find it without duplicating its
palette entry. **Show group** filters either mode. Search requires every entered
word across the component kind, title, group, labels, aliases and description.
For example, `memo`, `text input` and `description reply` find the multiline text
editor. Empty results offer **Clear filters**, retaining the presentation choice.

All default entries have high-level intent descriptions. They appear in native
hints/browser tooltips, and **Details** exposes them at full width for touch and
help reading. The Inspector also shows the selected component's explanation
above its Properties and Events tabs. Registered recipes retain their creator's
description rather than the description of their underlying layout control.
Browser presentation preferences recover separately from project
files; search text is transient and these choices do not add undo entries.

Component creators describe registered kinds or recipes through the public
catalog, using the typed vocabulary in
[nyx.catalog.labels.pas](../src/nyx.catalog.labels.pas):

```pascal
Catalog.RegisterRecipe(NyxCustomKind('observatory-notes'), 'Observatory notes',
  'Extensions', NotesTemplate);
Catalog.Describe(NyxCustomKind('observatory-notes'), pgForms,
  [clText, clMessaging, clCompound], 'journal logbook',
  'Collect night-shift observations and handover notes for the next crew.');
```

`Describe` returns the borrowed catalog for fluent setup. Its final two arguments
are additional search aliases and the creator's high-level description. These
are ordinary user text; the primary group and standard intent labels are enums
and a typed set. Registry metadata controls discovery/help, independently of the
component's runtime tree and target implementation. `pgAll` is a filter and is
rejected as a component's home. Unknown extension categories start in **Other**
until explicitly described. Metadata values are detached copies on both targets.

The [generated reference](components-reference.md) includes the descriptions,
groups and labels alongside the supported properties, contracts and named parts.
