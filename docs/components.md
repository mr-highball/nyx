# Components and compound recipes

[Architecture](architecture.md) · [Catalog](../src/nyx.catalog.pas) ·
[Default recipes](../src/nyx.recipes.pas) · [Defaults and named parts](components-reference.md)

Nyx currently defines 40 primitive/layout/authoring kinds and 35 compound recipes. A recipe
is an ordinary owned tree built from reusable primitives. Named parts are
editable nodes rather than inaccessible renderer internals. Native and browser
adapters consume the same expanded tree.

Compound scalar values and editable named fields now have explicit [typed
contracts](contracts.md). Rating/stepper selections are integers; segmented
choices are text; a search action carries its query field. Layout-only roots
declare no self value. Recipe instantiation retains independently owned root
contracts, extension data and bindings along with its parts.

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
date/time/color remain text fallbacks. Custom factories own their capability
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
interactions. Production behavior is still being completed: for example, the
Kanban recipe defines lanes and action slots but does not yet implement card
drag/drop, and the confirmation recipe is currently an inline panel. Data
virtualization, overlays, picker parity, accessibility and broader interaction
coverage remain explicit [component work](../TODO/NS-3_components_01.md).

Existing `INyxElement` units and demos remain as historical code. The new model
preserves fluent composition and observable interaction concepts while providing
one inspectable, serializable contract for Studio and both target adapters.

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
