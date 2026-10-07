# Managed command menus

[Managed controls](managed-controls.md) · [Contextual views](popover.md) ·
[Current evidence](../WORK.md)

`INyxMenu` gives ordinary specialized Nyx buttons and named parts a shared command
contract. Browser and LCL factories present the same independently owned content.
Studio's **Actions** button consumes `INyxMenuButton`; its **Inspect** submenu
uses the same managed recipes for Properties, Events and component help.

## Save menus with the application

Document-owned definitions now persist the item order, command/group references,
check defaults, submenu references and complete presentation/search policy.
Use ordinary reusable Nyx content, then attach its declaration to a specialized
button:

```pascal
LDocument.AddComponent(LActionsColumn);
LDocument.Menus.Define(
  NyxMenuRef('document-actions'),
  NewNyxMenuDefinition(
    NyxReusableRoot('document-actions'),
    NyxMenu('Document actions'))
    .Action(NyxPart('copy'), NyxMenuCommand('copy-selection'))
    .Check(NyxPart('guides'), NyxMenuCommand('show-guides'), False));

LOpenActionsButton.Configure.Menu(NyxMenuRef('document-actions')).Done;
NyxCallbacks(LOpenActionsButton)
  .OnNamed(NyxSemantic(nseActivate))
  .Add(NyxHandler('TDocumentMenuCommand'), NyxCallbackID('document-menu-command'));
```

The referenced content root must already belong to the document. Define a nested
menu independently, then use `.Submenu(NyxPart('density'), NyxMenuRef('density'))`.
`INyxMenuDefinition` builders return immutable independent plans; the document's
`INyxMenuDeclarations` registry normalizes public getters into owned definitions.
References are distinct Pascal types. `NoMenu` explicitly masks an inherited
attachment; `InheritMenu` removes that local declaration. Attachments are structural
and refuse platform/viewport/presentation-scoped configuration. Their visibility
and layout still use Nyx's ordinary presentation contracts.

Ordinary browser/LCL applications automatically bind the mounted invokers. Studio
binds them in **Interact**; design selection stays an editor operation. Commands
arrive on the invoker's named `nseActivate` stream. `NyxMenuInvocation(AEvent)`
returns the typed command, part and optional checked snapshot. Ordered callbacks
and per-stream scheduling use the existing event contract. Menu input remains
sequential on the UI thread. Buttons with renderer actions refuse admission.

Mounted families have independent check/radio state and never edit saved defaults.
Remounting creates a new family from those defaults. `Application.Menus.Menu`
observes one exact runtime control; release retained observations before changing
pages or destroying the application. Direct adapter users own the result of
`BindNyxBrowserMenus` / `BindNyxLCLMenus` and release it before renderer remount or
retirement. No binding retains a renderer back into the document.

Saved child policies apply in full. Choose `.Presentation(NyxPopover('Density')
.Placement(npsRight))` for a right-opening branch; its default opens below.
Invoker click/Down opens first and Up opens last, following menu-button keyboard
practice. The menu owns initial focus; the saved generic popover focus value is
retained for exact reconstruction, without overriding first/last item selection.

Menu-only documents use codec version 6; saved bar grouping selects version 7.
Existing versions 1–6 remain supported. Registry limits are 64 definitions,
256 items per definition, eight submenu levels and 2048 expanded family items.
Missing roots/parts, duplicate
parts, incompatible specialized controls, unknown options, dangling references,
cycles and multiple initially selected radios refuse. JSON keys exist at the
persistence/semantic boundary; generated Pascal uses typed fluent expressions.
Standalone views retain only reachable menu/content dependencies. A reusable
promoted to the standalone page also retargets its menu root.

## Query and author through MCP

`nyx_menus` lists at most eight definitions by default (sixteen maximum), without
their item arrays. Supply `name` for exact policy and ordered item inspection;
`itemOffset`/`itemLimit` page items. `textOffset`/`textLimit` page its title by
Unicode scalars. `nyx_node` exposes the selected control's local `menuAttachment`,
distinguishing inheritance from an explicit clear. These queries do not change
selection, revision or history.

Group `menu-define`, `menu-remove`, `menu-attach` and `menu-inherit` operations in
one `nyx_transaction` with the current `expectedRevision` and unique operation ID.
Related control creation, definitions and attachment may be one operation group;
the complete candidate is validated before paired design/source publication.
Removing a referenced definition refuses unless its dependents are handled in
the same group. Retained declarations also prevent removal of their content roots
through `nyx_roots`. One Undo/Redo restores the whole accepted design/source pair.

`tools/nyx_menu_authoring_review.lpr` is the maintained authenticated Pascal client.
It creates its own temporary review, composes the English content/declarations,
reads bounded context/source, checks refusals/history, builds both application
and standalone-view targets, verifies exact HTTP compiler-input bytes, then
discards only that review. Supply explicit enrolled `config.toml`, owned output
directory and editor HTTP base. Its source requires the twenty-one-tool schema,
which the current frozen observing release also authenticates.

`tools/build.ps1 -Target menu-authoring` checks the public portable/source fixture
and stages both target consumers. Add `-MenuAuthoringSourceDirectory` to consume
the exact exported `nyx.generated.view.pas` and qualify native Studio Interact.
Optional `-DesignerMCPConfig` plus `-HttpURL` first runs the authenticated review
into that explicit source directory. Serve `menu-declarations.html` and
`menu-declarations-controls.html` on an admitted HTTP host; the maintained Pascal
ready-capture driver qualifies real-clock completion and desktop/narrow captures.

## Edit saved definitions in Studio

Properties consumes the public `NewNyxMenuEditor` compound from
`nyx.menu.editor`. Its specialized Nyx input/select/checkbox/spin/button controls
edit the content root, ordered action/check/radio/separator/submenu entries,
command/group references, saved defaults and presentation/typeahead policy.
Selectors show readable names; clipped single-line captions map back to exact
Unicode identities. Open a saved definition to edit it, or choose the empty menu
choice to start a new name. This editor preserves existing names; it does not
silently rename definitions or overwrite a saved name from the new-definition form.

Save, add and reorder capture the complete form as one definition replacement.
Attach, suppress inheritance and restore inheritance remain distinct typed
commands. Opening a definition is presentation only. The public capture owns
independent values; Studio checks the exact mounted registry/local attachment
baseline before enqueue and again on the independent paired processor. Draft,
revision and source ownership checks remain with the existing queue. One Undo
restores both files. Definition removal shows a warning and requires an explicit
checkbox confirmation; ordinary candidate admission still refuses retained uses.

`TNyxMenuEditorDraft` lets a host retain incomplete form input while chrome is
rebuilt or temporarily hidden. `Capture` copies only scalar field values; it
borrows no tree, renderer or interface. `Restore` applies the complete snapshot
only to the same selected owner, exact registry/attachment baseline and inspected
definition. Changed context retires it without changing the new form. Copies stay
independent after capture/clear. This is ephemeral presentation, never exported
design state; Save remains the validation/admission boundary. Studio uses the
same value contract in browser and Lazarus hosts and clears it on project change.

Declared invokers expose menu command completion in their named `nseActivate`
event metadata. Existing Events authoring consumes that metadata without granting
a custom Emit API. The completion's `Details` belong to `NyxMenuInvocation`,
independently of the physical OnClick event and its routes.

```powershell
./tools/build.ps1 -Target menu-editor -MenuAuthoringSourceDirectory build/menu/source
```

Supply the exact semantic companion export described above. This target runs the
actual native inspector/paired-queue and ordinary Studio journeys, saves desktop/
compact native captures, then stages `menu-editor.html`, the same compiled companion,
a self-starting Pascal module worker and the ordinary browser Studio bundle.
Execute the fixture over HTTP with the maintained ready-capture driver and
`data-result` at desktop/narrow widths. Its inspector host qualifies actual controls
and the real worker, not a full observing browser Studio session or hardware input.
The observing menu editor is delivered; broader component/performance/accessibility
acceptance remains open. See
[delivery evidence](../WORK.md#current-return-path-menu-editor-observing-delivery--2026-10-07).

The same target builds `nyx_studio_menu_editor_observer` under its `observer/`
directory. Explicit invocation takes an isolated enrolled `config.toml`, loopback
editor base URL with trailing slash, a new evidence directory and CSS width.
It creates its own ordinary project through authenticated semantic MCP, leaving
that project available for review. Physical input then qualifies menu save,
paired history, Events registration/policy/removal, actual callback caret input
and canvas Interact; ordinary compilation uses `nyx_build` for both targets.
It never replaces a user's project or infers callback execution from a TODO stub.
Its optional exact retained fixture workspace plus `navigation` argument runs
only the bounded source/Events return path and records actual host allocation.
`interact` runs only the retained fixture's runtime menu journey. An independent
read-only MCP witness joins and retires while a menu is open; the observing
Agents row must change, and the popup family must survive both shell rebuilds.
The witness is retired even on failure. This mode does not earn full authoring
journey evidence or establish execution of the generated TODO implementation.
`observe` uses an exact existing selected project, with `primary` denoting the
omitted primary context. It edits only unsaved local form presentation, switches
Properties/Events, observes a read-only MCP witness join/retire in Design and
returns to the retained form. It restores the initial title and checks semantic
session fields; the delivery controller separately compares full private pairs
and checkpoint bytes. This qualifies observing form retention while the ordinary
registry is full, without replacing a project or claiming a full authoring journey.
`reset-fixture` is limited to that retained first-save/first-callback fixture:
ordinary Restore clears its own probe draft, then two revision-aware semantic
Undo operations return its companion to the initial boundary. It neither
replaces a project nor closes another workspace. Local draft changes have shared
revisions; the caret journey waits for their receipts before reading source.

Workspace details use the public Nyx split on desktop as well as compact hosts.
An expanding session/build list scrolls inside its allocation; the design/source
stage remains resizable. Compact hosts keep their existing collapse choice.
Source navigation chooses the Source tab, mounts its editor and then moves the
real caret, including after asynchronous callback creation. These controls do
not change accepted source, output settings or project Undo history.
The browser shell owns a dedicated mount inside the body. Public popovers and
floating views keep their independent body portals when roster or presentation
changes require a full shell render; shell teardown removes only its own mount.

## Attach runtime-only menus through an adapter

Use specialized controls for the content and typed values for command meaning:

```pascal
LActionsColumn := NewNyxColumn('document-actions');
LActionsColumn.Configure.Padding(8).Gap(4).Align(ncaStretch).Compound(True);

LCopyButton := NewNyxButton('copy-selection');
LCopyButton.Text := 'Copy';
LCopyButton.Configure.PartName(NyxPart('copy')).Variant(nvSecondary);
LActionsColumn.Add(LCopyButton);

LGuidesButton := NewNyxButton('show-guides');
LGuidesButton.Text := 'Show guides';
LGuidesButton.Configure.PartName(NyxPart('guides')).Variant(nvSecondary);
LActionsColumn.Add(LGuidesButton);

LDocument.AddPage(LActionsColumn);
LItems := NyxMenuItems
  .Add(NyxMenuAction(NyxPart('copy'), NyxMenuCommand('copy-selection')))
  .Add(NyxMenuCheck(NyxPart('guides'), NyxMenuCommand('show-guides'), False));

LMenu := NewNyxBrowserMenu(LInvokerElement, LDocument,
  NyxPageRoot('document-actions'), LItems);
{ Native adapter: NewNyxLCLMenu(LInvokerControl, LDocument, Root, LItems). }
LMenu.OnInvoke.Policy(neSequential).Subscribe(LCommandCallback);
LMenu.Open(NyxMenu('Document actions')
  .Opening(nmoFirst).Wrap(True)
  .TypeAhead(NyxTypeAhead.WindowMilliseconds(1200).Match(ntmFolded)));
```

The factory copies the complete document and item plan. The supplied document can
be freed immediately afterward; the menu owns its independent pages, components
and runtime state through its managed popover. Configure content before its first
Open. `Button(NyxPart('copy'))` returns `INyxButton`, preserving specialized text
and configuration access. Theme and physical invoker are borrowed target seams.
Keep any supplied theme alive until the presentation retires. UI operations run
on the UI thread; callbacks must not strongly retain their own menu.

`INyxMenuItem` and `INyxMenuItems` are specialized reference-counted, immutable
contracts. `Add` copies items into an independent plan; `Enabled` returns an
independent item. The original `TNyxMenuItem` / `TNyxMenuItems` spellings remain
aliases, without exposing mutable record fields. Presenters copy plans before
changing their own runtime states. Plans admit at most 256 unique named parts.
Actions/checks/radios/submenus require Nyx buttons; separators require Nyx
separators. A radio has a distinct
`NyxMenuGroup` reference, and at most one initial selection per group. Invalid
parts, duplicate registrations, physically disabled buttons and renderer Actions
refuse before presentation. Commands belong in `OnInvoke`: a renderer Action
would run before callback admission and could bypass logical disablement.

## Reuse submenu recipes and ordinary menu buttons

Create a content root from ordinary specialized controls, then capture an
independent immutable recipe. The document and its authored defaults may change
afterward without changing that recipe:

```pascal
LDensityColumn := NewNyxColumn('density-options');
LDensityColumn.Configure.Padding(8).Gap(4).Align(ncaStretch).Compound(True);

LComfortableButton := NewNyxButton('comfortable-density');
LComfortableButton.Text := 'Comfortable';
LComfortableButton.Configure.PartName(NyxPart('comfortable')).Variant(nvSecondary);
LDensityColumn.Add(LComfortableButton);

LCompactButton := NewNyxButton('compact-density');
LCompactButton.Text := 'Compact';
LCompactButton.Configure.PartName(NyxPart('compact')).Variant(nvSecondary);
LDensityColumn.Add(LCompactButton);
LDocument.AddComponent(LDensityColumn);

LDensityRecipe := NewNyxMenuRecipe(LDocument, NyxReusableRoot('density-options'),
  NyxMenuItems
    .Add(NyxMenuRadio(NyxPart('comfortable'), NyxMenuCommand('comfortable'),
      NyxMenuGroup('density'), True))
    .Add(NyxMenuRadio(NyxPart('compact'), NyxMenuCommand('compact'),
      NyxMenuGroup('density'), False)));

{ The parent content has an ordinary button whose named part is density. }
LParentItems := LParentItems.Add(NyxMenuSubmenu(NyxPart('density'), LDensityRecipe));

{ Once the target menu is attached, bind its ordinary specialized invoker. }
LMenuButton := NewNyxMenuButton(LOpenActionsButton, LRenderer.Events, LMenu,
  NyxMenu('Document actions'));
```

Keep `LMenuButton: INyxMenuButton` until its renderer/controller retires. It owns
the descriptor, router, menu and two cancellable weak registrations; releasing it
removes the registrations. Existing callbacks remain registered. Invocation uses
sequential click/main-key streams, rejects renderer Actions, and respects consumed
before-key input, disablement/visibility, modifiers and key repeats. Read-only
retains nonmutating invocation and branch navigation; read-only leaf commands
refuse activation. Each independently owned content root has its own policy scope.
`Click` toggles; Enter/Space/Down open at the first item and Up at the last. Both
adapters use the portable input contract; browser invokers additionally publish
`aria-haspopup`, `aria-expanded` and `aria-controls`, including silent closure.

A recipe owns a full copied document and immutable plan. `CopyDocument` returns
a caller-owned independent clone. Every branch is validated before any host opens;
invalid external counts, absent entries, duplicate parts and unknown kinds refuse
with `ENyxModel`. There are at most eight levels and 2048 total entries. External
recipe implementations must preserve their construction snapshot. Children mount
lazily and retain independent runtime state on reopening. Parent/child callback
leases are weak; a retained child does not keep its parent presenter alive.
Target hosts retain ancestor presentations only to keep borrowed invokers valid.

## Interaction and completion

These menu choices follow the applicable
[WAI menu pattern](https://www.w3.org/WAI/ARIA/apg/patterns/menubar/) and
[menu-button pattern](https://www.w3.org/WAI/ARIA/apg/patterns/menu-button/), checked
2026-10-06. The following table describes vertical command-menu families;
coordinated horizontal-bar behavior is described below.

| Input | Nyx behavior |
| --- | --- |
| Up / Down | Previous/next visible command; wrapping is configurable |
| Home / End | First/last visible command |
| Right / Enter / Space on a submenu | Open its child and focus the first item |
| Left in a submenu | Close that level and return to the parent item |
| Enter on a leaf | Activate and close the whole family before completion |
| Space | Toggle a check or select a radio without closing; actions close |
| Escape | Dismiss the current level and return focus to its invoker |
| Tab / Shift+Tab | Close the whole family; traverse from the original invoker |
| Printable character | Navigate using the shared typeahead engine |

Separators and hidden entries are skipped. Logical disabled commands remain
focusable and refuse activation. Use `LMenu.SetEnabled(Part, False)` or an item's
`.Enabled(False)`, leaving its physical button enabled. Browser roles and checked/
disabled attributes describe these states; LCL keeps actual focus and paints the
logical disabled appearance. Before-key callbacks can consume navigation first.

Checks/radios belong to this presentation, not the document's saved defaults.
`NyxMenuInvocation(AEvent)` returns the typed command, named part and checked
snapshot. Multiple registrations receive the same owned snapshot in order,
including when an earlier callback reopens or releases the menu. Close is silent;
`OnDismiss` uses the existing typed popover dismissal reasons. Runtime search and
focus never enter project Undo history. Descendant invocations forward the same
detached leaf command/part/check snapshot to parent registrations. Space toggles
remain inside the family; Enter completion closes every level first. Outside
presses close the family without stealing focus. Parent navigation closes any
open sibling child. On narrow hosts, placement can overlap ancestors to retain
viewport bounds; this is not a separate mobile drill-down presentation.

Typeahead accumulates prefixes during a configurable 1..60000 ms window, default
1000 ms. Repeated single characters cycle matches; an extended prefix first keeps
the current matching item. `ntmFolded` uses Unicode 17 full case folding;
`ntmExact` preserves case. Labels are never normalized, translated or rewritten.
It supports prefix navigation, not fuzzy matching or completion suggestions.
Adapters use decoded text and monotonic time; browser composition/modifier keys
remain with their ordinary input defaults.

## Coordinate a horizontal menu bar

`INyxMenuBar` composes an ordinary specialized `INyxRow`, named Nyx buttons and
independently owned menu families. It adds coordinated focus and dropdown
switching without introducing a second widget toolkit. Build each menu with the
physical heading returned by the same renderer, then bind the mounted row:

```pascal
LWorkspaceRow := RetainNyxControl(
  LRenderer.Root.Find('workspace-menu-bar')) as INyxRow;

LFileMenu := NewNyxBrowserMenu(LRenderer.FocusFor('bar-file'), LDocument,
  NyxPageRoot('thoughtful-actions'), LFileItems);
LEditMenu := NewNyxBrowserMenu(LRenderer.FocusFor('bar-edit'), LDocument,
  NyxPageRoot('thoughtful-actions'), LEditItems);

LWorkspaceBar := NewNyxBrowserMenuBar(LWorkspaceRow, LRenderer,
  NyxMenuBar('Workspace commands')
    .Wrap(True)
    .HoverSwitch(True)
    .TypeAhead(NyxTypeAhead.WindowMilliseconds(1200).Match(ntmFolded)));
LWorkspaceBar
  .Add(NyxPart('file'), LFileMenu, NyxMenu('File'))
  .Add(NyxPart('edit'), LEditMenu, NyxMenu('Edit'));
LWorkspaceBar.OnInvoke.Subscribe(LCommandCallback);
```

Native authoring substitutes `NewNyxLCLMenu` and `NewNyxLCLMenuBar`; both consume
the same typed row/parts, item plans, submenu recipes and policy values. Each
heading opens a dropdown. Direct command/check/radio headings, vertical bars and
an application-wide Alt/F10 shortcut are not part of this horizontal contract.

| Input/context | Coordinated behavior |
| --- | --- |
| Tab entry | One visible heading participates; returning remembers that heading |
| Left / Right on a heading | Move to the previous/next visible heading; optional wrapping |
| Home / End | Focus the first/last visible heading |
| Enter / Space / Down | Open the heading's first menu item |
| Up | Open the heading's last menu item |
| Left in a root dropdown | Close its family and open the previous heading's first item |
| Right on a leaf at any level | Close the family and open the next heading's first item |
| Right on a submenu / Left in a nested child | Open that child / return one level |
| Tab / Shift+Tab from any level | Close the family and leave the whole bar |
| Mouse entry while a family is open | Switch to another enabled heading when hover is enabled |

Touch entry never performs hover switching. Hidden headings are skipped; logical
disabled headings retain arrow focus but refuse opening. Printable decoded text
uses the shared Unicode typeahead policy; browser composition and modified input
keep their defaults. Before-consumed keys refuse navigation. A repeated opening
key does not repeatedly reopen the family. `SetEnabled` changes runtime admission;
it does not rewrite document defaults. Call `Refresh` after changing a mounted
heading's visibility/physical enablement. Remounting requires retiring and
rebinding the coordinator.

The bar retains row/families and weak subscription leases; it borrows the target
renderer. Release it before remounting or destroying that renderer. Its retirement
closes families, cancels all registrations and restores prior heading/row
accessibility, Tab stops and native text handlers. Independently retained menus
remain usable afterward. Callbacks must not strongly retain their own bar; they
may release the last external owner during ordered completion. `OnInvoke` forwards
the detached `NyxMenuInvocation` snapshot with the bar as source/target and its
heading as `OriginID`, preserving the leaf command/part/check state.

Admission requires unique parts/buttons, distinct closed root families, no
renderer actions and sequential input/completion streams. It never silently
changes an existing stream's policy. One exact mounted row admits one coordinator;
independently materialized copies can each own one. A foreign family coordinator
or wrong physical heading anchor refuses without replacing prior registrations.
Built-in target popovers expose optional `INyxBrowserPopoverAnchor` /
`INyxLCLPopoverAnchor` identity queries. They return Boolean observations and do
not publish dangling target pointers. Custom target families must provide that
trait and the optional portable `INyxMenuFamilyInput` registration seam.

Keyboard/role design follows the
[WAI menu and menubar pattern](https://www.w3.org/WAI/ARIA/apg/patterns/menubar/)
(checked 2026-10-07). Current evidence covers installed Win32 and headless
Chromium controls, including real host Tab/Shift+Tab at CSS 1100 and 390. This does
not establish hardware, IME, assistive technology or another widgetset. Narrow
cascades currently overlap ancestors to stay in bounds; a mobile drill-down
presentation and broader visual qualification remain open.

The coordinator can also consume a saved immutable declaration on the ordinary
row. Generated source and ordinary application mounting use the same public
contract described below. Direct runtime factories remain useful for transient
menus and custom adapters.

## Save a coordinated bar with the application

Declare ordinary named button parts in a compound `INyxRow`, and define each
dropdown through the document's `Menus` registry. Attach an immutable grouping:

```pascal
LWorkspaceMenuBarRow.Configure
  .MenuBar(
    NewNyxMenuBarDefinition(
      NyxMenuBar('Workspace commands')
        .Wrap(False)
        .HoverSwitch(True)
        .TypeAhead(NyxTypeAhead.WindowMilliseconds(1700)))
      .Heading(NyxPart('file'), NyxMenuRef('document-actions'))
      .Heading(NyxPart('edit'), NyxMenuRef('editing-actions'))
      .Heading(NyxPart('view'), NyxMenuRef('view-actions'), False))
  .Done;
```

Import `nyx.menu.types`, `nyx.menu.bar.declarations`, `nyx.menu.declarations`
and `nyx.controls`. Each fluent `Heading` returns an independent immutable plan;
attachment copies public getters, including foreign implementations. The row,
its parts and all referenced menu/content definitions must belong to the design.
One bar admits 1–64 ordered distinct headings. Several headings may reference
one menu definition; each mounted family owns independent check/radio state.
The Boolean heading argument is logical enablement. Its physical button stays
enabled so keyboard navigation remains available. A physically disabled button,
renderer action, competing standalone attachment or duplicate physical heading
refuses candidate admission.

Reusable row instances inherit the definition's grouping. `NoMenuBar` is an
explicit local mask; `InheritMenuBar` removes only the local declaration. Clones,
standalone dependency discovery and source reconstruction retain that distinction.
Grouping is structural, so platform/viewport/presentation scopes refuse it;
ordinary visibility, layout, captions and whole-content presentation recipes
continue to use the shared conditional contracts.

Ordinary browser/LCL applications mount saved groups automatically. Commands
reach the row's named `nseActivate` stream in registration order, with the exact
heading runtime ID as `OriginID`. `NyxMenuInvocation` reads the typed command and
checked snapshot. `INyxMenuBarBindings` is an optional capability on the existing
`Application.Menus` owner: `Bar(exactRuntimeRow)` observes the coordinator;
`Menu(exactRuntimeHeading)` observes each family. Release observations before
page navigation/application destruction. No runtime state rewrites saved defaults.

`nyx_menus` accepts an exact authored `row`, with up to 16 headings per page
(8 default). `barScope` defaults to `effective`, including reusable inheritance;
`local` inspects only the authored declaration. `localDeclared` distinguishes a
local mask from absence. Label text pages by Unicode scalars. Queries return the
same revision without changing selection/history. Group `menu-bar-set`
(`definition: null` masks) and `menu-bar-inherit` with related menu/content changes
in one revision-aware paired transaction. Pascal producers use
`NyxConfigureMenuBar`, `NyxNoMenuBar`, `NyxInheritMenuBar` and `NyxMenuPatch`.
Strict interchange rejects coercion, missing/unknown fields and invalid policies.
Wire version 7 retains opaque `menuBar` fields as extensions in older versions
and refuses collisions during explicit promotion.

This saved contract is qualified through direct candidate semantic dispatch and
actual generated browser/Win32 applications. Studio's public bar-authoring form
is described below. The observing LAN release still serves its preceding schema;
authenticated new-backend delivery remains with the
[existing workflow owner](../TODO/NS-4_agent-workflows_01.md).

```powershell
./tools/build.ps1 -Target menu-bar-authoring -MenuSourceDirectory build/menu/source
```

Use the exact exported companion that contains the Workspace commands page.
Pascal qualifies the paired declaration contract and exports its accepted unit;
ordinary native/browser consumers compile that exact unit. The target stages
`menu-bar-declarations.html` and `menu-bar-declarations-controls.html`, plus the
Pascal real-Tab observer, without starting listeners or replacing projects.

## Author a saved bar in Studio

Selecting a row exposes the reusable `NewNyxMenuBarEditor` compound in Properties.
Import `nyx.menu.bar.editor` to use that same public form in another Nyx tool.
It edits the accessible label, wrapping, mouse-entry switching, typed search
policy and ordered named-button/menu choices. Each heading has its own logical
enabled default. Choice captions are bounded for display; their separate maps
retain the exact part paths and menu references, including supplementary Unicode.

The card exposes named parts such as `label`, `search-window`, `heading-0/menu`
and `new-heading/part`. These are specialized Nyx controls; creators can customize
them through the ordinary compound contract. Events retain the compound source
and physical child origin. Studio resolves bar actions inside that source subtree
and captures a complete typed intent instead of changing the borrowed document.

The form distinguishes local grouping, inherited grouping and an explicit mask.
Save creates a local override from the effective settings. Add and reorder also
capture the entire edited plan as one paired source/design Undo step. Heading
removal requires its own reviewed confirmation; suppression requires the displayed
whole-bar warning. Restore inheritance removes only the local declaration. Saved
menu definitions and other reusable instances retain independent ownership.

Incomplete input remains an independent presentation draft through panel switches
and shell rebuilds. A changed owner or local/effective baseline retires that draft.
`CaptureNyxMenuBarEditor` returns copied immutable intent and the mounted baseline;
hosts must recheck it before publication. Studio checks at capture and again in
the independent source worker. Stale queued forms and pending Pascal drafts refuse
without publishing over accepted work. Mask and inheritance remain distinct typed
actions even though neither returns a definition.

```powershell
./tools/build.ps1 -Target menu-bar-editor -MenuAuthoringSourceDirectory build/menu-bar-saved/maintained/generated
```

Supply the exact saved-bar companion export. This target checks actual Win32 form
controls and ordinary native Studio, then compiles the HTTP browser form, matched
Pascal source worker and full browser Studio. It stages `menu-bar-editor.html`
without starting a listener or replacing a project. Use the maintained ready-capture
driver with `data-result` at desktop and narrow CSS widths. These focused browser
controls exercise ordinary inspector composition and worker publication; they do
not establish the full browser Studio or authenticated observing delivery journey.

## Reproduce the companion and qualify hosts

The Pascal semantic client authors the English **Thoughtful actions** companion
in one connection-owned empty MCP review: one revision-aware grouped transaction,
bounded source reads, exact paired Undo/Redo and canonical LF export. It retires
the review and preserves the primary project. Enroll the MCP configuration first:

```powershell
./tools/build.ps1 -Target menu-companion -DesignerMCPConfig .codex/config.toml -MenuSourceDirectory build/menu/source
./tools/build.ps1 -Target menu -MenuSourceDirectory build/menu/source
./tools/build.ps1 -Target menu-bar -MenuSourceDirectory build/menu/source
```

The menu target runs checked native controls and ordinary Studio integration,
then stages the exact same generated companion for HTTP browser execution. Serve
`build/menu/maintained/browser` on an existing admitted fixture host. Use the
maintained ready-capture tool with `menu.html` and the `data-menu` marker at desktop
and narrow widths. The Studio menu observer exercises actual pointer activation,
Chromium Tab defaults, Events navigation and public contextual help in `index.html`.
For an observing server, supply the enrolled repository and exact ordinary
workspace as the observer's fourth/fifth arguments, with that same workspace in
the URL's `workspace` query. Bounded semantic context establishes the selected
component; the connected toolbar's Builds control participates in ordinary Tab
order. Desktop/390 observing evidence and preserved-state receipts are in
[WORK.md](../WORK.md#current-return-path-observing-menu-families--2026-10-07).

The current companion also includes an English **Workspace commands** page with
named File/Edit/View headings. The `menu-bar` target runs its native consumer and
stages `menu-bar.html` plus the Pascal `nyx_menu_bar_observer`. Invoke that observer
with the admitted HTTP fixture URL, a new evidence directory and CSS width. It
captures a three-level family, sends real Chromium Tab/Shift+Tab and requires the
Pascal fixture's terminal result. It starts no HTTP/MCP listener and edits no
ordinary project. Current results are 42 semantic composition/history checks,
44 native and 46 per browser width; all native owners retire without heap leaks.

Runtime-only presentation/command/recipe plans remain available independently
of the saved declarations above. Public Studio bar forms, observing delivery and
broader semantic authoring remain with [the existing workflow owner](../TODO/NS-4_agent-workflows_01.md).
Mobile drill-down presentation, live menu binding, assistive technology,
hardware/IME, other widgetsets/DPI and full production accessibility remain open.
Compiled source admission alone establishes none of these interactions.

The maintained menu target also stages `nyx_studio_workspace_observer` and an
isolated `studio-workspace-conflict.html` fixture. The observer takes URL, capture
directory, CSS width and height, then optional enrolled root and exact workspace
(`primary` omits the MCP context). It qualifies compact/short navigation, details
collapse, actual host touch resizing, canvas expansion/restoration and retained
source. Optional bounded semantic queries check revision/navigation/history.
Use the recovery fixture only on an isolated test origin and fresh profile:
its public Pascal recovery seed intentionally creates an independent local pair.
Author a different shared title through MCP before using that conflict journey.
It never represents permission to overwrite an observing user's recovery.
