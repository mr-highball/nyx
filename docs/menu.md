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

Only documents with menu meaning use codec version 6; existing versions 1–5 stay
supported. Registry limits are 64 definitions, 256 items per definition, eight
submenu levels and 2048 expanded family items. Missing roots/parts, duplicate
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
directory and editor HTTP base. Its source requires the new twenty-one-tool
schema; the older observing release remains on twenty tools until refreshed.

`tools/build.ps1 -Target menu-authoring` checks the public portable/source fixture
and stages both target consumers. Add `-MenuAuthoringSourceDirectory` to consume
the exact exported `nyx.generated.view.pas` and qualify native Studio Interact.
Optional `-DesignerMCPConfig` plus `-HttpURL` first runs the authenticated review
into that explicit source directory. Serve `menu-declarations.html` and
`menu-declarations-controls.html` on an admitted HTTP host; the maintained Pascal
ready-capture driver qualifies real-clock completion and desktop/narrow captures.

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
2026-10-06. This implements vertical command-menu families; a horizontal menubar
contract remains open.

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

## Reproduce the companion and qualify hosts

The Pascal semantic client authors the English **Thoughtful actions** companion
in one connection-owned empty MCP review: one revision-aware grouped transaction,
bounded source reads, exact paired Undo/Redo and canonical LF export. It retires
the review and preserves the primary project. Enroll the MCP configuration first:

```powershell
./tools/build.ps1 -Target menu-companion -DesignerMCPConfig .codex/config.toml -MenuSourceDirectory build/menu/source
./tools/build.ps1 -Target menu -MenuSourceDirectory build/menu/source
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

Runtime-only presentation/command/recipe plans remain available independently
of the saved declarations above. The general editor menu-authoring interface
remains with [the existing workflow owner](../TODO/NS-4_agent-workflows_01.md).
Menubars, mobile
drill-down presentation, live menu binding, assistive technology,
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
