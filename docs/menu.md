# Managed command menus

[Managed controls](managed-controls.md) · [Contextual views](popover.md) ·
[Current evidence](../WORK.md)

`INyxMenu` gives ordinary specialized Nyx buttons and named parts a shared command
contract. Browser and LCL factories present the same independently owned content.
Studio's **Actions** button consumes `INyxMenuButton`; its **Inspect** submenu
uses the same managed recipes for Properties, Events and component help.

## Compose once, attach through an adapter

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
[WORK.md](../WORK.md#current-return-path-command-menu-observing-release--2026-10-06).

Menu presentation/command/recipe plans are explicit runtime attachment, not
serialized authoring declarations or MCP plan mutations. Their semantic
admission/generation remains with
[the existing workflow owner](../TODO/NS-4_agent-workflows_01.md). Menubars, mobile
drill-down presentation, live menu binding, assistive technology,
hardware/IME, other widgetsets/DPI and full production accessibility remain open.
Compiled source admission alone establishes none of these interactions.
