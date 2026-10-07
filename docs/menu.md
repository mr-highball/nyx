# Managed command menus

[Managed controls](managed-controls.md) · [Contextual views](popover.md) ·
[Current evidence](../WORK.md)

`INyxMenu` gives ordinary specialized Nyx buttons and named parts a shared command
contract. Browser and LCL factories present the same independently owned content.
Studio's **Actions** menu consumes this public contract for Undo, Redo, Inspector
navigation and component help.

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

Plans admit at most 256 unique named parts. Actions/checks/radios require Nyx
buttons; separators require Nyx separators. A radio has a distinct
`NyxMenuGroup` reference, and at most one initial selection per group. Invalid
parts, duplicate registrations, physically disabled buttons and renderer Actions
refuse before presentation. Commands belong in `OnInvoke`: a renderer Action
would run before callback admission and could bypass logical disablement.

## Interaction and completion

These menu choices follow the applicable
[WAI menu pattern](https://www.w3.org/WAI/ARIA/apg/patterns/menubar/), checked
2026-10-06. This implements a vertical command menu; submenu/menubar behavior
remains open.

| Input | Nyx behavior |
| --- | --- |
| Up / Down | Previous/next visible command; wrapping is configurable |
| Home / End | First/last visible command |
| Enter | Activate and close before completion |
| Space | Toggle a check or select a radio without closing; actions close |
| Escape | Dismiss and return focus to the invoker |
| Tab / Shift+Tab | Close and leave the menu in the requested direction |
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
focus never enter project Undo history.

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
./tools/build.ps1 -Target menu-companion -DesignerMCPConfig .codex/config.toml
./tools/build.ps1 -Target menu
```

The menu target runs checked native controls and ordinary Studio integration,
then stages the exact same generated companion for HTTP browser execution. Serve
`build/menu/maintained/browser` on an existing admitted fixture host. Use the
maintained ready-capture tool with `menu.html` and the `data-menu` marker at desktop
and narrow widths. The Studio menu observer exercises actual pointer activation,
Chromium Tab defaults, Events navigation and public contextual help in `index.html`.

Menu presentation/command plans are explicit runtime attachment, not serialized
authoring declarations or MCP plan mutations. Submenus, menu-button invocation
shortcuts/expanded-state semantics, live menu binding, assistive technology,
hardware/IME, other widgetsets/DPI and full production accessibility remain open.
Compiled source admission alone establishes none of these interactions.
