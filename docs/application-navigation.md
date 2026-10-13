# Application page navigation

[Architecture](architecture.md) · [Selection appearance](selection-styles.md) ·
[Runtime bindings](bindings.md)

Multi-page browser and LCL applications build their navigation from public Nyx
toolbar/button interfaces. Page Text supplies the caption; empty text falls back
to the stable page ID. Captions, typed targets and theme are independent snapshots.
The source document remains borrowed by the application and must outlive it.

Both hosts expose the optional `NavigationState` capability. Its `CurrentPage`
is a typed `TNyxControlRef`; `IsCurrent(NyxControl(...))` reads a navigation button's
status. Single-page and unmounted hosts return nil. ShowPage confirms the new
selection after mounting that page; a missing page refuses before selection
changes. Resolving an unconsumed click only returns an intent. It never selects
or mounts a page.

The default appearance uses primary for the current page and default for other
pages. Configure a copied enum policy before mounting:

```pascal
uses nyx.selection, nyx.types;

LApplication.ConfigureNavigation(
  TNyxSelectionOptions.Defaults
    .Selected(nvSuccess)
    .Unselected(nvSecondary));
```

Use `.Styling(nssAuthored)` to keep the navigation parts' authored variants.
Current-page identity and accessibility meaning still follow page changes.
The host owns selection publication; use ShowPage to navigate its application.
ConfigureNavigation refuses after mount. These runtime options do not rewrite
the application's authored pages, state defaults or exported project.

The independent `NewNyxPageNavigation` recipe also exposes this capability through
`NyxPageNavigationState(recipe)`. For a custom host, SelectPage confirms its typed
target and Project copies the variant policy into the host's realized navigation
tree. The complete captured button group is validated before any variant changes.
Additional toolbar parts and button descendants retain creator ownership.
The adapter then synchronizes its existing controls and supplies platform
accessibility meaning. No source document, target control or renderer is retained
by the recipe/state capability. Operations belong to the UI thread. Renderers
retire before the final recipe owner; independently retained state remains safe.

The standard hosts retain notifying navigation buttons when replacing the main
page. Browser navigation publishes exclusive `aria-current="page"`, following
[WAI's current-item technique](https://www.w3.org/WAI/WCAG21/Techniques/aria/ARIA26).
Current-page navigation does not create `aria-pressed` toggle state.
LCL supplies “Current page” as the current button's accessible description,
preserving its exact caption/name and retaining the description through ordinary
value/layout synchronization. Actual assistive-technology interpretation and
other widgetsets require separate qualification.

Mounted captions remain snapshots. Live localization/state-driven caption changes,
complete native keyboard/hardware/accessibility, production visual quality and
full component/parity acceptance retain their existing task owners.
