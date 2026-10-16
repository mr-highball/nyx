# Application page navigation

[Architecture](architecture.md) · [Selection appearance](selection-styles.md) ·
[Runtime bindings](bindings.md)

Multi-page browser and LCL applications build their navigation from public Nyx
toolbar/button interfaces. Page Text supplies the caption; empty text falls back
to the stable page ID. Captions, typed targets and theme are independent snapshots.
The source document remains borrowed by the application and must outlive it.

Both hosts expose the optional `NavigationState` capability. Its `CurrentPage`
is a typed `TNyxControlRef`; `IsCurrent(NyxControl(...))` reads a navigation button's
status. Single-page, explicitly focused and unmounted hosts return nil. ShowPage confirms the new
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

## Focused application views

Both application hosts accept the same copied, portable initial-view option:

```pascal
uses nyx.application.view, nyx.types;

LApplication.ConfigureView(
  TNyxApplicationView.Focused(NyxControl('Details')));
```

Configure before native Mount/Run or browser Run. It selects one exact authored
page, reusable or descendant and suppresses automatic page navigation;
NavigationState is nil for that focused mount. An unknown control refuses before
mounting. Default options preserve normal first-page/navigation behavior.
ConfigureView refuses after mount. Explicit ShowPage remains ordinary application
navigation to an available page, without changing the configured focus policy.
These runtime options do not rewrite an authored document or exported design.

Focused contexts can retain pages solely because a named canvas or menu requires
them. They remain source/binding context, not additional automatically displayed
pages. A reusable-only context needs no synthetic authored page. Runtime state,
collections and resource locale/fallback retain their independent existing
owners; descendant bindings recompose against that same collection context.
Containing-root and hidden-page prototypes still validate proposed values.

`CloneNyxViewDocument` keeps simple legacy isolated roots as their first page.
When a retained diagram, canvas or reusable reference needs a root in its original
partition, the clone keeps that complete original root instead of duplicating or
renaming IDs. Consumers resolve the requested node with its original identity,
using `Find` or the typed application-view option, rather than assuming Pages[0].
Named diagram/canvas catalogs retain exact definitions/order and complete root
dependencies; ordinary reusable/menu dependencies remain transitive. The returned
document is independently owned. Mounted hosts borrow it and retire first.

`tools/test-focused-context.ps1 -EvidenceDirectory <fresh-output>` runs portable
source/ownership checks, existing composition regressions, actual native focused
controls at wide/compact allocations and exact whole constructors. It reuses the
existing MCP-exported menu fixture; override its location with
`-MenuSourceDirectory` (the original `build/menu-bar/source` export, before typed
bar configuration). Add `-CompilerJobs` for ordinary backend compiler jobs and
an actually executed handwritten constructor. Matching browser consumers compile
only. No HTTP host, enrollment refresh or foreground capture is part of the
command. WORK.md separates accepted receipts from browser execution, full visual/
accessibility quality and delivery.
