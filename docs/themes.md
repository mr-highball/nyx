# Semantic themes

[Components](components.md) · [Architecture](architecture.md) · [Builds](building.md)

Use `nyx.design.tokens` for application authoring. `TNyxThemeTokens` is an
immutable fluent value with thirteen typed RGB roles and three integer metrics.
An omitted role inherits the renderer's independent base. For example:

```pascal
SetNyxThemeTokens(LDocument, NyxThemeTokens
  .Accent(NyxRGB(53, 195, 165))
  .AccentText(NyxRGB(22, 19, 36))
  .Danger(NyxRGB(180, 35, 24))
  .DangerText(NyxRGB(255, 255, 255))
  .Radius(18)
  .ControlRadius(10)
  .FontSize(16));
```

`NyxThemePreset(ntpLight)` and `NyxThemePreset(ntpDark)` return complete copied
palettes which can be enriched through the same fluent methods. Partial values
retain exact presence, ordering and imported hexadecimal case. `ColorValue` and
`MetricValue` require a declared role; `NyxDesignTokens` returns the effective
palette instead. `FromData` is the strict persistence boundary, rejecting unknown
roles, absent colors, wrong scalar types and values outside the ranges below.

`SetNyxThemeTokens` replaces the exact document declaration after validation;
`ResetNyxThemeTokens` removes it and reveals inherited colors/metrics. These
functions borrow the document and never own or mutate a renderer's base theme.
Generated Pascal uses these public typed calls rather than extension JSON. Source
admission supports the fluent role methods and closed light/dark preset factory;
existing extension-based source remains an explicit compatibility boundary.

Studio's **Project → Theme** consumes `NewNyxThemeEditor` from `nyx.theme.editor`.
Its RGB fields and integer spinners are ordinary specialized Nyx controls. Each
role has an override switch. Light/Dark buttons prefill a disposable proposal;
only **Apply theme** publishes it. **Restore inherited theme** removes the local
declaration. Both actions use one ordinary paired design/Pascal Undo step. A
pending Pascal draft refuses mutation; a changed theme refuses stale forms.
The canvas consumes application tokens while Studio chrome retains its own theme.

Studio's Pascal editor has an independently owned source document too.
`NewNyxStudioCodeDocument` in `nyx.studio.view` gives both controllers the same
typed dark palette; the ordinary Nyx code editor consumes its surface/text roles
through each adapter. The caller owns the returned document and can replace its
tokens through `SetNyxThemeTokens`. Inline/modal host moves retain that document
and its editor. Source colors do not depend on unscoped browser chrome selectors
and do not change the application or shell palette.

`TNyxThemeEditorDraft` owns only copied input and exact local/effective context.
Collapsing the form and switching compact panels retain it. Changed declarations,
base palettes, field kinds or explicit project replacement retire it. The browser
stores it per workspace in version-6 editor preferences; versions 2–5 migrate
without importing a theme proposal. It never enters exported designs or history.

`tools/build.ps1 -Target theme-authoring` consumes an English MCP review export
at `build/theme-authoring/seed/nyx.generated.view.pas`, executes shared/actual
native Studio checks, then executes the exact emitted Pascal. Matching browser
consumers are staged separately. Browser execution, physical-phone input and
observing rollout remain open; native success and compilation do not establish
those outcomes. See [current evidence](../WORK.md#current-return-path-typed-theme-authoring--2026-10-07).

`TNyxTheme` is a portable palette in `nyx.theme`. Its fields have the same meaning
in the browser and Lazarus adapters. `Create` selects the light defaults;
`Create(True)` selects dark defaults. A renderer creates and owns its default
theme, or borrows a caller-supplied theme. Free a borrowed theme after its renderer.

| Token | Meaning |
| --- | --- |
| Background | Page and surrounding workspace face |
| Surface | Cards, panels and editable/data control faces |
| Text | Primary content and editable values |
| Muted | Supporting text and field captions |
| Border | Resting surface and field outlines |
| Accent | Primary actions, links and input focus feedback |
| AccentText | Text over a primary accent action |
| Danger / DangerText | Destructive/error action fill and foreground |
| Success / SuccessText | Positive action fill and foreground |
| Warning / WarningText | Caution action fill and foreground |
| Radius | Card/panel corner radius; default 12 logical pixels |
| ControlRadius | Button/input-frame corner radius; default 8 logical pixels |
| FontSize | Body/control font size; default 14 logical pixels |

Colors must use `#RRGGBB`; either hexadecimal letter case is accepted. Radii
accept 0..1000, and body font size accepts 1..256. `Validate` and `CSS` reject
malformed fields with `ENyxModel`. Both renderers validate before mounting a
candidate, so an invalid borrowed palette preserves the accepted view.
`NyxThemeRGB` exposes the same validated RGB decoding without platform color types.

```pascal
LTheme := TNyxTheme.Create(True);
LTheme.Accent := '#123456';
LTheme.AccentText := '#fedcba';
LTheme.Border := '#8899aa';
LTheme.Radius := 23;
LTheme.ControlRadius := 19;
LTheme.FontSize := 17;
LTheme.Validate;
```

Pass that palette to `TNyxBrowserRenderer.Create(LTheme)` or
`TNyxLCLRenderer.Create(LTheme)`. Set fields before rendering. To apply an
intentional palette change to a mounted view, call `Render` again. Native controls
copy their palette fields during construction; changing a theme object alone does
not recolor existing controls. Heading, badge and field-caption recipes keep
their own typographic hierarchy instead of treating every text size as body size.

The browser adapter scopes palette variables and body typography to a unique
renderer host. Separately themed views can coexist, including nested previews.
`CSS` optionally accepts a trusted application root selector; its selector
argument must never come from editable design data.

The native adapter uses public `TNyxLCLButton` and `TNyxLCLSurface` controls from
`nyx.widgets.lcl`. Buttons retain Lazarus `TCDButton` tab, Space/Enter, pointer
capture and action behavior while painting the palette. Surfaces retain `TPanel`
ownership and layout. Input frames contain ordinary `TEdit`, `TMemo`, `TSpinEdit`
and `TComboBox` controls, retaining their native editing behavior. Caption labels
target the editable control, which also receives the node's accessible name.
Focused input frames paint an accent border; disabled button activation is refused
when the button or an ancestor is disabled.

`ControlFor(ID)` borrows a native node's projected host. `InputFor(ID)` borrows
its actual input independently of caption/frame nesting, or returns nil for a
mounted non-input. Both accept runtime or design identity and raise for an
unmounted identity. Neither returned control may be freed by the caller.
Custom factories may continue returning ordinary Lazarus controls, including
`TButton`; a factory owns its own presentation contract.

The standard Button consumes all seven closed `TNyxVariant` choices. Primary,
danger, success and warning use their independent fill/foreground pairs.
Secondary uses a surface fill with accent text and outline. Ghost keeps the
surrounding surface visible and omits its resting outline; focus and activation
remain available. Native ghost painting uses the standard solid parent backdrop;
custom image/gradient surfaces need their own adapter. Selection policies can use
these variants without changing scalar values, events or binding admission.

```pascal
LDelete := NewNyxButton('delete-draft').WithText('Delete draft');
LDelete.Configure.Variant(nvDanger).Done;
SetNyxThemeTokens(LDocument, NyxThemePreset(ntpDark)
  .Danger(NyxRGB(253, 162, 155))
  .DangerText(NyxRGB(30, 34, 48)));
```

Default light/dark idle button text pairs are checked against the unrounded
4.5:1 [WCAG text contrast threshold](https://www.w3.org/WAI/WCAG22/Understanding/contrast-minimum.html).
This does not establish whole-application accessibility or every interaction
state. Custom palettes are admitted without contrast clamping. Older partial
declarations inherit the added roles. The theme form's version-two draft has
sixteen fields; version-one drafts retain their original ten field ordinals and
mark new roles inherited. Exact context guards still govern restoration.

`tools/build.ps1 -Target button-palette -SelectionSourceDirectory <semantic-export>`
uses an exact MCP selection companion and explicit current-source enrichment.
`-ButtonPaletteEvidenceDirectory` selects a new packet. It executes checked
native controls and compiled generated Pascal, then stages the matched browser
consumer. HTTP execution and observing-release installation remain separate.

Native automatic input/button heights account for the selected font size. Native
rows stack caption controls when equal-width cells would clip their LCL preferred
size. This handles the sample's narrow option row; general flex sizing, responsive
container policies and high-DPI validation remain in the renderer/parity tasks.
Rounded surfaces paint the parent's backdrop outside their face. They do not yet
clip child controls to a curved region. Selection glyphs, combo arrows and
scrollbars retain widgetset painting. Date/time/color are still documented text
fallbacks; the theme foundation does not establish production picker behavior.

The portable suite checks validation and palette serialization to CSS. The browser
journey checks computed custom styles, coexistence and failed-theme recovery.
Native journeys exercise Space/Enter, focus, disabled ancestors, large fonts,
Unicode accessible names and retained editing state. `-Target visual` captures
five native sample variants and checks actual surface/button/input pixels,
including focus/blur borders. Browser `visual.html` renders the same sample with
15 computed-style/layout checks; `?theme=dark`, `?theme=custom` and `?parts=1`
select the corresponding variants. Use 900×900 desktop and `?narrow=1` for a
390×900 narrow host. The fixture records actual viewport/page widths: headless
Edge on Windows has a larger minimum viewport, so screenshot size alone does not
prove a narrow layout.
These checks do not establish physical-device or assistive-technology acceptance.
