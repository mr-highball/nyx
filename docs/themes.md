# Semantic themes

[Components](components.md) · [Architecture](architecture.md) · [Builds](building.md)

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
