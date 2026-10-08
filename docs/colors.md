# Typed RGB color fields

[Components](components.md) · [Building](building.md) · [Current evidence](../WORK.md)

`nyx.colors` supplies the portable, immutable `TNyxRGBColor`. Ordinary authoring
uses signed whole RGB channels, checked from zero through 255 before conversion.
The default value and `NyxNoColor` mean absence; defined black is `NyxRGB(0, 0, 0)`.
Reading absent channels raises `ENyxColorValue`. No DOM or LCL color type enters
the shared contract.

```pascal
uses
  nyx.controls, nyx.colors, nyx.contract;

var
  LAccentColor: INyxColor;
begin
  LAccentColor := NewNyxColor('accent-color')
    .WithColor(NyxRGB(115, 87, 232));
  LAccentColor.Contract.Value(NyxRGBDomain.Choices([
    NyxNoColor, NyxRGB(115, 87, 232), NyxRGB(18, 52, 86)]));
end;
```

`INyxColor.ColorValue` / `WithColor` and `Configure.Value(TNyxRGBColor)` retain
specialized, managed authoring. Hosts retain the returned interface or transfer
the control into an owning document tree. Runtime state stores remain independent
of document defaults. RGB state uses exact text at its explicit persistence and
binding boundary; the color domain admits that text before publishing it.

`TNyxRGBColor.FromText` admits empty text or exactly six ASCII hexadecimal digits
after `#`. `TryNyxRGB` returns False and absence for malformed input. Whitespace,
names, shorthand, alpha and wide-gamut syntax refuse. Numeric construction emits
lowercase; imported `#AbCdEf` retains its exact spelling through persistence,
generation and history. `SameColor` and domain choice membership compare channel
readings, so case variants match a choice and duplicate readings refuse. Absence
is a separate choice. RGB domains have no numeric bounds or steps.

`NyxRGBDomain` creates that text domain. `NyxRGBDomain(Existing.Definition)` copies
and enriches a compatible text domain, preserving choices; non-text, calendar and
clock domains refuse. Named slots and instance-owned overrides use the same typed
values with independent ownership. Crafted generated source declares `INyxColor`
and emits `NyxRGB` / `NyxNoColor`; the explicit typed `FromText` constructor is used
when an imported spelling must survive exactly.

## Target controls

The reusable `TNyxLCLColorField` groups an exact hex editor with a picker button.
Its owned nonmodal window uses standard LCL palette and RGB trackbars, a swatch,
and Use color, Cancel and Clear actions. Opening absent data only proposes black.
Cancel preserves accepted state; Use color and Clear run ordinary domain/binding
admission before ordered callbacks. A rejected choice stays visible with a
diagnostic. Changing the accepted context, choices or effective ancestor policy
revokes the open proposal. The renderer disconnects borrowed callbacks before
retirement; publication and focus return use a weak lifetime lease.

The browser groups the same exact text editor with a native color chooser. Its
opaque, limited-sRGB picker is a proposal surface; the editor represents absence
and imported spelling independently. Unchanged refresh preserves an open proposal.
An accepted-value/domain/interaction change revokes its publication context.
Chooser `change` enters the shared commit path; intermediate text input remains a
draft until editing completion. Cancellation never publishes a value. Native
chooser presentation is browser-owned and cannot establish identical dialog
appearance or hardware behavior across browsers. The HTML standard includes
alpha and wider color-space controls; this Nyx RGB contract deliberately exposes
opaque eight-bit sRGB. [HTML Living Standard](https://html.spec.whatwg.org/multipage/input.html#color-state-(type=color)).

Both adapters use the shared intrinsic color domain and preserve copied choices.
Invalid completed text restores the accepted value with a binding diagnostic;
it does not silently become black or issue a successful change callback. No
control instance owns a document or creates a reference cycle into its tree.

## Qualification and limits

The maintained `color-fields` build requires an exact English semantic export at
`build/color-fields/seed/nyx.generated.view.pas`, or `-ColorSourceDirectory` pointing
to its directory. Create an owned empty review through `nyx_reviews`; use one
revision-aware `nyx_transaction` for a page `color-workshop` and its three color
children: `accent-color` = `#7357e8`, `optional-color` = empty, and `imported-color`
= `#AbCdEf`. Page identity and control IDs are fixture inputs. Export the accepted
source through bounded `nyx_source` windows and discard that owned review after
checking paired Undo/Redo. No replacement of the primary project is required.
The local fixture then explicitly enriches this unchanged service export with
typed choices/state and two independent reusable instances.

```powershell
& ./tools/build.ps1 -Target color-fields
```

Pascal owns the checked value/domain/source/history/semantic journey, exact emitted
builder execution and ordinary native control tests. The shell stages both
browser consumers, Studio and its source worker, native Studio and backend, and
checks that both compilers reject string, Boolean and fractional RGB channels.
It starts no browser or listener and edits no active project.

Current Win32 execution and shared native checks are recorded in WORK.md. Browser
compilation is separate from execution. Current browser/phone, trusted chooser
input, IME, accessibility, DPI/other widgetsets, production visuals and budget
qualification remain open. Both adapters report Basic support; this packet does
not establish full renderer parity or deployed Studio behavior.
