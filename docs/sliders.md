# Typed numeric sliders

Nyx sliders use real HTML range inputs and LCL trackbars behind one portable
numeric scale. Applications, bindings and callbacks observe numbers. Internal
thumb positions are private ordinal coordinates.

```pascal
LGainSlider := NewNyxSlider('audio-gain');
LGainSlider.Contract
  .Value(NyxNumberDomain.Range(-1, 1));
LGainSlider.Configure
  .AccessibleName('Audio gain')
  .SliderIntervals(400)
  .Height(48)
  .Done;
LGainSlider.WithNumber(0.125);

LDocument.State.SetValue(NyxNumberState('gain'), 0.125);
LGainSlider.Binds.Value(NyxNumberState('gain')).Done;
LPage.Add(LGainSlider);
```

Use `INyxSlider` and import `nyx.controls`, `nyx.contract`, `nyx.state` and the
ordinary document units. `NumberValue` reads numeric meaning; its setter and
`WithNumber` require a Number domain. The inherited Integer `Value` remains
available for Integer sliders and refuses fractional reads. Undeclared sliders
retain their existing Integer default. Spin/progress controls retain their own
Integer families; adding Number support to sliders does not weaken their admission.

## Ranges, choices and resolution

Declared numeric ranges and choices govern both hosts. Legacy integer
`Minimum`/`Maximum` supply bounds only for unbounded domains. Defaults are 0..100.
Exact numeric choices, including negative and fractional choices, become sorted
physical ticks while authored choice order remains unchanged:

```pascal
LPresetSlider.Contract
  .Value(NyxNumberDomain.Choices([2.0, -0.5, 0.0, 0.125]));
LPresetSlider.WithNumber(0.125);
```

Integer ranges up to one million intervals retain one tick per integer. Larger
signed Integer spans and Number ranges use `SliderIntervals`, default 1000,
accepted range 1..1,000,000. The entire signed 32-bit value range remains
available without overflowing a native control's private coordinates. A
single-value range/choice has one physical position. Endpoints are exact;
intermediate Number values follow stored Double arithmetic on both compilers.
Changing resolution remaps the thumb without changing accepted application state.

An application may set an admitted value between physical ticks. Unchanged host
callbacks preserve that exact value and wire spelling. Moving the thumb proposes
the corresponding numeric tick/choice. Refused edits restore the accepted value;
disabled/read-only input cannot publish a value. Callbacks receive the committed
numeric value, independently copied from the underlying control.

`TNyxSliderScale` and `TNyxSliderValue` in `nyx.sliders` are public portable
contracts for custom adapters. Their copied data has no widget, node, store or
renderer back-link. Invalid domain families, bounds, positions or values refuse;
failed accepted-value updates leave the old baseline exact. Ordinary built-in
adapters consume these same contracts.

## Input standards and evidence

The native Win32 journey exercises Home/End and Left/Right through the real
trackbar message path, including negative numeric choices. The browser keeps a
unit-step native HTML range and exposes semantic numeric `aria-valuemin`,
`aria-valuemax`, `aria-valuenow` and `aria-valuetext`. The standard references are
the [HTML range state](https://html.spec.whatwg.org/multipage/input.html#range-state-(type=range))
and [WAI slider pattern](https://www.w3.org/WAI/ARIA/apg/patterns/slider/).
Browser execution, touch/hardware input, assistive technology, representative
visual quality and other widgetsets/DPI remain explicit qualification gates.

`tools/build.ps1 -Target slider-fields` runs checked shared/actual native tests
and stages the matching browser consumer. Optional `-SliderSourceDirectory`
consumes `nyx.generated.slider.pas`. The maintained Pascal companion tool uses
one authenticated grouped MCP seed, bounded source queries and exact paired
Undo/Redo on an independent review. It then explicitly enriches that exported
seed locally with new typed numeric policies. This does not claim the frozen
running service admits those new policies. `--enrich` reuses a retained export
without repeating editor mutations.

See [current evidence](../WORK.md#current-return-path-typed-numeric-sliders--2026-10-07).
