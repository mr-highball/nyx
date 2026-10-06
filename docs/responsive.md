# Responsive authoring

[Designer views](designer-views.md) · [Building](building.md) ·
[Current evidence](../WORK.md#typed-responsive-authoring--2026-10-05)

`TNyxViewportWidth` describes available rendering space in logical pixels.
It contains no DOM, LCL, operating-system or device-name dependency. Specialized
managed controls expose the same `WhenViewport` configuration as the base descriptor.

```pascal
uses nyx.types, nyx.responsive, nyx.controls;

// The page owns this row after Add; its interface retains the descriptor.
LWorkspaceRow := NewNyxRow('workspace');
LHomePage.Add(LWorkspaceRow);
LWorkspaceRow.Configure
  .Layout(nlRow)
  .Gap(16)
  .WhenViewport(TNyxViewportWidth.Below(640))
  .Layout(nlColumn)
  .Gap(8)
  .ForPlatform(npfNativeLCL)
  .Gap(10)
  .Done;
```

`Below` excludes its upper bound; `AtLeast` includes its lower bound;
`Between` includes its lower bound and excludes its upper bound. Bounds are
nonnegative Integers, with a positive upper bound where specified. Empty or
reversed intervals raise `EArgumentException`. `Any` restores ordinary defaults
within the current platform scope. `ForPlatform` preserves the width condition,
and `WhenViewport` preserves the platform. Retaining either facade does not
change another facade's scope.

Presentation first uses ordinary defaults and fixed target overrides. Matching
common width properties then apply, followed by concrete-target width properties.
Later persisted property positions win within each group; updating an existing
property retains its position. Leaving a rule exposes the current live default.

Width means the rendering host's client width, shared by that view's descendants.
It does not mean physical screen width or each nested container's width. Browser
observation reacts on ordinary rendering frames; LCL uses its resize/layout path.
Both adapters retain existing controls. First-rule admission starts observation;
last-rule removal disconnects it. A moved view adopts its new host's width.
Unmount disconnects observers before disposing controls.

Realized trees own a presentation overlay independently of authored properties.
`Prop` reads effective presentation; `StoredProp` is the explicit codec/authored
comparison boundary. Resizing does not rewrite the design, source or history.
Scalar defaults, bindings, ownership, identities and callbacks cannot become
viewport-specific. Unsupported control properties and contradictory effective
size/split bounds refuse before publication. Admission checks every piecewise
interval on browser and native targets, including overlaps.

The Nyx-built Inspector exposes **Responsive layout** for controls with a layout
property. Set an inclusive minimum, exclusive maximum (zero means no upper limit)
and a layout. **Set layout rule** submits one intent through the existing isolated
paired processor and one Undo step. Existing responsive fields use ordinary typed
property editors and readable scope titles. This form creates layout rules;
other presentation attributes can be authored fluently or semantically.

Agents inspect bounded `nyx_node` metadata and group related changes in one
revision-aware `nyx_transaction`. At this explicit JSON boundary, canonical keys
have the form `@nyx.viewport:0:640:any:gap`; numeric/Boolean values retain their
JSON types and enums use advertised choices. Malformed, noncanonical and
unsupported reserved keys refuse. Generated Pascal uses typed `WhenViewport`
calls and control-purpose names. The maintained English operation fixture is
[responsive-review.operations.json](../tests/responsive-review.operations.json).
Export bounded source windows at one revision; use `nyx_build` for actual compiler
diagnostics. Visual captures qualify rendering selectively, while real target
controls establish input behavior.

Checked shared fixtures pass 33 on both native compilers and the executed browser
contract. The unchanged MCP companion passes 22 actual Win32 control checks, nine
ordinary native Studio inspector/Undo checks and 23 browser checks at desktop and
an exact 390-pixel iframe. Checks cover first/last-rule refresh, automatic resize,
same input identity, independent English text, selection/focus, exclusive bounds,
host replacement and unchanged authored persistence. Actual MCP jobs compile the
companion on both targets; grouped Undo/Redo restores its exact generated source.

Ordinary browser Studio inspector/worker execution, physical phone/hardware,
IME/assistive technology, other widgetsets, nested container conditions, named
variants and comprehensive responsive semantics remain unqualified. Projection
scans authored rules; these fixtures make no large-project performance claim.
Full acceptance remains with the existing
[authoring owner](../TODO/NS-4_studio-authoring_01.md).
