# Responsive authoring

[Designer views](designer-views.md) · [Building](building.md) ·
[Current evidence](../WORK.md#manual-presentation-selection--2026-10-06)

`TNyxViewportWidth` and `TNyxViewportCondition` describe available rendering space in logical pixels.
It contains no DOM, LCL, operating-system or device-name dependency. Specialized
managed controls expose the same `WhenViewport` configuration as the base descriptor.

```pascal
uses nyx.types, nyx.responsive, nyx.layout.policy, nyx.controls;

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

Combine dimensions and orientation with a copied fluent value. Each method
replaces only its own axis; the other conditions remain independent:

```pascal
LWorkspaceRow.Configure
  .Layout(nlRow)
  .Gap(16)
  .WhenViewport(TNyxViewportCondition.Any
    .WidthAtLeast(640)
    .HeightBelow(300)
    .Orientation(nvoLandscape))
  .Layout(nlColumn)
  .Gap(6)
  .Done;

LDetailsPanel.Configure
  .Visible(True)
  .WhenViewport(TNyxViewportCondition.Any.HeightBelow(300))
  .Visible(False)
  .Done;
```

Width/height offer `Below`, `AtLeast` and `Between` methods with the same
inclusive lower / exclusive upper bounds. Orientation uses the closed
`nvoAny`, `nvoPortrait`, `nvoLandscape`, `nvoSquare` enum. Portrait means height
exceeds width; landscape means width exceeds height. A positive square matches
only Square, and zero-sized hosts match only Any orientation. These are host
rectangles, not device sensors. NaN, infinity and negative geometry refuse.
The existing width API, persisted keys and width-only generated source stay exact.

Name a shared presentation when several controls should respond to the same
condition. A distinct application reference keeps the condition in one place:

```pascal
uses nyx.types, nyx.responsive, nyx.presentations, nyx.controls;

LCompact := NyxPresentation('compact');
LDocument.Presentations.Define(LCompact,
  TNyxViewportCondition.Any.WidthBelow(640));

LWorkspaceRow.Configure
  .WhenPresentation(LCompact)
  .Layout(nlColumn)
  .Gap(8)
  .ForPlatform(npfNativeLCL)
  .Gap(10)
  .Done;

LDetailsPanel.Configure
  .WhenPresentation(LCompact)
  .Visible(False)
  .Done;
```

`LCompact` has type `TNyxPresentationRef`. The document owns its managed
`INyxPresentations` registry; a realized view retains an independent immutable
`INyxPresentationSnapshot`. Editing one definition reaches compatible retained
views without remounting their inputs. A snapshot can safely outlive its document.
Names are exact, case-sensitive application text: 1–128 Unicode scalars, including
supplementary characters. Blank/control/malformed names refuse, and at most 64
definitions are admitted. Replacing a definition retains its position. `Any`
alone refuses as an automatic named condition; ordinary defaults provide that
baseline. Unknown references and removal of still-referenced definitions refuse.

`WhenPresentation` replaces an anonymous condition; `WhenViewport` replaces a
named condition. Both preserve the selected platform, and `ForPlatform` preserves
the condition/name. All scopes retain the original property order, so an overlap
between named and anonymous conditions follows the same precedence described
below. Structural ownership, bindings and callbacks remain outside these scopes.

Version-four persistence stores definitions and an ordered `presentationRules`
array on each affected node. Names travel as values, with original property
positions, rather than long JSON object keys that older native fpjson containers
truncate. Versions 1–3 retain their format and opaque extension meaning; a
conflicting promotion refuses. Generated Pascal uses `Presentations.Define` and
`WhenPresentation`, never reserved property strings.

A manual presentation gives an application or designer an explicit, exclusive
choice alongside its automatic rules. Define its intent once, then configure
any number of controls through the same strongly typed scope:

```pascal
LFocused := NyxPresentation('focused');
LDocument.Presentations.Define(LFocused, TNyxPresentationCondition.Manual);

LWorkspaceRow.Configure
  .WhenPresentation(LFocused)
  .Layout(nlColumn)
  .Gap(4)
  .ForPlatform(npfNativeLCL)
  .Gap(5)
  .Done;

// Retain the public capability of this mounted view on either target.
LViewPresentations.Select(LFocused);
LViewPresentations.Automatic;
```

`LViewPresentations` is `INyxPresentationView`, exposed by each adapter's
`Presentations` property. Selecting a manual name does not change the document,
accepted Pascal, application state or Undo history. Different mounted views can
select different names. A choice replaced by an automatic definition or removed
in an admitted refresh clears to automatic/defaults. Unknown or automatic names
refuse before changing the view. A retained capability does not retain its view;
unmount retires its borrowed receivers, `Connected` becomes false, and further
selection reads or changes raise `ENyxPresentation`.

Definitions use `TNyxPresentationCondition` for complete automatic/manual/container
inspection. The earlier `Condition` viewport accessor refuses manual and container
definitions. Whole-view automatic-only registries preserve their exact nested
version-one wire shape. A registry with manual definitions and no container uses
nested version two, adding the
closed `activation` choice to each entry. Manual entries require all bounds zero
and orientation Any; contradictory hidden predicates refuse. The outer document
remains version four. Container definitions use nested version three, described
in [the container contract](containers.md). Existing automatic condition expressions
stay unchanged.

The Nyx-built Inspector exposes shared definitions for leaf and layout controls.
Use the condition fields to **Define or update presentation**, choose a shared
name/property/target to **Add override**, edit its ordinary typed field, and
**Reset override** to remove that exact scope. These operations use the ordinary
isolated processor and paired Undo. Updating a name affects every referencing
control. MCP `nyx_presentations` reads one exact definition or at most 16 entries
per page. Group `presentation-define`, `presentation-set`, `presentation-use`,
`presentation-reset` and `presentation-remove` inside `nyx_transaction` at an
expected revision. `presentation-set` admits the published property's exact
scalar family and carries the name as a value, including full-length Unicode.
Definition removal must be grouped with removal of its remaining overrides.

The shared-definition Inspector offers automatic/manual activation. Manual
activation ignores the automatic bounds fields. A **Presentation** preview
selector appears in the view bar when manual definitions exist; it changes
per-project editor presentation and retains the canvas inputs. `nyx_preview`
accepts an optional exact `presentation` name at the requested revision;
omission/null uses automatic/defaults. Its immutable preview does not change
the observing editor's choice. The maintained English semantic fixture is
[manual-presentation-review.operations.json](../tests/manual-presentation-review.operations.json).

Named automatic predicates normally use the rendering host rectangle.
[Container-aware presentations](containers.md) instead select the nearest
eligible named ancestor's measured content box through typed `Within` conditions
and static width/full-size containment. Independent reusable instances can adapt
at one unchanged host size. Alternate structural view trees remain open authoring
work. No physical device identity is inferred.

Presentation first uses ordinary defaults and fixed target overrides. Matching
common automatic properties apply, followed by the selected common manual
properties, concrete-target automatic properties, and concrete-target selected
manual properties.
Later persisted property positions win within each group; updating an existing
property retains its position. Leaving a rule exposes the current live default.

Width and height mean the rendering host's client dimensions, shared by that view's descendants.
It does not mean physical screen width or each nested container's width. Browser
observation reacts on ordinary rendering frames; LCL uses its resize/layout path.
Native conditions use the borrowed host rather than its internal scrolling panel.
Both adapters retain existing controls. First-rule admission starts observation;
last-rule removal disconnects it. A moved view adopts its new host's rectangle.
Unmount disconnects observers before disposing controls.

Realized trees own a presentation overlay independently of authored properties.
`Prop` reads effective presentation; `StoredProp` is the explicit codec/authored
comparison boundary. Resizing does not rewrite the design, source or history.
Scalar defaults, bindings, ownership, identities and callbacks cannot become
viewport-specific. Unsupported control properties and contradictory effective
size/split bounds refuse before publication. Admission partitions both dimensions
at relevant authored bounds and checks every feasible portrait/landscape/square
region on both targets. This includes conflicts occurring only on the interior
square diagonal. Text and gap rules do not inflate the constraint partition.
Each automatic region also checks defaults and every exclusive manual choice.
Candidates exceeding the bounded 65,536 partition budget refuse explicitly.

The Nyx-built Inspector exposes **Responsive layout** and shared presentations.
Set inclusive minima, exclusive maxima (zero means no upper limit),
orientation and, on layout controls, a layout. **Set layout rule** submits one intent through the existing isolated
paired processor and one Undo step. Existing responsive fields use ordinary typed
property editors and readable scope titles. This form creates layout rules;
other presentation attributes can be authored fluently or semantically.

The condition scopes a complete fluent presentation, including visibility,
logical positions, dimensions, size constraints, alignment and displayed captions.
For example, keep an optional details panel and its input alive while giving the
main content more room:

```pascal
LDetailsPanel.Configure
  .Width(320)
  .Visible(True)
  .WhenViewport(TNyxViewportWidth.Below(700))
  .Visible(False)
  .Done;

LContentRow.Configure
  .Layout(TNyxLayoutPolicy.Row.Wrap(nfwNoWrap))
  .Gap(24)
  .WhenViewport(TNyxViewportWidth.Below(700))
  .Layout(TNyxLayoutPolicy.Column.Align(ncaStretch))
  .Gap(12)
  .Done;
```

Inside an absolute layout, the same control can use different positions and
dimensions without a second implementation or compiler directives:

```pascal
LStatusBadge.Configure
  .Left(24).Top(24).Width(120).Height(32)
  .WhenViewport(TNyxViewportWidth.Below(640))
  .Left(12).Top(12).Width(96).Height(28)
  .ForPlatform(npfNativeLCL)
  .Top(16)
  .Done;
```

The browser and LCL adapters consume the same conditions. Native-specific `Top`
above applies only inside the selected width range. Tree ownership, application
state and callbacks remain shared. Hiding a control retains its existing input;
normal platform focus rules still apply when the focused control becomes hidden.
Studio uses these public conditions for compact source captions and Agents chrome.
Its compact panel switches and compatible synchronized history updates now retain
the canvas projection and independent input instead of recreating it.

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

Combined keys use `@nyx.viewport-size:0:0:0:300:landscape:any:gap` for width
minimum/maximum, height minimum/maximum, orientation, platform and attribute.
Width-only rules always keep the legacy namespace; alternate duplicate spellings
refuse. These keys belong to persistence/MCP boundaries. Handwritten and generated
default authoring uses strongly typed conditions and fluent configuration.

Checked shared fixtures pass 33 on both native compilers and the executed browser
contract. The unchanged MCP companion passes 22 actual Win32 control checks, nine
ordinary native Studio inspector/Undo checks and 23 browser checks at desktop and
an exact 390-pixel iframe. Checks cover first/last-rule refresh, automatic resize,
same input identity, independent English text, selection/focus, exclusive bounds,
host replacement and unchanged authored persistence. Actual MCP jobs compile the
companion on both targets; grouped Undo/Redo restores its exact generated source.

The ordinary browser Studio journey now connects to an explicitly MCP-authored
project, edits the actual responsive Inspector, waits for the module worker,
and uses ordinary synchronized Undo/Redo. It passes 22 desktop and 22 exact-390
checks, retaining the same canvas input, uncommitted English text, selection and
source editor. Compact source/Agents presentation uses the public width contract.
Existing source workspace/modal regression passes 30 on each browser size and
30 on Win32; responsive native Studio still passes nine. The resulting complete
application compiles through MCP on both targets, with exact build fingerprints.

The height/orientation extension passes 61 shared checks on each native compiler
and in the browser. Unchanged MCP source passes 30 actual Win32 controls and
31 browser controls at desktop and exact-390, including height-only observation,
exclusive height boundaries and retained input/focus/range. Ordinary combined
Inspector/paired Undo runs pass nine native and 22 per browser size. Actual LAN
MCP application jobs succeed on both targets and grouped Undo/Redo restores exact
100-line source. See [current evidence](../WORK.md#responsive-host-conditions--2026-10-06).

Physical phone/hardware, IME/assistive technology, other widgetsets,
nested container conditions and comprehensive responsive semantics remain
unqualified. Projection
scans authored rules; these fixtures make no large-project performance claim.
Keyboard/visual-viewport handling is not established by resizing a fixed test
host. A height rule follows that host when its layout resizes; it does not assume
the mobile keyboard has resized the browser layout viewport.
Full acceptance remains with the existing
[authoring owner](../TODO/NS-4_studio-authoring_01.md).

The manual-selection extension passes 72 shared checks per native compiler and
executed browser, two compiled Unicode checks per target, 52 Win32 / 53 per
browser-size retained-control checks, and 26 native / 64 per browser-size ordinary
Studio checks. Version 2/3 preference migration and exact version-four selection
round-trip pass in the native/browser workspace journey. The exact English MCP
companion compiles for both outputs; focused/wide semantic PNGs render different
layouts without editing the project. The LAN release preserves four existing
paired designs/navigation states. See
[manual selection evidence](../WORK.md#manual-presentation-selection--2026-10-06).
