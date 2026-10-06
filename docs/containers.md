# Presentations within allocated space

[Responsive views](responsive.md) · [Component reference](components-reference.md) ·
[Studio agents](studio-agents.md)

A reusable card should respond to the space its parent gives it. Two instances
can therefore use different presentations in the same window. Nyx supports
this through a typed, named query container and the existing shared presentation
registry. Both browser and Lazarus adapters consume the same portable contract.

```pascal
uses nyx.types, nyx.containers, nyx.responsive, nyx.presentations,
  nyx.controls;

// The card publishes its content box. Its children can query that space.
// Width containment leaves the card's height free to follow its content.
LCard.Configure
  .QueryContainer(NyxContainer('card space'))
  .Containment(nccWidth)
  .Done;

LCompactCard := NyxPresentation('compact card');
LDocument.Presentations.Define(LCompactCard,
  TNyxPresentationCondition.Within(NyxContainer('card space'),
    TNyxViewportCondition.Any.WidthBelow(300)));

LCardBody.Configure
  .Layout(nlRow)
  .Gap(16)
  .WhenPresentation(LCompactCard)
  .Layout(nlColumn)
  .Gap(8)
  .Done;

LExtraDetails.Configure
  .Visible(True)
  .WhenPresentation(LCompactCard)
  .Visible(False)
  .Done;
```

`NyxContainer` returns `TNyxContainerRef`, a distinct application name rather than
a property string, Pascal identifier or target widget. Names are exact and
case-sensitive, with 1–128 Unicode scalars. Apostrophes and supplementary text
round-trip through persistence and generated Pascal. Empty, blank, control and
malformed names refuse. The default reference is absent; reading its name refuses.
Publishers can share a name across independent instances or nested containers.

`Within` takes the same copied `TNyxViewportCondition` used for whole-view rules.
Width and height minima are inclusive, maxima exclusive. Orientation compares
the positive logical content-box dimensions; it does not read a device sensor.
Each fluent condition method replaces its own axis. An automatic `Any` condition
alone refuses: ordinary configuration supplies the default presentation.

The publisher is static configuration on a supported layout container. It cannot
itself depend on a platform, viewport or presentation scope. Leaf controls and
split panes do not publish query containers in the current adapters. A rule on
a descendant finds the nearest *eligible ancestor* with the exact name; the
control never queries itself. `nccWidth` is eligible for width-only rules.
`nccSize` also permits height and orientation. An ineligible ancestor is skipped.
Once an eligible publisher is found, an unavailable measurement makes the rule
inactive; it never borrows the dimensions of an outer instance or another card.

Containment makes allocation stable. Descendant content does not supply the
publisher's intrinsic size on the contained axes. Give those axes space through
ordinary parent allocation, such as fill, flex or stretch, or explicit dimensions
and constraints. `nccWidth` contains width only; `nccSize` contains both axes.
Padding and borders remain part of the outer allocation. An otherwise
unallocated contained axis contributes its chrome rather than a child-derived
natural size. Clearing `atContainerContainment` restores the width default;
clearing `atQueryContainer` removes publication.

```pascal
// The parent stretches this frame's height and allocates its width.
LFrame.Configure
  .Flex(1)
  .WidthSizing(nsFill)
  .HeightSizing(nsFill)
  .QueryContainer(NyxContainer('frame space'))
  .Containment(nccSize)
  .Done;

LDocument.Presentations.Define(NyxPresentation('tall frame'),
  TNyxPresentationCondition.Within(NyxContainer('frame space'),
    TNyxViewportCondition.Any.Orientation(nvoPortrait)));
```

Browser measurements come from actual `ResizeObserver` content boxes. Lazarus
uses the completed logical layout allocation, less padding and nonclient chrome.
Detached, unallocated and hidden boxes are absent. Initial unmeasured browser
boxes stay inactive until observation; compatible updates retain controls and
their independent live values. Zero-sized measured boxes can match zero-inclusive
dimension rules, but never a positive orientation. The current logical axes are
physical width/height in the adapters' horizontal writing model.

Portable immutable `INyxContainerSnapshot` values copy finite, nonnegative
dimensions under exact qualified runtime IDs. They retain no document, tree or
widget. Negative/nonfinite measurements and duplicate identities refuse.
`ApplyViewport` overloads without a snapshot preserve whole-view behavior and
leave container rules inactive. Ordinary renderers provide their own snapshots;
applications do not need to calculate or publish widget geometry.

Container and whole-view rules share authored ordering. Projection applies
ordinary defaults and target overrides, then common automatic rules, the selected
common manual rules, concrete-target automatic rules and concrete-target manual
rules. Later authored property positions win within each group. Leaving a
condition exposes current defaults without rewriting the accepted document,
Pascal, application state or history. Manual definitions have no container predicate.

Use these scopes for supported scalar presentation properties: visibility,
layout, spacing, alignment, dimensions, constraints, positions and captions.
Alternate child trees, state/event changes and conditional ownership are outside
this contract. Container conditions currently use named `WhenPresentation`
scopes; there is no separate anonymous `WhenContainer` facade.

Studio's Properties inspector exposes **Query container** and **Containment**
for supported publishers. Its shared presentation form accepts an optional
**Query container** name alongside the existing bounds/orientation fields.
Leaving that field empty measures the whole view. Manual activation ignores
automatic fields. These ordinary Nyx controls submit one paired editor operation;
the adjacent source uses typed `Within`, `NyxContainer` and containment enums.

MCP `nyx_presentations` returns bounded definitions including their optional
container name. Group `presentation-define` with publisher/property edits in
one revision-aware `nyx_transaction`. Its `container` field is an explicit wire
boundary. Persisted container definitions use registry version three; earlier
whole-view/manual versions remain readable. Admission checks independent host
and eligible ancestor coordinate spaces, correlates predicates sharing one
publisher and rejects contradictory bounds atomically. A candidate exceeding
the 65,536-region validation budget refuses explicitly.

The maintained English composition is
[container-review.operations.json](../tests/container-review.operations.json).
Export its accepted Pascal with `nyx_container_mcp_review`, then run
`tools/build.ps1 -Target containers`. The target compiles unchanged MCP source,
shared contracts and actual controls. Browser consumers require an admitted HTTP
host; the build neither launches a listener nor replaces an editor project.
Current evidence and target/device limits live in [WORK.md](../WORK.md).

The browser containment mechanism follows the content-box and ancestor eligibility
model described in [CSS Conditional Rules Level 5](https://www.w3.org/TR/css-conditional-5/),
currently a Working Draft. Pascal owns selection on both targets; CSS is the
browser's containment mechanism rather than the portable query language.
