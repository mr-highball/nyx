# Component capabilities

[Catalog matrix](components-reference.md) · [Events](events.md) ·
[Agent tools](studio-agents.md) · [Composition](components.md)

The public schema describes each property and event alongside its browser and
LCL support. Studio's property hints, visible unsupported-property explanations,
semantic MCP queries and catalog matrix consume this same metadata.
An editable portable field does not imply that every standard projection enacts
it. A leaf can retain layout metadata, for example, while a layout host supplies
the actual arrangement.

## Typed property support

`TNyxPropertyInfo.Support` is an immutable value snapshot. Its meaning is a
`TNyxPropertyMeaning`, and its target grades are `TNyxCapability` enums:

| Meaning | Use |
| --- | --- |
| `npmContract` | Identity, routing, composition or a declared semantic value |
| `npmPresentation` | Effect supplied by a projection or theme |
| `npmInteraction` | Input policy, focus/accessibility or interaction meaning |
| `npmCustom` | Effect supplied by a component/application implementation |

| Grade | Current contract |
| --- | --- |
| `ncAvailable` | The standard implementation supplies the described effect |
| `ncBasic` | A partial implementation; read its help for the boundary |
| `ncText` | A text fallback |
| `ncCustom` | A supplied implementation is required |
| `ncMissing` | The standard projection does not enact this effect |

`NyxProperties(Control, Document)` resolves ordinary, derived and reusable
properties. `NyxPropertySupport(Control, atReadOnly, Document)` queries a typed
attribute directly. Neither retains the document. Returned arrays and records
can be changed by the caller without changing the registry. Platform override
properties explain their scope and mark the opposite target unavailable.

Component creators can describe their own properties when publishing a schema:

```pascal
LProperty := Default(TNyxPropertyInfo);
LProperty.Key := 'tone';
LProperty.Title := 'Tone';
LProperty.ValueType := npText;
LProperty.Support := NyxPropertySupport(npmPresentation,
  ncAvailable, ncCustom,
  'Browser renders the tone; the native face requires a registered adapter.');
RegisterNyxSchema(NyxCustomKind('callout'), [LProperty], LEvents);
```

Descriptions are help text. They do not select behavior. A creator's declared
support must be backed by its adapters and tests; publishing metadata alone does
not implement a property or event bridge. Older schemas receive the standard
attribute support where applicable, and open custom keys remain custom.

## Input policy

Enabled and visible scopes govern descendant interaction. Read-only scopes
refuse user value edits, including compound commands targeting unbound parts.
Focus, keyboard notifications and deliberate application state updates remain
available. A child cannot turn off an ancestor's protection by setting its own
read-only flag false.

`NyxInteractionPolicy(Control)` captures these inherited decisions without
retaining controls. Both target adapters and the portable binding/command paths
use it. Refused physical drafts are restored without publishing a state change
or text-admission callbacks. Selectors without a standard read-only widget mode
have basic support: they restore refused drafts rather than presenting a native
read-only mode. Text entry uses the actual widget's read-only mode.

## Inspecting support

Studio's Properties tab places capability descriptions on the actual field's
hint. Properties unavailable on either target also have a visible explanation
for touch users. More properties retains all typed configuration fields, even
when a particular face needs composition or a custom adapter to enact them.

An agent can request a single property without fetching the document:

```json
{"id":"reply-memo","keys":["placeholder"],"limit":1}
```

The `nyx_node` property item includes `meaning`, `browser`, `native` and bounded
`help`, alongside the existing type/value/constraint fields. Events retain their
own target grades. Revision guards, paging, history and transaction admission
continue to use the same document model.

## Qualified boundaries

The generated matrix covers all default catalog kinds and typed properties and
their advertised runtime events. It describes implementation support, rather
than individual visual approval or hardware qualification of every combination.
The current LCL image resolves local picture files and exposes alternative text
as its accessible description; native network/portable asset resolution requires
a supplied adapter. A standard native link requires a handler for navigation.
Native multiline placeholders depend on the widgetset.
Numeric browser input hints do not replace an exact declared numeric domain.

Literal `Items` uses one row per line and table cells separated by tabs. Quotes
are literal text; leading/trailing empty cells are retained. Changed rows update
the existing face, while unchanged rows retain physical selection/scroll. Typed
collection attachments own their dataset independently of literal `Items`.
See the [property/projection reassessment](property-concordance.md) for the finite
attribute matrix and the remaining target gaps.

`tests/nyx.test.interactions.pas` checks support completeness, immutable creator
snapshots, scoped metadata and inherited input admission on native and executed
pas2js. `tests/nyx_interaction_controls_tests.lpr` checks actual DOM/LCL policy,
keyboard/text/pointer routes, phase payloads and lifetime guards. Studio's actual
browser/native authoring journeys check field hints, touch-visible explanations
and preserved source. Agent unit and real HTTP fixtures check bounded support
queries. These fixtures qualify those exercised contracts, not every event on
every widgetset. [Wheel/viewport contracts](events.md#wheel-requests-and-actual-scrolling)
and named producers have separate real-control evidence and explicit target
grades. Native gesture completion, drag/drop, rich selection, composition detail
and full native Studio retain their existing owners.

## Browser standards mapping

Keyboard actuation and text admission have separate contracts. Nyx's logical
`OnKeyPress` does not subscribe to the legacy DOM `keypress` event, which the
[W3C UI Events draft](https://www.w3.org/TR/uievents/#event-type-keypress)
deprecates. Exact text proposals also cover edits without physical keys.

Rejecting a Nyx value proposal restores the accepted value; it does not promise
to cancel an operating-system composition session. The
[Input Events Level 2 draft](https://www.w3.org/TR/input-events-2/#input-event-order-during-composition)
describes non-cancellable input during composition. Detailed composition and
selection contracts remain required follow-up work.

The [Pointer Events specification](https://www.w3.org/TR/pointerevents/)
also defines pointer cancellation/capture and wheel units. Wheel snapshots now
retain those units and physical cancelability. Pointer cancellation/capture
remain required scope; existing pointer snapshots do not imply those bridges
already exist. These references guide implementation and qualification, rather
than establish a blanket conformance claim.
