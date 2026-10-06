# Reusable presentation recipes

[Responsive configuration](responsive.md) · [Retained arrangements](retained-arrangements.md) ·
[Current evidence](../WORK.md#content-recipes--2026-10-06)

`INyxContent` chooses whole reusable recipes for an instance. Recipes can have
different descendants, control types and named parts. The document owns the
definitions; each realized instance owns its independently expanded selected
tree. Inactive recipes remain authored dependencies without creating controls.

```pascal
uses nyx.types, nyx.responsive, nyx.presentations, nyx.content, nyx.controls;

// Both definitions have already been admitted with Document.AddComponent.
LWideRecipe := NyxComponent('full-workspace');
LCompactRecipe := NyxComponent('quick-workspace');
LFocused := NyxPresentation('focused-work');

LWorkspace := NewNyxComponent('workspace');
LHomePage.Add(LWorkspace);
LWorkspace.Content
  .Use(LWideRecipe)
  .WhenViewport(TNyxViewportWidth.Below(640))
  .Use(LCompactRecipe)
  .WhenPresentation(LFocused)
  .Use(LCompactRecipe)
  .Done;
```

Recipe variables are `TNyxComponentRef`; `LFocused` is a distinct
`TNyxPresentationRef` defined in `Document.Presentations`. Its automatic/manual
activation and optional ancestor container belong to that shared definition.
`WhenViewport` also accepts a copied width/height/orientation condition.
`ForPlatform` preserves the current condition. Retaining another scope never
redirects an earlier scope.

Common defaults apply first, followed by matching automatic rules and then the
selected manual rule. Those phases repeat for the concrete target, with target
rules last. Within a phase, the last matching stored rule wins. `Use` replaces
an exact scope at its original position. At most 64 rules are admitted per
instance. References retain exact printable Unicode names of at most 128 scalars.

`Clear` removes only the current scope. `WhenViewport(Any)` returns to defaults
within the current target scope; `Done` returns ordinary common defaults. Each
instance needs an ordinary common recipe, through `Content.Use` or the compatible
`Configure.Component` reference. The specialized `Reference` property reads that
ordinary recipe rather than a mounted presentation. Setting it updates the
ordinary recipe/reference while retaining conditional rules.

All controls expose this typed contract so extensions with an instance projection
can use it too. Document admission rejects choices on other projections. Custom
`INyxControl` implementations supply `GetContent`; the unrelated managed decorator
fixture forwards through its retained inner control.

Scopes retain only their shared value registry. No registry/rule retains a node,
document, renderer or another scope. Retained scopes remain safe after a control
is released. `Clone` and the node's `SetContent` boundary copy independent storage.
Inspected rule/reference values cannot mutate registry storage.

## Composition and mounted views

`RealizeNyxView(Document, Root)` chooses ordinary content for dependency/source
inspection. A copied `TNyxViewFrame` supplies logical host dimensions, a concrete
target and optional exact manual selection:

```pascal
LFrame := TNyxViewFrame.At(390, 700, npfBrowser);
LView := RealizeNyxView(LDocument, LDocument.Pages[0], LFrame);
try
  // LView is an independent owned portable tree.
finally
  LView.Free;
end;
```

`LFrame.Selecting(LFocused)` retains geometry and validates that manual choice
against the document's immutable presentation snapshot. This frame chooses
recipes; scalar/platform projection remains the adapter's subsequent operation.

The measured overload accepts an independent `INyxContainerSnapshot`. Conditions
use the nearest eligible realized ancestor and exact qualified runtime ID. Self,
absent measurements and missing nearer boxes stay inactive. Instance publisher
overrides apply before nested selection. Append/prepend/replace payloads use their
inherited root/slot ancestry. Borrowed ancestry exists only during expansion; no
source tree is mutated or document/measurement backreference retained.

Both adapters choose automatic host recipes on **initial mount**. An explicit
`Render` remount can choose another control set against the same runtime state
store; typed bindings carry accepted values into its new controls. Invalid
inactive references/cycles refuse before replacing the mounted tree.

**Live structural switching remains open.** Resize and `Presentations.Select`
currently update scalar presentation only; they do not recompose recipe content.
Explicit remounts retire old controls, bindings and presentation leases; they do
not retain focus, caret or unfinished drafts. Stable publication, reentry guards,
logical part identity, input continuity and ordinary Studio recipe editing retain
their original responsive/parity authoring owners. Initial/remount evidence does
not accept physical phone/IME/assistive input or aesthetics.

## Persistence, source and semantic editing

Design version five stores ordered instance `contentRules` as structured values.
Older opaque fields retain extension meaning. Promotion refuses collisions rather
than assigning constructor behavior to legacy data. Every inactive/transitive
recipe is validated and retained by isolated view builds and root-removal review.

Generated Pascal declares specialized managed controls and readable `.Content`
blocks with typed references, conditions and enums. The managed reader replays
the same grammar, including `Clear`; paired synchronization preserves comments.
Source admission does not invoke an application compiler.

The source candidate extends existing MCP tools without another tool handle:

- `nyx_node` with `content: true` returns a separate rule page. `contentOffset`
  defaults to zero; `contentLimit` defaults to eight and permits at most sixteen.
  Property/event/part pagination remains independent.
- `nyx_transaction` accepts `content-set`, exact instance `id` and a complete
  version-one `content` registry. Related definition/title/recipe changes use one
  expected-revision guard and paired Undo step. Unknown fields, duplicate scopes,
  invalid geometry, missing references and recursive inactive branches refuse
  the complete group. `NyxSetContent` is the typed Pascal command; transport
  strings belong to its explicit structured boundary.

Current in-process semantic checks and server compilation qualify these fields.
Protected running services remain at their previous checkpoint; this packet
claims no new HTTP discovery or LAN deployment.

Run `tools/build.ps1 -Target content-recipes` for contract/native checks and
browser staging. On an isolated Pascal HTTP host, the maintained Pascal browser
driver observes `content-contracts.html` using `content-contracts`, and
`content-controls.html` using `projection`. Staged resources use relative URLs.
The build script starts no listener and changes no editor project.
