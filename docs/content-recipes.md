# Reusable presentation recipes

[Responsive configuration](responsive.md) · [Retained arrangements](retained-arrangements.md) ·
[Current evidence](../WORK.md#live-content-publication--2026-10-06)

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

Both adapters retain an independently owned authored blueprint containing the
selected view and every transitive recipe. The original document can be freed.
Host resizing, allocated container changes and `Presentations.Select` recompute
content through a coalesced UI queue job. The last request wins; selection reads
continue to expose the accepted choice until publication succeeds. Scalar-only
views retain their synchronous selection behavior.

A changed control set is constructed and admitted beside the mounted view before
that view retires. Its presentation capability stays connected. The same runtime
state store supplies bound values without importing document defaults. The same
collection context resolves local stores through copied recipe-instance provenance;
definition root names can differ. Compatible explicitly named collection parts
also retain selection. A geometry observation selecting the same recipe keeps
the actual control objects. Admitted retained source refresh replaces the blueprint
only after ordinary projection admission succeeds.
Custom `INyxCollectionBindings` implementations now supply `Recompose`, returning
an independently admitted view set against their retained runtime context.

Newly introduced and nested publishers settle before accepting a changed view.
Each hidden candidate consumes one frozen container snapshot for both structure
and scalar configuration. Its actual allocation produces the next snapshot.
Identity, child order, stored properties and every effective typed attribute must
agree with a fresh realization before admission. The comparison uses authored
baselines, excluding runtime bound values and physical drafts. One owned blueprint,
runtime store and collection context serve all passes; discarded candidates own
and release their controls, coordinators and staged validators.

At most eight candidates are constructed for one admission. A repeated
configuration refuses a cycle; an unsettled eighth candidate refuses the limit.
Both failures retain the accepted view. The queued route reports `LastContentError`;
explicit `Render` propagates the exception. This is a bounded admission guarantee,
not a claim that all arbitrary extension layouts converge.

The browser measures an inert hidden sibling using the host's classes, inline
layout, padding and exact client frame. Each candidate has its own theme scope;
discarding it removes that temporary host and never clears the caller's host.
Disconnected or `display:none` hosts preserve missing measurements. Native
candidates use hidden logical allocation. Ordinary scalar-only native container
views retain their existing scalar settling loop. An already settled observation
does not create a redundant queued job. General external stylesheet equivalence,
hardware input and other widgetsets remain separate qualification requirements.

Give corresponding inputs explicit `PartName(NyxPart('notes'))` values. Continuity
uses contiguous named part paths within the exact runtime instance. It never
guesses from captions, binding keys, sibling order or similar control kinds.
Unbound accepted values carry across compatible parts. Drafts carry only when
the value domain, every binding descriptor and accepted baseline remain exact.
A changed domain or descriptor creates a new field. Focus follows an eligible
matching part; caret ranges use Unicode scalar offsets and the target's actual
selection capabilities. Number inputs without a browser selection API retain
their drafts without an invented caret. Ambiguous saved identities refuse staging.

The queued job requires an idle store/command. A managed callback guard also
handles native callbacks that pump the UI queue: borrowed controls remain mounted
until the callback unwinds. Weak work/idle receivers retire before renderer
destruction. Composition and pointer capture defer replacement. Factory/admission
failure retains the accepted root, selection and capability, exposes
`LastContentError`, and remembers the exact failed observation to prevent an
automatic retry storm. Changed geometry or an explicit request permits retry.

Explicit public `Render` remounts still retire the old presentation lease. This
live gate does not accept rollback of a physical failure after publication,
arbitrary nested logical identity migration,
hardware IME/assistive input, other widgetsets, accessibility or release performance.
Hidden nested allocation, later-pass failure and controlled extension feedback
are qualified on actual browser and Win32 controls. Complete reversible physical
publication is still required. Ordinary Nyx-built Studio recipe editing and its
observing semantic
journey remain open under the original authoring/parity owners.

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
`content-controls.html` and `content-live-controls.html` using `projection`.
The live consumer qualifies resize/manual/container transitions, independent
stores, logical drafts/focus/selection, source refresh, failed factories, reentry,
coalescing and teardown. Optional `?preview` retains fresh wide/compact proof views
for a bounded capture after the teardown assertions; it is not the observing
editor or a screenshot of retained drafts. Staged resources use relative URLs.
The build script starts no listener and changes no editor project.
