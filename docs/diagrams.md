# Visual diagrams

[Diagram editor owner](../TODO/NS-3_diagram-editor_01.md) ·
[Canvas](canvas.md) · [Typed behavior graphs](logic.md)

The diagram layer is independently authored Pascal. `nyx.diagram` owns typed,
immutable visual graphs; `nyx.diagram.drawing` projects them through ordinary
Nyx Drawing controls. The reusable runtime editor below composes ordinary Nyx
chrome with the existing canvas attachment. The full composed editor's
canvas/program/flow prerequisites remain open. Ordinary Studio and bounded
semantic operations consume the same public model/editor. This adds no
registered control kind, separate semantic tool or draw.io file reader.

```pascal
LWorkflow := NewNyxDiagram
  .WithVertex(NyxDiagramVertex(NyxDiagramVertexRef('review'),
    ndsDiamond, NyxCanvasBounds(180, 40, 160, 120)).Captioned('Ready?'))
  .WithVertex(NyxDiagramVertex(NyxDiagramVertexRef('publish'),
    ndsEllipse, NyxCanvasBounds(440, 60, 140, 80)).Captioned('Publish'))
  .WithEdge(NyxDiagramEdge(NyxDiagramEdgeRef('approved'),
    NyxDiagramAttached(NyxDiagramVertexRef('review'), ndaRight),
    NyxDiagramAttached(NyxDiagramVertexRef('publish'), ndaLeft))
    .Routed(ndrOrthogonal).Arrows(ndaNone, ndaOpen));

LPreview := NewNyxDrawing('workflow-preview')
  .WithText('Publishing workflow')
  .WithPlan(DrawNyxDiagram(LWorkflow));
```

The routine imports `nyx.diagram`, `nyx.diagram.drawing`, `nyx.controls` and
`nyx.canvas.geometry`. Shape and edge references are distinct Pascal types.
Captions are application text; route/attachment/arrow choices are closed enums.
Drawing an arrow neither defines nor executes a behavior wire.

Each upsert returns an independent plan, retaining the original insertion order
for an existing exact name. Previously returned records, routes and plans stay
unchanged. Apply `AtBounds` to a vertex and upsert it to move/resize; route
resolution uses that plan's current geometry. Workers may read published plans
or create private copies after the normal safe handoff. They must not mutate a
live Drawing or call a renderer. Applications still own scheduling/publication.

Straight automatic ends intersect the mathematical rectangle, ellipse or diamond
boundary toward the opposite center. Orthogonal automatic ends choose a cardinal
midpoint and use 24-world-unit outward stubs. Explicit attachments retain their
side midpoint. Free ends retain their signed coordinates. Parallel edges remain
distinct and ordered. Orthogonal self-loops run around their own shape; automatic
ends select right/top. Explicit loops require distinct attachments. Routing does
not avoid other obstacles or promise an optimal path. Curves, arbitrary waypoints,
rotation and group transforms remain required. Named physical ports are described below.

`PickVertex` uses exact shape geometry, returning the last matching shape in paint
order. `PickEdge` tests centerline segments with caller tolerance in world units,
independent of stroke width, decorations and shape occlusion. It returns the last
matching edge. The caller chooses shape-versus-edge priority; absent results are
typed absent references. These pure queries do not implement editor selection,
keyboard focus, marquee or accessible alternatives.

Removing a connected vertex refuses by default. Explicit
`ndrmIncidentEdges` removes exactly its incident edges in the returned plan;
unrelated order/ownership remains. Missing references, absent endpoints, unknown
wire choices and dangling edges refuse. Styles use existing typed drawing paints,
including caller colors and semantic theme roles.

## Bounds and persistence

There are at most 256 shapes, 512 edges and 64 named physical ports per shape. Shape extents are 8..1,000,000 world
units inside +/-1e9. Captions admit 256 Unicode scalars and refuse controls/newlines.
These are admission limits, not production performance measurements. The strict
`ToData` / `NyxDiagramFromData` boundary admits at most 1 MiB UTF-8,
exact closed fields, native numeric values and integer enum spellings. Duplicate
members and unknown/dangling identities refuse. A large in-memory authored plan
can reach the persistence byte refusal before membership limits; applications must
admit its complete descriptor/projection before publishing it into saved work.
There is no hidden mutable JSON tree, document, renderer or reference cycle.

## Named physical ports

An application can give a custom or reusable shape stable connection points:

```pascal
LForm := NyxDiagramVertex(NyxDiagramVertexRef('Details'), ndsRectangle,
  NyxCanvasBounds(40, 20, 320, 260))
  .WithContent(NyxDiagramComponent(NyxComponent('Details form')))
  .WithPort(NyxDiagramPort(NyxDiagramPortRef('Reply'), 1, 0.5)
    .Directed(ndaRight).Captioned('Reply output'));

LReplyEnd := NyxDiagramPortEnd(LForm.Reference, NyxDiagramPortRef('Reply'));
```

`TNyxDiagramPortRef` is case-sensitive and local to one vertex. It retains text,
never a control or executable route. `TNyxDiagramPort` copies width/height ratios
from the shape's top-left: zero is left/top and one is right/bottom. Ratios may
lie outside the shape; ratios and resolved world points must remain finite and
inside the diagram coordinate budget. Moving/resizing recomputes the exact
anchor. `Directed` selects a closed cardinal exit or automatic outward direction;
orthogonal stubs leave the whole shape before turning. Self-loops need distinct
resolved points. These ports carry visual connectivity, independently of Tool
Canvas event/data ports and their execution authority.

`WithPort` upserts while preserving order and independent previous values.
`WithoutPort` removes a local value; complete graph admission refuses dangling
endpoints. `RemoveNyxDiagramPort` instead accepts an explicit removal policy:
the default refuses connected ports; `ndrmIncidentEdges` removes only lines on
that exact vertex/port pair. Peer ports and unrelated lines stay unchanged.
Missing or duplicated wire identities refuse instead of falling back to a side.

The optional `INyxDiagramPortsView` exposes revision-guarded typed authoring on
both adapters. The public **Ports** panel edits stable name, ratios, exit and
description. Apply/Revert deliberately handles all properties together; hiding
the panel retains unfinished text. A changed name creates an additional port.
Start from port / Connect to port support ordered selection and existing-edge
reconnection; Cancel connection clears transient intent without a graph edit.
Underlay pointer hooks also select visible named endpoints. Only
the selected shape displays bounded diamond glyphs, sized from logical pixels
and zoom. Other shapes' hidden ports are never pointer targets. Interior glyphs
can be covered by live embedded controls; the picker/buttons supply an explicit
alternative. Visible port hits precede resize grips at coincident points; hide
Ports to use that grip. Layered hit testing, full keyboard/accessibility and visual quality
remain part of the broader editor acceptance.

Port graphs use descriptor version three. Version one retains no-content bytes;
two adds content, and neither accepts named port/endpoints. Document persistence,
crafted typed Pascal, literal Source Apply and paired history keep the complete
intent. Existing `nyx_outline` exact-vertex windows expose up to 64 local ports;
use `member` and a one-row limit to avoid unrelated context. Existing grouped
`diagram-vertex-set`/`diagram-edge-set` operations use the same typed wire records
and expected revision/definition. The source schema adds version three, with no
new tool or installed-schema claim. Runtime selection/camera remain separate.

Ordinary Studio consumes this public panel and deliberate paired Save/Undo/Redo.
WORK.md records actual native input and exact complete constructor evidence
separately from compiled-only browser consumers. The reference remains the
[official draw.io connection-point manual](https://www.drawio.com/docs/manual/shapes/shape-connection-points-customise/);
this independent contract adds no draw.io format compatibility. Full browser
input, endpoint dragging, curves/waypoints, transformed/grouped endpoints,
production budgets and preserving delivery remain required.

`DrawNyxDiagram` captures and validates the whole descriptor once. It paints edges
and independent open arrowheads, then shapes and one-line themed captions, using
the existing drawing budgets. The 64-world-unit frame includes stroke/decorations.
Long captions are not automatically wrapped or constrained to their shape.
Drawing's contain mapping scales the entire picture, including text. A narrow
fit is a thumbnail, not a qualified readable editor viewport. Interactive zoom,
selection and compact properties now use the public runtime editor below;
direct move/resize handles and full editor integration remain open.

Persist the visual graph separately when editable intent is needed. Plain Drawing
clone, codec and generated Pascal retain its display list only. They do not
recover the graph from pixels/paths. Updating the authored Drawing after an
ordinary Render does not rewrite that renderer's independent runtime projection.
The maintained consumer uses the public `RetainNyxControl` /
`SetNyxDrawingPlan` bridge on the runtime projection, then Sync. It preserves the
authored document. This is not Studio source publication or paired Undo/Redo.

## Saved editable intent

`nyx.diagram.declarations` adds distinct `TNyxDiagramRef` names and the document's
owned `Diagrams` membership. Store the graph itself, then explicitly reopen an
independent editor descriptor:

```pascal
LDocument.Diagrams.Define(NyxDiagramRef('Publishing workflow'), LWorkflow);
LDesigner := NewNyxDiagramEditor('workflow', LDocument,
  NyxDiagramRef('Publishing workflow'));

// A host deliberately saves an applied runtime snapshot through its document owner.
LDocument.Diagrams.Define(NyxDiagramRef('Publishing workflow'), LView.Diagram);
```

`Define` normalizes a caller implementation once, before adopting independent
immutable values. Upserts preserve position; new names append. `Remove` requires
an existing name. Clone and focused-view contexts own independent membership,
and a returned graph/editor descriptor may outlive the borrowed document. Author
membership on one owning thread; published immutable graphs use ordinary safe
handoff. Reentrant membership changes during foreign capture refuse. The family
admits at most 64 definitions, 4,096 vertices, 8,192 edges and 4 MiB UTF-8, in
addition to each graph's limits. These are admission budgets, not speed claims.

Document version sixteen carries ordered exact name/diagram entries. An empty
family retains the previous canonical version and bytes. Older opaque fields
remain opaque until explicit typed adoption; collisions with promoted fields
refuse. Plain Drawing and Panel cloning still retain only their ordinary control
meaning: they cannot recover an editable graph from its display list or chrome.

Generated Pascal uses `Result.Diagrams.Define`, specialized vertex/edge records,
typed shapes/endpoints, numeric geometry and fluent drawing styles. Literal
source reconstruction and ordinary paired Source Apply/Undo/Redo use existing
owners. Handwritten compiler-backed continuation retains unchanged helpers and
adds typed chronological Define/Remove only for deliberate changes. Registry
reordering without an expressible change refuses complete candidate equality.
Preparing a continuation never proves its execution; its exact compiled builder
must reproduce the independently proposed whole document before publication.

This is an explicit saved-intent/runtime factory contract. Ordinary Studio Save
and bounded semantic graph operations use it as described below. Registering an
application designer kind and authenticated observing publication remain required.

## Reusable runtime editor

`nyx.diagram.editor` exposes specialized managed `INyxDiagramEditor` /
`TNyxDiagramEditor` and a portable mounted `INyxDiagramEditorView`:

```pascal
LDesigner := NewNyxDiagramEditor('workflow', LWorkflow)
  .Captioned('Workflow designer')
  .WithOptions(NyxDiagramEditorOptions.Nudge(10).ToolsExpanded(False));

LView := MountNyxLCLDiagramEditor(LDesigner, LDocument, LHost);
LView.SelectVertex(NyxDiagramVertexRef('review'));
LView.SetGeometry(LView.Selection, NyxCanvasBounds(240, 80, 180, 120),
  LView.Revision);
LIntent := LView.Diagram.ToData;
```

The native routine also imports `nyx.diagram.editor.lcl`; browser callers use
`nyx.diagram.editor.browser` and `MountNyxBrowserDiagramEditor` with a borrowed
HTML host. Context/theme are copied. A supplied application store is borrowed;
otherwise each mount owns an independent default store. Disconnect before
destroying either borrowed dependency. Each mount copies the initial graph,
ID and options; later descriptor edits never change an already mounted editor.
Editor IDs reserve descendant space with a portable 64 UTF-8-byte limit.

The ordinary Nyx toolbar offers Previous/Next selection, zoom, Fit, 100% and
Properties. The initial view centers the first shape at the configured zoom;
it does not force a whole-diagram thumbnail. Click a shape to select it. Previous/
Next select and reveal in declared order. Activate the Selected button to center
that shape; while it has keyboard focus, unmodified arrow keys nudge it.
Recognized keys consume their platform default before admission, including when
unfinished fields refuse the move. Text inputs retain ordinary keyboard editing.
Geometry buttons provide the same actions through normal keyboard activation.
These are single-shape alternatives; marquee and full accessibility remain open.

Properties start collapsed by default. Show/hide, pan and zoom preserve the same
inputs, exact unfinished text and caret. X/Y/Width/Height accept finite numbers;
Caption retains the diagram's single-line Unicode contract. Apply captures all
five values into one diagram replacement. Revert deliberately restores them.
Selection/replacement/nudges refuse unfinished fields unless the relevant API
explicitly permits discard. A caller implementation's ToData runs once before
normalization; connection, revision and exact property-buffer fences protect
revocation or newly entered fields during that call, even with deliberate discard.

Graph revisions count admitted replacements, independently of selection/camera.
Expected revisions, complete codec/projection and native allocation checks precede
the existing coordinated canvas publication. Diagram intent, projection provider
and scene bounds install before notification. A physical/observer failure after
that point reports committed data; it does not invent rollback. UI callback
failures appear in the ordinary status label and LastError; direct API failures
raise to their caller. Owned UI callbacks refuse worker entry before reading
their borrowed view; their retained execution carries that failure diagnostic.
User input respects disabled/read-only state, while deliberate
programmatic model updates retain Nyx's ordinary caller authority.

The editor uses reflowed vector Drawing content in the existing canvas. At the
default 100% zoom its text retains 14 logical pixels even at a narrow allocation;
Fit intentionally scales the whole picture. Caller zoom limits and native
allocation budgets remain effective. The default is an ungridded canvas.
WithGrid supplies the existing background grid; native opaque child Drawing
hosts can occlude it inside the picture. A continuous grid beneath all diagram
geometry still needs integration/qualification. Gesture pan currently follows
the existing canvas's background-input contract, not drag anywhere on a shape.

Export the exact immutable Diagram snapshot to preserve editable intent.
The runtime component has no hidden Undo stack or implicit source authority.
An explicit owning save host can publish through the ordinary paired source queue,
as described below. Plain Node clone/generation copies its Panel chrome only;
registered application kinds, richer direct manipulation and full diagram
content/quality remain open.

## Direct manipulation and reconnection

Drag a shape to move it, or one of its eight boundary grips to resize it. Grips
retain eight logical pixels across zoom. Selection reserves a stable drawing
allocation, so changing the highlighted shape or line cannot stretch its content.
The portable `TNyxDiagramManipulation` record captures an independent vertex and
world origin. Each `At` derives from that original snapshot, preserving signed
fractional coordinates without accumulating pointer rounding. Resize pins the
opposite sides and clamps extents to the diagram's existing 8..1,000,000 domain.
Optional grid snapping follows `WithGrid`; Alt bypasses it for the current sample.

Mounted editors capture one pointer and pin its camera, event generation, graph
revision and exact property buffer. Move samples coalesce through the existing
UI scheduler into one pending preview; this is event-turn coalescing, not a frame
rate or large-scene performance guarantee. `DisplayDiagram` exposes immutable
preview intent while `Diagram` and `Revision` retain accepted data. The same
prepared canvas publisher installs its rerouted picture and scene bounds.
Release accepts the final sample once, including when its queued preview has not
run. Save and other model commands refuse while that capture owns a preview.

Escape, lost capture, focus exit, a second pointer, a changed camera/revision or
new property text cancel the preview without accepting geometry. Closing the
floating window cancels the gesture while retaining its model and fields.
Disconnect revokes queued work and callbacks before releasing capture/controls.
`CancelManipulation` supplies the explicit application alternative. Actual host
pixel resolution limits the precision of input; the portable geometry remains
Double. Browser/pen/touch/hardware execution and full accessibility stay separate
from native host-dispatch evidence.
The native floating modal lets focused controls consume Escape before its
remaining-key dismissal fallback; form KeyPreview no longer preempts that input.
Generic nested native edit/form delivery is qualified separately. The retained
ordinary Studio probe exposed blanket canvas preview also suppressing the
editor's own picture input. Preview now supplies a managed exact-root
[host input grant](canvas.md#live-runtime-workspace) for that private picture;
injected application controls and their callback/graph behavior stay paused.
Actual native desktop/compact journeys pass 47 checks per width, including first
Escape cancelling a captured gesture while retaining the open modal and focused
selection, followed by Save/history and retirement. Matching browser consumers
compile only; browser execution, hardware/assistive input and full keyboard/focus
parity remain with the
[parity owner](../TODO/NS-2_parity-accessibility_01.md).

Select an existing line and choose Reconnect start or Reconnect end, then choose
the destination shape by pointer or the ordinary selection/Connect buttons.
`BeginReconnect` shares pending-connection cancellation and revision guards.
`ReconnectEdge` also accepts typed free endpoints. Both retain the edge identity,
opposite endpoint, routing, arrows and style; complete graph admission rejects
dangling targets. These operations add no hidden source or local Undo authority.
Explicit Studio Save below publishes the entire applied result as one paired
design/Pascal history step. Independent local command history, draggable endpoint
grips, waypoint editing and the broader behavior matrix remain open. Named physical
ports use the separate optional workspace above.

## Studio authoring and deliberate Save

The runtime editor's optional Draw panel now creates rectangles, ellipses and
decisions at the current camera center. Default names follow their shape type
(`Rectangle1`, `Ellipse1`, `Decision1`) and avoid existing identities. The new shape
becomes selected in the same prepared publication as its graph/projection. Shape
properties remain the existing exact geometry/caption form. Draw and Properties
alternate; hiding either preserves unfinished input and the same mounted controls.

Choose a source shape and Start orthogonal or Start straight, then click a target
shape. The same operation works through ordinary selection buttons and Connect
to selected. Source intent retains only a typed reference/routing policy; camera
and selection changes retain it, while any applied graph change cancels its
generation. Cancel changes no graph. Completion checks the current local revision
and publishes one independently named connection, including parallel edges and
the existing routed self-loop behavior. Incomplete fields or failed/stale
publication retain the graph and pending source.

Click a line outside shape geometry to select it. Hit tolerance follows six
logical pixels across camera zoom. Previous/Next line provide an alternative to
pointer precision; the selection label reveals the chosen line. Highlighting
changes projected stroke/arrow theme roles, never authored styles or history.
The selected line's Properties button opens Draw routing controls. Straight,
Orthogonal and Remove line operate on that exact reference. Shape removal refuses
connected shapes until the caller deliberately checks Also remove its connected
lines; the resulting graph/selection publishes once, with no dangling endpoints.
Empty shape fields accept no UI input when a connection is selected.

Applications use the same public `AddVertex`, `SetEdge`, `RemoveVertex`,
`RemoveEdge`, `SelectEdge`, `BeginConnection`, `CompleteConnection`,
`CancelConnection` and `ShowDrawTools` contracts. Full vertices/edges retain their
existing typed styles, endpoints, arrows and routing. `AddVertex` refuses an
existing name; `SetEdge` deliberately creates or replaces one. Selection, pending
connection intent and panel visibility grant no source/history authority. Save
below adopts the complete applied graph through its explicit owner.

```pascal
LView.AddVertex(NyxDiagramVertex(NyxDiagramVertexRef('Approval'), ndsDiamond,
  NyxCanvasBounds(280, 40, 160, 100)).Captioned('Approved?'), LView.Revision);
LView.BeginConnection(NyxDiagramVertexRef('start'), ndrOrthogonal);
LView.CompleteConnection(NyxDiagramVertexRef('Approval'), LView.Revision);
```

Project > Diagrams lists saved definitions and offers New diagram and Resume
editor. Each floating workspace consumes the public Nyx diagram editor and modal
adapters. The visible Close button also works on touch screens. Dismissal hides
the same mounted controls, retaining the applied model,
unfinished property fields and selection. Retained windows are scoped to their
original project load; their save host cannot silently retarget another project.
Up to sixteen windows are retained. Only a hidden clean window can be evicted;
unsaved or pending work refuses eviction. These proposals are in-memory and do
not yet have cold-recovery persistence or a complete retained-window picker.

`INyxDiagramSaveHost` is optional, explicit UI-owned authority. Bind a copied
`NyxDiagramSaveTarget` through `SavingTo`, then call `Save` or use Save diagram.
The request captures the exact applied graph and its original accepted definition.
Unfinished property fields must first be applied or reverted. Studio submits the
typed change through its existing source queue, compiler strategy and complete
candidate guards. Successful paired publication acknowledges the exact graph;
submission or source preparation alone does not. One ordinary Undo/Redo restores
both design and Pascal. Missing application outputs do not prevent opening or
designing in the built Studio.

Newer local model edits and text typed while Save is pending survive its reply.
Cancellation/refusal retain the proposal. A stale accepted definition refuses
overwrite; Reload saved inspects the current named definition through optional
`INyxDiagramSourceInspection`, which captures its graph and complete accepted
source together. Older graph-only hosts can still use `INyxDiagramSaveInspection`.
Local changes require an explicit discard checkbox.
Retirement revokes the borrowed reply before controls are freed; late replies
cannot revive it, and native worker replies refuse before accessing the UI owner.

Existing `nyx_outline` scopes `diagrams`, `diagram-vertices` and `diagram-edges`
return saved intent only. `name` selects an exact diagram, `member` selects an
exact vertex/edge, and `query` is an exact-case name/caption substring. Pages
default to eight entries, at most thirty-two, with bounded `offset` and optional
`expectedRevision`. No runtime camera, selection or mounted-control state is
implied. `nyx_transaction` accepts the seven typed operations represented by
`NyxDefineDiagram`, `NyxRemoveDiagram`, `NyxSetDiagramVertex`, `NyxSetDiagramBounds`,
`NyxRemoveDiagramVertex`, `NyxSetDiagramEdge` and `NyxRemoveDiagramEdge`.
Related changes are chronological and atomic under one expected project revision
and one paired Undo step. Optional exact/absent graph expectations additionally
protect retained editors. Removing connected vertices requires deliberate incident
edge removal. Unknown fields, dangling graphs and stale expectations refuse the
whole candidate. These are extensions of existing tools; inventory stays twenty-
four in source and twenty-three in the frozen release. No installed schema or
authenticated HTTP/browser execution is claimed by the local source consumers.

## Resource images and live Nyx content

Vertices retain optional content independently of their caption and connectors:

```pascal
LGraph := NewNyxDiagram.WithVertex(
  NyxDiagramVertex(NyxDiagramVertexRef('Details'), ndsRectangle,
    NyxCanvasBounds(40, -80, 320, 260)).Captioned('Details')
    .WithContent(NyxDiagramComponent(NyxComponent('Details form'))
      .Padded(12).CaptionSpace(28)));
```

`TNyxDiagramContent` is an immutable value with closed `ndckNone`, `ndckImage`
and `ndckComponent` choices. `NyxDiagramImage` accepts a `TNyxResourceImageRef`,
including its optional explicit locale/fallback pin; `Fitted` accepts a
`TNyxImageFit`. Hosted and embedded resources use the same selector.
`NyxDiagramComponent` accepts a `TNyxComponentRef`. Neither retains a control,
document, renderer or platform handle. `WithoutContent`/`NyxNoDiagramContent`
clears content while preserving the shape and connections.

The editor materializes ordinary Nyx image/component descriptors in the existing
canvas. Each reusable resolves an independent tree and ordinary state bindings
and scoped callbacks. Optional `INyxDiagramContentView` exposes typed content
item/control references and revision-guarded `SetContent`. Geometry, inset and
caption-space changes retain the surviving editor, text, caret and callbacks.
Changing an image selector/fit/alt caption or reusable template replaces its
content lifetime. Clearing/recreating content allocates fresh controlled IDs.

Optional `INyxCanvasSceneContent.PrepareContent` stages membership and target
renderers before graph/scene/view publication. Unchanged roots share explicit
content leases without cloning or reparenting. The plan owns new roots until
publication/retirement. `INyxCanvasContentPlan.PreviewControl` is staging-only
and revoked at retirement; its detached spatial preview remains readable.
Failed dependency or target-provider admission keeps accepted controls and graph.

Content uses a rectangular inner allocation. `Padded` and `CaptionSpace` accept
finite world units in 0..1,000,000. Header space is reserved only when a caption
exists; exhausted shapes have a zero-area content box. This implies no ellipse/
diamond masking or shape resizing. The vector `DrawNyxDiagram` result contains
shape/connector underlay and captions, not the separately mounted live content.

The public Content panel supplies source-context resource/reusable pickers,
resource creator help, inset/header/fit and image locale/fallback settings.
Its bounded scroll area preserves canvas space. Properties, Draw and Content
alternate without discarding fields. One Apply adopts geometry and content;
one Revert restores both. Selection, typed mutations and Save retain unfinished-
input guards. Ordinary Studio consumes the same panel and deliberate paired Save.

No-content graphs retain version-one bytes. Version two admits the optional
closed `content` vertex member; strict version-one admission refuses it. Document
persistence, crafted Pascal and paired history retain selectors. Existing bounded
`nyx_outline` vertex pages include that member, and grouped `diagram-vertex-set`/
`diagram-define` operations admit it under whole-document revision/dependency
guards. No tool is added; current source and frozen installed schemas differ.

The source context is copied when the editor mounts. Ordinary Studio's Reload
saved now refreshes that context deliberately; Save never silently remounts live
content. See [live source refresh](#live-source-refresh). Embedded and floating mounts accept an existing
`INyxApplicationResources` as their final argument. For example:

```pascal
LView := MountNyxLCLDiagramEditor(LEditor, LDocument, LHost,
  nil, nil, ndcmRuntime, nil, nil, LResources);
LResources.Start;
```

The caller owns Start/Stop and disconnects the view before retiring its host.
The existing canvas resource participant uses that broker's current catalog for
new content and prepares updates for retained items. A valid hosted image without
fallback starts empty; ordinary loading/cache/cancellation diagnostics remain
authoritative. Loaded bytes do not rewrite the diagram or its source resources.
No broker is constructed or started implicitly. Strict synchronous selector reads
still refuse unavailable bytes. An authored fallback does not prove network
loading. Native qualification uses a deterministic byte transport with real
resolver/decoder/publication; browser compilation does not establish execution.
Broader source/state/resource-owner changes, masking, programs, large-scene budgets and
full adapter quality retain their existing owners.

## Live source refresh

An open editor can follow changed or newly added reusable definitions without
being destroyed. The optional `INyxDiagramSourceView.ReplaceSource` takes a
borrowed admitted document, graph and exact local editor revision. It copies and
validates that source before staging actual content. `INyxDiagramSourceInspection`
on the save host supplies an inert `TNyxDiagramSourceSnapshot`: the named graph
and complete UTF-8 design belong to one accepted document. `OpenDocument` returns
an independent caller-owned tree. Captures are bounded to 16 MiB of UTF-8 design;
the optional host revision is a fence, never compiler execution provenance.

Ordinary Studio supplies that source inspection through the same revocable
project/load port as Save. Reload saved checks the exact accepted design and
retains its original owner while preparing the replacement. Source, graph,
selection, canvas membership and camera join the existing prepared publication.
A stale host, reentrant field edit, missing dependency, unavailable target
factory or cancellation refuses that whole proposal. Unfinished local graph or
property work still requires deliberate discard. Reload adds no project/history
command; ordinary Studio Undo/Redo changes the accepted pair, which the same
open editor can then reload.

Unchanged selectors retain their actual controls only when their source meaning
also matches. Reusable dependencies include nested components, conditional
content, menus and saved canvas content; global binding declarations participate
conservatively. Unrelated roots, project title and visual diagram geometry do not
force replacement. A changed reusable is staged as replacement content; other
controls retain their text, caret, runtime state and callback registrations.
New definitions also become available in the Content picker and future mounts.
Source comparison captures immutable data, without creating a differently
partitioned focused document or acquiring application execution authority.
The separately repaired focused-document compiler path now retains complete named
diagram/canvas dependencies and selects the requested node through typed
[application focus](application-navigation.md#focused-application-views).

Runtime stores remain independent. This operation performs no state migration,
resource-broker replacement, automatic source watching or executable callback
hot reload. With an attached application resource port, candidate declarations
must already match that port's declarations; a mismatch refuses. New bindings
must be valid against the existing runtime owner. Broader source meaning,
resource-owner replacement and full target parity remain separate requirements.

`tools/test-diagram-source.ps1 -EvidenceDirectory <fresh-output>` runs maintained
portable/source checks, native controls and ordinary Studio reload/Undo/Redo at
wide and compact diagram-window sizes. It verifies exact complete generated
constructors natively and compiles the corresponding browser consumers/products.
Its explicit input-only native mode grants no foreground-capture evidence;
browser compilation grants no browser execution evidence. It starts no host and
does not replace installed projects or refresh authority. WORK.md owns accepted
receipts, retained failures and the remaining qualification gates.

## Authored behavior and design preview

Embedded and floating diagram mounts accept `TNyxDiagramContentMode`, an optional
`INyxApplicationContext` and an optional `INyxLogicResolver`. `ndcmRuntime` is the
source-compatible default. Ordinary static content needs no authority; content
with authored callbacks/Logic refuses without context. Hosts can mount against
the parent's exact state without creating a second store:

```pascal
LView := MountNyxLCLDiagramEditor(LEditor, LDocument, LHost, nil, nil,
  ndcmRuntime, LContext, LResolver);
```

The browser mount accepts the same portable mode/context/resolver choices.
An explicit `AState` must be the context's exact store. Nil state adopts that
borrowed store; neither supplied clones nor silent replacements are admitted.
Without either state or context, independent defaults remain owned by the view.
An inactive context or resolver without context refuses. All mounting belongs
to UI; callers disconnect before disposing hosts, context owners or borrowed
stores. No context/authority becomes part of saved diagram data.

The canvas's existing view-behavior attachment binds ordinary authored callback
registrations in order, then their declared graph observation policy. The supplied
Logic resolver remains lazy and participates in ordinary graph invocation. Geometry
keeps the attachment; content replacement and Disconnect revoke its context and
generation without cancelling parent application authority. Retained child ports
report inactive after either their attachment or parent lease retires, and refuse
every borrowed accessor before entering disposed receivers. Hiding a floating
window retains its same mounted lifetime; explicit Disconnect retires it.

`ndcmDesignPreview` renders authored content without constructing project
callbacks or graph attachments. Editor chrome remains active, and visible status
identifies Design preview. Ordinary Studio deliberately selects this mode, so a
reusable with valid but not yet compiled/registered callback implementations can
be designed and saved. Preview is not application execution or proof that a
handler implementation compiles; ordinary application/view builds retain that
separate responsibility. Direct runtime hosts explicitly supply their authority.

## Reference and remaining behavior

The reference breadth is the official [feature overview](https://www.drawio.com/docs/features/)
and [layer manual](https://www.drawio.com/docs/manual/layers/), checked 2026-10-16.
The overview describes libraries, connector editing, formatting, keyboard tools,
import/export and advanced layout/navigation. The layer manual describes ordering,
visibility and locking. These establish comparison scope, not Nyx implementation
or format compatibility. The full requested scope stays with the owners below.

| Behavior | Current source evidence and remaining outcome | Owner |
| --- | --- | --- |
| Infinite pan/zoom, grid, snapping, rulers/guides | Public canvas exists; diagram editor integration, rulers/guides and full input quality remain open | [Canvas](../TODO/NS-3_canvas-workspace_01.md), [Diagram](../TODO/NS-3_diagram-editor_01.md) |
| Shape and connector libraries | Three mathematical shape recipes are directly authorable; two routing policies; custom recipes/libraries remain open | [Diagram](../TODO/NS-3_diagram-editor_01.md) |
| Text and formatting | Single-line Unicode captions, typed fills/strokes/theme roles; editing, wrapping, fonts and connector labels remain open | [Diagram](../TODO/NS-3_diagram-editor_01.md) |
| Selection/marquee | Runtime vertex/edge pointer and ordered selection/highlighting with eight resize grips; marquee remains open | [Diagram](../TODO/NS-3_diagram-editor_01.md) |
| Move/resize/rotate | Captured move/resize previews and one release commit, keyboard/buttons and grouped properties reroute; rotation remains open | [Diagram](../TODO/NS-3_diagram-editor_01.md) |
| Alignment/distribution, snapping/guides | Typed optional move/resize grid snapping and per-sample Alt bypass; alignment/distribution and guides remain open | [Diagram](../TODO/NS-3_diagram-editor_01.md), [Canvas](../TODO/NS-3_canvas-workspace_01.md) |
| Groups, containers, swimlanes | Full diagram membership/transforms remain open | [Diagram](../TODO/NS-3_diagram-editor_01.md) |
| Layers, order, locking/visibility | Scene layers exist; graph-to-layer semantics and editor controls remain open | [Diagram](../TODO/NS-3_diagram-editor_01.md) |
| Multiple pages | Document pages exist; independent diagram page lifecycle remains open | [Diagram](../TODO/NS-3_diagram-editor_01.md) |
| Clipboard and duplicate | Independent snapshots exist; semantic selection/clipboard operations remain open | [Diagram](../TODO/NS-3_diagram-editor_01.md) |
| Undo/Redo | Explicit Studio Save joins exact ordinary paired history; independent runtime local command history remains open | [Diagram](../TODO/NS-3_diagram-editor_01.md), [Workflow](../TODO/NS-4_agent-workflows_01.md) |
| Search | Exact typed reference lookup exists; text/library search remains open | [Diagram](../TODO/NS-3_diagram-editor_01.md) |
| Keyboard/touch/pen | Native pointer selection, focused-button nudges and ordinary property controls; full gesture/focus/accessibility and browser execution remain open | [Diagram](../TODO/NS-3_diagram-editor_01.md), [Parity](../TODO/NS-2_parity-accessibility_01.md) |
| Connectivity | Shape/free/named-port endpoints, guarded existing-edge reconnection, self-loops, parallel edges and independent arrows exist; native port input/source/history evidence is separate from browser compilation. Endpoint drag grips, editable waypoints, curves, obstacle routing and transformed/grouped endpoints remain open | [Diagram](../TODO/NS-3_diagram-editor_01.md) |
| Embedded controls, graphics/resources and programs | Typed image/reusable content, physical ports, public Content/Ports panels and runtime/design-preview authority share canvas/resource ownership; hosted-resource publication and explicit live source refresh have native evidence, with browser execution, broader source/state/resource-owner changes, masking/programs and full parity still open | [Diagram](../TODO/NS-3_diagram-editor_01.md), [Canvas](../TODO/NS-3_canvas-workspace_01.md) |
| Palettes, Inspector, layers/pages, minimap and adaptive panels | Public Nyx runtime editor has collapsible properties/readable zoom; palettes, layers/pages, minimap and complete editor integration remain open | [Diagram](../TODO/NS-3_diagram-editor_01.md) |
| Source, serialization and bounded MCP | Typed declarations, crafted generation, paired history, targeted continuations, explicit Studio Save and bounded saved graph operations exist; authenticated observing admission and full source/behavior breadth remain open | [Diagram](../TODO/NS-3_diagram-editor_01.md), [Codegen](../TODO/NS-1_codegen_01.md), [Workflow](../TODO/NS-4_agent-workflows_01.md) |
| Diagram compatibility/import/export | Nyx visual JSON only; draw.io formats and editor import/export remain open | [Diagram](../TODO/NS-3_diagram-editor_01.md) |
| Accessibility, budgets, visual quality and delivery | Limits and bounded native input are distinct from production measurements, browser execution, full parity and installed delivery | [Diagram](../TODO/NS-3_diagram-editor_01.md), [Performance](../TODO/NS-3_extension-performance_01.md), [Parity](../TODO/NS-2_parity-accessibility_01.md) |

## Qualification

`tools/test-diagram.ps1 -EvidenceDirectory build/<fresh-packet>` compiles checked
native consumers, executes the portable suite and ordinary native controls at
wide/compact allocations, then compiles/stages matching pas2js consumers with the
matched runtime. It starts no HTTP host. Native controls exercise actual Nyx event
registrations, retained drawing updates, rejected connected removal and retirement.
They do not establish physical hardware input, Studio paired history or a complete
WYSIWYG diagram editor. WORK.md records exact counts, failures, captures and the
retained HTTP/observing delivery gate. No browser execution or full parity follows
from browser compilation.

Add `-Editor` to the maintained command to also exercise both independent public
editor mounts, real target callbacks, property guards/caret, typed expected
revisions, prepared geometry, creator callouts and retirement. Matching browser
consumers stage only. WORK.md owns exact accepted runs and retained failed
fixtures; neither compiler admission nor native input proves browser execution.

Add `-Documents` for the checked portable document/source/history consumer and
five exact native constructor variants: generated, handwritten, changed, removed
and recreated. Matching browser consumers compile only. Combine `-Editor` to
reopen the maintained native editor through its serialized document declaration.
No command starts an HTTP service or changes installed projects/configuration.
Use `-Authoring` with `-Editor` to run the production semantic dispatcher and
actual ordinary native Studio Save/cancel/reload/history journey at wide/compact
sizes, then compile and execute its exact saved Pascal companions. Matching
browser consumers are staged only; execution still requires the separate HTTP
qualification boundary.

Add `-Manipulation` for portable signed/fractional move, eight-grip resize,
snapping/bypass, stable projection bounds and endpoint-preservation checks.
Combined `-Editor -Authoring -Manipulation` also exercises actual native capture,
coalesced preview, final release, cancellation and queued teardown, then saves
ordinary Studio drag/reconnection through paired history and exact companions.
Matching browser consumers compile/stage only; this command starts no HTTP host.

Add `-Content` for prepared membership, rich content/codec/source/semantic
checks, ordinary native Content-panel input and Studio Save at both widths.
The maintained Pascal image generator supplies packed raster fixtures. Exact
whole companions, including dedicated Unicode/NUL resource qualification, are
compiled/executed natively and compiled for the browser. Displayed native
captures wait for ordinary paint turns; redraw-only captures are not accepted
as evidence of embedded child-window visibility. No HTTP host starts.

Add `-Runtime` for the shared context/route regressions, actual native authored
callbacks/Logic, explicit embedded/floating authority, retirement and ordinary
Studio preview of unresolved handlers. Both native widths and exact Studio
companions execute; matching browser consumers/companions compile only.
