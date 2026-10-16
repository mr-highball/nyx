# Canvas geometry and retained scenes

[Architecture](architecture.md) · [Canvas owner](../TODO/NS-3_canvas-workspace_01.md) ·
[Tool Canvas](../TODO/NS-3_tool-canvas_01.md) ·
[Logic components](../TODO/NS-3_logic-components_01.md)

`nyx.canvas.geometry` supplies immutable signed world points, bounds, camera,
anchored zoom/pinch/fit and independent grid visibility/snapping. Coordinates are
logical world units; viewport coordinates are logical pixels. Limits, absent
values and arithmetic refusal are documented beside the public records.

`nyx.canvas.scene` adds a reference-counted retained scene and immutable spatial
snapshots. It has no DOM, LCL, application, scheduler or editor dependency.
They underlie the explicit runtime workspace composition described below. Full
declarative workspace/Studio integration and behavior graphs remain open.

Optional `INyxCanvasSceneLayouts` now stages a complete copied placement batch;
`INyxCanvasLayoutView` admits that plan and a camera as one runtime publication.
The [Pascal behavior adapter](programs.md#canvas-captures-and-complete-layout-publication)
uses this same boundary, with capture-local handles, typed resource reads and
expected scene/presentation revisions. No control is cloned or replaced merely
to move it. These runtime transactions do not edit document definitions, source
or Studio history; saved script authoring and broader diagram integration remain
open. WORK.md owns actual native/browser evidence and preserving-delivery limits.

```pascal
var
  LWorkspace: INyxCanvasScene;
  LControlsLayer: TNyxCanvasLayerRef;
  LReplyItem: TNyxCanvasItemRef;
  LReplyMemo: INyxMemo;
  LView: INyxCanvasSnapshot;
  LVisibleItems: INyxCanvasQuery;
begin
  LControlsLayer := NyxCanvasLayer('Controls');
  LReplyItem := NyxCanvasItem('Reply editor');
  LWorkspace := NewNyxCanvasScene;
  LWorkspace.AddLayer(LControlsLayer);

  LReplyMemo := NewNyxMemo('reply-memo');
  LReplyMemo.Text := 'Write a reply';
  LWorkspace.AddControl(LReplyItem,
    NyxCanvasPlacement(LControlsLayer, NyxCanvasBounds(-120, 40, 320, 180)),
    LReplyMemo);

  LView := LWorkspace.Snapshot;
  LVisibleItems := LView.Query(NyxCanvasBounds(-200, 0, 800, 600),
    ncqmVisible, 0, 64);
end;
```

The authoring routine also imports `nyx.controls` and both canvas units. Factories
require exact open references; an item reference cannot substitute for a layer
reference. Unicode names retain spaces and case. They admit 1..128 scalars without
controls and require nonspace content. Names remain stable across placement and
ordering. Reusing a removed name is an explicit new admission, not an opaque
runtime handle that proves a prior object's lifetime.

## Ownership and mutation

`AddControl` adopts an unowned managed Nyx tree. A private portable model anchor
retains its actual implementation using the existing tree ownership contract.
Compound descendants, specialized interfaces and creator implementations remain
the same objects. There is no closed canvas-only widget catalog. A root already
owned by a document or another item refuses; extract through its current owner
or explicitly clone before admission. Never extract/reparent/free the borrowed
root behind the scene's anchor.

`Control(Item)` returns the retained original `INyxNode`; callers may query its
specialized interface and edit ordinary properties or descendants on the
authoring thread. Removing an item or releasing its scene clears the anchor's
parent link and releases that ownership. Independently retained root/descendant
interfaces survive according to Nyx's normal lifetime rules. Content owns no
reference back to the scene.

Scenes mutate on one authoring thread. Successful scene operations advance their
revision once; ordinary control-property edits belong to the future document/
history integration and do not implicitly advance this geometry revision.
No-op scene operations still advance revision. Admission failures preserve the
candidate's ownership and current scene. Layer removal refuses nonempty layers.
`Layers[Index]` exposes detached metadata even for an empty layer.

Pascal compilers can retain fluent return temporaries until their routine ends.
Use a completed construction scope when checking final interface retirement;
setting one local to `nil` does not prove that every compiler-created owner ended.

## Spatial snapshots

A snapshot copies only item identities, world bounds, effective visibility/
pickability and paint order. It retains no scene, control tree or renderer.
Publish a completed snapshot through the normal safe worker handoff; concurrent
queries use independent local traversal/result storage. Capturing a mutable scene
from another thread is unsupported.

Layer order determines painting; within each layer, insertion and `OrderItem`
determine painting. Snapshot metadata is back-to-front. Query results are
front-to-back with an exact total match count, offset and a limit of 1..256.
The scene admits at most 10,000 items and 256 layers. Closed bounds include
touching edges and point/line content. `Pick` returns the frontmost broad-phase
interactive item or a typed absent reference. Precise shape hits, control-local
input dispatch and keyboard/accessibility alternatives belong to adapters.

Hidden items/layers do not enter visible or interactive results. Pickability is
independent of visibility; `ncqmAll` deliberately includes hidden content. No
query changes source flags. Returned metadata and results remain independent
after edits or complete scene retirement.

A balanced bounding-volume tree splits the widest union axis at its median.
Stable merge sorting makes construction O(n log² n), with logarithmic tree depth
even for coincident items. Internal unions retain finite edges spanning both
world extremes without trying to admit that entire span as one content bounds.
Disjoint branches prune geometry tests. The current query still uses O(n) local
storage/rank work to order matches; a small result is not a claim of O(k) query
work. `VisitedBounds` describes geometry-index work only. Native/browser fixtures
compare exact queries against a complete brute-force oracle and exercise limits,
world extremes, independent ownership and four real native workers.

## Live runtime workspace

`nyx.canvas.workspace` exposes `INyxCanvasWorkspace`, implemented by
`TNyxCanvasWorkspace`, as a public managed Panel/Row/Button composition:

```pascal
LWorkspace := NewNyxCanvasWorkspace('workshop', LScene)
  .WithCamera(NyxCanvasCamera(800, 600, NyxCanvasPoint(0, 0)))
  .WithGrid(NyxCanvasGrid(20, ncgsDots, ncsmFree))
  .WithScaling(nccsReflow);
```

The component owns its scene and ordinary Nyx chrome. Mount with
`MountNyxBrowserCanvasWorkspace` or `MountNyxLCLCanvasWorkspace`, supplying the
workspace, source document context and target host. Mounts copy that context into
private admitted documents, including detached scene fragments; they preserve
normal model ownership/identity/state/resource checks. The original document and
injected interfaces remain independent. Each item uses the existing Nyx renderer
for live controls, binding, events and reusable resolution. Creator setup callbacks
can register ordinary target factories before each item mounts.

The host and a supplied application store are borrowed and must outlive their
mount. Otherwise each view owns an independent default store. A view owns an
independent camera copied from the workspace's initial configuration. All live
view methods and final connected-view release belong to the UI thread. Native
worker mutation refuses through Nyx's scheduler; workers use immutable snapshots
and queued UI work. Views never retain editor objects through the scene tree.

Both targets clip item hosts to a real viewport. Off-screen items mount lazily;
already mounted items retain their renderer, local text and subscriptions when
culled. Focused visible editors move beyond the clipped viewport while staying
alive, instead of hiding their parent and transferring focus. Removing/replacing
an item through `Scene` followed by `View.Refresh` retires only its mount.
Scene changes require that explicit refresh; ordinary content edits are not
automatically synchronized into existing renderer copies.

Scaling is a closed portable choice. `nccsReflow` scales live allocation and lets
the ordinary control layout reflow; fonts keep their logical pixel size.
`nccsLogicalSize` keeps allocation fixed while position follows the world camera.
These policies keep native editors interactive and never substitute screenshots.
They are not arbitrary affine or typography scaling. A visible control exceeding
32,768 logical pixels on either axis refuses before camera or mount changes;
huge off-screen coordinates cull before integer allocation.

The view supplies typed pan/anchored zoom/fit/home operations. Ordinary Nyx
buttons provide non-gesture navigation. The background supports keyboard, wheel
and captured pointer pan; browser contacts additionally support anchored pinch
and cancellation/lost capture. Native mouse capture loss cancels its pan.
Normal editor keys and wheels retain their control behavior; Ctrl-wheel requests
canvas zoom. Browser `touch-action: none` is confined to a background sibling,
preserving default touch editing/scrolling through the embedded controls' ancestry.
This follows the direct-manipulation/capture contracts in
[Pointer Events Level 3](https://www.w3.org/TR/pointerevents3/#declaring-direct-manipulation-behavior)
and [wheel units](https://www.w3.org/TR/uievents/#event-type-wheel), checked 2026-10-14.
Host input assertions remain separate from real touch/pen/trackpad/IME/AT evidence.

Fit respects the live allocation policy. Fixed-size controls center by their
actual extents; grouped fit can reduce their position scale through bounded
bisection. Insets shrink when necessary to retain readable fixed allocations.
Caller zoom limits and controls larger than the viewport can prevent complete
fit, matching the camera's existing best-effort contract.

Grid dots/lines consume the existing copied Nyx theme background/border roles.
The native surface has keyboard focus/name/help; the browser surface is a labeled
region. Complete accessibility, new canvas-specific theme roles, rulers/guides,
custom paint providers and injected vector/text/image resources remain open.

The maintained runtime consumer is:

```powershell
./tools/build.ps1 -Target canvas-workspace -CanvasWorkspaceEvidenceDirectory <new-packet>
```

It executes actual checked Win32/LCL controls and stages the matched browser
consumer. Execute `web/canvas-workspace.html` over HTTP separately. Desktop and
CSS-390 checks include live binding, ordered callbacks, same-face editing/focus,
pan/zoom/cancel/pinch, wheel ownership, culling, independent cameras/default stores,
removal/replacement and callback-driven disconnect. Native checks add real worker
refusal. The small English workshop remains interactive after browser assertions.

The optional program journey additionally qualifies a remounted bound status with
actual browser/native host hit testing and visible foreground pixels. Keep native
PaintTo/GetFormImage exports separate: they can omit overlapping child windows.
Screen captures must invalidate bitmap caches before PNG export; the maintained
Pascal fixture uses Canvas.CopyRect and explicitly scoped borrowed DC ownership.
A raw GDI write after a prior PNG assignment produced stale duplicate images and
an incorrect missing-card diagnosis. WORK.md retains that correction and the
separate remaining root padding/face-size, hardware and full visual-parity limits.

## Saved document definitions

`TNyxDocument.Canvases` owns a registry of immutable `INyxCanvasDefinition`
plans. `TNyxCanvasRef` is distinct from item/layer identities. Definitions retain
typed page/reusable root references and placements, rather than nodes or views.
Document validation resolves those roots; missing content refuses the complete
candidate. Registry clones have independent membership and retained interfaces
may outlive the document without a backreference.

```pascal
LDocument.Canvases.Define(NyxCanvasRef('Reply workspace'),
  NewNyxCanvasDefinition
    .WithCamera(NyxCanvasCamera(800, 600, NyxCanvasPoint(0, 0)))
    .WithGrid(NyxCanvasGrid(24, ncgsLines, ncsmGrid))
    .WithScaling(nccsLogicalSize)
    .WithLayer(NyxCanvasLayer('Controls'))
    .WithItem(NyxCanvasItem('Reply card'), NyxReusableRoot('reply-card'),
      NyxCanvasPlacement(NyxCanvasLayer('Controls'),
        NyxCanvasBounds(-180, -140, 360, 280))));

LWorkspace := NewNyxCanvasWorkspace('reply-workspace', LDocument,
  NyxCanvasRef('Reply workspace'));
```

Materialization copies each referenced root into an independently owned runtime
scene. Two workspaces using the same definition have independent control trees,
stores and cameras. Ordinary target mounts still borrow their host/application
store and render copied contexts through the existing Nyx adapters.

Design version 13 stores these declarations and the previous typed document
families together. Versions 1..12 keep an opaque `canvases` extension unchanged;
promotion with that extension refuses instead of silently reinterpreting it.
The registry admits at most 64 definitions; each strict version-1 descriptor is
limited to 1 MiB of UTF-8 JSON and the existing 256 layers/10,000 items. Unknown
fields/choices, duplicate identities, absent layers and invalid geometry refuse
before publication. Alternate implementations normalize through this explicit
descriptor boundary before becoming immutable owned plans.

Generated Pascal uses readable typed constructors and fluent calls. The bounded
literal source reader admits that construction, and ordinary Studio source
application/paired Undo/Redo preserves exact complete designs and Pascal. This
does not establish unrestricted handwritten helper/conditional grammar. Named
geometry/definition locals and richer compiler-backed continuation remain open.

Maintain this boundary with:

```powershell
./tools/build.ps1 -Target canvas-document -CanvasDocumentEvidenceDirectory <new-packet>
```

The Pascal consumer exports the exact source/design companion; actual checked
native controls and matched browser compilation consume that source. Execute
`web/canvas-document.html` and `web/canvas-document-controls.html` over HTTP
separately. The browser compares its compiled complete document with the exact
native companion, while editing a real bound memo and retaining it across camera
navigation. Source admission and application execution remain distinct evidence.

## Ordinary rendering and authoring

The specialized `INyxCanvas`/`TNyxCanvas` declaration attaches a distinct saved
scene reference. Normal browser/LCL page rendering materializes independent
content and borrows the owning application store. Embedded callbacks enter the
application event stream; parent retirement disconnects retained public views.
Enabled/read-only restrictions compose with authored child settings through
runtime capabilities and never become saved properties.

```pascal
LCanvas := NewNyxCanvas('reply-canvas')
  .WithScene(NyxCanvasRef('Reply workspace'));
LPage.Add(LCanvas);
```

Studio consumes the public Nyx scene picker. Its bounded owner/registry baseline
guards queued input; applying a choice is one paired design/Pascal Undo step.
Pending source drafts and stale presentations refuse. `NyxDefineCanvas`,
`NyxRemoveCanvas`, `NyxAttachCanvas` and `NyxDetachCanvas` supply typed grouped
operations. Root removal discovers saved content dependencies, including
unmounted definitions; admission rejects combined canvas/reusable recursion.

Reusable declarations distinguish three choices: no local scene inherits the
template; `Configure.CanvasScene(NyxCanvasRef(...))` selects an independent saved
scene; `Configure.NoCanvasScene` explicitly empties that instance. Typed
`InheritCanvasScene` removes the local declaration. Named-part overrides use the
same facade. Version 13 retains absence/string/null, including empty-only designs.
`NyxEmptyCanvas` supplies the explicit semantic mask; `NyxDetachCanvas` restores
inheritance. Effective target admission rejects non-canvas reusable roots/parts.
The public picker exposes all three choices and its owning processor checks the
captured baseline again before paired publication.

Current-source `nyx_node` optionally pages saved canvas layers/items. The existing
transaction tool accepts the exact typed operations with ordinary revision,
permission and retry guards. These additions retain the 23-tool inventory;
they are not installed in the frozen observing release. Initial camera metadata
does not report a live runtime viewport.

`canvasScope` selects local or effective saved context, with effective as default.
`localDeclared` distinguishes an empty mask from inherited absence. Shared native
reuse, compiled native instances and actual desktop browser Studio input have
current evidence; standalone browser reuse and compact validation remain open
after retained pre-navigation debugger deadlines. WORK.md owns exact limits.

The newer handwritten continuation consumer uses an asynchronous exact-input
host boundary. Six emitted stages now construct complete expected documents on
native and executed HTTP browser targets; actual reconstructed instances retain
editing and independent cameras at desktop/CSS-390. This qualifies that consumer,
without reclassifying the earlier failed standalone startup attempts.

Handwritten scene edits emit typed registry Define/Remove and local scene
configuration in chronological continuation routines. Unchanged helper-produced
plans remain computed. A later helper edit still changes the untouched scene;
explicitly edited scenes retain their authored values. Registry reordering and
other unexpressed meaning refuse the entire proposal. Ordinary native Studio
publishes exact compiler-owned pairs with one Undo step; source preparation alone
never grants execution provenance. The maintained `canvas-continuation` target
executes native consumers and stages matching browser tests for explicit HTTP use.

The maintained `canvas-authoring` build target qualifies the shared contract,
compiled ordinary native controls and Studio picker, and stages matched browser
consumers plus their source worker for explicit HTTP execution.

## Typed drawing content

`nyx.drawing` provides immutable paths, fills/strokes, single-line Unicode text
and reference-counted display lists. `INyxDrawing/TNyxDrawing` is an ordinary
specialized Nyx control: place it in any layout or embed it as scene content.
The toolbox starter has useful themed geometry. Descriptor construction stays
empty so generated Pascal owns its exact declaration.

```pascal
LConnectionDrawing := NewNyxDrawing('connection-graphic')
  .WithText('Connection graphic')
  .WithPlan(NewNyxDrawingPlan(NyxCanvasBounds(0, 0, 320, 180))
    .Path(NyxDrawingPath(NyxCanvasPoint(24, 140))
      .CurveTo(NyxCanvasPoint(100, 20), NyxCanvasPoint(220, 160),
        NyxCanvasPoint(296, 40)),
      NyxDrawingStyle.Stroke(NyxThemePaint(ntcAccent), 3))
    .Text('Make connections', NyxCanvasPoint(24, 24),
      NyxThemePaint(ntcText), 18, ndtaStart, True)
    .Build);
```

Coordinates and curve control points belong to the positive view box. Paths
have one contiguous figure; Close ends further authoring. Open fills close
implicitly while strokes remain open. Both adapters use 32 subdivisions per
cubic curve, centered contain mapping and a top text anchor. LCL integer rounding,
glyph metrics and antialiasing remain target-specific; this is not pixel parity.
Text preserves supplementary Unicode; this drawing primitive explicitly refuses
control characters/NUL/newlines instead of truncating them in native TextOut.
Normal Nyx editable/text controls retain their wider text contract.

Admission limits are 2,048 commands, 1,024 steps per path, 65,536 total sampled
points and 1 MiB of encoded UTF-8 plan data. Allocations admit 0..32,768 logical
pixels and mapped stroke/font metrics at most 4,096. These are refusal boundaries,
not performance benchmark claims. Build captures an independent list; failed
command admission preserves the builder. Renderers normalize the entire candidate
before replacing its accepted face.

`INyxDrawingProvider.Build` receives copied dimensions and an immutable effective
palette. It returns a detached plan without retaining a model/view/renderer.
Select a distinct `NyxDrawingProvider('application-provider')` reference through
`WithProvider`; register the managed implementation with
`Renderer.RegisterDrawingProvider` before mounting. Registries copy into ordinary
candidates and embedded canvas renderers. An unknown provider refuses, rather
than silently substituting the saved fallback. Runtime implementations are host
registrations, never serialized executable class names or callbacks.

Providers construct synchronously on the owning UI thread. Explicit Configure/Sync
failure propagates and preserves the accepted drawing. Resize callbacks retain
the accepted face and expose `LastError` instead of throwing into host input.
Browser retirement disconnects ResizeObserver; native faces release their copied
theme and managed plans/providers. A creator must avoid reference cycles.

Generated source uses specialized `INyxDrawing`, typed `SetNyxDrawingPlan` /
`SetNyxDrawingProvider` and fluent geometry, with the same literal admission
contract. Known drawing settings do not become raw behavioral property strings.
Generic helpers support derived controls and named overrides. The saved immutable
declaration is the explicit extension/persistence boundary.

Resource graphics reuse ordinary `INyxImage` and its existing resource bindings.
The maintained drawing scene includes a packed image and an editable bound memo
alongside static/provider graphics. Filling roots and reflow allocations let these
adapt to a canvas host; fit should follow actual target allocation. Fixed-size
controls deliberately retain their sizes, so a too-small viewport cannot guarantee
a whole-scene nonoverlapping fit.

`tools/build.ps1 -Target canvas-drawing` executes portable admission, exact
compiled companions and actual native controls, then stages matching browser
consumers for explicit HTTP execution. WORK.md records executed desktop/narrow
input, captures and limits. This source is not installed in the observing LAN.

## Remaining integration

The separate [visual diagram preparation](diagrams.md) uses this same ordinary
Drawing content rather than a new platform painter. It supplies typed shapes,
endpoints and rerouting, not a completed editor, canvas geometry bridge or
executable behavior wire. Editable intent/source/history and diagram interaction
remain with the existing diagram and semantic-workflow owners.

The explicit workspace mount API remains available for runtime-owned scenes.
Ordinary declarations now reconstruct saved scenes through their canvas control.
Broader source grammar, scene authoring and observing delivery still
need separate evidence. Scoped reuse is not complete target/parity quality.
The historical generated reference is not evidence of
current catalog breadth or complete canvas quality.

Broader source continuation and reusable overrides must retain these typed
references and ownership. Bounded scene/layer/
viewport/selection queries and grouped revision-aware mutations belong to the
existing [semantic workflow owner](../TODO/NS-4_agent-workflows_01.md); the installed
server currently exposes no canvas operation. Current source attaches callbacks/
Logic routes to embedded runtime items and provides a specialized Tool Canvas
port inspector; see [Logic](logic.md#embedded-controls-and-tool-canvas). Full
Tool Canvas connection editing,
the Pascal interpreter and the nonvisual Logic collection retain their separate
owners. Standalone JSON/query/HTTP/file operations and useful connected outcomes
are required; metadata cards alone cannot establish that feature.

Maintain the shared native/compiler evidence with:

```powershell
./tools/build.ps1 -Target canvas-scene -CanvasSceneEvidenceDirectory <new-packet>
```

The command runs checked native assertions, checks two deliberately invalid
contracts with both compilers, and stages the matched browser consumer. Serve
`web/canvas-scene.html` over HTTP and execute it separately; browser compilation
alone is not browser evidence. This command never starts a listener or changes
an enrolled project.
