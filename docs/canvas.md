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

## Remaining integration

The runtime component needs its explicit mount API. Generic document rendering,
serialization and generated source currently know only its ordinary chrome node;
cloning/exporting that node does not reconstruct the scene or workspace behavior.
Do not present such an export as a complete canvas application. A typed owned
document definition, crafted source/admission and reusable overrides must bridge
that gap before Studio can author/compile these workspaces through its normal model.
The existing 76-kind generated reference has not gained a registered canvas kind.

Scene persistence, crafted source/admission, document history and reusable
overrides must retain these typed references and ownership. Bounded scene/layer/
viewport/selection queries and grouped revision-aware mutations belong to the
existing [semantic workflow owner](../TODO/NS-4_agent-workflows_01.md); the installed
server currently exposes no canvas operation. Tool Canvas ports/graph execution,
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
