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
These are foundations for a rendered workspace, rather than an implemented
canvas control or a behavior graph.

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

## Remaining integration

The full public workspace control, browser/LCL painting, native scaling policies,
clipping and editable embedded-control navigation remain open. So do custom
paint providers, vector/text/image injection, resources, theme tokens, rulers,
guides and actual hardware/accessibility qualification. Geometry/scene fixtures
do not establish any of those behaviors.

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
