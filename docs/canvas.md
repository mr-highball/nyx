# Canvas geometry and retained scenes

[Architecture](architecture.md) · [Canvas owner](../TODO/NS-3_canvas-workspace_01.md) ·
[Tool Canvas](../TODO/NS-3_tool-canvas_01.md) ·
[Logic components](../TODO/NS-3_logic-components_01.md)

`nyx.canvas.geometry` supplies immutable signed world points, bounds, camera,
anchored zoom/pinch/fit and independent grid visibility/snapping. Coordinates are
logical world units; viewport coordinates are logical pixels. Limits, absent
values and arithmetic refusal are documented beside the public records.

## Caller-defined navigation bounds

An ordinary camera remains unbounded by default. Callers opt into an immutable
world region with a closed Pascal policy:

```pascal
LBoardCamera := NyxCanvasCamera(960, 640, NyxCanvasPoint(0, 0), 1)
  .Within(NyxCanvasBounds(-1600, -1200, 3200, 2400), ncbViewport);
```

`ncbViewport` keeps each visible axis inside the region when it fits. When that
axis is larger than the region, its center stays at the region midpoint; Nyx
does not silently change zoom to make it fit. `ncbCenter` constrains only the
camera center and permits visible content outside the region. Zero width or
height locks that axis. `WithoutBounds` explicitly clears the optional policy;
the existing portable numerical/zoom limits always apply.

Pan, anchored zoom, pinch, viewport resize, Fit, keyboard navigation and interpreted
`SetCamera` share this camera contract. Boundaries take precedence over an anchor
that would require leaving the region. A pinch admits its final zoom/translation
together; `AtCenter(Point, Zoom)` likewise applies one final frame. Finite large
pan overshoots reach the configured edge before portable point admission.
`SameFrame` includes boundary mode and region, so matching position/zoom alone
does not authorize a stale behavior-program publication. Focus navigation reports
an incomplete result when constraints prevent revealing its requested content.

Saved bounded scenes use descriptor version 4 and a strict `camera.boundary`
object. Existing unbounded version 1/2/3 shapes and bytes remain unchanged.
Decoding refuses a saved center outside its policy instead of silently moving it.
Crafted Pascal emits typed `.Within(...)`; literal source admission and existing
revision-aware scene transactions use that same contract. The semantic camera
query includes the optional boundary only when defined. No additional tool is
introduced, and current-source schemas are separate from any installed release.

The ordinary Canvas Inspector consumes `NewNyxCanvasBoundaryEditor`: a reusable
public Nyx form with `NyxCanvasBoundaryPart` references and named parts. Mode and
four numeric fields remain proposals until explicit Apply through the owning
editor's paired source/history command. Invalid numbers, stale scene intent and
pending Pascal drafts refuse without rewriting the accepted pair. Its copied
`TNyxCanvasBoundaryDraft` preserves unfinished text across presentation rebuilds;
another scene never receives it. A changed context parks the proposal unless it
is exactly the result of applying that proposal. Untouched defaults retain the
existing presentation version/bytes; an unfinished navigation draft opts into
presentation version 20.
Capture and restoration share a copied-packet UTF-8 budget of twice
`MaximumNyxCanvasDefinitionBytes`; refusal leaves the mounted fields and previous
retained draft untouched. Individual inputs also retain Nyx's existing state
value limits.

This is the bounded-navigation portion of the precision workspace. Full
input/accessibility/visual quality, budgets and browser execution remain required
by the original canvas owner. WORK.md separates current native evidence from
browser compilation; neither establishes full criterion acceptance or delivery.

## Rulers and guides

`nyx.canvas.precision` supplies immutable `TNyxCanvasRulers`,
`TNyxCanvasGuide`, distinct `TNyxCanvasGuideRef` identities and ordered
`TNyxCanvasPrecisionOptions`. Rulers independently show the top/left axes.
Their shifted world origin, world-units-per-display-unit, caption, minimum
increment, logical-pixel interval, minor divisions and strip thickness are typed
fluent choices. Adaptive 1/2/5 major intervals and at most 512 total visible ticks
avoid enumerating off-screen coordinates. Near world extremes, spacing coarsens
to keep indices within exact floating-point identity.

```pascal
LDefinition := NyxCanvasPrecision(NewNyxCanvasDefinition)
  .Rulers(NyxCanvasRulers.Axes([ncaxX, ncaxY])
    .FromOrigin(NyxCanvasPoint(0, 0))
    .Units(10, 'cm').Interval(0.1, 72, 5).Thickness(24))
  .Guide(NyxCanvasGuide(NyxCanvasGuideRef('Left margin'), ncaxX, -120))
  .Guide(NyxCanvasGuide(NyxCanvasGuideRef('Top margin'), ncaxY, 60).Locked(True))
  .Snapping(ncgspVisible, 8)
  .Done;
```

X identifies a world X coordinate, hence a vertical guide; Y identifies a
horizontal guide. Guide visibility, input locking and snap policy are independent.
Locked guides can still be snap references. Tolerance is in logical pixels,
independent of zoom; exact ties keep declaration order. Call
`SnapNyxCanvasGuides` or the optional view's `SnapGuidePoint` to align an
application placement. An excluded guide cannot snap back to its own old position.
The guide gesture owner applies grid snapping first, then nearby guide hits
measured from the raw pointer. Diagram item manipulation currently consumes its
grid contract; automatic item-edge/center snapping to these guides remains open.

Optional `INyxCanvasWorkspacePrecision` configures a runtime workspace.
`INyxCanvasPrecisionView` inspects/replaces independent view options and selects
a named guide without changing the shared setup or authored document. Both
adapters paint themed ruler strips above retained controls and guides behind
them. Strips join focus-navigation occlusion; Fit/Reveal retain their existing
completeness contract. Camera updates keep embedded inputs and caret ownership.
The original workspace/view interfaces and their GUIDs remain unchanged.

Drag a ruler into the surface to create a guide parallel to that ruler; drag a visible
guide through exposed background to move it. Preview retains exact accepted
options until one release. Escape, capture loss and disconnect revoke the
preview. Enter/Space on a ruler creates a guide at the camera center. Select a
guide, then use the matching arrow axis to move one logical pixel (Shift: ten),
Delete to remove, or Escape to deselect. Locked guides refuse runtime movement
and deletion. Disabled/read-only/design-preview policies prevent editing;
embedded controls continue to own their input. Hardware, accessibility and full
browser behavior need their separate qualification.

Saved definitions explicitly carrying precision use descriptor version 5, with
either bounded or unbounded camera geometry. Versions 1–4 retain their original
shapes/bytes when precision is absent. The strict precision packet bounds guides
at 256 and its UTF-8 size at 128 KiB, within the existing complete scene budget.
Crafted Pascal, literal source admission and existing grouped scene transactions
carry the same typed values. Removing precision restores the applicable legacy
shape. Handwritten continuation and full source meaning remain under their
existing owner.

The ordinary Canvas Inspector consumes public `NewNyxCanvasPrecisionEditor`
and stable `NyxCanvasPrecisionPart` references. Load deliberately replaces only
guide fields. Apply publishes rulers, snapping and a chosen keep/add-or-update/
remove guide operation together through ordinary paired history. Explicit
authoring can change a lock or remove a locked guide; runtime locks guard input,
not caller authority. Invalid/stale proposals and pending Pascal retain the
accepted pair. `TNyxCanvasPrecisionDraft` copies all eighteen unfinished fields,
parks unrelated scene contexts and consumes only its exact applied result.
Its copied-packet budget matches the navigation draft. Untouched forms preserve
legacy presentation bytes; an unfinished precision proposal opts into version 21.

Existing `nyx_node` gains opt-in `canvasPrecision=true` with `canvas=true`.
Ruler/snapping values accompany independently paged guides:
`canvasGuideOffset` and `canvasGuideLimit` (default 10, maximum 50). Pin
`expectedRevision` across pages. These describe saved intent, not live camera,
selection or gesture state. Existing `canvas-define` accepts version 5 in a
single revision-aware `nyx_transaction`. No new tool or installed schema is
claimed. WORK.md records native execution, browser compilation and hosting limits.

## Retained scenes

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

Optional `INyxCanvasSourceLayoutView.ApplySourceLayout` replaces copied source
and an explicit complete content plan in the same prepared frame. The source is
borrowed during capture and privately cloned. Retained roots keep their actual
controls; new/replaced roots resolve against the candidate context. Source,
optional companion, scene membership and camera install before notifications.
Stale/reentrant/cancelled preparation retains the earlier frame. The public
diagram editor consumes this boundary for deliberate
[live source refresh](diagrams.md#live-source-refresh), including ordinary Studio
reload after accepted edits and Undo/Redo. It grants no automatic watcher,
runtime-store migration or new application/compiler authority. An attached
resource port must already own matching candidate declarations; source refresh
does not silently replace its broker. Both adapters implement the capability;
WORK.md separates actual native input from browser compilation and open parity.

Current-source mounts optionally accept an existing `INyxApplicationResources`
as their `AResources` argument. The caller owns its Start/Stop lifecycle;
Disconnect revokes only this view's subscription. The same broker supplies the
initial context, including already loaded bytes, and prepared updates to chrome
and every retained item. Its original loader/cache/cancellation/diagnostics remain
in force. Source-backed rows and scalar/image bindings join the same publication;
all models install before sequential target notifications. Independent runtime
brokers retain independent catalogs. Nil retains authored/default behavior or
an explicitly configured item-renderer resource owner.

Design-preview hosts can supply the optional final `AHostInput` argument, an
`INyxCanvasHostInput` policy, to keep their own authoring surface interactive.
`NewNyxCanvasHostInput(Item, Content)` creates an immutable grant for that typed
item and the exact supplied root instance; matching source IDs do not suffice.
The grant retains its root and an independent UI scheduler, without retaining a
scene, view or application. Creation, queries and final release belong to UI.
Views retain the policy until Disconnect and query it when mounting a new item
lifetime. Custom policies must not mutate or retire their owner during a query,
or retain it through a cycle. Nil keeps complete preview; runtime mounts ignore
the policy. This affects ordinary control input in the selected subtree only:
application callback/graph attachment remains paused by the global preview gate.
No grant is persisted in application data. Diagram preview uses this boundary
only for its synthesized picture, leaving injected source controls in preview.

Ordinary application renderers forward their existing broker automatically when
mounting saved Canvas controls; application code does not need to find child
renderers or start an additional loader for those items.
Native renderers also retry an initially unallocated nested canvas after normal
parent layout settles. First factory construction stays on the renderer's
explicit refresh stack, preserving its reversible candidate admission boundary;
canvas WM_SIZE continues to avoid constructing an unfinished first candidate.

Valid hosted images without fallback remain empty until bytes are published.
Newly mounted items consume the broker's current context; camera changes, resource
updates and loaded bytes never rewrite source definitions. Leaf groups keep the
coordinator's participant limit effective across retained scene membership. This
is an ownership/allocation rule, not a qualified 10,000-control update budget.
The maintained `tools/test-hosted-images.ps1 -EvidenceDirectory <fresh-output>`
executes native pending/ready/refusal/cancellation/independence/source-history
checks and compiles matching browser consumers. It starts no host. Optional
`-Capture` requires the actual owned window in foreground and refuses otherwise.

The host and a supplied application store are borrowed and must outlive their
mount. Otherwise each view owns an independent default store. A view owns an
independent camera copied from the workspace's initial configuration. All live
view methods and final connected-view release belong to the UI thread. Native
worker mutation refuses through Nyx's scheduler; workers use immutable snapshots
and queued UI work. Views never retain editor objects through the scene tree.

Embedded authored behavior uses a parent-aware weak application port. Its
`Active` value and borrowed accessors require both the item attachment and parent
lease. Closing either makes retained ports inactive without retaining its host
or cancelling an ancestor's shared scope. Item replacement/Disconnect retires
local callbacks and graph generation before physical controls. This same behavior
owner serves ordinary canvas content and the diagram runtime adapters.

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

The new opt-in `nccsUniform` policy keeps a stable world layout and scales its
live presentation, including text, instead of making the layout reflow at each
zoom. `TNyxCanvasControlFrame` distinguishes physical bounds from `LayoutWidth`,
`LayoutHeight` and `ContentScale`. Both visible allocations admit the same extent
budget; a small zoom cannot authorize an arbitrarily large logical layout.
Browser item renderers use an independent logical host and a CSS transform,
retaining their actual inputs and normal binding/events across camera changes.

Mounted standard views expose the optional `INyxCanvasScalingView` in
`nyx.canvas.observers`. Its read-only `ControlScaling` returns the copied typed
policy on the UI thread; retirement refuses access. It grants no document or
camera mutation and does not change an older view interface's ABI.

Tool Canvas operation cards also follow an explicit uniform policy. Each uses a
stable 280x320 world allocation with its own scrollable port list. Both adapters
project its ordinary live renderer; navigation extents, culling, wire endpoints
and pointer conversion follow that same factor. Floating connection tools and
control-port strips retain their readable viewport sizes. Earlier scaling policies
and custom views without the optional facet retain their existing card behavior.
Uniform scaling preserves authored operation positions. If those world allocations
already overlap, Fit reports Limited rather than moving the cards or claiming a
complete unobstructed fit. Callers may deliberately move them through the typed
operation-placement contract.

Studio's Properties Inspector exposes **Embedded controls → Zoom behavior** for
a selected saved canvas. In a Tool Canvas, select its authored workspace part.
Reflow, Keep size and Uniform zoom correspond to the three typed policies above.
**Apply zoom behavior** changes the shared scene definition through the ordinary
paired design/Pascal transaction; every use of that scene receives the choice.
The selection alone changes no design. Stale forms and unfinished Pascal refuse
publication through the existing guards. Earlier projects keep their saved policy.

Applications may embed the same public `NewNyxCanvasScalingEditor` Nyx form.
`NyxCanvasScalingPart` supplies typed stable part references, and
`CaptureNyxCanvasScalingEditor` returns a copied scene proposal on explicit Apply.
The host rechecks its complete project context and publishes the returned change;
the form retains no document or mounted view and cannot publish by itself.

LCL now retains a separate world layout frame and projects absolute native
geometry, constraints and font heights after layout. Repeated zoom restores the
logical baseline first, retaining the actual controls, text, focus and selection.
Native integer coordinates round once per frame; nonzero fonts have a one-pixel
minimum. Item hosts retain physical HWND clipping, while responsive layout sees
the logical frame. Buttons/surfaces use a separate absolute paint factor for
rounded faces, strokes, padding and original decoded glyphs; measurement restores
scale one first. Neither the theme tokens nor the original image are resampled
in place. Native strokes also retain a one-pixel minimum.

Native collection grids additionally retain absolute column widths, default and
header row heights, title typography, toolbar and alignment metrics. Their copied
body-height directory contains only the explicitly measured window, independently
of the dataset size. The LCL default-height setter itself still traverses its
native row storage; this is not a qualified large-dataset zoom performance budget.
A native logical-update lease restores the accepted subtree before an adapter
refresh and projects it after that synchronous update. Nested updates reuse that
section. The lease pins the original physical frame and revokes its renderer
access on retirement; callers must close it before returning to the event loop.

Projection-generated resize, selection and scroll notifications do not become
new collection commands. A reentrant canvas admission reads the complete copied
logical baseline while faces change, rather than capturing partially scaled
fonts. Native header resizing converts screen width through both monitor DPI
and canvas zoom, and refuses a drag whose projection epoch changed before its
release. Ordinary runtime queries retain the same grid, selected item and draft.

The Win32 uniform projection now has a shared scroll-chrome attachment in
`nyx.scroll.chrome.lcl`. It retains the actual editor/control HWND and chains a
Comctl32 subclass for nonclient allocation, drawing and hit testing. Semantic
theme colors and absolute logical metrics are copied; projection does not rescale
the previous thumb or replace an editor with an image. `nyx.scroll.chrome` supplies
portable inclusive ranges and geometry, with explicit minimum pixels and a
consistent upward tie rule for nonnegative metrics/travel on both compilers.
The helper admits the complete signed32-bit range with widened page arithmetic.

The native attachment uses full-width LCL positions for custom controls, including
the final release position for grids that disable continuous thumb tracking.
Memo vertical scrolling retains full line positions. Standard Windows edit
horizontal ranges are pixels while their wide relative-scroll API uses character
steps: ordinary thumb positions use exact native pixels, and positions above65535
use the nearest native character step without truncation; endpoints stay exact.
This precision limit is specific to that edit host. Read-only content remains
navigable; disabled/suspended input refuses. Captured collection epochs prevent an
old pointer stroke from resuming after an input authority changes.

Memo viewport restoration also carries a per-attachment receipt and source epoch
through deferred native bounds realization. Queued messages retain no control,
renderer or object pointer. Source replacement and retirement revoke the old
restore; repeated projection before delivery keeps the original accepted anchor.
The original physical retirement frame spans each synchronous native call only.

This scroll repair is a partial Win32 checkpoint. Focused native memo/grid input,
portable native/browser geometry and the existing live-control regression execute;
other scrollable families, actual display/assistive geometry and full target
qualification remain with [scrollbars01](../TODO/NS-3_canvas-scrollbars_01.md).
Win32 `WM_PRINT` now uses that same nonclient painter for diagnostic previews.
It preserves the caller's DC origin/clip, translates the measured client origin,
and retains requested client/child/owned drawing. Client-only and nonclient-only
requests remain separate. Hidden controls can render when explicitly requested
without a visibility check; they still refuse stale physical scroll input.
Inactive bars have no active-looking full-track thumb and cannot capture a drag.
Arrow glyphs follow the corresponding up/down/left/right action and stay within
their physical cells and the HWND's footprint, including tiny allocations.
Client-paint retirement revokes late chrome drawing while the original physical
frame keeps the native control alive until the callback returns. No DC survives
the synchronous call. Pixel/input checks cover memo, grid, scroll area with
embedded children and Tree at three scales. These are diagnostic redraws;
foreground display, hardware input and assistive geometry remain separate gates.
Other widgetsets retain their existing chrome when no explicit policy is set.
The original repair introduced no persisted choice; the opt-in contract below
adds caller control. Run `tools/build.ps1 -Target scroll-chrome` with a
configured external evidence/temp root, or supply a new
`-ScrollChromeEvidenceDirectory`; it executes native checks and stages the browser
geometry fixture without starting a listener. Browser execution remains separate.

Ordinary native scroll areas retain a world-space anchor through uniform zoom.
The projector records the last rounded native offsets separately, so repeated
fractional zoom cannot compound rounding. A real wheel/focus scroll adopts its
new position; a genuine range reduction adopts its reachable clamp. Both child
geometry and automatic ranges settle before restoring the projected offsets.
The same actual child HWNDs move through LCL's scroll implementation. Virtual
logical ports keep their own coordinate ownership, and the canvas camera host is
excluded. The mixed consumer checks both axes against actual on-screen child
origins, cancellable wheel defaults, read-only source keyboard navigation and
nested memo focus/editing. Native size-query retirement also revokes the original
projection before its next widget access.

`scroll-chrome` now stages `canvas-uniform-controls.html` beside the matched
browser fixture. Its executed desktop/compact checks retain CSS scroll anchors;
ordinary browser wheel, source PageDown, nested editing and bound peer/action
updates have separate interaction evidence. These checks do not establish
physical-device, assistive or complete canvas parity.

### Choosing scrollbar sizing

`nyx.scroll.presentation` supplies an immutable `TNyxScrollPresentation` value.
`nyx.scroll.configuration` supplies an additive managed facet without changing
existing control/configuration interface layouts:

```pascal
NyxScrollConfigure(LNotesMemo.Configure).Presentation(
  NyxScrollPresentation.MinimumThickness(24));

NyxScrollConfigure(LSourceCode.Configure.ForPlatform(npfBrowser)).Presentation(
  NyxScrollPresentation.Sizing(nssKeepSize));

NyxScrollConfigure(LNotesMemo.Configure).UseDefault;
```

`nssFollowZoom` follows the content factor; `nssKeepSize` keeps normal chrome
size; `nssMinimumThickness` follows zoom while retaining the chosen cross-axis
floor. `MinimumThickness(1..256)` selects that last mode. Switching modes retains
the authored floor. These are final host coordinates: CSS pixels after browser
content transforms, and native window pixels at the host's DPI. They do not
promise hardware-pixel equivalence across displays. Small available controls
can constrain thumb/arrow length; the thickness floor is not a universal minimum
hit rectangle. Text, content layout and scroll offsets have independent owners.

Platform, viewport and named-presentation scopes use the ordinary configuration
selectors. Empty/absent metadata retains platform defaults outside uniform canvas
projection. `UseDefault` withdraws the explicit choice in the selected scope.
Model values, saved descriptors, generated specialized Pascal and the literal
source editor preserve the exact choice. Studio's scrollable-control Inspector
offers a selector and integer minimum; each committed field change uses the
ordinary paired design/source transaction and one Undo step. Semantic clients
can use the existing bounded property/transaction operations at their explicit
persistence boundary, with expected revisions, rather than a separate scroll tool.

The native adapter currently supports the existing Win32 memo/code, scroll-host,
grid and Tree bridge. It retains the real editor/window and native input. An
explicit unsupported face/widgetset refuses; it does not claim native fallback
as parity. Private table panes inherit their outer table's copied policy;
independent public descendants retain their own intent.

Browser styling also retains the original DOM scrolling face. First explicit
use measures two different gutter allocations in a temporary hidden probe;
selector-support claims alone are insufficient. The receipt lasts for that
window; browser/platform setting changes require a fresh window. A browser that
cannot supply the measured pixel allocation reports unavailable. Unconfigured
creator faces keep their styles. The adapter compensates for the ancestor's
content transform and uses semantic theme track/thumb colors. This capability
is deliberately conditional: standard `scrollbar-width` has no pixel-length
option ([Chrome documentation](https://developer.chrome.com/docs/css-ui/scrollbar-styling)).
Source/model/history tests, actual browser interaction and native diagnostic
redraw evidence remain distinct from full Studio, hardware and assistive parity.

Pinned panes copy only the middle grid's current measured row window, clearing
their preceding sparse overrides before installing the new window. Text-window,
pane and header-button updates share one logical section. Scroll anchors restore
after the complete subtree has its final bounds and metrics; a temporary lack of
overflow while normalizing the tree cannot silently reset horizontal scrolling.
Native auto-fit uses owned copies of the exact logical header/body font heights,
retaining monitor DPI independently of canvas zoom. It never infers a logical
font by dividing rounded physical pixels or changes a live font to measure it.

Pointer and drag payloads use logical outer-face positions. The inverse screen
mapping applies the same transform, including native clipped-content offsets.
Browser axis-aligned face ratios include ancestor uniform transforms; arbitrary
rotation/skew remains outside this mapping. Designer resize/move helpers consume
that same inverse instead of applying another local correction. Geometry queries
check native ancestor client clips or browser ancestor overflow clips before
reporting a visible descendant, retaining the full face bounds on success.

This remains an unfinished capability, not complete target parity. The composed
memo/card consumer qualifies native geometry/font retention, fitted visibility,
binding, pointer round trips and ordinary action callbacks at wide/compact host
sizes. Actual browser clicks, editing and action activation also pass after Fit.
Native paint inspection uses an explicitly labelled diagnostic redraw; the
foreground-only display capture refused and remains separate evidence.
The native data consumer additionally qualifies plain, wrapped, resizable and
pinned tables at both host widths, including retained scroll anchors, real F2
editor draft/caret/selection, query refresh, header dragging and retirement during
an entered update. Repeated zoom and actual vertical/horizontal scrolling retain
pinned cell alignment and Fit-button size; native keyboard auto-fit publishes
the same logical width through zoom and preserves an unfinished pinned edit.
Native data-control paint inspection is also a diagnostic redraw.
Other internal scroll/drawing metrics, nested canvases, full drag/capture/keyboard journeys,
large-dataset performance and complete visual/assistive quality still need work.
Existing policies and standalone example defaults remain unchanged. Follow
[workspace02](../TODO/NS-3_canvas-workspace_02.md) for ordinary Studio selection and
the complete saved/generated consumer's remaining checks.

The saved/generated JSON Workshop now exercises this choice through ordinary
native Inspector/history/Save/Open and its own native/browser applications.
Visual review distinguishes the fitted Notebook from injected Tool Canvas
operation cards. Their uniform projection now fixes the extra overlap caused by
fixed-size faces following zoomed anchors. Existing authored overlap remains an
honest navigation constraint. Full browser Studio input and observing delivery
remain open; WORK.md separates focused adapter checks from the complete saved
application, visual review and original quality requirements.

Only uniform definitions emit descriptor version6. Its camera may be bounded or
unbounded and precision remains optional. Switching back to an existing policy
restores its applicable older encoding. Crafted Pascal emits
`.WithScaling(nccsUniform)` through the existing fluent contract. Current-source
semantic scene operations describe version6 as well; this does not update an
already running Studio server. WORK.md distinguishes compilation, actual browser
controls, native implementation checkpoints and observing deployment.

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

### Capturing live immutable plans

`CaptureNyxDrawingPlan` retains an admitted built-in display list directly.
Its immutable command array, view box and complete byte/sample limits were
established by Build. Continued builder authoring produces independent plans.
Creator-supplied implementations cross their complete strict wire boundary once;
live getters and a forwarded native object-instance query cannot establish the
private plan's interface identity. The admitted result retains no creator facade.
Nil or malformed proposals refuse without replacing an earlier output.

Both Drawing adapters and provider resolution use this boundary before replacing
their accepted plan. Existing mapping/allocation checks, theme ownership and
failure/Resize behavior remain. Graph projection uses the corresponding
[`CaptureNyxDiagram`](diagrams.md) boundary, including whole byte
admission for fresh edits. Sharing these immutable values introduces no document,
application, view or scheduler back edge, and changes no persistence schema.

### Reading typed live commands

`NyxDrawingCommands(Plan)` returns an `INyxDrawingCommands` reader of immutable
`TNyxDrawingCommand` values. `Kind` selects `ndckPath` or `ndckText`; path commands
expose `Path`/`Style`, and text commands expose `Text`, `Position`, `Paint`, `Size`,
`Anchor` and `Bold`. Reading a property for the wrong kind refuses. The default
command is `ndckAbsent` and cannot be serialized.

Paths expose `StepCount` and bounded `Step(Index)` values. `TNyxDrawingStep.Kind`
selects Move, Line, Curve or Close. `Point(0)` is Move/Line's coordinate; Curve
uses zero/one for its controls and two for its end. Close has no point. Absent
steps and invalid indices refuse. Extending a path derives an independent
sequence; retained commands and prior plans keep their original geometry.

```pascal
LCommands := NyxDrawingCommands(LConnectionDrawing.Plan);
LCommand := LCommands.TypedCommand(0);

if LCommand.Kind = ndckPath then
begin
  LStart := LCommand.Path.Step(0).Point(0);
end;
```

The reader first captures the complete plan through the existing admission
boundary. A creator's advertised typed interface cannot bypass its wire decoder
or retain a mutable facade. Both renderers, shared cubic sampling and mapping use
these typed values directly. Legacy `Command(Index)` and explicit `ToData` retain
the same version-one wire and strict decoder; they remain interchange operations.

Build still admits the complete command, point, sample and UTF-8 budgets. Small
live plans use a conservative bound derived from the closed wire vocabulary,
bounded numeric formatter and at most twelve encoded bytes per Unicode scalar.
If that bound exceeds 1 MiB, exact serialization decides admission, allowing
large valid plans. Checked builds independently compare the bound with complete
wire bytes. Every explicit `ToData` also enforces the exact byte boundary. No
encoded snapshot, mutable reader or application owner is cached by this path.

### Retaining saved fallback plans

The saved fallback of an ordinary Drawing is decoded separately from a live
provider result. Both renderer bindings now own an `INyxDrawingSource`, created
with `NewNyxDrawingSource`. Its `Read(Node)` borrows the descriptor only for that
call and retains its exact source text and admitted immutable plan. Matching
source bytes, extension presence and JSON escaping policy reuse that fallback.
Changed or removed extensions cross the original strict decoder again; failure
leaves the previous retained source/plan intact. Distinct bindings own distinct
readers, and teardown releases them before the borrowed model ends. This cache
retains no node, provider, theme, application or target handle.

Provider resolution, current allocation/mapping, theme and accessible metadata
still run on every synchronization. A valid prepared fallback does not publish a
physical face or accept a provider result. Provider failures therefore preserve
the same accepted native plan/SVG, and retries still invoke the provider. The
reader includes the caller's encoding policy in admission: a large descriptor
that fits plainly can still refuse under stricter normalized slash escaping.
Changing the global formatter during a read requires caller synchronization.

### Native clipped paths

The LCL painter tests each path's mapped control hull against the actual device
context clip before allocating sampled/native points. Cubic controls belong to
the hull, so a curve with offscreen endpoints can still enter the visible area.
The bound includes rounding slack and the current native stroke width. Win32
miter joins use the caller's actual limit; unavailable/failed limit or clip
queries keep the complete drawing path. No clip, pen join policy or model is
replaced. The underlying contracts are
[GetClipBox](https://learn.microsoft.com/en-us/windows/win32/api/wingdi/nf-wingdi-getclipbox)
and [GetMiterLimit/SetMiterLimit](https://learn.microsoft.com/en-us/windows/win32/api/wingdi/nf-wingdi-setmiterlimit).

Complete plan admission, color/metric refusal and final public pen/brush/font
state remain required even for clipped commands. Native glyph clipping remains
with the font renderer. The browser SVG implementation is unchanged; this native
optimization supplies no browser execution or visual-parity claim.

`tests/nyx_drawing_clip_tests.lpr` compares every pixel of complete in-memory
reference crops against direct and tiled paints across three zooms, three join
styles, a nondefault miter limit, partial/one-pixel/empty clips, entering curves,
wide strokes, enclosing fills and Unicode text. The unchanged fixture passes
226 checks in the checked baseline and checked/optimized native candidate,
including paint-state and invalid offscreen-metric refusal. It shows no
host and saves no raster or screenshot; it is a programmatic painter check,
separate from hardware, accessibility, observing-editor or visual-quality proof.
The maintained `tools/build.ps1 -Target canvas-drawing` includes this native
regression before its existing control/input journey.

### Native viewport buffers

Win32/LCL drawings, logical scrolling hosts and canvas item panels keep their
normal `DoubleBuffered` flag and inheritance. Their native paint scope allocates
only the bounding rectangle of the HWND's visible dirty region. The final copy
uses the original display DC's exact clip, including child-window holes. Parent
backgrounds, normal `OnPaint`/`WindowProc` callbacks and caller paint state still
belong to LCL; the portable drawing plan and camera geometry do not change.

`nyx.paint.buffer.lcl` supplies `AttachNyxLCLPaintBuffer`, its revocable
`INyxLCLPaintBuffer` and value-only `TNyxLCLPaintBufferStats`. The public native
`TNyxLCLBufferedPanel` manages that scope for ordinary panels. These are target
adapter APIs, not portable authoring properties. They borrow an existing LCL
window on its UI thread, retain no document/renderer and revoke control access
before handle teardown. A private installed lease and callback-local lease keep
the opaque receiver alive until its exact native subclass entry is retired.
Unsupported widgetsets or missing native API exports retain ordinary buffering.

Windows does not reference-count subclass callback data; the adapter therefore
owns those leases explicitly and removes only its callback/ID pair through the
[subclass API](https://learn.microsoft.com/en-us/windows/win32/api/commctrl/nf-commctrl-setwindowsubclass).
Its [BeginPaint](https://learn.microsoft.com/en-us/windows/win32/api/winuser/nf-winuser-beginpaint)
scope preserves the visible/update clip and balances every native DC/bitmap.
Existing supplied-DC/printing, nonclient, tab-page, RTL and nested-paint paths
remain delegated to LCL. Allocation refusal uses the existing direct paint path;
retirement prevents a pending buffer from publishing. A custom eraser can decline
drawing without exposing uninitialized pixels: the owned rectangle begins with
the same standard cache color as LCL. Caller-disabled buffering stays disabled.

One accepted native comparison changes 256/512 forced repaint from 61.42 to
15.94 ms at width 800 and 58.46 to 13.41 ms at width 390. Publication remains
39.43/39.19 ms. These measure the existing native paint calls, not compositor
presentation or hardware input. Native buffer/input, fixed numeric raster and
source/history evidence remain separate from browser compilation and the open
visual/accessibility, full frame/memory/production budgets and delivery gates.
WORK.md retains the discarded style hypothesis, loader/metadata repairs and
selective final qualification instead of claiming an uninterrupted first pass.

### Focusable Canvas names

`NewNyxCanvas(...).Configure.AccessibleName(...)` names the actual focusable canvas
surface as well as its outer layout control. Effective state/resource/computed
names follow ordinary renderer synchronization; the surface retains its controls
and focus. An absent name keeps `NyxDefaultCanvasAccessibleName`. This adds no
property or persistence flag to the authoring contract.

Keyboard help is a description rather than part of the short name. Native LCL
uses its description property; browser markup references an owned hidden
description with `aria-describedby`, following the
[W3C names/descriptions guidance](https://www.w3.org/WAI/ARIA/apg/practices/names-and-descriptions/).
Transient description IDs avoid existing caller DOM and independent views.
Caller hints take precedence over default help and retain their ordinary tooltip.
The shared default help remains available as `NyxCanvasKeyboardHelp`.

Optional `INyxCanvasAccessibilityView` copies runtime metadata to connected
surfaces on the UI thread. It grants no document, store or history authority;
the renderer's next synchronization owns the effective values. Existing view
GUIDs/vtables remain intact, and alternate implementations can expose the same
portable bridge without DOM/LCL types. Worker or revoked-view calls refuse.

`tools/nyx_canvas_accessibility_review.lpr` reports bounded actual target property
values. It checks typed/static names, state updates, Unicode, replacement, default
help, independent source and retained focus at widths 800/390. Exit 2 reports
product gaps; exit 1 reports a broken review. Native property reads and compiled
DOM consumers do not establish accessibility-tree, screen-reader, hardware or full
browser/native parity. WORK.md owns the exact executed/compiled evidence.

Native Win32 names/descriptions also require Windows provider publication;
LCL properties alone do not establish that clients receive them.
[`nyx.accessibility.lcl`](../src/nyx.accessibility.lcl.pas) supplies
`SyncNyxLCLAccessibility` for ordinary windowed controls and the actual canvas
surface. The renderer opts its controls in automatically. A control-owned bridge
copies its existing metadata through Microsoft's
[dynamic annotation API](https://learn.microsoft.com/en-us/windows/win32/winauto/dynamic-annotation-api).
It preserves the standard provider rather than replacing its value, focus or
action implementation. Empty fields restore that provider's defaults.

The helper returns typed publication/deferred/unavailable/unsupported status;
it requires the UI thread and never creates a handle solely for metadata.
Its own UI-thread Windows message observer refreshes lazy/recreated HWNDs and
clears owned annotations during destruction. It changes no LCL window procedure,
observes no other application/thread, retains no model/renderer/callback and
balances its native COM lifetime. A private HWND ownership receipt prevents
cleanup against a foreign/recycled handle. Other widgetsets/non-windowed controls
keep ordinary behavior. The zero-terminated native annotation API reports
embedded NUL as unavailable while preserving caller metadata; this is not a
restriction on the portable text contract. Broader native providers remain open.

`tests/nyx_accessibility_lcl_controls.lpr` uses actual direct `IAccessible` calls
and independent client processes. On the qualified Windows path, automation
dispatch getters bypass these annotations; the maintained test therefore uses
the native interface and an explicit `VT_I4` child identifier. It checks names/
descriptions, supplementary text/live binding, lazy/recreated handles, worker
refusal, retained native edit state, standard button action and retired clients.
This still does not qualify UI Automation trees, screen readers, all custom
roles/actions, non-windowed descendants, other widgetsets or complete parity.
The existing canvas-workspace runner includes this Windows/widgetset-gated
consumer. WORK.md separates its exact execution evidence from compiler checks,
full quality/budgets and delivery.

### Native button roles and activation

Canvas content and ordinary views share `TNyxLCLButton`. On Win32, that control
now exposes the system push-button role and default action through
[`nyx.accessibility.button.lcl`](../src/nyx.accessibility.button.lcl.pas).
The adapter uses Microsoft's
[system control proxy](https://learn.microsoft.com/en-us/windows/win32/api/oleacc/nf-oleacc-createstdaccessibleproxyw)
with the BUTTON class and implements its native press/focus/activation messages.
Windows owns the returned accessibility objects; they retain no Pascal control,
renderer, document or application callback. Every client request receives a fresh
[marshaling result](https://learn.microsoft.com/en-us/windows/win32/api/oleacc/nf-oleacc-lresultfromobject)
using its original request parameters. Authored names/descriptions continue through
the separate annotation adapter above.

The public native attachment accepts an existing button-like `TWinControl`, its
own copied-state reader and its `ExecuteDefaultAction` implementation. It requires
the UI thread and an allocated HWND; it allocates no window itself. The standard
Nyx button attaches after window creation and retires before window destruction,
including handle recreation. Alternate native control classes can use the same
contract. Missing optional native capabilities or another widgetset retain their
ordinary provider; this helper does not implement a general cross-widgetset
accessibility tree.

Managed activation uses the view's existing revocable event source and physical
input frame. A dormant candidate or disconnected view cannot run application
behavior. Synchronization refuses activation before creator callbacks; design
preview selects the control without running application callbacks. An admitted
callback can unmount its own view: the existing frame retains its control through
the callback, revokes further activation and disposes it afterward. Native factory
controls must be fresh, unparented and owned by their supplied host so the renderer
can connect this context before parenting.

Retirement clears borrowed receivers before native detachment. COM cleanup waits
for active native callbacks and runs on the UI thread; later release of an inert
interface on a worker performs no COM cleanup there. A failing application
exception reporter is contained at the native callback boundary.
`tests/nyx_accessible_button_controls.lpr` exercises actual direct/automation action
calls, an independent client process, disabled/pressed state, consumed keyboard
input, producer admission/revocation, synchronization, preview and self-unmount.
`tests/nyx_accessible_button_lifetime.lpr` additionally uses Windows' real COM
initialization observer with an alternate LCL control and worker release.
The widgetset-gated canvas-workspace runner includes both consumers. WORK.md owns
their execution and retained failure receipts. Full UIA/MSAA hierarchy, other
custom controls/non-windowed children, screen readers, hardware, other widgetsets,
browser runtime, quality/budgets and delivery remain separate requirements.

### Retaining native sibling order

Canvas publication keeps visible item hosts in scene paint order, followed by
visible rulers. On Win32, an already correct stack requires no restacking calls.
The target adapter reads both the current LCL child list and actual HWND siblings
on every presentation. It does not cache a previous order: handle creation,
ordinary caller reordering and native-only reordering can change either list.
A changed stack uses the original full LCL `BringToFront` sequence. Moving each
child to an indexed position is not an equivalent native reconciliation.

`nyx.order.lcl.NyxLCLChildSuffixCurrent` reads a borrowed back-to-front suffix on
the UI thread. It creates no handle, changes no control and retains no owner.
Missing handles or unsupported widgetsets return False so callers keep ordinary
LCL reconciliation. Existing clipping, offscreen focused editors, scene revisions
and coordinated publication retain their owners. This native optimization adds
no portable property, source syntax, persisted flag or browser behavior.

The maintained Win32 stacking consumer checks actual native order as well as LCL,
calibrates its native position-message spy, and verifies stable refreshes produce
no position notifications. It also exercises caller/native-only reorder, scene
order, hidden controls, both/single/no rulers and exact memo text/caret/focus.
WORK.md owns fixed-workload timing, ordinary editor/Studio and retirement evidence
separately from full frame/memory/quality, browser execution and delivery.

### Measuring portable projection

Typed builder paths/styles already own normalized values. Command admission
checks every point against the current view box and charges the same cubic-sample
budget directly, without decoding those private values again from serialized
commands. The strict wire decoders, atomic capacity refusal, detached Build
snapshots and final UTF-8 byte limit remain authoritative.

`tools/nyx_diagram_benchmark.lpr` measures graph capture, admission, routes,
projection and complete move-plus-projection at 16/64/256 vertices with twice as
many connections. Each workload includes three outlines, named ports, parallel
connections, orthogonal self-loops, captions, arrows and selection grips. Build
with the repository FPC in Delphi mode, `-O2`, `-Fusrc` and `-Futools`, directing
`-FU`/`-FE` into owned output directories. No argument emits three warmed samples
and their median per phase; native `--snapshot` emits complete graph/routes/drawing/moved output for
exact comparisons. Setup, assertions and output encoding are outside timing.
Win32 uses its high-resolution performance counter; other native hosts currently
use the coarser RTL tick clock. Browser compilation is separate from execution.

The preceding builder-only matched native release pair measured move/projection medians of
94.59/349.04/1434.44 ms before, and 59.78/225.84/917.15 ms after this change, at
those three sizes. Complete result bytes remain exact. These are local portable
CPU samples, excluding target rendering, painting, physical input and peak memory.
The provisional engineering target is at most 16 ms for the 256/512 portable
move-plus-projection phase on each qualification host. Current source fails that
target; the remaining wire materialization and target costs need their own work.
This bounded fixture does not qualify larger diagrams, full production budgets,
browser performance, visual parity or delivery. WORK.md owns the exact receipts.

With live immutable capture, a fresh matched native release pair measures
projection medians of 32.33/120.28/487.26 ms at those sizes, against
58.63/223.41/912.81 ms before capture sharing. Fresh move/projection measures
45.80/167.74/668.60 ms against 59.88/226.30/913.91 ms. The tool's `capture` phase
still measures complete wire encoding; that setup also admits the original
graph before repeated projection. Each move creates a fresh graph and includes
its first canonical byte check. Whole graph/routes/drawing/moved bytes remain
exact. This single warmed release pair excludes actual rendering and does not
meet the provisional phase target. Browser and production budgets remain open.

With typed live commands, a further matched native release pair gives the
following medians in milliseconds, preserving the complete previous output:

| Vertices / edges | Projection before | Projection after | Move + projection before | Move + projection after |
| --- | ---: | ---: | ---: | ---: |
| 16 / 32 | 33.67 | 0.82 | 46.08 | 11.70 |
| 64 / 128 | 123.49 | 3.63 | 170.47 | 46.87 |
| 256 / 512 | 499.19 | 19.82 | 693.91 | 195.57 |

Fresh graph admission is now the larger remaining portable cost. The full
256/512 update still exceeds the provisional target. This single local pair
excludes actual painting/input and peak memory, and does not establish browser
execution, alternating production measurements or full performance acceptance.

Whole typed graph admission then changes fresh move/projection medians from
12.05/46.61/191.50 to 1.19/11.17/28.00 ms at those three sizes, with every
complete result still exact. Repeated projection is effectively unchanged
(19.77 to 19.45 ms at 256/512); explicit wire encoding remains a separate phase.
The full 256/512 update still exceeds the provisional target. This further
single local release pair does not qualify target painting, browser execution,
frame/input/memory or full production budgets. See the
[complete graph admission contract](diagrams.md) and WORK.md for scope/receipts.

With private exact-name indices and one route preparation per projection, a
fresh matched native pair changes move/projection medians from 1.07/4.90/26.85
to 0.84/3.29/13.51 ms at 16/64/256 vertices and twice as many edges. Projection
changes 0.82/3.57/18.99 to 0.55/2.12/8.41 ms; complete outputs stay byte-exact.
The largest local portable sample is below the provisional phase target. This
single pair does not establish production, actual mounted target/frame/input,
peak/live-memory or browser budgets. Those remain separate required outcomes.

### Measuring the mounted native editor

`tools/nyx_diagram_lcl_benchmark.lpr` mounts the public Nyx diagram editor with
the same complete graph workload, shared through `nyx.benchmark.diagram`.
One reusable includes a bound memo and button. Each run edits that memo and
retains its exact control, text, caret and runtime value while applying complete
geometry changes and dispatching the actual installed native Right-key hook.
Every update checks accepted revision/geometry and the unchanged caller document.
It uses no alternate UI toolkit, host service, recovery store or image capture.

Build with the configured LCL compiler, Delphi mode, `-O2`, `-Fusrc`, `-Futools`
and ordinary Lazarus unit paths; direct `-FU`/`-FE` into owned output directories.
Run one fresh process with a vertex count of 16, 64 or 256, followed by either
`800 600` or `390 640`. One warm iteration precedes three reported samples.
Geometry publication, explicit forwarded `WM_PAINT` work and key dispatch have
separate timers. Forced repaint measures a native paint call, not compositor
presentation, a delivered animation frame or trusted hardware latency. Allocator
before/after/disconnect samples and process high-water include setup, validation
and runtime caches; they are not OS private bytes or an isolated per-view peak.
Checked heap tracing qualifies complete retirement separately from timings.

One unchanged baseline/final native release pair reduces 256/512 synchronous
publication from 1030.21 to 710.20 ms at width 800 and 1039.87 to 718.63 ms at
width 390. Native key dispatch changes 1034.69 to 699.85 ms and 1033.92 to
703.88 ms respectively. Forced repaint remains approximately 84 ms. Complete
portable output stays exact and the bound memo/caret survive, but these mounted
costs remain too high for smooth manipulation. The process high-water remains
10,093,792 allocator bytes in both builds. WORK.md owns full receipts and retained
fixture failures. Browser execution, full frame/input/memory budgets, visual/
accessibility parity and delivery remain required; native results imply none of
those outcomes.

Per-binding saved fallback admission then reduces a fresh matched native pair's
256/512 publication from 699.94 to 362.12 ms at width 800 and 699.99 to 363.69 ms
at width 390. The exact same workload/harness retains the bound memo/caret/state
and complete portable output. Largest key dispatch changes 700.95 to 359.59 ms
and 699.48 to 361.90 ms; forced painting remains approximately 83 ms and process
allocator high-water remains 10,093,792 bytes. Those are native forced paint and
installed-hook costs, not frame presentation or trusted input. The reductions
still leave mounted costs too high for smooth editing. WORK.md owns exact
receipts, failures and separate compiled-only browser evidence; full target
quality/input/memory/production budgets and delivery stay open.

Conservative native path clipping then changes forced repaint medians in one
fixed matched release pair from 11.47/23.56/83.37 to 10.36/20.02/60.47 ms at
16/64/256 vertices (twice as many edges) and width 800. At width 390 they change
9.43/23.91/82.13 to 8.76/17.59/57.94 ms. The same complete workload/memo/caret/
state and 1,364,909-byte portable result remain exact; allocator high-water is
10,093,816 bytes, 24 above its baseline. Synchronous publication remains about 356/358 ms at the largest
size. These forced native paint samples still exceed smooth-editing budgets and
are not compositor/frame presentation or trusted hardware latency. WORK.md owns
the exact receipts and separate input/lifetime/compiler qualifications.

Sharing admitted graph storage during document content validation then reduces
a fresh matched pair's largest 256/512 publication from 356.96 to 39.12 ms at
width 800, and 358.90 to 38.06 ms at width 390. Installed-key dispatch changes
353.81 to 34.03 ms and 353.27 to 34.94 ms respectively. Resource/locale/reusable
dependencies still resolve afresh; complete portable output and the live memo,
caret and state remain exact. Forced painting stays around 60 ms; process
allocator high-water changes from 10,093,816 to 10,093,848 bytes. Publication and
painting still exceed smooth-interaction budgets. These native measurements
establish no browser execution, compositor/hardware latency, isolated view peak,
accessibility/visual parity or delivery. WORK.md owns the scoped evidence.

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
