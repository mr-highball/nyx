# Visual diagrams

[Diagram editor owner](../TODO/NS-3_diagram-editor_01.md) ·
[Canvas](canvas.md) · [Typed behavior graphs](logic.md)

The diagram layer is independently authored Pascal. `nyx.diagram` owns typed,
immutable visual graphs; `nyx.diagram.drawing` projects them through ordinary
Nyx Drawing controls. This is preparation for the composed editor, whose complete
canvas/program/flow prerequisites remain open. It does not add a registered
control kind, Studio editor, semantic tool or draw.io file reader.

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
named ports, rotation and group transforms remain required.

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

There are at most 256 shapes and 512 edges. Shape extents are 8..1,000,000 world
units inside +/-1e9. Captions admit 256 Unicode scalars and refuse controls/newlines.
These are admission limits, not production performance measurements. The strict
version-one `ToData` / `NyxDiagramFromData` boundary admits at most 1 MiB UTF-8,
exact closed fields, native numeric values and integer enum spellings. Duplicate
members and unknown/dangling identities refuse. A large in-memory authored plan
can reach the persistence byte refusal before membership limits; applications must
admit its complete descriptor/projection before publishing it into saved work.
There is no hidden mutable JSON tree, document, renderer or reference cycle.

`DrawNyxDiagram` captures and validates the whole descriptor once. It paints edges
and independent open arrowheads, then shapes and one-line themed captions, using
the existing drawing budgets. The 64-world-unit frame includes stroke/decorations.
Long captions are not automatically wrapped or constrained to their shape.
Drawing's contain mapping scales the entire picture, including text. A narrow
fit is a thumbnail, not a qualified readable editor viewport. Interactive zoom,
selection, handles and compact panels belong to the composed editor.

Persist the visual graph separately when editable intent is needed. Plain Drawing
clone, codec and generated Pascal retain its display list only. They do not
recover the graph from pixels/paths. Updating the authored Drawing after an
ordinary Render does not rewrite that renderer's independent runtime projection.
The maintained consumer uses the public `RetainNyxControl` /
`SetNyxDrawingPlan` bridge on the runtime projection, then Sync. It preserves the
authored document. This is not Studio source publication or paired Undo/Redo.

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
| Shape and connector libraries | Three mathematical shapes and two route policies; palettes, custom recipes/libraries remain open | [Diagram](../TODO/NS-3_diagram-editor_01.md) |
| Text and formatting | Single-line Unicode captions, typed fills/strokes/theme roles; editing, wrapping, fonts and connector labels remain open | [Diagram](../TODO/NS-3_diagram-editor_01.md) |
| Selection/marquee | Pure shape/segment picking; selection model, marquee and handles remain open | [Diagram](../TODO/NS-3_diagram-editor_01.md) |
| Move/resize/rotate | Typed bounds reroute; native workshop buttons exercise Move/Resize. Direct manipulation/rotation remain open | [Diagram](../TODO/NS-3_diagram-editor_01.md) |
| Alignment/distribution, snapping/guides | Full diagram editing behavior remains open | [Diagram](../TODO/NS-3_diagram-editor_01.md), [Canvas](../TODO/NS-3_canvas-workspace_01.md) |
| Groups, containers, swimlanes | Full diagram membership/transforms remain open | [Diagram](../TODO/NS-3_diagram-editor_01.md) |
| Layers, order, locking/visibility | Scene layers exist; graph-to-layer semantics and editor controls remain open | [Diagram](../TODO/NS-3_diagram-editor_01.md) |
| Multiple pages | Document pages exist; independent diagram page lifecycle remains open | [Diagram](../TODO/NS-3_diagram-editor_01.md) |
| Clipboard and duplicate | Independent snapshots exist; semantic selection/clipboard operations remain open | [Diagram](../TODO/NS-3_diagram-editor_01.md) |
| Undo/Redo | Immutable predecessors exist; ordinary paired source/history integration remains open | [Diagram](../TODO/NS-3_diagram-editor_01.md), [Workflow](../TODO/NS-4_agent-workflows_01.md) |
| Search | Exact typed reference lookup exists; text/library search remains open | [Diagram](../TODO/NS-3_diagram-editor_01.md) |
| Keyboard/touch/pen | Nyx workshop buttons dispatch native events; direct diagram input, focus and accessible alternatives remain open | [Diagram](../TODO/NS-3_diagram-editor_01.md), [Parity](../TODO/NS-2_parity-accessibility_01.md) |
| Connectivity | Shape/free endpoints, self-loops, parallel edges and independent arrows exist; named ports, editable waypoints, curves, obstacle routing and transformed/grouped endpoints remain open | [Diagram](../TODO/NS-3_diagram-editor_01.md) |
| Embedded controls, graphics/resources and programs | Existing canvas facilities remain; diagram-level content/ports/recipes/program integration remain open | [Diagram](../TODO/NS-3_diagram-editor_01.md), [Canvas](../TODO/NS-3_canvas-workspace_01.md) |
| Palettes, Inspector, layers/pages, minimap and adaptive panels | Full reusable Nyx editor recipe remains open; current workshop is a qualification fixture | [Diagram](../TODO/NS-3_diagram-editor_01.md) |
| Source, serialization and bounded MCP | Own strict visual codec and picture projection exist; editable document declarations, crafted generation, bounded semantic graph operations and paired admission remain open | [Diagram](../TODO/NS-3_diagram-editor_01.md), [Codegen](../TODO/NS-1_codegen_01.md), [Workflow](../TODO/NS-4_agent-workflows_01.md) |
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
