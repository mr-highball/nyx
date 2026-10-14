# Canvas workspaces

[Architecture](architecture.md) · [Canvas task](../TODO/NS-3_canvas-workspace_01.md) ·
[Diagram editor](../TODO/NS-3_diagram-editor_01.md) · [Current evidence](../WORK.md)

Nyx's requested canvas is a general 2D workspace. Its scene will accept ordinary
Nyx controls, reusable compound subtrees, vector content, text and graphics.
A diagram editor extends that same workspace with vertices, ports and connectors.
The existing Studio design surface does not yet provide this contract.

## Implemented coordinate foundation

`nyx.canvas.geometry` supplies independent immutable records. It imports no
browser or LCL types and holds no document, control, renderer or callback.
World positions are signed; viewport dimensions and gesture deltas use logical
pixels. Device-pixel ratio belongs to the rendering adapter.

```pascal
var
  LCamera: TNyxCanvasCamera;
  LGrid: TNyxCanvasGrid;
  LPointer: TNyxCanvasPoint;
  LWorld: TNyxCanvasPoint;
begin
  LCamera := NyxCanvasCamera(1200, 800, NyxCanvasPoint(0, 0));
  LGrid := NyxCanvasGrid(20, ncgsDots, ncsmGrid);
  LPointer := NyxCanvasPoint(320, 240);
  LWorld := LCamera.ViewportToWorld(LPointer);

  // Zoom around the pointer without moving the world point beneath it.
  LCamera := LCamera.ZoomAt(2, LPointer);

  // A hand-tool drag moves content in the supplied screen direction.
  LCamera := LCamera.PanBy(80, -20);
  LWorld := LGrid.Snap(LWorld);

  // The view may hide the grid while retaining its placement policy.
  LGrid := LGrid.Styled(ncgsNone);
end;
```

The camera maps its world center to the viewport center. `WithViewport` preserves
that center and zoom. `ZoomAt` keeps an anchor stable even when its requested
zoom clamps. `Pinch` combines scaling with midpoint translation; finger rotation
does not implicitly rotate this camera. Coincident contacts refuse, allowing the
input adapter to explicitly choose a pan. `Fit` accepts point and line bounds
and respects caller zoom limits, which may prevent a complete fit.

Grid style and snapping are independent enum choices. `VisibleSpacing` increases
the display interval by powers of two at low zoom without changing the authored
snap spacing. Snap ties round away from the grid origin, including negative
positions. Actual drawing will enumerate only the visible grid region.

“Infinite” means there are no page edges or giant backing widget dimensions.
Finite arithmetic still has explicit precision limits: coordinates admit up to
±1e12 world/logical units; application zoom limits lie inside 1e-6 through 1e6.
The default range is 0.05 through 16 and callers may override it. Invalid,
nonfinite, missing or out-of-range geometry raises `ENyxCanvasGeometry` before
publication. The whole visible world is admitted with the camera. Very distant
off-screen projections can refuse, so future adapters must cull in world space
before converting coordinates. Exact grid indices also have a portable budget.

These primitives are implemented and have checked native/executed-browser
evidence. They do not yet render a workspace or receive hardware gestures.
`tools/build.ps1 -Target canvas-geometry -CanvasGeometryEvidenceDirectory <output>`
runs the checked native fixture and stages its matched browser counterpart.
HTTP execution remains separate; the current packet passes 1300 on each target.

## Embedding Nyx controls

The required scene model must make existing and future Nyx controls embeddable
without defining a second widget toolkit. Typed item placement will associate an
owned control subtree with stable scene identity, world bounds, layer, scale and
interaction policy. The document owns that subtree; mounted views own revocable
adapter handles and never hold an ownership cycle back into the tree.

A hosted Memo must retain its text editing, selection, focus, bindings and events;
a compound control must retain its named parts and instance state. Surface panning
and item manipulation must arbitrate with those controls' input. Painting an image
of a native control is insufficient to establish interactive parity.

Native widget scaling/clipping is a real adapter concern. Browser transforms alone
cannot establish the LCL contract. World-scaled and screen-sized content policies
need explicit typed meanings and actual widgetset qualification; unsupported
transforms must report capabilities rather than quietly alter interaction.
No embedded-control renderer or qualified transformation policy exists yet.

The user's extension requires optional input/output port chrome on those same
embedded controls. Event outputs carry typed payloads; data outputs carry typed
values; inputs expose supported value/property/action contracts. Scripts declare
their own typed interfaces. Static Pascal behavior remains usable when wiring is
disabled. Hiding ports and disabling graph execution are independent choices.

The [behavior-graph owner](../TODO/NS-1_canvas-flow_01.md) retains the complete
implementation: drag-to-connect, type compatibility, explicit conversions,
multi-registration order/fan-out, scheduler policy, cycle handling, diagnostics,
revocable subscriptions and paired semantic history. Drawing a visual connector
alone never authorizes execution. There is no implemented graph runtime yet.

The requested [Tool Canvas](../TODO/NS-3_tool-canvas_01.md) is a second specialized
compound workspace, independent of the draw.io-style editor. It focuses on
interactive tool construction: control/script palettes, port discovery, wired
behavior, source editing, diagnostics and run/pause feedback. It shares the scene,
resource and behavior layers and uses ordinary Nyx controls throughout. This
starter recipe is required but not yet implemented.

Tool Canvas also needs its own [Logic toolbox collection](../TODO/NS-3_logic-components_01.md).
These are nonvisual runtime components with visible design cards and typed ports.
The initial families cover files/dialogs, JSON, collections/queries, text/regex,
network requests, data mapping and flow. Help, creator tags and grouping use the
same discovery experience as visual controls. Direct Pascal callers use the same
implementations without mounting the editor or creating a graph.

Each operation distinguishes data from invocation, completion, failure/cancel and
progress. Useful starter chains include HTTP → JSON → sort/map → Table, or a
button event → JSON formatting → file save. Asynchronous work needs owned run
identity, explicit scheduler publication and revocable outputs. These components
remain required future work; the camera/expression foundation does not implement
that toolbox or establish actual file/network outcomes.

## Remaining layers

The canvas owner retains themed rendering, rulers/guides, scene ownership and
spatial queries/culling, resource binding, gesture arbitration, accessible input,
portable scene persistence/source generation, Studio/MCP authoring and delivery.
Embedded/hosted images use Nyx's existing resources and configurable cache policies.

The [behavior-program owner](../TODO/NS-1_canvas-behaviors_01.md) defines an optional
lightweight typed Delphi-dialect interpreter. It needs an explicit grammar, host
capabilities, execution budgets and atomic proposed changes. The existing source
reader and expression evaluator are infrastructure, not a complete interpreter.

The diagram editor owns graph connectivity: fixed/floating ports and endpoints,
straight, orthogonal and curved routes, arrowheads, waypoints and labels, including
node movement, grouping, rotation and deletion. Its reference matrix covers the
broader [draw.io feature set](https://www.drawio.com/docs/features/),
[editor/manual behavior](https://www.drawio.com/docs/manual/) and
[layers](https://www.drawio.com/docs/manual/layers/), checked 2026-10-13.
No 1:1 behavior or format compatibility is claimed from the foundation.

Semantic inspection must return bounded local scene/connection context. Related
agent mutations use expected revisions and one undoable operation, with visible
activity in Studio. The installed MCP release has no canvas authoring operation;
that gap remains with the existing workflow owner until implemented and delivered.
