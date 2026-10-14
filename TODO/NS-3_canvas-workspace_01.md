# NS-3_canvas-workspace_01 — Infinite canvas and injected workspace controls

[North stars](../MILESTONES.md) · [Task catalog](README.md) · [Task flow](../TASKFLOW.MD)

**Description:**

The user's 2026-10-13 request adds a general, beautiful 2D workspace control,
not merely another layout container or Studio-only drawing surface. NS-3 owns
the specialized managed control and extension contract. Existing gesture,
theme, resource, source/history, renderer and semantic-workflow owners consume
the result without duplicate credit. This genuinely extends the declared
product destination: the historical 76-kind catalog is not evidence of this
control. No completion allocation or aggregate increase is inferred from planning.

Separate independently deliverable consumers retain their complete scope:
[Pascal behavior programs](NS-1_canvas-behaviors_01.md) and the
[draw.io-style diagram editor](NS-3_diagram-editor_01.md).
The follow-up requires [typed behavior ports/graphs](NS-1_canvas-flow_01.md)
on embedded controls and scripts, with independently toggleable port chrome.
The [Tool Canvas](NS-3_tool-canvas_01.md) is a separate composed starter for that
intent, sharing this foundation without depending on complete diagram parity.
Finish the current computed-binding integration before the complete canvas batch;
independent coordinate primitives may be prepared without replacing that return path.

**Acceptance Criteria:**

- Public specialized reference-counted workspace/control/configuration interfaces
  support signed world coordinates, immutable camera snapshots, anchored zoom,
  pan, fit, viewport/world conversion, optional grid/dots/lines, independent
  snapping, rulers/guides and caller-configurable bounds. No DOM/LCL types enter
  the contract. Finite precision/limits and invalid/degenerate inputs are explicit.
- Browser and LCL adapters render the same owned scene with retained injected
  Nyx controls and vector/text/image content. Theme tokens govern surfaces,
  grid, guides, selection and handles. Embedded and hosted images use existing
  typed resource references, loading/cache policy, cancellation and diagnostics.
  Existing and future Nyx control kinds are first-class embeddable content, not
  a separate canvas widget catalog. Typed placements host a button, memo, chart
  or compound subtree with normal state, events, resource bindings and named
  parts. World transforms/clipping, focus, keyboard editing and gesture arbitration
  preserve control behavior on both targets. Explicit scale/interaction policies
  must qualify native controls that cannot accept arbitrary affine transforms;
  frozen images do not count as interactive-control parity.
- Mouse, touch, pen, wheel, trackpad and keyboard navigation share typed gestures
  and policies. Anchored pinch/zoom, captured pan, cancellation, pointer loss,
  focus, nested input editing and accessible non-gesture alternatives work.
  Host synthesis, real hardware and supported widgetsets are qualified separately.
- Independent documents/views, owned scene items, stable item/layer references,
  z-order, clipping, spatial queries/culling and extensible custom paint/content
  providers preserve lifetimes. A view never retains a cycle into the design tree.
  Caller content injection is not limited to an enumerated shape catalog.
- Versioned persistence, deterministic crafted Pascal, source admission and
  reusable instance overrides retain exact scene/camera policy and resources.
  Studio consumes public Nyx controls for authoring, and semantic MCP exposes
  bounded scene/selection/viewport queries plus revision-aware grouped undoable
  mutations. Pending drafts and stale candidates retain the active pair.
- Desktop/narrow English demos, actual browser/LCL input and rendered previews
  qualify visual quality, adaptive workspace controls and interaction. Measure
  large/off-screen scenes, frame/update costs, resource lifetimes and retirement;
  specify performance budgets and qualify observing delivery separately.

**Blockers**

- [Managed component interfaces](DONE/NS-1_component-interfaces_01.md).
- [Owned document model](DONE/NS-1_model_01.md).
- [Portable resources](DONE/NS-1_resources_01.md).

Initial bounded deliverable: portable camera/geometry/grid primitives, shared
checked native and executed-browser boundary evidence. This does not establish
a rendered workspace control or close the complete criteria above.

Current-source scene checkpoint, 2026-10-14: `nyx.canvas.scene` provides distinct
Unicode item/layer references, fluent immutable placement, explicit layer/item
order and independent visibility/picking. The managed scene adopts actual Nyx
control implementations through existing tree ownership, including compounds
and creator interfaces; removal/retirement preserves independently retained
controls. Immutable snapshots contain metadata only and own a balanced spatial
index with bounded front-to-back queries, exact totals and broad-phase picking.
Empty layers have detached discoverable metadata. This advances the ownership/
spatial portion of criterion four, without accepting its complete clipping,
providers and view integration or criterion one's specialized workspace control.

Checked native and executed HTTP pas2js consumers exercise ownership, exact
Unicode, failure preservation, ordering/filtering/pagination, an independent
brute-force oracle, 10,000-item/256-layer limits and both world extremes. Four real
native threads query one published snapshot. Exact final counts and maintained
command are in WORK.md and [the contract](../docs/canvas.md). No renderer, UI,
source/history, behavior graph, MCP canvas operation or observing rollout is
accepted from this fixture. No task/DONE or aggregate credit closes.

This follows the computed owner's two-partial-batch reassessment: preserving
delivery remains at its recorded external process-operation gate, so no equivalent
retry or release-variant batch was made. This is concrete shared foundation work,
not a third performance diagnostic. Counting the initial geometry and scene
foundation batches, two canvas batches have no full original criterion closure.
Stop foundation-only expansion and reassess toward the specialized public
workspace plus actual browser/LCL embedded-control rendering/input. Native scale
and nested editing policy must be concrete before that next batch. Full graph,
interpreter, diagram and Logic requirements remain separately owned.

Current-source runtime integration checkpoint, 2026-10-14: the reassessed batch
adds a public specialized managed `INyxCanvasWorkspace` Panel/Row/Button
composition and explicit browser/LCL mount adapters. Independent cameras/default
stores, copied admitted fragment contexts and existing Nyx item renderers preserve
model ownership. Typed reflow/logical-size policies keep live editing, clip real
controls and refuse oversized visible allocations before camera mutation.
Camera moves/culling retain actual faces, drafts, focus and subscriptions. Grid
uses copied theme roles; background navigation has ordinary Nyx buttons, keyboard,
wheel and pointer capture, with browser pinch and cancellation. Editor defaults
remain independent; background-only touch policy preserves embedded scrolling.

The final maintained Win32/LCL fixture passes 37 actual checks, clean heap and
zero owned warnings. HTTP browser passes 49 at desktop and CSS-390, with selective
English live-workshop previews inspected. Real native worker refusal and actual
callback-triggered disconnect are qualified; synthetic browser pointers do not
prove hardware capture/touch/pen or assistive quality. Seven upstream RTL warnings
remain separately attributed. Counts overlap earlier prototypes and are not summed.

This advances original criteria one/two/three/four/six but closes none completely.
The control is an explicit runtime composition, not a registered persisted kind.
Plain Node clone/generation reconstructs chrome only. Source/history, reusable
scene overrides, Studio/MCP authoring, resources/providers, rulers/guides, broader
input/quality/performance and observing delivery remain required. No task/DONE,
kind-count increase or aggregate credit closes. Canvas no-closure count advances
two → three after the evidence-backed action change; stop renderer/fixture variants.
Next select the concrete typed document/source ownership bridge, retaining complete
original criteria and both-target evidence instead of more prototype controls.
See [the runtime contract](../docs/canvas.md) and current WORK.md receipts.

Current-source saved-definition checkpoint, 2026-10-14: immutable typed plans in
`TNyxDocument.Canvases` retain root references, camera/grid/scaling, ordered layers
and placements. Version-13 persistence preserves preceding document families and
older opaque extension meaning; crafted fluent Pascal has bounded literal source
admission and exact paired history. Explicit materialization independently clones
content roots into public runtime workspaces. Actual compiled browser/LCL inputs
retain direct/computed bindings and fixed-size policy-aware fit. The full original
criterion five still requires normal renderer attachment, reusable overrides and
public Studio/MCP scene authoring. Full counts, retained preparation failures,
protected LAN restoration and current identity are in WORK.md.

This fourth partial canvas batch changes the action to document/source ownership;
it does not close an original criterion/task or add kind-count/aggregate credit.
Stop declaration/fixture variants. Next integrate the saved contract with normal
renderers and revision-aware authoring rather than extending local diagnostics.
Interpreter/graph/diagram/Tool Canvas/Logic outcomes remain required; metadata
alone cannot establish the user's functional nonvisual components.

Current-source ordinary-rendering checkpoint, 2026-10-14: specialized managed
canvas declarations attach distinct saved scenes through normal browser/LCL
renderers. Actual application controls qualify live binding, forwarding,
retirement, inherited interaction restrictions and refused nested candidates.
The public Studio picker qualifies exact paired source history and stale/draft
guards through both ordinary adapters. Bounded saved-scene semantic context and
grouped typed mutations retain revision/permission/retry guards; root removal
discovers unmounted scene dependencies. WORK.md owns exact counts, failures and
the protected frozen release. These schemas are current source, not installed.

Canvas no-closure count four → five. This materially changed integration action
still does not accept complete criterion five or full task/credit. Stop ordinary
renderer/picker/capture variants. The next deliverable is independent reusable
scene override ownership through exact source/history and actual instances.
Broader scene editing, handwritten continuation, resources/providers, rulers,
gesture/AT/widgetset quality, budgets and preserving delivery remain open under
the original criteria; graph/Logic outcomes must produce functional results.
