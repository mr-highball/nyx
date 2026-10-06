# Designer and retained views

[Project](../PROJECT.md) · [Building](building.md) · [Studio authoring](../TODO/NS-4_studio-authoring_01.md)

The browser and LCL renderers can project a document for application interaction
or for designer authoring. Pass `True` as the design-mode argument to `Render`;
ordinary runtime rendering remains the default. The LCL overload also preserves
existing callers that supply a runtime state store as their fourth argument.
This is a renderer capability consumed by the new [native Studio controller](native-studio.md).
Full native Studio integration and parity remain open product requirements.

## Place a control in another layout

Select a control, then choose **Move to another layout** in the inspector.
Select its destination on the canvas or in the hierarchy. **Place inside**
appends to that exact container; **Place before** and **Place after** use the
destination's owner. **Cancel move** clears the pending choice. This two-step
workflow uses ordinary Nyx controls and supplies a keyboard/touch alternative
for nested placement. Arming, selecting a destination and canceling do not add
project history. The accepted move updates design and Pascal together with one
paired Undo step; a changed pair, pending draft or project load retires the
pending source permanently, including after Undo.

The same admission is available through typed commands:

```pascal
Session.Place(NyxPlaceControl(NyxControl('reply-editor'),
  NyxControl('reply-layout'), nplInside));

Session.ApplyPatch(NyxPlacementPatch([
  NyxPlaceControl(NyxControl('send-button'), NyxControl('cancel-button'), nplBefore),
  NyxPlaceNewControl(nkBadge, NyxControl('reply-status'),
    NyxControl('reply-layout'), nplInside)
]));
```

Use `nyx.studio.edits` for these value-only commands. Exact control references
and the closed `TNyxPlacement` enum keep behavior typed. Custom catalog kinds
use the `TNyxKindRef` overload. Positions resolve after extracting the source,
so movement within one owner does not depend on a stale sibling index. Roots,
self-placement, cycles, leaf containers and foreign/occupied identities refuse
without publishing a partial result. Reusable instances require a customized
named layout part; inherited content is not silently copied or changed.
Placing content into its properties-only descriptor promotes that descriptor
to append mode. Final document/property admission still rejects incomplete
payload rules. The isolated processor uses strict version-6 tickets and also
reads the exact prior version-5 shape.

This is the shared placement and keyboard prerequisite. Physical designer
drag/drop adapters, resizing, constraints, snapping, responsive variants and
complete browser/native editor qualification remain with the original
[Studio authoring task](../TODO/NS-4_studio-authoring_01.md).

Designer clicks report `ntDesignSelect`. Editable controls keep their normal
focus/text behavior and report `ntDesignValue` with the actual realized field.
Nyx application actions, routed runtime subscribers and captured native creator
click/keyboard/pointer hooks are bypassed. Collections and split panes do not
perform their application editing/navigation in designer mode. Creators remain
responsible for behavior they implement outside the renderer's event bridges.

The editor host admits a proposal through `TNyxStudioSession.SetCanvasValue` and
selects through the session and renderer `Select` methods. These are different
operations: selection is presentation; an admitted value is a paired Pascal/design
history command. The renderer does not own the editor session or publish its
document. Do not rebuild a native hierarchy inside a widget notification; queue
controller refresh until that callback has returned.

Reusable fields need explicit named parts so an instance override has a stable,
unambiguous path. Authors use `.PartName(NyxPart('reply'))`; the generated companion
uses that same typed contract. An unnamed path refuses rather than changing the
shared definition. An instance edit retains the recipe and other instances.

`Select` outlines the outermost realized face for an authored identity and keeps
keyboard focus in its current control. Empty, absent or hidden identities clear
the outline. The LCL adapter owns non-focusable graphical edges in its scroll
panel and paints them alongside the selected face in its immediate native parent;
page-root edges sit inside the page to avoid creating extra scrolling.

`MoveHost` transfers a mounted view to an empty caller-owned host while keeping
the actual control objects, bindings, state and subscriptions. It preserves a
focused descendant's Unicode scalar range and the containing view's scroll
offsets. Moving to the same host does nothing. Nil, unmounted, occupied or
descendant destinations refuse before changing parentage. After success the old
host can be freed; views must be released before their current host/document.
MoveHost creates no document or history command.

Imperative event subscriptions belong to the subscribing caller. Releasing an
interface or remounting a view does not substitute for `Cancel`. Retire temporary
spies explicitly before installing replacement registrations.

## Explicit designer drops

Design views suppress application callbacks and have no live runtime binding
store. Hosts can opt into a separate public input contract before mounting:

```pascal
LRenderer.DesignerInput := NyxDesignerInput.Drops(True);
LRenderer.OnDesignerGesture := DesignerGesture;
```

The browser and LCL renderers both expose this typed policy and synchronous
borrowed receiver. Clear the receiver before destroying its object. Its copied
target identifies the authored owner, original source, exact direct named-part
path and effective container. It borrows no tree/widget. The adapter seals the
decision when the receiver returns or raises; retained decisions cannot act
later. Changing the policy on a mounted view refuses. Opting into drops keeps
application hooks and design-canvas drag sources suppressed.

Studio consumes public Nyx palette drag sources and a separate inspector grip,
so authored text controls retain selection and editing behavior. A closed
inside/before/after choice supplies placement intent. One local opaque lease
binds the transfer to this editor, accepted pair, source mount, active view,
load, placement and creator epoch. Hover reads protected formats and copied
identities only, paints an outline and never refreshes or publishes the editor.
Readable drop data must match the exact lease and accepted pair before one
isolated placement command updates both files. External text, HTML, files and
URIs never become an implicit import. This follows the
[HTML drag data-store phases](https://html.spec.whatwg.org/multipage/dnd.html#the-drag-data-store).

Inherited content is addressable only through an existing exact customized
layout slot. Studio cannot silently create an override or edit the shared
definition. Cycles, roots beside roots, leaves as containers, retired source
views, pending drafts and changed pairs/creator epochs refuse. New palette drops
and moved controls share the same candidate admission, source processor and
paired Undo as semantic transactions and the keyboard/touch alternative.

`./tools/build.ps1 -Target designer-drag -BrowserOutput build/designer-drag/staged`
reproduces the staged packet. Its Win32 callback evidence does not qualify
hardware hit-testing on disabled native widgets. Its compiled browser review
does not establish physical drag-manager, mobile touch or assistive-technology
behavior. The existing observing-release and host execution gates remain.

## Reusable resize grips

`nyx.designer.resize` supplies copied `TNyxResizeSize` and
`TNyxResizePolicy` values, closed width/height/both axes and a public
`TNyxResizeHandle` behavior attached to ordinary specialized Nyx button events:

```pascal
LGrip := NewNyxResizeGrip('resize-notes', nraBoth);
APolicy := NyxResizePolicy.Grid(8).KeyboardStep(8)
  .Bounds(NyxSizeConstraints.MinimumWidth(160).MaximumHeight(400));
```

Sizes and bounds are logical outer-face pixels in the admitted 0..100000 domain.
Snapping rounds final dimensions to the nearest grid multiple, with ties upward,
then applies exact bounds; the unchanged axis stays exact. A zero delta does
nothing even for an off-grid size. Alt bypasses the pointer grid. Both changes
dimensions together without implying aspect-ratio locking. Arrow keys commit
one step; Shift multiplies the step by ten. Application shortcuts and unrelated
axis arrows retain normal behavior. Recognized resize arrows remain consumed
when the host refuses capture, avoiding accidental scroll during a busy/draft
state.

The behavior borrows synchronous UI-thread capture/feedback method receivers.
The host must disconnect before destroying receiver objects and must not destroy
the handle inside a receiver. The owned behavior retains event subscriptions,
copied dimensions and pointer identity, without borrowing a model or widget.
Disconnect detaches callbacks before canceling subscriptions. Primary matching
pointer input requires actual capture capability; foreign pointer IDs and
secondary releases cannot commit. Escape, focus exit, pointer cancellation,
capture loss and retired routers cancel. Capture remains owned until terminal
pointer input after Escape/focus exit, preventing a second gesture from starting
inside the first one's capture lifetime.

Renderer `SizeFor` reads copied mounted outer geometry. LCL uses the full logical
allocation, including clipped/offscreen controls; browser uses integer
`offsetWidth`/`offsetHeight`, including borders and excluding transforms. These
are allocated dimensions, rather than an input's inner client viewport or its
authored width before weighted layout.

Studio builds three ordinary grips beside the selected authored non-root
control. It captures the exact source/canvas mounts, accepted pair, load, view,
selection and creator epoch. Pointer preview only updates the dimension status;
release submits one existing isolated paired operation. Pending drafts, busy
commands, changed pairs and retired leases refuse. Realized parent flow,
including reusable slot context and primitive Row defaults, determines whether
a touched main-axis weight must be cleared. Portable single-axis editing refuses
divergent target flow policies; explicit target fields or Both remain available.
Existing target sizing/bound overrides retain their explicit Inspector fields
instead of being silently erased by this first portable gesture path.

Fresh dimension-only projection can retain admitted widget identity, text,
selection and focus through the existing rollback boundary. Structure, identities,
bindings, platform metadata, creator context and custom-factory compatibility
still require exact admission; incompatible changes request a full mount.
An optional borrowed presentation sink now paints copied proposed dimensions
through the public canvas adapter, after shell refresh and a fresh lease check:

```pascal
ARenderer.PreviewResize(NyxResizePreview(NyxControl('notes-editor'),
  NyxResizeSize(320, 180)));
ARenderer.PreviewResize(Default(TNyxResizePreview));
```

Only the selected authored face in design mode accepts an active proposal.
Missing identity refuses before replacing presentation. Four inert accent strips
outline its proposed outer size without changing allocation, accepted source,
input, focus or scroll extent. Selection, cancellation and unmount retire them.
Browser strips follow captured scroll and viewport resize, preserve axis-aligned
scale, use locale-independent CSS numbers and clip to host/viewport bounds.
LCL reuses four bounded, disabled standard panels above descendant windows;
the renderer owns and reparents them independently of the accepted tree.

Native `PaintResizePreview` paints those actual visible panels into a borrowed
caller-owned canvas; its origin names the screen pixel represented by bitmap
pixel (0, 0). Win32 form `PaintTo` includes the non-client frame, so a composed
whole-form capture must use the window origin. This establishes offscreen
control painting, rather than physical desktop capture. The same capture now
includes mounted canvas buttons. Richer guides, responsive variants and complete
presentation/performance acceptance remain open.

`NewNyxCanvasResizeGrips` returns managed `INyxCanvasResizeGrips`, owning an
independent three-page adornment document with specialized Nyx buttons. A target
adapter retains it through `AttachResizeGrips`, renders those public controls in
independent event scopes and places 44-pixel targets at the right midpoint,
bottom midpoint and bottom-right corner. Small faces hide overlapping one-axis
handles; the Inspector alternatives remain available. Ordinary application
callbacks stay suppressed in the edited document.

Its capture/feedback receivers are borrowed exactly like ordinary grips.
`Disconnect` permanently retires them before their owner is destroyed; target
`Unbind` silently retires subscriptions and invokes an optional lease-retirement
receiver. That receiver may revoke lease state, never paint or reenter mounting.
Repeated attachment retains input identity and capture. Selection/unmount
retire hosts/scopes before releasing the interface-owned document.

Moving handles use `TNyxResizePointerMap` to return a finite, defined
`TNyxResizePoint` in one stable logical plane. Native mapping resolves the actual
button's screen origin; browser mapping uses its viewport origin and the edited
face's axis-aligned scale. Mapping runs once per sample before computing a delta;
local-only behavior remains the default for stationary grips. This prevents
movement feedback from altering the next sample's origin. Keyboard behavior
honors the configured snapping policy; precision clients can select
`nssUnsnapped`. Unsupported rotation, physical hardware/IME and full target
qualification remain outside these bounded checks.

`./tools/build.ps1 -Target resize -BrowserOutput build/resize/staged` reproduces
the checked shared and actual Win32 evidence plus compiled browser consumers,
Studio and its worker. Native callback checks do not qualify physical hardware,
IME, assistive technology or another widgetset. The additional browser preview
consumer passes 47 actual DOM checks at desktop and exact 390 pixels, including
real canvas keyboard listeners, retirement and retained scope identity. Native
Studio passes 70 actual checks including moving handles and one paired Undo.
That earlier adapter journey does not establish ordinary Studio pointer
interaction or phone observation. The guide journey below now adds ordinary
browser pointer/worker evidence. See
[evidence](../WORK.md#direct-canvas-resize-handles--2026-10-05).

## Alignment guides

`nyx.designer.guides` adds a copied, typed layout snapshot to the resize policy.
Capture it once at gesture start through either renderer's `AlignmentFor`, then
attach it with `.Guides`:

```pascal
LGuides := LRenderer.AlignmentFor('notes-editor', niDesign);
APolicy := NyxResizePolicy.Grid(8)
  .Bounds(NyxSizeConstraints.MinimumWidth(100).MaximumWidth(400))
  .Guides(LGuides);
```

Custom hosts can supply their own geometry without importing DOM or LCL types:

```pascal
LGuides := NyxAlignmentContext(
  NyxGuideBox(20, 30, 200, 120),
  NyxGuideBox(0, 0, 600, 400), NyxControl('workspace'))
  .Peer(NyxControl('companion'), NyxGuideBox(300, 60, 217, 143))
  .Tolerance(6)
  .Positions(True);
```

Coordinates belong to the same immediate parent's logical client plane.
Positions may be enabled only when resizing keeps the selected origin stable;
the built-in adapters enable them for absolute layouts. Flow layouts match
sibling widths/heights and avoid promising unstable edge/center alignment.
Snapshots copy identities and geometry, retain no widget/tree, and admit at most
256 visible sibling peers in stable mount order. Explicit oversized/duplicate
peer additions refuse. Fluent additions preserve independently retained copies.

Nearby valid integer dimensions take precedence over the grid. Nearest wins;
ties prefer matching size, edge, center, then snapshot order. Bounds eliminate
invalid targets before selection. Alt bypasses guides and grid together; arrow
keys bypass guides and retain the configured step/grid, avoiding sticky keys.
Zero movement remains a no-op. `TNyxResizeSize.WidthGuide` / `HeightGuide` explain
each match. They are transient presentation; only dimensions enter history and
the wire contract.

Matching sizes paint two measurement bars, one at each actual face. Absolute
edge/center matches paint a connecting line. Both adapters own at most four
additional inert strips, clip before physical allocation, and retire them with
the resize outline. `PaintResizePreview` includes native guide panels. Preview
changes neither accepted source nor live input. Studio resnapshots geometry at
release and refuses a changed layout rather than committing a stale match.
Canvas grips now belong to the selected control and remain available when a
compact Design panel hides its Inspector.

Portable resizing refuses viewport-scoped sizing or parent flow changes because
it cannot safely infer which presentation the author wants to edit. The existing
responsive Inspector remains available; choosing a presentation directly on the
canvas is still open. Transformed rotation, nested scroller/virtual geometry,
physical phone input, other widgetsets and large-project performance need broader
qualification. These checks do not close the full authoring/parity criteria.

Use semantic MCP to compose `tests/alignment-review.operations.json` in an
independent project, then export unchanged accepted Pascal in bounded same-revision
`nyx_source` windows. `tools/build.ps1 -Target guides -GuideSourceDirectory
build/alignment/mcp-source` stages shared/native checks, Studio, its worker and
browser consumers. The Pascal `nyx_guides_browser_review` driver uses real pointer
input only to qualify capture, paint, worker publication and Undo/Redo against
that explicit workspace. It does not compose a replacement document.

## Reproduce the boundary

Portable size bounds now provide the next resizing/constraint prerequisite.
The ordinary inspector publishes typed minimum/maximum width/height fields and
an Unset action, preserving explicit zero versus absence. Existing semantic
property transactions and the isolated ordinary property processor share
cross-field and target-override validation and paired history. Neither bounds
nor drag/drop introduce a separate authoring engine. See [layout](layout.md)
and [the evidence packet](../WORK.md#portable-size-constraints--2026-10-05).
Reusable resize grips now consume those constraints through the public input
contract and the existing paired processor. Direct canvas edge feedback, richer
snapping guides and responsive variants remain required under original Studio
authoring criterion 1; these fields and grips alone do not accept it.

The [semantic review author](../tests/nyx_mcp_designer_review.lpr) creates an owned
empty review using an explicitly supplied MCP configuration, applies the
[nine-operation composition](../tests/designer-review.operations.json) in one
revision-checked transaction, exports bounded source windows and requests both
application compilers. It verifies exact source fingerprints and current
revision/output, retires that exact review and compares the primary editor's
published content/history/navigation frame. It does not print credentials, reset
a project, start a listener or infer successful execution from compilation.

```powershell
./tools/build.ps1 -Target designer-controls -DesignerMCPConfig <local-config-file>
```

The endpoint must already support owned review workspaces. The current LAN
release has fifteen tools; the review-capable staged service has a distinct
inventory. Supplying no configuration instead consumes an existing export with
`-DesignerSourceDirectory <export-directory>`.

The maintained target runs the checked
[actual LCL consumer](../tests/nyx_designer_controls_tests.lpr), then compiles that
same Pascal consumer for the browser. Serve the staged `designer.html` through an
existing owned Pascal service and require `data-designer-tests="passed"`.
`designer.html?host=1` runs the same consumer at an actual 390-pixel viewport.
Passing an output directory to the native fixture additionally captures the
actual canvas/source controls and checks the painted component/page selection
accent. Browser synthesis qualifies listener behavior; it does not establish
trusted phone keyboard, hardware, IME or assistive-technology behavior.

Current commands, retained failures, captures and acceptance limits belong to
[WORK.md](../WORK.md). These adapter gates do not accept full native Studio,
concurrent project navigation, live workspace closure or the wider accessibility
and production UI requirements.

## Absolute-position movement

The public [move contract](../src/nyx.designer.move.pas) provides copied logical
origins, fluent policies and specialized managed grips. A policy has an explicit
grid, exact keyboard increment, origin bounds and copied alignment context:

```pascal
LPolicy := NyxMovePolicy
  .Snap(npsGrid)
  .Grid(8)
  .KeyboardStep(8)
  .Guides(LAlignment);
LProposal := LPolicy.Adjust(NyxMovePosition(20, 30), 23, 36);
```

`LAlignment` is the adapter's copied accepted parent/sibling geometry. Nearest
edge/center guides precede grid snapping, then bounds clamp the proposal. Alt
bypasses snapping; arrows use the exact increment and Shift multiplies it by ten.
An unchanged axis preserves its accepted origin, including an off-grid value.
The default portable origin domain is 0..100000 logical pixels; explicit zero
differs from an absent value. Copied guide explanations do not alter equality or
persistence.

`TNyxMoveHandle` retains subscriptions and borrows synchronous capture/feedback
receivers until disconnection. Hosts provide copied accepted policy/position,
map local pointer input into a stable logical plane and own mutation/history.
`INyxCanvasMoveGrip` owns a separate one-button Nyx document; renderers retain the
interface and borrow its tree. Its 44-pixel face remains available in compact
Design even when Inspector is absent. Escape, focus/capture loss and owner/mount
retirement cancel proposals. External cancellation retains a capture lease until
the adapter reports release/lost capture.

Ordinary Studio offers movement only for an authored nonroot control under an
absolute parent. Platform/presentation origin overrides, conditional parent layout
and active origin bindings refuse free movement. Configure these intentionally
through the [typed presentation contract](responsive.md). Existing flow placement
continues to use inside/before/after operations. Transient guides paint a copied
rectangle; accepted controls, source and live drafts remain unchanged until one
paired command is admitted at release. The captured pair, view, owner, mount,
creator epoch and geometry must still match.

Compose the English [semantic fixture](../tests/move-review.operations.json) in
an explicit project workspace through one revision-checked `nyx_transaction`.
Export bounded `nyx_source` windows from one revision with its terminal LF, then:

```powershell
./tools/build.ps1 -Target move-snapping -MoveSourceDirectory <export-directory>
```

This target starts no server and changes no enrollment. It runs shared/native
consumers and stages Pascal browser consumers, Studio and the worker. The
[Pascal host driver](../tests/nyx_move_browser_review.lpr) uses actual host pointer,
Escape and arrow input against `move-studio.html?workspace=<exact-handle>` on an
explicit existing test service. Desktop and actual 390-pixel viewports are separate
journeys. Maintained evidence is [recorded here](../WORK.md#absolute-position-move-snapping--2026-10-06).
The result qualifies this absolute-layout movement path. Full reparenting guides,
container allocation, nested scrolling, physical devices, accessibility and
large-project performance remain open.
