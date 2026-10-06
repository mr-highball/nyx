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

## Reproduce the boundary

Portable size bounds now provide the next resizing/constraint prerequisite.
The ordinary inspector publishes typed minimum/maximum width/height fields and
an Unset action, preserving explicit zero versus absence. Existing semantic
property transactions and the isolated ordinary property processor share
cross-field and target-override validation and paired history. Neither bounds
nor drag/drop introduce a separate authoring engine. See [layout](layout.md)
and [the evidence packet](../WORK.md#portable-size-constraints--2026-10-05).
Physical resizing handles, snapping and responsive variants remain required
under original Studio authoring criterion 1; these fields alone do not accept it.

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
