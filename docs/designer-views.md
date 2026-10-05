# Designer and retained views

[Project](../PROJECT.md) · [Building](building.md) · [Studio authoring](../TODO/NS-4_studio-authoring_01.md)

The browser and LCL renderers can project a document for application interaction
or for designer authoring. Pass `True` as the design-mode argument to `Render`;
ordinary runtime rendering remains the default. The LCL overload also preserves
existing callers that supply a runtime state store as their fourth argument.
This is a renderer capability consumed by the new [native Studio controller](native-studio.md).
Full native Studio integration and parity remain open product requirements.

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

## Reproduce the boundary

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
