# Native Nyx Studio

[Project](../PROJECT.md) · [Building](building.md) · [Authoring owner](../TODO/NS-4_studio-authoring_01.md)

The native editor now has a standalone entry point in
[nyx_studio_native.lpr](../studio/nyx_studio_native.lpr). It consumes the same
`BuildNyxStudioView` composition, portable session and source/property/event
routers as browser Studio. The palette, hierarchy, inspector, warning cards,
canvas surface, split view and Pascal editor are ordinary public Nyx components.
LCL supplies their native adapters and the containing application window.

```powershell
./tools/build.ps1 -Target native-studio
./build/native-studio/controller/nyx_studio_native.exe
```

The executable takes an optional local project directory as its first argument;
otherwise it uses the operating system's application configuration directory.
An optional second argument selects an explicit `http://127.0.0.1:port` service
origin, and a third selects one exact project reference. Omitting the reference
selects the service's primary project. Remote origins, paths, queries and
credentials refuse before network work. These arguments are machine settings,
outside portable designs; connecting is optional.
An explicit service argument now also installs the native shared source provider.
Pascal Apply and handwritten title/property continuation use the service's owned
FPC constructor, guarded paired publication and exact observing acknowledgement.
Creating the provider starts no compilation and chooses no application output.
The service must have an independently configured native compiler profile and
support the current native private source protocol. An older service refuses
without consuming local source input. This entry-point wiring is linked; real
HTTP/physical connected-editor qualification and installed rollout remain open.
Project-file Open retains its separate admission strategy; connecting alone does
not grant arbitrary imported files constructor authority.
The maintained Win32 source-control consumer now passes **92** checks: 68 local
cases plus 24 using the production shared provider/real backend/FPC through an
in-process transport. Actual memo/Apply, canvas selection, title/text fields and
shared Undo/Redo preserve the complete pair and invalid unfinished Unicode draft.
Rendered Apply/visual-edit views are inspected and its heap is clean. This proves
ordinary controller/widget notifications; sockets, timer delay, hardware/IME,
other widgetsets and installed delivery remain open. The connected Agents panel's
canvas share and apparent status lag need further presentation qualification.
Launching the built editor requires no application compiler, browser runtime or
server connection. The Outputs section is available at any time, and choosing an
output does not alter the design or generated source.

In an ordinary native launch, **Outputs → Pascal source execution** configures
local source compilation at any time. Supply an existing FPC executable, a Nyx
library folder containing `src`/`studio`, and an independent writable compiler
workspace; choose **Use for Pascal source**. Choosing an application output is
optional. Configuration starts no compiler process. Subsequent Pascal Apply and
project Open use the actual compiler/constructor for complete handwritten units,
including helpers and loops. Failures retain the current project and source input.

**Turn off source compilation** returns future operations to strict
compiler-independent admission. Changes require every source context to be idle;
running/queued work keeps its original immutable compiler settings. Configuration,
disable and re-enable preserve paired history and unfinished text. Embedders that
inject a local/shared compiler own that strategy and do not expose this competing
configuration section. The typed `TNyxLocalSourceSettings` and controller methods
also serve trusted UI-thread hosts; machine paths are not authored node properties.
An explicit service launch is such a shared strategy. Omit the service argument
for the ordinary local source configuration flow above.

Settings persist as bounded UTF-8 hints in `.local/source-settings.json` inside
the host's local project-store directory. They never enter project backups,
exported designs, adjacent Pascal or Undo. Reloading paths grants no execution
authority and requires no compiler. Damaged hints leave Studio usable with a
visible diagnostic. An ordinary persistent lock sibling serializes cooperating
writers, and exact prior bytes prevent a stale editor from replacing newer
settings; a busy/conflicting write retains the existing compiler. External hostile
filesystem changes and other OS/filesystem qualification remain separate gates.
Service output profiles remain independently configured when connected; offline
Studio presents local source settings without unavailable service-save actions.

The controller owns its session, three renderers, theme, local paired store and
parking hosts. Its application host is borrowed and must outlive the controller.
Destroy the controller before that host. Repaint callbacks are canceled during
destruction, and renderers are released before their documents and hosts.

The public Actions menu is independently owned. Compatible section presentation
retains its open parent/child windows and physical focus, while rebinding the
current button node/event router. Project-generation, selection/view, history
availability or actual-anchor changes retire the previous family. Build-job
navigation remains available before service capability, with an ordinary Nyx
connection explanation; service operations still require bridge admission.
The maintained native menu/workspace journey passes 37 on this Win32 host. See
[the recovery evidence](../WORK.md#current-return-path-recovery-menu-continuity--2026-10-10)
for scope and remaining parity requirements.

Native widget notifications publish portable commands and queue a coalesced
paint. The controller moves the existing canvas and Pascal editor to hidden
parking hosts before replacing chrome, then mounts those same views in the new
shell. Source typing does not rebuild chrome. Hiding Pascal or switching compact
panels retains its actual control, exact draft and paired baseline. View changes
replace the canvas deliberately; selection and value proposals keep it mounted.
Inspector and project-title input stays visible immediately while its paired
source reconciliation is prepared. Native field callbacks enqueue work; chrome
replacement follows the callback and restores field identity, focus and scalar
selection. The mounted Pascal editor receives the admitted companion afterward.

The source candidate routes Apply, inspector properties, project title,
canvas values and palette/structural operations through `TNyxSourceCommands`.
One immutable request
runs at a time. Repeated Apply supersedes older Apply requests; design commands
remain FIFO, coalescing only adjacent waiting changes to the same field. Up to
64 waiting intents are retained; excess input refuses instead of dropping work.
An immutable session/load context accompanies every queued command. Opening even
identical files retires earlier intent before a new baseline can be captured;
old work neither retargets matching IDs nor paints pending fields in that load.
Save/export and builds wait for current pending edits instead of announcing an
earlier accepted pair as current. A presentation exception cannot strand the
remaining command queue or alter already published history.
A worker
constructs a fresh complete document/companion using owned default recipes and a
captured creator-schema environment. Completion compares fresh accepted files,
the exact draft, session/load identity and creator generation before publishing
one paired Undo entry. Design requests replay the existing authoring commands on
independent session owners. Queued targets retain their original selection/view
identities; completion does not steal later user navigation. Invalid, superseded
and stale results retain current work. Existing pending Pascal drafts retain
their exact original base, becoming visibly stale when the accepted pair changes.
Source status sits above the Pascal actions, including at compact widths.

Native preparation uses `INyxScheduler` worker threads. The browser adapter uses
the separately compiled Pascal `nyx_source_worker.js`, bundled with browser Studio
and its matched embedded RTL. Ordinary deferred browser callbacks remain on the
UI loop. Worker replies use a private trusted processor channel; file, HTTP and
MCP imports retain complete source admission. Creator publication cannot
interleave the final generation check and paired swap.

Each native project owns its command controller. Before destroying several
projects, detach every UI port, then drain retained native work while servicing
its handoffs. Source workers never borrow accepted nodes, renderer handles or
mutable recipes. See WORK.md for 39 actual Win32 controls, desktop/390 captures,
original-size timings and the still-pending browser execution/deployment gate.
Broader state/binding/event authoring and general
source/import/review operations retain their existing owners. Preparation on a
worker does not establish comfortable editing: fresh admission, publication and
projection still consume the UI thread. Supported handwritten metadata displaced
by a move now follows the control's ownership call, preserving later extension
values, comments and exact compiled design meaning. Broader structural/source
synchronization remains under the original criterion.

Canvas input captures owned value text, its concrete platform, exact view,
runtime field, editable owner and mounted session/load identity. No realized
node or widget reaches the worker. The independent session realizes that field
again, applies platform overrides and document defaults, then invokes the shared
typed authoring command. Two-way bindings update typed document defaults without
changing explicit fallback text. Unbound reusable fields edit only their owning
instance's named-part override; reusable definitions and siblings stay independent.
Read-only, one-way, wrong-type and out-of-range proposals refuse atomically.

Accepted and rejected canvas completions reconcile that exact physical field.
A rejection remains a rejection; its presentation reset does not publish source
or create history. Copied pending proposals overlay accepted values so completion
of an older job cannot overwrite newer queued input. Repeated waiting input
coalesces only for the same field and platform. Old mounted fields and retained
intents refuse after an identical-ID project reload, before coalescing can replace
new work. Actual native controls qualify typing, caret, paired Undo/Redo and
retirement; the browser adapter compiles but retains its runtime execution gate.

Both public renderers expose `TryRefresh`. Each independently realizes and
validates the requested current view, compares exact document context, creator
generation, effective theme and ordered structure, then retains existing controls
for supported scalar presentation changes. An independent authored baseline
distinguishes changed defaults from live input: an unrelated caption update must
retain an independently edited value. Native checks exercise control identity,
draft/caret preservation, callbacks and failure paths; browser consumers compile,
with execution still gated. Scalar bindings, custom factories, structural/style
changes and context changes refuse reuse and request the normal staged `Render`.
The optional typed `TNyxProjectionValueRestores` argument uses
`TNyxProjectionValueRestore.ForField` to reset an exact runtime/editable identity
to its fresh accepted Value, including an absent property. The complete restore
group validates before any mutation; unrelated runtime drafts remain untouched.
Neither renderer retains the caller's document or mutable authored nodes. Native
candidate construction balances LCL's host sizing lock, including factory failure;
retirement disconnects resize before freeing bindings. Studio consumes this public
contract for its shell and native canvas. The stable public source-status label
changes text/visibility without inserting a new child during preparation.

A compatible native shell keeps the canvas and source mounted in their existing
borrowed hosts. Parking is reserved for full shell replacement before the old
hosts retire, with exact-host recovery on admission failure. This avoids two
large-view reparent/layout passes for every scalar chrome refresh, without
skipping current view validation or retaining the shell document. Ordinary native
builds can use `-NativeStudioConfiguration release`; the separate checked build
retains heap/lifetime evidence. Current timings and original-size qualification
remain in WORK.md, with browser runtime and full-product gates still open.

```powershell
./tools/build.ps1 -Target native-studio -VerifySourceScheduling
```

This local qualification launches no Studio/HTTP/MCP listener and replaces no
project, enrollment or application compiler profile.

The starting document and initial review examples use English copy. Dedicated
editing, draft and persistence checks still exercise supplementary and
multilingual text. These qualification inputs do not limit the author's language.

Pages, reusable definitions and instances use the shared authoring commands.
The Properties/Events inspector creates Pascal TODO handlers and navigates the
actual source editor. Multiple callback registrations, exact removal warnings
and paired Undo/Redo use the same contracts as the browser. Designer controls
bypass application actions and runtime callbacks.

Ordinary Import and local exports now consume the public [Nyx text-file exchange](files.md).
The typed picker admits a complete project backup or adjacent design/Pascal files,
preserving handmade source and staging mismatches for explicit conflict choices.
Native uses target-owned dialogs and exact UTF-8 byte streams. Export cancellation
retains the project; late results refuse after a different project/load. The
Win32 consumer substitutes only the OS chooser, then exercises real native file
bytes and editor callbacks. Its 64 assertions and exact compiled export pass;
physical chooser and full native parity remain separate requirements.

Save uses `TNyxProjectStore`: adjacent design/Pascal files, pending source and its
base are published through the qualified write-ahead paired contract. Expected
file revisions prevent silent overwrites. The warned saved-version action first
backs up the current exact pair under an independent key. Admission and failed
backup keep the current session. Output parameters must be distinct from inputs
at native file-call boundaries; qualification uses an independent returned
revision for its second writer.

## Shared service and concurrent projects

The native editor and browser share `TNyxStudioAgentBridge`, including revision
admission, protected local recovery, queued paired publications, server-owned
Undo/Redo and operator permissions. Platform adapters own HTTP and timers. Native
HTTP workers own bytes only; UI timers deliver replies on the UI thread. Pausing
detaches local notification, and destruction joins outstanding work before
releasing its borrowed session. Cancellation cannot revoke an already admitted
server operation. The current source candidate now snapshots a typed fifteen-
second whole-request policy for browser/native editor exchanges. Native queued
requests retain the original Post time; receiving partial bytes never resets
the deadline. Actual stalled-header/body/upload and detached-receiver checks
qualify the Win32 adapter. Deployment of the new editor HTTP route remains open.

Agents shows project sessions and activity in the same public Nyx composition.
Jumping waits for acknowledged local publications and successful admission of
the target. Each native context owns an independent mirror and an immutable
service bridge. Inactive bridges cannot retarget requests or repaint another
project. Return restores accepted/design/draft/base bytes, optional Pascal
visibility, scalar range/focus and stored scroll positions. Output machine
configuration is shared by this editor. The current journey qualifies draft,
history and source presentation; complete per-project presentation remains open.

A differing recovered pair is retained for explicit resolution. The native
**Save local backup and use shared design** action writes an independent paired
backup before adoption. Compiler diagnostics may borrow local accepted source
only after exact synchronization; consuming this metadata does not prove a build
request or compiled execution. Older servers visibly report unavailable project
closure instead of claiming confirmation succeeded.

## Qualification and remaining integration

The maintained actual-control journey consumes an unchanged MCP-authored export:

```powershell
./tools/build.ps1 -Target native-studio -VerifyNativeStudio `
  -DesignerSourceDirectory <semantic-export-directory>
```

The [Pascal consumer](../tests/nyx_studio_native_tests.lpr) uses the real controller
and actual native controls. Its full journey covers canvas proposals, recipe
independence, page/component navigation, properties, multiple callbacks, warning
confirmation, exact source/ranges, compact switching, paired Save/conflicts and
queued destruction. The optional third fixture argument `geometry` exercises
focused source and canvas visibility after compact parking without the full
callback/file journey. Artifacts and terminal counts belong to [WORK](../WORK.md).
An earlier longer callback journey captured a blank compact canvas despite
retained controls. The refreshed full English journey and focused geometry
journey both paint their memos. The earlier failure's cause remains unqualified;
reliable compact painting and native sidebar overflow stay under the original
native Studio acceptance.

The native controller remains an integration candidate, with Win32 evidence.
The maintained [service journey](../tests/nyx_native_workspace_tests.lpr) passes
49 checks: real MCP composition/observation, actual native Unicode input,
authoritative Undo/Redo, independent project drafts/history, full-editor return
and source focus/range restoration. Actual 390-pixel and desktop captures paint
their English memo. It also exercises deferred requests, canceled timers and
destruction with active HTTP work; heap tracing reports zero leaks. Reviewed
semantic cleanup restores both borrowed test projects' original pairs exactly.
The production primary and qualification primary remain unchanged. This bounded
journey does not establish reliable painting or performance across all workflows.

The compiler consumer below is a new source candidate. The identity-verified
older qualification listener still reports native Build unavailable; it has not
been replaced. The later portable file consumer above qualifies general native
import/export on this Win32 host. Successful live closure and complete native
parity retain their original acceptance paths. Neither native compilation
nor the shared-shell browser regression substitutes for those checks.

## Asynchronous compilation and compiled previews

A capable service advertises private editor compilation explicitly. Build view
captures the selected page or reusable definition; Build app captures the whole
accepted project. Requests use the same immutable job engine as semantic
`nyx_build`, with exact project revision and output identity, bounded diagnostic
windows and owned artifact manifests. Operator builds remain available when
agent access is disabled or read only. Public MCP credentials gain no operator
authority and cannot set machine compiler paths.

The native controller loads machine profiles on demand in Outputs, retains local
field changes during a delayed read and saves through output-identity comparison.
Persistence failure preserves the accepted service profile. Paths stay outside
project/design/source history. An empty output choice opens Outputs with useful
help; a missing compiler refuses only the requested build. An older service
reports the unavailable capability accurately.

`INyxCompilerRequest` provides reference-counted fluent authoring. Target/scope
are enums; roots, output identities and operation receipts have distinct types:

```pascal
LRequest := NewNyxCompilerRequest
  .Target(btNativeLCL)
  .Scope(bsView)
  .Root(NyxBuildRoot('account-page'))
  .AtRevision(LRevision)
  .Output(LOutputIdentity)
  .Operation(NyxBuildOperation('account-preview'));
LBridge.RequestBuild(LRequest);
```

The bridge owns a fixed project context. Compiler status queries cannot retarget
it; profile/build failures do not become document synchronization conflicts.
Each native context retains its job, captured pair/profile and completion state.
UI timers observe worker completion; compiler workers do not borrow widgets.
Inactive-context completion/return still needs actual consumer qualification.

Current compiler failures open the ordinary Pascal editor. Diagnostic actions
retain Unicode scalar line/column coordinates and require its unchanged accepted
source; drafts or changed source prevent stale navigation. The original compiler
log remains available with the job. The private status consumer asks for at most
twenty diagnostics, with actionable errors first.

Run compiled preview uses an admitted succeeded/current artifact, independently
of the designer's Interact mode. Before native execution, the adapter downloads
only the exact admitted executable from the explicit loopback service, rejects
redirects, caps bytes to the manifest and verifies exact size/MD5. MD5 checks
delivery consistency, not authentication. It then queries source/output
currentness again before activation. Native launch uses an executable and owned
working directory, with no client shell command or arguments.

The adapter owns its preview process separately from Studio. Stop retires only
that handle; it preserves an independently pending compiler admission/result.
Finishing that build does not restart an explicitly stopped preview.
A successful current rebuild can replace a running preview; the
old process remains until the verified candidate launches successfully. Prepared
native files have independent private directories and retire after their process
releases them. Download cancellation detaches notification immediately; native
destruction joins the worker. Native downloads now use a thirty-second whole-
request deadline and short nonblocking readiness waits for cancellation. The
browser artifact path opens a validated immutable
URL through the system browser; this native consumer's browser-launch lifecycle
has no new execution qualification.

The maintained Pascal compiler/control consumer uses a suspended private protocol
engine with real compiler workers. It never starts a listener. Only its exact
immutable artifact bytes are copied into an explicitly selected existing
artifact-serving root. Thus its HTTP download and native execution evidence does
not establish deployment of the new private editor HTTP route. The separate
updated-listener refusal remains recorded in WORK.md. Other widgetsets/operating
systems, disk/OS stalls, reliable visuals and measured performance remain open.

### Semantic preview launch and intrinsic label measurement

Prepared semantic launches use `nyx_build`'s exact-job `launch` operation. The
ordinary native observer reads the bounded typed intent, loads its private output
profile when necessary and uses the same verified download/currentness/activation
path as Run. It executes only in the currently observed ordinary project. A
browser artifact reports unavailable; a changed local draft/output or context
refuses. The latest native mount acknowledgment is queryable with `launch-status`.
Exact retries retain the owned process. A different operation ID deliberately
requests another run. See [the contract](studio-agents.md#exact-job-observing-preview-launch).

Label intrinsic measurement uses a renderer-owned detached LCL label and temporary
screen context. It preserves widgetset font/text rules without toggling a live
label's wrapping during ancestor preferred-size calculation. The helper owns no
native parent/window and retires with the renderer's controls. Other widgetsets,
font rotations and hardware DPI still need their own qualification.

Nyx also owns the allocated height of ordinary themed edit faces. Their native
AutoSize is disabled before parenting so font-height restoration cannot compete
with the renderer's layout and reject a source/Outputs repaint. Grouped pickers
keep their own internal layout. The input remains the existing LCL edit widget;
selection, clipboard and IME behavior are not reimplemented. Source qualification
checks actual mounted owners, separately from connection and paint-queue idleness.

## Typed transport deadlines

Transport policy is machine/runtime configuration, independent of designs,
paired history, DOM and LCL types. Both adapters copy an admitted snapshot;
changing the policy later cannot silently extend an existing adapter's limits:

```pascal
LPolicy := NewNyxTransportPolicy.WholeRequest(15000);
LExchange := TNyxLCLEditorExchange.Create(LServiceOrigin, LPolicy);
LPreview := TNyxLCLCompiledPreview.Create(LServiceOrigin, LPreviewDirectory,
  NewNyxTransportPolicy.WholeRequest(30000));
```

`TNyxBrowserEditorExchange.Create(LPolicy)` consumes the same contract. Defaults
are fifteen seconds for editor exchanges and thirty seconds for native artifact
downloads. The positive range is 1..120000 milliseconds; unset records and
invalid external policy implementations refuse before allocating transport work.
The deadline includes queued retirement waits, connection/upload and headers/body.
Native socket readiness waits check a thread-safe cancellation event at most
every fifty milliseconds. Native numeric-loopback connection waits use the
remaining deadline, capped at five seconds. Browser XHR uses its whole-request
timeout, as defined by the [XHR standard](https://xhr.spec.whatwg.org/#the-timeout-attribute).

Timeout returns bounded local failure help and never admits partial response
bytes or a runnable artifact. Cancel detaches receivers immediately; destruction
joins only owned work. Cancel does not revoke server admission or compiler jobs.
UI delivery and OS scheduling may occur after the network deadline; this is not
a hard real-time guarantee or a disk/OS-stall bound. The maintained consumer in
[building](building.md) uses a raw test-only Pascal peer and the real-clock
browser host. It does not replace any Studio listener or qualify new editor HTTP
deployment. Original service admission, job cancellation, retention, caching,
other-platform and full reload acceptance remain open.

## Source workspace and expanded editor

The shared Nyx source workspace offers mutually exclusive **Source** and
**Compiler messages** views. Compiler output receives its own scroll area;
it no longer reserves a fixed height above Pascal. The existing canvas/source
split remains resizable. **Expand** moves the retained workspace into a floating
host, and **Close** or Escape returns it. Apply Pascal, Restore accepted and
Save draft remain ordinary Nyx actions in either location. Diagnostic navigation
selects Source before moving the caret.

The ordinary source pane now asks the public split contract for 280 logical
pixels, while the canvas asks for 96. These explicit child minima preserve a
readable source area beside Outputs or Agents without rewriting the chosen
split proportion. In infeasible hosts the panes compress proportionally; Expand
remains available for concentrated editing. Actual native and browser input
fixtures check retained drafts, caret and editor identity through constrained
allocation and modal return; physical-phone usability remains a separate check.

`BuildNyxStudioSourcePane` creates an owned ordinary Nyx tree. Controllers retain
that view and the independent public code editor; moving the platform host does
not create another editor or spend document history. Native root height sizing
uses `nsFill` so the same memo follows window resizing. Source-view/expanded
preferences are per-project presentation, stored in the strict version-3 packet;
version 2 migrates with the inline Source default and preserves earlier choices.

The reusable library boundary is `nyx.modal`: immutable `TNyxModalOptions` and
managed `INyxModalHost`, with DOM/LCL types confined to their adapters. For example:

```pascal
LHost := NewNyxLCLModalHost(LWindow);
LHost.Show(NyxModal('Pascal source').Viewport(94).MaximumWidth(1400));
```

The controller borrows the exact owning window and clears its dismiss observer
before retiring. Native Hide restores that owner's previous enabled state before
reparenting focused controls; the host stays alive until the retained view moves.
The browser adapter uses the HTML standard dialog top layer and cancel event.
Close/Escape and meaningful modal focus follow the
[dialog standard](https://html.spec.whatwg.org/multipage/interactive-elements.html#the-dialog-element)
and [WAI dialog guidance](https://www.w3.org/WAI/ARIA/apg/patterns/dialog-modal/),
checked 2026-10-05. Full browser keyboard/accessibility and physical phone
qualification remain required; compilation alone does not establish them.
Use the focused [source-editor command](building.md#source-workspace-and-expanded-editor).
