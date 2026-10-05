# Agents in Nyx Studio

The protected-review source candidate advertises a sixteenth tool,
`nyx_reviews`. The current LAN release and this chat's connected native inventory
still expose fifteen tools. The qualification service demonstrates the new
capability; its source packet does not establish deployment or project switching.

Studio starts with agent access enabled and editing allowed. Open **Agents** to
see the shared revision, connected endpoint and recent operations. **Read only**
keeps semantic queries available; **Disabled** refuses agent queries and edits.
These operator controls apply immediately to the current server session and
return to the enabled default on a new server session. Agents cannot change
permissions through MCP. The footer shows recent activity even with the panel
closed. The panel itself is composed from public Nyx controls.

## Connect

The Pascal Studio service starts a separate MCP listener on **127.0.0.1**, using
the Studio port plus one unless explicitly configured. Its URL contains a fresh
session identifier and requires a private bearer credential. LAN access to the
editor does not expose the MCP listener. Browser Origins and the HTTP Host are
validated. Credentials are never included in design documents or editor labels.

On launch the server adds or replaces its marked block in the repository's
ignored `.codex/config.toml`. Other configuration bytes are retained; the exact
previous file is backed up locally before atomic publication. An existing
unmanaged `nyx_studio` entry or malformed managed block is retained and reported
in the Agents panel instead of overwritten. Protect these local configuration
and backup files as credentials. A new connection gets fresh credentials after
a server restart.

Codex supports project-scoped MCP configuration in trusted projects, shared by
its CLI, desktop and IDE clients. A client already running may need to reconnect
or restart before newly registered tools appear; Studio does not promise client
configuration hot reload. See [Codex MCP configuration](https://learn.chatgpt.com/docs/extend/mcp?surface=cli).

For a desktop chat attached to another project, explicitly register Studio in
the Codex user's configuration too. Build `tools/build.ps1 -Target mcp-client`,
then run the resulting `nyx_studio_mcp` program:

```text
nyx_studio_mcp register <nyx-repository> <Codex-user-config.toml>
```

This changes the same `nyx_studio` managed block in the chosen file and stores
its absolute path in ignored `.local/codex-mcp-registration.json`. Subsequent
Studio launches refresh project and enrolled user entries with fresh session
credentials. Unrelated configuration is retained byte for byte, with local
backups; ambiguous markers or unmanaged Nyx entries are refused. Enrollment is
explicit and does not affect editor permissions. Remove the local enrollment
record to stop automatic refresh; remove the managed block from the selected
Codex file to disconnect its global registration. Neither action changes a
design. Keep files and backups private.

Reconnect the desktop MCP client once after installing the entry. A successful
`codex mcp list` confirms configuration; initialized tool discovery confirms
connectivity. The installed app-server supports configuration reload, but a
running desktop connection may not expose its control socket on every platform.
See [Codex app-server methods](https://learn.chatgpt.com/docs/app-server).

The official desktop setup finishes with **Settings → MCP servers → Restart**.
Use that reconnect step when the current chat still lacks Nyx tools after the
entry is installed. These are separate checks: `codex mcp list` establishes
configuration, authenticated initialized discovery establishes the connection,
and the chat's tool inventory establishes that its native handles are available.
Keep using the semantic client below while the current inventory awaits refresh.

While a chat's inventory awaits reconnection, the same Pascal program provides
real semantic MCP access without another agent or browser automation:

```text
nyx_studio_mcp tools <nyx-repository> nyx_node
nyx_studio_mcp call <nyx-repository> nyx_session <empty-object.json>
nyx_studio_mcp call <nyx-repository> nyx_source <source-window.json>
```

Use `{}` for session arguments and `{"line":1,"count":8}` for a source window.
Argument files are explicit JSON wire data. Calls negotiate an authenticated
loopback session, identify the developer client in Studio activity and close
the transport afterward. Results print bounded structured context; refused
semantic operations exit with status 2. No design is claimed, loaded, replaced
or mutated implicitly. Mutations use the same revision/operation identity rules
below. Timed-out mutations are never retried automatically.

Other MCP clients can use the URL and Authorization header from that same local
configuration. No Node process, package manager or JavaScript service is needed.
The backend, model, tool implementation, browser controller and preview program
are all Pascal; browser JavaScript is compiler output.

## Semantic context

Semantic tools are the primary way agents inspect, compose and modify demos and
active designs. Read the session first, query only needed nodes/properties/events,
then group related edits into one undoable transaction. Preserve existing user
work and use an isolated review session for destructive protocol fixtures.
Use previews for visual validation. Actual browser/LCL input harnesses still
qualify behavior that semantic document changes cannot demonstrate.

The transport implements authenticated Streamable HTTP with JSON responses,
initialization, session IDs and protocol negotiation for 2025-11-25, 2025-06-18
and 2025-03-26. Notifications receive an empty 202 response. Optional streaming
GET is explicitly unsupported (405); DELETE closes a client session. See the
[MCP transport contract](https://modelcontextprotocol.io/specification/2025-11-25/basic/transports).

| Tool | Context or operation |
| --- | --- |
| `nyx_session` | Revision, current selection/view, title, draft status, permissions and history availability |
| `nyx_outline` | Paged roots or immediate children of an exact parent |
| `nyx_node` | Published typed properties, defaults, choices and optional supported events/registrations |
| `nyx_components` | Searchable catalog descriptions, intent groups, labels and component kinds |
| `nyx_tokens` | Effective colors and metrics for the design |
| `nyx_diagnostics` | Severity-ordered compiler diagnostics from the last current editor/agent build, with source navigation guards |
| `nyx_source` | A bounded range of accepted companion source lines |
| `nyx_transaction` | Atomic semantic operations against the accepted pair |
| `nyx_select` | Select a component, optionally activate a root view |
| `nyx_history` | Undo or redo one ordinary content command |
| `nyx_preview` | An immutable, revision-specific rendered view and optional PNG |
| `nyx_callbacks` | Grouped callback addition, policy, ordering and reviewed removal on the inspector's paired history |
| `nyx_build` | Output readiness, immutable accepted builds and bounded job/artifact/diagnostic inspection |
| `nyx_pascal` | Bounded accepted callback implementations and grouped exact-text guarded edits |
| `nyx_roots` | Reviewed removal of exact page/reusable groups on paired Undo history |

Tool schemas advertise required fields and limits. Unknown arguments and
unpublished properties are refused. Queries never return the full document.
Outline/catalog/property/diagnostic pages contain at most 50 items. Node queries
can specify up to 20 exact property `keys`; `textOffset` and `textLimit` retrieve
Unicode scalar slices (up to 2048 scalars), with a total and truncation flag.
Source queries return up to 80 lines. Structured context is capped at 48 KiB;
request fewer items, properties or lines when that budget is exceeded.

Preview dimensions specify an actual CSS viewport, including narrow layouts;
they are retained in the immutable snapshot. PNG capture checks the measured
viewport and revision before returning an image. Use captures sequentially:
competing browser processes can exceed the existing twenty-second capture budget.

Structured results also have a serialized text representation for compatible
clients. See [MCP tools](https://modelcontextprotocol.io/specification/2025-11-25/server/tools).

With `events:true`, `eventOffset` / `eventLimit` page exact event identities.
`routeOffset` / `routeLimit` page their physical semantic routes across that event
window (default 16, maximum 50). Each event reports its own route total and its
returned routes; the envelope reports `totalRoutes`, `routeOffset` and
`routesPartial`. Route IDs, typed payloads, optionality and target support retain
the selected instance's surrounding context. `declaredProducer` indicates an
explicit creator producer; inferred button/change aliases do not authorize
custom emission. `registrationOffset` / `registrationLimit` independently page
ordered callbacks across the same event window, with explicit partial status.

For example, read `nyx_session`, then query

```json
{"id":"reply-memo","keys":["text","value","readonly"],"textLimit":160,"events":true}
```

The property values retain their JSON scalar types. Human text and open authored
identifiers remain strings at this explicit wire boundary; Boolean/integer/number
properties are not accepted as string spellings. The Pascal implementation
admits these requests into a typed operation enum and an immutable patch object.

Related contextual properties belong in the same operation. For example,
`{"input-type":"number","value":0.1250}` creates/configures a numeric input;
reversing member order has the same meaning. Admission checks the complete
candidate's domain against the original JSON types. Numeric strings still refuse,
and a failed group preserves the accepted source/design, selection and history.

## Semantic compiler jobs

Inspect `nyx_build` with `{"mode":"outputs"}` first. It returns the current
`outputID` and readiness for browser/LCL, with useful configuration issues and
without machine paths. Missing compilers never prevent designing or connecting.
Only the ordinary Studio output section can configure compiler profiles; MCP
cannot supply executable paths, commands, flags, environments or source overrides.

Submit an accepted page, reusable definition or application:

```json
{
  "mode": "request",
  "expectedRevision": 7,
  "operationId": "compile-settings-review",
  "outputID": "<exact identity from outputs>",
  "target": "browser",
  "scope": "view",
  "view": "settings"
}
```

The closed scopes are `view`, `reusable` and `application`. View/reusable
requests require an exact page/definition root respectively. Applications omit
`view`. Browser and `lcl` outputs use the same fixed-argument compiler as
Studio's existing HTTP build route. Request requires Allow edits, exact revision
and output identity, and no pending draft. It captures an independently owned
accepted pair/profile and immediately returns a running job receipt. Compilation
does not hold the document lock, replace source, change selection or create Undo
history. Editing can continue while the captured pair compiles.

Query `{"mode":"status","job":"<returned job>","limit":5}`. States are
`running`, `succeeded` and `failed`. Diagnostics page by `offset`/ `limit`
(at most twenty) with stable error/fatal, warning, then informational order.
Add `"severity":"error"` to retrieve only errors; other closed filters are
`all`, `fatal`, `warning`, `hint`, `note` and `info`.
`diagnostics.total` is the filtered total, `available` the whole report.
Locations use Unicode scalar coordinates in the exact submitted companion.
`navigable` is false if a later edit/draft changed that pair. Successful jobs
include served artifact paths plus byte lengths/fingerprints covering runtime,
compiled companion and saved scoped design. Failures expose no successful artifact.

The original revision, source/design fingerprints, `outputID`, target and scope
remain attached to the job. `currentRevision`, `currentSource` and
`currentOutput` distinguish the present editor/profile. Undo may restore the
exact pair despite a newer monotonic revision. Fingerprints are explicitly MD5
optimistic byte identities, consistent with existing project revisions; they
are not authentication credentials. Current-pair/profile and retry checks compare
complete exact text. Profiles retain configured fields, not locked compiler binary
snapshots; toolchain pinning/caching/cancellation keep their service owners.

Exact actor/operation/argument retries return the initial receipt without another
compiler invocation, including after the editor revision or profile changes.
Different arguments with an accepted operation identity refuse. Sixty-four build
receipts and sixteen job handles are retained per server session; oldest terminal
handles expire first. An expired handle is reported explicitly and never silently
resubmitted. At most two semantic jobs run; a third submission refuses. Server
shutdown joins owned workers. The legacy HTTP route retains its synchronous
behavior; a global compiler scheduler remains separate service work.

An observing Studio shows start/completion/refusal activity and the first twenty
severity-ordered diagnostics, with the displayed/total counts. Its ordinary source
actions reuse the existing draft/source guards and additionally require an exact
acknowledged local editor frame. Additional report pages remain queryable through
the job. Semantic jobs are session-local, while existing artifacts keep the
service's disk lifecycle.

The maintained isolated journey is `nyx_mcp_build_tests`, compiled by
`tools/build.ps1 -Target agents`. Supply a disposable loopback editor URL,
its private generated MCP config, and an ignored artifact directory. It composes
through MCP, builds all three scopes on both compilers, verifies served manifest
bytes and exact application source, mounts an actual browser artifact, and checks
an observing Nyx Studio's mapped failure and stale navigation. The intentional
invalid-helper substrate uses a private editor commit because semantic body editing
remains an advertised gap. Add a fourth argument `phone` for the actual 390-by-844
observer. Both maintained journeys pass 107 checks against the real compilers.
Resource/lifetime checks use an explicitly owned Pascal
compiler substitute; they do not establish target compilation or rendering.

## Edits and history

Every mutation supplies the revision it read and a unique `operationId`.

```json
{
  "expectedRevision":12,
  "operationId":"compose-reply-controls-1",
  "operations":[
    {"op":"create","kind":"column","id":"reply-controls","parent":"home"},
    {"op":"create","kind":"memo","id":"reply-memo","parent":"reply-controls",
     "properties":{"text":"Write a reply","placeholder":"Your thoughts…"}},
    {"op":"create","kind":"button","id":"send-reply","parent":"reply-controls",
     "properties":{"text":"Post reply","variant":"primary"}}
  ]
}
```

A transaction contains 1–64 operations: create, update, move, delete, title or
tokens. Creation supports child placement or a new page/reusable root; these
placements are mutually exclusive. Move/delete currently operate on descendants,
not page/reusable roots. Updating a published property to null clears its explicit
override. Tokens cover the seven built-in palette colors and radius,
controlRadius and fontSize; null restores a default. Custom token families remain
future extensions; callback operations use the focused tool below.

The ordinary Studio command pipeline clones, applies, validates and verifies
the complete design/Pascal candidate before publishing either owner. A group is
one undoable command and one content revision. Generated source retains Nyx's
specialized interfaces, purposeful names, comments and authored source frame.
Selection changes advance synchronization revision without adding content undo
history. Revisions remain monotonic through Undo/Redo. Stale revisions and pending
Pascal drafts refuse mutation. Exact successful retries return their original
receipt; changing arguments under the same operation identity is refused. The
last 64 successful mutation receipts are retained for that session.

## Author callbacks semantically

Read the owner's published events and ordered registrations with `nyx_node`.
Physical events use an exact `{"trigger":"after-key-press"}` identity; semantic
events use `{"name":"search"}`. Supply exactly one, preserving its spelling.
Unsupported events, unknown fields and wrong scalar types refuse the batch.

`nyx_callbacks` accepts 1..32 ordered changes. `add` creates the inspector's
crafted handler class, initialization registration and commented TODO; `policy`
sets a closed execution policy; `move` places an exact registration at a
zero-based position within its event; `remove` removes its descriptor while
retaining Pascal code. For example:

```json
{"expectedRevision":7,"operationId":"reply-events-1","mode":"apply","changes":[
  {"op":"add","id":"reply-memo","event":{"trigger":"after-key-press"}},
  {"op":"policy","id":"reply-memo","event":{"trigger":"after-key-press"},"policy":"ui-queue"}
]}
```

Results identify owner/event, handler/registration, policy and position after
each operation. The one-based `line` points into the final accepted source;
zero means an external implementation without a local location. Read the needed
`nyx_source` window there. Add returns a template; it does not invent business
logic. Use `nyx_pascal` below to implement the local callback.

The batch prepares a detached candidate through the inspector's commands,
admits response size, then publishes once: one ordinary paired Undo step, with
operator selection/view retained. Failed changes retain design, source, draft
and history. Pending drafts refuse authoring. Inherited instance edits create
local overrides; definition edits affect inheriting instances. Runtime changes
require rebuilding/mounting.

Before any removal batch, call `mode:"review"` at the current revision with
exact `changes`, omitting `operationId`. Validation returns warnings and a
`reviewID` without editing. Inspect the warnings, then apply the unchanged batch
at that revision with a new operation ID and the review ID. Tickets bind exact
actor, revision and change bytes; altered identities or stale reviews refuse.
Sixteen tickets are retained. Successful apply consumes its ticket; exact retry
still returns the original mutation receipt. Read-only permission allows review;
disabled access refuses it. A Boolean confirmation cannot bypass review.

## Observe and resolve

The editor connects to the same authoritative paired document/source history.
Normal observations poll every 500 ms and send the pair only when its revision
changes. Editor publications are sent promptly in order. Unsent typing updates
of the same accepted pair can coalesce; content commands retain separate history.
The queue is bounded at 100 operations / 8 MiB of target text storage. The activity
feed retains the last 24 successes, retries and refusals with the client's name,
operation and revision. Preview work announces its start and completion/refusal.

The first editor may claim recovered local files. A later attachment protects
different recovered work instead of silently replacing it. An edit/revision race
pauses synchronization with the exact local pair and draft intact. The operator
can keep local work and pause, or download a project backup before explicitly
adopting the shared design. Pausing aborts an outstanding browser observation so
its late response cannot overwrite local work. Accepted source and pending drafts
remain distinct; compiler locations are navigable only against their exact
accepted source with no differing draft.

## Validate visually

`nyx_preview` snapshots an admitted page/reusable root at an exact revision and
returns an opaque-token URL rendered by the actual Nyx browser adapter. Later
edits do not change that snapshot. Capture adds an MCP PNG image when a supported
Edge executable is installed (or selected through `NYX_PREVIEW_BROWSER`). It
verifies the preview-ready and revision markers before returning an image,
constrains viewport/time/output budgets and invokes fixed process arguments.
Rendering is optional; semantic queries remain the primary context mechanism.
Sixteen snapshot packets are retained in memory. Capture artifacts remain under
ignored `build/agent-previews/`; disk artifact housekeeping is still manual.

## MCP-authored keyboard review

The maintained operations in
[keyboard-review.operations.json](../tests/fixtures/keyboard-review.operations.json)
compose a page with search, number-stepper and labeled-button compounds, a
disabled action group and a read-only memo. Use an isolated review service;
these fixed demo IDs must not overwrite an existing design. Inspect `nyx_session`,
then submit the operations through one `nyx_transaction` with its current
`expectedRevision` and a new operation ID. Preserve the exact ID/payload for
resolving ambiguous delivery. The resulting page is one undoable edit.

Inspect only its seven children with `nyx_outline`, and the needed properties
with `nyx_node`. Export accepted Pascal with consecutive `nyx_source` windows of
at most 80 lines. Require the same revision and total line count throughout, then
join the returned lines as UTF-8 into `nyx.generated.view.pas` under an ignored
build directory. Do not substitute handwritten demo source. For a selective
visual check, use `nyx_preview` with the exact revision and `keyboard-review` view.

Compile that exact source with FPC/LCL and pas2js:

```powershell
./tools/build.ps1 -Target keyboard `
  -KeyboardSourceDirectory build/keyboard/mcp `
  -BrowserOutput build/keyboard/browser
```

Serve `keyboard-host.html` from the isolated service, then run
`build/keyboard/driver/nyx_keyboard_cdp_tests.exe <loopback-URL> <artifact-directory>`.
The native Pascal driver sends host keys, queries DOM identities and reads small
Pascal-published assertions. It injects no application script. This qualifies
browser defaults that semantic document tools and synthetic events cannot prove.

The harness adds one typed collection binding after consuming the MCP-created
page. Collection/state/binding authoring is a confirmed missing semantic tool,
owned by the [workflow task](../TODO/NS-4_agent-workflows_01.md). It must become
an ordinary MCP operation before agents can build the full bound demo directly.
After exporting or reviewing, undo the owned transaction only if its revision
and history still identify that edit; preserve intervening user work.

## Implement callbacks semantically

`nyx_pascal` inspects the accepted implementation of an exact Pascal handler
class; it does not read a pending draft. Start with
`{"mode":"inspect","handler":"TReplyBeforeTextInput","offset":0,"count":256}`.
The response contains revision, source line, immutable signature, text window,
total character count and next offset. Offsets count Unicode scalars on both
targets. Continue from `nextOffset` until `total`, requiring the same revision
throughout. Join the windows exactly, including whitespace and local declarations.
The signature is limited to 1024 characters with its full count reported; each
implementation window contains at most 4096 characters.

Apply a related set through one revision-aware operation:

```json
{"mode":"apply","expectedRevision":12,"operationId":"reply-validation-1","changes":[
  {"handler":"TReplyBeforeTextInput","expected":"<exact inspected implementation>",
   "implementation":"\nbegin\n\n  if AExecution.Cancelled or not AEvent.HasTextEdit then\n  begin\n    Exit;\n  end;\n\n  if NyxTextScalarCount(AEvent.TextEdit.After) > 40 then\n  begin\n    NyxEventResponse(AExecution).Consume;\n  end;\nend;"}
]}
```

There are 1..16 changes, at most 32768 Unicode scalars per expected/new text and
131072 across the group. Duplicate handlers, unknown fields, wrong types, stale
revisions, mismatched expected text and pending drafts refuse the entire group.
The full detached companion is admitted before one publication and ordinary
paired Undo step. A no-op retains history and Redo. Retries follow the same exact
actor/argument identity rules as other mutations. Results return handler names
and final source lines, rather than the whole source.

The typed Pascal boundary is `TNyxHandlerEdit` / `INyxHandlerPatch`; source
inspection returns an immutable `TNyxHandlerSource`. It owns text and borrows
no document or lexer. This operation retains the method signature, imports,
sibling helpers and managed view bytes exactly. Nested local routines and
declaration types are supported; ambiguous boundaries, duplicate implementations,
conditional directives and inline type blocks refuse. This is a bounded region
editor, not a Pascal type checker. Submit `nyx_build` to diagnose ordinary helper
syntax/type errors; the authored code remains available for correction or Undo.
End replacement text at the method's `end;`, with only optional trailing
whitespace. Keep comments inside the body so they cannot swallow a sibling helper.
Arbitrary imports, helper/class creation and full-unit edits remain separate
source workflow gaps. Read-only allows inspection; Disabled refuses both modes.

## Maintained authored-input review

Build `agents`, then run the Pascal coordinator against a disposable service:

```powershell
nyx_mcp_handler_tests <loopback-editor-base> <isolated-config.toml> <artifact-directory>
./tools/build.ps1 -Target agent-handler-consumers `
  -HandlerSourceDirectory <artifact-directory>/source `
  -BrowserOutput <isolated-web-root>
```

Append `phone` to the coordinator command for an exact 390-by-844 observer.
MCP composes the page, adds two callbacks, implements digit/Unicode-length
validation, requests actual browser/LCL builds and diagnoses an authored helper
error. The observing Nyx editor displays the code and activity; editor Undo and
semantic Redo restore both exact implementations together. The coordinator
selectively validates real browser host typing and accessibility values. It
never injects scripts or authors through the designer. Both independent control
consumers compile the exported companion unchanged; run the native consumer and
load `handler-consumers.html` for executed-browser evidence. The portable fixture
separately qualifies ownership, exact Unicode windows, failed groups and drafts.
These journeys mutate their chosen service; keep them separate from user work.

## Maintained callback review

Build `agents`, then run the Pascal journey against a disposable service with
isolated project/user configuration. It composes a page through MCP and authors
physical and semantic callbacks while an already-open Nyx Studio verifies
visible order, policy, source and editor Undo. It reviews removal and uses
semantic Redo. The native browser owner reads small published attributes and
captures the final UI; it injects no scripts and performs no designer authoring.
Supply isolated paths:

```powershell
nyx_mcp_callback_tests <loopback-editor-base> <isolated-config.toml> `
  <artifact-directory> <source-export-directory>
./tools/build.ps1 -Target agent-callback-consumers `
  -CallbackSourceDirectory <source-export-directory> `
  -BrowserOutput <isolated-web-root>
```

The journey exports exact `ordered/` and `removed/` units through bounded MCP
source windows at one revision each. Both compilers consume them unchanged.
The native consumer runs; load each generated browser consumer host as well,
using `?ordered` for the two-registration case. Actual control clicks execute
generated TODO classes through the UI queue. This proves compiled callback
construction/execution, not authored business behavior. The portable fixture
independently qualifies draft/refusal/receipt, inheritance, history and batch
limits on both Pascal targets. These fixtures alter their chosen service.

## Reviewed root cleanup

Inspect the active selection/view and paged roots first. Create owned demo pages
and definitions beside existing work using `nyx_transaction`; its ordinary delete
operation remains descendant-only. `nyx_roots` removes an explicit group of up to
sixteen page/reusable roots. It never cascades into roots omitted from the group.

```json
{"mode":"review","expectedRevision":7,"roots":[{"root":"page","id":"demo"},{"root":"component","id":"demo-card"}]}
```

Review returns an opaque `reviewID`, descendant and callback-registration counts,
retained reusable-reference counts and a warning. References from roots that
will remain block removal. References within the complete removal group are
allowed. Review changes neither revision nor Undo history, and is available in
read-only mode. Only eight immutable paired reviews are retained.

Apply the same roots, in the same order, at the same revision and actor, adding
`"mode":"apply"`, `"operationId":"unique-cleanup"` and the returned `reviewID`.
Apply requires editing permission and no pending draft. A changed project,
missing/wrong root, altered group, expired ticket or retained dependency refuses
without editing. Exact retries return the original receipt. The group publishes
one ordinary paired Undo step. Missing selection/view falls back to the first
surviving page, then reusable root, then an empty workspace.

Pascal imports, helpers, callback classes and document state defaults are
retained. Only their managed root/binding registrations are regenerated. Compile
after cleanup to check application code that mentions removed IDs. Independent
review workspaces, general helper/import editing and semantic state/binding
operations remain open; root cleanup alone does not provide those workflows.

Studio's Project panel offers **Remove active view**. Its Nyx confirmation shows
the same counts and warning, disables removal for retained references, and offers
**Keep view**. Confirmation rechecks the reviewed pair; normal Undo restores it.

Pascal controllers use `ReviewNyxRootRemoval(Session.ProjectSnapshot,
[NyxPageRoot('demo'), NyxReusableRoot('demo-card')])` from
`nyx.studio.rootedits`/`nyx.root.types`, then `Session.RemoveRoots(Review)`.
The immutable `INyxRootRemoval` owns its copied pair and roots, with no supplier
session or widget dependency. `TNyxDocument.RemoveRoot` is the structural primitive
for detached groups; its caller must validate the complete group before adopting
it. Studio controllers use the reviewed command for paired source/history.

The maintained `nyx_mcp_root_tests` coordinator takes editor URL, private Codex
configuration and export directory, plus optional `phone`. Run it against a
disposable service with `root-observer.html` available. It composes only through
MCP, checks an ordinary observing Studio's confirmation/activity/Undo, restores
through semantic Redo, and builds the cleaned application on both targets.
`tools/build.ps1 -Target agent-root-consumers -RootSourceDirectory <export>/source`
compiles that unchanged companion for independent browser/LCL consumers. Execute
the generated browser `root-consumers.html` too. These fixtures mutate their
chosen session; preserve the active user's service.

## Build and verification

`nyx_mcp_catalog_focus` composes every current catalog kind and a radio-peer
page through bounded, revision-aware MCP groups in a disposable service. It
queries small node/event responses, exports accepted source in 80-line windows
and requests both actual application compilers. Supply its private Codex config
and an owned export directory. Optional third argument `inspect` only rechecks
live publication and exports the same revision; it never repeats composition.

`tools/build.ps1 -Target catalog-focus -CatalogFocusSourceDirectory <export>`
compiles that unchanged companion for native and browser consumers. The native
consumer qualifies actual LCL focus/key slots and reports heap ownership. Run
`nyx_catalog_focus_cdp_tests <loopback>/catalog-focus.html <artifacts>` for real
browser Tab/F8, or add `phone` for exact 390-by-844 CSS-pixel emulation. It uses
the owned Pascal host transport to read bounded fixture attributes and never
injects browser source or edits Studio. All kinds and expanded compound parts
must agree with their published event families through read-only, disabled,
re-enable and independent-registration cancellation transitions. Date/time
inputs retain their bounded native shadow-segment Tab traversal. The separate
radio peer journey checks one entry through checked, disabled and hidden peers.
These fixtures mutate only their chosen disposable design session. They do not
establish hardware/IME, assistive technology, another widgetset or full grid
interaction support; those remain explicit qualification work.

The maintained native semantic/configuration tool is built with
`tools/build.ps1 -Target mcp-client`. Its configuration fixture uses disposable
files under `build/mcp-client/`, never the real Codex user file. Actual MCP HTTP
fixtures likewise run against an isolated service because they deliberately
replace and mutate their selected session. See the open
[primary semantic workflow task](../TODO/NS-4_agent-workflows_01.md) for richer source,
state/binding and review-session capabilities still missing from tools.

`tools/build.ps1 -Target agents` builds the portable fixture, real HTTP consumers,
browser observer/safeguard fixtures, Studio and preview program. `-BrowserOutput`
can stage browser artifacts independently of the live instance. Run the HTTP
fixture against a dedicated service, then the live observer coordinator at
desktop and exact 390-pixel widths. These fixtures modify their selected session;
use a review session rather than an unsaved working design.

The shared command boundary executes under native FPC and pas2js. LCL projects
the same token values and Nyx agent panel controls. The complete native Studio
controller remains an open product outcome. Semantic builds run in independently
owned compiler workers and release the document lock. The legacy main editor
HTTP route still serializes its own delegated builds, which can delay browser
observations; a global scheduler remains separate service work. Full native
Studio and production-scale rendering retain their product owners.

## Protected review workspaces

An authenticated agent can create an independent temporary review without
replacing the user's project, pending draft, selection or Undo/Redo history.
`nyx_reviews` has four strict modes: `list`, `inspect`, `create` and `discard`.
Lists contain only that transport's reviews. Creation uses the primary session's
exact revision and either `empty` or `accepted` as its base; accepted copies omit
the user's pending draft. Discard uses the review's own current revision. Both
mutations require a unique operation ID and return exact retry receipts.

```json
{"mode":"create","expectedRevision":6,"operationId":"new-workshop","label":"Input workshop","base":"empty"}
```

Use the returned `review` reference as an outer argument on ordinary semantic
tools. Inspect its own `nyx_session` revision before editing. Related changes
remain one ordinary `nyx_transaction` / paired Undo operation. The editor's
permission applies immediately to every review; an ID does not grant ownership.
Foreign, retired and empty supplied references refuse without falling back to
the user's project. Refusal revisions belong only to an admitted owned context.

```json
{"review":"<returned-reference>","line":1,"count":20}
```

The same outer reference routes callbacks, bounded Pascal implementation edits,
reviewed root cleanup, history, compiler jobs and selective rendered previews.
Compiler inputs are immutable owned pairs. A review's job cannot be queried in
another context; after retirement its completion cannot publish diagnostics into
the primary project. Two workers and sixteen retained job handles remain shared
budgets. At most eight live reviews and sixty-four lifecycle receipts per live
transport are admitted; creation reserves disposal capacity. Receipts are not
evicted to recreate a retired workspace on a delayed retry.

The ordinary **Agents** panel shows bounded review summaries and **Watch live
review**. This opens a Nyx-built observing view while retaining the user's full
editor. The view polls accepted design revisions and stops on retirement. It
does not execute compiled callback implementations; use `nyx_build` and the
actual compiled artifact for behavior. Temporary reviews retire on authenticated
transport teardown. User project lifetimes and full-editor project switching
remain the separate concurrent-project criterion; these preview links do not
satisfy it.

Reproduction uses Pascal fixtures and existing platform tools:

```powershell
./tools/build.ps1 -Target review-workspaces
```

This stages a server, browser Studio/viewers, portable checks and the native
MCP/observing-browser fixture under `build/review-workspaces/orchestrated/`.
It never launches a service or mutates a live project. Launch the server against
an independently owned `build/review-workspaces/.../stage` repository/configuration
and its staged web directory. Run `nyx_mcp_review_tests` with that editor URL,
its generated `stage/.codex/config.toml` and a private evidence directory. The
fixture deliberately establishes and edits its disposable user baseline; its
path guard refuses the production configuration. Preserve failed run artifacts.

```powershell
./tools/build.ps1 -Target review-consumers -ReviewSourceDirectory '<journey-directory>/source'
```

The second target compiles the unchanged bounded MCP export into actual LCL and
browser control consumers. Serve `review-consumers.html` over the staged service
and check its completion marker. The maintained journey also types through the
real compiled browser input, separately from the programmatic control fixture.
The evidence packet in WORK.md records native/executed-browser, real transport,
compiler, selective rendering and ordinary desktop/narrow Studio results.
