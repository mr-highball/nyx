# Agents in Nyx Studio

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
| `nyx_diagnostics` | Paged compiler diagnostics from the last editor build, with current-source navigation guards |
| `nyx_source` | A bounded range of accepted companion source lines |
| `nyx_transaction` | Atomic semantic operations against the accepted pair |
| `nyx_select` | Select a component, optionally activate a root view |
| `nyx_history` | Undo or redo one ordinary content command |
| `nyx_preview` | An immutable, revision-specific rendered view and optional PNG |
| `nyx_callbacks` | Grouped callback addition, policy, ordering and reviewed removal on the inspector's paired history |

Tool schemas advertise required fields and limits. Unknown arguments and
unpublished properties are refused. Queries never return the full document.
Outline/catalog/property/diagnostic pages contain at most 50 items. Node queries
can specify up to 20 exact property `keys`; `textOffset` and `textLimit` retrieve
Unicode scalar slices (up to 2048 scalars), with a total and truncation flag.
Source queries return up to 80 lines. Structured context is capped at 48 KiB;
request fewer items, properties or lines when that budget is exceeded.
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
controlRadius and fontSize; null restores a default. Custom token families and
callback authoring operations remain future extensions to the focused tool set.

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
logic. Semantic source/body editing remains an explicit workflow gap.

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

## Build and verification

The maintained native semantic/configuration tool is built with
`tools/build.ps1 -Target mcp-client`. Its configuration fixture uses disposable
files under `build/mcp-client/`, never the real Codex user file. Actual MCP HTTP
fixtures likewise run against an isolated service because they deliberately
replace and mutate their selected session. See the open
[primary semantic workflow task](../TODO/NS-4_agent-workflows_01.md) for source/body,
build, state/binding and review-session capabilities still missing from tools.

`tools/build.ps1 -Target agents` builds the portable fixture, real HTTP consumers,
browser observer/safeguard fixtures, Studio and preview program. `-BrowserOutput`
can stage browser artifacts independently of the live instance. Run the HTTP
fixture against a dedicated service, then the live observer coordinator at
desktop and exact 390-pixel widths. These fixtures modify their selected session;
use a review session rather than an unsaved working design.

The shared command boundary executes under native FPC and pas2js. LCL projects
the same token values and Nyx agent panel controls. The complete native Studio
controller remains an open product outcome. The current main editor HTTP service
serializes delegated builds, so an in-progress compilation can delay browser
observations even though the MCP listener is separate. This delivery is not a
claim of full native Studio, threaded compilation, or production-scale rendering.
