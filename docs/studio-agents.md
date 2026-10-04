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

Other MCP clients can use the URL and Authorization header from that same local
configuration. No Node process, package manager or JavaScript service is needed.
The backend, model, tool implementation, browser controller and preview program
are all Pascal; browser JavaScript is compiler output.

## Semantic context

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

## Build and verification

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
