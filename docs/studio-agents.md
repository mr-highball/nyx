# Agents in Nyx Studio

The current observing release advertises twenty-two tools, including protected
reviews, project workspaces, state, collections, typed value-domain policy,
presentations and menu declarations. Authenticated Pascal MCP drives bounded
composition, paired source/history and real browser/LCL application/view builds.
The current native checkpoint retains all nine exact pairs, full histories and
ordinary handles through delivery. Earlier explicit disposable-test bootstrap
and production legacy migration remain separately qualified. Launch refreshes
project and enrolled-user Codex configuration. Native named handles are now
authenticated in this chat: an owned image review exercises bounded composition,
paired Undo/Redo, exact source export, browser/LCL compilation and an actual
rendered preview. The Pascal client below remains the maintained owner for
transport qualification. A different chat may still need to reconnect after
credential rotation. See
[current observing evidence](../WORK.md#installed-current-source-return--2026-10-08).

Studio starts with agent access enabled and editing allowed. Open **Agents** to
see the shared revision, connected endpoint and recent operations. **Read only**
keeps semantic queries available; **Disabled** refuses agent queries and edits.
These operator controls apply immediately. Fresh runtimes begin enabled; durable
runtime recovery retains the saved permission. Agents cannot change
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

Prepared resource source adds `nyx_resources`: bounded variant metadata, shared
category/source/locale/search/tag discovery, exact paged creator labels,
Unicode/byte windows, structural JSON paths and selected-owner binding context.
Optional revision pins keep metadata/tag pages consistent. Payload-free
`set-labels` changes preserve the exact variant's contents/cache/fallback/help
and use the existing paired admission path; discovery never changes editor filters.
Resource definitions and dependent scalar binding repairs use one paired Apply,
or an `op: "resources"` group in `nyx_transaction`. Existing operator policy,
transport authority, revision, retry receipts and workspace/review routing apply.
Hosted queries identify authored fallback and perform no network loading.
See [the contract](resources.md#semantic-resource-authoring) and run
`tools/build.ps1 -Target resource-workflow` for suspended-engine/source checks.
The protected deployed endpoint retains its existing tool set; local source
qualification does not establish authenticated rollout or observing Studio.

Prepared runtime inspection also exposes one exact resource variant through
`nyx_resources` mode `runtime`: expected design revision, run sequence, resource
reference and optional locale. It returns a bounded status item or null membership,
without payload or reload/cancel authority. Exact reads and pages are mutually
exclusive. Supporting ordinary editors negotiate the selection capability and
follow Resources Open/New intent, preserving queued document work; older peers
keep summary requests. See [runtime observations](resources.md#runtime-resource-observations)
for the wire and public typed cards. The maintained `resource-observations` target
qualifies independent source sessions and real controllers, with separate explicit
HTTP browser execution. It does not enroll clients or replace the installed server.

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

Current-source ordinary dispatch binds successful mutation receipts and callback/
root removal reviews to the authenticated connection owner, separately from its
visible actor name. Two connections with the same display name have independent
operation IDs and tickets; renaming that name preserves the original connection's
receipts. A foreign ticket refuses before changing either paired file or history.
Private reviews additionally require their owning connection. The same contract
applies to primary and explicitly routed project sessions. Compiler build requests
have a separate receipt path whose primary-agent owner propagation remains open.
Installed older-server and observing HTTP qualification remain recorded separately
in WORK.md.

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
| `nyx_build` | Output readiness, bounded project job discovery, immutable accepted builds, owned cancellation, artifact/diagnostic inspection and exact-job observing preview launch |
| `nyx_pascal` | Bounded callback/import context, exact guarded callback edits and grouped typed import changes |
| `nyx_roots` | Reviewed removal of exact page/reusable groups on paired Undo history |
| `nyx_state` (staged) | Bounded scalar defaults, exact text windows and contextual bindings; grouped typed state/binding changes |
| `nyx_collections` (staged) | Paged collection/schema/row/domain/view context and grouped typed collection commands |

Tool schemas advertise required fields and limits. Unknown arguments and
unpublished properties are refused. Queries never return the full document.
Outline/catalog/property/diagnostic pages contain at most 50 items. Node queries
can specify up to 20 exact property `keys`; `textOffset` and `textLimit` retrieve
Unicode scalar slices (up to 2048 scalars), with a total and truncation flag.
Source queries return up to 80 lines. Structured context is capped at 48 KiB;
request fewer items, properties or lines when that budget is exceeded.

`nyx_state` has four focused modes. `defaults` pages at most 50 authored scalar
defaults, optionally filtered by an exact case-sensitive name substring. Text
previews contain at most 80 Unicode scalars. `value` reads one scalar; text uses
`offset` / `count` windows of at most 4096 scalars, preserving supplementary text
and embedded NUL. Nontext values retain their Boolean, signed Integer or finite
Double type and refuse text-window arguments.

`bindings` takes one exact authored `owner`, including an existing named-part
override, and pages supported targets and scalar kinds. Each row distinguishes
the local descriptor, effective inherited binding and deliberate local clearing.
These queries leave operator selection, view, source and history unchanged.

`apply` accepts 1..32 ordered `changes`, current `expectedRevision` and a unique
`operationId`. Supported changes are `create`, `set`, `rename`, `remove`, `bind`,
`clear-binding` and `inherit-binding`. Every scalar operation carries its exact
`kind`; create/set primitive types must match it. Rename migrates authored
references across pages and reusable definitions. Clear masks inheritance;
inherit removes a local descriptor. Clear dependent bindings before removing a
used default. Wrong families, missing owners/references, unsupported targets,
unknown fields and pending drafts refuse the whole group. One admitted group
publishes one ordinary paired source/design Undo step, retains unrelated helpers
and records activity. Exact authority/operation/argument retries return the
original receipt. Optional workspace/review routing follows the existing context
contracts; it never follows the observing user's navigation.

The typed Pascal command is `NyxStateBindingPatch`, using `NyxCreateDefault`,
`NyxSetDefault`, `NyxRenameDefault`, `NyxRemoveDefault`, `NyxBindControl` and
`NyxInheritBinding`. Assignments use the four typed `NyxStateValue` overloads;
open state names and binding owners are distinct references. Binding overloads
derive their scalar family from the typed reference, for example:

```pascal
NyxStateBindingPatch([
  NyxCreateDefault(NyxStateValue(NyxTextState('reply'), 'Ready to compose.')),
  NyxBindControl(NyxBindingOwner('reply-memo'), bpValue,
    NyxTextState('reply'), bdTwoWay)
]);
```

This packet covers
scalar defaults and existing authored owners. Structured collection authoring,
creation of reusable overrides, general source/import editing and asynchronous
state/binding/event inspector routing retain their original workflow owners.

Run `tools/build.ps1 -Target state-bindings` for the maintained offline semantic,
discovery and exact compiled native-control checks. Browser semantic/control
consumers and matching RTL are staged under `build/state-bindings/browser`;
their compilation does not establish execution or authenticated new-tool access.

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
`view`. Browser and `lcl` outputs use the same fixed-argument compiler service.
Both current Studio controllers consume the semantic job workflow. Request requires Allow edits, exact revision
and output identity, and no pending draft. It captures an independently owned
accepted pair/profile and immediately returns an immutable queued job receipt. Compilation
does not hold the document lock, replace source, change selection or create Undo
history. Editing can continue while the captured pair compiles.

Query `{"mode":"status","job":"<returned job>","limit":5}`. States are
`queued`, `running`, `cancelling`, `succeeded`, `failed` and `cancelled`.
Queued/cancelling remain active; only a joined worker can become terminal.
`failure` distinguishes `none`, `compiler`, `time-budget` and `log-budget`.
Diagnostics page by `offset`/ `limit`
(at most twenty) with stable error/fatal, warning, then informational order.
Add `"severity":"error"` to retrieve only errors; other closed filters are
`all`, `fatal`, `warning`, `hint`, `note` and `info`.
`diagnostics.total` is the filtered total, `available` the whole report.
Locations use Unicode scalar coordinates in the exact submitted companion.
`navigable` is false if a later edit/draft changed that pair. Successful jobs
include served artifact paths plus byte lengths/fingerprints covering runtime,
compiled companion and saved scoped design. Failures expose no successful artifact.

Discover `{"mode":"jobs","filter":"active","offset":0,"limit":5}` before
selecting a job. The default is active, offset zero, limit ten; `all` includes
retained terminal handles. Offset is 0..16 and limit 1..16. Context filtering
precedes totals and paging, so primary projects, named projects and private reviews
never leak each other's handles or counts. Counts describe queued, running and
cancelling jobs in that exact context. Items expose only job identity, display
actor, revision, state/failure, target/scope/view, source/output currentness and
`canCancel`. Source, logs, artifacts, private connection owners and machine
profiles remain separate. Read-only callers may inspect; only admitting agents
with Allow edits or trusted project operators receive cancellation capability.
Portable callers use `NyxCompilerJobs` with the closed `TNyxCompilerJobFilter`.

The original revision, source/design fingerprints, `outputID`, target and scope
remain attached to the job. `currentRevision`, `currentSource` and
`currentOutput` distinguish the present editor/profile. Undo may restore the
exact pair despite a newer monotonic revision. Fingerprints are explicitly MD5
optimistic byte identities, consistent with existing project revisions; they
are not authentication credentials. Current-pair/profile and retry checks compare
complete exact text. Profiles retain configured fields, not locked compiler binary
snapshots; toolchain pinning/caching keep their service owners.

Exact connection/authority/project/operation/argument retries return the initial receipt without another
compiler invocation, including after the editor revision or profile changes.
Display renaming preserves authority; same-name connections remain independent.
Different arguments with an accepted operation identity refuse. Sixty-four build/cancel
receipts and sixteen job handles are retained per server session; oldest terminal
handles expire first. An expired handle is reported explicitly and never silently
resubmitted. Two semantic jobs run and eight await a slot in FIFO order. Host
request/status/completion polling advances the queue; it does not run detached
from the host's serialized observation lifecycle. A full ten-job budget refuses
before spawning. Shutdown signals every owned job before joining any one.

Cancel with `mode: cancel`, the retained `job`, the project's current
`expectedRevision` and a fresh `operationId`. Agents require Allow edits and the
connection that admitted the job. Project/review identity must match exactly.
A queued cancellation constructs no worker; a running cancellation returns
`cancelling`, then `cancelled` after the owned compiler and worker join. Terminal
cancellation is a no-op. Exact retries return the original receipt. Cancellation
does not change the document revision, accepted Pascal, draft or Undo history,
and a cancelled completion leaves the previous compiler report/preview retained.
An earlier-source job may be explicitly cancelled using the current revision;
editing alone does not automatically cancel a build. Portable callers use
`NyxCompilerCancel` with distinct job/operation references; the editor bridge
captures its current revision in `CancelBuild`.

Current ordinary Studio has a shared Nyx Builds panel with bounded active rows,
counts, earlier-source/output labels and exact-job Cancel actions. The host's
separate `buildJobControl` capability gates this UI against older services.
Cancellation can queue behind compiler-only polling, but pending local capture,
document/history operations, conflicts or an unacknowledged editor frame refuse.
Cancellation acknowledgments never advance another request/status/preview stage.
Starting or cancelling another job retains the accepted artifact; native Run
rechecks that artifact's own job rather than the most recent pending job.
Completed reports require both the exact accepted pair and captured current
output before publication. Earlier output settings cannot replace the accepted
report merely because the source still matches.

The current-source native executor checks deadlines/cancellation within pipe
draining and joins its exact process on every exit. On Windows each invocation
also owns an unnamed, non-inherited OS job: the compiler starts suspended, is
assigned before resume and does not allow breakaway. Its descendants stay owned
after the compiler exits. Normal success waits for the whole family; failure or
cancellation retires all members before the worker joins or its slot is reused.
Incompatible job admission refuses instead of launching an uncontained compiler.
These semantics follow Microsoft's [job-object contract](https://learn.microsoft.com/en-us/windows/win32/procthread/job-objects).
Log caps retain complete UTF-8 scalars. An OS that refuses reaping/accounting keeps
the worker active instead of publishing a false terminal result. Actual family
retirement is qualified on Win32; other hosts retain direct-process retirement.
The two-call suspended creation/assignment window is not crash-atomic; atomic
job-list creation and additional host qualification remain hardening work.
Current-source native semantic discovery and ordinary Win32 controls are qualified;
the browser controller uses the same asynchronous contract and has an actual
browser control journey with a scripted compiler seam. This does not qualify its
authenticated HTTP cancellation/completion journey or update protected older
services. The legacy synchronous HTTP route remains available to older clients;
current browser Studio uses semantic jobs. See
[current control evidence](../WORK.md#studio-build-controls--2026-10-06).

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

### Exact-job observing preview launch

Prepared source adds `launch` and `launch-status` to the existing `nyx_build`
tool. A launch requests execution of one already successful job in its ordinary
project. It performs no compilation, source changes, history changes or editor
navigation. Query status and currentness first, then request the exact job:

```json
{
  "mode": "launch",
  "job": "<successful job reference>",
  "expectedRevision": 7,
  "operationId": "preview-settings-page"
}
```

Supply `workspace` for an independent project, as with other compiler operations.
The job must belong to that project and its exact accepted source, revision and
machine output. Pending drafts, stale outputs, failed jobs and foreign contexts
refuse. Page and reusable builds additionally require their built root to be the
project's active view. Application builds do not change the observer's active
view. Allow edits is required; agents cannot enable themselves. Review workspace
runtime mounting remains unsupported and refuses explicitly.

Pascal clients use the same typed contract:

```pascal
LArguments := NyxCompilerLaunch(
  LSuccessfulJob, LRevision, NyxBuildOperation('preview-settings-page'));
```

The bounded receipt has a monotonic `sequence`, exact job/revision/output,
scope/root/target, display actor, `state` and nullable `browser`/`lcl`
acknowledgments. It contains no source, artifact manifest, machine paths or
runtime credential. `{"mode":"launch-status"}` queries the latest intent for
that project. `requested` means accepted intent; `mounted` means the adapter
placed a browser preview or activated its owned native process. Neither proves
successful application behavior. Actual input or selected rendering remains
necessary for that validation. Each adapter field is its latest acknowledgment,
not a census of every observing window.

An observing Studio consumes each sequence once through its ordinary compiler
and preview controller. It waits for synchronized source and clean output
configuration; hidden native projects do not execute until observed. A target
mismatch reports `unavailable` rather than running a different platform's build.
Native preparation downloads and verifies the exact executable and rechecks
currentness before activation. Browser mounting reuses its persistent compiled
surface. Runtime producer grants remain on the private editor channel; MCP
cannot mint or read those grants. Requested/mounted/refused activity is visible
in Studio's Agents panel and footer.

An exact retry on the same authenticated connection/context returns its original
receipt without another launch. Reusing that operation ID for changed arguments
refuses. A new operation ID deliberately requests a new runtime, even for the
same job. Source/draft/revision, scoped-view, output or permission changes retire
old intent permanently; restoring permissions or a profile does not replay it.
Already running previews retain their normal separately owned lifecycle.
Retirement does not stop an operator's process by inference.

The transient mailbox retains at most 16 context slots and 64 retry receipts.
Capacity refuses before replacement. Authenticated transport deletion frees its
unreachable retries; operator-confirmed project closure frees that context and
its retries. Server restart forgets all launch intents and cannot replay them
from the durable project checkpoint. Observer sequence tracking belongs to the
admitted backend endpoint: reconnecting to a different backend resets that
tracking without replaying the old backend's intent. Actual backend replacement
qualification is recorded separately from ordinary same-endpoint execution.
No new MCP tool or background service is required. The installed frozen release
remains separate from this prepared
source; see WORK.md for current execution and deployment evidence.

## Value domains

Current source extends existing tools rather than adding a separate policy
service. These capabilities remain staged until the protected observing release
is updated; inspect actual discovery before using them on a running endpoint.

Request `nyx_node` with `valueDomain: true` and `domainScope: "local"` or
`"effective"` (default). The response includes `localDeclared`, `defined`, scalar
`type`, `format`, exact inclusive `minimum`/`maximum` or null, `totalChoices`,
`offset`, and bounded `choices`. Local absence differs from an explicit NoValue
mask. `domainOffset` is 0..128; `domainLimit` is 1..16 (default 8). Text choices
reuse `textOffset`/`textLimit` Unicode-scalar windows (default 80 for domains),
with index, totalScalars and truncated metadata; numeric/Boolean choices keep
their exact primitive type. Omission returns no domain context.

Clock format `time` adds five small fields: `stepDeclared`, native JSON `step`
(positive signed Integer milliseconds, `"any"`, or null when absent), exact
`stepMilliseconds` (zero for unrestricted), wire-precise `stepBase` (minimum,
otherwise `"00:00"`) and `crossesMidnight`. Minimum and maximum are independent;
neither one-sided query fabricates the other endpoint. Clock choices use the
same bounded text windows. This source/local-session extension does not change
the frozen deployed endpoint; authenticate actual discovery before using it.

Group policy changes with other related operations in one `nyx_transaction`,
supplying the exact expected revision and unique retry identity:

```json
{
  "op": "value-domain-set",
  "id": "arrival",
  "domain": {
    "type": "text",
    "format": "date",
    "min": "2026-10-01",
    "max": "2026-10-31",
    "choices": ["2026-10-06", "2026-10-09", ""]
  }
}
```

This JSON is an explicit transport boundary; internal commands and generated
Pascal use typed domain/date/clock builders. Scalar types are text, Boolean, integer
and number. Calendar format is canonical Gregorian date text or empty. Calendar
and numeric bounds are paired, ascending and inclusive; numeric bounds retain numeric types. Choices
are unique, 1..128, exact and compatible with bounds. Unknown fields, impossible
dates, wrong control families and incompatible defaults refuse the complete
group without advancing history. Setting changes only the owner's local value
contract, retaining fields/events/bindings/defaults. Group dependent value updates
in the same candidate when needed. `{"op":"value-domain-inherit","id":"arrival"}`
removes an existing local declaration; missing declarations refuse. Each authored
reusable override remains independent. The ordinary Inspector uses the same typed
command and paired source/history path. See [usage](date-fields.md#studio-and-semantic-constraint-authoring).

Clock domains use `"type":"text","format":"time"` and exact
`HH:MM[:SS[.fff]]` bounds/choices. Bounds are independently optional; a reversed
pair spans midnight. An optional `step` uses positive Integer milliseconds or
`"any"`, based on the minimum or midnight. Omission retains no step declaration.
Choices are unique by reading, so `09:00:00.1` and `09:00:00.100` duplicate.
Empty text is an optional empty choice. The public typed `NyxSetValueDomain`
clock overload and ordinary Nyx-built policy form reuse the existing complete
candidate and paired Undo authority. Current source preparation passes local
semantic and Win32 form/queue checks; authenticated current-backend authoring,
executed browser/observing Studio and rollout still require qualification. See
[the public editor](time-fields.md#reusable-constraint-editor-and-studio).

## Reusable components

The source candidate adds four operations to `nyx_transaction`; the protected
LAN release retains its older vocabulary. `nyx_node` accepts `parts: true` with
`partOffset` and `partLimit` (20 default, 50 maximum). This returns only reachable
effective paths, control kind, exact authored source/design identity and the local
override descriptor ID. Properties and events have independent paging. `.` is
the root. Removed parts are absent; inspect the referenced definition separately
to find its inherited paths. Duplicate sibling names refuse as ambiguous.

- `derive` supplies `source`, a new reusable root `id`, and `identities`, an exact
  object mapping **every descendant** source ID to a new ID. Exclude the root.
  Foreign, omitted, occupied or duplicate destinations refuse. Derivation copies
  the subtree independently, retains named parts/contracts/bindings/callbacks and
  existing reusable references, and preserves Pascal helpers. It neither replaces
  the original subtree nor guesses references inside opaque extension data.
- `instance` supplies a new control `id`, exact reusable `component` and owning
  `parent`; optional `index` is an exact nonnegative insertion position. It adds
  an owned reference rather than duplicating the definition.
- `override` supplies exact `instance`, descriptor `id`, named `path` and closed
  `mode`: `properties`, `append`, `prepend`, `replace` or `remove`. A repeated path
  must retain the descriptor ID. Use ordinary `update` for typed properties and
  `create`/`move`/`delete` for payload children in the same transaction. Empty
  append/prepend, missing/extra replacement payloads and invalid paths refuse.
- `inherit` supplies exact `instance`, descriptor `id` and `path`. It removes that
  local rule and its payload, restoring the definition. Missing or changed rule
  identity refuses. Definitions, siblings and retained Pascal handlers stay owned.

All related operations publish as one paired design/Pascal Undo step under the
existing revision, permission, draft and connection-owner-bound receipt checks. Failed
candidate admission retains the complete pair/history. A copied named application
state binding remains deliberately shared; structural customization does not
implicitly create a new state namespace.

To promote an owned subtree in place, group `derive`, descendant `delete` and
`instance` together. The new instance may reuse the removed subtree's exact ID
and insertion position, while the new definition/descendants receive their own
identities. Root removal still requires the existing reviewed `nyx_roots` flow.

Pascal editor authors use `NyxReusablePatch`, `NyxDeriveComponent`,
`NyxInstantiateComponent`, `NyxOverrideComponentPart` and
`NyxInheritComponentPart` from `nyx.studio.edits`. `NyxControl`, `NyxComponent`,
`NyxPart` and `NyxIdentity` keep their reference families distinct; override
choices use `TNyxOverrideMode`. The ordinary inspector's Create component command
uses this same derivation engine. Portable `CloneNyxReusableDefinition` in
`nyx.composition` borrows a document/subtree and returns an independently owned
definition; the caller admits or releases that copy.

`tools/build.ps1 -Target reusables` qualifies semantic publication/refusals,
actual discovery and unchanged generated controls, including compiled callbacks,
and stages browser consumers without launching a listener or replacing an active
project. Browser compilation is separate from actual browser execution; current
authenticated deployment and broader editor/creator parity remain open in WORK.md.

## Edits and history

The source candidate also accepts an exact document palette operation:
`{"op":"theme","values":{"accent":"#35c3a5","fontSize":16}}`. Its seven
color roles require defined `#RRGGBB` values; radii are integers 0–1000 and font
size is an integer 1–256. Unknown roles and implicit scalar conversions refuse.
Unlike the existing `tokens` merge boundary, `theme` replaces the whole local
declaration: omitted roles inherit the independent host base. `values:null`
removes it; an empty object remains an explicitly empty declaration. Group it
with related edits in one revision-aware transaction and paired Undo operation.
Discovery advertises the closed role schema. The protected observing server
retains its prior API until an admitted rollout; the connected English review
seed therefore used its existing `tokens` operation. This candidate's exact
operation is exercised through the Pascal semantic dispatcher without claiming
an upgraded live endpoint. Studio consumes the [same typed theme contract](themes.md).

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

A transaction contains 1–64 total changes. Existing design operations can
interleave `{"op":"state","changes":[...]}` and
`{"op":"collections","changes":[...]}` groups. Their changes use the same
strict typed shapes advertised by `nyx_state` and `nyx_collections`; each
group retains its 1–32 limit, and **every nested change counts** toward 64.
Create a control, define its default, bind it, and move it into another layout
in one mutation with one exact paired Undo/Redo. Data and design groups run in
order: each complete intermediate must admit, so define dependencies before
binding and remove dependent owners/bindings before deleting their defaults.
Consecutive design operations share ordinary grouped admission.

The public Pascal contract uses copied typed steps, including existing placement,
reusable, content, menu and presentation patches:

```pascal
LTransaction := NyxProjectTransaction([
  NyxDesignStep(NyxPlacementPatch([
    NyxPlaceNewControl(nkMemo, NyxControl('reply-memo'),
      NyxControl('home'), nplInside)])),
  NyxStateStep(NyxStateBindingPatch([
    NyxCreateDefault(NyxStateValue(NyxTextState('reply'), 'Ready to compose.')),
    NyxBindControl(NyxBindingOwner('reply-memo'), bpValue,
      NyxTextState('reply'), bdTwoWay)]))
]);
```

`INyxProjectTransaction.ToData` supplies the existing tool's `operations`
argument; still provide the current expected revision and unique operation ID.
`Candidate` stages an independent complete pair; an embedding controller may
publish that pair once through ordinary Studio history. No intermediate candidate
history escapes. Pending drafts, late failures, stale revisions and foreign retry
authorities retain the accepted pair/navigation/history. Nested transaction
wrappers, permission changes and handwritten source edits are outside this
contract. Root removal still requires the actor-bound `nyx_roots` review.
Built-in design patches expose `INyxDesignChanges` through
`NyxDesignChanges`. The original `INyxDesignPatch` candidate-only interface
remains unchanged for custom ordinary commands; semantic composition requires
the optional strict snapshot capability.

The maintained English companion now composes its table, bound workspace-note
input and both default families in one authenticated mutation. Current isolated
qualification verifies exact paired Undo/Redo and all four browser/LCL application/
view compiler inputs. Native table controls execute separately; browser execution
and full observing Studio retain the boundaries recorded in WORK.md.

`tools/build.ps1 -Target project-transactions` runs checked shared admission and
exact compiled-source/runtime projection, builds the semantic English table
companion, and stages matched browser consumers. It starts no service/browser
and supplies no browser execution or observing qualification. The new grouped
contract requires a backend built from current source; frozen older servers do
not acquire it from local compilation. Browser readiness and LAN refresh retain
their separately recorded gates in WORK.md.

Creation supports child placement or a new page/reusable root; these
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
Operation identities are scoped to their authenticated connection owner. A retry
from another connection follows ordinary revision admission and cannot obtain the
first connection's cached result, even when both use an identical display name.

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
authenticated connection owner, revision and change bytes; altered identities or stale reviews refuse.
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
Unconditional namespace imports now have the semantic modes below. Helper/class
creation and full-unit edits remain source workflow gaps. Read-only allows
inspection; Disabled refuses every mode.

### Pascal imports

Use `nyx_pascal` with `mode: imports`, an explicit `section: interface` or
`implementation`, and optional `offset`/`limit`. It returns the current revision,
total/next offset and at most 50 namespaces with their actual one-based source
lines. The default page contains 20. Accepted imports remain inspectable while a
draft is pending; the response reports that draft. No source/comments or complete
unit text are returned. The existing 48 KiB response budget still applies.

`mode: edit-imports` takes current `expectedRevision`, unique `operationId` and
1..32 ordered `changes`. Each change contains exactly `op: add/remove`,
`section: interface/implementation` and `unit`, an ordinary Pascal namespace of
at most 120 ASCII characters. Unit identity is case insensitive. Add requires
absence and appends without reordering existing imports; Remove requires presence.
Removing the final namespace retires its uses clause. Ordinary comments, including
comments between namespace parts, retain their exact bytes. Separator indentation
can retire with a removed final import so the remaining line stays readable.
An absent clause can receive its first unit. File clauses, compiler options,
source payloads, conditional/directive clauses and duplicate namespaces refuse.

The public Pascal contract is `TNyxPascalUnitRef`, `TNyxImportSection`,
`TNyxImportAction`, `TNyxImportEdit` and managed `INyxImportPatch`:

```pascal
LPatch := NyxImportPatch([
  NyxImportEdit(nisImplementation, niaAdd, NyxPascalUnit('Math')),
  NyxImportEdit(nisImplementation, niaAdd, NyxPascalUnit('SysUtils'))
]);
```

The immutable group borrows no editor, node or lexer. It applies through the
ordinary source reader on an independent session and retains the exact design,
helper bodies, callback signatures and managed builder. A late refusal discards
the whole group; successful publication adds one paired Undo step. Pending drafts,
revision/permission/transport authority and exact retry receipts reuse the existing
Pascal boundary. Workspace/review references stay on the outer request only.
Import resolution and helper syntax/types require ordinary `nyx_build` diagnostics;
source admission does not establish compilation or execution. This mode changes
no target profile or unit search path. General helper implementation editing now
has the modes below; declaration/class/full-unit authoring and authenticated
updated observing deployment remain unqualified.

### Handwritten Pascal helpers

Use `nyx_pascal` with `mode: routines` and optional `offset`/`limit` to discover
accepted top-level implementations in declaration order. The default page has
20 entries; the maximum is 50. Each row has an exact qualified `routine`, closed
`kind` (procedure/function/constructor/destructor), one-based `line`, `editable`
and a refusal `reason`. Class functions retain their complete prefix. Escaped
Pascal identifiers retain their authored ampersands in the signature and use
ordinary unescaped qualified identities in references. Nested
local routines belong to their enclosing implementation. Type member/procedural
declarations, literals and comments never become independent edit targets.
Discovery admits at most 4096 entries and preserves selection/history.

`mode: routine` takes one case-insensitive qualified name, `offset` and `count`.
Offsets/counts address Unicode scalars; at most 4096 are returned. Concatenate
`text` windows only at the same `revision`. The response includes the exact
signature through its first semicolon (up to 1024 scalars, with its full size),
editable status/reason, source line and `pendingDraft`. The editable text starts
after that semicolon and includes whitespace, local declarations/nested routines
and the complete implementation through `end;`. Forward/external and conditional
entries can be inspected when uniquely identified, but cannot be edited.
Overloaded/duplicate names appear in discovery with refusal reasons; inspection
by an ambiguous name refuses instead of choosing a signature.

`mode: edit-routines` takes current `expectedRevision`, unique `operationId` and
1..16 changes containing exactly `routine`, `expected` and `implementation`.
Expected text must exactly match the accepted window sequence. Each expected or
proposed field permits at most 32768 Unicode scalars; the whole patch permits
131072. Each distinct routine occurs once. No-op patches, stale text, pending
drafts, directives, guessed conditional/overload ownership and compiler-managed
`BuildNyxDocument`/`BindNyxCallbacks` targets refuse. End text at the final
semicolon with only optional trailing whitespace; sibling declarations and
trailing comments cannot enter the replacement.

The public contract uses `TNyxRoutineRef`, immutable `TNyxRoutineSource`/
`TNyxRoutineCatalog`, `TNyxRoutineEdit` and managed `INyxRoutinePatch`:

```pascal
LPolicy := ReadNyxRoutineSource(LSource, NyxRoutine('TNotePolicy.Limit'));
LPatch := NewNyxRoutinePatch([
  NyxRoutineEdit(LPolicy.Routine, LPolicy.Code, LNewImplementation)
]);
```

Candidates reconstruct the ordinary complete source/design contract and require
exact retained design. Only one final publication adds a paired Undo step;
late group refusal preserves the entire pair, navigation and history. Signatures,
imports, surrounding helpers/comments, callback registrations and managed views
stay owned. Existing operator permission, revision, transport authority, exact
receipt and outer workspace/review routing guards apply. Read-only permits
bounded inspection; Disabled refuses access. These three modes extend the same
nineteen-tool source catalog; the protected release/current chat still has fifteen
tools and its older Pascal schema. Compilation/type correctness and actual helper
execution require `nyx_build` and target controls, not source admission. The
declaration boundary below supplies helper creation/removal. Signature/class/
full-unit changes and updated authenticated observing deployment retain their
open workflow owner.

### Helper declarations and grouped source composition

`nyx_pascal` now also has `mode: declaration` and `mode: edit-declarations`.
They extend the existing nineteen-tool source catalog to nine Pascal modes;
the protected release/current chat still has fifteen tools and its older schema.

`declaration` takes an exact unit-helper `routine`, optional `part` (interface
by default, or implementation), and Unicode-scalar `offset`/`count` windows of
at most 4096. It returns the accepted revision, visibility, part, actual source
line, total/next offset, exact signature text and pending-draft status. Private
helpers have an empty interface counterpart and line zero. Implementation
signature windows are independent of the older 1024-character routine preview,
so every exact removal field can be inspected without a whole-unit dump.
Concatenate windows only at the same revision. Interface and implementation
signatures must match lexically, ignoring ordinary comments/case/whitespace;
ambiguous overloads, conditional ownership and additional interface modifiers
require the broader source workflow.

`edit-declarations` takes current `expectedRevision`, unique `operationId` and
1..16 ordered action-specific changes:

- `create`: `routine`, closed `kind: procedure/function`, closed
  `visibility: interface/implementation`, exact `signature` and `implementation`.
  Public helpers create both counterparts; private helpers create only the
  implementation. Signatures are explicit Pascal fragments ending at their
  first semicolon, with no surrounding code. Created bodies contain one complete
  implementation. Existing routine identities, mismatched names/kinds, directives
  and sibling injections refuse.
- `edit`: `routine`, exact `expectedImplementation` and new `implementation`.
  This reuses guarded routine replacement, including qualified method bodies,
  while keeping existing signatures.
- `signature`: the same typed declaration fields as `create`, plus exact
  `expectedSignature`, `expectedImplementation` and `expectedDeclaration`.
  An ordinary helper's parameter/result contract and complete body change
  together; a public helper's interface counterpart changes in the same group.
  The identity and visibility must stay the same. Include explicit related
  caller body edits in this ordered group; Nyx does not guess caller rewrites.
  Exact acknowledgement, conditional/overload ownership and no-op refusal
  apply before independent admission and one paired Undo publication.
- `remove`: `routine`, exact `expectedSignature`, `expectedImplementation` and
  `expectedDeclaration` (empty for a private helper). Removal retires its owned
  implementation and interface together. Possible retained identifier references,
  including compiler directives, block removal. Comments/literals are otherwise
  not treated as callers. Lexical shadows/member names can be ambiguous and
  conservatively refuse; this does not claim Pascal type resolution.

Creation/removal/signature replacement concern ordinary unit helpers. Class-member signatures and
compiler-managed BuildNyxDocument/BindNyxCallbacks remain protected. Definition
ordering retains successive helpers before class/managed consumers and can
choose an earlier safe site for a possible existing caller. Attached inline
comments and leading comments of the following declaration stay beside their
original source. Conditional insertion gaps refuse. Removed comments inside
explicitly acknowledged signature/body ownership retire with that code;
surrounding comments remain.

The public contract uses `TNyxRoutineDeclaration`, `TNyxRoutineVisibility`,
typed `TNyxDeclarationPart`, immutable `TNyxRoutineDeclarationSource`,
`TNyxDeclarationEdit` and managed `INyxDeclarationPatch`:

```pascal
LPatch := NyxDeclarationPatch([
  NyxCreateDeclaration(NyxRoutineDeclaration(nrFunction,
    NyxRoutine('TextBudget'), rvImplementation,
    LSignature, LImplementation)),
  NyxEditDeclaration(LPolicy.Routine, LPolicy.Code, LNewPolicyCode)
]);
```

One independent session admits the final complete source/design pair before
response preflight and one live publication/paired Undo step. A late refusal
preserves all files, drafts, navigation and history. Permission/revision/transport
authority/receipt and outer workspace/review guards apply. Each fragment permits
32768 Unicode scalars; the group permits 131072. No-op groups refuse. The native
qualification creates private/public helpers, updates a class method, removes
an obsolete public helper and executes the unchanged compiled companion's actual
memo callback. External-unit callers and expression/type correctness still need
ordinary `nyx_build` diagnostics. Guarded signature replacement is source admission,
not successful compilation or execution: incompatible local/external callers
still need the ordinary compiler result. Class/full-unit authoring, updated
authenticated observing deployment and complete source quality retain their
original open owners.

### Typed view builder source

Current source adds two focused modes to `nyx_pascal`. They use the same
permission, exact revision, private transport owner, project/review routing and
retry receipts as other source edits. Discover the installed schema first:
an older running Studio may not advertise these modes yet.

`views` returns only a Unicode-scalar window of accepted text between the
`nyx:views` markers. Whitespace and comments belong to that text; the markers
do not. The response includes revision, starting line, total, nextOffset and
pendingDraft. Default count is 2048; maximum is 4096. Concatenate windows only
when their revisions match. A line points to the start of the complete body;
offsets count scalars, not native UTF-8 bytes or browser UTF-16 code units.

```json
{"mode":"views","offset":0,"count":2048}
```

`edit-views` supplies that complete exact body as `expected` and a typed Pascal
builder as `builder`, with expectedRevision and a unique operationId. Each text
permits 262144 Unicode scalars. An immutable public Pascal command is available
to other controllers as well:

```pascal
LPatch := NyxViewsPatch(LAcceptedBuilder, LProposedBuilder);
LPair := LPatch.Candidate(LAcceptedPair);
LSession.AdoptProject(LPair);
```

Import `nyx.studio.viewsedits` for the command. Candidate stages ordinary source
Apply on an independent session; the controller publishes once through its
existing paired Undo boundary. It retains application helpers, import sections
and both markers exactly. Pending drafts, mismatched expected text, no-op
proposals, malformed/oversized text, duplicate/escaped boundaries, directives
and unsupported fluent Pascal refuse before publication. Related builder changes
share one Undo/Redo step. The compact receipt includes no complete source/design.
This operation changes the admitted portable design beside its authored source;
it does not execute code or prove compiler correctness. Use `nyx_build` afterward.
Whole-unit/class editing and project-file import remain separate open outcomes.

`./tools/build.ps1 -Target pascal-views` runs the native semantic journey and
current discovery checks, executes the exact emitted companion against an actual
Win32 label, and stages the equivalent browser journey/compiled consumer under
`build/views-source/browser`. Serve the staged files over an admitted HTTP host
and observe `data-nyx-views` / `data-nyx-views-controls` becoming `passed`.
This target launches no backend and changes no editor project or configuration.
Browser execution, installed-tool availability and observing deployment are
distinct evidence from compilation.

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

Apply the same roots, in the same order, at the same revision and owning connection, adding
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

## Maintained full-catalog property review

The Pascal catalog author can qualify a shared service through its explicit
`review-properties` mode. One authenticated transport creates an accepted-base
review, composes all catalog sample/page/neighbor families, adds the numeric
property-review input, exports accepted Pascal in 80-line windows, checks exact
paired Undo/Redo and requests both application compilers. It explicitly disposes
the review before retiring its transport. Legacy compose/properties modes remain
for explicitly disposable services; inspect remains read-only.

Use an already configured service and a fresh output directory:

```powershell
./tools/build.ps1 -Target properties -PropertyMCPConfig .codex/config.toml -PropertySourceDirectory build/property-concordance/review-source -BrowserOutput build/property-concordance/review-browser
```

The enrolled configuration stays private. Existing output directories refuse
before connecting; the author never overwrites an older seed. Omit
`PropertyMCPConfig` to compile/consume an already exported companion with the
same source-directory argument. The command stages the matching browser harness
and RTL; it launches no listener or browser and does not alter Studio permissions.

A shared review uses one four-operation transaction per catalog family. The
source reconciler has its own bounded edit-distance/memory budget, so the
protocol's maximum 64 leaves does not guarantee that a large expanded recipe
group fits. Related page/sample/neighbors remain one paired Undo operation.
Compiler admission receipts are saved before bounded status polling.

The refreshed packet passes actual authenticated composition of 76 kinds,
exact Undo/Redo and both frozen-service application compilers, followed by
18,061 current-source Win32 property checks over 262 physical faces. All native
traces are leak-free and maintained consumers have zero owned warnings. The
frozen server's library snapshot is distinct from current-source native/browser
consumers. Its unchanged exported source now also passes 18,203 desktop / 18,204
CSS-390 HTTP browser property checks. Live checkpoints precede explicit renderer
disposal and expose the corrected retained-origin flow geometry. After serving
the staged host, the maintained Pascal ready/capture driver can inspect
`data-property-tests` at `properties.html?capture=1` (desktop) or
`properties.html?frame=1&capture=1` (390px viewport); a checkpoint is acknowledged
only after PNG/DOM capture. `data-property-geometry` includes bounded read-only
viewport/containing/control widths and computed styles, at most sixteen boxes,
without user text or a design export. Current desktop/CSS-390 measurements match
document and scroll widths; the suspected scrollbar is the progress control.
Default manual completion remains synchronous.
These target consumers retain the original implemented-property scope. Physical
phone/assistive input, complete matrix/visual/scroll-extent quality and observing
deployment remain open; no full workflow criterion closes. See
[current qualification](property-concordance.md#current-full-catalog-qualification).


## Maintained clock authoring review

`tools/nyx_clock_authoring_review.lpr` uses one authenticated connection and an
independent temporary review for an English two-page project and reusable
appointment. It authors clock policy through `NyxSetValueDomain`, `NyxTimeDomain`
and typed clock values at the explicit JSON tool boundary. Bounded node/choice
queries and 80-line source windows establish context; immutable semantic preview
packets are used selectively for exact paired history validation. Related edits
are one transaction, including a refused late-invalid group.

On Windows, compile the maintained author without connecting to any service:

```powershell
./tools/build.ps1 -Target clock-review
```

Explicit private configuration opts into the real semantic journey on an
already-running loopback Studio. Choose a fresh output directory:

```powershell
./tools/build.ps1 -Target clock-review -ClockReviewMCPConfig .codex/config.toml -ClockReviewDirectory build/clock-review/review -HttpURL http://127.0.0.1:8088
```

The Pascal owner refuses existing output directories before connecting. It never
starts/replaces a backend, edits enrollment/permissions or replaces the user's
design. One connection retains review ownership through source/history and all
six browser/LCL page/reusable/application jobs. Application compiler input is the
entire exact accepted source; page/reusable input is the public owned projection,
retaining reachable definitions and helpers while excluding unrelated pages.
Receipts precede polling of the same job until terminal; an observation interval
does not resubmit or declare a still-live compiler stopped.

The actual observing Studio exposes the owned review and its changing revision;
the Pascal browser pipe reveals its card for a meaningful capture without
clicking it or injecting application scripts. Compiled browser input and reusable
projection are captured separately. All browser profiles are independent. The
review is explicitly discarded and the transport/hosts retire; portable
`meeting-planner.nyxproject`, `design.nyx` and Pascal remain in the owned output.
Private build/preview receipts and screenshots are ignored evidence, not exports
of machine configuration.

Current maintained execution passes **729** assertions including real-clock
transport/status polling, with zero native leaks and six successful current
compiler jobs. This is bounded clock authoring/build/observing evidence; complete
native interaction, trusted picker/IME/accessibility, general source/import and
full runtime/workspace semantics retain their original owners. See
[the packet](../WORK.md#current-return-path-authenticated-clock-authoring--2026-10-08).

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

## Concurrent project sessions

The source candidate adds `nyx_workspaces` alongside temporary `nyx_reviews`.
These have different lifetimes: a review belongs to its authenticated transport;
an open project survives every agent disconnect and remains until an operator
closes it or the service shuts down. Open-session retention does not replace
saving a paired project file. The current LAN release still exposes fifteen
tools; the independently staged project fixture exposes seventeen. See WORK.md
for the exact qualification and deployment boundary.

`nyx_workspaces` offers bounded `list`, `inspect` and `create` modes. List returns
at most nine summaries: the primary project plus eight open projects. Summaries
contain project identity/title, revision, selection/view, draft/history flags
and friendly agent names with distinct public connection numbers. They omit
complete documents/source, transport authority, credentials and machine paths.
Labels admit up to 256 Unicode scalars. Creation guards the primary revision,
uses `empty` or `accepted` as its base and excludes pending source drafts from
accepted copies. Its immutable operation receipt belongs to the authenticated
connection, independently of the friendly display name.

```json
{"mode":"create","expectedRevision":6,"operationId":"new-project","label":"Input workshop","base":"empty"}
```

Use the returned `workspace` as an outer argument to ordinary tools. Read that
project's revision before editing, then keep related operations in one undoable
transaction. Supply either `workspace` or `review`; combining them refuses.
Omitting both always names the stable primary project, regardless of which
project the user is viewing. Missing, closed, foreign-service and explicitly
empty supplied handles cannot fall back to another project.

```json
{"workspace":"<returned-reference>","line":1,"count":20}
```

The same scope routes selection, properties, callbacks, Pascal implementation
editing, history, diagnostics, compiler jobs and previews. Jobs capture their
exact context and admitted pair. Status refuses a handle queried through
another context; navigation cannot retarget it. Two workers, sixteen retained
job handles and existing compiler receipt budgets remain shared across projects.
Connection presence is bounded to 64 connection/project pairs. Each live
transport retains up to 64 project-creation receipts without eviction; an old
retry cannot resurrect a closed project.

Successful primary-context queries/edits and compiler/preview discovery publish
their authenticated connection in the primary project's roster, just as explicit
project calls do. Repeated requests update that owner; two connections sharing a
display name remain distinct. Explicit review requests retain review attribution.
Transport DELETE removes the retiring owner's presence from every project.
Presence is transient and never changes document revisions or checkpoint bytes.

The Nyx-built **Agents** panel lists project sessions and offers **Jump into
project**. This opens the full ordinary editor in the same tab. Departing waits
for acknowledged local publications and refuses outstanding imports/file/build
operations or unresolved conflicts. Each project retains its own paired source,
pending draft/base, selection/view and Undo/Redo on the service. Per-tab typed
preferences retain code visibility/proportion, panels, search/filter, authoring
draft fields, caret and scroll. Agent paints defer through pointer release so
navigation controls remain mounted during a tap; project controls use stable
reference identities rather than list positions. Saved preferences contain no
document pointers, credentials or compiler paths.

Closing is operator-only. Switch to another project, request **Close project...**,
review the unsaved-work/draft/history warning and explicitly confirm its exact
revision, or choose **Keep project**. The primary project cannot be closed through
this registry. An older service that does not advertise closure disables
confirmation. The portable close contract and actual warning/cancel controls
are qualified; the newly added private editor HTTP close route still requires
live qualification. Automatic approval review refused launching its new fixture
server. Do not infer successful closure from its compile or UI presence.

```powershell
./tools/build.ps1 -Target project-workspaces
```

This builds portable checks, actual LCL controls, the service, Studio/viewers and
the maintained native semantic/browser journey under
`build/project-workspaces/orchestrated/`. It launches no service and edits no
live project. Serve `workspaces.html` and `workspace-view.html` to execute the
matching browser fixtures. The protocol program takes an owned fixture editor
URL, its private Codex configuration and an evidence directory; its path guard
accepts only the documented project fixture roots. It deliberately resets its
disposable baseline. Optional fourth argument names a private JSON artifact with
exact `first`/`second` project references for explicit fixture reuse; previous
frames are saved before reset. Optional fifth argument `390` selects an actual
390-pixel viewport. Composition, history, source export, build requests and
preview validation remain semantic; only physical navigation/typing uses the
browser host. Preserve failed captures, and never use this reset harness against
a user's working service.

Native full-editor jump/return now has a 49-check real MCP/LCL journey, including
observed input, independent paired drafts/history and actual source focus/range
restoration. [Native Studio](native-studio.md) documents its optional loopback
connection and remaining presentation/transport qualifications. This shares the
browser's protocol controller; native HTTP runs outside the UI thread. Native
permissions/activity use the same public Agents composition. Cancellation
suppresses local delivery without revoking an admitted server operation.

Complete native Studio, successful live warned closure, job completion
after project closure and broader source/state/reusable workflows retain their
original task owners. The current evidence establishes the portable foundation
and observing browser journey, not the complete concurrent authoring criterion.

## Local draft capture

Source typing changes the local Nyx editor/session immediately. Its project-owned
bridge marks a pending capture and asks the existing adapter timer to run after
250 milliseconds. Further keystrokes share that window; they do not continually
postpone it. The timer captures one fresh paired project, including accepted
Pascal, pending text and its original base. Browser recovery and agent sharing
consume the same encoded pair. Accepted design/source commands remain immediate,
ordered publications with ordinary paired history.

Pending local capture prevents source synchronization, build admission and project
navigation from reporting an acknowledged frame. A complete older observation
cannot replace unsent typing, even when the publication queue is still empty.
Concurrent remote changes retain local work and require explicit conflict
resolution. Capture/queue failures likewise cannot be cleared by a late reply.

Browser recovery continues while sharing is paused. Page hiding/navigation and
embedded-editor destruction attempt a current recovery write before retiring
callbacks. A denied/full store retains the preceding readable recovery and local
draft, with visible help for an explicit backup. Timers and HTTP are best effort;
these hooks cannot guarantee persistence after a process crash or OS termination.
Native local files still use the explicit paired save contract. Cancellation does
not revoke a publication already admitted by the service.

The maintained protocol fixture exercises reply ordering against an independent
real portable agent session, without an HTTP listener. Its native companion uses
actual Nyx Studio memo events, Win32 timers and painting at the original source
sizes. Authentication/network behavior remains covered by its separate consumers;
compile-only browser evidence does not establish DOM/timer/storage execution.

Studio's hierarchy consumes the specialized public Nyx tree with independent
typed caption/parent rows and exact component IDs. It occupies a bounded viewport;
it does not build one native editor button per component. Selection uses the
public typed `OnSelectionChange` stream with UI-queue delivery, so painting starts
after the originating tree notification. Shell replacement cancels queued old
events, and controller retirement cancels the borrowed receiver subscription.
Compact panels restore selection only when their hierarchy is mounted. Current
native large-canvas and browser execution limits remain recorded in WORK.md.

## Relative control placement

`nyx_transaction` also accepts `place` and `place-new` in an ordinary grouped,
revision-aware paired Undo operation. Inspect exact authored IDs through bounded
`nyx_outline` / `nyx_node` queries, then submit `expectedRevision` and a retry
`operationId` with the related operations:

```json
{
  "expectedRevision": 12,
  "operationId": "arrange-reply-controls",
  "operations": [
    {"op":"place","id":"reply-editor","target":"reply-layout","placement":"inside"},
    {"op":"place-new","kind":"badge","id":"reply-status","target":"send-button","placement":"before"}
  ]
}
```

The closed positions are `inside`, `before` and `after`. Inside appends to the
exact editable target; sibling positions resolve after detaching a moved
control. Root order, inherited instance children, self-placement, cycles,
occupied identities and leaf containers refuse. Customize a named layout part
before placing reusable-instance content. The same candidate engine serves
ordinary Studio's [two-step placement controls](designer-views.md#place-a-control-in-another-layout).
No new tool or document dump is needed. Source helpers, defaults and drafts keep
their existing admission/ownership contracts. `tools/build.ps1 -Target placement`
qualifies the semantic journey and actual native Studio input, compiles its
unchanged companion for both targets, and stages browser consumers/Studio/worker
without launching a host or refreshing enrolled configuration. Current browser
execution and updated authenticated observation retain their deployment gate.

## Structured collection context and authoring

`nyx_collections` inspects **document defaults**, not a running application's
independent stores. Every query reports its revision. Read several windows at the
same revision before applying one related group. An observing editor's selection
never supplies an omitted collection, row or view owner.

| Mode | Required context | Bounded result |
| --- | --- | --- |
| `list` | None; optional case-sensitive key substring `filter` | Up to 50 exact keys and field/row counts |
| `schema` | `key` | Up to 50 typed field definitions with 80-scalar default previews and domain summaries |
| `rows` | `key`; optionally 1..16 exact `fields` | Up to 20 scoped row IDs; selected cells only, with typed values or 80-scalar text previews |
| `value` | `key`, `field`; optionally `item` **or** domain `choice` index | Exact Boolean/Integer/Double, or a text window of at most 4096 Unicode scalars |
| `domain` | `key`, `field` | Range metadata and up to 50 indexed choice previews |
| `bindings` | Exact authored `owner` | Supported projection, local/cleared/effective/inherited/restorable meaning, paged columns and bounded saved/default typeahead |
| `column` | `owner`, `field`, explicit `source: local/effective/restorable` | Exact column family/mode and a title text window |
| `query` | `owner`, explicit `source: local/effective/restorable` | Default eight/max twenty predicate nodes, value previews and at most eight sort keys |
| `query-value` | `owner`, `source`, exact predicate child `path` | One exact typed value or a text window of at most 4096 Unicode scalars |
| `apply` | `expectedRevision`, `operationId`, 1..32 `changes` | One paired publication, ordinary Undo and exact retry receipt |

Page modes accept `offset`/`limit`; text windows accept `offset`/`count`.
Offsets/counts for text measure Unicode scalars, including supplementary characters
and embedded NUL. Numeric/Boolean values reject text-window arguments. A row and
a domain choice cannot both address one value. Full domains and long defaults/
titles are absent from discovery responses; request their precise windows.
The complete existing 48 KiB response budget still applies, including unusually
large open names. Queries preserve accepted source, drafts, history and navigation.

Query pages traverse the predicate tree in preorder. Each node supplies a
value-only child `path`, operator and child count; field predicates add the exact
field/family/comparison and an 80-scalar expected-value preview. A path starts at
`[]` and appends zero-based child indexes. `query-value` requires a field leaf;
branch, missing or out-of-budget paths refuse. The query page reports
`offset`/`nextOffset`/`total` and complete bounded ordering, excluding schema,
columns, row contents and recursive subtrees. Its maximum offset is 64; paths
have at most fifteen child indexes. Read the desired pages at one revision.

Named commands use strongly typed schema/cell descriptors at this explicit JSON
boundary. Pascal callers use `NyxDefineCollection`, `NyxSetCollectionField`,
`NyxAppendCollectionRow`, `NyxUpdateCollectionRow`, `NyxMoveCollectionRow`,
`NyxRemoveCollectionField`, `NyxBindCollection`, `NyxSetCollectionQuery`,
`NyxSetCollectionTypeAhead` and `NyxUseDefaultCollectionTypeAhead` with
the existing typed field,
item, domain and fluent view contracts. The immutable `INyxCollectionPatch`
owns copied proposals; it retains no mutable document, control or runtime store.

- `define`: exact new `key`, `fields`, `rows`. Each definition has `name`,
  `kind`, typed `default` and optional typed `domain`. Each row has `item` and
  `values`; each cell has `field`, `kind` and its matching primitive `value`.
  An existing key refuses. Omitted cells materialize schema defaults.
- `field`: `key` and one `definition` add/update a named field. Existing families
  cannot change. Unspecified fields, row values and order remain retained.
  New defaults do not rewrite materialized existing cells; new domains revalidate
  every accepted row and dependent view.
- `append`/`update-row`: `key`, exact `item`, typed `values`. Append refuses a
  duplicate row; update requires an existing row and changes only supplied cells.
  Unknown/duplicate fields and wrong primitives/families refuse.
- `move-row`: `key`, `item`, final `index` after removing the moved row.
  `remove-field` requires `key`, exact `field` and its `kind`.
- `bind`: exact authored `owner`, expected `projection: list/table/tree` and
  a complete versioned `spec` from `TNyxCollectionViewSpec.ToData`. Single
  selection retains version 1; query-free multiple selection uses version 2.
  A nonempty filter/order policy uses version 3. An explicit saved typeahead
  choice uses version 4, including selection and nullable query.
- `query`: exact authored `owner`, bound `key`, expected `projection` and a
  complete typed `query` from `TNyxCollectionQuery.ToData`. Null clears only
  filtering/ordering. Columns, scope, parent mapping, selection and saved
  typeahead remain intact.
  An inherited binding acquires an independent local override. Missing/unbound
  owners, changed key/projection and incompatible schema fields refuse. This
  operation can share a single collection group with row/default changes.
- `typeahead`: exact authored `owner`, bound `key`, expected `projection: list/tree`
  and `policy` from `TNyxTypeAheadOptions.ToData`. Its four required members are
  `version: 1`, native Boolean `enabled`, Integer `windowMS: 1..60000` and
  `match: folded/exact`. Null selects library defaults while retaining an
  independent local binding. Query, columns, scope, parent and selection survive.
  Reset does not remove a reusable override or restore inheritance; use the
  existing `inherit` intent for that separate operation. Missing/unbound owners,
  changed keys/projections, tables and malformed policies refuse atomically.

Each defined list/tree binding context contains `typeAhead: {declared, policy}`.
`declared: false` reports the library default; explicit equivalent options remain
declared. The outer `inherited` flag qualifies effective ownership. A clear mask
has no effective binding, while `restorable` exposes the inherited policy beneath
it. `typeAheadSupported` distinguishes list/tree capability; table policies are
null. Only bounded scalar options are returned: runtime text prefixes, timestamps,
focus and runtime overrides belong to the mounted application and are absent.

Group related policies through `nyx_collections` or a `collections` group inside
`nyx_transaction`. For example, at the revision read from `nyx_session`:

```json
{
  "mode": "apply",
  "expectedRevision": 12,
  "operationId": "destination-search-policy",
  "changes": [{
    "op": "typeahead",
    "owner": "destination-list",
    "key": "destinations",
    "projection": "list",
    "policy": {"version": 1, "enabled": true, "windowMS": 800, "match": "folded"}
  }]
}
```

These strings are the explicit MCP wire boundary. Pascal authoring uses distinct
references, fluent scalar options and enums; no string selects built-in behavior.

`intent` reuses the ordinary editor's closed operations. It requires a closed
`action` and exact `key`; view actions also require `owner` and `projection`.
Additional fields depend on that action and are discoverable through tools/list:

| Actions | Additional fields |
| --- | --- |
| `create`, `remove` | None |
| `add-field` | Scalar `kind` (ordinary automatically allocated field name) |
| `default` | `field`, `kind`, matching primitive `value` |
| `add-row`, `remove-row` | Scoped `item` |
| `cell` | `item`, `field`, `kind`, matching primitive `value` |
| `bind`, `clear`, `inherit` | No additional fields beyond view ownership/projection |
| `scope`, `selection` | `scope: application/instance`, or `selection: single/multiple` |
| `column-title`, `column-mode` | `field`, `kind` and `title` or Boolean `editable` |
| `parent` | Text field name `parent`, or null to clear the tree mapping |
| `remove-column`, `add-column` | Exact `field`, `kind` |
| `query` | Complete typed `query` and exact schema/binding `baseline` from the public query form |

Ordinary `bind` derives columns from the current schema. Ordinary `add-column`
adds the field's default presentation; change its title/mode in subsequent
intents or use a full fluent `bind`. Clear deliberately masks inheritance;
inherit removes a local override. Exact existing named-part owners are supported.
The query-only named command derives its baseline from the exact-revision
candidate; agents do not need to retrieve or resend private form metadata.

An explicit reusable clear keeps its effective view unbound; `restorable` reports
the exact inherited contract beneath that mask, with the same paged columns.
Studio retains its Restore button for that contract, and both ordinary capture
and semantic replay recheck the inherited key without changing the live mask.

Apply replays the same ordinary Studio commands on one independent candidate.
Each ordered intermediate must admit: clear dependent columns/parent mappings
before removing a field, and clear dependent bindings before removing a collection.
A late failure discards the entire group. Pending Pascal drafts, stale revisions,
missing owners/rows, mismatched projections, domains, schema families and unknown
members refuse without changing the active pair or history. Permission, transport
actor authority, exact request receipts and visible activity reuse the existing
agent boundary. Workspace/review routing is available only on the outer request;
foreign or retired contexts refuse without falling back to the primary project.

`tools/build.ps1 -Target collection-bindings` runs the independent Pascal
journey, actual discovery builder and unchanged compiled native controls, then
stages shared/browser controls with matched RTL. It starts no listener and
performs no deployment or configuration refresh. WORK.md owns observed results.
`tools/build.ps1 -Target collection-query-workflow` runs the focused query
boundary and builds the explicit authenticated grid/query companion tools.
It stages the shared browser program without launching it. Current isolated
authenticated query/source/history and all four browser/LCL application/view
builds are qualified; full browser Studio observation and deployed LAN authority
retain their recorded gate. See
[the current query workflow packet](../WORK.md#current-return-path-bounded-collection-query-mcp--2026-10-07).

`tools/build.ps1 -Target typeahead-workflow` exercises the current in-process
dispatcher, actual discovery builder, paired refusal/history and reusable
ownership using an independent copy of the unchanged authenticated English seed.
Actual Win32 controls mount the exact accepted semantic design. A separate
compiler executes its exact accepted source and retained handwritten helper.
The backend, both Studios, browser consumers and source worker compile without
launching services. This is not current HTTP authentication, executed browser or
observing/deployed Studio evidence; those gates remain recorded in
[the workflow packet](../WORK.md#current-return-path-semantic-saved-search-workflow--2026-10-07).
