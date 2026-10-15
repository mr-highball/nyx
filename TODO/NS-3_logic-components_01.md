# NS-3_logic-components_01 — Nonvisual logic components and Tool Canvas collection

[North stars](../MILESTONES.md) · [Task catalog](README.md) · [Task flow](../TASKFLOW.MD)

**Description:**

The user's final 2026-10-13 canvas requirement adds reusable nonvisual components
for useful operations: file dialogs, JSON formatting, sorting, regex transforms,
HTTP calls and data mapping. Tool Canvas displays configurable nodes/ports;
ordinary applications execute their logic without editor chrome. NS-3 owns the
catalog, specialized managed interfaces/implementations and toolbox collection.
Typed query, resource, scheduler and graph contracts supply shared foundations;
native/browser adapters own host services. Creators can compose and replace
implementations without forking Studio. This is additional required scope, not
accepted component count or completion credit. Planning earns no implementation credit.

Initial coverage is organized by intent, retaining creator tags/descriptions:

| Toolbox group | Required building blocks |
| --- | --- |
| Files | Open/save picker, text/byte read and write, file metadata, result/cancel |
| JSON | Parse, prettify/compact, select path, construct/update/merge, schema validation |
| Collections | Sort, filter, map, group, distinct, join, aggregate, pagination |
| Text | Regex match/replace/extract, split/join, trim/case, format, encoding |
| Network | HTTP request/response, headers, JSON/text/bytes, status/failure/cancellation |
| Data | Typed conversion, field mapping, defaults, validation, resource/localization lookup |
| Flow | Branch, sequence/fan-out, merge, delay/timer, debounce/throttle, explicit feedback |

**Acceptance Criteria:**

- Specialized reference-counted interfaces/factories expose typed configuration,
  input/output port schemas and execution results. Data, invocation triggers,
  completion/failure/cancel and progress have distinct meanings. Owned structured
  values, text, scalars, bytes and file/resource capabilities retain exact types;
  host objects/authority are not fabricated from ordinary data strings. Direct
  Pascal callers use the same component without Tool Canvas or an interpreter.
- Implement the coverage above with reusable adapters, not raw executable
  operation strings. Queries reuse Nyx's typed LINQ pipeline; HTTP/resources and
  async work reuse host/scheduler services. A published coverage ledger records
  unsupported host cases and explicit outcomes. Regex syntax/flags/Unicode and
  schema semantics have declared portable contracts and both-target evidence.
- Typed graph chains perform useful work: HTTP → JSON parse → filter/sort/map →
  Table, and control event → JSON pretty → Save picker/write. Errors/cancellation
  are usable outputs. Async outputs honor owning run/revision, ordering, UI
  scheduling and retirement; failure/cancellation/stale work never partially
  publishes control updates. Multiple runs/instances have independent ownership.
- File/dialog adapters preserve browser user activation and native host capability
  requirements. Selection/cancel and saving/downloading are distinguishable;
  success reflects the actual host outcome. HTTP exposes typed method/status/
  header/body configuration, tunable deadlines/retry/cache, cancellation and
  injected transport, with exact bytes/Unicode and explicit failure results.
- A distinct Logic toolbox collection supports optional intent grouping, useful
  search tags and creator descriptions/help. Public Nyx design cards/ports and
  inspectors explain intent; ordinary UI rendering omits that design view.
  Visual controls and logic nodes coexist without retaining editor objects in
  runtime documents. Creator extensions publish compatible schemas and help.
- Versioned persistence, crafted Pascal, source/history and reusable definitions
  retain configuration/connections. Bounded MCP searches/queries ports and
  creates/configures/connects operations through expected-revision grouped
  Undo/Redo. Drafts, type errors and malformed configurations preserve the pair;
  UI and agents share the same public typed operation API.
- English practical tools execute those chains on actual browser/LCL consumers,
  including hosted/embedded inputs, rejection, cancellation and retirement.
  Qualify toolbox desktop/narrow previews, actual file/network outcomes,
  numerical/Unicode exactness, resource budgets and observing rollout. Catalog
  metadata or standalone algorithm checks alone do not accept this task.

**Blockers**

- [Managed component interfaces](DONE/NS-1_component-interfaces_01.md).
- [Portable resources](DONE/NS-1_resources_01.md).
- [Typed behavior graphs](NS-1_canvas-flow_01.md).

Tool Canvas consumes this task; it is not a cyclic prerequisite for standalone
typed logic components. Graph/toolbox/runtime evidence remains required. Record
the scope now, then return to computed-binding authoring before expansion.

The 2026-10-14 graphics foundation is qualified separately. Next advance typed
graph execution with standalone JSON/text Logic as its first connected consumer.
Keep all Files/JSON/Collections/Text/Network/Data/Flow coverage and creator labels/
descriptions above. A drawn card, algorithm-only fixture or graph metadata cannot
satisfy functional outputs.

The first executable batch now supplies specialized managed JSON parse/select/
format and text join operations, reusable schemas/help/labels, independent graph
runs and actual browser/LCL control outputs. This is real connected computation,
with WORK.md evidence; it is not the full toolbox or the coverage ledger above.
Saved graph declarations now retain schemas/configuration without runtime host
authority. Versioned documents, typed source/admission, exact paired history and
compiled base/handwritten continuation execute on native/browser consumers, with
focused page/reusable ownership checks. WORK.md owns exact evidence and limits.
The public Logic workbench now supplies optional intent grouping, creator help/
labels/search, typed built-in configuration/paths, step loading and typed connection
choices. Studio consumes that form through paired source commands on both targets;
semantic graph edits share final-graph validation and ordinary history. Actual
control/HTTP evidence and limitations are in WORK.md. Logic no-closure count is
three. Full original criteria remain open. Next connect discoverable control/event
routes and Tool Canvas hosting, retaining all operation breadth. Avoid additional
standalone graph/editor/fixture variants or claiming canvas drag wiring from selects.

Saved control/event routes now drive these operations through ordinary browser/
LCL application mounting, typed event/state/constant mappings, atomic outputs and
revocable scheduler ownership. Current-source semantic route edits retain paired
source/history. WORK.md owns exact native/browser/HTTP evidence and retained
corrected failures. Logic no-closure advances three → four; original criteria
stay open. Return to embedded canvas/Tool Canvas attachment and broader useful
operations. Four JSON/text kinds do not meet the requested robust collection;
file/dialog/HTTP/regex/query/data/flow coverage and activation/authority remain open.

Embedded runtime items now execute those routes through item-owned revocable
contexts. The specialized Tool Canvas consumes the public immutable event/state
port catalog and exposes graph pause/cancel separately from static callbacks.
Actual native/browser generated companions and source/history are recorded in
WORK.md. Graph/Logic no-closure advances four → five; no original criterion closes.
Next advance useful operation breadth and connection editing. Do not add further
read-only inspector variants; four JSON/text operations remain far short of the
required collection above.

Current-source collection operations now add managed JSON filter, stable typed
sort, path map and skip/take through the existing LINQ pipeline. Typed public
workbench fields, source admission, legacy draft restoration, authenticated MCP
composition and actual generated native/browser tools have bounded evidence in
WORK.md. The coverage ledger in docs/logic.md explicitly retains the missing
groups above. Graph/Logic no-closure advances five → six; no original criterion
closes. Stop query/inspector variants; advance host-service HTTP/file capabilities
and useful data/table publication. Full regex/group/join/aggregate/mapping/flow
coverage, visual wiring, interpreter, budgets and delivery stay open.

Current-source INyxHTTPCall/NewNyxHTTPCall and INyxUTF8Decode/NewNyxUTF8Decode
add explicit host authority, data-only request/graph reconstruction and usable
typed HTTP outcomes. Shared native/executed-browser component checks substitute
transport deliberately; they do not establish sockets/TLS/Fetch. General Win32/
browser adapters compile, including method/body/header and deadline/cancel paths.
Automatic approval review rejected the isolated producer launch before execution
with only "blocked by policy"; no alternate startup occurs. WORK.md records exact
counts, retained corrected failures and protected live state. Graph/Logic no-closure
advances six → seven; no original criterion closes and the default toolbox stays
at eight kinds. Real network execution, tunable retry/cache, crafted specialized
source/admission, ordinary toolbox/MCP authoring and HTTP → Table publication
remain the immediate continuation. Full file/regex/query/flow/script/wire and
quality/delivery scope above remains required; do not add contract-only variants.

The typed authoring continuation now integrates HTTP/UTF-8 into the ten-kind
default collection and public Nyx request form. Typed method/port/source constructors,
exact duplicate headers/text/binary bodies, legacy/current unfinished drafts,
ordinary source-command publication and local bounded semantic changes have
current-source native/executed-browser evidence in WORK.md. Exact generated
applications execute saved routes through explicit substituted transport and live
controls. This qualifies source/authoring/application contracts, not network I/O
or authenticated current-schema observation. The isolated producer launch gate is
unchanged and no startup retry occurs. Graph/Logic no-closure advances seven →
eight; no original criterion/DONE/aggregate or installed delivery closes. Stop
authoring/fixture variants; return to real HTTP qualification, tunable cache/retry
and guarded data/table publication, retaining the complete Files/JSON/Collections/
Text/Network/Data/Flow, visual wiring, interpreter and quality scope above.

Current-source typed JSON row recipes now connect query outputs to existing
runtime collections. Scalar and row candidates publish through one guarded group;
late validation/owner retirement, stale revisions and cancellation retain the old
frame. Actual generated native/browser Table companions exercise substituted
HTTP → decode → parse → filter/sort/map → live rows and formatted output. WORK.md
owns evidence and limits. Graph/Logic no-closure advances eight → nine; no original
criterion/task/DONE/aggregate credit closes. This is typed handwritten runtime
mapping, not saved row-route/source/Studio/MCP authoring. Real network/cache/retry,
file capabilities, full operation breadth, interactive wiring, interpreter,
quality/budgets/parity and preserving delivery remain required.

Saved typed row routes now connect those operations without handwritten callback
glue. Exact legacy scalar bytes, new row recipes, crafted source/admission and
paired ordinary Nyx form commands have bounded evidence in WORK.md. The exact
UI-authored compiled native/browser application executes the changed field path,
all scalar families and supplementary/NUL structural names with substituted HTTP.
Local semantic row queries/transactions remain bounded and revision-pinned.
Graph/Logic no-closure advances nine → ten; no original criterion/task/DONE/
aggregate or delivered parity credit closes. Stop row/recipe/fixture variants.
Return to real HTTP qualification at an admitted host, cache/retry and file-save
capability/user activation for the JSON → Save chain. General route/input creation,
richer mapping choices, regex/query/flow/script/wire breadth, quality/budgets and
preserving delivery remain required.

The 2026-10-14 typed save continuation now adds INyxSaveFile/INyxUTF8Encode,
explicit runtime file hosts and a public Nyx save-options form to the twelve-kind
collection. Crafted enum/fluent source, exact options/history, optional version-5
unfinished drafts and bounded local semantic changes have native/executed-browser
evidence. Actual Win32 controls drive a real save dialog, exact disk writes, user
cancel and modal scope retirement; actual desktop/CSS-390 browser input verifies
download dispatch and independently checks the downloaded bytes. WORK.md owns
the final maintained packet and retained corrected failures. Compiled constructors
are separate from actual host execution; installed MCP/Studio are unchanged.
Graph/Logic no-closure advances ten → eleven; no original criterion/task/DONE or
aggregate parity/delivery credit closes. Stop schema/frame/fixture variants and
qualify browser picker/retirement plus a useful fresh-activation flow through
public Nyx controls. Retain real network/cache/retry and every open/read/metadata,
JSON/query/regex/data/flow, wiring/script, quality and delivery outcome above.

Prepared-save activation is now an optional typed runtime host composition, with
creator-owned Nyx confirmation recipes/options, unchanged file permissions and
same-byte graph continuation. Actual Win32 confirmations/picker/disk and HTTP
browser desktop/CSS-390 Save/Escape/retirement have bounded evidence in WORK.md;
the browser journey observes genuine activation expiry and an independently
verified exact download. Initial/later scheduler refusal and shutdown/cancellation
have shared both-target ownership/outcome evidence. The observing release and
tool inventory are unchanged. Graph/Logic no-closure advances eleven → twelve;
no original broad criterion/task/DONE/aggregate parity or delivery closes. Stop
activation/watch/prompt/geometry variants. Return to bounded real browser picker/
write/cancel and ordinary compiled application/Studio runtime host configuration,
retaining every broader Files/network/operation/wiring/script/quality outcome.
