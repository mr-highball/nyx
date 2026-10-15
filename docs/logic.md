# Executable Logic and typed graphs

[Logic owner](../TODO/NS-3_logic-components_01.md) ·
[Graph owner](../TODO/NS-1_canvas-flow_01.md) ·
[Tool Canvas](../TODO/NS-3_tool-canvas_01.md) · [Evidence](../WORK.md)

Logic components perform work without normal UI chrome. They expose specialized,
reference-counted interfaces and the same immutable operation contract used by
graph runs. A future Tool Canvas represents these operations with discoverable
cards and ports; drawing a connector alone never executes behavior.

The current standalone operations are:

| Interface/factory | Inputs → outputs | Meaning |
| --- | --- | --- |
| INyxJSONParse / NewNyxJSONParse | Text → JSON | Strict JSON admission, preserving exact number spellings, Unicode and NUL |
| INyxJSONSelect / NewNyxJSONSelect | JSON → JSON | Typed Field/Item selection, sharing the resource selector contract |
| INyxJSONFormat / NewNyxJSONFormat | JSON → Text | Compact or indented output with field order and exact numeric/text values retained |
| INyxTextJoin / NewNyxTextJoin | Text + Text → Text | Caller-defined separator; exact text without behavioral string interpretation |
| INyxJSONFilter / NewNyxJSONFilter | JSON array → JSON array | Typed path/scalar comparison, including ordinal text search and explicit null equality |
| INyxJSONSort / NewNyxJSONSort | JSON array → JSON array | Stable path ordering with an explicit Text, Boolean or exact decimal key family |
| INyxJSONMap / NewNyxJSONMap | JSON array → JSON array | Select a typed Field/Item path from every item, retaining structured results |
| INyxJSONSlice / NewNyxJSONSlice | JSON array → JSON array | Nonnegative skip/take pagination without changing source order |
| INyxUTF8Decode / NewNyxUTF8Decode | Bytes → Text | Explicit strict UTF-8 decoding; malformed input refuses |
| INyxHTTPCall / NewNyxHTTPCall | Trigger → Bytes, status, headers, error, success | Asynchronous host operation with explicit injected transport; component contract qualified with substituted transport |

Pure direct Pascal calls need no canvas, interpreter, document or scheduler:

```pascal
LProfile := NewNyxJSONSelect(NyxDataPath.Field('profile'))
  .Select(NewNyxJSONParse.Parse(LSourceText));
LPretty := NewNyxJSONFormat(njlIndented, 2).Format(LProfile);
```

Collection operations live in `nyx.logic.collections` and reuse the ordinary
Nyx LINQ Where/OrderBy/Select/Skip/Take pipeline:

```pascal
LActivePeople := NewNyxJSONFilter(NyxDataPath.Field('active'),
  njcEqual, NyxData(True)).Filter(LPeople);
LOrderedPeople := NewNyxJSONSort(NyxDataPath.Field('priority'),
  njkDecimal, njdDescending).Sort(LActivePeople);
LProfiles := NewNyxJSONMap(NyxDataPath.Field('profile')).Map(
  NewNyxJSONSlice(0, 20).Slice(LOrderedPeople));
```

These operations capture immutable input/configuration and expose no partial
result on failure. Missing paths, wrong array/key families and malformed settings
refuse. Sort validates every key, including singleton input, and keeps original
order for equal keys. Text order follows Unicode scalars; contains/prefix/suffix
are ordinal and an empty search matches any text. Numeric comparison explicitly
uses Nyx's 96-bit decimal domain (scale 0..28), with exact conversion and refusal
on overflow/precision loss; untouched JSON values retain their original numeric
spellings. It does not silently convert arbitrary JSON numbers to Double.
Null equality is explicit; null is not empty text, false or numeric zero.

Invoke checks cancellation during capture, projection, filtering, ordering and
materialization. These are pure operations and can run under the ordinary native
worker or browser event-loop scheduler policy. This does not establish parallel
partitioning of a single sort, resource budgets or interrupting entered host work.
The public Logic workbench supplies typed path/comparison/key/direction/page
fields. Editor draft version 2 adds those fields; legacy version 1 keeps its exact
encoding and restores across catalog growth when its accepted graph context still
matches. Accepted graph changes continue to invalidate unfinished proposals.

The current coverage ledger is deliberately separate from the full collection:

| Group | Executable coverage | Remaining requested coverage |
| --- | --- | --- |
| JSON | Parse, Field/Item selection, compact/indented format | Construct/update/merge and schema validation |
| Collections | Scalar-path filter, stable typed sort, path map, pagination | Group, distinct, join, aggregates, richer typed mapping |
| Text | Exact two-input join and strict UTF-8 decode | Regex, split, trim/case, formatting and remaining encoding operations |
| Files | None | Pickers, read/write, metadata and explicit cancel/host outcomes |
| Network | Typed asynchronous HTTP component contract; native/Fetch adapters compile | Real network qualification, tunable retry/cache, Studio/source/MCP authoring and useful table publication |
| Data / Flow | Existing graph typed values, DAG fan-out and atomic publication | Conversion/mapping/validation/resource lookup; branch/merge/timer/debounce/feedback |

`tools/build.ps1 -Target logic-collections -LogicCollectionsEvidenceDirectory
<fresh-directory>` runs shared/native/source/history and authenticated semantic
composition, then stages matching browser input consumers and the source worker.
Actual HTTP execution remains a separate step. Current-source tool count stays
24; the installed 23-tool release does not gain these operations without delivery.

Every operation publishes a copied descriptor: distinct kind reference, English
title/help, intent group, creator labels, affinity and exact input/output ports.
The immutable INyxLogicCatalog captures descriptors/default configuration without
retaining executable creators. WithOperation returns an independent catalog;
creator help, labels and intent participate in Unicode AND-term search.
Creators implement INyxLogicOperation on their own reference-counted base, with
immutable configuration and schema.

Text, Boolean, Integer, exact decimal Number, structured JSON, Bytes and Signal
have separate value families. Port and step references compare exact Unicode
names independently; punctuation has no delimiter meaning. Signals are invocation
tokens, never fabricated from Boolean/null data. Inputs have one producer per
invocation; outputs may fan out. No implicit JSON/text or numeric conversions occur.

NewNyxFlowGraph builds a reusable immutable DAG:

```pascal
LGraph := NewNyxFlowGraph
  .WithStep(NyxFlowStep('parse'), NewNyxJSONParse)
  .WithStep(NyxFlowStep('format'), NewNyxJSONFormat)
  .Connect(
    NyxFlowEndpoint(NyxFlowStep('parse'), NyxLogicPort(nlpJSON)),
    NyxFlowEndpoint(NyxFlowStep('format'), NyxLogicPort(nlpJSON)))
  .Build;
```

Build captures independent vectors, configuration and complete port descriptors.
It validates topology; cycles currently refuse and require a future explicit
feedback runner. Each step executes once in deterministic topological/declaration
order. Each run supplies all unwired required inputs. Unknown, mistyped, duplicate
or wired invocation inputs refuse before operations enter. Alternative operations
must return exactly their declared output frame. Failure identifies the step and
exposes no partially successful result. Previously completed results stay independent.

Graph.ToData and NyxFlowGraphFromData form a versioned standalone persistence
boundary. Built-in recovery invokes only the typed factories above. Custom kinds
require an explicit INyxLogicResolver; kind, configuration and complete descriptor
must agree, so recovery cannot silently reinterpret a saved port signature.

Documents own named immutable declarations through `Document.Flows`. A declaration
contains copied schema/configuration and connections, without retaining executable
operations, a resolver or host authority. `Definition(...).Build` explicitly creates
a runtime graph; unknown creator kinds require the caller's resolver. Clones and
focused page/reusable builds retain independent registry membership. A retained
definition can outlive its document.

```pascal
LDocument.Flows.Define(NyxFlowRef('Format profile'), LGraph);
LRuntimeGraph := LDocument.Flows.Definition(NyxFlowRef('Format profile')).Build;
```

Design version 14 saves these declarations alongside preceding typed families.
Documents without graphs retain their earlier minimal encoding. Older opaque
`flows` fields stay opaque; conflicting promotion refuses before publication.
The registry admits at most 64 definitions, each at most 1 MiB of UTF-8 wire data.

Generated Pascal uses typed factories, enums, paths and fluent graph construction.
Creator-only configuration is an explicit data boundary; its generated declaration
cannot execute until resolved. Literal admission reconstructs declarations without
invoking operations. Targeted handwritten continuation changes only named graphs
whose configuration changed, preserving unrelated helpers/computed definitions;
unexpressed registry reordering refuses whole-candidate equality. Ordinary paired
history retains accepted source/design and pending draft guards. This integration
is consumed by the public workbench and semantic boundaries described below.

TNyxFlowLimits.Defaults supplies 128 steps, 512 wires, 1 MiB per encoded value and
8 MiB per run. Fluent limits are caller-tunable within admitted finite ceilings.
The run counts invocation values and each produced value once, independent of
fan-out. Limits do not establish peak-memory, frame latency or arbitrary host-work
deadlines. JSON formatting cooperatively checks cancellation between emitted pieces.
Entered custom/blocking operations must cooperate themselves.

TNyxFlowExecution.Submit reuses Nyx's scheduler/query execution. UIQueue defers on
both targets; native Threaded uses a real worker, and browser Threaded refuses.
Asynchronous follows scheduler capability. UI-affinity operations refuse real
worker policies. Pure affinity is a creator promise about independent immutable,
thread-safe implementation state, not something a flag proves.

TNyxFlowApplication.Submit captures an active weak application context, exact
state identity/revision and page generation. Typed NyxFlowAssign overloads map
outputs to existing scalar state references. One ordinary State.Apply publishes
the complete group, and normal bindings update existing controls. Workers retain
only immutable graph/inputs; the UI courier owns the weak publication capability.
Destination admission reads an owned state snapshot. Creator getters are retained
and copied in a defined order; reentrant application retirement refuses before
live state is dereferenced or work is adopted.
JSON and bytes need explicit transformations before scalar publication. Exact
decimal outputs require canonical IEEE spelling for Number state; other spellings
or precision loss refuse instead of silently rounding. Explicit conversion coverage
remains required.

Cancellation combines explicit run cancellation and owning view retirement.
Pending work skips entry; entered graph/operation checkpoints revoke later steps
and publication. Cooperative cancellation is a cancelled outcome rather than a
compute failure. Stale state refuses the whole publication; navigation/disposal
revokes it. Dropping the run does not cancel it. Grouped output atomicity does not
roll back explicit host/UI side effects or a State observer that commits then throws.

The maintained command executes native shared and actual LCL application checks
and stages matched browser consumers for explicit HTTP execution:

```powershell
./tools/build.ps1 -Target logic-flow -LogicFlowEvidenceDirectory build/logic-flow/review
```

The English application connects parse/select/pretty/compact/join to real buttons,
a bound memo and result controls. It exercises multiple callback registrations,
native worker/browser event-loop publication, independent applications, failures,
pending/entered cancellation, stale state and page retirement. WORK.md records
exact counts, inspected desktop/narrow captures and limits.

Full coverage remains required: files/dialogs, HTTP, LINQ sorting/filtering/mapping,
regex/text transforms, data conversions, flow/timers, explicit feedback, script and
control-port discovery, embedded-canvas route attachment, interactive Tool Canvas wiring,
complete semantic authoring and preserving delivery. Hosted resource and user
activation/authority semantics belong to their existing adapters. This foundation
does not close those criteria or replace functional results with metadata cards.

`tools/build.ps1 -Target logic-document -LogicDocumentEvidenceDirectory <directory>`
qualifies saved declarations, source/history, exact compiled base and handwritten
continuation companions, and ordinary native controls. It stages matching browser
consumers; WORK.md distinguishes their actual HTTP execution from compilation.

Current-source Studio exposes **Project → Logic** as a dedicated workspace.
`NewNyxLogicEditor` is a public compound of ordinary Nyx cards, disclosures,
inputs, selects and buttons. It discovers operations, loads exact saved steps,
configures built-ins with typed layouts/numbers and structural Field/Item path
controls, and chooses typed output/input endpoints. Creator configuration uses
an explicitly labeled JSON data extension editor. Loading a creator retains that
step's own configuration-dependent schema. Removing a graph, step or wire needs
warning acknowledgement; removing a step explicitly detaches its connections.
These are accessible select/button alternatives, not canvas pointer wiring.

The form captures immutable guarded proposals. `NyxFlowPatch` and `EditNyxFlows`
stage related operation/wire changes before final graph admission, permitting
repair of intermediate signatures. Failure preserves original membership and the
accepted pair. Studio uses its ordinary source worker/compiler/publication queue;
one grouped change is one paired Undo. Draft forms retain malformed input, selected
schema and typed paths. Optional presentation version 17 parks those proposals per
project; untouched presentations retain exact version-16 bytes. Broader recovery/
navigation/ownership and large-workload qualification remain separate.

Current-source MCP adds the read-only `nyx_logic` tool. Scope `catalog` searches
intent/help/labels; `graphs`, `steps`, `ports` and `wires` return at most 32 rows
(8 default). Exact graph/step names scope configuration windows to at most 1024
Unicode scalars (512 default). Responses carry revision; pin expectedRevision
when paging. No query resolves runtime code or returns the complete document.
Compose flow-define/remove, flow-step-add/set/remove and flow-connect/disconnect
through the existing nyx_transaction tool, with expectedRevision and operationId.
Built-in step changes accept kind plus configuration; creator changes require
explicit schema data. Group guards, operator permissions, retry identity, draft
refusal and paired Undo/Redo remain ordinary Studio behavior. The source inventory
is 24 tools; the preserved installed release still advertises 23.

Ordinary application controls can now carry saved typed event-to-graph routes:

```pascal
SetNyxFlowRoutes(LFormatButton, NewNyxFlowRoutes.WithRoute(
  NyxFlowRoute(NyxFlowRouteID('Format JSON'), NyxFlowRef('Formatting'),
    ntClick, nfcmExclusive)
    .InputState(
      NyxFlowEndpoint(NyxFlowStep('parse'), NyxLogicPort(nlpText)),
      NyxTextState('source'))
    .OutputState(
      NyxFlowEndpoint(NyxFlowStep('format'), NyxLogicPort(nlpText)),
      NyxTextState('formatted'))
    .Using(neUIQueue).Concurrent(nfrLatest)));
```

Routes use distinct identities, exact input/output endpoints, typed state
references, constants, accepted scalar event values, structured event Field/Item
paths, or an explicit invocation signal. Every unwired input must be mapped once;
outputs map to existing scalar state without conversion. The complete effective
control/event schema admits before mounting. Disabled routes retain referential
integrity. Missing local routes inherit; an explicitly empty set masks inheritance.
`InheritNyxFlowRoutes` removes only that local declaration.

Browser/LCL application hosts attach ordinary realized page/reusable routes after
authored callbacks and before mounting. `ConfigureLogic` supplies optional explicit
resolver authority before mounting; its resolver must handle built-ins too.
Resolution is lazy on the first invocation, with retirement checks before reading
state. A saved creator declaration alone cannot gain host authority. `FlowRoutes`
exposes bounded per-route reports, exact cancellation and revocable Close without
retaining the application/document. No-output graphs still use guarded completion;
explicit host side effects already performed cannot be rolled back.

`nfcmExclusive` refuses matching ordinary callbacks, including registrations added
after mounting. `nfcmBeforeCallbacks` and `nfcmAfterDispatch` explicitly allow mixed
behavior. AfterDispatch means after invocation/submission, never after completion
of asynchronous callbacks. Optional event observers leave ordinary stream policy
unchanged and cannot consume physical defaults. Dispatch snapshots all phases
before entering any callback. Route policies are independent scheduler choices.

`nfrLatest` cancels superseded runs; `nfrEvery` admits each event up to caller-tuned
MaximumRuns (8 default, 1..64), and `nfrIgnoreWhileRunning` explicitly skips a busy
route. Cancelled workers cooperate; revocation prevents delivery rather than
forcibly interrupting entered host work. Graph output groups retain exact state
revision and view-generation guards. Reports distinguish succeeded, failed,
discarded, cancelled, skipped and active runs.

Current-source `nyx_logic` scopes `routes`, `route-inputs` and `route-outputs`
inspect an exact authored control and optional exact route. Mapping rows use
bounded Unicode scalar windows, including large structured constants. Query
revision pinning applies. `flow-routes-set` and `flow-routes-inherit` compose with
graph/property operations in one expected-revision `nyx_transaction`; typed
`NyxFlowRoutesPatch` supplies the same ordinary candidate/source/history path.
No new tool is added and no query executes a graph.

`tools/build.ps1 -Target flow-routes -FlowRoutesEvidenceDirectory <fresh-directory>`
qualifies saved routes, source/history, exact native base/handwritten companions,
actual native input and authenticated HTTP edits. It stages actual browser
counterparts for HTTP execution; FlowRoutesMCPPort defaults to unused localhost
9031. Visual route editing, control/script port breadth,
file/regex/HTTP operations, activation/host authority, full budgets and preserving
delivery remain separate requirements. This batch does not complete Tool Canvas.

`tools/build.ps1 -Target logic-authoring -LogicAuthoringEvidenceDirectory <fresh-directory>`
executes shared authoring, real native controls and authenticated HTTP semantic
checks. It stages browser counterparts **and their matching source worker**.
`-LogicAuthoringMCPPort` selects an unused localhost qualification port (9029
default); its fresh private runtime never enrolls or replaces the live project.
WORK.md owns actual HTTP browser execution, retained failures and scoped limits.

## Embedded controls and Tool Canvas

`NewNyxToolCanvas('data-tool').WithScene(NyxCanvasRef('tools'))` is a specialized
managed compound control. `WorkspaceCanvas`, `ToolsCard`, `ToolsTitleHeading` and
`ToolsPortHostScroll` expose its ordinary Nyx parts. Independent factory instances
own independent parts; required workspace/scroll roles are checked at admission.
The normal renderers mount the public port inspector into that scroll host.

Each runtime canvas item now binds its own authored callbacks and saved event
routes. The attachment shares the exact application state/collections and
explicit Logic resolver, with a separate weak context and generation. Camera
changes preserve that attachment. Removing an item, replacing its content or
leaving its page revokes it before disposing controls. Design previews do not
execute it. An explicit runtime mount with executable declarations requires an
application context; it refuses missing authority or a different state store.

The optional `INyxCanvasRuntimeView` exposes its runtime scene and exact mounted
item behavior. That scene is an independent materialization; runtime edits do not
rewrite the saved document. `INyxViewBehavior.EnableGraphs(False)` cancels graph
work while leaving ordinary callbacks enabled. Resuming accepts future events;
it does not replay paused input. Completed external side effects remain external.

`NyxToolControlPorts` captures immutable, exact item/control addresses: invocation
signals, available scalar/structured event payloads and real direct state-binding
inputs. Signal and optional data are distinct. Cleared, computed and resource
projections do not become writable state ports. Physical keyboard/pointer/editing
snapshot projections remain separate work. Discovery invokes no creator code.

The public inspector selects items/controls, searches and pages twelve ports,
shows their families/help, toggles port chrome and graph execution, cancels work
and refreshes bounded execution reports. Physical inspector publication runs on a
later UI turn: disposing a checkbox inside its own change notification is unsafe.
Its revocable queue receiver retires before the renderer/host. The private form
document excludes application scenes. One effective item snapshot is reused
until exact authored content or scene revision changes. Initial context validation
and full form reconstruction remain expensive; no production latency or memory
budget is claimed.

`tools/build.ps1 -Target tool-canvas -ToolCanvasEvidenceDirectory <fresh-directory>`
runs portable/source/history and actual native generated-companion checks, then
stages matching browser consumers for HTTP execution. Full wire creation/removal,
drag/keyboard connectors, script palette/editor, richer port discovery/MCP edits,
independent-project breadth, operation coverage and observing delivery remain
open. This is an executable tool and port inspector, not completed Tool Canvas.

## Asynchronous host operations

`INyxAsyncLogicOperation` is an optional capability beside the original operation
interface. `BeginInvoke` belongs to the supplied UI scheduler and may reply inline
or later. Its managed completion port accepts one complete frame or failure; its
invocation token disconnects pending work. Implementations marshal replies to the
UI scheduler. Cancellation and retirement suppress output rather than fabricate a
successful result. Already entered external side effects cannot be rolled back.

Built-in graphs expose optional `INyxResumableFlowGraph` continuations. A cursor
owns exact inputs, completed frames and budgets for one run. `Accept` validates
the complete pending output before advancing; `Result` refuses until every step
succeeds. Synchronous `Evaluate` refuses async graphs before entering any step.
Pure graphs keep the existing query-worker execution path.

`TNyxFlowExecution.Submit` coordinates async graphs on the UI scheduler. Each host
reply resumes on a later UI turn. Asynchronous pure steps use native workers or
the browser event loop; Sequential/UIQueue keep them on the UI queue. Threaded
refuses an async host graph. A temporary owned run lease and revocable weak ports
retire on explicit cancellation, generation retirement or scheduler shutdown,
including when the caller drops its run without polling. No partial result
reaches the ordinary final publication port.

`nyx.logic.http` supplies `INyxHTTPCall` and `INyxUTF8Decode`. The HTTP component
accepts a data-only `TNyxHTTPRequest` and optional explicit `INyxHTTPTransport`:

```pascal
LRequest := NyxHTTPRequest(NyxResourceURL('https://example.invalid/api/orders'))
  .Method(nhmPost)
  .Header('Content-Type', 'application/json')
  .WithBody(NyxEncodeUTF8(LJSONText))
  .WholeRequest(15000)
  .MaximumBytes(262144);
LRequestLogic := NewNyxHTTPCall(LRequest, LTransport);
```

Methods have enum choices and a distinct validated extension reference. Headers
are ordered immutable name/value pairs; responses retain duplicate observable
fields. Protocol token/Latin-1 admission, field/byte limits, request persistence
and detached body bytes share native/browser behavior. GET/HEAD bodies and
host-managed headers refuse at this portable boundary. These choices follow the
[Fetch method/header contract](https://fetch.spec.whatwg.org/). HEAD response
framing follows [HTTP semantics](https://www.rfc-editor.org/rfc/rfc9110.html#name-head):
its Content-Length describes the GET representation, not a received body.

Typed HTTP ports expose trigger, body bytes, status, headers, error, success and
header visibility. Header visibility carries `TNyxHTTPHeaderVisibility`'s ordinal,
distinguishing native headers from Fetch's observable filtered subset. A complete
non-2xx response retains its status/body and empty transport error. A transport
failure is usable output data; success requires both complete transport and 2xx
status. An unconditional downstream decode/parse can still fail on an error body;
connected outcome handling remains the caller's explicit choice.

`NewNyxHTTPLogicResolver` reconstructs exact graph configuration while injecting
transport authority; it also resolves the decoder and existing pure built-ins.
Saved request/graph data never supplies that authority. Direct `BeginInvoke`
requires a completion receiver and scheduler. Ordinary synchronous Invoke refuses.
`NewNyxBrowserHTTPTransport` and Win32 `NewNyxNativeHTTPTransport` extend the
existing resource byte/deadline/cancellation pipeline through an optional interface,
preserving the original resource GET interface. Request bodies remain owned until
native handle retirement. The current one-shot transport inherits the resource
adapter's cache bypass behavior. Tunable cache/retry, other native widgetsets,
real network qualification and full HTTP acceptance remain required.

The default Studio catalog still contains eight operations. HTTP/decoder toolbox
fields, crafted specialized generation/admission, semantic authoring, table
publication and observing delivery remain open; standalone explicit graph-wire
reconstruction does not establish those editor outcomes.

`tools/build.ps1 -Target logic-hosts -LogicHostsEvidenceDirectory <fresh-directory>`
runs native continuation/component checks with an explicitly substituted transport.
It compiles/stages matching HTTP browser checks and real network consumers; it
starts no server. `logic-async.html` and `logic-http-contract.html` qualify the
portable contracts when executed. The separate Pascal `nyx_http_logic_fixture`
producer and `logic-http-network.html` consumer require an independently admitted
loopback host. Compilation/staging never counts as network execution. WORK.md
records the current rejected producer launch and exact remaining boundary.
