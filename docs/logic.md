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

Direct Pascal calls need no canvas, interpreter, document or scheduler:

```pascal
LProfile := NewNyxJSONSelect(NyxDataPath.Field('profile'))
  .Select(NewNyxJSONParse.Parse(LSourceText));
LPretty := NewNyxJSONFormat(njlIndented, 2).Format(LProfile);
```

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
9031. Canvas-item attachment, visual route editing, control/script port breadth,
file/regex/HTTP operations, activation/host authority, full budgets and preserving
delivery remain separate requirements. This batch does not complete Tool Canvas.

`tools/build.ps1 -Target logic-authoring -LogicAuthoringEvidenceDirectory <fresh-directory>`
executes shared authoring, real native controls and authenticated HTTP semantic
checks. It stages browser counterparts **and their matching source worker**.
`-LogicAuthoringMCPPort` selects an unused localhost qualification port (9029
default); its fresh private runtime never enrolls or replaces the live project.
WORK.md owns actual HTTP browser execution, retained failures and scoped limits.
