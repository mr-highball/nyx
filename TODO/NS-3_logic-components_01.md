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
