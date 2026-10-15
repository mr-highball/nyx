# NS-1_canvas-flow_01 — Typed embedded-control and script behavior graphs

[North stars](../MILESTONES.md) · [Task catalog](README.md) · [Task flow](../TASKFLOW.MD)

**Description:**

The user's 2026-10-13 enhancement makes embedded Nyx controls optionally expose
input/output ports. Designers retain static compiled behavior or enable end-user
graph wiring from events/values to other controls and interpreted scripts. NS-1
owns the typed port/graph/runtime contract. Visual diagram edges remain a separate
representation: drawing a line never silently creates behavior. NS-3 canvas/diagram
controls and NS-4 Studio/MCP consume this same contract. This newly required scope
earns no planning or duplicate component credit.

**Acceptance Criteria:**

- Specialized managed interfaces expose distinct stable port references,
  direction, data-versus-event/action semantics, payload/value type, help,
  multiplicity and creator-defined names. Component properties, published event
  payloads and commands supply discoverable capabilities. Unsupported/read-only
  inputs are not fabricated. Scripts declare typed input/output schemas.
- Typed authoring enables behavior wiring and shows/hides ports per workspace
  or item. Visibility, graph execution and static callbacks are separate choices.
  Ordinary controls remain usable with port chrome hidden; explicit mixed
  callback/graph policy prevents accidental duplicate execution.
- Wire admission validates the whole candidate graph, exact owners and type
  compatibility. Conversions require explicit typed transforms. Missing ports,
  incompatible payloads, cardinality conflicts, cycles and retired controls
  refuse or follow a documented explicitly selected feedback policy. Cycles
  never become an accidental infinite execution loop.
- Runtime defines data propagation, event order/fan-out, grouped writes,
  reentrancy, errors, cancellation and budgets. Nyx scheduler policy governs
  sequential/asynchronous/worker work and UI publication. Independent runtime
  instances own revocable subscriptions without control/document/renderer cycles
  or stale deliveries after navigation/disposal.
- Public Nyx canvas/diagram controls let users drag outputs to compatible inputs,
  with diagnostics, keyboard/accessibility alternatives, enablement and confirmed
  removal. Scripts receive control events/data and publish typed control updates.
  User extension requires no library fork.
- Versioned persistence, crafted Pascal and source admission retain schemas,
  edges and programs. Bounded semantic queries expose ports/neighborhood/errors;
  grouped expected-revision edits are one undoable operation. Pending drafts and
  failures preserve the accepted pair.
- English event-to-script-to-control and data-to-control examples execute in
  actual browser/LCL applications with multiple registrations, independent
  instances and retirement. Budgets, desktop/narrow presentation and observing
  rollout are qualified separately from graph-only fixtures.

**Blockers**

- [Managed component interfaces](DONE/NS-1_component-interfaces_01.md).
- [Owned document model](DONE/NS-1_model_01.md).
- [Canvas behavior programs](NS-1_canvas-behaviors_01.md).

Canvas is a related host consumer, avoiding a cyclic prerequisite for the graph
engine. Actual control/script/canvas integration is still required for acceptance.

Current-source executable foundation (2026-10-14) adds typed value/signal ports,
immutable admitted DAGs, exact standalone wire signatures/configuration and
scheduler-owned grouped state publication. JSON/text chains execute in ordinary
browser/LCL controls with multiple callbacks, cancellation, stale revision and
retirement checks; WORK.md owns exact counts and limits. This does not yet supply
control/script port discovery, interactive wiring, feedback or semantic authoring.
The next batch adds immutable document declarations, version-14 persistence,
typed companion/source admission, focused page/reusable ownership and paired
history. Native/browser compiled base and handwritten continuation reconstruct
and execute exact graphs; WORK.md owns counts and limits. Graph no-closure count
is two at that boundary. The next batch adds public Logic workbench configuration,
typed port choices, grouped final-graph validation and bounded semantic queries/
transactions, with ordinary native/browser source-command input and authenticated
HTTP evidence. WORK.md owns exact counts and rollout limits. Graph no-closure is
now three; no original criterion/DONE/aggregate closes. Next connect discoverable
control/event routes and Tool Canvas hosting; stop standalone authoring variants.

Current-source saved routes (2026-10-14) now connect ordinary physical/named
control events to typed graph inputs and grouped scalar state outputs. Both
application hosts attach weak, revocable observers; explicit mixed callback
ordering, bounded concurrency, cancellation and generation/revision guards retain
ordinary event policies. Typed source, complete paired history, compiled base/
handwritten continuations and bounded semantic route edits have scoped evidence
in WORK.md. Graph no-closure advances to four; no original criterion closes.
Next attach these routes to embedded canvas item ownership and Tool Canvas port
presentation. Script/command/context breadth, gesture wiring, feedback, budgets
and preserving delivery remain required; stop ordinary route/fixture variants.

Current-source embedded attachment now uses exact item routers, shared application
state and explicit resolver authority, revoking routes before item/page retirement.
The public Tool Canvas exposes immutable event/state port inspection and separate
graph pause/cancellation alongside static callbacks. Exact generated companions,
ordinary source/history and actual native/browser input have scoped evidence in
WORK.md. Graph no-closure advances four → five; no original criterion closes.
Return to useful Logic operation breadth and connection creation/removal; full
script/context/gesture/budget/delivery outcomes above remain required.

Current-source collection Logic adds real filter/sort/map/pagination to those
graphs, preserving typed scalar families and exact numeric values. Actual
native-worker/browser-event-loop publication, public source-command controls,
paired history and authenticated semantic composition are qualified in WORK.md.
Graph/Logic no-closure advances five → six; no original criterion closes.
Next advance host-service capability/outcome contracts and data/table publication;
full operation/feedback/interpreter/wire/budget/delivery scope stays required.

Current-source optional continuations now suspend/resume asynchronous host steps,
admitting exact output frames/budgets before advancing and exposing no successful
prefix. Scheduler-owned completion handles inline/late/duplicate replies, owner
retirement and dropped run handles. Native/browser contract evidence is in
WORK.md; the HTTP provider is explicitly substituted in component checks. Native
workers retire before console heap finalization. General HTTP adapters compile,
but the owned producer launch was rejected before execution; no alternative
startup is attempted. Graph/Logic no-closure advances six → seven without original
criterion/DONE/aggregate credit. Return to real HTTP host qualification, tunable
retry/cache and ordinary typed source/toolbox/semantic authoring, then guarded
data/table publication. File capability/activation, full operation breadth, wires,
interpreter, budgets/parity and delivery remain required.

The HTTP authoring continuation now consumes that async foundation through exact
saved/typed source and ordinary source commands. The public Logic collection has
ten kinds; request forms preserve exact headers/body bytes and unfinished drafts.
Bounded in-process semantic queries/transactions and exact generated application
routes have actual native/browser evidence with substituted transport. WORK.md
owns limits, counts and protected-state preservation. Graph/Logic no-closure
advances seven → eight without original criterion/task/DONE credit. Return to
network/cache/retry and guarded table/data outcomes; stop authoring variants.
Real host behavior, full flow/script/wire/budget/parity and delivery remain open.

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
