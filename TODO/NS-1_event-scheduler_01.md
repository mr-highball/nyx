# Typed event registrations and scheduler

[Milestones](../MILESTONES.md) · [Catalog](README.md) · [Task flow](../TASKFLOW.MD)

## Description

NS-1 owns fluent typed multiple callback registrations and their execution policy
through a portable scheduler abstraction. The user requires complete supported
control event metadata, including focus entry/exit lifecycle callbacks, rather
than the current click/change scalar event bridge alone. Native LCL single-handler
slots and browser event listeners adapt to a shared registration contract. Credit:
Pending assessment. Studio consumes this contract through NS-4 authoring.

## Acceptance Criteria

- Public strongly typed event references and fluent registration objects support
  multiple independent callbacks, registration identity, ordered execution,
  removal and safe lifetime/cancellation. Supported lifecycle events and property
  metadata cover each control; extensions can publish their own schema.
- A scheduler interface defines sequential and asynchronous policies, ordering,
  cancellation, failure handling and UI-thread dispatch. Actual native threaded
  execution and browser-supported asynchronous execution have explicit capability
  evidence; unsupported threading is reported, never silently simulated.
- Mutation during dispatch, reentrancy, disposed controls, failed callbacks and
  navigation cannot cause stale calls, hidden lifetime cycles or partial policy
  updates. Both-target fixtures and actual LCL/browser controls exercise these
  cases, including OnAfterEnter/OnAfterExit and multiple callbacks.
- Persistence, history, generated specialized source and companion compilation
  retain registration identities/policies and authored handler names/comments.
  Wrong callback/policy types fail compilation on both targets.

## Blockers

- [Specialized managed component contracts](DONE/NS-1_component-interfaces_01.md)
- [Persistence and scalar state](DONE/NS-1_persistence-state_01.md)

Return path: NS-4_studio-authoring_01 must consume the accepted contract in tabbed
Properties / Events UI, add-handler source navigation and confirmed removal.

## Scheduler/runtime delivery — 2026-10-03

Scheduler criterion 2 is accepted for the implemented Windows FPC/LCL and pas2js
targets. nyx.scheduler defines sequential, asynchronous, deferred UI and explicit
threaded policies, advertised capabilities, owned execution diagnostics,
cooperative cancellation and worker-to-UI submission. Checked native execution
passes 28 scheduler/registration checks with real worker thread identities,
running threaded cancellation and UI handoff; heap tracing reports 500
allocations/frees and zero unfreed blocks. Executed browser checks pass 25,
including explicit rejection of worker-only policy and independent async events.
Each compiler rejects wrong callback-interface and policy arguments among 26
intended negative cases. Older FPC's ForceQueue inline behavior and worker queue
removal were discovered and addressed with a real threading bootstrap and
UI-thread admission. No manual RTL flags or fake browser threads are used.

The runtime router supplies multiple independent registrations, ordered sequential
dispatch, mutation snapshots, reentrancy, removal, owned failure results and view
scopes. Native/browser real-control journeys each exercise 14 focus/click/change,
removal, queued cancellation, retained payload and navigation/child-work checks.
Callback execution contexts retain cancellation leases without retaining their
callback objects. Focus domains persist and compile through recovered specialized
companions; generated browser reconstruction passes 32 checks. See
[events](../docs/events.md) and [WORK](../WORK.md) for exact artifacts.

This task remains open. Criteria 1, 3 and 4 still require complete supported
property/event schemas including extension publication, the remaining event
integration/admission boundaries, persisted handler identities/policies and
compiled authored callbacks. Studio tabs/stubs/navigation/confirmed removal have
not been delivered. Next batch integrates descriptor/schema authoring and the
Nyx-built inspector, following the accepted-companion compiler prerequisite for
executing edited handlers. Preserve all original criteria. Native worker pooling
and performance qualification retain the production/performance owner; the
current scheduler backend creates one worker per async submission. No full task,
Studio or product completion credit is claimed.

## Authored descriptors and inspector delivery — 2026-10-03

Criterion 4 is accepted for the supported version-1 event contract. Typed handler
and registration references, immutable descriptors, ordered registrations and
per-event policies survive persistence, paired history, specialized fluent source
admission and compiled companions. Handwritten callback names/comments remain
outside the managed builder. Actual compiled callbacks execute on both adapters;
HTTP builds retain their exact frames at application, page and reusable scope.
Each compiler rejects wrong handler/registration families as well as callback
interfaces and execution policies. Evidence and commands are in WORK.md.

Studio now consumes the contract in its shared Nyx Properties / Events inspector:
visible ordered registrations, policies, add-handler TODO stubs, source-line
navigation and exact warning/confirmation removal. Pending drafts are protected.
Inherited edits affect only the selected instance/part, and removal keeps the
implementation. All shared typed attributes are exposed; closed choices derive
from their enums even on leaf controls. Extensions publish owned property/event
schema snapshots. Browser and native common click bridges now include labels
and framed inputs; existing global callback behavior is retained.

Criteria 2 and 4 are accepted; criteria 1 and 3 remain open for complete
property/target capability qualification and the remaining extension/event
integration boundaries. Four runtime trigger families are exercised; broader
key/pointer/drag and named extension callback families need their own schema,
scheduler/admission and actual adapter journeys. Runtime tests establish current
mutation/reentrancy/failure/cancellation behavior; compiled application coverage
must advance with those families. Do not declare complete event breadth or task
completion from this delivery. Consecutive batches without criterion closure: 0,
because criterion 4 closed. The Studio return path is integrated; next bounded
deliverable should extend the event/capability contract with one complete
keyboard family and actual browser/LCL consumers, preserving supported behavior.
Stop if new hooks cause duplicate dispatch, stale borrowed-widget calls or silent
capability fallback. Production worker pooling remains with performance work.

## Keyboard/input response delivery — 2026-10-03

Typed OnKeyDown/OnKeyUp now join the authored/runtime contracts, schema, Studio
controls and compiled companions. Owned key/modifier/repeat snapshots use enum
identities and exact shortcut matching. An active sequential input invocation may
consume the default through INyxEventResponse; retained/cancelled/queued/threaded
contexts explicitly refuse consumption. Siblings remain ordered, and callback
failures do not suppress them. Native button activation is ordered after callbacks;
consumed down/up keys pass actual CN messages while unhandled activation remains.
Current text, semantic compound routing, repeat/focus reset, composition bypass,
navigation and pending-work cancellation have actual adapter evidence.

See WORK.md: 55 native / 52 browser scheduler checks; 35 real event/control checks
per adapter; 79 shared descriptor/source checks; 17 native / 11 browser compiled
callback checks; 49 desktop / 49 exact-390-pixel Studio and 33 native authoring;
96 HTTP checks including accepted keyboard-containing companions on both targets.
No full criterion/task closure is inferred. Criteria 1/3 remain for complete
capabilities and further text/pointer/drag/extension families; 2/4 remain accepted.
Consecutive batches without further criterion closure: 1. Continue wider event
capabilities with that owner after the codegen paired-project return; retain
strict duplicate/stale/unsupported-policy stop conditions and production pooling.

## Interaction families and per-phase contracts — 2026-10-04

The canonical registry now contains 23 runtime families. Typed named fluent
methods, schema descriptions/capabilities, persistence, specialized companions,
Studio TODO navigation and adapter dispatch include independent before/main/after
key-down, key-press and key-up, text proposals/acceptance, double click, pointer
down/up/move/enter/exit and context menus. Key-press is logical actuation; exact
text edits cover typing, paste, composition and virtual keyboards. Signal-only
phase contracts retain input data without inventing scalar values. Every phase
captures its own declaration, guarded against callback navigation.

Both actual targets execute 37 compiled-fixture interaction checks. The portable
packet passes 77 on native/executed pas2js, reconstructs all named methods and
distinct phase contracts, and frees every native allocation. Existing native
35 event, 50 binding, 17 compiled callback and 69 Studio checks remain accepted;
browser Studio passes 67 desktop and 67 exact-390 cases. Original shared gates
remain green. The delivery record in WORK.md owns compiler/HTTP artifacts.

Review caught two inherited limits: callback descriptors and component scalar
contracts still admitted only four triggers. Both are now bounded by the canonical
registry, retaining distinct-runtime-trigger and callback-budget admission.
Actual phase tests also caught payload reuse from the main event; fresh owned
capture now honors each phase's declaration while preserving physical input data.

Criteria 2/4 remain accepted; original criteria 1/3 remain open. Production
wheel/scroll, drag/drop, rich selection, composition detail, named extension
callbacks and complete target/property capability qualification keep their scope.
Consecutive deliveries without further criterion closure: 2. Reassessment ends
isolated hook expansion. The next event-owner delivery must qualify the complete
supported contract against its actual consumers: publish the control/property/
event matrix, address declared-but-unbridged capabilities and extend the explicit
extension/admission boundary. Preserve the original criteria and scheduler
performance owner. Stop on duplicate routes, stale suppliers, silent unsupported
policies or changes to accepted design/source pairs; do not claim closure from
another list of event names. Split-pane acceptance has its separate NS-2 owner.

## Capability qualification delivery — 2026-10-04

The reassessment produced an integrated consumer delivery: typed immutable
property meanings/support, a generated matrix for all 76 default catalog kinds,
Studio field help and touch-visible unavailable-property explanations, and
bounded semantic MCP support queries. Actual admission work resolves inherited
read-only on scalar controls and unbound compound targets. Definitions/custom
schemas remain owned snapshots; published descriptions do not manufacture an
adapter bridge. Target and platform-scope gaps are stated explicitly.

Evidence in WORK.md includes 171/171 shared interaction checks, 45/45 actual
interaction controls, 84/84 callback/source checks, 69/69 browser Studio and 71
native Studio checks, 44 wrong-type rejections per compiler, 173 delegated HTTP
and 30 real MCP HTTP checks. Desktop and exact-390 rendering were inspected.
Existing source/history/lifetime/scheduler gates remain green.

Criteria 2/4 remain accepted. Original 1/3 remain open for the full extensible
event outcome and integrated lifecycle/admission qualification; this packet does
not infer complete event breadth or blanket standards conformance from a matrix.
Consecutive deliveries without further original event criterion closure: 3.
The selected matrix/policy delivery is finished. Reassessment preserves all
original criteria and changes the next deliverable to the named extension-event
contract through actual browser/LCL producers, scheduler admission, Studio and
compiled companions. It must support owned payloads, independent registrations,
explicit target capabilities, mutation/navigation/disposal and paired source.
Do not resume isolated hook additions. Wheel/scroll, drag/drop, selection,
composition detail and production worker pooling retain their required scope.
Stop on duplicate routes, stale suppliers, silent policy fallback or damaged
accepted source/design pairs; none was observed in this delivery.

## Named producer and lifecycle qualification — 2026-10-04

Named streams now use exact typed references and independent registrations and
policies. Creator schemas declare signals, scalar domains or structured data
kinds and target grades. Both actual adapters provide managed emitter ports to
custom factories. Ports are dormant until atomic candidate admission, revoked
before disposal, and retain no renderer/node/widget. The scheduler now exposes
and enforces UI access; a real native worker is refused before producer entry.
Wrong/undeclared/unavailable payloads fail before dispatch. Disabled/hidden
ancestry suppresses notifications; read-only preserves deliberate notifications.

The same contract crosses Studio event cards, descriptions/payload help,
multiple TODO handlers, source navigation, confirmed removal, paired history,
strict persistence/source admission and executable companions. MCP event pages
now return only their exact streams' separately bounded callback windows,
including policy and explicit partial-descriptor marking.

Original criterion 3 is accepted for the exercised Windows/LCL and pas2js
targets. Current actual-control qualification passes 71 native and 69 browser
checks, with handwritten companion execution, cancellation during dispatch,
reentrancy, owned failure diagnostics, failed candidate preservation,
navigation/remount/destruction and queued-generation cancellation. Both native
preparation/compiled runs have zero unfreed allocations. Existing 35 native
event controls, 45/45 phase controls, 55 native/52 browser scheduler checks,
84/84 callback/source checks, 71 native Studio and 69 desktop/69 exact-390 Studio
checks remain green. Forty-six intended type rejections pass per compiler;
173 real compiler-service and 33 real MCP checks, including PNG rendering, pass.
WORK.md records exact artifacts, failed attempts and deployed identity.

Criteria 2/3/4 are now accepted; criterion 1 remains open for complete supported
control/event breadth and integrated default component semantics. Consecutive
deliveries without further original criterion closure reset from 3 to 0.
The selected named-extension packet is delivered; the task stays open. Preserve
wheel/scroll, drag/drop, rich selection, composition detail, default compound
semantic discovery and production worker pooling. Continue toward the full
supported contract rather than another isolated event-name list. Codegen's
separate original large-project owner/counter11 is unchanged.

## Wheel requests and actual viewport notifications — 2026-10-04

The next integrated criterion-1 packet is delivered. Owned immutable wheel and
viewport snapshots retain units, fractional deltas, modifiers, physical
cancelability and control-local offsets/ranges/page sizes. Before/main/after
wheel phases reuse the existing scheduler, decision and navigation guards.
Scroll is an observation of movement rather than an inferred wheel action;
scroll completion uses the actual browser host event when available. Native
notifications are coalesced at UI idle and explicitly Basic; native completion
is Missing, with no timer-based gesture approximation. Native scrollbar, grid
and list positions expose their actual units rather than pretending to be pixels.

Actual scroll, memo, code editor/block, list, table and tree controls consume the
contract on both targets. Native scroll uses a real LCL scroll box, and native
code blocks now expose both scrollbars. Browser fixed-height faces retain their
overflow through synchronization. Admission/teardown own one native UI-idle
observer per view and revoke browser hooks. Navigation, detached DOM and renderer
destruction during notification are exercised. Studio TODO/navigation/history,
crafted fluent companions and bounded MCP capabilities consume the canonical
schema. Source admission now resolves fluent methods through the runtime registry
instead of a separate hardcoded list. The generated reference covers all 76 kinds.

Evidence in WORK.md: 46 native preparation/compiled checks, both zero unfreed
blocks; 50 executed browser checks, including genuine host scroll/scrollend;
186 native/browser interaction checks and 45 actual controls per adapter;
84 callback/source checks per runtime, 71 native Studio and 69 desktop/69 exact-390
Studio checks; 48 intended type rejections per compiler; 173 delegated HTTP and
33 real MCP transport checks including PNG rendering. Existing native core,
scheduler, ownership, binding and generated reconstruction gates remain green.
Only exercised Win32/LCL and matching pas2js/Edge behavior is qualified; no full
standards, widgetset, physical-phone or performance claim is inferred.

Original criteria 2/3/4 remain accepted; criterion 1 remains open. Consecutive
deliveries without further original criterion closure advance from 0 to 1.
The task and full product stay open. Next qualification should integrate rich
selection and default component semantic discovery through actual collection
controls, reusable recipes, source/Studio and agent consumers. Preserve native
completion, drag/drop, composition detail, pointer cancellation/capture, worker
pooling and complete native Studio. Stop on stale routes, duplicate dispatch,
silent unsupported-policy fallback or damaged accepted pairs. Codegen's separate
original large-project owner/counter11 is unchanged.

## Managed collection selection and checkpoint reassessment — 2026-10-04

The next integrated criterion-1 packet is deployed. Typed immutable selection
separates membership, focus and anchor; collection-scoped identities survive
updates/reorder and are pruned on removal. Single selection preserves its v1
descriptor; explicit Multiple uses v2 within the existing design-v3 envelope.
Actual list/table/tree controls support modifier-assisted navigation, toggles,
ranges and select-all. Tree navigation excludes collapsed descendants. Tables
keep editor/cell movement separate from row membership. Before-key consumption,
read-only/disabled policy, independent instances and renderer disposal retain
the existing router/lifetime contract.

OnSelectionChange is an accepted-publication observation with immutable before
and after snapshots, ordered registrations and the normal scheduler policy.
Studio Bindings/Events, TODO/source navigation, exact paired history, crafted
companions and bounded agent metadata consume it. Target refresh compares typed
scalars per cell: selection, unrelated publications and normalization of another
cell preserve unfinished edits. Undefined/foreign edits reject without a
secondary normalization failure. The generated reference remains 76 kinds;
the runtime registry now contains 29 physical families.

WORK.md records 121 native preparation/compiled checks each with zero leaks,
136 executed compiled-browser checks, 27 shared / 29 native / 30 browser
collection authoring checks, 32 shared / 29 actual browser collection checks,
27 actual native collection checks, 189 interaction contracts and 45 actual
controls per runtime, current desktop/exact-390 Studio resize evidence, 173 HTTP
and 33 real MCP checks including PNG rendering. Intended type refusals are 50
per compiler. Hardware/assistive technology, type-ahead, paging, full grid cell
navigation, other widgetsets and production performance remain unqualified.

Original criteria 2/3/4 remain accepted; criterion 1 remains open. Consecutive
deliveries without further original closure advance from 1 to 2. Reassessment:
wheel/viewport and selection delivered usable integrated behavior, but neither
completes the full supported schema/default semantics requirement. The next
batch changes from isolated physical-family expansion to full-catalog recipe
semantic discovery using the already accepted physical routes and producer
contracts. Inventory every default recipe's routed names and typed payloads;
complete discovery through reuse/overrides and actual target controls, Studio,
source/history and bounded MCP pages as one deliverable. Do not reset the count
by renaming work or reduce the original criterion. Stop on descriptor/producer
drift, duplicate routes, stale receivers or damaged accepted pairs. Preserve
drag/drop, composition detail, pointer capture/cancellation, native completion,
complete keyboard/accessibility qualification, workers and native Studio. The
separate codegen counter 11 is unchanged, and the full goal remains active.

## Default compound semantic discovery — 2026-10-04

The selected counter-2 packet is now deployed. All 35 default compounds publish
55 expanded physical routes through owned semantic descriptors. A closed
38-name Pascal vocabulary drives recipes and crafted generated callbacks;
handwritten open event references keep their exact spelling. Context realization
preserves siblings, inherited callbacks, reusable instance/part overrides and
nested compounds. Scalar optionality, differing route contracts and declared
creator producers remain explicit; discovery cannot forge custom Emit permission.

Studio exposes semantic cards first, route/payload help, multiple TODO handlers,
source navigation, policies, warned removal and paired history. Callback controls
fit both desktop and phone Inspector cards. Agents independently page routes,
events and registrations with bounded totals/partial flags. Native preparation
and compiled companions execute 583/584 checks with zero leaks; the compiled
browser companion executes 582. Actual Studio journeys pass 15 per desktop and
exact-390 width. Both compilers reject 51 wrong-type fixtures. HTTP passes 173;
the final executable passes 38 real MCP checks including PNG rendering. Creator
producer, core, native control and generated/source regressions remain accepted.
See [the evidence and deployment record](../WORK.md#default-compound-semantic-discovery--2026-10-04).

Original criteria 2/3/4 remain accepted; criterion 1 remains open. Consecutive
deliveries without further original closure advance from 2 to 3. Reassessment:
the bounded full-recipe discovery outcome is usable end to end, while complete
editing/composition and remaining physical interaction requirements still prevent
original criterion closure. The next deliverable changes to an integrated typed
editing-session contract: edit intent, selection and composition lifecycle in
real controls, generated handlers, Studio and bounded agent context. Follow
current Input Events ordering/cancelability and supported LCL hooks, with honest
target grades. Require owned Unicode snapshots, draft preservation and paired
source/history. Stop on premature composition publication, false cancellation,
stale receivers or damaged accepted pairs. Preserve drag/drop, pointer capture,
full keyboard/accessibility qualification, native completion, workers and native
Studio. No counter reset, criterion/credit transfer or narrowed replacement gate.
Codegen criterion 3 stays at counter 11; the full goal remains active.

## Typed editing sessions and checkpoint reassessment — 2026-10-04

The counter-3 integrated editing outcome is deployed. Five appended runtime
families expose OnBeforeEdit, OnCompositionStart/Update/End and
OnTextSelectionChange through public typed fluent callbacks. Owned snapshots
carry 46 closed intentions plus Unknown, optional data, physical text, scalar
selection and composition phase. Physical cancelability and portable model
rejection stay distinct. Unknown intent remains diagnostic data; native input
intent is never guessed from shortcuts.

Browser hooks preserve genuine beforeinput cancelability and IME drafts. Win32
LCL chains actual IME messages and drains the end result at idle; selection is
coalesced and direction unknown. Unavailable pre-edit and directed native
selection requests receive explicit refusal/help. Other widgetsets need bridges.
Navigation from a native text callback revokes slots and defers control release
until its active platform message finishes. Queued callbacks retain snapshots
without widgets, and stale views cannot publish final phases.

Studio Events/help, TODO navigation, multiple registrations, policy, warned
removal, paired history and legacy companion imports consume this contract.
Bounded MCP event pages expose owned context declarations from canonical schema.
Final evidence is 117/118 native preparation/compiled checks with zero leaks,
121 executed compiled-browser checks, 29 Studio checks per desktop/exact-390,
54 intended type refusals per compiler, 204 interaction contracts and 45 actual
controls per target, broad native regressions, 173 HTTP checks and 44 final-server
MCP checks including PNG rendering. Exact deployment and failure corrections are
in [the work record](../WORK.md#typed-editing-sessions--2026-10-04).

Original criteria 2/3/4 remain accepted; criterion 1 remains open. Consecutive
deliveries without original closure advance from 3 to 4. Reassessment: editing
is usable end to end, but physical IME/accessibility and remaining gesture
contracts prevent complete supported-event acceptance. The next bounded usable
delivery is complete pointer-gesture ownership and typed drag/drop through
actual reusable controls, target adapters, Studio/source/history and bounded
agents. Require capture acquisition/loss/cancellation, owned transfer context,
explicit unavailable grades and safe navigation/disposal. Stop on leaked capture,
duplicate delivery, stale receivers or damaged accepted pairs. Reuse current
editing/semantic evidence. Preserve full keyboard/accessibility and physical
device qualification, other widgetsets, native completion, worker pooling and
full native Studio. Codegen criterion 3 remains at counter 11. No original
criterion, credit or counter is weakened, transferred or reset; full goal active.

## Typed gestures and supported-control reassessment — 2026-10-04

The counter-4 gesture outcome is integrated and deployed. Ten appended typed
fluent events expose pointer cancellation/capture/loss and seven drag phases.
Immutable transfers retain protected format names or readable owned payloads,
bounded file metadata and exact Unicode. Specialized configuration declares
source/target opt-in and closed touch behavior. Active sequential responses seal
before return; ordered siblings, callback failures, cancellation and retained
contexts have both-target evidence. Native construction/navigation safely retires
controls under an independent host until cancellation and queued release.

Actual browser host input qualifies mouse/touch capture, cancellation, implicit
loss and real drag-store negotiation. Native internal mouse transfers have basic
support; source progress and external native file drops are honestly unavailable.
Studio Properties/Events/help, typed TODO/source navigation, warned removal and
paired history consume the public metadata. Bounded MCP publishes both contexts.
See [the work record](../WORK.md#typed-gestures--2026-10-04): native 83/84 with
zero leaks, compiled browser 78, Studio 52 each at desktop/exact-390, 59 intended
wrong-type cases per compiler, 30/1537 shared core/designer, 234 interaction
contracts and 45 actual controls per target, 177 HTTP and 55 MCP checks with PNG.

Original criteria 2/3/4 remain accepted; criterion 1 remains open at consecutive
no-closure counter 5. Stop isolated event-family experiments. Next integrated
delivery is keyboard-operable rich selection and compound actions, retaining
focus and coherent disabled/read-only/selection behavior with honest accessibility
metadata, Studio/agent help and actual consumers on both targets. Stop on an
inaccessible primary action, duplicate delivery, lost focus or damaged accepted
pairs. Full physical-device/accessibility, other widgetsets, native external
transfers, workers and native Studio remain open. No criterion or credit is
weakened/transferred and no counter resets. Codegen criterion 3 stays at 11.
The user's subsequent request gives immediate priority to enabling direct Codex
MCP use and making semantic tools the normal demo/design workflow; retain the
supported-control return path after that connection prerequisite.

## Integrated keyboard review — 2026-10-04

The selected counter-5 outcome is qualified and deployed. Bound browser
collections now retain one Tab entry, transfer physical focus after removal,
keep an empty host reachable and exclude disabled controls. Read-only text cells
remain inspectable. F2/Enter, cell Tab traversal, Escape and boundary exit have
actual host-key evidence. Standard compound buttons deliver one semantic action
through Enter/Space; disabled descendants are skipped. Both-target consumers,
Studio and bounded MCP intent help share the public contracts. Native selection
preparation/compiled journeys pass 154 each with zero leaks; executed browser
passes 181; the MCP-authored native companion is also leak-free. Studio passes
52 each at desktop/exact-390, HTTP 177 and MCP 55. See the
[work record](../WORK.md#mcp-authored-keyboard-review--2026-10-04).

Original criteria 2/3/4 remain accepted; criterion 1 stays open at consecutive
no-closure counter 6. This is row-oriented navigation, not complete cell-grid
navigation, typeahead, assistive technology, hardware/IME or other-widgetset
qualification. No original requirement or credit transfers or resets. The
user-prioritized semantic callback workflow is now the concrete prerequisite:
the review's binding and deletion handler cannot yet be authored through MCP.
Follow its existing owner, then return to the complete supported-control outcome.
Codegen criterion 3 remains open at counter 11; the full goal remains active.

## Full-catalog focus/keyboard concordance — declared 2026-10-04

Counter-6 reassessment changes the qualification from a selected review page to
every default catalog kind and its expanded reusable/compound parts. The current
schema advertises keyboard/focus events on literal list/table/tree faces whose
browser projections lack the same entry point as LCL. A code block's disabled
property also leaves its browser Tab entry intact. These concrete findings require
an integrated adapter repair and catalog-wide consumer, not another hook list.
One MCP-authored companion must compile unchanged for both targets; typed
classification, ordered independent callbacks, actual focus/keys, disabled and
read-only transitions, inherited policy, and existing bound-focus ownership must
agree with published metadata. Budget and publication stops are in WORK.md.
Original criterion 1 and counter 6 remain open until the intended scope is proven;
criteria 2/3/4 retain their accepted evidence. Broader accessibility, physical
devices, widgetsets and performance retain their owners and requirements.

## Full-catalog focus/keyboard delivery — 2026-10-04

The declared catalog journey is qualified and deployed. A closed typed
classification now agrees with actual focus surfaces, including literal
inspection controls and the split divider. Borrowed `FocusFor` contracts retain
the separate meaning of scalar `InputFor`. Split key hooks precede defaults,
read-only/fixed grips remain inspectable, disabled faces lose entry, and bound
browser collection focus crosses the logical owner boundary once. Native radios
retain one eligible peer entry. Browser checked-but-disabled/hidden groups use
their existing label to delegate to the real input without changing its value.
Creator hooks and attachment-owned entry remain intact.

Actual MCP composes 76 catalog kinds and a peer page in six paired groups;
bounded service metadata agrees with the compiled catalog at revision 7. Exact
exported companion bytes match both actual compiler jobs. Windows LCL checks
103 physical faces / 30,637 assertions, with zero unfreed blocks. Host Tab/F8
passes 23,434 assertions at desktop and exact 390-by-844 each, including ordered
independent registrations and read-only/disabled/re-enable/cancellation cycles.
Focused splitter, bound collection and radio policy consumers pass too. Original
editing/gesture/extension/source evidence remains applicable. See
[the packet](../WORK.md#full-catalog-focuskeyboard-concordance--2026-10-04).

Criterion 1 remains open at consecutive no-closure counter 7. Focus/keyboard
concordance does not establish complete property/target capability concordance,
full cell-grid/typeahead, assistive technology, hardware/IME or another widgetset.
Criteria 2/3/4 retain accepted status; codegen criterion 3 stays at 11. No original
criterion, scope, counter or credit transfers or resets. Next reassessment must
compare the complete criterion with existing property/extension/event evidence
and produce a finite property/projection conformance matrix before more adapter
work; avoid another isolated hook-list batch. Full goal remains active.
