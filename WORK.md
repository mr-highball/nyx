# Current work

[Project](PROJECT.md) · [Milestones](MILESTONES.md) · [Tasks](TODO/README.md)

The full user outcome remains in MILESTONES.md. Execution is solo. Product and
substantive tools are Pascal in Delphi dialect, with thorough comments and blank
lines above if blocks. Semantic MCP is the primary demo/design/build workflow;
actual browser/LCL consumers qualify physical behavior selectively.

## Current return path: recipe editor and semantic release — 2026-10-06

Previous goal turn: progress. The compiler lifecycle implementation/evidence and
its handoff reached the verified remote branch. The reassessment keeps the
original NS-5 criteria open: direct-process retirement cannot establish helper
retirement. Complete that concrete boundary instead of adding queue/owner cases.

Current batch: NS-5 criterion 1, owned compiler-family retirement on Windows.
Deliver an invocation-owned job before the suspended compiler runs, with no
breakaway/fallback; include helpers in execution budgets, normal completion and
cancellation retirement. Qualify actual child/grandchild processes, compiler-exit
before helper-exit, cancellation, deadline/log failure and all-job shutdown, then
real pas2js/FPC compatibility. Stop at that integrated process-family boundary.
This is one native compiler-host change; the portable authoring contract stays
unchanged. Other hosts, UI cancellation and observing deployment remain gates.
Preserve all protected services/pairs; no listener/restart/enrollment or rollout.
OS failure to reap a terminated child must retain ownership and remain active;
never claim a terminal result or bounded OS shutdown from a timeout alone.

Current return: Windows compiler-family retirement below is qualified. Stop
process-family fixture expansion. Next deliver bounded operator job discovery
and visible cancellation in both Studio controllers in NS-5; the browser's
legacy synchronous HTTP build route must join the semantic job workflow before
claiming editor parity. Other hosts, crash-atomic compiler creation and OS
retirement refusal remain hardening/qualification gates. Protected history and observing
HTTP rollout remain separate gates. Workflow no-closure stays 10; no earlier
count is reset or an unrecorded NS-5 count invented. Original criteria stay open.

## Windows compiler families — 2026-10-06

NS-5 criterion 1's next lifecycle boundary now owns an unnamed Windows job for
each compiler invocation. The native adapter creates suspended, assigns before
resume and refuses incompatible job admission without an uncontained fallback.
Its handle is not inherited and breakaway is not allowed. Active family accounting
includes descendants after the compiler exits, independently of inherited pipe
handles. Normal completion waits for the whole family; cancellation, deadline,
log/read failure and shutdown terminate the family and join the exact compiler
before worker completion/slot reuse. A compiler's nonzero exit preserves its
compiler failure instead of waiting for stranded helpers to mask it as a timeout.
The portable document/component contract and immutable build receipts are unchanged.

Evidence: `build/compiler-family/` (ignored private artifacts), current source.
`tools/build.ps1 -Target compiler-lifecycle` passes **129** checked FPC 3.2.0
actual family/queue/semantic lifecycle checks, **47** retained compiler-job checks
and **56** portable admission checks, all with **zero native leaks**. The owned
Pascal fixture publishes actual living compiler/helper/grandchild handles before
opening its gate. Cancellation after compiler exit, normal helper completion,
compiler failure, family deadline/log failure, slot reuse and all-job shutdown
confirm every captured handle exited before publication. The marker reader retries
transient Windows sharing refusal within its existing bounded polling lifetime.

Current backend and browser Studio/worker/preview compile; the actual shared
browser consumer executes **56** checks (`browser-capture/capture.dom.html`).
Current LCL Studio compiles with matched FPC 3.3.1. Owned warnings are zero;
the four browser compilations retain **28 upstream RTL warnings**, unsuppressed.
The current host's real pas2js/FPC compatibility consumer passes **39** checks
with zero leaks, including exact semantic companions/manifests and actual native
application mount/normal close. Its browser application also mounts from the
admitted succeeded/current manifest (`application-capture/capture.dom.html`).
Both browser captures use new three-file children of the unchanged isolated
static host. Compiler input remains the earlier pristine frozen recovery payload,
not a newly sealed current-source candidate.
Final preservation receipt (`preservation.json`) confirms all fifteen exact
protected process identities, eight exact accepted pairs/navigation/draft/history
availability states and LAN HTTP 200. This is not a full-history migration receipt.
The owned qualification processes are terminal; the semantic companion retains
its previous exact SHA-256.

The existing Pascal semantic client authenticates and inspects the protected
primary project at revision 2; this chat's cached native MCP handles still refuse
initialization with HTTP 404. No listener/restart/enrollment/config refresh occurs.
This does not qualify the newly implemented cancellation via observing HTTP or UI.

Stop family/queue/authority fixture expansion at this boundary. Other native hosts
retain only direct-process retirement. Windows x64, incompatible outer-job refusal,
OS query/termination refusal and host death during creation are not exercised.
The two-call suspended-create/assign window is explicitly not crash-atomic;
kill-on-close protects assigned members. Atomic job-list creation remains NS-5
hardening. Bounded job discovery, ordinary browser job integration and visible
both-adapter cancellation are the next concrete editor deliverable. No original
criterion or task closes, no historical count resets, and the phone stays on its
earlier release. Protected full-history migration and observing rollout remain open.

## Compiler queue and cancellation — 2026-10-06

Implementation and qualification evidence are committed as
`28a5afff1f7cb068613140e47fffd983778dad9f`; `origin/hello-nyx` was verified at that
exact revision with a clean worktree. This handoff update follows separately.
Next complete owned compiler-family retirement, then bounded job discovery and
visible cancellation in both Studio controllers. The protected history migration
and observing release gates above remain open; do not expand queue/owner fixtures
or treat the phone's earlier release as this implementation.

NS-5 criterion 1's current-source prerequisite now owns two running slots and
eight FIFO pending jobs, with sixteen retained handles and sixty-four immutable
retry receipts. Admission captures the exact source/design/profile/context;
host request/status/completion polling advances the queue. Queued cancellation
never constructs a worker; running cancellation stays active until the executor
joins its exact process and the owner joins its worker. Shutdown signals every
owned job before joining any one. Only joined success advertises artifacts.
Deadlines/log caps are typed failures; draining a continuously writing compiler
cannot starve the time/cancellation checks. Log caps retain complete UTF-8 scalars.

`nyx_build` now admits revision-aware cancel through the same native semantic
seam used after HTTP authentication. Agents require Allow edits and the admitting
connection; operators can cancel jobs in their resolved project. Requests/retries
use private connection, authority and exact project/review, including the primary
project. Display renaming cannot change authority. Cancellation changes neither
document history nor accepted source/report/preview ownership. Typed portable
states and cancellation references reach both adapter compilations; native
polling recognizes cancelled terminal state without clearing its prior artifact.

Evidence: `build/compiler-lifecycle/` (ignored, private qualification artifacts).
The final checked FPC 3.2.0 lifecycle run passes **70** actual-child/native-host
checks with **zero leaks**. It covers the ten-job admission boundary, queued and
running cancellation, FIFO slot reuse, actual child exit, all-job shutdown,
same-display primary connections, renamed retries, foreign/stale/read-only
refusal, operator cancellation and cancellation of earlier-source jobs after a
semantic edit. Exact revised pair, navigation/draft/history availability and
previous report/sequence remain retained. Actual deadline and flooding processes
are reaped. A cap inside a four-byte supplementary scalar preserves earlier exact
Unicode and a valid prefix. The fixture uses stdout byte streams: its previous
Text-file path demonstrably converted the scalar to question marks.

The maintained `compiler-lifecycle` target adds orchestration only. Its retained
profile/receipt/handle regression passes **47** checks, and the portable admission
consumer passes **56** checks natively and in the actual browser
(`browser-capture/capture.dom.html`). Backend, browser Studio/independent worker
and preview compile; seven changed HTTP semantic consumers and current LCL
Studio compile with matched FPC 3.3.1. Owned warnings are zero; four browser
compilations retain **28 upstream RTL warnings**, unsuppressed.

The real-compiler compatibility consumer passes **39** checks: actual pas2js/FPC
application jobs retain exact admitted companions and manifests, and the native
application mounts/closes normally. The actual browser application also mounts
from its admitted artifact (`application-capture/capture.dom.html`,
`data-nyx-ready="true"`) through a new child of the unchanged isolated static host.
It consumes the previous pristine frozen
recovery payload; that payload predates the authority/lifecycle corrections and
is not a freshly sealed candidate. This does not qualify observing HTTP,
UI cancellation, process descendants, other widgetsets/operating systems or an
OS that refuses to reap its process. OS retirement retains ownership instead of
claiming terminal status. All fifteen protected process identities and eight
exact project/navigation/draft/history-availability pairs remain unchanged;
LAN remains HTTP 200 on its earlier release. No listener/restart/re-enrollment.

## Earlier return context

Previous goal turn: progress. Actual session/history storage, fresh-process recovery,
both-target ownership evidence and an exact remote checkpoint changed authoritative
state. This batch follows the independently actionable request-owner propagation
gap in NS-4 workflow criterion 5. Qualify same-display connections, renamed actors,
callback/root tickets, independent retries and paired history through the portable
routers and the actual native ordinary-tool seam. Stop at that integrated boundary;
protected older-service migration and authenticated observing rollout stay open.

Current return: ordinary dispatch authority is now qualified below. Stop router/
ticket fixture expansion. The previous sealed recovery bundle stays byte-exact
and predates this one-unit correction; the maintained backend/browser consumers
are rebuilt, without an authenticated rollout. Workflow no-closure advances 9→10
once, without resetting earlier counts. The protected older backend still cannot
export complete history. Follow independently actionable NS-5 criterion 1 next:
running/queued compiler cancellation and explicit termination/join. Its separate
primary agent build retry key still starts from the visible actor; carry the same
trusted connection identity into that compiler path rather than claiming all-tool
authority from ordinary dispatch. Original observing recipe rollout remains the
return path after these concrete prerequisites.

Previous goal turn: progress. Typed runtime separation, real compiler/artifact
execution and an exact remote checkpoint changed authoritative state and removed
the directory prerequisite. Current batch belongs to NS-5 criterion 2's accepted-
work preservation: versioned native runtime storage now retains actual protocol
projects, drafts, navigation and paired Undo/Redo, with durable admission before
acknowledgment and prepared in-memory rollback on write failure. Private engine
recreation/fresh processes, both portable consumers, Unicode and exact refusal
are qualified. Stop at that integrated boundary;
protected old-service migration and observing HTTP deployment remain separate gates.

Preceding recovery return path: the native session/history boundary below retains actual
protocol work through a fresh process. Stop recovery fixture expansion after the
maintained frozen consumer. The remaining preservation gap is specific to the
protected running older backend: it cannot export complete Undo/Redo stacks.
Do not mistake its availability flags or paired exports for a migration receipt.
Next assess that bridge within the existing workflow owner, then return to
authenticated content editing and the observing ordinary browser Studio. No
protected process restart, new listener or enrollment is authorized by staging.
The newly discovered distinct-request-owner propagation gap is independently
actionable first within that workflow owner; qualify it through the shared
semantic boundary before any new authenticated deployment claim.

NS-5 criterion 1's typed source/runtime/enrollment separation is integrated through
the actual server, MCP protocol and immutable compiler workers. Existing repository
launches remain compatible; release writes stay outside the frozen payload.
Real delegated browser/LCL application jobs and private profile/enrollment behavior
pass, and both produced applications execute. Stop directory/package fixture
expansion here. The current session/history packet below addresses that next
prerequisite, returning to the original observing recipe/MCP rollout. No protected
service is restarted or new listener launched; all fifteen identities/eight pairs
remain protected. The running phone editor still has the previous release.

The NS-6 criterion-3 prerequisite now prepares a frozen compiler-source and
backend/browser bundle with a strict byte manifest. It refuses existing outputs,
excludes private state and exercises the frozen sources. Stop package/integrity
fixtures here. The existing service/reload owner now separates release sources
from writable jobs/profiles/enrollment. Ordinary workspaces/history were memory-only
in that preceding packet; the new recovery boundary below addresses current-source
preservation. Older running-service migration remains required before this pristine
candidate supports an observing rollout.
All fifteen service identities/eight pairs remain protected. Staging is not an
authenticated HTTP deployment or a completed delivery criterion.

The original NS-4 criterion 1 now has ordinary Nyx-built recipe authoring. Its
public reusable Properties compound captures copied typed conditions/references
into the existing independent source queue. The accepted candidate generates
adjacent Pascal and owns one paired Undo/Redo step. Exact registered removal,
invalid intervals and stale queued registry refusal are exercised on both targets.

Stop local editor/queue fixtures here. Authenticated discovery still confirms
twenty running tools without content query or content-set. That concrete deployed
schema gap is recorded with the existing workflow owner (no-closure 9), while
current source supports the semantic command. Next deliverable is the observing
browser Studio/HTTP semantic journey using the same authoring boundary, through
the maintained protected release path. Prepare/check the release independently;
retain all fifteen services/eight pairs and their identities. This batch starts,
stops or restarts no service and claims no LAN rollout.

Return path: original structural presentations (NS-4 criterion 1), through the
open native/browser ownership and interaction prerequisites. Independent mounted
blueprints now publish genuinely different control sets through coalesced UI jobs.
Host/manual and allocated-container transitions retain stable capabilities,
explicit logical part focus/drafts, independent stores and admitted source meaning.
Introduced/nested publishers now settle as hidden candidates before acceptance.
Reversible physical preview now includes actual attachment/showing, synchronization,
observation, focus/ranges, containing scroll and selection paint before retirement.

Reassessment: return from ordinary local integration to the concrete release and
observing-transport gap, rather than another parser/renderer fixture. The native
full controller and both Properties/queue consumers are qualified; the latter
is an independently hosted subtree, not the full observing browser application.
Original full parity/accessibility,
performance and delivery criteria remain unchanged; the current proofs do not
accept hardware IME/assistive input or other widgetsets.

The ordinary editor packet closes no full original criterion and advances
authoring no-closure 25→26 once. Current renderer 7, workflow 9 and codegen 28
remain; the release prerequisite below advances delivery 1→2. The editor
does not restrict alternate structures to fixed control sets or credit an
explicit remount as continuity. Nothing moves to DONE. Main services
and all eight accepted pairs remain protected; no LAN rollout is claimed.

## Connection authority — 2026-10-06

Owner: NS-4 workflow criterion 5, returning to the observing recipe/HTTP release.
`TNyxReviewWorkspaces.Call` now validates a nonempty bounded Unicode connection
owner and forwards it to the actual session command. Activity retains the visible
actor; receipts and callback/root removal tickets use the trusted connection
identity on primary and private review routes. Existing explicitly routed ordinary
projects already supplied that owner and retain their behavior. Direct local
session calls retain their documented display-actor fallback; authenticated native
ordinary dispatch always supplies a distinct owner.

The new shared semantic journey creates independent controls and ordered callbacks,
reviews/removes exact registrations and a page root, and executes paired Undo/Redo.
Two connections intentionally use the same visible supplementary-Unicode label
and operation IDs. Foreign/current removal tickets refuse without consuming the
original ticket; original retries stay byte-exact after foreign edits. Every refusal
preserves exact accepted/draft files, revision, selection/view and history flags.
A private review retry survives an actor rename and still rejects another transport.
Empty/overlong trusted owner input refuses instead of silently falling back.

Private evidence under `build/agent-authority/`:

- `before/compile.log`, `run.log`: checked stable FPC reproduces the actual old
  primary behavior. The second same-name connection incorrectly receives the
  first connection's successful receipt despite its stale revision. The fixture
  fails at that refusal assertion, with zero leaks. This is a reproduced contract
  defect, not inferred only from code inspection.
- `fixed/compile.log`, `run.log`: the first corrected portable journey passes
  118 stable-FPC checks, zero leaks/owned warnings. Later empty/overlong-owner cases
  add 32 assertions; the maintained current fixture below owns the 150 count.
- `qualified/compile.log`, `run.log`: the actual suspended protocol engine passes
  147 stable-FPC checks across primary and an independent ordinary project,
  including prepared rollback/refusal and retained original removal authority.
  The primary pair remains exact after the project journey; zero native leaks/
  owned warnings. Calls use its actual locked ordinary-tool seam and trusted
  operator observations; no HTTP listener/authentication is inferred.
- `browser/compile.log`, `browser-review.log` and
  `browser-artifact/capture.dom.html`: actual browser execution publishes 150
  portable assertions. Only compiled fixture/matched RTL/HTML enter a new child
  of the existing protected isolated static host. No screenshot-driven editor
  automation or new visual/input qualification occurs.
- `maintained-build.log`: the maintained `review-workspaces` target passes existing
  200 review checks, current 150 portable authority and 147 actual native protocol
  checks with matched FPC 3.3.1, zero native leaks/owned warnings. It compiles the
  existing HTTP review consumer, actual backend, ordinary browser Studio, independent
source worker, private review and preview consumers using pas2js 3.3.1. Six browser
  compilations retain 42 visible upstream RTL warnings in total. The new tests and
  English fixture host are part of that maintained target; runtimes get independent
  identifiers and no previous runtime is deleted or reused.

PowerShell AST parsing and `git diff --check` pass. No listener is started,
protected process stopped/restarted or protected enrollment refreshed. The actual
native engine receives only a fresh owned runtime and an empty output profile;
application compilers are not an authoring prerequisite. No dependency is edited.
The previous sealed 200-member recovery artifact is retained unchanged; it does
not contain the new dispatch correction. Current maintained consumer compilation
is not a new sealed artifact or authenticated HTTP deployment.

`preservation.json` confirms fifteen exact PID/creation/executable/command identities
and eight exact accepted/draft/navigation/history-availability frames against the
preceding baseline. LAN HTTP returns 200. Final native verification retains all 200
members of the previous sealed recovery bundle. All owned qualification processes
are terminal; no protected process operation or automatic-review retry occurred.

This accepts the bounded ordinary-tool authority fix; no full original criterion
or task closes and nothing moves to DONE. Workflow no-closure advances 9→10 once;
authoring 26, renderer 7, codegen 28 and delivery 2 remain. Reassessment ends further
router/receipt fixtures here. Same-display compiler build retry ownership is a
separate existing path and remains an explicit integration gap. Next follow actual
running/queued worker cancellation, termination/join and that compiler ownership
under the original NS-5 criterion 1. Retain protected old-service history migration
and observing recipe/HTTP qualification as separate gates; do not substitute paired
files/availability flags for complete older live histories.

Implementation checkpoint: `d1b7af306152e29e5b04eda76dba4edf047c32e2` was committed
and pushed to `origin/hello-nyx`; exact remote comparison succeeds and the working
tree was clean. This packet's current maintained backend/browser consumers include
the correction; the preceding sealed recovery payload remains unchanged and does
not. Next criterion is original NS-5 worker cancellation/termination-join, with
the separate primary compiler owner propagation carried into that same integration.
No observing release, protected service change or full workflow acceptance is
claimed. Workflow's consecutive no-closure count is 10; no owner count is reset.

## Session recovery — 2026-10-06

Owner: NS-5 criterion 2 accepted-work preservation, returning to NS-4 observing
recipe/MCP integration. A native host-only version-1 stream now checkpoints the
actual protocol primary and eight ordinary projects, exact accepted files and
pending draft/base, selection/view/naming counters, revisions, enablement and
paired Undo/Redo. The portable model exposes copied recovery values; disk paths,
flush/replace and process-lifetime ownership remain in the native adapter.

The actual authenticated ordinary-tool handler now delegates through the same
locked semantic dispatch seam exercised by the private consumer. Editor claim,
commit/history/configuration and ordinary closure share durable admission. Each
durable operation prepares independent rollback owners, publishes its checkpoint
before returning success and restores exact in-memory state on failure. Read-only
queries do not rewrite storage. Rollback retains current activity/retry/review
values; disk recovery deliberately excludes transport credentials, authority,
presence, review tickets and compiler jobs. Restart rotates connection credentials.

The bounded codec verifies complete framing/digest and admits every session/history
pair before publication. It caps total bytes, UTF-8 fields, sessions and history.
Unsupported/corrupt/appended or semantically invalid files refuse before enrollment
without overwriting unknown work. An exclusive Windows handle prevents a second
new host sharing the runtime and is released even on process termination. A
unique sibling is flushed/closed before atomic OS replacement; a denied replacement
leaves the committed checkpoint and prepared memory baseline intact.

Private evidence under `build/session-recovery/`:

- `crash-qualification/compile.log` and `locked-run.log`: checked stable FPC 3.2.0
  passes 64 actual protocol checks with zero unfreed blocks/owned warnings. This
  includes exact supplementary Unicode/comments, empty/stale pending drafts,
  executed recovered history, nine sessions, closed-handle non-reuse, persistent
  enablement, expired authority, concurrent-host refusal, denied writes, retained
  retry receipts and malformed semantic/integrity frames.
- `crash-qualification/process-run.log` plus `crash-runtime/process.log`: five
  owned parent checks and sixteen fresh-process checks pass. The producer emits a
  flushed complete readiness frame after actual acknowledgments and stays alive.
  Its parent terminates only that owned process handle; the fresh process admits
  exact primary/child frames and executes recovered paired Redo/Undo. Parent and
  normal resumer have zero leaks; no graceful save or producer heap result is
  inferred from deliberate termination.
- `shared-clean/compile.log` and `run.log`: fourteen portable clone/recovery checks
  pass stable FPC with zero leaks/owned warnings. Independent owners/vectors,
  draft/base, navigation, exact history, naming sequence and expired retry authority
  are exercised. `browser-clean/compile.log`, `browser-current-review.log` and
  `browser-current-artifact/capture.dom.html` establish the same fourteen checks in
  the actual browser. Only the compiled fixture/matched RTL/host enter a new child
  of the existing isolated static host. This is algorithm ownership evidence,
  not a new visual/input or authenticated editor route.
- `regression/`: existing workspace ownership passes 237 and agent checks pass
  39, zero leaks and owned warnings. `native-studio/compile.log` rebuilds the
  affected ordinary Win32 LCL Studio consumer with matched FPC 3.3.1, zero warnings;
  no changed native input behavior is inferred from compilation.
- `maintained-build.log`: the maintained target freezes 200 members, compiles the
  actual backend/browser Studio/independent worker/preview and passes 29 integrity,
  39 real runtime/compiler/artifact, 66 recovery (including unsupported-version
  refusal), five parent/fresh sixteen, and fourteen shared ownership checks.
  Native consumers use matched FPC 3.3.1, zero leaks/owned warnings. Each of the
  three pas2js 3.3.1 builds retains seven visible upstream RTL warnings.
- `maintained-release.qualification/recovery-process/process.log` records the
  fresh process separately. Final native verification reports all 200 bundle
  members unchanged. The accepted semantic English companion remains SHA-256
  `30aa676532ae6dd381ce39a3a198197e3ef243b5c03936304e2a91379ffc52a1`.
- `preservation.json`: fifteen exact protected PID/creation/executable/command
  identities and eight exact paired/navigation/draft/history-availability frames
  match the preceding accepted baseline; LAN HTTP returns 200. Availability is
  not an export of those older live Undo/Redo stacks.

Initial fixture errors used the wrong transaction envelope, inferred truncated
short strings while enumerating field names, and omitted explicit native UTF-8
casts. They were corrected; protocol/source admission was not weakened. Older
native MD5/Windows unit declarations required a read-only byte view and the closed
write-through flag. No dependency was edited. Both maintained scripts parse and
`git diff --check` passes. No listener is started, protected service stopped or
restarted, or protected enrollment refreshed. Suspended engines are constructed
and retired only; their listener entry points never run.

Final fixture-only formatting expands exception bodies to the author's style;
`style-compile/` rebuilds both fixtures successfully with stable FPC and zero
warnings. It changes no checked behavior or frozen product member; completed
behavioral checks are not repeated for formatting.

This accepts the bounded session-preservation boundary, not full service reload,
OS/hardware power-loss durability, network filesystems, other widgetsets/platforms,
performance, HTTP deployment or product parity. No full original criterion/task
closes and nothing moves to DONE. Existing authoring 26, renderer 7, workflow 9,
codegen 28 and delivery 2 counts remain; no historical NS-5 numeric no-closure
count is invented. Stop recovery/codec fixture expansion here.

Next: the protected older backend cannot export complete history, so its paired
files/availability flags cannot migrate those stacks. Follow this concrete bridge
with the existing workflow owner while retaining all exact protected identities.
The earlier automatic new-listener rejection remains unretried; source dispatch
is not authenticated HTTP qualification. Also record the existing primary review
dispatch seam: `TNyxReviewWorkspaces.Call` currently omits the distinct request
owner when forwarding ordinary session calls. That authority propagation needs
explicit same-actor/different-transport qualification before new HTTP workflow
acceptance; this recovery packet does not claim to resolve it.

Implementation checkpoint: `dc0e1bd5843ecb57005917506256b9b3a10de14c` was committed
and pushed to `origin/hello-nyx`; exact remote comparison succeeds and the working
tree was clean. The sealed candidate retains its pre-commit base label
`2d22ef99a1abe7f1146a19b0de1ebc87c576d29f`; a private SHA-256 receipt maps all 192
owned compiler-source members to the clean implementation bytes. Its 200-member
manifest is unchanged. Do not rebuild merely to rewrite that label. Current
handoff documentation is separate from product compilation. Next work is the
existing workflow owner's request-owner propagation, retaining the protected
older-service migration and observing rollout as explicit gates.

## Separate release runtime — 2026-10-06

Owner: NS-5 criterion 1 isolation, returning to NS-4 observing recipe/MCP integration.
`TNyxStudioDirectories` is an immutable native host value with repository/release
modes, explicit source/runtime/enrollment roles and copied worker configuration.
Release admission verifies the byte manifest once and checks ordinary ancestors
and disjoint writable locations before host creation. No portable document or
public component depends on these host filesystem choices. Repository overloads
retain their prior layout and the existing compiler-job resource regression.

The actual server uses runtime profiles, saved project directories and jobs; MCP
uses the independent enrollment root and runtime preview directory. Compiler units
come from the frozen payload, while each job owns copied directory/profile/source
values. The launcher can select a prepared release without rebuilding Studio or
requiring application compilers; it refuses earlier bundles without this entry
point. The maintained release target optionally exercises the real protocol/jobs
using an explicit private profile and the unchanged semantic English companion.

Private evidence under `build/runtime-directories/`:

- `checks/run-executed.log`: 40 checked stable-FPC runtime checks, zero unfreed
  blocks, including an actual runtime junction refusal and both real compiler
  workers. The exact accepted companion is SHA-256
  `30aa676532ae6dd381ce39a3a198197e3ef243b5c03936304e2a91379ffc52a1`.
  The resulting full native application mounts its authored edit control and
  closes normally. This is startup/shutdown evidence, not new input qualification.
- `legacy/run.log`: the unchanged repository-mode compiler-job resource/lifetime
  regression passes 45 checks with zero leaks. Both checked host compilations have
  zero owned warnings; ten existing lifetime-retention notes remain.
- `browser-artifact-review.log` and `browser-artifact/capture.png`: the exact
  succeeded browser job publishes its application-ready marker and renders the
  English recipe workshop. Only three verified artifact members enter a new
  child of the existing protected isolated static host. This is application
  startup/visual evidence, not a new editor-route deployment or browser input test.
- `maintained-build.log`: the earlier integrated maintained build freezes 199
  members and passes 29 integrity plus 37 runtime checks, zero leaks, before
  native artifact execution was added to the runtime consumer. A final packet
  below records the current maintained result.
- `final-build.log`: the complete maintained target compiles the 199-member
  source snapshot, backend, ordinary browser Studio, independent module worker
  and preview. The native host/integrity/runtime consumers use matched FPC 3.3.1;
  browser builds use pas2js 3.3.1 and its matched runtime. It passes 29 integrity
  and 39 integrated runtime checks with zero native leaks and owned warnings.
  Each of three browser builds retains seven visible upstream RTL warnings.
  The earlier checked 40-case consumer independently uses stable FPC 3.2.0.
- `preservation.json`, `protected-services.json`, `protected-main.json`: all
  fifteen exact process identities and eight exact paired projects/navigation/
  draft/history-availability frames match the prior accepted baseline. LAN HTTP
  returns 200. This verifies history availability, not serialized Undo stacks.

PowerShell AST parsing passes for both maintained scripts. The launcher refuses
the preceding frozen candidate before executing a server or creating runtime
storage. That refusal command resolves the matched native compiler hint, so the
following maintained build correctly records FPC 3.3.1 rather than stable 3.2.0.
No listener, stop, restart or protected enrollment operation occurs.

Initial test-client failures omitted the required observation cursor and then
read the editor build-reply wrapper incorrectly. Those fixtures were corrected;
protocol admission was not weakened. The suspended engine and actual host are
constructed/destructed only; their listener entry points are never called. Runtime
test enrollment touches only a new owned directory, with no enrolled-user entry.
The original pristine payload remains byte-exact after all worker/profile/host work.

This bounded directory boundary is accepted within the original isolation owner;
no full original criterion, task or product percentage closes. Existing authoring
26, renderer 7, workflow 9, codegen 28 and delivery 2 counts remain. Sessions,
navigation, drafts and Undo/Redo stacks remain memory-only; saved project pairs
do not establish restart preservation. Next implement that explicit preservation
boundary, then check authenticated content editing and the full observing browser
Studio. Cache/cancellation/retention, native packaging, full parity/accessibility
and performance retain their original owners. No service or LAN rollout is claimed.

Checkpoint: implementation `1fe6c0a9b02f4a7e0b7a16e1c0ffd2b26e5fb9df` is pushed to
`origin/hello-nyx` and the exact remote SHA is verified in
`build/runtime-directories/remote-checkpoint.json`. All 191 frozen source members
match the clean working tree at that checkpoint. The sealed 199-member manifest
still records its preceding base revision; its inventory owns the compiled bytes,
and no metadata was rewritten or passing builds repeated merely to relabel it.
All compiler/harness/application processes completed. The only new hosted child
contains the executed browser application; no Studio frontend/backend was installed.

Next bounded deliverable: persist and restore the actual protocol workspace/session
state through typed host runtime storage, including accepted paired projects,
navigation, drafts and exact paired Undo/Redo ownership. Define version/admission
and atomic failure behavior, exclude transport credentials from portable designs,
and qualify actual private-engine save/recreation, Unicode and refusal using the
existing runtime source/profile boundary. Keep every protected service and user
pair exact. Stop at integrated restart-preservation evidence before attempting the
separate authenticated HTTP/observing recipe rollout; no new listener is authorized
by this handoff and the earlier automatic listener refusal must not be retried.

## Frozen release prerequisite — 2026-10-06

Owner: NS-6 delivery criterion 3, returning to NS-5 runtime/reload preservation and
the original NS-4 observing recipe/MCP journey. `nyx.studio.release` and the Pascal
CLI prepare a new source snapshot before backend/editor/worker/preview compilation.
Only owned Pascal sources, production hosts, matched runtime and MIT license enter
the bundle. A strict complete inventory checks exact paths/lengths/MD5 fingerprints;
MD5 is accidental byte-integrity evidence, not authentication. There is no overwrite,
recursive deletion, new listener, service replacement or enrollment change.

The first manifest gate exposed Windows RTL collation disagreement for dotted
units and underscore programs. An explicit ordinal comparator now owns the wire
order. The Windows unit also qualifies `SysUtils.FindClose` against the same-named
native API. The initial matched compiler flagged the deliberately separate
array/object fault cases; the fixture now covers every enum case explicitly and
consumes the refused function result without an unused local. Failed/unsealed
output remains separate for inspection.

Private evidence under `build/studio-release/`:

- `qualified-build.log`, `maintained-build.log` and `checkpoint-build.log`: the frozen
  198-file / 11,173,649
  byte candidate compiles backend, ordinary browser Studio, independent module
  worker and preview. All owned builds have zero warnings; each of the three
  pas2js builds retains seven upstream RTL warnings. The maintained target also
  verifies the bundle and runs 29 integrity/privacy/refusal checks with zero leaks.
- `checks/run-final.log`, `checks-matched/run-final.log`: 29 checked manifest/refusal checks
  under stable FPC 3.2.0 and matched 3.3.1, both zero leaks. Cases include same-size
  corruption, changed length, missing worker, private/extra/nested members,
  traversal/absolute/duplicate paths, malformed metadata, exact UTF-8 bytes,
  immutable existing outputs and preservation after refusal.
- `junction-refusal.log`: an actual owned Windows directory junction refuses
  before sealing; its borrowed target remains intact.
- `generated-native/run.log`, `generated-browser-review.log`: five checks each
  execute the unchanged recipe-editor companion against the frozen sources.
  Native is leak-free; the browser used only new child files beneath the existing
  protected isolated static host. No compiled frontend/backend was deployed.

Packaging preparation qualified a pristine candidate, not a live writable root.
At that checkpoint `TNyxBuildExecutor`, MCP preview/enrollment and server profiles/jobs used
the repository root, while ordinary project sessions/history end on shutdown.
These concrete prerequisites are recorded with their existing owners. Current
HTTP content query/mutation remains absent and its journey remains unqualified.
The release can be reproduced with `tools/build.ps1 -Target studio-release` and
a new `ReleaseOutput`; [the guide](docs/studio-releases.md) explains the boundary.

No original full criterion closes; delivery no-closure advances 1→2 once.
Authoring 26, renderer 7, workflow 9 and codegen 28 remain. Nothing moves to DONE.
No full supported-platform distribution, native Studio packaging, performance,
accessibility or LAN update is claimed. Protected service/project comparisons and
remote checkpoint receipts belong to this packet's final preservation record.

Checkpoint: implementation `c5a8175a71f7fdb2e913b3b9366c4a3889ddc33b` is pushed to
`hello-nyx`, with exact remote SHA verified. `checkpoint-package/release.nyx`
records that committed source revision; the final maintained target passes 29
checks with zero leaks and zero owned warnings. `live-tools.jsonl` independently
confirms twenty authenticated deployed tools and no advertised `content-set`.
`final-preservation.json` retains all fifteen process identities/eight exact
pairs/navigation/draft/history-availability frames and LAN HTTP 200. All compiler
and harness processes completed. The only new static-host child contains the
executed generated-companion check; no frontend/backend release was installed.
Return to source/runtime-root separation and explicit session preservation within
the existing service/reload owner, then the observing HTTP recipe journey. Do not
restart/re-enroll protected services or extend package fixtures from this handoff.

## Ordinary recipe authoring — 2026-10-06

`nyx.content.editor` is a public Nyx compound composed from managed specialized
cards, labels, selects, spins and buttons. Default, available width/height/
orientation, named presentation and closed platform choices produce an independent
registry. Exact rule removal retains other scopes. Studio's Properties view
consumes that public composition and captures immutable `NyxSetContent` values,
with an exact mounted registry baseline. Worker admission validates all inactive
dependencies and preserves source/history on refusal. Stale queued capture is a
normal rejected diagnostic, not an unavailable processor. Existing requests keep
v9; only the new content action uses strict v10. A cached count avoids the installed
pas2js implicit record-method array-index emission bug found by actual execution.

The maintained Pascal MCP author composed the unchanged English seed with one
semantic transaction in an owned review, exported bounded source windows, requested
both application builds, checked exact source/revision/output and explicitly
discarded its review within one owning transport. Both jobs succeeded; primary
paired frame/navigation/history availability remained exact. A preliminary
one-shot review could not be continued from a new transport and correctly refused;
the maintained session resolved the workflow without retargeting a user project.

Evidence under ignored `build/content-editor/`:

- `mcp/run.log` and `source/`: semantic seed/export/both compiler job receipts.
- `final-qualified.log`: maintained build orchestration passes, including 55
  actual Properties/queue checks, six additional
  ordinary native Studio checks (61 total), zero unfreed blocks. Actual native
  Undo/Redo and retained Pascal control are included. `maintained/result/` owns
  desktop/390 captures and the exact newly generated companion.
- `browser-final.log`, `browser-compact.log`: 55 per desktop/exact-390 Properties
  consumer with the real compiled independent worker and target previews.
  Captures under their corresponding browser-review directories were inspected.
- `generated-stable/run.log` and `generated-browser.log`: five checks each execute
  the unchanged newly generated builder and qualify its accepted registry and
  realized size/manual recipe meaning; native reports zero leaks.
- `stable/queue-run.log`: existing real native queue/presentation checks pass 10
  with zero leaks. Server and browser Studio compile independently; native Studio
  is compiled/executed by the maintained consumer. Zero owned warnings; each
  browser compile retains seven unchanged upstream RTL warnings.
- `final-preservation.json`, `protected-main-final.json`,
  `protected-services-final.json`: all fifteen process identities and eight
  exact accepted pairs/navigation/draft/history-availability flags retained;
  LAN HTTP 200. History stack serialization is not observed by that endpoint.
  `reviews-final.jsonl` reports zero remaining reviews after transport retirement.

`tools/build.ps1 -Target content-editor` maintains compilation/native execution,
unchanged generated-companion execution and browser/worker staging. Optional
`DesignerMCPConfig` composes/builds/retires a review; ordinary Studio startup does
not require it. Actual browser execution used only newly task-owned child files
under the existing isolated host. No new listener or production replacement.

Full responsive WYSIWYG/parity/accessibility/performance and original prerequisites
remain open. Rule loading/draft ergonomics, broader source-change continuity and
the full observing browser semantic release are not accepted from this packet.

Checkpoint: implementation `92c577e045e287d6a138eb9353cb1a2cb5097c6d` is pushed to
`hello-nyx`; exact remote SHA was verified. The final maintained build passes 61
actual native checks and five executed companion checks, with zero leaks. Browser
Properties passes 55 per desktop/390 and its companion passes five. No compiler
or harness remains live. Private remote/handoff receipts are under
`build/content-editor/`; the full user goal remains active. Next use the workflow
owner's protected release/observing path, retaining the eight current pairs and
fifteen service identities, instead of adding another local editor fixture.

## Reversible physical publication — 2026-10-06

Owner: original NS-2 native interaction/ownership and browser recovery criteria,
returning to NS-4 criterion 1. The bounded physical preview gate is qualified;
the original breadth/parity criteria remain open. Stop publication fixtures here.

The accepted model, controls, router epoch and capability stay owned until the
replacement succeeds in the real target. Browser preview retains exact old DOM
objects in an owned holder, mutes Nyx notifications and restores host classes/theme,
focus/ranges and scroll on refusal. Native preview shows/aligns the new panel while
the old panel remains alive, synchronizes actual controls and installs observers
before focus. Its optional managed observation port checks readiness before commit,
then copies the actual admitted router epoch into viewport/editing/capture hooks
without target calls, allocation or callbacks. Existing public observer interfaces
keep their ABI. Containing scroll and selection paint also prepare before retirement;
native owned selection strips transfer with the new panel.

Custom factories/updaters must leave borrowed source/store unchanged during
preparation; extension destructors must release without raising. This is recovery
for fallible physical preview, not reconstruction after a violating destructor has
destroyed the old view. General external stylesheet equivalence, arbitrary logical
identity migration, complete source-change publication, other widgetsets/DPI and
hardware/IME/assistive input remain separate qualifications.

Private evidence under `build/content-publication/`:

- `final-qualified.log`: maintained target passes 55 shared checks per stable
  3.2.0/matched 3.3.1 compiler, 55 unchanged companion export, 42 Win32 remount,
  63 live, 27 nested and 50 reversible-publication checks. Every native trace
  reports zero leaks. The new actual-control fixture refuses visible synchronization
  and focus after preparation, checks exact old input/draft/scalar range/epoch/
  capability, then old and committed editing/scroll/capture callbacks. A separate
  designer mount checks visible selection through refusal, retry and teardown.
- `browser-*-run.log`: the maintained Pascal driver executes 50 publication,
  63 live, 29 nested, 42 remount, 55 shared and 27 scalar-container checks.
  Browser capture is a host notification; it does not establish hardware capture.
  Only new task-owned files in a new child of the existing isolated web root are
  staged. No listeners or existing hosted files are changed.
- `container-native/run.log`: 27 ordinary scalar-container allocation/retained-
  input checks pass over the unchanged semantic companion, zero leaks.
- `server/compile.log`, `studio-web/compile.log`, `studio-worker/compile.log`
  and `studio-native/compile.log`: backend, browser Studio/module worker and
  native Studio compile with zero owned warnings. Each browser product retains
  seven unchanged upstream RTL warnings. These are compilations, not an ordinary
  Studio recipe-authoring journey or deployment.
- The generated companion stays SHA256
  `e5904d58dd7797d90da486e6d50596b71e761ef289d8c05e5f1708fbba1d2ed9`.
  New fixture attachments are explicitly typed consumer instrumentation, not
  MCP-authored replacement projects. Its initial callback check used design
  selectors for runtime identities; explicit runtime selectors correct the test.
  Its long scroll value correctly refused the reverse single-line recipe; restoring
  an admitted single-line value makes the intended reverse journey valid.
- `mcp-session.json`: authenticated bounded Pascal semantic read. Cached desktop
  handles and new-field HTTP integration remain with workflow owner 9.
- `final-preservation.json` / `protected-main-final.json`: fifteen exact
  process identities and eight exact paired project/navigation/draft/history-
  availability frames remain unchanged; LAN HTTP is 200. No service is restarted.

Full original criteria, ordinary Studio recipe editing/MCP observation,
accessibility/performance and delivery remain open. The full product goal stays
active. The next baseline is `build/content-publication/protected-main-final.json`.

Implementation checkpoint `6982d1ab6640aecde8b9c93e08d8238aed160a1e` is pushed to
`origin/hello-nyx`; exact remote equality is recorded in
`build/content-publication/remote-checkpoint.json`. No compiler or input harness
remains active. Continue with ordinary Studio integration, not another physical
publication fixture. Protected live services still serve their previous release.

## Hidden nested recipe admission — 2026-10-06

Owner: original NS-4 criterion 1, consuming the open NS-2 ownership prerequisite.
The batch deliverable was bounded real-target hidden settlement of introduced
publishers, with later-pass/cycle/limit failure retaining accepted controls. The
evidence boundary is now met; stop extending these fixtures and advance the
physical publication gate. No full original criterion closes and nothing moves
to DONE.

One owned authored blueprint, runtime store and collection context serve the
complete admission. Each candidate freezes structure and scalar projection to
the same input snapshot. Actual candidate allocation drives the next realization.
A portable guard compares exact authored identity/order, stored properties and
effective typed attributes; it excludes runtime drafts/values. Repeated meaning
or an unsettled eighth candidate refuses. Intermediate target controls, validators
and coordinators release independently. Browser probes are inert hidden siblings
with distinct theme scopes and explicit owned removal; missing/disconnected boxes
remain missing. Native probes use logical allocation. Ordinary scalar-only native
container views retain their original settling behavior.

Explicit native constructors can service the UI queue without queued structural
work retiring the accepted mount. A settled observation submits no redundant
courier. The initial guard change briefly left a cancelled courier at short-
harness shutdown; the final exact-observation gate corrects that regression.
The new limit fixture initially edited the retained conditional scope after
Clear; explicit Done correctly returns to ordinary recipe authoring. These
corrections keep the intended lifecycle/limit assertions.

Private evidence under `build/content-settling/`:

- `final-qualified.log`: maintained content target passes 55 shared checks per
  stable 3.2.0 and matched 3.3.1 compiler, 55 unchanged companion export checks,
  42 actual Win32 remount, 63 live and 27 nested checks, all zero native leaks.
  Source remains SHA256
  `e5904d58dd7797d90da486e6d50596b71e761ef289d8c05e5f1708fbba1d2ed9`.
- `browser/*-final-run.log`: maintained Pascal driver executes 55 contracts,
  42 remount, 63 live, 29 nested and 27 scalar-container checks. New nested
  consumers exercise cold admission before any queue turn, a three-level manual
  publication, later-pass factory failure with exact accepted input/draft/lease,
  independent stores, eight-candidate refusal, controlled extension allocation
  feedback, recovery and teardown. Browser additionally checks a hidden host's
  missing-box fallback and removal of connected probes. Native constructor
  callbacks pump CheckSynchronize; browser execution remains run-to-completion.
- `container-native/run.log`: 27 actual Win32 scalar-container allocation and
  retained-input checks over the unchanged semantic companion, zero leaks.
  This separately qualifies preservation of the ordinary nonstructural path.
- `server/compile.log`, `studio-web/compile.log`, `studio-worker/compile.log`
  and `studio-native/compile.log`: backend, browser Studio/module worker and
  native Studio compile with zero owned warnings. Each browser product retains
  seven warnings in unchanged upstream RTL source. These are compilations,
  not an observing Studio recipe-authoring acceptance or deployment.
- `mcp-session.json`: authenticated bounded read through the Pascal semantic
  client. The existing desktop reconnect/new-field HTTP integration gaps remain
  with the original workflow owner. Protected services are not replaced.
- `final-preservation.json` / `protected-main-final.json`: fifteen exact process
  identities and eight exact paired project/navigation/draft/history-availability
  frames remain unchanged; LAN HTTP is 200. Only a new task-owned child of the
  existing isolated web root hosts these compiled reviews. No listener changes.

Qualification includes controlled extension allocation feedback, not arbitrary
external stylesheet equivalence. Physical failure after old retirement, broader
identity migration, complete source-change publication, ordinary Studio recipe
editing/MCP observation, hardware/IME/assistive input, other widgetsets/DPI and
original accessibility/performance/delivery outcomes remain open. Next act on
reversible physical publication rather than repeating admission evidence.

Implementation checkpoint `6381268657ad2db966cd1c71f0b0ac783ee3eb63` is pushed to
`origin/hello-nyx`; the exact remote SHA is verified in
`build/content-settling/remote-checkpoint.json`. The next preservation baseline
is `build/content-settling/protected-main-final.json`, retaining all eight pairs.
No compiler or input harness remains active. The full product goal stays active.

## Live content publication — 2026-10-06

Owner: original NS-4 criterion 1, consuming the existing NS-2 runtime and NS-1
identity/source prerequisites. [The public contract](docs/content-recipes.md) now
describes queued publication alongside typed recipe authoring. Each mounted view
owns an independent authored blueprint, not a document backreference. A copied
realized recipe-owner reference separates instance identity from the selected
definition's source ID. Cloning preserves it; scalar and arrangement retention
refuse changed provenance before admission.

Actual host/container/manual observations coalesce through managed weak work
ports. Same-recipe geometry retains actual controls; changed sets stage controls,
bindings and copied values before replacing the accepted view. The original
presentation lease remains connected. The same scalar store and collection
context supply current values without importing defaults; local collection stores
use exact stable instance ownership. Compatible explicitly named list parts retain
selection. Contiguous named input paths carry accepted unbound values, compatible
drafts, focus and Unicode scalar ranges. Domains/descriptors/baselines must match;
anonymous names and changed contracts receive no guessed migration.

Current store/command guards and a separately managed callback guard prevent
structural retirement during synchronous callbacks, including native queue pumping.
Its idle receiver retires before renderer disposal and locally retained guards
finish without renderer access. Composition/capture defer replacement. Failed
factory/admission retains the exact accepted tree/choice/lease, records a copied
diagnostic and suppresses identical-observation retries. Explicit requests retry.
Retained source admission stages a new blueprint and replaces it only on success.

Private artifacts under `build/content-live/` retain qualification:

- `final-qualified.log`: maintained `-Target content-recipes` passes 55 shared
  ownership/source/semantic checks per stable 3.2.0 and matched 3.3.1 compiler,
  the preceding 42 Win32 initial/remount checks and 63 live checks, zero leaks.
  `native-final-compile.log` / `native-final-run.log` cover the final capture-aware
  fixture source separately. The unchanged generated Unicode companion retains
  SHA256 `e5904d58dd7797d90da486e6d50596b71e761ef289d8c05e5f1708fbba1d2ed9`.
- `browser-final-run.log`, `browser-contracts-final.log` and
  `browser-remount-final.log`: the maintained Pascal driver executes 63, 55 and
  42 respectively through task-owned files in a new child of the existing
  isolated host. The 63-check journey covers both resize directions, manual
  requests, coalescing, source refresh, named focus/drafts/scalar ranges, unbound
  values, local collection data/selection, failure recovery and teardown.
  Allocated 390→900→250 boxes change controls at an unchanged 900-pixel outer
  viewport. Browser run-to-completion and actual native nested queue service are
  distinguished. `browser-final-review/capture.png` shows fresh wide/390 proof
  views after teardown assertions, not retained drafts or observing Studio.
- `core-regression.log` preserves maintained portable/generated Unicode,
  event/state/managed-control/collection consumers and invalid-type compilation.
  `retained-final.log` passes 34 per native compiler, 92 original actual Win32
  arrangements and 61 bound arrangements, zero leaks. Browser counterparts compile;
  their earlier execution evidence remains, rather than a rerun in this packet.
- `manual-native/run.log` and `manual-browser-final.log` preserve 52 Win32 /
  53 browser checks over the unchanged semantic manual-presentation companion.
  Fresh source composition first reconciles the accepted choice against the
  new definition snapshot, preserving the existing manual-to-automatic admission
  behavior. Native tracing reports zero leaks.
- `server/final-compile.log`, `web-studio/*-final-compile.log` and
  `native-studio/final-compile.log` compile the current backend, Studio, module
  worker and native Studio with zero owned warnings. Each browser product retains
  seven warnings in unchanged upstream RTL source.
- `mcp-tools.jsonl` and `mcp-session.json`: current Pascal semantic discovery
  authenticates twenty tools and performs a bounded active-session read. The
  desktop's cached fifteen-tool handle still fails initialization with HTTP 404;
  reconnect remains owned by the existing workflow task. Protected services do
  not advertise the new content fields. In-process semantic tests qualify those
  fields; no new HTTP recipe-edit or observing release is claimed.
- `final-preservation.json` / `protected-main-final.json`: all fifteen service
  identities and eight exact paired project, navigation, draft and history-
  availability frames remain unchanged; existing LAN HTTP returns 200. No
  listener is started or service stopped/restarted. Existing hosting/source/profile
  files stay unchanged outside the new task-owned hosting child.

Qualification corrections preserve the intended assertions: native class field
placement and browser scroll integer bridging were corrected. Factory fixtures
declare the required bound updater before deliberate constructor failure. Browser
number inputs lack a scalar selection API; restoration checks capabilities.
Selection comparisons preserve each target's actual direction and exact initial
supplementary range, rather than inventing native endpoint direction. The capture
fixture uses an explicit HTMLElement bridge for this matched browser RTL.

Remaining gate: source refresh that changes structure still follows strict refusal
and explicit remount semantics. New nested publishers/allocation feedback need
bounded detached settling and full atomic changed-set publication. Physical failure
after retirement, arbitrary nested part migration, ordinary Studio recipe editing,
end-to-end hardware/IME/assistive behavior, other widgetsets/DPI, accessibility and
release performance/delivery remain open under original owners. No task moves to
DONE and the full product goal remains active.

Implementation checkpoint `9666b49c28d90c6a50b43134646577979b283995` is pushed to
`origin/hello-nyx`; its exact remote SHA is verified in
`build/content-live/remote-checkpoint.json`. The following documentation checkpoint
records that verification. This is a library milestone; the existing LAN Studio
release stays unchanged and ordinary recipe-editor integration remains open.

## Content recipes — 2026-10-06

Owner: original NS-4 criterion 1, consuming the open NS-2 ownership/interaction
and NS-1 source boundaries. [The public contract](docs/content-recipes.md) exposes
managed `INyxContent` on specialized controls. Independent scope facades choose
typed reusable references by copied viewport conditions, named presentations
and closed platform enums. Exact scopes replace in order; common/target defaults,
automatic and manual phases have explicit precedence. Scopes retain a value book,
never a document/node/UI cycle; clones and `SetContent` copy independent storage.
Custom `INyxControl` implementations now supply `GetContent`.

The portable composer validates every inactive/transitive dependency before
expanding only selected recipes. Immutable logical frames and optional allocated
container snapshots choose against qualified nearest ancestors. Instance publisher
overrides apply before nested expansion; appended/replaced slot payloads retain
their inherited ancestry. Version-five persistence preserves exact Unicode values
and refuses promotion collisions with older opaque extension fields. Generated
specialized Pascal and its managed reader share readable `.Content` blocks,
including authored `Clear`; paired reconciliation preserves handwritten comments.
Semantic root-removal review counts inactive recipe references without counting
multiple scopes on one retained instance more than once.

The semantic source candidate extends existing tools: separately paged
`nyx_node` content and strict `content-set` operations within one expected-revision
paired transaction. In-process checks exercise bounded inspection, grouped edits,
comment preservation, stale/invalid refusal and paired Undo/Redo. The backend
compiles, but protected live services do not advertise these new fields yet.
Current readonly CLI discovery still authenticates twenty existing tools; the
desktop chat's cached native handles require reconnection. No new transport tool,
HTTP recipe-edit journey or LAN deployment is claimed.

Private artifacts under `build/content-recipes/` retain these checks:

- `qualified-build.log`: `tools/build.ps1 -Target content-recipes` passes 50
  contract checks on stable FPC 3.2.0 and matched 3.3.1, then 42 actual Win32
  control checks, with zero native leaks. The exact exported supplementary-name
  companion is compiled unchanged by both actual consumers; source SHA256 is
  `e5904d58dd7797d90da486e6d50596b71e761ef289d8c05e5f1708fbba1d2ed9`.
- `browser-qualified-contracts.log` and `browser-qualified-controls.log`:
  the maintained Pascal browser driver executes 50 and 42 respectively through
  the existing isolated HTTP host. Concurrent wide/390-logical-pixel host views
  select different input/memo sets and independent stores. Actual editing retains
  exact supplementary text; repeated explicit remounts retain accepted values,
  dispatch callbacks once and retire old leases. Invalid inactive references keep
  the prior mounted root/store. These are actual programmatic controls; teardown
  captures do not establish aesthetics, physical phone/IME or assistive input.
- `core-qualified.log`: the maintained core regression and generated Unicode,
  event/state/managed-control/collection companions pass, including the existing
  invalid-argument compiler checks. `retained-regression.log` preserves 34 checks
  per native compiler, 92 original actual Win32 arrangements and 61 bound Win32
  checks, zero leaks. Its browser counterparts compile; current browser retained
  behavior retains the preceding packet's execution evidence rather than a rerun.
- `server/qualified-compile.log`, `web-studio/*-qualified-compile.log` and
  `native-studio/qualified-compile.log`: current backend, browser Studio, module
  worker and native Studio compile separately, with zero owned warnings. Browser
  builds retain seven warnings in the unchanged upstream RTL.
- `final-preservation.json`: all fifteen service identities and eight complete
  main project/navigation/draft/history frames remain exact; existing LAN HTTP
  returns 200. Only task-owned files inside a new isolated hosting subdirectory
  are staged. Existing source snapshots, compiler profiles and hosting artifacts
  stay unchanged. No listener is started or service stopped/restarted.

Qualification corrections retain the intended assertions: the unrelated managed
decorator now forwards the new public interface method. Native `StringReplace`
on an entire UTF-8 fixture source had transcoded supplementary references; exact
`TNyxText` span insertion fixes the fixture without changing the product's text
contract. Labeled fields expose wrappers, so physical input tests use public
`InputFor(..., niRuntime)`; nonexistent-control checks use the realized tree first.
Composer slot ancestry and early instance publisher overrides were product defects
found and fixed by the maintained shared consumers, including their browser run.

Initial adapter mounts choose automatic host recipes. Explicit `Render` remounts
may change descendants/control types against the same store. Live resize/container
selection and `Presentations.Select` still refresh scalar configuration only.
Remount replaces controls/coordinator/subscription and retires its presentation
lease; focus, caret and unfinished drafts do not survive by contract. Stable
changed-set publication, source/editor integration, ordinary Studio recipe editing,
other widgetsets/DPI, accessibility, release performance and delivery remain open
under the original owners. No task moves to DONE.

Implementation checkpoint `34e5477443e64a426bc22ca6bc26adaee693374f` is pushed to
`origin/hello-nyx`; its exact remote SHA is verified in
`build/content-recipes/remote-checkpoint.json`. The final readonly handoff check
again preserves all fifteen process identities and eight paired editor frames,
with existing LAN HTTP 200. The following documentation checkpoint records this
evidence; the full product goal remains active.

## Retained bound state — 2026-10-06

Owner: NS-2 criteria 1/2 as a prerequisite for original NS-4 responsive structures.
The portable `TNyxBindingSpec.Same` compares every copied descriptor field, including
open Unicode name, kind, direction and explicit clearing. Separate exact-binding
projection APIs preserve the strict unbound guard. The live coordinator requires
an active original subscription, unchanged identity/scope/control set and an idle
command/store. It projects detached candidate AND authored baseline against the
same current store before property admission. The original runtime root, store,
token and event binding objects remain attached. Fresh read-only state observations
are UI-thread contracts, not admission caches or cross-thread locks. Unbound views
avoid the additional projection clones.

Both adapters reuse the existing scalar or ordinary-host arrangement publication
and rollback paths with coordinator-prepared roots. Accepted-value markers retain
unfinished numeric drafts through moves and unrelated writes. Exact field restores
are fully checked before publication and reset only requested face markers, reading
current bound values rather than old authored defaults. This does not change store
revision or notify subscribers. Descriptor retarget/kind/direction/add/clear and
constructor changes retain their fresh-lifetime requirement.

Private qualification artifacts live under `build/bound-arrangement/`:

- `compiled-source-controls.log`: maintained ownership checks pass 34 per stable
  FPC 3.2 and matched 3.3.1; original actual retained controls pass 92 Win32, zero
  leaks. The companion is exact MCP-exported source, compiled with
  `tools/build.ps1 -Target retained-arrangement -ArrangementSourceDirectory ...`.
- `qualified-controls/run.log` and `browser-qualified-physical.log` /
  `browser-qualified-compact.log`: final bound controls pass 61 each native,
  ordinary browser and 390-pixel browser HOST. Checks include two independent
  stores, exact supplementary text, accepted decimal numbers, number drafts,
  selective restoration, physical width/visibility, focus/caret, repeated
  reparent/reversal, callbacks once, external writes, complete-group/descriptor/
  constructor refusals, validator/notification reentry and balanced teardown.
  Native heap tracing reports zero leaks. This is programmatic interaction through
  actual controls; hardware/IME/assistive input and an exact phone viewport are
  not claimed. Browser captures follow teardown and do not qualify aesthetics.
- `browser-qualified-retained.log`: original retained control consumer passes 76.
  `browser-qualified-owned.log`: portable ownership consumer passes 34.
  `browser-bindings.log`: original state/control consumer passes 48 browser checks.
  `regression-native/run-fixed.log`: the complete original native suite passes
  42 managed / 35 event / 50 binding / 75 Studio authoring checks plus its theme,
  catalog, derivation, optional-output, compact-inspector and Unicode/recovery
  journeys, with zero leaks. The earlier failed diagnostic-tab fixture and its
  correction are retained below; no original assertion was removed.
- `semantic-review.log` and `mcp-source/`: the maintained persistent Pascal
  `nyx_mcp_designer_review` creates an empty transport-owned review, composes twelve
  related operations as one paired transaction, reads two bounded source windows,
  builds both outputs and retires the review, preserving the service's primary
  editor frame. The English base appends the curated bound-review number controls
  to the existing arrangement fixture. Generated source is 138 lines / 3,218 bytes,
  MD5 `5d35e48ee1eada974a0f8bd8ee1bf964`; both downloaded immutable compiler sources
  match exactly. Browser job `3DA90AAC-8A62-483E-87C6-6E151C68D7B6` and LCL job
  `4589293A-6EE4-48DF-A9E8-83757D7D0C0C` succeed at revision 2 with current
  source/output. Browser has seven upstream RTL warnings, zero owned; native zero.
  These jobs use the protected existing container backend/source snapshot and
  establish companion compilation, not deployment of the new renderer. Current
  renderer execution is established by the checked consumers above. General MCP
  state/binding authoring is absent from this advertised transport; the consumer
  attaches those contracts with explicitly identified public typed Pascal calls.
- `server/compile.log`, `web/studio-compile.log`, `web/worker-compile.log` and
  `native-studio/compile.log`: current backend, browser Studio, module worker and
  native Studio compile separately. An automatic command review rejected starting
  the new isolated listener with only `blocked by policy`; no narrower reason was
  supplied. No stop/restart was attempted. Existing verified isolated hosting and
  persistent semantic review provided the safe qualification path instead.

Discovered/fixed qualification issues: the initial fixture omitted its event unit
and used a non-local loop counter; corrected before execution. Native comparisons
against an untyped decimal literal tested Extended precision rather than the public
Double contract; typed Double constants now retain exact authored values. A
constructor refusal fixture initially made its own numeric binding invalid; the
valid text/password constructor change now exercises reuse refusal independently.
The broad native authoring journey clicked diagnostics inside the hidden Messages
tab. Its fixture now follows Messages → owned location → Source and explicitly
checks the routed position; hidden controls remain unable to dispatch. No production
source navigation behavior is weakened. Browser driver compilation needs the
matched FPC package set (`fpwebsocket`); the stable compiler's absence is explicit.

The one-shot CLI review attempt correctly refused cross-transport ownership:
each `call` retires its transport. The maintained persistent consumer succeeds.
This client workflow limitation belongs to the existing MCP workflow owner; no
authentication or review-lifetime rule is weakened and no additional workflow
completion claim is made. Main eight pairs/navigation/draft/history frames remain
exact against the previous packet, and all fifteen service identities stay
protected. Only new files beneath an isolated review web subdirectory are staged;
existing hosting files, compiler profiles and source snapshots are unchanged.

No original full criterion closes. Renderer no-closure advances 5→6 once;
authoring 22, workflow 9, codegen 28 and delivery 1 remain. Lazy alternate recipes,
typed structural authoring, changed control sets, ordinary Studio switching,
IME continuity, other widgetsets/DPI, accessibility, fault-injected physical rollback,
release performance and the full product delivery remain open under original owners.
The main LAN release remains the existing container presentation release.

Implementation checkpoint `0db795ed183401ec5a88dcc537e2b107ff50139e` is pushed to
`origin/hello-nyx` and the exact remote SHA is verified in
`build/bound-arrangement/remote-checkpoint.json`. Final preservation verifies all
fifteen pre-existing service identities, eight exact accepted/navigation/draft/
history frames, LAN HTTP 200 and zero new fixture listeners. The existing isolated
host remains running with its original files/source/profile retained; this packet's
new review artifacts occupy only its dedicated subdirectory. All compiler/browser/
native test processes have completed. No task moves to DONE and the original goal
remains active. The next turn is fluent alternate structural recipes/publication,
with unchanged full acceptance criteria; do not restart local bound-state fixtures.

## Previous prerequisite: retained structural arrangement — 2026-10-06

Current structural gate (NS-4 authoring criterion 1, through the open NS-2 LCL
ownership/interaction prerequisite): first qualify retained rearrangement of an
unchanged realized control set. Both roots must retain exact runtime/source/design
identities, instance scopes, contracts and creator context. Prepare all ownership
arrays before publication; refuse additions, removals, changed bindings/factories,
special pane hosts and ambiguous identities. Keep the existing scalar guard intact.
Actual browser/Win32 controls must retain draft text, caret, focus and callback
registrations through reparent/order changes and reversal, with balanced teardown.
This is a prerequisite for alternate presentation structures, not that feature's
acceptance. Lazy alternate recipes, typed structural authoring and ordinary Studio
presentation switching remain the return path. Preserve the main LAN service,
eight project pairs and all auxiliary process identities; use isolated fixtures.

The [qualified foundation](#retained-structural-arrangement--2026-10-06) prepares
all owner arrays/implementation anchors before rearranging an unchanged realized
control set. Separate admission keeps the original scalar guard strict. Actual
browser/Win32 faces retain inputs, ranges, parents/order and callbacks through
repeated moves and reversal. Core ownership checks pass 34 per compiler/browser;
compiled MCP source passes 91 native / 75 browser at ordinary and 390-pixel host
widths. Existing native editing checks pass 118, with zero leaks. Both immutable
MCP application jobs compile identical 120-line/2,693-byte English source.

No ordinary Studio structural-presentation journey or LAN rollout is claimed.
Renderer no-closure advances 4→5 once; authoring 22, workflow 9, codegen 28 and
delivery 1 remain. No original full criterion closes. End rearrangement/fixture
expansion and return to typed alternate structures, retained bound state, publication,
ordinary editor/target parity and remaining original prerequisites.

## Previous prerequisite result: native measurement cost — 2026-10-06

Original authoring criterion 1 still needs alternate view structures with explicit
ownership/publication and retained state. Its parity prerequisite depends on the
open [LCL renderer](TODO/NS-2_lcl-renderer_01.md), whose model/catalog blockers are
accepted. The checked 30-step native Studio journey allocated 78,152,553 blocks /
1,908,781,421 cumulative bytes with zero leaks. That is allocation churn, not peak
memory or release latency. Measure this existing resize/interaction prerequisite
before increasing structural tree depth; keep the full authoring return path.

Gate: use identical MCP-authored accepted source and the unchanged ordinary native
Studio journey; count intrinsic traversals, compare the same checked workload and
toolchain, and preserve exact layout, retained input, resize, presentation changes
and teardown. Any reuse must have an explicit bounded lifetime and cannot persist
across model/widget changes. Qualify corresponding browser controls, without
claiming that a native optimization proves browser performance. Current LAN
service, all project pairs and fourteen auxiliary identities remain untouched.
The [qualified result](#allocation-free-property-lookup--2026-10-06) now removes
candidate-name allocations, without a persistent measurement cache. The unchanged
30-check native journey allocates 23,493,324 blocks / ~816 MB cumulatively versus
78,252,212 / ~1.91 GB in its reproduced baseline, with zero leaks. Exact text checks
pass 133 per compiler/browser; native layout 2,169 / container 27, browser layout
2,214 desktop / 2,215 exact-390 / container 27, and actual browser Studio 74 each.
Static platform generation is also restored after an existing false viewport-key
probe regression; original split/platform 141 and presentation 72 pass per native
compiler. Twenty tools authenticate on an isolated current backend, whose MCP
application jobs compile identical 83-line/1,690-byte typed platform source.

The implementation is staged and qualified; the current main LAN service and all
eight projects are untouched. No grouped main release is claimed. Renderer
no-closure advances 3→4 once; authoring 22, workflow 9, codegen 28 and delivery 1
remain. No original full criterion closes. End local lookup/cache expansion and
return to the original structural ownership/publication gate and remaining
parity/accessibility/performance/editor/delivery prerequisites.

## Current integrated result: container-aware presentations — 2026-10-06

The [container packet](#container-aware-presentations--2026-10-06) adds typed named
ancestor publishers, width/full-size containment and copied actual logical
content-box measurements. Reusable instances adapt independently at one unchanged
host size. Nearest eligible ancestry excludes self; unavailable boxes stay inactive
without borrowing another instance's dimensions. Contained axes receive ordinary
external fill/flex/stretch/bounds allocation, without descendant intrinsic feedback.
Persistence, crafted Pascal, scalar scopes, Studio Inspector/worker and semantic
transactions share the public portable contract. Independent coordinate spaces
are admitted atomically; correlated rules retain their common publisher.

Checks pass 32 per native compiler/browser, 27 per actual browser/Win32 control
consumer (English and compiled supplementary-name fixtures), 30 native Studio
and 74 per desktop/exact-390 ordinary browser Studio journey. The preceding 72
presentation checks remain green on both compilers/browser. Deployed semantic
application jobs compile the exact 216-line/5,900-byte English companion; bounded
MCP refusal/history/source checks pass 43 in its explicit workspace. Twenty tools
authenticate. Both output jobs have zero owned warnings; the browser retains seven
upstream RTL warnings. Selective immutable MCP captures show wide independent cards
and the compact stacked phone presentation.

The nine-artifact LAN release restores seven exact accepted pairs/selections/views
and retains fourteen auxiliary service identities. The necessary backend restart
resets prior histories and remaps concurrent handles; this is not history retention.
The main service is PID 41164, creation `2026-10-06T05:16:07.843166-04:00`, installed
at `build/native/3.2.0/i386-win32/nyx_studio_server.exe`, serving
`build/container-presentations/release/web`. HTTP binds all interfaces; MCP remains
loopback. Sixteen served web hashes match. `.local/container-refresh-20261006/`
owns exact seven-pair backups, identity, manifest, rollback, mapping, bounded build
receipts and the independent English **Container workshop** project. It supersedes
the manual release and subsequent Studio/worker overlays. Never rerun their scripts.
Primary revision 2 remains exact; Container workshop is selected at revision 5.

This integrated batch is ended. Authoring no-closure advances 21→22 once; workflow
9, codegen 28, renderer 3 and delivery 1 remain. No original criterion closes and
the original prerequisites remain hard gates. Stop local container/parser/fixture
expansion. Next declare the ownership/publication gate for original criterion 1's
alternate view structures, then complete ordinary editor/parity/accessibility/
performance/delivery and their unaccepted prerequisites. Current logical axes use
horizontal writing; physical phone input, other widgetsets/DPI, assistive technology
and performance budgets remain unqualified. Checked native Studio tracing shows
large allocation churn despite zero leaks; no release latency claim is inferred.

## Current integrated result: flow placement previews — 2026-10-06

The [flow packet](#flow-placement-previews--2026-10-06) adds copied typed placement
policies and physical/logical target frames, inert insertion paint on both adapters
and optional automatic row/column placement in ordinary Studio. A separate Nyx
drag button remains available beside the placement selector in compact Design.
Hover changes neither accepted file; release retains exact ownership/last-frame
agreement and publishes one paired Undo. Refused hover retires old agreement.
Checks pass 98 per native compiler/browser, 45 actual Win32 Studio callbacks and
36 per desktop/exact-390 host browser journey. Both deployed MCP application jobs
compile exact 178-line/4,267-byte English source. Twenty tools authenticate.

The one-editor-asset LAN overlay preserves seven exact paired projects/navigation/
history states, the byte-identical v9 worker and fourteen service identities.
Sixteen served hashes match. `.local/flow-assets-20261006/` owns preservation and
immutable build receipts, the overlay manifest, byte backup and independent English
**Flow workshop** handle. It supersedes only the Studio entry of the move overlay;
the main process remains PID 38744 and primary revision 2. No restart or history
reset. Native named handles in this chat still need reconnect; the authenticated
Pascal semantic client remains primary. Do not run obsolete refresh scripts.

This integrated batch is ended. Authoring no-closure advances 20→21 once;
workflow 9, codegen 28, renderer 3 and delivery 1 remain. No original criterion
closes. Stop local flow-policy/fixture expansion. Return to original criterion 1's
stable container allocation and alternate view structures, then the complete
ordinary editor/parity/accessibility/performance/delivery requirements and their
unaccepted prerequisites. Automatic grid/absolute insertion, uncommitted canvas
draft retention across structural reparent and full nested scrolling remain open.
The compact HTML select's text still clips in a host capture despite its wider
face; complete chrome typography/overflow remains an existing visual-quality gap.

## Preceding integrated result: absolute-position movement — 2026-10-06

The [move packet](#absolute-position-move-snapping--2026-10-06) now delivers public
typed policies, copied edge/center guides, ordinary browser/Win32 gestures,
retained live inputs and one paired Undo. Checks pass 99 per compiler/browser,
22 native Studio and 49 per desktop/exact-390 browser journey. Both deployed
semantic application jobs compile exact source. The LAN Studio/worker overlay
retains the running server, all five existing pairs/navigation/history states
and fourteen exact process identities. Sixteen served web hashes match; twenty
MCP tools authenticate. `.local/move-assets-20261006/` owns the two-asset overlay
manifest, byte backups, exact frame receipts and English **Move workshop** project
handle. It supersedes only Studio/worker entries of the preceding nine-artifact
manifest; executable/helper/runtime/HTML assets remain unchanged. No restart or
history reset. The primary remains at revision 2. Do not run old refresh scripts.

This integrated batch is ended. Authoring no-closure advances 19→20 once;
workflow 9, codegen 28, renderer 3 and delivery 1 remain. No original criterion
closes. Stop local move-policy/fixture expansion. Next inspect original criterion
1's reparenting/flow placement and snapping integration, preserving one semantic
paired publication and exact cancellation. Declare its coordinate/ownership
gate before another implementation packet. Stable container allocation and
structural variants remain separate open gaps, followed by complete ordinary
editor/parity/accessibility/performance/delivery. Preserve active/concurrent pairs.

The underlying [manual presentation release](#manual-presentation-selection--2026-10-06)
serves nine qualified artifacts over all-interface HTTP, with loopback MCP and
twenty authenticated tools. Four existing pairs/selection/view states remain
exact. Restart resets prior histories; concurrent projects receive new handles
and one import Undo step. The main server is PID 38744, creation
`2026-10-06T02:38:34.900485-04:00`, using the firewall-covered installed executable
and `build/manual-presentations/release/web`. Twelve auxiliary services retain
their exact identities. `.local/manual-presentations-refresh-20261006/` owns the
current identity, manifest, four paired backups, workspace mapping and rollback.
It supersedes previous release/overlay records; never run their old refresh
scripts. The independent English **Manual presentations** project is available
in Agents. Actual browser/LCL application jobs and selected semantic captures
pass. The primary remains exact at revision 2. Native named handles in this chat
still initialize against an obsolete cached endpoint (HTTP 404); the authenticated
Pascal semantic client remains primary until this chat reconnects.

The preceding [named presentation release](#named-responsive-presentations--2026-10-06)
serves nine exact qualified artifacts over all-interface HTTP, with MCP loopback
and twenty authenticated tools. The primary and two concurrent pairs/selection/
view states remain exact. Restart resets prior histories; concurrent projects
receive new handles and one import Undo step. The main server is PID 21036,
creation `2026-10-06T01:51:03.752648-04:00`, using the firewall-covered installed
executable and `build/presentations/release/web`. Eleven auxiliary services retain
their exact identities. `.local/presentations-refresh-20261006/` owns current
identity, nine-artifact manifest, three paired backups, workspace mapping and
rollback. It supersedes earlier process/overlay records; do not run their scripts.
The English **Shared presentations** project is independent and available in
Agents. The Pascal MCP client is connected; native handles in this chat still
cache the previous endpoint and need a reconnect.

The preceding [alignment packet](#alignment-guides--2026-10-06) updates the served
Studio asset without restarting its LAN server. Process/listeners, byte-identical
worker and exact primary pair/revision/selection/view/history are retained;
loopback/LAN asset hashes match. `.local/alignment-assets-20261006/` owns the
current Studio overlay manifest, backup and pair receipts. It supersedes only
the Studio asset entry of the preceding full-release manifest.

The user's latest explicit LAN instruction is fulfilled. The
[host-condition release](#responsive-host-conditions--2026-10-06) now supersedes
the preceding process/asset records: nine qualified artifacts, current server
identity, exact primary pair/selection/view preservation, served loopback/LAN
hashes and nineteen authenticated MCP tools are verified. Private current
identity/manifest/backup/rollback are under `.local/form-factors-refresh-20261006/`;
the web root is `build/form-factors/release/web`. Do not run previous refresh
scripts against their obsolete identities. The normal service restart resets
in-memory history; it preserves the primary pair and leaves auxiliary services
unchanged. The English MCP example is an independent project shown in Agents.

The preceding
[responsive editor follow-through](#ordinary-responsive-studio--2026-10-06) also
refreshes current Studio assets without restarting the service; installed/served
hashes match, the worker remains byte-identical, and primary revision/selection/
view remain unchanged. The current private asset receipt supplements the
preceding server release manifest below.

The preceding
[current release refresh](#current-lan-release-refresh--2026-10-05) now serves
the current checked server, Studio and worker on the existing all-interface HTTP
endpoint; MCP remains loopback. Exact installed/served hashes and nineteen
authenticated tools are verified. The existing executable path preserves the
working firewall allowance. Source-editor regression passes 30 desktop / 30
exact-390 against current code. The active pair/selection/view remain exact;
restart resets in-memory history, now revision 2. Seven auxiliary services remain
unchanged. The previous blocked/pending refresh below is historical and superseded;
do not execute its obsolete process manifest. Private current identity, artifact
manifest, paired backup and rollback live under the current refresh record.

Preceding independent work: [named responsive presentations](#named-responsive-presentations--2026-10-06)
qualify original authoring criterion 1 through managed fluency, exact Unicode
wire/source/history, immutable copied ownership, retained browser/LCL inputs and
ordinary Inspector/worker/semantic editing. The nine-artifact release is staged
and three existing pairs/navigation states qualify exactly after LAN deployment.
This goal turn is progress, not full completion. Authoring
no-closure advances 17→18 once; workflow 9, codegen 28, renderer 3 and delivery 1
remain. End local named-rule/parser/fixture expansion. Continue original
container/manual/structural variants, full move snapping and complete ordinary
editor/parity/accessibility/performance/delivery. Preserve the original
prerequisites, semantic framing/transport gaps and physical-device limits.

Current reassessment — manual presentation selection: original authoring
criterion 1 retains its blockers. The integrated packet below advances the
no-closure count 18→19 once; workflow 9, codegen 28, renderer 3 and delivery 1
remain. The bounded container
inspection ended: native natural measurement depends on descendants and browser
candidates initially mount detached. Direct parent client measurements alone
cannot establish stable, feedback-free allocation or per-instance semantics.
Do not implement guessed geometry. Container queries need an explicit allocation/
containment contract and remain open. The materially different next deliverable
is typed exclusive manual presentation selection, with automatic host rules
retained, independent view-local state and no design/history writes. Evidence
now includes strict source/wire admission, both actual retained adapters,
ordinary Studio preview/Undo and semantic MCP authoring/builds. This integrated
batch is ended. Stop local manual/parser/fixture expansion. Continue the original
allocation/containment and structural-variant gaps or full move snapping, then
ordinary editor/parity/accessibility/performance/delivery. The original criteria
and prerequisites remain; no original criterion or full-goal percentage closes.

Preceding declared deliverable: full move snapping under original authoring criterion
1. Inspect the existing placement/drag and public alignment contracts first;
preserve semantic ownership/revision guards and one paired publication. Establish
absolute-position versus flow placement behavior, coordinate conversion and
responsive-scope refusal before implementing ordinary browser/LCL gestures.
Container allocation and structural variants remain separately open; this choice
does not reset authoring's no-closure count or bypass its prerequisites.

Preceding independent work: [alignment guides](#alignment-guides--2026-10-06) now
has public copied geometry, both adapters and ordinary Studio evidence. The goal
turn is progress; it closes no full criterion/prerequisite. Authoring no-closure
advances 16→17 once; workflow 9, codegen 28, renderer 3 and delivery 1 remain.
End local guide/fixture expansion. Next continue original named/container
responsive presentations, move snapping and full ordinary editor/parity/
accessibility/performance/delivery. Explicitly retain the responsive canvas
selection, nested-scroll/virtual geometry and semantic source-framing gaps.

Preceding independent work: [responsive host conditions](#responsive-host-conditions--2026-10-06)
under original authoring criterion 1. Copied typed width/height/orientation rules
share managed authoring, strict persistence/source admission, both adapters and
the ordinary Inspector/paired processor. Shared checks pass 61 per native compiler
and browser; unchanged MCP source passes 30 actual Win32 controls, 31 per browser
size, nine ordinary native Studio combined Inspector/Undo and 22 per browser
Studio size. Actual LAN MCP jobs succeed on both outputs; grouped Undo/Redo
restores exact 100-line source. Authoring no-closure advances 15→16 once; workflow
9, codegen 28, renderer 3 and delivery 1 remain. Stop local condition/parser/test
expansion; continue original named/container variants, guides and complete
editor/parity/accessibility/performance/delivery outcomes. Desktop captures still
show view-bar title wrapping with the Inspector open. Phone keyboard/visual
viewport, trusted hardware/IME/assistive technology and other widgetsets remain
unqualified. The preceding goal turn made progress through retained ordinary
Studio input, current LAN UI assets and an exact remote checkpoint.

Preceding independent work: [ordinary responsive Studio](#ordinary-responsive-studio--2026-10-06)
under original authoring criterion 1 and the user's form-factor steering. The
ordinary browser Inspector reaches its module worker and shared paired history;
compatible remote/Undo updates and compact panel/preview changes retain live
canvas input. Public width conditions now shape Studio's compact source captions,
placement width and Agents visibility; conflict choices precede optional details.
Actual checks pass 22 desktop / 22 exact-390, source workspace 30 per browser size,
nine native responsive Studio and 30 native source workspace, with zero checked
leaks. MCP application compilation succeeds on both targets with exact source/
design/output receipts. Authoring no-closure advances 14→15 once; workflow 9,
codegen 28, renderer 3 and delivery 1 remain. Stop local width/fixture expansion;
continue original responsive conditions, guides and full editor/parity outcomes.
The preceding goal turn made progress by deploying the checked LAN release and
verifying nineteen authenticated tools; its historical approval blocker is resolved.

Preceding independent work: [typed responsive authoring](#typed-responsive-authoring--2026-10-05)
under original Studio authoring criterion 1. Managed public width conditions,
typed generation/admission, retained target adapters and the Nyx-built Inspector
share ordinary paired commands. Shared checks pass 33 per native compiler and
33 in the executed browser contract; unchanged MCP source passes 22 actual Win32
controls, nine ordinary native Studio checks and 23 desktop / 23 exact-390 browser
checks, with zero checked native leaks. First/last-rule refresh preserves input
and owns observer lifetime. Actual MCP browser/LCL jobs succeed; bounded source
and one grouped Undo/Redo retain exact generated Pascal. Core and retained
projection regressions pass. Authoring no-closure advances 13→14 once; workflow
9, codegen 28, renderer 3 and delivery 1 remain. End local interval/fixture
expansion. Next qualify ordinary browser Studio inspector/worker execution and
continue original responsive variants, guides, editor/parity/accessibility and
delivery outcomes. This independent packet retained the prepared release-refresh
closure and protected services/pair before the separately authorized LAN refresh
above. Its earlier refresh gate is now superseded by the successful deployment.

Preceding independent work: [direct canvas resize handles](#direct-canvas-resize-handles--2026-10-05)
under original Studio authoring criterion 1. Managed public Nyx adornments now
mount specialized button handles on the selected face in both target adapters.
Stable-plane mapping preserves deltas as handles move; scope retirement revokes
the shared gesture lease without reentering paint. Shared checks pass 56 per
native compiler, unchanged compiled controls seven, actual Win32 Studio 70 and
retained projection 27, with zero checked leaks. Browser adapter checks pass 47
desktop / 47 exact-390, including actual precision-key listeners and retirement;
ordinary browser Studio pointer input and physical phone remain unqualified.
Authoring no-closure advances 12→13 once; workflow 9, codegen 28, renderer 3 and
delivery 1 remain. End grip/mapping/fixture expansion. Next implement responsive
authoring through public typed configuration and the same paired processor;
complete original snapping/guides/editor/parity/accessibility/delivery remain.
The pending user-local release closure and all protected projects/services stay
intact. The preceding goal turn was progress: public proposal paint was committed
and pushed as `dbf27b320b0034b5d91d6e5907f741fbe4043c8b` with actual target evidence.

Preceding independent work: [canvas resize presentation](#canvas-resize-presentation--2026-10-05)
under original Studio authoring criterion 1. Public copied proposals now paint
through both canvas adapters without accepting per-pixel edits or changing input.
Shared checks pass 54 per native compiler, unchanged compiled controls seven,
actual Win32 Studio 59 and retained projection 27, with zero checked leaks.
The dedicated browser adapter journey passes 30 desktop / 30 exact-390 checks;
inspected captures qualify that bounded consumer, not ordinary browser Studio
grips or physical phone input. An owned MCP workspace was composed in one grouped
transaction and compiled from bounded exact source windows; one Undo/Redo
restores empty/accepted state and exact Pascal. Authoring no-closure advances
11→12 once; workflow 9, codegen 28, renderer 3 and delivery 1 remain. End local
paint/capture/fixture expansion. Next connect direct canvas handles and responsive
authoring through public contracts and the existing isolated paired processor.

Superseded user-local refresh: [reviewable release refresh](#reviewable-release-refresh--2026-10-05).
The earlier user request produced a separately staged
Pascal service now authenticates all nineteen tools and serves the exact backed-up
active pair. Actual browser source-workspace checks pass 30 at desktop and 30 at
390 pixels, including retained text/range, expanded tab refresh, Close and the
owned cancel/focus return. The public browser modal reconnects its retained host
after a surrounding shell remount; this fixes the observed Expand failure.
Replacing the LAN service was rejected by automatic approval review with only
`blocked by policy`; its process, pair, revision/history and configuration remain
unchanged. A private, parsed refresh script with qualified artifact hashes,
exact process checks, backup/restore and rollback is ready for the user to run.
That checkpoint did not refresh the phone release. The user's later explicit LAN
instruction is fulfilled by the guarded current refresh above; its earlier
process manifest is obsolete and must not be run.
Codegen criterion 3 remains open at no-closure 28; workflow 9, renderer 3,
authoring was 11 and delivery 1 remained at that checkpoint. While the local
refresh is pending, independent canvas work above leaves its qualified artifact
manifest, candidate editor closure, active user pair and protected services
intact. No new deployment or phone refresh is claimed.

The preceding user-directed packet covers source workspace usability under original codegen
criterion 3: Source/Compiler messages views and an expanded floating source
editor with Close/Escape, retained draft/selection, project preferences and
portable public Nyx modal adapters. An earlier goal turn qualified the verified
clean/pushed declaration checkpoint 845c698/2ba9107. Its guarded
paired signature return path now has 20/59 lexical/semantic checks on both native
compilers, actual discovery 50, compiled native input nine, all three stale-caller
compiler diagnostics and routine/callback regressions 31/72, with zero leaks.
That source UX passes 30 actual Win32 and 225 shared presentation/ownership
checks on each native compiler. Browser compilation cannot close its existing
execution/deployment gate. At that checkpoint, workflow no-closure was 8,
codegen 27, renderer 3, native authoring 7 and delivery 1. Stop on lost editor
identity/input, disabled-owner return, guessed source ownership or weakened
publication; preserve failed evidence and the protected observing release.

Preceding bounded packet: [reusable resize grips](#reusable-resize-grips--2026-10-05)
connects original Studio authoring criterion 1 to public Nyx pointer/key controls,
typed snapping/bounds and one isolated paired size operation. Shared checks pass
51 per native compiler; unchanged compiled controls pass seven and ordinary
Win32 Studio 40, with zero leaks/owned warnings. Retained projection/placement
regressions pass 27/44; core and staged browser consumers/Studio/worker compile.
Compatible dimension refresh preserves admitted controls and independent input.
Preview currently reports dimensions in status; direct canvas edge feedback,
richer guides, responsive variants and broader editor/parity outcomes remain.
No original criterion closes: authoring no-closure advances 10→11 once; workflow
9, codegen 27, renderer 3 and delivery 1 remain. End local grip/codec/fixture
expansion after relevant checks. Next connect direct canvas resize feedback and
responsive authoring, preserving public contracts and the same paired processor.
Preserve exact pairs/input/ownership and the protected observing release. No
listener, deployment or private configuration refresh; current browser/phone
runtime retains the existing host gate. Stop on a second authoring engine,
weakened admission or editor identity/input loss.

Preceding bounded packet: [portable size constraints](#portable-size-constraints--2026-10-05)
supplies original Studio authoring criterion 1's missing resizing prerequisite.
Copied typed bounds, specialized generated configuration, effective target
validation and weighted redistribution share ordinary semantic/isolated property
admission. Shared checks pass 237 per native compiler; unchanged compiled Win32
controls pass 38 and ordinary Studio set/unset/history passes 22, with zero leaks
and owned warnings. Original native layout/policy regression passes 2169; core
regressions and staged browser consumers/Studio/worker pass compilation. Browser
runtime and updated phone observation retain their existing gate. Authoring
no-closure advances 9→10 once; workflow 9, codegen 27, renderer 3 and delivery 1
remain. No original criterion closes. End bounds/allocator/fixture expansion;
next connect physical resizing and snapping to the same typed policies, public
input contracts and isolated paired operations, then responsive variants and
broader original editor/parity outcomes. Preserve exact pairs/input/ownership
and the protected release. No listener, deployment or private configuration
refresh. Stop on a second authoring engine, weakened admission or input loss.

Preceding packet: [physical designer drag/drop](#physical-designer-dragdrop--2026-10-05)
consumes the nested placement prerequisite under original authoring criterion 1.
Public Nyx sources and explicitly opted-in design adapters now submit the same
isolated paired operation. Shared guards pass 56 on each native compiler; actual
Win32 Studio/input policy passes 40 and its unchanged compiled consumer seven,
with zero leaks. Browser contract/compiled/DOM Studio reviews, Studio and module
worker compile; execution and updated phone observation remain gated. Authoring
no-closure advances 8→9 once; workflow 9, codegen 27, renderer 3 and delivery 1
remain. End drag codec/guard/fixture expansion after relevant regression checks.
Continue ordinary sizing/constraint/responsive authoring and broader original
editor/parity outcomes, using the public portable library and existing paired
processor. No criterion closes. Preserve exact leases/ownership, the active user
pair and protected observing release. Do not launch listeners, replace releases
or refresh private configuration. Stop on a second authoring engine, inferred
ownership or weakened paired publication.

Preceding verified clean/pushed checkpoint: `908331b7fc8988d223b47ee922f05dade88169bb`.
Its [nested placement prerequisite](#nested-placement-prerequisite--2026-10-05)
serves original Studio authoring criterion 1 through typed inside/before/after
commands, the existing semantic transaction and isolated paired processor, and
ordinary Nyx-built two-step placement controls beside selection. Both native
compilers pass 44 semantic/worker checks with byte-identical exports; actual
Win32 Studio/unchanged compiled controls pass 18, discovery 24 and retained
source/queue/reusable regressions 140/10/52 on both compilers, with zero leaks or
owned warnings. Final inspector rendering is inspected; browser consumers,
Studio and worker compile with execution still gated. At that checkpoint native
authoring no-closure was 8; workflow 9, codegen 27, renderer 3 and delivery 1 remained.
Placement codec/fixture expansion ended after those relevant checks. The next
action connected public Nyx drag/drop to this same operation and keyboard
alternative, with source/load/pair/creator/lease guards retained in the packet above.
This prerequisite alone cannot accept drag/drop, resizing, constraints,
snapping, responsive variants or complete both-target editor parity. Preserve
the observing release and existing listener/deployment gate. Stop on a second
authoring engine, inferred ownership or weakened paired publication.

Preceding bounded packet: [semantic reusable authoring](#semantic-reusable-authoring--2026-10-05)
serves original workflow criterion 5 through the existing transaction/query
tools: typed derivation/instantiation/part overrides/restored inheritance, exact
descendant identities and bounded effective named-part discovery. Related edits
use one paired Undo step. The ordinary inspector shares derivation admission;
helpers/contracts/defaults/history remain owned. Both native compilers pass 52
semantic checks, actual discovery 20 and unchanged compiled Win32 controls 14;
browser consumers/Studio/module worker compile, with execution still gated.
No criterion closes: workflow no-closure is now 9; codegen 27, renderer 3,
native authoring 7 and delivery 1 remain unchanged. End local reusable fixture/
schema expansion after these relevant checks. Return to the broader ordinary
editor and creator/source integration outcomes under their existing owners.
No listener launch, protected-release replacement or configuration refresh was
attempted; no alternative deployment substitutes the existing host gate.

Latest bounded packet: [source workspace and expanded editor](#source-workspace-and-expanded-editor--2026-10-05).
Native/modal qualification and original editor/source regressions are recorded
below; actual current browser/phone observation still needs its permitted host.
End local modal/fixture expansion after these relevant checks. Return to the
remaining source/reusable integration and complete both-target ordinary editor
outcomes with their existing owners; no partial source/Win32 claim substitutes
full presentation, accessibility, performance or deployment acceptance.

Preceding bounded packet: [semantic helper declarations](#semantic-helper-declarations--2026-10-05)
extends `nyx_pascal` with exact signature windows and typed grouped helper
creation, implementation editing and removal through one paired Undo step.
Both native compilers pass 16 lexical and 36 semantic checks; final actual
discovery passes 46, unchanged compiled native input nine and routine/callback
regressions 31/72, with zero leaks/owned warnings. The maintained
`pascal-declarations` command executes its actual consumers.
Current browser consumers/Studio/module worker compile; execution and authenticated
updated observation retain their host gate. Source catalog nineteen/protected
release fifteen remains explicit. Workflow criterion 5 stays open at no-closure
7; codegen 26, renderer 3, native authoring 7 and delivery 1 remain unchanged.
Signature/class/full-unit authoring, richer reusable
workflows and complete editor performance/presentation remain required. The
preceding [helpers](#semantic-pascal-helpers--2026-10-05),
[imports](#semantic-pascal-imports--2026-10-05) and
[collections](#semantic-structured-collection-commands--2026-10-05) retain their
original evidence and acceptance limits.

Earlier bounded source packet: [canvas proposals](#isolated-canvas-input-and-exact-field-reconciliation--2026-10-05)
now share isolated paired admission. Owned view/runtime/owner/platform and mounted-load identities replace
worker access to live realized nodes. Fresh replay preserves typed state defaults
and instance-only named parts. Exact field restoration fixes the actual rejected
Integer-input defect without claiming successful publication or overwriting newer
queued input. The focused native packet passes 47 actual canvas and 27 retained
projection checks, with zero leaks; both native compilers pass 140 shared/wire
checks. Both complete maintained native matrices pass, with zero leaks in all nine
checked consumers and exact companion hashes agreeing. English desktop/390 native
captures are inspected; browser Studio/module worker/shared consumers compile,
with changed browser runtime behavior still pending at its host gate.

The previous goal turn was progress: ddc67b3/79e3906 changed mounted native hosts
and qualified original-size optimized builds, leaving an exact clean/pushed
checkpoint. This turn changes product/evidence state through actual isolated
canvas editing and rejection reconciliation. Original codegen criterion 3 remains
open; its bounded-packet no-closure count advances 21→22 once, with renderer 3,
native authoring 7 and delivery 1 unchanged. No full criterion, comfortable-editing
or parity acceptance follows this packet. Earlier evidence keeps its original scope.

Next follow broader source/state/binding/event integration and its existing
semantic workflow owner, preserving fresh admission, handwritten meaning,
independent ownership and paired history. Ordinary browser/observing outcomes
still require the recorded permitted host without an equivalent refused launch.
Stop or switch on accepted-tree worker access, lost/reordered input, stale meaning,
retargeted ownership or weakened admission. No scope/count reset, another
timing/lookup variant or weaker DONE gate substitutes the full intended outcome.

Completed current batch follows workflow criterion 5 and original codegen
criterion 3. Typed state/default and binding edits now use bounded queries,
revision/authority-bound receipts and one paired Undo step. Callback authoring
retains its existing qualified semantic path. Native source/history/reference/
reusable/refusal and compiled-control evidence is assessed below; updated
browser/listener qualification retains the existing automatic-review gate.
The previous goal turn was progress: 4b753e9/1354fc3 are an exact clean/pushed
canvas checkpoint. This turn changes usable semantic vocabulary and fixes actual
staged discovery; no full criterion closes. Counts advance once as stated above.
Next connect ordinary state/default and binding inspectors to isolated typed
admission with explicit rename commit semantics. Naively replaying every rename
keystroke against its previous name would lose or retarget later input; do not
implement that shortcut. Stop on partial admission, reference retargeting, lost
source/history/input or weakened fresh guards. No repeated fixture expansion,
scope/count reset or narrower DONE/parity claim substitutes the remaining outcome.

Completed bounded packet under original codegen criterion 3 extends the existing
isolated source-command processor to ordinary visual property and structural
commands. Capture immutable paired/context values, replay those commands on an
independent session, and publish one admitted pair through the existing fresh
session/content/draft/creator guard. Evidence covers real controls, exact original
128/512/2048 sizes, preserved handwritten source, paired Undo/Redo and retirement.
Stop on accepted-tree worker access, lost/reordered edits, stale publication or
detached receiver delivery. Complete browser/ordinary-editor acceptance retains
its existing host gate; no full criterion is accepted from this packet.
Previous turn was progress: source 9e1498c and handoff ad9a80e are clean/pushed,
the concrete viewport failure is resolved, and this changes the next action.

The completed bounded implementation packet follows codegen criterion 3: coalesce source-draft capture
through the existing owned editor timer, share one fresh paired snapshot with
browser recovery, and retain unsent typing across observing replies. Typing must
update the ordinary Nyx editor/session immediately; history, builds and project
navigation retain exact acknowledged-frame guards. Qualify private protocol
ordering/conflicts, real native controls and the unchanged large source sizes;
stop on dropped draft text, reordered accepted commands, stale remote adoption
or receiver delivery after retirement. Browser execution retains its existing
host gate. The following renderer packet resolves the native viewport failure;
detached visual/structural reconciliation and comfortable editing remain required.
The preceding English preference turn confirmed existing content without changing
authoritative goal state: no progress toward the full goal, now revalidated with
current source, connected semantic session and protected process identities.
No count or acceptance gate is reset by that preference check.

Completed renderer packet follows the recorded native viewport prerequisite under
NS-2_lcl-renderer criteria 1/2, consumed by original codegen criterion 3. Keep full
logical extents and every control's ownership/text; project physical geometry
into a bounded native viewport and reveal exact identities for selection/focus.
Qualify coordinate boundaries, real large mixed controls, scrolling/resize,
focus/caret, original 128/512/2048 Studio input and existing layout consumers.
Stop on inaccessible descendants, lost drafts, false coordinates, callback
retirement or a silent size clamp. Browser compilation remains insufficient for
parity at the existing host gate. The preceding turn was progress: product source
3b700e6 and handoff fa28d20 are clean/pushed, and the preserved full-size failure
changed this next action. No live service or user pair is replaced.

Latest user authorization: the current project is disposable test content and
may be reset/removed when useful. Its exact prior contents are no longer an
operational preservation prerequisite. Keep the recorded fixture/baseline
evidence and continue qualifying protection of other projects as a product
invariant; this authorization does not weaken concurrent-session acceptance.
The earlier automatic production service replacement refusal remains separate.

User steering (2026-10-04): extend the protected workspace foundation toward
concurrent project sessions in one Studio service. The Agents view should show
which sessions/projects are working and let the user jump into their full editor
and return with each project's draft/history/view retained. Temporary test
workspaces and user project sessions require explicit, different lifetimes;
agent requests and compiler completions retain their own context when the
observing user switches projects. Existing NS-4 workflow and authoring owners
now retain these acceptance criteria. Preview links alone do not satisfy them.
The independent server remains a qualification fixture. The portable ownership
and full browser jump/return foundation now has evidence below; successful live
warned closure and full native editor navigation remain open. The full concurrent
authoring criterion is not accepted.

Protected-review prerequisite now has an integrated qualification packet below:
200 native/executed-browser lifecycle checks, 377 real MCP/ordinary Studio checks,
both actual compilers, exact compiled callbacks on both targets and preserved
production/user-fixture bytes. Teardown passes with zero leaks. The ordinary
browser shell retains its independent public Nyx code-editor view during agent
activity. Desktop/narrow/resize source and editing gates pass. This accepts the
bounded protected-review prerequisite, not original workflow criterion 5's full
state/binding/reusable/general-source scope, project concurrency or deployment.

The requested bounded warning cleanup under NS-6 delivery criteria 1/4 now has
accepted execution evidence below: zero owned warnings on qualified native,
actual LCL and pas2js consumers, including both current live MCP compiler jobs.
Seven installed pas2js RTL warnings remain visible; dependencies were not edited.
Shared Unicode/source/history, real controls and Studio resize/split checks pass.
The entire delivery task/CI matrix remains open (NS-6 no-closure count 1).
End warning inventory and return to the remaining concurrent-project gates;
their explicit reassessment/count 2 and service launch refusal remain in force.

Previous batch: the user's concurrent-project outcome under NS-4 workflow criterion
6 / authoring criterion 7. Deliver stable typed project references with lifetimes
independent of agent transports, explicit semantic routing and full-editor
jump/return retaining each project's draft/history/view/presentation. Qualify
both-target shared ownership/refusals and ordinary observing Studio; stop on
any implicit request/job retargeting or user pair loss. Existing native Studio,
state/binding and wider renderer requirements remain. One logical protected-review
batch has completed across its interrupted checkpoint and qualification turns;
the full criterion remains open (no-closure count 2 after the project packet
below, with explicit reassessment). No preview-only or
temporary-only workflow earns project-switching acceptance. Preserve failed
fixture evidence. No production replacement is retried.

Concurrent-project candidate follows qualified/pushed protected checkpoint
2ade79e (exact remote proof is private). Its typed project registry/presentation
passes 215 checks natively and in executed pas2js, with zero native leaks.
It owns independent ordinary sessions, retains projects across
transport disconnect, bounds summaries/creation receipts and separates friendly
actor names from retry/removal-review authority. Shared core Call now accepts
an optional private request owner while retaining friendly activity captions.
Primary legacy calls retain their existing contract.

Candidate MCP adds nyx_workspaces and optional workspace scope to ordinary tools.
Immutable jobs capture both exact project and review references, mutually
exclusive. Editor exchanges resolve a fixed project; configuration remains a
global operator choice. Nyx-built Agents cards show projects/connections and
full-editor Jump into project. Browser navigation waits for acknowledged local
publications and stores per-project recovery/presentation before switching.
Typed portable presentation covers source/split/panels, search, new-state drafts,
caret and scroll; first browser compile caught missing declarations and the
fixture's chosen split range was corrected to the actual 10..90 contract.
Subsequent qualification below exercises real project switching, both actual
compilers, diagnostic currentness, selective rendering and desktop/narrow Studio.
Shared Nyx Agents controls run on browser and LCL. Trusted close and its private
editor route are implemented, with visible warning/cancel and server capability
guards; successful live confirmation and completion after closure are pending.
The complete concurrent/native authoring criterion remains open.

Owned project fixture PID 4972 serves editor 8278 / MCP 8279, using independent
build/review-workspaces/projects/stage and build/project-workspaces/web. Verify
build/project-workspaces/server/launch.json identity before stopping it. Source
server compiles with checked installed FPC; full browser Studio compiles. The
stage has no user enrollment and does not replace active Codex configuration.
Previous review/context services and production remain intact. Automatic approval
review refused launching the new close-route server ("blocked by policy", no
further reason); the combined command did not execute and no launch/configuration
was published. It was not retried through another route. The new close-server
binary is compiled, unlaunched. Existing PID 4972 still runs its earlier backend;
the updated browser therefore disables closure confirmation. Next acceptance
checks are live operator close/authentication/refusal, closed-context compiler
completion and broader presentation/native navigation. Preserve failed captures.
Do not infer these outcomes from compilation or shared view controls.

Interrupted review checkpoint: production remains revision 6, home selection/
view, no pending draft/Undo and ordinary test Redo retained; native MCP reads
authenticate. Owned stage PID 37008 (launch.json under build/review-workspaces/
server) serves editor 8258 / MCP 8259. Verify that identity before stopping it.
Portable review tests passed 200 native checks with zero leaks; the executed
browser fixture passes after restoring its missing rtl.run startup call. The
long-lived lifecycle receipt test reserves cleanup slots and retains old create
receipts rather than silently resurrecting retired work. Direct seed constructors
avoid the sample/claim replacement and empty reviews skip active pair export.
Qualification remains in progress: the real MCP/ordinary Studio fixture reaches
the pending Unicode draft and review composition, then refuses its nyx_select
call because the fixture omitted the advertised operationId. The fixture now
supplies that guard. Its error also reported the main revision for a review
refusal; candidate rejection metadata now resolves only the exact owned context
and omits a substitute revision for foreign/retired handles. The independently
compiled context-candidate server passes checked native compilation, but this
repair has not yet run through the real protocol journey. Client teardown retired
its reviews; the owned stage's user draft is
retained. No compiler jobs were requested by that failed journey. The source,
viewer, build routing and new lifecycle tool remain in progress and undeployed.
The branch checkpoint preserves this candidate and the new project-concurrency
requirements; it does not accept the review prerequisite or project switching.

Desktop reconnect recheck (2026-10-04): all fifteen native Nyx MCP handles are
available in this chat. Seven direct named reads succeed: session before/after,
bounded catalog search, page outline, selected-node properties/events, eight
accepted Pascal lines and three diagnostics. All responses agree at revision 6;
selection/view, page/component counts, pending draft and Undo/Redo state remain
unchanged. Agent activity advances from 89 to 95. No setup change, service restart
or document mutation is required. Semantic MCP remains primary; this connection
check neither qualifies every mutation tool nor accepts the review work below.

Current batch (2026-10-04): NS-4 workflow criterion 5's protected-review
prerequisite, following the required two-batch NS-2 reassessment. The previous
goal turn was progress: typed layout source/evidence was implemented, qualified,
published and independently verified at implementation 20b00f3 / handoff fbded37.
Deliver explicit owner-bound review contexts with independent ordinary Nyx Studio
documents/source/history, bounded semantic lifecycle and routing, immutable
context-correct compiler/preview work and an observing Nyx-built Studio view.
Acceptance evidence must preserve the active user's exact accepted pair, pending
draft/baseline, selection/view, revision and Undo/Redo; qualify foreign and retired
handles, permission reduction, exact retry receipts and grouped review history
on native/browser, real MCP, actual compilers and ordinary Studio. Stop and repair
on any active-work mutation or workspace/job fallback. No source qualification
claims deployment; keep production and the previous refusal boundary intact.
This batch is in progress, not accepted. Original criterion 5/state/binding/
reusable/general-source scope and the NS-2 return path remain unchanged.

Completed bounded batch (2026-10-04): typed layout policies under the existing
NS-2 LCL/parity owners. Public value/managed authoring, source admission and
generation, persistence, bounded MCP metadata and both actual adapters qualify
wrap/alignment/justification, content/fill sizing, natural caption widths,
shared spacers and definite root height. The staged MCP service composes both
maintained review pages in one 50-operation paired transaction. Original user
work stays outside it; both compiler files equal the bounded semantic export.

Latest policy evidence: 2,169 actual native checks with zero leaks; 2,214 desktop
and 2,215 actual-390 browser checks. Studio passes 25 desktop layout/source/
history checks and the 21-check 390/1100/800/390 resize journey. The catalog
property and bound-selection regressions pass. The packet below records limits,
current process identities, exact source and refusal evidence.

The proportional/hidden-flow and typed-policy batches advance implementation
without closing the original full renderer/parity criteria: consecutive count 2.
Reassessment now changes the next action to the existing NS-4 workflow criterion
5 prerequisite: a protected semantic review workspace that can be authored and
disposed without altering the active user's pair, selection, drafts or history.
Qualify ordinary observing Studio, revision/refusal behavior and both actual
compilers before accepting that prerequisite. Retain full NS-2 intrinsic,
scaling, accessibility and native Studio requirements and their return path.

Latest layout evidence: 2,084 actual native control/arithmetic checks, zero leaks;
2,094 desktop and 2,095 actual-390 browser checks. Native named MCP composes the
27-node review as one paired edit, inspects bounded context, exports source and
requests real view/application jobs on both targets. Both final application files
equal that export. Reviewed root cleanup retains imports as documented; two
paired Undo operations then restore the original full project bytes exactly.
Current revision 6, home selection/view, one page/component, no draft/Undo and
the two test operations on Redo. Native MCP is primary. See the packet below.

Latest qualification (2026-10-04): integrated property/projection concordance.
Semantic MCP authors all 76 kinds and a property review in seven paired groups;
exported source equals both actual compiler job files. Native qualifies 262
expanded faces / 14,127 checks with zero leaks; browser passes 14,269 desktop and
14,270 exact-390. Contextual typed admission passes 39 native/browser each,
real MCP 75, HTTP 177 and Studio 52/52. Measured preview viewport validation is
also repaired. The complete packet follows below. Original event
criterion 1 remains open at no-closure counter 8; 2/3/4 stay accepted, codegen
criterion 3 stays at 11 and the full goal stays active. No scope or counter reset.

Production PID 29656 remains at the existing firewall-authorized executable path
and LAN binding, serving the preceding focus/keyboard release. Automatic approval
review rejected the combined stop/install/restart action before execution, with
only "blocked by policy" as its reason. All six production artifacts and the exact
user pair remain unchanged; health is 200. The new candidate is qualified and
staged with backups and release-manifest.json, but is not installed. Fresh installed
Codex authenticates fifteen tools from another project after connection rotation.
Project/enrolled configuration refresh remains automatic. The restarted desktop
chat now exposes all fifteen native Nyx handles; direct authenticated bounded
reads are verified. Native semantic MCP is primary, with the Pascal semantic
client retained for isolated test services. The temporary owned review was added
and removed through MCP; the exact original paired project is restored. Redo
retains the ordinary test history; no protected review workspace is claimed.

Typed-policy implementation 20b00f37c897a9f0a583fdb5228785275fffb954 is pushed
to origin/hello-nyx and independently equals git ls-remote at publication. The
worktree is clean; ignored layout-policy-remote-proof.json records exact identity.
This handoff follows that implementation; no unqualified deployment is claimed.

Requested desktop reconnect check (2026-10-04): the active chat exposes all
fifteen `mcp__nyx_studio__nyx_*` tools. Seven native named calls succeed:
session before/after, bounded component search, page outline, typed selected-node
properties/events, eight accepted source lines and bounded diagnostics.
All document-bearing responses agree at revision 2. Selection/view, page and
component counts, draft/Undo/Redo state and permission remain unchanged; only
observable agent activity advances. No mutation, build, preview, configuration
change or service restart was needed. Earlier reconnect-pending observations
below are historical and superseded. This verifies the connection without
claiming execution of every mutation tool or new acceptance credit.

Completed integrated batch (declared 2026-10-04): NS-1_event-scheduler_01
criterion 1, at consecutive no-closure counter 6. Prior keyboard evidence
qualified a selected review page; it did not establish agreement between every
default control's published focus/keyboard events and its actual adapter face.
Reassessment changes the decision path to a complete catalog concordance journey,
MCP-authored and compiled unchanged on browser/LCL. Centralize the typed physical
keyboard classification, repair unreachable standard faces and disabled-policy
transitions, and exercise all catalog kinds and expanded reusable/compound parts
with ordered multiple callbacks and actual focus/key consumers. Retain bound
collection focus ownership and creator adapters. Evidence must identify every
kind and compare metadata with real target behavior; no unavailable grade may be
invented to make the check pass. Budget: one integrated implementation,
qualification and deployment checkpoint. Stop publication on unreachable
advertised faces, duplicate/stale events, lost bound focus or damaged user pairs.
Reuse applicable editing/gesture/extension/source evidence. Full keyboard/grid,
assistive technology, hardware/IME, other widgetsets, native Studio and production
performance retain their original owners; no original criterion or counter is
reset or narrowed. Codegen criterion 3 remains at counter 11; full goal active.

Requested setup check (2026-10-04): the accepted
NS-4_agent-workflows_01 installation criterion is reverified against the current
production service and installed Codex. Project/enrolled user blocks match;
explicit enrollment preserves both existing files byte for byte. Actual Codex
initialization from another project authenticates all fifteen tools. An isolated
MCP transaction composes a page/heading/memo/button, bounded queries inspect its
children/source and Boolean properties, and semantic jobs compile the same pair
with actual pas2js and FPC/LCL. One grouped update and one paired Undo restore
the exact submitted pair, confirmed by both job records at monotonic revision 4.
Evidence remains in ignored build/mcp-setup-check-20261004/. No production
document/history/configuration change or service restart occurred; the disposable
service was stopped only after both jobs completed. Production remains PID 35152.

The setup batch was declared before enrollment: one configuration/qualification
checkpoint, stopping on connection, revision or ownership mismatch, without new
product scope or acceptance credit. One fresh client call lost its socket
response while requesting the second compiler job. The service remained healthy;
after querying the first job, one deliberate exact-actor/operation/payload retry
resolved the native request and compilation succeeded. No automatic mutation
retry was added. The cause remains unqualified under the existing workflow owner.
This chat still needs a client reconnect for native tool handles; the Pascal MCP
client is active and primary now. Original event criterion 1 remains at counter
6 and codegen criterion 3 at counter 11. Return to the declared supported-control
work after this requested check; the full goal stays active.

Previous integrated delivery (2026-10-04): NS-4_agent-workflows_01 criterion 5's
root-cleanup prerequisite is qualified, accepted and deployed. The ordinary
Nyx UI and fifteenth MCP tool share one reviewed paired removal command.
Contracts pass 45 native/executed-browser checks each; real semantic composition,
observer/Undo and both compilers pass 26 desktop and 26 exact-390. Unchanged
compiled consumers pass five per target, with zero native leaks. Final MCP/HTTP
gates pass 65/177; Studio passes 52/52 and shared contracts 30/1537 on both
targets. Fresh installed Codex authenticates fifteen tools from another project.
See [the packet](#reviewed-root-cleanup--2026-10-04).

Only production PID 35152 / persistent exec 74922 remains, on the existing
firewall-authorized executable path, editor 0.0.0.0:8088 and loopback MCP:8089.
All disposable qualification pipelines/services are terminal. The exact user
pair, selection/view and permission were preserved; the private observation
confirmed no draft/history before restart. Installed and served bytes match the
six-artifact manifest. Enrolled project/global Codex blocks match; configuration
and credentials remain ignored. This live chat still needs one reconnect for
native named handles; the Pascal semantic client is active and primary now.

Next authorized action: return to NS-1_event-scheduler_01 criterion 1 at counter
6, with standards-based supported-control qualification on browser/LCL using
semantic authoring/builds and selective physical consumers. Codegen criterion 3
stays open at counter 11. Workflow criterion 5 still owns independent review
workspaces, semantic state/bindings and richer reusable authoring; general
imports/helpers remain open. No full criterion/goal completion is inferred.

Remote protection: implementation a5bad51b8d080f8f11fc44af1c798c3e67e8c091 is
pushed on origin/hello-nyx; independent git ls-remote verifies exact identity.
The final handoff record follows that protected implementation. Full goal active.

The integrated root batch was declared before implementation (2026-10-04):
NS-4_agent-workflows_01 criterion 5's
missing root-cleanup prerequisite. Deliver a strongly typed shared root removal
contract and reviewed semantic groups on the same Studio paired history, with
dependency refusals and explicit warnings that Pascal helpers are retained.
Qualify interface/raw ownership, empty documents, dependent reusable roots,
exact unrelated-root/source preservation, failed groups/drafts/revisions/retries,
ordinary observing Studio Undo and unchanged compiled browser/LCL consumers.
Budget: one integrated implementation/evidence/deployment checkpoint. Stop
publication on dangling references, wrong-root deletion or unrelated work loss.
This is a prerequisite for protected review lifecycle, not its completion;
independent review workspaces, state/bindings and rich reusable workflows remain
with the same owner. Return to the original event owner afterward. Event counter
6 and codegen counter 11 stay unchanged; full goal active.

Previous integrated delivery (2026-10-04): NS-4_agent-workflows_01 criterion 2's
bounded local callback-implementation prerequisite is implemented, qualified,
accepted and deployed. Typed immutable edits and bounded Unicode reads retain
signature/helper/managed-view ownership and exact paired history. Native and
executed-browser admission pass 72 each; all checked allocations are freed.
Real MCP/observer/compiler/host-input journeys pass 21 desktop and 21 exact-390.
Unchanged compiled browser/LCL consumers pass nine each, with zero native leaks.
Final gates pass 64 real MCP, 177 compiler service, shared 30/1537 per target and
Studio Events/source/history 52/52. Fresh installed Codex authenticates fourteen
tools. See [the packet](#semantic-handler-implementations--2026-10-04).

The integrated body batch was declared before implementation, with one delivery
budget and a publication stop on ambiguous ownership, wrong-source edits or pair
corruption. No gate was weakened. Existing semantic callback/build criteria 3/4
and event criteria 2/3/4 remain accepted. Original event criterion 1 stays open at
counter 6; codegen criterion 3 stays open at counter 11. No completion credit
transfers from those owners. Full goal active.

The handler delivery's then-next prerequisite was the integrated root-cleanup
batch now delivered above under the existing workflow owner, criterion 5. State/bindings,
rich reusable operations and general source/import/helper authoring remain open.
Then return to supported-control qualification under the original event owner.
Compiler cancellation/caching/global scheduling retain NS-5 ownership.

Historical handler-batch handoff: qualification pipelines and disposable services were terminal.
Only production PID 36172 / persistent exec 79421 remains on the existing
firewall-authorized executable path, editor 0.0.0.0:8088 and MCP loopback:8089.
Server SHA256:
D86D0F6528A3B7653A95861E3426D19A90672DB525CA2F98AE0F5C1588253B94.
Main JS: 4BD7F187E4536BC57EFBD3765503DAB5D6C63C9AFD461DBEF3D6BC208B468076.
Preview JS: 259E105DA2C8A87C8B2C29097EB8EEABA11CAC802740B0E8225E7FFC63AE70E6.
The exact six-artifact manifest and backups are under ignored build/handler-edits/.
Immediate private observation rechecked revision, full pair, selection/view,
permission and no draft/history before restart. The live paired design/source,
selection and view were restored byte for byte. Local/LAN health and installed/
served artifact hashes pass; enrolled Codex credentials refresh with no warning.

The running desktop chat needs one MCP reconnect for native named handles; the
Pascal semantic client is active now. Actual fresh Codex initialization from
another project sees all fourteen tools. Existing Studio tabs need refresh after
credential rotation. No dependency source, compiler setup or firewall was changed.
Private configuration, tokens, user pairs and machine profiles remain excluded.
Remote protection: delivery 0f454d4ab0507663fddd5e3a91afb8d7cb777629 is
pushed on origin/hello-nyx; independent git ls-remote verifies exact identity.
This handoff update follows the protected implementation. Full goal active.

## Previous checkpoint before compiler and MCP delivery

Previous checkpoint: complete-command profiling removed workspace JSON encoding/
parsing from in-memory history and candidate preparation. Immutable typed
checkpoints retain exact canonical design/source frames; current public document
mutations synchronize freshly and detached restoration precedes either owner
swap. Recovery wire and fresh full candidate verification remain intact. All 1538
shared native/executed-browser checks pass, including thirteen new lifetime,
mutation, rollback/redo/wire/count-retention cases. Actual Studio passes 60 native
and 64 desktop/64 exact-390 browser cases; all 5822172 focused allocations are
freed with zero leaks. All 147 HTTP and compiled consumers pass. Largest browser
history falls from 10501.5 to 1218 ms, visual edits from 7577.8 to 6337.9 ms.
Large-project responsiveness remains open under original codegen criterion 3,
at closure counter 10; no completion credit is claimed from partial work.

Previous accepted prerequisite: observable structured state has all four
original criteria and its task is in DONE. Typed authored bindings survive wire,
composition, paired history and compiled isolated/full consumers. Applications
automatically mount independent reusable scopes and retain values/selection
through navigation. Studio's Nyx-built Data/Bindings panels edit the public
contract. New evidence is 27 shared authoring checks, 25 native / 26 browser
automatic-control/Studio checks, six compiled checks per target, forty intended
type errors per compiler and 147 HTTP checks. Checked ownership frees all
22091995 allocated blocks. The existing 1490 shared checks and target journeys
remain green. Measured materialized-control costs retain their NS-3 owner.
Evidence and corrected service-clone failure are in
[the authored binding record](#authored-collection-bindings-and-studio--2026-10-03).
The user's creator-help request also has contextual Inspector help on both
targets; [the help record](#selected-component-intent-help--2026-10-03) contains
the latest shared and real-control evidence.
Typed structural/declaration source editing retains its previous accepted
compiled and actual-control evidence in
[the structural record](#typed-structural-source-and-isolated-builds--2026-10-03).
The original codegen criterion 3 remains open for broader editing UX and usable
large-document reconciliation. Criteria 1/2 stay accepted. No parent task or
complete-product goal is claimed complete.

Next selected work remains [codegen](TODO/NS-1_codegen_01.md) criterion 3. The
bounded whole-command delivery is finished and its closure counter is 10.
Reassessment stops further isolated timing experiments. Deliver an integrated
source-editing outcome: typed compiler file/line/column diagnostics for authored
companions, HTTP transport and Nyx-built Studio navigation to the exact accepted
source, with Unicode/current-source guards and both-target failure journeys.
Budget: one implementation/evidence delivery; stop on wrong-source navigation or
accepted-pair/draft corruption. Plain helper-error build logs are the present gap.
Large-project responsiveness retains its original synchronization outcome and
128/512/2048 preservation benchmark. Do not expand scalar grammar, reset the
counter, weaken/transfer criteria or manufacture closure from speedups. The
structured-state prerequisite remains complete; full Studio/service/delivery
outcomes retain their owners. Final artifacts/process state are below.
The collection closure counter resets to 0 because criterion 4 closes.

The live service is on the firewall-authorized executable path, bound to
0.0.0.0:8088. The current PID/session and verified build identities are recorded
in the latest delivery record below. User staging remains untouched.

## Prior source-work checkpoints

Accepted prerequisites: [portable model](TODO/DONE/NS-1_model_01.md),
[catalog/theme](TODO/DONE/NS-1_catalog-theme_01.md) and
[portable identity](TODO/DONE/NS-1_identity_01.md).
Accepted prerequisite: [version-1 persistence/scalar state](TODO/DONE/NS-1_persistence-state_01.md).
Current return task: [structural/declaration source synchronization](TODO/NS-1_codegen_01.md).
The user's intervening palette request is delivered: optional List/Grouped modes,
typed intent groups/labels, descriptive search, creator help and native/browser
consumers. Evidence and live-service details are recorded at the end of this file.
Managed-source preservation is delivered: deliberate specialized control/state
locals, comments and unchanged typed expressions survive visual edits. Stable
control identities and declaration slots protect construction/types; explicit
state-key migrations retain authored local names. Metadata is reconciled by
owner, preserving ordered stores without matching against control construction.
Whole-pair verification precedes publication; rejected visual commands retain
both accepted members and redo. Both actual adapters and compiled companions
have evidence below. Structural/declaration editing keeps its original criterion
and is the next integrated return step, alongside broader source UX/performance.
The completed bounded batch owns criterion 3's paired-file boundary: admit design and
accepted Pascal together, retain rejected/stale drafts and their original base,
replace browser recovery with one coherent packet, and connect Nyx-built file
controls to adjacent files saved by the Pascal service. Exercise both-target
session admission, actual browser file controls, native disk transactions and
HTTP optimistic conflicts. Stop if either member changes before complete pair
admission or if conflict resolution silently discards crafted source/drafts.
This advances the original criterion; structural/declaration editing and the
full native Studio controller remain open with their existing owners.
Typed key-down/up and input consumption now extend the delivered
[events and schedulers](TODO/NS-1_event-scheduler_01.md) contract through Studio,
both actual adapters and compiled companions. Event capabilities remain open;
the next implementation batch follows the codegen three-batch reassessment and
has delivered paired project files, recovery and conflicts, followed by managed
names/comments. The next source delivery must address structural/declaration
synchronization rather than accumulating expression cases.
Accepted user-prioritized prerequisite:
[specialized managed controls](TODO/DONE/NS-1_component-interfaces_01.md).
Return path: [readable generation/source synchronization](TODO/NS-1_codegen_01.md),
including the accepted companion in delegated builds for handwritten handlers.
The event runtime/scheduler now has accepted criterion-2 evidence: sequential,
async, deferred UI and explicit threaded policies; owned callback/execution
lifetimes; mutation/reentrancy; actual focus/click/change; view/child-work
cancellation. Persisted handler/policy descriptors, published schemas and Studio's
Nyx-built Properties / Events inspector are now integrated, including source
stubs/navigation and confirmed removal. Accepted companions compile through the
HTTP application/page/reusable path. Event criteria 2/4 are accepted; the task
remains open for capability/event-family integration. Codegen's compiler return
batch succeeded, with its broader criterion 3 and three-batch reassessment open.
Delivered: portable typed configuration, reference/default, binding, scalar-domain
and structured-extension source edits with explicit builder/application boundaries,
retained drafts, stale-edit protection, paired history and actual browser/LCL
consumers. The codegen return deliverable compiles the accepted companion through
delegated application and isolated-view builds. Structural/declaration edits and
broader source UX/performance remain. Structured state for
production data controls remains [separate required work](TODO/DONE/NS-1_state-collections_01.md).
Codegen criteria 1 and 2 are accepted for the version-1 contract. Criterion 3
remains open beyond the delivered declarative subset. Consecutive batches
without criterion closure: 5. Stop/switch if unsupported syntax is silently
accepted or a rejected/stale draft changes accepted document/source/history.
An earlier batch advanced criterion 3 with public typed domain builders and
nested extension construction/removal. Its evidence covers complete fixture
reconstruction, exact data, family/range/shape rejection, shared/native/browser
targets, real controls and paired history. Criterion 3 did not close: structural
control/declaration edits, deliberate managed names/comments and companion-code
build/file workflows remain. The required two-batch reassessment is below.
Stop/switch: rejected candidates must preserve the accepted document, target
controls and generated source. Broader source synchronization and indexing remain
separate outcomes.

Reassessment at two batches without closure: the admitted declarative subset now
covers configuration, state, bindings, domains and structured payloads. Additional
expression cases would not resolve the missing end-to-end build/file/name workflow.
Stop expanding grammar and finish an integrated compiler-companion delivery using
the existing workspace/generator/service. This materially changes the next action;
it does not reset the count or weaken the original criterion. Existing NS-4/NS-5
owners still retain full authoring and service acceptance, without extra credit.

The previous compiler return batch owned criterion 3's handwritten-build boundary: send accepted companion
Pascal beside the admitted design, preserve its frame during isolated-view builder
replacement, and compile it for both targets through Studio's optional output
workflow. Require real helper code, exact source bytes, application/page/reusable
builds, diagnostics, rejected/stale draft protection and confined jobs/arguments.
Budget: one integrated implementation/evidence batch, then reassess the remaining
criterion before further parser work. Stop/switch if compiled source differs from
accepted design meaning, a rejected draft replaces accepted code, or compiler
arguments/job paths escape existing service admission.

The user's latest review requires generated Pascal to feel crafted. Locals now
combine authored purpose and control type and retain names across unrelated
insertions. Typed observable defaults travel through persistence, clones,
Studio history, generated execution and isolated HTTP builds. Generated binding
blocks reuse named typed state references and avoid repeating an existing scalar
type suffix. Live scalar binding and Studio state/binding editors are integrated.
Structured extension data also generates typed readable construction blocks and
survives project/control history and isolated builds. Shared typed triggers,
scalar callback snapshots and typed part/recipe customization now bridge both
targets. Declared self/field/event domains now narrow binding choices, preserve
scalar meaning and produce typed source blocks; named factory parts generate
purposeful locals. Optional Pascal edits now synchronize Title/Configure blocks,
typed scalar defaults/reference initialization, fluent bindings/domains and
structured extension construction;
broader authoring and general editable-source synchronization remain open.

Return path: accepted identity → persistence/state and code generation → browser/LCL
slice → Studio/service integration. These boundaries are connected, but their
broader acceptance criteria remain open.

## Evidence collected 2026-10-03

Current source delivery: 65 initial, 51 state/binding and 65 domain/extension
checks extend the shared suite to 30 core + 796 composition/designer checks.
contract-source-generated.log records native emitted-source execution, including
a preserved handwritten Unicode helper, source-edited domains/exact decimals and
runtime consumers of an edited default/new binding. Twenty intended type-rejection
fixtures pass per compiler. contract-source-reconstruction.dom.html passes 30
compiled browser reconstruction checks; contract-source-shared.dom.html executes
all 826 shared checks under pas2js. contract-source-lcl.log passes 25 actual native
authoring checks, including preview memos constrained by source-defined choices
and rejected state-field edits, plus the existing native regressions.

contract-source-desktop.dom.html passes 36 actual browser authoring checks;
contract-source-phone.dom.html passes 36 in an exact 390-pixel child. Both exercise
source Apply/Undo/Redo, real domain-constrained memos, structured data and retained
invalid drafts/accepted controls. contract-source-journey.dom.html retains all 49
Studio/DOM journeys. contract-source-http.log passes all 57 accepted-design build
checks across ten browser/native scopes, including late/empty output profiles and
exact payload/source checks. These builds still omit workspace helpers.
contract-source-heap.log reports
53135709 allocations/frees and zero unfreed blocks. Heap counts establish ownership,
not a performance budget.

One browser fixture initially assumed the old default-row order after inserting
a new default first. Source acceptance and preview values already passed; matching
the editor row to the authored source order resolved that test failure. The failed
state-source-authoring-desktop/phone artifacts remain separate from the passing
*-fixed artifacts. The initial generated build's misplaced test local was corrected
before the accepted build. Browser/LCL source commands share the portable router;
full native Studio remains open. source-layout-accepted.dom.html retains the prior
exact compact/resize evidence; this batch changes no layout. Updated live assets
retain LAN binding; machine profiles and staged Athena metadata are preserved.
The new source-contract fixture initially omitted an existing authored fallback
from its Text choices. Full candidate admission correctly rejected it; including
both the fallback and bound default resolved that fixture. The full generated
fixture preserves imported descriptor order/spelling and nested Unicode/NUL data.

The bounded reader does not yet admit structural/declaration source edits or
arbitrary helper expressions, preserve authored locals/comments inside regenerated
builders, or feed preserved application helpers into HTTP builds. .nyx/.pas exports
remain separate; local accepted-source/draft recovery needs broader crash/conflict
evidence. These remain original codegen/source acceptance gaps. See the supported path in
[fluent API](docs/fluent-api.md#crafted-source-and-editable-configuration).

| Boundary | Executed result | Limits |
| --- | --- | --- |
| Bootstrap | hello-nyx from master; Athena adopted | User staged metadata preserved |
| Shared fixtures | FPC 3.2.0 and pas2js 3.3.1: 30 core + 796 composition/designer checks pass | Includes 181 source checks, declared domains, structured data, events, typed recipes/parts and exact integer/version guards; browser executed over LAN HTTP |
| Ownership | Current core/designer fixtures with heap tracing: 53135709 allocations/frees, zero unfreed blocks | Includes typed source constructor/replay/refusal paths, detached candidates, immutable payloads, subscriptions and history; native portable fixtures only |
| Browser UI | 49 DOM journeys: actions/editor/output/history, source editing/rejection, derivation/parts/styles and long Unicode runtime/editable identity | Programmatic input; user's phone independently confirms Studio loading |
| Live control bindings | 48 browser and 50 native actual control checks: declared payload/value identities, numeric choices and full-range integers, Unicode edits, mirrors, range rejection, compound actions, drafts, navigation, updater failures and owned-store remount | Programmatic target events; complete control/parity matrix and measured update performance remain |
| Native UI | Matched FPC 3.3.1/Lazarus 4.99: 74 catalog projections, derivation/parts/shared/compact Studio; keyboard/theme recovery and long identity lookup/events | Full native Studio controller and assistive-technology validation remain |
| Studio state/bindings/source | 36 desktop and 36 exact-390-pixel browser checks; 25 actual LCL authoring-control checks use the shared command router | Source-defined domains/exact data, visual/source defaults/bindings, live consumers, history and retained invalid drafts; full native controller remains |
| Generated Pascal | Native-emitted fixture executes on FPC and passes 30 pas2js reconstruction checks; preserved helper, edited domains/exact decimals, defaults/new binding and independent runtimes | General source parsing/synchronization and handwritten HTTP builds remain |
| HTTP builds | 57 checks over LAN: optional profiles and ten view/application scopes retain typed defaults and exact structured extensions; client/service source matches; rounded integer/version and nonzero underflow refused | Serial worker; hardening remains |
| LAN | Configurable interface; current listener on 0.0.0.0, machine-local persistence, admitted destination origins and Windows Private/LocalSubnet TCP rule | User ran administrator firewall helper and confirms phone loading; no personal address in tracked config |
| Responsive Studio | 25 desktop / 27 compact journeys, including memo focus/selection/scroll/edit/undo/redo; exact 390-pixel frame plus 21 real resize/draft checks pass; captures inspected | Includes focused canvas drafts crossing both layout modes; programmatic events, physical phone/keyboard confirmation remains independent |
| Strong typing | FPC and pas2js each reject twenty wrong-argument fixtures with intended diagnostics | Includes scalar domain/range/event-source arguments, event triggers/handlers/actions, part/kind/override and binding/extension families |
| Persistence/state | Typed defaults, immutable structured extensions, atomic batches, clone isolation, version-1 binding codec, Studio authoring/history and runtime stores are integrated | All original version-1 criteria accepted, including declared scalar domains; observable collections and broader property schemas remain with explicit owners |
| Optional startup | Launcher serves health and editor with empty tool hints and no compiler on PATH | Built Studio assets/service required; source bootstrap needs host tools |
| Build command | Prior all passed; latest core/browser/LCL/studio/http target checks pass separately | Verified Windows profiles; browser assets are live on the configured service |
| Visual | Five matching native/browser light/dark/narrow/instance/custom-palette captures inspected; 15 computed-style/layout checks per browser variant and actual native theme/focus pixels pass | Browser narrow host measured at 390 pixels despite Edge's larger minimum viewport; scaling/full catalog visuals remain |
| Catalog reference | Pascal generator emits defaults/constraints/named paths/events and typed scalar field contracts for all 75 kinds | Root projection status is not whole-family or property parity |

Ignored build/ contains logs, screenshots and target artifacts. Private profiles
are confined to ignored .local/toolchain.json and .local/studio-outputs.nyx. Public setup:
[docs/building.md](docs/building.md).
Historical declared-domain results: domains-shared-accepted.dom.html reports 645 shared checks;
domains-reconstruction-accepted.dom.html reports 23 compiled reconstruction checks.
domains-controls-accepted.dom.html reports 48 browser binding checks;
domains-lcl-final.log reports 50 native binding checks, 10 native Studio authoring
checks and the existing native regressions. domains-authoring-desktop.dom.html
passes 23 checks at width 1416; domains-authoring-phone-accepted.dom.html passes
23 in its exact 390-pixel child. domains-journey.dom.html passes 41 journeys;
domains-layout-phone.dom.html passes 21 resize checks after 27 compact journeys.
The earlier desktop layout's 25 checks remain applicable evidence. Current source
and behavior changes preserve that layout; no new visual acceptance is claimed.

domains-generated-final.log proves native reconstruction and twenty intended
type rejections per compiler. domains-http-accepted.log passes 57 checks across
ten scopes, including complete declared contracts and exact imported spelling.
domains-studio-final.log rebuilds the live service/assets; listener is 0.0.0.0.
domains-heap-accepted.log reports 26185443 allocations/frees and zero unfreed
blocks. The initial uncached expanded fixture allocated 66126077 blocks;
read caching reduced churn substantially. Dedicated large-document budgets remain
open; these full-suite counts are ownership evidence, not a performance SLA.
Native/browser visual variants retain their earlier inspected artifacts.

## Implemented

- Owned document/node tree, pages, reusable definitions and independent realization.
- Version 1 persistence and deterministic Pascal generation.
- Document/node-owned immutable structured extensions preserve unknown fields,
  exact Unicode/NUL, decimal spelling and ordering. Scope protects standard fields;
  whole-candidate byte/depth/member admission preserves accepted data and history.
  Reusable/part overlays replace whole values independently. Readable generated
  constructors, ordinary Studio edits/undo and all HTTP scopes retain this data.
- Complete exports obey import budgets; integer defaults and the version tag use
  integer wire spellings instead of rounded Double admission.
- Typed scalar state, atomic revisioned batches and caller-owned subscriptions;
  complete read-only candidate validation precedes publication. Clone copies
  data without listeners; rejected updates preserve values/order/revision.
- Authored defaults survive document/view clones, source compilation, HTTP scopes
  and Studio undo/redo. Default commands admit a detached document before history.
- Nyx-built State and Bindings editors use shared runtime target/type metadata
  and a portable command router. Typed create/edit/rename/remove commands preserve
  exact values, order and references; clear/inherit retains reusable isolation.
  Rejected/no-op commands retain the accepted document and redo history. New-default
  drafts survive panel/viewport changes; escaped text supports exact NUL defaults.
- Immutable typed control bindings with explicit one-way/two-way value semantics,
  inherited binding replacement/clear and application-owned runtime stores that
  survive navigation. Unmounted pages validate proposed snapshots too.
- Shared binding commands stage independent projections before state publication.
  Rejected edits restore the same controls; committed notification failures are
  distinguished by typed error status. Custom factories supply mounted updaters
  whose ownership survives later registry changes.
- Shared enum triggers and action dispatch feed one browser/LCL typed handler.
  Callbacks own exact scalar values and source/origin/target IDs; copies survive
  later edits, navigation and destruction. Payload admission precedes publication.
  Designer triggers carry text drafts without runtime actions. Ancestor permissions,
  read-only targets and malformed integer steps are checked before mutation.
- Typed part references, enum override modes and kind-reference recipe registration
  share existing ownership/admission; omitted modes retain existing operations.
- Updates retain control/node identity, same-value text and unrelated field drafts.
  Native numeric text commits on editing completion; explicit renderer-owned
  store reuse survives full remount. Runtime multiline text uses LF while exact
  authored/default data survives; lossy NUL/single-line control text is refused.
- Strict portable JSON decoding into standard fpjson containers preserves NUL
  and Unicode, rejects decoded duplicates and shares UTF-8/depth/member budgets.
  Numeric defaults preserve Double precision without global locale changes;
  integer bounds, overflow and nonzero underflow are explicit.
- Typed built-in kinds and node-owned fluent configuration: layout/action/style/
  projection enums, Boolean/integer arguments, distinct open references and an
  explicit extension boundary. Default catalog recipes/sample use that API.
- Crafted generated locals combine purpose and control type, avoid unnecessary
  type repetition, and remain stable across unrelated same-kind insertions.
  Named typed state references initialize once and are reused by fluent defaults
  and binding blocks. Existing scalar/type-state suffixes avoid repetition. Output
  retains deterministic naming and safe ownership across client/service generation.
- Explicit UTF-8 text contract and owned text storage; no global codepage change.
- Portable 128-scalar authored ID admission; immutable escaped runtime keys,
  original template IDs/editable owners, explicit renderer lookup and bounded
  Studio chrome IDs carrying complete identities as command data.
- Isolated authored view cloning retains source IDs and only reachable definitions;
  maximum-length Unicode views build without preview-prefix overflow.
- Candidate rendering preserves accepted views when custom factories fail.
- 40 primitive/layout/authoring kinds, 35 compound recipes, named part paths, recipe
  derivation and custom browser/native factories.
- Shared primitive schema, typed property admission, inherited reusable defaults,
  projection capabilities and exact/base factory precedence. Unknown kinds fail
  explicitly; unknown extension properties remain preserved.
- Per-instance named part overrides: property edits, append/prepend, replacement,
  removal and nested reusable paths; independent payload ownership and design IDs.
- Nyx-built Studio part customization and primitive/reusable insertion, with history/source
  and target control evidence. Pascal-generated default catalog reference.
- Validated canonical RGB themes, separate surface/control radii and scoped
  browser palettes. Public themed Lazarus buttons/surfaces/field frames retain
  LCL interaction and editing. Native InputFor borrows editable controls without
  relying on frame child order. Native caption rows avoid measured text clipping.
- Shared compound stepping, clearing, selection, toggle/dismissal and semantic
  events; runtime browser updates retain target identity.
- Shared Pascal Nyx Studio shell, consumed by browser and native harnesses.
- Public code-editor/design-surface controls and identity-based host access.
- Studio browser controller: palette/search, canvas selection, hierarchy,
  inspector, optional generated-source split view, page/reusable editing,
  bounded history, local recovery and design/source export.
- FPC HTTP service: configurable loopback/LAN binding and destination-origin
  admission, confined jobs, controlled compiler
  arguments, time/log budgets and source/artifact URLs.
- Optional Nyx-built Target / output section, late browser/native selection and
  machine-local profiles; dirty settings apply automatically before a build.
- Neutral starter content and undoable project naming; no branch identity in defaults.
- Shared compact Studio panel compositions, optional source, dynamic browser
  viewport height, confined preview scrolling and focused-draft resize recovery.
- Canvas selection retains controls and scroll; ordinary design fields edit
  through undoable commands. Named inherited fields create independent instance
  overrides, preserving templates/siblings. Desktop chrome refresh moves the
  existing view through the public renderer host API.
- Focused canvas drafts survive both layout-mode transitions. Delayed output
  profile responses preserve focused shell drafts and mounted preview fields,
  instead of overwriting pending input during a tooling-only update.
- Generated browser/native application hosts support page navigation.

## Remaining

The [task catalog](TODO/README.md) owns the full scope. Concrete gaps:

- Advanced recipes need production behavior: drag/drop Kanban, overlays/dialogs,
  structured trees/tables, virtualization, filtering/sorting and picker parity.
- Native date/time/color currently use text projections; tabs and several
  compounds need complete behavior/capability parity.
- Complete property-level capability coverage and broader event-family schemas.
- Large-document indexing, metadata/history/update costs and byte-symmetric
  history limits still require measured contracts and implementation.
- [Observable structured state](TODO/DONE/NS-1_state-collections_01.md), arbitrary
  extension property schemas and broader trigger contracts remain. Declared
  scalar compound domains are integrated; generic containers no longer invent
  a self value or admit all scalar binding kinds. Scalar default kind changes
  are not yet an editor workflow.
- Accessibility, native scaling, rounded child clipping, widgetset chrome and
  broader component/visual validation.
- Full Pascal synchronization beyond Title/Configure/defaults/bindings, retained
  managed names/comments, drag/drop, resizing/snapping, project/assets/themes and
  native Studio remain.
- Adjacent project persistence, worker queue/cache/cancel, structured diagnostic
  locations and reliable reload remain.
- Per-project output choices/profiles and richer build options remain; the current
  choice is browser-local and compiler profiles are machine-local.
- Legacy migration/compatibility, clean CI pins, Unix/macOS checks, packaging
  and independent adoption remain.

## Checkpoint and next action

User steering prioritizes [specialized managed component contracts](TODO/DONE/NS-1_component-interfaces_01.md), now accepted.
The subsequent inspector/event request is owned by
[typed events and scheduler](TODO/NS-1_event-scheduler_01.md) and the expanded
original Studio authoring criteria: complete Properties / Events tabs, source
stubs/navigation, visible multiple registrations, confirmed removal and per-event
execution policies. The managed component prerequisite is accepted. Continue the
event contract/Studio workflow and the compiler-companion return path.
Scooty's progress drawings use characters/scenes as requested.
The generator now uses specialized managed declarations/factories across the
complete catalog. Interchangeable implementations, descendants/facades surviving
owner disposal and both-target generated recovery have accepted evidence.
The compiler-companion return path now executes authored helpers/callbacks through
delegated application/page/reusable builds on both targets. Event descriptor and
inspector evidence is recorded in the final delivery below. No overall completion
credit changes; complete event capabilities and native Studio remain open.

The model, catalog/theme, identity and version-1 persistence/scalar-state tasks
are accepted. All three original state criteria are closed by maintained code,
Studio, shared fixtures and actual browser/LCL consumers. Percentages and task
credits remain unallocated; the full production outcome is not complete.

An earlier batch delivered typed scalar-domain declarations and structured-extension
construction in the optional Pascal workspace. Its 65 new portable checks plus
actual browser/LCL consumers and compiled native/pas2js execution establish the
subset's atomic admission, exact values, history and failure behavior. Shared
fixtures pass 826 checks; reconstruction passes 30 browser checks; authoring passes
36 desktop/36 exact-390-pixel browser and 25 native-control checks. Native heap
tracing reports zero unfreed blocks. Codegen criteria 1 and 2 remain accepted;
criterion 3 remains open. Consecutive batches without criterion closure: 2.

The compiler return deliverable for criterion 3 of
[NS-1_codegen_01](TODO/NS-1_codegen_01.md) is fulfilled. Its three-batch reassessment
switches the next codegen action to atomic paired files/recovery/conflicts. The
current event action is one keyboard family with native/browser consumers.
Structural/declaration edits, managed local/comment preservation
and paired-file conflicts remain original acceptance gaps. No task/DONE/credit or
overall-completion claim is made from this partial delivery. Unsupported code must
preserve accepted design and the user's draft; resolve an admission leak before
adding more grammar.
Structured collections have an explicit open owner and are a prerequisite for
production data-control families. Complete Studio, component breadth, semantic
parity and measured performance retain their existing owners. Preserve UTF-8,
candidate recovery, solo execution and optional-output startup.

## Managed interfaces accepted / events batch — 2026-10-03

All original NS-1_component-interfaces_01 criteria are accepted and its task is
in DONE. Commands: build.ps1 -Target generated / lcl / http, checked -gh core
build, and Edge execution of controls/tests/generated/journey/authoring hosts.
Evidence under ignored build/: interfaces-generated.log, interfaces-lcl.log,
interfaces-http.log, interfaces-heap/run.log; managed-final, shared-final,
generated-final, journey-specialized, authoring-final and authoring390-final
DOM artifacts. Results: shared 30 + 980; managed 184; generated browser 32;
24 intended type failures/compiler; ten actual managed checks/adapter; browser
journey 49; desktop/390 authoring 36/36; native authoring 25; HTTP 57.
Heap: 109611743 allocations/frees, zero unfreed blocks. The alternate object is
a real independent implementation, not interface delegation to a default class.
Retained authored/runtime descendants and rejected-factory roots have meaningful
lifetime checks. LAN service remains on the existing firewall-authorized path.

Next batch owns NS-1_event-scheduler_01 criteria 1–3: deliver retained multiple
callback registrations, immutable event snapshots and sequential/deferred/real
native worker scheduling, with actual focus/click/change adapter consumers.
Verify cancellation, failed callbacks, dispatch mutation, reentrancy and view
disposal on both targets. Stop if queued work borrows UI/model state, unsupported
threads masquerade as native workers, or a callback can run after cancellation.
Persistent handler references and complete schema/Studio Properties–Events tabs
remain required integration work, with accepted-companion compilation as the
explicit dependency for executing authored handlers. Event task stays open
until all original criteria pass; no completion percentage or credit added.

## Callback inspector and compiler companions — 2026-10-03

Maintained delivery: nyx.callbacks typed handler/registration references and owned
fluent descriptors; nyx.schema complete shared attributes and immutable custom
schema publication; nyx.source callback admission and ordinary commented Pascal
stubs; nyx.studio.inspector Properties / Events workflow and exact removal
warnings. Studio and native harness consume public Nyx controls, not a second UI
layer. Add/policy/remove share source/design history and preserve pending drafts.
Definitions, sibling instances and named overrides retain independent callbacks.
The complete shared enum fields stay closed even on leaf controls.

nyx.studio.builds carries the accepted design/companion envelope. The server admits
source meaning/namespace/directives before a confined job, compiles exact full
applications and preserves frames in isolated pages/reusables. The frontend
refuses pending/rejected drafts and stale results. Browser worker-only policy
fails before job allocation. Actual source/log/artifact URLs remain available.
Both application hosts bind authored descriptors once, retaining registrations
through navigation. Classes resolve to fresh reference-counted callbacks.

Evidence under ignored build/: inspector-generated-final.log (30 + 1051 shared
native checks, native reconstructed companions and 28 intended type failures per
compiler); callback-native/compile.log and run.log (69 checks, 2850074 allocations/
frees, zero unfreed blocks); inspector-lcl-final.log (30 native authoring, 50 live
binding, 14 runtime event/control and 14 compiled callback/control checks, plus
74 catalog projections). Browser shared/authoring/390/callback/compiled-control/
journey/reconstruction DOM artifacts record executed results. HTTP final log
passes 96 checks; both targets compile application/page/reusable companions, and
native artifacts execute handwritten Unicode helpers and authored callbacks.
Browser artifacts expose the executed helper and callback totals 11/11/100.
Current Studio authoring is 45 desktop / 45 exact-390-pixel browser; compiled
callback controls are 8 browser / 14 native; shared total 1081 and compiled
browser reconstruction 32. Final runs follow the enum/schema delivery.

Final browser execution confirms accepted companion callbacks at application,
page and reusable scope (11/11/100), and the handwritten helper retains its exact
supplementary Unicode text. inspector-phone.png shows the passing 390-pixel
inspector fixture. Source navigation is token-based: comments/string literals
cannot impersonate a handler declaration, and reformatted qualified signatures
retain the correct source line; the 69-check suite includes that regression.
All 39 reviewed documentation files have existing local link targets. The final
LAN HTTP request returns 200 and the listener remains on 0.0.0.0:8088.

The broadened click bridge exposed LCL's click-on-change checkbox behavior.
inspector-lcl-diagnostic.log records state=False with an unexpected callback delta
of 2. General managed clicks are now opt-in while the existing global method
bridge retains command/named clicks. Browser clicks stop at their nearest Nyx
control. Labels and actual framed memo inputs execute authored callbacks; native
custom click navigation is checked before reading a disposed binding. The original
Boolean binding test and all native journeys pass, retaining the expected single
legacy value-change event. Ownership-related unused-variable hints are intentional;
existing compiler string-conversion warnings remain distinct from passing checks.

Acceptance: event criteria 2/4 accepted; 1/3 remain open for complete capabilities
and additional event/extension families. No event/Studio/full-product DONE claim.
The required compiler-companion return deliverable succeeded. Codegen criterion 3
remains open and its consecutive count is 3; the next codegen delivery switches to
atomic paired project files/recovery/conflicts, not more expression grammar.
The next event deliverable is one complete keyboard family with actual native/
browser consumers. Preserve original source naming/comment/structural scope and
production component/state/parity/performance owners. Stop on duplicate dispatch,
stale borrowed UI work, silent unsupported capability or partial pair admission.

The LAN server remains bound to all local interfaces at port 8088 on its original
firewall-authorized executable path. Private profiles stay in .local; no tools were
installed, no subagents used, and staged .gitmodules/athena entries were preserved.

## Keyboard callbacks and input consumption — 2026-10-03

Delivered typed key-down/up through runtime and authored fluent interfaces,
persisted descriptors/contracts, source admission/generation, Studio's Events
tab and both adapters. TNyxKeyStroke owns the key/modifier/repeat snapshot; exact
Matches rejects repeats by default and distinguishes AltGraph. Unsupported key
names remain unknown. Composition/process keys bypass shortcut handling.

INyxEventResponse is an owned invocation context, with no callback/model/widget
back-reference. Only active sequential UI input invocations may Consume; return,
cancellation and ordinary/queued/threaded execution refuse that operation.
Consumption remains visible to ordered siblings and failures stay isolated.
Native repeat state clears on release/focus loss. Navigation cancels stale input
defaults and remaining callbacks. Browser text drafts pass existing typed change
admission before key capture, matching native accepted text without requiring blur.
Numeric drafts retain their editing-complete boundary.

The initial native CN Space regression failed: TCDButtonControl activates on
key-up before its callback and regardless of a consumed key-down. The Nyx button
now tracks admitted activation keys and invokes key-up before the reused Lazarus
state/Click operations. Both consumed edges pass; original unhandled Space/Enter
activation still passes. The current-text fixture initially failed with an
unshown native form; a shown actual memo supplies the real OnChange boundary.
Its diagnostic includes payload/model values and the strict assertion now passes.

Evidence: keyboard-generated-final.log passes 30 core + 1061 composition/designer
checks (1091 shared) and 29 intended type failures per compiler. Native scheduler
passes 55; keyboard-scheduler.dom.html executes 52 browser checks, including real
queued/asynchronous Consume failures. keyboard-native/run.log passes 79 authoring
checks with 4868425 allocations/frees and zero unfreed blocks; scheduler-run.log
passes 55 with 713 allocations/frees and zero unfreed blocks. Browser callback
suite passes 79. keyboard-lcl-final.log passes 35 actual event/control, 33 native
Studio authoring, 50 live binding and 17 compiled callback/control checks, plus
74 catalog projections and existing accessibility/activation journeys. Browser
journey passes the same 35 event/control checks and 49 Studio interactions;
compiled callbacks pass 11, including handwritten Ctrl+Enter, repeat refusal and
key release. Actual Studio authoring passes 49 desktop / 49 exact-390-pixel checks.
keyboard-http.log passes 96 with accepted keyboard-containing companions at
application/page/reusable scope on both targets. Catalog generation refreshed
the reviewed 75-kind complete shared-property reference.

Event criteria 2/4 remain accepted; full 1/3 capabilities and remaining families
remain open. This batch does not close another criterion; consecutive count is 1.
Codegen's count stays 3, and the next batch implements paired project files/
recovery/conflicts rather than expanding expression cases. Preserve all original
criteria, production component/structured-state prerequisites and native Studio
scope. The existing firewall-authorized server executable was rebuilt and kept
bound to 0.0.0.0:8088; no private profiles, staging, dependencies or staffing changed.
Final delegated browser artifacts execute their callbacks (11/11/100 at
application/page/reusable scope). Maintained-code license/dialect checks pass;
all 21 changed Pascal files pass blank-above-if inspection after five whitespace
corrections. All 39 documentation files have existing local link targets. The
final LAN request returns HTTP 200; staged .gitmodules/athena entries remain intact.

### Paired projects, files and coherent recovery — 2026-10-03

Delivered the bounded NS-1_codegen_01 criterion-3 paired-file return path, with
NS-4 consuming the same public contract. nyx.studio.projects admits strict
versioned packets and deliberate Pascal/design resolutions on detached document/
workspace owners. Session.LoadProject publishes only after whole-pair admission,
retains exact pending/base data and supports empty/reusable-only projects.
Drafts can be empty, unsupported or stale. Independent conflicting Pascal buffers
are retained for manual merge rather than overwriting one.

The native project store saves adjacent design.nyx, the actual Pascal unit filename
and project.nyxproject under a host-owned project root. Complete flushed journals
roll interrupted saves forward before reads; incomplete .next files are ignored
and prior packets remain. Content revisions cover exact UTF-8 file bytes and draft
metadata. External edits are returned for explicit admission; stale creates/saves
return the complete remote packet without touching either accepted file. This is
coherent repository/service observation and process-interruption recovery, not an
atomic multi-file filesystem rename or a power-loss guarantee.

Nyx-built file controls now use real File/FileReader imports and HTTP save/open,
including portable backup export, accepted adjacent downloads, explicit remote/
copy decisions and backups before replacement. Output targets remain optional.
Browser recovery writes one bounded wrapper, retains draft base/unresolved input,
stages legacy keys before publication and backs up corrupt wrappers. Over-budget
recovery preserves the last readable packet. Paired import invalidates canvas
retention even when both projects reuse the same page IDs. Compact recovery
conflicts select the visible Project panel.

Corrected failures: older Web RTL uses attribute/style APIs rather than type_/
style.display; pas2js requires a local loop variable and an externalclass mode
directive. The initial test used a textarea descendant for a textarea control;
an asynchronous step-local fixture also lost its value across phases. Both test
errors were fixed. Version rejection initially matched a missing JSON needle;
it now constructs an invalid-version packet directly for both formatters. Phone tests now follow
visible panel tabs. Review found and fixed stale retained canvases and a zero-page
publication index. Explicit supplementary Unicode/NUL and helper source comparisons
remain strict. No compiler/dependency install, profile changes or subagents.

Final evidence: project-native-final-run.log passes 60 admission/file/recovery
checks with 26884592 allocations/frees and zero unfreed blocks. The shared browser
project-verified-shared.dom.html executes 39. project-verified-desktop.dom.html
and project-verified-phone.dom.html pass 21 each; phone reports exact width 390.
File and recovery tests use actual browser controls/API objects and the live
Pascal repository, restoring the user's storage values. project-lcl-run.log passes
37 actual native authoring, 35 event/control, 50 binding and 10 managed-control
checks plus 74 catalog projections and existing accessibility/activation journeys.
project-generated.log passes 30 + 1061 shared checks, 55 native scheduler checks,
native generated reconstruction and 29 intended type rejections per compiler;
its project count was 56 before additional empty/reusable cases (final 60/39 above).
project-regression-core.dom.html executes those 1091 shared cases; journey passes
49 Studio + 35 event + 10 managed checks. Authoring desktop/phone remain 49/49.
project-http.log passes 111, including 15 new paired-file protocol cases and all
accepted-companion app/page/reusable compiler cases; native artifacts execute
their helpers/callbacks. Current Studio browser compilation passes.

Codegen criterion 3 remains open; count without full criterion closure is 4.
The required paired-file return delivery ends successfully. Next source work must
retain deliberate managed names/comments across visual regeneration and actual
both-target consumers. Structural/declaration editing and broader source UX keep
their original scope; full Studio/assets/WYSIWYG/native controller and production
components remain with their owners. No task moved to DONE and no completion
credit/full goal completion is claimed. Service remains on the firewall-authorized
executable path and bound to 0.0.0.0:8088; user staging is preserved.
Final review passes git diff --check, local documentation link targets and all
10 changed Pascal files' MIT/Delphi/UTF-8/blank-above-if checks. Branch remains
hello-nyx; staged .gitmodules/athena entries are unchanged. LAN Studio returns
HTTP 200. The latest persistent service session is 31734 (PID 416); its original
firewall-authorized executable path is unchanged. Phone evidence is an executed
390-pixel browser frame, not a newly reconfirmed physical-device review.

## Optional component discovery and creator help — 2026-10-03

User steering owns this bounded delivery under NS-4 component discovery and
NS-3 creator metadata. Default full-list mode remains; Grouped gives each entry
one primary intent home. Typed group filters and multiword AND search combine
names, titles, labels, aliases and high-level descriptions. All 75 default kinds
have descriptions. Custom kinds/recipes use the public fluent Describe method
with enums/typed label sets plus descriptive user text. Metadata copies remain
independent of the registry and runtime trees on both targets.

The shared Nyx palette composition/router provides counts, omitted empty groups,
empty-result reset and full-width Details help. Actual browser title/native Hint
receives the same descriptions. Touch controls have usable height. Presentation
preferences recover separately from project/source/output packets; search is
transient. Discovery preserves history, exact pending drafts and desktop mounted
preview controls. The regenerated catalog reference includes groups, labels and
descriptions; the composition guide documents creator registration.

Evidence: palette-final-core-run.log passes 30 core + 1162 composition/designer
checks (1192 total, including 101 discovery cases). The same suite executes in
palette-final-shared.dom.html. palette-generated.log also passes reconstruction,
60 project cases, 55 native scheduler cases and 29 intended type failures per
compiler. Its native shared count is 1191 before the final metadata-copy case;
the final native/browser runs contain that case. palette-shared-run.log records
178988418 allocations/frees and zero unfreed blocks for the first 100 discovery
cases integrated in the complete shared suite.

palette-final-desktop.dom.html passes 23 real Studio interactions; the phone
counterpart passes 22 with its actual frame width reported as 390. The desktop
extra check proves mounted preview identity. Both cover visible help width,
group/query intersection, preference recovery, actual insertion and unchanged
undo/source recovery. palette-lcl-run.log passes 45 native authoring checks,
including eight palette actions through actual inputs/buttons and the shared
router; the other 74 catalog projections, 35 event and 50 binding checks pass.
palette-phone-review.png is a reviewed real Studio frame with full-width help.
palette-http.log passes 111 checks against the refreshed LAN service, including
accepted handwritten companion compilation/execution at both targets/scopes.

The live service remains on its original firewall-authorized executable path,
bound to 0.0.0.0:8088; latest persistent session 15771, PID 13444. Browser Studio
and service are rebuilt. LAN HTTP returns 200. No stage/commit changes: the user's
.gitmodules/athena staging remains unchanged on hello-nyx. Browser frame evidence
does not claim a newly repeated physical-phone review or full native Studio.

Return next to managed-source reconciliation with names/comments and atomic
visual publication. Preliminary identity-reader work deliberately rejects control
renames until that preservation path exists. Structural/declaration editing,
production controls, assets, full WYSIWYG and the native Studio controller retain
their original owners. Codegen criterion 3 stays open; its consecutive-batch
count stays 4. No parent task moves to DONE or earns complete-product credit.
Final review passes all 12 touched Pascal files' license/Delphi/UTF-8/blank-above-if
checks, local documentation link targets and git diff --check. The reviewed frame
shows readable full-width descriptions with no horizontal overflow. Live LAN
HTTP remains 200 after the 111-case service regression; user staging is unchanged.

## Managed source preservation — 2026-10-03

Delivered the next integrated return from NS-1_codegen_01. Deliberate specialized
control/state locals now admit against stable construction identities and typed
declaration slots. Visual edits preserve those names, comments and unchanged
constant expressions, including nested structured values. New controls reserve
authored names before choosing defaults; explicit state-key migration retains
the local. Deleted controls retain their notes. Both interfaces and constructors
remain typed and protected. A public session AddControl borrows a managed control
and inserts an independent descriptor clone through the existing command/history.

The private implementation uses the admitted lexer/expression reader and bounded
Myers comparisons, with a linear untouched-generator path. Metadata statements
are partitioned by stable owner, preventing moved contracts/callbacks from
matching against unrelated construction. Unchanged groups keep exact sites;
changed groups may gather at their first site. Verification reconstructs the
whole candidate before source publication. Source Apply transfers its already-
admitted companion directly. Visual commands stage or roll back both members;
undo trimming follows successful publication. A real source-size failure proves
accepted design/source and redo survive. Full builds retain exact companion
bytes; isolated builders reconcile and verify their reduced design.

Final evidence:

- `build/managed-final-core-run.log`: 30 + 1195 = 1225 shared checks. The executed
  `build/managed-shared.dom.html` records the same native/pas2js counts, including
  33 focused managed-source cases. These cover deliberate names, namespace/type
  rejection, collisions, exact history/recovery, nested expression retention,
  callbacks after moved contracts, independent managed insertion, empty open
  keys, semicolon literals, isolation, deletion notes and failed publication.
- `build/managed-reconstruction-run.log` executes the real crafted companion on
  FPC; `build/managed-generated.dom.html` executes it with pas2js (34 reconstruction
  checks). `build/managed-native/nyx.managed.view.pas` contains actual deliberate
  specialized locals, Unicode comments, unchanged expressions, a reusable
  definition and an ordinary Pascal callback TODO class/registration.
- `build/managed-authoring-desktop.dom.html`: 54 actual Studio checks, viewport
  1416. `build/managed-authoring-phone.dom.html`: 54 in the exact 390-pixel iframe.
  Actual Apply/property/undo/redo/rejection joins the existing event/state journey.
  `build/managed-lcl-run.log`: 49 real native authoring checks, plus existing
  managed controls, 35 events, 50 bindings, 74 catalog projections and native
  preview/identity/theme journeys. Source/property commands share public routing.
- `build/managed-heap-run.log`: 33 checks; 20612163 allocations/frees and zero
  unfreed blocks. `build/managed-final-generated.log` retains 60 project checks,
  55 scheduler checks and 29 intentional type rejections per compiler. The final
  formatting pass also passes full shared admission and compiled reconstruction.
- `build/managed-journey.dom.html`: all 49 DOM interactions. The refreshed service
  passes all 111 checks in `build/managed-http.log`, including full/page/reusable
  companions and executed native helpers/callbacks. Local output profiles are
  restored by the harness. All 12 touched Pascal files pass MIT/dialect/UTF-8/
  blank-above-if checks; git diff --check and local documentation links pass.

Corrected failures: the fixture's native ANSI StringReplace changed Unicode during
renaming; exact portable substitution fixes the fixture. New metadata inserted
before a moved authored contract changed extension order; whole-frame callback
updates could match ownership statements. Owner partitions resolve both. The
first scalar declaration after var was inconsistently protected during companion
comparison; it now follows the same typed comparison boundary as later locals.
Source Apply was reconciling an unused intermediate before replacing it with its
already-admitted source; atomic pair publication removes that duplicate path.
The size-failure fixture leaves sufficient packet headroom so it exercises source
rollback rather than failing earlier while checkpointing. Standalone GUI redirects
returned before Edge wrote output; piped invocations supplied authoritative
terminal evidence. These failures are retained in preliminary/trace logs and
are not accepted results.

Live service: PID 30304, persistent exec session 13127, approved executable
`build/native/3.2.0/i386-win32/nyx_studio_server.exe`, listening on 0.0.0.0:8088.
The isolated verified service build is `build/managed-server/`; the previous live
binary is backed up at `build/nyx-studio-before-managed-source.exe`. LAN HTTP is
200. Physical phone reachability was established previously; this batch's new
mobile evidence is the exact-width automated browser journey. No staging or
commit; the user's staged .gitmodules and athena remain unchanged.

The full goal remains active. Criterion 3 stays open; consecutive batches without
full closure become 5. Reassessment changes the next action to typed structural/
declaration admission with detached ownership replay, both compiled/actual
consumers and retained drafts/history. Stop if invalid construction, cycles or
wrong types change the accepted pair, or if visual edits drop authored work.
General source UX and large-document performance, structured collections,
production component depth/assets, full WYSIWYG and native Studio retain their
original owners. No parent scope is narrowed, moved to DONE or claimed complete.

## Typed structural source and isolated builds — 2026-10-03

Delivered the codegen return batch's typed structural/declaration outcome. The
reader now replays the entire managed builder into a fresh document instead of
cloning an accepted skeleton. Grouped/unused interface and state declarations,
specialized factories, specialized class constructors, dynamic base factories,
default compound recipes, typed WithText, AddPage/AddComponent/Add/Insert and
changed state families admit through their public Pascal contracts. Removing
construction/uses removes meaning; moved ownership reparents an independently
owned candidate. Static interface assignment, unique identities, one owner,
parent admission, initialization order and the exact owned cleanup frame are
checked before publication. Unsupported expressions, arbitrary builder control
flow and direct specialized-property/part-interface assignments remain explicit
reader limits; application helpers outside the frame remain compiler-owned.

The private reader lives in nyx.source.builder.inc. Retained managed controls
own staged constructions until document adoption; rejection releases all staged
references and the candidate. The accepted document/source/history remain
unchanged. Literal token kinds distinguish quoted semicolons/end from grammar.
The same fresh replay validates subsequent visual source publication and paired
undo/redo. Configure and Binds reconcile in separate per-owner groups; a missing
Configure expressed by WithText/default construction gains a new block only
when visual meaning changes. Open identity/group keys are length-framed.

Final HTTP testing exposed a staged-construction isolation failure: token-only
frame matching deleted a retained constructor's local and left an excluded
root. The verifier rejected the reduced pair before a build job. Complete
statements/declaration names are now pruned by admitted control identity in
both comparison and authored frames before reconciliation. Retained early
construction and later adoption keep their order/expressions; excluded grouped
locals lose only their names and retain Unicode notes. Removed statement lines
do not leave one indented blank row per control. The full pair verifier remains
the final gate. Focused page/reusable/grouped-declaration cases and all six
structural HTTP scopes pass; the earlier failed logs are retained as failures.

A separate native regression found that FPC reused an ANSI/codepage-0 result
buffer in TNyxStrings.Join despite its UTF8String return type. The joined bytes
were correct, then subsequent concatenation converted them incorrectly. Join
now labels its native result CP_UTF8 without conversion before copying bytes.
No global codepage setting changes. Reused-buffer, supplementary Unicode/NUL,
state/source/history, actual controls and heap checks pass. The temporary source
traces were removed. Final review corrected two blank-above-if omissions; the
formatting-only changes compile on both targets and reuse the functional evidence.

Accepted bounded evidence:

- build/structural-repaired-generated.log: 30 core + 1277 composition/designer
  checks (1307 total), including 84 structural cases; 55 native scheduler checks,
  60 paired-project checks, compiled structural/managed/edited source execution,
  actual handwritten helpers/runtime bindings and 29 intended wrong-argument
  diagnostics from each compiler. The maintained generated target exits zero.
- build/structural-repaired-shared.dom.html executes the same 30/1277 suite with
  pas2js. build/structural-repaired-generated.dom.html passes 37 compiled browser
  reconstruction checks. The real native-emitted nyx.structural.view.pas contains
  deliberate locals, a class constructor, default compound construction,
  reusable definition/instance, Unicode notes and a later reparented adoption.
- build/structural-repaired-desktop.dom.html: 60 actual Studio checks, width 1416.
  build/structural-repaired-phone.dom.html: 60 at the exact reported 390-pixel
  iframe width. Actual Apply/property/undo/redo consumes the source-created bound
  memo, action and reusable instance while retaining names/comments/source.
- build/structural-repaired-lcl-run.log: 55 actual native Studio authoring checks,
  10 managed controls, 35 events, 50 live bindings, 74 projections and the native
  theme/identity/preview/compound/optional-output journeys. Native memo/button
  events use the shared session/command router.
- build/structural-heap-repaired-run.log: 84 checks; 24087442 allocations/frees,
  zero unfreed blocks. This includes rejected constructors, namespaces, ownership,
  cleanup/control-flow/type cases, reparenting and reduced-view reconciliation.
- build/structural-repaired-http.log: 123 checks against the repaired service,
  including application/page/reusable companions on browser and LCL. Full builds
  retain exact admitted source; reduced views reconstruct their complete design.
  Output profiles are restored by the harness's finally block.

Live service: PID 6288, persistent exec session 34376, approved executable
build/native/3.2.0/i386-win32/nyx_studio_server.exe, listening on 0.0.0.0:8088;
LAN HTTP 200. The final build is build/structural-server; SHA256
BBEAD7FEF1F98FAF5B05D5628CDCD00149D90DBAC5AAE743559F14DFD0E63FE8.
build/nyx-studio-before-isolation-repair.exe retains the earlier live binary.
Deployment waits for the verified old process to exit before copying, correcting
an initial Windows executable-lock race. Final browser/server compiles after
spacing changes are in structural-format-browser-build.log and
structural-format-server-build.log. No new physical-phone verdict is claimed;
the new mobile evidence is the exact-width automated journey. No stage/commit;
the user's .gitmodules/athena staging remains unchanged on hello-nyx.

Criteria 1/2 stay accepted. The structural deliverable advances criterion 3;
broader source editing UX and measured large-document reconciliation stay open
with the original owner. Consecutive batches without full criterion closure:
6. Reassessment stops extending scalar grammar and selects observable structured
state as the real prerequisite for required production data-control depth. The
next package must expose typed schema/item/collection references, independently
owned ordered values, atomic changes and owned subscription snapshots on both
targets; persistence/history/compiled and actual-control consumers remain gates
for the full task. Return to original codegen/Studio source scope after that
prerequisite. Full component depth, WYSIWYG, assets, native Studio and service
hardening retain their owners. The full goal stays active and unscored.

Final review passes all 14 touched Pascal files' license, Delphi/UTF-8 directives,
blank-above-if and trailing-whitespace checks, touched documentation link targets
and git diff --check. The approved live service remains HTTP 200; the user's
staged .gitmodules and athena are the only staged changes.

## Typed observable collection foundation — 2026-10-03

Goal-turn audit: the preceding structural-source/isolation turn made meaningful
product progress and remains accepted at its stated boundary. This continuation
also makes meaningful progress: a new portable public collection contract, both
executed targets, compiler rejection consumers, ownership evidence, measured
costs and documentation. There is no repeated blocker, user pause or full-goal
completion. Work remains solo on hello-nyx.

`nyx.collections.pas` and its private implementation include provide immutable
four-family schemas/items, collection-scoped stable identities, managed mutation,
snapshot/change/subscription interfaces and fluent typed factories. Schema
defaults/domains reuse scalar admission. Insert/remove/final-position move and
partial Update/default-materializing Replace build one detached candidate.
Stale revisions, unknown/wrong-typed/wrong-scoped identities, domains and budgets
reject before publication; exact net no-ops keep revisions/subscriptions stable.
Snapshot row lookup is indexed. Reads fork exposed values while private immutable
rows can be shared safely across snapshots and independent clones.

Validators receive complete immutable proposals against the accepted baseline.
Observers see one complete commit; one failure does not skip other observers.
ENyxCollectionNotification distinguishes committed observer failure from admission
failure. Serial subscription identities handle callback disconnection without
borrowing a freed token. Explicit Disconnect cancels deterministically; last-token
release can be delayed by compiler interface temporaries. Tokens borrow stores,
store disposal detaches them, and callbacks borrow their receivers. Apply/Assign
retain an implementation during callbacks even if the application drops its last
owner. Retained changes do not keep the store alive. Writes/new subscriptions
during callbacks reject, with guard cleanup on failure. All writes are UI-thread
confined; workers must use the scheduler's UI marshal boundary.

Budgets are 64 fields, 16384 items, 256 Apply edits, 1024 subscriptions and 8 MiB
logical collection payload. Key/schema/default/domain metadata is budgeted even
with no rows. Scalar/row payload sizes are cached after admission, so an unrelated
field update does not rescan unchanged large text. Payload excludes retained
snapshots, temporary/object/index overhead and therefore is not a process-memory
bound. Whole Assign still enforces row/payload/schema limits independently of the
per-Apply edit budget. TNyxStateValue.Copy exposes the existing explicit scalar
copy boundary without changing scalar semantics.

Corrections are recorded rather than treating preliminary failures as passes:

- The first test unit omitted its closing end; that source error was corrected.
  A native test compared an extended-precision 0.1 literal with a stored Double;
  it now compares exact portable scalar values using SameValue. Product numeric
  admission was not changed to accommodate the test.
- Default-record rejection fixtures now initialize Default(T) explicitly instead
  of assuming native local primitive fields are zeroed. A callback cancellation
  fixture uses Disconnect before releasing its managed token because compiler
  temporaries can retain an interface. A fresh helper scope isolates the genuine
  last-owner case; the disconnected retained token proves store disposal on both
  targets while before/after snapshots remain usable. Optimized native execution
  also validates the intentional otherwise-unused keep-alive local.
- The new negative fixtures initially encountered the intended errors but the
  harness patterns did not cover both compilers' overload/member wording. Specific
  expected typed diagnostics now cover FPC's final numeric-overload Double message
  and pas2js's selected family/identifier wording. No broad any-error pass gate.
- Temporary Pascal traces used for lifetime diagnosis were removed. Browser
  performance timing runs without virtual time; contract execution and benchmark
  evidence remain separate. Earlier failed logs are retained as failures.

Accepted evidence:

- build/collections-lifetime-run.log: 92 checked native contract cases, 210671
  allocations/frees, zero unfreed blocks. Cases include full 16384-row admission,
  the next-row rejection, bounded batches/subscriptions, stale/domain/type/index
  rejection, Unicode/NUL, no-op logs, default/clone isolation, callback failures,
  receiver cancellation, store disposal and schema/row payload limits.
- build/collections-lifetime.dom.html: data-collections=passed, executed pas2js
  92-case contract. build/collections-optimized-run.log: the same 92 cases pass
  FPC 3.3.1 with -O2 and range/overflow/assertion checking.
- build/collections-generated.log: maintained generated target terminates with
  30 core + 1369 composition/designer checks (1399 total), 55 native scheduler and
  60 project cases, compiled managed/structural/edited/helper/runtime consumers
  and 36 intended wrong-type rejections per compiler (seven new collection cases).
- build/collections-shared.dom.html: data-tests=passed, executed pas2js
  30 core / 1369 composition/designer checks. This includes the collection suite
  alongside the existing source, scalar and Studio model consumers.
- build/collections-focused-target.log: focused orchestrator finishes native
  92-case execution, verified native CSV measurement and both browser compilation
  programs with matched RTL/host staging. It does not claim browser execution
  from compiler success. Both compiler processes are confirmed terminated.
- build/collections-benchmark-native.csv and collections-benchmark.dom.html:
  verified native/browser measurements at 512/4096/16384 rows. Native setup is
  0/157/2187 ms; twenty updates 31/125/516 ms; 20000 lookups 0/0/15 ms. Browser
  setup is 27.2/238.7/2529.5 ms; updates 60.1/390.7/1449.1 ms; lookups
  24.1/27.3/27.6 ms. Payload counts agree at 15853/130053/529653 bytes. Native
  zeros mean below the observed clock resolution; these are machine-specific
  checked-build measurements, not production frame-time acceptance. Repeated
  focused-target native timing varies, as expected.

The API and every ownership/rejection/budget distinction are documented in
[collections](docs/collections.md); the focused command/hosts are in
[building](docs/building.md). Criterion 1 closes, resetting this task's consecutive
batch count to 0. Criterion 2's store/default-clone pieces have evidence, but
actual reusable-instance integration is still required. Criteria 3/4 remain open:
document/runtime registration, versioned persistence/history/compiled source,
real bindings/list/table/tree controls and selection/rejection recovery. Per-edit
publication still copies an outer vector and rebuilds an O(rows) identity index;
production update/virtualization stays assigned to NS-3 performance. No actual
collection-bound control or physical-phone verdict is claimed.

The next package is the integrated registry/wire/history/generated consumer,
followed by both actual adapters. Original codegen criterion 3 remains open at
six consecutive batches without full closure; its original return scope and all
component/Studio/service/quality owners remain. No task moves to DONE or receives
manufactured completion credit; the full goal stays active.

The live service executable/protocol is unchanged: PID 6288, persistent exec
34376, 0.0.0.0:8088, approved native path and SHA256
BBEAD7FEF1F98FAF5B05D5628CDCD00149D90DBAC5AAE743559F14DFD0E63FE8.
LAN HTTP 200 is reconfirmed. Browser diagnostics are staged, without adding an
editor flow or output prerequisite. No new firewall change, staging or commit;
the user's .gitmodules/athena staging is preserved.

Final review passes all 14 touched Pascal files' license, Delphi/UTF-8 directives,
blank-above-if and whitespace checks, nine touched documentation link checks and
git diff --check. No NYX_COLLECTION_TRACE remains. The exact public API example
extracted from collections.md compiles on FPC/pas2js and executes natively;
collections-doc-native.log and collections-doc-browser.log retain those results.
The documented sample is not a substitute for the executed 92-case browser suite.
The unchanged service PID/path and LAN HTTP 200 are reconfirmed. The only staged
paths remain the user's .gitmodules and athena.

## Collection registries, persistence and source — 2026-10-03

Goal-turn audit: the previous collection foundation turn made meaningful product
progress, accepted criterion 1 and changed the next integrated action. This
continuation delivers that action: document/application registries, versioned
wire persistence, source/history, compiled consumers and actual application
hosts. There is no repeated blocker or pause; the full goal remains active.

Managed INyxCollectionDefaults owns ordered immutable authored snapshots.
Define normalizes whole rows before replacement, preserves key position, admits
64 definitions/8-MiB aggregate logical payload and resets authored revisions to
zero. Foreign snapshots are re-admitted without trusting reported bytes/revision/
indexes or mutable backing. Clone forks registry vectors while sharing only
private immutable data. Seeded NewNyxCollection overloads admit complete datasets
without synthetic edits or the 256-edit limit. Descriptor Schema.Field retains
the default's scalar family and permits explicit NyxNoDomain; ordinary typed
factories supply a defined unconstrained family domain. Admission, normalization
and validation distinguish those meanings without guessing kinds from text.

TNyxDocument owns the managed defaults registry; clones/isolation retain independent
registries and immutable snapshots. INyxCollections materializes independent
application stores. Runtime registry clones preserve current revisions/data and
omit observers. Browser/LCL application getters retain stores through navigation;
retained stores/default snapshots outlive actual applications/documents without
target/document backreferences. Collection-bound controls remain a separate gate.

Typed collection designs select version 2. A separately versioned descriptor
stores ordered schema/default/domain/item data; Double values retain finite decimal
strings. Designs without typed defaults remain version 1. Existing version-1
opaque collections root fields stay extension data; an explicit typed/opaque
collision rejects validation/export without discarding the old value. Other
unknown root/node data and scalar admission remain unchanged. Descriptor fields,
versions, duplicates, types, row arity, domains and aggregate budgets are strict.
The existing shared 4-MiB JSON budget still limits complete export, independently
of the larger logical store budget; oversize export rejects without truncation.

Generated Pascal uses typed Define/schema/field/item/WithValue constructions,
omits row values equal to defaults, and retains typed ranges/choices or explicit
domain absence. The bounded source reader calls those public builders, admits
one definition per key and enforces scalar/reference/domain families. It does
not interpret arbitrary runtime calls or new collection local variables. Root
collection statements participate in owned metadata reconciliation, so crafted
comments survive visual edits. Shared Studio source commands pair exact design/
source history, preserve rejected drafts/redo and reconstruct reduced companions.
No opaque JSON is embedded in generated application source.

Corrections and limitations:

- A new test assumed typed unconstrained domains were absent. The cases now
  distinguish both meanings. Adding the explicit descriptor boundary exposed
  unconditional domain admission; Schema.Field/Validate/Normalize now admit an
  explicit absent constraint while retaining scalar-family validation. All prior
  typed factory/domain tests remain in the suite.
- A new fixture initially used ANSI SysUtils.StringReplace and changed Unicode
  source. It now uses the existing portable EditNyxManagedFixture/TNyxStrings
  helper; crafted comments, supplementary text/NUL and exact pair checks pass.
  An assumed NyxUse helper did not exist; the reusable fixture now uses the
  actual typed Component(NyxComponent(...)) API.
- The added seeded-snapshot overload changed FPC's intended wrong-scope
  diagnostic to INyxCollectionSnapshot. Its harness pattern now admits that
  specific expected type alongside pas2js's collection-ref wording, preserving
  the compiler rejection gate. Earlier generated/source failure logs are not
  accepted evidence.
- FPC allowed an ignored interface property read in a test, while pas2js rejected
  that statement. The unmounted-app case now assigns the result to a typed local.
  Both target application tests pass. A headless invocation used compact width
  for the existing desktop-only journey, hiding its project field; the recorded
  successful run uses the intended 1440x1100 desktop viewport. The earlier DOM
  failure retains its failed status and is not physical-phone evidence.
- Final review fixes one blank-above-if omission and expands new case bodies;
  these formatting-only changes compile on both targets. Existing functional
  evidence is retained, with a final focused native heap execution below.

Accepted evidence:

- build/collection-registry-generated-final.log: maintained target terminates
  zero with 30 core + 1426 composition/designer checks (1456 total), 55 native
  scheduler checks, 60 paired-project/disk cases, 36 intended type failures per
  compiler, existing compiled managed/structural/edited/helper/runtime consumers
  and three new compiled collection checks. Native emits nyx.collections.view.pas
  and collections.nyx from the public fixture, including a reusable definition.
- build/collection-registry-shared.dom.html: data-tests=passed, executed pas2js
  30/1426 suite. collection-registry-source.dom.html separately executes 149
  collection checks (92 stores + 57 registry/wire/source-history cases).
- build/collection-registry-generated.dom.html: three compiled browser checks
  compare complete reconstructed design meaning, runtime/default isolation and
  retained managed lifetimes. Native reconstruction executes the exact same
  emitted unit, rather than an expected-design-only generator comparison.
- build/collection-registry-lcl-final.log: 15 managed target checks, including
  five new actual application mount/navigation/disposal cases; 35 native event,
  50 scalar binding, 55 Studio authoring, 74 catalog projections and existing
  theme/compound/optional-output/native journeys. Compiled callbacks pass 17.
- build/collection-registry-application-desktop.dom.html: data-managed-journey-
  checks=15, data-event-journey-checks=35 and data-nyx-journey=passed with 49 checks.
  Both mounted browser applications retain independent registry values through
  real navigation and disposal. This proves registry-host integration, not
  collection-bound table/list/tree interactions or trusted physical input.
- build/collection-registry-http.log: 135 checks, all six new collection
  application/page/reusable browser/LCL scopes compile with exact admitted source.
  The public fixture's version-2 Unicode/schema/default/row data survives each
  isolated/full compiler route. Profiles restore in the harness's finally block.
- build/collection-registry-source-utf8-run.log: 149 native checks, 5125944
  allocations/frees, zero unfreed blocks. It precedes adding the reusable fixture
  root; final focused heap/format checks cover the final fixture and are recorded
  after their authoritative completion below.

Collection criteria 1/3 are accepted at the documented contract boundary.
Criterion 2's atomic/snapshot/token/default-clone and sibling application portions
are evaluated; actual independent reusable-instance collection bindings remain.
Criterion 4 is open: typed shared binding descriptors, actual list/table/tree
controls, stable selection, edit/reorder/rejection recovery and real control
update/virtualization costs. Consecutive batches without full criterion closure:
0, because criterion 3 closes here. No task moves to DONE or gains full-product
credit. Original codegen criterion 3 remains open at count 6 for broader source
UX/performance; return after this real collection prerequisite. Full component
depth, native Studio, assets and service/quality owners keep their original scope.

Next integrated package: bind collections to actual browser/LCL data controls
through typed source/property contracts and independent reusable-instance scopes.
Preserve identity across insert/remove/reorder, retain selected IDs through
updates and reject commands atomically. Connect Studio's Nyx-built authoring
workflow and generated/isolated consumers; measure actual adapter costs before
claiming production depth or virtualization.

The version-2 service is deployed to the existing firewall-authorized native
path after guarding the old PID/path and waiting for its exit. Backup:
build/nyx-studio-before-collections.exe. New PID 20272, persistent exec 34787,
0.0.0.0:8088; LAN HTTP 200. Server build: build/collection-registry-server;
SHA256 58705B4E4200278E73EB9A732B055CBA6D7F176C722EA8732ABD71315651BCFF.
No new output prerequisite, firewall change, stage or commit; user staging remains
.gitmodules/athena. The full goal is active and unscored.

Final focused evidence: collection-registry-final-heap-run.log passes 149 cases
with the final reusable fixture, 6134339 allocations/frees and zero unfreed
blocks. collection-registry-final-format-browser-build.log compiles that exact
fixture and the formatting changes with pas2js. All 24 touched Pascal sources
retain the full MIT attribution, Delphi/UTF-8 declarations where applicable,
blank-above-if layout and whitespace checks. The final documentation links and
git diff --check are reviewed; user staging is unchanged. Deployment remains
PID 20272/session 34787 on the approved path, LAN HTTP 200. No remaining compiler
or test process is treated as live after its observed terminal result.

## Selected component intent help — 2026-10-03

User steering confirms the high-level description requirement. Existing portable
catalog descriptions already cover all 75 default kinds and custom creators via
typed `Catalog.Describe`. Search, tooltip/native Hint, optional full-width Details
and the generated reference consume this same text. The remaining contextual
surface now shows the selected component's explanation above Properties/Events.
Registered recipes retain their creator's intent instead of generic layout-root
help; unregistered projections can use the known primitive's explanation. Help
reads do not insert metadata into the design or add history entries. Public
comments and the creator/building guides describe these semantics.

Accepted evidence:

- `component-help-native-run.log`: 30 core + 1428 composition/designer checks
  (1458 total), including 103 discovery cases. Two new shared cases preserve
  a custom recipe's Unicode creator intent and accepted design during help reads.
- `component-help-shared.dom.html`: executed pas2js passes the same 30/1428.
  FPC and browser consumers were compiled from the same implementation; later
  Pascal edits only clarify existing public comments.
- `component-help-desktop.dom.html`: 25 actual Studio interactions; the exact
  compact-frame counterpart `component-help-phone.dom.html` passes 24 at 390px.
  Both verify selected Inspector help and its visible width, alongside description
  search, real tooltips, Details, grouping, insertion and paired undo preservation.
- `component-help-lcl.log`: terminal exit 0, 56 actual native authoring checks
  including reading context help from a real LCL label on the Events tab.
  Existing 15 managed, 35 event, 50 binding, 74 projection and 17 compiled
  callback-control checks remain passing.
- Six touched Pascal sources pass license/Delphi/UTF-8/blank-above-if checks;
  `git diff --check` passes. Browser Studio compilation succeeds. All compiler
  and test handles have observed terminal results; only the service stays live.

Browser artifacts are served immediately from the existing LAN-bound service;
no service binary, firewall or output configuration changed. PID 20272/session
34787 remains on the approved path, binding 0.0.0.0:8088; LAN HTTP returns 200.
Branch remains hello-nyx; user staging remains .gitmodules/athena. No commit or
parent-task completion is claimed. This is a bounded user-directed help delivery,
not acceptance of the full component, Studio or collection outcome.

Previous registry/persistence work made meaningful accepted progress; no blocking
condition recurred. Return next to typed collection bindings consumed by actual
browser/LCL list/table/tree controls, preserving the collection task's original
criteria, reusable-instance isolation and performance evidence. Its closure
counter remains 0; the original codegen criterion 3 remains open at 6. The full
goal remains active.

## Typed collection views and real controls — 2026-10-03

The preceding user-directed creator-help work made meaningful progress, with no
repeated blocking condition. This batch returns to collection criteria 2/4:
connect typed shared projections to real browser/LCL controls, prove reusable
isolation and identity/rejection/lifetime behavior, then measure update costs.
Original criteria are preserved. The deliverable is a managed runtime bridge;
authored/Studio mounting is still the next integrated boundary.

New portable contracts: immutable `TNyxCollectionViewSpec` with typed column
references and enum scope/projection/editability, strict versioned descriptor
packets, managed `INyxCollectionView`, ordered observer tokens, and an independent
application/instance context. Views retain stores/snapshots, preserve selection
by scoped item ID, and clear it after removal. Tree parent/cycle validation uses
an iterative walk against the complete candidate before publication. Adapter
wire edits use existing scalar domains and report rejection versus already-
committed observer failure. Lists project labels/selection; tables project typed
columns; trees project parent links and editable captions. Runtime instance
stores use separately retained owner/key pairs and authored defaults.

Both renderers expose `BindCollection` and retain attachments until disconnect
before widget/DOM disposal. Browser rows/cells retain identity through moves,
restore focused editors without scroll, and preserve root click/key callbacks.
Consumed keys reach Nyx before default row selection. Native adapters use actual
list boxes, string grids and tree views, preserving native tree-node identity
and restoring replaced handlers on disconnect. Active calls retain their own
managed attachment/view frame through last-owner release or callback unmount.
Exact NUL text uses an explicit native escaped editor representation; ordinary
text stays ordinary UTF-8. No global codepage changes or non-Pascal product code.

Accepted evidence under ignored `build/`:

- `collection-views-final-focused.log`: terminal exit 0; 149 collection checks,
  32 portable view checks, 38 intended pas2js type rejections, 25 actual native
  collection control checks and correctness-gated mounted-control CSV.
- `collection-views-desktop.dom.html` / `collection-views-phone.dom.html`: executed
  pas2js passes 32 shared / 26 actual collection control checks; the compact
  host reports width 390. These are synthetic DOM/viewport journeys, not trusted
  OS keys or new physical-phone evidence. Native editor tests invoke the real
  registered validation/change callbacks with caller-owned arguments.
- `collection-views-final-core.log`: 30 core + 1460 composition/designer checks
  (1490 total), 38 intended FPC type rejections, existing compiled reconstruction
  and three compiled collection checks. `collection-views-shared-browser.dom.html`
  executes the same 30/1460. `collection-views-compiled-browser.dom.html` passes
  three compiled collection checks; `collection-views-existing-generated-browser.dom.html`
  retains 37 compiled reconstruction checks.
- `collection-views-final-lcl.log`: terminal exit 0; 40 managed target checks
  (including the 25 collection cases), 35 event, 50 scalar binding, 56 actual
  Studio authoring, 74 catalog projections and 17 compiled callback/control checks.
  `collection-views-studio-journey.dom.html` retains 41 managed target, 35 event
  and 49 Studio interactions; `collection-views-scalar-bindings.dom.html` retains
  48 actual browser scalar binding checks. The pure authoring interface suite in
  `collection-views-managed-browser.dom.html` remains 184; it is distinct from the
  actual managed target journey and is not added to those counts.
- Final lifetime logs: `collection-views-heap-checked-run.log` passes 32 with
  199985 allocations/frees; `collection-views-heap-optimized-run.log` passes 32
  with 164812; `collection-views-controls-heap-run.log` passes 25 actual optimized
  LCL controls with 202518. All three report zero unfreed blocks.
- `collections-view-benchmark-native.csv`: 512/4096 rows mount in 15/93 ms;
  twenty integer updates with all three controls attached take 297/1844 ms.
  `collection-view-benchmark-browser.dom.html` reports 112.8/487.5 ms mount and
  936.9/5378 ms updates. Every attachment completes 21 refreshes and row/value/
  revision/disconnect gates pass. Native time is coarse and widgets are hidden;
  browser uses the real clock without a virtual-time budget. These observed
  samples include store/view/adapter work, exclude seed construction and do not
  measure paint, physical input, sustained memory or a production frame budget.
  Full materialization, changed-row work, deep hierarchy behavior and budgets
  retain their owner in NS-3_extension-performance_01.

Corrected failures are not acceptance evidence: the initial focused build found
an old generated fixture named `nyx.collections.view`, colliding with the new
library unit. The fixture now uses `nyx.fixture.collections`; only the verified
obsolete generated cache siblings were removed, and both compiled consumers
passed. pas2js required external-class mode for the typed focus bridge and dynamic
arrays for managed benchmark interfaces. The compact host now polls through a
temporarily absent iframe body. A range test was corrected to expect its actual
contract exception; interface lifetime probes isolate legitimate factory
temporaries. Empty observer diagnostics now report postcommit failure and allow
later observers. Review added last-owner/real-unmount regressions. A keyboard
regression then exposed missing native list/grid/tree handlers; their common
registration and browser descendant/default-action routing are now exercised.

Criterion 2 closes from actual reusable-instance/default isolation and lifetime
evidence, joining accepted criteria 1/3. Criterion 4 stays open for node-owned
authored bindings, composition/override inheritance, wire/source/history,
automatic application/instance mounting and Nyx-built Studio authoring commands.
Root disabled/read-only propagation into generated row editors also remains in
that package. Budget: one integrated implementation/evidence batch, then reassess.
Stop/switch on candidate-pair corruption, cross-instance leakage or navigation
data loss. The closure counter is 0 because criterion 2 closes here. No task moves
to DONE, no criterion is weakened/transferred, and no parent/product credit is
claimed. The original codegen criterion 3 stays open at count 6; return to its
source UX/performance after this prerequisite instead of expanding scalar grammar.

Documentation: `docs/collection-views.md`, the collection/build guides, project
profile and existing task/performance owners. Full MIT/Delphi/UTF-8, thorough
ownership/failure comments, blank-above-if style and whitespace checks apply.
The only Pascal edit after final verification clarifies the existing interface-
owner requirement before attachment activation; it changes no behavior.

Service remains PID 20272 / persistent exec 34787 on the firewall-authorized
executable, bound to 0.0.0.0:8088. LAN HTTP is verified 200; binary SHA256 remains
58705B4E4200278E73EB9A732B055CBA6D7F176C722EA8732ABD71315651BCFF.
Browser artifacts are served immediately; no service restart, firewall change,
new output prerequisite, stage or commit. Branch hello-nyx and user staging
`.gitmodules` / `athena` are preserved. All batch compiler/test processes have
observed terminal results; only the live service remains. Full goal stays active.

## Authored collection bindings and Studio — 2026-10-03

Owning task: NS-1_state-collections_01, original criterion 4. One integrated
implementation/evidence batch completes the authored boundary; criteria 1/2
retain their accepted evidence and criterion 3 is revalidated after the service
clone correction below. All four original criteria are accepted, the task is
now in `TODO/DONE/`, and its closure counter is 0. Full component depth, native
Studio, universal accessibility/interaction parity, service hardening and delivery
remain open with their existing owners. No task split, criterion weakening or
arithmetic parent/product completion claim is made.

The node owns a copied, immutable typed collection-view specification. Version 3
reserves its node packet, including explicit clear; version 1/2 same-name opaque
extensions remain data and promotion collisions reject. Composition carries the
nearest reusable owner through nested instances and override payloads without
saving runtime IDs. Specialized control bindings generate public fluent objects,
typed field references and enums. Paired reconciliation preserves deliberate
column expressions/comments through scope changes; invalid edits keep the
accepted pair and history.

Applications retain one shared collection context and per-page binding
controllers. Both adapters automatically attach actual list/table/tree controls;
navigation retains data and selected identities, hidden pages keep validators,
and independent reusable stores retain authored defaults. Controller compatibility
is checked before replacing accepted controls. Ancestor enabled/read-only policy
reaches collection editors; disabled physical callbacks preserve selection and
browser design mode refuses data writes. Manual mounts and last-owner/unmount
contracts remain supported. Ordinary scalar ancestor read-only semantics remain
with NS-2's original parity criterion; this collection result does not claim them.

Studio's Data/Bindings panels use public Nyx controls and a portable typed command
router. They create collections, add four field families, edit defaults/rows,
choose scope/columns/tree parents and clear/inherit bindings through detached
undoable candidates. Used-definition removal, invalid hierarchy/schema and stale
commands reject. Explicit escaped text editors preserve NUL and supplementary
Unicode. New field buttons use a two-column Nyx grid for sidebar width. Collection/
field names are currently assigned; arbitrary names/domains/order remain available
in the typed Pascal workspace. These limits are documented rather than counted
as full Studio completion.

Accepted artifacts under ignored `build/collection-authoring-check/`:

- `focused-final.log`: terminal exit 0; four checked FPC 3.2.0 Unicode
  packet/design/envelope/source boundaries, 27 shared authoring checks, 25 actual
  LCL automatic-control/application/Studio checks, six compiled application/page/
  reusable reconstruction checks and forty intended type rejections per compiler.
- `authoring-final.dom.html`: executed pas2js passes the same 27 shared plus
  26 browser automatic-control/Studio checks. `unicode-final.dom.html` passes all
  four Unicode boundaries; `generated-final.dom.html` executes six native-emitted
  companion checks. Compilation alone is not treated as browser execution.
- `heap-build.log` / `heap-run.log`: checked FPC 3.3.1 with range/overflow and
  heap tracing passes 27/25; all 22091995 allocated blocks are freed, zero unfreed.
- `regression-all.log`: core 30 + 1460 shared checks, scheduler/project/generated
  consumers and intended type matrices pass. This driver then stops at Windows'
  locked live server executable; that partial driver is not an `all` success.
  `lcl-final.log` subsequently completes with exit 0: 40 managed target, 35 event,
  50 scalar binding, 56 Studio authoring, 74 projections, 79 descriptor and
  17 compiled callback/control checks. `shared-final.dom.html` executes 30/1460;
  `studio-final.dom.html` executes 60 desktop authoring checks.
- `studio-phone-final.dom.html`: 60 Studio authoring checks in a verified
  390-pixel iframe. `palette-phone-final.dom.html` retains 24 intent/help/discovery
  checks at width 390. `journey-final.dom.html` retains 41 managed target, 35
  event and 49 Studio interactions; `bindings-final.dom.html` retains 48 actual
  scalar binding checks. These are synthetic DOM/viewport tests, not new physical
  phone or trusted operating-system input evidence.
- `server-build.log` / `http-final.log`: checked FPC 3.2.0 server and 147 HTTP
  checks. Application/page/reusable collection companions compile for both targets,
  and each saved job design retains exact Unicode defaults and typed bindings.
  Existing handwritten callback and paired-project scopes remain green.
- `studio-build.log`: final pas2js Studio compilation, terminal exit 0, including
  the Data/Bindings panels and two-column field controls.
- `catalog-final.log`: terminal exit 0; regenerated all 75 capability entries
  and updated the reviewed reference for the admitted collection/container
  read-only property schema.

Corrections are not acceptance evidence. A premature Unicode suspicion was
disproved by focused packet/design tests. The actual failure was the service's
manual application clone omitting collection defaults; v3 binding admission
exposed an existing saved-job-design gap. The service now uses `ADocument.Clone`,
and the HTTP harness explicitly compares saved job defaults/bindings against its
accepted companion. This applies the declared candidate-pair stop/switch condition
and strengthens criterion 3 revalidation before closing criterion 4. Earlier
failed HTTP runs are retained, not counted. The fixture also obtains its actual
Unicode field name instead of assuming `caption`, and its unit name avoids a
library collision. A micro-test's aliased envelope output was separated from its
input. The build driver now returns success after its intentionally failing final
type-rejection child. Native selection handlers enforce policy before accepting
nil/disabled selections; regression evidence includes forced real callbacks.

Prior correctness-gated 512/4096-row measurements remain applicable and are not
repeated without a changed performance question. Full materialization, incremental
updates, deep hierarchy and production frame/memory budgets retain NS-3 ownership.
API, wire, ownership/failure limits and reproducible targets are documented in
`docs/collection-views.md`, `docs/collections.md` and `docs/building.md`. Full MIT,
Delphi/UTF-8 and thorough comments remain; new Pascal files pass blank-above-if
and whitespace checks. No Pascal changed during compiler/service pipelines.
The final Pascal edit only inserts the preferred blank line before an existing
catalog initialization `if`; compiled behavior and the deployed binary are unchanged.

The approved service binary replaced the live executable at its existing
firewall-authorized path after guarded PID/path/hash verification and backup.
New PID 8608 / persistent exec 66348 binds 0.0.0.0:8088; loopback health and LAN
HTTP both return 200. SHA256:
`3E0F622C85C72221E72E10C179585F5354628980BA243657000C52D1A6A020B1`.
The staging service and old live session have observed terminal exits. All
compiler/test/Edge handles are terminal; only the new live service remains.
No firewall change, output prerequisite, stage or commit. Branch hello-nyx and
user staging `.gitmodules` / `athena` remain intact. The full goal stays active.

Return now to original NS-1_codegen_01 criterion 3's source UX/performance.
Its consecutive-batch count remains 6; reassessment selects navigable diagnostics
and correctness-gated source/visual-edit measurements at increasing sizes as one
bounded implementation/evidence package. Keep exact pairs, drafts/history and
crafted locals/comments; stop on corruption/discarded drafts or evidence that
requires another algorithm. Do not resume scalar grammar expansion as a substitute.

## Source diagnostics and reconciliation scaling — 2026-10-03

Owning task: original NS-1_codegen_01 criterion 3. This implementation/evidence
batch delivers Apply diagnostics and measures authored source operations through
the actual public session. It does not close criterion 3: the measured large-
project costs violate the intended responsive authoring outcome. Criteria 1/2
remain accepted; the original consecutive-batch count advances from 6 to 7. No
task moves to DONE or gains parent/product completion credit.

The session owns a typed diagnostic for the exact failed draft without retaining
its exception or any target control. Source text keeps its UTF-8 diagnostic
separately from native Exception.Message. Nyx-built labels/buttons display it
beside the editor; an explicit Go action validates its current snapshot and
navigates one-based Unicode scalar line/column. Browser/Win32 caret conversion
handles supplementary characters, CRLF and bounds. Typing clears/hides stale
presentation while retaining the editor; Restore/success removes it. Invalid
Apply retains the accepted pair, exact draft and history. Unknown admission sites
remain readable without guessed coordinates. Compiler-owned helper errors still
use build logs; structured compiler-file navigation retains original Studio/
service ownership and is not claimed from Apply-only coverage.

The maintained Pascal benchmark constructs 128/512/2048-control documents outside
timing. It applies crafted locals, Unicode comments and an unchanged expression,
performs one visual edit and one structural addition, verifies exact paired
undo/redo and rejects a wrong-type retained draft with a diagnostic. Only a fully
correct size publishes a row. Source bytes are UTF-8-normalized on both targets;
timing excludes fixture construction, correctness gates, painting, physical input
and compiler latency. Native uses coarse GetTickCount64; browser uses the real
performance clock without virtual-time scheduling. These are observed samples,
not cross-target speed comparisons or production budgets.

| Target | Controls | Source bytes | Generate ms | Apply ms | Visual ms | Structural ms | Reject ms | History ms |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| Checked FPC 3.2.0 | 128 | 25063 | 15 | 63 | 406 | 406 | 16 | 109 |
| Checked FPC 3.2.0 | 512 | 98791 | 47 | 234 | 2500 | 2390 | 63 | 407 |
| Checked FPC 3.2.0 | 2048 | 399991 | 156 | 1297 | 21312 | 21172 | 328 | 1859 |
| pas2js / Edge | 128 | 25063 | 21.9 | 273.3 | 1821.1 | 1819.4 | 54.1 | 546.4 |
| pas2js / Edge | 512 | 98791 | 55.4 | 993.8 | 14619.7 | 14485.6 | 233.8 | 2110.3 |
| pas2js / Edge | 2048 | 399991 | 227.4 | 5441 | 178968.2 | 177663.3 | 1292.5 | 8661.8 |

Earlier checked native visual/structural times were 7219/7234 ms at 128 and
31281/31297 ms at 512. Reconciliation now rejects non-assignment grammar before
catalog recognition, caches finite built-in names at unit initialization before
workers can read them, and skips known structural atoms before scalar probing.
These preserve semantics and reduce cost, but do not solve the large-case scaling.
The scalar-probe change alone did not materially improve the dominant native
cost; subsequent grammar/cached-name changes have separate measured results.
An initial all-size native run was stopped by verified PID/path at its explicit
four-minute measurement budget. A later probe was stopped after its 128-control
row resolved that experiment negatively. Neither stop was an observation timeout
or a reason to restart a supposedly missing process. Final both-target all-size
measurements completed and passed every gate.

Accepted artifacts under ignored `build/source-workspace/`:

- `focused-final.log`: native 15 diagnostic cases and both-target fixture/benchmark
  compilation. `diagnostics.dom.html` executes the same 15 in the browser.
- `core-final.log`: 30 core + 1475 composition/designer checks (1505 shared total),
  forty intended FPC type rejections, 55 scheduler, 60 project checks and compiled
  reconstruction including three collection checks. `shared.dom.html` executes
  the same 30/1475. The first verification wrapper expected the wrong attribute;
  the existing terminal DOM was checked against its maintained `data-tests=passed`
  marker without rerunning or counting the wrapper failure as product evidence.
- `lcl-final.log`: 60 actual native Studio checks, including the visible diagnostic,
  explicit Go action and UTF-16 caret after supplementary Unicode; existing 40
  managed target, 35 event, 50 scalar binding, 74 projections, 79 descriptor and
  17 compiled callback/control checks retain their gates.
- `authoring-desktop.dom.html` / `authoring-phone.dom.html`: 64 actual Studio checks
  each, with the compact frame reporting width 390. Failed Apply, explicit focus,
  exact quote-column navigation, stale-error hiding and successful removal are
  executed through real Nyx/DOM controls. These are synthetic viewport/events,
  not new physical phone or trusted OS input evidence.
- `heap-build.log` / `heap-run.log`: checked native diagnostics, 553149 allocations
  and frees, zero unfreed blocks including the finite immutable name caches.
- `benchmark-native-128.csv` / `benchmark-native-512.csv` retain pre-optimization
  samples. `benchmark-native-final.csv` and `benchmark-browser.dom.html` complete
  the final all-size correctness-gated rows above. The browser renderer's live
  CPU/process was verified while it ran; it was polled through terminal completion.
- `browser-final.log` retains forty intended pas2js type rejections and compiled
  shared/target fixtures, then stops at a new test's unsupported CSS binding.
  `authoring-browser-build.log` and `studio-browser-build.log` finish those changed
  consumers with exit 0. An earlier duplicate unit import and the CSS getter were
  corrected; failed driver attempts are not claimed as full browser build success.
- `server-build.log` / `http-final.log`: checked service and 147 HTTP checks,
  including exact source, v3 collection defaults/bindings and all application/
  page/reusable scopes for browser/LCL. The source optimization is thus consumed
  by compiled frontend/native and delegated service workflows.

Documentation: source/build guides, profile, original codegen task and milestones.
All relative links/whitespace and blank-above-if checks remain required. Full MIT,
Delphi/UTF-8 and thorough public/ownership/failure comments are preserved. No Pascal
changed during compiler or delegated HTTP pipelines. The only edits after final
behavior verification insert two preferred blank lines; they change no behavior.

The accepted server was deployed after guarded live PID/path/hash verification
and backup at the existing firewall-authorized path. PID 26568 / persistent exec
35775 binds 0.0.0.0:8088; loopback health and LAN page return 200. Binary SHA256:
`1D9030E45724D84859B89143E404A31FBC6C4B5F7AC186EB23E666AAD59E17BF`.
Staging and prior live handles have observed terminal exits. All compiler/test/
Edge handles are terminal; only this live server remains. No firewall changes,
new output prerequisites, stage or commit. User staging `.gitmodules` / `athena`
and branch hello-nyx remain intact. The full goal remains active.

Reassessment: the declared measured-scaling stop/switch condition is met. Next
delivery profiles and replaces repeated symbol/metadata scans with indexed lookup
and reuses unchanged sections, preserving current pair/history/draft/Unicode and
crafted-expression gates. Repeat this same both-target benchmark; do not rename
the sample or extend scalar grammar to reset the counter. Budget: one algorithm
implementation/evidence batch, then reassess the original criterion. Stop on pair
corruption or a failed preservation gate. Browser-primary responsiveness is the
priority; no source or production-performance criterion is weakened/transferred.

## Indexed source reconciliation — 2026-10-03

The selected one-batch algorithm delivery is complete. Original codegen criteria
1/2 remain accepted; criterion 3 remains open. Its consecutive batches without
full closure increase from 7 to 8. No DONE, completion credit or scope transfer.

Opt-in `NYX_SOURCE_PROFILE` records measured stages using the benchmark's supplied
monotonic clock. Production builds have no clock/observer/counters. Parent stages
include nested costs and cannot be added as exclusive totals; instrumentation is
single-threaded and absent from the service. The same 512-control fixture found
browser scaffolding at 10462.7/7834.8 ms (visual/structural); after indexing it
takes 684.2/705.1 ms. Native falls from 828/828 to 172/172 ms. Visual frame merging
falls from 1516.6 ms to zero in that profile because its generated frame is unchanged.
These profiles diagnose stage costs; final timing rows below use ordinary builds.

Implementation:

- The private source index owns exact key text and integer array positions.
  Collision resolution compares complete keys; growth detaches managed arrays.
  Caller-normalized ASCII Pascal names remain case insensitive, while open
  application IDs remain exact. Tables never determine emitted order.
- Reader declaration/reference lookup, uniqueness checks, generated/authored name
  matching, reserved names, metadata owner groups and removal checks use owned
  indexes released with their operation/reader, including failed admission.
- Scaffolding locates each owner's first construction/adoption/Configure/Binds
  anchor in one token pass, retaining existing empty Clear/Inherit merge behavior.
- Identical before/after sections return exact authored bytes immediately.
  A pristine section takes its generated replacement directly. Other changes
  retain typed atom comparison and bounded diffing; the complete candidate still
  reconstructs and matches the design before any source/pair publication.
- Eleven new public-session behavior checks exercise colliding local/identity
  keys, case-sensitive and Unicode IDs, unchanged expressions/comments, state
  migrations, removal, exact paired history and rejected namespace collisions.
  They do not assert implementation internals or wall-clock thresholds.

Final real-clock measurements, with compiler/other browser work idle. Source
UTF-8 byte counts remain 25063/98791/399991 on both targets. Every sample passes
the original crafted-local/comment/expression, structural, paired-history and
wrong-type retained-draft gates before publishing a row. Native clock resolution
is coarse; these costs cover portable source/document work, not paint, trusted
input, compilation latency or memory budgets.

| Target | Controls | Generate ms | Apply ms | Visual ms | Structural ms | Reject ms | Three history operations ms |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| Checked FPC | 128 | 16 | 47 | 297 | 375 | 16 | 109 |
| Checked FPC | 512 | 31 | 219 | 1234 | 1484 | 63 | 406 |
| Checked FPC | 2048 | 157 | 1110 | 5266 | 6265 | 312 | 1859 |
| pas2js / Edge | 128 | 26.2 | 309.4 | 1124.5 | 1181.9 | 52.5 | 543.4 |
| pas2js / Edge | 512 | 56.5 | 903 | 4127.5 | 4820.2 | 209 | 2213.9 |
| pas2js / Edge | 2048 | 233.1 | 3773.3 | 16496 | 18836.8 | 921.1 | 8729.4 |

Previous ordinary-build browser visual costs were 1821.1/14619.7/178968.2 ms;
native costs were 406/2500/21312 ms. Scaling improves materially, but larger
crafted projects still take seconds and remain unacceptable for Studio.

Final evidence under ignored `build/source-profile/`:

- `focused-final.log`, `focused-browser.dom.html`: 26 native/executed-pas2js
  diagnostic/index checks; both programs and ordinary benchmarks compile.
- `core-final.log`, `shared.dom.html`: 30 core + 1486 composition/designer checks
  on both runtimes (1516 total), forty intended native type rejections, 55 native
  scheduler and 60 paired-project checks. Compiled native structural/managed/
  legacy/edited/helper/runtime fixtures and three collection checks pass.
- `lcl-final.log`: 60 actual native Studio authoring, 40 managed controls, 35
  events, 50 scalar bindings, 79 descriptors, 74 projections and 17 compiled
  callback checks. `browser-final.log` compiles current frontend/actual consumers
  and verifies forty intended pas2js type rejections, exiting zero.
- `authoring-desktop.dom.html`, `authoring-phone.dom.html`: 64 actual authoring
  checks at a 756-pixel headless viewport and 64 in an exact 390-pixel frame.
  The first shell wrappers expected `data-authoring` and exited 1 despite actual
  PASS. The existing terminal DOMs were rechecked against their defined
  `data-nyx-authoring` / `data-nyx-authoring-host` markers, counts and frame width;
  that verification exits zero. No product error or duplicate rerun is claimed.
- `compiled-browser-build.log`, `compiled-generated.dom.html`,
  `compiled-collections.dom.html`, `compiled-callbacks.dom.html`: current native-
  emitted companions compile and execute 37 reconstruction, three collection and
  eleven actual compiled browser callback checks.
- `heap-build.log`, `heap-run.log`: all 26 focused cases with checked FPC/heaptrc;
  7550795 allocations/frees and zero unfreed blocks, including failed admission.
- `server-build.log`, `http-final.log`: checked non-profiled server; all 147 HTTP
  companion/application/page/reusable/typed collection source gates pass on its
  staging port, with both compiler targets and native artifact execution.
- `profile-native-before.csv`, `profile-native-after.csv`, corresponding browser
  DOMs, and profile compiler logs retain the stage comparison. Ordinary final
  rows are `benchmark-native-final.csv` and `benchmark-browser.dom.html`.

The live replacement briefly encountered Windows' image lock after shutdown.
The backup was restored and served health 200; after its verified terminal exit,
bounded copy retry installed the staging hash. Final live PID 16624 / persistent
exec 51936 uses the original firewall-authorized executable path, binds
0.0.0.0:8088 and returns 200 for loopback health and LAN Studio. SHA256:
`17BFB9D9F20A584E1054E4971B1841106B3810F586BCF90306AF87A15BD929E2`.
The previous binary is retained at `build/studio-before-source-indexing.exe`.
Staging/old/backup server handles have observed terminal exits; all compiler,
test, Edge and benchmark handles are terminal. Only the live service remains.
Pascal stayed frozen throughout compiler/delegated pipelines. The final source
edit after behavior gates inserts one preferred blank line; the checked server
and heap suite were compiled afterward. No stage, commit or firewall changes;
user-staged `.gitmodules` / `athena` and branch hello-nyx are intact.

Reassessment: indexed/unchanged-section work meets its bounded delivery but does
not close original criterion 3. Profiled costs now spread across repeated lexical/
symbol reconstruction, source preparation and complete verification. Next bounded
algorithm delivery shares owned parsed builder contexts within one reconciliation,
with explicit lifetime/admission/cache boundaries, measured stages and the same
128/512/2048 preservation benchmark. Budget one implementation/evidence batch,
then reassess; stop on pair corruption or failed preservation. Do not extend
scalar grammar, reset the counter or move production performance away to create
completion credit. Full Studio, compiler-file diagnostics and release goals keep
their original owners; the overall goal remains active.

## Parsed source contexts — 2026-10-03

Previous goal turn classification: progress. Its owned index/unchanged-section
implementation, preservation evidence and measured bottleneck changed the next
action. This selected context algorithm delivery is also complete progress, but
original codegen criterion 3 stays open. Criteria 1/2 remain accepted; consecutive
batches without full closure increase from 8 to 9. No DONE or completion credit.

An operation-owned context pool now keys each immutable builder snapshot by exact
source text. Each snapshot owns full/comment-filtered tokens and typed local/state
facts/symbols. A borrowed lexical reader scans constructor identities after its
single declaration/reference pass, retaining former uniqueness admission. Mutable
expression readers clone individual records and rebuild their own indexes; they
share no cursor, assignment flags or mutable arrays with context facts. No reader,
document, renderer or global registry is retained. Pools never survive a merge or
enter persistence/history/recovery. Full candidate reconstruction remains fresh
and complete before publishing any member of the pair.

Specialized builders skip unused legacy migration readers. Real base-class
builders still use the original migration rules and retain comments/identity
expressions. Complete/filtered tokens preserve native UTF-8 byte and browser
UTF-16 storage offsets. Private contexts are read-only to all consumers. Failure
releases the whole pool; independent candidate/diagnostic ownership stays intact.

Intermediate checked native 512-control profiles fall from 1234/1484 ms to
734/984 with pooling, then 563/735 with single-pass facts and mutable-reader
cloning. Current executed pas2js profiling passes its gates at 1964.6/2319.7 ms
(previous ordinary rows 4127.5/4820.2). Its later scaffolding costs are 9.3/7.7 ms,
metadata splits 26.5/25.1 ms and legacy probing 2/0.7 ms. Instrumented lex reads
are six unique snapshots for the visual edit and ten for the structural edit;
symbol reads are five/eight. These counts diagnose actual reuse. Parent stage
costs overlap nested lex/symbol reads; they cannot be added as exclusive totals.
Instrumentation remains absent from production builds and concurrent service use.

Nine new public-session behavior checks use identical authored local names with
different source/control types/scalar families/defaults in independent workspaces.
Visual/structural edits preserve each expression/comment/type; rejected drafts,
post-failure edits and exact histories retain independent meaning. The first
fixture compile used a nonexistent Restore method; it was corrected to the actual
DiscardSourceDraft API. This was a fixture error, not a production failure.

Final ordinary real-clock measurements, with other compiler/browser work idle.
All original preservation gates pass; source UTF-8 byte counts remain
25063/98791/399991 on both targets. Native clock resolution is coarse: its zero
small generation row means below one observed tick, not zero work. These costs
cover portable source/document work, not paint, trusted input, compile latency or
memory budgets. Apply/history were not optimized and vary with runtime/GC load.

| Target | Controls | Generate ms | Apply ms | Visual ms | Structural ms | Reject ms | Three history operations ms |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| Checked FPC | 128 | 0 | 47 | 125 | 172 | 15 | 94 |
| Checked FPC | 512 | 46 | 219 | 563 | 734 | 47 | 422 |
| Checked FPC | 2048 | 172 | 1109 | 2531 | 3234 | 312 | 1843 |
| pas2js / Edge | 128 | 25.1 | 308.4 | 554.2 | 652.4 | 54 | 597.9 |
| pas2js / Edge | 512 | 56.9 | 900.7 | 1879.1 | 2351.9 | 209.3 | 2228.1 |
| pas2js / Edge | 2048 | 235.1 | 3929 | 7577.8 | 9332.2 | 905.1 | 10501.5 |

Previous ordinary visual rows were 297/1234/5266 ms natively and
1124.5/4127.5/16496 ms in the browser. The gains are material, but large projects,
full source Apply and paired history still take seconds; responsiveness remains
unaccepted. The original largest browser visual case before indexing was
178968.2 ms; that historical improvement does not close the current gap.

Final evidence under ignored build/source-context/:

- focused-final.log and focused-browser.dom.html: 35 native/executed-pas2js
  diagnostic/index/context cases. The complete current browser suite below also
  executes the context cases after the final internal storage cleanup.
- core-final.log and shared.dom.html: 30 core + 1495 composition/designer checks
  on both runtimes (1525 total), forty intended native type rejections, 55 native
  scheduler and 60 paired-project checks. Compiled native ordinary, structural,
  managed, legacy, edited-helper/runtime and three collection consumers pass.
- lcl-final.log: 60 actual native Studio, 40 managed controls, 35 events, 50 scalar
  bindings, 79 descriptors, 74 projections and 17 compiled callback cases.
  browser-final.log compiles current frontend/real consumers and verifies forty
  intended pas2js type rejections, with terminal exit zero.
- authoring-desktop.dom.html / authoring-phone.dom.html: 64 actual Studio cases
  at a 1096-pixel headless viewport and 64 in an exact 390-pixel frame. Their
  declared result markers/counts/widths are verified; both wrappers exit zero.
- compiled-browser-build.log and compiled-generated/collections/callbacks DOMs:
  current native-emitted companions compile and execute 37 browser reconstruction,
  three collection and eleven actual compiled callback checks.
- heap-build.log / heap-run.log: all 35 focused cases with checked FPC/heaptrc;
  5417226 allocations/frees and zero unfreed blocks, including failed admission
  and operation-owned tokens/facts/readers. No target-wide memory budget is claimed.
- server-build.log / http-final.log: checked non-profiled staging server; all 147
  HTTP source/application/page/reusable/typed collection gates pass on both
  compiler targets, including native artifact execution and exact source checks.
- profile-native.csv, profile-native-facts.csv, profile-browser.dom.html and
  compiler logs retain intermediate/current stage evidence. Final ordinary rows
  are benchmark-native-final.csv / benchmark-browser.dom.html; benchmark-build.log
  confirms current checked native source with instrumentation absent.

All Pascal stayed frozen during native compiler/delegated pipelines. Every owned
compiler/test/Edge/benchmark handle has an observed terminal result; staging and
prior live servers were stopped only after their PID/path/hash were verified.
Their terminal handles were observed before bounded image-lock copy retry.
The verified replacement is live at the original firewall-authorized path:
PID 22552 / persistent exec 94077, bound to 0.0.0.0:8088, loopback health and LAN
Studio both 200. SHA256 E5898D884555213032B6351FABB1F845B1ADDAD069BD498D826DC173B552708A.
Backup: build/studio-before-source-context.exe. Only this live service remains.
No firewall change, stage, commit or sub-agent use; user staging and hello-nyx
remain intact. The full goal remains active.

Required reassessment after three successive diagnostic/index/context batches
without criterion closure: stop isolated lexer micro-optimization. Whole-command
cost includes repeated canonical design encoding, source rendering/snapshots,
candidate preparation/verification and full paired history restoration. Inspect
and measure those stages before changing them. The next integrated delivery removes
duplicate preparation/serialization while retaining fresh typed admission,
visibility of out-of-band document mutations, atomic publication/rollback and all
existing pair/draft/history/Unicode/crafted-expression gates. Repeat the identical
128/512/2048 benchmark. Budget one implementation/evidence batch, then reassess;
stop on pair corruption, stale snapshot reuse, ignored mutations or failed gates.
Do not expand scalar grammar, reset the counter or move performance to another
criterion to manufacture acceptance. Full Studio/compiler-file diagnostics/release
outcomes keep their original owners.

## Immutable paired history and whole commands — 2026-10-03

The previous goal turn made progress through operation-owned parsed contexts and
verified preservation/performance evidence. This bounded delivery followed its
whole-command reassessment under original codegen criterion 3. It changes the
integrated history/candidate path rather than continuing lexer-only experiments.
Criteria 1/2 remain accepted; criterion 3 remains open at closure counter 10.
No task, criterion or product completion credit is created from a partial gain.

Opt-in profiling now covers Apply/history as well as visual/structural commands,
checkpoint/commit, document Save/restore, workspace JSON snapshot/restore, Render/
encoding and fresh candidate validation/encoding. It identified a concrete
decision: at 512 controls, browser history costs 2166.1 ms, with 832 ms in snapshot
encoding and 1031 ms in restoration (about 86%). Native history costs 407 ms.
After implementation, the corresponding profile costs 287.3 ms in the browser
and 156 ms natively. Browser restored documents take 182.8 ms and fresh current
document encoding/rendering 103.1 ms. Workspace JSON snapshot calls are zero;
three immutable frame restorations measure below the observed browser tick.
Parent stage totals overlap children; these are diagnostic samples, not additive
exclusive costs or production observers. The define is absent from the service.

TNyxSourceCheckpoint is an immutable managed-text record captured by its workspace.
It retains exact prefix/body/suffix, canonical design and custom-frame meaning,
without holding a document, reader, renderer or dynamic array. Capture(Document)
freshly encodes/reconciles public document mutations. Capture without a document
copies the already-accepted frame only. Typed Restore copies this value; JSON
Snapshot/Restore remain the explicit unchanged recovery/interchange boundary.
Candidate preparation uses captured fields rather than escaped JSON round trips.

TNyxStudioHistory records complete pairs in one entry, eliminating allocation
between separate design/source lists. Session checkpointing uses one current
encoding rather than Save plus a second Render encoding. Detached target
documents decode/validate before either accepted owner swaps. Undo/redo remember
a freshly synchronized current pair, so out-of-band mutations cannot produce a
stale source beside a newer design. Rollback omits capturing its invalid candidate;
existing redo and exact drafts remain independent. Fresh complete reconstructed
candidate admission still precedes visual/source publication.

Undo retains the latest fifty commands within 16 MiB of logical target text bytes,
keeping one oversized previous command. Accounting carries canonical design once,
counts UTF-8 bytes natively and UTF-16 bytes in pas2js, and updates on entry changes.
This is a text-retention policy, not a process-memory measurement: allocator/VM
overhead, engine string sharing and compression are outside its byte total.
Thirteen new cases verify checkpoint lifetime after original workspace disposal,
wire equality, crafted frame/local preservation, external document mutations,
atomic rollback/redo, rejected source drafts and exact ordered fifty-entry history.

Final ordinary measurements run sequentially with compiler/other browser work
idle and no browser virtual-time budget. All original name/comment/Unicode/
expression/structural/pair/history/rejection gates pass. Source UTF-8 sizes stay
25063/98791/399991; no fixture size, grammar or gate is weakened. These costs
cover portable document/source work, not paint, trusted input or compiler latency.

| Target | Controls | Generate ms | Apply ms | Visual ms | Structural ms | Reject ms | Three history operations ms |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| Checked FPC | 128 | 16 | 31 | 125 | 172 | 16 | 32 |
| Checked FPC | 512 | 31 | 188 | 531 | 704 | 62 | 141 |
| Checked FPC | 2048 | 172 | 953 | 2375 | 3016 | 313 | 796 |
| pas2js / Edge | 128 | 24.1 | 207.4 | 463.5 | 565.5 | 54.8 | 74 |
| pas2js / Edge | 512 | 59.4 | 592 | 1572.9 | 2105.8 | 235.5 | 306.4 |
| pas2js / Edge | 2048 | 285.7 | 2581.1 | 6337.9 | 8092.6 | 878.7 | 1218 |

Previous ordinary browser history was 597.9/2228.1/10501.5 ms and visual edits
554.2/1879.1/7577.8 ms; native history was 94/422/1843 ms. The whole-command
change materially improves history and Apply. Largest visual/structural edits
still take seconds; it does not establish responsive large-project editing.

Evidence under ignored build/source-command/:

- focused-build.log / focused-run.log: 48 checked native cases, 5822172 allocated
  and freed blocks, zero unfreed. Later conditional instrumentation only changes
  diagnostic builds; the complete production pipelines execute the same behavior.
- core-final.log / shared.dom.html: 30 core + 1508 composition/designer checks on
  native FPC and executed pas2js (1538 total). Forty intended native type errors,
  55 scheduler and 60 paired-project/disk recovery cases pass. Native-emitted
  ordinary, structural, managed, legacy, edited helper/runtime and three collection
  consumers execute successfully.
- lcl-final.log: 60 actual native Studio, 40 managed controls, 35 events, 50 scalar
  bindings, 79 descriptors, 74 projections and 17 compiled callback cases.
  browser-final.log compiles the current frontend/consumers and verifies forty
  intended pas2js type errors. source-workspace-final.log rebuilds both focused
  consumers and the ordinary benchmark from current source.
- focused.dom.html executes all 48 cases. authoring-desktop.dom.html passes 64
  real Studio checks at width 1096; authoring-phone.dom.html passes 64 in the exact
  390-pixel frame. Declared markers/counts/widths are inspected, with wrapper exit 0.
- compiled-browser-build.log / compiled-generated/collections/callbacks DOMs:
  freshly emitted companions compile and execute 37 reconstruction, three
  collection and eleven real callback cases.
- server-build.log / http-final.log: current checked non-profiled staging service
  passes all 147 HTTP application/page/reusable/authored-source/typed-collection
  gates on both compiler targets, including native artifact execution.
- profile-before/after-native.csv and profile-before/after-browser.dom.html plus
  compiler logs retain the whole-command decision evidence. Ordinary final rows
  are benchmark-native-final.csv / benchmark-browser.dom.html; their real-clock
  run is sequential and every preservation gate passes.

All Pascal stayed frozen during compiler/delegated pipelines. Native pipelines
were serial; browser functional checks ran independently of HTTP, and both were
terminal before isolated timing. Every owned build/test/Edge/benchmark handle has
an observed terminal result. Staging PID 14496 / exec 43431 and old live PID 22552 /
exec 94077 were identity/hash checked and intentionally stopped. Both terminal
handles were observed before the bounded replacement-copy retry. The replacement
is live at the original firewall-authorized path, PID 26888 / persistent exec 68367,
bound to 0.0.0.0:8088. Loopback health and LAN Studio both return 200. SHA256
23E4DC2CCD43A7023E5A07C6CF7854A31EADDE1DF9541237BBDF40A1B4DCAF44.
Backup: build/studio-before-source-command.exe. Only the live service remains.
No firewall change, stage, commit or sub-agent use; user staging and hello-nyx
remain intact. The full goal remains active.

Reassessment after the bounded command delivery stops further isolated timing
experiments. Next is integrated authored-companion compiler-error navigation:
typed file/line/column diagnostics over HTTP and Nyx-built source navigation with
exact accepted-source/Unicode guards, tested failure journeys on both compilers
and both Studio target adapters. Budget one implementation/evidence delivery;
stop on wrong-source navigation or accepted-pair/draft corruption. Compiler helper
errors currently surface as plain logs. Large-project responsiveness retains the
original criterion/outcome and identical preservation benchmark. Do not transfer
scope, reset counter 10, add scalar grammar or manufacture acceptance from gains.

## Semantic agent operation and compiler navigation — 2026-10-04

Accepted task: [semantic agent operation](TODO/DONE/NS-4_agent-tools_01.md).
Pascal owns the loopback MCP listener, bounded semantic queries, typed atomic
patches, monotonic revisions, retry receipts and the ordinary paired history.
The Nyx-built Agents panel exposes access controls and retained actor activity.
The browser bridge preserves local recovery/drafts, cancels late observations
and coalesces only unsent typing of the same accepted pair. Document tokens
apply through both renderers without changing a borrowed theme; removal restores
the original palette. Eleven tools include selective immutable rendered previews.

Executed evidence under ignored `build/agents/`:

| Gate | Result |
| --- | --- |
| Shared native / actual pas2js | 30 core + 1525 composition = 1555 each |
| Portable agent core | 28 native / 28 executed browser; native zero leaks |
| Real MCP HTTP | 29, including actual Nyx-rendered PNG; zero leaks |
| Live desktop / exact390 observers | 12 browser each + 7 native coordinator each |
| Live bridge safeguards | 8 browser, cancellation/recovery/typing queue |
| Broader DOM / managed / events | 51 / 41 / 35 |
| Studio authoring | 64 desktop / 64 exact390 / 66 native |
| Focused source diagnostics | 65 native / 65 executed browser |
| Real compiler UI / MCP reports | 8 browser-output + 8 LCL-output; both live report consumers pass |
| HTTP builds / Unicode | 165 |
| Type refusals | 40 per compiler |

Typed compiler diagnostics carry severity, exact source ownership and Unicode
scalar positions. Studio's public code editor navigates those locations; pending
drafts disable stale navigation. Real compiler journeys exposed abbreviated
pas2js filenames: both provider commands now request full filenames using `-vb`.
The integration was rebuilt and rerun. An HTTP fixture initially selected the
unknown target `native`; corrected to the existing `lcl` wire name and all 165
checks passed. Full browser capture exceeded the former harness wall guard;
its bounded functional guard was extended, with no responsiveness claim.

Deployment: PID13040 / exec7825, the firewall-authorized native server path,
SHA256 `01C83BF52F1451963BE3BA165EEE6ECFB98A363D12DBFC367E24912E4779866C`.
Editor 0.0.0.0:8088; MCP 127.0.0.1:8089. Localhost and LAN return 200.
Real `codex mcp list --json` discovers enabled `nyx_studio` with the current
session endpoint. Credentials stay in ignored local configuration; unrelated
bytes survive publication. Old live/stage processes were identity-checked and
observed terminal. Backup is `build/studio-before-agents.exe`. Production was
restarted without test mutation of the user's recovered design. No staging,
commit, firewall change or sub-agent use.

Compiler navigation advances original codegen criterion 3 but does not close
broader editing UX or large-project performance; counter is 11. Original measured
128/512/2048 rows remain unchanged. MCP's six criteria are accepted on their own
evidence. Full native Studio, serialized-build observation latency, capture disk
housekeeping and production control depth remain open/documented. Latest user
request selects [touch-resizable split views and typed platform configuration](TODO/DONE/NS-2_split-platform_01.md).

## Resizable workspaces and interaction breadth — 2026-10-04

Accepted [split/platform task](TODO/DONE/NS-2_split-platform_01.md), retaining all
four original criteria. `INyxSplitView` / `NewNyxSplitView` owns two panes through
the ordinary managed component contract. Shared geometry, proportional bounds,
pointer cancellation and keyboard increments drive both adapters. The browser
uses an allocated 44-pixel semantic divider; LCL reuses panels/scroll boxes with
a focusable custom grip. Resizing keeps the mounted editor/canvas and their
focus, caret, drafts and scroll positions. Losing inherited resize permission
cancels a gesture safely. Studio consumes this public component and retains the
chosen proportion during the current session, including show/hide and optional
panels. Across-restart proportion recovery is not delivered.

Typed `Configure.ForPlatform(npfBrowser)` / `npfNativeLCL` scopes own independent
facades. Presentation/interaction overrides persist, clone, compile and reconcile
as ordinary crafted source. Applying them changes only the selected realized
view; authored defaults remain exact. Routing, identities, bindings, state and
callbacks cannot be silently overridden by a presentation scope. New enum keys
were added to source reconciliation's ordinal identity handling. Catalog totals
are 41 primitive/layout/authoring kinds (including the component reference) and
35 compound recipes: 76 specialized kinds.

The [event owner](TODO/NS-1_event-scheduler_01.md) adds 17 families, for 23 runtime
families. Canonical names drive fluent methods, metadata, codec and generated
companions. Keyboard before/main/after cycles are logical key actuation; text
admission owns complete proposed/accepted Unicode strings independently of
keyboard shortcuts. Pointer snapshots own kind/button/modifiers/control-relative
position, identity and pressure; native ordinary mouse hooks explicitly lack
browser touch/pen detail. Context menus have a sequential consumption window.
Readonly code and native links now have actual keyboard access. Framed inputs
and compound descendants route once; callback navigation invalidates remaining
phases before a borrowed supplier/widget can be used again.

Review discovered two old four-trigger descriptor ceilings and payload reuse
between phases. Both descriptor limits now follow the canonical registry while
preserving runtime/uniqueness/budget admission. The shared bridge captures each
phase's declared scalar contract afresh and reattaches its immutable input data.
The compiled fixture deliberately mixes signal-only and scalar declarations;
it verifies all key cycles and accepted/cancelled text completion. Code stays
Pascal, thoroughly commented and strongly typed at the public authoring boundary.

Executed packet under ignored `build/events/` and `build/split/`:

| Gate | Result and artifact |
| --- | --- |
| Full shared native / executed browser | 30 core + 1534 composition = 1564; `regression-final.log`, `shared-final/capture.dom.html` |
| Split portable and compiled contracts | 141 each; `server-and-split-final.log`, split consumer captures; portable 658823 allocations/frees, zero leaks |
| Actual split controls | 22 native / 22 browser; `server-and-split-final.log`, `split-permissions-final/capture.dom.html` |
| Studio split and host transitions | 20 desktop / 20 exact390 + 9 viewport cases; `browser-final.log`, `studio-viewport-final/capture.dom.html` |
| New interaction contracts | 77 native / 77 executed browser; `payload-repair/interactions.log`, `portable-browser/capture.dom.html`; 15789833 allocations/frees, zero leaks |
| Compiled interaction controls | 37 native / 37 browser; same log, `controls-browser/capture.dom.html`; own per-phase payloads and navigation |
| Existing native integration | 84 descriptors, 40 managed, 35 events, 50 bindings, 69 Studio, 75 projections, 17 compiled callbacks; `payload-repair/lcl.log` |
| Existing browser integration | 51 DOM and 11 compiled callbacks; `journey-browser` and `callbacks-browser-final` captures |
| Studio authoring | 67 desktop at width1076 / 67 in exact390 frame; `payload-repair/authoring-desktop` and `authoring-390` captures; narrow PNG inspected |
| Scheduler | 55 native / 52 browser, native zero leaks; `deploy-final.log`, `scheduler-final/capture.dom.html` |
| Intended type refusals | 42 per compiler; native `regression-final.log`, fresh pas2js `payload-repair/browser.log` |
| HTTP builds | 173 with current expanded phase contracts, isolated split/interaction companions on both outputs; `payload-repair/http.log` |

These are executed Pascal fixtures through real DOM/LCL hooks; synthetic pointer/
keyboard messages establish adapter semantics, not a physical phone or OS-input
qualification. Browser pointer capture on trusted touch is implemented; a manual
phone review remains useful. The generated compiled-browser callback harness
initially returned 404 because its optional HTML host had not been copied to
staging. Copying the existing host and rerunning passed all eleven checks; its
failed capture is retained beside the successful final artifact. No product
assertion was weakened. Core gates preceded the final focused phase-payload
repair; focused compiled, native integration, browser Studio and HTTP gates were
rebuilt and executed afterwards. No new performance qualification is claimed.

All Pascal was frozen during builds and delegated requests. Native pipelines
were serial. Every build/test/capture handle has an observed terminal result.
Staging PIDs14760/31712/28124 and old live PIDs13040/14000 were identity checked,
stopped and their terminal handles observed. Final deployment is the original
firewall-authorized server path, PID13716 / persistent exec48608, SHA256
`16692C96F0A791B81AA96465B953671B40333F77B1EC0227F4D43F542AF3DE08`.
Browser `nyx_studio.js` SHA256
`4C4050F2838C5BD6B19F975C2B65C5E634BE21DBD7D2D63CC17A8C5B31D8CBBC`.
Editor listens on 0.0.0.0:8088 and MCP on 127.0.0.1:8089. Loopback and LAN
return 200. Real Codex CLI discovery reports the enabled current session endpoint.
Backups are `build/studio-before-split.exe` and `studio-before-phase-payload.exe`.
Only production remains; no firewall change, staging, commit or sub-agent use.
User-staged `.gitmodules` and `athena` remain untouched on `hello-nyx`.

The split task closes on its own accepted evidence. Event criteria 2/4 remain
accepted; 1/3 remain open at consecutive-delivery counter2. Reassessment stops
isolated hook expansion and selects complete supported capability qualification:
the control/property/event matrix, actual declared-but-unbridged boundaries and
explicit extension admission, preserving the original criteria. Wheel/scroll,
drag/drop, selection, composition detail, worker pooling and complete native
Studio remain with their owners. Codegen's original large-project outcome and
counter11 remain unchanged. The full goal remains active.

## Property capabilities and inherited input policy — 2026-10-04

Owning criteria: NS-1 events 1/3 and NS-2 parity's matrix/input semantics, with
the shared Studio inspector as the required consumer. The previous two-delivery
reassessment selected supported-contract qualification instead of isolated hook
expansion. Concrete deliverable: one typed support schema used by the generated
catalog matrix, Studio and bounded MCP queries, plus actual correction of the
inherited scalar read-only gap. Stop conditions remained duplicate/stale routes,
silent target/policy fallback and damaged accepted design/source pairs.

`nyx.interaction` now captures immutable effective enabled/visible/read-only
policy without retaining a node. Scalar adapters, portable value admission,
binding commands and standalone compound actions use it. Read-only ancestor
protection reaches unbound targets, retains keyboard observation and allows
deliberate programmatic state changes. Refused physical drafts restore the
accepted control value before text hooks or state publication. Actual selector
widgets without standard read-only modes are documented as basic restore support.

`TNyxPropertySupport` records typed meaning, target grades and help. Creator
support and returned metadata are owned value snapshots. Platform overrides
qualify the opposite target explicitly. Ordinary/custom/unavailable effects are
distinguished; publishing metadata does not manufacture an adapter bridge. Studio
puts help on actual fields and visible unavailable-effect explanations alongside
them for touch users. `nyx_node` exact-key/paged responses include bounded
meaning/browser/native/help fields. The Pascal reference generator now emits
property and event capability tables for every default catalog kind. Review
corrected the previous record's double-counting of the component reference:
41 primitive entries already include it, plus 35 recipes makes 76 kinds.

The [capability guide](docs/capabilities.md) documents actual target fallbacks,
creator responsibilities, input policy and standards mappings. Current W3C UI
Events/Input Events/Pointer Events publications were checked: Nyx logical key
actuation stays separate from text admission; rejection is not a promise to
cancel an OS composition session. Pointer cancellation/capture, wheel units and
composition detail remain explicit required scope, not a blanket conformance
claim. Thorough comments, typed arguments and blank-above-if layout were retained.

Qualification used the existing configured FPC 3.2.0, LCL FPC 3.3.1 / Win32,
matched pas2js/rtl and actual headless Edge. Source was frozen during native
pipelines and all delegated builds. Native pipelines were serial. Commands were
`tools/build.ps1 -Target interactions|core|lcl|browser|agents|split|catalog` with
browser output staged under `build/capabilities/browser`. The two new final
native wrong-enum fixtures and updated callback fixture were compiled separately
under `build/capabilities/native`; the server used an independent
`build/capabilities/server` directory. Product web bytes were finalized by the
sequential split build before capture/deployment. Artifacts are ignored build
output; no performance measurement was inferred from these functional gates.

| Evidence | Actual result / artifact |
| --- | --- |
| Interaction/policy/support contract | 171 native + 171 executed pas2js; native zero unfreed blocks; `final-interactions.log`, `final-interactions-browser/` |
| Real interaction controls | 45 LCL + 45 DOM; `final-interactions.log`, `final-controls-browser/` |
| Core/source/history | 30 core + 1534 composition/designer on each runtime; `final-core.log`, `core-browser/` |
| Enum/type admission | 44 intended rejections per compiler; final two native enum assignments also rerun; `final-core.log`, `final-browser-build-repaired.log` |
| Callback/schema/source ownership | 84 native + 84 executed browser; native zero unfreed blocks; `final-callbacks.log`, `callbacks-browser/` |
| Native integration | 40 managed controls, 35 events, 50 bindings, 71 Studio, 17 compiled callbacks and catalog/shell/theme checks; `final-lcl.log` |
| Actual Studio | 69 desktop + 69 exact-390 browser; hints, visible explanations and unchanged source; `studio-desktop-final/`, `studio-phone-final/` |
| Browser regressions | 51 DOM journey, 48 bindings; `journey-browser/`, `bindings-browser/` |
| Split regressions | 141 portable + compiled reconstruction + 22 LCL controls; exact-390 host's 9 checks require its inner 20-check journey; `final-split.log`, `studio-split-phone/` |
| Semantic agents | 29 native + 29 executed browser; `final-agents.log`, `agents-browser/` |
| Delegated compiler service | 173 real HTTP checks, including browser/native applications, isolated views and authored companions; `final-http.log` |
| Real MCP transport | 30 checks including exact-key support query, revision guards, permissions, history and actual rendered PNG; `final-mcp-http.log` |
| Capability reference | 76 reviewed catalog kinds with typed property/event tables; `final-catalog.log`, `docs/components-reference.md` |

The first browser negative fixture run failed its diagnostic guard: overload
resolution reported the node overload instead of the intended enum family. The
two Pascal fixtures now assign strings directly to typed enums; final native
checks and all 44 browser guards passed. The first exact-390 authoring run failed
because its test helper read source on Design then searched for More properties
there. The helper now follows Inspector for that action. Final desktop/narrow
journeys pass all 69 assertions. Original failed logs and phone capture remain;
no product assertion, budget or type requirement was weakened. The creator
fixture now initializes the extended property record explicitly with Default.

Desktop/narrow screenshots were visually inspected. They demonstrate the
executed browser layouts, not physical-phone or every-widgetset approval. All
test/build/capture handles have observed terminal results. Old staging PID1884,
final staging PID4700 and old live PID13716 were identity/hash checked before
stopping, and their terminal handles were observed. Only production remains:
PID32844 / persistent exec33204 at the original firewall-authorized path,
`build/native/3.2.0/i386-win32/nyx_studio_server.exe`, SHA256
`BA0C0ED2146E653BCCE48CBD922305F1053F75DAC88B150DFC4EF40129E403DE`.
Browser `nyx_studio.js` SHA256
`B3885F0661AAF28BB990AEDD07A17F12EC5C23F6BFF682B70A2DED7C61F850EB`.
Only verified product index/runtime/Studio/preview files were copied from staging.
Backup: `build/studio-before-capabilities.exe`. Editor binds 0.0.0.0:8088,
MCP binds 127.0.0.1:8089; localhost/LAN return 200. The real Codex CLI discovers
the enabled current per-launch endpoint. Private bearer/config contents were
never printed. No firewall change, commit, staging or sub-agents; user-staged
`.gitmodules` and `athena` are untouched on `hello-nyx`.

The selected matrix/policy delivery is finished. NS-2's publication deliverable
has current consumer evidence; full parity acceptance still requires its original
renderer prerequisites and remaining criteria. NS-1 event criteria 2/4 remain
accepted and 1/3 remain open; consecutive deliveries without further original
event criterion closure: 3. Reassessment changes the next deliverable to the
named extension-event contract through both real target producers, scheduler
admission, Studio, persistence/history and compiled companions. It must qualify
owned payloads, independent registrations, mutation/navigation/disposal and
explicit target capabilities. Isolated hook expansion stays stopped. Wheel/scroll,
drag/drop, selection, composition detail, worker pooling, production component
depth and complete native Studio retain scope. Codegen's original large-project
owner/counter11 is unchanged. The full goal remains active.

## Named producers, payloads and lifecycle qualification — 2026-10-04

Selected owner: [NS-1_event-scheduler_01](TODO/NS-1_event-scheduler_01.md), original
criterion 3, with Studio and both actual adapters as consumers. The preceding
reassessment selected one integrated named-extension contract instead of more
isolated hook names. Stop conditions remained stale producers, duplicate routes,
silent policy/target fallback and damaged accepted source/design pairs.

Creators now declare named signals, scalar domains or structured data kinds with
owned help and explicit target grades. Exact typed names have independent ordered
registrations and policies. Payload admission rejects undeclared/unavailable
events and mismatched values before callbacks. Structured object fields remain an
explicit extension boundary; this is not a claim of compile-time field typing.
The physical registry remains separate from open semantic identities.

Browser and LCL contextual factories receive managed emitter ports. A port retains
its scope and origin ID, never a renderer/node/widget. Candidate scopes stay dormant
until ownership transfer, then activate against the admitted renderer; teardown
revokes them before disposing controls. Retained stale ports fail explicitly.
Scheduler UI admission refuses a real native worker before producer entry; worker
code must post to the UI queue. Existing scheduling, cancellation and input
decisions are reused rather than implemented by a second router. Disabled/hidden
ancestry suppresses notifications; read-only preserves deliberate notifications.

Studio exposes separate named event cards, payload help, ordered TODO handlers,
policies, source navigation and warned removal. Strict persistence, crafted
`.OnNamed(NyxEvent(...))` generation, typed source admission, draft protection and
paired history use the same identity. Authored-but-unpublished streams stay
discoverable with explicit missing-declaration help; metadata cannot create a
producer bridge. Actual handwritten companions compile and execute on each target.

`nyx_node` now has an event page and a separate bounded callback window within
that page. Only those exact streams' registrations are returned, including empty
authored streams and their policies. Counts and partial-descriptor flags prevent
treating a query window as a replacement descriptor. Default event/callback limits
are 32/16, with maximum 50 each. Existing response budgets remain enforced; this
does not promise that arbitrarily large domain-choice schemas fit one query.

Qualification used configured FPC 3.2.0, LCL FPC 3.3.1 / Win32, matching pas2js/rtl
and actual headless Edge. Pascal source was frozen during every native pipeline
and delegated build; native pipelines were serial. Substantive fixture generation,
capture and transport tests are Pascal. New commands include
`tools/build.ps1 -Target named-events`; existing core/LCL/browser/agents/interaction
gates were run against the changed contracts. Native and browser products were
staged separately under ignored `build/named/`. No functional count is a performance
benchmark or a claim about every native widgetset or a physical phone.

| Evidence | Actual result / artifact under `build/named/` |
| --- | --- |
| Named producers, payloads, lifetime and authoring | Native preparation 69, compiled native 71, both zero unfreed blocks; executed browser 69; `final-named4.log`, `browser-final-context/capture.dom.html` |
| Shared core/source/history | 30 core + 1534 composition/designer on each runtime, generated reconstruction and collections; `final-core2.log`, `core-browser/` |
| Strong typing | 46 intended rejections per compiler; `final-core2.log`, `final-browser.log` |
| Scheduler/UI admission | 55 native with zero unfreed blocks, 52 executed browser; `preflight/scheduler-test.log`, `scheduler-browser/` |
| Native integration | 40 managed controls, 35 event controls, 50 bindings, 71 Studio, 17 compiled callbacks, 75 catalog projections and shell/theme/identity checks; `final-lcl.log` |
| Callback/schema/source | 84 native + 84 browser; `final-lcl.log`, `callbacks-browser/` |
| Existing physical interactions | 171 native with zero unfreed blocks + 171 browser, 45 LCL + 45 DOM controls; `final-interactions.log`, `interactions-browser/`, `controls-browser/` |
| Studio visual authoring | 69 desktop + 69 exact-390 browser, both screenshots inspected; `studio-desktop/`, `studio-phone/` |
| Browser regressions | 51 DOM journey, 48 bindings; `journey-browser/`, `bindings-browser/` |
| Semantic agent metadata | 29 native + 29 browser; `final-agents3.log`, `agents-browser-final/` |
| Delegated compiler service | 173 actual HTTP checks, including both applications, isolated views and companions; `final-http.log` |
| Real MCP transport | 33, including separate event/callback windows and actual PNG rendering; `final-mcp-http5.log` |
| Final product builds | `final-server-compile4.log`, `final-studio-compile.log`; browser product hash matches inspected captures |

Failed attempts are retained. Source admission initially used a nonexistent event
value tag; it now uses the existing typed tag. pas2js required an explicitly typed
event handler and dynamic managed-interface array. A temporary fixture interface
survived raw node disposal and leaked three blocks; explicit owner release fixed
the lifetime, and final native runs report zero. New event-details presence checks
initially validated an absent default data value; the nonthrowing `Defined` query
restored compatibility, verified by the existing scheduler gates. The staging
claim fixture also had a malformed request; it now uses the actual required shape.

The first MCP render gate overlapped expensive browser captures and failed preview
completion, although a PNG was produced; its precise cause was not retained by
the original diagnostic. An isolated run subsequently passed within the unchanged
budget. Improved diagnostics initially assumed every successful content item was
text and failed on a resource link; error-text extraction is now conditional on
`isError`. Final 33-check gates pass with original assertions and budgets intact.
Whitespace-only spacing fixes preceded final server/Studio rebuilds; the inspected
browser hash stayed identical. No substantive changes followed the final gates.

All build/test/capture handles have observed terminal results. Staging servers and
the prior production server were path/hash checked before stopping. Production
alone remains: PID16484 / persistent exec58093 at the firewall-authorized
`build/native/3.2.0/i386-win32/nyx_studio_server.exe`, SHA256
`E6CC9D0DEE31F2BFD3FA3D722D54470941F2DA76B6816C2FBFF2230F58ADA932`.
Browser `nyx_studio.js` SHA256
`6907DDB90E51A28FDF925CD941A861149F853DE3138BEA2DA4AB97E01850D815`.
The verified five product web assets were copied explicitly. Backup executable:
`build/studio-before-named-events.exe`; prior web assets: `build/named/previous-web/`.
Before restart, the nonmutating claim probe reported no published editor pair;
there was no active backend pair to migrate and no design backup was fabricated.
Browser recovery/files were untouched; all mutating transport tests used staging.
Editor binds 0.0.0.0:8088 and MCP 127.0.0.1:8089. Local/LAN checks return 200,
and the actual Codex CLI discovers the enabled current per-launch endpoint.
Private bearer/config contents were never printed. No firewall change, commit,
staging or sub-agents; user-staged `.gitmodules` and `athena` remain untouched.

Original event criterion 3 is accepted for exercised Win32/LCL and pas2js; criteria
2/3/4 are accepted, criterion 1 remains open, and the no-closure counter resets
from 3 to 0. The named-producer packet is delivered; the event task and full goal
remain active. The next criterion-1 delivery should qualify integrated scroll/
wheel behavior on actual default scrollable text and collection controls,
including typed units, cancellation/propagation, viewport notifications and
Studio/source capabilities. Check current platform standards before defining
semantics; another event-name list alone does not qualify the outcome. Preserve
drag/drop, rich selection, composition detail, default compound semantic discovery,
worker pooling, production component depth and complete native Studio. Stop on
stale routes, silent unsupported policy fallback or damaged accepted pairs.
Codegen's separate original large-project owner/counter11 is unchanged.

## Wheel requests and actual viewport notifications — 2026-10-04

Selected owner: [NS-1_event-scheduler_01](TODO/NS-1_event-scheduler_01.md), original
criterion 1. The named-producer packet remains accepted. This delivery qualifies
actual default scrolling controls, shared snapshots and their authoring/agent
consumers; it does not infer criterion closure from five new names.

`nyx.viewport` provides immutable marker-backed wheel/axis/viewport records with
explicit units, fractional deltas, modifiers, physical cancelability and finite
dimension admission. Missing/default observations remain distinguishable from
zero movement. Before/main/after wheel phases reuse existing scheduler decisions,
capture each declaration independently and stop on navigation. Uncancelable host
requests cannot acquire a consumable context. Scroll observes actual local offsets
independently of wheel input and never changes the accepted design/source pair.

Actual scroll, memo, code editor/block, list, table and tree controls bridge the
contract on both targets. LCL uses a real scroll box and readonly code memo with
both scrollbars; positive horizontal wheel ticks map right and vertical ticks map
up, without a guessed pixel distance. Grid/list/native scrollbar units stay
explicit. One admitted-view observer samples subscribed controls at UI idle,
coalesces native movement and disconnects before disposal; no timer or background
polling is introduced. A callback may destroy the renderer safely. Browser hooks
retain actual DOM delta units, are explicitly non-passive where cancellation is
possible, use the true input face and revoke detached-element listeners. Fixed
height collections/text retain overflow through synchronization. Browser scrollend
is the actual supported host event; native completion is explicitly Missing.

Studio exposes the same metadata, policies, TODO/navigation and paired history.
Authored/generated fluent callbacks reconstruct and compile on both targets.
Source admission now derives method names from the canonical runtime registry,
removing a separate hardcoded recognition chain. The registry has 28 runtime
families, separate from named semantic identities. The Pascal reference generator
republished the 76-kind support matrix, also consumed by bounded MCP queries.

Current standards were checked against primary W3C wheel and CSSOM View scrolling
definitions before choosing units, cancellation and completion semantics. The
Pointer Events Level 4 source was a Working Draft, not a blanket conformance
claim. Detailed mappings and public contracts are in [events](docs/events.md).
Qualification uses configured FPC 3.2.0, LCL FPC 3.3.1 / Win32, matching pas2js/rtl
and actual headless Edge. Source was frozen during native/delegated pipelines;
native pipelines were serial. Substantive generation, capture and transport
fixtures are Pascal. No count below establishes performance, every widgetset or
physical-phone behavior. The `viewport` build target is included in `all`; this
packet ran the relevant targets individually, not the whole `all` command.

| Evidence | Result / artifact under `build/viewport/` |
| --- | --- |
| Typed snapshots, actual controls, authoring and lifetime | Native preparation/compiled 46 each, both zero unfreed blocks; `final-viewport4.log` |
| Executed browser viewport | 50, including actual host scroll/scrollend and detached-hook revocation; `browser-final/capture.dom.html` |
| Existing interactions | 186 native with zero unfreed blocks + 186 browser, 45 LCL + 45 DOM controls; `final-interactions4.log`, `interaction-browser/`, `interaction-controls-browser/` |
| Native shared model/source/history | 30 core + 1534 composition/designer, 55 scheduler, 60 paired project checks, generated reconstruction and 3 collections; `final-core.log` |
| Strong typing | 48 intended rejections per compiler, including raw wheel/viewport units; `final-core.log`, `final-browser.log` |
| Native integration | 40 managed controls, 35 event controls, 50 bindings, 71 Studio, 17 compiled callbacks, 75 catalog projections and shell/theme/identity; `final-lcl.log` |
| Callback/schema/source | 84 native + 84 executed browser; `final-lcl.log`, `callbacks-browser/` |
| Studio visual authoring | 69 desktop + 69 exact-390 browser; both screenshots inspected; `studio-desktop/`, `studio-phone/` |
| Generated capability reference | 76 kinds; `final-catalog.log`, `docs/components-reference.md` |
| Delegated compiler service | 173 actual HTTP checks with both targets/applications/isolated views/handwritten companions; `final-http4.log` |
| Real MCP transport | 33 including bounded metadata, transactions/history and actual PNG rendering; `final-mcp-http.log` |
| Final browser/server product builds | `final-browser.log`, `final-server.log`; deployed JS matches inspected artifact |

Failed attempts are retained in ignored build logs. A misplaced forward declaration
and missing native type import were corrected. Initial fixture assumptions about
callback APIs and nested input faces were corrected against actual public contracts.
Moving a memo caret did not prove scrolling; its native fixture now sends the real
scrollbar message. Native code had no scrollbars, and browser table synchronization
cleared its scrollable display; both were product defects fixed before final gates.
The source reader initially rejected the new fluent methods; canonical registry
lookup fixed that drift. Existing metadata checks were updated to exact 28/24
counts with explicit support grades, retaining their assertions.

An early staged server rooted at the real workspace replaced its managed Codex
configuration. The exact production block was privately restored, retaining other
configuration; final staging used an isolated root/junctions and private config.
An empty staged output profile first rejected builds. The existing production
profile was copied read-only to staging. The HTTP fixture then looked for native
artifacts under the wrong working directory. Running the unchanged executable in
the isolated server root corrected that setup; final 173 checks execute actual
artifacts without weakened assertions. MCP ran alone after costly captures/builds
completed and passed within the unchanged preview budget. During deployment the
first executable copy raced process teardown; after observing the old terminal
result, copying and verifying the final hash succeeded. No substantive Pascal
change followed the final gates.

All build/test/capture handles have observed terminal results. Staging PID23260 /
exec36246 and old production PID16484 / exec58093 were identity-checked before
stopping and their termination was observed. Only production remains: PID19588 /
persistent exec10835, firewall-authorized
`build/native/3.2.0/i386-win32/nyx_studio_server.exe`, SHA256
`D09392BFF2FADA5C038E7D817D7E5530C207FDAB15BE5D93FCF9CE30CBDA43C0`.
Browser `nyx_studio.js` SHA256
`0427A2FBAD75926530DC4FD689DF8D5F2AC89B87C7E08757EB2ED3D187FF7B8D`.
Five verified web assets were explicitly published; backups are
`build/studio-before-viewport.exe` and `build/viewport/previous-web/`. The
nonmutating pre-restart probe found no published backend editor pair; no design
backup was fabricated. Browser recovery/files were untouched, and all mutating
transport checks used staging. Local/LAN HTTP return 200, LAN-served JS has the
same exact hash, and actual Codex CLI discovery reports the enabled new production
loopback MCP endpoint. Editor binds 0.0.0.0:8088, MCP 127.0.0.1:8089. No private
bearer/config contents were printed; no firewall change, commit, staging or
sub-agents. User-staged `.gitmodules` and `athena` remain untouched.

Original event criteria 2/3/4 stay accepted; criterion 1 remains open. This packet
advances its consecutive deliveries without further original closure from 0 to 1.
Continue integrated rich selection/default semantic discovery through real
collection controls, reusable recipes, source/Studio and agent consumers. Preserve
native completion, drag/drop, composition detail, pointer cancellation/capture,
production worker pooling, component depth and full native Studio. Stop on stale
routes, duplicate dispatch, silent unsupported policy fallback or damaged accepted
pairs. Codegen's separate original large-project owner/counter11 is unchanged.
The full goal remains active.

## Managed rich collection selection — 2026-10-04

Owning acceptance: original event/scheduler criterion 1, with existing accepted
criteria 2/3/4 reused. Deliverable: typed multiple selection through actual
collection controls, reusable ownership, snapshots, Studio/source and agents.
Stop conditions were duplicate publication, stale receivers, silent target
fallback or damaged accepted pairs. The implementation is deployed; the full
criterion and full goal remain open.

`nyx.collections.selection` supplies indexed immutable managed membership,
independent focus/anchor, typed selection modes/actions and retained value event
snapshots. Candidate scope/existence/duplicate admission is atomic. Moves preserve
identity; removals prune membership and repair the cursor without selecting it.
Single mode preserves the canonical v1 specification; explicit Multiple uses v2
inside the unchanged design-v3 envelope. OnSelectionChange is the 29th runtime
family and describes accepted publication, with semantic compound origin/source,
multiple registrations and the existing scheduler. The matched pas2js compiler
cannot put a COM interface inside an event record; immutable value snapshots
accommodate that restriction while the live view remains interface-based.

Actual LCL list/grid/tree and browser list/grid/tree support modifier-assisted
navigation, ranges, toggles, select-all and tree expansion/parent/child movement.
Closed tree descendants are excluded from navigation. Canonical before-key
consumption suppresses defaults; disabled refuses gestures and read-only allows
selection. Teardown revokes borrowed observers/hooks before controls. Native grid
window construction no longer publishes an unsolicited initial selection.
Browser row clicks retain an actual editor's focus. Refresh compares accepted
typed scalars per cell, preserving drafts through selection and unrelated updates;
no-op/rejected edits normalize only their addressed cell. Undefined/foreign edit
identities reject without a secondary refresh error. Studio's actual Bindings
choice authors the enum, retains it through column/scope edits and exactly restores
the design/source pair through history. Events/TODO source navigation and bounded
agent discovery consume the same schema.

| Evidence | Result / artifact under `build/selection/` |
| --- | --- |
| Selection model, actual controls, Studio/source/agent and disposal | 121 native preparation + 121 compiled companion, each zero unfreed blocks; `final-selection10.log` |
| Executed crafted browser selection companion | 136; `selection-browser-qualified/capture.dom.html` |
| Actual collection authoring/Studio | 27 shared / 29 native, zero leaks; `final-authoring-qualified.log`; 27 shared / 30 browser, `authoring-qualified/capture.dom.html` |
| Target edit rejection and existing collection controls | 27 native, all 278034 allocated blocks freed; `final-collection-controls-qualified.log`; 32 shared / 29 browser in exact 390 viewport, `collection-controls-qualified-2/capture.dom.html` |
| Interaction regression | 189 native + 189 browser, 45 actual controls per adapter; `final-interactions-qualified.log`, `interactions-qualified/`, `interaction-controls-qualified/` |
| Viewport regression | 50 executed browser, `viewport-qualified/`; preceding final native preparation/compiled 46 each with zero leaks, `final-viewport-2.log` |
| Shared model/source/history and existing native consumers | 30 core + 1534 composition/designer, 55 scheduler, 60 paired projects, generated consumers; `final-core.log`; 84 callback/source, 40 managed, 35 event controls, 50 bindings, 71 Studio, 17 compiled callbacks and 75 catalog projections, `final-lcl.log` |
| Strong typing | 50 intended refusals per compiler, including raw selection mode/action; `final-core.log`, `final-browser.log` |
| Actual Studio layout/resize | 25 desktop layout checks, all child layout checks in exact 390 viewport plus 21 resize checks; `studio-desktop-qualified/`, `studio-phone-qualified/`; both screenshots inspected |
| Canonical capability reference | 76 kinds, `final-catalog-qualified.log`, `docs/components-reference.md` |
| Delegated compiler builds | 173, `http-qualified-2.log`; actual application/page/component/handwritten consumers on both targets |
| Real MCP HTTP transport | 33, including bounded metadata, transactions/history and actual PNG preview; `mcp-qualified.log` |
| Qualified product bytes | Final server/browser compilation logs end in `qualified.log`; only five explicit web assets were published |

Broader shared/negative gates preceded the last per-cell normalization and
malformed-identity guards. The affected actual native/browser collection paths
were rebuilt and executed afterward; unrelated model/type contracts were not
needlessly repeated. A single whitespace addition above an if followed deployment;
no behavioral product edit followed the final consumer gates.

Failed attempts remain in ignored logs. The native editor requires the actual F2
activation path, and horizontal caret movement must be tested inside text because
LCL uses boundary arrows for grid navigation. Browser key-event construction and
details/focus options required local typed wrappers around missing matched Web
declarations. Closed details can retain client rectangles, so actual closed
ancestor state is tested. The new Inspector command initially used the wrong
physical event route; it now follows the select control's change route. A global
event fixture incorrectly attached the selection producer to a memo; it now uses
an actual bound list while keeping the exact memo/code assertions.

Actual editing exposed two adapter defects: row clicks stole input focus, and
selection refresh overwrote drafts. Per-cell synchronization and editor focus
retention fix both. The broader reorder fixture then clicked a row background
after focusing its input; it was corrected to click the actual editor and assert
focus before and after reorder. Initial authoring compiler paths treated the
Lazarus root as an executable, and an attempted browser compile used a native-only
program; both orchestration mistakes were corrected using existing entry points.
The first isolated HTTP run lacked its machine output profile. The existing
production profile was read-only copied through staging's configuration API;
the unchanged HTTP fixture then passed. The real MCP run ran alone after costly
captures/builds. No compiler/dependency installation or dependency source edit.

The production pre-restart probe returned missing-project/400, so no published
backend pair existed and no design backup was fabricated. Browser recovery and
user files were untouched; mutating transport checks ran only in the isolated
root. Old staging PID14788/exec7004, final staging PID33128/exec80471 and old
production PID19588/exec10835 were identity checked and their termination observed
before copies. Only production remains: PID22768, persistent exec73371,
`build/native/3.2.0/i386-win32/nyx_studio_server.exe`, SHA256
`77AE2611BA893E443C955329400EC8F700BEAD69B8289F4E684492F50957F3A5`.
`nyx_studio.js` SHA256
`57B8953F56D565F6E0B42FFB55FE8691B93A2A5927702B61CA17A385BA852001`;
`nyx_studio_preview.js` SHA256
`8CEF43B88A367EDEAA2EAE70A905ADCD01220D682B6028743B3C81D7927F99B1`.
Backups: `build/studio-before-selection.exe`, `build/selection/previous-web/`.
Local health/LAN editor/LAN JS return 200; LAN bytes match the qualified hash.
Codex CLI discovers the enabled fresh loopback MCP endpoint. Editor binds
0.0.0.0:8088 and MCP 127.0.0.1:8089. Private bearer values stay in ignored
configuration/output. No firewall changes, commits, staging or sub-agents;
user-staged `.gitmodules` and `athena` remain untouched.

Qualification is Windows/LCL and matching pas2js/Edge. Browser dispatch fixtures
are genuine DOM paths with synthetic input, not physical-device input. The chosen
modifier-assisted selection model follows the cited W3C listbox/tree/grid
guidance; type-ahead, paging, full grid cell navigation, assistive technology,
physical-phone qualification and production virtualization/performance remain
open. No blanket standards conformance is inferred.

Checkpoint reassessment: original event criteria 2/3/4 remain accepted, criterion
1 remains open, and its no-closure count advances from 1 to 2. Both recent packets
delivered integrated usable behavior, but neither completes full supported schema
and default semantics. The next batch changes to full-catalog recipe semantic
discovery through existing physical routes, typed contracts and producer lifetimes.
Inventory all default routed names/payloads, then complete reused/overridden
control, Studio/source/history and bounded agent discovery as one deliverable.
Do not add another isolated physical trigger packet or reset the counter by
renaming work. Preserve all original requirements, drag/drop, composition detail,
pointer capture/cancellation, native completion, complete keyboard/accessibility
qualification, workers and full native Studio. Codegen's separate original
large-project counter 11 is unchanged. The full goal remains active.

## Default compound semantic discovery — 2026-10-04

The counter-2 bounded packet is delivered under original event criterion 1.
`TNyxSemanticEvent` provides 38 closed action names; `NyxSemantic` produces a
typed event reference. Every default recipe now uses that vocabulary. Generated
Pascal uses enums for these built-in names and preserves exact handwritten
`NyxEvent` spellings at reconciliation. Open application/creator names remain
distinct event references. Both compilers reject the wrong semantic enum family.

All 35 default compounds expose their 55 expanded physical action routes. The
canonical event schema owns immutable copies of origin/source/value identities,
trigger, payload domain, optionality and target support. Discovery realizes the
containing view, preserving sibling value sources, reusable definitions,
instance/part overrides and nested-compound boundaries. Renamed routes update
their stream; removed producers leave authored callbacks as explicit custom
requirements. Several producers can share a name without duplicate routes;
incompatible payloads retain their individual route contracts. The runtime uses
the same value-contract resolver. Inferred aliases never authorize creator Emit.
Optional declared creator payloads retain truthful absent-value flags.

Studio consumes the authored selection and document directly, displays semantic
cards before physical families, and shows bounded route/payload help. Multiple
callbacks, TODO/source navigation, policies, warned removal and paired undo/redo
use existing commands. Callback buttons now stack within their card; Remove has
enough width to remain on one line at desktop and phone sizes. The exact native
shell width and browser geometry are exercised. `nyx_node` pages routes separately
from exact events and callbacks, with totals/partial flags and a maximum of 50.
Inherited instance registrations use the same surrounding view context.

Accepted evidence in `build/semantics/`:

- `reuse-qualified.log`: 583 native preparation and 584 compiled companion
  checks; both report zero retained allocations. Every actual recipe action,
  independent reusable renamed/inherited routes and exact Unicode values reach
  authored/compiled callbacks once, with metadata-matching snapshots. Unmount
  cancels queued callbacks. Crafted companion: `lcl/nyx.semantic.fixture.pas`.
- `semantic-browser-qualified/capture.dom.html`: the compiled browser companion
  executes 582 checks through actual DOM/control paths.
- `studio-desktop-qualified/` and `studio-phone-qualified/`: 15 actual Studio
  Events/source/policy/removal/history journeys each at 1076 and exact 390-pixel
  widths. Both captures were inspected; the final Remove layout is readable.
- `named-qualified.log` and `named-browser-qualified/capture.dom.html`: creator
  producer preparation/compiled native checks 69/71, with zero leaks; compiled
  browser checks 69. This retains the distinction between discovered physical
  aliases and admitted custom producers.
- `core-fixtures-3.log` and `core-browser-qualified-2/`: 30 core and 1534
  designer/composition checks execute on both runtimes. `core-qualified-2.log`
  and `browser-regression.log` retain 51 intended wrong-type rejections per
  compiler. Source fixtures were updated to keep a genuinely open business
  event and to reject an invalid typed semantic constructor.
- `lcl-qualified.log`: 84 callback/source, 42 managed-control, 35 event,
  50 binding, 71 native Studio and 75 catalog-projection checks, plus actual
  compiled callbacks, Unicode/theme/identity, derivation/reuse and optional
  output cases. These broader checks are reused after the final Inspector
  geometry-only adjustment, which has direct current journey evidence above.
- `nyx_scheduler_tests-qualified.log`: 55 scheduler checks; paired project and
  generated/collection companions also execute, with zero retained allocations.
- `http-qualified.log`: 173 actual HTTP checks, including 40 delegated artifact
  jobs across application/page/component and handwritten/collection/callback
  companions on both targets; zero retained allocations.
- `mcp-release-qualified.log`: 38 actual transport checks on the final server
  executable, including bounded route schemas/queries, revision-aware alias
  edits and paired undo, permissions, origin checks and a real PNG; zero leaks.
- `catalog-final.log` and `docs/components-reference.md`: the Pascal reference
  generator publishes all 76 kinds and their default route/payload metadata.

Failed qualification attempts remain in the artifact directory. They include
overly narrow reusable identity lookup (fixed to resolve the realized design
identity), asking a framed container for its input (fixed through the public
browser InputFor accessor), source expectations that had become built-in semantic
names, an unopened phone panel/direct-textarea assumption in the Studio fixture,
and an old incorrect capture marker. No invalid assertion was simply suppressed;
the replacement fixtures preserve the intended admission/interaction boundary.
The first readable callback layout still wrapped Remove; the final 112-pixel
button has fresh native and both-width browser evidence.

Production is PID **36000**, exec **30995**. Editor binds 0.0.0.0:8088; MCP binds
127.0.0.1:8089. The final executable SHA256 is
`569F77DD5146DDE1CF9BA288821F47B61D9EDC5EF5FD49E97C9C5E32E40BB3D6`.
`nyx_studio.js` SHA256:
`1274B43E6EB1E8FFE065E1FBC7BFB9537C15EE1DCB89DC3B7410675BB5DA167C`.
`nyx_studio_preview.js` SHA256:
`EEE57A3414600CE6A9B8911E97FAF3ECBA20AF5146557B1B55ECDA49F40A0ACD`.
Backups: `build/semantics/previous-server.exe` and `previous-web/` with exactly
the five product assets. The production claim probe confirmed no shared design
before stopping the identity/hash-verified old process; no test/default project
was imported. Browser-local projects can reclaim their accepted pair. Health,
local/LAN editor and both served JS hashes pass; Codex CLI discovers one enabled
fresh loopback endpoint. Private tokens remain in ignored config/output. The
isolated final stage (PID 13700, exec 39124) was stopped and terminal observed.
No firewall/dependency changes, commits, staging or sub-agents. User-staged
`.gitmodules` and `athena` remain untouched.

The user's standards requirement remains broader than this packet. Primary
references checked on 2026-10-04 are [UI Events](https://www.w3.org/TR/uievents/)
(Working Draft, 2026-02-21), [Input Events Level 2](https://www.w3.org/TR/input-events-2/)
(Working Draft, 2026-05-01), and [Pointer Events Level 4](https://www.w3.org/TR/pointerevents4/)
(Working Draft, 2026-08-26; the specification links Level 3 as its latest
Recommendation). The browser's legacy keypress is deprecated; editing uses
beforeinput/input with IME-specific ordering and cancelability. Pointer capture
has distinct acquisition/loss notifications. Those details guide remaining
implementation; this packet does not claim broad standards conformance.
Programmatic DOM and Windows/LCL journeys do not establish physical keyboard,
IME, touch hardware, assistive-technology or other-widgetset qualification.

Reassessment: original criteria 2/3/4 remain accepted; criterion 1 stays open.
The consecutive no-closure count advances from 2 to **3** because recipe discovery
is delivered without closing the broader interaction requirement. The next
usable outcome is typed editing-session intent, selection and composition
lifecycle through real text controls and Studio/source/agent consumers, with
honest platform grades and owned Unicode snapshots. Use current input-event
ordering/cancelability and supported LCL hooks; preserve pending drafts and
accepted pairs. Stop on premature composition publication, fake cancellation,
stale receivers or lost source/history. Preserve all remaining drag/drop,
pointer capture, keyboard/accessibility, native completion, workers and native
Studio requirements. Codegen criterion 3 remains at counter 11. No renamed
counter, transferred/weakened criterion, DONE move or full-product credit.

## Typed editing sessions — 2026-10-04

Owner: original event criterion 1, with accepted scheduler/lifetime/source
criteria 2/3/4 reused. The selected counter-3 deliverable is integrated and
deployed; the original broad criterion remains open. The user's standards
requirement applies across the library rather than only the example control.

`nyx.editing` supplies immutable owned editing snapshots, 46 closed Input Events
intentions plus Unknown, optional data, composition phase and checked scalar
selection. Unknown wire names remain diagnostics. Full Unicode validation and
checked UTF-16 conversion reject malformed suffixes, surrogate splits and invalid
ranges; offsets are Unicode scalars, not graphemes or normalized model positions.
The public event context and specialized fluent callbacks carry this contract.
Existing handwritten companions acquire the public editing import when Studio
adds handlers; untouched legacy creator metadata safely reports no new contexts.

Browser adapters capture actual beforeinput/input/composition/select events.
Only genuine cancelable pre-edit requests outside composition expose an active
sequential consumption window. Portable text proposal rejection is a separate
contract. Composition drafts survive refresh and bypass shortcuts; final model
admission occurs once, while end snapshots retain the physical result even after
refusal. View disposal and queued cancellation preserve owned observations.

Win32 LCL chains original control message handlers for genuine IME observations.
END is drained at UI idle because committed characters can follow the message.
Selection uses checked physical UTF-16 positions, including CRLF, coalesces at
idle and reports unknown direction. Requests for a directed active endpoint fail
before altering selection; the basic setter accepts None/Unknown without focus
or scroll. Other widgetsets need a bridge. Physical pre-edit is unavailable in
LCL rather than inferred from keys. A discovered native navigation defect is
fixed by revoking Nyx callback slots immediately and releasing active controls
through the LCL queue after the current platform message finishes.

Studio exposes five new event cards with canonical capability help, named fluent
handlers, TODO source navigation, multiple registrations, policies, warned removal
and exact paired history. MCP pages publish bounded context declarations from
the same schema. The regenerated reference covers all 76 catalog kinds.

| Final evidence | Result / artifact under `build/editing/` |
| --- | --- |
| Checked native editing, actual controls, disposal, legacy compatibility and compiled companion | 117 preparation / 118 compiled; both zero unfreed blocks; `editing-release-qualified.log` |
| Executed compiled browser editing companion | 121; `editing-release-browser/capture.dom.html` |
| Actual Nyx Studio Events/source/history at desktop and exact 390 pixels | 29 each; `studio-release-desktop/`, `studio-release-phone/`; both PNGs inspected |
| Shared core, source/metadata, scheduler and paired project recovery | 30 core / 1534 designer, 55 scheduler, 60 paired; generated/collection consumers pass; `core-final-qualified.log` |
| Executed browser shared core | 30 core / 1534 designer; `core-release-browser/capture.dom.html` |
| Interaction regressions | 204 contracts plus 45 actual controls per target; `interaction-regression.log`, `interactions-browser-qualified/`, `interaction-controls-browser/` |
| Native managed controls, events, bindings and Studio authoring | 42 / 35 / 50 / 71; 75 projections and 17 compiled callback controls; `lcl-release-qualified.log` |
| Strong typing | 54 intended wrong-type refusals per compiler, including intent/phase/direction; `core-final-qualified.log`, `browser-release-build.log` |
| Delegated application/page/reusable and exact companion jobs | 173 HTTP checks, 40 reported output jobs plus compiler-diagnostic failure jobs; `http-release-qualified.log` |
| Actual final-executable MCP transport and selective PNG rendering | 44; `mcp-release-isolated-fresh.log` |
| Exact deployment manifest | Executable plus five product assets with lengths/SHA256; `release-manifest.json`; all LAN-served asset hashes match |

Qualification failures remain in ignored logs. The native text-navigation failure
was a real LCL lifetime defect and received the deferred-release fix. A mobile
test initially remounted the source pane before checking its caret; checking
focus before returning to Inspector corrected the harness. The first MCP preview
timed out while concurrent captures ran; a fresh isolated session passed the
unchanged 20-second gate. A retry against an already mutated fixture also refused
its duplicate creation, so qualification used a clean isolated session. Neither
failure weakened a product assertion. No physical IME or assistive-technology
qualification is claimed from injected DOM/Windows messages.

Production: PID **32900**, persistent exec **4085**, firewall-authorized existing
executable path; editor 0.0.0.0:8088 and MCP 127.0.0.1:8089. Executable SHA256:
`945AACE61A3C6A149AE3B53411B83CDB55DE6C095717B21B843376B348C25626`.
Main JS: `0EF32587FB2BB17B26A8A42A85D2AF0EB542E5BD301629554188108FF935389A`.
Preview JS: `0753CC47B2D57FCCACD6BAC3C84AAC10671EAB93F91CE5758DCC5C7995984FBA`.
Backups: `previous-server.exe`, `previous-web/` containing exactly five assets.
The immediate pre-restart claim probe confirmed no backend-owned editor pair;
no fixture or default project was imported. Browser-local projects can reclaim
their accepted pair. Local/LAN health and editor responses are 200; all five
served assets match. Codex CLI discovers the enabled fresh Nyx loopback endpoint;
private capabilities remain in ignored files. Staging PID 11668 / exec 78689 and
old production PID 36000 / exec 30995 were identity checked before stopping and
their termination observed. Every compiler/test/capture pipeline is terminal.
No firewall, dependency, commit, staging or sub-agent changes; user-staged
`.gitmodules` and `athena` remain untouched.

Primary standards and native bridge references are documented in
[events](docs/events.md#standards-baseline-and-open-qualification). UI Events,
Input Events Level 2 and Pointer Events Level 4 were checked on 2026-10-04;
their cited editions are Working Drafts, not blanket conformance evidence.
Physical IME/keyboard/touch, accessibility, other widgetsets, pointer capture,
drag/drop, native completion, worker pooling and complete native Studio remain
owned open requirements.

Reassessment: criteria 2/3/4 stay accepted; criterion 1 stays open. Consecutive
deliveries without original closure advance from 3 to **4**, with no reset or
credit transfer. The bounded editing outcome is finished. Next deliver complete
pointer-gesture ownership and typed drag/drop across reusable controls, actual
targets, Studio/source/history and bounded agents. Require capture acquisition,
loss/cancellation, owned transfer context, explicit unsupported grades and safe
navigation/disposal. Stop on leaked capture, duplicate delivery, stale receivers
or damaged accepted pairs; reuse applicable editing/semantic evidence. Full
standards/device qualification remains required. Codegen's original criterion 3
retains counter 11 and large-project/source UX scope. The full goal stays active;
no task moves to DONE or full-product acceptance is claimed.

## Typed gestures — 2026-10-04

Original owner is event criterion 1; accepted scheduler/lifetime/source criteria
2/3/4 remain applicable. The selected pointer-ownership/drag outcome is integrated
and deployed. Full supported-control/device/accessibility qualification remains
open, so the consecutive no-closure counter advances from 4 to 5. The next
integrated outcome and strict stop conditions are at the top of this file.

`nyx.gestures` provides immutable bounded transfer snapshots, distinct checked
format references, exact Unicode, present-empty/absent distinctions, owned file
metadata, protected access and closed operation/phase/request enums. Ten appended
triggers retain old ordinals. Specialized managed configuration mirrors the typed
source/target/touch methods; the maintained Pascal facade generator produced the
includes. Crafted generation, source admission, persistence, Studio and MCP all
consume the same public types and canonical capability metadata.

Active sequential callback responses negotiate only their physical window.
Last valid ordered request wins; failure does not suppress siblings, cancellation
revokes authority and retained/queued/worker contexts cannot respond later.
Browser capture acquisition/loss and cancellation are distinct. Opted-in browser
drag targets expose protected hover versus readable drop, with explicit operation
acceptance and canceled unsafe defaults; Nyx never automatically removes sources
or follows URI/markup data. Source identity is unknown when the host supplies none.
Native internal transfers use owned LCL drag objects; source-progress and external
OS file drops are unavailable. Read-only policy refuses editing and Move offers.
Public refusal diagnostics own their identities/text and retain no widgets.

Native capture hooks chain original handlers. Navigation revokes Nyx slots and
retirement keeps controls under an independent hidden host until the current drag
constructor/message finishes. Queued cancellation/release owns a bounded frame
lease, safely supports reentrant message pumping and survives closing the caller's
form. Capture loss alone does not invent cancellation.

| Final evidence | Artifact under `build/gestures/` |
| --- | --- |
| Native actual gesture contracts/controls/navigation and compiled companion | 83 preparation / 84 compiled, both zero leaks; `integrated-build.log` |
| Executed compiled browser, response ordering/lifetime, navigation and detached controls | 78; `compiled-browser-lifetime-qualified/` |
| Actual browser host-generated mouse/touch capture, cancellation and drag negotiation | two acquisitions/losses, two outside moves, touch move/cancel, start/hover/drop/end; `physical-browser-final/result.json`; inspected PNG |
| Actual Nyx Studio Properties/Events/source/history | 52 each desktop and exact 390; `studio-desktop-final/`, `studio-phone-390/`; inspected PNGs |
| Shared core/designer/source/history, scheduler and paired recovery | 30 / 1537, 55 scheduler, 60 paired, generated consumers; `core-qualified.log` |
| Executed browser shared core/designer | 30 / 1537; `core-browser-qualified/` |
| Interaction/capability regressions and compiled actual controls | 234 contracts / 45 controls per target; `interactions-final-qualified.log`, `interactions-browser-qualified/`, `interaction-controls-browser-qualified/` |
| Strong typing | 59 intended refusals per compiler; `core-qualified.log`, `browser-qualified-build.log` |
| Native existing managed/event/binding/authoring projections | 42 / 35 / 50 / 71, 75 projections, zero leaks; `lcl-regression.log` |
| Editing navigation/regression after retirement correction | 117, all 3508982 blocks freed; `editing-regression-test.log` |
| Delegated browser/native application/view/companion compilation | 177, including all gesture declarations/settings; `http-final-qualified.log` |
| Real final-executable MCP and selective rendering | 55, pointer/drag contexts and PNG; `mcp-final-qualified.log` |
| Exact executable plus five served product artifacts | `release-manifest.json`; local/LAN bytes match |

Failures were investigated rather than treated as acceptance. LCL constructs its
drag performer after OnStartDrag returns; inline cancellation or parentless
retirement broke that construction. The independent host/deferred cancellation
fix addresses this. Idle-only retirement leaked 394 blocks in an otherwise passing
editing journey; bounded queued release restores zero leaks. Synthetic browser
transfers cannot prove physical drag-store authority, so a Pascal driver uses
host-generated mouse/touch and intercepted real drag data without JavaScript eval.
Mobile harness navigation and incorrect capture URL/marker arguments were corrected.
The HTTP harness now runs from its isolated server root so native artifact paths
resolve correctly. A wrong-type fixture's missing identifier was corrected to a
real wrong enum; outdated capability/count assumptions now assert actual grades,
including unavailable native source progress. No product assertion was weakened.

Standards are the current Pointer Events Level 3 Recommendation and HTML Living
Standard drag model, checked 2026-10-04 and linked in docs/events.md. Host input
injection qualifies those browser paths, not physical hardware/IME/assistive
technology or another native widgetset. The generated reference covers 76 kinds.

Production PID **25544**, persistent exec **37288**, existing firewall-authorized
path, editor on 0.0.0.0:8088 and MCP loopback:8089. Executable SHA256:
`4AA4AF14F4F3747E82AAA302168EFE754B941315DC68B53586544DE3029C7CC5`.
Main JS: `1BB9F2C3E51D3598A449562C91749A0F2BF333D9C4D1BFEC1E58788F6F6754B0`.
Preview JS: `399D9959E8C426D2E9BF555DD6EA9B97D8D88DE7B8605D1A965D915B111C436C`.
Backups are previous-server.exe and previous-web/. The immediate pre-restart claim
probe confirmed no backend-owned editor pair; no fixture was imported into the live
editor. Local/LAN health and five exact asset hashes pass. Codex lists Nyx enabled
with the fresh endpoint; credentials remain ignored. Old production 32900/4085,
static staging 36204/53251 and qualified staging 672/93922 were identity checked
before stopping; termination was observed. All build/test/capture pipelines are
terminal. Only production remains. No dependency-source or firewall changes.

The user subsequently authorized a remote checkpoint and direct Codex MCP setup.
The existing staged Athena pin is already published upstream and is included
unchanged in that checkpoint. Semantic MCP is the primary demo/design workflow;
physical browser/LCL harnesses remain selective behavior evidence. Configuration
is enabled, but this chat currently has no direct Nyx handles. Finish supported
Codex connection reload/tool discovery, exercise small real semantic calls, and
record tool gaps before returning to the criterion-5 outcome. No sub-agents.

## Codex MCP registration — 2026-10-04

The user's immediate request is delivered: Nyx is registered in the selected
Codex user configuration as well as the trusted project. Per-session endpoint and
bearer credentials refresh on each Studio launch through one protected block
publisher. Explicit enrollment records only an absolute config path in ignored
local configuration. Unrelated bytes and exact backups are retained; unmanaged,
duplicate/malformed markers, invalid enrollment and remote transport are refused.
The native developer client negotiates real MCP, prints bounded structured
results, discovers an exact tool schema, identifies its actor and closes its
session. It never claims/replaces a design or retries a mutation implicitly.
Mutations retain the existing server revision and operationId contract.

Evidence under ignored `build/mcp-client/`:

- `mcp-client-build.log`: maintained build target and 22 configuration checks.
- `config-heap.log`: 1879 allocations/frees, zero unfreed blocks; exact Unicode
  unrelated bytes, backup/rotation, unmanaged/quoted/inline ownership, malformed
  marker/referral and remote-endpoint refusal checks.
- `mcp-qualified.log`: all 55 real HTTP/MCP checks against the revised executable,
  authenticated through an enrolled user configuration; actual PNG rendering.
  Fixture-only staging service 23892/99206 is stopped and termination observed.
- `codex-discovery.log` and `codex-discovery-after-rotation.log`: fresh installed
  Codex app-server initialization from another project authenticates `bearerToken`
  and all 11 tool schemas, before and after real production credential rotation.
  This diagnostic starts no model turn, agent or thread and closes its owned
  process. It does not replace or restart the desktop's existing app-server.
- Live session, bounded five-child outline, three-item catalog, two-key/two-event
  node context and eight-line source window succeed through the semantic client.
  A guessed source `limit` was correctly refused; its advertised `line`/`count`
  schema corrected the request. No live demo fixture was loaded or existing
  design content changed by these queries.

The Windows desktop control socket is unavailable here (the proxy reports socket
connection failure and no control socket is published). Official Codex docs
provide configuration reload/restart, but native Nyx handles are absent from this
running chat. Reconnect Codex once to load them. The Pascal semantic client is
usable immediately and is the primary inspection/edit route until then. Semantic
MCP remains primary afterward; physical browser/LCL consumers qualify input and
accessibility that the document API cannot prove. `AGENTS.md` and the agent guide
retain this operating rule. Confirmed callback/build/state/binding/root-cleanup/
review-session gaps have the open NS-4_agent-workflows_01 owner. No acceptance
credit or original event/codegen counter was changed by this follow-up.

The qualified server is installed at the LAN instance. Before restart, MCP
confirmed there was no pending draft, undo/redo history or restrictive permission.
The active paired design/source, selection and view were backed up privately,
revision rechecked and restored byte for byte. Old production 25544/37288 stopped
with identity verification and observed termination. Current production is
37076/20570; only this service remains. Local/LAN health is 200, and all five
served web assets match the qualified gesture hashes exactly. New executable
SHA256: `BD697581DCB7ABF6C0045506C4722093EB03AC7285ACEAF839C91829C24B908D`.
The exact six-artifact packet is `release-manifest.json`; the prior executable is
`previous-server.exe`. The live launch automatically refreshed both configuration
entries, privately checked equal. Existing Studio tabs may need a refresh because
editor capabilities rotate on restart. There are no active verification pipelines.

Remote protection: checkpoint 51bb982 is published on origin/hello-nyx, with the
existing Athena registration/pin unchanged. Private local config, bearer/editor
capabilities, fixture artifacts and generated binaries/JavaScript remain excluded.
The configuration/tool/docs follow-up is also authorized for remote publication;
verify its remote identity after pushing. Full goal remains active, solo, and the
supported-control keyboard/accessibility outcome remains the return path.

## MCP-authored keyboard review — 2026-10-04

Owner: original event/scheduler criterion 1, returning from the explicitly
prioritized Codex registration prerequisite. The counter-5 usable outcome is
implemented, integrated, qualified and deployed. Full original scope remains
open; criteria 2/3/4 retain accepted evidence and no-closure counter is 6.

The browser delegates physical focus to the portable view's admitted surviving
cursor after removal. It retains surviving editor drafts/carets, never steals
another control's focus, uses preventScroll and keeps empty collections reachable.
One visible row has the Tab entry; disabled hosts/rows have none. Read-only text
cells remain selectable and focusable, while HTML read-only checkboxes use
explicit disabled behavior. Leaf tree nodes no longer expose aria-expanded.
Grid F2/Enter enters an eligible editor, Tab/Shift+Tab traverses columns, boundary
Tab leaves, and Escape resets only the physical draft and returns to the row.
Native LCL retains its standard cell editor and focus behavior.

The primary semantic workflow actually composed the page through one ten-operation
revision-aware MCP transaction in a disposable review service. A seven-child
outline and typed read-only node query supplied bounded context. Five source
windows exported the unchanged revision's 376 accepted Pascal lines without
printing the whole source into model context. Both compilers consumed that exact
managed source. A revision-aware MCP PNG renders the page; inspected native/Nyx
browser consumers qualify compound actions, disabled descendants and read-only
memo focus. The operations fixture, agent guide and keyboard build target make
this journey reproducible. No fixture was loaded into user production work.

The physical keyboard driver shares the existing bounded Pascal-owned CDP
transport with the gesture driver. It issues DOM/host Input commands and never
injects JavaScript. Initial focus positioning is explicit; subsequent Tab,
Enter/Space, F2, Escape, row navigation, removal and boundary exit are host input.
Pascal-published attributes decide assertions; screenshots are selective evidence.
The initial driver omitted Enter's text phase and correctly failed activation;
Chromium's primary protocol mapping corrected it without weakening the assertion.
Existing mouse/touch/drag host assertions and sequence remain intact and pass.

Evidence under ignored build/focus/ and build/focus-*.log:

| Boundary | Accepted bounded evidence |
| --- | --- |
| Regression | baseline-browser fails removed-row physical focus; retained as reproduced failure |
| Native selection preparation/compiled companion | build/focus-integrated-build.log: 154 each, zero unfreed blocks |
| Executed browser selection/editor/lifetime | editor-qualified-browser/: 181 checks |
| Exact MCP-authored native companion | build/focus-keyboard-integrated.log: actual action focus/activation, disabled refusal, read-only memo; zero unfreed blocks |
| Host-generated keyboard | host-browser-final/result.json: two activations, one search/clear/decrement/increment, three removals, final focus keyboard-after; inspected PNG |
| Retained host gesture behavior | gesture-host-qualified/result.json: two capture/loss, outside movement, touch/cancel and real drag offer/hover/drop/end; all passes |
| Actual Nyx Studio/help/source/history | studio-desktop/ and studio-phone/: 52 each, exact phone CSS width 390; inspected captures |
| Revised server and actual MCP | build/focus-mcp-qualified.log: all 55 real HTTP/MCP checks |
| Compiler service/paired files | build/focus-http-qualified.log: all 177 generation/build/Unicode checks |
| Bounded agent intent/help | mcp-help-qualified.json: exact F2 query returns one table with current navigation/cell-edit help |
| Selective semantic preview | mcp-preview-qualified.json: revision-2 MCP PNG; inspected actual artifact |
| Live configured Codex | codex-discovery-after-rotation.log: real installed initialization, bearerToken authentication, all 11 tools from another project |

Setup failures are retained as limitations rather than product regressions: the
first HTTP run lacked the isolated machine profile, and the next ran from the
wrong repository for executing server-produced native artifacts. The existing
machine profile and correct review root resolve both; the final 177 gates pass.
A selective preview hit its fixed 20-second budget during competing browser
captures; one sequential retry succeeds. Do not assume reliable parallel PNG
capture or retry mutations. The native transport client prints structured preview
metadata but does not forward the image block; the saved server PNG was inspected.
MCP collection/binding/callback/build/review-lifecycle gaps keep their existing
workflow owner. No screenshot-driven designer authoring substitutes for them.

Delivery: six artifacts were hashed together in release-manifest.json; exact prior
server/web bytes remain in previous-server.exe and previous-web/. The user's
current paired project, selection/view and revision were saved privately and
rechecked immediately before stopping old production 37076/20570. No pending
draft, undo/redo or restrictive operator permission existed. The new service
restores the pair byte for byte and rotates only managed connection credentials.
Current production 37264/45363 serves the exact six hashes; loopback/LAN health
returns 200. Main JS SHA256:
6EC4C0175D3D51CEC751B578B3194882BD1B7EF9D6F62D8C7CB770819E2110C5.
Preview JS SHA256:
259E105DA2C8A87C8B2C29097EB8EEABA11CAC802740B0E8225E7FFC63AE70E6.
The revised executable hash is recorded at the top. Disposable services
40588/66324 and 15420/20486 were identity checked, stopped and termination
observed. All pipelines are terminal. Only production remains.

No full event, parity, accessibility, component or product criterion closes here.
The next concrete prerequisite is semantic callback authoring with real Studio,
paired-source/history and compiled consumers; original event counter 6 and
codegen counter 11 are retained. Do not start another isolated input-family
experiment or move requirements to manufacture closure. Full goal active, solo.

## Semantic callback authoring — 2026-10-04

Owner: NS-4_agent-workflows_01 criterion 3. The bounded implementation,
integration, qualification and delivery batch is finished; that criterion is
accepted. The full task/product remains open. Original event criterion 1's
counter 6 and codegen criterion 3's counter 11 remain unchanged.

Typed callback patches own 1..32 changes and prepare independent sessions through
the inspector's existing commands. Add creates crafted classes/initialization/
TODOs; policy and exact registration order retain siblings and identities.
Removal keeps implementations and shares an accurate reusable-definition/local
override warning with the inspector. One final adoption publishes a paired Undo
step while retaining operator selection/view. Response admission precedes live
publication. Reviewed removals bind actor, revision and exact change bytes in a
bounded sixteen-ticket cache; successful mutation receipts preserve exact retry
after ticket consumption. Unsupported/ambiguous events, invalid positions,
changed actors/reviews, drafts and refused grouped edits retain user work.

The real semantic journey composes a review page and adds physical, input-phase
and semantic callbacks through MCP while an already-open Nyx Studio observes.
Its Events/source panes show ordered registrations and policy. Warning review
precedes removal; the ordinary editor Undo restores the exact ordered source,
and semantic Redo restores the exact removal. Small Pascal-published attributes
coordinate the browser consumer; CDP reads them and captures the final UI without
injected scripts or designer authoring. The captured editor is inspected.
Bounded source windows export two accepted revisions unchanged for both compilers.
Actual LCL/browser button clicks invoke the generated registered TODO classes.
This proves template construction/execution, not authored business logic.

Evidence under ignored build/callbacks/:

| Boundary | Accepted evidence |
| --- | --- |
| Portable candidate/refusal/order/history | portable-native-final.log and portable-browser-qualified-final/: 45 each, including forward/no-op order; zero native unfreed blocks |
| Maintained tooling | agents-maintained-build.log: 29 existing plus 44 then-current callback checks; the later final order case raises the dedicated fixture to 45 |
| Real MCP + observing Nyx Studio | observed-qualified.log and observed-qualified/: 20 native / 7 browser checks, exact Undo/Redo and inspected final capture |
| Exact exported compiled consumers | consumers-maintained-build.log: native ordered/removed 10/9, zero leaks; consumer-ordered-final/ and consumer-removed-final/: browser 10/9 |
| Shared contract/type boundaries | core-regression.log and shared-browser/: 30/1537 on each target; 59 intended wrong-type refusals per compiler |
| Actual Studio Events/source/history | studio-desktop-qualified/ and studio-phone-qualified/: 52 each, exact phone width 390; inspected captures |
| Real authenticated protocol/rendering | mcp-http-qualified.log: 56 checks including actual PNG |
| Delegated compiler/source/artifacts | http-qualified-final.log: 177 checks |
| Actual installed Codex discovery | codex-discovery-staged.log and codex-discovery-after-rotation.log: bearerToken authentication, all twelve tools, including global discovery from another project |

Executed browser checks caught a pas2js method-as-array-index ambiguity in the
new result builder; High(LFields) fixes it in Pascal. A fixture's overlong owner
was corrected to the real 128-scalar limit. Native compiled-consumer selectors
were corrected to exact realized identities through RealizeNyxContext. An initial
Studio capture lacked its staged host HTML; copying the known host resolves it.
The first HTTP run used an old executable with obsolete generated-source bytes;
recompiling the current harness gives all 177 passes. A final candidate link
attempt encountered the running disposable executable; the release compiled to
a separate path. No product assertion was weakened. The maximum-batch test's
aggregate allocations remain substantial; this packet does not establish large
project source/performance acceptance or reset its existing owner/counter.

Delivery: release-manifest.json hashes all six artifacts together. Prior exact
bytes are previous-server.exe and previous-web/. Immediately before restart,
private editor observation rechecked the exact paired project, revision 2,
selection/view, no draft/history and editing permission. Old production
37264/45363 stopped with identity verification and observed termination. New
34780/68106 restores the pair byte for byte, rotates both managed Codex entries
and serves all six exact hashes. Loopback/LAN health is 200. Server SHA256:
B7ED671C9CAC076CACA5801403A8B66448729CFF9A3E6C77453DA1AF1A828029.
Main JS: 6C7AA86D4D413AC9478726242CC4FB3C1209D2BC520E4C992AF499BEDC5132A2.
Preview JS: 259E105DA2C8A87C8B2C29097EB8EEABA11CAC802740B0E8225E7FFC63AE70E6.
Private preservation packets are .local/callback-production-before/restore/after.json.
Disposable services 33212/1951 and 40284/38846 are identity checked, stopped and
termination observed. All pipelines are terminal; only production remains.

The running desktop chat still has no native Nyx handles and needs one reconnect;
the Pascal semantic client remains primary immediately and named MCP handles
remain primary after reconnection. Existing Studio tabs need refresh after
credential rotation. No dependency source, toolchain or firewall was changed.
Next declare criterion 4's structured build/job/diagnostic batch. Source/body,
state/binding, root cleanup and protected review-session gaps retain their owner.
Preview contention and client image forwarding remain recorded limitations.
The full goal remains active, solo; no broad source/event/product criterion closes.

Remote delivery: ac4c9fd2f2a239b656b2d50cde13b5fff0326ff4 is pushed and independently
verified on origin/hello-nyx. The working tree was clean after that implementation
commit. Final live semantic inspection advertises nyx_callbacks with 32 changes
and retains revision 2, home selection and no draft. This handoff-only update is
also authorized for publication. All qualified service/credential/hash state above
remains current; no new experiment or pipeline was started after acceptance.

## Semantic compiler jobs — 2026-10-04

Owner: NS-4_agent-workflows_01 criterion 4, declared before implementation as one
integrated build/status/observer delivery. Underlying event/codegen/service/native
Studio criteria retain their scope and counters. Criterion 4 is now accepted;
the broader workflow task remains open.

The thirteenth authenticated tool, nyx_build, has three closed shapes: outputs,
request and status. Admission captures exact accepted design/source and a private
output profile. View/reusable roots are exact admitted page/definition roots;
applications omit a root. Requests require current revision/output, edit permission
and no draft, forbid command/options/path/source injection and return immediately.
Workers own detached inputs and per-job results, with no borrowed widgets/model.
Both transports use the extracted fixed compiler pipeline and GUID artifact paths.
Only two semantic jobs run, sixteen handles persist, and sixty-four actor/exact
argument receipts prevent implicit resubmission. Shutdown joins workers; failed
start/list publication retains correct ownership. Stale pairs cannot navigate;
Undo may restore an exact pair at a newer revision. Current profile comparison
uses complete text. MD5 identities are optimistic fingerprints, not authentication.

Status windows contain at most twenty diagnostics with stable error/fatal-first
ordering and a closed severity filter. Successful browser manifests cover HTML,
JavaScript, runtime, compiled companion and scoped design; native manifests cover
executable, companion and design. Every served byte is checked by the real journey.
Failed builds expose no successful artifact. Observer diagnostics carry no repeated
source; ordinary Studio navigation additionally requires an acknowledged exact
local frame, with queue/conflict guards. User selection/history remain untouched.

Evidence under ignored build/agent-builds/:

| Boundary | Accepted evidence |
| --- | --- |
| Portable admission/currentness | admission-final/native.log and admission-browser-final/: 33 each; zero native leaks |
| Bounded workers/profile/retention/retry/shutdown | resource-final-owner-qualified.log: 45 native, all 4815579 allocations / 392185743 requested bytes freed; zero leaks |
| Maintained orchestration | agents-qualified-build.log: 29 agent, 45 callback, 33 admission, 45 resource checks plus actual browser compilation |
| Real semantic authoring/builds/observer | journey-accepted.log and journey-phone.log: 107 each, all six actual scopes/targets, immutable application source and complete served manifests; five observer assertions each |
| Actual rendered artifact and Studio | journey-accepted/ and journey-phone/: inspected compiled-browser and observer PNGs; exact phone observer 390 by 844 |
| Bridge source acknowledgement | bridge-qualified.log and bridge-qualified/: thirteen browser guard assertions |
| Shared contract/generated native consumers | core-final.log: 30 core, 1537 designer, 55 scheduler, 60 paired admission/recovery, compiled consumers and intended type refusals |
| Executed shared browser | shared-browser-final.log and shared-browser-final/: 30 core / 1537 designer |
| Studio Events/source/history | studio-desktop-final.log and studio-phone-final.log: 52 each, exact phone width 390 |
| Legacy HTTP/source/compiler/artifacts | http-extraction.log and http-final.log: 177 each |
| Final authenticated protocol and permissions | mcp-release.log: 61 real MCP against the release server; read-only readiness/request and disabled readiness/status guards |
| Fresh installed Codex after deployment | codex-release.log: bearerToken authentication and all thirteen tools discovered from another project |
| Exact delivery bytes | release-manifest.json: six installed artifacts match qualified bytes; all five served web hashes match |

Resource qualification caught a real first-use FPC recursive-parent creation race;
the serialized owner prepares the shared artifact parent before workers start.
Real compiler errors were initially hidden behind warnings; stable severity order
and closed filtering now expose actionable errors without mutating the report.
Case-normalizing severity wire names fixes the focused error query. Observer
fixtures now require actual running-job activity and explicitly open the ordinary
Pascal pane before checking diagnostics. Final bridge checks guard unsent local
source and queued publications. These failures are retained in their original logs;
no product assertion was weakened. An operator attempt compiled the native-only
core entrypoint with pas2js; the correct browser entrypoint was subsequently
compiled and executed. A resource invocation omitted its required runtime and
refused before setup; the complete final invocation passes. These invocation
failures are not counted as target evidence.

The real journeys compose the demo and reusable definition, export bounded source
and request every build through MCP. Intentional invalid-helper source uses the
existing explicit private editor fixture while semantic body editing is absent.
The resource fixture is an owned Pascal compiler substitute; actual target proof
comes from the separate real compiler journeys. Global HTTP/MCP scheduling,
caching, cancellation, persistent job lifecycle, toolchain binary snapshots and
large-project performance remain open with their existing owners.

Delivery preserves ignored .local/agent-build-production-before/restore/after.json.
Immediate observation rechecked the exact pair/revision 2, selection/view, no
pending draft/history and edit permission. Old production 34780/68106 was identity
checked, stopped and terminal. New 28012/71510 restores the exact pair, refreshes
both registered managed MCP entries without changing unrelated configuration and
serves all qualified hashes. Health is 200 on loopback and LAN. A final source-only
formatting edit recompiles to the exact installed JavaScript SHA256. Review
38416/67305 and 40260/69335 were identity checked, stopped and terminal; earlier
review services and every check pipeline are terminal too. Only production lives.
Final live semantic inspection returns thirteen tools, revision 2, home selection
and no draft without mutating the user document. Native chat handles still require
one client reconnect, while the Pascal semantic client remains primary now.

Remote delivery: implementation/evidence/deployment commit
6505d1e12746f0ad8f05669608ff16adfa7f2f34 is pushed and independently verified
on origin/hello-nyx. The working tree was clean after that commit. This
handoff-only update is also authorized for publication. All live preservation,
credential and artifact state above remains current; no experiment or pipeline
was started after acceptance. Full goal active, original counters unchanged.

## Semantic handler implementations — 2026-10-04

NS-4_agent-workflows_01 criterion 2's bounded body-authoring prerequisite is
accepted; the overall task and goal remain active. This integrated batch was
declared at the top before implementation, returning to criterion 5 and the
original event owner. It supplies a fourteenth focused tool rather than falling
back to designer automation or a private source-injection fixture.

`nyx_pascal` reads accepted local Invoke implementations through bounded Unicode
scalar windows, with an immutable signature and source line. Typed immutable
`TNyxHandlerEdit` / `INyxHandlerPatch` commands replace 1..16 exact implementations
on a detached full candidate. Expected text and revision guard one paired Undo
publication. The model, managed views, signature, imports and sibling helpers
retain their bytes; response size is admitted before publication. Grouped limits
are 32768 scalars per expected/new text and 131072 total. Drafts, failed groups,
duplicate/conditional/ambiguous methods, inline type blocks, sibling injection,
trailing comments, wrong types and unknown/joined fields refuse. No-op/Redo,
exact retry identity, selection and ownership remain intact. The scanner locates
a region; ordinary Pascal syntax/type errors remain compiler-owned.

The maintained real MCP journey composes a thoughtful-input page, adds two
callbacks, authors digit and Unicode-length validation, requests actual browser
and LCL view jobs, and authors a deliberate missing-helper compiler error.
An already-open Nyx Studio displays code/activity and undoes both bodies together;
semantic Redo restores exact text. Host typing rejects an invalid digit through
the actual compiled callback. Error navigation is current-source guarded and
becomes stale after recovery. The exported companion is compiled unchanged by
independent browser/LCL control consumers; its hash equals the final MCP build
source. This now proves authored business behavior, beyond compiled TODO stubs.

Evidence under ignored `build/handler-edits/`:

| Boundary | Qualified evidence |
| --- | --- |
| Portable source/admission/history/ownership | `checked-admitted.log`: 72 native; `portable-admitted/`: 72 executed browser. All 6335844 native blocks freed, zero leaks |
| Actual MCP/observer/compiler/host typing | `journey-current.log`: 21 desktop; `phone-admitted.log`: 21 exact-390 using the final guarded API; selective inspected PNGs and exact source exports |
| Independent compiled controls | `consumers-qualified-build.log`: nine LCL, zero leaks; `consumers-browser/`: nine executed browser. Exact final companion byte identity verified |
| Actual Studio Events/source/history | `gesture-studio-desktop/` and `gesture-studio-phone/`: 52 each, verified width 390; semantic Studio also passes 15 each |
| Existing callback/build/resource boundary | `agents-final.log`: 29 model, 45 callbacks, 67 pre-final handler checks, 33 build admission, 45 native job resources with zero leaks; executed callback/build gates retain 45/33 |
| Shared regression | `core-final.log`: 30 core, 1537 designer, 55 scheduler, 60 project and compiled reconstruction; `shared-browser-final/`: 30/1537 executed browser; 59 intended type refusals retained |
| Real protocol/service | `mcp-http-admitted.log`: 64 real MCP including PNG; `http-qualified.log`: 177 generation/build/Unicode; `bridge-admitted/`: 13 draft-conflict safeguards |
| Installed Codex/live semantics | `codex-production.log`: actual bearer authentication and fourteen tools from another project; `production-tools.jsonl` / `production-session.json`: fourteen tools and protected live context |
| Deployment | `release-manifest.json`, `release-backup/`, `served/`: six installed hashes and five LAN-served hashes match qualified bytes |

Native checked cumulative allocation is about 2.65 GB across repeated tiny-window
queries and detached admissions; every block is freed. This is correctness and
ownership evidence, not large-project responsiveness acceptance. Original
codegen criterion 3 remains open at counter 11; event criterion 1 remains open
at counter 6. Existing event criteria 2/3/4 and agent workflow criteria 3/4 retain
accepted evidence. General imports/helpers, state/bindings, rich reusable
operations, root cleanup and protected review lifecycle remain open. Compiler
caching/cancellation/global scheduling retain NS-5 ownership.

Failures were retained and resolved without weakening gates: duplicate-method
detection after a unit terminator was fixed; a fixture's Windows ANSI string
replacement was changed to Nyx text storage; pending draft setup now marks its
actual Pending contract; the coordinator uses enumerated keys because Field
refuses absent members; independent consumers bind generated callback classes
through the public startup contract and query runtime identity. The legacy HTTP
fixture first ran from the wrong repository, so native artifact execution could
not find the staged job; rerunning unchanged from the actual service root passes
177. Further ownership review added outer-conditional and trailing-comment
refusals, with final 72/72 evidence. No observation timeout caused a service reset.

Deployment's first private check used an incorrect capability header; production
was not stopped, and the attempted copy was blocked by its open executable.
The old server/main hashes were verified unchanged. The corrected check uses
`X-Nyx-Editor`, reobserves exact revision/pair/selection/view, requires no draft
or history and no active child processes, then stops the verified owner. Old
28012/71510 is terminal; new 36172/79421 restores the exact paired design/source,
selection and view. Private before/restore/after records remain excluded under
`.local/handler-production-*.json`. Local/LAN health is 200, all artifact hashes
match, enrolled credentials refresh and actual Codex authenticates fourteen tools.
All qualification pipelines and owned review services are terminal; only production
lives. No dependency source, compiler setup or firewall changed. Six new Pascal
files satisfy the requested blank-above-if style. Full goal active.

Remote delivery: implementation/evidence/deployment commit
0f454d4ab0507663fddd5e3a91afb8d7cb777629 is pushed on origin/hello-nyx
and independently verified. Its working tree was clean. This handoff-only update
records that protected checkpoint; production and all qualification state above
are unchanged, and no new experiment or pipeline was started after acceptance.
Full goal active; next declare the criterion-5 protected review/root-cleanup batch.

## Reviewed root cleanup — 2026-10-04

The declared single integrated batch delivers criterion 5's root-cleanup
prerequisite under NS-4_agent-workflows_01. It is implemented, integrated,
qualified, accepted and deployed; criterion 5 and the full goal remain open.
No event/codegen completion credit or counter reset transfers here.

Public `TNyxRootRef` distinguishes exact page/reusable partitions. The structural
model primitive retires the actual supplier-interface anchor under a temporary
raw token; retained specialized suppliers remain independently usable and may
be reattached. Immutable `INyxRootRemoval` owns copied paired text/root references,
counts authored descendants/registrations and retained reusable dependencies,
and admits a detached complete group. External references refuse; internal
references in the group are allowed. The sole live adoption uses ordinary paired
history. Pascal prefix/suffix, imports/helpers/callback classes and state defaults
remain. A missing view/selection falls back through pages, reusable roots, then
an empty workspace. Retained application references need compiler validation.

Studio's public Nyx confirmation and typed route use that command, show counts
and warning, allow cancellation and visibly disable referenced removal. MCP's
fifteenth tool reviews and applies exact groups of 1..16 roots with closed wire
fields, draft/refusal guards, actor/revision/group tickets and original retry
receipts. Eight review snapshots and 64 receipts bound session state. Response
admission precedes publication, including the actual surviving selection/view
names. Generic descendant deletion stays protected against root deletion.

Evidence under ignored build/root-cleanup/:

- Checked native contract: 45 passes; 3675219 allocations and frees, zero unfreed
  blocks (cumulative allocation bytes are not a performance measurement).
  Executed pas2js contract: 45 passes. Alternative supplier/raw ownership,
  exact root partitions, groups, dependencies, empty documents, source-frame
  retention, pending draft/stale review, permissions, eviction, retries and
  ordinary paired history are covered.
- Maintained real MCP journey: 26 desktop and 26 exact-390 passes. It authors
  two roots, a reusable instance and callback through semantic tools, checks an
  ordinary observing Nyx Studio's actual confirmation/activity/Undo, performs
  semantic Redo and compiles the cleaned full application with pas2js and FPC/LCL.
  Source is exported through bounded windows at one revision.
- Unchanged independent compiled browser/LCL consumers: five passes each. The
  entire unrelated design equals the original canonical design, retired roots
  and callback registrations stay absent, and surviving page/reusable views
  render through actual adapters. Native consumer frees all 53166 allocations.
- Final gates: 65 real MCP HTTP (including the fifteenth schema), 177 actual
  delegated HTTP/compilation; 29 agent, 45 callback, 72 handler and 33 build
  portable checks; 45 native compiler-job lifetime/resource checks; native
  shared 30/1537, 55 scheduler and 60 paired project cases. Executed browser
  shared 30/1537 and ordinary Studio Events/source/history 52 desktop/52 exact-390
  pass. Existing 59 intended native type refusals pass. No dependency changed.
- Fresh actual installed Codex initializes from another project and authenticates
  all fifteen tools. Native named tools in this existing desktop chat still need
  one client reconnect; real Pascal MCP calls are active now. Configuration,
  connection and current-chat discovery remain distinct in the guide.

Fixture/invocation corrections are retained honestly. The sample already had a
reusable instance, so adding one produces two retained references, not one.
Desktop has no compact panel switch; the observer clicks it only when present.
The consumer needed the runtime nyx.callbacks import, not a generated binder.
Two capture invocations supplied wrong success-marker names; corrected Studio
runs pass, and the shared recorded artifact is explicitly validated against its
actual data-tests marker and complete 30/1537 result. No semantic assertion or
acceptance criterion was weakened.

Production preservation/install details: immediate private observations retain
full pair/revision/selection/view/permission and confirm no pending draft/history
or owned child before stopping. The first copy was refused because Windows had
not released the stopped executable; an old service was mistakenly launched
before verifying install completion. Its exact saved pair was restored and
checked, then the correction reobserved it, waited for verified process exit,
installed all six hashes before launch and restored the pair again. Original
artifacts remain backed up. Private before/interim/restore/after records are
ignored; no user pair or credential is committed.

Final sole production PID 35152 / persistent exec 74922 serves the same LAN/editor
binding and loopback MCP. Both local and LAN health return 200. All six installed
and five served artifact hashes match release-manifest.json. Server SHA256:
892D77CF6827B23B6DF8A88D40604E065BA13EED62E6971D8B7BDCADAB0DD2B1.
Main JS: 27A1052F46FBCE95F605B92F7D4361657A30FE0F07D33EC7C84822132A264EB0.
Preview JS: 259E105DA2C8A87C8B2C29097EB8EEABA11CAC802740B0E8225E7FFC63AE70E6.
Enrolled Codex blocks refresh and match with no warning. Existing tabs need a
refresh after credential rotation. All disposable services and qualification
pipelines are terminal. Implementation a5bad51b8d080f8f11fc44af1c798c3e67e8c091
is pushed on origin/hello-nyx, with exact git ls-remote identity verified. The
handoff record follows this protected delivery. Full goal remains active.

## Full-catalog focus/keyboard concordance — 2026-10-04

Owner: original NS-1_event-scheduler_01 criterion 1. The declared complete-catalog
journey is implemented, qualified and deployed; full original criterion 1 remains
open at no-closure counter 7. Criteria 2/3/4 stay accepted, codegen criterion 3
stays at 11 and the full goal stays active. Focus evidence does not establish
every property's target behavior, complete cell-grid/typeahead, assistive
technology, hardware/IME, another widgetset, native Studio or production
performance. All original owners, criteria and counters remain intact.

`NyxSupportsKeyboard` centralizes the closed physical classification. Both
adapters expose borrowed `FocusFor` surfaces without redefining scalar `InputFor`.
Default literal code/list/table/tree faces are reachable, labeled and synchronized
through disabled/re-enable transitions; literal trees are flat leaves rather than
empty disclosure widgets. Disabled links lose their active href and regain only
validated authored destinations. Collection attachments retain their own entry.
Browser collection focus events cross the logical owner boundary once, and keys
arrive before default row actions. Split grips publish the complete focus/key
family, run hooks before resizing, remain inspectable when fixed/read-only, and
detach producers before disposal. Native radio peers retain one eligible entry.
An HTML checked-but-disabled/hidden peer requires label entry with immediate,
scroll-free delegation to the same real input; its checked value stays intact.
Creator adapters/hooks remain supported.

The primary workflow actually uses MCP: two bounded catalog pages, six paired
transactions for all 76 kinds and a peer page, small node/event queries, 80-line
source windows and actual immutable compiler jobs. Live service metadata agrees
with the independently compiled catalog at revision 7. Read-only re-export leaves
the accepted companion unchanged, and its SHA256 matches the actual source file
in each final compiler job. A separate five-control visual page is composed and
refined through grouped semantic operations; a bounded property-help query
establishes the literal-row newline format. Revision-9 MCP PNG at 390-by-844 was
inspected. No screenshot-driven Studio authoring or injected browser source is
used. Missing semantic state/binding/general-source/review operations retain the
existing NS-4 workflow owner; the bound-table physical consumer declares its
local binding extension explicitly.

Evidence under ignored build/catalog-focus/:

| Boundary | Accepted evidence |
| --- | --- |
| Semantic author/build | semantic-current.log: 76 kinds, six paired transactions, both actual application compilers, zero unfreed blocks |
| Live metadata/source | semantic-inspect.log: 76-kind concordance at revision 7, exact re-export, zero unfreed blocks |
| Final immutable compiler jobs | browser-release-build.json / lcl-release-build.json: succeeded/current, revision 7; exported companion bytes match both compiled files |
| Native catalog | qualification-current.log: 76 kinds, 103 faces, 30,637 checks, 6,405,014 allocations/frees, zero unfreed blocks |
| Browser desktop / exact phone | browser-qualified-final/ and phone-qualified-final/: 76 kinds, 103 faces, 23,434 checks each; inspected code focus PNG |
| Radio policies / HTML interop | radio-interoperability/: eight group policies and host Tab through three independently named-empty HTML inputs; the latter is an explicit browser boundary, not portable grouping authoring |
| Split defaults/consumption | split-build.log: 141 shared checks, compiled companion, 32 native real-control checks; split-browser/: 32 executed browser checks |
| Logical bound focus / keyboard | keyboard-build.log: unchanged MCP companion, actual native collection enter/exit, zero leaks; keyboard-browser/: host row/editor/internal transition and exit assertions |
| Collection lifetime/selection | selection-build.log: 154 preparation and 154 compiled native checks, zero leaks; selection-browser-qualified/: 181 executed browser checks |
| Shared contracts | core-build.log: 30/1537, scheduler 55 and paired-project 60; shared-browser-final/: 30/1537 |
| Nyx Studio | studio-desktop/ and studio-phone/: 52 each; exact phone CSS width 390 |
| Protocol/compiler service | mcp-gate.log: 65; http-gate.log: 177, including actual compiled companions |
| Semantic rendering | mcp-preview-final.json and dedicated preview PNG: revision 9, 390-by-844, inspected |
| Configured Codex | codex-production.log: fresh installed initialization from another project, bearer authentication, all fifteen tools |

Native catalog input uses real focus surfaces and LCL CN key messages; browser
uses owned host Tab/F8 and bounded native date/time shadow-segment traversal.
Neither establishes physical keyboard/IME or assistive technology behavior.
Every expanded interactive part has two independent ordered registrations;
disabled, inherited read-only, re-enable and explicit sibling cancellation cycles
must agree with publication. No unavailable grade or unreachable advertised face
is hidden to pass. Existing editing/gesture/extension/source packets remain
applicable; broader unrelated suites were not repeated without a changed boundary.

Failures exposed disabled link entry, unchecked native radio entry and unbridged
split focus/key defaults. Browser input tabindex alone also fails when a checked
radio is disabled; its retained failure PNG/JSON precedes the qualified delegated
entry. Initial fixture errors (component runtime identity, typed value method,
native date/time segmentation and one incorrect capture marker) were corrected
without lowering assertions or restarting running jobs. A read-only metadata
inspection was stopped after repeated deep-copy overhead; reading its bounded
array once fixes the test consumer, and the final inspect is leak-free. This is
no production performance claim. Existing compiler/scheduler capacity and client
lost-response qualification remain with their original owners.

Deployment: six prior artifacts remain in release-backup/; release-manifest.json
records both hashes. The private observation immediately before restart confirms
revision 2, editing permission, no draft, no Undo/Redo and no compiler jobs. Old
production 35152 was identity-checked, stopped and awaited for file release.
Every replacement hash was verified before launch. The new service restores the
exact user paired project, selection/view and permission; final observation
confirms it remains unchanged. All installed and served hashes match; LAN health
is 200. No user fixture/history/configuration override was introduced. Current
sole production PID 39004 / persistent exec 65653 retains the existing executable,
LAN bind and loopback MCP. All disposable services and qualification pipelines
are terminal. Managed enrolled Codex blocks refresh correctly after rotation;
fresh real installed discovery authenticates fifteen tools. This existing chat
still needs the documented native-client reconnect; Pascal semantic MCP stays
primary now. Existing Studio tabs need a refresh after credential rotation.

Server SHA256: 8A64F04282B61793606C93ED297D8084178360B8C35AFA63504532CEEE6D7FA6.
Main JS: 26E1566D31477A147E8AF1D299B15BDBE92C4F51F3D34BC3BF507D1A22C5A222.
Preview JS: 7ECA33774C8777ACF419B3B73E9CC0AE5D2BAC40E482561EB8964C2C9DDE4EB5.
Private observations, snapshots, credentials and machine paths remain ignored.

Next reassessment must compare all of original criterion 1 against existing
property/extension and event producer evidence, then declare a finite
property/projection conformance matrix and repair concrete false published
claims. Do not substitute another isolated hook-list batch or transfer remaining
requirements to obtain closure. The full goal remains active.

## Property/projection reassessment — declared 2026-10-04

Previous configuration follow-up verified the live pair and authenticated fifteen
tools without changing product state; it does not close another goal criterion.
The next available independent action is original event criterion 1's property
concordance, at counter 7. The finite matrix in docs/property-concordance.md maps
all 47 typed attributes to rendered or shared-contract acceptance and existing
evidence. This materially changes the decision path from hook-list expansion.
Criteria 2/3/4 remain accepted; codegen criterion 3 remains open at counter 11.

Concrete product deliverable: repair mutable literal content, input formats,
native code/group captions, image attributes/local pictures, browser progress
range presentation and layout/surface transitions. Use one disposable MCP-authored
76-kind companion and both real target consumers. Check Unicode/quoted/empty and
ragged rows, no-op identity/selection/drafts, attachment ownership and absence of
programmatic callback dispatch. Existing interaction/split/focus/source packets
are reused where unchanged. Do not replace or claim the LAN user's design.

Budget: one integrated repair and its focused target/Studio/protocol gates in
this batch; do not expand into a new resource loader, virtualization, hardware,
widgetset or native Studio project. Stop publication on failed lifetime, stale
focus, bound-row replacement, unwanted callback, source mismatch or preservation
checks. Record a concrete remaining gap with its existing owner and switch to
that acceptance path rather than repeated isolated property patches. General
native flex/hidden-space and resource-provider parity remain required; no support
grade or original criterion is weakened to earn closure.

## Updated Codex connection check — 2026-10-04

The user updated/restarted Codex and asked for live connection verification.
Authoritative process and listener checks found both prior Studio services gone;
the refreshed chat also exposes no native Nyx named handles. This changes the
next action: recover the qualified product service before testing authentication.
No replacement product artifacts or dependency changes were installed.

The qualified production server SHA256 remains
8A64F04282B61793606C93ED297D8084178360B8C35AFA63504532CEEE6D7FA6.
It now runs as hidden independent process 29656, with identity checked at the
original executable/root and editor 0.0.0.0:8088 / MCP 127.0.0.1:8089.
Both loopback and existing LAN health return 200. The last verified saved pair
from .local/catalog-production-final-observe.json was recovered through guarded
first-claim admission; exact ordinal project equality, selection/view, no draft
and empty history are verified at revision 2. No already-claimed user pair was
overwritten. Current private operator credentials/state and startup logs are in
ignored .local/codex-restart-check/; older connection credentials are expired.

Project/enrolled user managed blocks match the new session endpoint, without
configuration warning. The newly installed Codex executable authenticates all
fifteen tools through actual initialized discovery from another project;
the Pascal semantic MCP client also discovers fifteen and reads the live session.
This chat's native inventory still lacks Nyx handles. The user was told the
remaining desktop Settings > MCP servers > Restart step; primary semantic access
continues through the Pascal client. Do not confuse fresh initialized discovery
with the current chat's tool inventory. Evidence: codex-discovery.log,
tools.jsonl and connection.json in the private check directory. No setup criterion
or overall goal is newly closed by this recheck.

Return path: the property/projection batch above remains uncommitted and not
deployed. Native property consumers last passed 14,124 checks across 76 kinds / 262
faces with zero leaks; browser's preceding executed packet passed 14,259.
Subsequent dynamic numeric/text event-policy changes still require execution on
both targets. Real MCP creation exposed stale property metadata in ConfigureNode:
one input-type=number plus numeric value operation is refused against the
initial text domain. Repair shared detached admission without numeric-to-string
coercion, then qualify order-independent contextual properties and exact rejected
pair preservation. The disposable property server is no longer live after the
app restart; launch a fresh isolated service with its output profile before the
maintained properties author/build journey. Preserve the live production user
pair. Original event criterion 1 remains open at counter 7, codegen criterion 3
at 11; no original acceptance scope changes.

## Property/projection qualification — 2026-10-04

The declared integrated repair is implemented and qualified. Literal Items now
have independent content baselines on both targets: no-op publications retain
row handles/selection/scroll, changes retain the face, and managed collection
attachments keep their dataset ownership. A shared UTF-8 literal tab splitter
retains quotes and empty/ragged cells. Native group legends and code captions
synchronize. Detached local pictures decode before publication, expose alt text
and clear safely; portable network/resource providers remain open. Browser
images update validated attributes, progress maps the authored minimum/range,
layout clearing restores natural flow/local absolute positioning, and additional
surfaces toggle without replacing controls.

All seven input formats update in place; native masking clears correctly.
Intrinsic text/numeric editing policies change with format, including faces
initially mounted as numeric. Unrelated Sync retains drafts and emits no user
callbacks. Explicit declared recipe domains retain authority. Shared detached
edit admission stages complete scalar configurations before resolving their
final metadata, then checks original JSON types. Ownership transfers into the
detached tree before lookup so ancestor contracts participate. Member order
cannot turn numeric creation or numeric-to-text updates into a false refusal;
strings never gain numeric/Boolean authorization. Wrong types/unknown fields
retain exact paired source/design, selection/view, revision and Undo/Redo.

MCP remains primary: bounded catalog discovery, seven paired composition groups,
small metadata queries, 80-line export windows and both actual immutable
application compiler jobs. Companion SHA256
2ACA84384B8DD0C716294F75BD4CF686B1641635D871546FFFCDE228BE3B2E1D
equals both compiler-produced source files at revision 8. Physical fixtures
consume that unchanged source; their local binding extension remains explicit
because semantic collection binding authoring still has its existing workflow gap.

Selective rendering exposed that the old immutable packet omitted requested
dimensions: PNG width alone did not prove CSS viewport width. The packet now
carries dimensions, and the Pascal preview renders inside an exact-size child
viewport. Capture requires measured width, height and revision to agree. A
revision-10 MCP-authored view was inspected at 390-by-844; input/code/progress
fit the width, with the progress value correctly centered in its authored range.
This corrects the original preview boundary without claiming complete accessibility.

Evidence under ignored build/property-concordance/qualified/:

| Boundary | Qualified evidence |
| --- | --- |
| Semantic author / real compilers | semantic-author.log: 76 kinds, seven paired groups, browser/LCL applications succeeded, 33,401,158 allocations/frees, zero leaks |
| Immutable source equality | source/browser-build.json and lcl-build.json: revision 8/current; both actual files equal the export SHA256 above |
| Native control consumer | controls-final-build.log: 76 kinds, 262 faces, 14,127 checks; 6,045,799 allocations/frees, zero leaks |
| Executed browser / exact phone | controls-browser/ and controls-phone/: 14,269 and 14,270; the latter asserts actual inner width 390 |
| Typed detached admission | ../admission-run.log and agents-browser/: 39 each; native 3,718,036 allocations/frees, zero leaks |
| Existing shared agent boundaries | ../agents-build.log: callback 45, handler 72, roots 45, build admission 33, compiler resource/lifetime 45 |
| Real final MCP / CSS viewport | mcp-qualified-gate.log: 75; 3,553,430 allocations/frees, zero leaks |
| Delegated HTTP / actual execution | http-qualified-gate.log: 177 |
| Editing / ownership regression | editing-build.log: native preparation/compiled 117/118, zero leaks; editing-browser/: 121 executed checks |
| Nyx Studio | studio-desktop/ and studio-phone/: 52 each; phone asserts actual width 390 |
| Selective semantic rendering | viewport-preview-response.json and viewport-stage/build/agent-previews/: inspected revision-10 390-by-844 PNG |

Earlier core/designer, scheduler, focus, selection and gesture evidence remains
applicable where unchanged. No broad suite was repeated without a changed
boundary. Fixture failures are retained honestly: initial missing output profiles
and numeric metric strings were corrected; picture preparation had cached the
first encoded BMP; an input fixture inferred domain from family instead of its
declared contract. Native checkpoints exposed these rather than weakening
assertions. One preview overlapped another capture and timed out; a deliberate
sequential capture passed without restarting the live server. The first HTTP run
used an old executable and the wrong artifact working directory; the current
checked consumer, run under its owned server root, passes 177. Its retained
failed log is http-gate.log. No failed run is counted as qualification.

Delivery state: release-manifest.json records all six candidate/previous hashes;
release-backup/ retains prior files. Automatic approval review rejected the
combined production stop/install/restart command before execution, reporting
only "blocked by policy". Verification afterward confirms production PID 29656
is still live, all six installed files retain their previous hashes, health is
200 and the full user pair/selection/view/history/permission remains unchanged.
Private before/after/operator records stay under ignored .local/codex-restart-check/.
Do not retry the refused command in a disguised form or claim installation.
Native named handles still await the already-explained desktop reconnect;
the authenticated Pascal semantic client remains primary.

Candidate server SHA256:
F20118F5C39156C69183CF58741784B6948B10C6CC4E81ACC4FB1FEFB96090CD.
Candidate main JS:
FD083D1D2F6EF6E6EB5A37F88A92958B13BA36481BADF113B318F6F8CF4222E4.
Candidate preview JS:
10C942FEE9CD9C3B68A7948438CA70A47784BA68D9ABDF89C2B6386D817E99DC.

All qualification jobs/consumers are terminal. Three owned loopback services
remain idle: catalog/property PID 2580 (8218/8219), protocol PID 29628
(8228/8229), final viewport/protocol PID 7312 (8238/8239). Their exact executable
and root identities are recorded in qualified/service/launch.json,
qualified/protocol/launch.json and qualified/viewport-server/launch.json. The
first two use qualified/server/; the third qualified/viewport-server/. Production
uses the original executable. Do not confuse idle services with active compiler
jobs or stop another identity; the refused stop/install boundary stays visible.

Original event criterion 1 remains open at consecutive no-closure counter 8;
criteria 2/3/4 stay accepted, codegen criterion 3 stays at 11, and full goal active.
No scope/credit/support grade was lowered or transferred. Next independent path:
use the existing LCL/parity owners to qualify proportional and hidden layouts
across row/column nesting, authored dimensions/padding/gaps, resizing and toggles,
including retained focused inputs and collection/split descendants. The native
adapter's column-only flex and retained hidden space are concrete current gaps.
Keep portable resources, complete accessibility, hardware/IME, other widgetsets,
production component depth and native Studio on their original acceptance paths.

Remote source checkpoint dd5a95a8e81693024e0defcc0133af250601c659 is committed
and pushed to origin/hello-nyx; exact remote branch identity was checked with
git ls-remote after publication. All 21 owned changed files are tracked; private
records/build artifacts remain ignored. The current handoff records that verified
implementation without claiming the refused production install.

## Restart verification and typed layout continuation — 2026-10-04

The preceding connection check is progress: this restarted desktop chat exposes
all fifteen native Nyx handles, and eight live read-only capabilities authenticate
against revision 6. Selection/view remain home, the accepted pair and history
remain unchanged, and both output profiles report ready. Native named MCP is
the primary production design workflow; no setup edit or reconnect is needed.

Current batch stays with NS-2_lcl-renderer_01 criteria 1/2 and the original parity
owner. Deliver a public value-owned fluent layout policy, enum-valued wrap,
cross/main-axis alignment and automatic/content/fill sizing through descriptors,
managed configurations, persistence, generated Pascal, Studio metadata and both
adapters. Qualify authored-width text measurement, natural row widths, implicit
spacers and definite root height with actual controls and retained input state.
Use an independently staged current MCP service for new properties unavailable
in the older production binary; do not replace the user's pair or retry the
previously refused production install. Evidence must include semantic admission,
unchanged compiled source on both targets, real desktop/narrow geometry and
Studio consumption. A mismatch is a product failure, not permission to weaken
the fixture. Original acceptance, open counters and full goal remain intact;
arbitrary constraints, scaling, complete accessibility and native Studio still
require their original acceptance paths.

## Typed layout policy delivery — 2026-10-04

This finite integrated packet advances NS-2 LCL criteria 1/2 and the original
parity owner. `TNyxLayoutPolicy` is an independent value builder; managed/raw
configuration copies its mode, wrapping and logical alignment choices. Five
appended attributes preserve prior ordinals. Closed wrap/alignment/justification
and automatic/content/fill enums cross persistence, platform overrides, generated
methods and the bounded Studio source reader. Wrong enum families refuse in
both compilers and source admission. Existing pixel metrics survive sizing
overrides; positive parent weights still own main-axis allocation.

Portable line membership and cumulative spacing feed LCL's actual preferred
caption widths and authored-width text height. Wrapped lines use natural heights;
definite non-wrapping rows support cross alignment/stretch. Column alignment and
root fill propagate actual host height into retained weighted editors. Spacer
weight is one by default on both adapters and in metadata; explicit zero opts out
and Clear restores it. Native themed buttons now measure their painted caption.
Browser automatic wrapping follows its embedded size container; explicit policies
override it. Safe leading overflow and retained controls preserve keyboard order,
real draft/focus/selection and ordinary host accessibility behavior. Studio consumes
the public policy for its header and compact panel bar.

The maintained Pascal MCP client authors two review pages as one 50-operation
transaction on the independently staged current service, retaining home selection.
It inspects bounded enum/default metadata, refuses three wrong choice/scalar
patches at the same revision/history, exports 80-line source windows and requests
both real application compilers. No production mutation, operator replacement,
configuration enrollment or automatic mutation retry is used. Final revision 2
source is 17,468 bytes, SHA256
36D0D7AE2953621EAF103A6D32CCD097709B5071A6B76E9655A3E08000DFBB90
(MD5 56db93441b925f8c5714e00104542c7b), unchanged from the first qualified epoch.
Both compiler files and every served manifest entry match exact bytes/hashes.

Evidence under ignored build/layout-policy/:

| Boundary | Qualified evidence |
| --- | --- |
| Current actual LCL / public policy / codec / source / arithmetic | final-controls.log: 2,169 checks, 873,843 allocations/frees, zero leaks |
| Executed browser / true 390 viewport | desktop-final/ and phone-final/: 2,214 / 2,215; phone asserts actual inner width 390 |
| Final semantic defaults/refusals and actual compilers | final-metadata.log: both applications succeed/current, 1,805,922 allocations/frees, zero leaks; final-compiler-proof.json verifies full manifests |
| Core/designer/scheduler/project and strong types | core.log: 30 / 1,542 / 55 / 60, generated companions and three collection checks; 63 intended type errors per compiler in core.log / browser-build.log |
| Full catalog controls | property-regression-final.log: 15,437 native checks / 76 kinds / 262 faces, 8,099,033 allocations/frees, zero leaks; property-browser-final/: 15,579 executed checks against the current browser companion |
| Bound selection | selection-regression.log: 154 preparation + 154 compiled native, zero leaks; selection-browser/ publishes passed |
| Nyx Studio desktop and resizing | studio-desktop/: 25; studio-resize/: 21 across 390/1100/800/390, code/source/history and retained focus |
| Selective appearance | inspected live desktop and actual-390 policy captures plus Studio resize capture; no blank DOM teardown image counted |
| Preservation | private layout-policy-preserved.json: exact original paired project, revision 6, home selection/view, no draft/Undo, ordinary prior test Redo retained; production health 200 |

Failures remain visible: native preferred sizing initially gave different
captions the same generic button width; the public widget repair passed the
unchanged physical assertion. Earlier fixture/source-reader type issues were
repaired before qualification. A property gate selected an older export that
lacked its required numeric review; property-stale-export-failure.log is retained,
and the unchanged newer semantic export passes. One Studio parent capture used
the wrong terminal marker; the actual resize fixture already passed, and its
correct published marker qualifies studio-resize/. A final author rebuild briefly
overlapped its running client and hit Windows executable error 5; the client
completed and the sequential rebuild passed. author-lock-failure.log is retained.
No failed run is counted as accepted evidence.

All jobs/consumers/captures are terminal. The owned current review service remains
idle at PID 20776, identity recorded in server/launch.json; its executable/root
are confined to this packet. The prior three disposable services remain idle.
Production PID 29656 retains its original executable/LAN binding and accepted
pair. release/ and release-manifest.json stage exactly six qualified files with
SHA256 checks. The earlier automatic production stop/install/restart rejection
reported only "blocked by policy" and was not retried. Source publication does
not establish installation of this or the preceding property/allocation repairs.

Intrinsic constraints/shrinking, border/client/font/scaling behavior, baseline and
reverse flow, hardware/IME, assistive technology, widgetset breadth and native
Studio remain under original acceptance. The narrow geometry fixture is not
universal aesthetic or physical-phone approval. This packet accepts no full
renderer/parity criterion, task, milestone or product. After this and the preceding
allocation batch, consecutive no-closure count is 2. Reassessment changes the next
action to existing NS-4 workflow criterion 5's protected semantic review workspace:
independent paired source/history, bounded routing, observing Studio, safe
disposal, exact user-work preservation and both compilers. Keep that prerequisite
open until its integrated evidence passes, then return to the original target
outcomes. Event criterion 1 stays at counter 8; accepted 2/3/4 and codegen
criterion 3 counter 11 remain unchanged. Credits remain unassessed; full goal active.

The layout guide records typed policy semantics and reproduction via
`tools/build.ps1 -Target layout-policy -LayoutSourceDirectory <semantic export>`.
The Pascal generators refresh managed configuration and the 76-kind reference;
private outputs/configuration remain ignored. Implementation 20b00f3 is committed
and pushed on hello-nyx with exact local/remote identity verified. The private
proof stays under ignored local state and is refreshed after this handoff update.

## Native-MCP-authored proportional and hidden layout — 2026-10-04

This bounded integrated batch advances NS-2_lcl-renderer_01 criteria 1/2 and
the original parity owner, without accepting either task. The previous goal
turn was progress: a1e74a2 verified/pushed native desktop MCP connection. The
declared review covers weighted rows, definite/nested columns, fixed sizes,
padding/gaps, hidden first/middle/last children, rounding, resizing, cleared
weights and retained memo/list/split descendants. Original event criterion 1
stays open at counter 8; accepted 2/3/4 and codegen criterion 3 at 11 remain
unchanged. Full goal stays active; no scope, support grade or credit is reduced.

Implementation: a portable owned allocation array excludes hidden entries and
reserves visible gaps/fixed sizes before cumulative weighted rounding. Double
accumulation avoids overflowing Integer products; native bounds remain integers.
Rows now advance by actual authored/allocated widths. Columns use explicit or
parent-allocated heights, rather than requiring the latter exclusively. Hidden
natural/grid children consume neither slots nor gaps. Geometry is clamped, and
LCL's automatic anchor pass waits for the complete layout. This fixes a label
first mounted hidden returning at stale 0/0 despite SetBounds(10,10,...).
Authored widths honor actual available parent content, including scrollbar
differences. Browser positive weights admit zero minimum height and fill framed
editors; zero restores automatic basis rather than collapsing explicit widths.

Native named MCP is the primary workflow here. At revision 2, one transaction
adds the 27-node maintained review while retaining home selection/view. Bounded
outline/property queries and nine source windows inspect/export revision 3.
Native tools request actual browser/LCL view and application builds; final jobs
are terminal/current at revision 3. Both compiler files equal the unchanged
export SHA256 D017FBC4E26D68DD51C668ED67CD00DBCE6B3F38E54D34764B128F840300754D
(MD5 61aec34b00e611bfb53d5936b4c03018). Real control consumers compile that exact
companion. The maintained Pascal semantic client additionally qualifies bounded
inspection/export and both compilers, with no configuration change or project
replacement. The JSON operations file is an explicit typed wire fixture.

Evidence under ignored build/layout-concordance/:

| Boundary | Qualified evidence |
| --- | --- |
| Actual LCL and portable allocation | layout-build.log: 2,084, 308,094 allocations/frees, zero leaks |
| Executed browser / true phone viewport | desktop/ and phone/: 2,094 / 2,095; phone asserts actual inner width 390 |
| Native semantic jobs / source equality | final-compiler-jobs.json: both final applications succeeded, exact export above |
| Maintained Pascal protocol author | author-build.log / author-run.log: bounded inspect/export and both compilers; 1,787,236 allocations/frees, zero leaks |
| All catalog property consumers | property-regression.log: 14,127 native / 76 kinds / 262 faces, zero leaks; property-browser/: 14,269 executed |
| Bound selection regression | selection-regression.log: 154 preparation + 154 compiled native, zero leaks; selection-browser/: 181 executed |
| Editing regression | editing-regression.log: 117 preparation / 118 compiled native, zero leaks; editing-browser/: 121 executed |
| Nyx Studio editing/source/history | studio-desktop/ and studio-phone/: 29 each; latter actual width 390 |
| Selective appearance | native nyx_preview PNG at revision 3 plus inspected live current-consumer phone/capture.png |
| Cleanup and restored application | restored-compiler-jobs.json: both actual application jobs succeeded at revision 6 |

The selective native MCP PNG validates tool availability and the preceding
production preview adapter; it does not qualify installation of this repair.
Current consumer captures retain their real browser view until page teardown,
so the PNG shows the measured controls. Earlier fixtures freed their DOM before
capture and produced a blank image despite passed geometry. No blank image is
counted as visual qualification. Native consumers release immediately and retain
zero-leak evidence. The narrow figure deliberately exercises constrained controls;
it is not universal visual/typographic or physical-phone approval.

Meaningful failed checks were corrected rather than weakened: a fixture offset
had added an extra gap; pas2js exposes offset geometry as Double and its matched
textarea binding uses selection properties; native hidden-label anchoring was
a product defect. Browser zero weight was a real collapsing-basis defect. The
first narrow run wrapped an explicitly sized row after ordinary body margins
and scrollbars reduced the host; the review now establishes a margin-free owned
test surface, and a further 360-pixel host check explicitly qualifies capping and
remaining-space allocation. The original failed narrow DOM/log are retained as
phone-first-failure.dom.html / phone-first-failure.log. No failed run counts as
qualification.

Cleanup uses nyx_roots review at revision 3 (27 nodes, zero registrations or
retained dependencies), then unchanged actor-bound apply at revision 4 as one
paired Undo step. Both post-removal applications compile. Exact observation
confirms identical design but 65 extra source characters: retained editing/
gesture imports and separators, as the root tool explicitly promises. Two
native paired Undo steps return the exact original project string at revision 6.
Both restored application jobs succeed with the original source fingerprint
ad8491f65bee8990fe74bbac2ef4bd76 and design e418262a4860dfd1d900d522c8df9907.
Home selection/view, one page/component, edit permission and no draft remain.
Original source/design/draft fields are byte-identical; test operations remain
on Redo, with no Undo. This demonstrates the existing protected-review lifecycle
gap under NS-4; it does not claim a history-free workspace or use operator claim
to erase it. Private exact before/after observations remain under ignored
.local/codex-restart-check/layout-after-cleanup.json and layout-after-undo.json.

Production identity is still PID 29656 at the existing executable path, with
health 200. No service replacement, deployment retry or configuration change was
attempted. The earlier automatic "blocked by policy" rejection remains visible;
LAN frontend/server bytes still precede both the property and layout candidates.
Delegated jobs consume the current source independently. The three previously
recorded disposable services remain idle; only their owned test resource names
were updated. All compiler/control/capture handles in this packet are terminal.

Maintain the original outcome beyond this batch: natural/intrinsic row sizing,
implicit spacer policies, root height propagation, richer wrapping/alignment,
effective authored-width measurement for natural-height children,
client/border/typographic scaling, accessibility/hardware/widgetsets and native
Studio retain their original owners. The guide is docs/layout.md; build target
layout reproduces physical consumers after semantic export. Next meaningful
action is a complete typed layout policy and capability journey through those
remaining renderer/parity outcomes, including Studio consumption, rather than
closing full parity from this finite fixture. Protected semantic review sessions
remain the existing NS-4 workflow prerequisite. Source publication uses
hello-nyx; verify exact local/remote HEAD after pushing this packet and keep its
private proof outside committed files.

## Protected-review qualification — 2026-10-04

The maintained Pascal journey now passes 377 real MCP/ordinary Studio checks
with zero unfreed blocks. The earlier 359-check run failed exceptional cleanup:
the fixture sent DELETE twice for one retired transport and printed PASS too
early. Its artifact is retained. The client now closes idempotently and the gate
prints PASS only after teardown. A corrected 341-check run then passed; the
final extended journey additionally qualifies reviewed child root cleanup,
exact refusal revisions, trusted compiled browser input and a real native job
finishing after its mutable review owner is retired. Counts include bounded
polling/response guards and are not separate product features.

Private evidence lives under build/review-workspaces/: lifecycle/journey holds
the final exact user baseline/after, bounded source export, four view/application
job results, source/control preservation, live observation, compiled input and
the selective semantic PNG. protocol/lifecycle-run.log records the final clean
teardown. User-fixture accepted/draft/base bytes are equal; pending draft, Undo
and Redo all remain true. No reviews survive disconnect. Source SHA-256 is
617C91F2E0F28A525634CF5872DE5F4FC00ABD66AD97AD9FE5A6A76FEBC7CA82;
both real application compiler files equal that exact bounded export.

Portable lifecycle passes 200 native checks with zero leaks and 200 executed
pas2js checks. Actual unchanged generated LCL/browser controls pass eight each,
including numeric admission, ASCII/supplementary veto and clearing. The real
compiled browser also accepts 12 and atomically vetoes x and a supplementary
character through trusted host input. The actual LCL consumer uses matching
installed units and has zero leaks. Agent/shared chrome checks pass 39 natively
with zero leaks. Public Nyx desktop layout/source passes 25 at width 1076; real
390/1100/800/390 resize passes 21, and actual-390 authoring passes 69. Captures
under chrome-layout and chrome-authoring were inspected; narrow scrolling and
the optional split/source remain contained. No physical-phone, IME, assistive
technology or additional widgetset qualification is inferred.

The browser shell now hosts its code editor in an independent owned ordinary
Nyx document/renderer and moves that realization with the shell. Backend DOM
identity and exact 4,093-unit pending Unicode value survive activity refreshes.
Frontend inspection handles alone are not element identity; the initial fixture
comparison was corrected and its failures remain retained. Native/default shared
views keep the inline public editor. Browser-specific hosting uses a typed
presentation choice and the same specialized public code-editor factory.

Build targets review-workspaces/review-consumers stage and compile the maintained
Pascal fixtures and viewers without launching services or editing a user project.
Both orchestration paths pass using the installed toolchain. The context-candidate
service's owned identity remains recorded in build/review-workspaces/
context-candidate/launch.json; all five jobs from the final journey are terminal.
Neither that service nor the earlier review fixture is a production deployment.

Native named MCP still authenticates the fifteen-tool production release. Latest
read reports revision 6, home selection/view, one page/component, no pending draft
or Undo and ordinary Redo retained. Trusted exact pair comparison against the
original layout-after-undo observation passes; private review-gate-preserved.json
records it. Production PID 29656 and all release artifacts remain unchanged.
No replacement, approval request or attempt to bypass the earlier automatic
"blocked by policy" refusal occurred. Source publishing still requires exact
local/remote verification; it does not claim LAN delivery of the sixteenth tool.

This closes the bounded protected-review prerequisite only. Full workflow
criterion 5 and broader goal stay open; one logical batch completed across its
interrupted checkpoint and qualification, no-closure count 1. User-steered next
batch owns criterion 6 / authoring 7: concurrent user projects, stable explicit
agent/job targets and full-editor jump/return preserving each presentation and
pair/history. A read-only live review or temporary-only manager does not meet
that acceptance. State/binding, general source/import, richer reusable workflows,
native Studio and the original renderer/accessibility return paths remain.

## Concurrent-project qualification — 2026-10-04

The portable project registry and closed version-2 presentation contract pass
215 checked native and 215 executed-browser cases. Native teardown frees every
allocation. They qualify independent accepted/draft/base and both history
stacks, permission inheritance, exact private retry/removal authority, retained
creation receipts, refused closure, bounds and 256 supplementary-scalar labels.
Scroll conversion explicitly rounds nonnegative half pixels upward on both
targets. Actual public Nyx Agents view controls pass ten on LCL and browser,
including same-caption/distinct-session metadata and typed warning/cancel
actions. Shared view event capture does not prove a full native controller.

Authenticated Pascal semantic composition/history/source/build/preview and the
ordinary observing browser editor pass 202 desktop and 203 actual-390 checks,
with zero native leaks. Evidence: build/project-workspaces/journey-12 and
journey-11, protocol/run-12.log and run-11.log. Exact supplied fixture references
are reused after saving their previous frames; no registry eviction or hidden
restart manufactures capacity. Primary and Project A pending Unicode drafts,
selection/view, source proportions and history survive full-editor jump/return.
Both actual application jobs retain Project B while the user observes primary
then Project A; compiled files equal the unchanged bounded export. Its SHA-256:
E7CF71BC23EA27DE5F95368AE2BF670ADB975979D0239E3990D8CA7E9BC9A9DC.
Project B paired Undo/Redo changes diagnostic currentness exactly; wrong-scope
job reads refuse. A selective 390x640 semantic PNG renders its correct heading.
Agent disconnect releases presence, retains projects and does not change the
primary pair. Permission reduction/disable and foreign/mixed scope refusals pass.
Counts include bounded polling/response guards, not distinct product features.

Failed journeys remain under journey-1 through journey-10. Initial failures
exposed hidden first-arrival Agents UI, an imprecise fixture target, stale
frontend inspection handles, required lookups for unmounted compact panels,
null preference restoration and an agent paint removing a pressed button.
Fixes use exact service references, bounded read-only control lookup retries,
mounted-root guards and object-kind preference admission. Stable reference-based
control IDs, retained Agents scrolling and coalesced paints after pointer release
repair the tap race; the real fixture holds the same actual button through
semantic activity and releases once. No mutation/click replay hides a failure.
Earlier passes before the stronger footer/currentness gates are not substituted
for the final journeys. No physical phone/IME/assistive-technology claim follows.

Focused legacy shell checks pass 25 desktop layout/source, 21 real
390/1100/800/390 resizing and 20 split-pane checks. The first narrow capture used
the wrong expected attribute although its DOM reported all 21 checks passed;
regression-layout-narrow-confirmed corrects that command and passes. Current
contract/view captures report 215/10. Installed-toolchain project-workspaces
orchestration runs native/LCL consumers and compiles service, full Studio,
viewers and the maintained semantic/physical fixture. It launches no listener.
The final full Studio browser compile includes the explicit half-pixel rule.

Trusted operator closure is implemented in the registry and private editor HTTP
route, separate from agent permissions/tools. Actual warning/cancel and refusal
of confirmation on an older backend pass. Automatic approval review rejected
launching the new close-route service ("blocked by policy", no further reason).
The rejected combined command never executed; the compiled close-server binary
has no running process/configuration. Existing project fixture PID 4972 remains
the earlier backend at editor 8278/MCP 8279 with updated browser files. Successful
live authentication, warned confirmation/stale refusal and completion after
project closure remain unqualified. No alternate launch route was attempted.

Production PID 29656 remains the existing LAN release. Native named MCP
authenticates fifteen tools and reads revision 6, home selection/view, one
page/component, no pending draft/Undo and ordinary Redo. No production reset,
replacement, credential change or deployment occurred. The user now explicitly
authorizes disposal of this test project; preserving it is no longer an
operational prerequisite. Per-project isolation remains a product invariant.
Verify launch.json/process identity before any future owned-service shutdown.

Reassessment: NS-4 workflow criterion 6 / authoring criterion 7 remain open;
this second logical batch closes no original criterion (consecutive count 2).
Checkpoint the usable project foundation and end registry/fixture expansion.
The next concurrency deliverable finishes live operator close/closed-context
job currentness, then follows broader presentation/full native navigation.
The refusal is an external execution boundary; compilation is not a substitute.
User steering now selects bounded compiler-warning cleanup under NS-6 delivery
before returning. Criteria 2/3/4 retain accepted evidence; criterion 5's remaining
state/binding/reusable/general-source scope and the full goal remain open.
All compiler jobs and capture/fixture handles in this packet are terminal.
That project foundation was committed/pushed as a7bd0b1 and its exact remote
reference independently verified. Private proof is recorded under
.local/codex-restart-check/concurrent-project-remote-proof.json.

## Compiler warning cleanup — 2026-10-04

User-selected batch: repair owned build warnings under NS-6 delivery criteria
1/4, preserving admission, ownership, exact Unicode, generated source and actual
target behavior. This is a bounded installed-toolchain packet; full CI/package/
supported-widgetset prerequisites remain. No dependency source or global warning
flags changed. Stop condition was any changed source pair/control semantics.

Defensive enum cases now inspect ordinals so invalid cast/bridge values retain
their refusal branch rather than triggering an unreachable-code warning.
Intentional subset cases have explicit no-op/default behavior; catalog facets
use typed membership. Managed results and handler stacks initialize explicitly.
Dispatch declares its intentional inherited-name replacement. Browser byte/hash
accounting avoids unsupported Int64 while native intermediates remain wide;
admitted history budgets preserve exact arithmetic. Owned Unicode separators and
fixture expectations use TNyxText without implicit wide-string promotion.
The browser file bridge uses the current RTL type_ property.

Only two private, node-owned facade declarations scope FPC advisory 3018 off;
comments explain their lifetime contract. An implementation-only collection
validator constructor is public within its private implementation type.
Project directory-link refusal uses the RTL's native link query rather than a
platform-marked attribute bit. The maintained Pascal project fixture now admits
an optional explicitly prepared host-link fixture and verifies read/save refusal,
unchanged target/sentinel bytes and absence of project writes through a real
Windows junction. Shell orchestration only prepares that owned platform fixture.

Checked tools: installed FPC 3.2.0, matched FPC 3.3.1/LCL win32 and pas2js 3.3.1
with its matched runtime. Ignored evidence is under build/warning-cleanup/.

- tools/build.ps1 -Target core: zero warnings; 30 core, 1,542 designer, 55
  scheduler and 60 paired-project checks; 63 intended compiler type refusals;
  generated design/identity/events/structural/managed/legacy/source/state
  companions execute, plus three compiled collection checks. final-core.log.
- tools/build.ps1 -Target project-workspaces with independent browser output:
  215 portable and ten actual LCL Agents checks, zero retained blocks; owned
  server, protocol fixture, Studio/viewer/preview/browser consumers compile with
  zero owned warnings. project-orchestration.log. Executed workspaces.html also
  reports 215; that unchanged qualifying capture remains applicable.
- Focused checked server and actual LCL view builds report zero warnings in
  server/build.log and views/build.log. Final core-emitted project fixture runs
  with the real junction argument: 64 checks, no leaks; final-project-links.log.
- tools/build.ps1 -Target layout-policy with the unchanged semantic export:
  2,169 actual LCL/control/arithmetic checks, no leaks. final-layout.log.
  Actual browser executes 2,214 desktop / 2,215 exact-390 checks; retained memo
  text, focus, caret/selection, hidden flow and resizing remain qualified.
- The final pas2js shared suite executes 30 core / 1,542 designer checks;
  final-browser-core-build.log and final-browser-core capture. Physical Studio
  resize passes 21 checks; split passes its 20 child interaction checks plus nine
  host transition checks, preserving the exact draft. Final layout/resize/split
  logs and PNG/DOM captures retain terminal passed markers. Selective narrow
  layout and Studio split PNGs were inspected.

Every final browser compile reports only seven installed RTL classes.pas
incomplete-case warnings (lines 4282, 6997, 7168, 8651, 9478, 9504 and 10132).
They stay visible. Earlier inventory logs retain the owned warnings and failed
commands, including an omitted layout companion directory; the maintained
orchestration supplied that prerequisite before successful qualification.

Native named nyx_build status verifies both live revision-6 application jobs
succeeded/current: browser warning total seven, all classes.pas; LCL warning
total zero. Both compiled Pascal files are 11,826 bytes with exact MD5
ad8491f65bee8990fe74bbac2ef4bd76. Design fingerprint remains
e418262a4860dfd1d900d522c8df9907. live-mcp-proof.json retains bounded job evidence.
These are actual compiler results, not claims that both applications executed.
No active-project mutation, new listener, production replacement or enrollment
change was needed; the earlier deployment/close-route launch refusals remain.

One-off MCP curiosity assessment: a bounded eight-line native nyx_source read
returned 232 bytes of ASCII tool text versus the 11,826-byte compiled source,
about 98% less returned text for that specific query. Grouped Undo, explicit
revisions, compiler status and independent project routing provide concrete
workflow benefits. There is no controlled A/B timing/token benchmark or honest
overall speed multiplier. The user requested one report, not recurring telemetry.

The requested warning repair is qualified on this installed pair; full delivery
and broader target/harness qualification remain under their original owners.
No task moved to DONE. NS-6 cleanup count is 1; the existing NS-4 reassessment/
count 2 is unchanged. All compiler jobs, fixtures and capture handles are terminal.
Next concurrency deliverable remains live operator closure/authentication/refusal
and closed-context compiler completion, followed by broader presentation/native
navigation; compilation or preview-only views do not establish those outcomes.
Warning checkpoint remote verification is retained privately at
.local/codex-restart-check/warning-cleanup-remote-proof.json after publication.

Current continuation: the warning checkpoint ffe727e is pushed and independently
verified. The previous goal turn was progress. Existing services are live, but
the rejected updated-service launch is not retried through another route.
Follow the concrete native-controller prerequisite under original NS-4 authoring
criteria 4/7: LCL currently lacks the browser's designer-purpose projection and
retained MoveHost boundary. Deliver selection/undoable canvas editing without
application actions, and move the same mounted controls safely between hosts.
Qualify an MCP-exported companion on actual LCL/browser controls, including
Unicode editing, independent reusable parts, focus/caret/scroll, refusal and
destruction. Stop on action dispatch, source/history leakage or broken mounted
lifetime. This changes the platform prerequisite, rather than extending the
already reassessed registry/fixture investigation. Full native Studio/controller,
live closure and the original concurrency criteria remain required; count 2 is
retained until original acceptance advances.

## Native designer and retained-view prerequisite — 2026-10-05

Previous user-directed turn supplied the one-off MCP curiosity report; it made
no product progress. This continuation revalidated the live fixture handles and
delivered the concrete native-controller prerequisite under original NS-4
authoring criteria 4/7. The goal stays active; no full native Studio or project
concurrency acceptance is inferred from an adapter fixture.

LCL now offers designer-purpose Render, non-focus-stealing authored selection
and retained MoveHost. Designer input proposes values through the same undoable
session command as the browser while bypassing application actions, runtime
subscribers and captured creator hooks. Native/browser moves retain actual
controls, Unicode scalar selections, focused source and containing scrolling;
nil, descendant, occupied and unmounted destinations refuse. Former hosts can
be destroyed after a move. Manual subscriptions are explicitly canceled by
their owner. Browser selection now outlines an outer reusable instance once;
qualified nested identity remains distinct from an identically named control.

The real Pascal MCP author creates an owned empty review, performs nine related
operations in one expected-revision transaction, exports bounded source and
requests both application compilers. Exact revision/currentness/fingerprint
checks pass; downloaded compiled source bytes on both targets equal the export:
MD5 93bd930faac0d6ef69fd35cf1f1c6e2a. The exact review is retired and all published
primary content/history/navigation fields remain unchanged; presence advances
normally. Receipt/status/source/frame evidence is under
build/native-studio/source-qualified/. Both immutable jobs succeeded on the
existing staged backend. Its older library mirror still emits owned warnings;
those jobs do not qualify current-library warning cleanliness or execution.
Current repository adapter consumers below do qualify this installed pair.

- qualified-orchestration.log exercises the explicit DesignerMCPConfig path,
  real review/compilers and checked native controls, then compiles pas2js.
- final-qualified-orchestration.log passes 32 actual native checks with no leaks.
  native/qualified-visual.log passes 34, including actual painted accent pixels
  for both component and page-root selection; three PNGs were inspected.
- current-designer-desktop / current-designer-narrow pass 32/33 actual browser
  checks, including exactly one authored-instance outline. Narrow is a measured
  390-pixel iframe. The consumer preserves exact Unicode source drafts, recipe
  and sibling values, paired Undo/Redo, focused ranges and actual scroll.
- regression-interactions.log passes 234 portable contracts. The corrected
  checked native runtime fixture passes 45 with zero leaks under regression-native.
  current-runtime-inputs executes all 45 browser runtime cases with a terminal
  passed marker; owned harness casts, UTF-8 comparisons
  and both closed-trigger refusals are repaired. Current browser compile keeps
  only the seven installed RTL warnings.
- current-identity passes 51 legacy identity/view checks, 44 managed-control
  and 35 compiled-event checks. Current Studio resize passes 21 exact-width
  checks; split passes nine host transitions and 20 child interactions, retaining
  the exact draft. All five current designer/identity/resize/split captures are
  terminal with passed markers. Final native and narrow browser PNGs were inspected.

Failures remain in ignored output. The initial review omitted named parts and
therefore correctly refused an instance edit; a verified native debugger stack
identified its modal exception dialog. That exact fixture process was stopped
after verifying its executable; callback failures now report through assertions.
An overwritten imperative spy was still subscribed, exposing the caller-owned
Cancel contract. Painting then disproved a scroll-host outline: its full-page
native child occluded graphics. After bounded reassessment, adorners were moved
to the selected face's immediate paint parent, with an inset root perimeter.
TShape's excluded final fill row/column required a matching solid pen for the
full two-pixel accent. Pixel failures, attempted panel variants and the unusable
desktop-DC observation are retained; no dependency source was edited and no
capture was manually composed to manufacture passing pixels.

Current native/LCL fixtures report zero owned warnings; current pas2js reports
only the seven visible installed classes.pas warnings. This qualifies Win32 and
the executed browser host, not another widgetset, physical phone, IME, hardware
or assistive technology. The themed creator spy uses a public Nyx LCL button.
No production listener, compiler profile, private enrollment or user project was
replaced. Existing services retain their identities; no equivalent retry of the
automatically rejected updated-service launch occurred.

The bounded prerequisite is delivered; original authoring 4/7 and workflow 6
remain open. No task moves to DONE and no completion credit is transferred.
The original NS-4 no-closure counter advances from 2 to 3. Reassessment now ends
adapter/fixture expansion and chooses the actual standalone native controller:
consume the shared Nyx shell/session/router, queue native paints after widget
notifications, retain canvas/source through chrome and page/component navigation,
then connect existing service/file/workspace transports. Qualify ordinary full
editor journeys with retained drafts/history and optional outputs. Live warned
workspace closure and completion after closure still require the refused server
launch boundary to be resolved; they are not replaced by this prerequisite.

Publication: this complete adapter/semantic-consumer packet is committed on
hello-nyx and pushed, with exact remote-head verification retained privately at
.local/codex-restart-check/native-designer-remote-proof.json. The one-off report
reply made no additional product progress; the subsequent continuation verified
the final captures and proceeded to the native controller.

Current batch: original NS-4 authoring criteria 2/4/7 own the standalone native
controller. Deliver a runnable Nyx-built native editor using the existing shared
shell, source/authoring/inspector routers and paired store. Native callbacks only
enqueue painting; mounted canvas/source survive chrome and compact-panel changes.
Evidence must use actual controls, full page/component navigation, pending Unicode
source, paired Undo/Redo and real saved files. Stop on destruction within a widget
callback, draft/history loss or application dispatch in designer mode. The full
HTTP/MCP/concurrent-workspace and wider authoring requirements remain explicit;
this batch cannot accept native parity from compilation or another adapter probe.

## Standalone native Studio controller — 2026-10-05

Original NS-4 authoring criteria 2/4/7 own this runnable editor candidate. The
native entry point consumes BuildNyxStudioView, the portable session and existing
source/property/palette/authoring/event/root/diagnostic routers; native widgets
remain public Nyx adapters. An already built editor starts without application
compilers or a network service. Native event callbacks queue one coalesced paint,
parking the independently mounted canvas/source before replacing chrome. Source
typing and title/source feedback preserve the notifying input. Local paired
Save/Open uses the shared revision-guarded store; warned conflict resolution
backs up the exact current pair before admitting the saved version.

The shared shell now uses public scroll views for palette/inspector and typed
native platform layout overrides. Single unwrapped native rows with a definite
cross axis allocate that viewport instead of expanding to a scroll child's
natural height; wrapped/intrinsic lines retain their previous policy. Shared
browser consumers remain qualified separately. The maintained build target is
tools/build.ps1 -Target native-studio, with explicit -VerifyNativeStudio and
-DesignerSourceDirectory for the actual-controller qualification.

Checked installed FPC 3.3.1/LCL Win32 evidence under build/native-studio/:

- controller-qualified-orchestration.log: 74 actual native editor checks, zero
  owned warnings and zero retained allocations. Source is the unchanged earlier
  MCP export 93bd930faac0d6ef69fd35cf1f1c6e2a. The journey covers inherited canvas
  proposals/recipe independence, page/reusable creation/use/return, property
  editing, multiple callbacks/TODO navigation, warned removal/paired Undo,
  retained Unicode source focus/scalar ranges, compact navigation, optional
  outputs, real saved files, independent-writer conflict and warned recovery.
- controller-final-build.log / editor-title-geometry.log: the immediate title
  update branch passes in the focused 17-check actual editor journey, zero leaks.
  controller-final-application-build.log compiles the standalone executable.
- controller-layout-regression.log: 2,169 actual native layout/control checks
  with zero leaks; controller-browser-layout-qualified/capture.dom.html passes
  2,214 executed browser checks. Owned native warnings are zero; current pas2js
  retains only seven installed classes.pas warnings.
- controller-browser-authoring-pass2 and controller-browser-authoring-narrow:
  69 ordinary browser editor checks each, the latter measured at 390 pixels.
  controller-browser-resize passes 21; controller-browser-split passes nine
  host transitions and the existing 20 child interactions, retaining the draft.
  The first desktop authoring request had a missing host HTML/404; the admitted
  host subsequently executed. The first layout capture requested the wrong
  passed marker; the corrected bounded capture retains the actual 2,214 result.

The user clarified that starters/demos should use English, while Nyx retains
broader text support. The actual default document already has English copy,
confirmed with bounded native MCP property queries at revision 6. The reusable
review's initial memo/name now also use English. Dedicated Unicode title, memo,
draft, source/history and file checks retain their multilingual inputs. This
preference is recorded in AGENTS/PROJECT; user-authored text is never rewritten.
The real Pascal MCP client creates an owned review, groups nine changes at its
expected revision, exports bounded accepted source and asks both application
compilers. Both immutable jobs succeed/current, with exact source fingerprint
15ae74c977f0bb28909f5caae32f17cc (3,352 bytes); the review is retired and the
published primary content/history/navigation frame remains unchanged. Receipts
are in source-english/ and english-semantic-review.log. The older staged server
library still emits owned warnings; these jobs establish exact source/compilation
only. Current repository native/browser consumers qualify the repaired library.

English review qualification: english-designer-controls.log passes 32 actual
native designer/view checks with zero leaks, then compiles pas2js with only the
seven installed RTL warnings. english-designer-desktop/capture.dom.html passes
32 and english-designer-narrow/capture.dom.html passes 33 in the actual 390-pixel
iframe. The first desktop capture command supplied the wrong attribute; its
retained DOM already contains the correct terminal passed marker/count, verified
independently in english-designer-desktop-marker-proof.json without re-running
the UI. The narrow capture uses the declared marker and exits zero.
english-native-editor.log passes the complete 74-check controller journey with
the new unchanged English MCP export and zero leaks/owned warnings. Its actual
desktop capture shows English memo/name text. This rerun retains the Unicode
editing/draft/file cases. Final visual inspection of the refreshed full English
journey shows its memo painted inside the compact canvas; it does not explain
or establish a fix for the earlier blank capture.
english-native-geometry.log passes 18 focused controller checks with zero leaks:
Unicode title input updates source immediately, initial review presentation
returns to English, pending Unicode source survives parked views and the actual
memo intersects its 390-pixel canvas viewport. The final geometry and full
callback compact PNGs were inspected; both paint their English memos. All current
semantic-author/native/browser capture handles have terminal results. No passing
micro-journey substitutes for reliable full native visual qualification.

Failures and incomplete outcomes remain retained. The independent fixture writer
initially aliased expected revision with its returned revision and correctly
failed; a distinct output fixes the fixture without weakening the store guard.
Actual PNGs were captured from native controls. The earlier full 74-check callback
journey retained its compact controls/source yet captured a blank canvas; the
refreshed English full journey and short geometry journey both paint their actual
memos. The cause of that intermittent discrepancy remains unqualified, so it does
not establish reliable narrow native visual parity. Native sidebar horizontal
overflow/clipping and native editor performance remain open. The checked full
journey's cumulative allocations are not a release-performance measurement.
Original captures are retained under editor-before-english/ as review inputs.

No production listener, profile, private enrollment or primary project was
replaced. Native named MCP remains connected with fifteen tools; its published
revision-6 selection/view/history/draft frame is unchanged. Existing staged
services retain their verified executable identities. The earlier automatic
updated-listener launch rejection (blocked by policy) was not retried through
another route. Compiler/MCP observation, native workspace jump/return, compiled
reload and general native import/export remain open; the candidate reports its
missing service connection rather than claiming those actions ran.

No original acceptance criterion closes and no task moves to DONE. The original
NS-4 no-closure count advances 3 to 4. Reassessment ends adapter/controller-only
fixture expansion; next connect the runnable editor to existing HTTP/MCP project
transport, qualify actual observed edits and workspace navigation with retained
paired state, and qualify reliable compact painting after the longer journey. Live
warned workspace closure and closed-context compiler completion still retain
their separate refused-launch gate. Full Nyx/Studio remains the active goal.

Publication for this packet is committed on hello-nyx and pushed only after the
maintained checks and final evidence review. Exact remote-head verification is
retained privately at
.local/codex-restart-check/native-studio-controller-remote-proof.json. The earlier
adapter checkpoint 18b97e7 remains independently verified. Current publication
does not establish replacement of the LAN or staged service executables.

Current continuation: the preceding turn is verified progress; b8594ee is pushed
at the exact remote head. Original authoring 2/4/7 now own native editor/service
integration, with workflow 6 consuming its fixed project context. Deliver the
same shared agent-exchange protocol behind browser/native transports, connect the
actual native editor to an existing service, expose activity/permissions, and
qualify observed semantic edits plus project jump/return with independent paired
drafts/history/presentation. Native HTTP must run outside the UI thread, with
bounded lifetime and cancellation before its callback receiver/session dies.
Stop on retargeted requests, primary leakage, lost drafts/history, notification
destruction or source adoption after cancellation. Use the existing staged
listener; do not repeat the refused updated-server launch through another route.
The original no-closure count remains 4 until this deliverable is assessed.

## Native editor service integration — 2026-10-05

Original authoring criteria 2/4/7 own this packet; semantic workflow 6 consumes
its fixed project context. The shared agent protocol now owns revision admission,
protected recovery, source/currentness, paired publications/history and activity,
while browser XHR/timers and native HTTP workers/UI timers own platform work.
Typed editor Undo/Redo replaces raw authoring direction strings. Native worker
inputs/replies are owned UTF-8 bytes; workers never borrow widgets/controllers.
Requests/replies have a 16 MiB native byte cap. Per-wait five-second HTTP timeouts
do not establish a whole-request deadline. Cancellation detaches UI delivery;
server admission may already have occurred. All native context receivers detach
before view release/worker joins. Ordinary built Studio remains compiler/server
independent; optional launch arguments select an explicit loopback origin/context.

The actual native controller now adopts MCP-authored projects, observes semantic
edits, publishes actual controls, routes history to the server, exposes operator
permissions/activity and switches independent full-editor contexts. Successful
target admission and acknowledged local publications precede a jump. Pending
draft/base, accepted/design bytes, source visibility/range/focus and stored scroll
positions are scoped to each native context; machine output settings stay shared.
Failed attachment retains local recovery; explicit adoption saves a local paired
backup first. Diagnostic metadata requires an exact synchronized accepted source.
These paths do not establish native build requests or compiled execution.

Qualification uses the unchanged identity-verified staged listener (PID 4972,
project-workspaces/server, loopback 8278/8279). It has nine workspace summaries:
primary plus all eight project slots. The bounded read-only probe confirms
capacity; qualification uses explicit reuse instead of new allocation or reset.
Exact-revision explicit reuse
selects two previously owned test contexts, adds nonce-owned English review pages
with one semantic composition each and performs actor-bound reviewed root cleanup.
Both original design/source pairs return byte for byte. Existing history retains
ordinary test commands. The staged primary's revision-25 pending draft/frame and
published primary's revision-6 content/navigation/history frame remain intact.

Current packet artifacts are under build/native-studio/transport/. The connected
native journey passes 49 actual checks, including semantic observation, actual
Unicode memo input, authoritative Undo/Redo, independent pending draft/history,
full project jump/return, exact scalar source range/focus, and real 390-pixel
rendering. Deferred HTTP replacement replies on the UI thread once; canceled
timers/requests and active destruction have no borrowed receiver notification.
Heap tracing reports zero leaks. Desktop/narrow PNGs were inspected: both paint
their English memo. This does not establish reliable painting across all longer
flows, sidebar overflow fixes, other widgetsets, hardware/IME or accessibility.

Browser protection/coalescing/cleanup passes 14 actual checks using an explicit
owned project, restoring its exact accepted pair after private Unicode typing.
Shared authoring passes 69 desktop / 69 in an actual 390-pixel iframe. Native
offline full authoring passes 74 with zero leaks, retaining optional outputs,
callback/source/draft and paired-file behavior. Final native-entry/consumer builds
have zero owned warnings. Browser builds retain only seven installed RTL warnings.
Cumulative checked-fixture allocations are not release-performance measurements.

Final retirement requalification uses service-tests-build-retirement.log and
service-retirement.log: the same complete connected journey passes 49 with zero
leaks after all context receivers detach before joins. Its desktop/390 captures
were inspected. geometry-readiness.log passes 22 focused actual native checks
with zero leaks, including both accurately unavailable Build actions retaining
the exact project and the existing English compact memo/source journey. Its
actual geometry PNG was inspected. native-entry-retirement.log rebuilds the
runnable product with zero owned warnings. All current native/capture handles
have terminal results; no primary pair is replaced.

Retained failures: the capacity probe supplied an unsupported limit field before
using the server's fixed bounded response. The first native full journey reached
its return checks, then root cleanup correctly refused a string instead of the
required root/id object. The original pairs and nonce root were retained; a fresh
exact-revision semantic recovery performs reviewed cleanup and passes nine checks
with zero leaks. A corrected full journey then passes. A recovery compile command
initially split its compiler unit arguments; proper argument-array grouping fixes
that orchestration error. The first explicit browser-context compile lacked the
JS decoding unit; the corrected maintained fixture compiles and executes. No
mutation is blindly retried after ambiguous delivery.

No service, profile, private enrollment or primary project is replaced. Browser
artifacts alone are staged into the existing qualification web root. Production
retains fifteen native named tools; the staged project service retains seventeen
and reports unavailable closure. The previous automatic updated-listener launch
rejection (blocked by policy) is not retried. Live warned closure and closed-job
currentness retain that separate gate.

No original criterion closes or task moves to DONE. The original authoring
no-closure count advances 4 to 5. Reassessment selects actual native compiler
request/diagnostic and compiled-preview integration next, with explicit missing
output readiness, rather than extending another navigation fixture. Complete
native project presentation/import/export, whole-request deadlines, reliable
visuals/overflow and measured performance retain their original owners. Full
Nyx/Studio remains the active goal. This source packet is checkpointed/pushed on
hello-nyx; exact remote proof is retained privately in
.local/codex-restart-check/native-service-remote-proof.json.

## English presentation follow-through — 2026-10-05

The user's English starter/demo preference remains the current steering. Bounded
named MCP reads confirm the published welcome badge and root at revision 6;
the starter, browser demos and initial designer/keyboard/layout review operations
contain English copy. Two composition screenshot fixtures still displayed a
multilingual qualification suffix. Both now show `My activity`, with comments
preserving the distinction between presentation defaults and dedicated Unicode
qualification. No application text contract or multilingual input fixture changes.

Native visual qualification rebuilds with zero owned warnings, captures all five
maintained light/dark/narrow/composition/custom-theme views, passes their actual
theme-pixel checks and reports zero leaks. The browser composition view executes
15 computed-style/geometry checks through the identity-verified existing staged
listener. Its PNG and native composition PNG were inspected; both show English
copy. Evidence is under build/english-visual/. Browser compilation retains the
seven installed RTL warnings, with zero owned warnings. No listener, profile,
project pair, selection or history is replaced; only this standalone browser
visual artifact and its host are staged into the existing qualification web root.

This bounded preference correction accepts no original product criterion and
does not reset the native authoring no-closure count of 5. Native compiler
integration remains the selected next deliverable. Its uncommitted typed private
build/profile protocol and incomplete controller draft are not qualified or
included in this presentation checkpoint. Finish that controller and exercise
currentness, profile admission and diagnostics before claiming native builds;
compiled execution/reload and the recorded updated-listener gate remain open.

## Current batch: native asynchronous compiler consumer — 2026-10-05

The preceding goal turn made verified progress: both presentation fixtures now
start in English, executed browser/native captures pass, and remote hello-nyx
matches 7a4956f. Original authoring criteria 2/3/4/7 and compiler-service criteria
1/2/3/4 own this continuation; reliable reload criterion 2 retains compiled
activation/execution. The deliverable is the native Build/output/diagnostic
consumer of the same bounded immutable jobs as semantic MCP, followed by compiled
preview integration. Readiness and machine profiles remain optional for design.

Qualification must exercise real compilers, profile conflict/failure admission,
exact source/project currentness, active native controls, inactive project jobs,
and receiver retirement. The existing updated-listener rejection is not retried;
direct public protocol qualification can establish source/controller behavior,
but cannot establish HTTP deployment. Stop if a request is retargeted, stale
results activate, editor synchronization freezes on a build refusal, or profiles
enter portable project history. Record the separate live-HTTP gate and remaining
compiled-execution checks instead of claiming acceptance from compilation alone.

## Native compiler and compiled-preview consumer — 2026-10-05

Original authoring criteria 2/3/4/7 and compiler-service criteria 1/2/3/4 own
this source packet; reload criterion 2 consumes compiled activation/execution.
The native editor now uses the same guarded immutable compiler jobs as semantic
MCP. Typed managed requests carry distinct root/output/operation/job references
and enum targets/scopes. Trusted private editor authority is separate from agent
permissions; revision, scope, pending-draft and output admission still apply.
Build envelopes validate observation metadata before admitting effects. Profile
saves compare output identity, validate before persistence and retain accepted
configuration on failure. Machine paths stay outside portable pairs/history.
Delayed profile reads preserve local field changes; native views continue to
borrow the same stable output object. Older servers advertise no native build
capability and retain accurate unavailable behavior.

Each native context owns its captured pair/profile, job, bounded status timer
and result. Workers own bytes/immutable snapshots, not widgets. Job replies
cannot retarget the bridge or freeze ordinary synchronization on an admission
refusal. Current diagnostics use actual accepted source and retain scalar
line/column through the native renderer. Preview preparation admits an exact
succeeded/current artifact, enforces a 32 MiB ceiling, refuses redirects and
checks downloaded size/MD5. MD5 establishes delivery consistency, not identity.
A second source/output query precedes launch. The adapter owns its process and
private files; cancellation detaches callbacks before joins. Successful current
rebuilds replace a running preview only after candidate launch. Stop owns preview
execution; the final reviewed change also retains independent compiler admission
and observation rather than abandoning a pending job.

Qualification is under build/native-studio/compiler/. The corrected maintained
command passes 105 actual native protocol/control checks with zero leaks:
real browser page/reusable jobs, native application/view jobs, operator-disabled
builds, persistence/revision refusals, exact paired-source/profile fingerprints,
actual owned Run/Stop/re-Run, a running-view rebuild with old-process retirement,
compiled reply input and independent reusable actions, a real helper compiler
error, exact supplementary-Unicode navigation and changed-source refusal. This
105-check run precedes the final Stop-during-build correction; its final consumer
check is recorded below when terminal. Do not sum overlapping journeys.

The focused compiled-artifact journey passes ten with zero leaks, including
actual HTTP download/verification, compiled controls, mismatched-byte refusal
retaining the old process, refused Launch, canceled notification and owned
retirement. Focused diagnostics passes 35 with zero leaks, through the actual
editor/private protocol/real compiler, at scalar position 3547. The shared
request/currentness/artifact contract passes 43 natively and 43 in executed
pas2js. Actual browser shared authoring passes 69 desktop and 69 in an exact
390-pixel frame. Native product/server and browser product/consumers build with
zero owned warnings; seven installed pas2js RTL warnings remain visible.

The protocol engine is suspended and never starts its listener. Actual artifact
downloads use the unchanged identity-verified qualification listener, with only
the test's exact immutable manifest files copied into its explicitly selected
artifact root. This qualifies direct private-protocol/controller behavior and
HTTP artifact transport, not deployment of the new editor HTTP route. No
production/staged service, machine profile, enrollment or active user pair is
replaced. Production retains its fifteen named tools and revision-6 content /
selection / view / draft / history frame. The earlier automatic updated-listener
launch rejection ("blocked by policy", no additional reason) is not retried.

English desktop/narrow compiled-control captures were inspected: Run/Stop and
the memo are painted. Dedicated diagnostic qualification retains supplementary
Unicode; initial demos/reviews remain English. Sidebar clipping remains visible;
the diagnostic capture includes preceding Agents content and is not evidence of
a viewport-visible source caret. Focus/ranges are separately actual-control
evidence. Reliable visuals, layout/performance, other widgetsets/operating
systems, native browser-launch lifecycle, inactive-project build completion,
general import/export/assets and live HTTP deployment remain open. Checked
cumulative fixture allocations are not a performance measurement. HTTP/download
timeouts are per wait, not a whole-request deadline; joining stalled workers
still needs that original acceptance path.

Retained failures: a fresh private test repository initially omitted its library
directories; orchestration now creates explicit private source links and the
Pascal consumer checks them before compilation. Cross-process input first used
GetWindowText for edit text, then BM_CLICK for the custom Lazarus button. The
focused consumer uses bounded WM_GETTEXT and the actual Space-key contract, pairs
controls by reusable ownership and selects the exact document-titled form rather
than a Lazarus helper window. The Win32 text distinction is documented by
[Microsoft](https://learn.microsoft.com/en-us/windows/win32/api/winuser/nf-winuser-getwindowtextw).
The diagnostic assertion initially mixed accepted LF text / one-based byte
indices with native physical text / zero-based scalar ranges; the corrected
consumer converts the actual control prefix and adds a supplementary character.
All failed terminal logs remain retained. No live mutation is blindly retried.

Final maintained consumer evidence: qualified-stop-continuation.log terminates
with PASS 123 native compiler protocol/control checks and zero unfreed blocks.
It supersedes the overlapping 105-check consumer above. Stop during a pending
view build retires the existing owned process, preserves the exact build receipt
and result, does not restart the preview implicitly, and permits a subsequent
explicit Run. The helper error still focuses the real source control at scalar
position 3547; exact changed-source refusal and listener-free retirement pass.
The final desktop/390 PNGs in the unique protocol-061fcab6-d882-4f63-a410-1fc1ca68a1fc
repository were inspected: English reply copy and both Run/Stop controls paint;
the previously recorded clipping/presentation limits remain. This is checked
Win32 control/protocol evidence, not hardware/IME, accessibility or performance.

Handoff/reassessment: no full original authoring criterion closes; the consecutive
native-authoring no-closure count advances from 5 to 6. Criteria 2/3/4/7 and
the original prerequisite tasks remain open, as do the complete service/reload
criteria. The current stop point is the qualified compiler/preview source
candidate and its remote checkpoint. Do not add another stable navigation or
controller diagnostic fixture. The next bounded deliverable follows original
NS-5_service-reload_01 criterion 1: implement a whole-request deadline and qualify
stalled-worker retirement with detached receivers. One maintained stalled
transport consumer must establish the elapsed bound and owned cleanup; switch
to the failed boundary if that gate fails, rather than adding successful-only
journeys. New editor HTTP qualification remains required when execution is
available; the recorded automatic updated-listener refusal is not retried.

Final preservation reads confirm production revision 6, home selection/view,
one page/one component, no draft, Undo unavailable and ordinary test Redo retained.
Both production and staged listener identities are unchanged. Current native MCP
read activity is expected; no active design/source/profile is replaced. The
English demo checkpoint 7a4956f remains in branch history. The compiler/preview
commit and exact remote-head comparison are recorded privately in
.local/codex-restart-check/native-compiler-preview-remote-proof.json after push;
machine paths/accounts/endpoint configuration are excluded from the checkpoint.

## Typed transport deadlines and stalled retirement — 2026-10-05

Previous goal turn classification: progress. Checkpoint ff46074 changed the
native compiler/preview consumer, qualified 123 actual cases and matched the
remote branch. This continuation follows its selected NS-5_service-reload_01
criterion-1 prerequisite: a whole-request deadline and detached stalled-worker
retirement. The bounded deliverable covers shared private editor transport and
native artifact downloads, not all compiler-service cancellation or deployment.
One maintained stalled peer/consumer must prove elapsed bounds and owned cleanup;
stop/switch on late callbacks, expired admission, retained files/processes or
changed project pairs. All original criteria and hard prerequisites remain.

NewNyxTransportPolicy supplies a managed fluent WholeRequest numeric contract.
Adapters copy typed admitted snapshots; defaults are fifteen seconds for editor
exchanges and thirty for native downloads. Zero/default records and invalid
alternative implementations refuse before work. Native requests include time
queued behind canceled work. A worker-owned monotonic lifetime and thread-safe
cancel event gate nonblocking reads/writes; readiness polls are at most fifty
milliseconds. Partial headers/body and upload progress never extend the budget.
Numeric loopback connect uses remaining time capped at five seconds. Browser
XHR uses its whole-request timeout. Timeout refuses partial bytes/artifacts and
returns bounded help; cancellation cannot revoke existing server admission.

Maintained command: tools/build.ps1 -Target native-studio -VerifyTransportDeadlines.
build/transport-deadline/maintained-compatible.log terminates with 66 actual
native transport checks and 20 actual real-clock browser checks, with zero
unfreed blocks. Native failures for a 220 ms policy arrive at 266/281/281 ms,
including UI delivery; blocked upload arrives at 281 ms. Canceled editor/preview
retirement is 63/62 ms. Browser silent/header/body waits are 236.7/231.3/231.9 ms;
its bounded status is browser-maintained/browser-status.json. Queued expiry
sends no new packet, canceled replacement still receives its exact reply, and
destroyed request/timer receivers receive no notification. These measured bounds
are qualification results, not hard real-time or performance guarantees.

The raw Pascal qualification peer owns an OS-selected loopback socket, bounded
test packets/files and at most 64 connection workers. It is a byte producer
without Studio/MCP authentication, profiles, design state or configuration;
it never starts/replaces the rejected updated Studio listener. Synthetic bytes
qualify preparation only and are never launched. A separately rebuilt actual
compiled-input consumer passes ten with zero leaks in compiled-probe-run.log:
immutable HTTP bytes, actual memo/Space-key actions, independent reusable input,
mismatch refusal retaining the old process, canceled notification and owned
retirement. That retained artifact does not establish live source currentness.
Stable FPC 3.2 compiles the native socket unit; actual native execution uses
the installed FPC/LCL 3.3.1 Win32 toolchain. Current native and browser product
entries build with zero owned warnings; seven installed pas2js RTL warnings
remain visible. Other widgetsets/OSes, disk stalls and full reload stay open.

Retained failures: an initial test manifest had mismatched nested syntax;
the corrected fixture uses a separate typed manifest entry. Unquoted pas2js
arguments were split by PowerShell; explicit argument arrays fix orchestration.
The screenshot runner's accelerated virtual clock failed the elapsed assertion;
the maintained Pascal browser host runs the real clock and passes, without
weakening that assertion. A browser-host local declaration initially landed in
the wrong scope and was corrected. Stable FPC lacks the newer FCL handler Select
API; the adapter now uses native readiness behind the same nonblocking contract.
All terminal failure logs/DOM captures remain; no ambiguous mutation is retried.

Handoff/reassessment: no full original criterion closes, and the existing native
authoring/prerequisite no-closure sequence advances 6 to 7. The whole-request
transport deliverable is qualified; stop adding deadline/controller fixtures.
NS-5 criterion 1 still owns running/queued build cancellation, retention,
isolation and bounded shutdown. Source inspection found worker termination is
checked before execution only; RunCompiler owns a 60-second loop and terminates
on budget/log overflow without an explicit exit join. This is an existing gap
under that owner, not a new task or accepted cancellation outcome.

The next action follows the unmet NS-1_codegen_01 prerequisite, criterion 3's
large-project editing responsiveness, at its existing no-closure counter 11.
Use the existing correctness-gated whole-command workload to deliver an
integrated visual edit/structural edit/Apply improvement, retaining fresh full
candidate admission, exact authored frames and paired history on both targets.
Budget one implementation/consumer packet, then reassess; stop on pair/draft
corruption, stale mutation visibility or failed preservation. Do not add another
isolated lexer timing report or reset the original count. New editor HTTP
qualification remains required when execution is available; the recorded
automatic updated-listener refusal is not retried.

Preservation: named MCP still reads production revision 6, home selection/view,
one page/component, no draft/Undo and ordinary Redo. Production PID 29656 and
staged PID 4972 retain their exact prior executables. No active pair, profile or
enrollment is replaced. Source delivery is separate from production deployment;
the remote checkpoint proof is kept privately under
.local/codex-restart-check/transport-deadline-remote-proof.json.

## Paired source admission and canonical builder reuse — 2026-10-05

Previous goal turn classification: progress. Typed transport checkpoint b786c71
matched the remote branch and qualified its bounded deadline/retirement consumers.
This continuation follows the recorded return to NS-1_codegen_01 criterion 3,
retaining its counter 11 and full large-project source synchronization outcome.
The bounded implementation removes duplicate whole-command preparation, not
another isolated lexer study. Stop on lost authored frames, stale public model
mutations, invalid candidate publication, broken paired history or ownership.

Source Apply now prepares independently owned document/companion outputs together
from one complete draft admission. The portable reconstruction helper parses the
exact full source, replays a fresh document, validates all properties/ownership
and admits its canonical encoding. Visual verification calls this helper without
generating an unused verifier baseline; exact comparison uses the encoding just
admitted instead of encoding the same candidate twice. The session publishes
only the prepared pair through its ordinary paired checkpoint.

Each workspace retains derived canonical builder text for its accepted design.
Every public Render still freshly encodes the current tree; a direct model edit
cannot disappear behind the cache. Proposed frames/cache publish only after
verification. Reset and both Restore paths clear the derived text; the first
subsequent edit reconstructs it from the exact accepted canonical design.
Immutable history, recovery JSON, fifty-command retention and the 16 MiB history
text budget are unchanged. No document, reader, renderer or parse array is
retained. Three finite specialized Pascal-name indexes initialize before worker
reads and contain no application identities. Native finalization releases them;
pas2js owns them with the module's execution context.

Evidence is retained under ignored build/source-responsive/:

- focused-build.log / focused-run.log: 83 checked native source/diagnostic cases,
  including eighteen new paired admission/cache-restoration cases. Exact accepted
  pair/draft retention, wrong types/boundaries, direct mutation repair, independent
  prepared ownership, reset and typed/wire restoration pass. Heap tracing reports
  zero unfreed blocks. These cases overlap the complete suite below.
- core-build.log / core-run.log: 30 core + 1560 composition/designer checks pass.
  emitted companions remain under core/. generated-build.log / generated-run.log
  execute eight native compiled outcomes: persistence, Unicode identity/event
  ownership, structural creation/reuse, crafted names/comments/expressions,
  legacy migration/helper, handwritten helper and live runtime defaults/bindings.
  This execution also reports zero unfreed blocks.
- native-studio-build.log: maintained `tools/build.ps1 -Target native-studio`
  compiles the current standalone product consumer, without launching a service
  or requiring application output compilers. Owned warnings are zero.
- browser-focused-build.log, browser-authoring-build.log, browser-studio-build.log,
  browser-shared-corrected-build.log, browser-controls-final-build.log,
  browser-events-final-build.log and generated-browser-build.log compile the
  changed focused/shared, real editor/control/event, complete Studio and emitted
  companion consumers with pas2js/matched installed RTL. Seven dependency RTL
  warnings remain visible; zero owned warnings. Compilation is not execution.

The current and exact b786c71 archived ordinary LCL fixture initially fail the
same strict width-binding assertion. lcl-run.log / before/lcl-run.log retain it;
lcl-diagnostic-run.log establishes model width 420, actual width 126 and parent
width 150. The hidden mounted application's child host still has its provisional
unrealized size. The fixture now shows its owned window and pumps native alignment
before checking bindings; the assertion still requires exactly 420. The first
realized run qualifies 42 managed controls, 35 event registrations, 50 runtime
bindings and 71 actual Studio source/state/binding authoring checks, plus native
theme, Unicode recovery, 75 projections, reusable customization and desktop/
compact public shell controls. It reports zero unfreed blocks. Hardware/physical
input and browser pixel behavior are not claimed.

The ordinary LCL harness also exposed nine pre-existing owned warnings outside
the earlier bounded warning inventory. Explicit portable text comparisons,
direct native UTF-8 widget input and exhaustive inspector effects remove them,
without suppressions or dependency edits. Its failure path now lets exception/unit
owners unwind before heap reporting. lcl-final-build.log reports zero warnings;
lcl-final-run.log owns the repeated actual-consumer result after these fixture
changes. NS-6 delivery remains open with its original count and full CI/platform
requirements; this incidental repair does not restart the inventory task.

Before/after ordinary timing uses the exact archived b786c71 sources and current
sources, checked FPC 3.2.0, identical flags/workload, sequential execution and idle
compiler/other native control fixtures. All original crafted name/comment/Unicode/
expression, structural, exact paired history and rejected-draft gates pass. UTF-8
source sizes remain 25094/98822/400022 for both versions; fixture sizes/gates are
unchanged. These measure portable complete commands, not paint, trusted input,
network or compilation. before/native.csv / after/native.csv retain full rows.

| Controls | Apply before/after ms | Visual before/after ms | Structural before/after ms | Three history operations before/after ms |
| ---: | ---: | ---: | ---: | ---: |
| 128 | 78 / 78 | 250 / 188 | 312 / 265 | 78 / 93 |
| 512 | 328 / 328 | 1031 / 797 | 1312 / 1063 | 359 / 359 |
| 2048 | 1594 / 1485 | 4421 / 3297 | 5484 / 4390 | 1625 / 1656 |

Largest native visual editing improves about 25% and structural editing about
20%. History/rejection show no material gain; small timer-resolution differences
are not improvements. Opt-in before/native-profile-512.csv and after/native-
profile-index-512.csv retain the whole-command decision: unused old-generation
work is absent on the warmed edit; fresh complete candidate verification remains.
Largest commands still take seconds, so comfortable large-project authoring is
not accepted. Historical browser timings are not compared with these native rows.

Automatic approval review rejected launching the separate installed Pascal static
fixture listener as "blocked by policy", with no further reason. The combined
baseline-browser compile/capture/launch command did not execute. No listener,
profile, frontend or configuration was published, and no equivalent launch route
was retried. Current browser execution/timing and observing Studio qualification
remain pending. The real-clock capture option compiles and rejects unknown modes
before starting a browser (capture-invalid-mode.log); it does not establish a
current browser run. Keep the independent previous updated-Studio listener
refusal and protected production/stage services intact.

Retained corrected failures: browser-build.log first rejects an unsupported
pas2js finalization section; native-only finalization with documented browser
module lifetime fixes it (after/browser-build-corrected.log). An attempted native
file-writing core entry point is unsuitable for pas2js (browser-shared-final-
build.log); the maintained browser shared entry point compiles instead. No source
admission/ownership test was weakened and no dependency or compiler was reinstalled.

Criteria 1/2 stay accepted; original codegen criterion 3 stays open, counter 11→12.
No parent task, north-star credit, target/product completion or new deployment is
claimed. Reassessment ends this duplicate-preparation cleanup. First qualify the
unchanged browser candidate when a permitted fixture host is available, without
an equivalent rejected launch. The next safe implementation targets complete
candidate/property-metadata admission plus ordinary Studio interaction costs,
using operation-owned immutable facts. Preserve fresh complete admission, public
mutation visibility, exact authored frames, original workload and paired history;
stop on corruption or failed ownership/preservation. No isolated lexer report,
grammar expansion, counter reset or transfer to a narrower owner substitutes it.

Production/stage process identity and named semantic session are rechecked before
publication; the operator's accepted pair, view/selection, draft and Undo/Redo
remain untouched. The exact checkpoint/remote proof is private at
.local/codex-restart-check/source-responsive-remote-proof.json after push. Machine
paths, accounts, endpoint configuration and test outputs stay outside the commit.

## Fresh property admission and inspector metadata — 2026-10-05

Progress: the current bounded NS-1_codegen_01 criterion-3 delivery changes the
shared descriptor builder and its ordinary Studio/source consumers. It does not
close the complete source/performance outcome. Solo execution continues; running
production/stage services and the user's revision-6 pair remain preserved.

Admission now shares all inspector descriptor rules while omitting presentation
titles/help/capability construction. Every call reads current domains, creator
schemas, inherited defaults and actual platform overrides. Complete contract,
callback, structural, realization and persistence gates remain fresh. Public
metadata still returns every presentation field. Query-owned geometric arrays,
closed attribute positions and one resolved projection/primitive value remove
repeated copying/lookup work. These facts live only for the current operation:
there is no document cache, registry alias or retained node/view reference.

The new Pascal property benchmark consumes public selection, metadata and
BuildNyxStudioView with expanded properties and code visible. It asserts selected
values, typed support and unchanged accepted source/draft state. Its snapshot mode
emits every public metadata field with both present platform scopes. The maintained
source-workspace target compiles both benchmarks without heap tracing, runs
functional checks with tracing and stages browser artifacts to the explicit
private output. Its source workload and preservation assertions are unchanged.

Evidence is under ignored build/property-responsive/:

- focused-final-build.log / focused-final-run.log: 103 checked FPC 3.2.0 cases,
  including 20 new public behavior cases. Independently returned arrays, changed
  reusable domain/defaults, large creator schemas, builtin/scoped replacements,
  Unicode multiline defaults, invalid recognized values and removed scopes pass.
  Native ownership reports zero unfreed blocks; these cases overlap the core suite.
- core-latest-build.log / core-latest-run.log: current final source passes 30 core
  and 1580 composition/designer checks with zero unfreed blocks. Earlier
  core-final logs retain the same counts before shared projection resolution.
- generated-build.log / generated-run.log: newly emitted companions execute eight
  native outcomes, including crafted source/history, structural/reusable creation,
  Unicode identity/event ownership, handwritten helpers and live state/bindings.
  Zero unfreed blocks. generated-browser-build.log compiles their pas2js counterpart.
- lcl-final-build.log / lcl-final-run.log: actual Win32 LCL controls qualify 42
  managed, 35 event, 50 binding and 71 Studio source/state/binding authoring checks,
  ordinary/expanded inspector help, compact shell, optional outputs, themes,
  reusable customization, 75 catalog projections and Unicode recovery. All
  151797781 allocated blocks are freed. Programmatic actual controls do not
  establish hardware input, another widgetset, browser pixels or full-editor UX.
- native-studio-final-build.log compiles the current standalone product consumer,
  without launch/configuration/service changes. maintained-build.log executes
  source-workspace with isolated BrowserOutput: 103 native checks/zero leaks,
  both Pascal benchmarks and browser counterparts compile, three static hosts
  and the matching RTL stage outside the live frontend. No server is launched.
- browser-focused-final-build.log, browser-property-final-build.log,
  browser-shared-final-build.log, browser-studio-final-build.log and
  browser-source-build.log compile current focused/shared/Studio/benchmark consumers.
  All checked native/owned builds have zero warnings; seven installed pas2js RTL
  warnings remain visible. Dependency source/toolchains are untouched.
- before/metadata.jsonl and after/metadata-final.jsonl contain 4104 equal rows for
  all 76 catalog kinds and both present scopes. Exact SHA-256 equality establishes
  unchanged keys/order/types/defaults/choices/bounds/titles/advanced/support fields,
  not browser execution. Snapshot and all timed commands exit zero.

Before/after timing uses the exact archived 9d564b9 source, current source and the
same checked FPC 3.2.0 flags without tracing/profiling. Both native fixtures run
serially after compilers and other native fixture processes finish. All original
crafted name/comment/Unicode/expression, structural, rejected-draft and paired
history assertions pass. Source UTF-8 sizes stay 25094/98822/400022. Full rows
remain in before/source.csv / after/source.csv and before/property.csv /
after/property.csv; these are whole portable commands, excluding rendering,
physical input, HTTP latency and compilation.

| Controls | Apply before/after ms | Visual before/after ms | Structural before/after ms | Three history operations before/after ms |
| ---: | ---: | ---: | ---: | ---: |
| 128 | 63 / 47 | 188 / 140 | 265 / 203 | 93 / 47 |
| 512 | 328 / 188 | 781 / 593 | 1047 / 860 | 359 / 172 |
| 2048 | 1485 / 984 | 3281 / 2547 | 4297 / 3563 | 1625 / 891 |

Largest full property admission measures 406→156 ms. Sixty-four ordinary
selection/metadata queries measure 32→0 ms in that timer sample; zero indicates
native timer resolution, not free computation. Full shared Studio shell
composition remains 156 ms, so its wider cost/target painting are not improved
by this evidence. Large source commands still take seconds and are not accepted
as comfortable large-project authoring. Historical browser timings are not
substituted for this packet.

Retained construction failures: focused-build.log catches an accidentally copied
unit declaration in the new fixture; corrected files compile/run in the final
logs. after/property-build.log catches a nonexistent draft-query member; the
benchmark uses the public draft-base contract and its unchanged-source assertion.
No product admission/ownership gate or workload was weakened to pass either fix.

Current browser execution/observing qualification remains pending: automatic
approval review previously rejected the separate static fixture listener launch
as "blocked by policy", with no further reason. No equivalent launch was retried;
no frontend, listener, machine profile, enrollment or configuration was published.
The previous updated-Studio launch gate and all protected services remain intact.
Named semantic session/node reads verify bounded revision-6 context; they query
the existing production build, not execution of this new source candidate.

Criterion 3 remains open and its original no-closure count advances 12→13.
Native authoring's count 7 and delivery's count 1 remain unchanged. Stop this
bounded metadata optimization; next address complete reconciliation/structural
command responsiveness together with ordinary editor use, using existing owned
source contexts and the same exact preservation/history/admission workload.
Browser/observing evidence still needs a permitted host without an equivalent
refused launch. Do not expand grammar, return an isolated lexer report, reset
counters, transfer scope, weaken parity or claim DONE/north-star completion.

Production/stage process identity and named session are rechecked before push.
The private exact remote proof is
.local/codex-restart-check/property-responsive-remote-proof.json. Machine paths,
accounts, personal configuration and generated evidence stay outside the commit.

## Typed vocabulary in complete source reconciliation — 2026-10-05

Progress: exact closed Pascal symbol names now resolve through one immutable
typed index initialized before source consumers/workers run. Whole-command
profiling showed repeated enum construction/scans for every local declaration
dominating reconciliation's symbol facts. Startup keeps original matching order
and first-match precedence; each entry contains only an argument family and
ordinal. No application source/name/value, reader, document, callback or mutable
registry snapshot is retained. Native finalization frees the index; browser
module lifetime owns the table. All mutable source contexts, strict candidate
replay, fresh model/property/encoding admission and exact paired publication
remain unchanged. No Pascal grammar is broadened.

The public source fixture adds 129 cases for typed layout/wrap/alignment/sizing/
touch/variant/action/input choices and every closed attribute's clearing. It
also qualifies mixed-case enum spelling with exact Unicode literal text, ten
reserved locals, wrong-family/unknown arguments, retained accepted pair/buffer/
history and ordinary visual reconciliation with paired Undo/Redo. It inspects no
private table and runs through generated candidates/the same Studio session.

Evidence lives under ignored build/source-reconcile/:

- focused-build.log / focused-run.log: 232 checked native source diagnostics,
  including the new public cases, zero unfreed blocks. maintained-build.log
  repeats them through source-workspace, compiles both benchmarks/browser
  consumers and stages static hosts/matched RTL to explicit isolated output.
  These cases overlap core, not additional totals. No listener is launched.
- core-build.log / core-run.log: 30 core and 1709 composition/designer checks,
  zero unfreed blocks. Fresh emitted companions remain under core/.
- generated-build.log / generated-run.log: eight compiled native outcomes pass,
  including crafted names/comments/expressions, typed creation/reuse/ownership,
  Unicode identity/events, legacy/helper migration and live runtime bindings.
  Zero unfreed blocks. generated-browser-build.log compiles their counterpart.
- lcl-build.log / lcl-run.log: actual Win32 LCL controls pass 42 managed, 35 event,
  50 binding and 71 Studio state/source/binding authoring checks, ordinary and
  compact inspector/shell, optional outputs, theme/reusable factory/customization,
  75 catalog projections and Unicode recovery. All 150238341 allocated blocks
  are freed. Programmatic actual controls do not establish hardware input,
  another widgetset, browser pixels or complete native Studio parity.
- native-studio-build.log compiles the maintained current product controller;
  browser-focused-build.log, browser-shared-build.log, browser-studio-build.log
  and browser-source-build.log compile affected pas2js consumers. Owned warnings
  are zero; seven installed RTL warnings remain visible. No compiler/dependency
  source is reinstalled/edited and no service/controller is deployed.

Ordinary timing compares exact archived b309541 against this candidate using
identical checked FPC 3.2.0 flags without profiling/heap tracing. Commands run
serially after compilers/native fixtures finish. All original crafted name/
comment/Unicode/expression, structural, exact paired history and retained rejected
draft gates pass; source byte sizes remain 25094/98822/400022. Logs are
before/source.csv / after/source.csv. These are complete portable commands,
excluding rendering, trusted input, HTTP/compilation and operating-system scale.

| Controls | Apply before/after ms | Visual before/after ms | Structural before/after ms | Three history operations before/after ms |
| ---: | ---: | ---: | ---: | ---: |
| 128 | 47 / 47 | 141 / 78 | 219 / 94 | 47 / 47 |
| 512 | 187 / 172 | 594 / 313 | 859 / 390 | 172 / 172 |
| 2048 | 985 / 875 | 2531 / 1375 | 3594 / 1735 | 890 / 891 |

Largest visual/structural commands improve about 46%/52%; Apply improves modestly
and history does not. after/property.csv also qualifies ordinary selected metadata
and complete shared shell composition with unchanged source/draft. Largest
composition remains 156 ms; this does not establish faster painting. Values near
native timer resolution, including measured zero, do not establish zero cost.

before/profile-2048.csv / after/profile-2048.csv retain the opt-in explanation:
visual symbol-read falls 1359→251 ms across the same five snapshots; structural
symbol-read 2094→436 ms across eight snapshots. Parent timings overlap their
children and must not be summed. This is whole-command evidence, not an isolated
lexer benchmark; the fresh candidate still parses/replays the entire builder.
Remaining verification, source merging and document encoding are visible costs.
The ordinary timing rows above, not profiled rows, own performance observations.

Browser execution/observing qualification remains pending under the original
separate static listener launch refusal: automatic approval review reported
"blocked by policy", with no further reason. No equivalent launch was retried;
no listener, frontend, machine profile, enrollment or accepted design/source pair
was replaced. Existing production/stage services remain protected. English
starter/review defaults stay unchanged; broader Unicode is qualification input.

Original codegen criterion 3 remains open; its no-closure count advances 13→14.
Native authoring's count 7 and delivery's count 1 stay unchanged. End this bounded
closed-vocabulary delivery; next use complete-command evidence for remaining
sequence-sensitive control/reference access and structural reconciliation with
ordinary editor consumption. Preserve full fresh admission, direct mutation
visibility, independent ownership, exact crafted source, workload and paired
history. Browser/observing still needs a permitted host without an equivalent
refused launch. No isolated lexer report, grammar expansion, counter reset, scope
transfer, weakened parity/DONE gate or full-product completion is earned.

Named semantic session and production/stage process identities are checked again
before push. The exact private proof is
.local/codex-restart-check/source-reconcile-remote-proof.json; machine paths,
personal configuration, accounts and generated evidence stay outside the commit.

## Exact references and fresh document admission — 2026-10-05

Progress: sequence-sensitive candidate members now borrow the exact reader-owned
control interface after construction/adoption, replacing repeated document-wide
ID searches. This covers configuration, bindings, contracts, extensions and
callbacks without changing their typed public contract. The reader retains each
interface until replay ends; independent document/parent owners retain admitted
nodes. Existing grammar cannot remove/rename a local, and explicit guards keep
constructed identity/ownership current. Complete fresh model/property/encoding
admission still rejects implicit recipe identity conflicts before publishing
either accepted owner. No source grammar or retained document cache is added.

The proven private source text index is moved to an internal portable unit and
shared with document identity validation. Each traversal owns a fresh exact-text
membership index; original arrays/tree order still determine diagnostics/output.
Direct renames, exact case/Unicode, hash collisions, growth and cross-root
uniqueness remain visible on every call. Portable storage encodings may hash
differently; exact equality and fresh target-local lookup preserve meaning.
Instances are independently mutable or fully initialized/read-only before sharing.

Fifteen additional public cases qualify direct mutation/rejection/repair, exact
identities and independent source candidates. Duplicating a compound's implicit
parts and configuring before ownership reject atomically, retaining source,
design, rejected buffer and prior Redo. The user-requested English starter/demo
copy remains in sample/recipes; broader Unicode here is dedicated qualification
input. No active demo/project is rewritten by this code packet.

Evidence under ignored build/source-references/:

- focused-build.log / focused-run.log: 247 checked native source cases, zero
  leaks. maintained-build.log repeats the same cases through source-workspace,
  compiles both benchmarks and browser consumers, and stages static hosts/matched
  RTL to maintained-browser/. Counts overlap core; no listener is launched.
- core-build.log / core-run.log: 30 core and 1724 composition/designer checks,
  zero unfreed blocks; fresh compiler companions remain under core/.
- generated-build.log / generated-run.log: eight native compiled outcomes
  execute, preserving crafted names/comments/expressions, typed creation/reuse,
  exact Unicode identity/events/helpers and real runtime bindings. Zero leaks.
  generated-browser-build.log compiles their pas2js counterpart.
- lcl-build.log / lcl-run.log: actual Win32 controls pass 42 managed, 35 event,
  50 binding and 71 Studio authoring checks, plus ordinary/compact inspectors,
  optional outputs, theme/factory/customization and 75 catalog projections.
  All 150240164 allocated blocks are freed. These actual programmatic controls
  do not establish hardware/IME/assistive input, another widgetset, browser pixels
  or complete native Studio parity.
- native-studio-build.log compiles the maintained current controller.
  browser-shared-build.log, browser-studio-build.log and browser-authoring-build.log
  compile shared/Studio/actual-authoring counterparts. Owned warnings are zero;
  seven installed RTL warnings remain visible. No suppression/dependency edit,
  compiler reinstall, listener replacement or controller deployment occurs.

Ordinary timing compares exact archived 6fc31c4 to this candidate using identical
checked FPC 3.2.0 flags without profiling/heap tracing. All compiler/control runs
finish before serial measurements. Original source byte sizes stay
25094/98822/400022; every row passes crafted-name/comment/expression/Unicode,
structural, paired history and retained-draft gates before reporting. Raw logs:
before-source.csv / after/source.csv and before-property.csv / after/property.csv.
These are complete portable commands, excluding paint, trusted input, HTTP and
compilation. Single samples near native timer resolution are not zero-cost proof.

| Controls | Apply before/after ms | Visual before/after ms | Structural before/after ms | Three history operations before/after ms |
| ---: | ---: | ---: | ---: | ---: |
| 128 | 31 / 31 | 63 / 62 | 93 / 94 | 31 / 31 |
| 512 | 172 / 156 | 313 / 281 | 390 / 391 | 171 / 172 |
| 2048 | 891 / 625 | 1360 / 1141 | 1719 / 1484 | 891 / 656 |

The largest Apply/history samples improve about 30%/26%; visual/structural
commands improve about 16%/14%. Smaller structural/history samples do not improve.
Fresh property admission measures 156→141 ms and complete shared shell composition
156→109 ms at 2048 controls, with selected values/help/source/draft guards passing.
This does not qualify faster painting or comfortable whole-editor responsiveness.
after/profile-2048.csv and after/profile-index-2048.csv retain explanatory profiles
before/after fresh identity indexing; parent timings overlap and are not summed.
Ordinary rows above, not those profiled rows, own performance observations.

Retained qualification corrections: focused-first-run.log records the fixture
assuming a lexer exception for a complete model uniqueness failure. It now checks
the public ENyxModel duplicate diagnostic, as well as ownership's ENyxSource,
before asserting exact pair/history preservation. Final runs have zero leaks;
the failed entry's Halt retained its diagnostic object. Private failed browser
orchestration logs retain incorrect filenames/unquoted compiler arguments; the
verified final consumers use existing entry points and literal argument arrays.
No product admission/ownership or workload gate is weakened by these corrections.

Browser execution/observing remains pending: automatic approval review rejected
the separate static fixture listener launch as "blocked by policy", with no
further reason. No equivalent launch is retried and no frontend/profile/enrollment/
accepted pair/configuration is replaced. The earlier updated-Studio listener
refusal and all protected services remain intact. Named semantic MCP reads query
the existing revision-6 production build, not runtime execution of this candidate.
Production/stage process identities are checked before push.

Codegen criterion 3 remains open; no-closure count advances 14→15 without reset.
Native authoring's 7 and delivery's 1 remain unchanged. End lookup/micro-index
optimization. Next is one integrated source-command scheduling/responsiveness
packet through the existing public scheduler and ordinary Nyx editor controls:
immutable candidate work/coalescing, stale-result refusal, visible source/pending
status and exact paired publication/history. Preserve fresh full admission,
original workloads, direct mutation visibility and authored frames; never mutate
accepted trees in background work. Reassess after that bounded packet. Browser
acceptance still requires a permitted host without an equivalent refused launch.
No grammar expansion, count reset, scope transfer, weakened parity/DONE gate or
full-product completion is earned. The original unbounded goal remains active.

The exact remote proof is private at
.local/codex-restart-check/source-references-remote-proof.json. Machine paths,
personal configuration, accounts and generated evidence remain outside the commit.


## Isolated source admission and editor Apply — 2026-10-05

Owning gate: original codegen criterion 3, following the counter-15 scheduling
reassessment. One integrated boundary now connects fresh isolated admission to
ordinary editor Apply/Restore, instead of another lookup-only improvement. It
does not close large-project responsiveness, visual/structural scheduling,
current browser execution or complete Studio/native parity.

Source readers own default recipe blueprints. Immutable reference-counted creator
environments carry full properties/help/support and event payload/context data;
native queries/publication use short read leases and thread-local scopes. Scopes
restore on nested failure, and later registration cannot mutate captured arrays.
The Pascal processor owns a fully admitted document/companion. Native completion
transfers this pair without repeating parsing/validation on the UI; the private
browser worker reply decodes/validates its owned design/frame. File/HTTP/MCP
imports continue complete source replay and cannot enter that trusted handoff.

The shared source controller runs one preparation and coalesces one latest
queued Apply. Native work uses INyxScheduler real workers; the browser has a
separately compiled Pascal worker with matched embedded RTL, protocol guards,
retirement and a bounded startup/work timeout. Fresh document/source/draft,
session/load identity and creator-generation checks precede one paired Undo
entry. The final creator guard excludes concurrent registration through the
swap. Invalid/stale/superseded completions retain current files/draft/history.
Native project contexts own their controllers; weak UI ports revoke before
teardown drains workers. Ordinary pending typing keeps its original base without
regeneration per keystroke. Bridge/recovery snapshots still need coalescing.

The actual Nyx-built code pane shows source status above its actions, so compact
hosts need not scroll to an offscreen global footer. Desktop/390 Win32 captures
paint English starter/review content. Dedicated shared qualification input
retains supplementary Unicode and CJK text; it is not initial demo content.
No branch/development label is added to default editor chrome.

Evidence under ignored build/source-scheduling/:

- focused-build.log / focused-run.log: 292 checked native source cases, zero
  leaks. New ownership/transport/context and publication cases also run in core.
- core-build.log / core-run.log: 30 core and 1769 composition/designer checks,
  zero unfreed blocks. The final run emits fresh compiler companions to core/.
- generated-build.log / generated-run.log: eight fresh generated native outcomes
  execute with zero leaks; generated-browser-build.log compiles their counterpart. Initial missing-unit
  attempts came from omitting the runner's output directory, not compiler/type
  rejection; the corrected emission/build/run passes before handoff.
- scheduling-lcl-build.log / scheduling-lcl-run.log: 39 actual Win32 source
  control/worker checks, zero leaks. The real Apply buttons qualify coalescing,
  exact Undo/Redo, retained code control, invalid draft diagnostics, cancellation,
  visible status inside desktop/390 viewports and retirement with work in flight.
  The actual scheduler worker retains older creator rules while publication is
  visible in the main environment. controls/ PNGs were inspected.
- studio-lcl-build.log compiles the current standalone native controller.
  server-build.log compiles the service in an isolated output without launching it.
  worker-build.log, studio-browser-build.log and focused-browser-build.log compile
  the Pascal worker, current browser Studio and shared source counterparts.
  worker output includes its RTL and program startup. Compilation is not worker,
  input, rendering or observing Studio execution.
- scheduled-timing.csv uses the original 128/512/2048 controls and source byte
  sizes 25094/98822/400022. All crafted names/Unicode notes/expressions, structural
  ownership, paired history and rejection gates pass. Scheduled Apply total is
  63/157/641 ms; UI submission is 0/0/47 ms at the existing clock resolution.
  The main loop is serviced 4/10/35 times. At 2048, visual/structural commands
  still take 1172/1531 ms. This is portable command work, not input/paint or
  compiler latency, and does not establish comfortable large-project editing.
- Owned warnings are zero; seven installed pas2js RTL warnings remain visible.
  PowerShell orchestration parses; maintained-worker-build.log exercises its
  compiler helper in maintained-browser/ and stages both complete programs.
  Browser Studio builds now bundle the compiled
  Pascal worker; native-studio -VerifySourceScheduling invokes the maintained
  actual controls without a listener.

Codegen criterion 3 remains open; no-closure count advances 15→16 once for this
boundary. Criteria 1/2 remain accepted; native authoring's 7 and delivery's 1 stay
unchanged. No task moves to DONE and no north-star credit is earned. Reassessment
follows detached visual/structural reconciliation plus coalesced bridge/recovery
capture through ordinary controls at the original sizes. Preserve fresh complete
admission, mutable-public-tree visibility, exact authored frames and paired
history. Browser Apply/worker/observing acceptance still requires a permitted
host without an equivalent refused listener launch.

Semantic MCP remains primary for design/build/review. Current named session and
bounded outline reads leave revision 6, home selection, one page/one reusable,
no pending draft, empty Undo and retained Redo intact. The local source editor
worker path requires the physical harness: general source admission/job-status
semantic operations remain with the existing NS-4_agent-workflows owner, rather
than an unrecorded screenshot-driven replacement.

Production/staging process identities were read and retained. No listener,
served front-end, controller, enrollment, profile or active accepted pair is
deployed/replaced here. The earlier qualification-listener rejection remains
"blocked by policy", with no further reason supplied. This packet does not
retry it. Exact remote proof for this source checkpoint is private at
.local/codex-restart-check/source-scheduling-remote-proof.json; generated logs,
machine paths/accounts/hosts and personal configuration stay outside the commit.
The original unbounded goal remains active. This goal turn is progress because
product integration and actual preservation evidence changed; it is not a
blocked goal or completed product.

## Coalesced draft capture and public hierarchy — 2026-10-05

The source-command continuation implements coalesced capture through the existing
project-owned editor exchange timer. Actual source input updates its session
immediately, then marks one pending window; later keystrokes do not continually
postpone it. One fresh operation-owned `ProjectSnapshot` encoding supplies both
sharing and optional browser recovery. No pair is cached across direct mutation.
Accepted commands retain immediate ordered publication and paired Undo/Redo.
In-flight queue heads remain immutable; unsent text blocks acknowledged-frame,
build and project-switch admission. A full older observation cannot adopt over
an empty queue with dirty local text. Capture/queue refusals survive late replies;
explicit remote acceptance remains the operator's choice. Paused sharing can
still persist locally. Pagehide/hidden/destruction recovery is best effort and
does not claim crash durability or native automatic paired-file save.

The original full native control fixture first failed at 2048 on the hierarchy's
73800-pixel column. Studio now consumes `INyxTree`, typed independent caption /
parent rows and exact component item references in one 280-pixel viewport. It
retains every active-view descendant, including 128-scalar Unicode IDs, without
retaining authored nodes or allocating a native editor button per row. Selection
uses public `OnSelectionChange` / `neUIQueue`; the legacy click/change callback
does not receive it. Renderer generation cancellation and explicit subscription
retirement protect borrowed receivers. Initialization is a no-op when already
selected. Compact Project/Design panels restore only mounted bindings. Review
caught that omission and corrected it; maintained browser continuations now wait
for the real queued selection task instead of changing its execution policy.

Evidence under ignored `build/draft-capture/`:

- Checked native shared protocol/ownership fixture passes **41**, with zero
  leaks. It qualifies burst capture, exact supplementary text/original draft
  base, unchanged full old observation, newer unsent input behind an in-flight
  commit, remote conflict, explicit acceptance, ordered accepted history, paused
  persistence, malformed direct mutation/capture refusal, exact tree identity and
  cancellation of queued shell/receiver events. Its deterministic private
  `TNyxAgentSession.Exchange` adapter does not qualify sockets/authentication or
  real elapsed clocks. Failed fixture setup logs are retained separately.
- Actual checked Win32 Studio memo, timers and paints pass the complete **128
  and 512** journeys with all components represented and exact final-item tree
  selection. Eighty `SelText` insertions post/repaint zero times in the input
  callbacks; one real timer commit shares the exact pending pair. Accepted
  source/history stay unchanged, the ordinary Nyx memo/focus/caret stay mounted,
  and pending capture/typed selection retire safely. Original paired source
  sizes remain **25094 / 98822 / 400022 UTF-8 bytes**. No size is narrowed.
- The full consumer **fails at 2048** before typing: the design canvas requests
  height **81963** and LCL refuses `TWinControl.SetBounds`, called by
  `TNyxLCLRenderer.Layout` during actual canvas Render. Checked failure teardown
  frees all **83424664** allocated blocks with **zero leaks**. Initial hierarchy,
  selection, compact-fixture and final canvas failure logs remain retained.
  No renderer height clamp or descendant truncation substitutes acceptance.
- Actual native source Apply/Restore regression passes **39**, zero leaks,
  after the compact binding correction. Current source controls and independent
  shell ownership remain exercised. Current stable-FPC core/designer regression
  passes **30 / 1769**, frees all **102232428** allocated blocks and reports
  **zero leaks**; this includes the maintained exact long-identity hierarchy
  consumer, now querying typed rows rather than removed button ordinals.
- Current browser Studio, portable fixture, core/designer and maintained DOM journey compile
  with zero owned warnings. Seven installed pas2js RTL warnings remain visible.
  The standalone fixture host includes its matched runtime and starts no
  listener. These are compiled consumers, **not current browser execution**.
  Neither DOM/timer/storage nor observing parity is accepted from compilation.
- Desktop/390 Win32 captures show English review captions and the retained
  source pane after the compact correction. The narrow pane currently has very
  little visible source height, so it is not full visual-quality acceptance.
  The original benchmark's private Unicode note remains qualification input;
  starter demos continue to use English. No Unicode support was removed.

Tracing times are not release latency: checked first input / 80 insert callbacks
/ timer plus in-process protocol are 47 / 1485 / 5859 ms at 128 and 156 / 5000 /
12984 ms at 512. The private GUI fixture executes server admission in-process;
production HTTP admission has its independent worker. Earlier ordinary samples
are retained but predate the compact correction and are not current acceptance
evidence. No comparison or comfortable typing claim follows these timings.
`-VerifyDraftCapture` stages both consumers, then keeps the full failed native
gate nonzero. `docs/building.md` documents its present refusal.

Original codegen criterion 3 stays open; the no-closure count advances **16→17**.
Criteria 1/2 remain accepted, native authoring's **7**, native renderer's **2**
and delivery's **1** stay unchanged. Discovery is mapped to the existing
NS-2_lcl-renderer scaling/layout owner as a prerequisite of the original source
outcome, not a scope transfer or new completion credit. Next fix logical scroll
extent / safe native widget geometry through the public designer viewport, with
all original controls reachable and retained input, selection, ownership and
both-target evidence. Then return to detached visual/structural reconciliation.
Stop another capture micro-optimization packet; the full-size failure determines
the next action. No counter reset, weakened parity/DONE gate or goal completion.

Named semantic MCP remains connected and primary. Read-only revalidation retains
revision **6**, home view/selection, one page/one reusable, no pending draft,
empty Undo and existing Redo. Production/staging identities remain unchanged;
no listener, front-end deployment, controller replacement, enrollment/profile
change or active user pair mutation occurred. Local source scheduling status /
admission semantic gaps retain their NS-4 workflow owner; physical callbacks
still require their explicit maintained harness. The earlier listener approval
rejection remains "blocked by policy" without more detail. No equivalent launch
or alternate route retries it. This goal turn advances product integration and
actual evidence while the original unbounded goal remains active and incomplete.

Source checkpoint **3b700e6** is pushed and verified against the exact remote
branch. The private proof at
`.local/codex-restart-check/draft-capture-remote-proof.json` records the current
clean handoff head separately from that product source checkpoint. Generated
logs, captures, personal configuration and machine paths remain ignored.
## Logical native viewport and original-size controls — 2026-10-05

One bounded renderer packet owns the recorded NS-2_lcl-renderer criteria 1/2
prerequisite consumed by original codegen criterion 3. Complete logical extents,
all descendants and exact input/source ownership remain required. Public
`Reveal`, `ViewViewport` and `ScrollView` separate explicit navigation from
scroll-neutral selection painting. Studio hierarchy navigation consumes those
contracts; native project presentation captures/restores their logical offsets.

Portable value geometry rejects invalid signed endpoints and unrepresentable
physical clips. Native bindings retain logical boxes and borrowed parent bindings
in their existing owned preorder array. Small partly visible faces keep their
complete native size/origin. Offscreen faces retain their controls/text with zero
physical allocation. Large containers project their exact visible intersection;
themed surfaces and outlines retain original face coordinates. Pointer producers
report logical positions. No model height, descendant count or source is clamped.

The first scrollbar-page attempt failed on a compact zero-area host; preserve
`build/logical-viewport/failed-page-run.log`. Overriding native scrolling alone
also failed: actual child-window coordinates moved to -32768 after resize while
LCL properties still reported zero. Installed LCL Win32 positioning directly
subtracts inherited scrollbar fields. The final adapter keeps those fields at
zero in logical mode and reuses separate standard LCL scrollbar controls with
full Integer ranges and bounded physical pages. Its ordinary mode retains native
automatic scrolling. Both axis ports/controls retire before their borrowed
renderer; the existing viewport observer captures the actual logical ports.
Explicit-height columns inspect overflowing content before native child geometry;
automatic columns avoid a redundant descendant measurement.

Actual maintained evidence:

- Stable FPC 3.2.0 checked portable geometry: **17**, zero leaks. Trunk native
  geometry also passes **17** through the maintained build command.
- Original unchanged **128/512/2048** Studio workload: **45** actual Win32 checks,
  zero leaks across **198,894,870** allocations. Source bytes remain exactly
  **25,094 / 98,822 / 400,022**. The final hierarchy entry reveals a physically
  allocated canvas control; the ordinary memo retains exact source/draft,
  timer/command/history guards, focus/caret and receiver retirement.
- Maintained `tools/build.ps1 -Target native-studio -VerifyLogicalViewport` exits
  successfully. **4,118** actual mixed-control checks, zero leaks across
  **15,800,512** allocations: every one of 2048 captions is physically reached,
  bottom/nested memo editing and real focus entry retain the same controls,
  desktop/390 geometry and logical pointer coordinates qualify, actual standard
  scrollbar/wheel changes reach typed viewport events. A giant native memo and
  a 40000-pixel stacked split refuse during candidate staging while retaining
  the accepted view. Split pane/grip origins require signed position limits,
  even when the containing window's unsigned size would be legal.
  The added split fixture initially used 90% with the default 85% maximum,
  so schema admission refused before the geometry gate. Its final typed maximum
  is explicitly 90%, with two owned panes. Retain the zero-leak failed setup
  logs `failed-split-fixture-admission.log` and `failed-split-fixture-bounds.log`;
  those runs do not establish the geometry refusal.
- Native viewport regression: **46**, zero leaks. Actual Source Apply/Restore
  regression: **39**, zero leaks, with its original five-second deadline. An
  earlier concurrently running physical source consumer exceeded that deadline
  and reported ten unfreed blocks on its Halt failure path; preserve
  `failed-source-concurrent-run.log`. Restrict redundant column measurement and
  run final physical regressions sequentially. Do not weaken that deadline or
  infer fair timing from concurrent consumers.
- Current portable geometry, mixed browser companion and browser Studio compile.
  Native owned warnings remain zero; the browser's seven installed RTL warnings
  remain visible, with zero owned warnings. Matched runtime is staged. Compilation
  establishes no current browser execution, input, appearance or parity.
  Final review removed protected-method access warnings in the new wheel fixture
  and existing viewport consumer through their actual shared control ancestor;
  no warning suppression or dependency edits were introduced. Earlier warning
  logs remain in `maintained-before-review.log` and
  `viewport-before-warning-review.log`.
- Maintained English desktop/390 review captures are in
  `build/logical-viewport/logical-viewport-bottom.png` and
  `logical-viewport-390.png`; the compact memo paints and remains reachable.
  Original-source Unicode inputs/captures remain technical qualification, not
  starter demos or presentation examples.

The earlier traced Studio samples in this packet were collected while other
qualification work ran: first edit / 80 insert callbacks / timer plus in-process
protocol were **47/1594/6234**, **156/5063/13000**, **719/21891/54453** ms.
These are checked/heap-traced host fixture durations, not release or network
latency comparisons. Full source/draft correctness does not accept comfortable
large-project input. The former non-traced prototype also passed all sizes but
preceded the corrected physical-scroll adapter; it is not final acceptance.

The concrete **81963-pixel native canvas failure is resolved**. Full original
renderer criteria 1/2 and codegen criterion 3 remain open: oversized individual
native inputs/custom faces and large split panes still need explicit logical
adapters; non-panel client offsets, widgetset/DPI metrics, logical resize event
semantics and complete target breadth still need their existing acceptance
qualification. Browser execution retains its previously recorded permitted-host
gate. Native captures do not establish complete Studio aesthetics/accessibility.

This packet accepts no complete criterion/task and earns no new completion
credit. Original codegen no-closure count advances **17→18** and native renderer
**2→3**; native authoring **7**, delivery **1** and accepted codegen criteria 1/2
remain unchanged. Reassessment now returns to the existing source criterion's
detached visual/structural reconciliation and comfortable ordinary editing.
Reuse immutable candidates, creator guards and revocable ports; accepted trees
must not be mutated on workers. Preserve the original sizes and exact
source/history/input guards. Do not start another scrollbar/projection inventory
as a substitute, reset counters, weaken parity or mark the full goal complete.

Named semantic MCP inspected bounded session, outline and English text at revision
**6**, with home selection/view, no pending draft, empty Undo and existing Redo.
Production/staging and the other protected service identities remain unchanged.
No live transaction, listener, front-end deployment, enrollment/profile change or
active user pair replacement occurred. Missing general source/status workflows
retain their NS-4 owner; physical input/rendering uses its explicit Pascal
harness. No equivalent refused listener or alternate route is retried. The
unbounded original goal remains active and incomplete.

Source checkpoint **9e1498c** is pushed and verified against the exact remote
`hello-nyx` branch with a clean worktree. The private proof at
`.local/codex-restart-check/logical-viewport-remote-proof.json` records the final
handoff head separately from this product source checkpoint. Logs, captures,
toolchain paths and personal configuration remain ignored. The next source
reconciliation action and all original acceptance gates above remain unchanged.

## Detached design/source commands — 2026-10-05

This bounded packet consumes the existing isolated source boundary through
ordinary inspector/title and common structural controls. Closed action/move
enums carry copied intent; exact IDs and schema field data remain explicit
metadata boundaries. Session/load identity is captured at input, before a fresh
complete paired baseline is taken at dispatch. Existing session authoring methods
replay on independent document/workspace/history owners under an immutable
creator snapshot. Workers borrow no accepted node, renderer or mutable recipe.
Native schema interfaces are retained by the work object; a UI-owned holder keeps
job records compatible with pas2js's interface restrictions.

One active command and at most 64 waiting commands preserve FIFO. Adjacent
waiting updates of the same property/title coalesce within the same load.
Repeated Apply supersedes only older Apply; structural ordering stays intact.
Fresh owner/load, accepted pair, original draft/base and creator guards admit one
paired Undo step. Invalid/stale/cancelled results retain accepted work. Queued
selection/view never follow later navigation, and completion retains independent
navigation. Reloading even identical files retires old intent, pending fields and
busy claims. Pending presentation retains native/browser field selection through
stable chrome identity. Save/export/build refuse an old-pair success while
current input is pending. Presentation exceptions cannot strand the queue after
dispatch/publication. Revocable ports and native drain protect retired receivers.

Qualification artifacts remain ignored under `build/design-source/`:

- `portable-run.log` passes **62** detached ticket/wire, exact publication,
  handwritten source, Unicode, original pending draft/base, structural commands,
  independent navigation, same-file reload, direct mutation, creator guards and
  one-shot ownership checks. Stable FPC heap tracing reports 12657896 allocations
  and frees, **zero unfreed blocks**.
- `consumer-run.log` executes the exact admitted exported companion against its
  full expected design, with 623 allocations/frees and zero leaks. This checks
  reconstruction through the compiler, not only source admission.
- `queue-run.log` passes **10** current real native scheduler cases: preparing/
  applied presentation exceptions, later FIFO work, exact history, save guard,
  matching-ID load retirement, visible pending fields and independent new intent.
  Heap tracing reports 2713883 allocations/frees and zero leaks.
- `maintained-build-run.log` passes **95** original-size real native inspector,
  title and palette checks and **39** Apply/Restore controls, with zero leaks.
  It preserves all descendants, exact source byte sizes, authored names/comments/
  expressions, pending fields, focus/caret, retained source widget, compact panel
  return, paired Undo/Redo, duplicate/delete order and detached cancellation.
  Timer ticks are measured while preparation remains busy. Corrected timing
  includes the first pump and completion, rather than reporting a false zero.
- `editor-english-run.log` passes **74** current full native Studio checks against
  the existing exact semantic export in `build/native-studio/source-english`:
  named reusable memo parts, independent recipes, page/component operations,
  typed supplementary input, ordered callbacks, TODO/source navigation, warned
  removal/history, retained drafts/caret, compact parking, optional targets and
  paired saved-file conflicts. Heap tracing reports 111934081 allocations/frees
  and zero leaks. This runs after the final queue/load/save safety refinements.

| Original controls | Exact source bytes | Three input submissions, ms | Completion, ms | Busy timer ticks |
| --- | --- | --- | --- | --- |
| 128 | 25094 | 1343 | 16594 | 2 |
| 512 | 98822 | 1093 | 25563 | 3 |
| 2048 | 400022 | 2438 | 98422 | 4 |

These checked/heap-traced concurrent-host costs include ordinary controls and
expensive fresh UI publication/projection. They are neither release benchmarks
nor comfortable editing acceptance. The 95-case run precedes the final safety
refinements; their current evidence is the queue 10/full-editor 74 and final
target builds. The updated maintained switch includes the queue fixture, but the
entire combined large suite was not rerun after that addition. Current native
application compilation succeeds with zero owned warnings.

Current browser Studio, separate Pascal worker and portable qualification
consumers compile with zero owned warnings; each retains seven warnings in the
installed Classes RTL. Matched runtime is staged, with no listener or deployment.
Compilation does not qualify browser execution, physical input, pixels, parity
or observing editor scheduling. The maintained `-VerifyDesignSource` option
orchestrates these Pascal consumers without changing application profiles.

Preserved failures and material limits:

- `failed-interface-record-build.log` records pas2js rejecting a COM interface
  field in a record. The explicit owned schema holder resolves that portability
  failure; native zero-leak and both-target compilation qualify its ownership.
- `failed-handwritten-fixture-run.log` records a test comment inserted at a
  boundary absent from plain generation. The fixture now assembles its real
  handwritten prefix through `TNyxStrings`; it does not weaken admission.
- The combined maintained run later fails the broader editor journey because its
  default older semantic export lacks a named reusable part. That diagnostic and
  zero-leak teardown remain in `maintained-build-run.log`/`editor-run.log`.
  A fresh separate-output compile against the actual English semantic export
  passes all 74 checks. This is a fixture correction, not an unnamed-part bypass
  or a successful claim for the earlier combined command.
- Moving deliberately authored Configure slots with a renamed local and
  preserved expression can fail declaration/construction/admission ordering.
  Two negative checks prove exact pair/history preservation. Ordinary generated
  move succeeds; this does not accept arbitrary authored structural movement.
- Canvas value proposals, state/binding/event routes and general semantic source,
  job-status/import/review lifecycle remain with their existing owners. No
  screenshot-driven editor automation substitutes missing MCP operations.

Original codegen criterion 3 stays open; its no-closure count advances **18→19**.
Accepted criteria 1/2, native renderer **3**, native authoring **7** and delivery
**1** remain unchanged. No complete task/criterion or completion credit is earned.
Reassessment now profiles actual UI publication/projection and consumes guarded
retained projection plus supported authored structural ordering through this
reliable worker boundary. Preserve original-size descendants, complete fresh
admission, crafted source, drafts, input and exact paired history. Do not replace
this with another lookup variant, scope/count reset, accepted-tree worker access,
weakened parity or full-goal completion.

Connected semantic MCP inspected the bounded session at revision **6**, home
selection/view, one page/component, no pending draft, empty Undo and existing
Redo; activity sequence reached **173** through read-only context. All eight
protected production/staging/qualification process identities remain unchanged.
No live mutation, listener, front-end deployment, active pair replacement,
enrollment/profile change or equivalent refused launch occurred. Browser execution
retains the previously recorded automatic approval rejection of qualification
listeners, whose stated reason was only “blocked by policy”. English starter and
review copy remains separate from dedicated technical Unicode inputs. The
original unbounded goal remains active and incomplete.

Source checkpoint **86e61f1** is pushed and verified against the exact remote
`hello-nyx` head with a clean worktree. The private proof at
`.local/codex-restart-check/design-source-remote-proof.json` records the final
handoff head separately from this product checkpoint. Logs, captures and local
toolchain/configuration stay ignored. This remote checkpoint changes no live
service, active user pair or acceptance gate; the next UI-stage/source-order
action remains above.

## Guarded retained projection and authored ownership ordering — 2026-10-05

This integrated packet follows original codegen criterion 3. Opt-in native
`NYX_STUDIO_PROFILE` isolates UI composition, mounting, layout, retirement and
publication without authored values or production instrumentation. In the
unchanged 128-control checked/heap-traced probe, repeated shell mounting takes
about 4.7 seconds; guarded reuse takes roughly 0.4 seconds. The probe diagnoses
one original size and never substitutes the full three-size qualification.
Native staging now balances LCL's host sizing lock through success/factory
failure; retirement disconnects resize before destroying bindings. Installed
LCL behavior was inspected read-only; no dependency source was changed.

`nyx.projection.refresh` owns no document, node or target handle. Both renderers
validate fresh public data/context, independently realize/platform-apply the
requested view and compare ordered identities, metadata, creator generation and
effective theme. Reuse permits only the closed scalar presentation attributes;
structural/style/context changes, custom factories and scalar live-binding
coordinators request the full staged mount. An independent last-authored
projection applies only authored deltas, retaining runtime-edited values for
unchanged defaults. Renderer-owned rollback properties and existing Sync retain
event scopes/control identities. These bounded checks do not qualify every
component, injected Sync failure or assistive-technology behavior. Studio
consumes the public contract; its always-present public status label changes
visibility/text instead of inserting a child at preparation.

Source reconciliation indexes exact moved ownership semicolons, then places
displaced handwritten metadata immediately after that admission call, before
later statements. Surrounding comments, crafted names, unchanged expressions
and supported repeated extension values retain order. Complete reconstruction
still validates constructors, ownership, types and exact meaning; the lexical
placement index is not source admission or a shared mutable cache. Tests cover
both move directions, positive exact paired Undo/Redo and compiler execution.
The supported one-Configure-block grammar was not weakened.

Current ignored qualification artifacts:

- `build/ui-projection/maintained-run.log` runs the maintained native command
  with `-VerifyDesignSource -VerifySourceScheduling -VerifyNativeStudio` and the
  existing exact English semantic export. It passes detached **75**, real
  scheduler queue/load/presentation **10**, actual retained controls **20**, exact
  compiled companion, original-size controls **101**, Apply/Restore **39** and
  full English standalone editor **74**.
  These consumers report zero unfreed blocks; the large control run reports
  **210974972** allocations/frees. It retains every descendant, exact source byte
  sizes, handwritten content, pending input/caret, independent source control,
  actual inspector/canvas identity, complete logical extents and paired history.
  The full editor consumer reports **129361097** allocations/frees and zero
  leaks, including page/reusable operations, source/drafts/caret, optional outputs,
  ordered callbacks, warned removal/history and paired saved-file conflicts.
- `build/ui-projection/portable-regressions/` passes core **30**, composition/
  designer **1769** and structural source **84**, with zero leaks. These are
  current portable regressions, not additional UI/parity acceptance.
- `final-source/` and `final-lcl-source/` under `build/ui-projection/` rerun
  detached **75** on stable FPC and the native Studio compiler after explicit
  `TNyxText` casts remove three fixture conversion warnings. Both have zero
  warnings/leaks. Their exported pair equals the maintained compiled pair;
  stable FPC reports **14479111** allocations/frees and the Studio compiler
  **12543178**. The exact maintained companion execution reports **779**.
- `build/design-source/maintained/browser/` compiles current browser Studio,
  the separate Pascal worker and shared/actual-control consumers, staging the
  matched RTL. `build/ui-projection/final-browser-source/` rebuilds the final
  typed source fixture. Each has zero owned warnings; each browser compilation
  retains seven installed Classes RTL warnings. No browser execution, new
  listener, deployment, physical-device or full-parity claim follows compilation.
- `build/design-source/maintained/controls/` contains inspected desktop/390
  native captures with English review text. Dedicated supplementary/CJK inputs
  remain in technical source/codec qualification, separate from visible demos.

| Original controls | Exact source bytes | Three input submissions, ms | Completion, ms | Busy timer ticks |
| --- | --- | --- | --- | --- |
| 128 | 25094 | 1141 | 3969 | 2 |
| 512 | 98822 | 1985 | 10921 | 2 |
| 2048 | 400022 | 2203 | 56234 | 3 |

These are the same original checked/heap-traced concurrent-host workloads.
Earlier completion was 16594/25563/98422 ms. Current reduction does not establish
comfortable release typing/painting, network latency or another widgetset.
No smaller fixture, truncated hierarchy, dimension clamp or weakened admission
was substituted. Next qualify the unchanged workloads under ordinary native
application build settings, separate from current checked ownership evidence,
before choosing another optimization. Budget one actual-control/reconstruction
packet and stop on stale meaning, dropped/reordered input, weakened admission,
accepted-tree worker access or unsafe lifetime.

Preserved failures under `build/ui-projection/`: `profile-before/failed-argument-run.log`
records the original single-argument fixture gate, now expanded only for optional
diagnostic selection; all-size qualification remains default. The earlier
`source-order/failed-repeated-configure-run.log` used an unsupported repeated
Configure fixture; correction uses supported repeated Extensions, retaining
later final values instead of widening grammar. `controls/failed-withvalue-build.log`
records a nonexistent convenience method corrected to the existing fluent
Configure.Value contract. `failed-initial-status-shape-run.log` catches genuine
inspector replacement when status inserted a new child, with zero leaks. The
stable public status control resolves that actual identity failure; identity
assertions were retained. The initial `retained/` probe predates final authored
baseline/status refinements and is not the final acceptance run.

Criteria 1/2 stay accepted; original criterion 3 stays open at no-closure count
**19→20**. Renderer **3**, authoring **7** and delivery **1** remain unchanged.
Canvas/state/binding/event routes, broader handwritten synchronization, ordinary
browser/observing editor outcomes and full-product depth retain their existing
owners. No task/DONE move, counter/scope reset or blanket performance/parity
acceptance is earned. The unbounded original goal remains active and incomplete.

Connected semantic MCP read the bounded current session at revision **6**, home
selection/view, one page/component, no pending draft, empty Undo and existing
Redo, activity sequence **181**. Bounded node reads confirm English starter
heading/description, badge and code-block caption at that same revision. All
eight protected process identities match the prior checkpoint. No active
pair/history mutation, application profile,
enrollment, server replacement or equivalent refused launch occurred. Browser
execution retains the earlier automatic approval rejection of qualification
listeners; its stated reason was only “blocked by policy”.

Product checkpoint **ae27c9e** is pushed and verified against the exact remote
`hello-nyx` head, with a clean worktree after all final regressions. The private
proof at `.local/codex-restart-check/retained-projection-remote-proof.json` records
the final handoff head separately, exact counts/timings and protected identities.
No qualification fixture remains running. Logs, captures and local configuration
remain ignored. This checkpoint changes no live deployment or acceptance gate;
the next ordinary-build original-workload qualification remains above.

## Ordinary native builds and stable retained hosts — 2026-10-05

The previous goal turn was progress: ae27c9e/3042635 changed product and exact
evidence, leaving the authoritative branch clean/pushed. This packet follows
the original criterion-3 responsiveness outcome with the unchanged original
128/512/2048 controls and 25094/98822/400022 source bytes.

`-NativeStudioConfiguration release` adds an ordinary optimized Studio build.
It retains `-Sa -Cr -Co -Ci`, uses `-O2 -Xs` and omits `-gl -gh`; separate
native binaries/units and qualification artifacts preserve the checked build.
The existing default remains checked. Application compilers/services remain
optional at launch; this choice compiles Studio, not an exported user's app.
The maintained source, actual controls and exact compiled companion run through
the same Pascal consumers in either configuration. No compiler reinstall,
dependency edit, application profile or listener is involved.

The initial complete release packet passes detached **75**, queue **10**, actual
projection **20**, exact compiled companion, original-size controls **101**,
Apply/Restore **39** and full English native editor **74**. Its timings are
140/141/203 ms for the three input submissions and 1188/3844/32813 ms for
completion. This confirms tracing explains only part of the previous costs.
The existing UI-stage profiler at the unchanged 2048 size then finds repeated
parking/reparenting of independently owned canvas/source views: roughly
4750 ms to park and another 4800 ms to return, even while the shell reuses its
existing borrowed hosts. Current snapshot/capture/publication are small beside
these actual native layout costs. The diagnostic passes **27** original-largest
checks; its 33250-ms completion agrees with the ordinary 32813-ms result.

Native Paint now tries guarded shell refresh while its canvas/source remain
mounted. A compatible shell retains those same hosts; only full replacement
parks the independent views before old hosts retire. Existing exact-host
recovery handles replacement failure. No validation, source publication,
ownership guard, original descendant or history step is omitted. The unchanged
public `MoveHost` already returns immediately for the current host. The
post-change largest diagnostic passes **27**, completes in **4672 ms** and
services **91** busy UI timer ticks versus **2** before. Supported canvas Sync
in these retained frames takes roughly 485 ms, with no parking stage.

Ignored evidence lives under `build/native-release-qualification/`:

- `before-retained-hosts-run.log` is the complete initial release packet.
  `profile-before/` and `profile-after/` preserve exact 2048-control stage logs
  and independently compiled binaries/units. Selected-size diagnostics never
  substitute full-size or both-target acceptance.
- `maintained-run.log` requalifies the final ordinary build: detached **75**,
  queue **10**, retained projection **20**, exact compiled companion, original-size
  controls **101**, Apply/Restore **39** and full English editor **74** pass.
  Its exact exported source/design equals the checked companion. Release
  omits heap instrumentation; zero-leak claims belong to the checked run.
- Release native desktop/390 captures under `build/design-source/release/controls/`
  were inspected and paint English review content. Broader Unicode stays in
  dedicated input/source/codec qualification. The current source editor's
  hidden technical fixture is not presented as starter-demo copy.
- `checked-run.log` separately passes detached **75**, queue **10**, actual
  projection **20**, exact compiled companion, original-size controls **101**
  and Apply/Restore **39**, plus the full native English editor **74**, each
  with zero unfreed blocks. The original-size consumer allocates and frees
  **200130989** blocks / **7558818320** bytes.
  The full native editor allocates and frees **129260614** blocks /
  **3593258481** bytes. All seven traced consumers terminate successfully.
- Final checked/release companion source and `expected.nyx` match by SHA-256.
  Both builds retain all seven distinct installed pas2js Classes RTL warnings
  across four browser compilations (28 lines per log); no owned-source warning
  appears. Dependency source and warning settings remain unchanged.

| Original controls | Exact source bytes | Final ordinary input, ms | Final ordinary completion, ms | Busy UI timer ticks |
| --- | --- | --- | --- | --- |
| 128 | 25094 | 140 | 531 | 2 |
| 512 | 98822 | 156 | 1359 | 17 |
| 2048 | 400022 | 203 | 4718 | 100 |

| Original controls | Exact source bytes | Final checked input, ms | Final checked completion, ms | Busy UI timer ticks |
| --- | --- | --- | --- | --- |
| 128 | 25094 | 1000 | 2844 | 4 |
| 512 | 98822 | 1515 | 7765 | 72 |
| 2048 | 400022 | 2172 | 33079 | 432 |

These actual native concurrent-host fixture timings establish bounded progress,
not release readiness, physical-device/assistive-technology quality or all
comfortable authoring. Visible large-view editing, full shell replacements,
exact drafts/caret and paired history remain required acceptance behavior.
Browser consumers/worker compile; their current execution still needs the
previously recorded permitted-host gate. Nothing was deployed to the live phone
instance, and no equivalent refused qualification listener was attempted.

Bounded semantic MCP reinspection keeps the active user pair at revision **6**,
Untitled project/home, one page/component, no pending draft, no Undo and available
Redo, activity sequence **183**. All eight protected process identities match
the preceding checkpoint. No active pair/history, application profile,
enrollment or live deployment changed. The earlier automatic approval review
rejected qualification listeners with only “blocked by policy” as its reason;
this packet attempts no equivalent launch and supplies compile-only browser
evidence, not execution/parity credit.

Original codegen criterion 3 remains open; this turn advances its no-closure
count **20→21**. Accepted criteria 1/2, renderer **3**, native authoring **7** and
delivery **1** remain unchanged. No full task/criterion, scope reset or parity
credit follows the native timing improvement. The original unbounded goal stays
active and incomplete.

Next integrate ordinary canvas value proposals with the existing isolated
design-command queue. Current native CanvasEvent still calls SetCanvasValue
synchronously. Capture immutable owner/view/runtime identity and proposed value,
preserving two-way state/default and named reusable-part semantics; never send
live realized nodes to workers. Qualify actual input, invalid/read-only bindings,
same-ID load retirement, drafts/source preservation and exact paired Undo/Redo.
State/binding/event breadth, general semantic source/import/review operations,
ordinary browser/observing editor outcomes and full-product depth retain their
original owners. Another timing/lookup variant does not substitute that functional
integration or permit weaker admission/DONE gates.

Product checkpoint **ddc67b3** is pushed and verified against the exact remote
`hello-nyx` head, with a clean worktree after both maintained qualification
configurations. No qualification fixture remains running and all eight protected
processes still match. The private proof at
`.local/codex-restart-check/native-stable-host-remote-proof.json` records the final
handoff head separately, both timing sets, exact companion equality and scoped
ownership/compile evidence. Logs, binaries, captures and local configuration stay
ignored. This checkpoint changes no deployment, active user pair or acceptance
gate; the next functional canvas-input integration remains above.

## Isolated canvas input and exact field reconciliation — 2026-10-05

This is bounded functional integration under original codegen criterion 3,
following product ddc67b3 and handoff 79e3906. Ordinary canvas input now uses the
same isolated design-command queue as inspector/structural authoring. Capture
owns proposed wire text, concrete platform, exact view/runtime field/editable
owner and mounted session/load identity. Retained intent and old mounted controls
refuse after identical-ID reload before they can coalesce away current work.
The private processor ticket is version 2 and validates its closed platform enum;
portable design/project persistence is unchanged.

Fresh worker-owned realization reapplies platform overrides and document defaults
before invoking the existing typed canvas command. Two-way fields update typed
document defaults without changing explicit fallback text. Unbound named reusable
parts receive instance-only overrides, including nested component paths; recipe
definitions and sibling instances stay independent. Wrong-type/range, one-way and
read-only fields refuse without partial pair/history publication. No queued job
or worker borrows a realized node, widget, accepted tree or mutable recipe.

An actual native regression exposed invalid Integer input retaining its physical
text even though admission rejected it and preserved the default. Completion
previously exposed canvas restoration only through successful publication.
A separate transient completed-canvas effect now restores the exact field for
accepted/rejected/stale/failed results without treating failure as publication.
Both adapter controllers copy restore identities within their current load;
pending proposals overlay accepted values so an older completion cannot erase
newer waiting input. Native deferred paint re-resolves the current input after
possible full replacement, preserving focus/caret without dereferencing a retired
control. Adjacent waiting input coalesces only for the same field/platform.

The public renderer refresh contract accepts typed exact-field restore groups.
The whole group validates before any authored delta or reset. A fresh candidate
supplies Value or its absence; other controls' runtime drafts remain untouched.
All existing structure/context/theme/creator/custom-factory/binding guards retain
their normal full staged-render fallback. Neither this restoration nor pending
presentation bypasses fresh admission or changes document defaults/history.

The shared English technical fixture covers unbound/nested reusable parts on one
page and typed bound controls on a separate page. It exercises inherited identity,
platform policy, pending drafts, wire enum rejection and exact paired history.
Dedicated supplementary Unicode input remains a technical qualification rather
than non-English starter copy. The private physical-control fixture is necessary
for behavior the document API cannot establish; the full editor regression still
consumes the existing MCP-authored English source export. No screenshot-driven
editor composition or live design replacement was used.

Current evidence root: `build/canvas-input-qualification/`. Maintained commands:

```powershell
./tools/build.ps1 -Target native-studio -NativeStudioConfiguration release -VerifyDesignSource -VerifySourceScheduling -VerifyNativeStudio -DesignerSourceDirectory build/native-studio/source-english
./tools/build.ps1 -Target native-studio -NativeStudioConfiguration checked -VerifyDesignSource -VerifySourceScheduling -VerifyNativeStudio -DesignerSourceDirectory build/native-studio/source-english
```

| Consumer | Current evidence |
| --- | --- |
| Portable FPC 3.2.0 shared/wire | `portable-current/run.log`: 140, zero unfreed blocks |
| Focused checked FPC 3.3.1 actual canvas | `native-focused/canvas-restore-run.log`: 47, zero unfreed blocks |
| Focused actual retained projection | `native-focused/projection-run.log`: 27, zero unfreed blocks |
| Maintained optimized native matrix | `maintained-release.log`: 140 shared, queue 10, projection 27, canvas 47, two exact compiled companions, original-size controls 101, Apply/Restore 39, English full editor 74 |
| Maintained checked native matrix | `maintained-checked.log`: the same nine consumers pass, all nine with zero unfreed blocks |
| Browser Studio/shared/renderer/module worker | Maintained staging under `build/design-source/release/browser/` and `build/design-source/maintained/browser/`; compiles, changed runtime remains pending |
| Actual English desktop/390 native paint | `native-focused/captures/canvas-queue-desktop.png` and `canvas-queue-390.png`, both inspected |

The FPC 3.2.0 run frees all **28417040** allocations; focused native canvas frees
all **43576708**, projection **198665**, with zero leaks. These are separate
executions, not additive product coverage. The retained projection test preserves
its original 20 cases and adds seven native cases (six shared browser cases).
The shared test preserves all previous 75 cases and adds 65 canvas cases.
The native canvas journey qualifies rapid active/waiting/coalesced input, actual
caret/control identity, invalid/range/type restoration, typed two-way defaults,
one-way/read-only refusal, same-ID retired mounts/intents, source drafts, exact
paired Undo/Redo and detached worker retirement.

Both maintained configurations reconstruct both exact admitted companions.
Original source/design hashes remain equal to the preceding checkpoint; the new
canvas companion hashes agree between checked and optimized builds. Owned warnings
are zero. Seven distinct existing Classes RTL warnings remain in the installed
pas2js toolchain. Native notes/hints are retained, including intentional keepalive
variables.
The checked original-size consumer frees all **200129951** allocations /
**7499719637** requested bytes, and the full native editor frees all **130452302** /
**3635432183**, with zero leaks. Release deliberately omits heap tracing.

| Native configuration | Original controls | Source bytes | Three-input dispatch ms | Completion ms | Busy UI ticks |
| --- | ---: | ---: | ---: | ---: | ---: |
| Release | 128 | 25094 | 125 | 656 | 3 |
| Release | 512 | 98822 | 156 | 1469 | 11 |
| Release | 2048 | 400022 | 203 | 4656 | 53 |
| Checked | 128 | 25094 | 1125 | 3391 | 3 |
| Checked | 512 | 98822 | 2000 | 8109 | 56 |
| Checked | 2048 | 400022 | 1843 | 31812 | 432 |

These are the unchanged inspector/title/structural workloads, not large-project
canvas-input measurements. The new canvas fixture qualifies a bounded two-page
input journey. Timings do not establish comfortable editing, new canvas scaling,
browser behavior, hardware/IME/assistive-technology or another widgetset.

Failures retained in this packet: the original numeric restoration defect is in
`native-focused/canvas-before-numeric-diagnostic.log` and
`canvas-diagnostic-run.log`; assertions remain strict after the product fix.
Early fixture proposals incorrectly used the public text-only Configure.Value
overload for numeric wire text. They now enter through the explicit adapter
boundary with the typed Value attribute; public typing was not relaxed. An early
image-unit name and a two-argument fixture invocation refused before valid input
qualification, then were corrected. Their tool outputs are not claimed as passes.
The generic `-Target studio` build tried to relink the running production server;
Windows refused its locked executable (error 5, `browser-build.log`). Subsequent
browser work compiled directly into staging; maintained native builds stage their
own browser/module worker without relinking a server. The early direct worker
browser-target invocation was corrected to module mode. No compiler was
reinstalled, dependency edited or warnings suppressed.

Bounded MCP reinspection preserves revision **6**, Untitled project/home, one
page/component, no pending draft, no Undo and available Redo, activity **187**.
All eight protected process identities match the preceding private proof; the
current identity snapshot remains private under `.local/codex-restart-check/`.
No active user pair/history, enrollment, output profile or live deployment changed.
Browser execution still needs the permitted host: automatic approval review
previously rejected qualification listeners with only “blocked by policy”. This
packet launches no equivalent listener and claims compile-only browser evidence.

Original codegen criterion 3 remains open; its no-closure count advances **21→22**
once for this functional integration. Accepted criteria 1/2, renderer **3**, native
authoring **7** and delivery **1** remain unchanged. The original unbounded goal
is active and incomplete. Next follow broader source/state/binding/event and
semantic integration with the existing owners, while ordinary both-target
editor/observing outcomes retain their host and acceptance gates. Another
timing/lookup variant, scope reset or weaker admission/DONE gate cannot substitute
the full user outcome.

Product checkpoint **4b753e9** is pushed and verified against the exact remote
`hello-nyx` head, with a clean worktree after both complete maintained native
matrices. All nine checked consumers report zero leaks, both companion pairs
match between configurations, all eight protected processes match and no fixture
remains running. The private proof at
`.local/codex-restart-check/canvas-input-remote-proof.json` records the final
handoff head separately, hashes, timings, compiler/runtime scope and current
semantic session. Logs, binaries, captures and private configuration remain
ignored. This checkpoint changes no live deployment or active user pair; the
broader integration and original full-goal gates remain above.

## Semantic scalar state and binding commands — 2026-10-05

This bounded batch follows NS-4_agent-workflows criterion 5 and consumes original
NS-1_codegen criterion 3. `NyxStateBindingPatch` owns 1..32 typed copied commands;
its independent ordinary session admits ordered create/set/rename/remove/bind/
clear/inherit work, then the active session publishes its final pair once.
Typed reference overloads derive binding families without raw behavior strings.
Failed groups preserve accepted/draft/base bytes, source helpers, navigation and
history. Rename migrates authored references across pages/definitions; existing
explicit reusable override IDs preserve deliberate clearing versus inheritance.
Commands still use ordinary per-operation generation on their temporary session;
this packet makes no batching-throughput or comfortable-editing claim.

`nyx_state` exposes bounded default rows (80-scalar text previews), exact text
windows (4096 scalars), local/effective binding context and one grouped apply.
Permission, exact revision, private authority, bounded receipts and pending-draft
refusal use the existing shared agent boundary. Context wrappers retain explicit
project/review ownership and do not follow observing-user navigation. Activity
records completion/refusal through the ordinary operator observation. The current
source tools/list catalog contains eighteen tools; the protected LAN/current
native desktop inventory remains fifteen. No listener, service configuration,
Codex enrollment or active user pair was replaced.

Meaningful qualification exposed two product defects. Local `FindBinding` reports
usable bindings only, so discovery initially lost cleared descriptors; authored
local discovery now reads exact owned descriptors while effective discovery keeps
usable bindings. Actual whole-catalog construction then exposed a pre-existing
staged callback schema adding duplicate `not` keys. Composition now preserves
its original callback-review prohibitions and both-context refusal with one
combined constraint. The offline catalog consumer qualifies that exact boundary.
Early fixture errors (wrong override enum, disabled-agent preservation snapshot,
qualified reusable runtime IDs and native numeric commit timing) were corrected
to use the public contracts; they do not represent successful product behavior.

Maintained evidence is ignored under `build/state-binding-semantic/` and
`build/state-bindings/`:

- `tools/build.ps1 -Target state-bindings` / `maintained-build.log` passes **55**
  semantic checks on checked FPC 3.2.0, **14** actual offline tools/list schema
  checks and **15** actual Win32 controls from the unchanged admitted companion.
  All three traced processes report zero unfreed blocks.
- `native-3.3/build.log` / `run.log` passes the same **55** shared checks on checked
  FPC 3.3.1, with zero leaks. Tests include all four families, supplementary/NUL
  values, bounded pagination, wrong primitive/domain/reference refusal, failed
  later operations, actor-bound exact retries, pending draft/disabled/read-only,
  rename propagation, clear/inherit and one exact paired Undo/Redo.
- `regression/nyx_agent_tests-run.log` / `nyx_agent_callback_tests-run.log` passes
  **39 / 45** existing checks with zero leaks. These counts retain their original
  scope and are not newly delivered features.
- Native compiled controls prove actual memo/checkbox/numeric input, inherited
  reusable projections, invalid-number restoration, parent-disabled write refusal
  and independent application stores. Authored design/defaults remain byte-exact.
  This is actual control behavior, not store-only evidence or full Studio painting.
- `server/build.log` builds the current server to a separate staged directory.
  `NyxStudioMCPTools` executes pure discovery without any service constructor,
  credential refresh or listener. This proves catalog construction, not current
  authenticated new-tool availability.
- The maintained target compiles shared semantic and unchanged compiled browser
  control consumers, copies matched RTL and stages English hosts. Browser
  execution, physical input and observing updated Studio remain unqualified here.
  Owned warnings are zero; seven distinct installed Classes RTL warnings remain.
  Dependencies and warning policy were not altered.

The exact semantic export under `build/state-bindings/export` has SHA-256:
source `D8E40EB424F4A5BF54315EFFAACD0DFABB5EC5A2D8F650E1ECF5F3CE159563B5`,
design `375665ED5E4655C69A603420A444211394DB4006297A95C3AEACFCCC3C18C57E`,
paired project `3E6C90F0C3C1CA489CA52680830A030339CF6FF8054435F3E5C329F4853BEEFB`.
Native execution of that exact source reconstructs the design. English demo
controls and dedicated supplementary/NUL qualification defaults remain separate.

This goal turn is progress toward the full outcome: a confirmed missing semantic
operation is usable in source and qualified natively, with the actual staged
catalog repaired. No full criterion closes. Workflow criterion 5's no-closure
sequence advances **2→3**, codegen criterion 3 **22→23** once, renderer **3**, native
authoring **7** and delivery **1** unchanged. Reassessment stops schema/fixture
expansion and changes the next deliverable to ordinary state/binding inspectors
consuming isolated typed admission. Rename needs an explicit commit contract or
stable identity before queued replay; consecutive keystrokes cannot safely use
stale names. Broader event/source routing, structured collection/reusable/general
source/import semantics, comfortable large-project editing and ordinary both-
target editor outcomes retain their original owners and acceptance gates.

Automatic approval review previously rejected qualification-listener launch
("blocked by policy", no further reason). This packet does not repeat or replace
that action. Updated authenticated tools, observing Studio and browser execution
still need that permitted host; successful compilation/offline metadata is not a
substitute. Final protected-process/session and remote-checkpoint proof follows.

Final preservation check: all eight protected process IDs match their recorded
executable paths and exact creation timestamps; zero state/schema/control fixture
processes remain. Native named `nyx_session` still authenticates revision 6,
Untitled project / home, one page/reusable, no pending draft, Undo unavailable,
Redo available, activity sequence 189. Only read-only session inspection occurred
on that primary project in this packet. The Windows proof is private under
`.local/codex-restart-check/state-binding-processes.json`; date comparison uses
the deserialized DateTime directly so its subsecond identity is retained.

Remote checkpoint: product commit `79e8b59` contains this packet and is pushed to
`origin/hello-nyx`; exact remote equality is verified. The maintained state target
passes 55 / 14 / 15, native 3.3 passes 55, existing agent/callback regression 39/45,
all with zero leaks. Browser compilation is staged only. Final connected primary
revision/history and all eight protected process identities remain unchanged.
No state/schema/control fixtures remain. This handoff updates the work record;
its exact clean/remote proof is stored privately after its own push. Goal remains
active/incomplete, with the next acceptance deliverable and counts above.

Current continuation batch (original codegen criterion 3; authoring/workflow
consumers): route ordinary scalar default creation/edit/removal and bindings
through isolated design admission, with typed wire intent and pending-field
presentation. State names become retained drafts applied by an explicit Rename
control; pending rename locks its exact row to avoid stale-name replay. Preserve
partial/newer values, source/draft/history, independent navigation and load
retirement. Qualify the private wire, failed/stale replay, real native Project /
Bindings controls, English desktop/390 presentation and both compiler consumers.
The previous goal turn was progress: 79e8b59/a823a9e are exact clean/pushed state
semantic/discovery/control evidence. Counts remain 23 / workflow 3 / renderer 3 /
native authoring 7 / delivery 1 until assessed. Stop on dropped/retargeted typing,
partial publication or weaker admission; broader event/collection/source routes
and browser/observing host gates retain their original owners.

## Queued scalar Project/Bindings authoring — 2026-10-05

The continuation deliverable now routes ordinary default creation/edit/removal,
explicit rename, binding choice/flow/clear/inherit through the existing independent
design processor. Six closed intents carry copied scalar notation/binding values;
private tickets advance to version 3 with strict enum/descriptor admission. The
project persistence format remains version 1. Both adapters consume the same
Nyx-built controls and capture function; neither worker retains an accepted node.
Fresh pair/draft/load/creator guards and one paired Undo publication remain owned
by the existing session. No throughput or comfortable-editing acceptance follows.

Names are retained project-owned drafts until Rename. A queued rename guards its
exact row before paint; successful publication retires only its matching draft,
and rejection keeps correction text. Removed/load-retired identities cannot
inherit old drafts. Pending creation guards duplicate submission and clears only
the exact successful form name. Pending defaults retain notation as well as
family across earlier publication; pending bindings retain owner/target/flow.
Native/browser focus restoration qualifies the exact state identity rather than
reusing a positional row ID. The shell also carries its actual mounted-load guard.
Ordinary visual edits preserve an independent handwritten draft and original base;
grouped semantic mutations retain their stricter pending-draft refusal.

Actual qualification found an owned product defect: `DefaultNyxStudioViewState`
initialized individual fields but left pending-record scalar flags undefined on
native FPC. The older authoring consumer could therefore receive a disabled Add
form. Whole-record default initialization fixes that boundary; its complete
consumer now passes. A fixture initially compared a Double to an extended literal,
and another assumed semantic pending-draft refusal applied to ordinary visual
editing. Both assertions were corrected to the public contracts, not weakened to
claim successful behavior. Failed trials remain described here; their earlier
composite command did stop rather than continuing after failure.

Ignored evidence is under `build/state-inspector-queue/`:

- `shared/build.log` / `run.log`: **56** checked FPC 3.2.0 typed ticket/admission
  checks. `shared-3.3/` passes the same **56** on checked FPC 3.3.1. Cases include
  all four families, exact supplementary/NUL values, signed bounds/precision,
  wrong notation/reference/domain/enum refusal, rename migration, inheritance,
  paired Undo and stale owner/draft/load refusal.
- `native/shared-run.log`: existing **140** detached design/source checks pass
  after the private wire change. `queue/run.log`: existing **10** real scheduler
  presentation/retirement checks pass. These are regressions, not new features.
- `final-lcl/build.log` / `run.log`: **82** actual standalone Win32 state/binding
  checks pass, including rapid input/coalescing, failed renames, exact paired
  history, numeric rejection/focus, escaping across later queued values,
  captured binding selection, reusable clear/inherit, independent source drafts,
  same-ID replacement and detached retirement.
- `legacy/run.log`: **73** actual shared native authoring checks pass after the
  complete-record initializer fix. `canvas/run.log`: existing **47** actual
  queued canvas checks pass with the current consumers. All seven distinct
  successful behavior fixture processes report zero unfreed blocks.
- `final-lcl/capture-run.log` is painting only, with zero leaks. Its English
  desktop/390 captures in `visual-controls/` were inspected and show the new
  name/Rename/default controls. Captures use the real nested Project scrollbar;
  root Reveal alone did not scroll that sidebar. Existing broader sidebar/widget
  metric and visual-quality gaps remain with the renderer/authoring owners.
- `browser/` compiles current Studio, its matched compiled Pascal worker, shared
  checks and an asynchronous DOM control journey; matched RTL and English hosts
  are staged. These consumers were **not executed** here. Owned warnings are
  zero; the seven distinct installed Classes RTL warnings remain visible.
  Dependencies, profiles, enrollment and live artifacts were not changed.

`tools/build.ps1 -Target state-inspectors` now orchestrates the focused consumers
without launching a listener. Its initial composite run stopped at the native
initializer defect. Current corrected consumers were qualified explicitly as
above; the whole composite command was not needlessly repeated after those passes.
No claim of a new service deployment or authenticated eighteen-tool inventory is
made. The current desktop/LAN server still exposes fifteen tools; source discovery
remains eighteen, as qualified by the preceding semantic packet.

This goal turn is progress. Codegen criteria 1/2 remain accepted, criterion 3 stays
open and its consecutive no-closure count advances **23→24** once. Workflow **3**,
renderer **3**, native authoring **7** and delivery **1** are unchanged. Reassessment
ends scalar fixture expansion: remaining event/structured-collection authoring
routes must consume the same isolated source boundary, with actual ordinary
controls and draft/navigation/history preservation. Full source synchronization,
comfortable large-document editing, current browser execution and complete
both-target/editor/accessibility outcomes retain their original gates. No task
moves to DONE. Final process/session and remote-checkpoint proof follows.

Automatic approval review previously rejected qualification-listener launch as
"blocked by policy" without further reason. No equivalent listener or service
replacement was attempted. Changed browser behavior, authenticated updated tools
and observing Studio still need that permitted host; compilation and native
controls do not substitute those outcomes.

Final preservation check: all eight protected processes match their original
executable paths and exact creation times; no focused fixture remains running.
Native named MCP `nyx_session` remains connected at revision 6, home selection,
one page/one reusable component, no pending draft, Undo unavailable/Redo available
and activity sequence 192. This packet made no semantic mutation of that active
user pair. Private process proof remains in `.local/codex-restart-check/`.

Remote checkpoint: product commit
`ad45f550abda611ca07369ab9c8d9525da73a2d2` is pushed to `origin/hello-nyx`;
the exact remote ref matched locally. The overall Nyx/Nyx Studio goal remains
active and incomplete. Resume with ordinary event/structured-collection routes
through isolated admission, not more scalar fixture expansion. Preserve the
active user pair and all protected services; browser execution, observing
deployment and updated authenticated discovery retain the existing host gate.

Current continuation batch: original codegen criterion 3, consumed by the
ordinary event inspector. The previous turn was progress: queued scalar controls
and exact remote checkpoint are authoritative at ad45f55/a6f344d. Deliver typed
isolated add/policy/confirmed-removal intent, copied pending policies and a
guarded admitted-handler navigation receipt. Qualify private request/reply
admission, real native controls, drafts/history/navigation/load retirement and
current browser compilation. Stop on partial pairs, stale warning removal,
retargeted navigation or lost pending policy input. Collection routing follows
this source-producing callback boundary; current browser execution and observing
deployment retain their existing host gate. Counts remain 24 / workflow 3 /
renderer 3 / native authoring 7 / delivery 1 until handoff assessment.

Event-consumer reassessment: full ordinary journeys exposed fixture assumptions
about absence versus the renderer's raising lookup API, worker delivery occurring
before a preparing paint, and standalone CodeView versus ShellView ownership.
Audit all lookup sites, use the exact pending snapshot through a real independent
LCL inspector for disabled-control proof, and qualify source input through its
actual CodeView. The final journey records bounded phase progress; no passing
subset substitutes its remaining steps. The legacy consumer separately exposed
hidden-host source navigation requesting unavailable native focus. The adapter
now retains the caret and gates focus by CanFocus; deliberate admitted-source
navigation also takes priority over restoring an earlier policy field. Rebuild
and qualify these actual consumers sequentially; native focus tests share desktop
state and are not independent parallel operations. Stop for another unresolved
product/fixture condition rather than silently weakening history/focus evidence.

The remaining named-navigation trial exposed an observation gap: source work can
retire before its queued chrome paint. Waiting only for SourceCommands.Busy does
not establish that a Pascal toggle has painted. Native Studio now exposes its
queued/active presentation flag to embedded hosts; the maintained journey waits
for both source retirement and presentation drainage, then checks both shell
host absence and actual source-control visibility. This preserves the original
visible-navigation assertion rather than replacing it with an elapsed delay.

The bounded toggle probe resolved the repeated failure: the shell host does
disappear and the editor is parked under its hidden, non-focusable parent, but
LCL retains the memo's cached `Showing` flag. The probe records the complete
parent chain and reopening under the visible host in
`build/event-inspector-queue/toggle-probe/run.log`, with zero leaks. The fixture
now checks public `IsVisible` (which includes parents) as well as `Showing`;
the exact shell-host absence and later-navigation requirements remain. This is
an observation correction, not a demonstrated source-pane product defect.
The presentation fence remains necessary to distinguish queued painting from
completed source preparation. Run the full corrected journey and hidden-host
regression sequentially once; a new failure ends this focused investigation.

## Queued ordinary callback authoring — 2026-10-05

The ordinary Events inspector now submits copied typed add/policy/confirmed-
removal intent to independent source preparation. Private request version 4 and
reply version 2 strictly admit event choices and the successful added-handler
receipt; portable project persistence remains version 1. Exact supported-event,
registration/handler and session/load checks precede publication. Related source
and design changes still publish one paired Undo entry. Pending policies remain
visible and confirmed removal locks its exact event before the deferred paint.
Warnings clear only for the successful reviewed removal. Handwritten methods
remain after removal; policy/removal preserve an independent draft and its base,
while Add requires accepted Pascal. A completed Add navigates only while its
captured owner/view is still selected. Native deliberate source navigation takes
focus ahead of an older policy field; hidden hosts retain their caret without
requesting unavailable focus. Browser focus restoration uses exact owner/event
identity and the admitted/pending policy instead of overwriting it with old DOM
input. These are source integrations, not complete browser runtime acceptance.

Ignored evidence is under `build/event-inspector-queue/`:

- `shared-final/` and `shared-3.3-final/` each pass **49** checked typed event
  request/reply/admission checks on native FPC 3.2.0 and 3.3.1, respectively.
  The final case qualifies stale removal presentation after same-ID reload.
  Earlier 48-check trials are superseded, not extra features.
- `generated/` passes **4** checks after compiling the exact exported companion.
  This reconstructs accepted callbacks/policies and independent reusable
  instances; it does not establish executing the callback bodies.
- `shared/state-run.log`, `design/`, and `queue/` pass existing **56 / 140 / 10**
  typed state/binding, detached design/source and real scheduler/presentation
  regressions. All these native processes report zero unfreed blocks.
- `visible-controls/` passes the full **57** actual standalone Win32 callback
  checks, with zero leaks. This includes pending policy/focus, guarded TODO
  navigation, exact confirmed removal/native disabled snapshot, paired history,
  independent draft refusal/preservation, named reusable ownership, later
  navigation, old mounted controls after same-ID reload and detached retirement.
  Its English `controls/events-desktop.png` and `events-390.png` were inspected.
  Narrow controls paint/read correctly; existing desktop sidebar horizontal
  overflow and broader visual/widget metrics remain with their original owners.
- `browser/` compiles Studio, its matched Pascal module worker, the shared ticket
  checks, asynchronous DOM journey and compiled companion reconstruction.
  Matched RTL and English HTML hosts are staged, not served or executed. Owned
  warnings are zero; seven distinct installed Classes RTL warnings remain
  visible. No dependency source, profile, enrollment or live artifact changed.

The maintained `tools/build.ps1 -Target event-inspectors` orchestrates these
consumers without a listener. Its parser check passes; the full composite target
was not repeated after its separately qualified consumers. Earlier full control
trials and their corrected observation/ownership assumptions remain above;
passing shared subsets did not substitute the final actual journey. The existing
native authoring regression and final preservation/checkpoint results follow.

The sequential `visible-legacy/run.log` exposed the same hidden-form refusal
despite the first guard, with zero leaks. This ends the earlier paint/visibility
investigation; the 57-check ordinary journey remains evidence, but the whole
maintained target is not yet qualified. Inspecting the installed LCL contract
established the missing prerequisite: `CanFocus` deliberately excludes the form,
whereas documented `CanSetFocus` checks the complete containing chain. The
adapter now uses that native contract. Finish this bounded correction with one
direct hidden/visible/parked caret consumer and one final existing authoring
regression, sequentially; another failure stops the retry sequence. No weaker
caret/history assertion or additional event fixture expansion is authorized by
this reassessment. The original codegen criterion 3 and no-closure count 25 stay
open/unchanged, with collection integration as the return path after this guard.

The bounded correction is qualified: `focus-probe/run.log` passes **6** exact
native hidden-form/visible/parked/reopened caret and focus checks, including
supplementary Unicode; `form-qualified-legacy/run.log` passes the complete
existing **73** actual native shared authoring checks. Both report zero leaks.
The direct test establishes the final CanSetFocus guard; the earlier 57-check
ordinary journey remains applicable to its unchanged visible-form event flow.
`native-studio-final/build.log`, `browser/studio-qualified-build.log` and
`browser/worker-qualified-build.log` compile the final owned source after the
comment/layout corrections. Owned warnings remain zero, with the same installed
Classes warnings visible on pas2js. Validation stays scoped to these changed
consumers and their existing preservation regressions.

This goal turn is progress, not complete source synchronization or parity.
Codegen criteria 1/2 remain accepted and criterion 3 remains open; its no-closure
sequence advances **24→25** once for this packet. Workflow **3**, renderer **3**,
native authoring **7** and delivery **1** stay unchanged. Reassessment ends this
event/focus investigation. The next deliverable is the existing **17** structured
collection authoring operations through typed isolated intent, preserving exact
schema/row/field/view ownership, pending input, independent drafts/navigation and
paired history through ordinary controls. Comfortable large-document editing,
current browser execution, authenticated updated tools/observing deployment and
complete editor/accessibility outcomes retain their original owners and gates.
No task moves to DONE; the overall Nyx/Nyx Studio goal remains active/incomplete.

Automatic approval review previously rejected qualification-listener launch as
"blocked by policy" without further reason. No equivalent listener or service
replacement was attempted. The current protected desktop/LAN release remains
unchanged; source catalog eighteen/current authenticated fifteen is not updated
deployment evidence. Semantic MCP remains primary; this packet used read-only
session inspection and independent native input consumers for actual widget
behavior the document API cannot establish. Preservation/checkpoint follows.

Final preservation: all **eight** protected process IDs match their original
executable paths and exact creation timestamps; **zero** focused fixtures remain.
Connected native MCP `nyx_session` reports revision **6**, Untitled project /
home, one page/reusable component, no pending draft, Undo unavailable and Redo
available, activity sequence **196**. Only read-only MCP session inspection
occurred on that primary pair. Private identity proof is in
`.local/codex-restart-check/event-inspector-processes.json`. Existing live
artifacts, services, projects and personal compiler/configuration paths remain
untouched by this packet. Remote checkpoint follows.

Remote checkpoint: product commit
`d925b804139e5a9226ecced3bbb00c5d7cebccfe` is pushed to
`origin/hello-nyx`; the exact remote ref matched locally. Native event controls
57, shared authoring 73, direct focus/caret 6, private event admission 49 on both
native compilers and compiled reconstruction 4 pass with zero leaks. Existing
56 / 140 / 10 regressions remain applicable. Browser builds are staged only.
This handoff keeps codegen criterion 3 open at no-closure 25 and preserves the
next structured-collection deliverable and existing host gates above. Its own
clean/remote proof will be stored privately after pushing; the full goal remains
active/incomplete.

Current continuation batch: original codegen criterion 3, consumed by ordinary
structured collection authoring. The previous goal turn is progress: typed event
controls, final native focus eligibility and exact clean/pushed checkpoint are
authoritative at d925b80/3ade3e4. Deliver all seventeen existing operations through
copied typed collection/row/field/view intent, strict isolated replay and pending
presentation. Preserve independent input/drafts/navigation, exact schema families,
reusable ownership and one paired Undo publication. Qualify both native compilers,
actual ordinary native controls, exact emitted companion and current browser
compilation; browser runtime/observing deployment retain the existing host gate.
Stop on retargeted rows/columns, dropped newer input, partial publication or weaker
admission. Codegen no-closure 25 / workflow 3 / renderer 3 / native authoring 7 /
delivery 1 remain unchanged until handoff assessment. Protected services and the
active user pair remain independent of these qualification projects.

## Queued ordinary structured collection authoring — 2026-10-05

Original owner: codegen criterion 3, with the existing collection inspectors as
its ordinary consumer. All seventeen pre-existing operations now capture copied
typed intent before source synchronization. The new portable field reference
retains its scalar family; collection-scoped row references retain their namespace.
Data edits own the exact document collection, while view edits carry the exact
authored control/view and list/table/tree projection. The private processor request
is version 5 with strict collection payload; its reply stays version 2 and portable
project persistence stays version 1. No target object or live store enters replay.

Both adapters route these controls through the independent source processor.
Pending scalar notation, column titles and closed choices remain visible without
entering accepted state. Structural proposals lock their exact data/view controls
before paint. Adjacent compatible scalar/view choices coalesce without replacing
other fields, rows, owners or loads. Reordered columns resolve their exact schema
name/family instead of borrowing a positional field. Focus restoration requires
the complete collection/row/field/owner/family metadata identity; browser restore
uses the admitted/pending value. Successful publication still contributes one
paired Undo entry and preserves independent source drafts/helpers and navigation.

Independent compilation exposed a real source bug: import detection recognized
only two namespace segments, so repeated edits appended collection view/selection
units repeatedly. The emitted companion then failed compilation with duplicate
identifiers (`generated/build.log`). Import detection now compares the complete
qualified name, preserving comments, casing, existing imports and helpers. The
final exact exported companion compiles and drives a real native table callback.
Pending display now also tolerates unadmitted missing/wrong-family metadata while
isolated replay owns its diagnostic. This does not admit such a proposal or change
accepted data; four focused shared consumers qualify that added failure branch.

Ignored evidence is under `build/collection-inspector-queue/`:

- `shared-qualified/` and `shared-3.3-qualified/` each pass **115** checked typed
  request/reply/admission checks on FPC 3.2.0 and 3.3.1. All seventeen operations,
  exact Unicode/NUL/numbers, reordered columns, reusable inheritance, independent
  helpers/drafts/navigation, paired history, malformed descriptors and same-ID
  load retirement qualify. Earlier 104/114 runs are superseded, not extra features.
- `native-controls-final/` passes **71** actual standalone Win32 queued control
  checks. Pending text/focus, numeric refusal/restore, physical structural locks,
  exact row/column/default owners, all seventeen operations, tree mapping, reusable
  scope, drafts/history, later navigation and retired mounted/worker owners pass.
  English `controls/collections-desktop.png` and `collections-390.png` were viewed.
  Narrow binding controls read/paint correctly; existing sidebar horizontal
  overflow and wider native widget/aesthetic metrics remain open.
- `pending-native/` and `pending-3.3/` each pass **4** focused portable presentation
  checks: wrong family, missing field/key and unrelated owner cannot throw from
  panel construction or replace accepted data/source. The successful native
  71-check control path remains applicable to the unchanged valid presentation.
- `generated-final/` passes **8** after compiling the exact exported companion:
  typed reconstruction, supplementary/newline text, reusable/application store
  independence, tree mapping and an actual native table editor callback. Runtime
  edits leave document defaults and a second application untouched. The earlier
  `generated-qualified/run.log` refused an incorrect fixture lookup: reusable
  runtime roots use `instance/definition`, verified in composition/bindings code.
- `legacy-collection-final/` passes existing **27 shared / 29 actual native authoring**
  checks; `source-regression/` passes existing **140 detached design/source / 33
  managed source** checks. The earlier 56 state/binding request checks under
  version 5 remain applicable. These are distinct regressions, not new features.
- All native processes above report **zero unfreed blocks**. Native Studio's final
  `native-studio-final/build.log` compiles the owned guard with zero warnings.
- `browser-final/` compiles Studio, the correctly targeted Pascal module worker,
  shared tickets, focused pending failures, asynchronous DOM controls and the
  exact companion. Matched RTL and four English qualification hosts are staged,
  not served/executed. Owned warnings are **zero**; seven distinct installed
  Classes RTL warnings stay visible. No dependency source or toolchain changed.

The first ordinary fixture needed explicit Bindings/Pascal pane activation;
`native-controls/hidden-source-trial.log` retains its incomplete journey and zero
leak result. Its complete 71-check successor retains the intended assertions.
The three implicit Unicode comparison warnings in the compiled consumer were
fixed with explicit portable text operands, not warning suppression. Product
warnings from the initial draft were likewise corrected. No partial earlier
journey substitutes the final actual consumer or compiled source evidence.
The final warning audit also found six implicit Unicode comparisons in the
older collection authoring fixture. Its final successor retains the same values
through explicit TNyxText operands; the earlier warning log remains retained.
The final native run still passes 27/29 with zero leaks and warnings, and its
browser consumer compiles with zero owned warnings. Final warning audit covers
all ten native build logs above; none retains an owned warning.

`tools/build.ps1 -Target collection-inspectors` now orchestrates these consumers
and stages browser hosts without a listener. Its parser check passes; the full
composite command was not repeated after individually qualifying its consumers.
PROJECT, collection/build guides and original task owners record the scope.

This goal turn is progress. Codegen criteria 1/2 remain accepted; criterion 3
remains open and its no-closure sequence advances **25→26** once. Workflow **3**,
renderer **3**, native authoring **7** and delivery **1** remain unchanged. End
this inspector expansion. Next expose bounded collection/schema/row/view context
and grouped revision/actor-aware semantic operations through the existing workflow
owner, reusing the typed intent and paired candidate machinery. General source/
import workflows, comfortable large-project editing, current ordinary browser
execution, authenticated updated discovery/observing deployment and full editor/
accessibility quality retain their original owners and requirements. No task
moves to DONE; the full Nyx/Nyx Studio goal stays active/incomplete.

The existing automatic approval rejection of listener launch remains "blocked
by policy" without a further reason. No equivalent listener, live artifact
deployment or service replacement was attempted. Source catalog eighteen/current
authenticated release fifteen remains unchanged. Semantic MCP remains primary;
this packet used bounded read-only session inspection and independent native
input consumers for behavior the document API cannot establish. The missing
structured collection vocabulary is recorded with the existing MCP task owner.

Final preservation: all **eight** protected process identities match exact paths
and creation timestamps; **zero** focused fixture processes remain. Connected
native MCP `nyx_session` reports revision **6**, Untitled project / home, one page/
reusable root, no pending draft, Undo unavailable and Redo available, activity
sequence **200**. Primary source/design/history, services, deployed artifacts and
personal configuration remain untouched. Private process proof is
`.local/codex-restart-check/collection-inspector-processes.json`. Remote checkpoint
follows; authorization to push `hello-nyx` remains in force.

Remote checkpoint: product commit
`680b5d4ea9714410f2e2bcd626f63721c18fa541` is pushed to
`origin/hello-nyx`; exact remote/local refs match and the worktree was clean.
Both native compilers pass 115 private admission and four pending presentation
checks. Actual native collection controls pass 71, exact compiled companion/table
callback eight, existing collection authoring 27/29 and source regressions 140/33,
all with zero leaks. Final owned warnings are zero; browser consumers remain
staged only with the same seven installed RTL warnings visible. This handoff
keeps codegen criterion 3 open at no-closure 26 and the next structured semantic
workflow and host gates intact. Its own clean/exact remote proof is stored
privately after pushing; the full goal remains active/incomplete.

Current continuation batch: original workflow criterion 5 consumes the qualified
collection intent/source prerequisite. The preceding goal turn is progress:
680b5d4/b9e6b9a is the exact clean/pushed checkpoint, with private preservation
proof. Deliver bounded collection/schema/row/value/domain/view context and grouped
typed semantic edits through the existing revision/permission/actor/receipt and
paired candidate boundary. Preserve scoped references, defaults versus runtime
stores, handwritten source, pending drafts, exact Unicode/numbers, independent
projects/reviews, and observing activity. Reuse ordinary collection commands;
allow named schemas/fields/rows without requiring agents to simulate palette
clicks. Qualify native compilers, offline actual discovery and exact compiled
native controls; browser compilation/updated authenticated observing execution
retain the existing host gate. Stop on partial groups, weakened reference/domain
admission, hidden document dumps or implicit context retargeting. Workflow
no-closure 3 / codegen 26 / renderer 3 / native authoring 7 / delivery 1 remain
unchanged until handoff assessment. Eight protected services and the primary
revision-6 user pair remain independent of qualification projects. No sub-agents.

## Semantic structured collection commands — 2026-10-05

Original workflow criterion 5 now consumes the qualified inspector prerequisite.
`nyx_collections` exposes eight bounded modes: list, schema, rows, value, domain,
bindings, column and apply. Queries inspect document defaults, never application
runtime stores. Rows return IDs unless exact fields are requested; typed text/
default/domain/column previews contain at most 80 Unicode scalars, and exact text
windows at most 4096. Paging, revision, exact authored owner and the existing
48 KiB response budget stay explicit. Queries preserve selection/drafts/history.

The public immutable `INyxCollectionPatch` owns typed named schema/field/row/view
proposals and all seventeen ordinary editor intents. Define, field upsert/removal,
row append/partial update/move and complete fluent bindings replay ordinary
commands on one detached session. Each ordered intermediate must admit; clear
dependencies before removal. One publication creates one paired Undo step,
preserving handwritten helpers, scoped/case-sensitive references, schema families,
domains, exact text/numbers and independent navigation. Wrong primitives,
duplicate cells, foreign/missing references, drafts and late failures refuse
atomically. Permission/revision/transport actor/receipt/context/activity reuse
the existing agent boundary. No protocol or persistence migration is needed.

Qualification exposed a real ordinary-editor defect: clearing a reusable binding
hid the inherited key needed by Restore. The inspector now retains its Restore
button, while capture/replay recheck this exact owner's inherited contract on a
detached document. The typed read-only query supplies bounded `restorable` context
and title windows beneath the mask. Case-distinct keys refuse; other instances,
selection and paired history stay independent. An absent inherited contract
reports no restorable binding, rather than borrowing another owner's descriptor.

Maintained entry point: `tools/build.ps1 -Target collection-bindings`. Its parser
passes; the composite command was not repeated after the applicable individual
consumers below. Checked builds use Delphi mode, assertions, range/overflow/I/O
checks, debug lines and heap tracing, with separate stable FPC 3.2.0 and matched
LCL/FPC 3.3.1 outputs. Evidence is retained under ignored
`build/agent-collections/`:

| Consumer | Actual evidence | Artifact directory |
| --- | --- | --- |
| Portable semantic journey, FPC 3.2.0 / 3.3.1 | 96 / 96 checks; all seventeen intents, atomic failures, receipts, drafts, Unicode/NUL, numeric boundaries, schemas/domains and scoped ownership | `shared-final/`, `shared-3.3-final/` |
| Final masked-inheritance context, both native compilers | 14 / 14 checks; local/effective/restorable, scalar title window, exact key refusal and paired Undo/Redo | `masked-inheritance-stable-final/`, `masked-inheritance-final/` |
| Actual offline `NyxStudioMCPTools` builder | 57 checks; nineteen-tool catalog, eight modes, strict typed branches and context/value exclusivity | `discovery-complete/` |
| Existing scalar discovery / semantic regression | 14 / 55 checks | `discovery-final/`, `compile/state-regression.log` |
| Existing collection queue, both native compilers | 123 / 123 checks, including clear/restore isolated replay | `queue-stable/`, `queue-regression/` |
| Actual standalone Win32 collection controls | 76 checks; real Restore button, pending input/locks, focus, independent owners, drafts/history and retired loads | `ordinary-controls-qualified/` |
| Unchanged semantic companion compiled and executed with actual Win32 controls | 11 checks; typed table text/Integer/Boolean/Number callbacks, defaults and independent runtime/reusable stores | `compiled-controls/` |
| Full native Studio | Compiles with zero owned warnings | `native-studio-qualified/build.log` |
| Current pas2js Studio, module worker, semantic/inheritance/queue/DOM/exact companion consumers | Compile with zero owned warnings; seven distinct installed `Classes` RTL warnings stay visible | `browser-qualified/` |

Every executed successful native consumer reports zero unfreed blocks. The
96-check journeys precede the final read-only restorable context/window additions;
the final 14 checks on both native compilers, actual 57-check discovery and current
browser compilation qualify those additions. Applicable mutation/control evidence
is reused. Stable and matched-compiler exports have identical SHA-256 values:
design `BC562CAA2B1D489E23830EDC63DDA4BBBB69E68C95B680CA9A065AF09A902A4A`;
companion `B5E9D5FE341EDBCC82B60E47DA392ED0A197988FAB526362DE5496FBA0D42803`.
The compiled consumer imports that unchanged companion and reconstructs exact
saved design bytes; source admission alone is not execution evidence.

English desktop/exact-390 native captures in
`ordinary-controls-qualified/controls/` were visually inspected. The focused
inspector paints its text/controls; existing sidebar overflow, wider native
presentation and complete accessibility remain open. Supplementary characters
and NUL are qualification values, not starter/demo copy. The 50 KiB domain fixture
proves bounded responses/exact values, not acceptable latency: the fully traced
broad journey takes minutes and allocates extensively. Release performance and
comfortable large-project editing keep their original gates. Current browser
programs/English hosts are staged with matched RTL, not served/executed.
Initial failed value-literal comparison (native Extended promotion in the fixture),
masked-inherit replay and actual Restore capture logs remain retained. The guards
and comparison were corrected, not suppressed. Earlier incomplete-case warnings
have explicit fallbacks.

This batch is progress but closes no full criterion. Workflow criterion 5 stays
open; no-closure advances **3→4** once. Codegen **26**, renderer **3**, native
authoring **7** and delivery **1** remain unchanged. No DONE movement or completion
credit follows. Reassessment ends collection query/schema/fixture expansion.
Next deliver bounded general import inspection and revision-aware grouped import
authoring through the same paired boundary, preserving helpers, callback
signatures, managed views and deliberate source order. Qualify an unchanged
compiled companion and actual consumer; refuse ambiguous/conditional edits or
pending drafts. General source editing, richer reusable workflows, root ordering,
review lifecycle, full presentation/accessibility and comfortable source
synchronization remain with existing owners. Browser/observing and authenticated
updated discovery retain their requirements; no narrower parity claim replaces them.

The preceding status-only turn changed no implementation/acceptance state. This
continuation changes maintained semantic vocabulary and the actual Restore
consumer. Semantic MCP remains primary: connected handles supplied bounded
read-only session inspection; independent semantic/native input consumers qualify
operations absent from the protected deployed catalog. Final inspection reports
revision **6**, Untitled project / home, one page/reusable root, no pending draft,
Undo unavailable, Redo available, activity sequence **205**. All **eight** protected
executable paths/exact creation timestamps match; **zero** focused fixture
processes remain. Private proof:
`.local/codex-restart-check/collection-mcp-processes.json`. Source catalog now
has **nineteen** tools; authenticated protected release/current desktop chat still
expose **fifteen**. User work, services, deployed artifacts and personal
configuration remain untouched.

The existing automatic approval rejection of listener launch remains "blocked
by policy", with no further stated reason. No equivalent listener, live artifact
deployment, service replacement or configuration refresh was attempted. Updated
authenticated discovery and observing browser execution remain unqualified.
The full Nyx/Nyx Studio goal stays active/incomplete. Remote checkpoint follows;
the user's authorization to push `hello-nyx` remains in force.

Remote checkpoint: product commit
`629b2de300167f84d9fa216210e3db8d471df60c` is pushed to
`origin/hello-nyx`; exact remote/local references match and its worktree is clean.
Final authenticated `nyx_session` inspection retains revision 6, home, no draft,
Undo unavailable and Redo available; only ordinary read activity advances to 205.
The final warning/heap audit confirms the qualified native consumers above have
zero owned warnings/leaks. Current browser compilation retains zero owned warnings
and seven installed RTL warnings, without an execution/deployment claim.
This handoff preserves workflow count 4, codegen 26 and the concrete general
import return path; the full goal remains active/incomplete. Its own exact remote,
clean-worktree and protected-process proof is stored privately after pushing at
`.local/codex-restart-check/collection-mcp-remote-proof.json`.

Current continuation: the previous goal turn is progress, with product 629b2de /
handoff 9acbe4c exact, clean and pushed. Workflow criterion 5 now follows its
general-import deliverable: typed interface/implementation import inspection and
grouped add/remove proposals through the existing `nyx_pascal` boundary. Preserve
authored order/comments, managed builder, callback signatures/helpers, navigation,
draft refusal and one paired Undo. Qualify lexical boundaries on both native
compilers, real semantic revision/actor/receipt refusal and an unchanged companion
whose actual controls execute its imported helper. Compile current browser
consumers; execution/authenticated updated observation retains the host gate.
Stop on guessed conditional ownership, file-path clauses, partial publication,
lost comments/source/history or document mutation. No repeated collection/schema
expansion, count reset or narrower full-criterion claim follows. Workflow 4,
codegen 26, renderer 3, native authoring 7 and delivery 1 remain until handoff.
Protected services and the revision-6 user pair stay independent; solo execution.

## Semantic Pascal imports — 2026-10-05

Workflow criterion 5's general import deliverable now has two additional modes in
the existing `nyx_pascal` tool. `imports` pages exact interface/implementation
namespaces and one-based source lines at a revision, with 20 default / 50 maximum
entries. `edit-imports` admits 1..32 ordered add/remove commands through a managed
typed `INyxImportPatch`, distinct `TNyxPascalUnitRef` and closed section/action
enums. The portable reader reuses the existing Unicode lexer, never DOM/LCL types.
ASCII Pascal namespace identity is case insensitive. Private token offsets do not
escape into wire identities or survive source replacement.

Append preserves authored order; removing the final unit retires its uses clause.
Every ordinary comment retains exact bytes, including comments between namespace
parts. Separator indentation may retire with a removed final import so remaining
source reads naturally. File clauses, code/options/path payloads, duplicates,
missing removals and conditional/directive ownership refuse. Whole groups replay
ordinary source Apply on one independent session and must preserve exact design,
helper/callback bodies, signatures and the managed builder. One final publication
adds one paired Undo step. Fresh revision, operator permission, transport authority,
exact receipts, outer workspace/review routing, draft refusal and response
preflight reuse the existing guarded boundary. Queries preserve selection/history
and expose accepted imports plus pending-draft status, not complete source text.

The maintained command was actually executed:
`tools/build.ps1 -Target pascal-imports`. It starts no listener, deployment or
configuration refresh. Native checks use Delphi mode, assertions, range/overflow/
I/O checks, debug lines and heap tracing. Exact current evidence:

| Consumer | Actual result | Ignored artifact |
| --- | --- | --- |
| Lexical namespace/comment/section/conditional/file-clause checks, FPC 3.2.0 / matched 3.3.1 | 17 / 17 | `build/imports/maintained/run.log`, `build/imports/lexical-matched-final/` |
| Typed semantic query/group/draft/authority/receipt/history checks, both native compilers | 31 / 31 | `build/imports/semantic-stable-qualified/`, `semantic-matched-qualified/` |
| Actual offline MCP discovery builder | 22; nineteen tools, four Pascal modes, strict nested changes and outer context exclusivity | `build/imports/maintained/run.log` |
| Exact companion compiled unchanged with real Win32 memo input | Nine; newly imported Math/SysUtils helpers, actual callback execution, Unicode scalar limits and rejected-value preservation | `build/imports/controls-final/`, `build/imports/maintained/run.log` |
| Existing local callback source/transaction regression | 72 | `build/imports/handler-regression/` |
| Maintained full target | 17 / 31 / 22 / nine consumers pass, matched RTL and three English browser hosts staged | `build/imports/maintained/run.log`, `build/pascal-imports/` |
| Current pas2js lexical/semantic/exact control consumers | Compile; unexecuted | `build/pascal-imports/browser/` |
| Current browser Studio / Pascal module worker | Compile; unexecuted | `build/imports/browser-studio/` |

All successful native consumers report zero unfreed blocks and zero owned
warnings. Current browser compilation reports zero owned warnings; the same seven
distinct installed `Classes` RTL warnings stay visible. Stable/matched exports
agree exactly: design SHA-256
`C3335E4E10F4ADDBC528FCB690AEE41A332A68B57CEFA1FE646F6F29D9E63580`,
companion `B048C2AFFFA0E4D9B5603ABE9DD371D3BFF9A4F0D8FFAF212BE660A282305850`.
The actual compiled consumer reconstructs those saved design bytes. Successful
source admission is deliberately distinguished from compiler resolution/execution.
Import edits can leave helper type/unit errors for ordinary `nyx_build`; they
never supply compiler paths/search options or execute source themselves.

Initial fixture failures remain in `build/imports/controls/` and
`controls-qualified/`: the first harness lacked a mounted native memo handle,
then selected the generic pre-edit family while expecting a proposed-text payload.
The corrected consumer mounts its actual widget and authors the existing typed
`OnBeforeTextInput` contract, rather than claiming that a setter simulates a real
before-input notification. It now qualifies physical control/model retention and
actual compiled execution. Initial fixture Unicode promotions and an enum-case
warning were corrected with exact portable text and explicit ordinal fallback;
warnings were not suppressed and dependencies were not patched. Native control
notifications qualify this Win32 input path, not other widgetsets or hardware/IME.
Browser hosts are staged only; no visual/runtime/deployment acceptance follows.
The fully traced semantic fixtures are correctness evidence, not release latency.

This continuation is progress: it changes maintained public source semantics and
qualifies actual compiler/control consumers. It closes no full criterion; workflow
criterion 5 remains open and no-closure advances **4→5** once. Codegen **26**,
renderer **3**, native authoring **7** and delivery **1** remain unchanged. No
completion credit or DONE movement follows. Reassessment ends import/schema/
fixture expansion. Next deliver bounded general helper source inspection and
exact guarded grouped edits through this same owned source boundary, preserving
signatures/overloads/helpers/managed views and refusing conditional or ambiguous
ownership. Require an unchanged compiled companion and actual consumer. General
helper/class/full-unit authoring, richer reusable workflows, root ordering,
review lifecycle, complete source synchronization/performance, presentation and
accessibility retain their original owners and full acceptance requirements.

Connected native MCP inspection remains primary and reports revision **6**,
Untitled project / home, one page/reusable root, no pending draft, Undo unavailable,
Redo available, activity sequence **207**. Independent semantic/compiler/input
consumers qualified the new source modes absent from the protected deployed
catalog. All **eight** protected paths/exact process creation timestamps match;
**zero** focused fixture processes remain. Private process proof is
`.local/codex-restart-check/import-processes.json`. User files/history, live
services/artifacts and personal configuration stay untouched. Source inventory is
still nineteen; the authenticated protected release/current chat inventory is
fifteen and retains its older Pascal schema.

The prior automatic approval rejection of listener launch remains "blocked by
policy", with no further stated reason. No equivalent listener, service
replacement, live artifact deployment or configuration refresh was attempted.
Updated authenticated discovery and observing/browser execution remain required.
The full Nyx/Nyx Studio goal stays active/incomplete; remote checkpoint follows.

Remote checkpoint: product commit
`12519448d27b30b43db751299e6314e83e0d5360` is pushed to
`origin/hello-nyx`; exact remote/local references match and its worktree is clean.
Final authenticated `nyx_session` inspection retains revision 6, home, no draft,
Undo unavailable and Redo available; only read activity advances to 208. All eight
protected process paths and exact creation timestamps match; zero focused import
fixture processes remain. The qualified 17/31 lexical/semantic checks on both
native compilers, 22 discovery checks, nine actual compiled-control checks and
72 callback regressions retain zero owned native warnings/leaks. Browser programs
and Studio compile but remain unexecuted at the recorded host gate. Source catalog
nineteen / authenticated release fifteen and workflow no-closure count 5 remain;
the next deliverable is guarded general helper inspection/editing, not more import
fixture expansion. The full goal remains active/incomplete. Exact handoff remote,
clean-worktree and protected-process proof is stored privately after pushing at
`.local/codex-restart-check/import-remote-proof.json`.

## Semantic Pascal helpers — 2026-10-05

Previous goal turn was progress: product 1251944 / handoff ee05da5 are an exact
clean/pushed Pascal-import checkpoint. This continuation delivers criterion 5's
general helper implementation boundary rather than repeating import/schema
expansion. Public `TNyxRoutineRef`, closed routine-kind enum and immutable
`TNyxRoutineSource`/`TNyxRoutineCatalog` share the existing Unicode lexer and
callback block balancer. Ordinary procedures/functions, qualified class methods
and constructors/destructors preserve exact signatures through their first
semicolon. Escaped names retain authored ampersands in signatures and compiler-
style unescaped case-insensitive references. Nested declarations/routines remain
owned by the enclosing implementation; type members, procedural declarations,
comments and literal decoys do not become separate targets.

The existing `nyx_pascal` tool now has seven modes. `routines` pages declaration-
order name/kind/line/editability/reason context (20 default / 50 maximum, up to
4096 catalog entries); `routine` returns at most 4096 Unicode scalars of one
uniquely identified accepted implementation. Signatures preview at most 1024
scalars and expose their full size. Conditional/directive, duplicate/overloaded,
forward/external and generated ownership remain visible/refused, rather than
guessed. Managed BuildNyxDocument/BindNyxCallbacks cannot be replacement targets.
Unsupported generic/symbolic-operator and wider class/full-unit language forms
retain the full-source owner; this lexical service is not a complete Pascal parser.

`edit-routines` takes 1..16 immutable `TNyxRoutineEdit` proposals through managed
`INyxRoutinePatch`. Exact expected text, distinct names, 32768-scalar per-field
and 131072 group budgets are checked. Replacement owns local declarations/body
through its final semicolon; no sibling/trailing-comment/directive injection,
no-op publication or guessed signature change follows. Fresh whole-source
candidate admission requires exact design. Response preflight precedes the only
live AdoptProject, yielding one paired Undo step. Permission/revision/transport
authority/receipt/draft and outer workspace/review guards remain. Rejected late
groups preserve source, design, draft, navigation and both history stacks.
Source admission never executes Pascal or establishes type correctness.

Maintained command actually executed: `tools/build.ps1 -Target pascal-routines`.
It starts no listener, changes no configuration and stages only ignored artifacts.
Current successful evidence:

| Consumer | Actual result | Ignored artifact |
| --- | --- | --- |
| Routine lexical boundaries, FPC 3.2.0 / matched 3.3.1 | 21 / 21; local record/nested routines, case/repeat/try/literal decoys, overloads/directives/forwards, class constructors/destructors and escaped identifiers | `build/routines/maintained-final/run.log`, `build/routines/lexical-matched-final/` |
| Exact semantic candidate/queries/group/history/authority/draft/refusals, both native compilers | 31 / 31 | `build/routines/maintained-final/run.log`, `build/routines/matched-qualified/` |
| Actual offline MCP discovery builder | 31; nineteen tools, seven Pascal modes, bounded text/group budgets and strict outer-context routing | `build/routines/maintained-final/run.log` |
| Unchanged emitted companion and mounted Win32 memo | Nine; changed qualified policy/global caption, actual compiled callback, Unicode scalar limits and exact rejected input/model retention | `build/routines/maintained-final/run.log`, `build/pascal-routines/lcl/` |
| Existing callback implementation / Pascal import regressions | 72 / 31 | `build/routines/regression-qualified/`, `build/routines/regression/` |
| Current pas2js lexical/semantic/exact control consumers | Compile; matched RTL and three English hosts staged, unexecuted | `build/pascal-routines/browser/` |
| Browser Studio / source module worker | Compile; unexecuted | `build/routines/browser-studio/` |

All successful qualified native consumers report zero unfreed blocks and zero
owned warnings. Current browser compilation retains zero owned warnings and
seven distinct installed Classes RTL warnings, visible and unsuppressed. Stable/
matched exports agree exactly: design SHA-256
`46DA063DD8EC5CFE83E674C001EB7A24483F6431ED14A7120BD4D00B3F12FF9D`,
companion `7413269F9B0D220A45E36C39D6C6DE5B6C206F82E22145F0033DFD67C1B9BB1B`.
Actual compiled controls reconstruct the saved design bytes and execute the
edited policy through the generated callback. Native notifications qualify this
Win32 path, not other widgetsets or hardware/IME. No UI structure changed and no
visual/accessibility or release-latency claim follows. English captions and source
qualification Unicode comments remain distinct.

Initial fixture compilation in `build/routines/native-first/` used the wrong
assumption that ApplySourceDraft returns Boolean and that the agent accepts a
session constructor; it was corrected to the existing procedure/paired constructor.
The first callback regression compile revealed four implicit Unicode promotions
in older qualification strings. Explicit TNyxText casts fix those inputs;
`regression-qualified/` passes unchanged 72 checks with zero warnings/leaks.
Initial logs remain. No compiler reinstall, dependency patch or warning suppression
was used. Shared class-type recognition now distinguishes class constructors/
destructors from type blocks; lexical consumers qualify this change.

This packet is progress and closes no full criterion. Workflow criterion 5 stays
open; no-closure advances **5→6** once. Codegen **26**, renderer **3**, native
authoring **7** and delivery **1** remain unchanged; no DONE/percentage credit.
Reassessment ends routine/schema/fixture expansion. Next deliver guarded helper
declaration creation/removal through the same source pair, preserving interface/
implementation ownership, callers, class signatures, callbacks and managed views.
Require ordinary diagnostics and an unchanged compiled consumer; refuse guessed
conditional/overload ownership and incomplete publication. Full general helper/
signature/class/unit authoring, richer reusable semantics, root ordering, review
lifecycle, complete source synchronization/performance, presentation and
accessibility retain original owners and acceptance gates.

Connected native MCP remains primary and reports revision **6**, Untitled project/
home, one page/reusable root, no pending draft, Undo unavailable and Redo available;
only read activity advances to **210**. New modes qualified through independent
Pascal semantic consumers because the protected release still has its older schema.
All **eight** protected paths/exact creation timestamps match; **zero** focused
fixture processes remain. Private process proof is
`.local/codex-restart-check/routine-processes.json`. Live user work/history,
services/artifacts and personal configuration stay untouched. Source catalog
nineteen/current authenticated release fifteen remains explicit.

The earlier automatic approval rejection of listener launch remains "blocked by
policy", with no further stated reason. No equivalent listener, service replacement,
live artifact deployment or configuration refresh was attempted. Updated authenticated
discovery and observing/browser execution remain required. The full goal stays
active/incomplete; authorized hello-nyx remote checkpoint follows.

Remote checkpoint: product commit
`9ddbb1455b4574d9a960684667c5d89cfd031544` is pushed to
`origin/hello-nyx`; exact remote/local references match and its worktree is clean.
Final authenticated inspection retains revision 6, home, no draft, Undo unavailable
and Redo available; only read activity advances to 211. All eight protected process
paths/exact creation timestamps match and zero focused fixture processes remain.
The final qualified audit retains 21/31 lexical/semantic checks on both native
compilers, 31 discovery checks, nine actual unchanged compiled-control checks and
72/31 callback/import regressions, with zero owned native warnings/leaks. Current
browser compilation retains seven visible installed RTL warnings and zero owned
warnings; execution/deployment remain unqualified. Source nineteen/release fifteen,
workflow no-closure 6 and the guarded helper declaration return path are preserved.
The full goal stays active/incomplete. Exact handoff remote/clean/protected-process
proof is stored privately after pushing at
`.local/codex-restart-check/routine-remote-proof.json`.

## Semantic helper declarations — 2026-10-05

Previous goal turn was progress: product 9ddbb14 / handoff 6ca4197 are an exact
clean/pushed helper-editing checkpoint. This continuation delivers criterion 5's
unit-helper declaration boundary. Portable immutable `TNyxRoutineDeclaration`
uses closed kind/visibility choices; `TNyxRoutineDeclarationSource` exposes typed
interface/implementation parts with exact text and source lines. It shares the
existing Unicode lexer and routine ownership rules. Public functions/procedures
create both counterparts; private helpers create implementation only. Signature
matching ignores ordinary comments/case/whitespace while retaining literal and
symbol meaning. Duplicate identities, extra prototype modifiers, directives,
conditional ownership, sibling injection and guessed overloads refuse.

The existing `nyx_pascal` now has nine modes. `declaration` provides bounded
4096-scalar windows of either signature counterpart, including complete private
implementation signatures beyond the older 1024-character preview. Private
interface queries return empty text/line zero. `edit-declarations` consumes
1..16 ordered immutable create/edit/remove proposals through managed
`INyxDeclarationPatch`; each fragment permits 32768 scalars and the group 131072.
Its strict action-specific schema excludes nested context/compiler/source-unit
options. Bodies may be edited through existing qualified routine semantics;
creation/removal concern ordinary free unit helpers. Class-member signatures and
managed BuildNyxDocument/BindNyxCallbacks stay protected.

Definitions retain successive helper ordering before class/managed consumers;
a possible earlier caller can choose an earlier safe site. Attached inline
comments and leading comments of following declarations retain adjacency.
Conditional state is checked at the actual insertion gap. Removal acknowledges
exact interface/signature/body ownership and conservatively refuses retained
possible identifier references, including compiler directives. Comments/literals
otherwise do not count as callers. Lexical shadows/member names can refuse;
external-unit callers and expression/type resolution are not established.
Acknowledged comments inside removed spans retire with them; surrounding comments
remain. Ordinary `nyx_build` diagnostics and target consumers remain required.

One private ordinary source-admission session checks the final exact design/
source pair before response preflight and the only live publication. Related
operations yield one paired Undo step; late refusal retains source, design,
draft, navigation and history. Fresh permission/revision/transport authority,
actor-bound receipts, outer workspace/review routing and pending-draft refusal
remain unchanged. Source admission does not execute code or prove compilation.

Maintained command actually executed: `tools/build.ps1 -Target pascal-declarations`.
It starts no listener, changes no configuration and stages only ignored output.
Successful evidence uses Delphi mode, assertions, range/overflow/I/O checks,
debug lines and native heap tracing:

| Consumer | Actual result | Ignored artifact |
| --- | --- | --- |
| Declaration lexical boundaries, FPC 3.2.0 / matched 3.3.1 | 16 / 16; counterpart ownership, comments, duplicates, retained/directive callers, class/managed refusal and actual conditional gaps | `build/declarations/maintained-qualified/run.log`, `build/declarations/matched/nyx_declaration_lexical_tests-run.log` |
| Typed semantic query/group/authority/draft/history/refusals, both native compilers | 36 / 36 | `build/declarations/maintained-qualified/run.log`, `build/declarations/matched-qualified/` |
| Actual offline MCP discovery builder | Final 46; nineteen tools/nine modes, bounded counterpart parts and three strict mutation actions | `build/pascal-declarations/lcl/schema-qualified-run.log` |
| Unchanged emitted companion and mounted Win32 memo | Nine; compiled six-character policy/English caption, actual callback execution and exact supplementary-Unicode rejection | `build/declarations/maintained-qualified/run.log`, `build/pascal-declarations/lcl/` |
| Existing routine / callback source regressions | 31 / 72 | `build/declarations/regression/` |
| Maintained full command | 16 / 36 / 45 / nine pass; the final additional declaration-part schema assertion passes in the separate 46-check run above | `build/declarations/maintained-qualified/run.log` |
| Current pas2js lexical/semantic/exact control consumers | Compile; matching RTL and three English hosts staged, unexecuted | `build/pascal-declarations/browser/` |
| Browser Studio / Pascal module worker | Compile; unexecuted | `build/declarations/browser-studio/` |

The semantic group creates private TextBudget and public EnglishCaption, edits
retained TNotePolicy.Limit and removes obsolete HelperCaption. Actual unchanged
companion compilation/execution qualifies the resulting six-character policy,
English caption and saved-design reconstruction through the mounted memo's
compiled OnBeforeTextInput callback. Six scalars including a supplementary moon
are accepted; seven are rejected with exact physical/model retention. Read-only
and pending-draft tests propose an otherwise valid changed implementation, so
they qualify those guards independently of no-op refusal.

All successful native consumers have zero owned warnings and zero unfreed
blocks. Browser compilation has zero owned warnings; seven distinct installed
Classes RTL warnings remain visible and unsuppressed. Stable/matched exports
agree exactly: design SHA-256
`46DA063DD8EC5CFE83E674C001EB7A24483F6431ED14A7120BD4D00B3F12FF9D`,
companion `89E1F8DE2951FC37DBEF076A4578C931B9B929DA054671A97DC0A71D313253F2`.
Native notifications qualify the exercised Win32 input path, not other widgetsets
or hardware/IME. No UI structure changed; no visual/accessibility or release-
latency acceptance follows. English review text and dedicated Unicode inputs
remain separate. Fully traced fixtures establish correctness, not release speed.

Initial lexical failure in `build/declarations/lexical-first/` expected refusal
at a safely preceding insertion gap. The corrected fixture actually conditions
the implementation section; final checks qualify gap ownership without weakening
the algorithm. Initial compilation in `build/declarations/native-first/` used
a scalar-count helper not exported by nyx.text. The implementation now counts
through existing NyxNextScalar, avoiding an unnecessary runtime/event dependency.
Initial logs remain. No compiler reinstall, dependency patch or suppression was
used. Current browser consumers were compiled but not executed.

This packet is progress and closes no full criterion. Workflow criterion 5 stays
open; no-closure advances **6→7** once. Codegen **26**, renderer **3**, native
authoring **7** and delivery **1** remain unchanged; no DONE/percentage credit.
Reassessment ends declaration/schema/fixture expansion. Next deliver guarded
paired routine signature authoring, retaining parameter/result counterparts,
caller meaning and ordinary compiler diagnostics through this same boundary.
Class/full-unit authoring, richer reusable semantics, root ordering, review
lifecycle and complete source synchronization/performance/presentation/
accessibility retain their original owners and acceptance gates.

Connected native MCP remains primary and reports revision **6**, Untitled project/
home, one page/reusable root, no pending draft, Undo unavailable and Redo available;
only read activity advances to **213**. Independent Pascal semantic/compiled
consumers qualify modes absent from the protected deployed schema. All **eight**
protected paths/exact process creation timestamps match; **zero** focused fixture
processes remain. Private process proof is
`.local/codex-restart-check/declaration-processes.json`. Live user work/history,
services/artifacts and personal configuration stay untouched. Source catalog
nineteen/current authenticated release fifteen remains explicit.

The earlier automatic approval rejection of listener launch remains "blocked by
policy", with no further stated reason. No equivalent listener, service replacement,
live artifact deployment or configuration refresh was attempted. Updated authenticated
discovery and observing/browser execution remain required. The full Nyx/Nyx Studio
goal stays active/incomplete; authorized hello-nyx remote checkpoint follows.

Remote checkpoint: product commit
`845c69810f82db85de2899190d9e7b6a2220e954` is pushed to
`origin/hello-nyx`; exact remote/local references match and its worktree is clean.
Final authenticated inspection retains revision 6, home, no pending draft, Undo
unavailable and Redo available. All eight protected process paths/exact creation
timestamps match; zero focused declaration fixture processes remain. Final
qualified evidence retains 16/36 lexical/semantic checks on both native compilers,
46 actual discovery checks, nine unchanged compiled-control checks and 31/72
routine/callback regressions, with zero owned native warnings/leaks. Browser
consumers/Studio/module worker compile; runtime/deployment retain the host gate.
Source nineteen/release fifteen, workflow no-closure 7 and the guarded paired
signature return path remain explicit. No full criterion closes; the full goal
remains active/incomplete. Exact handoff remote/clean/protected-process and final
read-only activity proof is stored privately after pushing at
`.local/codex-restart-check/declaration-remote-proof.json`.

## Semantic helper signatures — 2026-10-05

Workflow criterion 5's guarded signature return path now uses the same typed
declaration group and paired source boundary. A complete replacement retains
the exact acknowledged implementation signature/body and interface counterpart,
then changes parameter/result and related explicitly authored callers together.
Routine identity and visibility remain owned. Class/managed/conditional/overload
boundaries, stale expected text, foreign receipts, no-op proposals, pending drafts
and late group failures refuse without partial source/design/history publication.
No caller rewrite or type success is inferred from source admission.

| Evidence | Current result |
| --- | --- |
| Checked FPC 3.2.0 and matched 3.3.1 | 20 lexical / 59 semantic each, zero owned warnings/leaks |
| Maintained `pascal-declarations` | Actual nineteen-tool/nine-mode discovery 50; unchanged compiled Win32 input nine |
| Deliberately outdated public caller | Named parameter/type diagnostic on both native compilers and pas2js |
| Existing source regressions | Routine 31 / callback 72, zero leaks |
| Browser consumers | Lexical/semantic/generated input compile; execution remains gated |

Logs remain under `build/signatures/native-first/`, `matched/`, `final/` and
`regression/`. Both native exports and the maintained export have exact matching
SHA-256 design `46DA063DD8EC5CFE83E674C001EB7A24483F6431ED14A7120BD4D00B3F12FF9D`
and source `38854BB7691495D5F3C0C02FD2F5990DF8E39B79957751ED77CB3B5B58B5F1AC`.
Actual compiled callbacks enforce the new five-character budget, parameterized
English caption and exact supplementary-Unicode rejection. These are actual
Win32 notifications, not hardware/IME or another widgetset qualification.

Initial maintained runs in `maintained/` and `qualified/` stopped on an inadequate
old-caller fixture: bare procedural values caused an unrelated I/O-type error.
The final fixture assigns to a typed text destination and requires the intended
named incompatibility/argument diagnostic. `diagnostic-probe/` retains the real
native compiler error. The gate was not weakened to generic compilation failure.
Seven installed pas2js RTL warnings remain visible; dependencies were not edited.

This packet is progress, with no full criterion closure: workflow no-closure
advances **7→8** once. Reassessment ends signature/schema/fixture expansion.
The user-directed source UX below is the next concrete deliverable; class/full-unit
and richer reusable/source workflows retain their original open owners. Current
authenticated observing discovery/browser execution retain the existing host gate.

## Source workspace and expanded editor — 2026-10-05

The user's screenshot establishes compiler messages consuming nearly all source
height. Original codegen criterion 3 now has a shared Nyx Source/Compiler messages
workspace and Expand/Close actions. Messages occupy their own scroll area; the
same independent source pane and code editor move into a public modal host.
No second editor/draft is created, and presentation spends no document history.
Per-project typed source-view/expanded preferences use strict version 3 with
exact version-2 migration. Diagnostic navigation selects Source before its caret.

`nyx.modal` supplies managed `INyxModalHost` with portable Show/Hide/options and
borrowed dismiss notification. Browser DOM and LCL controls remain in adapters;
all source UI content is ordinary public Nyx composition. Native fill sizing
follows modal resize, and identical Show options preserve a manual resize.
Close restores the exact owner's previous input state before reparenting focused
controls. Structural source diagnostic rebuilds park code inside the enabled
modal, and teardown restores borrowed parking ownership before host retirement.

The maintained `source-editor` command executes **225** shared presentation/project
checks on each native compiler and **30** actual Win32 source checks, with zero
leaks/owned warnings. Actual controls cover Source/Messages, retained editor and
draft/range, desktop/narrow/resize, Close/Escape, expanded rejected-draft diagnostics,
expanded Restore/Apply and paired Undo. Earlier full editor/source regressions
pass **74/39** with zero leaks; final current artifacts and logs live under
`build/source-editor/final/`. Adapted event/collection/project/compiler fixtures
compile; this packet does not claim their full service journeys were executed.
Native Studio, browser Studio/module worker and portable presentation consumers
compile. Final current full editor/source regressions also pass **74/39**, with
zero leaks; all four adapted physical/service fixture programs compile. Matched
staged RTL hashes agree; seven installed Classes warnings remain
visible and zero owned warnings were introduced.

Native desktop and narrow PNGs paint English source/workspace text and have been
inspected. The desktop source grows from its earlier roughly 80 pixels to 174
at the existing 65/35 proportion; expanded source receives the larger viewport.
Existing native chrome/sidebar/footer metrics and complete accessibility,
performance and other-widgetset outcomes remain open. The browser dialog follows
HTML dialog/WAI guidance referenced in docs/native-studio.md, checked 2026-10-05;
current browser/mobile rendering, keyboard, focus and observing deployment remain
unqualified. Native captures are not phone evidence.

Failed evidence is retained: `modal-first/` (missing Classes type),
`modal-qualified/` (incorrect guessed fluent method), `modal-accepted/` (fixture
missing Interfaces), the first maintained source run (unrealistic 200-pixel
assertion), and `modal-final/`, `modal-return/`, `modal-command/`, `modal-status/`
(resize/Close failure). Exact status showed `[TCustomForm.SetFocus] ... Cannot focus`;
restoring the owner before transfer resolves it without replacing the memo.
The outdated `regression/` export lacked today's required reusable named part;
the existing current MCP-authored English fixture passes the maintained 74-check
journey in `regression-final/`. No old fixture failure was counted as a pass.

This user-directed packet is progress and closes no full criterion. Original
codegen criterion 3 remains open; no-closure advances **26→27** once. Workflow
**8**, renderer **3**, native authoring **7** and delivery **1** remain explicit;
no DONE or percentage credit follows. End local modal/source fixture expansion
after current relevant checks. Next qualify current browser/phone return, focus,
keyboard and project switching through a permitted host, then return to broader
source/reusable integration and complete ordinary both-target editor outcomes.

Native semantic MCP remains primary and read-only for the active user project;
the observed revision changed to **8** before this turn's source edits, with home,
one page/reusable root, no pending draft, Undo unavailable and Redo available.
No live user source/design/history mutation, service stop, compiler reinstall,
dependency edit or private configuration refresh occurred. All eight protected
process identities match. Source catalog nineteen/protected release fifteen
remains explicit; offline semantic/physical consumers qualify source-only changes.

The earlier automatic approval rejection of listener launch remains "blocked by
policy", with no further stated reason. No equivalent listener, service replacement,
live artifact deployment or configuration refresh was attempted. The phone retains
the protected earlier release. Current browser/authenticated observation and the
full Nyx/Nyx Studio goal remain open; the goal is active/incomplete. The authorized
hello-nyx remote checkpoint follows after final qualification.

Remote checkpoint: product commit
`41793ef1ec30e85e79b7291528ae71c8c984c7d2` is pushed to
`origin/hello-nyx`; exact remote/local product references match and the product
worktree is clean. Final current native source/modal/editor/scheduling consumers
pass **30/74/39**, with zero leaks and owned warnings. Shared presentation passes
**225** on each native compiler; signature evidence remains **20/59/50/9**,
three intended compiler rejections and **31/72** regressions. Native Studio and
all four adapted fixture programs compile; browser Studio/module worker compile
with zero owned warnings and seven visible installed RTL warnings each.
Final English desktop/narrow captures were inspected. Current browser/phone
execution and observing deployment retain the existing policy gate.

Read-only authenticated MCP retains revision **8**, home, no pending draft,
Undo unavailable/Redo available and the user's rating-part selection; no agent
mutation was made. All eight exact protected process identities match; zero
focused fixture processes remain. Workflow **8**, codegen **27**, renderer **3**,
native authoring **7** and delivery **1** remain explicit. No full criterion
closes and the full goal stays active/incomplete. Final exact handoff/remote/
clean/process/session proof is stored privately after pushing at
`.local/codex-restart-check/source-editor-remote-proof.json`.

## Semantic reusable authoring — 2026-10-05

The preceding turn was verified progress: product 41793ef and handoff ed39189
were clean and pushed. This batch returns to original workflow criterion 5,
retaining the full Nyx/Studio north stars and the source/modal browser gate.
It adds four closed operations to the existing semantic transaction engine:
derive an independently owned reusable definition with exact descendant identity
assignments, insert an instance, create/change a named-part override, and restore
inheritance by its exact owner/path/descriptor identity. Mixed ordinary payload
edits publish through one paired design/Pascal Undo step. The ordinary inspector
Create component command now shares this engine instead of mutating its accepted
tree before complete candidate admission.

Portable typed authoring uses distinct control/component/part references,
identity assignments and enum override modes through copied reusable intents.
The derivation helper borrows the original, copies contracts/bindings/callbacks/
extensions and retains referenced definitions/Pascal helpers. Hash indexes avoid
quadratic identity matching without caching mutable authoring state. Unknown
extension reference meanings are retained; no guessed rewrites occur.
Named-part queries opt in to independent 20-default/50-maximum paging, returning
effective paths and exact source/design/local-override identities. Properties and
events coexist in separately bounded windows. Missing/removed and ambiguous
paths remain explicit. Duplicate sibling part names now refuse through the
same portable Part contract instead of silently choosing incidental child order.

The maintained command is `tools/build.ps1 -Target reusables`. Final evidence
is `build/reusables/final/promotion-run.log`: both native compilers pass **52**
semantic checks each, actual MCP discovery **20**, and unchanged compiled Win32
controls **14**, all with zero leaks/owned warnings. Evidence includes grouped
Undo/Redo, delivery receipts, late atomic refusal, map ownership/typing, all five
override modes, restored inheritance, nested reusable derivation, retained
helpers/callbacks/defaults and exact pending-draft refusal. Independent project
work promotes a subtree via derive/delete/instance with its retained authored
identity; actor-owned reviews refuse foreign/retired access. Primary work remains
unchanged. The exact exported companion and design hashes match across both
native compilers:

- Design: `F3893B2E5511ED212C8ABF723EC7C12073D7B246AFB63DC682C8541DDCB5637B`.
- Pascal: `E8876B37B3F1C54053401FD4441892D7A9FB76980BD8614FF402F11BCDD62381`.

Actual native controls mount the unchanged compiled companion, paint English
replacement/append/prepend content and invoke the same retained compiled callback
through original and both reusable button routes. Supplementary Unicode/NUL
defaults remain hidden qualification data, with exact persistence/history/
compiled reconstruction. Named application scalar bindings intentionally share
runtime state; customization never invents an instance-local namespace. Native
input changes runtime projections while preserving the authored document.

Initial failures remain in `build/reusables/native-first/`: the control fixture
first used the outer memo wrapper instead of InputFor, then mistakenly expected
isolation from an explicitly shared application binding. Corrected public handle
lookup and the established shared-state expectation produce the final evidence;
neither failure justified weakening admission or changing runtime semantics.
The first unquoted pas2js shell flag also failed; maintained quoted argument
arrays compile correctly. Earlier compile failures and runs remain retained.

Relevant regressions: `build/reusables/regression/agent-run.log` passes **39**
with zero leaks; the seven owned Unicode conversion warnings in that maintained
fixture are now fixed through explicit TNyxText literals. Detached source/design
checks pass **140** on each native compiler, with zero leaks/owned warnings,
including isolated inspector structural actions. The maintained core command
passes **30** core/**1769** composition-designer/**55** scheduler/**60** paired
project checks, intended compiler type refusals, unchanged generated companion
checks and **3** compiled collection checks. Its normal test artifacts share the
standard build directory; the protected server executable retains its earlier
timestamp and all eight exact service process identities remain unchanged.

Current native Studio, browser Studio/module worker and both browser reusable
consumers compile. The browser consumers remain staged in ignored
`build/reusables/staged/`; seven installed Classes RTL warnings stay visible in
each browser compilation, with zero owned warnings. No changed browser execution,
accessibility, other widgetset, physical hardware/IME or updated authenticated
observing release follows from compilation.

This is progress, not full criterion closure. Workflow criterion 5 no-closure
advances **8→9** once; codegen **27**, renderer **3**, native authoring **7** and
delivery **1** remain unchanged. No DONE/percentage credit follows. End local
reusable schema/fixture expansion and return to the broader ordinary editor/
creator and general-source integration owners: class/full-unit authoring, root
ordering, complete parity/presentation/accessibility/performance and delivery
remain required. Current tool catalog nineteen/protected release fifteen stays
explicit. The full goal remains active/incomplete.

The primary native semantic MCP remains connected/read-only for this work at
revision **8**, home, rating-part selection, no pending draft, Undo unavailable/
Redo available. No user pair/history mutation, listener launch, service stop,
live deployment, compiler reinstall, dependency edit or private configuration
refresh occurred. The earlier automatic approval rejection remains “blocked by
policy,” with no further stated reason; no equivalent deployment was attempted.
The phone retains the older observing release. Exact remote/clean/process/session
proof is stored privately after the authorized push at
`.local/codex-restart-check/reusable-remote-proof.json`.

## Physical designer drag/drop — 2026-10-05

Original Studio authoring criterion 1 now consumes the accepted-in-isolation
placement operation through physical host callbacks. Both ordinary Studio
controllers use the same portable drag broker and public Nyx sources. Palette
buttons offer a copy; the separate inspector grip offers an authored-control
move, preserving canvas input selection/IME. The Nyx-built viewbar supplies
typed inside/before/after intent and the existing two-step keyboard/touch workflow
remains. The narrowed drop selector is visible in the inspected Win32 capture.

Public `NyxDesignerInput.Drops(True)` opts into a copied-identity synchronous
designer port before mounting. Its owner/source/path/container snapshot borrows
no node or control; the adapter seals decisions on return or failure. Application
callbacks and custom native drag hooks stay suppressed. Design views have no
runtime binding store: the first real input run exposed an erroneous call to
the runtime signal bridge. Both adapters now construct the same designer-only
notification instead, with no application value command. Native mounted-policy
refusal, disabled/read-only target input and retained-decision sealing are exercised.

An opaque local per-drag lease guards source mount, exact load/session, active
view, captured placement, draft and creator epoch. Hover reads protected format/
identity only, outlines a visible owner and never refreshes or encodes the pair.
Only a readable exact final lease and unchanged paired files submit one isolated
placement command. External transfers, roots beside roots, leaf containers,
self/cycle moves, retired views and changed pairs refuse. Inherited content needs
an existing exact customized layout descriptor; no implicit override or shared
definition mutation occurs. Drop publication and ordinary Undo/Redo update both
files and preserve the existing source widget and handwritten Pascal.

Maintained reproduction:

```powershell
./tools/build.ps1 -Target designer-drag -BrowserOutput build/designer-drag/staged
./tools/build.ps1 -Target gestures -BrowserOutput build/designer-drag/gesture-browser
```

The first target completes with exit zero in `build/designer-drag/promotion.log`:
56 shared guards on FPC 3.2.0 and the matched FPC 3.3.1, 40 actual Win32 Studio/
adapter-policy checks and seven unchanged compiled-control checks. Native heap
tracing reports zero leaks in each. The exact ordinary-input export hashes are:

- Design: `02CE43B0D0459CF71AD6F7BEE87379A3D026A7CAE88ADA01528915844D77B067`.
- Adjacent Pascal: `E7DC620B007E2EFC91745855B5DE1C3426F7814ED9CF61BD3966D2C6E46A2219`.
- Pair: `00002E59D99F2FF6E3EF12AECC523E5EE7A185C3500626CEA823AD5ACC755DE9`.

The unchanged Pascal reconstructs the exact design and mounts the moved memo
and labeled-button parts; actual native memo editing remains usable. The selective
ordinary editor capture is `build/designer-drag/export/designer-drag.png`.
Its new grip/selector and Source/messages/Expand controls are inspected; broader
chrome clipping/help/visual polish and complete editor performance remain open.
Checked Studio input allocated roughly 505 MB across the journey; this packet
does not establish a production latency or memory budget.

Original gesture regression passes 83 preparation / 84 unchanged compiled native
checks, with zero leaks (`gesture-regression-final.log`). Its two native implicit
Unicode comparison warnings are fixed with typed portable expected constants.
The browser physical probe now explicitly handles other trigger families;
`physical-compile-final.log` qualifies that warning cleanup. Owned current builds
have zero warnings; the installed pas2js Classes unit's seven warnings per
compilation remain visible, without suppression or dependency source edits.
The final Studio browser review's failure cleanup is compiled separately in
`browser/studio-input-final.log` after promotion.

Browser shared guards, the unchanged compiled companion, actual DOM Studio/
worker review, Studio and its module worker compile and ship the matched RTL
into this isolated stage. The English hosts are `designer-drag-guards.html`,
`designer-drag-compiled.html` and `designer-drag-studio.html`. Their execution is
not established here. Synthetic DOM and direct Win32 callback evidence do not
qualify hardware drag-manager negotiation, mobile touch, IME, assistive technology,
disabled-widget hardware hit-testing or another native widgetset. This packet
cannot accept full browser/LCL parity or the whole WYSIWYG criterion.

Failed compile/guard/input evidence stays in `guards/compile-first.log`,
`guards/run-2.log` through `run-6.log`, and `lcl/run-first.log`; the successful
manual final and maintained logs remain alongside them. Early guard failures
were malformed synthetic event metadata/name/value, corrected to the existing
typed event contract. The first real designer signal failure freed all owners.
No original test requirement was reduced to make the packet pass.

Original authoring criterion 1 remains open: no-closure **8→9** once. Workflow
criterion 5 stays **9**, codegen criterion 3 **27**, renderer **3**, delivery **1**.
No task moves to DONE and no full-product percentage closes. End local drag
guard/codec/fixture expansion. Continue ordinary sizing/constraint/snapping/
responsive authoring and broader editor/parity/accessibility/performance outcomes
under their existing owners, preserving all original acceptance and blockers.
Stop on inferred ownership, a second authoring engine or weakened paired publication.

Authenticated read-only native MCP still connects to the protected fifteen-tool
release. Revision eight, selection `rating-2-part-4`, home view, one page/component,
no draft, Undo false/Redo true remain; read activity alone advances 228→229.
All eight protected process path/start identities match the retained baseline;
focused fixtures have finished. Production server size/timestamp remain unchanged.
No listener, protected-release replacement, configuration refresh, compiler
reinstall or dependency edit occurred. The earlier automatic approval rejection
remains “blocked by policy,” with no further stated reason. The phone retains
the older observing release; equivalent deployment was not attempted. Private
process/artifact/remote proof belongs in `.local/codex-restart-check/` after push.

## Nested placement prerequisite — 2026-10-05

Original Studio authoring criterion 1 now has immutable typed control/target
references and closed inside/before/after positions. `NyxPlaceControl`,
`NyxPlaceNewControl` (built-in/custom kind) and `NyxPlacementPatch` use the
existing independent candidate engine. Same-owner positions resolve after
source extraction. Exact container and reusable-part admission refuses roots,
cycles, self-placement, leaf targets, inherited instance children, moved part
descriptors and foreign/occupied identities. An editable properties-only layout
descriptor promotes to append when content arrives; final whole-document and
property admission still refuses incomplete payloads. No second authoring
implementation or platform tree enters this contract.

The existing `nyx_transaction` shares it through `place` and `place-new` and can
group placement with property/reusable operations as one revision-aware paired
Undo. Tool count remains nineteen in source/fifteen in the protected release.
Strict version-6 private worker tickets also read exact prior version-5 intent;
new placement cannot enter the older vocabulary. The ordinary isolated queue
retains project/load and exact pair/schema admission. Source helpers, defaults,
drafts and identities remain owned. The shared Nyx-built inspector offers
**Move to another layout**, followed by ordinary canvas/hierarchy destination
selection and wrapped **Place inside/before/after / Cancel move** controls beside
selection. Arming/canceling create no project history. A changed accepted pair,
draft, Undo/Redo or load permanently retires the armed move, rather than
resurrecting it when a prior pair returns. Actual accepted placement retains the
source widget and selects the moved control's owning view when navigation has
not changed independently.

Maintained qualification: `tools/build.ps1 -Target placement -BrowserOutput
build/placement/staged`. `build/placement/final/promotion-final.log` records 44
semantic/isolated checks on FPC 3.2 and matched 3.3, byte-identical design/Pascal/
paired exports, actual Win32 input/unchanged compiled controls 18 and actual
discovery 24, including all twelve transaction shapes. The final selection-
adjacent wrapped inspector is recompiled and requalified in
`build/placement/lcl-final/controls-run.log`: 18 actual controls, zero leaks and
zero owned warnings; its selective `build/placement/input-final/placement.png`
is inspected. The maintained native journey also qualifies cross-container/
cross-page movement, catalog compound ownership, reusable payload admission,
atomic grouped refusal, deduplication, exact paired Undo/Redo, stale publication,
load retirement, draft/cancel behavior and legacy worker compatibility. Native
regressions in `build/placement/regression-fpc` and `regression-lcl_fpc` pass
140 design/source, ten actual worker/queue and 52 reusable checks on each
compiler, all zero leaks/owned warnings. Ordinary native Studio compiles in
`build/placement/native-studio-final`; final browser Studio compiles in
`build/placement/browser-final`. Browser semantic/unchanged-control consumers
and module worker compile with matched RTL; seven installed Classes warnings
per compilation remain visible, with no dependency edits or suppressions.

Earlier native builds remain in `native-first`; failed/interrupted LCL evidence
is retained in `lcl-first` and the initial promotion logs. Traced debug heap
builds showed slow synchronous editor refresh rather than stranded work; three
focused fixture processes alone were
stopped after exact executable/PID verification, preserving services. A fixture
initially supplied memo label text rather than editable value, then a shell
argument continuation omitted its expected-design path. Both were corrected;
the unchanged compiled control now compares exact design and actual memo value.
The matched compiler's unreachable enum rejection warning was removed using
ordinal dispatch while retaining foreign-choice refusal. Do not infer ordinary
performance, physical hardware, browser execution, phone layout, accessibility
or complete editor quality from these checks/renderings. Broader native chrome
and long-help sizing remain with the existing presentation/parity owner.

This is progress, not full criterion closure. Native authoring no-closure
advances **7→8** once; workflow **9**, codegen **27**, renderer **3** and delivery
**1** remain unchanged. No DONE/percentage credit follows. End local placement
codec/fixture expansion. Next connect public Nyx physical drag/drop on both
targets to this same candidate/keyboard workflow, preserving exact local leases,
pair/project/load/creator barriers and borrowed receiver retirement. Resizing,
constraints, snapping, responsive variants and complete multi-page/reusable
editor presentation, accessibility, performance, parity and delivery remain
required under the original authoring/renderer/source/event owners.

The live semantic MCP is connected and read-only at revision 8, home, retained
rating-part selection, no draft and unchanged Undo/Redo. All eight protected
server executable/start identities are verified; no protected service stop,
listener launch, live deployment, compiler reinstall or private configuration
refresh occurred. The earlier automatic approval rejection remains “blocked
by policy,” with no further reason; no equivalent deployment was attempted.
The phone retains its earlier observing release. Private remote/process/session/
  artifact proof follows the authorized checkpoint in
`.local/codex-restart-check/placement-remote-proof.json`.

## Portable size constraints — 2026-10-05

Original Studio authoring criterion 1 lacked any portable minimum/maximum
dimension contract. This source packet adds copied `TNyxSizeRange` and
`TNyxSizeConstraints` values, four strongly typed configuration methods on every
managed specialized facade, common published integer metadata and readable
generated methods. The maintained Pascal facade generator regenerated its owned
includes; dependency source and existing attribute ordinals are unchanged.
Bounds are optional logical pixels, 0..100000; explicit zero differs from empty.
A complete policy validates before changing its four members. Common and each
effective browser/native pair must remain ordered at document/source/candidate
admission; an empty scoped value clears that bound.

Browser CSS translates the four bounds after sizing defaults, restoring defaults
on clear and retaining its containing-width cap. Native intrinsic measurement,
allocated geometry and cross alignment apply the same admitted bounds. The shared
zero-basis allocator reserves clamped fixed items/gaps, freezes min/max violations
by total adjustment and redistributes the remainder. Wrapped row membership
includes weighted minima; max-capped groups leave free space for justification;
overflow keeps leading alignment. Existing three-field flow records remain
compatible. Assignment conversion also fixes older FPC's unsupported explicit
Integer-to-Double casts in this shared allocator. The implementation uses the
[CSS Flexbox 9.7 freezing rule](https://www.w3.org/TR/css-flexbox-1/#resolve-flexible-lengths)
(Candidate Recommendation Draft checked 2026-10-05), restricted to Nyx grow
weights, without claiming complete CSS sizing/shrinking conformance.

Ordinary Studio exposes all four fields and owner-qualified Unset actions.
Zero remains a real limit. Reset routes through the existing property processor,
source worker and one paired Undo; stale selections and foreign properties
refuse. Existing bounded `nyx_node` and grouped revision-checked
`nyx_transaction` supply semantic admission, including atomic cross-field/
target refusals. No new tool, transaction codec or second authoring engine is
introduced. The strict source reader also accepts copied range/constraint
builders; ordinary Pascal compilation remains necessary for execution/type
evidence.

Actual Studio refusal exposed an existing inspector construction defect:
pending dimensions were sent through the primitive spin's temporary 0..100
domain before installing their published range. Inspector wire proposals now
remain at their explicit metadata boundary. Fields are admitted to the owning
shell before later configuration, and the public shared shell wrapper releases
its owning document if composition refuses. The accepted application property
and paired files still require unchanged typed admission. A rejected 170-pixel
maximum beneath the editor's 180-pixel minimum now restores the accepted field
and reports its diagnostic without leaking a shell or field.

Maintained command:

```powershell
./tools/build.ps1 -Target constraints -BrowserOutput build/constraints/staged
```

Final relevant evidence, in `build/constraints/promotion.log`:

- **237** shared checks on FPC 3.2.0 and matched FPC 3.3.1, with zero heap leaks.
  Copied policies, explicit zero/clear, mixed violations, overflow, hidden/gap/
  wrap behavior, integer rounding invariants, typed source replay, bounded
  semantic queries, grouped mutation/refusals, paired Undo/Redo and isolated
  ordinary-property wire/publication execute. Exact exports match byte for byte.
- **38** checks on the unchanged compiled Win32 companion, with zero leaks.
  Actual widths/heights/positions qualify rows, columns, wrapped weighted minima,
  natural/fixed/fill caps, explicit zero and native overrides. Limit clearing,
  sibling visibility and a 390-pixel actual host preserve the memo's real widget,
  focus, English draft and selection; its minimum can exceed that parent.
- **22** ordinary Win32 Studio checks, with zero leaks. Real selected-component
  controls, worker field admission/refusal, Unset, zero, paired history,
  handwritten source and source-editor identity execute. This qualifies registered
  widget callbacks, not hardware/IME/assistive technology or another widgetset.
- Original native layout/policy consumer passes **2169** checks, with zero leaks,
  using its exact maintained semantic `build/layout-policy/source` companion.
  Core regression passes **30/1777/55/60**, compiler-family refusals including
  untyped constraints, unchanged generated applications/source/Unicode/runtime
  binding consumers and three compiled collection checks.
- Browser shared/control consumers, Studio and its module worker compile. Two
  English hosts and matched RTL are staged. Updated browser/phone runtime and
  authenticated observing qualification retain the existing host gate; no
  listener or equivalent deployment is attempted. Owned final compiles report
  zero warnings. Installed pas2js `Classes` still emits seven visible warnings
  per program; its dependency source and warning policy remain unchanged.

The selective actual native capture `build/constraints/export/constraints.png`
is inspected at the final narrow host. Deliberately small maxima visibly clip
their controls: this is dimension evidence, not aesthetic/full presentation
acceptance. Border/padding metrics, arbitrary mixed intrinsic constraints,
flexible shrinking, all widgetsets, performance budgets, complete accessibility
and broader editor presentation remain under their original owners.

Failed evidence is retained privately: first contract/source compilation and
test expectation logs; the initial whole-shell leak and subsequent orphan-field
trace; the corrected rejection's old assertion; and a refused regression run
using the ordinary layout companion without its required policy root. The final
maintained command and correct policy regression supersede those attempts.
Compilation failures/refusals never substitute successful behavior.

No full criterion, DONE move or product percentage follows. Authoring no-closure
advances **9→10** once; workflow **9**, codegen **27**, renderer **3**, delivery
**1** remain. End local bounds/allocator/fixture expansion. Next connect physical
resizing and snapping through the public typed policy/input contract and existing
isolated paired operation; responsive variants and complete multi-page/reusable
authoring, parity, accessibility, performance and delivery retain their original
scope and blockers. Stop on inferred ownership, a second authoring engine,
weakened admission or editor identity/input loss.

Read-only native MCP remains connected at revision 8, home, retained rating-part
selection, no draft and unchanged Undo/Redo. All eight protected executable/start
identities and the live server artifact are verified unchanged at checkpoint.
No protected stop, listener launch, live deployment, reinstall or private
configuration refresh occurred. The earlier automatic approval rejection remains
“blocked by policy,” with no further reason. The phone retains its earlier
release. Final private process/session/artifact/remote proof belongs in
`.local/codex-restart-check/constraints-remote-proof.json`.

## Reusable resize grips — 2026-10-05

Original Studio authoring criterion 1 now consumes its size-bounds prerequisite
through public `nyx.designer.resize` contracts. Copied dimensions, closed axes,
fluent grid/keyboard/bounds policies and `TNyxResizeHandle` attach to ordinary
managed specialized buttons. Sequential public streams own matching primary
pointer capture, terminal release, foreign-pointer refusal, Escape/focus/capture
cancellation, Alt grid bypass and arrow/Shift keyboard steps. Zero delta leaves
off-grid dimensions exact; grid ties round upward before exact bounds clamp.
Borrowed UI-thread receivers detach before subscriptions are retired; no
model/widget/DOM/LCL handles or reference cycles enter the portable behavior.

Both renderer `SizeFor` methods return copied allocated outer geometry: native
full logical boxes survive clipping/scroll virtualization; browser integer
offset dimensions include borders and exclude transforms. Studio composes three
ordinary Nyx grips beside selected authored non-root controls. Start captures
the exact accepted pair, mounts, load, view, selection and creator epoch.
Preview updates dimensions in status only, without source regeneration/canvas
replacement. Release rechecks exact paired equality and publishes one existing
isolated command/Undo step. Busy commands, drafts, stale owners and retired
leases refuse. Recognized resize arrows remain consumed on host refusal.

`TNyxResizeChange` translates copied typed intent to the existing scalar patch
processor. Effective realized parent flow, including reusable slots, decides
whether the touched main-axis weight is cleared. The shared `NyxLayout` reader
now accepts an optional target scope and preserves primitive Row defaults.
Portable single-axis commands refuse target-flow divergence; explicit scoped
commands retain the portable dimensions. Studio's first portable grip path
refuses existing scoped sizing/bounds instead of silently erasing them. Existing
target Inspector fields remain available. Strict worker version 7 adds the
resize payload while preserving strict versions 5/6 compatibility for their
original action vocabularies. No new MCP tool or second authoring engine appears.

Actual native editing exposed a projection gap: scalar dimensions previously
forced full canvas replacement and lost an independent memo draft. The existing
fresh retained-projection admission now permits dimension/sizing/flex/bound
changes while retaining exact structure, identities, bindings, platform metadata,
creator context, custom-factory compatibility and rollback. Actual retained
controls qualify text/selection/focus and allocated geometry; incompatible
changes still request a full mount.

Maintained command:

```powershell
./tools/build.ps1 -Target resize -BrowserOutput build/resize/staged
```

Final relevant evidence in `build/resize/promotion.log`:

- **51** shared/semantic/worker/public-input checks on FPC 3.2.0 and matched
  FPC 3.3.1, with zero heap leaks. Bounded offline semantic inspection/mutation,
  atomic refusals, paired history, copied snapping/bounds, actual parent flow,
  old-wire migration, isolated publication and exact exports execute.
- **Seven** unchanged compiled Win32 companion checks, with zero leaks. Exact
  reconstructed design, allocated dimensions, retained real memo identity/text/
  selection/focus and invalid candidate retention execute. Six browser control
  checks compile but require actual host execution before claiming their result.
- **40** actual ordinary Win32 Studio checks, with zero leaks. Registered public
  pointer/key callbacks, isolated commit, paired Undo/Redo, Escape, no-op tap,
  bounds, Alt, arrow/Shift, stale-pair/draft refusal, handwritten source and
  independent memo/source identity execute. This qualifies callback behavior,
  not physical hardware, IME, assistive technology or another widgetset.
- Desktop and actual 390-pixel native captures `resize-desktop.png` and
  `resize-compact.png` are inspected. English grip/help text is visible. Existing
  narrow native chrome/sidebar overflow and complete presentation stay open.
- Original retained-projection/semantic-placement regressions pass **27/44**
  with zero leaks. Core passes **30/1777/55/60**, expected typed rejection,
  unchanged generated applications/Unicode/runtime bindings and three compiled
  collection checks. Both native compilers and pas2js reject string snapping
  with the intended `TNyxSizeSnap` diagnostic; an unrelated compiler failure
  cannot count. Browser consumers, Studio and recursive module worker
  compile; matched RTL and two English hosts are staged. Owned final compiles
  report zero warnings; installed pas2js `Classes` retains seven visible warnings
  per program without dependency edits or suppression.

Checked native builds use Delphi mode, assertions, range/overflow/I/O checks,
debug information and heap tracing; LCL uses the matched Win32 units. Exact
export and staged artifact hashes are retained privately with process/session/
remote proof. No listener, live-web deployment or private configuration refresh
occurs. Actual current browser/phone interaction remains unqualified at the
existing host gate; compiled hosts never substitute runtime pass markers.

Failed evidence is retained under `build/resize/`: incomplete input event
metadata, a harness notification that correctly made Studio busy, lost canvas
input from full replacement, an unavailable browser textarea declaration and
an incorrect primitive Row-default assumption. Corrected maintained evidence
supersedes each failure. The Row probe demonstrated that authored layout can be
absent while its effective primitive policy is row; adapters and sizing now read
the shared policy instead of guessing.

No original criterion, DONE move or full-product percentage follows. Authoring
no-closure advances **10→11** once; workflow **9**, codegen **27**, renderer **3**
and delivery **1** remain. End local grip/codec/fixture expansion. Next connect
direct canvas resize feedback and richer guides, then responsive authoring and
complete original multi-page/reusable editor, parity, accessibility, performance
and delivery. Status-only previews do not accept live canvas resizing/overlays.
Preserve exact pairs/input/ownership and stop on a second authoring engine,
weakened admission or identity/input loss.

Read-only native MCP remains connected at revision 8, home, retained rating-part
selection, no draft and unchanged Undo/Redo. The eight protected executable/start
identities and live server artifact remain unchanged. No protected stop, new
listener, release replacement, reinstall or private enrollment refresh occurred.
The earlier automatic approval rejection remains “blocked by policy,” with no
further reason. The phone retains its earlier observing release. Authorized
checkpoint proof is retained privately in
`.local/codex-restart-check/resize-remote-proof.json`.

## Reviewable release refresh — 2026-10-05

Owner/deliverable: the user's explicit running-version request and original
codegen criterion 3's source-workspace interaction. Stage the current service,
frontend, source worker, review/preview hosts and matched runtime independently;
qualify the changed real browser path, preserve the actual user pair, then refresh
the observing release only if the authorized service operation is allowed.
Stop on mismatched process/closure/pair or automatic approval rejection. The
preceding clean/pushed source checkpoint is `9a4f39ef85900dcc96a38341cc9b9633a9750225`.
No direct canvas-feedback implementation was made before this user steering.

The current checked FPC 3.2.0 Pascal server and semantic client compile into
`build/refresh-20261005/server/`. The actual candidate listener launched on its
own loopback ports with an isolated repository/configuration; protected services
were untouched. Its nineteen-tool inventory authenticates through the maintained
Pascal MCP client, and bounded `nyx_session` confirms the copied active selection,
view, permission and draft state. The candidate's exact source/design pair matches
the private backup byte for byte. That unchanged companion compiles and rebuilds
its design through `nyx_design_source_consumer`, with zero unfreed blocks.
Current Studio/module worker, review/preview and matched RTL bytes form a checked
staged web closure. Private compile logs, inventories, served hashes and process
identity are under the same ignored build directory. Native compilation has zero
owned warnings; installed browser RTL warnings remain visible.

The first actual browser journey failed because the shell's body remount detached
the independently owned modal. The public browser adapter now reconnects the
same dialog, closes its former top-layer state and calls `showModal` around the
retained descendants. It does not construct another editor. The maintained
`nyx_source_workspace_browser.lpr` now passes **30 desktop / 30 exact-390** real
DOM checks, including pending English text, exact textarea/range, independent
messages space, substantial expanded editor height, tab refresh while expanded,
explicit Close and cancellable owned return/focus. Both final screenshots are
inspected. Fixture declaration failures and the real failed modal capture are
retained, not overwritten by passing evidence. The installed RTL was not edited.
The existing source-editor build target now stages the browser fixture and host.

Relevant native source-workspace evidence (30 actual Win32 / 225 shared on each
native compiler) remains applicable: the fix changes only the browser modal
adapter. Current browser compilation and source-workspace execution do not
qualify full observing-editor interaction, isolated-worker admission, every
recent feature, project switching, physical phone keyboard/trusted Escape,
assistive technology, complete parity or large-project responsiveness.

The replacement command for the exact production process was rejected before
execution by automatic approval review, with the sole reason **blocked by
policy**. No equivalent replacement was attempted afterward. All eight protected
PID/executable/start identities still match; the actual pair remains byte-identical
at revision 8, selection `rating-2-part-4`, view `home`, no draft, Undo false and
Redo true. Direct native MCP session inspection confirms the preserved state;
only ordinary read activity advances. Production and the current chat retain
fifteen tools. The candidate retains nineteen tools and a separate copied project.
No production configuration or executable was replaced.

A private reviewable refresh is prepared at
`.local/codex-restart-check/refresh-20261005/refresh.ps1`. It verifies nine qualified
product artifacts by exact lengths/SHA-256, verifies process/start/listener
ownership, reads and backs up the current pair immediately before stopping,
refuses intervening edits, restores exact pair/selection/view and provides a
previous-executable rollback. It starts hidden, preserves LAN editor binding and
loopback MCP, and leaves auxiliary services alone. Its PowerShell AST parses
without errors; its release manifest and protected identity checks pass. The
script has **not executed**. Session Undo/Redo reset is explicitly reported for
an eventual restart; saved project files and source/design are retained. The
user must run the local script before deployment can be reported. Private backup,
configuration, manifest, post-rejection frame and unchanged-process evidence are
under that ignored directory; credentials are never printed or committed.

No full criterion closes. Codegen no-closure advances **27→28** once; workflow
**9**, renderer **3**, authoring **11** and delivery **1** remain unchanged.
Reassessment ends source/modal fixture expansion. Next finish the explicit
release request after local user execution and verify its served closure,
restored pair, authenticated tools and auxiliary identities. Then return to
direct canvas resize feedback/responsive authoring through public Nyx contracts
and the same isolated paired processor. No gate, history or partial browser result
narrows the original full-product acceptance criteria; no task moves to DONE.

## Canvas resize presentation — 2026-10-05

Owner/deliverable: original Studio authoring criterion 1, following public resize
grips while the user-local release refresh remains pending. Give an observing
designer direct canvas feedback without per-pixel admission, input replacement
or another Studio-only widget system. The preceding remote checkpoint is
`6662a4145239e055b6d25d8ecd375d61030ecf4d`.

`TNyxResizePreview` copies exact authored identity and proposed logical outer
dimensions; default explicitly clears it. No document, widget or receiver is
owned by that record. Both public canvas adapters accept only the selected
authored face in design mode. The existing gesture bridge sends copied
presentation after shell refresh and a fresh lease check, then clears before
commit/cancel/disconnection. Release still submits the existing isolated paired
operation exactly once; accepted allocation and source remain unchanged during
preview. No persistence/MCP schema or additional mutation path was introduced.

Browser paint uses four fixed, pointer-transparent, aria-hidden accent strips,
captured scroll/resize listeners and bounded host/viewport intersections.
Axis-aligned scaling and copied CSS decimal settings preserve logical geometry
without changing application locale. Native paint reuses four disabled standard
LCL panels, independently owned/reparented above descendant windows. Fresh
selection/viewport layout resolves the outer face; maximum logical proposals
never allocate enormous physical widgets. Retirement releases owned browser
listeners/paint and native windows with no reference cycle into the tree.

Actual semantic composition uses the separately staged nineteen-tool service.
An explicitly owned project receives six related title/page/control operations
as one revision-aware transaction; protected primary selection/pair are retained.
An initial unpublished-property spelling refused at the unchanged revision;
bounded catalog metadata supplied the existing typed wire name. Two bounded
source windows at revision 2 export 99 exact accepted lines, compiled unchanged
for the actual browser preview consumer. One semantic Undo returns the owned
workspace to empty at revision 3; Redo restores its accepted project and both
exact source windows at revision 4. Private workspace IDs/payloads are kept only
under `build/resize-feedback/`; no user project was replaced or root deleted.

Final maintained `./tools/build.ps1 -Target resize -BrowserOutput
build/resize-feedback/web-final` runs **54 shared checks per native compiler**,
compares exact exported design/source/pair bytes, runs **seven unchanged
compiled Win32 controls**, then **59 actual Studio checks**, with zero unfreed
blocks. Current browser consumers/Studio/module worker compile. A final native
identity-refusal guard and independently anchored geometry assertion also pass
59, followed by **27 actual retained-projection regressions**, with zero leaks.
Owned compilation has zero warnings; seven installed browser RTL warnings remain
visible and dependency source is unchanged. Logs are retained in the same output.

The dedicated browser consumer passes **30 desktop / 30 exact-390** actual DOM
checks: retained English textarea/range/focus, unchanged accepted dimensions,
selection refusal, scroll tracking, locale-safe fractional geometry, legal
maximum clipping and cancel/selection/unmount retirement. Its English captures
are inspected. `resize-preview.html` and its Pascal program are maintained
products; only their unique fixture files were added to the existing candidate
host. The nine qualified release artifacts and user-local refresh manifest are
unchanged. Candidate ordinary Studio remains the preceding qualified editor;
this standalone adapter journey does not accept current Studio grip input.

Native captures now compose actual standard-panel `PaintTo` over the form's
offscreen paint, with accent pixels checked on all four proposed edges. Win32
form painting includes its frame/caption: composing at client origin displaced
the first image, so the final capture uses the exact window origin. Explicitly
empty panel captions avoid LCL's automatic Name-as-Caption painting. Corrected
desktop/proposal and compact captures are inspected. Failed declaration,
illegal-dimension, print-order/caption and coordinate captures are retained;
the real desktop DC produced a black image and remains unavailable evidence.
Do not describe composed offscreen paint as physical desktop rendering.

No original criterion closes. Authoring no-closure advances **11→12** once;
workflow **9**, codegen **28**, renderer **3** and delivery **1** remain. End
paint/capture/fixture expansion. Next connect direct canvas edge handles and
responsive authoring through public Nyx contracts and the same isolated paired
processor. Richer guides, ordinary browser Studio input, observing phone refresh,
hardware/IME/assistive technology, full native presentation, accessibility,
performance and delivery retain their original owners/acceptance gates. Preserve
the pending user-local refresh and verify its closure/pair/processes after user
execution; no equivalent rejected service replacement was attempted here.

## Direct canvas resize handles — 2026-10-05

Owner/deliverable: original Studio authoring criterion 1, following the public
proposal-paint checkpoint `dbf27b320b0034b5d91d6e5907f741fbe4043c8b`. Move sizing
onto the selected face while retaining Inspector alternatives and the same
single paired command. This independent work leaves the pending user-local
release refresh intact; it does not bypass its rejected service operation.

Public `INyxCanvasResizeGrips` owns a small independent document with three
specialized Nyx buttons. Both adapters retain its interface and mount those
controls in separate runtime scopes, never enabling application callbacks in
the edited tree. Width/height/corner faces are 44 logical/viewport pixels,
bounded by the visible canvas; small faces hide overlapping one-axis handles.
Copied preview dimensions move the handles while accepted allocation, source,
independent input and tree ownership remain unchanged. Studio borrows its
existing guarded capture/feedback receivers; release still enters its isolated
paired admission exactly once.

Public `TNyxResizePoint` and optional `TNyxResizePointerMap` normalize local
samples into a stable logical plane. Each sample maps once before computing its
delta, so moving the handle cannot change the next sample's origin. Native uses
actual button screen origin; browser uses viewport origin and axis-aligned
scale. Undefined/non-finite positions refuse before pointer capture. Existing
stationary clients retain local mapping. Managed target unbind silently retires
subscriptions and invokes a restricted lease-retirement receiver; it revokes
Studio's shared lease without painting/reentering mounting. Permanent Disconnect
retires borrowed editor receivers. Adapters keep the document alive through
target teardown and reparent their native hosts with the retained canvas.

Final maintained `./tools/build.ps1 -Target resize -BrowserOutput
build/canvas-resize/staged` passes **56 shared checks per native compiler**,
compares exact exported design/source/pair bytes, exercises **seven unchanged
compiled controls** and **70 actual Win32 Studio checks**, with zero unfreed
blocks. The direct-canvas journey drives two screen-position deltas while the
same actual button moves, releases one paired edit, retains uncommitted English
memo/range and restores the exact pair with one Undo. Captured-scope retirement
permits a fresh keyboard operation without publishing abandoned input.
Relevant retained projection regression passes **27**, with zero leaks.
Current browser consumers/Studio/module worker compile; owned compilation has
zero warnings, while installed RTL warnings remain visible and unchanged.

The actual nineteen-tool semantic client confirms the owned review remains at
revision 4, with its one accepted transaction and no draft. The browser consumer
compiles its unchanged 99-line source exported in two bounded MCP windows.
Actual browser adapter checks pass **47 desktop / 47 exact-390**: specialized
44-pixel buttons/English accessible names, retained DOM/scopes, actual precision
keyboard listeners, one exact copied proposal, old-DOM refusal after retirement,
fresh remount, previous clipping/locale/input/paint guards and selection/unmount
cleanup. Its precision policy explicitly selects `nssUnsnapped`; Studio's grid
policy is qualified by the native path. A recorded callback proposal does not
prove browser worker/source admission. Final English captures on both targets
are inspected; native captures compose actual control painting, not desktop DC.

Retained failures explain revised fixture assumptions: canvas hosts correctly
sit above inert outline strips, midpoint pixels can be covered by handles,
native pointer-up must be supplied in its newly moved local coordinates, and
an off-grid keyboard proposal honors its configured snapping policy. Those
artifacts/logs remain under `build/canvas-resize/`. Only uniquely named review
HTML/JavaScript files were added to the already admitted candidate host. Its
ordinary Studio index and the nine qualified release artifacts are untouched.

No original criterion closes. Authoring no-closure advances **12→13** once;
workflow **9**, codegen **28**, renderer **3** and delivery **1** remain. End
grip/mapping/fixture expansion. Next implement responsive authoring through
public typed configuration and the same paired processor, then complete richer
guides, original editor/parity/accessibility/performance and delivery. Trusted
browser pointer/capture, ordinary browser Studio worker admission, physical
phone/IME/assistive technology, wider widgetsets and broad native presentation
retain their acceptance gates. Protected services/pairs/configuration and the
pending local refresh remain intact; verify that deployment after user execution.

## Typed responsive authoring — 2026-10-05

Original owner: [Studio authoring criterion 1](TODO/NS-4_studio-authoring_01.md).
Public `TNyxViewportWidth` supplies immutable half-open logical-width conditions.
Independent managed/base configuration scopes preserve both target and interval;
generated specialized Pascal and the closed source reader use `WhenViewport`.
Reserved wire keys remain an explicit persistence/semantic boundary. Schema
metadata retains integer/Boolean/enum types and readable interval intent.
Every piecewise effective size/split interval is checked on both concrete targets
before admission, including overlapping rules. See [contract](docs/responsive.md).

Realized presentation overlays preserve authored properties and current live
defaults. Browser ResizeObserver and ordinary LCL resizing refresh existing
controls. Compatible first/last-rule edits retain the same input; browser
observation starts/retires with those edits. Effective column CSS overrides the
primitive row's centering. The Nyx-built Inspector creates layout rules through
the existing independent paired processor; rule fields remain typed/editable.
No second editor toolkit, compiler directives in application authoring or
per-resize source/history mutation is introduced.

Maintained `tools/build.ps1 -Target responsive -ResponsiveSourceDirectory
build/responsive/mcp-source -BrowserOutput build/responsive/staged` passes:

- Shared contract/semantic/paired checks: **33** on stable FPC 3.2.0 and matched
  FPC 3.3.1, with identical generated exports and zero checked leaks.
- Unchanged 97-line MCP companion: **22** actual Win32 controls and **9** ordinary
  native Studio Inspector/Undo checks, with zero checked leaks. Input identity,
  independent English text/range/focus, exclusive bounds, concrete-target gap,
  host replacement and unchanged persistence are qualified.
- Browser contract: **33** executed through the matched RTL and the Pascal
  capture helper on ordinary clocks. Actual browser controls: **23** desktop /
  **23** exact-390 iframe checks through the maintained Pascal CDP observer,
  with zero checked driver leaks. It observes bounded fixture markers and
  captures actual rendering without script evaluation or design automation.
  This includes first/last-rule refresh and actual observer delivery.
- Focused retained projection: **27** actual native checks, zero leaks. Core
  regression passes **30**, composition/designer **1777**, scheduler **55**,
  paired project/disk recovery **60**, compiled collection **3** and **65**
  intended wrong-type compiler refusals. Generated reconstruction also executes.
  Studio and its matched module worker compile; owned warnings remain zero.
  The installed pas2js RTL's seven incomplete-case warnings remain visible.

An independently owned loopback MCP project composes the English maintained
operation fixture in one five-operation transaction. The initially wrong title
field is refused at revision 1 without publication. Bounded source windows
1..80/81..97 export revision 2, consumed unchanged by actual target controls.
`nyx_build` browser and LCL jobs both succeed with matching source/design/output
fingerprints and artifact manifests. One grouped Undo empties the owned project
at revision 3; Redo at revision 4 restores exact generated source. Windows are
joined with canonical LF and the generator's terminal LF before byte comparison.
No borrowed active user design is replaced. The original first build request
printed its receipt, then one-shot client session DELETE reported a socket error;
querying that exact job confirmed success, and later calls closed normally.
This retained transport-close gap belongs to the existing workflow owner.

Evidence lives under ignored `build/responsive/`: `qualification-current.log`,
`core-regression.log`, `projection/`, `browser-contracts/`, `browser-current/`,
`browser-390-current/`, actual native painted captures and semantic job/source/
history logs. Native painting is composed through actual Form.PaintTo, not a
physical desktop capture. Captures contain English text and were inspected.
Retained failures include initial source-reader integer decoding, an unsupported
installed textarea method, accelerated-clock observer assumptions and a mistaken
fixture method spelling. Final tests use the public TryRefresh contract and
ordinary browser frames. An owned capture helper's uninitialized byte buffer
warning is corrected without suppressing diagnostics or editing dependencies.

The independent current preview stages only its own browser closure into its
already running loopback service. The pending user-local release's nine hashes
stay unchanged; protected process identities and revision-8 user state remain.
The LAN release is still old, and its replacement is still blocked by the prior
automatic review. Its reviewed local refresh script still parses and awaits
user execution. No equivalent LAN launch or phone deployment is claimed.

No original criterion closes. Authoring no-closure advances **13→14** once;
workflow **9**, codegen **28**, renderer **3** and delivery **1** remain. End local
interval/fixture expansion. Next qualify ordinary browser Studio Inspector/worker
execution and continue richer responsive variants and original guides, editor/
parity/accessibility/performance/delivery. Physical phone, hardware/IME/assistive
technology, other widgetsets, nested container conditions, named variants and
large-project performance remain open. Preserve all original task blockers.
The post-push exact local/remote checkpoint receipt is retained under ignored
`build/responsive/remote-checkpoint.json`; verify `origin/hello-nyx` before handoff.

## Current LAN release refresh — 2026-10-05

The user explicitly requested the updated LAN service after the earlier generic
automatic-review refusal. This execution was permitted. A fresh nine-artifact
product closure includes the current Pascal server, Studio, matched module worker,
review/preview consumers, RTL and hosts. The current source-editor browser
regression passes **30 desktop / 30 exact-390**; the current candidate independently
admits the exact active paired source/design/selection/view. Owned server warnings
remain zero; dependency RTL warnings stay visible. Responsive evidence and its
original acceptance limits remain in the packet above.

The guarded refresh verifies hashes, executable bytes and exact process creation
before stopping the owned service. It backs up the executable and paired editor
frame, retains the existing installed executable path and its working firewall
allowance, and starts the current build hidden with its explicit current web root.
The first attempt hit a Windows executable lock; guarded rollback restored the
exact pair and old service. Waiting for the verified process to exit and disposing
its process handle before copying resolved that race. The subsequent refresh
completed, preserving the pair/selection/view. In-memory history resets on restart;
the current primary session is revision **2**, with no pending draft.

Actual checks verify all-interface HTTP, loopback-only MCP, one new exact process
owning both existing ports, HTTP 200 through this machine's LAN address, and exact
installed server/served Studio/served worker hashes. The native Pascal semantic
client authenticates **nineteen** current tools and reads the primary session.
All seven auxiliary protected services retain their previous exact identities.
This establishes deployment and local LAN-address delivery; physical phone review
and ordinary browser Studio responsive Inspector/worker qualification still need
their own evidence. No original criterion closes from deployment alone.

Private evidence lives under ignored `build/lan-refresh-current/` and
`.local/codex-restart-check/lan-refresh-current/`: nine-artifact manifest, current
process identity, exact paired before/after frames, executable backup/rollback,
served-byte verification, actual MCP discovery/session and desktop/narrow editor
captures. The earlier staged closure stays intact, but its old refresh script's
expected process has retired; it is superseded and must not be reused. Continue
semantic MCP through the current enrollment, never infer that old cached chat
handles have reconnected merely because the configuration rotated.

## Ordinary responsive Studio — 2026-10-06

Owner: original Studio authoring criterion 1, with the user's additional
form-factor steering. Deliverable: consume the existing strongly typed viewport
contract through ordinary browser Studio Inspector/worker/shared history, and use
that same public contract to reclaim narrow editor space. Stop on lost independent
input, guessed source ownership or bypassed paired admission; no alternate browser
design-authoring path or replacement of the primary project is permitted.

The first actual browser journey exposed lost canvas identity during synchronized
Undo. `HandleShell` retired the canvas before the remote response, and
`AgentRefresh` unconditionally replaced it after admitting the new pair. Both
paths now request guarded public projection refresh; incompatible structural/view
changes retain the normal render fallback. Compact Project/Inspector chrome also
keeps the last detached browser host reachable and reconciles compatible updates
there, instead of destroying the canvas. Returning to Design reuses its controls,
listeners, independent text and selection. Keyboard history and editor/preview
presentation switches share the same retention policy. Native Studio already
uses its independently owned parking host and guarded refresh.

Studio's shared Nyx compositions now use `WhenViewport` for shorter source action
and message captions, redundant status/Agents visibility, and the placement
select's compact width/caption with an explicit accessible name. Conflict choices
precede transport/permission information, making shared-project recovery reachable
on a phone. Documentation gives fluent position, dimension, visibility and layout
examples alongside target overrides; no compiler directives or raw property keys
are required for this authoring. The condition is still host-width based;
height/orientation, nested container and named-variant work remain open.

Semantic MCP created an independent English project and composed the maintained
five-operation fixture in one expected-revision transaction. Bounded source
windows retained specialized interfaces and typed rules. The ordinary browser
test addresses that exact workspace through `ConnectAgents`, drives the actual
Nyx Inspector, waits for the real module worker, and uses ordinary shared Undo/
Redo. A subsequent bounded `nyx_node` query confirms both exact layout properties
and both-target capability metadata at revision 22. Actual `nyx_build` application
jobs succeed on browser and LCL with matching source/design fingerprints; the
accepted companion is 2,221 bytes. Native warnings are zero; browser warnings are
the seven unchanged installed RTL case warnings, with no owned-source warnings.

Evidence under ignored `build/responsive/studio-review/`:

- `qualification.log`: maintained shared checks 33 per native compiler, actual
  native controls 22 and Studio nine, then staged browser Studio/worker/consumers.
- `native-studio-current.log` and `native-source-current.log`: final actual Win32
  responsive Studio nine and source workspace 30, with zero checked leaks.
- `desktop-final/` and `compact-final/`: 22 ordinary Studio checks each, captured
  on ordinary browser frames; same input/draft/range and source editor through
  worker publication, one Undo and one Redo. The compact host is exactly 390 px.
- `source-desktop-final/` and `source-compact-final/`: existing source workspace/
  modal regression 30 each, including expanded Close/cancel and retained draft.
- Semantic creation/transaction, paged source, history, bounded metadata and
  compiler receipts/status; failed first desktop and retained-history attempts
  remain alongside terminal successful evidence. Input callbacks are synthetic;
  these checks do not qualify trusted hardware, IME or assistive technology.

The qualified ordinary Studio JS is now served by the existing LAN service.
Process executable/creation identity and prior installed hash were checked before
copying; backup plus new installed/served SHA-256 receipt live in the ignored
current refresh record as `responsive-ui-20261006.json`. The worker is byte-identical
to the fresh compilation and remains in place. No service restart or enrollment
rotation was needed. The primary project's revision 2, selection, view, draft and
history flags remain unchanged. The original nine-artifact server manifest remains
historical evidence; this new UI receipt owns the later asset revision. The first
guard attempt refused because PowerShell auto-decoded the JSON timestamp, then an
implicit string conversion discarded its fractional seconds. Direct typed UTC
comparison verified exact identity before the successful update. No process was
stopped and no service change was inferred from that diagnostic.

No original criterion closes. Authoring no-closure advances **14→15** once;
workflow 9, codegen 28, renderer 3 and delivery 1 remain. The previous ordinary
browser consumer gate is now qualified. End local width/fixture expansion and
continue original responsive conditions, guides, editor quality, full parity/
accessibility and delivery. Physical phone review of these latest assets remains
separate from local LAN delivery and viewport emulation.

## Alignment guides — 2026-10-06

Outcome: the public Pascal `nyx.designer.guides` copied geometry/context/guide
contract now feeds `NyxResizePolicy.Guides`. Nearest eligible sibling dimensions
win before the grid, with deterministic equal-size/edge/center/order ties.
Bounds filter candidates first; Alt bypasses all snapping; keyboard steps bypass
guides. A no-op stays exact. Peer arrays copy independently on FPC/pas2js and
retain no model/widget. Captures contain at most 256 visible immediate peers;
absolute layouts admit positional guides, flow layouts only matching sizes.

Both adapters expose `AlignmentFor`, use current allocation/client geometry and
paint bounded inert guide strips beside existing outlines. Equal sizes paint
separate honest measurement bars rather than implying collinearity. Native
conversion clips in the wide domain before allocating physical windows; public
offscreen capture includes guide panels. Studio captures once and checks copied
geometry at release, then submits one existing isolated paired edit. Changed
neighbors cancel. The compact consumer exposed missing canvas handles when its
Inspector was absent; selection now owns that independent adornment. Short
resize captions and a view-label minimum width avoid the observed wrapping.
Conditional sizing/parent flow refuses a portable baseline gesture; selecting
the intended responsive presentation on the canvas remains open.

Semantic MCP: one independent English project is composed through the maintained
`tests/alignment-review.operations.json` group. Four typed node properties and
two bounded 80-line windows at one revision provide context. The 109-line,
2391-byte accepted source has MD5 `96ce1d371b30fb4781c754090da36817`, verified
against both immutable application jobs before unchanged compilation. Bounded
lines omit terminal framing; the final LF was verified against job bytes/hash,
and this gap is recorded with the existing workflow owner. Browser physical
review uses the explicit project, then semantic Undo restores its base dimensions
and retains independent history. Primary revision/selection/view never change.

Qualification: `tools/build.ps1 -Target guides -GuideSourceDirectory
build/alignment/mcp-source` passes 74 shared checks on stable FPC 3.2.0 and matched
3.3.1; executed browser shared checks also pass 74. The exact companion/native
Studio journey passes 24 actual Win32 checks, including painted accent pixels,
compact grips, Alt, stale layout refusal, retained memo/text/range and exact
paired Undo/Redo, with zero unfreed blocks. Actual browser Studio passes 27
desktop / 27 exact-390 checks through real pointer capture, canvas handles,
module-worker publication and synchronized Undo/Redo. English captures are under
`build/alignment/native/guides-native.png` and `build/alignment/review/`.
Both real MCP application jobs succeed; native has zero warnings, browser retains
seven installed RTL warnings. Final owned source has no compiler warnings.
Core/negative typed-argument/generated collection regression passes. Earlier
failed build/input/framing attempts remain in ignored evidence; final exact
source and current receipts supersede those attempts.

Deployment: current Studio JavaScript is atomically replaced at the already
running LAN web root after process/executable/creation-time and listener checks.
The worker is byte-identical. Loopback/LAN hashes match the qualified asset;
primary pair/revision/selection/view/history and server identity remain exact.
Private `.local/alignment-assets-20261006/` owns the current overlay manifest,
previous asset and before/after pair receipts. The full-release server manifest
still identifies the server; its Studio artifact entry is superseded by this
overlay. The branch checkpoint/remote verification is recorded in ignored
`build/alignment/review/remote-checkpoint.json` after push.

Goal-turn classification: **progress**, with authoritative product, both-target
input/paint/publication, semantic build/history, deployment and remote checkpoint.
No original criterion/prerequisite/DONE closes. Authoring no-closure advances
16→17 once; workflow 9, codegen 28, renderer 3 and delivery 1 remain. Stop local
guide/fixture expansion; continue original named/container presentations, moving
control snapping, complete editor/parity/accessibility/performance/delivery.
Nested scroller/virtual/rotation guide geometry, physical phone input, IME/AT,
another widgetset and large-project performance remain unqualified.

## Responsive host conditions — 2026-10-06

Owner: original criterion 1 in [Studio authoring](TODO/NS-4_studio-authoring_01.md)
and the user's screen-size/presentation request. The deliverable is a usable
public condition carried through normal authoring, admission, Inspector and both
actual adapters. Stop on lost input, incorrect conditional admission or guessed
viewport behavior; full WYSIWYG/guides/parity acceptance remains open.

- `TNyxViewportCondition` is an immutable copied width/height/orientation value.
  Integer half-open bounds combine fluently with a closed orientation enum;
  positive squares and zero-host behavior are explicit. Existing width keys and
  generated source remain exact. Managed facades retain separate scopes and
  ownership; the maintained Pascal generator owns their refreshed includes.
- The source reader admits generated typed chains and refuses a layout enum
  supplied as orientation. Reserved keys are canonical; effective constraints
  are checked over relevant rectangular intervals and every feasible orientation
  region on both targets, including square-only interior conflicts. Failed
  admission leaves paired source/design/revision unchanged. This is not a
  large-project performance qualification.
- Browser observation now reacts to height-only changes. Native rules use the
  borrowed host's stable client rectangle, matching the browser contract instead
  of its internal scrolling panel. Compatible retained projection recognizes
  both namespaces. The ordinary Nyx-built Inspector adds height bounds and
  orientation while keeping one intent, worker admission and paired Undo.
- Final shared evidence: **61** on stable FPC 3.2.0, matched FPC 3.3.1 and executed
  pas2js browser. Actual unchanged semantic source passes **30** Win32 controls,
  **31** desktop / **31** exact-390 browser controls, including height-only resize,
  the exclusive 300-pixel host boundary, unchanged persistence and live memo
  identity/text/range/focus. Native ordinary combined Inspector/Undo passes **9**;
  browser ordinary combined Inspector/worker/Undo/Redo passes **22** at each size.
  Native checked consumers and the Pascal browser observer report zero leaks.
- The maintained `responsive` build stages consumers, Studio and its module
  worker. Final native runs after the host-boundary change are separately logged.
  Core qualification and typed compiler refusal fixtures pass. Final owned source
  emits no build warnings; the seven installed pas2js RTL case warnings remain
  visible without altering or suppressing dependency source.
- Failed evidence is retained under `build/form-factors/`: an unattended native
  exception dialog hid an early assertion until the test disabled dialog capture;
  first-rule refresh then revealed the missing new-namespace compatibility.
  Another gap assertion overlapped a native-specific width rule / later common
  property and was corrected to isolate the intended height transition and exact
  persisted order. Full native Studio execution subsequently completed; its slow
  run did not establish a resize loop. The stable borrowed-host change is separately
  exercised by the exact boundary check.
- Semantic MCP authored the English **Responsive form factors** project as one
  five-operation transaction in an isolated service, then on the refreshed actual
  LAN service. Bounded 80-line windows reconstruct identical **100-line** Pascal;
  actual browser/LCL application jobs both succeed with source fingerprint
  `5c1bbbb357f976b270ab2b076ca76301`. Grouped Undo empties the demo and one Redo
  restores exact source. Bounded node metadata exposes typed scope values. The
  actual demo ends at revision **6**, view `home`, selection `workspace`, pending
  draft false; the stable primary remains separately preserved.
- Full release refresh verifies the exact previous process/executable, backs up
  the active pair, changes only that service and preserves its firewall-covered
  executable path. Nine staged artifacts include server, Studio, worker, review,
  preview and matched RTL/hosts. Installed/served hashes match on loopback and
  LAN; HTTP binds all interfaces, MCP stays loopback, **19** tools authenticate.
  The primary pair, `rating-2-part-4` selection and `home` view stay exact at
  revision 2. In-memory history resets during restart. Seven protected auxiliary
  services and two older isolated services retain their exact identities; this
  packet's additional isolated service remains available for bounded review.
  Current private deployment state is `.local/form-factors-refresh-20261006/`.

Evidence: `build/form-factors/qualification-current.log`, `core.log`,
`shared-stable/`, `shared-matched/`, `nyx_responsive_controls.result.log`,
`nyx_responsive_studio.result.log`, `review/contracts-current/`,
`review/controls-desktop/`, `review/controls-compact/`, `review/studio-desktop/`,
`review/studio-compact/`, `review/actual-*.receipt.json`, `release/`, and the
private deployment record. Inspected actual desktop, exact-390 and native
captures; synthetic callbacks / fixed hosts do not establish phone keyboard,
visual viewport, hardware, IME, assistive technology or another widgetset.

No original criterion closes. Authoring no-closure **15→16** once; workflow **9**,
codegen **28**, renderer **3**, delivery **1** remain. End local condition/parser/
fixture expansion. Continue named/container presentations, snapping/guides and
complete editor/parity/accessibility/performance/delivery. The observed desktop
view-bar title wrap with the Inspector open remains an ordinary-layout quality
gap. The branch checkpoint is verified after committing, with its private receipt
at `build/form-factors/review/remote-checkpoint.json`.

## Named responsive presentations — 2026-10-06

Original [authoring criterion 1](TODO/NS-4_studio-authoring_01.md) now shares one
typed named host condition across controls and reusable views. This is integrated
progress toward the full outcome, with the original prerequisites still open.

- `TNyxPresentationRef`, `INyxPresentations` and immutable
  `INyxPresentationSnapshot` keep exact open names, copied conditions and explicit
  ownership. Managed controls expose `WhenPresentation`; target scope remains
  orthogonal. Anonymous/named overlap retains original property order and concrete
  target priority. Constraints resolve both targets through the same definitions.
- Version-four document definitions and ordered node `presentationRules` arrays
  preserve full 128-scalar supplementary names. Qualification found old native
  fpjson's 255-byte object-key truncation; names now travel as array values with
  original indices. Old opaque fields retain meaning; conflicting promotion and
  dangling references refuse. Generation/source admission use crafted typed calls.
- Ordinary Nyx Inspector defines/updates shared predicates, adds a supported
  attribute override and resets one exact override. Leaf controls participate.
  Private worker version eight retains exact owner/typed intent; older tickets
  remain admitted with their original shape. Paired source/history and retained
  input identities/text/ranges survive ordinary worker/Undo/Redo execution.
- Authenticated discovery exposes **20** tools. `nyx_presentations` returns one
  exact definition or bounded pages. Existing `nyx_transaction` groups definition,
  scalar set/use/reset/remove operations. `presentation-set` avoids long JSON keys
  and refuses wrong scalar families atomically. The maintained English operations
  fixture composes an independent project semantically, with **104 lines / 2438
  bytes**, MD5 `fddc3a207f28b0ae82e673c0c3b24c41`, matching actual immutable
  browser/LCL application jobs. `nyx_source` still omits final LF; exact framing
  was checked against the immutable fingerprint/length before physical compilation.
- **55** shared checks pass on stable FPC, LCL-matched FPC and browser; **2** actual
  compiled full-length Unicode checks pass per target; **36** actual Win32 and
  **37** browser control checks include central definition refresh without input
  loss. Ordinary Studio passes **16** Win32 and **39** desktop / **39** exact-390
  checks through actual callbacks/module worker/synchronized history. Compact
  harness navigation now returns to Design before reading its detached code pane.
  Captures were inspected. No owned warning; seven known upstream pas2js RTL
  warnings remain without dependency edits. Checked native runs report zero leaks.
- Core regression passes; resize passes **74**; discovery regressions pass **57**
  collection, **14** state, **28** reusable, **50** declaration, **37** import and
  **37** routine checks. Three protected project pairs/selection/view states were
  backed up privately and admitted exactly against the candidate. Additional
  projects require their ordinary private commit after MCP creation; claiming an
  already seeded workspace deliberately preserves its existing pair.

Evidence: `build/presentations/final-qualification.log`, `core-regression.log`,
`regression/`, `stable/`, `maintained-matched/`, `review/contracts-final/`,
`review/compiled-names/`, `review/controls-final/`, `review/studio-desktop-final/`,
`review/studio-compact-final/`, `semantic/*receipt.json`, `mcp-source/`, and staged
`release/`. Private backup/preservation proof is
`.local/presentations-refresh-20261006/`. Full LAN deployment verifies installed
server bytes and sixteen loopback/LAN web-asset hashes against the nine-artifact
manifest from source commit `176ad0cc5dd66dca2e1cee482bbc400cb422c042`.
The firewall-covered executable path is preserved; HTTP binds all interfaces,
MCP loopback. Twenty tools authenticate. Three exact pairs/selection/view states
are restored; prior histories reset, and concurrent projects have new handles
and an import Undo step. Eleven auxiliary services keep their exact identities.
The deployed independent **Shared presentations** project is semantically
composed; one grouped Undo empties it and one Redo restores the exact 104-line
source. Both deployed application jobs succeed with the same source fingerprint,
no owned warnings and current source/output. The demo ends at revision 5,
selection `workspace`, view `home`, with no pending draft. A bounded 390×700
semantic preview descriptor is available; physical behavior was qualified by the
maintained ordinary input harnesses. The stable primary remains at revision 2.

The implementation checkpoint is pushed and exact remote HEAD verified. Final
handoff/checkpoint evidence is `build/presentations/review/remote-checkpoint.json`.

Discovered transport gap belongs to the existing
[workflow owner](TODO/NS-4_agent-workflows_01.md): the Pascal CLI receives its
build receipt, then reports a DELETE-session cleanup socket error. The mutation
was not blindly replayed; exact handles independently resolve successful jobs.
Active-chat native MCP handles still cache the previous endpoint; the Pascal
semantic client remains primary. Inspector form choices/drafts and restart
workspace/history identity remain ordinary editor/service lifecycle work.

No original criterion closes. Authoring no-closure **17→18** once; workflow **9**,
codegen **28**, renderer **3**, delivery **1** remain. Stop local named-rule/parser/
fixture expansion. Next continue original container/manual/structural variants,
full move snapping and complete editor/parity/accessibility/performance/delivery.
Synthetic host input does not establish physical phone keyboard, visual viewport,
hardware, IME, assistive technology, nested scrolling or another widgetset.

## Manual presentation selection — 2026-10-06

Original owner: [authoring criterion 1](TODO/NS-4_studio-authoring_01.md).
The bounded container investigation stopped at its geometry gate: native natural
measurement depends on descendants, while browser candidates mount detached.
Stable allocation/containment and per-instance conditions remain open. The
materially different integrated deliverable is exclusive manual configuration.

- `TNyxPresentationCondition` distinguishes automatic host predicates from manual
  definitions. `TNyxPresentationSelection` is a copied exact application reference.
  `INyxPresentationView` exposes fluent Select/Automatic on both adapters; its
  borrowed receivers retire before unmount frees a view. Different views own
  independent choices. Compatible refresh retains a valid manual choice or clears
  one deliberately removed/replaced by automatic activation.
- Matching order is common automatic, selected common manual, concrete automatic,
  selected concrete manual; authored property order remains stable within each
  group. Selection changes effective overlays only. Constraint admission checks
  every automatic region and exclusive manual choice on both targets, refusing
  more than 65,536 bounded partitions. Automatic-only nested wire remains version
  one; manual-containing registries use strict version two. Outer documents remain
  version four. Manual entries refuse hidden bounds/orientation predicates.
- Ordinary Nyx Inspector defines either activation. The view bar exposes manual
  preview choices when available; it retains canvas controls/text/ranges and writes
  no design/Pascal/history. Pending resize/placement proposals cancel before a
  preview change. Per-project preference version four adds a nullable exact name
  and admits strict versions two/three without discarding existing fields.
- Semantic MCP composes the maintained English fixture as one paired transaction.
  Bounded definition/source queries, grouped Undo/Redo, actual browser/LCL builds
  and immutable selected previews use explicit workspace/revision context.
  Automatic/unknown names, stale previews and mixed manual predicates refuse
  without changing the document/history. Exact source is **119 lines / 2908 bytes**,
  MD5 `09084a629502e003091f4717dbbd1497`; design fingerprint is
  `dee250690061647555816c7d4b4bab75`. The source-window terminal LF is restored and
  checked against immutable job fingerprints before actual consumer compilation.
- **72** shared checks pass on stable FPC, LCL-matched FPC and browser; **2** actual
  compiled Unicode checks pass per target. Unchanged MCP source passes **52**
  Win32 / **53** per browser-size control checks, **26** actual native Studio and
  **64** desktop / **64** exact-390 ordinary Studio checks through the module
  worker and synchronized Undo. Preference/workspace checks pass **237** native
  and browser. Resize regression passes **74**. Checked native runs report zero
  leaks. No owned warning; seven known upstream pas2js RTL warnings remain.
- Browser qualification found detached candidate Sync incorrectly replacing the
  measured final-host rectangle with zero-sized staging geometry. Candidate Sync
  now retains the final-host measurement; mounted Sync reads live geometry. The
  first/second actual views apply correct initial automatic defaults. The longer
  twelve-stage Studio journey passed just beyond the old 30-second host deadline;
  its bounded deadline is now 60 seconds. Failed captures/progress markers retain
  that reporting evidence without replacing the real-frame journey.

Evidence: `build/manual-presentations/qualification.log`, `stable/`,
`maintained-matched/`, `mcp-source/`, `mcp-lcl/`, `browser-contracts-admitted/`,
`browser-names/`, `browser-controls-fixed/`, `browser-controls-390/`,
`browser-studio-qualified/`, `browser-studio-390/`, `preferences/`,
`browser-preferences-final/`, `resize-regression/`, semantic receipts and `release/`.
Desktop/narrow and selected semantic preview captures were inspected.

The exact nine-artifact closure is deployed through the existing firewall-covered
executable with HTTP on all interfaces and MCP loopback. Sixteen served web hashes
match the release manifest. Candidate and deployed restoration retain all four
existing paired documents, selections and views exactly. Restart resets their
histories; concurrent projects receive new handles and one import Undo. Twelve
auxiliary services retain exact PID/executable/creation identities. Current private
identity/backup/manifest/mapping/rollback lives in
`.local/manual-presentations-refresh-20261006/`; previous refresh scripts are
obsolete. The independent **Manual presentations** demo is composed through the
deployed MCP server. Grouped Undo/Redo and both immutable application jobs succeed;
selected focused/wide PNG previews render real different layouts at revision 4.
The stable primary remains exact at revision 2. Twenty tools authenticate through
the Pascal semantic client; this chat's native named handles still cache an
obsolete endpoint and return initialization HTTP 404.

Implementation checkpoint `6fb9b78cdebe61a9eb8ce12dc3071dc586029242` is pushed to
`origin/hello-nyx` with exact remote HEAD verification. The private receipt is
`build/manual-presentations/remote-checkpoint.json`. Current production and all
twelve protected auxiliary process identities were reverified after deployment
and semantic qualification. Finish this batch at the integrated result; the next
deliverable is original full move snapping, not another manual-rule fixture.

No original criterion closes. Authoring no-closure advances **18→19** once;
workflow **9**, codegen **28**, renderer **3**, delivery **1** remain. End local
manual/parser/fixture expansion. Next continue original stable container allocation/
containment, structural variants or full move snapping, then complete ordinary
editor/parity/accessibility/performance/delivery. Physical phone input/keyboard,
IME/assistive technology, nested scrolling, another widgetset and large-project
performance remain unqualified. Preserve all original prerequisites and criteria.

## Absolute-position move snapping — 2026-10-06

Original owner: [authoring criterion 1](TODO/NS-4_studio-authoring_01.md). The
declared packet stopped at an integrated absolute-layout journey, preserving flow
placement and refusing ambiguous platform/presentation/bound origin scopes.

- `TNyxMovePosition` and immutable `TNyxMovePolicy` expose typed logical origins,
  grid/keyboard choices, bounds and copied alignment context. Deterministic
  leading/trailing/center candidates precede grid, clamp before conversion and
  preserve unchanged off-grid axes. Alt bypasses snapping; arrows step exactly
  even near a guide. Copied explanations rebase both axes to the final proposed
  rectangle. `TNyxCanvasPreview` preserves the old resize alias and paints copied
  translated outlines/guide segments without moving accepted live controls.
- `TNyxMoveHandle` uses public sequential pointer/key/focus/capture streams.
  It owns subscriptions and borrows receivers; matching pointer identity,
  Escape, loss and retirement cancel. Managed `INyxCanvasMoveGrip` owns an
  independent Nyx document consumed by both adapters. Explicit screen/logical
  mapping keeps the proposal stable as its grip moves. Compact Design retains
  the canvas grip independently of Inspector. No second Studio widget toolkit.
- Ordinary Studio captures exact accepted pair, owner, view, mount, creator
  epoch and sibling geometry. Preview changes no source; release revalidates
  that lease before one isolated worker publication. Both axes are one Undo.
  Private worker tickets use strict v9/15 fields while reading supported v5–v8;
  preceding v8 vocabulary cannot smuggle the appended position action. Existing
  semantic numeric grouped updates remain the MCP operation; no extra MCP tool.
- Native actual input found baseline left/top missing from retained-refresh
  compatibility. Both adapters already consumed those fields, but Studio remount
  lost a live memo draft. Compatible baseline origin changes now retain the same
  control/text/range; shape/creator/ownership admission stays unchanged. Actual
  browser and Win32 journeys qualify the fix, rather than store-only fixtures.
- **99** shared resize/move checks pass on stable FPC 3.2, LCL-matched FPC 3.3.1
  and the actual browser. They include the preceding-helper preparation path.
  Existing semantic placement regression passes **44**. The unchanged generated
  companion passes **22** actual Win32 Studio checks and **49** desktop / **49**
  exact-390 browser checks through the ordinary module worker. Host pointer
  capture, snapped paint, Alt, Escape, exact arrow movement, no preview source
  edits, retained memo/source controls and paired Undo/Redo are exercised.
  Checked native runs report zero leaks. No owned warning; seven known upstream
  pas2js RTL warnings remain per affected browser program.
- Semantic MCP composes the English fixture in one independent project, reads
  bounded windows at one revision and restores their terminal LF. Exact source
  is **106 lines / 2325 bytes**, MD5 `a33951c8cb194473f91ca0fd3fdc2efa`; design MD5
  `c827b2e4bfc1f276ba686b7ce2d67559`. Isolated and deployed immutable jobs compile
  both outputs; their actual source files match the exported companion. Deployed
  grouped position Undo/Redo restores exact source, then the demo returns to its
  baseline at revision 8. One browser build admission returns a valid running
  receipt before a socket-read client exit; terminal status subsequently succeeds.
  This retained transport gap belongs to the existing workflow task, not a retry
  or a compilation claim inferred from admission.

Evidence: `build/move-snapping/qualification.log`, `stable/final-run.log`,
`matched/final-run.log`, `placement-regression/`, `browser-contracts-final/`,
`native/visible-run.log` (**22**, superseding earlier 19), `mcp-source/`,
`browser-studio-input/` and `browser-studio-390/` (**49** each, superseding earlier
32), final Studio/worker compile logs and private semantic receipts. Native,
desktop and narrow moving/final captures were inspected. The maintained build
target is `move-snapping`; physical input tools remain Pascal.

Two final assets publish atomically, backwards-compatible worker first, through
the existing all-interface LAN service. The initial PowerShell replacement call
rejected its empty backup argument before either asset changed; an explicit
private backup path corrected it. The process/listeners and all thirteen auxiliary
services retain exact PID/executable/creation identities. Five existing project
pairs, revisions, selections, views, draft states and Undo/Redo availability remain
exact. Sixteen loopback/LAN served hashes match the nine-artifact closure with only
Studio/worker replaced. The unchanged executable/helper/runtime/HTML bytes remain
explicit; no new server deployment or history reset is claimed.
`.local/move-assets-20261006/` owns byte backups, overlay/served manifests, private
frame receipts and the independent English **Move workshop** in Agents. Twenty
tools authenticate using the Pascal client; this chat still needs a reconnect for
obsolete cached native named handles. The primary remains exact at revision 2.

Implementation checkpoint `a18ac62b61387f156710cceada39d8b13f3a5118` is pushed to
`origin/hello-nyx` with exact remote HEAD verification. The private receipt is
`build/move-snapping/remote-checkpoint.json`; the two-asset overlay manifest records
that implementation. Finish this packet at the integrated result and preserve
the five existing projects plus the independent Move workshop. Next inspect the
original reparenting/flow snap boundary before further implementation.

No original criterion closes. Authoring no-closure advances **19→20** once;
workflow **9**, codegen **28**, renderer **3**, delivery **1** remain. End local
move-policy/fixture expansion. Continue original reparenting/flow snap geometry,
stable container allocation, structural variants and complete ordinary editor/
parity/accessibility/performance/delivery. Physical phone input, changing scale
during a gesture, nested scrolling/virtual geometry, IME/assistive technology,
another widgetset and large-project performance remain unqualified. Original
criteria and prerequisites stand.

## Flow placement previews — 2026-10-06

Original authoring criterion 1's flow/reparenting boundary now has public copied
`TNyxDropPolicy`, `TNyxDropFrame` and `TNyxDropPreview`. The declared gate is actual
physical target geometry mapped to local logical pixels, exact runtime parent and
realized row/column axis. No DOM/LCL/tree ownership enters that portable contract.
Automatic container edge bands/middle and leaf halves resolve typed relative
intent; unknown/grid/absolute sibling axes refuse. Explicit Inside/Before/After
remain available. Geometry factories reject undefined/nonfinite/nonpositive data.

Both adapters copy target context and paint clipped inert insertion strips through
their existing adornments. Runtime application events retain their contract.
Ordinary Studio consumes its existing local lease and paired worker command; no
new MCP operation or private worker vocabulary exists. Its separate Nyx drag
source moved from the Inspector beside the placement selector, remaining usable
in compact Design. Automatic release requires exact last-hover runtime identity,
face and edge. Changed pair/mount/creator/ownership refuses. Invalid hover retires
previous agreement; native final leave hides paint while retaining the agreement
for its immediately following drop. Accepted input controls are never drag sources.

Maintained `tools/build.ps1 -Target flow-placement -FlowSourceDirectory <export>`
compiles/runs shared checked fixtures on FPC 3.2.0 and matched FPC 3.3.1, actual
Win32 Studio against unchanged semantic source, and stages Pascal browser
consumers/Studio/worker. It starts no listener and changes no enrollment. The
native projects directory is unique per run. The Pascal browser driver acquires
real offered drag data, dispatches host drag input/cancellation and selectively
captures paint. It retries only bounded geometry reads retired by an ordinary
editor refresh, never an offer/drop/mutation. Exact insertion edges are scrolled
into the clipped host before input; child visibility alone is insufficient.

Evidence under `build/flow-placement/`:

- `qualified-build.log`: **98** checks per native compiler, zero leaks. Browser
  contracts pass the same **98** (`browser-contracts-final.log`). New checks cover
  typed policy/Unicode copying, exact proposals and retired hover agreement.
- `native/release-compile.log`, `native/release-run.log`: **45** actual Win32 Studio
  checks, zero leaks, no owned warnings. They qualify registered native callbacks,
  actual accent pixels, column reparent, row reorder, exact paired Undo/Redo,
  changed physical face, ending without drop, inherited ownership refusal and
  retained source-editor identity. Capture is `native/release-projects/flow-native.png`.
- `browser-release-desktop.log`, `browser-release-compact.log`: **36 / 36** actual
  host browser checks at desktop and exact 390 pixels, zero driver leaks. Actual
  offered data, before insertion, worker reparenting, accepted memo text, paired
  Undo/Redo, host cancellation, row reordering and final paired baseline pass.
  `browser-release-{desktop,compact}/` owns insertion/row/capture PNGs and receipts.
  Captures were inspected. A dedicated fixed nonwrapping row deliberately scrolls
  in compact view; it is a placement consumer, not general responsive-layout proof.

Retained failed attempts explain the gates: a wrapped fixed-height demo overlapped
its target, corrected through one semantic `.Wrap(nfwNoWrap)` update. A temporary
face transform could receive a legitimate new browser hover before drop, so its
fixture was replaced with real host cancellation; shared/native consumers qualify
strict changed-frame release. The first host cancellation command did not retire
the intercepted preview; the protocol's actual `dragCancel` path does. A narrow
child scroll left the parent edge outside the viewport; scrolling the exact edge
corrected the driver. Two initial native runs were terminated by verified fixture
identity; tracing subsequently established advancing presentation work and full
passing runs. No service or user project was stopped/replaced. Checked native
full-presentation journeys are expensive; these are not performance benchmarks.

Semantic MCP composes the committed English `tests/flow-review.operations.json`
as one related transaction in an independent workspace. Bounded windows preserve
the 178-line/4,267-byte source and terminal LF, MD5
`1d2f658a8e84554f84bc73729f5771eb`. Actual production application jobs on both
targets succeed at revision 3 and serve exact matching compiled source bytes.
The native application has no warnings; browser diagnostics contain the seven
already documented upstream `classes.pas` warnings, with both bounded pages read.
The known native request client returned a valid running receipt followed by a
socket-read failure; status was read without mutation retry. The existing workflow
owner retains that transport gap. Twenty production tools authenticate; obsolete
native named handles in this chat still require reconnect.

The one-editor-asset overlay leaves the v9 worker byte-identical, preserves all
seven current project pairs/navigation/history fields and fourteen exact service
identities, and verifies sixteen loopback/LAN web hashes. No restart/history reset.
The main process stays PID 38744 and primary revision 2. Private
`.local/flow-assets-20261006/` owns the asset backup/manifest, frame/service receipts,
immutable build statuses and independent **Flow workshop** project handle. It
supersedes only Studio in the move overlay; the earlier worker/executable/helper/
runtime/HTML closure remains installed. No old refresh script should be rerun.

Implementation checkpoint `e7e7904a33a67660aad6686996bbbd9a294a7aeb` is pushed to
`origin/hello-nyx` with exact remote HEAD verification. Its private overlay manifest
records that implementation, the unchanged worker and previous overlay chain;
`.local/flow-assets-20261006/remote-checkpoint.json` owns the remote receipt. Finish
this packet at the integrated result and preserve all seven projects and services.

This integrated packet is ended. Authoring no-closure advances **20→21** once;
workflow **9**, codegen **28**, renderer **3**, delivery **1** remain. No original
criterion or task closes. Stop local flow-policy/fixture expansion; return to
original stable container allocation and alternate structures. Automatic grid/
absolute insertion, independent canvas drafts through structural reparent, full
nested scrolling, physical phone touch/hardware, another widgetset, IME/assistive
technology and complete ordinary editor/parity/accessibility/performance remain
unqualified. The compact select still clips text in a host capture despite its
wider face; complete chrome typography/overflow stays open. Preserve original
criteria and prerequisites rather than accepting this bounded preparation.

## Container-aware presentations — 2026-10-06

The declared gate was a named eligible ancestor with stable external allocation,
immutable qualified runtime measurements and no portable widget/tree back edge.
`nyx.containers` supplies distinct references, a containment enum and copied indexed
snapshots. `QueryContainer`/`Containment` are static configuration; named `Within`
conditions reuse the typed viewport axes. Browser `ResizeObserver` content boxes
and Win32 logical allocation publish actual dimensions. Unmeasured/hidden boxes
remain absent, self never matches, and nearest missing boxes never fall through.
Full-size containment supports orientation without using child natural height.

Persistence accepts strict registry version three and retains earlier versions.
Source generation/admission uses managed specialized controls and typed fluent
constructs. Effective bound admission independently partitions the host and each
actual eligible ancestor, correlates shared publishers, includes absent boxes and
checks both targets/manual choices under the existing 65,536-region budget. A
late conflicting group leaves both accepted files and revision unchanged.
Studio uses public Nyx inputs for publisher metadata and shared conditions; actual
Inspector callbacks run through its ordinary paired processor/worker and one Undo.

Evidence and reproduction:

- Compose `tests/container-review.operations.json` in an explicit empty workspace
  with `nyx_transaction`. `nyx_container_mcp_review` reads bounded source windows,
  refuses wrong families/conflicting bounds, checks grouped history and exports
  exact accepted bytes. Explicit optional workspace routing never follows editor
  navigation. The isolated starter-based journey passes 53 checks; the deployed
  empty-project journey passes 43, with fewer source windows. Both end leak-free.
- `tools/build.ps1 -Target containers` is terminal success in
  `build/container-presentations/final-build.log`: 32 shared checks each on FPC
  3.2.0 and matched 3.3.1; actual Win32 27 on unchanged MCP source and another 27
  on a compiled Unicode-name export. Supplementary query names persist, generate,
  compile and select actual controls, without changing the English starter text.
- Actual browser consumers pass 32 contracts and 27 English controls; the compiled
  Unicode-name consumer also passes 27. Allocation changes at fixed host width
  preserve memo objects, independent drafts, focus/ranges and accepted document
  bytes. Full-size frames change orientation under ordinary stretch/flex allocation.
- Existing presentation contracts pass 72 per compiler and actual browser;
  compiled presentation-name checks remain 2 per native compiler. Existing native
  controls pass 52. The direct profiled native Studio journey passes 30, including
  the new named-container Inspector field; browser Studio passes 74 per desktop
  and exact-390 iframe host through the ordinary worker, retained inputs and Undo.
  The early checked native runs were stopped by exact fixture identity while
  diagnosing slow progress, so their interrupted orchestration is not a passing
  build. The final instrumentation run completes with zero leaks. Its 78,152,553
  cumulative allocations/~1.91 GB of cumulative allocation are a performance gap,
  not a peak-memory or release-latency result. No dependency source was edited.
- Deployed MCP composes an independent English **Container workshop** in one group,
  with a whole-view compact rule stacking the same reusable cards on narrow hosts.
  Its exact 216-line/5,900-byte companion has MD5
  `f3869b122f2c2dee21432e2b5098aa57`. Browser job
  `70FC0315-14AF-4DA7-ACB4-4D5AFBEE09E2` and native job
  `EC7831EE-A7D0-448D-9365-914BBB51473B` succeed at revision 5 with matching downloaded
  source, current source/output and zero errors/owned warnings. Browser warnings
  are the existing seven upstream `classes.pas` cases; native warnings are zero.
- Immutable `nyx_preview` captures at 1000×780 and 390×900 show independent wide/
  compact cards and the stacked narrow layout. These are semantic captures, not
  editor screenshot automation. The 76-kind reference is regenerated from metadata.
- The reviewed nine-artifact release needs the new backend for version-three
  admission. It checks exact process/listener identity before stopping only the
  main server, retains fourteen auxiliary services and restores all seven accepted
  pairs/selections/views. No pending draft existed. Main primary revision 2 stays
  exact. Previous concurrent histories reset, handles remap and each imported
  project has one import Undo; the previous Move redo history is not retained.
  This delivery limitation remains open. Sixteen loopback/LAN web hashes match;
  twenty tools authenticate and the new demo is the seventh concurrent project.
  `.local/container-refresh-20261006/` owns current private receipts and rollback.

The isolated container server remains PID 36516, creation
`2026-10-06T04:47:55.226782-04:00`, loopback HTTP 19688/MCP 19689, with its frozen
repository and qualified test web root under `build/container-presentations/`.
The LAN closure is the current top-of-file identity. Earlier overlays/release
scripts are obsolete. Native named handles are not requalified in this chat;
the current Pascal semantic client remains the primary workflow.

No full criterion closes. Authoring no-closure advances 21→22 once, with other
owners unchanged. The original hard prerequisites remain unaccepted. Container
scalar scopes are integrated; anonymous container convenience, alternate child
structures, vertical writing, other widgetsets/DPI, hardware/IME/assistive input
and complete performance/editor/accessibility/delivery remain open. End this packet.

Container implementation checkpoint `f3db090ef3f9be112bfeaaeabd8c4af64e79b182`
is pushed to `origin/hello-nyx` and verified by exact remote SHA. Managed contract
regeneration is byte-identical across all five includes (77 kind contracts and
four facades); the default catalog reference remains 76 kinds. Final observation
retains all seven migrated accepted pairs/selections/views exactly. The container
demo is separate, and all fifteen current service identities remain unchanged
since its reviewed main-server replacement. `.local/container-refresh-20261006/`
owns the final remote receipt. The original goal remains active; no full task or
milestone is complete. Continue with the next declared ownership/publication gate
and outstanding original prerequisites, preserving all current projects.

## Retained structural arrangement — 2026-10-06

Owner: original NS-4 authoring criterion 1 through the open NS-2 LCL
ownership/interaction prerequisite. `TNyxNode.ArrangeLike` prepares all owned
child arrays and actual implementation anchors before publishing an unchanged
realized node set. A separate projection gate aligns independent comparison
copies, then runs the original scalar guard. Source/design/runtime identities,
instance scopes, context, contracts and constructor semantics stay exact.
Add/remove, duplicate/foreign keys, special pane changes, live bindings and
custom creators refuse. Only ordinary hosts reparent/reorder. Both adapters
retain controls, event bindings and emitter lifetimes; supported ranges and
scroll positions restore under their update guards. Native tab order follows
the new child order. A previous clone supports model/physical-parent rollback.

`tools/build.ps1 -Target retained-arrangement -ArrangementSourceDirectory <private
MCP export>` stages without starting listeners. The maintained English operation
fixture is `tests/arrangement-review.operations.json`; the public ownership/adapter
contract is [retained arrangements](docs/retained-arrangements.md). Evidence under
ignored `build/retained-arrangement/`:

- `final-build.log`: 34 shared ownership checks per stable/matched FPC; 91 actual
  Win32 retained-control checks against unchanged MCP-generated source. All native
  fixture teardown reports zero leaks. A separate managed implementation proves
  actual interface-owner retention through eight structural reversals, then exactly
  one destruction. Addition/removal/duplicate/scope/constructor/special-host refusals
  leave the prior tree intact. Invalid restore groups refuse before publication.
- `browser-qualified-owned.log`: 34 executed pas2js ownership checks.
  `browser-qualified-controls.log` and `browser-qualified-compact.log`: 75 each
  against compiled MCP source, including repeated physical reparent/reversal,
  sibling order, exact supplementary draft text, caret, focus and retained callbacks.
  Compact means a fixed 390-pixel host in the ordinary headless browser, not an
  emulated/physical phone. Host marker observation does not inject scripts.
- `editing-native/prepare-run.log` and `run.log`: existing 117/118 native editing
  checks including the compiled consumer, zero leaks. Reparenting exposed the range
  writer's use of a transient current read as a capability check. It now checks the
  admitted widgetset/window/type and validates the requested scalar range against
  current Text before writing. Native handles may be recreated; controls stay retained.
- `studio-browser/compile.log`, `worker-compile.log`, `studio-native/compile.log`:
  affected browser Studio, embedded-RTL module worker and native Studio compile.
  These compilation checks do not establish an ordinary Studio structural journey.
- `all-tools.jsonl`: twenty authenticated semantic tools. One isolated English
  project is composed in a single nine-operation group. Two moves and a caption
  update form another paired Undo step; one semantic Undo restores the source.
  Bounded windows of 80+40 lines at revision 2 export the base. The source API omits
  its final line separator; restoring that LF matches both immutable compiler
  copies exactly: 120 lines / 2,693 bytes / MD5 `bca3f8f1d6ab5741282b8eb666e3e0f4`.
- `status-browser.json` / `status-lcl.json`: jobs
  `F3448BE8-B65F-48E5-8C30-F4F6EE3BF6BF` and
  `A87D6E2B-8266-4034-A76B-BFC493C81B6E` succeed with currentSource/currentOutput
  true at revision 4. Both compiledSource downloads match that exact export.
  Browser warnings are seven upstream RTL cases; native/owned warnings are zero.
  These jobs establish compilation, while the separate control consumers establish
  execution. One native request returned a complete receipt then a socket-read error;
  subsequent authenticated status calls returned exit 0. The transport follow-up
  belongs to the existing workflow task, without inflating its completion credit.

Rejected/adjusted preparation: a local record containing an interface could not
compile with pas2js, and a local class was also unsupported. Unit-private temporary
classes now own anchors on both targets. Native focus checks establish each move's
actual starting field/range: clicking another control legitimately changes focus,
and LCL AutoSelect changes the range when that field is refocused. The fixture
does not assume an earlier action's focus persists. The original refusal case now
chooses a different typed variant from the MCP catalog's existing default. These
changes preserve the assertions and do not relax projection admission.

Readonly preservation observes eight main pairs/navigation/draft/history states,
all exact against the prior packet. Fifteen preexisting PID/creation/executable/
command identities and main LAN HTTP 200 remain. The own isolated service is stopped
only after final browser evidence and exact identity verification. No main binary,
frontend, worker, compiler profile, pair, history or concurrent handle is replaced.
Private service/pair/compile receipts remain ignored. Implementation checkpoint
`01c4f08e5060efdf3f3e853c74d3f07efa6a9bb5` is pushed to `origin/hello-nyx`, with
exact remote SHA verified in `build/retained-arrangement/remote-checkpoint.json`.

Renderer no-closure advances 4→5 once for this integrated ownership prerequisite.
Authoring 22, workflow 9, codegen 28 and delivery 1 remain. No original full criterion
closes and no DONE task moves. End this local arrangement/fixture batch. Return to
fluent alternate recipes/structures, retained bound state, atomic view publication,
ordinary Studio presentation switching and original parity/accessibility/performance/
delivery prerequisites. IME, nested viewport continuity, rollback fault injection,
large-project budgets, physical phone, another widgetset/DPI and assistive technology
remain unqualified. Existing main runtime awaits a grouped release.

## Allocation-free property lookup — 2026-10-06

This packet serves the available LCL renderer prerequisite before adding alternate
presentation structures. Its original model/catalog blockers are accepted; native
interaction/resize and complete parity criteria remain the acceptance owners.
The existing deeply composed Studio shell was retained, including every control,
ordinary worker/Inspector action, live memo draft, presentation transition and
paired Undo. No smaller document or relaxed assertion supplies the comparison.

Optional `NYX_LCL_LAYOUT_PROFILE` measures only fixed traversal counts and time.
An initial bounded recursive measurement cache produced zero reuse in the actual
journey, increased allocations slightly and was removed completely. The cause
addressed instead is `TNyxStrings.IndexOfName`: old lookup copied each candidate
name through `Names`; new lookup compares exact storage-unit prefixes without
allocating them. First duplicates, case, normalization, supplementary characters,
NUL, unseparated items, empty names and large separator offsets retain their public
semantics. No persistent cache, tree reference or ownership rule is added.

An unrelated original layout-policy assertion also failed against the old lookup.
Its new bounded expected/actual diagnostic showed the native-only wrapping value
being generated into the base scope. The successful platform decoder was followed
by a failed viewport decoder which reset its out parameters. Disjoint namespace
probes now preserve the platform, restoring the previously accepted split/platform
contract. Tests and acceptance assertions were retained rather than weakening
the comparison. Source/target runtime grades and upstream dependencies are unchanged.

Evidence under ignored `build/native-measurement/`:

- `baseline-build.log`, `baseline-run.log`: the unchanged complete 30-check native
  Studio journey, matched FPC/LCL, range/overflow/I/O/assertions, heap tracing and
  identical semantic manual-presentation source. 78,252,212 allocations /
  1,910,166,242 cumulative bytes, zero leaks. The early 23-check run omitted a
  consumer definition and is not the comparison baseline. The rejected cache
  run (`optimized-run.log`) passes 30 but has zero cache hits and 78,291,010 /
  1,918,422,535; none of that cache remains in source.
- `allocation-free-run.log`: 30 unchanged native Studio checks, 23,493,324 /
  816,314,623, zero leaks: approximately 70% fewer blocks and 57% fewer bytes.
  Maintained `tools/build.ps1 -Target native-measurement
  -ResponsiveSourceDirectory build/manual-presentations/mcp-source
  -BrowserOutput build/native-measurement/web` ends successfully in
  `maintained-build.log`; after the platform generator correction its identical
  block count is 23,493,324 / 813,711,446 bytes. Traversals/control counts remain
  unchanged. These are cumulative allocations, not peak usage or a release
  latency budget; development compiler/browser work overlapped timed runs, so
  wall-time ratios are not claimed.
- `maintained-build.log`: 133 exact text/ownership checks per FPC 3.2 and matched
  3.3.1, zero leaks. `browser-text.log`: the same 133 executed in pas2js/Chromium.
  `contracts-stable/` and `contracts-matched/`: original 141 split/platform and
  72 named-presentation checks per compiler, zero leaks.
- `layout-run.log`: 2,169 actual native control/arithmetic checks, zero leaks;
  `browser-layout.log` / `browser-layout-phone.log`: 2,214 desktop / 2,215 actual
  390-pixel iframe. `container-run.log` / `browser-container.log`: 27 each actual
  native/browser allocated-container/retained-input checks. Companions are exact
  existing semantic exports, not hand-written replacements.
- `browser-studio-worker-fixed.log` / `browser-studio-phone-worker-fixed.log`:
  unchanged ordinary Studio Inspector/worker/Undo journey, 74 each desktop /
  actual-390. Independent projects were composed with one revision-aware MCP
  transaction each. Earlier browser failures at stage 1 came from my manual
  staging command using `-Tbrowser` for the source worker; the maintained contract
  requires `-Tmodule` and its generated `rtl.run`. Correct staging passes. A
  speculative fixture polling change was reverted; all original assertions and
  the original fixture remain byte-identical to HEAD.
- Current isolated backend authenticates all twenty tools. Bounded semantic
  queries and one grouped transaction create a five-operation English platform
  companion. It generates both base `.Gap(12)` and native `.Gap(20)` with a typed
  `ForPlatform(npfNativeLCL)`. Optional outputs are first unconfigured; copying the
  existing authorized private machine profile into this owned test repository
  enables both ordinary MCP builds. No user's profile or enrollment is changed.
  Browser job `1C30EE11-E0D0-4C55-9F3D-4C751B7075D2` and native job
  `B795A008-6BC0-4FB9-8067-D1AFDB9533C2` succeed with current source/output at
  revision 2. Two bounded source windows and both downloaded compiler sources
  match MD5 `fde907a7c00e8d8acf8b2d79d1772777` exactly: 83 lines / 1,690 bytes.
  Native warnings zero; browser seven upstream RTL warnings, zero owned. These
  jobs prove compilation, not execution of that small companion.

Only the owned isolated test server was replaced to qualify current generator
bytes and its machine profile. Main service PID 41164 and fourteen auxiliary
identities remain protected. `protected-services.json` owns the before identities;
`protected-main-current.json` captures all eight current exact pairs and navigation,
with primary revision 2. No main server binary/frontend/profile, project history,
selection, pending draft or workspace handle is changed by this packet. The main
LAN release still serves the earlier container closure; the new shared lookup and
generation changes await a grouped release. Native named handles still return the
known cached endpoint 404; the authenticated Pascal semantic client is primary.

No full criterion closes. Renderer no-closure advances 3→4 once; authoring 22,
workflow 9, codegen 28 and delivery 1 remain. Stop local lookup/cache refinement.
Continue the original structural presentation ownership/publication gate, with
complete native/browser parity, component breadth, accessibility, performance,
editor and delivery acceptance requirements retained. The original goal is active.

Final fixture cleanup verifies and stops only the owned compiler test service
PID 15396. All fifteen pre-existing service identities remain exact and the LAN
endpoint responds HTTP 200; eight main project frames remain present, with primary
revision 2. `final-service-preservation.json` records this check. No additional
fixture listener remains running and no production refresh script was invoked.

Implementation checkpoint `9d2717b02df36017e0dc85316a847f223cede741` is pushed to
`origin/hello-nyx`, with exact remote SHA verified. Ignored
`build/native-measurement/remote-checkpoint.json` records the implementation receipt.
All owned code/test changes are checked; the original goal remains active. The
current LAN container release stays unchanged while the next grouped release is
prepared, retaining all current projects and the remaining structural gate.
