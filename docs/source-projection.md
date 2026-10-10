# Compiler-executed Pascal construction

[Architecture](architecture.md) · [Current work](../WORK.md) ·
[Source synchronization task](../TODO/NS-1_codegen_01.md)

New native invocations now use [typed owned storage](build-storage.md): exact
source/result/log evidence survives post-join derivative retirement, with an
all-files override and capacity preflight. Browser packages stay intact. This
changes no base receipt GUID/wire shape and grants no cleanup authority over
older jobs, projects or application outputs.

The fluent importer handles Studio's authored source vocabulary. Ordinary Pascal
helpers, class methods and loops also need the real compiler. The explicit
`TNyxBuildExecutor.ProjectSource` API compiles a complete trusted source unit and
executes its `BuildNyxDocument` constructor. It does not run the fluent interpreter
first or require an already reconstructed document.

The shared `nyx.studio.sourceprojection` contract has no DOM/LCL handles. Its
immutable interfaces retain exact source, typed target/state and independently
managed compiler diagnostics. Every `CopyDocument` returns a fresh caller-owned
tree, which can outlive the result and executor. Refused or compiled-only results
have no design and refuse `CopyDocument`.

```pascal
LBuild := LExecutor.ProjectSource(LPascal,
  NyxPascalUnit('my.application.views'), btNativeLCL);

if LBuild.Projection.State = spsExecuted then
begin
  LDocument := LBuild.Projection.CopyDocument;
  try
    { Consume this independent candidate through the caller's admission policy. }
  finally
    LDocument.Free;
  end;
end;
```

The native host API belongs to `nyx.studio.buildexecutor`. It copies an immutable
machine profile and admitted directory roles. Native model execution needs FPC,
without a widgetset/display prerequisite. Browser compilation needs pas2js and
its matching RTL; it returns `spsCompiled` and a fixed relative worker artifact.
Its complete Pascal worker executes the same constructor and sends a bounded
JSON text packet. The owning browser consumer terminates the worker and receives
the packet using `ReceiveNyxSourceProjection`, with its exact expected source,
opaque reference and `btBrowser`. A compiler report or worker URL alone proves
no executed design.

| State | Meaning |
| --- | --- |
| `spsCompiled` | Browser worker compiled; no design has executed/admitted yet |
| `spsExecuted` | Constructor ran and its complete canonical design was admitted |
| `spsUnavailable` | Requested compiler/runtime is absent; authoring remains available |
| `spsCompilationFailed` | Compilation/process admission failed; diagnostics stay separate |
| `spsExecutionFailed` | Constructor raised, child failed or execution budget expired |
| `spsInvalidDesign` | Missing/invalid document or malformed/mismatched/excessive reply |
| `spsCancelled` | Host cancellation retired work; no usable result/artifact |

Source is limited to 1 MiB of exact UTF-8. Replies are limited to 4 MiB before
native byte allocation/decoding or browser JSON parsing. The exact six-field
versioned envelope matches the expected invocation/target; successful designs
must decode, pass complete structural/property admission and encode identically.
Native stdout is diagnostic output, separate from the owned result byte file.
Each native compiler/constructor uses the existing process-family owner and a
separate host time/log budget (default 60 seconds and 1 MiB per invocation).
The trusted `spcChecked` choice adds range/overflow/heap diagnostics for native
qualification. Callers cannot pass arbitrary compiler arguments or shell text.

This API deliberately executes source, including its initializers and helpers.
It supplies process ownership and bounds, without a filesystem/network sandbox.
Only an explicitly authorized host should invoke it; ordinary file import and
MCP project payloads retain their existing admission rules. A received design is
staged independent data, with no authority to replace an editor project.

## Reproduce the qualification

`tools/build.ps1 -Target source-projection` compiles/stages the maintained Pascal
tools and browser consumer. It executes no constructor, starts no service and
changes no enrollment. Add `-SourceProjectionRuntimeHome <new-private-directory>`
to run native constructors/refusals and stage their compiled browser workers.
`-SourceProjectionToolchain` selects an existing local toolchain JSON; defaults
read `.local/toolchain.json`. Compilers are reused rather than installed.

`-SourceProjectionBuildHome` independently selects this target's compiled tools
and browser staging root; relative paths resolve against the checkout. Empty
preserves `build/source-projection/maintained`. It can use another local volume
without relocating source, profiles, projects or any existing build output.
`-SourceProjectionRuntimeHome` still requires a new directory for execution;
choosing a build home alone starts no constructor, browser or listener.

The runtime home must be new. For actual browser execution, explicitly serve that
home's `web` directory and Jobs root with the built
`nyx_resource_runtime_server`, passing repository, runtime home, an unused
loopback port and staged web directory. Observe its page with
`nyx_browser_ready_capture`, passing URL, a new evidence directory,
`data-source-projection`, width and height. Neither the build target nor these
consumers replace the production LAN host or global/user MCP enrollment.

The maintained handwritten fixture includes class/helper expressions, arithmetic,
a loop, two pages, reusable construction, scalar defaults, text/JSON resources
and supplementary/combining Unicode. An independent literal builder supplies
the exact whole expected codec snapshot. Current qualification passes **55 native
checks** and **33 actual HTTP browser checks**, with zero reported native heap
leaks. Browser execution includes successful, throwing and nil constructors;
native also qualifies type failures, unavailable tools, deadlines and cancellation
after a physical execution marker. Both packet consumers refuse malformed,
mismatched and excessive replies and exercise independent result/tree lifetimes.

## Guarded local editor publication

`nyx.studio.projectionediting.PrepareNyxProjectedSource` adapts an actually
executed result to the existing immutable `INyxPreparedSource` contract. Capture
the source request and creator snapshot before dispatch, then complete that exact
request through `TNyxStudioSession.CompleteSourceRequest`. Its existing guard
checks editor owner/load generation, accepted source/design, current draft and
creator revision before one paired publication. Failed or stale results retain
the current pair and newer draft; success consumes that draft and adds one Undo.

```pascal
LSchemas := CaptureNyxSchemas;
LRequest := LSession.PrepareSourceRequest(LSchemas.Revision);
{ Dispatch LRequest.Source through the explicitly owned compiler channel. }
LPrepared := PrepareNyxProjectedSource(LProjection, LSchemas);
LCompletion := LSession.CompleteSourceRequest(LRequest, LPrepared);
```

`TNyxSourceOrigin` distinguishes declarative and executed construction. An
executed workspace retains the entire exact unit, including units without managed
builder markers. Its immutable checkpoints preserve that origin through paired
Undo/Redo. Hosts can query the request's typed `Origin` for explicit dispatch.
The local projected preparation refuses literal-worker serialization; the bundled
literal receiver refuses the executed checkpoint shape. A serialized checkpoint
is restoration data, with no proof or permission to execute/import its source.

The paired boundary adds **32 guarded publication checks on FPC and actual HTTP
pas2js**, separately from the existing 55/33 execution checks. These qualify the
session/source/history contract, independently of physical Studio Apply or installed
HTTP/MCP execution. The existing managed-source regression passes 33 and ordinary
project-import admission passes 590, with clean native heap traces.

## Optional compiler strategy in ordinary Studio commands

`nyx.studio.sourcecompilation.INyxSourceCompiler` is the portable asynchronous
strategy for ordinary source Apply. Installing it is explicit trusted host
execution authority; it is neither a design property nor an output prerequisite.
`Start` receives exact copied source and an independent completion port, and
returns a typed cancellable operation. The producer owns bounded work and retires
without borrowing an accepted tree or Studio. Completion stages the actual
projection under captured creators and posts it to the existing guarded command
queue. Stale, cancelled and detached deliveries cannot publish into that session.

The native adapter creates one executor per job with copied machine profile,
directory roles and limits. The browser adapter asks an
`INyxBrowserSourceBuilder` provider for an exact compiled receipt, then owns the
worker, its 30-second execution deadline and bound receiver. Providers own their
own bounded compilation transport. A provider-supplied executed result is refused:
only the independently received worker result supplies browser execution evidence.

```pascal
{ Native embedding: these values are trusted host configuration. }
LCompiler := NewNyxNativeSourceCompiler(LDirectories, LProfile, LLimits);
LStudio := TNyxNativeStudio.Create(LHost, LProjectDirectory, LCompiler);

{ Browser embedding: the provider delegates compilation to its owned backend. }
LCompiler := NewNyxBrowserSourceCompiler(LProvider);
LStudio := TNyxStudio.Create(LCompiler);
```

These are separate target examples. Their adapters belong to
`nyx.studio.sourcecompilation.native` and `.browser`. A bare Studio constructor
omits the strategy and retains existing literal authoring without compilers.
`TNyxSourceCommands.UseCompiler` changes the host strategy only on an idle,
attached command context; it does not modify a draft, document or history.
Native Studio carries the explicit strategy into its independent project contexts.

### Native strategy shutdown

The native strategy exclusively owns its scheduler and bounded admitted jobs.
The five-argument `NewNyxNativeSourceCompiler` overload accepts trusted typed
`TNyxSchedulerOptions` alongside the storage policy; older overloads retain four
workers and 1024 pending slots. Jobs copy configuration and retain independent
completion ports; no job retains the strategy or an editor tree.

Pending cancellation and dispatch choose one completion owner atomically. A
cancelled queued job delivers one typed `spsCancelled` projection, releases its
port and reaches terminal without allocating a compiler directory. Cancellation
also releases its pending scheduler capacity. Strategy destruction cancels its
queue and running execution contexts without joining workers on the UI. Active
jobs observe both scheduler and caller cancellation; their compiler/constructor
family and executor retire before completion/terminal publication.

Pending completion runs on the cancelling thread, active completion on its worker.
Ports must stage/queue independent results rather than wait on the UI. Tokens stay
nonterminal while a callback returns; a delivery already begun can win a later
cancellation. A throwing port produces `scsFailed`; the optional native
`INyxNativeSourceCompilation.Failure` retains its diagnostic independently of the
strategy and is empty while nonterminal. Successful producer delivery does not
establish editor admission. Typed compiler/constructor failures retain their
ordinary diagnostic/rejection path instead of becoming generic transport failures.

The maintained `nyx_source_shutdown_tests` uses real marker-gated native Pascal
construction, one worker/one pending slot, exact live process handles and actual
ordinary editor commands. Current Win32 qualification passes 42 harness checks
(39 lifetime and three harness/consumer checks), with 40 separate ordinary source
checks and zero unfreed blocks. Existing LCL/storage consumers compile; other OS,
physical UI, browser/HTTP and installed rollout are separate evidence gates. See
[the owning service task](../TODO/NS-5_service-reload_01.md) and
[the current packet](../WORK.md#current-return-path-native-compiler-shutdown--2026-10-10).

The ordinary queue passes **35 shared native FPC / actual HTTP browser checks**
for full helper/loop Apply, exact paired Undo/Redo, actual throwing construction,
newer drafts, cancellation, detached lifetime and literal fallback. Actual Studio
controls pass **12 Win32 / 13 browser checks** with inspected rendered views and
the same mounted source editor. Complete source replacement now chooses a usable
root/scoped selection when the old view disappears. Managed-source 33, actual
native design queue/presentation 10 and default source scheduling/controls 65
regressions pass. Native consumers report zero heap leaks.

The maintained source-projection target also stages the literal source worker and
`source-compilation.html`. Serving the latter in the same explicitly owned runtime
home exercises physical browser Apply/Undo/Redo; the driver observes
`data-source-controls`. Its qualification provider accepts only the exact source
and throwing-source receipts compiled by the native staging host. It is not the
general HTTP compiler provider. The Win32 consumer
`tests/nyx_source_compilation_controls.lpr` uses the existing LCL toolchain and
accepts repository root, local toolchain JSON, staged runtime home and a new
evidence directory. Both fixtures own test projects; neither enrolls or replaces
the production editor.
The native consumer now starts the ordinary compiler-independent editor and uses
its actual Outputs fields/actions to install local execution, rather than injecting
a compiler at construction. It retains complete Apply/Open/history/file evidence
and checks failed/busy configuration, writer ownership, disable/re-enable, exact
saved hints and compiler-independent relaunch. Its details grip/scroll host reveal
the real settings card for capture. See [native Studio](native-studio.md) for the
explicit machine configuration contract. This Win32 controller qualification does
not establish browser source execution, physical hardware/IME or another widgetset.

## Private compiler service and receipt transport

Current source adds `/api/agents/source` for a same-origin private editor
capability. This is operator compilation, separate from public MCP/project
admission. The HTTP envelope is bounded before JSON parsing; exact source still
has its 1 MiB UTF-8 limit. Request captures one project/revision and machine
profile, including an unfinished draft, without changing its pair or history.
Status/cancel uses that exact owned job/context. The active `jobs` query returns
at most ten handles/states with no source, profiles, diagnostics or artifacts.

Source jobs share application slots, FIFO queue and retained handles. Their closed
purpose prevents application status/launch/report APIs from treating them as
application builds. A copied whole-job lease includes queue time (120 seconds by
default; trusted hosts may configure 1..120000 ms). An expired queued job cannot
start on a later poll; cancellation/expiry of running work holds the slot until
the child/process owner and worker join. Exact request retries return their original
receipt, including after revision changes; different arguments cannot reuse it.

`nyx.studio.sourcebuilds` encodes a six-field compiled browser receipt. Source stays
with the exact authenticated caller; diagnostic records omit its duplicate and
restore it on decode. Executed-state claims and substituted worker paths refuse.
`NewNyxBrowserSourceService` in `nyx.studio.sourcecompilation.service.browser`
captures private editor capability, typed workspace and acknowledged revision.
It implements the builder consumed by `NewNyxBrowserSourceCompiler`, owning XHR,
polling and deadline without borrowing a bridge or Studio. Lost admission replies
recover the exact operation; a local abort cannot claim server cancellation.
The browser compiler adapter separately owns execution of the returned worker.

The same private source request now accepts an optional closed `target`, encoded
with `NyxBuildTargetName`. Omission keeps the browser contract; `btNativeLCL`
requests native constructor compilation/execution independently of any chosen
application output. It uses the same owned job, immutable retry identity, worker
budget and precompile context. A terminal native receipt has seven exact fields:
version, producer reference, target, state, message, source-free diagnostics and
the nested producer packet. `EncodeNyxNativeSourceBuild` and
`DecodeNyxNativeSourceBuild` own this private boundary; the latter re-admits exact
producer/target/canonical meaning into independent ownership. Failed receipts
have no construction, and no native receipt advertises an executable URL.
The combined receipt is bounded at `NyxNativeSourceBuildMaximumReplyBytes`; its
individual producer packet retains the existing 4 MiB bound. An HTTP client must
bound reply bytes before JSON parsing and only decode its authenticated job.

Opt-in native completion sends only the retained producer reference. It consumes
the service's joined executed result under the captured authority, workspace,
revision, complete pair and creator guard. Client-supplied construction is
refused, even when it matches. Native and browser completions retain the same
small receipt, exact replay, durable rollback and paired history path. This does
not grant ordinary project-file import authority or add a new MCP tool. The
native provider is described below. The maintained publication consumer passes **70** native/delegated
checks: actual FPC helper/loop construction, independent copies, malformed native
receipts/type failure, guarded native publication/durable rollback and Undo/Redo,
plus the earlier real-pas2js/simulated-browser producer cases. Separate source
service regression remains **42**, clean heaps. Server and both Studio targets
compile with zero owned warnings. Real native HTTP/observing UI and installed
rollout remain required.

### Native shared provider

`nyx.studio.sourcecompilation.shared.native` supplies
`NewNyxSharedNativeSourceCompilerFactory`, implementing both shared Apply and
visual continuation interfaces. The ordinary native entry point opts in when a
service origin is explicitly supplied. Trusted embedding may configure its copied
operation/request/polling/scheduler policy before installing that strategy:

```pascal
LSharedFactory := NewNyxSharedNativeSourceCompilerFactory(LServiceOrigin,
  TNyxNativeSourceServiceOptions.Defaults
    .WholeOperation(150000)
    .Polling(100)
    .Requests(NewNyxTransportPolicy.WholeRequest(15000))
    .Scheduling(TNyxSchedulerOptions.Defaults.Workers(4)));
LStudio := TNyxNativeStudio.Create(LHost, LProjectDirectory, nil, LSharedFactory);
```

Only an exact numeric loopback HTTP origin is admitted. Creating the factory
requires neither a selected output nor an installed compiler. Each operation
captures its private capability, issuing server, typed workspace, revision,
source and optional semantic intent; no worker borrows the bridge or editor.
The existing deadline HTTP adapter owns sockets and bounds reply bytes before
UTF-8/JSON decoding. Whole-operation time includes local queueing and retries;
individual requests cannot exceed its remaining budget. Trusted settings are
machine configuration, outside exported documents and paired history.

Lost admission replies repeat the identical operation to recover its retained
job. Running cancellation requests remote retirement and observes terminal join;
an unconfirmed retirement is reported explicitly. Publication sends only the
service's retained producer reference. Lost completion replies recover the same
receipt. Wrong or malformed acknowledgements, including invalid server-error
bodies, require observing reconciliation: they cannot imply a definite refusal
after the server may have committed. Exact success goes through the existing
ordinary queue/bridge and paired guards; the next edit waits for observing
acknowledgement. Pending cancellation delivers once without network work.

Factories own bounded public scheduler work and independent operation tokens.
Retirement cancels queued/running work without synchronously joining on the UI
thread; operation owners retain tokens until terminal. Completion ports must
stage/queue safely because running completion is on a native worker. A throwing
port becomes a failed token with an immutable diagnostic through
`INyxNativeSharedSourceCompilation.Failure`. The trusted transport overload serves
embedding and qualification; an in-process transport does not prove HTTP input
or physical connected-editor behavior. General project import authority and
application output remain separate contracts.

The maintained ordinary queue/bridge consumer passes **176** checks: the earlier
135 coordination cases plus 41 cases using the production native provider and
actual FPC backend through a substituted transport. It covers helper/loop Apply,
handwritten title/property continuation, observing barriers, exact draft/base and
paired Undo/Redo, lost request/completion replies, wrong/malformed/500
acknowledgements, refusal, cancellation/retirement and retry expiry. Its heap is
clean. The native executable/server and browser Studio compile with zero owned
warnings and matched RTL. These results qualify the integrated model/queue path;
actual HTTP, physical connected-editor input and installed rollout stay open.

The maintained source-projection target compiles/stages `source-service.html` and
its separate generated module. Its consumer edits the input unit before requesting
compilation, then drives physical Apply/Undo/Redo and the ordinary queue journey;
it cannot replay the earlier precompiled fixture. The target's explicit runtime
option also runs `nyx_source_service_tests` with the actual source and held Pascal
compiler fixture, in a new owned runtime beneath that home.

Current native private-engine qualification passes **42** checks, including actual
pas2js compilation/type failure, private authority/revision/context refusal,
source-free diagnostics, malformed receipt refusal, exact retry/pair retention,
mixed application/source limits and physical cancellation/lease expiry/join.
The existing compiler lifecycle regression passes **142**; native heaps are clean.
The HTTP route and browser provider/consumer compile. Their actual HTTP/browser
qualification remains unproven: automatic approval review rejected the background
launch of their owned loopback host with only `blocked by policy`. No server
started and no alternate launch was attempted. These native/compiler results do
not establish browser transport, controls or installed delivery for this path.

## Trusted in-process shared publication

The backend's `EditorCaptureSourcePublication` captures the exact current buffer,
workspace/revision/full pending pair and the ordinary source request/immutable
creator environment before compilation. Its opaque transient ticket has no wire
codec or setters. `EditorCommitSourceProjection` requires that issuing private
editor authority and an actually executed immutable projection of the captured
unit. It reuses `PrepareNyxProjectedSource` and ordinary source completion, including
the short creator-generation publication guard. Successful completion becomes one
paired history step; a failed durable checkpoint replacement rolls back files,
buffers, selection/view, revision and history.

`EditorCaptureProject` supplies an authorized in-process observing host with an
exact pair and opaque live source checkpoint at one revision. `AdoptCapturedProject`
stages independent owners, validates current document properties and uses ordinary
paired history. `AdoptProjectedProject` additionally provides explicit admission
of a matching live executed result. Project strings and serialized origin flags
cannot substitute these values. An ordinary draft-only commit may reuse only its
current exact live executed files; different accepted strings still require strict
source admission. Undo/Redo retains construction origin and exact unfinished text.

Current checked native qualification passes **37** checks using real FPC helper/
loop execution and actual pas2js compilation. It covers issuer/revision/source/
selection/view/creator refusal, independent observation, supplementary Unicode
drafts, paired history, usable roots after moved identities and a physically blocked
checkpoint replacement. This is
private-engine/model execution evidence, not LCL controls or HTTP/browser
publication. The maintained source-projection target runs the same consumer in
an explicit new owned runtime. The live HTTP test-host gate remains unqualified.

## Private owning-editor observation

The private editor state now includes a negotiated `sourceObservationIssuer`.
On a changed revision it also supplies `sourceObservation`, captured from the
already admitted source workspace alongside the exact project under the same
server lock. Its eight fields describe copied frame boundaries in UTF-8 bytes,
custom-frame choice, closed origin and server/workspace/revision context. The
source and canonical design already travel in the project; neither is duplicated.
Unchanged observations omit both large values. The issuer is a server lifetime
identity, not a credential, signature or independently verifiable certificate.
The authenticated owning connection is the authority for the complete pair/frame.

`ReceiveNyxSourceObservation` is used only by that private owning receiver, never
general project import, MCP mutation, worker-result input or recovery. The bridge
captures its issuer on claim, retains immutable request workspace, checks response
revision and refuses missing/swapped negotiated frames before queue acknowledgement.
Decoded UTF-8 slices must preserve exact scalars. Current property/creator admission
still stages independent owners before `LoadCapturedProject` or
`AdoptCapturedProject` publishes. Intentional project admission resets history;
later synchronization retains paired history and existing local-draft protection.
Older peers keep strict literal admission. Metadata alone cannot grant execution.

The maintained `tools/build.ps1 -Target source-observer` compiles the actual native
and browser consumers; supplying `SourceProjectionRuntimeHome` and the existing
machine `SourceProjectionToolchain` runs the owning-engine fixture in a NEW runtime.
It starts no listener. Checked native qualification passes **33** with actual FPC
construction, actual second-workspace queries, independent observing sessions,
negotiated/legacy peers, local/shared Undo/Redo, UTF-8 frame refusal and draft races.
Draft/protocol regression passes **41**; paired project regression **60** and
source regression **32/35/55/42/37** pass separately, clean heaps. Browser Studio
compiles. Actual HTTP/browser observation and physical controls remain unqualified
for this new path after the earlier owned-host launch rejection.

## Owning browser worker publication

The private source request can opt into shared publication. Before compilation,
the server captures the exact editor authority, workspace, revision, full pair,
sealed source request and immutable creator snapshot. Compile-only jobs keep
their original contract. Completion belongs only to that joined successful job
and its bound owning producer, under the private editor capability. The browser
provider executes the existing owned worker, then delegates its admitted result
to the same server. This is explicit owning-client delegation, not independent
server proof that a browser ran the constructor. No public origin flag or general
file/project/recovery decoder gains execution authority.

Ordinary paired admission and durable rollback still apply. A small seven-field
acknowledgement retains server/project/job/producer/revision context. Exact
successful replay returns that original receipt without another history step;
changed producer bytes refuse. Retained metadata shares the sixteen-job bound.
Failure before durable success remains retryable. The distinct
`INyxSharedSourceCompiler`/completion port carries committed/refused/unconfirmed
outcomes and cannot substitute for a local-only Apply port. A lost reply or
malformed success requires observing reconciliation; a later refusal after a
lost reply cannot prove the earlier request never committed. Cancellation
revokes local delivery without promising remote Undo.

The maintained `source-projection` target now passes **39** native protocol,
durability, replay and second-workspace checks using actual pas2js compilation
but an explicitly **simulated browser producer**. Source **32/35/55/42/37**,
observer **33** and draft/protocol **41** regressions pass separately, clean heaps.
The specialized browser consumer compiles and is staged as `source-shared.html`;
it requires a fresh actual HTTP compilation, owned browser worker, local paired
completion and independent private observation. It has not run. Actual HTTP/
browser qualification and physical Studio behavior remain required. No new owned
warnings; each browser invocation retains seven existing upstream RTL warnings.

## Remaining editor integration

Opt-in shared Apply now uses `INyxSharedSourceHost` and a fresh
`INyxSharedSourceCompilerFactory` at acknowledged-draft dispatch. The ordinary
command queue posts immutable preparation/results to its UI scheduler, admits
through sealed local completion and holds later commands until exact source/
design/revision observation acknowledges the server commit. That acknowledgement
never reloads the local result over newer drafts or navigation. Later local work
then synchronizes through the ordinary queue. Explicit refusal releases the hold;
uncertain, cancelled or stale committed results require operator reconciliation.
Both Studio constructors accept optional shared configuration. Default startup
keeps ordinary compiler-independent behavior; the browser factory is idle.

Maintained native queue/bridge/engine qualification now passes **135** with actual FPC
construction, native durable publication, paired history, later typing, queued
intent, handwritten visual continuation, durable rollback, reload/retirement and
an actual second workspace. This does not qualify
browser execution/HTTP or physical LCL input. Draft **41**, observer **33** and
source **32/35/55/42/37/39** regressions pass separately, clean heaps. Both Studio
adapters compile. The `NYX_SOURCE_SHARED` ordinary Studio control harness compiles
and stages as `source-shared-controls.html`; actual Apply/Undo/Redo/observation
execution on HTTP remains pending after the earlier owned-host launch rejection.

Ordinary Studio Apply is qualified with an explicitly injected compiler strategy.
The new private compiler provider still requires actual HTTP/browser qualification.
The trusted in-process shared boundary now carries live admitted meaning, and the
private observer exchange implements its portable receiver. Opt-in HTTP worker
completion now binds its result to the captured job/source/context, but requires
actual browser/HTTP qualification of that queue/reservation/acknowledgement
coordination. Its opaque publication ticket never
becomes a generic input decoder. Default
shared startup remains disabled until the actual transport and executed recovery
re-admission are qualified. Semantic HTTP/MCP
execution and its revision-aware lifecycle remain
required. The local compiler queue now proposes expression-preserving title and
property additions/updates through a separate customization function, described
below. Structural/property-removal and broader contract/resource changes still
need reconciliation. Direct workspace rendering continues to raise
`ENyxSourceExecutionRequired` before regenerating changed executed meaning;
only the compiler-aware continuation supplies publishable paired owners.
General file/recovery re-admission must use explicit compiler evidence. Application
build integration and ordinary compiler-report presentation also remain open.
These remain original source/compiler/workflow tasks. Full native rendering/
Studio parity and preserving LAN delivery retain their acceptance requirements.

## Handwritten builders and visual customization

With an explicitly configured local `INyxSourceCompiler`, ordinary queued title
and property edits can retain complete handwritten Pascal. The original
parameterless exported builder becomes a private `BuildNyxOriginalDocument`;
the exported wrapper calls it once, applies a readable `CustomizeNyxDocument`
and releases the returned document if customization fails. Helpers and unrelated
expressions remain in the original builder. Only edited properties receive
overrides; computed properties that were not edited keep their expressions.

The customization retains specialized managed interfaces with an exact control
identity/kind guard. Its configuration uses the same typed emitter as ordinary
generation, including Boolean/numeric arguments, closed enums and platform/
viewport/presentation conditions. For example:

```pascal
procedure CustomizeNyxDocument(ADocument: TNyxDocument);
var
  LReplyMemo: INyxMemo;
begin
  LReplyMemo := RequireNyxControl(ADocument, NyxControl('reply'), nkMemo) as INyxMemo;
  LReplyMemo.Configure
    .Text('A carefully written reply')
    .Done;
end;
```

Managed comments identify exact control/property blocks. A later edit replaces
only its matching block, retains unrelated statements, and keeps one wrapper.
Scaffolding changes, duplicate boundaries/identities, ambiguous builder references,
conditional units and unsupported meaning refuse. Missing properties are distinct
from present empty values; removal is currently unsupported. Source proposals are
not accepted designs and cannot transfer owners or use the literal worker wire.

An opaque live checkpoint supplies the accepted baseline. An independent staging
session invokes the existing semantic editor commands to compute proposed meaning;
its temporary generated companion is discarded. The complete preserved Pascal
proposal then compiles and executes through the configured host. Exact source and
whole canonical design must match under captured creators before publication.
Successful results use the existing source/design/draft/load/schema guard and one
paired Undo command. Existing unfinished buffers keep their original source base;
a newer draft or project load refuses stale completion.

Native proposal preparation remains on the scheduler worker. Browser proposal
preparation currently runs locally because opaque execution authority cannot be
sent to the literal worker; actual source compilation/execution still belongs to
the configured owning service/worker. Off-loop browser preparation, actual browser
input/HTTP execution, generic MCP transaction continuation, structural edits and
broader state/resource/event reconciliation remain open. The local and ordinary
shared property/title paths do not establish complete WYSIWYG/source
synchronization or target parity.

The maintained `source-projection` target includes
`nyx_visual_customization_tests.lpr`; a fresh owned runtime is required. The shared
ordinary compiler journey also queues visual edits on both target consumers.

## Shared visual continuation

The optional `INyxSharedVisualSourceCompilerFactory` and
`INyxSharedDesignSourceHost` interfaces extend the original Apply-only contracts.
Older factories retain their existing interface and wire shape. A current host
advertises `sharedVisualSourcePublication` only on its private editor exchange;
this adds no public MCP tool or transferable execution flag.

The copied `TNyxStudioDesignRequest.IntentData` contains only the versioned closed
edit. The server parses that intent, captures its own live checkpoint/creators and
independently prepares the proposed source. It refuses any source differing from
that proposal before creating a compiler job. The native publication ticket has
no wire decoder and retains only immutable context/proposal, never a borrowed
accepted document. Job deduplication covers the complete semantic request.

Actual construction must match the exact source and entire proposed design.
Server publication uses the existing complete-registry durable rollback and
visual history policy, preserving the independent unfinished draft and original
base. The local queue uses its own sealed design request and creator snapshot,
then holds the FIFO for exact revision/source/design observation. A newer local
draft, server revision, foreign receipt, cancellation or retired owner refuses
local admission and retains work for reconciliation. Explicit prepublication
compiler refusal releases the reservation without changing accepted files.

Maintained native queue/bridge/backend checks exercise ordered actual FPC
construction, exact supplementary text, local/backend paired Undo/Redo,
independent drafts, wrong source/producer/authority fields, stale typing/revision,
retirement, cancellation and failed durable replacement with exact checkpoint
bytes. The extended browser consumer performs Apply, then visual compilation/
worker execution and independent observation when run against an owned current
host. It has only been compiled in this packet; actual HTTP/browser execution,
physical input, generic semantic MCP transactions and preserving deployment
remain separate acceptance gates.

## Compiler-aware semantic groups

Current source lets the existing `nyx_transaction` tool prepare a complete typed
semantic group against an admitted executed project. The portable queued edit
captures a `TNyxProjectTransactionSnapshot` from the managed transaction contract;
it retains detached normalized values rather than a COM interface in a record,
which pas2js cannot compile. The independent processor materializes its own
transaction, stages its whole candidate and applies the ordinary handwritten
customization strategy. Version-16 private intent carries the complete group;
older literal request shapes and strict field admission remain unchanged.

The backend captures its own current pair, creators and opaque native publication
ticket. A client cannot supply source, origin or execution authority. Native FPC
construction uses the existing worker pool, then the serialized owner compares
the entire executed meaning with the proposal. Current revision/draft/load/schema,
context/permission/profile and durable admission guards must all pass before one
paired Undo publication. Joined pending completions cannot be silently evicted.

Public job receipts distinguish compiler state from publication state. Constructor
verification is independent of output selection and produces no launch artifact.
See [agent completion semantics](studio-agents.md#current-source-handwritten-transactions).
The current handwritten customization boundary supports title/property additions
and updates; broader groups refuse atomically. Actual HTTP/browser execution and
preserving rollout are still required before claiming observing parity.

## Visible browser startup recovery

Ordinary browser Studio now probes the owning host before agent connection,
using `InspectNyxBrowserRuntimeRecovery`. This independent, bounded operation
returns a typed capability-free status. It reads no source and starts no worker;
the private connect capability is discarded. Unknown/contradictory phases,
oversized replies or transport failure surface a visible refusal. A missing
endpoint on an older host permits its existing immediate startup.

Waiting shared projects use `NewNyxOperationPanel`, a public reusable compound
made from specialized labels, progress and buttons. Its
`TNyxOperationPresentation` holds typed phase/actions and integer completed/total
units, independent of DOM/LCL, source authority and transport ownership.
`RestoreNyxOperationPanel` validates fixed direct parts before mutation.
Compound dispatch delivers the compound as Source and the clicked part as Origin;
the typed action helper resolves only that origin inside the delivered source.
The host additionally checks its mounted owner and current context.

The owning client starts only from an operator action. It reports compiler,
worker, admission and joining progress while preserving the local project and
unfinished text. A failed-attempt Retry first reconnects with
`brsCancelRetained`, cancels and observes joined server work, then requests a
fresh cancelled-stage retry. Explicit cancelled-stage retry does not infer
authority from an output target. A revocable managed delivery port borrows Studio;
controller destruction revokes it before retiring transports, preventing a cycle
or a callback into freed views. The ordinary agent bridge retains its existing
local/shared conflict decisions after complete recovery.

The launcher can explicitly select `browser-worker` as its eighth host argument;
see [runtime recovery](studio-releases.md#runtime-session-recovery).
Actual native control checks qualify panel input, numerical progress,
compact presentation and output-independent compiler fields, with a clean heap.
Browser Studio and both recovery consumers compile with matched RTL. Actual
browser readiness/retry/worker HTTP execution and installed rollout remain
separate open gates; compilation does not certify them.
