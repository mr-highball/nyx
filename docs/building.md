# Building Nyx

[Project](../PROJECT.md) · [Architecture](architecture.md) · [Current evidence](../WORK.md)

Product code, persistence, generation, designer state and compiler service are
Pascal. PowerShell only selects tools, passes compiler arguments and stages
matched target artifacts. No Node, npm, Python, CSS framework or remote font is
required.

`browser-worker` builds the Pascal real-clock readiness observer and stages the
ordinary Studio callback consumer with its matched module worker and RTL. It
starts no listener. Execute desktop/narrow journeys through an already admitted
host; see [browser worker qualification](browser-qualification.md) for arguments,
ownership, checkpoints and the distinction between readiness and admission.

`compiler-lifecycle` runs checked Pascal compiler-family/queue/cancellation and
semantic-host qualification on Windows, plus retained profile/retry/handle and
portable admission regressions. Each run owns a new runtime. It compiles the
backend and stages the browser Studio, independent worker, preview and shared
admission consumer without starting a listener or changing enrollment. Execute
the staged `agent-builds.html` through an already admitted host for the browser
half; compilation alone is insufficient. See
[the current evidence and remaining limits](../WORK.md#windows-compiler-families--2026-10-06).

`designer-controls` compiles the Pascal semantic review author, runs the checked
actual LCL designer/retained-source consumer and stages its pas2js counterpart.
Supply `-DesignerMCPConfig <local-file>` for an explicit owned MCP review or
`-DesignerSourceDirectory <export>` for an unchanged existing semantic export.
It launches no server. See [designer views](designer-views.md) for ownership,
runtime isolation, native painting, actual browser execution and remaining
native Studio requirements.

`native-studio` builds the standalone LCL editor at
`build/native-studio/controller/nyx_studio_native.exe`. An already built executable
launches without application compilers or a service. Add `-VerifyNativeStudio`
and `-DesignerSourceDirectory <semantic-export>` to execute its maintained actual
editor journey. See [native Studio](native-studio.md) for lifetime, paired files,
native/browser evidence and remaining compiler/MCP/workspace integration.

Native Studio defaults to `-NativeStudioConfiguration checked`: compiler safety
checks, line debugging and heap tracing, with binaries/units under
`build/native-studio/controller/`. Select `-NativeStudioConfiguration release`
for an ordinary optimized application build under `build/native-studio/release/`.
It retains assertions, range, overflow and I/O checks, uses `-O2 -Xs`, and omits
`-gl -gh`. Separate native units/binaries prevent reuse of a traced build. This
configuration choice alone does not establish release readiness or UI latency.

The same `-VerifyDesignSource` qualification executes all original 128/512/2048
workloads and exact companion reconstruction in either configuration. Release
artifacts live under `build/design-source/release/`; optional source/editor
journeys use `build/source-scheduling/controls-release/` and
`build/native-studio/editor-release/`. Checked artifacts remain in their existing
paths. Both configurations launch no Studio/MCP listener and preserve active work.

The verified Windows pair is FPC 3.2.0 for checked portable fixtures, and the
existing FPC 3.3.1/Lazarus 4.99 pair for LCL. Browser checks use pas2js 3.3.1 and
its matching `rtl.js`. These are observed capabilities, not a promise that every
development revision is supported. The older Lazarus installation's precompiled
units did not match its FPC compiler; use a matched pair.

Owned builds keep compiler warnings enabled. The current warning-cleanup packet
qualifies the core/generated consumers, Studio/server and actual layout/Agents
consumers on the installed native, LCL and pas2js pair; its commands, executed
results and remaining matrix limits are recorded in [WORK](../WORK.md).
The installed pas2js RTL still reports seven incomplete-case warnings in
`classes.pas`. They remain visible and are dependency diagnostics; Nyx does not
patch the installed RTL or suppress them globally.

Two node-owned fluent facades intentionally have private constructors. Their
declarations alone scope FPC advisory 3018 off, with the ownership reason beside
the code. This preserves the node's lifetime contract. Defensive enum admission
still rejects invalid cast/bridge ordinals, intentional partial dispatch is
explicit, and retained byte accounting uses the portable `TNyxTextBytes` type.
Native accounting uses `Int64`; pas2js uses number arithmetic within admitted
history/source budgets. These decisions do not replace execution on both targets.

For source builds, configure the tools needed by that build through explicit
parameters, `NYX_*` environment variables, or an ignored
`.local/toolchain.json` containing paths under these keys:

| Key | Capability |
| --- | --- |
| FPC | Native compiler for core fixtures and server |
| PAS2JS | Browser Pascal compiler |
| PAS2JS_RUNTIME | Its matching rtl.js |
| LAZARUS | Installation root with compiled LCL units |
| LCL_FPC | Compiler matching those LCL units |

From the repository, run:

```powershell
./tools/build.ps1 -Target core
./tools/build.ps1 -Target all
./tools/studio.ps1
```

Individual build targets are `core`, `generated`, `collections`, `collection-views`,
`collection-authoring`, `collection-inspectors`, `collection-bindings`,
`reusables`, `placement`, `designer-drag`, `constraints`, `resize`, `responsive`,
`source-workspace`, `source-editor`, `pascal-imports`, `pascal-routines`, `pascal-declarations`,
`agents`, `compiler-lifecycle`, `state-bindings`, `split`, `interactions`,
`named-events`, `viewport`, `catalog`, `browser`, `studio`, `lcl`, `http`,
`visual` and `all`.
The native unit cache includes compiler version and CPU/OS. LCL and pas2js
artifacts have separate output directories. Build failure propagates immediately.
`placement` executes typed/semantic relative placement on both native compilers,
compares their exact exported files, runs actual native Studio input and unchanged
compiled controls, checks transaction discovery, then stages browser consumers,
Studio and its module worker. Its isolated artifacts do not start a listener or
refresh MCP configuration. Browser compilation retains its host execution gate.
`responsive` runs shared width/scope/admission and paired Inspector fixtures on
both native compilers, compares their generated companion, runs actual native
controls/Studio and stages browser consumers, Studio and its matched module worker.
Supply `-ResponsiveSourceDirectory <bounded-semantic-export>` to compile unchanged
MCP source; omitting it uses the independently generated portable fixture. An
explicit missing source refuses. It also builds the Pascal ordinary-frame browser
review driver under `build/responsive/driver/`; it starts no listener and changes
no enrollment. Serve `responsive-contracts.html` and `responsive.html` through an
admitted HTTP host. The latter's `?host=1` route qualifies an exact 390-pixel iframe.
Run the driver with that loopback URL and an isolated artifact directory; it
observes bounded fixture markers and captures rendering without script evaluation.
These checks qualify [responsive authoring](responsive.md), not ordinary browser
Studio worker execution, hardware, phone deployment or performance budgets.
`designer-drag` executes the shared drag lease/identity guards on both native
compilers, real Win32 Studio source/target callbacks and the unchanged generated
native companion. It stages browser contract, compiled-control and Studio DOM/
worker reviews, plus Studio and its module worker, without launching a listener.
Serve the English `designer-drag-guards.html`, `designer-drag-compiled.html` and
`designer-drag-studio.html` hosts only through an admitted HTTP host and require
`data-result="passed"`. The Studio drag review requires a desktop-width viewport;
compact hosts retain the keyboard/touch placement alternative. DOM synthesis is
separate from physical browser drag-manager, mobile and accessibility evidence.
`generated` emits a fixture through native Pascal, compiles/executes its native
reconstruction and compiles its browser reconstruction. Serve `generated.html`
to execute the latter. `all` includes these checks. Ordinary browser builds do
not depend on a native-generated fixture.

`viewport` runs real LCL scrolling and wheel-cancellation checks, emits crafted
Pascal, executes its native reconstruction, and compiles the browser consumer.
Serve `viewport.html` to execute browser checks, including genuine host scroll/
completion events. `-BrowserOutput build/viewport/browser` stages these files
away from a running Studio. Compilation alone does not execute browser checks.
See [wheel and viewport semantics](events.md#wheel-requests-and-actual-scrolling)
for explicit units, native coalescing and unavailable gesture completion.

`collections` runs the native typed collection contract suite and its verified
CSV benchmark, then compiles both Pascal programs with pas2js and stages their
matching runtime/hosts. Open `collections.html` to execute the 149 browser store/
registry/wire/source-history checks and `collection-benchmark.html` to measure browser costs. Compiling these
programs does not execute them in a browser. The ordinary shared/generated build
also includes the collection suite and seven additional wrong-type fixtures.
`generated` now emits `nyx.fixture.collections.pas` through native Pascal, executes
its native reconstruction and compiles its browser consumer. Serve
`collection-generated.html` to execute three compiled collection checks: complete
design meaning, runtime/default isolation and retained managed lifetimes.
For headless timing, omit Edge's virtual-time budget so `performance.now` measures
elapsed time. Native zero-millisecond samples are below the observed clock
resolution. See [collection contracts and limits](collections.md).

`collection-views` adds the portable projection contract, actual browser/LCL
list/table/tree journey and mounted-control benchmark. Open `collection-views.html`
for the desktop journey or `collection-views.html?host=1` for the exact 390-pixel
iframe. Open `collection-view-benchmark.html` without a virtual-time budget for
the browser measurement. The native CSV is `build/collections-view-benchmark-native.csv`.
See [collection views](collection-views.md) for ownership, failure semantics,
instance scope and authored/Studio integration.

`collection-authoring` runs shared wire/source/history/scope assertions and actual
LCL application/Studio control events. Its Pascal fixture exports application,
page and reusable companions; both FPC and pas2js compile their consumers. It
also checks forty intended wrong-type cases per compiler. Open
`collection-authoring.html`, `collection-authoring-generated.html` and
`collection-unicode.html` to execute the browser journeys and Unicode packet,
design, companion-envelope and source checks. Compilation alone does not execute
them.

`source-workspace` runs the native retained-draft/Unicode diagnostic, indexed
identity/preservation, independent source-context, compiler-location, immutable
history, paired candidate/cache-restoration, fresh property admission, closed
vocabulary and exact control-reference checks (247 total), and
compiles their browser counterpart plus the portable source benchmark. Execute
`source-diagnostics.html` after compilation. Benchmark timing is deliberately a
separate run: native `nyx_source_benchmark.exe` accepts an optional size of 128,
512 or 2048 controls, and `source-benchmark.html?controls=512` selects the same
browser fixture. With no size, all three run. Use the real browser clock without
a virtual-time budget, and keep other compiler/timing work idle. Each row verifies
crafted locals/comments/unchanged expressions, structural changes, exact paired
history and rejected-draft recovery before publishing its timings. These measure
portable source/document work, not painting, trusted input or compiler latency.
Current costs and remaining large-project work are recorded in WORK.md.

Closed Pascal enum spellings now resolve through an immutable typed vocabulary
initialized at unit startup in the original matching order. It stores only names,
argument families and ordinals; source contexts, model values and candidate
admission remain fresh and operation-owned. Native finalization releases its
index; browser module lifetime owns the vocabulary. Public round trips, reserved
locals, wrong families and retained source/history qualify its consumers.

Candidate fluent members borrow their exact declared control only after
construction and attachment. Full document validation builds an independent
exact-text membership index on every call, preserving case, Unicode, ordered
diagnostics and direct-mutation visibility. Duplicate recipe parts and premature
configuration remain rejected without changing the accepted pair or Redo.

The same target also compiles `nyx_property_benchmark` and stages
`property-benchmark.html`. Run it separately on an idle host, using the real
browser clock. It measures full document property admission, 64 ordinary
selection/metadata queries and composition of the public Nyx Studio shell with
its expanded inspector and source editor at the same three project sizes.
Every row verifies typed help, selected values and unchanged source/draft state.
These composition times exclude target rendering and physical input. Native
`--snapshot` / browser `?snapshot` emits all public metadata fields in stable
catalog/descriptor order, with present browser/native overrides, for exact
comparison. Benchmark binaries omit heap tracing; functional checks retain it.
Native timer-resolution values do not establish sub-millisecond responsiveness.

The Pascal `nyx_browser_capture` helper accepts `--real-clock` as its optional
fourth argument for synchronous benchmarks. That mode removes the virtual-time
budget; asynchronous functional captures retain their ordinary budget. The mode
compiles and refuses unknown options before starting a browser. The current
source-performance packet has native execution and browser compilation only:
automatic approval review rejected launching its separate static fixture listener
("blocked by policy"). Do not infer current browser timing or runtime qualification
from compilation, historical captures, or the helper's new option.

Compile the same benchmark with `-dNYX_SOURCE_PROFILE` to append per-stage
`profile,controls,operation,stage,milliseconds,calls` records. Use a separate
output directory so this opt-in instrumentation cannot replace production units.
Its harness supplies the monotonic clock; the portable reader has no production
clock or profiling globals. Parent stages include nested stages, so their totals
must not be added as exclusive costs. This define is for the single-threaded
benchmark only and must remain absent from a concurrently serving Studio service.
The `lex` and `symbol-read` call counts identify unique source snapshots read by
one reconciliation's owned context pool. Mutable readers copy their records;
the final candidate verifier always parses fresh. No parsed context is persisted
in designs, source history or recovery packets. Whole-command samples now include
Apply and history, checkpoint/commit, document Save/restore, workspace snapshot/
restore, Render/encoding and fresh candidate validation/encoding. In-memory
history uses immutable typed checkpoints rather than JSON workspace snapshots;
JSON snapshot work remains at explicit recovery/interchange boundaries.

Current source Apply prepares the independently owned document and exact
companion together from one complete draft admission. Visual verification also
reconstructs the complete candidate, without generating an unused verifier
baseline or encoding that same candidate twice. A workspace retains derived
canonical builder text for its last accepted design; history/recovery omit it,
restore clears it, and every public Render still freshly encodes the document
to observe direct mutations. Rejected candidates cannot replace either owner.
The same 128/512/2048 workload and all its original preservation gates remain.

Studio defaults to [the local service](http://127.0.0.1:8088/). The server runs
in the foreground. `-Port` selects another local port; `-SkipBuild` reuses a
previous build. Its HTTP paths are relative so compiled views also run below
`builds/job-ID/`.

For phone/tablet review on the same LAN, run
`./tools/studio.ps1 -SkipBuild -BindAddress 0.0.0.0` and open
`http://YOUR_LAN_IP:8088/` on the device. Loopback access continues to work.
`-BindAddress` can also select a particular interface. Persist the choice through
`NYX_STUDIO_BIND` or the `STUDIO_BIND` key in ignored `.local/toolchain.json`;
the default is `127.0.0.1`. API origin checks follow the address used to open
Studio, so editing/building works through a LAN URL without allowing other
websites to issue cross-origin requests. The host firewall must admit inbound
TCP on the selected port for the device to connect.
On Windows, `./tools/studio-lan.ps1` from an administrator PowerShell creates
the executable/port rule for **Private networks / LocalSubnet**. It removes the
Private profile from matching TCP block rules while retaining their other
profiles. `-Server` and `-Port` select a different built service/port. The launcher
does not silently change firewall settings or request elevation.

Application outputs are optional. Studio's **Outputs** button opens a shared Nyx
**Target / output** section at any time. Choose later, Browser, or Native LCL;
enter only the chosen output's tools. Empty profiles are valid. Source generation,
design changes and persistence do not require a selected target or installed
application compiler. A requested build reports missing tools in diagnostics.
Edited settings apply automatically before a build, or through **Apply configuration**.

Compiling Studio itself from source needs FPC plus pas2js and its runtime. LCL is
needed only for a requested native output or the native validation harness. Once
Studio is built, `./tools/studio.ps1 -SkipBuild` does not require any compiler.
`-Server PATH` chooses an existing service executable explicitly, without probing
FPC. `-ToolchainConfiguration PATH` chooses an optional file of initial hints.
Runtime profile changes are stored in ignored `.local/studio-outputs.nyx`, and
take precedence over startup hints on later launches. Paths stay out of portable
designs and generated source. The current output choice is stored in browser
local storage; per-project output profiles and broader build options remain open.

Browser fixture hosts are `tests.html` (current shared count in [WORK](../WORK.md)) and `journey.html`
(49 actual DOM interaction checks). Serve them through Studio. Merely compiling
these fixtures does not execute them. Headless browser evidence is distinct
from physical input and device testing.
The Studio journey uses an ephemeral controller with browser recovery disabled,
so it neither reads nor overwrites an existing saved project or output choice.

Studio keeps its three desktop panels at wider widths. At 960 CSS pixels or
below, shared Nyx **Project / Design / Inspector** switches select one full-width
panel. Output configuration remains available from the header. **Pascal** opens
the optional split view; preview scrolling stays inside the canvas. The shell
uses dynamic viewport height so mobile browser chrome does not hide the footer.
Crossing the compact boundary retains the focused field's uncommitted draft.

`studio-layout.html` runs a separate recovery-free authoring/layout journey:
17 checks on desktop, 19 in compact mode. `studio-layout.html?host=1` runs it in
an exact 390×900 iframe, then performs 13 additional checks through real
390 → 1100 → 800 → 390 resize events, including draft focus, commit semantics,
generated source and viewport confinement. Its `data-nyx-studio-layout` and
`data-nyx-studio-resize` attributes report results. This avoids headless Edge's
minimum outer window width when collecting phone evidence. Browser builds stage
both the Pascal fixture and its host; native fixtures also project the shared
compact inspector/navigation contract. Complete native Studio authoring remains open.

The service accepts a version 1 `.nyx` design at `POST api/generate` or
`POST api/build?scope=view|application&target=browser|lcl&page=VIEW_ID`.
View IDs may identify a page, subtree or reusable definition. A full application
build includes every page and definition; the generated host supports page
navigation. Results include success, compiler log, artifact and generated-source
URLs. Build output is confined to `build/studio/jobs/`.

Studio uses `POST api/build?source=companion&scope=view|application&target=browser|lcl&page=VIEW_ID`
with an exact versioned envelope:

```json
{"version":1,"design":{"version":1,"pages":[],"components":[]},"source":"accepted Pascal unit"}
```

The example shows the envelope shape; `design` must contain the complete admitted
document. Pascal callers use `EncodeNyxBuildRequest` from `nyx.studio.builds`.
The service compares companion meaning with the paired design before allocating a
job. Empty/mismatched source, inadmissible namespaces and external-file directives
are refused. Full application source is exact. Isolated builders preserve the
unit name and handwritten frames, including callback implementations and helpers.
They also retain selected controls' deliberate Pascal locals, comments and
unchanged expressions. The reduced builder must reconstruct the isolated design
before a compiler job is created.
Compiler paths and arguments still come from separate local output profiles.
Responses identify `companion`, provide the actual source URL and retain compiler
failures. Admission errors from the source workspace include line/column positions.

Build commands refuse pending or rejected Pascal drafts until **Apply Pascal** or
**Restore accepted**. Delayed results compare both design and accepted source,
as well as draft state, before replacing the preview. Browser builds reject
`neThreaded` before creating a job; asynchronous browser callbacks use the event
loop. The legacy design-only build route remains available to API clients.

`GET api/configuration` reads the local profiles; `POST api/configuration` accepts
`{"version":1,"fields":{...}}`. Supported string fields are `pas2js`, `runtime`,
`fpc`, `lazarus`, `platform` and `widgetset`; omitted fields are empty. The route
stores explicit paths/identifiers, with no shell command or arbitrary argument
field. This is separate from a build request and does not edit the design.

With Studio running, `./tools/build.ps1 -Target http` executes 96 native Pascal
HTTP checks: empty profiles, generation without tools, missing-tool diagnostics,
invalid-config recovery, late browser-only configuration, byte-identical Unicode
generation, page/reusable/derived-primitive views, full browser/native builds,
instance overrides and Unicode property diagnostics. The same harness runs over
loopback or a LAN URL and verifies same-origin admission / cross-origin rejection.
Long supplementary page/definition/instance IDs survive percent-encoded queries;
overflowing IDs and missing reusable paths are rejected before compilation.
Isolated view sources preserve original IDs and include only their admitted
scope and required definitions. Run browser fixtures after HTTP compilation;
the service currently has a serial compiler worker.
The harness restores the accepted machine profile after its temporary fixtures.
`-HttpURL` selects
another local service. Stop the foreground server before rebuilding its executable
on Windows, where a running executable cannot be overwritten.

`./tools/build.ps1 -Target visual` runs the native journeys and paints light/dark
desktop and narrow LCL samples into PNGs in the native output directory. Win32
needs a visible native hierarchy for painting; the temporary capture windows are
placed outside the desktop and freed immediately. These captures verify layout
and theme projections, including an instance with a customized caption and added
action and a caller-selected palette/font/radius. Actual captured pixels verify
surface, border, accent and input focus/blur colors. The harness also checks that
the narrow option captions fit. Capture failures report a failing exit code
instead of opening an exception dialog. They do not establish physical input or
display scaling.

Browser `visual.html` renders the same sample and reports 15 computed-style/layout checks
in its `data-nyx-visual` / `data-nyx-visual-checks` attributes. Query flags
`?theme=dark`, `?theme=custom` and `?parts=1` select the native fixture counterparts;
capture it at 900×900, or use `?narrow=1` with a 390×900 screenshot. The fixture
reports its actual viewport and page widths to distinguish layout from cropping.
Browser builds compile/stage this Pascal fixture
alongside the shared tests and Studio. See [themes](themes.md) for the public API
and capability limits.

`./tools/build.ps1 -Target catalog` runs the Pascal reference generator and emits
`build/catalog-reference.md`. The reviewed snapshot is
[docs/components-reference.md](components-reference.md), with all 75 kinds,
root property defaults/constraints and compound named paths/events. Copy an
accepted generated artifact to that snapshot when catalog/schema contracts change.

Unix/macOS packaging, CI toolchain pinning, widgetset-specific validation and
release artifacts remain open in the [delivery task](../TODO/NS-6_delivery_01.md).
The current PowerShell launchers are validated on Windows.

Core and browser builds also run forty deliberately invalid Pascal fixtures in
`tests/compile_fail/`. These must fail with the expected type diagnostics for
configuration, distinct references, scalar domains, event payloads and bindings.
Unexpected success or an
unrelated compiler failure fails the build. See [typed configuration](fluent-api.md).

`callbacks.html` exercises 79 shared descriptor/schema/inspector/source checks.
`callback-controls.html` executes a native-emitted companion through real browser
controls; `-Target lcl` emits that fixture independently and runs its native
counterpart. These include multiple focus registrations, inherited reusable
callbacks, remount identity, label clicks, actual framed memo clicks and compiled
Ctrl+Enter key-down/key-up callbacks, consumption and repeat refusal. Native
checks also cover a custom click handler replacing the view mid-dispatch.
`authoring.html` exercises the actual Studio property/event/source workflow:
54 desktop checks and 54 in the exact-390-pixel iframe at `?host=1`. The shared
native Studio harness passes 49 authoring checks; it still rebuilds after callbacks
return and does not establish a complete standalone native Studio controller.
HTTP evidence includes accepted handwritten helpers and callback companions for
both targets at application, page and reusable scope. Run their browser artifacts
to verify executed callbacks, alongside the native artifact execution in the HTTP
harness. Current logs and limits are recorded in [WORK](../WORK.md).

`scheduler.html` executes 52 browser scheduler/registration checks; the native
counterpart passes 55 with real workers. Both exercise explicit failure for late
keyboard consumption. The real-control journey has 35 event checks per adapter,
including current text, ordered origin/compound routing, composition bypass,
repeats, consumption, focus loss and navigation cancellation. Native CN messages
also verify consumed button activation while the existing Space/Enter journey
continues to exercise unhandled default behavior. Browser synthetic key events
prove listener routing/default prevention, not trusted operating-system input.

`projects.html` executes 39 portable paired-file admission/resolution/recovery
checks. The maintained native/core build runs the same cases plus real filesystem
transactions, external-edit conflicts and interrupted-save recovery: 60 checks.
`studio-projects.html` uses actual File objects, FileReader, Nyx controls and HTTP
save/open; `?host=1` runs that journey in an exact 390-pixel frame. Each browser
journey passes 21 checks, including backed-up conflict choices and production
browser recovery. Test storage values are restored and application output targets
remain optional. The HTTP harness now passes 111 checks, including 15 paired-project
cases. Native heap tracing for the project suite reports zero unfreed blocks.
See [project files](studio-state.md#paired-projects-and-recovery) for transaction
semantics, budgets and remaining native-controller limitations.

`palette.html` exercises optional List/Grouped discovery, intent/group/description
search, touch-readable Details, empty-state reset, independent preferences,
mounted preview retention and undo: 25 desktop checks and 24 at `?host=1` in the
exact 390-pixel frame. `?show-host=1` leaves a real grouped Studio frame mounted
for visual review. The journey also reads the selected component's Inspector help
and checks that it fits both viewport sizes. Shared fixtures add 103 metadata/
query/presentation checks; current full-suite counts are recorded in WORK.md.
The actual native authoring journey includes eight palette checks and reads
context help through a real LCL label. Descriptions,
intent groups and labels also appear in the Pascal-generated catalog reference.

`nyx_managed_source_tests.lpr` provides a focused native/heap entry point for the
33 shared managed-source cases. The full shared suite also executes these on
pas2js. Native core emits `nyx.managed.view.pas` with deliberate specialized
locals, Unicode comments, constant expressions, a reusable definition and a
handwritten callback stub. Both compilers execute that companion and compare its
design; browser reconstruction passes 34 checks. Isolated source admission, exact
paired history, name collisions, metadata order and failed visual publication
are covered. General structural Pascal editing and large-document performance
remain with their original owners.

`tools/build.ps1 -Target grid-navigation` consumes an exact previously exported
ordinary MCP table application, checks actual LCL input/retirement and emits the
browser consumer plus Pascal trusted-key observer. Use `-GridSourceDirectory`
with a retained export directory. An explicit
`-DesignerMCPConfig <config.toml> -GridSourceDirectory <new-source-directory>`
first runs the authenticated Pascal companion in a new owned workspace; an
existing destination refuses. Layout and data binding currently form two separate
paired operations. Without that opt-in, the build creates no project, opens no
listener and changes no enrollment.

Serve only its three browser artifacts from an explicitly owned artifact child.
Run `nyx_grid_navigation_observer.exe` with the loopback grid URL, new evidence
directory and CSS width for actual host arrows/modifiers, Enter/F2,
Escape and Tab/Shift+Tab. Width 390 uses a 640-pixel-high host; desktop uses 900.
The observer changes runtime controls, never document/source composition. Native
PNG and checked input logs are under `build/grid-navigation/maintained/`; see
[the grid packet](../WORK.md#current-return-path-bound-grid-cell-navigation--2026-10-07)
for scope and qualification limits.

`tools/build.ps1 -Target selection -BrowserOutput <staging-directory>` runs the
actual LCL collection-selection journey, prepares its crafted companion, repeats
the native journey against that compiled source and compiles the same pas2js
consumer. Serve `selection.html` and run it through `nyx_browser_capture` for the
executed browser result. The fixture covers immutable scoped membership,
focus/anchor, modifier clicks/keys, consumed defaults, collapsed branches,
read-only/disabled behavior, navigation revocation, Studio handler history and
bounded agent discovery. Native protected virtual mouse/key paths and browser
synthetic host events exercise real controls; they do not establish trusted
hardware input or full accessibility conformance. The compiler matrix also
rejects string-valued selection modes/actions on both compilers.

`tools/build.ps1 -Target project-workspaces -BrowserOutput <staging-directory>`
stages checked portable ownership/presentation fixtures, actual LCL Agents
controls, the native semantic/browser coordinator and Studio/server artifacts.
It does not start a listener or mutate an editor. Execute `workspaces.html` and
`workspace-view.html` over HTTP too; a native pass does not establish pas2js
ownership or target presentation. The maintained MCP journey uses an explicitly
owned fixture configuration and records exact project frames, both compiler
sources and a selective PNG. See [project sessions](studio-agents.md#concurrent-project-sessions)
for arguments, lifetimes, limits and remaining close/native-controller gates.

`tools/build.ps1 -Target native-studio` builds the standalone LCL editor without
requiring a server or application compiler configuration. The optional
`-VerifyNativeStudioService` runs actual editor/MCP integration against an
explicit private service fixture:

```powershell
./tools/build.ps1 -Target native-studio -VerifyNativeStudioService `
  -HttpURL <loopback-editor-origin> `
  -NativeStudioServiceMCPConfig <private-build-config.toml> `
  -NativeStudioTestContexts <owned-test-contexts.json>
```

The context file contains `first` and `second` objects, each with an exact
`workspace` reference and `revision`. Both must be distinct non-primary owned
test projects without pending drafts. The Pascal consumer saves original paired
snapshots, appends nonce-owned English review pages, uses real MCP/actual native
controls and restores the exact original pairs through reviewed root cleanup.
It retains the primary frame. Histories keep the added ordinary test commands.
Without the context file it creates two owned projects, subject to the eight-slot
budget; project closure is a separate operator action and remains unqualified.

Artifacts and the owned-root manifest live under `build/native-studio/service-current/`.
On failure inspect the same project/revision and retained originals before retry;
never reset the service or substitute descendant deletion for root cleanup.
The fixture's explicit `cleanup-owned` mode accepts current revisions plus
`root` and each context's `original` snapshot path, performs fresh actor-bound
review/apply and checks exact pairs. It refuses protected pending drafts. Browser
`agent-bridge.html?workspace=<owned-reference>` separately qualifies protected
attachment/coalesced typing and restores its accepted pair after its private draft.

The optional native compiler journey consumes an unchanged English semantic
export and an explicit private machine profile:

```powershell
./tools/build.ps1 -Target native-studio -VerifyNativeStudioCompiler `
  -DesignerSourceDirectory <semantic-export-directory> `
  -NativeStudioCompilerProfile <private-output-profile.json> `
  -NativeStudioArtifactDirectory <existing-artifact-serving-build-root> `
  -HttpURL <loopback-editor-origin>
```

Its [Pascal consumer](../tests/nyx_native_build_tests.lpr) constructs a suspended
private protocol engine, qualifies operator admission/profile failures and uses
real compiler jobs through actual native controls. The shell creates a unique
ignored repository with links to the existing library directories. It never
starts or replaces a listener. The supplied artifact-serving root must already
exist; only that test's immutable manifest files are copied there. Profiles,
enrollment and active documents of the existing listener are untouched.

The maintained fixture requires the semantic designer-review/designer-reply
export used by the native authoring journey. It exercises browser page/reusable
compilation, native application/view compilation, owned Run/Stop, current preview
reload, compiled reply controls and a deliberately invalid retained Pascal
helper. Its source navigation check uses an exact line/column. Captures and
compiler status snapshots live in the unique repository under
`build/native-studio/compiler-current/`; terminal evidence belongs to WORK.md.
Native host messages establish actual control routing, not hardware/IME or
assistive-technology behavior. Direct protocol results and actual HTTP artifact
downloads do not qualify the new editor HTTP deployment.

For a failed compiled-input investigation, the same consumer accepts optional
`probe-input` as its sixth argument. Its first argument is an existing owned
compiler repository containing `lcl-application-status.json`; the remaining
profile/source/artifact paths and HTTP origin stay explicit. It downloads that
retained admitted artifact and exercises real native controls, byte-mismatch refusal, cancellation
and owned process retirement without repeating compiler/editor journeys. This
mode does not establish live source currentness or editor reload. It identifies
the exact document-titled native form, excluding Lazarus helper windows, and
uses the custom button's native Space-key path. Evidence belongs to WORK.md.

Optional `diagnostics` instead exercises only the guarded operator protocol,
actual native source editor, one real compiler error, exact supplementary
Unicode location, stale-source refusal and retirement. It preserves the same
five explicit arguments and skips successful compilation/preview journeys.
Editing ranges are zero-based scalar offsets in the actual control text;
source byte indices and accepted LF text are distinct coordinate domains.

Coalesced draft capture has a separate optional consumer:

```powershell
./tools/build.ps1 -Target native-studio -VerifyDraftCapture
```

It executes the guarded portable editor protocol with a deterministic owned
clock/reply queue, then the real native Studio memo/timer journey at the original
128/512/2048-control source sizes. Exact draft/base pairs, accepted command order,
history, conflicting/late replies and retirement are checked. Its browser
counterpart and `draft-capture.html` host are compiled/staged under
`build/draft-capture/browser/`; the command starts no listener. Serve that host
only through an already permitted qualification environment to establish browser
execution. Heap-checked fixture durations include tracing and in-process protocol
admission; they are not release typing/network latency measurements. Current
ordinary measurements and limits belong to WORK.md.

The full native consumer now passes all three original sizes, retaining every
descendant and exact source bytes. The public hierarchy uses one bounded typed
Nyx tree; the canvas keeps its full logical extent while allocating safe physical
geometry. Explicit hierarchy navigation reveals its exact control, while ordinary
selection painting preserves independent scroll/focus. This resolves the recorded
81963-pixel height failure. Traced fixture timings do not establish comfortable
release editing or full browser/LCL parity.

The maintained logical geometry and mixed-control qualification is available as:

```powershell
./tools/build.ps1 -Target native-studio -VerifyLogicalViewport
```

It stages both Pascal consumers and matched browser runtime before executing
native checked/heap-traced geometry, 2048-control navigation, bottom/nested memo
editing, desktop/390 resizing, actual native scrollbar/wheel events and candidate
refusal/retirement. Review captures use English; dedicated Unicode/source-size
inputs remain technical qualification. Browser compilation retains its separate
permitted-host execution gate. No listener is launched by either option.

The maintained detached visual/structural source qualification is available as:

```powershell
./tools/build.ps1 -Target native-studio -VerifyDesignSource
```

It executes immutable ticket/wire, exact paired publication, handwritten text,
draft/load/creator/navigation guards and every supported structural command. The
actual native scheduler fixture also qualifies presentation exceptions, retired
project loads with matching IDs, current pending fields and save/export guards. An
exported admitted companion is compiled and executed against its exact design.
An independent canvas/default/instance companion is exported and compiled in a
separate unit directory, then compared with its exact design. Actual native
canvas inputs exercise active/waiting/coalesced proposals, typed bound defaults,
unbound nested reusable parts, rejection restoration, retired mounted fields,
source drafts, focus/caret and paired Undo/Redo. Exact renderer restore groups
qualify atomic refusal and preservation of other controls' drafts.
Moved handwritten configuration/extension statements retain their ownership and
execution order. The actual retained-projection consumer exercises native control
identity, independent input, caret, callbacks, fresh refusal and balanced sizing.
Actual native inspector/title/palette controls exercise FIFO/coalescing, paired
Undo/Redo, field focus/caret, cancellation/staleness and detached retirement at
the unchanged 128/512/2048-control source sizes. English desktop/390 captures stay
separate from private Unicode qualification. The default checked configuration
enables heap tracing; release stages independent optimized evidence. Measured
fixture durations do not establish comfortable release latency. Browser Studio,
the Pascal worker and portable checks are compiled/staged under
`build/design-source/maintained/browser/`; execution retains its permitted-host
gate. The option launches no listener and changes no application profile or
enrollment. Current evidence and broader source-synchronization gaps belong to
WORK.md and the original source-synchronization owner.

For bounded native profiling, compile with `-dNYX_STUDIO_PROFILE`. It emits only
fixed UI/native stage names and elapsed milliseconds on the owning UI thread;
ordinary builds contain no profiling clocks or output. The compiled
`nyx_design_source_controls` fixture optionally accepts a second argument of
`128`, `512` or `2048` after its owned artifact directory. This selects one
unchanged original workload for diagnosis. Omitting it still qualifies all three
sizes; a selected run cannot establish omitted-size or complete-target acceptance.

The focused scalar Project/Bindings inspector qualification is available as:

```powershell
./tools/build.ps1 -Target state-inspectors
```

It runs typed private-wire/admission checks, queue presentation/retirement
regressions, real standalone native state/binding controls, the maintained
synchronous authoring consumer and queued canvas controls. The native journey
covers retained explicit Rename drafts, rapid/coalesced scalar input, rejection,
escaped supplementary/NUL values, exact paired history, captured binding owners,
inherit/clear, independent Pascal drafts and project replacement. English desktop
and 390-pixel captures live under `build/state-inspectors/controls/`.
Current browser Studio, the matched compiled Pascal worker, portable checks and
an asynchronous DOM control journey are staged under its `browser/` directory.
Compilation is not browser execution or visual acceptance. The command starts no
listener, changes no enrollment/profile/user project and replaces no live service.
Full responsiveness, browser/native parity and wider layout/accessibility remain
with their original task owners.

The focused callback inspector qualification is available as:

```powershell
./tools/build.ps1 -Target event-inspectors
```

Pascal fixtures qualify typed event requests/replies, reviewed removal, pending
policy presentation, guarded handler navigation, independent drafts and paired
history. It runs the shared regressions, actual standalone native callback
controls and the legacy authoring consumer, and compiles/executes the exported
companion's reconstruction. English desktop/390 captures belong to its `controls/`
directory. Current browser Studio, the matched Pascal worker, portable checks,
an asynchronous DOM journey and compiled reconstruction stage under `browser/`.
These staged browser consumers still need execution through a permitted host.
The command starts no listener and changes no enrollment, profile or live project.
Exact current execution evidence and remaining scope are in [WORK.md](../WORK.md).

The focused structured collection inspector qualification is available as:

```powershell
./tools/build.ps1 -Target collection-inspectors
```

It runs all seventeen typed collection operations through independent replay,
strict private descriptors and exact source/design publication. Native controls
exercise pending input/focus, row/default/column ownership, structural locks,
reusable scope, tree parent mapping, drafts, paired history and retired loads.
The exact exported companion compiles and drives a real native table callback;
runtime stores remain independent of document defaults and other applications.
Source regression also checks that qualified collection units are imported only
once through repeated edits. English desktop/390 captures are in `controls/`.
Studio, its matched Pascal module worker and portable/DOM/generated consumers
stage in `browser/`, with four English collection qualification hosts. Current
browser execution needs its permitted host; the command launches no listener,
changes no enrollment/profile and replaces no live service or observing project.
[WORK.md](../WORK.md) records current individually executed consumers separately
from the maintained composite command and the original acceptance gates.

The optional Win32 transport consumer needs no Studio server, project, enrollment
or application compiler profile:

```powershell
./tools/build.ps1 -Target native-studio -VerifyTransportDeadlines
```

Its maintained [native consumer](../tests/nyx_transport_deadline_tests.lpr) owns
a bounded raw Pascal TCP peer on an OS-selected loopback port. This is a scripted
byte producer, with no Studio/MCP authentication or design state. Real native
editor/preview adapters exercise stalled headers, body trickles, upload
backpressure, canceled receivers/timers, deferred replacement and expired queued
admission. A synthetic artifact qualifies preparation only and is never launched.
The matched pas2js [browser consumer](../tests/nyx_browser_transport_tests.lpr)
exercises real XHR deadlines, copied/invalid policy admission and cancellation.
The maintained Pascal CDP host reads bounded terminal attributes and uses actual
elapsed time. Accelerated virtual-time screenshot capture is unsuitable for
these timing assertions.

The command compiles the native socket unit with stable FPC as well as the LCL
consumer. Execution qualifies the current Win32 LCL toolchain; it does not qualify
other compilers/widgetsets. Logs and bounded browser status live under
`build/transport-deadline/`. Failure logs stay retained. This source candidate
does not deploy an updated editor route or cancel an already admitted build job.

The focused semantic collection workflow is available as:

```powershell
./tools/build.ps1 -Target collection-bindings
```

Pascal owns query/admission/refusal assertions. The native journey checks typed
named schemas/fields/scoped rows, domains, exact Unicode/numbers, view ownership,
revision/actor/receipt guards and one paired Undo step. A focused consumer checks
a reusable clear mask's exact inherited contract. Offline discovery executes the
actual MCP catalog builder without a listener or credential refresh. The exported
companion is compiled unchanged and drives actual native table edits with
independent runtime stores. Matching browser programs, RTL and English
`agent-collections.html` / `agent-collection-controls.html` hosts stage under
`browser/`; execution still requires an admitted HTTP host. The focused inheritance
program also compiles for the browser; its masked-contract cases are included in
the main semantic journey. Output defaults to ignored `build/collection-bindings/`.

The command launches no listener and replaces no service, enrollment or live
project. Current individual evidence in
[WORK.md](../WORK.md#semantic-structured-collection-commands--2026-10-05) passes 96
semantic and 14 masked-inheritance checks per native compiler, 57 discovery
checks and 11 exact compiled native control checks. The maintained composite has
parser evidence and was not repeated after those individual consumers. Heap-traced
large-domain checks establish semantics, not release latency. Browser execution
and updated authenticated observing deployment remain unqualified; seven installed
pas2js RTL warnings stay visible.

The maintained semantic Pascal import workflow is:

```powershell
./tools/build.ps1 -Target pascal-imports
```

It runs Pascal lexical and semantic query/group/refusal checks, the actual offline
MCP discovery builder and an unchanged exported companion with actual native memo
input. The helper requires the newly authored Math/SysUtils imports, so execution
checks unit resolution as well as source admission. Supplementary Unicode and
rejected input preserve exact physical/model values. Browser lexical/semantic/
control consumers and matched RTL stage with English `import-lexical.html`,
`import-edits.html` and `import-controls.html` hosts. Default output is ignored
`build/pascal-imports/`; `-BrowserOutput` selects a separate staging directory.
The command launches no listener or configuration refresh. Current execution
passes 17 lexical, 31 semantic, 22 discovery and nine compiled native checks, with
zero native leaks/owned warnings. Browser compilation retains seven installed
RTL warnings. Serve the staged hosts only through an admitted HTTP host to qualify
browser execution; that and authenticated updated observing deployment remain
at the recorded gate. See [evidence](../WORK.md#semantic-pascal-imports--2026-10-05).

## Semantic Pascal helper qualification

```powershell
./tools/build.ps1 -Target pascal-routines
```

The maintained command executes checked native lexical and semantic consumers,
actual offline nineteen-tool discovery and an unchanged emitted companion with
real native memo input. An MCP group edits a qualified policy function and a
global caption helper; the actual compiled callback uses the new four-character
limit. Dedicated Unicode input verifies scalar counting and rejected-value
retention. English demo text remains independent of Unicode qualification
comments. Candidate admission alone does not prove helper compilation/execution.

Native/export/LCL/browser artifacts live under `build/pascal-routines/`;
`-BrowserOutput` selects separate browser staging. The command compiles matching
pas2js consumers, copies the matched RTL and stages three English hosts. It starts
no listener and changes no live project, service or personal configuration.
Current browser execution and updated authenticated observation keep their
recorded host gate. General declaration/class/full-unit authoring and complete
source synchronization retain original acceptance requirements.
See [evidence](../WORK.md#semantic-pascal-helpers--2026-10-05).

## Semantic helper declaration qualification

```powershell
./tools/build.ps1 -Target pascal-declarations
```

The maintained command executes native lexical/semantic checks, actual offline
MCP discovery and an unchanged emitted companion with mounted Win32 memo input.
One semantic group creates private/public helpers, updates a retained class
method and removes an obsolete public helper. Actual compilation/execution
qualifies helper signature/result and related caller changes as one paired
operation, then the new five-character policy and parameterized English caption; dedicated Unicode
input verifies scalar limits and exact rejected-value retention.

Native/export/LCL/browser artifacts live under `build/pascal-declarations/`;
`-BrowserOutput` chooses separate staging. Matching pas2js consumers, RTL and
three English hosts are staged. No listener, deployment, configuration refresh
or active-project edit occurs. Browser runtime/updated authenticated observation,
wider class/full-unit authoring and full source synchronization retain
their existing gates. External callers/type correctness need ordinary compiler
diagnostics. A separate outdated public-helper caller must produce the intended
named parameter/type diagnostic on both native compilers and pas2js; a missing
unit or unrelated failure does not count. See
[evidence](../WORK.md#semantic-helper-signatures--2026-10-05).

## Source workspace and expanded editor

```powershell
./tools/build.ps1 -Target source-editor
```

Pascal fixtures qualify strict per-project presentation version 3 on both native
compilers, including exact migration from version 2. The actual Win32 controller
exercises Source/Compiler messages switching, Expand/Close/Escape, retained memo
identity/draft/selection and native window resizing. Its desktop and narrow
captures, native Studio and browser Studio/module worker/portable counterparts
are staged under `build/source-editor/`; `-BrowserOutput` selects isolated browser
staging. The command also compiles the maintained Pascal browser source-workspace
journey and stages `source-editor.html`. Serve the staged directory over HTTP
with its matched runtime, then open that host; `?host=1` runs the same journey
inside an exact 390-pixel viewport. Success publishes `data-source-editor="passed"`
and 30 checks. It uses real DOM controls and synthesized cancellation, preserves
the same textarea/draft/range and opts out of recovery and agent connection.
Desktop/narrow captures qualify Expand, tab refresh and Close/focus return;
they do not establish physical phone keyboard or assistive-technology behavior.
This build command launches no listener, changes no MCP enrollment and edits
no active observing project. The separate staged current service now executes
the journey; replacing the observing LAN release remains rejected by automatic
approval review. See
[evidence](../WORK.md#source-workspace-and-expanded-editor--2026-10-05).

## Portable size constraints

```powershell
./tools/build.ps1 -Target constraints -BrowserOutput build/constraints/staged
```

The maintained command exercises copied typed bounds, weighted allocation,
bounded semantic mutation, atomic refusal and paired Undo/Redo on both native
compilers. Their exported design/source/pair files must match byte for byte.
It compiles that exact source unchanged, measures actual Win32 controls and
retains editor text/focus/selection while limits, visibility and host width change.
An ordinary Win32 Studio journey edits/unsets limits through mounted Nyx controls
and the isolated processor, retaining its source editor and handwritten source.

Matching browser contract/control consumers, Studio, its module worker, RTL and
two English hosts are staged. Compilation alone does not qualify browser input,
phone observation or full target parity. The command launches no listener,
changes no personal MCP configuration and leaves observing projects/releases
intact. See [layout](layout.md) and
[evidence](../WORK.md#portable-size-constraints--2026-10-05).

## Reusable resize grips

```powershell
./tools/build.ps1 -Target resize -BrowserOutput build/resize/staged
```

The maintained command runs copied snapping/bounds, public pointer/key behavior,
bounded semantic mutation, paired history and isolated resize wire/publication
on both native compilers. Their exported design/source/pair files must match
byte for byte. It compiles that exact source unchanged and exercises mounted
Win32 dimensions, retained memo identity/text/selection/focus and rejected
candidate retention. Ordinary native Studio consumes its actual registered
grip callbacks and existing detached processor; desktop/390-pixel captures are
written under `build/resize/`.

Current shared checks pass 56 per compiler, unchanged compiled controls seven
and native Studio 70, with zero checked leaks. Matching browser contract/control/
projection and proposal consumers, Studio, its module worker, RTL and three
English qualification hosts are staged separately. The dedicated
`resize-preview.html` consumer publishes `data-resize-preview="passed"` and its
check count; `?host=1` runs its exact 390-pixel iframe. Both executed paths pass
47 actual DOM checks, retaining English input/range/focus, clipping large
proposals and removing owned paint/listeners on retirement. These now include
public canvas buttons, exact precision-key proposals, scope reuse and old-DOM
refusal. Native callbacks separately qualify default grid snapping, moving
capture, abandoned-scope retirement and one paired Undo. The ordinary
contract/control hosts use `data-result="passed"`; successful compilation alone
cannot establish any runtime result. This command starts
no listener, refreshes no personal MCP configuration and leaves observing
projects/releases intact. Ordinary browser Studio gesture execution, updated
phone observation and full target parity retain their existing gates. Native
proposal captures compose actual standard-panel painting over Win32 form
`PaintTo`; physical screen capture was unavailable. See
[designer views](designer-views.md#reusable-resize-grips) and
[evidence](../WORK.md#direct-canvas-resize-handles--2026-10-05).
