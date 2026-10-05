# Building Nyx

[Project](../PROJECT.md) · [Architecture](architecture.md) · [Current evidence](../WORK.md)

Product code, persistence, generation, designer state and compiler service are
Pascal. PowerShell only selects tools, passes compiler arguments and stages
matched target artifacts. No Node, npm, Python, CSS framework or remote font is
required.

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
`collection-authoring`, `source-workspace`, `agents`, `split`, `interactions`,
`named-events`, `viewport`, `catalog`, `browser`, `studio`, `lcl`, `http`,
`visual` and `all`.
The native unit cache includes compiler version and CPU/OS. LCL and pas2js
artifacts have separate output directories. Build failure propagates immediately.
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
identity/preservation, independent source-context and immutable history checks
(48 total), and
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
