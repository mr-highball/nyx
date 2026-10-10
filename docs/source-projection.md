# Compiler-executed Pascal construction

[Architecture](architecture.md) · [Current work](../WORK.md) ·
[Source synchronization task](../TODO/NS-1_codegen_01.md)

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

Current qualification adds **32 guarded publication checks on FPC and actual HTTP
pas2js**, separately from the existing 55/33 execution checks. These qualify the
session/source/history contract, not physical Studio Apply controls or installed
HTTP/MCP execution. The existing managed-source regression passes 33 and ordinary
project-import admission passes 590, with clean native heap traces.

## Remaining editor integration

The local paired boundary does not yet connect ordinary Studio Apply or semantic
HTTP/MCP dispatch to compiler execution. Visual/structural changes still need
expression-preserving reconciliation. An executed workspace currently raises
`ENyxSourceExecutionRequired` before regenerating changed meaning; ordinary title
commands roll back their full pair/history. This preserves source while that
required writer is implemented, without claiming full WYSIWYG synchronization.
General file/recovery re-admission must also use explicit compiler evidence.
These remain original source/compiler/workflow tasks. Full native rendering/
Studio parity and preserving LAN delivery retain their acceptance requirements.
