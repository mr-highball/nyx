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

## Remaining editor integration

The execution prerequisite does not implement general source Apply. Compiler
results still need fresh paired source/design/context admission, one undoable
editor transaction and preservation of handwritten expressions during later
visual/structural edits. Ordinary HTTP/MCP execution and browser worker lifecycle
integration belong to the existing source/compiler/workflow tasks. They must not
silently admit a projected tree through the strict importer or overwrite helpers
with a literal regeneration. Full native rendering/Studio parity and preserving
LAN delivery retain their existing acceptance requirements.
