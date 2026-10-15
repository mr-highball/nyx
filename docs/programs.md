# Typed Pascal behavior programs

[Task owner](../TODO/NS-1_canvas-behaviors_01.md) ·
[Canvas](canvas.md) · [Current evidence](../WORK.md)

Nyx compiles an explicit Delphi-dialect behavior language into immutable Pascal
instructions and interprets those instructions on FPC and pas2js. This supplies
custom runtime behavior without another language runtime. It does not send script
text to the native application compiler or infer behavior from control properties.
The current language version is `NyxProgramLanguageVersion = 1`.

## Direct execution

```pascal
uses
  nyx.text, nyx.programs.types, nyx.programs.contract, nyx.programs;

const
  CGreeting: TNyxText =
    'begin' + #10 +
    '  Caption := ''Hello, '' + Trim(Name) + ''!'';' + #10 +
    'end.';
var
  LGreeting: INyxProgram;
  LInputs: INyxProgramValues;
  LResult: INyxProgramResult;
begin
  LGreeting := CompileNyxProgram(CGreeting, NyxProgramContract
    .Input(NyxProgramSymbol('Name'), NyxProgramType(npkText))
    .Output(NyxProgramSymbol('Caption'), NyxProgramType(npkText)),
    NyxProgramLimits.Steps(10000).AllocationBytes(1048576));
  LInputs := NyxProgramValues
    .WithValue(NyxProgramSymbol('Name'), NyxProgramValue('Ada Lovelace'));
  LResult := LGreeting.Run(LInputs);
  { LResult.Outputs.Value(NyxProgramSymbol('Caption')).AsText is the greeting. }
end;
```

`CompileNyxProgram` raises `ENyxProgram` for a structured language refusal.
`InspectNyxProgram` instead returns `INyxProgramCompilation`: one complete program
or its first diagnostic, never an executable prefix. Unexpected implementation
exceptions are not recast as syntax errors. Both receipts and programs expose
`LanguageVersion`. Diagnostic spans refer only to that exact source: zero-based
UTF-8 byte start/count, one-based Unicode scalar line/column, CRLF as one newline.
EOF may have a defined zero-length span; metadata refusals may have no source span.

Programs own immutable source, contract, constants and instructions. Each call
owns its activation frames, operands, counters and proposed edits. Reentrant calls
share code, not variables. Values, argument vectors, named frames, options and
results use managed interfaces; alternative handle/query implementations do not
need to inherit Nyx's implementation classes. Immutable `Copy` retains the value;
JSON and byte getters detach their payloads. Fluent metadata/frame derivation
retains independent membership and ordering.

## Language version 1

| Construct | Supported meaning |
| --- | --- |
| Entry | Optional `program Name;`, declarations, `begin ... end.` |
| Declarations | Typed `var` lists, literal `const` values with optional type |
| Routines | Named procedures/functions, value or read-only `const` parameters, nested lexical capture, recursion and exact `forward` signatures |
| Result | Function name or `Result`; `Exit` and `Exit(value)` |
| Statements | Assignment, routine calls, blocks, `if/then/else`, `while/do`, `repeat/until`, `for/to/downto/do`, `Break`, `Continue` |
| Families | `Boolean`, signed 32-bit `Integer`, finite `Double`, `String`, `JSON`, `Bytes`, explicitly declared opaque handle types |
| Operators | Typed `+ - * / div mod`, comparisons, unary signs and Boolean `not/and/or` with short-circuiting |
| Text | Pascal quoted text with doubled quotes and contiguous `#decimal`/`#$hex` Unicode scalar escapes |
| Comments | `//`, nested `{...}` and `(*...*)`, bounded by the nesting limit |
| Built-ins | `Length`, `IntToStr`, `FloatToStr`, `Copy`, `Trim`, `Abs`, `Min`, `Max`, `Ord`, `Chr` |

Identifiers are ASCII Pascal names, case insensitive, with authored spelling
retained. Runtime text remains strict Unicode, including supplementary characters
and NUL. Text comparison uses scalar ordinal order. `Length(String)` counts
scalars, `Copy` takes a positive one-based scalar start and nonnegative count,
`Ord` accepts Boolean or exactly one scalar, and `Chr` refuses surrogate values.
`Length` also accepts bytes and JSON arrays/objects; JSON scalars refuse.

Arithmetic checks integer overflow, division by zero and finite numeric results.
Integer-to-Double widening occurs only at typed numeric boundaries; text needs
an explicit conversion. Inputs, constants, `const` arguments and active `for`
counters are read-only, including writes through a nested lexical capture. A
`for` end expression is evaluated once. Unassigned local/result reads refuse.
Every output must be assigned before a complete result can be returned. Extra,
missing or differently typed invocation inputs refuse.

This is a defined subset, not a complete Pascal compiler. Units/includes/compiler
directives, classes/records/arrays declared in source, generic types, first-class
routine values, anonymous functions, `var/out` parameters, exception statements,
bitwise integer operators and general constant expressions are not supported.
JSON/byte values can cross declared host boundaries; source does not gain an
implicit JSON path language or arbitrary object/member access.

## Explicit host authority and proposed edits

`TNyxProgramContract` declares typed inputs/outputs and named host routines.
`NyxProgramQuery` describes a side-effect-free value query against an explicitly
injected `INyxProgramQueries` snapshot. `NyxProgramPropose` describes a void host
call that appends an immutable typed `INyxProgramProposal` to the result journal.
It never calls a live mutation host during interpretation.

For example, a creator can declare `Selected(): TCanvasItem` and
`Move(Item: TCanvasItem; X, Y: Double)`, requiring `npcSceneRead` and
`npcSceneEdit` respectively. The host supplies its independently retained opaque
`INyxProgramHandle`; a string cannot reconstruct that handle or its authority.
The interpreter can retain, pass and compare handle identity, but cannot
dereference platform objects. These are extensible signatures, not preinstalled
scene/camera/resource adapters.

`NyxProgramRun.Grants(...).Queries(...).Cancellation(...).Clock(...)` builds
immutable runtime options. Fresh options grant no capabilities and retain no
host service. Built-in capability names use `TNyxProgramCapability`; extensions
use distinct validated `TNyxProgramCapabilityRef` identities. Unlike Pascal
symbols, capability identity is exact and case sensitive. All referenced grants
are checked before any host query, including calls in unreachable compiled
branches. Declaring a capability or importing its name never grants it.

Queries must be bounded, side-effect free and safe on the selected execution
thread. They must return the exact declared family. Nil/wrong-family results
refuse. Scope revocation and any host object lifetime remain the host's explicit
contract. An unbounded external query cannot be preempted by the interpreter.
Failures discard this invocation's unfinished journal. The caller must admit a
complete result through an owning revision/generation-guarded adapter; a journal
has no implicit commit, file access, network service or process execution.

## Limits, cancellation and scheduling

`NyxProgramLimits` creates initialized, caller-tunable limits:

| Fluent limit | Default |
| --- | ---: |
| SourceBytes | 262,144 UTF-8 bytes |
| Symbols | 4,096 |
| Instructions | 65,536 |
| Routines | 256 |
| Parameters | 64 per routine |
| Nesting | 64; parser ceiling 256 |
| Steps | 1,000,000 charged operations |
| CallDepth | 64 active frames, including entry |
| StackValues | 4,096 |
| ValueBytes | 1,048,576 |
| AllocationBytes | 67,108,864 cumulative accounted bytes |
| ResultBytes | 16,777,216 |
| Proposals | 1,024 |
| Milliseconds | 0, elapsed-time limit disabled |

Metadata import checks port/routine/parameter counts before allocating their
vectors. Compiled admission checks caller metadata before copying it. Imports
retain declaration order and reject differently cased duplicate Pascal names.
Source and compiler structure have separate limits. Runtime counters conservatively
account value/frame/result retention; they are not exact process heap or OS memory
caps. Host snapshot construction and host internal costs remain caller owned.
The result reports charged `Steps` and `AllocatedBytes`, not a latency benchmark.

Cancellation/time checks occur at entry, at least every 64 charged operations,
and before complete result return. Scalar comparison/string built-ins charge
their loops; host calls obey their separately bounded contract. A positive elapsed
limit requires an injected `INyxProgramClock` on that execution thread. Missing,
backward and nonfinite clocks refuse; the core has no implicit wall clock or
platform dependency. Cancellation is cooperative, not forced thread termination.

`TNyxProgramExecution.Submit` in `nyx.programs.execution` captures application
state on its UI scheduler. `INyxProgramStateBindings` maps declared inputs and
outputs to typed Boolean/integer/number/text state references. Execution owns
its snapshot; one ordinary `State.Apply` publishes all scalar outputs only if the
same application, store, revision and page generation remain current. Binding
validation failure, stale state, cancellation and navigation retain the previous
frame. Post-commit observer exceptions retain the existing `State.Apply` contract.
Alternative result/frame/value getters can reenter the UI: publication rechecks
ownership and cancellation after the entire output conversion, before touching
the captured store. A page retired during conversion receives no output group.

The default policy is `neUIQueue`. Native `neThreaded` uses scheduler workers.
The browser explicitly refuses native threading; a synchronous interpreter call
does not yield to other UI events, so same-turn cancellation cannot preempt it.
This scalar courier refuses programs that may propose host edits and checks the
completed journal again. It never silently drops or individually applies scene,
camera or resource changes. Those adapters remain required follow-up work.

## Persistence and current integration boundary

`INyxProgram.ToData` persists strict version/source/contract/limits metadata.
`NyxProgramFromData` reads limits first and recompiles the exact source. Neither
persisted declarations nor compiled constructors import grants, query services,
handles, scheduler objects or executable native code. Ports and host signatures
also have their own strict metadata conversions.

The English [workbench](../tests/nyx.test.programs.pas) is built with public Nyx
controls. An explicit callback compiles the edited memo, runs a nested routine
and loop, and publishes caption/count/preview width together. The actual
[control consumer](../tests/nyx_programs_controls.lpr) qualifies native and HTTP
browser state publication, independent applications, failure/cancellation and
retired page ownership. It is not a Studio program editor or a compiled-source
continuation claim.

Run `tools/test-programs.ps1 -EvidenceDirectory <fresh ignored directory>` for
checked native core, actual LCL input and both browser compilations. Supply an
independently preflighted `-StaticJobsDirectory` and `-BuildsBaseURL` to execute
the HTTP browser consumers at desktop and CSS-390. The orchestrator starts no
listener and changes no active design. Evidence and screenshots stay ignored.

Program document ownership, ordinary Studio paired editing, semantic MCP program
operations, scene/camera/resource result admission, canvas event wiring, useful
diagram behavior, production cost budgets and observing delivery remain open
under the [existing task](../TODO/NS-1_canvas-behaviors_01.md). Current engine and
workbench evidence does not close that full acceptance or establish hardware,
IME, accessibility, other browser/widgetset or complete native/browser parity.
