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
dereference platform objects. These are extensible signatures; the built-in
canvas vocabulary and its admission boundary are described below.

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
camera or resource changes. Canvas layout uses the separate owning adapter below;
combined state/scene publication and resource edits remain required follow-up work.

## Canvas captures and complete layout publication

`nyx.programs.canvas` supplies a managed `INyxCanvasProgramSnapshot`. Capture it
on the scene's authoring thread with `CaptureNyxCanvasProgram(Scene, Camera,
SelectedResources)`. It retains immutable spatial metadata, materialized resource
values and independent identity tokens. It retains no live scene, control, view,
application or mutable resource registry. The optional registry is an explicit
allowlist supplied by the caller; omission gives an empty resource catalog.

`Snapshot.Contract` declares the following typed host functions. Pascal callers
use `TNyxCanvasProgramRoutine` and `NyxCanvasProgramSymbol` when inspecting them.
The table's names belong to the explicit script-language boundary.

| Routine | Contract and required grant |
| --- | --- |
| `SceneCount(): Integer`, `SceneItem(Index): TNyxCanvasItem` | Scene metadata, `npcSceneRead` |
| `ItemX/Y/Width/Height(Item): Double`, `ItemVisible(Item): Boolean` | Captured geometry and effective layer/item visibility, `npcSceneRead` |
| `MoveItem(Item, X, Y)`, `SizeItem(Item, Width, Height)`, `ShowItem(Item, Visible)` | Deferred placement edits, `npcSceneEdit` |
| `CameraX/Y/Zoom(): Double` | Captured camera values, `npcCameraRead` |
| `SetCamera(X, Y, Zoom)` | Deferred center/zoom, `npcCameraEdit`; zoom obeys existing camera limits |
| `ResourceCount(): Integer`, `ResourceAt(Index): TNyxCanvasResource` | Selected materialized catalog, `npcResourceRead` |
| `ResourceName/Locale(Resource): String` | Exact catalog identity metadata, `npcResourceRead` |
| `ResourceText/JSON/Bytes(Resource)` | Exact family read, `npcResourceRead`; incompatible kinds refuse |
| `ResourceNumber(Resource): Double` | JSON numeric scalar, `npcResourceRead`; text is not implicitly parsed |

Indices are zero based. Item enumeration follows captured paint order; resource
enumeration preserves the caller's registry/locale order. Repeated handle reads
have stable identity within a capture. Handles from a different capture refuse,
even when names, geometry and scene revision happen to match. A removed/recreated
item cannot revive old authority: the exact scene token and monotonic revision
must still match when results are prepared.

Materialized resources keep exact Unicode/NUL, JSON values and detached byte
storage. Capture clones catalog membership and checks the cumulative
`AMaximumResourceBytes` budget, default 16 MiB. Hosted definitions explicitly
refuse. Resolve through the existing asynchronous loader/cache first, then use
typed materialized definitions such as `NyxResourceFromBytes`; no query can start
network or file I/O or change the caller's cache policy.

For example, explicit behavior source can arrange existing controls using a
selected JSON spacing resource:

```pascal
var
  Index: Integer;
  Item: TNyxCanvasItem;
  Spacing: Double;

procedure Slide(const Control: TNyxCanvasItem);
begin
  MoveItem(Control, ItemX(Control) + Spacing, ItemY(Control));
end;

begin
  Spacing := ResourceNumber(ResourceAt(0));

  for Index := 0 to SceneCount() - 1 do
  begin
    Item := SceneItem(Index);

    if ItemWidth(Item) > 0 then
    begin
      Slide(Item);
    end;
  end;
  SetCamera(CameraX() + Spacing / 2, CameraY(), CameraZoom());
end.
```

Compile this against `Snapshot.Contract`. Invoke it with explicit grants for
scene read/edit, camera read/edit and resource read, and
`NyxProgramRun.Grants(Grants).Queries(Snapshot)`. Ordinary compiled execution
returns a journal; it changes neither the scene nor its camera.

The original `INyxCanvasScene` and `INyxCanvasWorkspaceView` interfaces remain
unchanged. Optional `INyxCanvasSceneLayouts` prepares an `INyxCanvasLayoutPlan`;
optional `INyxCanvasLayoutView` stages and publishes it with a camera. Unsupported
creator implementations refuse the missing capability rather than falling back
to repeated single-item edits.

On UI, capture `LayoutView.PresentationRevision` before preparing/dispatching the
work. `ApplyNyxCanvasProgram(View, Snapshot, Result, ExpectedPresentation, Scope)`
decodes and validates the whole journal, then asks the view to stage all target
allocations and any newly needed mounts. The final gate checks cancellation,
connected ownership, exact scene revision and exact presentation revision before
committing scene and camera without external callouts between them. Each accepted
publication advances the scene and presentation once, including an explicit
empty/no-op layout. Ordinary pan/zoom/resize/refresh also advance presentation
identity, so worker results cannot overwrite intervening view changes.

Preparation merges repeated move/size/visibility edits chronologically, preserving
intrinsic item flags beneath hidden layers. A spatial preview is independent of
the live scene. New physical controls stage hidden; original editor faces, text,
bindings and subscriptions stay retained. Failed geometry, invalid signatures,
foreign handles, stale revisions, cancellation, disconnected owners and oversized
visible target allocation leave the accepted scene/camera untouched. Invalid
proposal geometry reports that proposal's source span. Physical callbacks or
observer failures after model publication retain post-commit semantics; they do
not fabricate a rollback of an already accepted frame.

This adapter edits placements, visibility and camera only. It does not create or
delete controls, mutate resource definitions or silently drop scalar outputs.
`Snapshot.Prepare` refuses results with scalar outputs; the scalar-state courier
and scene owner do not yet share a multi-owner transaction. Application event
capture and scheduler ownership remain explicit: workers receive only the
immutable program/capture/inputs and their cancellation/clock services; the
owning UI receiver calls `ApplyNyxCanvasProgram`. A dedicated saved event/script
route and public Studio program workflow remain required integration work.

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

`tools/test-program-canvas.ps1` separately qualifies portable scene regression,
typed captures/proposal admission and actual embedded Nyx button/layout behavior.
Its explicit `NYX_CANVAS_PROGRAM_TESTS` fixture retains the ordinary workspace
journey and adds useful resource-driven scripted layout, complete allocation
refusal, stale/cancelled/reentrant/retired publication and exact input retention.
Native concurrent readers prove worker computation, not an automatic worker/UI
publication route. The HTTP consumers execute at desktop and CSS-390; this is not
physical touch or full accessibility evidence.

Mixed state/scene transactions, saved canvas event/script routing, useful diagram
behavior, production cost budgets and observing delivery remain open
under the [existing task](../TODO/NS-1_canvas-behaviors_01.md). Current engine and
workbench evidence does not close that full acceptance or establish hardware,
IME, accessibility, other browser/widgetset or complete native/browser parity.

## Named programs in projects and Studio

A document owns `Programs`, an `INyxProgramDeclarations` registry. Each exact,
case-sensitive `TNyxProgramRef` selects an immutable `INyxProgramDefinition`.
It contains source, typed contract and limits; `Compile` returns validated,
immutable code. Defining, opening, cloning, generating or importing a program
does not execute it or supply runtime grants. Registry membership clones
independently; definitions may outlive their original document.

```pascal
LDocument.Programs.Define(
  NyxProgramRef('Welcome behavior'),
  NewNyxProgramDefinition(
    'begin Caption := ''Hello, '' + Name end.',
    NyxProgramContract
      .Input(NyxProgramSymbol('Name'), NyxProgramType(npkText))
      .Output(NyxProgramSymbol('Caption'), NyxProgramType(npkText)),
    NyxProgramLimits.Steps(20000).CallDepth(32)));
```

The registry admits up to 64 definitions and one MiB of encoded declaration
entries. Caller limits still bound each program. Admission captures an exact
name and retains creator interfaces before metadata callouts; malformed data,
reentry, excessive membership/bytes and creator retirement cannot partially
replace accepted membership. Definitions import no native executable code,
application values, runtime handles, services or authority.

Document version 15 includes nonempty program declarations alongside existing
state, resources, collections, scenes and graphs. Without programs, existing
document encodings stay unchanged. Generated Pascal uses typed public factories,
ports, host signatures, enums and fluent budgets. The standard canvas vocabulary
emits `NyxCanvasProgramContract`; arbitrary interpreted source stays explicit
string data inside `NewNyxProgramDefinition`, rather than becoming native
application instructions. The literal source reader admits that same vocabulary.
Chronological handwritten continuation emits targeted Define/Remove changes and
retains unchanged helper definitions; whole compiler/candidate equality remains
required. This latter richer continuation still needs its own actual compiler
and observing qualification.

`NewNyxProgramEditor` returns a public Nyx card used by both Studio adapters.
It supports saved/new definitions, exact source, local diagnostics, typed
input/output parameters, retained/pure/canvas host vocabulary, three common
budgets and confirmed removal. Other declared signatures and all unexposed
limits remain retained. Check and parameter changes affect the proposal only.
Save/Remove enter the ordinary paired source command and one Undo step.
Successful Save follows the admitted definition; rejected Save preserves the
accepted pair and local proposal. Changing the name saves a separate definition.
New forms choose unused default names. Studio rejects stale accepted contexts
and collisions with a different saved program; explicitly open that definition
before replacing it.
Dirty proposals refuse Open/New until saved or explicitly restored.

Unfinished source, numeric text and schema edits are separate presentation data.
Closing/reopening the pane preserves them; an exact accepted-definition context
prevents an older proposal from being replayed onto a different program.
Optional presentation version 18 preserves this state; unused program authoring
retains previous presentation encodings. Pending companion Pascal retains its
ordinary explicit resolution guard.

The existing `nyx_logic` semantic tool gains these current-source scopes:

| Scope | Bounded response |
| --- | --- |
| `programs` | Exact names and counts; 8 rows by default, 32 maximum |
| `program-source` | Source window; 512 Unicode scalars by default, 1,024 maximum |
| `program-ports` | Named input/output directions and exact types |
| `program-routines` | Host signature facts and parameter counts |
| `program-routine-arguments` | Parameters of one exact declared routine |
| `program-limits` | One fixed-size numeric budget record |
| `program-capabilities` | Required capability names; these are not grants |
| `program-check` | Proposed definition diagnostics; never executes or publishes |

Pin `expectedRevision` when paging. Saved definition scopes require an exact
`program`; routine arguments also require `routine`. Source windows use
`textOffset/textLimit`; row pages use `offset/limit`. Check accepts explicit
`definition` metadata (version/source/contract/limits) and returns structured
diagnostic category, UTF-8 byte range, scalar line/column and a bounded message.

`program-define` (`name`, `definition`) and `program-remove` (`name`)
compose inside `nyx_transaction`, sharing permission, expected revision,
operation identity, retry and paired Undo/Redo guards. The current-source
schemas extend an existing tool; this does not install them in an older server.
Local semantic execution and authenticated observing HTTP are separate evidence.

`tools/test-program-authoring.ps1` qualifies declarations/source, local semantic
queries and paired edits, actual ordinary LCL Studio input, compiler-produced
complete constructors and matching HTTP browser consumers. Supply the same
explicit preflighted static host arguments as the other program scripts.
Its worker uses pas2js's module target. The script starts no listener and never
selects or replaces a protected project. WORK.md owns exact counts, retained
failures, current-source boundaries and deployment state.
