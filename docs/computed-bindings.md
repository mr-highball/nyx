# Typed computed bindings

[State](studio-state.md) · [Task](../TODO/NS-1_computed-bindings_01.md) ·
[Evidence](../WORK.md)

`nyx.expressions` exposes specialized managed Boolean, text, Integer and number
expressions. Constructors accept literal values or the corresponding typed state
reference. Fluent methods accept their own family; Integer-to-number promotion
and value-to-text conversion are explicit. A control can use these expressions
through its ordinary public `Binds` interface.

```pascal
var
  LQuantity: INyxIntegerExpression;
  LTotal: INyxNumberExpression;
  LCaption: INyxTextExpression;
  LReady: INyxBooleanExpression;
  LSummary: INyxLabel;
  LSubmit: INyxButton;
begin
  LQuantity := NyxIntegerExpression(NyxIntegerState('quantity'));
  LTotal := NyxNumberExpression(NyxNumberState('price'))
    .Times(LQuantity.AsNumber)
    .Plus(NyxNumberExpression(NyxNumberState('tax')));
  LCaption := NyxTextExpression('Total: ').Append(LTotal.AsText);
  LReady := NyxBooleanExpression(NyxBooleanState('ready'))
    .AndAlso(LQuantity.GreaterThan(NyxIntegerExpression(0)));

  LSummary := NewNyxLabel('total');
  LSummary.Binds.Text(LCaption);
  LSubmit := NewNyxButton('submit');
  LSubmit.Configure.Text('Submit order');
  LSubmit.Binds.Enabled(LReady);
end;
```

Use `NyxChoose` for same-family conditional branches. Boolean expressions provide
`AndAlso`, `OrElse`, negation and equality. Numeric families provide comparisons,
checked arithmetic and text conversion; Integer also supports modulo and explicit
`AsNumber`. Text provides append, comparison, contains, prefix/suffix and Unicode
scalar length. Exact text comparison is default. The optional
`netAsciiInsensitive` enum folds ASCII letters only, preserving other Unicode.

Expressions are immutable independently owned trees. Composition admits copied
descriptors rather than retaining arbitrary implementation objects. Each tree is
bounded to 256 nodes, depth 32, 262144 UTF-8 wire bytes and 262144 result scalars.
`DependencyCount`/`Dependency` give copied ordered metadata. Persistence uses
strict version-1 descriptors at the explicit extension boundary; unknown fields,
operators, arities, types and versions refuse.

`CaptureNyxExpressionInputs` copies a store synchronously on its owning thread.
The immutable frame outlives that store and retains no subscriptions or controls.
Two actual native workers now qualify shared immutable evaluation. Use
`nyx.expressions.execution` for guarded scheduler publication as described below.
Do not read a live mutable store from a worker because an immutable frame exists.

All dependencies are admitted before evaluation, including an unchosen branch.
Evaluation then short-circuits Boolean operations and evaluates only the chosen
conditional branch. Missing keys, wrong types, overflow, division by zero and
nonfinite results raise `ENyxExpression`. Integer division truncates toward zero;
modulo follows that quotient on both targets.

Computed bindings are read-only projections. `Value` does not infer an inverse
expression or assign its dependencies; keep a direct-state binding for two-way
editing. The existing live binding admission validates a complete candidate
before publishing state/control changes. A failed grouped arithmetic update keeps
the accepted store, caption and unrelated editor intact.

Documents containing computed bindings use version 12; older readers refuse that
version. Documents without expressions retain their prior wire/source shape.
Generated Pascal uses specialized expression constructors and fluent methods,
with `nyx.expressions` imported only when needed. The shared source reader admits
generated multiline expression construction. Fluent operations and nested operands
receive deliberate indentation without splitting quoted literals. Managed builders
also admit specialized local expression variables as described below. Complete
refactor/transfer/reuse journeys remain open.
Rebuild native callers fully: `INyxBindings` has a new IID
for its added overloads; mixed old/new compiled implementations are not supported.

`tools/build.ps1 -Target computed-bindings -ComputedBindingEvidenceDirectory <output>`
runs checked native evaluators, document/source/history fixtures, actual LCL
applications and compiler rejection cases, then stages matched browser consumers.
Browser HTTP execution is a separate check. Current evidence passes 431 evaluator,
23 shared binding and 18 actual application checks on each target, including
desktop/CSS-390 browser control retention. An observing release is qualified
separately from these local applications and the authoring implementation below.

Studio's Properties → Data → Bindings section mounts the public
`INyxExpressionEditor` compound for scalar targets. Its named source part is a
Nyx CodeEditor containing fluent Pascal. Preview parses one closed typed
expression and evaluates the document's defaults; it does not execute arbitrary
Pascal or modify runtime state. Apply publishes the binding and generated Pascal
as one ordinary paired Undo command. Unbind uses the same existing command path.
Both adapters park unfinished expression text independently of application
Pascal. Preview can run while application Pascal is pending; Apply refuses until
that draft is resolved. A changed owner, target, binding or default context
retires the parked form draft instead of applying it to a different context.

Direct library callers can create `NewNyxExpressionEditor` with a typed
`NyxControl` owner and `TNyxBindingProperty`. `ReadNyxExpressionEditor` returns an
owned typed proposal after checking exact form/action identity, baseline,
dependencies, default evaluation and a detached complete document. The caller
publishes through its own ordinary command/history mechanism. The form retains
no document, mutable state store or renderer reference. Its initial suggestion
uses an existing computed expression or the first compatible default; it does
not yet prioritize an existing direct-state binding.

Current-source `nyx_state` adds two read modes: `expression` inspects one child
path and pages at most 50 dependencies; `expression-source` returns at most 4096
Unicode scalars of fluent Pascal. Both require the exact revision, authored
owner and binding target. Tree inspection includes operator, family, arity,
size/depth and an 80-scalar text-literal preview instead of the entire descriptor.
The `bind-expression` change accepts the strict expression descriptor at the
explicit interchange boundary. It can join other state changes or a grouped
`nyx_transaction`; clear/inherit retain their existing meanings. Typed Pascal
callers use `NyxBindControl(NyxBindingOwner(...), bpText, expression)` inside
`NyxStateBindingPatch`. Default rename rewrites every expression dependency with
its original scalar family, preserving literal text and paired Undo/Redo.
The existing tool count remains 23; these are additions to existing schemas.

`tools/build.ps1 -Target computed-authoring -ComputedAuthoringSourceDirectory
<semantic-export> -ComputedAuthoringEvidenceDirectory <new-output>` requires the
exact exported project and companion, checks shared authoring/discovery and the
authenticated production handler, runs an independent native Studio, and stages
browser Studio/shared consumers with their matched source worker and RTL. HTTP
browser execution remains explicit. Suspended-handler checks qualify request,
authentication, dispatch and paired publication, without binding a socket or
changing enrollment. They do not prove installation in an observing server.

Expression reads currently normalize a complete bounded tree and realize the
selected component context; the form baseline copies defaults. Preview renders
the bounded full source and result. These are not incremental algorithms or
production latency/memory budgets. Automatic background binding recomputation,
full reuse/transfer/refactor journeys, accessibility/hardware input,
large-workload quality and observing rollout remain open. The installed release
currently refuses `bind-expression` without changing the owned review revision;
its schema and frontend require a separately qualified preserving delivery.

`TNyxExpressionExecution.Submit` computes typed assignments from one captured
application-state frame and delivers them through one ordinary atomic `Apply`:

```pascal
LRun := TNyxExpressionExecution.Submit(LApplication.Context, [
  NyxCompute(NyxIntegerState('quantity'),
    NyxIntegerExpression(NyxIntegerState('quantity')).Times(NyxIntegerExpression(2))),
  NyxCompute(NyxBooleanState('ready'), NyxBooleanExpression(False))],
  neAsynchronous);
```

Specialized `NyxCompute` overloads retain each destination's scalar family.
Destinations must already exist; 1..256 distinct assignments and all dependencies
are admitted before scheduling. Every expression reads the original frame,
including references to another destination in the same group. Computation
failure publishes none of the outputs. Successful UI delivery requires the exact
captured store/revision and the original live application/page generation.
Concurrent results captured at one revision allow one changed publication; an
obsolete result fails rather than overwriting it. Cancel the returned `INyxQueryRun`
to retire work cooperatively; application/page retirement also revokes delivery.
The run reports completion only after UI publication, with ordinary query terminal
observation available. Receiver failure after state commit is still a failed run
with committed state; it cannot imply rollback.

Default policy is `neUIQueue`. Native `neThreaded` runs on a real worker;
browser explicitly refuses that policy. `neAsynchronous` uses native workers or
the browser event loop. This adds no browser worker transport or automatic
background recomputation to ordinary computed bindings. Submission itself runs
on UI and copies the expression trees, input frame and destination snapshot;
normalization/default admission costs are not hidden behind a worker claim.

`tools/build.ps1 -Target expression-execution -ComputedAuthoringSourceDirectory
<semantic-export> -ExpressionExecutionEvidenceDirectory <new-output>` qualifies
concurrent native reads and actual LCL application publication, then stages the
matched browser consumer for explicit HTTP execution. Current checked native
evidence includes two distinct simultaneous workers sharing one tree/frame,
2,000 exact Unicode/calculation evaluations, and an actual-control journey through
success, atomic failure, stale delivery, cancellation, navigation, overlapping
runs, reentrant extension getters and teardown. These checks establish the bounded
publication behavior, not production throughput or full asynchronous binding parity.

Managed `BuildNyxDocument` source can declare `INyxTextExpression`,
`INyxBooleanExpression`, `INyxIntegerExpression` and `INyxNumberExpression` locals,
including grouped declarations. Initialize a local with its matching typed factory
or another initialized expression, then compose its fluent methods and use it in
ordinary `Binds` calls. Explicit `AsNumber`/`AsText` conversions retain their
normal types. A readable builder can name `LQuantity`, `LTotal`, `LCaption` and
`LReady` separately instead of embedding every operation inside a binding.

Declarations share Pascal's case-insensitive namespace with controls and typed
state references. Wrong families, undeclared/uninitialized use and duplicate names
refuse. The managed declarative subset admits one assignment per expression local;
reassignment, base-interface casts, arbitrary helper calls and general imperative
Pascal are outside that subset. This is a source-model admission boundary, not a
restriction on ordinary compiled Pascal applications. The standalone expression
form still accepts one self-contained fluent expression without application locals.

The reader owns immutable descriptors; prepared reconciliation facts copy those
records into independent readers. Source atoms compare expression meaning using
the exact descriptor, and expression names remain reserved when Studio selects
new control names. Unrelated design edits preserve handcrafted declarations,
fluent operands and comments. Typed state rename and changed bindings retain
paired source/history admission; one Undo restores the exact handcrafted pair.
Generator output still uses its existing fluent inline expressions.

`tools/build.ps1 -Target expression-locals -ComputedAuthoringSourceDirectory
<semantic-export> -ExpressionLocalEvidenceDirectory <new-output>` builds a
handcrafted companion from the unchanged semantic seed, checks source/history,
compiles that exact companion and compares its document with source admission.
It also checks existing inline computed bindings and stages matched browser
counterparts for explicit HTTP execution. Current evidence is 24 source/history
checks, four exact compiled-companion checks and 23 inline regression checks per
qualified target; observing schema/frontend delivery remains separate.

Computed expressions survive independent derived definitions and per-instance
named-part overrides through the existing reusable API. Restoring inheritance
removes only the exact local descriptor; the definition and sibling instances
retain their own formulas. Shared immutable expression data does not grant shared
mutable ownership: changing a cloned/derived binding leaves its source intact.
Alternate `INyxExpression` implementations enter binding admission through their
strict descriptor, normalized into an independent tree. Later changes to the
extension object cannot change the admitted binding or supply its evaluator.

The ordinary `NyxStudioProjectCompanion` and `NyxStudioProjectBackup` file contracts
carry these formulas without a separate expression asset. Adjacent files retain
accepted design/Pascal; backups additionally retain pending Pascal and its exact
baseline. Import into a fresh session uses ordinary candidate admission and one
paired Undo command. Reopened derived definitions can be edited and state
dependencies renamed without changing the original recipe. Invalid incoming
source and invalid grouped derived bindings preserve the accepted pair.

`tools/build.ps1 -Target expression-reuse -ComputedAuthoringSourceDirectory
<semantic-export> -ExpressionReuseEvidenceDirectory <new-output>` runs the grouped
semantic derivation/override/inherit and file-transfer/history journey, then
compiles and executes its exact companion on LCL and stages matched browser
consumers. The shared journey passes 26 on each qualified target; the actual
compiled application passes 10 native/10 HTTP-browser checks, including sibling
formulas, supplementary Unicode captions, memo identity/draft/focus, independent
runtime stores and navigation. File-helper/session admission is qualified here;
new physical picker/download interaction and observing release are separate.

Live projection retains complete candidate validation. Each `ApplyNyxBindings`
invocation owns one immutable input frame and an indexed map of admitted computed
values by exact realized node/target/descriptor. The second property pass and
range checks reuse that value. The map, copied descriptors and values are released
at the end of the call, including failure. A new state candidate, committed
projection, resource reload, refresh or command starts with an empty map; values
never survive revisions or enter a global cache. Direct/resource bindings keep
their ordinary paths. This is deduplication within one projection, not incremental
dependency-driven evaluation.

For the maintained 32 calculated labels / 20 updates workload, complete staged
validation plus committed projection originally performed 7,040 descriptor
admissions, 3,840 evaluations and 40 input captures. Current source performs
3,200 admissions, 1,280 evaluations and the same 40 captures. The qualification
budget caps that exact workload at those current counts and checks every caption
plus atomic arithmetic failure. A profile-only compiler define
`NYX_EXPRESSION_PROFILE` exposes serial UI-workload counters; production builds
contain none of those counters. They are not synchronized worker instrumentation.

Tree admission discovers unique dependencies in first-use order, validates every
closed shape/family and enforces wire/node/depth/text limits. Evaluation checks all
dependencies before lazy computation. Input capture copies the state frame;
candidate/live paths still realize/clone and validate complete trees, and binding
descriptor validation still normalizes trees. The scoped index avoids a quadratic
linear search across consumers, but it does not remove those remaining costs.
Copied text/spec/value storage is bounded by the admitted design/tree budgets;
current counts do not prove a peak-memory or production update-latency budget.

`tools/build.ps1 -Target expression-costs -ComputedAuthoringSourceDirectory
<semantic-export> -ExpressionCostEvidenceDirectory <new-output>` runs the measured
projection/work-budget fixture, binding regression and actual LCL worker/UI
consumer, then stages browser counterparts. Timing observations are recorded
separately from deterministic work counts. A checked optimized native build of
the current workload without heap tracing takes 3,453 ms for 20 updates in the
current packet; this still indicates remaining cost and is not accepted as an
interactive production target. Observing delivery, representative broader shapes,
authoring costs and production latency/memory qualification remain open.
