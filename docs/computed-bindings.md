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
Worker reads are the intended contract; actual concurrent evaluation and guarded
scheduler publication remain qualification work. Do not read a live mutable store
from a worker because an immutable frame exists.

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
receive deliberate indentation without splitting quoted literals. General local
expression variables and complete refactor journeys remain open.
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
production latency/memory budgets. Worker publication, expression-variable
authoring, full reuse/transfer/refactor journeys, accessibility/hardware input,
large-workload quality and observing rollout remain open. The installed release
currently refuses `bind-expression` without changing the owned review revision;
its schema and frontend require a separately qualified preserving delivery.
