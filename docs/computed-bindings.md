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
expression variables, complete refactor journeys and Studio expression
editing remain open. Rebuild native callers fully: `INyxBindings` has a new IID
for its added overloads; mixed old/new compiled implementations are not supported.

`tools/build.ps1 -Target computed-bindings -ComputedBindingEvidenceDirectory <output>`
runs checked native evaluators, document/source/history fixtures, actual LCL
applications and compiler rejection cases, then stages matched browser consumers.
Browser HTTP execution is a separate check. Current evidence passes 431 evaluator,
23 shared binding and 18 actual application checks on each target, including
desktop/CSS-390 browser control retention. No public Studio/MCP authoring or
observing release is claimed from these local applications.
