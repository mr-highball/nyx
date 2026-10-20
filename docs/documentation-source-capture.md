# Capturing declared signatures

[Documentation contracts](component-documentation.md) · [Current work](../WORK.md)

`nyx.documentation.signatures` extracts declared procedure/function signatures
through Nyx's existing Pascal syntax owner. Parameter names, passing modes,
type expressions, result expressions and authored defaults remain detached data.
Named uses remain explicitly unbound until a source/binding owner supplies them.
Inspection executes no documented class, factory, callback or example.

For an independent selection use `CaptureNyxDocumentationRoutineSignature`.
For a coherent group from one source, use the atomic batch:

```pascal
uses
  nyx.pascal.syntax,
  nyx.documentation,
  nyx.documentation.typeexpressions,
  nyx.documentation.signatures;

function DescribeSelection(const Source: INyxPascalSyntaxSnapshot;
  const Ordinals: array of Integer): TNyxDocumentationRoutineSignatures;
var
  Construction: INyxDocumentationTypeConstruction;
begin
  Construction := NewNyxDocumentationTypeConstruction(
    TNyxDocumentationTypeExpressionLimits.Defaults);
  try
    Result := CaptureNyxDocumentationRoutineSignatures(Source, Ordinals,
      Construction);
  finally
    Construction.Retire;
  end;
end;
```

The nonempty selection is bounded by the existing 128-member ceiling. Returned
entries keep the caller's order, including reverse source order. Ordinals belong
only to that exact snapshot; they do not become stable member or overload keys.
Duplicate selections, unsupported/context-dependent headers, changed source,
insufficient shared admission or cancellation refuse the entire vector. An
earlier independent result stays readable after a later batch refuses.

Each selected header is still reparsed under the supplied construction. The
batch retains bounded observations of its routine, parameter scope, ordered
parameters and optional following-local boundary. It reobserves those receipts
after all siblings, so a later getter cannot silently change an earlier selected
row. A private ordinal copy prevents outward callbacks from changing the active
selection; final caller-array agreement follows the last outward admission.
Final agreement also checks lexical owner/policy, declaration/scope/scalar
counts and the complete captured source value. Getter and outward-admission
cancellation prevents publication. No source, parser, temporary witness,
construction or cancellation/admission lease survives in the returned metadata.

The current extractor handles representable non-generic procedures/functions.
Constructors, operators, contextual/untyped parameters and uncertain syntax keep
their explicit refusal. A successful group does not claim complete unit
discovery, compiler acceptance, type binding, physical include provenance,
implemented member mappings or automatic creator registration.

## Work and copy admission

Repeated independent selections scan source prefixes independently. A batch uses
`INyxDocumentationTextWindows` to reuse checked scalar/storage positions over
one immutable Pascal text value. This avoids repeatedly decoding the entire
earlier unit for adjacent declarations. The source value is borrowed during the
operation; copied header/default/type windows still reserve their exact UTF-8
bytes before allocation and consume the same shared counters.

The cursor keeps two positions for its latest window. Repeated and forward
requests can reuse those checked positions. Backward requests pay for whatever
earlier prefix must actually be scanned; arbitrary access has no constant-time
guarantee. Reuse grants no byte refund, new allowance, compiler fact or trust in
an alternate supplier's token/Slice methods. The independent single-signature
API retains its original copy/admission behavior.

For direct text-window use, call `NewNyxDocumentationTextWindows(Text,
Construction)` and retire the temporary cursor in a `finally` block. Retiring
the construction prevents further reads; retiring the cursor also releases its
source value and construction reference. Private interface leases protect both
objects across outward callbacks, including retirement of the caller's last
reference. Neither operation retires unrelated
callers' results. A failed window commits no new checkpoint and exposes no
truncated string.

## Validation boundary

The new [batch consumer](../tests/nyx_documentation_signaturebatch_tests.lpr)
requires exactly one valid repository root before native check zero:

```text
nyx_documentation_signaturebatch_tests.exe D:\Docs\GitHub\nyx
```

It selects all 81 actual generated factories from their include and all eight
handwritten public globals from the containing controls source. Those remain
two physical source owners; the fixture creates no fake partial unit descriptor
or help inventory. Other cases exercise Unicode/NUL positions, repeated-copy
bytes, forward/backward work, grouped order, late refusal, earlier sibling
mutation, whole-source changes, shared limits, cancellation and retirement.
The native consumer passes 264 checks over all 89 globals with a clean heap.
Its same binary refuses missing and invalid repository arguments before check
zero, also with clean heaps. A 32-header long-prefix case fits the same work
allowance that repeated independent captures exhaust. The common retained-source
matrix passes 45 compiler jobs, 21 native test programs and 21 portable compiles,
with zero owned warnings and 147 upstream RTL warnings. [WORK.md](../WORK.md)
retains the exact receipts and the earlier fixture-expectation failure. Full
source adoption and executed browser reference browsing retain their own
acceptance requirements.
