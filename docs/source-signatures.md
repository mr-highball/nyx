# Source-owned API signatures

[Documentation contracts](component-documentation.md) · [Current work](../WORK.md)

`CaptureNyxDocumentationRoutineSignature` reads a selected procedure/function
declaration through Nyx's existing Pascal syntax and documentation owners. It
returns an immutable `INyxDocumentationRichSignature`, with ordered logical
parameter names, passing modes, exact inert defaults and detached parameter/result
type expressions. Names such as `&type` retain the logical identifier `type`;
type/default text preserves the supplied snapshot's spelling and comments.

```pascal
LSource := NewNyxPascalSyntaxSnapshot(
  'function Select(const Items: array of Core.TItem): Core.TResult;',
  TNyxPascalSyntaxOptions.Defaults.Fragment(psfInterfaceDeclarations));
LConstruction := NewNyxDocumentationTypeConstruction(
  TNyxDocumentationTypeExpressionLimits.Defaults);
try
  LSignature := CaptureNyxDocumentationRoutineSignature(
    LSource, LSource.Named(0, 'Select'), LConstruction);
finally
  LConstruction.Retire;
end;
```

`LSignature` remains usable after both the source and construction are released.
Its named type uses are explicitly unbound, including bare names and qualified
generic arguments. The caller's source/binding layer must supply real qualified
identities before complete reference-index publication. A factory-looking name
or interface result does not grant factory authority.

## Capture and admission

The routine ordinal is local to one snapshot. An alternate snapshot's interface,
counts or claimed receipts establish no sealed syntax ownership. The extractor
checks bounded envelopes, captures only the selected header and reparses it once
with the existing declaration parser. It checks the supplied parameter prefix
against that owned header, then reobserves all relevant envelopes, rows and header
text before publication. Ordinary implementation locals after the parameter prefix
are not traversed as signature declarations.

Every selected window uses `CopyNyxDocumentationTypeTextWindow`. That function
charges Unicode prefix/window scanning to the same construction, determines exact
UTF-8 bytes and reserves them before allocating the substring. Multiple windows
and multiple signatures share work, copied bytes, lexical tokens and type nodes;
there is no fresh per-parameter scope. Abandoned attempts remain accounted. A
bounded private parameter supplier passes through the ordinary rich-signature
capture, including its final metadata-byte and duplicate-name checks.

Caller-owned construction is not retired by the extractor. Retire it at the end
of the encompassing operation. The result retains no whole source unit, supplier,
header parser, construction or cancellation lease. Cancellation after private
copying still refuses before assigning a managed public result.

## Present boundary

This is signature syntax, rather than a complete declaration descriptor. A
translated snapshot supplies analysis text; physical authored text requires its
separate checked origin map. Snapshot ordinals are not stable overload keys.
Class/static/visibility/calling-convention metadata, generic declarations,
constructors/destructors/operators, target availability and declared factory
ownership need their own source attachment. The present API refuses unsupported
routine families and generic headers instead of discarding those facts.

Untyped parameters also refuse because the present rich type-use contract cannot
represent their absence of a declared type. Incomplete/conditional/directive
headers, malformed default/result windows, structured types requiring their
original declaration context, inconsistent suppliers and over-budget operations
return no partial signature. None of this grants compiler execution or executes a
control, callback, factory or documentation example.

The [native consumer](../tests/nyx_documentation_signatures_tests.lpr) requires
exactly one valid repository-root argument before
any check, and extracts every public `Choices` overload in `nyx.contract` plus
`NyxObject` and `NyxArray` in `nyx.data`. Portable fixtures cover compound shapes,
defaults/modes, alternate mutation, shared admission and cancellation. It passes94
native checks with all9 actual signatures extracted and58,948 allocated blocks
freed. The same final binary refuses missing/invalid repository arguments at
check0 with clean heaps. Its portable consumer compiles. Final receipts are under
`E:\Blekohlab\nyx\qualification\documentation-declarations-20261020-194836`:

- `logs/nyx_documentation_signatures_tests-native-focused-supplier-compile.log`
- `logs/nyx_documentation_signatures_tests-native-focused-supplier-run.log`
- `logs/nyx_documentation_signatures_tests-browser-focused-supplier-compile.log`
- `source-argument-refusals-terminal-private.json`
- `final-audit-private.json`

The common source qualification passes39 compiler jobs,18 clean native test
programs and18 portable compiles, with zero owned warnings/126 upstream. WORK.md
retains both earlier fixture failures and their corrected assumptions separately.
This does not establish executed browser behavior, full core registration or
complete source-owned help adoption. Whole-unit adoption also must account for
the selected-window API's real prefix scan work once, within existing limits;
independently restarting an allowance for each routine would bypass the contract.
