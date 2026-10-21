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

These independent snapshot APIs handle representable non-generic procedures/functions.
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
At its earlier committed checkpoint, the native consumer passes 264 checks over
all 89 globals with a clean heap. Its same binary refuses missing and invalid
repository arguments before check zero, also with clean heaps. A 32-header
long-prefix case fits the same work allowance that repeated independent captures
exhaust. That checkpoint's common retained-source matrix passes 45 compiler jobs,
21 native test programs and 21 portable compiles, with zero owned warnings and
147 upstream RTL warnings. [WORK.md](../WORK.md) retains the exact receipts and
the earlier fixture-expectation failure. Later session checks are recorded below;
this historical matrix does not qualify the current source integration. Full
source adoption and executed browser reference browsing retain their own
acceptance requirements.

## Source-package integration under qualification

`nyx.documentation.source` now has the actual
`CaptureNyxDocumentationSourcePackage(World, Scopes, Creators, Platforms,
Construction)` path. A named `TNyxDocumentationSourceScopes` vector selects a
public type or, with `DeclarationIndex = -1`, the complete public globals of a
unit. `TNyxDocumentationSourceCreators` supplies exact operation-local routine
ordinals from the real registration/generator owner. A `New` prefix supplies no
creator authority. The generated controls companion supplies all 81 built-in
annotations from the same checked declaration inventory used by emission.

The adapter accepts only the exact immutable Nyx world and translated-view
owners. That check establishes implementation ownership, not compiler, disk or
target provenance. It requires complete resolved source and public imported
namespaces, captures complete selected scopes, follows genuine referenced type
declarations, and builds canonical per-unit chunks in one flat package. Every
scope, signature, type window, binding search and final contribution framing
uses the supplied construction. Each module now owns one checked signature
session across its selected global/class/interface groups. The operation retires temporary source and
admission leases before sole publication; caller-owned construction remains the
caller's responsibility.

Lifecycle/operator signatures, explicit untyped const/var/out parameters,
compound fields and inert global constant/variable/resource-string initializers
are part of this integration. Initializer/type text belongs to the checked
analysis projection; it is not represented as a byte-identical physical source
excerpt. Class bases and implemented interfaces remain separate. Unspecified
visibility is retained as unspecified; no effective ABI or default visibility is
inferred. Source prose is currently unknown/empty, rather than synthesized from
names or copied from a parallel help list.

The source consumer targets native and portable compilation; complete source
package publication remains unqualified. Its original archived-only native path
requires five valid arguments before check zero:

```text
nyx_documentation_source_tests.exe <repository> <accepted-RTL-runtime> <verified216-inputs> <retained-profile-runtime> <retained-Windows-compiler-directory>
```

Explicit seven-, eight-, nine- and ten-argument maintenance paths are described below;
the original five arguments and complete source/package gate remain mandatory.

The authoritative native compiler invocation must explicitly include
`-uPAS2JS`. That prospective caller setting remains separate from restored
bootstrap/compiler receipts. Native execution first restores the original
156-file/47-module System/ObjPas sequence and exact Windows/SysUtils requests,
then captures actual repository sources, selects complete controls/fixture
scopes, injects the package into the real catalog/schema/callback owners and
attempts immutable index publication with fixture execution counters at zero.
Portable synthetic inputs exercise ownership, role authority, late shared-input
mutation, cancellation, lifetime, missing namespaces and aggregate refusal;
they do not substitute for this actual-source adoption outcome.

Earlier source attempts remain retained, including the initial comment-policy
refusal and later unresolved SysUtils source/context frontiers. Preparation19
replays the accepted 35-name request and captures the actual next 36-name
request, preserving the original four size operands. The unchanged final source
assertion still fails at check88 with a clean heap: 3,303 SysUtils declarations,
19 answers, one typed compiler-root `Longword` requirement and six diagnostics
at the actual `record helper for TGUID` header. Its cumulative semantic work is
3,415,320 of four million, with 43 of 64 logical passes. Neither that failure
nor the later request-restoration checks establish source-package publication.

Preparation20 qualifies the existing parser/binder's distinct helper headers and
explicit refusal of unsupported helper meaning: syntax282 and binding85 pass
natively with clean heaps, their portable consumers compile, and the source
consumer compiles on both targets. It adds no helper lookup or documentation
adoption. The approved 24 literal Classes dependency additions are retained
separately from the original 216 inputs; two non-UTF8 views remain opaque.
The full journey has not consumed that prepared 240-file union. Generic
own-formal enumeration, nested declaring identity, generic interface bases,
variant-arm metadata, unsupported declaration families, genuine missing inputs
or facts and any exceeded quota still refuse complete capture. Full
source/prose/target adoption and executed decorated-browser evidence remain open.

Preparation23/24 adds a typed `MultipleHelpers` compiler policy. Its initial
absence stays unknown; only the inspected FPC30301 authored module `MODE`
operation establishes the known-off default. Qualified global `MODESWITCH`
changes retain activity and header ownership, and local `PUSH`/`POP` does not
restore this module-wide switch. Known policy values participate in context,
constant-storage and detached documentation-root identity; absent values retain
the prior framing. The policy value record grows, so consumers must rebuild.

This supports only a narrow lexical proof at an EOF helper prefix: an intact
header, no parent, an enclosing-site target that resolves to a primitive, and
explicitly disabled multiple helpers. Own earlier members still win. An unknown
or enabled policy, or a structured, missing or malformed target, cannot donate
outer lookup; real typed compiler-root requirements remain explicit. Complete
helper types, activation, parent-helper and structured-target lookup remain unsupported.
Focused native context250, storage233, witness43, translation241 and binding130
checks pass with clean heaps; their browser consumers compile only. The first
binding fixture's forward-alias expectation remains retained separately from
the corrected forward-refusal and actual self-recursion checks.

Preparation25's genuine prepared-profile source journey accepts the one actual
`Longword` compiler-root fact, then still fails the final source assertion at
check89. Updated System retains 13,101 declarations and 3,913,992 work units;
ObjPas retains 209 declarations at 113,006 work units. SysUtils advances from
5,462 declarations/19 answers to 5,654 declarations/27 answers, with zero syntax
diagnostics. Its next frontier has 111 source diagnostics and 110 unresolved
conditions, beginning with `defined(cpullvm)` and `defined(linux)` immediately
after returning from the helper include. Neither name belongs to the accepted
36-name profile; the prepared facts were retained rather than lost.

Both SysUtils attempts consume 3,422,333 of four million work units and 48 of
64 passes. Preparation uses 526,542 of four million units; dynamic fact jobs are
seven of eight, and initial-symbol jobs remain three of four. Another complete
semantic reconstruction would need to redo the existing 27 ordered answers;
the remaining 16 passes do not justify that attempt. The later
`not declared(MaxDirs)` condition is a source-declaration need, not a proposed
compiler define. The accepted `Longword` reply must be replayed in any later
journey. No complete source package or Classes-overlay result is established.

The consumer reports every provisional source/syntax diagnostic before any fact
admission. Only a current pending compiler-context result with the exact one
typed `Longword` requirement, independent bootstrap and original System owner
can reach the existing generic read-only fulfillment. An unanswered condition's
diagnostic is not treated as missing authority for that separately checked
request; no diagnostic is removed or ignored by final publication. The complete
zero-diagnostic source/package assertion and all quotas remain unchanged.

## Explicit prepared profiles for source maintenance

`RestoreNyxFPCCompilerProbe(Source, TraceOptions, Cancellation)` restores request
intent from the existing owner's exact canonical source. Preparation21 qualifies
the strict restoration path: the existing compilertrace consumer passes 516
native checks with a clean heap, including byte-for-byte equality against both
retained pre-emitter 35- and 36-name request sources. The planner regression
passes 37 native checks with a clean heap. Both source targets compile; browser
evidence remains compilation only. Restoration does not supply compiler facts,
source authority or evidence of a native job. The existing terminal
`ReadNyxFPCCompilerCapture` and complete capture `Merge` still validate replies.
See the [compiler capture protocol](pascal-projects.md#capturing-facts-from-a-configured-fpc-compiler)
and [requirement/preparation ownership](pascal-projects.md#compiler-requirements-and-module-preparation)
for the underlying contracts.

The source consumer accepts only these explicit forms, with argument validation
before check zero. No compiler or output path comes from an environment default.

| Argument count | Maintenance behavior |
| --- | --- |
| 5 | Original archived-only journey; no new native collection. |
| 7 | Base five, explicit compiler, fresh output directory: continue only actual source-requested eligible initial symbols under the original ceilings. |
| 8 | Seven plus one exact retained continuation directory: replay its matching source/reply before considering a later new symbol job; mismatch refuses without fallback. |
| 9 | Seven plus both accepted cumulative request/reply directories, in 35-name then 36-name order: prepare the validated profile before initial SysUtils construction. |
| 10 | Nine plus the accepted primitive-reply directory: replay its actual source-owned root fact and rebuild the namespace world before first SysUtils construction; permit at most the actual fourth symbol cohort. |

The prepared form is:

```text
nyx_documentation_source_tests.exe <repository> <accepted-RTL-runtime> <verified216-inputs> <retained-profile-runtime> <retained-Windows-compiler-directory> <explicit-compiler> <fresh-output-directory> <accepted35-directory> <accepted36-directory>
```

Each supplied directory must own `nyxcompilercontext.pas` and the complete
terminal `compiler-profile.log`. Starting from the original reconstructed
33-name request and archived reply, the prepared path restores both exact
canonical inputs. It requires unchanged identity, source mode, pristine scope,
ordered symbol prefixes and all four typed size operands in their original
order. Each reply must retain the complete earlier facts and observed-default
anchor through `ReadCapture` and `Merge`. The independent bootstrap is attached
separately; source settings and ordinary unit facts never become root evidence.
The three accepted logical symbol jobs count against the existing four-job
ceiling. Prepared mode launches no native symbol recollection.

The budgets remain separate and explicit:

| Boundary | Unchanged allowance and charging |
| --- | --- |
| Current preparation | One four-million budget for canonical restoration, terminal capture reading, source-origin proof, required-only planning and any admitted fourth symbol capture. Each plan is capped by the remainder and its exact detached work is debited once. |
| SysUtils semantics | Initial prepared-profile construction and its post-root or post-symbol continuation share four million work units and 64 logical passes. The second constructor receives only the measured remainder. |
| Rebuilt System and ObjPas | Each real namespace retains its existing per-module four-million/64 allowance; these are not one new global allowance and do not replenish SysUtils. |
| Windows | Preserve its original complete plain-translation contract. Report actual token/lexical work without inventing semantic answers or passes. |
| Source views and syntax | Existing two-million full-view and 100,000-declaration ceilings remain in force. |
| Dynamic primitive/source facts | The six original dynamic jobs remain counted; one additional generic fulfillment or replay of that accepted reply retains the logical seven-of-eight count. Replaying creates no new fact job. |
| Initial-symbol jobs | Archived33 plus accepted35/36 are three of four. Profile-first mode may capture only one further cumulative request derived from the actual first SysUtils result, preserving every old name and all four size operands. |

In the nine-argument form, the initial prepared SysUtils result must expose its actual single typed
`Longword` primitive requirement. The consumer constructs no fixed primitive
query list. Existing generic fulfillment preflights that requirement against
`Requirement.OwnerSource.Source.Files`, the exact original 156-file System
owner, and its captured module/context/bootstrap identity. A different typed frontier,
another required symbol cohort, mismatched capture or exhausted allowance refuses
before that collection. All provisional source/syntax diagnostics remain visible
and subject to final completeness refusal. See
[typed missing compiler requirements](pascal-projects.md#typed-missing-compiler-requirements).

After a validated reply, updated immutable descriptors feed a new empty world
in System, ObjPas, Windows order. Each old checked view supplies its own exact
source catalog and search policy: System156, ObjPas156 and Windows28. The old
views remain unchanged; pending semantic rebind is not relaxed. The complete
original counts remain mandatory: System 13,101 declarations/31 answers,
ObjPas 209/one and Windows 29,823 declarations. Namespace receipts and SysUtils
work/pass totals are printed. Candidate root, modules and world reach the
consumer's retained output only after the unchanged final SysUtils completeness
assertion. Further requirements remain failures; no second fact job or Classes
overlay is implicit.

The ten-argument profile-first form compiles; its full journey remains unqualified:

```text
nyx_documentation_source_tests.exe <repository> <accepted-RTL-runtime> <verified216-inputs> <retained-profile-runtime> <retained-Windows-compiler-directory> <explicit-compiler> <fresh-output-directory> <accepted35-directory> <accepted36-directory> <accepted-primitive-directory>
```

The last directory owns the already accepted `system.pp` and complete
`compiler-profile.log`. The original six-fact source-witness replay runs first,
preserving its source-v1 keys. Canonical restoration validates the bootstrap
request's identity, FPC mode, ordered guards and sole bare typed size operand.
The existing checked System type owner supplies a real simple alias using that
operand; `TypeOfSymbol` must derive its current compiler-root requirement at the
alias's exact type span and physical origin. Generic fulfillment planning and
`ApplyReplies` validate original156 ownership, the complete prior/default anchor
and exactly one added fact. The raw retained reply first passes through
`MergeNyxFPCSizeCapture` with the prior owner, producing the complete one-fact
delta expected by generic fulfillment. No compiler runs for this replay. A fresh checked
System/ObjPas/Windows world then precedes the first SysUtils construction.

If that result requests genuinely unknown initial symbols, its existing
required-only plan may form the one fourth cumulative request. The actual reply
must preserve all 36 prior names, their facts, the four original size operands,
the complete default anchor and the separate updated bootstrap. Only then can a
second SysUtils construction use the measured remaining work and passes. There
is no fifth symbol job, third construction, lookahead pool, new primitive job or
implicit Classes overlay. A missing eligible cohort or any later unmet
source/type/import/quota prerequisite retains final failure and earlier output;
the artifact from an admitted capture remains retained. Earlier preparation25
work and failure remain historical evidence, not a refund to these counters.
Full fit and publication require actual qualification of this new path.

Preparation26 runs the native compilertrace consumer with all accepted 35/36
request sources and the accepted bootstrap source/log: 521 checks pass with a
clean heap. Its browser consumer compiles. The new source consumer compiles for
native and browser; the same native binary refuses a missing ten-argument input
before check zero, creates no output and frees its heap. This qualifies request
restoration and compilation, not the actual alias-derived requirement, namespace
replay or complete source package.

Preparation27's ten-argument journey from that unchanged binary fails at check88
before a fresh world or SysUtils construction. The alias-derived requirement
reaches generic `ApplyReplies`, but the consumer supplies the raw retained reply
instead of the complete one-fact delta required by that owner. The exact
more-than-requested-fact guard refuses it; no context or scope defect is inferred.
All 23,387,105 allocated blocks/15,959,424,652 cumulative bytes are freed, and no
new output, fact or symbol job is created. The narrow consumer repair uses the
same existing size-merge owner as ordinary fulfillment, preserving every strict
comparison, limit and final assertion. Preparation28 qualifies that boundary:
the existing compilertrace consumer passes 525 native checks with a clean heap,
including the portable partial-packet versus complete-delta regression and the
accepted 35/36 request/bootstrap source-log inputs. All four trace/source native
and browser compiles pass with zero owned warnings and 14 upstream warnings.
These focused receipts retain preparation27's failure separately.

Preparation29 executes the same compiled ten-argument consumer through the
repaired replay. Its real `DWord` alias derives `Longword` at systemh.inc
scalar182/origin40; the saved four-byte reply preserves the exact original156
owner, complete defaults, earlier facts and one-fact delta. No primitive is
recollected. New checked namespaces retain System 13,101 declarations/31 answers,
ObjPas 209/one and Windows 29,823 declarations. The actual fourth symbol cohort
contains 57 names, adding 21 to the exact 36-name prefix while retaining all four
size operands, default anchors and the independent bootstrap.

The unchanged whole-source gate still fails at check90 with a clean heap:
SysUtils reaches 6,145 declarations, zero syntax diagnostics, nine source
diagnostics and eight unresolved conditions. Its two constructions consume
3,849,398 of four million shared work units and 56 of 64 logical passes;
preparation consumes 617,721 of four million. Symbol jobs are four of four and
dynamic facts remain seven of eight, including the replayed saved primitive.
All 33,449,874 allocated blocks/18,594,683,708 cumulative bytes are freed.

The first remaining condition is `not declared(MaxDirs)` in fina.inc scalar9,698.
Its actual binding issue is `pbiMissingModule`, related to `windirs`: SysUtils's
implementation imports WinDirs, but the checked world has not yet constructed
that namespace. Its physical root is already retained; this is not an unknown
compiler symbol or an uncaptured WinDirs source. Missing imports cannot prove a
name absent and cannot be skipped. The complete source/package gate remains
open. No fifth cohort, third SysUtils construction, primitive recollection or
Classes overlay is admitted by these receipts. Browser evidence remains
compilation only.

Preparation22's full nine-argument journey fails at check88 before any native
fact job. Both supplied canonical profiles restore successfully, consuming
133,956 and 134,601 work units respectively; with the original plan, current
preparation totals 335,668 of four million. The original namespaces remain
checked: System 13,101 declarations/31 answers consumes 3,913,992 work and 32
passes; ObjPas 209/one consumes 112,955 work and two passes. Windows retains
29,823 declarations under its original plain-translation contract.

The prepared SysUtils result exposes an earlier genuine frontend frontier:
5,086 declarations, 1,444,575 work, ten passes/nine answers, and
`pbiIncompleteSource` related to `integer` at syshelph.inc scalar80,570 inside
`TIntegerHelper`, for `sizeof(Integer)>=4`. There are zero typed compiler
requirements, seven unresolved source diagnostics and two unclosed helper
END/declaration diagnostics. The pre-collection guard refuses this incomplete
source instead of measuring an inferred primitive. No native capture directory
is created; all 24,013,227 allocated blocks/15,635,220,013 cumulative bytes are
freed. The updated-root world and final source package are not reached, so their
fit and publication remain unproven. The next review follows the actual
helper-prefix/scope/binding ownership without weakening incomplete-source or
unknown-shadowing refusal.

The preparation19 failure, exact accepted 35/36 requests/replies and all preceding
failures remain retained in [WORK.md](../WORK.md). Portable compilation remains
compile-only evidence; it establishes no browser execution or native target
provenance.

## Registered browsing and source maintenance

Ordinary registered help reads immutable descriptors, chunks, packages and
registration snapshots. It does not load source files, open a compiler, execute
controls or construct a semantic world. Source capture is a separate maintenance
path that produces those values from checked declarations and explicit target
facts. Native metadata cannot be relabeled as universal or browser metadata;
the browser target needs its own genuine source/context evidence and executed
reference-consumer qualification.

The current generated `nyx.controls.documentation` companion maps a supplied
complete package to creators. It also exposes the maintenance-only source
selector, so its interface currently imports `nyx.documentation.source` and
`nyx.pascal.binding`. Emitting complete ordinary Pascal descriptors and splitting
that selector from the runtime package/mapping dependency surface remain
downstream work. Both must use the same declaration owner and generator IR,
rather than a parallel help inventory. Deterministic emitted content should
recreate shared chunks/packages once and retain that sharing for all bindings;
process-local allocation identities are not persistent source identities.

The current full-source adapter already filters namespace recursion to public
imports before the actual implementation boundary, but it first requires a
complete resolved full-unit view. Private implementation conditions or includes
can therefore still block public extraction. Existing binding export-boundary
proof is evidence for a future explicit declaration-only owner, not permission
to relax this adapter's completeness gate or skip missing public imports that
could shadow earlier names. Preserve the full resolver journey and its original
assertions while assessing that narrower maintenance boundary. Portable
descriptors and automatic registered discovery remain owned by
`NS-4_studio-reference_01` and `NS-4_studio-reference_02` in the
[existing task catalog](../TODO/README.md); neither is complete at this checkpoint.

## Owned module signature sessions under qualification

`NewNyxDocumentationSignatureSource(View, Construction)` accepts only the exact
Nyx translated-view implementation with complete resolved translation and
diagnostic-free complete module syntax. Its
`INyxDocumentationSignatureSource.CaptureDeclared` takes the managed
`TNyxDocumentationRoutineOrdinals` vector; an ordinal identifies a row only in
this exact snapshot. One session shares its text-window cursor across repeated
selections of up to the existing 128-member ceiling. It constructs detached
parameter/default/result facts from the original checked spans using the
existing type grammar and rich-signature owners. It does not infer bindings,
creator roles, member identity, compiler facts or physical-source authority.

```pascal
Session := NewNyxDocumentationSignatureSource(View, Construction);
try
  Signatures := Session.CaptureDeclared(SelectedOrdinals);
finally
  Session.Retire;
end;
```

The source adapter retains exactly one such session per visited module and
retires it before package publication. A session holds source and construction
leases only during construction; published signatures retain neither. Caller
array/interface leases precede outward admission. Shared element mutation,
cancellation or reentrant retirement refuses atomic publication. Every copied
type/default window, detached field and actual cursor scan still consumes the
same caller allowance; backward windows pay for real rescans. There is no
independent allowance per class or callable group.

This addresses a proven adoption cost rather than changing quotas. The existing
independent snapshot batch rechecks its complete source once per call. The five
unconditional controls includes alone contain at least 99,348 significant
routine-header characters. At least 82 selected scopes would charge
`82 * 99,348 = 8,146,536` work just for those repeated comparisons, above the
unchanged two-million default before other work. The new exact-owned path needs
no alternate-supplier reparse or complete-text comparison for immutable rows.
The old single/batch snapshot APIs keep every existing validation and work
receipt; alternate/delegating views cannot enter the session path.

The existing signature-batch consumer now also contains the portable session
lifetime/mutation/byte/work refusal cases and the mandatory actual complete
controls-unit witness over its five original includes. Its native invocation
remains:

```text
nyx_documentation_signaturebatch_tests.exe D:\Docs\GitHub\nyx
```

That witness attempts all 89 public globals and every public type's callable
group through one session under the unchanged default construction. It is
signature-only: names remain unbound, imports are not represented as resolved
metadata, and it publishes no partial package or reference index. The separate
source consumer keeps its full retained-namespace-to-package gate in all of its
explicit argument forms.
Preparation10 qualifies the current session/receipt path: the complete controls
unit supplies 89 globals, 190 public type groups and 1,312 routine signatures
across 191 selections, consuming 597,176 shared work under the unchanged
two-million ceiling. The existing batch consumer passes 6,084 native checks with
a clean heap; the expression consumer passes 305 checks over all nine mandatory
actual windows, and both compile for pas2js. Existing signatures94/formals140
also pass natively with clean heaps; the full source consumer compiles on both
targets. These targeted receipts belong to the source-integration packet in
WORK.md. Earlier repeated-prefix and linear-receipt quota failures remain
retained. This is signature-only evidence, not source-package allowance fit,
nominal closure, executed browser evidence or complete registration adoption.

## Compiler-root metadata under qualification

An actual compiler-root type has no lexical declaration to publish as a fake
`System` help leaf. Optional `INyxDocumentationCompilerRootBindings.RootAt`
therefore supplements the old nominal and scoped-formal slots. A root retains
its original `TNyxPascalSizeReference`, declared family and exact immutable
compiler context. Its versioned typed equality stream visits every field owned
by context `SameFacts`, including unknown states, configured/source intent and
the separately captured bootstrap channel. It claims neither unmeasured
configuration equality nor ABI, encoding, compiler execution or source origin.

The source adapter derives a root only from its exact owned semantic world:
`pbsBound`, `pstBuiltin`, `pioSystemBootstrap`, and the exact intrinsic context
owned by the real System module's initial context (direct bootstrap or its
explicit attached packet). A named source declaration keeps its nominal owner.
Missing measured facts still return the existing binding prerequisite; matching
spelling, family, a consumer context or ordinary `System.X` size cannot supply
root authority. Opaque encoded-string codepage values remain outside type-name
closure; the separately checked ANSI base uses the root channel when applicable.

`NewNyxDocumentationAliasDefinition` preserves an actual direct named/qualified
alias whose terminal type is a root. It keeps `ndtAlias`; the old `AliasTarget`
is undefined when no nominal projection exists. `WithAlias` explicitly replaces
that definition. Generic/compound aliases are not added by this boundary. Root
leaves may be ordinary type arguments, but cannot become class bases or the
nominal root of an implemented-interface relationship.

Root capture reserves the exact temporary and joined context-stream storage
before formatting. Per-declaration, shared construction and final registration
limits remain in force. The operation caches only exact captured root owners
under the same context/name identity; independently constructed equivalent
owners do not earn copied-storage reuse. Canonical immutable reuse does not
remove repeated logical declaration bytes or final registration framing costs.
No supplier, source, world, admission or cancellation scope survives in a root.
Text-vector growth is admitted before the existing owner's actual initial
16-slot allocation and each subsequent `Count * 2 + 16` capacity. Clearing a
root slot restores old absence bytes/work. A newly specialized alias replaced
by `WithAlias` keeps a nil optional capability, whose bounded observation may
still consume two work units; semantic/framing equivalence does not promise
identical work for that new subtype.

The existing declaration-expression consumer now includes controlled complete
Pascal source units, explicitly authored known/unknown root facts, cached alias
and result binding, real package registration/index publication, late supplier
mutation, cancellation, caller-slot retirement and exact byte-boundary refusal.
Preparation12's declaration-expression leaf reports 176 native checks over all
ten mandatory actual definitions, with 1,287,948 allocated/freed blocks and
50,752,550 bytes, zero unfreed blocks. Its pas2js consumer compiles. The earlier
preparation11 disposal-timing failure remains retained with a clean heap; the
fixture now checks destruction after the bounded caller procedure's hidden
interface temporaries retire. The complete preparation12 group failed the
unchanged member-intent consumer at check62 because a public attachment leaked
the lower type-expression cancellation exception. Its clean-heap failure remains
retained. Preparation13 qualifies the narrow public repair against the unchanged
member consumer: 122 checks/all seven actual members pass with a clean heap.
The interface-attachment fixture's separate nil-vector assertion failure is also
retained; preparation14 corrects that assertion and passes 178 checks/all three
actual relationships with a clean heap. Their browser consumers compile only.
These controlled cases are mechanics inputs, not archived SDK facts or
substitutes for the full native source consumer. Genuine dependency closure,
requested facts, unsupported source forms and complete package quota fit remain
required. No browser execution or full acceptance is claimed by this integration.
