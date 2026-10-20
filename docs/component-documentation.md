# Typed component documentation

[Components](components.md) · [Descriptor API](../src/nyx.documentation.pas) ·
[Creator facts](../src/nyx.documentation.schema.pas) ·
[Declaration fixture](../tests/nyx.test.documentation.pas) ·
[Current qualification](../WORK.md)

Nyx has an ordinary Pascal API for immutable documentation declarations. It
describes a class, public control interface, record, enum, alias or callback;
members retain typed parameter/result references, summaries, examples and related
links. An optional adapter binds that declaration to an exact creator kind and
copies the existing schema and catalog facts. Reading documentation grants no
editing, compiler, file or callback authority.

This is a foundation for the open Studio reference task
`NS-4_studio-reference_01`. Descriptors are currently authored
metadata beside their declarations. They are not automatic compiler reflection,
and an edited method does not yet regenerate its descriptor or register it in a
reference index.

## Keep the declaration and descriptor together

The compiled fixture declares `TDocumentationBadge`, its specialized
`IDocumentationBadge` interface and a callback in one Pascal unit. Its static
`Reference` method can describe the class without constructing a badge. This
excerpt shows its fluent method's metadata; the complete fixture also describes
the constructor, property, base class, interface and callback signature.

```pascal
function GreetingMember(
  const AOwner: TNyxDocumentationRef): INyxDocumentationMember;
begin
  Result := NewNyxDocumentationMember(
    NyxDocumentationMember(AOwner, 'WithGreeting'), ndmFluentMethod,
    'Set the greeting and retain this specialized badge.')
    .WithSignature(NewNyxDocumentationSignature([
      NyxDocumentationParameter('AName',
        NyxDocumentationType('nyx.text', 'TNyxText'), ndpConst)],
      NyxDocumentationType('nyx.test.documentation', 'IDocumentationBadge')));
end;
```

`NewNyxDocumentationType` composes members with `WithMember`, bases with
`WithBase`, and inert Pascal samples with `WithExample`. `OfType` supplies a
property, field, event or enum-value type. `WithAlias` retains an alias's original
identity separately from its target. `WithRelated` adds qualified links. Each
fluent call returns a new descriptor; earlier readers keep their original value.

`TNyxDocumentationRef` keeps unit name, type name, type generic arity, member name
and overload key separate. Comparison folds ASCII Pascal name casing, preserving
the unit/type boundary and the original declaring owner. Names are bounded ASCII
Pascal identifiers; overload keys are explicit identifiers, not inferred
signatures. This contract does not describe constructed generic arguments or
claim a universal Pascal identifier grammar.

An absent member signature means unknown. An empty declared signature means a
known call with no parameters. An absent signature result means no declared
function result; the constructor member kind retains constructor ownership.
Unknown value types and missing alias targets remain absent. A signature default
is exact inert Pascal text, not an evaluated value. Reading an example never
compiles or runs it.

An existing control may expose `INyxDocumentationSource`. Its ordinary control
interface and class remain intact. Inspection must not construct an arbitrary
control just to ask whether it supports this optional capability. Static
declaration methods, such as the fixture's `Reference` and `InterfaceReference`,
provide the construction-free entry point for future registration.

## Capture existing creator facts

Use `CaptureNyxDocumentationType` before retaining an alternate descriptor
producer. It copies the bounded declaration graph rather than retaining the
producer, document or renderer. Bases, aliases and related links remain stable
data references; inherited members keep their original declaring owner.

The schema adapter takes an existing captured environment and an optional
existing catalog. In the fixture, startup first calls
`RegisterDocumentationFixture(ACatalog)` once. A later inspection can use:

```pascal
function CaptureFriendlyBadgeHelp(
  ACatalog: TNyxCatalog): INyxDocumentationSchema;
var
  LAPI: INyxDocumentationType;
  LSchemas: INyxSchemaSnapshot;
begin
  LAPI := TDocumentationBadge.Reference;
  LSchemas := CaptureNyxSchemas;
  Result := CaptureNyxDocumentationSchema(LAPI,
    NyxCustomKind(DocumentationFixtureKind), LSchemas, ACatalog);
end;
```

The example uses `nyx.documentation`, `nyx.documentation.schema`, `nyx.schema`,
`nyx.catalog`, `nyx.types` and the declaration fixture. Production extensions own
their corresponding declaration and registration in their own unit. The
caller owns and synchronizes the catalog; capture never creates one. Schema
registration and documentation inspection are separate operations.

`INyxSchemaInspection` is an optional capability on the existing immutable
`INyxSchemaSnapshot`. Its typed getters return detached property/event values;
invalid ordinals refuse and `FindKind` returns `-1` when absent. The old snapshot
GUID, methods and transport remain unchanged. A supplied alternate snapshot
without typed inspection refuses explicitly. The documentation adapter does not
call schema `Execute`, decode schema snapshot transport, call catalog `NewNode`
or resolve a callback implementation.

The binding preserves these authorities:

| Information | Owner and precedence |
| --- | --- |
| Public API identity, signature, bases, aliases, examples and related links | Authored declaration. `Evidence` is `ndeAuthoredDeclaration`. |
| Property defaults, choices, bounds and declared support | Exact captured creator schema; no replacement facts are inferred from prose. |
| Named event payloads, contexts and physical routes | Existing typed event/payload owners, copied before retention and return. |
| Binding summary | Nonempty catalog `Discovery.Description`, otherwise the authored declaration summary. `SummaryEvidence` reports that choice. |
| Primitive support grades | Existing `FindNyxPrimitive` metadata for the exact requested creator kind. A custom creator is not guessed to be a primitive. |

`HasSchema` distinguishes a missing registration from a registered empty schema.
`FactsEvidence` returns `ndeSchemaDeclaration` only when a schema is present.
`HasCatalog` and `HasPrimitive` separately describe their available facts;
requesting an absent catalog or primitive value refuses. Captured schema/catalog
support grades describe declarations, not observed browser or LCL behavior.
Binding a supplied API to a kind is explicit authored input; capture does not
verify or infer a concrete implementation class.

Returned metadata retains no catalog, schema producer, control, document,
renderer, cancellation scope or executable callback. Strings are immutable;
nested event route/payload vectors are detached. Later registration or edits to
a returned record cannot mutate an earlier capture.

## Bounds and cancellation

The current fixed limits are part of the inspection contract:

| Layer | Allowance |
| --- | --- |
| One type declaration | At most 128 members and 32 base links. |
| One signature | At most 64 parameters; duplicate Pascal parameter names refuse. |
| One type or member | At most 8 examples and 32 related links. |
| Names and prose | At most 256 scalars per identity component, 2,048 per summary, 256 per example title, 16,384 per example body and 1,024 per parameter default. Type generic arity is 0 through 32. |
| One captured declaration | 256 KiB aggregate UTF-8 content, including members, signatures, examples and links. |
| Schema search | At most 4,096 captured kinds; creator names at most 256 scalars. |
| One schema binding's copied facts | At most 128 properties, 128 events and 128 routes per event; a separate 256 KiB aggregate UTF-8 allowance covers schema/catalog fact text. |

The declaration allowance and schema/catalog allowance are separate. A binding
owns one captured declaration **plus** its bounded creator facts; it does not
share a single 256 KiB counter across both layers. These are metadata bounds,
not compiler, language-query or project quotas.

Capture functions accept an optional `INyxCancellationScope`. Cancellation,
invalid counts, malformed Unicode, duplicate identities, foreign member owners
or exhausted allowances refuse before publishing a captured result. Failed
captures return no partial interface. Text preserves supplementary characters,
NUL and CR/LF/CRLF exactly. Example/default text stays inert. No recursive base
or alias resolution runs at this layer; complete link and cycle admission belongs
to the future index.

## Qualification and capability boundary

The current native [documentation
consumer](../tests/nyx_documentation_tests.lpr) passes **33 checks with a clean
heap and zero owned warnings**. It covers typed declarations, exact text,
immutable fluent values, same-variable capture, cancellation, bounded alternate
schema producers, detached routes, missing/empty schemas and control lifetime.
An explicitly constructed badge exercises the specialized interface separately
from metadata lookup. Its browser consumer **compiles successfully**; the new
decorated-browser consumer has never been executed. Compilation is not executed
decorated-browser evidence.

The final native logs identify **FPC 3.3.1-20634-gd7f522a561 [2026/08/04], i386
Win32**. The browser compiler is **Pas2JS 3.3.1 [2026/08/04], Win32 i386**. The
qualification packet retains `nyx_documentation_tests-qualified-compile.log`,
`nyx_documentation_tests-qualified-run.log` and
`nyx_documentation_tests-browser-qualified-compile.log` beside the exact final
`source-qualified-freeze-private.json` receipt. The documentation sources remain
unchanged in this final common batch. Native compilation has no warnings;
browser compilation reports existing upstream Classes warnings. The
[current work record](../WORK.md) owns the packet,
whole-batch warning audit and retained preparation failures.

The earlier [runtime membership
adapter](../src/nyx.linq.runtime.pas) and its
[consumer](../tests/nyx.test.linq.combinations.pas) provide separate evidence,
recorded in WORK under **Typed combinations, runtime membership and positions —
2026-10-12** on the installed 3.3.1 compilers. They do not establish broad
documentation reflection:

| Capability | Native FPC | pas2js browser | Documentation use |
| --- | --- | --- | --- |
| Ordinary typed descriptor interfaces | Current native 33-check consumer passes. | Current consumer compiles successfully; decorated execution remains open. | Adopted ordinary Pascal contract; no special decorator syntax. |
| Runtime class ancestry | Earlier executed runtime adapter evidence. | Earlier executed runtime adapter evidence. | Existing adapter only; no class inventory is inferred. |
| Declared nonzero-GUID interface querying | Earlier executed runtime adapter evidence. | Earlier executed runtime adapter evidence. | Optional capabilities use explicit GUIDs. |
| GUID-less runtime interface identity | Unsupported by the earlier native query adapter; it refuses. | Earlier executed evidence finds a compiler-generated identity. | Browser-generated identity stays runtime-local, not a portable registration key. |
| Public member/signature enumeration and visibility coverage | Unqualified. | Unqualified. | No universal RTTI enumeration adopted. |
| Generic declaration/constructed argument reflection | Unqualified. | Unqualified. | Authored type arity and explicit references only. |
| Compiler/linker metadata retention and discovery of unavailable units | Unqualified. | Unqualified. | No promise of enumerating every class or unlinked unit. |

## Open reference work

Unit-level `NewNyx…` creator functions need an explicit unit/global declaration
identity; the current type-owned factory model must not invent a containing
class for them. Automatic registered-unit discovery, immutable index generations,
complete links/inheritance/overloads and contextual Studio browsing remain open
under reference discovery (`NS-4_studio-reference_02`), reference UI
(`NS-4_studio-reference_03`) and completeness/compatibility
(`NS-4_studio-reference_04`).

The existing [Pascal control contract
generator](../tools/nyx_control_contracts.lpr) already owns generated specialized
interfaces and facades. Emitting their documentation at those same declaration
points is a future path to one source of truth; that adoption is not implemented
here. The existing catalog export constructs nodes and is not runtime discovery.
Executed decorated-browser evidence, a complete supported-version capability
matrix, independent API coverage and the full original Studio/library acceptance
remain required. No reference task, criterion, completion credit or DONE status
closes from this contract or its assertion count.
