# Typed component documentation

[Components](components.md) · [Descriptor API](../src/nyx.documentation.pas) ·
[Creator facts](../src/nyx.documentation.schema.pas) ·
[Declaration fixture](../tests/nyx.test.documentation.pas) ·
[Current qualification](../WORK.md)

Nyx has an ordinary Pascal API for immutable documentation declarations. It
describes a unit, class, public control interface, record, enum, alias, intrinsic
family or callback;
members retain typed parameter/result references, summaries, examples and related
links. An optional adapter binds that declaration to an exact creator kind and
copies the existing schema and catalog facts. Reading documentation grants no
editing, compiler, file or callback authority.

This is a foundation for the open Studio reference task
`NS-4_studio-reference_01`. Descriptors are currently authored
metadata beside their declarations. They are not automatic compiler reflection,
and an edited method does not yet regenerate its descriptor. Existing schema,
callback and catalog registrations now accept their declaration bundles; an
immutable reference index admits only a complete supplied documentation closure.
The genuine extension currently refuses that gate because its core dependencies
have not adopted declaration-owned metadata.

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

## Unit-owned factories and routines

Use `NyxDocumentationUnit` for a unit's actual global declarations. A unit root
has no type name or type generic arity; `IsUnit` distinguishes it from a type or
an absent reference. A global member has `IsMember = True`, `IsUnit = False` and
an owner whose `IsUnit` is true. `NyxDocumentationType(AUnit, '')` still refuses:
an empty type argument is not an alternative unit factory.

The fixture declares a real `NewDocumentationBadge` global factory and two
`DocumentationGreeting` function overloads. `DocumentationUnitReference`
describes them beside their source. Its factory metadata follows this pattern:

```pascal
LUnit := NyxDocumentationUnit('nyx.test.documentation');
LGlobals := NewNyxDocumentationType(LUnit, ndtUnit,
  'Create friendly badges and format their greetings.')
  .WithMember(NewNyxDocumentationMember(
    NyxDocumentationMember(LUnit, 'NewDocumentationBadge'), ndmFactory,
    'Create a badge with its stable component identifier.')
    .WithSignature(NewNyxDocumentationSignature([
      NyxDocumentationParameter('AID',
        NyxDocumentationType('nyx.text', 'TNyxText'), ndpConst)],
      NyxDocumentationType('nyx.test.documentation', 'IDocumentationBadge'))));
```

`ndmRoutine` denotes an ordinary unit procedure/function; `ndmMethod` remains a
type-owned method. Units may own factories, routines and global fields. They
cannot own constructors, fluent class methods, properties, events or enum values.
The fixture's zero-argument and named greeting overloads have separate explicit
`Default` and `Named` overload keys. For example, the named overload's key is
`nyx.test.documentation||0|documentationgreeting|named`; its empty type component
does not collide with an existing type/member key.

A unit can be a related reference or global member owner. It cannot serve as a
parameter type, function result type, property/field value type, base type or
alias target. A unit descriptor cannot inherit a base or masquerade as a class;
a class identity cannot masquerade as `ndtUnit`. Existing record fields/layouts,
interface GUIDs/method slots and earlier enum ordinals remain unchanged.

`CaptureNyxDocumentationType` freezes unit descriptors through the same bounded,
cancellable immutable path as type descriptors. Reading `DocumentationUnitReference`
does not invoke the factory or either greeting overload. The examples remain
inert text. Actual factory/routine execution belongs to an explicit caller, not
documentation lookup. Globals enter discovery only through a bundle attached to
their real unit/creator registration. No reflection probe enumerates them.

## Intrinsic families and target-specific aliases

`NewNyxDocumentationIntrinsic(Identity, Family, Summary)` retains the exact
qualified type identity and an explicit Boolean, integer, real, character,
string or pointer family. The optional `INyxDocumentationIntrinsic` capability
exposes that family. It supplies no guessed width, encoding, conversion,
representation or runtime reflection facts. `ndtIntrinsic` appends to the old
type-kind ordinals; `NewNyxDocumentationType(..., ndtIntrinsic, ...)` refuses
because it lacks a family. Intrinsic declarations may retain prose, examples and
related references; they cannot invent bases, aliases or instance members.

Use a target alias when the declaration actually differs by platform. This
helper takes already established references rather than inventing System types:

```pascal
function PlatformAlias(const Identity, NativeType,
  BrowserType: TNyxDocumentationRef): INyxDocumentationType;
begin
  Result := NewNyxDocumentationTargetAlias(Identity,
    'Retain each declared platform target.', [
      NyxDocumentationAliasTarget(npfNativeLCL, NativeType),
      NyxDocumentationAliasTarget(npfBrowser, BrowserType)]);
end;
```

The copied branch record contains a platform and exact type reference, with no
interface or executable authority. `INyxDocumentationAliasTargets` exposes
`TargetCount`, `TargetAt`, `HasTarget` and `TargetFor`. One specific branch is
valid for a platform-only declaration; `TargetFor` refuses an absent projection.
Branches sort deterministically, reject duplicates or an overlapping `npfAny`
case, and retain at most two cases. An explicit `npfAny` branch declares a
universal target. Two specific branches establish a common target only when
their exact qualified references agree. Otherwise the old `AliasTarget` getter
stays undefined and `HasTarget(npfAny)` is false. There is no fallback from one
projection to another. Calling the existing `WithAlias` explicitly replaces a
specialized alias's complete map with one universal target.

Every fluent copy and alternate-provider capture retains these specialized
capabilities. Capture reconstructs derived target queries from bounded copied
rows, checks an advertised common target against those rows, and retains no
supplier. Family and branch facts share the existing 256 KiB declaration limit
and participate in exact framed registration equality and its 4 MiB bundle
allowance. Per-platform graph traversal consumes the same index work ceiling;
the 4096 declaration-occurrence, 16 MiB byte and two-million work bounds remain
unchanged. Unextended declaration
framing remains unchanged; existing interface GUIDs and method slots, copied
record fields and old enum ordinals remain intact.

The index requires every provided branch link to resolve. Explicit registration
availability also requires a branch for every promised platform; unspecified
availability does not invent one. Alias/base cycles are checked separately for
each concrete platform, so a cyclic union of mutually exclusive branch edges
does not become a false cycle. Inheritance kinds are checked on the owner's
declared projections. Target-neutral `Members` refuses inherited traversal of
an alias without a common target; select `TargetFor(platform)` and query that
exact owner instead. This is declared graph validation, not compiler type or
cross-platform compatibility proof.

The [shape consumer](../tests/nyx_documentation_shapes_tests.lpr) passes **66
native checks with a clean heap** and **compiles for pas2js**, with zero owned
warnings. It uses explicitly synthetic declarations to test these mechanics.
It does not supply placeholder descriptors for real System/core dependencies or
replace the genuine extension's still-incomplete closure. These checks cover
all six families, specialized fluent/capture lifetimes and bounds, target
absence/common truth, malformed producers, atomic cancellation, framed
conflicts, per-platform cycles, availability and inherited base kinds. No new
decorated browser has been executed.

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
| One unit or type declaration | At most 128 members. Class/interface/callback declarations admit at most 32 base links; units admit none. |
| One signature | At most 64 parameters; duplicate Pascal parameter names refuse. |
| One unit, type or member | At most 8 examples and 32 related links. |
| Names and prose | At most 256 scalars per identity component, 2,048 per summary, 256 per example title, 16,384 per example body and 1,024 per parameter default. Type generic arity is 0 through 32. |
| One captured declaration | 256 KiB aggregate UTF-8 content, including members, signatures, examples and links. |
| Schema search | At most 4,096 captured kinds; creator names at most 256 scalars. |
| One schema binding's copied facts | At most 128 properties, 128 events and 128 routes per event; a separate 256 KiB aggregate UTF-8 allowance covers schema/catalog fact text. |
| One registration bundle | At most 128 declarations and 4 MiB of length-framed UTF-8 declaration content. Existing per-declaration limits still apply. |
| Callback/catalog owner capture | At most 8,192 rows each; capture accepts cancellation and retains no executable class/recipe. Existing registration capacity does not change. |
| One index publication | Defaults: at most 4,096 declaration occurrences, 8,192 total binding rows, 16 MiB of captured framed content/registration names and 2,000,000 field-admission/comparison/traversal steps. Fluent limits may lower these ceilings. |

The declaration allowance and schema/catalog allowance are separate. A binding
owns one captured declaration **plus** its bounded creator facts; it does not
share a single 256 KiB counter across both layers. These are metadata bounds,
not compiler, language-query or project quotas.

Capture functions accept an optional `INyxCancellationScope`. Cancellation,
invalid counts, malformed Unicode, duplicate identities, foreign member owners
or exhausted allowances refuse before publishing a captured result. Failed
captures return no partial interface. Text preserves supplementary characters,
NUL and CR/LF/CRLF exactly. Example/default text stays inert. No recursive base
or alias resolution runs at the descriptor layer; complete link and cycle
admission runs in the registration index described below.

## Discover through the actual registration owners

[Registration bundles](../src/nyx.documentation.registration.pas) are immutable
ordinary Pascal values. A bundle names its unit, an exact public type or global
factory member, and that unit's decorated declarations. `ForPrimary` returns a
new owner for another public entry in the same bundle. It never rewrites the
declarations' owners. A callback mapping requires a class/callback declaration;
a creator mapping may name its real global factory. Declared native/browser
availability is independent of schema capability grades and observed execution.

The fixture passes one bundle through its existing calls, conceptually:

```pascal
RegisterNyxSchema(Kind, Properties, Events, Documentation);
RegisterNyxCallback(NyxHandler('TDocumentationCallback'),
  TDocumentationCallback, Documentation.ForPrimary(CallbackType));
Catalog.RegisterKind(Kind, 'Friendly badge', 'Custom', False, Documentation);
```

No second `RegisterHelp` call, central help inventory or documentation page list
is populated. The fixture bundle includes the real unit factory/routine
overloads, class, specialized interface and callback descriptors. It retains all
links to `nyx.text.TNyxText`, `nyx.controls.TNyxBadge`/`INyxBadge`,
`nyx.events.TNyxEventCallback`, `nyx.behavior.TNyxEventInfo` and
`nyx.scheduler.INyxExecution`. Those core owners are currently undocumented.
The index therefore refuses this real extension closure and leaves a previously
valid index exact. Empty substitute core descriptors, stripped signatures or a
smaller replacement extension do not qualify that gate.

Optional `INyxSchemaDocumentation`, `INyxCallbackDocumentation` and
`INyxCatalogDocumentation` expose attachments beside the corresponding captured
row. Old registration overloads remain valid and expose nil attachments.
`CaptureNyxCallbacks` exports handler identities, never callback classes or
`Resolve`. `Catalog.Capture` exports copied palette facts, never `NewNode` or a
recipe tree. Schema attachments follow the exact current/scoped snapshot; they
are not added to its existing transport. A decoded schema snapshot explicitly
reports no local documentation generation and refuses index discovery. Its
existing schema execution/typed-inspection/transport behavior remains available.

Use the [caller-owned registry](../src/nyx.documentation.registry.pas) with three
already captured authority owners:

```pascal
Schemas := CaptureNyxSchemas;
Callbacks := CaptureNyxCallbacks;
Palette := Catalog.Capture(Cancellation);
Reference := Registry.Refresh(Schemas, Callbacks, Palette,
  Registry.Version, TNyxDocumentationIndexLimits.Defaults, Cancellation);
```

`Reference.Generation` is the schema revision, callback revision and catalog
instance identity/revision vector. It is not a transaction across the three
authorities. Catalog identities are process-local and cannot be compared across
native/browser processes. `Reference.Version` is the registry's separate local
publication sequence. Consumers retain an immutable owner; refreshing from
older captures intentionally indexes those captures, without asserting that
they are the current global registrations. A caller must obtain new captures
when it needs the latest generation.

The registry orders qualified declarations deterministically and resolves every
present type, unit, related/member, signature, base and alias reference. Matching
declarations from different real registrations deduplicate only when their exact
framed content and declared availability agree. Conflicts, missing links,
invalid base kinds, base/alias cycles, unsupported captures, cancellation,
exhausted allowances or a changed expected publication version refuse before
replacing `Current`. Schema and catalog documentation for the same creator must
name the same public primary. This agreement does not grant either registry
authority to register the other. Legacy undocumented rows remain explicit
`UndocumentedCount` coverage gaps; they do not become invented type metadata.

`FindDeclaration` and `FindMember` use exact qualified/overload identities.
`Members(TypeReference)` follows declared bases and aliases with bounded,
cancellable traversal. Inherited members retain their original owners;
independent overloads and shadowed declarations remain alternatives. This is
documentation enumeration, not Pascal overload selection or compiler proof.

Aggregate declaration counts and bytes charge every registration occurrence,
including identical duplicates. Framed text uses UTF-8 byte lengths on both
targets; fields are admitted before appending capture content. The index work
counter covers its field admissions, comparisons and graph traversal, while the
existing bounded per-declaration capture owns its internal copying/validation.
These are layered metadata bounds, not a promise that retained object overhead
or compiler/runtime string allocation equals the text byte allowance. All new
inspection work accepts cancellation; no cancellation scope or metadata supplier
is retained after capture.

The discovery boundary is linked/loaded units that actually executed their
ordinary startup/UI registration. Capturing an index never executes a class,
callback, factory or example. Registration is append-only: existing schema and
callback owners have no removal/unload API, and catalog ownership is local.
There is no dynamic module-unload contract. Immutable declarations and copied
indices remain valid after a catalog retires; callers serialize publication and
mutable catalog/startup registration, as their existing authorities require.

The [registry consumer](../tests/nyx_documentation_registry_tests.lpr) passes
**64 native checks with a clean heap** and **compiles for pas2js**. It covers
immutable alternate capture, actual registration attachments, exact overload and
alias ownership, conflicts, cycles, missing links/alias targets, cancellation,
aggregate limits, local publication reentrancy, source-generation refusal and
catalog-independent lifetimes. Its explicitly synthetic mechanics graph tests
the index algorithms. The complete real extension independently refuses its
currently undocumented core closure; that gate remains open. Inspection invokes
none of its control, callback, factory or routine bodies. The new decorated
browser consumer has never been executed.

## Qualification and capability boundary

The native [documentation consumer](../tests/nyx_documentation_tests.lpr) passes
**68 checks with a clean heap and zero owned warnings**. It retains the preceding
33-check type/schema foundation and adds unit ownership, real global factories
and routine overloads, invalid type/role refusals, unchanged identity/ordinal
contracts, immutable capture, bounded admission and cancellation. Inspection
invokes no documented control, factory, routine or callback. Separate explicit
callers exercise their public behavior and release all owners before checking
destruction. The browser consumer **compiles successfully**; the decorated-browser
consumer has never been executed. Compilation is not executed browser evidence.

The native compiler is **FPC 3.3.1-20634-gd7f522a561 [2026/08/04], i386 Win32**;
the browser compiler is **Pas2JS 3.3.1 [2026/08/04], Win32 i386**. Current focused
shape/registration evidence is in
`E:\Blekohlab\nyx\qualification\documentation-shapes-20261020-132053`:

- `logs/nyx_documentation_shapes_tests-native-focused-reviewed-{compile,run}.log`
- `logs/nyx_documentation_shapes_tests-browser-focused-reviewed-compile.log`
- `logs/nyx_documentation_registry_tests-native-focused-reviewed-{compile,run}.log`
- `logs/nyx_documentation_registry_tests-browser-focused-reviewed-compile.log`
- `logs/nyx_documentation_tests-native-focused-reviewed-{compile,run}.log`
- `logs/nyx_documentation_tests-browser-focused-reviewed-compile.log`

The shape run allocates and frees all 65,252 blocks; the registry run frees all
434,968 blocks and the unchanged declaration consumer frees all 583,651 blocks.
Each reports zero unfreed blocks. All three consumers have zero owned compiler
warnings; each browser compilation has seven existing upstream Classes warnings.
`source-reviewed-freeze-private.json` retains the six changed Pascal inputs for
this shared batch, including the four metadata sources and two separate
generator consumers. The same unchanged six-source freeze passes the broader
29-compiler matrix: 13 native test programs with clean heaps, 13 pas2js
compile-only consumers and zero owned warnings. The separate generator consumer
joins 14 actual maintenance invocations with clean child heaps and two exact
five-file candidates. The [current work record](../WORK.md) owns
`final-audit-private.json`, the 1729-input inventory and retained prior-pin
comparisons. This qualification does not establish executed decorated-browser
or GUI behavior.

The initial registry browser preparation rejected direct `as` syntax between
unrelated interface types. Its source/log remain retained. The final test uses
`Supports` and managed optional-capability outputs, preserving all 58 original
behavior checks and adding six explicit capability assertions. Native runtime
querying and browser compilation are separate evidence. The earlier registration
packet, `E:\Blekohlab\nyx\qualification\definition-history-20261020-125511`, retains
that failed preparation and its 64-check native/compile-only browser baseline.

The prior unit/global declaration baseline is commit
`78caee7790b6d323e7ae8d1b98dace987a2e4160`; its packet retains the first native
lifetime assertion failure with a clean heap. The repaired fixture checks the
same destruction contract after compiler-held interface temporaries leave the
explicit caller's lifetime. The preceding 33-check foundation remains qualified
separately at commit `2a3226de25c8af8fb2652a676ed14897b1016c91`.

The earlier [runtime membership
adapter](../src/nyx.linq.runtime.pas) and its
[consumer](../tests/nyx.test.linq.combinations.pas) provide separate evidence,
recorded in WORK under **Typed combinations, runtime membership and positions —
2026-10-12** on the installed 3.3.1 compilers. They do not establish broad
documentation reflection:

| Capability | Native FPC | pas2js browser | Documentation use |
| --- | --- | --- | --- |
| Ordinary typed descriptor/registration interfaces | Current declaration68, registry64 and shape66 consumers pass with clean heaps. | All three consumers compile successfully; decorated execution remains open. | Ordinary Pascal contracts; no special decorator syntax. |
| Runtime class ancestry | Earlier executed runtime adapter evidence. | Earlier executed runtime adapter evidence. | Existing adapter only; no class inventory is inferred. |
| Declared nonzero-GUID interface querying | Earlier executed runtime adapter evidence. | Earlier executed runtime adapter evidence. | Optional capabilities use explicit GUIDs. |
| GUID-less runtime interface identity | Unsupported by the earlier native query adapter; it refuses. | Earlier executed evidence finds a compiler-generated identity. | Browser-generated identity stays runtime-local, not a portable registration key. |
| Public member/signature enumeration and visibility coverage | Unqualified. | Unqualified. | No universal RTTI enumeration adopted. |
| Generic declaration/constructed argument reflection | Unqualified. | Unqualified. | Authored type arity and explicit references only. |
| Compiler/linker metadata retention and discovery of unavailable units | Unqualified. | Unqualified. | No promise of enumerating every class or unlinked unit. |

## Open reference work

Unit/global identities now represent creator functions without inventing a
containing class. Adoption for the actual `NewNyx…` creator inventory remains
open. Registration-owned discovery and immutable index generations are now
implemented for supplied decorated closures; actual extension adoption is gated
on missing core declarations and generator adoption. Contextual Studio browsing remains open
under reference discovery (`NS-4_studio-reference_02`), reference UI
(`NS-4_studio-reference_03`) and completeness/compatibility
(`NS-4_studio-reference_04`).

The existing [Pascal control contract
generator](../tools/nyx_control_contracts.lpr) already owns generated specialized
interfaces and facades. Emitting their documentation at those same declaration
points is a future path to one source of truth; that adoption is not implemented
here. The existing catalog export constructs nodes and is not runtime discovery.
The current generator receipt contains **81 built-in kinds and four facades**.
Under full-unit factory-primary attachments, repeating one whole unit descriptor
and its 81 distinct result interfaces for all 81 creators already costs
`81 * (1 + 81) = 6642` declaration occurrences, above the unchanged 4096 ceiling
before classes, facades or core dependencies. Imports alone cannot fix that
ownership model. The next prerequisite is canonical immutable source chunks
owning copied declarations, flat packages retaining those chunks and ordinary
creator bindings referencing their package and exact primary. Exact captured
chunk ownership, package/chunk/binding reference costs and legacy bundle charges
must remain separate; equivalent independent copies still charge fully. This
architecture is not implemented here. Actual adoption still requires a complete
source-derived emitter/core closure, genuine registration integration and
executed decorated-browser evidence, without omitted facts or raised ceilings.

The required truthful declaration prerequisites are explicit:

- Intrinsic/scalar/string families and target-specific alias shapes now have an
  explicit representation. Actual System/core declarations still need their
  source-owned metadata; Boolean, Integer and string must not become invented
  enum/record descriptors or empty leaves merely to satisfy a missing link.
- `TNyxText` has native `UTF8String` and browser `String` alias targets. Its actual
  metadata adoption must retain both source-owned branches; the new shape API
  does not itself register that declaration or manufacture a common target.
- Core text/scheduler units already sit below the documentation contract.
  Source-owned generated companion metadata must preserve that dependency
  direction. Imported bundles must retain each original unit owner, remain
  bounded and flow through actual registration attachments.
- Indexed properties such as `INyxControl.Children[AIndex: Integer]` need typed
  index parameters and declared access information. Omitting the parameter or
  relabelling the property as a method would conceal the API.

Generator adoption must derive metadata at the existing declaration emission
points, including hand-authored dependencies and deterministic per-creator/unit
chunks within current allowances. Generator maintenance now prepares a separate
candidate-output/check workflow, documented in [managed controls](managed-controls.md).
That tooling does not yet emit documentation. Adoption must produce and review
scratch candidates before replacing source after all readers join. No generator
has been run for this metadata integration, and no parallel manually authored
core-help inventory is introduced.

Executed decorated-browser evidence, a complete supported-version capability
matrix, independent API coverage and the full original Studio/library acceptance
remain required. No reference task, criterion, completion credit or DONE status
closes from this contract or its assertion count.
