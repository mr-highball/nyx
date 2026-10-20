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

## Declared property syntax

`NewNyxDocumentationProperty` creates a property member with an optional
`INyxDocumentationProperty` capability. It keeps ordered index parameters,
authored read/write clauses and the separate default-property marker. The
existing member `OfType` supplies its declared value type. A caller that already
owns the exact references can describe an indexed property as follows:

```pascal
function IndexedItems(const Owner, IndexType,
  ValueType: TNyxDocumentationRef): INyxDocumentationMember;
begin
  Result := NewNyxDocumentationProperty(
    NyxDocumentationMember(Owner, 'Items'),
    'Read an item at its declared index.', [
      NyxDocumentationParameter('AIndex', IndexType)],
    [ndpaRead], True).OfType(ValueType);
end;
```

`ParameterCount` and `ParameterAt` expose copied parameter values through the
same parameter contract used by signatures, including passing modes and inert
default text. Indexing is not a callable signature: the property's existing
`Signature` stays absent, and `WithSignature` refuses. Ordinary properties have
zero index parameters. `DeclaredAccess = []` means neither clause was authored;
an inherited redeclaration can omit its value type and clauses without implying
effective runtime readability or writability. `DefaultProperty` records the
separate `; default;` member marker, not a property's scalar default value.
Getter/setter identities, effective inherited access and scalar property
defaults are outside this capability.

Legacy `ndmProperty` members without this capability retain unknown shape and
byte-identical old registration framing. Present shape adds framed facts and
all index-type references to duplicate comparison and closure validation.
Specialized fluent copies and immutable captures retain the capability;
alternate suppliers are copied, their bounded envelope and parameter values
are rechecked, and cancellation is checked before publication. Inspection
retains no supplier and executes no getter, control or callback. Units cannot
own properties or serve as their index/value types. The existing limits remain
64 parameters, 256 KiB per member/declaration, 4 MiB per legacy bundle/chunk,
and caller-owned aggregate index/package limits; individual parameter/name/text
limits do not bypass the shared byte ceiling.

The new [property consumer](../tests/nyx_documentation_properties_tests.lpr)
requires exactly one valid repository-root argument on native builds, before
performing any checks:

```text
nyx_documentation_properties_tests.exe D:\Docs\GitHub\nyx
```

It exercises portable shape/copy/capture/framing/index refusal cases and reads
the actual `INyxControl.Children[AIndex: Integer]: INyxControl read GetChild`
declaration through the existing syntax owner. That witness retains raw
`Integer` and `INyxControl` spelling; it supplies no guessed qualified System or
core type binding and does not execute `GetChild`. The browser consumer excludes
only filesystem inspection. The current focused native run passes **43 checks
with a clean heap**; all 770,296 allocated blocks are freed. Separate
missing/invalid repository arguments both refuse at check count zero with clean
heaps. The portable consumer compiles successfully. These receipts are in
`E:\Blekohlab\nyx\qualification\documentation-properties-20261020-143002`:

- `logs/nyx_documentation_properties_tests-native-focused-reviewed-compile.log`
- `logs/nyx_documentation_properties_tests-native-focused-reviewed-run.log`
- `logs/nyx_documentation_properties_tests-browser-focused-reviewed-compile.log`
- `logs/property-argument-missing-run.log`
- `logs/property-argument-invalid-run.log`

The same focused freeze retains native contribution70, shape66, registry64 and
documentation68 checks with clean heaps; all five portable metadata consumers
compile. The generator consumer passes71 checks across16 joined invocations,
retaining two exact five-file candidates and clean parent/child heaps. The final
common-source qualification passes33 compiler jobs,15 native test programs with
clean heaps and15 pas2js compiles, including both Studios, worker and backend as
applicable. It has zero owned warnings;105 upstream warnings remain separately
counted. [WORK](../WORK.md) owns the aggregate and remote receipt. These checks
establish no browser execution or actual metadata-generation/core-registration adoption.
The preceding qualification below remains separate.

## Compound parameter and result types

The richer contract has focused native and portable compilation evidence below.
It reuses
the existing [Pascal type grammar](../src/nyx.pascal.types.pas) over a copied
small lexical window; it introduces no second type tree or signature parser.
`INyxDocumentationTypeExpression` exposes that immutable syntax, exact inert
text and an explicit declaration occurrence. In particular `array of TValue`
in a parameter keeps its open-array occurrence, separately from the same text
in a type declaration. Structured bodies needing their original declaration
owner, incomplete grammar and untranslated directives refuse.

`INyxDocumentationTypeUse` retains either a real named reference or an
expression with ordered explicit bindings. It never substitutes an element
reference for an array or invents a named alias. Qualified designators and
generic bases/arguments retain their exact syntax; namespace components stay
inside their designator. String syntax remains an explicit unresolved use
until a truthful binding is supplied. Bounds and defaults remain opaque text,
without evaluating their identifiers or deriving fake type links. Missing
bindings remain inspectable and refuse complete index publication. No binder
infers unit ownership from a short name. Generic formal/nested ownership that
cannot be represented by the existing identities remains unresolved.

Managed `INyxDocumentationParameterDeclaration` values hold these type uses;
the old `TNyxDocumentationParameter` record and its strict named-type
constructor/capture are unchanged. `INyxDocumentationRichSignature` can mix
named and expression parameters/results. An optional
`INyxDocumentationMemberSignature.DeclaredSignature` attaches it to a member.
The old `Signature` is nil when named records cannot truthfully represent the
declaration; a wholly named rich signature can retain a coherent old projection.
`INyxDocumentationRichProperty` similarly keeps complex indices/value syntax,
authored access and default status without exposing invalid simple-property
records. `OfType` replaces the value use and preserves its indices.

`WithExample` and `WithRelated` keep richer specialization. Explicit
`WithSignature` replaces the old callable projection and clears stale rich
data; earlier readers retain their original declaration. Captures copy alternate
suppliers, recheck optional and legacy envelopes, and publish no partial result.
They execute no producer fluent method, control, getter, callback or sample.

`NewNyxDocumentationTypeConstruction` supplies one caller-owned byte/token/node/
depth/work policy across the whole capture. Parser work is admitted before
operations, and unknown analysis phases refuse before outward charging.
Captured owner receipts share exact immutable syntax only, while
each reference consumes work and independent equal copies consume storage
fully. Copied syntax, binding keys, parameter names/defaults and rich member
identity/prose consume cumulative remaining capacity, including an outer index
allowance. The existing 256 KiB signature/member ceiling also counts repeated
parameter occurrences when their immutable syntax owner is shared; sharing a
source window does not erase its declaration facts. Large binding maps and
default-heavy signatures refuse before later supplier rows. Binding capture
allocates one private vector and visits each slot with admission. Whole-type
capture appends its already captured members privately through the same scope;
the public `WithMember` method still captures alternate producers. No expression
silently resets the allowance. Retiring the construction releases its
policy/cancellation leases; published expressions
retain neither. Core capture retires its declaration child before exact
registration framing charges final fields once. Existing member, declaration,
bundle, contribution and index limits remain authoritative.

The new [rich documentation consumer](../tests/nyx_documentation_typeexpressions_tests.lpr)
requires one valid repository root before any native check:

```text
nyx_documentation_typeexpressions_tests.exe D:\Docs\GitHub\nyx
```

Its mandatory source witness reads all seven public `Choices` overloads from
`nyx.contract` plus `NyxObject`/`NyxArray` from `nyx.data`. It copies their exact
parameter type windows through existing syntax owners and retains all unbound
leaves. Browser compilation excludes only filesystem inspection. The native
consumer passes **99 checks**, inspecting all nine actual windows, with all
553,607 allocated blocks freed. The portable consumer compiles successfully.
These focused receipts are under
`E:\Blekohlab\nyx\qualification\documentation-typeexpressions-20261020-145122`:

- `logs/nyx_documentation_typeexpressions_tests-native-focused-phase-validation-compile.log`
- `logs/nyx_documentation_typeexpressions_tests-native-focused-phase-validation-run.log`
- `logs/nyx_documentation_typeexpressions_tests-browser-focused-phase-validation-compile.log`
- `logs/expression-argument-missing-run.log`
- `logs/expression-argument-invalid-run.log`

The same final native binary refuses missing and invalid repository arguments
at check count zero, exits with status `1` and frees all 143/170 allocated blocks
respectively. The final common seven-Pascal-source freeze passes **35 compiler
jobs**, 16 native test programs with clean heaps and 16 portable compilations, including both
Studios, backend and worker as applicable. It has zero owned warnings; 112
upstream warnings remain separately counted. The generator consumer passes 81
checks across 17 joined actual invocations, with clean parent/child heaps and
two exact five-file candidates. The property consumer retains 43 passing checks,
and the prior metadata consumers remain green. `source-phase-validation-freeze-private.json` and
`final-audit-private.json` own this final common source; the earlier reviewed 35
receipts remain retained as preceding evidence. [WORK](../WORK.md) owns aggregate
and remote receipts. The qualified simple-property and earlier descriptor
receipts remain separate. Complete generated/core metadata adoption and executed
decorated-browser evidence remain open.

## Authored member modifiers

The member-intent attachment is qualified by native execution and portable
compilation; complete source discovery and browser execution remain separate.
`INyxDocumentationMemberDeclaration.DeclarationIntent` returns a managed
`INyxDocumentationMemberIntent`, or nil for unknown. Its `DeclaredClass` and
`DeclaredStatic` flags preserve the authored modifiers independently. Belonging
to a class or having constructor/factory kind does not imply an authored `class`
keyword. `Visibility` and `CallingConvention` use the existing checked syntax
enums. Unspecified visibility and an absent convention remain unspecified; this
contract supplies no effective accessibility, compiler default or ABI.
The snippet uses `nyx.documentation`, `nyx.documentation.typeexpressions` and
`nyx.pascal.syntax` for the existing visibility/convention enum names.

```pascal
function DescribeParser(const Identity: TNyxDocumentationRef;
  const Construction: INyxDocumentationTypeConstruction): INyxDocumentationMember;
begin
  Result := WithNyxDocumentationMemberIntent(
    NewNyxDocumentationMember(Identity, ndmMethod,
      'Parse an immutable data value.'),
    NewNyxDocumentationMemberIntent(True, True, psavPublic, prcDefault,
      Construction), Construction);
end;
```

The caller supplies the actual qualified member identity. This attaches only
the stated declaration facts; it does not infer a class, factory, signature or
source origin. Passing nil to `WithNyxDocumentationMemberIntent` clears intent
back to unknown. A global unit member cannot invent class/static modifiers, an
enum value cannot acquire them, and a non-callable member cannot claim a routine
calling convention. Compiler acceptance of other authored combinations remains
separate from storing their declared syntax.

The existing virtual member copy path preserves intent alongside simple
signatures, rich signatures and both property shapes. Explicit signature/value
replacement keeps independent modifiers; example/related copies retain all
specializations. The private captured-owner check prevents an alternate supplier
from borrowing another member's authority. Alternate capture admits fixed scalar
work and logical copied bytes through the same sealed construction, reobserves
capability/intent presence including nil changes, scalar values and the legacy
envelope, and checks cancellation before publication. No source, producer,
construction, cancellation or admission is retained.

Only present intent adds the `member-declaration-intent` framed fields. Nil keeps
legacy bytes and existing legacy capture-work counters unchanged. Existing
per-member, registration and aggregate bounds still apply; intent creates no new
reference edges or inheritance rules. Changed facts therefore participate in
ordinary conflict refusal and prior-index/version preservation. Property/event
schemas and observed target capabilities retain their separate authorities.

The new [member-intent consumer](../tests/nyx_documentation_memberdeclarations_tests.lpr)
requires exactly one valid repository root before native check zero:

```text
nyx_documentation_memberdeclarations_tests.exe D:\Docs\GitHub\nyx
```

Its mandatory source witness compares actual `TNyxDataValue.ParseJSON` and
`TNyxNode.CreateRealized` with instance `ToJSON`/`Named` and all three ordinary
node constructors, through the existing syntax owner. Forward declarations are
followed to their full original owner. These seven observations attach only
declared modifier/visibility/convention facts, leaving signatures unknown.
Portable cases separately cover composition, unknown/clear framing, alternate
mutation and late cancellation, shared allowances, retirement and atomic index
conflicts. Native execution passes 122 checks over all seven actual members, with
zero unfreed blocks; the portable consumer compiles. Its final binary also refuses
missing/invalid source arguments before check zero with clean heaps. No inspection
executes a documented class, callback, factory or example. Automatic complete
source adoption and executed decorated-browser evidence remain open.

The current composite packet is
`E:\Blekohlab\nyx\qualification\documentation-memberintent-20261020-161704`:
41 accepted compiler jobs, 19 clean native test programs and 19 portable compiles;
zero owned warnings and 133 upstream warnings. Four result initializations in raise-only test
stubs are the sole change after the reviewed matrix; that consumer was rebuilt
on both targets, while 39 unchanged dependency jobs retain their original source
receipts. [WORK.md](../WORK.md) records both freezes, the retained initial warnings,
same-binary argument guards and remaining acceptance scope.

## Declared implemented interfaces

This attachment is qualified by native execution and portable compilation.
`INyxDocumentationDeclaredInterfaces.ImplementedInterfaces` returns an immutable
`INyxDocumentationInterfaceImplementations` vector of existing typed uses.
Nil means unknown; a non-nil vector with `Count = 0` explicitly states none.
Passing nil to `WithNyxDocumentationImplementedInterfaces` clears to unknown.

```pascal
function DescribeInterfaces(const ClassDeclaration: INyxDocumentationType;
  const PublicInterface: TNyxDocumentationRef;
  const Construction: INyxDocumentationTypeConstruction): INyxDocumentationType;
begin
  Result := WithNyxDocumentationImplementedInterfaces(ClassDeclaration,
    NewNyxDocumentationInterfaceImplementations([
      NewNyxDocumentationNamedTypeUse(PublicInterface)], Construction),
    Construction);
end;
```

The caller supplies those actual declared identities. Ordinary named uses must
be nongeneric. A generic application retains its complete existing standalone
type syntax with `ndtoImplementedInterface`, ordered explicit bindings and all
unbound leaves. Only name/qualified/generic roots are valid; arrays, pointers,
procedural types and a generic declaration without arguments cannot substitute
for an interface application. No new parser, global formal-parameter identity,
effective generic specialization or compiler conversion is inferred.

The root binding is selected through captured `NamedUseNode` topology, including
a generic root's `ptrBase`; binding zero may instead describe an enclosing
generic name. Complete index closure resolves the root and its actual alias
branches to an interface. Explicit argument bindings follow ordinary type
closure, so a record, class or scalar argument is not required to be an
interface. Missing/unbound links, wrong root kinds, alias cycles and known
contradictions in declared availability refuse before publication. Unspecified
platform metadata stays unspecified and proves no adapter/compiler capability.

Class/callback-class owners keep this vector separate from `BaseAt`. Inherited
member queries still follow class bases and aliases only. Declaring an interface
does not synthesize its members on the class, claim a method-name mapping or
describe property delegation. The latter already occurs in real browser code
and remains a distinct source-adoption fact. Interface inheritance continues
to use the ordinary interface base contract.

Capture borrows alternate vectors synchronously under the same sealed
construction. It bounds the vector at the existing 32-link ceiling, admits
copied fields/work before retention, and preserves the existing 256 KiB type,
bundle and aggregate index ceilings. Canonical expression reuse follows the
existing private receipt owner; copied binding vectors still consume their
actual admission. Final capability/presence/item identity, typed-use fields,
bindings and type envelope are reobserved, with cancellation checks between
borrowed getters and before publication. Immutable copies retain the shape;
results retain no source, producer, admission or cancellation lease.

Only present facts add `implemented-interfaces` framing. Known empty differs
from unknown, while clearing restores the earlier absent bytes. Ordinary
duplicate/conflict refusal preserves the previous `Current` and version.

The new [relationship consumer](../tests/nyx_documentation_implementations_tests.lpr)
requires exactly one valid native repository argument before check zero:

```text
nyx_documentation_implementations_tests.exe D:\Docs\GitHub\nyx
```

Required source witnesses inspect the real badge/include owner, both extension
interfaces, and `INyxQueryResultPort<TNyxQueryOutcome>` with its actual interface
and concrete record declarations. They capture selected relationship vectors;
they neither register incomplete core classes/units nor invent generic formals.
Portable cases separately cover composition/clear, a root outside binding zero,
mutation/cancellation, bounds/retirement, closure/availability and atomic index
refusal. The native consumer passes 109 checks over all three required source
sets with a clean heap. Its same binary refuses missing and invalid repository
arguments before check zero, also with clean heaps. The common retained-source
matrix passes 45 compiler jobs, 21 native test programs and 21 portable compiles,
with zero owned warnings and 147 upstream RTL warnings. Browser execution is
not part of this evidence; [WORK.md](../WORK.md) owns the exact receipts and
retained preparations. Complete whole-unit/core/creator adoption,
source binding and executed decorated-browser evidence remain open.

## Scoped generic parameters

`INyxDocumentationGenericDeclaration.GenericParameters` is an optional,
immutable declaration vector. Nil means unknown. A present empty vector
declares no own parameters; passing nil to
`WithNyxDocumentationGenericParameters` clears that fact. The overloads accept
the existing type and member contracts and preserve their specialized shapes.
The snippet below uses `nyx.documentation` and
`nyx.documentation.typeexpressions`; the caller supplies an actual declaration
whose checked identity has generic arity one.

```pascal
function DeclareParameter(const Declaration: INyxDocumentationType;
  const Construction: INyxDocumentationTypeConstruction): INyxDocumentationType;
var
  Names: TNyxDocumentationGenericNames;
  Parameters: INyxDocumentationGenericParameters;
begin
  SetLength(Names, 1);
  Names[0] := 'T';
  Parameters := NewNyxDocumentationGenericParameters(
    Declaration.Identity, Names, Construction);
  Result := WithNyxDocumentationGenericParameters(
    Declaration, Parameters, Construction);
end;
```

Each `INyxDocumentationFormalParameter` retains its exact `Owner`, zero-based
`ParameterIndex` and authored `Name`. A type's vector count must match its own
`GenericArity`. A routine member's vector describes that routine's own
parameters, independently of the containing type's arity. A unit cannot own a
formal directly: a global generic routine uses its exact unit-member and
overload identity. Same-spelled names in different declarations remain distinct.
Names use the existing ASCII case comparison policy for lexical closure.

An expression use can bind an ordered occurrence through
`WithNyxDocumentationFormalBinding(Use, Slot, Parameter, Construction)`.
`INyxDocumentationFormalBindings.FormalAt` exposes the captured slot. Only a
bare name of arity zero matching the formal is accepted; qualified names,
generic constructor positions and member designators cannot be reinterpreted
as formals. The unchanged nominal `BindingAt` is undefined at a formal slot.
Calling the old `WithBinding` replaces it with a nominal binding and clears the
formal. Passing nil to the new attachment clears the slot to unbound. Clearing
the last formal restores the original absent-map path, while prior published
values remain readable.

Complete index closure requires the exact captured declaring owner and its own
ordinal/name. A member may use its own formals or a containing type's formals.
The latter requires a known member-own vector: unknown does not prove absence
of shadowing. A same-spelled member-own formal shadows the outer one, so a bare
occurrence cannot claim that hidden outer owner. No dotted-name ancestry,
compiler specialization or global formal identity is inferred. Implemented
interface roots remain nominal interface/alias references; their ordinary
argument occurrences may use properly scoped formals. These facts add no class
base edges or inherited members.

The vector is bounded at 32 own parameters, with the existing name/reference,
256 KiB declaration and aggregate registration/index ceilings. Constructors and
alternate-producer capture use the caller's same sealed construction for copied
fields, work, comparisons and cancellation. Count admission precedes row reads;
every borrowed getter is separated by cancellation checks. Final capability,
presence, item identity, scalar and envelope agreement precedes publication.
Managed input names and construction interfaces have local lifetime leases,
and the caller's name vector is checked again after the last outward admission.
Published values retain neither suppliers nor source/editor/admission leases.
No constraint facts are supplied: an existing binding owner's implied
`BoundType = TObject` must not become an authored constraint.

Only present facts append `generic-parameters` or per-slot `formal-binding`
content. Absent/cleared metadata preserves the earlier framing and charged work;
known empty remains distinct. Duplicate conflicting facts, incomplete closure,
wrong scope/name/overload and shadowed or unknown ownership refuse atomically,
preserving the previous index `Current` and version.

The [scoped-formal consumer](../tests/nyx_documentation_formals_tests.lpr)
requires exactly one valid native repository argument before check zero:

```text
nyx_documentation_formals_tests.exe D:\Docs\GitHub\nyx
```

It must inspect all three selected actual source sets: `TNyxKeyValue` and
`INyxKeyValue` in `nyx.linq.aggregates`, plus the distinct
`TSignatureBox<T>.Replace` and global `Identity<T>` owners in the existing
signature fixture. Portable cases cover specialization, nil/empty/clear,
nominal replacement, alternate mutation and lies, final cancellation, shared
limits and getter counters, caller-slot retirement and atomic index conflicts.
The portable result is also rendered into `document.body.textContent` for a
normal browser bootstrap. Native qualification passes 140 checks over all three
actual source sets with a clean heap; the same binary refuses missing and
invalid repository arguments before check zero, also with clean heaps. The
portable consumer compiles. The browser URL policy rejected the selected local
file loader before execution, so its display supplies no executed evidence.
The selected source witnesses neither publish partial real units/classes nor establish
complete source-derived discovery, constraints, target binding or adoption.

## Named compound type declarations

The appended `ndtExpression` kind preserves the original named identity of an array,
set, procedural or distinct declaration. Its optional
`INyxDocumentationDeclaredTypeExpression` exposes `Definition`, an immutable
`INyxDocumentationTypeUse`, and `Family` from the existing `TNyxPascalTypeKind`
owner. It retains exact declaration-position syntax, ordered explicit name
bindings, procedural qualifiers and inert bounds/defaults. It supplies no layout,
effective calling convention, compiler acceptance or runtime capability facts.

```pascal
function DeclareValues(const Identity, ElementType: TNyxDocumentationRef;
  const Construction: INyxDocumentationTypeConstruction): INyxDocumentationType;
var
  Definition: INyxDocumentationTypeUse;
begin
  Definition := NewNyxDocumentationExpressionTypeUse(
    NewNyxDocumentationTypeExpression('array of TElement', ndtoDeclaration,
      Construction), Construction).WithBinding(0, ElementType);
  Result := NewNyxDocumentationDeclaredType(Identity,
    'An owned vector of elements.', Definition, Construction);
end;
```

The caller supplies both real qualified identities; the constructor guesses no
binding from `TElement`. This is a descriptor example, not a generated core
inventory. The closed root families are `ptxArray`, `ptxSet`, `ptxProcedure`,
`ptxFunction` and `ptxDistinct`. Plain named/qualified/generic aliases, missing or
wrong-position expressions, unsupported families and bodies needing their
original declaration context refuse. `NewNyxDocumentationType` refuses the new
kind without its explicit definition factory. `AliasTarget` remains undefined;
base/member/alias additions refuse rather than invent an inheritance relationship
or synthetic `Invoke`. Target-specific named aliases such as `TNyxText` retain
their separate source/binding requirements and targetless-alias refusal.

Ordinary prose/example/related copies retain specialization. Capture shares the
existing construction allowance, freezes alternate binding maps, checks advertised
family against the owned syntax and rechecks the capability and legacy envelope
before publication. Published values retain no supplier, construction, source
unit, cancellation or admission lease. The new framed fields appear only when
the capability is present; old absent-shape bytes and public slots/layouts remain
unchanged. Complete index publication requires every explicit name occurrence to
resolve, with current limits and prior-index/version preservation on refusal.

The new [declared-expression consumer](../tests/nyx_documentation_declarationexpressions_tests.lpr)
requires exactly one valid repository root before any native checks:

```text
nyx_documentation_declarationexpressions_tests.exe D:\Docs\GitHub\nyx
```

Its mandatory physical-source witness covers ten actual definitions:
`TNyxExecutions`, `TNyxSignalSnapshot`, `TNyxRegistrationID`, `TNyxEventHandler`,
`TNyxStateValidator`, `TNyxStateObserver`, `TNyxStateKinds`,
`TNyxBindingTargetInfos`, `TNyxKeyModifiers` and `TNyxPointerButtons`. Exact windows
use the existing shared construction text-copy owner; all leaf names remain
unbound, and `of object` remains authored syntax. Portable mechanics separately
cover immutable copies/captures, shared work/storage, alternate getter bounds,
late mutation/cancellation, retirement and atomic closure/conflict refusal.
The consumer passes **126 native checks with a clean heap**, including all ten
actual definitions, and **compiles for pas2js**. Current focused receipts are in
`E:\Blekohlab\nyx\qualification\documentation-declarations-20261020-194836`:
`logs/nyx_documentation_declarationexpressions_tests-native-focused-supplier-compile.log`,
`logs/nyx_documentation_declarationexpressions_tests-native-focused-supplier-run.log`
and `logs/nyx_documentation_declarationexpressions_tests-browser-focused-supplier-compile.log`.
The native run allocated and freed all 703325 blocks (27784423 bytes). The same
final binary refuses missing and invalid repository arguments at check zero with
clean heaps; its receipts are `logs/nyx_documentation_declarationexpressions_tests-argument-missing-run.log`
and `logs/nyx_documentation_declarationexpressions_tests-argument-invalid-run.log`.

The final common seven-source freeze is `source-supplier-freeze-private.json`;
`all-source-final-private.json` and `final-audit-private.json` retain final source
and audit identities. The full batch passes **39 compiler jobs**, **18 native test
programs with clean heaps** and **18 pas2js compilation checks**, with zero owned
warnings and 126 upstream warnings. The unchanged generator consumer passes 81
checks and 17 joined generator invocations with clean parent/child heaps; both
five-file candidates remain exact. WORK.md owns whole-batch preservation and
remote receipts. Earlier reviewed/admission preparations, their failed assertions
and clean heaps remain retained separately. The bounded reusable signature
extractor is documented in [Source-owned API signatures](source-signatures.md).
Full source binding, complete generated/core declaration adoption and executed
browser evidence remain open; these source windows and compilation checks do
not establish that broader acceptance.

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

## Own source declarations once

The [declaration contribution API](../src/nyx.documentation.declarations.pas)
separates copied source facts from the creator bindings that reference them.
`INyxDocumentationDeclarationChunk` owns frozen declarations from one actual
unit. A chunk is a storage boundary; it does not split a unit's factory-member
descriptor into conflicting partial unit declarations. Several chunks may own
different complete declarations from the same unit. A flat
`INyxDocumentationPackage` retains these exact chunk owners, and different
packages may share a core chunk without another declaration copy.

Create the chunks and package from the same declaration-producing source:

```pascal
SetLength(Chunks, 1);
Chunks[0] := NewNyxDocumentationDeclarationChunk(UnitReference,
  Declarations, [ndplatformNative, ndplatformBrowser]);
PackageOwner := NewNyxDocumentationPackage(Chunks,
  TNyxDocumentationContributionLimits.Defaults);
Binding := NewNyxDocumentationBinding(PackageOwner, FactoryReference,
  [ndplatformNative, ndplatformBrowser]);
Catalog.RegisterKind(Kind, Title, Category, False, Binding);
```

The existing schema, callback and catalog documentation arguments accept this
binding. No separate help-registration call is added. The optional
`INyxDocumentationContributionBinding` exposes its package owner; existing
registration getters expose the complete declaration that owns the primary.
A factory primary therefore still exposes the whole unit descriptor, retaining
all its factories and signatures. `ForPrimary` keeps the same package and
original unit. A type primary is its declared public type, with no guessed
concrete implementation class.

Canonical immutable chunks/packages/bindings use a verified private owner and
exact canonical interface identity. Capturing those owners retains them;
alternate producers are frozen and their claimed identities, content, counts
and byte receipts do not replace the admitted facts. Captures reject changed
source envelopes/bindings, contradictory primary views and cancellation before
publication, retaining no mutable supplier. References point from snapshots and
bindings to packages, then chunks, then inert declarations; they form no import
ownership cycle. There is no global help inventory, module-unload contract or
retained control, callback, factory, renderer or source document.

On the contribution path, the index charges each exact captured chunk's copied
declarations and content once, each distinct package/chunk reference once and
every creator binding/package reference separately. Two independently captured
equivalent chunks charge in full before declaration deduplication. Old bundles
without this optional capability retain their existing repeated occurrence
charges, framing and refusal rules. Conflicting declarations/availability,
missing links, bad inheritance/cycles, exhausted budgets or stale publication
still leave the previous index/version exact. The optional
`INyxDocumentationContributionGeneration` retains package/chunk identities;
the existing three-authority generation record stays unchanged. These new
identities are local to the process, with no source-hash or cross-process claim.

`TNyxDocumentationContributionLimits` is an explicit caller-owned package
allowance. Its `Defaults` uses the existing 4096 declaration, 16 MiB and
two-million work ceilings; zeroed/oversized limits refuse. This is no evidence
that the intended complete generated inventory fits. Each chunk still admits
at most 128 declarations/4 MiB and each declaration retains its existing bounds.
Package accounting includes independently captured equal chunks and its
reference fields. Bounded declaration/chunk copying and package/index admission
are layered limits, not an assertion that text bytes measure object overhead or
that a framing work step equals all internal declaration-copy work. Cancellation
scopes and admission suppliers remain capture-local.

The optional `INyxDocumentationContributionAdmission` carries the index's same
remaining declaration/byte/work allowance through an alternate registration,
its package and its chunks. A chunk reserves its bounded declaration count
before any declaration getter, then reads and freezes one declaration at a
time; each framed field is admitted before append. Package topology, duplicate
comparisons and primary-owner/member searches also consume that shared work
allowance. A fresh package allowance cannot reset a partly consumed index
allowance. Query-local receipts retain exact verified canonical owners, so a
chunk's copied count/content and each package/chunk reference are charged once;
independently captured equivalents remain separately charged. Canonical chunk
admission replays every privately captured field's work. An alternate binding's
separate primary-view validation copy consumes additional actual field work and
bytes; it is never mistaken for another retained source declaration. The old
field-admission GUID/slots and all legacy bundle accounting stay unchanged.

The new [contribution consumer](../tests/nyx_documentation_contributions_tests.lpr)
uses an explicitly synthetic whole 81-factory unit with its 81 distinct interface
results to exercise ownership and the 6642-occurrence failure. This is mechanics
evidence, not a replacement for actual generated control/core metadata. It also
retains the real extension's unresolved-core refusal without constructing its
controls or invoking callbacks/factories/routines. It passes **70 native checks
with a clean heap and no owned warnings**, and its browser consumer compiles.
The alternate catalog case retains every real built-in/recipe row, placing the
two controlled rows first through one complete ordinal permutation. Smaller
remaining declaration/byte/work limits refuse before later source or duplicate
primary getters; adequate limits retain complete framing and the original
undocumented rows. No declaration getter or range guard is weakened.

These focused results use
`E:\Blekohlab\nyx\qualification\documentation-contributions-20261020-135130`,
with `source-final-freeze-private.json` and logs:

- `nyx_documentation_contributions_tests-native-focused-final-compile.log`
- `nyx_documentation_contributions_tests-native-focused-final-run.log`
- `nyx_documentation_contributions_tests-browser-focused-final-compile.log`

The browser log retains seven existing upstream `Classes` warnings. The same
focused qualification retains native shapes66, registry64, documentation68,
syntax159 and indexed-property413 checks with clean heaps; all six portable
consumers compile. A separate run of the syntax binary inspects five actual
inputs/4060 declarations and passes8355 checks. The real generator/CLI passes71
checks with16 joined invocations and two exact five-file candidates. These are
focused terminal results; [WORK](../WORK.md) owns the broader qualification and
its final audit. Browser compilation is not browser execution.

Initial managed-result warning, the undersized alternate-catalog attachment
fixture and diagnostic trace, and the browser empty-finalization preparation
remain retained with their exact sources/logs. The repaired fixture preserves
the entire backing snapshot. Actual emitted documentation/core closure, real
fixture adoption and executed decorated-browser evidence remain open.

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
ownership model. Canonical immutable source chunks now own copied declarations,
flat packages retain those chunks and ordinary creator bindings reference their
package and exact primary. Exact captured
chunk ownership, package/chunk/binding reference costs and legacy bundle charges
remain separate; equivalent independent copies still charge fully. The mechanics
API does not establish adoption. Actual adoption still requires a complete
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
  index parameters, declared access and separate default-property status.
  Omitting the parameter or relabelling the property as a method would conceal
  the API. The optional descriptor extension passes43 native checks, including
  actual Children syntax, and its browser consumer compiles; actual
  source-derived metadata adoption remains open.
- Open-array parameters already occur in `nyx.contract` and `nyx.data`.
  Detached type-expression metadata must preserve their shape and original
  named element owners; substituting an element type or invented named alias
  would erase the declaration. The richer optional attachment passes 99 native
  checks over all nine required actual windows and compiles for pas2js; actual
  source-derived binding/adoption remains open.
- Named array/set/procedural/distinct definitions now retain declaration-role
  expressions through the optional type attachment, with 126 native checks over
  all ten required actual definitions and pas2js compilation. Their detached
  names remain unbound until the original source/binding owner supplies them.
- Authored class/static modifiers, visibility and convention facts now have a
  managed attachment qualified by 122 native checks over seven actual members and
  portable compilation. Complete source extraction still
  needs their original owner/context. The separate implemented-interface vector
  is qualified by 109 native checks over three actual source sets and portable
  compilation; it preserves generic applications
  without treating them as class bases or inherited class members. Descriptor
  mechanics and signature-only extraction establish none of those full-source
  facts automatically.

Generator adoption must derive metadata at the existing declaration emission
points, including hand-authored dependencies and deterministic per-creator/unit
chunks within current allowances. The generator's existing candidate/check
workflow and typed declaration admission are documented in
[managed controls](managed-controls.md). The generator was run to verify actual
contract declarations and exact candidates; it does not yet emit documentation.
Adoption must produce and review scratch candidates before replacing source
after all readers join. No parallel manually authored core-help inventory is
introduced.

Executed decorated-browser evidence, a complete supported-version capability
matrix, independent API coverage and the full original Studio/library acceptance
remain required. No reference task, criterion, completion credit or DONE status
closes from this contract or its assertion count.
