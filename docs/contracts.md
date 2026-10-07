# Compound value and event contracts

[Fluent API](fluent-api.md) · [Events](events.md) · [Bindings](bindings.md) ·
[Components](components.md)

Compound controls expose declared scalar values and named fields through the
node-owned `Contract` facade from `nyx.contract`. A container projection alone
does not imply a text value or admit arbitrary Boolean/numeric bindings.
Declarations use immutable typed value objects:

```pascal
LRating.Contract.Value(NyxIntegerDomain.Range(1, 5));
LRating.Configure.Value(3).Done;

LSearch.Contract
  .NoValue
  .Field(FQueryPart, NyxTextDomain);

LSearch.Part(FSearchPart).Configure.OnClick(FSearchRequested).Done;
LSearch.Part(FSearchPart).Contract
  .On(ntClick, NyxPartValue(FQueryPart), NyxTextDomain);
```

`FQueryPart` and `FSearchPart` are `TNyxPartRef` values; `FSearchRequested`
is a `TNyxEventRef`. Initialize open names once with `NyxPart`/`NyxEvent`.
The search callback carries the query's text while its action target remains
the search button. `ValueID` identifies that payload field independently of
`SourceID`, `OriginID` and `TargetID`.

## Scalar specifications

| Construct | Admitted values |
| --- | --- |
| `NyxTextDomain` | Exact portable text |
| `NyxBooleanDomain` | Boolean |
| `NyxIntegerDomain` | Signed 32-bit integer |
| `NyxNumberDomain` | Finite Double |
| `NyxDateDomain` | Exact Gregorian calendar dates; [calendar field contract](date-fields.md) |
| `NyxTimeDomain` | Exact local clock readings; [clock field contract](time-fields.md) |
| `NyxNoDomain` | Explicit descriptor-level absence |

The four general scalar builders have typed `Choices([...])`. Numeric builders also
have typed `Range(minimum, maximum)`. These methods return independent narrowed
specifications, retaining their baseline. Choices contain 1..128 distinct
admitted values. Numeric membership compares numeric values: `1`, `1.0` and
`1e0` denote the same Number choice. Text comparison retains exact Unicode.
Ranges are inclusive and ordered; choices must also satisfy their range.
Selection presentation uses the same scalar comparison, preserving a numeric
option's selected state across canonicalization and imported wire spellings.

`Value` declares the component's self value. `NoValue` explicitly suppresses
a logical self value on a compound layout host. `Field` declares an existing
named part path, including nested paths. Bind that actual field through
`Part(FQueryPart).Binds.Value(...)`. This release does not provide virtual field
aliases or redirect a root binding to a descendant.

Primitive value families remain authoritative. A memo cannot be redeclared as
Boolean; a numeric input may be narrowed to Integer. Effective binding choices
use the declared family; Number also admits the integral subset, retaining
the binding descriptor's narrower kind. Values and field constraints are
checked before designer/runtime candidate publication. `Configure.Value`
and selection `Option` provide Boolean, Integer and Double overloads alongside
text; generated source selects the appropriate typed overload.
Studio uses the declared range for logical integer inspectors. Values exceeding
the spinner projection's bounds use a numeric draft input with an exact Integer
contract, preserving the entire signed 32-bit range.

## Payload declarations

`On(trigger, source, domain)` declares an optional scalar payload for
`ntClick` or `ntChange`. Source constructs are `NyxTargetValue`,
`NyxOriginValue`, `NyxSourceValue` and `NyxPartValue(part)`. A named part is
resolved against the semantic component, normally the nearest compound.
An origin declaration takes precedence over the semantic source's declaration.
`Signal(trigger)` explicitly carries no payload. A declared value that is absent
has `HasValue=False`; an explicitly empty text value has `HasValue=True`.

Without an event declaration, the action target's admitted control/value domain
supplies the scalar family. An undeclared compound value is never guessed as
text. Conversion and range/choice checks run before state publication. Selection
actions use the component's declared value domain; toggles require Boolean.
Numeric LCL edit drafts wait for editing completion whether bound or unbound.

## Ownership and persistence

A node owns its facade; do not free it or retain it after the node. Readers return
independent field/event/domain snapshots. `Assign(other.Contract)` copies the
complete declaration. Catalog instantiation copies root contracts, bindings and
extension data as well as descendants.

Declarations reside in the owned node extension `nyx.contract`, with inner
schema version 1 inside the existing design version-1 envelope. Other extension
fields retain their independent values/order. Reusable instance/part overlays
replace this entire namespace when it is present. For a partial customization,
assign the template contract before changing the desired field/event; an ordinary
instance without a declaration inherits the complete template.

`Metadata(TNyxDataValue)` is the explicit descriptor/import boundary. It validates
the complete candidate before publication. Studio-generated source normally emits
readable typed fluent declarations. Admitted imports with deliberate decimal
spelling, empty arrays or member order use an explicit structured metadata block
to retain exact reconstruction across targets. The read cache compares immutable
namespace snapshots, including edits/removal through the extension boundary.
Noncanonical admitted numeric control spelling also uses the explicit
`Configure.Metadata` boundary. Ordinary numbers remain typed literals; signed
integer limits use readable `High(Integer)`/`Low(Integer)` expressions.

Shared FPC/pas2js fixtures exercise admission, independent ownership, persistence,
history, source and reusable overlays. Actual DOM/LCL controls exercise search
payloads, rating selections and declared numeric choice rejection. Structured
collection domains, arbitrary extension property schemas, broader event triggers
and editable source synchronization remain separate product work.
