# Design and runtime identity

[Architecture](architecture.md) · [Components](components.md) ·
[Builds](building.md) · [Public model](../src/nyx.model.pas)

Nyx preserves authored IDs through version 1 persistence, cloning, generated
Pascal and isolated view builds. Realization creates separate runtime keys for
reusable instances; it does not rename the design or its component definitions.

## Authored admission

An explicit ID contains **1–128 Unicode scalar values**, with at least one value
outside Unicode `White_Space`. This counts Unicode characters rather than native
UTF-8 bytes or browser UTF-16 units. For example, 128 `🌙` characters are valid
on both targets; 129 are rejected. Combining marks each count as a scalar;
the limit is not a grapheme-cluster limit.

The admission boundary rejects malformed UTF-8, unpaired UTF-16 surrogates,
C0/C1 controls including DEL, and U+2028/U+2029 line/paragraph separators.
Whitespace-only IDs are invalid; meaningful leading/trailing spaces are retained.
Comparison is exact and case-sensitive. There is no trimming, case folding or
Unicode normalization: `é` and `e` followed by U+0301 remain distinct IDs.
Slash and tilde are valid authored characters.

`TNyxNode.Create` without an explicit ID creates a convenience `node-N` identity.
Saved designs and generated source contain explicit IDs. `Named` checks a
candidate before assignment; an invalid rename preserves the previous ID.
`TNyxDocument.Validate` separately checks global uniqueness, reference/cycle
admission and the document's 128-depth / 10,000-node budgets. A caller editing
several nodes directly should validate the candidate before publishing it.

`NyxNextScalar` in `nyx.text` reads the target's native storage without a global
codepage change. ID diagnostics distinguish malformed encoding, controls,
whitespace-only content and the scalar limit. Invalid Studio imports retain the
accepted document, active view and selection.

## Realized identities

Each realized node exposes three read-only identities:

| Property | Meaning |
| --- | --- |
| `ID` | Unique runtime key in the realized view |
| `SourceID` | Original authored ID of this template or payload node |
| `DesignID` | Editable owner: the selecting instance for inherited parts, or the payload node for inserted content |

On an authored node, all three return its authored ID. `IsRealized` distinguishes
the roles. A realized tree remains independently owned; cloning retains its
identity role and keys. `Named` refuses realized nodes, and design admission
refuses a realized root or a mixed authored/realized child without consuming it.

`NyxQualifiedID(Prefix, SourceID)` escapes each source segment as `~` → `~0`
and `/` → `~1`. It appends the escaped segment to a nonempty prefix with `/`.
Realization applies this to every node, including ordinary roots, and adds scopes
for each reusable reference. Ordinary container ancestry does not add scopes;
the source document already guarantees globally unique authored IDs.

| Authored structure | Runtime key |
| --- | --- |
| Owned node `literal/instance` | `literal~1instance` |
| Reference `literal` using definition root `instance` | `literal/instance` |
| Owned node `a~1b` | `a~01b` |

Escaping is injective, including nested references and instance payloads. IDs
without slash/tilde retain their existing runtime spelling. Qualification has its
own bounded encoding/expansion budget; a qualified path may exceed 128 scalars.
`CreateRealized` is the composition boundary for adapters: provide canonical keys
produced by `NyxQualifiedID`, with admitted source/owner IDs. The factory validates
encoding/size; arbitrary handcrafted path syntax is not a source-design format.

`RealizeNyxView` borrows a root that belongs to its source document and returns
an independently owned runtime tree. Detached or already realized roots are
rejected. Renderers retain an accepted projection when candidate admission fails.
The old `design-id` property alias remains available, but selection/event routing
uses the read-only `DesignID` contract.

## Renderer lookup and events

Browser `ElementFor`, native `ControlFor` and native `InputFor` accept an optional
`TNyxIdentityKind`: `niAutomatic`, `niRuntime` or `niDesign`. Automatic lookup
performs a complete exact-runtime pass before searching editable owners. Explicit
modes disambiguate a literal authored slash name from another node's runtime key.
Design lookup borrows the first projected control for that editable owner;
runtime lookup identifies one specific part. Returned target handles remain
borrowed and expire with the renderer's projection.

Target events carry a borrowed realized node. Use `SourceID` to identify the
template part and `DesignID` to address its editable instance/payload. Studio
canvas selection uses `DesignID`. Generated previews and applications exercise
the same contract as hand-built Nyx views.

## Isolated builds

`CloneNyxViewDocument(Document, Root)` returns an owned authored document with
one selected page/subtree/definition and only its transitive reusable dependencies.
It preserves IDs and independent payload ownership. A definition-root preview
does not include a duplicate copy of that definition. Source designs remain
unchanged on success or failure; compiler paths and runtime keys never enter them.

Studio's HTTP service uses that public API for view builds and clones the complete
document for application builds. UTF-8 percent-encoded query values preserve
maximum-length supplementary IDs, literal slash/tilde and nested view selection.
Request bodies and decoded query bytes are relabeled UTF-8 before entering Nyx.

## Executed evidence

The shared identity fixture has 36 checks, executed under FPC and pas2js: ASCII,
CJK and supplementary scalar boundaries, malformed/escaped-surrogate rejection,
exact round trips, bounded Studio chrome keys carrying original IDs as command
data, independent isolated scopes, runtime escaping, long nested
references/payloads, immutable runtime identity and failed Studio import recovery.
Generated Pascal reconstructs and realizes the same fixture. Browser/native
journeys exercise explicit lookup, inherited/payload selection, real actions and
accepted-view recovery. The HTTP harness builds long Unicode page, definition and
instance scopes on both targets, and rejects overflowing IDs before compilation.
Broader indexing, state bindings, codec budgets and source synchronization remain
separate tasks; this contract does not establish large-document performance.
