# Architecture

[Project](../PROJECT.md) · [Milestones](../MILESTONES.md) ·
[Task catalog](../TODO/README.md)

## Contract and dependency direction

The [public split-view](split-views.md) owns portable proportional geometry;
browser and LCL adapters retain mounted child identity. Typed `.ForPlatform`
configuration is applied to independently realized views, preserving authored
documents. [Interaction snapshots and phases](events.md#interaction-families-and-timing)
share one typed registry across schema, persistence, source generation and Studio.

```text
Nyx application / generated Pascal
              |
        fluent document API
              |
  document tree + catalog + theme + state
       /                         \
browser adapter              LCL adapter
 DOM/CSS/events             controls/events

Nyx Studio -> design commands -> same document tree -> Pascal generator
     |                                                |
     +-------- HTTP build request --------------------+
                       |
              Pascal compiler service
                 /             \
             view build    application build
```

The arrows are dependency direction. Portable units do not import DOM, browser,
Forms, Controls, or widgetset units. Renderer adapters may depend on portable
units. Studio edits the same model consumed by applications and generators.

## Portable model

`nyx.model.pas` implements `TNyxDocument` and `TNyxNode`; page roots and reusable
definitions/instances use that node contract. The document owns pages and definitions;
each node owns its child nodes. Parents are non-owning references or stable IDs,
and renderer handles live outside the model. This prevents ownership cycles and
makes save/load, clone, undo, diff, generation, and target rendering operate on
one graph.

Nodes currently carry stable identity, an extensible kind key, ordered string
properties, immutable typed binding descriptors, structured extension data and
owned children. Properties preserve extension data; named compound parts and
shared action/event recipes are
implemented. Scalar live bindings are integrated. `nyx.collections` now provides
[typed ordered stores](collections.md), immutable owned snapshots, atomic logs,
document/runtime registries and versioned wire/source/history integration. Actual
collection-bound controls remain open. Fluent methods return the concrete
builder/node contract and preserve deterministic child/property order. Mutation
is observable through commands or change records rather than renderer callbacks
hidden inside model setters.

`nyx.data.pas` supplies owner-scoped extension stores, distinct references and
immutable nested values. Unknown document/node wire fields retain exact Unicode,
numeric spelling and ordering. Stores protect standard fields and admit complete
candidates. Reusable instances/part rules overlay whole values independently;
isolated view builds retain project data. The codec validates complete exported
designs against import budgets. Generated Pascal uses the same typed constructors.
See [structured extensions](extensions.md).

`nyx.catalog.pas` currently owns palette descriptors, defaults and compound
templates. `nyx.recipes.pas` defines reusable compositions; `Part` resolves named
slots and `nyx.behavior.pas` applies portable actions. `nyx.schema.pas` supplies
typed primitive defaults, projection capabilities and property admission.
Recipe `Kind` and persisted base `ProjectionKind` are distinct, so successive
primitive derivation preserves target semantics without a runtime registry.
Exact factories override base factories. Bound factories supply updaters that
retain their target/child identity. Consumer-defined property/event schemas
and a complete property-level capability matrix remain planned. Metadata drives validation,
Studio property panels, code generation, and renderer admission. It must allow a
consumer to register a custom component and renderer without editing a central
case statement.

Reusable references own explicit part-override descriptors. Independent realization
applies property edits, inserted/replaced/removed parts and nested named paths;
ordinary instance children cannot be silently discarded. Studio admits effective
paths, hosts and typed values before committing history. The same data generates
Pascal and reaches both renderers; extension payloads retain selectable design IDs.
The [identity contract](identity.md) separates original source IDs, editable owners
and escaped runtime keys. Authored Unicode scalar admission is portable; isolated
view documents retain exact IDs and only required reusable definitions.

Themes use semantic tokens and component recipes. Target adapters translate
tokens into CSS/DOM behavior or LCL colors, fonts, metrics, drawing and control
properties while retaining documented meaning.
The [theme contract](themes.md) documents palette validation, borrowed lifetime,
scoped browser styles, native themed widgets and the remaining widgetset limits.

## Renderers and parity

Each renderer owns an independent realized view and target bindings. Application
hosts own runtime stores that survive navigation and validate unmounted pages.
[Live bindings](bindings.md) stage commands before state publication and retain
controls/drafts during updates. Runtime compound actions synchronize existing
controls; Studio design changes currently
use a full projection replacement. Incremental structural changes remain planned.
The intended projection applies create/update/move/remove changes, connects target events
to shared commands, and disposes target handles independently from the document.
Full rerender remains a correctness fallback.

Parity is semantic: value, selection, focus order, validation, activation,
disabled/read-only behavior, keyboard operation, layout, accessibility intent,
and theme tokens have one contract. Exact pixels may differ with native platform
controls; intentional differences are capabilities recorded in the catalog and
tested in a parity matrix.

## Studio and code generation

Studio is a Nyx application. Its project explorer owns applications, pages,
reusable component definitions, themes, and assets. The canvas, hierarchy,
property/event inspector, catalog palette, responsive preview, history, and code
view issue model commands. Undo/redo stores complete immutable design/source
checkpoints. Each entry contains the canonical design once and the exact authored
companion frame; it retains no document, node or reader. Current public document
edits are freshly encoded and reconciled before checkpoint capture. Restoring a
pair decodes and validates the detached document before either owner is replaced.
The latest fifty undo commands have a 16 MiB retained-text budget (one oversized
previous command remains undoable). JSON is used for recovery/interchange rather
than encoding and parsing every in-memory history operation.

The [State and Bindings editors](studio-state.md) are public Nyx compositions.
Their shared `nyx.studio.commands` router decodes typed authoring metadata and
calls portable session commands; it borrows the shell and session during a callback.
Detached candidate admission precedes document/history publication. This router is
exercised by browser and actual LCL controls; the complete native Studio controller
is still required. Focused new-default drafts stay in presentation state until Add.

Generation is deterministic Pascal in Delphi dialect. A no-op design change
produces byte-identical output. Stable node identities allow the generator to
preserve declared extension regions without parsing arbitrary Pascal as design
state. The exact design-source format and safe custom-code boundary must be
proved by the persistence/code-generation task before becoming public API.

## Compiler service

The service is an FPC program using admitted HTTP facilities. Requests identify
an admitted project, target, build scope (`view` or `application`), revision, and
options from a server-side allowlist. The service produces structured phases,
diagnostics, artifact identities, and reload metadata.

Build roots, outputs, compiler executables, arguments, concurrency, timeouts,
and artifact retention are controlled by server configuration. Client input does
not become shell text. Content/revision identities support cancellation,
stale-result refusal, and cache reuse. View builds optimize feedback but never
replace complete application builds as release evidence.

## Compatibility evolution

Studio's [agent integration](studio-agents.md) adds a separately authenticated,
localhost MCP transport around the ordinary session commands and paired history.
Bounded semantic queries and detached typed patches share the model admission
rules used by the editor. A monotonic synchronization revision guards mutations;
connected browsers retain local files on a conflict. Permission controls and
the activity feed are public Nyx compositions, with operator credentials separate
from MCP credentials. Optional rendered previews use immutable admitted snapshots.

Public model, catalog, serialization, generator, and service protocol versions
evolve explicitly. Readers reject unsupported required features with diagnostics
and preserve admitted extension data. Migration behavior and the first stability
guarantee remain pending evidence and a documented release policy.
