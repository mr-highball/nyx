# Nyx

Specialized authoring uses managed interfaces and factories such as
`INyxBadge` / `NewNyxBadge` and `INyxCommentThread.ReplyMemo`.
See [managed controls](docs/managed-controls.md) for typing, lifetime and compound
composition examples.

[Typed sliders](docs/sliders.md) retain fractional values and exact numeric
choices through shared scales, specialized interfaces and real target controls.

Managed [contextual views](docs/popover.md) present reusable Nyx content beside
an invoker with typed placement, focus and dismissal on browser and LCL. Studio's
component help uses the same public managed contract and specialized help card.

[Managed command menus](docs/menu.md) reuse specialized buttons and named parts,
with typed commands, check/radio choices and shared Unicode typeahead. Studio's
Actions menu consumes the same browser/LCL contract.

A Pascal-first, fluent UI library for Free Pascal/Lazarus and pas2js. One owned
UI document describes pages and reusable components; native and browser adapters
produce real target controls. Nyx Studio uses that same contract for visual
editing, adjacent Pascal generation and delegated HTTP compilation.
Studio's shell is a shared Pascal Nyx document, including reusable public
`code-editor` and `design-surface` components; the same shell is exercised by LCL.
Studio also exposes [semantic MCP tools](docs/studio-agents.md) for inspecting
and editing its active design. Agent access defaults enabled, with visible
activity and operator controls for read-only access or disabling agents.
The optional Pascal workspace uses the public [split-view contract](docs/split-views.md),
with touch/keyboard resizing and typed platform configuration. The shared
[event contract](docs/events.md#interaction-families-and-timing) includes key
phases, text proposals, pointer callbacks and typed wheel/viewport observations.
The [capability contract](docs/capabilities.md) describes each property's meaning,
target support and help through the same schema used by Studio and agent tools.

Work is on `hello-nyx`, starting from the original codebase and adopting
[Athena](athena/README.md). The first runnable foundation is implemented;
production breadth and complete Studio behavior are tracked in
[MILESTONES.md](MILESTONES.md). This is an active development branch.

```pascal
LDocument := TNyxDocument.Create;
LDocument.AddPage(
  TNyxNode.Create(nkPage, 'home').Configure.Layout(nlColumn).Gap(16).Done
    .Add(TNyxNode.Create(nkHeading, 'title').Configure.Text('My workspace').Done)
    .Add(TNyxNode.Create(nkButton, 'continue').Configure
      .Text('Continue').Variant(nvPrimary).OnClick(NyxEvent('continue')).Done)
);
```

The document owns admitted nodes. [Compound recipes](docs/components.md) provide
named parts and independent instances: customize a list's actions, derive a
search field, or compose a new component without patching platform internals.
The initial catalog contains 41 primitive/layout/authoring kinds and 35 compound recipes.
[Defaults and named parts](docs/components-reference.md) are generated from the
public schema. Reusable instances can customize parts and add independent content,
including through Studio's inspector and palette.
The [semantic theme contract](docs/themes.md) provides shared light/dark palettes,
surface and control radii, scoped browser views and themed native Lazarus controls.
The [identity contract](docs/identity.md) preserves Unicode design IDs while
giving nested reusable instances distinct runtime keys and editable owners.
The [typed fluent API](docs/fluent-api.md) uses enums, numeric/Boolean arguments
and node-owned configuration objects. Generated Pascal follows that same API,
with purposeful control names and readable configuration blocks. The
[typed state contract](docs/state.md) provides atomic observable values, saved
defaults and generated text/Boolean/integer/number declarations.
The [live binding contract](docs/bindings.md) reuses named typed references across
defaults and fluent control bindings. Browser/LCL application stores survive
page navigation; accepted edits update existing controls and preserve unrelated
field drafts. Studio's [State and Bindings editors](docs/studio-state.md) create
typed defaults and undoable control bindings through that same public contract.
[Observable collections](docs/collections.md) add typed ordered schemas, atomic
item edits and owned snapshots. [Collection views](docs/collection-views.md)
attach them to actual browser/LCL lists, tables and trees, with stable selection
and independent reusable-instance stores. Typed authored bindings mount
automatically and Studio's Data/Bindings panels edit their saved values and
specifications. Production virtualization remains open.
Document and node [structured extension data](docs/extensions.md) retain custom
objects, arrays, exact Unicode and decimal spelling through history and generated
builds. Their public constructors and reference types keep that data explicit.
The [typed event contract](docs/events.md) uses enum triggers, distinct event
references and owned value snapshots shared by browser and native controls.
Creator-defined events declare typed signal, scalar or structured payloads;
managed producer ports connect actual custom controls to independent callback
streams, Studio authoring and bounded agent queries.
Compound values, named fields, range/choice constraints and scalar payloads use
[typed fluent contracts](docs/contracts.md). Generated locals name their purpose
and control type, including named composition parts.

```powershell
./tools/build.ps1 -Target all
./tools/studio.ps1
```

See [tool configuration and builds](docs/building.md). Product code and substantive
tools are Pascal; no Node, npm, Python or external UI framework is required.
Generated JavaScript and matched pas2js runtime are browser target artifacts.

Studio currently supports a palette, selection, hierarchy, property editing,
multi-page and reusable-component authoring, undo/redo, optional editable-Pascal
split view, browser preview and browser/LCL compiler builds. Pascal title,
configuration, scalar-default, binding, domain and extension edits use typed
admission and paired history, retain rejected drafts, and preserve handwritten helpers around the
synchronized builder. See the
[supported source workflow](docs/fluent-api.md#crafted-source-and-editable-configuration).
**Open** exposes paired project files: save/reopen through the Pascal backend,
download a portable backup including drafts, or import a `.nyx` and `.pas` together.
Conflicts retain both inputs and require an explicit choice. See
[project files and recovery](docs/studio-state.md#paired-projects-and-recovery).
Full drag/drop, broader code synchronization, advanced control behavior, native Studio and
production target parity remain open work rather than completed claims.

Output targets are optional. Open **Outputs** at any time to choose Browser or
Native LCL and configure its tools. A built Studio starts without application
compilers; only the requested build checks them. Machine profiles stay separate
from exported designs. Starter content uses a neutral, editable project title.
Narrow hosts use Nyx-built Project / Design / Inspector switches to give each
panel the full workspace. Pascal opens the optional source split when requested.
Component discovery offers **List / Grouped**, purpose filtering and **Details**
for readable creator descriptions. Search understands intent labels, aliases and
help text. Creators supply metadata through the
[typed catalog API](docs/components.md#component-discovery-and-creator-descriptions).

[Architecture](docs/architecture.md) · [Current validation](WORK.md) ·
[Tasks](TODO/README.md) · [MIT license](LICENSE)

Report features or bugs through the repository's GitHub issues with reproduction
steps. Original interface-based demos are retained for reference.

**Tip Jar**
  * :dollar: BTC - bc1q55qh7xptfgkp087sfr5ppfkqe2jpaa59s8u2lz
  * :euro: LTC - LPbvTsFDZ6EdaLRhsvwbxcSfeUv1eZWGP6
