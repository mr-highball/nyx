# Selection Summary creator example

This compound uses ordinary Nyx Card, multiple-selection List, Label and Button
controls. Its separately registered Pascal factory projects the retained
selection count and enables Clear when membership is nonempty. Count explicitly
includes selected items hidden by the current query. Clear changes membership
through that same collection view; it preserves the query, rows and defaults.

The example unit supplies specialized INyxSelectionSummary part access and
INyxSelectionSummaryBinding commands. Generic renderer lookup returns the exact
custom binding, so Supports can retrieve the specialized interface.

## Use in an application

Add this directory to the compiler unit search path. Import nyx.selection.summary,
nyx.coordinators, nyx.collections.view.types, nyx.controls and nyx.types along with
your normal application units. After defining a people collection with a text
field named name, build the component and explicitly register its behavior:

```pascal
LRegistry := NewNyxCoordinatorRegistry;
RegisterSelectionSummary(LRegistry);
LRenderer.CoordinatorRegistry := LRegistry;

LSummary := NewSelectionSummary('people-summary',
  NyxCollectionView(NyxCollection('people')).
    Column(NyxTextField('name'), 'People').Selection(nsmMultiple));
LPage.Add(LSummary);
```

LRegistry is INyxCoordinatorRegistry, LSummary is INyxSelectionSummary and LPage
is an ordinary INyxPage. LRenderer is either application renderer. Mount the
document through your existing application/collection context. The ordinary
consumer in ../../tests/nyx_coordinator_controls.lpr exercises ordinary native
controls and matching browser assertions through two independent renderer stores.

For an ordinary application, assign its borrowed View.CoordinatorRegistry before mounting:

```pascal
LRegistry := NewNyxCoordinatorRegistry;
RegisterSelectionSummary(LRegistry);
LApp.View.CoordinatorRegistry := LRegistry;
```

LApp is TNyxLCLApplication before Mount, or TNyxBrowserApplication before Run.
Each application supplies its own runtime collection owner. Reusable instances
use NyxCollectionView(...).Scoped(csInstance) for independent stores, including
when two applications borrow the same authored document. Keep that document
unchanged and alive until both applications are destroyed. App.ShowPage navigates
the existing runtime; a rejected page must preserve the accepted view.

The separate [reusable application consumer](../../tests/nyx_coordinator_reuse_controls.lpr)
mounts two instances in each of two actual application objects, alongside
MasterDetail and Kanban. It resolves nested roles on the realized workspace and
retrieves INyxSelectionSummaryBinding through Supports on CoordinatorBinding.
Realization can project the authoring compound to an ordinary Card; retrieve
standard named parts and the specialized runtime binding rather than assuming
the custom authoring facade remains attached to that realized Card.

Each future render captures an immutable registry snapshot. Changing the caller's
registry does not rewrite an accepted view. Without this factory registration,
the saved controls remain static; the versioned intent cannot load executable
code or acquire file, HTTP, compiler or other host services.

## Embedded registry rules

Saved Canvas item renderers inherit the parent candidate's fixed caller
membership before they build their controls. Future Scene content replacements
inside an accepted Canvas use that same accepted membership, even if the caller
subsequently registers another factory on the parent's mutable registry. A new
parent candidate, such as App.ShowPage, captures the updated registrations.
Failed parent admission leaves its accepted membership and children intact.

Each child composes its own automatic MasterDetail and Kanban factories. Those
built-ins are not part of the inherited caller snapshot and are not duplicated.
For the borrowed child renderer returned by RendererFor, choose future caller
membership through its ordinary CoordinatorRegistry property:

- nil inherits the parent's captured caller snapshot.
- A nonnil registry supplies explicit caller membership instead of inheritance.
- An explicitly empty NewNyxCoordinatorRegistry suppresses caller factories
  while retaining the child's automatic built-ins.

Assignment configures the next candidate; it does not rewrite the accepted
child. Existing structural Presentations.Select/Automatic can remount the child
from its own retained source and collection bindings. Do not manufacture a
replacement document/context just to apply an override. Each child retains its
own event generation and weak refresh capability; replacement/disposal revokes
its commands independently. A structural remount preserves the child's compatible
stores; Scene content replacement creates a distinct child lifetime without a
store-migration promise. Inherited configuration survives Unmount, while
accepted view membership retires with that view. The current qualification
boundary and retained outcomes remain in [WORK.md](../../WORK.md).

## Creator lifetime contract

Factories receive copied intent and scoped typed role discovery. Reserve text
and availability roles during Prepare; the driver seals those claims before
physical controls are built. Initial text/availability is ready before Build.
Availability is an AND constraint that preserves the other enabled policies.
Text conflicts and preexisting text bindings refuse admission.

Subscriptions use the supplied future event view and remain dormant until
Install. Install adopts preallocated ownership with flags only. Revocation first
removes local authority; fallible cleanup runs afterward and attempts independent
resources. Weak refresh checks the installed generation and is the final outward
call because it can dispose the complete view. Retained custom commands must
check their own generation and revocation before using collection authority.

Factories are trusted Pascal application code. These capabilities are lifetime
contracts, not a security sandbox. A retained control facade owns model memory,
not its document, parent, widget or permission to update a retired view. The
driver keeps canonical facades private; this example retains no raw nodes,
widgets, document or renderer.

## Maintained qualification command

The factory establishes a managed prepared-interface owner before Initialize
can call outward or acquire temporary Self leases. Construction itself stays
inert, so initialization exceptions also unwind through that existing owner.
This matters on pas2js, where constructor-time Self leasing cannot assume the
native compiler's construction lifetime protection. For a capability retained
in a field, query Supports into a local interface and then assign that local to
the owning field. The local and field each retain their own reference; generated
local cleanup must not consume the field's intended lease. The example source
documents both patterns without changing its public contracts.

```powershell
./tools/build.ps1 -Target coordinators -CoordinatorEvidenceDirectory <new-directory>
```

Use a fresh evidence directory. The command executes the portable native
contracts and ordinary native controls at desktop and compact widths, then
compiles matching browser assertions and stages their HTML/runtime. It starts
no server or listener. Browser compilation does not establish execution or input.
Current results and any retained failures belong in [WORK.md](../../WORK.md).

Run the ordinary reusable-application proof separately, preserving a new packet:

```powershell
./tools/build.ps1 -Target coordinator-reuse -CoordinatorReuseEvidenceDirectory <new-directory>
```

This command executes the application consumer at1280 and390, then compiles and
stages matching browser assertions. It starts no browser or listener. The retained
native result is588 checks at each width, preserving the original267 reusable
checks before the embedded cases, with clean heaps and no Nyx compiler warnings.
Separate reusable/embedded design and Pascal exports match across widths and
preserve authored/default baselines. This command does not perform Save/Open or
compile those exports as independent applications.

## Qualified behavior and boundaries

The same maintained application has also completed actual browser input and
lifetimes at1280x900 and390x900:256 reusable checks and563 aggregate checks at
each width, final cleanup complete and no final console warnings/errors. Two Apps
each mount two reusable workspaces, isolating twelve stores across Summary,
MasterDetail and Kanban. Saved Canvas children exercise captured caller membership,
later content replacement, explicit/nonempty/empty/nil overrides, refusal,
navigation, stale commands and independent child/peer disposal.

The browser execution is retained from the renderer version before Studio's
facelift; it is not a rerun of the current renderer bytes. Both adapters' complete
canvas setup, typed lookup, candidate publication, clear, refresh and context
methods remain unchanged. Current facelift browser/input checks cover the changed
button/presentation boundary. Current full MasterDetail Studio Save/Open and its
own newly generated application pass165/175 and36/36; the equivalent Kanban
journey passes114/124 and15/15. Those are separate compatibility journeys using
their own saved pairs, not compiled exports from the reusable application above.
[WORK.md](../../WORK.md#reusableembedded-creator-acceptance--2026-10-21) records
the exact source comparison, current compatibility evidence and joint assessment.

Focused native tests separately exercise injected reentrant validation, final
fences, abandonment and notification-triggered disposal; browser counterparts of
those injections compile only. The563 actual browser checks establish their own
useful input/refusal/lifetime paths. DOM-handler input, Win32 native results and
controlled viewport widths do not establish hardware/assistive input, macOS/other
widgetsets, DPI or production performance qualification. Full target quality
remains with [parity](../../TODO/NS-2_parity-accessibility_01.md).

### Retained failures

Earlier browser runs failed in Summary construction ownership, Kanban managed-array
ownership, the consumer's global Summary storage and shared collection-view
ownership. Each failure, narrow repair and stopped host remains in
[the first execution](../../WORK.md#first-reusableembedded-browser-execution--2026-10-21),
[the repaired execution](../../WORK.md#repaired-reusableembedded-browser-execution--2026-10-21),
[the next Kanban execution](../../WORK.md#kanban-repaired-browser-execution--2026-10-21)
and the later checkpoints in WORK. Their failed outcomes are not acceptance;
the final completed application above supplies the browser result.

This is the independently useful registration example. Scoped
[reusable/embedded adoption](../../TODO/DONE/NS-3_coordinators_05.md) consumes the
accepted built-in migrations and preserves their manual APIs/shared ownership.
[Common coordinators](../../TODO/NS-3_coordinators_01.md) remains open on its
existing parity prerequisite; this example does not accept DataToolbar or full
component/authoring/parity delivery.
