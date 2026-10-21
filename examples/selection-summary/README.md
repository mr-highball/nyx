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
consumer in ../../tests/nyx_coordinator_controls.lpr provides a complete native
and browser application with two independent stores.

Each future render captures an immutable registry snapshot. Changing the caller's
registry does not rewrite an accepted view. Without this factory registration,
the saved controls remain static; the versioned intent cannot load executable
code or acquire file, HTTP, compiler or other host services.

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

```powershell
./tools/build.ps1 -Target coordinators -CoordinatorEvidenceDirectory <new-directory>
```

Use a fresh evidence directory. The command executes the portable native
contracts and ordinary native controls at desktop and compact widths, then
compiles matching browser assertions and stages their HTML/runtime. It starts
no server or listener. Browser compilation does not establish execution or input.
Current results and any retained failures belong in [WORK.md](../../WORK.md).

This is the independently useful registration example. Built-in migration,
shared ownership with manual MasterDetail, and full reusable/embedded adoption
remain acceptance requirements of [common coordinators](../../TODO/NS-3_coordinators_01.md).
