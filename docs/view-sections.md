# Independent mounted view sections

[Architecture](architecture.md) · [Current work](../WORK.md) ·
[Accepted section prerequisite](../TODO/DONE/NS-2_section-publication_01.md)

`nyx.view.sections` lets an application replace one independently owned view
without rebuilding neighboring sections. The adapters use the ordinary Nyx
renderers, factories, bindings, events and collection mounts. They do not change
the meaning of the portable document or relax retained `TryRefresh` admission.

The application supplies dedicated attached hosts through
`NewNyxBrowserViewSection` or `NewNyxLCLViewSection`. Keep that choice in the
application's target adapter. Both return specialized managed interfaces extending
the portable `INyxViewSection`; ordinary preparation/publication uses the same
contract on either target.

```pascal
var
  LHeaderChange: INyxViewSectionChange;
  LDetailsChange: INyxViewSectionChange;
begin
  { FHeader and FDetails are application-owned INyxViewSection references.
    Each adapter factory received an empty, dedicated child host. }
  LHeaderChange := FHeader.Prepare(AHeaderDocument, AHeaderDocument.Pages[0]);
  LDetailsChange := FDetails.Prepare(ADetailsDocument, ADetailsDocument.Pages[0]);

  if not PublishNyxViewSections([LHeaderChange, LDetailsChange]) then
  begin
    { Revision, host geometry, schema or dispatch readiness changed.
      Keep the accepted views and prepare again on a later UI turn. }
    LHeaderChange.Cancel;
    LDetailsChange.Cancel;
  end;
end;
```

Use distinct `TNyxViewSectionRef.Named('header')` and
`TNyxViewSectionRef.Named('details')` references at construction. Names are open
application Unicode data, separate from control IDs. Empty names, malformed
Unicode and NUL raise. A default reference is undefined.

## Publication and refusal

`Prepare` synchronously renders an independent candidate in a private parking
host. Ordinary renderer admission copies the source tree and validates its
document/schema/recipes/defaults. The caller may release the source document
after preparation. The candidate has its own realization, event router and
collection scope. The current section still supplies all live lookups.

`PublishNyxViewSections` validates every handle and checks every section before
moving controls. Nil, foreign, already published/canceled or duplicate section
objects/names raise. A stale section revision, changed schema revision, changed
parent/client allocation, occupied host or busy renderer returns `False` before
placement changes. That refusal leaves handles prepared for explicit cancellation.
An empty group returns `True` without changes.

Physical preview moves accepted controls into rollback hosts and candidates into
the real hosts. A failing preview triggers reverse rollback, including the member
which partly moved before raising. Every member is canceled; the exception
propagates. Rollback attempts every member even if an extension's rollback itself
fails, and reports that additional failure. An extension that cannot restore its
target cannot promise atomic physical rollback.

After all previews succeed, each section adopts its new renderer and advances its
revision. Only then are the former renderers retired. Unmentioned sections keep
their actual controls, edit drafts and event/collection routes. Changed sections
receive fresh views; their old collection mounts disconnect and event
registrations retire. Revisions belong to **view publication** and do not stand
in for document/MCP revisions, paired Pascal history or Undo.

## Lifetimes and dispatch

Hosts and their parents are borrowed. They must outlive section references and
every outstanding prepared change because staging and rollback hosts are siblings.
The browser host is attached and initially empty. The LCL host is an initially
empty child of a live parent. Foreign children refuse replacement instead of being
erased. Use an application host layer to arrange these sections.

A prepared handle leases its section. Canceling or dropping its last reference
releases the unpublished candidate and parking hosts. `Cancel` is idempotent
after cancellation; it raises during preview and after publication. A published
handle keeps only copied identity/revision information. `Close` retires the
accepted view and advances its revision, making other proposals stale. Cancel or
release those proposals before destroying the borrowed hosts.

A nonnil `TNyxState` passed to `Prepare` is caller-owned and must outlive both
prepared and accepted views which borrow it. Nil creates an independent store
from authored defaults for that replacement. A replacement refuses a store owned
by its retiring renderer; sharing that pointer would leave a dangling binding.

`Prepare` and `Close` raise while their current renderer is dispatching input or
model notifications. Group publication returns `False` while current or candidate
renderers are busy. Queue such changes for a later UI turn through the Nyx
scheduler. Readiness includes collection selection/query notifications as well
as store publication. Custom collection views must implement the optional
`INyxCollectionViewPublication` readiness capability; unknown views refuse
retirement. The existing `INyxCollectionView` interface remains unchanged.

`INyxViewSectionObserver` is managed and receives only exact nodes in the
currently accepted view. A candidate with matching textual IDs cannot impersonate
those nodes. The callback borrows the node synchronously; copy identity/text or
use revocable ports for queued work. Do not retain a section in its own observer.

The optional target configurator is a borrowed method receiver and must outlive
section/change handles. Register factories before candidate rendering; do not
retain candidates, subscribe to their events, pump queues or schedule work from
that configurator. Registered target factories/updaters must obey ordinary
renderer lifetime rules and must not reenter section publication. The specialized
section's borrowed `Renderer` is for lookups and event subscriptions. Never call
its `Render`, `Unmount` or `MoveHost` directly.

Both target factories accept an optional borrowed theme after the observer. It
must outlive the section and prepared change handles. Each candidate uses the
ordinary renderer's theme admission; no section-specific styling toolkit exists.

## Target extension obligations

`INyxViewSectionPublication` is the explicit adapter boundary. `Check` has no
side effects; `Preview` performs reversible target placement; `Rollback` restores
the old placement after partial failure. `Publish` performs prepared assignments
only: no allocation, target work, callbacks or exceptions. `RetirePrevious`
disconnects former owners after the entire group publishes. Destructors must not
raise. Run all phases synchronously on the owning UI thread without pumping it.

## Qualification and remaining integration

Run `tools/build.ps1 -Target view-sections` to compile and execute the checked
Win32 consumer and stage pas2js/HTML/matched RTL artifacts. This command starts no
HTTP listener. Serve the exact staged closure through an admitted existing host
and execute `view-sections.html` with the maintained Pascal browser-pipe driver,
using `data-test-result` as the terminal marker. The fixture pauses for a live
capture checkpoint before retiring its views.

The maintained consumer exercises actual controls, typed table edits, independent
Unicode drafts, factory/second-preview failure, grouped rollback, stale/duplicate
refusal, allocation changes, callback/collection notification refusal and explicit
scope retirement on both targets. Native heap evidence belongs to that consumer;
browser-driver heap tracing is not a JavaScript heap audit. Native printed-control
and browser desktop/narrow captures help inspect the bounded presentation.

The ordinary Studio controllers now consume the
[mounted routing contract](studio-section-routing.md). Its current work packet
owns integration qualification, paired drafts/history and full workload timing;
the independent publication fixture alone does not qualify those outcomes.
This prerequisite establishes neither a Studio speedup nor hardware, phone,
assistive-technology or other-widgetset qualification.
