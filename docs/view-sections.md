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

`Prepare` synchronously renders an independent candidate in a private target
host. Ordinary renderer admission copies the source tree and validates its
document/schema/recipes/defaults. The caller may release the source document
after preparation. The candidate has its own realization, event router and
collection scope. The current section still supplies all live lookups.

`PublishNyxViewSections` validates every handle and checks every section before
changing physical presentation. Nil, foreign, already published/canceled or duplicate section
objects/names raise. A stale section revision, changed schema revision, changed
parent/client allocation, occupied host or busy renderer returns `False` before
placement changes. That refusal leaves handles prepared for explicit cancellation.
An empty group returns `True` without changes.

Browser preview moves accepted controls into rollback hosts and candidates into
the real hosts. Native preview uses private presentation planes beneath the same
stable host: the accepted plane is hidden and the prepared plane is shown.
Neither native subtree changes parent, preserving its allocated window handles.
The renderer guards focus and grid validation during these transitions so hiding
an editor cannot accept its unfinished draft. Final ordinary synchronization and
registered updater validation still run before publication.

A failing preview triggers reverse rollback, including the member whose physical
transition partly completed before raising. Every member is canceled; the exception
propagates. Rollback attempts every member even if an extension's rollback itself
fails, and reports that additional failure. An extension that cannot restore its
target cannot promise atomic physical rollback.

After all previews succeed, each section adopts its new renderer and advances its
revision. Only then are the former renderers retired. Unmentioned sections keep
their actual controls, edit drafts and event/collection routes. Changed sections
receive fresh views; their old collection mounts disconnect and event
registrations retire. Revisions belong to **view publication** and do not stand
in for document/MCP revisions, paired Pascal history or Undo.

Retirement can fail after the new group has already become authoritative. Every
member's former view still receives its retirement attempt before the first
cleanup error is reported. That error does not undo the accepted publication;
inspect the current section revisions before deciding on a subsequent change.

## Lifetimes and dispatch

Hosts and their parents are borrowed. They must outlive section references and
every outstanding prepared change. The browser host is attached and initially
empty; its staging and rollback hosts are siblings. The LCL host is an initially
empty child of a live parent; its private native planes remain children of that
host until retirement. Each pending native proposal owns its own hidden plane,
and the section owns the accepted plane. Multiple prepared proposals can coexist;
publishing one makes the others stale through the ordinary section revision.
Only planes belonging to that exact section are recognized. Foreign children
refuse replacement instead of being erased. Use an application host layer to
arrange these sections.

A prepared handle leases its section. Canceling or dropping its last reference
releases only its unpublished candidate and private target hosts. `Cancel` is idempotent
after cancellation; it raises during preview and after publication. A published
handle keeps only copied identity/revision information. `Close` retires the
accepted view and advances its revision, making other proposals stale. Cancel or
release those proposals before destroying the borrowed hosts.

Explicit `Cancel` becomes terminal before invoking fallible candidate cleanup.
A reported cleanup error leaves that handle canceled, so retrying `Cancel`
cannot repeat its former ownership duty. `Cancel`, `Close` and postpublication
retirement report supported disconnect failures after attempting independent
cleanup. Dropping the final reference also attempts retirement, but destructors
must not raise; use an explicit operation when the caller needs its diagnostic.

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

Native collection-grid factories used in a section currently need
`TNyxCollectionStringGrid` or a subclass. Its counted presentation guard prevents
LCL focus-exit validation from accepting or resetting an unfinished cell draft.
A custom raw `TStringGrid` lacks that guard and refuses native activation; an
independent data-view readiness capability alone does not supply it.

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

The optional `INyxViewSectionCandidateRetirement` facet supplies terminal,
idempotent `RetireCandidate` cleanup while the publication port is still alive.
It leaves the original publication GUID and vtable unchanged. Implement it when
cancellation has fallible disconnect work to report; release raw objects only
after that explicit work, through nonthrowing destructors. A control or observer
destructor must not be used as a fallible retirement callback.

## Qualification and remaining integration

### Enclosing presentation space

An independent renderer normally evaluates viewport rules against its own logical
host allocation. This remains the default for standalone applications and views.
A section embedded in a desktop sidebar can instead share the application's
presentation viewport without pretending its sidebar is as wide as the window:

```pascal
{ Both target renderers expose the same runtime contract. The outer view has
  already rendered into its application host; all calls use its UI scheduler. }
FDetailsRenderer.PresentationViewport := FApplicationRenderer.ViewportContext;
FDetailsRenderer.Render(FDocument, FDocument.Pages[0], FDetailsHost);
```

The section factories accept that same optional `INyxViewportContext` after the
theme argument. For example, a native host adapter can create its section with
`NewNyxLCLViewSection(TNyxViewSectionRef.Named('details'), FHost, nil, nil, FTheme, FViewport)`.
The browser factory
has the corresponding final argument. Studio supplies its Core renderer's context
to independently mounted editor sections.

`PresentationViewport` is host configuration. Set it before rendering or while
the view is idle; `nil` restores the allocated-host default. Direct and named
viewport conditions, including structural recipes, consume the shared dimensions.
Explicit named **container** conditions continue to use the nearest eligible
measured local ancestor. Physical bounds, scrolling and the layout allocation
remain local. Manual presentation selection remains independent for each view.
The lower-level `TNyxViewFrame.At(...).WithinViewport(...)` expresses both spaces
when realizing a tree without an adapter.

`ViewportContext` exports the renderer's resolved presentation space. Consumers
can export it again for nested scopes even when every host has a different local
width. Observations use logical pixels, preserve fractional dimensions and
distinguish an unknown initial value from a measured zero. A rule-free browser
parent still observes resizes when it exports this context. Updates coalesce
through the UI scheduler, rather than replacing a child during a parent callback.

Each consumer owns an independent subscription. Dropping its subscription cancels
that borrowed callback; retaining a reader or token does not retain a renderer,
document or target control. The writer's retirement revokes all receivers. The
last snapshot stays readable, but an inactive source cannot supply a new frame.
Ordinary structural replacement keeps the logical renderer's exported port;
explicit `Unmount` or destruction retires it. Remounting exports a fresh port.
A section replacement owns a different renderer and therefore requires explicit
rebinding of readers that should follow its successor. Retire dependent views
before destroying their enclosing host.

Prepared sections and retained form publications fence the enclosing source's
revision as well as their local geometry. An outer-only resize can therefore
refuse an obsolete proposal even when the child slot did not resize. Section
publication returns `False` for a stale proposal; retained-form publication
reports its readiness failure and restores its previous input/context on retirement.
An unsuccessful deferred adapter refresh exposes `PresentationViewportFailure`;
structural recipe admission additionally reports `LastContentError`.

The context belongs to `nyx.viewport.context`; its copied observation is
`TNyxViewportContextSnapshot`, distinct from a control's scroll viewport snapshot.
Custom hosts can own a writer through `NewNyxViewportContext(Scheduler)`, publish
validated dimensions and pass its read-only `Context` to renderers. All operations
belong to that scheduler's UI thread. Runtime host wiring is not persisted into
designs and introduces no raw-string authoring option or generated-code change.

Run `tools/build.ps1 -Target viewport-context` to execute the checked native
ownership and real-host journeys and stage their browser counterparts. Supply a
**new** `-ViewportContextEvidenceDirectory`, or configure the private
`EVIDENCE_ROOT`; `TEMP_ROOT` also redirects compiler subprocess scratch files.
Serve the staged `viewport-context.html` and `viewport-context-contracts.html`
through a local preview host and inspect their `data-test-result` markers.
This command starts no listener. The actual-host journey covers enclosing/local
widths, nested resize, named containers, exact Unicode selection and numeric
draft transfer, stale section/form refusal, replacement, retirement and remount.
It does not by itself establish full Studio project-pair navigation, zoom/scroll,
phone hardware or assistive-technology acceptance.

Run `tools/build.ps1 -Target view-sections` to compile and execute the checked
Win32 consumer and stage pas2js/HTML/matched RTL artifacts. This command starts no
HTTP listener. Serve the exact staged closure through an admitted existing host
and execute `view-sections.html` with the maintained Pascal browser-pipe driver,
using `data-test-result` as the terminal marker. The fixture pauses for a live
capture checkpoint before retiring its views. Observe that checkpoint, capture
the live presentation, then click the fixture's **Continue after capture** button.
That ordinary Pascal harness action acknowledges the existing checkpoint; it
does not replace the recovery assertions or their capture time bound.

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
