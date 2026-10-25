# Retained view arrangements

[Responsive authoring](responsive.md) · [Architecture](architecture.md) ·
[Evidence and remaining work](../WORK.md#retained-bound-state--2026-10-06)

Both adapters can now retain an unchanged set of realized controls when a fresh
design moves them between ordinary layout hosts or changes their sibling order.
`TryRefresh` performs fresh document/property/context admission, then chooses the
strict scalar path or the separately checked structural path. A successful move
keeps the same realized nodes, physical controls, event bindings and emitter scope.
Unchanged authored values retain independent runtime drafts. Actual authored deltas
and explicit field restorations still follow runtime identity.

Projection context still admits the complete current design before excluding
unrelated authored roots. Within that synchronous operation, the codec reuses
its already formatted export packet for the formatted UTF-8 read limit, rather
than formatting the admitted tree again. Exact decimal spellings, Unicode,
formatter preferences and all original byte/depth/member refusals remain
significant. This is temporary value reuse, with no persistent document cache
or skipped admission. The [current measurement](../WORK.md#active-prerequisite--repeated-context-formatting)
keeps isolated allocation/timing evidence separate from whole-editor latency.

Scalar state bindings can also retain that lifetime. The owning
`TNyxLiveBindings.TryPrepareRefresh` validates independent candidate and baseline
copies against the same current runtime store. Ordered binding descriptors,
identities, scopes and the complete realized control set must stay exact.
Both copies use current state, so an old document default cannot replace an
accepted runtime value during layout publication. The existing coordinator,
subscription token and store stay attached; refresh publishes no state revision
or notification. External state writes continue through the original validator.

The public unbound guards retain their strict meaning. Explicit
`CanRefreshNyxBoundProjection` / `CanArrangeNyxBoundProjection` are adapter
boundaries for coordinator-prepared roots, not substitutes for state admission.
Commands and store validation/notification refuse refresh reentry. Read-only
`RefreshReady`, `HasBindings` and `TNyxState.Busy` observations require the UI
thread; they are neither cached admission nor cross-thread locks. Unbound views
keep their existing property path without additional state-projection clones.

An unfinished numeric draft remains independent of the accepted bound number.
Unchanged accepted-value markers prevent ordinary Sync from rewriting it during
a move or an unrelated state update. A typed `TNyxProjectionValueRestore` resets
only its exact field: unbound fields use the authored default; bound fields use
the current accepted store value. The complete group is checked before mutation,
and only requested faces reset their accepted-value marker. Other field drafts
remain untouched. Browser HTML number inputs may sanitize unfinished text to an
empty physical draft; native edits retain literal trailing decimal text.

For a form whose accepted context changes, the portable
`TNyxProjectionFormContext.ForForm` contract names its exact runtime/design
identity and explicit extension metadata keys. `CanRefreshNyxFormProjection`
and `RefreshNyxFormProjectionProperties` retain the same shape while publishing
those existing metadata properties with the form's newly accepted values.
Unchanged defaults also reset inside a changed form; an absent accepted Value
removes the old runtime property. Unchanged neighboring forms keep their drafts.
The corresponding Bound APIs require independently coordinator-projected roots,
as the ordinary exact-binding boundary does.

This is an explicit opt-in portable contract. It does not change the strict
default APIs or permit arbitrary custom-property changes. Missing properties,
changed semantic owners, binding changes and duplicate/nested context scopes
refuse before publication. The host owns its metadata meaning and must fence
pending actions from the previous context. Before physical adoption it also uses
`PrepareNyxProjectionFormRestores` to reset the exact input accepted-value
markers, and preserves ordinary rollback and input lifetimes.

The restore-preparation overload also returns whether context metadata changed,
independently of its input-reset count. An empty form can still have old queued
callbacks to retire. Refused preparations clear both outputs. The form contract
passes 35 shared native/browser checks, with a clean native heap and empty
browser logs. The previous unchanged native retained-control journey passes
174 checks with a clean heap against its recorded source cohort.

`nyx.projection.context` couples a prepared successor event generation with an
adapter's reversible target publication. The optional `INyxEventViewPreview`
facet refuses pending/running old work or an entered dispatch before any target
change, preserving those tickets without cancellation or replay. Once admitted,
new dispatch refuses until installation or complete rollback. Target rollback
finishes before the event guard retires, so focus/parenting callouts cannot run
old callbacks against a partly restored form. Successful installation rotates
one event generation without callbacks; notification follows all installations.
This explicit preview facility does not change ordinary prepared-view behavior.
Real event generations also own scheduler work domains. A completed or pruned
callback cannot hide pending descendants posted with its execution as parent.
Preview refuses while that work remains active, preserving its queued tickets.
An admitted preview temporarily blocks new work through the old context;
rollback reopens it and installation retires it permanently. An unsupported
scheduler refuses this optional path instead of claiming quiescence.

The corresponding `PublishNyxViewSections` overload accepts that prepared effect
and returns a separate committed flag. It checks the original readiness fence
after every section preview, rolls failed previews back in reverse order and
attempts cleanup on every outcome. It retains the original effect through caller
reference withdrawal. Notification/retirement failure keeps committed data;
an already propagating first exception retains its exact identity even when
cleanup also fails, including raw JavaScript throw values.

Reentrant retirement marks the wrapper unavailable immediately, but defers
detaching an entered validation/notification group until that phase returns.
This keeps its participant vector alive through target and scheduler callouts;
retirement during validation refuses before preview, while notification
retirement preserves already installed data.

This portable publication consumer now passes 58 native checks with a complete
clean heap and 114 actual browser checks with empty logs. The browser additionally
checks raw throw values, including undefined, null, false, zero and an object
whose string conversion refuses. The unchanged event-publication regression
passes 58 per actual target.
These use real routers/coordinators and injected target phases. The maintained
`retained-arrangement` target includes portable form and retained-context
publication consumers, plus the actual physical form consumer described below.

Both physical renderers expose `TryPrepareFormRefresh(Document, Root, Forms)`.
Its exact form contexts are the same explicit portable contract. Nil means the
form cannot be retained or its old event work remains busy; it leaves accepted
controls, drafts and pending work untouched. The ordinary replacement path is
still available. Invalid source raises through ordinary document admission.

A nonnil participant has already previewed the new values on the retained
controls and fenced ordinary dispatch. Include it in `PublishNyxGroup` or the
effect overload of `PublishNyxViewSections` to install it with other changes.
Retiring or releasing an uninstalled participant restores the old metadata,
accepted baseline and copied physical drafts before releasing that event fence.
An installed participant retains its controls and registrations, while old
executions become canceled and later input receives the successor context.
The participant owns its snapshots and a canonical generation lease; it never
owns its renderer or the caller's document. Renderer retirement makes its
borrowed target unavailable without reviving the old view.

The actual form consumer passes 38 native checks with a complete clean heap and
34 browser checks at independently verified desktop and compact widths, with
empty warning/error logs. It exercises bound/unbound numeric drafts, refusal
with a real queued descendant, rollback, same-destination interface assignment,
event retirement and subsequent independent state writes. The authored document
stays exact. Studio's stock Inspector adoption is qualified separately: its
complete section consumer now passes 93 native
checks with a complete clean heap and 84 browser checks at desktop and compact
widths, with empty logs. Independent property, reusable-copy, viewport and
geometry contexts keep same-owner drafts and reset only the changed form.
Complete ordinary editor performance and independent application production
remain required.

The generic prepared-group notification loop attempts every original receiver
after an earlier receiver fails. Nested groups retain their original participant
vector through caller reference withdrawal. Pascal diagnostics preserve exact
empty or Unicode text; a raw browser failure retains its original value and
identity without string conversion. Event-view retirement likewise attempts
every captured execution cancellation before reporting its first failure. The
consumer exercises both direct/nested groups and two actual completed router
invocations whose cancellation fails before a later target notification.
Installed data stays committed and all adopted preparations retire. This resolves
the earlier notification gap. The actual form path above now uses descendant
admission; complete Studio adoption and performance remain separate requirements.

The portable `TNyxNode.ArrangeLike` boundary prepares every owner array and actual
managed implementation anchor before publication. It adopts no candidate node.
Runtime/source/design identities, instance scopes, complete node set and root stay
exact. Duplicate identities, foreign roots and count/depth budgets refuse before
changing ownership. Prepared arrays replace owned children; borrowed parent links
follow that same independent shape. Managed implementations survive owner changes
and release exactly once at teardown.

`CanArrangeNyxProjection` aligns independent comparison copies and applies the
original scalar compatibility checks. Changed contracts/extensions, constructor
properties, binding contracts, collection semantics, creators/factories or document context
retain their full-mount requirement. Reparent/order changes are admitted only at
ordinary page, row, column, grid, panel, card, group-box, form, toolbar and sidebar
hosts. Special pane hosts remain independent. Unchanged hosts are not reattached,
preserving private native/DOM panes, captions and glyphs.

Physical publication walks the new parent arrangement before its descendants,
retains existing faces, suppresses incidental focus/editing callbacks, and restores
supported ranges and scroll positions. Native keyboard order follows new child
order. Win32 handles may be recreated by LCL while the control objects stay retained;
the selection writer validates platform capability and the requested scalar range
against current Text, independently of a transient current read. Each adapter keeps
an independent previous clone for property/arrangement rollback on target failure.

Reproduce the shared ownership and actual native control checks with
`tools/build.ps1 -Target retained-arrangement`. Supply
`-ArrangementSourceDirectory` with exact bounded MCP-exported `nyx.generated.view.pas`
to exercise an agent-authored companion. The small English
`tests/arrangement-review.operations.json` composes that base in one semantic group.
For bound-state qualification append
`tests/bound-arrangement-review.operations.json` to that same semantic group. The
maintained persistent Pascal `nyx_mcp_designer_review` client owns one empty review
through composition, bounded export, both compiler requests and retirement.
One-shot `nyx_studio_mcp call` invocations open separate transports; a temporary
review from one call cannot be used by another. The control fixture then attaches
state and bindings through explicit typed Pascal calls. Those attachments qualify
library consumers; general semantic state/binding authoring remains with the
existing MCP workflow owner and is not claimed by this companion.
Browser artifacts stage under `build/retained-arrangement/maintained/web`; observe
their Pascal-owned markers through the maintained browser driver, using `projection`
for retained controls and the generic marker mode for the ownership fixture.
The `?compact` control fixture uses a 390-pixel host, not a physical phone.
The same `projection` marker observer runs `bound-arrangement.html`; `bindings`
observes the original browser state/control regression suite.

This is the ownership foundation for alternate presentations. Lazy alternate
recipes, fluent structural rules, ordinary Studio
presentation switching, IME continuity, widgetset/DPI/accessibility and release
performance remain with the original task owners. Rearranging the design through
MCP still uses an expected revision and one paired Undo group; the runtime ownership
primitive does not replace editor transactions or persist an authored document.
