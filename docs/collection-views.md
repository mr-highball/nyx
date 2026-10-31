# Collection views and target controls

[Collection stores](collections.md) · [Scalar bindings](studio-state.md) ·
[Accepted collection foundation](../TODO/DONE/NS-1_state-collections_01.md)

The shared `INyxCollectionView` contract projects a typed collection into a list,
table or hierarchy. Browser and LCL renderers can attach it to an already mounted
Nyx control through `BindCollection`. The ordinary authored path uses the same
immutable specification through a specialized control's `Binds.Collection`.
Both renderers mount authored bindings automatically. Studio's Nyx-built Data
and Bindings panels edit saved defaults and those typed specifications.

```pascal
LTasksTable := NewNyxTable('tasks-table');
LTasksTable.Binds
  .Collection(
    NyxCollectionView(NyxCollection('tasks'))
      .Column(NyxTextField('title'), 'Task', cmEditable)
      .Column(NyxBooleanField('complete'), 'Complete'))
  .Done;
LPage.Add(LTasksTable);
```

Declare `LTasksTable: INyxTable` and import `nyx.controls`, `nyx.collections` and
`nyx.collections.view.types`. Saved collections are defined on
`LDocument.Collections`; their schemas must match the typed column references.
`Binds.ClearCollection` records explicit unbinding. `Binds.InheritCollection`
removes the local descriptor, restoring a reusable definition's binding. These
choices are distinct in clones, part overrides, source and paired history.

Bound native tables use LCL's viewport wheel mode: browsing scrolls rows while
retaining the active cell, selection and unfinished editor text. Wheel input over
either pinned edge reaches the same outer table. The ordinary cancellable Nyx
wheel callback chain still runs before the native default. Disconnect restores
the original wheel setting on a borrowed grid; unbound grids keep their creator's
policy. Keyboard commands remain responsible for moving the active selection.

Saved JSON-backed definitions use `LDocument.ResourceCollections.Define` with
a typed `TNyxResourceRows` recipe. Authored snapshots then contain empty schema
seeds; both hosts/renderers resolve independent application/instance stores at
their explicit initial resource locale. The same ordinary `Binds.Collection`
contract mounts their tables. Static collection editors refuse these recipes
until source authoring is supplied. Changed resource datasets now prepare shared
and resolved reusable rows together with scalar/catalog updates; equivalent
source rows preserve local edits, and later scopes use the installed seed.
See [saved row recipes](resources.md#saved-row-recipes)
for crafted source, ownership, materialization and current qualification limits.

## Pausing live inspection

The optional `INyxCollectionInspection` capability pauses an individual view's
display while its **same authoritative store** and independent peer views keep
receiving updates. Obtain it with `NyxCollectionInspection(LView)` from
`nyx.collections.view`; alternative implementations may return nil. The owned
List, Table and Tree views support this when their source provides prepared
atomic publication. Ordinary views keep their previous behavior until configured.

Authored defaults use the same public policy on the view specification:

```pascal
LServices.Binds.Collection(NyxCollectionView(NyxCollection('services'))
  .Column(NyxTextField('name'), 'Service')
  .Inspection(NyxInspectionPolicy
    .PausedBytes(16 * 1024 * 1024)
    .RemovedIdentities(2048)
    .Follow(ncfKeepPosition))).Done;
```

Normal application construction copies this policy into each independent view.
The policy is active when `NewNyxCollectionView` returns, including on the browser
target; the returned interface owns a live view through its first inspection
command. Temporary constructor interface leases use Nyx's portable construction
lifetime and retire before ordinary caller ownership takes over.
`ncfLastVisible` is supported by flat List/Table views; Tree supports pause and
keep-position and refuses follow-last before mounting. Runtime configuration,
follow commands and pause state never rewrite these saved defaults.
`UseDefaultInspection` removes only the declaration. Absence leaves inspection
unconfigured; explicitly applying `NyxInspectionPolicy` enables its default
bounds. Existing descriptors retain their exact earlier shape when absent;
version 13 carries the complete binding and declared inspection scalars.

Studio's bound-control inspector includes a **Live inspection** section with
follow mode and exact nonnegative byte/identity limits. Apply creates one paired
design/Pascal history entry; reset clears only this policy. Unfinished inputs
stay drafts, and stale binding/schema forms or pending Pascal refuse admission.
Reusable parts acquire independent local defaults. The same semantic
[`inspection` operation](studio-agents.md#structured-collection-context-and-authoring)
derives the current binding after earlier changes in a revision-guarded group.

Applications can still override defaults directly at runtime:

```pascal
LInspection := NyxCollectionInspection(LView);

if LInspection <> nil then
begin
  LInspection.ConfigureInspection(NyxInspectionPolicy
    .PausedBytes(16 * 1024 * 1024).RemovedIdentities(2048));
  LInspection.PauseInspection;
end;
```

Declare `LInspection: INyxCollectionInspection` and import
`nyx.collections.inspection` for the typed retention policy and observations.
`GetDisplayedSource` retains immutable data without a view/store back-reference.
Selection, local queries, disclosure and supported cell copying use this frozen
frame. View edits refuse while paused so stale values cannot overwrite newer
producer values. Deliberate application writes through the independent source
store remain possible; pause never stops or cancels that source.

`NyxInspectionPolicy` defaults to 32 MiB of logical displayed `DataBytes` and
4096 removed identities; callers can raise or lower either bound, including zero.
The byte limit is checked before pause. It excludes widget/query/index overhead
and is not a total-memory guarantee. Pause retains one existing displayed source,
not every intervening snapshot or a change queue. Incoming publications coalesce
into the latest source revision and a saturating pending-publication count.
Removed-ID tracking prevents a removed/reinserted ID from inheriting its old
selection or Tree disclosure. Overflow, including a lowered tracking budget,
requires conservative context reset on resume and is reported explicitly.

`CaptureInspection` returns detached scalar status. `SubscribeInspection` sends
those facts and a typed cause without pretending that pending data changed the
display. Receivers own their subscription and disconnect it before destruction.
Notifications are synchronous on the UI owner; receiver failures are reported
after commitment and do not suppress independent receivers or roll back data.
`ResumeInspection` reprojects the latest source through the existing query/page
policies, retains surviving stable selection and reports lost selection/focus.
Repeated pause/resume is idempotent. Resume before disabling the policy or
reducing its byte budget below the already retained frame.

The [live service inspection example](../examples/service-monitor/README.md#live-inspection)
composes two actual Nyx tables, pause/resume buttons, bound status and a
deterministic feed. Its saved/generated document carries typed inspection defaults;
the runtime binding attaches feed/status actions without replacing an already
configured policy. Native mounted input and the exact generated companion
exercise that boundary; browser consumers currently compile only. This is progress toward
[live-inspection task acceptance](../TODO/NS-3_control-usefulness_04.md).
Full browser interaction, AT/visual qualification, measured scale/memory/input
budgets, native Tree editors and the remaining original criteria stay open.

### Stable reading positions

Configuring inspection also enables identity-based position recovery for flat
Lists and Tables. The native adapter captures its last rendered TopIndex/TopRow
and restores the chosen row after selection/editor synchronization; pinned panes
follow the outer table. This prevents an unchanged offscreen selection from
pulling a reader away after an insertion. Browser flat windows capture an actual
resident visible row and its relative pixel position, realize its new location,
and correct scrollable ancestors in their own scaled CSS coordinates. Detached
or offscreen previews do not acquire a containing-page scroll anchor.

A surviving row remains the anchor through insertion, sorting/filtering and
deliberate resume. If it disappears, recovery chooses the nearest surviving old
successor, then an old predecessor. It never silently chooses a newly inserted
row as that old neighbor. Source removals/reinsertions use an immutable receipt
with exact collection/revision identity, including the coalesced paused interval.
Its removed identities use the inspection policy's bound. Missing, skipped or
overflowed evidence reports `ncpReset`, retaining an approximate ordinal without
claiming identity continuity. A known new page/projection with no surviving old
rows starts at its first row. Empty results report `ncpEmpty` and no invented row.
Recovery changes viewport position, independently of ordinary row selection.

Import `nyx.collections.position` and call
`NyxCollectionPositionState(LMount)` to capture a readonly data port. Alternative
mounts may return nil. Its `Recovery` value contains kind, old/current identities,
source revisions, an observing serial and a requested zero-based index. The
serial distinguishes new recovery from a later selection-only notification,
so application status need not repeat an older explanation. This is the last recovery
proposal; a native widget/browser scroll owner may clamp near its physical end.
`Active` clears before mount cleanup. Retained ports own no mount, widget, event
router, callback or view, so application callbacks can compose status without
creating a renderer/router reference cycle. Read the port on its UI owner; pass
the captured record to workers rather than reading mutable UI state there.
Disconnect/close the application's ordinary behavior binding before retiring
its application as usual.

The service example supplies that port to its public runtime binding and shows
an explanation when the reading row disappears or its position resets. Checked
native fixtures exercise 64 added runtime records through the public handwritten
and exact generated examples, an offscreen selected gateway, insertion, deletion,
sorting, pause/resume and ID replacement at desktop/compact widths. Separate
visible native List and plain-grid journeys qualify those adapter paths and
retained-port retirement. Browser fixtures compile; actual browser input, pixel
validation and complete task acceptance remain open. Native Tree physical
scrolling remains a separate unfinished L1 requirement.

## Collection feedback and feed states

`nyx.collections.feedback` observes an existing flat List/Table view and combines
its displayed counts/pause status with caller-supplied producer activity. It does
not create a store, query engine, loader, update queue or data-admission authority.
The optional compound card in `nyx.collections.feedback.controls` uses ordinary
Nyx labels, an indeterminate progress control and Retry/Reset/Resume buttons.

```pascal
LParts := NyxCollectionFeedbackParts('service-feedback');
LParts.Define(LDocument.State);
LPanel := NewNyxCollectionFeedbackPanel(LParts,
  TNyxCollectionFeedbackText.Defaults);
LPage.Add(LPanel);

// After mounting, using this application's actual context and view:
LFeedback := BindNyxCollectionFeedback(LContext, LView, LParts,
  TNyxCollectionFeedbackText.Defaults, [ncfaRetry, ncfaResume]);
LFeedback.Refreshing;
```

Declare the parts as `TNyxCollectionFeedbackParts`, the panel as
`INyxCollectionFeedbackPanel`, and its runtime binding as
`INyxCollectionFeedback`. The specialized panel derives from `INyxCard`; its
named child controls remain replaceable through the normal Nyx contract. Export
expands the recipe into portable typed controls and scalar bindings; attach the
runtime behavior using the same parts descriptor in the generated application.

`Loading`, `Refreshing`, `Ready` and `Failed(ADiagnostic)` signal producer activity
without changing records. Ready feedback distinguishes a truly empty displayed
source from a nonempty source with no filter matches. `Capture` returns bounded
copied facts: kind/activity, exact diagnostic, source/matched/page counts, source
revisions, pending publications, affordances and an observing serial. Paused
feedback keeps displayed-source counts while incoming revisions advance. Pause
takes visual precedence, but activity/failure remains visible independently;
resuming does not invent a successful retry or clear the producer diagnostic.

Actions are opt-in **affordances**, with Resume alone enabled by default.
Register ordinary Nyx event callbacks for `LParts.RetryID`, `ResetID` and
`ResumeID`. The application owns the actual retry, scoped query command or resume
authority. The library never clears all filters, resumes implicitly or reloads
data merely because an action is visible. `Actions([ncfaRetry, ncfaResume])` can
withdraw Reset after this surface's contribution is cleared, even when a peer
filter still hides records. This availability changes atomically with its labels
and count state. The service demo uses a live query owner, reacquiring only when
an explicit legacy query replacement retired its former scope.

Heading/action text is freely configurable with
`TNyxCollectionFeedbackText.Defaults.WithHeading(...)` and the typed button-text
builders. Rebind named parts to resource or computed expressions for other
presentations/localization. Typed `SourceRows`, `MatchedRows` and `PageRows` state
references are updated in the same scalar publication as heading, detail, busy
and action visibility. The default detail is an English count summary. Labels
and the card expose current accessible names; indeterminate progress does not
claim a percentage. Actual screen-reader/live-announcement qualification remains
open; a name assertion is not that evidence.

The binding uses the current application's context/generation and existing weak
view subscriptions. Close it on UI before application disposal. No widget,
renderer, router or document is retained. A closed binding cannot publish again;
retained observations stay inert and readable. Validation refusal leaves both
scalar state and cached facts unchanged. A receiver failure after installation
reports committed state without rolling metadata back. Reentrant view changes
during validation refuse mixed facts before installation; `Refresh` reconciles
current facts afterward. Caller-supplied diagnostics retain exact Unicode/NUL/
line endings in state/observation; physical rendering of control characters is
still target-dependent.

Producer results must separately follow the existing scheduler, cancellation,
generation and expected-revision rules. This feedback binding does not make a
stale worker result safe to install. The deterministic nine-step example simulates
activity without a timer/network service and performs real collection mutations
for empty data and row churn. Retry submits immutable rows using the existing
LINQ/scheduler pipeline, with native workers or the browser event loop. The public
demo's native handwritten/generated controls exercise stale-revision refusal,
supersession, cancellation and close-before-delivery; actual worker retirement
is observed after widget disposal. Complete results use ordinary collection
`Assign` replacement semantics, rather than an inferred stable-ID merge.
Tree-specific
match/context counts, saved feedback policy/Studio/MCP adoption and browser/
visual/full live-inspection acceptance remain open. Native handwritten/generated
applications exercise all seven feedback states and independent filter ownership
at desktop/compact widths; browser consumers compile only.

## Revealing a row or field deliberately

Selection and scrolling are separate intentions. A source update should not
move someone who is reading older records. Use the mounted control's optional
viewport port when a command such as Find, Go to record or Show selected should
bring a particular stable identity into view. Add `nyx.collections.viewport`:

```pascal
var
  LViewport: INyxCollectionViewport;
  LReveal: INyxCollectionReveal;
begin
  LViewport := NyxCollectionViewport(LTableMount);

  if LViewport <> nil then
  begin
    LReveal := LViewport.Reveal(
      NyxRevealItem(LService)
        .InField(NyxTextField('notes'))
        .Vertical(ncvaNearest)
        .Focus(ncvfPreserve));
  end;
end;
```

The defaults are nearest alignment and preserved focus. Omitting `InField`
requests vertical row reveal only. Field references retain their scalar family;
an Integer field cannot impersonate a Text field with the same name. Both axes
also accept `ncvaStart`, `ncvaCenter` and `ncvaEnd`. Targets may clamp at their
physical row/column or scrolling boundaries. Oversized content can complete with
`Capture.FullyVisible = False`; this is distinct from an unavailable viewport.

The default keeps the exact row-selection membership, canonical cursor, active
editor, unfinished text and caret. `.Focus(ncvfTake)` deliberately moves the
cursor and keyboard focus to the revealed cell without replacing membership or
entering edit mode. These are UI-owner operations, including receipt reads and
cancellation. Marshal worker results to the UI scheduler before requesting a
reveal; the port does not make native widgets or live views thread safe.

`Reveal` returns a small independent receipt. `Pending` may remain true while a
browser measures virtual rows or a native control finishes an existing guarded
layout turn. Read `Capture` on a later UI turn to distinguish `ncvsCompleted`,
`ncvsRefused`, `ncvsCancelled` and `ncvsFailed`, with a typed reason. `Cancel`
withdraws a pending request. Terminal observations never change, including after
late cancellation. A new request supersedes the previous pending receipt.
Changed displayed data, input authority, selection or explicit viewport movement
can invalidate unfinished work; it must never redirect an old request. A paused
view may reveal its retained displayed frame while its producer continues.

Only already displayed rows and visible fields qualify. Reveal never changes a
filter, page, sort, hidden column or paused mode to manufacture a match. It
scrolls this control's available data area, excluding headers and opaque pinned
panes, and does not scroll containing pages or open hidden parents. Unavailable
or wholly clipped targets refuse. Completion describes software geometry, not
physical display timing, screen-reader quality or other-window occlusion.

The owned browser and LCL flat List/Table mounts expose this optional facet.
Alternative mounts can return nil; the original mount interface and GUID remain
unchanged. Tree retains its hierarchy-aware reveal contract. A viewport port
borrows its mount only during entry and is revoked before target disposal.
Retaining the port or receipt cannot retain a renderer, widget, view or document.
After retirement, an old port is inactive and returns a disconnected refusal.
Do not retain `INyxCollectionViewportAccess` as a substitute for the independent
port: that acquisition interface is still the mount owner.

## Following the last visible row

Receiving source updates and following a viewport are separate choices.
`NyxInspectionPolicy` defaults to `ncfKeepPosition`. To supply a different initial
intent, configure `NyxInspectionPolicy.Follow(ncfLastVisible)`. Runtime commands
use an optional specialized interface from `nyx.collections.view`:

```pascal
LFollowing := NyxCollectionFollowing(LView);

if LFollowing <> nil then
begin
  LFollowing.FollowLatest;
end;
```

Declare `LFollowing: INyxCollectionFollowing`. `FollowLatest` immediately requests
the current last row, including when no source revision changed. Repeating the
command deliberately reveals it again. Later source publications follow only
when the last displayed row identity changes. Updating an unrelated value does
not repeatedly pull a reader away. An exact removal receipt also recognizes a
replacement which reuses that last row's ID and values. Missing, skipped or
overflowed evidence never invents a new incarnation from an equal ID; an explicit
Follow request remains available. The default retains the reading anchor through
those same publications. `KeepReading` stops following and preserves that place.
Neither command selects a row, changes source values, modifies query/page choices,
rewrites authored policy defaults or changes an independent peer's follow mode.

"Last" means the final row of the current filtered, sorted, paged projection.
It does not assume insertion time or a timestamp field. Authors choose an
appropriate sort for a log or monitor. Local query/page changes retain ordinary
anchor recovery; they are not treated as incoming source publications. Use
`FollowLatest` again to reveal the new current tail deliberately. Physical targets
may clamp near the end of their viewport; the last row need not become its first.

Following remains enabled until `KeepReading` or an explicit policy change.
Manually scrolling elsewhere does not change that mode: unrelated value updates
preserve the new anchor, but another changed tail will follow. The demo exposes
both choices visibly so users can stop movement before inspecting older rows.
Automatic suspension on user scroll is not implemented by this contract.

Pause retains the viewport and incoming updates continue in the authoritative
store. `FollowLatest` refuses while paused; it never resumes implicitly. The demo
disables that button through an ordinary Nyx Boolean binding. `KeepReading` can
stop following while paused, and resume honors that choice. An explicit follow
request is acknowledged only after successful target refresh; failure/defer keeps
it available for retry. Observation reports runtime `FollowMode` and a monotonic
`FollowRequestSerial`, separately from update-delivery `Mode`. These describe
intent; a failed/deferred physical target has not necessarily reached the proposal.

Current owned flat List/Table views support this optional runtime capability;
configure inspection first. Tree physical following refuses explicitly. Existing
view/inspection method lists and GUIDs remain unchanged. Their policy/observation
value records gain follow fields, so rebuild dependent compiled units. The public
demo's handwritten
and generated native applications exercise arranged/pinned Tables at both widths;
visible native List/plain-grid cases and model failure/retry tests cover those
paths. Browser consumers compile only. Browser execution, full visual/input
parity and saved-policy/Studio/source/MCP adoption remain open.

## Recovering unfinished cell drafts

Mounted editable Tables preserve incompatible unfinished text before replacing
their physical editor. A changed accepted value, removed row, same-ID replacement,
hidden row, unavailable field or unverified identity transition withdraws the old
editing authority. Recovery never silently applies that text to the live source.
Unrelated updates retain a compatible draft. Clearing a filter does not restore
an old draft over current data. A paused view keeps its displayed editor until
deliberate resume; the transition then uses the same removal-history evidence.

Import `nyx.collections.recovery` and `nyx.collections.mount`:

```pascal
LRecovery := NyxCollectionDraftRecovery(LMount);

if (LRecovery <> nil) and (LRecovery.Capture.Count > 0) then
begin
  LEntry := LRecovery.EntryAt(0);
  LText := LEntry.Cell.Text;
end;
```

Declare `LRecovery: INyxCollectionDraftRecovery`,
`LEntry: INyxRecoveredCollectionDraft` and `LText: TNyxText`. Alternative mounts
may return nil. The independent data port owns no view, source snapshot,
renderer, widget, event router or callback receiver. Read/capture it on its UI
owner. Immutable entries may outlive both journal and mount. Each entry retains
exact text, scalar caret range, typed field, accepted scalar, loss reason and
before/after source revisions. Its copied cell has no edit/focus authority.
Supplementary Unicode, NUL and line endings remain exact in the journal; a UI
may escape NUL for presentation without changing those facts.

`Capture` returns count/revision/lifetime and text-byte observations; `EntryAt`
reads one entry instead of dumping every draft. `Discard(LEntry)` requires the
exact entry interface, so a stale index cannot discard a different user's work.
Applications should warn before discarding. Retired journals remain readable
and explicitly discardable; retirement clears `Active` before target teardown
and refuses further capture. Retaining one entry does not retain its writer.

Capture owners may use the optional `INyxCollectionDraftRecoveryChanges` facet
on their writer. `Subscribe` accepts a typed `TNyxDraftRecoveryObserver` and
returns an ordinary `INyxCollectionSubscription`. Receivers are borrowed: keep
and disconnect their token before freeing them. Last-token release disconnects;
writer retirement silently detaches all receivers and the journal's borrowed
notification channel. Retained readers cannot acquire this writer capability,
and entries/snapshots contain no callbacks.

An admitted insertion or explicit discard sends one bounded committed membership
observation to every remaining live receiver. Reads, limits/warning changes and
coalesced retries do not announce another membership change. Journal mutation
and new subscriptions refuse inside notification; disconnect/retirement remain
safe. A failing receiver does not skip siblings; a Pascal failure reports
`ENyxCollectionNotification` after commit. Browser foreign thrown values retain
their original identity. Ordinary table mounts own one token and update only
their count presentation. Native busy input defers that work through its existing
idle boundary, without rebuilding rows or changing accepted input ownership.

`nyx.collections.recovery.controls` provides a reusable detached review card:

```pascal
LReview := NewNyxRecoveredDraftReview('recovered-service-edit', LEntry);
LPage.Add(LReview);
```

Declare `LReview: INyxRecoveredDraftReview`. The card uses specialized Heading,
Label and read-only Memo controls with named parts. It copies the entry's typed
cell, reason and source revisions; its `Cell` property returns exact raw text and
scalar caret with inactive editing/focus flags. The card remains usable after
the original journal is retired or its entry is explicitly discarded. Both
previews visibly escape control characters and backslashes; preview selection
therefore copies that displayed representation. An authorized application can
use the separate raw `Cell.Text` for exact transfer through its existing host.
This compound grants no clipboard authority and performs no restore or discard.

Renderer-managed tables expose **Recovered edits (N)** beside Columns when the
journal contains entries, including an empty filtered result. The panel presents
one immutable entry at a time. Previous/Next retain the captured journal revision;
if another caller changes the journal, navigation refuses visibly until Reload.
The current detached preview remains the same entry. Review never commits,
restores or discards a draft. The optional `INyxCollectionRecoveryReviewMount`
facet exposes `RecoveredEditCount` and `OpenRecoveredEdits`; alternative mounts
may lack it or return false when presentation/input is unavailable.

Ordinary tables without a clipboard policy receive read-only review. Copy needs
the existing cell-copy policy and an explicitly configured clipboard host. Its
unavailable/refused result remains visible. Preview escaping is separate from
the exact raw transfer, which is prepared lazily from the frozen review through
`NewNyxCollectionRecoveredDraftText`. Existing copy byte limits, original input
epoch, successful-popover lifetime and write-slot cancellation remain in force.
No additional resource, file or clipboard authority comes from opening review.
Automatic recovery uses the optional `INyxCollectionCellActionDefaultsMount`.
It does not consume the caller's one explicit presenter assignment. The established
Render, ConfigureClipboard, ConfigureExport, ConfigureCellActions sequence remains
valid. First explicit services rebuild the default against those exact controllers;
first explicit presentation replaces it. Duplicate explicit assignments still
refuse. Candidate failure leaves the previous default, journal and live input
intact. Each controller has a revocable lease on the target factory, while the
mount owns the underlying anchor; old callbacks cannot reach a replacement.
Cleanup after successful adoption is diagnostic and cannot roll back that new
owner. Alternative mounts may omit the defaults facet without losing their
legacy explicit setup contract.
Scoped saved-application and target qualification is recorded in
[recovery review integration](../TODO/DONE/NS-3_collection-recovery_01.md) and
[action adoption](../TODO/DONE/NS-3_collection-action-adoption_01.md).
Full workbench, physical/assistive input and production performance remain open.
The maintained `nyx_table_recovery_application_controls` native consumer compiles
beside the unchanged `nyx.generated.view` unit from Studio's Service monitor job.
It uses the real pinned/compact grid faces and managed popup, retaining the exact
authored design and source data. Public query changes isolate recovery from the
separate physical filter/pointer gate; they do not qualify hardware input.

`ConfigureWarnings(NyxDraftRecoveryWarnings.Entries(64).Bytes(8 * 1024 * 1024))`
configures advisory thresholds; these are also the defaults. Zero disables the
corresponding warning. Warnings never truncate, expire or discard text. Byte
accounting covers UTF-8 draft text only and saturates explicitly. Independent
hard admission limits now default to **256 entries and 16 MiB logical UTF-8
payload**, including draft text, accepted text and collection/item/field names.
Entry count separately bounds fixed record/interface overhead. These are limits
on journal ownership, excluding active editors, host allocations and discarded
entries independently retained by callers. They are not a process-wide memory
or measured performance guarantee. Paused-source bounds remain separate.

Saved defaults use the typed policy from `nyx.collections.recovery.policy`:

```pascal
LSpec := LSpec.DraftRecovery(NyxDraftRecoveryPolicy
  .Entries(128)
  .PayloadBytes(16 * 1024 * 1024)
  .WarnEntries(64)
  .WarnTextBytes(8 * 1024 * 1024));
```

`UseDefaultDraftRecovery` removes the authored override. Without one, the
library uses the defaults above and preserves older descriptor bytes. A declared
policy uses descriptor version14, including nullable inspection settings; it
contains only scalar limits and never a runtime journal or recovered text.
All four values are nonnegative integers. Zero entry/payload limits prevent new
storage; zero warning values disable only that advisory threshold. Warnings
measure draft text, while the hard payload limit includes the other text named
above. Each newly mounted view initializes its own journal from this policy.
Refresh does not reapply defaults over later explicit runtime configuration.

Studio exposes a collapsed **Draft recovery** section beside collection
inspection settings. Apply and Restore defaults use the same paired
design/Pascal history as other collection edits. Bounded semantic binding reads
expose the declared policy; grouped `draft-recovery` edits accept a policy or
null for reset with the existing revision and baseline checks. These operations
do not inspect or change a running application's drafts or journal limits.

```pascal
LBudget := NyxCollectionDraftRecoveryBudget(LRecovery);

if LBudget <> nil then
begin
  LBudget.ConfigureLimits(NyxDraftRecoveryLimits
    .Entries(128)
    .PayloadBytes(8 * 1024 * 1024));
end;
```

Declare `LBudget: INyxCollectionDraftRecoveryBudget`. Both limits accept zero;
zero prevents new storage. Lowering below current journal ownership refuses
atomically. No policy evicts, truncates or overwrites already retained entries.
Exact duplicate retries coalesce even at capacity. `CaptureBudget` returns only
scalar limits/counts/payload/refusal facts; it retains no rejected draft or host.
`RequiredPayloadBytes` describes the refused entry, with explicit saturation.
`CanRetry` checks current capacity, independently of physical display readiness.
Mounted retries use an opaque recovery-frame identity, so partial admission
cannot store the same original draft again when later source activity changes
its loss classification. The first receipt remains immutable. Another installed
frame stays independent even at the same data revision with identical text.
The marker retains no snapshot, widget, source or callback. Custom capture owners
may use the optional `INyxCollectionDraftRecoveryScopedWriter` and
`NewNyxDraftRecoveryFrame`; unscoped writer behavior remains compatible.

A direct writer's `Preserve` raises `ENyxDraftRecoveryPressure` before taking
ownership when a new entry cannot fit. The caller must keep its original dirty
cell. Ordinary mounts handle that refusal by holding the previous physical
display and shielding its stale input. Committed source updates and peers keep
advancing. Native arranged editors preflight capture before closing their actual
editor; plain-grid and browser recovery also precede dropping their dirty value.
Review/copy retained entries, discard an exact entry deliberately or raise limits,
then call the same mount's `Refresh`. Successful capture precedes display adoption
and input release. Free capacity alone does not claim a completed refresh.
`NyxCollectionDraftRecoveryHeld(LMount)` reports the current physical hold;
the journal's `LastRefusal` is a historical capture receipt. For example,
deliberately reverting the dirty text can let a fresh frame install without
needing another journal entry. Native/browser deferred refresh queues retain
their pending work but do not busy-retry a quota-held display. Source updates
and explicit refresh remain able to attempt admission.
There is no spill service or hidden queue of rejected text. Explicit retirement
releases physical drafts normally; preserve/export unresolved work before disposal.

The public live-inspection example composes a bound read-only recovery memo,
previous/next navigation, an explanation, exact-entry discard confirmation and
an explicit **Retry display** action after capacity pressure
from ordinary Nyx controls. Review changes runtime state only. Current native
checks exercise real arranged Table editors through handwritten/generated apps
at desktop/compact widths; pure and model-mounted fixtures cover exact text,
retirement, capacity refusal/retry and failed-refresh input refusal. Browser Table/Tree recovery paths
compile but still require actual browser execution. Native Tree inline-editor
recovery and full parity remain open. Saved policy/source/Studio/semantic checks
execute natively, including actual handwritten/generated mount initialization;
ordinary observed Studio interaction remains a separate gate. A compilation
pass does not qualify those interactions.
An oversized **pending** draft is different from an already retained entry.
`INyxCollectionPendingDraftMount.CapturePendingDraft` explicitly reads the exact
refused physical draft without hiding it or admitting another journal entry.
It returns an inert `INyxPendingCollectionDraft`: copied text, typed cell and
Unicode scalar caret, plus an opaque physical-frame identity. The mount retains
coordinates only; normal producer updates never queue rejected text. A caller
may retain this review independently after the target retires.

`DiscardPendingDraft(LReview)` requires that original live frame and the exact
reviewed text/caret/accepted scalar. Focus may move to the review controls.
Newer typing, a different frame, retirement or active composition refuses. A
successful discard changes only that physical draft; it never writes source or
discards journal entries. Call the existing mount's `Refresh` afterward to
install the latest source display. If another dirty cell also exceeds capacity,
that cell remains held and can be reviewed separately. Applications must obtain
deliberate confirmation before invoking this destructive command.

The example's **Review held draft** button exposes the pending value in its
ordinary read-only memo. **Export reviewed draft** uses an explicitly injected
`INyxFileSaveHost`, exact UTF-8 bytes (including NUL), cancellation and typed
delivery outcomes. Export never consumes the original or silently reapplies it.
**Discard pending draft** requires two clicks; if text/caret changed, a new
review and confirmation are required. Saved-entry navigation/discard remains a
separate action. These are runtime controls, not authored history.

The original byte-only file-save contract still accepts at most 4 MiB. The same
LCL/browser hosts now expose optional `INyxFileSaveStreamHost`. The example
prefers that capability, using `NyxUTF8ByteSource(LReview.Cell.Text)` rather than
constructing another complete encoded array. The immutable source validates
Unicode once and opens independent cursors; chunks may split UTF-8 sequences,
but their concatenation preserves exact bytes, supplementary scalars and NUL.

`NyxFileSaveStreamOptions(LFileOptions)` defaults to 64 MiB total output, 64 KiB
chunks and a 64 MiB browser-download buffer. Fluent `MaximumBytes`, `ChunkBytes`
and `DownloadBufferBytes` tune those independently. The total limit is checked
before opening a cursor. Every read is bounded and exact declared EOF precedes
commit. A short source, extra bytes or an oversized chunk fails without claiming
delivery. Changing runtime policy never widens an already admitted request.
`TNyxLiveInspectionDemo.ConfigureDraftExport` configures the next export action.

Native writes use cooperative Nyx scheduler turns and an exclusively created
temporary sibling. Only exact complete content replaces the selected file;
cancellation/failure closes and removes owned staging output. Picker browsers
await each chunk write and close only after exact EOF, aborting uncommitted work
when possible. Browser Blob downloads inherently buffer the whole output within
the separately chosen download limit. This bounds logical content, not browser
engine allocation/peak memory. Zero download buffer permits empty downloads and
does not disable incremental picker output. A download remains a request, not a
disk-completion guarantee; browser activation/security rules still apply.

Byte-only alternative hosts remain supported for smaller values. Larger values
refuse explicitly when that host lacks streaming, leaving both reviewed and
original text intact. Streaming policy is runtime authority and does not change
the existing saved byte-file schema. Complete pressure UX, executed browser
behavior and total application memory acceptance remain open; bounded chunks
and native file checks must not be presented as those broader outcomes.

## Typed construction

```pascal
uses
  nyx.collections,
  nyx.collections.view.types,
  nyx.collections.view,
  nyx.collections.mount;

var
  LTasksView: INyxCollectionView;
  LTasksMount: INyxCollectionMount;
begin
  { The application owns an independent runtime registry. The view retains its
    store; the renderer owns the attachment to the mounted NyxTable control. }
  LTasksView := NewNyxCollectionView(
    LApplication.Collections.Collection(NyxCollection('tasks')),
    NyxCollectionView(NyxCollection('tasks'))
      .Column(NyxTextField('title'), 'Task', cmEditable)
      .Column(NyxBooleanField('complete'), 'Complete', cmEditable)
      .Column(NyxIntegerField('priority'), 'Priority'),
    cpTable);
  LTasksMount := LApplication.View.BindCollection('tasks-table', LTasksView);
end;
```

`NyxCollectionView` returns an immutable fluent specification. Its four `Column`
overloads require the matching text, Boolean, integer or number field reference.
Completed specifications contain 1–64 distinct columns. The collection key and
field families must exactly match the admitted store schema. Closed choices use
`TNyxCollectionScope`, `TNyxCollectionProjection` and `TNyxCollectionCellMode`.

`ToData`/`FromData` preserve the canonical version-1 specification for single
selection, with key, scope, optional parent field and ordered columns. Explicit
multiple selection uses version 2 and its closed `selection` member. The document
envelope remains version 3. Nonempty authored query defaults use view descriptor
version 3; see [typed queries](collection-queries.md) for filtering, stable ordering,
hidden membership, independent runtime policy and current integration limits.
A null packet means absent;
unknown fields, choices, versions and duplicate columns reject. Design version 3
reserves `collectionView` for this packet on a node. Version 1/2 fields with that
name remain opaque extension data; a collision rejects promotion to version 3.

Explicit column sizing uses view descriptor version 5. Its optional per-column
`sizing` member carries a closed version-one policy; query and type-ahead remain
independent nullable members. Bindings without explicit sizing retain their exact
version 1–4 bytes. Resetting the last explicit policy restores the corresponding
legacy descriptor. The document envelope stays version 3.

Lists display the first column and expose stable item selection. Tables display
all declared columns. Trees display the first column and require a text parent
field supplied through `.Parent(NyxTextField('parent'))`. Table/tree cells marked
`cmEditable` can commit edits; read-only cells reject writes. List editing can
use another form or the typed view API.

## Collection pages and reusable pagers

Import `nyx.collections.paging` to select positive Integer page defaults. Paging
follows the complete filter/order result; it never rewrites the collection store.
Each runtime view owns its current page independently, including reusable instances.

```pascal
LTasksTable := NewNyxTable('tasks-table');
LTasksTable.Binds
  .Collection(
    NyxCollectionView(NyxCollection('tasks'))
      .Column(NyxTextField('title'), 'Task', cmEditable)
      .Paged(NyxCollectionPaging.PageSize(25)))
  .Done;
LPage.Add(LTasksTable);

LTasksPager := NewNyxPagination('tasks-pager');
LTasksPager.Configure
  .PageView(NyxControl('tasks-table'))
  .Done;
LPage.Add(LTasksPager);
```

The variables retain `INyxTable` and `INyxPagination`. `.InitialPage(2)` selects an
authored starting page; the default is one. Actual membership clamps a requested
page to the last available page. An empty result has page one, zero pages and no
navigation authority. Filtering or store changes update counts and clamp the
current page atomically. Invalid/reentrant commands preserve the prior view.

`NyxCollectionPages(LRuntimeView)` returns the optional managed page capability
without changing `INyxCollectionView`'s existing interface. It exposes the copied,
read-only `Window` (Page, Pages, First, Count, Total), `ConfigurePaging`, `GoToPage`,
`FirstPage`, `LastPage`, `PreviousPage` and `NextPage`. Equal page commands retain
snapshot identity and notify nobody. `SelectAll` covers the complete filtered
result while focus stays on the current page; selected hidden items remain owned.
The retained page snapshot accounts for the complete source's `DataBytes`.

Both renderers automatically connect `PageView` to the existing reusable
Previous/Page/Total/Next parts. Their count, input bounds and boundary buttons
follow the view; empty results show “No results.” Named parts remain accessible
through the specialized pagination interface for styling and added actions.
Caller configuration, live Boolean state bindings and viewport overrides on
Previous/Next/Page remain effective during navigation. Availability adds a
restriction; it never writes over those values. An allowing state cannot enable
Next at the last page or Previous at the first page.
Missing/retyped required parts refuse. The portable control reference resolves
under the same reusable expansion prefix; a missing local target cannot select
another instance or an outer page. Target admission precedes adapter publication.

`INyxCollectionPager` is a retained runtime attachment in `nyx.collections.pager`.
Renderers expose it through `CollectionPager`; unmount disconnects its weak view
subscription and borrowed refresh receiver before freeing controls. Retained
retired attachments refuse commands. Default navigation runs on the UI thread
before application callbacks, independently of their chosen execution policy.
No callback registration or retention cycle is added to the event router.

Component authors can import `nyx.interaction.policy` and implement the managed,
read-only `INyxRuntimeEnabledPolicy`. A realized node retains attached policies;
all must allow interaction alongside its current Enabled value. Attach/detach,
inspection and synchronous policy reads belong to the UI thread. Policies cannot
mutate, call back, or retain a node, document or renderer. Ordinary descriptor
clones exclude them; independent runtime projections retain their live
capabilities. The owner must revoke retired capabilities safely even when a
projection retains a policy. Neither persistence nor generated Pascal acquires
runtime-policy keys. The pager replaces only its own constraints on a reconfigured
runtime copy, preserving unrelated extension policies.

`ConfigurePaging(Default(TNyxCollectionPaging))` temporarily shows all filtered
results through the runtime capability. Its attachment remains valid and disables
navigation; remounts retain that runtime policy and the live page independently
of authored defaults. Page bounds/value and Total text are the pager's runtime
presentation. Arbitrary bindings to those attributes are not qualified by the
Enabled contract.

Explicit paging selects view descriptor version six, with a strict version-one
`paging` object and nullable query/type-ahead members. Column sizing remains
optional. `.UseDefaultPaging` restores the exact corresponding version 1–5
descriptor. Generation uses the typed factories/references above; raw strings
remain at descriptor/extension boundaries. Flat paging supports list/table views;
trees explicitly refuse it because a sliced interval can detach ancestors.

Studio's Bindings panel supplies Items per page, Initial page and Show all results
through ordinary typed queued commands. MCP uses the existing collection tool's
`page-size`, `initial-page` and `default-paging` intents, with JSON Integers,
expected revisions and one paired transaction for related edits. A referenced
pager must be disconnected in the same ordered transaction before its authored
target is unpaged or deleted. This design-admission rule is distinct from the
runtime reset above. Create/bind the target before assigning its pager reference.

The maintained companion uses `tools/build.ps1 -Target grid-navigation
-CollectionPagination -GridSourceDirectory <exact-MCP-export>`; add a new
`-GridEvidenceDirectory` to preserve prior evidence. Native controls execute;
browser hosts and the trusted-input consumer are staged for explicit HTTP use.
The companion tool's `--customize-pager` mode discovers bounded semantic named
parts and applies their typed state bindings as one paired Undo/Redo command.
Its new `customized` export can be passed to the same maintained consumer.
Current-source evidence and remaining visual/accessibility/performance/widgetset
limits are in [WORK.md](../WORK.md#current-return-path-live-reusable-pager-customization--2026-10-12).

## Typed table column sizing

Import `nyx.collections.columns` and configure the column just declared. Ordinary
authoring and emitted Pascal use integer dimensions, closed modes and an immutable
fluent policy, rather than property strings:

```pascal
LTable := NewNyxTable('project-table').Binds.Collection(
  NyxCollectionView(NyxCollection('project-ideas'))
    .Column(NyxTextField('idea'), 'Idea', cmEditable)
      .ColumnSizing(NyxColumnSizing.Flex(3).Minimum(120))
    .Column(NyxIntegerField('priority'), 'Priority', cmEditable)
      .ColumnSizing(NyxColumnSizing.Fixed(96))
    .Column(NyxTextField('status'), 'Status')
      .ColumnSizing(NyxColumnSizing.Flex(1).Minimum(80))
).Done;
```

`Content` (the factory default) distributes available width in proportion to
host-measured natural widths, retaining each bounded natural minimum. `Fixed`
requests logical pixels, clamped by its bounds. `Flex` assigns a positive integer
weight to the remaining constrained space. `Minimum` and `Maximum` apply to every
mode; positive extents and weights are bounded by `MaximumNyxLayoutBound`.
An inverted range refuses without changing an existing value. Insufficient space
overflows horizontally rather than silently dropping positive column minima.
`UseDefaultColumnSizing` clears the last declared column's explicit policy.

Both adapters reuse the shared `NyxFlowSizes` allocator. Browser measurement uses
headers and a bounded first-row sample; cached metrics avoid measuring the full
virtualized dataset on each resize or edit. Native measurement retains its initial
grid metrics. Font metrics can differ between hosts: shared allocation semantics
do not promise pixel-identical content measurements. An unrelated publication or
host resize preserves the active cell draft. Native user column widths remain
runtime overrides across publication/resize, independently of authored defaults.

Studio's table Bindings panel exposes content, fixed/flexible sizing, bounds and
default reset. Its ordinary queued controls preserve exact typed field identity,
neighboring policies, unfinished Pascal and paired history. At the MCP boundary,
`nyx_collections` exposes table-only `column-content`, `column-fixed`, `column-flex`,
`column-minimum`, `column-maximum` and `column-default` intents. Numeric values are
JSON Integers. Related intents can be applied as one revision-guarded paired Undo
command; an invalid intermediate rejects the complete group. These additions are
qualified on an isolated current-source host; the observing LAN release has not
yet received them.

Win32/browser contract/source, real Studio input, compiled semantic companions,
resize and selective visual evidence are in
[WORK.md](../WORK.md#current-return-path-typed-table-column-sizing--2026-10-11).
Complete visual parity, native DPI/widgetset coverage, accessibility, production
performance and browser column-resize handles retain their original task owners.

## Typed column display formatting

Import `nyx.formatting` to configure the display of the column just declared.
Accepted values retain their original scalar family and canonical spelling:

```pascal
LTable := NewNyxTable('services-table').Binds.Collection(
  NyxCollectionView(NyxCollection('services'))
    .Column(NyxTextField('Service'), 'Service', cmEditable)
      .ColumnFormat(NyxValueFormat.TextAffixes('Service: ', ''))
    .Column(NyxNumberField('Memory'), 'Memory', cmEditable)
      .ColumnFormat(NyxValueFormat.Decimal(2)
        .Identified(NyxFormatter('memory-display'), 1)
        .Localized(NyxFormatLocale.Named(NyxLocale('en-US'))
          .NumberSeparators('.', ',')))
    .Column(NyxBooleanField('Running'), 'Status')
      .ColumnFormat(NyxValueFormat.BooleanLabels('Running', 'Stopped'))
).Done;
```

`NyxValueFormat` starts with canonical display. Its immutable policies include
decimal scale/rounding, calendar date order/separator, clock precision, Boolean
labels and text prefixes/suffixes. Calendar and clock presentation require their
actual typed field domains; numeric-looking text is never interpreted as a
number. Decimal formatting uses Nyx's exact decimal domain and explicit rounding.
It refuses values outside that domain rather than falling back to binary
formatting. Reducing clock precision also refuses nonzero discarded parts.

Locale names and formatter identities record provenance alongside copied facts
and a revision. The locale is explicit: it does not query the machine locale or
resolve CLDR data. Caller-supplied separators survive save/load and generated
Pascal unchanged. `.UseDefaultColumnFormat` withdraws the saved override;
`.ColumnFormat(NyxValueFormat)` saves an explicit canonical policy. That presence
distinction survives reusable inheritance, local masks and paired Undo/Redo.
Existing descriptors without formatting retain their original representation;
explicit formatting uses the additive version-nine column descriptor.

Browser labels, native painting and detached Fit samples share the presentation
policy. Native editors still receive canonical text. A valid accepted value that
cannot be displayed under the selected policy remains usable: paint exposes an
explicit unavailable marker and a separate diagnostic. `Present` returns these
facts with exact `RawText`; strict `FormatValue` refuses. Inspection, comparison,
editing and the existing RAW copy/export commands never receive the marker or a
formatted replacement for accepted data. A separate managed display-copy action
transfers explicitly captured presentation text as described below. Explicit
focused-draft capture remains a separate integration requirement.

Studio's column forms expose the same typed choices through the existing
isolated edit queue. Semantic collection reads request `formatControls` only when
needed; the bounded response distinguishes declared policy from effective default.
Column-format mutations use the shared revision-aware command path. Generated
Pascal and source reading preserve every style, explicit locale and identity.
Current pure, target-control and generated-application qualification is recorded
in [WORK.md](../WORK.md); full workbench acceptance and installed Studio delivery
remain separate.

## Table text and row spacing

`nyx.collections.presentation` separates text layout and body-row spacing from
typed values, formatting and column geometry. Author the choices on the same
collection specification:

```pascal
LServicesTable.Binds.Collection(
  NyxCollectionView(NyxCollection('services'))
    .Column(NyxTextField('command'), 'Command line')
      .ColumnText(NyxColumnText.Wrap(4))
    .Column(NyxTextField('status'), 'Status')
      .ColumnText(NyxColumnText.Ellipsis)
    .TablePresentation(NyxTablePresentation.Density(ntrdComfortable)
      .Header(NyxTableHeader.Lines(1).MinimumHeight(32))));
```

`Clip`, `Ellipsis` and `Wrap` affect presentation only. Wrap honors hard breaks
and long unspaced paths; its positive line count bounds the intended height.
There is no arbitrary authoring ceiling below `High(Integer)`. Runtime hosts
have separate, adjustable work limits with typed refusal outcomes. An accepted
saved policy does not promise that every host can admit its requested work.
Raw values, unfinished drafts and explicitly represented copy remain distinct.

`NyxTableHeader` independently configures caption lines and a minimum logical
height. The explicit factory reserves one natural caption line and no additional
minimum. `Lines` accepts positive values and `MinimumHeight` accepts zero or
positive values within Nyx's geometry domain. Targets scale the logical minimum
once and retain their own space for interactive filtering, sorting and column
management. Body-density changes, including runtime preferences, preserve the
header choice. Native and browser font metrics can produce different pixel sizes.

Omit `.Header` to preserve legacy target headers. `.UseDefaultHeader` removes
only the header choice, retaining body density. Existing table-presentation wire
objects remain version one byte-for-byte when no header is present; an explicit
header adds nested version two. Generated Pascal uses the same typed fluent
construction, and source reading retains it.

Absence preserves the former host behavior. Explicit Clip and Normal density
remain present choices. `UseDefaultColumnText` and
`UseDefaultTablePresentation` remove their respective authored choices without
clearing formatting, workbench permissions, query or layout defaults. The saved
descriptor uses version11 only when a text/density policy is present; resetting
both reproduces the earlier descriptor and generated Pascal exactly.

The optional `INyxCollectionTablePresentation` facet prepares runtime preferences
through the existing column publication owner. Density and field/family keyed
text overrides reset independently; applications persist those preferences
separately from designs and collection data. Captures retain copied values and
inert identity proofs, so they do not keep a view or its store alive. Stale,
foreign or competing captures refuse before publication. Existing view
interfaces retain their signatures.

Runtime preferences can activate presentation on a table without authored
presentation defaults. Reset removes that runtime override without inventing a
saved declaration. Native/browser activation and reset have focused control
checks; full Studio integration remains tracked in the workbench task.

Studio's **Table defaults** includes collapsed **Table presentation** and per-column
**Text layout** sections. Apply captures one typed policy through the ordinary
source queue; each reset clears only its own declaration. A partial wrap count
stays an unsubmitted draft. Clip, Ellipsis and reset ignore that inactive count,
while Wrap requires a positive Pascal integer. These forms share the existing
layout/display draft owner, exact schema/binding baseline and paired Undo/Redo.

The semantic `nyx_collections` tool accepts `presentationControls: true` on
bounded binding and exact column reads. Requested table and column policies
report `{declared, policy}` independently of `presentationControlsSupported`.
An absent presentation policy stays `null`: it preserves legacy host behavior,
rather than claiming one universal resolved font or density. Omitted/false
retains the earlier response shape. No live cell text, user preference or
measurement state is read by this selector.

Use `NyxSetCollectionTablePresentation` / `NyxUseDefaultCollectionTablePresentation`
and `NyxSetCollectionColumnText` / `NyxUseDefaultCollectionColumnText` in a typed
patch. Their semantic operations are `table-presentation` and `column-text`,
with a complete policy or `null` reset; column edits retain the exact field and
scalar family. Ordered groups derive the current binding baseline after each
preceding change. Revisions, operation retries, atomic refusal and paired history
use the same admission path as ordinary authoring. Resets retain local ownership;
restoring reusable inheritance is a separate explicit operation.

Value ownership and source reading/generation have executed native/browser
evidence, including independent compilation of the emitted application. Actual
target checks exercise painting and Fit. Portable Studio presentation
capture, paired history and semantic version11 discovery have separate checks;
visual qualification, complete arrangement absence, mounted Inspector input and
full Studio/native performance remain open under
[the table workbench task](../TODO/NS-3_control-usefulness_03.md). This source
contract alone does not establish those interaction or parity outcomes.

## Logical column alignment

Import `nyx.collections.alignment` to set either alignment axis independently:

```pascal
LServicesTable.Binds.Collection(
  NyxCollectionView(NyxCollection('services'))
    .Column(NyxTextField('name'), 'Service')
      .ColumnAlignment(NyxColumnAlignment.Horizontal(nchaStart))
    .Column(NyxIntegerField('workers'), 'Workers')
      .ColumnAlignment(NyxColumnAlignment.Horizontal(nchaEnd)
        .Vertical(ncvaMiddle)));
```

Horizontal choices are logical Start, Center and End; vertical choices are Top,
Middle and Bottom. Start/End follow the host's text direction. Alignment affects
the presentation of values and editors, never their stored scalar families,
formatting, copy representation or selection identity. An omitted axis preserves
the host's existing behavior. Nyx does not infer alignment from a value's type.

`UseDefaultHorizontal` and `UseDefaultVertical` remove one axis from a policy;
`UseDefaultColumnAlignment` removes the current column's authored alignment.
Overlay combines only declared axes. Resetting alignment preserves text overflow,
row density, formatting, query settings and table arrangement. A descriptor uses
version12 only while an alignment is present; clearing all alignments restores
the earlier applicable descriptor version. Older versions retain their exact
alignment-free shape. Generated Pascal uses the same typed fluent contract.

The existing `INyxCollectionTablePresentation` facet also prepares field/family
keyed runtime alignments through `WithAlignment` and `WithoutAlignment`.
Applications persist these preferences separately from the saved design.
Preparation and publication share the existing column serial and detached
capture ownership; no second controller or publication owner is introduced.

Studio's collapsed per-column **Alignment** form offers independent horizontal
and vertical choices, Apply and Use defaults. Capture validates the mounted form,
column identity, scalar family and binding baseline before submitting the usual
paired source/history edit. A reset ignores unfinished draft choices and clears
only alignment.

For bounded semantic inspection, request `alignmentControls: true` from
`nyx_collections` on a binding or exact column. The response distinguishes support,
declaration and policy. Omitted/false keeps the former response shape. Use
`NyxSetCollectionColumnAlignment` or `NyxUseDefaultCollectionColumnAlignment` in a
typed patch, or the semantic `column-alignment` operation with a complete policy
or `null` reset. Groups use the same revision, retry, atomic admission and Undo
path as other collection edits.

Value/view, source/generated-application, target controls and Studio/MCP
authoring checks qualify different boundaries. Current evidence and remaining
native focus/paint, mounted Inspector and full Studio requirements remain in
[WORK.md](../WORK.md); this contract does not imply full target acceptance.

## Authored table arrangement defaults

Import `nyx.collections.arrangement` with the existing view and typed-field units
to author a table's initial visibility, logical pins and user command policy:

```pascal
LSpec := NyxCollectionView(NyxCollection('services'))
  .Column(NyxTextField('name'), 'Service', cmEditable)
    .ColumnLayout(NyxColumnLayout.PinStart)
  .Column(NyxIntegerField('priority'), 'Priority')
    .ColumnLayout(NyxColumnLayout.Visible(False).PinEnd)
  .TableLayout(NyxTableLayout.Enabled(True)
    .AllowOnly([nclfResize, nclfAutoSize, nclfVisibility, nclfPinning])
    .MinimumCenter(96).AutoSizeSample(64));
```

`LSpec` is a `TNyxCollectionViewSpec`. `ColumnLayout` applies to the column just
declared. `TNyxColumnPin` uses `ncpNone`, `ncpStart` and `ncpEnd`; start/end follow
the target's reading direction. The five command features are Resize, AutoSize,
Reorder, Visibility and Pinning. Table and column features jointly restrict user
commands. `Enabled(False)` disables table commands while retaining authored
visibility and pins. Minimum center width is a positive logical pixel value;
Fit samples at most `AutoSizeRows` current-page rows, bounded to 1..1024.

When the requested edge widths plus `MinimumCenter` exceed the available logical
clip, both pin groups temporarily join the horizontally scrolling columns. The
host deducts its actual scrollbar and gridline space before making this decision;
a window width alone does not establish that the pins fit. Widening the table
automatically restores the requested pins. Neither transition changes authored
widths, runtime preferences, row identities or accepted values.

Showing a managed native table creates grid handles, including lazy pinned
panes. Their local cursor initialization is not an application selection command:
an empty row selection stays empty. The initial cursor belongs to the first
visible logical field. Responsive pin transitions retain that field rather than
adopting a newly created pane's physical column zero. Deliberate pointer and
keyboard navigation still use the ordinary selection and callback contract.

Browser tables explain temporary pin suspension in a visible caption status
beside **Columns**. Both remain within the horizontal scrollport as middle
columns scroll. The message clears when the requested pins fit again, without
replacing authored help or dismissing an unfinished cell edit.

Native cell tooltips combine current authored help with query, sizing and pin
suspension guidance. The native adapter reads this owned text through LCL's
cell-hint callback because the grid's `Hint` property is a transient hover cache.
Renderer synchronization updates authored help through the optional
`INyxLCLCollectionHints` facet without refreshing data or dismissing a draft.
Disconnect restores the latest authored hint and the former host callback;
retained disconnected facets ignore later updates.

An absent policy differs from an explicitly applied default object. Either a
table or column declaration opts into the arrangement contract. A complete
default policy remains a declaration; an explicit empty feature set remains
empty. `.UseDefaultTableLayout` and `.UseDefaultColumnLayout` withdraw only the
corresponding raw policy. They retain the local binding and other column choices.
Use whole-binding **Inherit** to restore a reusable definition's binding;
**Clear** instead records a deliberate unbound mask. A policy reset cannot
silently remove that mask or rebind the table.

The selected bound table's Inspector includes an independent **Table defaults**
card. Its table form exposes Enabled, all five features, minimum center
width and Fit sample size. Each column disclosure exposes initial visibility,
logical pin and its five features. **Apply layout defaults** submits one complete
typed policy through Studio's existing paired design/Pascal command and Undo.
**Use Nyx defaults** clears only that form's declaration. Unfinished numbers such
as `-` remain raw input through inspector replacement; they are parsed only on
Apply. Pending source or structural edits lock the exact owner, and changed
schema/binding context retires an older form draft.

Each table column also has **Move earlier** and **Move later** buttons beside
its sizing controls. They author the default order represented by the fluent
`.Column(...)` sequence. The first/last edge move is disabled. A move swaps the
chosen field with its immediate authored neighbour, including hidden columns;
width, editing, formatting, text, query, visibility and logical pins stay with
their fields. Pins still determine the target's physical edge panes.

Each move is one paired design/Pascal change with Undo/Redo and survives
Save/Open and generation. A reusable instance receives a local binding; its
definition and peers retain their order. Changed schema/binding baselines,
foreign fields, pending Pascal and impossible edge moves refuse before mutation.
No new view descriptor version is needed: order is already part of the saved
column sequence. Runtime **Columns** preferences remain independent and reset
to that saved sequence.

These saved defaults are separate from an application's runtime **Columns**
preferences. Runtime resize, order, hidden fields and pins do not rewrite the
document or generated Pascal. Saved layout declarations use strict view descriptor
version eight, preserving nullable complete table/column policies and the existing
64-field budget. Removing every layout declaration returns to the applicable
earlier descriptor version. [Collection agent commands](studio-agents.md#structured-collection-context-and-authoring)
use the same typed admission and distinguish local, inherited and cleared owners.
The maintained public service-monitor consumer qualifies ordinary native Studio
ordering, paired history and its own saved/generated application at wide/compact
widths. Both Studios and portable authoring consumers compile; actual browser
interaction, physical input and visual/accessibility quality remain separate gates.

## Browser column resizing — source preview

The current W1 browser source adds resize separators only for tables using the
typed column-arrangement contract. It compiles in the maintained browser
consumer; real resize/Fit gestures still need their dedicated input checks.
The maintained query/arrangement journey passes 748 browser checks at both
1280 and 390 px. That journey does not qualify every new interaction path.

Drag a permitted header edge to preview a bounded logical width. The guide moves
without resizing the table or replacing cell editors during the drag. Release
submits one width request on the existing animation frame, through the original
typed columns capture and prepared-publication group. Logical direction follows
RTL; font, scale, available width and accepted layout changes cancel an obsolete
draft. Authored pointer capture and consumed events retain precedence.

Each separator also supports Left/Right by one logical pixel, Shift by ten,
Home/End for the authored bounds, Enter to submit and Escape to cancel. Tab or
focus departure cancels an unfinished keyboard draft; navigation after Enter
keeps the already submitted request. Cancellation restores the accepted accessible
value immediately. Composition, suspension, disable/re-enable, replacement and
retirement invalidate older input epochs before a request can publish.

Pointer capture is reconciled after acquisition and on failure. Cleanup revokes
the old proposal before platform callbacks and preserves a newer owner of the
same handle/pointer. The final publication fence reads retained canonical input
and cached layout facts after outward validation. Actual platform input,
accessibility and both-host W1 parity still require their original tests.

## Column management

The current W1 implementation includes `NewNyxColumnArrangementEditor` in
`nyx.collections.arrangement.controls`. It is an ordinary Nyx compound built from
specialized cards, rows, labels, inputs, selects and buttons. Bound tables with
authored layout controls receive a persistent **Columns** button. It opens the
shared manager through an ordinary Nyx popover on either target. Current target
qualification and remaining W1 input/accessibility work are recorded in WORK.md;
these APIs alone do not establish complete parity.

`INyxCollectionColumnArrangementMount` is an optional mount facet. Its
`ColumnArrangement` property exposes `INyxColumnArrangementManager`, with typed
Open, ReloadDraft, SetEnabled, Observation and Presenter operations. The
renderer supplies the target factory and its original readonly input lifetime;
the manager uses the existing view-owned preferences and prepared publications.
No additional preferences, collection or query store is created.

Each column's compact button and field captions have a contextual accessible
name, for example `Set width: Service (name)` and
`Pinned edge: Status (status)`. The exact field distinguishes duplicate authored
titles; an empty title falls back to its field name. Browser focus controls and
native `AccessibleName` receive the same text. Global reset actions keep their
ordinary names. This changes presentation metadata only: command identities,
capabilities, preference ownership and saved column defaults are unchanged.

Retain the view's exact launch capture separately from the detached form:

```pascal
LColumns := NyxCollectionColumns(LView);
LCapture := LColumns.CaptureColumns;
LObservation := TNyxColumnLayoutObservation.Read(LCapture);
LEditor := NewNyxColumnArrangementEditor('service-columns', LObservation);
```

Every field appears in the complete preference order, including hidden fields.
Show/Hide and pin choices respect the effective table/field policy. Move earlier
and later do not cross fixed reorder barriers, including hidden barriers. Pin
groups use the same relative order; changing the complete order does not move a
field into a different pin group. The launcher remains outside the visible-field
map so an all-hidden table can still open it. Authored policy can
intentionally prevent every visibility change; the editor explains that case
instead of inventing permission to show a column.

Set width accepts a positive whole number within the authored bounds. Restore
default width removes the runtime override and returns to authored Fixed, Flex
or Content sizing. Fit current page is a separate typed measurement request,
respecting AutoSize even when manual Resize is disabled. Restore default order
preserves per-field choices; Reset width, visibility and pinning preserves order;
Restore all column defaults removes both. Removing overrides remains available
when new user changes have been disabled by authored policy.

`CaptureNyxColumnArrangementEditor` reads the synchronized mounted form and
returns a detached `TNyxColumnArrangementChange`. An unrelated control returns
False; stale baselines, wrong typed fields, malformed choices and disabled
actions refuse without changing the form or view. Capture reads only the input
needed by the chosen action, preserving unrelated incomplete width drafts. The
host must preserve those drafts across subsequent accepted refreshes as well.

Actions carry complete typed field identities. Bounded opaque IDs are checked
against those identities and the retained launch observation; displayed indexes
and captions never select a target. Long and supplementary Unicode field names
remain intact in metadata. Supply an explicit valid editor ID of at most96
Unicode scalars, leaving room within Nyx's128-scalar node-ID bound for children.
Use the typed action/input ID helpers when locating a part. They return source
IDs; a presenter resolves them inside the exact realized editor instance, where
runtime IDs may be escaped and qualified.

The host still needs the original capture's private identity proof, current
input/event/form lifetime guards and the existing prepared-publication group.
Preferences publish through `INyxCollectionColumns.PrepareColumns`; a fit request
needs bounded host measurement and `PrepareAutoSize`. The compound creates no
second preference/query owner and changes no source data or authored defaults.

The shared `nyx.collections.arrangement.manager` controller captures the actual
realized editor before ordinary callbacks run. Both main and form generations
keep their original input epoch; form generations also keep the actual popup's
successful-open lifetime. Silent Close, disablement or retirement cannot grant
old queued work a fresh form. Closing and reopening preserves the mounted form
and unfinished inputs. The visible Close button is safe initial focus. An
external layout change keeps the old baseline, explains the stale draft on
reopen and refuses stale actions; **Discard draft and reload current columns**
deliberately captures the new accepted state. The explanation samples after
presentation/focus callbacks without granting existing actions fresh authority.

Hiding a column withdraws active editing and focus authority while preserving
compatible unfinished cell text as a dormant draft. Revealing it neither commits
that text nor steals focus; an explicit later editing entry can resume it.
Draft handoffs carry hidden values without an editing/focus claim. Changes to
the row, typed field/schema or accepted scalar invalidate incompatible text.

Native editor reveal can itself change table geometry. That reflow waits until
the original edit and its draft restoration finish, then uses the existing idle
queue after native input, validation, composition and active sizing have ended.
Geometry alone does not block editing or mark the data/column map stale. This
keeps unfinished text intact when a compact table reuses an editor across fields.

Escape cancels the current native column drag before its original mouse release
can accept a width or produce a table Click. The canonical key callback runs
first and may consume Escape. Cancellation retains the original input epoch and
request, so a callback cannot redirect it to a replacement table or newer drag.
The existing idle path restores accepted geometry while preserving cell drafts;
a fresh drag can then start normally.

Browser resize notifications can arrive after the next keyboard adjustment has
already captured current geometry. These notifications preserve that adjustment
when its original geometry still matches; actual geometry or input changes
still revoke it. Rapid consecutive minimum/maximum adjustments need no extra
settling delay in application code.

`CaptureNyxColumnArrangementDraft` additionally captures raw width/pin inputs from
the exact synchronized realized editor and mounted root. It records only values
that differ from the launch display as unfinished drafts. An incomplete width
such as `-` remains text; hiding/reordering another field must not parse or erase
it. Hidden fields keep their drafts, too. The detached record copies its
specification/observation and retains no renderer, widget or publication owner.

After the host knows an action committed, use the draft's `AfterAccepted` with a
fresh accepted observation. Use `AfterMeasuredFit` for a fit action, supplying
the actual bounded width installed by `PrepareAutoSize`. Both require the exact
expected preferences, unchanged specification and single expected serial change;
a reentrant layout change refuses rebasing even when it restores equal values.
A committed notification error can still require this accepted-state handling;
a refused publication must keep the original draft unchanged.

Width application/default/fit clears only that field's width draft. Pin application
clears its pin draft. A field reset clears both; restoring all defaults clears all
drafts. Visibility/order changes retain unrelated unfinished inputs. Reconstruct
with the three-argument `NewNyxColumnArrangementEditor` overload at the draft's
exact rebased baseline. The actual management host and its successful-publication,
input/event/form guards remain integration work; these helpers do not prove that
any action committed or make the form safe to apply after it closes.

## Fit current page sample — source preview

The optional runtime facet `INyxCollectionColumnSampling` separates a bounded
page sample from host font measurement. `CaptureColumnSample` admits the exact
launch columns capture and typed field, then copies the title/descriptor and at
most `AutoSizeRows` current-page cell texts into `INyxCollectionColumnSample`.
An empty page has no sampled cells; its title can still be measured. Hidden
fields can be sampled without changing visibility. AutoSize permission is
independent of manual Resize permission. All four scalar families use the same
canonical display formatter as ordinary `CellText`.

The sample retains immutable data and a canonical inert copy of the admitted
layout proof. It retains no live view, store, renderer or widget and does not
retain an arbitrary caller capture wrapper. The host measures these copied
texts in its actual font/DPI context, clamps the result to the authored bounds,
then calls `PrepareSampleAutoSize`. This joins the existing column preparation
and publication coordinator; it creates no second query/data/preference owner.

The measured path checks exact source, page/unpaged snapshots, layout identity
and serial, query intent and an internal view-notification stamp. These checks
run after outward capture/data getters and again during final prepared readiness.
Every accepted view notification invalidates an older sample, including a
selection-only notification. Paging away and back cannot silently bless an old
measurement. Stamp saturation refuses samples without blocking ordinary view
notifications. Obtain a fresh sample when a measured request is refused as stale.

The older `PrepareAutoSize` remains the caller-supplied-width contract. Its
original ABI and semantics do not acquire the new sample guarantees. Target font,
DPI, input/IME and actual form-open lifetime checks remain host obligations.
This model sample/preparation slice compiles for both targets; actual target
measurement, form integration and both-host W1 input tests remain ahead.

## Native fit metrics — source preview

`MeasureNyxLCLColumnFit` in `nyx.collections.arrangement.lcl` measures accepted
sample text on a private bitmap canvas. Supply the actual header and body fonts
separately: standard native headers use TitleFont. Supply the actual drawing
styles, the resolved target PPI and already-scaled total horizontal chrome insets.
The header text includes the current query/header adornments. The adapter copies
these facts before any sample getter and borrows no active grid or cell editor.

For a table embedded with uniform canvas zoom, the adapter copies fonts with
their exact logical heights from the grid's current projection port. The optional
`INyxLCLLogicalTypography` facet supplies detached native-DPI heights without
changing the original projection interface. A retired or partially projected
observation refuses; a projected grid without that facet cannot guess by dividing
rounded physical heights. Unprojected grids use their ordinary fonts. Private
managed copies survive outward sample getters and retain no grid or renderer.
The existing metric and publication fences still apply; copied typography alone
grants no authority to publish a width.

Measurement follows native single-line, prefix, tab and bidi flags, using the
same NUL-to-quoted-JSON conversion as native cell display. Natural width removes
soft wrapping and ellipsis before authored clamping. Physical metrics are
converted to logical pixels once; already-scaled padding is not scaled again.
Rotated fonts and `SystemFont` callbacks are explicitly refused; they and
arbitrary custom painting require their own resolved metric contract.

The returned `INyxLCLColumnFitMeasurement` owns copied fonts and exposes a detached
`INyxColumnFitMeasurement`. That immutable, reference-counted value reports natural width, ceiled/bounded
Width, Clamped, the exact immutable Sample and SampleRows. SampleRows is the
measured count, not the total current-page row count. No viewport clamp is applied;
horizontal overflow and narrow pin suspension stay with normal presentation.

`Copy` retains the immutable measurement. `WithNaturalWidth` creates a distinct
measurement using its copied bounds and sample facts without rereading sample
getters. Nil means no measurement. Both FPC and pas2js retain the exact sample
through ordinary interface assignment and release it after the final measurement
reference is cleared; no raw holder or record finalization workaround is needed.

`Matches` is an explicit UI-thread font/style/header/chrome comparison. A host
must retain/recheck its actual mount before reading those current facts. This
method is not the final callback-free publication fence and grants no input,
event, IME or form-open authority. Actual host command/fence integration and
both-target physical validation remain open; compiled metric support does not
establish a completed Fit UI.
Actual main/form cancellation, popup lifetime integration, host launchers and
both-target wide/compact journeys remain required before this is ready to use.

## Browser Fit measurement

`MeasureNyxBrowserColumnFit` in `nyx.collections.arrangement.browser` copies a
bounded sample before observing the mounted table's styles. Supply its exact
header cell, label, sort/filter buttons and same-column body cell/navigation
label through `TNyxBrowserColumnFitSurface`. An empty sample needs only the
header. If the body cell also owns a standard editor, supply `BodyEditor` so the
metric can recognize that child by identity. It never reads the editor value.
For the standard compact header, supply `HeaderTools`, the exact single-row
flex parent of the label and buttons. Its complete child order, horizontal
direction, gap, insets and minimum width participate in the detached metric.
`FilterGlyph` identifies the standard decorative fixed-size SVG; its admitted
box is reproduced by a neutral element, without cloning vector/live content.
Absent optional parts preserve the original direct-child composition. Unknown
nested content still requires a custom metric.
The host remains responsible for exact column identity, a current mounted target
and uniform library body typography across the sampled rows.

The header is measured as one ordinary offscreen composition, in actual child
order, with copied table-cell whitespace/direction/language and each visible
part's own typography, display, padding, borders, margins and minimum width.
That preserves spaces between the title and filter button, hard line breaks
and inline box rules. Accepted body strings use the body label's separate
typography. Actual horizontal cell chrome is added once; reserved resize-grip
space is not counted twice. CSS pixels already are logical pixels, so no DPR
or ancestor transform multiplier is applied.

Text-aware `FontFaceSet.check(font, renderedText)` runs before and after measuring
the attached probes. A pending font refuses synchronously; the metric does not
load a font, register a listener or queue a later publication. All temporary
ordinary elements are removed in `finally`, have no IDs/classes/events and
cannot participate in a form or normal keyboard navigation. Only detached
styles, strings and the immutable sample survive in the returned interface.

Canonical hidden columns remain measurable from their copied styles without
being shown. This default-library metric refuses generated/custom child content,
other non-table cell layouts, vertical text, transforms on the measured parts,
negative margins,
nonzero relative text offsets and unsupported header/body layouts. Custom
renderers need their own resolved metric. `Matches` recaptures the styles and
remeasures the copied strings, which also catches a changed font face under an
unchanged CSS font string. It is a UI validation step, not the final callback-free
model/input/IME/generation/form fence. The shared `WithNaturalWidth` value update
uses only previously copied bounds and performs no model/interface getter.

The ordinary managed Columns command uses this metric. The 2026-10-31 full
presentation journey passes 159 browser checks at each 1280x900/390x844 allocation
and 167 native checks per width with clean heaps. It preserves the original
full-value Fit and operational-limit assertions, and additionally rejects a
changed tool-row gap, changed glyph allocation, wrong parent and custom child.
Accepting that known glyph relaxes only the text-child check; an initially
out-of-flow filter button still refuses measurement under the original guards.
The metric retains no live DOM. Broader W1/W2, physical/assistive and performance
acceptance remain separate; WORK.md retains the original failures and source
identities. Unpinned arrangement headers also retain their sticky positioning,
with normal headers above pinned body cells and pinned headers above both.

## Mounted Fit preparation

The target mounts now provide an internal protected `PrepareColumnFit` seam.
It accepts the original launch capture and full typed field, measures accepted
current-page sample text, and returns an owned preparation with the model plan
and final input fence. The ordinary managed Columns form consumes this seam;
applications retain the public typed arrangement contract.
The managed output parameter uses `var` so a former metric is released only
after the mount owns its entered call frame. A returned metric is a proposal,
not evidence that its width was installed.

Browser acquisition uses existing canonical header parts and an already-resident
cell of that same field; it does not realize/scroll a row or read an editor.
Native acquisition uses the actual visible/pinned pane fonts and default text
styles. Hidden fields use the logical table's uniform outer defaults without
being shown. Pinned panes inherit title/body drawing defaults together. Native
headers and their Fit metrics share the NUL-safe display conversion.

Both paths prepare through the current-page sampling facet and independently
check original input epoch, exact mount/map and model mutation facts. Native
input activity includes complete grid/editor/OS call tails through the existing
renderer frame. Final readiness reads retained flags, without dereferencing a
native component or calling a browser/style/router getter. Read-only data can
still permit column fitting; AutoSize permission is independent of manual Resize.

The shared manager validates its original event/form generation guard before
the nested Fit group, making metric validation the last known outward check.
The resulting width does not promise atomic typography against unrelated later
direct style changes. Ordinary model notifications own refresh and compatible
draft restoration. A committed notification exception must not cause a retry or
rollback. The full presentation consumer exercises actual form lifetime and
publication at both widths, as qualified above. Complete W1/W2, physical and
assistive input acceptance remain separate.

## Native column resizing — source preview

The current native source retains each drag's original input epoch,
typed column identity, model capture and physical table mapping. A later callback
cannot supply a fresh capture to revive a disabled, suspended or superseded drag.
The authored Resize feature and width bounds still govern the model proposal.

Lazarus delivers the final header width after its mouse-release callback. Nyx's
nested release stamps distinguish that tail from a newer drag started inside the
callback, even on the same column. Queued publication waits for the complete
native input tail, then checks the original input and model again. Cleanup clears
only its own request; a newer drag keeps its width and ownership.

Cancellation also uses the existing coalesced idle handler when a callback pumps
messages during refresh or editing is suspended programmatically. It preserves
pending cell refreshes and dormant edits. Physical reconciliation follows ordinary
refresh, END and Resume behavior; it does not manufacture a native mouse release.
A committed notification error reports the installed change without retrying it.

The separate native Fit source below now uses the same pending command owner.
Optional Lazarus auto-size is not admitted as a Nyx manual resize result.
The management launcher, actual form lifetime integration and complete native/
browser wide/compact qualification remain ahead.

## Native header and keyboard Fit — source preview

The current native source offers **Ctrl+Shift+F** for the focused
column and **double-click on a header edge**. Both measure accepted current-page
text through the bounded Fit preparation above. AutoSize permission is separate
from Resize: denying a drag does not deny Fit. The grid hint describes the actions.

The keyboard action honours an application's consumed key and does not commit,
hide or focus the active editor. Header Fit waits for actual mouse release;
measurement runs after the complete native input tail. One typed pending command
arbitrates Fit and Resize, retaining the original column, input epoch and capture.
Nested newer gestures supersede older ones; failed canonical callbacks retire
their own intention. A stale double-click cannot reset the newer grid state or
fall through to unbounded native auto-size.

Accepted layout notification uses the existing compatible draft restoration.
Composition, disabled/suspended input, stale maps and superseded commands refuse
the original request. Real native input/draft tests are still required. Browser
Fit input below, manager/form integration and complete both-host qualification remain
open; this preview is source behavior, not a validated shipped control.

## Browser Fit and canonical input — source preview

The browser source offers **Ctrl+Shift+F** on the exact focused
column grip, resident cell or cell editor, and **double-click on the owned header
grip**. Fit-only grips also accept Enter/Space, separately from manual Resize.
Their initial pointer down preserves editor focus and drafts. Only accepted
current-page text enters the bounded sample and metric; editor draft text does
not become measurement input.

The renderer captures one managed command before the first real event-router
query. It brackets the existing keyboard phases or double-click callbacks, then
either transfers that same intention to the existing frame or retires it. A
consumed key preserves an earlier manual resize draft. A later nested resize
action wins even when it returns to the same width. Replaced/retired controls
cannot regain authority through bubbling.

Canonical callback delivery and default Fit eligibility have different guards.
An ordinary callback can update data and the remaining keyboard phases still
receive fresh accepted payloads. That change may invalidate the earlier Fit
request; it does not silently delete the application's keyboard cycle. Original
model/input/map/activity fences still govern publication. General Click, pointer
dispatch and Emit paths retain their existing behavior.

This partial source compiles, and the maintained query/arrangement browser
journey executes at both widths. Complete physical Fit-input qualification is
still separate from that journey.
Fit-only buttons now use canonical Click actions, state admission and callbacks
before their original Fit default. Browser Enter follows key phases; Space
activates on release and cancels on blur, disable or suspension. Repeats do not
activate it, and a trailing double-click does not add a third Fit. Native buttons
use the existing Nyx/LCL button and physical input frame, preserving original
release provenance across Win32's CN_KEYUP delivery. Explicit suspension/resume
and disable/re-enable cannot revive an old held press.

Focused browser cases pass at desktop and compact widths; focused native cases
include both pinned edges, draft preservation, command rejection, notification
failure and callback retirement, with clean heaps. These are actual DOM/LCL
control checks, not hardware or assistive-technology certification. Complete
management/form integration and the full physical input/accessibility journey
remain required; WORK.md records the exact qualified source cohorts.

Arrangement-enabled query mounts configure the existing target factory through
`INyxCollectionQueryInputFactory`, retaining only readonly input facts. Both the
main router and the actual form router keep their existing subscriptions and
scheduler. A form additionally captures the actual popover's successful-open
token. A custom factory must support this optional input contract before its
first form; missing or late configuration refuses explicitly. Ordinary factories
on tables without arrangement retain their existing behavior.

The optional `INyxCancellationReadiness` contract supplies callback-free final
cancellation admission after model validators. Owned scheduler/event scopes and
arranged query generations expose it. Captured query readiness checks the original
scope, input epoch and actual form-open token; it cannot capture a replacement
generation or invoke an arbitrary Cancelled getter during publication install.

Safe single-click wiring also requires immediate retirement of the binding's
state subscription while an already-entered authored action keeps its data alive
until return. A retained root alone cannot prevent an obsolete subscriber from
affecting a replacement view. This shared ownership prerequisite is currently a
separate renderer-lifetime integration requirement. The shared
`TNyxLiveBindings.Retire` operation is now implemented and tested natively and in
the browser. It immediately disconnects the exact subscription, revokes pending
resource receivers and clears both synchronization callbacks. Retirement is
terminal: Activate and nonnil callback reassignment refuse afterward. An entered
command can finish with its accepted result or committed notification failure;
Retire does not undo accepted state.

Retire does not own or free the coordinator, root or store. The caller must keep
them alive until every entered call returns. Generation-owned renderer teardown,
including state transfer to a successor and physical native Sync frames, remains
required before callback-driven renderer disposal is supported by this path.

`nyx.binding.lifetime` supplies the minimal `INyxBindingOwnedState` holder for
that ownership transfer. `OwnNyxBindingState` adopts one raw state exactly once;
entered and successor generations must share the same returned interface.
External borrowed stores use no holder. Eleven counted checks pass on both
targets, including successor retirement while an entered lease survives. This
primitive is tested separately; consuming it in renderer generations remains
part of the pending integration.

## Selection, edits and failures

`Select` accepts a collection-scoped `TNyxItemRef`. Single selection remains the
default. Import `nyx.collections.selection` and choose `.Selection(nsmMultiple)`
on the view specification to admit multiple identities:

```pascal
LTasksList := NewNyxList('tasks-list');
LTasksList.Binds.Collection(
  NyxCollectionView(NyxCollection('tasks'))
    .Column(NyxTextField('title'), 'Task')
    .Selection(nsmMultiple)).Done;

LTasksView.Select(LFirstTask);
LTasksView.Select(LReviewTask, nsaToggle);
LTasksView.Select(LNextTask, nsaFocus);
```

The managed immutable `Selection` snapshot exposes `Count`, `Contains`, `ItemAt`,
`Focus`, `Anchor` and `DataRevision`. Membership, keyboard focus and range anchor
are independent scoped identities. Items are returned in admitted dataset order.
`Selected` remains the compatible single-item convenience; it prefers the focused
selected item, then the first member, and rejects empty membership.

`SetSelection` admits the complete candidate before publishing once. Duplicate,
unknown or foreign identities, invalid focus/anchor, or multiple members in Single
reject atomically. Equal membership/focus/anchor emits no duplicate notification.
`nsaRange` replaces a range; `nsaAddRange` keeps existing members. Programmatic
ranges follow dataset order. Target adapters pass the actual visible hierarchy
to `SelectRange`, so collapsed descendants do not enter a keyboard range.
`SelectAll` requires Multiple and includes the whole dataset. `ClearSelection`
removes membership, focus and anchor.

Selection survives value updates and reorder operations. Removal prunes missing
members; a removed focus may move to a nearby cursor without selecting it. A
retained snapshot still owns its earlier identities and retains no dataset,
widget or receiver. Each bound view owns its selection independently, including
views sharing an application store. Application page navigation preserves those
views. Native grid cell/editor position stays separate from row membership.

Actual browser/LCL list, table and tree controls support arrows, Home/End,
modifier-assisted focus, toggle/range/additive-range and select-all. Trees support
left/right parent, child and expansion navigation. Canonical before-key callbacks
run first and can consume a default. Read-only data permits selection; disabled
controls refuse forced gestures. Browser rows expose their selected state.
Lists/trees retain a visible row entry; tables have one data-cell Tab entry and
grid/row/gridcell semantics, with row/column dimensions and cell indices.

These choices are mapped to the W3C [listbox](https://www.w3.org/WAI/ARIA/apg/patterns/listbox/),
[tree view](https://www.w3.org/WAI/ARIA/apg/patterns/treeview/) and
[grid](https://www.w3.org/WAI/ARIA/apg/patterns/grid/) guidance. The current adapter
uses the modifier-assisted selection model. Full pattern qualification remains
open, including cell/column selection, assistive technology checks and
physical-device testing. This delivery does not establish blanket accessibility
conformance.

## Copying accepted collection data

`nyx.collections.clipboard` captures an exact accepted cell or an explicitly
chosen selection into `INyxCollectionClipboardText`. The payload owns its text,
typed scalar values, row/field identities and source revision. It retains no
view, store, editor or target widget, so a later refresh or retirement cannot
redirect a pending copy to a different service/process.

Use the complete store snapshot for source membership. The view's snapshot is
its current filtered, sorted and possibly paged result; pass that separately
when choosing `nccmSelectedPage`. A selection snapshot and source must have the
same collection key/data revision. A stale or empty selection refuses before
any clipboard write.

```pascal
LSource := LTable.Store.Snapshot;
LSelection := LTable.Selection.Snapshot;
LPayload := NewNyxCollectionClipboardRows(
  LSource,
  LSelection,
  [NyxColumnIdentity(NyxTextField('Service')),
    NyxColumnIdentity(NyxTextField('Status'))],
  nccmAllSelectedSource,
  nil,
  True,
  nccleLF,
  TNyxCollectionClipboardLimits.Defaults);

LRequest := WriteNyxCollectionClipboard(
  LPayload, LClipboardHost, LScheduler, LCancellation, LReply);
```

Here the caller supplies an `INyxClipboardWriteHost`, UI scheduler, optional
cancellation scope and completion method. The host may complete inline. Keep
the borrowed completion receiver alive until completion or explicitly cancel
its request before destroying that receiver. Cancellation detaches delivery;
it cannot reverse a clipboard write that already happened. Configure platform
authority at the adapter boundary rather than storing it in a design document.

`nccmAllSelectedSource` includes selected rows hidden by filtering/paging, in
source order. `nccmSelectedPage` intersects that same immutable selection with
the explicit page, in page order. Neither means all matching unselected rows.
Explicit typed fields choose order and hidden-column inclusion. Alternatively,
`NyxCollectionClipboardFields` resolves visible presentation order or complete
arrangement order from an already captured `TNyxColumnLayoutObservation`.

Row output uses TSV, optional exact field-name headers and explicit LF/CRLF row
separators, without a trailing row terminator. Cells containing tabs, newlines,
quotes or NUL are quoted, with doubled quotes. A single-cell capture instead
returns the whole unescaped raw value. Boolean/numeric text is invariant; text
preserves Unicode, embedded NUL and original cell line endings. Neither path
copies a formatted display or unfinished edit draft. Fluent limits bound rows,
fields, output cells, inspected rows and UTF-8 bytes before external effects.

The typed clipboard receipt distinguishes written, unavailable, denied,
activation-required, cancelled and failed outcomes, and identifies the actual
representation. Exact mode refuses a host that cannot preserve the requested
text. Platform-text mode explicitly allows platform line-ending conventions;
embedded NUL cannot be advertised as interoperable plain text. The browser and
LCL adapters expose injectable authority for testing. Real OS clipboard and
complete mounted keyboard/menu workflows have their own qualification; the
pure capture helpers do not establish those interactions.

## Explicitly copying displayed values

An enabled managed cell menu offers **Copy displayed value** beside **Copy
accepted value** when its column declares a display policy and the existing
clipboard controller supports the optional represented-output port. The same
choice is available in full-value inspection. It captures the original scalar,
row/typed field, revision, formatter, locale facts and domain; later column or
locale changes cannot redirect that copy. An unfinished editor remains a draft.

`INyxCollectionCellText` owns the tagged output, exact accepted `RawText` and
scalar/provenance facts. A formatted output carries the complete immutable
formatter policy. It is distinct from the unchanged RAW
`INyxCollectionClipboardText` interface and its observer/result contract.
`INyxCollectionClipboardRepresentationWrite` exposes represented results while
sharing the existing host, request serial and cancellation slot. A newer RAW or
display request cancels the earlier receiver; an old ticket cannot cancel its
successor, including during reentrant cancellation. Receipts validate the actual
outbound UTF-8 byte count.

Display copy is strict. Unsupported decimal range, exact rounding or clock
precision refuses before any clipboard host effect. The inspector explains the
refusal and keeps **Copy accepted value** available; a paint's unavailable marker
is never copied as successful formatted text. Host failure/cancellation retains
its typed outcome. Normal editor text-copy remains the platform editor's own
operation. Explicit focused-draft output is not inferred from accepted values or
from an arbitrary tagged string.

Target adapters reserve the exact actions intent through the optional
`INyxCollectionCellActionInput.BeginCellInput` before reading physical cells,
source identities or a foreign RAW input factory. Its returned
`INyxCollectionClipboardCellCommand` accepts the exact row/field through
`ResolveCell` and stays the same envelope through canonical dispatch. A nested
newer context makes the earlier envelope stale permanently; completing or
retiring that earlier action cannot open a menu with the newer authority or
close its accepted menu. This optional facet preserves existing interface
vtables and performs no clipboard effect by itself.

Current qualification uses injected clipboard/save authority on actual native
and browser controls, with separate RAW/display arbitration and retirement
checks. It does not claim real OS clipboard access or full workbench acceptance;
see [WORK.md](../WORK.md).

## Explicit focused editor drafts

The optional `INyxCollectionFocusedDraftInput` in `nyx.collections.draft` reads
the actual focused editable text cell through `CaptureFocusedDraft`. Its copied
value is tagged `nccrExplicitDraft`, with exact accepted RAW scalar, row/field,
revision and domain beside the unfinished text. A numeric `-` remains text;
capture does not parse, validate, commit, blur or suspend that editor. The copied
selection uses Unicode scalar positions. Retaining the value keeps no widget,
mount, model or store alive.

Capture requires configured managed copy authority and an observed editor/input
lifetime. Browser mounts and ordinary or arranged renderer-managed native tables
provide that observation. Unobserved direct native mounts return unavailable;
dormant, foreign, disabled and composing editors cannot lend focus authority.
The capture's original input epoch/readiness cannot authorize a replacement
editor after retirement or an ABA input transition.

Callers deliberately submit this tagged value through the existing represented
clipboard port and its original request/cancellation slot. Accepted-value copy
and the platform editor's normal Ctrl+C remain independent defaults.

Managed draft actions are deliberately opt-in at runtime. The optional
`INyxCollectionCellActionOptionsMount` preserves the original mount interface;
configure it at the same one-time presenter admission as ordinary cell actions:

```pascal
LDraftOptions := TNyxCollectionCellActionOptions.Defaults.FocusedDraft(True);
LActionOptionsMount.ConfigureCellActionsWithOptions(
  LPresenterFactory, LScheduler, LDraftOptions);
```

An opted-in editor context captures its exact focused draft before previous or
canonical context callbacks and before menu focus changes. The same reserved
command carries its accepted row, fully typed field, value and revision through
dispatch. A newer nested context makes the returning older command inert; it
cannot replay a native menu or borrow the newer command's authority. Default-off
editors retain their ordinary platform context menu.

The managed menu exposes **Inspect captured draft** and **Copy captured draft**
separately from accepted/displayed values. Inspection uses a readonly Nyx memo
with escaped controls and backslashes; copying preserves the exact original
unfinished text. The inspector also offers **Copy accepted value**. Draft copy
uses the existing request slot and an explicitly tagged representation result,
without an accepted-RAW reply or any parse/commit of the editor text. Stale draft
readiness refuses transfer while keeping accepted-value copying available.

Current qualification passes 90 command and 76 representation checks on each
actual target, plus 224 native control checks with a clean heap and 283 browser
control checks at measured desktop/compact widths with empty logs. The control
journey covers arranged and ordinary tables through the same maintained fixture.
The later explicit-action journey retains those assertions and passes 332 native
checks with a clean heap and 415 browser checks at verified 1280x900 and 390x900,
with empty logs. It exercises actual menu/inspector buttons, accepted/draft
receipt separation and nested stale context admission on both targets.
Effects use injected hosts; real OS clipboard behavior and full workbench
acceptance remain separate.

## Explicit cell range captures

`nyx.collections.range` supplies immutable cell rectangles using stable
`TNyxCollectionCellIdentity` endpoints. Each endpoint pairs a collection-scoped
item with its fully typed field. Capture takes the actual accepted source/page
and copied visible presentation fields; it never infers coordinates from a
native grid pane or substitutes ordinary row selection.

Both endpoints are included. Anchor/focus preserve gesture direction, while the
payload follows actual page and visible-column order. Reversing endpoints keeps
the same output. Hidden/off-page/foreign endpoints, mismatched scalar families
or source revisions and work/output limits refuse before publication. Captured
RAW TSV reuses `NewNyxCollectionClipboardRows` with SelectedPage membership,
including its header, escaping and line-ending rules. A singleton range remains
explicit TSV; ordinary single-cell RAW copy keeps its existing exact text.

The independent `INyxCollectionCellRangeSelection` in
`nyx.collections.range.actions` owns one accepted range intent. Select reserves
intent before outward snapshot getters and publishes only a complete admitted
candidate. Failed admission preserves the previous choice. Nested Select, Clear
or terminal Revoke prevents an older returning call from replacing that newer
decision. Replacing/clearing a selection retires its original flags before
releasing old snapshots; retained copied data remains readable afterward.
Each intent exposes flags-only readiness independently of its immutable source,
page and payload. Commands must still fence their original mount/input/column
cohort before using it.

An ordinary table mount also exposes `INyxCollectionCellRangeMount` from
`nyx.collections.range.commands`. Enable it explicitly with `ConfigureCellRange`
and use `SelectCells` with those typed endpoints. Its selection stays independent
of row selection. The actual native/browser table highlights the accepted
rectangle across both pinned edges. Native paint reads cached bounds/field bits,
with no dataset getters in `DrawCell`; browser cells expose their selected state.
Refresh, filtering, suspension, disabled input and retirement withdraw the old
range rather than borrowing authority from a new page.

With the existing injected clipboard controller configured, Ctrl+C on a data
cell copies that exact current rectangle through the original request slot.
Ordinary editor Ctrl+C and default-off tables retain their existing behavior.
`INyxCollectionClipboardRangeInput` is an optional controller facet; old
interfaces and row commands keep their contracts. A stale range remains a
claimed refusal and cannot turn into a newly captured single-cell command.

An enabled managed cell menu also offers **Copy selected cells** when its launch
cell belongs to the captured rectangle and `nccaCellRange` is allowed. It captures
that range before outward context callbacks and shares the existing clipboard
request slot. The action copies immutable accepted RAW TSV; focused editor text
and the ordinary **Copy raw value** action remain separate. Clearing or replacing
the original range visibly refuses its old menu action without disabling RAW
cell inspection/copy or substituting a newer rectangle. An optional range that
exceeds copy limits does not prevent the ordinary cell menu from opening.

For an explicitly enabled table, the current adapters also support these keys
from a data cell in navigation mode:

| Key | Cell-range action |
| --- | --- |
| Shift+Left/Right/Up/Down | Extend from the original anchor to the neighboring visible cell |
| Shift+Page Up/Page Down | Extend in the same column by the current viewport's row step |
| Shift+Home/End | Extend to the first/last visible column of the focused row |
| Ctrl+Shift+Home/End, or Meta+Shift+Home/End | Extend to the first/last cell of the actual current page |
| Escape without modifiers | Clear an existing cell range |

Movement clamps at page and column boundaries. It follows visible presentation
order, including pinned columns, and preserves the original anchor when the
direction reverses. Arranged tables on both targets respect the table's
right-to-left direction. Extending a range moves its physical focus
without replacing ordinary row membership or editing accepted data. The browser
retains one navigation Tab entry. Active editors keep their own text-selection,
caret and copy keys; composing input and Alt/AltGraph combinations are left to
the existing input route. Tables without the explicit opt-in keep their existing
selection behavior.

Page extension uses the same geometry as ordinary Page Up/Page Down navigation:
native grids measure visible rows; browsers use the clipped, scaled data plane,
excluding virtual overscan and retained offscreen drafts. Consecutive steps
overlap one row and always advance at least one. The movement clamps within the
current logical data page, retains the original anchor when reversing and does
not wrap, fetch another page or change ordinary row membership. Resizing the
viewport changes the next key's step. This follows the viewport paging model in
the [WAI grid keyboard guidance](https://www.w3.org/WAI/ARIA/apg/patterns/grid/);
Nyx adds the Shift extension alongside its existing cell-range keys.

`nyx.collections.range.navigation` exposes the typed movement intentions and
the optional actual-mount `INyxCollectionCellRangeKeyInput` contract. Its
single-use envelope reserves the original input epoch before row/field getters,
prepares the complete candidate before canonical key callbacks and rechecks
the original source/page/column and selection authority after focus callbacks.
Consumed input, a newer selection or retirement defeats that candidate without
retargeting another cell. `Complete` claims the envelope once even when its
default action is refused; calling it again cannot revive the prepared range.
These synchronous UI contracts do not transfer a live mount to a worker or
clipboard host.

Page-key adapters query the optional `INyxCollectionCellRangePageKeyCommand`
facet on that same reserved envelope and call `ResolvePageRows` before Prepare.
Geometry is captured once after input reservation; missing, malformed or late
geometry cannot silently become a one-row command. The optional readonly
`INyxCollectionRangePageNavigation` facet carries the scalar step on detached
proposals. Selection preparation rechecks foreign proposals against that step
and fences every extension getter against newer selection decisions. Existing
interface identities and key ordinals remain unchanged. Authors enable the keys
with the existing `NyxTableWorkbench.RangeSelection(True)` policy; there is no
new saved setting or raw property string.

With range selection enabled, focus a data cell and Shift-click another cell to
extend the anchored rectangle. Both adapters use the exact resident endpoints
in visible column order, including pinned surfaces. Editors, custom interactive
descendants and headers keep their ordinary input behavior. Plain clicks retain
ordinary row selection; range selection never edits accepted data or grants a
clipboard effect.

`nyx.collections.range.pointer` provides a typed endpoint proposal and optional
selection/mount preparation facets. The adapters reserve the original input
before hit testing or focus, admit the complete RAW rectangle, deliver the
canonical pointer callback once and recheck the same input after focus. A
sequential callback can consume the default and negotiate capture through that
same dispatch. Cancellation, a newer selection and disable/re-enable prevent
both stale range installation and stale pointer capture. The matching click
still delivers its callback once while preserving independent row membership.
Both routes now additionally implement sustained Shift-mouse movement after the
immediate endpoint selection. Native message qualification is described below;
browser execution and native/browser pen/touch admission remain open.

`nyx.collections.range.stroke` supplies the portable pending lifecycle for those
adapters. A typed pointer identity and copied original-input facts keep
mouse, pen and touch releases distinct. `Stage` admits a complete RAW candidate
and exposes an immutable preview without publishing it. `PrepareRelease` reserves
the last candidate; its first `Complete` can publish only after unconsumed
canonical input and exact endpoint focus. Cancellation, capture loss, changed
source/page or input generation preserve the accepted range. Retained previews
keep no renderer, mounted entry or live view/store owner alive. This model does
not establish physical capture authority or install drag handlers by itself.

The optional `INyxCollectionCellRangeStrokeInput` mount facet connects that model
to a **fresh synchronous entry per message**. Reserve Begin/Move/Release before
target hit/focus/source reads, resolve the owned cell identities, prepare, then
complete from that original canonical event's allow-default result. Begin uses
the requested origin; Move/Release use the actual currently focused cell.
Only `PendingStroke` survives entry retirement, never the mount envelope:

```pascal
LEntry := LInput.BeginCellRangeStroke(LPhase, LPointer, ncrpReplace, LPending);

if LEntry <> nil then
begin
  try
    ResolveActualCells(LPhase, LOrigin, LEndpoint);
    LEntry.ResolveCells(LOrigin, LEndpoint);
    LEntry.Prepare;
    LAllowDefault := DispatchOriginalPointerMessage(LEntry);
    LEntry.Complete(LAllowDefault);
    LPending := LEntry.PendingStroke;
  finally
    LEntry.Retire;
  end;
end;
```

Here `ResolveActualCells` belongs to the target adapter, and `LAllowDefault`
comes from that same message's synchronous canonical dispatch. The mount keeps
cached preview membership separate from its public accepted `CellRange` and
ordinary row selection. Matching `AbandonCellRangeStroke(LPointer, LPending)`
withdraws capture loss/Escape without clearing the previous accepted range.
Other pointers and superseded subjects are inert. Refresh, edit suspension,
disable/re-enable, newer input and teardown retire the pending authority. An
entry retired reentrantly during focus cannot resume its old default. A focus
failure remains the first diagnostic when repaint cleanup also fails.
These mount APIs alone do not enable a new default gesture.

The browser's optional `INyxBrowserCollectionCellRangeStrokeInput` now uses
that entry for each original Down/Move/Up dispatch. Successful Shift-down first
retains its ordinary immediate endpoint publication. The optional synchronous
`INyxCollectionCellRangePointerReceipt` distinguishes that installation from
merely claimed or consumed input; only a still-current installed endpoint arms
the pending extension. The adapter retains the original event-generation flags,
not its router or input envelope, and requires actual pointer capture. Movement
uses fractional CSS-pixel hit testing because capture retargets `event.target`;
only exact owned resident cells qualify. Release focuses and commits the admitted
rectangle. Wrong pointers do not control the stroke; capture loss, cancellation,
Escape, refresh and teardown withdraw its preview. Leaving owned data cells
cancels this current bounded gesture rather than constructing an invalid endpoint.
Automatic scrolling and off-page extension are not implemented.

An explicit canonical capture/release directive takes precedence over that
optional default. Capture refusal preserves the already accepted Shift-click.
Plain clicks retain ordinary row selection, and pen/touch still use their prior
endpoint behavior rather than implicitly acquiring the mouse drag contract.
The expanded browser control fixture injects only pointer capture authority;
its real DOM hit testing and canonical dispatch remain available for desktop/
compact execution. Current evidence is compilation, not browser execution or
trusted device capture.

The native adapter keeps Down in the grid's existing pre-focus prepared port.
Its optional `INyxLCLCollectionCellRangeStrokeInput` admits Move/Release, retaining
only original generation flags, detached pending data and its borrowed capture
surface between messages. Actual pinned-pane forwarding still reports logical
outer-grid coordinates; hit testing translates the sender's coordinate system
and accepts only an exact owned native grid cell. Existing row/editor defaults
stay shielded through the managed press's move/click/up tail. Native preview
paint reads copied row/field membership and flags, without querying the dataset.

LCL captures a source before its MouseDown callback and normally releases it
before MouseUp. The adapter adopts that default pair and defers only that
release's capture-loss cancellation. Independent loss cancels immediately.
Explicit canonical capture can transfer ownership without old range cleanup
releasing the caller's capture. The original source still retains its ordinary
mouse-up release behavior. The shared retirement hook releases exact physical
ownership when a model stroke is withdrawn, including a deferred refresh;
accepted paint is restored while public accepted selection remains separate.
Disabling retains its pre-existing range-clear policy.

The native09 control journey passes1254 checks at measured900 and1252 at390
client widths, each with a complete clean heap. Both ordinary tables and arranged
tables use actual LCL Down/Move/Up messages, physical hit testing and native
capture; no OS cursor movement or trusted hardware is claimed. Narrow pin
suspension remains the existing presentation policy. Tests reveal the launch
before capture through ordinary horizontal scrolling and use a reachable release
cell; automatic scrolling and off-page dragging remain unimplemented.

Native initial Down now validates the source's client clip and actual cell bounds
after reserving the original input. An outside press remains claimed but has no
resolved endpoint: the canonical callback still runs once, while clamped native
coordinates cannot move focus, install cells or invoke ordinary row/edit defaults.
Header/editor/default-off behavior remains on its existing route.

The later edge03 journey passes1506 native checks at900 and1504 at390 client
widths, with complete clean heaps. It adds the four outside edges, Escape and
native CancelMode during pending movement, and direct/callback retirement of a
live captured owner. Retained range interfaces cannot revive that owner. Fault
diagnostics remain on subscription execution receipts, consistent with Nyx's
ordered fault isolation. A failing callback that consumes input withdraws the
pending default; a failing callback that leaves it unconsumed allows it, and
later subscribers still execute. Neither capture nor range cleanup replaces the
original receipt diagnostic. These are LCL message/hit/cache checks, not trusted
hardware or raster evidence. Browser execution, canonical cancellation from every
physical source, pen/touch and full gesture/parity acceptance remain open.

Current range/command fixtures pass 47/127 checks on both actual targets. Real
controls pass393 native checks with a complete clean heap and465 browser checks
at measured1280x900 and390x900, with empty logs. These include exact Unicode/NUL,
view/getter reentry, ordinary/arranged tables, pinned paint and same-slot copy
with injected host authority. The later keyboard11 consumer retains the original
journey and passes611 native checks with a complete clean heap. Its browser
artifacts are byte-identical to the executed10 consumer, which passes617 at each
independently preflighted1280x900/390x900 width with empty logs. These checks cover
consumed input, single completion, refresh, stale endpoints, ordinary/pinned
tables and retirement during a native cursor setter. Actual LCL Click entry
still delivers the ordinary callback once outside internal transitions; internal
movement and refresh deliver none.

The separate `--probe-native-pointer` fixture option reports unavailable when
Windows cannot provide cursor positioning. Its retained unavailable result
grants no hardware-pointer coverage. The later managed-menu consumer passes670
native checks with a complete clean heap and701 browser checks at each measured
1280x900/390x900 viewport, with empty logs. Portable command checks pass156 on
both actual targets. These retain the earlier functional assertions and exercise
real menu invocation, original-range retirement, launch-cell membership,
admission reentry and focused-editor isolation with injected clipboard authority.
The initial fixture's absent-control lookup and optional-scheduler failures are
retained separately. Pointer/touch gestures, authored action-policy integration
and selected-row menu choices remain required. These results do not establish real
OS clipboard effects, native qualification at two widths or complete workbench
acceptance.

The maintained `nyx_table_workbench_application_controls` consumer compiles beside
Studio's unchanged generated Service monitor. It now passes25 native checks per
wide/compact width, with clean heaps: keyboard and managed-menu range Copy, hidden
selected-source IDs, independent runtime stores/ranges/query and one-sided
application retirement. Its real LCL clipboard adapter uses an explicit recording
publication target; this adds no OS clipboard or hardware-input evidence.
The same MCP-built browser application transfers exact RAW rectangles through its
actual menu at both widths, with independent runs. The initial automated browser
shortcut leaves the clipboard unchanged. The focused
`nyx_table_workbench_keyboard_browser` consumer now identifies why: the browser
driver emits a standard Copy event rather than a DOM copy key. The new optional
`INyxBrowserCollectionCopyInput` adapter facet reserves the same original command
for a cancellable Copy event on the exact focused resident cell. It uses existing
policy, source/input fences and the single configured host, with no invented key/
Click callbacks or direct event clipboard-data writes. Active editors/custom
descendants retain their native Copy route. Default handling is claimed only for
that original data command, and the exact listener detaches with its binding.
The same generated application's actual Copy now writes its exact173-byte RAW
rectangle through the real host at both widths. The explicit-adoption
prerequisite is accepted, with its original failure retained. Its separately
labeled Pascal synthetic producer reaches the original keyboard command and
writes the exact173-byte range through the real browser host at both widths,
without Nyx key subscribers. This establishes synthetic routing and real host
completion, not physical keyboard/assistive input or full workbench acceptance.
Recording is the default no-OS-effect mode; explicit `?browser` uses the real
adapter and `?unavailable` exercises the existing missing-service host. The helper
retains bounded key/host/result observations and explicit retirement. The admitted
unavailable keyboard result is also the starting case for
[default Copy feedback](../TODO/DONE/NS-3_collection-copy-feedback_01.md). See
[the current work](../WORK.md#current-work--table-copy-feedback-return--2026-10-30).

### Ordinary Table Copy feedback

Renderer-managed Tables provide a default result surface when clipboard actions
are configured. It reports the original controller's pending state and typed
Written, Unavailable, ActivationRequired, Denied, Cancelled or Failed receipt.
A new request clears the previous terminal message. Silent withdrawal displays
an interrupted completion; it never manufactures a host cancellation or success.
Host explanations remain plain text. Visible control characters use the existing
cell-inspection notation; the copied result retains the exact original text.

The optional `INyxCollectionClipboardFeedbackMount` exposes copied
`TNyxCollectionClipboardFeedback` state and `ClipboardFeedbackEnabled`, which is
enabled by default. A caller may hide the default surface and present that state
elsewhere. The admitted caption space stays allocated when hidden, preserving
data geometry and live input. The facet is optional: an alternative mount's state
support alone does not claim a visual surface. Existing interfaces keep their
GUIDs and methods.

The browser uses a focusable, politely announced status output inside the Table
caption. Native LCL uses a readonly, keyboard-reachable text control in the same
header band as column/recovery actions. Long explanations scroll inside that
bounded surface. Reading it does not perform data-cell navigation. Completion
changes no accepted rows, row/range selection, input epoch, editor text or caret;
it does not refresh the row presentation. The reserved height does not grow when
the outcome changes.

Native actions and feedback share a row when its measured client width leaves
at least 160 logical pixels for the status. Recovered-edit controls can move
feedback to a second row; removing the last recovered edit reclaims that space.
Resizing and right-to-left presentation reposition the complete composition
together, including tables without column arrangement. Text size and the full
scrollable diagnostic remain intact. This allocation depends on available width
and actions, never on a particular clipboard outcome or diagnostic length.

Browser caption feedback, pin guidance and column/recovery commands share one
bounded toolbar row. The toolbar follows the real clip without widening authored
columns; exceptionally narrow allocations scroll its commands. Status text keeps
its complete accessible value and internal scrolling. Sort and Filter share a
header row while the authored header-line policy and full accessible names remain
in force. Filter uses a theme-colored decorative glyph; names/tooltips stay complete
and header actions retain usable minimum hit areas. These arrangements preserve
readable data space in short tables without
changing the authored height or reducing the data font.

This separate borrowed presentation receiver preserves application observers,
menu replies and the original single host request slot. Retirement withdraws it
before cancellation or target teardown. Original serial/readiness checks prevent
stale or cancelled work from changing a newer or replacement mount's outcome.
Presentation failure is a mount notification diagnostic and cannot turn an
accepted service assignment into an admission failure.

The feedback qualification executes318 shared command checks and55 model-mounted
admission/retirement checks per target. Full control journeys pass1683 in measured
1280/390 browser viewports and1868/1857 native, with clean native heaps. The same
saved Service monitor passes28 native checks per width, including visible status
and retained keyboard focus. Its real browser host writes the exact173-byte RAW
range at both widths and reports an actual activation refusal separately; an
MCP-built observing Studio preview displays Unavailable. This is browser-driver
and native widget-message evidence, without physical/assistive-input claims.

## Saving table workbench action choices

Canonical Table context callbacks also expose an optional owned
[`CollectionCell` snapshot](events.md#table-context-cells), independent of selected
membership, display text and drafts. Use its scoped item, typed field, accepted
scalar and original revision when deciding which application action to offer.
The snapshot retains no live view or future command authority. Existing managed
cell actions and clipboard policy continue through their original envelope.

`nyx.collections.workbench.policy` supplies a pure authored policy. Apply it to
the existing typed table binding; clipboard services and live input state belong
to the application, separately from this saved value.

```pascal
LTasksTable.Binds.Collection(
  NyxCollectionView(NyxCollection('tasks'))
    .Column(NyxTextField('title'), 'Task', cmEditable)
    .TableWorkbench(NyxTableWorkbench
      .RangeSelection(True)
      .AllowCopy([nwcaCell, nwcaSelectedSource, nwcaSelectedPage, nwcaCellRange])
      .CopyFields(nwcfAllArrangement)
      .Headers(False)
      .LineEnding(nwleCRLF)
      .FocusedDraft(True)));
```

Declare `LTasksTable: INyxTable`; import `nyx.controls`, `nyx.collections`,
`nyx.collections.view.types` and `nyx.collections.workbench.policy`. The four
copy flags permit accepted-cell, selected-source-row, selected-current-page-row
and selected-cell-range transfers respectively. Range selection and deliberate
focused-draft context are independent opt-ins: permission to copy a range does
not enable selection, and an empty copy set does not disable readonly inspection.
Ordinary browser/LCL renderers now adopt these choices when a runtime table is
mounted. Design-mode projections retain authoring behavior.

Set the renderer's `TableWorkbenchServices` before rendering to provide an
`INyxTableWorkbenchServices` from `nyx.collections.workbench.runtime`.
`NewNyxTableWorkbenchServices(LClipboardHost)` supplies explicit clipboard
authority; its second overload also takes bounded copy limits. The renderer
retains these application services separately from the saved document. Replacing
them while a tree is mounted refuses; explicitly unmount first. Missing services
produce a visible typed Unavailable outcome while readonly inspection stays
usable. No saved policy or generated design acquires OS authority by itself.

Managed context menus distinguish selected rows from the complete source and
selected rows on the current page. Their original membership/column snapshots
are captured before outward input callbacks. A selection, source, query, page
or column change retires that row proof, including change-and-restore; an old
action visibly refuses instead of substituting current rows. Range and RAW-cell
actions retain their independent readiness and permissions. All transfers share
the existing controller request slot.

Selected records also have explicit **Copy selected source record IDs** and
**Copy selected record IDs on this page** actions. These use the corresponding selected-row
permission and membership, headers/line-ending choices and runtime bounds.
`nyx.collections.identities` returns `INyxCollectionIdentityText`: owned scoped
references, exact ordered ID text, source revision and representation metadata.
This has no invented schema field or cell value. The optional
`INyxCollectionCellIdentityActions` exposes its distinct payload/result without
overwriting earlier RAW row/cell facts. Stale membership visibly cancels the
original ID action while inspection remains useful. Identity and ordinary copy
still arbitrate through one pending-request slot.

Application services may additionally provide `INyxTableWorkbenchExportServices`.
Use the `NewNyxTableWorkbenchServices` overload taking clipboard host, explicit
`INyxFileSaveHost` and copied `TNyxCollectionExportOptions`, optionally followed
by copy limits. The bridge configures export before managed actions. Without
an authored export policy, absence keeps exports absent. A supplied nil save host refuses, and an unavailable host
retains its typed outcome. Page and all-filtered exports keep the existing RAW
producer, cancellation and Written/DownloadRequested distinction. None of these
runtime services or options enters the saved design.

Authors can save independent export choices alongside the copy policy:

```pascal
LTable.Binds.Collection(LView.TableWorkbench(NyxTableWorkbench
  .Export(NyxTableExport
    .Allow([nweaCurrentPage, nweaAllFiltered])
    .Columns(nwcfVisiblePresentation)
    .Headers(True)
    .LineEnding(nwleCRLF))));
```

`TNyxTableExportPolicy` describes accepted RAW TSV, independently of clipboard
headers, columns and row separators. Current-page and all-filtered permissions
are separate; neither means selected rows. `.Allow([])` explicitly disables both
managed actions even when the application supplies file authority.
`.UseDefaultExport` removes only the nested export choices and restores the
application's existing export configuration, preserving the copy policy.

Runtime membership limits intersect the authored actions. Save hints, file hosts
and byte/work limits remain application-owned. An explicitly authored action
without file services remains available and returns `nfsoUnavailable` through
the ordinary inspector status. It never implies filesystem access. The optional
`INyxCollectionExportPermissions` exposes copied membership flags to managed
menus without changing older exporter interfaces; older implementations retain
their previous page/all-filtered behavior. Denied actions are also refused at
command/serialization admission, before invoking file authority.

The nested export value has its own strict version-one representation. Its
presence uses workbench policy version two; removing it reproduces the original
version-one policy bytes. Version two requires a real export object, including
explicit empty permissions. Generated Pascal uses typed fluent builders, and
the confined source reader rejects strings or symbols from another enum family.

Optional `INyxCollectionInputPreparation` synchronizes only the base mount's
accepted Enabled flags before the bridge reserves its input epoch. It invokes
no application or widget callback. Subsequent scheduler/service reentry cannot
disable/enable a table and have its new epoch adopted as the original request.

The complete retained runtime07 fixture passes968 native checks with a clean
heap and1073 browser checks at measured1280x900/390x900, logs empty. It exercises
actual controls with injected clipboard authority and synthetic target input.
Real OS clipboard effects, hardware gestures, the full Studio journey and
complete workbench acceptance remain separate requirements.

`NyxTableWorkbench` defaults to range and focused-draft opt-ins off, the first
three copy flags on, `nwcfVisiblePresentation`, headers on and `nwleLF`.
`nwcfAllArrangement` includes every authored arrangement field, including hidden
columns; it does not add schema-only fields absent from that arrangement. LF or
CRLF controls TSV row separators, preserving embedded cell line endings and
ordinary RAW cell text. These choices never select a clipboard host or save
runtime work limits, promises, selections, drafts, receipts or machine paths.

Absence preserves the existing unconfigured table behavior.
`.TableWorkbench(NyxTableWorkbench)` records explicit default presence;
`.UseDefaultTableWorkbench` removes only that policy, retaining the collection
binding, column layout/format/query policies and reusable peers. A local binding
with its policy reset remains local. `Binds.ClearCollection` records a whole
unbound mask; `Binds.InheritCollection` restores the reusable definition instead.
The policy is table-only once its actual projection is resolved.

Bindings with a workbench declaration use descriptor version10. Removing it
preserves the applicable earlier descriptor bytes, including versions1–9.
Generation emits typed fluent calls and adds the pure policy unit only when
needed. Source synchronization preserves handwritten prefixes, suffixes,
comments and imports; resetting a policy does not remove an existing import
that handwritten code may still use. The confined source reader round-trips
the closed enum/Boolean families, explicit defaults, empty permission sets,
local reusable bindings and masks without executing host callbacks.

Studio's table defaults include a collapsed **Inspection, selection, copy and export**
section. Its complete typed form edits range selection, the four copy actions,
included columns, headers, line endings and deliberate focused-draft inspection.
The export controls separately choose whether to save export settings, permitted
memberships, columns, headers and row separators. Clearing **Use saved export
choices** restores runtime defaults without resetting the other action settings.
**Apply actions** creates an explicit policy; **Use Nyx defaults** clears only
its presence. The existing layout draft owner and source queue carry the action,
and one Undo restores the accepted design/source pair. An old binding/schema,
pending structural edit or pending Pascal draft refuses before publication.

The existing `nyx_collections` semantic tool accepts optional
`workbenchControls: true` on a bounded `mode: "bindings"` read. Each requested
local/effective/restorable binding reports `{declared, policy}` separately from
`workbenchControlsSupported`. Omitted/false preserves the previous response;
the closed column-read shape does not accept this table-level selector. This
metadata describes saved intent and resolved defaults, never live selection or
application clipboard authority.

For grouped edits, use `NyxSetCollectionTableWorkbench` or
`NyxUseDefaultCollectionTableWorkbench` in the existing typed collection patch.
The semantic `table-workbench` operation carries owner/key/table projection and
one complete policy, or `null` for a local default reset. It derives the current
binding baseline after preceding changes in that same group. Expected revisions,
exact operation retries, atomic refusal and paired Undo/Redo retain their existing
meaning. Resetting a masked reusable binding refuses; explicit inheritance is
still a separate operation. A table action policy on a non-table bind refuses
at typed construction as well as in schema discovery and runtime admission.

Authoring05 passes33 native/browser model and semantic checks; actual native MCP
discovery passes204. The unchanged representation consumer passes76 per actual
target. Native heaps are complete/clean and browser logs are empty. These prove
portable form capture, strict admission, source/history and semantic behavior;
mounted Studio input and the full native Studio/own-export journey remain open.
The maintained `query-workbench` build includes the pure policy/source consumers
and independently compiles this run's exact emitted companion on both targets;
browser execution remains a separate check.

## Exporting captured table results

`nyx.collections.export` prepares immutable RAW TSV from an actual
`INyxCollectionExportCapture`. Its source, complete filtered/ordered result,
current page and column arrangement belong to one accepted view cohort.
`ncerCurrentPage` and `ncerAllFiltered` select those explicit memberships;
neither follows mutable selection or silently includes hidden selected records.
`TNyxCollectionExportOptions.Defaults` uses visible presentation fields, exact
field-name headers and LF separators. Call Fields, Headers, LineEnding, Limits
or SaveAs to derive another typed policy. All-arrangement fields deliberately
include hidden columns. Text preserves Unicode, NUL and original cell line
endings; numeric/Boolean values retain their canonical accepted spelling.
Escaping follows the same TSV rules as row copying, without a final terminator.

`NewNyxCollectionExportText` admits row/field/work/UTF-8 limits before file
authority, verifies every result row against its exact accepted source and owns
copied identities, scalars and bytes. An empty result produces headers only,
or zero bytes when headers are disabled. Bytes returns an independent array.
Formatted display and unfinished edit drafts are separate representations;
neither substitutes for RAW output.

The optional `INyxCollectionExportMount` in `nyx.collections.export.actions`
injects an existing `INyxFileSaveHost`, event router, UI scheduler and runtime
options through ConfigureExport. Configure it before managed cell actions to
expose separate current-page/all-filtered context choices. It preserves older
mount/view interface GUIDs; saved designs never carry host authority. Prepare
captures the original input/query/page/columns before canonical callbacks. A
later accepted source/query/page/layout change refuses that original intent
rather than exporting a newly recaptured result.

The controller has one pending save slot. New accepted saves withdraw an older
receiver, while ignored/stale inputs leave it alone. Disable, suspension and
retirement cancel its exact pending token; a late receipt cannot update a newer
session. Results reuse `TNyxFileSaveResult`, so Written, DownloadRequested,
Cancelled, ActivationRequired, Unavailable and Failed retain their different
meanings. A requested browser download is never reported as a confirmed disk
write, and receiver cancellation cannot undo an already-entered platform effect.
The managed inspector names the captured **current page** or **all filtered
rows** in both pending and terminal export messages, including cancellation,
unavailable hosts and activation refusal. That scope is copied into the original
reply before host entry; completion never reads a newer page to label an older
request. Download-requested text still describes a request rather than a disk write.
Current control qualification uses injected hosts on both targets; actual OS
effects and complete workbench acceptance remain separate.

## Viewport-sized table paging

Bound browser tables present accepted values as readable cell text. Retained
input objects stay hidden outside the active cell's edit mode, preserving their
identity and drafts without exposing every field in the page's Tab order.
Enter/F2 enters the current eligible editor; Escape restores its accepted value
and returns to cell navigation. Editor Tab traverses eligible columns and leaves
the composite at its boundary. Trusted double-click enters the addressed browser
cell; ordinary blur commits through the typed view/store boundary. Disabled,
read-only or removed/filtered membership cannot retain active editing authority.
Native LCL continues to use its standard grid values and owned cell editor.

Browser cell/editor styling uses Nyx's semantic theme tokens. Column headings
remain visible in a scrolling table; measured header occlusion is excluded from
the realization/paging plane, with logical scroll margins for focused cells.
The current [WAI grid pattern](https://www.w3.org/WAI/ARIA/apg/patterns/grid/)
separates navigation from interaction within a cell. Checked 2026-10-12 alongside
the [HTML focusing model](https://html.spec.whatwg.org/multipage/interaction.html#focus).
Host tests qualify Chromium defaults; full screen-reader, hardware/IME, other
widgetset and complete visual parity acceptance remain open. See
[the readable-cell packet](../WORK.md#current-return-path-readable-bound-table-cells--2026-10-12).

Bound tables now handle Page Up/Down through shared closed `ngmPageUp` /
`ngmPageDown` movements. A page uses the actual visible data rows with one row
of overlap, advancing at least one row and clamping at the dataset's ends.
The current column stays fixed. Normal paging selects its destination; Control
moves focus without replacing membership; Shift extends the existing row range.
Canonical before-key callbacks may consume the default. Active cell editors keep
their draft and row; native forwarding no longer commits a draft by paging away.
Disabled controls refuse forced input. These are runtime choices; source defaults
and paired history remain unchanged.

Browser steps use the same clipped/scaled measured geometry as virtualization,
excluding overscan and retained offscreen drafts. LCL uses its actual visible-data
row cache. A resize changes the next page length. The public pure movement helper
accepts a positive `APageRows` for page moves and clamps before integer arithmetic;
other movements preserve their previous contract. No persisted row index or
dataset pagination/filter is introduced.

Current source passes 3606 movement checks on each target, 49 native / 70 executed
browser control checks and 36 trusted Chromium input checks, including short/tall/
CSS-390 allocation. The English forty-row companion is composed through MCP with
one paired Undo/Redo; MCP builds its exact view on both targets. Artifact admission
proves compilation separately from executed control fixtures. Native/table visual
polish, cell/column selection, other widgetsets and assistive technology remain open.
This consumes the current [WAI grid paging guidance](https://www.w3.org/WAI/ARIA/apg/patterns/grid/)
(checked 2026-10-11), without claiming full pattern conformance. See
[the evidence and limits](../WORK.md#current-return-path-viewport-sized-data-grid-paging--2026-10-11).

For a fresh explicitly owned companion, the maintained Pascal tool accepts
`--paging` after its existing configuration and new source-directory arguments.
`--build-paging` reads that existing export and submits both view jobs; configure
machine outputs independently before that build step. Neither mode replaces the
primary project. Reproduce the exact exported control checks with:

```powershell
./tools/build.ps1 -Target grid-navigation -GridSourceDirectory <exact-mcp-export> -GridEvidenceDirectory <new-evidence-directory>
```

The build stages explicit-start browser hosts and the trusted input observer;
HTTP browser execution remains a separate step. Omitting the new evidence option
retains the conventional build directory. An older four-row export still runs its
original navigation journey; the forty-row companion adds the paging journey.

`NyxCallbacks(LTasksList).OnSelectionChange` authors multiple ordered handlers and
their normal scheduler policy. Runtime subscriptions use
`LEvents.OnSelectionChange(NyxControlEvents('tasks-list')).Subscribe(LCallback)`.
Callbacks receive `HasCollectionSelection=True` and immutable typed
`SelectionBefore`/`Selection` value snapshots, including focus and anchor. Value
snapshots accommodate the matched pas2js compiler's restriction on managed
interface fields in records. The live view still returns a managed interface.
Notifications follow accepted publication, cannot consume it, and retain semantic
compound source/origin routing. Teardown revokes borrowed producers before widgets
are freed. Studio's Bindings choice, Events cards, source templates, paired
undo/redo and bounded semantic agent event discovery consume the same contract.

The ordinary edit path is typed:

```pascal
LTasksView.Edit(LTasksView.Selected, 0,
  TNyxStateValue.FromText('Review the interface'));
```

The supplied value must match the column family and the declared store domain.
`Apply` admits a complete batch before publication. `EditWire` and attachment
`EditCell` are explicit adapter boundaries: text from a target editor passes
through the matching scalar parser and domain before changing accepted state.
`TNyxCollectionColumn.Read(Item)` reads that column's exact typed scalar; missing
fields or incompatible families reject. Adapters compare accepted scalars per
cell, preserving an unfinished draft across selection and unrelated publications.
Completed no-op or rejected edits restore only their addressed item/column.
Browser editors retain focus through keyed row relocation; native editors retain
their draft when selection or another row's values change.

An attachment exposes `Failure` and `ErrorText`. Rejected input returns false
and restores the accepted display. An observer failure after publication reports
`nbfNotificationFailed` and returns true, because the change was committed.
Successful edits clear the previous diagnostic. Notification failure never
promises a rollback of an already published snapshot.

Browser text retains embedded NUL directly. LCL displays text containing NUL as
a JSON-quoted string and decodes its edited representation at the adapter
boundary. Other text is displayed normally. Exact Unicode remains in the store.

## Hierarchies and reusable instances

An empty parent value identifies a root. Child rows may precede their parents.
Every nonempty parent must resolve in the same collection, and the complete
candidate must be acyclic. An iterative validation walk avoids recursive Pascal
stack growth. An atomic batch may remove a subtree or change several parents;
admission evaluates the final candidate before any control update.

`NewNyxCollectionContext` captures independent authored defaults and exposes an
application registry. `Resolve` uses a view specification and the actual runtime
reusable-owner ID. Application scope returns the shared application store.
`.Scoped(csInstance)` resolves an independent store for each exact owner/key
pair, seeded from authored defaults. Repeated resolution reuses that store;
other instances and application mutations do not change its defaults. Empty
instance owners reject, and a context admits at most 1024 local owner/key pairs.
Composition propagates the nearest reusable owner's runtime ID automatically,
including nested instances and override payloads. A standalone page/view uses
its root ID as the local owner. Scope IDs are runtime metadata and never enter
saved designs or generated source. Manual `Resolve` remains available for custom
runtime consumers.

Applications retain one `INyxCollectionBindings` controller per page in a shared
context. Hidden pages continue validating data; navigation preserves edited
application/instance stores and each view's selected item. A standalone renderer
creates an independent context. Supplied controllers must match the realized
IDs, scopes, specs and projections before replacing accepted controls.

## Prepared model publication

Built-in stores offer optional `INyxAtomicCollection`; the original collection
interface/GUID is unchanged. Views prepare complete query, selection and
hierarchy candidates before installation, including for ordinary Assign/Apply.
Reserved models reject competing dataset/view/subscription commands. Independent
prepared stores can join `PublishNyxGroup` so a later invalid view preserves all
models and the first notification reads every accepted store/view. Borrowed
projection installers adopt preallocated data; target mounts synchronize during
notification. Extension installers/retirement must be nonthrowing. This does not
promise simultaneous physical paint. See [the resource group contract](resources.md#coordinated-runtime-row-publication)
for ownership, refusal and explicit resource-backed table usage.

## Ownership and target behavior

Views retain their stores and accepted snapshots. Their store subscription and
observer tokens disconnect deterministically without a reference cycle. Multiple
observers execute in registration order; cancellation can suppress a later
observer in the same dispatch. Mutations and new subscriptions during dispatch
reject. A nil change argument denotes selection, query or tree disclosure. Receivers
are borrowed: disconnect their tokens before disposing receivers.

An attachment retains its view and borrows its target control. The renderer
disconnects attachments before removing controls during unmount. Retained
attachment interfaces then remain safe to inspect and disconnect, with
`Connected = False`; accessing their released view rejects. A new render needs
a new attachment; application controllers retain their view through navigation.
Only one collection attachment may own a mounted control. `CollectionView(ID)`
returns a retained automatically mounted view by exact runtime identity.

Root/ancestor enabled and read-only settings propagate into generated editors.
Read-only permits selection while rejecting edits; disabled controls reject
selection and editing. Browser design mode disables data editing. Physical
callbacks enforce this policy even when invoked programmatically; application
code can still edit its independent store through the typed runtime API.

## Typed runtime tree disclosure

Bound trees expose a managed optional `INyxTreeHierarchy` through their ordinary
runtime collection view. Alternative view implementations may supply that
interface; `NyxTreeHierarchy` refuses absent/non-tree/unsupported views without
changing the existing `INyxCollectionView` interface.

```pascal
LHandbookView := LRenderer.CollectionView('handbook-tree');
LHandbookTree := NyxTreeHierarchy(LHandbookView);
LDocumentation := NyxItem(NyxCollection('handbook'), 'docs');

LHandbookTree
  .SetExpanded(LDocumentation, True)
  .ExpandAll;

LVisibleSections := LHandbookTree.VisibleItems;
```

Branches start closed. Exact scoped identities retain disclosure through source
moves/reparenting and temporary query displacement. Removal retires disclosure,
including an admitted grouped remove/reinsert; a no-op store batch does not
publish a removal. Leaves expose no disclosure and expansion commands on a leaf
are silent. Commands require an item in the current query result, which can
include a child hidden by a closed ancestor. `ExpandAll` opens current query
branches in one publication; `CollapseAll` also clears parked source branches.

`CommandSerial` is a monotonic stamp for accepted disclosure/explicit-focus
commands, including silent no-ops. It is independent of document/store revision.
Invalid/reentrant commands do not advance it; exhaustion refuses before mutation.
Native queued physical proposals compare that stamp before publishing, so a later
application `CollapseAll`, even an unchanged one, cannot be overwritten by an
older host expansion. Authoritative view refresh and disconnect revoke pending
physical proposals. Physical native disclosure publishes on the next UI queue
turn after LCL returns from its node stack; browser disclosure uses its queued
DOM toggle event. Direct typed commands publish synchronously on both targets.

`VisibleItems` returns independently owned identities in sibling-stable preorder,
including datasets with children before their parents. Adjacency construction
and traversal are iterative; a deep tree does not use recursive call-stack space.
Disclosure owns logical state independently of realization. Current browser
controls realize a measured disclosed-row viewport and its required ancestors;
native retains the complete standard LCL tree with clipped painting. Collection
lookup/query/model costs and production budgets remain.

Collapsing an ancestor returns a descendant cursor to its highest visible closed
ancestor, retaining selected membership and anchor. Explicit `Select`/`SetSelection`
focus opens ancestors before the same publication. A source/query refresh instead
returns a hidden cursor to its visible ancestor. Descendant disclosure remains
remembered when an ancestor closes. Selection and disclosure are independent
runtime state; neither changes document defaults, saved designs, generated source
or authoring Undo. Controllers retain the view to retain this state through
navigation; a newly materialized runtime starts independently.

Both adapters consume this public state for physical disclosure and navigation.
Right opens a closed parent without moving focus, then moves to its first child;
Left closes an open parent or moves a closed child to its parent. Visible ranges
and typeahead use the same portable preorder. This follows the
[WAI tree keyboard pattern](https://www.w3.org/WAI/ARIA/apg/patterns/treeview/).
The browser exposes `aria-expanded` only on parents and returns physical focus
from a hidden row to the admitted visible entry. Native expansion slots are
restored on disconnect; disabled physical disclosure cannot change runtime state,
while read-only permits disclosure. Existing ordered view observers see one
publication; no-op commands stay silent, reentry refuses, and observer errors
report after publication.

Qualification (2026-10-07): the maintained exact MCP-exported English companion
passes 83 checked Win32/shared assertions, leak-free, including deep traversal,
query/move/reparent/removal, independent ownership, physical keys/disclosure,
disabled state, superseded/cancelled host proposals and whole-renderer retirement
inside physical disclosure publication. The companion's authenticated semantic
composition/source/paired Undo/Redo passes 37 on the existing combined-operation
service. The connected primary frozen server refuses combined collection steps;
its empty refused review is discarded. Collection view/control/selection
regressions pass 32/27/155. Matching pas2js consumer and both full Studios compile
with zero owned warnings. Browser checks are staged, **not executed** here;
synthetic DOM events would not establish trusted pointer/keyboard behavior.
Hardware/assistive technology, other widgetsets, visuals and full parity remain
unqualified. Literal unbound tree items have their existing host behavior;
this capability belongs to typed bound runtime collections.

Reproduce without starting or replacing a service:

```powershell
& tools/build.ps1 -Target tree-hierarchy -TreeSourceDirectory <exact-mcp-export>
```

Omit `TreeSourceDirectory` for the independent public Pascal fixture. The Pascal
consumer's `--semantic <existing-config.toml> <new-export-directory>` mode uses one
authenticated transport and a temporary owned review, with one grouped typed
composition and exact bounded source windows. It never edits enrollment, profiles
or primary user work. It requires a backend admitting combined collection steps.

## Tree matches, context and hidden selection

`nyx.collections.tree.inspection` supplies an immutable public inspection capture:

```pascal
LProcesses := CaptureNyxTreeInspection(LProcessView);
LFacts := LProcesses.Observation;

if LFacts.MatchedRows > 0 then
begin
  LOutcome := RevealNyxTreeItem(LProcessView, LProcesses.MatchAt(0));
end;
```

Capture on the view's UI thread. `MatchedRows` satisfies the complete effective
predicate, including authored and peer contributions. `ContextRows` counts
included ancestors which do not themselves match; `ResultRows` is their sum.
`DisclosedRows` counts the logical open preorder, not painted/realized rows or
viewport capacity. Source counts use the displayed source facet, not a fresh
lookup against a potentially advancing producer. Alternative views must supply
that facet and a coherent hierarchy/snapshot/selection. A changed capture refuses.

`RoleOf` distinguishes match/context/absence. `VisibilityOf` distinguishes a
missing source identity, a filtered identity, a closed branch and a disclosed
row. `FilteredSelectedRows` and `CollapsedSelectedRows` are disjoint counts;
selected ancestors included for context are still present. `MatchAt` uses query
ordering, includes closed descendants and rejects invalid indices. Retained
captures own immutable source/query snapshots and disclosed metadata until
released; they retain no live view, store, widget, receiver or command authority.
Their logical source payload remains retained even after the application closes.

`RevealNyxTreeItem` explicitly focuses an admitted item and opens its ancestors
through the optional `INyxTreeReveal` view facet, preserving exact membership and range anchor.
Its typed outcome reports missing/filtered/revealed. It never relaxes a filter,
selects a substitute or restores an older disclosure/selection snapshot. A foreign
or undefined identity refuses. The caller still owns its normal UI/generation
authority, and observers retain ordinary post-commit semantics. Each admitted
call includes fresh viewport intent even when logical focus/disclosure are
unchanged. The default `ntrfPreserve` keeps keyboard focus in the caller;
`ntrfTake` deliberately transfers it to the Tree:

```pascal
LOutcome := RevealNyxTreeItem(LProcessView, LProcess, ntrfTake);
```

`INyxTreeReveal.CaptureTreeReveal` returns copied one-shot intent. Its serial
belongs to that exact view, never another instance or mounted generation. The
capture must read owned facts without application callbacks. Source revisions,
query changes (including ABA), later disclosure and ordinary focus commands
withdraw old intent. Notification failure follows committed navigation. A
disabled/suspended target does not gain authority by later enabling/resuming;
new target activation ignores historical requests. Existing input epochs and
successful refresh acknowledgement govern consumption. Failed/deferred target
work can retry the same admitted request; ordinary later refresh does not replay
success or pull a reader back after deliberate scrolling.

Native explicit requests call the standard Tree node's `MakeVisible` after
disclosure/EndUpdate, then optionally take focus. Browser requests realize the
target/path and use nearest instant scrolling through the Tree and surrounding
scroll containers. Queued measurement corrections check the same current intent;
scrolling away or superseding it withdraws them. Raw DOM option keys remain
inside the adapter. These operations establish navigation intent and target
delivery, not an eternal onscreen lock against later user/application changes.
Native long-tree checks execute; browser long-tree assertions compile/stage but
still require execution. Broader transforms/scroll containers, AT and physical
input remain distinct qualification.

The public [process-tree example](../examples/service-monitor/README.md#process-tree-inspection)
uses ordinary Nyx Input/Label/Button/Tree controls, one explicit search owner and
an independent peer view of the same collection. Clear removes only that owner's
predicate and clears its bound text in one grouped publication. It retains later
disclosure choices and surviving selected identities; another owner's accepted
filter can still hide a selected process. The separate Clear-and-reveal command
makes relaxation deliberate. Input labels distinguish membership search from the
Tree's keyboard typeahead. Native handwritten/generated controls exercise both
widths; matching browser artifacts build, while browser/AT/pixel and
full live-inspection acceptance remain open.

## Immutable tree rows and browser windows

Import `nyx.collections.tree.rows` to obtain `INyxTreeRows` with
`NyxTreeRows(LHandbookView)`. Each copied `TNyxTreeRow` exposes its exact scoped
`Item`, optional rootless `Parent`, query `SourceIndex`, one-based `Level`,
sibling `Position`/`Size`, `HasChildren` and `Expanded`. `RowAt` uses zero-based
disclosed ordinals and refuses invalid bounds; `IndexOf` returns -1 for undefined,
foreign, missing or collapsed items. Sibling sizes describe the complete current
query hierarchy, rather than the realized viewport.

The immutable managed projection retains only its query snapshot and copied
metadata, surviving later disclosure, query changes and view retirement. It never
retains a view, store, document, widget or subscription. Construction validates
parent bounds/cycles, disclosure and the exact preorder, then rechecks snapshot
identity and command stamp before returning. Incoherent alternative providers
refuse without publishing or mutating the view. Depth traversal is iterative.
`CommandSerial` records this read; it is not a view identity. A mount caches rows
only beside its independently owned view, comparing snapshot identity and stamp.

Browser realization retains viewport rows, a logical Tab entry, navigation
destinations and their ancestor closure. Real parent treeitems still own nested
`group` elements. Accessibility-hidden numerical spacers represent only omitted
disclosed intervals. Closed branches create no physical descendant entries or
spacers. Parent disclosure decisions use the model, never the count of currently
realized child elements. Explicit `aria-level`, `aria-posinset` and `aria-setsize`
follow [WAI tree guidance](https://www.w3.org/WAI/ARIA/apg/patterns/treeview/)
(checked 2026-10-12); this is not complete assistive-technology qualification.

Readable captions own navigation mode; F2/Enter or a double click enters the
eligible browser editor. Escape discards its unfinished value and returns focus
to the row; ordinary Tab leaves the composite. Closing an edited branch revokes
active editing while retaining an unfinished control, Unicode value and caret
outside the physical tree. Reopening restores that independently owned control.
Native uses its ordinary LCL label editor; see the entered-edit contract below.
Keyboard reveal uses the destination's
summary, rather than scrolling its complete descendant box into view.

### Native label editing and recovery

The first Tree column opts into editing through the same typed contract on both
targets. `Folders`, `FolderLabel` and `FolderParent` below are typed collection
and text-field references; no target class appears in the document:

```pascal
LHierarchy.Binds.Collection(
  NyxCollectionView(Folders)
    .Column(FolderLabel, 'Label', cmEditable)
    .Parent(FolderParent)).Done;
```

The Win32 LCL adapter retains the standard editor, Unicode scalar selection and
accepted field provenance around live refresh. Completion keeps LCL's borrowed
node alive until its native edit stack returns; physical remapping waits for UI
idle. Ordinary validator refusal retains accepted source data and reopens the
unfinished label for correction. An externally changed, removed, replaced or
filtered label uses the shared recovery journal instead of overwriting newer data.
Moving selection or closing an ancestor retains compatible text and caret as a
keyed dormant draft. Returning through F2 restores that work; it does not reopen
an old editor over a newly selected row. Escape explicitly discards the label.
Later source changes also admit dormant drafts through the same recovery journal.
Discarding one dormant pending receipt does not cancel a different active label.
Native composition observation delays incompatible refresh until the platform's
post-composition idle boundary. The application's ordinary UI loop supplies idle;
an in-process control harness must pump it as well as pending messages.

`NyxCollectionDraftRecovery(LMount)` supplies the same data-only journal used by
Tables. `NewNyxRecoveredDraftReview` composes a captured entry into an independent
Nyx Card; it grants no write authority. `INyxCollectionPendingDraftMount` exposes
an exact pending receipt when journal capacity refuses admission. Review/export
does not consume its text, and discard must match the current receipt. Ordinary
capacity pressure keeps the original physical editor. If a host has already
refused reopening, the pending receipt instead retains its detached draft.
Runtime edits/recovery do not change saved document defaults.

Deferred mounts keep revision scalars and bounded removed identities for the
still-physical frame, so explicit retry can recognize unchanged rows across
several producer updates. This uses the authored inspection identity limit, or
its 4096-entry default when inspection is absent; it does not enable pause/follow
or change the view's optional continuity capability. Change packets and producer
snapshots are not retained by this proof. A gap or exceeded bound remains unknown
and uses conservative recovery. Successful installation or retirement releases
the proof. After resolving a pending discard, explicitly call `LMount.Refresh`;
discard alone does not claim that the physical map has caught up.

The extended native editor journey and exact target evidence are recorded in WORK.md.
This contract description does not accept full navigation/context,
widgetset, assistive-technology or scale requirements of
the [live-inspection task](../TODO/NS-3_control-usefulness_04.md).

### Composable recovery panel and runtime binding

`nyx.collections.recovery.panel` supplies a specialized, reference-counted Card
made from ordinary Nyx labels, readonly Memos and buttons. Define its typed state
addresses before mounting, then bind that application's original collection mount:

```pascal
LRecoveryParts := NyxCollectionRecoveryParts('process-recovery');
LRecoveryParts.Define(LDocument.State);
LPage.Add(NewNyxCollectionRecoveryPanel(LRecoveryParts));

{ After mounting: retain beside the application; Close before disposing it.
  LSaveHost is the application's explicitly supplied platform file-save host. }
LRecovery := BindNyxCollectionRecoveryPanel(LApplication.Context,
  LApplication.View.Events, LApplication.View.CollectionMount(ProcessTreeID),
  LRecoveryParts, LSaveHost);
```

The panel occupies no space when its journal is empty and no edit is held. It
shows the item/field, admitted loss reason, source revisions, unfinished text and
accepted value separately. Preview escaping leaves raw text unchanged. Previous
and Next navigate admitted entries. **Review held edit** captures an exact pending
receipt without pretending it has entered the journal; **Review recovered edits**
returns to that list so the user can free capacity. Export never consumes either
kind of work. Discard requires a second confirmation, and a stale held receipt
cannot discard newer typing. Retry explicitly refreshes the same display after
capacity becomes available. These operations never restore drafts into source data.

The binding consumes `INyxCollectionRecoveryNotifications` from
`nyx.collections.recovery.status`, plus the mount's existing journal, input-lifetime
and pending-receipt facets. The bounded observation contains copied flags/counts,
not row vectors or editor objects. It updates after the physical refresh settles,
including a capacity decision deferred until native composition ends; no polling
timer is required. The factory refuses a mount lacking these capabilities. Tokens
borrow their receiver and are revoked before mount teardown. The public binding
owns event subscriptions separately from the router-retained callback to avoid
reference cycles; `Close` revokes callbacks and cancels a pending export first.

Use `ConfigureExport` with `TNyxFileSaveStreamOptions` to set the filename, media
type and byte limit. A streaming host receives an immutable UTF-8 byte source;
a byte-only host also obeys its smaller supported limit. Inline and pending
completions are supported. A missing host disables Export with an explanation;
review, discard and retry remain usable. Save outcomes distinguish a written file
from a requested browser download. This panel does not confer filesystem authority.

`TNyxCollectionRecoveryParts` exposes enum-based action, text and flag roles.
The specialized panel exposes its parts, while its named descendants remain
ordinary controls that an author can theme or rearrange. Only controls and state
defaults are generated; runtime bindings, save hosts and receipts remain explicit
application code. `NewNyxBoundRecoveredDraftReview` provides the state-bound review
Card separately, sharing the frozen review's layout without claiming its immutable
entry interface.

The public `process_tree` application opts into this panel. Its same handwritten
and generated consumers exercise actual native/browser inputs, confirmed discard,
capacity/retry, exact export requests, independent peers and retirement at wide
and compact sizes. Injected save-host checks prove request bytes and cancellation;
they do not claim a new platform picker or disk-write qualification. Exact results,
retained failures and screenshots are recorded in WORK.md. Physical-device,
assistive-technology and measured L2 workload requirements remain open.

Reproduce with the exact semantic export:

```powershell
& tools/build.ps1 -Target tree-hierarchy -TreeWindow `
  -TreeSourceDirectory <exact-mcp-export> -TreeEvidenceDirectory <new-directory>
```

The maintained entry runs native hierarchy/projection/large controls and stages
the browser hosts plus trusted Pascal input driver. It never starts a service.
Current evidence is **4127/4127** portable native/HTTP-browser checks, **83/82**
native/browser hierarchy controls, **32** native large-tree and **425/425** HTTP
desktop/CSS-390 large-tree checks, plus **17** trusted input assertions. Large
journeys retain all 4096 runtime items through ranges, type-ahead, editing,
ancestor-preserving filters, empty recovery, reparenting, disable and retirement.
The owning MCP review composes/exports one paired transaction and passes **37**;
it is discarded without changing the primary. List/table regressions pass **138/38**.

Explicit focus commands advance the command stamp even when shape is unchanged,
so row projection/geometry may rebuild; complete query, frame and memory budgets
remain open. Ancestor closure can grow with tree depth. Native still materializes
all LCL nodes. Hardware/IME, screen readers, other widgetsets, full parity,
production component depth and observing Studio delivery retain their original
owners. See [the packet](../WORK.md#current-return-path-virtualized-tree-projection-and-controls--2026-10-12).

## Studio and persistence

The project **Data** panel creates collections, adds fields from four typed
families, edits field defaults and saved row values, and adds/removes rows.
The selected list/table/tree's **Bindings** panel chooses a collection, data
scope, column titles/editability and tree parent fields. Columns can be added
or removed, and bindings cleared or inherited. These are undoable commands on
detached candidates; invalid hierarchy/schema edits and removal of used defaults
retain the accepted design/source pair. Escaped text editors preserve NUL and
supplementary Unicode. Field/collection names are currently assigned by Studio;
custom names, domains and ordering can be authored in the typed Pascal workspace.

Authored view bindings select design version 3. Collection defaults without view
bindings retain version 2; ordinary scalar-only designs retain version 1. Earlier
opaque node fields named `collectionView` remain extension data. Promotion with
a conflicting typed binding rejects rather than reinterpreting that data.
Version 3 uses the strict, independently versioned view specification packet.
Generated source uses specialized managed controls and fluent objects/enums;
visual edits preserve admitted column expressions/comments.

The browser adapter keeps keyed row elements and editor identity during updates.
It restores focus with `preventScroll` after structural relocation and retains
Nyx's registered root callbacks. Row and editor keys reach the owning control's
keyboard registrations; a consumed key suppresses default row selection. Active
command/callback frames retain their attachments and views until returning, even
when an observer releases the last owner or unmounts the target. The LCL adapter
uses actual `TListBox`,
`TStringGrid` and `TTreeView` widgets. Tree nodes retain identity across edits and
relocation; existing widget selection/edit handlers are restored at disconnect.

Bound browser collections offer one Tab entry. Lists/trees use arrows, Home and
End among visible rows; selection and keyboard focus are separate. Removing the
focused row transfers physical focus to the surviving model cursor. An empty
collection remains reachable, while a disabled collection has no Tab entry.
Restoring focus uses `preventScroll` and never takes focus from another control.
Collapsed tree descendants stay outside the visible navigation order; leaf
nodes do not advertise a collapsed parent.

## Bound table cell navigation

Browser and LCL tables share typed `TNyxGridMove` intentions and zero-based
data-cell positions; the header is excluded from navigation. Left/Right move
within a row, Up/Down retain the column, Home/End address that row's endpoints,
and Control+Home/End address the first/last data cells. Edges clip without wrapping.
Horizontal movement preserves row membership. Vertical movement keeps the
existing modifier model: Shift extends its anchored row range, Control moves
focus only, and Control+Shift extends additively. Cells and columns do not become
selection members. Cursor columns are runtime presentation, never saved defaults.

Enter or F2 addresses the current cell's editor. A noneditable column does not
redirect to a different column. Active text editors retain ordinary caret keys.
Browser Tab/Shift+Tab validate and commit before visiting eligible editors in
that row; Tab at a boundary leaves through ordinary host traversal. Enter or F2
finishes a browser edit through the same typed edit path and restores navigation
to that cell. Rejected typed input retains its exact draft, selection and focus,
with `aria-invalid` and native validation text, so it can be corrected. Escape
discards that draft and its validation state without changing accepted data.
Presentation refreshes cannot substitute for an intentional commit; callbacks
that retire/replace the original input cannot redirect the remaining key action.
Read-only browser text remains inspectable; a
read-only checkbox uses disabled HTML behavior. LCL retains its standard editor
validation/discard and widget Tab behavior; read-only grids refuse editor entry.

The browser grid has one data-cell Tab entry; editors and rows add none while
navigating. The cell itself receives focus, so readonly columns remain reachable.
Per-cell `aria-readonly`, column indices and row indices include the accessible
header. Native initial columns fit admitted content using LCL font measurement;
later publications retain user widths and active drafts. This is presentation
only. Canonical key callbacks can consume defaults; selection callbacks can
retire the view safely on both targets.

The ordinary exact MCP-authored English table passes 28 checked Win32 controls
and 43 HTTP browser controls per desktop/CSS-390. Trusted Chromium keys and
Tab/Shift+Tab add 30 host checks per width, including current numeric entry,
Escape discard, editor boundaries and exact row membership. Selection regression
passes 155 native and 182 browser checks. See
[the current packet](../WORK.md#current-return-path-bound-grid-cell-navigation--2026-10-07).
The [WAI grid pattern](https://www.w3.org/WAI/ARIA/apg/patterns/grid/), rechecked
2026-10-07, guides this contract. Paging, cell/column selection, virtualization,
full Studio sorting/filtering authoring, hardware/IME/assistive technology and other widgetsets/DPI
remain open. Host emulation and controlled LCL messages do not prove full APG
conformance or physical Android input.

## List and tree typeahead

Bound lists and trees share a portable managed `INyxTypeAhead` engine. Typing a
printable Unicode scalar searches the first displayed column, beginning after
the current focus and wrapping once. Repeating a single letter cycles through
matches. A rapid extended prefix first tests the current match. The default
window is one second, measured on a monotonic clock. No match leaves selection
unchanged. Search uses the actual visible tree order, excluding descendants of
collapsed branches. A match replaces membership and moves focus, consistent
with the existing modifier-assisted selection model.

Unicode 17 default full case folding supplies locale-independent comparison,
including supplementary characters and one-to-many mappings. Authored text
stays exact; folding neither normalizes nor removes accents. The pinned data,
license and Pascal regeneration path are described [here](../data/unicode/README.md).
The engine bounds transient state to 64 typed scalars. Search streams only the
required label prefix; controls still materialize their complete dataset.

The same immutable fluent value can be authored on a saved list/tree binding:

```pascal
LDestinations.Binds
  .Collection(
    NyxCollectionView(NyxCollection('destinations'))
      .Column(NyxTextField('title'), 'Destination')
      .TypeAhead(NyxTypeAhead
        .Enabled(True)
        .WindowMilliseconds(800)
        .Match(ntmFolded)))
  .Done;
```

Import `nyx.typeahead` when authoring by hand; generated units include it when
needed, including applications without menus. `TypeAhead` copies the binding's
columns, query, selection and scope. `UseDefaultTypeAhead` removes its explicit
choice and returns to the library default. Both parameterless Pascal spellings
reconstruct through the source workspace. An explicit saved default differs
from an absent choice. Policy getters and fluent copies own only scalar values.

An explicit policy uses the strict version-four collection-binding descriptor:
eight required members, including a nullable query and a four-member version-one
typeahead value. Unknown versions, choices, extra/missing/duplicate fields and
wrong scalar families refuse. Bindings without an explicit policy keep their
exact version-one/two/three wire shape. Document clone/persistence and generated
Pascal preserve the choice; a source Apply/Undo/Redo owns one exact design/source
pair. Invalid source retains the accepted pair and editable draft with a location.
Tables refuse declared typeahead during document/live-view admission. This
contract covers bound lists/trees, not literal items or grid cell search.

Each ordinary browser/native mount initializes an independent engine from its
accepted binding. Runtime policy can independently override that engine:

```pascal
uses nyx.typeahead;

LDestinations := LRenderer.CollectionMount('destination-list');
LDestinations.ConfigureTypeAhead(
  NyxTypeAhead.WindowMilliseconds(800).Match(ntmFolded));
```

`Enabled(False)` disables search; `ntmExact` preserves case. A policy requires a
factory-defined value and a window of 1..60000 milliseconds. Invalid replacement
refuses before changing the accepted search. Each mount owns its own engine and
borrows the pure label reader only during search. Retained mount handles report
disconnected after unmount and refuse configuration. Prefix/focus/time never
enter saved documents, stores or Undo history. Dataset revisions, navigation,
effective interaction changes and external model cursor changes reset the buffer;
selection refresh alone preserves it. Unrelated retained refreshes keep runtime
overrides; a changed saved policy refuses retained reuse, and a full remount
restores the newly authored choice. Ordinary Inspector column/scope/parent edits
retain the saved policy. Dedicated Inspector policy controls and bounded semantic
policy inspection/editing remain open Studio workflows.

Control/Meta shortcuts, Alt/AltGr text, IME composition, cell/label editors and
consumed key callbacks retain their host ownership. Space keeps its existing
toggle-selection meaning, so spaces are not part of a search prefix. Read-only
controls permit selection; disabled controls preserve it. Native adapters use
the admitted UTF-8 character callback, never translate a virtual key into text,
and restore the previous callback at disconnect. A queued character following
consumed KeyDown is also consumed. Browser focus reveals the matched item using
the host's ordinary focus scrolling; selection callbacks may retire the view
before that focus step.

This follows [listbox](https://www.w3.org/WAI/ARIA/apg/patterns/listbox/) and
[treeview](https://www.w3.org/WAI/ARIA/apg/patterns/treeview/) typeahead guidance.
Current checked Win32 controls pass 67 assertions with zero leaks, and real
browser consumers pass 68 at CSS widths 1076 and 576. They consume the identical
English MCP-authored companion. Synthetic DOM events and native callback routing
qualify the default's semantics, not trusted hardware, IME, assistive technology,
other widgetsets or complete APG conformance. Literal unbound items and grid
typeahead are outside this bound list/tree packet.

To reproduce, create an independent empty `nyx_reviews` workspace at the current
primary revision. Apply the `layout` group in
[the review recipe](../tests/typeahead-review.operations.json) through
`nyx_transaction`, then its `collections` group through `nyx_collections`, each
at the fresh exact revision with a unique operation ID. Export accepted
`nyx_source` windows of at most 60 lines at one revision to
`build/typeahead/source/nyx.generated.view.pas`, preserving exact lines. Run
`tools/build.ps1 -Target typeahead`; `-TypeAheadSourceDirectory` selects another
semantic export. Serve the resulting browser fixture through an already-owned
static host, and qualify it using the Pascal browser capture/input harness.
Discard only the owned review at its exact current revision when finished.

Saved-policy qualification uses `tools/build.ps1 -Target typeahead-policy` with
that same exported seed, then explicit local typed enrichment. The maintained
target checks wire/source/history and actual Win32 input, executes an independently
compiled exact emitted builder, checks wrong-family rejection on both compilers,
and compiles both Studios, the browser consumer and source worker. It starts no
listener, launches no browser and edits no active project. Current browser
execution and observing deployment remain required; compilation is not parity.

The existing semantic collection tool also supports saved policy-only edits and
library-default reset. It reports small local/effective/restorable option values
with explicit declared/default and capability meaning; runtime overrides stay
private. `tools/build.ps1 -Target typeahead-workflow` qualifies its in-process
dispatcher, exact source execution, discovery and actual native controls without
editing a live project. See the [semantic contract](studio-agents.md#structured-collection-context-and-authoring).

## Incremental value refresh

Both adapters consume the portable immutable
[`TNyxCollectionRefreshPlan`](../src/nyx.collections.refresh.pas).
An authoritative source log, matching revisions and exact visible identities/
ordering mark updated or replaced rows. Missing context, structural operations,
query reshaping and changed parent fields use the complete synchronization path.
Selection and interaction policy continue to synchronize independently of values.
Rejected/no-op edits still normalize the exact widget cell; unchanged editor
drafts retain their existing protection.

Canonical scalar snapshots can establish unchanged order through
`SameNyxCollectionItemOrder`, avoiding a complete row-ID walk. Completed owned
query projections use `SameNyxQueryItemOrder` to compare their private numeric
maps over independently known source order. This still evaluates the current
predicate and sort normally; equal policy/count/revision does not establish
order. Foreign, structural, paged or otherwise unproven contexts retain complete
identity comparison. Operation, scope, revision, changed-row and hierarchy
admission remain in the refresh plan. These hints retain no live owner and do
not imply equal values, physical continuity or performance-budget acceptance.

Mounted tables reuse admitted column layout and presentation observations only
when `SameNyxCollectionColumns` proves the same immutable built-in owner and
configuration serial. An equal public serial, another view or a foreign capture
does not qualify. First use and configuration changes still take complete
validation. A capture owns copied configuration and an inert identity, with no
backlink to a live view, store or widget. A held/failed refresh does not advance
the last successfully rendered configuration.

Within a guarded scalar refresh, the browser can also retain its own row slots
when the refresh plan proves the exact physical predecessor's identities and
order. Changed values, draft recovery, measurement and paint still run. Full,
structural and uncertain transitions keep the removal-aware remapping path;
matching revision numbers alone cannot authorize slot reuse.

Native lists retain entry objects and update only affected captions. Table cells
and tree captions skip unchanged row values. Browser labels retain unchanged text
nodes and editors retain scalar comparison guards. The plan owns only Boolean
bits, with no dataset/renderer/widget references; borrowed publication context
retires after notification, and mounts disconnect before widget disposal.

Browser lists and tables use measured row windows; trees still materialize rows.
Native lists retain the standard LCL list box and its clipped widget painting;
their caption storage still covers the complete dataset. Native tables read values on demand;
browser tables contain the qualified measured row window described below.
Visible-identity, selection and hierarchy scans,
snapshot validation and query evaluation still contribute dataset-wide work.
A refresh count describes completed adapter passes. Viewport virtualization and
documented both-target frame/memory budgets retain their owner in
[extension/performance](../TODO/NS-3_extension-performance_01.md).

`tools/build.ps1 -Target collection-refresh -GridSourceDirectory <semantic-export-directory>`
consumes an unchanged MCP-exported table companion. Native focused controls pass
29, existing collection controls 27 and ordinary table/query controls 75; browser
fixtures, Studio and its worker compile/stage without launching a browser/service.
The same serial native 4096-row workload reduces twenty updates 3406→1391 ms;
these hidden-control samples exclude paint and do not establish production budgets.
Current browser execution and full observing/delivery remain separate gates.
See [the exact evidence](../WORK.md#current-return-path-incremental-collection-values--2026-10-07).

### Mounted live-inspection workload

[`nyx_live_inspection_workload.lpr`](../tests/nyx_live_inspection_workload.lpr)
measures the public two-table Live service inspection application with 10,000
five-field services. Its authored 160px table height is unchanged. It separates
immutable preparation, visible publication, prepared admission, target work and
the next target turn. Twenty ten-row bursts retain a real Unicode input draft
and scalar caret. A second twenty-burst series runs under an active filter;
pause/resume, an independent peer, sorting and distant-row value readback remain
part of the same journey. Freshly generated Pascal runs through the same driver.

Qualification allocations are 1280x900 and 390x844. The declared targets remain
3000ms visible population, 100ms update/target-turn p95 with a 250ms maximum,
250ms discrete actions and 128MiB native managed-heap growth. Browser frame
opportunities and native `Update` are software observations, not physical input
or display latency. Optional browser heap estimates cover the whole context and
do not establish equivalent memory acceptance. Exit code 2 means behavioral
checks completed with failed timing/memory targets; it is not a passing result.

The 2026-10-31 run completes 188/187 handwritten and 187/187 generated native
behavior checks. Desktop p95 targets remain unmet; compact handwritten/generated
runs retain one/zero failed budgets. Browser handwritten applications complete
147 checks per width, with six/eight failed budgets. These samples qualify
behavior and expose remaining cost; they do not accept the original L2 budgets.
Browser distant-row readback proves the realized value, while reviewed geometry
shows that row outside the table's visible clip. Missing normal header faces and
the retained full presentation consumer's Fit failure also remain open under the
existing workbench task. Do not substitute resident DOM contents for successful
visible navigation. WORK.md retains exact cohorts, failures and the reassessment
after two product repair cycles.

## On-demand native table values

Ordinary authored tables automatically use
[`TNyxCollectionStringGrid`](../src/nyx.collections.lcl.grid.pas), an owned
TStringGrid subclass. Existing public `Cells`, keyboard/editing and rendering
contracts remain available. With a collection attached, native painting and
direct cell reads obtain the current accepted query value through a borrowed pure
UI-thread reader. Without a reader, literal tables retain inherited behavior.
No authoring string, target directive or extra saved descriptor is required.

Explicit widget drafts retain sparse exact-text overlays. Unrelated publications
retain them; accepted/no-op normalization clears the exact overlay, and query
displacement/empty-result geometry withdraws obsolete coordinates. Source/model
snapshots own accepted values. Mount disconnect detaches its receiver before
restoring native handlers; the grid does not retain the mount/view/document.
Native widget XML streaming is not a persistence path for provider data: portable
Nyx persistence owns the collection and its authoring contract.

The maintained collection-refresh command includes 20 actual native checks using
the unchanged authenticated English MCP-exported layout and an independent
4096-row runtime source. A real viewport repaint requests 30 source cell values;
a distant `Cells` read requests one. Drafts, updates, query ordering, selection,
empty results and teardown pass with zero leaks. These counts establish on-demand
native reads, not complete memory/frame budgets. Initial content autosizing may
read the whole source; grid geometry, model/query and metadata work remain
dataset-sized. `OverrideCount` reports only Nyx draft overlays, excluding native
physical caches and all model memory. Browser row-window qualification and both-target
production qualification remain open. See
[the current evidence](../WORK.md#current-return-path-on-demand-native-table-values--2026-10-07).

## Measured browser table windows

The ordinary browser adapter contains a viewport row window plus six overscan
rows, using portable [`TNyxCollectionRowGeometry`](../src/nyx.collections.window.pas)
for logical prefixes and pixel-to-row lookup. Positive measurements replace
unvisited estimates; variable text heights need no font scaling. Physical rows
carry source/query identities, while hidden spacers represent missing intervals.
Logical keyboard movement and Shift ranges use the entire admitted dataset.
Navigation destinations realize before selection callbacks can retire the view.

Focused editors remain attached; unfocused unfinished drafts retain independent
detached controls. Returning to their viewport reuses those controls. Accepted
field changes and explicit normalization keep prior comparison guards. Native
scroll offsets, focused caret and relocation change noise have explicit handling.
Scroll/resize/row/ancestor observation coalesces into one animation callback, and
disconnect withdraws all listeners/targets and cancels pending work. Old ancestors
retire when a retained view moves. Same-source refresh keeps the privately owned
logical slot map and visits a sorted union of resident rows/drafts, the incoming
viewport and pinned destinations. Width redistribution and measurement likewise
visit resident faces. A changed source still remaps dataset-sized slots; geometry,
query validation and logical selection retain their separate costs.

Complete logical row counts include the header; each exposed header/data row
has its index. Spacers carry no selectable item identity and are hidden from
accessibility, following [WAI grid/table structural guidance](https://www.w3.org/WAI/ARIA/apg/practices/grid-and-table-properties/).
Typed browser bridges retain fractional scroll coordinates as specified by
[CSSOM View](https://drafts.csswg.org/cssom-view/#dom-element-scrolltop).
This metadata does not establish screen-reader or full APG conformance.

The maintained collection-refresh target runs checked native geometry and actual
control consumers, then stages `collection-window.html`, `virtual-table.html`,
Studio and its worker. The 4096-row consumer now executes **38** assertions at
desktop and CSS-390 over HTTP against unchanged authenticated English MCP source:
draft/caret return, offscreen keyboard/range movement, query reshaping, empty
results and retirement. Native on-demand painting/reads pass **20**, with 30
source reads in the focused repaint. Existing browser grid/query journeys pass
**47/75**, and the trusted keyboard/editing/Tab consumer passes **30**.

The fixture records 40 same-source synchronous refreshes outside its correctness
gates. One identical desktop sample changes **557.7 → 34.6 ms** after resident-work
iteration. This excludes paint and is neither a general speedup nor a production
frame/memory budget. Desktop/narrow/native captures are inspected; hardware/IME,
assistive technology, other widgetsets, list/tree virtualization, dataset-sized
arrays, query/selection work and installed Studio delivery remain open. See
[current qualification](../WORK.md#current-return-path-large-table-interaction-and-update-cost--2026-10-12).

## Measured browser list windows

Bound lists use the same portable row geometry and resident-work renderer as
tables. Logical order, ranges, selection and type-ahead use the complete admitted
query result. Keyboard destinations realize before selection callbacks can retire
their host. Home/End and type-ahead reveal within the list's own scroll plane,
including an already-focused option relocated by a query. A short-lived reveal
intent survives neighboring height measurements; stable geometry consumes it and
manual host scrolling cancels it. Source refresh retains containing scroll/focus.

Options carry `aria-posinset` and `aria-setsize` for the complete query result;
hidden spacers have no selectable identity. One logical option owns Tab entry,
or the host does when empty. This follows the metadata and navigation guidance
in the [WAI listbox pattern](https://www.w3.org/WAI/ARIA/apg/patterns/listbox/)
(checked 2026-10-12), without claiming screen-reader qualification. List options
are captions; put independent actions in a compound's named action slot or use
an interactive grid instead of embedding buttons inside a listbox option.

Application scope deliberately shares stores. Select `.Scoped(csInstance)` in a
reusable definition when each instance needs independent data:

```pascal
LIdeasList.Binds
  .Collection(
    NyxCollectionView(NyxCollection('ideas'))
      .Column(NyxTextField('title'), 'Idea')
      .Selection(nsmMultiple)
      .Scoped(csInstance)
      .TypeAhead(NyxTypeAhead))
  .Done;
```

`tools/nyx_list_companion.lpr` creates a named-part reusable card and two instances
through one authenticated MCP transaction, validates paired Undo/Redo, exports
specialized Pascal and discards its owning review. Its source is consumed by
`tools/build.ps1 -Target collection-refresh -CollectionListWindow
-ListSourceDirectory <exact-export> [-ListEvidenceDirectory <new-directory>]`.
The maintained entry runs native controls and stages the HTTP application plus
the Pascal semantic and trusted-input tools. It never starts a server or edits
an active design by browser automation.

The 4096-row journey passes **21** native and **138/138** desktop/CSS-390 browser
checks. Ten trusted pointer/keyboard/resize/Tab checks and the existing 38-assertion
large-table journey pass. Native still materializes caption strings; complete
memory/frame budgets, tree virtualization, hardware/IME, assistive technology,
other widgetsets and installed Studio delivery remain open. See
[current evidence](../WORK.md#current-return-path-virtualized-bound-lists--2026-10-12).

## Studio collection authoring

The Project Data section edits saved typed schemas, defaults and rows. The
selected list/table/tree exposes its collection binding through the Inspector's
Bindings panel. The seventeen existing operations now capture immutable Pascal
intent before source synchronization: collection/row/field identity, scalar
family, notation, projection and enum choices travel independently of widgets.
View column order may differ from schema order; edits follow the exact field.

Ordinary Studio controls submit that intent to the shared isolated command queue.
An admitted candidate publishes source and design together as one Undo entry.
Newer waiting scalar/title/choice text stays visible; structural changes lock the
exact collection or authored view before repaint. Rejected numeric text returns
to its accepted value while retaining the field's focus. Reusable overrides,
later navigation, independent Pascal drafts and new loads retain their existing
ownership guards. Pending presentation is not admission: missing/mismatched
typed metadata keeps the last displayable binding until replay reports its
diagnostic. Renderers do not borrow a worker's document or mutable store.

The current native journey qualifies these ordinary controls and exact emitted
companion; browser Studio, its module worker and asynchronous DOM journey
compile. Current browser execution/observing deployment, comfortable large-project
editing, wider widget metrics and accessibility retain their original gates.
Structured collection semantic operations remain with the existing
[agent workflow owner](../TODO/NS-4_agent-workflows_01.md); the active MCP release
is not changed by this inspector integration. See [WORK.md](../WORK.md).

## Reproducible checks

`tools/build.ps1 -Target collection-views` compiles the portable view fixtures,
both adapters, their real-control journey and the mounted-control benchmark.
The browser `collection-views.html?host=1` journey runs inside an exact 390-pixel
iframe. This is viewport emulation, not physical phone testing.

`tools/build.ps1 -Target collection-authoring` verifies saved binding admission,
composition, paired source/history, automatically mounted browser/LCL controls,
Studio commands and compiled application/page/component consumers. It also checks
Unicode packet/envelope reconstruction and fifty intended type errors per compiler.
`tools/build.ps1 -Target selection` prepares a crafted companion, executes the
actual native selection journey, compiles/repeats it and compiles the same browser
consumer. Execute `selection.html` to qualify that browser journey. All artifacts
can be staged with `-BrowserOutput` while Studio remains live.

Execute `collection-authoring.html`, `collection-authoring-generated.html` and
`collection-unicode.html` after compilation to check the browser consumers.

`tools/build.ps1 -Target collection-inspectors` runs typed isolated request/reply
checks, source regressions, pending refusal presentation, real standalone native
controls, the existing collection authoring consumer and the exact compiled
companion with its native table callback. English desktop/390 captures belong to
its `controls/` directory. The `browser/` directory stages Studio, its matched
Pascal module worker and the four `collection-inspector*.html` qualification
hosts. The build command launches no listener and replaces no observing project
or live service. Compilation alone does not qualify those browser journeys.

`collection-view-benchmark.html` measures mounting and twenty committed integer
updates with a list, table and four-way tree simultaneously attached, at 512 and
4096 rows. Version two gives each control a 220-pixel viewport. Correct values,
revisions, full logical membership, bounded browser rows, subscription disposal
and refresh counts gate output. Distant checked rows are realized after timing;
closed tree descendants need no physical rows. Setup excludes seed construction. Update time
includes store publication, view validation and target synchronization. Native
widgets run in a hidden form; browser timing uses `performance.now` without a
virtual-time budget. Neither measurement includes a paint/interaction latency
guarantee or establishes a production frame budget.

The same MCP-exported builder supplies both targets and their independent runtime
store. Reproduce with `tools/nyx_collection_companion.lpr` (explicit enrolled
configuration and a new export directory), then:

```powershell
& tools/build.ps1 -Target collection-performance `
  -CollectionPerformanceSourceDirectory <exact-mcp-export> `
  -CollectionPerformanceEvidenceDirectory <new-directory>
```

This entry runs shared/native control and checked ownership consumers, records
an optimized native sample, and stages browser hosts plus the Pascal semantic
producer. It starts no server and changes no protected project. Historical
materialized measurements use different layouts and are not comparable baselines.

Current selection reconciliation looks up selected identities in the complete
source instead of visiting every source item. Canonical source order is retained
independently from display/query order. Only displaced membership needs an
iterative merge; ordered dense selections stay linear and empty membership
needs no source enumeration. Cursor, anchor, filtered membership and retained
immutable snapshots preserve their prior semantics on both targets.

Three alternating before/after samples per target, without concurrent UI or
compiler workloads, qualify this exact closed-tree/unselected workload (2026-10-12):

| Target | Runtime rows | Before median, 20 updates (ms) | After median (ms) |
| --- | ---: | ---: | ---: |
| Win32 LCL | 512 | 234 | 203 |
| Win32 LCL | 4096 | 1453 | 1250 |
| HTTP Chromium | 512 | 706.1 | 609.1 |
| HTTP Chromium | 4096 | 4297.8 | 3492.1 |

These are scoped observations, not general speedups or frame/memory budgets.
Source/query validation, snapshots and tree metadata still visit whole datasets;
native trees/lists retain complete native storage. Shared reconciliation cases
include complete 4096-member reorder alongside sparse/empty/dense and query
cases. Actual controls retain callbacks, edits, ownership and teardown. Current
evidence and remaining performance/extension/parity outcomes are in
[WORK.md](../WORK.md#current-return-path-combined-collection-updates-and-sparse-selection--2026-10-12).

Snapshot reads subsequently remove redundant deep field-vector allocation while
preserving the immutable item contract (2026-10-12). All-family ownership,
retained proposals/snapshots, query, coordinated publication and actual controls
pass on both targets. Three alternating samples of the same complete workload
change 4096-row median twenty-update time from **1234 to 641 ms native** and
**3621.7 to 1943.3 ms browser**. Source/query/index/hierarchy and complete frame/
memory/interaction budgets remain open. See
[the read packet](../WORK.md#current-return-path-immutable-collection-reads-and-projection-cost--2026-10-12).

Admitted identity reads subsequently avoid repeated open-name reconstruction
(2026-10-12). Defined/collection/bounds checks and constructor refusals stay intact;
typed record replacement cannot mutate the owner or its index. Ownership/query/
publication/control gates pass on both targets. Three alternating samples of the
same complete workload change 4096-row median twenty-update time from **656 to
531 ms native** and **2066.3 to 1357.6 ms browser**, without establishing general
speedups or full frame/memory/interaction budgets. See
[the identity packet](../WORK.md#current-return-path-admitted-collection-identity-reads--2026-10-12).
