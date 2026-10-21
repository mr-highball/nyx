# Selected-record master/detail

[Components](components.md) · [Collections](collections.md) · [Current work](../WORK.md)

`nyx.master.detail` supplies copied field mappings for the specialized
`INyxMasterDetail`. Its master List remains the sole owner of the collection
source, scope, columns, query, paging and selection. Configure that ordinary
collection binding, then opt into live detail presentation:

```pascal
LDirectory := NewNyxMasterDetail('directory');
LDirectory.MasterList.Binds.Collection(NyxCollectionView(NyxCollection('people'))
  .Column(NyxTextField('short-name'), 'People')
  .Scoped(csInstance)).Done;
SetNyxMasterDetailIntent(LDirectory, TNyxMasterDetailIntent.Configured(
  NyxMasterDetailOptions(NyxTextField('full-name'), NyxTextField('summary'))));
```

Both detail fields must be declared text fields in that source. The list's own
display column can differ from the detail title. Single selection is required;
multiple selection never silently chooses an arbitrary record. A selected record
must appear in the current query/page snapshot. Hidden, removed or empty selection
restores the authored title/description placeholders and disables Edit. Retained
hidden membership stays owned by the collection view and resumes when visible.

`TNyxMasterDetailIntent.Disabled` is an explicit local mask.
`InheritNyxMasterDetailIntent` removes only that local intent. Absence preserves
the static recipe or inherits a reusable definition. Authored captions, custom
payloads and sibling instances remain independent. Runtime detail projection
never changes authored data, scalar state or selected membership. Existing
bindings on the two detail text properties refuse competing projection authority.

Ordinary application renderers register the built-in factory alongside caller
coordinators and prepare one common behavior vector before replacing an accepted
view. Configured applications need no extra registration. Immutable membership
composition refuses duplicate kind/intent registrations before factory calls.
The runtime owner is available through `MasterDetailBinding(RuntimeID)` and the
same specialized interface from `CoordinatorBinding(RuntimeID)`;
`Current` and `CaptureEdit` return copied `TNyxMasterDetailRecord` values, including
stable collection/item identity, data revision and typed item fields. These descriptive
snapshots grant no mutation authority and can outlive the original view.
Independent instance stores may reuse the same logical collection/item names;
the callback's copied `SourceID` identifies the runtime MasterDetail instance.

The Edit callback receives the same copied record through existing event Details:

```pascal
if TryNyxMasterDetailEdit(AEvent, LRecord) then
begin
  // Use the record captured for this Edit, even if selection has since moved.
  LName := LRecord.Item.GetValue(NyxTextField('full-name'));
end;
```

The adapters capture before callback scheduling. A conflicting structured payload
refuses replacement; unrelated events retain their payload. Disconnect/replacement
revokes the old binding and its weak input ports.
Explicit `INyxMasterDetailBinding.Disconnect` also releases that owner's text
reservations and Edit availability constraint immediately. It preserves the
enclosing event generation and other policies, permitting deliberate manual
reattachment to the same realized control. Later retirement of the old common
mount cannot detach the replacement's independent ownership.

Automatic factories use guarded role projection and copied semantic/primitive
kind, compound and effective placeholder facts. Runtime text reservations also
cover manual MasterDetail attachment, so a caller behavior cannot claim the same
title or description even when both initial strings agree. Reservations belong
to exact realized nodes, are revoked before cleanup, and never become serialized
metadata or inherited authority on an independent clone. Enabled constraints
continue to compose with authored, bound and caller policies.
The optional `INyxCoordinatorProjectionLifetime` lets a behavior whose public
Disconnect releases presentation invoke the scope's existing retirement path.
Its Revoke is local and flags only; Disconnect attempts the independent cleanup.

`NewNyxMasterDetailCoordinatorFactory` is also available to a custom portable
host using the common prepared driver. The existing manual preparation API and
legacy mounts helper remain available. Optional common event enrichment captures
the Edit record before scheduling, preserving scalar Value alongside Details;
it grants no additional callback, routing or mutation authority.

`NyxEventsMetadata` preserves the original scalar Value contract.
`NyxEventDetailsMetadata` independently describes structured Details over the
same checked event and route identities. A configured Edit role advertises an
owned object; its explicit scalar contract, if present, remains available too.
Static or Disabled recipes do not acquire that object guarantee. Same-name
producers with different contracts retain their individual route facts and no
uniform aggregate guarantee. Discovery does not grant custom `Emit` authority.

The Events Inspector shows and searches Details independently. The public Logic
route form offers **Event details**, and Tool Canvas discovery exposes a separate
JSON output alongside invocation signals and any scalar Value output. Select the
exact named Edit descriptor when a larger compound has mixed same-name sources.

A route can format the complete captured record into existing bound text:

```pascal
LDocument.State.SetValue(NyxTextState('record-preview'), 'Choose a person to review.');
LDocument.Flows.Define(NyxFlowRef('Review record'), NewNyxFlowGraph.WithStep(
  NyxFlowStep('preview'), NewNyxJSONFormat(njlIndented, 2)).Build);
SetNyxFlowRoutes(LDirectory, NewNyxFlowRoutes.WithRoute(
  NyxNamedFlowRoute(NyxFlowRouteID('review'), NyxFlowRef('Review record'),
    NyxSemantic(nseEdit), nfcmAfterDispatch)
    .InputEventDetails(NyxFlowEndpoint(NyxFlowStep('preview'), NyxLogicPort(nlpJSON)),
      NyxResourcePath)
    .OutputState(NyxFlowEndpoint(NyxFlowStep('preview'), NyxLogicPort(nlpText)),
      NyxTextState('record-preview'))));
LMemo := NewNyxMemo('record-preview-memo');
LMemo.Configure.ReadOnly(True).Done;
LMemo.Binds.Value(NyxTextState('record-preview')).Done;
```

The graph captures the event's owned Details before submission. UI publication
is a separate scheduled step even with sequential computation; observe the
published state or completion instead of reading a result inside the click.
The [integration task](../TODO/NS-4_master-detail-events_01.md) tracks ordinary
Studio/save/generated application qualification and its platform limits.

Studio composes the reusable Nyx-built `NewNyxMasterDetailEditor`. Its two mapped
field drafts share the existing inspector draft lifecycle. **Apply detail fields**,
**Disable live details** and **Use inherited details** create one paired
design/Pascal history step after exact authored owner and source-baseline checks.
Bind the master list through its existing Bindings workflow first; unbound static
lists keep a helpful disabled Configure action. Source/scope/query are configured
in that existing workflow, rather than duplicated by this form.

```powershell
./tools/build.ps1 -Target master-detail -MasterDetailEvidenceDirectory <new-directory>
```

The maintained target runs focused preview ownership, policy/runtime/editor/event/route/port consumers and the full
native Studio journey at desktop and compact widths. It compares exact saved
project/Pascal bytes and compiles the saved unit into independent applications.
Browser consumers and the exact generated companion are staged for separate
execution; this target starts no listener.

The preceding runtime checkpoint qualified 62 runtime checks, 63 public editor checks,
ordinary native Studio/application 106 at 1280 and 114 at 390, exact saved compiled
application 30/30 and existing actual Kanban regression 98/98, all with clean heaps.
The native queued Edit check captures one record before selecting another and
delivers the original typed item/revision afterward. Nine accepted compiler logs
contain zero Nyx warnings; 28 upstream pas2js RTL warnings remain separately
identified. Browser consumers compile only in this packet. See
[the work record](../WORK.md#selected-record-masterdetail-checkpoint--2026-10-21).
The independent channel integration additionally passes 687 native semantic/
Inspector checks, 85 public route checks and 289 Tool Canvas/port checks with
clean heaps. The Interact integration now also qualifies the complete native
Studio journey165/175 and its own exact independently admitted/compiled saved
application36/36. The final callback-lifetime repair qualifies57 focused checks
and reruns the unchanged165/175 journey with clean heaps; source93, compiled36/36
and raw-runtime Canvas55/63 reuse their unchanged relevant boundaries. Both final
widths export identical project/Pascal bytes. Generated checked NyxNamedEvent
routes now roundtrip through the existing source reader, without a captured-pair
fallback. All17 compiler logs in that composite have zero Nyx warnings;63 repeated
upstream RTL warnings remain. Browser counterparts compile only.
Browser input, hardware/assistive quality, other widgetsets/DPI, production
performance and full component/parity acceptance remain open.

The common-coordinator migration additionally qualifies99 focused checks,
171 public coordinator checks, ordinary controls68/68, the unchanged full Studio
journey165/175 and its own exact saved compiled application36/36. Publication
136/136 and Kanban98/98 retain compatibility; all12 native outcomes have clean
heaps. Sixteen compilers pass with zero Nyx warnings and56 repeated upstream RTL
warnings. The matching browser programs compile only. This accepts the scoped
[migration](../TODO/DONE/NS-3_coordinators_03.md); reusable/embedded adoption and
the original browser/parity gates remain open. See
[the final qualification](../WORK.md#masterdetail-common-coordinator-migration--2026-10-21).
