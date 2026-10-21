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

Ordinary application renderers prepare the behavior before replacing an accepted
view. The runtime owner is available through `MasterDetailBinding(RuntimeID)`;
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

Structured Edit data is currently available to callbacks through this reader.
Automatic semantic flow/Tool Canvas port discovery remains a separate
[required follow-up](../TODO/NS-4_master-detail-events_01.md); callback delivery
does not establish that authoring integration.

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

The maintained target runs focused policy/runtime/editor consumers and the full
native Studio journey at desktop and compact widths. It compares exact saved
project/Pascal bytes and compiles the saved unit into independent applications.
Browser consumers and the exact generated companion are staged for separate
execution; this target starts no listener.

Current local qualification is 62 runtime checks, 63 public editor checks,
ordinary native Studio/application 106 at 1280 and 114 at 390, exact saved compiled
application 30/30 and existing actual Kanban regression 98/98, all with clean heaps.
The native queued Edit check captures one record before selecting another and
delivers the original typed item/revision afterward. Nine accepted compiler logs
contain zero Nyx warnings; 28 upstream pas2js RTL warnings remain separately
identified. Browser consumers compile only in this packet. See
[the work record](../WORK.md#selected-record-masterdetail-checkpoint--2026-10-21).
Browser input, hardware/assistive quality, other widgetsets/DPI, production
performance and full component/parity acceptance remain open.
