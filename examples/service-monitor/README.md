# Service monitor

A portable Nyx table workbench with English sample service records. It uses
specialized controls and typed collection policies throughout. The sample data
is local to the document; connecting to a real debugger is an application concern.

Try the column controls for filtering, sorting, resizing, bounded content sizing,
reordering, visibility, pinning and restoring defaults. Service starts pinned at
the logical start and Status at the logical end. Both remain editable; Process ID
is read-only. Notes wrap and long command lines retain their complete raw value
for inspection and copying. Cell actions expose inspection, copy and TSV export.
Hold Shift with Page Up or Page Down to extend selected cells by the visible
viewport. Arrow and Home/End range keys remain available; Escape clears the
range. Editing keeps the control's own text-selection keys.
Body-row density is independent of the query/column header reservation, both at
initial mount and during runtime updates. Native headers use their captured
native metric rather than multiplying the selected body-row height. This example
also requests a compact caption area through the portable typed policy:

```pascal
.TablePresentation(NyxTablePresentation.Density(ntrdComfortable)
  .Header(NyxTableHeader.Lines(1).MinimumHeight(32)))
```

The header minimum is in logical pixels. Targets preserve their separate space
for filtering and column management. Change the line count or minimum to suit
your captions; `.UseDefaultHeader` restores target defaults while retaining body
density. Omit `.Header` altogether to preserve older header behavior.

The application explicitly supplies clipboard and file-save hosts through the
public renderer service contract. Authored export intent and runtime permission
remain separate. Unavailable, cancelled and download-requested outcomes retain
their existing typed meaning. A requested browser download is not a confirmed
disk write. No clipboard or save operation runs merely by opening the example.

## Live inspection

`live_inspection.lpr` uses the same public table and four English service records
in two independent views of one application collection. Pause the upper table,
then choose **Next feed update**: the lower table changes while the upper frame
and its inspected values remain readable. The bound status shows the pending
publication count. **Resume latest** adopts current data and explains an inspected
record that was removed/replaced, including reuse of its original ID.
The command row becomes a column below 560 logical pixels through Nyx's ordinary
typed viewport configuration; no adapter-specific UI construction is needed.

The replay groups a change/insert, a removal, and a gateway replacement into
three successive atomic updates. It runs only on request, without an external
process or timer. Copy and cell-range commands read the paused frame; editing
that frame refuses until resumed. The application explicitly supplies its target
clipboard host; merely opening or advancing the demo does not write a clipboard.
Runtime policy and pause status never change the saved document/history.

`TNyxLiveInspectionDemo` attaches through public collection, context and event
contracts after mounting. Its reference-counted callbacks retain no application,
router or document. Own the binding beside the application and close/free it
before navigation or application disposal. Retired contexts/generations refuse
late calls. Generated documents use that same runtime attachment, with no
privileged generated-code path.

The [retention contract](../../docs/collection-views.md#pausing-live-inspection)
describes payload/removed-ID bounds, overflow and current limitations. This demo
does not yet qualify Tree physical anchors, full feed states or Studio policy
authoring.

**Follow last visible** reveals the final row of the current filtered/sorted page
without selecting it. Later changed tail identities follow; unrelated value
updates retain the reading anchor. Repeating Follow deliberately reveals the
current tail again. **Keep reading position** stops following, including while
paused. Follow is disabled during pause and never resumes implicitly. Manual
scrolling does not itself stop follow mode; choose Keep reading before inspecting
older arrivals. There is no assumed timestamp/order field. These choices use
`INyxCollectionFollowing` and ordinary Nyx buttons/state bindings, including in
the generated application. See [follow semantics](../../docs/collection-views.md#following-the-last-visible-row).

If an unfinished Table edit loses its original row/value, a recovery card keeps
the exact draft separate from live data. It explains the reason, provides a
read-only memo and previous/next navigation, and requires confirmation before
discarding the exact reviewed entry. Changing entries cancels a pending discard.
The runtime binding accepts an optional `NyxCollectionDraftRecovery(LMount)` data
port without retaining the mount. NUL is shown as `\0` in the memo; the journal's
raw text is unchanged. See [draft recovery](../../docs/collection-views.md#recovering-unfinished-cell-drafts)
for advisory warnings, lifetime and the remaining hard-memory/browser gates.

`tests/nyx_collection_recovery_controls.lpr` passes 24 actual native control
checks at each width in both handwritten and generated applications. It types
unfinished supplementary text, changes a producer value, replaces a same-ID row,
filters the edited row out and exercises the two-step discard. These injected
native interactions do not establish browser, trusted hardware or pixel quality.

Opted-in Lists and Tables recover their reading row independently of ordinary
selection. Inserting above it keeps the same record in view; removing it chooses
an old surviving neighbor. Resume uses the complete paused removal history, so a
replacement with the same ID cannot inherit the old reading position. The bound
status explains a disappeared reading row or an explicit reset. The callback
retains a readonly UI data port, without retaining a mount or event router.
See [stable reading positions](../../docs/collection-views.md#stable-reading-positions)
for fallback, skipped-history and physical clamping limits.

`tests/nyx_live_inspection_controls.lpr` exercises actual native button messages
at desktop/compact widths, shared-source peer behavior, pending status, frozen
query counts, copying, range lifetime, refused stale edits, replacement recovery,
unchanged designs and binding retirement. The exact emitted Pascal companion
runs the same journey. It now also exercises both follow buttons, physical tail
visibility, value-only updates, repeated same-revision requests, peer isolation
and pause disablement. Native List/plain-grid cases exercise fluent initial
following and selected-row independence. Browser fixture/example consumers compile; browser input
and visual acceptance remain separate.
The fixture adds 64 runtime records, moves the visible viewport away from the
selected gateway and checks insertion, deletion, sorting, pause/resume and ID
replacement. Separate visible native List and plain-grid cases exercise their
own paths and retained-port retirement. These are input/contract checks, not a
scale benchmark or physical-device/screen qualification.

## Build

From the repository root, using the existing `.local/toolchain.json` configuration
and installed Win32 LCL units:

```powershell
$cfg = Get-Content .local/toolchain.json -Raw | ConvertFrom-Json
$out = Join-Path ([IO.Path]::GetTempPath()) ('nyx-service-monitor-' + [Guid]::NewGuid())
$native = Join-Path $out 'native'
$browser = Join-Path $out 'browser'
New-Item -ItemType Directory -Path $native, $browser | Out-Null
$units = @(
  "-Fu$($cfg.LAZARUS)/lcl/units/i386-win32",
  "-Fu$($cfg.LAZARUS)/lcl/units/i386-win32/win32",
  "-Fu$($cfg.LAZARUS)/components/lazutils/lib/i386-win32",
  "-Fu$($cfg.LAZARUS)/packager/units/i386-win32"
)
& $cfg.FPC -Mdelphi -O2 -Sa -Cr -Co -Ci -Fusrc -Fisrc `
  -Fuexamples/service-monitor "-FU$native" "-FE$native" @units `
  examples/service-monitor/service_monitor.lpr
& $cfg.PAS2JS -Mdelphi -Tbrowser -Jirtl.js -Fusrc -Fisrc `
  -Fuexamples/service-monitor "-FE$browser" examples/service-monitor/service_monitor.lpr
Copy-Item $cfg.PAS2JS_RUNTIME (Join-Path $browser 'rtl.js')
Copy-Item examples/service-monitor/service-monitor.html $browser
```

Run `service_monitor.exe` from the native output. Serve the browser directory
with a Pascal HTTP host, then open `service-monitor.html`. Compilation does not
establish browser interaction or browser clipboard/file permissions. Other native
widgetsets use their corresponding installed LCL unit directories.
For the live inspection variant, use `live_inspection.lpr` in both compiler
commands and copy/open `live-inspection.html` beside the generated script/runtime.

## Qualification

`tests/nyx_service_monitor_controls.lpr` mounts the public document in two native
applications. It checks editable pinned fields, read-only refusal, exact Unicode,
independent stores, unchanged authored defaults and mount retirement at an explicit
width. An independently mounted body-density comparison also protects header
allocation. Its optional second argument writes a fresh UTF-8 generated companion;
`NYX_SERVICE_GENERATED` selects that companion for the same checks.

The same fixture also supplies 64 local test records to separate instances of
the public document. Actual Win32/LCL key messages exercise the center and both
pinned fields at tall and short authored heights, including measured page steps,
anchor reversal, independent row membership and Escape. The exact generated
application runs the same journey. These additional records belong to the test;
the starter document retains its four English service records.

These are mounted contract checks, not physical keyboard/pointer qualification.
The original workbench task retains complete interaction, browser, authoring,
visual and lifetime acceptance. Studio's semantic connection was unavailable when
this example was authored; no active Studio project was replaced.
