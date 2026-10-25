# Service monitor

A portable Nyx table workbench with English sample service records. It uses
specialized controls and typed collection policies throughout. The sample data
is local to the document; connecting to a real debugger is an application concern.

Try the column controls for filtering, sorting, resizing, bounded content sizing,
reordering, visibility, pinning and restoring defaults. Service starts pinned at
the logical start and Status at the logical end. Both remain editable; Process ID
is read-only. Notes wrap and long command lines retain their complete raw value
for inspection and copying. Cell actions expose inspection, copy and TSV export.

The application explicitly supplies clipboard and file-save hosts through the
public renderer service contract. Authored export intent and runtime permission
remain separate. Unavailable, cancelled and download-requested outcomes retain
their existing typed meaning. A requested browser download is not a confirmed
disk write. No clipboard or save operation runs merely by opening the example.

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

## Qualification

`tests/nyx_service_monitor_controls.lpr` mounts the public document in two native
applications. It checks editable pinned fields, read-only refusal, exact Unicode,
independent stores, unchanged authored defaults and mount retirement at an explicit
width. Its optional second argument writes a fresh UTF-8 generated companion;
`NYX_SERVICE_GENERATED` selects that companion for the same checks.

These are mounted contract checks, not physical keyboard/pointer qualification.
The original workbench task retains complete interaction, browser, authoring,
visual and lifetime acceptance. Studio's semantic connection was unavailable when
this example was authored; no active Studio project was replaced.
