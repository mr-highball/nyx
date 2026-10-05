# Native Nyx Studio

[Project](../PROJECT.md) · [Building](building.md) · [Authoring owner](../TODO/NS-4_studio-authoring_01.md)

The native editor now has a standalone entry point in
[nyx_studio_native.lpr](../studio/nyx_studio_native.lpr). It consumes the same
`BuildNyxStudioView` composition, portable session and source/property/event
routers as browser Studio. The palette, hierarchy, inspector, warning cards,
canvas surface, split view and Pascal editor are ordinary public Nyx components.
LCL supplies their native adapters and the containing application window.

```powershell
./tools/build.ps1 -Target native-studio
./build/native-studio/controller/nyx_studio_native.exe
```

The executable takes an optional local project directory as its first argument;
otherwise it uses the operating system's application configuration directory.
An optional second argument selects an explicit `http://127.0.0.1:port` service
origin, and a third selects one exact project reference. Omitting the reference
selects the service's primary project. Remote origins, paths, queries and
credentials refuse before network work. These arguments are machine settings,
outside portable designs; connecting is optional.
Launching the built editor requires no application compiler, browser runtime or
server connection. The Outputs section is available at any time, and choosing an
output does not alter the design or generated source.

The controller owns its session, three renderers, theme, local paired store and
parking hosts. Its application host is borrowed and must outlive the controller.
Destroy the controller before that host. Repaint callbacks are canceled during
destruction, and renderers are released before their documents and hosts.

Native widget notifications publish portable commands and queue a coalesced
paint. The controller moves the existing canvas and Pascal editor to hidden
parking hosts before replacing chrome, then mounts those same views in the new
shell. Source typing does not rebuild chrome. Hiding Pascal or switching compact
panels retains its actual control, exact draft and paired baseline. View changes
replace the canvas deliberately; selection and value proposals keep it mounted.
Project title edits update the mounted generated source immediately without
replacing the title field that is notifying.

The starting document and initial review examples use English copy. Dedicated
editing, draft and persistence checks still exercise supplementary and
multilingual text. These qualification inputs do not limit the author's language.

Pages, reusable definitions and instances use the shared authoring commands.
The Properties/Events inspector creates Pascal TODO handlers and navigates the
actual source editor. Multiple callback registrations, exact removal warnings
and paired Undo/Redo use the same contracts as the browser. Designer controls
bypass application actions and runtime callbacks.

Save uses `TNyxProjectStore`: adjacent design/Pascal files, pending source and its
base are published through the qualified write-ahead paired contract. Expected
file revisions prevent silent overwrites. The warned saved-version action first
backs up the current exact pair under an independent key. Admission and failed
backup keep the current session. Output parameters must be distinct from inputs
at native file-call boundaries; qualification uses an independent returned
revision for its second writer.

## Shared service and concurrent projects

The native editor and browser share `TNyxStudioAgentBridge`, including revision
admission, protected local recovery, queued paired publications, server-owned
Undo/Redo and operator permissions. Platform adapters own HTTP and timers. Native
HTTP workers own bytes only; UI timers deliver replies on the UI thread. Pausing
detaches local notification, and destruction joins outstanding work before
releasing its borrowed session. Cancellation cannot revoke an already admitted
server operation. Five-second connect/read timeouts are per wait; a whole-request
deadline and stalled-service teardown qualification remain open.

Agents shows project sessions and activity in the same public Nyx composition.
Jumping waits for acknowledged local publications and successful admission of
the target. Each native context owns an independent mirror and an immutable
service bridge. Inactive bridges cannot retarget requests or repaint another
project. Return restores accepted/design/draft/base bytes, optional Pascal
visibility, scalar range/focus and stored scroll positions. Output machine
configuration is shared by this editor. The current journey qualifies draft,
history and source presentation; complete per-project presentation remains open.

A differing recovered pair is retained for explicit resolution. The native
**Save local backup and use shared design** action writes an independent paired
backup before adoption. Compiler diagnostics may borrow local accepted source
only after exact synchronization; consuming this metadata does not prove a build
request or compiled execution. Older servers visibly report unavailable project
closure instead of claiming confirmation succeeded.

## Qualification and remaining integration

The maintained actual-control journey consumes an unchanged MCP-authored export:

```powershell
./tools/build.ps1 -Target native-studio -VerifyNativeStudio `
  -DesignerSourceDirectory <semantic-export-directory>
```

The [Pascal consumer](../tests/nyx_studio_native_tests.lpr) uses the real controller
and actual native controls. Its full journey covers canvas proposals, recipe
independence, page/component navigation, properties, multiple callbacks, warning
confirmation, exact source/ranges, compact switching, paired Save/conflicts and
queued destruction. The optional third fixture argument `geometry` exercises
focused source and canvas visibility after compact parking without the full
callback/file journey. Artifacts and terminal counts belong to [WORK](../WORK.md).
An earlier longer callback journey captured a blank compact canvas despite
retained controls. The refreshed full English journey and focused geometry
journey both paint their memos. The earlier failure's cause remains unqualified;
reliable compact painting and native sidebar overflow stay under the original
native Studio acceptance.

The native controller remains an integration candidate, with Win32 evidence.
The maintained [service journey](../tests/nyx_native_workspace_tests.lpr) passes
49 checks: real MCP composition/observation, actual native Unicode input,
authoritative Undo/Redo, independent project drafts/history, full-editor return
and source focus/range restoration. Actual 390-pixel and desktop captures paint
their English memo. It also exercises deferred requests, canceled timers and
destruction with active HTTP work; heap tracing reports zero leaks. Reviewed
semantic cleanup restores both borrowed test projects' original pairs exactly.
The production primary and qualification primary remain unchanged. This bounded
journey does not establish reliable painting or performance across all workflows.

Delegated native build requests, compiled reload and general native import/export
remain open. Those buttons explain the missing native implementation even after
connecting. Selecting output profiles and ordinary local paired Save/Open remain
available. Successful live closure retains its separate qualification gate. This does
not accept full native parity, editor performance, another widgetset, hardware,
IME or assistive-technology behavior. Browser regression evidence qualifies the
shared shell separately; a native compilation cannot substitute for it.
