# JSON Workshop

A practical Tool Canvas made from ordinary Nyx controls and reusable logic.
Edit the JSON in the notebook, choose **Format JSON**, or choose **Save JSON**
to format and save the current input. The operation cards expose the saved
Parse JSON → Format JSON → Encode UTF-8 → Save file graph for further editing.
Pan the workspace to explore the notebook and its operation cards.

[nyx.json.workshop.pas](nyx.json.workshop.pas) is the portable document builder.
It composes a reusable Card, Memo, buttons, Code display and receipt labels.
Two typed graphs distinguish formatting from saving. Routes bind the real
control events and state directly; there is no handwritten JSON-result handler.
The canvas declaration retains embedded content and operation positions.

[json_workshop.lpr](json_workshop.lpr) hosts that document with LCL or pas2js.
Its file host is supplied explicitly by the application. The saved document
contains save options, not file-system authority. Browser saving requests a
fresh Nyx confirmation when the original click's activation has expired.
Cancellation preserves the source. A browser download request is reported as
an accepted request; only a written-file result shows **The file was written**.
Browser page navigation retires pending work; returning from the back/forward
cache creates a fresh workshop runtime.

The preview and receipt describe the last accepted result. Invalid JSON does
not replace that result or request a save. The Tool Canvas's execution feedback
retains the graph failure diagnostic. Runtime state belongs to each application;
editing the notebook does not rewrite its authored defaults.

Build and qualify from the repository root:

```powershell
./tools/build.ps1 -Target json-workshop
```

Add `-JSONWorkshopStudio` to begin that same journey in ordinary Nyx Studio.
It opens the Save and Format graphs, changes their indentation from two to four
spaces through the Logic form, and checks that unfinished input survives a tab
change. Apply changes the running graphs; **Save graphs** publishes their design
and Pascal together. Undo/Redo and ordinary project Save/Open preserve that pair.
The command then compiles the saved constructor and runs the original control
journey against its four-space configuration. Native file outcomes remain
injected; the browser assertion page substitutes only file bytes and editor
transport. Browser execution is a separate check.

Configure `LCL_FPC`, `LAZARUS`, `PAS2JS`, `PAS2JS_RUNTIME`, `EVIDENCE_ROOT` and
`TEMP_ROOT` in the private toolchain configuration. Alternatively pass a fresh
absolute `-JSONWorkshopEvidenceDirectory`. The command prints its output packet,
routes owned process `TEMP`/`TMP`, and starts no server. Optional
`-JSONWorkshopPlatform` and `-JSONWorkshopNativeUnitCache` reuse a known platform
and closed native unit outputs; normal FPC source/dependency checks still apply.

Run the public native example at `<packet>/native/json_workshop.exe` (without
the suffix on other native platforms). Serve the browser directory over HTTP
with a Pascal host and open `json_workshop.html`. The matched `rtl.js` is staged
beside the generated program. The example launcher uses real platform save
services; choosing Save may open a picker or request a browser download.

The same build runs the actual-control consumer at desktop and compact widths,
exports its exact project/Pascal pair, compiles that constructor and repeats the
journey in its generated application. The native consumer injects file outcomes
and checks exact UTF-8 bytes, cancellation, invalid-input refusal, page retirement
and independent reopened applications. Those injected receipts are not evidence
of a physical picker, disk write or download. Browser assertion pages and the
compiled constructor's exact project input are staged separately; compilation
alone does not establish browser interaction, accessibility or visual quality.

Open the exported `workshop.nyxproject` in Nyx Studio to continue designing.
File saving is an explicit local output-host setting when launching from Studio;
it is deliberately absent from the portable project. Broader graph/port authoring,
actual file outcomes and full browser/LCL parity retain their backlog acceptance
requirements.
