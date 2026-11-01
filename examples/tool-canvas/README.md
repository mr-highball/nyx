# Tool Canvas workshops

## Resource Workshop

[nyx.resource.workshop.pas](nyx.resource.workshop.pas) builds a reusable team panel
inside the public Tool Canvas. **Load embedded sample** and **Load hosted team**
invoke the same typed graph with different declared references: resource → items
selection → active-person filtering → exact rank sorting → Table/summary.
Failures keep accepted rows and expose loading/cache diagnostics. Numbers above
the IEEE integer range retain their exact spelling in the Table.

[resource_workshop.lpr](resource_workshop.lpr) hosts the identical document in LCL
or pas2js, using on-demand resource loading. Its default hosted URL is illustrative;
pass a public JSON URL to `BuildResourceWorkshopDocument` or edit the declared
resource in Studio. Cache policy stays on that resource. The saved graph holds no
transport authority and requires no handwritten result callback.

Expand **Load a URL directly** to edit a typed URL-bound input and invoke the
separate HTTP graph. Response bytes pass through strict UTF-8 decoding, JSON
parsing and the same selection/filter/sort recipe. The HTTP status has its own
integer state binding. Non-success responses retain the accepted rows and summary;
invalid JSON or row families refuse the whole mixed publication. The launcher
explicitly supplies the platform HTTP transport, independently of resource loading.

The maintained Pascal consumers `nyx_resource_logic_tests`,
`nyx_resource_logic_authoring_tests` and `nyx_resource_workshop_controls` separate
scope/cache/lifetime checks, typed form/source/semantic history and actual
embedded controls. Define `NYX_RESOURCE_WORKSHOP_STUDIO` for the last consumer to
begin in ordinary Studio: load the resource step, retain unfinished Unicode input
across tabs, reject invalid options, set its deadline, Apply, Save graphs, Undo/
Redo and Save/Open the project. The resulting exact source is separately compiled
with `NYX_COMPILED_RESOURCE_WORKSHOP`. Its native/browser Table journey uses the
same actual controls. Controlled byte hosts do not prove a real HTTP origin.
The additional `nyx_resource_http_controls` consumer uses the actual platform
transports and the maintained `tests/fixtures/resource-workshop/` responses served
by an explicitly configured Pascal host. Its native arguments are an evidence
directory, viewport width and fixture base URL ending in `/`; the browser consumer
uses its current origin's `/resource-http-01/data/` fixture prefix. Define
`NYX_COMPILED_RESOURCE_HTTP` to execute an independently compiled saved constructor
from `nyx.generated.view`. This journey also retains newer-edit, navigation and
independent-application checks. Local HTTP is distinct from HTTPS/CORS coverage.
WORK.md owns current results and remaining observing delivery, instance/remount,
physical/assistive and performance acceptance. The desktop/narrow input assertions
do not establish full aesthetics.

## JSON Workshop

A practical Tool Canvas made from ordinary Nyx controls and reusable logic.
Choose **Open JSON** to load a file, enter an HTTP(S) **JSON URL** and choose
**Load URL**, or edit/paste JSON in the notebook, then
choose **Format JSON** or **Save JSON**. Plain JSON and complete Markdown fences
labelled `json` or `JSON`, or without a language label, are accepted. The
operation cards expose the saved Prepare JSON → Parse JSON → Format JSON →
Encode UTF-8 → Save file graph for further editing.
Pan the workspace to explore the notebook and its operation cards.

[nyx.json.workshop.pas](nyx.json.workshop.pas) is the portable document builder.
It composes a reusable Card, Memo, buttons, Code display and receipt labels.
Four typed graphs distinguish opening, URL loading, formatting and saving. Routes bind the real
control events and state directly; there is no handwritten JSON-result handler.
The canvas declaration retains embedded content and operation positions.

**Prepare JSON** is an authored Pascal program, executed by Nyx's portable
interpreter. It removes surrounding whitespace, a leading Unicode BOM and a
complete fence; incomplete or differently labelled fences still reach the strict JSON
parser. It does not rewrite the entered source, round JSON numbers or acquire
file/network authority. Its typed contract accepts `Source: String` and returns
`Prepared: String`, with a caller-visible execution budget.

Open **Pascal programs**, choose **Prepare JSON**, and customize the source.
**Check program** reports source-line diagnostics. **Apply live program** updates
the referencing Open, Load URL, Format and Save graphs in this canvas. Invalid or unfinished
source leaves the accepted program working and survives tab navigation. Separate
applications own their live definitions independently.

Live edits in the standalone example last for that runtime. To retain a customized
tool, open its exported project in Studio, apply the program, choose **Save graphs
and connections**, then save the project. This deliberately preserves the
difference between a live experiment and an authored application.

[json_workshop.lpr](json_workshop.lpr) hosts that document with LCL or pas2js.
Its HTTP transport and read/save hosts are supplied explicitly by the application.
The saved document contains options, not network or file-system authority. Browser saving requests a
fresh Nyx confirmation when the original click's activation has expired.
Cancellation preserves the source. A browser download request is reported as
an accepted request; only a written-file result shows **The file was written**.
Browser page navigation retires pending work; returning from the back/forward
cache creates a fresh workshop runtime.

Opening uses a one-MiB read budget and a JSON extension hint, both editable through
the normal operation configuration. The complete selected snapshot must decode as
UTF-8 and parse as JSON before source, preview and filename publish together.
Cancellation retains the last accepted content and filename. Invalid bytes/JSON
retain the working state and an inspectable route diagnostic. Newer text entered
while a read is pending refuses that stale result; navigation retires the request.
The source keeps its decoded file text, including an optional BOM/fence, while the
preview shows the prepared JSON. Each graph's formatter remains independently
configurable. Opening a file does not grant permission to overwrite its path.

**Load URL** connects the URL input to the existing HTTP operation's optional
`NyxHTTPPort(nhpURL)` input. The input overrides the configured request URL for
that invocation; it preserves method, headers, body, deadline and byte limit.
An absent input keeps the configured URL. A present empty or invalid URL refuses
before sending, so an empty notebook URL never fetches an example by accident.
`TNyxHTTPRequest.WithURL` provides the same validated value derivation in fluent
application code. Request configuration remains unchanged by runtime inputs.

The network graph uses a fifteen-second whole-request deadline and a one-MiB
response limit. A complete 2xx response passes through strict UTF-8 decoding,
**Prepare JSON**, parsing and formatting. Source, preview, accepted URL and status
publish together. Invalid UTF-8 or JSON publishes no partial result; newer edits
refuse stale completion, and page retirement cancels the receiver. Non-2xx replies
and transport errors retain the accepted notebook and report their status/error.
The status label describes the last published result. Browser requests follow
the browser's origin, CORS and mixed-content rules; Nyx does not bypass them.

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
The browser build includes the Pascal source worker required for that publication.
The command then compiles the saved constructor and runs the complete control
journey against its four-space configuration. Native file and HTTP outcomes remain
injected; the browser assertion page substitutes file/network bytes and editor
transport. Browser execution is a separate check.

The maintained browser consumer now also runs this ordinary Studio journey,
waiting for delivered shared-source observations before driving the editor. It
retains the actual exported/imported project as inert `workshop-saved-project`
text for independent compilation; it does not substitute a pre-save fixture.
WORK records executed desktop/compact evidence and the distinct native, browser,
controlled-host and physical-input limits.

Configure `LCL_FPC`, `LAZARUS`, `PAS2JS`, `PAS2JS_RUNTIME`, `EVIDENCE_ROOT` and
`TEMP_ROOT` in the private toolchain configuration. Alternatively pass a fresh
absolute `-JSONWorkshopEvidenceDirectory`. The command prints its output packet,
routes owned process `TEMP`/`TMP`, and starts no server. Optional
`-JSONWorkshopPlatform` and `-JSONWorkshopNativeUnitCache` reuse a known platform
and closed native unit outputs; normal FPC source/dependency checks still apply.

Run the public native example at `<packet>/native/json_workshop.exe` (without
the suffix on other native platforms). Serve the browser directory over HTTP
with a Pascal host and open `json_workshop.html`. The matched `rtl.js` is staged
beside the generated program. The example launcher uses real platform read/save
services; Open requests a file chooser, Load URL sends through the explicit HTTP
transport, and Save may request a picker or download.

The same build runs the actual-control consumer at desktop and compact widths,
exports its exact project/Pascal pair, compiles that constructor and repeats the
journey in its generated application. The native consumer injects file outcomes
and checks exact UTF-8 bytes, read/save cancellation, invalid-input refusal,
stale-read protection, page retirement, live Pascal replacement/refusal and
independent reopened applications. URL cases additionally cover input propagation,
preserved request options, status-label binding, non-2xx, malformed UTF-8/JSON,
transport failure and stale/retired replies. An explicitly supplied native test
origin (`NYX_JSON_WORKSHOP_HTTP_ORIGIN`) can exercise the same graph through its
platform HTTP transport and the existing owned HTTP qualification fixture. This
option starts no server and is absent from portable project data. Those
injected receipts are not evidence
of a physical picker, disk write or download. Browser assertion pages and the
compiled constructor's exact project input are staged separately; compilation
alone does not establish browser interaction, accessibility or visual quality.

Open the exported `workshop.nyxproject` in Nyx Studio to continue designing.
File reading/saving are explicit local output-host settings when launching from
Studio; authority is absent from the portable project. Broader graph/port authoring,
actual file outcomes and full browser/LCL parity retain their backlog acceptance
requirements.
