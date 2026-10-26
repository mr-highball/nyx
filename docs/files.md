# Portable text files

[Project](../PROJECT.md) · [Building](building.md) · [Native Studio](native-studio.md)

`nyx.files` exposes immutable UTF-8 text files and an interface for local picking
and export. The contract contains no DOM element, LCL control, stream or machine
path. `NewNyxBrowserTextFiles` and `NewNyxLCLTextFiles` supply the target adapters
at the application's composition boundary. A host may inject another exchange.
Studio consumes this public contract for its ordinary project files.

```pascal
const
  CJsonExtension: TNyxText = 'json';
  CTextExtension: TNyxText = 'txt';

procedure TNotebook.OpenNotes;
var
  LSelection: TNyxTextFileSelection;
begin
  LSelection := NyxTextFileSelection
    .Allow(CJsonExtension)
    .Allow(CTextExtension)
    .UpToFiles(4)
    .UpToBytes(256 * 1024);

  FLocalFiles.Pick(LSelection, @FilesPicked);
end;
```

`FLocalFiles` is an owned `INyxTextFileExchange`. The callback is an object method
with `TNyxFilePickStatus`, `const TNyxTextFiles` and `const TNyxText` error
arguments. Native dialogs may complete inline; browser reads finish later. Cancel
the exchange before destroying the receiver. `Cancel` silently detaches that
borrowed method; it does not deliver another callback. Both adapters retain
themselves through a completion that may release their owner.

Build selections through `NyxTextFileSelection`, whose defaults allow one file
up to 4 MiB. Extensions are open ASCII metadata without a leading dot or wildcard.
Empty filters allow any text-file suffix. The caller may select up to 32 files,
with a complete group bounded to 8 MiB; the per-file ceiling remains 4 MiB. Fluent
copies have independent extension vectors on native FPC and pas2js.

`NyxTextFile(Name, Text)` admits a leaf name and exact scalar text into an
`INyxTextFile`. It owns its immutable name, value and UTF-8 byte count. Names cannot
contain paths, reserved filename characters, control characters, or trailing
spaces/dots. The host's filesystem may impose additional name restrictions.
`NyxTextFileBytes` strictly decodes UTF-8: malformed bytes refuse, rather than
becoming replacement characters. Empty content, NUL and supplementary Unicode
remain exact. This is a text contract; binary resource admission remains under
the [resource contract](resources.md).

`ExportFiles` validates the complete group before delivery. Native uses save
dialogs and exact UTF-8 byte streams; browser uses temporary download anchors.
The adapter retires temporary host objects even on refusal. A `True` result means
all host deliveries were accepted, not that browser downloads reached a physical
disk. Multiple files are separate writes: canceling a later save may leave earlier
files delivered. Ordinary exports do not change supplied values or editor history.

## Incremental saving

`nyx.files.save` retains its existing bounded whole-byte save contract. Standard
LCL/browser hosts additionally expose `INyxFileSaveStreamHost`; existing custom
byte-only hosts do not need to implement it. Obtain that optional interface from
the explicitly injected runtime host, rather than from serialized model data.

`nyx.bytes.sources` supplies immutable `INyxByteSource` and independently opened
`INyxByteReader` cursors. `NyxUTF8ByteSource` validates and retains text without
another complete encoded byte array. Each positive bounded read returns owned
bytes; chunks may split UTF-8 sequences, preserving their exact concatenation.
Alternative sources must provide stable known lengths/content and fresh cursors.

```pascal
LSource := NyxUTF8ByteSource(LReviewedText);
LOptions := NyxFileSaveStreamOptions(
  NyxFileSaveOptions('unfinished-draft.txt')
    .MediaType(NyxMediaType(nfmtText)))
  .MaximumBytes(128 * 1024 * 1024)
  .ChunkBytes(64 * 1024)
  .DownloadBufferBytes(16 * 1024 * 1024);

FRequest := FStreamHost.BeginStream(LOptions, LSource,
  FScheduler, FScope, @Saved);
```

Here `LOptions` is `TNyxFileSaveStreamOptions`, `LSource` is `INyxByteSource`,
`FStreamHost` is `INyxFileSaveStreamHost`, and `Saved` has the ordinary
`TNyxFileSaveReply` signature. Cancel `FRequest` before disposing its borrowed
receiver. Receipts use `ValidateNyxStreamFileSaveResult` with the admitted total
limit; the old direct validator retains its 4 MiB cap. Stream limits are runtime
authority and do not widen the existing saved byte-file schema.

Defaults are 64 MiB total output/download buffering and 64 KiB chunks. Caller
limits are independent: zero total permits an empty file; zero download budget
still permits incremental picker output. Per-read chunks are 1..1 MiB. Sources
exceeding the total budget refuse before opening; early EOF, excess bytes or an
oversized returned chunk cannot claim successful delivery.

Native saving writes cooperative UI chunks into an exclusively created sibling
stage and replaces the selected file only after exact EOF. Request cancellation,
reader failure and skipped scheduler turns retire owned staging output. Browser
picker saves await each write, close only at exact EOF and abort uncommitted
output when possible. Blob downloads inherently buffer complete output within
their separate logical budget; this is not a peak engine-memory guarantee.
They report DownloadRequested, never guaranteed disk delivery. Current browser
activation/API/security rules still apply; the caller's existing typed download
activation option remains available.

Portable tests exercise exact scalar/NUL boundaries, independent cursors, more
than 4 MiB, strict lengths/chunk bounds and cancellation. The native dialog test
checks actual disk bytes, cancelled/malformed replacements, scheduler shutdown,
owned-stage removal and compatibility with the original direct byte path.
Browser consumers compile; executed browser picker/download streaming, physical
input and accessibility remain separate qualification requirements.

## Studio project files

Open the Project file section at any time; no output selection or compiler is
required. On compact browser layouts, Open is available through Actions / Project.
The shared `nyx.studio.files` functions define both controllers' file meanings:

- A `.nyxproject` backup contains accepted design/Pascal and any exact unfinished
  draft/base. Incoming bytes remain available for explicit conflict review.
- Adjacent `.nyx` and `.pas` files are paired by suffix, in either selection order.
  Duplicate or missing members refuse. They contain the accepted companion;
  a project backup separately retains an unfinished buffer.
- Import stages complete admission before replacing either accepted owner. A
  disagreeing or unsupported companion offers explicit Pascal/design choices,
  input export and cancellation. Opening design retains unsupported Pascal as a
  draft. Both controllers retain the current project when admission fails.
- A delayed picker result cannot replace a different project/load with matching
  IDs. Changing the pair while files are read requires an explicit review.

Native named Save/Open continues to use its owned paired repository. Local
export uses ordinary host dialogs; that separate delivery is not the repository's
atomic paired journal. Machine locations never enter the portable document.

## Current qualification and limits

Checked native and actual HTTP pas2js shared admission pass 590 checks, including
16 public file/selection assertions. Ordinary Win32 Studio passes 64 file
controls: actual UTF-8 disk boundaries, import, exports, Save/Open, retained drafts,
conflict choices, page/reusable navigation and compact source expansion. The
native OS chooser is substituted at its interface; physical chooser interaction
is not claimed. Its exact exported Pascal compiles and reconstructs the design.

The ordinary desktop browser journey passes 26 assertions using actual File
objects and FileReader callbacks. Its two-page reusable seed is composed through
authenticated semantic MCP and compiles on both application targets. Selective
desktop/native source captures are inspected. Browser disk delivery, physical
phone/IME input and accessibility remain separate qualification boundaries.

The CSS-390 journey reaches its imported design/source capture, then fails during
recovery because the expected View submenu is absent. Background configuration or
agent refresh is a suspected cause, not an established diagnosis. Full compact
recovery and installed LAN delivery remain open; see [the current packet](../WORK.md#current-return-path-ordinary-portable-project-files--2026-10-10).
