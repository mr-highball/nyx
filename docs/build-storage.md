# Constructor build storage

[Architecture](architecture.md) · [Source projection](source-projection.md) ·
[Storage task](../TODO/DONE/NS-5_projection-storage_01.md)

Compiler-executed source projection returns an independently owned design and
diagnostics. A native constructor receipt advertises no executable artifact;
a browser receipt still needs its complete compiled worker package. Application
outputs use their separate build lifetime and are not subject to this policy.

The native host now defaults to retaining source, wrapper, result and compiler/
execution logs, while retiring unchanged native compiler derivatives after both
process families join. It requires 128 MiB available before allocating another
invocation. This is an available-space preflight, not a reservation or a guarantee
against another process filling the drive. Authoring and existing projects remain
independent of compiler readiness and storage admission.

## Typed host choices

Configure a copied policy before delegating an executor to a worker:

```pascal
LExecutor.ConfigureProjectionStorage(
  TNyxProjectionStoragePolicy.Default
    .Retaining(prEvidence)
    .MinimumFreeBytes(256 * Int64(1024) * 1024));
```

`prAllFiles` preserves native compiler outputs for debugging. Zero minimum bytes
explicitly disables the capacity preflight; negative values and unknown retention
choices refuse before allocation. Browser packages are preserved with either
choice. The overloaded `NewNyxNativeSourceCompiler` accepts the same typed policy
and copies it into each independent worker. Wire/editor documents cannot set
machine paths, compiler arguments or retention policy.

Native executor receipts optionally expose `INyxProjectionStorageBuild`. Its
`Storage` record distinguishes unallocated, preserved, retired, deferred and
capacity-refused outcomes. Captured/retired file counts and retired logical byte
counts describe only this invocation. Deferred items count bounded refusal
observations, not a complete recursive inventory. Files held by unrelated readers
can delay physical space recovery; the byte count is not reserved free space.

This facet holds detached values, not filesystem authority. Existing base receipt
GUIDs and strict source-build wire shapes remain unchanged. A transport-decoded
receipt does not gain authoritative local storage evidence. Storage refusal stays
separate from compilation/execution: a valid design can survive deferred cleanup.
Capacity refusal instead returns the existing typed `spsUnavailable` state with
a useful message before an invocation is created.

## Ownership and retirement

The current Win32 adapter creates an exclusively new host-minted job directory.
It never adopts an existing job. Ordinary directory handles pin every admitted
ancestor and the job/units directories without write/delete sharing. Read access
is essential: metadata-only opens did not enforce that sharing guard in actual
qualification. Root moves, child moves, external hardlink creation and a reparse
writer are refused while the relevant directories are pinned. Reparse ancestors
are rejected before touching their targets. DOS device names are not accepted as
ordinary source filenames, even when they are valid Pascal identifiers.

The compiler runner joins its exact invocation/family on every exit. Capture
then records only its known flat compiler derivatives, before constructor code
runs. Source/wrapper evidence is created exclusively and held for exact readback.
Native result reads refuse links and excessive byte counts before allocation;
source, result and logs remain pinned through retirement. Existing log files
created by a constructor are preserved and reported as an evidence refusal.

Capture is limited to 4,096 entries per directory, 64 MiB per derivative and
256 MiB of total hashed bytes. It does not descend into unknown directories.
Unexpected entries, enumeration/hash failures and uncaptured late files remain
with a deferred report. The identity handle prevents recycling a captured file
ID. Retirement reopens with read/delete access and no write/delete sharing, then
checks volume/file identity, single-link count, size, creation/write times and
SHA-256. A changed or locked file remains. Deletion uses that verified handle,
not a filename resolved after comparison. Final enumeration never captures a new
file for deletion. Finishing twice has no additional effect.

The destructor only closes handles. It never assumes that a compiler has joined
or performs cleanup while a process might still be using its outputs. The executor
owns that ordering and holds no editor/model lock. Sources and logs also survive
compiler failure and cancellation; collected execution log bytes remain exact.

This is ownership of a private constructor evidence directory, not a sandbox for
trusted Pascal code's own filesystem/network access. Do not rename/link compiler
files from a constructor. General artifact sweeps, application caching, cache
reuse and the full compiler service lifetime remain with their existing tasks.
Nothing here authorizes deleting an older runtime, project or active output.

The Win32 implementation follows the documented
[CreateFile sharing and reparse flags](https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-createfilew),
[opened-handle information operations](https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-setfileinformationbyhandle)
and [one-byte disposition structure](https://learn.microsoft.com/en-us/windows/win32/api/winbase/ns-winbase-file_disposition_info).
Actual sharing/retirement behavior is established by the maintained host checks,
not inferred from successful compilation.

## Evidence and remaining hosts

`tests/nyx_projection_storage_tests.lpr` passes **80** checked assertions on
FPC **3.3.1-20634**, Win32/i386, with zero unfreed blocks. The complete actual
constructor preserves helpers, class methods, loops, reusable controls,
state/resources and supplementary Unicode against independent expected meaning.
Actual type failure, throwing construction, running cancellation, capacity
refusal, owning native worker, exact retained evidence, policy override and
filesystem refusal paths are included. A real junction target remains untouched;
ordinary move/hardlink attempts are blocked before mutation. Changed contents
with equal size/write time remain, exercising the hash guard rather than merely
timestamp comparison. The worker's one fresh invocation retains its evidence and
has no executable after terminal host release.

The same host actually compiles a pas2js worker and reads back its complete
preserved package after executor release. This is package preservation evidence;
it does not claim another HTTP/browser execution, UI interaction or visual parity.
The maintained `source-projection` build target registers this separate consumer.
Its deliberate zero-capacity override keeps qualification of retirement distinct
from the independently tested capacity refusal. Run only with enough real space
for an actual compiler invocation.

Other native ownership adapters refuse before allocation until qualified. The
existing application Build path is unchanged; this does not withdraw the broader
native/LCL parity requirement. General OS/widgetset qualification, application
cache/performance, HTTP/browser integration and preserving installed rollout stay
open. The current packet's full native Studio link hit disk pressure; compile-only
LCL compatibility must not be presented as a completed new native Studio binary.
