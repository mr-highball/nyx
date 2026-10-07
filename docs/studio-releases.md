# Frozen Studio release preparation

[Build entry point](../tools/build.ps1) · [Agent workflows](studio-agents.md) ·
[Current work](../WORK.md)

Prepare a complete backend/browser candidate without replacing a running Studio:

```powershell
./tools/build.ps1 -Target studio-release -ReleaseOutput build/studio-release/candidate
```

The destination must be new. The Pascal packager first copies owned `src/` and
`studio/` Pascal units/includes/programs into a frozen snapshot. The backend,
browser Studio, independent source worker and preview program are compiled from
that snapshot. The two production HTML hosts, matched pas2js runtime and MIT
license complete the bundle. Compiler units, logs and qualification fixtures stay
outside it. No listener, enrollment or project operation runs during preparation.

`release.nyx` records the source checkpoint, compiler version labels and a sorted
complete list of paths, byte lengths and MD5 fingerprints. The inventory owns
the exact snapshot bytes; the checkpoint alone does not identify uncommitted
source changes. MD5 detects accidental mixing and corruption; it is not a signed
release or an authentication mechanism. The manifest contains no machine paths.

The Pascal verifier refuses missing or changed artifacts, extra files/directories,
unsafe or duplicate paths, links/junctions, malformed metadata and unsupported
manifest versions. Existing destinations and sealed manifests refuse rather than
being overwritten. Failed preparation leaves an unsealed directory for inspection;
choose another destination after correcting the cause.

The maintained target verifies the real bundle and exercises corruption/privacy
refusals against a separate inert fixture. To recheck a pristine candidate, use
the packager built under `build/studio-release/tool`:

```powershell
./build/studio-release/tool/nyx_studio_release.exe verify build/studio-release/candidate
```

The initial Windows evidence qualifies FPC 3.2.0 and the installed matched pas2js
3.3.1 runtime. The native manifest consumer also executes under FPC 3.3.1. This
does not establish the supported-platform CI matrix or native Studio packaging.

The native host now consumes one immutable `TNyxStudioDirectories` value. Repository
mode retains the development layout. Release mode verifies the pristine payload,
borrows its compiler units and web artifacts read-only, and puts jobs, previews,
profiles and saved project pairs beneath a separate private runtime root. Each
admitted compiler worker copies that value, independently of later host navigation.
Factories create no directories and require ordinary ancestors; overlapping roots
and links/junctions refuse before writes. This is a trusted local host contract,
not protection against concurrent hostile filesystem replacement.

For an installed release, the launcher accepts explicit roles:

```powershell
./tools/studio.ps1 -ReleaseRoot build/studio-release/candidate `
  -RuntimeRoot .local/studio-runtime -BindAddress 127.0.0.1
```

This command launches a new server; it is not an update or restart command. Use a
new runtime and unused ports for a separate instance, and follow the preservation
gate in WORK.md before replacing any running service. Release launches skip Studio
compilation and require no application compiler. Configure outputs when requesting
a build. The launcher refuses earlier candidates that predate the separated-root
entry point; prepare a current bundle rather than modifying a sealed payload.

On Windows, an existing LAN firewall allowance can name the installed executable
as well as the port. A primary LAN update must retain that covered executable
path: preserve its backup, install only the verified candidate's server bytes and
keep the frozen payload/runtime arguments separate. Changing the payload directory
does not require changing the installed executable path. Confirm the exact process
identity before replacement, retain the full runtime checkpoint and verify the
payload and installed binary afterward. Same-host LAN requests establish local
delivery; access from a phone requires separate confirmation. The current retained
path and private preservation baselines belong to WORK.md.

MCP enrollment defaults to the runtime root. Optional `-EnrollmentRoot` chooses
the project whose `.codex/config.toml` is refreshed, including an explicitly enrolled
user entry. Its `.codex` and `.local` write locations must be outside the payload.
Optional `-MCPPort` defaults to the editor port plus one; MCP remains loopback-only.
Keep these private host paths and credentials out of exported designs and commits.

The maintained build can also qualify the actual protocol and compiler workers
without binding an HTTP/MCP listener:

```powershell
./tools/build.ps1 -Target studio-release -ReleaseOutput build/studio-release/checked `
  -VerifyReleaseRuntime -ReleaseRuntimeProfile .local/studio-outputs.nyx `
  -ReleaseRuntimeSourceDirectory build/content-editor/maintained/result
```

This opt-in check needs a private profile with both outputs ready and the unchanged
MCP/Studio-authored English companion at the supplied source directory. It owns a
new qualification runtime beside the bundle, exercises enrollment and profile
admission, delegates both real application builds, checks their immutable source
and artifact paths, and re-verifies the pristine payload. On Windows it also mounts
and closes the resulting native application. This is separate from the always-run
29 manifest/refusal checks. Browser execution requires a separately hosted artifact;
the runtime check alone does not prove browser interaction or authenticated HTTP.

The opt-in runtime check also compiles the native session/recovery consumers
against the frozen payload. It exercises ordinary engine recreation, denied writes
and an owned producer terminated before graceful shutdown, followed by a fresh
recovery process. Shared ownership checks execute natively; the maintained browser
fixture separately executes the same portable recovery/clone contract.

## Runtime session recovery

Current native hosts automatically maintain `.local/studio-session.nyx` beneath
the writable runtime root. This private version-1 checkpoint retains the primary
and up to eight ordinary projects: exact accepted design/Pascal files, pending
draft/base (including an empty draft), selection, active view, naming counter,
revision, agent enablement, registry identities and paired Undo/Redo entries.
Project exports remain portable and independent of this runtime file.

Editor commits/history/configuration, semantic durable mutations and ordinary
project creation/closure use the existing serialized document boundary. Each call
prepares independent rollback owners, stages the operation and publishes the
checkpoint before acknowledgment. A failed write restores the complete in-memory
baseline, including transient retry/ticket values, and returns a refusal. Read-only
queries do not rewrite the checkpoint. A process-lifetime lock prevents two new
hosts from sharing one runtime; the OS releases it after abrupt termination.

Recovery admits every complete project/history pair before publishing new owners.
Missing files allow first launch; corrupt, appended, unsupported-version or invalid
state refuses launch before MCP enrollment, retaining the input file. This avoids
silently replacing recoverable work with a sample. Keep the failed runtime intact
for diagnosis and explicitly choose a separate runtime if starting independently.

The stream caps the file at 512 MiB, each UTF-8 field at 4 MiB, nine sessions and
fifty total paired history entries per session. Its trailing MD5 is an accidental
integrity check. A unique sibling is flushed and closed before same-directory
replacement; Windows uses the replacement/write-through flags from
[MoveFileExW](https://learn.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-movefileexw).
The Windows qualification covers process termination and denied replacement;
it does not establish hardware power-loss or other-OS/filesystem durability.

Connection credentials, activity/presence, retry authority, review tickets and
compiler jobs expire at restart. They are excluded from the checkpoint; each new
host rotates and enrolls its connection authority. Ordinary project handles and
operator enablement remain durable. The portable clone/recovery values work on
both compiler targets; disk ownership and OS replacement stay native host concerns.

The checked native recovery consumer also accepts `--retained <verified-release>
<copied-runtime> <private-observing-baseline.json>`. Compile that consumer against
the candidate's frozen units before running it. Supply an independent private
runtime containing an exact checkpoint copy; the live runtime stays owned by its
server and must never be used for this qualification. The bounded baseline has
the primary first and up to eight ordinary contexts, with exact encoded pairs,
labels, handles and public session fields. The consumer admits all accepted/history
pairs, compares observations and requires a byte-identical native round trip.
A mismatch refuses before Save; it starts no listener or enrollment and prints
no project text or credentials. This proves candidate compatibility with retained
work, separately from generic recovery fixtures and authenticated rollout checks.

## Explicit legacy test bootstrap

`nyx.studio.legacy` admits a bounded observing snapshot when an older host has
no native runtime checkpoint. This is a separate explicit bootstrap contract;
production migration should retain the complete runtime checkpoint instead.
It preserves exact accepted design/source, pending draft/base (including a defined
empty draft), public revision, selection, view, labels and operator permission.
It cannot infer private legacy naming counters, registry serials or history.

Default `nlhRequireEmptyHistory` refuses any reported Undo/Redo. The typed
`nlhResetTestHistory` policy explicitly permits unavailable test history to reset;
it is never selected because a runtime is malformed. Both policies reset unknown
naming counters and assign a fresh ordinary workspace epoch, returning a small
old/new handle mapping. Retired handles refuse and cannot alias later projects.
The primary context remains the primary context. Original production history,
isolation and rollout acceptance requirements stay unchanged.

The Pascal host tool admits every pair before creating a fresh runtime, saves
through the ordinary bounded/atomic recovery store, then loads it again and
compares exact recovery stamps. Existing destinations refuse without changes.
It starts no listener, selects no live project and refreshes no credentials:

```powershell
./tools/build.ps1 -Target legacy-snapshot
./build/legacy-refresh/maintained/native/nyx_studio_seed.exe `
  <observing-snapshot.json> <verified-release-root> <new-runtime-root> require-empty-history
```

Use `reset-test-history` only with explicit operator authorization for disposable
test sessions. The input is an array with the primary first and at most eight
ordinary entries. Each entry contains workspace, label, exact encoded project,
and session metadata (revision, permission, selection, view, pendingDraft,
canUndo, canRedo). Unknown fields, inconsistent draft metadata, invalid pairs or
navigation, duplicate handles and a reused creation epoch refuse.

The maintained shared fixture passes 16 checks in checked matched FPC and actual
browser execution. The current eight captured test pairs save/load with zero
native leaks; strict-history and existing-destination refusals preserve storage.
See [the rollout return path](../WORK.md#current-return-path-test-mode-observing-release-refresh--2026-10-06).

## Qualify an observing editor

Build the maintained Windows Pascal observer with
`./tools/build.ps1 -Target release-observer`. It uses authenticated semantic MCP
for document queries, grouped policies, history and real compiler jobs. It observes
the ordinary editor through Chromium anonymous pipes on real clocks; host pointer
input only opens panels and the source modal. It starts no service/debugger listener,
injects no scripts and never claims or replaces the primary project.

First create an explicitly owned empty ordinary workspace through `nyx_workspaces`,
then compose `tests/date-field-review.operations.json` through one `nyx_transaction`
at its exact revision. Configure output compilers through ordinary Studio Outputs.
Run the resulting program against the current enrolled default-port service:

```text
nyx_studio_release_observer <enrolled-repository> <owned-workspace> <fresh-evidence-directory> 1100
nyx_studio_release_observer <enrolled-repository> <same-owned-workspace> <another-fresh-directory> 390
```

The explicitly supplied workspace is modified and retains its paired history.
Each run inspects existing policy before restoring inheritance, applies two
independent typed date ranges, refuses a default-incompatible change and exercises
one paired Undo/Redo. It observes actual Inspector values and the entire canonical
LF-terminated companion through source modal opening/closing. Desktop also builds
both application targets and saves bounded terminal status packets. Compilation
alone does not establish native execution or parity. Do not run this destructive
fixture against a user's authored project. All evidence stays under ignored output.

The physical observer distinguishes absent fields from present empty controls and
uses the protocol's separate input/textarea value tables; see the primary
[Chromium DOMSnapshot contract](https://raw.githubusercontent.com/ChromeDevTools/devtools-protocol/master/pdl/domains/DOMSnapshot.pdl).
This mechanism validates presentation selectively; it does not replace bounded
semantic MCP as the design authoring/inspection contract.

The earlier legacy host predated native recovery and could not export full
history. Its CanUndo/CanRedo flags established availability, not serialized stacks;
the explicitly authorized test bootstrap did not qualify production migration.
The current observing host now uses native recovery. Its contextual-help refresh
retains nine exact pairs, complete byte-identical checkpoint/history and ordinary
handles through restart and authenticated desktop/narrow observing checks. See
[current rollout evidence](../WORK.md#current-return-path-contextual-help-observing-release--2026-10-06).
Full production migration from an older host remains open. A staged candidate
alone is not authority to replace a service or discard its history.

A compiled candidate does not update the running MCP schema. Recipe editing over
authenticated HTTP and full observing browser Studio must be checked after a
permitted deployment. Preserve protected process identities and active pairs as
recorded in WORK.md; staging alone authorizes no restart or replacement.
