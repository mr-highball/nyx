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

Open project sessions, navigation, drafts and Undo/Redo history remain memory-only.
Explicit restart preservation and the observing HTTP rollout are still required
within the existing service/reload and workflow owners. Saved project pairs alone
do not satisfy that session-preservation gate.

A compiled candidate does not update the running MCP schema. Recipe editing over
authenticated HTTP and full observing browser Studio must be checked after a
permitted deployment. Preserve protected process identities and active pairs as
recorded in WORK.md; staging alone authorizes no restart or replacement.
