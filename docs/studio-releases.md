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

Preparation stops at a pristine candidate. Current service code writes runtime
jobs, preview artifacts, local profiles and enrollment beneath its repository
root, and open project sessions/history are retained in memory. An installed
runtime therefore needs an explicit preservation/deployment boundary; this verifier
does not promise that a live, writable service root stays a pristine package.
Those limitations belong to the existing service/reload and workflow owners.

A compiled candidate does not update the running MCP schema. Recipe editing over
authenticated HTTP and full observing browser Studio must be checked after a
permitted deployment. Preserve protected process identities and active pairs as
recorded in WORK.md; staging alone authorizes no restart or replacement.
