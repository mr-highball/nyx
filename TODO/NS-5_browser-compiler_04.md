# NS-5_browser-compiler_04 - Package virtual project inputs and cached toolchains

[North stars](../MILESTONES.md) · [Task catalog](README.md) · [Task flow](../TASKFLOW.MD)

**Description:**

Supply browser-local compilation with a bounded virtual file system and a
versioned static compiler/RTL/Nyx SDK package. Resolve real unit/include/resource
closure using immutable captured inputs, without access to host filesystem paths
or dynamic server compiler routes.

Starting evidence: [captured project inputs](../studio/nyx.studio.compilerinputs.pas),
[Pascal projects](../docs/pascal-projects.md), the
[Athena cache boundaries](../athena/docs/fpc-builds.md#cache-boundaries) and
[browser hosting rules](../athena/docs/platforms/browser.md#hosting).

North-star owner: NS-5. Milestone outcome: optional client-only WEB authoring under M4, consumed by M5 delivery.
Completion credit: pending evidence-backed assessment. These are open tasks;
planning earns no implementation credit and changes no existing acceptance,
counter or DONE status. Existing host/native and language-engine owners retain
their full requirements.

**Acceptance Criteria:**

- Define normalized portable virtual paths, deterministic unit/include search
  order and case/collision policy. Capture exact main, units, physical includes,
  resource bytes and compiler defines/options; refuse traversal, duplicates,
  unresolved inputs and dependencies outside the declared closure.
- Package the verified WASM compiler, worker/loader, matched RTL, required
  compiler units, Nyx SDK and resources under a versioned manifest with relative
  paths, lengths, hashes and license notices. Separate build-time compiler
  inputs from runtime resources and package every required dependency.
- Key toolchain/compiled-unit/artifact caches by compiler, RTL/SDK revision,
  target, options/defines, protocol/package version and relevant source hashes.
  Different configurations cannot reuse incompatible units or output.
- Stage updates atomically and verify complete manifests before activation.
  Interrupted downloads, mismatched hashes, stale cache entries and mixed
  generations fail clearly while preserving the last usable package.
- Resolve worker, WASM, RTL, include, resource and application asset URLs from
  the deployment base at both root and repository subpaths. Validate HTTP
  status, actual MIME/type, length and hash; an HTML error page cannot become
  a compiler or successful cached module.
- Separate fresh online startup, warm cached startup, fully cached offline
  operation and first-visit offline failure. Explicitly document download sizes,
  required cached closure, quota, private-mode and eviction limitations; never
  promise offline readiness from a service-worker registration alone.
- Bound cache storage and retirement while keeping active worker packages and
  user projects intact. Reject incomplete packages and recover after cache loss.
  No required CDN, server compiler, custom response headers, cross-origin
  isolation headers or dynamic server route may be assumed for Pages operation.
- Exercise real multi-unit/include/resource compilation at a subpath and after
  update, interrupted staging and eviction. Reproduce a clean uncached build;
  cache hits cannot conceal a missing declared input.

**Blockers**

- [Qualified toolchain](NS-5_browser-compiler_01.md).
- [Browser worker lifecycle](NS-5_browser-compiler_03.md).
