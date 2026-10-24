# NS-5_browser-compiler_07 - Qualify the static WEB edition and delivery path

[North stars](../MILESTONES.md) · [Task catalog](README.md) · [Task flow](../TASKFLOW.MD)

**Description:**

Deliver a reproducible static Nyx Studio edition whose browser performs WEB
compilation locally. Users must be able to design, edit, compile, preview and
save/export without a required Nyx backend, account, server compiler or cloud
storage, avoiding a required always-on application hosting service. Existing
host builds and native/LCL support remain available in their supported editions.

Starting evidence: [Athena GitHub Actions](../athena/docs/platforms/github-actions.md),
[Pages extension](../athena/templates/github-actions.md#pages-extension),
[browser checks](../athena/docs/platforms/browser.md) and existing
[delivery ownership](NS-6_delivery_01.md). Athena examples are guidance, not
installed workflows or successful Nyx deployment evidence.

North-star owner: NS-5. Milestone outcome: optional client-only WEB authoring under M4, consumed by M5 delivery.
Completion credit: pending evidence-backed assessment. These are open tasks;
planning earns no implementation credit and changes no existing acceptance,
counter or DONE status. Existing host/native and language-engine owners retain
their full requirements.

**Acceptance Criteria:**

- Stage a complete versioned static artifact with the editor, worker, WASM
  compiler, matched RTL/Nyx units, resources, licenses and manifests. A clean
  build reproduces the required closure without private paths or developer
  caches. Document the supported browsers and feature/capacity limits.
- Run the exact staged bytes on a Pages-equivalent plain static host at both
  root and a repository subpath. Verify relative worker/WASM/RTL/resource URLs,
  HTTPS/secure-context behavior, MIME handling and hard reload/deep entry paths.
  Do not require custom response headers, rewrite rules or application routes.
- Demonstrate real browser Studio journeys for a generated and edited
  main/unit/include project, callback/resource use, success/failure diagnostics,
  cancel/retry, stale refusal, isolated previews, local save/recovery and
  import/export. Capture actual desktop/narrow interaction and accessibility;
  a browser compile-only check does not satisfy runtime acceptance.
- Block Nyx backend, account, remote compiler and cloud-storage access during
  the client-only journey, verifying that compilation and project operations
  still work and no hidden host fallback or source upload occurs. Initial static
  asset downloads are recorded separately from compiler-service requests.
- Record cold online, warm online, fully cached offline, first-visit offline,
  interrupted update, storage denial and cache/project eviction behavior.
  Publish measured download/initialization/compile/storage budgets and honest
  limits; do not imply that every fresh browser can compile while offline.
- Retain regression evidence for existing host WEB view/application builds,
  native/LCL builds and native Studio settings/source/history. Keep the stable
  native gate; optional browser toolchains and canaries cannot replace it.
- Use Athena's Actions conventions when implementation is authorized:
  declared timeouts, read-only validation permissions, reviewed action SHA pins,
  required submodule checkout, versioned cache keys and retained failure logs.
  Save verified toolchain caches after capability probes and before downstream
  product checks. Validate the same fresh stage that publication would consume;
  pull requests validate without publishing.
- Keep build/upload and deployment stages distinct. Any later Pages deployment
  selects GitHub Actions as the Pages source and uses the adopted publishing
  branch, the github-pages environment, deployment permissions pages: write
  and id-token: write, a successful build/upload
  dependency and a policy that does not cancel an active deployment.
- Record Pages-equivalent local/CI qualification separately from a later,
  separately authorized live Pages deployment and smoke check of the exact
  published artifact. This task document authorizes no workflow/account change,
  deployment, server restart or production update. No deployment claim follows
  from staging; live rollout remains with the existing delivery owner.

**Blockers**

- [Browser compiler slice 01](NS-5_browser-compiler_01.md).
- [Browser compiler slice 02](NS-5_browser-compiler_02.md).
- [Browser compiler slice 03](NS-5_browser-compiler_03.md).
- [Browser compiler slice 04](NS-5_browser-compiler_04.md).
- [Browser compiler slice 05](NS-5_browser-compiler_05.md).
- [Browser compiler slice 06](NS-5_browser-compiler_06.md).
