# NS-5_browser-compiler_06 - Isolate compiled previews and trusted messages

[North stars](../MILESTONES.md) · [Task catalog](README.md) · [Task flow](../TASKFLOW.MD)

**Description:**

Run compiler-produced JavaScript and any handwritten project behavior in a
separate, disposable preview context. A program being designed must not gain the
editor's storage, document objects, compiler provider, session credentials or
publication authority.

Starting evidence: [compiler/accepted-source ownership](../docs/pascal-projects.md)
and [managed Nyx presentation](../docs/popover.md). Existing connected artifact
delivery and same-page demonstrations do not prove safe client-only execution.

North-star owner: NS-5. Milestone outcome: optional client-only WEB authoring under M4, consumed by M5 delivery.
Completion credit: pending evidence-backed assessment. These are open tasks;
planning earns no implementation credit and changes no existing acceptance,
counter or DONE status. Existing host/native and language-engine owners retain
their full requirements.

**Acceptance Criteria:**

- Declare and implement the preview trust boundary, permitted capabilities and
  the isolation mechanism that works on static Pages. The running program
  cannot access editor DOM, localStorage/IndexedDB, file handles, service-worker
  authority, authentication or accepted-project mutation APIs.
- Use sandbox/origin boundaries that remain effective with generated scripts.
  Do not combine same-origin script access with editor authority, assume a second
  configurable server, or depend on custom response headers unavailable on the
  static host. State the supported resource/network and navigation policy.
- Validate message source/window, expected origin including opaque-origin
  behavior, unpredictable per-preview channel identity, protocol version, payload
  shape and size, and exact project/revision/build generation. A wildcard target
  or a source-supplied identifier alone is not authorization.
- Expose only narrowly typed preview-ready, resize, error/log and supported
  interaction messages. Parent-side receipt never executes source text, trusts
  HTML as editor markup or turns preview data into a semantic edit/build request.
- Manage each artifact's RTL/resources/object URLs and callbacks as one owned
  generation. Reload, stop, failed startup, worker cancellation and project
  switches revoke previous channels and release retained resources.
- Preserve the last good admitted preview on compile failure and prevent stale
  success from replacing it. Bound log floods and startup failure; document
  actual recovery limits for a non-cooperative running preview.
- Exercise adversarial generated code attempting parent access, storage reads,
  forged messages, stale-frame replies and navigation/popups. Demonstrate useful
  real Nyx application/view previews, callback behavior and resource loading at
  root and repository subpaths without exposing editor authority.

**Blockers**

- [Guarded worker results](NS-5_browser-compiler_03.md).
- [Versioned virtual inputs/assets](NS-5_browser-compiler_04.md).
