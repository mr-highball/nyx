# NS-5_browser-compiler_05 - Own client-local projects and recovery

[North stars](../MILESTONES.md) · [Task catalog](README.md) · [Task flow](../TASKFLOW.MD)

**Description:**

Make the static edition useful for independent authoring with durable local
project storage, bounded recovery and portable import/export. Reuse existing
accepted main/helper inputs, unfinished file buffers, project versions and paired
history; cloud storage and shared-server ownership remain optional separate modes.

Starting evidence: [project ownership and recovery](../docs/pascal-projects.md),
[Studio authoring](NS-4_studio-authoring_01.md) and existing public file-save
contracts in [nyx.files.save](../src/nyx.files.save.pas).

North-star owner: NS-5. Milestone outcome: optional client-only WEB authoring under M4, consumed by M5 delivery.
Completion credit: pending evidence-backed assessment. These are open tasks;
planning earns no implementation credit and changes no existing acceptance,
counter or DONE status. Existing host/native and language-engine owners retain
their full requirements.

**Acceptance Criteria:**

- Use a versioned browser-local store with transactional accepted project/source
  pairs, supporting units/includes/resources, exact unfinished buffers/first bases
  and the supported paired Undo/Redo/recovery state. Keep stored project data
  independent of evictable compiler caches.
- Open, create, rename, save, reopen and recover projects through ordinary Nyx
  controls with no account, Nyx backend or cloud storage. Report whether changes
  are durable, pending or refused; do not imply remote synchronization.
- Preserve exact Unicode and mixed source line endings through reload and
  recovery. A crash, aborted write, migration failure or incompatible newer format
  cannot partially replace the last accepted project or silently discard drafts.
- Handle unavailable storage, quota exhaustion, private-mode restrictions,
  user-cleared data and eviction with useful diagnostics and an export/recovery
  route where data remains available. Document local persistence and browser
  origin/subpath boundaries without promising permanent storage.
- Detect concurrent-tab or stale-revision writes and offer a bounded conflict
  path rather than silently overwriting a newer project. Recovery must not
  resurrect a retired compiler request or trust an obsolete artifact.
- Import/export complete portable projects with their declared inputs and
  resources. Validate versions, path traversal, duplicate/case-colliding names,
  size limits and malicious content; do not export credentials, host paths,
  file handles or compiler authority.
- Exercise crash/reload, interrupted migration, quota refusal, stale tabs,
  import/export round trips and offline reopen in actual browsers. Recompile a
  recovered main/unit/include/resource project locally, preserving accepted state,
  independent drafts and paired history on failure.

**Blockers**

- [Provider and project-mode selection](NS-5_browser-compiler_02.md).
- [Virtual inputs and cached toolchain](NS-5_browser-compiler_04.md).
