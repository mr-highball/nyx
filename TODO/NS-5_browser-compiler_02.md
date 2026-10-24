# NS-5_browser-compiler_02 - Select compiler providers in Studio

[North stars](../MILESTONES.md) · [Task catalog](README.md) · [Task flow](../TASKFLOW.MD)

**Description:**

Expose the qualified browser compiler through the existing compiler/provider
boundary and ordinary Nyx-built Studio output settings. Choosing an execution
backend is distinct from choosing WEB or native/LCL output. Keep the current
host build path and native support available without making either a dependency
of the static client-only edition.

Starting evidence: [compiler/provider contracts](../studio/nyx.studio.compiler.pas),
[ordinary source/project ownership](../docs/pascal-projects.md) and existing
[Studio authoring](NS-4_studio-authoring_01.md). This task owns only the optional
backend setting and guarded integration, not full authoring acceptance.

North-star owner: NS-5. Milestone outcome: optional client-only WEB authoring under M4, consumed by M5 delivery.
Completion credit: pending evidence-backed assessment. These are open tasks;
planning earns no implementation credit and changes no existing acceptance,
counter or DONE status. Existing host/native and language-engine owners retain
their full requirements.

**Acceptance Criteria:**

- Use typed backend choices and immutable requests/results for browser-local
  WEB compilation and the existing host provider. Reuse target/scope, diagnostics,
  captured main/unit/include inputs and artifact contracts; keep DOM/LCL types
  behind adapters and owned implementation in Pascal.
- Provide ordinary Nyx controls to choose/configure the backend while Studio
  is running. Report readiness, supported outputs, toolchain identity and
  actionable failure at build time. Unconfigured outputs must not prevent
  Studio launch, design editing, inspection or project opening.
- Keep backend choice separate from target selection and host compiler profiles.
  Browser-local compilation offers WEB only; native/LCL stays with a capable
  host/native provider. Preserve existing host/native behavior and user settings.
- Carry the exact project, source revision, target/scope, toolchain generation
  and request identity through dispatch and guarded publication. A setting change,
  project switch, newer edit or retired consumer makes old results ineligible;
  no failed build replaces the accepted pair or unrelated drafts/history.
- Persist suitable local preferences without exporting machine paths,
  authentication or host authority in portable projects. Explicitly distinguish
  client-local project ownership from connected/shared projects and host jobs.
- A client-only build must open and compile with no required Nyx backend,
  account, server compiler or cloud storage. Do not silently fall back to a host,
  upload source or turn on network services when local compilation is unavailable.
- Qualify actual provider selection and change-during-build behavior in browser
  Studio and retained native Studio controls. Preserve host WEB and native/LCL
  builds; describe browser-local availability accurately in each adapter.

**Blockers**

- [Qualified browser toolchain](NS-5_browser-compiler_01.md).
