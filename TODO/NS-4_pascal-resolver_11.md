# NS-4_pascal-resolver_11 - Qualify real native and browser compiler contexts

[North stars](../MILESTONES.md) · [Task catalog](README.md) · [Task flow](../TASKFLOW.MD)

**Description:**

Deferred on 2026-10-21. Supply actual supported target/compiler context to the
owned resolver without machine paths becoming portable document data. This is
the resolver-specific prerequisite of [Studio authoring](NS-4_studio-authoring_01.md)
and consumes existing compiler-provider work; it does not own or duplicate the
[browser-local compiler](NS-5_browser-compiler_01.md). North-star owner: NS-4;
completion credit stays with the original outcomes.

Starting owners: [context](../src/nyx.pascal.context.pas),
[compiler capture](../src/nyx.pascal.compilertrace.pas),
[file catalogs](../src/nyx.pascal.files.pas) and
[source configuration](../studio/nyx.studio.sourceconfiguration.pas).

**Acceptance Criteria:**

- Record exact supported FPC/pas2js versions, Delphi-dialect settings, defines,
  implicit modules, source search order and actual target facts.
- Distinguish native analysis of browser-target source from executing analysis
  in the browser; host Win32 measurements cannot become browser defaults.
- Support the intended explicit source catalogs/context provider with no silent
  fallback to unrelated installed SDKs or an optional GPL helper.
- Qualify include/conditional/module behavior on executed native and browser
  consumers; report unavailable target facts and unsupported forms explicitly.
- Preserve context isolation across target changes, cached projects and sessions;
  changed compiler identity invalidates affected answers without accepting source.

**Blockers**

- None beyond the existing capture/provider seams.
- External: exact toolchain/source inputs and authorized execution environment
  for each target; task authoring does not authorize host/configuration retries.
