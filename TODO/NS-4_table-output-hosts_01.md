# NS-4_table-output-hosts_01 — Runtime hosts for Studio-built table actions

[Milestones](../MILESTONES.md) · [Tasks](README.md) · [Current work](../WORK.md) ·
[Table workbench](NS-3_control-usefulness_03.md)

**Description:**

The same saved Service monitor builds and runs its column, filter, editing and
inspection controls through Studio. Its ordinary Copy action currently reports
that no clipboard service was supplied. The public handwritten example injects
platform clipboard/save hosts through `TableWorkbenchServices`; generated entry
points omit that setup. Existing FileSaving output selection configures Logic
file operations, and must also compose with table export where selected.

Provide this missing generated-runtime integration through the existing typed
host contracts. Preserve the separation between portable saved table intent and
machine-local runtime authority. Reuse Studio's existing output-host policy and
optional configuration pattern; source files or imported metadata must not grant
platform authority by themselves. Keep all output choices available independently
of installed compilers and preserve callers' ability to inject alternative hosts.
This is an NS-4 prerequisite of the original workbench03 outcomes, with no separate
completion credit or waiver of its broader interaction/quality requirements.

**Acceptance Criteria:**

- A typed fluent local output choice and ordinary Outputs control select
  clipboard writing. Preserve old profile meanings/bytes when absent, unrelated
  fields, per-setting loading and newer edits. Invalid input leaves the accepted
  configuration intact. Export reuses the existing FileSaving choice; neither
  option grants the other operation or enters portable project/design files.
- Browser/LCL generated entry points configure the existing table workbench
  services before mounting. Reuse the selected save host and its activation/
  lifetime behavior; file Logic and tables compose. Disabled or unavailable hosts
  report honest outcomes without silent fallback, lost text or broader authority.
- Keep reference ownership, UI scheduling, captured rows/values, cancellation and
  retirement correct. Reuse the library's exact-text and byte-save contracts;
  policy values retain strong types in generated code. Runtime host setup does
  not alter authored defaults, unfinished source, paired history or user projects.
- Return the same MCP-authored/saved Service monitor through ordinary Outputs,
  profile persistence, generated native/browser builds and actual Copy/export
  input at desktop/compact allocations. Verify exact accepted clipboard text and
  scoped TSV bytes through real configured hosts where supported. Record actual
  platform refusal/activation behavior separately from successful writes, download
  requests and confirmed disk outcomes. Manual-only host injection is insufficient.
- Retain meaningful regression assertions, failures and representative images.
  Keep compiler success, semantic admission, runtime input, physical/assistive
  qualification and untested platforms distinct. Use external scratch storage
  and preserve fixed serving slots, unrelated projects and output settings.

**Blockers**

- None for implementation. Existing host adapters, workbench services, output
  configuration and application entry-point generation are present.
