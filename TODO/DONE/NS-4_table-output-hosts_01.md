# NS-4_table-output-hosts_01 — Runtime hosts for Studio-built table actions

[Milestones](../../MILESTONES.md) · [Tasks](../README.md) · [Current work](../../WORK.md) ·
[Table workbench](../NS-3_control-usefulness_03.md)

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

**Acceptance — 2026-10-29:**

All five criteria are qualified for this generated-host integration. Production
implementation 21fa6371 is delivered. Typed clipboard selection is independent
of saving/opening/HTTP; versions 1..4 retain their exact absent/revoked meanings.
Native/browser reconciliation passes 270 per target and strict profiles 69 per
target. Ordinary native Outputs passes 16 at each width; actual browser desktop/
compact Apply/Reload/revoke preserves independent choices and restores the exact
original effective profile. Eight saved-application compiler jobs exercise both
targets with neither/copy/save/both. Stale output fingerprints refuse submission.

Generated applications configure services before mounting, share one selected
activated save host with file Logic and retain typed export defaults. The
[native instrumenter](../../tests/nyx_service_output_instrument.lpr) proves its
two insertions reversible. Its [driver](../../tests/nyx.test.service.outputs.lcl.pas)
borrows that generated application and never replaces service factories, hosts,
mounting or teardown. Real controls and process-owned Save dialogs pass 63 checks
at each native width with two complete clean heaps. Fresh system clipboard
writes match both 85/42-unit accepted commands. Current-page export writes one
Gateway row (112 bytes); filtered export writes two Running rows (175 bytes). Both
match independent fixed TSV expectations; real cancellation creates no file.
Runtime query/paging are explicit public calls and do not rewrite the design.

The same MCP-built browser application copies exact raw commands at desktop/
compact widths through the real Windows clipboard. Ordinary desktop page export
writes all four rows (452 bytes). Compact Status filtering and all-filtered export
write exactly the two Running rows (175 bytes), also byte-identical to native.
The user operates the real browser Save As dialogs; ordinary pointer input,
terminal Written messages and binary disk comparisons qualify the result. No
virtual-clipboard injection, forced download fallback or manual service injection
substitutes for these effects. Browser downloads and other platforms are not
claimed from those picker writes.

All nine QA project pairs/identities/revisions and the original effective output
profile remain exact. Prior matched QA/LAN delivery preserves fixed slots and all
three LAN pairs plus 82 durable settings/project files. External packets
table-output-hosts-01/02 retain builds, failed setup/retired-presenter driver runs,
clean final runs, source identities, actual files and representative images.
Native widget input is synthetic; browser picker choice is user-operated. Full
physical/assistive, broad workbench and other-widgetset quality remain with the
parent task. This acceptance adds no separate completion credit. Return to that
workbench rather than expanding host/profile fixtures.

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
