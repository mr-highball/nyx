# NS-4_studio-reference_04 - Verify reference completeness and compatibility

[North stars](../MILESTONES.md) · [Task catalog](README.md) · [Task flow](../TASKFLOW.MD)

**Description:**

Keep the reflective help feature complete and useful as the public Nyx API
evolves. Validate metadata against the actual registered API, cross-reference
integrity and ordinary Studio use on native, browser and the static client-only
edition, without claiming unsupported language reflection.

Starting evidence: [capability metadata](../docs/capabilities.md),
[existing reference](../docs/components-reference.md),
[ordinary help checks](../tests/nyx_studio_help_controls.lpr) and
[Athena validation](../athena/docs/validation.md). Existing generated/reference
material should be reconciled with the decorator source rather than maintained
as a competing authoritative API inventory.

North-star owner: NS-4. Milestone outcome: reflective Studio help under M4, consumed by M5 adoption.
Completion credit: pending evidence-backed assessment. These are open tasks;
planning earns no implementation credit and changes no existing acceptance,
counter or DONE status. Existing host/native and language-engine owners retain
their full requirements.

**Acceptance Criteria:**

- Define the intended public API inventory and explicit justified exclusions,
  derived from registered classes/control interfaces, schemas, callbacks and
  types. Verify coverage against that independent API inventory so a shared
  omission cannot make both the index and its test appear complete.
- Check missing descriptions, duplicate identities, stale/removed members,
  inherited/overloaded signatures, parameter/result/payload type links, broken
  related references and invalid examples. Every discovered public item has a
  reference entry or an explicit diagnostic/exclusion; do not hide gaps.
- Add a small decorated extension fixture and change/remove its declarations.
  Prove automatic registration, search and contextual lookup follow the one
  source across native/pas2js without hand-editing generated pages or help lists.
- Execute meaningful Pascal examples against supported toolchains, with
  native/browser-specific examples labelled accurately. No documentation lookup
  executes sample code; verification runs in the appropriate bounded test context.
- Exercise real Studio global lookup, contextual member help, keyboard/focus,
  navigation, stale-registry refusal and project/history preservation on both
  adapters. Distinguish compilation from actual interaction, accessibility,
  viewport and memory/lifetime evidence.
- Use the staged static edition at root and repository subpaths with backend
  access disabled. Verify bundled help/link/assets, warm cached offline lookup,
  cold-offline limits and compatibility with compiler/toolchain cache updates,
  without requiring an account or cloud documentation service.
- Integrate bounded registration/completeness/link checks into the existing
  Pascal validation path when implementation is authorized. Retain native
  baseline and reviewed Athena CI/cache/artifact conventions; unsupported RTTI
  or decorator syntax fails visibly rather than yielding an empty passing index.
- Reconcile any exported/static reference generation with the same decorated
  metadata, record supported compiler/browser versions and remaining gaps, and
  provide a documented extension-author workflow. Full Studio, delivery and
  independent-user evaluation remain with their existing owners.

**Blockers**

- [Nyx-built reference UI](NS-4_studio-reference_03.md).
- [Qualified static client edition](NS-5_browser-compiler_07.md) for final static-mode acceptance; native/browser reference checks can proceed independently.
