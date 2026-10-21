# NS-4_studio-reference_07 - Publish the complete source-derived package

[North stars](../MILESTONES.md) · [Task catalog](README.md) · [Task flow](../TASKFLOW.MD)

**Description:**

Deferred on 2026-10-21. Finish the actual source-to-package/index integration
under [descriptors](NS-4_studio-reference_01.md) and
[discovery](NS-4_studio-reference_02.md). North-star owner: NS-4; no duplicated
credit. Supplied-package mechanics, shared signature sessions and creator
selection are qualified, but the complete actual source package has not passed.

Starting owners: [source consumer](../tests/nyx_documentation_source_tests.lpr),
[source adapter](../src/nyx.documentation.source.pas),
[catalog](../src/nyx.catalog.pas), the actual controls/fixture declarations and
[retained checkpoint](../WORK.md#active-priority--back-to-core-nyx-2026-10-20).

**Acceptance Criteria:**

- Capture all 81 built-in creator declarations, all 89 actual controls-unit
  globals and complete selected fixture class/interface/callback/unit scopes.
- Follow genuinely referenced external/core types through complete actual
  namespaces; no fabricated leaves, exported-prefix stand-ins or omitted members.
- Fit the existing package/index aggregate 4,096-declaration/16-MiB/2M limits
  and the separate type-construction 4-MiB lexical/copy and 2M-work limits.
  Preserve existing token/node, 256-KiB per-declaration and 128-member bounds;
  the aggregate byte cap does not enlarge any construction allowance. Use one
  shared canonical package with exact chunk/package sharing and source origins.
- Reuse that exact package at actual schema, callback, factory and catalog
  registration owners; do not recapture independently or execute inspected code.
- Refresh the immutable index with complete metadata and unchanged construction/
  callback/example execution counters. Failed/missing/changed inputs preserve
  prior Current/version and independent registry authorities.
- Execute the source-to-index journey on native and browser targets; retain
  failed preparation receipts and distinguish compiler success from adoption.

**Blockers**

- [Actual Classes/import closure](NS-4_pascal-resolver_02.md).
- [Variant topology](NS-4_studio-reference_05.md) and
  [authored generic descriptors](NS-4_studio-reference_06.md).
