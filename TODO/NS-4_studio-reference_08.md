# NS-4_studio-reference_08 - Emit deterministic static documentation packages

[North stars](../MILESTONES.md) · [Task catalog](README.md) · [Task flow](../TASKFLOW.MD)

**Description:**

Deferred on 2026-10-21. Emit the real captured package as ordinary Pascal so API
browsing needs no source resolver, compiler or backend, under
[automatic discovery](NS-4_studio-reference_02.md). North-star owner: NS-4;
completion credit stays with the original reference outcome.

Starting owners: [contract generator](../tools/nyx_control_contracts.lpr),
[generated companion](../src/nyx.controls.documentation.pas) and its generator
consumer. The current companion maps a supplied package and selects maintenance
creators; it does not emit static captured descriptors. Existing public typed
builders cover current canonical metadata; no new reconstruction schema is implied.

**Acceptance Criteria:**

- Generate declarations, type uses/formal bindings, contexts, chunks and packages
  from the same admitted canonical input and generator IR as creator intent.
- Produce deterministic escaped Pascal and preserve exact content, limits,
  platforms, provenance, context/bootstrap values and shared package/chunk structure.
- Reconstruct through public typed builders; never reverse-parse private framing
  or persist process-local Identity numbers as semantic identities.
- Compare full declaration/package content and sharing against the original
  capture, including optional metadata, Unicode and repeated deterministic emission.
- Split runtime static imports from source/binding maintenance selectors. Browse
  linked emitted metadata without source files, filesystem, compiler or Nyx backend.
- Compile and execute the emitted companion on native/browser consumers without
  constructing arbitrary controls/callbacks or weakening source admission.

**Blockers**

- [Complete source package](NS-4_studio-reference_07.md).
