# NS-4_studio-reference_02 - Register and discover decorated API help

[North stars](../MILESTONES.md) · [Task catalog](README.md) · [Task flow](../TASKFLOW.MD)

**Description:**

Populate one discoverable reference index automatically from decorated
class/control declarations and existing metadata registration. Adding an
extension should not require editing a central help list, a second schema or
handwritten browser/native reference pages.

Starting evidence: [TNyxCatalog registration](../src/nyx.catalog.pas),
[RegisterNyxSchema and immutable snapshots](../src/nyx.schema.pas),
[callback registration](../src/nyx.callbacks.pas) and
[current contextual help](../studio/nyx.studio.help.pas).

North-star owner: NS-4. Milestone outcome: reflective Studio help under M4, consumed by M5 adoption.
Completion credit: pending evidence-backed assessment. These are open tasks;
planning earns no implementation credit and changes no existing acceptance,
counter or DONE status. Existing host/native and language-engine owners retain
their full requirements.

**Acceptance Criteria:**

- Connect documentation discovery to the declared unit/package/creator
  registration lifecycle. Use verified reflection where available or generated
  ordinary Pascal registration from the same decorated source, preserving a
  single source of truth and no manual help-only registration inventory.
- Define the actual discovery boundary: only linked/loaded registered units
  can be discovered without an additional manifest. Account for initialization
  order and dead-code/linker elimination; do not promise enumeration of every
  class in arbitrary unavailable units.
- Use stable unit-qualified class/interface/member and overload identities,
  with aliases, inheritance, extension provenance and target availability.
  Recipe/creator kinds resolve to their declared public API without guessing a
  concrete implementation class from serialized design data.
- Publish immutable versioned snapshots with deterministic ordering and
  generation identity. Duplicate/conflicting identities, unresolved type links,
  unsupported metadata and partial registration fail before replacing a valid
  index; consumers never hold mutable registry internals.
- Make registration of an extension's decorated class, schema and callback
  discoverable in the intended reference consumer without executing the class or
  constructing a hidden control. Define lifetime/removal behavior for any
  supported dynamic extensions and retain existing registry authority boundaries.
- Create the same logical index on native and pas2js, with declared differences
  for platform-only APIs. Bundle generated static metadata locally when needed
  so discovery requires no Nyx backend, account or documentation service.
- Verify repeated initialization, duplicate identities, missing units/types,
  inheritance/overloads, malicious text/links and a new extension. One changed
  decorated source must update discovery and dependent help without a second
  documentation edit.

**Blockers**

- [Portable documentation decorators](NS-4_studio-reference_01.md).
