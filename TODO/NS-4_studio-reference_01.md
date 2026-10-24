# NS-4_studio-reference_01 - Define portable documentation decorators

[North stars](../MILESTONES.md) · [Task catalog](README.md) · [Task flow](../TASKFLOW.MD)

**Description:**

Establish typed documentation decorators attached to Nyx class/control
definitions so help can reflect the same declarations and metadata that authors
use. Define decorators through ordinary Pascal APIs and adapter contracts;
compiler-specific syntax and reflection capabilities must be verified before
adoption. No universal runtime reflection capability is assumed.

Starting evidence: [existing interface decorator](../tests/nyx.test.controls.pas),
[managed host decorator](../src/nyx.files.save.activation.pas),
[catalog descriptions](../src/nyx.catalog.pas),
[property/event schemas](../src/nyx.schema.pas) and the target-specific TypeInfo
bridge in [nyx.linq.runtime](../src/nyx.linq.runtime.pas). These are reusable
patterns, not an existing reflective documentation registry.

North-star owner: NS-4. Milestone outcome: reflective Studio help under M4, consumed by M5 adoption.
Completion credit: pending evidence-backed assessment. These are open tasks;
planning earns no implementation credit and changes no existing acceptance,
counter or DONE status. Existing host/native and language-engine owners retain
their full requirements.

**Acceptance Criteria:**

- Define an ordinary typed Pascal documentation decorator/descriptor API for
  classes, public control interfaces, creators and their members. Associate
  summaries/examples/related references with stable qualified identities while
  retaining the original class/control contract and ownership.
- Reuse immutable catalog discovery, property/event schemas, callback metadata
  and type identities rather than writing a separate help-only copy. Make
  descriptions, signatures, defaults, ranges, capability limits and links come
  from one declared source with explicit ownership and precedence.
- Cover constructors/factories, fluent methods, inherited members, properties,
  callbacks and payload/parameter/result types, enums/records/interfaces and
  cross-references. Distinguish declared metadata, documentation prose and
  actual target capability; absent information remains explicit.
- Document a capability matrix for the supported native FPC and pas2js
  versions, including available RTTI, visibility, generic/interface identity and
  compiler/linker metadata retention. Verify a minimal decorated class/control
  and callback on each actual target before adopting a reflection mechanism.
- Keep any native RTTI or pas2js-specific inspection behind an adapter. Where
  declarations cannot be reflected, derive deterministic descriptors/registration
  from the same decorated source using owned Pascal tooling if required.
  Do not invent language support or silently replace decorators with a parallel
  hand-maintained reference file.
- Preserve the existing public interfaces and supported native baseline.
  Decorators must not instantiate arbitrary controls/callbacks, execute user
  examples or retain documents/renderers merely to inspect documentation.
- Deliver a usable metadata contract and small public examples with native and
  executed browser evidence, exact compiler identities and explicit remaining
  limitations. The owned editing resolver remains independent and is not a
  prerequisite for browsing registered API help.

**Blockers**

- None. Existing schemas, registries and decorator patterns above are the baseline.
