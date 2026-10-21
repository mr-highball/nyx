# NS-4_studio-reference_05 - Preserve variant-record documentation topology

[North stars](../MILESTONES.md) · [Task catalog](README.md) · [Task flow](../TASKFLOW.MD)

**Description:**

Deferred on 2026-10-21. Add faithful variant-record metadata to the existing
[documentation contract](NS-4_studio-reference_01.md), consumed by automatic
discovery. North-star owner: NS-4; no separate completion credit. Existing parser
selector/arm/label/field relationships are qualified; the source documentation
adapter still explicitly refuses variant-arm fields.

Starting owners: [variant syntax](../src/nyx.pascal.syntax.pas),
[documentation](../src/nyx.documentation.pas),
[source adapter](../src/nyx.documentation.source.pas), registration/registry
and existing source/declaration consumers. Actual retained TGUID has nine callable
members, fourteen fields and one unnamed selector with three labeled arms.

**Acceptance Criteria:**

- Preserve selector type use, optional tag identity, parent-arm relationships,
  ordered labels/arms and exact field membership without synthesizing a tag field.
- Retain authored packed/bitpacked intent independently of unknown offsets/ABI;
  labels remain exact source expressions unless separately evaluated with authority.
- Validate same-record ownership, acyclic/reachable topology and unique complete
  field membership before lifting only the corresponding source refusal.
- Copy/seal/frame metadata through existing work/byte/cancellation owners;
  absent versus known-empty attachments retain their distinct meaning and old
  absence encoding remains compatible.
- Qualify actual TGUID, nested/named/unnamed selectors, malicious custom metadata
  and atomic index refusal on native and executed browser consumers.

**Blockers**

- None beyond the existing typed syntax and descriptor owners; full source
  package adoption remains a separate task below.
