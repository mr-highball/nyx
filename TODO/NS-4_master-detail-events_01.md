# NS-4_master-detail-events_01 — Discover and wire selected-record Edit data

[North stars](../MILESTONES.md) · [Task catalog](README.md) · [Task flow](../TASKFLOW.MD)

**Description:**

Make the configured MasterDetail Edit record discoverable and usable through
ordinary semantic event, flow and Tool Canvas authoring. North-star owner: NS-4,
serving [component reuse](NS-3_components_01.md) and
[Studio authoring](NS-4_studio-authoring_01.md). Completion credit stays with
those original outcomes; this newly recorded gap earns no additional credit.

Starting evidence at `72d0244`: the native Studio/application journey passes106/114,
including actual queued callbacks receiving a copied record through Details.
However, `NyxEventsMetadata.AddRoute` still describes the exact semantic Edit
route as `NyxSignalPayload`. Existing schema therefore refuses event-Details
inputs, the flow editor reports no Details and Tool Canvas exposes no Details
port. Native callback success does not establish semantic/canvas wiring.

Use the existing metadata/payload and route owners. Prove the exact configured
specialized MasterDetail and its `detail/edit` role; do not infer from an open
event name such as `edit`. Preserve aggregate generic clicks, unrelated names,
same-name route distinctions, reusable ownership and independent instances.
No new MCP tool, wire schema, runtime event format or producer authority is needed.

**Acceptance Criteria:**

- Configured roots and selected named Edit descriptors expose their exact
  semantic route with existing object-data payload metadata and correct copied
  origin/source identities. A renamed exact Edit role retains that contract.
- Static, Disabled, unrelated same-name and aggregate generic-click producers
  retain truthful signal/undefined contracts. Mixed same-name routes preserve
  per-route payloads and refuse a fabricated uniform aggregate contract.
- Existing control-flow admission, public flow editor and Tool Canvas discovery
  expose/admit the structured Details port from that exact route, with no new
  independent selection or event engine. Stale/foreign/incompatible routes refuse.
- A useful authored route consumes the selected-record data through ordinary
  source/history/save/reopen and generated application behavior. Native/browser
  evidence and its actual limits are explicit; compiler-only browser evidence
  does not establish browser input or canvas interaction.
- Existing scalar events, custom structured producers, reusable descriptors and
  multiple registrations retain their original behavior. Focused checks cover
  both positive and refusal cases without weakening broader event/reuse criteria.
- Explicit scalar Value and the copied structured Details may coexist on the
  same Edit event. Both remain discoverable/admissible through their own typed
  inputs; relabeling scalar metadata as data, or hiding Details whenever a scalar
  domain exists, does not meet this task's outcome.

**Blockers**

- [Interact root-flow publication](NS-4_interact-flows_01.md) is a newly verified
  product prerequisite. Completion-aware native1280/390 runs both receive one
  copied callback but never publish the flow result; both have clean heaps.
  Studio's Interact host does not bind root declarative routes. Preserve those
  failures and the unchanged ordinary consumer; stop wait/fixture variants.
- Current local evidence: shared independent discovery, Events Inspector and
  scalar/Details admission pass687 native checks; public route form85 and Tool
  port289 pass with clean heaps. Earlier premature-read failures retain19
  unretired blocks/1581 bytes each. The later completion-aware runs have clean
  heaps and still fail the actual publication predicate after60 seconds: this
  proves the missing root-flow host, not merely a scheduling delay. Preserve
  both distinct results. Exact saved/generated application qualification remains
  required; this task stays open.

- [Observable structured state](DONE/NS-1_state-collections_01.md) supplies the
  accepted typed collection/item foundation.
- External observing/browser execution needs an already admitted host/session;
  no host/configuration retry or new rollout is authorized by this task file.
