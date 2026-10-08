# NS-1_resources_01 — Portable resources and resource bindings

[North stars](../MILESTONES.md) · [Task catalog](README.md) · [Task flow](../TASKFLOW.MD)

**Description:**

Supply the portable resource prerequisite for the user's common Studio Resources
area. Images, JSON, UTF-8 text and arbitrary data files share metadata, import,
query and history tooling. Strong resource/path/locale references make direct
label, prompt and table bindings fluent, including localization. Documents own
immutable defaults; runtime applications own independent mutable state/stores.
Embedded and hosted sources share a portable abstraction, explicit same-kind
fallbacks and tunable caller cache policy. Server restrictions are respected by
default, with a deliberate override for Nyx-managed private resource storage.
This prerequisite does not replace full Studio authoring, complete media/catalog
or semantic-workflow requirements. North-star owner NS-1; credit pending accepted
evidence. Existing packed PNG/JPEG and typed scalar/collection engines are inputs.

**Acceptance Criteria:**

- Named immutable resources retain exact bytes, Unicode text, numeric JSON tokens
  and creator metadata. Independent clones/snapshots safely outlive a document;
  malformed, unsupported, oversized and conflicting inputs refuse atomically.
- Versioned persistence, crafted public Pascal and managed source replay retain
  resources, typed selectors and bindings through full/page/reusable builds,
  candidate admission, paired Undo/Redo and pending-draft/stale refusal.
- Labels, prompt text and tables bind through typed resource selectors. Real
  browser/LCL controls exercise source changes, runtime independence, reusable
  scopes, invalid types/paths, rows/identity and subscriber retirement. Store-only
  fixtures do not establish live control behavior.
- Locale selection/fallback is explicit and typed, preserving exact keys/text and
  English defaults while supporting other Unicode content. Actual consumers cover
  missing keys/locales and updates without corrupting accepted application data.
- Hosted HTTP(S) resources resolve through replaceable target adapters, with
  bounded transport, cancellation/retirement and atomic typed admission. Caller
  freshness/stale/bypass/server policies govern memory and native-temp/browser
  persistent caches. Actual both-target consumers cover unavailable/corrupt/quota
  storage, loading failures, stale results and explicit fallback/override;
  declaration or storage-only checks do not establish hosted loading.
- A common public Nyx-built Studio Resources area and portable import tooling are
  consumed by both ordinary controllers. Bounded semantic inspection/mutation and
  visible paired operations retain user work; rendered/trusted-input evidence is
  reported separately from document API and compiler success.

**Blockers**

- [NS-1_model_01](DONE/NS-1_model_01.md) supplies accepted explicit ownership.
- [NS-1_persistence-state_01](DONE/NS-1_persistence-state_01.md) supplies portable
  persistence/scalar binding.
- [NS-1_state-collections_01](DONE/NS-1_state-collections_01.md) supplies typed
  independent runtime collections and bindings.

Browser/phone/trusted chooser/observing rollout currently needs the documented
launch/deployment blocker resolved. Until then compile/native evidence remains
partial and this task stays open. See the [return path](../WORK.md#current-return-path-common-resources-and-direct-bindings--2026-10-07).

## Foundation and direct consumer boundary — 2026-10-08

Immutable resources, structural scalar selectors, explicit locale lookup,
document version 8 and crafted public source replay are implemented. Both
ordinary renderer adapters consume privately copied catalogs. Actual Win32
captions/prompts update atomically; typed JSON row recipes update ordinary tables
through independent runtime stores, preserve stable selection and retire mounts.
Hosted declarations retain URL, embedded fallback and caller cache policy through
wire/source. Memory/private native-temp storage and shared freshness policy are
qualified; browser Cache Storage is compiled without execution evidence.

Maintained `resources` checks pass 78 plus eight exact emitted-builder checks,
and workspace regression passes 251, leak-free. Both Studios/backend/worker and
browser counterparts compile with zero owned warnings. The unchanged authenticated
primary is inspected read-only; current resource operations are absent there.

This foundation closed no full criterion; its first no-closure boundary was 1.
Existing workflow/authoring/renderer/codegen/delivery counts remain 21/36/19/28/2.
The subsequent hosted packet below advances the prerequisite and returns work
to the common Nyx-built Resources/import/binding panel and bounded semantic tools.
Automatic application loading, complete HTTP validation,
saved row mappings, combined scalar/table commits, runtime navigation/scopes and
actual browser/phone/observing journeys remain open. See [usage](../docs/resources.md)
and the linked work packet; a fallback caption is not a successful hosted fetch.

## Explicit hosted loading and ordinary consumers — 2026-10-08

Criterion 5 now has an explicit portable resolver, immutable request options and
replaceable byte-only transports. Win32 uses async WinHTTP on Nyx's bounded
workers; browser uses abortable streaming fetch. Both enforce decoded byte budgets
and retire borrowed callbacks before cancellation completes. Every cache hit/store
applies caller policy; persistent failures fall back to reusable private memory.
Network failure can select explicitly eligible stale content or the authored
same-kind fallback, retaining diagnostics. Publication remains an explicit
candidate catalog/ordinary renderer operation, independent of saved defaults.

Maintained `resource-loading` passes 31 checked shared/actual Win32 assertions,
leak-free, with zero owned warnings. Fifteen shared checks cover resolver policy,
failure and receiver retirement; sixteen actual native checks cover HTTP/HTTPS,
existing caption/prompt updates, invalid-selector refusal, persistent restart,
quota-to-memory reuse, queued deadline and worker retirement. Browser transport,
control journey and repaired Cache Storage boundary compile without execution.
The read-only authenticated primary and protected services/pairs remain exact.

No full criterion closes; resource no-closure advances 1→2 only. Stop transport
fixtures. Common Studio Resources/import/binding tools and bounded semantic
operations are the next consumer boundary. Browser/CORS/cache/phone/observing,
in-flight native cancellation timing, negative TLS, redirects/compression and
other native systems remain unqualified. Full HTTP validation, cache-operation
deadlines, automatic application loading, row mappings and runtime lifecycle
retain their original acceptance requirements. See [usage](../docs/resources.md)
and the linked work packet; native qualification does not establish browser parity.
