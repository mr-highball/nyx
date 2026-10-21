# NS-4_interact-flows_03 — Compose root publication with an accepted view

[North stars](../../MILESTONES.md) · [Task catalog](../README.md) · [Task flow](../../TASKFLOW.MD)

**Description:**

Let a native or browser renderer accept an optional managed root participant
alongside its existing reversible view preparation. A host needs to adopt the
runtime borrowed by new controls at the actual commit boundary, even if a later
notification raises. Returning successfully from RenderContext is too late to
serve as that ownership boundary.

North star: NS-4, serving [Interact flows](../NS-4_interact-flows_01.md) and original
Studio authoring. Credit remains with that original outcome; this split earns
none. Starting owners are [the LCL renderer](../../src/nyx.render.lcl.pas),
[the browser renderer](../../src/nyx.render.browser.pas) and existing prepared
publication/readiness contracts. The managed
[publication source](../../src/nyx.view.publication.pas) is the accepted API.

Accepted on 2026-10-21: the maintained ordinary application executes136 checks
at1280 and390, including exact dormant reservations, precommit refusal, reentrant
validation, committed Notify/Retire failure, retained-source structural remount,
design retirement and actual renderer disposal inside Notify. Both native runs
have clean heaps; the matching browser assertions compile. The public
`view-publication` build target executes both widths and stages its browser
application without starting a listener. Original content42, route controls26,
Kanban98 and the unchanged previously accepted MasterDetail compiled consumer30/30
retain compatibility. Both Studio targets compile. Sixteen accepted compilers
have zero owned/49 upstream RTL warning occurrences; ten native runs have clean
heaps. All readers joined and audited33 owned/1776 Pascal/13 concurrent/eleven
extra source/driver/generated pins before this task move.

Only two fixture browser references changed after the production freeze:
`Web.document` avoids a field collision. Retain that first compiler failure,
the missing generated content input and the mistakenly paired newer MasterDetail
consumer/older project. The latter fails twice with clean heaps and retains two
fixture warnings; it is not accepted compatibility evidence. The correct older
consumer is the exact retained delivery source with its own saved pair, compiled
against the current production source. No ordinary completion-aware assertion,
wait or source-default contract changed to qualify this task. Native autosizing
failure handling is inspected, not injected. Browser execution/input/parity,
Studio root-flow adoption and the newer saved/generated-flow outcome remain open.

[WORK.md](../../WORK.md#composable-view-publication--2026-10-21) owns commands,
source hashes, terminal receipts, failures and return path. This accepts only
the independently useful renderer API; original criteria/credit and no-closure10
remain unchanged. The optional liveness capability reuses the existing content
guard, preserving its original GUID and callback-depth semantics.

**Acceptance Criteria:**

- Both renderers support a typed optional participant prepared against the exact
  document, realized root and same prospective event view used by the controls.
  Existing callers without the participant retain their behavior. Route-only
  pages and runtime-to-design transitions still obtain the required successor.
- Hidden Build/Sync and every outward validator finish before all final local
  readiness fences. Any precommit failure leaves old controls, stores, routes
  and context usable. Never strip routes or substitute an empty document to
  manufacture admission.
- Installation adopts preallocated host/runtime ownership through the existing
  pure publication boundary. Old generations revoke before their controls
  retire. A postcommit notification failure cannot cause the caller to free
  stores borrowed by accepted controls or attempt a false rollback.
- Accepted runtime views retain the managed preparation source across implicit
  Sync/remount. Explicit replacement and a one-shot design retirement release
  it correctly. Weak host receivers revoke on disposal; no retained source keeps
  widgets, documents or retired controllers alive accidentally.
- Every displaced notification/disconnect/retirement gets its required cleanup
  attempt even when an earlier one raises. Focused ordinary renderer consumers
  execute abandonment, validator reentrancy, remount, design transition and
  committed cleanup failure with native clean heaps. Matching browser consumers
  compile; real browser input/parity remains a separate gate.

The seam is independently useful with an ordinary managed participant. Later
root-flow integration additionally consumes
[prepared routes](NS-4_interact-flows_02.md).

**Blockers**

- None. Existing renderer preparation and publication contracts are the starting
  owners; no new routing, state or event engine is required.
- No host/configuration retry, listener launch or new MCP/wire schema follows
  from this renderer API task.
