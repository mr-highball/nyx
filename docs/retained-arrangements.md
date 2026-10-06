# Retained view arrangements

[Responsive authoring](responsive.md) · [Architecture](architecture.md) ·
[Evidence and remaining work](../WORK.md#retained-bound-state--2026-10-06)

Both adapters can now retain an unchanged set of realized controls when a fresh
design moves them between ordinary layout hosts or changes their sibling order.
`TryRefresh` performs fresh document/property/context admission, then chooses the
strict scalar path or the separately checked structural path. A successful move
keeps the same realized nodes, physical controls, event bindings and emitter scope.
Unchanged authored values retain independent runtime drafts. Actual authored deltas
and explicit field restorations still follow runtime identity.

Scalar state bindings can also retain that lifetime. The owning
`TNyxLiveBindings.TryPrepareRefresh` validates independent candidate and baseline
copies against the same current runtime store. Ordered binding descriptors,
identities, scopes and the complete realized control set must stay exact.
Both copies use current state, so an old document default cannot replace an
accepted runtime value during layout publication. The existing coordinator,
subscription token and store stay attached; refresh publishes no state revision
or notification. External state writes continue through the original validator.

The public unbound guards retain their strict meaning. Explicit
`CanRefreshNyxBoundProjection` / `CanArrangeNyxBoundProjection` are adapter
boundaries for coordinator-prepared roots, not substitutes for state admission.
Commands and store validation/notification refuse refresh reentry. Read-only
`RefreshReady`, `HasBindings` and `TNyxState.Busy` observations require the UI
thread; they are neither cached admission nor cross-thread locks. Unbound views
keep their existing property path without additional state-projection clones.

An unfinished numeric draft remains independent of the accepted bound number.
Unchanged accepted-value markers prevent ordinary Sync from rewriting it during
a move or an unrelated state update. A typed `TNyxProjectionValueRestore` resets
only its exact field: unbound fields use the authored default; bound fields use
the current accepted store value. The complete group is checked before mutation,
and only requested faces reset their accepted-value marker. Other field drafts
remain untouched. Browser HTML number inputs may sanitize unfinished text to an
empty physical draft; native edits retain literal trailing decimal text.

The portable `TNyxNode.ArrangeLike` boundary prepares every owner array and actual
managed implementation anchor before publication. It adopts no candidate node.
Runtime/source/design identities, instance scopes, complete node set and root stay
exact. Duplicate identities, foreign roots and count/depth budgets refuse before
changing ownership. Prepared arrays replace owned children; borrowed parent links
follow that same independent shape. Managed implementations survive owner changes
and release exactly once at teardown.

`CanArrangeNyxProjection` aligns independent comparison copies and applies the
original scalar compatibility checks. Changed contracts/extensions, constructor
properties, binding contracts, collection semantics, creators/factories or document context
retain their full-mount requirement. Reparent/order changes are admitted only at
ordinary page, row, column, grid, panel, card, group-box, form, toolbar and sidebar
hosts. Special pane hosts remain independent. Unchanged hosts are not reattached,
preserving private native/DOM panes, captions and glyphs.

Physical publication walks the new parent arrangement before its descendants,
retains existing faces, suppresses incidental focus/editing callbacks, and restores
supported ranges and scroll positions. Native keyboard order follows new child
order. Win32 handles may be recreated by LCL while the control objects stay retained;
the selection writer validates platform capability and the requested scalar range
against current Text, independently of a transient current read. Each adapter keeps
an independent previous clone for property/arrangement rollback on target failure.

Reproduce the shared ownership and actual native control checks with
`tools/build.ps1 -Target retained-arrangement`. Supply
`-ArrangementSourceDirectory` with exact bounded MCP-exported `nyx.generated.view.pas`
to exercise an agent-authored companion. The small English
`tests/arrangement-review.operations.json` composes that base in one semantic group.
For bound-state qualification append
`tests/bound-arrangement-review.operations.json` to that same semantic group. The
maintained persistent Pascal `nyx_mcp_designer_review` client owns one empty review
through composition, bounded export, both compiler requests and retirement.
One-shot `nyx_studio_mcp call` invocations open separate transports; a temporary
review from one call cannot be used by another. The control fixture then attaches
state and bindings through explicit typed Pascal calls. Those attachments qualify
library consumers; general semantic state/binding authoring remains with the
existing MCP workflow owner and is not claimed by this companion.
Browser artifacts stage under `build/retained-arrangement/maintained/web`; observe
their Pascal-owned markers through the maintained browser driver, using `projection`
for retained controls and the generic marker mode for the ownership fixture.
The `?compact` control fixture uses a 390-pixel host, not a physical phone.
The same `projection` marker observer runs `bound-arrangement.html`; `bindings`
observes the original browser state/control regression suite.

This is the ownership foundation for alternate presentations. Lazy alternate
recipes, fluent structural rules, ordinary Studio
presentation switching, IME continuity, widgetset/DPI/accessibility and release
performance remain with the original task owners. Rearranging the design through
MCP still uses an expected revision and one paired Undo group; the runtime ownership
primitive does not replace editor transactions or persist an authored document.
