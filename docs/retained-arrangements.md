# Retained view arrangements

[Responsive authoring](responsive.md) · [Architecture](architecture.md) ·
[Evidence and remaining work](../WORK.md#retained-structural-arrangement--2026-10-06)

Both adapters can now retain an unchanged set of realized controls when a fresh
design moves them between ordinary layout hosts or changes their sibling order.
`TryRefresh` performs fresh document/property/context admission, then chooses the
strict scalar path or the separately checked structural path. A successful move
keeps the same realized nodes, physical controls, event bindings and emitter scope.
Unchanged authored values retain independent runtime drafts. Actual authored deltas
and explicit field restorations still follow runtime identity.

The portable `TNyxNode.ArrangeLike` boundary prepares every owner array and actual
managed implementation anchor before publication. It adopts no candidate node.
Runtime/source/design identities, instance scopes, complete node set and root stay
exact. Duplicate identities, foreign roots and count/depth budgets refuse before
changing ownership. Prepared arrays replace owned children; borrowed parent links
follow that same independent shape. Managed implementations survive owner changes
and release exactly once at teardown.

`CanArrangeNyxProjection` aligns independent comparison copies and applies the
original scalar compatibility checks. Changed contracts/extensions, constructor
properties, bindings, collection semantics, creators/factories or document context
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
Browser artifacts stage under `build/retained-arrangement/maintained/web`; observe
their Pascal-owned markers through the maintained browser driver, using `projection`
for retained controls and the generic marker mode for the ownership fixture.
The `?compact` control fixture uses a 390-pixel host, not a physical phone.

This is the ownership foundation for alternate presentations. Lazy alternate
recipes, fluent structural rules, bound-state continuity, ordinary Studio
presentation switching, IME continuity, widgetset/DPI/accessibility and release
performance remain with the original task owners. Rearranging the design through
MCP still uses an expected revision and one paired Undo group; the runtime ownership
primitive does not replace editor transactions or persist an authored document.
