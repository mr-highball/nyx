# NS-3_control-usefulness_03 — Make tables comfortable data workbenches

[North star NS-3](../MILESTONES.md#assessment) · [Task catalog](README.md) ·
[Usefulness review](NS-3_control-usefulness_01.md) · [Task flow](../TASKFLOW.MD)

**Description:**

Let application users arrange columns, read difficult values and act on cells
without leaving their working context. Prioritize real debugging/inspection and
data-entry workflows over adding isolated properties with no visible interaction.

North star: NS-3. Milestone: M3/components01 advanced table behavior and reuse.
Completion credit: pending assessment inside original component scope; existing
column sizing/navigation/selection implementation earns no duplicate credit.
Starting evidence: [typed column sizing and cell navigation](../docs/collection-views.md),
[native grid](../src/nyx.collections.lcl.grid.pas) and
[shared cell movement](../src/nyx.collections.grid.pas). Native mount-local width
override retention exists for externally changed `ColWidths`; the standard Nyx
table does not currently expose a resize handle/command. Browser drag handles,
portable column management and the remaining workbench affordances require
explicit review and qualification.
Inherited native grid copy behavior already exists; the gap is a documented,
portable Nyx copy/range contract with the same stable identities on both targets.
Coordinate the header surface with [filter/sort work](NS-3_control-usefulness_02.md);
that packet owns query semantics, while this packet owns presentation/cell actions.
Existing programmatic queries suffice for this packet's coexistence checks;
accepting the new filter UI is not a hidden prerequisite for column management.

Ordered pull packets preserve every criterion below. Each needs its own reviewed
source scope and actual result; completing W1 does not accept W2 or this task.

| Packet | Useful finish line | Remains open afterward |
| --- | --- | --- |
| W1 | Real column resize with keyboard alternative, choose/reorder/show/hide/pin and default reset; stable per-instance arrangement | Cell actions and full task acceptance |
| W2 | Full-value inspection, format/select/copy/export/context actions and integrated preference/lifecycle behavior on W1 | Any unmet original criterion and broader parity/budgets |

**Acceptance Criteria:**

- Provide typed authoring policies and real user controls for resizing, bounded
  content autosize, reordering, showing/hiding and pinning/unpinning columns, with
  a visible restore-defaults action. Use stable field identities rather than
  physical column indexes. Define the no-visible-columns case, minimum widths
  and narrow overflow behavior; keys and non-draggable alternatives remain usable.
- Expose appropriate typed formatting/alignment, wrapping/truncation, row density
  and full-value inspection. Preserve raw typed values for comparison/editing/
  copying. A long service name, command line or error message remains readable
  without shrinking the entire UI. Formatting must not execute saved text as code.
- Users can copy a cell, selected rows and an explicitly enabled cell range, with
  a documented delimiter/header/escaping policy. Typed opt-in export of the
  current filtered result is available through existing file-save abstractions.
  Distinguish displayed versus raw values, hidden columns and full versus paged
  results; cancellation or unavailable clipboard/save capabilities are visible.
- Complete keyboard and context-action workflows: navigate, select, enter edit,
  commit, cancel, inspect and copy. Per-column editors respect scalar domains,
  read-only/disabled states and validation failures while retaining drafts/focus.
  Authors can add row/cell actions through ordinary Nyx menus/components. Events
  carry stable row/field identities and typed values, preserving existing
  cancellable stages, multiple registrations and scheduler rules.
- Authored column defaults and optional user preferences have separate ownership,
  reset and persistence policies. Rebind, schema change, refresh, filters and
  reusable replacement cannot apply an old coordinate to the wrong field/row.
  Independent table instances retain their own arrangement, selection and drafts;
  preference restoration never mutates source data or silently rewrites a design.
- Public specialized contracts, Studio properties/events/help, saved policy,
  local inheritance/masks, paired history, generated Pascal/source reading and
  bounded semantic operations expose the same capabilities. Extend existing
  optional capabilities without breaking older interface implementations or
  requiring callers to manipulate LCL/DOM internals.
- Run a useful public Nyx workbench on actual native and browser targets at wide
  and compact sizes. Exercise real resize/reorder/filter coexistence, long text,
  hidden/pinned columns, editing/copy/actions, session reset and retirement.
  Build its own saved/generated application, retain failures and clean native
  heaps, and state widgetset/clipboard/assistive-input limits honestly.

**Blockers**

- [Control usefulness review](NS-3_control-usefulness_01.md) supplies the reviewed
  gaps and target-specific interaction contract.
- [Observable collections](DONE/NS-1_state-collections_01.md) supply stable typed
  data/selection/edit publication. Existing query/navigation/sizing owners remain
  authoritative rather than being replaced by a second workbench data model.
