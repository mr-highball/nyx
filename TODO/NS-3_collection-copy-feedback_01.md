# NS-3_collection-copy-feedback_01 — Show keyboard-copy outcomes in ordinary Tables

[North star NS-3](../MILESTONES.md#assessment) · [Task catalog](README.md) ·
[Table workbench](NS-3_control-usefulness_03.md) · [Task flow](../TASKFLOW.MD)

**Description:**

The same saved Service monitor exposes a concrete feedback gap. A correctly
produced keyboard range-copy command reaches the ordinary unavailable clipboard
host and completes with typed Unavailable, zero bytes and its exact reason.
The ordinary Table shows only Columns: no visible result, status or explanation.
Menu Copy already presents its refusal. Users should not have to register a
callback to learn that their keyboard Copy did not write anything.

Provide reusable default clipboard feedback for ordinary renderer-managed tables,
using the existing typed result, original command/controller and explicit hosts.
Keep navigation/editor focus and live drafts intact. Reuse the existing table
presentation/retirement infrastructure; no second clipboard request engine,
permission grant, row refresh or source mutation belongs here. Preserve caller
observers and specialized alternative implementations. This is a prerequisite of
the existing workbench's visible-outcome/keyboard criteria, with no separate
completion credit or percentage.

Starting evidence: the maintained
[same-application browser consumer](../tests/nyx_table_workbench_keyboard_browser.lpr)
separates raw input, host entry and terminal receipt without keyboard subscribers.
The browser driver's Control+C shortcut produces no DOM key; that is separate
from Nyx's admitted keyboard command. Explicitly labeled Pascal synthetic input
proves exact real clipboard writes at both widths. The unavailable case completes
honestly but has no ordinary visible feedback. Receipts and the wide screenshot
are in table-keyboard-return-01; [WORK](../WORK.md) records the target/input limits.

**Acceptance Criteria:**

- A user-initiated data-cell/range Copy displays a useful accessible outcome on
  ordinary browser/LCL tables. Unavailable, activation required, denied, failed
  and reported cancellation cannot look like success or silently retain an old
  success. Written reflects the existing verified receipt. The explanation is
  reachable at wide/compact widths without opening an unrelated inspector.
- Use the existing typed command/result and single host request slot. Preserve
  application-owned observers, canonical cancellation, scheduler behavior and
  exact raw payloads; each admitted result is delivered once. Consumed/stale input
  cannot claim a write or borrow a newer command's status. Existing menu feedback
  remains usable and does not trigger another host effect.
- Feedback changes presentation only. It must preserve accepted rows, row/range
  selection, the exact live editor/draft/caret, original input lifetime/epoch,
  authored defaults and independent peer tables. Active editors retain their
  ordinary text-selection/copy route. A count/status change must not refresh rows.
- Ownership is explicit and acyclic. UI receivers detach before target teardown;
  late/cancelled completions cannot update a retired/replacement mount or replace
  a newer outcome. Alternative mounts and caller-owned presentations remain
  honest about support, without breaking existing interface implementations.
- Reproduce the missing ordinary feedback before repair and qualify the repaired
  same saved/generated application through browser and native controls at both
  widths. Retain original regression assertions, failure evidence and clean native
  heaps. Distinguish raw/synthetic/widget-message input, real clipboard effects and
  physical/assistive coverage. Preserve exact project pairs/history, clipboard
  items, output choices, unrelated projects and fixed listener paths.

**Blockers**

- [Browser semantic Copy input](DONE/NS-2_browser-copy-input_01.md) supplies the missing
  standard Copy-event entry before qualifying ordinary browser outcome feedback.

- [Generated table output hosts](DONE/NS-4_table-output-hosts_01.md) supplies the
  existing explicit clipboard capability and honest runtime absence.
- [Ordinary recovered-edit review](DONE/NS-3_collection-recovery_01.md) supplies
  the shared table-caption ownership and busy-input presentation baseline.
