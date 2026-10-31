# NS-3_menu-first-adoption_01 — Make the first authored menu work end to end

[North star NS-3](../../MILESTONES.md#assessment) · [Task catalog](../README.md) ·
[Typed Table actions](../NS-3_collection-cell-context_01.md) · [Task flow](../../TASKFLOW.MD)

**Description:**

The MCP-authored Service monitor exposed two first-menu failures on 2026-10-30.
Discovery advertises a `reusable` menu root, while the persisted menu reader
requires `component`. Correcting that request admits atomically, but adding the
first menu to an older handwritten source frame omits its new typed imports.
The compiled browser application then exposes a disconnected popover host when
mounting directly into `document.body`: menu binding prepares that host before
the initial render clears its target.

Repair the existing discovery, source-reconciliation and browser presentation
boundaries. Preserve typed persistence, authored source, ordinary menu ownership
and the existing renderer/event machinery. This is a prerequisite of the same
Table custom-action workflow, with no separate milestone credit.

**Acceptance Criteria:**

- Actual MCP discovery advertises exactly the menu reader's page/component
  choices. Every advertised root choice admits through that ordinary reader.
  Keep menu names distinct from control IDs; malformed groups refuse atomically.
- Adding the first menu to a pre-menu handwritten frame retains its authored
  text and reconciles each required namespace once. Its exact exported companion
  reconstructs the saved design, compiles and executes on FPC and pas2js.
- A standalone browser application mounted into the body opens its saved menu,
  nested menu and remounted menu through actual rendered invokers. Run existing
  assertions at measured desktop and compact widths; retain the original failure.
- The useful MCP-authored Table action works through ordinary managed browser
  and native menus. Keep peer applications, saved defaults, clipboard, project
  pairs, output choices and fixed listeners intact. Record compiler-service
  failures separately from successful native consumer compilation/execution.

**Accepted evidence — 2026-10-30:**

- Discovery uses page/component on the actual replaced QA backend. The native
  semantic/schema suite passes 80 checks with a clean heap, including each
  advertised choice through the ordinary persisted-definition reader.
- The original first-menu import failure is retained. Source/declaration checks
  pass 73 on both targets; the exact exported promoted companion compiles and
  executes on both. The native consumer frees all 1,169 allocations.
- The original disconnected-host browser failure is retained. Existing nested,
  command and remount assertions pass 14 at each measured browser width.
  The final full clipboard/control regression passes 1,755 per browser width.
- The same MCP-authored useful action and stale-revision refusal pass browser
  input and 43 native generated-application checks per width, with clean heaps.
  The refreshed backend passes 12 further real MCP import/first-menu/source/
  Undo/Redo operations, freeing all 30,363 client allocations. The pre-menu
  handwritten frame receives required imports automatically.
- Fixed-slot QA replacement preserves all nine project revisions, selections,
  active views and history availability, plus the exact output profile. The
  verified 27-file preservation copy remains on external storage; LAN is intact.
  Recovery's first compiler crash and expired retry context are retained. The
  exact failed source compiles unchanged; a fresh recovery completes all 20
  saved units. Native compiler-service deadline failures remain separately open
  under native cold-build01 and do not receive success credit here.

Evidence packet: table-context-return-01; current return and limits: WORK.md.
All criteria above are accepted for this prerequisite only. Return to typed
Table actions and their remaining native build-service gate; no parent criterion
or additional milestone credit is assigned.

**Blockers**

- None remaining for this scope.
