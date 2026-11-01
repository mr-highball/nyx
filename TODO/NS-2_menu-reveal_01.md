# NS-2_menu-reveal_01 — Keep compact nested menu actions reachable

[Milestones](../MILESTONES.md) · [Task catalog](README.md) · [Task flow](../TASKFLOW.MD)

**Description:**

The current ordinary Resource Workshop Studio journey passes native at desktop
and compact allocations and browser at desktop width. In the 390-by-900 browser,
opening the Project branch does not leave a mounted Open project item. The
original direct click fails; waiting for actual item publication reaches the
unchanged stage-20 deadline. Preserve this failure instead of skipping Save/Open
or raising the deadline. The logic-toolbox01 packet owns the two browser failures
and native97/99/browser76 evidence.

Inspect the actual menu/anchor geometry and lifecycle before selecting a repair.
Browser menu FocusFace currently focuses without scrolling, while child popovers
require visible anchors. An offscreen parent item is a hypothesis to qualify,
not an established root cause. Fix the reusable menu/popover adapter when that
owns the failure; keep Studio on public Nyx contracts. This is an existing
authoring/parity prerequisite, with no new completion weight or approval boundary.

**Acceptance Criteria:**

- Retain a bounded failing reproduction identifying the real parent item,
  mounted submenu, viewport/scroll state and any refused anchor or retired owner.
  Distinguish missing publication from clipped, hidden or closed content.
- Browser/LCL nested-menu navigation keeps the intended item and submenu
  reachable at desktop, compact and short allocations. Keyboard focus and
  ordinary pointer activation agree, including an item initially outside the
  popup's visible portion. Reveal stays within the owned popup; the application
  page and unrelated controls retain their reading position and focus intent.
- Existing outside-press, Escape, disabled-item, callback ordering and retirement
  contracts remain. Opening/invoking a child does not accidentally dismiss a
  newer presentation, dispatch twice or revive a retired menu.
- The original ordinary Studio Resource Workshop journey completes project
  actions, paired history and actual Save/Open at both allocations on each
  target. Preserve the original assertions, deadline and independent application
  checks; compilation or a synthetic menu-only pass does not close this journey.

**Blockers**

- [First menu adoption](DONE/NS-3_menu-first-adoption_01.md).
