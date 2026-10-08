# NS-2_split-platform_01 — Resizable views and platform configuration

[Milestones](../../MILESTONES.md) · [Catalog](../README.md) · [Work](../../WORK.md)

**Description:** deliver the user's touch-resizable Studio code pane through a
public Nyx component. Provide typed fluent platform-specific presentation and
interaction configuration without application compiler directives.

**Acceptance Criteria:**

- A specialized managed split-view control composes two independently owned
  child panes, with typed orientation, proportional position, bounds and
  resize enablement. Browser and LCL execute the same sizing semantics.
- Touch/pointer and keyboard dividers resize mounted panes without remounting
  editable controls. Bounds, cancellation and teardown preserve ownership,
  focus, source drafts and scrolling.
- Typed platform configuration persists, clones, generates crafted fluent
  Pascal and reconstructs through the source workspace. Both adapters apply
  only their selected overrides without changing the authored document.
- Studio consumes the public contract. Desktop and exact narrow browser
  journeys demonstrate resizing, remount/visibility retention and usable
  remaining canvas/code space. Actual LCL controls and both-target compiled
  contracts have executed evidence.

**Blockers:** accepted model, specialized interfaces and persistence foundations;
the existing paired-source boundary supports new typed configuration.

**Accepted delivery — 2026-10-04:** all four original criteria accepted, solo.
`INyxSplitView` / `NewNyxSplitView`, portable geometry and concrete DOM/LCL
adapters are integrated in Studio's public Nyx shell. Typed independent
`ForPlatform` scopes retain authored defaults and apply to realized views only.

Evidence: 141 native/executed-browser contract cases, compiled reconstruction on
both compilers, zero portable leaks, and 22 actual control cases per target.
Desktop and exact-390 Studio each pass 20 checks; the containing viewport journey
passes nine, including narrow/wide/narrow transitions, pending Unicode source,
caret/focus, both scroll positions, show/hide and optional output retention.
Inherited read-only and mid-gesture permission loss cancel safely on both targets.
Full shared gates pass 1564, native projections 75 and HTTP builds 173, including
isolated split companions on both outputs. Verified artifacts are deployed at the
LAN instance, with MCP loopback isolation retained.

See [split guide](../../docs/split-views.md) and
[delivery evidence](../../WORK.md#resizable-workspaces-and-interaction-breadth--2026-10-04).
Native Studio remains open under its original owner. This task does not close
general accessibility, platform parity or large-project source performance.

Maintenance qualification (2026-10-06): the viewport-key generator probe added
after this task's acceptance was clearing a successfully decoded static platform
scope. Disjoint namespace probes now preserve it. The original 141 split/platform
checks pass again under both native compilers; actual layout-policy browser
reconstruction also passes. This restores accepted behavior rather than adding
credit or changing the original criteria. WORK.md owns the reproduction and fix.

Maintenance integration (2026-10-08): explicit realized child axis minima now
constrain both adapters through shared geometry without rewriting the requested
percentage. Infeasible split tracks compress proportionally; gestures start at
the physical divider, outward no-op movement retains the preference, cancellation
restores it, and passive browser allocation has an owned observer. Shared checks
pass 152 on FPC and actual browser; controls pass 39 native/38 browser. Studio
consumes the contract for readable source beside Outputs, with 20 checks at both
browser sizes and nine viewport transitions. Source modal/input evidence belongs
to the original authoring owner. This neither duplicates accepted prerequisite
credit nor accepts full native Studio/accessibility. See
[the integration packet](../../WORK.md#current-return-path-useful-source-allocation--2026-10-08).
