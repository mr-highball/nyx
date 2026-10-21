# NS-3_data-toolbar_01 — Live collection search, sort and clear

[North stars](../MILESTONES.md) · [Task catalog](README.md) · [Task flow](../TASKFLOW.MD)

**Description:**

Make the existing DataToolbar an opt-in useful collection-control experience.
Its current recipe creates search/sort faces and semantic actions but does not
coordinate a target List/Table. Reuse `INyxCollectionView.ConfigureQuery`, its
query policy and typed predicates/orders so Studio authors can configure a
toolbar, save/reopen it and use the exact generated application.

Scope/credit remain [components](NS-3_components_01.md) criteria1/3 →
[Studio authoring](NS-4_studio-authoring_01.md) criterion2 → parity. Preserve the
static recipe; introduce no second collection, selection or event engine.

**Acceptance Criteria:**

- Copied typed options identify the scoped target List/Table and schema-checked
  search/sort fields, order and comparison. Configured/Disabled/Inherit preserve
  reusable semantics and strict codec/source admission. Missing, foreign,
  ambiguous or incompatible targets refuse. Named parts, captions, column
  cosmetics, paging/typeahead and create/refresh callbacks remain independent;
  enabling coordination never silently changes caller event policies.
- Actual typing, typed sort choice and a visible accessible Clear action update
  the bound view through its existing query owner. Transient search composes with
  the admitted authored baseline; Clear restores that exact baseline. Displayed
  membership/order and empty results prove behavior. Row/default data and hidden
  selection membership remain unchanged. Intervening target/query changes have
  explicit ownership/admission rather than blindly restoring stale query state.
- The common prepared coordinator path supplies dormant initial behavior and
  prospective event generation. Exact source/target/query authority, cancellation,
  stale input and replacement are checked before mutation. Reusable instances
  remain isolated. Refusal leaves query/snapshot unchanged; observer failure after
  publication is committed. Navigation/disposal revokes late work and host access.
- A public Nyx form and ordinary paired Studio candidate configure/disable/inherit
  whole toolbars and named reusable toolbar descriptors. Required role/spec
  changes admit together, preserving siblings, definitions, unfinished drafts and
  stale/pending-source refusal. One Undo/Redo includes intent and Pascal.
- Native desktop/390 Studio input configures and exercises search/sort/Clear,
  Save/Open and the exact saved generated application. Matching browser consumers
  retain the same assertions and async owners; browser compilation does not
  establish input/parity. Useful examples explain opt-in registration/static
  compatibility. Preserve existing quotas and no new MCP/tool/wire rollout.

**Blockers**

- [Useful query runtime](NS-3_data-toolbar_02.md) owns public options and actual
  search/sort/Clear behavior. This parent retains the original configured/
  Disabled/Inherit codec/source rules, paired Studio authoring and complete
  saved/generated journey. Original criteria1-3 remain integration gates and
  criteria4/5 remain here; the split adds no credit or relaxed acceptance.
- [Prepared coordinators](NS-3_coordinators_01.md) supply creator-extensible mounting.
- [Observable state and collections](DONE/NS-1_state-collections_01.md) and the
  existing collection-query/view owners supply authoritative data semantics.
- Full [parity](NS-2_parity-accessibility_01.md) quality/observing gates stay open;
  Studio authoring is a return owner, not a circular prerequisite.
