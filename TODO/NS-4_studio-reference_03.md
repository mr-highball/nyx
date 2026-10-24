# NS-4_studio-reference_03 - Browse class references with Nyx controls

[North stars](../MILESTONES.md) · [Task catalog](README.md) · [Task flow](../TASKFLOW.MD)

**Description:**

Add searchable help and class/control reference lookup directly to Nyx Studio,
built entirely from public reusable Nyx controls and shared browser/LCL adapters.
The experience composes the reflected registry, including contextual access from
the current component, inspector and declared API identity.

Starting evidence: [NewNyxComponentHelp](../src/nyx.component.help.pas),
[BuildNyxStudioComponentHelp](../studio/nyx.studio.help.pas),
[managed popovers](../docs/popover.md) and
[current native help journey](../tests/nyx_studio_help_controls.lpr).
The existing action displays creator descriptions/capabilities; it is not yet
a searchable class/member reference.

North-star owner: NS-4. Milestone outcome: reflective Studio help under M4, consumed by M5 adoption.
Completion credit: pending evidence-backed assessment. These are open tasks;
planning earns no implementation credit and changes no existing acceptance,
counter or DONE status. Existing host/native and language-engine owners retain
their full requirements.

**Acceptance Criteria:**

- Provide a discoverable Studio Help/reference entry plus contextual lookup
  from selection, palette and inspector property/event entries. Use registered
  identities for classes, interfaces and members; source-caret lookup is optional
  where an exact resolver identity is actually available, with explicit
  unavailable/ambiguous results otherwise.
- Build search, navigation, result pages, detail views, member/type lists,
  examples, related links and contextual cards entirely with public Nyx controls,
  shared state/events and managed presentation. Studio must not add a separate
  DOM/LCL help toolkit or embed an unrelated web documentation application.
- Search qualified/simple class and control names, aliases, descriptions,
  properties, fluent methods, callbacks and referenced types. Reuse the existing
  Unicode search facilities and bounded/paged query patterns; support empty,
  no-result, duplicate-name and unavailable-platform cases.
- Show signatures, declaring/inherited type, summaries, defaults/constraints,
  event payloads, callback parameters/results, examples and target capability
  from the registered source. Navigate stable type/member/related links and
  preserve a useful back/forward trail without maintaining copied reference prose.
- Display metadata/example text safely as content. Reading help or navigating
  a link cannot execute examples, modify the design/source pair or grant file,
  compiler or agent authority; external links have an explicit user action.
- Keep help read-only with respect to project editing, drafts, caret and
  Undo/Redo. Closing a panel/popover restores the expected focus; selection
  changes and registry generations refuse stale contextual results.
- Qualify keyboard, focus, accessible labels, scrolling and desktop/narrow
  layouts in actual native and browser Studio. Package the same reference data
  and Nyx UI for a static client build with no required backend, account or
  documentation server; missing optional output compilers cannot block help.

**Blockers**

- [Automatic reference discovery](NS-4_studio-reference_02.md).
