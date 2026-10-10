# Working in Nyx

Start with [PROJECT.md](PROJECT.md), then read the current task in
[TODO](TODO/README.md), [WORK.md](WORK.md), and only the relevant Athena
standards.

- Apply Athena [Task flow](athena/docs/task-flow.md): follow prerequisites,
  preserve acceptance criteria, record discovered gaps, and move a task to
  `TODO/DONE/` only with accepted evidence.
- Implement product code, generators, servers, build tools, and substantive
  test tools in Pascal using `{$mode delphi}`. Shell scripts may only
  orchestrate platform tools. Do not add Node, npm, Python, or a JavaScript
  framework. Generated JavaScript from pas2js is a build artifact.
- Keep the portable document/component contract independent of DOM and LCL
  types. Put pas2js browser and Lazarus Component Library behavior behind
  adapters and exercise shared behavior on both targets.
- Preserve explicit ownership. The document owns its pages and reusable
  components; nodes own descendants; renderers and views do not create
  reference cycles back into that tree.
- Treat browser and LCL parity as an acceptance requirement. A compile on one
  target does not prove parity, interaction, accessibility, or visual quality.
- Apply Athena [coding standards](athena/docs/coding-standards.md),
  [browser cautions](athena/docs/platforms/browser.md), and
  [validation matrix](athena/docs/validation.md).
- Preserve the project's thoroughly commented style. Document public types and
  methods, ownership and lifetime contracts, property meanings, failure behavior,
  and non-obvious implementation choices beside their code.
- Built-in authoring and generated code must use strong Pascal types: enums for
  closed choices, Boolean/numeric arguments and fluent configuration objects.
  Open application names use distinct references. Generated code should feel
  crafted, with names combining purpose/control type and readable blocks. Raw property strings
  belong at explicit persistence/extension boundaries, not default authoring.
- Documents own typed state defaults; runtime applications use independent
  stores. Preserve atomic candidate admission, exact Unicode/numeric values,
  token lifetimes and both-target wire/generation evidence. Do not claim live
  binding integration from store-only fixtures.
- Compound components are a first-class product requirement. Build advanced
  controls from reusable parts, named slots, state, event contracts and shared
  recipes. Customization and derived recipes must preserve independent ownership.
- Nyx Studio must itself be built with Nyx as the proof of capability. Put
  designer/editor controls in reusable library components and target adapters;
  Studio consumes public Nyx contracts instead of maintaining a second UI toolkit.
- Use Nyx Studio's semantic MCP tools as the primary way to inspect, compose and
  modify demos and active designs. Read bounded context, preserve user work,
  supply expected revisions and use one undoable transaction for related edits.
  Use rendered previews selectively for visual validation. Browser/LCL input
  harnesses remain necessary for behavior the document API cannot establish.
  Record missing semantic operations with the MCP task owner instead of silently
  falling back to screenshot-driven editor automation.
- Output targets are optional and can be selected/configured at any time through
  Studio's target/output section. Missing application compilers must not block
  launching a built Studio or designing a project. Keep machine compiler paths
  separate from exported designs and report readiness when that output is built.
- Keep editor defaults and chrome free of branch names and development milestones.
  Project titles and user-authored text supply the editor's project identity.
- Use English text for starter documents, demos and initial review examples.
  Keep broader Unicode coverage in dedicated qualification inputs; never narrow
  the portable text contract or rewrite user-authored content to meet this default.
- Leave a blank line above `if` blocks, as in the original author's preferred
  layout. Keep two-space indentation and expanded begin/end bodies.
- Declare `{$codepage utf8}` in owned Pascal units/programs and use `TNyxText`
  for the portable text contract (`UTF8String` natively, Unicode `String` in
  pas2js). The source directive alone does not protect native concatenation or
  ANSI RTL collections. Use `TNyxStrings` for owned user text and byte streams at
  file/HTTP boundaries. Verify supplementary Unicode through persistence,
  history, generated compilation and target controls; avoid global codepage changes.
- The user has requested solo execution for this work. Do not use sub-agents
  unless the user changes that instruction.
- Inspect [PROJECT.md](PROJECT.md) and the local toolchain before changing
  setup. Do not reinstall working compilers or IDEs.
- Preserve the MIT license and its existing `mr-highball` attribution in
  authored source files. Do not edit dependency source in place.
- Keep private paths, accounts, hosts, and personal configuration out of
  committed files.
- Use [WORK.md](WORK.md) for concise evidence and handoff state. Do not claim a
  task, milestone, target, or product complete from partial implementation.

Standards include the current Pointer Events Level 3 Recommendation and HTML
Living Standard drag model, linked in docs/events.md, plus WAI keyboard/grid
practice in docs/collection-views.md (checked 2026-10-04). Host input qualifies
browser defaults, not hardware/IME/assistive technology or another widgetset.
The generated reference covers 76 kinds. Bound tables use shared data-cell
movement with separate row membership, current-cell editing and one browser
cell Tab entry. Paging, cell selection and broader grid qualification remain open.

Codex project and explicitly enrolled user configuration refresh on each Studio
launch. The observing release advertises twenty-three tools. Native named handles
must be revalidated after per-launch authority rotation. Current project/user
configuration and the Pascal MCP client authenticate; the old chat handle needs
connection refresh after the current LAN update. Use semantic tools as the primary
design workflow; the Pascal client remains available for authenticated operation
and maintained transport-owning qualification. Current-source additions to
installed schemas still require their own rollout evidence. Missing
general source/import/state/binding/review operations belong to the existing workflow
task, not silent browser automation. No sub-agents. WORK.md owns current process,
qualification, deployment, preservation and remote-checkpoint state; verify
process identity before stopping a service and preserve the active user pair.

Local callback implementation editing is now semantic: inspect bounded
`nyx_pascal` windows at one revision, then apply exact expected text as one
grouped paired Undo step. Signatures, surrounding helpers and managed views stay
owned; ambiguous/conditional methods and pending drafts refuse. Use `nyx_build`
for ordinary Pascal compiler diagnostics, never infer successful execution from
source admission. The maintained input review qualifies actual browser/LCL
callbacks and observing Studio; richer source and review lifecycle remain open.

Root cleanup is semantic through `nyx_roots`: inspect exact roots/dependencies,
review at the current revision, then apply the unchanged group with its actor-
bound ticket as one paired Undo step. Never substitute a descendant delete or
replace the user's project to remove demo roots. Pascal helpers and document
defaults remain retained; compile to check application references afterward.

Installed project-file import is semantic through `nyx_project`: reserve
exact UTF-8 bytes, append bounded scalar windows at one revision, inspect/review
the complete input, then apply its exact owner/revision ticket through ordinary
paired history. Preserve current/incoming drafts and use explicit owned contexts
for tests. Private upload authority expires on revision/cancel/disconnect/recovery.
543 shared native/executed-browser and 34 authenticated isolated ordinary-editor
assertions qualify this contract, both compiler jobs and paired Undo/Redo. The
LAN release advertises 23 tools. The installed owned-review consumer passes 34
source/import assertions, four compiler jobs and exact paired Undo/Redo without
replacing protected work; the ordinary observer separately qualifies retained
source/modal/Resources presentation. Current authority/identity are in WORK.md.
Admission/compilation are separate from application execution and full parity.
