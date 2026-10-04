# State and bindings in Studio

[Typed state](state.md) · [Live bindings](bindings.md) · [Current evidence](../WORK.md)

Open **State** in the Project panel to create, rename, edit or remove saved
defaults. Choose Text, Boolean, Integer or Number explicitly. Defaults initialize
each application runtime; changing a running control does not silently rewrite
the design. No output target or compiler is required for this authoring workflow.

For text containing NUL, Studio displays an escaped, quoted text literal through
an ordinary Nyx memo. **Text (escaped)** also creates such a default. Unicode,
quotes and control characters survive the Pascal JSON decoder exactly. Integer
fields require signed 32-bit whole values; Number fields require complete finite
decimals. Invalid input restores the accepted default and reports a diagnostic.
The declared scalar type of an existing default stays fixed in its editor.

Open **Bindings** in the Inspector after selecting a control or reusable part.
Choose a control property, then a state key with a compatible scalar type.
The choices come from Nyx's public `NyxBindingTargets`/`NyxBindingKinds` contract,
also used by runtime admission. The complete default/control constraints still
run before a binding is accepted; compatible type alone does not waive ranges,
text representability or application validation.

**Read and write** accepts control edits into state. **State to control** projects
the default without accepting control writes. Other binding properties project
from state. **Unbind property** records a typed clear; **Use inherited binding**
removes a local descriptor so the reusable definition supplies its contract again.
These operations preserve independent definitions and sibling instances.

The ordinary property editor shows effective bound defaults and disables their
hidden fallback fields. Edit State or Bindings to change that behavior. A two-way
bound field edited on the design canvas changes its saved default through one
undoable command. A state-to-control canvas edit is refused. **Interact** uses a
runtime store; a full application host preserves that store across navigation,
while a fresh isolated view mount starts from saved defaults.

Creation refuses duplicate keys. Rename retains default order, exact value and
kind, updating page, reusable-definition and instance-override binding references
atomically. Removing a referenced key is refused. Undo/redo restores the complete
design, including defaults, descriptors and generated reference meaning. No-op
and rejected authoring commands retain the accepted document and redo history.
Browser rejection also retains mounted canvas controls when the design is unchanged.

The optional Pascal split updates from the same public fluent contract after
accepted edits. State locals describe purpose and type, initialize once and are
reused by defaults and controls. Keys such as `replyText` and `replyTextState`
avoid repeated type suffixes. **Apply Pascal** can create/remove/reparent typed
controls, add pages and reusable definitions, and edit declarations, reference
initialization, scalar defaults, fluent bindings/domains and structured extensions through
the same whole-document admission and undo/redo boundary. Removing a referenced default or using a wrong
scalar family retains the accepted design and pending source buffer. See the
[supported source workflow](fluent-api.md#crafted-source-and-editable-configuration).
Compiler-service generation uses the same public contracts.

Existing Pascal control/state locals can be deliberately renamed while retaining
their strong types. Visual edits preserve those names, comments and unchanged
expressions. New defaults avoid collisions with authored names. State-key renames
keep the existing Pascal local. Reconciliation failures retain the accepted pair
and redo history; paired undo/redo restores exact source spelling.

Studio's state rows, new-default form and binding inspector are Nyx compositions
built from public controls and fluent configuration. `nyx.studio.commands` routes
their events into the portable session; adapters decode transport metadata into
closed enums before any model mutation. Full state keys travel as data, with
bounded ordinal widget IDs. Stale binding controls cannot act on a different
current selection. Drafts remain presentation data until Add, and focused new
defaults survive panel/viewport changes without creating history.

Shared source/history fixtures, compiled companions and actual browser/Lazarus
authoring journeys exercise these boundaries; exact counts/artifacts are in
[WORK.md](../WORK.md). Native preview labels/memos consume edited defaults,
bindings and source-declared domains. These are programmatic events; the native
harness rebuilds after callbacks return and does not yet provide a complete native Studio controller.
Broader Pascal synchronization, structured collections, extension/event contracts,
large-document performance and complete native Studio remain open work.

## Paired projects and recovery

**Open** reveals the Project file controls. Choose a saved project name and
**Save paired files**; the Pascal service writes `design.nyx`, the companion's
actual unit filename, and `project.nyxproject` beneath `.local/projects/<name>/`.
These are user files, independent of compiler profiles and build jobs. Names use
letters, digits, hyphens and underscores; the ordinary project title stays Unicode.
**Open saved project** reads those adjacent files and downloads a backup of the
current session before replacement. Native Studio's shared file controls render
in the LCL harness; its complete filesystem/HTTP controller remains open work.

**Download project backup** exports a portable `.nyxproject`: accepted design and
exact Pascal plus any pending draft and its original baseline. **Import project
or paired files** accepts that backup or a `.nyx` and `.pas` selected together, in
either order. **Download design + Pascal** exports only the accepted pair; browsers
may require allowing multiple downloads. Use the single backup to carry drafts.
No target or installed application compiler is required for these operations.

Both members are admitted before replacing the session or touching saved files.
Matching pairs preserve imports, helpers, callbacks, local names and comments
exactly. Unsupported or mismatched input remains downloadable while the current
document, source, draft and history stay intact. An explicit **Open using Pascal
values** replays the supported typed source subset. **Open design; keep Pascal as
draft** generates the admitted design's companion and retains the original Pascal
verbatim as the pending buffer. Two independent conflicting Pascal buffers require
a manual merge; Studio refuses to overwrite one to make the conflict disappear.
The documented typed builder grammar is replayed; ordinary helpers outside its
delimiters go through the delegated compiler. See the
[source boundary](fluent-api.md#crafted-source-and-editable-configuration).

Saves compare a content revision covering both adjacent files and recovery data.
Another client or external editor changing any member produces a conflict, even
if file timestamps match. Studio retains local work and offers **Back up mine and
open saved**, or a new project name for a separate copy. Delayed open responses
also require a choice if the local session changed while waiting. There is no
automatic last-writer overwrite. The repository retains `previous.nyxproject`.

A complete flushed `pending.nyxproject` commits a save. Member replacements and
the final packet follow; an interrupted process is recovered by rolling that
whole journal forward before the next repository read. Incomplete `.next` packets
are ignored. This provides coherent reads through the service, not a simultaneous
two-file filesystem rename for external watchers or a power-loss guarantee.
External edits are returned as-is for explicit admission, rather than silently
repaired from the saved packet.

Browser recovery now uses one coherent `nyx-studio-project-v2` storage write,
including the accepted pair, rejected/empty/stale draft, original baseline and
unresolved imported input. Legacy separate keys migrate only after paired admission.
Malformed wrappers remain in `nyx-studio-rejected-recovery-v2`; if that backup cannot
be retained, automatic writes stop. Combined transport/recovery packets have a
4 MiB UTF-8 budget. A recovery wrapper that exceeds its budget leaves the prior
readable snapshot untouched and asks for an explicit project backup. Output paths
remain private to the service and never enter portable project backups.

## Finding components

The Project palette defaults to the full **List**. Switch to **Grouped** for
purpose headings, use **Show group** to narrow either presentation, and combine
names or descriptive words in search. Groups reflect intent rather than whether
a component is primitive or compound. Search also understands labels and common
aliases such as `memo`, `dropdown` and `dashboard`.

**Details** displays creator descriptions without requiring hover. Hints/tooltips
offer the same description. Empty results provide **Clear filters**. Mode, group
and Details preferences are browser-local; they remain separate from designs,
paired backups, source drafts, output profiles and undo history. Search remains
transient. The shared Nyx palette composition and command router also drive the
actual native harness. A complete standalone native Studio remains open.

Creators can register their intent/help through the
[typed catalog metadata API](components.md#component-discovery-and-creator-descriptions).
