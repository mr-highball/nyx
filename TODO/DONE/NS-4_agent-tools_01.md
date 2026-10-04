# NS-4_agent-tools_01 — Semantic agent operation

[Milestones](../../MILESTONES.md) · [Catalog](../README.md) · [Work](../../WORK.md)

**Description:** Deliver the user's Pascal-only MCP integration for Nyx Studio.
Agents operate the active editor document through the same admitted model and
paired-source history. Observing editor views reflect committed agent changes.
This is an additional product requirement; existing authoring, compiler and
performance acceptance remains with its original owners.

**Acceptance Criteria:**

- A local, isolated MCP HTTP endpoint negotiates the protocol and advertises
  semantic tools with explicit schemas and bounded, queryable responses.
- Selection, page/component structure, properties/events, discovery metadata,
  design tokens and diagnostics can be inspected without fetching a whole design.
- Creating, updating, reparenting and deleting controls supports atomic grouped
  edits, exact revision checks and one undoable paired design/source transaction.
  Rejected/stale work preserves accepted owners, drafts and history.
- Connected Studio views visibly receive agent changes; local editor changes
  reach the same session and conflicts retain work. Native/shared ownership and
  browser interactions have executed evidence.
- A Nyx-built observer panel exposes agent activity and operator-controlled
  Disabled / Read only / Allow edits permissions. Agents cannot raise their own
  access or hide their operations.
- Actual rendered previews are available selectively. Per-session localhost
  connection details are included in Codex configuration on launch/connect,
  preserving unrelated configuration and explaining client reload requirements.

**Blockers:** satisfied by accepted model, persistence/scalar state and
specialized contracts, and the admitted paired-source/history boundary under
the open codegen task. Full native Studio remains a separate open owner.

**Acceptance — 2026-10-04:** all six original criteria have executed evidence.
The real MCP HTTP consumer passes 29 checks, including negotiation, schemas,
origin refusal, revisions, typed atomic edits, history and an actual Nyx PNG.
Portable native and executed pas2js consumers each pass 28 checks. Connected
desktop and exact-390-pixel observers each pass 12 browser checks, with seven
native coordinator checks per journey. Eight live bridge checks preserve local
recovery, cancellation and queued drafts. Native consumers have zero leaks.
LCL mounts token overlays/removal and the Nyx-built Agents controls.
Broader gates pass 1555 shared checks, 51 DOM interactions, 64 desktop and
64 compact authoring checks, 66 native authoring checks and 165 HTTP builds.

The verified artifacts are installed at the authorized LAN instance. Editor
listens on all interfaces; MCP listens only on loopback. Real Codex discovery
reports `nyx_studio` enabled with the new session URL. Unrelated configuration
bytes are preserved. An already-open client may need to reconnect. Capture
housekeeping, serialized compiler delays and the full native Studio controller
remain documented limits. See [agent guide](../../docs/studio-agents.md) and
[evidence](../../WORK.md#semantic-agent-operation-and-compiler-navigation--2026-10-04).
