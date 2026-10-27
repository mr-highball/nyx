# NS-4_agent-workflows_03 — Import projects through the native semantic client

[Milestones](../../MILESTONES.md) · [Tasks](../README.md) · [Current work](../../WORK.md)

**Description:**

The native developer client closes its MCP connection after each `call`.
Project uploads and reviewed candidates intentionally expire on disconnect, so
separate invocations cannot perform the existing upload/review/Apply workflow.
This blocks the current JSON Workshop return through semantic tools even though
the server supports the required operations. Keep the server's ownership rules.

Add an explicit project-import command that retains one authenticated connection
for the complete bounded transfer. This is a developer-client prerequisite of
agent-workflows01's ordinary semantic workflow, not another server tool or an
alternative document model. Original NS-4 criteria and completion credit remain
with that owner. Compiler execution, generic persistent agent transports and
observing/browser/full authoring quality remain separate requirements.

**Acceptance Criteria:**

- The command requires an explicit workspace, expected revision, project file
  and final operation identity. Existing registration/discovery/single-call
  behavior remains available. It does not silently target the primary project.
- One actual initialized MCP connection owns begin/append/review/Apply. Use
  literal matching admission and existing revision guards; never silently switch
  to compiler execution or auto-retry an ambiguous mutation.
- Advertised omitted admission and explicit literal admission behave identically
  through actual MCP dispatch; invalid choices refuse without compiler execution.
- Validate the bounded input's UTF-8 before transfer. Preserve exact bytes,
  supplementary scalars and separate scalar offsets/byte accounting through
  bounded chunks. No filename, credentials or machine configuration enters the
  portable document. Return compact structured success/refusal data.
- Stale revisions, malformed UTF-8/JSON, empty/oversized files and semantic
  refusal preserve the accepted pair. Disconnect retires unfinished uploads and
  tickets. Qualify the real protocol, exact exported pairs, ordinary Undo/Redo,
  unrelated project preservation and the complete useful Workshop import.
- Document the command, ownership, exit codes and admission limits. Keep all
  temporary runners/output on configured external storage and preserve prior
  failure evidence. No compiler or browser interaction is claimed from import.

**Blockers**

- None. This client prerequisite uses the existing qualified protocol without
  requiring full parent-workflow acceptance. Its original observing/quality
  requirements remain with [agent workflows01](../NS-4_agent-workflows_01.md).

## Acceptance — 2026-10-27

All six scoped criteria pass. The maintained client keeps one initialized
connection through the existing protocol, uses literal matching and preserves
compact semantic refusals. Its build passes22 configuration checks. Actual
protocol qualification passes19 lifecycle/default-admission checks without
publication or compiler execution. The omitted optional admission dispatch bug
is repaired in the live server; ownership and revision guards remain intact.

The useful MCP Workshop imports, accepts a three-operation grouped edit, restores
the exact original pair with Undo and the whole edit with Redo, then imports its
exact65905-byte/65899-scalar supplementary-text pair. Stale, malformed UTF-8/JSON,
empty and oversized inputs refuse. Windows Unicode project/argument filenames,
registration, discovery and ordinary call pass; general Unicode enrollment roots
are not claimed. Two unrelated projects and client settings remain exact across
activation and staging. All readers are joined; no browser/runtime execution,
original parent acceptance or extra completion credit follows.

WORK.md's current semantic URL checkpoint records receipts under the private
configured evidence root: mcp-project-import-02, mcp-default-import-01,
mcp-default-import-activation-01 and mcp-unicode-input-01. Earlier refusal and
orchestration evidence is retained beside the successes.
