# NS-4_http-output-hosts_01 — HTTP services in Studio-built applications

[Milestones](../MILESTONES.md) · [Tasks](README.md) · [Current work](../WORK.md)

**Description:**

JSON Workshop's connected URL graph executes through explicitly configured HTTP
transports in its public application and native consumers. The current Studio
build executor generates file-reading/saving setup but no HTTP resolver, and
`TNyxOutputConfiguration` has no HTTP service choice. Compiling a document with
an HTTP graph therefore does not establish a functional Studio-built network
tool. Use the existing typed transports/resolver; do not create another HTTP
engine or grant host authority from saved graph data.

This prerequisite belongs to NS-4's delegated application/runtime integration,
consuming logic-components01. It adds no completion credit or waiver to either
original owner. Keep output configuration independent of design and selectable
at any time, with no compiler prerequisite for opening/authoring Studio.

**Acceptance Criteria:**

- A typed fluent local output option and ordinary Studio Outputs control select
  HTTP services explicitly. Preserve old profile meanings, compatible wire
  encoding, per-setting loading and unrelated fields. Bad values/unfinished
  edits preserve the accepted profile. Host choices and compiler paths remain
  outside portable project/design files.
- Native/browser generated entry points configure the existing HTTP resolver
  and correct platform transport when selected, before application mounting.
  File hosts and resources compose with it. Missing authority remains explicit;
  graph configuration alone never chooses a host implementation.
- Retain UI-thread scheduling, independent application stores, cancellation,
  receiver retirement and transport ownership. Unsupported native services have
  an honest readiness/diagnostic path without blocking design work.
- Exercise actual profile persistence, ordinary output controls and both
  generated application paths for the same MCP-authored Workshop. Compile its
  accepted source and execute real URL-control input through the selected host;
  manual test-only transport injection and entry-source inspection do not alone
  establish generated runtime integration. Preserve newer edits/retirement,
  existing projects, paired saving and every original browser/quality/parity gate.
- Keep current evidence separated from compiler success, physical input, visual
  observation and native platforms not actually qualified. Use external storage
  for all new runners, compiler outputs and child temporary files.

**Blockers**

- None for implementation: the existing HTTP contracts, transports, output
  profile and builder are present. Full acceptance still requires the runtime
  interaction evidence above and the original owners' remaining quality gates.
