# NS-5_browser-compiler_03 - Bound browser compiler worker lifecycles

[North stars](../MILESTONES.md) · [Task catalog](README.md) · [Task flow](../TASKFLOW.MD)

**Description:**

Implement the worker execution adapter behind the selected local provider,
keeping compilation off the editor UI thread. Cancellation, limits and structured
physical-source diagnostics must preserve existing accepted source/history and
publication guards.

Starting evidence: [compiler contracts](../studio/nyx.studio.compiler.pas),
[captured input ownership](../studio/nyx.studio.compilerinputs.pas) and
[service lifecycle ownership](NS-5_service-reload_01.md). Host-process cancellation
does not establish browser-worker behavior.

North-star owner: NS-5. Milestone outcome: optional client-only WEB authoring under M4, consumed by M5 delivery.
Completion credit: pending evidence-backed assessment. These are open tasks;
planning earns no implementation credit and changes no existing acceptance,
counter or DONE status. Existing host/native and language-engine owners retain
their full requirements.

**Acceptance Criteria:**

- Define typed bootstrap, ready, compile, progress, result, failure, cancellation
  and retirement messages. Validate protocol/version, lengths and payload shape;
  associate every message with the current worker generation and request.
- Bound download, initialization, queue length, source/input bytes, output and
  diagnostic bytes, compilation duration and concurrent workers. State which
  memory limits the browser can actually enforce; handle allocation failures and
  measured memory pressure without claiming a hard quota the platform lacks.
- Cancel before start, during bootstrap and during non-cooperative compilation.
  Terminate/recreate a worker when necessary, release transferred buffers/object
  URLs and ignore late callbacks. Shutdown, project changes and adapter disposal
  must leave no owned active work or publishable stale result.
- Return structured severity, physical main/unit/include identity, line/column
  and exact submitted-source mapping for diagnostics. Preserve Unicode and
  authored line endings; distinguish compiler locations from editor navigation
  authority and refuse stale or ambiguous locations.
- Keep the ordinary editor responsive under a costly or looping compile;
  qualify cancel, timeout, failed bootstrap, malformed messages, worker crash,
  rapid backend changes and a successful retry with actual browser workers.
- Prove that duplicate/out-of-order responses, worker replacement and old
  provider generations cannot replace a newer accepted preview/source pair.
  Readiness requires successful compiler initialization, not only script loading.
- Measure cold initialization and warm compile latency with fixed input/version
  identities. Record browser/viewport/runtime evidence separately from native
  contract checks and compiled-only consumers.

**Blockers**

- [Studio provider selection](NS-5_browser-compiler_02.md).
