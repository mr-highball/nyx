# NS-5_browser-compiler_01 - Qualify a Nyx-compatible browser compiler

[North stars](../MILESTONES.md) · [Task catalog](README.md) · [Task flow](../TASKFLOW.MD)

**Description:**

Establish a reproducible, redistributable pas2js compiler running locally in a
browser worker for the WEB target. The compiler may be WebAssembly while its
Nyx application output remains JavaScript with matched pas2js RTL. This is an
optional alternative to the existing host compiler, with native output retained.

Starting evidence: the [upstream August 2025 announcement](https://lists.freepascal.org/pipermail/fpc-pascal/2025-August/062972.html)
describes this general architecture and explicitly notes incomplete units and a
one-compilation limitation in that demonstration. It does not qualify Nyx.
Existing [compiler contracts](../studio/nyx.studio.compiler.pas),
[captured inputs](../studio/nyx.studio.compilerinputs.pas) and
[Pascal project ownership](../docs/pascal-projects.md) are the integration baseline.
The Nyx-owned editing resolver remains independent of compiler admission.

North-star owner: NS-5. Milestone outcome: optional client-only WEB authoring under M4, consumed by M5 delivery.
Completion credit: pending evidence-backed assessment. These are open tasks;
planning earns no implementation credit and changes no existing acceptance,
counter or DONE status. Existing host/native and language-engine owners retain
their full requirements.

**Acceptance Criteria:**

- Pin the compiler source/release, WASM build recipe and binary hashes,
  bootstrap tools, compiler options, matching RTL/unit set and Nyx SDK revision.
  Record required browser features and distinguish the compiler's WASM format
  from the application's WEB output.
- Document actual FPC/pas2js dialect, generics, interfaces, callbacks, Unicode,
  conditional defines, units/includes and resource support needed by maintained
  Nyx sources. Derive the required file closure from those sources; a version
  banner or a tiny Pascal example cannot establish compatibility.
- Run a bounded real browser probe compiling current Nyx generated application
  and focused view/reusable inputs, including a supporting unit, physical include,
  callback and resource. Execute the resulting UI and callback in an isolated
  probe context; retain exact input, version, diagnostics and output identities.
- Identify how the existing compiler-backed handwritten-source construction
  and admission contract can run locally without executing user code in the
  editor realm. Exercise a supported edited constructor and failure case in the
  probe; record any missing capability as an explicit blocker.
- Prove at least two successive compilations and a failed-then-successful retry
  without reloading the editor. Compare representative results with the retained
  matched host pas2js path and state every unsupported case.
- Review compiler, WASM loader, RTL, SDK and bundled unit/resource licenses
  separately. Preserve Nyx's MIT terms and required third-party notices, source
  offers or redistribution conditions; do not describe dependencies as MIT by
  association.
- Produce a finite compatibility decision with reproducible evidence and a
  declared probe budget/stop condition. An unresolved compiler or licensing gap
  keeps this task open and does not silently select a newer compiler or download
  an unverified toolchain.

**Blockers**

- None. The existing contracts above are starting evidence; full host-service or language-engine completion is not a prerequisite.
