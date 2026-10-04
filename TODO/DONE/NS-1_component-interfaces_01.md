# Specialized managed component contracts

[Milestones](../../MILESTONES.md) · [Catalog](../README.md) · [Task flow](../../TASKFLOW.MD)

## Description

NS-1 owns the user-requested specialized classes, interfaces and factories for
every built-in control and compound component. The old prototype used specialized
interfaces, while the current generator declares every local as TNyxNode. Restore
that authoring intent on the portable model without losing existing low-level
construction, explicit tree admission or renderer independence. Interfaces must
retain actual component lifetimes, including configuration and binding objects;
they cannot be decorative borrowed wrappers. Credit: Pending assessment.

Accepted 2026-10-03 on hello-nyx. Maintained nyx.controls provides all 76 enum
contracts (75 catalog kinds plus the internal override), typed families and
compound parts, actual adopted implementation retention, and managed fluent
facades. Studio generates specialized interfaces/factories and accepted legacy
companions recover their exact handwritten Unicode helpers. Both-target shared
fixtures pass 30 + 980 checks, including 184 managed checks; compiled browser
reconstruction passes 32. Each compiler rejects 24 intended wrong-type cases.
Actual adapters each pass ten managed-control/lifetime/factory-recovery checks;
the browser Studio passes 49 interactions and 36 desktop/36 exact-390-pixel
authoring checks, while LCL authoring passes 25. HTTP passes all 57 checks across
both targets and ten build scopes. Checked native heap tracing reports
109611743 allocations/frees and zero unfreed blocks. Commands and artifact paths
are recorded in [WORK](../../WORK.md); [managed controls](../../docs/managed-controls.md)
documents usage, ownership and target limits. Full event/scheduler, broader source
synchronization, production component behavior and native Studio retain their
required owners. No overall completion or unallocated credit is claimed.

## Acceptance Criteria

- Every built-in kind has a documented specialized interface, default class and
  typed factory. Appropriate text, editable value, checked, image and composition
  properties remain accessible through the specialized type. Wrong scalar and
  reference families fail compilation on FPC and pas2js.
- Retained interfaces and fluent configurations remain valid after document,
  ancestor or other local references are released. Tree links remain acyclic;
  detach/reparent, removal, admission failure and final release have executed
  native/browser evidence and native heap tracing. Legacy raw construction and
  explicit extraction keep documented ownership semantics.
- Alternative implementation objects satisfy the public contracts and compose,
  generate and render through both adapters without depending on a concrete
  default control class. Compound factories distinguish ready recipes from exact
  descriptor reconstruction and preserve independent named parts.
- Studio generation uses specialized interface declarations and factories for
  built-ins, retains typed extension construction, and reconstructs exact designs
  when compiled on both targets. Accepted source editing, recovery, existing
  generated companions and no-op determinism remain exercised.
- Actual Nyx-built Studio/browser and LCL consumers exercise retained and
  specialized controls. Documentation describes lifetime, extension and fluent
  usage; the complete catalog has an explicit coverage check.

## Blockers

- [Portable owned model](NS-1_model_01.md)
- [Catalog and theme](NS-1_catalog-theme_01.md)
- [Persistence and scalar state](NS-1_persistence-state_01.md)

The user prioritizes this contract before remaining compiler-companion work.
Return to NS-1_codegen_01 criterion 3 after this prerequisite is accepted.
