# Open task catalog

[Milestones](../MILESTONES.md) · [Task flow](../TASKFLOW.MD) ·
[Current work](../WORK.md) · [Accepted tasks](DONE/README.md)

Credits and overall contributions are `Pending assessment`; none are earned by
the initial documentation bootstrap. Dependency order is expressed by task
blockers rather than filename order.

| Goal | Open task | Purpose | Credit |
| --- | --- | --- | --- |
| NS-1 | [Portable owned model](DONE/NS-1_model_01.md) | Document, pages, components and node tree | Pending assessment |
| NS-1 | [Catalog and theme](DONE/NS-1_catalog-theme_01.md) | Accepted composable metadata and semantic visual tokens | Pending assessment |
| NS-1 | [Portable runtime identity](DONE/NS-1_identity_01.md) | Accepted Unicode admission and qualified reusable identity | Pending assessment |
| NS-1 | [Persistence and state](DONE/NS-1_persistence-state_01.md) | Accepted version-1 data, scalar bindings and commands | Pending assessment |
| NS-1 | [Observable structured state](DONE/NS-1_state-collections_01.md) | Accepted typed stores, persisted/source bindings, runtime scopes and Studio/control consumers | Pending assessment |
| NS-1 | [Specialized component interfaces](DONE/NS-1_component-interfaces_01.md) | Accepted managed specialized controls, factories and generated authoring | Pending assessment |
| NS-1 | [Events and scheduler](NS-1_event-scheduler_01.md) | Typed multiple callbacks, execution policies and target schedulers | Pending assessment |
| NS-1 | [Pascal generation](NS-1_codegen_01.md) | Deterministic adjacent Delphi-dialect source | Pending assessment |
| NS-2 | [Browser renderer slice](NS-2_browser-renderer_01.md) | Incremental DOM projection for M1 | Pending assessment |
| NS-2 | [LCL renderer slice](NS-2_lcl-renderer_01.md) | Incremental native projection for M1 | Pending assessment |
| NS-2 | [Parity and accessibility](NS-2_parity-accessibility_01.md) | Shared semantics, keyboard and accessibility | Pending assessment |
| NS-3 | [Production component catalog](NS-3_components_01.md) | Exhaustive control and layout families | Pending assessment |
| NS-3 | [Extension and performance](NS-3_extension-performance_01.md) | Custom components, virtualization and budgets | Pending assessment |
| NS-4 | [Studio vertical slice](NS-4_studio-slice_01.md) | Canvas, tree, inspector and split generated code | Pending assessment |
| NS-4 | [Studio authoring system](NS-4_studio-authoring_01.md) | Multi-page/reusable workflows, history and project UX | Pending assessment |
| NS-4 | [Semantic agent operation](DONE/NS-4_agent-tools_01.md) | Accepted local MCP tools, revision-aware transactions and observing Studio views | Pending assessment |
| NS-2 | [Resizable views and platform configuration](DONE/NS-2_split-platform_01.md) | Accepted public split panes, mobile Studio resizing and typed platform overrides | Pending assessment |
| NS-5 | [Compiler service slice](NS-5_compile-service_01.md) | Pascal HTTP view/application builds | Pending assessment |
| NS-5 | [Service hardening and reload](NS-5_service-reload_01.md) | Isolation, caching, cancellation and reload | Pending assessment |
| NS-6 | [Toolchain and delivery](NS-6_delivery_01.md) | Clean builds, CI, packaging and compatibility | Pending assessment |
| NS-6 | [Guides and evaluation](NS-6_adoption_01.md) | Examples, reference and independent use | Pending assessment |

The M1 chain begins with model and catalog. Persistence enables deterministic
generation; renderers consume model/catalog; Studio consumes model/catalog and
generation; the service consumes generated projects. Later breadth and quality
tasks depend on this accepted slice.
The model, catalog/theme, identity and version-1 persistence/scalar-state
foundations are accepted. Typed callback/inspector and executable companion
workflows are integrated; complete event capabilities and source synchronization
remain active contract work. Structured state is an accepted prerequisite for
production collection controls; their breadth, virtualization and performance
still have open NS-3 owners.
