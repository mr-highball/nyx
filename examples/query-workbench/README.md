# Task query workbench

An English, two-page Nyx application composed through Studio's authenticated
semantic tools. The exact admitted companion runs in pas2js and FPC/LCL and
needs no running Studio server once exported.

Choose **Important only**, edit task points and press **Calculate summary**.
The callback captures immutable rows/filter on UI, then runs an owned filter/fold.
Native uses bounded workers; browser explicitly chooses deferred event-loop
execution. One typed summary updates the bound label. New requests supersede
old results, source changes require recalculation, and page changes/disposal
cancel obsolete work.

`nyx.generated.view.pas` is the exact semantic export, including handwritten
helpers/callback. `query-workbench.nyx` retains its matching portable design/source
pair for Studio import. `behavior.inc` supplies the regeneration tool's helper
template, inserted outside managed views; the exported unit compiles standalone.

With Nyx's existing local toolchain configured:

```powershell
& tools/build.ps1 -Target linq-projection -LinqWorkbench `
  -LinqSourceDirectory examples/query-workbench `
  -LinqEvidenceDirectory build/query-workbench-review
```

Choose a new evidence directory. This executes native control/context checks
and builds both entries. Run the native `query_workbench.exe`, or serve its
browser directory with a Pascal HTTP host and open `query-workbench.html`.
Browser execution is a separate HTTP step; compilation alone does not qualify it.

For semantic regeneration, compile `tools/nyx_query_workbench_companion.lpr` with
the existing FPC and `-Fusrc -Fustudio -Futests -Fisrc`, directing intermediates
and the executable into a created build directory with `-FU` and `-FE`.
Its arguments are the
enrolled local Codex configuration, a new evidence directory and `behavior.inc`.
It owns an independent review, groups design/state/collection changes, imports
the exact pair, qualifies paired Undo/Redo, captures ordinary MCP compiler
diagnostics and discards only its review. Primary work and machine profiles stay
retained. Use current-source Studio to compile this new runtime dependency;
the observing older release reports its missing context unit through `nyx_build`.
Literal admission is not compilation or execution evidence.

The example uses a trusted small dataset. General error/cancellation UI,
large-data budgets, browser workers, broader input/accessibility qualification
and preserving installed delivery remain part of the full LINQ/Studio tasks.
