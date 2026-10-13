# Selection styling for reusable compounds

[Components](components.md) · [Scalar contracts](contracts.md) ·
[Bindings](bindings.md) · [Current evidence](../WORK.md)

Steppers, ratings, segmented controls and custom scalar compounds can choose
their selection appearance through the immutable `TNyxSelectionOptions` value
from `nyx.selection`. The policy applies to direct `naSelect` parts. The root
retains its existing typed value domain, actions, callbacks and bindings.

```pascal
LSteps := NewNyxStepper('project-progress');
SetNyxSelectionOptions(LSteps,
  TNyxSelectionOptions.Defaults
    .Selected(nvPrimary)
    .Unselected(nvSecondary));

// A derived component may keep every part's authored variant instead.
SetNyxSelectionOptions(LSteps,
  TNyxSelectionOptions.Defaults.Styling(nssAuthored));
```

`nssVariants` sets the selected and unselected variants from the copied policy.
Missing policy preserves the previous primary/default behavior. `nssAuthored`
leaves each part's variant under creator control while derived Boolean `Pressed`
continues to track the root's accepted scalar selection. An explicit Pressed
binding retains its existing precedence. Variant styling follows scalar selection;
it does not reinterpret an explicit independent Pressed binding as a new value.
Custom adapters or browser styles can consume Pressed independently.

Fluent calls return independent values. Changing a recipe, one reusable instance
or a local policy override does not change another instance's policy or store.
Runtime projection changes only the realized tree. Authored nodes, declarations
and document-owned defaults remain exact. Readonly, enabled and visibility
admission still govern user commands; deliberate programmatic state updates remain
available. Styling does not introduce a command or bypass value-domain admission.

The known extension uses strict version-one fields. Missing, extra, wrong-family
or unknown choices refuse before complete candidate publication. A non-value host
or a host without direct selection parts refuses; reusable-reference overrides
are checked again in their complete expanded context. The default variant's
persistence name is empty, while authored/generated Pascal uses `nvDefault`.

Studio's ordinary Pascal admission understands the typed helper and enum families.
Generated code retains specialized interfaces and emits this fluent policy; exact
policy changes participate in paired Undo/Redo. A dedicated Inspector form and
bounded semantic policy operations remain with the existing Studio/workflow
owners. The installed observing release has its earlier selection behavior.

Current evidence executes 38 native checks, 39 generated native/browser checks,
and eight trusted Chromium input checks at desktop/CSS-390. It covers copied
policies, strict refusal, exact source reconstruction, ordinary paired source
admission, independent reusable overrides, state projection, physical controls,
keyboard/pointer defaults and readonly admission. Existing variant capability
grades are unchanged: this contract does not implement every palette effect or
establish full component, hardware/IME, assistive, widgetset or DPI/theme quality.

The maintained Pascal companion composes an English seed through authenticated
MCP, with one grouped transaction, bounded source export, paired history, an
ordinary installed compiler job and owned review discard:

```powershell
./build/selection-style/semantic-tool/nyx_slider_companion.exe <config.toml> <NEW-seed-directory> --selection
./tools/build.ps1 -Target selection-style -SelectionSourceDirectory <exact-seed-directory> -SelectionEvidenceDirectory <NEW-evidence-directory>
```

Compile `tools/nyx_slider_companion.lpr` into an owned tool directory first. The
companion's seed export stays unchanged; current policy enrichment is explicitly
performed by the public Pascal fixture. Installed seed compilation does not prove
execution or new-policy support on that older release.

Serve the staged `browser/selection-style.html` through an existing HTTP host.
The maintained ready-capture owner observes its selection-styles checkpoint and
`data-selection="passed"`. On Windows, `nyx_selection_input` takes the same page's
`#review` URL, a NEW capture directory and CSS width. It sends trusted host input,
uses exact published runtime identities and evaluates no scripts. Native controls
optionally take a generated-source path followed by a client-pixel PNG path.
