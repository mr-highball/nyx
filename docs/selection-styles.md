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
policy changes participate in paired Undo/Redo. Studio's Component group now
uses the public `NewNyxSelectionEditor` Card/Select/Button compound. Appearance,
selected variant and unselected variant are typed choices. Apply submits one
paired operation; Use inherited removes only the local policy. Exact owner,
domain, parts and local/effective baseline guards reject stale forms. Copied
scalar drafts survive panel refresh/collapse without becoming design commands.
The installed observing release has its earlier selection behavior.

Current-source `nyx_node` supports optional `selectionStyle=true`, `selectionOffset`
and `selectionLimit` (1..50). It returns local/effective policy, total count and
only the requested direct selection part identities. Read their text/properties
through the existing bounded node windows. `selection-set` and `selection-inherit`
participate in revision-aware grouped transactions, permission checks and paired
history. These source additions await qualified installed transport/rollout; the
current observer explicitly refuses them without changing the owned review.

The shared editor/semantic suite passes 23 native/22 executed browser checks;
native adds discovery-schema qualification. The ordinary Inspector and real paired
queue pass 18 each native/desktop/CSS-390 browser, including group draft retention,
apply, exact Undo/Redo, stale refusal, inheritance removal and unfinished Pascal.
This establishes those controls and worker paths, not every connected host lifecycle.

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
./tools/build.ps1 -Target selection-inspector -SelectionSourceDirectory <exact-seed-directory> -SelectionInspectorEvidenceDirectory <NEW-evidence-directory>
```

Compile `tools/nyx_slider_companion.lpr` into an owned tool directory first. The
companion's seed export stays unchanged; current policy enrichment is explicitly
performed by the public Pascal fixture. Installed seed compilation does not prove
execution or new-policy support on that older release.
The optional `--selection-observer` companion mode records the known old observer's
query/operation refusals and exact preservation before its ordinary seed build.
It is a qualification mode for that frozen release; normal `--selection` remains
usable with newer backends.

Serve the staged `browser/selection-style.html` through an existing HTTP host.
The maintained ready-capture owner observes its selection-styles checkpoint and
`data-selection="passed"`. On Windows, `nyx_selection_input` takes the same page's
`#review` URL, a NEW capture directory and CSS width. It sends trusted host input,
uses exact published runtime identities and evaluates no scripts. Native controls
optionally take a generated-source path followed by a client-pixel PNG path.
The Inspector target stages `selection-policy-editor.html` and
`selection-policy-authoring.html`, plus their matching source worker. Observe
`data-selection-editor="passed"` and `data-selection-authoring="passed"`
respectively; browser compilation alone does not establish either execution.
