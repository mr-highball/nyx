# Fluent layout policies

[Architecture](architecture.md) · [Capabilities](capabilities.md) ·
[LCL owner](../TODO/NS-2_lcl-renderer_01.md) ·
[Parity owner](../TODO/NS-2_parity-accessibility_01.md)

Use typed `Layout(nlColumn/nlRow/nlGrid/nlAbsolute)`, `Width`, `Height`, `Padding`,
`Gap`, `Flex` and `Visible` configuration. The document owns these values;
adapters consume them without making the portable tree depend on DOM/LCL types.

Use `nyx.layout.policy` for an independent fluent value builder. Managed controls
and raw descriptors accept the same policy through `Configure.Layout`:

```pascal
uses
  nyx.types, nyx.layout.policy, nyx.controls;

var
  LActions: INyxRow;
  LPolicy: TNyxLayoutPolicy;
begin
  LPolicy := TNyxLayoutPolicy.Row
    .Wrap(nfwWrap)
    .Align(ncaCenter)
    .Justify(njSpaceBetween);

  LActions := NewNyxRow('document-actions');
  LActions.Configure.Layout(LPolicy).Gap(12).WidthSizing(nsFill);
end;
```

Each builder call returns an independent changed value. A configuration copies
the policy; retaining or changing another value cannot change an existing view.
`Layout(nlRow)` changes direction alone. The policy overload copies direction,
wrapping, cross alignment and justification. `Wrap`, `Align` and `Justify` also
configure their individual typed choices without replacing the other choices.

| Contract | Meaning |
| --- | --- |
| `nfwAutomatic` | Rows wrap when their Nyx host is at most 600 logical pixels wide |
| `nfwNoWrap` | One source-ordered row line, including deliberate overflow |
| `nfwWrap` | Pack source-ordered lines using available content width |
| `ncaAutomatic` | Row center; column stretch, with the primitive's own self-sizing default |
| `ncaStart`, `ncaCenter`, `ncaEnd` | Position children on the perpendicular cross axis |
| `ncaStretch` | Stretch children lacking an explicit cross-axis size to their line/column extent |
| `njStart`, `njCenter`, `njEnd` | Position the main-axis group after allocation |
| `njSpaceBetween`, `njSpaceAround`, `njSpaceEvenly` | Distribute remaining main-axis space while retaining the authored minimum gap |
| `nsAutomatic` | Use a retained pixel metric or the primitive's natural/default size |
| `nsContent` | Request intrinsic content size, capped by available width |
| `nsFill` | Request the containing content extent; a weight still owns main-axis allocation |

`WidthSizing` and `HeightSizing` retain existing pixel metrics. Clearing the
sizing choice, or selecting Automatic, restores those metrics. Fill height needs
a definite containing height; otherwise content supplies the natural height.
Set a root's `HeightSizing(nsFill)` to propagate the host height into nested
weighted columns. A browser host must itself have a definite CSS height; the LCL
adapter uses its actual client extent. Fill does not subtract unrelated siblings;
use `Flex` when the intent is sharing their remaining space.

All specialized controls expose `MinimumWidth`, `MaximumWidth`, `MinimumHeight`
and `MaximumHeight` through their managed `Configure` interface. These optional
logical-pixel bounds accept integers from 0 to 100000. Zero is an explicit bound;
`Clear(atMaximumHeight)`, for example, restores an unset maximum. Individual
methods validate their scalar first; complete document admission also requires
each minimum to fit its corresponding maximum, for the common policy and every
effective target override.

```pascal
LNotesEditor.Configure.Constraints(
  NyxSizeConstraints
    .Width(NyxSizeRange.Minimum(180).Maximum(640))
    .Height(NyxSizeRange.Minimum(120).Maximum(480)));
```

Import `nyx.layout.constraints` for the copied value builders. They retain no
control or platform handle and refuse invalid changes without changing the
original value. Applying a complete `Constraints` value replaces all four
bounds; absent members explicitly clear their corresponding bounds in that
configuration scope. `ForPlatform` uses the same methods; an empty scoped bound
clears it rather than falling through to its common value. Studio's strict
source reader accepts these closed builders; generated code uses the equivalent
readable scalar configuration methods on specialized interfaces.

Bounds apply after automatic/content/fill sizing and during main-axis weighted
allocation. A capped sibling releases space for siblings still able to grow;
an explicit minimum can overflow the parent, retaining leading alignment.
Wrapped row membership includes weighted minima before allocating each line.
If all maxima are reached, justification consumes the remaining space.
Browser CSS owns its layout; the native zero-basis allocator follows the
min/max freezing rule in [CSS Flexbox section 9.7](https://www.w3.org/TR/css-flexbox-1/#resolve-flexible-lengths)
(Candidate Recommendation Draft checked 2026-10-05). This is the Nyx grow-weight
subset, not a claim of complete CSS sizing conformance. Browser border/padding,
intrinsic text metrics, flexible shrinking and every widgetset retain the
original renderer/parity gates.

Studio exposes the four typed fields and an **Unset** action for each published
bound, including present target overrides. Unset uses the same isolated property
processor and paired history. Its exact selected owner is captured; changing
selection refuses an old reset action. Semantic clients use an ordinary bounded
`nyx_node` query and one revision-checked `nyx_transaction` for related bounds.
Malformed/inverted common or effective scoped pairs refuse atomically. Current
source candidates require the matching built server; the protected observing
release has not been replaced.

Rows use actual caption/content widths rather than unspecified equal cells.
Both adapters collect line membership before distributing zero-basis positive
weights on each line. Wrapped lines retain natural cross sizes; a non-wrapping
definite row can center/end/stretch within its full height. Content sizing and
explicit pixel sizes remain explicit cross-axis choices. Grid/absolute layouts
retain flow choices for a later direction change, without interpreting them.
Safe center/end alignment keeps leading overflow reachable. Authored order and
keyboard order remain unchanged.

Automatic wrapping follows the embedded Nyx host, rather than an outer browser
window. The browser adapter uses a size container; the native adapter uses its
host's client width. These bridges follow the [CSS flex line and alignment
contract](https://www.w3.org/TR/css-flexbox-1/). Font/widget metrics can differ
between targets; a natural caption is measured by its actual host control.
Native themed buttons report their painted font/caption through `GetPreferredSize`.
Natural parent height measures a child at its actual authored/allocated width.

Use `Configure.ForPlatform(npfBrowser/npfNativeLCL)` with these same enum methods
for a deliberate target-specific presentation rule. Rules belong to portable
descriptors; realization applies only the selected target to an independent tree.
Studio's header consumes this public policy, and its compact panel buttons use
explicit equal weights. Studio/MCP expose closed enum choices and their help;
generated code uses typed Pascal methods, including Clear for optional choices.

```pascal
var
  LWorkspace: INyxColumn;
  LHeading: INyxLabel;
  LEditor: INyxMemo;
begin
  LWorkspace := NewNyxColumn('workspace');
  LWorkspace.Configure.Height(400).Padding(12).Gap(12);

  LHeading := NewNyxLabel('workspace-heading');
  LHeading.Text('Notes');
  LHeading.Configure.Height(28);

  LEditor := NewNyxMemo('notes-editor');
  LEditor.Text('Write a note').Value('');
  LEditor.Configure.Flex(1);

  LWorkspace.Add(LHeading).Add(LEditor);
end;
```

Positive weights share the remaining main-axis space after padding, visible gaps
and fixed children. Rows use the available width. Columns need an authored height
or a height allocated by their parent; an auto-height column keeps natural
content sizing. A nested weighted row/column can allocate its own descendants.
`Flex(0)` opts out of weighting. `Clear(atFlex)` restores the primitive default:
ordinary controls default to zero, and a spacer defaults to one. An indefinite
column's spacer has a natural 16-pixel height. On a weighted item,
allocation takes precedence over its main-axis width/height. Very small explicit
allocations can clip content; the widgetset never receives negative input bounds.

`Visible(False)` retains the mounted control and its owned descendants but
removes its flow slot, size, weight and adjacent gap. Showing it recomputes current
bounds, including a control initially mounted hidden. This applies to columns,
rows and grid track placement. An empty visible flow retains only its padding.
Hiding unrelated siblings or resizing retains an editable memo's real focus,
draft and selection. Hiding the focused control itself follows host focus rules.

Native allocations use cumulative integer rounding so all available pixels are
assigned. Browser CSS can retain fractional pixels; measured target bounds can
differ by one rounded pixel. The native adapter batches its geometry update before
LCL's automatic anchor pass. Authored widths are capped by actual parent content
width, matching the browser theme's `max-width`. Actual available space includes
platform scrollbar/client-area differences.

Browser positive weights opt into a zero minimum height and let a framed field's
editor fill its allocation. The Pascal zero-weight contract uses an automatic
basis; a bare CSS zero would incorrectly collapse an explicitly sized item.
These choices are informed by the [CSS flex basis and automatic minimum-size
rules](https://www.w3.org/TR/css-flexbox-1/), rather than an alternate browser
layout engine. The maintained adapters keep their ordinary host primitives.

The current policy packet covers natural caption widths, row wrapping/alignment,
shared spacers, authored-width height measurement and definite root propagation.
Border/client metrics, arbitrary mixed intrinsic constraints, flexible shrinking,
baseline/reverse-line alignment, typography/scaling, all widgetsets and complete
accessibility still belong to the original renderer/parity criteria. It does not
claim universal CSS layout implementation or complete target parity, and does
not lower published support grades to avoid remaining outcomes.

## Semantic review and reproduction

The maintained [operations fixture](../tests/layout-review.operations.json)
describes two additive pages in one 50-operation transaction. `layout-review`
contains fixed/weighted siblings, nested
columns/rows, a memo, literal list, split panes, hidden natural siblings and grid.
`policy-review` adds intrinsic actions, a wrapped row, logical alignment,
authored-width text measurement, an implicit spacer and a root-filling editor.
JSON here is an explicit MCP wire boundary; metrics/weights are numbers and
visibility is Boolean. Inspect current roots and revision before composing it.
Apply its 50 operations in one `nyx_transaction` with current `expectedRevision`
and a unique `operationId`. Existing IDs refuse; never replace a user's project.
Keep selection unchanged unless the user wants to activate the review.

Use bounded `nyx_outline`, `nyx_node` and 80-line `nyx_source` windows. Request
actual browser and LCL application jobs through `nyx_build`, inspect their
terminal status and compare their compiled source bytes with the unchanged
export. Selective preview validates appearance; physical consumers qualify
geometry, resizing, editor focus and collection/split descendants.

The Pascal [semantic review client](../tests/nyx_mcp_layout_review.lpr) accepts
local MCP configuration, this operations file, an owned export directory and
`compose` or `inspect`. Inspect does not mutate the design and still checks both
application compilers. Job receipts remain on disk if polling times out. It never
automatically retries mutations, replaces a project or prints configuration.
Native named MCP handles remain the primary interactive workflow.

After exporting, `tools/build.ps1 -Target layout -LayoutSourceDirectory <export>`
builds the semantic client and runs the checked native control consumer, then
builds its browser companion. Serve `layout.html` through the owned Pascal
service; the actual consumer must publish `data-layout-tests="passed"`.
`layout.html?host=1` executes the same consumer in an actual 390-pixel viewport.
The [shared fixture](../tests/nyx_layout_controls_tests.lpr) includes fixed known
allocations, overflow boundaries, pixel conservation, visibility transitions,
host resizing, actual memo selection and retained collection/split faces.

For the complete typed-policy consumer, use `-Target layout-policy` and supply
the directory containing the unchanged semantic export with
`-LayoutSourceDirectory <export>`. That target requires the policy root, exercises
the public value/managed/source/persistence contract and actual control geometry,
and refuses missing prerequisites. `layout.html?host=1` supplies an actual
390-pixel viewport. The semantic client additionally inspects bounded enum
metadata and rejects wrong choices/types at the same revision/history.

The current evidence is in [WORK.md](../WORK.md). Build/capture logs and private
accepted source remain under ignored output. User-facing editor defaults remain
free of review names; this page is an owned demo root, not editor chrome. Cleanup
uses reviewed `nyx_roots` at the current revision, preserving all other roots.

Native large views retain complete logical content behind safe physical geometry.
`Reveal(ID, Identity)` navigates to an exact mounted face; `ViewViewport` observes
its containing extent/offset and `ScrollView(X, Y)` restores logical-pixel
position. `Select` paints an outline without changing scroll/focus. Studio's
explicit hierarchy navigation consumes this separation. All original controls
remain owned and retain input while offscreen; projection does not create an
Undo entry or modify source. The native implementation reuses standard LCL bars.

`tools/build.ps1 -Target native-studio -VerifyLogicalViewport` qualifies checked
geometry and actual 2048-control/native nested input, resize, scrolling, events
and retirement, while compiling its browser companion. See WORK.md for current
evidence and the browser execution gate. Giant native inputs/custom faces and
large split panes explicitly require logical adapters. Non-panel client offsets,
logical resize events, widgetset/DPI metrics and complete parity remain under
the original renderer owners; this API does not accept those untested outcomes.

Native measurement can be inspected with the optional `NYX_LCL_LAYOUT_PROFILE`
compiler definition. It logs control counts, width/height traversals, row plans
and elapsed milliseconds, with no authored text or retained document references.
Ordinary builds contain neither those counters nor measurement clocks.

`TNyxStrings.IndexOfName` compares exact name prefixes in their original storage
units, without creating a name substring for every candidate property. UTF-8
bytes natively and UTF-16 units in the browser preserve case, supplementary text,
embedded NUL, duplicate ordering and the existing empty-name behavior. There is
no persistent property/measurement cache to invalidate after edits or resizes.

To reproduce the checked native Studio workload, first export the maintained
manual-presentation companion through semantic MCP, then run
`tools/build.ps1 -Target native-measurement -ResponsiveSourceDirectory <export>`.
The target exercises the unchanged ordinary Inspector/worker/history journey,
runs exact text/ownership fixtures with both native compilers and stages their
browser counterpart. Execute `text-lookup.html` through an admitted HTTP host;
compilation alone does not qualify browser behavior. The current allocation
comparison and actual target/editor evidence are recorded in
[WORK.md](../WORK.md#allocation-free-property-lookup--2026-10-06). Heap-traced
allocation counts are cumulative; they are not peak memory or release latency.
