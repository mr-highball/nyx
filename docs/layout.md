# Proportional and hidden-flow layouts

[Architecture](architecture.md) · [Capabilities](capabilities.md) ·
[LCL owner](../TODO/NS-2_lcl-renderer_01.md) ·
[Parity owner](../TODO/NS-2_parity-accessibility_01.md)

Use typed `Layout(nlColumn/nlRow/nlGrid/nlAbsolute)`, `Width`, `Height`, `Padding`,
`Gap`, `Flex` and `Visible` configuration. The document owns these values;
adapters consume them without making the portable tree depend on DOM/LCL types.

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
`Flex(0)` and `Clear(atFlex)` restore fixed/natural sizing. On a weighted item,
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

The native unspecified nonflex row-width policy still uses equal cells, with a
caption fallback to stacking. Browser natural text widths and narrow row wrapping
remain different in those cases. Border/client metrics, arbitrary mixed intrinsic
constraints, root height propagation, richer wrapping/alignment policies,
effective authored-width measurement of natural children,
typography/scaling and accessibility still belong to the original renderer/parity
criteria. This packet does not claim complete layout or target parity and does
not lower published support grades to avoid those remaining outcomes.

## Semantic review and reproduction

The maintained [operations fixture](../tests/layout-review.operations.json)
describes one additive `layout-review` page: fixed/weighted siblings, nested
columns/rows, a memo, literal list, split panes, hidden natural siblings and grid.
JSON here is an explicit MCP wire boundary; metrics/weights are numbers and
visibility is Boolean. Inspect current roots and revision before composing it.
Apply its 27 operations in one `nyx_transaction` with current `expectedRevision`
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

The current evidence is in [WORK.md](../WORK.md). Build/capture logs and private
accepted source remain under ignored output. User-facing editor defaults remain
free of review names; this page is an owned demo root, not editor chrome. Cleanup
uses reviewed `nyx_roots` at the current revision, preserving all other roots.
