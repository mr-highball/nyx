# Managed contextual views

[Managed controls](managed-controls.md) · [Confirmation](confirmation.md) ·
[Component contracts](contracts.md) · [Current evidence](../WORK.md)

`INyxPopover` presents an independently owned Nyx page or reusable root beside an
invoker. Browser and LCL factories supply the physical host; application code uses
one portable managed interface. Studio's **About this component** action consumes
`NewNyxComponentHelp`, a public specialized Nyx card with creator descriptions,
capabilities and an ordinary Close button.

## Authoring and ownership

Build the content with specialized controls and named parts:

```pascal
LDocument := TNyxDocument.Create;
LQuickNotes := NewNyxCard('quick-notes');
LDocument.AddPage(LQuickNotes);
LQuickNotes.Configure.Layout(nlColumn).Padding(20).Gap(12).Compound(True).Done;

LNotesMemo := NewNyxMemo('notes-memo');
LNotesMemo.Text := 'Your note';
LNotesMemo.Placeholder := 'An idea worth keeping';
LNotesMemo.Configure.PartName(NyxPart('notes')).Done;
LQuickNotes.Add(LNotesMemo);

LDoneButton := NewNyxButton('notes-done');
LDoneButton.Configure.Text('Done').PartName(NyxPart('done'))
  .OnClick(NyxSemantic(nseDismiss)).Done;
LQuickNotes.Add(LDoneButton);
```

The host factories take a physical anchor from the application's target renderer
and an exact typed root reference:

```pascal
// Browser host boundary: LInvoker is an INyxButton in the background renderer.
LPopover := NewNyxBrowserPopover(LRenderer.FocusFor(LInvoker.ID),
  LDocument, NyxPageRoot(LQuickNotes.ID));

// Native host boundary uses the same portable document/root contract.
LPopover := NewNyxLCLPopover(LRenderer.FocusFor(LInvoker.ID),
  LDocument, NyxPageRoot(LQuickNotes.ID));
```

Each factory copies the complete document, including defaults, reusable
definitions and collection declarations. The original can be freed immediately.
Content remains specialized through its retained interface; configure it before
the first successful Open. Mounted content then stays stable across Close/Open,
retaining controls, unbound drafts, bindings and subscriptions. This differs from
a new presentation instance, which starts with independent stores and content.
A retained content handle can outlive its presenter. The exposed State and target
renderer/window/element are borrowed; never free them or use them past their owner.
An optional supplied theme must outlive the presenter.

## Typed geometry, focus and completion

```pascal
LPopover.OnDismiss.Policy(neSequential).Subscribe(LCompletion);
LPopover.Open(NyxPopover('Quick notes')
  .Placement(npsBelow, npaEnd)
  .Size(360, 400)
  .Sizing(npzContent)
  .Spacing(8, 12)
  .Focus(NyxPart('notes'))
  .DismissOn([npdEscape, npdOutsidePress]));
```

Options are immutable copied values. Size bounds logical pixel allocation:
16..16384 per axis; spacing is 0..4096. Content sizing follows the rendered root
height up to the height cap, including browser host borders. Fixed sizing requests
the complete height. Both are capped by the available viewport/work area. Shared
placement flips to the opposite side when it offers more room and clamps to the
viewport margin. Alignment follows the physical anchor axis; logical RTL
alignment and broader DPI qualification remain open.

Focus is optional; omission keeps invoker focus. An unavailable named focus part
refuses before the host opens. An explicit part must have an enabled, visible
physical focus face. Escape respects consumed child keys and returns invoker
focus. Outside press preserves its new focus destination. Clicking the anchor is
excluded from outside dismissal. The independent nonmodal background stays
interactive.

Close is silent and idempotent. Dismiss closes first, then invokes multiple
completion registrations in order. Callbacks may reopen or release the presenter.
Use `NyxPopoverDismissReason(AEvent)` for the typed reason from the owned event
snapshot: it survives queues, reopening and retirement. LastReason describes the
presenter's current state, which a later Open resets. The reason's numeric field
is an internal structured-payload boundary; application authoring uses the enum
and helper. Ordinary Events exposes all mounted Nyx control callbacks.

Presentation methods require the UI thread. Threaded callbacks must queue UI work
through the scheduler. A callback must not strongly retain its own presenter.
Open/Close/Dismiss during an unfinished focus transition refuse, preventing
recursive half-open hosts. An anchor that detaches, hides or disables dismisses
with `nprAnchorUnavailable`; reopening refuses until it becomes available.
Adapters track anchor geometry on a 100ms UI timer while open. They disconnect
timers/input and unmount renderers before destroying owned stores/documents.

The browser uses the standard [HTML manual popover top layer](https://html.spec.whatwg.org/multipage/popover.html),
a nonmodal dialog label and the Nyx view's own theme. The native adapter uses a
standard borderless LCL form and ordinary Nyx-rendered LCL controls, weak component
notifications and application after-key/input hooks. Menu/picker-specific keyboard
patterns, nested overlay coordination, assistive technology, hardware/IME input,
other widgetsets and per-monitor DPI remain open qualification work.

## Semantic companion and reproducible checks

The maintained Pascal client composes an English **Quick notes** card inside one
connection-owned temporary MCP review. It uses one revision-aware grouped edit,
bounded 80-line source reads and paired Undo/Redo, exports the exact canonical LF
companion, then retires the review without changing the primary project.
Explicitly enroll the configuration first:

```powershell
& tools/build.ps1 -Target popover-companion -DesignerMCPConfig .codex/config.toml
& tools/build.ps1 -Target popover
```

The second command builds/runs native popover and full Studio help consumers,
compiles the same companion for the browser, and stages full browser Studio with
its real Pascal source worker plus a Pascal host-input observer. It starts no
listener and performs no editor mutation. Serve its browser directory on an
existing admitted Pascal HTTP host, then run the existing real-clock ready
capture for `popover.html` / `data-popover` and
`nyx_studio_help_observer` for `index.html` at CSS 1100 and 390.
Run full editor observers sequentially on a constrained host.

For an already enrolled observing server, the help observer accepts two optional
arguments after URL, evidence directory and width: repository and exact ordinary
workspace. The URL must carry that same `?workspace=` handle. It uses bounded
`nyx_session`/`nyx_node` queries, waits for the shared title/selection before host
input, and verifies unchanged revision/navigation/draft/history availability.
Use an existing explicitly owned workspace whose selected component exposes help;
this read-only journey neither creates nor replaces its project. Full durable
pair/history preservation belongs to the release owner, not UI availability flags.

Managed presentation declarations and primitive semantic action assignment are
not yet exposed by MCP. The fixture attaches typed runtime dismissal through the
public Pascal API; its exported companion stays unchanged. This is explicit
runtime enrichment, not semantic presentation admission. That gap stays with
[the workflow owner](../TODO/NS-4_agent-workflows_01.md). Scalar default cloning
and unbound draft retention are exercised here; live scalar/collection binding
inside popovers needs additional actual-control qualification.
