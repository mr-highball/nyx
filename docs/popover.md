# Managed contextual views

[Managed controls](managed-controls.md) · [Confirmation](confirmation.md) ·
[Component contracts](contracts.md) · [Current evidence](../WORK.md)

`INyxPopover` presents an independently owned Nyx page or reusable root beside an
invoker. Browser and LCL factories supply the physical host; application code uses
one portable managed interface. Studio's **About this component** action consumes
`NewNyxComponentHelp`, a public specialized Nyx card with creator descriptions,
capabilities and an ordinary Close button.

## Authoring and ownership

Each successful opening also exposes an optional `INyxPopoverOpenAdmission`.
Its `GetOpenLifetime` returns an `INyxPopoverOpenLifetime` for that exact opening;
it returns nil while closed, transitioning or retired. Capture the token before
deferring work. `Current` becomes permanently false on silent Close, dismissal,
retirement or closure of the original parent presentation. Reopening produces a
different token, so a later `IsOpen = True` cannot revive an earlier action.

Tokens own no presenter, document, event router or native/DOM control. Their
readonly cancellation state supports native worker reads; presentation methods
and token capture remain UI operations. Nested tokens retain the original
parent's token, invalidating the captured family before descendant cleanup can
call application code. A failed Present publishes no token, attempts cleanup and
preserves the original failure if cleanup also fails. The optional facets leave
the original `INyxPopover` GUID and vtable unchanged.

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

Browser placement uses the visible part of an embedded application, including
same-origin frame ancestors, their visual viewports and independent horizontal/
vertical overflow clips. Frame borders, padding, scrolling and positive
axis-aligned scale are converted into the application's own client coordinates.
The existing layout tick samples these borrowed observations without retaining
parent windows or installing additional parent listeners. Oversized content
scrolls within the reachable popup allocation. Hidden, detached or wholly clipped
anchors dismiss with `nprAnchorUnavailable`; this includes the anchor's own scroll
container, while the popup can still escape that container through the top layer.

Cross-origin parent geometry is unavailable at the browser's `frameElement`
boundary. Rotation, perspective and nonrectangular clipping are outside this
rectangle contract. These limits do not imply screen-coordinate placement or a
guarantee of visibility through an inaccessible ancestor.

The maintained `popover-embedded.html` consumer exercises clipped, scrolled,
scaled, resized and nested frames plus anchor retirement. The ordinary and nested
popover regression pages accept `?run` to release their optional screenshot
checkpoint automatically while retaining every assertion. Their default URLs
still pause for a representative capture.

Focus is optional; omission keeps invoker focus. An unavailable named focus part
refuses before the host opens. An explicit part must have an enabled, visible
physical focus face. Escape respects consumed child keys and returns invoker
focus. Outside press preserves its new focus destination. Clicking the anchor is
excluded from outside dismissal. The independent nonmodal background stays
interactive.

Native delivered input uses the exact LCL control and its popup-family/anchor
ownership. Current cursor coordinates cannot override that delivered target;
overlapping windows or later pointer movement must not keep an outside menu open
or dismiss an inside press. Cross-application deactivation supplies no receiving
control and retains the guarded screen-coordinate fallback. Both paths borrow
their observations only for admission, before dismissal callbacks run.

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
patterns, general modal/picker coordination, assistive technology, hardware/IME input,
other widgetsets and per-monitor DPI remain open qualification work.

## Declared contextual children

`Within` gives both target presenters the same fluent family contract. Create the
child with an anchor inside its already open parent, then declare the relationship
before opening it:

```pascal
LChild.Within(LParent).Open(NyxPopover('More options')
  .Size(280, 360)
  .Focus(NyxPart('notes')));
```

`LChild` keeps its specialized browser/LCL interface; the fluent result is the
portable `INyxPopover`. Each presenter still owns an independent document,
runtime store and controls. The relation retains revocable lifetime leases rather
than either presenter. Keep ordinary interface owners while using the views.
An ignored interface result can live until the end of a compiler-generated scope;
use an explicit result owner and release it when testing earlier retirement.

Child presses belong to the declared ancestor family. Escape closes the popup
owning the focused control or invoker, leaving ancestors open and returning focus
to the exact invoking part. This also applies when browser hosts are physically
nested. A press back in the parent closes its child; a genuine outside press
closes the family without stealing the destination's focus.

Closing a parent immediately closes descendants with `nprAnchorUnavailable`,
including grandchildren, before hiding their anchors. Close remains silent for
the parent itself; children notify their own ordered completion subscribers.
Destruction silently retires the family. A closing/retired parent refuses child
reopening; parent loss during an actual focus callback also refuses a pending
Open. Completion callbacks may release later members safely.

Nesting changes require a closed stable child. `Within(nil)` detaches it. Cycles,
more than 64 ancestors, more than 256 direct children, unrelated anchors and
incompatible target adapters refuse before changing the accepted relation. A
parent's external background invoker is not nested content. Rejected Open does
not mark the child open. Physical closure attempts cover the remaining children
if a target raises; a closure refusal can therefore describe an already closed
parent. Inspect state rather than treating an exception as rollback.

Custom presenter adapters can inherit `TNyxPopoverPresenter`, implement physical
ancestry through `CanNestWithin` and use its revocable nesting seam. They must
call `RetireNesting` before retiring target hosts; default adapters do this.
Native `INyxPopover` and its browser/LCL specializations have version-two GUIDs
with this method addition. Rebuild native adapters/packages together; portable
document and resource wire versions are unchanged. Older native window-family
registration remains available, with its original ownership behavior.

The maintained `popover` build also runs `nyx_popover_nested_controls` and stages
`popover-nested.html`. Its exact MCP companion feeds both targets; runtime
presenter linkage remains explicit Pascal enrichment. The browser driver pauses
at `nested-popovers` to capture three live English views before checking closure
and retirement. Win32 displayed capture remains separately qualified from native
logical focus/control checks. General modal/picker coordination, hardware/IME,
assistive quality, other widgetsets and complete overlay acceptance remain open.

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
