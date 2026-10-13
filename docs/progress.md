# Task progress

[Components](components.md) · [Bindings](bindings.md) · [Current work](../WORK.md)

`INyxProgress` represents completion of a task. It retains an Integer value and
range. Use its typed unknown-total flag while the application cannot yet report
how much work remains:

```pascal
LProgress := NewNyxProgress('prepare-workspace').WithIndeterminate(True);
LProgress.Configure
  .AccessibleName('Workspace preparation')
  .Minimum(0).Maximum(100).Value(35)
  .Done;

LDocument.State.SetValue(NyxBooleanState('discovering-total'), True);
LDocument.State.SetValue(NyxIntegerState('completed-work'), 35);
LProgress.Binds
  .Indeterminate(NyxBooleanState('discovering-total'))
  .Value(NyxIntegerState('completed-work'), bdFromState)
  .Done;
```

Changing the Boolean state to False resumes the current numeric value; it does
not reset the task. Value updates during unknown work remain stored. Independent
reusable instances and their bindings retain independent presentation. The flag
defaults to False in old designs. `.Configure.Indeterminate(Boolean)` is the
generic typed authoring call; the specialized fluent method returns `INyxProgress`.
The binding also accepts typed Boolean resource values and remains read-only.
Wrong scalar kinds and two-way mode bindings refuse before publication.

Studio's ordinary Properties Inspector discovers a Boolean choice and both-target
help from the shared schema. Its binding picker exposes Boolean state for this
target. Authored/generated Pascal uses typed calls and reference names; source
admission rejects strings such as `'true'` in Boolean arguments. Persistence and
semantic grouped history retain the exact design/source pair. An unsupported
retained attribute on another standard control does not add binding support;
creator schemas can explicitly define their own supported projection.

The browser uses its real HTML `progress` element: unknown work removes the
`value` attribute and `aria-valuenow`; known work restores normalized geometry
with the actual semantic range/value in accessibility attributes. This follows
the [HTML progress contract](https://html.spec.whatwg.org/multipage/form-elements.html#the-progress-element)
and [ARIA progressbar semantics](https://www.w3.org/TR/wai-aria-1.2/#progressbar).
Unrelated refresh does not briefly add/remove a numeric value during unknown work.

Native `TNyxLCLProgress` reuses `TProgressBar` for its Integer range and position.
An inert LCL child paints actual activity because a widgetset/theme can accept
marquee mode while continuing to display a percentage. Its owned timer runs only
while unknown activity is shown, parks when hidden or known, and retires before
the native control. Palette fields are copied; no document, renderer or borrowed
theme survives in the timer. Known work retains ordinary widgetset painting.

The maintained `progress-activity` target consumes an exact MCP export supplied
with `-ProgressSourceDirectory`, and a new `-ProgressEvidenceDirectory`. It checks
both compilers' rejection of wrong public argument types, native live controls,
displayed animation/timer lifecycle and separately compiled generated source.
The matching browser page still requires HTTP execution. The companion tool's
`--progress` mode composes and builds its seed in an owned semantic review;
`--progress-observer` additionally qualifies the older installed backend's exact
new-binding refusal. Current-source enrichment remains explicit until rollout.

Win32/desktop and narrow browser evidence does not establish every widgetset,
DPI, reduced-motion/high-contrast setting or assistive-technology implementation.
Animation budgets and full production feedback-family quality remain open.

Studio consumes the same specialized progress component. Its source pane reads
the owning source queue's Busy state; status wording is never used to infer work.
Queue notifications update the retained activity face before deferred chrome
composition, leaving the independently owned Pascal editor intact. Invalid source
stops activity at refusal while retaining the accepted pair and unfinished draft.
Repeated notifications with the same typed working state do not synchronize the
pane again; `RestoreNyxStudioSourceActivity` reports an actual visibility change
to its target controller.

The Builds panel uses one aggregate indicator for queued, running or retiring
jobs, including successful constructors awaiting paired publication. Refused or
reviewed terminal publication stops its activity. Cancelling one job does not stop
the indicator while other jobs remain active. Counts do not imply a percentage;
exact job identity, diagnostic navigation and cancellation authority are retained.

The maintained `studio-progress` target takes `-ProgressSourceDirectory` containing
an exact semantic export and bounded real `build-jobs-running.json` /
`build-jobs-succeeded.json` receipts, plus a new `-StudioProgressEvidenceDirectory`.
It executes the public native source-worker consumer and complete ordinary native
editor workloads, then stages both browser consumers and the matching module
worker. Their HTTP execution is separate. The component review explicitly labels
recorded compiler metadata; its independently owned source queue executes live.
The ordinary browser compiler exchange qualifies controller behavior without
claiming a live HTTP compiler transport or an observing release update.
