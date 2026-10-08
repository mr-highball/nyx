# Typed clock fields

[Components](components.md) · [Contracts](contracts.md) · [Calendar fields](date-fields.md) ·
[Executed control evidence](../WORK.md#current-return-path-clock-control-consumers--2026-10-08)

The [portable clock prerequisite](../TODO/DONE/NS-1_time-values_01.md) is accepted
from checked native and executed browser value/domain/authoring suites and exact
emitted-source reconstruction. This establishes its typed contract; physical
picker, hardware/IME, accessibility and complete Studio/parity requirements retain
their own consumer evidence. The [acceptance packet](../WORK.md#current-return-path-executed-clock-prerequisite--2026-10-08)
records the unchanged 1,613 shared assertions and twelve intended type refusals.

`nyx.times` supplies immutable local clock readings. `NyxTime(Hour, Minute,
Second, Millisecond)` accepts hours 0..23, minutes/seconds 0..59 and milliseconds
0..999. Seconds and milliseconds are optional. `NyxNoTime` means an empty field;
defined midnight is `NyxTime(0, 0)`. Neither construct reads the system clock.

Normal authoring uses the specialized managed `INyxTime` from `nyx.controls`:

```pascal
var
  LMeetingTime: INyxTime;
begin
  LMeetingTime := NewNyxTime('meeting-time');
  LPage.Add(LMeetingTime);
  LMeetingTime.WithText('Meeting time').WithTime(NyxTime(9, 30));
  LMeetingTime.Contract.Value(NyxTimeDomain
    .Range(NyxTime(8, 0), NyxTime(18, 0))
    .StepSeconds(900));
end;
```

The page owns the node; the interface retains its managed control lifetime.
`TimeValue` reads/writes the typed value, `WithTime` returns the specialized
interface, and `Configure.Value(NyxTime(...))` supports the common fluent facade.
Malformed imported value text raises `ENyxTimeValue` when read as a clock.

## Exact values and precision

Clock arithmetic compares integer milliseconds since midnight. Text precision
is retained separately: `08:30`, `08:30:00` and `08:30:00.000` compare equal,
but persistence and generated code preserve each spelling. The explicit
persistence/control boundary `TNyxClockTime.FromText` accepts empty text,
`HH:MM`, `HH:MM:SS`, and one to three fractional second digits. It refuses
whitespace, AM/PM, non-ASCII digits, dates, time zones and leap seconds.
`TryNyxTime` returns `False` and an empty result for malformed input.

```pascal
LMeetingTime.WithTime(NyxTime(9, 30, 0, 100).WithPrecision(ntpTenth));
LMeetingTime.WithTime(NyxTime(9, 30).WithPrecision(ntpSecond));
LMeetingTime.WithTime(NyxNoTime);
```

`TNyxTimePrecision` provides minute, second, tenth, hundredth and millisecond
choices. `WithPrecision` returns an independent value and refuses any change
that discards a nonzero part. Empty values have no parts, precision or ordering;
those reads refuse, while `ToText` returns empty text.

## Fluent domains

`NyxTimeDomain` is a distinct builder. Its bounds and choices require
`TNyxClockTime`; step arguments require integers. It preserves the existing
text wire family while adding clock validation. Runtime stores remain independent
of the document's defaults, and ordinary text domains keep their original meaning.

| Construct | Meaning |
| --- | --- |
| `Range(minimum, maximum)` | Inclusive bounds; a reversed pair spans midnight; equal bounds admit one clock reading |
| `Minimum(value)` / `Maximum(value)` | One inclusive bound |
| `Choices([...])` | 1..128 distinct clock readings, including `NyxNoTime` if empty is an allowed choice |
| `StepMilliseconds(value)` | Positive exact integer step |
| `StepSeconds(value)` | Positive integer seconds, checked before conversion to milliseconds |
| `AnyStep` | Explicit unrestricted step; an omitted declaration is also unrestricted |

An overnight appointment can declare:

```pascal
LMeetingTime.Contract.Value(NyxTimeDomain
  .Range(NyxTime(22, 0), NyxTime(2, 0))
  .StepMilliseconds(1500));
```

The step base is the declared minimum, otherwise midnight. Admission checks the
exact signed millisecond difference from that base. Empty values bypass range
and step checks, but must appear in a declared choice list. Choice comparison
uses the reading rather than its precision: two spellings of the same reading
are duplicate choices. Constraints compose; every choice must satisfy its bounds
and step. Invalid candidate descriptors fail before replacing accepted metadata.

An explicit base overload accepts `TNyxValueDomain`; use a builder's `.Definition`
when intentionally enriching an existing text specification. Calendar, numeric
and other wrong-family bases refuse. Native clock values remain independent of
locale and floating-point timestamps.

## Reusable constraint editor and Studio

`nyx.times.editor` exposes `NewNyxTimeDomainEditor`, an independently owned Nyx
card composed from specialized time, select, input, memo, label and button
interfaces. It borrows the authored contract only during construction and retains
a copied local/effective baseline. Applications and Studio can use the same form:

```pascal
LTimePolicy := NewNyxTimeDomainEditor('meeting-policy', NyxControl('meeting-time'),
  LMeetingTime.Node.Contract, NyxNodeValueDomain(LMeetingTime.Node));
LSettingsPage.Add(LTimePolicy);
```

The policy form offers independently optional earliest/latest times, a closed
step selector and allowed times, one per line. Use `(empty)` for an optional empty
value in a restricted choice list. Bounds and choices retain minute, second or
fractional precision. A latest time before the earliest time spans midnight.
`TNyxTimeDomainEditorStep` distinguishes no declaration, explicit Any and fixed
integer milliseconds. The fixed-step proposal uses an ordinary text input so a
native numeric widget cannot round a fractional or overflow draft before Apply.

`NyxTimeDomainEditorFieldID` addresses named descendants through the closed
`TNyxTimeDomainEditorField` enum. `CaptureNyxTimeDomainEditor` recognizes only
the exact mounted Apply/Restore buttons and returns a copied
`TNyxTimeDomainEditorChange`. Unrelated actions return `False`; stale/invalid
fields refuse before publication. The receiver must recheck the owner and
baseline before admitting the proposed domain. Restore is disabled when no local
declaration exists. Neither construction nor capture mutates the authored owner.

Studio's ordinary Properties panel consumes this public form for effective
clock domains, including inherited compound fields. Apply/Restore enters the
existing independent paired source queue with the exact selection and baseline.
Successful admission updates design and adjacent typed Pascal as one Undo step.
Invalid steps and dependent defaults refuse atomically. Ordinary Win32 Studio
now retains unsubmitted form values through repaint, Properties/Events changes
and compact viewport allocation. A refused Apply leaves incomplete step/choice
text available for correction. Corrected Apply updates the retained Pascal pane
when displayed, and ordinary Undo/Redo restores the exact accepted pairs.

`TNyxTimeDomainEditorDraft` is the public reusable input snapshot used by both
Studio hosts. Initialize it with `Default(TNyxTimeDomainEditorDraft)`, capture
the old disposable form before replacement, and restore onto its freshly
composed replacement before rendering:

```pascal
// Capture borrows this root only during the call; the draft owns copied text.
LTimeDraft.Capture('time-policy', LRenderedFormRoot);
LTimePolicy := NewNyxTimeDomainEditor('time-policy', NyxControl('meeting-time'),
  LMeetingTime.Node.Contract, LEffectiveClockDomain);
LTimeDraft.Restore(LTimePolicy.Node);
```

The exact editor, owner, local/effective baseline and all five field kinds must
match. A changed context retires the draft before any write. An absent form parks
it across panel changes; explicit `Clear` retires it on project replacement.
Copies outlive their original controls and preserve independent Unicode text.
The snapshot captures the clock fields' admitted values and raw step/choice
proposals. Opaque/incomplete physical clock/picker buffers, caret/focus continuity
and browser reload recovery are separate concerns; the draft is ephemeral and
never enters exported designs or Undo history. Full observing browser execution
and authenticated current-backend authoring retain their existing gates.

`NyxSetValueDomain(NyxControl('meeting-time'), NyxTimeDomain...)` has a specialized
typed overload. The MCP persistence boundary advertises a closed `format: time`
domain with independently optional `min`/`max`, exact positive signed-Integer
millisecond `step` or explicit `any`, and 1..128 choices. Pascal admission checks
clock syntax and duplicate readings; JSON spelling uniqueness alone is insufficient.
The existing `nyx_node` value-domain window reports format, independent bounds,
paged exact choices, `stepDeclared`, native JSON `step`, `stepMilliseconds`,
`stepBase` and `crossesMidnight`. Absent step is null, distinct from explicit Any.
These are source/local-session capabilities; a frozen running backend gains them
only after independently qualified replacement. The current LAN backend is unchanged.

`tools/build.ps1 -Target time-policy` consumes the existing exact public-Pascal
clock companion without repeating foundation/picker tests. It runs offline MCP
schema checks, local semantic admission/history, physical native Inspector/queue
input and exact compiled reconstruction of the emitted accepted pair. It stages
browser controls, their source worker and reconstruction hosts at
`build/time-policy/maintained/web/`. No listener, browser, enrollment or observing
project is created or replaced. Browser staging is not execution evidence.

## Generated source and current limits

Generated Pascal uses specialized `INyxTime` references, `NewNyxTime`, typed
`Configure.Value`, precision enums and fluent `NyxTimeDomain` constraints.
The bounded source reader reconstructs those declarations, including reusable
part overrides. Imports with deliberate descriptor order or other admitted wire
details retain an explicit `Metadata` block when canonical fluent declarations
would change their exact representation. Invalid source candidates retain the
accepted document/source pair; one Undo restores an accepted grouped change.

Intrinsic time controls and legacy text contracts on time controls gain clock
validation. Invalid legacy time strings are diagnosed rather than coerced.
The browser adapter projects typed bounds and converts millisecond steps to exact
decimal seconds; its default is `step="any"`. The ordinary native renderer now
uses an LCL grouped clock field with an owned picker. Its real inner editor keeps
focus, keyboard and grouped forwarding; native clock drafts wait for editing
completion before shared admission. An invalid draft is diagnosed and the accepted
reading restored, without a successful change callback.

The native picker supplies separate hour/minute/second/millisecond parts, Clear,
Cancel and Use time. Parts compose native edits and native arrow controls. They
retain empty, fractional or otherwise invalid drafts across focus loss; values
must be whole ASCII digits within the part's bounds. Arrows and Up/Down step only
valid integer parts and clamp rather than round or wrap. Domain rejection leaves
the picker open with a diagnostic. Existing precision is retained when lossless;
new nonzero seconds/milliseconds increase it when necessary. Opening an empty
field proposes midnight, a minimum or the first nonempty choice, and never changes
the field until explicit acceptance. Escape cancels and returns focus; Enter uses
the same admission as Use time. Clear remains subject to declared choices.

Accepted Use time, Enter and Clear return to their exact clock editor after value
admission. The shared native weak focus-return helper preserves a deliberate
application redirect, distinguishing it from the host's automatic restoration
when a popup closes. Native focus observations share logical LCL Enter/Exit's
transition baseline, so the returning callback sees the committed state once.
Destroyed/disconnected/unfocusable targets refuse; change or focus callbacks may
retire the view safely. The maintained Win32 clock consumer passes 85 checks,
leak-free, including these paths. Its actual HTTP browser counterpart passes 22
checks at desktop and CSS-390, including bound-state completion, ordered
callbacks, retained draft/focus identity, exact range/step/choice refusal,
empty versus midnight, inherited read-only/disabled policy and retirement.

Each field owns its popup. Domain changes and inherited hidden/read-only/disabled
policy revoke the old popup context. Retirement disconnects borrowed callbacks
before freeing controls; an accepted callback may retire its own view safely.
The native capability is Basic support. Current evidence qualifies Win32 control
drafts, retained keyboard/arrow action routes, millisecond admission and lifetime;
printed images are diagnostics, not displayed-pixel or hardware evidence.
Browser picker chrome, other widgetsets/DPI, hardware/IME/accessibility,
production styling and full authenticated/observing Studio/MCP domain authoring
retain their original task owners. Compiling the adapters does not qualify those
behaviors; the local/native policy form evidence is described above.

`tools/build.ps1 -Target time-values` executes checked native contract and exact
compiled-companion reconstruction, checks six wrong argument families on both
compilers, and stages the shared pas2js fixtures with their matched runtime.
Execute `time-values.html` and `time-reconstruction.html` over an admitted HTTP
host to establish browser evidence. The build command starts no browser or service.

`tools/build.ps1 -Target time-fields` includes that same contract/reconstruction
qualification, executes `tests/nyx_time_controls.lpr` against its exact compiled
public-Pascal companion through the ordinary LCL renderer, and stages the matching
pas2js control consumer at `build/time-fields/maintained/web/time-controls.html`.
It neither edits an active MCP design nor starts a browser/service. Execute the
staged host through an admitted HTTP path to establish browser control evidence.

Current consumer qualification (2026-10-08) executes those controls and the
public clock Inspector through an admitted static HTTP host. The Inspector's
browser source worker passes 35 checks at both widths, including atomic paired
Apply/Restore/Undo/Redo and explicit queue/view/host retirement. The complete
ordinary native Studio journey passes 51 checks, including the public form's
34 assertions; these counts overlap. The accepted policy's exact compiled
reconstruction passes five assertions on both targets. Full ordinary browser
Studio and authenticated current-backend observation remain open.

Formatted clock input is a physical draft until editing completion. Browser
qualification sends separate `input` and bubbling `change` notifications;
unrelated Sync retains a draft, and refused completion restores the accepted
reading without a successful callback. This follows the distinction described
in the [HTML common event behavior](https://html.spec.whatwg.org/multipage/input.html#common-event-behaviors)
and [Time state](https://html.spec.whatwg.org/multipage/input.html#time-state-(type=time)),
checked 2026-10-08. Authored wire precision remains separate from the browser's
localized visible clock representation.

The maintained browser control/policy programs use the existing Pascal
`nyx_browser_ready_capture` observer: checkpoint acknowledgment separates
navigation, meaningful mounted captures and explicit disposal. Their fixture
hosts require that observation protocol to publish a terminal pass; loading
the HTML alone does not establish completed qualification. Captures and DOM
markers are retained before the controls retire. These synthetic handler routes
and CSS viewport checks do not establish trusted hardware or accessibility.
