# Typed clock fields

[Components](components.md) · [Contracts](contracts.md) · [Calendar fields](date-fields.md) ·
[Current evidence](../WORK.md#current-return-path-typed-time-field-prerequisite--2026-10-07)

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
| `AnyStep` | No step restriction; the default |

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
decimal seconds; its default is `step="any"`. Native projection remains the
existing text field. Browser execution, native pickers, physical input,
accessibility and Studio/MCP domain authoring remain open with their original
task owners. Compiling the adapters does not qualify those behaviors.

`tools/build.ps1 -Target time-values` executes checked native contract and exact
compiled-companion reconstruction, checks six wrong argument families on both
compilers, and stages the shared pas2js fixtures with their matched runtime.
Execute `time-values.html` and `time-reconstruction.html` over an admitted HTTP
host to establish browser evidence. The build command starts no browser or service.
