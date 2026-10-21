# Typed calendar fields

[Components](components.md) · [Contracts](contracts.md) · [Clock fields](time-fields.md) ·
[Current evidence](../WORK.md#typed-date-fields-checkpoint--2026-10-06)

`nyx.dates` supplies immutable Gregorian calendar dates without DOM, LCL,
time-zone or floating timestamp dependencies. `NyxDate(Year, Month, Day)`
rejects impossible days; years are 1..9999. `NyxNoDate` means empty, never today.
Normal authoring uses integer parts and the managed specialized `INyxDate`.

```pascal
LArrivalDate := NewNyxDate('arrival');
LArrivalDate.WithText('Arrival').WithDate(NyxDate(2026, 10, 6));
LArrivalDate.Contract.Value(NyxDateDomain
  .Range(NyxDate(2026, 1, 1), NyxDate(2026, 12, 31)));
LPage.Add(LArrivalDate);
```

`DateValue`, `WithDate` and `Configure.Value` accept `TNyxCalendarDate`.
The inclusive domain bounds require two defined ascending dates. Empty remains
permitted independently of bounds. A typed `Choices` whitelist can exclude
empty, and all choices must fit the domain. Builders copy their admitted values;
failure does not mutate an existing declaration.

At explicit persistence/state/control boundaries, dates use exact ASCII
`YYYY-MM-DD` text, or empty. Invalid dates, whitespace, locale formats and
non-ASCII digits refuse. Date semantics are retained by the text domain's
persisted format. Legacy text declarations on date projections retain their
choices while acquiring calendar admission. Runtime stores remain independent
of document defaults; this does not introduce a separate date state kind.

The generator emits `NyxDate(...)`, `NyxNoDate`, typed ranges and typed choices.
Reusable value overrides resolve their actual named-part projection before
generation. Studio source admission understands these closed constructors and
domain builders; it never evaluates arbitrary Pascal or locale date expressions.
Wrongly typed/impossible source candidates leave the accepted document intact.

The browser uses the standard HTML date input, with domain bounds projected to
its min/max attributes and shared admission as the final authority. The visible
date follows the host locale while its value stays canonical. This follows the
[HTML date-state contract](https://html.spec.whatwg.org/multipage/input.html#date-state-(type=date)).
The default browser picker is owned by the browser; these fixtures do not qualify
its operating-system popup, trusted hardware input or assistive technology.

LCL uses an owned `TDateEdit` field and standard `TCalendar` popup. The renderer
keeps the real inner editor's grouped forwarding, keyboard, focus and editing
hooks. Partial native text remains a draft; editing completion admits or restores
it without locale coercion. Enter/Space accepts a month-view date; Escape cancels
and returns focus. Inclusive bounds, inherited availability and changed-domain
contexts are enforced. Hiding/read-only ancestry closes an open popup. Calendar
acceptance travels through the ordinary shared value/state/callback path.

Acceptance returns to the exact editor after shared admission. The native weak
focus-return helper distinguishes an application's deliberate redirect from the
host's automatic focus restoration when concealing the popup. Hidden, disabled,
disconnected or destroyed targets refuse restoration. The returning OnAfterEnter
callback observes the admitted value; it may retire the view. Escape retains its
separate cancellation path. Win32 calendar controls pass 94 maintained checks,
including exact native focus handles, one returning callback, explicit redirect
and change/focus callback retirement, with zero leaks. The 94 include 49 shared
date-contract checks. This is native evidence;
browser popup, other widgetsets and accessibility qualification remain separate.

Each native field owns its exact popup. Renderer retirement revokes callbacks
before disposing its controls; deferred physical retirement permits a change
callback to unmount its own view safely. Popups size to the installed calendar
widget and stay within the current monitor's work area.

The date-range recipe retains two customizable named date parts, `start` and
`finish`, separated by a label. Optional coordinated ordering is described below;
a richer range-selection calendar and custom accessible dialog remain open.
The [WAI date-picker example](https://www.w3.org/WAI/ARIA/apg/patterns/dialog-modal/examples/datepicker-dialog/)
is useful guidance for later custom presentations; this standard native tool
window does not claim to implement that modal-dialog example.

The maintained semantic recipe is
[date-field-review.operations.json](../tests/date-field-review.operations.json).
Compose it in an owned empty MCP review or project with exact expected revisions
and one transaction, then export bounded accepted-source windows. Current MCP
supports the typed policy operations below. The observing release qualifies these
through authenticated MCP and the full desktop/narrow Studio Inspector; see the
[observing packet](../WORK.md#current-return-path-test-mode-observing-release-refresh--2026-10-06).
The date-field physical fixture enriches the semantic seed
using public Pascal contracts and a runtime binding. It does not pretend that
the enriched design was admitted by the older running MCP service.

```powershell
./tools/build.ps1 -Target date-fields -DateSourceDirectory <exported-source-directory>
```

This target replays current generated typed source, runs checked native controls
and stages both browser consumers. It starts no listener. Execute the staged
consumers with the maintained anonymous-pipe readiness observer on an existing
admitted HTTP host. Current actual desktop/exact-390 controls pass 70 checks each;
compiled browser reconstruction passes and meaningful field captures are inspected.
Checked Win32 passes 87, managed-source regression 33, with zero native/observer
leaks; see WORK for exact artifacts and limits. Native capability remains Basic.
Other widgetsets, DPI matrices, IME, hardware/assistive input,
complete accessibility and advanced date-range behavior remain open.

## Coordinated date ranges

`nyx.date.range` adds copied, typed ordering policy to `INyxDateRange` without
changing the existing factory, interface identity or independent default recipe.
Use `Ordered` when both defined endpoints must satisfy start <= finish:

```pascal
LStayDateRange := NewNyxDateRange('stay');
SetNyxDateRangeOptions(LStayDateRange, TNyxDateRangeOptions.Ordered);
LPage.Add(LStayDateRange);
```

Equal dates are valid. Empty or one-sided ranges stay explicit; each Date's own
domain still decides whether empty is permitted and enforces bounds and choices.
Ordering never fills an empty endpoint, swaps dates, clamps a value or writes the
other field. A rejected edit leaves the accepted state and revision intact.
The native adapter restores the rejected field while preserving an unfinished
date draft in its peer. Runtime input leaves authored defaults unchanged.

`Independent` is an explicit local mask; `InheritNyxDateRangeOptions` removes
only that local policy and reveals the reusable definition's setting:

```pascal
SetNyxDateRangeOptions(LStayComponent, TNyxDateRangeOptions.Independent);
InheritNyxDateRangeOptions(LStayComponent);
```

The same setters support authored whole instances and named-part descriptors.
Ordinary admission resolves their actual target. An ordered range must retain
its own unambiguous direct `start` and `finish` Date parts. Policies on another
kind, malformed metadata and missing/replaced endpoints refuse. Pair validation
uses the complete bound candidate before publication, so simultaneous changes
and endpoints sharing a state key are assessed together. Unbound edits use the
same prepublication comparison. Raw defaults can be superseded by valid bindings.

Studio's Properties Inspector composes `NewNyxDateRangeEditor`, a reusable Nyx
Card/Button/Label form. Its actions are **Keep dates in order**, **Keep dates
independent** and **Use inherited ordering**. Captured changes retain their exact
authored owner and baseline;
the shared `NyxDateRangePolicyPatch` candidate publishes design and readable
Pascal in one Undo step. Stale selection, pending Pascal and changed baselines
refuse. Instance policy edits preserve customized parts and sibling instances.

Generated source uses specialized interfaces and
`SetNyxDateRangeOptions(..., TNyxDateRangeOptions.Ordered)`. Source admission
understands the two closed factories and set/inherit calls. Promoting older
accepted source adds the required `nyx.date.range` import through existing source
reconciliation; arbitrary Pascal is never evaluated by the builder reader.

```powershell
./tools/build.ps1 -Target date-range -DateRangeEvidenceDirectory <new-directory>
```

This maintained target runs the policy/editor and existing time/date regression
consumers, then the ordinary native Studio workflow at desktop and compact widths.
It compares the complete saved project and Pascal bytes, compiles that exact saved
unit and exercises its application independently at both widths. It also stages
the browser consumers and exact generated companion with the matched RTL.

Current local evidence is 70 policy/editor checks, 1,613 maintained time checks,
49 maintained date checks, ordinary Studio/application 82 at 1280 and 90 at 390,
and exact saved compiled application 19/19; all native runs have clean heaps.
The input is synthesized through actual native controls. Browser consumers
compile only in this packet. Browser execution, trusted hardware/assistive input,
other widgetsets/DPI, production performance and richer range pickers remain open.
See [the work record](../WORK.md#coordinated-date-range-checkpoint--2026-10-21).

## Studio and semantic constraint authoring

The ordinary Properties Inspector composes the public
`NewNyxDateDomainEditor` from specialized Nyx Card, Date, Memo, Button and Label
interfaces. It appears when the selected authored control or named-part override
has an effective calendar domain. Bounds are inclusive; leave both empty for any
date. Allowed dates use one canonical date per line; `(empty)` explicitly permits
no date in a restricted list. Leave the list blank for unrestricted choices.

Editing these controls changes only the mounted draft. Apply captures the exact
owner and copied local/effective baseline, then uses Studio's ordinary isolated
source queue to admit one paired design/Pascal Undo step. Invalid dates, duplicate
choices, incomplete bounds, stale selection or a changed baseline refuse. A
domain excluding an authored value or dependent state default also refuses;
semantic callers can group dependent default changes with policy changes.
Restore removes only the local value declaration and reveals inherited policy.
It preserves named fields/events and is disabled when no local declaration exists.
This differs from `NoValue`, which explicitly masks an inherited value contract.

The typed shared command is usable without the editor:

```pascal
LSession.ApplyPatch(NyxValueDomainPatch([
  NyxSetValueDomain(NyxControl('arrival'), NyxDateDomain
    .Range(NyxDate(2026, 10, 1), NyxDate(2026, 10, 31))
    .Choices([NyxDate(2026, 10, 6), NyxDate(2026, 10, 9), NyxNoDate]))]));
```

Use `nyx.studio.edits` for copied set/inherit commands and `nyx.dates.editor`
for the reusable public editor. Other scalar domain families have specialized
command overloads; the date editor deliberately handles calendar constraints.
`Contract.InheritValue` removes just the local declaration. Generated source uses
typed `NyxDateDomain.Range(...).Choices(...)`, rather than string property writes.
Unchanged legacy accepted source retains its spelling during incremental edits;
fresh generation emits typed date constructors throughout.

Current-source MCP exposes opt-in `nyx_node` value-domain inspection and closed
`value-domain-set` / `value-domain-inherit` operations in ordinary grouped
transactions. See [the semantic guide](studio-agents.md#value-domains).
The protected running endpoint retains its earlier schema; offline discovery,
native controls and real browser workers do not establish an observing rollout.

```powershell
./tools/build.ps1 -Target date-policy -DatePolicySourceDirectory <MCP-exported-source-directory>
```

This target consumes the unchanged English semantic date seed, exercises actual
native Studio controls and compiles/executes the newly accepted exact companion.
It stages the browser consumer, real Pascal source worker and reconstruction
with the matched RTL. Run the browser consumers on an existing admitted HTTP
host; this target starts no listener and does not change a user project.
