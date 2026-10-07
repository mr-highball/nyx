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

The existing date-range recipe remains two customizable named date parts,
`start` and `finish`, separated by a label. This batch adds actual field picking,
not automatic cross-field ordering, a richer range-selection calendar or a custom
accessible dialog. The [WAI date-picker example](https://www.w3.org/WAI/ARIA/apg/patterns/dialog-modal/examples/datepicker-dialog/)
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
