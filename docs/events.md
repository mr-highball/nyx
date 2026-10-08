# Typed events and portable commands

[Fluent API](fluent-api.md) · [State](state.md) · [Bindings](bindings.md) ·
[Compound components](components.md)

[Compound value/payload declarations](contracts.md) define exact scalar families,
named field payloads, ranges and choices through the public fluent API.

Both renderers expose the managed multiple-registration router described below,
and retain the synchronous `TNyxEventHandler` bridge from `nyx.behavior`:

```pascal
procedure TReplyPage.HandleEvent(ANode: TNyxNode; const AEvent: TNyxEventInfo);
begin

  if AEvent.IsNamed(FReplyChanged) and AEvent.HasValue then
  begin
    FLastReply := AEvent.Value.AsText;
  end;
end;
```

`FReplyChanged` is a `TNyxEventRef`, initialized once with
`NyxEvent('reply.changed')`. Configure the memo with the same reference:

```pascal
LReplyMemo.Configure
  .Text('Reply')
  .OnChange(FReplyChanged)
  .Done;
```

Application event names are open data in a distinct reference family. A part or
component reference cannot serve as an event reference. Closed physical triggers
use `TNyxTrigger`: command, focus, keyboard, text, pointer, wheel and viewport
families for runtime controls,
`ntDesignSelect` and `ntDesignValue` for the designer. An event named `select`
from a segmented control still has trigger `ntClick`; Studio selection uses
`ntDesignSelect`. Handlers can compare names with `IsNamed` and triggers with
ordinary enum comparisons, without interpreting behavioral strings.
An explicitly empty event name suppresses the semantic callback while accepted
state updates and portable actions still run.

Portable actions use `TNyxAction`: none, clear, dismiss, toggle, select, increment
and decrement. Fluent configuration supplies the enum and a typed part reference:

```pascal
LIncrementButton.Configure
  .Action(naIncrement)
  .Target(NyxPart('value'))
  .OnClick(NyxEvent('quantity.increased'))
  .Done;
```

The interpreter decodes the version-1 action spelling once at its persistence
boundary. Unknown actions fail before mutation. Step values and ranges require
complete bounded integers; malformed drafts cannot silently become zero.
Read-only value targets are refused. Routing chooses the nearest compound while
checking enabled/visible permissions through all ancestors.

## Event data and lifetime

Default compound actions use the closed `TNyxSemanticEvent` vocabulary through
`NyxSemantic(nseSearch)`, `NyxSemantic(nseSave)` and related references. Configure
and register with the same typed reference:

```pascal
LSearch := NewNyxSearchField('project-search');
NyxCallbacks(LSearch).OnNamed(NyxSemantic(nseSearch))
  .Add(NyxHandler('TProjectSearch'), NyxCallbackID('project.search'));
```

`NyxEventsMetadata(Component, Document)` returns the selected component's
physical events and semantic actions. It realizes the containing view so sibling
value sources, reusable inheritance and instance/part overrides retain their
actual meaning. Each semantic action has owned `Routes` describing its trigger,
origin, nearest compound source, value control, payload domain and target support.
Several controls may share an action: Kanban's three Add buttons are three routes
on one action. Nested compounds publish their own actions. Removed or renamed
parts update discovery; an inherited callback without a remaining route is
retained as a custom requirement rather than advertised as a working producer.

Scalar routes explicitly mark `PayloadOptional`: an unfilled input may have no
accepted value yet. Read `HasValue` before accessing the callback value. Different
route domains remain separate; their stream-level payload is undefined when no
single common contract applies. `DeclaredProducer` distinguishes a registered
creator's custom producer contract from these inferred physical aliases. Discovery
does not grant permission to call custom `Emit`. Studio shows the source controls
and payload help, and MCP pages routes independently of events/registrations.

Built-in generated Pascal uses semantic enum references. Existing handwritten
`NyxEvent('search')` references remain admitted and retained. Open creator and
application names still use distinct `TNyxEventRef` values. The semantic callback
retains the actual physical `Trigger`; it is dispatched once through the existing
router and shares its ordered registrations, policies and cancellation lifetime.

`TNyxEventInfo` contains owned scalar data and exact portable identities:

| Field | Meaning |
| --- | --- |
| `Trigger` | Closed physical/design trigger |
| `Name` | Open typed application event reference |
| `SourceID` | Semantic component, normally the nearest compound |
| `OriginID` | Physical control that issued the command |
| `TargetID` | Control whose value the command targeted |
| `ValueID` | Control supplying the payload; can differ from the action target |
| `HasValue` | Distinguishes no value from an explicitly empty value |
| `ValueKind` | Text, Boolean, signed integer or finite number scalar contract |
| `Value` | Immutable `TNyxDataValue` snapshot |
| `Changed` | Accepted edit/action requests synchronization; not a revision delta |
| `HasKeyboard` | True for a typed key-down/key-up snapshot |
| `Keyboard` | Immutable `TNyxKeyStroke`: key enum, exact modifiers and repeat flag |
| `HasWheel` / `Wheel` | Owned device request with explicit units, modifiers and physical cancelability |
| `HasViewport` / `Viewport` | Owned control-local offsets, ranges, page sizes and explicit axis units |

For a stepper increment, `SourceID` identifies the stepper, `OriginID` identifies
its increment button, and `TargetID` identifies its numeric part. The callback's
borrowed `ANode` is that semantic source. A clear action carries empty text with
`HasValue=True`; an ordinary command without a value carries `HasValue=False`
and `NyxNull`. Integer and number payloads both use the numeric data family;
`ValueKind` preserves their distinct scalar contracts. Read `Value` through its
checked accessors (`AsText`, `AsBoolean`, `AsInteger`, `AsNumber`).

Use `AEvent.Copy` to retain an event. The copy owns its names, identities and
immutable value; later edits, navigation, unmounting and application destruction
leave it intact. Replacing fields in a caller's copy cannot mutate runtime data.
Do not retain `ANode`, `TNyxDispatch.Source` or `Target` beyond their borrowed
view/callback lifetime. Default, uninitialized event records are not admitted
events and must not be read as payloads.

## Admission and target bridges

`TNyxLiveBindings` stages independent candidates. It captures the target's scalar
contract from its declaration, binding or concrete primitive schema, validates and converts
the payload **before** state publication, then synchronizes accepted controls.
The adapter invokes the callback only after a successful command. Rejected edits
retain state, controls and the previous event; notification failures report the
distinct committed-state diagnostic described in [bindings](bindings.md).
`Edit` accepts exact control wire text at the DOM/LCL adapter boundary; handwritten
state updates use typed references and arguments. `Dispatch` takes `TNyxTrigger`.

`DispatchNyxBehavior` is the lower-level detached-candidate interpreter: its
result describes routing/actions, and its value remains uncaptured until the
live coordinator admits the command. Runtime adapters use the coordinator.
`NyxDesignEvent` carries a text draft without application actions; the undoable
Studio command subsequently admits or rejects that design edit.

Version-1 designs preserve open event names and the existing action wire format.
Generated Pascal uses `OnClick`/`OnChange` and `OnNamed` with `NyxSemantic` for
built-in semantic names and `NyxEvent` for open application names; `Action` uses
enums.
The callback API now requires `TNyxEventInfo`; an earlier string-only callback
must migrate its signature and use `IsNamed`/`Trigger`. Browser and native event
type aliases resolve to the same portable handler contract.

Executed evidence includes 32 shared event checks on FPC and actual pas2js,
41 browser / 43 native control-binding checks, compiled Unicode event
reconstruction on both runtimes and event/action plus typed part/recipe compiler
rejection fixtures on both compilers. The bridges expose four application triggers
and two design triggers.
The expanded families below have newer compiled/control evidence. Structured
extension events and the full production component parity matrix remain open;
declared scalar compound domains are described in [contracts](contracts.md).

## Multiple callbacks and execution policy

Import nyx.events and nyx.scheduler. BrowserRenderer.Events and LCLRenderer.Events
implement the same INyxEvents contract. Each event stream has its own execution
policy and an ordered list of independent subscriptions:

```pascal
FEntered := FView.Events
  .OnAfterEnter(NyxControlEvents('reply'))
  .Policy(neSequential)
  .Subscribe(TReplyEntered.Create);
FAudited := FView.Events
  .OnAfterEnter(NyxControlEvents('reply'))
  .Subscribe(TReplyAudit.Create);
```

TReplyEntered and TReplyAudit can derive from TNyxEventCallback or implement
INyxEventCallback on another reference-counted base. Their callback signature is
Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution). They receive
owned snapshots and cancellation context. Retaining that context does not retain
the subscription's callback or form a cycle. Callbacks own their data dependencies;
avoid a callback retaining its own router/view. Explicit Close also releases
registrations when an application itself owns a router. Renderer disposal calls
Close automatically.

NyxControlEvents selects the originating control. NyxCompoundEvents selects the
semantic source. Design identity is the default; explicit niRuntime selects an
exact realized instance/part. These names remain distinct typed selectors.
OnAfterEnter/OnAfterExit fire from actual focus transitions. A Nyx registration
runs once for a transition described by both a logical LCL slot and a native focus
message. The LCL adapter observes the admitted inner
keyboard surface through its existing revocable message chain. This also covers
returning from a top-level picker while LCL retains the same host active control.
Creator Enter/Exit forwarding remains in its original logical slots; physical
observations capture the typed Nyx focus payload after default native handling
and editing completion. Native focus callbacks share the deferred-control lifetime
guard used by physical input and may retire their own view safely.

Native observation checks LCL's actual post-handler `Focused` state before changing
that transition baseline. A synthetic kill/set reaffirmation that leaves the
editor focused cannot invent another exit/entry or registration invocation.
Real picker departures/returns and explicit application focus redirection still
use the ordinary contract. `NYX_FOCUS_TRACE` enables diagnostic logical/native
message ordering in checked native builds; it is compiled out by default and
does not define application behavior.

Focus observations never execute a click action, change state or clone a view.
Contract.On can declare their scalar
payload; Contract.Signal declares a signal without a payload. Those declarations
persist and generate typed Pascal just like click/change domains.

| Policy | Contract |
| --- | --- |
| neSequential | Each callback completes in registration order before dispatch returns |
| neAsynchronous | Native worker execution; browser deferred event-loop execution |
| neUIQueue | Deferred execution on the UI thread on either target |
| neThreaded | Requires a real worker thread; browser admission rejects it |

Capabilities advertises nscDeferredUI/nscWorkerThreads. Async registrations are
submitted in order; their completion order is unspecified. Every invocation has
an independent execution token and diagnostic. A failing callback does not
suppress siblings. Registration.LastExecution exposes Status/Failure without an
exception escaping a timer/thread or an unsolicited UI dialog.

Stream.Count/Registrations enumerate active subscriptions with distinct IDs.
Subscription.Cancel removes one registration and cancels pending calls. Releasing
a local subscription variable does not remove it. Dispatch snapshots membership
and policy before callbacks: additions begin on the next dispatch, cancellation
suppresses calls that have not started, and nested dispatch uses its own snapshot.
Running work cooperatively observes AExecution.Cancelled. View replacement
cancels its scope, including remaining callbacks and queued child work, while
retaining registrations for the next mount. The legacy click/change bridge runs
after managed callbacks only while that same view remains mounted.

Workers receive no borrowed node/widget. Marshal owned results with
Scheduler.PostUI(LResultWork, AExecution). The parent context links cancellation:
navigation or removal can reject a queued result even after its worker callback
has returned. Omitting Parent deliberately creates independent UI work. Native
PostUI waits for the UI to accept the submission; it does not execute UI work on
the worker. A UI thread must not join a worker waiting for that handoff. Native
console hosts service the UI queue with CheckSynchronize; LCL provides its loop.
Shutdown cancels outstanding work without blocking the UI on running workers.
The first UI submission initializes older FPC threading before ForceQueue so the
deferred contract holds even in a previously single-threaded host.

Native asynchronous/threaded work now shares a lazily started, bounded pool per
scheduler. Configure it with copied Pascal values, independently of the document:

```pascal
LScheduler := NewNyxScheduler(
  TNyxSchedulerOptions.Defaults.Workers(4).PendingCapacity(1024));
LEvents := NewNyxEvents(LScheduler);
```

Defaults use four workers and 1024 pending slots. `Workers` accepts 1..64;
`PendingCapacity` accepts 1..65536. Invalid values and incomplete zeroed options
refuse construction. Queued work dequeues in FIFO order; concurrent completion
remains unspecified. Reused threads require callbacks to clean their own
thread-local state. Workers and pending queues belong to each scheduler; there
is no global pool or thread object retained by the work queue.

A full native queue raises `ENyxScheduleCapacity` for direct submissions, without
adopting/running the refused job. Cancellation reclaims pending capacity before
the next admission. Keep an explicit `INyxWork` lease while handling submission
exceptions; the caller retains refused work. The event router converts that specific refusal into an
independent failed execution/`LastExecution` diagnostic and continues siblings;
worker callbacks never silently run on UI. Other invalid/unsupported admission
errors retain their existing refusal behavior. An adapter may use
`NewNyxFailedExecution` to own an exact nonempty failure diagnostic without work.

The optional `INyxSchedulerMonitor.WorkerLoad` returns a small copied snapshot:
configured limits, active workers, running/pending counts and closed admission.
Read it on UI. Pending excludes running and deferred UI work; cancelled entries
can remain until admission/dequeue/shutdown. Active counts may lag startup or
retirement. Shutdown releases pending couriers and wakes idle workers immediately,
without joining running callbacks. Independent queue/work leases let those finish
after releasing the scheduler. UI handoff continues through `PostUI`.

Browser options are validated, but native pool limits/counts are zero: async stays
on the host event loop and `neThreaded` still refuses. Current native qualification
passes 118 checked pool/capacity/lifetime assertions, including actual retained
Win32 termination handles; existing scheduler/interaction/real-control/ordinary
Studio source checks pass 55/234/45/45, leak-free. A baseline fixture assumed every
source pane displayed its duplicate status; the compact directive deliberately
uses the exact footer instead. That presentation is now checked, and native
captures allocate the Win32 nonclient frame so the footer is not cropped.
Affected browser consumers, Studio and worker compile; current browser execution,
sustained production timing/resident-memory budgets, other widgetsets and physical
device qualification remain open. See
[the worker packet](../WORK.md#current-return-path-bounded-native-callback-workers--2026-10-07).

## Authored callbacks and executable companions

`nyx.callbacks` supplies a reference-counted authoring facade for specialized
controls or their base descriptors. The design stores handler references and
registration IDs, with a policy per event; it never stores a callback instance,
widget or scheduler. These references have distinct Pascal types:

```pascal
LReplyMemo := NewNyxMemo('reply');
LReplyMemo.Configure.Text('Write a reply').Done;

NyxCallbacks(LReplyMemo).OnAfterEnter
  .Policy(neSequential)
  .Add(NyxHandler('TReplyEntered'), NyxCallbackID('reply.entered'))
  .Add(NyxHandler('TReplyAudit'), NyxCallbackID('reply.audit'));
```

Move an existing authored registration without changing its ID, handler or policy:

```pascal
NyxCallbacks(LReplyMemo).OnAfterEnter
  .Move(NyxCallbackID('reply.audit'), 0);
```

The index is its zero-based destination within that exact event. A missing ID or
out-of-range index refuses before metadata changes; an unchanged position is a
no-op. Runtime subscriptions take the new order at the next bind/mount.

Handler class names and registration identities remain independent. IDs retain
exact authored Unicode, are unique within the component, and survive persistence,
history and generated execution. Every registration resolves a fresh callback.
The class registry supplies ordinary reference-counted implementations:

```pascal
type
  { Receives an owned event snapshot and cooperative cancellation context. }
  TReplyEntered = class(TNyxEventCallback)
  public
    procedure Invoke(const AEvent: TNyxEventInfo;
      const AExecution: INyxExecution); override;
  end;

procedure TReplyEntered.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
begin

  if AExecution.Cancelled then
  begin
    Exit;
  end;
  // TODO: implement TReplyEntered.
end;

initialization
  RegisterNyxCallback(NyxHandler('TReplyEntered'), TReplyEntered);
```

Register each referenced class at startup. The browser and LCL application hosts
bind descriptors before their first mount and retain registrations through page
navigation. A lower-level renderer host can call
`BindNyxCallbacks(LDocument, LRenderer.Events, LFactory)` once on its fresh router.
`INyxCallbackFactory.Resolve` may inject dependencies and return
`INyxEventCallback` on another reference-counted base. Factory resolution and
scheduler policy admission precede subscription publication; a missing handler
fails without a partial set of live callbacks.

Reusable definitions supply inherited registrations. Per-instance and named-part
edits materialize an independent descriptor; siblings and templates retain their
registrations. `Clear` explicitly suppresses inherited callbacks; `Inherit`
removes the local descriptor. Authored edits take effect at the next application
mount/build. Runtime subscription cancellation remains a separate live operation.

## Studio properties and events

Select a component or one of its reusable parts, then open **Inspector → Events**.
The Nyx-built tab lists each supported callback, its execution policy and ordered
registrations. **+ Add callback** creates a commented Pascal class with a TODO,
registers it in unit initialization, opens the optional source editor and moves
the caret to that line. Clicking a registered handler returns to its implementation.
Additions refuse an unapplied source draft so handwritten work is retained.

**Remove** displays a warning beside the exact registration and requires explicit
confirmation. It removes the
registration and retains the implementation. The warning names the exact owner,
event, registration and handler; an intervening change invalidates it. Addition,
policy changes and removal share paired source/design undo and redo.

Ordinary browser/native Studio now captures these mutations as typed commands
for independent source preparation. Accepted files stay in place while preparing;
waiting policies remain visible, with focus restored only to the exact owner and
event. A confirmed removal disables that event's editing controls until it
retires. Its warning stays owned until successful publication; a rejection keeps
the warning available for correction. Reloading a project retires its warning,
even when the same registration IDs reappear. Keep registration only dismisses
the presentation and does not cancel an already submitted confirmation.

Adding a handler returns an admitted handler reference. Studio opens its TODO
line only if that command's owner/view is still selected. Later navigation stays
independent. Policies and removals retain an independent Pascal draft and its
original base; additions still require Apply or Restore before generating a new
implementation. The legacy synchronous router uses the same typed intent and
registration checks. The real compiled browser worker now passes this local
ordinary-control journey at desktop and narrow widths; authenticated observing
deployment remains open. See [browser qualification](browser-qualification.md)
and [current evidence](../WORK.md#real-browser-worker-and-contextual-warnings--2026-10-06).

**Properties** exposes text and the relevant typed fields; **More properties**
expands the complete shared fluent configuration. `NyxProperties` and
`NyxEventsMetadata` return independent snapshots. Extensions publish typed
properties, constraints and event capabilities with `RegisterNyxSchema` against
a `TNyxKindRef`; custom factories remain responsible for their advertised hooks.
Declaring a signal alone does not manufacture an adapter bridge.

Common application triggers include **OnClick**, **OnChange**, **OnAfterEnter**,
**OnAfterExit**, **OnKeyDown** and **OnKeyUp**. Both adapters wire clicks for common controls, including
labels and framed fields. Focus/change callbacks appear for focusable/editable
controls and compound descendants. Keyboard hooks use the actual editable input
or button and route to its nearest semantic compound. Complete property/event
capability coverage, production control behavior and remaining physical input/accessibility
retain their open [event](../TODO/NS-1_event-scheduler_01.md) and
[parity](../TODO/NS-2_parity-accessibility_01.md) owners.

General managed click subscriptions are explicit. The older whole-view method
bridge retains default button/link clicks and explicitly named clicks; adding
support for labels and fields does not invent extra global events. Direct native
checkbox setters retain LCL's click-on-change semantics. Updates through Nyx
State suppress callback feedback on both targets.

## Typed keyboard callbacks

Authored keyboard registrations use the same owned descriptors, policies and
source/history workflow. Studio exposes both families for supported focusable
controls. The expanded phase contract is described below. A shortcut is an enum
and a modifier set, rather than a behavior string:

```pascal
NyxCallbacks(LReplyMemo).OnKeyDown
  .Policy(neSequential)
  .Add(NyxHandler('TReplySubmit'), NyxCallbackID('reply.submit'));

procedure TReplySubmit.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
begin

  if AExecution.Cancelled then
  begin
    Exit;
  end;

  if AEvent.HasKeyboard and
    AEvent.Keyboard.Matches(nkEnterKey, [nmControl]) then
  begin
    NyxEventResponse(AExecution).Consume;
    // TODO: submit the reply using owned application data.
  end;
end;
```

`TNyxKey` covers navigation/editing keys, modifiers, letters, digits and F1–F24.
The browser adapter reads logical `KeyboardEvent.key`; the LCL adapter admits
virtual key codes. Unknown/layout-specific names stay `nkUnknownKey`, without
guessing letters from browser physical codes. Text insertion remains the value/
change contract; key identities are not Unicode characters or an IME transport.
Composition/process-key messages bypass shortcut dispatch. `nmAltGraph` is a
distinct modifier, and exact matching does not mistake it for a Ctrl+Alt shortcut.

`Matches` requires exact modifiers and refuses repeats by default. Pass its third
argument as True when repeat commands are deliberate. Native repeat tracking
clears on key release and focus loss; browser repeats use the platform flag.
Owned keyboard/scalar snapshots remain readable after navigation and disposal.
Before a browser keyboard payload is captured, an outstanding text-field edit
passes through the existing typed change admission. A shortcut sees current
accepted text without requiring blur, matching native text controls. Numeric
domain drafts keep their editing-complete boundary; keyboard signals do not
execute a click action or bypass value validation.

`NyxEventResponse(AExecution).Consume` suppresses the platform default while
ordered sibling callbacks still run. An invocation can consume only while its
sequential UI callback is active. Returning, cancellation and navigation seal or
invalidate that opportunity. Queued/threaded callbacks may inspect the snapshot
but an attempted Consume records an explicit execution failure. Ordinary signal
dispatch and scheduler child work have no physical input consumption window.
`CanConsume` and the monotonic `Consumed` decision expose those distinctions.

Both adapters cancel a stale input's default when navigation invalidates its
view. The themed native button reuses Lazarus's state and Click operations while
placing key-up callbacks before activation; a consumed key-down cannot cause a
later release click. Unhandled Space/Enter retain normal button activation.

Studio builds its accepted Pascal companion through the HTTP service; a pending
draft must be applied or restored first. Full applications preserve exact source.
Isolated pages/reusable views replace the managed builder and preserve application
imports, helpers, callback classes, initialization and comments. See
[building](building.md) for the paired request and current executed evidence.

## Interaction families and timing

The current registry has 28 runtime event families. `NyxEventsMetadata` describes
the selected projection and its interactive compound parts; Studio uses that
same schema for descriptions, policies, ordered registrations and source links.
Built-in display controls have pointer/menu hooks. Focusable controls, including
read-only code blocks, also have key/focus hooks. Text inputs have text-admission
hooks; number/choice drafts retain their editing-complete value contract.

`NyxSupportsKeyboard(Node)` is the typed classification shared by default event
metadata and adapters. Literal lists, tables, trees and code blocks each expose
one inspection face; bound collections keep their attachment-owned row/editor
entry. A literal tree is a flat item presentation. Use a collection tree for
hierarchy, expansion and selection rather than inventing empty disclosure parts.

Both renderers expose `FocusFor(ID, Identity)` alongside `InputFor`. The former
borrows the actual focus face, including a split divider; the latter retains its
scalar-editing meaning. Missing identities raise, unsupported faces return nil,
and borrowed widgets/elements expire when their view unmounts. A bound collection
returns its logical host; its attachment manages descendant entry. Browser
collection enter/exit notifications observe crossings of that logical boundary,
so moving from a row into its cell editor does not manufacture another entry.

Read-only inspection faces remain focusable. Disabled links lose their live
href, and disabled static faces lose their Tab entry; synchronization restores
these when re-enabled. Native radio peers retain one eligible entry, preferring
the checked peer. HTML groups retain their physical name/form boundary. When a
checked HTML peer is disabled or hidden, the eligible entry's existing label
forwards focus without scrolling to its real input; checked values and native
input keyboard behavior remain intact. Creator factories retain their hooks.

| Family | Fluent registration methods |
| --- | --- |
| Commands | `OnClick`, `OnChange`, `OnDoubleClick` |
| Focus | `OnAfterEnter`, `OnAfterExit` |
| Keyboard | `OnBeforeKeyDown`, `OnKeyDown`, `OnAfterKeyDown`; `OnBeforeKeyPress`, `OnKeyPress`, `OnAfterKeyPress`; `OnBeforeKeyUp`, `OnKeyUp`, `OnAfterKeyUp` |
| Text | `OnBeforeTextInput`, `OnTextInput`, `OnAfterTextInput` |
| Pointer | `OnPointerDown`, `OnPointerUp`, `OnPointerMove`, `OnPointerEnter`, `OnPointerExit` |
| Menus | `OnContextMenu` |
| Wheel requests | `OnBeforeWheel`, `OnWheel`, `OnAfterWheel` |
| Viewport observations | `OnScroll`, `OnScrollEnd` |

Each method is available on both `NyxCallbacks(LControl)` and the runtime
`INyxEvents` router, with a typed target on the latter. They retain the same
multiple-registration and execution-policy contract as the original events.
Generated companions use these named methods, rather than behavioral strings.

Each phase captures its own scalar declaration immediately before dispatch.
For example, a component can declare `Contract.Signal(ntBeforeKeyPress)` and
`Contract.On(ntKeyPress, NyxTargetValue, NyxTextDomain)` independently. The first
has `HasValue=False`; the second reads the declared text target. Both still carry
their owned keyboard snapshot. Text phases follow the same rule: a signal-only
hook retains `HasTextEdit` without inventing a scalar payload. Accepted text
phases capture after the change callbacks; their scalar value reflects current
accepted state while `TextEdit` preserves the original admission attempt.

Adapters emit keyboard phases before the platform default. Sequential callbacks
execute there; deferred/worker policies receive owned snapshots later and cannot
consume. Their completion order does not follow phase order. A key-down first emits its
before/main/after cycle, followed by the key-press cycle for a non-modifier key.
`OnKeyPress` means logical key actuation, including configured repeat; characters
and composition belong to text input. It does not depend on the browser's legacy
`keypress` event. Key-up has its own cycle. Consuming a before hook skips that
cycle's main hook; its after hook observes `DefaultPrevented=True`. Consuming a
down cycle also suppresses its press cycle. After hooks are observers and cannot
consume. Navigation stops all remaining phases. These are Nyx callback phases,
rather than a promise that an after-key callback runs after native text insertion.
The browser adapter follows current [UI Events keyboard/input separation](https://w3c.github.io/uievents/).

`HasTextEdit` supplies `TextEdit.Before` and `TextEdit.After`, complete owned text
values covering typing, paste, deletion, composition and virtual keyboards.
`OnBeforeTextInput` runs **before Nyx model admission**, while the physical control
contains a proposed draft. Sequential callbacks can consume it, restoring the
latest accepted physical value without a partial state update. Accepted changes
emit `OnChange`, `OnTextInput` and `OnAfterTextInput`; callback-cancelled proposals
emit only `OnAfterTextInput` with `DefaultPrevented=True`. Invalid domain values
use the existing binding-rejection diagnostic. Equal-value input/change messages
create no duplicate command. This portable hook does not claim every native edit
origin has a browser-style cancellable `beforeinput` message.

```pascal
NyxCallbacks(LReplyMemo).OnBeforeTextInput
  .Policy(neSequential)
  .Add(NyxHandler('TValidateReply'), NyxCallbackID('reply.validate'));

procedure TValidateReply.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
begin

  if AEvent.HasTextEdit and (Length(AEvent.TextEdit.After) > FMaximumLength) then
  begin
    NyxEventResponse(AExecution).Consume;
  end;
end;
```

`HasPointer` supplies `Pointer`: typed kind/button/button-set/modifiers, logical
control-relative position, identity, primary status and normalized pressure.
Positions start at the control's outer logical face, including its frame. An
inner native input or group client origin is converted into that face's space;
clipped native faces retain their logical content offset. The native renderer's
`ScreenPointFor` converts this snapshot back to an actual screen point, removing
the projection offset exactly once. Actual captioned-group and cropped-page
round trips are qualified in [the native group packet](../WORK.md#current-return-path-native-group-content-geometry--2026-10-07).
Browsers retain mouse/touch/pen data; ordinary LCL slots report mouse, identity
zero and unavailable pressure zero. Keyboard menu requests have
`HasPosition=False`. Only the context-menu hook has a consumption window; ordinary
pointer callbacks observe interaction without suppressing selection, focus or
scrolling. Framed fields route once through their owning face. Separately
projected descendants retain their own identity and never duplicate a down/up/
move callback on an ancestor.

All snapshots survive unmounting. Adapters preserve custom widget handlers and
stop after a chained handler navigates. Empty phase streams avoid scheduler work.
Studio creates normal typed TODO handlers for these families, supports multiple
registrations, and uses paired undo/removal/source-navigation safeguards.
Complete rich-selection keyboard patterns and broader physical-device qualification
remain with the open event/parity tasks; the registry
above does not imply that every possible platform event is already supported.

## Wheel requests and actual scrolling

`nyx.viewport` provides immutable `TNyxWheelSnapshot`, `TNyxViewportAxis` and
`TNyxViewportSnapshot` records. Events carry `HasWheel/Wheel` or
`HasViewport/Viewport`. Their owned values survive navigation and disposal;
no DOM event, native control or mutable array enters a scheduled callback.
Default records are undefined observations. Constructors reject nonfinite data
and negative dimensions; signed positions retain browser RTL/overscroll values.

```pascal
NyxCallbacks(LReplyMemo).OnBeforeWheel
  .Add(NyxHandler('TReplyWheelHandler'), NyxCallbackID('reply-wheel'));
LRenderer.Events.OnScroll(NyxControlEvents('result-list'))
  .Subscribe(LViewportObserver);
```

Wheel phases bracket Nyx dispatch before the platform default. A consumed before
hook skips the main hook; the after hook reports `DefaultPrevented` and cannot
consume. Fresh phase snapshots preserve each declared payload. Browser wheel
deltas retain `nwuPixels`, `nwuLines` or `nwuPages`, including fractional values
and modifiers. LCL normalizes 120 wheel ticks to `nwuDetents`, never guessing a
pixel distance. Positive X/Y mean right/down; the native horizontal and vertical
sign conventions are adapted separately. A wheel may scroll, zoom or do nothing.
It never fabricates an `OnScroll` notification.

Browser wheel listeners are explicitly non-passive. `Wheel.CanCancel` reports
the physical event's cancelability; un-cancelable requests have read-only
responses even in sequential callbacks. Asynchronous and after callbacks cannot
consume. This follows [Wheel Events](https://www.w3.org/TR/pointerevents4/#wheel-events),
whose Level 4 publication is a working draft. Native callbacks use LCL's actual
handled flag and preserve creator-installed handlers. Navigation ends a phase
cycle before accessing a borrowed control again.

`OnScroll` observes actual local movement, whether caused by a wheel, keyboard,
touch, scrollbar or programmatic operation. It cannot cancel past movement and
does not change design values, selection or undo history. `ViewportFor(ID)` reads
the same snapshot directly from either renderer. Each axis declares its units:
browser CSS pixels, native scroll-box logical pixels, grid rows/columns, list
items, or explicitly widget-specific native scrollbar units. Undefined axes
remain undefined rather than pretending the platform supplied offsets.

Memo, code editor, code block, scroll area, list, table and tree projections
publish viewport support. Browser listeners attach to the actual framed input
or scroll face and are revoked during disposal, including for retained detached
elements. Fixed-height tables retain their scrolling face through `Sync`.
Native scroll areas reuse `TScrollBox`; read-only code blocks retain native
scrollbars. One managed native idle observer per admitted view compares actual
control offsets. It coalesces movement until UI idle, scans only when scroll
subscribers exist, and installs no timer or background polling. Its weak sink is
revoked before disposal. This is explicit `ncBasic` native support, not a promise
to report every intermediate offset or support every widgetset.

`OnScrollEnd` follows genuine host completion from
[CSSOM View](https://www.w3.org/TR/cssom-view/#scrolling-events). Browser support
depends on the host's `scrollend` event and is marked `ncBasic`. Nyx does not
approximate completion with a timeout. LCL has no shared completion producer;
metadata explicitly marks it `ncMissing`. Studio and semantic MCP expose these
grades and descriptions with the independently authored callbacks and policies.

## Collection selection notifications

Bound lists, tables and trees expose `OnSelectionChange` through the canonical
event registry. An admitted collection binding is required; an unbound decorative
list does not advertise a producer it cannot supply. Compound metadata discovers
the event through actual bound parts, preserving semantic source/origin routing.

```pascal
NyxCallbacks(LTasksList).OnSelectionChange
  .Policy(neSequential)
  .Add(NyxHandler('TUpdateTaskActions'), NyxCallbackID('tasks.selection.actions'));
```

The callback's `HasCollectionSelection` flag qualifies its typed immutable
`SelectionBefore` and `Selection` snapshots. Each contains scoped membership,
focus, range anchor and the dataset revision used for admission. They retain no
control, dataset or callback receiver. Selection-only updates do not change
authored collection defaults or generated source. Equal membership/focus/anchor
does not fire twice; pruning removed identities does report an accepted change.
Published selection cannot be consumed. Policy, multiple registrations, failures,
navigation cancellation and UI handoff use the normal scheduler.

Studio exposes the selection mode in Bindings and the callback in Events, with
the normal TODO template, source navigation and paired history. MCP `nyx_node`
with `events=true` exposes support and bounded registration context. See
[collection views](collection-views.md#selection-edits-and-failures) for the typed
runtime commands, target gestures, compatibility and remaining pattern checks.

## Creator-defined named events

Named streams use exact `TNyxEventRef` identities instead of extending the closed
physical-trigger enum for every component. `OnNamed` accepts distinct names on
the same control, each with its own ordered registrations and execution policy:

```pascal
NyxCallbacks(LItemButton).OnNamed(NyxEvent('ItemOpened'))
  .Policy(neUIQueue)
  .Add(NyxHandler('TOpenItemDetails'), NyxCallbackID('item.open.details'));
```

Runtime subscriptions use `LEvents.OnNamed(NyxControlEvents('item-button'),
NyxEvent('ItemOpened')).Subscribe(LCallback)`. Existing semantic click/change
names can also be subscribed to this way. Names are case sensitive, preserve
Unicode, and require 1–128 scalars without controls or line separators.
`NyxNamedEvent` validates a name explicitly; `OnNamed` always validates it too.
The transport value `ntNamed` is deliberately refused by anonymous `On(Trigger)`.

A custom producer publishes an owned schema through `RegisterNyxSchema`:

```pascal
RegisterNyxSchema(NyxCustomKind('item-picker'), [], [
  NyxNamedEventSchema(NyxEvent('ItemOpened'), 'OnItemOpened',
    'An item opened; its ordinal is between 0 and 10.',
    ncCustom, ncCustom, NyxScalarPayload(NyxIntegerDomain.Range(0, 10)))
]);
```

Import `nyx.schema`, `nyx.event.payload` and `nyx.contract` for this declaration.
`NyxSignalPayload` means no payload; even explicit JSON null is refused.
`NyxScalarPayload` accepts typed text/Boolean/integer/number domains, including
ranges and choices. `NyxDataPayload(ndObject)` declares structured details of an
exact data kind. This last form is an explicit extension boundary; it does not
claim compile-time typing for arbitrary object fields. Nested Unicode and decimal
spelling remain exact owned values.

`RegisterEventFactory` accepts a typed custom kind and a target-specific factory
with an `INyxEventEmitter` argument. The browser factory returns a detached
element; the LCL factory also receives its owning `TComponent` and returns an
unparented owned control. Existing updater/binding requirements apply. A creator
retains the emitter in its control and calls `Emit(NyxEvent('ItemOpened'),
NyxData(LOrdinal))` from an actual target hook.

Ports are dormant during candidate construction, connected after the whole view
is accepted, and revoked before remount, unmount or destruction. A failed
candidate revokes only its own ports and leaves the accepted view intact.
Retained stale ports fail explicitly. Undeclared names, unavailable target grades
and wrong payloads fail before any callback. Disabled/hidden ancestors suppress
notifications; read-only controls may still report them. Producer calls require
UI access through the scheduler's `RequireUI` contract. Native worker results
must use `PostUI`, with the parent execution supplied for cancellation.

Callbacks receive `Trigger=ntNamed`, exact `Name` and mounted source/origin IDs.
Scalar payloads populate `HasValue`, `ValueKind` and `Value`; structured payloads
populate `HasDetails` and `Details`. Signals set neither flag. Copies retain no
node/widget handles and survive view disposal. Scheduling, failure diagnostics,
subscription mutation, reentrancy and generation cancellation share the existing
router; named events do not introduce a second dispatch system.

Studio publishes one Events card per declared name, shows its description and
payload summary, and supports policies, multiple TODO handlers, source navigation
and confirmed removal. Persistence, paired history and source reconciliation keep
exact identities. Creator declarations belong to linked Pascal component code;
documents store authored subscriptions, not executable adapters. An authored
event whose creator is absent stays discoverable with an explicit custom bridge
requirement and cannot be emitted as a declared custom producer.

MCP `nyx_node` reports exact names and payload descriptors with `events=true`.
`eventOffset`/`eventLimit` request a bounded page (default 32, maximum 50);
`totalEvents` and `eventOffset` describe that page. It does not return the document
to inspect one callback family.
Registrations belong only to the returned exact event identities. Across that
page, `registrationOffset`/`registrationLimit` request callbacks in event and
registration order (default 16, maximum 50). `totalRegistrations` counts callbacks
within that event page. `registrationsPartial` marks omitted callbacks or authored
streams outside the event page. Empty authored streams still report their
independent policy. A page is context for inspection, not a complete replacement
descriptor for an editor mutation.

## Typed editing sessions

Text inputs and code editors expose `OnBeforeEdit`, `OnCompositionStart`,
`OnCompositionUpdate`, `OnCompositionEnd` and `OnTextSelectionChange` beside the
existing Nyx text-admission phases. Authored and runtime registration use the
same named fluent methods and independent ordered callbacks/policies:

```pascal
LReplyMemo := NewNyxMemo('reply-memo').WithText('Reply');
NyxCallbacks(LReplyMemo).OnCompositionEnd
  .Add(NyxHandler('TReplyComposition'), NyxCallbackID('reply.composition'));
```

Guard `AEvent.HasEditing` before reading the immutable `AEvent.Editing` snapshot.
`TNyxEditIntent` carries the 46 defined editing intentions plus `neiUnknown`.
Unknown platform names remain diagnostic `WireIntent` data. `HasData` distinguishes
missing insertion/composition data from a present empty string. Snapshots own
their physical text and selection; they survive control destruction and queued
dispatch without retaining a widget.

`TNyxTextSelection` uses zero-based Unicode scalar positions and an exclusive end.
Combining marks count separately; these positions are not grapheme, UTF-8 byte or
UTF-16 offsets. Checked constructors reject malformed Unicode, reversed ranges,
out-of-bounds positions and offsets inside a surrogate pair. `Defined=False`
means selection is unavailable. `Direction` distinguishes forward, backward,
none and unknown. Renderer `EditingFor`, `TextSelectionFor` and `SetTextSelection`
query or select the mounted input on the UI thread without rewriting its value,
focusing it or scrolling its viewport. Native positions refer to physical text,
including native CRLF, rather than a separately normalized model value.
The basic native setter accepts None/Unknown direction and refuses a requested
forward/backward active endpoint before altering the range.

Browser `OnBeforeEdit` adapts genuine `beforeinput` cancelability outside IME.
Only an active sequential callback with `NyxEventResponse(AExecution).CanConsume`
may consume that physical operation. Post-edit observations, queued callbacks
and composing requests cannot consume it. LCL does not advertise a physical
beforeinput bridge; native intent is unknown when the platform cannot identify it.
`OnBeforeTextInput` remains the portable model-admission rejection boundary on
both adapters. Physical cancellation and restoring a rejected model proposal
are distinct contracts.

An active IME owns its draft: input and state refresh preserve it until completion,
and Nyx shortcuts bypass it. Final text is admitted once, with ordinary proposal,
acceptance/refusal and after phases; the end observation retains the original
physical result even when admission restores accepted text. Win32 LCL chains
genuine IME messages and drains the result at UI idle because committed characters
can follow END. Native text-selection observations coalesce at idle and report
unknown direction. Other widgetsets need an editing bridge; capability help
states this limitation. Native callback navigation revokes all Nyx slots and
retires active controls through the LCL release queue so the current message can
finish safely.

Studio exposes these events through the same inspector, TODO/source navigation,
removal warning and paired history as other families. Bounded MCP event pages
publish a `contexts` array identifying editing, keyboard, text-admission,
pointer, wheel, viewport and collection-selection snapshots. Context metadata
describes supported observations; applications still check the corresponding
event presence field.

## Pointer ownership and typed drag/drop

`nyx.gestures` keeps physical response requests independent of widgets. An active
sequential callback queries `NyxGestureResponse(AExecution)` and checks
`CanRequest` before calling `CapturePointer`, `ReleasePointer`, `OfferDrag` or
`AcceptDrop`. Adapters apply the sealed result only if the original view is still
mounted. Last valid ordered request wins; a failed registration does not suppress
siblings. Cancellation revokes that registration's authority. Retained, queued,
asynchronous and threaded callbacks cannot alter a physical gesture retroactively.
Their owned event snapshots remain usable for later application work.

`OnPointerCapture` and `OnPointerCaptureLost` observe actual acquisition/loss.
`OnPointerCancel` describes an interrupted pointer stream; losing capture alone
does not imply cancellation. Observation callbacks have no request authority.
The browser uses actual pointer identities and releases capture on disposal.
LCL supports the native mouse capture bridge and genuine cancel-mode messages.

Sources and targets opt in through specialized configuration:

```pascal
LTransferButton.Configure.DragSource(True).TouchBehavior(ntbNone);
LDropCard.Configure.DropTarget(True);
```

`TNyxTouchBehavior` provides Automatic, None, PanX, PanY and Manipulation choices.
The browser applies the corresponding direct-manipulation behavior. Native LCL
reports this directive unavailable rather than simulating touch restrictions.
Per-platform fluent configuration can select a directive through the existing
typed platform contract; application code needs no compiler conditional.

Seven named events describe Start, Drag, Enter, Over, Exit, Drop and End. Guard
`AEvent.HasDrag` before reading `AEvent.Drag`. Its immutable snapshot owns Phase,
Transfer, source Allowed operations, current/final Operation and optional
SourceID. Browser SourceID is empty when the platform supplies no Nyx identity.
Native internal transfers retain their known source identity. `CanRespond`
describes physical negotiation support, while execution `CanRequest` additionally
checks the registration's active synchronous authority.

For example, a handwritten source handler can offer multiple typed formats:

```pascal
LResponse := NyxGestureResponse(AExecution);

if LResponse.CanRequest(ngcOfferDrag) then
begin
  LResponse.OfferDrag(
    NyxTransferText('A useful description').WithItem(
      NyxTransferValue(NyxObject([NyxField('id', NyxData('document-42'))]))),
    [ndoCopy, ndoMove]);
end;
```

Targets explicitly accept a source-allowed Copy, Move or Link, or reject with
None. Acceptance alone never mutates a document, removes a source or opens a URI.
Applications own those domain actions. Read-only targets refuse editing; read-only
sources refuse offers containing Move. An absent offer cancels a configured drag.
`LastGestureError` and `OnGestureFailure` expose owned adapter refusal diagnostics.

Transfers preserve present-empty versus absent values and exact Unicode. Named
format constructors supply text, markup, URI lists and structured Nyx data;
custom MIME names use a checked distinct `TNyxTransferFormatRef`. At most 16 formats,
65,536 text scalars and 64 file metadata records enter one snapshot. Start and Drop
permit data access. Hover, Exit and End retain protected format names and file
advertisement; attempts to read text or file metadata there fail explicitly.
Browser external drops expose bounded owned metadata, never file handles, contents
or local paths. Markup and URI text are data; Nyx does not render or follow them.

The native adapter uses owned LCL drag objects for internal mouse transfers.
Controls retired during a callback move to an independent hidden host until the
native constructor/message unwinds; queued cancellation precedes release. A
caller may close its original form in that interval. Arbitrary custom native
drag objects require their own adapter. Native external OS file drops and the
continuous source `OnDrag` event are unavailable in the present LCL bridge.
Studio and bounded MCP discovery publish these capability differences through
the same canonical metadata, alongside the pointer/drag context declarations.

## Standards baseline and open qualification

The keyboard/text split follows [UI Events](https://www.w3.org/TR/uievents/):
logical key actuation and editing are separate, and browser keypress is a
deprecated event. Nyx's named key-press phases do not depend on that legacy DOM
event. [Input Events Level 2](https://www.w3.org/TR/input-events-2/) defines edit
intent and IME composition ordering/cancelability. Nyx's owned editing contract
preserves these physical distinctions alongside its Before/After model-admission
snapshots.

[Pointer Events Level 3](https://www.w3.org/TR/pointerevents3/) is the current
Recommendation, published 2026-06-30. It supplies the capture/cancellation and
direct-manipulation baseline. The [HTML Living Standard drag-and-drop
model](https://html.spec.whatwg.org/multipage/dnd.html) supplies the physical
transfer access windows and operation negotiation. These references were checked
on 2026-10-04. The cited UI Events and Input Events Level 2 versions are Working
Drafts; recommendations, draft guidance and implemented capability grades remain
distinct.

The Pascal browser-input driver exercises host-generated mouse/touch capture,
release/cancellation and real drag-store negotiation. Synthetic DOM and injected
Windows/LCL messages also test adapter boundaries. These checks do not establish
physical hardware, IME, assistive-technology or other-widgetset qualification.
Full keyboard accessibility, pen/coalesced/predicted pointer data and native
external file transfers remain open with the original event/parity owners.
