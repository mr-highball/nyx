# Managed confirmation presentations

[Compound components](components.md) · [Events](events.md) ·
[Portable contract](../src/nyx.confirmation.pas) ·
[Browser adapter](../src/nyx.confirmation.browser.pas) ·
[LCL adapter](../src/nyx.confirmation.lcl.pas)

`INyxConfirmationDialog` describes independently owned, customizable content.
`INyxConfirmation` presents a clone of that content and owns its rendering lifetime.
The existing recipe can still be used inline. Applications opt into the managed
presentation through `NewNyxBrowserConfirmation` or `NewNyxLCLConfirmation` at their
target host boundary; the remaining application contract uses `INyxConfirmation`.
The portable units contain no DOM or LCL types.

## Typed configuration and customization

Construct and configure the template through specialized interfaces:

```pascal
LTemplate := NewNyxConfirmationDialog('remove-project');
LTemplate.TitleHeading.Text := 'Remove this project?';
LTemplate.DescriptionLabel.Text :=
  'Review the change before continuing. Your other projects stay available.';
LTemplate.ActionsCancelButton.Text := 'Keep project';
LTemplate.ActionsConfirmButton.Configure
  .Text('Remove project')
  .Variant(nvDanger)
  .Done;
LTemplate.ActionsRow.Add(NewNyxButton('learn-more')
  .Configure.Text('Learn more').Done);
```

The presenter copies the complete template, including extra controls, named parts,
contracts and nested descendants. Its `Content` remains specialized and editable.
Configure it before `Open`; edits while open are mounted at the next opening.
Changing one presentation does not change its original template or another clone.
The default actions row wraps rather than assuming a fixed button count.

```pascal
// APrompt is the managed presentation supplied by the application's target host.
// AApply and ACancel implement INyxEventCallback; their owners do not retain APrompt.
APrompt.OnConfirm.Policy(neSequential).Subscribe(AApply);
APrompt.OnCancel.Subscribe(ACancel);
APrompt.Open(NyxConfirmation('Remove this project?')
  .Focus(NyxPart('actions/cancel'))
  .Sizing(ncsContent));
```

`NyxConfirmation` defaults to content sizing, a 560-pixel maximum width, a
360-pixel maximum height, 94% of the owning viewport and initial focus on Cancel.
Use `.Window(NyxModal('Review change').MaximumWidth(720).MaximumHeight(480))`
to supply copied geometry, or `.Sizing(ncsViewport)` to reserve that geometry.
Pixels/percentages are logical host values. `MaximumHeight(0)` preserves the
original modal host's viewport sizing; nonzero height accepts 120..16384.
Width accepts 240..16384 and viewport percent accepts 20..100. Invalid values
raise `ENyxModel` before changing the target window.

The focus reference must resolve to an enabled, visible, physically focusable
part. Missing or unavailable focus refuses opening and retains the previous
result/background. Text is `TNyxText`; dimensions, sizing, semantic actions and
execution policies use numeric/enum/fluent contracts rather than behavior strings.
Part and application identities remain explicit open references.

## Results, events and lifetime

`Result` moves from `ncrNotOpened` to `ncrPending`, then `ncrConfirmed` or
`ncrCancelled`. The dialog itself never edits an application document. A consumer
must recheck the relevant application revision/context when admitting its command.

The template's typed Confirm/Cancel actions produce completion events only from
the confirmation root's semantic scope. Clicking another added action keeps the
dialog open. `Events` exposes its ordinary mounted Nyx router, so extra buttons,
key/focus events and other parts use the existing public event contracts.
`OnConfirm` and `OnCancel` support multiple ordered registrations, subscription
removal and the scheduler's execution policies. Completion first closes the host
and returns focus, then dispatches an owned snapshot. A synchronous completion
callback may reopen or release the presenter. Escape/window dismissal is Cancel;
`Close` is silent and turns a pending result into Cancelled. Repeated opening and
calls during initial/return focus transitions refuse instead of overlapping hosts.

Call methods on the UI thread. A threaded callback must queue UI work through the
existing scheduler; the presenter does not create a second scheduler. Registrations
must avoid strongly retaining their own presenter, which would create an application
reference cycle. Retained content handles stay valid after presenter retirement.
Retained event streams/subscriptions become inactive when their router closes.

Target `Renderer` observations are borrowed. Target `Host` observations are retained
managed interfaces: retirement closes/disconnects the host, and a retained host can
outlive the presenter. The native owning window must outlive both. Optional themes
are borrowed and must outlive the presentation. Target adapters clear borrowed
observers and unmount their views before releasing the host. Native focus uses weak
component notification, so deleting the invoking control while open does not leave
a stale widget pointer. Browser return focus checks that the invoker is attached.

## Studio reuse and semantic reproduction

Studio's registration-removal warning now consumes `NewNyxConfirmationDialog`
and its specialized title, description and action parts. It remains inline in the
Inspector. Stable control identities, the exact registration/context checks,
pending-command admission and paired history remain owned by the Studio controller.
This does not claim that Studio warnings now use floating modal windows.

Compose the English companion with Nyx MCP, without replacing the user's project:

1. Inspect `nyx_session`, create an empty owned `nyx_reviews` workspace at its
   revision, and use that review handle on every following operation.
2. Apply each array in
   [the semantic recipe](../tests/confirmation-review.operations.json) as a grouped
   `nyx_transaction` with a fresh expected revision and unique operation ID. Inspect
   bounded `nyx_node` parts before customizing a recipe whose part identities differ.
3. Export bounded `nyx_source` windows (at most 60 lines per query) at one unchanged
   revision to `build/confirmation/source/nyx.generated.view.pas`. Preserve exact
   line endings/bytes; do not rewrite the accepted source as a handwritten fixture.
4. Use `nyx_build` for browser and LCL application builds at that revision. Compare
   the accepted source fingerprint with the actual compiled source. These builds
   qualify the authored content, not execution of the runtime presenter.
5. Run `tools/build.ps1 -Target confirmation` to compile the same source into the
   [actual target consumer](../tests/nyx_confirmation_controls.lpr). It executes
   checked native controls and stages `confirmation.html`, generated JavaScript
   and matched `rtl.js` for an already admitted HTTP host. It starts no listener.
6. Execute that browser fixture at desktop and narrow widths, inspect its passed
   marker/check count and preview selectively. Discard the owned review at its
   current revision after retaining evidence. The user's accepted pair is unchanged.

## Evidence and limits

The 2026-10-06 packet passes 36 checked Win32 interaction checks with zero leaks and
35 actual browser checks at each measured CSS width, 1076 and 576 pixels. Both use
the same byte-exact English MCP companion. Added-action routing, safe initial and
return focus, unavailable focus refusal, ordered callbacks, callback-driven reopen/
release, closing transition refusal, independent content, invoker removal and target
retirement are covered. Native Studio's queued callback workflow passes 57 and its
source workspace passes 30, both with zero leaks. See
[the evidence record](../WORK.md#managed-confirmation-presentation--2026-10-06).

Browser modality uses the HTML dialog top layer. Native modality disables/restores
the exact owning window while ordinary event processing continues. The intent is
consistent with the [WAI modal dialog pattern](https://www.w3.org/WAI/ARIA/apg/patterns/dialog-modal/)
and [HTML dialog model](https://html.spec.whatwg.org/multipage/interactive-elements.html#the-dialog-element),
checked 2026-10-06; full standards/accessibility qualification is still open.
These fixtures invoke actual host controls/events, but do not establish hardware
Tab navigation, physical-phone behavior, IME, assistive technology, other widgetsets
or application-wide native modality. Unrelated native windows are not isolated;
nested/out-of-order presentations need further qualification. Oversized custom
native content must provide a suitable Nyx scrolling layout; fitting/capping alone
does not guarantee every custom action remains reachable.

The observing LAN Studio still runs its preserved earlier release. Its protected
history migration and rollout remain open. The later
[real-worker packet](browser-qualification.md) qualifies ordinary browser Studio's
inline callback warning at desktop and narrow widths, with 64 checks each and
59 native checks. Its warning is beside the exact registration. This does not
qualify authenticated observing deployment. General modal, picker and overlay
lifecycle integration remain with the original NS-2/NS-3 owners.
