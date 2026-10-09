# Studio mounted section routing

[Architecture](architecture.md) · [Independent view sections](view-sections.md) ·
[Current evidence](../WORK.md#current-return-path-nested-source-host-recovery--2026-10-09)

Studio still composes its chrome with `BuildNyxStudioView` and the public Nyx
components. `nyx.studio.sections` copies that composition into independent
Chrome, Project, Inspector, Resources and workspace Details documents.
`nyx.studio.section.views` mounts them
through the ordinary browser/LCL renderers and public managed view sections.
Canvas and Pascal source already have separate view owners. The borrowed lookup
forest is rebuilt after each successful admission; resolve it again after refresh.
Real model roots and controls follow their owning section's lifetime instead.

The surrounding Chrome owns empty Nyx panel ports at the original section
positions. Each independent document owns its actual scroll root and descendants. The
dedicated physical host belongs to the section owner and outlives its renderer;
side views retire before the Chrome ports. No borrowed model children are
attached to a synthetic root.

`ShellView.Root` is a **lookup forest**, not a `TNyxNode`. `Root.Find` returns the
exact mounted node; `RootFor(ID)` returns its real owning root, or nil when absent.
`ViewFor(ID)` returns its renderer and raises when unmounted. Compound commands,
draft capture and restoration use the owning root. Borrowed roots, controls and
renderers expire when their section is replaced; hosts must not free them or call
the managed side renderer's Render, Unmount or MoveHost directly.

`Events` means the mounted Chrome router. Looking it up before mounting raises;
early registrations must not silently disappear into an unused router. Use
`ViewFor(ID).Events` for a specific control and `SectionEvents(Role)` for a closed
section role. Missing compact sections return nil, allowing their consumers to
disconnect. The hierarchy binds to its actual Inspector router. The drag broker
connects the complete set of real routers/roots in one batch; sequential calls
to its former single-view overload would replace earlier registrations.

The stock **Drag selected** button carries a stable selected-control intent.
Drag start resolves the current selection into a typed control reference, then
the ordinary exact-pair lease keeps that identity through hover/drop. Selection
refresh cancels an existing lease. The button does not need a different callback
contract whenever the selection changes. It remains declared for an immovable
root, with typed visibility/enabled state, so selecting the first child does not
introduce a new toolbar node. Explicit fixed-control drag sources remain supported.

The section context default is `nscComplete`: keep all creator defaults, recipes
and resources. Stock Studio explicitly chooses `nscEditorOwnedHierarchy`, which
omits its own hierarchy collection from sections that do not bind it. Custom
composers whose callbacks or recipes use that collection indirectly must retain
the complete context. This policy is not inferred for arbitrary application
collections. The optional configurator is borrowed and runs before **every**
fresh renderer, including replacements; it must obey ordinary factory/lifetime
rules and must not pump queues. A supplied theme is also borrowed until the
section owner and prepared changes retire.

Compatible refresh keeps real controls and their runtime properties. Incompatible
Project, Inspector, Resources and Details sections prepare hidden replacements
and publish as one group.
A new activity row changes only the independently owned Details descendants;
its candidate can replace that section while Resources retains its live editor
input, Unicode draft, physical caret/focus and event scope. Details is the
existing stable root within the composed Design area, including while collapsed.
Compact navigation can omit that area; returning to Design realizes its latest
copied activity. Growing activity does not change section membership or
manufacture extra empty activity rows.
A changed Chrome structure or compact membership uses a fully admitted hidden
frame. Before a full native frame retirement, Canvas and source views park; a
failed admission restores both exact prior hosts. The source pane physically
contains a separately owned CodeView: native Studio captures its input before
parking, then replays its supported copied text/focus/range after the outer pane
returns. Interacting previews also capture runtime input; design canvas bindings
remain inactive and only their exact host is restored. Every touched owner gets
one recovery attempt, and additional failures are reported. Allocation retains
local host ownership until return, so refused insertion cannot leak an unassigned
native panel or leave an attached browser proposal. Retained Chrome keeps those
ports in place. A resource form whose selected binding target changes may require
a new event scope: preserving its draft does not authorize retaining a stale
target contract.

Complete `Render` defaults to discarding prior physical input. Its optional
`TNyxStudioSectionSet` declares precisely which roles may transfer supported
interaction state. The caller must establish the same semantic owner before
opting in: an identical control ID does not identify the same project or
resource. Stock browser/native Studio qualifies its session/load with
`MatchesCommandContext`, then uses `NyxStudioResourceContinuity` to compare the
public resource proposal contexts. That helper permits only Resources, only
when both complete forms describe the same accepted catalog, resource/locale
and exact local/effective owner-binding contract. `TNyxResourceEditorDraft.SameContext`
compares ownership independently of unfinished field values. Absent/incomplete
forms or changed contexts refuse without carrying text into another owner.

The complete view owner captures immutable adapter observations before creating
its hidden candidate. Opted-in roles receive text/ranges/scroll before revelation
and focus afterward; their field domain, bindings and accepted value still
qualify restoration. Only then does the candidate replace the old frame and
retire its event scopes. Resolve new controls afterward: copied continuity does
not preserve widget pointers. Candidate admission failure destroys the candidate
and restores all prior mounted roles, independently of the forward opt-in set.
Recovery attempts each role once and reports a recovery refusal. This operation
does not edit document/history or emit an input command.

Retained changes made before a grouped refusal replay their previous source
baseline and restore the captured runtime property/resource projection. Recovery
then restores copied physical continuity: an uncommitted numeric draft can differ
from the restored accepted model value. Both ordinary renderers expose typed
`CaptureInteraction` / `RestoreInteraction` operations using
`TNyxContentFaceStates`. These copies contain text, Unicode scalar selections,
logical identity, domain/binding context and target scroll/focus observations;
they retain no view, model, widget or store. An idle mounted view is required.
Ambiguous copied identities raise before touching any face. Changed contracts or
accepted values do not admit a stale draft; restoration never edits history or
dispatches an input command. Browser textarea content scroll and native control
scroll boxes use their adapter observations; native memo scroll, hardware/IME
composition and assistive technology are not qualified by this gate. Native
TCustomEdit faces are covered; composite date/time/color editor draft bridges
remain open under the Studio authoring owner.

Recovery attempts every touched retained role once, including a retained refresh
whose target synchronization raised, and reports failures. Source/model replay
precedes physical continuity. A creator's custom editor requires its own adapter
continuity contract; an extension unable to restore its physical state cannot
promise atomic rollback. The actual shared Studio shell/facade now qualifies a
later Inspector preparation refusal and a second physical publication refusal
after Project preview, with original roots, drafts, ranges and callback tokens
preserved, followed by successful grouped refresh. This is distinct from the
accepted public section-publication fixture. The complete-frame journey also
qualifies a later Inspector factory refusal, exact prior roots/drafts/focus,
explicit forward continuity, safe default discard and retired/new callback scopes.
Ordinary Resources consumers exercise a real Pascal split/mount change and
different-resource refusal. The maintained target includes 15 actual native
controller checks for nested source recovery after an embedding-host refusal,
with exact text/pair/history and event scopes. Shared 53-check native/browser
journeys additionally qualify unassigned host insertion and cleanup. These do
not establish ordinary browser nested-source recovery, runtime-preview faults,
full publication rollback after shell admission, wider custom/composite editors
or reveal/focus extension failures; those remain separate gates.

Browser refresh queues past borrowed shell, live input and designer callbacks.
Multiple requests coalesce, with explicit reset choices taking precedence. The
timer is canceled on destruction and reports asynchronous refresh refusal through
the existing status control. Design canvases leave application bindings inactive;
their callback depth is tracked separately from live-store publication readiness.
`PresentationPending` and `SourceBusy` are distinct observations, neither a
compiler-success result. Native continues to use its ordinary queued paint path.

The current work packet owns executed evidence and remaining limits. Compilation,
synthetic input and a retained control do not establish production latency,
physical phone input, accessibility, another widgetset or observing deployment.
