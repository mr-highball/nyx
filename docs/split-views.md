# Split views and platform configuration

[Fluent API](fluent-api.md) · [Components](components.md) · [Events](events.md)

`INyxSplitView`, `TNyxSplitView` and `NewNyxSplitView` describe a workspace with
two independently owned child panes. Both adapters resize the mounted controls
in place; resizing does not replace text editors or their selection.

```pascal
var
  LWorkspace: INyxSplitView;
begin
  LWorkspace := NewNyxSplitView('editor-workspace');
  LWorkspace.Configure
    .SplitOrientation(nsoStacked)
    .SplitPosition(65)
    .SplitMinimum(10)
    .SplitMaximum(90)
    .Done;
  LWorkspace.Configure.ForPlatform(npfNativeLCL)
    .SplitOrientation(nsoSideBySide)
    .Done;
  LWorkspace.Add(LPreview);
  LWorkspace.Add(LSourceEditor);
end;
```

The first child occupies the selected percentage of the space remaining after
the divider. `nsoStacked` places it above the second; `nsoSideBySide` places it
beside the second. Defaults are 65 percent, bounds of 15–85 percent and resizing
enabled. Bounds are integers from 0 to 100 and must contain the position. There
may be zero, one or two children; a missing child leaves an empty pane. A third
child is rejected. A split without an explicit height or flex allocation uses
320 logical pixels; use `Height` or `Flex` to allocate workspace space.

An explicit child `.MinimumHeight(280)` constrains a stacked pane; side-by-side
panes use `.MinimumWidth`. The adapters copy the realized platform/viewport
configuration, so a caller can tune these through the same fluent scopes as
other layout settings. Passive resizing preserves the requested percentage.
When enough space returns, the original proportion returns too. The separator's
accessible value describes its physical allocation; `TNyxSplitState.Position`
retains the preference and `EffectivePosition` describes the realized percentage.

If both minima cannot fit, split-owned tracks compress in proportion to those
minima and consume the available axis exactly. This is the bounded allocation
policy of a split, rather than a change to ordinary layout minimum constraints.
Pane contents may need their own scrolling at such sizes. The 44-pixel divider
shrinks when the whole host is smaller. Nested browser splits observe their own
host allocation; native splits follow ordinary LCL resize. Neither path replaces
mounted children or writes a passive resize into document history.

The divider has a 44-pixel target, reduced only when the entire allocation is
smaller. Drag it with a pointer or touch. Arrow keys follow the split orientation,
Shift changes ten percentage points, and Home/End select the bounds. Escape,
pointer cancellation and capture loss restore the gesture's starting position.
At a constrained edge, arrows and dragging start from the visible divider.
Outward movement that changes no pixels does not accumulate a hidden preference
or emit a completed resize. Cancellation restores the original requested value,
including a proportion that was temporarily constrained by pane minima.
Disabled, read-only or non-resizable splits reject resize gestures. A completed
change emits `OnChange` with an owned integer percentage; normal multiple
registrations and execution policies apply.

The divider also publishes the complete typed focus and keyboard family.
Sequential before/main key hooks run before the platform resizing default and
may consume it; after hooks observe Nyx dispatch. `FocusFor` borrows the grip
without declaring it a scalar input. Fixed and read-only dividers remain
keyboard-inspectable and deliver notifications while refusing resize commands.
Disabled dividers leave the Tab order. Navigation detaches their producers before
the borrowed view is released.

`Configure.ForPlatform` returns an independent configuration scope. `npfAny`
addresses portable defaults; `npfBrowser` and `npfNativeLCL` address overrides.
Retaining an override scope does not change a previously retained default scope.
The actual renderer selects its platform at runtime, after creating an
independent realized view. It never changes the authored document or another
renderer's view.

Overrides support typed presentation and interaction settings, including layout,
dimensions, captions, visibility, enablement and split configuration. Identity,
ownership, state values, bindings, actions and event registrations retain their
shared meaning and cannot be overridden through this scope. Unsupported scope
operations fail explicitly. `Clear(atHeight)`, for example, can remove a default
fixed height for a flex-allocated browser workspace.

Persistence and clones retain the scopes. Studio emits typed `.ForPlatform`
blocks and reconstructs them through its source workspace; the Inspector exposes
present overrides with their ordinary typed validation. The `@nyx.` wire
namespace belongs to persistence, rather than handwritten configuration.

Studio uses this public split for its design canvas and Pascal workspace. Its
chosen proportion survives panel changes, show/hide, viewport changes and ordinary
shell refreshes within the running session. A drag retains pending source drafts,
caret, focus and pane scrolling. It is an editor preference and creates no design
history entry. The shared shell also renders with actual LCL split controls;
the complete native Studio controller remains under development.
