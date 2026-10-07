# Available host space

[Responsive authoring](responsive.md) · [Split views](split-views.md) ·
[Current evidence](../WORK.md#current-return-path-available-host-space--2026-10-07)

`nyx.hostspace` supplies a portable host observation and immutable fitting policy.
Layout dimensions, visual height and visual magnification remain distinct.
It owns values and listener lifetime, without owning a document, renderer or
control. Ordinary responsive conditions still consume logical width/height.

```pascal
LPolicy := NyxHostSizing.Fit(nhfAvailableHeight);
LAllocation := LObservation.Snapshot.Resolve(LPolicy);

if TNyxViewportCondition.Any.HeightBelow(500).Matches(
  LAllocation.Width, LAllocation.Height) then
begin
  // Present compact chrome using the same public Nyx composition.
end;
```

`nhfLayout` keeps the allocated client rectangle and is the default.
`nhfAvailableHeight` retains layout width and fits the smaller of layout height
and visual height multiplied by visual scale. Removing magnification from that
measurement keeps ordinary pinch zoom from becoming a narrow presentation or
smaller font. Allocation rounds to logical pixels; a one-pixel rounding change
is possible. Page zoom already changes layout dimensions. No keyboard detection
or browser panning override is attempted.

The browser adapter uses `NewNyxBrowserHostSpace(LPolicy)` and listens to both
window and VisualViewport resize. It changes no global DOM styles; absent or
inactive visual metrics fall back to layout dimensions. The LCL adapter uses
`NewNyxLCLHostSpace(LHost, LPolicy)` and additional LCL resize handlers, retaining
the caller's existing `OnResize`. LCL reports its allocated client rectangle;
it does not claim a separate native keyboard-occlusion measurement.

`INyxHostSpace.Extent` gives the current resolved allocation. `Snapshot` is a
copied observation for other policies. `Refresh` atomically admits new metrics;
failure retains the previous accepted value. Equal allocations are silent.
`OnChange` borrows its method receiver. Clear it or call `Disconnect` before
retiring that receiver. Disconnect is idempotent; the last copied value remains
readable. A callback may disconnect/release its own observation. Native host
destruction cancels its observation through component notification.

Ordinary Studio consumes the managed observation on both targets. The browser
sets the editor's external mount allocation through a CSS variable, which its
renderer cannot clear while synchronizing authored node metrics. Same-mode
height changes resize in place; only a compact-mode crossing uses the existing
draft/focus-retaining shell transition. Application fonts, preview scale and
document/history are unaffected.

The public modal contract also offers
`NyxModal('Pascal source').Sizing(nhfAvailableHeight)`. The browser modal fits and
centers its existing dialog within the available height, without remounting its
Nyx content. Default layout policy remains available; native modal geometry uses
the same owning client space. Studio's expanded source explicitly opts in.

Run `tools/build.ps1 -Target host-space` for checked shared/actual Win32 geometry
and lifetime checks. It stages the matching Pascal browser consumer and RTL at
`build/host-space/maintained/browser/host-space.html`, without launching a service
or browser. The current packet passes 47 native/shared checks and the actual
source workspace passes 30 with zero leaks. Browser fixtures and both Studios
compile with zero owned warnings. Actual browser viewport events, phone keyboard/
IME, zoom gestures and assistive input remain unqualified; compilation and
supplied metric observations do not establish those behaviors. No observing
LAN rollout is claimed.

The visual/layout distinction and magnification follow the
[CSSOM View draft](https://www.w3.org/TR/cssom-view-1/#the-visualviewport-interface).
Chrome documents keyboard modes that resize the visual viewport independently
of the layout viewport in its
[viewport resize explanation](https://developer.chrome.com/blog/viewport-resize-behavior/).
This policy does not recover occlusion from an overlay mode that reports no
viewport resize.
