# Retained tabs

[Components](components.md) · [Public contract](../src/nyx.tabs.pas) ·
[Semantic example](../examples/tabs/README.md)

`NewNyxTabs` and `NewNyxTab` return the specialized, reference-counted `INyxTabs`
and `INyxTab` interfaces. Each direct child of a tabs control must project as a
tab page; its descendants can be ordinary controls, reusable content or nested
tabs. The document owns the tree. The mounted browser/LCL host owns copied
selection/focus state and retains all page controls until that view retires.
Switching a tab hides its page rather than reconstructing it, retaining unfinished
input drafts and control identity.
Reusable initial references resolve only within their own direct-child source
scope after runtime qualification. Separate instances never share selection.

```pascal
uses
  nyx.controls, nyx.tabs, nyx.types;

LProjectTabs := NewNyxTabs('project-tabs');
LPlanTab := NewNyxTab('plan');
LDetailsTab := NewNyxTab('details');

LPlanTab.Configure.Text('Plan').Done;
LDetailsTab.Configure.Text('Details').Done;
LProjectTabs.Add(LPlanTab).Add(LDetailsTab);

SetNyxTabsOptions(LProjectTabs,
  TNyxTabsOptions.Defaults
    .Initial(NyxControl('details'))
    .Activation(ntaManual)
    .Orientation(ntoVertical)
    .Wrap(False));
```

Use explicit `TNyxTabsOptions.Defaults`: the default is first available page,
horizontal headers, automatic activation and wrapping. `.Initial` names a direct
child through a distinct control reference, independently of caption and order.
Manual activation moves focus without selecting; Enter/Space explicitly selects.
Hidden headers disappear, while disabled headers remain descriptive. Movement
skips both restrictions. No available page has focus/selection index `-1`.
Malformed known policy, non-tab children, duplicate identities, invalid Boolean
restrictions and unavailable initial intent refuse admission before rendering.
The snapshot limit is 4096 pages; it is a safety bound, not a performance claim.

The browser uses `tablist`, `tab` and `tabpanel` roles, linked IDs, selected state,
one header Tab entry and orientation-aware arrows, Home/End and manual activation.
It leaves unrelated vertical arrows to ordinary browser scrolling for horizontal
tabs. A leading heading/nonfocusable face gives its panel a separate Tab entry.
This follows the [WAI tabs pattern](https://www.w3.org/WAI/ARIA/apg/patterns/tabs/).
The native adapter composes LCL buttons, a scrolling header band, `TPageControl`
and retained `TTabSheet` hosts. Its separate header focus supports manual activation.
Both adapters apply the root's live Enabled restriction.

An actual selection change emits `ntChange` after presentation, with the exact
selected child identity in the owned Text value and `Changed=True`. An unchanged
activation emits no selection notification. Ordinary ordered event registrations,
execution policies, explicit subscription cancellation and view retirement use
the existing public `INyxEvents` contract. The portable state invokes no callback
and retains no source document, renderer, widget or subscriber. Focus entry can
retire a host; a separate cancellation lease guards subsequent adapter work.

Authoring policy uses one strict version-one `nyx.tabs` persistence extension;
wire strings remain inside the codec. Generated Pascal imports the public unit
only when needed and emits multiline typed options beside specialized variables.
Actual compiled builders reproduce the exact configured document on both targets.
The ordinary semantic `value` field does not select a page; schema reports that
application-provided behavior as Custom. Runtime selection does not rewrite the
authored options or document.

Current qualification covers shared policy/admission, exact generated execution,
actual browser/Win32 LCL controls, managed callbacks, draft retention and trusted
Chromium pointer/keyboard at desktop/CSS-390. Native diagnostic Print captures
are separate from physical display evidence. Hardware, assistive technology,
IME, another widgetset and complete visual/DPI acceptance are not established.

Policy/captions/child availability are mount-time snapshots: remount to adopt
source edits. Live selected-page state binding, tab insertion/removal/reordering,
closable pages, overflow focus reveal, customizable header parts, comprehensive
Nyx keyboard hooks and ordinary Studio typed-policy/source-reading authoring
retain the existing component/parity/codegen/workflow owners. Both catalog kinds
remain Basic. The installed observing Studio retains its previous adapters; the
new standalone example is served independently without replacing the protected
service or project. See the current evidence packet in [WORK.md](../WORK.md).
