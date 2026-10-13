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
Reusable references can declare a local typed policy before realization;
complete document admission checks the selected recipe's tabs projection.
Removing the declaration restores the definition's policy independently of
other instances. An explicit Defaults declaration remains distinct.

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

Application state can also select a page. A selection value names an exact local
child, independently of caption, order and runtime-qualified reusable IDs. Use
the distinct reference helper at the text-state boundary:
include `nyx.state` and `nyx.binding.types` in the application's uses list.

```pascal
LDocument.State.SetValue(NyxTextState('selected-page'),
  NyxTabValue(NyxControl('details')));
LProjectTabs.Binds.Value(NyxTextState('selected-page'), bdTwoWay).Done;

// Without a binding, author an explicit initial semantic value instead.
SetNyxTabSelection(LProjectTabs, NyxControl('details'));
```

The empty value follows the policy's initial page. Missing, hidden, disabled or
ambiguous references refuse the complete candidate before publication. A user
selection proposes a transition before changing indices or retained panels;
read-only/from-state bindings and application validators can refuse it. Accepted
input publishes one bound update, then ordered callbacks observe its local value
and presented page. Programmatic synchronization emits no user change callback.
Failed post-commit notification reports committed state without retry/rollback.
Rejected automatic focus returns to the accepted header, and unrelated unfinished
input remains intact. Captions/availability and selection can publish together in
one batch; disabling the selected page requires a valid accompanying selection.
Source defaults and independent sibling stores stay unchanged.

Generated selection properties use `NyxTabValue(NyxControl(...))`; the ordinary
source reader requires that distinct reference and refuses raw text substitutes.
The existing state inspector/semantic binding operation consumes this current
contract. The installed observing schema still refuses tabs Value bindings;
that refusal is retained separately from current-source execution evidence.

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

Studio's Properties inspector consumes the public reusable `NewNyxTabsEditor`
card. Initial tab, activation, direction and wrapping use specialized Nyx select/
checkbox parts. Apply captures one complete proposal; Use inherited behavior
removes only an existing local declaration. The copied local/effective/child
baseline is checked again by the ordinary paired worker before publication.
Form drafts survive unrelated refreshes and Properties/Events switches while
their exact owner/baseline remains valid. Project replacement retires them.
Source reading admits the same typed grammar and rejects string substitutes
for enums, Boolean values or distinct control references.

Current-source agents request `nyx_node` with `tabs=true`, `tabOffset` and
`tabLimit` (maximum 50). The response contains local/effective policy and only
the requested direct child source identities/availability. `nyx_transaction`
accepts compact `tabs-set` and `tabs-inherit` operations at the expected revision;
one related group shares paired Undo/Redo. Internal form intent carries its
full stale-capture baseline; semantic operations rely on the document revision.
These schema additions await a separately qualified observing-release rollout.

The current Properties inspector also contains a public `NewNyxTabPagesEditor`
compound from `nyx.tabs.pages.editor`. It uses specialized selects, inputs,
checkboxes, buttons and a wrapping action row. Add chooses an unused page
identity; Rename preserves identity and contents; Move earlier/later preserves
the complete owned page. Every accepted edit updates design/Pascal as one paired
Undo step. A changed descendant invalidates a captured operation and its form
draft, so later work cannot be removed through stale confirmation.

Removal requires the visible acknowledgement and deletes only that exact owned
page and its descendants. Initial selection, bound defaults and modeled reference
dependencies must be resolved first; refusal preserves them instead of choosing
a replacement. Bound captions require editing the text binding/default explicitly.
Selecting another page prefills its authored caption and clears removal consent;
duplicate captions retain distinct choices. Incomplete caption/create input parks
across shell refreshes while the exact owner/subtree/choices still match.

Reusable instances expose an **Edit reusable tab pages** action which targets
their exact definition. Structural commands against the instance refuse. Editing
the definition changes its source for all instances; local policy overrides remain
independent. Inputs are named parts; command buttons are under `actions/add`,
`actions/rename`, `actions/earlier`, `actions/later` and `actions/remove`.
Hosts capture `TNyxTabPageChange`, then use `NyxTabPagePatch` in the ordinary paired
queue. Agents use existing compact create/update/move/delete tools at an expected
revision; no full form/subtree response is needed. Current source and frozen
observing Studio remain separate deliveries.

Current qualification covers shared policy/admission, exact generated execution,
actual browser/Win32 LCL controls, managed callbacks, draft retention and trusted
Chromium pointer/keyboard at desktop/CSS-390. Native diagnostic Print captures
are separate from physical display evidence. Hardware, assistive technology,
IME, another widgetset and complete visual/DPI acceptance are not established.

Policy and structure remain mount-time snapshots: remount to adopt those edits.
Tab insertion/removal/reordering, closable pages, overflow focus reveal,
customizable header parts, comprehensive
Nyx keyboard hooks and broader connected/native Studio authoring integration
retain the existing component/parity/codegen/workflow owners. Both catalog kinds
remain Basic. The installed observing Studio retains its previous adapters; the
new standalone example is served independently without replacing the protected
service or project. See the current evidence packet in [WORK.md](../WORK.md).
