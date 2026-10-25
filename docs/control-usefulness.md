# Control usefulness: what application users can do

[Component outcome](../TODO/NS-3_components_01.md) ·
[Review task and follow-ups](../TODO/DONE/NS-3_control-usefulness_01.md) ·
[Typed catalog reference](components-reference.md) · [Capabilities](capabilities.md)

Current status on2026-10-22: [table filtering/sorting02](../TODO/DONE/NS-3_control-usefulness_02.md)
is accepted through actual native/browser controls, Studio/history/source and its
own independently compiled application. [WORK](../WORK.md#table-filtering-and-sorting-accepted)
owns current evidence and target limits. The80-kind rows below preserve the
2026-10-21 source-review baseline, including its then-missing header/filter UI;
those historical gap labels are superseded by02's dated result. Column arrangement/
inspection/copy remain with03 and live operational data with04.

## Scope and evidence

This review is bound to `544ca92ba393c299759e1237a180d70432e0a592`, its unchanged
1795 Pascal inputs, and generated reference
`191E0AACB1923F0ADD5970FBFD60C0C61F97A8F676DEDD4109E92973F217A351`.
The reference contains **80 default kinds: 43 primitives and 37 recipes**.
`SlotOverride` is an authoring descriptor, covered with Component below; it is
not an omitted 81st palette control. Custom registrations remain extensible.

The question is whether someone using an application can accomplish a useful
task through its controls. A declared property, an inherited widget feature,
a static recipe and an integrated workflow are different evidence. Source
inspection here starts no compiler, renderer, browser, server or external probe.
The external process/service debugger is unavailable, so this does not diagnose
that application's code or blame its author.

Root reviews 35 kinds and Lil' Scooty reviews 45 disjoint kinds. The complete
index is checked against the actual reference, including positive findings:
80 unique rows, no missing/extra/duplicate kinds. Both primary reviews and
independent cross-review pass the six unchanged review-task criteria.
The [work record](../WORK.md#control-usefulness-review--2026-10-21) records review
and exact source boundaries. This review adds no product implementation credit.

## Follow-up backlog and current progress

The matrix below remains the historical review of checkpoint544ca92. Its gap
labels are starting evidence, rather than a claim that subsequent work is absent.
The user's process/service-debugger report is covered by these existing owners:

| User outcome | Backlog owner and current boundary |
| --- | --- |
| Find records with column filters, numeric sorting, visible criteria and reset | [Column queries02](../TODO/DONE/NS-3_control-usefulness_02.md): separate native-local results cover P3 workbench782/782 and P4A Studio policy/history266/267, with clean heaps at their own source boundaries. Browser assertions compile only. Events/semantic/source agreement, OWN Save/Open/generated and actual browser interaction remain open. |
| Arrange columns, inspect long values and copy/export useful selections | [Table workbench03](../TODO/NS-3_control-usefulness_03.md): W1 owns resize/choose/reorder/hide/pin/reset; W2 owns full-value/cell actions, copy/export and preference lifetime. Both remain open. |
| Keep context while services/logs change and recognize paused/failed/empty states | [Live inspection04](../TODO/NS-3_control-usefulness_04.md): L1 owns follow/pause, context and recovery; L2 owns measured mounted responsiveness. Both remain open. |
| Find a nested process in a List/Tree and understand matches, ancestor context and hidden selection | [Live inspection04](../TODO/NS-3_control-usefulness_04.md) owns context; existing [DataToolbar search](../TODO/NS-3_data-toolbar_02.md) targets List/Table, while Tree find/reveal stays with components01/hierarchy and04. Typeahead and membership filtering are distinct; Table-only workbench behavior is not inferred. |
| Preserve explicit query choices during unrelated lane reconfiguration | [Kanban preservation05](../TODO/DONE/NS-3_control-usefulness_05.md): accepted 2026-10-25 with native editor/candidate 197/170 and clean heaps; matching browser assertions compile. Retains explicit declarations and absent defaults without adding a lane filter UI. |

The 2026-10-22 backlog refinement adds explicit user journeys to the existing
tasks, preserving this historical80-kind baseline. Task02 now includes an
application-author diagnosis checklist (binding shape, effective opt-outs,
supported field family and build) and a discoverable native filter story.
Task03 requires editable start/end pins, compact recovery and deliberate
raw/display/draft and selected/page/full-result copy/export choices. Task04
adds the cross-family search/context and independent paused/live-view checks.
The external debugger's code remains unavailable; these checks do not attribute
its missing controls to Nyx or its author. No new runtime verdict follows.

Task02 retains its first full P4A native60s Ready failure and old partial heaps.
After the narrow pending-presentation repair, two fresh same-executable runs now
qualify266 desktop/267 compact authoring/history checks with complete zero-unfreed
heaps. The browser-only callback collision is repaired and matching compilation
passes. [The current work record](../WORK.md#table-query-policy-authoring--2026-10-21)
keeps the source/reuse boundaries explicit. Complete authoring/browser/product
acceptance and release-performance validation remain open.

The remaining control families keep the concrete owners and finish lines in
[Priorities](#priorities-public-examples-and-finish-lines), including forms,
choices, editing, media, navigation, feedback, splitters and canvas controls.
This preserves one review across all80 kinds without duplicating table tasks.
The external debugger's implementation has not been inspected; a caller-specific
diagnosis remains separate from the public library review.

## Reading the matrix

Each row identifies a user goal, its existing public contract, ordinary native
and browser behavior, event/authoring profiles and a concrete next outcome.
Statuses qualify the named interaction, never the whole control:

| Status | Meaning |
| --- | --- |
| Supported | The standard source implements the described ordinary interaction. Execution evidence is identified separately. |
| Opt-in | A caller must deliberately attach/configure an existing public capability. |
| Implemented-unexposed | A shared/runtime capability exists but lacks the described standard user command surface. |
| Target gap | A named effect is missing or different on a standard target projection. |
| Absent | No public implementation of the named workflow was found in the inspected contract/adapter. |
| N/A | The action is inappropriate to that passive control, rather than a missing feature. |

`N/B` means native LCL/browser. `S` means inspected current source; `H` names a
historical executed receipt at its original revision. Neither means every
widgetset, hardware gesture, assistive client, DPI or configuration was tested.
The absence of a new run is explicit for every row: **all new matrix judgments
are S**. Historical receipts are useful regression starting points, not a
current-source execution sweep of 80 kinds. P1 means a frequent blocked user
workflow; P2 means important depth/predictability; P3 means secondary convenience.
These are ordering judgments, not defect severity or completion percentages.

## Shared public, event and authoring profiles

Every `INyxX` below has a managed `NewNyxX` constructor and matching `TNyxX`
implementation in [the specialized declarations](../src/nyx.controls.types.inc).
The [common interfaces](../src/nyx.controls.pas) retain independent configuration,
bindings, contracts, content and extensions. `Part(NyxPart(...))` and specialized
part accessors retain managed controls; a raw descriptor bridge is borrowed.
Custom implementations can implement the interfaces without inheriting a Nyx
implementation class. This is an existing foundation to preserve.

| Profile | Existing configuration, interaction and limits |
| --- | --- |
| L | `INyxLayoutControl`; typed `Configure.Layout`, wrapping/alignment/justification, dimensions/bounds, Gap/Padding/Flex and `WhenViewport`/`ForPlatform`. Passive layout does not itself move application records or navigate pages. See [layout](layout.md), [responsive](responsive.md), [presentations](../src/nyx.presentations.pas). |
| C | `INyxCaptionControl.Text`, typed variants, typography, accessible naming and text/resource bindings. Text is display content, independent of field Value. Native inherited selection/copy is available only on a face which supplies it, not every Label. |
| M | `INyxImageControl.Image`, fit/anchors/alternative text, `TNyxImageSource`, and distinct resource-image binding. Standard image projection is required: sharing the interface does not make Avatar paint a picture. [Images](images.md) and [resources](resources.md) define loading, locale and cache policy. |
| E0 | Shared pointer/context/wheel notifications where metadata admits them. No scalar Value is promised by a passive caption or layout. Do not make an unbound Click into an application action automatically. |
| EK | Focusable faces add before/main/after key-down, logical key-press and key-up plus after-enter/exit. Key snapshots carry typed keys/modifiers; sequential synchronous consumption can prevent a default. An after hook observes Nyx dispatch, not completion of arbitrary asynchronous platform work. |
| ET | Text admission and editable draft lifecycle; callback `HasValue`/`ValueKind`/Value distinguish an empty accepted value from no value. Focus/key hooks remain available when read-only; edits refuse. |
| EA | Ordinary Button activation plus typed semantic action references. SourceID/OriginID/TargetID/ValueID and optional copied scalar payload describe actual routes. A semantic name such as Reply, Undo or Save does not implement a backend service. |
| EI | Image Loading/Ready/Error/Cleared carry `HasImage` and an immutable request/source/phase/dimensions/failure snapshot. Ready is decode acceptance. Replaced/cleared/retired generations suppress stale delivery. These observations cannot consume. |
| A0 | Ordinary schema Properties/Events, descriptions/target grades, multiple ordered callback registrations, callback TODO/source navigation, closed typed configuration, persistence, paired history and generated Pascal/source reading. Unsupported projection fields remain explained, not silently promised. [Capability](capabilities.md) and [event](events.md) discovery supply this path. |
| AP | Named parts are ordinary independently configurable controls; instance overrides/masks and source/history use existing reusable ownership. Hiding/removing a part can remove an action route; discovery must be queried in the effective instance/document context. |
| AR | Runtime service/attachment is explicit host code. The saved content/configuration is authored through A0/AP; it does not serialize service objects, callback implementations, authentication, an open window, active selection or a live network request. |

All event profiles preserve ordered multiple registrations, exact removal,
per-event execution policy and the existing scheduler. Only synchronous
cancellable stages can veto their documented default/publication. Background
callbacks use the UI queue for UI access; replacement/disposal revokes former
authority. `TNyxEventInfo.Copy` owns scalar/identity snapshots. Rich `Details`
metadata has its own discovery API; it is not interchangeable with the scalar
Value contract. See [events](events.md#event-data-and-lifetime) and
[Details discovery](../src/nyx.schema.pas).

All rows inherit enabled/visible/read-only protection. Display-only controls have
no user-edit command to disable; an application can still update their bound
content. Empty captions/containers are valid, missing resources and invalid
values follow their respective admission/runtime contracts, and ordinary
placement should preserve readable text rather than shrink the UI to fit.
Specific deviations/recovery needs are called out below.

## Source and historical evidence map

| Key | Current source to inspect | Existing evidence and remaining verification |
| --- | --- | --- |
| Core | [specialized types](../src/nyx.controls.types.inc), [facades](../src/nyx.controls.facades.inc), [schema](../src/nyx.schema.pas), [recipes](../src/nyx.recipes.pas) | Reference191E confirms default population/metadata only. All ordinary user workflows still need their own target evidence. |
| Layout | LCL `CreateControl`, `MeasureHeight`, layout/Sync in [renderer](../src/nyx.render.lcl.pas); browser `CreateElement`/Sync in [renderer](../src/nyx.render.browser.pas) | Existing layout/constraint/property journeys in [layout guide](layout.md) and WORK cover their exercised coordinates, retention and source. All metric/visual/accessibility combinations remain open. |
| Split | [portable](../src/nyx.split.pas), [LCL](../src/nyx.split.lcl.pas), [browser](../src/nyx.split.browser.pas) | Historical [split acceptance](../TODO/DONE/NS-2_split-platform_01.md) coexists with the unreplicated [snap-back report](../TODO/NS-2_split-platform_02.md). Never erase the latter using old green evidence. |
| Disclosure | [contract](../src/nyx.disclosure.pas), both renderer coordinator paths | [Disclosure evidence](disclosure.md) includes actual retained drafts, ordered focus/reentry/disposal and trusted browser keys at wide/compact widths. It does not qualify all native hardware/assistive clients. |
| Media | [image contract](../src/nyx.images.pas), [native image](../src/nyx.images.lcl.pas), [browser requests](../src/nyx.images.browser.pas), renderer SyncPicture/SyncImage, [lifecycle](../src/nyx.image.lifecycle.pas) | [Images](images.md) records native199 and actual browser231/231 scoped admission/decoding/presentation work; resource resolution has its own [accepted owner](../TODO/DONE/NS-1_resources_01.md). Do not count these as Avatar portrait or all current media execution. |
| Feedback | [recipes](../src/nyx.recipes.pas#L313), [behavior](../src/nyx.behavior.pas), [confirmation](../src/nyx.confirmation.pas), [progress](progress.md) | [Confirmation](confirmation.md) and progress guides retain target-specific input/lifetime limits. Inline confirmation is not proof of managed modality; emitted source is not proof of executing an Undo service. |
| Canvas | [workspace](../src/nyx.canvas.workspace.pas), [declarations](../src/nyx.canvas.declarations.pas), [drawing](../src/nyx.drawing.pas), [tool](../src/nyx.tool.canvas.pas), [adapters](../src/nyx.canvas.lcl.pas) | Current [canvas](canvas.md), [diagram](diagrams.md) and [Logic](logic.md) guides track actual features; the earlier canvas-workspace roadmap is historical. Native133/190 and browser133/202 corrected-capture boundaries, later native precision/layout and compiled-only browser evidence remain distinct. |
| Reuse | [content](../src/nyx.content.pas), common creator/binding paths in both renderers | Accepted [reuse05](../TODO/DONE/NS-3_coordinators_05.md): native588/588, older-adapter browser563/563 and separately scoped current Studio/compiled pairs. Parent/parity/DataToolbar remain open; full method/body impact review bounds reuse. |
| Chrome | [typed button face](../src/nyx.button.presentation.pas), [managed facet](../src/nyx.button.configuration.pas) | Facelift5DBE native161/67, chrome48/52 and actual browser styles/history/36-event **discovery** support their specific paths. Discovery of36 is not firing36 events. |

## Layout and retained presentation: 11 kinds

All rows use A0; named compositions also use AP. Noninteractive layout has E0,
not a hidden text-edit or data-selection value. Reproduction uses each named
`NewNyxX` plus its existing Configure methods and ordinary child controls.

| Kind and user goal | Existing typed contract | N/B ordinary behavior (S), state/input boundary | Next useful outcome and owner |
| --- | --- | --- | --- |
| `page` — reach a complete application view | `INyxPage`, L; application host ShowPage/presentation selection | Supported/Supported layout. Page switching is Opt-in through host navigation, not a click on the background. Empty page valid; bindings/owned view generation govern replacement. E0, A0/AR. | No separate Page defect established. P2: full navigation/focus restoration and adaptive workflows stay with [parity](../TODO/NS-2_parity-accessibility_01.md) and [authoring](../TODO/NS-4_studio-authoring_01.md). |
| `column` — read a vertical flow | `INyxColumn`, L; positive Flex sharing requires definite height | Supported/Supported flow, min/max and hidden-slot removal; no scalar edit. Empty retains padding, focus/drafts belong to children. E0. | Preserve as foundation; full mixed intrinsic/DPI metrics stay with renderer/parity owners, rather than add artificial column actions. |
| `row` — use a compact action/form row | `INyxRow`, L; explicit typed Wrap/Align/Justify | Supported/Supported wrap/layout; automatic wrap follows Nyx host, explicit Wrap helps a narrow subpanel. Overflow can be deliberate. Child keyboard order stays authored order. E0/AP. | P2: actual compact/long-caption presentation under [parity](../TODO/NS-2_parity-accessibility_01.md), preserving typography and reachable actions. |
| `grid` — scan arranged content | `INyxGrid`, L, typed Columns | Supported/Supported layout grid. N/A for data-column filter/sort; this is not Table. Empty/hidden cells follow layout admission. E0/AP. | No data-table feature assigned here. Responsive composition/metrics remain renderer/parity work. |
| `panel` — contain an application region | `INyxPanel`, L, Surface/theme/configuration | Supported/Supported general containment. It does not become a modal, dock host or command router solely by its name. E0/AP. | Foundation with no separate defect found. Optional docking is not assumed to be an existing required feature. |
| `card` — read grouped content/actions | `INyxCard`, L, named/custom children | Supported/Supported ordinary themed content; keyboard/actions belong to its parts. Empty card valid; no automatic fetch/error/retry service. E0/AP. | P2: readable bounded content/contrast across themes and compact layouts under [parity](../TODO/NS-2_parity-accessibility_01.md); retain arbitrary added actions. |
| `group` — understand a field grouping | `INyxGroup`, L/C caption | Supported/Supported captioned grouping: native `TGroupBox`, browser fieldset/legend. Group is not Disclosure; no hide/show toggle implied. E0/AP. | Positive foundation. Verify native/browser assistive grouping, long legends and disabled descendants under parity. |
| `scroll` — reach overflowing content | `INyxScroll`, L; viewport/wheel callbacks from real face | Supported/Supported scroll hosts, with target scrollbar/client metrics. Wheel request and actual viewport change are different snapshots; after-scroll completion grades remain target-specific. E0, viewport profile in events guide. | P1 in narrow workspaces: reachable content/focus and no unexpected scroll jumps; existing renderer/parity owners. No new scroll engine needed. |
| `spacer` — separate actions without losing space allocation | `INyxSpacer`, C plus common Configure.Flex | Supported/Supported grow spacer defaults Flex1; indefinite native column has a natural16px height. It has no user-edit/selection workflow; empty text intentional. E0, keyboard N/A. | No new user command needed. Preserve allocation/default-reset behavior under layout owner. |
| `split-view` — give preview/editor more space | `INyxSplitView`/`INyxSplitControl`, typed orientation/position/resizability and bounds | Supported implementations/Supported implementations: retained two panes, pointer/keyboard divider, escape/capture cancellation, integer OnChange; disabled/read-only refuses resize but focus inspection remains. Actual external snap-back remains unverified. EK/A0. | P1: reproduce and repair [split02](../TODO/NS-2_split-platform_02.md), retaining position through reflow/sync, nested bounds and keyboard regression. Explicitly deferred; this review does not investigate it. |
| `disclosure` — reveal advanced options without losing edits | `INyxDisclosure`, Boolean Value/binding, `SetNyxDisclosureOptions` typed header/body policy | Supported/Supported retained body, header toggle, independent ancestor visibility/read-only gate, focus recovery. Closed body retains drafts; an empty body is valid. EA/EK; Boolean change/owned identities, A0/AP. | Positive executed foundation in Disclosure evidence. Full assistive/native hardware and quality remain parity; no competing collapse engine. |

## Display and vector content: 5 kinds

| Kind and user goal | Existing typed contract | N/B ordinary behavior (S), event/authoring profile | Next useful outcome and owner |
| --- | --- | --- | --- |
| `heading` — understand a section hierarchy | `INyxHeading`, C; typography/accessible name | Supported/Supported readable heading text; native bold Label, browser fixed `h2`. No public per-heading semantic level was found in the specialized caption contract. Empty text valid but author must supply useful naming. E0/A0. | P2: explicit semantic hierarchy and actual assistive/native/browser evidence under parity criterion2. Do not infer hierarchy from font size. |
| `label` — read instructions or bound values | `INyxLabel`, C, `Binds.Text` and resources/expressions | Supported/Supported wrapped text and live binding. Native Label is not a selectable Memo; portable selection/copy/full-value inspection is not promised. E0/A0; read-only editing N/A. | P2: users must reveal/copy important long values through deliberate content/actions; data workbench03 owns cell inspection, parity owns general accessible text. |
| `separator` — recognize separated sections | `INyxSeparator`, C/common layout | Target-gap/Supported: browser creates `hr`; native falls through to an ordinary bevel-free panel with measured2px height, not a dedicated line face. Pointer callbacks do not create a useful command; keyboard/scalar N/A. E0/A0. | P2: native meaningful orientation/contrast/semantic separation under LCL/parity. Compare actual default face and theme variants before claiming invisibility. |
| `code` — inspect/copy code or logs | `INyxCode`, C; shared font/layout configuration | Supported/Supported inspection: native read-only no-wrap Memo with both scrollbars; browser `pre` with focus/key support. Native selection/clipboard inherited; no shared search/copy-button/line-number API. EK/A0. | P2: opt-in find/copy/line/overflow affordances under components01; CodeEditor is a separate editable control. Preserve read-only keyboard and host text scale. |
| `drawing` — see custom vectors and text | `INyxDrawing.WithPlan/WithProvider`, immutable `INyxDrawingPlan`, typed path/text/style/provider reference | Opt-in/Opt-in real custom content on standard adapters. Empty drawing is valid; providers are trusted registered Pascal implementations, not serialized executable classes. E0/EK where focus admitted; A0/AR. | P2: drawing hit/selection semantics belong to embedding Canvas/diagram, not an invented event per painted path. [Canvas](../TODO/NS-3_canvas-workspace_01.md) and [diagram](../TODO/NS-3_diagram-editor_01.md) retain quality/performance/accessibility. |

## Images and reusable media: 6 kinds

EI applies only to actual eligible Image/Button projections. A compound's event
union is not evidence every Avatar or every descendant can decode an image.
Semantic action payloads must use effective route discovery; a word such as
Favorite does not promise a Boolean toggle unless a value/action is configured.

| Kind and user goal | Existing typed contract | N/B ordinary behavior (S), event/authoring profile | Next useful outcome and owner |
| --- | --- | --- | --- |
| `image` — view an embedded/hosted picture | `INyxImage`, M; typed resource Image binding and imported/embedded PNG/JPEG | Supported/Supported embedded image fitting and alternative text; resource-hosted resolution is Opt-in. Direct native HTTP location needs a resolver. Decoder refusal can preserve previous picture; browser failures async. EI/A0 with image proposal form; AR for resolver. | P1: compose loading/error/retry/replacement visibly from lifecycle/resource status, not blank-face guessing. Existing resources acceptance is retained; media/parity owner qualifies full presentation/accessibility and remaining source forms. |
| `avatar` — identify a person | `INyxAvatar` inherits `INyxImageControl`; common Text gives initials | Supported/Supported **initials** face. Portrait source is Target-gap/Target-gap: native Label and browser text div; schema correctly marks image projection fields unsupported. No EI producer is inferred. E0/A0. | P1 media depth: typed portrait with initials fallback/alternative name and stale/error behavior on both targets, owned by components01 media/parity. Preserve existing initials when no source is authored. |
| `profile-card` — identify someone and choose an action | `INyxProfileCard` typed Avatar/Name/Bio/Actions Message/Follow parts | Supported/Supported reusable presentation and semantic Message/Follow callbacks; portraits follow Avatar limit. No messaging/account/follow backend implied. Empty data/failure/loading must be composed. EA/AP/AR. | P2: consume future Avatar capability; author data/loading/action policy through existing bindings/custom parts. No extra social-service engine is required. |
| `media-card` — inspect/open a preview | `INyxMediaCard` Media Image/Title/Description/Actions Open/Favorite parts | Supported/Supported composition with real Image, optional EI and semantic actions. Image starts empty unless bound/configured; favorite persistence/open target belongs to caller. EA/EI/AP/AR. | P2: visible missing/loading/error picture and deliberate Open/Favorite state through components01 media/parity. Reuse lifecycle and binding owners. |
| `avatar-group` — see participants and overflow | `INyxAvatarGroup` named Member1..4 Avatar and Overflow Label parts | Supported/Supported four default initials and static `+8 more` caption; dynamic membership/count/open-overflow behavior is Absent as a specialized collection contract. E0/AP; avatars inherit portrait gap. | P2: collection-driven identities/count and optional accessible overflow action under components01, retaining arbitrary reusable parts. A static placeholder is not live membership. |
| `comment-thread` — read and submit a reply | `INyxCommentThread` two named comment rows/author/body, ReplyMemo and SendButton | Supported/Supported editable reply and semantic Reply; collection-backed thread, validation/posting/error/retry/pagination are caller-owned/Absent as coordinated workflow. ET/EA/AP/AR; preserve unfinished Reply on refusal. | P2: useful bound repeated comments and pending/failed send presentation under components01 and existing bindings, with no implicit network endpoint. Actual memo editing/focus remains ordinary input parity, not a second editor. |

## Feedback and task status: 7 kinds

| Kind and user goal | Existing typed contract | N/B ordinary behavior (S), event/authoring profile | Next useful outcome and owner |
| --- | --- | --- | --- |
| `progress` — know whether work is happening | `INyxProgress`, Integer range/value, WithIndeterminate and Boolean binding | Supported/Supported determinate/unknown work. Native owned activity painter/timer; browser progress removes numeric value while indeterminate. It is observation, not an input; cancel is a separate action. E0/A0. | Positive foundation, including Studio source/build use. P2: reduced-motion/high-contrast/assistive and actual update/paint budgets remain feedback/parity/performance owners. |
| `badge` — read a concise status | `INyxBadge`, C/variant/binding | Supported/Supported caption; browser pill styling and native small accent Label differ. No shared typed live-announcement service is exposed. E0/A0; scalar edit/keyboard command N/A. | P2: native readable status face/contrast and accessible non-color meaning under parity; author text such as Stopped, not only a color. |
| `alert` — understand a problem and recover | `INyxAlert`, C/common configuration; callers can compose action siblings/parts | Supported/Supported static message. Standard native Label/browser div is not a shared typed polite/assertive live-region policy; retry/dismiss is not intrinsic to Alert. E0/A0. | P1: field/feed errors need discoverable text, focus/announcement and recovery actions under components01/parity. Compose existing controls; do not invent a separate validation engine. |
| `empty-state` — recover when there is no content | `INyxEmptyState` Illustration Avatar/Title/Description/CreateButton | Supported/Supported reusable empty presentation and semantic Create. It does not detect filtered-empty versus loading versus failed data. Default illustration uses initials Avatar, not auto artwork. EA/AP/AR. | P1: connect explicit feed/view states and matching Retry/Reset/Create actions in [live04](../TODO/NS-3_control-usefulness_04.md), keeping this recipe replaceable. |
| `confirmation-dialog` — safely confirm/cancel an operation | `INyxConfirmationDialog`; managed `INyxConfirmation` and typed options/focus/result | Supported/Supported inline recipe; **Opt-in/Opt-in** managed modal presenter clones content, closes/returns focus then dispatches Confirm/Cancel. Consumed child Escape refuses dismissal; open/result/lifetime checks exist. EA/EK/AP/AR. | Positive managed foundation. P2: unify discoverability of host-bound modality and actual input/accessibility under parity, keeping inline Studio warnings truthful. Application rechecks its own command revision. |
| `notification-card` — read and dismiss a notice | `INyxNotificationCard` Status/Content Title+Message/DismissButton | Supported/Supported static content and built-in `naDismiss`; semantic Dismiss goes through ordinary behavior. No coordinated notice queue/priority/announcement center is supplied by this recipe. EA/AP. | P2: typed optional notice lifecycle and accessible announcement under components01/parity; preserve caller-owned visibility and safe manual dismiss. |
| `toast` — see an outcome and undo deliberately | `INyxToast` MessageLabel/UndoButton/DismissButton | Supported/Supported inline recipe and built-in dismiss. Undo dispatches semantic Undo; application supplies reversal. Timed popup stacking, pause-on-hover/focus and durable undo-window policy are Absent as a public toast manager. EA/AP/AR. | P2: opt-in managed notice lifetime with keyboard recovery/announcement under components01; defaults must not silently erase actionable messages. Existing app/editor history is not automatically attached. |

## Dashboard, composition and authoring: 6 kinds

| Kind and user goal | Existing typed contract | N/B ordinary behavior (S), event/authoring profile | Next useful outcome and owner |
| --- | --- | --- | --- |
| `stat-card` — read a metric with context | `INyxStatCard.CaptionLabel/ValueHeading/TrendBadge`, L/C/bindings | Supported/Supported independent parts; defaults are sample text, no automatic numeric aggregation/formatting/feed state. E0/AP/AR. | P2: bind real values, units/trend meaning and stale/error states through existing expressions/resources under components01 and live04 where applicable. No new query engine. |
| `metric-grid` — compare several metrics | `INyxMetricGrid` three named Metric column/label/value/progress sets | Supported/Supported static3-column composition with real progress parts. Adaptive count/data repetition is caller content, not an automatic dashboard store. E0/AP/AR. | P2: readable compact reflow and explicit unknown/stale state via responsive content/bindings, owned by components01/parity/live04. Keep each metric independently configurable. |
| `component` — reuse a workflow with isolated customization | `INyxComponent`/`INyxReferenceControl`, typed Component references, Content/presentations, OverridePart descriptors | Opt-in/Opt-in independent realization, named parts/overrides and accepted coordinator snapshots. SlotOverride is owned authoring metadata, not a face. Bad references/parts refuse before replacement; stale callbacks revoke. E0 plus effective child profiles, A0/AP. | Positive reuse05 foundation. Full source/quality/parity under [components01](../TODO/NS-3_components_01.md) remains; do not weaken parent/DataToolbar prerequisites. |
| `design-surface` — compose an editor area | `INyxDesignSurface`, L, ordinary children | Supported/Supported layout container in standard renderer. It is not by itself infinite navigation or WYSIWYG drag/selection; Studio explicitly supplies editor behavior. E0/AP/AR. | P2: help should distinguish plain host from Canvas and Studio-managed selection. Full authoring remains [Studio owner](../TODO/NS-4_studio-authoring_01.md), not another canvas engine. |
| `canvas` — navigate a 2D workspace with real controls | `INyxCanvas.WithScene(TNyxCanvasRef)`; immutable scene declarations, runtime workspace optional interfaces | Opt-in/Opt-in saved/live scene, pan/anchored zoom/fit/home, embedded controls, own cameras/stores and source refresh. Absent scene deliberately creates an empty workspace; invalid authored references refuse. Background gestures preserve editable child input. EK/AP/AR; camera/grid/precision payloads use their typed services. | P1 for requested canvas vision: existing [canvas](../TODO/NS-3_canvas-workspace_01.md) and [diagram](../TODO/NS-3_diagram-editor_01.md) owners retain full drawing, connectivity, gesture/input, quality and measured budgets. Existing workspace is not absent. |
| `tool-canvas` — connect control events/data to useful operations | `INyxToolCanvas` WorkspaceCanvas/ToolsCard/PortHostScroll, `INyxToolCanvasInspector` and optional routing/logic/program/connector interfaces | Opt-in/Opt-in specialized saved Canvas plus Nyx inspector, script/Logic routes and port connections. Static callbacks and graph pause differ; runtime sandbox edits do not automatically become authored project history. EK/AP/AR with owned route/graph snapshots. | P1: finish the full [Tool Canvas](../TODO/NS-3_tool-canvas_01.md), [Logic](../TODO/NS-3_logic-components_01.md) and diagram outcomes, preserving actual browser/native, source/history and lifetime gates; no metadata-only completion. |

## Actions, inputs, data and navigation: 45 kinds

`T317`, for example, locates the interface at line317 of
[nyx.controls.types.inc](../src/nyx.controls.types.inc#L317); `R162` locates the
recipe at line162 of [nyx.recipes.pas](../src/nyx.recipes.pas#L162), at the exact
baseline above. These are source locators, not test counts. A0/AP supply generic
authoring, not a dedicated workflow editor for every recipe.

Additional shared profiles:

| Profile | Source-bound meaning and recovery |
| --- | --- |
| EV | Exact Boolean/Integer/Number/calendar/clock/RGB domains, bounds/choices and precommit binding admission. InputType hints do not implement required/email/regex validation. Optional date/time empty differs from an invalid date or midnight. Partial drafts and completed proposals are distinct; no silent scalar coercion. [Contracts](contracts.md), [binding owner](../src/nyx.binding.pas). |
| EC | Literal Items is an inspection face; `Binds.Collection` automatically mounts the admitted typed view. Four column families: Text/Boolean/Integer/Number. Independent immutable selection snapshots carry stable identities, focus and anchor. Read-only permits selection/disclosure but refuses edits; disabled refuses input. Hidden selection survives filters; source removal reconciles it. Empty data is valid; feed/loading/retry is separate presentation. Native on-demand values/browser windows do not establish whole-view budgets. [Views](collection-views.md), [view contracts](../src/nyx.collections.view.types.pas). |
| ER | Explicit coordinated intent plus common prepared mounts; exact generation/roles, dormant observations, copied payloads, flags-first revocation, typed Disconnect and stale-command refusal. Static absence/Disabled/Inherit differ. Runtime records/selection are not saved design defaults. [Creator contract](../src/nyx.coordinators.pas) and [reusable authoring](reusable-authoring.md). |
| EN | Navigation buttons use EA/EK. Retained Tabs, PageView paging and saved managed menus exist. Other named navigation actions still need an application/controller; no implicit wizard/service/router is inferred. |

For ET, native ordinary text Change publishes while editing; browser normally
commits Change on blur and explicitly admits pending text before a keyboard
shortcut payload. Numeric/domain completion preserves unfinished drafts.
`BindingFailed` and optional `OnBindingError` are existing refusal paths, not
automatic FormField error/focus presentation. Genuine browser beforeinput
cancellation and native model rejection have different physical timing; native
does not promise physical BeforeEdit veto or directed selection endpoints.
[Editing contract](events.md#typed-editing-sessions) and renderer
[keyboard admission](../src/nyx.render.browser.pas#L6788) describe that boundary.

### Actions: 5 kinds

| Kind and user goal | Existing typed contract | N/B ordinary behavior (S), event/authoring/recovery | Next useful outcome and owner |
| --- | --- | --- | --- |
| `button` — invoke a command | `INyxButton` T317; optional `NyxButtonConfigure`/typed presentation | Supported/Supported focusable button, caption/glyph/density/alignment/weight/tone and accessible fallback. EA/EK, optional EI glyph lifecycle; saved managed menu attachment Opt-in. Disabled/hidden cannot activate; glyph failure retains caption fallback. A0. | No replacement button engine needed. Existing facelift receipts cover their exact paths; full hardware/assistive/quality remains parity. |
| `link` — follow a destination | `INyxLink` T333; `Configure.LinkTo` and atHref metadata | Supported/Supported Click/keyboard activation; browser safe anchor navigates. Native is a focusable Button with no href/OpenURL path: automatic URL navigation Target-gap. Disabled browser href/Tab entry restores on enable. Empty destination is not a useful link. EA/EK/A0/AR. | P2 components01/parity: define explicit portable URL-launch authority/failure and demonstrate enabled/disabled navigation on both targets; native Click itself is supported. |
| `labeled-button` — understand an action | `INyxLabeledButton` T806, CaptionLabel/Button; R162 | Supported/Supported Label+Button with nseActivate signal; independent parts/default policy. EA/EK/AP. Outer label is not an automatic separate accessibility relationship; preserve the Button name. | Positive composition baseline. Any missing label relationship belongs to parity verification, not a new action engine. |
| `split-button` — primary command plus more actions | `INyxSplitButton` T834, PrimaryButton/MenuButton; R167 | Supported/Supported two action buttons; nsePrimary/nseMenu signals. Popup Opt-in through existing `MenuButton.Configure.Menu` declaration, not automatic from Menu semantic. Managed menus already provide keyboard/check/radio/submenu/invocation behavior. EA/EK/EN/AP/AR. | P3 components01/help: make menu attachment discoverable and retain empty/refused-menu recovery; do not duplicate the existing menu implementation. |
| `command-bar` — find/invoke common commands | `INyxCommandBar` T2384, New/Open/Save buttons and CommandInput; R423 | Supported/Supported actions and editable input; automatic command-search/execute palette Absent. Empty input is valid; matching, errors/history and actual file commands belong to caller. EA/EK/ET/AP/AR. | P2 components01: command discovery/select/execute/recovery using existing menu/popover and callback contracts; stable command identity independent of caption. |

### Primitive inputs: 11 kinds

| Kind and user goal | Existing typed contract | N/B ordinary behavior (S), event/authoring/recovery | Next useful outcome and owner |
| --- | --- | --- | --- |
| `input` — enter one value | `INyxInput` T349/`INyxTextInput`, InputType/Value/Placeholder/ReadOnly | Supported/Supported TEdit/HTML input, password both; native numeric adaptations and ordinary text differ from browser Email/Search/Tel/URL hints. ET/EV/A0. Complete refusal restores accepted value; host must explain domain/application failure. | P1 components01/parity: visible field-specific required/refusal feedback and correction, retaining peer drafts. Input hint is not a portable email validator. |
| `memo` — compose multiline text | `INyxMemo` T365/`INyxTextInput` | Supported/Supported TMemo/textarea, ET, owned editing snapshots and text Value. Empty valid; read-only retains inspection. Native placeholder is widgetset-limited; native clipboard/Undo is not a portable Nyx history promise. A0. | P2 components01/parity: explicit multiline placeholder/help/refusal and retained draft/focus journey; preserve existing editor rather than implement a second text engine. |
| `checkbox` — set an independent Boolean | `INyxCheckbox` T381, Checked | Supported/Supported native/HTML checkbox, click/Space and accepted Boolean Change. Default false; no tri-state facade. Read-only restores refused proposal; disabled blocks. EV/EK/A0. | No gap for Boolean choice. Tri-state requires a concrete admitted use case; do not advertise it today. |
| `switch` — toggle a preference | `INyxSwitch` T397, Checked | Supported/Supported Boolean; native Checkbox vs browser role=switch is a presentation/semantic difference, not missing toggling. EV/EK/A0, same refusal policy as Checkbox. | P3 components01/parity: name/state/focus and native presentation evidence before claiming identical styling or assistive semantics. |
| `radio` — choose one group peer | `INyxRadio` T413, Checked | Supported/Supported physical radio, eligible Tab entry; native-parent and browser name/form grouping differ. Per-node Boolean does not imply a stable-ID group model or aggregate payload. EV/EK/A0. | P2 components01/parity: explicit authorable group/choice identity and hidden/disabled/read-only peer behavior. SegmentedControl already supplies a closed-choice alternative. |
| `select` — choose a stable option | `INyxSelect` T429/`INyxChoiceControl.Items/Value` | Supported/Supported dropdown; current line Items uses identical label and stored string. Separate stable-key/display-label model Absent. Empty/unavailable choice follows domain admission; read-only restores proposals; unchanged items retain selection. ET/EV/A0. | P2 components01: typed immutable option identities/display/disabled/group policy. Change a translated label without changing stored value on either target, with full authoring checklist. |
| `spin` — adjust an integer | `INyxSpin` T445/`INyxIntegerInput` | Supported/Supported TSpinEdit/number input, integer bounds/Change. Native Spin owns a valid Integer face, not arbitrary malformed text. Disabled/read-only refuse writes; completed out-of-range proposals reject. EV/EK/A0. | Positive baseline. P2 shared field-refusal explanation; preserve exact Integer contract. Input plus typed capture is the existing path for independently retained malformed drafts. |
| `slider` — adjust a bounded amount | `INyxSlider` T461; Integer Value, NumberValue/WithNumber with declared Number domain, SliderIntervals | Supported/Supported trackbar/range through shared scale and keyboard; arbitrary programmatic between-tick numeric values remain exact. Explicit endpoints/intervals; read-only rejects edits. EV/EK/A0. | P3 components01/parity: expose value/units and keyboard help using ordinary parts. No new scale engine or claim every fine numeric value is physically reachable. |
| `date` — choose/clear a day | `INyxDate` T487, DateValue/WithDate and typed date domain | Supported/Supported owned native calendar/editor vs browser date field; optional empty, exact Gregorian/bounds/choices and draft completion. Popup Apply/Clear/Cancel already exists; required policy belongs to application. ET/EV/A0 plus domain editor. | P1 shared field feedback; retain optional empty/cancel. Richer range-calendar UI remains component depth, distinct from existing DateRange ordering. |
| `time` — choose/clear a time | `INyxTime` T512, ClockValue/WithTime and typed clock domain | Supported/Supported native picker/editor/browser clock, exact milliseconds/steps/wrapping range and empty versus midnight. Partial draft/Apply/Clear/Cancel exist; browser picker chrome varies. ET/EV/A0 plus domain editor. | P1 shared field feedback only; preserve exact domain and optional empty rather than round values or invent a missing basic clock picker. |
| `color` — choose an RGB value | `INyxColor` T538, ColorValue/WithColor and RGB domain | Supported/Supported owned hex/picker boundaries and strict RGB channels/choices. Partial hex retained until completion; cancel preserves accepted value; read-only blocks. Alpha/wide gamut outside current RGB contract. ET/EV/A0. | P3 components01: broader color spaces only with a useful admitted requirement and typed format/editor/source changes. Existing RGB picker is a positive foundation. |

### Compound inputs: 5 kinds

| Kind and user goal | Existing typed contract | N/B ordinary behavior (S), event/authoring/recovery | Next useful outcome and owner |
| --- | --- | --- | --- |
| `search-field` — run/clear a search | `INyxSearchField` T862, QueryInput/SearchButton/ClearButton; R172 | Supported/Supported input/actions; Search owns an explicit query-part Text payload. Clear really executes naClear before semantic Clear. It does not filter a collection automatically. Empty query valid; read-only refuses clearing/editing. ET/EA/AP/AR. | P2 components01/DataToolbar02 Q1: declared target through existing query owner, visible text/query coherent and clear/no-results recovery. Preserve existing functional Clear. |
| `date-range` — enter endpoint dates | `INyxDateRange` T1022, StartDate/FinishDate; typed range options; R204 | Supported/Supported independent fields; Ordered coordination Opt-in, admits complete final pair start≤finish including equality/empty/one-sided values. Independent masks inheritance. Endpoint date payloads, not invented aggregate range. Invalid pair refuses without peer swap/clamp. EV/ER/AP and dedicated range editor. | Positive existing range journey. P1 shared field feedback; richer combined calendar is depth under components01, not another state/domain engine. |
| `number-stepper` — adjust quantity | `INyxNumberStepper` T1056, DecrementButton/ValueSpin/IncrementButton; R210 | Supported/Supported actions target actual integer part, default1/bounds0..999. Managed value redirects to that part; increment/decrement payload identifies command/target. Read-only/bounds protect edits. EA/EV/EK/AP. | Positive baseline. P2 communicate boundary/refusal and button availability through components01/parity. Malformed-text drafting belongs to Input rather than an invented Spin Text overload. |
| `segmented-control` — choose a named mode | `INyxSegmentedControl` T1090, Day/Week/Month buttons; `TNyxSelectionOptions`; R216 | Supported/Supported naSelect buttons with Text choices day/week/month, defaultday and selected payload. Typed styling/availability is Opt-in; identity differs from caption. Read-only and reusable independence remain. EA/EV/AP plus selection editor. | P2 parity/components01: chosen/focus state and compact keyboard discovery; ordinary buttons do not imply automatic radio-group arrow navigation. Retain existing styling. |
| `rating` — choose a bounded score | `INyxRating` T1124/IntegerInput and five named buttons; R224 | Supported/Supported naSelect1..5/default3 and Integer Rate payload; selection styling Opt-in. Unrated/empty is outside default domain. Read-only remains inspectable. EA/EV/EK/AP. | Positive integer scoring. P3 components01: explicit optional unrated/clear and useful labels if required; hover/fractional stars/slider keys are not current promises. |

### Forms and editable code: 5 kinds

| Kind and user goal | Existing typed contract | N/B ordinary behavior (S), event/authoring/recovery | Next useful outcome and owner |
| --- | --- | --- | --- |
| `form-field` — enter/correct a field | `INyxFormField` T896, Input/HelpLabel/ErrorAlert; R181 | Supported/Supported input/help/initially hidden Alert. Automatic required/domain/binding-error→ErrorAlert mapping Absent; default required wording is placeholder, not a validator. App can bind text/visibility/domain. ET/EV/AP. | P1 components01/parity: field reason/correction/first-invalid focus and retry using current domain/OnBindingError/error part; distinguish accepted value and bad draft, preserve peers. |
| `login-form` — request sign-in | `INyxLoginForm` T930, EmailInput/PasswordInput/RememberCheckbox/SubmitButton; R187 | Supported/Supported controls/password masking/Submit signal. Email defaults to plain Input; root NoValue gives no automatic credentials record. Empty fields not rejected by recipe; authentication/network/security are caller services. ET/EV/EA/AP/AR. | P1 shared forms: required/invalid/pending/error/retry/cancel and explicit aggregate capture, disabled/read-only flow. No implicit secret logging/persistence or authentication service. |
| `settings-panel` — edit/save preferences | `INyxSettingsPanel` T976, NotificationsSwitch/SyncSwitch/LanguageSelect/SaveButton; R195 | Supported/Supported individual values and Save signal; no automatic aggregate durable transaction. Dirty/revert/commit/error/persistence are caller-owned, defaults are recipe values. ET/EV/EA/AP/AR. | P2 components01/authoring: deliberate commit/cancel/validation/retry over current multi-state candidate owner, proving drafts versus saved state without duplicate history. |
| `property-grid` — inspect/edit named values | `INyxPropertyGrid` T2314, three typed Label/Input rows; R414 | Supported/Supported static editable rows; root NoValue, no object-schema/type-dependent editor/aggregate Apply. Rich Studio Inspector is a separate consumer. Empty value valid; feedback/apply/reset must be composed. ET/AP/AR. | P2 components01: schema-driven value-kind editors/help and accepted/refused atomic edit flow over existing fields, preserving parts/identity. Static recipe is not a full object workbench. |
| `code-editor` — edit code/configuration | `INyxCodeEditor` T742/`INyxTextInput` | Supported/Supported no-wrap native Memo/browser textarea with spellcheck disabled, ET/EK, owned editing/selection/IME and selection capability. Native clipboard/basic editing retained; generic language/completion/diagnostic/line-number service Absent. Read-only inspection supported. A0/AR. | P2 components01/parity: useful indent/tab/find/select/undo/help/error-location workflow. Studio separately composes source input, per-file history and Pascal resolver; do not attribute that IDE engine to every CodeEditor or call Studio merely a textarea. |

### Data and data-bearing compositions: 11 kinds

| Kind and user goal | Existing typed contract | N/B ordinary behavior (S), event/authoring/recovery | Next useful outcome and owner |
| --- | --- | --- | --- |
| `list` — browse/find/select records | `INyxList` T563, typed collection specification and `INyxCollectionView` | Supported literal inspection; bound selection/query/typeahead/multiple selection Opt-in/Opt-in. Native ListBox/browser semantic list uses first declared label column. SelectionChange owns before/current membership/focus/anchor, not row-index guesses. Empty valid, read-only selects, filtering preserves hidden membership. EC/A0 and Data/query editors. | P2 components01: discoverable search/selected/hidden/count/no-results using existing query/paging. Live/large04 owns new context/budgets; no replacement list store. |
| `table` — inspect/edit/filter records | `INyxTable` T579; view columns, CellText/EditWire/ConfigureQuery | Opt-in/Opt-in bound grid, with typed editing/navigation/row selection once bound; literal inspection Supported both. Native on-demand values/browser row windows. Query engine exists but header filter/sort UI Implemented-unexposed. Native Ctrl+C inherited; portable copy policy Absent. EC/A0/Data/query editor; drafts/selection have stable identity. | P1 [02](../TODO/DONE/NS-3_control-usefulness_02.md) header filters/sort/count/reset; [03](../TODO/NS-3_control-usefulness_03.md) W1 arrangement/W2 inspection-copy; [04](../TODO/NS-3_control-usefulness_04.md) live context/budgets. No standard resize/reorder/hide/pin UI today; external width retention is not a drag handle. |
| `tree` — inspect hierarchy | `INyxTree` T595; collection Parent field, typed hierarchy/rows | Supported flat literal inspection; bound hierarchy/disclosure/selection/typeahead/editing Opt-in/Opt-in. Arrow parent/child navigation, expansion and cycle/schema refusal exist. Native retains full tree; browser windows disclosed rows+ancestors. Paging explicitly refuses until hierarchy-aware. Read-only can disclose/select. EC/A0. | P2 components01/live04: useful search/reveal-path/loading/error/context and separate scale evidence. No second tree/disclosure engine; do not call a flat literal tree a bound hierarchy. |
| `data-toolbar` — search/order/create/refresh | `INyxDataToolbar` T1170, SearchInput/SortSelect/CreateButton/RefreshButton; R236 | Supported/Supported parts/create/refresh signals. Shared query engine exists; automatic coordinated hookup is Absent here, sort captions are not field mappings and Clear is not a default part. ET/EA/AP/AR. | P1 existing [DataToolbar02](../TODO/NS-3_data-toolbar_02.md) Q1/Q2: settle same-target/query authority and scalar-reset/query publication before source release, useful typing/sort/clear and two independent applications; then01 A1/A2 Studio/save/generated. Preserve full parent/parity blockers. |
| `filter-bar` — apply/reset criteria | `INyxFilterBar` T1210, StatusSelect/QueryInput/ApplyButton/ResetButton; R243 | Supported/Supported controls/Apply/Reset signals; Reset does not automatically reset fields/query. Shared predicates/query editor already exist. Draft/accepted state/error presentation belongs to caller. ET/EA/AP/AR. | P1 usefulness02 with Q1 same-target policy: typed query contribution/apply/reset to current baseline using one evaluator/publication owner. A toolbar textbox is not the missing column-filter UI. |
| `master-detail` — inspect/edit selected record | `INyxMasterDetail` T1370, MasterList/detail parts; typed options/intent/binding | Static recipe by default; Configured intent automatically mounts real common coordinator N/B. Visible single selection maps text, restores placeholders/disables Edit when absent/hidden/removed. Checked Edit owns record Details independently of scalar Value. Immediate Disconnect/manual reacquire and stale refusal exist. EC/ER/AP and dedicated editor/Details flows. | Positive native165/175 and owncompiled36/36 source path, historical reusable browser boundaries in evidence map. Richer detail fields/edit form/backend remain components01/caller depth; no replacement basic coordinator. |
| `list-card` — browse a titled list | `INyxListCard` T1416, List/ActionsPrimaryButton; R280 | Supported/Supported composition, EC Opt-in via normal list binding. ViewAll is signal, not automatic expansion/navigation. Empty/loading/error/destination are caller content. EA/AP/AR. | P3 components01: documented binding/no-results/ViewAll destination flow, reusing collection/navigation capabilities. |
| `data-card` — inspect/export a compact table | `INyxDataCard` T1456, Table/ActionsExportButton; R286 | Supported/Supported composition with EC Opt-in. Export is signal, no automatic encoding/destination/selected-row packet. Table query/column/copy limits inherit02–04. EA/AP/AR. | P2 components01/03 W2: explicit selected/all/current-query copy/export, exact identity/schema and refusal/progress/retry through existing file/flow owners. |
| `kanban-board` — move stable cards | `INyxKanbanBoard` T1496; typed Kanban options/intent/binding Items/MoveButton/CaptureMove | Static three-lane lists/Add signals; Configured intent automatically mounts real movement N/B. Move button/Space/local drag share revision-checked publication. Click scheduling preserved; synchronous drag negotiation requires live sequential response. Read-only/disabled/missing/unselected authority refuses. EC/ER/AP and board editor. | Positive current C2 native114/124 and owncompiled15/15; older-adapter actual browser scope retained. Arbitrary lanes/card forms/add backend and full quality remain components01 depth; Add signal is not CRUD. |
| `timeline` — read activity in order | `INyxTimeline` T1584, named entry/time/title/description; R303 | Supported/Supported static3-entry display, E0, default edit/selection N/A. Feed/date grouping/query/follow workflow Absent. Displayed badge strings do not establish timestamp ordering; empty/error/timezone summaries need composition. AP/AR. | P2 components01/live04: bind timestamped items with useful empty/error/live context through existing collection/time/resource owners, not another timeline store. |
| `floating-action-panel` — browse and invoke Add | `INyxFloatingActionPanel` T2430, ContentList/ActionsFloatingButton; R431 | Supported/Supported Stack/List/action row with Add signal; EC Opt-in. Name does not install sticky/overlay placement or CRUD. Empty valid; loading/add result caller-owned. EA/EK/AP/AR. | P3 components01: compact reachable placement and one real Add journey through existing layout/overlay capability; no automatic floating window claim. |

### Navigation: 8 kinds

| Kind and user goal | Existing typed contract | N/B ordinary behavior (S), event/authoring/recovery | Next useful outcome and owner |
| --- | --- | --- | --- |
| `toolbar` — reach grouped actions | `INyxToolbar` T221, L and normal children | Supported/Supported horizontal layout; actual buttons supply EA/EK. No automatic command registry/overflow/roving-key model. Empty container activation N/A. Children retain hide/disable/reflow and meaningful naming. A0/AP. | P3 components01/parity: accessible compact overflow where a real action set requires it, composing existing menu/presentation/layout rather than another command engine. |
| `tabs` — switch retained pages | `INyxTabs` T253; typed Tabs options/selection/change/binding | Supported/Supported retained hosts and automatic/manual activation, orientation/wrap/direct-child selection. Hidden pages retain drafts. Disabled/hidden headers skipped; manual Enter/Space; invalid/foreign/retyped candidate refuses. No eligible page gives -1. Programmatic sync does not fabricate Change. EK/EV/A0 plus Tabs editor. | Positive retained-tabs foundation. Existing observing-schema restriction remains distinct from current source; broad input/assistive/widgetset quality stays parity. |
| `tab` — use one retained page | `INyxTab` T269/L; direct child of Tabs | Supported/Supported content and parent-generated header; child drafts persist. Page itself is not a route command. Empty valid; eligibility follows enabled/visible policy; misplaced/retyped child refuses Tabs admission. EN/EK via header, child ET, AP. | P3 help/parity: clear direct-child contract and accessible caption; richer workflows belong to child controls, not a new page engine. |
| `pagination` — navigate a result window | `INyxPagination` T1250, PreviousButton/PageSpin/TotalLabel/NextButton; `Configure.PageView`, typed paging; R250 | Supported/Supported bounded default spin1..100/actions; actual paged List/Table autoattachment Opt-in/Opt-in. Filter/order before paging; counts/bounds/availability use one shared view/pager. Empty page1/pages0, no navigation; no-op silent, hidden selection preserved. Unpaged/tree/missing/retyped target refuses. EA/EV/ER/AP/Data. | Positive PageView foundation. P3 components01/help: discoverable target setup and no-results/reset story; static total label does not mean paging runtime is absent. |
| `breadcrumbs` — identify location/navigate ancestors | `INyxBreadcrumbs` T1290, HomeButton/SectionButton/CurrentLabel; R257 | Supported/Supported named actions/current caption; history/path expansion/URL mapping Absent as automatic workflow. App/controller chooses destination. Empty/current/disabled/error policy caller-owned. EA/EK/AP/AR, read-only edit N/A. | P2 components01: typed current-path/action mapping, keyboard/current-item/failed-navigation recovery while preserving independent page history. Reuse existing application navigation. |
| `sidebar-nav` — choose an application section | `INyxSidebarNav` T1330, Overview/Projects/Settings buttons; R264 | Supported/Supported named signals/heading; routing/current marker not automatic. Disabled entries refuse, forbidden/loading destination caller-owned. Empty custom navigation valid. EA/EK/AP/AR. | P2 components01/parity: real ShowPage/current-marker/keyboard/recovery journey through existing selection/presentation, not a duplicate router. |
| `wizard-step` — complete a form step | `INyxWizardStep` T1978, ContentNameInput/NavigationBackButton/NavigationNextButton; R361 | Supported/Supported one form and Back/Next signals. Step sequence/record/validation gate/async submit Absent as coordinated controller. Step badge is text. ET/EA/AP/AR. | P1/P2 shared form feedback/components01: refuse advance with useful reason, preserve prior steps, pending/error/retry and meaningful Back through existing state/publication/history. Disabled Next alone is not explanation. |
| `stepper` — choose a numbered step | `INyxStepper` T2036/IntegerInput and four buttons; R369 | Supported/Supported naSelect1..4/default1 and Integer Step payload; styling Opt-in. Does not show WizardStep content or enforce completed/locked steps. No none/error/completed domain by default; read-only refuses current-value edits. EA/EV/EK/AP. | P2 components01: explicit progression/availability and relationship to content where needed through current state/Tabs/controller; no implicit wizard transitions. |

The helper's source path checks include:
[native primitive creation](../src/nyx.render.lcl.pas#L1720),
[browser primitive creation](../src/nyx.render.browser.pas#L1583),
[literal choice values](../src/nyx.render.browser.pas#L7038),
[native collection attachment](../src/nyx.collections.lcl.pas#L206),
[browser headings](../src/nyx.collections.browser.pas#L239),
[query/editor](../src/nyx.collections.query.editor.pas#L34),
[pager](../src/nyx.collections.pager.pas), [tabs](../src/nyx.tabs.pas),
[source input](../studio/nyx.studio.sourceinput.pas),
[MasterDetail](../src/nyx.master.detail.pas), [Kanban](../src/nyx.kanban.pas),
and the exact per-kind interfaces/recipes above. Native inherited grid Copy is
an upstream LCL implementation detail; the configured Nyx grid does not enable
the ordinary `goColSizing`/`goColMoving` user controls. A test assigning ColWidths
programmatically does not demonstrate a drag-resize affordance.

## A concrete process/service workbench

Use one deterministic service dataset with stable service IDs and typed fields
Status, CPU, ExecutablePath and LastError. An optional replay supplies changes;
no real process termination or privileged service control is needed for a demo.
Literal Items tables and schema-bound tables must be demonstrated separately.

| User story | Existing foundation | Missing result, expected behavior and owner |
| --- | --- | --- |
| Find a stopped service by status | Typed query predicates, stable identities, List/Table and advanced query editor | Column filter UI in [02](../TODO/DONE/NS-3_control-usefulness_02.md): apply Status=Stopped, visible count/chip, clear one/all, no-results recovery. Query the full result before paging. |
| Order numeric CPU and keep a secondary name order | Typed stable multi-key ordering | Header sort actions/indicators/precedence in02; numeric10 sorts above2, equal values remain stable, reset restores current authored baseline. |
| Read a full executable path without shrinking text | Typed column sizing, cell navigation/editing, scrolling, Label/Code/Memo inspection | W1/W2 [03](../TODO/NS-3_control-usefulness_03.md): resize/choose/pin columns and open full-value inspection; browser/native keyboard equivalent. |
| Copy IDs or one raw value | Native inherited copy, typed row/field values and shared selection | W2 defines portable cell/row/range copy, raw/display choice, header/escaping and clipboard refusal; hidden rows/columns never silently change identities. |
| Keep reading while updates arrive | Observable stores, immutable snapshots, scheduler and row windows | L1 [04](../TODO/NS-3_control-usefulness_04.md): retain focus/anchor/draft, deliberate follow/pause/resume and bounded queued updates. Deleted/hidden records have explicit recovery. |
| Tell no results from failed, refreshing or paused | EmptyState/Alert/Progress/Badge and ordinary buttons | L1 composes distinct states and Retry/Reset/Resume, meaningful text and optional announcements. P1 field errors and media errors reuse the same presentation principles. |
| Change a configuration safely | Typed domains/admission, field drafts and confirmation presenter | Field feedback/focus and revision-aware command remain components01/parity. Confirmation is not permission to overwrite a newer selected record. |

## Priorities, public examples and finish lines

The implementation owner table below extends existing tasks; it creates no new
task identities or duplicate stores/query evaluators. Every enhancement must
retain a specialized managed interface or optional capability, immutable typed
policy, explicit defaults and caller override. Open names identify application
data/parts/resources; enum/record/interface choices select behavior.

All proposed features need the **same adoption checklist**: Properties/Events and
meaningful help, both target support grades, persistence and reusable local
inheritance/masks, paired Undo/Redo, crafted fluent generated Pascal and its
strict reader, bounded revision-aware semantic reads/mutations, useful ordinary
native/browser input and the journey's own saved/generated compilation. Runtime
selection/drafts/paused feeds/session preferences remain separate from authored
design history. New event stages define copied scalar versus Details payloads,
cancelability, registration order, execution policy and retirement. Do not
serialize service instances or execute untrusted saved strings as Pascal.

| Rank | Enhancement and typed direction (proposal, not an implemented API) | Explicit owner and useful finish line |
| --- | --- | --- |
| P1 | Per-column query capabilities and immutable operator/sort policy; compose with authored query authority | [02](../TODO/DONE/NS-3_control-usefulness_02.md): actual header filter/sort/clear/counts on both targets, two independent tables and current baseline restoration. Settle DataToolbar Q1 authority before source release; no hard reciprocal dependency. |
| P1 | Field validation presentation over existing domain/admission results, with owned field identity and focus intent | [components01](../TODO/NS-3_components_01.md) usable forms plus [parity](../TODO/NS-2_parity-accessibility_01.md) criterion2: ordinary form identifies/corrects invalid fields and focuses first error without losing peers' drafts or changing accepted data. No new validation engine. |
| P1 | Optional live-view follow/pause/update-state policy over current collection/scheduler owners | [04/L1](../TODO/NS-3_control-usefulness_04.md): two views preserve context during churn, show failure/paused state and reject retired updates. L2 separately measures actual mounted input/paint/memory budgets. |
| P1 | Discoverable column arrangement, full-value/copy/export policy with stable row/field identities | [03/W1 then W2](../TODO/NS-3_control-usefulness_03.md): real resize/keyboard alternative, choose/reorder/hide/pin/reset, then inspect/copy/actions/export and preference lifecycle. Native ColWidths retention alone is not a user resize handle. |
| P1 media | Optional portrait/initials fallback policy using the existing immutable image/resource/lifecycle owners | [components01](../TODO/NS-3_components_01.md) media and parity: actual portrait Avatar on both targets, independent reusable names/fallbacks, loading/error/clear/stale behavior and source/history. Initials-only remains a valid default. |
| P1 deferred bug | Existing split runtime preference and capture policies | [split02](../TODO/NS-2_split-platform_02.md): reproduce external snap-back, then qualify retained position/reflow/cancellation/bounds on real controls. No new splitter API or investigation in this audit. |
| P1 canvas | Existing scene/graph/program/port policies and managed optional capabilities | Existing [canvas](../TODO/NS-3_canvas-workspace_01.md), [diagram](../TODO/NS-3_diagram-editor_01.md), [Tool Canvas](../TODO/NS-3_tool-canvas_01.md) and [Logic](../TODO/NS-3_logic-components_01.md) tasks retain full requested 2D outcomes and their original acceptance. Do not substitute another miniature demo. |
| P2 | Typed display/value separation for choices; explicit editing/search commands; optional semantic heading/live-feedback policy | Components01/parity and helper findings below: retain existing native defaults and specialized contracts, then prove the actual user journey and authoring checklist rather than infer it from metadata. |
| P2 | Optional managed notification lifetime/announcement and data-driven media/dashboard repetition | Components01/parity: real dismiss/undo action ownership and bounded notice queue/focus/timeout, or bound repeated items with stable IDs and correct counts. Preserve manual composition and caller services; no automatic messaging/authentication. |

The following are **proposed contract directions**, not newly callable methods,
released ABI or executed features. Existing owners settle signatures before
implementation. This supplies a policy/event finish line for each enhancement
instead of treating every missing behavior as another untyped property.

| Enhancement | Typed surface and intended default/override boundary | Useful semantic event meaning |
| --- | --- | --- |
| Column queries | Optional managed Table/binding capability with immutable field references, operator/sort enums and schema-derived allowed values. Authors can opt out per table/column; old literal Items and authored baseline do not silently become a second query owner. | Cancellable before-query/filter/sort proposal with exact view/field/origin/old-new policy; committed after-change/count snapshot. Clear has explicit current-baseline authority, not implicit restore. |
| Field feedback/forms/wizard | Reusable managed field/form presentation with immutable field-state/reason and focus policy over current bindings/domains. Do not infer Required; first-invalid focus occurs on explicit Submit, not every keystroke. Caller can replace error/help parts and choose validation timing. | Owned field-ref/reason/draft-versus-accepted state on validation observation; cancellable submission before command publication, completion/failure separately. No implicit credentials/aggregate record. |
| Live inspection | Optional collection-view update/follow policy with typed retention/coalescing/overflow enums. Opt-in; pause affects the view, not automatically its producer. Two views can choose independently. | Pause/resume/follow/update-state and lost-anchor observations carry revision/stable identity; stale completions cannot reach retired view. Budgets are separate measured acceptance. |
| Column arrangement | Optional Table presentation facet returning copied field-keyed arrangements, widths/bounds/pin enums. Preserve authored defaults; runtime preference changes/reset remain mount/session-owned unless explicitly saved. | Cancellable arrange proposal and committed resize/reorder/visibility/reset snapshots; no row mutation. Keyboard/drag share one publication path. |
| Inspection/copy/export | Typed selection/range, raw/display and delimiter/header/escaping policy with managed command/result capability. Existing native copy stays available; full-result export is explicit, not silent selection expansion. | Stable captured row/field IDs and copy/export request/result/unavailable/cancelled observations; before-side-effect cancellation occurs before clipboard/file output. Reuse file-save/scheduler owners. |
| Avatar/media recovery | Optional portrait/fallback capability using `TNyxImageSource`, image fit and resource binding. Preserve initials-only default; caller chooses name/initials/decorative fallback and error/retry presentation. | Reuse EI generation/source/dimensions/failure phases; optional Retry is an ordinary command. No second image loader or fake readiness. |
| Splitter regression | Existing `INyxSplitControl` and captured requested/effective preference; no new behavioral property. Preserve caller bounds, explicit resets and cancellation. | Existing integer Change on completed accepted resize; cancelled or no-pixel movement does not fabricate completion. Exact external regression still needs reproduction. |
| Canvas/diagram/Tool Canvas | Preserve existing scene/graph/program/provider/precision optional interfaces and immutable references. No global script authority; static callbacks, graph activation and authoring versus sandbox history remain explicit. | Existing owner-defined prepared edit/selection/connection/program events carry exact generations and copied values. Full original graph/input/quality criteria remain the finish line, not a new minimal substitute. |
| Select/radio choices | Optional immutable choice set with typed stable value, display/resource label, enabled/group metadata and empty policy. Legacy Items behavior remains compatible; structured options are deliberate authoring, with opt-out grouping/empty rules. | Existing typed selection/change evolves with exact old/new stable identities; rejected/removed/disabled choices retain an explicit recovery policy. A translated label is not a value change. |
| Generic code editing | Optional managed text-command/history/search capability with typed indentation/tab/search/read-only options. Plain editing remains valid; language/highlighting/completion service is explicitly attached. | ET editing/draft/selection snapshots remain authoritative; search-result/navigation/command observations use copied scalar positions/revisions, not retained widgets. Never substitute the Studio resolver for the generic contract. |
| Semantic text/status | Typed heading level and optional polite/assertive/off announcement policy; preserve ordinary caption defaults and explicit author override. Passive text does not acquire forced focus or automatic copy actions. | Announcement completion/availability is a target observation, not proof an assistive user heard it; source text changes remain existing binding events. Native separator/status presentation also needs actual target evidence. |
| Managed notices | Optional managed presenter/queue with immutable placement/priority/timeout and pause-on-focus policy. Manual inline recipe remains valid; no timeout is the safe actionable-notice baseline. Caller owns Undo command validity. | Dismiss reason, action/Undo request and expiry observations use owned notice/revision IDs; callbacks cannot revive retired notice/service authority. |
| Data-driven media/dashboard | Typed collection/content template references and stable item IDs over existing source/binding/resource services. Opt-in; static recipes remain useful. Empty/loading/error/paused states are distinct replaceable parts. | Existing collection/source revisions and meaningful item/action IDs, not physical row positions or a fabricated root scalar. Insert/remove/refusal remains ordinary prepared publication. |
| Navigation/command recipes | Optional typed destination/command refs and copied current-path/progression policy over existing App, selection, Tabs and menu services. Signal-only recipes remain compatible; URL launch is explicit host authority and can refuse. | Cancellable navigation/command request and committed destination/current-state/error; independent page history and stale selection checks remain. A menu/Save semantic name alone grants no service behavior. |

Existing public API sketches below are **source-only reproduction starting
points** using current contracts; they were not compiled or executed in this
review. Required application stores, bytes, host presenters and callbacks are
explicit arguments, rather than invented service implementations.

```pascal
// Ordinary native/browser sizing exists; actual user column handles are03 work.
LWorkspace := NewNyxSplitView('service-workspace');
LWorkspace.Configure.SplitOrientation(nsoSideBySide)
  .SplitPosition(60).SplitMinimum(20).SplitMaximum(80).Done;
LWorkspace.Add(LServiceTable).Add(LInspection);

// This is a real picture face; standard Avatar currently remains initials-only.
LPortrait := NewNyxImage('service-owner-picture');
LPortrait.Configure.Source(NyxEmbeddedImageBytes(nimPNG, APictureBytes))
  .AlternativeText('Service owner').ImageFit(nifContain).Done;
LInitials := NewNyxAvatar('service-owner-initials');
LInitials.Text := 'NY';

// The host constructed APrompt from an independently owned template clone.
// Its specialized Content remains configurable before opening.
APrompt.Content.TitleHeading.Text := 'Apply this configuration?';
APrompt.Content.ActionsCancelButton.Text := 'Keep current values';
APrompt.Open(NyxConfirmation('Review service configuration')
  .Focus(NyxPart('actions/cancel')).Sizing(ncsContent));

// Status is explicit and unknown work is not a fabricated percentage.
LActivity := NewNyxProgress('service-refresh');
LActivity.Configure.Text('Refreshing services').Indeterminate(True).Done;
LNotice := NewNyxToast('service-result');
LNotice.MessageLabel.Text := 'Configuration updated';
// Subscribe the Undo semantic to the application's actual reversible command.
```

Imports for these source snippets are `nyx.controls`, `nyx.types`, `nyx.images`
and `nyx.confirmation`; `APrompt` was constructed from its template at the host
boundary. The code does not claim a new common presenter factory. Exact query/form
examples and their ordinary current limitations follow.

```pascal
// Collection/schema declarations are supplied by the application.
// Filtering and numeric ordering exist; this does not install header commands.
LTable := NewNyxTable('services');
LTable.Binds.Collection(
  NyxCollectionView(NyxCollection('services'))
    .Column(NyxTextField('name'), 'Service')
    .Column(NyxTextField('status'), 'Status')
    .Column(NyxNumberField('cpu'), 'CPU')
    .Query(NyxCollectionQuery
      .Where(NyxWhere(NyxTextField('status')).EqualTo('stopped'))
      .OrderBy(NyxNumberField('cpu'), nsdDescending)
      .ThenBy(NyxTextField('name'))))
  .Done;

// Advanced predicate/order editing is already a reusable Nyx form.
// The host captures its Save proposal and publishes through the existing owner.
LQueryEditor := NewNyxQueryEditor('service-query', NyxControl('services'),
  AServiceSchema, AServiceViewSpecification);

// Admission exists; displaying the field-specific reason needs host wiring.
LCount := NewNyxInput('retry-count');
LCount.Contract.Value(NyxIntegerDomain.Range(0, 9));
LCount.Configure.Text('Retry count').InputType(niNumber).Value(3).Done;

// Select currently couples stored identity and displayed label.
LState := NewNyxSelect('status');
LState.Configure.Items('Open' + #10 + 'Closed').Value('Open').Done;
// There is no current .Options/.LabelField API for independent option keys.
```

These examples additionally use `nyx.collections`, `nyx.collections.query`,
`nyx.collections.view.types`, `nyx.collections.query.editor` and `nyx.contract`.
`AServiceSchema` and `AServiceViewSpecification` are the actual immutable typed
schema/specification, not runtime widget handles. The domain checker refuses
incompatible fields/values; it does not supply a required-field visual workflow.

```pascal
// A separately chosen paged specification intentionally replaces the binding.
// Combine Query and Paged on one specification when both are desired.
LTable.Binds.Collection(
  NyxCollectionView(NyxCollection('services'))
    .Column(NyxTextField('name'), 'Service')
    .Paged(NyxCollectionPaging.PageSize(25)))
  .Done;
LPager := NewNyxPagination('services-pager');
LPager.Configure.PageView(NyxControl('services')).Done;

// The referenced menu must already be declared in the owning document.
LSplit := NewNyxSplitButton('create-actions');
LSplit.MenuButton.Configure.Menu(NyxMenuRef('document-actions')).Done;
// Its invocation owns the command snapshot; Menu semantic alone is not a popup.
```

Paging additionally uses `nyx.collections.paging`; saved menu declarations and
`NyxMenuRef` come from `nyx.menu.declarations`, with runtime services in `nyx.menu`.
These are existing
opt-in features. Runtime editing is admitted through the live binding/state/query
owner, rather than a descriptor setter being mistaken for an atomic live command.

## Review boundary

This artifact records current source and existing historical evidence; it does
not execute all controls or accept any later implementation task. Its review
finish line is met: the complete80-kind matrix, actionable gap ownership and
independent checks of completeness, source claims and dependencies pass. The original
component, parity, authoring, accessibility, performance and delivery criteria
remain intact. A later implementation must qualify its actual intended targets
and useful application workflow, with failures preserved.
