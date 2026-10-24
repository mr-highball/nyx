# Studio inspector groups

[Components](components.md) · [Retained disclosure](disclosure.md) ·
[Typed editor choices](../studio/nyx.studio.inspector.groups.pas)

Studio's Properties inspector groups existing public Nyx editors into
Component options, Data and bindings, Responsive layout and Properties.
Tab pages keeps its independent nested disclosure. Groups begin expanded;
users can collapse each header without losing the controls and drafts inside.
Empty component families are omitted. Component help, Properties/Events tabs,
placement commands and hierarchy remain directly accessible.

The wrappers are ordinary `INyxDisclosure` components, with named button/body
parts and Boolean expansion. Studio consumes the same public control as user
applications. Group choices do not enter the design, accepted Pascal,
unfinished source or paired Undo/Redo. Header activation captures presentation
immediately, without rebuilding the input or scroll frame.

The five roles use a closed `TNyxInspectorGroup` enum. Immutable
`TNyxInspectorGroups` values copy their fixed arrays explicitly on both targets:

```pascal
LChoices := TNyxInspectorGroups.Defaults
  .Expanded(nigResponsive, False)
  .Expanded(nigTabPages, False);
```

Capturing an absent group retains its choice through selection, inspector-tab
and compact-panel navigation. A containing section root resolves its exact
disclosure before capturing; a same-named wrong contract refuses. Native project
workspaces retain this value with their ordinary presentation state. Browser
project preferences use strict outer version 16 and a version-one nested object
of named Boolean roles. Versions 2–15 retain their original shape and default to
expanded groups; unknown fields and wrong scalar types refuse atomically.

The shared interaction policy evaluates the disclosure ancestor gate once.
Open sections avoid repeatedly parsing named-part policy. Header recognition
first checks for a button projection. This reduces repeated work in the deeper
inspector tree without caching mutable document facts or changing authored
visibility. Allocation receipts from checked fixtures are diagnostic evidence;
they do not establish production latency, frame budgets or complete performance.

Button style uses an independent retained form inside Component options. Its
copied baseline and accepted presentation advance together, so Apply and paired
Undo/Redo can refresh its values while preserving the physical inputs. The
publication retires the old callback generation before the fresh context becomes
active. Other forms keep independent unfinished drafts. Custom hosts composing
the public [button editor](../src/nyx.button.editor.pas) can declare
`NyxButtonEditorBaselineKey` and `NyxButtonEditorValueKey` together through
`TNyxProjectionFormContext` and the renderer's grouped form publication. Changing
the owner, form structure or undeclared creator metadata still requires a fresh
view; retaining controls grants no stale action authority.

Run `tools/build.ps1 -Target button-editor` with the configured FPC/Lazarus and
pas2js toolchains to check the public form and ordinary native Studio history at
desktop and compact allocations. `-ButtonEditorEvidenceDirectory` selects a new
output directory. The command retains checked native process/heap evidence and
stages the pure browser fixture; executing that fixture over HTTP remains a
separate check.

Qualification covers exact migration/copy ownership, actual browser/LCL headers,
collapsed shell refresh, retained form drafts and unchanged paired history.
The ordinary browser controller additionally preserves an unfinished Pascal
draft and Redo through group activation and inspector/view navigation, using
an exact MCP-authored companion. Trusted input and visual captures qualify
specific host paths. Native connected/project lifecycle, cross-launch recovery,
hardware/IME/assistive input, additional widgetsets, full visual quality and
preserving observing delivery remain separately owned requirements.
