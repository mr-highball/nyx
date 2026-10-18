# Captured Pascal projects

[Building](building.md) · [Source construction](source-projection.md) ·
[Current evidence](../WORK.md)

`nyx.pascal.project` describes an immutable compilation input, independent of
DOM/LCL, machine paths and a selected output. A project owns one main unit plus
ordered auxiliary Pascal units and include fragments. It reads no host files.

```pascal
uses nyx.pascal.names, nyx.pascal.project;

LInputs := NewNyxPascalProject(NyxPascalUnit('example.views'), LMainSource)
  .WithUnit(NyxPascalUnit('example.presenter'), LPresenterSource)
  .WithInclude(NyxPascalInclude('view-body.inc'), LViewBody);
```

Every edit returns a new snapshot. WithUnit/WithInclude replace an existing exact
identity without reordering it; earlier snapshots retain their original text.
WithoutUnit/WithoutInclude remove from the new snapshot only. Removing an absent
entry is idempotent. WithSource changes only the main text. The main unit cannot
be replaced through auxiliary membership.

MainUnit and Source identify the main file separately. Count/Input enumerate only
auxiliary entries; ByteCount includes every file. UnitReference is available on
unit entries and refuses on include fragments. Retained entries survive project
disposal and retain no back-reference to their project.

Pascal namespaces and include leaves are distinct typed references. Compiler
leaves use ASCII names and no machine paths, directory traversal or device aliases;
unit filenames follow their namespace plus `.pas`, include names end in `.inc`.
Case-only filename aliases refuse. Literal include references preserve exact
filename case, including on hosts whose filesystem is case-insensitive. Source
text retains exact Unicode/UTF-8 independently of these compiler-name rules.

Snapshots may temporarily reference an include added later. Validate checks the
complete inventory before compilation: declared unit namespaces, include
references, the Delphi/UTF-8 directive policy and existing external-file refusal.
Only the captured include leaves supplement that refusal. File clauses, external
linker/resource inputs and arbitrary search paths remain outside this contract.
Actual Pascal syntax/types and conditional compilation remain compiler decisions;
inventory admission never establishes successful construction.

Defaults allow 128 total files including the main unit and 32 MiB of UTF-8 text.
`TNyxPascalProjectLimits.Defaults.UpToFiles(...).UpToBytes(...)` tunes those values,
up to 4096 files/256 MiB. The existing source-construction main-unit limit remains
1 MiB; individual auxiliary files retain the text-file boundary's 4 MiB limit.
Budget refusal leaves the old snapshot intact. Exact byte counts are independent
of native UTF-8 versus browser UTF-16 string storage.

CaptureNyxPascalProject synchronously freezes alternate implementations, distrusts
their claimed byte counts/Validate method, and validates the resulting owned
inventory. A caller serializes a mutable provider during capture. The result has
no provider borrow and can be retained by a native compiler worker.

## Actual construction and application compilation

The native Studio compiler executor accepts the same typed project. Its existing
profile selects FPC/pas2js and host storage policy; no compiler path/options belong
to the portable input. Existing single-unit calls keep their previous contract.

```pascal
LConstruction := LExecutor.ProjectSource(LInputs, btNativeLCL, spcChecked);

if LConstruction.Projection.State = spsExecuted then
begin
  LReply := LExecutor.BuildProject(LConstruction, btBrowser, bsApplication);
end;
```

The receipt retains the exact frozen inputs alongside the ordinary projection.
Native construction can produce an independently owned admitted design. Browser
construction first produces a compiled worker and grants no design until actual
worker execution; BuildProject refuses that compiled-only receipt. Cross-target
application compilation from native construction proves compilation with the
captured files, not browser execution or equal behavior across target conditionals.

BuildProject consumes its executor-local captured receipt, including the original
helper meaning. It accepts no replacement input argument beside an older design.
A public metadata wrapper cannot attach a changed include to an old construction
merely because the main unit text matches. No serialized origin flag supplies
this attachment. Compiler and execution failures keep their distinct states and
retain their exact captured input; neither becomes an application build.

Use bsView with a control reference for a focused page/part and bsReusable for an
exact reusable root. The complete context constructs once, then an independent
focused clone mounts with the original root identities. Source text, callback
implementations and additional files remain exact. A full bsApplication build
omits a focused root. Native/application execution remains separate from both
constructor admission and compiler success.

Win32 staging creates only a fresh invocation and its own `project` directory.
Read-pinned source files stay exact through compiler/construction join. The
executor adds only that owned directory to fixed unit/include search arguments.
Ordinary library sources keep their existing separate read lease. Native
projection cleanup retires only its previously captured compiler derivatives;
project source remains evidence. Application outputs and browser packages remain
preserved. The configured storage-capacity check is a preflight, not a reservation.
Compiler/RTL/LCL/configuration identity and full cache reuse remain separate.

## Studio project ownership

`TNyxPascalProjectFiles.Capture(LInputs)` retains an immutable supporting-file
inventory for source checkpoints and Studio project values. Open reconstructs a
complete independent project against the exact main source. The main text remains
with its existing source frame; it is not duplicated in the file inventory.
Only managed text travels through records, including on pas2js.

```pascal
LPair := LSession.ProjectSnapshot;
LPair.Source := LConstruction.Projection.Source;
LPair.Design := LConstruction.Projection.Design;
LPair.Files := TNyxPascalProjectFiles.Capture(LConstruction.Inputs);
LSession.AdoptProjectedProject(LPair, LConstruction.Projection);
```

This explicit local admission requires the executed result's exact inventory,
source and design. Helpers may contain initialization behavior even when the
main source or rendered design is unchanged. Changing only those helpers is
therefore a real paired Undo command. A different inventory beside an older
projection or live checkpoint refuses. Source-only commands cannot silently
discard supporting files. The independent main draft/base retains its existing
history contract.

Portable project packets use version two only when a supporting-file value is
present. Existing six-field version-one bytes remain unchanged. Source snapshots
similarly add one explicit member only for a multi-file executed frame. Decoding
these values does not execute or reauthorize them. Native project-store journals
and exports retain all supporting text inside the complete packet; the established
adjacent main companion remains separate. An interrupted multi-file journal
remains compiler-required data until exact admitted completion. Current JSON/
project-store transport still has its existing **4 MiB encoded packet limit**.
A larger valid compiler project refuses that persistence boundary before
publication; its helper files are never truncated to fit.

Dedicated file selection/editing, independent helper drafts and complete observing/
recovery/publication quality still need integration under the existing authoring/
service owners. The installed MCP protocol/release has not
changed. Embedded images/data/localization keep the Resources abstraction; this
API adds no native linker-asset mechanism.

## Ordinary compiler commands

An accepted project now carries its exact supporting-file inventory through normal
queued Open, Pascal Apply and supported visual continuations. These commands edit
the main unit; helper bytes and limits remain with the captured checkpoint. A
compiler failure, cancellation or newer draft cannot replace the accepted pair.
One ordinary Undo/Redo retains the main source, helpers and independent draft/base
together. Include-aware import editing and callback-stub generation use only the
admitted inventory's literal include names; they do not add filesystem access.
Controller retirement detaches late completion delivery before releasing its
session. An already queued UI courier retires on a subsequent host UI turn.

`INyxProjectSourceCompiler` is an optional extension of the original compiler
strategy. `StartNyxSourceCompilation` selects it when a checkpoint has supporting
files. Native workers freeze the entire project before dispatch. Browser strategies
likewise require `INyxBrowserProjectSourceBuilder`; an older provider refuses
project work before receiving a completion port. Single-unit providers and their
original calls remain compatible.

The private source-compiler service carries a bounded supporting-file object with
the exact proposed main source. Existing receipt shapes remain unchanged. Trusted
receivers bind their locally captured request inventory to the authenticated job
result; a received metadata object cannot assert execution. Native decoding retains
actual construction, while a compiled browser worker still needs separate execution.
Shared strategies carry that same inventory through their existing revision-aware
transport. Whole-registry recovery and compiler-backed semantic adoption now retain
that inventory as described below. Ordinary observing-editor input, socket/browser
execution and preserving delivery remain separate qualification outcomes.

Ordinary application/view jobs now open supporting inputs from their exact accepted
source checkpoint. A pair/checkpoint mismatch refuses before a compiler slot is
allocated. No replacement helper argument can be attached to an older checkpoint.
The executor-local `BuildProject` receipt path remains available to trusted hosts;
transport metadata wrappers do not acquire that private application-build facet.

## Whole-registry recovery and semantic import

The private shared-runtime checkpoint uses version three when supporting files
occur in a current, Undo or Redo frame. Every such frame retains its complete
canonical inventory beside the exact main source. Drafts and their independent
bases keep their existing framing. Registries without supporting files retain
their exact version-one or version-two layout; old files remain readable. Existing
text/packet and whole-checkpoint budgets still refuse excessive data without
truncation. The format carries data, never execution authority or compiler paths.

Recovery identifies construction by the exact main source **and** supporting-file
value, including declared input limits. Equal main text with different helper
meaning requires independent verification. Repeated identical inputs across
projects and draft-only history execute once per load, while every saved design
is still compared. No execution cache survives that load or a process restart.

`INyxRuntimeProjectVerifier` supplements the original source-only startup strategy.
The native verifier returned by `NewNyxNativeRuntimeSourceVerifier` implements both
capabilities and freezes complete input before delegating to the existing owned
compiler. An older verifier refuses helper-bearing data before executing its main
unit. Automatic literal admission keeps such data pending for explicit recovery;
it never discards the helpers to obtain a parseable single unit.

Staged browser recovery reads one captured source/helper context and compiles it
through the existing private pool. A returned worker remains compiled-only until
actual execution supplies its bound producer message. The server binds its retained
inventory before checking exact saved meaning. Cancellation, malformed results and
an execution from a different helper context cannot advance the stage or rewrite
the original registry. Browser compilation is distinct from browser execution.

The existing `nyx_project` upload/review/apply workflow also accepts supporting-file
packets through compiler admission. Its job compiles the uploaded inventory rather
than the destination's helpers. A ready review has no adoption authority: the exact
owner/revision ticket must still be applied. One paired Undo/Redo restores accepted
source, helpers and unfinished draft/base together. Resolve the current draft before
adopting another project; ordinary draft-only synchronization preserves accepted
files and does not manufacture an additional history command.

The maintained `pascal-projects` target includes the complete-project native
recovery/import consumer. It uses actual constructors and the suspended production
semantic engine, never a listener or an invented execution flag. It additionally
checks the private deferred compiler protocol without executing a browser worker.
WORK.md owns accepted evidence, retained harness failures and the separate ordinary
observing/browser/delivery boundaries. Dedicated helper selection/editing and
independent helper drafts remain required under the existing authoring owner.

## Independent source-file buffers

`nyx.pascal.drafts` adds typed main/unit/include references and an immutable value
for unfinished helper buffers. Unit/include factories use the existing Pascal
namespace/include identities. References resolve exact authored members, with no
host paths or implicit case folding. Main references use the established main-unit
buffer rather than introducing a competing copy of that text.

```pascal
LHelper := TNyxPascalSourceRef.UnitFile(NyxPascalUnit('example.presenter'));
LSession.SetFileDraft(LHelper, LUnfinishedText);
LBuffer := LSession.DraftFileText(LHelper);
LAccepted := LSession.SourceFileText(LHelper);
```

Each helper buffer retains its accepted text at the first edit. Later edits keep
that original baseline, including when acceptance changes independently. Returning
to current accepted text closes just that buffer. Explicit discard likewise
addresses one typed reference. Empty text remains a pending buffer; incomplete
Pascal, exact Unicode and embedded NUL stay data. Encoding/size and membership
checks do not compile unfinished text. Accepted compiler inputs remain separate.

Copies preserve value semantics on both compilers through detached record arrays.
Sessions cache immutable accepted inputs for file lookup rather than reparsing the
whole helper inventory on each keystroke. Draft-only synchronization preserves
existing Redo; traversal captures the current helper buffers in the opposite entry.
Ordinary main-source application retains independent helper buffers.

Portable project packets add version three only when helper buffers are present.
Shared registry checkpoints similarly add version four for buffers anywhere in
current/Undo/Redo history. Absent buffers keep the existing older layouts. Recovery
validates buffer membership but verifies only accepted main/helper inputs, so
unfinished helper text cannot change construction or multiply the execution cache.
Helper buffers are data, never execution claims. Existing enclosing JSON/checkpoint
budgets still apply; excessive values refuse rather than truncate buffers.

`TNyxStudioSession.FileDrafts` exposes an immutable snapshot. `SourceDraftsPending`
combines main/helper pending state for deliberate whole-project replacement guards.
The main-only `SourceDraftPending` remains useful when an operation edits only main
text and preserves helpers. Saved project adoption must resolve all pending source
work through the existing explicit workflow.

This is the durable source-file editing contract. Buffer persistence alone does
not establish ordinary editor input or full parity. The maintained
`pascal-projects` target accepts `-PascalFileDrafts` to include
the buffer/session/history/recovery journey and compiles its browser consumers.

## Selecting and applying a source file

The current-source Pascal pane uses an ordinary Nyx Select to list the main unit
and its captured units/includes. Switching files keeps each independent buffer;
the same code editor moves between inline and expanded presentation. Apply,
Restore accepted and Save draft address the selected file. Selection belongs to
the local editor context and grants no compiler or document authority.

```pascal
LHelper := TNyxPascalSourceRef.IncludeFile(NyxPascalInclude('customer-rules.inc'));
LSession.ActiveSourceFile := LHelper;
LSession.SetFileDraft(LHelper, LEditedRules);
LCommands.ApplyFile(LHelper);
```

Helper Apply captures accepted main/other files plus only the selected draft.
The isolated compiler constructs that complete candidate. Publication verifies
the sealed owner/load, schema, accepted baseline and selected buffer before
changing the accepted pair in one Undo step. It consumes only that helper draft,
retaining unfinished main/other helper text and first bases. Repeated waiting
Apply intents coalesce per file; another file's intent stays queued. Stale bases,
older providers and substituted compiler inputs refuse rather than silently
applying the main unit or dropping files.

The Undo entry captures the dispatch frame on both peers. Newer unrelated typing
stays current after publication and is retained in the opposite Redo entry if
Undo deliberately restores that earlier frame. Restore retires only the selected
file's Apply intent; it preserves another file's active compilation and queued
visual work. Visual compilation likewise keeps unfinished helper buffers outside
its disposable literal planner and retains their exact history snapshots.

The shared private compiler protocol carries a typed selected-file intent. Its
owning backend independently prepares the acknowledged file change and checks
the exact candidate inventory. Newer unrelated local typing remains retained
through publication and the following observing acknowledgement. No serialized
file field grants execution provenance, and no new MCP tool/schema is introduced.
Callback navigation still returns to the main buffer. Current-source compiler
reports can retain typed helper sites and their exact submitted text. A failed
file Apply checks that context against its sealed candidate and current target
buffer before offering Go to. Switching files retains the error's target;
navigation selects that buffer without replacing text or adding history. Editing
the target retires the old location. Legacy/unmapped providers keep readable
messages with no invented coordinates. General declaration navigation, semantic
helper navigation and completion remain separate outcomes. WORK.md owns the
ongoing actual editor qualification; this contract alone proves no target input.

## Semantic source-file windows and grouped edits

Current-source `nyx_pascal` adds `mode:files`: accepted main/unit/include
identities in authored order, twenty by default and fifty maximum. Use `offset`
and `limit`, and pin `expectedRevision` while paging. Each entry contains a
round-trippable `file` selector, title and pending-buffer indicator. No source
text or host path is returned. Main uses `{"kind":"main"}`; helpers use
`{"kind":"unit","name":"example.presenter.pas"}` or
`{"kind":"include","name":"presenter-body.inc"}`. Names/extensions/case must
match the accepted inventory exactly; missing helpers never fall back to main.

Pass that selector to `mode:unit` to read an accepted helper window. The existing
zero-based Unicode-scalar `offset` or one-based source `line` selects the start;
`count` remains capped at 4096 scalars. A helper's unfinished buffer stays local,
and `pendingDraft` reports it without substituting that text for accepted input.
Omitting `file` retains the original main-unit query/response contract.

Each `mode:edit-unit` change can likewise carry `file`. Omitted selectors retain
main-source semantics. One group can interleave main, unit and include ranges;
each file's ranges must remain ascending and nonoverlapping in its ORIGINAL
accepted text. The sixteen-change and 262144-scalar expected/replacement-side
budgets apply to the entire group. Exact expected text, current revision, owning
transport, operation identity, permission and all pending-source guards remain.
Membership, names, main identity, order and limits cannot change through ranges.
Use deliberate Files authoring for inventory changes; this schema does not yet
add semantic creation/removal or declaration/completion operations for helpers.

The typed preparation API shares the existing scalar patcher:

```pascal
LFile := TNyxPascalSourceRef.IncludeFile(NyxPascalInclude('presenter-body.inc'));
LPatch := NyxSourceProjectPatch([
  NyxSourceFileEdit(LFile, NyxSourceEdit(LOffset, LExpected, LReplacement)),
  NyxSourceFileEdit(TNyxPascalSourceRef.Main,
    NyxSourceEdit(LMainOffset, LMainExpected, LMainReplacement))
]);
LProposedInputs := LPatch.PrepareProject(LAcceptedInputs);
LRequest := LSession.PrepareSourceEditRequest(LProposedInputs, NyxSchemaRevision);
```

Preparation returns independent validated source data and a sealed request. It
does not compile, execute, replace visible buffers or publish a document. The
owning semantic compiler constructs the exact complete project before one paired
Undo publication. Supporting-file groups require that compiler even when main
text is unchanged or its prior origin is literal. A newer main/helper draft,
changed revision/context/profile or failed construction/publication refuses the
whole result. Existing main-only declarative admission stays separate.

For these grouped project jobs only, bounded `nyx_build` diagnostic rows can add
`submittedSource:{file,line,column}`. Those one-based Unicode-scalar coordinates
belong to the immutable proposed file. Existing `navigable`, `line` and `column`
retain main-only current-source navigation meaning; a helper failure never
masquerades as a main-file caret location. No proposed source is returned in the
diagnostic row. Poll `publication.state` and its revision to establish success;
compiler success alone does not establish paired editor publication.

This extends the existing tool in current source, with no new tool name or
installed-release claim. WORK.md owns actual native/compiler/observing evidence
separately from browser compilation and the existing HTTP/delivery gate.

## General declarations

The public `CaptureNyxPascalSymbolIndex` inspects declared structure from an exact
accepted/editing snapshot and explicit owning unit. It shares include expansion,
Unicode physical positions and typed byte/depth/occurrence limits with routine
inspection. No host files, compiler, mutable editor or source provider survive
capture. The immutable result exposes at most 4096 declaration-order entries.

```pascal
LSources := CaptureNyxPascalSources(LAcceptedInputs)
  .WithBuffer(LUnitReference, LCurrentUnitText);
LDeclarations := CaptureNyxPascalSymbolIndex(LSources, LUnitReference);
LDeclaration := LDeclarations.SymbolAt(LOrdinal);
LLocation := LDeclaration.Location;
```

Each `TNyxPascalSymbolSite` has a typed kind/visibility, name, lexical scope,
qualified name and physical location. Types, constants, global variables,
resource strings, fields, properties, enum members and routine headers remain
distinct. Grouped names get individual positions. Nested structured types,
generic type headers, class helpers and variant-record fields keep their containing
scope; implementation locals are not relabeled as globals. Routine prototypes
and implementation headers are separate physical rows. Default class access is
unknown until an explicit visibility section; effective inherited access is not
inferred.

This catalog describes declarations rather than resolving compiler symbols.
It does not evaluate expressions, follow imported unit graphs, select conditional
branches or determine inherited members/types. Full generic method identity,
overload resolution and completion still require compiler-aware context.
A conditional boundary reports
unresolved context; later scopes are not guessed. Strict accepted lexical errors
refuse. Editing snapshots may retain a complete prefix with physical problem data
through `INyxPascalRoutineReadStatus`; hard quotas/cycles still refuse the index.
An unfinished include type cannot borrow a later routine/unit closing token.
The absence of a reported problem proves no compiler acceptance.

The ordinary Outline's **Show** selector offers **Routines** and **Declarations**.
Both share owner, accepted/editing origin, full-fold search and twelve-row pages.
Declaration search includes names, scopes, kinds and visibility; its single
Declaration action selects the exact physical file/caret using the retained code
editor. Existing snapshot/load/mount/target guards remain, as do independent
buffers and paired history. General source completion remains separate.

Current-source `nyx_pascal` exposes the same catalog with `mode:"symbols"`.
Use `file`, explicit `owner` for includes, optional `query` (512 scalars),
`offset` and `limit` (20 default, 50 maximum). Query matching uses Nyx's shared
Unicode folding. Rows contain `ordinal`, `name`, `scope`, `qualifiedName`,
`kind`, `visibility`, `location` and `editable:false`, without source text.
Include queries return only rows physically belonging to that fragment.
The response labels `inspectionOnly:true` and includes bounded `problem` data.
Optional `snapshot:"editing"` requires the exact acknowledged `expectedRevision`;
omitting snapshot retains accepted-source inspection. New revisions retire the
cached catalog. Names/ordinals do not grant editing or compiler authority: use
bounded physical source windows and the existing guarded authoring operations.

This adds one read mode to the existing current-source tool, with no new tool or
installed-schema claim. WORK.md separates native editor/semantic evidence,
compiled-only browser consumers and remaining grammar/quality/parity/delivery.

## Studio routine outline

The source workspace's **Outline** tab shares the public include-aware routine
index with semantic inspection. Its default **Editing buffers** snapshot follows
the current main/unit/include text. **Accepted code** remains an explicit choice.
Choose an owning main/helper unit, type a name
or signature fragment, then choose **Find**. Explicit search keeps the live input
mounted while typing. Previous/Next show twelve matching routines at a time;
an owning unit remains explicit because an include can have several contexts.
Matching reuses the type-ahead engine's Unicode 17 default full folding, including
expansions and supplementary case pairs. Authored text and query fields remain
exact; search performs no locale conversion, normalization or accent removal.

Each row has separate Definition, Declaration and Body actions. Their hints name
the physical file and one-based line/column. Undefined or split sites are disabled;
an unresolved counterpart never substitutes a line in the main unit. Read-only
routine reasons remain visible. This is lexical routine discovery, not compiler
type/symbol completion or a general declaration outline.

Navigation selects the actual source file and moves the retained editor's caret,
including in expanded mode. It creates no Undo command and changes no accepted
source. Editing-buffer positions follow shifted unfinished files without applying
them. Newer typing invalidates an older buffer snapshot before navigation.
Accepted-code positions still refuse a changed **target** file; choose Editing
buffers to browse it, or apply/explicitly restore the file. Unrelated drafts stay
independent. Source/load changes and retired controls invalidate old actions.

When a quote, comment or routine body is unfinished, editing inspection retains
only trustworthy complete routines before that fault. It never inserts closing
tokens or invents later scope. Feedback names the physical buffer and position;
unfinished implementations anchor at their opening token. Missing/split locations
stay undefined. Hard byte/token/routine/depth/include-cycle limits return no index.
An absent inspection problem is not proof of correct Pascal or successful build.

`nyx.pascal.outline` supplies the reusable Nyx card, typed counterpart selectors
and action decoder. `nyx.studio.sourceoutline` adds Studio's snapshot/mount/draft
guards. Its managed owner holds copied discovery data without retaining a session
or widget. Copied view records borrow its presenter to respect pas2js's record
limitations. Discovery resets on a load switch; it is not persisted project data.
The native and browser controllers consume the same component and route, leaving
caret/focus placement to their existing source-editor adapters. WORK.md separates
actual native input from browser compilation and remaining visual/parity gates.

### Read-only buffer snapshots

`nyx.pascal.sources` separates inspectable text from `INyxPascalProject` compiler
inputs. Accepted membership fixes file identities; editing a namespace cannot
rename/remove/add a file through this API. Each immutable `WithBuffer` copy accepts
empty or unfinished Pascal, supplementary Unicode and embedded NUL, with a 4 MiB
per-file bound and the captured project's total byte budget. Capturing an alternate
provider freezes its getters and validates encoding/membership/budgets without
interpreting a draft as an application. Callers serialize mutable providers during
capture; snapshots retain no session, provider, widget or compiler authority.

```pascal
LSources := CaptureNyxPascalSources(LAcceptedInputs)
  .WithBuffer(LIncludeReference, LUnfinishedText);
LIndex := CaptureNyxPascalRoutineIndex(LSources, LOwningUnit,
  TNyxPascalRoutineLimits.Defaults.UpToDepth(48));

if Supports(LIndex, INyxPascalRoutineReadStatus, LReadStatus) then
begin
  LProblem := LReadStatus.ProblemLocation;
end;
```

The snapshot cannot be used as a compiler project. `AcceptedText` is its captured
accepted comparison, not a replacement for the editor's independent first-edit
base/history. Buffer inspection never admits source, compiles it or publishes an
accepted pair. The existing compiler-project index overload remains strict.

### Semantic buffer context

Current-source `nyx_pascal` supports `snapshot:"editing"` on `files`, `unit`,
`routines`, `routine`, `declaration` and `symbols` reads. An editing read requires the exact
`expectedRevision` of the acknowledged server state. Unsynchronized keystrokes
are not part of that revision. Use the usual bounded pages/scalar windows and
exact file/owning-unit selectors; selecting a buffer never changes membership.

For example, after ordinary typing has been acknowledged, request a small helper
window with the revision from that shared context:

```json
{
  "mode": "unit",
  "snapshot": "editing",
  "expectedRevision": 12,
  "file": {"kind": "unit", "name": "example.presenter.pas"},
  "line": 20,
  "count": 512
}
```

Explicit reads label their `snapshot`. Editing responses add `inspectionOnly:true`;
routine rows/windows always report `editable:false`. Their physical locations
belong to the captured current buffers, including shifted helper/include text.
Recognized unfinished source retains only complete prefix routines and adds a
bounded `problem` with `defined`, message and physical location. The absence of a
reported problem does not prove syntax correctness or compilation. Hard budgets
and include cycles still refuse the index; raw text windows remain independent
of indexing.

Omitting `snapshot` retains accepted text and the existing successful response
shape. Explicit `snapshot:"accepted"` labels that origin without granting access
to editing text. Mutations reject the selector: inspected buffer text cannot
replace the accepted expectations, draft guards, compiler construction or paired
publication required by ordinary edits. Reads change no selection, source,
history or revision. A new acknowledged revision retires cached text/indexes;
an older revision refuses rather than silently rebasing a window.

This extends the existing current-source schema, with no new tool or installed
release claim. WORK.md separates native semantic/editor evidence from compiled-
only browser products and remaining HTTP/parity/delivery qualification.

## Project routines and physical source locations

The current-source Pascal tool accepts exact `file` selectors on `routines`,
`routine` and `declaration` reads, plus an optional `expectedRevision`. Omitting
the selector selects main. Captured-file projects automatically use include-aware
context; projects without captured files retain legacy managed-main behavior.
Selecting a main/helper unit
indexes that translation unit and its nested includes. Selecting an include also
requires `owner`, an exact main/helper selector: the same fragment may have
different scope in different units. No include context is guessed.

`routines` remains paged (20 default, 50 maximum). Each row reports a stable name,
kind, revision-local ordinal, editability/reason and separate physical locations
for its definition, body, implementation signature and interface counterpart.
Locations use zero-based Unicode-scalar offsets and one-based line/column values
in the original accepted file, never the concatenated expansion. Feed those file/
line values into existing `unit` windows for surrounding context. Queries do not
move the observing editor or publish source.
The simple `line` field remains scoped to the queried file and is zero for a
location in another leaf. A default main query cannot redirect a caret to an
include's line by treating it as a main-unit location.

`routine` and `declaration` return at most 4096 scalars of the requested accepted
text. Concatenate windows at one revision for an exact expectation. Declaration
`part` chooses interface or implementation; unresolved/conditional counterparts
report `visibility:unknown`. A defined `textSource` identifies the exact physical
span. An undefined location deliberately omits file/caret coordinates and the
window contains expanded read-only text. Nested routines remain inside their
parent. Overloads remain distinct discovery rows; inspecting one needs its ordinal
and exact revision. Ordinals never become focused mutation identities.

Bodies retain authored include directives when both boundaries belong to one
physical occurrence. Bodies split across different files/occurrences remain
readable and explain why focused replacement refuses. Use explicit grouped file
ranges for broader changes. Physical text and `pendingDraft` refer to the returned
span; `pendingDrafts` also reports unfinished work elsewhere. Pending buffers are
not returned as accepted routine text.

`edit-routines` items can supply `file` and `owner` with the existing routine,
expected and implementation fields. Explicit contexts, and admitted handwritten
main projects, use one complete compiler-backed project candidate and one paired
Undo publication. No visible buffer is staged. A name must be unambiguous in its
owner; signatures, calling conventions and siblings stay retained. Source-data
preparation rejects scope escapes before compilation. Compiler diagnostics, failed
publication and late ordinary typing preserve the complete accepted pair/drafts.
Legacy main-only literal admission remains separate.

```pascal
LOwner := TNyxPascalSourceRef.UnitFile(NyxPascalUnit('example.presenter'));
LIndex := CaptureNyxPascalRoutineIndex(LAcceptedInputs, LOwner);
LSite := LIndex.Item(LChosenIndex);
LPatch := NewNyxProjectRoutinePatch([
  NyxProjectRoutineEdit(LOwner,
    NyxRoutineEdit(LSite.Routine, LSite.Body, LImplementation))
]);
LProposedInputs := LPatch.PrepareProject(LAcceptedInputs);
```

The immutable index copies text/locations and outlives its input provider. Browsing
is lexical, not compiler type resolution or target-conditional execution. Cycles,
unsupported/incomplete syntax and bounds refuse without a partial index. Typed
`TNyxPascalRoutineLimits` defaults allow 32 MiB expanded UTF-8, depth 32 and 4096
include occurrences; callers can tune these independently of project limits.
Sparse physical Unicode cursors bound location lookup scans. Studio caches one
owning-unit index at the accepted revision and retires it on change. Shared include
edits intentionally affect every user of that physical leaf; compile the complete
project to check the used dependency closure.

This extends the existing tool in source only. It does not install a schema,
execute browser consumers or prove a general declaration/symbol outline,
type/symbol completion, full grammar, performance budgets, parity or delivery.
WORK.md owns exact qualification, retained failures and those separate owners.

## Semantic source-file membership

Current-source `nyx_pascal` also supports `edit-files`: typed helper creation or
removal with optional related exact ranges in `edits`. It always queues one
complete constructor and publishes one paired Undo step. For example, add an
include and its directive together, or remove a helper and repair its callers
together. No dependency text is silently rewritten.

Each `changes` item has `op:create|remove` and the same exact unit/include `file`
selector used by file windows. Creation optionally supplies `text`; otherwise it
uses the English Delphi/UTF-8 template. Removal has no text. Main cannot be
created/removed. A membership target appears once and cannot also receive ranges.
Ranges address other ORIGINAL accepted files. Combined operations are limited to
16; creation text and replacements share 262144 scalars, with the existing separate
262144-scalar expected-text budget. Project identity, order of retained files and
configured limits stay retained. Used dependencies are validated/compiled; unused
helpers remain captured data, not a standalone type-check claim.

For any removal, first call `review-files` with `expectedRevision`, the exact
`changes` and optional `edits`. It returns named removals, a warning and `reviewID`
without compiling or changing history. Supply that ticket on `edit-files`, along
with the unchanged group, revision and a unique `operationId`. Eight reviews are
bounded; each belongs to its transport owner and expires on revision/disconnect.
Creation-only groups need no ticket. Missing/changed reviews, pending drafts,
stale revisions and compiler/publication failure preserve the complete pair.
As with range edits, text typed while compilation runs prevents publication.

Code-first preparation uses the same typed intents:

```pascal
LPatch := NyxSourceProjectMutation([
  TNyxPascalFileChange.CreateInclude(NyxPascalInclude('presenter-body.inc'))
    .WithText(LBody)
], [
  NyxSourceFileEdit(TNyxPascalSourceRef.Main,
    NyxSourceEdit(LOffset, LExpected, LReplacement))
]);
LProposedInputs := LPatch.PrepareProject(LAcceptedInputs);
```

Preparation is detached source data. The ordinary owning compiler must construct
the exact complete inputs before accepted publication. `INyxPascalFileChanges.Stage`
deliberately permits private intermediate dependency gaps; `Candidate` validates
its complete final project. A staged value is never execution authority.
Poll the existing build job's publication result, rather than infer publication
from a compile result. This extends the existing source tool; installed schema,
authenticated HTTP/browser input, full parity and delivery remain separate.
WORK.md owns qualification and retained failures.

## Creating and removing source files

The current-source **Files** tab consumes the public Nyx source-file card. It
offers a typed unit/include choice, keeps unfinished names visible after refusal
and identifies the exact target before removal. Main source cannot be removed.
The same command queue owns local and shared compilation and paired history.

```pascal
LCommands.ChangeFile(TNyxPascalFileChange.CreateUnit(
  NyxPascalUnit('example.presenter')));
LCommands.ChangeFile(TNyxPascalFileChange.CreateInclude(
  NyxPascalInclude('presenter-body.inc')));

LReview := LSession.ReviewFileRemoval(LHelper);
{ Present the named-file warning and wait for the user's explicit confirmation. }
LCommands.ChangeFile(LSession.PrepareReviewedFileRemoval(LReview, NyxSchemaRevision));
```

Creation refuses existing identities, including case aliases. Removal requires
the exact captured helper. A review seals the session/load and accepted/draft
target bytes; changed targets or another project refuse before confirmation can
prepare a candidate. Commands retain independent main/other helper buffers and
first bases. A successful removal consumes its named draft deliberately; one
Undo restores that file and draft/base together. Inventory commands keep FIFO
order and do not coalesce with selected-file Apply or Restore.

No uses/include clauses are silently rewritten. Add dependencies explicitly
where the helper belongs. Project compilation checks the used dependency closure;
unused units/fragments remain captured source data, not a standalone type-check
claim. Removing a still-used include or unit refuses validation/compilation while
retaining the accepted pair. A captured project with zero helpers retains its
explicit main identity and limits; that value differs from an absent project.

Private shared intent names one closed create/remove. The backend reconstructs
the proposed change against its acknowledged inventory and independently verifies
all retained files. Older strategies refuse the optional capability. Saved files
or wire fields grant no compiler, execution or publication authority. There is
no separate tool for this private intent; semantic membership is described above.
WORK.md owns native/compiler and compiled-only browser
qualification; full authoring/semantic analysis/parity/delivery remain separate.

`-PascalSourceFileEditors` adds the maintained ordinary native controls mode to
the `pascal-projects` target. That mode uses real local/shared compiler workers
and wide/compact Win32 controls without a listener or captures. Socket transport
is substituted by the existing production-backend consumer. WORK.md distinguishes
actual native input from browser compilation and retains failed preparations;
neither this API nor a compile pass proves browser input, full parity or delivery.

`tools/build.ps1 -Target pascal-projects -PascalProjectEvidenceDirectory <NEW-path>`
runs the maintained data checks and actual native compiler/session/history/store
journey, then compiles the browser consumers. It never launches an HTTP host or
browser, refreshes enrollment or changes an active Studio project. WORK.md owns
accepted evidence and retained failures; compiler success is distinct from actual
application/control execution and complete parity.

`tools/build.ps1 -Target compiled-runtime -CompiledRuntimeProjectInputs
-CompiledRuntimeEvidenceDirectory <NEW-path>` runs the maintained captured project
and real native control journey, including both-provider application/view jobs,
main/helper includes, independent helper-only revisions, failure/receipt guards and
native source-pin retirement. Project-input mode also exercises ordinary queued
Open/Apply/visual edits and the production private compiler pool, exact retries,
owner refusal, cancellation and accepted-checkpoint application submission. It
additionally compiles the matching browser consumers. It starts no listener/browser
and changes no enrollment/active project.
WORK.md separates actual native evidence from compiled-only browser results and
the remaining full service/editor/parity/delivery outcomes.

## Nyx-owned Pascal analysis

`nyx.pascal.lexing` is the first layer of the user-selected fresh MIT-licensed
engine. Its immutable snapshot owns exact physical text, including whitespace,
comments, compiler directives, quoted delimiters, character-code spellings and
unfinished/invalid tokens. One zero-length EOF closes every stream. Public ranges
count Unicode scalars, with one-based lines/columns and CRLF treated as one newline;
native byte and browser UTF-16 indices stay private. Malformed Unicode refuses.
Unfinished strings recover at the next physical newline; unfinished comments retain
their remaining text and diagnostic. Nothing here admits or executes Pascal.

Closed word/symbol/numeric enums describe spelling without claiming every spelling
is legal in every compiler mode. Defaults use ASCII identifiers and Delphi-style
non-nested comment termination. Nested comments and category-based Unicode
identifiers are explicit independent policies; Unicode analysis is not compiler
eligibility. Options bound UTF-8 source bytes on both targets, tokens including
trivia/EOF, diagnostics and comment depth. Exceeding a quota or cooperative
cancellation refuses the complete candidate instead of returning truncated context.

`nyx.pascal.syntax` retains declared type/signature expressions, generic/ancestor
ranges, authored visibility, grouped fields/parameters, properties, overload links,
imports and lexical scope containment. Module, declaration, type, member and
interface-declaration fragments have explicit starting contexts. Name lookup is
ASCII case-insensitive within one exact scope; it follows no imports or inheritance.
Unspecified member access remains unspecified instead of guessing compiler modes.
Scope/declaration ordinals belong only to that immutable snapshot. Scope `Complete`
describes closed boundaries; it never certifies grammar or successful compilation.

```pascal
LAnalysis := NewNyxPascalSyntaxSnapshot(LPhysicalText,
  TNyxPascalSyntaxOptions.Defaults
    .Fragment(psfTypes)
    .UpToDeclarations(4096));
LBadgeType := LAnalysis.Named(0, 'INyxBadge');

if LBadgeType >= 0 then
begin
  LMembers := LAnalysis.Declaration(LBadgeType).NestedScope;
  { Inspect declared syntax only; forward duplicates retain NextNamed links. }
end;
```

Includes and unevaluated conditional branches produce explicit context diagnostics.
Both branches remain marked rather than silently choosing one. Unsupported
anonymous/inline/variant constructs retain uncertainty in their affected scope;
they grant no authority to infer an outer declaration. Type expressions remain
source spans, rather than resolved aliases, generic substitutions or compiler types.
The original admission lexer/index remains unchanged and independent.

WORK.md records native lossless corpus and declared-scope checks separately from
browser compilation. Captured include/conditional/compiler context, richer type
and statement syntax, name/type/member/overload/helper binding, incremental
dependency invalidation and provider/ordinary-editor adoption remain ahead. This
layer introduces no MCP mode, installed resolver, source edit or rollout. Full
navigation, project-scale budgets and both-target execution/quality remain under
the existing authoring/service owners.

## Captured compiler and include context

`nyx.pascal.context` owns immutable compiler metadata and conditional facts.
Compiler families/modes, three-valued knowledge, define kinds and language features
use closed types; open symbol names use `NyxPascalDefine`. Missing symbols stay
unknown until the caller explicitly seals its define set. An explicitly unknown
name stays unknown even in a sealed set. Version metadata never silently creates
FPC/PAS2JS/platform defines from the analyzer's own build. Unknown family/version
metadata supports inspection before optional output/compiler configuration.

Facts include exact signed 32-bit condition integers, short-option state and
typed nested/anonymous/function-reference callback features. Persistent changes
detach data; source mode changes revoke option/feature defaults rather than guessing
them. Condition analysis supports `defined`, integer comparisons, parentheses,
Boolean operators and unary signs with Pascal precedence. General arithmetic,
replacement macros, strings, `declared`/`sizeof` and ambiguous conditional spellings
remain explicitly unresolved. This is a bounded subset of the compiler's
[conditional grammar](https://www.freepascal.org/docs-html/prog/progsu126.html),
not a replacement for compiler admission.

`nyx.pascal.files` captures normalized relative virtual names and exact source text,
with explicit case comparison and include search order. Current physical directory
precedes the captured search directories. Relative parents normalize inside the
virtual capture; an escaping candidate cannot hide a later contained match. There
is no filesystem fallback. `nyx.pascal.files.project` separately freezes existing
main/unit/include editing buffers, preserving accepted ownership and keeping the
project/code-generation model out of the standalone analysis core. SDK sources
can be explicitly captured without entering the application's admitted project.

```pascal
LFiles := CaptureNyxPascalProjectFiles(LCurrentBuffers, pfcExact);
LCompilerFacts := NewNyxPascalCompilerContext(ppcFPC,
  TNyxPascalCompilerVersion.Create(3, 3, 1), ppmDelphi)
  .WithSymbol(NyxPascalDefine('FPC'), ppdDefined)
  .WithSymbol(NyxPascalDefine('FPC_FULLVERSION'), ppdInteger, 30301);
{ The host supplies its actual captured facts; this example leaves absence unknown. }
LTranslation := NewNyxPascalTranslation(LFiles,
  NyxPascalProjectPhysicalFile(LCurrentBuffers, LOwnerFile),
  LCompilerFacts, TNyxPascalTranslationOptions.Defaults);
```

`nyx.pascal.translation` retains physical token spans and a separate occurrence
for every include. Entry/exit states preserve textual define/option changes, even
when the same file appears again under different facts. Conditional frames can
cross include boundaries. Every token records its pre-token state and known active,
inactive or unknown participation; this is not semantic validity. Inactive includes
perform no lookup or mutation. Unknown branch writes revoke the affected fact;
uncaptured includes revoke all mutable state knowledge and stop interpreting later
directives, since their lexical defaults could also have changed. Repeated recursion with
unchanged state diagnoses a cycle, while a changing include guard can finish.

The snapshot's `Resolved` flag covers encountered supported translation constructs,
not complete compiler context, grammar, type binding, compilation or execution.
Missing files, unknown expressions, unsupported directives and lexical errors
remain physical diagnostics. Fixed lexical policies are explicit: transitions that
need different comment termination or uncaptured mode defaults stop interpreting
later directives and mark participation unknown. Full dynamic lexical-mode support,
broader compiler expressions/macros and complete SDK/compiler-default capture
remain required before general language coverage can be claimed.

Tokens, expanded bytes, depth, occurrences, states and diagnostics have independent
positive fluent budgets. Limits/cancellation refuse the complete candidate. The
current native checks include Nyx's real callback gate and a matching actual native
compiler assertion; older/browser profiles are captured-data simulations. Browser
consumers compile only. WORK.md owns exact receipts and remaining incremental/
project-scale cancellation/budgets. Structured type expressions, semantic binding,
provider/editor adoption and full platform/quality/delivery remain open. No MCP
tool/schema, default engine or active design is changed by these primitives.

## Structured type syntax and physical origins

`nyx.pascal.types` parses a bounded lexical window into an immutable typed arena.
Nodes describe names/qualification, generic bases/arguments, pointers, arrays and
dimensions, sets, files, short strings, class references, distinct aliases,
enumerations/subranges and procedural/function-reference signatures. Parameter
groups retain names, passing modes and one type child; defaults/bounds retain exact
opaque syntax for later constant binding. ABI, method/nested/packing and explicit
specialization qualifiers are typed values. No constant or authored code executes.

Nested generic closers split `>>`/`>=` contextually without mutating lexical tokens.
The declaration parser also preserves the split between `TBox<T>` and the adjacent
alias equals sign in `TBox<T>=class`. Native qualification includes an actual
compiler declaration with that spelling. Unsupported/incomplete grammar stays
diagnostic; `Complete` means recognized type syntax, not compiler eligibility,
resolved type identity, generic substitution or successful application compilation.
Structured class/member bodies remain with their owning declaration scopes.

`nyx.pascal.syntax.translated` exposes known-active declarations across captured
includes. It creates an **analysis-only projection** with explicit separators
between significant tokens. That text must never replace authored buffers or be
submitted as a source edit. The exact original files remain in `Source`. Every
projected token maps to physical file/scalar span, inclusion occurrence and its
pre-token compiler state. `OriginAt` refuses synthetic separators/EOF; `Part`
maps an intersected range without rounding Unicode or compound-symbol boundaries.
Repeated inclusions retain independent identities. The first significant unknown
participant ends a visible incomplete prefix instead of selecting a guessed branch.

```pascal
LDeclared := NewNyxPascalTranslatedSyntax(LTranslation,
  TNyxPascalSyntaxOptions.Defaults);
LDeclaration := LDeclared.Syntax.Named(0, 'TReply');

if LDeclaration >= 0 then
begin
  LType := LDeclared.TypeOfDeclaration(LDeclaration,
    TNyxPascalTypeOptions.Defaults, LCancellation);
  { Inspect typed nodes; retain LDeclared for their physical origin mapping. }
end;
```

Type windows allocate their own token interval rather than the remainder of an
owning unit. Node/depth/diagnostic budgets and cancellation refuse complete
candidates; no truncated arena escapes. No editor/UI/compiler capability is retained.
WORK.md records real controls/include declaration and specialized interface result/
parameter evidence, exact supplementary-scalar mapping and native compiler oracles.
Browser consumers compile only. This adds no Studio/default provider/MCP mode or
source edit. Scoped name/alias/member/inheritance/interface/generic/overload/helper
binding, broader grammar/context, incremental budgets/navigation and full platform/
quality/delivery remain with the original authoring/service owners.

## Scoped name and type binding

`nyx.pascal.binding` owns a persistent module world over captured translated views.
Exact-world interfaces retain caller sites, declarations, types and bounded
responses. Replacement detaches the module vector; old handles keep their original
world and refuse use in the new one. No result/type/site is cached back into its
world, so reference-counting graphs retain no reverse cycle to an editor/document.
The world accepts matching compiler family/version metadata; complete target/default/
SDK coherence remains part of the broader context requirement.

Lookup preserves lexical shadowing, visible declaration order, direct unit exports
and reverse `uses` order. Qualified units do not gain visibility merely by existing
in the capture. Implementation-only globals remain unavailable to another unit;
own-unit qualification retains caller-position visibility. A later uncaptured
import refuses rather than selecting an earlier candidate, and `RelatedName`
identifies the needed unit for focused capture. These choices follow FPC's
[unit-scope rules](https://docs.freepascal.org/daily/doc/ref/refsu108.html).

Type binding follows explicit aliases, distinct wrappers, visible forward identities
and their later completion, and supported structured/constructed/procedural shapes.
Names inside callback parameters/results bind through the same scopes. Language
intrinsics carry typed identities; sizes, ABI/layout and opaque bounds/defaults
are not inferred. Default string meaning requires captured option/feature facts.
This is declaration/name binding, not complete compiler type checking or source
admission. Generic substitution, general expression/call inference and overload
selection still return explicit unresolved states.

Ordinary member lookup follows supported class/interface/record/object ancestors
with cycle/depth guards and an explicit instance/type receiver mode. Private is
unit-scoped, strict private is restricted to its owning class, and protected forms
use the caller's class/ancestry. Supported checks follow the documented
[class visibility rules](https://docs.freepascal.org/docs-html/current/ref/refse35.html).
Unmodeled scopes, missing ancestors, compiler context and ambiguous declarations
stay explicit. Complete helper/receiver-specific visibility, inherited overload
and generic rules remain qualification requirements, not implied compiler parity.

```pascal
LWorld := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults)
  .WithModule(LDeclared);
LCaller := LWorld.At(0, 0, LWorld.InterfaceBoundary(0));
LFound := LWorld.Lookup(LCaller, NyxPascalBindingName('INyxBadge'));

if (LFound.Status = pbsBound) and (LFound.Count = 1) then
begin
  LType := LWorld.TypeOfSymbol(LFound.SymbolAt(0));

  if LType.Status = pbsBound then
  begin
    LMember := LWorld.Member(LType.BoundType, LCaller,
      NyxPascalBindingName('WithText'));
  end;
end;
```

Explicit declaration inspection does not confer program visibility or source edit
permission. Lookup/member queries make supported caller decisions; compiler-backed
admission and the host's revision/permission guards remain independent. Per-query
steps/depth/candidates and world-module budgets refuse without truncated responses;
cancellation retains no partial publication. Cached interface boundaries avoid
rescanning an entire unit for every imported-name query. No authored code executes.

WORK.md records native binding and exact compiled fixture behavior, plus selected
real badge/ancestor/member/result binding at original physical declarations. A
handwritten minimal System fixture isolates ordinary root/import behavior; it is
not a captured real SDK or full System qualification. Browser consumer compiles
only. There is no default-provider/Studio/MCP rollout here. General generic/helper/
overload meaning, complete compiler/SDK/grammar capture, paged completion candidates,
incremental dependencies/budgets and full editor/navigation/quality/parity/delivery
remain under original authoring/service ownership.

## Replaceable Pascal completion providers

The user selected a **fresh, wholly Nyx-owned Pascal resolver** on 2026-10-17.
The engine and core retain Nyx's MIT license. The existing provider/request/result
contracts remain the integration seam for Studio and semantic clients. Historical
CodeTools worker qualification below establishes that seam; it is not the chosen
engine, a default dependency or a distribution plan. Original authoring criterion
two owns language breadth, unfinished-buffer recovery, incremental/cancellable
project analysis, navigation and full native/browser quality. A controlled fixture
reply qualifies editor integration only and proves no language-resolution capability.

`nyx.pascal.completion` describes completion independently of Studio, CodeTools,
DOM and LCL. Queries freeze exact main/unit/include membership, accepted comparison
text and every current buffer. Carets and replacement ranges count Unicode
scalars. `At`, `From`, `Skip` and `Take` return independent values; an include names
its owning unit explicitly. The provider establishes that actual include context.
No query parses an unfinished buffer into an accepted compiler project.

```pascal
LQuery := NewNyxPascalCompletionRequest(LBuffers)
  .At(LSelectedFile, LCaret)
  .Take(12);
{ Dispatch on a host worker, retaining its explicit revision/context guard. }
LSuggestions := LProvider.Complete(LQuery);

if (LSuggestions.Status = pcsReady) and (LSuggestions.Count > 0) then
begin
  LProposedBuffers := LSuggestions.Preview(0, LCurrentBuffers, LNewCaret);
end;
```

Rows have typed kinds, insertion text and readable details. A page contains at
most fifty rows; per-row scalar limits and a 128 KiB whole-text budget apply.
Unavailable, failed and cancelled results contain no insertions. `Preview`
compares **all** captured accepted/current files and source limits before returning
an independent proposed buffer. It replaces the full expected identifier around
the caret, including any tail beyond it. A changed helper refuses the proposal
even when the selected file is unchanged. Preview edits no editor, history or
accepted project and grants no compilation/execution provenance. A later editor
integration must additionally guard its revision, live context and permission.

The native `nyx.studio.completion.native` adapter accepts a copied
`TNyxPascalCompletionHost.Using(...)` configuration with an optional deadline.
Each synchronous worker call owns a fresh private directory and separate process.
Structured process arguments, bounded pipes/replies and the existing compiler
family adapter retain ownership through actual join. Missing tools return
unavailable; process/deadline/protocol failures return empty failed results.
Private logs retain platform details; public failures contain no SDK paths.
These machine paths are never document data. This factory installs no provider
into Studio or its server and must not run on the UI thread.

`tools/nyx_pascal_completion_worker.lpr` is an **optional, separately built**
CodeTools adapter. CodeTools source declares GPLv2-or-later; a linked helper must
be treated accordingly. Nyx's portable API/client source retains its MIT notice.
The helper is absent from default product builds and no binary is bundled. It
borrows CodeTools' one unit-owned global manager inside one invocation; another
manager would overwrite process-global cache roots. Captured logical source leaves
are materialized only inside that invocation's owned directory so directory-based
include discovery resolves the exact captured text. Requests never execute
authored applications. Its presently qualified compiler context is Win32/i386
FPC in Delphi mode; browser-target defines/search paths and other host platforms
remain separate work.

`nyx.pascal.completion.wire` is strict private version-one request/reply transport,
not an MCP tool, public project import or source admission channel. Correlation,
closed roles/kinds/statuses, exact fields and range validation bind replies to the
caller-supplied frozen query. Its current JSON codec imposes 4 MiB requests,
1 MiB replies and 1024 members per container; formatting/escaped text counts toward
that limit. Portable source limits can exceed this transport, in which case this
adapter refuses rather than silently omitting helpers. Another provider/transport
can support larger projects. No general completion-latency/cache budget is claimed.

The maintained Pascal consumers are `nyx_pascal_completion_tests.lpr` and
`nyx_pascal_completion_host_tests.lpr`. The latter takes the built optional helper,
SDK root, FPC executable, FPC source root, Lazarus source root and a fresh owned
scratch directory. It also exports the exact proposed Pascal for independent
compiler checks. WORK.md separates native engine execution from browser-only
compilation. Interactive Studio completion, observing/socket qualification, cancellation
integration, general overload/generic identity, browser compiler context, broad
language coverage, quality/parity and delivery remain with the original authoring,
service and workflow owners.

## Semantic completion queries

Current source extends the existing `nyx_pascal` tool with `mode:complete`.
An explicit acknowledged `expectedRevision` and Unicode-scalar `caret` are always
required. For this mode, omitted `snapshot` selects **editing** buffers; explicit
`accepted` selects the accepted inputs. Other existing source reads retain their
accepted default. Unsynchronized frontend keystrokes are not acknowledged server
inputs. The same typed file/owner selectors used by routine reads select supporting
units and the owning unit of an include.

```json
{
  "mode": "complete",
  "expectedRevision": 12,
  "file": {"kind": "unit", "name": "example.presenter.pas"},
  "caret": 164,
  "limit": 8
}
```

The response contains typed suggestion kinds, names, insertion text, readable
details, total/page positions and exact scalar replacement metadata. It never
returns the full source project. Twenty rows are the default, fifty the maximum;
the complete semantic response retains the existing 48 KiB context limit. Reduce
the page size if rich signatures exceed it. Results remain inspection-only:
neither a ready row nor its replacement range grants source admission, compilation,
application execution or authority to consume a pending draft. Existing accepted
source edits retain their compiler/history/draft guards; semantic current-buffer
editing and ordinary interactive completion remain separate workflow work.

The owning backend captures immutable inputs under its guard, records scoped
`resolving completion` activity, then releases that guard before calling the
optional provider. Ordinary editor observation and independent projects can
continue. It resolves the live context afresh afterward; raw session pointers do
not survive external analysis. Revision, session/load identity, complete sources,
all query selectors, permission generations and provider generation must still
match. Disable/reenable or provider replacement/restoration retires the old reply.
A retired review cannot fall back to the primary project. The page is independently
bounded/validated before a completed activity row is recorded. External exceptions
produce a generic refusal, keeping private host details out of operator activity.

Hosts explicitly supply a trusted adapter through
`TNyxStudioMCP.ConfigureCompletionProvider`. It is independent of document output
targets and exposes no executable/search-path configuration through MCP. A missing
adapter returns `unavailable`; it never blocks browsing or design. The default
products still install no analysis helper. Selecting/distributing the optional
CodeTools engine, a private host settings UI and interactive Nyx completion
presentation remain open. WORK.md owns native production-engine evidence separately
from socket/observing browser qualification, broader source/target behavior and
preserving delivery; this schema is not installed in the frozen release.

## Private operator completion channel

Current source supplies `TNyxStudioCompletionClient` in
`nyx.studio.completion` for an embedding editor's asynchronous completion requests.
It owns a separate `TNyxStudioEditorExchange`, an immutable query and a managed
`INyxStudioCompletionReply`. The existing private editor transport authenticates
the operator; neither a packet field nor an MCP caller can select that authority.
Agent enablement remains independent. Disabling agents refuses their completion
queries while a person holding the private editor capability may still inspect
an ordinary project's acknowledged buffers. Private queries cannot target review
contexts or configure an analysis executable.

The client sends only the exact acknowledged revision, project, source origin,
file/include-owner selectors, scalar caret and bounded page. It sends no source
project or private capability in the packet. The embedding owner must first require
its complete local source frame to match the server's acknowledged frame; a
revision alone does not establish that unsynchronized keystrokes have arrived.
The server reconstructs all inputs under the model guard and releases that guard
for analysis. It resolves the project afresh and independently rechecks role,
revision, session/load identity, full sources, query and provider generation before
admitting the page. Agent permission changes do not revoke an operator read;
provider replacement, later source changes and retired contexts still refuse it.
Operator activity remains visible through ordinary observation.

Replies use the shared typed completion codec, with exact request/project/revision
correlation. Results retain the caller's frozen query for the public completion
menu's final source/history/caret validation. Transport errors and malformed pages
produce scoped failure feedback without exposing private server diagnostics.
`Cancel` revokes local delivery and releases its receiver; it does not cancel an
already admitted server read. Reentrant requests are allowed after the delivery
has detached. Queue owner/client destruction after the callback returns because
the underlying exchange may still be dispatching it. Exchange destruction owns
its transport join.

This channel is independent of selected application outputs. An absent optional
provider returns unavailable. No default engine, helper binary, MCP tool or MCP
schema is installed by this addition. The maintained native production-engine
consumer exercises the private operation on a suspended, never-listening backend;
the client consumer substitutes asynchronous transport and compiles for pas2js.
These consumers are not HTTP/browser execution. Ordinary retained-editor adoption
now uses the managed context and public popup described below; controlled replies
qualify that integration independently of language resolution. WORK.md owns exact
evidence and the user-selected fresh Nyx-owned engine/delivery boundary.

## Ordinary Studio completion popup

Both Studio adapters configure an independently owned private completion exchange
beside ordinary polling. A managed `INyxStudioCompletionContext` borrows the bridge
through a revocable lease. Submission freezes the query and requires the complete
local frame, accepted inputs and all draft buffers to match their acknowledged
counterparts. It retains the exact revision and query selectors for presentation.
Neither agent enablement nor application output configuration grants or revokes
this operator capability. Bridge retirement/reconfiguration detaches old leases
and cancels their local deliveries before releasing sessions or transport.

Control+Space in the retained source view opens a public Nyx completion card inside
the existing target popover. Pending synchronization has visible feedback and
retries after ordinary acknowledgement; it sends no stale or user-supplied source
through the query packet. Offline/unavailable/failed states remain explicit. Main
and unit buffers use themselves as context; includes consume the ordinary Outline
owner selector through an optional read-only capability. No owning unit is inferred
from filesystem proximity.

The source-input owner retains one menu, opaque ticket, query receiver, physical
generation/caret and weak service context. A changed file/load/source, selection,
composition or retired mount revokes it. A late reply must match the local current
buffers and the service's acknowledged revision/context before the menu may admit
it. Popup rows route through their exact current Nyx mount/ticket. Arrow selection,
page actions, Escape and suggestion buttons use the shared presentation contract.
Enter/Tab acceptance uses that same source keyboard producer.

`INyxPascalCompletionPreparation` optionally stages an immutable candidate without
moving history, letting Studio validate helper-draft admission before acceptance.
One completion then publishes ordinary authored draft text through the same text
timeline and existing draft bridge. Undo/Redo retain the exact preceding bytes and
caret; completion does not compile/admit source or add a paired design command.
The source memo survives popup updates and expanded-source movement. Target
presentation runs after callback return, preserves source focus, and retires the
old public popover before mounting a changed card. A hidden/unmounted source
requires no readiness retry. Native presentation-pending includes this queued work.

The owned `nyx_studio_completion_controls.lpr` consumer substitutes asynchronous
transport into a suspended, never-listening production backend and uses controlled
provider replies. It executes real ordinary Win32 memo/button/menu input at native
390/1280 host widths. It proves editor synchronization, popup/history/input ownership,
not real symbol/type resolution, HTTP transport, browser execution, visual quality
or hardware/IME/accessibility. Browser Studio/client compile only. The user-selected
Nyx-owned language engine, comprehensive main/unit/include interaction, caret-level
anchoring, full performance/quality/parity and preserving delivery remain open.
No default helper, new MCP tool/schema or installed release is added here.

## Authored and presented text

`nyx.text.projection` separates exact authored Pascal from the text a target
control displays. `NewNyxTextProjection` freezes both complete texts and permits
only CR/LF/CRLF spelling differences. Unicode scalars, supplementary characters,
combining sequences and NUL remain exact in the portable model; native widgets
retain their own explicit NUL limitation. No identifier/case/grapheme normalization
is inferred. The projection owns no editor, source session or document.

A sparse ordered newline-width map translates scalar boundaries in either
direction. A CRLF interior without a one-scalar counterpart refuses under default
`npaExact`; callers may explicitly choose `npaBefore` or `npaAfter`. Defined
selections preserve direction; unavailable selections remain unavailable. Typed
`TNyxTextInsertion` values carry authored/displayed bytes independently and fluent
`UsingLineEndings` accepts `ntePreserve`, `nteLF`, `nteCRLF` or `nteCR`.

```pascal
LProjection := NewNyxTextProjection(LAuthoredText, LDisplayedText);
LAuthoredCaret := LProjection.ToAuthored(LSelection.Finish);
LFrame := NyxProjectedTextEditorFrame(LProjection, LSelection, nteLF);

LChanged := LProjection.ReplacePresented(LStart, LExpectedDisplayedText,
  NyxTextInsertion(LInsertedPascal).UsingLineEndings(nteLF));
```

Exact `ReplacePresented` preserves the untouched authored prefix/suffix. When a
physical input producer supplies only complete text, `ObservePresented` finds one
contiguous delta using common scalar prefix/suffix and expands indivisible CRLF
edges. Ambiguous repeated content is attributed after the longest common prefix;
this is not semantic multi-edit provenance. Pure target-only newline respelling
preserves the authored text. Explicit replacement remains available for deliberate
authored newline changes. Complete validation precedes publishing a new projection.

Projected history frames own both strings and an insertion presentation policy,
with no interfaces in records. Distinct authored bytes count in addition to
physical bytes under the existing retention budget. Authored-only changes remain
undoable even when displayed text is identical. `INyxProjectedTextHistory` marks
alternative implementations which preserve this contract. The expanded returned
frame record versions `INyxTextEditHistory`'s native IID; native consumers must be
rebuilt together rather than mixing old binaries. This changes no persistence,
MCP tool inventory or wire schema.

The public completion menu converts its physical collapsed caret to authored
coordinates for a query, stages the complete authored proposal, then maps the
exact replacement back to displayed coordinates for one history transaction.
Projected frames require the optional history capability. Tickets, exact sources,
generation/caret guards and stale-result refusal remain unchanged. WORK.md owns
native model/control evidence separately from compiled-only browser consumers;
ordinary Studio popup/channel adoption and full quality/parity remain open.

## Studio source input ownership

Current source connects `TNyxStudioSourceInput` to both Studio adapters' retained
Nyx code-editor view. The controller owns bounded recent-file text timelines and
managed callback leases, while borrowing the current authoring session and target
reader/presenter. It owns no widgets or document tree. Source/project/load identity,
accepted inputs, selected file and the known draft must still match before an
input command may publish. An independently changed draft refuses old commands
until ordinary rendering reconciles a fresh baseline.

Keyboard Undo/Redo and platform history requests share `INyxTextHistoryCommands`.
The optional renderer `BindTextHistory` installs that managed authority for one
physical mount. `BeforeEdit` captures the preceding text/caret before keyboard,
clipboard or range replacement moves them. Composition observations retain one
transaction. Exhausted history commands remain consumed. Paired design/source
history stays separate: typing does not add design commands, and explicit Restore
accepted resets the local text timeline through ordinary reconciliation.

The Win32 adapter uses a retirement-aware Common Controls subclass because native
edit messages can bypass LCL's ordinary window callback. Its context-menu query and
Undo messages use the same timeline; handle destruction removes the subclass and
rebinding admits a recreated handle. An optional observer capability preserves the
original editing-observer interface. Exceptions stay within the native callback
boundary. Other widgetsets retain their existing editing behavior pending their
own admitted bridge. See Microsoft's [subclassing contract](https://learn.microsoft.com/en-us/windows/win32/controls/subclassing-overview).

Each physical frame now owns its exact authored projection. Ordinary contiguous
edits preserve all untouched source bytes rather than rewriting the entire buffer
using the widget's line endings. A cancelled composition restores its exact authored
base and preserves Redo. The explicit Unicode restore bridge retains mixed physical
line endings/caret across Undo and handle/modal movement, without publishing a
draft merely to repair presentation. Native NUL projection refuses before writing.
Native insertion presentation defaults to Preserve; the browser source owner
explicitly chooses LF. This is the bounded projection contract above, not arbitrary
semantic multi-edit provenance.

The browser adapter captures `beforeinput`, consumes cancelable history requests
and routes noncancelable history `input` through the owner before admitting the
platform stack's value. Programmatic presentation is kept outside physical history
observation. The browser implementation is compiled only in this packet.

WORK.md owns actual native Studio keyboard/context/readonly/recreation/modal/stale
input evidence at two host widths and the ordinary editing regression. Synthetic
native composition covers commit/cancelled-base ownership, not hardware/IME.
Recent-file eviction/multi-project journeys, assistive technology, broader budgets,
other widgetsets and full browser/parity/delivery remain open. The completion menu
and private query channel now consume this controller and its typed authored/
physical position/replacement projection as described above. No default helper,
MCP tool/schema or observing release is installed by this source change.

## Shared editing history and completion presentation

`nyx.editing.history` supplies an independent `INyxTextEditHistory` for one
physical buffer. Immutable frames contain exact physical/authored text, insertion
presentation policy and scalar selection,
including unavailable selection rather than an inferred caret. Ordinary physical
observations and programmatic replacements share the same timeline. Each accepted
physical text change is one entry; composition drafts retain a separate base and
commit one entry at genuine composition end. Returning to that base adds no entry
and preserves Redo. Selection-only observations also preserve Redo.

The retention limits are copied fluent values. Defaults keep 128 entries and
16 MiB across both stacks, accounting physical UTF-8 text, distinct authored UTF-8
text and 32 bytes per frame. This is
a portable accounting budget, not a heap measurement; Current and a live composition
base are excluded. Physical edits remain real input even when their previous
frames cannot be retained; `Truncated` reports eviction. Programmatic replacement
instead refuses before mutation if it cannot retain its immediate Undo/Redo.
`UndoFrame`/`RedoFrame` let an owning source model stage its own candidate/budget
admission before synchronously changing the timeline. Reset, selection, composition
and edits advance a runtime generation, preventing an old query from reviving after
an edit/Undo back to identical text. None of this admits accepted source.

`nyx.pascal.completion.view` builds a reusable completion card from ordinary Nyx
buttons, badges, rows, labels and a bounded scroll area. It can be embedded in an
existing popover or inline host; it owns no window-positioning system. The default
page is twelve rows, with readable signatures, typed kind captions, current-row
presentation, paging actions and explicit pending/unavailable/failure feedback.

```pascal
LHistory := NewNyxTextEditHistory(NyxTextEditorFrame(LText, LSelection));
LMenu := NewNyxPascalCompletionMenu(LHistory);
LQuery := LMenu.BeginQuery(LBuffers, LFile, LOwner, LTicket);
{ Run the provider on a worker, then return its immutable result to the UI. }

if LMenu.Present(LTicket, LReply, LCurrentBuffers) then
begin
  LContent := LMenu.Build('source-completion', LCurrentBuffers);
end;
```

The opaque ticket belongs to this exact menu request, including equal successive
queries. Current sources, history generation and the captured caret must still
match. Foreign/out-of-order replies leave the newest request intact; stale matching
replies close it. Row routing additionally checks exact node presence in the current
tree, its ticket and effective interaction policy. `Accept` prepares the complete
source snapshot first, then performs one exact expected-frame replacement through
the text history. Accepted text and independent helper buffers remain exact.

The typed `Key` action decoder offers Control+Space, arrows, Enter/Tab, Escape and
ordinary Control/Command Undo/Redo. The embedding input owner consumes handled
actions through Nyx's sequential event response and renders the proposed text/caret
without replacing its input widget. Empty/repeated history commands still belong
to this timeline and must not fall through to a stale platform stack. Composition
retains its own keys. The owner must enforce its physical mount/read-only policy,
observe every physical edit/selection/composition, publish ordinary draft changes
and route **all** platform history producers consistently. This component does not
intercept native context menus, browser beforeinput or OS history actions itself.

The maintained `nyx_completion_view_tests.lpr` covers the portable model/view.
`nyx_completion_controls.lpr` exercises real Win32 memo/button/key callbacks and
an asynchronously joined SDK provider at two native host widths. Its arguments are
SDK root, toolchain JSON, owned scratch, optional helper, FPC source root and width.
It changes only its independent buffer and never runs an authored application.
WORK.md distinguishes that native evidence from browser compilation. Ordinary
full Studio interaction, caret-level anchor/layout/visual quality, complete native/browser
history-producer integration, hardware/IME/accessibility, performance and preserving
delivery remain open; these factories do not install an editor or helper in Studio.
